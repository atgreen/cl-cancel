;;;; streams.lisp
;;;;
;;;; SPDX-FileCopyrightText: 2026 Anthony Green <green@moxielogic.com>
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Stream cancellation monitoring for cl-cancel
;;;;
;;;; Provides utilities for automatically closing streams when cancellables
;;;; are cancelled, enabling Go-style immediate cancellation for blocking I/O.

(in-package #:cl-cancel)

(defun close-stream-on-cancel (stream &optional (cancellable *current-cancel-context*))
  "Spawns a thread that closes STREAM when CANCELLABLE is cancelled.
   Returns a cleanup function that MUST be called when the I/O operation completes.

   This provides Go-style immediate cancellation by forcibly closing the socket,
   interrupting any blocked I/O operations. Uses semaphores for efficient waiting
   without polling.

   STREAM: The stream to close when cancellable is cancelled
   CANCELLABLE: The cancellable to monitor (defaults to *current-cancel-context*)

   Returns: A cleanup function (lambda of no arguments) that must be called to:
            - Stop the monitor thread
            - Prevent resource leaks

   Only monitors cancellable contexts - non-cancellable contexts (including NIL
   and background) are skipped since they can never be done.

   Usage:
     (let ((cancel-monitor (close-stream-on-cancel stream)))
       (unwind-protect
            (do-io-operation stream)
         (funcall cancel-monitor)))

   Examples:

     ;; HTTP client
     (with-timeout-context (ctx 5)
       (let* ((socket (connect-to-server))
              (cancel-monitor (close-stream-on-cancel socket)))
         (unwind-protect
              (read-http-response socket)
           (funcall cancel-monitor))))

     ;; Database connection
     (with-deadline-context (ctx (+ (get-current-time) 30))
       (let* ((conn (connect-to-db))
              (cancel-monitor (close-stream-on-cancel conn)))
         (unwind-protect
              (execute-query conn query)
           (funcall cancel-monitor))))

     ;; File I/O with timeout
     (with-timeout-context (ctx 10)
       (with-open-file (stream path)
         (let ((cancel-monitor (close-stream-on-cancel stream)))
           (unwind-protect
                (process-large-file stream)
             (funcall cancel-monitor)))))"
  ;; Only monitor if we have a cancellable context
  (if (typep cancellable 'cancellable-context)
      (let* ((done nil)
             (done-lock (bt2:make-lock :name "cancel-stream-monitor"))
             (monitor-thread
               (bt2:make-thread
                (lambda ()
                  ;; Park until the cancellable is done (no polling!).  When the
                  ;; I/O completes without cancellation the cancellable is never
                  ;; signalled, so the cleanup function wakes us via
                  ;; INTERRUPT-THREAD, which throws out to this tag.
                  (catch 'stop-monitor
                    ;; The cleanup function may already have run: I/O that
                    ;; completes at once is over before this thread reaches the
                    ;; CATCH.  Its interrupt then found no tag to throw to, and
                    ;; nothing else will wake this thread before the deadline,
                    ;; while the cleanup function waits to join it.  DONE is set
                    ;; before the interrupt is sent, so it tells us.
                    (unless (bt2:with-lock-held (done-lock) done)
                      (let ((err (wait-until-done cancellable)))
                        ;; Only close stream if cancellable actually became done (err is non-NIL)
                        (when err
                          (bt2:with-lock-held (done-lock)
                            (unless done
                              ;; Cancellable was cancelled - abort the stream to interrupt blocked I/O
                              (ignore-errors (close stream :abort t)))))))))
                :name "cancel-stream-monitor")))
        ;; Return cleanup function
        (lambda ()
          ;; Claim the done flag first: taking the lock here guarantees the
          ;; monitor is either not yet closing (it will see DONE and skip) or
          ;; has finished closing, so the interrupt below never aborts a
          ;; close in progress.
          (bt2:with-lock-held (done-lock)
            (setf done t))
          ;; Wake the monitor if it is still parked waiting for cancellation,
          ;; then reap it so the thread is never leaked.  The throw is wrapped
          ;; in IGNORE-ERRORS to tolerate the monitor being outside the CATCH
          ;; extent: it has just left it (tag already gone), or it has not
          ;; reached it yet, in which case it sees DONE when it does.
          (when (bt2:thread-alive-p monitor-thread)
            (ignore-errors
             (bt2:interrupt-thread monitor-thread
                                   (lambda () (ignore-errors (throw 'stop-monitor nil))))))
          (ignore-errors (bt2:join-thread monitor-thread))))
      ;; No monitor needed for non-cancellable contexts
      (lambda () nil)))
