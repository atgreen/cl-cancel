# Migrating from cl-context to cl-cancel

This guide helps you migrate code from `cl-context` to `cl-cancel`.

## Key Differences

1. **No context values** - Use dynamic variables instead
2. **Renamed types** - `context` → `cancellable`
3. **Renamed conditions** - `context-cancelled` → `cancelled`
4. **Focused API** - Only cancellation, no value methods

## Quick Reference

### Package and Imports

```lisp
;; Before (cl-context)
(use-package :cl-context)

;; After (cl-cancel)
(use-package :cl-cancel)
```

### Type Names

| cl-context | cl-cancel |
|------------|-----------|
| `context` | `cancellable` |
| `cancellable-context` | `cancellable-context` |
| `background-context` | `background-context` |
| `deadline-context` | `deadline-context` |
| `value-context` | (removed) |

### Condition Names

| cl-context | cl-cancel |
|------------|-----------|
| `context-error` | `cancellation-error` |
| `context-cancelled` | `cancelled` |
| `context-deadline-exceeded` | `deadline-exceeded` |

### Function Names

| cl-context | cl-cancel |
|------------|-----------|
| `check-context` | `check-cancellation` |
| `with-context` | `with-cancellable` |
| `call-with-context` | `call-with-cancellable` |
| `ensure-context` | `ensure-cancellable` |
| `*current-context*` | `*current-cancel-context*` |

### Creation Functions

Most creation functions remain the same:

```lisp
;; Same in both libraries
(background)
(with-cancel parent)
(with-timeout parent seconds)
(with-deadline parent absolute-time)
```

### Removed: Context Values

The `value`, `with-value`, and `value-context` APIs have been removed.

#### Before (cl-context)

```lisp
(defvar +request-id+ nil)

;; Creating context with value
(let ((ctx (with-value (background) +request-id+ "REQ-12345")))
  (process-request ctx))

;; Retrieving value
(defun log-message (msg)
  (format t "[~A] ~A~%"
          (value *current-context* +request-id+ "NO-ID")
          msg))
```

#### After (cl-cancel)

```lisp
;; Use dynamic variables directly
(defvar *request-id* nil)

;; Binding at request boundary
(let ((*request-id* "REQ-12345"))
  (with-cancel-context (ctx (background))
    (process-request)))

;; Direct access
(defun log-message (msg)
  (format t "[~A] ~A~%"
          (or *request-id* "NO-ID")
          msg))
```

### Benefits of Dynamic Variables

1. **Standard Lisp feature** - No new API to learn
2. **Type safety** - `(declaim (type string *request-id*))`
3. **Better tooling** - IDE navigation, completion, refactoring
4. **Performance** - Direct variable access vs hash table lookup
5. **Compiler optimization** - Can inline and optimize

## Migration Examples

### Example 1: HTTP Request Handler

#### Before (cl-context)

```lisp
(defvar +request-id+ nil)
(defvar +user-id+ nil)

(defun handle-request (request)
  (let* ((ctx (with-value (background) +request-id+ (generate-id)))
         (ctx (with-value ctx +user-id+ (get-user request))))
    (with-timeout-context (ctx 30 ctx)
      (process-request request))))

(defun process-request (request)
  (log-info "Processing request")
  ;; ...
  )

(defun log-info (msg)
  (format t "[~A] [~A] ~A~%"
          (value *current-context* +request-id*)
          (value *current-context* +user-id*)
          msg))
```

#### After (cl-cancel)

```lisp
(defvar *request-id* nil)
(defvar *user-id* nil)

(defun handle-request (request)
  (let ((*request-id* (generate-id))
        (*user-id* (get-user request)))
    (with-timeout-context (ctx 30)
      (process-request request))))

(defun process-request (request)
  (log-info "Processing request")
  ;; ...
  )

(defun log-info (msg)
  (format t "[~A] [~A] ~A~%"
          *request-id*
          *user-id*
          msg))
```

### Example 2: Nested Contexts with Values

#### Before (cl-context)

```lisp
(defvar +trace-id+ nil)
(defvar +span-id+ nil)

(defun outer-operation ()
  (let ((ctx (with-value (background) +trace-id+ (make-trace-id))))
    (with-timeout-context (ctx 10 ctx)
      (inner-operation))))

(defun inner-operation ()
  (let ((ctx (with-value *current-context* +span-id+ (make-span-id))))
    (with-context (ctx ctx)
      (do-work))))
```

#### After (cl-cancel)

```lisp
(defvar *trace-id* nil)
(defvar *span-id* nil)

(defun outer-operation ()
  (let ((*trace-id* (make-trace-id)))
    (with-timeout-context (ctx 10)
      (inner-operation))))

(defun inner-operation ()
  (let ((*span-id* (make-span-id)))
    (do-work)))
```

### Example 3: Database Connection Pool

#### Before (cl-context)

```lisp
(defvar +db-connection+ nil)

(defun with-database ((ctx db-config))
  (let* ((conn (connect-to-db db-config))
         (ctx (with-value ctx +db-connection+ conn)))
    (with-context (ctx ctx)
      (unwind-protect
           (execute-queries)
        (disconnect conn)))))

(defun execute-query (sql)
  (let ((conn (value *current-context* +db-connection+)))
    (db-query conn sql)))
```

#### After (cl-cancel)

```lisp
(defvar *db-connection* nil)

(defun with-database (db-config)
  (let* ((conn (connect-to-db db-config))
         (*db-connection* conn))
    (unwind-protect
         (execute-queries)
      (disconnect conn))))

(defun execute-query (sql)
  (db-query *db-connection* sql))
```

## Automated Migration Steps

1. **Replace package references**
   ```
   sed -i 's/cl-context/cl-cancel/g' *.lisp
   ```

2. **Rename types**
   ```
   sed -i 's/context-cancelled/cancelled/g' *.lisp
   sed -i 's/context-deadline-exceeded/deadline-exceeded/g' *.lisp
   sed -i 's/context-error/cancellation-error/g' *.lisp
   ```

3. **Rename functions**
   ```
   sed -i 's/check-context/check-cancellation/g' *.lisp
   sed -i 's/with-context/with-cancellable/g' *.lisp
   sed -i 's/\*current-context\*/\*current-cancellable\*/g' *.lisp
   ```

4. **Manual migration of values**
   - Find all `(define-context-key ...)` → Convert to `(defvar ...)`
   - Find all `(with-value ...)` → Convert to `(let ...)`
   - Find all `(value context key)` → Convert to direct variable access
   - Find all `value-context` references → Remove/refactor

## Testing Migration

1. **Update test dependencies**
   ```lisp
   ;; In .asd file
   :depends-on (#:cl-cancel ...)  ; was :cl-context
   ```

2. **Run existing tests**
   ```lisp
   (asdf:test-system :your-system)
   ```

3. **Check for value API usage**
   ```bash
   grep -r "with-value\|define-context-key\|value-context" src/
   ```

## Common Issues

### Issue: Missing value API

**Problem**: Code relies on context values for passing data.

**Solution**: Replace with dynamic variables bound at the appropriate scope.

### Issue: Type confusion

**Problem**: Code checks for `context` type instead of `cancellable`.

**Solution**: Update type checks:
```lisp
;; Before
(typep x 'context)

;; After
(typep x 'cancellable)
```

### Issue: Macro name conflicts

**Problem**: Using old macro names.

**Solution**: Update macro usage:
```lisp
;; Before
(with-timeout-context (ctx 5 parent) ...)

;; After (same, but parent is now optional via *current-cancel-context*)
(with-timeout-context (ctx 5 parent) ...)
;; or
(with-timeout-context (ctx 5) ...)
```

## Advantages After Migration

1. **Simpler mental model** - One concern: cancellation
2. **Standard idioms** - Dynamic variables are well-understood
3. **Better performance** - No hash table lookups
4. **Smaller memory footprint** - No value storage per context
5. **Type safety** - Dynamic variables can be declared
6. **Better debugging** - Standard Lisp debugging tools work

## Questions?

If you encounter issues during migration, please open an issue on GitHub with:
- Example of cl-context code
- Your attempted cl-cancel translation
- What's not working as expected
