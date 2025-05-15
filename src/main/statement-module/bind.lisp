
(in-package :tqlite)

(define-condition bind-parameter-failed (sqlite3-error)
  ()
  (:report (lambda (condition stream)
             (format stream
                     "Binding parameter failed with message: ~S"
                     (error-message condition)))))

;;; As much as I'd prefer to do some validation, SQLite allows the
;;; programmer to specify the index of each parameter, so we have no
;;; way of knowing whether an index is valid or not unless we parse
;;; the SQL ourselves. So better to just leave the validation to the
;;; SQLite side.

(defgeneric bind-parameter (statement index value)
  (:documentation "(bind-parameter statement index value)

Binds the given value to the parameter with the given index in the
given statement. For named parameters, a string can be given in place
of an index.

The type of the parameter is inferred from the type of VALUE. An
integer is assumed to correspond to an integer parameter, a
floating-point number to a double parameter, nil to a null parameter,
a string to a text parameter, and a vector other than a string to a
blob parameter. If the vector's elements are not all unsigned bytes,
then an error of type CANNOT-MAKE-VALID-BLOB is thrown.

Once a statement has been stepped at least once, it must be reset with
RESET-STATEMENT before it can have new parameters bound to
it. Attempting to bind parameters to a statement that's already been
stepped and not reset will result in a NO-APPLICABLE-METHOD error.

SEE ALSO:
step-statement
reset-statement
clear-bindings"))

(defmethod bind-parameter ((statement bindable-statement) (name string) value)
  (let ((index (sqlite3-bind-parameter-index (statement-pointer statement)
                                             name)))
    (if (zerop index)
        (error 'bind-parameter-failed
               :message (database-error-message statement))
        (bind-parameter statement index value))))

(defmethod bind-parameter ((statement bindable-statement)
                           (index integer)
                           (value integer))
  (unless (eql +sqlite-ok+
               (sqlite3-bind-int64 (statement-pointer statement) index value))
    (error 'bind-parameter-failed
           :message (database-error-message statement))))

(defmethod bind-parameter ((statement bindable-statement)
                           (index integer)
                           (value float))
  (unless (eql +sqlite-ok+
               (sqlite3-bind-double (statement-pointer statement)
                                    index
                                    (coerce value 'double-float)))
    (error 'bind-parameter-failed
           :message (database-error-message statement))))

(defcallback deallocate-string :void ((string :pointer))
  (foreign-string-free string))

(defmethod bind-parameter ((statement bindable-statement)
                           (index integer)
                           (value string))
  (let ((foreign-string (foreign-string-alloc value)))
    (unless (eql +sqlite-ok+
                 (sqlite3-bind-text (statement-pointer statement)
                                    index
                                    foreign-string
                                    -1
                                    (callback deallocate-string)))
      (foreign-string-free foreign-string)
      (error 'bind-parameter-failed
             :message (database-error-message statement)))))

(defcallback deallocate-blob :void ((blob :pointer))
  (foreign-free blob))

(defclass valid-blob ()
  ((contents :reader blob-contents
             :initarg :contents)))

(define-condition cannot-make-valid-blob (error)
  ((vector :reader error-vector
           :initarg :vector))
  (:report (lambda (condition stream)
             (format stream
                     "Argument inferred to be a blob, but ~S cannot be ~
                      converted to an array of bytes"
                     (error-vector condition)))))

(defun make-valid-blob (vector)
  (if (every (lambda (element) (typep element '(unsigned-byte 8)))
             vector)
      (make-instance 'valid-blob :contents vector)
      (error 'cannot-make-valid-blob
             :vector vector)))

(defmethod make-blob-foreign-pointer ((blob valid-blob))
  (foreign-alloc :uint8 :initial-contents (blob-contents blob)))

(defmethod blob-length ((blob valid-blob))
  (length (blob-contents blob)))

(defmethod bind-parameter ((statement bindable-statement)
                           (index integer)
                           (value vector))
  (bind-parameter statement index (make-valid-blob value)))

(defmethod bind-parameter ((statement bindable-statement)
                           (index integer)
                           (value valid-blob))
  (let ((blob-pointer (make-blob-foreign-pointer value)))
    (unless (eql +sqlite-ok+
                 (sqlite3-bind-blob (statement-pointer statement)
                                    index
                                    blob-pointer
                                    (blob-length value)
                                    (callback deallocate-blob)))
      (foreign-free blob-pointer)
      (error 'bind-parameter-failed
             :message (database-error-message statement)))))

(defmethod bind-parameter ((statement bindable-statement)
                           (index integer)
                           (value null))
  (unless (eql +sqlite-ok+
               (sqlite3-bind-null (statement-pointer statement) index))
    (error 'bind-parameter-failed
           :message (database-error-message statement))))

(defgeneric clear-bindings (statement)
  (:documentation "(clear-bindings statement)

Clears all parameter bindings on the given statement.

In SQLite, any unbound parameters are implicitly bound to null. Thus,
one can argue that there isn't actually any such thing as an \"unbound
parameter\" in SQLite. But you can use this to at least ensure that
e.g. a statement isn't accidentally stepped with the same arguments
twice in a row.

SEE ALSO:
bind-parameter")
  (:method ((statement statement))
    ;; This returns an int, but the documentation has no information
    ;; on the return value. I thought maybe it works like
    ;; bind-parameter, but then using it after stepping a statement
    ;; (when binding new parameters isn't allowed) didn't return
    ;; SQLITE_MISUSE. So I can only assume that this operation never
    ;; fails
    (sqlite3-clear-bindings (statement-pointer statement))))
