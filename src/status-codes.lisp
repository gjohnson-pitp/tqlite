
(in-package :tqlite)

(defconstant +sqlite-ok+ 0)

(defun sqlite3-error-message (error-code)
  (foreign-funcall "sqlite3_errstr"
                   :int error-code
                   :string))

(defun check-status-code (status-code if-error)
  (unless (eql status-code +sqlite-ok+)
    (funcall if-error (sqlite3-error-message status-code))))

(defmacro when-fail ((message-variable form) &body body)
  `(check-status-code ,form (lambda (,message-variable) ,@body)))

(define-condition sqlite3-error (error)
  ((message :reader error-message
            :initarg :message)))
