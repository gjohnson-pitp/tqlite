
(in-package :tqlite)

(defconstant +sqlite-ok+ 0)

(defun sqlite3-error-message (error-code)
  (foreign-funcall "sqlite3_errstr"
                   :int error-code
                   :string))

(define-condition sqlite3-error (error)
  ((message :reader error-message
            :initarg :message)))
