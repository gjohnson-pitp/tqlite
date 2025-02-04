
(in-package :tqlite)

(defconstant +sqlite-ok+ 0)

(defconstant +sqlite-busy+ 5)

(defconstant +sqlite-row+ 100)

(defconstant +sqlite-done+ 101)

(defconstant +sqlite-misuse+ 21)

(defun sqlite3-error-message (error-code)
  (foreign-funcall "sqlite3_errstr"
                   :int error-code
                   :string))

(define-condition sqlite3-error (error)
  ((message :reader error-message
            :initarg :message)))
