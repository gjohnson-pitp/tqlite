
(in-package :tqlite)

(defconstant +sqlite-ok+ 0)

(defconstant +sqlite-busy+ 5)

(defconstant +sqlite-row+ 100)

(defconstant +sqlite-done+ 101)

(defconstant +sqlite-misuse+ 21)

(define-condition sqlite3-error (error)
  ((message :reader error-message
            :initarg :message)))
