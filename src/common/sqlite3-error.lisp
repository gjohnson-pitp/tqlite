
(in-package :tqlite)

(define-condition sqlite3-error (error)
  ((message :reader error-message
            :initarg :message)))
