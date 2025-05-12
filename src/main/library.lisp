
(in-package :tqlite)

(define-foreign-library libsqlite3
  (:unix (:or "libsqlite3.so.3.48.0" "libsqlite3.so"))
  (t (:default "libsqlite3")))

(use-foreign-library libsqlite3)
