
(defsystem "tqlite"
  :author "Gabriel Johnson <gabriel.johnson.pitp@gmail.com>"
  :description "Thtructured Query Language (SQL with a Lisp) -- an SQLite3 wrapper"
  :depends-on ("cffi" "trivial-garbage")
  :components ((:file "package")
               (:module "src"
                :depends-on ("package")
                :components ((:file "library")
                             (:file "status-codes")
                             (:file "connection"
                              :depends-on ("status-codes"))
                             (:file "statement"
                              :depends-on ("status-codes"))))))
