
(defsystem "tqlite"
  :author "Gabriel Johnson <gabriel.johnson.pitp@gmail.com>"
  :description "Thtructured Query Language (SQL with a Lisp) -- an SQLite3 wrapper"
  :depends-on ("cffi" "trivial-garbage")
  :components ((:file "package")
               (:file "status-codes"
                :depends-on ("package"))
               (:module "src"
                :depends-on ("package" "status-codes")
                :components ((:file "library")
                             (:file "connection")
                             (:file "statement")
                             (:module "statement-module"
                              :depends-on ("statement")
                              :components ((:file "row")
                                           (:file "step"
                                            :depends-on ("row"))))))))
