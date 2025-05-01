
(defsystem "tqlite"
  :author "Gabriel Johnson <gabriel.johnson.pitp@gmail.com>"
  :description "Thtructured Query Language (SQL with a Lisp) -- an SQLite3 wrapper"
  :license "BSD"
  :depends-on ("cffi" "trivial-garbage")
  :components ((:file "package")
               (:file "c-functions"
                :depends-on ("package"))
               (:file "status-codes"
                :depends-on ("package" "c-functions"))
               (:module "src"
                :depends-on ("package" "status-codes" "c-functions")
                :components ((:file "library")
                             (:file "connection"
                              :depends-on ("statement-module"))
                             (:file "statement")
                             (:module "statement-module"
                              :depends-on ("statement")
                              :components ((:file "row")
                                           (:file "step"
                                            :depends-on ("row"))
                                           (:file "bind")))))))
