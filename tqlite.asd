
(defsystem "tqlite"
  :author "Gabriel Johnson <gabriel.johnson.pitp@gmail.com>"
  :description "Thtructured Query Language (SQL with a Lisp) -- an SQLite3 wrapper"
  :license "BSD"
  :depends-on ("cffi" "trivial-garbage")
  :components ((:file "package")
               (:module "src"
                :depends-on ("package")
                :components ((:module "common"
                              :components ((:file "c-functions")
                                           (:file "status-codes"
                                            :depends-on ("c-functions"))))
                             (:module "main"
                              :depends-on ("common")
                              :components ((:file "library")
                                           (:file "connection"
                                            :depends-on ("statement-module"))
                                           (:file "statement")
                                           (:module "statement-module"
                                            :depends-on ("statement")
                                            :components ((:file "row")
                                                         (:file "step"
                                                          :depends-on ("row"))
                                                         (:file "bind")))))))))
