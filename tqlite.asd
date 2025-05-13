
(defsystem "tqlite"
  :author "Gabriel Johnson <gabriel.johnson.pitp@gmail.com>"
  :description "Thtructured Query Language (SQL with a Lisp) -- an SQLite3 wrapper"
  :license "BSD"
  :defsystem-depends-on ("cffi-grovel")
  :depends-on ("cffi" "trivial-garbage")
  :components ((:file "package")
               (:module "src"
                :depends-on ("package")
                :components ((:module "common"
                              :components ((:cffi-grovel-file "sqlite3-grovel")
                                           (:file "c-functions")
                                           (:file "sqlite3-error")))
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
