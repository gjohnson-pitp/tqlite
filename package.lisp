
(defpackage :tqlite
  (:use :cl :cffi :trivial-garbage)
  (:export #:open-database
           #:close-database
           #:prepare-statement
           #:prepare-persistent-statement
           #:with-statement
           #:execute-sql
           #:finalize-statement
           #:reset-statement
           #:step-statement
           #:step-until-done
           #:do-rows
           #:bind-parameter
           #:clear-bindings
           #:column-count
           #:get-column
           #:cannot-connect-to-database
           #:cannot-prepare-statement
           #:step-statement-failed
           #:cannot-make-column
           #:bind-parameter-failed
           #:cannot-make-valid-blob))
