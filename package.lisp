
(defpackage :tqlite
  (:use :cl :cffi :trivial-garbage)
  (:export #:open-database
           #:close-database
           #:prepare-statement
           #:with-statement
           #:finalize-statement
           #:reset-statement
           #:step-statement
           #:step-until-done
           #:do-rows
           #:column-count
           #:get-column
           #:cannot-connect-to-database
           #:cannot-prepare-statement
           #:step-statement-failed
           #:cannot-make-column))
