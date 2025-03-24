
(in-package :tqlite)

(defclass statement ()
  ((pointer :reader statement-pointer
            :initarg :pointer)
   (database :reader statement-database
             :initarg :database)))

(defclass finalized-statement (statement)
  ())

(defclass unfinalized-statement (statement)
  ())

(defclass statement-bindability (unfinalized-statement)
  ())

(defclass bindable-statement (unfinalized-statement)
  ())

(defclass unbindable-statement (unfinalized-statement)
  ())

(defun sqlite3-finalize (statement-pointer)
  (foreign-funcall "sqlite3_finalize"
                   :pointer statement-pointer
                   :int))

(defmethod initialize-instance :after ((statement unfinalized-statement)
                                       &key pointer)
  (finalize statement
            (lambda () (sqlite3-finalize pointer))))

(defgeneric finalize-statement (statement)
  (:documentation "(finalize-statement statement)

Finalizes the given statement object. A statement object should be
finalized when it's no longer needed. However, if you lose a
reference to a statement object without getting to finalize it, then
it will be finalized automatically when it's garbage collected. If you
do finalize it yourself, then it will not be finalized again when it's
garbage collected.

Finalizing a statement causes it to change class; attempting to
perform any further operations on it (including finalizing again) will
then result in a NO-APPLICABLE-METHOD error. Therefore, it's
impossible to accidentally perform invalid operations like finalizing
a statement twice during normal use of this package.

SEE ALSO:
prepare-statement")
  (:method ((statement unfinalized-statement))
    (sqlite3-finalize (statement-pointer statement))
    (cancel-finalization statement)
    (change-class statement 'finalized-statement)))

(defmethod database-error-message ((statement unfinalized-statement))
  (database-error-message (statement-database statement)))

(defgeneric reset-statement (statement)
  (:documentation "(reset-statement statement)

Restores STATEMENT to its initial state. Must be used before binding
new parameters to a statement that has previously been stepped at
least once.

Resetting a statement does not clear any parameter bindings; stepping
the statement again without binding new parameters will execute it
again from the beginning with the same arguments as
before. sqlite3_clear_bindings is not yet implemented.

See documentation for prepare-statement for other operations on
statements.")
  (:method ((statement unfinalized-statement))
    (foreign-funcall "sqlite3_reset"
                     :pointer (statement-pointer statement)
                     :int)))

(defmethod reset-statement :after ((statement unbindable-statement))
  (change-class statement 'bindable-statement))

(defgeneric step-until-done (statement &key if-row)
  (:documentation "(step-until-done statement &key if-row)

Steps the statement repeatedly until it finishes. IF-ROW should be a
function of one argument. If stepping the statement gives a result
row, then IF-ROW will be called on a corresponding row object.

See STEP-STATEMENT for details.

SEE ALSO:
step-statement
do-rows")
  (:method ((statement unfinalized-statement)
            &key (if-row #'do-nothing))
    (loop
      (step-statement statement
                      :if-row if-row
                      :if-done (lambda () (return))))))

(defmacro do-rows ((row-variable statement) &body body)
  "(do-rows (row-variable statement) &body body)

Steps STATEMENT repeatedly, executing BODY with variable ROW-VARIABLE
bound to each row of the result set, until the statement
finishes. Equivalent to

(step-until-done statement :if-row (lambda (row-variable) [body]))

SEE ALSO:
step-statement
step-until-done"
  `(step-until-done ,statement :if-row (lambda (,row-variable) ,@body)))
