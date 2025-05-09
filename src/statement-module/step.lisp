
(in-package :tqlite)

(defgeneric step-statement (statement &key if-row if-done))

(define-condition step-statement-failed (sqlite3-error)
  ((statement :reader error-statement
              :initarg :statement))
  (:report (lambda (condition stream)
             (format stream
                     "Stepping the statement ~S failed with message: ~S"
                     (error-statement condition)
                     (error-message condition)))))

;; TODO: Replace this with a more helpful error class
(define-condition sqlite-misuse-returned (error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream
                     "SQLITE_MISUSE returned. Your program and/or this library ~
                      is broken!"))))

(defclass step-result ()
  ((statement :reader result-statement
              :initarg :statement)))

(defmethod statement-pointer ((result step-result))
  (statement-pointer (result-statement result)))

(defmethod database-error-message ((result step-result))
  (database-error-message (result-statement result)))

(defgeneric act-on-step-result (step-result if-row if-done))

(defclass statement-done (step-result)
  ())

(defmethod act-on-step-result ((result statement-done) if-row if-done)
  (funcall if-done))

(defclass statement-has-row (step-result)
  ())

(defmethod act-on-step-result ((result statement-has-row) if-row if-done)
  (funcall if-row
           (make-instance 'row :pointer (statement-pointer result))))

(defclass statement-failed-busy (step-result)
  ())

;; The only time we're allowed to retry stepping a statement
(defmethod act-on-step-result ((result statement-failed-busy) if-row if-done)
  (restart-case (error 'step-statement-failed
                       :statement (result-statement result)
                       :message (database-error-message result))
    (retry ()
      :report "Try stepping the statement again."
      (step-statement (result-statement result)
                      :if-row if-row
                      :if-done if-done))))

(defclass statement-failed-misuse (step-result)
  ())

;; Can't treat this the same as other errors because, according to the
;; docs, the error code and error message "may or may not be set"
(defmethod act-on-step-result ((result statement-failed-misuse) if-row if-done)
  ;; TODO: Replace this with whatever more helpful error class you
  ;; come up with
  (error 'sqlite-misuse-returned))

(defclass statement-failed-error (step-result)
  ())

(defmethod act-on-step-result ((result statement-failed-error) if-row if-done)
  (error 'step-statement-failed
         :statement (result-statement result)
         :message (database-error-message result)))

;; I tried using a case statement for this at first because I thought
;; it'd be less annoying to write, but it turns out that case
;; statements don't evaluate the keys in their clauses (i.e. the key
;; in each clause would be the name of the constant rather than the
;; actual constant), so it's equally annoying to write either way :D
(defparameter *step-result-classes*
  `((,+sqlite-done+ . statement-done)
    (,+sqlite-row+ . statement-has-row)
    (,+sqlite-busy+ . statement-failed-busy)
    (,+sqlite-misuse+ . statement-failed-misuse)))

(defun step-result-class (result-code)
  (cdr (or (assoc result-code *step-result-classes*)
           (return-from step-result-class 'statement-failed-error))))

(defmethod try-step-statement ((statement unfinalized-statement))
  (make-instance (step-result-class (sqlite3-step (statement-pointer statement)))
                 :statement statement))

;; We have to change class at this point because try-step-statement is
;; where sqlite3_step is called
(defmethod try-step-statement :after ((statement bindable-statement))
  (change-class statement 'unbindable-statement))

(defun do-nothing (&rest args)
  (declare (ignore args)))

(defgeneric step-statement (statement &key if-row if-done)
  (:documentation "(step-statement statement &key if-row if-done)

Steps the given statement. IF-ROW should be a function of one
argument, and IF-DONE a function of no arguments. A default is
provided for each if not passed.

If stepping the statement returns a result row, then IF-ROW is called
with a ROW object as argument. Columns can be retrieved from the ROW
object using GET-COLUMN. The ROW object is only guaranteed to be valid
during the execution of the IF-ROW callback, and so should not be
saved.

If the statement finished, IF-DONE is called with no arguments. Use
this to, e.g., exit from loops.

Once a statement has been stepped at least once, it must be reset
before any new parameters can be bound to it.

For the common case of stepping a statement until it's done, use
STEP-UNTIL-DONE. As a shorthand for calling STEP-UNTIL-DONE with an
IF-ROW callback, use the macro DO-ROWS.

SEE ALSO:
get-column
step-until-done
do-rows")
  (:method ((statement unfinalized-statement)
            &key
              (if-row #'do-nothing)
              (if-done #'do-nothing))
    (act-on-step-result (try-step-statement statement)
                        if-row
                        if-done)))
