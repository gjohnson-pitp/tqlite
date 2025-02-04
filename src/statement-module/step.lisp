
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

(defun sqlite3-step (statement-pointer)
  (foreign-funcall "sqlite3_step"
                   :pointer statement-pointer
                   :int))

(defmethod try-step-statement ((statement unfinalized-statement))
  ;; It'd probably be better to defparameter an alist to determine the
  ;; class, but it's annoying to construct those when the keys are
  ;; variable/constant names rather than literals...
  (make-instance (case (sqlite3-step (statement-pointer statement))
                   (+sqlite-done+
                    'statement-done)
                   (+sqlite-row+
                    'statement-has-row)
                   (+sqlite-busy+
                    'statement-failed-busy)
                   (+sqlite-misuse+
                    'statement-failed-misuse)
                   (otherwise
                    'statement-failed-error))
                 :statement statement))

(defun do-nothing (&rest args)
  (declare (ignore args)))

(defmethod step-statement ((statement unfinalized-statement)
                           &key
                             (if-row #'do-nothing)
                             (if-done #'do-nothing))
  (act-on-step-result (try-step-statement statement)
                      if-row
                      if-done))
