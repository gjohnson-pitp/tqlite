
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

(defclass finished-statement (unfinalized-statement)
  ())

(defclass unfinished-statement (unfinalized-statement)
  ())

(defun sqlite3-finalize (statement-pointer)
  (foreign-funcall "sqlite3_finalize"
                   :pointer statement-pointer
                   :int))

(defmethod initialize-instance :after ((statement unfinalized-statement)
                                       &key pointer)
  (finalize statement
            (lambda () (sqlite3-finalize pointer))))

(defmethod finalize-statement ((statement unfinalized-statement))
  (sqlite3-finalize (statement-pointer statement))
  (cancel-finalization statement)
  (change-class statement 'finalized-statement))

(defmethod database-error-message ((statement unfinalized-statement))
  (database-error-message (statement-database statement)))

;; "Row pointer" actually points to a statement, since SQLite3 has you
;; access result rows through the statement. But this class is only
;; instantiated when sqlite3_step returns SQLITE_ROW.
(defclass row ()
  ((pointer :reader row-pointer
            :initarg :pointer)))

(defun sqlite3-step (statement-pointer)
  (foreign-funcall "sqlite3_step"
                   :pointer statement-pointer
                   :int))

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
             (write-string "SQLITE_MISUSE returned. Your program and/or this library is broken!"
                           stream))))

(defmethod step-statement ((statement unfinished-statement) row-callback)
  (tagbody
     retry
     (case (sqlite3-step (statement-pointer statement))
       (+sqlite-done+
        (change-class statement 'finished-statement))
       (+sqlite-row+
        (funcall row-callback
                 (make-instance 'row
                                :pointer (statement-pointer statement))))
       (+sqlite-busy+
        (restart-case (error 'step-statement-failed
                             :statement statement
                             :message (database-error-message statement))
          (retry ()
            :report "Try stepping the statement again"
            (go retry))))
       (+sqlite-misuse+
        ;; TODO: Replace this with whatever more helpful error you come up with
        (error 'sqlite-misuse-returned))
       (otherwise
        (error 'step-statement-failed
               :statement statement
               :message (database-error-message statement))))))

(defmethod reset-statement ((statement unfinalized-statement))
  (foreign-funcall "sqlite3_reset"
                   :pointer (statement-pointer statement)
                   :int))

(defmethod reset-statement :after ((statement finished-statement))
  (change-class statement 'unfinished-statement))
