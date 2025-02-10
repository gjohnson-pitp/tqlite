
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

(defmethod finalize-statement ((statement unfinalized-statement))
  (sqlite3-finalize (statement-pointer statement))
  (cancel-finalization statement)
  (change-class statement 'finalized-statement))

(defmethod database-error-message ((statement unfinalized-statement))
  (database-error-message (statement-database statement)))

(defmethod reset-statement ((statement unfinalized-statement))
  (foreign-funcall "sqlite3_reset"
                   :pointer (statement-pointer statement)
                   :int))

(defmethod reset-statement :after ((statement unbindable-statement))
  (change-class statement 'bindable-statement))
