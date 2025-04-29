
(in-package :tqlite)

(defclass connection ()
  ((pointer :reader sqlite3-pointer
            :initarg :pointer)))

(defclass closed-connection (connection)
  ())

(defclass open-connection (connection)
  ())

(defun sqlite3-close (pointer)
  ;; TODO: Do something if this fails?
  (foreign-funcall "sqlite3_close_v2"
                   :pointer pointer
                   :int))

(defmethod initialize-instance :after ((connection open-connection)
                                       &key sqlite3-pointer)
  (finalize connection
            (lambda ()
              (sqlite3-close sqlite3-pointer))))

(defgeneric close-database (connection)
  (:documentation "(close-database connection)

Closes the given database connection. A database connection object
should be closed when it's no longer needed. However, if you lose a
reference to a connection object without getting to close it, then it
will be closed automatically when it's garbage collected. If you do
close it yourself, then it will not be closed again when it's garbage
collected.

Closing a connection object causes it to change class; attempting to
perform any further operations on it (including closing again) will
then result in a NO-APPLICABLE-METHOD error. Therefore, it's
impossible to accidentally perform invalid operations like closing a
database twice during normal use of this package.

SEE ALSO:
open-database")
  (:method ((connection open-connection))
    (sqlite3-close (sqlite3-pointer connection))
    (cancel-finalization connection)
    (change-class connection 'closed-connection)))

(defmethod database-error-message ((connection open-connection))
  (foreign-funcall "sqlite3_errmsg"
                   :pointer (sqlite3-pointer connection)
                   :string))

(defclass connection-result ()
  ((name :reader result-database-name
         :initarg :name)
   (connection :reader result-connection
               :initarg :connection)))

(defclass failed-connection (connection-result)
  ())

(defclass successful-connection (connection-result)
  ())

(defun try-open-database (name)
  (with-foreign-object (pointer-to-pointer :pointer)
    (let* ((return-code (foreign-funcall "sqlite3_open"
                                         :string name
                                         :pointer pointer-to-pointer
                                         :int))
           (connection (make-instance 'open-connection
                                      :pointer (mem-ref pointer-to-pointer
                                                        :pointer))))
      (make-instance (if (eql return-code +sqlite-ok+)
                         'successful-connection
                         'failed-connection)
                     :name name
                     :connection connection))))

(defmethod validate-connection ((result successful-connection))
  (result-connection result))

(define-condition cannot-connect-to-database (sqlite3-error)
  ((name :reader error-database-name
         :initarg :name))
  (:report (lambda (condition stream)
             (format stream
                     "Failed to connect to database ~S with message: ~S"
                     (error-database-name condition)
                     (error-message condition)))))

(defmethod validate-connection ((result failed-connection))
  (let ((message (database-error-message (result-connection result))))
    (close-database (result-connection result))
    (error 'cannot-connect-to-database
           :name (result-database-name result)
           :message message)))

(defun open-database (name)
  "(open-database name) => database-connection

Takes a string denoting a valid SQLite3 database and returns a
database connection object. On failure, signals a condition of type
CANNOT-CONNECT-TO-DATABASE.

The connection object should be closed with CLOSE-DATABASE when it's
no longer needed; however, if you lose a reference to a connection
object without getting to close it, it will be closed automatically
when it's garbage collected. Attempting to perform operations on a
database that's already been closed will result in an error. See
CLOSE-DATABASE for details.

SEE ALSO:
close-database
prepare-statement
with-statement
execute-sql"
  (validate-connection (try-open-database name)))

(define-condition cannot-prepare-statement (sqlite3-error)
  ((database :reader error-database
             :initarg :database)
   (sql :reader error-sql
        :initarg :sql))
  (:report (lambda (condition stream)
             (format stream
                     "Database ~S failed to prepare a statement from the given ~
                      SQL code with message: ~S"
                     (error-database condition)
                     (error-message condition)))))

;; DRY'ed out code that was common to prepare-statement and
;; prepare-persistent-statement below. It takes a callback to allow
;; pointer-to-pointer to be allocated in the subroutine rather than
;; the caller
(defun try-prepare-statement (connection callback code)
  (with-foreign-object (pointer-to-pointer :pointer)
    (if (eql +sqlite-ok+ (funcall callback pointer-to-pointer))
        (make-instance 'bindable-statement
                       :database connection
                       :pointer (mem-ref pointer-to-pointer :pointer))
        (error 'cannot-prepare-statement
               :database connection
               :message (database-error-message connection)
               :sql code))))

(defgeneric prepare-statement (connection code)
  (:documentation "(prepare-statement connection code) => statement-object

Uses database connection CONNECTION to compile SQL code CODE into a
prepared statement, then returns an object representing this
statement. Signals a condition of type CANNOT-PREPARE-STATEMENT on
failure.

Currently, code containing multiple SQL statements is not
supported. Only the first SQL statement will be executed.

The statement object should be finalized with FINALIZE-STATEMENT when
it's no longer needed; however, if you lose a reference to a statement
object without getting to finalize it, it will be finalized
automatically when it's garbage collected. Attempting to perform
operations on a statement that's already been finalized will result in
an error. See FINALIZE-STATEMENT for details.

SEE ALSO:
prepare-persistent-statement
finalize-statement
with-statement
bind-parameter
step-statement
step-until-done
do-rows
reset-statement")
  (:method ((connection open-connection) (code string))
    (try-prepare-statement connection
                           (lambda (pointer-to-pointer)
                             (foreign-funcall "sqlite3_prepare_v2"
                                              :pointer (sqlite3-pointer connection)
                                              :string code
                                              :int -1
                                              :pointer pointer-to-pointer
                                              :pointer (null-pointer)
                                              :int))
                           code)))

(defconstant +sqlite-prepare-persistent+ #x01)

(defgeneric prepare-persistent-statement (connection code)
  (:documentation "(prepare-persistent-statement connection code) => statement-object

Prepares a persistent statement object. A persistent statement is
identical to any other statement from Lisp's perspective, but SQLite
will store it differently.

See PREPARE-STATEMENT for details.")
  (:method ((connection open-connection) (code string))
    (try-prepare-statement connection
                           (lambda (pointer-to-pointer)
                             (foreign-funcall "sqlite3_prepare_v3"
                                              :pointer (sqlite3-pointer connection)
                                              :string code
                                              :int -1
                                              :uint +sqlite-prepare-persistent+
                                              :pointer pointer-to-pointer
                                              :pointer (null-pointer)
                                              :int))
                           code)))

(defun call-with-statement (connection code function)
  (let ((statement (prepare-statement connection code)))
    (unwind-protect (funcall function statement)
      (finalize-statement statement))))

(defmacro with-statement ((variable connection code) &body body)
  "(with-statement (variable connection code) &body body)

Compiles SQL code CODE into a statement using CONNECTION, then
executes BODY with VARIABLE bound to the resulting statement
object. FINALIZE-STATEMENT is automatically called on the statement
upon leaving the body, even if a non-local exit occurs.

SEE ALSO:
prepare-statement
finalize-statement"
  `(call-with-statement ,connection ,code (lambda (,variable) ,@body)))

(defun execute-sql (connection code)
  "(execute-sql connection code)

Executes the given SQL code CODE on database connection
CONNECTION. Preparation, stepping, and finalization of statements is
all done automatically. Any result set will not be accessible.

Currently, code containing multiple SQL statements is not
supported. Only the first statement will be executed.

SEE ALSO:
prepare-statement"
  (with-statement (statement connection code)
    (step-until-done statement)))
