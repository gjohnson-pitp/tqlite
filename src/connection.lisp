
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

(defmethod close-database ((connection open-connection))
  (sqlite3-close (sqlite3-pointer connection))
  (cancel-finalization connection)
  (change-class connection 'closed-connection))

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

;; TODO: Use classes to indicate whether this succeeded for
;; consistency with everything else?
(defmethod prepare-statement ((connection open-connection) (code string))
  (with-foreign-object (pointer-to-pointer :pointer)
    (if (eql +sqlite-ok+
             (foreign-funcall "sqlite3_prepare_v2"
                              :pointer (sqlite3-pointer connection)
                              :string code
                              :int -1
                              :pointer pointer-to-pointer
                              :pointer (null-pointer)
                              :int))
        (make-instance 'unfinalized-statement
                       :database connection
                       :pointer (mem-ref pointer-to-pointer :pointer))
        (error 'cannot-prepare-statement
               :database connection
               :message (database-error-message connection)
               :sql code))))
