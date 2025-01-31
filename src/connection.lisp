
(in-package :tqlite)

(defclass connection ()
  ((sqlite3-pointer :reader sqlite3-pointer
                    :initarg :sqlite3-pointer)))

(defgeneric perform-database-command (connection command))

(defclass closed-connection (connection)
  ())

;; TODO: Actually, I intended to have my error classes indicate what
;; operation failed, not why there was a failure, so this won't do; we
;; need to have each command construct its own error. Then again,
;; trying to run a command on an already-closed database seems like a
;; more "fundamental" error than trying to perform an operation that's
;; forbidden at the application level, so maybe it's fine this way?
(define-condition cannot-run-command (error)
  ((connection :reader error-database-connection
               :initarg :connection)
   (command :reader error-database-command
            :initarg :command))
  (:report (lambda (condition stream)
             (format stream
                     "Cannot perform any more commands on connection ~S ~
                      because it is already closed."
                     (error-database-connection condition)))))

(defmethod perform-database-command ((connection closed-connection) command)
  (error 'cannot-run-command
         :connection connection
         :command command))

(defclass open-connection (connection)
  ())

(defmethod initialize-instance :after ((connection open-connection)
                                       &key sqlite3-pointer)
  (finalize connection
            (lambda ()
              (sqlite3-close sqlite3-pointer))))

(define-condition cannot-connect-to-database (sqlite3-error)
  ((name :reader error-database-name
         :initarg :name))
  (:report (lambda (condition stream)
             (format stream
                     "Failed to connect to database ~S with message: ~S"
                     (error-database-name condition)
                     (error-message condition)))))

(defun open-database (name)
  (with-foreign-object (pointer-to-pointer :pointer)
    (when-fail (message (foreign-funcall "sqlite3_open"
                                         :string name
                                         :pointer pointer-to-pointer
                                         :int))
      (error 'cannot-connect-to-database
             :name name
             :message message))
    (make-instance 'open-connection
                   :sqlite3-pointer (mem-ref pointer-to-pointer :pointer))))

(defun sqlite3-close (pointer)
  ;; TODO: Do something if this fails?
  (foreign-funcall "sqlite3_close_v2"
                   :pointer pointer
                   :int))

(defmethod close-database ((connection open-connection))
  (sqlite3-close (sqlite3-pointer connection))
  (cancel-finalization connection)
  (change-class connection 'closed-connection))
