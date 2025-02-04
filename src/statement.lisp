
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

(defgeneric step-statement (statement &key if-row if-done))

;; "Row pointer" actually points to a statement, since SQLite3 has you
;; access result rows through the statement. But this class is only
;; instantiated when sqlite3_step returns SQLITE_ROW.
(defclass row ()
  ((pointer :reader row-pointer
            :initarg :pointer)))

(defmethod column-count ((row row))
  (foreign-funcall "sqlite3_column_count"
                   :pointer (row-pointer row)
                   :int))

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

(defun do-nothing (&rest args)
  (declare (ignore args)))

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

(defmethod step-statement ((statement unfinalized-statement)
                           &key
                             (if-row #'do-nothing)
                             (if-done #'do-nothing))
  (act-on-step-result (try-step-statement statement)
                      if-row
                      if-done))

(defmethod reset-statement ((statement unfinalized-statement))
  (foreign-funcall "sqlite3_reset"
                   :pointer (statement-pointer statement)
                   :int))

(define-condition cannot-make-column (error)
  ((row :reader error-row
        :initarg :row)
   (index :reader error-column-index
          :initarg :index))
  (:report (lambda (condition stream)
             (format stream
                     "Row ~S has no column with index ~S"
                     (error-row condition)
                     (error-column-index condition)))))

;; Each possible return type of sqlite3_column_type corresponds to an
;; index in this array. 0 shouldn't be returned.
(defparameter *column-data-types*
  #(nil
    int-column
    float-column
    text-column
    blob-column
    null-column))

(defun column-type-class (type-code)
  (aref *column-data-types* type-code))

(defclass column-existence ()
  ((row :reader column-row
        :initarg :row)
   (index :reader column-index
          :initarg :index)))

(defmethod row-pointer ((column column-existence))
  (row-pointer (column-row column)))

(defclass column-exists (column-existence)
  ())

(defun sqlite3-column-type (pointer)
  (foreign-funcall "sqlite3_column_type"
                   :pointer pointer
                   :int))

(defmethod make-column-if-possible ((column column-exists))
  (make-instance (column-type-class (sqlite3-column-type (row-pointer column)))
                 :index (column-index column)))

(defclass column-does-not-exist (column-existence)
  ())

(defmethod make-column-if-possible ((column column-does-not-exist))
  (error 'cannot-make-column
         :row (column-row column)
         :index (column-index column)))

(defmethod try-make-column ((row row) (column integer))
  (make-instance (if (<= 0 column (1- (column-count (column-row column))))
                     'column-exists
                     'column-does-not-exist)
                 :row row
                 :index column))

(defmethod make-column ((row row) (column integer))
  (make-column-if-possible (try-make-column row column)))

(defclass column-type ()
  ((pointer :reader row-pointer
            :initarg :pointer)
   (index :reader column-index
          :initarg :index)))

;; I wanted to use a template method for all the methods on this
;; generic function, but unfortunately it looks like foreign-funcall
;; is a macro, not a function, so the return type has to be known at
;; compile time.
(defgeneric column-value (column-type))

(defclass int-column (column-type)
  ())

(defmethod column-value ((column int-column))
  (foreign-funcall "sqlite3_column_int"
                   :pointer (row-pointer column)
                   :int (column-index column)
                   :int))

(defclass float-column (column-type)
  ())

(defmethod column-value ((column float-column))
  (foreign-funcall "sqlite3_column_double"
                   :pointer (row-pointer column)
                   :int (column-index column)
                   :double))

(defclass text-column (column-type)
  ())

(defmethod column-value ((column text-column))
  ;; The declared return type is a pointer to "unsigned" char, so
  ;; maybe double-check that it plays nice with CFFI's :string return
  ;; type...
  (foreign-funcall "sqlite3_column_text"
                   :pointer (row-pointer column)
                   :int (column-index column)
                   :string))

(defclass blob-column (column-type)
  ())

(defmethod column-value ((column blob-column))
  ;; I could've sworn there was a function to do this automatically in
  ;; the CFFI, but I couldn't find anything in the documentation, so...
  (loop
    with byte-count = (foreign-funcall "sqlite3_column_bytes"
                                       :pointer (row-pointer column)
                                       :int (column-index column)
                                       :int)
    with foreign-bytes = (foreign-funcall "sqlite3_column_blob"
                                          :pointer (row-pointer column)
                                          :int (column-index column)
                                          :pointer)
    with array = (make-array byte-count :element-type 'unsigned-byte)
    for i from 0 below byte-count
    do (setf (aref array i)
             (mem-ref foreign-bytes :uint8 i))
    finally (return array)))

(defclass null-column (column-type)
  ())

(defmethod column-value ((column null-column))
  nil)
