
(in-package :tqlite)

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

(defun sqlite3-column-type (pointer index)
  (foreign-funcall "sqlite3_column_type"
                   :pointer pointer
                   :int index
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

(defmethod make-column ((row row) (column integer))
  (if (<= 0 column (1- (column-count row)))
      (make-instance (column-type-class (sqlite3-column-type (row-pointer row)
                                                             column))
                     :pointer (row-pointer row)
                     :index column)
      (error 'cannot-make-column
             :row row
             :index column)))

;; TODO: Rename this to column-value, and rename
;; what's-called-column-value-now to something else--e.g.,
;; typed-column-value or something?
(defmethod get-column ((row row) (column integer))
  (column-value (make-column row column)))
