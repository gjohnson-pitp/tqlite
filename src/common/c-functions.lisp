
(in-package :tqlite)

(defcfun "sqlite3_bind_blob" :int
  (statement :pointer)
  (index :int)
  (blob-pointer :pointer)
  (blob-length :int)
  (deconstructor-callback :pointer))

(defcfun "sqlite3_bind_double" :int
  (statement :pointer)
  (index :int)
  (value :double))

(defcfun "sqlite3_bind_int" :int
  (statement :pointer)
  (index :int)
  (value :int))

(defcfun "sqlite3_bind_null" :int
  (statement :pointer)
  (index :int))

(defcfun "sqlite3_bind_parameter_index" :int
  (statement :pointer)
  (name :string))

(defcfun "sqlite3_bind_text" :int
  (statement :pointer)
  (index :int)
  (string :pointer)
  (length :int)
  (deconstructor-callback :pointer))

(defcfun "sqlite3_clear_bindings" :int
  (statement :pointer))

(defcfun "sqlite3_close_v2" :int
  (connection :pointer))

(defcfun "sqlite3_column_blob" :pointer
  (statement :pointer)
  (column-index :int))

(defcfun "sqlite3_column_bytes" :int
  (statement :pointer)
  (column-index :int))

(defcfun "sqlite3_column_count" :int
  (statement :pointer))

(defcfun "sqlite3_column_double" :double
  (statement :pointer)
  (column-index :int))

(defcfun "sqlite3_column_int" :int
  (statement :pointer)
  (column-index :int))

(defcfun "sqlite3_column_text" :string
  (statement :pointer)
  (column-index :int))

(defcfun "sqlite3_column_type" :int
  (statement :pointer)
  (column-index :int))

(defcfun "sqlite3_errmsg" :string
  (connection :pointer))

(defcfun "sqlite3_finalize" :int
  (statement :pointer))

(defcfun "sqlite3_last_insert_rowid" sqlite3-int64
  (connection :pointer))

(defcfun "sqlite3_open" :int
  (filename :string)
  (connection-pointer :pointer))

(defcfun "sqlite3_prepare_v2" :int
  (connection :pointer)
  (sql-code :string)
  (length :int)
  (statement-pointer :pointer)
  (string-pointer :pointer))

(defcfun "sqlite3_prepare_v3" :int
  (connection :pointer)
  (sql-code :string)
  (length :int)
  (flags :uint)
  (statement-pointer :pointer)
  (tail :pointer))

(defcfun "sqlite3_reset" :int
  (statement :pointer))

(defcfun "sqlite3_step" :int
  (statement :pointer))
