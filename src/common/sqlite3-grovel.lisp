;; I was able to come so far without this... but it looks like I'm
;; finally going to need to grovel.

(in-package :tqlite)

(include "sqlite3.h")

(ctype sqlite3-int64 "sqlite3_int64")

(constant (+sqlite-ok+ "SQLITE_OK"))

(constant (+sqlite-busy+ "SQLITE_BUSY"))

(constant (+sqlite-row+ "SQLITE_ROW"))

(constant (+sqlite-done+ "SQLITE_DONE"))

(constant (+sqlite-misuse+ "SQLITE_MISUSE"))
