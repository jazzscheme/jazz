;;; resqlite3/resqlite3.scm -- Copyright (C) 2012 Jussi Piitulainen

;;; This program is free software: you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public License
;;; as published by the Free Software Foundation, either version 3 of
;;; the License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.

;;; This is a Gambit-C foreign interface to SQLite3, all written in
;;; June 2012 by Jussi Piitulainen <jpiitula@ling.helsinki.fi> on an
;;; Ubuntu GNU/Linux system with Gambit-C version 4.6.6, GCC version
;;; 4.4.3 and SQLite3 version 3.6.22,, with Marco Benelli's minimal
;;; sqlite3-r1 from Gambit-C Dumping Grounds as a starting point.

;;; TODO: add sqlite3-bind-null (maybe, for completeness)

(unit sqlite.implementation.resqlite3

(c-declaration sqlite #<<C-END
#include <sqlite3.h>
// #include <stdlib.h> // calloc and free
#include <string.h> // memcpy
// #include <stdio.h>  // release_prepared_statement
C-END
)

(c-define-type sqlite3-database-connection
   (pointer (struct "sqlite3")
	    sqlite3-database-connection))

;;; The contract on the SQLite3 side is that (1) a prepared statement
;;; should be finalized to release the associated resources and (2) it
;;; is a "grievous error" to do anything with a finalized statement.
;;; The contract on the Gambit-C side is that (1) the release function
;;; is invoked at most once for any foreign object, (2) the garbage
;;; collector will invoke it if needed, and (3) the foreign object can
;;; only be used if it has not been released. (There does not seem to
;;; be any meaningful way to return to the caller an actual error
;;; status from sqlite3_finalize.) (If you hold on to the foreign
;;; object after its release, Gambit-C appears to pass a null pointer
;;; in its place. SQLite3 can handle it.)

(c-define-type sqlite3-prepared-statement
   (pointer (struct "sqlite3_stmt")
	    sqlite3-prepared-statement
	    "xxx_release_prepared_statement_xxx"))

(c-declaration sqlite #<<C-END
___SCMOBJ xxx_release_prepared_statement_xxx(void *stmt) {
   int status = sqlite3_finalize(stmt);
   // fputs("RELEASED PREPARED STATEMENT\n", stderr);
   if (status)
      return ___FIX(___UNKNOWN_ERR);
   else
      return ___FIX(___NO_ERR);
}
C-END
)

;;; Latest error codes and messages available to Scheme through either
;;; a connection or a statement

(c-external (%%-sqlite3-database-error-code sqlite3-database-connection) int "sqlite3_errcode")

(c-external (%%-sqlite3-database-error-message sqlite3-database-connection) char-string
#<<C-END
   const char *text = sqlite3_errmsg(___arg1);
   ___result = (char *)text;
C-END
)

(c-external (%%-sqlite3-statement-error-code sqlite3-prepared-statement) int
#<<C-END
  sqlite3 *db = sqlite3_db_handle(___arg1);
  ___result = sqlite3_errcode(db);
C-END
)

(c-external (%%-sqlite3-statement-error-message sqlite3-prepared-statement) char-string
#<<C-END
  sqlite3 *db = sqlite3_db_handle(___arg1);
  const char *text = sqlite3_errmsg(db);
  ___result = (char *)text;
C-END
)

(define (sqlite3-error-code obj)
  (cond
   ((and (foreign? obj)
	 (equal? (foreign-tags obj) '(sqlite3-database-connection)))
    (%%-sqlite3-database-error-code obj))
   ((and (foreign? obj)
	 (equal? (foreign-tags obj) '(sqlite3-prepared-statement)))
    (%%-sqlite3-statement-error-code obj))
   (else
    (error "not an sqlite3 connection or statement" obj))))

(define (sqlite3-error-message obj)
  (cond
   ((and (foreign? obj)
	 (equal? (foreign-tags obj) '(sqlite3-database-connection)))
    (%%-sqlite3-database-error-message obj))
   ((and (foreign? obj)
	 (equal? (foreign-tags obj) '(sqlite3-prepared-statement)))
    (%%-sqlite3-statement-error-message obj))
   (else
    (error "not an sqlite3 connection or statement" obj))))

;;; Open a database connection

;;; The low-level %%-sqlite3-open wraps the even lower-level foreign
;;; function so that the result is either a database connection (on
;;; success) or an error code (on failure). The higher-level
;;; sqlite3-open returns the database object or raises an exception.

(c-definition (%%-database-connection-%% db)
   (sqlite3-database-connection) scheme-object "database_connection" "static"
   db)

(c-external (%%-sqlite3-open char-string) scheme-object
#<<C-END
  sqlite3* db;
  int res = sqlite3_open(___arg1, &db);
  if (res) {
     sqlite3_close(db);
     ___result = ___FIX(res);
  }
  else {
     ___result = database_connection(db);
  }
C-END
)

(define (sqlite3-open filename)
   (let ((result (%%-sqlite3-open filename)))
     (if (integer? result)
	 (error "failed to open database" filename result)
	 result)))

;;; Close a database connection

;;; An SQLite3 database connection should be closed when no longer
;;; needed, but only after all its prepared statements have been
;;; finalized (and blob handles also finalized, but resqlite3 does not
;;; provide those). They are not set up to be closed when collected,
;;; because that would roll back any transaction and invalidate any
;;; prepared statements. Also, a copy of the connection would still be
;;; available through a prepared statement. (Statements are finalized
;;; on collection, and a user program can will a connection to keep
;;; trying to be closed.)

(c-external (%%-sqlite3-close sqlite3-database-connection) int "sqlite3_close")

(define (sqlite3-close database)
  (let ((status (%%-sqlite3-close database)))
    (if (not (zero? status))
	(error "failed to close database" database status))))

;;; Prepare an SQL statement

;;; The low-level %%-sqlite3-prepare wraps the even lower-level
;;; foreign function so that the result is either a prepared statement
;;; (on success) or an error code (on failure). The higher level
;;; sqlite3-prepare returns the prepared statement or raises an
;;; exception.

(c-definition (%%-prepared-statement-%% stmt)
   (sqlite3-prepared-statement) scheme-object "prepared_statement" "static"
   stmt)

(c-external (%%-sqlite3-prepare sqlite3-database-connection char-string) scheme-object
#<<C-END
  sqlite3_stmt *stmt;
  const char *rest;
  int res = sqlite3_prepare_v2(___arg1, ___arg2, -1, &stmt, &rest);
  if (res) {
     sqlite3_finalize(stmt);
     ___result = ___FIX(res);
  }
  else {
     ___result = prepared_statement(stmt);
  }
C-END
)

(define (sqlite3-prepare database statement-text)
  (let ((result (%%-sqlite3-prepare database statement-text)))
    (if (integer? result)
	(error "prepare failed" result statement-text)
	result)))

(c-external (sqlite3-db-handle sqlite3-prepared-statement) sqlite3-database-connection
     "sqlite3_db_handle")

;;; Executing a prepared statement

;;; The low-level %%-sqlite3-step returns a numerical status code. The
;;; higher-level sqlite3-step! returns true for a row and false for
;;; done, or raises an exception.

(c-external (%%-sqlite3-step sqlite3-prepared-statement) int "sqlite3_step")

(define (sqlite3-step! statement)
  (let ((status (%%-sqlite3-step statement)))
    (case status
      ((100) #t) ;row
      ((101) #f) ;done
      ((5)
       (error "database busy" status))
      (else
       (error "step failed" status)))))

(c-external (%%-sqlite3-reset sqlite3-prepared-statement) int "sqlite3_reset")

(define (sqlite3-reset! statement)
  (let ((status (%%-sqlite3-reset statement)))
    (if (not (zero? status))
	(error "reset failed" status))))

(define (sqlite3-finalize! statement)
  (foreign-release! statement))

;;; Accessing the current row

(c-external (sqlite3-column-count sqlite3-prepared-statement) int "sqlite3_column_count")

(c-external (%%-sqlite3-column-type sqlite3-prepared-statement int) int "sqlite3_column_type")

(define (sqlite3-column-type statement k)
  (let ((type (%%-sqlite3-column-type statement k)))
    (case type
      ((1) 'integer)  ;64-bit signed integer
      ((2) 'float)    ;64-bit IEEE floating point number
      ((3) 'text)     ;string
      ((4) 'blob)     ;blob
      ((5) 'null)     ;null
      (else
       (error "column type error code" type)))))

(define (sqlite3-column-name statement k)
  (c-function sqlite3_column_name (sqlite3-prepared-statement int) char-string
#<<C-END
  const char *text = sqlite3_column_name(___arg1, ___arg2);
  ___result = (char *)text;
C-END
))

(c-external (sqlite3-column-int sqlite3-prepared-statement int) int "sqlite3_column_int")

(c-external (sqlite3-column-int64 sqlite3-prepared-statement int) int64 "sqlite3_column_int64")

(c-external (sqlite3-column-double sqlite3-prepared-statement int) double "sqlite3_column_double")

(c-external (sqlite3-column-text sqlite3-prepared-statement int) char-string
#<<C-END
   const char *text = sqlite3_column_text(___arg1, ___arg2);
   ___result = (char *)text;
C-END
)

;;; Blobs can be accessed as Gambit-C homogeneous vectors, below. Raw
;;; blob intentionally not available - stepping the statement further
;;; would invalidate them anyway.

;;; Binding values to prepared-statement parameters

;;; The provided binding commands indicate the parameter by its index
;;; in the statement. The low-level %%-sqlite3-bind* return a success
;;; or error code. The higher-level sqlite3-bind-*! raise an exception
;;; if they do not succeed.

;;; Access bind parameters

(c-external (sqlite3-bind-parameter-count sqlite3-prepared-statement) int "sqlite3_bind_parameter_count")

(c-external (sqlite3-bind-parameter-name sqlite3-prepared-statement int) char-string
#<<C-END
   const char *name = sqlite3_bind_parameter_name(___arg1, ___arg2);
   ___result = (char *)name;
C-END
)

(c-external (%%-sqlite3-bind-parameter-index sqlite3-prepared-statement char-string) int "sqlite3_bind_parameter_index")

(define (sqlite3-bind-parameter-index statement name)
  (let ((k (%%-sqlite3-bind-parameter-index statement name)))
    (if (zero? k) (error "no such parameter" name) k)))

;;; Bind int (presumably 32 bit integer)

(c-external (%%-sqlite3-bind-int sqlite3-prepared-statement int int) int "sqlite3_bind_int")

(define (sqlite3-bind-int! statement k m)
  (let ((s (%%-sqlite3-bind-int statement k m)))
    (if (not (zero? s))	(error "bind-int error" s))))

;;; Bind int64

(c-external (%%-sqlite3-bind-int64 sqlite3-prepared-statement int int64) int "sqlite3_bind_int64")

(define (sqlite3-bind-int64! statement k m)
  (let ((s (%%-sqlite3-bind-int64 statement k m)))
    (if (not (zero? s))	(error "bind-int64 error" s))))

;;; Bind double

(c-external (%%-sqlite3-bind-double sqlite3-prepared-statement int float64) int "sqlite3_bind_double")

(define (sqlite3-bind-double! statement k x)
  (let ((s (%%-sqlite3-bind-double statement k x)))
    (if (not (zero? s))	(error "bind-double error" s))))

;;; Bind text

;;; This appears to work for UTF-8.

(c-external (%%-sqlite3-bind-text sqlite3-prepared-statement int char-string) int
#<<C-END
  ___result = sqlite3_bind_text(___arg1, ___arg2, ___arg3, -1, SQLITE_TRANSIENT);
C-END
)

(define (sqlite3-bind-text! statement k str)
  (let ((s (%%-sqlite3-bind-text statement k str)))
    (if (not (zero? s))	(error "bind-text error code" s))))


;;; And now for binding homogeneous vectors as blobs and accessing
;;; blobs as homogeneous vectors. The code is arranged by type, first
;;; exact integers types by the increasing number of bytes (unsigned
;;; binding and accessing, then signed), then the two floating point
;;; types similarly.

;;; The low-level binding operations return a success or error code.
;;; The higher-level versions raise an exception when they don't
;;; succeed.

;;; Scheme side names of the homogeneous vector oprations are not used
;;; anywhere, at least by people. They just duplicate the existing
;;; Gambit procedures that are not documented to be callable from C. I
;;; have uglified these names with the "%%%-" prefix. The C side names
;;; begin with "blob_" for a namespace of sorts. (Is that needed? No.)

;;; TODO: in sqlite3-bind-blob-code! check for non-subtyped (or
;;; otherwise prevent crashes)

(c-external (sqlite3-bind-blob-code! sqlite3-prepared-statement int scheme-object) int
#<<C-END
  void *buf = ___BODY_AS(___arg3,___tSUBTYPED);
  int n = ___HD_BYTES(___HEADER(___arg3));
  ___result = sqlite3_bind_blob(___arg1, ___arg2, buf, n, SQLITE_TRANSIENT);
C-END
)

(c-declaration sqlite
#<<c-declare-end
static ___SCMOBJ
resqlite3_column_blob(sqlite3_stmt* stmt, int col, int type, int size)
{
   const void *buf = sqlite3_column_blob(stmt, col);
   int n = sqlite3_column_bytes(stmt, col);
   if (n % size) return ___FIX(___UNKNOWN_ERR);
   ___SCMOBJ result = ___EXT(___alloc_scmobj)(___ps,type,n);
   if (___FIXNUMP(result)) return result;
   memcpy(___BODY(result), buf, n);
   return result;
}
c-declare-end
)

(c-external (sqlite3-column-u8vector-or-code
  (sqlite3-prepared-statement int) scheme-object
   "___result = resqlite3_column_blob(___arg1,___arg2,___sU8VECTOR,1);")

(c-external (sqlite3-column-s8vector-or-code
  (sqlite3-prepared-statement int) scheme-object
   "___result = resqlite3_column_blob(___arg1,___arg2,___sS8VECTOR,1);")

(c-external (sqlite3-column-u16vector-or-code
  (sqlite3-prepared-statement int) scheme-object
   "___result = resqlite3_column_blob(___arg1,___arg2,___sU16VECTOR,2);")

(c-external (sqlite3-column-s16vector-or-code
  (sqlite3-prepared-statement int) scheme-object
   "___result = resqlite3_column_blob(___arg1,___arg2,___sS16VECTOR,2);")

(c-external (sqlite3-column-u32vector-or-code
  (sqlite3-prepared-statement int) scheme-object
   "___result = resqlite3_column_blob(___arg1,___arg2,___sU32VECTOR,4);")

(c-external (sqlite3-column-s32vector-or-code
  (sqlite3-prepared-statement int) scheme-object
   "___result = resqlite3_column_blob(___arg1,___arg2,___sS32VECTOR,4);")

(c-external (sqlite3-column-f32vector-or-code
  (sqlite3-prepared-statement int) scheme-object
   "___result = resqlite3_column_blob(___arg1,___arg2,___sF32VECTOR,4);")

(c-external (sqlite3-column-u64vector-or-code
  (sqlite3-prepared-statement int) scheme-object
   "___result = resqlite3_column_blob(___arg1,___arg2,___sU64VECTOR,8);")

(c-external (sqlite3-column-s64vector-or-code
  (sqlite3-prepared-statement int) scheme-object
   "___result = resqlite3_column_blob(___arg1,___arg2,___sS64VECTOR,8);")

(c-external (sqlite3-column-f64vector-or-code
  (sqlite3-prepared-statement int) scheme-object
   "___result = resqlite3_column_blob(___arg1,___arg2,___sF64VECTOR,8);")

;;; --- The mid-level interface ---

(define (sqlite3-bind-blob! statement k obj)
  (let ((code (sqlite3-bind-blob-code! statement k obj)))
    (or (zero? code)
	(error "error code from bind" code))))

(define-macro (ensure-blob expr)
  `(let ((val ,expr))
     (if (number? val) (error "error code instead of blob" val) val)))

(define (sqlite3-column-u8vector statement k)
  (ensure-blob (sqlite3-column-u8vector-or-code statement k)))

(define (sqlite3-column-s8vector statement k)
  (ensure-blob (sqlite3-column-s8vector-or-code statement k)))

(define (sqlite3-column-u16vector statement k)
  (ensure-blob (sqlite3-column-u16vector-or-code statement k)))

(define (sqlite3-column-s16vector statement k)
  (ensure-blob (sqlite3-column-s16vector-or-code statement k)))

(define (sqlite3-column-u32vector statement k)
  (ensure-blob (sqlite3-column-u32vector-or-code statement k)))

(define (sqlite3-column-s32vector statement k)
  (ensure-blob (sqlite3-column-s32vector-or-code statement k)))

(define (sqlite3-column-f32vector statement k)
  (ensure-blob (sqlite3-column-f32vector-or-code statement k)))

(define (sqlite3-column-u64vector statement k)
  (ensure-blob (sqlite3-column-u64vector-or-code statement k)))

(define (sqlite3-column-s64vector statement k)
  (ensure-blob (sqlite3-column-s64vector-or-code statement k)))

(define (sqlite3-column-f64vector statement k)
  (ensure-blob (sqlite3-column-f64vector-or-code statement k))))
