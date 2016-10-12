;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; lib/news-reader/sqlite3.scm - Id generator for SQLite3
;;;
;;;   Copyright (c) 2016  Takashi Kato  <ktakashi@ymail.com>
;;;
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

(library (news-reader sqlite3)
  (export generator)
  (import (rnrs)
	  (rnrs mutable-pairs)
	  (sagittarius)
	  (dbi)
	  (srfi :18))

  ;; in our table model, we don't use autoincrement keyword for
  ;; simplicity of SQL script. So what we do here is basically
  ;; manual sequence management.
  (define-constant +table-lists+
    "select tbl_name from sqlite_master where type = 'table'")
  (define-constant +max-id+ "select max(id) from ")
  (define (init-sequence dbi-conn)
    (define table (make-eq-hashtable))
    (define query (dbi-execute-query-using-connection! dbi-conn +table-lists+))
    (dbi-do-fetch! (r query)
		   (let* ((n (string-downcase (vector-ref r 0)))
			  (q (dbi-execute-query-using-connection!
			      dbi-conn (string-append +max-id+ n))))
		     (let ((v (vector-ref (dbi-fetch! q) 0)))
		       (dbi-close q)
		       (hashtable-set! table (string->symbol n)
			 (cons (make-mutex) (if (null? v) 1 (+ v 1)))))))
    (dbi-close query)
    table)

  (define (symbol-downcase s)
    (string->symbol (string-downcase (symbol->string s))))
  (define generator
    (let ((mutex (make-mutex))
	  (sequence #f))
      (lambda (dbi-conn table)
	(unless sequence
	  (mutex-lock! mutex)
	  (unless sequence (set! sequence (init-sequence dbi-conn)))
	  (mutex-unlock! mutex))

	(let* ((t (symbol-downcase table))
	       (p (hashtable-ref sequence t #f)))
	  (unless p (assertion-violation 'sqlite3-generator "unknown table" t))
	  (mutex-lock! (car p))
	  (let ((r (cdr p)))
	    (set-cdr! p (+ r 1))
	    (mutex-unlock! (car p))
	    r)))))


  )

