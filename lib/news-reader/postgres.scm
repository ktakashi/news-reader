;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; lib/news-reader/postgres.scm - Id generator for PostgreSQL
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

(library (news-reader postgres)
    (export dialect-procedures
	    max-connection)
    (import (rnrs)
	    (sagittarius)
	    (maquette connection)
	    (text sql)
	    (srfi :13)
	    (dbi))

(define max-connection 10);; for now, we need to get this from configuration
    
(define (dialect-procedures)
  (values generator duplicate-insert))
;; PostgreSQL uses sequence to generate id
;; the name must be `table_name`_seq
(define (generator conn table)
  (define dbi-conn (maquette-connection-dbi-connection conn))
  (let ((seq (format "~a_seq" table)))
    (let ((q (dbi-execute-query-using-connection! dbi-conn
		(format "select nextval('~a')" seq))))
      (let ((r (vector-ref (dbi-fetch! q) 0)))
	(dbi-close q)
	r))))

;; duplicate-insert
;; if the insert statement violates unique constraint, then ignores the
;; insertion.
;; on PostgreSQL using conflict on ignore
(define (duplicate-insert table uniques columns)
  (string-append
   (ssql->sql
    `(insert-into ,table ,columns (values ,(map (lambda (c) '?) columns))))
   " ON CONFLICT (" (string-join (map symbol->string uniques)) ") DO NOTHING"))

)

