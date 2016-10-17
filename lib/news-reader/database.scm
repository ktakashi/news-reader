;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; lib/news-reader/database.scm - DBI connection
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

;; wrapper library of maquette connection
(library (news-reader database)
    (export make-dbi-connection
	    repeal-dbi-connection
	    dbi-connection-commit!
	    dbi-prepared-statement)
    (import (rnrs)
	    (dbi)
	    (news-reader constants)
	    (maquette connection))

;; TODO make this configurable
(define *connection-pool*
  (make-maquette-connection-pool 10 +dsn+ :auto-commit? #f 
				 :username "news_reader"
				 :password "news_reader"))
    
(define (make-dbi-connection)
  (maquette-connection-pool-get-connection *connection-pool*))
(define (repeal-dbi-connection conn)
  (maquette-connection-pool-return-connection *connection-pool* conn))
(define (dbi-connection-commit! conn)
  (maquette-connection-commit! conn))
(define (dbi-prepared-statement conn sql)
  (maquette-connection-prepared-statement conn sql))


)
