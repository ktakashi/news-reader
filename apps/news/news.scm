;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; app/news/news.scm - Viewer for RSS feed
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

#!read-macro=sagittarius/regex
(library (plato webapp news)
  (export entry-point support-methods mount-paths)
  (import (rnrs)
	  (paella)
	  (tapas)
	  (plato)
	  (dbi)
	  (text json)
	  (sagittarius regex)
	  (sagittarius)
	  (news-reader commands)
	  (news-reader constants))

(define dbi-connection (dbi-connect +dsn+))

(define provider-retriever (generate-retrieve-provider dbi-connection))
(define summary-retriever (generate-retrieve-summary dbi-connection))

(define (json->string obj)
  (let-values (((out extract) (open-string-output-port)))
    (json-write obj out)
    (extract)))

(define (retrieve-providers req)
  (let ((names (map provider-name (provider-retriever))))
    (values 200 'application/json (json->string names))))
    
(define (mount-paths)
  `(
    ((GET) "/providers" ,retrieve-providers)
    ))

(define (support-methods) '(GET))

(define (static-handler file)
  (lambda (req)
    (define root-path (plato-current-path (*plato-root-context*)))
    (call-with-input-file (build-path root-path file)
      html->tapas-component)))

(define entry-point (tapas-request-handler (static-handler "main.html")))

)