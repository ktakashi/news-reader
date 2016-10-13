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
	  (cuberteria)
	  (dbi)
	  (clos user)
	  (text json)
	  (sagittarius regex)
	  (sagittarius object)
	  (sagittarius)
	  (news-reader commands)
	  (news-reader constants)
	  (srfi :19))

(define-syntax with-path-variable
  (syntax-rules ()
    ((_ "gen" regexp (b ...) ((v d n) ...))
     (let ((m regexp))
       (let ((v (if m (m n) d)) ...)
	 b ...)))
    ((_ "count" regexp () (b ...) (t ...) n)
     (with-path-variable "gen" regexp (b ...) (t ...)))
    
    ((_ "count" regexp ((v d) v* ...) (b ...) (t ...) n)
     (with-path-variable "count" regexp (v* ...) (b ...)
			 ((v d (+ n 1)) t ...) (+ n 1)))
    
    ((_ regexp (variables ...) body ...)
     (with-path-variable "count" regexp (variables ...) (body ...) () 0))))
  
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

(define (utf8->integer u8)
  (let ((v (utf8->string u8)))
    (or (string->number v)
	(error 'limit&offset "invalid value" v))))
(define-class <limit&offset> (<converter-mixin>)
  ((limit :init-value 50 :converter utf8->integer)
   (offset :init-value 0 :converter utf8->integer)))

(define retrieve-summary
  (cuberteria-object-mapping-handler <limit&offset>
    (lambda (limit&offet req)
      (define (->array summary)
	`#((link . ,(feed-summary-link summary))
	   (title . ,(feed-summary-title summary))
	   (summary . ,(utf8->string
			(get-bytevector-all (feed-summary-summary summary))))
	   (created . ,(string-append
			(date->string
			 (time-utc->date (feed-summary-created-date summary))
			 "~5")
			"Z"))))
      (with-path-variable (#/summary\/(.+)/ (http-request-path req))
	  ((provider #f))
	(if provider
	    (let ((summaries (summary-retriever provider
						(~ limit&offet 'limit)
						(~ limit&offet 'offset))))
	      (values 200 'application/json
		      (json->string (map ->array summaries))))
	    (values 404 'text/plain "Not found"))))))
	  
(define (mount-paths)
  `(
    ((GET) "/providers" ,retrieve-providers)
    ((GET) #/summary\/.+/ ,retrieve-summary)
    ))

(define (support-methods) '(GET))

(define (static-handler file)
  (lambda (req)
    (define root-path (plato-current-path (*plato-root-context*)))
    (call-with-input-file (build-path root-path file)
      html->tapas-component)))

(define entry-point (tapas-request-handler (static-handler "main.html")))

)
