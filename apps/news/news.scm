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
	  (rfc uri)
	  (news-reader commands)
	  (news-reader database)
	  (news-reader config)
	  (util hashtables)
	  (srfi :19)
	  (srfi :26))

(define style-loader (cuberteria-resource-loader 'text/css "./css"))
(define template-loader (cuberteria-resource-loader 'text/html "./templates"))
(define image-loader (cuberteria-resource-loader 'text/html "./images"))
(define js-loader (cuberteria-resource-loader 'text/javascript "./scripts"))
  
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

(define (json->string obj)
  (let-values (((out extract) (open-string-output-port)))
    (json-write obj out)
    (extract)))

(define (retrieve-providers req)
  (define (->array provider)
    `#((name . ,(provider-name provider))
       (url . ,(provider-url provider))
       (languages . ,(provider-languages provider))
       (feeds . ,(provider-feeds provider))))
  (let ((names (map ->array (news-reader-retrieve-provider))))
    (values 200 'application/json (json->string names))))

(define (utf8->integer u8)
  ;; in case of JSON
  (if (number? u8)
      u8
      (let ((v (utf8->string u8)))
	(or (string->number v) 0))))
(define-class <limit> (<converter-mixin>)
  ((limit :init-value max-feeds :converter utf8->integer)))
(define-class <limit&offset> (<limit>)
  ((offset :init-value 0 :converter utf8->integer)))

(define-class <criteria> (<limit&offset>)
  ((providers :init-value '#())
   (feeds     :init-value #f)
   (from      :init-value #f)
   (to        :init-value #f)))

(define (summary->array summary)
  `#((link . ,(feed-summary-link summary))
     (feed_name . ,(feed-summary-feed-name summary))
     (feed_url . ,(feed-summary-feed-url summary))
     (language . ,(feed-summary-language summary))
     (title . ,(feed-summary-title summary))
     (summary . ,(feed-summary-summary summary))
     (created . ,(string-append
		  (date->string
		   (time-utc->date (feed-summary-created-date summary))
		   "~5")
		  "Z"))))

(define (->criteria criteria)
  (define (->criterion criteria name conv)
    (cond ((~ criteria name) =>
	   (lambda (v)
	     (let ((r (conv v)))
	       (if r
		   `((,name . ,r))
		   '()))))
	  (else '())))
  (define (second->time-utc v)
    (let ((n (if (number? v) v (string->number v))))
      (and n (list (make-time time-utc 0 n)))))
  `(,@(->criterion criteria 'feeds vector->list)
    ,@(->criterion criteria 'from second->time-utc)
    ,@(->criterion criteria 'to second->time-utc)))

(define retrieve-summary
  (cuberteria-object-mapping-handler <criteria>
    (lambda (limit&offet req)
      (define (query retriever param)
	)
      (with-path-variable (#/summary\/(.+)/ (http-request-path req))
	((provider #f))
	
	(cond (provider
	       (let ((summaries (news-reader-retrieve-summary
				 (uri-decode-string provider :cgi-decode #t)
				 (->criteria limit&offet)
				 (~ limit&offet 'limit)
				 (~ limit&offet 'offset))))
		 (values 200 'application/json
			 (json->string (map summary->array summaries)))))
	      (else (values 404 'text/plain "Not found")))))
    :json? #t))

(define retrieve-summaries
  (cuberteria-object-mapping-handler <criteria>
    (lambda (criteria req)
      (define (pub-date<=? a b)
	(time>=? (feed-summary-created-date a) (feed-summary-created-date b)))
      (define (->summary-map provider summaries)
	`#(("provider" . ,provider)
	   ("feeds" ,@(map summary->array (list-sort pub-date<=? summaries)))))
      (define (name-ref e) (cdr (vector-ref e 0)))
      (define (name<=? a b) (string<=? (name-ref a) (name-ref b)))
      (if (null? (~ criteria 'providers))
	  (values 200 'application/json "{}")
	  (let ((summaries (news-reader-retrieve-summaries
			    (vector->list (~ criteria 'providers))
			    (->criteria criteria)
			    (~ criteria 'limit)
			    (~ criteria 'offset))))
	    (values 200 'application/json
		    (if summaries
			(json->string
			 (list-sort name<=?
			    (hashtable-map ->summary-map summaries)))
			"[]")))))
    :json? #t))

(define (mount-paths)
  `(
    ((GET) "/providers"   ,retrieve-providers)
    ((POST GET) #/summary\/.+/ ,retrieve-summary)
    ((POST) "/summary"    ,retrieve-summaries)
    ((GET) #/styles/      ,style-loader)
    ((GET) #/html/        ,template-loader)
    ((GET) #/img/         ,image-loader)
    ((GET) #/js/         ,js-loader)
    ))

(define (support-methods) '(GET))

(define (static-handler file)
  (lambda (req)
    (define root-path (plato-current-path (*plato-root-context*)))
    (call-with-input-file (build-path root-path file)
      html->tapas-component)))

(define entry-point (tapas-request-handler (static-handler "main.html")))

)
