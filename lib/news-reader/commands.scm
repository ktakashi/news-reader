;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; lib/news-reader/commands.scm - Commands for news reader application
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

(library (news-reader commands)
  (export news-reader-add-provider
	  news-reader-add-feed
	  news-reader-process-feed
	  news-reader-retrieve-provider
	  news-reader-retrieve-summary
	  ;; Should we move this somewhere?
	  provider-name
	  provider-url
	  provider-languages
	  feed-summary-name
	  feed-summary-link
	  feed-summary-feed-name
	  feed-summary-language
	  feed-summary-title
	  feed-summary-summary
	  feed-summary-created-date
	  
	  *command-logger*)
  (import (rnrs)
	  (rnrs eval)
	  (news-reader constants)
	  (news-reader database)
	  (util concurrent)
	  (text sql)
	  (rfc http)
	  (srfi :13)
	  (srfi :39)
	  (util logging)
	  (sagittarius)
	  (sagittarius control)
	  (dbi))

;; default do nothing
(define *command-logger* (make-parameter (make-logger +error-level+)))

(define-syntax write-log
  (lambda (x)
    (define (->check level)
      (string->symbol (format "logger-~a?" (syntax->datum level))))
    (define (->logging level)
      (string->symbol (format "~a-log" (syntax->datum level))))
    (syntax-case x ()
      ((k level logger msg)
       (with-syntax ((check (datum->syntax #'k (->check #'level)))
		     (logging (datum->syntax #'k (->logging #'level))))
	 #'(let ((l logger)) (when (check l) (logging l msg)))))
      ((k level logger fmt args ...)
       #'(k level logger (format fmt args ...))))))
(define-syntax write-debug-log
  (syntax-rules ()
    ((_ logger msg ...) (write-log debug logger msg ...))))
(define-syntax write-info-log
  (syntax-rules ()
    ((_ logger msg ...) (write-log info logger msg ...))))
(define-syntax write-error-log
  (syntax-rules ()
    ((_ logger msg ...) (write-log error logger msg ...))))

(define-values (generator duplicate-insert)
  (let-values (((driver op alist) (dbi-parse-dsn +dsn+)))
    (eval '(dialect-procedures)
	  (environment `(news-reader ,(string->symbol driver))))))

(define news-reader-add-provider
  (let ()
    (define insert-sql
      (duplicate-insert 'provider '(name) '(id name url)))
    (lambda (name url)
      (call-with-dbi-connection
       (lambda (dbi)
	 (let ((stmt (dbi-prepared-statement dbi insert-sql))
	       (id (generator dbi 'provider)))
	   (write-info-log (*command-logger*) "Adding provider ~a" name)
	   (dbi-execute! stmt id name url)
	   (dbi-commit! stmt)))))))

(define news-reader-add-feed
  (let ()
    (define sql
      (duplicate-insert 'feed '(url)
			'(id provider_id feed_type_id language_id title url)))
    (define feed-type-sql "select id, plugin from feed_type where name = ?")
    (define provider-id-sql "select id from provider where name = ?")
    (define (feed-type-info conn name)
      (define stmt (dbi-prepared-statement conn feed-type-sql))
      (let ((q (dbi-execute-query! stmt name)))
	(cond ((dbi-fetch! q) =>
	       (lambda (v) (values (vector-ref v 0) (vector-ref v 1))))
	      (else (error 'add-feed "unknown feed type" name)))))
    (define (get-provider-id conn name)
      (define stmt (dbi-prepared-statement conn provider-id-sql))
      (let ((q (dbi-execute-query! stmt name)))
	(cond ((dbi-fetch! q) => (lambda (v) (vector-ref v 0)))
	      (else              (error 'add-feed "unknown provider" name)))))
    (define (get-language-id conn lang)
      (define sql "select id from languages where code2 = ?")
      (or (and-let* (( lang )
		     (q (dbi-execute-query!
			 (dbi-prepared-statement conn sql) lang))
		     (v (dbi-fetch! q)))
	    (vector-ref v 0))
	  ;; unknown
	  0))
    
    (define (retrieve-feed-title xml plugin)
      (define feed-title
	(eval 'feed-info (environment (read (open-string-input-port plugin)))))
      (feed-title xml))
    
    (lambda (provider url type)
      (let*-values (((server path) (url-server&path url))
		    ((status header body)
		     (http-get server path
			       :secure? (string-prefix? "https" url))))
	(unless (string=? status "200") (error 'add-feed "invalid feed" url))
	(call-with-dbi-connection
	 (lambda (dbi)
	   (define-values (feed-type-id plugin) (feed-type-info dbi type))
	   (define provider-id (get-provider-id dbi provider))
	   (define-values (title lang) (retrieve-feed-title body plugin))
	   (define stmt (dbi-prepared-statement dbi sql))
	   (define id (generator dbi 'feed))
	   (define lang-id (get-language-id dbi lang))
	   (write-info-log (*command-logger*)
			   "Adding feed for provider ~a (~a)" provider url)
	   (write-debug-log (*command-logger*)
			    "SQL ~a (~a ~s ~s ~a ~s ~a)"
			    sql id provider type lang-id title url)
	   (dbi-execute! stmt id provider-id feed-type-id lang-id title url)
	   (dbi-commit! stmt)))))))

(define news-reader-process-feed
  (let ()
    (define count-sql "select count(*) from feed")    
    (define insert-sql
      (duplicate-insert 'feed_summary
			'(guid)
			'(id feed_id guid title summary pubDate)))
    (define max-thread-count 100)
    (define (error-handler e)
      (write-error-log (*command-logger*)
       (call-with-string-output-port (lambda (out) (report-error e out)))))
    (define feed-info-select-sql
      (ssql->sql
       '(select ((~ f id) (~ f url) (~ t plugin) (~ t id))
		(from ((as feed f)
		       (inner-join (as feed_type t)
				   (on (= (~ f feed_type_id) (~ t id))))
		       (inner-join (as provider p)
				   (on (= (~ f provider_id) (~ p id))))))
		(where (= (~ p name) ?)))))
    (lambda (provider)
      (define (get-count)
	(call-with-dbi-connection
	 (lambda (dbi)
	   (define count-stmt (dbi-prepared-statement dbi count-sql))
	   (let ((q (dbi-execute-query! count-stmt)))
	  (vector-ref (dbi-fetch! q) 0)))))
      (define thread-pool (make-thread-pool (min (get-count) max-thread-count)
					    error-handler))
      (call-with-dbi-connection
       (lambda (dbi)
	 (define select-stmt (dbi-prepared-statement dbi feed-info-select-sql))
	 (define (task feed-id url plugin)
	   (lambda ()
	     (write-debug-log (*command-logger*) "Task for ~a" url)
	     (call-with-dbi-connection
	      (lambda (conn)
		(define stmt (dbi-prepared-statement conn  insert-sql))
		(define id-generator (lambda () (generator conn 'feed_summary)))
		(define process-feed
		  (eval 'process-feed
			(environment (read (open-string-input-port plugin)))))
		(write-debug-log (*command-logger*)
				 "Retrieving feed from ~a" url)
		(let*-values (((server path) (url-server&path url))
			      ((s h b) (http-get server path
					:secure? (string-prefix? "https" url))))
		  (for-each (lambda (item)
			      (let ((id (id-generator)))
				(write-debug-log (*command-logger*)
				  "SQL ~a ~s"
				  insert-sql (cons* id feed-id item))
				(apply dbi-execute! stmt id feed-id item)))
			    (process-feed b)))
		(dbi-connection-commit! conn)))))
	 (dbi-do-fetch! (v (dbi-execute-query! select-stmt provider))
	   (thread-pool-push-task! thread-pool
	     (task (vector-ref v 0) (vector-ref v 1) (vector-ref v 2))))))
      (thread-pool-wait-all! thread-pool))))

(define-record-type provider (fields name url languages))
(define news-reader-retrieve-provider
  (let ()
    (define language-sql
      (ssql->sql
       '(select-distinct ((~ l code2))
	 (from ((as languages l)
		(inner-join (as feed f) (on (= (~ f language_id) (~ l id))))
		(inner-join (as provider p)
			    (on (= (~ f provider_id) (~ p id))))))
	 (where (= (~ p id) ?)))))
    (lambda ()
      (call-with-dbi-connection
       (lambda (dbi)
	 (define select-sql "select id, name, url from provider order by name")
	 (define select-stmt (dbi-prepared-statement dbi select-sql))
	 (let ((id&name&urls (dbi-query-map (dbi-execute-query! select-stmt)
					    vector->list))
	       (language-stmt (dbi-prepared-statement dbi language-sql)))
	   (map (lambda (id&name&url)
		  (let ((id (car id&name&url))
			(name (cadr id&name&url))
			(url (caddr id&name&url)))
		    (make-provider name url
		      (dbi-query-map (dbi-execute-query! language-stmt id)
				     (lambda (q) (vector-ref q 0))))))
		id&name&urls)))))))
    
(define-record-type feed-summary
  (fields name feed-name language link title summary created-date))

;; TODO should be configurable.
(define-constant +max-fetch-count+ 50)
(define (non-negative-fixnum? n) (and (fixnum? n) (not (negative? n))))
(define news-reader-retrieve-summary
  (let ()
    (define select-sql
      (ssql->sql
       '(select ((~ p name) (~ f title) (~ l code2)
		 (~ s guid) (~ s title) (~ s summary) (~ s pubDate))
		(from ((as feed_summary s)
		       (inner-join (as feed f) (on (= (~ f id) (~ s feed_id))))
		       (inner-join (as languages l)
				   (on (= (~ l id) (~ f language_id))))
		       (inner-join (as provider p)
				   (on (= (~ p id) (~ f provider_id))))))
		(where (= (~ p name) ?))
		(order-by ((~ s pubDate) desc))
		(limit ?)
		(offset ?))))
    (lambda (provider limit offset)
      (if (and (non-negative-fixnum? limit) (non-negative-fixnum? offset))
	  (call-with-dbi-connection
	   (lambda (dbi)
	     (define select-stmt (dbi-prepared-statement dbi select-sql))
	     (dbi-query-map (dbi-execute-query! select-stmt provider
						(min limit +max-fetch-count+)
						offset)
		(lambda (query)
		  (apply make-feed-summary (vector->list query))))))
	  '()))))
)
