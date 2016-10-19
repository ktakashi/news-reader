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
	  feed-summary-name
	  feed-summary-link
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

(define (news-reader-add-provider name url)
  (call-with-dbi-connection
   (lambda (dbi)
     (let ((stmt (dbi-prepared-statement dbi 
		  "insert into provider (id, name, url) values (?, ?, ?)"))
	   (id (generator dbi 'provider)))
       (write-info-log (*command-logger*) "Adding provider ~a" name)
       (dbi-execute! stmt id name url)
       (dbi-commit! stmt)))))

(define news-reader-add-feed
  (let ()
    (define sql
      (ssql->sql
       '(insert-into feed (id provider_id feed_type_id url)
		     (values (?
			      (select (id) (from provider) (where (= name ?)))
			      (select (id) (from feed_type) (where (= name ?)))
			      ?)))))
    (lambda (provider url type)
      (call-with-dbi-connection
       (lambda (dbi)
	 (define stmt (dbi-prepared-statement dbi sql))
	 (define id (generator dbi 'feed))
	 (write-info-log (*command-logger*)
			 "Adding feed for provider ~a (~a)" provider url)
	 (write-debug-log (*command-logger*)
			  "SQL ~a (~a ~a ~a ~a)" sql id provider url type)
	 (dbi-execute! stmt id provider type url)
	 (dbi-commit! stmt))))))

(define news-reader-process-feed
  (let ()
    (define count-sql "select count(*) from feed")
    (define select-sql
      (ssql->sql
       '(select ((~ f id) (~ f url) (~ t plugin))
		(from ((as feed f)
		       (inner-join (as feed_type t)
				   (on (= (~ f feed_type_id) (~ t id))))
		       (inner-join (as provider p)
				   (on (= (~ f provider_id) (~ p id))))))
		(where (= (~ p name) ?)))))
    
    (define insert-sql
      (duplicate-insert 'feed_summary
			'(guid)
			'(id feed_id guid title summary pubDate)))
    (define max-thread-count 100)
    (define (error-handler e)
      (write-error-log (*command-logger*)
       (call-with-string-output-port (lambda (out) (report-error e out)))))

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
	 (define select-stmt (dbi-prepared-statement dbi select-sql))
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

(define-record-type provider (fields name))
(define (news-reader-retrieve-provider)
  (call-with-dbi-connection
   (lambda (dbi)
     (define select-sql "select name from provider")
     (define select-stmt (dbi-prepared-statement dbi select-sql))
     (dbi-query-map (dbi-execute-query! select-stmt)
	(lambda (query) (make-provider (vector-ref query 0)))))))
    
(define-record-type feed-summary
  (fields name link title summary created-date))
(define news-reader-retrieve-summary
  (let ()
    (define select-sql
      (ssql->sql
       '(select ((~ p name) (~ s guid) (~ s title) (~ s summary) (~ s pubDate))
		(from ((as feed_summary s)
		       (inner-join (as feed f) (on (= (~ f id) (~ s feed_id))))
		       (inner-join (as provider p)
				   (on (= (~ p id) (~ f provider_id))))))
		(where (= (~ p name) ?))
		(order-by ((~ s pubDate) desc))
		(limit ?)
		(offset ?))))
    (lambda (provider limit offset)
      (call-with-dbi-connection
       (lambda (dbi)
	 (define select-stmt (dbi-prepared-statement dbi select-sql))
	 (dbi-query-map (dbi-execute-query! select-stmt provider limit offset)
	   (lambda (query)
	     (apply make-feed-summary (vector->list query)))))))))
)
