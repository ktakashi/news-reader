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
  (export generate-add-provider
	  generate-add-feed
	  generate-process-feed
	  *command-logger*)
  (import (rnrs)
	  (rnrs eval)
	  (news-reader constants)
	  (util concurrent)
	  (text sql)
	  (rfc http)
	  (srfi :13)
	  (srfi :39)
	  (util logging)
	  (sagittarius)
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
    ((_ logger msg ...) (write-log info logger msg ...))))
(define-syntax write-info-log
  (syntax-rules ()
    ((_ logger msg ...) (write-log info logger msg ...))))
(define-syntax write-error-log
  (syntax-rules ()
    ((_ logger msg ...) (write-log error logger msg ...))))

(define generator
  (let-values (((driver op alist) (dbi-parse-dsn +dsn+)))
    (eval 'generator (environment `(news-reader ,(string->symbol driver))))))

(define (generate-add-provider dbi)
  (let ((stmt (dbi-prepare dbi "insert into provider (id, name) values (?, ?)"))
	(id-generator (lambda () (generator dbi 'provider))))
    (lambda (name)
      (define id (id-generator))
      (write-info-log (*command-logger*) "Adding provider ~a" provider)
      (dbi-execute! stmt id name))))

(define (generate-add-feed dbi)
  (define sql
    (ssql->sql
     '(insert-into feed (id provider_id feed_type_id url)
		   (values (?
			    (select (id) (from provider) (where (= name ?)))
			    (select (id) (from feed_type) (where (= name ?)))
			    ?)))))
  (let ((stmt (dbi-prepare dbi sql))
	(id-generator (lambda () (generator dbi 'feed))))
    (lambda (provider url type)
      (define id (id-generator))
      (write-info-log (*command-logger*)
		      "Adding feed for provider ~a (~a)" provider url)
      (dbi-execute! stmt id provider type url))))

(define (generate-process-feed dbi)
  (define count-sql "select count(*) from feed")
  (define select-sql
    (ssql->sql
     '(select ((~ f url) (~ t plugin))
	      (from ((as feed f)
		     (inner-join (as feed_type t)
				 (on (= (~ f feed_type_id) (~ t id))))
		     (inner-join (as provider p)
				 (on (= (~ f provider_id) (~ p id))))))
	      (where (= (~ p name) ?)))))
;; SQLite3 doesn't support (VALUES ...) AS T(...) syntax...
;;   (define insert-sql
;;     (ssql->sql
;;      '(insert-into feed_summary (id guid title summary pubDate)
;; 	(select ((~ v i) (~ v g) (~ v t) (~ v s) (~ v p))
;; 	  (from (as (values (?) (?) (?) (?) (?)) (v i g t s p)))
;; 	  (where (not-exists (select (id)
;; 				(from (as feed_summary f))
;; 				(where (= (~ f guid) (~ v g))))))))))
  (define insert-sql
    (ssql->sql
     '(insert-into feed_summary (id guid title summary pubDate)
	(with ((as v (select ((as ? i) (as ? g) (as ? t) (as ? s) (as ? p)))))
	  (select ((~ v i) (~ v g) (~ v t) (~ v s) (~ v p))
	    (from v)
	    (where (not-exists (select (id)
				 (from (as feed_summary f))
				 (where (= (~ f guid) (~ v g)))))))))))
		  
  (define count-stmt (dbi-prepare dbi count-sql))
  (define select-stmt (dbi-prepare dbi select-sql))
  (define insert-stmt (dbi-prepare dbi insert-sql))
  (define (get-count)
    (let ((q (dbi-execute-query! count-stmt)))
      (vector-ref (dbi-fetch! q) 0)))
  (define max-thread-count 100)
  (define id-generator (lambda () (generator dbi 'feed_summary)))
  (define (error-handler e)
    (write-error-log (*command-logger*) (describe-condition e)))
  (lambda (provider)
    (define thread-pool (make-thread-pool (min (get-count) max-thread-count)
					  error-handler))
    (define (task url plugin)
      (lambda ()
	(define process-feed
	  (eval 'process-feed
		(environment (read (open-string-input-port plugin)))))
	(write-debug-log (*command-logger*) "Retrieving feed from ~a" url)
	(let*-values (((server path) (url-server&path url))
		      ((s h b) (http-get server path
				 :secure? (string-prefix? "https" url))))
	  (for-each (lambda (item)
		      (let ((id (id-generator)))
			(apply dbi-execute! insert-stmt id item)))
		    (process-feed b)))))
    (dbi-do-fetch! (v (dbi-execute-query! select-stmt provider))
      (thread-pool-push-task! thread-pool
        (task (vector-ref v 0) (vector-ref v 1))))
    (thread-pool-wait-all! thread-pool)))

)
