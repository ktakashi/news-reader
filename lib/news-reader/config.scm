;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; lib/news-reader/config.scm - Configuration library
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

(library (news-reader config)
  (export max-process-feed-threads
	  max-sql-connectios
	  max-feeds)
  (import (rnrs)
	  (sagittarius)
	  (text json)
	  (srfi :39))

  (define *config-file*
    (find-file-on-paths "news-reader/config.json" (load-path)))

  (define *config-json*
    (if (file-exists? *config-file*)
	(parameterize ((*json-map-type* 'alist))
	  (call-with-input-file *config-file* json-read))
	'()))
  
  (define (get config key env env-conv default)
    (cond ((and-let* ((e (getenv env))) (env-conv e))) ;; env first
	  ((assoc key config) => cdr) ;; then config
	  (else default)))
  (define (get-number config key env default)
    (get config key env string->number default))

  (define max-process-feed-threads
    (get-number *config-json* "max_process_feed_threads"
		"NEWS_READER_MAX_PROCESS_FEED_THREADS" 10))
  (define max-sql-connectios
    (get-number *config-json* "max_sql_connections"
		"NEWS_READER_MAX_SQL_CONNECTIONS" 10))
  (define max-feeds
    (get-number *config-json* "max_feeds" "NEWS_READER_MAX_FEEDS" 50))

  
  )
