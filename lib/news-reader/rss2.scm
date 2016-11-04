;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; lib/news-reader/rss2.scm - RSS 2.0 processor
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

(library (news-reader rss2)
  (export process-feed
	  feed-info)
  (import (rnrs)
	  (net rss)
	  (text sxml ssax)
	  (srfi :1)
	  (srfi :13)
	  (srfi :19))

(define (xml->rss xml)
  (define sxml (ssax:xml->sxml (open-string-input-port xml) '()))
  (sxml->rss-object sxml))
(define (get-content r) (or (and (not r) "") (rss-simple-content r)))

(define (process-feed xml)
  (define rss (xml->rss xml))
  (define channel (rss-rss-channel rss))
  (define item (rss-channel-item channel))
  (define (get-date r)
    (or (and (not r) (current-date)) (rss-simple-content r)))
  (define (get-url item)
    (let ((guid (rss-item-guid item)))
      (cond ((and guid (rss-guid-permalink? guid)) (rss-simple-content guid))
	    ((rss-item-link item) => rss-simple-content)
	    (else #f))))
  
  (filter-map (lambda (item)
		(let ((title (get-content (rss-item-title item)))
		      (desc (get-content (rss-item-description item)))
		      (url (get-url item)))
		  (and (not (or (string-null? title) (string-null? desc)))
		       url
		       (list url
			     title desc
			     (get-date (rss-item-pub-date item)))))) item))

(define (feed-info xml)
  (define rss (xml->rss xml))
  (define channel (rss-rss-channel rss))
  (define (->iso-code2 code)
    (and (>= (string-length code) 2)
	 (substring code 0 2)))
  (values (rss-simple-content (rss-channel-title channel))
	  (->iso-code2 (get-content (rss-channel-language channel)))))
  
  )
