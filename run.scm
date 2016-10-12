;; -*- mode:scheme; coding:utf-8 -*-
(import
  (rnrs)
  (paella)
  (plato)
  (plato tools)
  (getopt)
  (clos user)
  (util concurrent)
  (sagittarius)
  (sagittarius io)
  (sagittarius process)
  (sagittarius socket)
  (sagittarius threads)
  (sagittarius remote-repl)
  (srfi :39 parameters))


(define *plato-roots* (make-parameter '()))
(define-constant +default-plato-root+ "/home/takashi/projects/news-reader")
(define-constant +stop-commands+ '("stop" "restart"))

;; paella dispatcher
(define http-dispatcher (plato-make-dispatcher +default-plato-root+))

(define (make-shutdown-handler shared-queue)
  (lambda (server socket)
    (let ((command (utf8->string (socket-recv socket 255))))
      (and (member command +stop-commands+)
           (shared-queue-put! shared-queue command)))))

;; create server config
(define (make-server-config shutdown-port max-thread)
  (let* ((p (string->number shutdown-port))
         (shared-queue (make-shared-queue))
         (handler (make-shutdown-handler shared-queue)))
    (values
      (make-http-server-config
        :max-thread
        max-thread
        :shutdown-port
        (and p shutdown-port)
        :shutdown-handler
        handler)
      shared-queue)))

;; run the server
(define (run-server port config remote-port shared-queue)
  (define server-thread
    (make-thread
      (lambda ()
        (plato-run port config http-dispatcher))))
  (define (wait! socket)
    (thread-join! server-thread)
    (when socket
          (socket-shutdown socket SHUT_RDWR)
          (socket-close socket))
    (unless
      (shared-queue-empty? shared-queue)
      (let ((command (shared-queue-get! shared-queue)))
        (when (equal? command "restart")
              (reload-all-webapp)
              (run-server port config remote-port shared-queue)))))
  (thread-start! server-thread)
  (if remote-port
    (let-values
      (((repl socket) (make-remote-repl remote-port)))
      (thread-start! (make-thread repl))
      (wait! socket))
    (wait! #f)))

(define (send-command config command)
  (let ((socket
          (make-client-socket
            "localhost"
            (slot-ref config 'shutdown-port))))
    (socket-send socket (string->utf8 command))))

;; Re-load all roots
(define (reload-all-webapp)
  (define (reload-webapps root)
    (for-each
      (lambda (name)
        (plato-reload name root http-dispatcher))
      (plato-collect-handler root)))
  (reload-webapps +default-plato-root+)
  (for-each reload-webapps (*plato-roots*)))
;; For interactive development
(define (reload-webapp name)
  (plato-reload
    name
    +default-plato-root+
    http-dispatcher))

(define (usage script)
  (format #t "~a [OPTIONS]~%" script)
  (format #t " OPTIONS~%")
  (format
    #t
    "  -p<num>,--port <num>	specify HTTP port (default 8080)~%")
  (format
    #t
    "  -r<num>,--remote-port <num>	specify remote repl port~%")
  (format
    #t
    "  -s<num>,--shutdown-port <num>	specify shutdown port (default 8081)~%")
  (format
    #t
    "  -c<command>,--command <command>	run the command~%")
  (format #t "    `run`     starts the server~%")
  (format
    #t
    "    `restart` re-starts the server~%")
  (format #t "    `stop`    stops the server~%")
  (format
    #t
    "  -m<num>,--max-thread <num>	specify the number of server thread~%")
  (format #t "  -h,--help	show this message~%"))


;; entry point
(define (main args)
  (with-args
    (cdr args)
    ((port (#\p "port") #t "8080")
     (remote-port (#\r "remote-port") #t #f)
     (shutdown-port (#\s "shutdown-port") #t "8081")
     (command (#\c "command") #t "run")
     (max-thread (#\t "max-thread") #t "10")
     (help (#\h "help") #f #f))
    (when help (usage (car args)) (exit 0))
    (let-values
      (((config shared-queue)
        (make-server-config
          shutdown-port
          (string->number max-thread))))
      (case (string->symbol command)
        ((run)
         (run-server port config remote-port shared-queue))
        (else (send-command config command))))))

