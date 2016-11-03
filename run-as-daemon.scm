(import (rnrs)
	(clos user)
	(getopt))

(define pid-file "news-reader.pid")
(define (main args)
  (when (file-exists? pid-file)
    (print "PID file exists")
    (exit -1))
  (with-args (cdr args)
      ((sash (#\s "sagittarius") #t "sagittarius")
       (port (#\p "port") #t "8080")
       (remote-port (#\r "remote-port") #t "")
       (shutdown-port (#\s "shutdown-port") #t "8081")
       (max-thread (#\t "max-thread") #t "10"))
    
    (let-values (((pid in out err)
		  (sys-process-call sash `("run.scm"
					   "-p" ,port
					   "-r" ,remote-port
					   "-s" ,shutdown-port
					   "-t" ,max-thread)
				    :output :stdout
				    :error :stderr
				    :directory (current-directory)
				    :detach? #t)))
      (call-with-output-file pid-file
	(lambda (out) (put-string out (number->string pid)))))))
