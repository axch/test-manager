;;;; Test Registration

(define (register-test test)
  (tg:register-test! *current-test-group* test))

;; TODO Unit test this, and port it to Guile
(define (generate-test-name)
  (generate-uninterned-symbol 'anonymous-test))

;; TODO Teach Emacs to syntax-highlight this just like define
(define-syntax define-test
  (syntax-rules ()
    ((define-test (name formal ...) body-exp1 body-exp2 ...)
     (let ((proc (lambda (formal ...) body-exp1 body-exp2 ...)))
       (register-test (make-single-test 'name proc))))
    ((define-test () body-exp1 body-exp2 ...)
     (let ((proc (lambda () body-exp1 body-exp2 ...)))
       (register-test (make-single-test (generate-test-name) proc))))
    ((define-test form)
     (define-test () form))))

;;;; Test Running

;; Poor man's dynamic dispatch by storing the
;; procedures that do the job in a record
(define (run-given-test test-runner test)
  ((tr:run-one test-runner) (list (st:name test)) test))

(define (run-given-group test-runner group name-stack)
  ((tr:run-group test-runner) group name-stack))

(define (run-given-test-or-group test-runner test name-stack)
  (cond ((test-group? test)
	 (run-given-group test-runner test name-stack))
	((single-test? test)
	 (run-given-test test-runner test))
	(else
	 (error "Unknown test type" test))))

(define (report-results test-runner)
  ((tr:report-results test-runner)))

(define (run-test test-name-stack . opt-test-runner)
  (let-optional opt-test-runner ((test-runner (make-standard-test-runner)))
   (let loop ((test (current-test-group))
	      (stack-left test-name-stack)
	      (stack-traversed '()))
     (cond ((null? stack-left)
	    (run-given-test-or-group test-runner test (reverse stack-traversed)))
	   ((test-group? test)
	    (tg:in-group-context test
	     (lambda ()
	       (tg:in-test-context test
		(lambda ()
		  (loop (tg:get test (car stack-left))
			(cdr stack-left)
			(cons (car stack-left) stack-traversed)))))))
	   (else
	    (error "Name stack did not lead to a valid test" test-name-stack))))
   (report-results test-runner)))

(define (run-registered-tests . opt-test-runner)
  (apply run-test (cons '() opt-test-runner)))
