(define (instantiate-template template arguments)
  (if (not (= (length arguments) (- (length template) 1)))
      (error "Template and argument lists are length-mismatched: "
	     template arguments))
  (let loop ((result (car template))
	     (template (cdr template))
	     (arguments arguments))
    (if (null? template)
	result
	(loop (string-append result (car arguments) (car template))
	      (cdr template)
	      (cdr arguments)))))

(define (messagify object)
  (with-output-to-string (lambda () (display object))))

(define (build-message header template . arguments)
  (let ((body (instantiate-template template (map messagify arguments))))
    (if header
	(string-append header "\n" body)
	(string-append "\n" body))))

(define (assert-proc message proc)
  (if (proc)
      'ok
      (test-fail message)))

(define (assert-equivalent predicate . opt-pred-name)
  (define (full-message message expected actual)
    (if (null? opt-pred-name)
	(build-message message
		       '("<" "> expected but was\n<" ">.")
		       expected actual)
	(build-message message
		       '("<" "> expected to be " " to\n<" 
			 ">.")
		       expected (car opt-pred-name) actual)))
  (lambda (expected actual . opt-message)
    (let-optional 
     opt-message ((message #f))
     (assert-proc (full-message message expected actual)
		  (lambda () (predicate expected actual))))))

(define assert-eq (assert-equivalent eq? "eq?"))
(define assert-eqv (assert-equivalent eqv? "eqv?"))
(define assert-equal (assert-equivalent equal? "equal?"))
(define assert-= (assert-equivalent = "="))
(define assert-equals assert-equal)
(define assert= assert-=)

(define (assert-in-delta expected actual delta . opt-message)
  (let-optional opt-message ((message #f))
   (let ((full-message
	  (build-message message '("<" "> and\n<" "> expected to be within\n<"
				   "> of each other.")
			 expected actual delta)))
     (assert-proc full-message (lambda () (<= (abs (- expected actual)) delta))))))

(define (assert-matches regexp string . opt-message)
  (let-optional opt-message ((message #f))
   (let ((full-message
	  (build-message message '("<" "> expected to match <" ">")
			 string regexp)))
     (assert-proc full-message
		  (lambda ()
		    ;; TODO Fix this
		    (re-string-search-forward regexp string))))))

(define (assert-true thing . opt-message)
  (let-optional opt-message ((message #f))
   (let ((full-message
	  (build-message message '("<" "> expected to be a true value.")
			 thing)))
     (assert-proc full-message (lambda () thing)))))

(define (assert-false thing . opt-message)
  (let-optional opt-message ((message #f))
   (let ((full-message
	  (build-message message '("<" "> expected to be a false value.")
			 thing)))
     (assert-proc full-message (lambda () (not thing))))))
