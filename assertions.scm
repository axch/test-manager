(define condition-type:test-failure
  (make-condition-type 'test-failure condition-type:error
		       '(message) (lambda (condition port)
				    (display (access-condition condition 'message) port))))

(define condition/test-failure?
  (condition-predicate condition-type:test-failure))

(define test-fail
  (condition-signaller condition-type:test-failure
		       '(message) standard-error-handler))

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

(define (assert-equivalent predicate #!optional pred-name)
  (define (full-message message expected actual)
    (if (default-object? pred-name)
	(build-message message
		       '("<" "> expected but was\n<" ">.")
		       expected actual)
	(build-message message
		       '("<" "> expected to be " " to\n<" 
			 ">.")
		       expected pred-name actual)))
  (lambda (expected actual #!optional message)
    (if (default-object? message) (set! message #f))
    (assert-proc (full-message message expected actual)
		 (lambda () (predicate expected actual)))))

(define assert-eq (assert-equivalent eq? "eq?"))
(define assert-eqv (assert-equivalent eqv? "eqv?"))
(define assert-equal (assert-equivalent equal? "equal?"))
(define assert-= (assert-equivalent = "="))
(define assert-equals assert-equal)
(define assert= assert-=)

(define (assert-in-delta expected actual delta #!optional message)
  (if (default-object? message) (set! message #f))
  (let ((full-message
	 (build-message message '("<" "> and\n<" "> expected to be within\n<"
				  "> of each other.")
			expected actual delta)))
    (assert-proc full-message (lambda () (<= (abs (- expected actual)) delta)))))

(define (assert-matches regexp string #!optional message)
  (if (default-object? message) (set! message #f))
  (let ((full-message
	 (build-message message '("<" "> expected to match <" ">")
			string regexp)))
    (assert-proc full-message
		 (lambda ()
		   ;; TODO Fix this
		   (re-string-search-forward regexp string)))))

(define (assert-true thing #!optional message)
  (if (default-object? message) (set! message #f))
  (let ((full-message
	 (build-message message '("<" "> expected to be a true value.")
			thing)))
    (assert-proc full-message (lambda () thing))))

(define (assert-false thing #!optional message)
  (if (default-object? message) (set! message #f))
  (let ((full-message
	 (build-message message '("<" "> expected to be a false value.")
			thing)))
    (assert-proc full-message (lambda () (not thing)))))
