;;; I apologize to the reader for this horrible collection of hacks,
;;; but Guile appears to lack a condition system worth the name, so I
;;; am synthesizing one with exactly (read: only) the characteristics
;;; I need on top of catch-throw.

(define-record-type condition
  (make-condition type message continuation)
  condition?
  (type condition/type)
  (message condition/message)
  (continuation condition/continuation))

(define (condition/test-failure? condition)
  (eq? 'test-failure (condition/type condition)))

(define (condition/error? condition)
  (not (condition/test-failure? condition)))

(define (test-fail message)
  (throw 'test-failure "test-fail" message))

(define (ignore-errors thunk)
  "Run the given thunk.  If it returns normally, return its return
value.  If it signals an error, return an object representing that
error instead."
  (define (extract-message throw-arguments)
    ;; TODO This relies on the arguments following Guile's throwing 
    ;; convention.
    (cadr throw-arguments))
  (let ((error-object #f))
    (catch 
     #t
     thunk
     (lambda (key . args)
       error-object)
     (lambda (key . args)
       (call-with-current-continuation
	(lambda (thrown-at)
	  (set! error-object
		(make-condition
		 key
		 (extract-message args)
		 thrown-at))))))))

(define (write-condition-report condition port)
  (display (condition/message condition) port))
