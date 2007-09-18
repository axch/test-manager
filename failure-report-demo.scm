(define-test (this-test-passes)
  (assert-eqv 4 (+ 2 2) "Two plus two isn't four."))

(define-test (this-test-fails)
  (assert-eqv 5 (+ 2 2) "Two plus two isn't five."))

(in-test-group
 a-test-group
 (define-test (happy-internal-test)
   (assert-= 12 (* 3 4) "Three by four should be twelve"))
 (define-test (unhappy-internal-test)
   (assert-equal '() #f "Nil and false are different"))
 (define-test (broken-internal-test)
   (foo))
 (let ((this-test-group *current-test-group*))
   (define-test (meta-internal-test)
     (assert-equal '(happy-internal-test unhappy-internal-test
					 broken-internal-test
					 meta-internal-test)
		   (omap:key-list (tg:test-map this-test-group))))))

(in-test-group
 failed-assertion-showcase
 (define-test (fail-generic-assert-equivalent)
   ((assert-equivalent (lambda (x y)
			 (or (eq? x y)
			     (and (list? x)
				  (list? y)))))
    #(a) #(f))))

(define-test (this-test-errors)
  (assert-eqv 4 (+ 2 (/ 2 0)) "Don't divide by zero."))

(run-registered-tests)
