;;; These are the definitions that are actively intertwined with MIT
;;; Scheme's condition system, which this test manager originally
;;; used.  They are replaced by equivalent (I hope) domain-specific
;;; definitions tailored for other condition systems in other
;;; *-conditions.scm files.

(define condition-type:test-failure
  (make-condition-type 'test-failure condition-type:error
		       '(message) (lambda (condition port)
				    (display (access-condition condition 'message) port))))

(define condition/test-failure?
  (condition-predicate condition-type:test-failure))

(define test-fail
  (condition-signaller condition-type:test-failure
		       '(message) standard-error-handler))
