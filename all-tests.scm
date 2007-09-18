(define-test (test-structure-smoke)
  (let ((mock-test-group (make-test-group 'mockery)))
    (with-top-level-group 
     mock-test-group
     (lambda ()
       (define-test (foo)
	 foo!)))
    (assert-= 1 (omap:count (tg:test-map mock-test-group)))
    (assert-true (single-test? (tg:get mock-test-group '(foo))))))

(define-test (test-structure)
  (let ((mock-test-group (make-test-group 'mockery)))
    (with-top-level-group 
     mock-test-group
     (lambda ()
       (in-test-group
	subgroup-1
	(define-test (some-name) foo!)
	(define-test (repeated-name) bar!))
       (in-test-group
	subgroup-2
	(define-test (some-other-name) baz!)
	(define-test (repeated-name) quux!))))
    (assert-= 2 (omap:count (tg:test-map mock-test-group)))
    (assert-true (test-group? (tg:get mock-test-group '(subgroup-1))))
    (assert-true (test-group? (tg:get mock-test-group '(subgroup-2))))
    (let ((fetched-tests
	   (map (lambda (test-path)
		  (assert-eq (tg:get mock-test-group test-path)
			     (tg:get mock-test-group test-path))
		  (tg:get mock-test-group test-path))
		'((subgroup-1 some-name)
		  (subgroup-1 repeated-name)
		  (subgroup-2 some-other-name)
		  (subgroup-2 repeated-name)))))
      (for-each (lambda (test)
		  (assert-true (single-test? test)))
		fetched-tests))
    (assert-false
     (eq? (tg:get mock-test-group '(subgroup-1 repeated-name))
	  (tg:get mock-test-group '(subgroup-2 repeated-name))))))

(define-test (test-running)
  (let ((mock-test-group (make-test-group 'mockery))
	(events '()))
    (define (add-event event)
      (set! events (cons event events)))
    (with-top-level-group 
     mock-test-group
     (lambda ()
       (define-group-surround
	 (add-event 'top-group-surround-begin)
	 (run-test)
	 (add-event 'top-group-surround-end))
       (define-group-set-up (add-event 'top-group-set-up))
       (define-group-tear-down (add-event 'top-group-tear-down))
       (define-surround
	 (add-event 'top-surround-begin)
	 (run-test)
	 (add-event 'top-surround-end))
       (define-set-up (add-event 'top-set-up))
       (define-tear-down (add-event 'top-tear-down))
       (in-test-group
	group-a
	(define-group-surround
	  (add-event 'a-group-surround-begin)
	  (run-test)
	  (add-event 'a-group-surround-end))
	(define-group-set-up (add-event 'a-group-set-up))
	(define-group-tear-down (add-event 'a-group-tear-down))
	(define-surround
	  (add-event 'a-surround-begin)
	  (run-test)
	  (add-event 'a-surround-end))
	(define-set-up (add-event 'a-set-up))
	(define-tear-down (add-event 'a-tear-down))
	(define-test (test-a1)
	  (add-event 'test-a1)
	  (assert-= 5 (+ 2 2)))
	(define-test (test-a2)
	  (add-event 'test-a2)
	  (assert-equal '() #f)))
       (in-test-group
	group-b
	(define-group-surround
	  (add-event 'b-group-surround-begin)
	  (run-test)
	  (add-event 'b-group-surround-end))
	(define-group-set-up (add-event 'b-group-set-up))
	(define-group-tear-down (add-event 'b-group-tear-down))
	(define-surround
	  (add-event 'b-surround-begin)
	  (run-test)
	  (add-event 'b-surround-end))
	(define-set-up (add-event 'b-set-up))
	(define-tear-down (add-event 'b-tear-down))
	(define-test (test-b1)
	  (add-event 'test-b1)
	  (foo))
	(define-test (test-b2)
	  (add-event 'test-b2)
	  (assert-= 4 (+ 2 (/ 2 0)))))))
    (let ((result-string
	   (with-output-to-string
	     (lambda ()
	       (with-top-level-group
		mock-test-group
		(lambda ()
		  (run-registered-tests)))))))
      (assert-matches
       "4 tests, 2 failures, 2 errors"
       result-string))
    (assert-equal
     ;; Beware, the indentation in this list was produced manually
     '(top-group-surround-begin top-group-set-up
       top-surround-begin top-set-up
        a-group-surround-begin a-group-set-up
         a-surround-begin a-set-up test-a1 a-tear-down a-surround-end
	 a-surround-begin a-set-up test-a2 a-tear-down a-surround-end
        a-group-tear-down a-group-surround-end
       top-tear-down top-surround-end
       top-surround-begin top-set-up
        b-group-surround-begin b-group-set-up
	 b-surround-begin b-set-up test-b1 b-tear-down b-surround-end
	 b-surround-begin b-set-up test-b2 b-tear-down b-surround-end
	b-group-tear-down b-group-surround-end
       top-tear-down top-surround-end
       top-group-tear-down top-group-surround-end)
     (reverse events))))

(let ((entered-group #f))
  (in-test-group
   a-test-group-with-surroundings
   (set-tg:group-set-up! (current-test-group)
			 (lambda ()
			   (set! entered-group #t)))
   (define-test (check-enter-this-thunk-runs)
     (assert-eq #t entered-group))))

