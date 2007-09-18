;; This depends on:
;; - queue.scm from the MIT Scheme runtime
;; - fluid-let

;; define-record-type
(cond-expand
 (guile
  (use-modules (srfi srfi-9)))
 (srfi-9))

;; load-relative, broken in Guile, depends on MIT Scheme's pathname
;; system.
;; TODO Fix for interactive use?
(cond-expand
 (guile
  (define (load-relative filename)
    (load (string-concatenate (list filename ".scm")))))  ; This is not quite right
 (else ;; What symbol is MIT Scheme?
  (define (load-relative filename)
    (with-working-directory-pathname 
     (directory-namestring (current-load-pathname))
     (lambda () (load filename))))))

;; Structured conditions
(cond-expand
 (guile
  (load-relative "guile-conditions"))
 (else ;; What symbol is MIT Scheme?
  (load-relative "mitscheme-conditions")))

;; Optional arguments
(cond-expand
 (guile
  (use-modules (ice-9 optargs)))
 (else ;; What symbol is MIT Scheme?
  (define-syntax let-optional
    (syntax-rules ()
      ((_ arg-list () expr ...)
       (begin expr ...))
      ((_ arg-list ((variable1 default1) binding ...) expr ...)
       (if (null? arg-list)
	   (let ((variable1 default1) binding ...)
	     expr ...)
	   (let ((variable1 (car arg-list))
		 (arg-list (cdr arg-list)))
	     (let-optional
	      arg-list
	      (binding ...)
	      expr ...))))
      ((_ arg-list (variable1 binding ...) expr ...)
       (let ((variable1 (car arg-list))
	     (arg-list (cdr arg-list)))
	 (let-optional
	  arg-list
	  (binding ...)
	  expr ...)))
      ))
  ))

;; Hash tables
(cond-expand
 (srfi-69)
 (else ; Do I want to use Guile's hash tables instead?
  (load-relative "srfi-69-hash-tables")))

;; Macros
(cond-expand
 (guile
  (use-modules (ice-9 syncase)))
 (else))

;; Actual code
(load-relative "ordered-map")
(load-relative "assertions")
(load-relative "test-runner")
(load-relative "test-group")
(load-relative "testing")
