;; Macros
(cond-expand
 (guile
  (use-modules (ice-9 syncase)))
 (else))

;; SRFI-9: define-record-type
(cond-expand
 (guile
  (use-modules (srfi srfi-9)))
 (srfi-9))

;; Structured conditions
(cond-expand
 (guile
  (load-relative "guile-conditions"))
 (else ;; The MIT Scheme that knows it is 'mit' isn't in Debian Stable yet
  (load-relative "mitscheme-conditions")))

;; SRFI-69: Hash tables
(cond-expand
 (srfi-69)
 (else ; Do I want to use Guile's hash tables instead?
  (load-relative "srfi-69-hash-tables")))

;; Optional arguments
(cond-expand
 (guile
  (use-modules (ice-9 optargs)))
 (else ;; The MIT Scheme that knows it is 'mit' isn't in Debian Stable yet
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

;; Fluid-let (in the MIT Scheme sense of the word 'fluid'.
(cond-expand
 (guile
  (define-syntax fluid-let
    (syntax-rules ()
      ((_ () expr ...)
       (begin expr ...))
      ((_ ((variable1 value1) binding ...) expr ...)
       (let ((out-value variable1)
	     (in-value value1))
	 (dynamic-wind
	     (lambda ()
	       (set! out-value variable1)
	       (set! variable1 in-value))
	     (lambda ()
	       (fluid-let (binding ...)
		 expr ...))
	     (lambda ()
	       (set! in-value variable1)
	       (set! variable1 out-value))))))))
 (else))

;; Regexes (using MIT Scheme's name for no good reason)
(cond-expand
 (guile
  (use-modules (ice-9 regex))
  (define re-string-search-forward string-match))
 (else))
