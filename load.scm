;; load-relative, broken in Guile, depends on MIT Scheme's pathname
;; system.
;; TODO Fix for interactive use?
(cond-expand
 (guile
  (define (load-relative filename)
    (load (string-concatenate (list filename ".scm")))))  ; This is not quite right
 (else ;; The MIT Scheme that knows it is 'mit' isn't in Debian Stable yet
  (define (load-relative filename)
    (with-working-directory-pathname 
     (directory-namestring (current-load-pathname))
     (lambda () (load filename))))))

(load-relative "portability")
(load-relative "ordered-map")
(load-relative "assertions")
(load-relative "test-runner")
(load-relative "test-group")
(load-relative "testing")
