;;; ----------------------------------------------------------------------
;;; Copyright 2007-2009 Alexey Radul.
;;; ----------------------------------------------------------------------
;;; This file is part of Test Manager.
;;; 
;;; Test Manager is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;; 
;;; Test Manager is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; 
;;; You should have received a copy of the GNU General Public License
;;; along with Test Manager.  If not, see <http://www.gnu.org/licenses/>.
;;; ----------------------------------------------------------------------

(define-test (test-check-captures-info)
  (with-top-level-group
   (make-test-group 'mockery)
   (lambda ()
     (let ((foo 7) (bar 8))
       (define-test (arguments-for-check)
	 (check (> foo bar))))
     (assert-matches
      "1 tests, 1 failures, 0 errors"
      (run-test-capturing-output '(arguments-for-check)))
     (assert-matches
      "(> foo bar)"
      (run-test-capturing-output '(arguments-for-check)))
     (assert-matches
      "(7 8)"
      (run-test-capturing-output '(arguments-for-check))))))

(define-test (test-define-each-check)
  (with-top-level-group
   (make-test-group 'mockery)
   (lambda ()
     (in-test-group subgroup
      (define-each-check
	(even? (* 2 3))
	(odd? (* 4 3))
	(even? (+ 6 1))
	(odd? (+ 8 1))))
     (assert-matches
      "4 tests, 2 failures, 0 errors"
      (run-test-capturing-output '(subgroup)))
     (check (string-search-forward
	     "(odd? (* 4 3))"
	     (run-test-capturing-output '(subgroup))))
     (assert-matches
      "(12)"
      (run-test-capturing-output '(subgroup)))
     (check (string-search-forward
	     "(even? (+ 6 1))"
	     (run-test-capturing-output '(subgroup))))
     (assert-matches
      "(7)"
      (run-test-capturing-output '(subgroup))))))

(define-test (interactions)
  (interaction
   (define foo 5)
   (+ foo 2)
   (produces 7)
   ((if (even? 4) * +) 3 5)
   (produces 15)))
