;;; ----------------------------------------------------------------------
;;; Copyright 2007-2008 Alexey Radul.
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
