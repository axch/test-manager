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



(define-test (interactions)
  (interaction
   (define foo 5)
   (+ foo 2)
   (produces 7)
   ((if (even? 4) * +) 3 5)
   (produces 15)))