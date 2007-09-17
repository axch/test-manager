;; This depends on:
;; - queue.scm from the MIT Scheme runtime
;; - fluid-let
;; - hash tables

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

;; Actual code
(load-relative "ordered-map")
(load-relative "assertions")
(load-relative "test-runner")
(load-relative "test-group")
(load-relative "testing")

#|

=pod

=head1 NAME

testing/ - An automatic unit-testing framework for MIT Scheme

=head1 SYNOPSYS

  (load "/infolab/share/lib/testing/load.scm")

  (in-test-group
   simple-stuff
   (define-test (easy)
     (assert-= 4 (+ 2 2) "Two and two should make four.")
     (assert-= 6 (+ 2 2 2)))
   (define-test (harder)
     (assert-= 2147483648 (+ 2147483647 1) "Addition shouldn't overflow."))
   (define-test (cons)
     (assert-equal '(1 2 3) (cons 1 '(2 3)))))

=head1 DESCRIPTION

This test framework defines a language for specifying test suites and
a simple set of commands for running them.  A test suite is a
collection of individual tests grouped into a hierarchy of test
groups.  The test group hierarchy serves to semantically aggregate the
tests, allowing the definition of shared code for set up, tear down,
and surround, and also partition the test namespace to avoid
collisions.

The individual tests are ordinary procedures, with some associated
bookkeeping.  A test is considered to pass if it returns normally,
and to fail if it raises some condition that it does not handle
(tests escaping into continuations leads to unspecified behavior).
The framework provides a library of assertions that can be invoked
in tests and have the desired behavior of raising an appropriate 
condition if they fail.

=head2 Defining Test Suites

All tests are grouped into a hierarchy of test groups.
At any point in the definition of a test suite, there is an implicit
"current test group", into which tests and subgroups can be added.  By
default, the current test group is the top-level test group, which is
the root of the test group hierarchy.

=over

=item (define-test (name) expression ... )

Define a test named C<name> that consists of the given expressions, and
add it to the current test group.  When the test is run, the
expressions will be executed in order, just like the body of any
procedure.  If the test raises any condition that it does not handle,
it is considered to have failed.  If it returns normally, it is
considered to have passed.  Usually, tests will contain assertions
from L<assertions.scm>, which raise appropriate conditions when they
fail.

=item (in-test-group name expression ... )

Locate (or create) a test subgroup called C<name> in the current test
group.  Then temporarily make this subgroup the current test group,
and execute the expressions in the body of C<in-test-group>.  This
groups any tests and further subgroups defined by those expressions
into this test group.  Test groups can nest arbitrarily deep.  Test
groups serve to disambiguate the names of tests, and to group them
semantically.  In particular, should a test fail, the names of the
stack of groups it's in will be displayed along with the test name
itself.

=item (define-set-up expression ...)

Defines a sequence of expressions to be run before every test in
the current test group.  Clobbers any previously defined set up
for this group.

=item (define-tear-down expression ...)

Defines a sequence of expressions to be run after every test in
the current test group.  Clobbers any previously defined tear down
for this group.

=item (define-surround expression ...)

Defines a sequence of expressions to be run surrounding every test in
the current test group.  Inside the C<define-surround>, the identifier
C<run-test> is bound to a nullary procedure that actually runs the
test.  Clobbers any previously defined surround for this group.

=item (define-group-set-up expression ...)

Defines a sequence of expressions to be run once before running any
test in the current test group.  Clobbers any previously defined group
set up for this group.

=item (define-group-tear-down expression ...)

Defines a sequence of expressions to be run once after running all
tests in the current test group.  Clobbers any previously defined
group tear down for this group.

=item (define-group-surround expression ...)

Defines a sequence of expressions to be run once surrounding running
the tests in the current test group.  Inside the
C<define-group-surround>, the identifier C<run-test> is bound to a
nullary procedure that actually runs the tests in this group.
Clobbers any previously defined group surround for this group.

=back

=head2 Running Test Suites

The following procedures are provided for running test suites:

=over

=item (run-test name-stack)

Looks up the test indicated by name-stack in the current test group,
runs it, and prints a report of the results.  Returns the number of
tests that did not pass.  An empty list for a name stack indicates the
whole group, a singleton list indicates that immediate descendant, a
two-element list indicates a descendant of a descentant, etc.

=item (run-registered-tests)

Runs all tests registered so far, and prints a report of the results.
Returns the number of tests that did not pass.  This could have been
defined as C<(run-test '())>.

=back

=head2 Assertions

The following assertions are provided for writing tests:

=over 

=item (assert-proc message proc)

Passes iff the given procedure, invoked with no arguments, returns a
true value.  On failure, arranges for the given message to appear in
the failure report.  This is a primitive assertion in whose terms
other assertions are defined.

=item (assert-true thing #!optional message)

Passes iff the given value is a true value (to wit, not #f).

=item (assert-false thing #!optional message)

Passes iff the given value is a false value (to wit, #f).

=item (assert-equal expected actual #!optional message)
 Likewise assert-eqv, assert-eq, and assert-=

Passes iff the given actual value is equivalent, according to the
corresponding predicate, to the expected value.  Produces a reasonably
helpful message on failure, and includes the optional message argument
in it if present.  When in doubt, use C<assert-equal> to compare most
things; use C<assert-=> to compare exact numbers like integers; and
use C<assert-in-delta>, below, for inexact numbers like floating points.

=item assert-equals, assert=

Are aliases for assert-equal and assert-=, respectively.

=item (assert-equivalent predicate #!optional pred-name)

This is intended as a tool for building custom assertions.  Returns an
assertion procedure that compares an expected and an actual value with
the given predicate and produces a reasonable failure message.
C<assert-equal> and company could have been defined in terms of
C<assert-equivalent> as, for example, C<(define assert-equal
(assert-equivalent equal? "equal?"))>.

=item (assert-matches regexp string #!optional message)

Passes iff the given regular expression matches the given string.

=item (assert-in-delta expected actual delta #!optional message)

Passes iff the given actual value differs, in absolute value, from the
given expected value by no more than delta.  Use this in preference
to C<assert-=> to check sameness of inexact numerical values.

=back

=head1 BUGS

This unit testing framework is a work in progress.  The assertion
library is quite impoverished, the test groups do not support as much
shared set up code among their tests as I would like, and the language
for explicit test group handling is ill-specified and undocumented
(peruse test-group.scm if interested).  Suggestions are welcome.

=head1 AUTHOR

Alexey Radul, axch@mit.edu

=cut

|#
