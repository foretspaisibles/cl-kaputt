# Kaputt – A Simple Interactive Test Framework for Common Lisp

**Kaputt** is a test framework for Common Lisp that focuses on the
following features:

- *Kaputt* is simple, it only defines three abstractions *testcase*,
  *assertion* and *protocol* and does not add any artefact on the
  backtrace when errors occur.

- *Kaputt* is extensible, it is possible to add problem-specific
  assertions to make test code more informative.

- *Kaputt* fits well interactive development.

See [Comparison to some-other available test frameworks](#comparison-to-some-other-available-test-frameworks)
below for a more detailed motivation of the need for a new test framework.

## Table of Contents

  * [Installation](#installation)
  * [Simple Usage](#simple-usage)
  * Fixtures
  * Mocking and Stubbing
  * Reiteration
  * Skipping tests
  * TODO Flacky tests
  * TODO Randomise test order
  * TODO Parallelise tests
  * TODO Parameter generator
  * TODO Fuzzer
  * TODO Junit Report

## Installation

### Quicklisp users

*Kaputt* is *not* yet available in the *Quicklisp* distribution, so
*Kaputt* needs to be installed manually. Clone the repository in a
directory listed in the `ql:*local-project-directories*` parameter and
`(ql:register-local-projects)` so that you can `(require "kaputt")`.

### ASDF users

Clone the repository in a place where ASDF can find it, for instance
`~/common-lisp` if you are using a modern ASDF module with its default
configuration. Then `(require "kaputt")`.


## Simple Usage

This is illustrated by the file `example.lisp` from the
distribution. We describe an interactive session based on the content
of this file. After evaluating

~~~ lisp
(defpackage #:kaputt/example
  (:use #:common-lisp #:kaputt)
  (:export
   #:run-all-tests))

(in-package #:kaputt/example)

(define-testcase cl-strings/string-downcase-turns-nil-into-a-string ()
  (assert-string= "nil" (string-downcase nil)))
~~~

We can move to the REPL to evaluate:

~~~ lisp
CL-USER> (in-package #:kaputt/example)
#<PACKAGE "KAPUTT/EXAMPLE">

KAPUTT/EXAMPLE> (cl-strings/string-downcase-turns-nil-into-a-string)
.

Test suite ran 1 assertions split across 1 test cases.
 Success: 1/1 (100%)
 Failure: 0/1 (0%)

T
~~~

Let's define a few more tests by evaluating a further form from
`example.lisp` but we modify it to display how Kaputt handles failing
assertions:

~~~ lisp
(define-testcase cl-strings/string-upcase-turns-nil-into-a-string ()
  (assert-string= "nil" (string-upcase nil)))
~~~

The evaluation of the function

~~~ lisp
KAPUTT/EXAMPLE> (cl-strings/string-upcase-turns-nil-into-a-string)
~~~

summons the debugger:

~~~
Test assertion failed:

  (ASSERT-STRING= "nil" (STRING-UPCASE NIL))

In this call, the composed forms in argument position evaluate as:

  (STRING-UPCASE NIL) => "NIL"

The assertion ASSERT-STRING= is defined by the body forms

  (STRING= STRING1 STRING2 :START1 START1 :END1 END1 :START2 START2 :END2 END2)

It has no high-level description.
   [Condition of type KAPUTT::ASSERTION-FAILED]

Restarts:
 0: [CONTINUE] Record a failure for ASSERT-STRING= and continue testing.
 1: [IGNORE] Record a success for ASSERT-STRING= and continue testing.
 2: [RETRY] Retry ASSERT-STRING=.
 3: [SKIP] Skip the rest of test case CL-STRINGS/STRING-UPCASE-TURNS-NIL-INTO-A-STRING and continue testing.
 4: [RETRY] Retry SLIME REPL evaluation request.
 5: [*ABORT] Return to SLIME's top level.
 --more--
~~~

We can fix the testcase by formulating a reasonable expectation:

~~~ lisp
(define-testcase cl-strings/string-upcase-turns-nil-into-a-string ()
  (assert-string= "NIL" (string-upcase nil)))
~~~

and `4. [RETRY]` the SLIME REPL evaluation request to see the tests
complete.  The report describing the error condition met by the
failing assertion can be customised freely when defining a new assertion.

We can finally organise our tests in a testsuite by defining a test
case calling other test cases:

~~~ lisp
(define-testcase run-all-tests ()
  (cl-strings/string-downcase-turns-nil-into-a-string)
  (cl-strings/string-upcase-turns-nil-into-a-string))
~~~

It is possible to define parametrised testcases. See in `kaputt.lisp`
various example of advanced usages of the `define-assertion` function.


## Fixtures

In *Kaputt* a testcase is just a normal function. It computes a
specific value and runs in a specific environment but it is really
just a plain Lisp function.  As a consequence there is no real need
for supporting fixtures in *Kaputt*.

A simple `let` clause can be used to set test-specific values for
parameters.

Database connection, contents, etc. can conveniently be provided by a
`with-*` macro.

## Mocking and Stubbing

No support yet.


## Reiteration

There is currently no support to run again only failed tests.


## Skipping tests

Use Common Lisp features system:

~~~ lisp
(define-testcase run-arch-specific-tests ()
	#+ARM
	(assert- ))
~~~

## TODO Flacky tests

Mark a test as flacky (non-reproducible failures).

A parameter governs if flacky tests are failing or not. This parameter
can be bound locally with a `with-*` macro.

Failed flacky tests are counted separately.  This is mostly a
documentation purpose.


## TODO Randomise test order

There is test supervisor that allows for parallel and randomised test
execution.  Random seeds are traced all the way long
(reproducibility). An independant random seed is used by this facility.

## TODO Parallelise tests

There is test supervisor that allows for parallel test
execution. Timeline is recorded and could allow reproducibility of a
specific run order.

## TODO Parameter generator

There is functions allowing for random generation of
parameters. Random seeds are traced all the way long
(reproducibility). An independant random seed is used by this
facility.  What is the threads semantic?

Note: Random seeds are quite complicated in lisp but they can be
`print`-ed.

## TODO Fuzzer

!!! The functionality is not clear, let's see how renowned fuzzer work. !!!

There is functions allowing to fuzz code.  In a testcase the `fuzz`
macro can be used on a form to randomly modify its result.

## TODO Junit Report

Write test reports in JUnit format, which makes it easy to display in
CID tools.


### Defining an ASDF system depending on Kaputt

This snippets show an example of ASF system definition for a systeme
depending on *Kaputt*:

~~~ lisp
(asdf:defsystem #:kaputt/example
  :description "An Example for the Kaputt Test Framework"
  :author "Michaël Le Barbier"
  :license "CeCILL-B"
  :depends-on (#:kaputt)
  :components
  ((:file "example")))
~~~

