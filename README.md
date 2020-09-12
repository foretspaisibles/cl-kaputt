# Kaputt – A Simple Interactive Test Framework for Common Lisp

**Kaputt** is a test framework for Common Lisp that focuses on the
following features:

- *Kaputt* is simple, it only defines three abstractions *testcase*,
  *assertion* and *protocol* and does not add any artefact on the
  backtrace when errors occur.

- *Kaputt* is extensible, it is possible to add problem-specific
  assertions to make tests more informative.

- *Kaputt* fits well interactive development.

See [Comparison to some-other available test frameworks](#comparison-to-some-other-available-test-frameworks)
below for a more detailed motivation of the need for a new test framework.

## Table of Contents

  * [Installation](#installation)
  * [Usage](#usage)
  * [Comparison to some other available test frameworks](#comparison-to-some-other-available-test-frameworks)

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


## Usage

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


### Defining an ASDF system depending on Kaputt

This snippets show an example of ASF system definition for a systeme
depending on *Kaputt*:

~~~ lisp
(asdf:defsystem #:kaputt/example
  :description "An Example for the Kaputt Test Framework"
  :author "Michaël Le Barbier"
  :license "MIT"
  :depends-on (#:kaputt)
  :components
  ((:file "example")))
~~~


## Comparison to some other available test frameworks

Before writing *Kaputt* I worked with *Stefil* and *FiveAM* which in
my experience were:


### Not simple

These other test frameworks make provision to organise tests
hierarchically in test suites.  In *Kaputt* a test case is also a
normal Lisp function and test suites can be modelled by test cases
calling several test cases or test suites in sequence.  There is no
need for specific support in the test framework to define this
hierarchical organisation.

These other test frameworks make provision to define test fixtures
(test setup and tearoff). In *Kaputt* since a test case is also a
function, the usual `WITH-*` idiom can be used to define test fixtures
and therefore the test framework does not need to define specific
support for these.


### Not extensible

These other test frameworks are built around an `IS` macro which is
presented as a generic “do what I mean” comparison operator.  I found
this not convenient to use in specific situations, e.g. when testing
numerical algorithms and was very unsatisified with the kludges and
workarounds I needed to build to still use the `IS` macro and found
the resulting code rather convoluted and uninformative.

In *Kaputt* tests are built around *assertions* and new assertions can
be defined with the `DEFINE-ASSERTION` macro.


### Not fitting well interactive development

These other frameworks using the “do what I mean” comparison approach
were not showing me the information I needed to make a first
diagnostic of error conditions met and I spent a lot of time
*evaluating* various expressions in the more-or-less right backtrace
frame.

In *Kaputt* we have total control of the report produced by the error
condition triggered by error failures, so that reports are much more
informative and lead to quicker error resolution

