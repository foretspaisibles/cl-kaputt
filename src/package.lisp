;;;; package.lisp — Package for Kaputt

;;;; Kaputt (https://github.com/foretspaisibles/cl-kaputt)
;;;; This file is part of Kaputt.
;;;;
;;;; Copyright © 2019–2021 Michaël Le Barbier
;;;; All rights reserved.

;;;; This software is governed by the CeCILL-B license under French law and
;;;; abiding by the rules of distribution of free software.  You can  use, 
;;;; modify and/ or redistribute the software under the terms of the CeCILL-B
;;;; license as circulated by CEA, CNRS and INRIA at the following URL
;;;; "https://cecill.info/licences/Licence_CeCILL-B_V1-en.txt"

(defpackage #:kaputt
  (:use #:common-lisp)
  (:import-from #:alexandria #:with-unique-names)
  (:export
   ;; Assertions and Testcases
   #:define-assertion
   #:define-testcase
   #:assert-t
   #:assert-t*
   #:assert-nil
   #:assert-type
   #:assert-eq
   #:assert-eql
   #:assert-equal
   #:assert=
   #:assert<
   #:assert>
   #:assert<=
   #:assert>=
   #:assert-condition
   #:assert-string-equal
   #:assert-string=
   #:assert-string<
   #:assert-string>
   #:assert-string<=
   #:assert-string>=
   #:assert-subsetp
   #:assert-set-equal
   #:assert-vector-equal
   #:assert-float-is-approximately-equal
   #:assert-float-is-definitely-greater-than
   #:assert-float-is-definitely-less-than
   #:assert-float-is-essentially-equal
   ;; Results
   #:assertion
   #:assertion-p
   #:make-assertion
   #:assertion-name
   #:assertion-path
   #:assertion-arguments
   #:assertion-form
   #:assertion-type
   #:assertion-outcome
   #:assertion-description
   #:assertion-condition
   #:testcase
   #:testcase-p
   #:make-testcase
   #:testcase-name
   #:testcase-path
   #:testcase-arguments
   #:testcase-total
   #:testcase-success
   #:testcase-failure
   #:testcase-condition
   #:testcase-skip
   #:testcase-results
   ;; Utilities
   #:assert-failure
   ;; Error handlers
   #:testcase-batch
   #:testcase-ignore
   #:testcase-continue
   ;; Test Run Supervisor
   #:supervisor-testsuite-begin
   #:supervisor-testsuite-end
   #:supervisor-testcase-begin
   #:supervisor-testcase-end
   #:supervisor-assertion
   #:supervisor-success-p
   #:supervisor
   #:supervisor-verbose
   #:supervisor-trace
   #:supervisor-count
   #:supervisor-record
   #:supervisor-dotta
   #:*testcase-protocol-class*
   )
  (:documentation
   "The Kaputt Test Framework."))

;;;; End of file `package.lisp'
