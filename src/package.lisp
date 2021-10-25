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
  (:export
   ;; Assertions and Testcases
   #:define-assertion
   #:define-testcase
   #:assert-t
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
   ;; Test Run Protocol
   #:protocol-testsuite-begin
   #:protocol-testsuite-end
   #:protocol-testcase-begin
   #:protocol-testcase-end
   #:protocol-assertion-begin
   #:protocol-assertion-end
   #:protocol-success-p
   #:protocol
   #:protocol-verbose
   #:protocol-trace
   #:protocol-count
   #:protocol-record
   #:protocol-dotta
   #:*testcase-protocol-class*
   )
  (:documentation
   "The Kaputt Test Framework."))

;;;; End of file `package.lisp'
