;;;; assert.lisp — A Testsuite for the Kaputt Test Framework

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

(in-package #:kaputt/testsuite)

(define-condition expected-condition (simple-error)
  ((message :initarg :message)))

(define-testcase testsuite-basic-assertions ()
  (assert-t t)
  (assert-t 1)
  (assert-assertion-failed
   (assert-t nil))
  (assert-nil nil)
  (assert-assertion-failed
   (assert-nil 0))
  (assert-type t t)
  (assert-type 1 t)
  (assert-type 1 'integer)
  (assert-assertion-failed
   (assert-type 1 'string)))

(define-testcase testsuite-assert-condition ()
  (assert-condition (error "A simple error") error)
  (assert-condition
      (error 'expected-condition :message "An expected message") expected-condition
      (message)
    (string= message "An expected message"))
  (assert-condition
      (assert-condition (error "A simple error") nil) kaputt::assertion-failed
      (kaputt::assertion-description)
    (ppcre:scan "was expected to signal a condition" kaputt::assertion-description)))

(define-testcase testsuite-assert-string* ()
  (assert-assertion-failed-description
   (assert-string= t (string-upcase nil))
   "The parameter STRING1 is expected to have type STRING but actually has type")
  (assert-assertion-failed-description
   (assert-string<= "AAB" "AAAA")
   "Every character of STRING1 is less than or equal to the character of STRING2")
  (assert-assertion-failed-description
   (assert-string<= "AAB" "AAAA")
   "at position 2, which are #.B and #.A"))

(define-testcase testsuite-list-as-set ()
  (assert-subsetp '(a b) '(a b c))
  (assert-assertion-failed
   (assert-subsetp '(a b c) '(a b)))
  (assert-set-equal '(a b) '(a b))
  (assert-assertion-failed
   (assert-set-equal '(a b) '(a)))
  (assert-set-equal '((a . 1)(b . 2)) '((a . 3)(b . 4)) :key #'car))

(define-testcase testsuite-assert-vector* ()
  (assert-vector-equal #('a 'b) #('a 'b))
  (assert-assertion-failed
   (assert-vector-equal #('a) #('a 'b))))

(define-testcase testsuite-assert-float* ()
  (let ((kaputt::*double-float-precision* 1))
    (assert-float-is-definitely-less-than 0.0 1.0)
    (assert-float-is-definitely-greater-than 1.0 0.0)
    (assert-float-is-approximately-equal 0.6 0.5)
    (assert-float-is-essentially-equal 0.35 0.4)))

(define-testcase testsuite-assert ()
  (testsuite-basic-assertions)
  (testsuite-assert-condition)
  (testsuite-assert-string*)
  (testsuite-list-as-set)
  (testsuite-assert-vector*)
  (testsuite-assert-float*))

;;;; End of file `assert.lisp'
