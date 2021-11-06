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

(declaim (optimize safety debug))

(define-condition expected-condition (simple-error)
  ((message :initarg :message)))

(define-testcase a-simple-failure ()
  (assert-t nil))

(define-testcase testsuite-define-assertion ()
  (assert-t (functionp #'assert-t))
  (testcase-outcome-bind (outcome)
      (testcase-ignore (a-simple-failure))
    (assert-t outcome))
  (testcase-outcome-bind (outcome)
      (testcase-continue (a-simple-failure))
    (assert-nil outcome)))

(define-testcase testsuite-basic-assertions ()
  (assert-t t)
  (assert-t 1)
  (assert-failure
   (assert-t nil))
  (assert-nil nil)
  (assert-failure
   (assert-nil 0))
  (assert-type t t)
  (assert-type 1 t)
  (assert-type 1 'integer)
  (assert-failure
   (assert-type 1 'string)))

(define-testcase testsuite-assert-condition ()
  (assert-condition (error "A simple error") error)
  (assert-condition
      (error 'expected-condition :message "An expected message") expected-condition
      (message)
    (string= message "An expected message"))
  (assert-nil
   (assert-condition (error "A simple error") nil)))

(define-testcase testsuite-assert-string* ()
  (assert-failure
   (assert-string= t (string-upcase nil))
   "*The parameter STRING1 is expected to have type STRING but actually has type*")
  (assert-failure
   (assert-string<= "AAB" "AAAA")
   "*Every character of STRING1 is less than or equal to the character of STRING2*")
  (assert-failure
   (assert-string<= "AAB" "AAAA")
   "*at position 2, which are #?B and #?A*"))

(define-testcase testsuite-list-as-set ()
  (assert-subsetp '(a b) '(a b c))
  (assert-failure
   (assert-subsetp '(a b c) '(a b)))
  (assert-set-equal '(a b) '(a b))
  (assert-failure
   (assert-set-equal '(a b) '(a)))
  (assert-set-equal '((a . 1)(b . 2)) '((a . 3)(b . 4)) :key #'car))

(define-testcase testsuite-assert-vector* ()
  (assert-vector-equal #(0 1) #(0 1))
  (assert-failure
   (assert-vector-equal #(0) #(0 1))))

(define-testcase testsuite-assert-float* ()
  (let ((kaputt::*double-float-precision* 1))
    (assert-float-is-definitely-less-than 0.0 1.0)
    (assert-float-is-definitely-greater-than 1.0 0.0)
    (assert-float-is-approximately-equal 0.6 0.5)
    (assert-float-is-essentially-equal 0.35 0.4)))

(define-testcase testsuite-assert ()
  (testsuite-define-assertion)
  (testsuite-basic-assertions)
  (testsuite-assert-condition)
  (testsuite-assert-string*)
  (testsuite-list-as-set)
  (testsuite-assert-vector*)
  (testsuite-assert-float*))

;;;; End of file `assert.lisp'
