;;;; testcase.lisp — A Testsuite for the Kaputt Test Framework

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

(define-testcase t1-success ()
  "The first passing test should have a basic “is” test and a signals test."
  (assert= 1 1)
  (assert-condition (error 'division-by-zero) division-by-zero))

(define-testcase validate-t1-success ()
  (testcase-outcome-bind (outcome result)
      (t1-success)
    (assert-t outcome)
    (assert= (length (testcase-results result)) 2)))

(define-testcase t1-fail ()
  "The most basic failing test."
  (assert= 1 2))

(define-testcase validate-t1-fail ()
  (testcase-outcome-bind (outcome result)
      (testcase-batch (t1-fail))
    (assert-nil outcome)
    (assert= (length (testcase-results result)) 1)))

(define-testcase t2 ()
  "describe t2"
  (assert= 1 1)
  (assert= 2 2)
  ;; This test is as in the upstrean definition of t2
  ;;  It should fail, but we do not want to support
  ;;  multiple values (yet).
  (assert= (values 1 2) (values 1 3)))

(define-testcase validate-t2 ()
  (testcase-outcome-bind (outcome result)
      (testcase-batch (t2))
    (assert-t outcome)
    (assert= (length (testcase-results result)) 3)))

(let ((l1 '(#\a #\B #\z))
      (l2 '(97 66 122)))
  (define-testcase t2-loop ()
      (loop for x in l1 for y in l2 do
            (assert= (char-code x) y))))

(define-testcase validate-t2-loop ()
  (testcase-outcome-bind (outcome result)
      (testcase-batch (t2-loop))
    (assert-t outcome)
    (assert= (length (testcase-results result)) 3)))

(define-testcase t3 ()
  "describe t3"
  (assert-eq 'a 'a)
  (t2))

(define-testcase validate-t3 ()
  (testcase-outcome-bind (outcome result)
      (testcase-batch (t3))
    (assert-t outcome)
    (assert= (length (testcase-results result)) 2)
    (assert-t (kaputt::assertion-p (first (testcase-results result))))
    (assert-t (kaputt::testcase-p (second (testcase-results result))))))

(define-testcase testsuite-testcase ()
  (validate-t1-success)
  (validate-t1-fail)
  (validate-t2)
  (validate-t2-loop)
  (validate-t3))

;;;; End of file `testcase.lisp'
