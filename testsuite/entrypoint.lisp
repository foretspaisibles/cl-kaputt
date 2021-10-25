;;;; entrypoint.lisp — A Testsuite for the Kaputt Test Framework

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


(defpackage #:kaputt/testsuite
  (:use #:common-lisp #:kaputt)
  (:export
   #:run-all-tests)
  (:documentation
   "A testsuite for the Kaputt Test Framewok."))

(in-package #:kaputt/testsuite)

(defparameter *assertion-description* nil
  "The description of the assertion being tested.")

(defparameter *assertion-form* nil
  "The form featuring the assertion being tested.")

(defparameter *assertion-args* nil
  "The evaluated arguments of the assertion being tested.")

(defmacro validate-assert-form (form &body body)
  `(handler-case ,form
     (kaputt::assertion-failed (condition)
       (let ((*assertion-description*
               (kaputt::assertion-description condition))
             (*assertion-form*
               (kaputt::assertion-form condition))
             (*assertion-args*
               (kaputt::assertion-args condition)))
         (unless (progn ,@body)
           (error "Validate assertion failed."))))))

(defun test-string-comparison ()
  (validate-assert-form (assert-string= t (string-upcase nil))
    (ppcre:scan
     "The parameter STRING1 is expected to have type STRING but actually has type"
     *assertion-description*))
  (validate-assert-form (assert-string<= "AAB" "AAAA")
    (ppcre:scan
     "Every character of STRING1 is less than or equal to the character of STRING2"
     *assertion-description*))
  (validate-assert-form (assert-string<= "AAB" "AAAA")
    (ppcre:scan
     "at position 2, which are #.B and #.A"
     *assertion-description*)))

(defun run-all-tests ()
  (handler-case
      (progn
        (test-string-comparison)
        t)
    (error (condition)
      (declare (ignore condition))
      nil)))

;;;; End of file `entrypoint.lisp'
