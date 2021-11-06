;;;; utilities.lisp — Utilities for the Kaputt Test Framework Testsuite

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

(defmacro testcase-outcome-bind ((outcome &optional result output) form &body body)
  "Run a testcase FORM in a controlled environment and validate RESULT with BODY forms."
  (let ((buffer
	  (gensym "BUFFER"))
	(actual-result
	  (or result (gensym "RESULT")))
	(actual-output
	  (or output (gensym "OUTPUT"))))
    `(let ((,buffer
	     (make-string-output-stream)))
       (multiple-value-bind (,outcome ,actual-result)
	   (let ((kaputt::*current-supervisor*
		   (make-instance 'kaputt::trace-supervisor))
		 (kaputt::*testcase-results*
		   nil))
	     (setf (slot-value kaputt::*current-supervisor* 'kaputt::stream-output)
		   ,buffer)
	     ,form)
	 ,(unless result `(declare (ignore ,actual-result)))
	 (let ((,actual-output
		 (get-output-stream-string ,buffer)))
	   ,(unless output `(declare (ignore ,actual-output)))
	   ,@body)))))


;;;;
;;;; Testing STRING-MATCH
;;;;

(define-testcase testsuite-string-match ()
  (assert-t (kaputt::string-match "" ""))
  (assert-t (kaputt::string-match "a" "a"))
  (assert-t (kaputt::string-match "a*" "a"))
  (assert-t (kaputt::string-match "a*" "ab"))
  (assert-t (kaputt::string-match "a*" "abc"))
  (assert-t (kaputt::string-match "a?" "ab"))
  (assert-t (kaputt::string-match "a*a" "aba"))
  (assert-t (kaputt::string-match "a*a" "abca"))
  (assert-t (kaputt::string-match "a?a" "aba"))
  (assert-nil (kaputt::string-match "*a" "b")))

;;;; End of file `utilities.lisp'
