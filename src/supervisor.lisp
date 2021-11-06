;;;; supervisor.lisp — A Simple Interactive Test Framework for Common Lisp

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

(in-package #:kaputt)


;;;;
;;;; Test Run Supervisor
;;;;

(defparameter *current-supervisor* nil
  "The current supervisor.")

(defclass supervisor nil
  ((success-p :initform t))
  (:documentation "This class models test suite execution supervisors.
When running a test suite, the progress and results of the execution
are recorded in a SUPERVISOR instance.

Descendants of this class provide more functionalities."))

(defgeneric supervisor-toplevel-begin (supervisor)
  (:method-combination progn)
  (:documentation
   "This event is sent to a supervisor, when a test suite starts.
This is the appropriate place to prepare output files, register
starting time and so on.")
  (:method progn ((instance null)))
  (:method progn ((instance supervisor))))

(defgeneric supervisor-toplevel-end (supervisor results)
  (:method-combination progn)
  (:documentation
   "This event is sent to a supervisor when a test suite ends.
This is the appopriate place to close output files, display
aggregated reports.")
  (:method progn ((instance null) results))
  (:method progn ((instance supervisor) results)))

(defgeneric supervisor-testcase-begin (supervisor testcase)
  (:method-combination progn)
  (:documentation
   "This event is sent to a supervisor when a test case begins.")
  (:method progn ((instance null) testcase))
  (:method progn ((instance supervisor) testcase)))

(defgeneric supervisor-testcase-end (supervisor testcase outcome)
  (:method-combination progn)
  (:documentation
   "This event is sent to a supervisor when a test case ends.")
  (:method progn ((instance null) testcase outcome))
  (:method progn ((instance supervisor) testcase outcome)))

(defgeneric supervisor-assertion (supervisor assertion)
  (:method-combination progn)
  (:documentation
   "This event is sent to a supervisor when an assertion has been performed.")
  (:method progn ((instance null) assertion))
  (:method progn ((instance supervisor) assertion)
    (with-slots (success-p) instance
      (setf success-p (and success-p (eq :success (assertion-outcome assertion)))))))

(defgeneric supervisor-success-p (supervisor)
  (:documentation
   "This method tells if a test suite has never recorded a failure.")
  (:method ((instance null))
    nil)
  (:method ((instance supervisor))
    (slot-value instance 'success-p)))


;;;;
;;;; Verbose Supervisor
;;;;

(defclass verbose-supervisor (supervisor)
  ((stream-output :initform *standard-output*))
  (:documentation
   "A verbose supervisor owns a STREAM-OUTPUT."))


;;;;
;;;; Trace Supervisor
;;;;

(defclass trace-supervisor (verbose-supervisor) nil
  (:documentation
   "A trace supervisor reports each event sent to it."))

(defmethod supervisor-testcase-begin progn ((instance trace-supervisor) testcase)
  (format (slot-value instance 'stream-output)
	  "~&~@<SUPERVISOR-TESTCASE-BEGIN ~S~@:>" testcase))

(defmethod supervisor-testcase-end progn ((instance trace-supervisor) testcase outcome)
  (format (slot-value instance 'stream-output)
	  "~&~@<SUPERVISOR-TESTCASE-END ~S ~S~@:>" testcase outcome))

(defmethod supervisor-assertion progn ((instance trace-supervisor) assertion)
  (format (slot-value instance 'stream-output)
	  "~&~@<SUPERVISOR-ASSERTION ~S ~@:>" assertion))


;;;;
;;;; Tally Supervisor
;;;;

(defclass tally-supervisor (verbose-supervisor)
  ((testcase-count :initform 0)
   (assertion-count :initform 0)
   (success-count :initform 0)
   (failure-count :initform 0)
   (condition-count :initform 0)
   (skip-count :initform 0))
  (:documentation
   "A tally supervisor counts TESTCASE and ASSERTION by their outcomes. At the end of a testsuite,
it prints basic counts describing the current testsuite and a detailed failure report."))

(defmethod supervisor-testcase-begin progn ((instance tally-supervisor) testcase)
  (declare (ignore testcase))
  (incf (slot-value instance 'testcase-count)))

(defmethod supervisor-assertion progn ((instance tally-supervisor) assertion)
  (incf (slot-value instance 'assertion-count))
  (ecase (assertion-outcome assertion)
    (:success
     (incf (slot-value instance 'success-count)))
    (:failure
     (incf (slot-value instance 'failure-count)))
    (:condition
     (incf (slot-value instance 'condition-count)))
    (:skip
     (incf (slot-value instance 'skip-count)))))

(defmethod supervisor-toplevel-end progn ((instance tally-supervisor) results)
  (declare (ignore results))
  (labels
      ((ratio (a n)
         (if (> n 0)
             (round (* 100 (/ a n)))
             100))
       (format-ratio (stream-output name numerator denominator)
	 (when (> numerator 0)
	   (format stream-output "  ~9@A: ~D/~D (~D%)~%"
		   name numerator denominator (ratio numerator denominator)))))	 
    (with-slots (stream-output testcase-count assertion-count success-count
		 failure-count condition-count skip-count)
	instance
      (format stream-output
	      "~&~%Test suite ran ~D assertions split across ~D test cases.~%"
              assertion-count testcase-count)
      (mapcar (lambda (spec) (apply #'format-ratio stream-output spec))
	      `(("Success" ,success-count ,assertion-count)
		("Failure" ,failure-count ,assertion-count)
		("Condition" ,condition-count ,assertion-count)
		("Skip" ,skip-count ,assertion-count)))
      (let ((total-number-of-failed-assertions
	      (+ failure-count condition-count)))
	(cond
	  ((= 0 total-number-of-failed-assertions)
	   (format stream-output "~%There was no error.~%"))
	  ((= 1 total-number-of-failed-assertions)
	   (format stream-output "~%There was one error.~%"))
	  (t
	   (format stream-output "~%There were some errors.~%"))))
      (format stream-output "~%"))))


;;;;
;;;; Pointless Supervisor
;;;;

(defclass pointless-supervisor (tally-supervisor)
  ((width :initform 70))
  (:documentation
   "A pointless supervisor reports assertion progress with dots and capital letter E,
for success and errors respectively. At the end of a testsuite, it prints basic
counts describing the current testsuite and a detailed failure report."))

(defmethod supervisor-assertion progn ((instance pointless-supervisor) assertion)
  (with-slots (assertion-count width stream-output) instance
    (write-char (ecase (assertion-outcome assertion)
		  (:success #\.)
		  (:failure #\E)
		  (:condition #\!)
		  (:skip #\^))
		stream-output)
    (if (= 0 (rem (1+ assertion-count) width))
        (write-char #\Newline stream-output))))


;;;;
;;;; Serviceable Supervisor
;;;;

(defclass serviceable-supervisor (tally-supervisor)
  ((width :initform 70))
  (:documentation
   "A serviceable supervisor reports assertion failures as they come. At the end of
the toplevel testsuite it displays a tally of the failures."))

(defmethod supervisor-assertion progn ((instance serviceable-supervisor) assertion)
  (with-slots (width stream-output failure-count condition-count) instance
    (let ((current-number-of-failed-assertions
	    (+ failure-count condition-count))
	  (failed-assertion-p
	    (case (assertion-outcome assertion)
	      ((:failure :condition)
	       t)
	      (t
	       nil))))
      (when failed-assertion-p
	(when (> current-number-of-failed-assertions 1)
	  (write-char #\Newline stream-output))
	(dotimes (i width)
	  (write-char #\= stream-output))
	(write-char #\Newline stream-output)
	(assertion-failed/report assertion stream-output)))))

;;;; End of file `supervisor.lisp'
