;;;; kaputt.lisp — A Simple Interactive Test Framework for Common Lisp

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
;;;; Test Run Protocol
;;;;

(defparameter *current-protocol* nil
  "The current protocol.")

(defclass protocol nil
  ((success-p :initform t)
   (batch-p :initform *batch-mode*))
  (:documentation "This class models test suite execution protocols.
When running a test suite, the progress and results of the execution
are recorded in a PROTOCOL instance.

Descendants of this class provide more functionalities."))

(defgeneric protocol-testsuite-begin (protocol)
  (:method-combination progn)
  (:documentation
   "This event is sent to a protocol, when a test suite starts.
This is the appropriate place to prepare output files, register
starting time and so on.")
  (:method progn ((p null)))
  (:method progn ((p protocol))))

(defgeneric protocol-testsuite-end (protocol)
  (:method-combination progn)
  (:documentation
   "This event is sent to a protocol when a test suite ends.
This is the appopriate place to close output files, display
aggregated reports.")
  (:method progn ((p null)))
  (:method progn ((p protocol))))

(defgeneric protocol-testcase-begin (protocol testcase)
  (:method-combination progn)
  (:documentation
   "This event is sent to a protocol when a test case begins.")
  (:method progn ((p null) testcase))
  (:method progn ((p protocol) testcase)))

(defgeneric protocol-testcase-end (protocol testcase outcome)
  (:method-combination progn)
  (:documentation
   "This event is sent to a protocol when a test case ends.")
  (:method progn ((p null) testcase outcome))
  (:method progn ((p protocol) testcase outcome)))

(defgeneric protocol-assertion-begin (protocol assertion arguments)
  (:method-combination progn)
  (:documentation
   "This event is sent to a protocol when an assertion begins.")
  (:method progn ((p null) assertion arguments))
  (:method progn ((p protocol) assertion arguments)))

(defgeneric protocol-assertion-end (protocol assertion outcome)
  (:method-combination progn)
  (:documentation
   "This event is sent to a protocol when an assertion ends.")
  (:method progn ((p null) assertion outcome))
  (:method progn ((p protocol) assertion outcome)
    (setf (slot-value p 'success-p)
          (and (slot-value p 'success-p)
               (not (eq :failure outcome))))))

(defgeneric protocol-success-p (protocol)
  (:documentation
   "This method tells if a test suite has never recorded a failure.")
  (:method ((p null)) nil)
  (:method ((p protocol)) (slot-value p 'success-p)))

(defgeneric protocol-batch-p (protocol)
  (:documentation
   "This method tells if a test suite is in batch mode.")
  (:method ((p null)) nil)
  (:method ((p protocol))(slot-value p 'batch-p)))

(defgeneric protocol-enable-batch-mode (protocol)
  (:documentation
   "This method enables batch mode for a test suite.")
  (:method ((p null)) nil)
  (:method ((p protocol))(setf (slot-value p 'batch-p) t)))


;;;;
;;;; Verbose Protocol
;;;;

(defclass protocol-verbose (protocol)
  ((stream-output :initform *standard-output*))
  (:documentation
   "A verbose protocol owns a STREAM-OUTPUT."))


;;;;
;;;; Trace Protocol
;;;;

(defclass protocol-trace (protocol-verbose) nil
  (:documentation
   "A trace protocol reports each event sent to it."))

(defmethod protocol-testcase-begin progn ((p protocol-trace) testcase)
  (format (slot-value p 'stream-output)
	  "~&~@<PROTOCOL-TESTCASE-BEGIN ~S~@:>" testcase))

(defmethod protocol-testcase-end progn ((p protocol-trace) testcase outcome)
  (format (slot-value p 'stream-output)
	  "~&~@<PROTOCOL-TESTCASE-END ~S ~S~@:>" testcase outcome))

(defmethod protocol-assertion-begin progn ((p protocol-trace) assertion arguments)
  (format (slot-value p 'stream-output)
	  "~&~@<PROTOCOL-ASSERTION-BEGIN ~S ~{ ~S ~}~@:>" assertion arguments))

(defmethod protocol-assertion-end progn ((p protocol-trace) assertion outcome)
  (format (slot-value p 'stream-output)
	  "~&~@<PROTOCOL-ASSERTION-END ~S ~S~@:>" assertion outcome))


;;;;
;;;; Count Protocol
;;;;

(defclass protocol-count (protocol)
  ((testcase-count :initform 0)
   (assertion-count :initform 0)
   (success-count :initform 0)
   (failure-count :initform 0))
  (:documentation
   "A count protocol counts TESTCASE, ASSERTION, SUCCESS and FAILURE."))

(defmethod protocol-testcase-begin progn ((p protocol-count) testcase)
  (declare (ignore testcase))
  (incf (slot-value p 'testcase-count)))

(defmethod protocol-assertion-begin progn ((p protocol-count) assertion arguments)
  (declare (ignore assertion arguments))
  (incf (slot-value p 'assertion-count)))

(defmethod protocol-assertion-end progn ((p protocol-count) assertion outcome)
  (declare (ignore assertion))
  (case outcome
    (:success
     (incf (slot-value p 'success-count)))
    (:failure
     (incf (slot-value p 'failure-count)))))


;;;;
;;;; Protocol Records
;;;;

(defclass protocol-record (protocol-verbose)
  ((current-testcase :initform nil)
   (current-assertion :initform nil)
   (failure-list :initform nil))
  (:documentation
   "A protocol record keeps track of all failures encountered in a test suite
and prints a detailed list of the failures when the test suite finishes."))

(defmethod protocol-testcase-begin progn ((p protocol-record) testcase)
  (setf (slot-value p 'current-testcase) testcase))

(defmethod protocol-assertion-begin progn ((p protocol-record) assertion arguments)
  (setf (slot-value p 'current-assertion) (cons assertion arguments)))

(defmethod protocol-assertion-end progn ((p protocol-record) assertion outcome)
  (with-slots (current-testcase current-assertion failure-list) p
    (when (eq outcome :failure)
      (push (list current-testcase current-assertion) failure-list))))

(defmethod protocol-testsuite-end progn ((p protocol-record))
  (with-slots (stream-output failure-list) p
    (when failure-list
      (format stream-output "~&~%List of failed assertions:")
      (loop :with last-testcase = nil
            :for failure :in failure-list
            :do
            (progn
              (unless (eq last-testcase (first failure))
                (format stream-output "~& Testcase ~A:" (symbol-name (first failure)))
                (setf last-testcase (first failure)))
              (format stream-output "~&    ~S" (second failure)))))))


;;;;
;;;; Dotta Protocol
;;;;

(defclass protocol-dotta (protocol-count protocol-record)
  ((width :initform 70))
  (:documentation
   "A dotta protocol reports assertion progress with dots and capital letter E,
for success and errors respectively. At the end of a testsuite, it prints basic
counts describing the current testsuite and a detailed failure report."))

(defmethod protocol-assertion-end progn ((p protocol-dotta) assertion outcome)
  (with-slots (assertion-count width stream-output) p
    (write-char (if (eq outcome :failure) #\E #\.) stream-output)
    (if (= 0 (rem assertion-count width))
        (write-char #\Newline stream-output))))

(defmethod protocol-testsuite-end progn ((p protocol-dotta))
  (labels
      ((ratio (a n)
         (if (> n 0)
             (round (* 100 (/ a n)))
             100)))
    (with-slots (stream-output assertion-count success-count failure-count testcase-count) p
      (format (slot-value p 'stream-output)
"~&~%Test suite ran ~D assertions split across ~D test cases.
 Success: ~D/~D (~D%)
 Failure: ~D/~D (~D%)~%~%"
          assertion-count testcase-count
          success-count assertion-count (ratio success-count assertion-count)
          failure-count assertion-count (ratio failure-count assertion-count)))))


;;;;
;;;; Test Cases
;;;;

(defparameter *testcase-protocol-class* 'protocol-dotta
  "The protocol class to use when running test suites.")

(defun define-testcase/wrap-assert-form (body-forms)
  "Walks through BODY-FORMS and wrap assertion forms in a RESTART-CASE."
  (labels
      ((is-funcall-p (form)
         (when (and (listp form) (not (null form)) (symbolp (first form)) (listp (cdr form)))
           (case (first form)
             ((funcall apply)
              (second form))
             (t (first form)))))
       (is-assert-name-p (symbol)
         (and (>= (length (symbol-name symbol)) 7)
              (and (string= (symbol-name symbol) "ASSERT" :end1 6)
                   (position (char (symbol-name symbol) 6) "-=<>"))))
       (is-assert-form-p (form)
         (is-assert-name-p (is-funcall-p form)))
       (wrap-assert-form (form)
         (cond
           ((is-assert-form-p form)
            `(let ((run-assertion-result t))
               (protocol-assertion-begin *current-protocol* ',(first form) ',(rest form))
               (loop :with run-assertion-p = t
                     :while run-assertion-p
                     :do
                   (let ((*testcase-assertion-form* (quote ,form))
                         (*testcase-assertion-args* ,(cons 'list (rest form))))
                     (restart-case
                         (setf run-assertion-result
                               (apply (function ,(first form)) *testcase-assertion-args*)
                               run-assertion-p nil)
                       (retry ()
                         :report
                         (lambda (stream)
                           (format stream "~@<Retry ~A.~@:>" ,(symbol-name (first form))))))))
               (protocol-assertion-end *current-protocol*
                                       ',(first form)
                                       (if run-assertion-result :success :failure))
               (values run-assertion-result)))
           ((is-funcall-p form)
            (cons (first form) (mapcar #'wrap-assert-form (rest form))))
           (t form))))
    (mapcar #'wrap-assert-form body-forms)))

(defmacro define-testcase (testcase-name testcase-args &body body)
    "Define a test case function TESTCASE-NAME, accepting TESTCASE-ARGS with BODY.

The BODY is examined and assertions spotted in it are wrapped with extra code
installing restarts and triggering protocol events.

Test cases are allowed to be nested.  A toplevel test case is a test suite and triggers testsuite
protocol events when beginning and ending.  The return value of a testcase is a boolean which
is true iff the current testsuite has experienced a failed assertion.  Thus, even if a test case
does not experience any failure, a NIL value is returned if a previous test case in the current
test suite has experienced a failure."
  `(defun ,testcase-name ,testcase-args
     (let ((testsuite-p (eq *current-protocol* nil))
           (*current-protocol* (or *current-protocol* (make-instance *testcase-protocol-class*))))
       (when testsuite-p
         (protocol-testsuite-begin *current-protocol*))
       (protocol-testcase-begin *current-protocol* ',testcase-name)
       (restart-case
           ,(define-testcase/wrap-assert-form
                (cons 'progn body))
         (skip ()
           :report
           (lambda (stream)
             (format stream
		     "~@<Skip the rest of test case ~A and continue testing.~@:>"
		     ,(symbol-name testcase-name)))
           (values)))
       (protocol-testcase-end *current-protocol* ',testcase-name :unknown)
       (when testsuite-p
         (protocol-testsuite-end *current-protocol*))
       (values (protocol-success-p *current-protocol*)))))

;;;; End of file `kaputt.lisp'
