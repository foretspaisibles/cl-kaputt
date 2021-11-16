;;;; testcase.lisp — A Simple Interactive Test Framework for Common Lisp

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

(defparameter *testcase-path* nil
  "The current path in the testcase hierarchy.
When running in a testcase, the first elements of *testcase-path* designate this
specific testcase.")

(defparameter *testcase-skip* nil
  "Flag governing skipping assertions in the current testcase.")

(defparameter *testcase-results* nil
  "The accumulated results of the current testcase.
This is NIL when not running in a testcase.")

(defparameter *testcase-interaction*
  #+swank
  :retry
  #-swank
  nil
  "The interaction mode for running testcases.
Accepted values are NIL, :RETRY, :CONTINUE, :IGNORE or :SKIP.

When the SWANK feature is active, we assume interactive use
of the system and initialise the interaction mode to :RETRY.
It is set to NIL otherwise.")

(defparameter *testcase-supervisor-class* 'serviceable-supervisor
  "The supervisor class to use when running test suites.")

(defmacro testcase-batch (&body forms)
  "Run FORMS in an environment where testcase are batched."
  `(let ((*testcase-interaction* :batch))
     ,@forms))

(defmacro testcase-ignore (&body forms)
  "Run FORMS in an environment where testcase failures are ignored."
  `(let ((*testcase-interaction* :ignore))
     ,@forms))

(defmacro testcase-continue (&body forms)
  "Run FORMS in an environment where testcase failures are continued."
  `(let ((*testcase-interaction* :continue))
     ,@forms))

(defstruct assertion
  "A strucure capturing an assertion result.
Slots are populated as follows:

 NAME:
  The symbol designating the assertion. This is the first element of the FORM.

 PATH:
  The path of the assertion in the test hierarchy. This the stack of preceding testcases in
  the test hierarchy.

 ARGUMENTS:
  The list of evaluated arguments for the assertion.

 FORM:
  The form for the assertion invocation.

 TYPE:
  One of the keywords :FUNCTION or :MACRO.

 OUTCOME:
  One of the keywords :SUCCESS, :FAILURE or :CONDITION.

 DESCRIPTION:
  When the OUTCOME is :SUCCESS, this is NIL. When the OUTCOME is :FAILURE, the failure
  description provided by the assertion as a second value.  When the OUTCOME is :CONDITION,
  the description of the condition.

 CONDITION:
  When the OUTCOME is :CONDITION, the condition."
  name path arguments form type outcome description condition)

(defstruct testcase
  "A structure capturing a testcase result.
Slots are populated as follows:

 NAME:
  The symbol designating the testcase.

 PATH:
  The path of the testcase in the test hierarchy. This the stack of preceding testcases in
  the test hierarchy.

 ARGUMENTS:
  The list of evaluated arguments for the testcase.

 TOTAL:
  The total number of assertions in the testcase and its descendants.

 SUCCESS:
  The total number of assertions that yielded a :SUCCESS in the testcase and its descendants.

 FAILURE:
  The total number of assertions that yielded a :FAILURE in the testcase and its descendants.

 CONDITION:
  The total number of assertions that yielded a :CONDITION in the testcase and its descendants.

 SKIP:
  The total number of assertions that yielded a :SKIP in the testcase and its descendants.

 RESULTS:
  The list of testcase results and assertions results yielded by descendants."
  name path arguments total success failure condition skip results)

(defun count-total-number-of-assertions (results)
  "Count the total number of assertions in RESULTS."
  (loop for object in results
	sum (cond
	      ((assertion-p object)
	       1)
	      ((testcase-p object)
	       (testcase-total object))
	      (t
	       (error "Cannot count number of assertions in ~S." object)))))

(defun count-total-number-of-assertions-by-outcome (results outcome)
  "Count the total number of assertions in RESULTS that yielded a OUTCOME."
  (loop for object in results
	sum (cond
	      ((assertion-p object)
	       (if (eq (assertion-outcome object) outcome) 1 0))
	      ((testcase-p object)
	       (ecase outcome
		 (:success
		  (testcase-success object))
		 (:failure
		  (testcase-failure object))
		 (:condition
		  (testcase-condition object))
		 (:skip
		  (testcase-skip object))))
	      (t
	       (error "Cannot count number of assertions in ~S." object)))))


;;;;
;;;; ASSERTION-FAILED Condition
;;;;

(defun assertion-failed/report (details-designator stream)
  (labels
      ((format-header (details)
	 (format stream "Test assertion failed:~%~%  ~S~%~%" (assertion-form details)))
       (format-path (details)
	 (when (assertion-path details)
	   (format stream "Test assertion context:~%~%")
	   (loop for path-element in (reverse (assertion-path details))
		 for level from 1
		 do (dotimes (_ (* 2 level)) (write-char #\Space stream))
		 do (format stream "~S~%" path-element))
	   (write-char #\Newline stream)))
       (argument-forms (details)
	 (rest (assertion-form details)))
       (argument-values (details)
	 (assertion-arguments details))
       (composed-argument-p (form)
	 (not (atom form)))
       (format-arguments-1 (form value)
 	 (when (composed-argument-p form)
	   (format stream "~&  ~S => ~S" form value)))
       (format-arguments (details)
	 (when (and (argument-values details)
		    (position-if #'composed-argument-p (argument-forms details)))
	   (format stream "~&In this call, the composed forms in argument position evaluate as:~%~%")
	   (mapcar #'format-arguments-1
		   (argument-forms details)
		   (argument-values details))
	   (format stream "~%~%")))
       (format-description (details)
	 (when (assertion-description details)
	   (write-string (assertion-description details) stream)
	   (format stream "~%~%"))))
    (let ((*print-circle*
	    nil)
	  (details
	    (cond
	      ((assertion-p details-designator)
	       details-designator)
	      (t
	       (slot-value details-designator 'details)))))
      (format-header details)
      (format-arguments details)
      (format-description details)
      (format-path details))))

(define-condition assertion-failed (serious-condition)
  ((details :initarg :details))
  (:report assertion-failed/report))


;;;;
;;;; ASSERT-FAILURE
;;;;

(defun assert-failure/unexpected-condition (form unexpected-condition)
  "Handle unexpected conditions in ASSERT-FAILURE."
  (make-assertion :name 'assert-failure :path *testcase-path*
		  :arguments nil :form form :type :macro
		  :outcome :condition
		  :description
		  (description "The form was expected to evaluate to NIL but instead "
			       "it signalled a condition ~S.~%~%  ~S~%~%"
			       (type-of unexpected-condition)
			       unexpected-condition)
		  :condition unexpected-condition))

(defun assert-failure/main (form pattern success-p assertion-description)
  "Handle the main line for ASSERT-FAILURE."
  (cond
    ((and (assertion-p success-p) (eq (assertion-name success-p) 'assert-failure))
     success-p)
    ((assertion-p success-p) (eq (assertion-outcome success-p) :failure)
     (make-assertion :name 'assert-failure :path *testcase-path*
		     :arguments nil :form form :type :macro
		     :outcome :success :description nil :condition nil))
    ((and (eq success-p nil) pattern)
     (if (string-match pattern assertion-description)
	 (make-assertion :name 'assert-failure :path *testcase-path*
			 :arguments nil :form form :type :macro
			 :outcome :success :description nil :condition nil)
	 (make-assertion :name 'assert-failure :path *testcase-path*
			 :arguments nil :form form :type :macro
			 :outcome :failure
			 :description
			 (description "The form was expected to evaluate to "
                                      "NIL and yield a description matching "
                                      "the pattern~%~%  ~S~%~%but the description"
                                      "~%~%  ~S~%~%did not."
				      pattern assertion-description)
			 :condition nil)))
       ((eq success-p nil)
	(make-assertion :name 'assert-failure :path *testcase-path*
			:arguments nil
			:form form :type :macro
			:outcome :success :description nil :condition nil))
       (t
	(make-assertion :name 'assert-failure :path *testcase-path*
			:arguments nil
			:form form :type :macro
			:outcome :failure
			:description
			(description "The form was expected to evaluate "
				     "to NIL but instead it yielded~%~%  ~S."
				     success-p)
			:condition nil))))
  
(defmacro assert-failure (form &optional pattern)
  "An assertion that verifies that the assertion FORM yields a failure.
When a globbing PATTERN is supplied the description of the failure is
matched against it."
  (let ((assertion-form
	  (list 'assert-failure form)))
    `(multiple-value-bind (success-p description)
	 (handler-case
	     ,form
	   (t (unexpected-condition)
	     (assert-failure/unexpected-condition ',assertion-form
						  unexpected-condition)))
       (assert-failure/main ',assertion-form ,pattern success-p description))))


;;;;
;;;; DEFINE-TESTCASE
;;;;

(defmacro supervise-assertion (form)
  "Supervise the execution of the assertion FORM and return ASSERTION evaluation details."
  (let* ((assertion-name
	   (first form))
	 (assertion-type
	   (if (macro-function (first form))
	       :macro
	       :function))
	 (assertion-arguments
	   (when (eq assertion-type :function)
	     (cons 'list (rest form))))
	 (eval-form
	   (ecase assertion-type
	     (:macro
	      form)
	     (:function
	      `(apply (function ,assertion-name) assertion-arguments)))))
    `(let* ((assertion-arguments
	      nil))
       (multiple-value-bind (success-p description)
	   (handler-case
	       (progn
		 (setf assertion-arguments ,assertion-arguments)
		 ,eval-form)
	     (t (unexpected-condition)
	       (make-assertion :name ',assertion-name :path *testcase-path*
			       :arguments assertion-arguments
			       :form (quote ,form) :type ,assertion-type
			       :outcome :condition :description nil :condition unexpected-condition)))
	 (cond
	   ((assertion-p success-p)
	    success-p)
	   ((not success-p)
	    (make-assertion :name ',assertion-name :path *testcase-path*
			    :arguments assertion-arguments
			    :form (quote ,form) :type ,assertion-type
			    :outcome :failure :description description :condition nil))
	   (t
	    (make-assertion :name ',assertion-name :path *testcase-path*
			    :arguments assertion-arguments
			    :form (quote ,form) :type ,assertion-type
			    :outcome :success :description nil :condition nil)))))))

(defmacro restart-assertion (form)
  "Supervise the execution of FORM with restarts."
  (let ((assertion-name
	  (symbol-name (first form))))
    (with-unique-names (retry-p result)
      `(loop :with ,retry-p = t
	     :with ,result = nil
	     :while ,retry-p
	     :do
	     (restart-case
		 (progn
		   (setf ,result (supervise-assertion ,form))
		   (unless (eq :success (assertion-outcome ,result))
		     (error 'assertion-failed :details ,result))
		   (setf ,retry-p nil)
		   (values ,result))
	       (retry ()
		 :report
		 (lambda (stream)
		   (format stream "~@<Retry ~A.~@:>" ,assertion-name)))
	       (continue ()
		 :report
		 (lambda (stream)
		   (format stream "~@<Record a failure for ~A and continue testing.~@:>"
			   ,assertion-name))
		 (setf ,retry-p nil))
	       (ignore ()
		 :report
		 (lambda (stream)
		   (format stream "~@<Record a success for ~A and continue testing.~@:>"
			   ,assertion-name))
		 (setf ,retry-p nil
		       (assertion-outcome ,result) :success
		       (assertion-description ,result) nil
		       (assertion-condition ,result) nil))
	       (skip ()
		 :report
		 (lambda (stream)
		   (if *testcase-path*
		       (format stream
			       "~@<Skip the rest of test case ~A and continue testing.~@:>"
			       (symbol-name (first *testcase-path*)))
		       (format stream
			       "~@<Skip the rest of test case and continue testing.~@:>")))
		 (when *testcase-path*
		   (setf *testcase-skip* t))
		 (setf ,retry-p nil)))
	     :finally
	     (return ,result)))))

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
                   (position (char (symbol-name symbol) 6) "-=<>"))
	      symbol))
       (is-assert-form-p (form)
         (is-assert-name-p (is-funcall-p form)))
       (wrap-assert-form (form)
         (cond
	   ((is-assert-form-p form)
	    (with-unique-names (result)
              `(labels
		   ((assertion-handler (c)
		      (declare (ignore c))
		      (invoke-restart
		       (ecase *testcase-interaction*
			 (:skip
			  'skip)
			 ((:continue :batch nil)
			  'continue)
			 (:ignore
			  'ignore)))))
		 (let ((,result
			 (ecase *testcase-interaction*
			   ((:skip :continue :ignore)
			    (handler-bind ((assertion-failed #'assertion-handler))
			      (restart-assertion ,form)))
			   (:retry
			    (restart-assertion ,form))
			   ((:batch nil)
			    (supervise-assertion ,form)))))
		   (push ,result *testcase-results*)
		   (supervisor-assertion *current-supervisor* ,result)
		   ,result))))
           ((is-funcall-p form)
            (cons (first form) (mapcar #'wrap-assert-form (rest form))))
           (t form))))
    (mapcar #'wrap-assert-form body-forms)))

(defmacro define-testcase (testcase-name testcase-args &body body)
  "Define a test case function TESTCASE-NAME, accepting TESTCASE-ARGS with BODY.

The BODY is examined and assertions spotted in it are wrapped with extra code
installing restarts and triggering supervisor events.

Test cases are allowed to be nested.  A toplevel test case is a test suite and triggers testsuite
supervisor events when beginning and ending.  The return value of a testcase is a boolean which
is true iff the current testsuite has experienced a failed assertion.  Thus, even if a test case
does not experience any failure, a NIL value is returned if a previous test case in the current
test suite has experienced a failure."
  (with-unique-names (toplevel-p testcase-results saved-results)
    `(defun ,testcase-name ,testcase-args
       (let ((,toplevel-p
	       (eq *current-supervisor* nil))
	     (,testcase-results
	       nil)
	     (,saved-results
	       nil)
	     (*current-supervisor*
		 (or *current-supervisor*
		     (make-instance *testcase-supervisor-class*))))
	 (when ,toplevel-p
           (supervisor-toplevel-begin *current-supervisor*))
	 (supervisor-testcase-begin *current-supervisor* ',testcase-name)
 	 (let ((*testcase-results*
		 nil)
	       (*testcase-skip*
		 *testcase-skip*)
	       (*testcase-path*
		 (cons ',testcase-name *testcase-path*)))
           ,(define-testcase/wrap-assert-form (cons 'progn body))
	   (setf ,saved-results *testcase-results*))
	 (setf ,testcase-results
	       (make-testcase :name ',testcase-name
			      :path *testcase-path*
			      :arguments ,testcase-args
			      :total (count-total-number-of-assertions ,saved-results)
			      :success (count-total-number-of-assertions-by-outcome
					,saved-results
					:success)
			      :failure (count-total-number-of-assertions-by-outcome
					,saved-results
					:failure)
			      :condition (count-total-number-of-assertions-by-outcome
					  ,saved-results
					  :condition)
			      :skip (count-total-number-of-assertions-by-outcome
				     ,saved-results
				     :skip)
			      :results (nreverse ,saved-results)))
	 (unless ,toplevel-p
	   (push ,testcase-results *testcase-results*))
	 (supervisor-testcase-end *current-supervisor* ',testcase-name ,testcase-results)
	 (when ,toplevel-p
           (supervisor-toplevel-end *current-supervisor* ,testcase-results))
	 (values (supervisor-success-p *current-supervisor*) ,testcase-results)))))

;;;; End of file `testcase.lisp'
