;;;; result.lisp — Test Result

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

;;;; End of file `result.lisp'
