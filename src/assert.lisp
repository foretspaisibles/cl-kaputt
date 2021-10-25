;;;; assert.lisp — Defining assertions

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
;;;; ASSERTION-FAILED Condition
;;;;

(define-condition assertion-failed (serious-condition)
  ((assertion-description :accessor assertion-description :initarg :assertion-description)
   (assertion-form :accessor assertion-form :initarg :assertion-form)
   (assertion-args :accessor assertion-args :initarg :assertion-args))
  (:report
   (lambda (condition stream)
     (format stream "Test assertion failed:~%~%")
     (when (assertion-form condition)
       (format stream "  ~S~%~%" (assertion-form condition)))
     (when (assertion-args condition)
       (let ((*print-circle* nil)
	     (print-flag (position-if (lambda (form) (not (atom form)))
				      (rest (assertion-form condition)))))
	 (when print-flag
	   (format stream "~&In this call, the composed forms in argument position evaluate as:~%~%"))
	 (mapcar (lambda (form value)
		   (unless (atom form)
		     (format stream "~&  ~S => ~S" form value)))
		 (rest (assertion-form condition))
		 (assertion-args condition))
	 (when print-flag (format stream "~%~%"))))
     (write-string (assertion-description condition) stream))))


;;;;
;;;; DEFINE-ASSERTION Macro
;;;;

(defvar *testcase-assertion-form* nil
  "The form used to call the current assertion in a test case.")

(defvar *testcase-assertion-args* nil
  "The list of argument values used to call the current assertion in a test case.")

(defmacro define-assertion (assertion-name assertion-args &body body)
  "Define an assertion function ASSERTION-NAME, accepting ASSERTION-ARGS with BODY.
The BODY is interpreted as computing a generalised boolean value,
which triggers an ASSERTION-FAILED when this boolean is NIL.

The ASSERTION-NAME must start with ASSERT-, ASSERT<, ASSERT> or ASSERT=.

When the first BODY form is a string, this string is used as an informational message
when reporting the corresponding error condition. It is also used as a documentaion
string for the created function.

When the next BODY form starts with the keyword :REPORT, then the rest of that form
must be function of a STREAM.  This function is then used to generate an informational
message when reporting the corresponding error condition. This allows to add dynamic
context to this informational messsage, like the value of some variables."
  (let (description docstring report actual-body)
    (labels
        ((read-docstring (body)
           (if (and (>= (length body) 1) (stringp (first body)))
               (progn
                 (setf docstring (first body))
                 (rest body))
               body))
         (read-report (body)
           (if (and (>= (length body) 1) (listp (first body)) (eq :report (first (first body))))
               (progn
                 (setf report (second (first body)))
                 (rest body))
               body)))
      (setf actual-body
            (reduce (lambda (accumulator f) (funcall f accumulator))
                    (list #'read-docstring #'read-report)
                    :initial-value body))
      (unless docstring
        (setf docstring
              (with-output-to-string (assertion-documentation)
                (format assertion-documentation
                        "The assertion ~A is defined by the body forms~%~%"
                        (symbol-name assertion-name))
                (dolist (form body)
                  (format assertion-documentation "~&  ~S" form))
                (format assertion-documentation "~&~%It has no specific documentation.")))))
    (setf description
          `(with-output-to-string (assertion-description)
             (when ,docstring
               (format assertion-description "~A" ,docstring))
             (when ,report
               (funcall ,report assertion-description))))
    `(defun ,assertion-name ,assertion-args
       ,docstring
       (restart-case
           (if (progn ,@actual-body)
               t
               (error 'assertion-failed
                      :assertion-description ,description
                      :assertion-form *testcase-assertion-form*
                      :assertion-args *testcase-assertion-args*))
       (continue ()
         :report
         (lambda (stream)
           (format stream "~@<Record a failure for ~A and continue testing.~@:>"
                   ,(symbol-name assertion-name)))
         (values nil))
       (ignore ()
         :report
         (lambda (stream)
           (format stream "~@<Record a success for ~A and continue testing.~@:>"
                   ,(symbol-name assertion-name)))
         (values t))))))


;;;;
;;;; Basic Assertions
;;;;

(define-assertion assert-t (expr)
  "The assertion (ASSERT-T EXPR) is true, iff EXPR is a true generalised boolean."
  expr)

(define-assertion assert-nil (expr)
  "The assertion (ASSERT-NIL EXPR) is true, iff EXPR is NIL."
  (eq nil expr))

(define-assertion assert-type (expr type)
  "The assertion (ASSERT-TYPE EXPR TYPE) is true, iff EXPR evaluates to a value of type TYPE."
  (typep expr type))

(define-assertion assert-eq (a b)
  "The assertion (ASSERT-EQ A B) is true, iff A and B satisfy the EQ predicate."
  (eq a b))

(define-assertion assert-eql (a b)
  "The assertion (ASSERT-EQL A B) is true, iff A and B satisfy the EQL predicate."
  (eql a b))

(define-assertion assert-equal (a b)
  "The assertion (ASSERT-EQUAL A B) is true, iff A and B satisfy the EQUAL predicate."
  (equal a b))

(define-assertion assert= (a b)
  "The assertion (ASSERT= A B) is true, iff A and B satisfy the = predicate."
  (= a b))

(define-assertion assert< (a b)
  "The assertion (ASSERT< A B) is true, iff A and B satisfy the < predicate."
  (< a b))

(define-assertion assert> (a b)
  "The assertion (ASSERT> A B) is true, iff A and B satisfy the > predicate."
  (> a b))

(define-assertion assert<= (a b)
  "The assertion (ASSERT<= A B) is true, iff A and B satisfy the <= predicate."
  (<= a b))

(define-assertion assert>= (a b)
  "The assertion (ASSERT>= A B) is true, iff A and B satisfy the >= predicate."
  (>= a b))


;;;;
;;;; String Assertions
;;;;

(defun report-string-comparison/unexpected-type (stream label object)
  (format stream "~&The parameter ~A is expected to have type STRING but actually has type ~A."
          label (type-of object)))

(defun report-string-comparison/details (stream comparison string1 string2)
  (declare (string string1 string2))
  (let ((length1
          (length string1))
        (length2
          (length string2))
        (first-difference
          (loop for i upto (1- (min (length string1) (length string2)))
                when (not (funcall comparison (char string1 i) (char string2 i)))
                return i
                finally (return i)))
        (predicate-description
          (ecase comparison
            (char=
             "equal to")
            (char-equal
             "equal to up to case difference")
            (char<
             "less than")
            (char<=
             "less than or equal to")
            (char>
             "greater than")
            (char>=
             "greater than or equal to"))))
    (cond
      ((= length1 length2 0)
       (format stream "~&Both strings STRING1 and STRING2 are empty."))
      ((= length1 0)
        (format stream "~&The string STRING1 is empty."))
      ((= length2 0)
       (format stream "~&The string STRING2 is empty."))
      ((= length1 length2 first-difference)
       (format stream
               "~&Every character of STRING1 is ~A the character of STRING2 at
the same index. Furthermore STRING1 and STRING2 have the same length ~A."
               predicate-description length1))
      ((and (< first-difference length1) (< first-difference length2))
       (format stream
               "~&Every character of STRING1 is ~A the character of STRING2 at
the same index upto index ~A. However this condition does not hold for characters
at position ~A, which are ~S and ~S."
               predicate-description first-difference first-difference
               (char string1 first-difference) (char string2 first-difference)))
      ((< first-difference length1)
       (format stream
               "~&Every character of STRING1 is ~A the character of STRING2 at
the same index. However STRING1 is longer than STRING2, these two strings
have length ~A and ~A respectively." 
               predicate-description length1 length2))
      ((< first-difference length2)
       (format stream
                "~&Every character of STRING1 is ~A the character of STRING2 at
the same index. However STRING1 is shorter than STRING2, these two strings
have length ~A and ~A respectively." 
                predicate-description length1 length2)))))

(defun report-string-comparison (comparison string1 string2)
  (lambda (stream)
    (unless (typep string1 'string)
      (report-string-comparison/unexpected-type stream 'string1 string1))
    (unless (typep string2 'string)
      (report-string-comparison/unexpected-type stream 'string2 string2))
    (when (and (typep string1 'string) (typep string2 'string))
      (report-string-comparison/details stream comparison string1 string2))))

(define-assertion assert-string-equal (string1 string2 &key (start1 0) end1 (start2 0) end2)
  "The assertion (ASSERT-STRING-EQUAL STRING1 STRING2) is true, iff STRING1 and STRING2
satisfy the STRING-EQUAL predicate.
This assertion supports the same keyword parameters as STRING-EQUAL."
  (:report (report-string-comparison 'char-equal string1 string2))
  (string-equal string1 string2 :start1 start1 :end1 end1 :start2 start2 :end2 end2))

(define-assertion assert-string= (string1 string2 &key (start1 0) end1 (start2 0) end2)
  "The assertion (ASSERT-STRING= STRING1 STRING2) is true, iff STRING1 and STRING2
satisfy the STRING= predicate.
This assertion supports the same keyword parameters as STRING=."
  (:report (report-string-comparison 'char= string1 string2))
  (string= string1 string2 :start1 start1 :end1 end1 :start2 start2 :end2 end2))

(define-assertion assert-string< (string1 string2 &key (start1 0) end1 (start2 0) end2)
  "The assertion (ASSERT-STRING< STRING1 STRING2) is true, iff STRING1 and STRING2
satisfy the STRING< predicate.
This assertion supports the same keyword parameters as STRING<."
  (:report (report-string-comparison 'char< string1 string2))
  (string< string1 string2 :start1 start1 :end1 end1 :start2 start2 :end2 end2))

(define-assertion assert-string> (string1 string2 &key (start1 0) end1 (start2 0) end2)
  "The assertion (ASSERT-STRING> STRING1 STRING2) is true, iff STRING1 and STRING2
satisfy the STRING> predicate.
This assertion supports the same keyword parameters as STRING>."
  (:report (report-string-comparison 'char> string1 string2))
  (string> string1 string2 :start1 start1 :end1 end1 :start2 start2 :end2 end2))

(define-assertion assert-string<= (string1 string2 &key (start1 0) end1 (start2 0) end2)
  "The assertion (ASSERT-STRING<= STRING1 STRING2) is true, iff STRING1 and STRING2
satisfy the STRING<= predicate.
This assertion supports the same keyword parameters as STRING<=."
  (:report (report-string-comparison 'char<= string1 string2))
  (string<= string1 string2 :start1 start1 :end1 end1 :start2 start2 :end2 end2))

(define-assertion assert-string>= (string1 string2 &key (start1 0) end1 (start2 0) end2)
  "The assertion (ASSERT-STRING>= STRING1 STRING2) is true, iff STRING1 and STRING2
satisfy the STRING>= predicate.
This assertion supports the same keyword parameters as STRING>=."
  (:report (report-string-comparison 'char>= string1 string2))
  (string>= string1 string2 :start1 start1 :end1 end1 :start2 start2 :end2 end2))


;;;;
;;;; List as Set Assertions
;;;;

(define-assertion assert-subsetp (list1 list2 &key key (test #'eql))
  "The assertion (ASSERT-SUBSETP LIST1 LIST2) is true iff LIST1 is a subset of LIST2."
  (:report
   (lambda (stream)
     (format stream "~%~%The items with keys:~%~%  ~{~S~^ ~}~&~%are in LIST1 but not in LIST2."
             (set-difference list1 list2 :key key :test test))))
  (subsetp list1 list2 :key key :test test))

(define-assertion assert-set-equal (list1 list2 &key key (test #'eql))
  "The assertion (ASSERT-SET-EQUAL LIST1 LIST2) is true iff LIST1 denotes the same set as LIST2."
  (:report
   (lambda (stream)
     (unless (subsetp list1 list2 :key key :test test)
       (format stream "~%~%The items with keys:~%~%  ~{~S~^ ~}~&~%are in LIST1 but not in LIST2."
               (set-difference list1 list2 :key key :test test)))
     (unless (subsetp list2 list1 :key key :test test)
       (format stream "~%~%The items with keys:~%~%  ~{~S~^ ~}~&~%are in LIST2 but not in LIST1."
               (set-difference list2 list1 :key key :test test)))))
  (and (subsetp list1 list2 :key key :test test)
       (subsetp list2 list1 :key key :test test)))


;;;;
;;;; Vector Assertions
;;;;

(define-assertion assert-vector-equal (vector1 vector2 &key (test #'eql))
"The assertion (ASSERT-VECTOR-EQUAL VECTOR1 VECTOR2) is true iff VECTOR1 and VECTOR2 are equal.
Vectors are equal if they have the same length and have equal elements at each
index. The equality predicate used to compare elements is either EQL or the predicate
provided by the :TEST keyword argument."
  (:report
   (lambda (stream)
     (if (not (= (length vector1) (length vector2)))
         (format stream
                 "~%~%The VECTOR1 and VECTOR2 have length ~A and ~A respectively."
                 (length vector1) (length vector2))
         (loop :with difference-seen = nil
               :for item1 :across vector1
               :for item2 :across vector2
               :for index = 0 :then (1+ index)
               :unless (funcall test item1 item2)
               :do (progn
                     (unless difference-seen
                       (format stream "~%~%")
                       (setf difference-seen t))
                     (format stream
			     "~&  VECTOR1 and VECTOR2 differ at index ~A where they feature items ~S and ~S."
			     index (aref vector1 index) (aref vector2 index)))))))
  (loop :for answer = (= (length vector1) (length vector2)) :then (funcall test item1 item2)
        :for item1 :across vector1
        :for item2 :across vector2
        :while answer
        :finally (return answer)))


;;;;
;;;; Float Assertions
;;;;

(defparameter *double-float-precision* (float-precision 1.0d0))

(defmacro float-comparison-threshold (binop k u v)
  `(let (_ eu ev)
     (setf (values _ eu _) (decode-float ,u)
           (values _ ev _) (decode-float ,v))
     (scale-float 1.0d0 (+ (- ,k *double-float-precision* 2) (,binop eu ev)))))

(define-assertion assert-float-is-definitely-less-than (float1 float2 &optional (inaccuracy 0))
  "The assertion (ASSERT-FLOAT-IS-DEFINITELY-LESS-THAN FLOAT1 FLOAT2) is true iff FLOAT2
is greater than FLOAT1 and not in a neighbourhood of FLOAT1 whose diameter is controlled
by the INACCURACY, the magnitude orders of FLOAT1 and FLOAT2 and the floating point precision."
  (:report
   (lambda (stream)
     (format stream
"~%~%The neighbourhood used to compare definitive ordering of

  ~A  and  ~A

with an inaccuracy of ~A has size ~A."
            float1 float2 inaccuracy
            (float-comparison-threshold max inaccuracy float1 float2))))
  (> (- float2 float1) (float-comparison-threshold max inaccuracy float1 float2)))


(define-assertion assert-float-is-definitely-greater-than (float1 float2 &optional (inaccuracy 0))
"The assertion (ASSERT-FLOAT-IS-DEFINITELY-GREATER-THAN FLOAT1 FLOAT2) is true iff FLOAT1
is greater than FLOAT2 and not in a neighbourhood of FLOAT2 whose diameter is controlled
by the INACCURACY, the magnitude orders of FLOAT1 and FLOAT2 and the floating point precision."
  (:report
   (lambda (stream)
     (format stream
"~%~%The neighbourhood used to compare definitive ordering of

  ~A  and  ~A

with an inaccuracy of ~A has size ~A."
            float1 float2 inaccuracy
            (float-comparison-threshold max inaccuracy float2 float1))))
  (> (- float1 float2) (float-comparison-threshold max inaccuracy float2 float1)))

(define-assertion assert-float-is-approximately-equal (float1 float2 &optional (inaccuracy 0))
  "The assertion (ASSERT-FLOAT-IS-APPROXIMATELY-EQUAL FLOAT1 FLOAT2) is true iff FLOAT1 and FLOAT2
are in a neighbourhood whose size is based on the magnitude orders of FLOAT1 and FLOAT2 and the
floating point precision."
  (:report
   (lambda (stream)
     (format stream
"~%~%The neighbourhood used to compare approximate equality of

  ~A  and  ~A

with an inaccuracy of ~A has size ~A."
            float1 float2 inaccuracy
            (float-comparison-threshold max inaccuracy float1 float2))))
  (<= (abs (- float1 float2)) (float-comparison-threshold max inaccuracy float1 float2)))

(define-assertion assert-float-is-essentially-equal (float1 float2 &optional (inaccuracy 0))
  "The assertion (ASSERT-FLOAT-IS-ESSENTIALLY-EQUAL FLOAT1 FLOAT2) is true iff FLOAT1 and FLOAT2
are in a neighbourhood whose size is based on the magnitude orders of FLOAT1 and FLOAT2 and the
floating point precision."
  (:report
   (lambda (stream)
     (format stream
"~%~%The neighbourhood used to compare essential equality of

  ~A  and  ~A

with an inaccuracy of ~A has size ~A."
            float1 float2 inaccuracy
            (float-comparison-threshold max inaccuracy float1 float2))))
  (<= (abs (- float1 float2)) (float-comparison-threshold min inaccuracy float1 float2)))

;;;; End of file `assert.lisp'
