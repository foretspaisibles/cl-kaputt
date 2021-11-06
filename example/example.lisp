;;;; example.lisp — A Kaputt Example

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

(defpackage #:kaputt/example
  (:use #:common-lisp #:kaputt)
  (:export
   #:run-all-tests-batch
   #:run-all-tests))

(in-package #:kaputt/example)

(define-testcase cl-strings/string-downcase-turns-nil-into-a-string ()
  "Assert that CL:STRING-DOWNCASE maps NIL to \"nil\"."
  (assert-string= "nil" (string-downcase nil)))

(define-testcase cl-strings/string-upcase-turns-nil-into-a-string ()
  "Assert that CL:STRING-UPCASE maps NIL to \"NIL\"."
  (assert-string= "NIL" (string-upcase nil)))

(define-testcase run-all-tests ()
  (cl-strings/string-downcase-turns-nil-into-a-string)
  (cl-strings/string-upcase-turns-nil-into-a-string))

(defun run-all-tests-batch ()
  (if (run-all-tests)
      (uiop:quit 0)
      (uiop:quit 1)))

;;;; End of file `example.lisp'
