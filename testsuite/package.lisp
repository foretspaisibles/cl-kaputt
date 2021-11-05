;;;; package.lisp — A Testsuite for the Kaputt Test Framework

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
   #:run-all-tests
   #:run-all-tests-batch)
  (:documentation
   "A testsuite for the Kaputt Test Framewok."))

;;;; End of file `package.lisp'
