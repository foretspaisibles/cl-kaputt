;;;; kaputt.asd — A Simple Interactive Test Framework for Common Lisp

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

(asdf:defsystem #:kaputt
  :description "A Simple Interactive Test Framework for Common Lisp"
  :homepage "https://github.com/michipili/cl-kaputt"
  :author "Michaël Le Barbier"
  :license "CeCILL-B"
  :depends-on (#:alexandria)
  :components
  ((:module "src"
    :components ((:file "package")
		 (:file "utilities")
		 (:file "configuration")
                 (:file "assert")
		 (:file "result")
		 (:file "supervisor")
		 (:file "testcase")))))

(asdf:defsystem #:kaputt/testsuite
  :description "A testsuite for the Kaputt Test Framework"
  :author "Michaël Le Barbier"
  :license "CeCILL-B"
  :depends-on (#:kaputt)
  :components
  ((:module "testsuite"
    :components ((:file "package")
		 (:file "utilities")
		 (:file "assert")
		 (:file "testcase")
		 (:file "entrypoint")))))

(asdf:defsystem #:kaputt/example
  :description "An Example for the Kaputt Test Framework"
  :author "Michaël Le Barbier"
  :license "CeCILL-B"
  :depends-on (#:kaputt)
  :components
  ((:module "example"
    :components ((:file "example")))))

;;;; End of file `kaputt.asd'
