;;;; utilities.lisp — Utilities for Kaputt

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
;;;; STRING-MATCH
;;;;

(defun string-match (pattern text)
  "Predicate recognising TEXT matching a globbing PATTERN."
  (let ((text-length (length text))
        (pattern-length (length pattern)))
    (labels
        ((match-step (i j)
           (case (when (and (<= j text-length) (< i pattern-length))
		   (elt pattern i))
             ((nil)
              (eq j text-length))
             (#\?
              (and (< j text-length) (match-step (1+ i) (1+ j))))
             (#\*
	      (or (match-step (1+ i) j) (match-step i (1+ j))))
             (t
              (when (< j text-length)
                (and (char= (elt pattern i) (elt text j))
                     (match-step (1+ i) (1+ j))))))))
      (match-step 0 0))))

;;;; End of file `utilities.lisp'
