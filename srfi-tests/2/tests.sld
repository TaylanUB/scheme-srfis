;; Copyright (C) Oleg Kiselyov (1998). All Rights Reserved.

;; Made an R7RS program by Taylan Ulrich Bayırlı/Kammer, Copyright (C) 2014.

(define-library (srfi-tests 2 tests)
  (export run-tests)
  (import
   (scheme base)
   (scheme write)
   (scheme eval)
   (rename (srfi 2) (and-let* land*)))
  ;; (include "2-alt.tests.scm")
  (include "2.tests.scm")
  )
