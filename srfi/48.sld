;;; SPDX-FileCopyrightText: 2014 Taylan Kammer <taylan.kammer@gmail.com>
;;;
;;; SPDX-License-Identifier: MIT

(define-library (srfi 48)
  (export format)
  (import (rename (scheme base)
                  (exact inexact->exact)
                  (inexact exact->inexact))
          (scheme char)
          (scheme complex)
          (rename (scheme write)
                  (write-shared write-with-shared-structure)))
  (include "48.upstream.scm"))
