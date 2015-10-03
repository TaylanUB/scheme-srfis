;; Copyright (C) Taylan Ulrich Bayırlı/Kammer (2015). All Rights Reserved.

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; The SRFI claims that having the same variable appear multiple times is an
;;; error in let* and so also in and-let*.  In fact let* allows rebinding the
;;; same variable, so we also allow it here.

(define-library (srfi 2)
  (export and-let*)
  (import (scheme base))
  (begin
    (define-syntax and-let*
      (syntax-rules ()

        ;; Handle zero-clauses special-case.
        ((_ () . body)
         (begin #t . body))

        ;; Reduce clauses down to one regardless of body.
        ((_ ((var expr) rest . rest*) . body)
         (let ((var expr))
           (and var (and-let* (rest . rest*) . body))))
        ((_ ((expr) rest . rest*) . body)
         (and expr (and-let* (rest . rest*) . body)))
        ((_ (var rest . rest*) . body)
         (begin
           (let ((var #f)) #f)          ;(identifier? var)
           (and var (and-let* (rest . rest*) . body))))

        ;; Handle 1-clause cases without a body.
        ((_ ((var expr)))
         expr)
        ((_ ((expr)))
         expr)
        ((_ (var))
         (begin
           (let ((var #f)) #f)          ;(identifier? var)
           var))

        ;; Handle 1-clause cases with a body.
        ((_ ((var expr)) . body)
         (let ((var expr))
           (and var (begin . body))))
        ((_ ((expr)) . body)
         (and expr (begin . body)))
        ((_ (var) . body)
         (begin
           (let ((var #f)) #f)          ;(identifier? var)
           (and var (begin . body))))))))
