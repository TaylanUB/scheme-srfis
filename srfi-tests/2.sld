;; Copyright (C) Oleg Kiselyov (1998). All Rights Reserved.

;; Made an R7RS library by Taylan Ulrich Bayırlı/Kammer, Copyright (C) 2014.

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(define-library (srfi-tests 2)
  (export run-tests)
  (import
   (scheme base)
   (scheme eval)
   (srfi 2)
   (srfi 64)
   (srfi-tests aux))
  (begin

    (define (test-eval form)
      (eval form (environment '(scheme base) '(srfi 2))))

    ;; We want to check whether 'form' has indeed wrong syntax.  We eval it and
    ;; check for any exception, which is our best approximation.
    (define-syntax test-syntax-error
      (syntax-rules ()
        ((_ form)
         (test-error (->string 'form) #t (test-eval 'form)))))

    (define-tests run-tests "SRFI-2"
      (test-equal+ 1 (and-let* () 1))
      (test-equal+ 2 (and-let* () 1 2))
      (test-equal+ #t (and-let* ()))

      (test-equal+ #f (let ((x #f)) (and-let* (x))))
      (test-equal+ 1 (let ((x 1)) (and-let* (x))))
      (test-equal+ #f (and-let* ((x #f))))
      (test-equal+ 1 (and-let* ((x 1))))
      (test-syntax-error (and-let* (#f (x 1))))
      (test-equal+ #f (and-let* ((#f) (x 1))))
      (test-syntax-error (and-let* (2 (x 1))))
      (test-equal+ 1 (and-let* ((2) (x 1))))
      (test-equal+ 2 (and-let* ((x 1) (2))))
      (test-equal+ #f (let ((x #f)) (and-let* (x) x)))
      (test-equal+ "" (let ((x "")) (and-let* (x) x)))
      (test-equal+ "" (let ((x "")) (and-let* (x))))
      (test-equal+ 2 (let ((x 1)) (and-let* (x) (+ x 1))))
      (test-equal+ #f (let ((x #f)) (and-let* (x) (+ x 1))))
      (test-equal+ 2 (let ((x 1)) (and-let* (((positive? x))) (+ x 1))))
      (test-equal+ #t (let ((x 1)) (and-let* (((positive? x))))))
      (test-equal+ #f (let ((x 0)) (and-let* (((positive? x))) (+ x 1))))
      (test-equal+ 3
        (let ((x 1)) (and-let* (((positive? x)) (x (+ x 1))) (+ x 1))))
      ;; This is marked as must-be-error in the original test suite; see
      ;; comments in the implementation for our rationale for intentionally
      ;; breaking off from the specification.
      (test-equal+ 4
        (let ((x 1))
          (and-let* (((positive? x)) (x (+ x 1)) (x (+ x 1))) (+ x 1))))

      (test-equal+ 2
        (let ((x 1)) (and-let* (x ((positive? x))) (+ x 1))))
      (test-equal+ 2
        (let ((x 1)) (and-let* (((begin x)) ((positive? x))) (+ x 1))))
      (test-equal+ #f
        (let ((x 0)) (and-let* (x ((positive? x))) (+ x 1))))
      (test-equal+ #f
        (let ((x #f)) (and-let* (x ((positive? x))) (+ x 1))))
      (test-equal+ #f
        (let ((x #f)) (and-let* (((begin x)) ((positive? x))) (+ x 1))))

      (test-equal+ #f
        (let ((x 1)) (and-let* (x (y (- x 1)) ((positive? y))) (/ x y))))
      (test-equal+ #f
        (let ((x 0)) (and-let* (x (y (- x 1)) ((positive? y))) (/ x y))))
      (test-equal+ #f
        (let ((x #f)) (and-let* (x (y (- x 1)) ((positive? y))) (/ x y))))
      (test-equal+ 3/2
        (let ((x 3)) (and-let* (x (y (- x 1)) ((positive? y))) (/ x y)))))

    ))
