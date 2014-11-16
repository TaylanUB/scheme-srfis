;; Copyright (C) Oleg Kiselyov (1998). All Rights Reserved.

;; Made an R7RS program by Taylan Ulrich Bayırlı/Kammer, Copyright (C) 2014.

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

(import
 (scheme base)
 (scheme eval)
 (scheme write)
 (rename (srfi 2) (and-let* land*)))

(define (test-eval form)
  (eval form (environment
              '(scheme base)
              '(rename (srfi 2) (and-let* land*)))))

(define-syntax expect
  (syntax-rules ()
    ((expect form expected-result*)
     (let ((expected-result expected-result*))
       (display "evaluating ")
       (write 'form)
       (let ((real-result (test-eval 'form)))
         (if (equal? real-result expected-result)
             (begin
               (display "... gave the expected result: ")
               (display expected-result)
               (newline))
             (begin
               (display "... yielded: ")
               (display real-result)
               (display " which differs from the expected result: ")
               (display expected-result)
               (error "Test failed:" 'form))
             ))))))

;; We want to check whether 'form' has indeed wrong syntax.
;; We eval it and check for any exception, which is our best approximation.
(define-syntax must-be-a-syntax-error
  (syntax-rules ()
    ((must-be-a-syntax-error form)
     (guard (err
             (else
              (display "catching what should be a syntax error: ")
              (display err)
              (newline)))
       (test-eval 'form)
       (error "No syntax error detected, unexpectedly")))))

(expect  (land* () 1) 1)
(expect  (land* () 1 2) 2)
(expect  (land* () ) #t)

(expect (let ((x #f)) (land* (x))) #f)
(expect (let ((x 1)) (land* (x))) 1)
(expect (land* ((x #f)) ) #f)
(expect (land* ((x 1)) ) 1)
(must-be-a-syntax-error (land* ( #f (x 1))) )
(expect (land* ( (#f) (x 1)) ) #f)
(must-be-a-syntax-error (land* (2 (x 1))) )
(expect (land* ( (2) (x 1)) ) 1)
(expect (land* ( (x 1) (2)) ) 2)
(expect (let ((x #f)) (land* (x) x)) #f)
(expect (let ((x "")) (land* (x) x)) "")
(expect (let ((x "")) (land* (x)  )) "")
(expect (let ((x 1)) (land* (x) (+ x 1))) 2)
(expect (let ((x #f)) (land* (x) (+ x 1))) #f)
(expect (let ((x 1)) (land* (((positive? x))) (+ x 1))) 2)
(expect (let ((x 1)) (land* (((positive? x))) )) #t)
(expect (let ((x 0)) (land* (((positive? x))) (+ x 1))) #f)
(expect (let ((x 1)) (land* (((positive? x)) (x (+ x 1))) (+ x 1)))  3)
;;; This is marked as must-be-error in the original test suite; see comments in
;;; the implementation for our rationale for intentionally breaking off from the
;;; specification.
(let ((x 1)) (land* (((positive? x)) (x (+ x 1)) (x (+ x 1))) (+ x 1)))


(expect (let ((x 1)) (land* (x ((positive? x))) (+ x 1))) 2)
(expect (let ((x 1)) (land* ( ((begin x)) ((positive? x))) (+ x 1))) 2)
(expect (let ((x 0)) (land* (x ((positive? x))) (+ x 1))) #f)
(expect (let ((x #f)) (land* (x ((positive? x))) (+ x 1))) #f)
(expect (let ((x #f)) (land* ( ((begin x)) ((positive? x))) (+ x 1))) #f)

(expect  (let ((x 1)) (land* (x (y (- x 1)) ((positive? y))) (/ x y))) #f)
(expect  (let ((x 0)) (land* (x (y (- x 1)) ((positive? y))) (/ x y))) #f)
(expect  (let ((x #f)) (land* (x (y (- x 1)) ((positive? y))) (/ x y))) #f)
(expect  (let ((x 3)) (land* (x (y (- x 1)) ((positive? y))) (/ x y))) 3/2)

(display "\nAll tests passed\n")
