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

(define-library (srfi 17)
  (export set! setter getter-with-setter)
  (import
   (rename (scheme base) (set! %set!))
   (srfi 1))
  (begin

    (define-syntax set!
      (syntax-rules ()
        ((_ (getter arg ...) val)
         ((setter getter) arg ... val))
        ((_ var val)
         (%set! var val))))

    (define setter
      (let ((setters `((,car . ,set-car!)
                       (,cdr . ,set-cdr!)
                       (,caar . ,(lambda (p v) (set-car! (car p) v)))
                       (,cadr . ,(lambda (p v) (set-car! (cdr p) v)))
                       (,cdar . ,(lambda (p v) (set-cdr! (car p) v)))
                       (,cddr . ,(lambda (p v) (set-cdr! (cdr p) v)))
                       (,list-ref . ,list-set!)
                       (,vector-ref . ,vector-set!)
                       (,string-ref . ,string-set!)
                       (,bytevector-u8-ref . ,bytevector-u8-set!))))
        (letrec ((setter
                  (lambda (proc)
                    (let ((probe (assv proc setters)))
                      (if probe
                          (cdr probe)
                          (error "No setter for " proc)))))
                 (set-setter!
                  (lambda (proc setter)
                    (set! setters (cons (cons proc setter) setters)))))
          (set-setter! setter set-setter!)
          setter)))

    (define (getter-with-setter get set)
      (let ((proc (lambda args (apply get args))))
        (set! (setter proc) set)
        proc))

    ))
