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

(define-library (srfi 5)
  (export (rename let+ let))
  (import (scheme base))
  (begin

    (define-syntax let+
      (syntax-rules ()
        ;; Unnamed, no rest args.
        ((_ ((var val) ...) body ...)
         (let ((var val) ...) body ...))
        ;; Unnamed, with rest args.
        ((_ ((var val) spec ...) body ...)
         (rest ((var val) spec ...) () () body ...))
        ;; Signature style, no rest args.
        ((_ (name (var val) ...) body ...)
         (let name ((var val) ...) body ...))
        ;; Signature style, with rest args.
        ((_ (name (var val) spec ...) body ...)
         (rest/named name ((var val) spec ...) () () body ...))
        ;; Named let, no rest args.
        ((_ name ((var val) ...) body ...)
         (let name ((var val) ...) body ...))
        ;; Named let, with rest args.
        ((_ name ((var val) spec ...) body ...)
         (rest/named name ((var val) spec ...) () () body ...))))

    (define-syntax rest
      (syntax-rules ()
        ((_ ((var val) spec ...) (var* ...) (val* ...) body ...)
         (rest name (spec ...) (var var* ...) (val val* ...) body ...))
        ((_ (rest-var rest-val ...) (var ...) (val ...) body ...)
         (let ((var val)
               ...
               (rest-var (list rest-val ...)))
           body ...))))

    (define-syntax rest/named
      (syntax-rules ()
        ((_ name ((var val) spec ...) (var* ...) (val* ...) body ...)
         (rest/named name (spec ...) (var var* ...) (val val* ...) body ...))
        ((_ name (rest-var rest-val ...) (var ...) (val ...) body ...)
         (letrec ((name (lambda (var ... . rest-var) body ...)))
           (name val ... rest-val ...)))))

    ))
