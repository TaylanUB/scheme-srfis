;; Copyright (c) 2005, 2006, 2007, 2012, 2013 Per Bothner
;; Added "full" support for Chicken, Gauche, Guile and SISC.
;;   Alex Shinn, Copyright (c) 2005.
;; Modified for Scheme Spheres by Álvaro Castro-Castilla, Copyright (c) 2012.
;; Support for Guile 2 by Mark H Weaver <mhw@netris.org>, Copyright (c) 2014.
;; Refactored by Taylan Ulrich Bayırlı/Kammer, Copyright (c) 2014, 2015.
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(define-library (srfi 64 source-info)
  (import (scheme base))
  (export set-source-info!)
  (begin

    (define-syntax set-source-info!
      (cond-expand
       ((or kawa mzscheme guile-2)
        (lambda (stx)
          (syntax-case stx ()
            ((_ runner)
             (let ((file (syntax-source-file stx))
                   (line (syntax-source-line stx)))
               (quasisyntax
                (begin
                  (test-result-set! runner 'source-file (unsyntax file))
                  (test-result-set! runner 'source-line (unsyntax line)))))))))
       (else
        (syntax-rules ()
          ((_ runner)
           (values))))))

    (define (syntax-source-file stx)
      (cond-expand
       (kawa
        (syntax-source stx))
       (mzscheme
        (let ((source (syntax-source stx)))
          (cond
           ((string? source)
            file)
           ((path? source)
            (path->string source))
           (else
            #f))))
       (guile-2
        (let ((source (syntax-source stx)))
          (and source (assq-ref source 'filename))))))

    (define (syntax-source-line stx)
      (cond-expand
       ((or kawa mzscheme)
        (syntax-line stx))
       (guile-2
        (let ((source (syntax-source stx)))
          (and source (assq-ref source 'line))))))

    ))
