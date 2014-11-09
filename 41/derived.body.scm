;;; Copyright (C) Philip L. Bewig (2007). All Rights Reserved.

;;; Made an R7RS library by Taylan Ulrich Bayırlı/Kammer, Copyright (C) 2014.

;;; Permission is hereby granted, free of charge, to any person obtaining a copy
;;; of this software and associated documentation files (the "Software"), to
;;; deal in the Software without restriction, including without limitation the
;;; rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
;;; sell copies of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:

;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.

;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
;;; IN THE SOFTWARE.

;;; Eval these in Emacs:
;; (put 'stream-lambda 'scheme-indent-function 1)
;; (put 'stream-let 'scheme-indent-function 2)

(define-syntax define-stream
  (syntax-rules ()
    ((define-stream (name . formal) body0 body1 ...)
     (define name (stream-lambda formal body0 body1 ...)))))

(define (list->stream objs)
  (define list->stream
    (stream-lambda (objs)
      (if (null? objs)
          stream-null
          (stream-cons (car objs) (list->stream (cdr objs))))))
  (if (not (list? objs))
      (error "non-list argument" objs)
      (list->stream objs)))

(define (port->stream . port)
  (define port->stream
    (stream-lambda (p)
      (let ((c (read-char p)))
        (if (eof-object? c)
            stream-null
            (stream-cons c (port->stream p))))))
  (let ((p (if (null? port) (current-input-port) (car port))))
    (if (not (input-port? p))
        (error "non-input-port argument" p)
        (port->stream p))))

(define-syntax stream
  (syntax-rules ()
    ((stream) stream-null)
    ((stream x y ...) (stream-cons x (stream y ...)))))

(define (stream->list . args)
  (let ((n (if (= 1 (length args)) #f (car args)))
        (strm (if (= 1 (length args)) (car args) (cadr args))))
    (cond
     ((not (stream? strm)) (error "non-stream argument" strm))
     ((and n (not (integer? n))) (error "non-integer count" n))
     ((and n (negative? n)) (error "negative count" n))
     (else (let loop ((n (if n n -1)) (strm strm))
             (if (or (zero? n) (stream-null? strm))
                 '()
                 (cons (stream-car strm)
                       (loop (- n 1) (stream-cdr strm)))))))))

(define (stream-append . strms)
  (define stream-append
    (stream-lambda (strms)
      (cond
       ((null? (cdr strms)) (car strms))
       ((stream-null? (car strms)) (stream-append (cdr strms)))
       (else (stream-cons (stream-car (car strms))
                          (stream-append (cons (stream-cdr (car strms))
                                               (cdr strms))))))))
  (cond
   ((null? strms) stream-null)
   ((find (lambda (x) (not (stream? x))) strms)
    => (lambda (strm)
         (error "non-stream argument" strm)))
   (else (stream-append strms))))

(define (stream-concat strms)
  (define stream-concat
    (stream-lambda (strms)
      (cond
       ((stream-null? strms) stream-null)
       ((not (stream? (stream-car strms)))
        (error "non-stream object in input stream" strms))
       ((stream-null? (stream-car strms))
        (stream-concat (stream-cdr strms)))
       (else (stream-cons
              (stream-car (stream-car strms))
              (stream-concat
               (stream-cons (stream-cdr (stream-car strms))
                            (stream-cdr strms))))))))
  (if (not (stream? strms))
      (error "non-stream argument" strms)
      (stream-concat strms)))

(define stream-constant
  (stream-lambda objs
    (cond
     ((null? objs) stream-null)
     ((null? (cdr objs)) (stream-cons (car objs)
                                      (stream-constant (car objs))))
     (else (stream-cons (car objs)
                        (apply stream-constant
                               (append (cdr objs) (list (car objs)))))))))

(define (stream-drop n strm)
  (define stream-drop
    (stream-lambda (n strm)
      (if (or (zero? n) (stream-null? strm))
          strm
          (stream-drop (- n 1) (stream-cdr strm)))))
  (cond
   ((not (integer? n)) (error "non-integer argument" n))
   ((negative? n) (error "negative argument" n))
   ((not (stream? strm)) (error "non-stream argument" strm))
   (else (stream-drop n strm))))

(define (stream-drop-while pred? strm)
  (define stream-drop-while
    (stream-lambda (strm)
      (if (and (stream-pair? strm) (pred? (stream-car strm)))
          (stream-drop-while (stream-cdr strm))
          strm)))
  (cond
   ((not (procedure? pred?)) (error "non-procedural argument" pred?))
   ((not (stream? strm)) (error "non-stream argument" strm))
   (else (stream-drop-while strm))))

(define (stream-filter pred? strm)
  (define stream-filter
    (stream-lambda (strm)
      (cond
       ((stream-null? strm) stream-null)
       ((pred? (stream-car strm))
        (stream-cons (stream-car strm) (stream-filter (stream-cdr strm))))
       (else (stream-filter (stream-cdr strm))))))
  (cond
   ((not (procedure? pred?)) (error "non-procedural argument" pred?))
   ((not (stream? strm)) (error "non-stream argument" strm))
   (else (stream-filter strm))))

(define (stream-fold proc base strm)
  (cond
   ((not (procedure? proc)) (error "non-procedural argument" proc))
   ((not (stream? strm)) (error "non-stream argument" strm))
   (else (let loop ((base base) (strm strm))
           (if (stream-null? strm)
               base
               (loop (proc base (stream-car strm)) (stream-cdr strm)))))))

(define (stream-for-each proc . strms)
  (define (stream-for-each strms)
    (if (not (find stream-null? strms))
        (begin (apply proc (map stream-car strms))
               (stream-for-each (map stream-cdr strms)))))
  (cond
   ((not (procedure? proc)) (error "non-procedural argument" proc))
   ((null? strms) (error "no stream arguments"))
   ((find (lambda (x) (not (stream? x))) strms)
    => (lambda (strm)
         (error "non-stream argument" strm)))
   (else (stream-for-each strms))))

(define (stream-from first . step)
  (define stream-from
    (stream-lambda (first delta)
      (stream-cons first (stream-from (+ first delta) delta))))
  (let ((delta (if (null? step) 1 (car step))))
    (cond
     ((not (number? first)) (error "non-numeric starting number" first))
     ((not (number? delta)) (error "non-numeric step size" delta))
     (else (stream-from first delta)))))

(define (stream-iterate proc base)
  (define stream-iterate
    (stream-lambda (base)
      (stream-cons base (stream-iterate (proc base)))))
  (if (not (procedure? proc))
      (error "non-procedural argument" proc)
      (stream-iterate base)))

(define (stream-length strm)
  (if (not (stream? strm))
      (error "non-stream argument" strm)
      (let loop ((len 0) (strm strm))
        (if (stream-null? strm)
            len
            (loop (+ len 1) (stream-cdr strm))))))

(define-syntax stream-let
  (syntax-rules ()
    ((stream-let tag ((name val) ...) body1 body2 ...)
     ((letrec ((tag (stream-lambda (name ...) body1 body2 ...)))
        tag)
      val ...))))

(define (stream-map proc . strms)
  (define stream-map
    (stream-lambda (strms)
      (if (find stream-null? strms)
          stream-null
          (stream-cons (apply proc (map stream-car strms))
                       (stream-map (map stream-cdr strms))))))
  (cond
   ((not (procedure? proc)) (error "non-procedural argument" proc))
   ((null? strms) (error "no stream arguments"))
   ((find (lambda (x) (not (stream? x))) strms)
    => (lambda (strm)
         (error "non-stream argument" strm)))
   (else (stream-map strms))))

(define-syntax stream-match
  (syntax-rules ()
    ((stream-match strm-expr clause ...)
     (let ((strm strm-expr))
       (cond
        ((not (stream? strm)) (error "non-stream argument" strm))
        ((stream-match-test strm clause) => car) ...
        (else (error "pattern failure")))))))

(define-syntax stream-match-test
  (syntax-rules ()
    ((stream-match-test strm (pattern fender expr))
     (stream-match-pattern strm pattern () (and fender (list expr))))
    ((stream-match-test strm (pattern expr))
     (stream-match-pattern strm pattern () (list expr)))))

(define-syntax stream-match-pattern
  (syntax-rules (_)
    ((stream-match-pattern strm () (binding ...) body)
     (and (stream-null? strm) (let (binding ...) body)))
    ((stream-match-pattern strm (_ . rest) (binding ...) body)
     (and (stream-pair? strm)
          (let ((strm (stream-cdr strm)))
            (stream-match-pattern strm rest (binding ...) body))))
    ((stream-match-pattern strm (var . rest) (binding ...) body)
     (and (stream-pair? strm)
          (let ((temp (stream-car strm)) (strm (stream-cdr strm))) 
            (stream-match-pattern strm rest ((var temp) binding ...) body))))
    ((stream-match-pattern strm _ (binding ...) body)
     (let (binding ...) body))
    ((stream-match-pattern strm var (binding ...) body) 
     (let ((var strm) binding ...) body))))

(define-syntax stream-of
  (syntax-rules ()
    ((_ expr rest ...)
     (stream-of-aux expr stream-null rest ...))))

(define-syntax stream-of-aux
  (syntax-rules (in is)
    ((stream-of-aux expr base)
     (stream-cons expr base))
    ((stream-of-aux expr base (var in stream) rest ...)
     (stream-let loop ((strm stream))
       (if (stream-null? strm)
           base
           (let ((var (stream-car strm)))
             (stream-of-aux expr (loop (stream-cdr strm)) rest ...)))))
    ((stream-of-aux expr base (var is exp) rest ...)
     (let ((var exp)) (stream-of-aux expr base rest ...)))
    ((stream-of-aux expr base pred? rest ...)
     (if pred? (stream-of-aux expr base rest ...) base))))

(define (stream-range first past . step)
  (define stream-range
    (stream-lambda (first past delta lt?)
      (if (lt? first past)
          (stream-cons first (stream-range (+ first delta) past delta lt?))
          stream-null)))
  (cond
   ((not (number? first)) (error "non-numeric starting number" first))
   ((not (number? past)) (error "non-numeric ending number" past))
   (else (let ((delta (cond ((pair? step) (car step))
                            ((< first past) 1)
                            (else -1))))
           (if (not (number? delta))
               (error "non-numeric step size" delta)
               (let ((lt? (if (< 0 delta) < >)))
                 (stream-range first past delta lt?)))))))

(define (stream-ref strm n)
  (cond
   ((not (stream? strm)) (error "non-stream argument" strm))
   ((not (integer? n)) (error "non-integer argument" n))
   ((negative? n) (error "negative argument" n))
   (else (let loop ((strm strm) (n n))
           (cond
            ((stream-null? strm) (error "beyond end of stream" strm))
            ((zero? n) (stream-car strm))
            (else (loop (stream-cdr strm) (- n 1))))))))

(define (stream-reverse strm)
  (define stream-reverse
    (stream-lambda (strm rev)
      (if (stream-null? strm)
          rev
          (stream-reverse (stream-cdr strm)
                          (stream-cons (stream-car strm) rev)))))
  (if (not (stream? strm))
      (error "non-stream argument" strm)
      (stream-reverse strm stream-null)))

(define (stream-scan proc base strm)
  (define stream-scan
    (stream-lambda (base strm)
      (if (stream-null? strm)
          (stream base)
          (stream-cons base (stream-scan (proc base (stream-car strm))
                                         (stream-cdr strm))))))
  (cond
   ((not (procedure? proc)) (error "non-procedural argument" proc))
   ((not (stream? strm)) (error "non-stream argument" strm))
   (else (stream-scan base strm))))

(define (stream-take n strm)
  (define stream-take
    (stream-lambda (n strm)
      (if (or (stream-null? strm) (zero? n))
          stream-null
          (stream-cons (stream-car strm)
                       (stream-take (- n 1) (stream-cdr strm))))))
  (cond
   ((not (stream? strm)) (error "non-stream argument" strm))
   ((not (integer? n)) (error "non-integer argument" n))
   ((negative? n) (error "negative argument" n))
   (else (stream-take n strm))))

(define (stream-take-while pred? strm)
  (define stream-take-while
    (stream-lambda (strm)
      (cond
       ((stream-null? strm) stream-null)
       ((pred? (stream-car strm))
        (stream-cons (stream-car strm)
                     (stream-take-while (stream-cdr strm))))
       (else stream-null))))
  (cond
   ((not (stream? strm)) (error "non-stream argument" strm))
   ((not (procedure? pred?)) (error "non-procedural argument" pred?))
   (else (stream-take-while strm))))

(define (stream-unfold mapper pred? generator base)
  (define stream-unfold
    (stream-lambda (base)
      (if (pred? base)
          (stream-cons (mapper base) (stream-unfold (generator base)))
          stream-null)))
  (cond
   ((not (procedure? mapper)) (error "non-procedural mapper" mapper))
   ((not (procedure? pred?)) (error "non-procedural pred?" pred?))
   ((not (procedure? generator)) (error "non-procedural generator" generator))
   (else (stream-unfold base))))

(define (stream-unfolds gen seed)
  (define (len-values gen seed)
    (call-with-values
        (lambda () (gen seed))
      (lambda vs (- (length vs) 1))))
  (define unfold-result-stream
    (stream-lambda (gen seed)
      (call-with-values
          (lambda () (gen seed))
        (lambda (next . results)
          (stream-cons results (unfold-result-stream gen next))))))
  (define result-stream->output-stream
    (stream-lambda (result-stream i)
      (let ((result (list-ref (stream-car result-stream) (- i 1))))
        (cond
         ((pair? result)
          (stream-cons
           (car result)
           (result-stream->output-stream (stream-cdr result-stream) i)))
         ((not result)
          (result-stream->output-stream (stream-cdr result-stream) i))
         ((null? result) stream-null)
         (else (error "can't happen"))))))
  (define (result-stream->output-streams result-stream)
    (let loop ((i (len-values gen seed)) (outputs '()))
      (if (zero? i)
          (apply values outputs)
          (loop (- i 1) (cons (result-stream->output-stream result-stream i)
                              outputs)))))
  (if (not (procedure? gen))
      (error "non-procedural argument" gen)
      (result-stream->output-streams (unfold-result-stream gen seed))))

(define (stream-zip . strms)
  (define stream-zip
    (stream-lambda (strms)
      (if (find stream-null? strms)
          stream-null
          (stream-cons (map stream-car strms)
                       (stream-zip (map stream-cdr strms))))))
  (cond
   ((null? strms) (error "no stream arguments"))
   ((find (lambda (x) (not (stream? x))) strms)
    => (lambda (strm)
         (error "non-stream argument" strm)))
   (else (stream-zip strms))))
