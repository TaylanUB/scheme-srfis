;;; 17.scm --- SRFI 17: Generalized set!

;; Copyright (C) 2014  Taylan Ulrich Bayırlı/Kammer

;; Author: Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;; Keywords: srfi 17 srfi-17 generalized set!

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

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

;;; 17.scm ends here
