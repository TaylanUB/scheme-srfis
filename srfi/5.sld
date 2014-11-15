;;; 5.sld --- SRFI-5: A compatible let form with signatures and rest arguments

;; Copyright (C) 2014  Taylan Ulrich Bayırlı/Kammer

;; Author: Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;; Keywords: srfi-5 srfi 5 let

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as published
;; by the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Please don't use weird non-standard licenses for your works!  Thankfully this
;; SRFI is trivial to implement, because the license of the reference
;; implementation made me irk.

;; Eval this in Emacs:
;; (put 'standard-let 'scheme-indent-function 1)

;;; Code:

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

;;; 5.sld ends here
