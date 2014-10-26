;;; 2.scm --- AND-LET*: an AND with local bindings, a guarded LET* special form

;; Copyright (C) 2014  Taylan Ulrich Bayırlı/Kammer

;; Author: Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;; Keywords: srfi srfi-2 and-let and-let* and let let*

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Implementing this is tricky because when there is no body, the last evaluated
;; clause should be returned, not #t, so we can't terminate our recursive macro
;; with a rule (and-let* () body ...) -> (and body ...), since that would mean
;; we're left with a dangling (and) i.e. #t.  Additionally, the BOUND-VARIABLE
;; case should match identifiers only; we force it to fail on non-identifiers by
;; abusing `let', but are careful not to evaluate any expressions under this
;; let, since that changes the effect of `set!'.
;;
;; We break up from the specification of this SRFI in that we allow the same
;; variable to be bound multiple times in sequential clauses.  The SRFI author
;; seems to have interpreted old RnRS versions as disallowing that in `let*'
;; when in fact it was ambiguous, and it's explicitly allowed starting from
;; R6RS.  Racket's, Guile's, and Chibi's SRFI-2 implementations allow it too.
;; Also, this is simple to implement; the alternative seems either very hard or
;; impossible to implement with syntax-rules.

;;; Code:

(define-library (srfi 2)
  (export and-let*)
  (import (scheme base))
  (begin
    (define-syntax and-let*
      (syntax-rules ()
        ((and-let* ((var expr) rest rest* ...) body ...)
         (let ((var expr))
           (and var (and-let* (rest rest* ...) body ...))))
        ((and-let* ((expr) rest rest* ...) body ...)
         (and expr (and-let* (rest rest* ...) body ...)))
        ((and-let* (bound-var rest rest* ...) body ...)
         (begin
           (let ((bound-var #f)) #f)    ;(identifier? bound-var)
           (and bound-var (and-let* (rest rest* ...) body ...))))
        ((and-let* ((var expr)) body ...)
         (let ((var expr))
           (and var body ...)))
        ((and-let* ((expr)) body ...)
         (and expr body ...))
        ((and-let* (bound-var) body ...)
         (begin
           (let ((bound-var #f)) #f)    ;(identifier? bound-var)
           (and bound-var body ...)))
        ((and-let* () body ...)
         (and body ...))))))

;;; 2.scm ends here
