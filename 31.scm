;;; 31.scm --- SRFI 31: A special form `rec' for recursive evaluation

;; Copyright (C) 2014  Taylan Ulrich Bayırlı/Kammer

;; Author: Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;; Keywords: srfi 31 srfi-31 rec recursive

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

(define-library (srfi 31)
  (export rec)
  (import (scheme base))
  (begin
    (define-syntax rec
      (syntax-rules ()
        ((rec (name . args) body ...)
         (letrec ((name (lambda args body ...)))
           name))
        ((rec name expr)
         (letrec ((name expr))
           name))))))

;;; 31.scm ends here
