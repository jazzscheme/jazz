;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Scheme Syntax
;;;
;;;  The contents of this file are subject to the Mozilla Public License Version
;;;  1.1 (the "License"); you may not use this file except in compliance with
;;;  the License. You may obtain a copy of the License at
;;;  http://www.mozilla.org/MPL/
;;;
;;;  Software distributed under the License is distributed on an "AS IS" basis,
;;;  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
;;;  for the specific language governing rights and limitations under the
;;;  License.
;;;
;;;  The Original Code is JazzScheme.
;;;
;;;  The Initial Developer of the Original Code is Guillaume Cartier.
;;;  Portions created by the Initial Developer are Copyright (C) 1996-2018
;;;  the Initial Developer. All Rights Reserved.
;;;
;;;  Contributor(s):
;;;
;;;  Alternatively, the contents of this file may be used under the terms of
;;;  the GNU General Public License Version 2 or later (the "GPL"), in which
;;;  case the provisions of the GPL are applicable instead of those above. If
;;;  you wish to allow use of your version of this file only under the terms of
;;;  the GPL, and not to allow others to use your version of this file under the
;;;  terms of the MPL, indicate your decision by deleting the provisions above
;;;  and replace them with the notice and other provisions required by the GPL.
;;;  If you do not delete the provisions above, a recipient may use your version
;;;  of this file under the terms of any one of the MPL or the GPL.
;;;
;;;  See www.jazzscheme.org for details.


(module scheme.syntax scheme


(export declaration-unit
        declaration-path
        declaration-locator
        optional
        let-optionals*)

(import (scheme.kernel)
        (scheme.syntax-rules (phase syntax)))


(native private jazz:error)
(native private jazz:unspecified)


;;;
;;;; Declaration
;;;


(define-syntax declaration-unit
  (lambda (form-src usage-environment macro-environment)
    (sourcify-deep-if
      `(quote ,(car (get-declaration-path (get-declaration-toplevel (current-declaration)))))
      form-src)))


(define-syntax declaration-path
  (lambda (form-src usage-environment macro-environment)
    (sourcify-deep-if
      `(quote ,(get-declaration-path (current-declaration)))
      form-src)))


(define-syntax declaration-locator
  (lambda (form-src usage-environment macro-environment)
    (sourcify-deep-if
      `(quote ,(apply compose-reference (get-declaration-path (current-declaration))))
      form-src)))


;;;
;;;; Optionals
;;;


(define-syntax optional
  (syntax-rules ()
    ((optional rest default-exp)
     (let ((maybe-arg rest))
       (cond ((null? maybe-arg) default-exp)
             ((null? (cdr maybe-arg)) (car maybe-arg))
             (else (error "too many optional arguments" maybe-arg)))))
    ((optional rest default-exp validator)
     (optional rest default-exp))))


(define-syntax really-let-optionals*
  (syntax-rules ()
    ;; Standard case. Do the first var/default and recurse.
    ((really-let-optionals* args ((var1 default1 typecheck1 ...) etc ...)
       body1 ...)
     (call-with-values (lambda () (if (null? args)
                                      (values default1 '())
                                      (values (car args) (cdr args))))
                       (lambda (var1 rest)
                         (really-let-optionals* rest (etc ...)
                           body1 ...))))

    ;; Single rest arg -- bind to the remaining rest values.
    ((really-let-optionals* args (rest) body1 ...)
     (let ((rest args)) body1 ...))

    ;; No more vars. Make sure there are no unaccounted-for values, and do the body.
    ((really-let-optionals* args () body1 ...)
     (if (null? args) (begin body1 ...)
         (error "Too many optional arguments." args)))))

(define-syntax let-optionals*
  (syntax-rules ()
    ((let-optionals* args vars&defaults body1 ...)
     (let ((rest args))
       (really-let-optionals* rest vars&defaults body1 ...))))))
