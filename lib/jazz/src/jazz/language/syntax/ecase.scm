;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; ECase Expansion
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


(module protected jazz.language.syntax.ecase scheme


(export ecase
        nucase)

(import (jazz.language.runtime.kernel))


(native private jazz:error)


;; @syntax (ecase target ((a) 1) ((b c) 2) (else 3))
;; @expansion (let ((sym8 target))
;;             (cond ((eqv? sym8 a) 1)
;;               ((or (eqv? sym8 b) (eqv? sym8 c)) 2)
;;               (else 3)))


(define-syntax ecase
  (lambda (form-src usage-environment macro-environment)
    (let ((form (cdr (source-code form-src))))
      (if (null? form)
          (error "Ill-formed ecase")
        (let ((target (car form))
              (clauses (cdr form)))
          (if (null? clauses)
              (error "Ill-formed ecase")
            (sourcify-deep-if
              (with-uniqueness target
                (lambda (symbol)
                  `(cond ,@(map (lambda (clause)
                                  (let ((selector (car (source-code clause)))
                                        (body (cdr (source-code clause))))
                                    (cond ((eq? (source-code selector) 'else)
                                           (cons 'else body))
                                          ((pair? (source-code selector))
                                           (cons (cons 'or (map (lambda (value)
                                                                  (if (integer? (source-code value))
                                                                      (list '= symbol value)
                                                                    (list 'eqv? symbol value)))
                                                                (source-code selector)))
                                                 body))
                                          (else
                                           (error "Ill-formed selector list: {s}" (desourcify selector))))))
                                clauses))))
              form-src)))))))


;; quick try
(define-syntax nucase
  (lambda (form-src usage-environment macro-environment)
    (let ((form (cdr (source-code form-src))))
      (if (null? form)
          (error "Ill-formed nucase")
        (let ((target (car form))
              (clauses (cdr form)))
          (if (null? clauses)
              (error "Ill-formed nucase")
            (sourcify-deep-if
              (with-uniqueness target
                (lambda (symbol)
                  `(cond ,@(map (lambda (clause)
                                  (let ((selector (car (source-code clause)))
                                        (body (cdr (source-code clause))))
                                    (cond ((eq? (source-code selector) 'else)
                                           (cons 'else body))
                                          ((pair? (source-code selector))
                                           (cons (cons 'or (map (lambda (value)
                                                                  (list 'nu=? symbol value))
                                                                (source-code selector)))
                                                 body))
                                          (else
                                           (error "Ill-formed selector list: {s}" (desourcify selector))))))
                                clauses))))
              form-src))))))))
