;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Scheme Scheme Backend
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


(unit scheme.backend.scheme.emit


;;;
;;;; Define
;;;


(jazz:define-emit (define (scheme backend) declaration walker resume environment expression)
  (jazz:sourcify-deep-if
    (let ((locator (jazz:get-declaration-locator declaration)))
      `(begin
         (define ,locator
           ,expression)
         ,@(let ((name (jazz:get-lexical-binding-name declaration))
                 (parent (jazz:get-declaration-parent declaration)))
             (if (%%is? parent jazz:Module-Declaration)
                 (if (jazz:get-generate? 'register)
                     `((jazz:register-define ',(jazz:get-lexical-binding-name parent) ',name ',locator))
                   '())
               `((jazz:add-field ,(jazz:get-declaration-locator parent) (jazz:new-define ',name ',locator)))))))
    (jazz:get-declaration-source declaration)))


;;;
;;;; Lambda
;;;


(jazz:define-emit (lambda (scheme backend) expression declaration walker resume environment signature-emit signature-casts cast-body)
  `(lambda ,signature-emit
     ,@(jazz:add-signature-casts signature-casts cast-body)))


;;;
;;;; Let
;;;


(jazz:define-emit (let (scheme backend) expression declaration walker resume environment bindings-output body-code)
  `(let ,bindings-output
     ,@(jazz:sourcified-form body-code)))


;;;
;;;; Named Let
;;;


(jazz:define-emit (named-let (scheme backend) expression declaration walker resume environment variable-emit bindings-output body-code)
  `(let ,variable-emit ,bindings-output
     ,@(jazz:sourcified-form body-code)))


;;;
;;;; Letstar
;;;


(jazz:define-emit (letstar (scheme backend) expression declaration walker resume environment bindings-output body-code)
  `(let* ,bindings-output
     ,@(jazz:sourcified-form body-code)))


;;;
;;;; Letrec
;;;


(jazz:define-emit (letrec (scheme backend) expression declaration walker resume environment bindings-output body-code)
  `(letrec ,bindings-output
     ,@(jazz:sourcified-form body-code)))


;;;
;;;; Let Macro
;;;


(jazz:define-emit (let-macro (scheme backend) expression declaration walker resume environment body-emit)
  (jazz:simplify-begin
    `(begin
       ,@(jazz:sourcified-form body-emit))))


;;;
;;;; Let Symbol
;;;


(jazz:define-emit (let-symbol (scheme backend) expression declaration walker resume environment body-emit)
  (jazz:simplify-begin
    `(begin
       ,@(jazz:sourcified-form body-emit))))


;;;
;;;; Receive
;;;


(jazz:define-emit (receive (scheme backend) expression declaration walker resume environment bindings-output expression-output body-code)
  `(receive ,bindings-output
       ,expression-output
     ,@(jazz:sourcified-form body-code)))


;;;
;;;; And
;;;


(jazz:define-emit (and (scheme backend) expression declaration walker resume environment expressions)
  `(and ,@(jazz:codes-forms expressions)))


;;;
;;;; Or
;;;


(jazz:define-emit (or (scheme backend) expression declaration walker resume environment expressions)
  `(or ,@(jazz:codes-forms expressions)))


;;;
;;;; If
;;;


(jazz:define-emit (if (scheme backend) expression declaration walker resume environment test yes no)
  `(if ,(jazz:sourcified-form test)
       ,(jazz:sourcified-form yes)
     ,@(if no
           (%%list (jazz:simplify-begin (jazz:sourcified-form no)))
         '())))


;;;
;;;; Cond
;;;


(jazz:define-emit (cond (scheme backend) expression declaration walker resume environment clauses)
  `(cond ,@clauses))


;;;
;;;; Case
;;;


(jazz:define-emit (case (scheme backend) expression declaration walker resume environment target-emit clauses clauses-emit)
  `(case ,(jazz:sourcified-form target-emit)
     ,@(map (lambda (clause emited-clause)
              (let ((tries (%%car clause)))
                `(,tries ,(jazz:sourcified-form emited-clause))))
            clauses
            clauses-emit)))


;;;
;;;; Do
;;;


(jazz:define-emit (do (scheme backend) expression declaration walker resume environment bindings-output test-code result-code body-code)
  `(do ,bindings-output
       (,(jazz:sourcified-form test-code) ,@(jazz:sourcified-form result-code))
     ,@(jazz:sourcified-form body-code)))


;;;
;;;; Delay
;;;


(jazz:define-emit (delay (scheme backend) expression declaration walker resume environment expr)
  `(delay ,(jazz:sourcified-form expr)))


;;;
;;;; Quasiquote
;;;


(jazz:define-emit (quasiquote (scheme backend) expression declaration walker resume environment)
  (define (emit form)
    (if (%%pair? form)
        (if (or (%%eq? (%%car form) 'unquote)
                (%%eq? (%%car form) 'unquote-splicing))
            (%%list (%%car form) (jazz:sourcified-form (jazz:emit-expression (%%cadr form) declaration walker resume environment backend)))
          (%%cons (emit (%%car form)) (emit (%%cdr form))))
      form))
  
  (%%list 'quasiquote (emit (jazz:get-quasiquote-form expression))))


;;;
;;;; Parameterize
;;;


(jazz:define-emit (parameterize (scheme backend) expression declaration walker resume environment body-code)
  (let ((bindings (jazz:get-parameterize-bindings expression)))
    `(parameterize ,(map (lambda (binding)
                           (let ((variable (%%car binding))
                                 (value (%%cdr binding)))
                             `(,(jazz:sourcified-form (jazz:emit-expression variable declaration walker resume environment backend))
                               ,(jazz:sourcified-form (jazz:emit-expression value declaration walker resume environment backend)))))
                         bindings)
       ,@(jazz:sourcified-form body-code))))


;;;
;;;; Unspecific
;;;


(jazz:define-emit (unspecific (scheme backend) expression declaration walker resume environment code)
  `(begin ,@(jazz:sourcified-form code) (%%unspecified)))


;;;
;;;; Time
;;;


(jazz:define-emit (time (scheme backend) expression declaration walker resume environment expr port)
  `(time
     ,(jazz:sourcified-form expr)
     ,(jazz:sourcified-form port))))
