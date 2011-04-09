;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Scheme Base Backend
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
;;;  Portions created by the Initial Developer are Copyright (C) 1996-2008
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


(unit protected dialect.base.syntax.backend.scheme


(jazz:define-backend scheme)


;;;
;;;; Module
;;;


(jazz:define-emit (module (scheme backend) declaration environment)
  (let ((body-expansion (jazz:emit-namespace-statements (%%get-namespace-declaration-body declaration) declaration environment backend))
        (inclusions-expansion (jazz:emit-module-inclusions declaration backend))
        (literals-expansion (jazz:emit-module-literals declaration backend))
        (variables-expansion (jazz:emit-module-variables declaration backend))
        (autoloads-expansion (jazz:emit-module-autoloads declaration environment backend))
        (registration-expansion (jazz:emit-module-registration declaration environment backend)))
    `(begin
       ,@(case (jazz:walk-for)
           ((eval) '())
           (else (jazz:declares 'module)))
       ,@(let ((queue (jazz:new-queue))
               (load-units (%%make-table test: eq?)))
           (define (enqueue-load-unit unit-name)
             (%%when (%%not (%%table-ref load-units unit-name #f))
               (%%table-set! load-units unit-name #t)
               (jazz:enqueue queue `(jazz:load-unit ',unit-name))))
           
           (enqueue-load-unit 'dialect.base)
           (let ((dialect-name (%%get-module-declaration-dialect-name declaration)))
             (%%when (%%neq? dialect-name 'base)
               (enqueue-load-unit dialect-name)))
           (for-each (lambda (spec)
                       (jazz:parse-require spec
                                           (lambda (unit-name feature-requirement phase)
                                             (enqueue-load-unit unit-name))))
                     (%%get-module-declaration-requires declaration))
           (for-each (lambda (module-invoice)
                       (let ((only (%%get-module-invoice-only module-invoice))
                             (autoload (%%get-export-invoice-autoload module-invoice)))
                         (%%when (and (%%not only) (%%not autoload))
                           (let ((module-declaration (jazz:resolve-reference (%%get-module-invoice-module module-invoice) declaration))
                                 (phase (%%get-module-invoice-phase module-invoice)))
                             (%%when (and (%%neq? module-declaration declaration) (%%neq? phase 'syntax))
                               (enqueue-load-unit (%%get-lexical-binding-name module-declaration)))))))
                     (%%get-module-declaration-exports declaration))
           (for-each (lambda (module-invoice)
                       (let ((module-declaration (%%get-module-invoice-module module-invoice))
                             (phase (%%get-module-invoice-phase module-invoice)))
                         (%%when (and module-declaration (%%neq? phase 'syntax))
                           (enqueue-load-unit (%%get-lexical-binding-name module-declaration)))))
                     (%%get-module-declaration-imports declaration))
           (jazz:queue-list queue))
       ,@registration-expansion
       ,@inclusions-expansion
       ,@autoloads-expansion
       ,@literals-expansion
       ,@variables-expansion
       ,@body-expansion)))


;;;
;;;; Proclaim
;;;


(jazz:define-emit (proclaim (scheme backend) expression declaration environment)
  #f)


;;;
;;;; Quasiquote
;;;


(jazz:define-emit (quasiquote (scheme backend) expression declaration environment)
  (define (emit form)
    (if (%%pair? form)
        (if (or (%%eq? (%%car form) 'unquote)
                (%%eq? (%%car form) 'unquote-splicing))
            (%%list (%%car form) (jazz:sourcified-form (jazz:emit-expression (%%cadr form) declaration environment backend)))
          (%%cons (emit (%%car form)) (emit (%%cdr form))))
      form))
  
  (%%list 'quasiquote (emit (%%get-quasiquote-form expression))))


;;;
;;;; Reference
;;;


;;;
;;;; Call
;;;


(jazz:define-emit (call (scheme backend) expression declaration operator arguments)
  `(,(jazz:sourcified-form operator) ,@(jazz:codes-forms arguments)))


(jazz:define-emit (method-reference-call (scheme backend) expression declaration operator arguments)
  `(,(jazz:sourcified-form operator) ,@(jazz:codes-forms arguments)))


;;;
;;;; Binding Call
;;;


(jazz:define-emit (walk-binding-binding-call (scheme backend) binding binding-src operator arguments)
  `(,(jazz:sourcified-form2 operator binding-src)
    ,@(jazz:codes-forms arguments)))


(jazz:define-emit (method-binding-call (scheme backend) binding binding-src dispatch-code self arguments)
  `(,(jazz:sourcified-form dispatch-code)
    ,(jazz:sourcified-form self)
    ,@arguments))


(jazz:define-emit (nextmethod-binding-call (scheme backend) binding binding-src self arguments)
  (let ((name (%%get-lexical-binding-name binding)))
    (if self
        `(,name
           ,(jazz:sourcified-form self)
           ,@(jazz:codes-forms arguments))
      `(,name
         ,@(jazz:codes-forms arguments)))))


;;;
;;;; Specialized Call
;;;


(jazz:define-emit (specialized-call (scheme backend) expression declaration operator arguments)
  #f)


;;;
;;;; Specialized Class-of Call
;;;


(jazz:define-emit (specialized-class-of-call (scheme backend) object)
  `(jazz:class-of ,(jazz:sourcified-form object)))


;;;
;;;; New Call
;;;


(jazz:define-emit (new-call (scheme backend) operator locator arguments arguments-codes declaration environment)
  #f)


;;;
;;;; Primitive Call
;;;


(jazz:define-emit (primitive-call (scheme backend) expression declaration operator arguments)
  #f)


;;;
;;;; Inlined Call
;;;


(jazz:define-emit (inlined-call (scheme backend) expression declaration operator arguments)
  #f)


;;;
;;;; Assignment
;;;


(jazz:define-emit (define-assignment (scheme backend) declaration source-declaration environment value-code)
  (let ((locator (%%get-declaration-locator declaration)))
    `(set! ,locator ,(jazz:sourcified-form value-code))))


(jazz:define-emit (definition-assignment (scheme backend) declaration source-declaration environment value-code)
  (let ((locator (%%get-declaration-locator declaration)))
    `(set! ,locator ,(jazz:sourcified-form value-code))))


(jazz:define-emit (slot-assignment (scheme backend) declaration source-declaration environment self value-code)
  (let ((offset-locator (jazz:compose-helper (%%get-declaration-locator declaration) 'offset)))
    `(%%object-set! ,(jazz:sourcified-form self) ,offset-locator ,(jazz:sourcified-form value-code))))


(jazz:define-emit (variable-assignment (scheme backend) binding source-declaration environment binding-code value-code)
  `(set! ,binding-code ,(jazz:sourcified-form value-code)))


;;;
;;;; Method Reference
;;;


(jazz:define-emit (method-reference (scheme backend) expression declaration environment)
  (let ((method-declaration (%%get-reference-binding expression)))
    (%%get-declaration-locator method-declaration)))


;;;
;;;; Lambda
;;;


(jazz:define-emit (lambda (scheme backend) expression declaration environment signature-emit signature-casts cast-body)
  (if (%%not signature-casts)
      `(lambda ,signature-emit
         ,cast-body)
    `(lambda ,signature-emit
       ,@signature-casts
       (let ()
         ,cast-body))))


;;;
;;;; Let
;;;


(jazz:define-emit (let (scheme backend) expression declaration environment bindings-output body-code)
  `(let ,bindings-output
     ,@(jazz:sourcified-form body-code)))


;;;
;;;; Named Let
;;;


(jazz:define-emit (named-let (scheme backend) expression declaration environment variable-emit bindings-output body-code)
  `(let ,variable-emit ,bindings-output
     ,@(jazz:sourcified-form body-code)))


;;;
;;;; Letstar
;;;


(jazz:define-emit (letstar (scheme backend) expression declaration environment bindings-output body-code)
  `(let* ,bindings-output
     ,@(jazz:sourcified-form body-code)))


;;;
;;;; Letrec
;;;


(jazz:define-emit (letrec (scheme backend) expression declaration environment bindings-output body-code)
  `(letrec ,bindings-output
     ,@(jazz:sourcified-form body-code)))


;;;
;;;; Receive
;;;


(jazz:define-emit (receive (scheme backend) expression declaration environment bindings-output expression-output body-code)
  `(receive ,bindings-output
       ,expression-output
     ,@(jazz:sourcified-form body-code)))


;;;
;;;; Begin
;;;


(jazz:define-emit (begin (scheme backend) expression declaration environment code)
  `(begin ,@(jazz:sourcified-form code)))


;;;
;;;; Do
;;;


(jazz:define-emit (do (scheme backend) expression declaration environment bindings-output test-code result-code body-code)
  `(do ,bindings-output
       (,(jazz:sourcified-form test-code) ,@(jazz:sourcified-form result-code))
     ,@(jazz:sourcified-form body-code)))


;;;
;;;; Delay
;;;


(jazz:define-emit (delay (scheme backend) expression declaration environment expr)
  `(delay ,(jazz:sourcified-form expr)))


;;;
;;;; If
;;;


(jazz:define-emit (if (scheme backend) expression declaration environment test yes no)
  `(if ,(jazz:sourcified-form test)
       ,(jazz:sourcified-form yes)
     ,(jazz:simplify-begin (jazz:sourcified-form no))))


;;;
;;;; Cond
;;;


(jazz:define-emit (cond (scheme backend) expression declaration environment)
  (let ((clauses (%%get-cond-clauses expression)))
    `(cond ,@(let recurse ((clauses clauses)
                           (environment environment))
                  (if (%%null? clauses) '()
                    (let ((clause (%%car clauses)))
                      (let ((test (%%car clause))
                            (arrow? (%%cadr clause))
                            (body (%%cddr clause)))
                        (jazz:bind (yes-environment . no-environment) (jazz:branch-types test environment)
                          (let ((output
                                  `(,(if (%%not test)
                                         'else
                                       (jazz:sourcified-form (jazz:emit-expression test declaration environment backend)))
                                    ,@(if arrow? `(=>) '())
                                    ,(jazz:sourcified-form (jazz:emit-expression body declaration yes-environment backend)))))
                            (%%cons output (recurse (%%cdr clauses) no-environment)))))))))))


;;;
;;;; Case
;;;


(jazz:define-emit (case (scheme backend) expression declaration environment target-emit clauses clauses-emit)
  `(case ,(jazz:sourcified-form target-emit)
     ,@(map (lambda (clause emited-clause)
              (let ((tries (%%car clause)))
                `(,tries ,(jazz:sourcified-form emited-clause))))
            clauses
            clauses-emit)))


;;;
;;;; And
;;;


(jazz:define-emit (and (scheme backend) expression declaration environment expressions)
  `(and ,@(jazz:codes-forms expressions)))


;;;
;;;; Or
;;;


(jazz:define-emit (or (scheme backend) expression declaration environment expressions)
  `(or ,@(jazz:codes-forms expressions)))


;;;
;;;; Declare
;;;


(jazz:define-emit (declare (scheme backend) expression declaration environment)
  (let ((declarations (%%get-declare-declarations expression)))
    `(declare ,@declarations)))


;;;
;;;; Parameterize
;;;


(jazz:define-emit (parameterize (scheme backend) expression declaration environment body-code)
  (let ((bindings (%%get-parameterize-bindings expression)))
    `(parameterize ,(map (lambda (binding)
                           (let ((variable (%%car binding))
                                 (value (%%cdr binding)))
                             `(,(jazz:sourcified-form (jazz:emit-expression variable declaration environment backend))
                               ,(jazz:sourcified-form (jazz:emit-expression value declaration environment backend)))))
                         bindings)
       ,@(jazz:sourcified-form body-code))))


;;;
;;;; Time
;;;


(jazz:define-emit (time (scheme backend) expression declaration environment expressions)
  `(time
     (begin
       ,@(jazz:codes-forms expressions)))))
