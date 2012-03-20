;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Foundation Scheme Backend
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


(unit foundation.backend.scheme.emit


;;;
;;;; Module
;;;


(jazz:define-emit (module (scheme backend) declaration environment)
  (let ((body-expansion (jazz:emit-namespace-statements (jazz:get-namespace-declaration-body declaration) declaration environment backend))
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
           
           (enqueue-load-unit 'foundation)
           (let ((dialect-name (jazz:get-module-declaration-dialect-name declaration)))
             (%%when (%%neq? dialect-name 'foundation)
               (enqueue-load-unit dialect-name)))
           (for-each (lambda (spec)
                       (jazz:parse-require spec
                                           (lambda (unit-name feature-requirement phase)
                                             (enqueue-load-unit unit-name))))
                     (jazz:get-module-declaration-requires declaration))
           (for-each (lambda (module-invoice)
                       (let ((only (jazz:get-module-invoice-only module-invoice))
                             (autoload (jazz:get-export-invoice-autoload module-invoice)))
                         (%%when (and (%%not only) (%%not autoload))
                           (let ((module-declaration (jazz:resolve-reference (jazz:get-module-invoice-module module-invoice) declaration))
                                 (phase (jazz:get-module-invoice-phase module-invoice)))
                             (%%when (and (%%neq? module-declaration declaration) (%%neq? phase 'syntax))
                               (enqueue-load-unit (jazz:get-lexical-binding-name module-declaration)))))))
                     (jazz:get-module-declaration-exports declaration))
           (for-each (lambda (module-invoice)
                       (let ((module-declaration (jazz:get-module-invoice-module module-invoice))
                             (phase (jazz:get-module-invoice-phase module-invoice)))
                         (%%when (and module-declaration (%%neq? phase 'syntax))
                           (enqueue-load-unit (jazz:get-lexical-binding-name module-declaration)))))
                     (jazz:get-module-declaration-imports declaration))
           (jazz:queue-list queue))
       ,@registration-expansion
       ,@inclusions-expansion
       ,@autoloads-expansion
       ,@literals-expansion
       ,@variables-expansion
       ,@body-expansion)))


;;;
;;;; Declare
;;;


(jazz:define-emit (declare (scheme backend) expression declaration environment)
  (let ((declarations (jazz:get-declare-declarations expression)))
    `(declare ,@declarations)))


;;;
;;;; Proclaim
;;;


(jazz:define-emit (proclaim (scheme backend) expression declaration environment)
  #f)


;;;
;;;; Specialize
;;;


(jazz:define-emit (specialize (scheme backend) expression declaration environment)
  `(begin))


;;;
;;;; Begin
;;;


(jazz:define-emit (begin (scheme backend) expression declaration environment code)
  `(begin ,@(jazz:sourcified-form code)))


;;;
;;;; Reference
;;;


(jazz:define-emit (autoload-reference (scheme backend) declaration environment referenced-declaration)
  `(,(jazz:autoload-locator referenced-declaration)))


(jazz:define-emit (define-reference (scheme backend) declaration)
  (jazz:get-declaration-locator declaration))


(jazz:define-emit (export-reference (scheme backend) declaration)
  (jazz:get-export-declaration-symbol declaration))


(jazz:define-emit (export-syntax-reference (scheme backend) declaration)
  (jazz:get-export-syntax-declaration-symbol declaration))


(jazz:define-emit (local-variable-reference (scheme backend) declaration)
  (jazz:get-local-variable-binding-variable declaration))


(jazz:define-emit (special-form-reference (scheme backend) binding)
  (jazz:get-lexical-binding-name binding))


(jazz:define-emit (variable-reference (scheme backend) binding source-declaration environment)
  (jazz:emit-binding-symbol binding source-declaration environment backend))


(jazz:define-emit (lexical-binding-reference (scheme backend) binding)
  (jazz:get-lexical-binding-name binding))


(jazz:define-emit (named-parameter-reference (scheme backend) parameter)
  (jazz:get-lexical-binding-name parameter))


(jazz:define-emit (symbol-reference (scheme backend) binding)
  (or (jazz:get-symbol-binding-gensym binding)
      (jazz:unwrap-syntactic-closure (jazz:get-lexical-binding-name binding))))


;;;
;;;; Call
;;;


(jazz:define-emit (call (scheme backend) expression declaration environment)
  #f)


(jazz:define-emit (expression-call (scheme backend) expression declaration operator arguments)
  `(,(jazz:sourcified-form operator) ,@(jazz:codes-forms arguments)))


;;;
;;;; Binding Call
;;;


(jazz:define-emit (walk-binding-binding-call (scheme backend) binding binding-src operator arguments)
  `(,(jazz:sourcified-form2 operator binding-src)
    ,@(jazz:codes-forms arguments)))


;;;
;;;; Assignment
;;;


(jazz:define-emit (define-assignment (scheme backend) declaration source-declaration environment value-code)
  (let ((locator (jazz:get-declaration-locator declaration)))
    `(set! ,locator ,(jazz:sourcified-form value-code))))


(jazz:define-emit (variable-assignment (scheme backend) binding source-declaration environment binding-code value-code)
  `(set! ,binding-code ,(jazz:sourcified-form value-code))))
