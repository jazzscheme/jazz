;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Scheme Walker Syntax
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
;;;  Portions created by the Initial Developer are Copyright (C) 1996-2012
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


(unit protected scheme.dialect.syntax


;;;
;;;; Dialect
;;;


(jazz:define-class-syntax jazz:Scheme-Dialect jazz:Dialect (constructor: jazz:allocate-scheme-dialect)
  ())


;;;
;;;; Walker
;;;


(jazz:define-class-syntax jazz:Scheme-Walker jazz:Walker (constructor: jazz:allocate-scheme-walker)
  ())


;;;
;;;; Define
;;;


(jazz:define-class-syntax jazz:Define-Declaration jazz:Declaration (constructor: jazz:allocate-define-declaration)
  ((signature jazz:get-define-declaration-signature ())
   (value     jazz:get-define-declaration-value     jazz:set-define-declaration-value)))


;;;
;;;; Define Special Form
;;;


(jazz:define-class-syntax jazz:Define-Special-Form-Declaration jazz:Declaration (constructor: jazz:allocate-define-special-form-declaration)
  ((signature jazz:get-define-special-form-signature jazz:set-define-special-form-signature)
   (body      jazz:get-define-special-form-body      jazz:set-define-special-form-body)))


;;;
;;;; Define Macro
;;;


(jazz:define-class-syntax jazz:Define-Macro-Declaration jazz:Declaration (constructor: jazz:allocate-define-macro-declaration)
  ((signature jazz:get-define-macro-signature jazz:set-define-macro-signature)
   (body      jazz:get-define-macro-body      jazz:set-define-macro-body)))


;;;
;;;; Lambda
;;;


(jazz:define-class-syntax jazz:Lambda jazz:Expression (constructor: jazz:allocate-lambda)
  ((signature jazz:get-lambda-signature ())
   (body      jazz:get-lambda-body      ())))


;;;
;;;; Let
;;;


(jazz:define-class-syntax jazz:Let jazz:Expression (constructor: jazz:allocate-let)
  ((bindings jazz:get-let-bindings ())
   (body     jazz:get-let-body     ())))


;;;
;;;; Named Let
;;;


(jazz:define-class-syntax jazz:Named-Let jazz:Let (constructor: jazz:allocate-named-let)
  ((variable jazz:get-named-let-variable ())))


;;;
;;;; Letstar
;;;


(jazz:define-class-syntax jazz:Letstar jazz:Expression (constructor: jazz:allocate-letstar)
  ((bindings jazz:get-letstar-bindings ())
   (body     jazz:get-letstar-body     ())))


;;;
;;;; Letrec
;;;


(jazz:define-class-syntax jazz:Letrec jazz:Expression (constructor: jazz:allocate-letrec)
  ((bindings jazz:get-letrec-bindings ())
   (body     jazz:get-letrec-body     ())))


;;;
;;;; Receive
;;;


(jazz:define-class-syntax jazz:Receive jazz:Expression (constructor: jazz:allocate-receive)
  ((variables  jazz:get-receive-variables  ())
   (expression jazz:get-receive-expression ())
   (body       jazz:get-receive-body       ())))


;;;
;;;; Do
;;;


(jazz:define-class-syntax jazz:Do jazz:Expression (constructor: jazz:allocate-do)
  ((bindings jazz:get-do-bindings ())
   (test     jazz:get-do-test     ())
   (result   jazz:get-do-result   ())
   (body     jazz:get-do-body     ())))


;;;
;;;; Reference Reification
;;;


(jazz:define-class-syntax jazz:Reference-Reification jazz:Expression (constructor: jazz:allocate-reference-reification)
  ((reference jazz:get-reference-reification-reference ())
   (resolver  jazz:get-reference-reification-resolver  ()))))
