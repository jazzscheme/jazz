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


(jazz:define-class jazz:Scheme-Dialect jazz:Dialect (name declarations bindings) jazz:Object-Class jazz:allocate-scheme-dialect
  ())


;;;
;;;; Walker
;;;


(jazz:define-class jazz:Scheme-Walker jazz:Walker (declarations bindings warnings errors literals variables references autoloads) jazz:Object-Class jazz:allocate-scheme-walker
  ())


;;;
;;;; Define
;;;


(jazz:define-class jazz:Define-Declaration jazz:Declaration (name type hits access compatibility attributes toplevel parent locator source) jazz:Object-Class jazz:allocate-define-declaration
  ((signature %%get-define-declaration-signature ())
   (value     %%get-define-declaration-value     %%set-define-declaration-value)))


;;;
;;;; Define Special Form
;;;


(jazz:define-class jazz:Define-Special-Form-Declaration jazz:Declaration (name type hits access compatibility attributes toplevel parent locator source) jazz:Object-Class jazz:allocate-define-special-form-declaration
  ((signature %%get-define-special-form-signature %%set-define-special-form-signature)
   (body      %%get-define-special-form-body      %%set-define-special-form-body)))


;;;
;;;; Define Macro
;;;


(jazz:define-class jazz:Define-Macro-Declaration jazz:Declaration (name type hits access compatibility attributes toplevel parent locator source) jazz:Object-Class jazz:allocate-define-macro-declaration
  ((signature %%get-define-macro-signature %%set-define-macro-signature)
   (body      %%get-define-macro-body      %%set-define-macro-body)))


;;;
;;;; Lambda
;;;


(jazz:define-class jazz:Lambda jazz:Expression (type source) jazz:Object-Class jazz:allocate-lambda
  ((signature %%get-lambda-signature ())
   (body      %%get-lambda-body      ())))


;;;
;;;; Let
;;;


(jazz:define-class jazz:Let jazz:Expression (type source) jazz:Object-Class jazz:allocate-let
  ((bindings %%get-let-bindings ())
   (body     %%get-let-body     ())))


;;;
;;;; Named Let
;;;


(jazz:define-class jazz:Named-Let jazz:Let (type source bindings body) jazz:Object-Class jazz:allocate-named-let
  ((variable %%get-named-let-variable ())))


;;;
;;;; Letstar
;;;


(jazz:define-class jazz:Letstar jazz:Expression (type source) jazz:Object-Class jazz:allocate-letstar
  ((bindings %%get-letstar-bindings ())
   (body     %%get-letstar-body     ())))


;;;
;;;; Letrec
;;;


(jazz:define-class jazz:Letrec jazz:Expression (type source) jazz:Object-Class jazz:allocate-letrec
  ((bindings %%get-letrec-bindings ())
   (body     %%get-letrec-body     ())))


;;;
;;;; Receive
;;;


(jazz:define-class jazz:Receive jazz:Expression (type source) jazz:Object-Class jazz:allocate-receive
  ((variables  %%get-receive-variables  ())
   (expression %%get-receive-expression ())
   (body       %%get-receive-body       ())))


;;;
;;;; Do
;;;


(jazz:define-class jazz:Do jazz:Expression (type source) jazz:Object-Class jazz:allocate-do
  ((bindings %%get-do-bindings ())
   (test     %%get-do-test     ())
   (result   %%get-do-result   ())
   (body     %%get-do-body     ())))


;;;
;;;; Reference Reification
;;;


(jazz:define-class jazz:Reference-Reification jazz:Expression (type source) jazz:Object-Class jazz:allocate-reference-reification
  ((reference %%get-reference-reification-reference ())
   (resolver  %%get-reference-reification-resolver  ()))))
