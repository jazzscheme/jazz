;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Jazz Dialect Syntax
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


(unit jazz.dialect.syntax


;;;
;;;; Definition
;;;


(jazz:define-class jazz:Definition-Declaration jazz:Declaration (constructor: jazz:allocate-definition-declaration)
  ((expansion %%get-definition-declaration-expansion ())
   (signature %%get-definition-declaration-signature %%set-definition-declaration-signature)
   (value     %%get-definition-declaration-value     %%set-definition-declaration-value)))


;;;
;;;; Specialize
;;;


(jazz:define-class jazz:Specialize jazz:Expression (constructor: jazz:allocate-specialize)
  ())


;;;
;;;; Generic
;;;


(jazz:define-class jazz:Generic-Declaration jazz:Declaration (constructor: jazz:allocate-generic-declaration)
  ((dispatch-types %%get-generic-declaration-dispatch-types ())
   (signature      %%get-generic-declaration-signature      %%set-generic-declaration-signature)
   (body           %%get-generic-declaration-body           %%set-generic-declaration-body)))


;;;
;;;; Specific
;;;


(jazz:define-class jazz:Specific-Declaration jazz:Declaration (constructor: jazz:allocate-specific-declaration)
  ((generic   %%get-specific-declaration-generic   ())
   (signature %%get-specific-declaration-signature ())
   (body      %%get-specific-declaration-body      %%set-specific-declaration-body)
   (root?     %%get-specific-declaration-root?     ())))


;;;
;;;; Category
;;;


(jazz:define-class jazz:Category-Declaration jazz:Namespace-Declaration ()
  ((implementor %%get-category-declaration-implementor ())
   (metaclass   %%get-category-declaration-metaclass   ())))


;;;
;;;; Class
;;;


(jazz:define-class jazz:Class-Declaration jazz:Category-Declaration (constructor: jazz:allocate-class-declaration)
  ((ascendant          %%get-class-declaration-ascendant          ())
   (ascendant-relation %%get-class-declaration-ascendant-relation ())
   (ascendant-base     %%get-class-declaration-ascendant-base     ())
   (interfaces         %%get-class-declaration-interfaces         ())))


;;;
;;;; Interface
;;;


(jazz:define-class jazz:Interface-Declaration jazz:Category-Declaration (constructor: jazz:allocate-interface-declaration)
  ((ascendants %%get-interface-declaration-ascendants ())))


;;;
;;;; Field
;;;


(jazz:define-class jazz:Field-Declaration jazz:Declaration ()
  ())


;;;
;;;; Slot
;;;


(jazz:define-class jazz:Slot-Declaration jazz:Field-Declaration (constructor: jazz:allocate-slot-declaration)
  ((initialize  %%get-slot-declaration-initialize  %%set-slot-declaration-initialize)
   (getter-name %%get-slot-declaration-getter-name ())
   (setter-name %%get-slot-declaration-setter-name ())))


;;;
;;;; Property
;;;


(jazz:define-class jazz:Property-Declaration jazz:Slot-Declaration (constructor: jazz:allocate-property-declaration)
  ((getter %%get-property-declaration-getter %%set-property-declaration-getter)
   (setter %%get-property-declaration-setter %%set-property-declaration-setter)))


;;;
;;;; Method
;;;


(jazz:define-class jazz:Method-Declaration jazz:Field-Declaration (constructor: jazz:allocate-method-declaration)
  ((root         %%get-method-declaration-root         ())
   (propagation  %%get-method-declaration-propagation  ())
   (abstraction  %%get-method-declaration-abstraction  ())
   (expansion    %%get-method-declaration-expansion    ())
   (remote       %%get-method-declaration-remote       ())
   (synchronized %%get-method-declaration-synchronized ())
   (signature    %%get-method-declaration-signature    %%set-method-declaration-signature)
   (body         %%get-method-declaration-body         %%set-method-declaration-body)))


;;;
;;;; Method Node Reference
;;;


(jazz:define-class jazz:Method-Node-Reference jazz:Binding-Reference (constructor: jazz:allocate-method-node-reference)
  ())


;;;
;;;; NextMethod Variable
;;;


(jazz:define-class jazz:NextMethod-Variable jazz:Variable (constructor: jazz:allocate-nextmethod-variable)
  ())


;;;
;;;; Self Binding
;;;


(jazz:define-class jazz:Self-Binding jazz:Lexical-Binding (constructor: jazz:allocate-self-binding)
  ())


;;;
;;;; Dynamic Self Binding
;;;


(jazz:define-class jazz:Dynamic-Self-Binding jazz:Lexical-Binding (constructor: jazz:allocate-dynamic-self-binding)
  ((code %%get-dynamic-self-binding-code ())))


;;;
;;;; With Self
;;;


(jazz:define-class jazz:With-Self jazz:Expression (constructor: jazz:allocate-with-self)
  ((body %%get-with-self-body ())))


;;;
;;;; With Dynamic Self
;;;


(jazz:define-class jazz:With-Dynamic-Self jazz:Expression (constructor: jazz:allocate-with-dynamic-self)
  ((code %%get-with-dynamic-self-code ())
   (body %%get-with-dynamic-self-body ())))


;;;
;;;; Cast
;;;


(jazz:define-class jazz:Cast jazz:Expression (constructor: jazz:allocate-cast)
  ((expression %%get-cast-expression ())))


;;;
;;;; Allocate
;;;


(jazz:define-class jazz:Allocate jazz:Expression (constructor: jazz:allocate-allocate)
  ((class  %%get-allocate-class  ())
   (values %%get-allocate-values ())))


;;;
;;;; Dispatch
;;;


(jazz:define-class jazz:Dispatch jazz:Expression (constructor: jazz:allocate-dispatch)
  ((name      %%get-dispatch-name      ())
   (arguments %%get-dispatch-arguments ())))


;;;
;;;; Dialect
;;;


(jazz:define-class jazz:Jazz-Dialect jazz:Dialect (constructor: jazz:allocate-jazz-dialect)
  ())


;;;
;;;; Walker
;;;


(jazz:define-class jazz:Jazz-Walker jazz:Scheme-Walker (constructor: jazz:allocate-jazz-walker)
  ()))
