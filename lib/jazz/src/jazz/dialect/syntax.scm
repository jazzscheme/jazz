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


(jazz:define-class-syntax jazz:Definition-Declaration jazz:Declaration (constructor: jazz:allocate-definition-declaration)
  ((expansion jazz:get-definition-declaration-expansion ())
   (signature jazz:get-definition-declaration-signature jazz:set-definition-declaration-signature)
   (value     jazz:get-definition-declaration-value     jazz:set-definition-declaration-value)))


;;;
;;;; Specialize
;;;


(jazz:define-class-syntax jazz:Specialize jazz:Expression (constructor: jazz:allocate-specialize)
  ())


;;;
;;;; Generic
;;;


(jazz:define-class-syntax jazz:Generic-Declaration jazz:Declaration (constructor: jazz:allocate-generic-declaration)
  ((dispatch-types jazz:get-generic-declaration-dispatch-types ())
   (signature      jazz:get-generic-declaration-signature      jazz:set-generic-declaration-signature)
   (body           jazz:get-generic-declaration-body           jazz:set-generic-declaration-body)))


;;;
;;;; Specific
;;;


(jazz:define-class-syntax jazz:Specific-Declaration jazz:Declaration (constructor: jazz:allocate-specific-declaration)
  ((generic   jazz:get-specific-declaration-generic   ())
   (signature jazz:get-specific-declaration-signature ())
   (body      jazz:get-specific-declaration-body      jazz:set-specific-declaration-body)
   (root?     jazz:get-specific-declaration-root?     ())))


;;;
;;;; Category
;;;


(jazz:define-class-syntax jazz:Category-Declaration jazz:Namespace-Declaration ()
  ((implementor jazz:get-category-declaration-implementor ())
   (metaclass   jazz:get-category-declaration-metaclass   ())))


;;;
;;;; Class
;;;


(jazz:define-class-syntax jazz:Class-Declaration jazz:Category-Declaration (constructor: jazz:allocate-class-declaration)
  ((ascendant          jazz:get-class-declaration-ascendant          ())
   (ascendant-relation jazz:get-class-declaration-ascendant-relation ())
   (ascendant-base     jazz:get-class-declaration-ascendant-base     ())
   (interfaces         jazz:get-class-declaration-interfaces         ())))


;;;
;;;; Interface
;;;


(jazz:define-class-syntax jazz:Interface-Declaration jazz:Category-Declaration (constructor: jazz:allocate-interface-declaration)
  ((ascendants jazz:get-interface-declaration-ascendants ())))


;;;
;;;; Field
;;;


(jazz:define-class-syntax jazz:Field-Declaration jazz:Declaration ()
  ())


;;;
;;;; Slot
;;;


(jazz:define-class-syntax jazz:Slot-Declaration jazz:Field-Declaration (constructor: jazz:allocate-slot-declaration)
  ((initialize  jazz:get-slot-declaration-initialize  jazz:set-slot-declaration-initialize)
   (getter-name jazz:get-slot-declaration-getter-name ())
   (setter-name jazz:get-slot-declaration-setter-name ())))


;;;
;;;; Property
;;;


(jazz:define-class-syntax jazz:Property-Declaration jazz:Slot-Declaration (constructor: jazz:allocate-property-declaration)
  ((getter jazz:get-property-declaration-getter jazz:set-property-declaration-getter)
   (setter jazz:get-property-declaration-setter jazz:set-property-declaration-setter)))


;;;
;;;; Method
;;;


(jazz:define-class-syntax jazz:Method-Declaration jazz:Field-Declaration (constructor: jazz:allocate-method-declaration)
  ((root         jazz:get-method-declaration-root         ())
   (propagation  jazz:get-method-declaration-propagation  ())
   (abstraction  jazz:get-method-declaration-abstraction  ())
   (expansion    jazz:get-method-declaration-expansion    ())
   (remote       jazz:get-method-declaration-remote       ())
   (synchronized jazz:get-method-declaration-synchronized ())
   (signature    jazz:get-method-declaration-signature    jazz:set-method-declaration-signature)
   (body         jazz:get-method-declaration-body         jazz:set-method-declaration-body)))


;;;
;;;; Method Node Reference
;;;


(jazz:define-class-syntax jazz:Method-Node-Reference jazz:Binding-Reference (constructor: jazz:allocate-method-node-reference)
  ())


;;;
;;;; NextMethod Variable
;;;


(jazz:define-class-syntax jazz:NextMethod-Variable jazz:Variable (constructor: jazz:allocate-nextmethod-variable)
  ())


;;;
;;;; Self Binding
;;;


(jazz:define-class-syntax jazz:Self-Binding jazz:Lexical-Binding (constructor: jazz:allocate-self-binding)
  ())


;;;
;;;; Dynamic Self Binding
;;;


(jazz:define-class-syntax jazz:Dynamic-Self-Binding jazz:Lexical-Binding (constructor: jazz:allocate-dynamic-self-binding)
  ((code jazz:get-dynamic-self-binding-code ())))


;;;
;;;; With Self
;;;


(jazz:define-class-syntax jazz:With-Self jazz:Expression (constructor: jazz:allocate-with-self)
  ((body jazz:get-with-self-body ())))


;;;
;;;; With Dynamic Self
;;;


(jazz:define-class-syntax jazz:With-Dynamic-Self jazz:Expression (constructor: jazz:allocate-with-dynamic-self)
  ((code jazz:get-with-dynamic-self-code ())
   (body jazz:get-with-dynamic-self-body ())))


;;;
;;;; Cast
;;;


(jazz:define-class-syntax jazz:Cast jazz:Expression (constructor: jazz:allocate-cast)
  ((expression jazz:get-cast-expression ())))


;;;
;;;; Allocate
;;;


(jazz:define-class-syntax jazz:Allocate jazz:Expression (constructor: jazz:allocate-allocate)
  ((class  jazz:get-allocate-class  ())
   (values jazz:get-allocate-values ())))


;;;
;;;; Dispatch
;;;


(jazz:define-class-syntax jazz:Dispatch jazz:Expression (constructor: jazz:allocate-dispatch)
  ((name      jazz:get-dispatch-name      ())
   (arguments jazz:get-dispatch-arguments ())))


;;;
;;;; Dialect
;;;


(jazz:define-class-syntax jazz:Jazz-Dialect jazz:Dialect (constructor: jazz:allocate-jazz-dialect)
  ())


;;;
;;;; Walker
;;;


(jazz:define-class-syntax jazz:Jazz-Walker jazz:Scheme-Walker (constructor: jazz:allocate-jazz-walker)
  ()))
