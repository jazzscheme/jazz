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
  ((expansion getter: generate)
   (signature getter: generate setter: generate)
   (value     getter: generate setter: generate)))


;;;
;;;; Specialize
;;;


(jazz:define-class-syntax jazz:Specialize jazz:Expression (constructor: jazz:allocate-specialize)
  ())


;;;
;;;; Generic
;;;


(jazz:define-class-syntax jazz:Generic-Declaration jazz:Declaration (constructor: jazz:allocate-generic-declaration)
  ((dispatch-types getter: generate)
   (signature      getter: generate setter: generate)
   (body           getter: generate setter: generate)))


;;;
;;;; Specific
;;;


(jazz:define-class-syntax jazz:Specific-Declaration jazz:Declaration (constructor: jazz:allocate-specific-declaration)
  ((generic   getter: generate)
   (signature getter: generate)
   (body      getter: generate setter: generate)
   (root?     getter: generate)))


;;;
;;;; Category
;;;


(jazz:define-class-syntax jazz:Category-Declaration jazz:Namespace-Declaration ()
  ((implementor getter: generate)
   (metaclass   getter: generate)))


;;;
;;;; Class
;;;


(jazz:define-class-syntax jazz:Class-Declaration jazz:Category-Declaration (constructor: jazz:allocate-class-declaration)
  ((ascendant          getter: generate)
   (ascendant-relation getter: generate)
   (ascendant-base     getter: generate)
   (interfaces         getter: generate)))


;;;
;;;; Interface
;;;


(jazz:define-class-syntax jazz:Interface-Declaration jazz:Category-Declaration (constructor: jazz:allocate-interface-declaration)
  ((ascendants getter: generate)))


;;;
;;;; Field
;;;


(jazz:define-class-syntax jazz:Field-Declaration jazz:Declaration ()
  ())


;;;
;;;; Slot
;;;


(jazz:define-class-syntax jazz:Slot-Declaration jazz:Field-Declaration (constructor: jazz:allocate-slot-declaration)
  ((initialize  getter: generate setter: generate)
   (getter-name getter: generate)
   (setter-name getter: generate)))


;;;
;;;; Property
;;;


(jazz:define-class-syntax jazz:Property-Declaration jazz:Slot-Declaration (constructor: jazz:allocate-property-declaration)
  ((getter getter: generate setter: generate)
   (setter getter: generate setter: generate)))


;;;
;;;; Method
;;;


(jazz:define-class-syntax jazz:Method-Declaration jazz:Field-Declaration (constructor: jazz:allocate-method-declaration)
  ((root         getter: generate)
   (propagation  getter: generate)
   (abstraction  getter: generate)
   (expansion    getter: generate)
   (remote       getter: generate)
   (synchronized getter: generate)
   (signature    getter: generate setter: generate)
   (body         getter: generate setter: generate)))


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
  ((code getter: generate)))


;;;
;;;; With Self
;;;


(jazz:define-class-syntax jazz:With-Self jazz:Expression (constructor: jazz:allocate-with-self)
  ((body getter: generate)))


;;;
;;;; With Dynamic Self
;;;


(jazz:define-class-syntax jazz:With-Dynamic-Self jazz:Expression (constructor: jazz:allocate-with-dynamic-self)
  ((code getter: generate)
   (body getter: generate)))


;;;
;;;; Cast
;;;


(jazz:define-class-syntax jazz:Cast jazz:Expression (constructor: jazz:allocate-cast)
  ((expression getter: generate)))


;;;
;;;; Allocate
;;;


(jazz:define-class-syntax jazz:Allocate jazz:Expression (constructor: jazz:allocate-allocate)
  ((class  getter: generate)
   (values getter: generate)))


;;;
;;;; Dispatch
;;;


(jazz:define-class-syntax jazz:Dispatch jazz:Expression (constructor: jazz:allocate-dispatch)
  ((name      getter: generate)
   (arguments getter: generate)))


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
