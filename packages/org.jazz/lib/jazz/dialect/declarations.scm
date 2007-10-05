;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Jazz Declarations
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
;;;  Portions created by the Initial Developer are Copyright (C) 1996-2006
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


(module jazz.dialect.declarations


;;;
;;;; Definition
;;;


(jazz.define-class-syntax jazz.Definition-Declaration jazz.Declaration (name type access compatibility attributes toplevel parent children locator) jazz.Object-Class jazz.allocate-definition-declaration
  ((signature %%get-definition-declaration-signature ())
   (value     %%get-definition-declaration-value     %%set-definition-declaration-value)))


;;;
;;;; Generic
;;;


(jazz.define-class-syntax jazz.Generic-Declaration jazz.Declaration (name type access compatibility attributes toplevel parent children locator) jazz.Object-Class jazz.allocate-generic-declaration
  ;; the %%set is temp for a quicky
  ((signature     %%get-generic-declaration-signature     %%set-generic-declaration-signature)
   (dispatch-type %%get-generic-declaration-dispatch-type ())))


;;;
;;;; Specific
;;;


(jazz.define-class-syntax jazz.Specific-Declaration jazz.Declaration (name type access compatibility attributes toplevel parent children locator) jazz.Object-Class jazz.allocate-specific-declaration
  ((generic    %%get-specific-declaration-generic    ())
   (parameters %%get-specific-declaration-parameters ())
   (method?    %%get-specific-declaration-method?    ())
   (body       %%get-specific-declaration-body       %%set-specific-declaration-body)))


;;;
;;;; Unit
;;;


(jazz.define-class-syntax jazz.Unit-Declaration jazz.Namespace-Declaration (name type access compatibility attributes toplevel parent children locator lookups body) jazz.Object-Class ()
  ((metaclass %%get-unit-declaration-metaclass ())))


;;;
;;;; Class
;;;


(jazz.define-class-syntax jazz.Class-Declaration jazz.Unit-Declaration (name type access compatibility attributes toplevel parent children locator lookups body metaclass) jazz.Object-Class jazz.allocate-class-declaration
  ((ascendant  %%get-class-declaration-ascendant  ())
   (interfaces %%get-class-declaration-interfaces ())))


;;;
;;;; Interface
;;;


(jazz.define-class-syntax jazz.Interface-Declaration jazz.Unit-Declaration (name type access compatibility attributes toplevel parent children locator lookups body metaclass) jazz.Object-Class jazz.allocate-interface-declaration
  ((ascendants %%get-interface-declaration-ascendants ())))


;;;
;;;; Field
;;;


(jazz.define-class-syntax jazz.Field-Declaration jazz.Declaration (name type access compatibility attributes toplevel parent children locator) jazz.Object-Class ()
  ())


;;;
;;;; Slot
;;;


(jazz.define-class-syntax jazz.Slot-Declaration jazz.Field-Declaration (name type access compatibility attributes toplevel parent children locator) jazz.Object-Class jazz.allocate-slot-declaration
  ((initialize  %%get-slot-declaration-initialize  %%set-slot-declaration-initialize)
   (getter-name %%get-slot-declaration-getter-name ())
   (setter-name %%get-slot-declaration-setter-name ())))


;;;
;;;; Property
;;;


(jazz.define-class-syntax jazz.Property-Declaration jazz.Slot-Declaration (name type access compatibility attributes toplevel parent children locator initialize getter-name setter-name) jazz.Object-Class jazz.allocate-property-declaration
  ((getter %%get-property-declaration-getter %%set-property-declaration-getter)
   (setter %%get-property-declaration-setter %%set-property-declaration-setter)))


;;;
;;;; Method
;;;


(jazz.define-class-syntax jazz.Method-Declaration jazz.Field-Declaration (name type access compatibility attributes toplevel parent children locator) jazz.Object-Class jazz.allocate-method-declaration
  ((propagation    %%get-method-declaration-propagation    ())
   (implementation %%get-method-declaration-implementation ())
   (expansion      %%get-method-declaration-expansion      ())
   (parameters     %%get-method-declaration-parameters     ())))


;;;
;;;; With Self
;;;


(jazz.define-class-syntax jazz.With-Self jazz.Expression (type) jazz.Object-Class jazz.allocate-with-self
  ((body %%get-with-self-body ())))


;;;
;;;; Slot Reference
;;;


(jazz.define-class-syntax jazz.Slot-Reference jazz.Expression (type) jazz.Object-Class jazz.allocate-slot-reference
  ((declaration %%get-slot-reference-declaration ())
   (name        %%get-slot-reference-name        ())
   (context     %%get-slot-reference-context     ())))


;;;
;;;; Slot Assignment
;;;


(jazz.define-class-syntax jazz.Slot-Assignment jazz.Expression (type) jazz.Object-Class jazz.allocate-slot-assignment
  ((declaration %%get-slot-assignment-declaration ())
   (name        %%get-slot-assignment-name        ())
   (context     %%get-slot-assignment-context     ())
   (value       %%get-slot-assignment-value       ())))


;;;
;;;; Dispatch
;;;


(jazz.define-class-syntax jazz.Dispatch jazz.Expression (type) jazz.Object-Class jazz.allocate-dispatch
  ((name      %%get-dispatch-name      ())
   (arguments %%get-dispatch-arguments ()))))
