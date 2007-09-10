;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Classes
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


(module core.class.syntax.classes


;;;
;;;; Object
;;;


(jazz.define-class-syntax jazz.Object () () () ()
  ())


;;;
;;;; Unit
;;;


(jazz.define-class-syntax jazz.Unit jazz.Object () () ()
  ((name        () ())
   (fields      () ())
   (ancestors   () ())
   (descendants () ())))


;;;
;;;; Class
;;;


(jazz.define-class-syntax jazz.Class jazz.Unit (name fields ancestors descendants) () jazz.allocate-class
  ((ascendant          () ())
   (interfaces         () ())
   (slots              () ())
   (instance-size      () ())
   (level              () ())
   (dispatch-table     () ())
   (core-method-alist  () ())
   (core-virtual-alist () ())
   (core-virtual-names () ())
   (core-vtable        () ())
   (class-table        () ())
   (interface-table    () ())))


;;;
;;;; Object-Class
;;;


(jazz.define-class-syntax jazz.Object-Class jazz.Class (name fields ancestors descendants ascendant interfaces slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) () ()
  ())


;;;
;;;; Primitive Classes
;;;


(jazz.define-class-syntax jazz.Number-Class   jazz.Object-Class   (name fields ancestors descendants ascendant interfaces slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class () ())
(jazz.define-class-syntax jazz.Integer-Class  jazz.Number-Class   (name fields ancestors descendants ascendant interfaces slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class () ())
(jazz.define-class-syntax jazz.Real-Class     jazz.Number-Class   (name fields ancestors descendants ascendant interfaces slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class () ())
(jazz.define-class-syntax jazz.Sequence-Class jazz.Object-Class   (name fields ancestors descendants ascendant interfaces slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class () ())
(jazz.define-class-syntax jazz.List-Class     jazz.Sequence-Class (name fields ancestors descendants ascendant interfaces slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class () ())
(jazz.define-class-syntax jazz.String-Class   jazz.Sequence-Class (name fields ancestors descendants ascendant interfaces slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class () ())
(jazz.define-class-syntax jazz.Vector-Class   jazz.Sequence-Class (name fields ancestors descendants ascendant interfaces slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class () ())


(jazz.define-class-syntax jazz.Boolean   jazz.Object   () jazz.Object-Class   () ())
(jazz.define-class-syntax jazz.Char      jazz.Object   () jazz.Object-Class   () ())
(jazz.define-class-syntax jazz.Number    jazz.Object   () jazz.Number-Class   () ())
(jazz.define-class-syntax jazz.Integer   jazz.Number   () jazz.Integer-Class  () ())
(jazz.define-class-syntax jazz.Real      jazz.Number   () jazz.Real-Class     () ())
(jazz.define-class-syntax jazz.Sequence  jazz.Object   () jazz.Sequence-Class () ())
(jazz.define-class-syntax jazz.List      jazz.Sequence () jazz.List-Class     () ())
(jazz.define-class-syntax jazz.Null      jazz.List     () jazz.List-Class     () ())
(jazz.define-class-syntax jazz.Pair      jazz.List     () jazz.List-Class     () ())
(jazz.define-class-syntax jazz.Port      jazz.Object   () jazz.Object-Class   () ())
(jazz.define-class-syntax jazz.Procedure jazz.Object   () jazz.Object-Class   () ())
(jazz.define-class-syntax jazz.String    jazz.Sequence () jazz.String-Class   () ())
(jazz.define-class-syntax jazz.Symbol    jazz.Object   () jazz.Object-Class   () ())
(jazz.define-class-syntax jazz.Keyword   jazz.Object   () jazz.Object-Class   () ())
(jazz.define-class-syntax jazz.Vector    jazz.Sequence () jazz.Vector-Class   () ())
(jazz.define-class-syntax jazz.Hashtable jazz.Object   () jazz.Object-Class   () ())


;;;
;;;; Interface
;;;


(jazz.define-class-syntax jazz.Interface jazz.Unit (name fields ancestors descendants) jazz.Object-Class jazz.allocate-interface
  ((ascendants () ())
   (rank       () ())))


;;;
;;;; Field
;;;


(jazz.define-class-syntax jazz.Field jazz.Object () jazz.Object-Class ()
  ((name %%get-field-name ())))


(jazz.define-macro (%%get-unit-field unit field-name)
  `(%%hashtable-ref (%%get-unit-fields ,unit) ,field-name #f))


(jazz.define-macro (%%set-unit-field unit field-name field)
  `(%%hashtable-set! (%%get-unit-fields ,unit) ,field-name ,field))


;;;
;;;; Slot
;;;


(jazz.define-class-syntax jazz.Slot jazz.Field (name) jazz.Object-Class jazz.allocate-slot
  ((rank       %%get-slot-rank       ())
   (initialize %%get-slot-initialize ())))


;;;
;;;; Property
;;;


(jazz.define-class-syntax jazz.Property jazz.Slot (name rank initialize) jazz.Object-class jazz.allocate-property
  ((getter %%get-property-getter ())
   (setter %%get-property-setter ())))


;;;
;;;; Method
;;;


(jazz.define-class-syntax jazz.Method jazz.Field (name) jazz.Object-Class jazz.allocate-method
  ((propagation    %%get-method-propagation    ())
   (implementation %%get-method-implementation ())))


;;;
;;;; Nil
;;;


(jazz.define-class-syntax jazz.Nil jazz.Object () jazz.Object-Class jazz.allocate-nil
  ())


;;;
;;;; Queue
;;;


(jazz.define-class-syntax jazz.Queue jazz.Object () jazz.Object-Class jazz.allocate-queue
  ((list        %%get-queue-list        %%set-queue-list)
   (last-list   %%get-queue-last-list   %%set-queue-last-list)
   (last-anchor %%get-queue-last-anchor %%set-queue-last-anchor)
   (current     %%get-queue-current     %%set-queue-current))))
