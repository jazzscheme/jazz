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
;;;; Type
;;;


(jazz.define-class-syntax jazz.Type jazz.Object () () ()
  ())


(jazz.define-virtual-syntax (jazz.of-type? (jazz.Type type) object) #t)
(jazz.define-virtual-syntax (jazz.of-subtype? (jazz.Type type) subtype) #t)
(jazz.define-virtual-syntax (jazz.category-type? (jazz.Type type)) #t)
(jazz.define-virtual-syntax (jazz.emit-specifier (jazz.Type type)) #t)
(jazz.define-virtual-syntax (jazz.emit-type (jazz.Type type) source-declaration environment) #t)
(jazz.define-virtual-syntax (jazz.emit-test (jazz.Type type) value source-declaration environment) #t)
(jazz.define-virtual-syntax (jazz.emit-check (jazz.Type type) value source-declaration environment) #t)


(jazz.define-macro (%%subtype? target type)
  `(jazz.of-subtype? ,type ,target))


(jazz.define-macro (%%subcategory? target category)
  `(jazz.vector-memq? ,category (%%get-category-ancestors ,target)))


(jazz.define-macro (%%subclass? target class)
  `(jazz.vector-memq? ,class (%%get-category-ancestors ,target)))


(jazz.define-macro (%%is? object type)
  `(jazz.of-type? ,type ,object))


(jazz.define-macro (%%is-not? object type)
  `(%%not (%%is? ,object ,type)))


(jazz.define-macro (%%instance-of? object class)
  `(%%subclass? (%%class-of ,object) ,class))


;;;
;;;; Category
;;;


(jazz.define-class-syntax jazz.Category jazz.Type () () ()
  ((name         () ())
   (fields       () ())
   (virtual-size () ())
   (ancestors    () ())
   (descendants  () ())))


;;;
;;;; Class
;;;


(jazz.define-class-syntax jazz.Class jazz.Category (name fields virtual-size ancestors descendants) () jazz.allocate-class
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


(jazz.define-macro (%%class-subtype? target class)
  (jazz.with-expression-value target
    (lambda (trg)
      (jazz.with-expression-value class
        (lambda (cls)
          `(let ((class-level (%%get-class-level ,cls)))
             (and (%%fx>= (%%get-class-level ,trg) class-level)
                  (%%eq? (%%vector-ref (%%get-category-ancestors ,trg) class-level) ,cls))))))))


(jazz.define-macro (%%class-is? object class)
  `(%%class-subtype? (%%class-of ,object) ,class))


(jazz.define-macro (%%object-class? object)
  `(%%eq? ,object jazz.Object))


;;;
;;;; Object-Class
;;;


(jazz.define-class-syntax jazz.Object-Class jazz.Class (name fields virtual-size ancestors descendants ascendant interfaces slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) () ()
  ())


;;;
;;;; Primitive Classes
;;;


(jazz.define-class-syntax jazz.Boolean-Class   jazz.Class          (name fields virtual-size ancestors descendants ascendant interfaces slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class () ())
(jazz.define-class-syntax jazz.Char-Class      jazz.Class          (name fields virtual-size ancestors descendants ascendant interfaces slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class () ())
(jazz.define-class-syntax jazz.Numeric-Class   jazz.Class          (name fields virtual-size ancestors descendants ascendant interfaces slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class () ())
(jazz.define-class-syntax jazz.Number-Class    jazz.Numeric-Class  (name fields virtual-size ancestors descendants ascendant interfaces slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class () ())
(jazz.define-class-syntax jazz.Complex-Class   jazz.Number-Class   (name fields virtual-size ancestors descendants ascendant interfaces slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class () ())
(jazz.define-class-syntax jazz.Real-Class      jazz.Complex-Class  (name fields virtual-size ancestors descendants ascendant interfaces slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class () ())
(jazz.define-class-syntax jazz.Rational-Class  jazz.Real-Class     (name fields virtual-size ancestors descendants ascendant interfaces slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class () ())
(jazz.define-class-syntax jazz.Integer-Class   jazz.Rational-Class (name fields virtual-size ancestors descendants ascendant interfaces slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class () ())
(jazz.define-class-syntax jazz.Fixnum-Class    jazz.Integer-Class  (name fields virtual-size ancestors descendants ascendant interfaces slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class () ())
(jazz.define-class-syntax jazz.Flonum-Class    jazz.Real-Class     (name fields virtual-size ancestors descendants ascendant interfaces slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class () ())
(jazz.define-class-syntax jazz.Sequence-Class  jazz.Class          (name fields virtual-size ancestors descendants ascendant interfaces slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class () ())
(jazz.define-class-syntax jazz.List-Class      jazz.Sequence-Class (name fields virtual-size ancestors descendants ascendant interfaces slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class () ())
(jazz.define-class-syntax jazz.Null-Class      jazz.List-Class     (name fields virtual-size ancestors descendants ascendant interfaces slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class () ())
(jazz.define-class-syntax jazz.Pair-Class      jazz.List-Class     (name fields virtual-size ancestors descendants ascendant interfaces slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class () ())
(jazz.define-class-syntax jazz.String-Class    jazz.Sequence-Class (name fields virtual-size ancestors descendants ascendant interfaces slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class () ())
(jazz.define-class-syntax jazz.Vector-Class    jazz.Sequence-Class (name fields virtual-size ancestors descendants ascendant interfaces slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class () ())
(jazz.define-class-syntax jazz.Port-Class      jazz.Class          (name fields virtual-size ancestors descendants ascendant interfaces slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class () ())
(jazz.define-class-syntax jazz.Procedure-Class jazz.Class          (name fields virtual-size ancestors descendants ascendant interfaces slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class () ())
(jazz.define-class-syntax jazz.Foreign-Class   jazz.Class          (name fields virtual-size ancestors descendants ascendant interfaces slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class () ())
(jazz.define-class-syntax jazz.Symbol-Class    jazz.Class          (name fields virtual-size ancestors descendants ascendant interfaces slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class () ())
(jazz.define-class-syntax jazz.Keyword-Class   jazz.Class          (name fields virtual-size ancestors descendants ascendant interfaces slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class () ())
(jazz.define-class-syntax jazz.Hashtable-Class jazz.Class          (name fields virtual-size ancestors descendants ascendant interfaces slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class () ())


(jazz.define-class-syntax jazz.Boolean   jazz.Object   () jazz.Boolean-Class   () ())
(jazz.define-class-syntax jazz.Char      jazz.Object   () jazz.Char-Class      () ())
(jazz.define-class-syntax jazz.Numeric   jazz.Object   () jazz.Numeric-Class   () ())
(jazz.define-class-syntax jazz.Number    jazz.Numeric  () jazz.Number-Class    () ())
(jazz.define-class-syntax jazz.Complex   jazz.Number   () jazz.Complex-Class   () ())
(jazz.define-class-syntax jazz.Real      jazz.Complex  () jazz.Real-Class      () ())
(jazz.define-class-syntax jazz.Rational  jazz.Real     () jazz.Rational-Class  () ())
(jazz.define-class-syntax jazz.Integer   jazz.Rational () jazz.Integer-Class   () ())
(jazz.define-class-syntax jazz.Fixnum    jazz.Integer  () jazz.Fixnum-Class    () ())
(jazz.define-class-syntax jazz.Flonum    jazz.Real     () jazz.Flonum-Class    () ())
(jazz.define-class-syntax jazz.Sequence  jazz.Object   () jazz.Sequence-Class  () ())
(jazz.define-class-syntax jazz.List      jazz.Sequence () jazz.List-Class      () ())
(jazz.define-class-syntax jazz.Null      jazz.List     () jazz.Null-Class      () ())
(jazz.define-class-syntax jazz.Pair      jazz.List     () jazz.Pair-Class      () ())
(jazz.define-class-syntax jazz.String    jazz.Sequence () jazz.String-Class    () ())
(jazz.define-class-syntax jazz.Vector    jazz.Sequence () jazz.Vector-Class    () ())
(jazz.define-class-syntax jazz.Port      jazz.Object   () jazz.Port-Class      () ())
(jazz.define-class-syntax jazz.Procedure jazz.Object   () jazz.Procedure-Class () ())
(jazz.define-class-syntax jazz.Foreign   jazz.Object   () jazz.Foreign-Class   () ())
(jazz.define-class-syntax jazz.Symbol    jazz.Object   () jazz.Symbol-Class    () ())
(jazz.define-class-syntax jazz.Keyword   jazz.Object   () jazz.Keyword-Class   () ())
(jazz.define-class-syntax jazz.Hashtable jazz.Object   () jazz.Hashtable-Class () ())
(jazz.define-class-syntax jazz.Promise   jazz.Object   () jazz.Promise-Class   () ())


;;;
;;;; Interface
;;;


(jazz.define-class-syntax jazz.Interface jazz.Category (name fields virtual-size ancestors descendants) jazz.Object-Class jazz.allocate-interface
  ((ascendants %%get-interface-ascendants ())
   (rank       %%get-interface-rank       ())))


;;;
;;;; Field
;;;


(jazz.define-class-syntax jazz.Field jazz.Object () jazz.Object-Class ()
  ((name %%get-field-name ())))


(jazz.define-macro (%%get-category-field category field-name)
  `(%%hashtable-ref (%%get-category-fields ,category) ,field-name #f))


(jazz.define-macro (%%set-category-field category field-name field)
  `(%%hashtable-set! (%%get-category-fields ,category) ,field-name ,field))


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
  ((dispatch-type        %%get-method-dispatch-type        %%set-method-dispatch-type)
   (implementation       %%get-method-implementation       %%set-method-implementation)
   (implementation-tree  %%get-method-implementation-tree  %%set-method-implementation-tree)
   (category-rank        %%get-method-category-rank        %%set-method-category-rank)
   (implementation-rank  %%get-method-implementation-rank  %%set-method-implementation-rank)))


;;;
;;;; Method-Node
;;;


(jazz.define-class-syntax jazz.Method-Node jazz.Object () jazz.Object-Class jazz.allocate-method-node
  ((category            %%get-method-node-category            %%set-method-node-category)
   (implementation      %%get-method-node-implementation      %%set-method-node-implementation)
   (next-node           %%get-method-node-next-node           %%set-method-node-next-node)
   (next-implementation %%get-method-node-next-implementation %%set-method-node-next-implementation)
   (children            %%get-method-node-children            %%set-method-node-children)))


;;;
;;;; Queue
;;;


(jazz.define-class-syntax jazz.Queue jazz.Object () jazz.Object-Class jazz.allocate-queue
  ((list        %%get-queue-list        %%set-queue-list)
   (last-list   %%get-queue-last-list   %%set-queue-last-list)
   (last-anchor %%get-queue-last-anchor %%set-queue-last-anchor)
   (current     %%get-queue-current     %%set-queue-current))))
