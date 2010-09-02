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


(unit protected core.class.syntax.classes


;;;
;;;; Object
;;;


(jazz.define-class jazz.Object () () () ()
  ())


(jazz.define-virtual (jazz.print-object (jazz.Object object) output detail))
(jazz.define-virtual (jazz.tree-fold (jazz.Object object) down up here seed environment))


;;;
;;;; Type
;;;


(jazz.define-class jazz.Type jazz.Object () () ()
  ())


(jazz.define-virtual (jazz.of-type? (jazz.Type type) object) #t)
(jazz.define-virtual (jazz.of-subtype? (jazz.Type type) subtype) #t)
(jazz.define-virtual (jazz.category-type? (jazz.Type type)) #t)
(jazz.define-virtual (jazz.emit-specifier (jazz.Type type)) #t)
(jazz.define-virtual (jazz.emit-type (jazz.Type type) source-declaration environment) #t)
(jazz.define-virtual (jazz.emit-test (jazz.Type type) value source-declaration environment) #t)
(jazz.define-virtual (jazz.emit-check (jazz.Type type) value source-declaration environment) #t)


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


;;;
;;;; Category
;;;


(jazz.define-class jazz.Category jazz.Type () () ()
  ((name         () ())
   (fields       () ())
   (virtual-size () ())
   (ancestors    () ())
   (descendants  () ())))


(jazz.define-virtual (jazz.update-category (jazz.Category category)))


;;;
;;;; Class
;;;


(jazz.define-class jazz.Class jazz.Category (name fields virtual-size ancestors descendants) () jazz.allocate-class
  ((ascendant          () ())
   (interfaces         () ())
   (slots              () ())
   (instance-slots     () ())
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
  (jazz.with-uniqueness target
    (lambda (trg)
      (jazz.with-uniqueness class
        (lambda (cls)
          `(let ((class-level (%%get-class-level ,cls)))
             (and (%%fx>= (%%get-class-level ,trg) class-level)
                  (%%eq? (%%vector-ref (%%get-category-ancestors ,trg) class-level) ,cls))))))))


(jazz.define-macro (%%class-is? object class)
  `(%%class-subtype? (jazz.class-of ,object) ,class))


(jazz.define-macro (%%category-is? object category)
  `(%%is? ,object ,category))


(jazz.define-macro (%%class? object)
  `(%%class-is? ,object jazz.Class))


(jazz.define-macro (%%object-class? object)
  `(%%eq? ,object jazz.Object))


;;;
;;;; Object-Class
;;;


(jazz.define-class jazz.Object-Class jazz.Class (name fields virtual-size ancestors descendants ascendant interfaces slots instance-slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) () ()
  ())


;;;
;;;; Primitive Classes
;;;


(jazz.define-class jazz.Boolean-Class      jazz.Class          (name fields virtual-size ancestors descendants ascendant interfaces slots instance-slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class () ())
(jazz.define-class jazz.Char-Class         jazz.Class          (name fields virtual-size ancestors descendants ascendant interfaces slots instance-slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class () ())
(jazz.define-class jazz.Numeric-Class      jazz.Class          (name fields virtual-size ancestors descendants ascendant interfaces slots instance-slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class () ())
(jazz.define-class jazz.Number-Class       jazz.Numeric-Class  (name fields virtual-size ancestors descendants ascendant interfaces slots instance-slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class () ())
(jazz.define-class jazz.Complex-Class      jazz.Number-Class   (name fields virtual-size ancestors descendants ascendant interfaces slots instance-slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class () ())
(jazz.define-class jazz.Real-Class         jazz.Complex-Class  (name fields virtual-size ancestors descendants ascendant interfaces slots instance-slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class () ())
(jazz.define-class jazz.Rational-Class     jazz.Real-Class     (name fields virtual-size ancestors descendants ascendant interfaces slots instance-slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class () ())
(jazz.define-class jazz.Integer-Class      jazz.Rational-Class (name fields virtual-size ancestors descendants ascendant interfaces slots instance-slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class () ())
(jazz.define-class jazz.Fixnum-Class       jazz.Integer-Class  (name fields virtual-size ancestors descendants ascendant interfaces slots instance-slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class () ())
(jazz.define-class jazz.Flonum-Class       jazz.Real-Class     (name fields virtual-size ancestors descendants ascendant interfaces slots instance-slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class () ())
(jazz.define-class jazz.Sequence-Class     jazz.Class          (name fields virtual-size ancestors descendants ascendant interfaces slots instance-slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class () ())
(jazz.define-class jazz.List-Class         jazz.Sequence-Class (name fields virtual-size ancestors descendants ascendant interfaces slots instance-slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class () ())
(jazz.define-class jazz.Null-Class         jazz.List-Class     (name fields virtual-size ancestors descendants ascendant interfaces slots instance-slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class () ())
(jazz.define-class jazz.Pair-Class         jazz.List-Class     (name fields virtual-size ancestors descendants ascendant interfaces slots instance-slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class () ())
(jazz.define-class jazz.String-Class       jazz.Sequence-Class (name fields virtual-size ancestors descendants ascendant interfaces slots instance-slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class () ())
(jazz.define-class jazz.Vector-Class       jazz.Sequence-Class (name fields virtual-size ancestors descendants ascendant interfaces slots instance-slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class () ())
(jazz.define-class jazz.S8Vector-Class     jazz.Sequence-Class (name fields virtual-size ancestors descendants ascendant interfaces slots instance-slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class () ())
(jazz.define-class jazz.U8Vector-Class     jazz.Sequence-Class (name fields virtual-size ancestors descendants ascendant interfaces slots instance-slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class () ())
(jazz.define-class jazz.S16Vector-Class    jazz.Sequence-Class (name fields virtual-size ancestors descendants ascendant interfaces slots instance-slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class () ())
(jazz.define-class jazz.U16Vector-Class    jazz.Sequence-Class (name fields virtual-size ancestors descendants ascendant interfaces slots instance-slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class () ())
(jazz.define-class jazz.S32Vector-Class    jazz.Sequence-Class (name fields virtual-size ancestors descendants ascendant interfaces slots instance-slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class () ())
(jazz.define-class jazz.U32Vector-Class    jazz.Sequence-Class (name fields virtual-size ancestors descendants ascendant interfaces slots instance-slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class () ())
(jazz.define-class jazz.S64Vector-Class    jazz.Sequence-Class (name fields virtual-size ancestors descendants ascendant interfaces slots instance-slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class () ())
(jazz.define-class jazz.U64Vector-Class    jazz.Sequence-Class (name fields virtual-size ancestors descendants ascendant interfaces slots instance-slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class () ())
(jazz.define-class jazz.F32Vector-Class    jazz.Sequence-Class (name fields virtual-size ancestors descendants ascendant interfaces slots instance-slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class () ())
(jazz.define-class jazz.F64Vector-Class    jazz.Sequence-Class (name fields virtual-size ancestors descendants ascendant interfaces slots instance-slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class () ())
(jazz.define-class jazz.Structure-Class    jazz.Sequence-Class (name fields virtual-size ancestors descendants ascendant interfaces slots instance-slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class () ())
(jazz.define-class jazz.Port-Class         jazz.Class          (name fields virtual-size ancestors descendants ascendant interfaces slots instance-slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class () ())
(jazz.define-class jazz.Continuation-Class jazz.Class          (name fields virtual-size ancestors descendants ascendant interfaces slots instance-slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class () ())
(jazz.define-class jazz.Procedure-Class    jazz.Class          (name fields virtual-size ancestors descendants ascendant interfaces slots instance-slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class () ())
(jazz.define-class jazz.Symbol-Class       jazz.Class          (name fields virtual-size ancestors descendants ascendant interfaces slots instance-slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class () ())
(jazz.define-class jazz.Keyword-Class      jazz.Class          (name fields virtual-size ancestors descendants ascendant interfaces slots instance-slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class () ())
(jazz.define-class jazz.Table-Class        jazz.Class          (name fields virtual-size ancestors descendants ascendant interfaces slots instance-slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class () ())
(jazz.define-class jazz.Thread-Class       jazz.Class          (name fields virtual-size ancestors descendants ascendant interfaces slots instance-slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class () ())
(jazz.define-class jazz.Promise-Class      jazz.Class          (name fields virtual-size ancestors descendants ascendant interfaces slots instance-slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class () ())
(jazz.define-class jazz.Foreign-Class      jazz.Class          (name fields virtual-size ancestors descendants ascendant interfaces slots instance-slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class () ())
(jazz.define-class jazz.Values-Class       jazz.Class          (name fields virtual-size ancestors descendants ascendant interfaces slots instance-slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class () ())
(jazz.define-class jazz.EOF-Class          jazz.Class          (name fields virtual-size ancestors descendants ascendant interfaces slots instance-slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class () ())
(jazz.define-class jazz.Unspecified-Class  jazz.Class          (name fields virtual-size ancestors descendants ascendant interfaces slots instance-slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class () ())
(jazz.define-class jazz.Marker-Class       jazz.Class          (name fields virtual-size ancestors descendants ascendant interfaces slots instance-slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class () ())


(jazz.define-class jazz.Boolean      jazz.Object   () jazz.Boolean-Class      () ())
(jazz.define-class jazz.Char         jazz.Object   () jazz.Char-Class         () ())
(jazz.define-class jazz.Numeric      jazz.Object   () jazz.Numeric-Class      () ())
(jazz.define-class jazz.Number       jazz.Numeric  () jazz.Number-Class       () ())
(jazz.define-class jazz.Complex      jazz.Number   () jazz.Complex-Class      () ())
(jazz.define-class jazz.Real         jazz.Complex  () jazz.Real-Class         () ())
(jazz.define-class jazz.Rational     jazz.Real     () jazz.Rational-Class     () ())
(jazz.define-class jazz.Integer      jazz.Rational () jazz.Integer-Class      () ())
(jazz.define-class jazz.Fixnum       jazz.Integer  () jazz.Fixnum-Class       () ())
(jazz.define-class jazz.Flonum       jazz.Real     () jazz.Flonum-Class       () ())
(jazz.define-class jazz.Sequence     jazz.Object   () jazz.Sequence-Class     () ())
(jazz.define-class jazz.List         jazz.Sequence () jazz.List-Class         () ())
(jazz.define-class jazz.Null         jazz.List     () jazz.Null-Class         () ())
(jazz.define-class jazz.Pair         jazz.List     () jazz.Pair-Class         () ())
(jazz.define-class jazz.String       jazz.Sequence () jazz.String-Class       () ())
(jazz.define-class jazz.Vector       jazz.Sequence () jazz.Vector-Class       () ())
(jazz.define-class jazz.S8Vector     jazz.Sequence () jazz.S8Vector-Class     () ())
(jazz.define-class jazz.U8Vector     jazz.Sequence () jazz.U8Vector-Class     () ())
(jazz.define-class jazz.S16Vector    jazz.Sequence () jazz.S16Vector-Class    () ())
(jazz.define-class jazz.U16Vector    jazz.Sequence () jazz.U16Vector-Class    () ())
(jazz.define-class jazz.S32Vector    jazz.Sequence () jazz.S32Vector-Class    () ())
(jazz.define-class jazz.U32Vector    jazz.Sequence () jazz.U32Vector-Class    () ())
(jazz.define-class jazz.S64Vector    jazz.Sequence () jazz.S64Vector-Class    () ())
(jazz.define-class jazz.U64Vector    jazz.Sequence () jazz.U64Vector-Class    () ())
(jazz.define-class jazz.F32Vector    jazz.Sequence () jazz.F32Vector-Class    () ())
(jazz.define-class jazz.F64Vector    jazz.Sequence () jazz.F64Vector-Class    () ())
(jazz.define-class jazz.Structure    jazz.Sequence () jazz.Structure-Class    () ())
(jazz.define-class jazz.Port         jazz.Object   () jazz.Port-Class         () ())
(jazz.define-class jazz.Continuation jazz.Object   () jazz.Continuation-Class () ())
(jazz.define-class jazz.Procedure    jazz.Object   () jazz.Procedure-Class    () ())
(jazz.define-class jazz.Symbol       jazz.Object   () jazz.Symbol-Class       () ())
(jazz.define-class jazz.Keyword      jazz.Object   () jazz.Keyword-Class      () ())
(jazz.define-class jazz.Table        jazz.Object   () jazz.Table-Class        () ())
(jazz.define-class jazz.Thread       jazz.Object   () jazz.Thread-Class       () ())
(jazz.define-class jazz.Promise      jazz.Object   () jazz.Promise-Class      () ())
(jazz.define-class jazz.Foreign      jazz.Object   () jazz.Foreign-Class      () ())
(jazz.define-class jazz.Values       jazz.Object   () jazz.Values-Class       () ())
(jazz.define-class jazz.EOF          jazz.Object   () jazz.EOF-Class          () ())
(jazz.define-class jazz.Unspecified  jazz.Object   () jazz.Unspecified-Class  () ())
(jazz.define-class jazz.Marker       jazz.Object   () jazz.Marker-Class       () ())


;;;
;;;; Interface
;;;


(jazz.define-class jazz.Interface jazz.Category (name fields virtual-size ancestors descendants) jazz.Object-Class jazz.allocate-interface
  ((ascendants %%get-interface-ascendants ())
   (rank       %%get-interface-rank       ())))


;;;
;;;; Field
;;;


(jazz.define-class jazz.Field jazz.Object () jazz.Object-Class ()
  ((name %%get-field-name ())))


(jazz.define-macro (%%get-category-field category field-name)
  `(%%table-ref (%%get-category-fields ,category) ,field-name #f))


(jazz.define-macro (%%set-category-field category field-name field)
  `(%%table-set! (%%get-category-fields ,category) ,field-name ,field))


;;;
;;;; Slot
;;;


(jazz.define-class jazz.Slot jazz.Field (name) jazz.Object-Class jazz.allocate-slot
  ((offset     %%get-slot-offset     ())
   (initialize %%get-slot-initialize ())))


;;;
;;;; Property
;;;


(jazz.define-class jazz.Property jazz.Slot (name offset initialize) jazz.Object-Class jazz.allocate-property
  ((getter %%get-property-getter ())
   (setter %%get-property-setter ())))


;;;
;;;; Method
;;;


(jazz.define-class jazz.Method jazz.Field (name) jazz.Object-Class jazz.allocate-method
  ((dispatch-type        %%get-method-dispatch-type        %%set-method-dispatch-type)
   (implementation       %%get-method-implementation       %%set-method-implementation)
   (implementation-tree  %%get-method-implementation-tree  %%set-method-implementation-tree)
   (category-rank        %%get-method-category-rank        %%set-method-category-rank)
   (implementation-rank  %%get-method-implementation-rank  %%set-method-implementation-rank)))


;;;
;;;; Method-Node
;;;


(jazz.define-class jazz.Method-Node jazz.Object () jazz.Object-Class jazz.allocate-method-node
  ((category            %%get-method-node-category            %%set-method-node-category)
   (implementation      %%get-method-node-implementation      %%set-method-node-implementation)
   (next-node           %%get-method-node-next-node           %%set-method-node-next-node)
   (next-implementation %%get-method-node-next-implementation %%set-method-node-next-implementation)
   (children            %%get-method-node-children            %%set-method-node-children)))


;;;
;;;; Queue
;;;


(jazz.define-class jazz.Queue jazz.Object () jazz.Object-Class jazz.allocate-queue
  ((list        %%get-queue-list        %%set-queue-list)
   (last-list   %%get-queue-last-list   %%set-queue-last-list)
   (last-anchor %%get-queue-last-anchor %%set-queue-last-anchor)
   (current     %%get-queue-current     %%set-queue-current))))
