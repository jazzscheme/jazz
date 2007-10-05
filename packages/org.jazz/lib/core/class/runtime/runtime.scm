;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Classes Runtime
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


(module core.class.runtime.runtime


;;;
;;;; Identifier
;;;


(define (jazz.dispatch? symbol)
  (and (%%symbol? symbol)
       (let ((name (%%symbol->string symbol)))
         (%%eqv? (%%string-ref name (%%fx- (%%string-length name) 1))
                 #\~))))


(define (jazz.dispatch->symbol dispatch)
  (let ((name (%%symbol->string dispatch)))
    (%%string->symbol (%%substring name 0 (%%fx- (%%string-length name) 1)))))


(define (jazz.composite-name? symbol)
  (and (%%symbol? symbol)
       (jazz.memstring #\. (%%symbol->string symbol))))


(define (jazz.compose-name . rest)
  (%%string->symbol (jazz.join-strings (map symbol->string rest) ".")))


(define (jazz.compose-helper locator suffix)
  (%%string->symbol (%%string-append (%%symbol->string locator) ":" (%%symbol->string suffix))))


(define (jazz.split-identifier identifier)
  (map string->symbol (jazz.split-string (%%symbol->string identifier) #\.)))


;;;
;;;; Object
;;;


;; debugging utility
(define (jazz.object-content object)
  (%%assert (%%object? object)
    (let* ((size (%%object-length object))
           (content (%%make-vector size)))
      (let iter ((n 0))
        (if (%%fx< n size)
            (begin
              (%%vector-set! content n (%%object-ref object n))
              (iter (%%fx+ n 1)))))
      content)))


;;;
;;;; Core
;;;


(define jazz.Core-Classes
  (%%make-hashtable eq?))


(define (jazz.get-core-classes)
  jazz.Core-Classes)


(define (jazz.core-class? name)
  (%%boolean (%%hashtable-ref jazz.Core-Classes name #f)))


(define (jazz.get-core-class name)
  (%%hashtable-ref jazz.Core-Classes name #f))


(define (jazz.set-core-class name class)
  (%%hashtable-set! jazz.Core-Classes name class))


;;;
;;;; Class
;;;


(define (jazz.object? expr)
  (%%object? expr))


(define (jazz.primitive? expr)
  (%%not (%%object? expr)))


(define (jazz.subtype? target type)
  (%%boolean (%%subtype? target type)))


(define (jazz.subcategory? target category)
  (%%boolean (%%subcategory? target category)))


(define (jazz.subclass? target class)
  (%%boolean (%%subclass? target class)))


(define (jazz.get-class-ascendant class)
  (%%get-class-ascendant class))


(define (%%object-of-class? object class)
  (%%subclass? (%%get-object-class object) class))


(define (jazz.collect-type type lst)
  (jazz.collect-if (lambda (obj)
                     (%%is? obj type))
                   lst))


(define (jazz.get-core-class-all-slot-names core-class)
  (let ((slot-names (%%get-class-slots core-class))
        (ascendant (%%get-class-ascendant core-class)))
    (if (%%not ascendant)
        slot-names
      (%%append (jazz.get-core-class-all-slot-names ascendant) slot-names))))


(define (jazz.copy-dispatch-table class)
  (let ((class-dispatch-table (%%get-class-dispatch-table class)))
    (if class-dispatch-table
        (%%copy-hashtable class-dispatch-table)
      #f)))


(define (jazz.update-dispatch-table class name value)
  (jazz.iterate-descendants-tree class
    (lambda (subclass)
      (%%when (jazz.class? subclass)
        (%%when (%%not (%%get-class-dispatch-table subclass))
          (%%set-class-dispatch-table subclass (%%make-hashtable eq?)))
        (let ((dispatch-table (%%get-class-dispatch-table subclass)))
          (%%hashtable-set! dispatch-table name value))))))


;;;
;;;; Define Class
;;;


(define (jazz.new-core-class class name fields ascendant slot-names instance-size)
  (let ((core-class
         (%%object
          class
          ; Category
          name
          fields
          #f
          '()
          ; Class
          ascendant
          '()
          slot-names
          instance-size
          (if ascendant (%%fx+ (%%get-class-level ascendant) 1) 0)
          #f
          '()
          '()
          (if ascendant (%%get-class-core-virtual-names ascendant) '())
          #f
          #f
          #f)))
    (%%set-category-ancestors core-class (jazz.compute-core-class-ancestors core-class ascendant))
    (%%when ascendant
      (%%set-category-descendants ascendant (%%cons core-class (%%get-category-descendants ascendant)))
      (%%set-class-dispatch-table core-class (jazz.copy-dispatch-table ascendant)))
    core-class))


(define (jazz.compute-core-class-ancestors class ascendant)
  (if (%%not ascendant)
      (%%list class)
    (%%cons class (%%get-category-ancestors ascendant))))


(define (jazz.validate-inherited-slots name ascendant inherited-slot-names)
  (if (or (and (%%not ascendant) (%%not (%%null? inherited-slot-names)))
          (and ascendant (%%not (%%equal? (jazz.get-core-class-all-slot-names ascendant) inherited-slot-names))))
      (jazz.error "Inconsistant inherited slots for {s}: {s} vs {s}" name inherited-slot-names (and ascendant (jazz.get-core-class-all-slot-names ascendant)))))


;;;
;;;; Object
;;;


(jazz.define-class jazz.Object () () ()
  ())


(define (jazz.get-object-slot object slot-rank)
  (%%get-object-slot object slot-rank))


(define (jazz.set-object-slot object slot-rank value)
  (%%set-object-slot object slot-rank value))


(define (jazz.classname->string class)
  (if (%%null? class)
      "()"
    (%%symbol->string (%%get-category-name class))))


(jazz.encapsulate-class jazz.Object)


;;;
;;;; Type
;;;


(jazz.define-class jazz.Type jazz.Object () ()
  ())


(jazz.define-virtual (jazz.of-type? (jazz.Type type) object))


(jazz.define-method (jazz.of-type? (jazz.Type type) object)
  (jazz.of-subtype? type (%%class-of object)))


(jazz.define-virtual (jazz.of-subtype? (jazz.Type type) class))


(jazz.define-method (jazz.of-subtype? (jazz.Type type) class)
  (jazz.error "Unable to test type on: {s}" type))


(jazz.encapsulate-class jazz.Type)


;;;
;;;; Category
;;;


(jazz.define-class jazz.Category jazz.Type () ()
  (name
   fields
   ancestors
   descendants))


(define (jazz.category? object)
  (%%is? object jazz.Category))


(jazz.define-method (jazz.of-subtype? (jazz.Category type) class)
  (%%memq type (%%get-category-ancestors class)))


(define (jazz.add-field category field)
  (%%set-category-field category (%%get-field-name field) field))


(jazz.encapsulate-class jazz.Category)


;;;
;;;; Class
;;;


(jazz.define-class jazz.Class jazz.Category (name fields ancestors descendants) ()
  (ascendant
   interfaces
   slots
   instance-size
   level
   dispatch-table
   core-method-alist
   core-virtual-alist
   core-virtual-names
   core-vtable
   class-table
   interface-table))


(define (jazz.new-class class-of-class name ascendant interfaces)
  (let ((class (jazz.allocate-class class-of-class name (%%make-hashtable eq?) #f '()
                ascendant
                interfaces
                (if ascendant (%%get-class-slots ascendant) '())
                (if ascendant (%%get-class-instance-size ascendant) 0)
                (if ascendant (%%fx+ (%%get-class-level ascendant) 1) 0)
                (if ascendant (jazz.copy-dispatch-table ascendant) #f)
                #f
                #f
                #f
                (if ascendant (%%get-class-core-vtable ascendant) #f)
                #f
                #f)))
    (%%set-category-ancestors class (jazz.compute-class-ancestors class ascendant interfaces))
    (%%when ascendant
      (%%set-category-descendants ascendant (%%cons class (%%get-category-descendants ascendant))))
    (jazz.dialect.language.Object.initialize class)
    class))


(define (jazz.compute-class-ancestors class ascendant interfaces)
  (jazz.remove-duplicates
    (%%append (if (%%not ascendant)
                  (%%list class)
                (%%cons class (%%get-category-ancestors ascendant)))
              (%%apply append (map (lambda (interface)
                                     (%%get-category-ancestors interface))
                                   interfaces)))))


(define (jazz.class? object)
  (%%is? object jazz.Class))


(define (jazz.i-class-of expr)
  (%%i-class-of-impl expr))


(define (jazz.class-of expr)
  (%%class-of-impl expr))


;; This function enables jazz to bootstrap fully interpreted
;; Note that we need to implement every type that can potentially be used by Jazz code
;; so that really means every type if we want to be a super set of the underlying scheme!
(define (jazz.class-of-native expr)
  (cond ((%%boolean? expr)
         jazz.Boolean)
        ((%%char? expr)
         jazz.Char)
        ((%%integer? expr)
         jazz.Integer)
        ((%%real? expr)
         jazz.Real)
        ((%%null? expr)
         jazz.Null)
        ((%%pair? expr)
         jazz.Pair)
        ((%%string? expr)
         jazz.String)
        ((%%vector? expr)
         jazz.Vector)
        ((%%symbol? expr)
         jazz.Symbol)
        ((%%keyword? expr)
         jazz.Keyword)
        ((%%port? expr)
         jazz.Port)
        ((%%procedure? expr)
         jazz.Procedure)
        ((%%foreign? expr)
         jazz.Foreign)
        (else
         (jazz.error "Unable to get class of {s}" expr))))


(define (jazz.is? object category)
  (%%boolean (%%is? object category)))


(define (jazz.is-not? object category)
  (%%boolean (%%not (%%is? object category))))


;; for bootstrapping the of-type? / of-subtype? core methods
(define (jazz.is-type? object type-class)
  ;; should add a low-level test here
  #t)


(define (jazz.get-category-name category)
  (%%get-category-name category))


(define (jazz.slot-form? form)
  (and (%%pair? form)
       (%%eq? (%%car form) 'slot)))


(define (jazz.new class . rest)
  (%%assert (jazz.class? class)
    (let* ((base jazz.object-size)
           (size (%%get-class-instance-size class))
           (object (%%make-object (%%fx+ base size))))
      (%%set-object-class object class)
      (jazz.initialize-slots object)
      ;; todo optimize initialize call and at the same time enable Object.initialize to take 0 arguments
      ;; (%%when (%%not (%%null? (%%get-generic-pending-specifics jazz.dialect.language.Object.initialize!generic)))
      ;;  (jazz.update-generic jazz.dialect.language.Object.initialize!generic))
      ;; (apply (%%hashtable-ref (%%get-class-dispatch-table class) 'initialize) object rest)
      (apply jazz.dialect.language.Object.initialize object rest)
      object)))


(define jazz.dialect.language.Object.initialize
  #f)

(set! jazz.dialect.language.Object.initialize #f)


(define (jazz.iterate-descendants-tree class proc)
  (let iter ((class class))
    (proc class)
    (for-each iter (%%get-category-descendants class))))


(jazz.encapsulate-class jazz.Class)


;;;
;;;; Object-Class
;;;


(jazz.define-class jazz.Object-Class jazz.Class (name fields ancestors descendants ascendant interfaces slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) ()
  ())


(jazz.encapsulate-class jazz.Object-Class)


;;;
;;;; Class Bootstrap
;;;


(%%set-object-class jazz.Type jazz.Class)
(%%set-object-class jazz.Category jazz.Class)
(%%set-object-class jazz.Class jazz.Class)
(%%set-object-class jazz.Object-Class jazz.Class)
(%%set-object-class jazz.Object jazz.Object-Class)


;;;
;;;; Primitive Classes
;;;


(jazz.define-class jazz.Sequence-Class jazz.Object-Class   (name fields ancestors descendants ascendant interfaces slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class ())
(jazz.define-class jazz.List-Class     jazz.Sequence-Class (name fields ancestors descendants ascendant interfaces slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class ())
(jazz.define-class jazz.String-Class   jazz.Sequence-Class (name fields ancestors descendants ascendant interfaces slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class ())
(jazz.define-class jazz.Vector-Class   jazz.Sequence-Class (name fields ancestors descendants ascendant interfaces slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class ())


(jazz.define-class jazz.Boolean   jazz.Object   () jazz.Object-Class   ())
(jazz.define-class jazz.Char      jazz.Object   () jazz.Object-Class   ())
(jazz.define-class jazz.Numeric   jazz.Object   () jazz.Object-Class   ())
(jazz.define-class jazz.Number    jazz.Numeric  () jazz.Object-Class   ())
(jazz.define-class jazz.Complex   jazz.Number   () jazz.Object-Class   ())
(jazz.define-class jazz.Real      jazz.Complex  () jazz.Object-Class   ())
(jazz.define-class jazz.Rational  jazz.Real     () jazz.Object-Class   ())
(jazz.define-class jazz.Integer   jazz.Rational () jazz.Object-Class   ())
(jazz.define-class jazz.Fixnum    jazz.Integer  () jazz.Object-Class   ())
(jazz.define-class jazz.Flonum    jazz.Real     () jazz.Object-Class   ())
(jazz.define-class jazz.Sequence  jazz.Object   () jazz.Sequence-Class ())
(jazz.define-class jazz.List      jazz.Sequence () jazz.List-Class     ())
(jazz.define-class jazz.Null      jazz.List     () jazz.List-Class     ())
(jazz.define-class jazz.Pair      jazz.List     () jazz.List-Class     ())
(jazz.define-class jazz.Port      jazz.Object   () jazz.Object-Class   ())
(jazz.define-class jazz.Procedure jazz.Object   () jazz.Object-Class   ())
(jazz.define-class jazz.Foreign   jazz.Object   () jazz.Object-Class   ())
(jazz.define-class jazz.String    jazz.Sequence () jazz.String-Class   ())
(jazz.define-class jazz.Symbol    jazz.Object   () jazz.Object-Class   ())
(jazz.define-class jazz.Keyword   jazz.Object   () jazz.Object-Class   ())
(jazz.define-class jazz.Vector    jazz.Sequence () jazz.Vector-Class   ())
(jazz.define-class jazz.Hashtable jazz.Object   () jazz.Object-Class   ())


(jazz.encapsulate-class jazz.Sequence-Class)
(jazz.encapsulate-class jazz.List-Class)
(jazz.encapsulate-class jazz.String-Class)
(jazz.encapsulate-class jazz.Vector-Class)
(jazz.encapsulate-class jazz.Boolean)
(jazz.encapsulate-class jazz.Char)
(jazz.encapsulate-class jazz.Numeric)
(jazz.encapsulate-class jazz.Number)
(jazz.encapsulate-class jazz.Complex)
(jazz.encapsulate-class jazz.Real)
(jazz.encapsulate-class jazz.Rational)
(jazz.encapsulate-class jazz.Integer)
(jazz.encapsulate-class jazz.Fixnum)
(jazz.encapsulate-class jazz.Flonum)
(jazz.encapsulate-class jazz.Sequence)
(jazz.encapsulate-class jazz.List)
(jazz.encapsulate-class jazz.Null)
(jazz.encapsulate-class jazz.Pair)
(jazz.encapsulate-class jazz.Port)
(jazz.encapsulate-class jazz.Procedure)
(jazz.encapsulate-class jazz.Foreign)
(jazz.encapsulate-class jazz.String)
(jazz.encapsulate-class jazz.Symbol)
(jazz.encapsulate-class jazz.Keyword)
(jazz.encapsulate-class jazz.Vector)
(jazz.encapsulate-class jazz.Hashtable)


(cond-expand
  (gambit
    (include "~/gambit/lib/type#.scm")
    
    (define jazz.subtypes
      (make-vector 32 #f))
    
    (define jazz.specialtypes
      (make-vector 16 #f))
    
    ;; quicky until we find a clean solution with Marc
    (define jazz.hashtable-type
      (##structure-type (make-table)))
    
    ;; quicky until we find a clean solution with Marc
    (define jazz.port-type
      (##structure-type (open-output-string)))
    
    (%%vector-set! jazz.subtypes (macro-subtype-vector)    jazz.Vector)
    (%%vector-set! jazz.subtypes (macro-subtype-pair)      jazz.Pair)
    (%%vector-set! jazz.subtypes (macro-subtype-ratnum)    jazz.Rational)
    (%%vector-set! jazz.subtypes (macro-subtype-cpxnum)    jazz.Complex)
    ;; super quicky untill we add structure dispatch to %%c-class-of
    (%%vector-set! jazz.subtypes (macro-subtype-structure) jazz.Port)
    (%%vector-set! jazz.subtypes (macro-subtype-symbol)    jazz.Symbol)
    (%%vector-set! jazz.subtypes (macro-subtype-keyword)   jazz.Keyword)
    (%%vector-set! jazz.subtypes (macro-subtype-procedure) jazz.Procedure)
    (%%vector-set! jazz.subtypes (macro-subtype-foreign)   jazz.Foreign)
    (%%vector-set! jazz.subtypes (macro-subtype-string)    jazz.String)
    (%%vector-set! jazz.subtypes (macro-subtype-flonum)    jazz.Flonum)
    (%%vector-set! jazz.subtypes (macro-subtype-bignum)    jazz.Rational)
    
    (%%vector-set! jazz.specialtypes 0 jazz.Boolean)
    (%%vector-set! jazz.specialtypes 1 jazz.Boolean)
    (%%vector-set! jazz.specialtypes 2 jazz.Null)
    ;;(%%vector-set! jazz.specialtypes 3 jazz.EOF)
    ;;(%%vector-set! jazz.specialtypes 4 jazz.Void)
    ;;(%%vector-set! jazz.specialtypes 4 jazz.Absent)
    )
  
  (else))


;;;
;;;; Interface
;;;


(jazz.define-class jazz.Interface jazz.Category (name fields ancestors descendants) jazz.Object-Class
  (ascendants
   rank))


(define (jazz.new-interface class name ascendants)
  (let ((interface (jazz.allocate-interface class name (%%make-hashtable eq?) #f '() ascendants #f)))
    (%%set-category-ancestors interface (jazz.compute-interface-ancestors interface ascendants))
    (for-each (lambda (ascendant)
                (%%set-category-descendants ascendant (%%cons class (%%get-category-descendants ascendant))))
              ascendants)
    interface))


(define (jazz.compute-interface-ancestors interface ascendants)
  (jazz.remove-duplicates
    (%%cons interface (%%apply append (map (lambda (ascendant)
                                             (%%get-category-ancestors ascendant))
                                           ascendants)))))


(define (jazz.interface? object)
  (%%is? object jazz.Interface))


(jazz.encapsulate-class jazz.Interface)


;;;
;;;; Field
;;;


(jazz.define-class jazz.Field jazz.Object () jazz.Object-Class
  (name))


(define (jazz.field? object)
  (%%is? object jazz.Field))


(define (jazz.field-name field)
  (%%get-field-name field))


(define (jazz.find-field category field-name)
  (or (%%get-category-field category field-name)
      (let ((ascendant (%%get-class-ascendant category)))
        (if (%%not ascendant)
            #f
          (jazz.find-field ascendant field-name)))))


(define (jazz.require-object-field object name)
  (let* ((class (%%class-of object))
         (field (jazz.find-field class name)))
    (if (%%not field)
        (jazz.error "Unknown field '{s} of {s}" name (%%get-category-name (%%get-object-class object)))
      field)))


(jazz.encapsulate-class jazz.Field)


;;;
;;;; Slot
;;;


(jazz.define-class jazz.Slot jazz.Field (name) jazz.Object-Class
  (rank
   initialize))


(define (jazz.new-slot slot-name slot-rank slot-initialize)
  (jazz.allocate-slot jazz.Slot slot-name slot-rank slot-initialize))


(define (jazz.slot? object)
  (%%is? object jazz.Slot))


(define (jazz.add-slot class slot-name slot-initialize)
  ;; this is a quicky that needs to be well tought out
  (or (%%get-category-field class slot-name)
      (let* ((slot-rank (%%get-class-instance-size class))
             (slot (jazz.new-slot slot-name slot-rank slot-initialize)))
        (jazz.add-field class slot)
        (%%set-class-slots class (%%append (%%get-class-slots class) (%%list slot)))
        (%%set-class-instance-size class (%%fx+ slot-rank 1))
        slot)))


(define (jazz.remove-slots class)
  (let ((actual (%%get-class-slots class)))
    (%%set-class-slots class '())
    (%%set-class-instance-size class (%%fx- (%%get-class-instance-size class) (%%length actual)))))


(define (jazz.slot-value object slot-name)
  (%%assert (%%object? object)
    (let ((slot (jazz.require-object-field object slot-name)))
      (%%get-object-slot object (%%get-slot-rank slot)))))


(define (jazz.set-slot-value object slot-name value)
  (%%assert (%%object? object)
    (let ((slot (jazz.require-object-field object slot-name)))
      (%%set-object-slot object (%%get-slot-rank slot) value))))


(define (jazz.find-slot-offset object slot-name)
  (let ((slot (jazz.require-object-field object slot-name)))
    (%%slot-offset (%%get-slot-rank slot))))


(define (jazz.initialize-slots object)
  (let* ((class (%%get-object-class object))
         (slots (%%get-class-slots class)))
    (for-each (lambda (slot)
                (let ((rank (%%get-slot-rank slot))
                      (initialize (%%get-slot-initialize slot)))
                  (%%set-object-slot object rank (initialize object))))
              slots)))


(jazz.encapsulate-class jazz.Slot)


;;;
;;;; Property
;;;


(jazz.define-class jazz.Property jazz.Slot (name rank initialize) jazz.Object-Class
  (getter
   setter))


(define (jazz.new-property slot-name slot-rank slot-initialize slot-getter slot-setter)
  (jazz.allocate-property jazz.Property slot-name slot-rank slot-initialize slot-getter slot-setter))


(define (jazz.property? object)
  (%%is? object jazz.Property))


(define (jazz.property-getter property)
  (%%get-property-getter property))


(define (jazz.property-setter property)
  (%%get-property-setter property))


(define (jazz.all-properties category)
  (let iter ((slots (%%get-class-slots category)))
     (cond ((%%null? slots) '())
           ((jazz.property? (%%car slots)) (%%cons (%%car slots) (iter (%%cdr slots))))
           (else (iter (%%cdr slots)))))) 


(define (jazz.add-property class slot-name slot-initialize slot-getter slot-setter)
  ;; this is a quicky that needs to be well tought out
  (or (%%get-category-field class slot-name)
      (let* ((slot-rank (%%get-class-instance-size class))
             (slot (jazz.new-property slot-name slot-rank slot-initialize slot-getter slot-setter)))
        (jazz.add-field class slot)
        (%%set-class-slots class (%%append (%%get-class-slots class) (%%list slot)))
        (%%set-class-instance-size class (%%fx+ slot-rank 1))
        slot)))


(jazz.encapsulate-class jazz.Property)


;;;
;;;; Method
;;;


(jazz.define-class jazz.Method jazz.Field (name) jazz.Object-Class
  (propagation
   implementation))


(define (jazz.new-method name propagation implementation)
  (jazz.allocate-method jazz.Method name propagation implementation))


(define (jazz.method? object)
  (%%is? object jazz.Method))


(define (jazz.method-virtual? method)
  (%%eq? (%%get-method-propagation method) 'virtual))


(jazz.encapsulate-class jazz.Method)


;;;
;;;; Queue
;;;


(jazz.define-class jazz.Queue jazz.Object () jazz.Object-Class
  (list
   last-list
   last-anchor
   current))


(define (jazz.new-queue)
  (jazz.allocate-queue jazz.Queue '() '() '() '()))


(define (jazz.enqueue queue object)
  (let ((current (%%get-queue-current queue)))
    (cond ((%%null? current)
           (%%set-queue-current queue (%%cons object current))
           (%%set-queue-list queue (%%get-queue-current queue)))
          (else
           (%%when (%%not-null? (%%get-queue-last-list queue))
             (jazz.queue-copy-last-list queue))
           (let ((pair (%%cons object '())))
             (%%set-cdr! current pair)
             (%%set-queue-current queue pair))))))


(define (jazz.enqueue-list queue lst)
  (%%when (%%not-null? lst)
    (cond ((%%null? (%%get-queue-current queue))
           (%%set-queue-current queue lst)
           (%%set-queue-last-list queue lst)
           (%%set-queue-list queue lst))
          (else
           (%%when (%%not-null? (%%get-queue-last-list queue))
             (jazz.queue-copy-last-list queue))
           (%%set-queue-last-list queue lst)
           (%%set-queue-last-anchor queue (%%get-queue-current queue))
           (%%set-cdr! (%%get-queue-current queue) lst)))))


(define (jazz.queue-copy-last-list queue)
  (let ((last-anchor (%%get-queue-last-anchor queue)))
    (cond ((%%null? last-anchor)
           (%%set-queue-list queue (jazz.list-copy (%%get-queue-last-list queue)))
           (%%set-queue-current queue (jazz.last-pair (%%get-queue-list queue))))
          (else
           (%%set-cdr! last-anchor (jazz.list-copy (%%get-queue-last-list queue)))
           (%%set-queue-current queue (jazz.last-pair last-anchor))))
    (%%set-queue-last-list queue '())
    (%%set-queue-last-anchor queue '())))


(define (jazz.queue-list queue)
  (%%get-queue-list queue))


(jazz.encapsulate-class jazz.Queue))
