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


(define jazz.new-interface:next-rank
  0)


;;;
;;;; Identifier
;;;


(define (jazz.neodispatch? symbol)
  (and (%%symbol? symbol)
       (let ((name (%%symbol->string symbol)))
         (let ((len (%%string-length name)))
           (and (%%fx> len 1)
                (%%eqv? (%%string-ref name (%%fx- len 1))
                        #\$))))))


(define (jazz.dispatch? symbol)
  (and (%%symbol? symbol)
       (let ((name (%%symbol->string symbol)))
         (let ((len (%%string-length name)))
           (and (%%fx> len 1)
                (%%eqv? (%%string-ref name (%%fx- len 1))
                        #\~))))))


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


(define (jazz.create-class-tables class)
  (jazz.create-class-interface-table class)
  (jazz.create-class-class-table class))


(define (jazz.create-core-class-tables class)
  (jazz.create-class-class-table class))


(define (jazz.create-class-interface-table class)
  (%%when (%%not (%%get-class-interface-table class))
    (let ((vtable (%%make-vector jazz.new-interface:next-rank #f))
          (ascendant (%%get-class-ascendant class)))
      (%%when ascendant
        (let ((ascendant-interface-table (%%get-class-interface-table ascendant)))
          (%%when ascendant-interface-table
            (let ((size (%%vector-length ascendant-interface-table)))
              (let iter ((i 0))
                   (%%when (< i size)
                     (let ((ascendant-vtable (%%vector-ref ascendant-interface-table i)))
                       (%%when ascendant-vtable
                         (%%vector-set! vtable i (%%vector-copy ascendant-vtable))))
                     (iter (+ i 1))))))))
      (for-each (lambda (category)
                  (%%when (%%is? category jazz.Interface)
                    (let* ((rank (%%get-interface-rank category))
                           (size (%%get-interface-virtual-size category)))
                      (%%when (%%not (%%vector-ref vtable rank))
                        (%%vector-set! vtable rank (%%make-vector size jazz.call-into-abstract))))))
                (%%get-category-ancestors class))
      (%%set-class-interface-table class vtable))))


(define (jazz.create-class-class-table class)
  (%%when (%%not (%%get-class-class-table class))
    (let ((ascendant (%%get-class-ascendant class)))
      (%%set-class-class-table class
        (if ascendant
            (let* ((ascendant-class-table (%%get-class-class-table ascendant))
                   (size (%%vector-length ascendant-class-table))
                   (vtable (%%make-vector (+ size 1) '#())))
              (let iter ((i 0))
                   (%%when (< i size)
                     (%%vector-set! vtable i (%%vector-copy (%%vector-ref ascendant-class-table i)))
                     (iter (+ i 1))))
              vtable)
          (%%make-vector 1 '#()))))))


(define (jazz.encapsulate-class class)
  (let ((virtual-names (%%get-class-core-virtual-names class)))
    (let ((vtable-size (%%length virtual-names)))
      (%%when (%%fx> vtable-size 0)
        (let ((vtable (make-vector vtable-size #f)))
          (let ((ascendant (%%get-class-ascendant class)))
            (%%when ascendant
              (let ((ascendant-vtable (%%get-class-core-vtable ascendant)))
                (%%when ascendant-vtable
                  (let iter ((n (%%fx- (%%vector-length ascendant-vtable) 1)))
                    (%%when (%%fx>= n 0)
                      (%%vector-set! vtable n (%%vector-ref ascendant-vtable n))
                      (iter (%%fx- n 1))))))))
          (for-each (lambda (method)
                      (let ((method-name (%%car method))
                            (method-implementation (%%cdr method)))
                        (%%vector-set! vtable (jazz.get-method-rank class method-name) method-implementation)))
                    (%%get-class-core-virtual-alist class))
          (for-each (lambda (method)
                      (let ((method-name (%%car method))
                            (method-implementation (%%cdr method)))
                        (%%vector-set! vtable (jazz.get-method-rank class method-name) method-implementation)))
                    (%%get-class-core-method-alist class))
          (%%set-class-core-vtable class vtable)
          (jazz.update-core-class class))))))


;;jazz.update-interface
(define (jazz.update-class class)
  (jazz.update-class-class-table class))


(define (jazz.update-core-class class)
  (jazz.update-class-class-table class))


(define (jazz.update-class-class-table class)
  (receive (count added-methods) (jazz.update-class-class-root-methods class)
    (%%when (%%not-null? added-methods)
      (let ((class-rank (%%get-class-level class)))
        (let iter ((class class))
             (let* ((class-table (%%get-class-class-table class))
                    (implementation-table (jazz.resize-vector (%%vector-ref class-table class-rank) count)))
               (for-each (lambda (field)
                           (let ((implementation-rank (%%get-virtual-method-implementation-rank field))
                                 (implementation (%%get-method-node-implementation (%%get-virtual-method-implementation-tree field))))
                             (%%vector-set! implementation-table implementation-rank implementation)))
                         added-methods)
               (%%vector-set! class-table class-rank implementation-table))
             (for-each (lambda (descendant)
                         (iter descendant))
                       (%%get-category-descendants class)))))))


(define (jazz.update-class-class-root-methods class)
  (let* ((class-table (%%get-class-class-table class))
         (class-rank (%%get-class-level class))
         (root-implementation-table (%%vector-ref class-table class-rank))
         (count (if root-implementation-table (%%vector-length root-implementation-table) 0))
         (added-methods '()))
    (%%iterate-hashtable (%%get-category-fields class)
      (lambda (key field)
        (%%when (%%is? field jazz.Virtual-Method)
          (let ((implementation-rank (%%get-virtual-method-implementation-rank field)))
            (if implementation-rank
                (let ((old-implementation (%%vector-ref root-implementation-table implementation-rank))
                      (new-implementation (%%get-method-node-implementation (%%get-virtual-method-implementation-tree field))))
                  (%%when (%%neq? old-implementation new-implementation)
                    ;; method exists and has changed - update vtable + propagate to descendants
                    (let iter ((class class))
                         (let* ((class-table (%%get-class-class-table class))
                                (implementation-table (%%vector-ref class-table class-rank)))
                           (%%when (%%eq? old-implementation (%%vector-ref implementation-table implementation-rank))
                             (%%vector-set! implementation-table implementation-rank new-implementation)
                             (for-each (lambda (descendant)
                                         (iter descendant))
                                       (%%get-category-descendants class)))))))
              (begin
                (%%set-virtual-method-category-rank field class-rank)
                (%%set-virtual-method-implementation-rank field count)
                (set! count (+ count 1))
                (set! added-methods (cons field added-methods))))))))
    (values count added-methods)))


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
    (jazz.create-core-class-tables core-class)
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


(jazz.define-virtual (jazz.of-subtype? (jazz.Type type) subtype))


(jazz.define-method (jazz.of-subtype? (jazz.Type type) subtype)
  (jazz.error "Unable to test type on: {s}" type))


(jazz.define-virtual (jazz.category-type? (jazz.Type type)))


(jazz.define-method (jazz.category-type? (jazz.Type type))
  #f)


(jazz.define-virtual (jazz.emit-type (jazz.Type type)))


(jazz.define-method (jazz.emit-type (jazz.Type type))
  (jazz.error "Unable to emit type for: {s}" type))


(jazz.define-virtual (jazz.emit-specifier (jazz.Type type)))


(jazz.define-method (jazz.emit-specifier (jazz.Type type))
  (jazz.error "Unable to emit specifier for: {s}" type))


(jazz.define-virtual (jazz.emit-test (jazz.Type type) value environment))


(jazz.define-method (jazz.emit-test (jazz.Type type) value environment)
  (let ((locator (jazz.emit-type type)))
    `(%%is? ,value ,locator)))


(jazz.define-virtual (jazz.emit-check (jazz.Type type) value environment))


(jazz.define-method (jazz.emit-check (jazz.Type type) value environment)
  (let ((locator (jazz.emit-type type)))
    `(if (%%not ,(jazz.emit-test type value environment))
         (jazz.type-error ,value ,locator))))


;; for bootstrapping the core methods of type
(define (jazz.bootstrap-type? object type-class)
  ;; should add a low-level test here if possible
  #t)


(define (jazz.type? object)
  (%%is? object jazz.Type))


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
  (jazz.category-type? object))


(jazz.define-method (jazz.of-subtype? (jazz.Category type) subtype)
  (and (jazz.category-type? subtype)
       (%%memq type (%%get-category-ancestors subtype))))


(jazz.define-method (jazz.category-type? (jazz.Category type))
  #t)


(jazz.define-method (jazz.emit-type (jazz.Category type))
  (%%get-category-name type))


(define (jazz.is? object category)
  (%%boolean (%%is? object category)))


(define (jazz.is-not? object category)
  (%%boolean (%%not (%%is? object category))))


(define (jazz.get-category-name category)
  (%%get-category-name category))


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
    (jazz.create-class-tables class)
    (jazz.initialize-object class '())
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
;; so that really means every type if we want to be a superset of the underlying scheme
(define (jazz.class-of-native expr)
  (cond ((%%boolean? expr)   jazz.Boolean)
        ((%%char? expr)      jazz.Char)
        ((%%fixnum? expr)    jazz.Fixnum)
        ((%%flonum? expr)    jazz.Flonum)
        ((%%integer? expr)   jazz.Integer)
        ((%%rational? expr)  jazz.Rational)
        ((%%real? expr)      jazz.Real)
        ((%%complex? expr)   jazz.Complex)
        ((%%number? expr)    jazz.Number)
        ((%%null? expr)      jazz.Null)
        ((%%pair? expr)      jazz.Pair)
        ((%%string? expr)    jazz.String)
        ((%%vector? expr)    jazz.Vector)
        ((%%symbol? expr)    jazz.Symbol)
        ((%%keyword? expr)   jazz.Keyword)
        ((%%port? expr)      jazz.Port)
        ((%%procedure? expr) jazz.Procedure)
        ((%%foreign? expr)   jazz.Foreign)
        (else
         (jazz.error "Unable to get class of {s}" expr))))


;;tBool is_class_subtype(jType target, jType type)
;;{
;;	tInt	target_offset = target->class_ancestors_sizeGet();
;;	tInt	type_offset = type->class_ancestors_sizeGet();
;;	
;;	return	target_offset >= type_offset && 
;;			target->ancestorsGet()[type_offset-1] == type;
;;}
(jazz.define-method (jazz.of-type? (jazz.Class class) object)
  (jazz.of-subtype? class (%%class-of object)))


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
      ;; todo optimize initialize call and at the same time enable Object.initialize to take variable arguments
      ;; (%%when (%%not (%%null? (%%get-generic-pending-specifics jazz.dialect.language.Object.initialize!generic)))
      ;;  (jazz.update-generic jazz.dialect.language.Object.initialize!generic))
      ;; (apply (%%hashtable-ref (%%get-class-dispatch-table class) 'initialize) object rest)
      (jazz.initialize-object object rest)
      object)))


(define (jazz.initialize-object object rest)
  (if jazz.neo?
      (jazz.neo-initialize-object object rest)
    (apply jazz.dialect.language.Object.initialize object rest)))


;; the rank of basic methods like initialize should be harcode to 0, 1, ...
(define jazz.neo-initialize-object
  (let ((rank #f))
    (lambda (object rest)
      (if (%%not rank)
          (let ((field (%%get-category-field jazz.Object 'initialize)))
            (set! rank (%%get-virtual-method-implementation-rank field))))
      (let ((implementation (%%vector-ref (%%vector-ref (%%get-class-class-table (%%get-object-class object)) 0) rank)))
        (apply implementation object rest)))))


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
;;;; Boolean
;;;


(jazz.define-class jazz.Boolean-Class jazz.Class (name fields ancestors descendants ascendant interfaces slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class
  ())


(jazz.define-method (jazz.of-type? (jazz.Boolean-Class class) object)
  #t)


(jazz.define-method (jazz.emit-specifier (jazz.Boolean-Class class))
  'bool)


(jazz.define-method (jazz.emit-test (jazz.Boolean-Class type) value environment)
  `(%%boolean? ,value))


(jazz.encapsulate-class jazz.Boolean-Class)


(jazz.define-class jazz.Boolean jazz.Object () jazz.Boolean-Class
  ())


(jazz.encapsulate-class jazz.Boolean)


;;;
;;;; Char
;;;


(jazz.define-class jazz.Char-Class jazz.Class (name fields ancestors descendants ascendant interfaces slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class
  ())


(jazz.define-method (jazz.of-type? (jazz.Char-Class class) object)
  (%%char? object))


(jazz.define-method (jazz.emit-specifier (jazz.Char-Class class))
  'char)


(jazz.define-method (jazz.emit-test (jazz.Char-Class type) value environment)
  `(%%char? ,value))


(jazz.encapsulate-class jazz.Char-Class)


(jazz.define-class jazz.Char jazz.Object () jazz.Char-Class
  ())


(jazz.encapsulate-class jazz.Char)


;;;
;;;; Numeric
;;;


(jazz.define-class jazz.Numeric-Class jazz.Class (name fields ancestors descendants ascendant interfaces slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class
  ())


(jazz.define-method (jazz.emit-specifier (jazz.Numeric-Class class))
  'numeric)


(jazz.encapsulate-class jazz.Numeric-Class)


(jazz.define-class jazz.Numeric jazz.Object () jazz.Numeric-Class
  ())


(jazz.encapsulate-class jazz.Numeric)


;;;
;;;; Number
;;;


(jazz.define-class jazz.Number-Class jazz.Numeric-Class (name fields ancestors descendants ascendant interfaces slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class
  ())


(jazz.define-method (jazz.of-type? (jazz.Number-Class class) object)
  (%%number? object))


(jazz.define-method (jazz.emit-specifier (jazz.Number-Class class))
  'number)


(jazz.define-method (jazz.emit-test (jazz.Number-Class type) value environment)
  `(%%number? ,value))


(jazz.encapsulate-class jazz.Number-Class)


(jazz.define-class jazz.Number jazz.Numeric () jazz.Number-Class
  ())


(jazz.encapsulate-class jazz.Number)


;;;
;;;; Complex
;;;


(jazz.define-class jazz.Complex-Class jazz.Number-Class (name fields ancestors descendants ascendant interfaces slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class
  ())


(jazz.define-method (jazz.of-type? (jazz.Complex-Class class) object)
  (%%complex? object))


(jazz.define-method (jazz.emit-specifier (jazz.Complex-Class class))
  'complex)


(jazz.define-method (jazz.emit-test (jazz.Complex-Class type) value environment)
  `(%%complex? ,value))


(jazz.encapsulate-class jazz.Complex-Class)


(jazz.define-class jazz.Complex jazz.Number () jazz.Complex-Class
  ())


(jazz.encapsulate-class jazz.Complex)


;;;
;;;; Real
;;;


(jazz.define-class jazz.Real-Class jazz.Complex-Class (name fields ancestors descendants ascendant interfaces slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class
  ())


(jazz.define-method (jazz.of-type? (jazz.Real-Class class) object)
  (%%real? object))


(jazz.define-method (jazz.emit-specifier (jazz.Real-Class class))
  'real)


(jazz.define-method (jazz.emit-test (jazz.Real-Class type) value environment)
  `(%%real? ,value))


(jazz.encapsulate-class jazz.Real-Class)


(jazz.define-class jazz.Real jazz.Complex () jazz.Real-Class
  ())


(jazz.encapsulate-class jazz.Real)


;;;
;;;; Rational
;;;


(jazz.define-class jazz.Rational-Class jazz.Real-Class (name fields ancestors descendants ascendant interfaces slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class
  ())


(jazz.define-method (jazz.of-type? (jazz.Rational-Class class) object)
  (%%rational? object))


(jazz.define-method (jazz.emit-specifier (jazz.Rational-Class class))
  'rational)


(jazz.define-method (jazz.emit-test (jazz.Rational-Class type) value environment)
  `(%%rational? ,value))


(jazz.encapsulate-class jazz.Rational-Class)


(jazz.define-class jazz.Rational jazz.Real () jazz.Rational-Class
  ())


(jazz.encapsulate-class jazz.Rational)


;;;
;;;; Integer
;;;


(jazz.define-class jazz.Integer-Class jazz.Rational-Class (name fields ancestors descendants ascendant interfaces slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class
  ())


(jazz.define-method (jazz.of-type? (jazz.Integer-Class class) object)
  (%%integer? object))


(jazz.define-method (jazz.emit-specifier (jazz.Integer-Class class))
  'int)


(jazz.define-method (jazz.emit-test (jazz.Integer-Class type) value environment)
  `(%%integer? ,value))


(jazz.encapsulate-class jazz.Integer-Class)


(jazz.define-class jazz.Integer jazz.Rational () jazz.Integer-Class
  ())


(jazz.encapsulate-class jazz.Integer)


;;;
;;;; Fixnum
;;;


(jazz.define-class jazz.Fixnum-Class jazz.Integer-Class (name fields ancestors descendants ascendant interfaces slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class
  ())


(jazz.define-method (jazz.of-type? (jazz.Fixnum-Class class) object)
  (%%fixnum? object))


(jazz.define-method (jazz.emit-specifier (jazz.Fixnum-Class class))
  'fx)


(jazz.define-method (jazz.emit-test (jazz.Fixnum-Class type) value environment)
  `(%%fixnum? ,value))


(jazz.encapsulate-class jazz.Fixnum-Class)


(jazz.define-class jazz.Fixnum jazz.Integer () jazz.Fixnum-Class
  ())


(jazz.encapsulate-class jazz.Fixnum)


;;;
;;;; Flonum
;;;


(jazz.define-class jazz.Flonum-Class jazz.Real-Class (name fields ancestors descendants ascendant interfaces slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class
  ())


(jazz.define-method (jazz.of-type? (jazz.Flonum-Class class) object)
  (%%flonum? object))


(jazz.define-method (jazz.emit-specifier (jazz.Flonum-Class class))
  'fl)


(jazz.define-method (jazz.emit-test (jazz.Flonum-Class type) value environment)
  `(%%flonum? ,value))


(jazz.encapsulate-class jazz.Flonum-Class)


(jazz.define-class jazz.Flonum jazz.Real () jazz.Flonum-Class
  ())


(jazz.encapsulate-class jazz.Flonum)


;;;
;;;; Sequence
;;;


(jazz.define-class jazz.Sequence-Class jazz.Class (name fields ancestors descendants ascendant interfaces slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class
  ())


(jazz.define-method (jazz.emit-specifier (jazz.Sequence-Class class))
  'sequence)


(jazz.encapsulate-class jazz.Sequence-Class)


(jazz.define-class jazz.Sequence jazz.Object () jazz.Sequence-Class
  ())


(jazz.encapsulate-class jazz.Sequence)


;;;
;;;; List
;;;


(jazz.define-class jazz.List-Class jazz.Sequence-Class (name fields ancestors descendants ascendant interfaces slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class
  ())


(jazz.define-method (jazz.emit-specifier (jazz.List-Class class))
  'list)


(jazz.define-method (jazz.emit-test (jazz.List-Class type) value environment)
  `(or (%%null? ,value) (%%pair? ,value)))


(jazz.encapsulate-class jazz.List-Class)


(jazz.define-class jazz.List jazz.Sequence () jazz.List-Class
  ())


(jazz.encapsulate-class jazz.List)


;;;
;;;; Null
;;;


(jazz.define-class jazz.Null-Class jazz.List-Class (name fields ancestors descendants ascendant interfaces slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class
  ())


(jazz.define-method (jazz.of-type? (jazz.Null-Class class) object)
  (%%null? object))


(jazz.define-method (jazz.emit-specifier (jazz.Null-Class class))
  'null)


(jazz.define-method (jazz.emit-test (jazz.Null-Class type) value environment)
  `(%%null? ,value))


(jazz.encapsulate-class jazz.Null-Class)


(jazz.define-class jazz.Null jazz.List () jazz.Null-Class
  ())


(jazz.encapsulate-class jazz.Null)


;;;
;;;; Pair
;;;


(jazz.define-class jazz.Pair-Class jazz.List-Class (name fields ancestors descendants ascendant interfaces slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class
  ())


(jazz.define-method (jazz.of-type? (jazz.Pair-Class class) object)
  (%%pair? object))


(jazz.define-method (jazz.emit-specifier (jazz.Pair-Class class))
  'pair)


(jazz.define-method (jazz.emit-test (jazz.Pair-Class type) value environment)
  `(%%pair? ,value))


(jazz.encapsulate-class jazz.Pair-Class)


(jazz.define-class jazz.Pair jazz.List () jazz.Pair-Class
  ())


(jazz.encapsulate-class jazz.Pair)


;;;
;;;; String
;;;


(jazz.define-class jazz.String-Class jazz.Sequence-Class (name fields ancestors descendants ascendant interfaces slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class
  ())


(jazz.define-method (jazz.of-type? (jazz.String-Class class) object)
  (%%string? object))


(jazz.define-method (jazz.emit-specifier (jazz.String-Class class))
  'string)


(jazz.define-method (jazz.emit-test (jazz.String-Class type) value environment)
  `(%%string? ,value))


(jazz.encapsulate-class jazz.String-Class)


(jazz.define-class jazz.String jazz.Sequence () jazz.String-Class
  ())


(jazz.encapsulate-class jazz.String)


;;;
;;;; Vector
;;;


(jazz.define-class jazz.Vector-Class jazz.Sequence-Class (name fields ancestors descendants ascendant interfaces slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class
  ())


(jazz.define-method (jazz.of-type? (jazz.Vector-Class class) object)
  (%%vector? object))


(jazz.define-method (jazz.emit-specifier (jazz.Vector-Class class))
  'vector)


(jazz.define-method (jazz.emit-test (jazz.Vector-Class type) value environment)
  `(%%vector? ,value))


(jazz.encapsulate-class jazz.Vector-Class)


(jazz.define-class jazz.Vector jazz.Sequence () jazz.Vector-Class
  ())


(jazz.encapsulate-class jazz.Vector)


;;;
;;;; Port
;;;


(jazz.define-class jazz.Port-Class jazz.Class (name fields ancestors descendants ascendant interfaces slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class
  ())


(jazz.define-method (jazz.of-type? (jazz.Port-Class class) object)
  (%%port? object))


(jazz.define-method (jazz.emit-specifier (jazz.Port-Class class))
  'port)


(jazz.define-method (jazz.emit-test (jazz.Port-Class type) value environment)
  `(%%port? ,value))


(jazz.encapsulate-class jazz.Port-Class)


(jazz.define-class jazz.Port jazz.Object () jazz.Port-Class
  ())


(jazz.encapsulate-class jazz.Port)


;;;
;;;; Procedure
;;;


(jazz.define-class jazz.Procedure-Class jazz.Class (name fields ancestors descendants ascendant interfaces slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class
  ())


(jazz.define-method (jazz.of-type? (jazz.Procedure-Class class) object)
  (%%procedure? object))


#; ;; do this tomorrow I'm going nuts lol
(jazz.define-method (jazz.of-subtype? (jazz.Procedure-Class class) subtype)
  (or (nextmethod class subtype)
      (%%is? subtype jazz.Function-Type)))


(jazz.define-method (jazz.emit-specifier (jazz.Procedure-Class class))
  'procedure)


(jazz.define-method (jazz.emit-test (jazz.Procedure-Class type) value environment)
  `(%%procedure? ,value))


(jazz.encapsulate-class jazz.Procedure-Class)


(jazz.define-class jazz.Procedure jazz.Object () jazz.Procedure-Class
  ())


(jazz.encapsulate-class jazz.Procedure)


;;;
;;;; Foreign
;;;


(jazz.define-class jazz.Foreign-Class jazz.Class (name fields ancestors descendants ascendant interfaces slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class
  ())


(jazz.define-method (jazz.of-type? (jazz.Foreign-Class class) object)
  (%%foreign? object))


(jazz.define-method (jazz.emit-specifier (jazz.Foreign-Class class))
  'foreign)


(jazz.define-method (jazz.emit-test (jazz.Foreign-Class type) value environment)
  `(%%foreign? ,value))


(jazz.encapsulate-class jazz.Foreign-Class)


(jazz.define-class jazz.Foreign jazz.Object () jazz.Foreign-Class
  ())


(jazz.encapsulate-class jazz.Foreign)


;;;
;;;; Symbol
;;;


(jazz.define-class jazz.Symbol-Class jazz.Class (name fields ancestors descendants ascendant interfaces slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class
  ())


(jazz.define-method (jazz.of-type? (jazz.Symbol-Class class) object)
  (%%symbol? object))


(jazz.define-method (jazz.emit-specifier (jazz.Symbol-Class class))
  'symbol)


(jazz.define-method (jazz.emit-test (jazz.Symbol-Class type) value environment)
  `(%%symbol? ,value))


(jazz.encapsulate-class jazz.Symbol-Class)


(jazz.define-class jazz.Symbol jazz.Object () jazz.Symbol-Class
  ())


(jazz.encapsulate-class jazz.Symbol)


;;;
;;;; Keyword
;;;


(jazz.define-class jazz.Keyword-Class jazz.Class (name fields ancestors descendants ascendant interfaces slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class
  ())


(jazz.define-method (jazz.of-type? (jazz.Keyword-Class class) object)
  (%%keyword? object))


(jazz.define-method (jazz.emit-specifier (jazz.Keyword-Class class))
  'keyword)


(jazz.define-method (jazz.emit-test (jazz.Keyword-Class type) value environment)
  `(%%keyword? ,value))


(jazz.encapsulate-class jazz.Keyword-Class)


(jazz.define-class jazz.Keyword jazz.Object () jazz.Keyword-Class
  ())


(jazz.encapsulate-class jazz.Keyword)


;;;
;;;; Hashtable
;;;


(jazz.define-class jazz.Hashtable-Class jazz.Class (name fields ancestors descendants ascendant interfaces slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class
  ())


(jazz.define-method (jazz.of-type? (jazz.Hashtable-Class class) object)
  (%%hashtable? object))


(jazz.define-method (jazz.emit-specifier (jazz.Hashtable-Class class))
  'hashtable)


(jazz.define-method (jazz.emit-test (jazz.Hashtable-Class type) value environment)
  `(%%hashtable? ,value))


(jazz.encapsulate-class jazz.Hashtable-Class)


(jazz.define-class jazz.Hashtable jazz.Object () jazz.Hashtable-Class
  ())


(jazz.encapsulate-class jazz.Hashtable)


;;;
;;;; Types
;;;


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


(jazz.define-class jazz.Interface jazz.Category (name fields ancestors descendants) jazz.Class
  (ascendants
   rank
   virtual-size))


(define (jazz.new-interface class name ascendants)
  (let ((interface (jazz.allocate-interface class name (%%make-hashtable eq?) #f '() ascendants jazz.new-interface:next-rank 0)))
    (set! jazz.new-interface:next-rank (+ jazz.new-interface:next-rank 1))
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


;;tBool is_interface_subtype(jType target, jType type)
;;{
;;	jTypePtr	ptr_start = target->ancestorsGet() + target->class_ancestors_sizeGet();
;;	jTypePtr	ptr = target->ancestorsGet() + target->ancestors_sizeGet();
;;	
;;	while (--ptr >= ptr_start)
;;		if (*ptr == type)
;;			return true;
;;	
;;	return false;
;;}
(jazz.define-method (jazz.of-type? (jazz.Interface interface) object)
  (jazz.of-subtype? interface (%%class-of object)))


(define (jazz.update-interface interface)
  (receive (count added-methods) (jazz.update-interface-root-methods interface)
    (%%when (%%not-null? added-methods)
      (%%set-interface-virtual-size interface count)
      (let ((interface-rank (%%get-interface-rank interface)))
        (let iter ((category interface))
             (%%when (%%is? category jazz.Class)
               (let* ((interface-table (%%get-class-interface-table category))
                      (implementation-table (jazz.resize-vector (%%vector-ref interface-table interface-rank) count)))
                 (for-each (lambda (field)
                             (let ((implementation-rank (%%get-virtual-method-implementation-rank field))
                                   (implementation (%%get-method-node-implementation (%%get-virtual-method-implementation-tree field))))
                               (%%vector-set! implementation-table implementation-rank implementation)))
                           added-methods)
                 (%%vector-set! interface-table interface-rank implementation-table)))
             (for-each (lambda (descendant)
                         (iter descendant))
                       (%%get-category-descendants category)))))))


(define (jazz.update-interface-root-methods interface)
  (let* ((interface-rank (%%get-interface-rank interface))
         (count (%%get-interface-virtual-size interface))
         (added-methods '()))
    (%%iterate-hashtable (%%get-category-fields interface)
      (lambda (key field)
        (%%when (and (%%is? field jazz.Virtual-Method)
                     (%%not (%%get-virtual-method-implementation-rank field)))
          (%%set-virtual-method-category-rank field interface-rank)
          (%%set-virtual-method-implementation-rank field count)
          (set! count (+ count 1))
          (set! added-methods (cons field added-methods)))))
    (values count added-methods)))


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
        (and ascendant
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
  ())


(define (jazz.new-method name)
  (jazz.allocate-method jazz.Method name))


(define (jazz.method? object)
  (%%is? object jazz.Method))


(define (jazz.method-virtual? method)
  (%%not (%%is? method jazz.Final-Method)))


(define (jazz.locate-method-owner category method-name)
  (let iter ((category category))
    (cond ((not category)
           #f)
          ((%%get-category-field category method-name)
           category)
          ((%%is? category jazz.Class)
           (or (iter (%%get-class-ascendant category))
               (jazz.find-in iter (%%get-class-interfaces category))))
          ((%%is? category jazz.Interface)
           (jazz.find-in iter (%%get-interface-ascendants category))))))


(jazz.encapsulate-class jazz.Method)


;;;
;;;; Final Method
;;;


(jazz.define-class jazz.Final-Method jazz.Method (name) jazz.Object-Class
  (implementation))


(define (jazz.new-final-method name implementation)
  (jazz.allocate-final-method jazz.Final-Method name implementation))


(define (jazz.add-final-method class method-name method-implementation)
  (let ((owner (jazz.locate-method-owner class method-name)))
    (cond ((not owner)
           (jazz.create-final-method class method-name method-implementation))
          ((%%eq? owner class)
           (jazz.update-final-method class method-name method-implementation))
          (else
           (jazz.error "Cannot redefine virtual method: {a}" method-implementation)))))


(define (jazz.update-final-method class method-name method-implementation)
  (let ((field (%%get-category-field class method-name)))
    (if (%%is? field jazz.Final-Method)
        (%%set-final-method-implementation field method-implementation)
      (jazz.error "Cannot change method propagation to final: {a}" method-implementation))
    field))


(define (jazz.create-final-method class method-name method-implementation)
  (let ((method (jazz.new-final-method method-name method-implementation)))
    (jazz.add-field class method)
    method))


(jazz.encapsulate-class jazz.Final-Method)


;;;
;;;; Virtual Method
;;;


(jazz.define-class jazz.Virtual-Method jazz.Method (name) jazz.Object-Class
  (implementation-tree
   dispatch-type
   category-rank
   implementation-rank))


(define (jazz.new-virtual-method name implementation-tree dispatch-type category-rank implementation-rank)
  (jazz.allocate-virtual-method jazz.Virtual-Method name implementation-tree dispatch-type category-rank implementation-rank))


(define (jazz.add-virtual-method category method-name method-implementation)
  (let ((owner (jazz.locate-method-owner category method-name)))
    (cond ((not owner)
           (jazz.create-virtual-method category method-name method-implementation))
          ((%%eq? owner category)
           (jazz.update-virtual-method category method-name method-implementation))
          (else
           (jazz.error "Cannot rebase virtual method: {a}" method-implementation)))))


(define (jazz.update-virtual-method category method-name method-implementation)
  (let ((field (%%get-category-field category method-name)))
    (if (%%is? field jazz.Virtual-Method)
        (let ((node (%%get-virtual-method-implementation-tree field)))
          (%%set-method-node-implementation node method-implementation))
      (jazz.error "Cannot virtualize final method: {a}" method-implementation))
    field))


(define (jazz.create-virtual-method category method-name method-implementation)
  (let* ((dispatch-type (if (%%is? category jazz.Class) 'class 'interface))
         (node (jazz.new-method-node category method-implementation #f '()))
         (method (jazz.new-virtual-method method-name node dispatch-type #f #f)))
    (jazz.add-field category method)
    method))


(jazz.encapsulate-class jazz.Virtual-Method)


;;;
;;;; Method - Dispatch
;;;


(define (jazz.add-dispatch-method class method-name method-implementation)
  (let ((owner (jazz.locate-method-owner class method-name)))
    (cond ((not owner)
           (jazz.error "Cannot locate root method: {a}" method-implementation))
          ((%%eq? owner class)
           (jazz.error "Cannot remove root method: {a}" method-implementation))
          (else
           (let ((field (%%get-category-field owner method-name)))
             (cond ((%%is? field jazz.Virtual-Method)
                    (let ((root-node (%%get-virtual-method-implementation-tree field)))
                      (receive (start-node end-nodes) (jazz.create/update-dispatch-method root-node class method-implementation)
                        (let ((category-rank (%%get-virtual-method-category-rank field))
                              (implementation-rank (%%get-virtual-method-implementation-rank field)))
                          (jazz.update-method-tree (lambda (class)
                                                     (let* ((dispatch-table (case (%%get-virtual-method-dispatch-type field)
                                                                              ((class)     (%%get-class-class-table class))
                                                                              ((interface) (%%get-class-interface-table class))))
                                                            (method-table (%%vector-ref dispatch-table category-rank)))
                                                       (%%vector-set! method-table implementation-rank method-implementation)))
                                                   start-node end-nodes)
                          start-node))))
                   ((%%is? field jazz.Final-Method)
                    (jazz.error "Cannot remove final method: {a}" method-implementation))
                   (else
                    (error "Method jazz.add-dispatch-method unimplemented for Interface"))))))))


(define (jazz.create/update-dispatch-method root-node class method-implementation)
  (let ((node (jazz.locate-most-specific-method-node root-node class)))
    (if (%%eq? class (%%get-method-node-category node))
        (jazz.update-dispatch-method node class method-implementation)
      (jazz.create-dispatch-method node class method-implementation))))


(define (jazz.update-dispatch-method node class method-implementation)
  (%%set-method-node-implementation node method-implementation)
  (values node (%%get-method-node-children node)))


(define (jazz.create-dispatch-method node class method-implementation)
  (let* ((partition (jazz.partition (%%get-method-node-children node)
                                    (lambda (child)
                                      (let ((child-class (%%get-method-node-category child)))
                                        (%%subtype? class child-class)))))
         (new-children (%%cdr (or (assq #t partition) '(#t))))
         (old-children (%%cdr (or (assq #f partition) '(#f))))
         (new-node (jazz.new-method-node class method-implementation node new-children)))
    (for-each (lambda (child)
                (%%set-method-node-next-node child new-node)
                (%%set-method-node-next-implementation child method-implementation))
              new-children)
    (%%set-method-node-children node (%%cons new-node old-children))
    (values new-node new-children)))


;;;
;;;; Method Node
;;;


(jazz.define-class jazz.Method-Node jazz.Object () jazz.Object-Class
  (category
   implementation
   next-node
   next-implementation
   children))


(define (jazz.new-method-node category implementation next-node children)
  (let ((next-implementation (if next-node (%%get-method-node-implementation next-node) #f)))
    (jazz.allocate-method-node jazz.Method-Node category implementation next-node next-implementation children)))


(define (jazz.locate-most-specific-method-node node category)
  (let iter ((node node))
       (if (%%eq? category (%%get-method-node-category node))
           node ; exact match
         (let sub-iter ((children (%%get-method-node-children node)))
              (if (%%null? children)
                  node ; inherited match
                (let* ((child (%%car children))
                       (child-category (%%get-method-node-category child)))
                  (if (%%subtype? category child-category)
                      (iter child)
                    (sub-iter (%%cdr children)))))))))


(define (jazz.update-method-tree proc start-node end-nodes)
  (let ((end-categories (map (lambda (node)
                               (%%get-method-node-category node))
                             end-nodes)))
    (let iter ((category (%%get-method-node-category start-node)))
         (%%when (%%not (%%memq category end-categories))
           (proc category)
           (for-each (lambda (descendant)
                       (iter descendant))
                     (%%get-category-descendants category))))))


(define (jazz.call-into-abstract . rest)
  (error "cannot call abstract nextmethod"))


(jazz.encapsulate-class jazz.Method-Node)


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
