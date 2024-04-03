;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Class Runtime
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
;;;  Portions created by the Initial Developer are Copyright (C) 1996-2018
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


(block core.class-runtime


;;;
;;;; Runtime
;;;


(define jazz:new-interface-rank
  0)


;;;
;;;; Identifier
;;;


(define (jazz:dispatch? symbol)
  (and (%%symbol? symbol)
       (let ((name (%%symbol->string symbol)))
         (let ((len (%%string-length name)))
           (and (%%fx> len 1)
                (%%eqv? (%%string-ref name (%%fx- len 1))
                        #\~))))))


(define (jazz:dispatch->symbol dispatch)
  (let ((name (%%symbol->string dispatch)))
    (%%string->symbol (%%substring name 0 (%%fx- (%%string-length name) 1)))))


(define (jazz:composite-identifier? symbol)
  (and (%%symbol? symbol)
       (jazz:memstring #\. (%%symbol->string symbol))))


(define (jazz:composite-reference? symbol)
  (and (%%symbol? symbol)
       (jazz:memstring #\: (%%symbol->string symbol))))


(define (jazz:split-symbol symbol separator)
  (%%debug-assert (%%symbol? symbol)
    (map string->symbol (jazz:split-string (%%symbol->string symbol) separator))))


(define (jazz:split-identifier symbol)
  (jazz:split-symbol symbol #\.))


(define (jazz:split-reference symbol)
  (jazz:split-symbol symbol #\:))


(define (jazz:composite-namespace? symbol)
  (and (%%symbol? symbol)
       (jazz:memstring #\# (%%symbol->string symbol))))


(define (jazz:break-namespace symbol)
  (let ((str (%%symbol->string symbol)))
    (let ((n (jazz:string-find-reversed str #\#)))
      (if (%%not n)
          (values #f symbol)
        (let ((next (%%fx+ n 1)))
          (values (%%string->symbol (%%substring str 0 next))
                  (%%string->symbol (%%substring str next (%%string-length str)))))))))


(define (jazz:namespace-name symbol)
  (receive (namespace name) (jazz:break-namespace symbol)
    name))


;;;
;;;; Mutation
;;;


(define jazz:loading-module
  (%%make-parameter #f))


(define jazz:Mutations
  (%%make-table test: eq?))


(define (jazz:get-mutations)
  jazz:Mutations)


(define (jazz:reset-mutations)
  (set! jazz:Mutations (%%make-table test: eq?)))


(define (jazz:register-mutation obj)
  (%%table-set! jazz:Mutations obj #t))


;;;
;;;; Object
;;;


(define (jazz:inspect-object object)
  (%%assert (%%object? object)
    (let* ((size (%%object-length object))
           (content (%%make-vector size)))
      (let iter ((n 0))
        (if (%%fx< n size)
            (begin
              (%%vector-set! content n (%%object-ref object n))
              (iter (%%fx+ n 1)))))
      content)))


(define (jazz:object->vector obj)
  (%%vector-copy obj))


(define (jazz:vector->object vec)
  (%%jazzify (%%vector-copy vec)))


(define (jazz:object-copy obj)
  (%%jazzify (%%vector-copy obj)))


;;;
;;;; Core
;;;


(define jazz:Core-Classes
  (%%make-table test: eq?))


(define (jazz:get-core-classes)
  jazz:Core-Classes)


(define (jazz:core-class? name)
  (%%boolean (%%table-ref jazz:Core-Classes name #f)))


(define (jazz:get-core-class name)
  (%%table-ref jazz:Core-Classes name #f))


(define (jazz:set-core-class name class)
  (%%table-set! jazz:Core-Classes name class))


(define (jazz:set-core-class-redefined name core-class-locator)
  (%%table-set! jazz:Core-Classes name core-class-locator))


;;;
;;;; Category
;;;


(define (jazz:object? expr)
  (%%object? expr))


(define (jazz:primitive? expr)
  (%%not (%%object? expr)))


(define (jazz:subtype? target type)
  (%%debug-assert (and (jazz:type? target) (jazz:type? type))
    (%%boolean (%%subtype? target type))))


(define (jazz:subcategory? target category)
  (%%boolean (%%subcategory? target category)))


(define (jazz:subclass? target class)
  (%%boolean (%%subclass? target class)))


(define (jazz:get-category-descendants category)
  (%%get-category-descendants category))


(define (jazz:get-class-ascendant class)
  (%%get-class-ascendant class))


(jazz:define-variable-override jazz:object-of-class?
  (lambda (object class)
    (%%subclass? (%%get-object-class object) class)))


(define (jazz:collect-type type lst)
  (jazz:collect-if (lambda (obj)
                     (%%is? obj type))
                   lst))


(define (jazz:create-class-tables class)
  (jazz:create-class-interface-table class)
  (jazz:create-class-class-table class))


(define (jazz:create-core-class-tables class)
  (jazz:create-class-class-table class))


(define (jazz:create-type-class-tables class)
  (jazz:create-class-class-table class))


(define (jazz:create-class-interface-table class)
  (%%when (%%not (%%get-class-interface-table class))
    (let ((vtable (%%make-vector jazz:new-interface-rank #f))
          (ascendant (%%get-class-ascendant class)))
      (%%when ascendant
        (let ((ascendant-interface-table (%%get-class-interface-table ascendant)))
          (%%when ascendant-interface-table
            (let ((size (%%vector-length ascendant-interface-table)))
              (let iter ((i 0))
                   (%%when (%%fx< i size)
                     (let ((ascendant-vtable (%%vector-ref ascendant-interface-table i)))
                       (%%when ascendant-vtable
                         (%%vector-set! vtable i (%%vector-copy ascendant-vtable))))
                     (iter (%%fx+ i 1))))))))
      (jazz:vector-for-each (lambda (category)
                              (%%when (%%class-is? category jazz:Interface)
                                (let* ((rank (%%get-interface-rank category))
                                       (size (%%get-category-virtual-size category)))
                                  (%%when (%%not (%%vector-ref vtable rank))
                                    (let ((category-vtable (%%make-vector size jazz:call-into-incoherent))
                                          (class-name (%%get-category-identifier class))
                                          (category-identifier (%%get-category-identifier category)))
                                      (jazz:iterate-table (%%get-category-fields category)
                                        (lambda (field-name field)
                                          (%%when (%%is? field jazz:Method)
                                            (%%vector-set! category-vtable
                                                           (%%get-method-implementation-rank field)
                                                           (lambda (object . rest)
                                                             (jazz:call-into-abstract (%%get-category-identifier (jazz:class-of object)) field-name object rest))))))
                                      (%%vector-set! vtable rank category-vtable))))))
                            (%%get-category-ancestors class))
      (%%set-class-interface-table class vtable))))


(define (jazz:create-class-class-table class)
  (%%when (%%not (%%get-class-class-table class))
    (let ((ascendant (%%get-class-ascendant class)))
      (%%set-class-class-table class
        (if ascendant
            (let* ((ascendant-class-table (%%get-class-class-table ascendant))
                   (size (%%vector-length ascendant-class-table))
                   (vtable (%%make-vector (%%fx+ size 1) '#())))
              (let iter ((i 0))
                   (%%when (%%fx< i size)
                     (%%vector-set! vtable i (%%vector-copy (%%vector-ref ascendant-class-table i)))
                     (iter (%%fx+ i 1))))
              vtable)
          (%%make-vector 1 '#()))))))


(define (jazz:update-class class)
  (jazz:update-class-class-table class))


(define (jazz:update-class-class-table class)
  (let ((class-table (%%get-class-class-table class))
        (class-level (%%get-class-level class)))
    (jazz:iterate-table (%%get-category-fields class)
      (lambda (key field)
        (%%when (jazz:virtual-method? field)
          (if (%%not (%%get-method-category-rank field))
              (%%set-method-category-rank field class-level))
          (let ((class-virtual-size (%%get-category-virtual-size class)))
            (%%when (%%not (%%fx= class-virtual-size (%%vector-length (%%vector-ref class-table class-level))))
              (let iter ((class class))
                   (let ((class-table (%%get-class-class-table class)))
                     (let ((implementation-table (jazz:resize-vector (%%vector-ref class-table class-level) class-virtual-size)))
                       (%%vector-set! class-table class-level implementation-table))
                     (for-each iter (%%get-category-descendants class))))))
          (let ((root-implementation-table (%%vector-ref class-table class-level)))
            (let ((implementation-rank (%%get-method-implementation-rank field)))
              (let ((old-implementation (%%vector-ref root-implementation-table implementation-rank))
                    (new-implementation (%%get-method-node-implementation (%%get-method-implementation-tree field))))
                (%%when (%%neq? old-implementation new-implementation)
                  ;; method exists and has changed - update vtable + propagate to descendants
                  (let iter ((class class))
                       (let* ((class-table (%%get-class-class-table class))
                              (implementation-table (%%vector-ref class-table class-level)))
                         (%%when (%%eq? old-implementation (%%vector-ref implementation-table implementation-rank))
                           (%%vector-set! implementation-table implementation-rank new-implementation)
                           (for-each iter (%%get-category-descendants class))))))))))))))


;;;
;;;; Core
;;;


(define (jazz:add-slot class slot-name slot-initialize slot-allocate?)
  (let ((actual (%%get-category-field class slot-name))
        (eval? (%%eq? (jazz:walk-for) 'eval)))
    (cond (actual
           (if eval?
               (%%set-slot-initialize actual slot-initialize))
           actual)
          (else
           (let* ((instance-size (%%get-class-instance-size class))
                  (slot-offset (and slot-allocate? (if eval? #f instance-size)))
                  (slot (%%allocate-slot jazz:Slot slot-name slot-offset slot-initialize)))
             (jazz:add-field class slot)
             (%%set-class-slots class (%%append (%%get-class-slots class) (%%list slot)))
             (let ((class-instance-slots (%%get-class-instance-slots class)))
               (%%when (jazz:find-in (lambda (instance-slot)
                                       (%%eq? slot-name (%%get-field-name instance-slot)))
                                     class-instance-slots)
                 (jazz:error "Redefinition of slot {s} in {s}" slot-name (%%get-category-identifier class)))
               (%%set-class-instance-slots class (%%append class-instance-slots (%%list slot))))
             (%%when slot-allocate?
               (%%set-class-instance-size class (%%fx+ instance-size 1)))
             slot)))))


(define (jazz:add-core-virtual-method class method-name)
  (%%set-class-virtual-names class
    (%%append (%%get-class-virtual-names class)
              (%%list method-name)))
  (let ((method-rank (%%get-category-virtual-size class)))
    (%%set-category-virtual-size class (%%fx+ method-rank 1))
    method-rank))


(define (jazz:add-core-method-node class method-name implementation)
  (receive (root-class method-rank) (jazz:require-core-level/rank class method-name)
    (let ((root-level (%%get-class-level root-class))
          (root-size (%%length (%%get-class-virtual-names root-class))))
      (let iter ((class class))
           (let* ((class-table (%%get-class-class-table class))
                  (implementation-table (jazz:resize-vector (%%vector-ref class-table root-level) root-size)))
             (%%vector-set! implementation-table method-rank implementation)
             (%%vector-set! class-table root-level implementation-table))
           (for-each (lambda (descendant)
                       (iter descendant))
                     (%%get-category-descendants class))))))


(define (jazz:find-nextmethod class method-name)
  (receive (root-class method-rank) (jazz:require-core-level/rank class method-name)
    (let ((root-level (%%get-class-level root-class)))
      (if (eq? root-class class)
          (lambda (obj . rest)
            (jazz:error "No nextmethod for {s} on {s}" obj method-name))
        (%%class-dispatch (%%get-class-ascendant class) root-level method-rank)))))


(define (jazz:find-core-level/rank class method-name)
  (let iter ((class class))
       (if class
           (let ((method-rank (jazz:find-rank method-name (%%get-class-virtual-names class))))
             (if (%%not method-rank)
                 (iter (%%get-class-ascendant class))
               (values class method-rank)))
         #f)))


(define (jazz:require-core-level/rank class method-name)
  (or (jazz:find-core-level/rank class method-name)
      (jazz:error "Invalid core method: {s}" method-name)))


;;;
;;;; Define Class
;;;


(define (jazz:new-core-class class name fields ascendant)
  (define (compute-core-class-ancestors class ascendant)
    (if (%%not ascendant)
        (%%list class)
      (%%append (%%vector->list (%%get-category-ancestors ascendant)) (%%list class))))
  
  (let ((core-class
         (%%allocate-class
          ; Object
          class
          ; Category
          name
          fields
          0
          #f
          '()
          ; Class
          ascendant
          '()
          '()
          (if ascendant (%%get-class-instance-slots ascendant) '())
          (if ascendant (%%get-class-instance-size ascendant) 1)
          (if ascendant (%%fx+ (%%get-class-level ascendant) 1) 0)
          '()
          #f
          #f
          #f)))
    (%%set-category-ancestors core-class (%%list->vector (compute-core-class-ancestors core-class ascendant)))
    (%%when ascendant
      (%%set-category-descendants ascendant (%%cons core-class (%%get-category-descendants ascendant))))
    (jazz:create-core-class-tables core-class)
    core-class))


(define (jazz:new-type-class class name ascendant user-data)
  (define (compute-type-class-ancestors class ascendant)
    (%%append (%%vector->list (%%get-category-ancestors ascendant)) (%%list class)))
  
  (let ((type-class
         (%%allocate-class
          ; Object
          class
          ; Category
          name
          (%%make-table test: eq?)
          0
          #f
          '()
          ; Class
          ascendant
          '()
          '()
          (%%get-class-instance-slots ascendant)
          (%%get-class-instance-size ascendant)
          (%%fx+ (%%get-class-level ascendant) 1)
          '()
          #f
          #f
          user-data)))
    (%%set-category-ancestors type-class (%%list->vector (compute-type-class-ancestors type-class ascendant)))
    (jazz:create-type-class-tables type-class)
    type-class))


;;;
;;;; Object
;;;


(jazz:define-class-runtime jazz:Object #t)


(define (jazz:get-object-slot object slot-offset)
  (%%debug-assert (%%object? object)
    (if (%%integer? slot-offset)
        (%%object-ref object slot-offset)
      (jazz:unspecified))))


(define (jazz:set-object-slot object slot-offset value)
  (%%debug-assert (%%object? object)
    (%%debug-assert (%%integer? slot-offset)
      (%%object-set! object slot-offset value))))


(define (jazz:classname->string class)
  (if (%%null? class)
      "()"
    (%%symbol->string (%%get-category-identifier class))))


(jazz:define-virtual (jazz:initialize (jazz:Object object)))
(jazz:define-virtual (jazz:destroy (jazz:Object object)))
(jazz:define-virtual (jazz:call-print (jazz:Object object) output detail))
(jazz:define-virtual (jazz:print-object (jazz:Object object) output detail))
(jazz:define-virtual (jazz:tree-fold (jazz:Object expression) down up here seed environment))


(jazz:define-method (jazz:initialize (jazz:Object object))
  #f)


(jazz:define-method (jazz:destroy (jazz:Object object))
  #f)


(jazz:define-method (jazz:call-print (jazz:Object object) output detail)
  (jazz:print-object object output detail))


(jazz:define-method (jazz:print-object (jazz:Object object) output detail)
  (jazz:print-serial object output))


(define (jazz:print-serial object output)
  (let ((serial (object->serial-number object)))
    (display "#<jazz #" output)
    (display serial output)
    (display ">" output)))


(jazz:define-method (jazz:tree-fold (jazz:Object expression) down up here seed environment)
  (here expression seed environment))


;;;
;;;; Type
;;;


(jazz:define-class-runtime jazz:Type #t)


(jazz:define-virtual (jazz:of-type? (jazz:Type type) object))
(jazz:define-virtual (jazz:of-subtype? (jazz:Type type) subtype))
(jazz:define-virtual (jazz:specifiable? (jazz:Type type)))
(jazz:define-virtual (jazz:category-type? (jazz:Type type)))
(jazz:define-virtual (jazz:resolve-type (jazz:Type type)))
(jazz:define-virtual (jazz:emit-specifier (jazz:Type type)))
(jazz:define-virtual (jazz:emit-type (jazz:Type type) source-declaration walker resume environment backend))
(jazz:define-virtual (jazz:emit-test (jazz:Type type) value source-declaration walker resume environment backend))
(jazz:define-virtual (jazz:emit-cast (jazz:Type type) value source-declaration walker resume environment backend))


(jazz:define-method (jazz:of-type? (jazz:Type type) object)
  (jazz:of-subtype? type (jazz:class-of object)))


(jazz:define-method (jazz:of-subtype? (jazz:Type type) subtype)
  (jazz:error "Unable to test type on: {s}" type))


(jazz:define-method (jazz:specifiable? (jazz:Type type))
  #t)


(jazz:define-method (jazz:category-type? (jazz:Type type))
  #f)


(jazz:define-method (jazz:resolve-type (jazz:Type type))
  type)


(jazz:define-method (jazz:emit-specifier (jazz:Type type))
  (jazz:error "Unable to emit specifier for: {s}" type))


(jazz:define-method (jazz:emit-type (jazz:Type type) source-declaration walker resume environment backend)
  (jazz:error "Unable to emit type for: {s}" type))


(jazz:define-method (jazz:emit-test (jazz:Type type) value source-declaration walker resume environment backend)
  (let ((locator (jazz:emit-type type source-declaration walker resume environment backend)))
    `(%%is? ,value ,locator)))


(jazz:define-method (jazz:emit-cast (jazz:Type type) value source-declaration walker resume environment backend)
  `(if ,(jazz:emit-test type value source-declaration walker resume environment backend)
       ,value
     (jazz:type-error ,value ',(jazz:emit-specifier type))))


;; for bootstrapping the core methods of type
(define (jazz:bootstrap-type? object type-class)
  ;; should add a low-level test here if possible
  #t)


(define (jazz:type? object)
  (and (%%object? object)
       (%%is? object jazz:Type)))


;;;
;;;; Structure
;;;


(define jazz:structure-types
  '())


(define (jazz:register-structure-type test type)
  (set! jazz:structure-types (%%cons (%%cons test type) jazz:structure-types)))


(define (jazz:structure-type object)
  (let iter ((scan jazz:structure-types))
    (if (%%null? scan)
        jazz:Structure
      (if ((%%caar scan) object)
          (%%cdar scan)
        (iter (%%cdr scan))))))


;;;
;;;; Category
;;;


(jazz:define-class-runtime jazz:Category #t)


(define (jazz:category? object)
  (and (jazz:type? object)
       (jazz:category-type? object)))


(jazz:define-method (jazz:of-subtype? (jazz:Category type) subtype)
  (and (jazz:category-type? subtype)
       (jazz:vector-memq? type (%%get-category-ancestors subtype))))


(jazz:define-method (jazz:category-type? (jazz:Category type))
  #t)


(jazz:define-method (jazz:emit-type (jazz:Category type) source-declaration walker resume environment backend)
  (%%get-category-identifier type))


(jazz:define-virtual (jazz:update-category (jazz:Category category)))


(jazz:define-method (jazz:update-category (jazz:Category category))
  #f)


(define (jazz:is? object category)
  (%%boolean (%%is? object category)))


(define (jazz:is-not? object category)
  (%%boolean (%%not (%%is? object category))))


(define (jazz:get-category-identifier category)
  (%%debug-assert (jazz:category? category)
    (%%get-category-identifier category)))


(define (jazz:add-field category field)
  (%%set-category-field category (%%get-field-name field) field)
  (%%unspecified))


;;;
;;;; Class
;;;


(jazz:define-class-runtime jazz:Class #t)


(jazz:define-virtual (jazz:write-object (jazz:Class class) we obj))


(jazz:define-method (jazz:write-object (jazz:Class class) we obj)
  (%%default-wr we obj))


(define (jazz:new-class class-of-class identifier ascendant interfaces)
  (define (compute-class-ancestors class ascendant interfaces)
    (let ((ancestors '()))
      (let add-interfaces ((category class))
           (cond ((%%class? category)
                  (let ((ascendant (%%get-class-ascendant category)))
                    (%%when ascendant
                      (add-interfaces ascendant)))
                  (for-each add-interfaces (%%get-class-interfaces category)))
                 (else
                  (%%when (%%not (%%memq category ancestors))
                    (set! ancestors (%%cons category ancestors))
                    (for-each add-interfaces (%%get-interface-ascendants category))))))
      (let add-classes ((class class))
           (%%when class
             (set! ancestors (%%cons class ancestors))
             (add-classes (%%get-class-ascendant class))))
      ancestors))

  ;; this should be made into a call to jazz:new somehow
  (let ((class (%%make-object class-of-class (%%get-class-instance-size class-of-class))))
    (%%set-category-identifier class identifier)
    (%%set-category-fields class (%%make-table test: eq?))
    (%%set-category-virtual-size class 0)
    (%%set-category-ancestors class #f)
    (%%set-category-descendants class '())
    (%%set-class-ascendant class ascendant)
    (%%set-class-interfaces class interfaces)
    (%%set-class-slots class '())
    (%%set-class-instance-slots class (if ascendant (%%get-class-instance-slots ascendant) '()))
    (%%set-class-instance-size class (if ascendant (%%get-class-instance-size ascendant) jazz:object-size))
    (%%set-class-level class (if ascendant (%%fx+ (%%get-class-level ascendant) 1) 0))
    (%%set-class-virtual-names class '())
    (%%set-class-class-table class #f)
    (%%set-class-interface-table class #f)
    (%%set-category-ancestors class (%%list->vector (compute-class-ancestors class ascendant interfaces)))
    (%%when ascendant
      (jazz:register-mutation ascendant)
      (%%set-category-descendants ascendant (%%cons class (%%get-category-descendants ascendant))))
    (jazz:create-class-tables class)
    (jazz:initialize-slots class)
    ((%%class-dispatch class-of-class 0 0) class)
    class))


(define (jazz:class? object)
  (%%class-is? object jazz:Class))


(define (jazz:class-of expr)
  (%%class-of-impl expr))


(define (jazz:class-subtype? target class)
  (%%class-subtype? target class))


(define (jazz:set-object-class object class)
  (%%set-object-class object class))


(jazz:define-method (jazz:of-type? (jazz:Class class) object)
  (%%class-subtype? (jazz:class-of object) class))


(jazz:define-method (jazz:update-category (jazz:Class class))
  (jazz:update-class class))


(define (jazz:get-class-level class)
  (%%get-class-level class))


(define (jazz:slot-form? form)
  (and (%%pair? form)
       (%%eq? (%%car form) 'slot)))


(define (jazz:keyword-constructor class)
  (%%debug-assert (%%class? class)
    (lambda rest
      (let ((object (%%make-object class (%%get-class-instance-size class))))
        (jazz:initialize-object object rest)
        object))))


(define (jazz:new-instance class . rest)
  (%%debug-assert (%%class? class)
    (let ((object (%%make-object class (%%get-class-instance-size class))))
      (jazz:initialize-object object rest)
      object)))


(define (jazz:initialize-object object initargs)
  (let ((class (%%get-object-class object)))
    (jazz:initialize-slots object)
    (let iter ((scan initargs))
         (if (%%not (%%null? scan))
             (let ((key (%%car scan))
                   (remain (%%cdr scan)))
               (%%debug-assert (and (%%keyword? key) (%%not (%%null? remain)))
                 (let ((value (%%car remain))
                       (remain (%%cdr remain)))
                   (let ((field (jazz:category-field class (%%string->symbol (%%keyword->string key)))))
                     (%%debug-assert (%%class-is? field jazz:Slot)
                       (let ((offset (%%get-slot-offset field)))
                         (if offset
                             (jazz:set-object-slot object offset value)))))
                   (iter remain))))))
    (jazz:initialize object)
    object))


(define (jazz:new class . rest)
  (%%debug-assert (%%class? class)
    (let ((object (%%make-object class (%%get-class-instance-size class))))
      (jazz:initialize-slots object)
      (apply (%%class-dispatch class 0 0) object rest)
      object)))


(define (jazz:nascent-new class . rest)
  (%%debug-assert (%%class? class)
    (let ((object (%%make-object class (%%get-class-instance-size class))))
      (jazz:initialize-slots object)
      object)))


(define (jazz:iterate-descendants-tree class proc)
  (let iter ((class class))
    (proc class)
    (for-each iter (%%get-category-descendants class))))


(define (jazz:$$object$$ class . rest)
  (%%jazzify (apply vector class rest)))


;;;
;;;; Object-Class
;;;


(jazz:define-class-runtime jazz:Object-Class #t)


(jazz:define-method (jazz:of-subtype? (jazz:Object-Class class) subtype)
  (if (%%object-class? class)
      #t
    (nextmethod class subtype)))


(jazz:define-method (jazz:emit-specifier (jazz:Object-Class class))
  'Object)


;;;
;;;; Field
;;;


(jazz:define-class-runtime jazz:Field #t)


(define (jazz:field? object)
  (%%class-is? object jazz:Field))


(define (jazz:field-name field)
  (%%get-field-name field))


(define (jazz:category-field category field-name)
  (or (%%get-category-field category field-name)
      (let ((ascendant (%%get-class-ascendant category)))
        (and ascendant
             (jazz:category-field ascendant field-name)))))


(define (jazz:require-object-field object name)
  (let* ((class (%%get-object-class object))
         (field (jazz:category-field class name)))
    (if (%%not field)
        (jazz:error "Unknown field '{s} of {s}" name (%%get-category-identifier (%%get-object-class object)))
      field)))


;;;
;;;; Slot
;;;


(jazz:define-class-runtime jazz:Slot #t)


(define (jazz:new-slot slot-name slot-offset slot-initialize)
  (jazz:allocate-slot slot-name slot-offset slot-initialize))


(define (jazz:slot? object)
  (%%class-is? object jazz:Slot))


(define (jazz:slot-ref object slot)
  (let ((offset (%%get-slot-offset slot)))
    (if offset
        (jazz:get-object-slot object offset)
      (jazz:get-dynamic object slot (%%get-slot-initialize slot)))))


(define (jazz:slot-set! object slot value)
  (let ((offset (%%get-slot-offset slot)))
    (if offset
        (jazz:set-object-slot object offset value)
      (jazz:set-dynamic object slot value))))


(define (jazz:slot-value object slot-name)
  (jazz:slot-ref object (jazz:require-object-field object slot-name)))


(define (jazz:set-slot-value object slot-name value)
  (jazz:slot-set! object (jazz:require-object-field object slot-name) value))


(define (jazz:initialize-slots object)
  (let ((slots (%%get-class-instance-slots (%%get-object-class object))))
    (for-each (lambda (slot)
                (let ((initialize (%%get-slot-initialize slot)))
                  (%%when initialize
                    (let ((offset (%%get-slot-offset slot)))
                      (%%when offset
                        (%%object-set! object offset (initialize object)))))))
              slots)))


(define jazz:dynamics-mutex
  (make-mutex 'dynamics))


(define jazz:dynamics
  (%%make-table test: eq? weak-keys: #t))


(define (jazz:require-dynamics object)
  (or (%%table-ref jazz:dynamics object #f)
      (let ((dynamics (%%make-table test: eq?)))
        (%%table-set! jazz:dynamics object dynamics)
        dynamics)))


(define (jazz:get-dynamic object slot initialize)
  (mutex-lock! jazz:dynamics-mutex)
  (let ((dynamics (jazz:require-dynamics object)))
    (let ((value (%%table-ref dynamics slot slot)))
      (let ((effective-value
              (if (%%eq? value slot)
                  (let ((initialize-value (if initialize (initialize object) (%%unspecified))))
                    (%%table-set! dynamics slot initialize-value)
                    initialize-value)
                value)))
        (mutex-unlock! jazz:dynamics-mutex)
        effective-value))))


(define (jazz:set-dynamic object slot value)
  (mutex-lock! jazz:dynamics-mutex)
  (%%table-set! (jazz:require-dynamics object) slot value)
  (mutex-unlock! jazz:dynamics-mutex)
  (%%unspecified))


;;;
;;;; Interface
;;;


(jazz:define-class-runtime jazz:Interface #t)


(define (jazz:new-interface class identifier ascendants)
  (define (compute-interface-ancestors interface ascendants)
    (jazz:remove-duplicates
      (%%apply append (%%cons (map (lambda (ascendant)
                                     (%%vector->list (%%get-category-ancestors ascendant)))
                                   ascendants)
                              (%%list (%%list interface))))))
  
  (let ((interface (%%allocate-interface class identifier (%%make-table test: eq?) 0 #f '() ascendants jazz:new-interface-rank)))
    (set! jazz:new-interface-rank (%%fx+ jazz:new-interface-rank 1))
    (%%set-category-ancestors interface (%%list->vector (compute-interface-ancestors interface ascendants)))
    (for-each (lambda (ascendant)
                (%%set-category-descendants ascendant (%%cons class (%%get-category-descendants ascendant))))
              ascendants)
    interface))


(define (jazz:interface? object)
  (%%class-is? object jazz:Interface))


;;tBool is_interface_subtype(jType target, jType type)
;;{
;;  jTypePtr ptr_start = target->ancestorsGet() + target->class_ancestors_sizeGet();
;;  jTypePtr ptr = target->ancestorsGet() + target->ancestors_sizeGet();
;;
;;  while (--ptr >= ptr_start)
;;      if (*ptr == type)
;;          return true;
;;
;;  return false;
;;}
(jazz:define-method (jazz:of-type? (jazz:Interface interface) object)
  (jazz:of-subtype? interface (jazz:class-of object)))


(jazz:define-method (jazz:update-category (jazz:Interface interface))
  (jazz:update-interface interface))


(define (jazz:update-interface interface)
  (define (update-interface-root-methods interface)
    (let* ((interface-rank (%%get-interface-rank interface))
           (added-methods '()))
      (jazz:iterate-table (%%get-category-fields interface)
        (lambda (key field)
          (%%when (and (jazz:virtual-method? field)
                       (%%not (%%get-method-category-rank field)))
            (%%set-method-category-rank field interface-rank)
            (set! added-methods (%%cons field added-methods)))))
      added-methods))

  (let ((added-methods (update-interface-root-methods interface)))
    (%%when (%%not-null? added-methods)
      (let ((interface-rank (%%get-interface-rank interface)))
        (let iter ((category interface))
             (%%when (%%class-is? category jazz:Class)
               (let* ((interface-table (%%get-class-interface-table category))
                      (implementation-table (jazz:resize-vector (%%vector-ref interface-table interface-rank) (%%get-category-virtual-size interface))))
                 (for-each (lambda (field)
                             (let ((implementation-rank (%%get-method-implementation-rank field))
                                   (implementation (%%get-method-node-implementation (%%get-method-implementation-tree field))))
                               (%%vector-set! implementation-table implementation-rank implementation)))
                           added-methods)
                 (%%vector-set! interface-table interface-rank implementation-table)))
             (for-each (lambda (descendant)
                         (iter descendant))
                       (%%get-category-descendants category)))))))


;;;
;;;; Class Bootstrap
;;;


(jazz:define-class-bootstrap jazz:Object)
(jazz:define-class-bootstrap jazz:Type)
(jazz:define-class-bootstrap jazz:Category)
(jazz:define-class-bootstrap jazz:Class)
(jazz:define-class-bootstrap jazz:Object-Class)
(jazz:define-class-bootstrap jazz:Field)
(jazz:define-class-bootstrap jazz:Slot)
(jazz:define-class-bootstrap jazz:Interface)


;;;
;;;; Bool
;;;


(jazz:define-class-runtime jazz:Bool-Class)


(jazz:define-method (jazz:of-type? (jazz:Bool-Class class) object)
  #t)


(jazz:define-method (jazz:emit-specifier (jazz:Bool-Class class))
  'bool)


(jazz:define-method (jazz:emit-test (jazz:Bool-Class type) value source-declaration walker resume environment backend)
  #t)


(jazz:define-method (jazz:emit-cast (jazz:Bool-Class type) value source-declaration walker resume environment backend)
  value)


(jazz:define-class-runtime jazz:Bool)


;;;
;;;; Boolean
;;;


(jazz:define-class-runtime jazz:Boolean-Class)


(jazz:define-method (jazz:of-type? (jazz:Boolean-Class class) object)
  (%%boolean? object))


(jazz:define-method (jazz:emit-specifier (jazz:Boolean-Class class))
  'boolean)


(jazz:define-method (jazz:emit-test (jazz:Boolean-Class type) value source-declaration walker resume environment backend)
  `(%%boolean? ,value))


(jazz:define-class-runtime jazz:Boolean)


;;;
;;;; Char
;;;


(jazz:define-class-runtime jazz:Char-Class)


(jazz:define-method (jazz:of-type? (jazz:Char-Class class) object)
  (%%char? object))


(jazz:define-method (jazz:emit-specifier (jazz:Char-Class class))
  'char)


(jazz:define-method (jazz:emit-test (jazz:Char-Class type) value source-declaration walker resume environment backend)
  `(%%char? ,value))


(jazz:define-class-runtime jazz:Char)


;;;
;;;; Numeric
;;;


(jazz:define-class-runtime jazz:Numeric-Class)


(jazz:define-method (jazz:emit-specifier (jazz:Numeric-Class class))
  'numeric)


(jazz:define-class-runtime jazz:Numeric)


;;;
;;;; Number
;;;


(jazz:define-class-runtime jazz:Number-Class)


(jazz:define-method (jazz:of-type? (jazz:Number-Class class) object)
  (%%number? object))


(jazz:define-method (jazz:emit-specifier (jazz:Number-Class class))
  'number)


(jazz:define-method (jazz:emit-test (jazz:Number-Class type) value source-declaration walker resume environment backend)
  `(%%number? ,value))


(jazz:define-class-runtime jazz:Number)


;;;
;;;; Complex
;;;


(jazz:define-class-runtime jazz:Complex-Class)


(jazz:define-method (jazz:of-type? (jazz:Complex-Class class) object)
  (%%complex? object))


(jazz:define-method (jazz:emit-specifier (jazz:Complex-Class class))
  'complex)


(jazz:define-method (jazz:emit-test (jazz:Complex-Class type) value source-declaration walker resume environment backend)
  `(%%complex? ,value))


(jazz:define-class-runtime jazz:Complex)


;;;
;;;; Real
;;;


(jazz:define-class-runtime jazz:Real-Class)


(jazz:define-method (jazz:of-type? (jazz:Real-Class class) object)
  (%%real? object))


(jazz:define-method (jazz:emit-specifier (jazz:Real-Class class))
  'real)


(jazz:define-method (jazz:emit-test (jazz:Real-Class type) value source-declaration walker resume environment backend)
  `(%%real? ,value))


(jazz:define-class-runtime jazz:Real)


;;;
;;;; Rational
;;;


(jazz:define-class-runtime jazz:Rational-Class)


(jazz:define-method (jazz:of-type? (jazz:Rational-Class class) object)
  (%%rational? object))


(jazz:define-method (jazz:emit-specifier (jazz:Rational-Class class))
  'rational)


(jazz:define-method (jazz:emit-test (jazz:Rational-Class type) value source-declaration walker resume environment backend)
  `(%%rational? ,value))


(jazz:define-class-runtime jazz:Rational)


;;;
;;;; Integer
;;;


(jazz:define-class-runtime jazz:Integer-Class)


(jazz:define-method (jazz:of-type? (jazz:Integer-Class class) object)
  (%%integer? object))


(jazz:define-method (jazz:emit-specifier (jazz:Integer-Class class))
  'int)


(jazz:define-method (jazz:emit-test (jazz:Integer-Class type) value source-declaration walker resume environment backend)
  `(%%integer? ,value))


(jazz:define-class-runtime jazz:Integer)


;;;
;;;; Fixnum
;;;


(jazz:define-class-runtime jazz:Fixnum-Class)


(jazz:define-method (jazz:of-type? (jazz:Fixnum-Class class) object)
  (%%fixnum? object))


(jazz:define-method (jazz:emit-specifier (jazz:Fixnum-Class class))
  'fx)


(jazz:define-method (jazz:emit-test (jazz:Fixnum-Class type) value source-declaration walker resume environment backend)
  `(%%fixnum? ,value))


(jazz:define-class-runtime jazz:Fixnum)


;;;
;;;; Ratnum
;;;


(jazz:define-class-runtime jazz:Ratnum-Class)


(jazz:define-method (jazz:of-type? (jazz:Ratnum-Class class) object)
  (%%ratnum? object))


(jazz:define-method (jazz:emit-specifier (jazz:Ratnum-Class class))
  'rt)


(jazz:define-method (jazz:emit-test (jazz:Ratnum-Class type) value source-declaration walker resume environment backend)
  `(%%ratnum? ,value))


(jazz:define-class-runtime jazz:Ratnum)


;;;
;;;; Flonum
;;;


(jazz:define-class-runtime jazz:Flonum-Class)


(jazz:define-method (jazz:of-type? (jazz:Flonum-Class class) object)
  (%%flonum? object))


(jazz:define-method (jazz:emit-specifier (jazz:Flonum-Class class))
  'fl)


(jazz:define-method (jazz:emit-test (jazz:Flonum-Class type) value source-declaration walker resume environment backend)
  `(%%flonum? ,value))


(jazz:define-method (jazz:emit-cast (jazz:Flonum-Class type) value source-declaration walker resume environment backend)
  `(cond ((%%flonum? ,value)
          ,value)
         ((%%fixnum? ,value)
          (%%fixnum->flonum ,value))
         ((%%ratnum? ,value)
          (%%ratnum->flonum ,value))
         (else
          (jazz:type-error ,value ',(jazz:emit-specifier type)))))


(jazz:define-class-runtime jazz:Flonum)


;;;
;;;; Flovec
;;;


(jazz:define-class-runtime jazz:Flovec-Class)


(jazz:define-method (jazz:of-type? (jazz:Flovec-Class class) object)
  (%%flonum? object))


(jazz:define-method (jazz:emit-specifier (jazz:Flovec-Class class))
  'fv)


(jazz:define-method (jazz:emit-test (jazz:Flovec-Class type) value source-declaration walker resume environment backend)
  `(or (%%flonum? ,value)
       (%%f64vector? ,value)))


(jazz:define-method (jazz:emit-cast (jazz:Flovec-Class type) value source-declaration walker resume environment backend)
  `(if (or (%%flonum? ,value)
           (%%f64vector? ,value))
       ,value
     (if (%%fixnum? ,value)
         (%%fixnum->flonum ,value)
       (jazz:type-error ,value ',(jazz:emit-specifier type)))))


(jazz:define-class-runtime jazz:Flovec)


;;;
;;;; S64
;;;


(jazz:define-class-runtime jazz:S64-Class)


(jazz:define-method (jazz:of-type? (jazz:S64-Class class) object)
  (%%integer? object))


(jazz:define-method (jazz:emit-specifier (jazz:S64-Class class))
  's64)


(jazz:define-method (jazz:emit-test (jazz:S64-Class type) value source-declaration walker resume environment backend)
  `(%%integer? ,value))


(jazz:define-class-runtime jazz:S64)


;;;
;;;; Sequence
;;;


(jazz:define-class-runtime jazz:Sequence-Class)


(jazz:define-method (jazz:emit-specifier (jazz:Sequence-Class class))
  'sequence)


(jazz:define-class-runtime jazz:Sequence)


;;;
;;;; List
;;;


(jazz:define-class-runtime jazz:List-Class)


(jazz:define-method (jazz:emit-specifier (jazz:List-Class class))
  'list)


(jazz:define-method (jazz:emit-test (jazz:List-Class type) value source-declaration walker resume environment backend)
  `(or (%%null? ,value)
       (%%pair? ,value)))


(jazz:define-class-runtime jazz:List)


;;;
;;;; Null
;;;


(jazz:define-class-runtime jazz:Null-Class)


(jazz:define-method (jazz:of-type? (jazz:Null-Class class) object)
  (%%null? object))


(jazz:define-method (jazz:emit-specifier (jazz:Null-Class class))
  'null)


(jazz:define-method (jazz:emit-test (jazz:Null-Class type) value source-declaration walker resume environment backend)
  `(%%null? ,value))


(jazz:define-class-runtime jazz:Null)


;;;
;;;; Pair
;;;


(jazz:define-class-runtime jazz:Pair-Class)


(jazz:define-method (jazz:of-type? (jazz:Pair-Class class) object)
  (%%pair? object))


(jazz:define-method (jazz:emit-specifier (jazz:Pair-Class class))
  'pair)


(jazz:define-method (jazz:emit-test (jazz:Pair-Class type) value source-declaration walker resume environment backend)
  `(%%pair? ,value))


(jazz:define-class-runtime jazz:Pair)


;;;
;;;; String
;;;


(jazz:define-class-runtime jazz:String-Class)


(jazz:define-method (jazz:of-type? (jazz:String-Class class) object)
  (%%string? object))


(jazz:define-method (jazz:emit-specifier (jazz:String-Class class))
  'string)


(jazz:define-method (jazz:emit-test (jazz:String-Class type) value source-declaration walker resume environment backend)
  `(%%string? ,value))


(jazz:define-class-runtime jazz:String)


;;;
;;;; Vector
;;;


(jazz:define-class-runtime jazz:Vector-Class)


(jazz:define-method (jazz:of-type? (jazz:Vector-Class class) object)
  (%%vector? object))


(jazz:define-method (jazz:emit-specifier (jazz:Vector-Class class))
  'vector)


(jazz:define-method (jazz:emit-test (jazz:Vector-Class type) value source-declaration walker resume environment backend)
  `(%%vector? ,value))


(jazz:define-class-runtime jazz:Vector)


;;;
;;;; S8Vector
;;;


(jazz:define-class-runtime jazz:S8Vector-Class)


(jazz:define-method (jazz:of-type? (jazz:S8Vector-Class class) object)
  (%%s8vector? object))


(jazz:define-method (jazz:emit-specifier (jazz:S8Vector-Class class))
  's8vector)


(jazz:define-method (jazz:emit-test (jazz:S8Vector-Class type) value source-declaration walker resume environment backend)
  `(%%s8vector? ,value))


(jazz:define-class-runtime jazz:S8Vector)


;;;
;;;; U8Vector
;;;


(jazz:define-class-runtime jazz:U8Vector-Class)


(jazz:define-method (jazz:of-type? (jazz:U8Vector-Class class) object)
  (%%u8vector? object))


(jazz:define-method (jazz:emit-specifier (jazz:U8Vector-Class class))
  'u8vector)


(jazz:define-method (jazz:emit-test (jazz:U8Vector-Class type) value source-declaration walker resume environment backend)
  `(%%u8vector? ,value))


(jazz:define-class-runtime jazz:U8Vector)


;;;
;;;; S16Vector
;;;


(jazz:define-class-runtime jazz:S16Vector-Class)


(jazz:define-method (jazz:of-type? (jazz:S16Vector-Class class) object)
  (%%s16vector? object))


(jazz:define-method (jazz:emit-specifier (jazz:S16Vector-Class class))
  's16vector)


(jazz:define-method (jazz:emit-test (jazz:S16Vector-Class type) value source-declaration walker resume environment backend)
  `(%%s16vector? ,value))


(jazz:define-class-runtime jazz:S16Vector)


;;;
;;;; U16Vector
;;;


(jazz:define-class-runtime jazz:U16Vector-Class)


(jazz:define-method (jazz:of-type? (jazz:U16Vector-Class class) object)
  (%%u16vector? object))


(jazz:define-method (jazz:emit-specifier (jazz:U16Vector-Class class))
  'u16vector)


(jazz:define-method (jazz:emit-test (jazz:U16Vector-Class type) value source-declaration walker resume environment backend)
  `(%%u16vector? ,value))


(jazz:define-class-runtime jazz:U16Vector)


;;;
;;;; S32Vector
;;;


(jazz:define-class-runtime jazz:S32Vector-Class)


(jazz:define-method (jazz:of-type? (jazz:S32Vector-Class class) object)
  (%%s32vector? object))


(jazz:define-method (jazz:emit-specifier (jazz:S32Vector-Class class))
  's32vector)


(jazz:define-method (jazz:emit-test (jazz:S32Vector-Class type) value source-declaration walker resume environment backend)
  `(%%s32vector? ,value))


(jazz:define-class-runtime jazz:S32Vector)


;;;
;;;; U32Vector
;;;


(jazz:define-class-runtime jazz:U32Vector-Class)


(jazz:define-method (jazz:of-type? (jazz:U32Vector-Class class) object)
  (%%u32vector? object))


(jazz:define-method (jazz:emit-specifier (jazz:U32Vector-Class class))
  'u32vector)


(jazz:define-method (jazz:emit-test (jazz:U32Vector-Class type) value source-declaration walker resume environment backend)
  `(%%u32vector? ,value))


(jazz:define-class-runtime jazz:U32Vector)


;;;
;;;; S64Vector
;;;


(jazz:define-class-runtime jazz:S64Vector-Class)


(jazz:define-method (jazz:of-type? (jazz:S64Vector-Class class) object)
  (%%s64vector? object))


(jazz:define-method (jazz:emit-specifier (jazz:S64Vector-Class class))
  's64vector)


(jazz:define-method (jazz:emit-test (jazz:S64Vector-Class type) value source-declaration walker resume environment backend)
  `(%%s64vector? ,value))


(jazz:define-class-runtime jazz:S64Vector)


;;;
;;;; U64Vector
;;;


(jazz:define-class-runtime jazz:U64Vector-Class)


(jazz:define-method (jazz:of-type? (jazz:U64Vector-Class class) object)
  (%%u64vector? object))


(jazz:define-method (jazz:emit-specifier (jazz:U64Vector-Class class))
  'u64vector)


(jazz:define-method (jazz:emit-test (jazz:U64Vector-Class type) value source-declaration walker resume environment backend)
  `(%%u64vector? ,value))


(jazz:define-class-runtime jazz:U64Vector)


;;;
;;;; F32Vector
;;;


(jazz:define-class-runtime jazz:F32Vector-Class)


(jazz:define-method (jazz:of-type? (jazz:F32Vector-Class class) object)
  (%%f32vector? object))


(jazz:define-method (jazz:emit-specifier (jazz:F32Vector-Class class))
  'f32vector)


(jazz:define-method (jazz:emit-test (jazz:F32Vector-Class type) value source-declaration walker resume environment backend)
  `(%%f32vector? ,value))


(jazz:define-class-runtime jazz:F32Vector)


;;;
;;;; F64Vector
;;;


(jazz:define-class-runtime jazz:F64Vector-Class)


(jazz:define-method (jazz:of-type? (jazz:F64Vector-Class class) object)
  (%%f64vector? object))


(jazz:define-method (jazz:emit-specifier (jazz:F64Vector-Class class))
  'f64vector)


(jazz:define-method (jazz:emit-test (jazz:F64Vector-Class type) value source-declaration walker resume environment backend)
  `(%%f64vector? ,value))


(jazz:define-class-runtime jazz:F64Vector)


;;;
;;;; FixedVector
;;;


(jazz:define-class-runtime jazz:FixedVector-Class)


(jazz:define-method (jazz:of-type? (jazz:FixedVector-Class class) object)
  (and (%%vector? object)
       (%%fx= (%%vector-length object) (%%get-class-user-data class))))


(jazz:define-method (jazz:emit-specifier (jazz:FixedVector-Class class))
  (%%string->symbol (%%string-append "vector#" (%%number->string (%%get-class-user-data class)))))


(jazz:define-method (jazz:emit-test (jazz:FixedVector-Class class) value source-declaration walker resume environment backend)
  `(and (%%vector? ,value)
        (%%fx= (%%vector-length ,value) ,(%%get-class-user-data class))))


(jazz:define-class-runtime jazz:FixedVector)


;;;
;;;; FixedS8Vector
;;;


(jazz:define-class-runtime jazz:FixedS8Vector-Class)


(jazz:define-method (jazz:of-type? (jazz:FixedS8Vector-Class class) object)
  (and (%%s8vector? object)
       (%%fx= (%%s8vector-length object) (%%get-class-user-data class))))


(jazz:define-method (jazz:emit-specifier (jazz:FixedS8Vector-Class class))
  (%%string->symbol (%%string-append "s8vector#" (%%number->string (%%get-class-user-data class)))))


(jazz:define-method (jazz:emit-test (jazz:FixedS8Vector-Class class) value source-declaration walker resume environment backend)
  `(and (%%s8vector? ,value)
        (%%fx= (%%s8vector-length ,value) ,(%%get-class-user-data class))))


(jazz:define-class-runtime jazz:FixedS8Vector)


;;;
;;;; FixedU8Vector
;;;


(jazz:define-class-runtime jazz:FixedU8Vector-Class)


(jazz:define-method (jazz:of-type? (jazz:FixedU8Vector-Class class) object)
  (and (%%u8vector? object)
       (%%fx= (%%u8vector-length object) (%%get-class-user-data class))))


(jazz:define-method (jazz:emit-specifier (jazz:FixedU8Vector-Class class))
  (%%string->symbol (%%string-append "u8vector#" (%%number->string (%%get-class-user-data class)))))


(jazz:define-method (jazz:emit-test (jazz:FixedU8Vector-Class class) value source-declaration walker resume environment backend)
  `(and (%%u8vector? ,value)
        (%%fx= (%%u8vector-length ,value) ,(%%get-class-user-data class))))


(jazz:define-class-runtime jazz:FixedU8Vector)


;;;
;;;; FixedS16Vector
;;;


(jazz:define-class-runtime jazz:FixedS16Vector-Class)


(jazz:define-method (jazz:of-type? (jazz:FixedS16Vector-Class class) object)
  (and (%%s16vector? object)
       (%%fx= (%%s16vector-length object) (%%get-class-user-data class))))


(jazz:define-method (jazz:emit-specifier (jazz:FixedS16Vector-Class class))
  (%%string->symbol (%%string-append "s16vector#" (%%number->string (%%get-class-user-data class)))))


(jazz:define-method (jazz:emit-test (jazz:FixedS16Vector-Class class) value source-declaration walker resume environment backend)
  `(and (%%s16vector? ,value)
        (%%fx= (%%s16vector-length ,value) ,(%%get-class-user-data class))))


(jazz:define-class-runtime jazz:FixedS16Vector)


;;;
;;;; FixedU16Vector
;;;


(jazz:define-class-runtime jazz:FixedU16Vector-Class)


(jazz:define-method (jazz:of-type? (jazz:FixedU16Vector-Class class) object)
  (and (%%u16vector? object)
       (%%fx= (%%u16vector-length object) (%%get-class-user-data class))))


(jazz:define-method (jazz:emit-specifier (jazz:FixedU16Vector-Class class))
  (%%string->symbol (%%string-append "u16vector#" (%%number->string (%%get-class-user-data class)))))


(jazz:define-method (jazz:emit-test (jazz:FixedU16Vector-Class class) value source-declaration walker resume environment backend)
  `(and (%%u16vector? ,value)
        (%%fx= (%%u16vector-length ,value) ,(%%get-class-user-data class))))


(jazz:define-class-runtime jazz:FixedU16Vector)


;;;
;;;; FixedS32Vector
;;;


(jazz:define-class-runtime jazz:FixedS32Vector-Class)


(jazz:define-method (jazz:of-type? (jazz:FixedS32Vector-Class class) object)
  (and (%%s32vector? object)
       (%%fx= (%%s32vector-length object) (%%get-class-user-data class))))


(jazz:define-method (jazz:emit-specifier (jazz:FixedS32Vector-Class class))
  (%%string->symbol (%%string-append "s32vector#" (%%number->string (%%get-class-user-data class)))))


(jazz:define-method (jazz:emit-test (jazz:FixedS32Vector-Class class) value source-declaration walker resume environment backend)
  `(and (%%s32vector? ,value)
        (%%fx= (%%s32vector-length ,value) ,(%%get-class-user-data class))))


(jazz:define-class-runtime jazz:FixedS32Vector)


;;;
;;;; FixedU32Vector
;;;


(jazz:define-class-runtime jazz:FixedU32Vector-Class)


(jazz:define-method (jazz:of-type? (jazz:FixedU32Vector-Class class) object)
  (and (%%u32vector? object)
       (%%fx= (%%u32vector-length object) (%%get-class-user-data class))))


(jazz:define-method (jazz:emit-specifier (jazz:FixedU32Vector-Class class))
  (%%string->symbol (%%string-append "u32vector#" (%%number->string (%%get-class-user-data class)))))


(jazz:define-method (jazz:emit-test (jazz:FixedU32Vector-Class class) value source-declaration walker resume environment backend)
  `(and (%%u32vector? ,value)
        (%%fx= (%%u32vector-length ,value) ,(%%get-class-user-data class))))


(jazz:define-class-runtime jazz:FixedU32Vector)


;;;
;;;; FixedS64Vector
;;;


(jazz:define-class-runtime jazz:FixedS64Vector-Class)


(jazz:define-method (jazz:of-type? (jazz:FixedS64Vector-Class class) object)
  (and (%%s64vector? object)
       (%%fx= (%%s64vector-length object) (%%get-class-user-data class))))


(jazz:define-method (jazz:emit-specifier (jazz:FixedS64Vector-Class class))
  (%%string->symbol (%%string-append "s64vector#" (%%number->string (%%get-class-user-data class)))))


(jazz:define-method (jazz:emit-test (jazz:FixedS64Vector-Class class) value source-declaration walker resume environment backend)
  `(and (%%s64vector? ,value)
        (%%fx= (%%s64vector-length ,value) ,(%%get-class-user-data class))))


(jazz:define-class-runtime jazz:FixedS64Vector)


;;;
;;;; FixedU64Vector
;;;


(jazz:define-class-runtime jazz:FixedU64Vector-Class)


(jazz:define-method (jazz:of-type? (jazz:FixedU64Vector-Class class) object)
  (and (%%u64vector? object)
       (%%fx= (%%u64vector-length object) (%%get-class-user-data class))))


(jazz:define-method (jazz:emit-specifier (jazz:FixedU64Vector-Class class))
  (%%string->symbol (%%string-append "u64vector#" (%%number->string (%%get-class-user-data class)))))


(jazz:define-method (jazz:emit-test (jazz:FixedU64Vector-Class class) value source-declaration walker resume environment backend)
  `(and (%%u64vector? ,value)
        (%%fx= (%%u64vector-length ,value) ,(%%get-class-user-data class))))


(jazz:define-class-runtime jazz:FixedU64Vector)


;;;
;;;; FixedF32Vector
;;;


(jazz:define-class-runtime jazz:FixedF32Vector-Class)


(jazz:define-method (jazz:of-type? (jazz:FixedF32Vector-Class class) object)
  (and (%%f32vector? object)
       (%%fx= (%%f32vector-length object) (%%get-class-user-data class))))


(jazz:define-method (jazz:emit-specifier (jazz:FixedF32Vector-Class class))
  (%%string->symbol (%%string-append "f32vector#" (%%number->string (%%get-class-user-data class)))))


(jazz:define-method (jazz:emit-test (jazz:FixedF32Vector-Class class) value source-declaration walker resume environment backend)
  `(and (%%f32vector? ,value)
        (%%fx= (%%f32vector-length ,value) ,(%%get-class-user-data class))))


(jazz:define-class-runtime jazz:FixedF32Vector)


;;;
;;;; FixedF64Vector
;;;


(jazz:define-class-runtime jazz:FixedF64Vector-Class)


(jazz:define-method (jazz:of-type? (jazz:FixedF64Vector-Class class) object)
  (and (%%f64vector? object)
       (%%fx= (%%f64vector-length object) (%%get-class-user-data class))))


(jazz:define-method (jazz:emit-specifier (jazz:FixedF64Vector-Class class))
  (%%string->symbol (%%string-append "f64vector#" (%%number->string (%%get-class-user-data class)))))


(jazz:define-method (jazz:emit-test (jazz:FixedF64Vector-Class class) value source-declaration walker resume environment backend)
  `(and (%%f64vector? ,value)
        (%%fx= (%%f64vector-length ,value) ,(%%get-class-user-data class))))


(jazz:define-class-runtime jazz:FixedF64Vector)


;;;
;;;; Structure
;;;


(jazz:define-class-runtime jazz:Structure-Class)


(jazz:define-method (jazz:of-type? (jazz:Structure-Class class) object)
  (%%structure? object))


(jazz:define-method (jazz:emit-specifier (jazz:Structure-Class class))
  'structure)


(jazz:define-method (jazz:emit-test (jazz:Structure-Class type) value source-declaration walker resume environment backend)
  `(%%structure? ,value))


(jazz:define-class-runtime jazz:Structure)


;;;
;;;; Port
;;;


(jazz:define-class-runtime jazz:Port-Class)


(jazz:define-method (jazz:of-type? (jazz:Port-Class class) object)
  (%%port? object))


(jazz:define-method (jazz:emit-specifier (jazz:Port-Class class))
  'port)


(jazz:define-method (jazz:emit-test (jazz:Port-Class type) value source-declaration walker resume environment backend)
  `(%%port? ,value))


(jazz:define-class-runtime jazz:Port)


(jazz:register-structure-type port? jazz:Port)


;;;
;;;; Continuation
;;;


(jazz:define-class-runtime jazz:Continuation-Class)


(jazz:define-method (jazz:of-type? (jazz:Continuation-Class class) object)
  (%%continuation? object))


(jazz:define-method (jazz:emit-specifier (jazz:Continuation-Class class))
  'continuation)


(jazz:define-method (jazz:emit-test (jazz:Continuation-Class type) value source-declaration walker resume environment backend)
  `(%%continuation? ,value))


(jazz:define-class-runtime jazz:Continuation)


;;;
;;;; Procedure
;;;


(jazz:define-class-runtime jazz:Procedure-Class)


(jazz:define-method (jazz:of-type? (jazz:Procedure-Class class) object)
  (%%procedure? object))


(jazz:define-method (jazz:of-subtype? (jazz:Procedure-Class class) subtype)
  (or (nextmethod class subtype)
      #; ;; fix because it is defined only is a later unit
      (%%is? subtype jazz:Function-Type)))


(jazz:define-method (jazz:emit-specifier (jazz:Procedure-Class class))
  'procedure)


(jazz:define-method (jazz:emit-test (jazz:Procedure-Class type) value source-declaration walker resume environment backend)
  `(%%procedure? ,value))


(jazz:define-class-runtime jazz:Procedure)


;;;
;;;; Symbol
;;;


(jazz:define-class-runtime jazz:Symbol-Class)


(jazz:define-method (jazz:of-type? (jazz:Symbol-Class class) object)
  (%%symbol? object))


(jazz:define-method (jazz:emit-specifier (jazz:Symbol-Class class))
  'symbol)


(jazz:define-method (jazz:emit-test (jazz:Symbol-Class type) value source-declaration walker resume environment backend)
  `(%%symbol? ,value))


(jazz:define-class-runtime jazz:Symbol)


;;;
;;;; Keyword
;;;


(jazz:define-class-runtime jazz:Keyword-Class)


(jazz:define-method (jazz:of-type? (jazz:Keyword-Class class) object)
  (%%keyword? object))


(jazz:define-method (jazz:emit-specifier (jazz:Keyword-Class class))
  'keyword)


(jazz:define-method (jazz:emit-test (jazz:Keyword-Class type) value source-declaration walker resume environment backend)
  `(%%keyword? ,value))


(jazz:define-class-runtime jazz:Keyword)


;;;
;;;; Table
;;;


(jazz:define-class-runtime jazz:Table-Class)


(jazz:define-method (jazz:of-type? (jazz:Table-Class class) object)
  (%%table? object))


(jazz:define-method (jazz:emit-specifier (jazz:Table-Class class))
  'table)


(jazz:define-method (jazz:emit-test (jazz:Table-Class type) value source-declaration walker resume environment backend)
  `(%%table? ,value))


(jazz:define-class-runtime jazz:Table)


(jazz:register-structure-type table? jazz:Table)


;;;
;;;; Thread
;;;


(jazz:define-class-runtime jazz:Thread-Class)


(jazz:define-method (jazz:of-type? (jazz:Thread-Class class) object)
  (%%thread? object))


(jazz:define-method (jazz:emit-specifier (jazz:Thread-Class class))
  'thread)


(jazz:define-method (jazz:emit-test (jazz:Thread-Class type) value source-declaration walker resume environment backend)
  `(%%thread? ,value))


(jazz:define-class-runtime jazz:Thread)


(jazz:register-structure-type thread? jazz:Thread)


;;;
;;;; Promise
;;;


(jazz:define-class-runtime jazz:Promise-Class)


(jazz:define-method (jazz:emit-specifier (jazz:Promise-Class class))
  'promise)


(jazz:define-class-runtime jazz:Promise)


;;;
;;;; Foreign
;;;


(jazz:define-class-runtime jazz:Foreign-Class)


(jazz:define-method (jazz:of-type? (jazz:Foreign-Class class) object)
  (%%foreign? object))


(jazz:define-method (jazz:emit-specifier (jazz:Foreign-Class class))
  'foreign)


(jazz:define-method (jazz:emit-test (jazz:Foreign-Class type) value source-declaration walker resume environment backend)
  `(%%foreign? ,value))


(jazz:define-class-runtime jazz:Foreign)


;;;
;;;; Values
;;;


(jazz:define-class-runtime jazz:Values-Class)


(jazz:define-method (jazz:of-type? (jazz:Values-Class class) object)
  (%%values? object))


(jazz:define-method (jazz:emit-specifier (jazz:Values-Class class))
  'values)


(jazz:define-method (jazz:emit-test (jazz:Values-Class type) value source-declaration walker resume environment backend)
  `(%%values? ,value))


(jazz:define-class-runtime jazz:Values)


;;;
;;;; EOF
;;;


(jazz:define-class-runtime jazz:EOF-Class)


(jazz:define-method (jazz:of-type? (jazz:EOF-Class class) object)
  (%%eof-object? object))


(jazz:define-method (jazz:emit-specifier (jazz:EOF-Class class))
  'eof)


(jazz:define-method (jazz:emit-test (jazz:EOF-Class type) value source-declaration walker resume environment backend)
  `(%%eof-object? ,value))


(jazz:define-class-runtime jazz:EOF)


;;;
;;;; Unspecified
;;;


(jazz:define-class-runtime jazz:Unspecified-Class)


(jazz:define-method (jazz:of-type? (jazz:Unspecified-Class class) object)
  (%%unspecified? object))


(jazz:define-method (jazz:emit-specifier (jazz:Unspecified-Class class))
  'unspecified)


(jazz:define-method (jazz:emit-test (jazz:Unspecified-Class type) value source-declaration walker resume environment backend)
  `(%%unspecified? ,value))


(jazz:define-class-runtime jazz:Unspecified)


;;;
;;;; Marker
;;;


(jazz:define-class-runtime jazz:Marker-Class)


(jazz:define-method (jazz:of-type? (jazz:Marker-Class class) object)
  (jazz:marker? object))


(jazz:define-method (jazz:emit-specifier (jazz:Marker-Class class))
  'marker)


(jazz:define-method (jazz:emit-test (jazz:Marker-Class type) value source-declaration walker resume environment backend)
  `(jazz:marker? ,value))


(jazz:define-class-runtime jazz:Marker)


;;;
;;;; Types
;;;


(define jazz:subtypes
  (%%make-vector 32 #f))

(define jazz:specialtypes
  (%%make-vector 16 #f))

;; quicky until we find a clean solution with Marc
(define jazz:table-type
  (%%structure-type (make-table)))

;; quicky until we find a clean solution with Marc
(define jazz:port-type
  (%%structure-type (open-output-string)))

;; quicky until we find a clean solution with Marc
(define jazz:thread-type
  (%%structure-type (current-thread)))

(%%vector-set! jazz:subtypes jazz:subtype-vector       jazz:Vector)
(%%vector-set! jazz:subtypes jazz:subtype-pair         jazz:Pair)
(%%vector-set! jazz:subtypes jazz:subtype-ratnum       jazz:Rational)
(%%vector-set! jazz:subtypes jazz:subtype-cpxnum       jazz:Complex)
(%%vector-set! jazz:subtypes jazz:subtype-symbol       jazz:Symbol)
(%%vector-set! jazz:subtypes jazz:subtype-keyword      jazz:Keyword)
(%%vector-set! jazz:subtypes jazz:subtype-continuation jazz:Continuation)
(%%vector-set! jazz:subtypes jazz:subtype-procedure    jazz:Procedure)
(%%vector-set! jazz:subtypes jazz:subtype-string       jazz:String)
(%%vector-set! jazz:subtypes jazz:subtype-flonum       jazz:Flonum)
(%%vector-set! jazz:subtypes jazz:subtype-bignum       jazz:Rational)
(%%vector-set! jazz:subtypes jazz:subtype-foreign      jazz:Foreign)
(%%vector-set! jazz:subtypes jazz:subtype-s8vector     jazz:S8Vector)
(%%vector-set! jazz:subtypes jazz:subtype-u8vector     jazz:U8Vector)
(%%vector-set! jazz:subtypes jazz:subtype-s16vector    jazz:S16Vector)
(%%vector-set! jazz:subtypes jazz:subtype-u16vector    jazz:U16Vector)
(%%vector-set! jazz:subtypes jazz:subtype-s32vector    jazz:S32Vector)
(%%vector-set! jazz:subtypes jazz:subtype-u32vector    jazz:U32Vector)
(%%vector-set! jazz:subtypes jazz:subtype-s64vector    jazz:S64Vector)
(%%vector-set! jazz:subtypes jazz:subtype-u64vector    jazz:U64Vector)
(%%vector-set! jazz:subtypes jazz:subtype-f32vector    jazz:F32Vector)
(%%vector-set! jazz:subtypes jazz:subtype-f64vector    jazz:F64Vector)
(%%vector-set! jazz:subtypes jazz:subtype-boxvalues    jazz:Values)

(%%vector-set! jazz:specialtypes #x0 jazz:Boolean)
(%%vector-set! jazz:specialtypes #x1 jazz:Boolean)
(%%vector-set! jazz:specialtypes #x2 jazz:Null)
(%%vector-set! jazz:specialtypes #x3 jazz:EOF)
(%%vector-set! jazz:specialtypes #x4 jazz:Unspecified)
(%%vector-set! jazz:specialtypes #x5 jazz:Marker)   ;; absent
(%%vector-set! jazz:specialtypes #x6 jazz:Marker)   ;; unbound
(%%vector-set! jazz:specialtypes #x7 jazz:Marker)   ;; unbound2
(%%vector-set! jazz:specialtypes #x8 jazz:Marker)   ;; optional
(%%vector-set! jazz:specialtypes #x9 jazz:Marker)   ;; key
(%%vector-set! jazz:specialtypes #xA jazz:Marker)   ;; rest
(%%vector-set! jazz:specialtypes #xB jazz:Marker)   ;; unused
(%%vector-set! jazz:specialtypes #xC jazz:Marker)   ;; deleted
(%%vector-set! jazz:specialtypes #xD jazz:Marker)   ;; promise
(%%vector-set! jazz:specialtypes #xE jazz:Marker)   ;; unassigned1
(%%vector-set! jazz:specialtypes #xF jazz:Marker)   ;; unassigned2


;;;
;;;; Property
;;;


(jazz:define-class-runtime jazz:Property)


(define (jazz:new-property slot-name slot-offset slot-initialize slot-getter slot-setter)
  (jazz:allocate-property slot-name slot-offset slot-initialize slot-getter slot-setter))


(define (jazz:property? object)
  (%%class-is? object jazz:Property))


(define (jazz:property-getter property)
  (%%get-property-getter property))


(define (jazz:property-setter property)
  (%%get-property-setter property))


(define (jazz:all-properties category)
  (let iter ((slots (%%get-class-instance-slots category)))
     (cond ((%%null? slots) '())
           ((jazz:property? (%%car slots)) (%%cons (%%car slots) (iter (%%cdr slots))))
           (else (iter (%%cdr slots))))))


(define (jazz:add-property class slot-name slot-initialize slot-allocate? slot-getter slot-setter)
  (let ((actual (%%get-category-field class slot-name)))
    (cond (actual
           (%%set-slot-initialize actual slot-initialize)
           (%%set-property-getter actual slot-getter)
           (%%set-property-setter actual slot-setter)
           actual)
          (else
           (let* ((instance-size (%%get-class-instance-size class))
                  (slot-offset (and slot-allocate? instance-size))
                  (slot (jazz:new-property slot-name slot-offset slot-initialize slot-getter slot-setter)))
             (jazz:add-field class slot)
             (%%set-class-slots class (%%append (%%get-class-slots class) (%%list slot)))
             (let ((class-instance-slots (%%get-class-instance-slots class)))
               (%%when (jazz:find-in (lambda (instance-slot)
                                       (%%eq? slot-name (%%get-field-name instance-slot)))
                                     class-instance-slots)
                 (jazz:error "Redefinition of property {s} in {s}" slot-name (%%get-category-identifier class)))
               (%%set-class-instance-slots class (%%append class-instance-slots (%%list slot))))
             (%%when slot-allocate?
               (%%set-class-instance-size class (%%fx+ instance-size 1)))
             slot)))))


;;;
;;;; Method
;;;


(jazz:define-class-runtime jazz:Method)


(define (jazz:method? object)
  (%%class-is? object jazz:Method))


(define (jazz:final-method? field)
  (and (%%class-is? field jazz:Method)
       (%%eq? (%%get-method-dispatch-type field) 'final)))


(define (jazz:virtual-method? field)
  (and (%%class-is? field jazz:Method)
       (%%neq? (%%get-method-dispatch-type field) 'final)))


(define (jazz:locate-method-owner category method-name)
  (let iter ((category category))
    (cond ((%%not category)
           #f)
          ((%%get-category-field category method-name)
           category)
          ((%%class-is? category jazz:Class)
           (or (iter (%%get-class-ascendant category))
               (jazz:find-in iter (%%get-class-interfaces category))))
          ((%%class-is? category jazz:Interface)
           (jazz:find-in iter (%%get-interface-ascendants category))))))


(define (jazz:iterate-class-overrides class proc)
  (let ((ascendant (%%get-class-ascendant class)))
    (let iter ((ancestor ascendant))
         (if ancestor
             (begin
               (jazz:iterate-table (%%get-category-fields ancestor)
                 (lambda (name field)
                   (if (and (%%is? field jazz:Method)
                            (let ((dispatch-type (%%get-method-dispatch-type field)))
                              (or (%%eq? dispatch-type 'class)
                                  (%%eq? dispatch-type 'interface)))
                            (let ((category-rank (%%get-method-category-rank field))
                                  (implementation-rank (%%get-method-implementation-rank field)))
                              (%%neq? (%%class-dispatch class category-rank implementation-rank)
                                      (%%class-dispatch ascendant category-rank implementation-rank))))
                       (proc field))))
               (iter (%%get-class-ascendant ancestor)))))))


(define (jazz:update-method class method-name method-implementation)
  (let ((owner (jazz:locate-method-owner class method-name)))
    (let ((method (%%get-category-field owner method-name))
          (method-locator (%%compose-reference (%%get-category-identifier class) method-name)))
      (jazz:global-set! method-locator method-implementation)
      (if (%%eq? owner class)
          (case (%%get-method-dispatch-type method)
            ((class interface) (jazz:add-virtual-method class method-name method-implementation))
            ((final) (jazz:add-final-method class method-name method-implementation)))
        (jazz:add-method-node class method-name method-implementation)))))

  
;;;
;;;; Final Method
;;;


(define (jazz:new-final-method name implementation)
  (jazz:allocate-method name 'final implementation #f #f #f))


(define (jazz:add-final-method class method-name method-implementation)
  (define (create-final-method class method-name method-implementation)
    (let ((method (jazz:new-final-method method-name method-implementation)))
      (jazz:add-field class method)
      method))
  
  (define (update-final-method class method-name method-implementation)
    (let ((field (%%get-category-field class method-name)))
      (if (jazz:final-method? field)
          (%%set-method-implementation field method-implementation)
        (jazz:error "Cannot change method propagation to final: {a}" method-implementation))
      field))
  
  (let ((owner (jazz:locate-method-owner class method-name)))
    (cond ((%%not owner)
           (create-final-method class method-name method-implementation))
          ((%%eq? owner class)
           (update-final-method class method-name method-implementation))
          (else
           (jazz:error "Cannot redefine virtual method: {a}" method-implementation)))))


;;;
;;;; Virtual Method
;;;


(define (jazz:new-virtual-method name dispatch-type implementation-tree category-rank implementation-rank)
  (jazz:allocate-method name dispatch-type #f implementation-tree category-rank implementation-rank))


(define (jazz:add-virtual-method category method-name method-implementation)
  (define (create-virtual-method category method-name method-implementation)
    (let* ((dispatch-type (if (%%class-is? category jazz:Class) 'class 'interface))
           (node (jazz:new-method-node category method-implementation #f '()))
           (method (jazz:new-virtual-method method-name dispatch-type node #f #f))
           (virtual-size (%%get-category-virtual-size category))
           (core-level/rank (and (%%class-is? category jazz:Class) (jazz:find-core-level/rank category method-name))))
      (if core-level/rank
          (receive (level rank) core-level/rank
            (%%set-method-implementation-rank method rank))
        (begin
          (%%set-method-implementation-rank method virtual-size)
          (%%set-category-virtual-size category (%%fx+ virtual-size 1))))
      (jazz:add-field category method)
      (jazz:update-category category)
      (%%get-method-implementation-rank method)))
  
  (define (update-virtual-method category method-name method-implementation)
    (let ((field (%%get-category-field category method-name)))
      (if (jazz:virtual-method? field)
          (let ((node (%%get-method-implementation-tree field)))
            (%%set-method-node-implementation node method-implementation)
            (for-each (lambda (child)
                        (%%set-method-node-next-implementation child method-implementation))
                      (%%get-method-node-children node)))
        (jazz:error "Cannot virtualize final method: {a}" method-implementation))
      (jazz:update-category category)
      (%%get-method-implementation-rank field)))
  
  (let ((owner (jazz:locate-method-owner category method-name)))
    (cond ((%%not owner)
           (create-virtual-method category method-name method-implementation))
          ((%%eq? owner category)
           (update-virtual-method category method-name method-implementation))
          (else
           (jazz:error "Cannot rebase virtual method: {a}" method-implementation)))))


;;;
;;;; Method Dispatch
;;;


(define (jazz:add-method-node class method-name method-implementation)
  (define (create/update-method-node root-node class method-implementation)
    (let ((node (locate-most-specific-method-node root-node class)))
      (if (%%eq? class (%%get-method-node-category node)) ; exact match
          (update-method-node node class method-implementation)
        (create-method-node node class method-implementation))))
  
  (define (locate-most-specific-method-node node category)
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
  
  (define (create-method-node node class method-implementation)
    (let* ((partition (jazz:partition (%%get-method-node-children node)
                                      (lambda (child)
                                        (let ((child-class (%%get-method-node-category child)))
                                          (%%subtype? class child-class)))
                                      assv))
           (new-children (%%cdr (or (%%assq #t partition) '(#t))))
           (old-children (%%cdr (or (%%assq #f partition) '(#f))))
           (new-node (jazz:new-method-node class method-implementation node new-children)))
      (for-each (lambda (child)
                  (%%set-method-node-next-node child new-node)
                  (%%set-method-node-next-implementation child method-implementation))
                new-children)
      (%%set-method-node-children node (%%cons new-node old-children))
      (values new-node new-children)))
  
  (define (update-method-node node class method-implementation)
    (%%set-method-node-implementation node method-implementation)
    (for-each (lambda (child)
                (%%set-method-node-next-implementation child method-implementation))
              (%%get-method-node-children node))
    (values node (%%get-method-node-children node)))

  (define (update-method-tree proc start-node end-nodes)
    (let ((end-categories (map (lambda (node)
                                 (%%get-method-node-category node))
                               end-nodes)))
      (let iter ((category (%%get-method-node-category start-node)))
           (%%when (%%not (%%memq category end-categories))
             (proc category)
             (for-each (lambda (descendant)
                         (iter descendant))
                       (%%get-category-descendants category))))))

  (let ((owner (jazz:locate-method-owner class method-name)))
    (cond ((%%not owner)
           (jazz:error "Cannot locate root method: {a}" method-implementation))
          ((%%eq? owner class)
           (jazz:error "Cannot remove root method: {a}" method-implementation))
          (else
           (let ((field (%%get-category-field owner method-name)))
             (cond ((jazz:virtual-method? field)
                    (let ((root-node (%%get-method-implementation-tree field)))
                      (receive (start-node end-nodes) (create/update-method-node root-node class method-implementation)
                        (let ((category-rank (%%get-method-category-rank field))
                              (implementation-rank (%%get-method-implementation-rank field)))
                          (update-method-tree (lambda (class)
                                                (let* ((dispatch-table (case (%%get-method-dispatch-type field)
                                                                         ((class)     (%%get-class-class-table class))
                                                                         ((interface) (%%get-class-interface-table class))))
                                                       (method-table (%%vector-ref dispatch-table category-rank)))
                                                  (%%vector-set! method-table implementation-rank method-implementation)))
                                                   start-node end-nodes)
                          start-node))))
                   ((jazz:final-method? field)
                    (jazz:error "Overriding final method: {a}" method-implementation))
                   (else
                    (error "Method jazz:add-method-node unimplemented for Interface"))))))))


;;;
;;;; Method Node
;;;


(jazz:define-class-runtime jazz:Method-Node)


(define (jazz:new-method-node category implementation next-node children)
  (let ((next-implementation (if next-node (%%get-method-node-implementation next-node) #f)))
    (jazz:allocate-method-node category implementation next-node next-implementation children)))


(define (jazz:call-into-incoherent . rest)
  (jazz:error "Dispatch table contains non-method, parameters are {l}" rest))


(define (jazz:call-into-abstract class method object arguments)
  (jazz:error "Cannot call abstract method {s} on a {s}" method class))


;;;
;;;; Call-Site
;;;


(jazz:define-class-runtime jazz:Call-Site)


(define (jazz:new-call-site name id properties procedure)
  (jazz:allocate-call-site name id properties procedure #f))


;;;
;;;; Queue
;;;


(jazz:define-class-runtime jazz:Queue)


(define (jazz:new-queue)
  (jazz:allocate-queue '() #f #f))


(define (jazz:queue-empty? queue)
  (%%null? (%%get-queue-head queue)))


(define (jazz:queue-length queue)
  (%%length (%%get-queue-head queue)))


(define (jazz:enqueue queue object)
  (let ((added (%%cons object '())))
    (jazz:enqueue-impl queue added
      (lambda (tail)
        (%%set-cdr! tail added)))
    (%%set-queue-tail queue added)
    (%%set-queue-shared queue #f)))


(define (jazz:enqueue-list queue lst)
  (%%when (%%not-null? lst)
    (jazz:enqueue-impl queue lst
      (lambda (tail)
        (%%set-cdr! tail lst)
        (%%set-queue-tail queue tail)))
    (%%set-queue-shared queue lst)))


(define (jazz:enqueue-impl queue added proc)
  (define (stitch-head added)
    (let ((tail (%%get-queue-tail queue)))
      (if tail
          (%%set-cdr! tail added)
        (%%set-queue-head queue added))))
  
  (let ((shared (%%get-queue-shared queue)))
    (if shared
        (let ((copy (jazz:list-copy (%%get-queue-shared queue))))
          (stitch-head copy)
          (proc (jazz:last-pair copy)))
      (stitch-head added))))


(define (jazz:dequeue queue)
  (let ((head (%%get-queue-head queue)))
    (if (%%null? head)
        (error "Queue is empty")
      (let ((next (%%cdr head)))
        (cond ((%%eq? head (%%get-queue-tail queue))
               (%%set-queue-tail queue #f))
              ((%%eq? head (%%get-queue-shared queue))
               (%%set-queue-shared queue (if (%%null? next) #f next))))
        (%%set-queue-head queue next)
        (%%car head)))))


(define (jazz:queue-list queue)
  (%%get-queue-head queue))


;; should be made to work with shared content
(define (jazz:trim-queue queue keep)
  (define (tail lst n)
    (if (or (%%null? lst) (%%fx= n 0))
        lst
      (tail (%%cdr lst) (%%fx- n 1))))
  
  (if (%%fx= keep 0)
      (let ((trimmed (%%length (%%get-queue-head queue))))
        (jazz:reset-queue queue)
        trimmed)
    (let ((tail (tail (%%get-queue-head queue) (%%fx- keep 1))))
      (if (and (%%not-null? tail) (%%not-null? (%%cdr tail)))
          (let ((trimmed (%%length (%%cdr tail))))
            (%%set-cdr! tail '())
            (%%set-queue-tail queue tail)
            trimmed)
        0))))


(define (jazz:reset-queue queue)
  (%%set-queue-head queue '())
  (%%set-queue-tail queue #f)
  (%%set-queue-shared queue #f))


;;;
;;;; Output Hook
;;;


(jazz:define-variable jazz:print-hook
  #f)


(%%wr-set!
  (lambda (we obj)
    (cond ((and (%%not (%%jazz? obj)) (%%record? obj))
           (%%default-wr we (jazz:record->vector obj)))
          ((and (%%jazz? obj) jazz:print-hook)
           (jazz:print-hook obj (jazz:writeenv-port we) (jazz:writeenv-style we)))
          (else
           (jazz:write-object (jazz:class-of obj) we obj)))))


;;;
;;;; Output
;;;


(define jazz:output-mode
  ':reader)


(jazz:define-variable-override jazz:display
  (lambda (value output)
    (jazz:output-value value output ':human)))


(jazz:define-variable-override jazz:write
  (lambda (value output)
    (jazz:output-value value output ':reader)))


(define (jazz:print-detail value output detail)
  (case detail
    ((:human) (display value output))
    ((:reader :text :describe) (write value output))
    (else (jazz:error "Unknown print detail: {s}" detail))))


(define (jazz:->string value)
  (cond ((%%unspecified? value)
         "<unspecified>")
        ((%%values? value)
         "<values>")
        (else
         (let ((output (open-output-string)))
           (jazz:output-value value output jazz:output-mode)
           (get-output-string output)))))


(define (jazz:output-value value output detail)
  (cond ((or (%%null? value) (%%pair? value))
         (jazz:output-list value output detail))
        ((jazz:primitive? value)
         (jazz:print-detail value output detail))
        (else
         (jazz:print-jazz value output detail))))


(define (jazz:output-list lst output detail)
  (define (output-list-content lst output detail)
    (if (%%not (%%null? lst))
        (let ((scan lst)
              (done? #f))
          (%%while (and (%%not done?) (%%not (%%null? scan)))
            (jazz:output-value (%%car scan) output detail)
            (set! scan (%%cdr scan))
            (if (%%not (%%null? scan))
                (if (%%pair? scan)
                    (display " " output)
                  (begin
                    (display " . " output)
                    (jazz:output-value scan output detail)
                    (set! done? #t))))))))
  
  (display "(" output)
  (output-list-content lst output detail)
  (display ")" output))


(define (jazz:debug . rest)
  (let ((port (console-port)))
    (%%when (%%not-null? rest)
      (display (jazz:->string (%%car rest)) port)
      (for-each (lambda (expr)
                  (display " " port)
                  (display (jazz:->string expr) port))
                (%%cdr rest)))
    (newline port)
    (force-output port)))


(define (jazz:debug-string str)
  (let ((port (console-port)))
    (display str port)
    (force-output port)))


(define (jazz:debug-line str)
  (let ((port (console-port)))
    (display str port)
    (newline port)
    (force-output port)))


(define (jazz:debug-newline)
  (let ((port (console-port)))
    (newline port)
    (force-output port)))


(define jazz:terminal
  jazz:debug)


(define jazz:terminal-string
  jazz:debug-string)


(define jazz:terminal-line
  jazz:debug-line)


(define jazz:terminal-newline
  jazz:debug-newline)


(define (jazz:terminal-port)
  (console-port))


(define (jazz:bootstrap-output-value value output)
  (display (jazz:->string value) output))


(define (jazz:pretty-print expr . rest)
  (apply pretty-print expr rest))


(define (jazz:print-value call-print we object)
  (if (jazz:use-print?)
      (let ((detail (if (eq? (jazz:writeenv-style we) 'display) ':human ':reader)))
        (call-print object (jazz:writeenv-port we) detail))
    (%%default-wr we object)))


(define (jazz:print-jazz object output detail)
  (if (jazz:use-print?)
      (jazz:call-print object output detail)
    (jazz:print-serial object output)))


(jazz:define-variable-override jazz:print-hook
  (lambda (object port style)
    (let ((detail (if (%%eq? style 'display) ':human ':reader)))
      (jazz:print-jazz object port detail)))))
