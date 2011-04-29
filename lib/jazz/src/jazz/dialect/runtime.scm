;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Jazz Dialect
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


(unit protected jazz.dialect.runtime


;;;
;;;; Definition-Declaration
;;;


(jazz:define-class jazz:Definition-Declaration)


(define (jazz:new-definition-declaration name type access compatibility attributes parent expansion signature)
  (let ((new-declaration (jazz:allocate-definition-declaration jazz:Definition-Declaration name type #f access compatibility attributes #f parent #f #f expansion signature #f)))
    (jazz:setup-declaration new-declaration)
    new-declaration))


(jazz:define-method (jazz:walk-binding-validate-call (jazz:Definition-Declaration declaration) walker resume source-declaration operator arguments form-src)
  (let ((signature (%%get-definition-declaration-signature declaration)))
    (if signature
        (jazz:validate-arguments walker resume source-declaration declaration signature arguments form-src))))


(jazz:define-method (jazz:emit-inlined-binding-call (jazz:Definition-Declaration declaration) arguments call source-declaration environment backend)
  (let ((value (%%get-definition-declaration-value declaration)))
    (if (%%class-is? value jazz:Lambda)
        (if (and (%%eq? (%%get-definition-declaration-expansion declaration) 'inline)
                 (or (jazz:inline-definitions?) (jazz:untyped-inline-definition? value)))
            (let ((signature (%%get-lambda-signature value))
                  (body (%%get-lambda-body value)))
              (if (jazz:only-positional? signature)
                  (if (%%fx= (%%get-signature-mandatory signature) (%%length arguments))
                      (jazz:with-annotated-frame (jazz:annotate-inlined-signature signature arguments)
                        (lambda (frame)
                          (let ((augmented-environment (%%cons frame environment)))
                            (let ((body-code (jazz:emit-expression body source-declaration augmented-environment backend)))
                              (jazz:new-code
                                `(let ,(map (lambda (parameter argument)
                                              `(,(jazz:emit-binding-symbol parameter source-declaration environment backend)
                                                ,(jazz:emit-type-cast argument (%%get-lexical-binding-type parameter) source-declaration environment backend)))
                                            (%%get-signature-positional signature)
                                            arguments)
                                   ,@(jazz:sourcify-list (%%get-code-form body-code) (%%get-expression-source call)))
                                (jazz:call-return-type (%%get-lexical-binding-type declaration))
                                #f)))))
                    (jazz:error "Wrong number of arguments passed to {s}" (%%get-lexical-binding-name declaration)))
                (jazz:error "Only positional parameters are supported in inlining: {s}" (%%get-lexical-binding-name declaration))))
          #f)
      #f #;
      ;; not correct as the value is always #f when looking up external declarations!
      ;; we need to walk the value even at parse time for inline declarations
      (jazz:error "Constant inlining is not yet supported: {s}" (%%get-lexical-binding-name declaration)))))


;; quick solution for now as some inlined definitions like /= will change the semantics if not inlined
(define (jazz:untyped-inline-definition? value)
  (jazz:every? (lambda (parameter)
                 (%%not (%%get-lexical-binding-type parameter)))
               (%%get-signature-positional (%%get-lambda-signature value))))


(jazz:define-method (jazz:emit-declaration (jazz:Definition-Declaration declaration) environment backend)
  (let ((value (%%get-definition-declaration-value declaration)))
    (let ((expression (jazz:emit-type-cast (jazz:emit-expression value declaration environment backend) (%%get-lexical-binding-type declaration) declaration environment backend)))
      (jazz:emit 'definition backend declaration environment expression))))


(jazz:define-method (jazz:emit-binding-reference (jazz:Definition-Declaration declaration) source-declaration environment backend)
  (jazz:new-code
    (jazz:emit 'definition-reference backend declaration)
    (or (%%get-lexical-binding-type declaration)
        jazz:Any)
    #f))


(jazz:define-method (jazz:walk-binding-validate-assignment (jazz:Definition-Declaration declaration) walker resume source-declaration symbol-src)
  (nextmethod declaration walker resume source-declaration symbol-src)
  (%%when (%%neq? (%%get-declaration-toplevel declaration) (%%get-declaration-toplevel source-declaration))
    (jazz:walk-error walker resume source-declaration symbol-src "Illegal inter-module assignment to: {s}" (%%get-lexical-binding-name declaration))))


(jazz:define-method (jazz:walk-binding-assignable? (jazz:Definition-Declaration declaration))
  #t)


(jazz:define-method (jazz:emit-binding-assignment (jazz:Definition-Declaration declaration) value source-declaration environment backend)
  (let ((value (jazz:emit-expression value source-declaration environment backend)))
    (jazz:new-code
      (jazz:emit 'definition-assignment backend declaration source-declaration environment value)
      jazz:Any
      #f)))


(jazz:define-method (jazz:tree-fold (jazz:Definition-Declaration expression) down up here seed environment)
  (jazz:tree-fold (%%get-definition-declaration-value expression) down up here seed environment))


;;;
;;;; Specialize
;;;


(jazz:define-class jazz:Specialize)


(define (jazz:new-specialize)
  (jazz:allocate-specialize jazz:Specialize #f #f))


(jazz:define-method (jazz:emit-expression (jazz:Specialize expression) declaration environment backend)
  (jazz:new-code
    (jazz:emit 'specialize backend expression declaration environment)
    jazz:Any
    #f))


;;;
;;;; Generic-Declaration
;;;


(jazz:define-class jazz:Generic-Declaration)


(define (jazz:new-generic-declaration name type access compatibility attributes parent dispatch-types signature)
  (let ((new-declaration (jazz:allocate-generic-declaration jazz:Generic-Declaration name type #f access compatibility attributes #f parent #f #f dispatch-types signature #f)))
    (jazz:setup-declaration new-declaration)
    new-declaration))


(jazz:define-method (jazz:walk-binding-validate-call (jazz:Generic-Declaration declaration) walker resume source-declaration operator arguments form-src)
  (jazz:validate-arguments walker resume source-declaration declaration (%%get-generic-declaration-signature declaration) arguments form-src))


(jazz:define-method (jazz:emit-declaration (jazz:Generic-Declaration declaration) environment backend)
  (let ((signature (%%get-generic-declaration-signature declaration))
        (body (%%get-generic-declaration-body declaration)))
    (jazz:with-annotated-frame (jazz:annotate-signature signature)
      (lambda (frame)
        (let ((augmented-environment (%%cons frame environment)))
          (let ((signature-emit (jazz:emit-signature signature declaration augmented-environment backend))
                (body-emit (jazz:emit-expression body declaration augmented-environment backend)))
            (jazz:sourcify-if
              (jazz:emit 'generic backend declaration environment signature-emit body-emit)
              (%%get-declaration-source declaration))))))))


(jazz:define-method (jazz:emit-binding-reference (jazz:Generic-Declaration declaration) source-declaration environment backend)
  (jazz:new-code
    (jazz:emit 'generic-reference backend declaration)
    jazz:Any
    #f))


;;;
;;;; Specific-Declaration
;;;


(jazz:define-class jazz:Specific-Declaration)


(define (jazz:new-specific-declaration name type access compatibility attributes parent generic signature root?)
  (let ((new-declaration (jazz:allocate-specific-declaration jazz:Specific-Declaration name type #f access compatibility attributes #f parent #f #f generic signature #f root?)))
    (jazz:setup-declaration new-declaration)
    new-declaration))


(jazz:define-method (jazz:get-nextmethod-signature (jazz:Specific-Declaration declaration))
  (%%get-specific-declaration-signature declaration))


(jazz:define-method (jazz:emit-declaration (jazz:Specific-Declaration declaration) environment backend)
  (let* ((generic-declaration (%%get-specific-declaration-generic declaration))
         (generic-locator (%%get-declaration-locator generic-declaration))
         (generic-object-locator (jazz:generic-object-locator generic-locator))
         (signature (%%get-specific-declaration-signature declaration))
         (body (%%get-specific-declaration-body declaration)))
    (jazz:with-annotated-frame (jazz:annotate-signature signature)
      (lambda (frame)
        (let ((augmented-environment (%%cons frame environment)))
          (let ((signature-emit (jazz:emit-signature signature declaration augmented-environment backend))
                (body-emit (jazz:emit-expression body declaration augmented-environment backend)))
            (jazz:sourcify-if
              (jazz:emit 'specific backend declaration environment signature-emit body-emit)
              (%%get-declaration-source declaration))))))))


;;;
;;;; Category-Declaration
;;;


(jazz:define-class jazz:Category-Declaration)


(jazz:define-method (jazz:emit-binding-reference (jazz:Category-Declaration declaration) source-declaration environment backend)
  (jazz:new-code
    (jazz:emit 'category-reference backend declaration)
    jazz:Category-Declaration
    #f))


;;;
;;;; Class-Declaration
;;;


(jazz:define-class jazz:Class-Declaration)


(define (jazz:new-class-declaration name type access compatibility attributes parent implementor metaclass ascendant ascendant-relation ascendant-base interfaces)
  (let ((new-declaration (jazz:allocate-class-declaration jazz:Class-Declaration name type #f access compatibility attributes #f parent #f #f (jazz:make-access-lookups jazz:protected-access) (jazz:new-queue) #f implementor metaclass ascendant ascendant-relation ascendant-base interfaces)))
    (jazz:setup-declaration new-declaration)
    new-declaration))


(define (jazz:setup-class-lookups class-declaration)
  (define (resolve-class decl)
    (if decl
        (let ((class-declaration (jazz:resolve-binding decl)))
          (%%assert (%%is? class-declaration jazz:Class-Declaration))
          class-declaration)
      #f))
  
  (define (resolve-interface decl)
    (if decl
        (let ((interface-declaration (jazz:resolve-binding decl)))
          (%%assert (%%is? interface-declaration jazz:Interface-Declaration))
          interface-declaration)
      #f))
  
  (let ((ascendant (resolve-class (%%get-class-declaration-ascendant class-declaration)))
        (interfaces (map resolve-interface (%%get-class-declaration-interfaces class-declaration))))
    
    (let ((private (%%get-access-lookup class-declaration jazz:private-access)))
      (if ascendant
          (let ((same-module? (%%eq? (%%get-declaration-toplevel class-declaration)
                                     (%%get-declaration-toplevel ascendant))))
            (%%table-merge! private (%%get-access-lookup ascendant (if same-module? jazz:private-access jazz:public-access)) #t)))
      (for-each (lambda (interface)
                  (%%table-merge! private (%%get-access-lookup interface jazz:public-access)))
                interfaces))
    
    (let ((public (%%get-access-lookup class-declaration jazz:public-access)))
      (if ascendant
          (%%table-merge! public (%%get-access-lookup ascendant jazz:public-access)))
      (for-each (lambda (interface)
                  (%%table-merge! public (%%get-access-lookup interface jazz:public-access)))
                interfaces))
    
    ;; jazz:add-declaration-child does not set jazz:protected-access
    (let ((not-private (%%get-access-lookup class-declaration jazz:public-access)))
      (%%vector-set! (%%get-namespace-declaration-lookups class-declaration) jazz:protected-access not-private))
    
    #;
    (let ((protected (%%get-access-lookup class-declaration jazz:protected-access)))
      (if ascendant
          (%%table-merge! protected (%%get-access-lookup ascendant jazz:public-access)))
      (for-each (lambda (interface)
                  (%%table-merge! protected (%%get-access-lookup interface jazz:public-access)))
                interfaces))))


(jazz:define-method (jazz:emit-binding-reference (jazz:Class-Declaration declaration) source-declaration environment backend)
  (jazz:new-code
    (jazz:emit 'class-reference backend declaration)
    (or (%%get-category-declaration-metaclass declaration)
        jazz:Class-Declaration)
    #f))


(jazz:define-method (jazz:of-subtype? (jazz:Class-Declaration declaration) subtype)
  (if (jazz:object-declaration? declaration)
      #t
    (and (%%class-is? subtype jazz:Class-Declaration)
         (let iter ((target subtype))
           (if (%%not target)
               #f
             (let ((target-declaration (jazz:resolve-binding target)))
               (if (%%eq? target-declaration declaration)
                   #t
                 (iter (%%get-class-declaration-ascendant target-declaration)))))))))


(jazz:define-method (jazz:specifiable? (jazz:Class-Declaration declaration))
  #t)


(jazz:define-method (jazz:emit-declaration (jazz:Class-Declaration declaration) environment backend)
  (jazz:emit 'class backend declaration environment))


(define (jazz:emit-ascendant-access declaration environment backend)
  (let ((ascendant (%%get-class-declaration-ascendant declaration))
        (ascendant-relation (%%get-class-declaration-ascendant-relation declaration))
        (ascendant-base (%%get-class-declaration-ascendant-base declaration)))
    (cond ((%%not ascendant)
           #f)
          ((%%not ascendant-relation)
           (jazz:sourcified-form (jazz:emit-binding-reference ascendant declaration environment backend)))
          (else
           (let rec ((rel ascendant-relation))
             (if (%%null? rel)
                 (jazz:sourcified-form (jazz:emit-binding-reference ascendant-base declaration environment backend))
               `(%%get-object-class ,(rec (%%cdr rel)))))))))


(define (jazz:find-class-declaration declaration)
  (let iter ((decl declaration))
    (cond ((%%not decl)
           (jazz:error "Unable to find class declaration for {s}" declaration))
          ((%%class-is? decl jazz:Class-Declaration)
           decl)
          (else
           (iter (%%get-declaration-parent decl))))))


(set! jazz:object-declaration?
      (lambda (type)
        (and (%%class-is? type jazz:Class-Declaration)
             (%%not (%%get-class-declaration-ascendant type)))))


;;;
;;;; Validate
;;;


(define (jazz:validate-core-class core-class declaration)
  (define (validate-category)
    (define (validate-ascendant)
      (let* ((core-class-ascendant (%%get-class-ascendant core-class))
             (core-class-ascendant-name (if (%%not core-class-ascendant) '() (jazz:reference-name (%%get-category-identifier core-class-ascendant))))
             (declaration-ascendant (%%get-class-declaration-ascendant declaration))
             (declaration-ascendant-name (if (%%not declaration-ascendant) '() (jazz:reference-name (%%get-declaration-locator declaration-ascendant)))))
        (%%when (%%not (%%eq? core-class-ascendant-name declaration-ascendant-name))
          (jazz:error "Inconsistant core-class/class ascendant for {s}: {s} / {s}" (%%get-lexical-binding-name declaration) core-class-ascendant-name declaration-ascendant-name))))
    
    (define (validate-interfaces)
      (let ((declaration-interfaces (%%get-class-declaration-interfaces declaration)))
        (%%when (%%not (%%null? declaration-interfaces))
          (jazz:error "Interfaces are not supported in open classes: {s}" (%%get-lexical-binding-name declaration)))))
    
    (validate-ascendant)
    (validate-interfaces))
  
  (define (validate-slots)
    (define (collect-slots lst)
      (let ((queue (jazz:new-queue)))
        (define (process obj)
          (cond ((%%is? obj jazz:Slot-Declaration)
                 (jazz:enqueue queue obj))
                ((%%is? obj jazz:Begin)
                 (for-each process (%%get-begin-expressions obj)))))
        
        (for-each process lst)
        (jazz:queue-list queue)))
    
    (let ((core-class-slot-names (map (lambda (name/slot) (if (%%symbol? name/slot) name/slot (%%get-field-name name/slot))) (%%get-class-instance-slots core-class)))
          (declaration-slot-names (map (lambda (decl) (%%get-lexical-binding-name decl)) (collect-slots (%%get-namespace-declaration-body declaration)))))
      (%%when (%%not (%%equal? core-class-slot-names declaration-slot-names))
        (jazz:error "Inconsistant core-class/class slots for {s}: {s} / {s}" (%%get-lexical-binding-name declaration) core-class-slot-names declaration-slot-names))))
  
  (validate-category)
  (validate-slots))


;;;
;;;; Interface-Declaration
;;;


(jazz:define-class jazz:Interface-Declaration)


(define (jazz:new-interface-declaration name type access compatibility attributes parent implementor metaclass ascendants)
  (let ((new-declaration (jazz:allocate-interface-declaration jazz:Interface-Declaration name type #f access compatibility attributes #f parent #f #f (jazz:make-access-lookups jazz:protected-access) (jazz:new-queue) #f implementor metaclass ascendants)))
    (jazz:setup-declaration new-declaration)
    new-declaration))


(define (jazz:setup-interface-lookups interface-declaration)
  (define (resolve-interface decl)
    (if decl
        (let ((interface-declaration (jazz:resolve-binding decl)))
          (%%assert (%%is? interface-declaration jazz:Interface-Declaration))
          interface-declaration)
      #f))
  
  (let ((ascendants (map resolve-interface (%%get-interface-declaration-ascendants interface-declaration))))
    
    (let ((private (%%get-access-lookup interface-declaration jazz:private-access)))
      (for-each (lambda (interface)
                  (%%table-merge! private (%%get-access-lookup interface jazz:public-access) #t))
                ascendants))
    
    ;; a quick test
    (let ((private (%%get-access-lookup interface-declaration jazz:private-access)))
      (%%vector-set! (%%get-namespace-declaration-lookups interface-declaration) jazz:public-access private)
      (%%vector-set! (%%get-namespace-declaration-lookups interface-declaration) jazz:protected-access private))
    
    #;
    (let ((public (%%get-access-lookup interface-declaration jazz:public-access)))
      (for-each (lambda (interface)
                  (%%table-merge! public (%%get-access-lookup interface jazz:public-access)))
                ascendants))
    
    #;
    (let ((protected (%%get-access-lookup interface-declaration jazz:protected-access)))
      (for-each (lambda (interface)
                  (%%table-merge! protected (%%get-access-lookup interface jazz:public-access)))
                ascendants))))


(jazz:define-method (jazz:of-subtype? (jazz:Interface-Declaration declaration) subtype)
  ;; quicky to fill later on
  #f)


(jazz:define-method (jazz:specifiable? (jazz:Interface-Declaration declaration))
  #t)


(jazz:define-method (jazz:emit-declaration (jazz:Interface-Declaration declaration) environment backend)
  (jazz:emit 'interface backend declaration environment))


;;;
;;;; Field-Declaration
;;;


(jazz:define-class jazz:Field-Declaration)


;;;
;;;; Slot-Declaration
;;;


(jazz:define-class jazz:Slot-Declaration)


(define (jazz:new-slot-declaration name type access compatibility attributes parent initialize getter-name setter-name)
  (let ((new-declaration (jazz:allocate-slot-declaration jazz:Slot-Declaration name type #f access compatibility attributes #f parent #f #f initialize getter-name setter-name)))
    (jazz:setup-declaration new-declaration)
    new-declaration))


(jazz:define-method (jazz:walk-binding-validate-call (jazz:Slot-Declaration declaration) walker resume source-declaration operator arguments form-src)
  #f)


(jazz:define-method (jazz:emit-declaration (jazz:Slot-Declaration declaration) environment backend)
  (let* ((class-declaration (%%get-declaration-parent declaration))
         (allocate? (%%neq? (%%get-lexical-binding-type declaration) jazz:Void))
         (core? (jazz:core-class? (%%get-lexical-binding-name class-declaration)))
         (initialize (%%get-slot-declaration-initialize declaration))
         (initialize? (and allocate? (%%not core?) initialize))
         (initialize-emit (and initialize? (jazz:emit-expression initialize declaration environment backend))))
    (jazz:emit 'slot backend declaration environment initialize-emit)))


(jazz:define-method (jazz:emit-binding-reference (jazz:Slot-Declaration declaration) source-declaration environment backend)
  (let ((self (jazz:*self*)))
    (if self
        (jazz:new-code
          (jazz:emit 'slot-reference backend declaration self)
          (jazz:find-annotated-type declaration environment)
          #f)
      (jazz:error "Illegal reference to a slot: {s}" (%%get-declaration-locator declaration)))))


(jazz:define-method (jazz:walk-binding-assignable? (jazz:Slot-Declaration declaration))
  #t)


(jazz:define-method (jazz:emit-binding-assignment (jazz:Slot-Declaration declaration) value source-declaration environment backend)
  (let ((self (jazz:*self*)))
    (if self
        (let ((value (jazz:emit-expression value source-declaration environment backend)))
          (jazz:new-code
            (jazz:emit 'slot-assignment backend declaration source-declaration environment self value)
            jazz:Any
            #f))
      (jazz:error "Illegal assignment to a slot: {s}" (%%get-declaration-locator declaration)))))


;;;
;;;; Property
;;;


(jazz:define-class jazz:Property-Declaration)


(define (jazz:new-property-declaration name type access compatibility attributes parent initialize getter-name setter-name)
  (let ((new-declaration (jazz:allocate-property-declaration jazz:Property-Declaration name type #f access compatibility attributes #f parent #f #f initialize getter-name setter-name #f #f)))
    (jazz:setup-declaration new-declaration)
    new-declaration))


(jazz:define-method (jazz:emit-declaration (jazz:Property-Declaration declaration) environment backend)
  (let* ((class-declaration (%%get-declaration-parent declaration))
         (allocate? (%%neq? (%%get-lexical-binding-type declaration) jazz:Void))
         (core? (jazz:core-class? (%%get-lexical-binding-name class-declaration)))
         (initialize (%%get-slot-declaration-initialize declaration))
         (initialize? (and allocate? (%%not core?) initialize))
         (initialize-emit (and initialize? (jazz:emit-expression initialize declaration environment backend))))
    (jazz:emit 'property backend declaration environment initialize-emit)))


(define (jazz:expand-property walker resume declaration environment form-src)
  (jazz:expand-slot-form walker resume declaration form-src '%property))


;;;
;;;; Method-Declaration
;;;


(jazz:define-class jazz:Method-Declaration)


(define (jazz:new-method-declaration name type access compatibility attributes parent root propagation abstraction expansion remote synchronized signature)
  (let ((new-declaration (jazz:allocate-method-declaration jazz:Method-Declaration name type #f access compatibility attributes #f parent #f #f root propagation abstraction expansion remote synchronized signature #f)))
    (jazz:setup-declaration new-declaration)
    new-declaration))


(define (jazz:method-dispatch-info declaration)
  (let ((root (%%get-method-declaration-root declaration))
        (propagation (%%get-method-declaration-propagation declaration)))
    (if (and (%%not root) (%%eq? propagation 'final))
        (values 'final declaration)
      (let ((root-method-declaration (%%get-method-declaration-root declaration)))
        (let ((method-declaration (or root-method-declaration declaration)))
          (let ((category-declaration (%%get-declaration-parent method-declaration)))
            (cond ((%%class-is? category-declaration jazz:Class-Declaration)
                   (values 'class method-declaration))
                  ((%%class-is? category-declaration jazz:Interface-Declaration)
                   (values 'interface method-declaration))
                  (else
                   (error "Invalid category declaration: {s}" category-declaration)))))))))


(define (jazz:native-category? category-declaration)
  (%%neq? (%%get-category-declaration-implementor category-declaration) 'primitive))


(define (jazz:emit-method-dispatch object declaration source-declaration environment backend)
  (let ((name (%%get-lexical-binding-name declaration)))
    (receive (dispatch-type method-declaration) (jazz:method-dispatch-info declaration)
      (let ((category-declaration (%%get-declaration-parent method-declaration)))
        (let ((object-cast (jazz:emit-type-cast object category-declaration source-declaration environment backend)))
          (jazz:new-code
            (case dispatch-type
              ((final)
               (let ((implementation-locator (%%get-declaration-locator method-declaration)))
                 `(%%final-dispatch (jazz:class-of ,object-cast) ,implementation-locator)))
              ((class)
               (let ((class-level-locator (%%compose-helper (%%get-declaration-locator category-declaration) 'level))
                     (method-rank-locator (%%compose-helper (%%get-declaration-locator method-declaration) 'rank)))
                 (if (jazz:native-category? category-declaration)
                     `(%%class-dispatch (%%get-object-class ,object-cast) ,class-level-locator ,method-rank-locator)
                   `(%%class-dispatch (jazz:class-of ,object-cast) ,class-level-locator ,method-rank-locator))))
              ((interface)
               (let ((interface-rank-locator (%%compose-helper (%%get-declaration-locator category-declaration) 'rank))
                     (method-rank-locator (%%compose-helper (%%get-declaration-locator method-declaration) 'rank)))
                 (if (jazz:native-category? category-declaration)
                     `(%%interface-dispatch (%%get-object-class ,object-cast) ,interface-rank-locator ,method-rank-locator)
                   `(%%interface-dispatch (jazz:class-of ,object-cast) ,interface-rank-locator ,method-rank-locator)))))
            (jazz:call-return-type (%%get-lexical-binding-type method-declaration))
            #f))))))


(jazz:define-method (jazz:compose-declaration-locator (jazz:Method-Declaration declaration))
  (if (%%eq? (%%get-method-declaration-abstraction declaration) 'core)
      (let ((class-name (%%compose-reference 'jazz (%%get-lexical-binding-name (%%get-declaration-parent declaration))))
            (name (%%compose-reference 'jazz (%%get-lexical-binding-name declaration))))
        (jazz:method-implementation-name class-name name))
    (nextmethod declaration)))


(jazz:define-method (jazz:emit-binding-reference (jazz:Method-Declaration declaration) source-declaration environment backend)
  (let ((self (jazz:*self*)))
    (if self
        (let ((dispatch-code (jazz:emit-method-dispatch self declaration source-declaration environment backend)))
          (jazz:new-code
            (jazz:emit 'method-reference backend declaration source-declaration environment self dispatch-code)
            (%%get-code-type dispatch-code)
            #f))
      (jazz:error "Methods can only be called directly from inside a method: {a} in {a}" (%%get-lexical-binding-name declaration) (%%get-declaration-locator source-declaration)))))


(jazz:define-method (jazz:walk-binding-validate-call (jazz:Method-Declaration declaration) walker resume source-declaration operator arguments form-src)
  (let ((signature (%%get-method-declaration-signature declaration)))
    (if signature
        (jazz:validate-arguments walker resume source-declaration declaration signature arguments form-src))))


(jazz:define-method (jazz:emit-inlined-binding-call (jazz:Method-Declaration declaration) arguments call source-declaration environment backend)
  ;; mostly copy/pasted and adapted from definition declaration. need to unify the code
  (if (%%eq? (%%get-method-declaration-expansion declaration) 'inline)
      (receive (dispatch-type method-declaration) (jazz:method-dispatch-info declaration)
        (case dispatch-type
          ((final)
           (let ((signature (%%get-method-declaration-signature declaration))
                 (body (%%get-method-declaration-body declaration)))
             (if (jazz:only-positional? signature)
                 (if (%%fx= (%%get-signature-mandatory signature) (%%length arguments))
                     (jazz:with-annotated-frame (jazz:annotate-signature signature)
                       (lambda (frame)
                         (let ((augmented-environment (%%cons frame environment)))
                           (let ((body-code (jazz:emit-expression body source-declaration augmented-environment backend)))
                             (jazz:new-code
                               `(let ,(map (lambda (parameter argument)
                                             `(,(jazz:emit-binding-symbol parameter source-declaration environment backend)
                                               ,(jazz:emit-type-cast argument (%%get-lexical-binding-type parameter) source-declaration environment backend)))
                                           (%%get-signature-positional signature)
                                           arguments)
                                  ,(jazz:sourcify-if (jazz:desourcify-all (%%get-code-form body-code)) (%%get-expression-source call)))
                               (jazz:call-return-type (%%get-lexical-binding-type declaration))
                               #f)))))
                   (jazz:error "Wrong number of arguments passed to {s}" (%%get-lexical-binding-name declaration)))
               (jazz:error "Only positional parameters are supported in inlining: {s}" (%%get-lexical-binding-name declaration)))))
          (else
           #f)))
    #f))


(jazz:define-method (jazz:emit-binding-call (jazz:Method-Declaration declaration) binding-src arguments source-declaration environment backend)
  (let ((self (jazz:*self*)))
    (if self
        (let ((type (%%get-lexical-binding-type declaration))
              (arguments (jazz:codes-forms arguments))
              (dispatch-code (jazz:emit-method-dispatch self declaration source-declaration environment backend)))
          (jazz:new-code
            (jazz:emit 'method-binding-call backend declaration binding-src dispatch-code self arguments)
            (%%get-code-type dispatch-code)
            #f))
      (jazz:error "Methods can only be called directly from inside a method: {a} in {a}" (%%get-lexical-binding-name declaration) (%%get-declaration-locator source-declaration)))))


(jazz:define-method (jazz:get-nextmethod-signature (jazz:Method-Declaration declaration))
  (define (lookup category-declaration method-name)
    (if (%%is? category-declaration jazz:Autoload-Declaration)
        (let ((category-declaration (%%get-autoload-declaration-declaration category-declaration)))
          (jazz:lookup-declaration category-declaration method-name jazz:private-access category-declaration))
      (jazz:lookup-declaration category-declaration method-name jazz:private-access category-declaration)))
  
  (define (get-next-method-declaration)
    (let ((method-name (%%get-lexical-binding-name declaration))
          (category-declaration (%%get-declaration-parent declaration)))
      (or (let ((ascendant (%%get-class-declaration-ascendant category-declaration)))
            (lookup ascendant method-name))
          (let iter ((scan (%%get-class-declaration-interfaces category-declaration)))
               (if (null? scan)
                   #f
                 (or (lookup (car scan) method-name)
                     (iter (cdr scan))))))))
  
  (let ((next-method-declaration (get-next-method-declaration)))
    (if next-method-declaration
        (%%get-method-declaration-signature next-method-declaration))))


(jazz:define-method (jazz:emit-declaration (jazz:Method-Declaration declaration) environment backend)
  (let ((signature (%%get-method-declaration-signature declaration)))
    (jazz:with-annotated-frame (jazz:annotate-signature signature)
      (lambda (frame)
        (let* ((augmented-environment (%%cons frame environment))
               (signature-emit (jazz:emit-signature signature declaration augmented-environment backend))
               (signature-casts (jazz:emit-signature-casts signature declaration augmented-environment backend))
               (body (%%get-method-declaration-body declaration))
               (body-type (let ((type (%%get-lexical-binding-type declaration)))
                            (if (%%is? type jazz:Function-Type) (%%get-function-type-result type) jazz:Any)))
               (body-emit (and body (let ((body-code (jazz:emit-expression body declaration augmented-environment backend)))
                                      (jazz:emit-type-cast body-code body-type declaration augmented-environment backend)))))
          (jazz:emit 'method backend declaration environment signature-emit signature-casts body-emit))))))


(jazz:define-method (jazz:tree-fold (jazz:Method-Declaration expression) down up here seed environment)
  (let ((body (%%get-method-declaration-body expression)))
    (if (%%not body)
        (here expression seed environment)
      (jazz:tree-fold body down up here seed environment))))


;;;
;;;; Method Node Reference
;;;


(jazz:define-class jazz:Method-Node-Reference)


(define (jazz:new-method-node-reference binding)
  (jazz:allocate-method-node-reference jazz:Method-Node-Reference #f #f binding))


(jazz:define-method (jazz:emit-expression (jazz:Method-Node-Reference expression) declaration environment backend)
  (let ((method-declaration (%%get-reference-binding expression)))
    (jazz:new-code
      (jazz:emit 'method-node-reference backend expression declaration environment)
      (or (%%get-lexical-binding-type method-declaration)
          jazz:Any)
      #f)))


(jazz:define-method (jazz:emit-call (jazz:Method-Node-Reference expression) arguments declaration environment backend)
  (let ((operator (jazz:emit-expression expression declaration environment backend)))
    (jazz:new-code
      (jazz:emit 'method-node-call backend expression declaration operator arguments)
      jazz:Any
      #f)))


;;;
;;;; Dialect
;;;


(jazz:define-class jazz:Jazz-Dialect)


(define (jazz:new-jazz-dialect name)
  (jazz:allocate-jazz-dialect jazz:Jazz-Dialect name (%%make-table test: eq?) (%%make-table test: eq?)))


(jazz:define-method (jazz:dialect-walker (jazz:Jazz-Dialect dialect))
  (jazz:new-jazz-walker))


;;;
;;;; Walker
;;;


(jazz:define-class jazz:Jazz-Walker)


(define (jazz:new-jazz-walker)
  (jazz:allocate-jazz-walker jazz:Jazz-Walker #f #f '() '() '() (jazz:new-queue) (%%make-table test: eq?) '()))


(jazz:define-method (jazz:runtime-export (jazz:Jazz-Walker walker) declaration)
  (or (nextmethod walker declaration)
      (if (or (%%is? declaration jazz:Definition-Declaration)
              (%%is? declaration jazz:Generic-Declaration)
              (%%is? declaration jazz:Category-Declaration))
          (%%get-declaration-locator declaration)
        #f)))


;;;
;;;; Environment
;;;


(jazz:define-method (jazz:walker-declarations (jazz:Jazz-Walker walker))
  (cons (%%get-dialect-declarations (jazz:get-dialect 'jazz))
        (nextmethod walker)))


(jazz:define-method (jazz:walker-bindings (jazz:Jazz-Walker walker))
  (cons (%%get-dialect-bindings (jazz:get-dialect 'jazz))
        (nextmethod walker)))


;;;
;;;; Parse
;;;


(define (jazz:parse-keywords keywords rest)
  (let ((table (%%make-table test: eq?))
        (done? #f))
    (%%while (and (%%not done?) (%%not (%%null? rest)))
      (let ((symbol (jazz:source-code (%%car rest))))
        (if (%%not (%%memq symbol keywords))
            (set! done? #t)
          (begin
            (%%table-set! table symbol (%%desourcify (%%cadr rest)))
            (set! rest (%%cddr rest))))))
    (%%apply values (%%append (map (lambda (keyword)
                                     (%%table-ref table keyword (jazz:unspecified)))
                                   keywords)
                              (%%list rest)))))


;;;
;;;; Specialize
;;;


(jazz:define-variable-override jazz:emit-specialized-locator
  (lambda (locator arguments environment backend)
    (case locator
      ((jazz.language.runtime.kernel:class-of)
       (%%assert (and (%%pair? arguments) (%%null? (%%cdr arguments)))
         (jazz:emit-specialized-class-of (%%car arguments) environment backend)))
      (else
       #f))))


(define (jazz:emit-specialized-class-of object environment backend)
  (jazz:new-code
    (jazz:emit 'specialized-class-of-call backend object)
    (let ((type (%%get-code-type object)))
      (if (%%class-is? type jazz:Class-Declaration)
          (%%get-category-declaration-metaclass type)
        jazz:Class-Declaration))
    #f))


;;;
;;;; New
;;;


(jazz:define-variable-override jazz:emit-new-call
  (lambda (operator locator arguments arguments-codes declaration environment backend)
    (jazz:emit 'new-call backend operator locator arguments arguments-codes declaration environment)))


;;;
;;;; Symbol
;;;


(jazz:define-method (jazz:walk-symbol (jazz:Jazz-Walker walker) resume declaration environment symbol-src)
  (let ((symbol (jazz:unwrap-syntactic-closure symbol-src)))
    (jazz:split-tilde symbol
      (lambda (tilde? name self/class-name)
        (if tilde?
            (cond ((and name self/class-name)
                   (if (%%eq? self/class-name 'self)
                       (let ((slot-declaration (jazz:lookup-declaration (jazz:find-class-declaration declaration) name jazz:private-access declaration)))
                         (%%assert (%%class-is? slot-declaration jazz:Slot-Declaration)
                           (jazz:new-binding-reference symbol-src slot-declaration)))
                     (let ((category-declaration (jazz:resolve-binding (jazz:lookup-reference walker resume declaration environment self/class-name))))
                       (%%assert (%%class-is? category-declaration jazz:Category-Declaration)
                         (let ((method-declaration (jazz:lookup-declaration category-declaration name jazz:private-access declaration)))
                           (%%assert (%%class-is? method-declaration jazz:Method-Declaration)
                             (jazz:new-method-node-reference method-declaration)))))))
                  ((and name (not self/class-name))
                   (let ((method-symbol-src (jazz:sourcify-if (jazz:dispatch->symbol (jazz:unwrap-syntactic-closure symbol-src)) symbol-src)))
                     (jazz:walk walker resume declaration environment `(lambda (object . rest) (apply (~ ,method-symbol-src object) rest)))))
                  ((and (not name) self/class-name)
                   (jazz:walk-error walker resume declaration symbol-src "Ill-formed expression: {s}" symbol))
                  (else
                   (nextmethod walker resume declaration environment symbol-src)))
          (nextmethod walker resume declaration environment symbol-src))))))


(define (jazz:split-tilde symbol proc)
  (let ((str (%%symbol->string symbol)))
    (let ((n (jazz:string-find-reversed str #\~)))
      (if (%%not n)
          (proc #f #f #f)
        (let ((len (%%string-length str)))
          (proc #t
                (if (%%fx> n 0) (%%string->symbol (%%substring str 0 n)) #f)
                (if (%%fx< (%%fx+ n 1) len) (%%string->symbol (%%substring str (%%fx+ n 1) len)) #f)))))))


(jazz:define-method (jazz:lookup-analyse (jazz:Jazz-Walker walker) declaration symbol-src referenced-declaration)
  (if (and (%%is? declaration jazz:Method-Declaration)
           (%%eq? (%%get-method-declaration-propagation declaration) 'final)
           (or (%%eq? (jazz:source-code symbol-src) 'self)
               (%%is? referenced-declaration jazz:Slot-Declaration)
               (%%is? referenced-declaration jazz:Method-Declaration)))
      (let ((data (jazz:get-analysis-data (%%get-declaration-locator declaration))))
        (%%set-analysis-data-declaration-references data (%%cons referenced-declaration (%%get-analysis-data-declaration-references data))))))


;;;
;;;; Assignment
;;;


(jazz:define-method (jazz:walk-symbol-assignment (jazz:Jazz-Walker walker) resume declaration environment symbol-src value)
  (jazz:split-tilde (jazz:source-code symbol-src)
    (lambda (tilde? name self/class-name)
      (if tilde?
          (if (and name (%%eq? self/class-name 'self))
              (let ((slot-declaration (jazz:lookup-declaration (jazz:find-class-declaration declaration) name jazz:private-access declaration)))
                (%%assert (%%class-is? slot-declaration jazz:Slot-Declaration)
                  (jazz:new-assignment slot-declaration (jazz:walk walker resume declaration environment value))))
            (jazz:walk-error walker resume declaration symbol-src "Ill-formed set! variable: {s}" (%%desourcify symbol-src)))
        (nextmethod walker resume declaration environment symbol-src value)))))


;;;
;;;; Form
;;;


(jazz:define-method (jazz:walk-form (jazz:Jazz-Walker walker) resume declaration environment form-src)
  (let ((procedure-expr (jazz:source-code (%%car (jazz:source-code form-src)))))
    (if (jazz:dispatch? procedure-expr)
        (jazz:walk-dispatch walker resume declaration environment form-src)
      (nextmethod walker resume declaration environment form-src))))


;;;
;;;; With-Self
;;;


(jazz:define-class jazz:With-Self)


(define (jazz:new-with-self body)
  (jazz:allocate-with-self jazz:With-Self #f #f body))


(jazz:define-method (jazz:emit-expression (jazz:With-Self expression) declaration environment backend)
  (let ((body (%%get-with-self-body expression)))
    (let ((body-emit (parameterize ((jazz:*self* (jazz:new-code 'self declaration #f)))
                       (jazz:emit-expression body declaration environment backend))))
      (jazz:new-code
        (jazz:emit 'with-self backend expression declaration environment body-emit)
        jazz:Any
        #f))))


(jazz:define-method (jazz:tree-fold (jazz:With-Self expression) down up here seed environment)
  (let ((aug-env environment #; (cons 'self environment)
          )
        (seed1 (down expression seed environment)))
    (up expression seed (jazz:tree-fold (%%get-with-self-body expression) down up here seed1 aug-env) environment)))


;;;
;;;; With-Dynamic-Self
;;;


(jazz:define-class jazz:With-Dynamic-Self)


(define (jazz:new-with-dynamic-self code body)
  (jazz:allocate-with-dynamic-self jazz:With-Dynamic-Self #f #f code body))


(jazz:define-method (jazz:emit-expression (jazz:With-Dynamic-Self expression) declaration environment backend)
  (let ((code (%%get-with-dynamic-self-code expression))
        (body (%%get-with-dynamic-self-body expression)))
    (let ((body-emit (parameterize ((jazz:*self* (jazz:new-code code declaration #f)))
                       (jazz:emit-statements-code body declaration environment backend))))
      (jazz:new-code
        (jazz:emit 'with-dynamic-self backend expression declaration environment body-emit)
        jazz:Any
        #f))))


(define (jazz:parse-with-dynamic-self form)
  (let ((code (%%car form))
        (body (%%cdr form)))
    (values code body)))


(define (jazz:walk-with-dynamic-self-declaration walker resume declaration environment form-src)
  (receive (code body) (jazz:parse-with-dynamic-self (%%cdr (%%desourcify form-src)))
    (jazz:walk-declarations walker resume declaration environment body)))


(define (jazz:walk-with-dynamic-self walker resume declaration environment form-src)
  (receive (code body) (jazz:parse-with-dynamic-self (%%cdr (%%desourcify form-src)))
    (let ((new-environment (%%cons (jazz:new-dynamic-self-binding #f code) environment)))
      (jazz:new-with-dynamic-self
        code
        (jazz:walk-list walker resume declaration new-environment body)))))


;;;
;;;; Cast
;;;


(jazz:define-class jazz:Cast)


(define (jazz:new-cast type expression)
  (jazz:allocate-cast jazz:Cast type #f expression))


(jazz:define-method (jazz:emit-expression (jazz:Cast expression) declaration environment backend)
  (let ((type (%%get-expression-type expression))
        (expression (%%get-cast-expression expression)))
    (let ((expression-emit (jazz:emit-expression expression declaration environment backend)))
      (jazz:new-code
        (jazz:emit 'cast backend expression declaration environment type expression-emit)
        type
        #f))))


(define (jazz:walk-cast walker resume declaration environment form-src)
  (let ((form (%%desourcify form-src)))
    (let ((specifier (%%cadr form))
          (expression (%%car (%%cddr form))))
      (jazz:new-cast (jazz:walk-specifier walker resume declaration environment specifier)
                     (jazz:walk walker resume declaration environment expression)))))


;;;
;;;; Allocate
;;;


(jazz:define-class jazz:Allocate)


(define (jazz:new-allocate class values)
  (jazz:allocate-allocate jazz:Allocate #f #f class values))


(jazz:define-method (jazz:emit-expression (jazz:Allocate expression) declaration environment backend)
  (let ((class (%%get-allocate-class expression))
        (values (%%get-allocate-values expression)))
    (let ((class-emit (jazz:emit-expression class declaration environment backend))
          (values-emit (jazz:emit-expressions values declaration environment backend)))
      (jazz:new-code
        (jazz:emit 'allocate backend expression declaration environment class-emit values-emit)
        jazz:Any
        #f))))


(define (jazz:walk-allocate walker resume declaration environment form-src)
  (let ((form (%%desourcify form-src)))
    (let ((class (%%cadr form))
          (values (%%cddr form)))
      (jazz:new-allocate (jazz:walk walker resume declaration environment class)
                         (jazz:walk-list walker resume declaration environment values)))))


;;;
;;;; Dispatch
;;;


(define (jazz:cache-dispatch name setter)
  (lambda (object)
    (let ((class (jazz:class-of object)))
      (let ((category (jazz:locate-method-owner class name)))
        (%%assertion category (jazz:error "Unable to find method {s} in: {s}" name object)
          (let ((field (%%get-category-field category name)))
            (%%assertion (%%class-is? field jazz:Method) (jazz:error "Field {s} is not a method of {s}" name object)
              (let ((proc
                      (case (%%get-method-dispatch-type field)
                        ((final)
                         (jazz:final-dispatch field category))
                        ((class)
                         (jazz:class-dispatch field category))
                        ((interface)
                         (jazz:interface-dispatch field category)))))
                (setter proc)
                (proc object)))))))))


(define (jazz:final-dispatch field type)
  (lambda (object)
    (%%debug-assertion (%%category-is? object type) (jazz:dispatch-error field object type)
      (%%final-dispatch (jazz:class-of object) (%%get-method-implementation field)))))


(define (jazz:class-dispatch field type)
  (let ((class-level (%%get-method-category-rank field))
        (implementation-rank (%%get-method-implementation-rank field)))
    (lambda (object)
      (%%debug-assertion (%%category-is? object type) (jazz:dispatch-error field object type)
        (%%class-dispatch (jazz:class-of object) class-level implementation-rank)))))


(define (jazz:interface-dispatch field type)
  (let ((interface-rank (%%get-method-category-rank field))
        (implementation-rank (%%get-method-implementation-rank field)))
    (lambda (object)
      (%%debug-assertion (%%category-is? object type) (jazz:dispatch-error field object type)
        (%%interface-dispatch (jazz:class-of object) interface-rank implementation-rank)))))


(define (jazz:dispatch class name)
  (or (jazz:find-dispatch class name)
      (jazz:error "Unable to find method {s} in: {s}" name class)))


(define (jazz:find-dispatch class name)
  (let ((category (jazz:locate-method-owner class name)))
    (if (%%not category)
        #f
      (let ((field (%%get-category-field category name)))
        (if (not (%%class-is? field jazz:Method))
            (jazz:error "Field {s} is not a method of {s}" name class))
        (case (%%get-method-dispatch-type field)
          ((final)
           (%%final-dispatch class (%%get-method-implementation field)))
          ((class)
           (%%class-dispatch class (%%get-method-category-rank field) (%%get-method-implementation-rank field)))
          ((interface)
           (%%interface-dispatch class (%%get-method-category-rank field) (%%get-method-implementation-rank field))))))))


(jazz:define-class jazz:Dispatch)


(define (jazz:new-dispatch source name arguments)
  (jazz:allocate-dispatch jazz:Dispatch #f source name arguments))


(jazz:define-method (jazz:emit-expression (jazz:Dispatch expression) declaration environment backend)
  (jazz:emit-dispatch expression declaration environment backend))


(define (jazz:emit-dispatch expression declaration environment backend)
  (let ((arguments (%%get-dispatch-arguments expression)))
    (let ((object-argument (%%car arguments))
          (rest-arguments (%%cdr arguments)))
      (let ((object-code (jazz:emit-expression object-argument declaration environment backend))
            (rest-codes (jazz:emit-expressions rest-arguments declaration environment backend)))
        (jazz:emit 'dispatch backend expression declaration environment object-code rest-codes)))))


(define (jazz:emit-inlined-final-dispatch expression declaration object arguments source-declaration environment backend)
  ;; mostly copy/pasted and adapted from method declaration. need to unify the code
  (if (%%eq? (%%get-method-declaration-expansion declaration) 'inline)
      (receive (dispatch-type method-declaration) (jazz:method-dispatch-info declaration)
        (case dispatch-type
          ((final)
           (let ((signature (%%get-method-declaration-signature declaration))
                 (body (%%get-method-declaration-body declaration)))
             (if (jazz:only-positional? signature)
                 (if (%%fx= (%%get-signature-mandatory signature) (%%length arguments))
                     (jazz:with-annotated-frame (jazz:annotate-signature signature)
                       (lambda (frame)
                         (let ((augmented-environment (%%cons frame environment)))
                           (let ((body-code (jazz:emit-expression body source-declaration augmented-environment backend)))
                             (jazz:new-code
                               `(let ,(%%cons `(self ,(jazz:sourcified-form object))
                                              (map (lambda (parameter argument)
                                                     `(,(jazz:emit-binding-symbol parameter source-declaration environment backend)
                                                       ,(jazz:emit-type-cast argument (%%get-lexical-binding-type parameter) source-declaration environment backend)))
                                                   (%%get-signature-positional signature)
                                                   arguments))
                                  ,(jazz:sourcify-if (jazz:desourcify-all (%%get-code-form body-code)) (%%get-expression-source expression)))
                               (jazz:call-return-type (%%get-lexical-binding-type declaration))
                               #f)))))
                   (jazz:error "Wrong number of arguments passed to {s}" (%%get-lexical-binding-name declaration)))
               (jazz:error "Only positional parameters are supported in inlining: {s}" (%%get-lexical-binding-name declaration)))))
          (else
           #f)))
    #f))


(define (jazz:walk-dispatch walker resume declaration environment form-src)
  (let ((name (jazz:dispatch->symbol (jazz:unwrap-syntactic-closure (%%car (jazz:source-code form-src)))))
        (arguments (%%cdr (jazz:source-code form-src))))
    (%%assertion (%%not (%%null? arguments)) (jazz:walk-error walker resume declaration form-src "Dispatch call must contain at least one argument: {s}" (%%desourcify form-src))
      (jazz:new-dispatch form-src name
        (jazz:walk-list walker resume declaration environment arguments)))))


;;;
;;;; Definition
;;;


(define jazz:definition-modifiers
  '(((private protected package public) . private)
    ((deprecated undocumented uptodate) . uptodate)
    ((inline onsite) . onsite)))


(define (jazz:parse-definition walker resume declaration rest)
  (receive (access compatibility expansion rest) (jazz:parse-modifiers walker resume declaration jazz:definition-modifiers rest)
    (if (%%symbol? (jazz:unwrap-syntactic-closure (%%car rest)))
        (let ((name (jazz:unwrap-syntactic-closure (%%car rest))))
          (jazz:parse-specifier (%%cdr rest)
            (lambda (specifier rest)
              (values name specifier access compatibility expansion (%%car rest) #f))))
      (let* ((name (jazz:source-code (%%car (jazz:unwrap-syntactic-closure (%%car rest)))))
             (parameters (%%cdr (%%desourcify (%%car rest)))))
        (jazz:parse-specifier (%%cdr rest)
          (lambda (specifier body)
            (let ((effective-body (if (%%null? body) (%%list (%%list 'unspecified)) body))
                  (specifier-list (if specifier (%%list specifier) '())))
              (let ((value
                     `(lambda ,parameters ,@specifier-list
                        ,@effective-body)))
                (values name specifier access compatibility expansion value parameters)))))))))


(define (jazz:walk-definition-declaration walker resume declaration environment form-src)
  (receive (name specifier access compatibility expansion value parameters) (jazz:parse-definition walker resume declaration (%%cdr (jazz:source-code form-src)))
    (%%assertion (%%class-is? declaration jazz:Namespace-Declaration) (jazz:walk-error walker resume declaration form-src "Definitions can only be defined inside namespaces: {s}" name)
      (let ((type (jazz:specifier->type walker resume declaration environment specifier)))
        (let ((signature (and parameters (jazz:walk-parameters walker resume declaration environment parameters #t #f))))
          (let ((effective-type (if parameters (jazz:build-function-type signature type) type)))
            (let ((new-declaration (or (jazz:find-declaration-child declaration name)
                                       (jazz:new-definition-declaration name effective-type access compatibility '() declaration expansion signature))))
              (%%set-declaration-source new-declaration form-src)
              (let ((effective-declaration (jazz:add-declaration-child walker resume declaration new-declaration)))
                (%%when (%%eq? expansion 'inline)
                  (let ((new-environment (%%cons effective-declaration environment)))
                    (%%set-definition-declaration-signature effective-declaration signature)
                    (%%set-definition-declaration-value effective-declaration
                                                        (jazz:walk walker resume effective-declaration new-environment value))))
                effective-declaration))))))))


(define (jazz:walk-definition walker resume declaration environment form-src)
  (receive (name specifier access compatibility expansion value parameters) (jazz:parse-definition walker resume declaration (%%cdr (jazz:source-code form-src)))
    (%%assertion (%%class-is? declaration jazz:Namespace-Declaration) (jazz:walk-error walker resume declaration form-src "Definitions can only be defined inside namespaces: {s}" name)
      (let ((new-declaration (jazz:require-declaration declaration name)))
        (%%when (%%neq? expansion 'inline)
          ;; adding source information for parameters (default for optional and keyword may be source code)
          ;; jazz:find-annotated fails on first keyword at jazz.language.runtime.functional:minimum
          ;; because (eq? variable annotated-variable) -> one points to a stale value
          (let ((new-environment (if #f #; parameters
                                   (receive (signature augmented-environment) (jazz:walk-parameters walker resume declaration environment parameters #t #t)
                                     (%%set-definition-declaration-signature new-declaration signature)
                                     (%%cons new-declaration augmented-environment))
                                   (%%cons new-declaration environment))))
            (%%set-definition-declaration-value new-declaration (jazz:walk walker resume new-declaration new-environment value))))
        (%%set-declaration-source new-declaration form-src)
        new-declaration))))


;; Until I unify signature and function type
;; Only positional parameters as a first draft
(define (jazz:build-function-type signature result-type)
  (define (parameter-type parameter)
    (or (%%get-lexical-binding-type parameter)
        jazz:Any))
  
  (jazz:new-function-type
    (map parameter-type (%%get-signature-positional signature))
    (map parameter-type (%%get-signature-optional signature))
    (map parameter-type (%%get-signature-named signature))
    (let ((rest (%%get-signature-rest signature)))
      (and rest (parameter-type rest)))
    (or result-type jazz:Any)))


;;;
;;;; Specialize Macro
;;;


(define jazz:specialize-modifiers
  '(((inline onsite) . onsite)))


(define (jazz:parse-specialize walker resume declaration rest)
  (receive (expansion rest) (jazz:parse-modifiers walker resume declaration jazz:specialize-modifiers rest)
    (if (%%eq? (%%car rest) 'as)
        (values expansion (%%cadr rest) (%%cddr rest))
      (values expansion #f rest))))


(define (jazz:expand-specialize walker resume declaration environment . rest)
  (receive (expansion as rest) (jazz:parse-specialize walker resume declaration rest)
    (let ((signature (%%car rest))
          (rest (%%cdr rest)))
      (let ((operator (%%car signature))
            (parameters (%%cdr signature)))
        (let ((name (or as (jazz:compose-specializer-name operator parameters))))
          `(begin
             (definition public undocumented ,expansion (,name ,@parameters) ,@rest)
             (%specialize ,operator ,name)))))))


(define (jazz:compose-specializer-name operator parameters)
  (%%string->symbol
    (%%string-append (%%symbol->string operator)
                     (%%apply string-append (%%apply append (map (lambda (parameter)
                                                                   (if (jazz:specifier? parameter)
                                                                       (%%list (%%symbol->string (jazz:specifier->name parameter)))
                                                                     '()))
                                                                 parameters))))))


;;;
;;;; Specialize
;;;


(define (jazz:walk-%specialize-declaration walker resume declaration environment form-src)
  (let ((specialized (jazz:source-code (%%cadr (jazz:source-code form-src))))
        (specializer (jazz:source-code (%%car (%%cddr (jazz:source-code form-src))))))
    (let ((specialized-declaration (jazz:lookup-reference walker resume declaration environment specialized))
          (specializer-declaration (jazz:lookup-reference walker resume declaration environment specializer)))
      (jazz:add-specializer specialized-declaration specializer-declaration)
      (jazz:new-specialize))))


;; we should not even have to do this
(define (jazz:walk-%specialize walker resume declaration environment form-src)
  (jazz:new-specialize))


;;;
;;;; Generic
;;;


(define jazz:generic-modifiers
  '(((private protected package public) . public)
    ((deprecated undocumented uptodate) . uptodate)))


(define (jazz:parse-generic walker resume declaration rest)
  (receive (access compatibility rest) (jazz:parse-modifiers walker resume declaration jazz:generic-modifiers rest)
    (let ((signature (jazz:source-code (%%car rest))))
      (let ((name (jazz:source-code (%%car signature)))
            (parameters (%%cdr signature)))
        (jazz:parse-specifier (%%cdr rest)
          (lambda (specifier body)
            (values name specifier access compatibility parameters body)))))))


(define (jazz:walk-generic-declaration walker resume declaration environment form-src)
  (receive (name specifier access compatibility parameters body) (jazz:parse-generic walker resume declaration (%%cdr (jazz:source-code form-src)))
    (%%assertion (%%class-is? declaration jazz:Module-Declaration) (jazz:walk-error walker resume declaration form-src "Generics can only be defined at the module level: {s}" name)
      (let ((type (if specifier (jazz:walk-specifier walker resume declaration environment specifier) jazz:Any))
            (dispatch-type-declarations (map (lambda (dynamic-parameter-type)
                                               (jazz:lookup-reference walker resume declaration environment dynamic-parameter-type))
                                             (jazz:dynamic-parameter-types parameters)))
            (signature (jazz:walk-parameters walker resume declaration environment parameters #t #f)))
        (let ((new-declaration (jazz:new-generic-declaration name type access compatibility '() declaration dispatch-type-declarations signature)))
          (%%set-declaration-source new-declaration form-src)
          (let ((effective-declaration (jazz:add-declaration-child walker resume declaration new-declaration)))
            effective-declaration))))))


(define (jazz:walk-generic walker resume declaration environment form-src)
  (receive (name specifier access compatibility parameters body) (jazz:parse-generic walker resume declaration (%%cdr (jazz:source-code form-src)))
    (%%assertion (%%class-is? declaration jazz:Module-Declaration) (jazz:walk-error walker resume declaration form-src "Generics can only be defined at the module level: {s}" name)
      (receive (signature augmented-environment) (jazz:walk-parameters walker resume declaration environment parameters #t #t)
        (let ((new-declaration (jazz:require-declaration declaration name)))
          (%%set-generic-declaration-signature new-declaration signature)
          (%%set-generic-declaration-body new-declaration
                                          (jazz:walk-body walker resume new-declaration augmented-environment body))
          (%%set-declaration-source new-declaration form-src)
          new-declaration)))))


;;;
;;;; Specific
;;;


(define jazz:specific-modifiers
  '())


(define (jazz:parse-specific walker resume declaration rest)
  (receive (rest) (jazz:parse-modifiers walker resume declaration jazz:specific-modifiers rest)
    (let* ((signature (jazz:desourcify (%%car rest)))
           (body (%%cdr rest))
           (effective-body (if (%%null? body) (%%list (%%list 'unspecified)) body))
           (name (%%car signature))
           (parameters (%%cdr signature)))
      (values name parameters effective-body))))


(define (jazz:walk-specific walker resume declaration environment form-src)
  (define (root-dynamic-parameters? generic-declaration specific-signature name parameters)
    (let iter ((generic-parameters (%%get-signature-positional (%%get-generic-declaration-signature generic-declaration)))
               (specific-parameters (%%get-signature-positional specific-signature))
               (root? #t))
      (let ((generic-parameter (and (%%pair? generic-parameters) (%%car generic-parameters)))
            (specific-parameter (and (%%pair? specific-parameters) (%%car specific-parameters))))
        (let ((generic-dynamic? (%%is? generic-parameter jazz:Dynamic-Parameter))
              (specific-dynamic? (%%is? specific-parameter jazz:Dynamic-Parameter)))
          (cond ((and generic-dynamic? specific-dynamic?)
                 (let ((generic-class (jazz:resolve-binding (%%get-reference-binding (%%get-dynamic-parameter-class generic-parameter))))
                       (specific-class (jazz:resolve-binding (%%get-reference-binding (%%get-dynamic-parameter-class specific-parameter)))))
                   (if (jazz:of-subtype? generic-class specific-class)
                       (iter (%%cdr generic-parameters)
                             (%%cdr specific-parameters)
                             (%%eq? generic-class specific-class))
                     (jazz:walk-error walker resume declaration form-src "Dynamic parameter {a} is not a subtype of {a}: {s}"
                       (%%get-lexical-binding-name specific-parameter)
                       (%%get-declaration-locator generic-class)
                       (%%cons name parameters)))))
                ((or generic-dynamic? specific-dynamic?)
                 (jazz:walk-error walker resume declaration form-src "Specific must dispatch on the same number of dynamic parameters: {s}" (%%cons name parameters)))
                (else
                 root?))))))
  
  (receive (name parameters body) (jazz:parse-specific walker resume declaration (%%cdr (jazz:source-code form-src)))
    (%%assertion (%%class-is? declaration jazz:Module-Declaration) (jazz:walk-error walker resume declaration form-src "Specifics can only be defined at the module level: {s}" name)
      (let ((generic-declaration (jazz:lookup-declaration declaration name jazz:private-access declaration)))
        (if (%%class-is? generic-declaration jazz:Generic-Declaration)
            (receive (signature augmented-environment) (jazz:walk-parameters walker resume declaration environment parameters #t #t)
              (let* ((root? (root-dynamic-parameters? generic-declaration signature name parameters))
                     (new-declaration (jazz:new-specific-declaration name #f 'public 'uptodate '() declaration generic-declaration signature root?))
                     (body-environment (if root? augmented-environment (%%cons (jazz:new-nextmethod-variable 'nextmethod #f) augmented-environment))))
                (%%set-specific-declaration-body new-declaration
                                                 (jazz:walk-body walker resume new-declaration body-environment body))
                (%%set-declaration-source new-declaration form-src)
                new-declaration))
          (jazz:walk-error walker resume declaration form-src "Cannot find generic declaration for {s}" (%%cons name parameters)))))))


;;;
;;;; Class
;;;


(define jazz:class-modifiers
  '(((private protected package public) . public)
    ((abstract concrete) . concrete)
    ((deprecated undocumented uptodate) . uptodate)
    ((primitive native) . native)))

(define jazz:class-keywords
  '(metaclass extends implements attributes))


(define (jazz:parse-class walker resume declaration rest)
  (receive (access abstraction compatibility implementor rest) (jazz:parse-modifiers walker resume declaration jazz:class-modifiers rest)
    (let ((name (jazz:source-code (%%car rest)))
          (type jazz:Any)
          (rest (%%cdr rest)))
      (%%assert (%%symbol? name)
        (receive (metaclass-name ascendant-name interface-names attributes body) (jazz:parse-keywords jazz:class-keywords rest)
          (values name type access abstraction compatibility implementor metaclass-name ascendant-name interface-names attributes body))))))


(define (jazz:expand-class walker resume declaration environment form-src)
  (define (preprocess-meta body)
    (let ((metaclass (jazz:new-queue))
          (class (jazz:new-queue)))
      (define (expand-form-hack expr)
        (if (and (%%pair? (jazz:source-code expr))
                 (%%eq? (jazz:source-code (%%car (jazz:source-code expr))) 'form))
            (jazz:expand-macros walker resume declaration environment expr)
          expr))
      
      (define (preprocess expr)
        (let ((expr (expand-form-hack expr)))
          (cond ((and (%%pair? (jazz:source-code expr))
                      (%%eq? (jazz:source-code (%%car (jazz:source-code expr))) 'begin))
                 (for-each preprocess (%%cdr (jazz:source-code expr))))
                ((and (%%pair? (jazz:source-code expr))
                      (%%pair? (%%cdr (jazz:source-code expr)))
                      (%%eq? (jazz:source-code (%%cadr (jazz:source-code expr))) 'meta))
                 (jazz:enqueue metaclass (jazz:sourcify-if (%%cons (%%car (jazz:source-code expr)) (%%cddr (jazz:source-code expr))) expr)))
                (else
                 (jazz:enqueue class expr)))))
      
      (for-each preprocess body)
      (values (jazz:queue-list metaclass)
              (jazz:queue-list class))))
  
  (receive (name type access abstraction compatibility implementor metaclass-name ascendant-name interface-names attributes body) (jazz:parse-class walker resume declaration (%%cdr (jazz:source-code form-src)))
    (receive (metaclass-body class-body) (preprocess-meta body)
      (cond ((and (%%not-null? metaclass-body)
                  (%%specified? metaclass-name))
             (jazz:walk-error walker resume declaration form-src "Ambiguous use of both metaclass and meta keywords: {s}" name))
            ((or (%%specified? metaclass-name)
                 (jazz:core-class? name)
                 (%%unspecified? ascendant-name))
             (jazz:sourcify-if
               `(%class ,@(%%cdr (jazz:source-code form-src)))
               form-src))
            (else
             (let ((metaclass-name (%%string->symbol (%%string-append (%%symbol->string name) "~Class"))))
               `(begin
                  ,(jazz:sourcify-if
                     `(%class ,metaclass-name extends (:class ,ascendant-name)
                        ,@metaclass-body)
                     form-src)
                  ,(jazz:sourcify-if
                     `(%class ,access ,abstraction ,compatibility ,implementor ,name metaclass ,metaclass-name extends ,ascendant-name implements ,interface-names
                        ,@class-body)
                     form-src))))))))


(define (jazz:walk-%class-declaration walker resume declaration environment form-src)
  (receive (name type access abstraction compatibility implementor metaclass-name ascendant-name interface-names attributes body) (jazz:parse-class walker resume declaration (%%cdr (jazz:source-code form-src)))
    (%%assertion (%%class-is? declaration jazz:Module-Declaration) (jazz:walk-error walker resume declaration form-src "Classes can only be defined at the module level: {s}" name)
      ;; explicit test on Object-Class is to break circularity
      (receive (ascendant ascendant-relation ascendant-base) (jazz:lookup-ascendant walker resume declaration environment ascendant-name)
        (let ((metaclass (jazz:lookup-metaclass walker resume declaration environment ascendant metaclass-name))
              (interfaces (if (jazz:unspecified? interface-names) '() (map (lambda (interface-name) (jazz:lookup-reference walker resume declaration environment interface-name)) (jazz:listify interface-names)))))
          (let ((new-declaration (or (jazz:find-declaration-child declaration name)
                                     (jazz:new-class-declaration name type access compatibility attributes declaration implementor metaclass ascendant ascendant-relation ascendant-base interfaces))))
            (%%set-declaration-source new-declaration form-src)
            (let ((effective-declaration (jazz:add-declaration-child walker resume declaration new-declaration)))
              (jazz:setup-class-lookups effective-declaration)
              (let ((new-environment (%%cons effective-declaration environment)))
                (jazz:walk-declarations walker resume effective-declaration new-environment body)
                effective-declaration))))))))


(define (jazz:walk-%class walker resume declaration environment form-src)
  (receive (name type access abstraction compatibility implementor metaclass-name ascendant-name interface-names attributes body) (jazz:parse-class walker resume declaration (%%cdr (jazz:source-code form-src)))
    (%%assertion (%%class-is? declaration jazz:Module-Declaration) (jazz:walk-error walker resume declaration form-src "Classes can only be defined at the module level: {s}" name)
      (let* ((new-declaration (jazz:require-declaration declaration name))
             (new-environment (%%cons new-declaration environment))
             (ascendant-declaration (%%get-class-declaration-ascendant new-declaration)))
        (if (and (%%not ascendant-declaration) (%%neq? name 'Object))
            (jazz:walk-error walker resume declaration form-src "Class {s} does not specify an ascendant" name)
          (begin
            (%%set-namespace-declaration-body new-declaration (jazz:walk-namespace walker resume new-declaration new-environment body))
            (%%set-declaration-source new-declaration form-src)
            new-declaration))))))


(define (jazz:lookup-metaclass walker resume declaration environment ascendant metaclass-name)
  (if (or (jazz:unspecified? metaclass-name) (%%eq? metaclass-name 'Object-Class))
      #f
    (jazz:lookup-reference walker resume declaration environment metaclass-name)))


(define (jazz:lookup-ascendant walker resume declaration environment ascendant-name)
  (cond ((jazz:unspecified? ascendant-name)
         (values #f
                 #f
                 #f))
        ((and (%%pair? ascendant-name)
              (%%eq? (%%car ascendant-name) ':class))
         (let ((object-class (jazz:lookup-reference walker resume declaration environment 'Object-Class)))
           (let rec ((ascendant-name ascendant-name))
                (if (%%pair? ascendant-name)
                    (receive (decl relation base) (rec (%%cadr ascendant-name))
                      (values (if (%%eq? decl object-class)
                                  object-class
                                (or (jazz:effective-class-declaration-metaclass base) object-class))
                              (%%cons (%%car ascendant-name) relation)
                              base))
                  (let ((base (jazz:lookup-reference walker resume declaration environment ascendant-name)))
                    (values base '() base))))))
        (else
         (values (jazz:lookup-reference walker resume declaration environment ascendant-name)
                 #f
                 #f))))


(define (jazz:effective-class-declaration-metaclass class-declaration)
  (if (%%not class-declaration)
      #f
    (let ((class-declaration (jazz:resolve-binding class-declaration)))
      (or (%%get-category-declaration-metaclass class-declaration)
          (let ((ascendant (%%get-class-declaration-ascendant class-declaration)))
            (if (%%not ascendant)
                #f
              (jazz:effective-class-declaration-metaclass ascendant)))))))


;;;
;;;; Interface
;;;


(define jazz:interface-modifiers
  '(((private protected package public) . public)
    ((deprecated undocumented uptodate) . uptodate)
    ((primitive native) . native)))

(define jazz:interface-keywords
  '(metaclass extends attributes))


(define (jazz:parse-interface walker resume declaration rest)
  (receive (access compatibility implementor rest) (jazz:parse-modifiers walker resume declaration jazz:interface-modifiers rest)
    (let ((name (%%car rest))
          (type jazz:Any)
          (rest (%%cdr rest)))
      (%%assert (%%symbol? name)
        (receive (metaclass-name ascendant-names attributes body) (jazz:parse-keywords jazz:interface-keywords rest)
          (values name type access compatibility implementor metaclass-name ascendant-names attributes body))))))


(define (jazz:walk-interface-declaration walker resume declaration environment form-src)
  (let ((form (%%desourcify form-src)))
    (receive (name type access compatibility implementor metaclass-name ascendant-names attributes body) (jazz:parse-interface walker resume declaration (%%cdr form))
      (%%assertion (%%class-is? declaration jazz:Module-Declaration) (jazz:walk-error walker resume declaration form-src "Interfaces can only be defined at the module level: {s}" name)
        (let ((metaclass (if (or (jazz:unspecified? metaclass-name) (%%eq? metaclass-name 'Interface)) #f (jazz:lookup-reference walker resume declaration environment metaclass-name)))
              (ascendants (if (jazz:unspecified? ascendant-names) '() (map (lambda (ascendant-name) (jazz:lookup-reference walker resume declaration environment ascendant-name)) (jazz:listify ascendant-names)))))
          (let ((new-declaration (or (jazz:find-declaration-child declaration name)
                                     (jazz:new-interface-declaration name type access compatibility attributes declaration implementor metaclass ascendants))))
            (%%set-declaration-source new-declaration form-src)
            (let ((effective-declaration (jazz:add-declaration-child walker resume declaration new-declaration)))
              (jazz:setup-interface-lookups effective-declaration)
              (let ((new-environment (%%cons effective-declaration environment)))
                (jazz:walk-declarations walker resume effective-declaration new-environment body)
                effective-declaration))))))))


(define (jazz:walk-interface walker resume declaration environment form-src)
  (let ((form (%%desourcify form-src)))
    (receive (name type access compatibility implementor metaclass-name ascendant-names attributes body) (jazz:parse-interface walker resume declaration (%%cdr form))
      (%%assertion (%%class-is? declaration jazz:Module-Declaration) (jazz:walk-error walker resume declaration form-src "Interfaces can only be defined at the module level: {s}" name)
        (let* ((new-declaration (jazz:require-declaration declaration name))
               (new-environment (%%cons new-declaration environment)))
          (%%set-namespace-declaration-body new-declaration (jazz:walk-namespace walker resume new-declaration new-environment body))
          (%%set-declaration-source new-declaration form-src)
          new-declaration)))))


;;;
;;;; Slot
;;;


(define jazz:slot-modifiers
  '(((private protected package public) . #f)
    ((deprecated undocumented uptodate) . uptodate)))

(define jazz:slot-keywords
  '(initialize accessors getter setter))


(define jazz:slot-accessors-modifiers
  '(((private protected package public) . #f)
    ((final virtual chained override) . final)
    ((abstract concrete) . concrete)
    ((inline onsite) . inline)
    ((none generate explicit) . none)))


(define jazz:slot-accessor-modifiers
  '(((private protected package public) . #f)
    ((final virtual chained override) . #f)
    ((abstract concrete) . #f)
    ((inline onsite) . #f)
    ((none generate explicit) . #f)))


(define (jazz:parse-slot walker resume declaration form-src)
  (receive (access compatibility rest) (jazz:parse-modifiers walker resume declaration jazz:slot-modifiers (%%cdr (jazz:source-code form-src)))
    (let ((name (jazz:source-code (%%car rest))))
      (jazz:parse-specifier (%%cdr rest)
        (lambda (specifier rest)
          (receive (initialize accessors getter setter rest) (jazz:parse-keywords jazz:slot-keywords rest)
            (if (%%not-null? rest)
                (jazz:walk-error walker resume declaration form-src "Invalid slot definition: {s}" name)
              (values name specifier access compatibility initialize accessors getter setter))))))))


(define (jazz:expand-slot walker resume declaration environment form-src)
  (jazz:expand-slot-form walker resume declaration form-src '%slot))


(define (jazz:expand-slot-form walker resume declaration form-src symbol)
  (define (parse-accessors form slot-access)
    (receive (access propagation abstraction expansion generation rest) (jazz:parse-modifiers walker resume declaration jazz:slot-accessors-modifiers form)
      (if (%%not-null? rest)
          (jazz:walk-error walker resume declaration form-src "Invalid slot accessors definition: {s}" form)
        (values (or access slot-access) propagation abstraction expansion generation))))
  
  (define (parse-accessor slot-name default-access default-propagation default-abstraction default-expansion default-generation form prefix)
    (receive (access propagation abstraction expansion generation rest) (jazz:parse-modifiers walker resume declaration jazz:slot-accessor-modifiers form)
      (let ((generation (or generation default-generation)))
        (let ((name (cond ((%%null? rest)
                           (and (%%neq? generation 'none)
                                (%%string->symbol (%%string-append prefix (%%symbol->string slot-name)))))
                          ((%%null? (%%cdr rest))
                           (%%car rest))
                          (else
                           (jazz:walk-error walker resume declaration form-src "Invalid slot accessor definition: {s}" form)))))
          (values (or access default-access)
                  (or propagation default-propagation)
                  (or abstraction default-abstraction)
                  (or expansion default-expansion)
                  generation
                  name)))))
  
  (receive (name specifier access compatibility initialize accessors getter setter) (jazz:parse-slot walker resume declaration form-src)
    (let ((standardize
            (lambda (info)
              (let ((info (jazz:desourcify info)))
                (cond ((jazz:unspecified? info)
                       '())
                      ((%%symbol? info)
                       (%%list info))
                      (else
                       info))))))
      (let ((accessors (standardize accessors))
            (getter (standardize getter))
            (setter (standardize setter)))
        (receive (default-access default-propagation default-abstraction default-expansion default-generation) (parse-accessors accessors access)
          (receive (getter-access getter-propagation getter-abstraction getter-expansion getter-generation getter-name) (parse-accessor name default-access default-propagation default-abstraction default-expansion default-generation getter "get-")
            (receive (setter-access setter-propagation setter-abstraction setter-expansion setter-generation setter-name) (parse-accessor name default-access default-propagation default-abstraction default-expansion default-generation setter "set-")
              (let ((name-self (%%string->symbol (%%string-append (%%symbol->string name) "~self")))
                    (generate-getter? (%%eq? getter-generation 'generate))
                    (generate-setter? (%%eq? setter-generation 'generate))
                    (specifier-list (if specifier (%%list specifier) '())))
                `(begin
                   ,(jazz:sourcify-if
                      `(,symbol ,name ,specifier ,access ,compatibility ,(if (%%unspecified? initialize) initialize `(with-self ,initialize)) ,getter-name ,setter-name)
                      form-src)
                   ,@(if generate-getter?
                         `((method ,(or getter-access 'public) ,getter-propagation ,getter-abstraction ,getter-expansion (,getter-name) ,@specifier-list
                             ,name-self))
                       '())
                   ,@(if generate-setter?
                         `((method ,(or setter-access 'protected) ,setter-propagation ,setter-abstraction ,setter-expansion (,setter-name value ,@specifier-list) <void>
                             (set! ,name-self value)))
                       '()))))))))))


(define (jazz:walk-%slot-declaration walker resume declaration environment form-src)
  (let ((form (%%desourcify form-src)))
    (jazz:bind (name specifier access compatibility initialize getter-name setter-name) (%%cdr form)
      (%%assertion (%%class-is? declaration jazz:Class-Declaration) (jazz:walk-error walker resume declaration form-src "Slots can only be defined inside classes: {s}" name)
        (let ((type (if specifier (jazz:walk-specifier walker resume declaration environment specifier) jazz:Any))
              (new (if (%%eq? (%%car form) '%property) jazz:new-property-declaration jazz:new-slot-declaration)))
          (let ((new-declaration (or (jazz:find-declaration-child declaration name)
                                     (new name type access compatibility '() declaration #f getter-name setter-name))))
            (%%set-declaration-source new-declaration form-src)
            (let ((effective-declaration (jazz:add-declaration-child walker resume declaration new-declaration)))
              effective-declaration)))))))


(define (jazz:walk-%slot walker resume declaration environment form-src)
  (let ((form (%%desourcify form-src)))
    (jazz:bind (name specifier access compatibility initialize getter-name setter-name) (%%cdr form)
      (%%assertion (%%class-is? declaration jazz:Class-Declaration) (jazz:walk-error walker resume declaration form-src "Slots can only be defined inside classes: {s}" name)
        (let ((new-declaration (jazz:require-declaration declaration (%%cadr form))))
          (%%set-slot-declaration-initialize new-declaration
            (and (%%specified? initialize) (jazz:walk walker resume declaration environment initialize)))
          (%%when (%%class-is? new-declaration jazz:Property-Declaration)
            (let ((allocate? (%%neq? (%%get-lexical-binding-type new-declaration) jazz:Void)))
              (%%set-property-declaration-getter new-declaration
                (jazz:walk walker resume declaration environment
                  (cond (getter-name
                          `(lambda (self)
                             (with-self
                               (,getter-name))))
                        (allocate?
                          `(lambda (self)
                             (with-self ,name)))
                        (else #f))))
              (%%set-property-declaration-setter new-declaration
                (let ((value (jazz:generate-symbol "val")))
                  (jazz:walk walker resume declaration environment
                    (cond (setter-name
                            `(lambda (self ,value)
                               (with-self
                                 (,setter-name ,value))))
                          (allocate?
                            `(lambda (self ,value)
                               (with-self
                                 (set! ,name ,value))))
                          (else #f)))))))
          (%%set-declaration-source new-declaration form-src)
          new-declaration)))))


;;;
;;;; Method
;;;


(define jazz:method-modifiers
  '(((private protected package public) . protected)
    ((deprecated undocumented uptodate) . uptodate)
    ((final virtual chained override) . final)
    ((abstract concrete core) . concrete)
    ((inline onsite) . onsite)
    ;; quicky
    ((remote notremote) . notremote)
    ((synchronized notsynchronized) . notsynchronized)))


(define (jazz:parse-method walker resume declaration rest)
  (receive (access compatibility propagation abstraction expansion remote synchronized rest) (jazz:parse-modifiers walker resume declaration jazz:method-modifiers rest)
    (%%assertion (and (%%pair? rest) (%%pair? (jazz:source-code (%%car rest)))) (jazz:walk-error walker resume declaration #f "Ill-formed method in {a}: {s}" (%%get-lexical-binding-name (%%get-declaration-toplevel declaration)) (%%cons 'method (jazz:desourcify-all rest)))
      (let ((name (jazz:source-code (%%car (jazz:source-code (%%car rest)))))
            (parameters (jazz:wrap-parameters (%%cdr (%%desourcify (%%car rest))))))
        (jazz:parse-specifier (%%cdr rest)
          (lambda (specifier body)
            (let ((effective-body
                    (if (%%null? body)
                        (and (%%eq? abstraction 'concrete)
                             (%%list (%%list 'unspecified)))
                      body)))
              (values name specifier access compatibility propagation abstraction expansion remote synchronized parameters effective-body))))))))


(define (jazz:walk-method-declaration walker resume declaration environment form-src)
  (define (find-root-declaration name)
    (let* ((next-declaration (jazz:lookup-declaration declaration name jazz:private-access declaration))
           (root-declaration (and next-declaration (or (%%get-method-declaration-root next-declaration) next-declaration))))
      (if (and root-declaration (%%eq? declaration (%%get-declaration-parent root-declaration)))
          #f
        root-declaration)))
  
  (receive (name specifier access compatibility propagation abstraction expansion remote synchronized parameters body) (jazz:parse-method walker resume declaration (%%cdr (jazz:source-code form-src)))
    (%%assertion (%%class-is? declaration jazz:Category-Declaration) (jazz:walk-error walker resume declaration form-src "Methods can only be defined inside categories: {s}" name)
      (let ((type (if specifier (jazz:new-function-type '() '() '() #f (jazz:walk-specifier walker resume declaration environment specifier)) jazz:Procedure))
            (inline? (and (%%eq? expansion 'inline) body)))
        (receive (signature augmented-environment)
            ;; yuck. to clean
            (if inline?
                (jazz:walk-parameters walker resume declaration environment parameters #t #t)
              (values
                (jazz:walk-parameters walker resume declaration environment parameters #t #f)
                (jazz:unspecified)))
          (let* ((root-declaration (find-root-declaration name))
                 (new-declaration (or (jazz:find-declaration-child declaration name)
                                      (jazz:new-method-declaration name type access compatibility '() declaration root-declaration propagation abstraction expansion remote synchronized signature))))
            (%%set-declaration-source new-declaration form-src)
            (let ((effective-declaration (jazz:add-declaration-child walker resume declaration new-declaration)))
              (%%when inline?
                (%%set-method-declaration-signature effective-declaration signature)
                (%%set-method-declaration-body effective-declaration
                  (jazz:walk walker resume effective-declaration augmented-environment
                    `(with-self ,@body))))
              effective-declaration)))))))


(define (jazz:walk-method walker resume declaration environment form-src)
  (receive (name specifier access compatibility propagation abstraction expansion remote synchronized parameters body) (jazz:parse-method walker resume declaration (%%cdr (jazz:source-code form-src)))
    (%%assertion (%%class-is? declaration jazz:Category-Declaration) (jazz:walk-error walker resume declaration form-src "Methods can only be defined inside categories: {s}" name)
      (let* ((new-declaration (jazz:lookup-declaration declaration name jazz:private-access declaration))
             (category-declaration (%%get-declaration-parent new-declaration))
             (root-method-declaration (%%get-method-declaration-root new-declaration))
             (root-method-propagation (and root-method-declaration (%%get-method-declaration-propagation root-method-declaration)))
             (root-category-declaration (and root-method-declaration (%%get-declaration-parent root-method-declaration))))
        (cond ((and root-category-declaration (%%eq? root-method-propagation 'final))
               (jazz:walk-error walker resume declaration form-src "Cannot redefine method: {s}" name))
              ((and root-category-declaration (%%memq root-method-propagation '(virtual chained)) (%%neq? propagation 'override))
               (case propagation
                 ((virtual chained)
                  (jazz:walk-error walker resume declaration form-src "Method is already virtual: {s}" name))
                 ((final)
                  (jazz:walk-error walker resume declaration form-src "Cannot finalize virtual method: {s}" name))))
              ((and (%%not root-category-declaration) (%%eq? propagation 'override))
               (jazz:walk-error walker resume declaration form-src "Cannot find root method: {s}" name))
              ((and (%%not root-category-declaration) (%%class-is? category-declaration jazz:Interface-Declaration) (%%neq? propagation 'virtual))
               (jazz:walk-error walker resume declaration form-src "Interface method must be virtual: {s}" name))
              (else
               (if (and (jazz:analysis-mode?)
                        (%%eq? (%%get-method-declaration-propagation new-declaration) 'final))
                   (let ((data (jazz:get-analysis-data (%%get-declaration-locator new-declaration))))
                     (%%set-analysis-data-declaration-references data '())))
               (receive (signature augmented-environment) (jazz:walk-parameters walker resume declaration environment parameters #t #t)
                 (let ((body-expression
                         (cond (root-category-declaration
                                (jazz:walk walker resume new-declaration (%%cons (jazz:new-nextmethod-variable 'nextmethod (%%get-lexical-binding-type root-method-declaration)) augmented-environment) `(with-self ,@body)))
                               (body
                                (jazz:walk walker resume new-declaration augmented-environment `(with-self ,@body)))
                               (else
                                #f))))
                   (%%when (%%not (and (%%eq? expansion 'inline) (%%neq? abstraction 'abstract)))
                     (%%set-method-declaration-signature new-declaration signature)
                     (%%set-method-declaration-body new-declaration body-expression))
                   (%%set-declaration-source new-declaration form-src)
                   new-declaration))))))))


;; quick not elegant solution to wrap with-self around parameter code
(define (jazz:wrap-parameters parameters)
  (let ((queue (jazz:new-queue)))
    (let iter ((scan parameters))
      (cond ((%%null? scan))
            ((%%symbol? scan)
             (jazz:enqueue-list queue scan))
            (else
             (let ((parameter (%%car scan)))
               (if (%%pair? parameter)
                   (if (jazz:specifier? (%%car scan))
                       (jazz:enqueue queue parameter)
                     (if (%%keyword? (%%car parameter))
                         (jazz:parse-specifier (%%cddr parameter)
                           (lambda (specifier rest)
                             (let ((specifier-list (if specifier (%%list specifier) '())))
                               (jazz:enqueue queue `(,(%%car parameter) ,(%%cadr parameter) ,@specifier-list (with-self ,(%%car rest)))))))
                       (jazz:parse-specifier (%%cdr parameter)
                         (lambda (specifier rest)
                           (let ((specifier-list (if specifier (%%list specifier) '())))
                             (jazz:enqueue queue `(,(%%car parameter) ,@specifier-list (with-self ,(%%car rest)))))))))
                 (jazz:enqueue queue parameter)))
             (iter (%%cdr scan)))))
    (jazz:queue-list queue)))


;;;
;;;; NextMethod Variable
;;;


(jazz:define-class jazz:NextMethod-Variable)


(define (jazz:new-nextmethod-variable name type)
  (%%assertion (jazz:variable-name-valid? name) (jazz:error "Invalid variable name: {s}" name)
    (jazz:allocate-nextmethod-variable jazz:NextMethod-Variable name type #f #f 0)))


(jazz:define-method (jazz:emit-binding-reference (jazz:NextMethod-Variable binding) source-declaration environment backend)
  (jazz:new-code
    (jazz:emit 'nextmethod-variable-reference backend binding)
    jazz:Any
    #f))


(jazz:define-method (jazz:walk-binding-validate-call (jazz:NextMethod-Variable declaration) walker resume source-declaration operator arguments form-src)
  (let ((signature (jazz:get-nextmethod-signature source-declaration)))
    (if signature
        (jazz:validate-arguments walker resume source-declaration declaration signature arguments form-src))))


(jazz:define-method (jazz:emit-binding-call (jazz:NextMethod-Variable binding) binding-src arguments source-declaration environment backend)
  (let ((type (%%get-lexical-binding-type binding))
        (self (jazz:*self*)))
    (jazz:new-code
      (jazz:emit 'nextmethod-binding-call backend binding binding-src self arguments)
      (jazz:call-return-type type)
      #f)))


;;;
;;;; Self Binding
;;;


(jazz:define-class jazz:Self-Binding)


(define (jazz:new-self-binding type)
  (jazz:allocate-self-binding jazz:Self-Binding 'self type #f))


(jazz:define-method (jazz:emit-binding-reference (jazz:Self-Binding declaration) source-declaration environment backend)
  (jazz:new-code
    'self
    (jazz:emit 'self-reference backend declaration source-declaration)
    #f))


;;;
;;;; Dynamic Self Binding
;;;


(jazz:define-class jazz:Dynamic-Self-Binding)


(define (jazz:new-dynamic-self-binding type code)
  (jazz:allocate-dynamic-self-binding jazz:Dynamic-Self-Binding 'self type #f code))


(jazz:define-method (jazz:emit-binding-reference (jazz:Dynamic-Self-Binding declaration) source-declaration environment backend)
  (jazz:new-code
    (jazz:emit 'dynamic-self-reference backend declaration)
    (%%get-declaration-parent source-declaration)
    #f))


;;;
;;;; With Self
;;;


(define (jazz:walk-with-self walker resume declaration environment form-src)
  (let ((form (%%desourcify form-src)))
    (let ((new-environment (%%cons (jazz:new-self-binding #f) environment)))
      (jazz:new-with-self
        (let ((body (%%cdr form)))
          (jazz:walk-body walker resume declaration new-environment body))))))


(define jazz:*self*
  (make-parameter #f))


;;;
;;;; With Local Variables
;;;


(define (jazz:parse-with-local-variables form)
  (let ((variables (%%car form))
        (body (%%cdr form)))
    (values variables body)))


(define (jazz:walk-with-local-variables-declaration walker resume declaration environment form-src)
  (receive (variables body) (jazz:parse-with-local-variables (%%cdr (%%desourcify form-src)))
    (jazz:walk-declarations walker resume declaration environment body)))


(define (jazz:walk-with-local-variables walker resume declaration environment form-src)
  (define (make-bindings variables)
    (let ((table (%%make-table test: eq?)))
      (for-each (lambda (variable)
                  (let ((name (%%get-lexical-binding-name variable)))
                    (%%table-set! table name variable)))
                variables)
      table))
  
  (receive (variables body) (jazz:parse-with-local-variables (%%cdr (%%desourcify form-src)))
    (let ((new-environment (%%cons (jazz:new-walk-frame (make-bindings variables)) environment)))
      (jazz:new-begin form-src (jazz:walk-list walker resume declaration new-environment body)))))


;;;
;;;; Proclaim
;;;


(jazz:define-method (jazz:validate-proclaim (jazz:Jazz-Walker walker) resume declaration environment form-src)
  (if (and (%%not (%%class-is? declaration jazz:Module-Declaration))
           (%%not (%%class-is? declaration jazz:Category-Declaration)))
      (jazz:walk-error walker resume declaration form-src "For now, proclaim can only be used at the module or category level")))


;;;
;;;; Declare
;;;


(define (jazz:walk-declare walker resume declaration environment form-src)
  (let ((form (%%desourcify form-src)))
    (let ((declarations (%%cdr form)))
      (jazz:new-declare declarations))))


;;;
;;;; Parameterize
;;;


(define (jazz:walk-parameterize walker resume declaration environment form-src)
  (let ((bindings (jazz:source-code (%%cadr (jazz:source-code form-src))))
        (body (%%cddr (jazz:source-code form-src))))
    (let ((effective-body (if (%%null? body) (%%list (%%list 'unspecified)) body))
          (expanded-bindings (jazz:new-queue)))
      (for-each (lambda (binding-form)
                  (continuation-capture
                    (lambda (resume)
                      (let ((variable (%%car (jazz:source-code binding-form)))
                            (value (%%car (jazz:source-code (%%cdr (jazz:source-code binding-form))))))
                        (jazz:enqueue expanded-bindings
                                      (%%cons (continuation-capture
                                                (lambda (resume)
                                                  (jazz:walk walker resume declaration environment variable)))
                                              (continuation-capture
                                                (lambda (resume)
                                                  (jazz:walk walker resume declaration environment value)))))))))
                bindings)
      (jazz:new-parameterize (jazz:queue-list expanded-bindings)
                             (jazz:walk-body walker resume declaration environment effective-body)))))


;;;
;;;; With Slots
;;;


(define (jazz:walk-with-slots walker resume declaration environment form-src)
  (let ((form (%%desourcify form-src)))
    (jazz:bind (slot-names object . body) (%%cdr form)
      (let ((object-symbol (jazz:generate-symbol "object")))
        (jazz:walk walker resume declaration environment
          `(let ((,object-symbol ,object))
             (let-symbol ,(map (lambda (slot-name)
                                 (let* ((slot-declaration (jazz:lookup-reference walker resume declaration environment slot-name))
                                        (getter-name (%%get-slot-declaration-getter-name slot-declaration))
                                        (setter-name (%%get-slot-declaration-setter-name slot-declaration)))
                                   (%%list slot-name `(lambda () (%%list ',getter-name ',object-symbol)) `(lambda (value) (%%list ',setter-name ',object-symbol value)))))
                               slot-names)
                         ,@body)))))))


;;;
;;;; Time
;;;


(define (jazz:walk-time walker resume declaration environment form-src)
  (let ((form (%%desourcify form-src)))
    (let ((forms (%%cdr form)))
      (jazz:new-time-special (jazz:walk-list walker resume declaration environment forms)))))


;;;
;;;; Dialect
;;;


(jazz:define-dialect jazz
  (jazz:new-jazz-dialect 'jazz))

(jazz:define-dialect jazz.dialect
  (jazz:new-jazz-dialect 'jazz.dialect))


(jazz:define-walker-declaration definition           jazz jazz:walk-definition-declaration jazz:walk-definition)
(jazz:define-walker-declaration generic              jazz jazz:walk-generic-declaration jazz:walk-generic)
(jazz:define-walker-special     specific             jazz jazz:walk-specific)
(jazz:define-walker-syntax      class                jazz jazz:expand-class)
(jazz:define-walker-declaration %class               jazz jazz:walk-%class-declaration jazz:walk-%class)
(jazz:define-walker-declaration interface            jazz jazz:walk-interface-declaration jazz:walk-interface)
(jazz:define-walker-syntax      slot                 jazz jazz:expand-slot)
(jazz:define-walker-syntax      property             jazz jazz:expand-property)
(jazz:define-walker-declaration %slot                jazz jazz:walk-%slot-declaration jazz:walk-%slot)
(jazz:define-walker-declaration %property            jazz jazz:walk-%slot-declaration jazz:walk-%slot)
(jazz:define-walker-declaration method               jazz jazz:walk-method-declaration jazz:walk-method)
(jazz:define-walker-special     declare              jazz jazz:walk-declare)
(jazz:define-walker-macro       specialize           jazz jazz:expand-specialize)
(jazz:define-walker-declaration %specialize          jazz jazz:walk-%specialize-declaration jazz:walk-%specialize)
(jazz:define-walker-special     parameterize         jazz jazz:walk-parameterize)
(jazz:define-walker-special     with-slots           jazz jazz:walk-with-slots)
(jazz:define-walker-special     with-self            jazz jazz:walk-with-self)
(jazz:define-walker-declaration with-dynamic-self    jazz jazz:walk-with-dynamic-self-declaration jazz:walk-with-dynamic-self)
(jazz:define-walker-declaration with-local-variables jazz jazz:walk-with-local-variables-declaration jazz:walk-with-local-variables)
(jazz:define-walker-special     cast                 jazz jazz:walk-cast)
(jazz:define-walker-special     allocate             jazz jazz:walk-allocate)
(jazz:define-walker-special     time                 jazz jazz:walk-time))
