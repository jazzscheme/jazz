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
;;;  Portions created by the Initial Developer are Copyright (C) 1996-2015
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


(jazz:define-class jazz:Definition-Declaration jazz:Declaration (constructor: jazz:allocate-definition-declaration)
  ((expansion getter: generate)
   (signature getter: generate setter: generate)
   (value     getter: generate setter: generate)))


(define (jazz:new-definition-declaration name type access compatibility attributes parent expansion signature)
  (let ((new-declaration (jazz:allocate-definition-declaration name type #f access compatibility attributes #f parent #f #f #f expansion signature #f)))
    (jazz:setup-declaration new-declaration)
    new-declaration))


(jazz:define-method (jazz:walk-binding-validate-call (jazz:Definition-Declaration declaration) walker resume source-declaration operator arguments form-src)
  (let ((signature (jazz:get-definition-declaration-signature declaration)))
    (if signature
        (jazz:validate-arguments walker resume source-declaration declaration signature arguments form-src))))


(jazz:define-method (jazz:emit-inlined-binding-call (jazz:Definition-Declaration declaration) arguments call source-declaration environment backend)
  (let ((value (jazz:get-definition-declaration-value declaration)))
    (if (%%class-is? value jazz:Lambda)
        (if (%%eq? (jazz:get-definition-declaration-expansion declaration) 'inline)
            (let ((signature (jazz:get-lambda-signature value))
                  (body (jazz:get-lambda-body value)))
              (if (jazz:only-positional-signature? signature)
                  (if (%%fx= (jazz:get-signature-mandatory signature) (%%length arguments))
                      (jazz:with-annotated-frame (jazz:annotate-inlined-signature signature arguments)
                        (lambda (frame)
                          (let ((augmented-environment (%%cons frame environment)))
                            (let ((body-code (jazz:emit-expression body source-declaration augmented-environment backend)))
                              (jazz:new-code
                                `(let ,(map (lambda (parameter argument)
                                              `(,(jazz:emit-binding-symbol parameter source-declaration environment backend)
                                                ,(jazz:emit-type-check argument (jazz:get-lexical-binding-type parameter) source-declaration environment backend)))
                                            (jazz:get-signature-positional signature)
                                            arguments)
                                   ,@(jazz:sourcify-list (jazz:get-code-form body-code) (jazz:get-expression-source call)))
                                (jazz:call-return-type (jazz:get-lexical-binding-type declaration))
                                #f)))))
                    (jazz:error "Wrong number of arguments passed to {s}" (jazz:get-lexical-binding-name declaration)))
                (jazz:error "Only positional parameters are supported in inlining: {s}" (jazz:get-lexical-binding-name declaration))))
          #f)
      #f #;
      ;; not correct as the value is always #f when looking up external declarations!
      ;; we need to walk the value even at parse time for inline declarations
      (jazz:error "Constant inlining is not yet supported: {s}" (jazz:get-lexical-binding-name declaration)))))


;; quick solution for now as some inlined definitions like /= will change the semantics if not inlined
;; (not needed anymore now that we always inline even in debug)
(define (jazz:untyped-inline-definition? value)
  (jazz:every? (lambda (parameter)
                 (%%not (jazz:get-lexical-binding-type parameter)))
               (jazz:get-signature-positional (jazz:get-lambda-signature value))))


(jazz:define-method (jazz:emit-declaration (jazz:Definition-Declaration declaration) environment backend)
  (let ((value (jazz:get-definition-declaration-value declaration))
        (type (jazz:get-lexical-binding-type declaration)))
    (if (and jazz:debug-user?
             (%%class-is? value jazz:Lambda)
             ;; no need to emit unsafe and safe when inline because as the checks are not included
             ;; in the body it is possible to define the function itself with checks so it is safe
             ;; used not inlined and only insert checks at call site when the types don't match
             (%%neq? (jazz:get-definition-declaration-expansion declaration) 'inline)
             ;; safe first iteration simplification
             (jazz:only-positional-signature? (jazz:get-lambda-signature value))
             (jazz:typed-signature? (jazz:get-lambda-signature value)))
        (let ((safe (jazz:emit-type-check (jazz:emit-safe value declaration environment backend) type declaration environment backend))
              (unsafe (jazz:emit-type-check (jazz:emit-unsafe value declaration environment backend) type declaration environment backend)))
          (jazz:emit backend 'definition declaration environment safe unsafe))
      (let ((expression (jazz:emit-type-check (jazz:emit-expression value declaration environment backend) type declaration environment backend)))
        (jazz:emit backend 'definition declaration environment expression #f)))))


(jazz:define-method (jazz:emit-binding-reference (jazz:Definition-Declaration declaration) source-declaration environment backend)
  (jazz:new-code
    (let ((value (jazz:get-definition-declaration-value declaration)))
      (if (and (%%eq? (jazz:get-definition-declaration-expansion declaration) 'inline)
               (%%is? value jazz:Constant))
          (jazz:get-code-form (jazz:emit-expression value declaration environment backend))
        (jazz:emit backend 'definition-reference declaration)))
    (or (jazz:find-annotated-type declaration environment)
        jazz:Any)
    #f))


(jazz:define-method (jazz:walk-binding-validate-assignment (jazz:Definition-Declaration declaration) walker resume source-declaration symbol-src)
  (nextmethod declaration walker resume source-declaration symbol-src)
  (%%when (%%neq? (jazz:get-declaration-toplevel declaration) (jazz:get-declaration-toplevel source-declaration))
    (jazz:walk-error walker resume source-declaration symbol-src "Illegal inter-module assignment to: {s}" (jazz:get-lexical-binding-name declaration))))


(jazz:define-method (jazz:walk-binding-assignable? (jazz:Definition-Declaration declaration))
  #t)


(jazz:define-method (jazz:emit-binding-assignment (jazz:Definition-Declaration declaration) value source-declaration environment backend)
  (let ((value (jazz:emit-expression value source-declaration environment backend)))
    (jazz:new-code
      (jazz:emit backend 'definition-assignment declaration source-declaration environment value)
      jazz:Any
      #f)))


(jazz:define-method (jazz:tree-fold (jazz:Definition-Declaration declaration) down up here seed environment)
  (here declaration seed environment)
  (jazz:tree-fold (jazz:get-definition-declaration-value declaration) down up here seed environment))


(jazz:define-method (jazz:outline-extract (jazz:Definition-Declaration declaration) meta)
  (let ((signature (jazz:get-definition-declaration-signature declaration)))
    (if (not signature)
        `(definition ,(jazz:get-declaration-access declaration) ,(jazz:get-lexical-binding-name declaration) ,@(jazz:outline-generate-type-list (jazz:get-lexical-binding-type declaration)))
      `(definition ,(jazz:get-declaration-access declaration) (,(jazz:get-lexical-binding-name declaration) ,@(jazz:outline-generate-signature signature)) ,@(jazz:outline-generate-type-list (jazz:get-function-type-result (jazz:get-lexical-binding-type declaration)))))))


;;;
;;;; Specialize
;;;


(jazz:define-class jazz:Specialize jazz:Expression (constructor: jazz:allocate-specialize)
  ())


(define (jazz:new-specialize)
  (jazz:allocate-specialize #f #f))


(jazz:define-method (jazz:emit-expression (jazz:Specialize expression) declaration environment backend)
  (jazz:new-code
    (jazz:emit backend 'specialize expression declaration environment)
    jazz:Any
    #f))


;;;
;;;; Generic-Declaration
;;;


(jazz:define-class jazz:Generic-Declaration jazz:Declaration (constructor: jazz:allocate-generic-declaration)
  ((dispatch-types getter: generate)
   (signature      getter: generate setter: generate)
   (body           getter: generate setter: generate)))


(define (jazz:new-generic-declaration name type access compatibility attributes parent dispatch-types signature)
  (let ((new-declaration (jazz:allocate-generic-declaration name type #f access compatibility attributes #f parent #f #f #f dispatch-types signature #f)))
    (jazz:setup-declaration new-declaration)
    new-declaration))


(jazz:define-method (jazz:walk-binding-validate-call (jazz:Generic-Declaration declaration) walker resume source-declaration operator arguments form-src)
  (jazz:validate-arguments walker resume source-declaration declaration (jazz:get-generic-declaration-signature declaration) arguments form-src))


(jazz:define-method (jazz:emit-declaration (jazz:Generic-Declaration declaration) environment backend)
  (let ((signature (jazz:get-generic-declaration-signature declaration))
        (body (jazz:get-generic-declaration-body declaration)))
    (jazz:with-annotated-frame (jazz:annotate-signature signature)
      (lambda (frame)
        (let ((augmented-environment (%%cons frame environment)))
          (let ((signature-emit (jazz:emit-signature signature declaration augmented-environment backend))
                (body-emit (jazz:emit-expression body declaration augmented-environment backend)))
            (jazz:sourcify-deep-if
              (jazz:emit backend 'generic declaration environment signature-emit body-emit)
              (jazz:get-declaration-source declaration))))))))


(jazz:define-method (jazz:emit-binding-reference (jazz:Generic-Declaration declaration) source-declaration environment backend)
  (jazz:new-code
    (jazz:emit backend 'generic-reference declaration)
    jazz:Any
    #f))


(jazz:define-method (jazz:tree-fold (jazz:Generic-Declaration declaration) down up here seed environment)
  (here declaration seed environment)
  (jazz:tree-fold-list (jazz:get-signature-expressions (jazz:get-generic-declaration-signature declaration)) down up here seed environment)
  (jazz:tree-fold (jazz:get-generic-declaration-body declaration) down up here seed environment))


(jazz:define-method (jazz:outline-extract (jazz:Generic-Declaration declaration) meta)
  `(generic (,(jazz:get-lexical-binding-name declaration) ,@(jazz:outline-generate-signature (jazz:get-generic-declaration-signature declaration)))))


;;;
;;;; Specific-Declaration
;;;


(jazz:define-class jazz:Specific-Declaration jazz:Declaration (constructor: jazz:allocate-specific-declaration)
  ((generic   getter: generate)
   (signature getter: generate)
   (body      getter: generate setter: generate)
   (root?     getter: generate)))


(define (jazz:new-specific-declaration name type access compatibility attributes parent generic signature root?)
  (let ((new-declaration (jazz:allocate-specific-declaration name type #f access compatibility attributes #f parent #f #f #f generic signature #f root?)))
    (jazz:setup-declaration new-declaration)
    new-declaration))


(jazz:define-method (jazz:get-nextmethod-signature (jazz:Specific-Declaration declaration))
  (jazz:get-specific-declaration-signature declaration))


(jazz:define-method (jazz:emit-declaration (jazz:Specific-Declaration declaration) environment backend)
  (let* ((generic-declaration (jazz:get-specific-declaration-generic declaration))
         (generic-locator (jazz:get-declaration-locator generic-declaration))
         (generic-object-locator (jazz:generic-object-locator generic-locator))
         (signature (jazz:get-specific-declaration-signature declaration))
         (body (jazz:get-specific-declaration-body declaration)))
    (jazz:with-annotated-frame (jazz:annotate-signature signature)
      (lambda (frame)
        (let ((augmented-environment (%%cons frame environment)))
          (let ((signature-emit (jazz:emit-signature signature declaration augmented-environment backend))
                (body-emit (jazz:emit-expression body declaration augmented-environment backend)))
            (jazz:sourcify-deep-if
              (jazz:emit backend 'specific declaration environment signature-emit body-emit)
              (jazz:get-declaration-source declaration))))))))


(jazz:define-method (jazz:tree-fold (jazz:Specific-Declaration declaration) down up here seed environment)
  (here declaration seed environment)
  (jazz:tree-fold-list (jazz:get-signature-expressions (jazz:get-specific-declaration-signature declaration)) down up here seed environment)
  (jazz:tree-fold (jazz:get-specific-declaration-body declaration) down up here seed environment))


;;;
;;;; Category-Declaration
;;;


(jazz:define-class jazz:Category-Declaration jazz:Namespace-Declaration ()
  ((implementor         getter: generate)
   (metaclass           getter: generate)
   (metaclass-explicit? getter: generate)))


(jazz:define-method (jazz:emit-binding-reference (jazz:Category-Declaration declaration) source-declaration environment backend)
  (jazz:new-code
    (jazz:emit backend 'category-reference declaration)
    jazz:Category-Declaration
    #f))


;;;
;;;; Class-Declaration
;;;


(jazz:define-class jazz:Class-Declaration jazz:Category-Declaration (constructor: jazz:allocate-class-declaration)
  ((ascendant          getter: generate)
   (ascendant-relation getter: generate)
   (ascendant-base     getter: generate)
   (interfaces         getter: generate)))


(define (jazz:new-class-declaration name type access compatibility attributes parent implementor metaclass metaclass-explicit? ascendant ascendant-relation ascendant-base interfaces)
  (let ((new-declaration (jazz:allocate-class-declaration name type #f access compatibility attributes #f parent #f #f #f (jazz:make-access-lookups jazz:protected-access) (jazz:new-queue) #f implementor metaclass metaclass-explicit? ascendant ascendant-relation ascendant-base interfaces)))
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
  
  (let ((ascendant (resolve-class (jazz:get-class-declaration-ascendant class-declaration)))
        (interfaces (map resolve-interface (jazz:get-class-declaration-interfaces class-declaration))))
    
    (let ((private (jazz:get-private-lookup class-declaration)))
      (if ascendant
          (let ((same-module? (%%eq? (jazz:get-declaration-toplevel class-declaration)
                                     (jazz:get-declaration-toplevel ascendant))))
            (%%table-merge! private (jazz:get-access-lookup ascendant (if same-module? jazz:private-access jazz:public-access)) #t)))
      (for-each (lambda (interface)
                  (%%table-merge! private (jazz:get-public-lookup interface)))
                interfaces))
    
    (let ((public (jazz:get-public-lookup class-declaration)))
      (if ascendant
          (%%table-merge! public (jazz:get-public-lookup ascendant)))
      (for-each (lambda (interface)
                  (%%table-merge! public (jazz:get-public-lookup interface)))
                interfaces))
    
    ;; jazz:add-declaration-child does not set jazz:protected-access
    (let ((not-private (jazz:get-public-lookup class-declaration)))
      (%%vector-set! (jazz:get-namespace-declaration-lookups class-declaration) jazz:protected-access not-private))
    
    #;
    (let ((protected (jazz:get-protected-lookup class-declaration)))
      (if ascendant
          (%%table-merge! protected (jazz:get-public-lookup ascendant)))
      (for-each (lambda (interface)
                  (%%table-merge! protected (jazz:get-public-lookup interface)))
                interfaces))))


(jazz:define-method (jazz:emit-binding-reference (jazz:Class-Declaration declaration) source-declaration environment backend)
  (jazz:new-code
    (jazz:emit backend 'class-reference declaration)
    (or (jazz:get-category-declaration-metaclass declaration)
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
                 (iter (jazz:get-class-declaration-ascendant target-declaration)))))))))


(jazz:define-method (jazz:specifiable? (jazz:Class-Declaration declaration))
  #t)


(jazz:define-method (jazz:emit-declaration (jazz:Class-Declaration declaration) environment backend)
  (jazz:emit backend 'class declaration environment))


(define (jazz:emit-ascendant-access declaration environment backend)
  (let ((ascendant (jazz:get-class-declaration-ascendant declaration))
        (ascendant-relation (jazz:get-class-declaration-ascendant-relation declaration))
        (ascendant-base (jazz:get-class-declaration-ascendant-base declaration)))
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
           (iter (jazz:get-declaration-parent decl))))))


(set! jazz:object-declaration?
      (lambda (type)
        (and (%%class-is? type jazz:Class-Declaration)
             (%%not (jazz:get-class-declaration-ascendant type)))))


;;;
;;;; Validate
;;;


(define (jazz:validate-core-class core-class declaration)
  (define (validate-category)
    (define (validate-ascendant)
      (let* ((core-class-ascendant (%%get-class-ascendant core-class))
             (core-class-ascendant-name (if (%%not core-class-ascendant) '() (jazz:reference-name (%%get-category-identifier core-class-ascendant))))
             (declaration-ascendant (jazz:get-class-declaration-ascendant declaration))
             (declaration-ascendant-name (if (%%not declaration-ascendant) '() (jazz:reference-name (jazz:get-declaration-locator declaration-ascendant)))))
        (%%when (%%not (%%eq? core-class-ascendant-name declaration-ascendant-name))
          (jazz:error "Inconsistant core-class/class ascendant for {s}: {s} / {s}" (jazz:get-lexical-binding-name declaration) core-class-ascendant-name declaration-ascendant-name))))
    
    (define (validate-interfaces)
      (let ((declaration-interfaces (jazz:get-class-declaration-interfaces declaration)))
        (%%when (%%not (%%null? declaration-interfaces))
          (jazz:error "Interfaces are not supported in open classes: {s}" (jazz:get-lexical-binding-name declaration)))))
    
    (validate-ascendant)
    (validate-interfaces))
  
  (define (validate-slots)
    (define (collect-slots lst)
      (let ((queue (jazz:new-queue)))
        (define (process obj)
          (cond ((%%is? obj jazz:Slot-Declaration)
                 (jazz:enqueue queue obj))
                ((%%is? obj jazz:Begin)
                 (for-each process (jazz:get-begin-expressions obj)))))
        
        (for-each process lst)
        (jazz:queue-list queue)))
    
    (let ((core-class-slot-names (map (lambda (name/slot) (if (%%symbol? name/slot) name/slot (%%get-field-name name/slot))) (%%get-class-slots core-class)))
          (declaration-slot-names (map (lambda (decl) (jazz:get-lexical-binding-name decl)) (collect-slots (jazz:get-namespace-declaration-body declaration)))))
      (%%when (%%not (%%equal? core-class-slot-names declaration-slot-names))
        (jazz:error "Inconsistant core-class/class slots for {s}: {s} / {s}" (jazz:get-lexical-binding-name declaration) core-class-slot-names declaration-slot-names))))
  
  (validate-category)
  (validate-slots))


(jazz:define-method (jazz:outline-generate (jazz:Class-Declaration declaration) output)
  (let ((name (symbol->string (jazz:get-lexical-binding-name declaration)))
        (metaclass (jazz:get-category-declaration-metaclass declaration))
        (metaclass-explicit? (jazz:get-category-declaration-metaclass-explicit? declaration))
        (ascendant (jazz:get-class-declaration-ascendant declaration))
        (interfaces (jazz:get-class-declaration-interfaces declaration)))
    (%%unless (jazz:string-ends-with? name "~Class")
      (let ((name (jazz:get-lexical-binding-name declaration))
            (metaclass (and metaclass metaclass-explicit? (jazz:get-lexical-binding-name (jazz:resolve-binding metaclass))))
            (ascendant (and ascendant (jazz:get-lexical-binding-name (jazz:resolve-binding ascendant))))
            (interfaces (map jazz:get-lexical-binding-name (jazz:resolve-bindings interfaces))))
        (jazz:format output "{%}  (class {a}" name)
        (%%when metaclass
          (jazz:format output " metaclass {a}" metaclass))
        (%%when ascendant
          (let ((ascendant-name (%%symbol->string ascendant))
                (class-suffix "~Class"))
            (if (jazz:string-ends-with? ascendant-name class-suffix)
                (jazz:format output " extends (:class {a})" (substring ascendant-name 0 (- (string-length ascendant-name) (string-length class-suffix))))
              (jazz:format output " extends {a}" ascendant))))
        (%%when (%%not-null? interfaces)
          (if (%%null? (%%cdr interfaces))
              (jazz:format output " implements {a}" (%%car interfaces))
            (jazz:format output " implements {a}" interfaces))))
      (%%when (and metaclass (%%not metaclass-explicit?))
        (let ((children (jazz:outline-generate-filter-access (jazz:queue-list (jazz:get-namespace-declaration-children metaclass)))))
          (for-each (lambda (decl)
                      (let ((expr (jazz:outline-extract decl '(meta))))
                        (if expr
                            (jazz:format output "{%}    {s}" expr))))
                    children)))
      (let ((children (jazz:outline-generate-filter-access (jazz:queue-list (jazz:get-namespace-declaration-children declaration)))))
        (let ((accessor-names (%%make-table test: eq?)))
          (for-each (lambda (decl)
                      (%%when (%%is? decl jazz:Slot-Declaration)
                        (let ((getter-name (jazz:get-slot-declaration-getter-name decl))
                              (setter-name (jazz:get-slot-declaration-setter-name decl)))
                          (%%when getter-name
                            (%%table-set! accessor-names getter-name #t))
                          (%%when setter-name
                            (%%table-set! accessor-names setter-name #t)))))
                    children)
          (for-each (lambda (decl)
                      (%%unless (and (%%is? decl jazz:Method-Declaration)
                                     (%%table-ref accessor-names (jazz:get-lexical-binding-name decl) #f))
                        (let ((expr (jazz:outline-extract decl '())))
                          (if expr
                              (jazz:format output "{%}    {s}" expr)))))
                    children)))
      (jazz:format output ")"))))


;;;
;;;; Interface-Declaration
;;;


(jazz:define-class jazz:Interface-Declaration jazz:Category-Declaration (constructor: jazz:allocate-interface-declaration)
  ((ascendants getter: generate)))


(define (jazz:new-interface-declaration name type access compatibility attributes parent implementor metaclass metaclass-explicit? ascendants)
  (let ((new-declaration (jazz:allocate-interface-declaration name type #f access compatibility attributes #f parent #f #f #f (jazz:make-access-lookups jazz:protected-access) (jazz:new-queue) #f implementor metaclass metaclass-explicit? ascendants)))
    (jazz:setup-declaration new-declaration)
    new-declaration))


(define (jazz:setup-interface-lookups interface-declaration)
  (define (resolve-interface decl)
    (if decl
        (let ((interface-declaration (jazz:resolve-binding decl)))
          (%%assert (%%is? interface-declaration jazz:Interface-Declaration))
          interface-declaration)
      #f))
  
  (let ((ascendants (map resolve-interface (jazz:get-interface-declaration-ascendants interface-declaration))))
    
    (let ((private (jazz:get-private-lookup interface-declaration)))
      (for-each (lambda (interface)
                  (%%table-merge! private (jazz:get-public-lookup interface) #t))
                ascendants))
    
    ;; a quick test
    (let ((private (jazz:get-private-lookup interface-declaration)))
      (%%vector-set! (jazz:get-namespace-declaration-lookups interface-declaration) jazz:public-access private)
      (%%vector-set! (jazz:get-namespace-declaration-lookups interface-declaration) jazz:protected-access private))
    
    #;
    (let ((public (jazz:get-public-lookup interface-declaration)))
      (for-each (lambda (interface)
                  (%%table-merge! public (jazz:get-public-lookup interface)))
                ascendants))
    
    #;
    (let ((protected (jazz:get-protected-lookup interface-declaration)))
      (for-each (lambda (interface)
                  (%%table-merge! protected (jazz:get-public-lookup interface)))
                ascendants))))


(jazz:define-method (jazz:of-subtype? (jazz:Interface-Declaration declaration) subtype)
  ;; quicky to fill later on
  #f)


(jazz:define-method (jazz:specifiable? (jazz:Interface-Declaration declaration))
  #t)


(jazz:define-method (jazz:emit-declaration (jazz:Interface-Declaration declaration) environment backend)
  (jazz:emit backend 'interface declaration environment))


(jazz:define-method (jazz:outline-generate (jazz:Interface-Declaration declaration) output)
  (let ((metaclass (jazz:get-category-declaration-metaclass declaration))
        (metaclass-explicit? (jazz:get-category-declaration-metaclass-explicit? declaration))
        (ascendants (map jazz:get-lexical-binding-name (jazz:resolve-bindings (jazz:get-interface-declaration-ascendants declaration)))))
    (jazz:format output "{%}  (interface {a}" (jazz:get-lexical-binding-name declaration))
    (%%when (and metaclass metaclass-explicit?)
      (jazz:format output " metaclass {a}" (jazz:get-lexical-binding-name (jazz:resolve-binding metaclass))))
    (%%when (%%not-null? ascendants)
      (if (%%null? (%%cdr ascendants))
          (jazz:format output " extends {a}" (%%car ascendants))
        (jazz:format output " extends {a}" ascendants)))
    (%%when (and metaclass (%%not metaclass-explicit?))
      (let ((children (jazz:outline-generate-filter-access (jazz:queue-list (jazz:get-namespace-declaration-children metaclass)))))
        (for-each (lambda (decl)
                    (let ((expr (jazz:outline-extract decl '(meta))))
                      (if expr
                          (jazz:format output "{%}    {s}" expr))))
                  children)))
    (let ((children (jazz:outline-generate-filter-access (jazz:queue-list (jazz:get-namespace-declaration-children declaration)))))
      (for-each (lambda (decl)
                  (let ((expr (jazz:outline-extract decl '())))
                    (if expr
                        (jazz:format output "{%}    {s}" expr))))
                children))
    (jazz:format output ")")))


;;;
;;;; Field-Declaration
;;;


(jazz:define-class jazz:Field-Declaration jazz:Declaration ()
  ())


;;;
;;;; Slot-Declaration
;;;


(jazz:define-class jazz:Slot-Declaration jazz:Field-Declaration (constructor: jazz:allocate-slot-declaration)
  ((initialize  getter: generate setter: generate)
   (getter-name getter: generate)
   (setter-name getter: generate)))


(define (jazz:new-slot-declaration name type access compatibility attributes parent initialize getter-name setter-name)
  (let ((new-declaration (jazz:allocate-slot-declaration name type #f access compatibility attributes #f parent #f #f #f initialize getter-name setter-name)))
    (jazz:setup-declaration new-declaration)
    new-declaration))


(jazz:define-method (jazz:walk-binding-validate-call (jazz:Slot-Declaration declaration) walker resume source-declaration operator arguments form-src)
  #f)


(jazz:define-method (jazz:emit-declaration (jazz:Slot-Declaration declaration) environment backend)
  (let* ((class-declaration (jazz:get-declaration-parent declaration))
         (allocate? (%%neq? (jazz:get-lexical-binding-type declaration) jazz:Void))
         (core? (jazz:core-class? (jazz:get-lexical-binding-name class-declaration)))
         (initialize (jazz:get-slot-declaration-initialize declaration))
         (initialize? (and allocate? (%%not core?) initialize))
         (initialize-emit (and initialize? (jazz:emit-expression initialize declaration environment backend))))
    (jazz:emit backend 'slot declaration environment initialize-emit)))


(jazz:define-method (jazz:emit-binding-reference (jazz:Slot-Declaration declaration) source-declaration environment backend)
  (let ((self (jazz:*self*)))
    (if self
        (jazz:new-code
          (jazz:emit backend 'slot-reference declaration self)
          (jazz:find-annotated-type declaration environment)
          #f)
      (jazz:error "Illegal reference to a slot: {s}" (jazz:get-declaration-locator declaration)))))


(jazz:define-method (jazz:walk-binding-assignable? (jazz:Slot-Declaration declaration))
  #t)


(jazz:define-method (jazz:emit-binding-assignment (jazz:Slot-Declaration declaration) value source-declaration environment backend)
  (let ((self (jazz:*self*)))
    (if self
        (let ((value (jazz:emit-expression value source-declaration environment backend)))
          (jazz:new-code
            (jazz:emit backend 'slot-assignment declaration source-declaration environment self value)
            jazz:Any
            #f))
      (jazz:error "Illegal assignment to a slot: {s}" (jazz:get-declaration-locator declaration)))))


(jazz:define-method (jazz:outline-extract (jazz:Slot-Declaration declaration) meta)
  `(slot ,@meta
         ,@(jazz:outline-generate-access-list declaration)
         ,(jazz:get-lexical-binding-name declaration)
         ,@(jazz:outline-generate-type-list (jazz:get-lexical-binding-type declaration))
         ,@(jazz:outline-generate-accessors declaration)))


(define (jazz:outline-generate-accessors declaration)
  (let ((getter (jazz:get-slot-declaration-getter-name declaration))
        (setter (jazz:get-slot-declaration-setter-name declaration)))
    (cond ((and getter setter) '(accessors generate))
          (getter '(getter generate))
          (setter '(setter generate))
          (else '()))))


;;;
;;;; Property
;;;


(jazz:define-class jazz:Property-Declaration jazz:Slot-Declaration (constructor: jazz:allocate-property-declaration)
  ((getter getter: generate setter: generate)
   (setter getter: generate setter: generate)))


(define (jazz:new-property-declaration name type access compatibility attributes parent initialize getter-name setter-name)
  (let ((new-declaration (jazz:allocate-property-declaration name type #f access compatibility attributes #f parent #f #f #f initialize getter-name setter-name #f #f)))
    (jazz:setup-declaration new-declaration)
    new-declaration))


(jazz:define-method (jazz:emit-declaration (jazz:Property-Declaration declaration) environment backend)
  (let* ((class-declaration (jazz:get-declaration-parent declaration))
         (allocate? (%%neq? (jazz:get-lexical-binding-type declaration) jazz:Void))
         (core? (jazz:core-class? (jazz:get-lexical-binding-name class-declaration)))
         (initialize (jazz:get-slot-declaration-initialize declaration))
         (initialize? (and allocate? (%%not core?) initialize))
         (initialize-emit (and initialize? (jazz:emit-expression initialize declaration environment backend))))
    (jazz:emit backend 'property declaration environment initialize-emit)))


(define (jazz:expand-property walker resume declaration environment form-src)
  (jazz:expand-slot-form walker resume declaration form-src '%property))


(jazz:define-method (jazz:outline-extract (jazz:Property-Declaration declaration) meta)
  `(property ,@meta
             ,@(jazz:outline-generate-access-list declaration)
             ,(jazz:get-lexical-binding-name declaration)
             ,@(jazz:outline-generate-type-list (jazz:get-lexical-binding-type declaration))
             ,@(jazz:outline-generate-accessors declaration)))


;;;
;;;; Method-Declaration
;;;


(jazz:define-class jazz:Method-Declaration jazz:Field-Declaration (constructor: jazz:allocate-method-declaration)
  ((root         getter: generate)
   (propagation  getter: generate)
   (abstraction  getter: generate)
   (expansion    getter: generate)
   (remote       getter: generate)
   (synchronized getter: generate)
   (signature    getter: generate setter: generate)
   (body         getter: generate setter: generate)))


(define (jazz:new-method-declaration name type access compatibility attributes parent root propagation abstraction expansion remote synchronized signature)
  (let ((new-declaration (jazz:allocate-method-declaration name type #f access compatibility attributes #f parent #f #f #f root propagation abstraction expansion remote synchronized signature #f)))
    (jazz:setup-declaration new-declaration)
    new-declaration))


(define (jazz:method-dispatch-info declaration)
  (let ((root (jazz:get-method-declaration-root declaration))
        (propagation (jazz:get-method-declaration-propagation declaration)))
    (if (and (%%not root) (%%eq? propagation 'final))
        (values 'final declaration)
      (let ((root-method-declaration (jazz:get-method-declaration-root declaration)))
        (let ((method-declaration (or root-method-declaration declaration)))
          (let ((category-declaration (jazz:get-declaration-parent method-declaration)))
            (cond ((%%class-is? category-declaration jazz:Class-Declaration)
                   (values 'class method-declaration))
                  ((%%class-is? category-declaration jazz:Interface-Declaration)
                   (values 'interface method-declaration))
                  (else
                   (error "Invalid category declaration: {s}" category-declaration)))))))))


(define (jazz:primitive-category? category-declaration)
  (%%eq? (jazz:get-category-declaration-implementor category-declaration) 'primitive))


(define (jazz:emit-class-of category-declaration object-code)
  (if (jazz:primitive-category? category-declaration)
      `(jazz:class-of ,object-code)
    `(%%get-object-class ,object-code)))


(define (jazz:emit-method-dispatch object-argument object-code operator-src arguments arguments-codes declaration source-declaration environment backend)
  (let ((name (jazz:get-lexical-binding-name declaration)))
    (receive (dispatch-type method-declaration) (jazz:method-dispatch-info declaration)
      (let ((category-declaration (jazz:get-declaration-parent method-declaration)))
        (jazz:add-to-module-references source-declaration method-declaration)
        (let ((object-type (jazz:get-code-type object-code))
              (object-cast (jazz:emit-type-check object-code category-declaration source-declaration environment backend)))
          (jazz:new-code
            (case dispatch-type
              ((final)
               (let ((type (jazz:get-lexical-binding-type method-declaration))
                     (implementation-locator (jazz:get-declaration-locator method-declaration)))
                 (if (and jazz:debug-user?
                          arguments
                          ;; fail safe as this should never occur as inlined-call is before
                          (%%neq? (jazz:get-method-declaration-expansion method-declaration) 'inline)
                          ;; safe first iteration simplification
                          (jazz:only-positional-function-type? type)
                          (jazz:typed-function-type? type #t))
                     (let ((types (jazz:codes-types arguments-codes)))
                       (let ((mismatch (jazz:signature-mismatch (%%cons object-argument arguments) (%%cons object-type types) type)))
                         (if (or (%%not mismatch)
                                 (%%not (jazz:get-generate? 'check)))
                             (let ((locator (jazz:unsafe-locator implementation-locator)))
                               `(%%final-dispatch ,(jazz:emit-class-of category-declaration object-cast) ,locator))
                           (begin
                             (%%when (and (or (jazz:reporting?) (jazz:warnings?)) (jazz:get-warn? 'optimizations))
                               (let ((expression (and (%%pair? mismatch) (%%car mismatch))))
                                 (jazz:warning "Warning: In {a}{a}: Unmatched call to typed method {a}"
                                               (jazz:get-declaration-locator declaration)
                                               (jazz:present-expression-location (and expression (jazz:get-expression-source expression)) operator-src)
                                               (jazz:reference-name (jazz:get-declaration-locator declaration)))))
                             `(%%final-dispatch ,(jazz:emit-class-of category-declaration object-cast) ,implementation-locator)))))
                   `(%%final-dispatch ,(jazz:emit-class-of category-declaration object-cast) ,implementation-locator))))
              ((class)
               (let ((class-level-locator (%%compose-helper (jazz:get-declaration-locator category-declaration) 'level))
                     (method-rank-locator (%%compose-helper (jazz:get-declaration-locator method-declaration) 'rank)))
                 `(%%class-dispatch ,(jazz:emit-class-of category-declaration object-cast) ,class-level-locator ,method-rank-locator)))
              ((interface)
               (let ((interface-rank-locator (%%compose-helper (jazz:get-declaration-locator category-declaration) 'rank))
                     (method-rank-locator (%%compose-helper (jazz:get-declaration-locator method-declaration) 'rank)))
                 `(%%interface-dispatch ,(jazz:emit-class-of category-declaration object-cast) ,interface-rank-locator ,method-rank-locator))))
            (jazz:call-return-type (jazz:get-lexical-binding-type method-declaration))
            #f))))))


(jazz:define-method (jazz:compose-declaration-locator (jazz:Method-Declaration declaration))
  (if (%%eq? (jazz:get-method-declaration-abstraction declaration) 'core)
      (let ((class-name (%%compose-reference 'jazz (jazz:get-lexical-binding-name (jazz:get-declaration-parent declaration))))
            (name (%%compose-reference 'jazz (jazz:get-lexical-binding-name declaration))))
        (jazz:method-implementation-name class-name name))
    (nextmethod declaration)))


(jazz:define-method (jazz:emit-binding-reference (jazz:Method-Declaration declaration) source-declaration environment backend)
  (let ((name (jazz:get-lexical-binding-name declaration)))
    (jazz:emit backend 'dispatch-reference name #f declaration environment)))


(jazz:define-method (jazz:walk-binding-validate-call (jazz:Method-Declaration declaration) walker resume source-declaration operator arguments form-src)
  ;; GAZOUM the self test is a quick hack
  (if (%%null? arguments)
      (jazz:walk-error walker resume source-declaration form-src "Not enough arguments for: {a}" (jazz:get-lexical-binding-name declaration))
    (%%when (%%eq? (%%car arguments) 'self)
      (let ((signature (jazz:get-method-declaration-signature declaration)))
        (if signature
            (jazz:validate-arguments walker resume source-declaration declaration signature arguments form-src))))))


(jazz:define-method (jazz:emit-binding-call (jazz:Method-Declaration declaration) binding-src arguments arguments-codes source-declaration environment backend)
  (let ((object-argument (%%car arguments))
        (object-code (%%car arguments-codes))
        (others-arguments (%%cdr arguments))
        (others-codes (%%cdr arguments-codes)))
    (jazz:emit backend 'dispatch-call (jazz:get-lexical-binding-name declaration) binding-src source-declaration environment object-argument object-code others-arguments others-codes)))


(jazz:define-method (jazz:get-nextmethod-signature (jazz:Method-Declaration declaration))
  (define (lookup category-declaration method-name)
    (if (%%is? category-declaration jazz:Autoload-Declaration)
        (let ((category-declaration (jazz:get-autoload-declaration-declaration category-declaration)))
          (jazz:lookup-declaration category-declaration method-name jazz:private-access category-declaration))
      (jazz:lookup-declaration category-declaration method-name jazz:private-access category-declaration)))
  
  (define (get-next-method-declaration)
    (let ((method-name (jazz:get-lexical-binding-name declaration))
          (category-declaration (jazz:get-declaration-parent declaration)))
      (or (let ((ascendant (jazz:get-class-declaration-ascendant category-declaration)))
            (lookup ascendant method-name))
          (let iter ((scan (jazz:get-class-declaration-interfaces category-declaration)))
               (if (null? scan)
                   #f
                 (or (lookup (car scan) method-name)
                     (iter (cdr scan))))))))
  
  (let ((next-method-declaration (get-next-method-declaration)))
    (if next-method-declaration
        (jazz:get-method-declaration-signature next-method-declaration))))


(jazz:define-method (jazz:emit-declaration (jazz:Method-Declaration declaration) environment backend)
  (let ((signature (jazz:get-method-declaration-signature declaration)))
    (jazz:with-annotated-frame (jazz:annotate-signature signature)
      (lambda (frame)
        (let* ((augmented-environment (%%cons frame environment))
               (signature-emit (jazz:emit-signature signature declaration augmented-environment backend))
               (signature-casts (jazz:emit-signature-casts signature declaration augmented-environment backend))
               (body (jazz:get-method-declaration-body declaration))
               (body-type (let ((type (jazz:get-lexical-binding-type declaration)))
                            (if (%%is? type jazz:Function-Type) (jazz:get-function-type-result type) jazz:Any)))
               (body-emit (and body (let ((body-code (jazz:emit-expression body declaration augmented-environment backend)))
                                      (jazz:emit-type-check body-code body-type declaration augmented-environment backend))))
               (generate-unsafe? (and jazz:debug-user?
                                      ;; no need to emit unsafe and safe when inline because as the checks are not included
                                      ;; in the body it is possible to define the function itself with checks so it is safe
                                      ;; used not inlined and only insert checks at call site when the types don't match
                                      (%%neq? (jazz:get-method-declaration-expansion declaration) 'inline)
                                      ;; safe first iteration simplification
                                      (jazz:only-positional-signature? signature)
                                      (jazz:typed-signature? signature #t)))
               (unsafe-signature (and generate-unsafe? signature)))
          (jazz:emit backend 'method declaration environment signature-emit signature-casts body-emit unsafe-signature))))))


(jazz:define-method (jazz:tree-fold (jazz:Method-Declaration declaration) down up here seed environment)
  (here declaration seed environment)
  (jazz:tree-fold-list (jazz:get-signature-expressions (jazz:get-method-declaration-signature declaration)) down up here seed environment)
  (let ((body (jazz:get-method-declaration-body declaration)))
    (%%when body
      (jazz:tree-fold body down up here seed environment))))


(jazz:define-method (jazz:outline-extract (jazz:Method-Declaration declaration) meta)
  (define (generate-propagation-list)
    (let ((propagation (jazz:get-method-declaration-propagation declaration)))
      (list propagation)))
  
  `(method ,@meta
           ,(jazz:get-declaration-access declaration)
           ,@(generate-propagation-list)
           (,(jazz:get-lexical-binding-name declaration) ,@(jazz:outline-generate-signature (jazz:get-method-declaration-signature declaration) #t))
           ,@(jazz:outline-generate-type-list (jazz:get-function-type-result (jazz:get-lexical-binding-type declaration)))))


;;;
;;;; Dialect
;;;


(jazz:define-class jazz:Jazz-Dialect jazz:Dialect (constructor: jazz:allocate-jazz-dialect)
  ())


(define (jazz:new-jazz-dialect name)
  (jazz:allocate-jazz-dialect name (%%make-table test: eq?) (%%make-table test: eq?)))


(jazz:define-method (jazz:dialect-walker (jazz:Jazz-Dialect dialect))
  (jazz:new-jazz-walker))


;;;
;;;; Walker
;;;


(jazz:define-class jazz:Jazz-Walker jazz:Scheme-Walker (constructor: jazz:allocate-jazz-walker)
  ())


(define (jazz:new-jazz-walker)
  (jazz:allocate-jazz-walker #f #f '() '() '() (jazz:new-queue) (jazz:new-queue) (%%make-table test: eq?) (%%make-table test: eq?) '()))


(jazz:define-virtual-syntax (jazz:jazz-walker-supports-selfdot? (jazz:Jazz-Walker walker)))
(jazz:define-virtual-syntax (jazz:jazz-walker-supports-composite? (jazz:Jazz-Walker walker)))


(jazz:define-virtual (jazz:jazz-walker-supports-selfdot? (jazz:Jazz-Walker walker)))
(jazz:define-virtual (jazz:jazz-walker-supports-composite? (jazz:Jazz-Walker walker)))


(jazz:define-method (jazz:jazz-walker-supports-selfdot? (jazz:Jazz-Walker walker))
  #t)

(jazz:define-method (jazz:jazz-walker-supports-composite? (jazz:Jazz-Walker walker))
  #t)


(jazz:define-method (jazz:runtime-export (jazz:Jazz-Walker walker) declaration)
  (or (nextmethod walker declaration)
      (if (or (%%is? declaration jazz:Definition-Declaration)
              (%%is? declaration jazz:Generic-Declaration)
              (%%is? declaration jazz:Category-Declaration))
          (jazz:get-declaration-locator declaration)
        #f)))


;;;
;;;; Environment
;;;


(jazz:define-method (jazz:walker-declarations (jazz:Jazz-Walker walker))
  (cons (jazz:get-dialect-declarations (jazz:get-dialect 'jazz))
        (nextmethod walker)))


(jazz:define-method (jazz:walker-bindings (jazz:Jazz-Walker walker))
  (cons (jazz:get-dialect-bindings (jazz:get-dialect 'jazz))
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
    (jazz:emit backend 'specialized-class-of-call object)
    (let ((type (jazz:get-code-type object)))
      (if (%%class-is? type jazz:Class-Declaration)
          (jazz:get-category-declaration-metaclass type)
        jazz:Class-Declaration))
    #f))


;;;
;;;; New
;;;


(jazz:define-variable-override jazz:emit-new-call
  (lambda (operator locator arguments arguments-codes declaration environment backend)
    (jazz:emit backend 'new-call operator locator arguments arguments-codes declaration environment)))


;;;
;;;; Symbol
;;;


(jazz:define-method (jazz:walk-symbol (jazz:Jazz-Walker walker) resume declaration environment symbol-src)
  (let ((symbol (jazz:unwrap-syntactic-closure symbol-src)))
    (if (jazz:enumerator? symbol)
        (jazz:walk-enumerator walker symbol)
      (let ((name (jazz:extract-selfdot symbol)))
        (if name
            (let ((slot-declaration (jazz:lookup-declaration (jazz:find-class-declaration declaration) name jazz:private-access declaration)))
              (%%assertion (%%class-is? slot-declaration jazz:Slot-Declaration) (jazz:error "Slot expected: self.{s}" name)
                (jazz:new-binding-reference symbol-src slot-declaration)))
          (nextmethod walker resume declaration environment symbol-src))))))


(define (jazz:extract-selfdot symbol)
  (let ((str (%%symbol->string symbol)))
    (if (jazz:string-starts-with? str "self.")
        (%%string->symbol (%%substring str 5 (%%string-length str)))
      #f)))


(jazz:define-method (jazz:lookup-environment (jazz:Jazz-Walker walker) resume declaration environment symbol-src symbol)
  (define (lookup-composite walker environment symbol)
    (receive (module-name name) (jazz:break-reference symbol)
      (let ((exported-module-reference (jazz:outline-module module-name)))
        (let ((decl (jazz:lookup-declaration exported-module-reference name jazz:public-access declaration)))
          (if decl
              (let ((autoload-decl 
                      (if (%%is? decl jazz:Autoload-Declaration)
                          decl
                        (jazz:new-autoload-declaration name #f #f (jazz:get-declaration-toplevel declaration) (jazz:new-module-reference module-name #f)))))
                ;; manually add since lookup-declaration was not done on the local module
                (jazz:add-to-module-references declaration autoload-decl)
                autoload-decl)
            (jazz:walk-error walker resume declaration symbol-src "Unable to find {s} in unit {s}" name module-name))))))
  
    (if (and (jazz:composite-reference? symbol) (jazz:jazz-walker-supports-composite? walker))
        (lookup-composite walker environment symbol)
      (nextmethod walker resume declaration environment symbol-src symbol)))


(jazz:define-method (jazz:lookup-analyse (jazz:Jazz-Walker walker) declaration symbol-src referenced-declaration)
  (if (and (%%is? declaration jazz:Method-Declaration)
           (%%eq? (jazz:get-method-declaration-propagation declaration) 'final)
           (or (%%eq? (jazz:source-code symbol-src) 'self)
               (%%is? referenced-declaration jazz:Slot-Declaration)
               (%%is? referenced-declaration jazz:Method-Declaration)))
      (let ((data (jazz:get-analysis-data (jazz:get-declaration-locator declaration))))
        (jazz:set-analysis-data-declaration-references data (%%cons referenced-declaration (jazz:get-analysis-data-declaration-references data))))))


;;;
;;;; Assignment
;;;


(jazz:define-method (jazz:walk-symbol-assignment (jazz:Jazz-Walker walker) resume declaration environment symbol-src value)
  (let ((name (jazz:extract-selfdot (jazz:source-code symbol-src))))
    (if (and name (jazz:jazz-walker-supports-selfdot? walker))
        (let ((slot-declaration (jazz:lookup-declaration (jazz:find-class-declaration declaration) name jazz:private-access declaration)))
          (%%assertion (%%class-is? slot-declaration jazz:Slot-Declaration) (jazz:error "Slot expected: self.{s}" name)
            (jazz:new-assignment slot-declaration (jazz:walk walker resume declaration environment value) symbol-src)))
        (nextmethod walker resume declaration environment symbol-src value))))


;;;
;;;; With-Self
;;;


(jazz:define-class jazz:With-Self jazz:Expression (constructor: jazz:allocate-with-self)
  ((body getter: generate)))


(define (jazz:new-with-self type body)
  (jazz:allocate-with-self type #f body))


(jazz:define-method (jazz:emit-expression (jazz:With-Self expression) declaration environment backend)
  (let ((type (jazz:get-expression-type expression))
        (body (jazz:get-with-self-body expression)))
    (let ((body-emit (parameterize ((jazz:*self* (jazz:new-code 'self type #f)))
                       (jazz:emit-expression body declaration environment backend))))
      (jazz:new-code
        (jazz:emit backend 'with-self expression declaration environment body-emit)
        (jazz:get-code-type body-emit)
        #f))))


(jazz:define-method (jazz:tree-fold (jazz:With-Self expression) down up here seed environment)
  (let ((aug-env environment #; (cons 'self environment))
        (seed1 (down expression seed environment)))
    (up expression seed (jazz:tree-fold (jazz:get-with-self-body expression) down up here seed1 aug-env) environment)))


;;;
;;;; With-Dynamic-Self
;;;


(jazz:define-class jazz:With-Dynamic-Self jazz:Expression (constructor: jazz:allocate-with-dynamic-self)
  ((code getter: generate)
   (body getter: generate)))


(define (jazz:new-with-dynamic-self type code body)
  (jazz:allocate-with-dynamic-self type #f code body))


(jazz:define-method (jazz:emit-expression (jazz:With-Dynamic-Self expression) declaration environment backend)
  ;; should type be (jazz:get-expression-type expression) like for with-self
  (let ((type declaration)
        (code (jazz:get-with-dynamic-self-code expression))
        (body (jazz:get-with-dynamic-self-body expression)))
    (let ((body-emit (parameterize ((jazz:*self* (jazz:new-code code type #f)))
                       (jazz:emit-statements-code body declaration environment backend))))
      (jazz:new-code
        (jazz:emit backend 'with-dynamic-self expression declaration environment body-emit)
        (jazz:get-code-type body-emit)
        #f))))


(define (jazz:parse-with-dynamic-self form)
  (let ((code (%%car form))
        (body (%%cdr form)))
    (values code body)))


(define (jazz:walk-with-dynamic-self-declaration walker resume declaration environment form-src)
  (receive (code body) (jazz:parse-with-dynamic-self (%%cdr (jazz:source-code form-src)))
    (jazz:walk-declarations walker resume declaration environment body)))


(define (jazz:walk-with-dynamic-self walker resume declaration environment form-src)
  (receive (code body) (jazz:parse-with-dynamic-self (%%cdr (jazz:source-code form-src)))
    (let ((new-environment (%%cons (jazz:new-dynamic-self-binding #f code) environment)))
      (jazz:new-with-dynamic-self
        ;; should type be (jazz:find-class-declaration declaration) like for walk-with-self
        #f
        code
        (jazz:walk-list walker resume declaration new-environment body)))))


;;;
;;;; Cast
;;;


(jazz:define-class jazz:Cast jazz:Expression (constructor: jazz:allocate-cast)
  ((expression getter: generate)))


(define (jazz:new-cast type expression)
  (jazz:allocate-cast type #f expression))


(jazz:define-method (jazz:emit-expression (jazz:Cast expression) declaration environment backend)
  (let ((type (jazz:get-expression-type expression))
        (expression (jazz:get-cast-expression expression)))
    (let ((expression-emit (jazz:emit-expression expression declaration environment backend)))
      (jazz:new-code
        (jazz:emit backend 'cast expression declaration environment type expression-emit)
        type
        #f))))


(jazz:define-method (jazz:tree-fold (jazz:Cast expression) down up here seed environment)
  (up expression
      seed
      (jazz:tree-fold (jazz:get-cast-expression expression) down up here (down expression seed environment) environment)
      environment))


(define (jazz:walk-cast walker resume declaration environment form-src)
  (let ((form (jazz:source-code form-src)))
    (let ((specifier (jazz:source-code (%%cadr form)))
          (expression (%%car (%%cddr form))))
      (jazz:new-cast (jazz:walk-specifier walker resume declaration environment specifier)
                     (jazz:walk walker resume declaration environment expression)))))


;;;
;;;; Allege
;;;


(jazz:define-class jazz:Allege jazz:Expression (constructor: jazz:allocate-allege)
  ((test    getter: generate)
   (expr    getter: generate)
   (message getter: generate)))


(define (jazz:new-allege source test expr message)
  (jazz:allocate-allege #f source test expr message))


(jazz:define-method (jazz:emit-expression (jazz:Allege expression) declaration environment backend)
  (let ((test (jazz:get-allege-test expression))
        (expr (jazz:get-allege-expr expression))
        (message (jazz:get-allege-message expression)))
    (jazz:bind (yes-environment . no-environment) (jazz:branch-types test environment)
      (let ((expr (jazz:emit-expression expr declaration yes-environment backend)))
        (if jazz:debug-user?
            (let ((test (jazz:emit-expression test declaration environment backend)))
              (jazz:new-code
                `(if ,(jazz:sourcified-form test)
                     ,(jazz:sourcified-form expr)
                   (error ,message))
                (jazz:get-code-type expr)
                (jazz:get-expression-source expression)))
          (jazz:new-code
            (jazz:sourcified-form expr)
            (jazz:get-code-type expr)
            (jazz:get-expression-source expression)))))))


(jazz:define-method (jazz:tree-fold (jazz:Allege expression) down up here seed environment)
  (up expression
      seed
      (jazz:tree-fold
        (jazz:get-allege-expr expression) down up here
        (jazz:tree-fold
          (jazz:get-allege-test expression) down up here (down expression seed environment) environment)
        environment)
      environment))


(define (jazz:walk-%allege walker resume declaration environment form-src)
  (let ((form (jazz:source-code form-src)))
    (let ((test (%%cadr form))
          (expr (%%car (%%cddr form))))
      (let ((message (and jazz:debug-user? (let ((port (open-output-string)))
                                             (display "Allege " port)
                                             (write (jazz:desourcify-all test) port)
                                             (display " failed" port)
                                             (get-output-string port)))))
        (jazz:new-allege form-src
                         (jazz:walk walker resume declaration environment test)
                         (jazz:walk walker resume declaration environment expr)
                         message)))))


;;;
;;;; Allocate
;;;


(jazz:define-class jazz:Allocate jazz:Expression (constructor: jazz:allocate-allocate)
  ((class  getter: generate)
   (values getter: generate)))


(define (jazz:new-allocate class values)
  (jazz:allocate-allocate #f #f class values))


(jazz:define-method (jazz:emit-expression (jazz:Allocate expression) declaration environment backend)
  (let ((class (jazz:get-allocate-class expression))
        (values (jazz:get-allocate-values expression)))
    (let ((class-emit (jazz:emit-expression class declaration environment backend))
          (values-emit (jazz:emit-expressions values declaration environment backend)))
      (jazz:new-code
        (jazz:emit backend 'allocate expression declaration environment class-emit values-emit)
        jazz:Any
        #f))))


(jazz:define-method (jazz:tree-fold (jazz:Allocate expression) down up here seed environment)
  (let ((class (jazz:get-allocate-class expression))
        (values (jazz:get-allocate-values expression)))
    (up expression
        seed
        (jazz:tree-fold-list
          (%%cons class values) down up here
          (down expression seed environment)
          environment)
        environment)))


(define (jazz:walk-allocate walker resume declaration environment form-src)
  (let ((form (%%cdr (jazz:source-code form-src))))
    (let ((class (%%car form))
          (values (%%cdr form)))
      (jazz:new-allocate (jazz:walk walker resume declaration environment class)
                         (jazz:walk-list walker resume declaration environment values)))))


;;;
;;;; Static
;;;


(jazz:define-class jazz:Static jazz:Expression (constructor: jazz:allocate-static)
  ((expression getter: generate)))


(define (jazz:new-static expression)
  (jazz:allocate-static #f #f expression))


(jazz:define-method (jazz:emit-expression (jazz:Static expression) declaration environment backend)
  (let ((expr (jazz:get-static-expression expression)))
    (let ((static (jazz:register-static declaration "static" expr)))
      (let ((code (jazz:emit-expression (%%cdr static) declaration environment backend)))
        (jazz:new-code
          (jazz:emit backend 'static expression declaration environment static)
          (jazz:get-code-type code)
          #f)))))


(jazz:define-method (jazz:tree-fold (jazz:Static expression) down up here seed environment)
  (up expression
      seed
      (jazz:tree-fold (jazz:get-static-expression expression) down up here (down expression seed environment) environment)
      environment))


(define (jazz:walk-static walker resume declaration environment form-src)
  (let ((form (jazz:source-code form-src)))
    (let ((expression (jazz:walk walker resume declaration environment (%%cadr form))))
      (jazz:new-static expression))))


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


;;;
;;;; Hub
;;;


(define jazz:hub-declarations
  (%%make-table test: eq?))


(define (jazz:find-hub-declaration name)
  (%%table-ref jazz:hub-declarations name #f))


(define (jazz:register-hub-declaration hub-declaration)
  (%%table-set! jazz:hub-declarations (jazz:get-lexical-binding-name hub-declaration) hub-declaration))


(jazz:define-class jazz:Hub-Declaration jazz:Declaration (constructor: jazz:allocate-hub-declaration)
  ((nodes getter: generate setter: generate)))


(define (jazz:new-hub-declaration name type access compatibility attributes parent)
  (let ((new-declaration (jazz:allocate-hub-declaration name type #f access compatibility attributes #f parent #f #f #f '())))
    (jazz:setup-declaration new-declaration)
    new-declaration))


(define (jazz:add-hub-declaration-node hub-declaration node)
  (jazz:set-hub-declaration-nodes hub-declaration (%%cons node (jazz:get-hub-declaration-nodes hub-declaration))))


(jazz:define-method (jazz:emit-declaration (jazz:Hub-Declaration declaration) environment backend)
  (jazz:sourcify-deep-if
    `(begin)
    (jazz:get-declaration-source declaration)))


(jazz:define-method (jazz:walk-binding-validate-call (jazz:Hub-Declaration declaration) walker resume source-declaration operator arguments form-src)
  #f)


(jazz:define-method (jazz:emit-binding-reference (jazz:Hub-Declaration declaration) source-declaration environment backend)
  (let ((name (jazz:get-lexical-binding-name declaration)))
    (jazz:emit backend 'dispatch-reference name #f declaration environment)))


(jazz:define-method (jazz:emit-binding-call (jazz:Hub-Declaration declaration) binding-src arguments arguments-codes source-declaration environment backend)
  (if (%%null? arguments)
      (jazz:error "Ill-formed hub call: ({a})" (jazz:get-lexical-binding-name declaration))
    (let ((object-argument (%%car arguments))
          (object-code (%%car arguments-codes))
          (others-arguments (%%cdr arguments))
          (others-codes (%%cdr arguments-codes)))
      (jazz:emit backend 'dispatch-call (jazz:get-lexical-binding-name declaration) binding-src source-declaration environment object-argument object-code others-arguments others-codes))))


(jazz:define-method (jazz:outline-extract (jazz:Hub-Declaration declaration) meta)
  #f)


(define (jazz:parse-hub walker resume declaration rest)
  (jazz:bind (name) rest
    (let ((name (jazz:source-code name)))
      (values name jazz:Any 'public 'uptodate))))


(define (jazz:walk-hub-declaration walker resume declaration environment form-src)
  (receive (name type access compatibility) (jazz:parse-hub walker resume declaration (%%cdr (jazz:source-code form-src)))
    (let ((new-declaration (or (jazz:find-hub-declaration name)
                               (let ((hub-declaration (jazz:new-hub-declaration name type access compatibility '() declaration)))
                                 (jazz:register-hub-declaration hub-declaration)
                                 hub-declaration))))
      (jazz:set-declaration-source new-declaration form-src)
      (let ((effective-declaration (jazz:add-declaration-child walker resume declaration new-declaration)))
        effective-declaration))))


(define (jazz:walk-hub walker resume declaration environment form-src)
  (receive (name type access compatibility) (jazz:parse-hub walker resume declaration (%%cdr (jazz:source-code form-src)))
    (let ((new-declaration (jazz:require-declaration declaration name)))
      (jazz:set-declaration-source new-declaration form-src)
      new-declaration)))


(jazz:define-variable-override jazz:manifest-ignore?
  (lambda (obj)
    (%%is? obj jazz:Hub-Declaration)))


;;;
;;;; Node
;;;


(define (jazz:expand-node walker resume declaration environment . rest)
  (let ((signature (%%car rest))
        (body (%%cdr rest)))
    (let ((name (%%car signature))
          (parameters (%%cdr signature)))
      `(begin
         (hub ,name)
         (%node ,name ,parameters ,@body)))))


(jazz:define-class jazz:Node-Declaration jazz:Declaration (constructor: jazz:allocate-node-declaration)
  ((hub-name  getter: generate)
   (signature getter: generate)))


(define (jazz:new-node-declaration name type access compatibility attributes parent hub-name signature)
  (let ((new-declaration (jazz:allocate-node-declaration name type #f access compatibility attributes #f parent #f #f #f hub-name signature)))
    (jazz:setup-declaration new-declaration)
    new-declaration))


(jazz:define-method (jazz:emit-declaration (jazz:Node-Declaration declaration) environment backend)
  (let ((class-reference (jazz:get-dynamic-parameter-class (%%car (jazz:get-signature-positional (jazz:get-node-declaration-signature declaration))))))
    (jazz:sourcify-deep-if
      `(begin)
      #; ;; FOR NOW THE HUB OBJECT IS NOT NEEDED
      `(jazz:register-hub-node ',(jazz:get-node-declaration-hub-name declaration) ,(jazz:sourcified-form (jazz:emit-expression class-reference declaration environment backend)))
      (jazz:get-declaration-source declaration))))


(jazz:define-method (jazz:outline-extract (jazz:Node-Declaration declaration) meta)
  #f)


(define (jazz:parse-node walker resume declaration rest)
  (jazz:bind (name parameters . body) rest
    (let ((node-name (%%string->symbol (%%string-append (%%symbol->string name) "$" (%%symbol->string (%%caar parameters))))))
      (values name node-name jazz:Any 'public 'uptodate parameters body))))


(define (jazz:walk-node-declaration walker resume declaration environment form-src)
  (receive (name node-name type access compatibility parameters body) (jazz:parse-node walker resume declaration (%%cdr (jazz:source-code form-src)))
    (receive (signature augmented-environment) (jazz:walk-parameters walker resume declaration environment parameters #t #t)
      (let ((new-declaration (or (jazz:find-declaration-child declaration node-name)
                                 (jazz:new-node-declaration node-name type access compatibility '() declaration name signature))))
        (jazz:add-hub-declaration-node (jazz:find-hub-declaration name) new-declaration)
        (jazz:set-declaration-source new-declaration form-src)
        (let ((effective-declaration (jazz:add-declaration-child walker resume declaration new-declaration)))
          effective-declaration)))))


(define (jazz:walk-node walker resume declaration environment form-src)
  (receive (name node-name type access compatibility parameters body) (jazz:parse-node walker resume declaration (%%cdr (jazz:source-code form-src)))
    (let ((new-declaration (jazz:require-declaration declaration node-name)))
      (jazz:set-declaration-source new-declaration form-src)
      new-declaration)))


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
        (let ((name (jazz:unwrap-syntactic-closure (%%car rest)))
              (name-src (%%car rest)))
          (jazz:parse-specifier (%%cdr rest)
            (lambda (specifier rest)
              (values name name-src specifier access compatibility expansion (if (%%null? rest) (%%list 'unspecified) (%%car rest)) #f))))
      (let* ((name (jazz:source-code (%%car (jazz:unwrap-syntactic-closure (%%car rest)))))
             (name-src (%%car (jazz:unwrap-syntactic-closure (%%car rest))))
             (parameters (%%cdr (jazz:source-code (%%car rest)))))
        (jazz:parse-specifier (%%cdr rest)
          (lambda (specifier body)
            (let ((effective-body (if (%%null? body) (%%list (%%list 'unspecified)) body))
                  (specifier-list (if specifier (%%list specifier) '())))
              (let ((value
                     `(lambda ,parameters ,@specifier-list
                        ,@effective-body)))
                (values name name-src specifier access compatibility expansion value parameters)))))))))


(define (jazz:walk-extended-definition-declaration walker resume declaration environment form-src new-definition-declaration)
  (receive (name name-src specifier access compatibility expansion value parameters) (jazz:parse-definition walker resume declaration (%%cdr (jazz:source-code form-src)))
    (%%assertion (%%class-is? declaration jazz:Namespace-Declaration) (jazz:walk-error walker resume declaration form-src "Definitions can only be defined inside namespaces: {s}" name)
      (let ((type (jazz:specifier->type walker resume declaration environment specifier)))
        (let ((signature (and parameters (jazz:walk-parameters walker resume declaration environment parameters #t #f))))
          (let ((effective-type (if signature (jazz:signature->function-type signature type) type)))
            (let ((new-declaration (or (jazz:find-declaration-child-of-type jazz:Definition-Declaration declaration name)
                                       (new-definition-declaration name effective-type access compatibility '() declaration expansion signature))))
              (jazz:set-declaration-source new-declaration form-src)
              (jazz:set-declaration-name-source new-declaration name-src)
              (let ((effective-declaration (jazz:add-declaration-child walker resume declaration new-declaration)))
                (%%when (%%eq? expansion 'inline)
                  (let ((new-environment (%%cons effective-declaration environment)))
                    (jazz:set-definition-declaration-signature effective-declaration signature)
                    (jazz:set-definition-declaration-value effective-declaration
                                                           (jazz:walk walker resume effective-declaration new-environment value))))
                effective-declaration))))))))


(define (jazz:walk-extended-definition walker resume declaration environment form-src)
  (receive (name name-src specifier access compatibility expansion value parameters) (jazz:parse-definition walker resume declaration (%%cdr (jazz:source-code form-src)))
    (%%assertion (%%class-is? declaration jazz:Namespace-Declaration) (jazz:walk-error walker resume declaration form-src "Definitions can only be defined inside namespaces: {s}" name)
      (let ((new-declaration (jazz:require-declaration-of-type jazz:Definition-Declaration declaration name)))
        (%%when (%%neq? expansion 'inline)
          #; ;; wait buggy if file is both loaded interpreted and compiled
          (%%when (and (%%neq? (jazz:walk-for) 'eval) (jazz:get-definition-declaration-value new-declaration))
            (jazz:walk-error walker resume declaration form-src "Cannot redefine definition: {s}" name))
          ;; adding source information for parameters (default for optional and keyword may be source code)
          ;; jazz:find-annotated fails on first keyword at jazz.language.runtime.functional:minimum
          ;; because (eq? variable annotated-variable) -> one points to a stale value
          (let ((new-environment (if #f #; parameters
                                     (receive (signature augmented-environment) (jazz:walk-parameters walker resume declaration environment parameters #t #t)
                                       (jazz:set-definition-declaration-signature new-declaration signature)
                                       (%%cons new-declaration augmented-environment))
                                     (%%cons new-declaration environment))))
            (jazz:set-definition-declaration-value new-declaration (jazz:walk walker resume new-declaration new-environment value))))
        (jazz:set-declaration-source new-declaration form-src)
        (jazz:set-declaration-name-source new-declaration name-src)
        new-declaration))))


(define (jazz:walk-definition-declaration walker resume declaration environment form-src)
  (jazz:walk-extended-definition-declaration walker resume declaration environment form-src
    jazz:new-definition-declaration))


(define (jazz:walk-definition walker resume declaration environment form-src)
  (jazz:walk-extended-definition walker resume declaration environment form-src))


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
            (name-src (%%car signature))
            (parameters (%%cdr signature)))
        (jazz:parse-specifier (%%cdr rest)
          (lambda (specifier body)
            (values name name-src specifier access compatibility parameters body)))))))


(define (jazz:walk-generic-declaration walker resume declaration environment form-src)
  (receive (name name-src specifier access compatibility parameters body) (jazz:parse-generic walker resume declaration (%%cdr (jazz:source-code form-src)))
    (%%assertion (%%class-is? declaration jazz:Module-Declaration) (jazz:walk-error walker resume declaration form-src "Generics can only be defined at the module level: {s}" name)
      (let ((type (if specifier (jazz:walk-specifier walker resume declaration environment specifier) jazz:Any))
            (dispatch-type-declarations (map (lambda (dynamic-parameter-type)
                                               (jazz:lookup-reference walker resume declaration environment dynamic-parameter-type))
                                             (jazz:dynamic-parameter-types parameters)))
            (signature (jazz:walk-parameters walker resume declaration environment parameters #t #f)))
        (let ((new-declaration (jazz:new-generic-declaration name type access compatibility '() declaration dispatch-type-declarations signature)))
          (jazz:set-declaration-source new-declaration form-src)
          (jazz:set-declaration-name-source new-declaration name-src)
          (let ((effective-declaration (jazz:add-declaration-child walker resume declaration new-declaration)))
            effective-declaration))))))


(define (jazz:walk-generic walker resume declaration environment form-src)
  (receive (name name-src specifier access compatibility parameters body) (jazz:parse-generic walker resume declaration (%%cdr (jazz:source-code form-src)))
    (%%assertion (%%class-is? declaration jazz:Module-Declaration) (jazz:walk-error walker resume declaration form-src "Generics can only be defined at the module level: {s}" name)
      (receive (signature augmented-environment) (jazz:walk-parameters walker resume declaration environment parameters #t #t)
        (let ((new-declaration (jazz:require-declaration declaration name)))
          (jazz:set-generic-declaration-signature new-declaration signature)
          (jazz:set-generic-declaration-body new-declaration
                                          (jazz:walk-body walker resume new-declaration augmented-environment body))
          (jazz:set-declaration-source new-declaration form-src)
          (jazz:set-declaration-name-source new-declaration name-src)
          new-declaration)))))


;;;
;;;; Specific
;;;


(define jazz:specific-modifiers
  '())


(define (jazz:parse-specific walker resume declaration rest)
  (receive (rest) (jazz:parse-modifiers walker resume declaration jazz:specific-modifiers rest)
    (let* ((signature (jazz:source-code (%%car rest)))
           (name (jazz:source-code (%%car signature)))
           (name-src (%%car signature))
           (parameters (%%cdr signature))
           (body (%%cdr rest))
           (effective-body (if (%%null? body) (%%list (%%list 'unspecified)) body)))
      (values name name-src parameters effective-body))))


(define (jazz:walk-specific walker resume declaration environment form-src)
  (define (root-dynamic-parameters? generic-declaration specific-signature name parameters)
    (let iter ((generic-parameters (jazz:get-signature-positional (jazz:get-generic-declaration-signature generic-declaration)))
               (specific-parameters (jazz:get-signature-positional specific-signature))
               (root? #t))
      (let ((generic-parameter (and (%%pair? generic-parameters) (%%car generic-parameters)))
            (specific-parameter (and (%%pair? specific-parameters) (%%car specific-parameters))))
        (let ((generic-dynamic? (%%is? generic-parameter jazz:Dynamic-Parameter))
              (specific-dynamic? (%%is? specific-parameter jazz:Dynamic-Parameter)))
          (cond ((and generic-dynamic? specific-dynamic?)
                 (let ((generic-class (jazz:resolve-binding (jazz:get-binding-reference-binding (jazz:get-dynamic-parameter-class generic-parameter))))
                       (specific-class (jazz:resolve-binding (jazz:get-binding-reference-binding (jazz:get-dynamic-parameter-class specific-parameter)))))
                   (if (jazz:of-subtype? generic-class specific-class)
                       (iter (%%cdr generic-parameters)
                             (%%cdr specific-parameters)
                             (%%eq? generic-class specific-class))
                     (jazz:walk-error walker resume declaration form-src "Dynamic parameter {a} is not a subtype of {a}: {s}"
                       (jazz:get-lexical-binding-name specific-parameter)
                       (jazz:get-declaration-locator generic-class)
                       (%%cons name parameters)))))
                ((or generic-dynamic? specific-dynamic?)
                 (jazz:walk-error walker resume declaration form-src "Specific must dispatch on the same number of dynamic parameters: {s}" (%%cons name parameters)))
                (else
                 root?))))))
  
  (receive (name name-src parameters body) (jazz:parse-specific walker resume declaration (%%cdr (jazz:source-code form-src)))
    (%%assertion (%%class-is? declaration jazz:Module-Declaration) (jazz:walk-error walker resume declaration form-src "Specifics can only be defined at the module level: {s}" name)
      (let ((generic-declaration (jazz:lookup-declaration declaration name jazz:private-access declaration)))
        (if (%%class-is? generic-declaration jazz:Generic-Declaration)
            (receive (signature augmented-environment) (jazz:walk-parameters walker resume declaration environment parameters #t #t)
              (let* ((root? (root-dynamic-parameters? generic-declaration signature name parameters))
                     (new-declaration (jazz:new-specific-declaration name #f 'public 'uptodate '() declaration generic-declaration signature root?))
                     (body-environment (if root? augmented-environment (%%cons (jazz:new-nextmethod-variable 'nextmethod #f #f) augmented-environment))))
                (jazz:set-specific-declaration-body new-declaration
                                                 (jazz:walk-body walker resume new-declaration body-environment body))
                (jazz:set-declaration-source new-declaration form-src)
                (jazz:set-declaration-name-source new-declaration name-src)
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
  (define (preprocess-meta name body)
    (let ((metaclass (jazz:new-queue))
          (class (jazz:new-queue))
          (nodes (jazz:new-queue)))
      (define (preprocess expr)
        (let ((expr (parameterize ((jazz:current-declaration-name name))
                      (jazz:expand-macros walker resume declaration environment expr))))
          (cond ((and (%%pair? (jazz:source-code expr))
                      (%%eq? (jazz:source-code (%%car (jazz:source-code expr))) 'begin))
                 (for-each preprocess (%%cdr (jazz:source-code expr))))
                ((and (%%pair? (jazz:source-code expr))
                      (%%pair? (%%cdr (jazz:source-code expr)))
                      (%%eq? (jazz:source-code (%%cadr (jazz:source-code expr))) 'meta))
                 (jazz:enqueue metaclass (jazz:sourcify-deep-if (%%cons (%%car (jazz:source-code expr)) (%%cddr (jazz:source-code expr))) expr)))
                ;; first draft lets not worry about meta public methods generating nodes
                ((and (%%pair? (jazz:source-code expr))
                      (%%eq? (jazz:source-code (%%car (jazz:source-code expr))) 'method)
                      (%%pair? (%%cdr (jazz:source-code expr)))
                      (or 
                        (%%eq? (jazz:source-code (%%cadr (jazz:source-code expr))) 'public)
                        (%%eq? (jazz:source-code (%%cadr (jazz:source-code expr))) 'package)))
                 (jazz:enqueue class expr)
                 ;; is there a function to just skip the modifiers?
                 (receive (access compatibility propagation abstraction expansion remote synchronized rest) (jazz:parse-modifiers walker resume declaration jazz:method-modifiers (%%cdr (jazz:source-code expr)))
                   (let ((signature (jazz:source-code (%%car rest))))
                     (let ((method-name (jazz:source-code (%%car signature)))
                           (parameters (%%cdr signature)))
                         ;; quicky for tests as parameters are not needed at the moment and break some code in jazz.locale
                         (set! parameters '())
                         (jazz:enqueue nodes `(node (,method-name (,(jazz:name->specifier name) self) ,@parameters)))))))
                (else
                 (jazz:enqueue class expr)))))
      
      (for-each preprocess body)
      (values (jazz:queue-list metaclass)
              (jazz:queue-list class)
              (jazz:queue-list nodes))))
  
  (receive (name type access abstraction compatibility implementor metaclass-name ascendant-name interface-names attributes body) (jazz:parse-class walker resume declaration (%%cdr (jazz:source-code form-src)))
    (receive (metaclass-body class-body nodes) (preprocess-meta name body)
      (cond ((and (%%not-null? metaclass-body)
                  (%%specified? metaclass-name))
             (jazz:walk-error walker resume declaration form-src "Ambiguous use of both metaclass and meta keywords: {s}" name))
            ((or (%%specified? metaclass-name)
                 (jazz:core-class? name)
                 (%%unspecified? ascendant-name))
             (jazz:sourcify-deep-if
               `(begin
                  (%class ,@(%%cdr (jazz:source-code form-src)))
                  ,@(map (lambda (node)
                           (jazz:sourcify-deep-if
                             node
                             form-src))
                         nodes))
               form-src))
            (else
             (let ((metaclass-name (%%string->symbol (%%string-append (%%symbol->string name) "~Class"))))
               `(begin
                  ,(jazz:sourcify-deep-if
                     `(%class ,metaclass-name extends (:class ,ascendant-name)
                        ,@metaclass-body)
                     form-src)
                  ,(jazz:sourcify-deep-if
                     `(%class ,access ,abstraction ,compatibility ,implementor ,name metaclass (:generated ,metaclass-name) extends ,ascendant-name implements ,interface-names
                        ,@class-body)
                     form-src)
                  ,@(map (lambda (node)
                           (jazz:sourcify-deep-if
                             node
                             form-src))
                         nodes))))))))


(define (jazz:walk-%class-declaration walker resume declaration environment form-src)
  (define (lookup-metaclass walker resume declaration environment ascendant metaclass-name)
    (cond ((or (jazz:unspecified? metaclass-name) (%%eq? metaclass-name 'Object-Class))
           (values #f #f))
          ((%%symbol? metaclass-name)
           (values (jazz:lookup-reference walker resume declaration environment metaclass-name) #t))
          (else
           (values (jazz:lookup-reference walker resume declaration environment (%%cadr metaclass-name)) #f))))
  
  (receive (name type access abstraction compatibility implementor metaclass-name ascendant-name interface-names attributes body) (jazz:parse-class walker resume declaration (%%cdr (jazz:source-code form-src)))
    (%%assertion (%%class-is? declaration jazz:Module-Declaration) (jazz:walk-error walker resume declaration form-src "Classes can only be defined at the module level: {s}" name)
      ;; explicit test on Object-Class is to break circularity
      (receive (ascendant ascendant-relation ascendant-base) (jazz:lookup-ascendant walker resume declaration environment ascendant-name)
        (receive (metaclass metaclass-explicit?) (lookup-metaclass walker resume declaration environment ascendant metaclass-name)
          (let ((interfaces (if (jazz:unspecified? interface-names) '() (map (lambda (interface-name) (jazz:lookup-reference walker resume declaration environment interface-name)) (jazz:listify interface-names)))))
            (let ((new-declaration (or (jazz:find-declaration-child declaration name)
                                       (jazz:new-class-declaration name type access compatibility attributes declaration implementor metaclass metaclass-explicit? ascendant ascendant-relation ascendant-base interfaces))))
              (jazz:set-declaration-source new-declaration form-src)
              (let ((effective-declaration (jazz:add-declaration-child walker resume declaration new-declaration)))
                (jazz:setup-class-lookups effective-declaration)
                (let ((new-environment (%%cons effective-declaration environment)))
                  (jazz:walk-declarations walker resume effective-declaration new-environment body)
                  effective-declaration)))))))))


(define (jazz:walk-%class walker resume declaration environment form-src)
  (receive (name type access abstraction compatibility implementor metaclass-name ascendant-name interface-names attributes body) (jazz:parse-class walker resume declaration (%%cdr (jazz:source-code form-src)))
    (%%assertion (%%class-is? declaration jazz:Module-Declaration) (jazz:walk-error walker resume declaration form-src "Classes can only be defined at the module level: {s}" name)
      (let* ((new-declaration (jazz:require-declaration declaration name))
             (new-environment (%%cons new-declaration environment))
             (ascendant-declaration (jazz:get-class-declaration-ascendant new-declaration)))
        (if (and (%%not ascendant-declaration) (%%neq? name 'Object))
            (jazz:walk-error walker resume declaration form-src "Class {s} does not specify an ascendant" name)
          (begin
            (jazz:set-namespace-declaration-body new-declaration (jazz:walk-namespace walker resume new-declaration new-environment body))
            (jazz:set-declaration-source new-declaration form-src)
            new-declaration))))))


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
      (or (jazz:get-category-declaration-metaclass class-declaration)
          (let ((ascendant (jazz:get-class-declaration-ascendant class-declaration)))
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
                                     (jazz:new-interface-declaration name type access compatibility attributes declaration implementor metaclass (%%boolean metaclass) ascendants))))
            (jazz:set-declaration-source new-declaration form-src)
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
          (jazz:set-namespace-declaration-body new-declaration (jazz:walk-namespace walker resume new-declaration new-environment body))
          (jazz:set-declaration-source new-declaration form-src)
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


(define (jazz:expand-doc walker resume declaration environment form-src)
  #f)


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
              (let ((name-self (%%string->symbol (%%string-append "self." (%%symbol->string name))))
                    (generate-getter? (%%eq? getter-generation 'generate))
                    (generate-setter? (%%eq? setter-generation 'generate))
                    (specifier-list (if specifier (%%list specifier) '())))
                `(begin
                   ,(jazz:sourcify-deep-if
                      `(,symbol ,name ,specifier ,access ,compatibility ,(if (%%unspecified? initialize) initialize `(with-self ,initialize)) ,getter-name ,setter-name)
                      form-src)
                   ,@(if generate-getter?
                         `((method ,(or getter-access 'public) ,getter-propagation ,getter-abstraction ,getter-expansion (,getter-name self) ,@specifier-list
                             ,name-self))
                       '())
                   ,@(if generate-setter?
                         `((method ,(or setter-access 'protected) ,setter-propagation ,setter-abstraction ,setter-expansion (,setter-name self value ,@specifier-list) <void>
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
            (jazz:set-declaration-source new-declaration form-src)
            (let ((effective-declaration (jazz:add-declaration-child walker resume declaration new-declaration)))
              effective-declaration)))))))


(define (jazz:walk-%slot walker resume declaration environment form-src)
  (let ((form (%%desourcify form-src)))
    (jazz:bind (name specifier access compatibility initialize getter-name setter-name) (%%cdr form)
      (%%assertion (%%class-is? declaration jazz:Class-Declaration) (jazz:walk-error walker resume declaration form-src "Slots can only be defined inside classes: {s}" name)
        (let ((new-declaration (jazz:require-declaration declaration (%%cadr form))))
          #; ;; wait buggy if file is both loaded interpreted and compiled
          (let ((ascendant-declaration (let ((ascendant-declaration (jazz:get-class-declaration-ascendant declaration)))
                                         (if (%%is? ascendant-declaration jazz:Autoload-Declaration)
                                             (jazz:get-autoload-declaration-declaration ascendant-declaration)
                                           ascendant-declaration))))
            (let ((duplicate-declaration (jazz:lookup-declaration ascendant-declaration name jazz:private-access declaration)))
              (%%when (and duplicate-declaration
                           ;; remove this test when methods duplicating slots are fixed
                           (%%is? duplicate-declaration jazz:Slot-Declaration))
                (jazz:walk-error walker resume declaration form-src "Cannot override slot: {s}" name))))
          (jazz:set-slot-declaration-initialize new-declaration
            (and (%%specified? initialize) (jazz:walk walker resume declaration environment initialize)))
          (%%when (%%class-is? new-declaration jazz:Property-Declaration)
            (let ((allocate? (%%neq? (jazz:get-lexical-binding-type new-declaration) jazz:Void)))
              (jazz:set-property-declaration-getter new-declaration
                (jazz:walk walker resume declaration environment
                  (cond (getter-name
                         `(lambda (self)
                            (with-self
                              (,getter-name self))))
                        (allocate?
                         `(lambda (self)
                            (with-self ,name)))
                        (else
                         #f))))
              (jazz:set-property-declaration-setter new-declaration
                (let ((value (jazz:generate-symbol "val")))
                  (jazz:walk walker resume declaration environment
                    (cond (setter-name
                           `(lambda (self ,value)
                              (with-self
                                (,setter-name self ,value))))
                          (allocate?
                           `(lambda (self ,value)
                              (with-self
                                (set! ,name ,value))))
                          (else
                           #f)))))))
          (jazz:set-declaration-source new-declaration form-src)
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
    (%%assertion (and (%%pair? rest) (%%pair? (jazz:source-code (%%car rest)))) (jazz:walk-error walker resume declaration #f "Ill-formed method in {a}: {s}" (jazz:get-lexical-binding-name (jazz:get-declaration-toplevel declaration)) (%%cons 'method (jazz:desourcify-all rest)))
      (let ((name (jazz:source-code (%%car (jazz:source-code (%%car rest)))))
            (name-src (%%car (jazz:source-code (%%car rest))))
            (parameters (jazz:wrap-parameters declaration (%%cdr (jazz:source-code (%%car rest))))))
        (jazz:parse-specifier (%%cdr rest)
          (lambda (specifier body)
            (let ((effective-body
                    (if (%%null? body)
                        (and (%%eq? abstraction 'concrete)
                             (%%list (%%list 'unspecified)))
                      body)))
              (values name name-src specifier access compatibility propagation abstraction expansion remote synchronized parameters effective-body))))))))


(define (jazz:walk-method-parameters walker resume declaration environment name parameters extended? walk? form-src)
  (define (first-positional-parameter parameter specifier)
    (%%assert (%%not specifier)
      (jazz:new-self-parameter parameter declaration #f)))
  
  (cond ((%%null? parameters)
         (jazz:walk-error walker resume declaration form-src "Missing self for method {a}" name))
        ((%%not (%%eq? (%%car parameters) 'self))
         (jazz:walk-error walker resume declaration form-src "Missing self for method {a}" name))
        (else
         (jazz:walk-parameters walker resume declaration environment parameters extended? walk? first-positional-parameter))))


(define (jazz:walk-method-declaration walker resume declaration environment form-src)
  (define (find-root-declaration name)
    (let* ((next-declaration (jazz:lookup-declaration declaration name jazz:private-access declaration))
           (root-declaration (and next-declaration (or (jazz:get-method-declaration-root next-declaration) next-declaration))))
      (if (and root-declaration (%%eq? declaration (jazz:get-declaration-parent root-declaration)))
          #f
        root-declaration)))
  
  (receive (name name-src specifier access compatibility propagation abstraction expansion remote synchronized parameters body) (jazz:parse-method walker resume declaration (%%cdr (jazz:source-code form-src)))
    (%%assertion (%%class-is? declaration jazz:Category-Declaration) (jazz:walk-error walker resume declaration form-src "Methods can only be defined inside categories: {s}" name)
      (let ((inline? (and (%%eq? expansion 'inline) body)))
        (receive (signature augmented-environment)
            ;; yuck. to clean
            (if inline?
                (jazz:walk-method-parameters walker resume declaration environment name parameters #t #t form-src)
              (values
                (jazz:walk-method-parameters walker resume declaration environment name parameters #t #f form-src)
                (jazz:unspecified)))
          (let* ((type (jazz:signature->function-type signature (if specifier (jazz:walk-specifier walker resume declaration environment specifier) jazz:Any)))
                 (root-declaration (find-root-declaration name))
                 (new-declaration (or (jazz:find-declaration-child declaration name)
                                      (jazz:new-method-declaration name type access compatibility '() declaration root-declaration propagation abstraction expansion remote synchronized signature))))
            (jazz:set-declaration-source new-declaration form-src)
            (jazz:set-declaration-name-source new-declaration name-src)
            (let ((effective-declaration (jazz:add-declaration-child walker resume declaration new-declaration)))
              (%%when inline?
                (jazz:set-method-declaration-signature effective-declaration signature)
                (jazz:set-method-declaration-body effective-declaration
                  (jazz:walk walker resume effective-declaration augmented-environment
                    `(with-self ,@body))))
              effective-declaration)))))))


(define (jazz:walk-method walker resume declaration environment form-src)
  (receive (name name-src specifier access compatibility propagation abstraction expansion remote synchronized parameters body) (jazz:parse-method walker resume declaration (%%cdr (jazz:source-code form-src)))
    (%%assertion (%%class-is? declaration jazz:Category-Declaration) (jazz:walk-error walker resume declaration form-src "Methods can only be defined inside categories: {s}" name)
      (let* ((new-declaration (jazz:lookup-declaration declaration name jazz:private-access declaration))
             (category-declaration (jazz:get-declaration-parent new-declaration))
             (root-method-declaration (jazz:get-method-declaration-root new-declaration))
             (root-method-propagation (and root-method-declaration (jazz:get-method-declaration-propagation root-method-declaration)))
             (root-category-declaration (and root-method-declaration (jazz:get-declaration-parent root-method-declaration))))
        (cond ((and root-category-declaration (%%eq? root-method-propagation 'final))
               (jazz:walk-error walker resume declaration form-src "Cannot redefine final method: {s}" name))
              ((and root-category-declaration (%%memq root-method-propagation '(virtual chained)) (%%neq? propagation 'override))
               (case propagation
                 ((virtual chained)
                  (jazz:walk-error walker resume declaration form-src "Cannot redefine virtual method: {s}" name))
                 ((final)
                  (jazz:walk-error walker resume declaration form-src "Cannot finalize virtual method: {s}" name))))
              ((and (%%not root-category-declaration) (%%eq? propagation 'override))
               (jazz:walk-error walker resume declaration form-src "Cannot find root method: {s}" name))
              ((and (%%not root-category-declaration) (%%class-is? category-declaration jazz:Interface-Declaration) (%%neq? propagation 'virtual))
               (jazz:walk-error walker resume declaration form-src "Interface method must be virtual: {s}" name))
              (else
               (if (and (jazz:analysis-mode?)
                        (%%eq? (jazz:get-method-declaration-propagation new-declaration) 'final))
                   (let ((data (jazz:get-analysis-data (jazz:get-declaration-locator new-declaration))))
                     (jazz:set-analysis-data-declaration-references data '())))
               (receive (signature augmented-environment) (jazz:walk-method-parameters walker resume declaration environment name parameters #t #t form-src)
                 (let ((with-self-body
                         (if (%%class-is? category-declaration jazz:Interface-Declaration)
                             body
                           `(with-self ,@body))))
                   (let ((body-expression
                           (cond (root-category-declaration
                                  (jazz:walk walker resume new-declaration (%%cons (jazz:new-nextmethod-variable 'nextmethod (jazz:get-lexical-binding-type root-method-declaration) #f) augmented-environment) with-self-body))
                                 (body
                                  (jazz:walk walker resume new-declaration augmented-environment with-self-body))
                                 (else
                                  #f))))
                     (%%when (%%not (and (%%eq? expansion 'inline) (%%neq? abstraction 'abstract)))
                       #; ;; wait buggy if file is both loaded interpreted and compiled
                       (%%when (and (%%neq? (jazz:walk-for) 'eval) (jazz:get-method-declaration-body new-declaration))
                         (jazz:walk-error walker resume declaration form-src "Cannot redefine method: {s}" name))
                       (jazz:set-method-declaration-signature new-declaration signature)
                       (jazz:set-method-declaration-body new-declaration body-expression))
                     (jazz:set-declaration-source new-declaration form-src)
                     (jazz:set-declaration-name-source new-declaration name-src)
                     new-declaration)))))))))


;; quick not elegant solution to wrap with-self around optional and named parameter code
(define (jazz:wrap-parameters declaration parameters)
  (if (%%class-is? declaration jazz:Interface-Declaration)
      parameters
    (let ((queue (jazz:new-queue)))
      (let iter ((scan parameters))
           (cond ((%%null? scan))
                 ((%%symbol? (jazz:source-code scan))
                  (jazz:enqueue-list queue scan))
                 (else
                  (let ((parameter (jazz:source-code (%%car scan))))
                    (if (%%pair? parameter)
                        (if (jazz:specifier? (jazz:source-code (%%car parameter)))
                            (jazz:enqueue queue parameter)
                          (if (%%keyword? (jazz:source-code (%%car parameter)))
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
      (jazz:queue-list queue))))


;;;
;;;; NextMethod Variable
;;;


(jazz:define-class jazz:NextMethod-Variable jazz:Variable (constructor: jazz:allocate-nextmethod-variable)
  ())


(define (jazz:new-nextmethod-variable name type source)
  (%%assertion (jazz:variable-name-valid? name) (jazz:error "Invalid variable name: {s}" (jazz:desourcify-all name))
    (jazz:allocate-nextmethod-variable name type #f #f source 0)))


(jazz:define-method (jazz:emit-binding-reference (jazz:NextMethod-Variable binding) source-declaration environment backend)
  (jazz:new-code
    (jazz:emit backend 'nextmethod-variable-reference binding)
    jazz:Any
    #f))


(jazz:define-method (jazz:walk-binding-validate-call (jazz:NextMethod-Variable declaration) walker resume source-declaration operator arguments form-src)
  (let ((signature (jazz:get-nextmethod-signature source-declaration)))
    (if signature
        (jazz:validate-arguments walker resume source-declaration declaration signature arguments form-src))))


(jazz:define-method (jazz:emit-binding-call (jazz:NextMethod-Variable binding) binding-src arguments arguments-codes source-declaration environment backend)
  (let ((type (jazz:get-lexical-binding-type binding))
        (self (jazz:*self*)))
    (jazz:new-code
      (jazz:emit backend 'nextmethod-call binding binding-src arguments-codes)
      (jazz:call-return-type type)
      #f)))


;;;
;;;; Self Parameter
;;;


(jazz:define-class jazz:Self-Parameter jazz:Parameter (constructor: jazz:allocate-self-parameter)
  ())


(define (jazz:new-self-parameter name type source)
  (jazz:allocate-self-parameter name type #f #f source 0))


(jazz:define-method (jazz:emit-check? (jazz:Self-Parameter parameter))
  #f)


;;;
;;;; Self Binding
;;;


(jazz:define-class jazz:Self-Binding jazz:Lexical-Binding (constructor: jazz:allocate-self-binding)
  ())


(define (jazz:new-self-binding type)
  (jazz:allocate-self-binding 'self type #f))


(jazz:define-method (jazz:emit-binding-reference (jazz:Self-Binding declaration) source-declaration environment backend)
  (jazz:new-code
    'self
    (jazz:emit backend 'self-reference declaration source-declaration)
    #f))


;;;
;;;; Dynamic Self Binding
;;;


(jazz:define-class jazz:Dynamic-Self-Binding jazz:Lexical-Binding (constructor: jazz:allocate-dynamic-self-binding)
  ((code getter: generate)))


(define (jazz:new-dynamic-self-binding type code)
  (jazz:allocate-dynamic-self-binding 'self type #f code))


(jazz:define-method (jazz:emit-binding-reference (jazz:Dynamic-Self-Binding declaration) source-declaration environment backend)
  (jazz:new-code
    (jazz:emit backend 'dynamic-self-reference declaration)
    (jazz:get-declaration-parent source-declaration)
    #f))


;;;
;;;; With Self
;;;


(define (jazz:walk-with-self walker resume declaration environment form-src)
  (let ((form (jazz:source-code form-src)))
    (let ((new-environment (%%cons (jazz:new-self-binding #f) environment)))
      (jazz:new-with-self
        (jazz:find-class-declaration declaration)
        (let ((body (%%cdr form)))
          (jazz:walk-body walker resume declaration new-environment body))))))


(define jazz:*self*
  (%%make-parameter #f))


;;;
;;;; With Local Variables
;;;


(define (jazz:parse-with-local-variables form)
  (let ((variables (%%car form))
        (body (%%cdr form)))
    (values variables body)))


(define (jazz:walk-with-local-variables-declaration walker resume declaration environment form-src)
  (receive (variables body) (jazz:parse-with-local-variables (%%cdr (jazz:source-code form-src)))
    (jazz:walk-declarations walker resume declaration environment body)))


(define (jazz:walk-with-local-variables walker resume declaration environment form-src)
  (define (make-bindings variables)
    (let ((table (%%make-table test: eq?)))
      (for-each (lambda (variable)
                  (%%table-set! table variable (jazz:new-local-variable-binding #f variable)))
                variables)
      table))
  
  (receive (variables body) (jazz:parse-with-local-variables (%%cdr (jazz:source-code form-src)))
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
  (let ((form (jazz:source-code form-src)))
    (jazz:bind (slot-names object . body) (%%cdr form)
      (let ((object-symbol (jazz:generate-symbol "object")))
        (jazz:walk walker resume declaration environment
          `(let ((,object-symbol ,object))
             (let-symbol ,(map (lambda (slot-name)
                                 (let* ((slot-declaration (jazz:lookup-reference walker resume declaration environment slot-name))
                                        (getter-name (jazz:get-slot-declaration-getter-name slot-declaration))
                                        (setter-name (jazz:get-slot-declaration-setter-name slot-declaration)))
                                   (%%list slot-name `(lambda () (%%list ',getter-name ',object-symbol)) `(lambda (value) (%%list ',setter-name ',object-symbol value)))))
                               slot-names)
                         ,@body)))))))


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
;(jazz:define-walker-syntax      doc                  jazz jazz:expand-doc)
(jazz:define-walker-syntax      property             jazz jazz:expand-property)
(jazz:define-walker-declaration %slot                jazz jazz:walk-%slot-declaration jazz:walk-%slot)
(jazz:define-walker-declaration %property            jazz jazz:walk-%slot-declaration jazz:walk-%slot)
(jazz:define-walker-declaration method               jazz jazz:walk-method-declaration jazz:walk-method)
(jazz:define-walker-declaration hub                  jazz jazz:walk-hub-declaration jazz:walk-hub)
(jazz:define-walker-macro       node                 jazz jazz:expand-node)
(jazz:define-walker-declaration %node                jazz jazz:walk-node-declaration jazz:walk-node)
(jazz:define-walker-special     declare              jazz jazz:walk-declare)
(jazz:define-walker-macro       specialize           jazz jazz:expand-specialize)
(jazz:define-walker-declaration %specialize          jazz jazz:walk-%specialize-declaration jazz:walk-%specialize)
(jazz:define-walker-special     parameterize         jazz jazz:walk-parameterize)
(jazz:define-walker-special     with-slots           jazz jazz:walk-with-slots)
(jazz:define-walker-special     with-self            jazz jazz:walk-with-self)
(jazz:define-walker-declaration with-dynamic-self    jazz jazz:walk-with-dynamic-self-declaration jazz:walk-with-dynamic-self)
(jazz:define-walker-declaration with-local-variables jazz jazz:walk-with-local-variables-declaration jazz:walk-with-local-variables)
(jazz:define-walker-special     cast                 jazz jazz:walk-cast)
(jazz:define-walker-special     %allege              jazz jazz:walk-%allege)
(jazz:define-walker-special     allocate             jazz jazz:walk-allocate)
(jazz:define-walker-special     static               jazz jazz:walk-static))
