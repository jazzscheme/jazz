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


(unit protected jazz.dialect.dialect


;;;
;;;; Definition-Declaration
;;;


(jazz.define-class-runtime jazz.Definition-Declaration)


(define (jazz.new-definition-declaration name type access compatibility attributes parent expansion signature)
  (let ((new-declaration (jazz.allocate-definition-declaration jazz.Definition-Declaration name type #f access compatibility attributes #f parent #f #f expansion signature #f)))
    (jazz.setup-declaration new-declaration)
    new-declaration))


(jazz.define-method (jazz.walk-binding-validate-call (jazz.Definition-Declaration declaration) walker resume source-declaration operator arguments form-src)
  (let ((signature (%%get-definition-declaration-signature declaration)))
    (if signature
        (jazz.validate-arguments walker resume source-declaration declaration signature arguments form-src))))


(jazz.define-method (jazz.emit-inlined-binding-call (jazz.Definition-Declaration declaration) arguments call source-declaration environment)
  (let ((value (%%get-definition-declaration-value declaration)))
    (if (%%class-is? value jazz.Lambda)
        (if (and (%%eq? (%%get-definition-declaration-expansion declaration) 'inline)
                 (or (jazz.inline-definitions?) (jazz.untyped-inline-definition? value)))
            (let ((signature (%%get-lambda-signature value))
                  (body (%%get-lambda-body value)))
              (if (jazz.only-positional? signature)
                  (if (%%fx= (%%get-signature-mandatory signature) (%%length arguments))
                      (jazz.with-annotated-frame (jazz.annotate-inlined-signature signature arguments)
                        (lambda (frame)
                          (let ((augmented-environment (%%cons frame environment)))
                            (let ((body-code (jazz.emit-expression body source-declaration augmented-environment)))
                              (jazz.new-code
                                `(let ,(map (lambda (parameter argument)
                                              `(,(jazz.emit-binding-symbol parameter source-declaration environment)
                                                ,(jazz.emit-type-cast argument (%%get-lexical-binding-type parameter) source-declaration environment)))
                                            (%%get-signature-positional signature)
                                            arguments)
                                   ,@(jazz.sourcify-list (%%get-code-form body-code) (%%get-expression-source call)))
                                (jazz.call-return-type (%%get-lexical-binding-type declaration))
                                #f)))))
                    (jazz.error "Wrong number of arguments passed to {s}" (%%get-lexical-binding-name declaration)))
                (jazz.error "Only positional parameters are supported in inlining: {s}" (%%get-lexical-binding-name declaration))))
          #f)
      #f #;
      ;; not correct as the value is always #f when looking up external declarations!
      ;; we need to walk the value even at parse time for inline declarations
      (jazz.error "Constant inlining is not yet supported: {s}" (%%get-lexical-binding-name declaration)))))


;; quick solution for now as some inlined definitions like /= will change the semantics if not inlined
(define (jazz.untyped-inline-definition? value)
  (jazz.every? (lambda (parameter)
                 (%%not (%%get-lexical-binding-type parameter)))
               (%%get-signature-positional (%%get-lambda-signature value))))


(jazz.define-method (jazz.emit-declaration (jazz.Definition-Declaration declaration) environment)
  (let ((locator (%%get-declaration-locator declaration))
        (value (%%get-definition-declaration-value declaration)))
    (jazz.sourcify-if
      `(begin
         (define ,locator
           ,(jazz.emit-type-cast (jazz.emit-expression value declaration environment) (%%get-lexical-binding-type declaration) declaration environment))
         ,(let ((name (%%get-lexical-binding-name declaration))
                (parent (%%get-declaration-parent declaration)))
            (if (%%is? parent jazz.Module-Declaration)
                `(jazz.register-definition ',(%%get-lexical-binding-name parent) ',name ',locator)
              `(jazz.add-field ,(%%get-declaration-locator parent) (jazz.new-definition ',name ',locator)))))
      (%%get-declaration-source declaration))))


(jazz.define-method (jazz.emit-binding-reference (jazz.Definition-Declaration declaration) source-declaration environment)
  (jazz.new-code
    (%%get-declaration-locator declaration)
    (or (%%get-lexical-binding-type declaration)
        jazz.Any)
    #f))


(jazz.define-method (jazz.walk-binding-validate-assignment (jazz.Definition-Declaration declaration) walker resume source-declaration symbol-src)
  (nextmethod declaration walker resume source-declaration symbol-src)
  (%%when (%%neq? (%%get-declaration-toplevel declaration) (%%get-declaration-toplevel source-declaration))
    (jazz.walk-error walker resume source-declaration symbol-src "Illegal inter-module assignment to: {s}" (%%get-lexical-binding-name declaration))))


(jazz.define-method (jazz.walk-binding-assignable? (jazz.Definition-Declaration declaration))
  #t)


(jazz.define-method (jazz.emit-binding-assignment (jazz.Definition-Declaration declaration) value source-declaration environment)
  (let ((locator (%%get-declaration-locator declaration)))
    (jazz.new-code
      `(set! ,locator ,(jazz.emit-type-cast (jazz.emit-expression value source-declaration environment) (%%get-lexical-binding-type declaration) source-declaration environment))
      jazz.Any
      #f)))


(jazz.define-method (jazz.fold-declaration (jazz.Definition-Declaration declaration) f k s)
  (f declaration
     (k (jazz.fold-statement (%%get-definition-declaration-value declaration) f k s)
        s)))


(jazz.encapsulate-class jazz.Definition-Declaration)


;;;
;;;; Specialize
;;;


(jazz.define-class-runtime jazz.Specialize)


(define (jazz.new-specialize)
  (jazz.allocate-specialize jazz.Specialize #f #f))


(jazz.define-method (jazz.emit-expression (jazz.Specialize expression) declaration environment)
  (jazz.new-code
    `(begin)
    jazz.Any
    #f))


(jazz.define-method (jazz.fold-expression (jazz.Specialize expression) f k s)
  (f expression s))


(jazz.encapsulate-class jazz.Specialize)


;;;
;;;; Generic-Declaration
;;;


(jazz.define-class-runtime jazz.Generic-Declaration)


(define (jazz.new-generic-declaration name type access compatibility attributes parent dispatch-types signature)
  (let ((new-declaration (jazz.allocate-generic-declaration jazz.Generic-Declaration name type #f access compatibility attributes #f parent #f #f dispatch-types signature #f)))
    (jazz.setup-declaration new-declaration)
    new-declaration))


(jazz.define-method (jazz.walk-binding-validate-call (jazz.Generic-Declaration declaration) walker resume source-declaration operator arguments form-src)
  (jazz.validate-arguments walker resume source-declaration declaration (%%get-generic-declaration-signature declaration) arguments form-src))


(jazz.define-method (jazz.emit-declaration (jazz.Generic-Declaration declaration) environment)
  (let ((generic-locator (%%get-declaration-locator declaration))
        (signature (%%get-generic-declaration-signature declaration))
        (body (%%get-generic-declaration-body declaration)))
    (jazz.with-annotated-frame (jazz.annotate-signature signature)
      (lambda (frame)
        (let ((augmented-environment (%%cons frame environment)))
          (jazz.sourcify-if
            `(jazz.define-generic ,(%%cons generic-locator (jazz.emit-signature signature declaration augmented-environment))
                                  ,@(jazz.sourcified-form (jazz.emit-expression body declaration augmented-environment)))
            (%%get-declaration-source declaration)))))))


(jazz.define-method (jazz.emit-binding-reference (jazz.Generic-Declaration declaration) source-declaration environment)
  (jazz.new-code
    (%%get-declaration-locator declaration)
    jazz.Any
    #f))


(jazz.define-method (jazz.fold-declaration (jazz.Generic-Declaration declaration) f k s)
  (f declaration
     (k (jazz.fold-statement (%%get-generic-declaration-body declaration) f k s)
        s)))


(jazz.encapsulate-class jazz.Generic-Declaration)


;;;
;;;; Specific-Declaration
;;;


(jazz.define-class-runtime jazz.Specific-Declaration)


(define (jazz.new-specific-declaration name type access compatibility attributes parent generic signature root?)
  (let ((new-declaration (jazz.allocate-specific-declaration jazz.Specific-Declaration name type #f access compatibility attributes #f parent #f #f generic signature #f root?)))
    (jazz.setup-declaration new-declaration)
    new-declaration))


(jazz.define-method (jazz.get-nextmethod-signature (jazz.Specific-Declaration declaration))
  (%%get-specific-declaration-signature declaration))


(jazz.define-method (jazz.emit-declaration (jazz.Specific-Declaration declaration) environment)
  (let* ((generic-declaration (%%get-specific-declaration-generic declaration))
         (generic-locator (%%get-declaration-locator generic-declaration))
         (generic-object-locator (jazz.generic-object-locator generic-locator))
         (signature (%%get-specific-declaration-signature declaration))
         (body (%%get-specific-declaration-body declaration)))
    (jazz.with-annotated-frame (jazz.annotate-signature signature)
      (lambda (frame)
        (let ((augmented-environment (%%cons frame environment))
              (modifier (if (%%get-specific-declaration-root? declaration) 'root 'child)))
          (jazz.sourcify-if
            `(jazz.define-specific ,(%%cons generic-locator (jazz.emit-signature signature declaration augmented-environment)) ,modifier
                       ,@(jazz.sourcified-form (jazz.emit-expression body declaration augmented-environment)))
            (%%get-declaration-source declaration)))))))


(jazz.define-method (jazz.fold-declaration (jazz.Specific-Declaration declaration) f k s)
  (f declaration
     (k (jazz.fold-statement (%%get-specific-declaration-body declaration) f k s)
           s)))


(jazz.encapsulate-class jazz.Specific-Declaration)


;;;
;;;; Category-Declaration
;;;


(jazz.define-class-runtime jazz.Category-Declaration)


(jazz.define-method (jazz.emit-binding-reference (jazz.Category-Declaration declaration) source-declaration environment)
  (jazz.new-code
    (%%get-declaration-locator declaration)
    jazz.Category-Declaration
    #f))


(jazz.encapsulate-class jazz.Category-Declaration)


;;;
;;;; Class-Declaration
;;;


(jazz.define-class-runtime jazz.Class-Declaration)


(define (jazz.new-class-declaration name type access compatibility attributes parent implementor metaclass ascendant ascendant-relation ascendant-base interfaces)
  (let ((new-declaration (jazz.allocate-class-declaration jazz.Class-Declaration name type #f access compatibility attributes #f parent #f #f (jazz.make-access-lookups jazz.protected-access) (jazz.new-queue) #f implementor metaclass ascendant ascendant-relation ascendant-base interfaces)))
    (jazz.setup-declaration new-declaration)
    new-declaration))


(define (jazz.setup-class-lookups class-declaration)
  (define (resolve-class decl)
    (if decl
        (let ((class-declaration (jazz.resolve-binding decl)))
          (%%assert (%%is? class-declaration jazz.Class-Declaration))
          class-declaration)
      #f))
  
  (define (resolve-interface decl)
    (if decl
        (let ((interface-declaration (jazz.resolve-binding decl)))
          (%%assert (%%is? interface-declaration jazz.Interface-Declaration))
          interface-declaration)
      #f))
  
  (let ((ascendant (resolve-class (%%get-class-declaration-ascendant class-declaration)))
        (interfaces (map resolve-interface (%%get-class-declaration-interfaces class-declaration))))
    
    (let ((private (%%get-access-lookup class-declaration jazz.private-access)))
      (if ascendant
          (let ((same-module? (%%eq? (%%get-declaration-toplevel class-declaration)
                                     (%%get-declaration-toplevel ascendant))))
            (%%table-merge! private (%%get-access-lookup ascendant (if same-module? jazz.private-access jazz.public-access)) #t)))
      (for-each (lambda (interface)
                  (%%table-merge! private (%%get-access-lookup interface jazz.public-access)))
                interfaces))
    
    (let ((public (%%get-access-lookup class-declaration jazz.public-access)))
      (if ascendant
          (%%table-merge! public (%%get-access-lookup ascendant jazz.public-access)))
      (for-each (lambda (interface)
                  (%%table-merge! public (%%get-access-lookup interface jazz.public-access)))
                interfaces))
    
    ;; jazz.add-declaration-child does not set jazz.protected-access
    (let ((not-private (%%get-access-lookup class-declaration jazz.public-access)))
      (%%vector-set! (%%get-namespace-declaration-lookups class-declaration) jazz.protected-access not-private))
    
    #;
    (let ((protected (%%get-access-lookup class-declaration jazz.protected-access)))
      (if ascendant
          (%%table-merge! protected (%%get-access-lookup ascendant jazz.public-access)))
      (for-each (lambda (interface)
                  (%%table-merge! protected (%%get-access-lookup interface jazz.public-access)))
                interfaces))))


(jazz.define-method (jazz.emit-binding-reference (jazz.Class-Declaration declaration) source-declaration environment)
  (jazz.new-code
    (%%get-declaration-locator declaration)
    (or (%%get-category-declaration-metaclass declaration)
        jazz.Class-Declaration)
    #f))


(jazz.define-method (jazz.of-subtype? (jazz.Class-Declaration declaration) subtype)
  (if (jazz.object-declaration? declaration)
      #t
    (and (%%class-is? subtype jazz.Class-Declaration)
         (let iter ((target subtype))
           (if (%%not target)
               #f
             (let ((target-declaration (jazz.resolve-binding target)))
               (if (%%eq? target-declaration declaration)
                   #t
                 (iter (%%get-class-declaration-ascendant target-declaration)))))))))


(jazz.define-method (jazz.specifiable? (jazz.Class-Declaration declaration))
  #t)


(jazz.define-method (jazz.emit-declaration (jazz.Class-Declaration declaration) environment)
  (let ((name (%%get-lexical-binding-name declaration))
        (locator (%%get-declaration-locator declaration))
        (ascendant-declaration (%%get-class-declaration-ascendant declaration))
        (interface-declarations (%%get-class-declaration-interfaces declaration))
        (body (%%get-namespace-declaration-body declaration)))
    (let ((level-locator (jazz.compose-helper locator 'level)))
      (jazz.sourcify-if
        `(begin
           ,@(if (jazz.core-class? name)
                 (let ((core-class (jazz.get-core-class name)))
                   (if (%%not (%%symbol? core-class))
                       (jazz.validate-core-class core-class declaration))
                   (let ((core-class-locator (if (%%symbol? core-class) core-class (%%get-category-identifier core-class)))
                         (ascendant-access (if (%%not ascendant-declaration) #f (jazz.sourcified-form (jazz.emit-binding-reference ascendant-declaration declaration environment)))))
                     `((define ,locator ,core-class-locator)
                       (define ,level-locator (%%get-class-level ,locator))
                       (jazz.set-core-class-redefined ',name ',core-class-locator)
                       (jazz.remove-own-slots ,locator))))
               (let ((metaclass-declaration (%%get-category-declaration-metaclass declaration)))
                 (let ((ascendant-access (jazz.emit-ascendant-access declaration environment)))
                   (let ((metaclass-access (if (%%not metaclass-declaration) (if (%%not ascendant-access) 'jazz.Object-Class `(%%get-object-class ,ascendant-access)) (jazz.sourcified-form (jazz.emit-binding-reference metaclass-declaration declaration environment))))
                         (interface-accesses (map (lambda (declaration) (jazz.sourcified-form (jazz.emit-binding-reference declaration declaration environment))) interface-declarations)))
                     `((define ,locator
                         ;; this is a quicky that needs to be well tought out
                         (if (jazz.global-bound? ',locator)
                             (jazz.global-ref ',locator)
                           (jazz.new-class ,metaclass-access ',locator ,ascendant-access (%%list ,@interface-accesses))))
                       (define ,level-locator (%%get-class-level ,locator)))))))
           ,(let ((toplevel-declaration (%%get-declaration-toplevel declaration)))
              `(jazz.register-module-entry ',(%%get-lexical-binding-name toplevel-declaration) ',name ,locator))
           ,@(jazz.emit-namespace-statements body declaration environment))
        (%%get-declaration-source declaration)))))


(define (jazz.emit-ascendant-access declaration environment)
  (let ((ascendant (%%get-class-declaration-ascendant declaration))
        (ascendant-relation (%%get-class-declaration-ascendant-relation declaration))
        (ascendant-base (%%get-class-declaration-ascendant-base declaration)))
    (cond ((%%not ascendant)
           #f)
          ((%%not ascendant-relation)
           (jazz.sourcified-form (jazz.emit-binding-reference ascendant declaration environment)))
          (else
           (let rec ((rel ascendant-relation))
             (if (%%null? rel)
                 (jazz.sourcified-form (jazz.emit-binding-reference ascendant-base declaration environment))
               `(%%get-object-class ,(rec (%%cdr rel)))))))))


(jazz.encapsulate-class jazz.Class-Declaration)


(define (jazz.find-class-declaration declaration)
  (let iter ((decl declaration))
    (cond ((%%not decl)
           (jazz.error "Unable to find class declaration for {s}" declaration))
          ((%%class-is? decl jazz.Class-Declaration)
           decl)
          (else
           (iter (%%get-declaration-parent decl))))))


(set! jazz.object-declaration?
      (lambda (type)
        (and (%%class-is? type jazz.Class-Declaration)
             (%%not (%%get-class-declaration-ascendant type)))))


;;;
;;;; Validate
;;;


(define (jazz.validate-core-class core-class declaration)
  (define (validate-category)
    (define (validate-ascendant)
      (let* ((core-class-ascendant (%%get-class-ascendant core-class))
             (core-class-ascendant-name (if (%%not core-class-ascendant) '() (jazz.identifier-name (%%get-category-identifier core-class-ascendant))))
             (declaration-ascendant (%%get-class-declaration-ascendant declaration))
             (declaration-ascendant-name (if (%%not declaration-ascendant) '() (jazz.identifier-name (%%get-declaration-locator declaration-ascendant)))))
        (%%when (%%not (%%eq? core-class-ascendant-name declaration-ascendant-name))
          (jazz.error "Inconsistant core-class/class ascendant for {s}: {s} / {s}" (%%get-lexical-binding-name declaration) core-class-ascendant-name declaration-ascendant-name))))
    
    (define (validate-interfaces)
      (let ((declaration-interfaces (%%get-class-declaration-interfaces declaration)))
        (%%when (%%not (%%null? declaration-interfaces))
          (jazz.error "Interfaces are not supported in open classes: {s}" (%%get-lexical-binding-name declaration)))))
    
    (validate-ascendant)
    (validate-interfaces))
  
  (define (validate-slots)
    (define (collect-slots lst)
      (let ((queue (jazz.new-queue)))
        (define (process obj)
          (cond ((%%is? obj jazz.Slot-Declaration)
                 (jazz.enqueue queue obj))
                ((%%is? obj jazz.Begin)
                 (for-each process (%%get-begin-expressions obj)))))
        
        (for-each process lst)
        (jazz.queue-list queue)))
    
    (let ((core-class-slot-names (map (lambda (name/slot) (if (%%symbol? name/slot) name/slot (%%get-field-name name/slot))) (%%get-class-instance-slots core-class)))
          (declaration-slot-names (map (lambda (decl) (%%get-lexical-binding-name decl)) (collect-slots (%%get-namespace-declaration-body declaration)))))
      (%%when (%%not (%%equal? core-class-slot-names declaration-slot-names))
        (jazz.error "Inconsistant core-class/class slots for {s}: {s} / {s}" (%%get-lexical-binding-name declaration) core-class-slot-names declaration-slot-names))))
  
  (validate-category)
  (validate-slots))


;;;
;;;; Interface-Declaration
;;;


(jazz.define-class-runtime jazz.Interface-Declaration)


(define (jazz.new-interface-declaration name type access compatibility attributes parent implementor metaclass ascendants)
  (let ((new-declaration (jazz.allocate-interface-declaration jazz.Interface-Declaration name type #f access compatibility attributes #f parent #f #f (jazz.make-access-lookups jazz.protected-access) (jazz.new-queue) #f implementor metaclass ascendants)))
    (jazz.setup-declaration new-declaration)
    new-declaration))


(define (jazz.setup-interface-lookups interface-declaration)
  (define (resolve-interface decl)
    (if decl
        (let ((interface-declaration (jazz.resolve-binding decl)))
          (%%assert (%%is? interface-declaration jazz.Interface-Declaration))
          interface-declaration)
      #f))
  
  (let ((ascendants (map resolve-interface (%%get-interface-declaration-ascendants interface-declaration))))
    
    (let ((private (%%get-access-lookup interface-declaration jazz.private-access)))
      (for-each (lambda (interface)
                  (%%table-merge! private (%%get-access-lookup interface jazz.public-access) #t))
                ascendants))
    
    ;; a quick test
    (let ((private (%%get-access-lookup interface-declaration jazz.private-access)))
      (%%vector-set! (%%get-namespace-declaration-lookups interface-declaration) jazz.public-access private)
      (%%vector-set! (%%get-namespace-declaration-lookups interface-declaration) jazz.protected-access private))
    
    #;
    (let ((public (%%get-access-lookup interface-declaration jazz.public-access)))
      (for-each (lambda (interface)
                  (%%table-merge! public (%%get-access-lookup interface jazz.public-access)))
                ascendants))
    
    #;
    (let ((protected (%%get-access-lookup interface-declaration jazz.protected-access)))
      (for-each (lambda (interface)
                  (%%table-merge! protected (%%get-access-lookup interface jazz.public-access)))
                ascendants))))


(jazz.define-method (jazz.of-subtype? (jazz.Interface-Declaration declaration) subtype)
  ;; quicky to fill later on
  #f)


(jazz.define-method (jazz.specifiable? (jazz.Interface-Declaration declaration))
  #t)


(jazz.define-method (jazz.emit-declaration (jazz.Interface-Declaration declaration) environment)
  (let* ((name (%%get-lexical-binding-name declaration))
         (locator (%%get-declaration-locator declaration))
         (rank-locator (jazz.compose-helper locator 'rank))
         (ascendant-declarations (%%get-interface-declaration-ascendants declaration))
         (metaclass-declaration (%%get-category-declaration-metaclass declaration))
         (metaclass-access (if (%%not metaclass-declaration) 'jazz.Interface (jazz.sourcified-form (jazz.emit-binding-reference metaclass-declaration declaration environment))))
         (ascendant-accesses (map (lambda (declaration) (jazz.sourcified-form (jazz.emit-binding-reference declaration declaration environment))) ascendant-declarations))
         (body (%%get-namespace-declaration-body declaration)))
    (jazz.sourcify-if
      `(begin
         (define ,locator
           (jazz.new-interface ,metaclass-access ',locator (%%list ,@ascendant-accesses)))
         (define ,rank-locator
           (%%get-interface-rank ,locator))
         ,(let ((toplevel-declaration (%%get-declaration-toplevel declaration)))
            `(jazz.register-module-entry ',(%%get-lexical-binding-name toplevel-declaration) ',name ,locator))
         ,@(jazz.emit-namespace-statements body declaration environment))
      (%%get-declaration-source declaration))))


(jazz.encapsulate-class jazz.Interface-Declaration)


;;;
;;;; Field-Declaration
;;;


(jazz.define-class-runtime jazz.Field-Declaration)


(jazz.encapsulate-class jazz.Field-Declaration)


;;;
;;;; Slot-Declaration
;;;


(jazz.define-class-runtime jazz.Slot-Declaration)


(define (jazz.new-slot-declaration name type access compatibility attributes parent initialize getter-name setter-name)
  (let ((new-declaration (jazz.allocate-slot-declaration jazz.Slot-Declaration name type #f access compatibility attributes #f parent #f #f initialize getter-name setter-name)))
    (jazz.setup-declaration new-declaration)
    new-declaration))


(jazz.define-method (jazz.walk-binding-validate-call (jazz.Slot-Declaration declaration) walker resume source-declaration operator arguments form-src)
  #f)


(jazz.define-method (jazz.emit-declaration (jazz.Slot-Declaration declaration) environment)
  (let* ((name (%%get-lexical-binding-name declaration))
         (locator (%%get-declaration-locator declaration))
         (class-declaration (%%get-declaration-parent declaration))
         (class-locator (%%get-declaration-locator class-declaration))
         (allocate? (%%neq? (%%get-lexical-binding-type declaration) jazz.Void))
         (core? (jazz.core-class? (%%get-lexical-binding-name class-declaration)))
         (initialize (%%get-slot-declaration-initialize declaration))
         (initialize? (and allocate? (%%not core?) initialize))
         (initialize-locator (and initialize? (jazz.compose-helper locator 'initialize)))
         (slot-locator (jazz.compose-helper locator 'slot))
         (offset-locator (jazz.compose-helper locator 'offset)))
    (jazz.sourcify-if
      `(begin
         ,@(if initialize?
               `((define (,initialize-locator self)
                   ,(jazz.sourcified-form (jazz.emit-expression initialize declaration environment))))
             '())
         (define ,slot-locator
           (jazz.add-slot ,class-locator ',name ,initialize-locator ,allocate?))
         (define ,offset-locator
           (%%get-slot-offset ,slot-locator))
         ,@(jazz.declaration-result))
      (%%get-declaration-source declaration))))


(jazz.define-method (jazz.emit-binding-reference (jazz.Slot-Declaration declaration) source-declaration environment)
  (let ((self (jazz.*self*)))
    (if self
        (let ((offset-locator (jazz.compose-helper (%%get-declaration-locator declaration) 'offset)))
          (jazz.new-code
            `(%%object-ref ,(jazz.sourcified-form self) ,offset-locator)
            (jazz.find-annotated-type declaration environment)
            #f))
      (jazz.error "Illegal reference to a slot: {s}" (%%get-declaration-locator declaration)))))


(jazz.define-method (jazz.walk-binding-assignable? (jazz.Slot-Declaration declaration))
  #t)


(jazz.define-method (jazz.emit-binding-assignment (jazz.Slot-Declaration declaration) value source-declaration environment)
  (let ((self (jazz.*self*)))
    (if self
        (let ((offset-locator (jazz.compose-helper (%%get-declaration-locator declaration) 'offset)))
          (jazz.new-code
            `(%%object-set! ,(jazz.sourcified-form self) ,offset-locator ,(jazz.sourcified-form (jazz.emit-expression value source-declaration environment)))
            jazz.Any
            #f))
      (jazz.error "Illegal assignment to a slot: {s}" (%%get-declaration-locator declaration)))))


(jazz.define-method (jazz.fold-declaration (jazz.Slot-Declaration declaration) f k s)
  (f declaration
     (k (jazz.fold-statement (%%get-slot-declaration-initialize declaration) f k s)
        s)))


(jazz.encapsulate-class jazz.Slot-Declaration)


;;;
;;;; Property
;;;


(jazz.define-class-runtime jazz.Property-Declaration)


(define (jazz.new-property-declaration name type access compatibility attributes parent initialize getter-name setter-name)
  (let ((new-declaration (jazz.allocate-property-declaration jazz.Property-Declaration name type #f access compatibility attributes #f parent #f #f initialize getter-name setter-name #f #f)))
    (jazz.setup-declaration new-declaration)
    new-declaration))


(jazz.define-method (jazz.emit-declaration (jazz.Property-Declaration declaration) environment)
  (let* ((name (%%get-lexical-binding-name declaration))
         (locator (%%get-declaration-locator declaration))
         (class-declaration (%%get-declaration-parent declaration))
         (class-locator (%%get-declaration-locator class-declaration))
         (allocate? (%%neq? (%%get-lexical-binding-type declaration) jazz.Void))
         (core? (jazz.core-class? (%%get-lexical-binding-name class-declaration)))
         (initialize (%%get-slot-declaration-initialize declaration))
         (initialize? (and allocate? (%%not core?) initialize))
         (initialize-locator (and initialize? (jazz.compose-helper locator 'initialize)))
         (slot-locator (jazz.compose-helper locator 'slot))
         (offset-locator (jazz.compose-helper locator 'offset))
         (getter (%%get-property-declaration-getter declaration))
         (setter (%%get-property-declaration-setter declaration)))
    ;; XXXX hack in a literal self instead of the hygienically renamed self
    (define (fix-self expr)
      (if (pair? expr)
          (cons (car expr) (cons (cons 'self (cdadr expr)) (cddr expr)))
        expr))
    (jazz.sourcify-if
      `(begin
         ,@(if initialize?
               `((define (,initialize-locator self)
                   ,(jazz.sourcified-form (jazz.emit-expression initialize declaration environment))))
             '())
         (define ,slot-locator
           (jazz.add-property ,class-locator ',name ,initialize-locator ,allocate?
             ,(fix-self (jazz.sourcified-form (jazz.emit-expression getter declaration environment)))
             ,(fix-self (jazz.sourcified-form (jazz.emit-expression setter declaration environment)))))
         (define ,offset-locator
           (%%get-slot-offset ,slot-locator))
         ,@(jazz.declaration-result))
      (%%get-declaration-source declaration))))


(jazz.encapsulate-class jazz.Property-Declaration)


;;;
;;;; Method-Declaration
;;;


(jazz.define-class-runtime jazz.Method-Declaration)


(define (jazz.new-method-declaration name type access compatibility attributes parent root propagation abstraction expansion remote synchronized signature)
  (let ((new-declaration (jazz.allocate-method-declaration jazz.Method-Declaration name type #f access compatibility attributes #f parent #f #f root propagation abstraction expansion remote synchronized signature #f)))
    (jazz.setup-declaration new-declaration)
    new-declaration))


(define (jazz.method-dispatch-info declaration)
  (let ((root (%%get-method-declaration-root declaration))
        (propagation (%%get-method-declaration-propagation declaration)))
    (if (and (%%not root) (%%eq? propagation 'final))
        (values 'final declaration)
      (let ((root-method-declaration (%%get-method-declaration-root declaration)))
        (let ((method-declaration (or root-method-declaration declaration)))
          (let ((category-declaration (%%get-declaration-parent method-declaration)))
            (cond ((%%class-is? category-declaration jazz.Class-Declaration)
                   (values 'class method-declaration))
                  ((%%class-is? category-declaration jazz.Interface-Declaration)
                   (values 'interface method-declaration))
                  (else
                   (error "Invalid category declaration: {s}" category-declaration)))))))))


(define (jazz.native-category? category-declaration)
  (%%neq? (%%get-category-declaration-implementor category-declaration) 'primitive))


(define (jazz.emit-method-dispatch object declaration source-declaration environment)
  (let ((name (%%get-lexical-binding-name declaration)))
    (receive (dispatch-type method-declaration) (jazz.method-dispatch-info declaration)
      (let ((category-declaration (%%get-declaration-parent method-declaration)))
        (let ((object-cast (jazz.emit-type-cast object category-declaration source-declaration environment)))
          (jazz.new-code
            (case dispatch-type
              ((final)
               (let ((implementation-locator (%%get-declaration-locator method-declaration)))
                 `(%%final-dispatch (jazz.class-of ,object-cast) ,implementation-locator)))
              ((class)
               (let ((class-level-locator (jazz.compose-helper (%%get-declaration-locator category-declaration) 'level))
                     (method-rank-locator (jazz.compose-helper (%%get-declaration-locator method-declaration) 'rank)))
                 (if (jazz.native-category? category-declaration)
                     `(%%class-dispatch (%%get-object-class ,object-cast) ,class-level-locator ,method-rank-locator)
                   `(%%class-dispatch (jazz.class-of ,object-cast) ,class-level-locator ,method-rank-locator))))
              ((interface)
               (let ((interface-rank-locator (jazz.compose-helper (%%get-declaration-locator category-declaration) 'rank))
                     (method-rank-locator (jazz.compose-helper (%%get-declaration-locator method-declaration) 'rank)))
                 (if (jazz.native-category? category-declaration)
                     `(%%interface-dispatch (%%get-object-class ,object-cast) ,interface-rank-locator ,method-rank-locator)
                   `(%%interface-dispatch (jazz.class-of ,object-cast) ,interface-rank-locator ,method-rank-locator)))))
            (jazz.call-return-type (%%get-lexical-binding-type method-declaration))
            #f))))))


(jazz.define-method (jazz.emit-binding-reference (jazz.Method-Declaration declaration) source-declaration environment)
  (let ((self (jazz.*self*)))
    (if self
        (let ((dispatch-code (jazz.emit-method-dispatch self declaration source-declaration environment)))
          (jazz.new-code
            `(lambda rest
               (apply ,(jazz.sourcified-form dispatch-code) ,(jazz.sourcified-form self) rest))
            (%%get-code-type dispatch-code)
            #f))
      (jazz.error "Methods can only be called directly from inside a method: {a} in {a}" (%%get-lexical-binding-name declaration) (%%get-declaration-locator source-declaration)))))


(jazz.define-method (jazz.walk-binding-validate-call (jazz.Method-Declaration declaration) walker resume source-declaration operator arguments form-src)
  (let ((signature (%%get-method-declaration-signature declaration)))
    (if signature
        (jazz.validate-arguments walker resume source-declaration declaration signature arguments form-src))))


(jazz.define-method (jazz.emit-inlined-binding-call (jazz.Method-Declaration declaration) arguments call source-declaration environment)
  ;; mostly copy/pasted and adapted from definition declaration. need to unify the code
  (if (%%eq? (%%get-method-declaration-expansion declaration) 'inline)
      (receive (dispatch-type method-declaration) (jazz.method-dispatch-info declaration)
        (case dispatch-type
          ((final)
           (let ((signature (%%get-method-declaration-signature declaration))
                 (body (%%get-method-declaration-body declaration)))
             (if (jazz.only-positional? signature)
                 (if (%%fx= (%%get-signature-mandatory signature) (%%length arguments))
                     (jazz.with-annotated-frame (jazz.annotate-signature signature)
                       (lambda (frame)
                         (let ((augmented-environment (%%cons frame environment)))
                           (let ((body-code (jazz.emit-expression body source-declaration augmented-environment)))
                             (jazz.new-code
                               `(let ,(map (lambda (parameter argument)
                                             `(,(jazz.emit-binding-symbol parameter source-declaration environment)
                                               ,(jazz.emit-type-cast argument (%%get-lexical-binding-type parameter) source-declaration environment)))
                                           (%%get-signature-positional signature)
                                           arguments)
                                  ,(jazz.sourcify-if (jazz.desourcify-all (%%get-code-form body-code)) (%%get-expression-source call)))
                               (jazz.call-return-type (%%get-lexical-binding-type declaration))
                               #f)))))
                   (jazz.error "Wrong number of arguments passed to {s}" (%%get-lexical-binding-name declaration)))
               (jazz.error "Only positional parameters are supported in inlining: {s}" (%%get-lexical-binding-name declaration)))))
          (else
           #f)))
    #f))


(jazz.define-method (jazz.emit-binding-call (jazz.Method-Declaration declaration) binding-src arguments source-declaration environment)
  (let ((self (jazz.*self*)))
    (if self
        (let ((type (%%get-lexical-binding-type declaration))
              (arguments (jazz.codes-forms arguments))
              (dispatch-code (jazz.emit-method-dispatch self declaration source-declaration environment)))
          (jazz.new-code
            `(,(jazz.sourcified-form dispatch-code)
              ,(jazz.sourcified-form self)
              ,@arguments)
            (%%get-code-type dispatch-code)
            #f))
      (jazz.error "Methods can only be called directly from inside a method: {a} in {a}" (%%get-lexical-binding-name declaration) (%%get-declaration-locator source-declaration)))))


(jazz.define-method (jazz.get-nextmethod-signature (jazz.Method-Declaration declaration))
  (define (lookup category-declaration method-name)
    (if (%%is? category-declaration jazz.Autoload-Declaration)
        (let ((category-declaration (%%get-autoload-declaration-declaration category-declaration)))
          (jazz.lookup-declaration category-declaration method-name jazz.private-access category-declaration))
      (jazz.lookup-declaration category-declaration method-name jazz.private-access category-declaration)))
  
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


(jazz.define-method (jazz.emit-declaration (jazz.Method-Declaration declaration) environment)
  (let ((name (%%get-lexical-binding-name declaration))
        (body-type (let ((type (%%get-lexical-binding-type declaration)))
                     (if (%%is? type jazz.Function-Type) (%%get-function-type-result type) jazz.Any)))
        (propagation (%%get-method-declaration-propagation declaration))
        (abstraction (%%get-method-declaration-abstraction declaration))
        (signature (%%get-method-declaration-signature declaration))
        (body (%%get-method-declaration-body declaration)))
    (let* ((category-declaration (%%get-declaration-parent declaration))
           (root-method-declaration (%%get-method-declaration-root declaration))
           (root-category-declaration (and root-method-declaration (%%get-declaration-parent root-method-declaration)))
           (class-locator (%%get-declaration-locator category-declaration))
           (method-locator (%%get-declaration-locator declaration))
           (method-rank-locator (jazz.compose-helper method-locator 'rank))
           (method-node-locator (jazz.compose-helper method-locator 'node))
           (method-call (cond ((%%class-is? category-declaration jazz.Class-Declaration)     (case propagation
                                                                                               ((override)        'jazz.add-method-node)
                                                                                               ((final)           'jazz.add-final-method)
                                                                                               ((virtual chained) 'jazz.add-virtual-method)))
                              ((%%class-is? category-declaration jazz.Interface-Declaration) (case propagation
                                                                                               ((override)        'jazz.add-method-node)
                                                                                               ((virtual)         'jazz.add-virtual-method))))))
      (jazz.with-annotated-frame (jazz.annotate-signature signature)
        (lambda (frame)
          (let* ((augmented-environment (%%cons frame environment))
                 (emit-body (lambda () 
                              (let ((body-code (jazz.emit-expression body declaration augmented-environment)))
                                (jazz.emit-type-cast body-code body-type declaration augmented-environment)))))
            (jazz.sourcify-if
              (case method-call
                ((jazz.add-method-node)
                 (let ((node (jazz.generate-symbol "node")))
                   `(begin
                      (define (,method-locator self ,@(jazz.emit-signature signature declaration augmented-environment))
                        ,@(jazz.emit-signature-casts signature declaration augmented-environment)
                        (let ((nextmethod (%%get-method-node-next-implementation ,method-node-locator)))
                          ,(emit-body)))
                      (define ,method-node-locator
                        (,method-call ,class-locator ',name ,method-locator))
                      ,@(jazz.declaration-result))))
                ((jazz.add-virtual-method)
                 `(begin
                    ,(if (%%eq? abstraction 'abstract)
                         `(define (,method-locator self . rest)
                            (jazz.call-into-abstract ',class-locator ',name self rest))
                       `(define (,method-locator self ,@(jazz.emit-signature signature declaration augmented-environment))
                          ,@(jazz.emit-signature-casts signature declaration augmented-environment)
                          (let ()
                            ,(emit-body))))
                    (define ,method-rank-locator
                      (,method-call ,class-locator ',name ,method-locator))
                    ,@(jazz.declaration-result)))
                ((jazz.add-final-method)
                 `(begin
                    (define (,method-locator self ,@(jazz.emit-signature signature declaration augmented-environment))
                      ,@(jazz.emit-signature-casts signature declaration augmented-environment)
                      (let ()
                        ,(emit-body)))
                    (,method-call ,class-locator ',name ,method-locator)
                    ,@(jazz.declaration-result))))
              (%%get-declaration-source declaration))))))))


(jazz.define-method (jazz.fold-declaration (jazz.Method-Declaration declaration) f k s)
  (f declaration
     (let ((body (%%get-method-declaration-body declaration)))
       (if (%%not body)
           s
         (k (jazz.fold-statement body f k s) s)))))


(jazz.encapsulate-class jazz.Method-Declaration)


;;;
;;;; Dialect
;;;


(jazz.define-class-runtime jazz.Jazz-Dialect)


(define (jazz.new-jazz-dialect)
  (jazz.allocate-jazz-dialect jazz.Jazz-Dialect '()))


(jazz.define-method (jazz.dialect-name (jazz.Jazz-Dialect dialect))
  'jazz)


(jazz.define-method (jazz.dialect-walker (jazz.Jazz-Dialect dialect))
  (jazz.new-jazz-walker))


(jazz.encapsulate-class jazz.Jazz-Dialect)


;;;
;;;; Walker
;;;


(jazz.define-class-runtime jazz.Jazz-Walker)


(define (jazz.new-jazz-walker)
  (jazz.allocate-jazz-walker jazz.Jazz-Walker '() '() '() (jazz.new-queue) (%%make-table test: eq?) '()))


(jazz.define-method (jazz.runtime-export (jazz.Jazz-Walker walker) declaration)
  (or (nextmethod walker declaration)
      (if (or (%%is? declaration jazz.Definition-Declaration)
              (%%is? declaration jazz.Generic-Declaration)
              (%%is? declaration jazz.Category-Declaration))
          (%%get-declaration-locator declaration)
        #f)))


;;;
;;;; Environment
;;;


(jazz.define-method (jazz.walker-bindings (jazz.Jazz-Walker walker))
  (append (%%get-dialect-bindings (jazz.get-dialect 'jazz))
          (nextmethod walker)))


;;;
;;;; Declaration
;;;


(jazz.define-method (jazz.walk-declaration (jazz.Jazz-Walker walker) resume declaration environment form-src)
  (if (%%pair? (jazz.source-code form-src))
      (let ((first (jazz.source-code (%%car (jazz.source-code form-src)))))
        (case first
          ((definition)           (jazz.walk-definition-declaration           walker resume declaration environment form-src))
          ((%specialize)          (jazz.walk-%specialize-declaration          walker resume declaration environment form-src))
          ((generic)              (jazz.walk-generic-declaration              walker resume declaration environment form-src))
          ((specific)             #f)
          ((%class)               (jazz.walk-%class-declaration               walker resume declaration environment form-src))
          ((interface)            (jazz.walk-interface-declaration            walker resume declaration environment form-src))
          ((%slot %property)      (jazz.walk-%slot-declaration                walker resume declaration environment form-src))
          ((method)               (jazz.walk-method-declaration               walker resume declaration environment form-src))
          ((with-dynamic-self)    (jazz.walk-with-dynamic-self-declaration    walker resume declaration environment form-src))
          ((with-local-variables) (jazz.walk-with-local-variables-declaration walker resume declaration environment form-src))
          ((c-include)            #f)
          ((c-named-declare)      (jazz.walk-c-named-declare-declaration      walker resume declaration environment form-src))
          ((c-type)               (jazz.walk-c-type-declaration               walker resume declaration environment form-src))
          ((c-definition)         (jazz.walk-c-definition-declaration         walker resume declaration environment form-src))
          (else                   (nextmethod walker resume declaration environment form-src))))
    #f))


(define (jazz.expand-declaration-path walker resume declaration environment)
  `(quote ,(jazz.get-declaration-path declaration)))


(define (jazz.expand-declaration-locator walker resume declaration environment)
  `(quote ,(apply jazz.compose-name (jazz.get-declaration-path declaration))))


;;;
;;;; Parse
;;;


(define (jazz.parse-keywords keywords rest)
  (let ((table (%%make-table test: eq?))
        (done? #f))
    (%%while (and (%%not done?) (%%not (%%null? rest)))
      (let ((symbol (jazz.source-code (%%car rest))))
        (if (%%not (%%memq symbol keywords))
            (set! done? #t)
          (begin
            (%%table-set! table symbol (%%desourcify (%%cadr rest)))
            (set! rest (%%cddr rest))))))
    (%%apply values (%%append (map (lambda (keyword)
                                     (%%table-ref table keyword (jazz.unspecified)))
                                   keywords)
                              (%%list rest)))))


;;;
;;;; Specialize
;;;


(set! jazz.emit-specialized-locator
      (lambda (locator arguments environment)
        (case locator
          ((jazz.dialect.kernel.class-of)
           (%%assert (and (%%pair? arguments) (%%null? (%%cdr arguments)))
             (jazz.emit-specialized-class-of (%%car arguments) environment)))
          (else
           #f))))


(define (jazz.emit-specialized-class-of object environment)
  (jazz.new-code
    `(jazz.class-of ,(jazz.sourcified-form object))
    (let ((type (%%get-code-type object)))
      (if (%%class-is? type jazz.Class-Declaration)
          (%%get-category-declaration-metaclass type)
        jazz.Class-Declaration))
    #f))


;;;
;;;; New
;;;


(set! jazz.emit-primitive-new-call
      (lambda (operator locator arguments arguments-codes declaration environment)
        (if (%%eq? locator 'jazz.dialect.kernel.new)
            (%%assert (%%pair? arguments)
              (let ((class-expression (%%car arguments)))
                (if (%%class-is? class-expression jazz.Binding-Reference)
                    (let ((binding (%%get-reference-binding class-expression)))
                      (if (or (%%class-is? binding jazz.Class-Declaration)
                              (%%class-is? binding jazz.Autoload-Declaration))
                          (let ((values-codes (%%cdr arguments-codes)))
                            (jazz.new-code
                              (case (%%length values-codes)
                                ((0) `(jazz.new0 ,@(jazz.codes-forms arguments-codes)))
                                ((1) `(jazz.new1 ,@(jazz.codes-forms arguments-codes)))
                                ((2) `(jazz.new2 ,@(jazz.codes-forms arguments-codes)))
                                (else `(jazz.new ,@(jazz.codes-forms arguments-codes))))
                              binding
                              #f))
                        #f))
                  #f)))
          #f)))


;;;
;;;; Symbol
;;;


(jazz.define-method (jazz.walk-symbol (jazz.Jazz-Walker walker) resume declaration environment symbol-src)
  (let ((symbol (unwrap-syntactic-closure symbol-src)))
    (jazz.split-tilde symbol
      (lambda (tilde? name self/class-name)
        (if tilde?
            (cond ((and name self/class-name)
                   (if (%%eq? self/class-name 'self)
                       (let ((slot-declaration (jazz.lookup-declaration (jazz.find-class-declaration declaration) name jazz.private-access declaration)))
                         (%%assert (%%class-is? slot-declaration jazz.Slot-Declaration)
                           (jazz.new-binding-reference symbol-src slot-declaration)))
                     (let ((category-declaration (jazz.resolve-binding (jazz.lookup-reference walker resume declaration environment self/class-name))))
                       (%%assert (%%class-is? category-declaration jazz.Category-Declaration)
                         (let ((method-declaration (jazz.lookup-declaration category-declaration name jazz.private-access declaration)))
                           (%%assert (%%class-is? method-declaration jazz.Method-Declaration)
                             (jazz.new-method-reference method-declaration)))))))
                  ((and name (not self/class-name))
                   (let ((method-symbol-src (jazz.sourcify-if (jazz.dispatch->symbol (unwrap-syntactic-closure symbol-src)) symbol-src)))
                     (jazz.walk walker resume declaration environment `(lambda (object . rest) (apply (~ ,method-symbol-src object) rest)))))
                  ((and (not name) self/class-name)
                   (jazz.walk-error walker resume declaration symbol-src "Ill-formed expression: {s}" symbol))
                  (else
                   (nextmethod walker resume declaration environment symbol-src)))
          (nextmethod walker resume declaration environment symbol-src))))))


(define (jazz.split-tilde symbol proc)
  (let ((str (%%symbol->string symbol)))
    (let ((n (jazz.string-find-reversed str #\~)))
      (if (%%not n)
          (proc #f #f #f)
        (let ((len (%%string-length str)))
          (proc #t
                (if (%%fx> n 0) (%%string->symbol (%%substring str 0 n)) #f)
                (if (%%fx< (%%fx+ n 1) len) (%%string->symbol (%%substring str (%%fx+ n 1) len)) #f)))))))


(jazz.define-method (jazz.lookup-analyse (jazz.Jazz-Walker walker) declaration symbol-src referenced-declaration)
  (if (and (%%is? declaration jazz.Method-Declaration)
           (%%eq? (%%get-method-declaration-propagation declaration) 'final)
           (or (%%eq? (jazz.source-code symbol-src) 'self)
               (%%is? referenced-declaration jazz.Slot-Declaration)
               (%%is? referenced-declaration jazz.Method-Declaration)))
      (let ((data (jazz.get-analysis-data (%%get-declaration-locator declaration))))
        (%%set-analysis-data-declaration-references data (%%cons referenced-declaration (%%get-analysis-data-declaration-references data))))))


;;;
;;;; Assignment
;;;


(jazz.define-method (jazz.walk-symbol-assignment (jazz.Jazz-Walker walker) resume declaration environment symbol-src value)
  (jazz.split-tilde (jazz.source-code symbol-src)
    (lambda (tilde? name self/class-name)
      (if tilde?
          (if (and name (%%eq? self/class-name 'self))
              (let ((slot-declaration (jazz.lookup-declaration (jazz.find-class-declaration declaration) name jazz.private-access declaration)))
                (%%assert (%%class-is? slot-declaration jazz.Slot-Declaration)
                  (jazz.new-assignment slot-declaration (jazz.walk walker resume declaration environment value))))
            (jazz.walk-error walker resume declaration symbol-src "Ill-formed set! variable: {s}" (%%desourcify symbol-src)))
        (nextmethod walker resume declaration environment symbol-src value)))))


;;;
;;;; Form
;;;


(jazz.define-method (jazz.walk-form (jazz.Jazz-Walker walker) resume declaration environment form-src)
  (let ((procedure-expr (jazz.source-code (%%car (jazz.source-code form-src)))))
    (if (jazz.dispatch? procedure-expr)
        (jazz.walk-dispatch walker resume declaration environment form-src)
      (nextmethod walker resume declaration environment form-src))))


;;;
;;;; With-Self
;;;


(jazz.define-class-runtime jazz.With-Self)


(define (jazz.new-with-self body)
  (jazz.allocate-with-self jazz.With-Self #f #f body))


(jazz.define-method (jazz.emit-expression (jazz.With-Self expression) declaration environment)
  (let ((body (%%get-with-self-body expression)))
    (jazz.new-code
      (jazz.simplify-begin
        `(begin
           ,@(parameterize ((jazz.*self* (jazz.new-code 'self declaration #f)))
               (jazz.sourcified-form (jazz.emit-expression body declaration environment)))))
      jazz.Any
      #f)))


(jazz.define-method (jazz.fold-expression (jazz.With-Self expression) f k s)
  (f expression
     (k (jazz.fold-expression (%%get-with-self-body expression) f k s)
        s)))


(jazz.encapsulate-class jazz.With-Self)


;;;
;;;; With-Dynamic-Self
;;;


(jazz.define-class-runtime jazz.With-Dynamic-Self)


(define (jazz.new-with-dynamic-self code body)
  (jazz.allocate-with-dynamic-self jazz.With-Dynamic-Self #f #f code body))


(jazz.define-method (jazz.emit-expression (jazz.With-Dynamic-Self expression) declaration environment)
  (let ((code (%%get-with-dynamic-self-code expression))
        (body (%%get-with-dynamic-self-body expression)))
    (jazz.new-code
      (jazz.simplify-begin
        `(begin
           ,@(parameterize ((jazz.*self* (jazz.new-code code declaration #f)))
               (jazz.sourcified-form (jazz.emit-statements-code body declaration environment)))))
      jazz.Any
      #f)))


(jazz.define-method (jazz.fold-expression (jazz.With-Dynamic-Self expression) f k s)
  (f expression
     (jazz.fold-expressions (%%get-with-dynamic-self-body expression) f k s s)))


(jazz.encapsulate-class jazz.With-Dynamic-Self)


;;;
;;;; Cast
;;;


(jazz.define-class-runtime jazz.Cast)


(define (jazz.new-cast type expression)
  (jazz.allocate-cast jazz.Cast type #f expression))


(jazz.define-method (jazz.emit-expression (jazz.Cast expression) declaration environment)
  (let ((type (%%get-expression-type expression))
        (expression (%%get-cast-expression expression)))
    (jazz.new-code
      (jazz.emit-type-cast
        (jazz.emit-expression expression declaration environment)
        type
        declaration
        environment)
      type
      #f)))


(jazz.define-method (jazz.fold-expression (jazz.Cast expression) f k s)
  (f expression
     (jazz.fold-expression (%%get-cast-expression expression) f k s)))


(jazz.encapsulate-class jazz.Cast)


;;;
;;;; Construct
;;;


(jazz.define-class-runtime jazz.Construct)


(define (jazz.new-construct class values)
  (jazz.allocate-construct jazz.Construct #f #f class values))


(jazz.define-method (jazz.emit-expression (jazz.Construct expression) declaration environment)
  (let ((class (%%get-construct-class expression))
        (values (%%get-construct-values expression)))
    (jazz.new-code
      `(%%object ,(jazz.sourcified-form (jazz.emit-expression class declaration environment))
         ,@(jazz.codes-forms (jazz.emit-expressions values declaration environment)))
      jazz.Any
      #f)))


(jazz.define-method (jazz.fold-expression (jazz.Construct expression) f k s)
  (f expression
     (k (jazz.fold-expression (%%get-construct-class expression) f k s)
        (jazz.fold-expressions (%%get-construct-values expression) f k s s))))


(jazz.encapsulate-class jazz.Construct)


;;;
;;;; Dispatch
;;;


(define (jazz.cache-dispatch name setter)
  (lambda (object)
    (let ((class (jazz.class-of object)))
      (let ((category (jazz.locate-method-owner class name)))
        (%%assertion category (jazz.error "Unable to find method {s} in: {s}" name object)
          (let ((field (%%get-category-field category name)))
            (%%assertion (%%class-is? field jazz.Method) (jazz.error "Field {s} is not a method of {s}" name object)
              (let ((proc
                      (case (%%get-method-dispatch-type field)
                        ((final)
                         (jazz.final-dispatch field category))
                        ((class)
                         (jazz.class-dispatch field category))
                        ((interface)
                         (jazz.interface-dispatch field category)))))
                (setter proc)
                (proc object)))))))))


(define (jazz.final-dispatch field type)
  (lambda (object)
    (%%debug-assertion (%%category-is? object type) (jazz.dispatch-error field object type)
      (%%final-dispatch (jazz.class-of object) (%%get-method-implementation field)))))


(define (jazz.class-dispatch field type)
  (let ((class-level (%%get-method-category-rank field))
        (implementation-rank (%%get-method-implementation-rank field)))
    (lambda (object)
      (%%debug-assertion (%%category-is? object type) (jazz.dispatch-error field object type)
        (%%class-dispatch (jazz.class-of object) class-level implementation-rank)))))


(define (jazz.interface-dispatch field type)
  (let ((interface-rank (%%get-method-category-rank field))
        (implementation-rank (%%get-method-implementation-rank field)))
    (lambda (object)
      (%%debug-assertion (%%category-is? object type) (jazz.dispatch-error field object type)
        (%%interface-dispatch (jazz.class-of object) interface-rank implementation-rank)))))


(define (jazz.dispatch class name)
  (or (jazz.find-dispatch class name)
      (jazz.error "Unable to find method {s} in: {s}" name class)))


(define (jazz.find-dispatch class name)
  (let ((category (jazz.locate-method-owner class name)))
    (if (%%not category)
        #f
      (let ((field (%%get-category-field category name)))
        (if (not (%%class-is? field jazz.Method))
            (jazz.error "Field {s} is not a method of {s}" name class))
        (case (%%get-method-dispatch-type field)
          ((final)
           (%%final-dispatch class (%%get-method-implementation field)))
          ((class)
           (%%class-dispatch class (%%get-method-category-rank field) (%%get-method-implementation-rank field)))
          ((interface)
           (%%interface-dispatch class (%%get-method-category-rank field) (%%get-method-implementation-rank field))))))))


(jazz.define-class-runtime jazz.Dispatch)


(define (jazz.new-dispatch source name arguments)
  (jazz.allocate-dispatch jazz.Dispatch #f source name arguments))


(jazz.define-method (jazz.emit-expression (jazz.Dispatch expression) declaration environment)
  (jazz.emit-dispatch expression declaration environment))


(define (jazz.emit-dispatch expression declaration environment)
  (let ((name (%%get-dispatch-name expression))
        (arguments (%%get-dispatch-arguments expression)))
    (define (resolve-type object-code)
      (let ((object-type (jazz.patch-type-until-unification (%%get-code-type object-code))))
        (if (%%class-is? object-type jazz.Autoload-Declaration)
            (jazz.resolve-binding object-type)
          object-type)))
    
    (define (lookup-method object-code)
      (let ((object-type (resolve-type object-code)))
        (if (%%class-is? object-type jazz.Category-Declaration)
            (let ((declaration (jazz.lookup-declaration object-type name jazz.public-access declaration)))
              (if (and declaration (%%class-is? declaration jazz.Method-Declaration))
                  declaration
                #f))
          #f)))
    
    (define (add-to-module-references namespace-declaration method-declaration)
      (%%when (and method-declaration
                   (%%neq? namespace-declaration (%%get-declaration-toplevel method-declaration)))
        (let* ((module-declaration (%%get-declaration-toplevel namespace-declaration))
               (references-table (%%get-module-declaration-walker-references module-declaration)))
          (%%when (%%neq? module-declaration (%%get-declaration-toplevel method-declaration))
            (%%table-set! references-table (%%get-declaration-locator method-declaration) method-declaration)))))
    
    (define (lookup-method/warn object-code)
      (let ((method-declaration (lookup-method object-code)))
        (if (%%not method-declaration)
            (begin
              (if (and (jazz.warnings?) (jazz.get-module-warn? (%%get-declaration-toplevel declaration) 'optimizations))
                  (jazz.debug 'Warning: 'In (%%get-declaration-locator declaration) 'unable 'to 'find 'dispatch 'method name))
              #f)
          (begin
            (add-to-module-references declaration method-declaration)
            method-declaration))))
    
    (let ((object-argument (%%car arguments))
          (rest-arguments (%%cdr arguments)))
      (let ((object-code (jazz.emit-expression object-argument declaration environment))
            (rest-codes (jazz.emit-expressions rest-arguments declaration environment)))
        (let ((method-declaration (lookup-method/warn object-code)))
          (if method-declaration
              (or (jazz.emit-inlined-final-dispatch expression method-declaration object-code rest-codes declaration environment)
                  (jazz.with-code-value object-code
                    (lambda (code)
                      (let ((dispatch-code (jazz.emit-method-dispatch code method-declaration declaration environment)))
                        (jazz.new-code
                          `(,(jazz.sourcified-form dispatch-code)
                            ,(jazz.sourcified-form code)
                            ,@(jazz.codes-forms rest-codes))
                          (%%get-code-type dispatch-code)
                          (%%get-expression-source expression))))))
            (let ((dv (jazz.register-variable declaration (%%string-append (%%symbol->string name) "!d") #f)))
              (let ((d (%%car dv)))
                (%%set-cdr! dv `(jazz.cache-dispatch ',name (lambda (d) (set! ,d d))))
                (jazz.new-code
                  (jazz.with-uniqueness (jazz.sourcified-form object-code)
                    (lambda (object)
                      `((,d ,object) ,object ,@(jazz.codes-forms rest-codes))))
                  jazz.Any
                  (%%get-expression-source expression))))))))))


(define (jazz.with-code-value code proc)
  (let ((form (%%get-code-form code)))
    (if (%%symbol? form)
        (proc code)
      (let ((value (jazz.generate-symbol "val")))
        (let ((code (proc (jazz.new-code value (%%get-code-type code) #f))))
          (jazz.new-code
            `(let ((,value ,form))
               ,(%%get-code-form code))
            (%%get-code-type code)
            (%%get-code-source code)))))))


(define (jazz.emit-inlined-final-dispatch expression declaration object arguments source-declaration environment)
  ;; mostly copy/pasted and adapted from method declaration. need to unify the code
  (if (%%eq? (%%get-method-declaration-expansion declaration) 'inline)
      (receive (dispatch-type method-declaration) (jazz.method-dispatch-info declaration)
        (case dispatch-type
          ((final)
           (let ((signature (%%get-method-declaration-signature declaration))
                 (body (%%get-method-declaration-body declaration)))
             (if (jazz.only-positional? signature)
                 (if (%%fx= (%%get-signature-mandatory signature) (%%length arguments))
                     (jazz.with-annotated-frame (jazz.annotate-signature signature)
                       (lambda (frame)
                         (let ((augmented-environment (%%cons frame environment)))
                           (let ((body-code (jazz.emit-expression body source-declaration augmented-environment)))
                             (jazz.new-code
                               `(let ,(%%cons `(self ,(jazz.sourcified-form object))
                                              (map (lambda (parameter argument)
                                                     `(,(jazz.emit-binding-symbol parameter source-declaration environment)
                                                       ,(jazz.emit-type-cast argument (%%get-lexical-binding-type parameter) source-declaration environment)))
                                                   (%%get-signature-positional signature)
                                                   arguments))
                                  ,(jazz.sourcify-if (jazz.desourcify-all (%%get-code-form body-code)) (%%get-expression-source expression)))
                               (jazz.call-return-type (%%get-lexical-binding-type declaration))
                               #f)))))
                   (jazz.error "Wrong number of arguments passed to {s}" (%%get-lexical-binding-name declaration)))
               (jazz.error "Only positional parameters are supported in inlining: {s}" (%%get-lexical-binding-name declaration)))))
          (else
           #f)))
    #f))


(jazz.define-method (jazz.fold-expression (jazz.Dispatch expression) f k s)
  (f expression
     (k (%%get-dispatch-name expression)
        (jazz.fold-expressions (%%get-dispatch-arguments expression) f k s s))))


(jazz.encapsulate-class jazz.Dispatch)


(define (jazz.walk-dispatch walker resume declaration environment form-src)
  (let ((name (jazz.dispatch->symbol (unwrap-syntactic-closure (%%car (jazz.source-code form-src)))))
        (arguments (%%cdr (jazz.source-code form-src))))
    (%%assertion (%%not (%%null? arguments)) (jazz.walk-error walker resume declaration form-src "Dispatch call must contain at least one argument: {s}" (%%desourcify form-src))
      (jazz.new-dispatch form-src name
        (jazz.walk-list walker resume declaration environment arguments)))))


;;;
;;;; Definition
;;;


(define jazz.definition-modifiers
  '(((private protected package public) . private)
    ((deprecated undocumented uptodate) . uptodate)
    ((inline onsite) . onsite)))


(define (jazz.parse-definition walker resume declaration rest)
  (receive (access compatibility expansion rest) (jazz.parse-modifiers walker resume declaration jazz.definition-modifiers rest)
    (if (%%symbol? (jazz.source-code (%%car rest)))
        (let ((name (jazz.source-code (%%car rest))))
          (jazz.parse-specifier (%%cdr rest)
            (lambda (specifier rest)
              (values name specifier access compatibility expansion (%%car rest) #f))))
      (let* ((name (jazz.source-code (%%car (jazz.source-code (%%car rest)))))
             (parameters (%%cdr (%%desourcify (%%car rest)))))
        (jazz.parse-specifier (%%cdr rest)
          (lambda (specifier body)
            (let ((effective-body (if (%%null? body) (%%list (%%list 'unspecified)) body))
                  (specifier-list (if specifier (%%list specifier) '())))
              (let ((value
                     `(lambda ,parameters ,@specifier-list
                        ,@effective-body)))
                (values name specifier access compatibility expansion value parameters)))))))))


(define (jazz.walk-definition-declaration walker resume declaration environment form-src)
  (receive (name specifier access compatibility expansion value parameters) (jazz.parse-definition walker resume declaration (%%cdr (jazz.source-code form-src)))
    (%%assertion (%%class-is? declaration jazz.Namespace-Declaration) (jazz.walk-error walker resume declaration form-src "Definitions can only be defined inside namespaces: {s}" name)
      (let ((type (jazz.specifier->type walker resume declaration environment specifier)))
        (let ((signature (and parameters (jazz.walk-parameters walker resume declaration environment parameters #t #f))))
          (let ((effective-type (if parameters (jazz.build-function-type signature type) type)))
            (let ((new-declaration (or (jazz.find-child-declaration declaration name)
                                       (jazz.new-definition-declaration name effective-type access compatibility '() declaration expansion signature))))
              (%%set-declaration-source new-declaration form-src)
              (let ((effective-declaration (jazz.add-declaration-child walker resume declaration new-declaration)))
                (%%when (%%eq? expansion 'inline)
                  (let ((new-environment (%%cons effective-declaration environment)))
                    (%%set-definition-declaration-value effective-declaration
                                                        (jazz.walk walker resume effective-declaration new-environment value))))
                effective-declaration))))))))


(define (jazz.walk-definition walker resume declaration environment form-src)
  (receive (name specifier access compatibility expansion value parameters) (jazz.parse-definition walker resume declaration (%%cdr (jazz.source-code form-src)))
    (%%assertion (%%class-is? declaration jazz.Namespace-Declaration) (jazz.walk-error walker resume declaration form-src "Definitions can only be defined inside namespaces: {s}" name)
      (let ((new-declaration (jazz.require-declaration declaration name)))
        (%%when (%%neq? expansion 'inline)
          ;; adding source information for parameters (default for optional and keyword may be source code)
          ;; jazz.find-annotated fails on first keyword at jazz.dialect.language.functional.minimum
          ;; because (eq? variable annotated-variable) -> one points to a stale value
          (let ((new-environment (if #f #; parameters
                                   (receive (signature augmented-environment) (jazz.walk-parameters walker resume declaration environment parameters #t #t)
                                     (%%set-definition-declaration-signature new-declaration signature)
                                     (%%cons new-declaration augmented-environment))
                                   (%%cons new-declaration environment))))
            (%%set-definition-declaration-value new-declaration (jazz.walk walker resume new-declaration new-environment value))))
        (%%set-declaration-source new-declaration form-src)
        new-declaration))))


;; Until I unify signature and function type
;; Only positional parameters as a first draft
(define (jazz.build-function-type signature result-type)
  (define (parameter-type parameter)
    (or (%%get-lexical-binding-type parameter)
        jazz.Any))
  
  (jazz.new-function-type
    (map parameter-type (%%get-signature-positional signature))
    (map parameter-type (%%get-signature-optional signature))
    (map parameter-type (%%get-signature-named signature))
    (let ((rest (%%get-signature-rest signature)))
      (and rest (parameter-type rest)))
    (or result-type jazz.Any)))


;;;
;;;; Specialize Macro
;;;


(define jazz.specialize-modifiers
  '(((inline onsite) . onsite)))


(define (jazz.parse-specialize walker resume declaration rest)
  (receive (expansion rest) (jazz.parse-modifiers walker resume declaration jazz.specialize-modifiers rest)
    (if (%%eq? (%%car rest) 'as)
        (values expansion (%%cadr rest) (%%cddr rest))
      (values expansion #f rest))))


(define (jazz.expand-specialize walker resume declaration environment . rest)
  (receive (expansion as rest) (jazz.parse-specialize walker resume declaration rest)
    (let ((signature (%%car rest))
          (rest (%%cdr rest)))
      (let ((operator (%%car signature))
            (parameters (%%cdr signature)))
        (let ((name (or as (jazz.compose-specializer-name operator parameters))))
          `(begin
             (definition public undocumented ,expansion (,name ,@parameters) ,@rest)
             (%specialize ,operator ,name)))))))


(define (jazz.compose-specializer-name operator parameters)
  (%%string->symbol
    (%%string-append (%%symbol->string operator)
                     (%%apply string-append (%%apply append (map (lambda (parameter)
                                                                   (if (jazz.specifier? parameter)
                                                                       (%%list (%%symbol->string (jazz.specifier->name parameter)))
                                                                     '()))
                                                                 parameters))))))


;;;
;;;; Specialize
;;;


(define (jazz.walk-%specialize-declaration walker resume declaration environment form-src)
  (let ((specialized (jazz.source-code (%%cadr (jazz.source-code form-src))))
        (specializer (jazz.source-code (%%car (%%cddr (jazz.source-code form-src))))))
    (let ((specialized-declaration (jazz.lookup-reference walker resume declaration environment specialized))
          (specializer-declaration (jazz.lookup-reference walker resume declaration environment specializer)))
      (jazz.add-specializer specialized-declaration specializer-declaration)
      (jazz.new-specialize))))


;; we should not even have to do this
(define (jazz.walk-%specialize walker resume declaration environment form-src)
  (jazz.new-specialize))


;;;
;;;; Generic
;;;


(define jazz.generic-modifiers
  '(((private protected package public) . public)
    ((deprecated undocumented uptodate) . uptodate)))


(define (jazz.parse-generic walker resume declaration rest)
  (receive (access compatibility rest) (jazz.parse-modifiers walker resume declaration jazz.generic-modifiers rest)
    (let ((signature (jazz.source-code (%%car rest))))
      (let ((name (jazz.source-code (%%car signature)))
            (parameters (%%cdr signature)))
        (jazz.parse-specifier (%%cdr rest)
          (lambda (specifier body)
            (values name specifier access compatibility parameters body)))))))


(define (jazz.walk-generic-declaration walker resume declaration environment form-src)
  (receive (name specifier access compatibility parameters body) (jazz.parse-generic walker resume declaration (%%cdr (jazz.source-code form-src)))
    (%%assertion (%%class-is? declaration jazz.Module-Declaration) (jazz.walk-error walker resume declaration form-src "Generics can only be defined at the module level: {s}" name)
      (let ((type (if specifier (jazz.walk-specifier walker resume declaration environment specifier) jazz.Any))
            (dispatch-type-declarations (map (lambda (dynamic-parameter-type)
                                               (jazz.lookup-reference walker resume declaration environment dynamic-parameter-type))
                                             (jazz.dynamic-parameter-types parameters)))
            (signature (jazz.walk-parameters walker resume declaration environment parameters #t #f)))
        (let ((new-declaration (jazz.new-generic-declaration name type access compatibility '() declaration dispatch-type-declarations signature)))
          (%%set-declaration-source new-declaration form-src)
          (let ((effective-declaration (jazz.add-declaration-child walker resume declaration new-declaration)))
            effective-declaration))))))


(define (jazz.walk-generic walker resume declaration environment form-src)
  (receive (name specifier access compatibility parameters body) (jazz.parse-generic walker resume declaration (%%cdr (jazz.source-code form-src)))
    (%%assertion (%%class-is? declaration jazz.Module-Declaration) (jazz.walk-error walker resume declaration form-src "Generics can only be defined at the module level: {s}" name)
      (receive (signature augmented-environment) (jazz.walk-parameters walker resume declaration environment parameters #t #t)
        (let ((new-declaration (jazz.require-declaration declaration name)))
          (%%set-generic-declaration-signature new-declaration signature)
          (%%set-generic-declaration-body new-declaration
                                          (jazz.walk-body walker resume new-declaration augmented-environment body))
          (%%set-declaration-source new-declaration form-src)
          new-declaration)))))


;;;
;;;; Specific
;;;


(define jazz.specific-modifiers
  '())


(define (jazz.parse-specific walker resume declaration rest)
  (receive (rest) (jazz.parse-modifiers walker resume declaration jazz.specific-modifiers rest)
    (let* ((signature (jazz.desourcify (%%car rest)))
           (body (%%cdr rest))
           (effective-body (if (%%null? body) (%%list (%%list 'unspecified)) body))
           (name (%%car signature))
           (parameters (%%cdr signature)))
      (values name parameters effective-body))))


(define (jazz.walk-specific walker resume declaration environment form-src)
  (define (root-dynamic-parameters? generic-declaration specific-signature name parameters)
    (let iter ((generic-parameters (%%get-signature-positional (%%get-generic-declaration-signature generic-declaration)))
               (specific-parameters (%%get-signature-positional specific-signature))
               (root? #t))
      (let ((generic-parameter (and (%%pair? generic-parameters) (%%car generic-parameters)))
            (specific-parameter (and (%%pair? specific-parameters) (%%car specific-parameters))))
        (let ((generic-dynamic? (%%is? generic-parameter jazz.Dynamic-Parameter))
              (specific-dynamic? (%%is? specific-parameter jazz.Dynamic-Parameter)))
          (cond ((and generic-dynamic? specific-dynamic?)
                 (let ((generic-class (jazz.resolve-binding (%%get-reference-binding (%%get-dynamic-parameter-class generic-parameter))))
                       (specific-class (jazz.resolve-binding (%%get-reference-binding (%%get-dynamic-parameter-class specific-parameter)))))
                   (if (jazz.of-subtype? generic-class specific-class)
                       (iter (%%cdr generic-parameters)
                             (%%cdr specific-parameters)
                             (%%eq? generic-class specific-class))
                     (jazz.walk-error walker resume declaration form-src "Dynamic parameter {a} is not a subtype of {a}: {s}"
                       (%%get-lexical-binding-name specific-parameter)
                       (%%get-declaration-locator generic-class)
                       (%%cons name parameters)))))
                ((or generic-dynamic? specific-dynamic?)
                 (jazz.walk-error walker resume declaration form-src "Specific must dispatch on the same number of dynamic parameters: {s}" (%%cons name parameters)))
                (else
                 root?))))))
  
  (receive (name parameters body) (jazz.parse-specific walker resume declaration (%%cdr (jazz.source-code form-src)))
    (%%assertion (%%class-is? declaration jazz.Module-Declaration) (jazz.walk-error walker resume declaration form-src "Specifics can only be defined at the module level: {s}" name)
      (let ((generic-declaration (jazz.lookup-declaration declaration name jazz.private-access declaration)))
        (if (%%class-is? generic-declaration jazz.Generic-Declaration)
            (receive (signature augmented-environment) (jazz.walk-parameters walker resume declaration environment parameters #t #t)
              (let* ((root? (root-dynamic-parameters? generic-declaration signature name parameters))
                     (new-declaration (jazz.new-specific-declaration name #f 'public 'uptodate '() declaration generic-declaration signature root?))
                     (body-environment (if root? augmented-environment (%%cons (jazz.new-nextmethod-variable 'nextmethod #f) augmented-environment))))
                (%%set-specific-declaration-body new-declaration
                                                 (jazz.walk-body walker resume new-declaration body-environment body))
                (%%set-declaration-source new-declaration form-src)
                new-declaration))
          (jazz.walk-error walker resume declaration form-src "Cannot find generic declaration for {s}" (%%cons name parameters)))))))


;;;
;;;; Class
;;;


(define jazz.class-modifiers
  '(((private protected package public) . public)
    ((abstract concrete) . concrete)
    ((deprecated undocumented uptodate) . uptodate)
    ((primitive native) . native)))

(define jazz.class-keywords
  '(metaclass extends implements attributes))


(define (jazz.parse-class walker resume declaration rest)
  (receive (access abstraction compatibility implementor rest) (jazz.parse-modifiers walker resume declaration jazz.class-modifiers rest)
    (let ((name (jazz.source-code (%%car rest)))
          (type jazz.Any)
          (rest (%%cdr rest)))
      (%%assert (%%symbol? name)
        (receive (metaclass-name ascendant-name interface-names attributes body) (jazz.parse-keywords jazz.class-keywords rest)
          (values name type access abstraction compatibility implementor metaclass-name ascendant-name interface-names attributes body))))))


(define (jazz.expand-class walker resume declaration environment form-src)
  (define (preprocess-meta body)
    (let ((metaclass (jazz.new-queue))
          (class (jazz.new-queue)))
      (define (expand-form-hack expr)
        (if (and (%%pair? (jazz.source-code expr))
                 (%%eq? (jazz.source-code (%%car (jazz.source-code expr))) 'form))
            (jazz.expand-macros walker resume declaration environment expr)
          expr))
      
      (define (preprocess expr)
        (let ((expr (expand-form-hack expr)))
          (cond ((and (%%pair? (jazz.source-code expr))
                      (%%eq? (jazz.source-code (%%car (jazz.source-code expr))) 'begin))
                 (for-each preprocess (%%cdr (jazz.source-code expr))))
                ((and (%%pair? (jazz.source-code expr))
                      (%%pair? (%%cdr (jazz.source-code expr)))
                      (%%eq? (jazz.source-code (%%cadr (jazz.source-code expr))) 'meta))
                 (jazz.enqueue metaclass (jazz.sourcify-if (%%cons (%%car (jazz.source-code expr)) (%%cddr (jazz.source-code expr))) expr)))
                (else
                 (jazz.enqueue class expr)))))
      
      (for-each preprocess body)
      (values (jazz.queue-list metaclass)
              (jazz.queue-list class))))
  
  (receive (name type access abstraction compatibility implementor metaclass-name ascendant-name interface-names attributes body) (jazz.parse-class walker resume declaration (%%cdr (jazz.source-code form-src)))
    (receive (metaclass-body class-body) (preprocess-meta body)
      (cond ((and (%%not-null? metaclass-body)
                  (%%specified? metaclass-name))
             (jazz.walk-error walker resume declaration form-src "Ambiguous use of both metaclass and meta keywords: {s}" name))
            ((or (%%specified? metaclass-name)
                 (jazz.core-class? name)
                 (%%unspecified? ascendant-name))
             (jazz.sourcify-if
               `(%class ,@(%%cdr (jazz.source-code form-src)))
               form-src))
            (else
             (let ((metaclass-name (%%string->symbol (%%string-append (%%symbol->string name) "~Class"))))
               `(begin
                  ,(jazz.sourcify-if
                     `(%class ,metaclass-name extends (:class ,ascendant-name)
                        ,@metaclass-body)
                     form-src)
                  ,(jazz.sourcify-if
                     `(%class ,access ,abstraction ,compatibility ,implementor ,name metaclass ,metaclass-name extends ,ascendant-name implements ,interface-names
                        ,@class-body)
                     form-src))))))))


(define (jazz.walk-%class-declaration walker resume declaration environment form-src)
  (receive (name type access abstraction compatibility implementor metaclass-name ascendant-name interface-names attributes body) (jazz.parse-class walker resume declaration (%%cdr (jazz.source-code form-src)))
    (%%assertion (%%class-is? declaration jazz.Module-Declaration) (jazz.walk-error walker resume declaration form-src "Classes can only be defined at the module level: {s}" name)
      ;; explicit test on Object-Class is to break circularity
      (receive (ascendant ascendant-relation ascendant-base) (jazz.lookup-ascendant walker resume declaration environment ascendant-name)
        (let ((metaclass (jazz.lookup-metaclass walker resume declaration environment ascendant metaclass-name))
              (interfaces (if (jazz.unspecified? interface-names) '() (map (lambda (interface-name) (jazz.lookup-reference walker resume declaration environment interface-name)) (jazz.listify interface-names)))))
          (let ((new-declaration (or (jazz.find-child-declaration declaration name)
                                     (jazz.new-class-declaration name type access compatibility attributes declaration implementor metaclass ascendant ascendant-relation ascendant-base interfaces))))
            (%%set-declaration-source new-declaration form-src)
            (let ((effective-declaration (jazz.add-declaration-child walker resume declaration new-declaration)))
              (jazz.setup-class-lookups effective-declaration)
              (let ((new-environment (%%cons effective-declaration environment)))
                (jazz.walk-declarations walker resume effective-declaration new-environment body)
                effective-declaration))))))))


(define (jazz.walk-%class walker resume declaration environment form-src)
  (receive (name type access abstraction compatibility implementor metaclass-name ascendant-name interface-names attributes body) (jazz.parse-class walker resume declaration (%%cdr (jazz.source-code form-src)))
    (%%assertion (%%class-is? declaration jazz.Module-Declaration) (jazz.walk-error walker resume declaration form-src "Classes can only be defined at the module level: {s}" name)
      (let* ((new-declaration (jazz.require-declaration declaration name))
             (new-environment (%%cons new-declaration environment))
             (ascendant-declaration (%%get-class-declaration-ascendant new-declaration)))
        (if (and (%%not ascendant-declaration) (%%neq? name 'Object))
            (jazz.walk-error walker resume declaration form-src "Class {s} does not specify an ascendant" name)
          (begin
            (%%set-namespace-declaration-body new-declaration (jazz.walk-namespace walker resume new-declaration new-environment body))
            (%%set-declaration-source new-declaration form-src)
            new-declaration))))))


(define (jazz.lookup-metaclass walker resume declaration environment ascendant metaclass-name)
  (if (or (jazz.unspecified? metaclass-name) (%%eq? metaclass-name 'Object-Class))
      #f
    (jazz.lookup-reference walker resume declaration environment metaclass-name)))


(define (jazz.lookup-ascendant walker resume declaration environment ascendant-name)
  (cond ((jazz.unspecified? ascendant-name)
         (values #f
                 #f
                 #f))
        ((and (%%pair? ascendant-name)
              (%%eq? (%%car ascendant-name) ':class))
         (let ((object-class (jazz.lookup-reference walker resume declaration environment 'Object-Class)))
           (let rec ((ascendant-name ascendant-name))
                (if (%%pair? ascendant-name)
                    (receive (decl relation base) (rec (%%cadr ascendant-name))
                      (values (if (%%eq? decl object-class)
                                  object-class
                                (or (jazz.effective-class-declaration-metaclass base) object-class))
                              (%%cons (%%car ascendant-name) relation)
                              base))
                  (let ((base (jazz.lookup-reference walker resume declaration environment ascendant-name)))
                    (values base '() base))))))
        (else
         (values (jazz.lookup-reference walker resume declaration environment ascendant-name)
                 #f
                 #f))))


(define (jazz.effective-class-declaration-metaclass class-declaration)
  (if (%%not class-declaration)
      #f
    (let ((class-declaration (jazz.resolve-binding class-declaration)))
      (or (%%get-category-declaration-metaclass class-declaration)
          (let ((ascendant (%%get-class-declaration-ascendant class-declaration)))
            (if (%%not ascendant)
                #f
              (jazz.effective-class-declaration-metaclass ascendant)))))))


;;;
;;;; Interface
;;;


(define jazz.interface-modifiers
  '(((private protected package public) . public)
    ((deprecated undocumented uptodate) . uptodate)
    ((primitive native) . native)))

(define jazz.interface-keywords
  '(metaclass extends attributes))


(define (jazz.parse-interface walker resume declaration rest)
  (receive (access compatibility implementor rest) (jazz.parse-modifiers walker resume declaration jazz.interface-modifiers rest)
    (let ((name (%%car rest))
          (type jazz.Any)
          (rest (%%cdr rest)))
      (%%assert (%%symbol? name)
        (receive (metaclass-name ascendant-names attributes body) (jazz.parse-keywords jazz.interface-keywords rest)
          (values name type access compatibility implementor metaclass-name ascendant-names attributes body))))))


(define (jazz.walk-interface-declaration walker resume declaration environment form-src)
  (let ((form (%%desourcify form-src)))
    (receive (name type access compatibility implementor metaclass-name ascendant-names attributes body) (jazz.parse-interface walker resume declaration (%%cdr form))
      (%%assertion (%%class-is? declaration jazz.Module-Declaration) (jazz.walk-error walker resume declaration form-src "Interfaces can only be defined at the module level: {s}" name)
        (let ((metaclass (if (or (jazz.unspecified? metaclass-name) (%%eq? metaclass-name 'Interface)) #f (jazz.lookup-reference walker resume declaration environment metaclass-name)))
              (ascendants (if (jazz.unspecified? ascendant-names) '() (map (lambda (ascendant-name) (jazz.lookup-reference walker resume declaration environment ascendant-name)) (jazz.listify ascendant-names)))))
          (let ((new-declaration (or (jazz.find-child-declaration declaration name)
                                     (jazz.new-interface-declaration name type access compatibility attributes declaration implementor metaclass ascendants))))
            (%%set-declaration-source new-declaration form-src)
            (let ((effective-declaration (jazz.add-declaration-child walker resume declaration new-declaration)))
              (jazz.setup-interface-lookups effective-declaration)
              (let ((new-environment (%%cons effective-declaration environment)))
                (jazz.walk-declarations walker resume effective-declaration new-environment body)
                effective-declaration))))))))


(define (jazz.walk-interface walker resume declaration environment form-src)
  (let ((form (%%desourcify form-src)))
    (receive (name type access compatibility implementor metaclass-name ascendant-names attributes body) (jazz.parse-interface walker resume declaration (%%cdr form))
      (%%assertion (%%class-is? declaration jazz.Module-Declaration) (jazz.walk-error walker resume declaration form-src "Interfaces can only be defined at the module level: {s}" name)
        (let* ((new-declaration (jazz.require-declaration declaration name))
               (new-environment (%%cons new-declaration environment)))
          (%%set-namespace-declaration-body new-declaration (jazz.walk-namespace walker resume new-declaration new-environment body))
          (%%set-declaration-source new-declaration form-src)
          new-declaration)))))


;;;
;;;; Slot
;;;


(define jazz.slot-modifiers
  '(((private protected package public) . #f)
    ((deprecated undocumented uptodate) . uptodate)))

(define jazz.slot-keywords
  '(initialize accessors getter setter))


(define jazz.slot-accessors-modifiers
  '(((private protected package public) . #f)
    ((final virtual chained override) . final)
    ((abstract concrete) . concrete)
    ((inline onsite) . inline)
    ((none generate explicit) . none)))


(define jazz.slot-accessor-modifiers
  '(((private protected package public) . #f)
    ((final virtual chained override) . #f)
    ((abstract concrete) . #f)
    ((inline onsite) . #f)
    ((none generate explicit) . #f)))


(define (jazz.parse-slot walker resume declaration form-src)
  (receive (access compatibility rest) (jazz.parse-modifiers walker resume declaration jazz.slot-modifiers (%%cdr (jazz.source-code form-src)))
    (let ((name (jazz.source-code (%%car rest))))
      (jazz.parse-specifier (%%cdr rest)
        (lambda (specifier rest)
          (receive (initialize accessors getter setter rest) (jazz.parse-keywords jazz.slot-keywords rest)
            (if (%%not-null? rest)
                (jazz.walk-error walker resume declaration form-src "Invalid slot definition: {s}" name)
              (values name specifier access compatibility initialize accessors getter setter))))))))


(define (jazz.expand-slot walker resume declaration environment form-src)
  (jazz.expand-slot-form walker resume declaration form-src '%slot))


(define (jazz.expand-slot-form walker resume declaration form-src symbol)
  (define (parse-accessors form slot-access)
    (receive (access propagation abstraction expansion generation rest) (jazz.parse-modifiers walker resume declaration jazz.slot-accessors-modifiers form)
      (if (%%not-null? rest)
          (jazz.walk-error walker resume declaration form-src "Invalid slot accessors definition: {s}" form)
        (values (or access slot-access) propagation abstraction expansion generation))))
  
  (define (parse-accessor slot-name default-access default-propagation default-abstraction default-expansion default-generation form prefix)
    (receive (access propagation abstraction expansion generation rest) (jazz.parse-modifiers walker resume declaration jazz.slot-accessor-modifiers form)
      (let ((generation (or generation default-generation)))
        (let ((name (cond ((%%null? rest)
                           (and (%%neq? generation 'none)
                                (%%string->symbol (%%string-append prefix (%%symbol->string slot-name)))))
                          ((%%null? (%%cdr rest))
                           (%%car rest))
                          (else
                           (jazz.walk-error walker resume declaration form-src "Invalid slot accessor definition: {s}" form)))))
          (values (or access default-access)
                  (or propagation default-propagation)
                  (or abstraction default-abstraction)
                  (or expansion default-expansion)
                  generation
                  name)))))
  
  (receive (name specifier access compatibility initialize accessors getter setter) (jazz.parse-slot walker resume declaration form-src)
    (let ((standardize
            (lambda (info)
              (let ((info (jazz.desourcify info)))
                (cond ((jazz.unspecified? info)
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
                   ,(jazz.sourcify-if
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


(define (jazz.walk-%slot-declaration walker resume declaration environment form-src)
  (let ((form (%%desourcify form-src)))
    (jazz.bind (name specifier access compatibility initialize getter-name setter-name) (%%cdr form)
      (%%assertion (%%class-is? declaration jazz.Class-Declaration) (jazz.walk-error walker resume declaration form-src "Slots can only be defined inside classes: {s}" name)
        (let ((type (if specifier (jazz.walk-specifier walker resume declaration environment specifier) jazz.Any))
              (new (if (%%eq? (%%car form) '%property) jazz.new-property-declaration jazz.new-slot-declaration)))
          (let ((new-declaration (or (jazz.find-child-declaration declaration name)
                                     (new name type access compatibility '() declaration #f getter-name setter-name))))
            (%%set-declaration-source new-declaration form-src)
            (let ((effective-declaration (jazz.add-declaration-child walker resume declaration new-declaration)))
              effective-declaration)))))))


(define (jazz.walk-%slot walker resume declaration environment form-src)
  (let ((form (%%desourcify form-src)))
    (jazz.bind (name specifier access compatibility initialize getter-name setter-name) (%%cdr form)
      (%%assertion (%%class-is? declaration jazz.Class-Declaration) (jazz.walk-error walker resume declaration form-src "Slots can only be defined inside classes: {s}" name)
        (let ((new-declaration (jazz.require-declaration declaration (%%cadr form))))
          (%%set-slot-declaration-initialize new-declaration
            (and (%%specified? initialize) (jazz.walk walker resume declaration environment initialize)))
          (%%when (%%class-is? new-declaration jazz.Property-Declaration)
            (let ((allocate? (%%neq? (%%get-lexical-binding-type new-declaration) jazz.Void)))
              (%%set-property-declaration-getter new-declaration
                (jazz.walk walker resume declaration environment
                  (cond (getter-name
                          `(lambda (self)
                             (with-self
                               (,getter-name))))
                        (allocate?
                          `(lambda (self)
                             (with-self ,name)))
                        (else #f))))
              (%%set-property-declaration-setter new-declaration
                (let ((value (jazz.generate-symbol "val")))
                  (jazz.walk walker resume declaration environment
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
;;;; Property
;;;


(define (jazz.expand-property walker resume declaration environment form-src)
  (jazz.expand-slot-form walker resume declaration form-src '%property))


;;;
;;;; Method
;;;


(define jazz.method-modifiers
  '(((private protected package public) . protected)
    ((deprecated undocumented uptodate) . uptodate)
    ((final virtual chained override) . final)
    ((abstract concrete) . concrete)
    ((inline onsite) . onsite)
    ;; quicky
    ((remote notremote) . notremote)
    ((synchronized notsynchronized) . notsynchronized)))


(define (jazz.parse-method walker resume declaration rest)
  (receive (access compatibility propagation abstraction expansion remote synchronized rest) (jazz.parse-modifiers walker resume declaration jazz.method-modifiers rest)
    (%%assertion (and (%%pair? rest) (%%pair? (jazz.source-code (%%car rest)))) (jazz.walk-error walker resume declaration #f "Ill-formed method in {a}: {s}" (%%get-lexical-binding-name (%%get-declaration-toplevel declaration)) (%%cons 'method (jazz.desourcify-all rest)))
      (let ((name (jazz.source-code (%%car (jazz.source-code (%%car rest)))))
            (parameters (jazz.wrap-parameters (%%cdr (%%desourcify (%%car rest))))))
        (jazz.parse-specifier (%%cdr rest)
          (lambda (specifier body)
            (let ((effective-body (if (%%null? body) (%%list (%%list 'unspecified)) body)))
              (values name specifier access compatibility propagation abstraction expansion remote synchronized parameters effective-body))))))))


(define (jazz.walk-method-declaration walker resume declaration environment form-src)
  (define (find-root-declaration name)
    (let* ((next-declaration (jazz.lookup-declaration declaration name jazz.private-access declaration))
           (root-declaration (and next-declaration (or (%%get-method-declaration-root next-declaration) next-declaration))))
      (if (and root-declaration (%%eq? declaration (%%get-declaration-parent root-declaration)))
          #f
        root-declaration)))
  
  (receive (name specifier access compatibility propagation abstraction expansion remote synchronized parameters body) (jazz.parse-method walker resume declaration (%%cdr (jazz.source-code form-src)))
    (%%assertion (%%class-is? declaration jazz.Category-Declaration) (jazz.walk-error walker resume declaration form-src "Methods can only be defined inside categories: {s}" name)
      (let ((type (if specifier (jazz.new-function-type '() '() '() #f (jazz.walk-specifier walker resume declaration environment specifier)) jazz.Procedure))
            (inline? (and (%%eq? expansion 'inline) (%%eq? abstraction 'concrete))))
        (receive (signature augmented-environment)
            ;; yuck. to clean
            (if inline?
                (jazz.walk-parameters walker resume declaration environment parameters #t #t)
              (values
                (jazz.walk-parameters walker resume declaration environment parameters #t #f)
                (jazz.unspecified)))
          (let* ((root-declaration (find-root-declaration name))
                 (new-declaration (or (jazz.find-child-declaration declaration name)
                                      (jazz.new-method-declaration name type access compatibility '() declaration root-declaration propagation abstraction expansion remote synchronized signature))))
            (%%set-declaration-source new-declaration form-src)
            (let ((effective-declaration (jazz.add-declaration-child walker resume declaration new-declaration)))
              (%%when inline?
                (%%set-method-declaration-signature effective-declaration signature)
                (%%set-method-declaration-body effective-declaration
                  (jazz.walk walker resume effective-declaration augmented-environment
                    `(with-self ,@body))))
              effective-declaration)))))))


(define (jazz.walk-method walker resume declaration environment form-src)
  (receive (name specifier access compatibility propagation abstraction expansion remote synchronized parameters body) (jazz.parse-method walker resume declaration (%%cdr (jazz.source-code form-src)))
    (%%assertion (%%class-is? declaration jazz.Category-Declaration) (jazz.walk-error walker resume declaration form-src "Methods can only be defined inside categories: {s}" name)
      (let* ((new-declaration (jazz.lookup-declaration declaration name jazz.private-access declaration))
             (category-declaration (%%get-declaration-parent new-declaration))
             (root-method-declaration (%%get-method-declaration-root new-declaration))
             (root-method-propagation (and root-method-declaration (%%get-method-declaration-propagation root-method-declaration)))
             (root-category-declaration (and root-method-declaration (%%get-declaration-parent root-method-declaration))))
        (cond ((and root-category-declaration (%%eq? root-method-propagation 'final))
               (jazz.walk-error walker resume declaration form-src "Cannot redefine method: {s}" name))
              ((and root-category-declaration (%%memq root-method-propagation '(virtual chained)) (%%neq? propagation 'override))
               (case propagation
                 ((virtual chained)
                  (jazz.walk-error walker resume declaration form-src "Method is already virtual: {s}" name))
                 ((final)
                  (jazz.walk-error walker resume declaration form-src "Cannot finalize virtual method: {s}" name))))
              ((and (%%not root-category-declaration) (%%eq? propagation 'override))
               (jazz.walk-error walker resume declaration form-src "Cannot find root method: {s}" name))
              ((and (%%not root-category-declaration) (%%class-is? category-declaration jazz.Interface-Declaration) (%%neq? propagation 'virtual))
               (jazz.walk-error walker resume declaration form-src "Interface method must be virtual: {s}" name))
              (else
               (if (and (jazz.analysis-mode?)
                        (%%eq? (%%get-method-declaration-propagation new-declaration) 'final))
                   (let ((data (jazz.get-analysis-data (%%get-declaration-locator new-declaration))))
                     (%%set-analysis-data-declaration-references data '())))
               (receive (signature augmented-environment) (jazz.walk-parameters walker resume declaration environment parameters #t #t)
                 (let ((body-expression
                         (cond (root-category-declaration
                                 (jazz.walk walker resume new-declaration (%%cons (jazz.new-nextmethod-variable 'nextmethod (%%get-lexical-binding-type root-method-declaration)) augmented-environment) `(with-self ,@body)))
                               ((%%eq? abstraction 'concrete)
                                (jazz.walk walker resume new-declaration augmented-environment `(with-self ,@body)))
                               (else
                                #f))))
                   (%%when (%%not (and (%%eq? expansion 'inline) (%%eq? abstraction 'concrete)))
                     (%%set-method-declaration-signature new-declaration signature)
                     (%%set-method-declaration-body new-declaration body-expression))
                   (%%set-declaration-source new-declaration form-src)
                   new-declaration))))))))


;; quick not elegant solution to wrap with-self around parameter code
(define (jazz.wrap-parameters parameters)
  (let ((queue (jazz.new-queue)))
    (let iter ((scan parameters))
      (cond ((%%null? scan))
            ((%%symbol? scan)
             (jazz.enqueue-list queue scan))
            (else
             (let ((parameter (%%car scan)))
               (if (%%pair? parameter)
                   (if (jazz.specifier? (%%car scan))
                       (jazz.enqueue queue parameter)
                     (if (%%keyword? (%%car parameter))
                         (jazz.parse-specifier (%%cddr parameter)
                           (lambda (specifier rest)
                             (let ((specifier-list (if specifier (%%list specifier) '())))
                               (jazz.enqueue queue `(,(%%car parameter) ,(%%cadr parameter) ,@specifier-list (with-self ,(%%car rest)))))))
                       (jazz.parse-specifier (%%cdr parameter)
                         (lambda (specifier rest)
                           (let ((specifier-list (if specifier (%%list specifier) '())))
                             (jazz.enqueue queue `(,(%%car parameter) ,@specifier-list (with-self ,(%%car rest)))))))))
                 (jazz.enqueue queue parameter)))
             (iter (%%cdr scan)))))
    (jazz.queue-list queue)))


;;;
;;;; Cast
;;;


(define (jazz.walk-cast walker resume declaration environment form-src)
  (let ((form (%%desourcify form-src)))
    (let ((specifier (%%cadr form))
          (expression (%%car (%%cddr form))))
      (jazz.new-cast (jazz.walk-specifier walker resume declaration environment specifier)
                     (jazz.walk walker resume declaration environment expression)))))


;;;
;;;; Construct
;;;


(define (jazz.walk-construct walker resume declaration environment form-src)
  (let ((form (%%desourcify form-src)))
    (let ((class (%%cadr form))
          (values (%%cddr form)))
      (jazz.new-construct (jazz.walk walker resume declaration environment class)
                          (jazz.walk-list walker resume declaration environment values)))))


;;;
;;;; With Self
;;;


(define (jazz.walk-with-self walker resume declaration environment form-src)
  (let ((form (%%desourcify form-src)))
    (let ((new-environment (%%cons (jazz.new-self-binding #f) environment)))
      (jazz.new-with-self
        (let ((body (%%cdr form)))
          (jazz.walk-body walker resume declaration new-environment body))))))


;;;
;;;; With Dynamic Self
;;;


(define (jazz.parse-with-dynamic-self form)
  (let ((code (%%car form))
        (body (%%cdr form)))
    (values code body)))


(define (jazz.walk-with-dynamic-self-declaration walker resume declaration environment form-src)
  (receive (code body) (jazz.parse-with-dynamic-self (%%cdr (%%desourcify form-src)))
    (jazz.walk-declarations walker resume declaration environment body)))


(define (jazz.walk-with-dynamic-self walker resume declaration environment form-src)
  (receive (code body) (jazz.parse-with-dynamic-self (%%cdr (%%desourcify form-src)))
    (let ((new-environment (%%cons (jazz.new-dynamic-self-binding #f code) environment)))
      (jazz.new-with-dynamic-self
        code
        (jazz.walk-list walker resume declaration new-environment body)))))


;;;
;;;; With Local Variables
;;;


(define (jazz.parse-with-local-variables form)
  (let ((variables (%%car form))
        (body (%%cdr form)))
    (values variables body)))


(define (jazz.walk-with-local-variables-declaration walker resume declaration environment form-src)
  (receive (variables body) (jazz.parse-with-local-variables (%%cdr (%%desourcify form-src)))
    (jazz.walk-declarations walker resume declaration environment body)))


(define (jazz.walk-with-local-variables walker resume declaration environment form-src)
  (receive (variables body) (jazz.parse-with-local-variables (%%cdr (%%desourcify form-src)))
    (let ((new-environment (%%cons (jazz.new-walk-frame (map (lambda (variable) (jazz.new-local-variable-binding #f variable)) variables)) environment)))
      (jazz.new-begin form-src (jazz.walk-list walker resume declaration new-environment body)))))


;;;
;;;; Proclaim
;;;


(jazz.define-method (jazz.validate-proclaim (jazz.Jazz-Walker walker) resume declaration environment form-src)
  (if (and (%%not (%%class-is? declaration jazz.Module-Declaration))
           (%%not (%%class-is? declaration jazz.Category-Declaration)))
      (jazz.walk-error walker resume declaration form-src "For now, proclaim can only be used at the module or category level")))


;;;
;;;; Remotable Stub
;;;


(define jazz.remotable-stub-modifiers
  '(((private protected package public) . public)))

(define jazz.remotable-stub-keywords
  '(extends))


(define (jazz.parse-remotable-stub walker resume declaration rest)
  (receive (access rest) (jazz.parse-modifiers walker resume declaration jazz.remotable-stub-modifiers rest)
    (let ((name (%%car rest))
          (rest (%%cdr rest)))
      (receive (ascendant-name body) (jazz.parse-keywords jazz.remotable-stub-keywords rest)
        (values name #f access ascendant-name body)))))


(define jazz.method-stub-modifiers
  '(((private protected package public) . private)
    ((post exec call) . call)
    ((reference value) . reference)))


(define (jazz.parse-method-stub walker resume declaration rest)
  (receive (access invocation passage rest) (jazz.parse-modifiers walker resume declaration jazz.method-stub-modifiers rest)
    (let* ((signature (%%car rest))
           (name (%%car signature))
           (parameters (%%cdr signature))
           (body (%%cdr rest)))
      (values name jazz.Any access invocation passage parameters body))))


(define (jazz.expand-remotable-stub walker resume declaration environment . rest)
  (receive (name type stub-access ascendant-name body) (jazz.parse-remotable-stub walker resume declaration rest)
    (define (add name suffix)
      (%%string->symbol (%%string-append (%%symbol->string name) suffix)))
    
    (define (parse-parameters params)
      (let ((parameters (jazz.new-queue))
            (positional (jazz.new-queue)))
        (define (encode parameter)
          (%%string->symbol (%%string-append "__" (%%symbol->string parameter))))
        
        (let iter ((scan params))
             (cond ((%%null? scan)
                    (values (jazz.queue-list parameters) (jazz.queue-list positional) #f))
                   ((%%symbol? scan)
                    (let ((rest (encode scan)))
                      (jazz.enqueue-list parameters rest)
                      (values (jazz.queue-list parameters) (jazz.queue-list positional) rest)))
                   (else
                    (let ((parameter (encode (%%car scan))))
                      (jazz.enqueue parameters parameter)
                      (jazz.enqueue positional parameter)
                      (iter (%%cdr scan))))))))
    
    (define (parse-value-keyword name passage)
      (case passage
        ((reference)
         #f)
        ((value)
         (let* ((str (%%symbol->string name))
                (len (%%string-length str)))
           (%%assert (and (%%fx> len 4) (%%string=? (%%substring str 0 4) "get-"))
             (jazz.string->keyword (%%substring str 4 len)))))))
    
    (let ((interface-class (add name "-Stub-Interface"))
          (stub-interface (add name "-Stub"))
          (local-class (add name "-Local-Proxy"))
          (remote-class (add name "-Remote-Proxy"))
          (proxies (jazz.new-queue))
          (values (jazz.new-queue))
          (locals (jazz.new-queue))
          (remotes (jazz.new-queue)))
      (for-each (lambda (method-form)
                  (%%assert (%%eq? (%%car method-form) 'method)
                    (receive (name type access invocation passage parameters body) (jazz.parse-method-stub walker resume declaration (%%cdr method-form))
                      (receive (parameters positional rest) (parse-parameters parameters)
                        (let ((invoker (case invocation ((post) 'post-remote) ((exec) 'exec-remote) ((call) 'call-remote)))
                              (dispatch (%%string->symbol (%%string-append (%%symbol->string name) "~")))
                              (local-result (case invocation ((post exec) '((unspecified))) ((call) '())))
                              (value-keyword (parse-value-keyword name passage)))
                          (jazz.enqueue proxies `(method ,access virtual abstract (,name ,@parameters)))
                          (%%when value-keyword
                            (jazz.enqueue values value-keyword)
                            (jazz.enqueue values `(,name)))
                          (jazz.enqueue locals `(method override (,name ,@parameters)
                                                  ,@(cond ((%%not-null? body) body)
                                                          (rest `((apply (~ ,name object) ,@positional ,rest) ,@local-result))
                                                          (else `((,dispatch object ,@positional) ,@local-result)))))
                          (jazz.enqueue remotes `(method override (,name ,@parameters)
                                                   ,(let ((call (if rest `(apply ,invoker ',name self ,@positional ,rest) `(,invoker ',name self ,@positional))))
                                                      (if value-keyword
                                                          `(proxy-value ,value-keyword (lambda () ,call))
                                                        call)))))))))
                body)
      (let* ((values-list (jazz.queue-list values))
             (values-method
               (if (%%null? values-list)
                   '()
                 `((method override (proxy-values)
                     (append (list ,@values-list)
                             (nextmethod)))))))
        `(begin
           (class package ,interface-class extends ,(if (jazz.specified? ascendant-name) (add ascendant-name "-Stub-Interface") 'Stub-Interface)
             (method override (local-class)
               ,local-class)
             (method override (remote-class)
               ,remote-class))
           (interface ,stub-access ,stub-interface extends ,(if (jazz.specified? ascendant-name) (add ascendant-name "-Stub") 'Remotable-Stub) metaclass ,interface-class
             ,@(jazz.queue-list proxies))
           (class package ,local-class extends ,(if (jazz.specified? ascendant-name) (add ascendant-name "-Local-Proxy") 'Local-Proxy) implements ,stub-interface
             (method override (stub-reference)
               (reference ,stub-interface))
             ,@values-method
             ,@(jazz.queue-list locals))
           (class package ,remote-class extends ,(if (jazz.specified? ascendant-name) (add ascendant-name "-Remote-Proxy") 'Remote-Proxy) implements ,stub-interface
             (method override (stub-reference)
               (reference ,stub-interface))
             ,@(jazz.queue-list remotes)))))))


;;;
;;;; CoExternal
;;;


;; (com-external 22 VT_HRESULT (OpenDatabase (in VT_BSTR) (in VT_VARIANT) (in VT_VARIANT) (in VT_VARIANT) (out VT_PTR VT_UNKNOWN)))
#;
(define (jazz.expand-com-external walker resume declaration environment offset result-type signature)
  (let ((name (%%car signature))
        (resolve-declaration (lambda (type) (if (%%symbol? type)
                                                (jazz.resolve-c-type-reference walker resume declaration environment type)
                                              (jazz.walk-error walker resume declaration #f "Illegal parameter type in com-external {s}: {s}" (%%car signature) type))))
        (fix-locator (lambda (declaration) (if (%%eq? (%%get-c-type-declaration-kind declaration) 'type)
                                               (string->symbol (%%string-append (%%symbol->string (%%get-declaration-locator declaration)) "*"))
                                             (%%get-declaration-locator declaration)))))
    ;; we assume coparam-types are symbols that exactly match the c type
    (let ((resolved-result (resolve-declaration result-type))
          (resolved-params (map resolve-declaration (map cadr (%%cdr signature))))
          (lowlevel-name (%%string->symbol (%%string-append (%%symbol->string name) "$"))))
      #; ;; debug
      (%%apply jazz.debug name resolved-result resolved-params)
      (if (jazz.every? (lambda (resolved) (%%class-is? resolved jazz.C-Type-Declaration)) (%%cons resolved-result resolved-params))
          `(definition ,lowlevel-name
             (c-function ,(%%cons 'IUnknown* (map fix-locator resolved-params))
                         ,(%%get-declaration-locator resolved-result)
                         ,(string-append
                            "{typedef "
                            (jazz.->string (%%get-lexical-binding-name resolved-result))
                            " (*ProcType)(IUnknown*"
                            (%%apply string-append (let iter-arg ((resolved-params resolved-params))
                                                        (if (%%pair? resolved-params)
                                                            (%%cons ", " (%%cons (jazz.->string (%%get-lexical-binding-name (%%car resolved-params)))
                                                                                 (iter-arg (%%cdr resolved-params))))
                                                          '())))
                            "); ProcType fn = (*(ProcType**)___arg1)["
                            (%%number->string offset)
                            "]; ___result = (*fn)(___arg1"
                            (%%apply string-append (let iter-arg ((resolved-params resolved-params)
                                                                  (order 2))
                                                        (if (%%pair? resolved-params)
                                                            (cons (if (%%eq? (%%get-c-type-declaration-kind (%%car resolved-params)) 'type) ", *___arg" ", ___arg")
                                                                  (%%cons (%%number->string order)
                                                                          (iter-arg (%%cdr resolved-params) (%%fx+ order 1))))
                                                          '())))
                            ");}")))))))


;;;
;;;; CoExternal
;;;


;; (com-external 22 VT_HRESULT (OpenDatabase (in VT_BSTR) (in VT_VARIANT) (in VT_VARIANT) (in VT_VARIANT) (out VT_PTR VT_UNKNOWN)))
(define (jazz.expand-com-external walker resume declaration environment result-type signature com-interface offset)
  (let* ((name (%%car signature))
         (param-types (map cadr (%%cdr signature)))
         (resolve-declaration (lambda (type) (if (%%symbol? type)
                                                 (jazz.resolve-c-type-reference walker resume declaration environment type)
                                               (jazz.walk-error walker resume declaration #f "Illegal parameter type in com-external {s}: {s}" name type)))))
    (let ((resolved-result (resolve-declaration result-type))
          (resolved-params (map resolve-declaration param-types))
          (resolved-directions (map car (%%cdr signature)))
          (lowlevel-name (%%string->symbol (%%string-append (%%symbol->string name) "$"))))
      (let ((hresult? (%%eq? (%%get-declaration-locator resolved-result) 'jazz.platform.windows.com.HRESULT)))
        (if (jazz.every? (lambda (resolved) (%%class-is? resolved jazz.C-Type-Declaration)) (%%cons resolved-result resolved-params))
            `(begin
               (definition ,lowlevel-name ,(jazz.emit-com-function offset result-type resolved-result param-types resolved-params))
               (definition public ,name ,(jazz.emit-com-external hresult? lowlevel-name resolved-params resolved-directions com-interface))))))))


(define (jazz.emit-com-function offset result-type resolved-result param-types resolved-params)
  (define (fix-locator type declaration)
    (if (%%eq? (%%get-c-type-declaration-kind declaration) 'type)
        (%%string->symbol (%%string-append (%%symbol->string type) "*"))
      type))
  
  ;; we assume lexical-binding-name exactly matches the c type
  `(c-function ,(%%cons 'IUnknown* (map fix-locator param-types resolved-params))
               ,result-type
               ,(string-append
                  "{typedef "
                  (jazz.->string (%%get-lexical-binding-name resolved-result))
                  " (*ProcType)(IUnknown*"
                  (%%apply string-append (let iter ((resolved-params resolved-params))
                                              (if (%%pair? resolved-params)
                                                  (%%cons ", " (%%cons (jazz.->string (%%get-lexical-binding-name (%%car resolved-params)))
                                                                       (iter (%%cdr resolved-params))))
                                                '())))
                  "); ProcType fn = (*(ProcType**)___arg1)["
                  (%%number->string offset)
                  "]; ___result = (*fn)(___arg1"
                  (%%apply string-append (let iter ((resolved-params resolved-params)
                                                    (order 2))
                                              (if (%%pair? resolved-params)
                                                  (cons (if (%%eq? (%%get-c-type-declaration-kind (%%car resolved-params)) 'type) ", *___arg" ", ___arg")
                                                        (%%cons (%%number->string order)
                                                                (iter (%%cdr resolved-params) (%%fx+ order 1))))
                                                '())))
                  ");}")))


(define (jazz.emit-com-external hresult? lowlevel-name resolved-params resolved-directions com-interface)
  (define (generate-in resolved-param resolved-direction order)
    (if (%%eq? resolved-direction 'out)
        #f
      (%%string->symbol (%%string-append "in$" (%%number->string order)))))
  
  (define (generate-low resolved-param resolved-direction order)
    (%%string->symbol (%%string-append "low$" (%%number->string order))))
  
  (define (generate-out resolved-param resolved-direction order)
    (if (%%eq? resolved-direction 'in)
        #f
      (%%string->symbol (%%string-append "out$" (%%number->string order)))))
  
  (define (generate-encode/enref resolved-param resolved-direction order)
    (let ((binding (generate-low resolved-param resolved-direction order))
          (encode/enref (get-cotype-encode/enref resolved-param))
          (value (if (%%eq? resolved-direction 'out)
                     (get-cotype-default-value resolved-param)
                   (generate-in resolved-param resolved-direction order))))
      (if encode/enref
          `(,binding (,encode/enref ,value))
        `(,binding ,value))))
  
  (define (generate-ref resolved-param resolved-direction order)
    (if (%%eq? resolved-direction 'in)
        #f
      (let ((binding (generate-out resolved-param resolved-direction order))
            (ref (get-cotype-ref resolved-param))
            (value (generate-low resolved-param resolved-direction order)))
        (if ref
            `(,binding (,ref ,value))
          `(,binding ,value)))))
  
  (define (generate-free resolved-param resolved-direction order)
    (let ((free (get-cotype-free resolved-param))
          (value (generate-low resolved-param resolved-direction order)))
      (if free
          `(,free ,value)
        #f)))
  
  (define (generate-cotype-transform generator)
    (let iter ((resolved-params resolved-params)
               (resolved-directions resolved-directions)
               (order 1))
         (if (%%pair? resolved-directions)
             (let ((generated (generator (%%car resolved-params) (%%car resolved-directions) order)))
               (if generated
                   (cons generated (iter (%%cdr resolved-params) (%%cdr resolved-directions) (%%fx+ order 1)))
                 (iter (%%cdr resolved-params) (%%cdr resolved-directions) (%%fx+ order 1))))
           '())))
  
  (let ((out-list (generate-cotype-transform generate-out)))
    `(lambda (coptr ,@(generate-cotype-transform generate-in))
               (let (,@(generate-cotype-transform generate-encode/enref))
                 (let ((result (,lowlevel-name coptr ,@(generate-cotype-transform generate-low))))
                   ,(if hresult?
                        (if com-interface
                            `(validate-hresult2 result coptr ,com-interface)
                          `(validate-hresult result))
                      '(begin))
                   (let (,@(generate-cotype-transform generate-ref))
                     (begin
                       ,@(generate-cotype-transform generate-free))
                     ,(if hresult?
                          (case (%%length out-list)
                            ((0)
                             '(unspecified))
                            ((1)
                             (%%car out-list))
                            (else
                             `(values ,@out-list)))
                        (if (%%fx= (%%length out-list) 0)
                            'result
                          `(values result ,@out-list)))))))))


(define (get-cotype-default-value cotype)
  (case (%%get-declaration-locator cotype)
    ((jazz.platform.windows.com.BSTR) (error "cotype BSTR has no default value"))
    ((jazz.platform.windows.com.BSTR*) #f)
    ((jazz.platform.windows.com.GUID) (error "cotype GUID has no default value"))
    ((jazz.platform.windows.com.GUID*) #f)
    ((jazz.platform.windows.com.VARIANT_BOOL) (error "cotype VARIANT_BOOL has no default value"))
    ((jazz.platform.windows.com.VARIANT_BOOL*) #f)
    ((jazz.platform.windows.com.VARIANT) (error "cotype VARIANT has no default value"))
    ((jazz.platform.windows.com.VARIANT*) '())
    ((jazz.platform.windows.com.IUnknown*) (error "cotype IUnknown* has no default value"))
    ((jazz.platform.windows.com.IUnknown**) #f)
    ((jazz.platform.windows.WinTypes.INT*) 0)
    ((jazz.platform.windows.WinTypes.LONG*) 0)
    (else 0)))


(define (get-cotype-encode/enref cotype)
  (case (%%get-declaration-locator cotype)
    ((jazz.platform.windows.com.BSTR) 'BSTR-encode)
    ((jazz.platform.windows.com.BSTR*) 'BSTR*-enref)
    ((jazz.platform.windows.com.GUID) 'GUID-encode)
    ((jazz.platform.windows.com.GUID*) 'GUID-encode)
    ((jazz.platform.windows.com.VARIANT_BOOL) 'VARIANT_BOOL-encode)
    ((jazz.platform.windows.com.VARIANT_BOOL*) 'VARIANT_BOOL-enref)
    ((jazz.platform.windows.com.VARIANT) 'VARIANT-encode)
    ((jazz.platform.windows.com.VARIANT*) 'VARIANT-encode)
    ((jazz.platform.windows.com.IUnknown*) #f)
    ((jazz.platform.windows.com.IUnknown**) 'IUnknown*-enref)
    ((jazz.platform.windows.WinTypes.INT*) 'INT-enref)
    ((jazz.platform.windows.WinTypes.LONG*) 'LONG-enref)
    (else #f)))


(define (get-cotype-ref cotype)
  (case (%%get-declaration-locator cotype)
    ((jazz.platform.windows.com.BSTR) 'BSTR-ref)
    ((jazz.platform.windows.com.BSTR*) 'BSTR*-ref)
    ((jazz.platform.windows.com.GUID) 'GUID-ref)
    ((jazz.platform.windows.com.GUID*) 'GUID-ref)
    ((jazz.platform.windows.com.VARIANT_BOOL) 'VARIANT_BOOL-decode)
    ((jazz.platform.windows.com.VARIANT_BOOL*) 'VARIANT_BOOL*-ref)
    ((jazz.platform.windows.com.VARIANT) 'VARIANT-ref)
    ((jazz.platform.windows.com.VARIANT*) 'VARIANT-ref)
    ((jazz.platform.windows.com.IUnknown*) #f)
    ((jazz.platform.windows.com.IUnknown**) 'IUnknown**-ref)
    ((jazz.platform.windows.WinTypes.INT*) 'INT*-ref)
    ((jazz.platform.windows.WinTypes.LONG*) 'LONG*-ref)
    (else #f)))


(define (get-cotype-free cotype)
  (case (%%get-declaration-locator cotype)
    ((jazz.platform.windows.com.BSTR) 'BSTR-free)
    ((jazz.platform.windows.com.BSTR*) 'BSTR*-free)
    ((jazz.platform.windows.com.GUID) 'GUID-free)
    ((jazz.platform.windows.com.GUID*) 'GUID-free)
    ((jazz.platform.windows.com.VARIANT_BOOL) #f)
    ((jazz.platform.windows.com.VARIANT_BOOL*) 'VARIANT_BOOL*-free)
    ((jazz.platform.windows.com.VARIANT) 'VARIANT-decode)
    ((jazz.platform.windows.com.VARIANT*) 'VARIANT-decode)
    ((jazz.platform.windows.com.IUnknown*) #f)
    ((jazz.platform.windows.com.IUnknown**) 'IUnknown**-free)
    ((jazz.platform.windows.WinTypes.INT*) 'INT*-free)
    ((jazz.platform.windows.WinTypes.LONG*) 'LONG*-free)
    (else #f)))


;;;
;;;; Assert
;;;


(define (jazz.expand-assert walker resume declaration environment form-src)
  (jazz.sourcify-if
    ;; we really want assertions in release and not in a new distribution safety
    (jazz.expand-assert-test #t form-src)
    form-src))


(define (jazz.expand-assertion walker resume declaration environment form-src)
  (jazz.sourcify-if
    ;; we really want assertions in release and not in a new distribution safety
    (jazz.expand-assertion-test #t form-src)
    form-src))


(define (jazz.expand-debug-assert walker resume declaration environment form-src)
  (jazz.sourcify-if
    (jazz.expand-assert-test jazz.debug-user? form-src)
    form-src))


(define (jazz.expand-debug-assertion walker resume declaration environment form-src)
  (jazz.sourcify-if
    (jazz.expand-assertion-test jazz.debug-user? form-src)
    form-src))


(define (jazz.expand-assert-test test? src)
  (let ((assertion (%%cadr (jazz.source-code src)))
        (body (%%cddr (jazz.source-code src))))
    (let ((message (let ((port (open-output-string)))
                     (display "Assertion " port)
                     (write (%%desourcify assertion) port)
                     (display " failed" port)
                     (get-output-string port))))
      (jazz.expand-assertion-body test? assertion (%%list 'error message) body))))


(define (jazz.expand-assertion-test test? src)
  (let ((assertion (%%cadr (jazz.source-code src)))
        (action (%%car (%%cddr (jazz.source-code src))))
        (body (%%cdr (%%cddr (jazz.source-code src)))))
    (jazz.expand-assertion-body test? assertion action body)))


(define (jazz.expand-assertion-body test? assertion action body)
  (let ((body (if (%%not-null? body) body '((unspecified)))))
    (if test?
        `(if (not ,assertion)
             ,action
           ,(jazz.simplify-begin
              `(begin
                 ,@body)))
      (jazz.simplify-begin `(begin ,@body)))))


;;;
;;;; Declare
;;;


(define (jazz.walk-declare walker resume declaration environment form-src)
  (let ((form (%%desourcify form-src)))
    (let ((declarations (%%cdr form)))
      (jazz.new-declare declarations))))


;;;
;;;; C-Include
;;;


(define (jazz.walk-c-include walker resume declaration environment form-src)
  (let ((form (%%desourcify form-src)))
    (jazz.bind (name) (%%cdr form)
      (jazz.new-c-include name))))


;;;
;;;; C-Declare
;;;


(define (jazz.walk-c-declare walker resume declaration environment form-src)
  (let ((form (%%desourcify form-src)))
    (jazz.bind (code) (%%cdr form)
      (jazz.new-c-declare code))))


;;;
;;;; C-Named-Declare
;;;


(define jazz.c-named-declare-modifiers
  '(((private protected package public) . public)
    ((deprecated undocumented uptodate) . uptodate)))


(define (jazz.parse-c-named-declare walker resume declaration rest)
  (receive (access compatibility rest) (jazz.parse-modifiers walker resume declaration jazz.c-named-declare-modifiers rest)
    (jazz.bind (name code) rest
      (let ((type jazz.Any))
        (values name type access compatibility code)))))


(define (jazz.walk-c-named-declare-declaration walker resume declaration environment form-src)
  (receive (name type access compatibility code) (jazz.parse-c-named-declare walker resume declaration (%%cdr (%%desourcify form-src)))
    (let ((new-declaration (or (jazz.find-child-declaration declaration name)
                               (jazz.new-c-named-declare-declaration name type access compatibility '() declaration code))))
        (let ((effective-declaration (jazz.add-declaration-child walker resume declaration new-declaration)))
          effective-declaration))))


(define (jazz.resolve-c-named-declare-reference walker resume declaration environment symbol)
  (let ((c-named-declare-declaration (jazz.lookup-reference walker resume declaration environment symbol)))
    (if (%%class-is? c-named-declare-declaration jazz.C-Named-Declare-Declaration)
        c-named-declare-declaration
      (jazz.walk-error walker resume declaration #f "{s} did not resolve to a c-named-declare: {s}" symbol (%%get-declaration-locator c-named-declare-declaration)))))


(define (jazz.walk-c-named-declare walker resume declaration environment form-src)
  (let ((form (%%desourcify form-src)))
    (receive (name type access compatibility code) (jazz.parse-c-named-declare walker resume declaration (%%cdr form))
      (let ((new-declaration (jazz.require-declaration declaration name)))
        new-declaration))))


;;;
;;;; C-Initialize
;;;


(define (jazz.walk-c-initialize walker resume declaration environment form-src)
  (let ((form (%%desourcify form-src)))
    (jazz.bind (code) (%%cdr form)
      (jazz.new-c-initialize code))))


;;;
;;;; C-Type
;;;


(define jazz.c-type-modifiers
  '(((private protected package public) . public)
    ((deprecated undocumented uptodate) . uptodate)))


(define (jazz.parse-c-type walker resume declaration rest)
  (receive (access compatibility rest) (jazz.parse-modifiers walker resume declaration jazz.c-type-modifiers rest)
    (jazz.bind (name c-type . conversions) rest
      (let ((type jazz.Any))
        (if (%%null? conversions)
            (values name type access compatibility c-type #f #f #f)
          (jazz.bind (c-to-scheme scheme-to-c declare) conversions
            (values name type access compatibility c-type c-to-scheme scheme-to-c declare)))))))


(define (jazz.walk-c-type-declaration walker resume declaration environment form-src)
  (receive (name type access compatibility c-type c-to-scheme scheme-to-c declare) (jazz.parse-c-type walker resume declaration (%%cdr (%%desourcify form-src)))
    (if (%%class-is? declaration jazz.Module-Declaration)
        (receive (kind expansion base-type-declaration inclusions) (jazz.resolve-c-type walker resume declaration environment c-type)
          (let ((inclusions (if declare
                                (if (%%string? expansion)
                                    (%%cons (jazz.resolve-c-named-declare-reference walker resume declaration environment declare) inclusions)
                                  (jazz.walk-error walker resume declaration form-src "{s} defined with c-to-scheme and scheme-to-c but expansion is not a string: {s}" name expansion))
                              inclusions)))
            (let ((new-declaration (or (jazz.find-child-declaration declaration name)
                                       (jazz.new-c-type-declaration name type access compatibility '() declaration kind expansion base-type-declaration inclusions c-to-scheme scheme-to-c declare))))
              (%%when base-type-declaration
                (%%set-c-type-declaration-pointer-types base-type-declaration (%%cons new-declaration (%%get-c-type-declaration-pointer-types base-type-declaration))))
              (let ((effective-declaration (jazz.add-declaration-child walker resume declaration new-declaration)))
                effective-declaration))))
      (jazz.walk-error walker resume declaration form-src "C types can only be defined at the module level: {s}" name))))


(define (jazz.walk-c-type walker resume declaration environment form-src)
  (let ((form (%%desourcify form-src)))
    (receive (name type access compatibility c-type c-to-scheme scheme-to-c declare) (jazz.parse-c-type walker resume declaration (%%cdr form))
      (jazz.require-declaration declaration name))))


(define (jazz.resolve-c-type walker resume declaration environment type)
  (let ((queue (jazz.new-queue)))
    (define (resolve type)
      (cond ((%%symbol? type)
             (let ((c-type-declaration (jazz.resolve-c-type-reference walker resume declaration environment type)))
               (jazz.enqueue queue c-type-declaration)
               (values 'alias (%%get-declaration-locator c-type-declaration) #f)))
            ((%%string? type)
             (values 'type type #f))
            ((%%pair? type)
             (case (%%car type)
               ((native)
                (values 'native (%%cadr type) #f))
               ((type)
                (jazz.bind (c-string . tag-rest) (%%cdr type)
                  (values 'type `(type ,c-string ,@tag-rest) #f)))
               ((pointer)
                (jazz.bind (base-type . tag-rest) (%%cdr type)
                  (values 'pointer `(pointer ,(resolve-expansion base-type) ,@tag-rest)
                    (and (%%symbol? base-type) (jazz.resolve-c-type-reference walker resume declaration environment base-type)))))
               ((function)
                (jazz.bind (parameter-types result-type) (%%cdr type)
                  (values 'function `(function ,(map resolve-expansion parameter-types) ,(resolve-expansion result-type)) #f)))
               ((struct)
                (jazz.bind (c-string . tag-rest) (%%cdr type)
                  (values 'struct `(struct ,c-string ,@tag-rest) #f)))
               ((union)
                (jazz.bind (c-string . tag-rest) (%%cdr type)
                  (values 'union `(union ,c-string ,@tag-rest) #f)))))
            (else
             (jazz.walk-error walker resume declaration #f "Ill-formed c-type: {s}" type))))
                
    (define (resolve-expansion type)
      (receive (kind expansion base-type-declaration) (resolve type)
        expansion))
    
    (receive (kind expansion base-type-declaration) (resolve type)
      (values kind expansion base-type-declaration (jazz.queue-list queue)))))


(define (jazz.resolve-c-type-reference walker resume declaration environment symbol)
  (let ((c-type-declaration (jazz.lookup-reference walker resume declaration environment symbol)))
    (if (%%class-is? c-type-declaration jazz.C-Type-Declaration)
        c-type-declaration
      (jazz.walk-error walker resume declaration #f "{s} did not resolve to a c-type: {s}" symbol (%%get-declaration-locator c-type-declaration)))))


(define (jazz.expand-c-type-reference walker resume declaration environment type)
  (receive (kind expansion base-type-declaration inclusions) (jazz.resolve-c-type walker resume declaration environment type)
    (let ((module-declaration (%%get-declaration-toplevel declaration)))
      (%%set-module-declaration-inclusions module-declaration (%%append (%%get-module-declaration-inclusions module-declaration) inclusions))
      expansion)))


;;;
;;;; C-Function
;;;


(define (jazz.walk-c-function walker resume declaration environment form-src)
  (let ((form (%%desourcify form-src)))
    (%%assertion (and (list? form) (%%fx= 4 (%%length form))) (jazz.walk-error walker resume declaration form-src "Ill-formed c-function")
      (jazz.bind (types result-type c-name-or-code) (%%cdr form)
        (let ((resolve-access (lambda (type) (jazz.expand-c-type-reference walker resume declaration environment type))))
          (jazz.new-c-function
            `(c-lambda ,(map resolve-access types) ,(resolve-access result-type) ,c-name-or-code)))))))


;;;
;;;; C-Definition
;;;


(define jazz.c-definition-modifiers
  '(((private protected package public) . public)
    ((deprecated undocumented uptodate) . uptodate)))


(define (jazz.parse-c-definition walker resume declaration rest)
  (receive (access compatibility rest) (jazz.parse-modifiers walker resume declaration jazz.c-definition-modifiers rest)
    (jazz.bind (signature parameter-types result-type c-name scope . body) rest
      (let ((name (%%car signature))
            (type jazz.Any)
            (parameters (%%cdr signature)))
        (values name type access compatibility parameters parameter-types result-type c-name scope body)))))


(define (jazz.walk-c-definition-declaration walker resume declaration environment form-src)
  (let ((form (%%desourcify form-src)))
    (receive (name type access compatibility parameters parameter-types result-type c-name scope body) (jazz.parse-c-definition walker resume declaration (%%cdr form))
      (let ((resolve-access (lambda (type) (jazz.expand-c-type-reference walker resume declaration environment type)))
            (signature (jazz.walk-parameters walker resume declaration environment parameters #f #f)))
        (let ((new-declaration (or (jazz.find-child-declaration declaration name)
                                   (jazz.new-c-definition-declaration name type access compatibility '() declaration signature (map resolve-access parameter-types) (resolve-access result-type) c-name scope))))
          (%%set-declaration-source new-declaration form-src)
          (let ((effective-declaration (jazz.add-declaration-child walker resume declaration new-declaration)))
            effective-declaration))))))


(define (jazz.walk-c-definition walker resume declaration environment form-src)
  (let ((form (%%desourcify form-src)))
    (receive (name type access compatibility parameters parameter-types result-type c-name scope body) (jazz.parse-c-definition walker resume declaration (%%cdr form))
      (let* ((new-declaration (jazz.require-declaration declaration name)))
        (receive (signature augmented-environment) (jazz.walk-parameters walker resume declaration environment parameters #f #t)
          (%%set-c-definition-declaration-signature new-declaration signature)
          (%%set-c-definition-declaration-body new-declaration
            (jazz.walk-body walker resume new-declaration augmented-environment body))
          (%%set-declaration-source new-declaration form-src)
          new-declaration)))))


;;;
;;;; C-Structure
;;;


(define (jazz.build-pointer-symbol type)
  (%%string->symbol (%%string-append (%%symbol->string type) "*")))


(define (jazz.pointer? type)
  (let* ((str (%%symbol->string type))
         (lgt (%%string-length str)))
    (%%eq? (%%string-ref str (%%fx- lgt 1)) #\*)))


(define (jazz.build-method-symbol struct . rest)
  (%%string->symbol (apply string-append (%%symbol->string struct) "-" (map symbol->string rest))))


(define (jazz.parse-structure-name name)
  (if (%%symbol? name)
      (values name (%%symbol->string name) '())
    (values (%%car name) (%%cadr name) (%%cddr name))))


(define (jazz.kind+type walker resume declaration environment type)
  (let loop ((declaration (jazz.resolve-c-type-reference walker resume declaration environment type)))
       (let ((kind (%%get-c-type-declaration-kind declaration))
             (expansion (%%get-c-type-declaration-expansion declaration))
             (inclusions (%%get-c-type-declaration-inclusions declaration)))
         (if (%%eq? kind 'alias)
             (loop (%%car inclusions))
           (values kind expansion)))))


(define (jazz.expand-accessor walker resume declaration environment clause struct)
  (let ((type (%%car clause))
        (id   (%%cadr clause))
        (size (let ((clause-rest (%%cddr clause)))
                (and (%%not (%%null? clause-rest))
                     (%%car clause-rest)))))
    (%%when (and size (%%not (%%integer? size)) (%%not (%%symbol? size)))
      (jazz.walk-error walker resume declaration #f "Illegal clause size in {a}: {a}" struct clause))
    (receive (kind expansion) (jazz.kind+type walker resume declaration environment type)
      (let ((id-string (%%symbol->string id)))
        (let ((getter-string
                (if size
                    (if (or (%%eq? expansion 'char)
                            (%%eq? expansion 'wchar_t))
                        (%%string-append "___result = ___arg1->" id-string ";")
                      (%%string-append "___result_voidstar = ___arg1->" id-string ";"))
                  (case kind
                    ((native)
                     (%%string-append "___result = ___arg1->" id-string ";"))
                    ((pointer function)
                     (%%string-append "___result_voidstar = ___arg1->" id-string ";"))
                    ((type struct union)
                     (%%string-append "___result_voidstar = &___arg1->" id-string ";")))))
              (setter-string
                (cond (size
                        (let ((size-string (if (%%integer? size)
                                               (%%number->string size)
                                             (%%symbol->string size))))
                          (cond ((%%eq? expansion 'char)
                                 (%%string-append "strncpy(___arg1->" id-string ", ___arg2, " size-string ");"))
                                ((%%eq? expansion 'wchar_t)
                                 (%%string-append "wcsncpy(___arg1->" id-string ", ___arg2, " size-string ");"))
                                (else
                                 ;; We need to adjust this to get the real c-type in the expansion
                                 (%%string-append "memcpy(___arg1->" id-string ", ___arg2, " size-string "*" "sizeof(" (%%symbol->string type) "));")))))
                      ((or (%%eq? kind 'struct)
                           (%%eq? kind 'union)
                           (%%eq? kind 'type))
                       #f)
                      (else
                       (%%string-append "___arg1->" id-string " = ___arg2;")))))
          (let* ((struct* (jazz.build-pointer-symbol struct))
                 (type* (jazz.build-pointer-symbol type))
                 (type (cond ((and size (%%eq? expansion 'char)) '(native char-string))
                             ((and size (%%eq? expansion 'wchar_t)) '(native wchar_t-string))
                             (size type*)
                             ((%%memq kind '(type struct union)) type*)
                             (else type))))
            (let ((getter `(definition public ,(jazz.build-method-symbol struct id '-ref)
                                       (c-function (,struct*) ,type ,getter-string)))
                  (setter (and setter-string
                               `(definition public ,(jazz.build-method-symbol struct id '-set!)
                                            (c-function (,struct* ,type) (native void) ,setter-string)))))
              (values getter setter))))))))


(define (jazz.expand-structure/union walker resume declaration environment name clauses)
  (receive (struct c-struct-string tag-rest) (jazz.parse-structure-name name)
    (let ((struct* (jazz.build-pointer-symbol struct))
          (sizeof (%%string-append "sizeof(" c-struct-string ")"))
          (tag*-rest (if (%%null? tag-rest) '() (%%cons (jazz.build-pointer-symbol (%%car tag-rest)) (%%cdr tag-rest)))))
      (define (expand-accessor clause)
        (receive (getter setter) (jazz.expand-accessor walker resume declaration environment clause struct)
          (if setter
              (%%list getter setter)
            (%%list getter))))
      `(begin
         (c-type ,struct (type ,c-struct-string ,@tag-rest))
         (c-type ,struct* (pointer ,struct ,@tag*-rest))
         (definition public ,(jazz.build-method-symbol struct 'make)
                     (c-function () ,struct* ,(%%string-append "___result_voidstar = calloc(1," sizeof ");")))
         (definition public ,(jazz.build-method-symbol struct 'free)
                     (c-function (,struct*) (native void) "free(___arg1);"))
         (definition public ,(jazz.build-method-symbol struct 'sizeof)
                     (c-function () (native unsigned-int) ,(%%string-append "___result = " sizeof ";")))
         ,@(%%apply append (map expand-accessor clauses))))))


(define (jazz.expand-c-structure walker resume declaration environment name . clauses)
  (jazz.expand-structure/union walker resume declaration environment name clauses))


(define (jazz.expand-c-structure-array walker resume declaration environment name . rest)
  (let* ((struct name)
         (struct* (jazz.build-pointer-symbol struct))
         (c-struct-string (if (%%not (%%null? rest)) (%%car rest) (%%symbol->string struct))))
    `(begin
       (definition public ,(jazz.build-method-symbol struct 'array-make)
         (c-function (int) ,struct* ,(%%string-append "___result = calloc(___arg1,sizeof(" c-struct-string "));")))
       (definition public ,(jazz.build-method-symbol struct 'array-element)
         (c-function (,struct* int) ,struct* ,(%%string-append "___result = ___arg1+___arg2;"))))))


(define (jazz.expand-c-union walker resume declaration environment name . clauses)
  (jazz.expand-structure/union walker resume declaration environment name clauses))


;;;
;;;; C-External
;;;


(define (jazz.expand-c-external walker resume declaration environment type signature . rest)
  (let* ((s-name (%%car signature))
         (params (%%cdr signature))
         (c-name (if (%%null? rest) (%%symbol->string s-name) (%%car rest))))
    `(definition public ,s-name
       (c-function ,params ,type ,c-name))))


;; tofix : risk of segmentation fault if passing an bad string size
(define (jazz.expand-c-external-so walker resume declaration environment type arg signature . rest)
  (let* ((s-name (%%car signature))
         (ext-s-name (%%string->symbol (%%string-append (%%symbol->string s-name) "_EXT")))
         (params (%%cdr signature))
         (new-params (map (lambda (param) (jazz.generate-symbol (%%symbol->string param))) params))
         (string-param (list-ref new-params arg))
         (c-name (if (%%null? rest) (%%symbol->string s-name) (%%car rest))))
    `(begin
       (c-external ,type ,(%%cons ext-s-name params) ,c-name)
       (definition public (,s-name ,@new-params)
         (let ((pt (WCHAR-array-make (+ (string-length ,string-param) 1))))
           (WCHAR-copy pt ,string-param (string-length ,string-param))
           (let* ((,string-param pt)
                  (result (,ext-s-name ,@new-params)))
             (values result (WCHAR-string ,string-param))))))))



;;;
;;;; Parameterize
;;;


(define (jazz.walk-parameterize walker resume declaration environment form-src)
  (let ((bindings (jazz.source-code (%%cadr (jazz.source-code form-src))))
        (body (%%cddr (jazz.source-code form-src))))
    (let ((effective-body (if (%%null? body) (%%list (%%list 'unspecified)) body))
          (expanded-bindings (jazz.new-queue)))
      (for-each (lambda (binding-form)
                  (continuation-capture
                    (lambda (resume)
                      (let ((variable (%%car (jazz.source-code binding-form)))
                            (value (%%car (jazz.source-code (%%cdr (jazz.source-code binding-form))))))
                        (jazz.enqueue expanded-bindings
                                      (%%cons (continuation-capture
                                                (lambda (resume)
                                                  (jazz.walk walker resume declaration environment variable)))
                                              (continuation-capture
                                                (lambda (resume)
                                                  (jazz.walk walker resume declaration environment value)))))))))
                bindings)
      (jazz.new-parameterize (jazz.queue-list expanded-bindings)
                             (jazz.walk-body walker resume declaration environment effective-body)))))


;;;
;;;; With Slots
;;;


(define (jazz.walk-with-slots walker resume declaration environment form-src)
  (let ((form (%%desourcify form-src)))
    (jazz.bind (slot-names object . body) (%%cdr form)
      (let ((object-symbol (jazz.generate-symbol "object")))
        (jazz.walk walker resume declaration environment
          `(let ((,object-symbol ,object))
             (let-symbol ,(map (lambda (slot-name)
                                 (let* ((slot-declaration (jazz.lookup-reference walker resume declaration environment slot-name))
                                        (getter-name (%%get-slot-declaration-getter-name slot-declaration))
                                        (setter-name (%%get-slot-declaration-setter-name slot-declaration)))
                                   (%%list slot-name `(lambda () (%%list ',getter-name ',object-symbol)) `(lambda (value) (%%list ',setter-name ',object-symbol value)))))
                               slot-names)
                         ,@body)))))))


;;;
;;;; Time
;;;


(define (jazz.walk-time walker resume declaration environment form-src)
  (let ((form (%%desourcify form-src)))
    (let ((forms (%%cdr form)))
      (jazz.new-time-special (jazz.walk-list walker resume declaration environment forms)))))


(jazz.encapsulate-class jazz.Jazz-Walker)


;;;
;;;; Dialect
;;;


(jazz.define-dialect jazz
  (jazz.new-jazz-dialect))

(jazz.define-dialect jazz.dialect
  (jazz.new-jazz-dialect))


(jazz.define-walker-special definition           jazz jazz.walk-definition)
(jazz.define-walker-special generic              jazz jazz.walk-generic)
(jazz.define-walker-special specific             jazz jazz.walk-specific)
(jazz.define-walker-syntax  class                jazz jazz.expand-class)
(jazz.define-walker-special %class               jazz jazz.walk-%class)
(jazz.define-walker-special interface            jazz jazz.walk-interface)
(jazz.define-walker-syntax  slot                 jazz jazz.expand-slot)
(jazz.define-walker-syntax  property             jazz jazz.expand-property)
(jazz.define-walker-special %slot                jazz jazz.walk-%slot)
(jazz.define-walker-special %property            jazz jazz.walk-%slot)
(jazz.define-walker-special method               jazz jazz.walk-method)
(jazz.define-walker-special declare              jazz jazz.walk-declare)
(jazz.define-walker-special c-include            jazz jazz.walk-c-include)
(jazz.define-walker-special c-declare            jazz jazz.walk-c-declare)
(jazz.define-walker-special c-named-declare      jazz jazz.walk-c-named-declare)
(jazz.define-walker-special c-initialize         jazz jazz.walk-c-initialize)
(jazz.define-walker-special c-function           jazz jazz.walk-c-function)
(jazz.define-walker-special c-type               jazz jazz.walk-c-type)
(jazz.define-walker-special c-definition         jazz jazz.walk-c-definition)
(jazz.define-walker-macro   specialize           jazz jazz.expand-specialize)
(jazz.define-walker-special %specialize          jazz jazz.walk-%specialize)
(jazz.define-walker-special parameterize         jazz jazz.walk-parameterize)
(jazz.define-walker-special with-slots           jazz jazz.walk-with-slots)
(jazz.define-walker-special with-self            jazz jazz.walk-with-self)
(jazz.define-walker-special with-dynamic-self    jazz jazz.walk-with-dynamic-self)
(jazz.define-walker-special with-local-variables jazz jazz.walk-with-local-variables)
(jazz.define-walker-special cast                 jazz jazz.walk-cast)
(jazz.define-walker-special construct            jazz jazz.walk-construct)
(jazz.define-walker-special time                 jazz jazz.walk-time)
(jazz.define-walker-macro   remotable-stub       jazz jazz.expand-remotable-stub)
(jazz.define-walker-syntax  assert               jazz jazz.expand-assert)
(jazz.define-walker-syntax  assertion            jazz jazz.expand-assertion)
(jazz.define-walker-macro   c-structure          jazz jazz.expand-c-structure)
(jazz.define-walker-macro   c-union              jazz jazz.expand-c-union)
(jazz.define-walker-macro   c-external           jazz jazz.expand-c-external)
(jazz.define-walker-macro   c-external-so        jazz jazz.expand-c-external-so)
(jazz.define-walker-macro   com-external         jazz jazz.expand-com-external)
(jazz.define-walker-macro   declaration-path     jazz jazz.expand-declaration-path)
(jazz.define-walker-macro   declaration-locator  jazz jazz.expand-declaration-locator)
(jazz.define-walker-syntax  debug-assert         jazz jazz.expand-debug-assert)
(jazz.define-walker-syntax  debug-assertion      jazz jazz.expand-debug-assertion))
