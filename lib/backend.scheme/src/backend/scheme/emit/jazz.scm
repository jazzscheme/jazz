;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Scheme Jazz Backend
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


(unit protected backend.scheme.emit.jazz


;;;
;;;; Definition
;;;


(jazz:define-emit (definition (scheme backend) declaration environment expression)
  (jazz:sourcify-if
    (let ((locator (%%get-declaration-locator declaration)))
      `(begin
         (define ,locator
           ,expression)
         ,(let ((name (%%get-lexical-binding-name declaration))
                (parent (%%get-declaration-parent declaration)))
            (if (%%is? parent jazz:Module-Declaration)
                `(jazz:register-definition ',(%%get-lexical-binding-name parent) ',name ',locator)
              `(jazz:add-field ,(%%get-declaration-locator parent) (jazz:new-definition ',name ',locator))))))
    (%%get-declaration-source declaration)))


;;;
;;;; Specialize
;;;


(jazz:define-emit (specialize (scheme backend) expression declaration environment)
  `(begin))


;;;
;;;; Generic
;;;


(jazz:define-emit (generic (scheme backend) declaration environment signature-emit body-emit)
  (let ((generic-locator (%%get-declaration-locator declaration)))
    `(jazz:define-generic ,(%%cons generic-locator signature-emit)
       ,@(jazz:sourcified-form body-emit))))


;;;
;;;; Specific
;;;


(jazz:define-emit (specific (scheme backend) declaration environment signature-emit body-emit)
  (let ((generic-declaration (%%get-specific-declaration-generic declaration)))
    (let ((generic-locator (%%get-declaration-locator generic-declaration))
          (modifier (if (%%get-specific-declaration-root? declaration) 'root 'child)))
      `(jazz:define-specific ,(%%cons generic-locator signature-emit) ,modifier
         ,@(jazz:sourcified-form body-emit)))))


;;;
;;;; Class
;;;


(jazz:define-emit (class (scheme backend) declaration environment)
  (let ((name (%%get-lexical-binding-name declaration))
        (locator (%%get-declaration-locator declaration))
        (ascendant-declaration (%%get-class-declaration-ascendant declaration))
        (interface-declarations (%%get-class-declaration-interfaces declaration))
        (body (%%get-namespace-declaration-body declaration)))
    (let ((level-locator (jazz:compose-helper locator 'level)))
      (jazz:sourcify-if
        `(begin
           ,@(if (jazz:core-class? name)
                 (let ((core-class (jazz:get-core-class name)))
                   (if (%%not (%%symbol? core-class))
                       (jazz:validate-core-class core-class declaration))
                   (let ((core-class-locator (if (%%symbol? core-class) core-class (%%get-category-identifier core-class)))
                         (ascendant-access (if (%%not ascendant-declaration) #f (jazz:sourcified-form (jazz:emit-binding-reference ascendant-declaration declaration environment backend)))))
                     `((define ,locator ,core-class-locator)
                       (define ,level-locator (%%get-class-level ,locator))
                       (jazz:set-core-class-redefined ',name ',core-class-locator)
                       (jazz:remove-own-slots ,locator))))
               (let ((metaclass-declaration (%%get-category-declaration-metaclass declaration)))
                 (let ((ascendant-access (jazz:emit-ascendant-access declaration environment backend)))
                   (let ((metaclass-access (if (%%not metaclass-declaration) (if (%%not ascendant-access) 'jazz:Object-Class `(%%get-object-class ,ascendant-access)) (jazz:sourcified-form (jazz:emit-binding-reference metaclass-declaration declaration environment backend))))
                         (interface-accesses (map (lambda (declaration) (jazz:sourcified-form (jazz:emit-binding-reference declaration declaration environment backend))) interface-declarations)))
                     `((define ,locator
                         ;; this is a quicky that needs to be well tought out
                         (if (jazz:global-bound? ',locator)
                             (jazz:global-ref ',locator)
                           (jazz:new-class ,metaclass-access ',locator ,ascendant-access (%%list ,@interface-accesses))))
                       (define ,level-locator (%%get-class-level ,locator)))))))
           ,(let ((toplevel-declaration (%%get-declaration-toplevel declaration)))
              `(jazz:register-module-entry ',(%%get-lexical-binding-name toplevel-declaration) ',name ,locator))
           ,@(jazz:emit-namespace-statements body declaration environment backend))
        (%%get-declaration-source declaration)))))


;;;
;;;; Interface
;;;


(jazz:define-emit (interface (scheme backend) declaration environment)
  (let* ((name (%%get-lexical-binding-name declaration))
         (locator (%%get-declaration-locator declaration))
         (rank-locator (jazz:compose-helper locator 'rank))
         (ascendant-declarations (%%get-interface-declaration-ascendants declaration))
         (metaclass-declaration (%%get-category-declaration-metaclass declaration))
         (metaclass-access (if (%%not metaclass-declaration) 'jazz:Interface (jazz:sourcified-form (jazz:emit-binding-reference metaclass-declaration declaration environment backend))))
         (ascendant-accesses (map (lambda (declaration) (jazz:sourcified-form (jazz:emit-binding-reference declaration declaration environment backend))) ascendant-declarations))
         (body (%%get-namespace-declaration-body declaration)))
    (jazz:sourcify-if
      `(begin
         (define ,locator
           (jazz:new-interface ,metaclass-access ',locator (%%list ,@ascendant-accesses)))
         (define ,rank-locator
           (%%get-interface-rank ,locator))
         ,(let ((toplevel-declaration (%%get-declaration-toplevel declaration)))
            `(jazz:register-module-entry ',(%%get-lexical-binding-name toplevel-declaration) ',name ,locator))
         ,@(jazz:emit-namespace-statements body declaration environment backend))
      (%%get-declaration-source declaration))))


;;;
;;;; Slot
;;;


(jazz:define-emit (slot (scheme backend) declaration environment initialize-emit)
  (let* ((name (%%get-lexical-binding-name declaration))
         (locator (%%get-declaration-locator declaration))
         (class-declaration (%%get-declaration-parent declaration))
         (class-locator (%%get-declaration-locator class-declaration))
         (allocate? (%%neq? (%%get-lexical-binding-type declaration) jazz:Void))
         (core? (jazz:core-class? (%%get-lexical-binding-name class-declaration)))
         (initialize (%%get-slot-declaration-initialize declaration))
         (initialize? (and allocate? (%%not core?) initialize))
         (initialize-locator (and initialize? (jazz:compose-helper locator 'initialize)))
         (slot-locator (jazz:compose-helper locator 'slot))
         (offset-locator (jazz:compose-helper locator 'offset)))
    (jazz:sourcify-if
      `(begin
         ,@(if initialize?
               `((define (,initialize-locator self)
                   ,(jazz:sourcified-form initialize-emit)))
             '())
         (define ,slot-locator
           (jazz:add-slot ,class-locator ',name ,initialize-locator ,allocate?))
         (define ,offset-locator
           (%%get-slot-offset ,slot-locator))
         ,@(jazz:declaration-result))
      (%%get-declaration-source declaration))))


;;;
;;;; Property
;;;


(jazz:define-emit (property (scheme backend) declaration environment initialize-emit)
  (let* ((name (%%get-lexical-binding-name declaration))
         (locator (%%get-declaration-locator declaration))
         (class-declaration (%%get-declaration-parent declaration))
         (class-locator (%%get-declaration-locator class-declaration))
         (allocate? (%%neq? (%%get-lexical-binding-type declaration) jazz:Void))
         (core? (jazz:core-class? (%%get-lexical-binding-name class-declaration)))
         (initialize (%%get-slot-declaration-initialize declaration))
         (initialize? (and allocate? (%%not core?) initialize))
         (initialize-locator (and initialize? (jazz:compose-helper locator 'initialize)))
         (slot-locator (jazz:compose-helper locator 'slot))
         (offset-locator (jazz:compose-helper locator 'offset))
         (getter (%%get-property-declaration-getter declaration))
         (setter (%%get-property-declaration-setter declaration)))
    ;; hack in a literal self instead of the hygienically renamed self
    (define (fix-self expr)
      (if (pair? expr)
          (cons (car expr) (cons (cons 'self (cdadr expr)) (cddr expr)))
        expr))
    (jazz:sourcify-if
      `(begin
         ,@(if initialize?
               `((define (,initialize-locator self)
                   ,(jazz:sourcified-form initialize-emit)))
             '())
         (define ,slot-locator
           (jazz:add-property ,class-locator ',name ,initialize-locator ,allocate?
             ,(fix-self (jazz:sourcified-form (jazz:emit-expression getter declaration environment backend)))
             ,(fix-self (jazz:sourcified-form (jazz:emit-expression setter declaration environment backend)))))
         (define ,offset-locator
           (%%get-slot-offset ,slot-locator))
         ,@(jazz:declaration-result))
      (%%get-declaration-source declaration))))


;;;
;;;; Method
;;;


(jazz:define-emit (method (scheme backend) declaration environment signature-emit signature-casts body-emit)
  (let* ((name (%%get-lexical-binding-name declaration))
         (abstraction (%%get-method-declaration-abstraction declaration))
         (propagation (%%get-method-declaration-propagation declaration))
         (category-declaration (%%get-declaration-parent declaration))
         (class-locator (%%get-declaration-locator category-declaration))
         (method-locator (%%get-declaration-locator declaration))
         (method-rank-locator (jazz:compose-helper method-locator 'rank))
         (method-node-locator (jazz:compose-helper method-locator 'node))
         (method-call (cond ((%%class-is? category-declaration jazz:Class-Declaration)     (case propagation
                                                                                             ((override)        'jazz:add-method-node)
                                                                                             ((final)           'jazz:add-final-method)
                                                                                             ((virtual chained) 'jazz:add-virtual-method)))
                            ((%%class-is? category-declaration jazz:Interface-Declaration) (case propagation
                                                                                             ((override)        'jazz:add-method-node)
                                                                                             ((virtual)         'jazz:add-virtual-method))))))
    (jazz:sourcify-if
      (case method-call
        ((jazz:add-method-node)
         (let ((node (jazz:generate-symbol "node")))
           `(begin
              (define (,method-locator self ,@signature-emit)
                ,@signature-casts
                (let ((nextmethod (%%get-method-node-next-implementation ,method-node-locator)))
                  ,body-emit))
              (define ,method-node-locator
                (,method-call ,class-locator ',name ,method-locator))
              ,@(jazz:declaration-result))))
        ((jazz:add-virtual-method)
         `(begin
            ,(if (%%eq? abstraction 'abstract)
                 `(define (,method-locator self . rest)
                    (jazz:call-into-abstract ',class-locator ',name self rest))
               `(define (,method-locator self ,@signature-emit)
                  ,@signature-casts
                  (let ()
                    ,body-emit)))
            (define ,method-rank-locator
              (,method-call ,class-locator ',name ,method-locator))
            ,@(jazz:declaration-result)))
        ((jazz:add-final-method)
         `(begin
            (define (,method-locator self ,@signature-emit)
              ,@signature-casts
              (let ()
                ,body-emit))
            (,method-call ,class-locator ',name ,method-locator)
            ,@(jazz:declaration-result))))
      (%%get-declaration-source declaration))))


;;;
;;;; With-Self
;;;


(jazz:define-emit (with-self (scheme backend) expression declaration environment body-emit)
  (jazz:simplify-begin
    `(begin
       ,@(jazz:sourcified-form body-emit))))


;;;
;;;; With-Dynamic-Self
;;;


(jazz:define-emit (with-dynamic-self (scheme backend) expression declaration environment body-emit)
  (jazz:simplify-begin
    `(begin
       ,@(jazz:sourcified-form body-emit))))


;;;
;;;; Dispatch
;;;


(jazz:define-emit (dispatch (scheme backend) expression declaration environment object-code rest-codes)
  (let ((name (%%get-dispatch-name expression)))
    (define (resolve-type object-code)
      (let ((object-type (jazz:patch-type-until-unification (%%get-code-type object-code))))
        (if (%%class-is? object-type jazz:Autoload-Declaration)
            (jazz:resolve-binding object-type)
          object-type)))
    
    (define (lookup-method object-code)
      (let ((object-type (resolve-type object-code)))
        (if (%%class-is? object-type jazz:Category-Declaration)
            (let ((declaration (jazz:lookup-declaration object-type name jazz:public-access declaration)))
              (if (and declaration (%%class-is? declaration jazz:Method-Declaration))
                  declaration
                #f))
          #f)))
    
    (define (lookup-method/warn object-code)
      (let ((method-declaration (lookup-method object-code)))
        (if (%%not method-declaration)
            (begin
              (if (and (jazz:warnings?) (jazz:get-module-warn? (%%get-declaration-toplevel declaration) 'optimizations))
                  (jazz:debug 'Warning: 'In (%%get-declaration-locator declaration) 'unable 'to 'find 'dispatch 'method name))
              #f)
          (begin
            (add-to-module-references declaration method-declaration)
            method-declaration))))
    
    (define (jazz:with-code-value code proc)
      (let ((form (%%get-code-form code)))
        (if (%%symbol? form)
            (proc code)
          (let ((value (jazz:generate-symbol "val")))
            (let ((code (proc (jazz:new-code value (%%get-code-type code) #f))))
              (jazz:new-code
                `(let ((,value ,form))
                   ,(%%get-code-form code))
                (%%get-code-type code)
                (%%get-code-source code)))))))
    
    (let ((method-declaration (lookup-method/warn object-code)))
      (if method-declaration
          (or (jazz:emit-inlined-final-dispatch expression method-declaration object-code rest-codes declaration environment backend)
              (jazz:with-code-value object-code
                (lambda (code)
                  (let ((dispatch-code (jazz:emit-method-dispatch code method-declaration declaration environment backend)))
                    (jazz:new-code
                      `(,(jazz:sourcified-form dispatch-code)
                        ,(jazz:sourcified-form code)
                        ,@(jazz:codes-forms rest-codes))
                      (%%get-code-type dispatch-code)
                      (%%get-expression-source expression))))))
        (let ((dv (jazz:register-variable declaration (%%string-append (%%symbol->string name) "!d") #f)))
          (let ((d (%%car dv)))
            (%%set-cdr! dv `(jazz:cache-dispatch ',name (lambda (d) (set! ,d d))))
            (jazz:new-code
              (jazz:with-uniqueness (jazz:sourcified-form object-code)
                (lambda (object)
                  `((,d ,object) ,object ,@(jazz:codes-forms rest-codes))))
              jazz:Any
              (%%get-expression-source expression))))))))


;;;
;;;; Cast
;;;


(jazz:define-emit (cast (scheme backend) expression declaration environment type expression-emit)
  (jazz:emit-type-cast
    expression-emit
    type
    declaration
    environment
    backend))


;;;
;;;; Allocate
;;;


(jazz:define-emit (allocate (scheme backend) expression declaration environment class-emit values-emit)
  `(%%object ,(jazz:sourcified-form class-emit)
             ,@(jazz:codes-forms values-emit)))


;;;
;;;; Reference
;;;


(jazz:define-emit (category-reference (scheme backend) declaration)
  (%%get-declaration-locator declaration))


(jazz:define-emit (class-reference (scheme backend) declaration)
  (%%get-declaration-locator declaration))


(jazz:define-emit (c-definition-reference (scheme backend) declaration)
  (%%get-declaration-locator declaration))


(jazz:define-emit (definition-reference (scheme backend) declaration)
  (%%get-declaration-locator declaration))


(jazz:define-emit (dynamic-self-reference (scheme backend) declaration)
  (%%get-dynamic-self-binding-code declaration))


(jazz:define-emit (generic-reference (scheme backend) declaration)
  (%%get-declaration-locator declaration))


(jazz:define-emit (method-reference (scheme backend) declaration source-declaration environment self dispatch-code)
  `(lambda rest
     (apply ,(jazz:sourcified-form dispatch-code) ,(jazz:sourcified-form self) rest)))


(jazz:define-emit (method-node-reference (scheme backend) expression declaration environment)
  (let ((method-declaration (%%get-reference-binding expression)))
    (%%get-declaration-locator method-declaration)))


(jazz:define-emit (nextmethod-variable-reference (scheme backend) binding)
  (let ((name (%%get-lexical-binding-name binding))
        (self (jazz:*self*)))
    (if self
        `(lambda rest (apply ,name ,(jazz:sourcified-form self) rest))
      name)))


(jazz:define-emit (self-reference (scheme backend) declaration source-declaration)
  (%%get-declaration-parent source-declaration))


(jazz:define-emit (slot-reference (scheme backend) declaration self)
  (let ((offset-locator (jazz:compose-helper (%%get-declaration-locator declaration) 'offset)))
    `(%%object-ref ,(jazz:sourcified-form self) ,offset-locator)))


;;;
;;;; Call
;;;


(jazz:define-emit (method-node-call (scheme backend) expression declaration operator arguments)
  `(,(jazz:sourcified-form operator) ,@(jazz:codes-forms arguments)))


;;;
;;;; New Call
;;;


(jazz:define-emit (new-call (scheme backend) operator locator arguments arguments-codes declaration environment)
  (if (%%eq? locator 'jazz.dialect.runtime.kernel:new)
      (%%assert (%%pair? arguments)
        (let ((class-expression (%%car arguments)))
          (if (%%class-is? class-expression jazz:Binding-Reference)
              (let ((binding (%%get-reference-binding class-expression)))
                (if (or (%%class-is? binding jazz:Class-Declaration)
                        (%%class-is? binding jazz:Autoload-Declaration))
                    (let ((values-codes (%%cdr arguments-codes)))
                      (jazz:new-code
                        (case (%%length values-codes)
                          ((0) `(jazz:new0 ,@(jazz:codes-forms arguments-codes)))
                          ((1) `(jazz:new1 ,@(jazz:codes-forms arguments-codes)))
                          ((2) `(jazz:new2 ,@(jazz:codes-forms arguments-codes)))
                          (else `(jazz:new ,@(jazz:codes-forms arguments-codes))))
                        binding
                        #f))
                  #f))
            #f)))
    #f)))
