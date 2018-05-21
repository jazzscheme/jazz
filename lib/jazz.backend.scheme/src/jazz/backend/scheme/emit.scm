;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Jazz Scheme Backend
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


(unit protected jazz.backend.scheme.emit


;;;
;;;; Definition
;;;


(jazz:define-emit (definition (scheme backend) declaration walker resume environment expression unsafe-expression)
  (jazz:sourcify-deep-if
    (let ((locator (jazz:get-declaration-locator declaration)))
      (let ((unsafe-locator (and unsafe-expression (jazz:unsafe-locator locator))))
        (jazz:simplify-begin
          `(begin
             ,@(if unsafe-locator
                   `((define ,unsafe-locator
                       ,unsafe-expression))
                 '())
             (define ,locator
               ,expression)
             ,@(let ((name (jazz:get-lexical-binding-name declaration))
                     (parent (jazz:get-declaration-parent declaration)))
                 (if (%%is? parent jazz:Module-Declaration)
                     (if (jazz:get-generate? 'register)
                         `((jazz:register-definition ',(jazz:get-lexical-binding-name parent) ',name ',locator))
                       '())
                   `((jazz:add-field ,(jazz:get-declaration-locator parent) (jazz:new-definition ',name ',locator)))))))))
    (jazz:get-declaration-source declaration)))


;;;
;;;; Generic
;;;


(jazz:define-emit (generic (scheme backend) declaration walker resume environment signature-emit body-emit)
  (let ((generic-locator (jazz:get-declaration-locator declaration)))
    `(jazz:define-generic ,(%%cons generic-locator signature-emit)
       ,@(jazz:sourcified-form body-emit))))


;;;
;;;; Specific
;;;


(jazz:define-emit (specific (scheme backend) declaration walker resume environment signature-emit body-emit)
  (let ((generic-declaration (jazz:get-specific-declaration-generic declaration)))
    (let ((generic-locator (jazz:get-declaration-locator generic-declaration))
          (modifier (if (jazz:get-specific-declaration-root? declaration) 'root 'child)))
      `(jazz:define-specific ,(%%cons generic-locator signature-emit) ,modifier
         ,@(jazz:sourcified-form body-emit)))))


;;;
;;;; Class
;;;


(jazz:define-emit (class (scheme backend) declaration walker resume environment)
  (let ((name (jazz:get-lexical-binding-name declaration))
        (locator (jazz:get-declaration-locator declaration))
        (ascendant-declaration (jazz:get-class-declaration-ascendant declaration))
        (interface-declarations (jazz:get-class-declaration-interfaces declaration))
        (body (jazz:get-namespace-declaration-body declaration)))
    (let ((level-locator (%%compose-helper locator 'level)))
      (jazz:sourcify-deep-if
        `(begin
           ,@(if (jazz:core-class? name)
                 (let ((core-class (jazz:get-core-class name)))
                   (if (%%not (%%symbol? core-class))
                       (jazz:validate-core-class core-class declaration))
                   (let ((core-class-locator (if (%%symbol? core-class) core-class (%%get-category-identifier core-class)))
                         (ascendant-access (if (%%not ascendant-declaration) #f (jazz:sourcified-form (jazz:emit-binding-reference ascendant-declaration declaration walker resume environment backend)))))
                     `((define ,locator ,core-class-locator)
                       (define ,level-locator (%%get-class-level ,locator))
                       (jazz:set-core-class-redefined ',name ',core-class-locator))))
               (let ((metaclass-declaration (jazz:get-category-declaration-metaclass declaration)))
                 (let ((ascendant-access (jazz:emit-ascendant-access declaration walker resume environment backend)))
                   (let ((metaclass-access (if (%%not metaclass-declaration) (if (%%not ascendant-access) 'jazz:Object-Class `(%%get-object-class ,ascendant-access)) (jazz:sourcified-form (jazz:emit-binding-reference metaclass-declaration declaration walker resume environment backend))))
                         (interface-accesses (map (lambda (declaration) (jazz:sourcified-form (jazz:emit-binding-reference declaration declaration walker resume environment backend))) interface-declarations)))
                     `((define ,locator
                         ;; this is a quicky that needs to be well tought out
                         (if (jazz:global-bound? ',locator)
                             (jazz:global-ref ',locator)
                           (jazz:new-class ,metaclass-access ',locator ,ascendant-access (%%list ,@interface-accesses))))
                       (define ,level-locator (%%get-class-level ,locator)))))))
           ,@(let ((toplevel-declaration (jazz:get-declaration-toplevel declaration)))
               (if (jazz:get-generate? 'register)
                   `((jazz:register-module-entry ',(jazz:get-lexical-binding-name toplevel-declaration) ',name ,locator))
                 '()))
           ,@(jazz:emit-namespace-statements body declaration walker resume environment backend))
        (jazz:get-declaration-source declaration)))))


;;;
;;;; Interface
;;;


(jazz:define-emit (interface (scheme backend) declaration walker resume environment)
  (let* ((name (jazz:get-lexical-binding-name declaration))
         (locator (jazz:get-declaration-locator declaration))
         (rank-locator (%%compose-helper locator 'rank))
         (ascendant-declarations (jazz:get-interface-declaration-ascendants declaration))
         (metaclass-declaration (jazz:get-category-declaration-metaclass declaration))
         (metaclass-access (if (%%not metaclass-declaration) 'jazz:Interface (jazz:sourcified-form (jazz:emit-binding-reference metaclass-declaration declaration walker resume environment backend))))
         (ascendant-accesses (map (lambda (declaration) (jazz:sourcified-form (jazz:emit-binding-reference declaration declaration walker resume environment backend))) ascendant-declarations))
         (body (jazz:get-namespace-declaration-body declaration)))
    (jazz:sourcify-deep-if
      `(begin
         (define ,locator
           (jazz:new-interface ,metaclass-access ',locator (%%list ,@ascendant-accesses)))
         (define ,rank-locator
           (%%get-interface-rank ,locator))
         ,@(let ((toplevel-declaration (jazz:get-declaration-toplevel declaration)))
             (if (jazz:get-generate? 'register)
                 `((jazz:register-module-entry ',(jazz:get-lexical-binding-name toplevel-declaration) ',name ,locator))
               '()))
         ,@(jazz:emit-namespace-statements body declaration walker resume environment backend))
      (jazz:get-declaration-source declaration))))


;;;
;;;; Slot
;;;


(jazz:define-emit (slot (scheme backend) declaration walker resume environment initialize-emit)
  (let* ((name (jazz:get-lexical-binding-name declaration))
         (locator (jazz:get-declaration-locator declaration))
         (class-declaration (jazz:get-declaration-parent declaration))
         (class-locator (jazz:get-declaration-locator class-declaration))
         (allocate? (%%neq? (jazz:get-lexical-binding-type declaration) jazz:Void))
         (core? (jazz:core-class? (jazz:get-lexical-binding-name class-declaration)))
         (initialize (jazz:get-slot-declaration-initialize declaration))
         (initialize? (and allocate? (%%not core?) initialize))
         (initialize-locator (and initialize? (%%compose-helper locator 'initialize)))
         (slot-locator (%%compose-helper locator 'slot))
         (offset-locator (%%compose-helper locator 'offset)))
    (jazz:sourcify-deep-if
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
      (jazz:get-declaration-source declaration))))


;;;
;;;; Property
;;;


(jazz:define-emit (property (scheme backend) declaration walker resume environment initialize-emit)
  (let* ((name (jazz:get-lexical-binding-name declaration))
         (locator (jazz:get-declaration-locator declaration))
         (class-declaration (jazz:get-declaration-parent declaration))
         (class-locator (jazz:get-declaration-locator class-declaration))
         (allocate? (%%neq? (jazz:get-lexical-binding-type declaration) jazz:Void))
         (core? (jazz:core-class? (jazz:get-lexical-binding-name class-declaration)))
         (initialize (jazz:get-slot-declaration-initialize declaration))
         (initialize? (and allocate? (%%not core?) initialize))
         (initialize-locator (and initialize? (%%compose-helper locator 'initialize)))
         (slot-locator (%%compose-helper locator 'slot))
         (offset-locator (%%compose-helper locator 'offset))
         (getter (jazz:get-property-declaration-getter declaration))
         (setter (jazz:get-property-declaration-setter declaration)))
    ;; hack in a literal self instead of the hygienically renamed self
    (define (fix-self expr)
      (if (pair? expr)
          (cons (car expr) (cons (cons 'self (cdadr expr)) (cddr expr)))
        expr))
    (jazz:sourcify-deep-if
      `(begin
         ,@(if initialize?
               `((define (,initialize-locator self)
                   ,(jazz:sourcified-form initialize-emit)))
             '())
         (define ,slot-locator
           (jazz:add-property ,class-locator ',name ,initialize-locator ,allocate?
             ,(fix-self (jazz:sourcified-form (jazz:emit-expression getter declaration walker resume environment backend)))
             ,(fix-self (jazz:sourcified-form (jazz:emit-expression setter declaration walker resume environment backend)))))
         (define ,offset-locator
           (%%get-slot-offset ,slot-locator))
         ,@(jazz:declaration-result))
      (jazz:get-declaration-source declaration))))


;;;
;;;; Method
;;;


(jazz:define-emit (method (scheme backend) declaration walker resume environment signature-emit signature-casts body-emit unsafe-signature)
  (let* ((name (jazz:get-lexical-binding-name declaration))
         (abstraction (jazz:get-method-declaration-abstraction declaration))
         (propagation (jazz:get-method-declaration-propagation declaration))
         (category-declaration (jazz:get-declaration-parent declaration))
         (class-name (jazz:get-lexical-binding-name category-declaration))
         (class-locator (jazz:get-declaration-locator category-declaration))
         (core-method-node-locator (jazz:method-implementation-name (%%compose-reference 'jazz class-name) name))
         (method-locator (jazz:get-declaration-locator declaration))
         (method-rank-locator (%%compose-helper method-locator 'rank))
         (method-node-locator (%%compose-helper method-locator 'node))
         (unsafe-locator (and unsafe-signature (jazz:unsafe-locator method-locator)))
         (add-method-proc (cond ((%%class-is? category-declaration jazz:Class-Declaration)     (case propagation
                                                                                                 ((override)        'jazz:add-method-node)
                                                                                                 ((final)           'jazz:add-final-method)
                                                                                                 ((virtual chained) 'jazz:add-virtual-method)))
                                ((%%class-is? category-declaration jazz:Interface-Declaration) (case propagation
                                                                                                 ((override)        'jazz:add-method-node)
                                                                                                 ((virtual)         'jazz:add-virtual-method))))))
    (jazz:sourcify-deep-if
      (case add-method-proc
        ((jazz:add-final-method)
         `(begin
            ,@(if unsafe-locator
                  `((define (,unsafe-locator ,@signature-emit)
                      ,body-emit)
                    (define (,method-locator ,@signature-emit)
                      ,@(jazz:add-signature-casts signature-casts
                        `(,unsafe-locator ,@(map jazz:get-lexical-binding-name (jazz:get-signature-positional unsafe-signature))))))
                `((define (,method-locator ,@signature-emit)
                    ,@(jazz:add-signature-casts signature-casts body-emit))))
            (,add-method-proc ,class-locator ',name ,method-locator)
            ,@(jazz:declaration-result)))
        ((jazz:add-virtual-method)
         `(begin
            ,@(if body-emit
                  `((define (,method-locator ,@signature-emit)
                      ,@(jazz:add-signature-casts signature-casts body-emit)))
                (if (eq? abstraction 'abstract)
                    `((define (,method-locator . rest)
                        (jazz:call-into-abstract ',class-locator ',name rest)))
                  '()))
            (define ,method-rank-locator
              (,add-method-proc ,class-locator ',name
                ,(if (and (%%eq? abstraction 'core) (%%not body-emit))
                     core-method-node-locator
                   method-locator)))
            ,@(jazz:declaration-result)))
        ((jazz:add-method-node)
         (let ((node (jazz:generate-symbol "node")))
           `(begin
              ,@(if body-emit
                    `((define (,method-locator ,@signature-emit)
                        (let ((nextmethod (%%get-method-node-next-implementation ,method-node-locator)))
                          ,@(jazz:add-signature-casts signature-casts body-emit))))
                  '())
              (define ,method-node-locator
                (,add-method-proc ,class-locator ',name
                  ,(if (and (%%eq? abstraction 'core) (%%not body-emit))
                       core-method-node-locator
                     method-locator)))
              ,@(jazz:declaration-result)))))
      (jazz:get-declaration-source declaration))))


;;;
;;;; With-Self
;;;


(jazz:define-emit (with-self (scheme backend) expression declaration walker resume environment body-emit)
  (jazz:simplify-begin
    `(begin
       ,@(jazz:sourcified-form body-emit))))


;;;
;;;; With-Dynamic-Self
;;;


(jazz:define-emit (with-dynamic-self (scheme backend) expression declaration walker resume environment body-emit)
  (jazz:simplify-begin
    `(begin
       ,@(jazz:sourcified-form body-emit))))


;;;
;;;; Dispatch Call
;;;


(jazz:define-emit (dispatch-call (scheme backend) name source declaration walker resume environment object-argument object-code others-arguments others-codes)
  (define (resolve-type object-code)
    (let ((object-type (jazz:patch-type-until-unification (jazz:get-code-type object-code))))
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
            (%%when (and (or (jazz:reporting?) (jazz:warnings?)) (jazz:get-warn? 'optimizations))
              (jazz:warning "Warning: In {a}{a}: Unable to find dispatch method {a}"
                            (jazz:get-declaration-locator declaration)
                            (jazz:present-expression-location source #f)
                            name))
            #f)
        (begin
          (jazz:add-to-module-references declaration method-declaration)
          method-declaration))))
  
  (define (jazz:with-code-value code proc)
    (let ((form (jazz:get-code-form code)))
      (if (%%symbol? form)
          (proc code)
        (let ((value (jazz:generate-symbol "val")))
          (let ((code (proc (jazz:new-code value (jazz:get-code-type code) #f))))
            (jazz:new-code
              `(let ((,value ,form))
                 ,(jazz:get-code-form code))
              (jazz:get-code-type code)
              (jazz:get-code-source code)))))))
  
  (let ((method-declaration (lookup-method/warn object-code)))
    (if method-declaration
        (or (jazz:emit-inlined-final-dispatch-call source method-declaration object-code others-codes declaration walker resume environment backend)
            (jazz:with-code-value object-code
              (lambda (code)
                (jazz:emit-method-dispatch object-argument code source others-arguments others-codes method-declaration declaration walker resume environment backend))))
      (let ((dv (jazz:register-variable declaration (%%string-append (%%symbol->string name) "!d") #f)))
        (let ((d (%%car dv)))
          (%%set-cdr! dv `(jazz:cache-dispatch ',name (lambda (d) (set! ,d d))))
          (jazz:new-code
            (jazz:with-uniqueness (jazz:sourcified-form object-code)
              (lambda (object)
                `((,d ,object) ,object ,@(jazz:codes-forms others-codes))))
            jazz:Any
            source))))))


(define (jazz:emit-inlined-final-dispatch-call source declaration object arguments source-declaration walker resume environment backend)
  ;; mostly copy/pasted and adapted from method declaration. need to unify the code
  (if (%%eq? (jazz:get-method-declaration-expansion declaration) 'inline)
      (receive (dispatch-type method-declaration) (jazz:method-dispatch-info declaration)
        (case dispatch-type
          ((final)
           (let ((signature (jazz:get-method-declaration-signature declaration))
                 (body (jazz:get-method-declaration-body declaration)))
             (if (jazz:only-positional-signature? signature)
                 ;; the +1 is for the self that is now part of the method signature
                 (if (%%fx= (jazz:get-signature-mandatory signature) (%%fx+ 1 (%%length arguments)))
                     (jazz:with-annotated-frame (jazz:annotate-signature signature)
                       (lambda (frame)
                         (let ((augmented-environment (%%cons frame environment)))
                           (let ((body-code (jazz:emit-expression body source-declaration walker resume augmented-environment backend)))
                             (jazz:new-code
                               `(let ,(map (lambda (parameter argument)
                                             `(,(jazz:emit-binding-symbol parameter source-declaration environment backend)
                                               ,(jazz:emit-type-cast argument (jazz:get-lexical-binding-type parameter) source source-declaration walker resume environment backend)))
                                           (jazz:get-signature-positional signature)
                                           (%%cons object arguments))
                                  ,(jazz:get-code-form body-code))
                               (jazz:call-return-type (jazz:get-lexical-binding-type declaration))
                               #f)))))
                   (jazz:error "Wrong number of arguments passed to {s}" (jazz:get-lexical-binding-name declaration)))
               (jazz:error "Only positional parameters are supported in inlining: {s}" (jazz:get-lexical-binding-name declaration)))))
          (else
           #f)))
    #f))


;;;
;;;; Dispatch Reference
;;;


(jazz:define-emit (dispatch-reference (scheme backend) name source declaration walker resume environment)
  (jazz:new-code
    `(lambda (object . rest)
       (apply (jazz:dispatch (jazz:class-of object) ',name) object rest))
    jazz:Any
    source))


;;;
;;;; Cast
;;;


(jazz:define-emit (cast (scheme backend) expression declaration walker resume environment type expression-emit)
  (jazz:emit-type-cast
    expression-emit
    type
    (jazz:get-expression-source expression)
    declaration
    walker
    resume
    environment
    backend))


;;;
;;;; Allocate
;;;


(jazz:define-emit (allocate (scheme backend) expression declaration walker resume environment class-emit values-emit)
  `(%%object ,(jazz:sourcified-form class-emit)
             ,@(jazz:codes-forms values-emit)))


;;;
;;;; Static
;;;


(jazz:define-emit (static (scheme backend) expression declaration walker resume environment static)
  (%%car static))


;;;
;;;; Reference
;;;


(jazz:define-emit (c-definition-reference (scheme backend) declaration)
  (jazz:get-declaration-locator declaration))


(jazz:define-emit (category-reference (scheme backend) declaration)
  (jazz:get-declaration-locator declaration))


(jazz:define-emit (class-reference (scheme backend) declaration)
  (jazz:get-declaration-locator declaration))


(jazz:define-emit (definition-reference (scheme backend) declaration)
  (jazz:get-declaration-locator declaration))


(jazz:define-emit (dynamic-self-reference (scheme backend) declaration)
  (jazz:get-dynamic-self-binding-code declaration))


(jazz:define-emit (generic-reference (scheme backend) declaration)
  (jazz:get-declaration-locator declaration))


(jazz:define-emit (nextmethod-variable-reference (scheme backend) binding)
  (jazz:get-lexical-binding-name binding))


(jazz:define-emit (self-reference (scheme backend) declaration source-declaration)
  (jazz:get-declaration-parent source-declaration))


(jazz:define-emit (slot-reference (scheme backend) declaration self)
  (let ((offset-locator (%%compose-helper (jazz:get-declaration-locator declaration) 'offset)))
    `(%%object-ref ,(jazz:sourcified-form self) ,offset-locator)))


;;;
;;;; Call
;;;


(jazz:define-emit (call (scheme backend) expression declaration walker resume environment)
  (let ((operator (jazz:get-call-operator expression))
        (arguments (jazz:get-call-arguments expression)))
    (let ((locator (if (%%class-is? operator jazz:Binding-Reference)
                       (let ((binding (jazz:get-binding-reference-binding operator)))
                         (if (%%class-is? binding jazz:Declaration)
                             (jazz:get-declaration-locator binding)
                           #f))
                     #f))
          (arguments-codes (jazz:emit-expressions arguments declaration walker resume environment backend)))
      (jazz:sourcify-code
        (or (jazz:emit-specialized-call operator locator arguments arguments-codes expression declaration walker resume environment backend)
            (jazz:emit-new-call operator locator arguments arguments-codes declaration walker resume environment backend)
            (jazz:emit-primitive-call operator locator arguments arguments-codes declaration walker resume environment backend)
            (jazz:emit-inlined-call operator arguments-codes expression declaration walker resume environment backend)
            (jazz:emit-unsafe-call operator locator arguments arguments-codes declaration walker resume environment backend)
            (jazz:emit-call operator arguments arguments-codes declaration walker resume environment backend))
        (jazz:get-expression-source expression)))))


(jazz:define-emit (nextmethod-call (scheme backend) binding binding-src arguments)
  (let ((name (jazz:get-lexical-binding-name binding)))
    `(,name
       ,@(jazz:codes-forms arguments))))


;;;
;;;; Specialized Call
;;;


(jazz:define-emit (specialized-call (scheme backend) expression declaration operator arguments)
  #f)


;;;
;;;; Specialized Class-of Call
;;;


(jazz:define-emit (specialized-class-of-call (scheme backend) object)
  `(jazz:class-of ,(jazz:sourcified-form object)))


;;;
;;;; New Call
;;;


(jazz:define-emit (new-call (scheme backend) operator locator arguments arguments-codes declaration walker resume environment)
  (if (%%eq? locator 'jazz.language.runtime.kernel:new)
      (%%assert (%%pair? arguments)
        (let ((class-expression (%%car arguments)))
          (if (%%class-is? class-expression jazz:Binding-Reference)
              (let ((binding (jazz:get-binding-reference-binding class-expression)))
                (if (or (%%class-is? binding jazz:Class-Declaration)
                        (%%class-is? binding jazz:Autoload-Declaration))
                    (let ((values-codes (%%cdr arguments-codes)))
                      (jazz:new-code
                        `(%%new ,@(jazz:codes-forms arguments-codes))
                        binding
                        #f))
                  #f))
            #f)))
    #f))


;;;
;;;; Primitive Call
;;;


;; To make this a lot more clean would necessitate moving the specializer into the walk phase so that the
;; result of inlining can be Jazz code. With this we could specialize for instance (##length x) and ##length
;; would simply be an external typed as <list:int> which would do all type propagation automatically.
;; This is really difficult to achieve because as inlining can impact type inference it also needs
;; to be done at emit phase... Even better, all those should be specialized definitions in Jazz with support
;; for specializing based on static types...


(define jazz:*primitive-patterns*
  (%%make-table test: eq?))


(define (jazz:primitive-patterns-get)
  jazz:*primitive-patterns*)


(define (jazz:add-primitive-patterns operator patterns)
  (%%table-set! jazz:*primitive-patterns* operator
    (map (lambda (pattern)
           (let ((name (%%car pattern))
                 (specifier (%%cadr pattern)))
             (%%list name (jazz:walk-specifier #f #f #f '() specifier))))
         patterns)))


(define (jazz:get-primitive-patterns locator)
  (%%table-ref jazz:*primitive-patterns* locator #f))


(jazz:add-primitive-patterns     'scheme.language.runtime:=                           '((%%fx=  <fx*:bool>)  (%%fl=  <fv*:bool>)  (%%= <number^number:bool>)))
(jazz:add-primitive-patterns     'scheme.language.runtime:<                           '((%%fx<  <fx*:bool>)  (%%fl<  <fv*:bool>)))
(jazz:add-primitive-patterns     'scheme.language.runtime:<=                          '((%%fx<= <fx*:bool>)  (%%fl<= <fv*:bool>)))
(jazz:add-primitive-patterns     'scheme.language.runtime:>                           '((%%fx>  <fx*:bool>)  (%%fl>  <fv*:bool>)))
(jazz:add-primitive-patterns     'scheme.language.runtime:>=                          '((%%fx>= <fx*:bool>)  (%%fl>= <fv*:bool>)))

(jazz:add-primitive-patterns     'scheme.language.runtime:+                           '((%%fx+  <fx*:fx>)    (%%fl+  <fv*:fl>)    (%%+ <int^int:int>) (%%+ <number^number:number>)))
(jazz:add-primitive-patterns     'scheme.language.runtime:-                           '((%%fx-  <fx^fx*:fx>) (%%fl-  <fv^fv*:fl>) (%%- <int^int:int>) (%%- <number^number:number>)))
(jazz:add-primitive-patterns     'scheme.language.runtime:*                           '((%%fx*  <fx*:fx>)    (%%fl*  <fv*:fl>)    (%%* <int^int:int>) (%%* <number^number:number>)))

(jazz:add-primitive-patterns     'scheme.language.runtime:/                           '(                     (%%fl/  <fv^fv*:fl>)                     (%%/ <number^number:number>)))
(jazz:add-primitive-patterns     'scheme.language.runtime:quotient                    '((%%fxquotient <fx^fx:fx>)))

(jazz:add-primitive-patterns     'scheme.language.runtime:floor                       '(                     (%%flfloor    <fv:fl>)))
(jazz:add-primitive-patterns     'scheme.language.runtime:ceiling                     '(                     (%%flceiling  <fv:fl>)))
(jazz:add-primitive-patterns     'scheme.language.runtime:truncate                    '(                     (%%fltruncate <fv:fl>)))
(jazz:add-primitive-patterns     'scheme.language.runtime:round                       '(                     (%%flround    <fv:fl>)))

(jazz:add-primitive-patterns     'scheme.language.runtime:abs                         '((%%fxabs <fx:fx>)    (%%flabs      <fv:fl>)))
(jazz:add-primitive-patterns     'scheme.language.runtime:sqrt                        '(                     (%%flsqrt     <fv:fl>)))
(jazz:add-primitive-patterns     'scheme.language.runtime:expt                        '(                     (%%flexpt     <fv^fv:fl>)))
(jazz:add-primitive-patterns     'scheme.language.runtime:square                      '(                     (%%flsquare   <fv:fl>)))

(jazz:add-primitive-patterns     'scheme.language.runtime:sin                         '(                     (%%flsin      <fv:fl>)))
(jazz:add-primitive-patterns     'scheme.language.runtime:cos                         '(                     (%%flcos      <fv:fl>)))
(jazz:add-primitive-patterns     'scheme.language.runtime:tan                         '(                     (%%fltan      <fv:fl>)))
(jazz:add-primitive-patterns     'scheme.language.runtime:asin                        '(                     (%%flasin     <fv:fl>)))
(jazz:add-primitive-patterns     'scheme.language.runtime:acos                        '(                     (%%flacos     <fv:fl>)))
(jazz:add-primitive-patterns     'scheme.language.runtime:atan                        '(                     (%%flatan     <fv:fl>) (%%flatan <fv^fv:fl>)))

(jazz:add-primitive-patterns     'scheme.language.runtime:not                         '((%%not  <any:bool>)))
(jazz:add-primitive-patterns     'scheme.language.runtime:eq?                         '((%%eq?  <any^any:bool>)))
(jazz:add-primitive-patterns     'scheme.language.runtime:eqv?                        '((%%eqv? <any^any:bool>)))

(jazz:add-primitive-patterns     'scheme.language.runtime:car                         '((%%car    <pair:any>)))
(jazz:add-primitive-patterns     'scheme.language.runtime:cdr                         '((%%cdr    <pair:any>)))
(jazz:add-primitive-patterns     'scheme.language.runtime:cons                        '((%%cons   <any^any:pair>)))
(jazz:add-primitive-patterns     'scheme.language.runtime:length                      '((%%length <list:int>)     (%%vector-length <vector:int>)          (%%string-length <string:int>)))
(jazz:add-primitive-patterns     'scheme.language.runtime:null?                       '((%%null?  <any:bool>)))

(jazz:add-primitive-patterns     'scheme.language.runtime:string-length               '((%%string-length <string:fx>)))
(jazz:add-primitive-patterns     'scheme.language.runtime:string-ref                  '((%%string-ref    <string^fb:char>)))

(jazz:add-primitive-patterns     'scheme.language.runtime:vector-length               '((%%vector-length <vector:fx>)))
(jazz:add-primitive-patterns     'scheme.language.runtime:vector-ref                  '((%%vector-ref    <vector^fb:any>)))
(jazz:add-primitive-patterns     'scheme.language.runtime:vector-set!                 '((%%vector-set!   <vector^fb^any:void>)))

(jazz:add-primitive-patterns     'jazz.language.runtime.kernel:values-ref             '((%%values-ref  <values^fb:any>)))
(jazz:add-primitive-patterns     'jazz.language.runtime.kernel:values-set!            '((%%values-set! <values^fb^any:void>)))

(jazz:add-primitive-patterns     'gambit.language.runtime:table-ref                   '((%%table-ref  <table^any:any>) (%%table-ref    <table^any^any:any>)))
(jazz:add-primitive-patterns     'gambit.language.runtime:table-set!                  '((%%table-set! <table^any^any:void>)))

(jazz:add-primitive-patterns     'scheme.language.runtime:min                         '((%%fxmin <fx^fx:fx>) (%%flmin <fv^fv:fl>) (%%flmin <fv^fv^fv:fl>)))
(jazz:add-primitive-patterns     'scheme.language.runtime:max                         '((%%fxmax <fx^fx:fx>) (%%flmax <fv^fv:fl>) (%%flmax <fv^fv^fv:fl>)))
(jazz:add-primitive-patterns     'scheme.language.runtime:modulo                      '((%%fxmodulo <fx^fx:fx>)))
(jazz:add-primitive-patterns     'scheme.language.runtime:even?                       '((%%fxeven? <fx:bool>)))
(jazz:add-primitive-patterns     'scheme.language.runtime:odd?                        '((%%fxodd? <fx:bool>)))

(jazz:add-primitive-patterns     'gambit.language.runtime:fx+                         '((%%fx+ <fx^fx:fx>)))
(jazz:add-primitive-patterns     'gambit.language.runtime:fx-                         '((%%fx- <fx^fx:fx>)))
(jazz:add-primitive-patterns     'gambit.language.runtime:fx*                         '((%%fx* <fx^fx:fx>)))

(jazz:add-primitive-patterns     'gambit.language.runtime:fl+                         '(                     (%%fl+ <fv^fv:fl>)))
(jazz:add-primitive-patterns     'gambit.language.runtime:fl-                         '(                     (%%fl- <fv^fv:fl>)))
(jazz:add-primitive-patterns     'gambit.language.runtime:fl*                         '(                     (%%fl* <fv^fv:fl>)))
(jazz:add-primitive-patterns     'gambit.language.runtime:fl/                         '(                     (%%fl/ <fv^fv:fl>)))

(jazz:add-primitive-patterns     'gambit.language.runtime:nan?                        '(                     (%%flnan? <fv:bool>)))

(jazz:add-primitive-patterns     'gambit.language.runtime:bitwise-not                 '((%%fxnot <fx:fx>)))
(jazz:add-primitive-patterns     'gambit.language.runtime:bitwise-and                 '((%%fxand <fx^fx:fx>)))
(jazz:add-primitive-patterns     'gambit.language.runtime:bitwise-ior                 '((%%fxior <fx^fx:fx>)))
(jazz:add-primitive-patterns     'gambit.language.runtime:bitwise-xor                 '((%%fxxor <fx^fx:fx>)))
(jazz:add-primitive-patterns     'gambit.language.runtime:arithmetic-shift            '((%%fxarithmetic-shift <fx^fx:fx>)))
(jazz:add-primitive-patterns     'jazz.language.runtime.kernel:arithmetic-shift-left  '((%%fxarithmetic-shift-left <fx^fx:fx>)))
(jazz:add-primitive-patterns     'jazz.language.runtime.kernel:arithmetic-shift-right '((%%fxarithmetic-shift-right <fx^fx:fx>)))

(jazz:add-primitive-patterns     'jazz.language.runtime.kernel:fixnum->flonum         '((%%fixnum->flonum <fx:fl>)))
(jazz:add-primitive-patterns     'jazz.language.runtime.kernel:flonum->fixnum         '((%%flonum->fixnum <fv:fx>)))

(jazz:add-primitive-patterns     'gambit.language.runtime:s8vector                    '((%%s8vector         <fx*:s8vector>)))
(jazz:add-primitive-patterns     'gambit.language.runtime:s8vector-length             '((%%s8vector-length  <s8vector:fx>)))
(jazz:add-primitive-patterns     'gambit.language.runtime:s8vector-ref                '((%%s8vector-ref     <s8vector^fb:fx>)))
(jazz:add-primitive-patterns     'gambit.language.runtime:s8vector-set!               '((%%s8vector-set!    <s8vector^fb^any:void>)))

(jazz:add-primitive-patterns     'gambit.language.runtime:u8vector                    '((%%u8vector         <fx*:u8vector>)))
(jazz:add-primitive-patterns     'gambit.language.runtime:u8vector-length             '((%%u8vector-length  <u8vector:fx>)))
(jazz:add-primitive-patterns     'gambit.language.runtime:u8vector-ref                '((%%u8vector-ref     <u8vector^fb:fx>)))
(jazz:add-primitive-patterns     'gambit.language.runtime:u8vector-set!               '((%%u8vector-set!    <u8vector^fb^any:void>)))

(jazz:add-primitive-patterns     'gambit.language.runtime:s16vector                   '((%%s16vector        <fx*:s16vector>)))
(jazz:add-primitive-patterns     'gambit.language.runtime:s16vector-length            '((%%s16vector-length <s16vector:fx>)))
(jazz:add-primitive-patterns     'gambit.language.runtime:s16vector-ref               '((%%s16vector-ref    <s16vector^fb:fx>)))
(jazz:add-primitive-patterns     'gambit.language.runtime:s16vector-set!              '((%%s16vector-set!   <s16vector^fb^any:void>)))

(jazz:add-primitive-patterns     'gambit.language.runtime:u16vector                   '((%%u16vector        <fx*:u16vector>)))
(jazz:add-primitive-patterns     'gambit.language.runtime:u16vector-length            '((%%u16vector-length <u16vector:fx>)))
(jazz:add-primitive-patterns     'gambit.language.runtime:u16vector-ref               '((%%u16vector-ref    <u16vector^fb:fx>)))
(jazz:add-primitive-patterns     'gambit.language.runtime:u16vector-set!              '((%%u16vector-set!   <u16vector^fb^any:void>)))

(jazz:add-primitive-patterns     'gambit.language.runtime:s32vector                   '((%%s32vector        <fx*:s32vector>)))
(jazz:add-primitive-patterns     'gambit.language.runtime:s32vector-length            '((%%s32vector-length <s32vector:fx>)))
(jazz:add-primitive-patterns     'gambit.language.runtime:s32vector-ref               '((%%s32vector-ref    <s32vector^fb:fx>)))
(jazz:add-primitive-patterns     'gambit.language.runtime:s32vector-set!              '((%%s32vector-set!   <s32vector^fb^any:void>)))

(jazz:add-primitive-patterns     'gambit.language.runtime:u32vector                   '((%%u32vector        <fx*:u32vector>)))
(jazz:add-primitive-patterns     'gambit.language.runtime:u32vector-length            '((%%u32vector-length <u32vector:fx>)))
(jazz:add-primitive-patterns     'gambit.language.runtime:u32vector-ref               '((%%u32vector-ref    <u32vector^fb:fx>)))
(jazz:add-primitive-patterns     'gambit.language.runtime:u32vector-set!              '((%%u32vector-set!   <u32vector^fb^any:void>)))

(jazz:add-primitive-patterns     'gambit.language.runtime:s64vector                   '((%%s64vector        <fx*:s64vector>)))
(jazz:add-primitive-patterns     'gambit.language.runtime:s64vector-length            '((%%s64vector-length <s64vector:fx>)))
(jazz:add-primitive-patterns     'gambit.language.runtime:s64vector-ref               '((%%s64vector-ref    <s64vector^fb:fx>)))
(jazz:add-primitive-patterns     'gambit.language.runtime:s64vector-set!              '((%%s64vector-set!   <s64vector^fb^any:void>)))

(jazz:add-primitive-patterns     'gambit.language.runtime:u64vector                   '((%%u64vector        <fx*:u64vector>)))
(jazz:add-primitive-patterns     'gambit.language.runtime:u64vector-length            '((%%u64vector-length <u64vector:fx>)))
(jazz:add-primitive-patterns     'gambit.language.runtime:u64vector-ref               '((%%u64vector-ref    <u64vector^fb:fx>)))
(jazz:add-primitive-patterns     'gambit.language.runtime:u64vector-set!              '((%%u64vector-set!   <u64vector^fb^any:void>)))

(jazz:add-primitive-patterns     'gambit.language.runtime:f32vector                   '((%%f32vector        <fl*:f32vector>)))
(jazz:add-primitive-patterns     'gambit.language.runtime:f32vector-length            '((%%f32vector-length <f32vector:fx>)))
(jazz:add-primitive-patterns     'gambit.language.runtime:f32vector-ref               '((%%f32vector-ref    <f32vector^fb:fl>)))
(jazz:add-primitive-patterns     'gambit.language.runtime:f32vector-set!              '((%%f32vector-set!   <f32vector^fb^any:void>)))

(jazz:add-primitive-patterns     'gambit.language.runtime:f64vector                   '((%%f64vector        <fl*:f64vector>)))
(jazz:add-primitive-patterns     'gambit.language.runtime:f64vector-length            '((%%f64vector-length <f64vector:fx>)))
(jazz:add-primitive-patterns     'gambit.language.runtime:f64vector-ref               '((%%f64vector-ref    <f64vector^fb:fl>)))
(jazz:add-primitive-patterns     'gambit.language.runtime:f64vector-set!              '((%%f64vector-set!   <f64vector^fb^any:void>)))

;; use at your own risk versions that do not initialize memory
(jazz:add-primitive-patterns     'jazz.language.runtime.functional:allocate-s8vector  '((%%allocate-s8vector  <any*:s8vector>)))
(jazz:add-primitive-patterns     'jazz.language.runtime.functional:allocate-u8vector  '((%%allocate-u8vector  <any*:u8vector>)))
(jazz:add-primitive-patterns     'jazz.language.runtime.functional:allocate-s16vector '((%%allocate-s16vector <any*:s16vector>)))
(jazz:add-primitive-patterns     'jazz.language.runtime.functional:allocate-u16vector '((%%allocate-u16vector <any*:u16vector>)))
(jazz:add-primitive-patterns     'jazz.language.runtime.functional:allocate-s32vector '((%%allocate-s32vector <any*:s32vector>)))
(jazz:add-primitive-patterns     'jazz.language.runtime.functional:allocate-u32vector '((%%allocate-u32vector <any*:u32vector>)))
(jazz:add-primitive-patterns     'jazz.language.runtime.functional:allocate-s64vector '((%%allocate-s64vector <any*:s64vector>)))
(jazz:add-primitive-patterns     'jazz.language.runtime.functional:allocate-u64vector '((%%allocate-u64vector <any*:u64vector>)))
(jazz:add-primitive-patterns     'jazz.language.runtime.functional:allocate-f32vector '((%%allocate-f32vector <any*:f32vector>)))
(jazz:add-primitive-patterns     'jazz.language.runtime.functional:allocate-f64vector '((%%allocate-f64vector <any*:f64vector>)))
(jazz:add-primitive-patterns     'jazz.language.runtime.functional:allocate-vector    '((%%allocate-vector    <any*:vector>)))

;; tracking allocations
(jazz:add-primitive-patterns     'scheme.language.runtime:make-string                 '((%%make-string    <any*:string>)))
(jazz:add-primitive-patterns     'scheme.language.runtime:make-vector                 '((%%make-vector    <any*:vector>)))
(jazz:add-primitive-patterns     'gambit.language.runtime:make-s8vector               '((%%make-s8vector  <any*:s8vector>)))
(jazz:add-primitive-patterns     'gambit.language.runtime:make-u8vector               '((%%make-u8vector  <any*:u8vector>)))
(jazz:add-primitive-patterns     'gambit.language.runtime:make-s16vector              '((%%make-s16vector <any*:s16vector>)))
(jazz:add-primitive-patterns     'gambit.language.runtime:make-u16vector              '((%%make-u16vector <any*:u16vector>)))
(jazz:add-primitive-patterns     'gambit.language.runtime:make-s32vector              '((%%make-s32vector <any*:s32vector>)))
(jazz:add-primitive-patterns     'gambit.language.runtime:make-u32vector              '((%%make-u32vector <any*:u32vector>)))
(jazz:add-primitive-patterns     'gambit.language.runtime:make-s64vector              '((%%make-s64vector <any*:s64vector>)))
(jazz:add-primitive-patterns     'gambit.language.runtime:make-u64vector              '((%%make-u64vector <any*:u64vector>)))
(jazz:add-primitive-patterns     'gambit.language.runtime:make-f32vector              '((%%make-f32vector <any*:f32vector>)))
(jazz:add-primitive-patterns     'gambit.language.runtime:make-f64vector              '((%%make-f64vector <any*:f64vector>)))
(jazz:add-primitive-patterns     'gambit.language.runtime:make-table                  '((%%make-table     <any*:table>)))
(jazz:add-primitive-patterns     'gambit.language.runtime:make-condition-variable     '((%%make-condition-variable <any*:any>)))
(jazz:add-primitive-patterns     'gambit.language.runtime:make-mutex                  '((%%make-mutex     <any*:any>)))
(jazz:add-primitive-patterns     'gambit.language.runtime:make-thread                 '((%%make-thread    <any*:any>)))
(jazz:add-primitive-patterns     'gambit.language.runtime:make-will                   '((%%make-will      <any*:any>)))
(jazz:add-primitive-patterns     'gambit.language.runtime:make-parameter              '((%%make-parameter <any*:any>)))

(jazz:add-primitive-patterns     'jazz.language.runtime.kernel:flref                  '((%%flref <fv^fb:fl>)))
(jazz:add-primitive-patterns     'jazz.language.runtime.kernel:flset!                 '((%%flset! <fv^fb^fv:void>)))

(%%when (%%not jazz:debug-user?)
  (jazz:add-primitive-patterns   'jazz.language.runtime.functional:element            '((list-ref <list^int:any>) (%%vector-ref    <vector^int:any>)      (%%string-ref    <string^int:char>)))
  (jazz:add-primitive-patterns   'jazz.language.runtime.functional:set-element!       '(                          (%%vector-set!   <vector^int^any:void>) (%%string-set!   <string^int^char:void>))))

(jazz:add-primitive-patterns     'jazz.language.runtime.functional:between?           '((%%fxbetween? <fx^fx^fx:bool>)))

(jazz:add-primitive-patterns     'jazz.language.runtime.kernel:current-seconds!       '((%%get-current-time! <f64vector^fb:void>)))
(jazz:add-primitive-patterns     'jazz.language.runtime.kernel:bytes-allocated!       '((%%get-bytes-allocated! <f64vector^fb:void>)))


(jazz:define-emit (primitive-call (scheme backend) operator locator arguments arguments-codes declaration walker resume environment)
  (if (%%not locator)
      #f
    (let ((patterns (jazz:get-primitive-patterns locator)))
      (if (%%not patterns)
          #f
        (let ((types (jazz:codes-types arguments-codes)))
          (let iter ((scan patterns) (least-mismatch #f))
               (if (%%null? scan)
                   (begin
                     (%%when (and (or (jazz:reporting?) (jazz:warnings?)) (jazz:get-warn? 'optimizations)
                                  ;; a bit extreme for now
                                  (%%not (%%memq locator '(scheme.language.runtime:car
                                                           scheme.language.runtime:cdr))))
                       (let ((expression (and (%%pair? least-mismatch) (%%car least-mismatch))))
                         (jazz:warning "Warning: In {a}{a}: Unmatched call to primitive {a}"
                                       (jazz:get-declaration-locator declaration)
                                       (jazz:present-expression-location (and expression (jazz:get-expression-source expression)) (jazz:get-expression-source operator))
                                       (jazz:reference-name locator))))
                     #f)
                 (jazz:bind (name function-type) (%%car scan)
                   (let ((mismatch (jazz:signature-mismatch arguments types function-type)))
                     (if (%%not mismatch)
                         (jazz:new-code
                           `(,name ,@(jazz:codes-forms arguments-codes))
                           (jazz:get-function-type-result function-type)
                           #f)
                       (iter (%%cdr scan) (if (or (%%not least-mismatch)
                                                  (%%symbol? least-mismatch)
                                                  (and (%%pair? mismatch)
                                                       (%%fx< (%%length mismatch) (%%length least-mismatch))))
                                              mismatch
                                            least-mismatch))))))))))))


;;;
;;;; Inlined Call
;;;


(jazz:define-emit (inlined-call (scheme backend) expression declaration operator arguments)
  #f)


;;;
;;;; Unsafe Call
;;;


(jazz:define-emit (unsafe-call (scheme backend) operator locator arguments arguments-codes declaration walker resume environment)
  (and jazz:debug-user?
       (and (%%class-is? operator jazz:Binding-Reference)
            (let ((binding (jazz:get-binding-reference-binding operator)))
              (cond ((%%class-is? binding jazz:Definition-Declaration)
                     (let ((type (jazz:get-lexical-binding-type binding)))
                       (and (%%is? type jazz:Function-Type)
                            ;; fail safe as this should never occur as inlined-call is before
                            (%%neq? (jazz:get-definition-declaration-expansion binding) 'inline)
                            ;; safe first iteration simplification
                            (jazz:only-positional-function-type? type)
                            (jazz:typed-function-type? type)
                            (let ((types (jazz:codes-types arguments-codes)))
                              (let ((mismatch (jazz:signature-mismatch arguments types type #t)))
                                ;; quick solution to calling an inline function from
                                ;; a script that does not have access to source code
                                (cond ((%%not jazz:kernel-source-access?)
                                       #f)
                                      ((or (%%not mismatch)
                                           (%%not (jazz:get-generate? 'check)))
                                       (jazz:new-code
                                         (let ((locator (jazz:unsafe-locator locator)))
                                           `(,locator ,@(map (lambda (code type)
                                                               (jazz:emit-implicit-cast code type))
                                                             arguments-codes
                                                             (jazz:get-function-type-positional type))))
                                         (jazz:get-function-type-result type)
                                         #f))
                                      (else
                                       (%%when (and (or (jazz:reporting?) (jazz:warnings?)) (jazz:get-warn? 'optimizations))
                                         (let ((expression (and (%%pair? mismatch) (%%car mismatch))))
                                           (jazz:warning "Warning: In {a}{a}: Unmatched call to typed definition {a}"
                                                         (jazz:get-declaration-locator declaration)
                                                         (jazz:present-expression-location (and expression (jazz:get-expression-source expression)) (jazz:get-expression-source operator))
                                                         (jazz:reference-name locator))))
                                       #f)))))))
                    ((%%class-is? binding jazz:Internal-Define-Variable)
                     (let ((type (jazz:get-lexical-binding-type binding)))
                       (and (%%is? type jazz:Function-Type)
                            ;; safe first iteration simplification
                            (jazz:only-positional-function-type? type)
                            (jazz:typed-function-type? type)
                            (let ((types (jazz:codes-types arguments-codes)))
                              (let ((mismatch (jazz:signature-mismatch arguments types type #t)))
                                (if (or (%%not mismatch)
                                        (%%not (jazz:get-generate? 'check)))
                                    (jazz:new-code
                                      `(,(jazz:get-lexical-binding-name binding) ,@(map (lambda (code type)
                                                                                          (jazz:emit-implicit-cast code type))
                                                                                        arguments-codes
                                                                                        (jazz:get-function-type-positional type)))
                                      (jazz:get-function-type-result type)
                                      #f)
                                  (begin
                                    (let ((expression (and (%%pair? mismatch) (%%car mismatch))))
                                      (jazz:unsafe-warning "Unsafe: In {a}{a}: Unmatched call to typed internal define {a}"
                                                           (jazz:get-declaration-locator declaration)
                                                           (jazz:present-expression-location (and expression (jazz:get-expression-source expression)) (jazz:get-expression-source operator))
                                                           (jazz:get-lexical-binding-name binding))
                                      #; ;; waiting for warning tooltip
                                      (display (jazz:format "  Unsafe: {l}{%}" (%%apply append (map (lambda (arg type)
                                                                                                      (if (%%class-is? arg jazz:Binding-Reference)
                                                                                                          (let ((variable (jazz:get-binding-reference-binding arg)))
                                                                                                            (%%list (jazz:get-lexical-binding-name variable)
                                                                                                                    (jazz:type->specifier type)))
                                                                                                        '(unknown unknown)))
                                                                                                    arguments
                                                                                                    types)))))
                                    #f)))))))
                    (else
                     #f))))))


;;;
;;;; Assignment
;;;


(jazz:define-emit (definition-assignment (scheme backend) declaration source-declaration walker resume environment value-code)
  (let ((locator (jazz:get-declaration-locator declaration)))
    `(set! ,locator ,(jazz:emit-type-cast value-code (jazz:get-lexical-binding-type declaration) source-declaration declaration walker resume environment backend))))


(jazz:define-emit (slot-assignment (scheme backend) declaration source-declaration walker resume environment self value-code)
  (let ((offset-locator (%%compose-helper (jazz:get-declaration-locator declaration) 'offset)))
    `(%%object-set! ,(jazz:sourcified-form self) ,offset-locator ,(jazz:emit-type-cast value-code (jazz:get-lexical-binding-type declaration) source-declaration declaration walker resume environment backend)))))
