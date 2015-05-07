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


(unit protected jazz.backend.scheme.emit


;;;
;;;; Definition
;;;


(jazz:define-emit (definition (scheme backend) declaration environment expression)
  (jazz:sourcify-if
    (let ((locator (jazz:get-declaration-locator declaration)))
      `(begin
         (define ,locator
           ,expression)
         ,(let ((name (jazz:get-lexical-binding-name declaration))
                (parent (jazz:get-declaration-parent declaration)))
            (if (%%is? parent jazz:Module-Declaration)
                `(jazz:register-definition ',(jazz:get-lexical-binding-name parent) ',name ',locator)
              `(jazz:add-field ,(jazz:get-declaration-locator parent) (jazz:new-definition ',name ',locator))))))
    (jazz:get-declaration-source declaration)))


;;;
;;;; Generic
;;;


(jazz:define-emit (generic (scheme backend) declaration environment signature-emit body-emit)
  (let ((generic-locator (jazz:get-declaration-locator declaration)))
    `(jazz:define-generic ,(%%cons generic-locator signature-emit)
       ,@(jazz:sourcified-form body-emit))))


;;;
;;;; Specific
;;;


(jazz:define-emit (specific (scheme backend) declaration environment signature-emit body-emit)
  (let ((generic-declaration (jazz:get-specific-declaration-generic declaration)))
    (let ((generic-locator (jazz:get-declaration-locator generic-declaration))
          (modifier (if (jazz:get-specific-declaration-root? declaration) 'root 'child)))
      `(jazz:define-specific ,(%%cons generic-locator signature-emit) ,modifier
         ,@(jazz:sourcified-form body-emit)))))


;;;
;;;; Class
;;;


(jazz:define-emit (class (scheme backend) declaration environment)
  (let ((name (jazz:get-lexical-binding-name declaration))
        (locator (jazz:get-declaration-locator declaration))
        (ascendant-declaration (jazz:get-class-declaration-ascendant declaration))
        (interface-declarations (jazz:get-class-declaration-interfaces declaration))
        (body (jazz:get-namespace-declaration-body declaration)))
    (let ((level-locator (%%compose-helper locator 'level)))
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
                       (jazz:set-core-class-redefined ',name ',core-class-locator))))
               (let ((metaclass-declaration (jazz:get-category-declaration-metaclass declaration)))
                 (let ((ascendant-access (jazz:emit-ascendant-access declaration environment backend)))
                   (let ((metaclass-access (if (%%not metaclass-declaration) (if (%%not ascendant-access) 'jazz:Object-Class `(%%get-object-class ,ascendant-access)) (jazz:sourcified-form (jazz:emit-binding-reference metaclass-declaration declaration environment backend))))
                         (interface-accesses (map (lambda (declaration) (jazz:sourcified-form (jazz:emit-binding-reference declaration declaration environment backend))) interface-declarations)))
                     `((define ,locator
                         ;; this is a quicky that needs to be well tought out
                         (if (jazz:global-bound? ',locator)
                             (jazz:global-ref ',locator)
                           (jazz:new-class ,metaclass-access ',locator ,ascendant-access (%%list ,@interface-accesses))))
                       (define ,level-locator (%%get-class-level ,locator)))))))
           ,(let ((toplevel-declaration (jazz:get-declaration-toplevel declaration)))
              `(jazz:register-module-entry ',(jazz:get-lexical-binding-name toplevel-declaration) ',name ,locator))
           ,@(jazz:emit-namespace-statements body declaration environment backend))
        (jazz:get-declaration-source declaration)))))


;;;
;;;; Interface
;;;


(jazz:define-emit (interface (scheme backend) declaration environment)
  (let* ((name (jazz:get-lexical-binding-name declaration))
         (locator (jazz:get-declaration-locator declaration))
         (rank-locator (%%compose-helper locator 'rank))
         (ascendant-declarations (jazz:get-interface-declaration-ascendants declaration))
         (metaclass-declaration (jazz:get-category-declaration-metaclass declaration))
         (metaclass-access (if (%%not metaclass-declaration) 'jazz:Interface (jazz:sourcified-form (jazz:emit-binding-reference metaclass-declaration declaration environment backend))))
         (ascendant-accesses (map (lambda (declaration) (jazz:sourcified-form (jazz:emit-binding-reference declaration declaration environment backend))) ascendant-declarations))
         (body (jazz:get-namespace-declaration-body declaration)))
    (jazz:sourcify-if
      `(begin
         (define ,locator
           (jazz:new-interface ,metaclass-access ',locator (%%list ,@ascendant-accesses)))
         (define ,rank-locator
           (%%get-interface-rank ,locator))
         ,(let ((toplevel-declaration (jazz:get-declaration-toplevel declaration)))
            `(jazz:register-module-entry ',(jazz:get-lexical-binding-name toplevel-declaration) ',name ,locator))
         ,@(jazz:emit-namespace-statements body declaration environment backend))
      (jazz:get-declaration-source declaration))))


;;;
;;;; Slot
;;;


(jazz:define-emit (slot (scheme backend) declaration environment initialize-emit)
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
      (jazz:get-declaration-source declaration))))


;;;
;;;; Property
;;;


(jazz:define-emit (property (scheme backend) declaration environment initialize-emit)
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
      (jazz:get-declaration-source declaration))))


;;;
;;;; Method
;;;


(jazz:define-emit (method (scheme backend) declaration environment signature-emit signature-casts body-emit)
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
         (add-method-proc (cond ((%%class-is? category-declaration jazz:Class-Declaration)     (case propagation
                                                                                                 ((override)        'jazz:add-method-node)
                                                                                                 ((final)           'jazz:add-final-method)
                                                                                                 ((virtual chained) 'jazz:add-virtual-method)))
                                ((%%class-is? category-declaration jazz:Interface-Declaration) (case propagation
                                                                                                 ((override)        'jazz:add-method-node)
                                                                                                 ((virtual)         'jazz:add-virtual-method))))))
    (jazz:sourcify-if
      (case add-method-proc
        ((jazz:add-final-method)
         `(begin
            (define (,method-locator self ,@signature-emit)
              ,@signature-casts
              (let ()
                ,body-emit))
            (,add-method-proc ,class-locator ',name ,method-locator)
            ,@(jazz:declaration-result)))
        ((jazz:add-virtual-method)
         `(begin
            ,@(if body-emit
                  `((define (,method-locator self ,@signature-emit)
                      ,@signature-casts
                      (let ()
                        ,body-emit)))
                (if (eq? abstraction 'abstract)
                    `((define (,method-locator self . rest)
                        (jazz:call-into-abstract ',class-locator ',name self rest)))
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
                    `((define (,method-locator self ,@signature-emit)
                        ,@signature-casts
                        (let ((nextmethod (%%get-method-node-next-implementation ,method-node-locator)))
                          ,body-emit)))
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
  (let ((name (jazz:get-dispatch-name expression)))
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
              (%%when (and (jazz:warnings?) (jazz:get-module-warn? (jazz:get-declaration-toplevel declaration) 'optimizations))
                (jazz:feedback "Warning: In {a}{a}: Unable to find dispatch method {a}"
                               (jazz:get-declaration-locator declaration)
                               (jazz:present-expression-location expression)
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
          (or (jazz:emit-inlined-final-dispatch expression method-declaration object-code rest-codes declaration environment backend)
              (jazz:with-code-value object-code
                (lambda (code)
                  (let ((dispatch-code (jazz:emit-method-dispatch code method-declaration declaration environment backend)))
                    (jazz:new-code
                      `(,(jazz:sourcified-form dispatch-code)
                        ,(jazz:sourcified-form code)
                        ,@(jazz:codes-forms rest-codes))
                      (jazz:get-code-type dispatch-code)
                      (jazz:get-expression-source expression))))))
        (let ((dv (jazz:register-variable declaration (%%string-append (%%symbol->string name) "!d") #f)))
          (let ((d (%%car dv)))
            (%%set-cdr! dv `(jazz:cache-dispatch ',name (lambda (d) (set! ,d d))))
            (jazz:new-code
              (jazz:with-uniqueness (jazz:sourcified-form object-code)
                (lambda (object)
                  `((,d ,object) ,object ,@(jazz:codes-forms rest-codes))))
              jazz:Any
              (jazz:get-expression-source expression))))))))


;;;
;;;; Cast
;;;


(jazz:define-emit (cast (scheme backend) expression declaration environment type expression-emit)
  (jazz:emit-type-cast
    expression-emit
    type
    expression
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
;;;; Static
;;;


(jazz:define-emit (static (scheme backend) expression declaration environment static)
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


(jazz:define-emit (method-reference (scheme backend) declaration source-declaration environment self dispatch-code)
  `(lambda rest
     (apply ,(jazz:sourcified-form dispatch-code) ,(jazz:sourcified-form self) rest)))


(jazz:define-emit (method-node-reference (scheme backend) expression declaration environment)
  (let ((method-declaration (jazz:get-binding-reference-binding expression)))
    (jazz:get-declaration-locator method-declaration)))


(jazz:define-emit (nextmethod-variable-reference (scheme backend) binding)
  (let ((name (jazz:get-lexical-binding-name binding))
        (self (jazz:*self*)))
    (if self
        `(lambda rest (apply ,name ,(jazz:sourcified-form self) rest))
      name)))


(jazz:define-emit (self-reference (scheme backend) declaration source-declaration)
  (jazz:get-declaration-parent source-declaration))


(jazz:define-emit (slot-reference (scheme backend) declaration self)
  (let ((offset-locator (%%compose-helper (jazz:get-declaration-locator declaration) 'offset)))
    `(%%object-ref ,(jazz:sourcified-form self) ,offset-locator)))


;;;
;;;; Call
;;;


(jazz:define-emit (call (scheme backend) expression declaration environment)
  (let ((operator (jazz:get-call-operator expression))
        (arguments (jazz:get-call-arguments expression)))
    (let ((locator (if (%%class-is? operator jazz:Binding-Reference)
                       (let ((binding (jazz:get-binding-reference-binding operator)))
                         (if (%%class-is? binding jazz:Declaration)
                             (jazz:get-declaration-locator binding)
                           #f))
                     #f))
          (arguments-codes (jazz:emit-expressions arguments declaration environment backend)))
      (jazz:sourcify-code
        (or (jazz:emit-specialized-call operator locator arguments arguments-codes expression declaration environment backend)
            (jazz:emit-new-call operator locator arguments arguments-codes declaration environment backend)
            (jazz:emit-primitive-call operator locator arguments arguments-codes declaration environment backend)
            (jazz:emit-inlined-call operator arguments-codes expression declaration environment backend)
            (jazz:emit-call operator arguments-codes declaration environment backend))
        (jazz:get-expression-source expression)))))


(jazz:define-emit (method-node-call (scheme backend) expression declaration operator arguments)
  `(,(jazz:sourcified-form operator) ,@(jazz:codes-forms arguments)))


(jazz:define-emit (method-binding-call (scheme backend) binding binding-src dispatch-code self arguments)
  `(,(jazz:sourcified-form dispatch-code)
    ,(jazz:sourcified-form self)
    ,@arguments))


(jazz:define-emit (nextmethod-binding-call (scheme backend) binding binding-src self arguments)
  (let ((name (jazz:get-lexical-binding-name binding)))
    (if self
        `(,name
           ,(jazz:sourcified-form self)
           ,@(jazz:codes-forms arguments))
      `(,name
         ,@(jazz:codes-forms arguments)))))


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


(jazz:define-emit (new-call (scheme backend) operator locator arguments arguments-codes declaration environment)
  (if (%%eq? locator 'jazz.language.runtime.kernel:new)
      (%%assert (%%pair? arguments)
        (let ((class-expression (%%car arguments)))
          (if (%%class-is? class-expression jazz:Binding-Reference)
              (let ((binding (jazz:get-binding-reference-binding class-expression)))
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
    #f))


;;;
;;;; Primitive Call
;;;


;; We should really expand using %% primitives but this cannot be used directly as we want for
;; instance %%+ to expand into ##+ or + depending on safety level but because the code is expanded
;; in Jazz code, the + would get captured as we have no way of safely saying scheme.+ in Scheme...


;; To make this alot more clean would necessitate moving the specializer into the walk phase so that the
;; result of inlining can be Jazz code. With this we could specialize for instance (##length x) and ##length
;; would simply be an external typed as <list:int> which would do all type propagation automatically.
;; This is really difficult to achieve because as inlining can impact type inference it also needs
;; to be done at emit phase... Even better, all those should be specialized definitions in Jazz with support
;; for specializing based on static types...


(define jazz:*primitive-patterns*
  '())


(define (jazz:initialize-primitive-patterns)
  (let ((table (%%make-table test: eq?)))
    (for-each (lambda (pair)
                (let ((operator (%%car pair))
                      (patterns (%%cdr pair)))
                  (%%table-set! table operator
                    (map (lambda (pattern)
                           (let ((name (%%car pattern))
                                 (specifier (%%cadr pattern)))
                             (%%list name (jazz:walk-specifier #f #f #f '() specifier))))
                         patterns))))
              jazz:*primitive-patterns*)
    (set! jazz:*primitive-patterns* table)))


(define (jazz:add-primitive-patterns operator patterns)
  (set! jazz:*primitive-patterns* (%%cons (%%cons operator patterns) jazz:*primitive-patterns*)))


(define (jazz:get-primitive-patterns locator)
  (%%table-ref jazz:*primitive-patterns* locator '()))


(jazz:add-primitive-patterns 'scheme.language.runtime.kernel:=                    '((##fx=  <fx*:bool>)  (##fl=  <fl*:bool>)  (##= <number^number:bool>)))
(jazz:add-primitive-patterns 'scheme.language.runtime.kernel:<                    '((##fx<  <fx*:bool>)  (##fl<  <fl*:bool>)))
(jazz:add-primitive-patterns 'scheme.language.runtime.kernel:<=                   '((##fx<= <fx*:bool>)  (##fl<= <fl*:bool>)))
(jazz:add-primitive-patterns 'scheme.language.runtime.kernel:>                    '((##fx>  <fx*:bool>)  (##fl>  <fl*:bool>)))
(jazz:add-primitive-patterns 'scheme.language.runtime.kernel:>=                   '((##fx>= <fx*:bool>)  (##fl>= <fl*:bool>)))

(jazz:add-primitive-patterns 'scheme.language.runtime.kernel:+                    '((##fx+  <fx*:fx>)    (##fl+  <fl*:fl>)    (##+ <int^int:int>) (##+ <number^number:number>)))
(jazz:add-primitive-patterns 'scheme.language.runtime.kernel:-                    '((##fx-  <fx^fx*:fx>) (##fl-  <fl^fl*:fl>) (##- <int^int:int>) (##- <number^number:number>)))
(jazz:add-primitive-patterns 'scheme.language.runtime.kernel:*                    '((##fx*  <fx*:fx>)    (##fl*  <fl*:fl>)    (##* <int^int:int>) (##* <number^number:number>)))

;; only done in release as these can crash on a division by zero
(cond-expand
  (release
    (jazz:add-primitive-patterns 'scheme.language.runtime.kernel:/                '(                     (##fl/  <fl^fl*:fl>)                     (##/ <number^number:number>)))
    (jazz:add-primitive-patterns 'scheme.language.runtime.kernel:quotient         '((##fxquotient <fx^fx:fx>))))
  (else))

(jazz:add-primitive-patterns 'scheme.language.runtime.kernel:floor                '(                     (##flfloor    <fl:fl>)))
(jazz:add-primitive-patterns 'scheme.language.runtime.kernel:ceiling              '(                     (##flceiling  <fl:fl>)))
(jazz:add-primitive-patterns 'scheme.language.runtime.kernel:truncate             '(                     (##fltruncate <fl:fl>)))
(jazz:add-primitive-patterns 'scheme.language.runtime.kernel:round                '(                     (##flround    <fl:fl>)))

(jazz:add-primitive-patterns 'scheme.language.runtime.kernel:abs                  '(                     (##flabs      <fl:fl>)))
(jazz:add-primitive-patterns 'scheme.language.runtime.kernel:sqrt                 '(                     (##flsqrt     <fl:fl>)))

(jazz:add-primitive-patterns 'scheme.language.runtime.kernel:sin                  '(                     (##flsin      <fl:fl>)))
(jazz:add-primitive-patterns 'scheme.language.runtime.kernel:cos                  '(                     (##flcos      <fl:fl>)))
(jazz:add-primitive-patterns 'scheme.language.runtime.kernel:tan                  '(                     (##fltan      <fl:fl>)))
(jazz:add-primitive-patterns 'scheme.language.runtime.kernel:asin                 '(                     (##flasin     <fl:fl>)))
(jazz:add-primitive-patterns 'scheme.language.runtime.kernel:acos                 '(                     (##flacos     <fl:fl>)))
(jazz:add-primitive-patterns 'scheme.language.runtime.kernel:atan                 '(                     (##flatan     <fl:fl>) (##flatan <fl^fl:fl>)))

(jazz:add-primitive-patterns 'scheme.language.runtime.kernel:not                  '((##not  <any:bool>)))
(jazz:add-primitive-patterns 'scheme.language.runtime.kernel:eq?                  '((##eq?  <any^any:bool>)))
(jazz:add-primitive-patterns 'scheme.language.runtime.kernel:eqv?                 '((##eqv? <any^any:bool>)))

(jazz:add-primitive-patterns 'scheme.language.runtime.kernel:car                  '((##car    <pair:any>)))
(jazz:add-primitive-patterns 'scheme.language.runtime.kernel:cdr                  '((##cdr    <pair:any>)))
(jazz:add-primitive-patterns 'scheme.language.runtime.kernel:length               '((##length <list:int>)     (##vector-length <vector:int>)          (##string-length <string:int>)))
(jazz:add-primitive-patterns 'scheme.language.runtime.kernel:null?                '((##null?  <any:bool>)))

(jazz:add-primitive-patterns 'scheme.language.runtime.kernel:string-length        '((##string-length <string:fx>)))
(jazz:add-primitive-patterns 'scheme.language.runtime.kernel:string-ref           '((##string-ref    <string^fx:char>)))

(jazz:add-primitive-patterns 'scheme.language.runtime.kernel:vector-length        '((##vector-length <vector:fx>)))
(jazz:add-primitive-patterns 'scheme.language.runtime.kernel:vector-ref           '((##vector-ref    <vector^fx:any>)))
(jazz:add-primitive-patterns 'scheme.language.runtime.kernel:vector-set!          '((##vector-set!   <vector^fx^any:void>)))

(jazz:add-primitive-patterns 'scheme.language.runtime.kernel:table-ref            '((##table-ref  <table^any:any>) (##table-ref    <table^any^any:any>)))
(jazz:add-primitive-patterns 'scheme.language.runtime.kernel:table-set!           '((##table-set! <table^any^any:void>)))

(jazz:add-primitive-patterns 'scheme.language.runtime.kernel:min                  '((##fixnum.min <fx^fx:fx>) (##flonum.min <fl^fl:fl>) (##flonum.min <fl^fl^fl:fl>)))
(jazz:add-primitive-patterns 'scheme.language.runtime.kernel:max                  '((##fixnum.max <fx^fx:fx>) (##flonum.max <fl^fl:fl>) (##flonum.max <fl^fl^fl:fl>)))
(jazz:add-primitive-patterns 'scheme.language.runtime.kernel:modulo               '((##fixnum.modulo <fx^fx:fx>)))
(jazz:add-primitive-patterns 'scheme.language.runtime.kernel:even?                '((##fixnum.even? <fx:bool>)))
(jazz:add-primitive-patterns 'scheme.language.runtime.kernel:odd?                 '((##fixnum.odd? <fx:bool>)))

(jazz:add-primitive-patterns 'jazz.language.runtime.kernel:fx+                    '((##fx+ <fx^fx:fx>)))
(jazz:add-primitive-patterns 'jazz.language.runtime.kernel:fx-                    '((##fx- <fx^fx:fx>)))
(jazz:add-primitive-patterns 'jazz.language.runtime.kernel:fx*                    '((##fx* <fx^fx:fx>)))

(jazz:add-primitive-patterns 'jazz.language.runtime.kernel:fl+                    '(                     (##fl+ <fl^fl:fl>)))
(jazz:add-primitive-patterns 'jazz.language.runtime.kernel:fl-                    '(                     (##fl- <fl^fl:fl>)))
(jazz:add-primitive-patterns 'jazz.language.runtime.kernel:fl*                    '(                     (##fl* <fl^fl:fl>)))
(jazz:add-primitive-patterns 'jazz.language.runtime.kernel:fl/                    '(                     (##fl/ <fl^fl:fl>)))

(jazz:add-primitive-patterns 'jazz.language.runtime.kernel:bitwise-not            '((##fixnum.bitwise-not <fx:fx>)))
(jazz:add-primitive-patterns 'jazz.language.runtime.kernel:bitwise-and            '((##fixnum.bitwise-and <fx^fx:fx>)))
(jazz:add-primitive-patterns 'jazz.language.runtime.kernel:bitwise-ior            '((##fixnum.bitwise-ior <fx^fx:fx>)))
(jazz:add-primitive-patterns 'jazz.language.runtime.kernel:bitwise-xor            '((##fixnum.bitwise-xor <fx^fx:fx>)))
(jazz:add-primitive-patterns 'jazz.language.runtime.kernel:arithmetic-shift       '((##fixnum.arithmetic-shift <fx^fx:fx>)))
(jazz:add-primitive-patterns 'jazz.language.runtime.kernel:arithmetic-shift-left  '((##fixnum.arithmetic-shift-left <fx^fx:fx>)))
(jazz:add-primitive-patterns 'jazz.language.runtime.kernel:arithmetic-shift-right '((##fixnum.arithmetic-shift-right <fx^fx:fx>)))

(jazz:add-primitive-patterns 'jazz.language.runtime.kernel:fixnum->flonum         '((##fixnum->flonum <fx:fl>)))
(jazz:add-primitive-patterns 'jazz.language.runtime.kernel:flonum->fixnum         '(                     (##flonum->fixnum <fl:fx>)))

(jazz:add-primitive-patterns 'jazz.language.runtime.kernel:s8vector               '((##s8vector         <fx*:s8vector>)))
(jazz:add-primitive-patterns 'jazz.language.runtime.kernel:s8vector-length        '((##s8vector-length  <s8vector:fx>)))
(jazz:add-primitive-patterns 'jazz.language.runtime.kernel:s8vector-ref           '((##s8vector-ref     <s8vector^fx:fx>)))
(jazz:add-primitive-patterns 'jazz.language.runtime.kernel:s8vector-set!          '((##s8vector-set!    <s8vector^fx^any:void>)))

(jazz:add-primitive-patterns 'jazz.language.runtime.kernel:u8vector               '((##u8vector         <fx*:u8vector>)))
(jazz:add-primitive-patterns 'jazz.language.runtime.kernel:u8vector-length        '((##u8vector-length  <u8vector:fx>)))
(jazz:add-primitive-patterns 'jazz.language.runtime.kernel:u8vector-ref           '((##u8vector-ref     <u8vector^fx:fx>)))
(jazz:add-primitive-patterns 'jazz.language.runtime.kernel:u8vector-set!          '((##u8vector-set!    <u8vector^fx^any:void>)))

(jazz:add-primitive-patterns 'jazz.language.runtime.kernel:s16vector              '((##s16vector        <fx*:s16vector>)))
(jazz:add-primitive-patterns 'jazz.language.runtime.kernel:s16vector-length       '((##s16vector-length <s16vector:fx>)))
(jazz:add-primitive-patterns 'jazz.language.runtime.kernel:s16vector-ref          '((##s16vector-ref    <s16vector^fx:fx>)))
(jazz:add-primitive-patterns 'jazz.language.runtime.kernel:s16vector-set!         '((##s16vector-set!   <s16vector^fx^any:void>)))

(jazz:add-primitive-patterns 'jazz.language.runtime.kernel:u16vector              '((##u16vector        <fx*:u16vector>)))
(jazz:add-primitive-patterns 'jazz.language.runtime.kernel:u16vector-length       '((##u16vector-length <u16vector:fx>)))
(jazz:add-primitive-patterns 'jazz.language.runtime.kernel:u16vector-ref          '((##u16vector-ref    <u16vector^fx:fx>)))
(jazz:add-primitive-patterns 'jazz.language.runtime.kernel:u16vector-set!         '((##u16vector-set!   <u16vector^fx^any:void>)))

(jazz:add-primitive-patterns 'jazz.language.runtime.kernel:s32vector              '((##s32vector        <fx*:s32vector>)))
(jazz:add-primitive-patterns 'jazz.language.runtime.kernel:s32vector-length       '((##s32vector-length <s32vector:fx>)))
(jazz:add-primitive-patterns 'jazz.language.runtime.kernel:s32vector-ref          '((##s32vector-ref    <s32vector^fx:fx>)))
(jazz:add-primitive-patterns 'jazz.language.runtime.kernel:s32vector-set!         '((##s32vector-set!   <s32vector^fx^any:void>)))

(jazz:add-primitive-patterns 'jazz.language.runtime.kernel:u32vector              '((##u32vector        <fx*:u32vector>)))
(jazz:add-primitive-patterns 'jazz.language.runtime.kernel:u32vector-length       '((##u32vector-length <u32vector:fx>)))
(jazz:add-primitive-patterns 'jazz.language.runtime.kernel:u32vector-ref          '((##u32vector-ref    <u32vector^fx:fx>)))
(jazz:add-primitive-patterns 'jazz.language.runtime.kernel:u32vector-set!         '((##u32vector-set!   <u32vector^fx^any:void>)))

(jazz:add-primitive-patterns 'jazz.language.runtime.kernel:f32vector              '((##f32vector        <fl*:f32vector>)))
(jazz:add-primitive-patterns 'jazz.language.runtime.kernel:f32vector-length       '((##f32vector-length <f32vector:fx>)))
(jazz:add-primitive-patterns 'jazz.language.runtime.kernel:f32vector-ref          '((##f32vector-ref    <f32vector^fx:fl>)))
(jazz:add-primitive-patterns 'jazz.language.runtime.kernel:f32vector-set!         '((##f32vector-set!   <f32vector^fx^any:void>)))

(jazz:add-primitive-patterns 'jazz.language.runtime.kernel:f64vector              '((##f64vector        <fl*:f64vector>)))
(jazz:add-primitive-patterns 'jazz.language.runtime.kernel:f64vector-length       '((##f64vector-length <f64vector:fx>)))
(jazz:add-primitive-patterns 'jazz.language.runtime.kernel:f64vector-ref          '((##f64vector-ref    <f64vector^fx:fl>)))
(jazz:add-primitive-patterns 'jazz.language.runtime.kernel:f64vector-set!         '((##f64vector-set!   <f64vector^fx^any:void>)))

(jazz:add-primitive-patterns 'jazz.language.runtime.kernel:flset!                 '((##f64vector-set!   <fl^fx^fl:void>)))

(jazz:add-primitive-patterns 'jazz.language.runtime.functional:element            '((list-ref <list^int:any>) (##vector-ref    <vector^int:any>)      (##string-ref    <string^int:char>)))
(jazz:add-primitive-patterns 'jazz.language.runtime.functional:set-element!       '(                          (##vector-set!   <vector^int^any:void>) (##string-set!   <string^int^char:void>)))


(jazz:initialize-primitive-patterns)


(jazz:define-emit (primitive-call (scheme backend) operator locator arguments arguments-codes declaration environment)
  (if (%%not locator)
      #f
    (let ((patterns (jazz:get-primitive-patterns locator)))
      (let ((types (jazz:codes-types arguments-codes)))
        (let iter ((scan patterns))
             (if (%%null? scan)
                 (begin
                   (%%when (and (jazz:warnings?) (%%not (%%null? patterns)) (jazz:get-module-warn? (jazz:get-declaration-toplevel declaration) 'optimizations)
                             ;; a bit extreme for now
                             (%%not (%%memq locator '(scheme.language.runtime.kernel:car
                                                      scheme.language.runtime.kernel:cdr))))
                     (jazz:feedback "Warning: In {a}{a}: Unable to match call to primitive {a}"
                                    (jazz:get-declaration-locator declaration)
                                    (jazz:present-expression-location operator)
                                    (jazz:reference-name locator)))
                   #f)
               (jazz:bind (name function-type) (%%car scan)
                 (if (jazz:match-signature? arguments types function-type)
                     (jazz:new-code
                       `(,name ,@(jazz:codes-forms arguments-codes))
                       (jazz:get-function-type-result function-type)
                       #f)
                   (iter (%%cdr scan))))))))))


;;;
;;;; Inlined Call
;;;


(jazz:define-emit (inlined-call (scheme backend) expression declaration operator arguments)
  #f)


;;;
;;;; Assignment
;;;


(jazz:define-emit (definition-assignment (scheme backend) declaration source-declaration environment value-code)
  (let ((locator (jazz:get-declaration-locator declaration)))
    `(set! ,locator ,(jazz:sourcified-form value-code))))


(jazz:define-emit (slot-assignment (scheme backend) declaration source-declaration environment self value-code)
  (let ((offset-locator (%%compose-helper (jazz:get-declaration-locator declaration) 'offset)))
    `(%%object-set! ,(jazz:sourcified-form self) ,offset-locator ,(jazz:sourcified-form value-code)))))
