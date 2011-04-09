;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Expressions
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


(unit protected dialect.syntax.walker.expression


;;;
;;;; Proclaim
;;;


(jazz:define-class-runtime jazz:Proclaim)


(define (jazz:new-proclaim clauses)
  (jazz:allocate-proclaim jazz:Proclaim #f #f clauses))


(jazz:define-method (jazz:emit-expression (jazz:Proclaim expression) declaration environment backend)
  (let ((clauses (%%get-proclaim-clauses expression))
        (module-declaration (%%get-declaration-toplevel declaration)))
    (for-each (lambda (clause)
                (jazz:proclaim module-declaration clause))
              clauses))
  #f)


(jazz:encapsulate-class jazz:Proclaim)


(jazz:define-virtual-runtime (jazz:validate-proclaim (jazz:Walker walker) resume declaration environment form-src))


(jazz:define-method (jazz:validate-proclaim (jazz:Walker walker) resume declaration environment form-src)
  (if (%%not (%%class-is? declaration jazz:Module-Declaration))
      (jazz:walk-error walker resume declaration form-src "For now, proclaim can only be used at the module level")))


(define (jazz:walk-proclaim walker resume declaration environment form-src)
  (jazz:validate-proclaim walker resume declaration environment form-src)
  (let ((form (%%desourcify form-src)))
    (let ((clauses (%%cdr form)))
      (jazz:new-proclaim clauses))))


;;;
;;;; Delay
;;;


(jazz:define-class-runtime jazz:Delay)


(define (jazz:new-delay expression)
  (jazz:allocate-delay jazz:Delay #f #f expression))


(jazz:define-method (jazz:emit-expression (jazz:Delay expression) declaration environment backend)
  (let ((expr (jazz:emit-expression (%%get-delay-expression expression) declaration environment backend)))
    (jazz:new-code
      (jazz:emit 'delay backend expression declaration environment expr)
      jazz:Any
      #f)))


(jazz:encapsulate-class jazz:Delay)


;;;
;;;; Quasiquote
;;;


(jazz:define-class-runtime jazz:Quasiquote)


(define (jazz:new-quasiquote form)
  (jazz:allocate-quasiquote jazz:Quasiquote #f #f form))


(jazz:define-method (jazz:emit-expression (jazz:Quasiquote expression) declaration environment backend)
  (jazz:new-code
    (jazz:emit 'quasiquote backend expression declaration environment)
    jazz:List
    #f))


(jazz:encapsulate-class jazz:Quasiquote)


;;;
;;;; Method Reference
;;;


(jazz:define-class-runtime jazz:Method-Reference)


(define (jazz:new-method-reference binding)
  (jazz:allocate-method-reference jazz:Method-Reference #f #f binding))


(jazz:define-method (jazz:emit-expression (jazz:Method-Reference expression) declaration environment backend)
  (let ((method-declaration (%%get-reference-binding expression)))
    (jazz:new-code
      (jazz:emit 'method-reference backend expression declaration environment)
      (or (%%get-lexical-binding-type method-declaration)
          jazz:Any)
      #f)))


(jazz:define-method (jazz:emit-call (jazz:Method-Reference expression) arguments declaration environment backend)
  (let ((operator (jazz:emit-expression expression declaration environment backend)))
    (jazz:new-code
      (jazz:emit 'method-reference-call backend expression declaration operator arguments)
      jazz:Any
      #f)))


(jazz:encapsulate-class jazz:Method-Reference)


;;;
;;;; Lambda
;;;


(jazz:define-class-runtime jazz:Lambda)


(define (jazz:new-lambda type source signature body)
  (jazz:allocate-lambda jazz:Lambda type source signature body))


(jazz:define-method (jazz:emit-expression (jazz:Lambda expression) declaration environment backend)
  (let ((type (%%get-expression-type expression))
        (signature (%%get-lambda-signature expression))
        (body (%%get-lambda-body expression)))
    (jazz:with-annotated-frame (jazz:annotate-signature signature)
      (lambda (frame)
        (let ((augmented-environment (%%cons frame environment)))
          (let ((signature-emit (jazz:emit-signature signature declaration augmented-environment backend)))
            (let ((body-code (jazz:emit-expression body declaration augmented-environment backend)))
              (let ((signature-casts (jazz:emit-signature-casts signature declaration augmented-environment backend))
                    (cast-body (jazz:simplify-begin (jazz:emit-type-cast (jazz:new-code `(begin ,@(jazz:sourcified-form body-code)) (%%get-code-type body-code) #f) type declaration environment backend))))
                (jazz:new-code
                  (jazz:emit 'lambda backend expression declaration environment signature-emit signature-casts cast-body)
                  (jazz:new-function-type '() '() '() #f (%%get-code-type body-code))
                  (%%get-expression-source expression))))))))))


(jazz:define-method (jazz:tree-fold (jazz:Lambda expression) down up here seed environment)
  (jazz:with-annotated-frame (jazz:annotate-signature (%%get-lambda-signature expression))
    (lambda (frame)
      (let ((aug-env (cons frame environment)))
        (up expression
            seed
            (jazz:tree-fold (%%get-lambda-body expression) down up here (down expression seed environment) aug-env)
            environment)))))


(jazz:encapsulate-class jazz:Lambda)


;;;
;;;; Let
;;;


(jazz:define-class-runtime jazz:Let)


(define (jazz:new-let source bindings body)
  (jazz:allocate-let jazz:Let #f source bindings body))


(jazz:define-method (jazz:emit-expression (jazz:Let expression) declaration environment backend)
  (let ((bindings (%%get-let-bindings expression))
        (body (%%get-let-body expression)))
    (jazz:with-annotated-frame (jazz:annotate-bindings bindings)
      (lambda (frame)
        (let ((variables (%%get-annotated-frame-variables frame))
              (augmented-environment (%%cons frame environment)))
          (let ((bindings-output
                  (map (lambda (binding annotated-variable)
                         (let ((variable (%%car binding))
                               (value (%%cdr binding)))
                           (let ((value-code (jazz:emit-expression value declaration augmented-environment backend)))
                             (jazz:extend-annotated-type frame annotated-variable (%%get-code-type value-code))
                             `(,(jazz:emit-binding-symbol variable declaration environment backend) ,(jazz:emit-type-cast value-code (%%get-lexical-binding-type variable) declaration environment backend)))))
                       bindings
                       variables))
                (body-code (jazz:emit-expression body declaration augmented-environment backend)))
            (jazz:new-code
              (jazz:emit 'let backend expression declaration environment bindings-output body-code)
              (%%get-code-type body-code)
              (%%get-expression-source expression))))))))


(jazz:define-method (jazz:tree-fold (jazz:Let expression) down up here seed environment)
  (let* ((bindings (%%get-let-bindings expression))
         (aug-env (cons (map car bindings) environment))
         (seed1 (down expression seed environment))
         (seed2 (jazz:tree-fold-list (map cdr bindings) down up here seed1 environment)))
    (up expression seed (jazz:tree-fold (%%get-let-body expression) down up here seed2 aug-env) environment)))


(jazz:encapsulate-class jazz:Let)


;;;
;;;; Named Let
;;;


(jazz:define-class-runtime jazz:Named-Let)


(define (jazz:new-named-let source variable bindings body)
  (jazz:allocate-named-let jazz:Named-Let #f source bindings body variable))


(jazz:define-method (jazz:emit-expression (jazz:Named-Let expression) declaration environment backend)
  (let ((variable (%%get-named-let-variable expression))
        (bindings (%%get-let-bindings expression))
        (body (%%get-let-body expression)))
    (jazz:with-annotated-frame (%%cons (jazz:new-annotated-variable variable jazz:Any jazz:Any) (jazz:annotate-bindings bindings))
      (lambda (frame)
        (let ((variables (%%get-annotated-frame-variables frame))
              (augmented-environment (%%cons frame environment)))
          (let ((variable-emit (jazz:emit-binding-symbol variable declaration environment backend))
                (bindings-output
                  (map (lambda (binding annotated-variable)
                         (let ((variable (%%car binding))
                               (value (%%cdr binding)))
                           (let ((value-code (jazz:emit-expression value declaration augmented-environment backend)))
                             (jazz:extend-annotated-type frame annotated-variable (%%get-code-type value-code))
                             `(,(jazz:emit-binding-symbol variable declaration environment backend) ,(jazz:emit-type-cast value-code (%%get-lexical-binding-type variable) declaration environment backend)))))
                       bindings
                       (%%cdr variables)))
                (body-code (jazz:emit-expression body declaration augmented-environment backend)))
            (jazz:new-code
              (jazz:emit 'named-let backend expression declaration environment variable-emit bindings-output body-code)
              (%%get-code-type body-code)
              (%%get-expression-source expression))))))))


(jazz:define-method (jazz:tree-fold (jazz:Named-Let expression) down up here seed environment)
  (let* ((bindings (%%get-let-bindings expression))
         (aug-env (cons (cons (%%get-named-let-variable expression) (map car bindings)) environment))
         (seed1 (down expression seed environment))
         (seed2 (jazz:tree-fold-list (map cdr bindings) down up here seed1 environment)))
    (up expression seed (jazz:tree-fold (%%get-let-body expression) down up here seed2 aug-env) environment)))


(jazz:encapsulate-class jazz:Named-Let)


;;;
;;;; Letstar
;;;


(jazz:define-class-runtime jazz:Letstar)


(define (jazz:new-letstar source bindings body)
  (jazz:allocate-letstar jazz:Letstar #f source bindings body))


(jazz:define-method (jazz:emit-expression (jazz:Letstar expression) declaration environment backend)
  (let ((bindings (%%get-letstar-bindings expression))
        (body (%%get-letstar-body expression)))
    (jazz:with-annotated-frame (jazz:annotate-bindings bindings)
      (lambda (frame)
        (let ((variables (%%get-annotated-frame-variables frame))
              (augmented-environment (%%cons frame environment)))
          (let ((bindings-output
                  (map (lambda (binding annotated-variable)
                         (let ((variable (%%car binding))
                               (value (%%cdr binding)))
                           (let ((value-code (jazz:emit-expression value declaration augmented-environment backend)))
                             (jazz:extend-annotated-type frame annotated-variable (%%get-code-type value-code))
                             `(,(jazz:emit-binding-symbol variable declaration environment backend) ,(jazz:emit-type-cast value-code (%%get-lexical-binding-type variable) declaration environment backend)))))
                       bindings
                       variables))
                (body-code (jazz:emit-expression body declaration augmented-environment backend)))
            (jazz:new-code
              (jazz:emit 'letstar backend expression declaration environment bindings-output body-code)
              (%%get-code-type body-code)
              (%%get-expression-source expression))))))))


(jazz:define-method (jazz:tree-fold (jazz:Letstar expression) down up here seed environment)
  (let lp ((ls (%%get-letstar-bindings expression))
           (seed2 (down expression seed environment))
           (aug-env environment))
       (if (pair? ls)
           (lp (cdr ls)
               (jazz:tree-fold (cdar ls) down up here seed2 aug-env)
               (cons (list (caar ls)) aug-env))
         (up expression seed (jazz:tree-fold (%%get-letstar-body expression) down up here seed2 aug-env) environment))))


(jazz:encapsulate-class jazz:Letstar)


;;;
;;;; Letrec
;;;


(jazz:define-class-runtime jazz:Letrec)


(define (jazz:new-letrec source bindings body)
  (jazz:allocate-letrec jazz:Letrec #f source bindings body))


(jazz:define-method (jazz:emit-expression (jazz:Letrec expression) declaration environment backend)
  (let ((bindings (%%get-letrec-bindings expression))
        (body (%%get-letrec-body expression)))
    (jazz:with-annotated-frame (jazz:annotate-bindings bindings)
      (lambda (frame)
        (let ((variables (%%get-annotated-frame-variables frame))
              (augmented-environment (%%cons frame environment)))
          (let ((bindings-output
                  (map (lambda (binding annotated-variable)
                         (let ((variable (%%car binding))
                               (value (%%cdr binding)))
                           (let ((value-code (jazz:emit-expression value declaration augmented-environment backend)))
                             (jazz:extend-annotated-type frame annotated-variable (%%get-code-type value-code))
                             `(,(jazz:emit-binding-symbol variable declaration environment backend) ,(jazz:emit-type-cast value-code (%%get-lexical-binding-type variable) declaration environment backend)))))
                       bindings
                       variables))
                (body-code (jazz:emit-expression body declaration augmented-environment backend)))
            (jazz:new-code
              (jazz:emit 'letrec backend expression declaration environment bindings-output body-code)
              (%%get-code-type body-code)
              (%%get-expression-source expression))))))))


(jazz:define-method (jazz:tree-fold (jazz:Letrec expression) down up here seed environment)
  (let* ((bindings (%%get-letrec-bindings expression))
         (aug-env (cons (map car bindings) environment))
         (seed1 (down expression seed environment))
         (seed2 (jazz:tree-fold-list (map cdr bindings) down up here seed1 aug-env)))
    (up expression seed (jazz:tree-fold (%%get-letrec-body expression) down up here seed2 aug-env) environment)))


(jazz:encapsulate-class jazz:Letrec)


;;;
;;;; Receive
;;;


(jazz:define-class-runtime jazz:Receive)


(define (jazz:new-receive source variables expression body)
  (jazz:allocate-receive jazz:Receive #f source variables expression body))


(jazz:define-method (jazz:emit-expression (jazz:Receive expression) declaration environment backend)
  (let ((variables (%%get-receive-variables expression))
        (expr (%%get-receive-expression expression))
        (body (%%get-receive-body expression)))
    (jazz:with-annotated-frame (jazz:annotate-receive variables)
      (lambda (frame)
        (let ((augmented-environment (%%cons frame environment)))
          (let ((expression-output (jazz:sourcified-form (jazz:emit-expression expr declaration environment backend))))
            (let ((bindings-output (map (lambda (variable)
                                          (jazz:emit-binding-symbol variable declaration environment backend))
                                        variables))
                  (body-code (jazz:emit-expression body declaration augmented-environment backend)))
              (jazz:new-code
                (jazz:emit 'receive backend expression declaration environment bindings-output expression-output body-code)
                (%%get-code-type body-code)
                (%%get-expression-source expression)))))))))


(jazz:define-method (jazz:tree-fold (jazz:Receive expression) down up here seed environment)
  (let* ((aug-env (cons (%%get-receive-variables expression) environment))
         (seed1 (down expression seed environment))
         (seed2 (jazz:tree-fold (%%get-receive-expression expression) down up here seed1 environment)))
    (up expression seed (jazz:tree-fold (%%get-receive-body expression) down up here seed2 aug-env) environment)))


(jazz:encapsulate-class jazz:Receive)


;;;
;;;; Do
;;;


(jazz:define-class-runtime jazz:Do)


(define (jazz:new-do bindings test result body)
  (jazz:allocate-do jazz:Do #f #f bindings test result body))


(jazz:define-method (jazz:emit-expression (jazz:Do expression) declaration environment backend)
  (let ((bindings (%%get-do-bindings expression))
        (test (%%get-do-test expression))
        (result (%%get-do-result expression))
        (body (%%get-do-body expression)))
    (jazz:with-annotated-frame (jazz:annotate-bindings bindings)
      (lambda (frame)
        (let ((variables (%%get-annotated-frame-variables frame))
              (augmented-environment (%%cons frame environment)))
          (let ((bindings-output
                  (map (lambda (binding annotated-variable)
                         (let ((variable (%%car binding))
                               (init (%%cadr binding))
                               (step (%%cddr binding)))
                           (let ((init-code (jazz:sourcified-form (jazz:emit-expression init declaration augmented-environment backend)))
                                 (step-code-list (if step (%%list (jazz:sourcified-form (jazz:emit-expression step declaration augmented-environment backend))) '())))
                             `(,(jazz:emit-binding-symbol variable declaration environment backend)
                               ,init-code
                               ,@step-code-list))))
                       bindings
                       variables))
                (test-code (jazz:emit-expression test declaration augmented-environment backend))
                (result-code (jazz:emit-expression result declaration augmented-environment backend))
                (body-code (jazz:emit-expression body declaration augmented-environment backend)))
            (jazz:new-code
              (jazz:emit 'do backend expression declaration environment bindings-output test-code result-code body-code)
              (%%get-code-type result-code)
              #f)))))))


(jazz:define-method (jazz:tree-fold (jazz:Do expression) down up here seed environment)
  (let* ((aug-env (cons (map car (%%get-do-bindings expression)) environment))
         (seed1 (jazz:tree-fold-list (map cadr (%%get-do-bindings expression)) down up here (down expression seed environment) environment))
         (seed2 (jazz:tree-fold-list (map (lambda (x) (or (cddr x) (cadr x))) (%%get-do-bindings expression)) down up here seed1 aug-env)))
    (up expression
        seed
        (jazz:tree-fold
          (%%get-do-result expression) down up here
          (jazz:tree-fold
            (%%get-do-body expression) down up here
            (jazz:tree-fold (%%get-do-test expression) down up here seed2 aug-env)
            aug-env)
          aug-env)
        environment)))


(jazz:encapsulate-class jazz:Do)


;;;
;;;; Specialized Call
;;;


(define jazz:specializers
  (%%make-table test: eq?))


(define (jazz:add-specializer specialized-declaration specializer)
  (%%table-set! jazz:specializers specialized-declaration
    (%%append (%%table-ref jazz:specializers specialized-declaration '())
              (%%list specializer))))


(define (jazz:get-specializers binding)
  (%%table-ref jazz:specializers binding '()))


(jazz:define-variable-override jazz:emit-specialized-call
  (lambda (operator locator arguments arguments-codes call declaration environment backend)
    (if (%%not locator)
        #f
      (or (jazz:emit-specialized-locator locator arguments-codes environment backend)
          (if (%%class-is? operator jazz:Binding-Reference)
              (let ((binding (%%get-reference-binding operator)))
                (let ((specializers (jazz:get-specializers binding)))
                  (let ((types (jazz:codes-types arguments-codes)))
                    (let iter ((scan specializers))
                         (if (%%null? scan)
                             (begin
                               (%%when (and (jazz:warnings?) (%%not (%%null? specializers)) (jazz:get-module-warn? (%%get-declaration-toplevel declaration) 'optimizations)
                                         ;; quicky to suppress duplicate warnings as for the moment those are both primitive and specialize
                                         (%%not (%%memq locator '(scheme.dialect.runtime.kernel:=
                                                                   scheme.dialect.runtime.kernel:<
                                                                   scheme.dialect.runtime.kernel:<=
                                                                   scheme.dialect.runtime.kernel:>
                                                                   scheme.dialect.runtime.kernel:>=
                                                                   scheme.dialect.runtime.kernel:+
                                                                   scheme.dialect.runtime.kernel:-
                                                                   scheme.dialect.runtime.kernel:*
                                                                   scheme.dialect.runtime.kernel:/))))
                                 (jazz:debug 'Warning: 'In (%%get-declaration-locator declaration) 'unable 'to 'match 'call 'to 'specialized (%%get-lexical-binding-name binding)))
                               ;; for debugging
                               (%%when (%%memq (%%get-lexical-binding-name binding) (jazz:debug-specializers))
                                 (jazz:debug 'Warning: 'In (%%get-declaration-locator declaration) 'unable 'to 'match 'call 'to 'specialized (%%get-lexical-binding-name binding) 'on types))
                               #f)
                           (let ((specializer (%%car scan)))
                             (let ((function-type (%%get-lexical-binding-type specializer)))
                               (if (jazz:match-signature? arguments types function-type)
                                   (or (jazz:emit-inlined-binding-call specializer arguments-codes call declaration environment backend)
                                       (begin
                                         (jazz:add-to-module-references (%%get-declaration-toplevel declaration) specializer)
                                         (jazz:new-code
                                           (let ((locator (%%get-declaration-locator specializer)))
                                             `(,locator ,@(jazz:codes-forms arguments-codes)))
                                           (%%get-function-type-result function-type)
                                           #f)))
                                 (iter (%%cdr scan))))))))))
            #f)))))


;; quicky because classes are not yet defined at this point
(jazz:define-variable jazz:emit-specialized-locator)

(jazz:define-variable-override jazz:emit-specialized-locator
  (lambda (locator arguments-codes environment backend)
    #f))


;;;
;;;; New Call
;;;


(jazz:define-variable-override jazz:emit-new-call
  (lambda (operator locator arguments arguments-codes declaration environment backend)
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


(define jazz:primitive-patterns
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
              jazz:primitive-patterns)
    (set! jazz:primitive-patterns table)))


(define (jazz:add-primitive-patterns operator patterns)
  (set! jazz:primitive-patterns (%%cons (%%cons operator patterns) jazz:primitive-patterns)))


(define (jazz:get-primitive-patterns locator)
  (%%table-ref jazz:primitive-patterns locator '()))


(jazz:add-primitive-patterns 'scheme.dialect.runtime.kernel:=                       '((##fx=  <fx*:bool>)  (##fl=  <fl*:bool>)  (##= <number^number:bool>)))
(jazz:add-primitive-patterns 'scheme.dialect.runtime.kernel:<                       '((##fx<  <fx*:bool>)  (##fl<  <fl*:bool>)))
(jazz:add-primitive-patterns 'scheme.dialect.runtime.kernel:<=                      '((##fx<= <fx*:bool>)  (##fl<= <fl*:bool>)))
(jazz:add-primitive-patterns 'scheme.dialect.runtime.kernel:>                       '((##fx>  <fx*:bool>)  (##fl>  <fl*:bool>)))
(jazz:add-primitive-patterns 'scheme.dialect.runtime.kernel:>=                      '((##fx>= <fx*:bool>)  (##fl>= <fl*:bool>)))

(jazz:add-primitive-patterns 'scheme.dialect.runtime.kernel:+                       '((##fx+  <fx*:fx>)    (##fl+  <fl*:fl>)    (##+ <int^int:int>) (##+ <number^number:number>)))
(jazz:add-primitive-patterns 'scheme.dialect.runtime.kernel:-                       '((##fx-  <fx^fx*:fx>) (##fl-  <fl^fl*:fl>) (##- <int^int:int>) (##- <number^number:number>)))
(jazz:add-primitive-patterns 'scheme.dialect.runtime.kernel:*                       '((##fx*  <fx*:fx>)    (##fl*  <fl*:fl>)    (##* <int^int:int>) (##* <number^number:number>)))

;; only done in release as these can crash on a division by zero
(cond-expand
  (release
    (jazz:add-primitive-patterns 'scheme.dialect.runtime.kernel:/                   '(                     (##fl/  <fl^fl*:fl>)                     (##/ <number^number:number>)))
    (jazz:add-primitive-patterns 'scheme.dialect.runtime.kernel:quotient            '((##fxquotient <fx^fx:fx>))))
  (else))

(jazz:add-primitive-patterns 'scheme.dialect.runtime.kernel:floor                   '(                     (##flfloor    <fl:fl>)))
(jazz:add-primitive-patterns 'scheme.dialect.runtime.kernel:ceiling                 '(                     (##flceiling  <fl:fl>)))
(jazz:add-primitive-patterns 'scheme.dialect.runtime.kernel:truncate                '(                     (##fltruncate <fl:fl>)))
(jazz:add-primitive-patterns 'scheme.dialect.runtime.kernel:round                   '(                     (##flround    <fl:fl>)))

(jazz:add-primitive-patterns 'jazz.dialect.runtime.kernel:fx+                       '((##fx+ <fx^fx:fx>)))
(jazz:add-primitive-patterns 'jazz.dialect.runtime.kernel:fx-                       '((##fx- <fx^fx:fx>)))
(jazz:add-primitive-patterns 'jazz.dialect.runtime.kernel:fx*                       '((##fx* <fx^fx:fx>)))

(jazz:add-primitive-patterns 'jazz.dialect.runtime.kernel:fl+                       '(                     (##fl+ <fl^fl:fl>)))
(jazz:add-primitive-patterns 'jazz.dialect.runtime.kernel:fl-                       '(                     (##fl- <fl^fl:fl>)))
(jazz:add-primitive-patterns 'jazz.dialect.runtime.kernel:fl*                       '(                     (##fl* <fl^fl:fl>)))
(jazz:add-primitive-patterns 'jazz.dialect.runtime.kernel:fl/                       '(                     (##fl/ <fl^fl:fl>)))

(jazz:add-primitive-patterns 'jazz.dialect.runtime.kernel:fixnum->flonum            '((##fixnum->flonum <fx:fl>)))
(jazz:add-primitive-patterns 'jazz.dialect.runtime.kernel:flonum->fixnum            '(                     (##flonum->fixnum <fl:fx>)))

(jazz:add-primitive-patterns 'scheme.dialect.runtime.kernel:not                     '((##not  <any:bool>)))
(jazz:add-primitive-patterns 'scheme.dialect.runtime.kernel:eq?                     '((##eq?  <any^any:bool>)))
(jazz:add-primitive-patterns 'scheme.dialect.runtime.kernel:eqv?                    '((##eqv? <any^any:bool>)))

(jazz:add-primitive-patterns 'scheme.dialect.runtime.kernel:car                     '((##car    <pair:any>)))
(jazz:add-primitive-patterns 'scheme.dialect.runtime.kernel:cdr                     '((##cdr    <pair:any>)))
(jazz:add-primitive-patterns 'scheme.dialect.runtime.kernel:length                  '((##length <list:int>)     (##vector-length <vector:int>)          (##string-length <string:int>)))
(jazz:add-primitive-patterns 'jazz.dialect.runtime.language.functional:element      '((list-ref <list^int:any>) (##vector-ref    <vector^int:any>)      (##string-ref    <string^int:char>)))
(jazz:add-primitive-patterns 'jazz.dialect.runtime.language.functional:set-element! '(                          (##vector-set!   <vector^int^any:void>) (##string-set!   <string^int^char:void>)))


(jazz:define-variable-override jazz:emit-primitive-call
  (lambda (operator locator arguments arguments-codes declaration environment backend)
    (if (%%not locator)
        #f
      (let ((patterns (jazz:get-primitive-patterns locator)))
        (let ((types (jazz:codes-types arguments-codes)))
          (let iter ((scan patterns))
               (if (%%null? scan)
                   (begin
                     (%%when (and (jazz:warnings?) (%%not (%%null? patterns)) (jazz:get-module-warn? (%%get-declaration-toplevel declaration) 'optimizations)
                               ;; a bit extreme for now
                               (%%not (%%memq locator '(scheme.dialect.runtime.kernel:car
                                                         scheme.dialect.runtime.kernel:cdr))))
                       (jazz:debug 'Warning: 'In (%%get-declaration-locator declaration) 'unable 'to 'match 'call 'to 'primitive (jazz:reference-name locator)))
                     #f)
                 (iter (%%cdr scan)))))))))


;;;
;;;; Inlined Call
;;;


(jazz:define-variable-override jazz:emit-inlined-call
  (lambda (operator arguments call declaration environment backend)
    (if (%%class-is? operator jazz:Binding-Reference)
        (let ((binding (%%get-reference-binding operator)))
          (jazz:emit-inlined-binding-call binding arguments call declaration environment backend))
      #f)))


;;;
;;;; Signature
;;;


(define (jazz:match-signature? arguments argument-types function-type)
  (let ((argcount (%%length argument-types))
        (mandatory (%%get-function-type-mandatory function-type))
        (positional (%%get-function-type-positional function-type))
        (optional (%%get-function-type-optional function-type))
        (named (%%get-function-type-named function-type))
        (rest (%%get-function-type-rest function-type)))
    (define (match? arg type expect)
      (if (%%class-is? expect jazz:Category-Type)
          (or (and (%%class-is? arg jazz:Binding-Reference)
                   (%%eq? (%%get-reference-binding arg) (%%get-category-type-declaration expect)))
              (and (%%class-is? type jazz:Category-Type)
                   (%%eq? (%%get-category-type-declaration type) (%%get-category-type-declaration expect))))
        (%%subtype? (or type jazz:Any) expect)))
    
    (define (match-positional?)
      (and (%%fx>= argcount mandatory)
           ;; this crude test for optional and named needs to be refined
           (or (%%fx<= argcount mandatory) (%%not (%%null? optional)) (%%not (%%null? named)) rest)
           (let iter ((args arguments)
                      (types argument-types)
                      (expected positional))
                (cond ((%%null? expected)
                       #t)
                      ((match? (%%car args) (%%car types) (%%car expected))
                       (iter (%%cdr args) (%%cdr types) (%%cdr expected)))
                      (else
                       #f)))))
    
    (define (match-rest?)
      (or (%%not rest)
          (let ((expect (%%get-rest-type-type rest)))
            (jazz:every? (lambda (type)
                           ;; should validate that type is not a category-type
                           (match? #f type expect))
                         (list-tail argument-types mandatory)))))
    
    (and (match-positional?)
         ;; testing the presence of optional named this way is a quicky that needs to be refined
         (or (%%not (%%null? optional))
             (%%not (%%null? named))
             (match-rest?)))))


;;;
;;;; If
;;;


(jazz:define-class-runtime jazz:If)


(define (jazz:new-if source test yes no)
  (jazz:allocate-if jazz:If #f source test yes no))


(define jazz:type-tests
  (%%make-table test: eq?))


(%%table-set! jazz:type-tests 'scheme.dialect.runtime.kernel:number?       jazz:Number)
(%%table-set! jazz:type-tests 'scheme.dialect.runtime.kernel:complex?      jazz:Complex)
(%%table-set! jazz:type-tests 'scheme.dialect.runtime.kernel:real?         jazz:Real)
(%%table-set! jazz:type-tests 'scheme.dialect.runtime.kernel:rational?     jazz:Rational)
(%%table-set! jazz:type-tests 'scheme.dialect.runtime.kernel:integer?      jazz:Integer)
(%%table-set! jazz:type-tests 'scheme.dialect.runtime.kernel:number?       jazz:Number)
(%%table-set! jazz:type-tests 'scheme.dialect.runtime.kernel:boolean?      jazz:Boolean)
;; not 100% correct because of Scheme's semantic for list?
(%%table-set! jazz:type-tests 'scheme.dialect.runtime.kernel:list?         jazz:List)
(%%table-set! jazz:type-tests 'scheme.dialect.runtime.kernel:null?         jazz:Null)
(%%table-set! jazz:type-tests 'scheme.dialect.runtime.kernel:pair?         jazz:Pair)
(%%table-set! jazz:type-tests 'scheme.dialect.runtime.kernel:symbol?       jazz:Symbol)
(%%table-set! jazz:type-tests 'scheme.dialect.runtime.kernel:char?         jazz:Char)
(%%table-set! jazz:type-tests 'scheme.dialect.runtime.kernel:string?       jazz:String)
(%%table-set! jazz:type-tests 'scheme.dialect.runtime.kernel:vector?       jazz:Vector)
(%%table-set! jazz:type-tests 'jazz.dialect.runtime.kernel:s8vector?       jazz:S8Vector)
(%%table-set! jazz:type-tests 'jazz.dialect.runtime.kernel:u8vector?       jazz:U8Vector)
(%%table-set! jazz:type-tests 'jazz.dialect.runtime.kernel:s16vector?      jazz:S16Vector)
(%%table-set! jazz:type-tests 'jazz.dialect.runtime.kernel:u16vector?      jazz:U16Vector)
(%%table-set! jazz:type-tests 'jazz.dialect.runtime.kernel:s32vector?      jazz:S32Vector)
(%%table-set! jazz:type-tests 'jazz.dialect.runtime.kernel:u32vector?      jazz:U32Vector)
(%%table-set! jazz:type-tests 'jazz.dialect.runtime.kernel:s64vector?      jazz:S64Vector)
(%%table-set! jazz:type-tests 'jazz.dialect.runtime.kernel:u64vector?      jazz:U64Vector)
(%%table-set! jazz:type-tests 'jazz.dialect.runtime.kernel:f32vector?      jazz:F32Vector)
(%%table-set! jazz:type-tests 'jazz.dialect.runtime.kernel:f64vector?      jazz:F64Vector)
(%%table-set! jazz:type-tests 'jazz.dialect.runtime.kernel:continuation?   jazz:Continuation)
(%%table-set! jazz:type-tests 'scheme.dialect.runtime.kernel:procedure?    jazz:Procedure)
(%%table-set! jazz:type-tests 'scheme.dialect.runtime.kernel:input-port?   jazz:Port)
(%%table-set! jazz:type-tests 'scheme.dialect.runtime.kernel:output-port?  jazz:Port)
(%%table-set! jazz:type-tests 'scheme.dialect.runtime.kernel:eof-object?   jazz:EOF)
(%%table-set! jazz:type-tests 'jazz.dialect.runtime.kernel:fixnum?         jazz:Fixnum)
(%%table-set! jazz:type-tests 'jazz.dialect.runtime.kernel:flonum?         jazz:Flonum)
(%%table-set! jazz:type-tests 'jazz.dialect.runtime.kernel:keyword?        jazz:Keyword)
(%%table-set! jazz:type-tests 'jazz.dialect.runtime.kernel:object?         jazz:Object)
(%%table-set! jazz:type-tests 'jazz.dialect.runtime.kernel:category?       jazz:Category)
(%%table-set! jazz:type-tests 'jazz.dialect.runtime.kernel:class?          jazz:Class)
(%%table-set! jazz:type-tests 'jazz.dialect.runtime.kernel:interface?      jazz:Interface)
(%%table-set! jazz:type-tests 'jazz.dialect.runtime.kernel:field?          jazz:Field)
(%%table-set! jazz:type-tests 'jazz.dialect.runtime.kernel:slot?           jazz:Slot)
(%%table-set! jazz:type-tests 'jazz.dialect.runtime.kernel:method?         jazz:Method)
(%%table-set! jazz:type-tests 'jazz.dialect.runtime.kernel:table?          jazz:Table)
(%%table-set! jazz:type-tests 'jazz.dialect.runtime.kernel:thread?         jazz:Thread)
(%%table-set! jazz:type-tests 'jazz.dialect.runtime.kernel:foreign?        jazz:Foreign)
(%%table-set! jazz:type-tests 'jazz.dialect.runtime.kernel:values?         jazz:Values)
(%%table-set! jazz:type-tests 'jazz.dialect.runtime.kernel:unspecified?    jazz:Unspecified)


(define jazz:not-type-tests
  (%%make-table test: eq?))


(%%table-set! jazz:not-type-tests 'jazz.dialect.runtime.kernel:not-null? jazz:Null)


(define (jazz:restrict-type base type)
  (jazz:new-restriction-type base type))


(define (jazz:restriction-of? type class)
  (and (%%class-is? type jazz:Restriction-Type)
       (%%class-is? (%%get-restriction-type-type type) class)))


(define (jazz:complement-type base type)
  (if (and (jazz:restriction-of? type jazz:Complement-Type)
           (%%eq? (%%get-restriction-type-base type) base))
      (%%get-complement-type-type (%%get-restriction-type-type type))
    (jazz:new-restriction-type base (jazz:new-complement-type type))))


(define (jazz:branch-types test environment)
  (define (process-not expr env)
    (revenv (process-expr expr env)))
  
  (define (process-and expr-list env)
    (let iter ((scan expr-list) (augmented env))
      (if (%%null? scan)
          augmented
        (let ((expr (%%car scan)))
          (let ((newenv (process-expr expr augmented)))
            (iter (%%cdr scan) (%%cons (%%car newenv) (%%cdr env))))))))
  
  (define (process-or expr-list env)
    (let iter ((scan expr-list) (augmented env))
      (if (%%null? scan)
          augmented
        (let ((expr (%%car scan)))
          (let ((newenv (process-expr expr augmented)))
            (iter (%%cdr scan) (%%cons (%%car env) (%%cdr newenv))))))))
  
  (define (process-is expr type-expr env)
    (receive (origin actual-type) (extract-binding expr env)
      (if origin
          (let ((yes-type (cond ((jazz:type? type-expr)
                                 type-expr)
                                ((%%class-is? type-expr jazz:Binding-Reference)
                                 (let ((binding (%%get-reference-binding type-expr)))
                                   (if (%%class-is? binding jazz:Declaration)
                                       (jazz:resolve-binding binding)
                                     #f)))
                                (else
                                 #f))))
            ;; quick try for fun
            (let ((no-type
                    (if (%%eq? actual-type jazz:List)
                        (cond ((%%eq? yes-type jazz:Null)
                               jazz:Pair)
                              ((%%eq? yes-type jazz:Pair)
                               jazz:Null)
                              (else
                               #f))
                      #f)))
              (let ((yes
                      (if yes-type
                          (%%cons (jazz:new-annotated-frame (%%list (jazz:new-restricted-binding origin yes-type)) #f) (%%car env))
                        (%%car env)))
                    (no
                      (if no-type
                          (%%cons (jazz:new-annotated-frame (%%list (jazz:new-restricted-binding origin no-type)) #f) (%%cdr env))
                        (%%cdr env))))
                (%%cons yes no))))
        env)))
  
  (define (extract-binding expr env)
    (if (%%class-is? expr jazz:Binding-Reference)
        (let ((binding (%%get-reference-binding expr)))
          (cond ((%%class-is? binding jazz:Variable)
                 (receive (frame actual-variable actual-type) (jazz:find-annotated binding (%%car env))
                   (let ((origin (%%get-annotated-variable-variable actual-variable)))
                     (values origin actual-type))))
                ;; this is really for slots so i need to think about this
                ((%%class-is? binding jazz:Declaration)
                 (values binding (%%get-lexical-binding-type binding)))
                (else
                 (values #f #f))))
      (values #f #f)))
  
  (define (revenv env)
    (%%cons (%%cdr env) (%%car env)))
  
  (define (process-expr expr env)
    (cond ((%%class-is? expr jazz:And)
           (process-and (%%get-and-expressions expr) env))
          ((%%class-is? expr jazz:Or)
           (process-or (%%get-or-expressions expr) env))
          ((%%class-is? expr jazz:Call)
           (let ((operator (%%get-call-operator expr)))
             (if (%%class-is? operator jazz:Binding-Reference)
                 (let ((operator-binding (%%get-reference-binding operator)))
                   (if (%%class-is? operator-binding jazz:Declaration)
                       (let ((operator-locator (%%get-declaration-locator operator-binding))
                             (arguments (%%get-call-arguments expr)))
                         (let ((count (%%length arguments)))
                           (case operator-locator
                             ((scheme.dialect.runtime.kernel:not)
                              (if (%%fx= count 1)
                                  (process-not (%%car arguments) env)
                                env))
                             ((jazz.dialect.runtime.kernel:is?)
                              (if (%%fx= count 2)
                                  (process-is (%%car arguments) (%%cadr arguments) env)
                                env))
                             ((jazz.dialect.runtime.language.functional:is-not?)
                              (if (%%fx= count 2)
                                  (revenv (process-is (%%car arguments) (%%cadr arguments) env))
                                env))
                             (else
                              (if (%%fx= count 1)
                                  (let ((class (%%table-ref jazz:type-tests operator-locator #f)))
                                    (if class
                                        (process-is (%%car arguments) class env)
                                      (let ((class (%%table-ref jazz:not-type-tests operator-locator #f)))
                                        (if class
                                            (revenv (process-is (%%car arguments) class env))
                                          env))))
                                env)))))
                     env))
               env)))
          (else
           (receive (origin actual-type) (extract-binding expr env)
             (if origin
                 (if (%%class-is? actual-type jazz:Nillable-Type)
                     (let ((yes (%%cons (jazz:new-annotated-frame (%%list (jazz:new-restricted-binding origin (%%get-nillable-type-type actual-type))) #f) (%%car env)))
                           (no (%%cdr env)))
                       (%%cons yes no))
                   env)
               env)))))
  
  (process-expr test (%%cons environment environment)))


(jazz:define-method (jazz:emit-expression (jazz:If expression) declaration environment backend)
  (let ((test (%%get-if-test expression)))
    (jazz:bind (yes-environment . no-environment) (jazz:branch-types test environment)
      (let ((test (jazz:emit-expression test declaration environment backend))
            (yes (jazz:emit-expression (%%get-if-yes expression) declaration yes-environment backend))
            (no (jazz:emit-expression (%%get-if-no expression) declaration no-environment backend)))
        (jazz:new-code
          (jazz:emit 'if backend expression declaration environment test yes no)
          (jazz:extend-type (%%get-code-type yes) (%%get-code-type no))
          (%%get-expression-source expression))))))


(jazz:define-method (jazz:tree-fold (jazz:If expression) down up here seed environment)
  (up expression
      seed
      (jazz:tree-fold
        (%%get-if-no expression) down up here
        (jazz:tree-fold
          (%%get-if-yes expression) down up here
          (jazz:tree-fold
            (%%get-if-test expression) down up here (down expression seed environment) environment)
          environment)
        environment)
      environment))


(jazz:encapsulate-class jazz:If)


;;;
;;;; Cond
;;;


(jazz:define-class-runtime jazz:Cond)


(define (jazz:new-cond source clauses)
  (jazz:allocate-cond jazz:Cond #f source clauses))


(jazz:define-method (jazz:emit-expression (jazz:Cond expression) declaration environment backend)
  (let ((clauses (%%get-cond-clauses expression)))
    (jazz:new-code
      (jazz:emit 'cond backend expression declaration environment)
      (jazz:extend-types (map (lambda (clause)
                                (%%get-code-type
                                  (let ((body (%%cddr clause)))
                                    (jazz:emit-expression body declaration environment backend))))
                              clauses))
      (%%get-expression-source expression))))


(jazz:define-method (jazz:tree-fold (jazz:Cond expression) down up here seed environment)
  (up expression
      seed
      (let fold ((ls (%%get-cond-clauses expression))
                 (seed (down expression seed environment)))
           (if (null? ls)
               seed
             (let* ((clause (%%car ls))
                    (test (%%car clause))
                    (body (%%cddr clause))
                    (seed (jazz:tree-fold body down up here seed environment)))
               (fold (%%cdr ls)
                     (if (%%not test)
                         body
                       (jazz:tree-fold test down up here seed environment))))))
      environment))


(jazz:encapsulate-class jazz:Cond)


;;;
;;;; Case
;;;


(jazz:define-class-runtime jazz:Case)


(define (jazz:new-case source target clauses)
  (jazz:allocate-case jazz:Case #f source target clauses))


(jazz:define-method (jazz:emit-expression (jazz:Case expression) declaration environment backend)
  (let ((target (%%get-case-target expression))
        (clauses (%%get-case-clauses expression)))
    (let ((target-emit (jazz:emit-expression target declaration environment backend))
          (clauses-emit (map (lambda (clause)
                               (let ((body (%%cdr clause)))
                                 (jazz:emit-expression body declaration environment backend)))
                             clauses)))
      (jazz:new-code
        (jazz:emit 'case backend expression declaration environment target-emit clauses clauses-emit)
        (jazz:extend-types (map (lambda (emited-clause)
                                  (%%get-code-type emited-clause))
                                clauses-emit))
        (%%get-expression-source expression)))))


(jazz:define-method (jazz:tree-fold (jazz:Case expression) down up here seed environment)
  (up expression
      seed
      (jazz:tree-fold-list
        (map cdr (%%get-case-clauses expression)) down up here
        (jazz:tree-fold (%%get-case-target expression) down up here (down expression seed environment) environment)
        environment)
      environment))


(jazz:encapsulate-class jazz:Case)


;;;
;;;; And
;;;


(jazz:define-class-runtime jazz:And)


(define (jazz:new-and source expressions)
  (jazz:allocate-and jazz:And #f source expressions))


(jazz:define-method (jazz:emit-expression (jazz:And expression) declaration environment backend)
  (let ((expressions (jazz:emit-expressions (%%get-and-expressions expression) declaration environment backend)))
    (jazz:new-code
      (jazz:emit 'and backend expression declaration environment expressions)
      jazz:Any
      (%%get-expression-source expression))))


(jazz:define-method (jazz:tree-fold (jazz:And expression) down up here seed environment)
  (up expression
      seed
      (jazz:tree-fold-list
        (%%get-and-expressions expression) down up here (down expression seed environment) environment)
      environment))


(jazz:encapsulate-class jazz:And)


;;;
;;;; Or
;;;


(jazz:define-class-runtime jazz:Or)


(define (jazz:new-or source expressions)
  (jazz:allocate-or jazz:Or #f source expressions))


(jazz:define-method (jazz:emit-expression (jazz:Or expression) declaration environment backend)
  (let ((expressions (jazz:emit-expressions (%%get-or-expressions expression) declaration environment backend)))
    (jazz:new-code
      (jazz:emit 'or backend expression declaration environment expressions)
      jazz:Any
      (%%get-expression-source expression))))


(jazz:define-method (jazz:tree-fold (jazz:Or expression) down up here seed environment)
  (up expression
      seed
      (jazz:tree-fold-list
        (%%get-or-expressions expression) down up here (down expression seed environment) environment)
      environment))


(jazz:encapsulate-class jazz:Or)


;;;
;;;; Declare
;;;


(jazz:define-class-runtime jazz:Declare)


(define (jazz:new-declare declarations)
  (jazz:allocate-declare jazz:Declare #f #f declarations))


(jazz:define-method (jazz:emit-expression (jazz:Declare expression) declaration environment backend)
  (jazz:new-code
    (jazz:emit 'declare backend expression declaration environment)
    jazz:Any
    #f))


(jazz:encapsulate-class jazz:Declare)


;;;
;;;; Parameterize
;;;


(jazz:define-class-runtime jazz:Parameterize)


(define (jazz:new-parameterize bindings body)
  (jazz:allocate-parameterize jazz:Parameterize #f #f bindings body))


(jazz:define-method (jazz:emit-expression (jazz:Parameterize expression) declaration environment backend)
  (let ((body (%%get-parameterize-body expression)))
    (let ((body-code (jazz:emit-expression body declaration environment backend)))
      (jazz:new-code
        (jazz:emit 'parameterize backend expression declaration environment body-code)
        (%%get-code-type body-code)
        #f))))


(jazz:define-method (jazz:tree-fold (jazz:Parameterize expression) down up here seed environment)
  (let ((seed2 (jazz:tree-fold-list (map cdr (%%get-parameterize-bindings expression)) down up here (down expression seed environment) environment)))
    (up expression
        seed
        (jazz:tree-fold (%%get-parameterize-body expression) down up here seed2 environment)
        environment)))


(jazz:encapsulate-class jazz:Parameterize)


;;;
;;;; Time Special
;;;


(jazz:define-class-runtime jazz:Time-Special)


(define (jazz:new-time-special expressions)
  (jazz:allocate-time jazz:Time-Special #f #f expressions))


(jazz:define-method (jazz:emit-expression (jazz:Time-Special expression) declaration environment backend)
  (let ((expressions (%%get-time-special-expressions expression)))
    (let ((expressions-emit (jazz:emit-expressions expressions declaration environment backend)))
      (jazz:new-code
        (jazz:emit 'time backend expression declaration environment expressions-emit)
        jazz:Any
        #f))))


(jazz:encapsulate-class jazz:Time-Special))
