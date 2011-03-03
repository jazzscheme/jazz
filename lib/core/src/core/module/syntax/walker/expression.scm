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


(unit protected core.module.syntax.walker.expression


;;;
;;;; Proclaim
;;;


(jazz.define-class-runtime jazz.Proclaim)


(define (jazz.new-proclaim clauses)
  (jazz.allocate-proclaim jazz.Proclaim #f #f clauses))


(jazz.define-method (jazz.emit-expression (jazz.Proclaim expression) declaration environment)
  (let ((clauses (%%get-proclaim-clauses expression))
        (module-declaration (%%get-declaration-toplevel declaration)))
    (for-each (lambda (clause)
                (jazz.proclaim module-declaration clause))
              clauses))
  #f)


(jazz.define-method (jazz.fold-expression (jazz.Proclaim expression) f k s)
  (f expression s))


(jazz.encapsulate-class jazz.Proclaim)


;;;
;;;; Delay
;;;


(jazz.define-class-runtime jazz.Delay)


(define (jazz.new-delay expression)
  (jazz.allocate-delay jazz.Delay #f #f expression))


(jazz.define-method (jazz.emit-expression (jazz.Delay expression) declaration environment)
  (let ((expression (%%get-delay-expression expression)))
    (jazz.new-code
      `(delay ,(jazz.sourcified-form (jazz.emit-expression expression declaration environment)))
      jazz.Any
      #f)))


(jazz.define-method (jazz.fold-expression (jazz.Delay expression) f k s)
  (f expression
     (k (jazz.fold-expression (%%get-delay-expression expression) f k s)
        s)))


(jazz.encapsulate-class jazz.Delay)


;;;
;;;; Quasiquote
;;;


(jazz.define-class-runtime jazz.Quasiquote)


(define (jazz.new-quasiquote form)
  (jazz.allocate-quasiquote jazz.Quasiquote #f #f form))


(jazz.define-method (jazz.emit-expression (jazz.Quasiquote expression) declaration environment)
  (define (emit form)
    (if (%%pair? form)
        (if (or (%%eq? (%%car form) 'unquote)
                (%%eq? (%%car form) 'unquote-splicing))
            (%%list (%%car form) (jazz.sourcified-form (jazz.emit-expression (%%cadr form) declaration environment)))
          (%%cons (emit (%%car form)) (emit (%%cdr form))))
      form))
  
  (jazz.new-code
    (%%list 'quasiquote (emit (%%get-quasiquote-form expression)))
    jazz.List
    #f))


(jazz.define-method (jazz.fold-expression (jazz.Quasiquote expression) f k s)
  (f expression s))


(jazz.encapsulate-class jazz.Quasiquote)


;;;
;;;; Method Reference
;;;


(jazz.define-class-runtime jazz.Method-Reference)


(define (jazz.new-method-reference binding)
  (jazz.allocate-method-reference jazz.Method-Reference #f #f binding))


(jazz.define-method (jazz.emit-expression (jazz.Method-Reference expression) declaration environment)
  (let ((method-declaration (%%get-reference-binding expression)))
    (jazz.new-code
      (%%get-declaration-locator method-declaration)
      (or (%%get-lexical-binding-type method-declaration)
          jazz.Any)
      #f)))


(jazz.define-method (jazz.emit-call (jazz.Method-Reference expression) arguments declaration environment)
  (jazz.new-code
    `(,(jazz.sourcified-form (jazz.emit-expression expression declaration environment)) ,@(jazz.codes-forms arguments))
    jazz.Any
    #f))


(jazz.define-method (jazz.fold-expression (jazz.Method-Reference expression) f k s)
  (f expression s))


(jazz.encapsulate-class jazz.Method-Reference)


;;;
;;;; Lambda
;;;


(jazz.define-class-runtime jazz.Lambda)


(define (jazz.new-lambda type source signature body)
  (jazz.allocate-lambda jazz.Lambda type source signature body))


(jazz.define-method (jazz.emit-expression (jazz.Lambda expression) declaration environment)
  (let ((type (%%get-expression-type expression))
        (signature (%%get-lambda-signature expression))
        (body (%%get-lambda-body expression)))
    (jazz.with-annotated-frame (jazz.annotate-signature signature)
      (lambda (frame)
        (let ((augmented-environment (%%cons frame environment)))
          (let ((signature-output (jazz.emit-signature signature declaration augmented-environment)))
            (let ((body-code (jazz.emit-expression body declaration augmented-environment)))
              (let ((signature-casts (jazz.emit-signature-casts signature declaration augmented-environment))
                    (cast-body (jazz.simplify-begin (jazz.emit-type-cast (jazz.new-code `(begin ,@(jazz.sourcified-form body-code)) (%%get-code-type body-code) #f) type declaration environment))))
                (jazz.new-code
                  (if (%%not signature-casts)
                      `(lambda ,signature-output
                         ,cast-body)
                    `(lambda ,signature-output
                       ,@signature-casts
                       (let ()
                         ,cast-body)))
                  (jazz.new-function-type '() '() '() #f (%%get-code-type body-code))
                  (%%get-expression-source expression))))))))))


(jazz.define-method (jazz.fold-expression (jazz.Lambda expression) f k s)
  (f expression
     (k (jazz.fold-statement (%%get-lambda-body expression) f k s)
        s)))


(jazz.define-method (jazz.tree-fold (jazz.Lambda expression) down up here seed environment)
  (jazz.with-annotated-frame (jazz.annotate-signature (%%get-lambda-signature expression))
    (lambda (frame)
      (let ((aug-env (cons frame environment)))
        (up expression
            seed
            (jazz.tree-fold (%%get-lambda-body expression) down up here (down expression seed environment) aug-env)
            environment)))))


(jazz.encapsulate-class jazz.Lambda)


;;;
;;;; Let
;;;


(jazz.define-class-runtime jazz.Let)


(define (jazz.new-let source bindings body)
  (jazz.allocate-let jazz.Let #f source bindings body))


(jazz.define-method (jazz.emit-expression (jazz.Let expression) declaration environment)
  (let ((bindings (%%get-let-bindings expression))
        (body (%%get-let-body expression)))
    (jazz.with-annotated-frame (jazz.annotate-bindings bindings)
      (lambda (frame)
        (let ((variables (%%get-annotated-frame-variables frame))
              (augmented-environment (%%cons frame environment)))
          (let ((bindings-output
                  (map (lambda (binding annotated-variable)
                         (let ((variable (%%car binding))
                               (value (%%cdr binding)))
                           (let ((value-code (jazz.emit-expression value declaration augmented-environment)))
                             (jazz.extend-annotated-type frame annotated-variable (%%get-code-type value-code))
                             `(,(jazz.emit-binding-symbol variable declaration environment) ,(jazz.emit-type-cast value-code (%%get-lexical-binding-type variable) declaration environment)))))
                       bindings
                       variables)))
            (let ((body-code (jazz.emit-expression body declaration augmented-environment)))
              (jazz.new-code
                `(let ,bindings-output
                   ,@(jazz.sourcified-form body-code))
                (%%get-code-type body-code)
                (%%get-expression-source expression)))))))))


(jazz.define-method (jazz.fold-expression (jazz.Let expression) f k s)
  (f expression
     (k (jazz.fold-expression (%%get-let-body expression) f k s)
        s)))


(jazz.define-method (jazz.tree-fold (jazz.Let expression) down up here seed environment)
  (let* ((bindings (%%get-let-bindings expression))
         (aug-env (cons (map car bindings) environment))
         (seed1 (down expression seed environment))
         (seed2 (jazz.tree-fold-list (map cdr bindings) down up here seed1 environment)))
    (up expression seed (jazz.tree-fold (%%get-let-body expression) down up here seed2 aug-env) environment)))


(jazz.encapsulate-class jazz.Let)


;;;
;;;; Named Let
;;;


(jazz.define-class-runtime jazz.Named-Let)


(define (jazz.new-named-let source variable bindings body)
  (jazz.allocate-named-let jazz.Named-Let #f source bindings body variable))


(jazz.define-method (jazz.emit-expression (jazz.Named-Let expression) declaration environment)
  (let ((variable (%%get-named-let-variable expression))
        (bindings (%%get-let-bindings expression))
        (body (%%get-let-body expression)))
    (jazz.with-annotated-frame (%%cons (jazz.new-annotated-variable variable jazz.Any jazz.Any) (jazz.annotate-bindings bindings))
      (lambda (frame)
        (let ((variables (%%get-annotated-frame-variables frame))
              (augmented-environment (%%cons frame environment)))
          (let ((bindings-output
                  (map (lambda (binding annotated-variable)
                         (let ((variable (%%car binding))
                               (value (%%cdr binding)))
                           (let ((value-code (jazz.emit-expression value declaration augmented-environment)))
                             (jazz.extend-annotated-type frame annotated-variable (%%get-code-type value-code))
                             `(,(jazz.emit-binding-symbol variable declaration environment) ,(jazz.emit-type-cast value-code (%%get-lexical-binding-type variable) declaration environment)))))
                       bindings
                       (%%cdr variables))))
            (let ((body-code (jazz.emit-expression body declaration augmented-environment)))
              (jazz.new-code
                `(let ,(jazz.emit-binding-symbol variable declaration environment) ,bindings-output
                   ,@(jazz.sourcified-form body-code))
                (%%get-code-type body-code)
                (%%get-expression-source expression)))))))))


(jazz.define-method (jazz.fold-expression (jazz.Named-Let expression) f k s)
  (f expression
     (k (jazz.fold-expression (%%get-let-body expression) f k s)
        s)))


(jazz.define-method (jazz.tree-fold (jazz.Named-Let expression) down up here seed environment)
  (let* ((bindings (%%get-let-bindings expression))
         (aug-env (cons (cons (%%get-named-let-variable expression) (map car bindings)) environment))
         (seed1 (down expression seed environment))
         (seed2 (jazz.tree-fold-list (map cdr bindings) down up here seed1 environment)))
    (up expression seed (jazz.tree-fold (%%get-let-body expression) down up here seed2 aug-env) environment)))


(jazz.encapsulate-class jazz.Named-Let)


;;;
;;;; Letstar
;;;


(jazz.define-class-runtime jazz.Letstar)


(define (jazz.new-letstar source bindings body)
  (jazz.allocate-letstar jazz.Letstar #f source bindings body))


(jazz.define-method (jazz.emit-expression (jazz.Letstar expression) declaration environment)
  (let ((bindings (%%get-letstar-bindings expression))
        (body (%%get-letstar-body expression)))
    (jazz.with-annotated-frame (jazz.annotate-bindings bindings)
      (lambda (frame)
        (let ((variables (%%get-annotated-frame-variables frame))
              (augmented-environment (%%cons frame environment)))
          (let ((bindings-output
                  (map (lambda (binding annotated-variable)
                         (let ((variable (%%car binding))
                               (value (%%cdr binding)))
                           (let ((value-code (jazz.emit-expression value declaration augmented-environment)))
                             (jazz.extend-annotated-type frame annotated-variable (%%get-code-type value-code))
                             `(,(jazz.emit-binding-symbol variable declaration environment) ,(jazz.emit-type-cast value-code (%%get-lexical-binding-type variable) declaration environment)))))
                       bindings
                       variables)))
            (let ((body-code (jazz.emit-expression body declaration augmented-environment)))
              (jazz.new-code
                `(let* ,bindings-output
                   ,@(jazz.sourcified-form body-code))
                (%%get-code-type body-code)
                (%%get-expression-source expression)))))))))


(jazz.define-method (jazz.fold-expression (jazz.Letstar expression) f k s)
  (f expression
     (k (jazz.fold-expression (%%get-letstar-body expression) f k s)
        s)))


(jazz.define-method (jazz.tree-fold (jazz.Letstar expression) down up here seed environment)
  (let lp ((ls (%%get-letstar-bindings expression))
           (seed2 (down expression seed environment))
           (aug-env environment))
    (if (pair? ls)
        (lp (cdr ls)
            (jazz.tree-fold (cdar ls) down up here seed2 aug-env)
            (cons (list (caar ls)) aug-env))
        (up expression seed (jazz.tree-fold (%%get-letstar-body expression) down up here seed2 aug-env) environment))))


(jazz.encapsulate-class jazz.Letstar)


;;;
;;;; Letrec
;;;


(jazz.define-class-runtime jazz.Letrec)


(define (jazz.new-letrec source bindings body)
  (jazz.allocate-letrec jazz.Letrec #f source bindings body))


(jazz.define-method (jazz.emit-expression (jazz.Letrec expression) declaration environment)
  (let ((bindings (%%get-letrec-bindings expression))
        (body (%%get-letrec-body expression)))
    (jazz.with-annotated-frame (jazz.annotate-bindings bindings)
      (lambda (frame)
        (let ((variables (%%get-annotated-frame-variables frame))
              (augmented-environment (%%cons frame environment)))
          (let ((bindings-output
                  (map (lambda (binding annotated-variable)
                         (let ((variable (%%car binding))
                               (value (%%cdr binding)))
                           (let ((value-code (jazz.emit-expression value declaration augmented-environment)))
                             (jazz.extend-annotated-type frame annotated-variable (%%get-code-type value-code))
                             `(,(jazz.emit-binding-symbol variable declaration environment) ,(jazz.emit-type-cast value-code (%%get-lexical-binding-type variable) declaration environment)))))
                       bindings
                       variables)))
            (let ((body-code (jazz.emit-expression body declaration augmented-environment)))
              (jazz.new-code
                `(letrec ,bindings-output
                   ,@(jazz.sourcified-form body-code))
                (%%get-code-type body-code)
                (%%get-expression-source expression)))))))))


(jazz.define-method (jazz.fold-expression (jazz.Letrec expression) f k s)
  (f expression
     (k (jazz.fold-expression (%%get-letrec-body expression) f k s)
        s)))


(jazz.define-method (jazz.tree-fold (jazz.Letrec expression) down up here seed environment)
  (let* ((bindings (%%get-letrec-bindings expression))
         (aug-env (cons (map car bindings) environment))
         (seed1 (down expression seed environment))
         (seed2 (jazz.tree-fold-list (map cdr bindings) down up here seed1 aug-env)))
    (up expression seed (jazz.tree-fold (%%get-letrec-body expression) down up here seed2 aug-env) environment)))


(jazz.encapsulate-class jazz.Letrec)


;;;
;;;; Receive
;;;


(jazz.define-class-runtime jazz.Receive)


(define (jazz.new-receive source variables expression body)
  (jazz.allocate-receive jazz.Receive #f source variables expression body))


(jazz.define-method (jazz.emit-expression (jazz.Receive expression) declaration environment)
  (let ((variables (%%get-receive-variables expression))
        (expr (%%get-receive-expression expression))
        (body (%%get-receive-body expression)))
    (jazz.with-annotated-frame (jazz.annotate-receive variables)
      (lambda (frame)
        (let ((augmented-environment (%%cons frame environment)))
          (let ((expression-output (jazz.sourcified-form (jazz.emit-expression expr declaration environment))))
            (let ((body-code (jazz.emit-expression body declaration augmented-environment)))
              (jazz.new-code
                `(receive ,(map (lambda (variable)
                                  (jazz.emit-binding-symbol variable declaration environment))
                                variables)
                     ,expression-output
                   ,@(jazz.sourcified-form body-code))
                (%%get-code-type body-code)
                (%%get-expression-source expression)))))))))


(jazz.define-method (jazz.fold-expression (jazz.Receive expression) f k s)
  (f expression
     (k (jazz.fold-expression (%%get-receive-body expression) f k s)
        s)))


(jazz.define-method (jazz.tree-fold (jazz.Receive expression) down up here seed environment)
  (let* ((aug-env (cons (%%get-receive-variables expression) environment))
         (seed1 (down expression seed environment))
         (seed2 (jazz.tree-fold (%%get-receive-expression expression) down up here seed1 environment)))
    (up expression seed (jazz.tree-fold (%%get-receive-body expression) down up here seed2 aug-env) environment)))


(jazz.encapsulate-class jazz.Receive)


;;;
;;;; Do
;;;


(jazz.define-class-runtime jazz.Do)


(define (jazz.new-do bindings test result body)
  (jazz.allocate-do jazz.Do #f #f bindings test result body))


(jazz.define-method (jazz.emit-expression (jazz.Do expression) declaration environment)
  (let ((bindings (%%get-do-bindings expression))
        (test (%%get-do-test expression))
        (result (%%get-do-result expression))
        (body (%%get-do-body expression)))
    (jazz.with-annotated-frame (jazz.annotate-bindings bindings)
      (lambda (frame)
        (let ((variables (%%get-annotated-frame-variables frame))
              (augmented-environment (%%cons frame environment)))
          (let ((bindings-output
                  (map (lambda (binding annotated-variable)
                         (let ((variable (%%car binding))
                               (init (%%cadr binding))
                               (step (%%cddr binding)))
                           (let ((init-code (jazz.sourcified-form (jazz.emit-expression init declaration augmented-environment)))
                                 (step-code-list (if step (%%list (jazz.sourcified-form (jazz.emit-expression step declaration augmented-environment))) '())))
                             `(,(jazz.emit-binding-symbol variable declaration environment)
                               ,init-code
                               ,@step-code-list))))
                       bindings
                       variables)))
            (let ((test-code (jazz.emit-expression test declaration augmented-environment))
                  (result-code (jazz.emit-expression result declaration augmented-environment))
                  (body-code (jazz.emit-expression body declaration augmented-environment)))
              (jazz.new-code
                `(do ,bindings-output
                     (,(jazz.sourcified-form test-code) ,@(jazz.sourcified-form result-code))
                   ,@(jazz.sourcified-form body-code))
                (%%get-code-type result-code)
                #f))))))))


(jazz.define-method (jazz.fold-expression (jazz.Do expression) f k s)
  (f expression
     (k (jazz.fold-expression (%%get-do-test expression) f k s)
        (k (jazz.fold-expression (%%get-do-result expression) f k s)
           (k (jazz.fold-expression (%%get-do-body expression) f k s)
              s)))))


(jazz.define-method (jazz.tree-fold (jazz.Do expression) down up here seed environment)
  (let* ((aug-env (cons (map car (%%get-do-bindings expression)) environment))
         (seed1 (jazz.tree-fold-list (map cadr (%%get-do-bindings expression)) down up here (down expression seed environment) environment))
         (seed2 (jazz.tree-fold-list (map (lambda (x) (or (cddr x) (cadr x))) (%%get-do-bindings expression)) down up here seed1 aug-env)))
    (up expression
        seed
        (jazz.tree-fold
         (%%get-do-result expression) down up here
         (jazz.tree-fold
          (%%get-do-body expression) down up here
          (jazz.tree-fold (%%get-do-test expression) down up here seed2 aug-env)
          aug-env)
         aug-env)
        environment)))


(jazz.encapsulate-class jazz.Do)


;;;
;;;; Specialized Call
;;;


(define jazz.specializers
  (%%make-table test: eq?))


(define (jazz.add-specializer specialized-declaration specializer)
  (%%table-set! jazz.specializers specialized-declaration
    (%%append (%%table-ref jazz.specializers specialized-declaration '())
              (%%list specializer))))


(define (jazz.get-specializers binding)
  (%%table-ref jazz.specializers binding '()))


(set! jazz.emit-specialized-call
      (lambda (operator locator arguments arguments-codes call declaration environment)
        (if (%%not locator)
            #f
          (or (jazz.emit-specialized-locator locator arguments-codes environment)
              (if (%%class-is? operator jazz.Binding-Reference)
                  (let ((binding (%%get-reference-binding operator)))
                    (let ((specializers (jazz.get-specializers binding)))
                      (let ((types (jazz.codes-types arguments-codes)))
                        (let iter ((scan specializers))
                             (if (%%null? scan)
                                 (begin
                                   (%%when (and (jazz.warnings?) (%%not (%%null? specializers)) (jazz.get-module-warn? (%%get-declaration-toplevel declaration) 'optimizations)
                                             ;; quicky to suppress duplicate warnings as for the moment those are both primitive and specialize
                                             (%%not (%%memq locator '(scheme.dialect.kernel:=
                                                                      scheme.dialect.kernel:<
                                                                      scheme.dialect.kernel:<=
                                                                      scheme.dialect.kernel:>
                                                                      scheme.dialect.kernel:>=
                                                                      scheme.dialect.kernel:+
                                                                      scheme.dialect.kernel:-
                                                                      scheme.dialect.kernel:*
                                                                      scheme.dialect.kernel:/))))
                                     (jazz.debug 'Warning: 'In (%%get-declaration-locator declaration) 'unable 'to 'match 'call 'to 'specialized (%%get-lexical-binding-name binding)))
                                   ;; for debugging
                                   (%%when (%%memq (%%get-lexical-binding-name binding) (jazz.debug-specializers))
                                     (jazz.debug 'Warning: 'In (%%get-declaration-locator declaration) 'unable 'to 'match 'call 'to 'specialized (%%get-lexical-binding-name binding) 'on types))
                                   #f)
                               (let ((specializer (%%car scan)))
                                 (let ((function-type (%%get-lexical-binding-type specializer)))
                                   (if (jazz.match-signature? arguments types function-type)
                                       (or (jazz.emit-inlined-binding-call specializer arguments-codes call declaration environment)
                                           (jazz.new-code
                                             (let ((locator (%%get-declaration-locator specializer)))
                                               `(,locator ,@(jazz.codes-forms arguments-codes)))
                                             (%%get-function-type-result function-type)
                                             #f))
                                     (iter (%%cdr scan))))))))))
                #f)))))


;; quicky because classes are not yet defined at this point
(define jazz.emit-specialized-locator #f)

(set! jazz.emit-specialized-locator
      (lambda (locator arguments-codes environment)
        #f))


;;;
;;;; Primitive New
;;;


(set! jazz.emit-primitive-new-call
      (lambda (operator locator arguments arguments-codes declaration environment)
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


(define jazz.primitive-patterns
  '())


(define (jazz.initialize-primitive-patterns)
  (let ((table (%%make-table test: eq?)))
    (for-each (lambda (pair)
                (let ((operator (%%car pair))
                      (patterns (%%cdr pair)))
                  (%%table-set! table operator
                    (map (lambda (pattern)
                           (let ((name (%%car pattern))
                                 (specifier (%%cadr pattern)))
                             (%%list name (jazz.walk-specifier #f #f #f '() specifier))))
                         patterns))))
              jazz.primitive-patterns)
    (set! jazz.primitive-patterns table)))


(define (jazz.add-primitive-patterns operator patterns)
  (set! jazz.primitive-patterns (%%cons (%%cons operator patterns) jazz.primitive-patterns)))


(define (jazz.get-primitive-patterns locator)
  (%%table-ref jazz.primitive-patterns locator '()))


(jazz.add-primitive-patterns 'scheme.dialect.kernel:=                       '((##fx=  <fx*:bool>)  (##fl=  <fl*:bool>)  (##= <number^number:bool>)))
(jazz.add-primitive-patterns 'scheme.dialect.kernel:<                       '((##fx<  <fx*:bool>)  (##fl<  <fl*:bool>)))
(jazz.add-primitive-patterns 'scheme.dialect.kernel:<=                      '((##fx<= <fx*:bool>)  (##fl<= <fl*:bool>)))
(jazz.add-primitive-patterns 'scheme.dialect.kernel:>                       '((##fx>  <fx*:bool>)  (##fl>  <fl*:bool>)))
(jazz.add-primitive-patterns 'scheme.dialect.kernel:>=                      '((##fx>= <fx*:bool>)  (##fl>= <fl*:bool>)))

(jazz.add-primitive-patterns 'scheme.dialect.kernel:+                       '((##fx+  <fx*:fx>)    (##fl+  <fl*:fl>)    (##+ <int^int:int>) (##+ <number^number:number>)))
(jazz.add-primitive-patterns 'scheme.dialect.kernel:-                       '((##fx-  <fx^fx*:fx>) (##fl-  <fl^fl*:fl>) (##- <int^int:int>) (##- <number^number:number>)))
(jazz.add-primitive-patterns 'scheme.dialect.kernel:*                       '((##fx*  <fx*:fx>)    (##fl*  <fl*:fl>)    (##* <int^int:int>) (##* <number^number:number>)))

;; only done in release as these can crash on a division by zero
(cond-expand
  (release
    (jazz.add-primitive-patterns 'scheme.dialect.kernel:/                   '(                     (##fl/  <fl^fl*:fl>)                     (##/ <number^number:number>)))
    (jazz.add-primitive-patterns 'scheme.dialect.kernel:quotient            '((##fxquotient <fx^fx:fx>))))
  (else))

(jazz.add-primitive-patterns 'scheme.dialect.kernel:floor                   '(                     (##flfloor    <fl:fl>)))
(jazz.add-primitive-patterns 'scheme.dialect.kernel:ceiling                 '(                     (##flceiling  <fl:fl>)))
(jazz.add-primitive-patterns 'scheme.dialect.kernel:truncate                '(                     (##fltruncate <fl:fl>)))
(jazz.add-primitive-patterns 'scheme.dialect.kernel:round                   '(                     (##flround    <fl:fl>)))

(jazz.add-primitive-patterns 'jazz.dialect.kernel:fx+                       '((##fx+ <fx^fx:fx>)))
(jazz.add-primitive-patterns 'jazz.dialect.kernel:fx-                       '((##fx- <fx^fx:fx>)))
(jazz.add-primitive-patterns 'jazz.dialect.kernel:fx*                       '((##fx* <fx^fx:fx>)))

(jazz.add-primitive-patterns 'jazz.dialect.kernel:fl+                       '(                     (##fl+ <fl^fl:fl>)))
(jazz.add-primitive-patterns 'jazz.dialect.kernel:fl-                       '(                     (##fl- <fl^fl:fl>)))
(jazz.add-primitive-patterns 'jazz.dialect.kernel:fl*                       '(                     (##fl* <fl^fl:fl>)))
(jazz.add-primitive-patterns 'jazz.dialect.kernel:fl/                       '(                     (##fl/ <fl^fl:fl>)))

(jazz.add-primitive-patterns 'jazz.dialect.kernel:fixnum->flonum            '((##fixnum->flonum <fx:fl>)))
(jazz.add-primitive-patterns 'jazz.dialect.kernel:flonum->fixnum            '(                     (##flonum->fixnum <fl:fx>)))

(jazz.add-primitive-patterns 'scheme.dialect.kernel:not                     '((##not  <any:bool>)))
(jazz.add-primitive-patterns 'scheme.dialect.kernel:eq?                     '((##eq?  <any^any:bool>)))
(jazz.add-primitive-patterns 'scheme.dialect.kernel:eqv?                    '((##eqv? <any^any:bool>)))

(jazz.add-primitive-patterns 'scheme.dialect.kernel:car                     '((##car    <pair:any>)))
(jazz.add-primitive-patterns 'scheme.dialect.kernel:cdr                     '((##cdr    <pair:any>)))
(jazz.add-primitive-patterns 'scheme.dialect.kernel:length                  '((##length <list:int>)     (##vector-length <vector:int>)          (##string-length <string:int>)))
(jazz.add-primitive-patterns 'jazz.dialect.language.functional:element      '((list-ref <list^int:any>) (##vector-ref    <vector^int:any>)      (##string-ref    <string^int:char>)))
(jazz.add-primitive-patterns 'jazz.dialect.language.functional:set-element! '(                          (##vector-set!   <vector^int^any:void>) (##string-set!   <string^int^char:void>)))


(set! jazz.emit-primitive-call
      (lambda (operator locator arguments arguments-codes declaration environment)
        (if (%%not locator)
            #f
          (let ((patterns (jazz.get-primitive-patterns locator)))
            (let ((types (jazz.codes-types arguments-codes)))
              (let iter ((scan patterns))
                   (if (%%null? scan)
                       (begin
                         (%%when (and (jazz.warnings?) (%%not (%%null? patterns)) (jazz.get-module-warn? (%%get-declaration-toplevel declaration) 'optimizations)
                                   ;; a bit extreme for now
                                   (%%not (%%memq locator '(scheme.dialect.kernel:car
                                                            scheme.dialect.kernel:cdr))))
                           (jazz.debug 'Warning: 'In (%%get-declaration-locator declaration) 'unable 'to 'match 'call 'to 'primitive (jazz.reference-name locator)))
                         #f)
                     (jazz.bind (name function-type) (%%car scan)
                       (if (jazz.match-signature? arguments types function-type)
                           (jazz.new-code
                             `(,name ,@(jazz.codes-forms arguments-codes))
                             (%%get-function-type-result function-type)
                             #f)
                         (iter (%%cdr scan)))))))))))


;;;
;;;; Inlined Call
;;;


(set! jazz.emit-inlined-call
      (lambda (operator arguments call declaration environment)
        (if (%%class-is? operator jazz.Binding-Reference)
            (let ((binding (%%get-reference-binding operator)))
              (jazz.emit-inlined-binding-call binding arguments call declaration environment))
          #f)))


;;;
;;;; Signature
;;;


(define (jazz.match-signature? arguments argument-types function-type)
  (let ((argcount (%%length argument-types))
        (mandatory (%%get-function-type-mandatory function-type))
        (positional (%%get-function-type-positional function-type))
        (optional (%%get-function-type-optional function-type))
        (named (%%get-function-type-named function-type))
        (rest (%%get-function-type-rest function-type)))
    (define (match? arg type expect)
      (if (%%class-is? expect jazz.Category-Type)
          (or (and (%%class-is? arg jazz.Binding-Reference)
                   (%%eq? (%%get-reference-binding arg) (%%get-category-type-declaration expect)))
              (and (%%class-is? type jazz.Category-Type)
                   (%%eq? (%%get-category-type-declaration type) (%%get-category-type-declaration expect))))
        (%%subtype? (or type jazz.Any) expect)))
    
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
            (jazz.every? (lambda (type)
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


(jazz.define-class-runtime jazz.If)


(define (jazz.new-if source test yes no)
  (jazz.allocate-if jazz.If #f source test yes no))


(define jazz.type-tests
  (%%make-table test: eq?))


(%%table-set! jazz.type-tests 'scheme.dialect.kernel:number?       jazz.Number)
(%%table-set! jazz.type-tests 'scheme.dialect.kernel:complex?      jazz.Complex)
(%%table-set! jazz.type-tests 'scheme.dialect.kernel:real?         jazz.Real)
(%%table-set! jazz.type-tests 'scheme.dialect.kernel:rational?     jazz.Rational)
(%%table-set! jazz.type-tests 'scheme.dialect.kernel:integer?      jazz.Integer)
(%%table-set! jazz.type-tests 'scheme.dialect.kernel:number?       jazz.Number)
(%%table-set! jazz.type-tests 'scheme.dialect.kernel:boolean?      jazz.Boolean)
;; not 100% correct because of Scheme's semantic for list?
(%%table-set! jazz.type-tests 'scheme.dialect.kernel:list?         jazz.List)
(%%table-set! jazz.type-tests 'scheme.dialect.kernel:null?         jazz.Null)
(%%table-set! jazz.type-tests 'scheme.dialect.kernel:pair?         jazz.Pair)
(%%table-set! jazz.type-tests 'scheme.dialect.kernel:symbol?       jazz.Symbol)
(%%table-set! jazz.type-tests 'scheme.dialect.kernel:char?         jazz.Char)
(%%table-set! jazz.type-tests 'scheme.dialect.kernel:string?       jazz.String)
(%%table-set! jazz.type-tests 'scheme.dialect.kernel:vector?       jazz.Vector)
(%%table-set! jazz.type-tests 'jazz.dialect.kernel:s8vector?       jazz.S8Vector)
(%%table-set! jazz.type-tests 'jazz.dialect.kernel:u8vector?       jazz.U8Vector)
(%%table-set! jazz.type-tests 'jazz.dialect.kernel:s16vector?      jazz.S16Vector)
(%%table-set! jazz.type-tests 'jazz.dialect.kernel:u16vector?      jazz.U16Vector)
(%%table-set! jazz.type-tests 'jazz.dialect.kernel:s32vector?      jazz.S32Vector)
(%%table-set! jazz.type-tests 'jazz.dialect.kernel:u32vector?      jazz.U32Vector)
(%%table-set! jazz.type-tests 'jazz.dialect.kernel:s64vector?      jazz.S64Vector)
(%%table-set! jazz.type-tests 'jazz.dialect.kernel:u64vector?      jazz.U64Vector)
(%%table-set! jazz.type-tests 'jazz.dialect.kernel:f32vector?      jazz.F32Vector)
(%%table-set! jazz.type-tests 'jazz.dialect.kernel:f64vector?      jazz.F64Vector)
(%%table-set! jazz.type-tests 'jazz.dialect.kernel:continuation?   jazz.Continuation)
(%%table-set! jazz.type-tests 'scheme.dialect.kernel:procedure?    jazz.Procedure)
(%%table-set! jazz.type-tests 'scheme.dialect.kernel:input-port?   jazz.Port)
(%%table-set! jazz.type-tests 'scheme.dialect.kernel:output-port?  jazz.Port)
(%%table-set! jazz.type-tests 'scheme.dialect.kernel:eof-object?   jazz.EOF)
(%%table-set! jazz.type-tests 'jazz.dialect.kernel:fixnum?         jazz.Fixnum)
(%%table-set! jazz.type-tests 'jazz.dialect.kernel:flonum?         jazz.Flonum)
(%%table-set! jazz.type-tests 'jazz.dialect.kernel:keyword?        jazz.Keyword)
(%%table-set! jazz.type-tests 'jazz.dialect.kernel:object?         jazz.Object)
(%%table-set! jazz.type-tests 'jazz.dialect.kernel:category?       jazz.Category)
(%%table-set! jazz.type-tests 'jazz.dialect.kernel:class?          jazz.Class)
(%%table-set! jazz.type-tests 'jazz.dialect.kernel:interface?      jazz.Interface)
(%%table-set! jazz.type-tests 'jazz.dialect.kernel:field?          jazz.Field)
(%%table-set! jazz.type-tests 'jazz.dialect.kernel:slot?           jazz.Slot)
(%%table-set! jazz.type-tests 'jazz.dialect.kernel:method?         jazz.Method)
(%%table-set! jazz.type-tests 'jazz.dialect.kernel:table?          jazz.Table)
(%%table-set! jazz.type-tests 'jazz.dialect.kernel:thread?         jazz.Thread)
(%%table-set! jazz.type-tests 'jazz.dialect.kernel:foreign?        jazz.Foreign)
(%%table-set! jazz.type-tests 'jazz.dialect.kernel:values?         jazz.Values)
(%%table-set! jazz.type-tests 'jazz.dialect.kernel:unspecified?    jazz.Unspecified)


(define jazz.not-type-tests
  (%%make-table test: eq?))


(%%table-set! jazz.not-type-tests 'jazz.dialect.kernel:not-null? jazz.Null)


(define (jazz.restrict-type base type)
  (jazz.new-restriction-type base type))


(define (jazz.restriction-of? type class)
  (and (%%class-is? type jazz.Restriction-Type)
       (%%class-is? (%%get-restriction-type-type type) class)))


(define (jazz.complement-type base type)
  (if (and (jazz.restriction-of? type jazz.Complement-Type)
           (%%eq? (%%get-restriction-type-base type) base))
      (%%get-complement-type-type (%%get-restriction-type-type type))
    (jazz.new-restriction-type base (jazz.new-complement-type type))))


(define (jazz.branch-types test environment)
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
          (let ((yes-type (cond ((jazz.type? type-expr)
                                 type-expr)
                                ((%%class-is? type-expr jazz.Binding-Reference)
                                 (let ((binding (%%get-reference-binding type-expr)))
                                   (if (%%class-is? binding jazz.Declaration)
                                       (jazz.resolve-binding binding)
                                     #f)))
                                (else
                                 #f))))
            ;; quick try for fun
            (let ((no-type
                    (if (%%eq? actual-type jazz.List)
                        (cond ((%%eq? yes-type jazz.Null)
                               jazz.Pair)
                              ((%%eq? yes-type jazz.Pair)
                               jazz.Null)
                              (else
                               #f))
                      #f)))
              (let ((yes
                      (if yes-type
                          (%%cons (jazz.new-annotated-frame (%%list (jazz.new-restricted-binding origin yes-type)) #f) (%%car env))
                        (%%car env)))
                    (no
                      (if no-type
                          (%%cons (jazz.new-annotated-frame (%%list (jazz.new-restricted-binding origin no-type)) #f) (%%cdr env))
                        (%%cdr env))))
                (%%cons yes no))))
        env)))
  
  (define (extract-binding expr env)
    (if (%%class-is? expr jazz.Binding-Reference)
        (let ((binding (%%get-reference-binding expr)))
          (cond ((%%class-is? binding jazz.Variable)
                 (receive (frame actual-variable actual-type) (jazz.find-annotated binding (%%car env))
                   (let ((origin (%%get-annotated-variable-variable actual-variable)))
                     (values origin actual-type))))
                ;; this is really for slots so i need to think about this
                ((%%class-is? binding jazz.Declaration)
                 (values binding (%%get-lexical-binding-type binding)))
                (else
                 (values #f #f))))
      (values #f #f)))
  
  (define (revenv env)
    (%%cons (%%cdr env) (%%car env)))
  
  (define (process-expr expr env)
    (cond ((%%class-is? expr jazz.And)
           (process-and (%%get-and-expressions expr) env))
          ((%%class-is? expr jazz.Or)
           (process-or (%%get-or-expressions expr) env))
          ((%%class-is? expr jazz.Call)
           (let ((operator (%%get-call-operator expr)))
             (if (%%class-is? operator jazz.Binding-Reference)
                 (let ((operator-binding (%%get-reference-binding operator)))
                   (if (%%class-is? operator-binding jazz.Declaration)
                       (let ((operator-locator (%%get-declaration-locator operator-binding))
                             (arguments (%%get-call-arguments expr)))
                         (let ((count (%%length arguments)))
                           (case operator-locator
                             ((scheme.dialect.kernel:not)
                              (if (%%fx= count 1)
                                  (process-not (%%car arguments) env)
                                env))
                             ((jazz.dialect.kernel:is?)
                              (if (%%fx= count 2)
                                  (process-is (%%car arguments) (%%cadr arguments) env)
                                env))
                             ((jazz.dialect.language.functional:is-not?)
                              (if (%%fx= count 2)
                                  (revenv (process-is (%%car arguments) (%%cadr arguments) env))
                                env))
                             (else
                              (if (%%fx= count 1)
                                  (let ((class (%%table-ref jazz.type-tests operator-locator #f)))
                                    (if class
                                        (process-is (%%car arguments) class env)
                                      (let ((class (%%table-ref jazz.not-type-tests operator-locator #f)))
                                        (if class
                                            (revenv (process-is (%%car arguments) class env))
                                          env))))
                                env)))))
                     env))
               env)))
          (else
           (receive (origin actual-type) (extract-binding expr env)
             (if origin
                 (if (%%class-is? actual-type jazz.Nillable-Type)
                     (let ((yes (%%cons (jazz.new-annotated-frame (%%list (jazz.new-restricted-binding origin (%%get-nillable-type-type actual-type))) #f) (%%car env)))
                           (no (%%cdr env)))
                       (%%cons yes no))
                   env)
               env)))))
  
  (process-expr test (%%cons environment environment)))


(jazz.define-method (jazz.emit-expression (jazz.If expression) declaration environment)
  (let ((test (%%get-if-test expression)))
    (jazz.bind (yes-environment . no-environment) (jazz.branch-types test environment)
      (let ((test (jazz.emit-expression test declaration environment))
            (yes (jazz.emit-expression (%%get-if-yes expression) declaration yes-environment))
            (no (jazz.emit-expression (%%get-if-no expression) declaration no-environment)))
        (jazz.new-code
          `(if ,(jazz.sourcified-form test)
               ,(jazz.sourcified-form yes)
             ,(jazz.simplify-begin (jazz.sourcified-form no)))
          (jazz.extend-type (%%get-code-type yes) (%%get-code-type no))
          (%%get-expression-source expression))))))


(jazz.define-method (jazz.fold-expression (jazz.If expression) f k s)
  (f expression
     (k (jazz.fold-expression (%%get-if-test expression) f k s)
        (k (jazz.fold-expression (%%get-if-yes expression) f k s)
           (k (jazz.fold-expression (%%get-if-no expression) f k s)
              s)))))


(jazz.define-method (jazz.tree-fold (jazz.If expression) down up here seed environment)
  (up expression
      seed
      (jazz.tree-fold
       (%%get-if-no expression) down up here
       (jazz.tree-fold
        (%%get-if-yes expression) down up here
        (jazz.tree-fold
         (%%get-if-test expression) down up here (down expression seed environment) environment)
        environment)
       environment)
      environment))


(jazz.encapsulate-class jazz.If)


;;;
;;;; Cond
;;;


(jazz.define-class-runtime jazz.Cond)


(define (jazz.new-cond source clauses)
  (jazz.allocate-cond jazz.Cond #f source clauses))


(jazz.define-method (jazz.emit-expression (jazz.Cond expression) declaration environment)
  (let ((clauses (%%get-cond-clauses expression)))
    (jazz.new-code
      `(cond ,@(let recurse ((clauses clauses)
                             (environment environment))
                    (if (%%null? clauses) '()
                      (let ((clause (%%car clauses)))
                        (let ((test (%%car clause))
                              (arrow? (%%cadr clause))
                              (body (%%cddr clause)))
                          (jazz.bind (yes-environment . no-environment) (jazz.branch-types test environment)
                            (let ((output
                                    `(,(if (%%not test)
                                           'else
                                         (jazz.sourcified-form (jazz.emit-expression test declaration environment)))
                                      ,@(if arrow? `(=>) '())
                                      ,(jazz.sourcified-form (jazz.emit-expression body declaration yes-environment)))))
                              (%%cons output (recurse (%%cdr clauses) no-environment)))))))))
      (jazz.extend-types (map (lambda (clause)
                                (%%get-code-type
                                  (let ((body (%%cddr clause)))
                                    (jazz.emit-expression body declaration environment))))
                              clauses))
      (%%get-expression-source expression))))


(jazz.define-method (jazz.fold-expression (jazz.Cond expression) f k s)
  (f expression (map (lambda (clause)
                       (let ((test (%%car clause))
                             (body (%%cdr clause)))
                         (if (%%not test)
                             (jazz.fold-expression body f k s)
                           (k (jazz.fold-expression test f k s)
                              (k (jazz.fold-expression body f k s)
                                 s)))))
                     (%%get-cond-clauses expression))))


(jazz.define-method (jazz.tree-fold (jazz.Cond expression) down up here seed environment)
  (up expression
      seed
      (let fold ((ls (%%get-cond-clauses expression))
                 (seed (down expression seed environment)))
        (if (null? ls)
            seed
            (let* ((clause (%%car ls))
                   (test (%%car clause))
                   (body (%%cddr clause))
                   (seed (jazz.tree-fold body down up here seed environment)))
              (fold (%%cdr ls)
                    (if (%%not test)
                        body
                        (jazz.tree-fold test down up here seed environment))))))
      environment))


(jazz.encapsulate-class jazz.Cond)


;;;
;;;; Case
;;;


(jazz.define-class-runtime jazz.Case)


(define (jazz.new-case source target clauses)
  (jazz.allocate-case jazz.Case #f source target clauses))


(jazz.define-method (jazz.emit-expression (jazz.Case expression) declaration environment)
  (let ((target (%%get-case-target expression))
        (clauses (%%get-case-clauses expression)))
    (let ((emited-clauses (map (lambda (clause)
                                 (let ((body (%%cdr clause)))
                                   (jazz.emit-expression body declaration environment)))
                               clauses)))
      (jazz.new-code
        `(case ,(jazz.sourcified-form (jazz.emit-expression target declaration environment))
           ,@(map (lambda (clause emited-clause)
                    (let ((tries (%%car clause)))
                      `(,tries ,(jazz.sourcified-form emited-clause))))
                  clauses
                  emited-clauses))
        (jazz.extend-types (map (lambda (emited-clause)
                                  (%%get-code-type emited-clause))
                                emited-clauses))
        (%%get-expression-source expression)))))


(jazz.define-method (jazz.fold-expression (jazz.Case expression) f k s)
  (f expression
     (k (jazz.fold-expression (%%get-case-target expression) f k s)
        (jazz.fold-expressions (map cdr (%%get-case-clauses expression)) f k s s))))


(jazz.define-method (jazz.tree-fold (jazz.Case expression) down up here seed environment)
  (up expression
      seed
      (jazz.tree-fold-list
       (map cdr (%%get-case-clauses expression)) down up here
       (jazz.tree-fold (%%get-case-target expression) down up here (down expression seed environment) environment)
       environment)
      environment))


(jazz.encapsulate-class jazz.Case)


;;;
;;;; And
;;;


(jazz.define-class-runtime jazz.And)


(define (jazz.new-and source expressions)
  (jazz.allocate-and jazz.And #f source expressions))


(jazz.define-method (jazz.emit-expression (jazz.And expression) declaration environment)
  (jazz.new-code
    `(and ,@(jazz.codes-forms (jazz.emit-expressions (%%get-and-expressions expression) declaration environment)))
    jazz.Any
    (%%get-expression-source expression)))


(jazz.define-method (jazz.fold-expression (jazz.And expression) f k s)
  (f expression
     (jazz.fold-expressions (%%get-and-expressions expression) f k s s)))


(jazz.define-method (jazz.tree-fold (jazz.And expression) down up here seed environment)
  (up expression
      seed
      (jazz.tree-fold-list
       (%%get-and-expressions expression) down up here (down expression seed environment) environment)
      environment))


(jazz.encapsulate-class jazz.And)


;;;
;;;; Or
;;;


(jazz.define-class-runtime jazz.Or)


(define (jazz.new-or source expressions)
  (jazz.allocate-or jazz.Or #f source expressions))


(jazz.define-method (jazz.emit-expression (jazz.Or expression) declaration environment)
  (jazz.new-code
    `(or ,@(jazz.codes-forms (jazz.emit-expressions (%%get-or-expressions expression) declaration environment)))
    jazz.Any
    (%%get-expression-source expression)))


(jazz.define-method (jazz.fold-expression (jazz.Or expression) f k s)
  (f expression
     (jazz.fold-expressions (%%get-or-expressions expression) f k s s)))


(jazz.define-method (jazz.tree-fold (jazz.Or expression) down up here seed environment)
  (up expression
      seed
      (jazz.tree-fold-list
       (%%get-or-expressions expression) down up here (down expression seed environment) environment)
      environment))


(jazz.encapsulate-class jazz.Or)


;;;
;;;; Declare
;;;


(jazz.define-class-runtime jazz.Declare)


(define (jazz.new-declare declarations)
  (jazz.allocate-declare jazz.Declare #f #f declarations))


(jazz.define-method (jazz.emit-expression (jazz.Declare expression) declaration environment)
  (let ((declarations (%%get-declare-declarations expression)))
    (jazz.new-code
      `(declare ,@declarations)
      jazz.Any
      #f)))


(jazz.define-method (jazz.fold-expression (jazz.Declare expression) f k s)
  (f expression s))


(jazz.encapsulate-class jazz.Declare)


;;;
;;;; Parameterize
;;;


(jazz.define-class-runtime jazz.Parameterize)


(define (jazz.new-parameterize bindings body)
  (jazz.allocate-parameterize jazz.Parameterize #f #f bindings body))


(jazz.define-method (jazz.emit-expression (jazz.Parameterize expression) declaration environment)
  (let ((bindings (%%get-parameterize-bindings expression))
        (body (%%get-parameterize-body expression)))
    (let ((body-code (jazz.emit-expression body declaration environment)))
      (jazz.new-code
        `(parameterize ,(map (lambda (binding)
                               (let ((variable (%%car binding))
                                     (value (%%cdr binding)))
                                 `(,(jazz.sourcified-form (jazz.emit-expression variable declaration environment))
                                   ,(jazz.sourcified-form (jazz.emit-expression value declaration environment)))))
                             bindings)
           ,@(jazz.sourcified-form body-code))
        (%%get-code-type body-code)
        #f))))


(jazz.define-method (jazz.fold-expression (jazz.Parameterize expression) f k s)
  (f expression s))


(jazz.define-method (jazz.tree-fold (jazz.Parameterize expression) down up here seed environment)
  (let ((seed2 (jazz.tree-fold-list (map cdr (%%get-parameterize-bindings expression)) down up here (down expression seed environment) environment)))
    (up expression
       seed
       (jazz.tree-fold (%%get-parameterize-body expression) down up here seed2 environment)
       environment)))


(jazz.encapsulate-class jazz.Parameterize)


;;;
;;;; Time Special
;;;


(jazz.define-class-runtime jazz.Time-Special)


(define (jazz.new-time-special expressions)
  (jazz.allocate-time jazz.Time-Special #f #f expressions))


(jazz.define-method (jazz.emit-expression (jazz.Time-Special expression) declaration environment)
  (let ((expressions (%%get-time-special-expressions expression)))
    (jazz.new-code
      `(time
         (begin
           ,@(jazz.codes-forms (jazz.emit-expressions expressions declaration environment))))
      jazz.Any
      #f)))


(jazz.define-method (jazz.fold-expression (jazz.Time-Special expression) f k s)
  (f expression s))


(jazz.encapsulate-class jazz.Time-Special)


;;;
;;;; Proclaim
;;;


(jazz.define-virtual-runtime (jazz.validate-proclaim (jazz.Walker walker) resume declaration environment form-src))


(jazz.define-method (jazz.validate-proclaim (jazz.Walker walker) resume declaration environment form-src)
  (if (%%not (%%class-is? declaration jazz.Module-Declaration))
      (jazz.walk-error walker resume declaration form-src "For now, proclaim can only be used at the module level")))


(define (jazz.walk-proclaim walker resume declaration environment form-src)
  (jazz.validate-proclaim walker resume declaration environment form-src)
  (let ((form (%%desourcify form-src)))
    (let ((clauses (%%cdr form)))
      (jazz.new-proclaim clauses)))))
