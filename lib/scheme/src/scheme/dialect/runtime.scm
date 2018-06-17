;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Scheme Walker Runtime
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


(unit protected scheme.dialect.runtime


;;;
;;;; Dialect
;;;


(jazz:define-class jazz:Scheme-Dialect jazz:Dialect (constructor: jazz:allocate-scheme-dialect)
  ())


(define (jazz:new-scheme-dialect name)
  (jazz:allocate-scheme-dialect name (%%make-table test: eq?) (%%make-table test: eq?)))


(jazz:define-method (jazz:dialect-walker (jazz:Scheme-Dialect dialect))
  (jazz:new-scheme-walker))


;;;
;;;; Walker
;;;


(jazz:define-class jazz:Scheme-Walker jazz:Walker (constructor: jazz:allocate-scheme-walker)
  ())


(define (jazz:new-scheme-walker)
  (jazz:allocate-scheme-walker #f #f '() '() '() (jazz:new-queue) (jazz:new-queue) (%%make-table test: eq?) (%%make-table test: eq?) '()))


(jazz:define-method (jazz:runtime-export (jazz:Scheme-Walker walker) declaration)
  (or (nextmethod walker declaration)
      (if (%%is? declaration jazz:Define-Declaration)
          (jazz:get-declaration-locator declaration)
        #f)))


;;;
;;;; Environment
;;;


(jazz:define-method (jazz:walker-declarations (jazz:Scheme-Walker walker))
  (cons (jazz:get-dialect-declarations (jazz:get-dialect 'scheme))
        (nextmethod walker)))


(jazz:define-method (jazz:walker-bindings (jazz:Scheme-Walker walker))
  (cons (jazz:get-dialect-bindings (jazz:get-dialect 'scheme))
        (nextmethod walker)))


;;;
;;;; Define
;;;


(jazz:define-class jazz:Define-Declaration jazz:Declaration (constructor: jazz:allocate-define-declaration)
  ((signature        getter: generate)
   (specifier-source getter: generate)
   (value            getter: generate setter: generate)))


(define (jazz:new-define-declaration name type parent signature specifier-source)
  (let ((new-declaration (jazz:allocate-define-declaration name type #f 'private 'uptodate '() '() #f parent #f #f #f signature specifier-source #f)))
    (jazz:setup-declaration new-declaration)
    new-declaration))


;; Not 100% Scheme compatible as it is possible to set! a define at runtime...
(jazz:define-method (jazz:walk-binding-validate-call (jazz:Define-Declaration declaration) walker resume source-declaration operator arguments form-src)
  (let ((signature (jazz:get-define-declaration-signature declaration)))
    (if signature
        (jazz:validate-arguments walker resume source-declaration declaration signature arguments form-src))))


(jazz:define-method (jazz:emit-declaration (jazz:Define-Declaration declaration) walker resume environment backend)
  (let ((value (jazz:get-define-declaration-value declaration)))
    (let ((expression (jazz:emit-type-cast (jazz:emit-expression value declaration walker resume environment backend) (jazz:get-lexical-binding-type declaration) (jazz:get-expression-source value) declaration walker resume environment backend)))
      (jazz:emit backend 'define declaration walker resume environment expression))))


(jazz:define-method (jazz:emit-binding-reference (jazz:Define-Declaration declaration) source-declaration walker resume environment backend)
  (jazz:new-code
    (jazz:emit backend 'define-reference declaration)
    jazz:Any
    #f))


(jazz:define-method (jazz:walk-binding-validate-assignment (jazz:Define-Declaration declaration) walker resume source-declaration symbol-src)
  (nextmethod declaration walker resume source-declaration symbol-src)
  (%%when (%%neq? (jazz:get-declaration-toplevel declaration) (jazz:get-declaration-toplevel source-declaration))
    (jazz:walk-error walker resume source-declaration symbol-src "Illegal inter-module assignment to: {s}" (jazz:get-lexical-binding-name declaration))))


(jazz:define-method (jazz:walk-binding-assignable? (jazz:Define-Declaration declaration))
  #t)


(jazz:define-method (jazz:emit-binding-assignment (jazz:Define-Declaration declaration) value source-declaration walker resume environment backend)
  (let ((value (jazz:emit-expression value source-declaration walker resume environment backend)))
    (jazz:new-code
      (jazz:emit backend 'define-assignment declaration source-declaration walker resume environment value)
      jazz:Any
      #f)))


(jazz:define-method (jazz:tree-fold (jazz:Define-Declaration declaration) down up here seed environment)
  (here declaration seed environment)
  (jazz:tree-fold (jazz:get-define-declaration-value declaration) down up here seed environment))


(define (jazz:walk-define-declaration walker resume declaration environment form-src)
  (receive (name name-src specifier specifier-source value parameters) (jazz:parse-define walker resume declaration #f form-src)
    (if (%%not (%%class-is? declaration jazz:Namespace-Declaration))
        (jazz:walk-error walker resume declaration form-src "Ill-placed define")
      (let ((type (if specifier (jazz:walk-specifier walker resume declaration environment specifier) jazz:Any))
            (signature (and parameters (jazz:walk-parameters walker resume declaration environment parameters #t #f))))
        (let ((effective-type (if signature (jazz:signature->function-type signature type) type)))
          (let ((new-declaration (or (jazz:find-declaration-child declaration name)
                                     (jazz:new-define-declaration name effective-type declaration signature specifier-source))))
            (jazz:set-declaration-source new-declaration form-src)
            (jazz:set-declaration-name-source new-declaration name-src)
            (let ((effective-declaration (jazz:add-declaration-child walker resume declaration new-declaration)))
              effective-declaration)))))))


(define (jazz:walk-define walker resume declaration environment form-src)
  (receive (name name-src specifier specifier-source value parameters) (jazz:parse-define walker resume declaration #t form-src)
    (if (%%not (%%class-is? declaration jazz:Namespace-Declaration))
        (jazz:walk-error walker resume declaration form-src "Ill-placed define")
      (let* ((new-declaration (jazz:require-declaration declaration name))
             (new-environment (%%cons new-declaration environment)))
        (jazz:set-define-declaration-value new-declaration (jazz:walk walker resume new-declaration new-environment value))
        (jazz:set-declaration-source new-declaration form-src)
        (jazz:set-declaration-name-source new-declaration name-src)
        new-declaration))))


(jazz:define-method (jazz:outline-extract (jazz:Define-Declaration declaration) meta)
  #f)


;;;
;;;; Define Special Form
;;;


(jazz:define-class jazz:Define-Special-Form-Declaration jazz:Declaration (constructor: jazz:allocate-define-special-form-declaration)
  ((signature getter: generate setter: generate)
   (body      getter: generate setter: generate)))


(define (jazz:new-define-special-form-declaration name type parent signature)
  (let ((new-declaration (jazz:allocate-define-special-form-declaration name type #f 'public 'uptodate '() '() #f parent #f #f #f signature #f)))
    (jazz:setup-declaration new-declaration)
    new-declaration))


(jazz:define-method (jazz:walk-binding-walkable? (jazz:Define-Special-Form-Declaration declaration))
  #t)


(jazz:define-method (jazz:walk-binding-walk-form (jazz:Define-Special-Form-Declaration binding) walker resume declaration environment form-src)
  (let ((locator (jazz:get-declaration-locator binding)))
    (if (%%eq? (jazz:get-declaration-toplevel binding) (jazz:get-declaration-toplevel declaration))
        (jazz:walk-error walker resume declaration form-src "Special forms cannot be used from within the same file: {s}" locator)
      (let ((parent-declaration (jazz:get-declaration-parent binding)))
        (jazz:load-unit (jazz:get-declaration-locator (jazz:get-declaration-toplevel parent-declaration)))
        (let ((special-form (jazz:need-special-form locator)))
          (special-form walker resume declaration environment form-src))))))


(jazz:define-method (jazz:tree-fold (jazz:Define-Special-Form-Declaration declaration) down up here seed environment)
  (here declaration seed environment)
  (jazz:tree-fold-list (jazz:get-signature-expressions (jazz:get-define-special-form-declaration-signature declaration)) down up here seed environment)
  (jazz:tree-fold (jazz:get-define-special-form-declaration-body declaration) down up here seed environment))


(jazz:define-method (jazz:outline-extract (jazz:Define-Special-Form-Declaration declaration) meta)
  #f)


(define jazz:Special-Forms
  (%%make-table test: eq?))


(define (jazz:register-special-form name proc)
  (table-set! jazz:Special-Forms name proc))


(define (jazz:get-special-form name)
  (table-ref jazz:Special-Forms name #f))


(define (jazz:need-special-form name)
  (or (jazz:get-special-form name)
      (jazz:error "Unable to find special-form: {s}" name)))


(jazz:define-macro (jazz:define-special-form signature . body)
  (let ((name (car signature))
        (parameters (cdr signature)))
    (let ((src `(lambda ,parameters ,@body)))
      `(begin
         (define ,name
           ,src)
         (jazz:register-special-form ',name ,name)))))


(jazz:define-method (jazz:emit-declaration (jazz:Define-Special-Form-Declaration declaration) walker resume environment backend)
  (let ((locator (jazz:get-declaration-locator declaration))
        (signature (jazz:get-define-special-form-declaration-signature declaration))
        (body (jazz:get-define-special-form-declaration-body declaration)))
    (jazz:with-annotated-frame (jazz:annotate-signature signature)
      (lambda (frame)
        (let ((augmented-environment (cons frame environment)))
          (jazz:sourcify-deep-if
            `(jazz:define-special-form ,(%%cons locator (jazz:emit-signature signature declaration walker resume augmented-environment backend))
               ,@(jazz:sourcified-form (jazz:emit-expression body declaration walker resume augmented-environment backend)))
            (jazz:get-declaration-source declaration)))))))


(define (jazz:parse-define-special-form walker resume declaration rest)
  (let* ((signature (%%desourcify (%%car rest)))
         (body (%%cdr rest))
         (name (%%car signature))
         (type jazz:Any)
         (parameters (%%cdr signature)))
    (values name type parameters body)))


(define (jazz:walk-define-special-form-declaration walker resume declaration environment form-src)
  (receive (name type parameters body) (jazz:parse-define-special-form walker resume declaration (%%cdr (jazz:source-code form-src)))
    (let ((signature (jazz:walk-parameters walker resume declaration environment parameters #t #f)))
      (let ((new-declaration (or (jazz:find-declaration-child declaration name)
                                 (jazz:new-define-special-form-declaration name type declaration signature))))
        (jazz:set-declaration-source new-declaration form-src)
        (let ((effective-declaration (jazz:add-declaration-child walker resume declaration new-declaration)))
          effective-declaration)))))


(define (jazz:walk-define-special-form walker resume declaration environment form-src)
  (receive (name type parameters body) (jazz:parse-define-special-form walker resume declaration (%%cdr (jazz:source-code form-src)))
    (let* ((new-declaration (jazz:require-declaration declaration name)))
      (receive (signature augmented-environment) (jazz:walk-parameters walker resume declaration environment parameters #t #t)
        (jazz:set-define-special-form-declaration-signature new-declaration signature)
        (jazz:set-define-special-form-declaration-body new-declaration
          (jazz:walk-body walker resume new-declaration augmented-environment body))
        (jazz:set-declaration-source new-declaration form-src)
        new-declaration))))


;;;
;;;; Define Macro
;;;


(jazz:define-class jazz:Define-Macro-Declaration jazz:Declaration (constructor: jazz:allocate-define-macro-declaration)
  ((signature getter: generate setter: generate)
   (body      getter: generate setter: generate)))


(define (jazz:new-define-macro-declaration name type parent signature)
  (let ((new-declaration (jazz:allocate-define-macro-declaration name type #f 'public 'uptodate '() '() #f parent #f #f #f signature #f)))
    (jazz:setup-declaration new-declaration)
    new-declaration))


(jazz:define-method (jazz:walk-binding-expandable? (jazz:Define-Macro-Declaration declaration))
  #t)


(jazz:define-method (jazz:walk-binding-expand-form (jazz:Define-Macro-Declaration binding) walker resume declaration environment form-src)
  (let ((form (%%desourcify form-src)))
    (let ((locator (jazz:get-declaration-locator binding)))
      (if (%%eq? (jazz:get-declaration-toplevel binding) (jazz:get-declaration-toplevel declaration))
          (jazz:walk-error walker resume declaration form-src "Macros cannot be used from within the same file: {s}" locator)
        (let ((parent-declaration (jazz:get-declaration-parent binding)))
          (jazz:load-unit (jazz:get-declaration-locator (jazz:get-declaration-toplevel parent-declaration)))
          (let ((expander (jazz:need-macro locator)))
            (jazz:with-walker-context walker resume declaration form-src
              (lambda ()
                (%%apply expander (%%cdr form))))))))))


(jazz:define-method (jazz:emit-declaration (jazz:Define-Macro-Declaration declaration) walker resume environment backend)
  (let ((locator (jazz:get-declaration-locator declaration))
        (signature (jazz:get-define-macro-declaration-signature declaration))
        (body (jazz:get-define-macro-declaration-body declaration)))
    (jazz:with-annotated-frame (jazz:annotate-signature signature)
      (lambda (frame)
        (let ((augmented-environment (%%cons frame environment)))
          (jazz:sourcify-deep-if
            `(jazz:define-macro ,(%%cons locator (jazz:emit-signature signature declaration walker resume augmented-environment backend))
               ,@(jazz:sourcified-form (jazz:emit-expression body declaration walker resume augmented-environment backend)))
            (jazz:get-declaration-source declaration)))))))


(define (jazz:parse-define-macro walker resume declaration rest)
  (%%assertion (and (%%not-null? rest) (%%not-null? (%%cdr rest))) (jazz:walk-error walker resume declaration rest "Ill-formed define-macro")
    (let* ((signature (%%desourcify (%%car rest)))
           (body (%%cdr rest))
           (name (%%car signature))
           (type jazz:Any)
           (parameters (%%cdr signature)))
      (values name type parameters body))))


(define (jazz:walk-define-macro-declaration walker resume declaration environment form-src)
  (receive (name type parameters body) (jazz:parse-define-macro walker resume declaration (%%cdr (jazz:source-code form-src)))
    (let ((signature (jazz:walk-parameters walker resume declaration environment parameters #t #f)))
      (let ((new-declaration (or (jazz:find-declaration-child declaration name)
                                 (jazz:new-define-macro-declaration name type declaration signature))))
        (jazz:set-declaration-source new-declaration form-src)
        (let ((effective-declaration (jazz:add-declaration-child walker resume declaration new-declaration)))
          effective-declaration)))))


(define (jazz:walk-define-macro walker resume declaration environment form-src)
  (receive (name type parameters body) (jazz:parse-define-macro walker resume declaration (%%cdr (jazz:source-code form-src)))
    (let ((new-declaration (jazz:require-declaration declaration name)))
      (receive (signature augmented-environment) (jazz:walk-parameters walker resume declaration environment parameters #t #t)
        (jazz:set-define-macro-declaration-signature new-declaration signature)
        (jazz:set-define-macro-declaration-body new-declaration
          (jazz:walk-body walker resume new-declaration augmented-environment body))
        (jazz:set-declaration-source new-declaration form-src)
        new-declaration))))


(jazz:define-method (jazz:tree-fold (jazz:Define-Macro-Declaration declaration) down up here seed environment)
  (here declaration seed environment)
  (jazz:tree-fold-list (jazz:get-signature-expressions (jazz:get-define-macro-declaration-signature declaration)) down up here seed environment)
  (jazz:tree-fold (jazz:get-define-macro-declaration-body declaration) down up here seed environment))


(jazz:define-method (jazz:outline-extract (jazz:Define-Macro-Declaration declaration) meta)
  #f)


;;;
;;;; Lambda
;;;


(jazz:define-class jazz:Lambda jazz:Expression (constructor: jazz:allocate-lambda)
  ((signature getter: generate)
   (body      getter: generate)))


(define (jazz:new-lambda type source signature body)
  (jazz:allocate-lambda type source signature body))


(jazz:define-variable-override jazz:emit-unsafe
  (lambda (expression source declaration walker resume environment backend)
    (let ((type (jazz:get-expression-type expression))
          (signature (jazz:get-lambda-signature expression))
          (body (jazz:get-lambda-body expression)))
      (jazz:with-annotated-frame (jazz:annotate-signature signature)
        (lambda (frame)
          (let ((augmented-environment (%%cons frame environment)))
            (let ((signature-emit (jazz:emit-signature signature declaration walker resume augmented-environment backend)))
              (let ((body-code (jazz:emit-expression body declaration walker resume augmented-environment backend)))
                (let ((body-emit (jazz:emit backend 'begin expression declaration walker resume environment body-code)))
                  (let ((cast-body (jazz:emit-return-cast (jazz:new-code body-emit (jazz:get-code-type body-code) #f) type (jazz:get-expression-source expression) declaration walker resume environment backend)))
                    ;; the simplify-begin is a quicky to make sure we are
                    ;; eq? to emit-return-cast that also calls simplify-begin
                    #;
                    (let ((cast-needed? (%%neq? (jazz:simplify-begin body-emit) (jazz:simplify-begin cast-body))))
                      (%%when cast-needed?
                        (jazz:warning "Warning: In {a}{a}: Typed definition needs return cast"
                                      (jazz:get-declaration-locator declaration)
                                      (jazz:present-expression-location source #f))))
                    (let ((cast-body (jazz:simplify-begin cast-body)))
                      (jazz:new-code
                        (jazz:emit backend 'lambda expression declaration walker resume environment signature-emit '() cast-body)
                        (jazz:new-function-type '() '() '() #f (jazz:get-code-type body-code))
                        (jazz:get-expression-source expression)))))))))))))


(jazz:define-variable-override jazz:emit-safe
  (lambda (expression source declaration walker resume environment backend)
    (let ((type (jazz:get-expression-type expression))
          (signature (jazz:get-lambda-signature expression))
          (body (jazz:get-lambda-body expression)))
      (jazz:with-annotated-frame (jazz:annotate-signature signature)
        (lambda (frame)
          (let ((augmented-environment (%%cons frame environment)))
            (let ((signature-emit (jazz:emit-signature signature declaration walker resume augmented-environment backend)))
              (let ((body-code (jazz:emit-expression body declaration walker resume augmented-environment backend)))
                (let ((signature-casts (jazz:emit-signature-casts signature declaration walker resume augmented-environment backend))
                      (unsafe-locator (jazz:unsafe-locator (jazz:get-declaration-locator declaration))))
                  (let ((call-expression `(,unsafe-locator ,@(map jazz:get-lexical-binding-name (jazz:get-signature-positional signature)))))
                    (jazz:new-code
                      (jazz:emit backend 'lambda expression declaration walker resume environment signature-emit signature-casts call-expression)
                      (jazz:new-function-type '() '() '() #f (jazz:get-code-type body-code))
                      (jazz:get-expression-source expression))))))))))))


(jazz:define-method (jazz:emit-expression (jazz:Lambda expression) declaration walker resume environment backend)
  (let ((type (jazz:get-expression-type expression))
        (signature (jazz:get-lambda-signature expression))
        (body (jazz:get-lambda-body expression)))
    (jazz:with-annotated-frame (jazz:annotate-signature signature)
      (lambda (frame)
        (let ((augmented-environment (%%cons frame environment)))
          (let ((signature-emit (jazz:emit-signature signature declaration walker resume augmented-environment backend)))
            (let ((body-code (jazz:emit-expression body declaration walker resume augmented-environment backend)))
              (let ((signature-casts (and (jazz:get-check? 'lambda) (jazz:emit-signature-casts signature declaration walker resume augmented-environment backend)))
                    (body-emit (jazz:emit backend 'begin expression declaration walker resume environment body-code)))
                (let ((cast-body (jazz:simplify-begin (jazz:emit-return-cast (jazz:new-code body-emit (jazz:get-code-type body-code) #f) type (jazz:get-expression-source expression) declaration walker resume environment backend))))
                  (jazz:new-code
                    (jazz:emit backend 'lambda expression declaration walker resume environment signature-emit signature-casts cast-body)
                    (jazz:new-function-type '() '() '() #f (jazz:get-code-type body-code))
                    (jazz:get-expression-source expression)))))))))))


(jazz:define-method (jazz:tree-fold (jazz:Lambda expression) down up here seed environment)
  (let ((signature (jazz:get-lambda-signature expression)))
    (jazz:tree-fold-list (jazz:get-signature-expressions signature) down up here seed environment)
    (jazz:with-annotated-frame (jazz:annotate-signature signature)
      (lambda (frame)
        (let ((aug-env (cons frame environment)))
          (up expression
              seed
              (jazz:tree-fold (jazz:get-lambda-body expression) down up here (down expression seed environment) aug-env)
              environment))))))


(define (jazz:walk-lambda walker resume declaration environment form-src)
  (%%assertion (%%not-null? (%%cdr (jazz:source-code form-src))) (jazz:walk-error walker resume declaration form-src "Ill-formed lambda")
    (let ((parameters (jazz:source-code (%%cadr (jazz:source-code form-src)))))
      (jazz:parse-specifier (%%cddr (jazz:source-code form-src))
        (lambda (specifier specifier-source body)
          (receive (signature augmented-environment) (jazz:walk-parameters walker resume declaration environment parameters #t #t)
            (let ((type (if specifier (jazz:walk-specifier walker resume declaration environment specifier) jazz:Any))
                  (effective-body (if (%%null? body) (%%list (%%list 'unspecified)) body)))
              (jazz:new-lambda type form-src signature
                (jazz:walk-body walker resume declaration augmented-environment effective-body)))))))))


;;;
;;;; Binding
;;;


(define (jazz:parse-binding walker resume declaration environment form-src)
  (%%assertion (and (%%pair? (jazz:source-code form-src)) (%%pair? (%%cdr (jazz:source-code form-src)))) (jazz:walk-error walker resume declaration form-src "Ill-formed binding: {s}" (%%desourcify form-src))
    (let ((symbol-src (%%car (jazz:source-code form-src))))
      (let ((symbol (jazz:source-code symbol-src)))
        (jazz:parse-specifier (%%cdr (jazz:source-code form-src))
          (lambda (specifier specifier-source rest)
            (let ((type (if specifier (jazz:walk-specifier walker resume declaration environment specifier) #f))
                  (value (%%car rest)))
              (values (jazz:new-variable symbol type symbol-src specifier-source) value))))))))


;;;
;;;; Let
;;;


(jazz:define-class jazz:Let jazz:Expression (constructor: jazz:allocate-let)
  ((bindings getter: generate)
   (body     getter: generate)))


(define (jazz:new-let source bindings body)
  (jazz:allocate-let #f source bindings body))


;; warning duplicated code with emit-expression
(define (jazz:annotate-let expression declaration walker resume environment backend proc)
  (let ((bindings (jazz:get-let-bindings expression)))
    (jazz:with-annotated-frame (jazz:annotate-bindings bindings)
      (lambda (frame)
        (let ((variables (jazz:get-annotated-frame-variables frame))
              (augmented-environment (%%cons frame environment)))
          (for-each (lambda (binding annotated-variable)
                      (let ((value (%%cdr binding)))
                        (let ((value-code (jazz:emit-expression value declaration walker resume augmented-environment backend)))
                          (jazz:extend-annotated-type frame annotated-variable (jazz:get-code-type value-code)))))
                       bindings
                       variables)
          (proc augmented-environment))))))


(jazz:define-method (jazz:emit-expression (jazz:Let expression) declaration walker resume environment backend)
  (let ((bindings (jazz:get-let-bindings expression))
        (body (jazz:get-let-body expression)))
    (jazz:with-annotated-frame (jazz:annotate-bindings bindings)
      (lambda (frame)
        (let ((variables (jazz:get-annotated-frame-variables frame))
              (augmented-environment (%%cons frame environment)))
          (let ((bindings-output
                  (map (lambda (binding annotated-variable)
                         (let ((variable (%%car binding))
                               (value (%%cdr binding)))
                           (let ((value-code (jazz:emit-expression value declaration walker resume augmented-environment backend))
                                 (src (jazz:get-variable-source variable)))
                             (jazz:extend-annotated-type frame annotated-variable (jazz:get-code-type value-code))
                             (jazz:sourcify-deep-if
                               `(,(jazz:emit-binding-symbol variable declaration environment backend) ,(jazz:emit-type-cast value-code (jazz:get-lexical-binding-type variable) (jazz:get-expression-source value) declaration walker resume environment backend))
                               src))))
                       bindings
                       variables))
                (body-code (jazz:emit-expression body declaration walker resume augmented-environment backend)))
            (jazz:new-code
              (jazz:emit backend 'let expression declaration walker resume environment bindings-output body-code)
              (jazz:get-code-type body-code)
              (jazz:get-expression-source expression))))))))


(jazz:define-method (jazz:tree-fold (jazz:Let expression) down up here seed environment)
  (let* ((bindings (jazz:get-let-bindings expression))
         (aug-env (cons (map car bindings) environment))
         (seed1 (down expression seed environment))
         (seed2 (jazz:tree-fold-list (map cdr bindings) down up here seed1 environment)))
    (up expression seed (jazz:tree-fold (jazz:get-let-body expression) down up here seed2 aug-env) environment)))


(define (jazz:walk-let walker resume declaration environment form-src)
  (let ((unwrapped (jazz:unwrap-syntactic-closure form-src)))
    (%%assertion (%%not-null? (%%cdr unwrapped)) (jazz:walk-error walker resume declaration form-src "Ill-formed let")
      (if (%%symbol? (jazz:unwrap-syntactic-closure (%%cadr unwrapped)))
          (jazz:walk-named-let walker resume declaration environment form-src)
        (let* ((bindings-src (%%cadr unwrapped))
               (bindings (jazz:unwrap-syntactic-closure bindings-src))
               (body (%%cddr (jazz:source-code form-src))))
          (if (and (%%pair? bindings) (%%symbol? (jazz:unwrap-syntactic-closure (%%car bindings))))
              (jazz:signature-named-let walker resume declaration environment form-src bindings body)
            (%%assertion (or (%%null? bindings) (%%pair? bindings)) (jazz:walk-error walker resume declaration bindings-src "Ill-formed let bindings: {s}" bindings)
              (let ((effective-body (if (%%null? body) (%%list (%%list 'unspecified)) body)))
                (let ((augmented-environment environment)
                      (expanded-bindings (jazz:new-queue)))
                  (for-each (lambda (binding-form)
                              (continuation-capture
                                (lambda (resume)
                                  (receive (variable value) (jazz:parse-binding walker resume declaration environment binding-form)
                                    (jazz:enqueue expanded-bindings (%%cons variable (jazz:walk walker resume declaration environment value)))
                                    (set! augmented-environment (%%cons variable augmented-environment))))))
                            bindings)
                  (jazz:new-let form-src
                                (jazz:queue-list expanded-bindings)
                                (jazz:walk-body walker resume declaration augmented-environment effective-body)))))))))))


;;;
;;;; Named Let
;;;


(jazz:define-class jazz:Named-Let jazz:Let (constructor: jazz:allocate-named-let)
  ((variable getter: generate)))


(define (jazz:new-named-let source variable bindings body)
  (jazz:allocate-named-let #f source bindings body variable))


(jazz:define-method (jazz:emit-expression (jazz:Named-Let expression) declaration walker resume environment backend)
  (let ((variable (jazz:get-named-let-variable expression))
        (bindings (jazz:get-let-bindings expression))
        (body (jazz:get-let-body expression)))
    (jazz:with-annotated-frame (%%cons (jazz:new-annotated-variable variable jazz:Any jazz:Any) (jazz:annotate-bindings bindings))
      (lambda (frame)
        (let ((variables (jazz:get-annotated-frame-variables frame))
              (augmented-environment (%%cons frame environment)))
          (let ((variable-emit (jazz:emit-binding-symbol variable declaration environment backend))
                (bindings-output
                  (map (lambda (binding annotated-variable)
                         (let ((variable (%%car binding))
                               (value (%%cdr binding)))
                           (let ((value-code (jazz:emit-expression value declaration walker resume augmented-environment backend))
                                 (src (jazz:get-variable-source variable)))
                             (jazz:extend-annotated-type frame annotated-variable (jazz:get-code-type value-code))
                             (jazz:sourcify-deep-if
                               `(,(jazz:emit-binding-symbol variable declaration environment backend) ,(jazz:emit-type-cast value-code (jazz:get-lexical-binding-type variable) (jazz:get-expression-source value) declaration walker resume environment backend))
                               src))))
                       bindings
                       (%%cdr variables)))
                (body-code (jazz:emit-expression body declaration walker resume augmented-environment backend)))
            (jazz:new-code
              (jazz:emit backend 'named-let expression declaration walker resume environment variable-emit bindings-output body-code)
              (jazz:get-code-type body-code)
              (jazz:get-expression-source expression))))))))


(jazz:define-method (jazz:tree-fold (jazz:Named-Let expression) down up here seed environment)
  (let* ((bindings (jazz:get-let-bindings expression))
         (aug-env (cons (cons (jazz:get-named-let-variable expression) (map car bindings)) environment))
         (seed1 (down expression seed environment))
         (seed2 (jazz:tree-fold-list (map cdr bindings) down up here seed1 environment)))
    (up expression seed (jazz:tree-fold (jazz:get-let-body expression) down up here seed2 aug-env) environment)))


(define (jazz:walk-named-let walker resume declaration environment form-src)
  (let ((name (jazz:source-code (%%cadr (jazz:source-code form-src))))
        (bindings (jazz:source-code (%%car (%%cddr (jazz:source-code form-src)))))
        (body (%%cdr (%%cddr (jazz:source-code form-src)))))
    (let ((augmented-environment environment)
          (expanded-bindings (jazz:new-queue)))
      (for-each (lambda (binding-form)
                  (continuation-capture
                    (lambda (resume)
                      (receive (variable value) (jazz:parse-binding walker resume declaration environment binding-form)
                        (jazz:enqueue expanded-bindings (%%cons variable (jazz:walk walker resume declaration environment value)))
                        (set! augmented-environment (%%cons variable augmented-environment))))))
                bindings)
      (let ((variable (jazz:new-variable name #f #f #f)))
        (set! augmented-environment (%%cons variable augmented-environment))
        (jazz:new-named-let form-src variable
          (jazz:queue-list expanded-bindings)
          (jazz:walk-body walker resume declaration augmented-environment body))))))


(define (jazz:signature-named-let walker resume declaration environment form-src bindings body)
  (let ((name (jazz:source-code (%%car bindings)))
        (bindings (%%cdr bindings)))
    (jazz:walk-named-let walker resume declaration environment
      (jazz:sourcify-deep-if
        `(let ,name ,bindings
           ,@body)
        form-src))))


;;;
;;;; Letstar
;;;


(jazz:define-class jazz:Letstar jazz:Expression (constructor: jazz:allocate-letstar)
  ((bindings getter: generate)
   (body     getter: generate)))


(define (jazz:new-letstar source bindings body)
  (jazz:allocate-letstar #f source bindings body))


(jazz:define-method (jazz:emit-expression (jazz:Letstar expression) declaration walker resume environment backend)
  (let ((bindings (jazz:get-letstar-bindings expression))
        (body (jazz:get-letstar-body expression)))
    (jazz:with-annotated-frame (jazz:annotate-bindings bindings)
      (lambda (frame)
        (let ((variables (jazz:get-annotated-frame-variables frame))
              (augmented-environment (%%cons frame environment)))
          (let ((bindings-output
                  (map (lambda (binding annotated-variable)
                         (let ((variable (%%car binding))
                               (value (%%cdr binding)))
                           (let ((value-code (jazz:emit-expression value declaration walker resume augmented-environment backend))
                                 (src (jazz:get-variable-source variable)))
                             (jazz:extend-annotated-type frame annotated-variable (jazz:get-code-type value-code))
                             (jazz:sourcify-deep-if
                               `(,(jazz:emit-binding-symbol variable declaration environment backend) ,(jazz:emit-type-cast value-code (jazz:get-lexical-binding-type variable) (jazz:get-expression-source value) declaration walker resume environment backend))
                               src))))
                       bindings
                       variables))
                (body-code (jazz:emit-expression body declaration walker resume augmented-environment backend)))
            (jazz:new-code
              (jazz:emit backend 'letstar expression declaration walker resume environment bindings-output body-code)
              (jazz:get-code-type body-code)
              (jazz:get-expression-source expression))))))))


(jazz:define-method (jazz:tree-fold (jazz:Letstar expression) down up here seed environment)
  (let lp ((ls (jazz:get-letstar-bindings expression))
           (seed2 (down expression seed environment))
           (aug-env environment))
       (if (pair? ls)
           (lp (cdr ls)
               (jazz:tree-fold (cdar ls) down up here seed2 aug-env)
               (cons (list (caar ls)) aug-env))
         (up expression seed (jazz:tree-fold (jazz:get-letstar-body expression) down up here seed2 aug-env) environment))))


(define (jazz:walk-letstar walker resume declaration environment form-src)
  (%%assertion (%%not-null? (%%cdr (jazz:unwrap-syntactic-closure form-src))) (jazz:walk-error walker resume declaration form-src "Ill-formed let*")
    (let ((bindings (jazz:unwrap-syntactic-closure (%%cadr (jazz:unwrap-syntactic-closure form-src))))
          (body (%%cddr (jazz:source-code form-src))))
      (let ((augmented-environment environment)
            (expanded-bindings (jazz:new-queue)))
        (for-each (lambda (binding-form)
                    (continuation-capture
                      (lambda (resume)
                        (receive (variable value) (jazz:parse-binding walker resume declaration environment binding-form)
                          (jazz:enqueue expanded-bindings (%%cons variable (jazz:walk walker resume declaration augmented-environment value)))
                          (set! augmented-environment (%%cons variable augmented-environment))))))
                  bindings)
        (jazz:new-letstar form-src
                          (jazz:queue-list expanded-bindings)
                          (jazz:walk-body walker resume declaration augmented-environment body))))))


;;;
;;;; Letrec
;;;


(jazz:define-class jazz:Letrec jazz:Expression (constructor: jazz:allocate-letrec)
  ((bindings getter: generate)
   (body     getter: generate)))


(define (jazz:new-letrec source bindings body)
  (jazz:allocate-letrec #f source bindings body))


(jazz:define-method (jazz:emit-expression (jazz:Letrec expression) declaration walker resume environment backend)
  (let ((bindings (jazz:get-letrec-bindings expression))
        (body (jazz:get-letrec-body expression)))
    (jazz:with-annotated-frame (jazz:annotate-bindings bindings)
      (lambda (frame)
        (let ((variables (jazz:get-annotated-frame-variables frame))
              (augmented-environment (%%cons frame environment)))
          (let ((bindings-output
                  (map (lambda (binding annotated-variable)
                         (let ((variable (%%car binding))
                               (value (%%cdr binding)))
                           (let ((value-code (jazz:emit-expression value declaration walker resume augmented-environment backend))
                                 (src (jazz:get-variable-source variable)))
                             (jazz:extend-annotated-type frame annotated-variable (jazz:get-code-type value-code))
                             (jazz:sourcify-deep-if
                               `(,(jazz:emit-binding-symbol variable declaration environment backend) ,(jazz:emit-type-cast value-code (jazz:get-lexical-binding-type variable) (jazz:get-expression-source value) declaration walker resume environment backend))
                               src))))
                       bindings
                       variables))
                (body-code (jazz:emit-expression body declaration walker resume augmented-environment backend)))
            (jazz:new-code
              (jazz:emit backend 'letrec expression declaration walker resume environment bindings-output body-code)
              (jazz:get-code-type body-code)
              (jazz:get-expression-source expression))))))))


(jazz:define-method (jazz:tree-fold (jazz:Letrec expression) down up here seed environment)
  (let* ((bindings (jazz:get-letrec-bindings expression))
         (aug-env (cons (map car bindings) environment))
         (seed1 (down expression seed environment))
         (seed2 (jazz:tree-fold-list (map cdr bindings) down up here seed1 aug-env)))
    (up expression seed (jazz:tree-fold (jazz:get-letrec-body expression) down up here seed2 aug-env) environment)))


(define (jazz:walk-letrec walker resume declaration environment form-src)
  (%%assertion (%%not-null? (%%cdr (jazz:unwrap-syntactic-closure form-src))) (jazz:walk-error walker resume declaration form-src "Ill-formed letrec")
    (let ((bindings (jazz:unwrap-syntactic-closure (%%cadr (jazz:unwrap-syntactic-closure form-src))))
          (body (%%cddr (jazz:source-code form-src))))
      (let* ((new-variables (map (lambda (binding-form) (jazz:new-variable (jazz:source-code (%%car (jazz:source-code binding-form))) #f #f #f)) bindings))
             (augmented-environment (%%append new-variables environment))
             (expanded-bindings (jazz:new-queue)))
        (for-each (lambda (variable binding-form)
                    (continuation-capture
                      (lambda (resume)
                        (let ((value (%%cadr (jazz:source-code binding-form))))
                          (jazz:enqueue expanded-bindings (%%cons variable (jazz:walk walker resume declaration augmented-environment value)))))))
                  new-variables
                  bindings)
        (jazz:new-letrec form-src
                         (jazz:queue-list expanded-bindings)
                         (jazz:walk-body walker resume declaration augmented-environment body))))))


;;;
;;;; Receive
;;;


(jazz:define-class jazz:Receive jazz:Expression (constructor: jazz:allocate-receive)
  ((variables  getter: generate)
   (expression getter: generate)
   (body       getter: generate)))


(define (jazz:new-receive source variables expression body)
  (jazz:allocate-receive #f source variables expression body))


(jazz:define-method (jazz:emit-expression (jazz:Receive expression) declaration walker resume environment backend)
  (let ((variables (jazz:get-receive-variables expression))
        (expr (jazz:get-receive-expression expression))
        (body (jazz:get-receive-body expression)))
    (let ((expression-output (jazz:emit-expression expr declaration walker resume environment backend)))
      (jazz:with-annotated-frame (jazz:annotate-receive variables (jazz:get-code-type expression-output))
        (lambda (frame)
          (let ((augmented-environment (%%cons frame environment)))
            (let ((bindings-output (map (lambda (variable)
                                          (jazz:emit-binding-symbol variable declaration environment backend))
                                        variables))
                  (body-code (jazz:emit-expression body declaration walker resume augmented-environment backend)))
              (jazz:new-code
                (jazz:emit backend 'receive expression declaration walker resume environment bindings-output (jazz:sourcified-form expression-output) body-code)
                (jazz:get-code-type body-code)
                (jazz:get-expression-source expression)))))))))


(jazz:define-method (jazz:tree-fold (jazz:Receive expression) down up here seed environment)
  (let* ((aug-env (cons (jazz:get-receive-variables expression) environment))
         (seed1 (down expression seed environment))
         (seed2 (jazz:tree-fold (jazz:get-receive-expression expression) down up here seed1 environment)))
    (up expression seed (jazz:tree-fold (jazz:get-receive-body expression) down up here seed2 aug-env) environment)))


(define (jazz:walk-receive walker resume declaration environment form-src)
  (define (walk-parameters parameters)
    (let ((queue (jazz:new-queue)))
      (let iter ((scan parameters))
        (if (%%null? scan)
            (jazz:queue-list queue)
          (let ((expr (jazz:source-code (%%car scan))))
            (%%assert (%%symbol? expr)
              (jazz:parse-specifier (%%cdr scan)
                (lambda (specifier specifier-source rest)
                  (let ((type (if specifier (jazz:walk-specifier walker resume declaration environment specifier) jazz:Any)))
                    (jazz:enqueue queue (jazz:new-variable expr type #f #f))
                    (iter rest))))))))))
  
  (%%assertion (%%not-null? (%%cdr (jazz:unwrap-syntactic-closure form-src))) (jazz:walk-error walker resume declaration form-src "Ill-formed receive")
    (let* ((parameters (jazz:source-code (%%cadr (jazz:unwrap-syntactic-closure form-src))))
           (expression (%%car (%%cddr (jazz:unwrap-syntactic-closure form-src))))
           (body (%%cdr (%%cddr (jazz:source-code form-src))))
           (variables (walk-parameters parameters))
           (new-environment (%%append variables environment)))
      (jazz:new-receive form-src variables (continuation-capture
                                             (lambda (resume)
                                               (jazz:walk walker resume declaration environment expression)))
        (jazz:walk-body walker resume declaration new-environment body)))))


;;;
;;;; Let Macro
;;;


(define (jazz:walk-let-macro walker resume declaration environment form-src)
  (let ((form (%%desourcify form-src)))
    (let* ((bindings (%%cadr form))
           (body (%%cddr form))
           (macro-forms (map (lambda (binding)
                               (let ((name (%%car binding))
                                     (expander (%%cadr binding)))
                                 (jazz:new-macro-form name (eval expander))))
                             bindings))
           (new-environment (%%append macro-forms environment)))
      `(begin
         ,@(jazz:walk-body walker resume declaration new-environment body)))))


;;;
;;;; Let Symbol
;;;


(define (jazz:walk-let-symbol walker resume declaration environment form-src)
  (let ((form (%%desourcify form-src)))
    (let* ((bindings (%%cadr form))
           (body (%%cddr form))
           (macro-symbols (map (lambda (binding)
                                 (let ((name (%%car binding))
                                       (getter (%%cadr binding))
                                       (setter (%%car (%%cddr binding))))
                                   (jazz:new-macro-symbol name (eval getter) (eval setter))))
                               bindings))
           (new-environment (%%append macro-symbols environment)))
      `(begin
         ,@(jazz:walk-body walker resume declaration new-environment body)))))


;;;
;;;; If
;;;


(define (jazz:walk-if walker resume declaration environment form-src)
  (if (%%fx< (%%length (jazz:source-code form-src)) 3)
      (jazz:walk-error walker resume declaration form-src "Ill-formed if: {s}" (%%desourcify form-src))
    (let ((test (%%cadr (jazz:source-code form-src)))
          (yes (%%car (%%cddr (jazz:source-code form-src))))
          (no (%%cdr (%%cddr (jazz:source-code form-src)))))
      (jazz:new-if form-src
                   (continuation-capture
                     (lambda (resume)
                       (jazz:walk walker resume declaration environment test)))
                   (continuation-capture
                     (lambda (resume)
                       (jazz:walk walker resume declaration environment yes)))
                   (if (%%null? no)
                       #f
                     (jazz:walk walker resume declaration environment
                       (%%cons 'begin no)))))))


;;;
;;;; Cond
;;;


(define (jazz:walk-cond walker resume declaration environment form-src)
  (let ((clauses (%%cdr (jazz:source-code form-src))))
    (if (%%null? clauses)
        (jazz:walk-error walker resume declaration form-src "Ill-formed cond")
      (let ((expanded-clauses (jazz:new-queue)))
        (for-each (lambda (clause)
                    (%%assertion (%%pair? (jazz:source-code clause)) (jazz:walk-error walker resume declaration clause "Ill-formed cond clause: {s}" (%%desourcify clause))
                      (let ((test (%%car (jazz:source-code clause)))
                            (body (%%cdr (jazz:source-code clause))))
                        (continuation-capture
                          (lambda (resume)
                            (jazz:enqueue expanded-clauses
                                          (if (and (%%not-null? body) (%%eq? (jazz:unwrap-syntactic-closure (%%car body)) '=>))
                                              (%%cons (jazz:walk walker resume declaration environment test)
                                                      (%%cons #t (jazz:walk walker resume declaration environment (%%cadr body))))
                                            (%%cons (if (%%eq? (jazz:unwrap-syntactic-closure test) 'else)
                                                        (begin
                                                          (%%when (%%null? body)
                                                            (jazz:walk-error walker resume declaration clause "Ill-formed else clause: {s}" (%%desourcify clause)))
                                                          #f)
                                                      (jazz:walk walker resume declaration environment test))
                                                    (%%cons #f (and (%%not-null? body) (jazz:walk-implicit-begin walker resume declaration environment clause body)))))))))))
                  clauses)
        (jazz:new-cond form-src (jazz:queue-list expanded-clauses))))))


;;;
;;;; Case
;;;


(define (jazz:walk-case walker resume declaration environment form-src)
  (let ((form (%%cdr (jazz:source-code form-src))))
    (if (%%null? form)
        (jazz:walk-error walker resume declaration form-src "Ill-formed case")
      (let ((target (%%car form))
            (clauses (%%cdr form)))
        (if (%%null? clauses)
            (jazz:walk-error walker resume declaration form-src "Ill-formed case")
          (jazz:new-case form-src
                         (continuation-capture
                           (lambda (resume)
                             (jazz:walk walker resume declaration environment target)))
                         (map (lambda (clause)
                                (if (%%not (%%pair? (jazz:source-code clause)))
                                    (jazz:walk-error walker resume declaration clause "Ill-formed case clause: {s}" (%%desourcify clause))
                                  (continuation-capture
                                    (lambda (resume)
                                      (let* ((tries-src (%%car (jazz:source-code clause)))
                                             (tries (%%desourcify tries-src))
                                             (body (%%cdr (jazz:source-code clause)))
                                             (effective-body (if (%%null? body) (%%list (%%list 'unspecified)) body)))
                                        (if (or (%%eq? (jazz:unwrap-syntactic-closure tries) 'else) (%%pair? tries))
                                            (%%cons tries (jazz:walk-implicit-begin walker resume declaration environment clause effective-body))
                                          (jazz:walk-error walker resume declaration tries-src "Ill-formed selector list: {s}" tries)))))))
                              clauses)))))))


;;;
;;;; And
;;;


(define (jazz:walk-and walker resume declaration environment form-src)
  (jazz:new-and form-src (jazz:walk-list walker resume declaration environment (%%cdr (jazz:source-code form-src)))))


;;;
;;;; Or
;;;


(define (jazz:walk-or walker resume declaration environment form-src)
  (jazz:new-or form-src (jazz:walk-list walker resume declaration environment (%%cdr (jazz:source-code form-src)))))


;;;
;;;; Begin
;;;


(define (jazz:walk-begin walker resume declaration environment form-src)
  (let ((body (%%cdr (jazz:source-code form-src))))
    (jazz:new-begin form-src (jazz:walk-list walker resume declaration environment body))))


(define (jazz:walk-implicit-begin walker resume declaration environment form-src form-list)
  (if (%%null? form-list)
      (jazz:walk walker resume declaration environment '(unspecified))
    (jazz:new-begin form-src (jazz:walk-list walker resume declaration environment form-list))))


;;;
;;;; Do
;;;


(jazz:define-class jazz:Do jazz:Expression (constructor: jazz:allocate-do)
  ((bindings getter: generate)
   (test     getter: generate)
   (result   getter: generate)
   (body     getter: generate)))


(define (jazz:new-do bindings test result body)
  (jazz:allocate-do #f #f bindings test result body))


(jazz:define-method (jazz:emit-expression (jazz:Do expression) declaration walker resume environment backend)
  (let ((bindings (jazz:get-do-bindings expression))
        (test (jazz:get-do-test expression))
        (result (jazz:get-do-result expression))
        (body (jazz:get-do-body expression)))
    (jazz:with-annotated-frame (jazz:annotate-bindings bindings)
      (lambda (frame)
        (let ((variables (jazz:get-annotated-frame-variables frame))
              (augmented-environment (%%cons frame environment)))
          (let ((bindings-output
                  (map (lambda (binding annotated-variable)
                         (let ((variable (%%car binding))
                               (init (%%cadr binding))
                               (step (%%cddr binding)))
                           (let ((init-code (jazz:sourcified-form (jazz:emit-expression init declaration walker resume augmented-environment backend)))
                                 (step-code-list (if step (%%list (jazz:sourcified-form (jazz:emit-expression step declaration walker resume augmented-environment backend))) '())))
                             `(,(jazz:emit-binding-symbol variable declaration environment backend)
                               ,init-code
                               ,@step-code-list))))
                       bindings
                       variables))
                (test-code (jazz:emit-expression test declaration walker resume augmented-environment backend))
                (result-code (jazz:emit-expression result declaration walker resume augmented-environment backend))
                (body-code (jazz:emit-expression body declaration walker resume augmented-environment backend)))
            (jazz:new-code
              (jazz:emit backend 'do expression declaration walker resume environment bindings-output test-code result-code body-code)
              (jazz:get-code-type result-code)
              #f)))))))


(jazz:define-method (jazz:tree-fold (jazz:Do expression) down up here seed environment)
  (let* ((aug-env (cons (map car (jazz:get-do-bindings expression)) environment))
         (seed1 (jazz:tree-fold-list (map cadr (jazz:get-do-bindings expression)) down up here (down expression seed environment) environment))
         (seed2 (jazz:tree-fold-list (map (lambda (x) (or (cddr x) (cadr x))) (jazz:get-do-bindings expression)) down up here seed1 aug-env)))
    (up expression
        seed
        (jazz:tree-fold
          (jazz:get-do-result expression) down up here
          (jazz:tree-fold
            (jazz:get-do-body expression) down up here
            (jazz:tree-fold (jazz:get-do-test expression) down up here seed2 aug-env)
            aug-env)
          aug-env)
        environment)))


(define (jazz:walk-do walker resume declaration environment form-src)
  (let ((bindings (jazz:source-code (%%cadr (jazz:source-code form-src))))
        (test (%%car (jazz:source-code (%%car (%%cddr (jazz:source-code form-src))))))
        (result (%%cdr (jazz:source-code (%%car (%%cddr (jazz:source-code form-src))))))
        (body (%%cdr (%%cddr (jazz:source-code form-src)))))
    (let* ((new-variables (map (lambda (binding-form) (jazz:new-variable (jazz:source-code (%%car (jazz:source-code binding-form))) #f #f #f)) bindings))
           (augmented-environment (%%append new-variables environment))
           (expanded-bindings (jazz:new-queue)))
      (for-each (lambda (variable binding-form)
                  (continuation-capture
                    (lambda (resume)
                      (let ((init (jazz:walk walker resume declaration augmented-environment (%%cadr (jazz:source-code binding-form))))
                            (step (if (%%null? (%%cddr (jazz:source-code binding-form))) #f (jazz:walk walker resume declaration augmented-environment (%%car (%%cddr (jazz:source-code binding-form)))))))
                        (jazz:enqueue expanded-bindings (%%cons variable (%%cons init step)))))))
                new-variables
                bindings)
      (jazz:new-do (jazz:queue-list expanded-bindings)
                   (continuation-capture
                     (lambda (resume)
                       (jazz:walk walker resume declaration augmented-environment test)))
                   (continuation-capture
                     (lambda (resume)
                       (jazz:walk-body walker resume declaration augmented-environment result)))
                   (jazz:walk-body walker resume declaration augmented-environment body)))))

;;;
;;;; Delay
;;;


(define (jazz:walk-delay walker resume declaration environment form-src)
  (let ((expression (%%cadr (jazz:source-code form-src))))
    (jazz:new-delay (jazz:walk walker resume declaration environment expression))))


;;;
;;;; Unspecific
;;;


(jazz:define-class jazz:Unspecific jazz:Expression (constructor: jazz:allocate-unspecific)
  ((expressions getter: generate)))


(define (jazz:new-unspecific source expressions)
  (jazz:allocate-unspecific #f source expressions))


(jazz:define-method (jazz:emit-expression (jazz:Unspecific expression) declaration walker resume environment backend)
  (let ((expressions (jazz:get-unspecific-expressions expression)))
    (let ((code (jazz:emit-statements-code expressions declaration walker resume environment backend)))
      (jazz:new-code
        (jazz:emit backend 'unspecific expression declaration walker resume environment code)
        (jazz:get-code-type code)
        (jazz:get-expression-source expression)))))


(jazz:define-method (jazz:tree-fold (jazz:Unspecific expression) down up here seed environment)
  (up expression
      seed
      (jazz:tree-fold-list
        (jazz:get-unspecific-expressions expression) down up here
        (down expression seed environment)
        environment)
      environment))


(define (jazz:walk-unspecific walker resume declaration environment form-src)
  (let ((body (%%cdr (jazz:source-code form-src))))
    (jazz:new-unspecific form-src (jazz:walk-list walker resume declaration environment body))))


;;;
;;;; Reference Reification
;;;


(jazz:define-class jazz:Reference-Reification jazz:Expression (constructor: jazz:allocate-reference-reification)
  ((reference getter: generate)
   (resolver  getter: generate)))


(define (jazz:new-reference-reification source reference resolver)
  (jazz:allocate-reference-reification #f source reference resolver))


(jazz:define-method (jazz:emit-expression (jazz:Reference-Reification expression) declaration walker resume environment backend)
  ;; all this should be cleanly obtained from the code walk but it requires a
  ;; non-trivial work of changing references to remember how they where obtained
  (define (determine-serialization reference)
    (let ((reified-reference (jazz:get-reference-reification-reference expression)))
      (if (jazz:composite-reference? reified-reference)
          (receive (module-name name) (jazz:break-reference reified-reference)
            `(module-public ,module-name ,name))
        (let ((module (jazz:get-declaration-toplevel declaration))
              (binding (jazz:get-binding-reference-binding reference)))
          (if (%%is? binding jazz:Declaration)
              (if (%%eq? (jazz:get-declaration-toplevel binding) module)
                  `(module-private ,(jazz:get-declaration-locator binding))
                (let ((import (find-import module reified-reference))
                      (name (jazz:get-lexical-binding-name binding)))
                  (if import
                      `(module-public ,import ,name)
                    #f)))
            #f)))))
  
  (define (find-import module symbol)
    (let iter ((scan (jazz:get-module-declaration-imports module)))
      (if (%%null? scan)
          #f
        (let ((module-invoice (%%car scan)))
          (let ((imported-module-declaration (jazz:get-module-invoice-module module-invoice)))
            (let ((imported (jazz:get-public-lookup imported-module-declaration)))
              (if (%%table-ref imported symbol #f)
                  (jazz:get-lexical-binding-name (jazz:get-module-invoice-module module-invoice))
                (iter (%%cdr scan)))))))))
  
  (let ((resolver (jazz:get-reference-reification-resolver expression)))
    (let ((reference (%%car (jazz:get-body-expressions (jazz:get-lambda-body resolver)))))
      (%%assert (%%is? reference jazz:Binding-Reference)
        (let ((serialization (determine-serialization reference)))
          (jazz:new-code
            `(jazz:new-runtime-reference ,(jazz:get-code-form (jazz:emit-expression resolver declaration walker resume environment backend)) ',serialization)
            jazz:Any
            #f))))))


(define (jazz:walk-reify-reference walker resume declaration environment form-src)
  (let ((reference-src (%%cadr (jazz:source-code form-src))))
    (let ((resolver `(lambda () ,reference-src)))
      (jazz:new-reference-reification form-src (jazz:source-code reference-src) (jazz:walk walker resume declaration environment resolver)))))


;;;
;;;; Register
;;;


(jazz:define-dialect scheme
  (jazz:new-scheme-dialect 'scheme))


(jazz:define-walker-declaration define              scheme jazz:walk-define-declaration jazz:walk-define)
(jazz:define-walker-declaration define-macro        scheme jazz:walk-define-macro-declaration jazz:walk-define-macro)
(jazz:define-walker-declaration define-special-form scheme jazz:walk-define-special-form-declaration jazz:walk-define-special-form)
;; quicky to put this here
(jazz:define-walker-declaration special-form        scheme jazz:walk-define-special-form-declaration jazz:walk-define-special-form)
(jazz:define-walker-special     cond-expand         scheme jazz:walk-cond-expand)
(jazz:define-walker-special     quote               scheme jazz:walk-quote)
(jazz:define-walker-special     begin               scheme jazz:walk-begin)
(jazz:define-walker-special     lambda              scheme jazz:walk-lambda)
(jazz:define-walker-special     let                 scheme jazz:walk-let)
(jazz:define-walker-special     let*                scheme jazz:walk-letstar)
(jazz:define-walker-special     letrec              scheme jazz:walk-letrec)
(jazz:define-walker-special     let-macro           scheme jazz:walk-let-macro)
(jazz:define-walker-special     let-symbol          scheme jazz:walk-let-symbol)
(jazz:define-walker-special     receive             scheme jazz:walk-receive)
(jazz:define-walker-special     set!                scheme jazz:walk-setbang)
(jazz:define-walker-special     and                 scheme jazz:walk-and)
(jazz:define-walker-special     or                  scheme jazz:walk-or)
(jazz:define-walker-special     if                  scheme jazz:walk-if)
(jazz:define-walker-special     case                scheme jazz:walk-case)
(jazz:define-walker-special     cond                scheme jazz:walk-cond)
(jazz:define-walker-special     do                  scheme jazz:walk-do)
(jazz:define-walker-special     delay               scheme jazz:walk-delay)
(jazz:define-walker-special     quasiquote          scheme jazz:walk-quasiquote)
(jazz:define-walker-special     unspecific          scheme jazz:walk-unspecific)
(jazz:define-walker-special     reify-reference     scheme jazz:walk-reify-reference))
