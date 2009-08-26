;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Scheme Dialect
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


(module protected scheme.dialect.dialect


;;;
;;;; Define
;;;


(jazz.define-class-runtime jazz.Define-Declaration)


(define (jazz.new-define-declaration name type parent signature)
  (let ((new-declaration (jazz.allocate-define-declaration jazz.Define-Declaration name type #f 'private 'uptodate '() #f parent #f #f signature #f)))
    (jazz.setup-declaration new-declaration)
    new-declaration))


;; Not 100% Scheme clean as it is possible to set! a define at runtime...
(jazz.define-method (jazz.walk-binding-validate-call (jazz.Define-Declaration declaration) walker resume source-declaration operator arguments)
  (let ((signature (%%get-define-declaration-signature declaration)))
    (if signature
        (jazz.validate-arguments walker resume source-declaration declaration signature arguments))))


(jazz.define-method (jazz.emit-declaration (jazz.Define-Declaration declaration) environment)
  (let ((locator (%%get-declaration-locator declaration))
        (value (%%get-define-declaration-value declaration)))
    (jazz.sourcify-if
      `(define ,locator
         ,(jazz.emit-type-cast (jazz.emit-expression value declaration environment) (%%get-lexical-binding-type declaration) declaration environment))
      (%%get-declaration-source declaration))))


(jazz.define-method (jazz.emit-binding-reference (jazz.Define-Declaration declaration) source-declaration environment)
  (jazz.new-code
    (%%get-declaration-locator declaration)
    jazz.Any
    #f))


(jazz.define-method (jazz.walk-binding-validate-assignment (jazz.Define-Declaration declaration) walker resume source-declaration)
  (nextmethod declaration walker resume source-declaration)
  (%%when (%%neq? (%%get-declaration-toplevel declaration) (%%get-declaration-toplevel source-declaration))
    (jazz.walk-error walker resume source-declaration "Illegal inter-module assignment to: {s}" (%%get-lexical-binding-name declaration))))


(jazz.define-method (jazz.walk-binding-assignable? (jazz.Define-Declaration declaration))
  #t)


(jazz.define-method (jazz.emit-binding-assignment (jazz.Define-Declaration declaration) value source-declaration environment)
  (let ((locator (%%get-declaration-locator declaration)))
    (jazz.new-code
      `(set! ,locator ,(jazz.sourcified-form (jazz.emit-expression value source-declaration environment)))
      jazz.Any
      #f)))


(jazz.define-method (jazz.fold-declaration (jazz.Define-Declaration declaration) f k s)
  (f declaration
     (k (jazz.fold-statement (%%get-define-declaration-value declaration) f k s)
        s)))


(jazz.encapsulate-class jazz.Define-Declaration)


;;;
;;;; Define Special Form
;;;


(jazz.define-class-runtime jazz.Define-Special-Form-Declaration)


(define (jazz.new-define-special-form-declaration name type parent signature)
  (let ((new-declaration (jazz.allocate-define-special-form-declaration jazz.Define-Special-Form-Declaration name type #f 'public 'uptodate '() #f parent #f #f signature #f)))
    (jazz.setup-declaration new-declaration)
    new-declaration))


(jazz.define-method (jazz.walk-binding-expandable? (jazz.Define-Special-Form-Declaration declaration))
  #t)


(jazz.define-method (jazz.walk-binding-expand-form (jazz.Define-Special-Form-Declaration binding) walker resume declaration environment form-src)
  (let ((form (%%desourcify form-src)))
    (let ((locator (%%get-declaration-locator binding)))
      (if (%%eq? (%%get-declaration-toplevel binding) (%%get-declaration-toplevel declaration))
          (jazz.walk-error walker resume declaration "Special forms cannot be used from within the same file: {s}" locator)
        (let ((parent-declaration (%%get-declaration-parent binding)))
          (jazz.load-module (%%get-declaration-locator parent-declaration))
          (let ((expander (jazz.need-macro locator)))
            (%%apply expander (%%cdr form))))))))


(jazz.define-method (jazz.emit-declaration (jazz.Define-Special-Form-Declaration declaration) environment)
  (let ((locator (%%get-declaration-locator declaration))
        (signature (%%get-define-special-form-signature declaration))
        (body (%%get-define-special-form-body declaration)))
    (jazz.with-annotated-frame (jazz.annotate-signature signature)
      (lambda (frame)
        (let ((augmented-environment (cons frame environment)))
          (jazz.sourcify-if
            `(jazz.define-special-form ,(%%cons locator (jazz.emit-signature signature declaration augmented-environment))
               ,@(jazz.sourcified-form (jazz.emit-expression body declaration augmented-environment)))
            (%%get-declaration-source declaration)))))))


(jazz.define-method (jazz.fold-declaration (jazz.Define-Special-Form-Declaration declaration) f k s)
  (f declaration
     (k (jazz.fold-statement (%%get-define-special-form-body declaration) f k s)
        s)))


(jazz.encapsulate-class jazz.Define-Special-Form-Declaration)


;;;
;;;; Define Macro
;;;


(jazz.define-class-runtime jazz.Define-Macro-Declaration)


(define (jazz.new-define-macro-declaration name type parent signature)
  (let ((new-declaration (jazz.allocate-define-macro-declaration jazz.Define-Macro-Declaration name type #f 'public 'uptodate '() #f parent #f #f signature #f)))
    (jazz.setup-declaration new-declaration)
    new-declaration))


(jazz.define-method (jazz.walk-binding-expandable? (jazz.Define-Macro-Declaration declaration))
  #t)


(jazz.define-method (jazz.walk-binding-expand-form (jazz.Define-Macro-Declaration binding) walker resume declaration environment form-src)
  (let ((form (%%desourcify form-src)))
    (let ((locator (%%get-declaration-locator binding)))
      (if (%%eq? (%%get-declaration-toplevel binding) (%%get-declaration-toplevel declaration))
          (jazz.walk-error walker resume declaration "Macros cannot be used from within the same file: {s}" locator)
        (let ((parent-declaration (%%get-declaration-parent binding)))
          (jazz.load-module (%%get-declaration-locator parent-declaration))
          (let ((expander (jazz.need-macro locator)))
            (%%apply expander (%%cdr form))))))))


(jazz.define-method (jazz.emit-declaration (jazz.Define-Macro-Declaration declaration) environment)
  (let ((locator (%%get-declaration-locator declaration))
        (signature (%%get-define-macro-signature declaration))
        (body (%%get-define-macro-body declaration)))
    (jazz.with-annotated-frame (jazz.annotate-signature signature)
      (lambda (frame)
        (let ((augmented-environment (%%cons frame environment)))
          (jazz.sourcify-if
            `(jazz.define-macro ,(%%cons locator (jazz.emit-signature signature declaration augmented-environment))
               ,@(jazz.sourcified-form (jazz.emit-expression body declaration augmented-environment)))
            (%%get-declaration-source declaration)))))))


(jazz.define-method (jazz.fold-declaration (jazz.Define-Macro-Declaration declaration) f k s)
  (f declaration
     (k (jazz.fold-statement (%%get-define-macro-body declaration) f k s)
        s)))


(jazz.encapsulate-class jazz.Define-Macro-Declaration)


;;;
;;;; Dialect
;;;


(jazz.define-class-runtime jazz.Scheme-Dialect)


(define (jazz.new-scheme-dialect)
  (jazz.allocate-scheme-dialect jazz.Scheme-Dialect))


(jazz.define-method (jazz.dialect-name (jazz.Scheme-Dialect dialect))
  'scheme)


(jazz.define-method (jazz.dialect-walker (jazz.Scheme-Dialect dialect))
  (jazz.new-scheme-walker))


(jazz.encapsulate-class jazz.Scheme-Dialect)


;;;
;;;; Walker
;;;


(jazz.define-class-runtime jazz.Scheme-Walker)


(define (jazz.new-scheme-walker)
  (jazz.allocate-scheme-walker jazz.Scheme-Walker '() '()))


(jazz.define-method (jazz.runtime-export (jazz.Scheme-Walker walker) declaration)
  (or (nextmethod walker declaration)
      (if (%%is? declaration jazz.Define-Declaration)
          (%%get-declaration-locator declaration)
        #f)))


;;;
;;;; Declaration
;;;


(jazz.define-method (jazz.walk-declaration (jazz.Scheme-Walker walker) resume declaration environment form)
  (if (%%pair? form)
      (let ((first (%%car form)))
        (case first
          ((define)       (jazz.walk-define-declaration walker resume declaration environment form))
          ((define-macro) (jazz.walk-define-macro-declaration walker resume declaration environment form))
          (else           (nextmethod walker resume declaration environment form))))
    #f))


;;;
;;;; Environment
;;;


(define (jazz.scheme-bindings)
  (%%list
    (jazz.new-special-form 'define              jazz.walk-define)
    (jazz.new-special-form 'define-macro        jazz.walk-define-macro)
    (jazz.new-special-form 'define-special-form jazz.walk-define-special-form)
    (jazz.new-special-form 'quote               jazz.walk-quote)
    (jazz.new-special-form 'if                  jazz.walk-if)
    (jazz.new-special-form 'case                jazz.walk-case)
    (jazz.new-special-form 'cond                jazz.walk-cond)
    (jazz.new-special-form 'begin               jazz.walk-begin)
    (jazz.new-special-form 'lambda              jazz.walk-lambda)
    (jazz.new-special-form 'let                 jazz.walk-let)
    (jazz.new-special-form 'let*                jazz.walk-letstar)
    (jazz.new-special-form 'letrec              jazz.walk-letrec)
    (jazz.new-special-form 'let-macro           jazz.walk-let-macro)
    (jazz.new-special-form 'let-symbol          jazz.walk-let-symbol)
    (jazz.new-special-form 'receive             jazz.walk-receive)
    (jazz.new-special-form 'set!                jazz.walk-setbang)
    (jazz.new-special-form 'and                 jazz.walk-and)
    (jazz.new-special-form 'or                  jazz.walk-or)
    (jazz.new-special-form 'do                  jazz.walk-do)
    (jazz.new-special-form 'delay               jazz.walk-delay)
    (jazz.new-special-form 'quasiquote          jazz.walk-quasiquote)))


(define jazz.scheme-environment
  #f)


(jazz.define-method (jazz.walker-environment (jazz.Scheme-Walker walker))
  (or jazz.scheme-environment
      (begin
        (set! jazz.scheme-environment (%%list (jazz.new-walk-frame (append (jazz.core-bindings) (jazz.scheme-bindings)))))
        jazz.scheme-environment)))


;;;
;;;; Define
;;;


(define (jazz.walk-define-declaration walker resume declaration environment form)
  (receive (name specifier value parameters) (jazz.parse-define walker resume declaration (%%cdr form))
    (%%assert (%%class-is? declaration jazz.Namespace-Declaration)
      (let ((type (if specifier (jazz.walk-specifier walker resume declaration environment specifier) jazz.Any))
            (signature (and parameters (jazz.walk-parameters walker resume declaration environment parameters #f #f))))
        (let ((new-declaration (jazz.new-define-declaration name type declaration signature)))
          (let ((effective-declaration (jazz.add-declaration-child walker resume declaration new-declaration)))
            effective-declaration))))))


(define (jazz.walk-define walker resume declaration environment form-src)
  (receive (name specifier value parameters) (jazz.parse-define walker resume declaration (%%cdr (jazz.source-code form-src)))
    (%%assert (%%class-is? declaration jazz.Namespace-Declaration)
      (let* ((new-declaration (jazz.find-form-declaration declaration name))
             (new-environment (%%cons new-declaration environment)))
        (%%set-define-declaration-value new-declaration
          (jazz.walk walker resume new-declaration new-environment value))
        (%%set-declaration-source new-declaration form-src)
        new-declaration))))


;;;
;;;; Define Special Form
;;;


(define (jazz.parse-define-special-form walker resume declaration rest)
  (let* ((signature (%%desourcify (%%car rest)))
         (body (%%cdr rest))
         (name (%%car signature))
         (type jazz.Any)
         (parameters (%%cdr signature)))
    (values name type parameters body)))


(define (jazz.walk-define-special-form-declaration walker resume declaration environment form)
  (receive (name type parameters body) (jazz.parse-define-special-form walker resume declaration (%%cdr form))
    (let ((signature (jazz.walk-parameters walker resume declaration environment parameters #f #f)))
      (let ((new-declaration (jazz.new-define-special-form-declaration name type declaration signature)))
        (let ((effective-declaration (jazz.add-declaration-child walker resume declaration new-declaration)))
          effective-declaration)))))


(define (jazz.walk-define-special-form walker resume declaration environment form-src)
  (receive (name type parameters body) (jazz.parse-define-special-form walker resume declaration (%%cdr (jazz.source-code form-src)))
    (let* ((new-declaration (jazz.find-form-declaration declaration name)))
      (receive (signature augmented-environment) (jazz.walk-parameters walker resume declaration environment parameters #f #t)
        (%%set-define-special-form-signature new-declaration signature)
        (%%set-define-special-form-body new-declaration
          (jazz.walk-body walker resume new-declaration augmented-environment body))
        (%%set-declaration-source new-declaration form-src)
        new-declaration))))


;;;
;;;; Define Macro
;;;


(define (jazz.parse-define-macro walker resume declaration rest)
  (let* ((signature (%%desourcify (%%car rest)))
         (body (%%cdr rest))
         (name (%%car signature))
         (type jazz.Any)
         (parameters (%%cdr signature)))
    (values name type parameters body)))


(define (jazz.walk-define-macro-declaration walker resume declaration environment form)
  (receive (name type parameters body) (jazz.parse-define-macro walker resume declaration (%%cdr form))
    (let ((signature (jazz.walk-parameters walker resume declaration environment parameters #f #f)))
      (let ((new-declaration (jazz.new-define-macro-declaration name type declaration signature)))
        (let ((effective-declaration (jazz.add-declaration-child walker resume declaration new-declaration)))
          effective-declaration)))))


(define (jazz.walk-define-macro walker resume declaration environment form-src)
  (receive (name type parameters body) (jazz.parse-define-macro walker resume declaration (%%cdr (jazz.source-code form-src)))
    (let* ((new-declaration (jazz.find-form-declaration declaration name)))
      (receive (signature augmented-environment) (jazz.walk-parameters walker resume declaration environment parameters #f #t)
        (%%set-define-macro-signature new-declaration signature)
        (%%set-define-macro-body new-declaration
          (jazz.walk-body walker resume new-declaration augmented-environment body))
        (%%set-declaration-source new-declaration form-src)
        new-declaration))))


;;;
;;;; Let Macro
;;;


(define (jazz.walk-let-macro walker resume declaration environment form-src)
  (let ((form (%%desourcify form-src)))
    (let* ((bindings (%%cadr form))
           (body (%%cddr form))
           (macro-forms (map (lambda (binding)
                               (let ((name (%%car binding))
                                     (expander (%%cadr binding)))
                                 (jazz.new-macro-form name (eval expander))))
                             bindings))
           (new-environment (%%append macro-forms environment)))
      `(begin
         ,@(jazz.walk-body walker resume declaration new-environment body)))))


;;;
;;;; Let Symbol
;;;


(define (jazz.walk-let-symbol walker resume declaration environment form-src)
  (let ((form (%%desourcify form-src)))
    (let* ((bindings (%%cadr form))
           (body (%%cddr form))
           (macro-symbols (map (lambda (binding)
                                 (let ((name (%%car binding))
                                       (getter (%%cadr binding))
                                       (setter (%%car (%%cddr binding))))
                                   (jazz.new-macro-symbol name (eval getter) (eval setter))))
                               bindings))
           (new-environment (%%append macro-symbols environment)))
      `(begin
         ,@(jazz.walk-body walker resume declaration new-environment body)))))


;;;
;;;; Lambda
;;;


(define (jazz.walk-lambda walker resume declaration environment form-src)
  (let ((parameters (%%desourcify (%%cadr (jazz.source-code form-src)))))
    (jazz.parse-specifier (%%cddr (jazz.source-code form-src))
      (lambda (specifier body)
        (receive (signature augmented-environment) (jazz.walk-parameters walker resume declaration environment parameters #t #t)
          (let ((type (if specifier (jazz.walk-specifier walker resume declaration environment specifier) jazz.Any)))
            (jazz.new-lambda type form-src signature
              (jazz.walk-body walker resume declaration augmented-environment body))))))))


;;;
;;;; Binding
;;;


(define (jazz.parse-binding walker resume declaration environment form-src)
  (%%assertion (and (%%pair? (jazz.source-code form-src)) (%%pair? (%%cdr (jazz.source-code form-src)))) (jazz.error "Ill-formed binding: {s}" (%%desourcify form-src))
    (let ((symbol (jazz.source-code (%%car (jazz.source-code form-src)))))
      (jazz.parse-specifier (%%cdr (jazz.source-code form-src))
        (lambda (specifier rest)
          (let ((type (if specifier (jazz.walk-specifier walker resume declaration environment specifier) #f))
                (value (%%car rest)))
            (values (jazz.new-variable symbol type) value)))))))


(define (jazz.walk-let walker resume declaration environment form-src)
  (if (%%symbol? (jazz.source-code (%%cadr (jazz.source-code form-src))))
      (jazz.walk-named-let walker resume declaration environment form-src)
    (let ((bindings (jazz.source-code (%%cadr (jazz.source-code form-src))))
          (body (%%cddr (jazz.source-code form-src))))
      (if (and (%%pair? bindings) (%%symbol? (jazz.source-code (%%car bindings))))
          (jazz.signature-named-let walker resume declaration environment form-src bindings body)
        (let ((effective-body (if (%%null? body) (%%list (%%list 'unspecified)) body)))
          (let ((augmented-environment environment)
                (expanded-bindings (jazz.new-queue)))
            (for-each (lambda (binding-form)
                        (continuation-capture
                          (lambda (resume)
                            (receive (variable value) (jazz.parse-binding walker resume declaration environment binding-form)
                              (jazz.enqueue expanded-bindings (%%cons variable (jazz.walk walker resume declaration environment value)))
                              (set! augmented-environment (%%cons variable augmented-environment))))))
                      bindings)
            (jazz.new-let form-src
                          (jazz.queue-list expanded-bindings)
                          (jazz.walk-body walker resume declaration augmented-environment effective-body))))))))


(define (jazz.walk-letstar walker resume declaration environment form-src)
  (let ((bindings (jazz.source-code (%%cadr (jazz.source-code form-src))))
        (body (%%cddr (jazz.source-code form-src))))
    (let ((augmented-environment environment)
          (expanded-bindings (jazz.new-queue)))
      (for-each (lambda (binding-form)
                  (continuation-capture
                    (lambda (resume)
                      (receive (variable value) (jazz.parse-binding walker resume declaration environment binding-form)
                        (jazz.enqueue expanded-bindings (%%cons variable (jazz.walk walker resume declaration augmented-environment value)))
                        (set! augmented-environment (%%cons variable augmented-environment))))))
                bindings)
      (jazz.new-letstar form-src
                        (jazz.queue-list expanded-bindings)
                        (jazz.walk-body walker resume declaration augmented-environment body)))))


(define (jazz.walk-letrec walker resume declaration environment form-src)
  (let ((bindings (jazz.source-code (%%cadr (jazz.source-code form-src))))
        (body (%%cddr (jazz.source-code form-src))))
    (let* ((new-variables (map (lambda (binding-form) (jazz.new-variable (jazz.source-code (%%car (jazz.source-code binding-form))) #f)) bindings))
           (augmented-environment (%%append new-variables environment))
           (expanded-bindings (jazz.new-queue)))
      (for-each (lambda (variable binding-form)
                  (continuation-capture
                    (lambda (resume)
                      (let ((value (%%cadr (jazz.source-code binding-form))))
                        (jazz.enqueue expanded-bindings (%%cons variable (jazz.walk walker resume declaration augmented-environment value)))))))
                new-variables
                bindings)
      (jazz.new-letrec form-src
                       (jazz.queue-list expanded-bindings)
                       (jazz.walk-body walker resume declaration augmented-environment body)))))


(define (jazz.walk-receive walker resume declaration environment form-src)
  (define (walk-parameters parameters)
    (let ((queue (jazz.new-queue)))
      (let iter ((scan parameters))
        (if (%%null? scan)
            (jazz.queue-list queue)
          (let ((expr (%%car scan)))
            (jazz.parse-specifier (%%cdr scan)
              (lambda (specifier rest)
                (let ((type (if specifier (jazz.walk-specifier walker resume declaration environment specifier) jazz.Any)))
                  (jazz.enqueue queue (jazz.new-variable expr type))
                  (iter rest)))))))))
  
  (let* ((parameters (%%desourcify (%%cadr (jazz.source-code form-src))))
         (expression (%%car (%%cddr (jazz.source-code form-src))))
         (body (%%cdr (%%cddr (jazz.source-code form-src))))
         (variables (walk-parameters parameters))
         (new-environment (%%append variables environment)))
    (jazz.new-receive form-src variables (continuation-capture
                                           (lambda (resume)
                                             (jazz.walk walker resume declaration environment expression)))
      (jazz.walk-body walker resume declaration new-environment body))))


;;;
;;;; Control
;;;


(define (jazz.walk-if walker resume declaration environment form-src)
  (if (%%fx< (%%length (jazz.source-code form-src)) 3)
      (jazz.walk-error walker resume declaration "Ill-formed if: {s}" (%%desourcify form-src))
    (let ((test (%%cadr (jazz.source-code form-src)))
          (yes (%%car (%%cddr (jazz.source-code form-src))))
          (no (%%cdr (%%cddr (jazz.source-code form-src)))))
      (jazz.new-if form-src
                   (continuation-capture
                     (lambda (resume)
                       (jazz.walk walker resume declaration environment test)))
                   (continuation-capture
                     (lambda (resume)
                       (jazz.walk walker resume declaration environment yes)))
                   (jazz.walk walker resume declaration environment
                     (%%cons 'begin
                             (if (%%null? no)
                                 '((unspecified))
                               no)))))))


(define (jazz.walk-cond walker resume declaration environment form-src)
  (let ((clauses (%%cdr (jazz.source-code form-src))))
    (let ((expanded-clauses (jazz.new-queue)))
      (for-each (lambda (clause)
                  (%%assertion (%%pair? (jazz.source-code clause)) (jazz.error "Ill-formed cond clause: {s}" (%%desourcify clause))
                    (let ((test (%%car (jazz.source-code clause)))
                          (body (%%cdr (jazz.source-code clause))))
                      (jazz.enqueue expanded-clauses
                        (%%cons (if (%%eq? (jazz.source-code test) 'else)
                                    #f
                                  (continuation-capture
                                    (lambda (resume)
                                      (jazz.walk walker resume declaration environment test))))
                                (jazz.walk-implicit-begin walker resume declaration environment clause body))))))
                clauses)
      (jazz.new-cond form-src (jazz.queue-list expanded-clauses)))))


(define (jazz.walk-case walker resume declaration environment form-src)
  (let ((target (%%cadr (jazz.source-code form-src)))
        (clauses (%%cddr (jazz.source-code form-src))))
    (jazz.new-case form-src
                   (continuation-capture
                     (lambda (resume)
                       (jazz.walk walker resume declaration environment target)))
                   (map (lambda (clause)
                          (%%assertion (%%pair? (jazz.source-code clause)) (jazz.error "Ill-formed case clause: {s}" (%%desourcify clause))
                            (continuation-capture
                              (lambda (resume)
                                (let* ((tries (%%desourcify (%%car (jazz.source-code clause))))
                                       (body (%%cdr (jazz.source-code clause)))
                                       (effective-body (if (%%null? body) (%%list (%%list 'unspecified)) body)))
                                  (if (or (%%eq? tries 'else) (%%pair? tries))
                                      (%%cons tries (jazz.walk-implicit-begin walker resume declaration environment clause effective-body))
                                    (jazz.walk-error walker resume declaration "Ill-formed selector list: {s}" tries)))))))
                        clauses))))


(define (jazz.walk-and walker resume declaration environment form-src)
  (jazz.new-and form-src (jazz.walk-list walker resume declaration environment (%%cdr (jazz.source-code form-src)))))


(define (jazz.walk-or walker resume declaration environment form-src)
  (jazz.new-or form-src (jazz.walk-list walker resume declaration environment (%%cdr (jazz.source-code form-src)))))


;;;
;;;; Sequencing
;;;


(define (jazz.walk-begin walker resume declaration environment form-src)
  (let ((body (%%cdr (jazz.source-code form-src))))
    (jazz.new-begin form-src (jazz.walk-list walker resume declaration environment body))))


(define (jazz.walk-implicit-begin walker resume declaration environment form-src form-list)
  (if (%%null? form-list)
      (jazz.walk walker resume declaration environment '(unspecified))
    (jazz.new-begin form-src (jazz.walk-list walker resume declaration environment form-list))))


;;;
;;;; Iteration
;;;


(define (jazz.walk-do walker resume declaration environment form-src)
  (let ((bindings (jazz.source-code (%%cadr (jazz.source-code form-src))))
        (test (%%car (jazz.source-code (%%car (%%cddr (jazz.source-code form-src))))))
        (result (%%cdr (jazz.source-code (%%car (%%cddr (jazz.source-code form-src))))))
        (body (%%cdr (%%cddr (jazz.source-code form-src)))))
    (let* ((new-variables (map (lambda (binding-form) (jazz.new-variable (jazz.source-code (%%car (jazz.source-code binding-form))) #f)) bindings))
           (augmented-environment (%%append new-variables environment))
           (expanded-bindings (jazz.new-queue)))
      (for-each (lambda (variable binding-form)
                  (continuation-capture
                    (lambda (resume)
                      (let ((init (jazz.walk walker resume declaration augmented-environment (%%cadr (jazz.source-code binding-form))))
                            (step (if (%%null? (%%cddr (jazz.source-code binding-form))) #f (jazz.walk walker resume declaration augmented-environment (%%car (%%cddr (jazz.source-code binding-form)))))))
                        (jazz.enqueue expanded-bindings (%%cons variable (%%cons init step)))))))
                new-variables
                bindings)
      (jazz.new-do (jazz.queue-list expanded-bindings)
                   (continuation-capture
                     (lambda (resume)
                       (jazz.walk walker resume declaration augmented-environment test)))
                   (continuation-capture
                     (lambda (resume)
                       (jazz.walk-body walker resume declaration augmented-environment result)))
                   (jazz.walk-body walker resume declaration augmented-environment body)))))


(define (jazz.walk-named-let walker resume declaration environment form-src)
  (let ((name (jazz.source-code (%%cadr (jazz.source-code form-src))))
        (bindings (%%desourcify (%%car (%%cddr (jazz.source-code form-src)))))
        (body (%%cdr (%%cddr (jazz.source-code form-src)))))
    (let ((augmented-environment environment)
          (expanded-bindings (jazz.new-queue)))
      (for-each (lambda (binding-form)
                  (continuation-capture
                    (lambda (resume)
                      (receive (variable value) (jazz.parse-binding walker resume declaration environment binding-form)
                        (jazz.enqueue expanded-bindings (%%cons variable (jazz.walk walker resume declaration environment value)))
                        (set! augmented-environment (%%cons variable augmented-environment))))))
                bindings)
      (let ((variable (jazz.new-variable name #f)))
        (set! augmented-environment (%%cons variable augmented-environment))
        (jazz.new-named-let form-src variable
          (jazz.queue-list expanded-bindings)
          (jazz.walk-body walker resume declaration augmented-environment body))))))


(define (jazz.signature-named-let walker resume declaration environment form-src bindings body)
  (let ((name (jazz.source-code (%%car bindings)))
        (bindings (%%cdr bindings)))
    (jazz.walk-named-let walker resume declaration environment
      (jazz.sourcify-if
        `(let ,name ,bindings
           ,@body)
        form-src))))


;;;
;;;; Delayed Evaluation
;;;


(define (jazz.walk-delay walker resume declaration environment form-src)
  (let ((expression (%%cadr (jazz.source-code form-src))))
    (jazz.new-delay (jazz.walk walker resume declaration environment expression))))


;;;
;;;; Quasiquotation
;;;


(define (jazz.walk-quasiquote walker resume declaration environment form-src)
  (letrec ((walk
             (lambda (form-src)
               (if (%%pair? (jazz.source-code form-src))
                   (let ((first (jazz.source-code (%%car (jazz.source-code form-src)))))
                     (if (or (%%eq? first 'unquote)
                             (%%eq? first 'unquote-splicing))
                         (%%list first (jazz.walk walker resume declaration environment (%%cadr (jazz.source-code form-src))))
                       (%%cons (walk (%%car (jazz.source-code form-src))) (walk (%%cdr (jazz.source-code form-src))))))
                 form-src))))
    (jazz.new-quasiquote (walk (%%cadr (jazz.source-code form-src))))))


(jazz.encapsulate-class jazz.Scheme-Walker)


;;;
;;;; Register
;;;


(jazz.register-dialect 'scheme (jazz.new-scheme-dialect)))
