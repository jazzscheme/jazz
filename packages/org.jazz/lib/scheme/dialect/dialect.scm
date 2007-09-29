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
;;;  Portions created by the Initial Developer are Copyright (C) 1996-2006
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


(module scheme.dialect.dialect


;;;
;;;; Define
;;;


(jazz.define-class jazz.Define-Declaration jazz.Declaration (name type access compatibility attributes toplevel parent children locator) jazz.Object-Class
  (signature))


(define (jazz.new-define-declaration name type parent parameters)
  (let ((new-declaration (jazz.allocate-define-declaration jazz.Define-Declaration name type 'public 'uptodate '() #f parent '() #f (and parameters (jazz.new-walk-signature parameters)))))
    (jazz.setup-declaration new-declaration)
    new-declaration))


(jazz.define-method (jazz.walk-binding-walk-reference (jazz.Define-Declaration declaration) walker resume source-declaration environment)
  (%%get-declaration-locator declaration))


(jazz.define-method (jazz.walk-binding-walk-assignment (jazz.Define-Declaration declaration) walker resume source-declaration environment value)
  (let ((locator (%%get-declaration-locator declaration))
        (walk (jazz.walk walker resume source-declaration environment value)))
    `(set! ,locator ,walk)))


;; Not 100% Scheme clean as it is possible to set! a define at runtime...
(jazz.define-method (jazz.walk-binding-validate-call (jazz.Define-Declaration declaration) walker resume source-declaration operator arguments)
  (let ((signature (%%get-define-declaration-signature declaration)))
    (if signature
        (jazz.validate-arguments walker resume source-declaration declaration signature arguments))))


(jazz.encapsulate-class jazz.Define-Declaration)


;;;
;;;; Define Macro
;;;


(jazz.define-class jazz.Define-Macro-Declaration jazz.Declaration (name type access compatibility attributes toplevel parent children locator) jazz.Object-Class
  ())


(define (jazz.new-define-macro-declaration name type parent)
  (let ((new-declaration (jazz.allocate-define-macro-declaration jazz.Define-Macro-Declaration name type 'public 'uptodate '() #f parent '() #f)))
    (jazz.setup-declaration new-declaration)
    new-declaration))


(jazz.define-method (jazz.walk-binding-expandable? (jazz.Define-Macro-Declaration declaration))
  #t)


(jazz.define-method (jazz.walk-binding-expand-form (jazz.Define-Macro-Declaration binding) walker resume declaration environment form)
  (let ((locator (%%get-declaration-locator binding)))
    (if (%%eq? (%%get-declaration-toplevel binding) (%%get-declaration-toplevel declaration))
        (jazz.walk-error walker resume declaration "Macros cannot be used from within the same file: {s}" locator)
      (let ((parent-declaration (%%get-declaration-parent binding)))
        (jazz.load-module (%%get-declaration-locator parent-declaration))
        (let ((expander (jazz.need-macro locator)))
          (%%apply expander (%%cdr form)))))))


(jazz.define-method (jazz.walk-binding-walk-assignment (jazz.Define-Macro-Declaration declaration) walker resume source-declaration environment value)
  (jazz.walk-error walker resume source-declaration "Illegal assignment to a macro: {s}" (%%get-declaration-locator declaration)))


(jazz.encapsulate-class jazz.Define-Macro-Declaration)


;;;
;;;; Dialect
;;;


(jazz.define-class jazz.Scheme-Dialect jazz.Dialect () jazz.Object-Class
  ())


(define (jazz.new-scheme-dialect)
  (jazz.allocate-scheme-dialect jazz.Scheme-Dialect))


(jazz.define-method (jazz.dialect-walker (jazz.Scheme-Dialect dialect))
  (jazz.new-scheme-walker))


(jazz.encapsulate-class jazz.Scheme-Dialect)


;;;
;;;; Walker
;;;


(jazz.define-class jazz.Scheme-Walker jazz.Walker (warnings errors literals variables references autoloads) jazz.Object-Class
  ())


(define (jazz.new-scheme-walker)
  (jazz.allocate-scheme-walker jazz.Scheme-Walker '() '() '() '() '() '()))


;;;
;;;; Declaration
;;;


(jazz.define-method (jazz.walk-declaration (jazz.Scheme-Walker walker) resume declaration environment form)
  (if (%%pair? form)
      (let ((first (%%car form)))
        (case first
          ((%define)       (jazz.walk-%define-declaration walker resume declaration environment form))
          ((%define-macro) (jazz.walk-%define-macro-declaration walker resume declaration environment form))
          (else            (nextmethod walker resume declaration environment form))))
    #f))


;;;
;;;; Environment
;;;


(define (jazz.scheme-bindings)
  (%%list
    (jazz.new-macro-form 'define         jazz.expand-define)       (jazz.new-special-form '%define       jazz.walk-%define)
    (jazz.new-macro-form 'define-macro   jazz.expand-define-macro) (jazz.new-special-form '%define-macro jazz.walk-%define-macro)
    
    (jazz.new-special-form 'quote        jazz.walk-quote)
    (jazz.new-special-form 'if           jazz.walk-if)
    (jazz.new-special-form 'case         jazz.walk-case)
    (jazz.new-special-form 'cond         jazz.walk-cond)
    (jazz.new-special-form 'begin        jazz.walk-begin)
    (jazz.new-special-form 'lambda       jazz.walk-lambda)
    (jazz.new-special-form 'let          jazz.walk-let)
    (jazz.new-special-form 'let*         jazz.walk-letstar)
    (jazz.new-special-form 'letrec       jazz.walk-letrec)
    (jazz.new-special-form 'let-macro    jazz.walk-let-macro)
    (jazz.new-special-form 'let-symbol   jazz.walk-let-symbol)
    (jazz.new-special-form 'receive      jazz.walk-receive)
    (jazz.new-special-form 'set!         jazz.walk-setbang)
    (jazz.new-special-form 'and          jazz.walk-and)
    (jazz.new-special-form 'or           jazz.walk-or)
    (jazz.new-special-form 'do           jazz.walk-do)
    (jazz.new-special-form 'delay        jazz.walk-delay)
    (jazz.new-special-form 'quasiquote   jazz.walk-quasiquote)))


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


(define (jazz.expand-define walker resume declaration environment . rest)
  (jazz.expand-define-form walker resume declaration (%%cons 'define rest)))


(define (jazz.expand-define-form walker resume declaration form)
  (receive (name type value parameters) (jazz.parse-define walker resume declaration (%%cdr form))
    `(%define ,name ,type ,value ,parameters)))


(define (jazz.walk-%define-declaration walker resume declaration environment form)
  (jazz.bind (name type value parameters) (%%cdr form)
    (%%assert (%%is? declaration jazz.Namespace-Declaration)
      (let ((new-declaration (jazz.new-define-declaration name type declaration parameters)))
        (jazz.add-declaration-child walker resume declaration new-declaration)
        new-declaration))))


(define (jazz.walk-%define walker resume declaration environment form)
  (jazz.bind (name type value parameters) (%%cdr form)
    (let* ((new-declaration (jazz.find-form-declaration declaration form))
           (locator (%%get-declaration-locator new-declaration))
           (new-environment (%%cons new-declaration environment)))
      `(define ,locator
         ,(jazz.generate walker resume new-declaration new-environment value)))))


;;;
;;;; Define Macro
;;;


(define (jazz.parse-define-macro walker resume declaration rest)
  (let* ((signature (%%car rest))
         (body (%%cdr rest))
         (name (%%car signature))
         (type #f)
         (parameters (%%cdr signature)))
    (values name type parameters body)))


(define (jazz.expand-define-macro walker resume declaration environment . rest)
  (jazz.expand-define-macro-form walker resume declaration (%%cons 'define-macro rest)))


(define (jazz.expand-define-macro-form walker resume declaration form)
  (receive (name type parameters body) (jazz.parse-define-macro walker resume declaration (%%cdr form))
    `(%define-macro ,name ,type ,parameters ,body)))


(define (jazz.walk-%define-macro-declaration walker resume declaration environment form)
  (jazz.bind (name type parameters body) (%%cdr form)
    (let ((new-declaration (jazz.new-define-macro-declaration name type declaration)))
      (jazz.add-declaration-child walker resume declaration new-declaration)
      new-declaration)))


(define (jazz.walk-%define-macro walker resume declaration environment form)
  (jazz.bind (name type parameters body) (%%cdr form)
    (let* ((new-declaration (jazz.find-form-declaration declaration form))
           (locator (%%get-declaration-locator new-declaration))
           (new-variables (jazz.parameters->variables parameters))
           (new-environment (%%append new-variables environment)))
      `(jazz.define-macro ,(%%cons locator parameters)
         ,@(jazz.walk-body walker resume new-declaration new-environment body)))))


;;;
;;;; Let Macro
;;;


(define (jazz.walk-let-macro walker resume declaration environment form)
  (let* ((bindings (%%cadr form))
         (body (%%cddr form))
         (macro-forms (map (lambda (binding)
                             (let ((name (%%car binding))
                                   (expander (%%cadr binding)))
                               (jazz.new-macro-form name (eval expander))))
                           bindings))
         (new-environment (%%append macro-forms environment)))
    `(begin
       ,@(jazz.walk-body walker resume declaration new-environment body))))


;;;
;;;; Let Symbol
;;;


(define (jazz.walk-let-symbol walker resume declaration environment form)
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
       ,@(jazz.walk-body walker resume declaration new-environment body))))


;;;
;;;; Parameters
;;;


(define (jazz.walk-parameter-list walker resume declaration environment parameters defaults?)
  (let ((section 'positional)
        (parameter-list (jazz.new-queue))
        (augmented-environment environment))
    (let iter ((scan parameters))
      (cond ((%%null? scan))
            ((%%symbol? scan)
             (jazz.enqueue parameter-list #!rest)
             (jazz.enqueue parameter-list scan)
             (set! augmented-environment (%%cons (jazz.new-variable scan #f) augmented-environment)))
            (else
             (let ((parameter (%%car scan)))
               (cond ((%%symbol? parameter)
                      (if (%%eq? section 'positional)
                          ;; ignore type specifiers for now
                          (if (%%not (jazz.specifier? parameter))
                              (begin
                                (jazz.enqueue parameter-list parameter)
                                (set! augmented-environment (%%cons (jazz.new-variable parameter #f) augmented-environment))))
                        (jazz.walk-error walker resume declaration "Ill-formed lambda parameters: {s}" parameters)))
                     ((%%pair? parameter)
                      (cond ((or (jazz.specifier? (%%car parameter))
                                 ;; quicky support for autoload expression
                                 (%%pair? (%%car parameter)))
                             (if (%%eq? section 'positional)
                                 (begin
                                   (jazz.enqueue parameter-list parameter)
                                   (set! augmented-environment (%%cons (jazz.new-variable (%%cadr parameter) #f) augmented-environment)))
                               (jazz.walk-error walker resume declaration "Ill-formed lambda parameters: {s}" parameters)))
                            ((%%keyword? (%%car parameter))
                             (let ((keyword (%%car parameter))
                                   (variable (%%cadr parameter))
                                   (default (%%car (%%cddr parameter))))
                               (if (%%eq? section 'positional)
                                   (begin
                                     (jazz.enqueue parameter-list #!key)
                                     (set! section 'named)))
                               (if (%%eq? (%%string->symbol (%%keyword->string keyword)) variable)
                                   (begin
                                     (jazz.enqueue parameter-list (%%list variable (if defaults? (jazz.walk walker resume declaration augmented-environment default) #f)))
                                     (set! augmented-environment (%%cons (jazz.new-variable variable #f) augmented-environment)))
                                 (jazz.walk-error walker resume declaration "Keyword parameter key and name must match: {s}" parameter))))
                            (else
                             (let ((variable (%%car parameter))
                                   (default (%%cadr parameter)))
                               (if (or (%%eq? section 'positional) (%%eq? section 'named))
                                   (begin
                                     (jazz.enqueue parameter-list #!optional)
                                     (set! section 'optional)))
                               (jazz.enqueue parameter-list (%%list variable (if defaults? (jazz.walk walker resume declaration augmented-environment default) #f)))
                               (set! augmented-environment (%%cons (jazz.new-variable variable #f) augmented-environment))))))
                     (else
                      (jazz.walk-error walker resume declaration "Ill-formed lambda parameter: {s}" parameter))))
             (iter (%%cdr scan)))))
    (values (jazz.queue-list parameter-list)
            augmented-environment)))


;;;
;;;; Lambda
;;;


(define (jazz.walk-lambda walker resume declaration environment form)
  (let ((parameters (%%cadr form))
        (body (%%cddr form)))
    (receive (parameter-list augmented-environment) (jazz.walk-parameter-list walker resume declaration environment parameters #t)
      (jazz.new-lambda parameter-list
        (jazz.walk-body walker resume declaration augmented-environment body)))))


(jazz.define-virtual (jazz.walk-parameters (jazz.Scheme-Walker walker) parameters))


(jazz.define-method (jazz.walk-parameters (jazz.Scheme-Walker walker) parameters)
  parameters)


;;;
;;;; Type
;;;


;; TO THINK
;; <fx> -> fixnum
;; <fl> -> flonum
;; <integer> -> integer
;; <rational> -> rational
;; <real> -> real
;; <complex> -> complex
;; <string> -> string
;; <Object>
;; <Point>
;; <Point+> ;; pas sur
;; #f ou bien <any> ou bien ???
;; keep <int> ... for interfaces to java, c, ... !?
;; <void> !?


(define jazz.primitive-types
  (%%make-hashtable eq?))


(define (jazz.specifier->type specifier)
  (let ((name (jazz.specifier->name specifier)))
    name))


;;;
;;;; Binding
;;;


(define (jazz.parse-binding walker resume declaration form)
  (%%assertion (and (%%pair? form) (%%pair? (%%cdr form))) (jazz.format "Ill-formed binding: {s}" form)
    (let ((symbol (%%car form))
          (second (%%cadr form)))
      (if (jazz.specifier? second)
          (values (jazz.new-variable symbol (jazz.specifier->type second)) (%%car (%%cddr form)))
        (values (jazz.new-variable symbol #f) second)))))


(define (jazz.walk-let walker resume declaration environment form)
  (if (%%symbol? (%%cadr form))
      (jazz.walk-named-let walker resume declaration environment form)
    (let ((bindings (%%cadr form))
          (body (%%cddr form)))
      (if (and (%%pair? bindings) (%%symbol? (%%car bindings)))
          (jazz.walk-signature-named-let walker resume declaration environment bindings body)
        (let ((effective-body (if (%%null? body) (%%list (%%list 'void)) body)))
          (let ((augmented-environment environment)
                (expanded-bindings (jazz.new-queue)))
            (for-each (lambda (binding-form)
                        (receive (variable value) (jazz.parse-binding walker resume declaration (jazz.remove-specifiers-quicky binding-form))
                          (jazz.enqueue expanded-bindings (%%cons variable (jazz.walk walker resume declaration environment value)))
                          (set! augmented-environment (%%cons variable augmented-environment))))
                      bindings)
            (jazz.new-let (map (lambda (expanded-binding)
                                 (let ((variable (%%car expanded-binding))
                                       (value (%%cdr expanded-binding)))
                                   (cons variable value)))
                               (jazz.queue-list expanded-bindings))
                          (jazz.walk-body walker resume declaration augmented-environment effective-body))))))))


(define (jazz.walk-letstar walker resume declaration environment form)
  (let ((bindings (%%cadr form))
        (body (%%cddr form)))
    (let ((augmented-environment environment)
          (expanded-bindings (jazz.new-queue)))
      (for-each (lambda (binding-form)
                  (receive (variable value) (jazz.parse-binding walker resume declaration (jazz.remove-specifiers-quicky binding-form))
                    (jazz.enqueue expanded-bindings (%%cons variable (jazz.walk walker resume declaration augmented-environment value)))
                    (set! augmented-environment (%%cons variable augmented-environment))))
                bindings)
    `(let* ,(map (lambda (expanded-binding)
                   (let ((variable (%%car expanded-binding))
                         (value (%%cdr expanded-binding)))
                     `(,(%%get-lexical-binding-name variable) ,value)))
                 (jazz.queue-list expanded-bindings))
       ,@(jazz.walk-body walker resume declaration augmented-environment body)))))


(define (jazz.walk-letrec walker resume declaration environment form)
  (let ((bindings (%%cadr form))
        (body (%%cddr form)))
    (let* ((new-variables (map (lambda (binding-form) (jazz.new-variable (%%car binding-form) #f)) bindings))
           (augmented-environment (%%append new-variables environment)))
      `(letrec ,(map (lambda (binding-form)
                       (let ((variable (%%car binding-form))
                             (value (%%cadr binding-form)))
                         `(,variable ,(jazz.walk walker resume declaration augmented-environment value))))
                     bindings)
         ,@(jazz.walk-body walker resume declaration augmented-environment body)))))


(define (jazz.walk-receive walker resume declaration environment form)
  (let* ((parameters (jazz.remove-specifiers-quicky (%%cadr form)))
         (values (%%car (%%cddr form)))
         (body (%%cdr (%%cddr form)))
         (variables (map (lambda (parameter)
                           (jazz.new-variable parameter #f))
                         parameters))
         (new-environment (%%append variables environment)))
    `(receive ,parameters ,(jazz.walk walker resume declaration environment values)
       ,@(jazz.walk-body walker resume declaration new-environment body))))


;;;
;;;; Control
;;;


(define (jazz.walk-if walker resume declaration environment form)
  (let ((test (%%cadr form))
        (yes (%%car (%%cddr form)))
        (no (%%cdr (%%cddr form))))
    (jazz.new-if (jazz.walk walker resume declaration environment test)
                 (jazz.walk walker resume declaration environment yes)
                 (jazz.walk-list walker resume declaration environment
                   (if (%%null? no)
                       '((void))
                     no)))))


(define (jazz.walk-cond walker resume declaration environment form)
  (let ((clauses (%%cdr form)))
    (jazz.new-cond (map (lambda (clause)
                          (let ((test (%%car clause))
                                (body (%%cdr clause)))
                            (cons (if (%%eq? test 'else)
                                      #f
                                    (jazz.walk walker resume declaration environment test))
                                  (jazz.walk-list walker resume declaration environment body))))
                        clauses))))


(define (jazz.walk-case walker resume declaration environment form)
  (let ((target (%%cadr form))
        (clauses (%%cddr form)))
    (jazz.new-case (jazz.walk walker resume declaration environment target)
                   (map (lambda (clause)
                          (let* ((tries (%%car clause))
                                 (body (%%cdr clause))
                                 (effective-body (if (%%null? body) (%%list (%%list 'void)) body)))
                            (if (or (%%eq? tries 'else) (list? tries))
                                (cons tries (jazz.walk-list walker resume declaration environment effective-body))
                              (jazz.walk-error walker resume declaration "Ill-formed selector list: {s}" tries))))
                        clauses))))


(define (jazz.walk-and walker resume declaration environment form)
  (jazz.new-and (jazz.walk-list walker resume declaration environment expressions)))


(define (jazz.walk-or walker resume declaration environment form)
  (jazz.new-or (jazz.walk-list walker resume declaration environment expressions)))


;;;
;;;; Sequencing
;;;


(define (jazz.walk-begin walker resume declaration environment form)
  (let ((body (%%cdr form)))
    `(begin ,@(jazz.walk-list walker resume declaration environment body))))


;;;
;;;; Iteration
;;;


(define (jazz.walk-do walker resume declaration environment form)
  (jazz.unimplemented 'jazz.walk-do))


(define (jazz.walk-named-let walker resume declaration environment form)
  (let ((name (%%cadr form))
        (bindings (%%car (%%cddr form)))
        (body (%%cdr (%%cddr form))))
    (let ((augmented-environment environment)
          (expanded-bindings (jazz.new-queue)))
      (for-each (lambda (binding-form)
                  (let ((variable (jazz.new-variable (%%car binding-form) #f))
                        (value (%%cadr binding-form)))
                    (jazz.enqueue expanded-bindings (%%cons variable (jazz.walk walker resume declaration environment value)))
                    (set! augmented-environment (%%cons variable augmented-environment))))
                bindings)
      (set! augmented-environment (%%cons (jazz.new-variable name #f) augmented-environment))
      `(let ,name ,(map (lambda (expanded-binding)
                          (let ((variable (%%car expanded-binding))
                                (value (%%cdr expanded-binding)))
                            `(,(%%get-lexical-binding-name variable) ,value)))
                        (jazz.queue-list expanded-bindings))
         ,@(jazz.walk-body walker resume declaration augmented-environment body)))))


(define (jazz.walk-signature-named-let walker resume declaration environment bindings body)
  (let ((name (%%car bindings))
        (bindings (%%cdr bindings)))
    (jazz.walk-named-let walker resume declaration environment
      `(let ,name ,bindings
         ,@body))))


;;;
;;;; Delayed Evaluation
;;;


(define (jazz.walk-delay walker resume declaration environment form)
  (jazz.unimplemented 'jazz.walk-delay))


;;;
;;;; Quasiquotation
;;;


(define (jazz.walk-quasiquote walker resume declaration environment form)
  (letrec ((walk
            (lambda (form)
              (if (%%pair? form)
                  (if (or (%%eq? (%%car form) 'unquote)
                          (%%eq? (%%car form) 'unquote-splicing))
                      (%%list (%%car form) (jazz.walk walker resume declaration environment (%%cadr form)))
                    (%%cons (walk (%%car form)) (walk (%%cdr form))))
                form))))
    (%%list (%%car form) (walk (%%cadr form)))))


(jazz.encapsulate-class jazz.Scheme-Walker)


;;;
;;;; Register
;;;


(jazz.register-dialect 'scheme (jazz.new-scheme-dialect)))
