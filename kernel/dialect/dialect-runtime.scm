;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Dialect Runtime
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


;; Concepts
;; - lookup: direct lookup of a symbol returning a declaration
;; - resolve: references are resolved only on first usage
;; - expand take a source code expr and expand it to equivalent scheme code
;;   - walk: take a source code expr and generate its expression tree
;;   - emit: take an expression tree and emit implementation scheme code
;;
;; Notes
;; - It is key to have compile time reference loading identical to runtime module loading
;;   to ensure that runtime problems are detected at compile time exactly as they would occur
;; - Autoload declarations are treated specially as they are the only case where different
;;   references to the same declaration generate different code: direct access will expand to
;;   just the locator while an autoload access must add code to load the unit
;;
;; Todo
;; - Is the extra indirection level of having declaration references really necessary?
;; - Convert and remove the temporary patch jazz:register-autoload that was used to implement
;;   the old load
;; - Cleanup the probably not usefull new method jazz:resolve-binding that I added to get
;;   things working


(block dialect.dialect-runtime


;; mega hack to test idea
(define jazz:call-being-inlined
  (make-parameter #f))


;;;
;;;; Dialect
;;;


(jazz:define-class jazz:Dialect jazz:Object ()
  ((name         getter: generate)
   (declarations getter: generate)
   (bindings     getter: generate)))


(jazz:define-virtual (jazz:dialect-walker (jazz:Dialect dialect)))
(jazz:define-virtual (jazz:dialect-wrap (jazz:Dialect dialect) body))


(jazz:define-method (jazz:dialect-walker (jazz:Dialect dialect))
  #f)


(jazz:define-method (jazz:dialect-wrap (jazz:Dialect dialect) body)
  body)


;;;
;;;; Dialects
;;;


(define jazz:Dialects
  (%%make-table test: eq?))


(define (jazz:get-dialect name)
  (%%table-ref jazz:Dialects name #f))


(define (jazz:require-dialect name)
  (%%unless (%%eq? name 'foundation.dialect)
    (jazz:load-unit name))
  (or (jazz:get-dialect name)
      (jazz:error "Unknown dialect: {s}" name)))


(define (jazz:register-dialect name dialect)
  (%%table-set! jazz:Dialects name dialect))


(jazz:define-macro (jazz:define-dialect name dialect)
  `(jazz:register-dialect ',name ,dialect))


;;;
;;;; Bindings
;;;


(define (jazz:register-declaration dialect-name binding)
  (let ((dialect (jazz:get-dialect dialect-name))
        (binding-name (jazz:get-lexical-binding-name binding)))
    (%%table-set! (jazz:get-dialect-declarations dialect) binding-name binding)))


(define (jazz:register-binding dialect-name binding)
  (let ((dialect (jazz:get-dialect dialect-name))
        (binding-name (jazz:get-lexical-binding-name binding)))
    (%%table-set! (jazz:get-dialect-bindings dialect) binding-name binding)))


(jazz:define-macro (jazz:define-walker-declaration name dialect-name declaration-method binding-method)
  `(begin
     (jazz:register-declaration ',dialect-name (jazz:new-declaration-form ',name ,declaration-method))
     (jazz:register-binding ',dialect-name (jazz:new-special-form ',name ,binding-method))))


(jazz:define-macro (jazz:define-walker-special name dialect-name method)
  `(jazz:register-binding ',dialect-name (jazz:new-special-form ',name ,method)))


(jazz:define-macro (jazz:define-walker-syntax name dialect-name method)
  `(jazz:register-binding ',dialect-name (jazz:new-syntax-form ',name ,method)))


(jazz:define-macro (jazz:define-walker-macro name dialect-name method)
  `(jazz:register-binding ',dialect-name (jazz:new-macro-form ',name ,method)))


;;;
;;;; Code Analysis
;;;


;; complex code analysis for tools
(define jazz:analysis-mode?
  (%%make-parameter #f))


;; declarations analysis table
(define jazz:analysis-data
  (%%make-table test: eq?))


;;;
;;;; Walk Access
;;;


;; access internal to the module
(define jazz:private-access
  0)

;; access from external modules
(define jazz:public-access
  1)

;; access through inheritance
(define jazz:protected-access
  2)


(define (jazz:make-access-lookups access-level)
  (let ((lookups (%%make-vector (%%fx+ access-level 1))))
    (let iter ((n 0))
      (if (%%fx<= n access-level)
          (begin
            (%%vector-set! lookups n (%%make-table test: eq?))
            (iter (%%fx+ n 1)))))
    lookups))


;;;
;;;; Walker
;;;


(jazz:define-class jazz:Walker jazz:Object ()
  ((declarations getter: generate setter: generate)
   (bindings     getter: generate setter: generate)
   (warnings     getter: generate setter: generate)
   (errors       getter: generate setter: generate)
   (literals     getter: generate setter: generate)
   (variables    getter: generate)
   (statics      getter: generate)
   (exports      getter: generate)
   (references   getter: generate)
   (autoloads    getter: generate setter: generate)))


(jazz:define-virtual (jazz:walker-declarations (jazz:Walker walker)))
(jazz:define-virtual (jazz:walker-bindings (jazz:Walker walker)))
(jazz:define-virtual (jazz:walk-form (jazz:Walker walker) resume declaration environment form-src))
(jazz:define-virtual (jazz:walk-symbol (jazz:Walker walker) resume declaration environment symbol-src))
(jazz:define-virtual (jazz:walk-symbol-assignment (jazz:Walker walker) resume declaration environment symbol-src value form-src))
(jazz:define-virtual (jazz:validate-proclaim (jazz:Walker walker) resume declaration form-src))
(jazz:define-virtual (jazz:runtime-export (jazz:Walker walker) declaration))
(jazz:define-virtual (jazz:lookup-environment (jazz:Walker walker) resume declaration environment symbol-src symbol))
(jazz:define-virtual (jazz:lookup-analyse (jazz:Walker walker) declaration symbol-src referenced-declaration))


;; provide virtual access to some walker slots via the module-declaration
(define (jazz:get-module-declaration-walker-literals lib-decl)
  (jazz:get-walker-literals (jazz:get-module-declaration-walker lib-decl)))
(define (jazz:set-module-declaration-walker-literals lib-decl value)
  (jazz:set-walker-literals (jazz:get-module-declaration-walker lib-decl) value))
(define (jazz:get-module-declaration-walker-variables lib-decl)
  (jazz:get-walker-variables (jazz:get-module-declaration-walker lib-decl)))
(define (jazz:get-module-declaration-walker-statics lib-decl)
  (jazz:get-walker-statics (jazz:get-module-declaration-walker lib-decl)))
(define (jazz:get-module-declaration-walker-exports lib-decl)
  (jazz:get-walker-exports (jazz:get-module-declaration-walker lib-decl)))
(define (jazz:get-module-declaration-walker-references lib-decl)
  (jazz:get-walker-references (jazz:get-module-declaration-walker lib-decl)))
(define (jazz:get-module-declaration-walker-autoloads lib-decl)
  (jazz:get-walker-autoloads (jazz:get-module-declaration-walker lib-decl)))
(define (jazz:set-module-declaration-walker-autoloads lib-decl value)
  (jazz:set-walker-autoloads (jazz:get-module-declaration-walker lib-decl) value))


;;;
;;;; Walk Binding
;;;


(jazz:define-class jazz:Walk-Binding jazz:Type ()
  ())


(jazz:define-method (jazz:emit-type (jazz:Walk-Binding type) source-declaration walker resume environment)
  (jazz:sourcified-form (jazz:emit-binding-reference type source-declaration walker resume environment)))


(jazz:define-method (jazz:specifiable? (jazz:Walk-Binding binding))
  #f)


(jazz:define-virtual (jazz:walk-binding-lookup (jazz:Walk-Binding binding) symbol source-declaration))
(jazz:define-virtual (jazz:walk-binding-referenced (jazz:Walk-Binding binding)))
(jazz:define-virtual (jazz:walk-binding-validate-call (jazz:Walk-Binding binding) walker resume source-declaration operator arguments form-src))
(jazz:define-virtual (jazz:walk-binding-validate-assignment (jazz:Walk-Binding binding) walker resume source-declaration symbol-src))
(jazz:define-virtual (jazz:walk-binding-assignable? (jazz:Walk-Binding binding)))
(jazz:define-virtual (jazz:walk-binding-walkable? (jazz:Walk-Binding binding)))
(jazz:define-virtual (jazz:walk-binding-walk-form (jazz:Walk-Binding binding) walker resume declaration environment form-src))
(jazz:define-virtual (jazz:walk-binding-expandable? (jazz:Walk-Binding binding)))
(jazz:define-virtual (jazz:walk-binding-expand-form (jazz:Walk-Binding binding) walker resume declaration environment form-src))
(jazz:define-virtual (jazz:emit-binding-symbol (jazz:Walk-Binding binding) source-declaration environment))
(jazz:define-virtual (jazz:emit-binding-reference (jazz:Walk-Binding binding) source-declaration walker resume environment))
(jazz:define-virtual (jazz:emit-binding-call (jazz:Walk-Binding binding) binding-src arguments arguments-codes source-declaration walker resume environment))
(jazz:define-virtual (jazz:emit-inlined-binding-call (jazz:Walk-Binding binding) arguments call source-declaration walker resume environment))
(jazz:define-virtual (jazz:emit-binding-assignment (jazz:Walk-Binding binding) value source-declaration walker resume environment form-src))


(jazz:define-method (jazz:walk-binding-lookup (jazz:Walk-Binding binding) symbol source-declaration)
  #f)


(jazz:define-method (jazz:walk-binding-referenced (jazz:Walk-Binding binding))
  (jazz:unspecified))


(jazz:define-method (jazz:walk-binding-validate-call (jazz:Walk-Binding binding) walker resume source-declaration operator arguments form-src)
  (jazz:unspecified))


(jazz:define-method (jazz:walk-binding-validate-assignment (jazz:Walk-Binding binding) walker resume source-declaration symbol-src)
  (%%when (%%not (jazz:walk-binding-assignable? binding))
    (jazz:walk-error walker resume source-declaration symbol-src "Illegal assignment to: {s}" binding)))


(jazz:define-method (jazz:walk-binding-assignable? (jazz:Walk-Binding binding))
  #f)


(jazz:define-method (jazz:walk-binding-walkable? (jazz:Walk-Binding binding))
  #f)


(jazz:define-method (jazz:walk-binding-walk-form (jazz:Walk-Binding binding) walker resume declaration environment form-src)
  (jazz:call-into-abstract (jazz:class-of binding) 'walk-binding-walk-form binding '()))


(jazz:define-method (jazz:walk-binding-expandable? (jazz:Walk-Binding binding))
  #f)


(jazz:define-method (jazz:walk-binding-expand-form (jazz:Walk-Binding binding) walker resume declaration environment form-src)
  (jazz:call-into-abstract (jazz:class-of binding) 'walk-binding-expand-form binding '()))


(jazz:define-method (jazz:emit-binding-symbol (jazz:Walk-Binding binding) source-declaration environment)
  (jazz:error "Unable to emit binding symbol for: {s}" binding))


(jazz:define-method (jazz:emit-binding-reference (jazz:Walk-Binding binding) source-declaration walker resume environment)
  (jazz:error "Unable to emit binding reference for: {s}" binding))


(jazz:define-method (jazz:emit-binding-call (jazz:Walk-Binding binding) binding-src arguments arguments-codes source-declaration walker resume environment)
  (let ((type (jazz:get-lexical-binding-type binding))
        (operator (jazz:emit-binding-reference binding source-declaration walker resume environment)))
    (jazz:new-code
      `(,(jazz:sourcified-form2 operator binding-src)
        ,@(jazz:codes-forms arguments-codes))
      (jazz:call-return-type type)
      #f)))


(jazz:define-method (jazz:emit-inlined-binding-call (jazz:Walk-Binding binding) arguments call source-declaration walker resume environment)
  #f)


(jazz:define-method (jazz:emit-binding-assignment (jazz:Walk-Binding binding) value source-declaration walker resume environment form-src)
  (jazz:unspecified))


(define (jazz:call-return-type operator-type)
  (if (%%is? operator-type jazz:Function-Type)
      (jazz:get-function-type-result operator-type)
    jazz:Any))


;;;
;;;; Lexical Binding
;;;


(jazz:define-class jazz:Lexical-Binding jazz:Walk-Binding ()
  ((name getter: generate)
   (type getter: generate setter: generate)
   (hits getter: generate setter: generate)))


(jazz:define-virtual (jazz:resolve-binding (jazz:Lexical-Binding binding)))


(jazz:define-method (jazz:resolve-binding (jazz:Lexical-Binding binding))
  binding)


(define (jazz:resolve-bindings bindings)
  (map (lambda (binding)
         (jazz:resolve-binding binding))
       bindings))


(jazz:define-method (jazz:resolve-type (jazz:Lexical-Binding binding))
  (jazz:resolve-binding binding))


(jazz:define-method (jazz:print-object (jazz:Lexical-Binding binding) output detail)
  (jazz:format output "#<{a} {a} #{a}>"
               (%%get-category-identifier (%%get-object-class binding))
               (jazz:get-lexical-binding-name binding)
               (jazz:object->serial binding)))


(jazz:define-method (jazz:walk-binding-lookup (jazz:Lexical-Binding binding) symbol source-declaration)
  (if (%%eq? (jazz:get-lexical-binding-name binding) symbol)
      binding
    #f))


(define (jazz:lexical-binding-hits binding)
  (or (jazz:get-lexical-binding-hits binding)
      (let ((table (%%make-table test: eq?)))
        (jazz:set-lexical-binding-hits binding table)
        table)))


(jazz:define-method (jazz:emit-binding-symbol (jazz:Lexical-Binding binding) declaration environment)
  (jazz:get-lexical-binding-name binding))


;;;
;;;; Declaration
;;;


(jazz:define-class jazz:Declaration jazz:Lexical-Binding ()
  ((access        getter: generate)
   (compatibility getter: generate)
   (modifiers     getter: generate)
   (attributes    getter: generate)
   (toplevel      getter: generate setter: generate)
   (parent        getter: generate setter: generate)
   (locator       getter: generate setter: generate)
   (source        getter: generate setter: generate)
   (name-source   getter: generate setter: generate)))


(define (jazz:setup-declaration new-declaration)
  (let ((parent (jazz:get-declaration-parent new-declaration))
        (name (jazz:get-lexical-binding-name new-declaration)))
    (jazz:set-declaration-locator new-declaration (if (%%not parent) name (jazz:compose-declaration-locator new-declaration)))
    (jazz:set-declaration-toplevel new-declaration (if (%%not parent) new-declaration (jazz:get-declaration-toplevel parent)))))


(define (jazz:get-declaration-path declaration)
  (define (iter declaration)
    (let ((name (jazz:get-lexical-binding-name declaration))
          (parent (jazz:get-declaration-parent declaration)))
      (if (%%not parent)
          (%%list name)
        (%%cons name (iter parent)))))
  
  (jazz:reverse! (iter declaration)))


(jazz:define-method (jazz:walk-binding-lookup (jazz:Declaration binding) symbol source-declaration)
  (jazz:lookup-declaration binding symbol jazz:private-access source-declaration))


(jazz:define-method (jazz:walk-binding-validate-call (jazz:Declaration declaration) walker resume source-declaration operator arguments form-src)
  (jazz:walk-error walker resume source-declaration form-src "{a} is not callable" declaration))


(jazz:define-virtual (jazz:compose-declaration-locator (jazz:Declaration declaration)))
(jazz:define-virtual (jazz:lookup-declaration (jazz:Declaration declaration) symbol access source-declaration))
(jazz:define-virtual (jazz:get-declaration-inclusions (jazz:Declaration declaration)))
(jazz:define-virtual (jazz:get-nextmethod-signature (jazz:Declaration declaration)))
(jazz:define-virtual (jazz:emit-declaration (jazz:Declaration declaration) walker resume environment))
(jazz:define-virtual (jazz:expand-referenced-declaration (jazz:Declaration declaration)))
(jazz:define-virtual (jazz:outline-generate (jazz:Declaration declaration) output))
(jazz:define-virtual (jazz:outline-extract (jazz:Declaration declaration) meta))


(jazz:define-method (jazz:compose-declaration-locator (jazz:Declaration declaration))
  (let ((parent (jazz:get-declaration-parent declaration))
        (name (jazz:get-lexical-binding-name declaration)))
    (%%compose-reference (jazz:get-declaration-locator parent) name)))


(jazz:define-method (jazz:lookup-declaration (jazz:Declaration declaration) symbol access source-declaration)
  #f)


(jazz:define-method (jazz:get-declaration-inclusions (jazz:Declaration declaration))
  '())


(jazz:define-method (jazz:get-nextmethod-signature (jazz:Declaration declaration))
  (jazz:error "No nextmethod signature for: {s}" declaration))


(jazz:define-method (jazz:emit-declaration (jazz:Declaration declaration) walker resume environment)
  (jazz:error "Unable to emit: {s}" declaration))


(jazz:define-method (jazz:expand-referenced-declaration (jazz:Declaration declaration))
  '())


(jazz:define-method (jazz:outline-generate (jazz:Declaration declaration) output)
  (let ((expr (jazz:outline-extract declaration '())))
    (if expr
        (jazz:format output "{%}  {s}" expr))))


(jazz:define-method (jazz:outline-extract (jazz:Declaration declaration) meta)
  (jazz:error "Unable to outline extract: {s}" declaration))


;; inline not supported in outlines at the moment
(define (jazz:outline-generate-modifiers declaration)
  (jazz:remove 'inline (jazz:get-declaration-modifiers declaration)))


(define (jazz:outline-generate-filter-access declarations #!optional (public-lookup #f))
  declarations
  ;; only generating public definitions works for using
  ;; a .o1 as a library that can be imported but doesn't
  ;; work for ctrl-enter which needs access to all fields
  ;; maybe one day the debugger could generate or retrieve
  ;; the full outline and send it to the debuggee!?
  #;
  (jazz:collect-if (lambda (decl)
                     (or (%%not (%%eq? (jazz:get-declaration-access decl) 'private))
                         (and public-lookup
                              (%%table-ref public-lookup (jazz:get-lexical-binding-name decl) #f))))
                   declarations))


(define (jazz:outline-generate-signature signature #!optional (method? #f))
  (if (not signature)
      #f
    (let ((queue (jazz:new-queue))
          (first-positional? #t))
      (define (add-positional parameter)
        (let ((name (jazz:get-lexical-binding-name parameter)))
          (if (and method? first-positional?)
              (begin
                (jazz:enqueue queue name)
                (set! first-positional? #f))
            (jazz:enqueue-list queue `(,name ,@(jazz:outline-generate-parameter-type-list parameter))))))
      
      ;; adding (unspecified) is a quick solution
      (define (add-optional parameter)
        (jazz:enqueue queue `(,(jazz:get-lexical-binding-name parameter) (unspecified))))
      
      ;; adding (unspecified) is a quick solution
      (define (add-keyword parameter)
        (let ((name (jazz:get-lexical-binding-name parameter)))
          (jazz:enqueue queue `(,(string->keyword (symbol->string name)) ,name (unspecified)))))
      
      (define (add-rest parameter)
        (jazz:enqueue-list queue (jazz:get-lexical-binding-name parameter)))
      
      (let ((positional (jazz:get-signature-positional signature))
            (optional (jazz:get-signature-optional signature))
            (named (jazz:get-signature-named signature))
            (rest (jazz:get-signature-rest signature)))
        (for-each add-positional positional)
        (for-each add-optional optional)
        (for-each add-keyword named)
        (%%when rest (add-rest rest)))
      (jazz:queue-list queue))))


(define (jazz:outline-generate-type-list type)
  (if (and type (%%is-not? type jazz:Any-Class))
      (%%list (jazz:type->specifier type))
    '()))


(define (jazz:outline-generate-specifier-list specifier-source)
  (if specifier-source
      (%%list (jazz:source-code specifier-source))
    '()))


(define (jazz:outline-generate-parameter-type-list parameter)
  (let ((specifier-source (jazz:get-variable-specifier-source parameter)))
    (if specifier-source
        (%%list (jazz:source-code specifier-source))
      (jazz:outline-generate-type-list (jazz:get-lexical-binding-type parameter)))))


(define (jazz:outline-generate-access-list declaration)
  (if (%%table-ref (jazz:get-public-lookup (jazz:get-declaration-toplevel declaration)) (jazz:get-lexical-binding-name declaration) #f)
      '(public)
    (let ((access (jazz:get-declaration-access declaration)))
      (if access
          (%%list access)
        '()))))


(define (jazz:outline-generate-phase-list phase)
  (if (and phase (%%neq? phase 'runtime))
      `((phase ,phase))
    '()))


(define (jazz:outline-generate-transformations transformations)
  (if (%%not transformations)
      '()
    transformations))


(define (jazz:declaration-result)
  (if (%%eq? (jazz:walk-for) 'eval)
      '((jazz:unspecified))
    '()))


;; In order to be able to resolve internal declarations as we walk the code, the declaration
;; tree is pre-expanded including expanding macros when needed. Then, during the actual walk
;; we just find the declarations in this tree. This tree is also merged with any preexisting
;; declaration tree coming from the runtime catalog.


(define (jazz:walk-declarations walker resume declaration environment forms)
  (define (walk forms)
    (for-each (lambda (form-src)
                (continuation-capture
                  (lambda (resume)
                    (jazz:cond-expand walker resume declaration form-src
                      (lambda (expr expr?)
                        (%%when expr?
                          (let ((expansion (jazz:expand-macros walker resume declaration environment expr)))
                            (cond ((jazz:include-form? expansion)
                                   (walk (jazz:include-forms (jazz:include-filename expansion))))
                                  ((jazz:begin-form? expansion)
                                   (walk (%%cdr (jazz:unwrap-syntactic-closure expansion))))
                                  (else
                                   (walk-declaration resume expansion))))))))))
              forms))
  
  (define (walk-declaration resume form-src)
    (if (%%pair? (jazz:unwrap-syntactic-closure form-src))
        (let ((first-src (%%car (jazz:unwrap-syntactic-closure form-src))))
          (let ((first (jazz:unwrap-syntactic-closure first-src))
                (declaration-environment (jazz:walker-declaration-environment walker)))
            (let ((frame (%%car declaration-environment))) ;; unique frame for now
              (let ((binding (jazz:walk-binding-lookup frame first #f)))
                (if binding
                    (let ((walk (jazz:get-declaration-form-walk binding)))
                      (walk walker resume declaration environment form-src))
                  #; ;; todo validate outline errors
                  (jazz:walk-free-reference walker resume declaration first-src))))))))
  
  (walk forms))


(define (jazz:add-declaration-child walker resume namespace-declaration child)
  (let ((name (jazz:get-lexical-binding-name child)))
    (%%when (%%not (jazz:find-declaration-child namespace-declaration name))
      (jazz:enqueue (jazz:get-namespace-declaration-children namespace-declaration) child))
    (%%table-set! (jazz:get-private-lookup namespace-declaration) name child)
    ;; for now everything not private is considered public
    (%%when (%%neq? (jazz:get-declaration-access child) 'private)
      (%%table-set! (jazz:get-public-lookup namespace-declaration) name child))
    child))


(define (jazz:require-declaration namespace-declaration name)
  (let ((declaration (jazz:find-declaration namespace-declaration name)))
    (%%assertion declaration (jazz:error "Unable to find declaration: {a}" name)
      declaration)))


(define (jazz:require-declaration-of-type type namespace-declaration name)
  (let ((decl (jazz:require-declaration namespace-declaration name)))
    (if (%%is? decl type)
        decl
      (jazz:error "Found conflicting declarations for {a}" name))))


(define (jazz:find-declaration namespace-declaration name)
  (%%table-ref (jazz:get-private-lookup namespace-declaration) name #f))


(define (jazz:find-public-declaration namespace-declaration name)
  (%%table-ref (jazz:get-public-lookup namespace-declaration) name #f))


(define (jazz:find-declaration-child namespace-declaration name)
  (jazz:find-if (lambda (decl)
                  (%%eq? (jazz:get-lexical-binding-name decl) name))
                (jazz:queue-list (jazz:get-namespace-declaration-children namespace-declaration))))


(define (jazz:find-declaration-child-of-type type namespace-declaration name)
  (let ((decl (jazz:find-declaration-child namespace-declaration name)))
    (if (or (%%not decl)
            (%%is? decl type))
        decl
      (jazz:error "Found conflicting declarations for {a}" name))))


(define (jazz:remove-declaration-child namespace-declaration name)
  (let ((declaration (jazz:find-declaration-child namespace-declaration name)))
    (%%when declaration
      (let ((queue (jazz:get-namespace-declaration-children namespace-declaration)))
        (let ((lst (jazz:remove! declaration (jazz:queue-list queue))))
          ;; this two-step process might be inefficient
          (jazz:reset-queue queue)
          (jazz:enqueue-list queue lst))))))


(define (jazz:begin-form? form)
  (and (%%pair? (jazz:source-code form))
       (%%eq? (jazz:source-code (%%car (jazz:source-code form))) 'begin)))


(define (jazz:define-form? form)
  (and (%%pair? (jazz:source-code form))
       (%%eq? (jazz:source-code (%%car (jazz:source-code form))) 'define)))


(define (jazz:declare-form? form)
  (and (%%pair? (jazz:source-code form))
       (%%eq? (jazz:source-code (%%car (jazz:source-code form))) 'declare)))


(define (jazz:proclaim-form? form)
  (and (%%pair? (jazz:source-code form))
       (%%eq? (jazz:source-code (%%car (jazz:source-code form))) 'proclaim)))


;;;
;;;; Declaration Reference
;;;


(jazz:define-class jazz:Declaration-Reference jazz:Object ()
  ((name        getter: generate)
   (declaration getter: generate setter: generate)))


(jazz:define-virtual (jazz:resolve-reference (jazz:Declaration-Reference declaration-reference) module-declaration))


(jazz:define-method (jazz:resolve-reference (jazz:Declaration-Reference declaration-reference) module-declaration)
  (or (jazz:get-declaration-reference-declaration declaration-reference)
      (receive (name symbol) (jazz:parse-exported-symbol module-declaration (jazz:get-declaration-reference-name declaration-reference))
        (let ((declaration (jazz:new-export-declaration name #f 'public 'uptodate '() '() #f symbol)))
          (jazz:set-declaration-reference-declaration declaration-reference declaration)
          declaration))))


;;;
;;;; Module Reference
;;;


(jazz:define-class jazz:Module-Reference jazz:Declaration-Reference (constructor: jazz:allocate-module-reference)
  ())


(define (jazz:new-module-reference name declaration)
  (jazz:allocate-module-reference name declaration))


(jazz:define-method (jazz:resolve-reference (jazz:Module-Reference declaration-reference) module-declaration)
  (or (jazz:get-declaration-reference-declaration declaration-reference)
      (let ((declaration (jazz:outline-module (jazz:get-declaration-reference-name declaration-reference))))
        (jazz:set-declaration-reference-declaration declaration-reference declaration)
        declaration)))


;;;
;;;; Export Reference
;;;


(jazz:define-class jazz:Export-Reference jazz:Declaration-Reference (constructor: jazz:allocate-export-reference)
  ((module-reference getter: generate)))


(define (jazz:new-export-reference name declaration module-reference)
  (jazz:allocate-export-reference name declaration module-reference))


(jazz:define-method (jazz:resolve-reference (jazz:Export-Reference declaration-reference) module-declaration)
  (or (jazz:get-declaration-reference-declaration declaration-reference)
      (receive (name symbol) (jazz:parse-exported-symbol module-declaration (jazz:get-declaration-reference-name declaration-reference))
        (let ((locator (jazz:compose-reference (jazz:get-lexical-binding-name module-declaration) name)))
          (let ((declaration (jazz:new-export-declaration name #f 'public 'uptodate '() '() module-declaration locator)))
            (jazz:set-declaration-reference-declaration declaration-reference declaration)
            declaration)))))


(define (jazz:parse-exported-symbol module-declaration name)
  (cond ((jazz:composite-namespace? name)
         (values (jazz:namespace-name name) name))
        ((jazz:composite-reference? name)
         (values (jazz:reference-name name) name))
        (else
         (values name name))))


;;;
;;;; Autoload Reference
;;;


(jazz:define-class jazz:Autoload-Reference jazz:Export-Reference (constructor: jazz:allocate-autoload-reference)
  ((hubs? getter: generate)))


(define (jazz:new-autoload-reference name declaration module-reference hubs?)
  (jazz:allocate-autoload-reference name declaration module-reference hubs?))


(define (jazz:autoload-reference=? x y)
  (and (%%eq? (jazz:get-export-reference-module-reference x)
              (jazz:get-export-reference-module-reference y))
       (%%eqv? (jazz:get-autoload-reference-hubs? x)
               (jazz:get-autoload-reference-hubs? y))))


(define (jazz:cache-autoload-reference declaration-reference module-declaration exported-module-reference)
  (or (jazz:get-declaration-reference-declaration declaration-reference)
      (let* ((name (jazz:get-declaration-reference-name declaration-reference))
             (type jazz:Any)
             (declaration (jazz:new-autoload-declaration name type #f module-declaration exported-module-reference)))
        (%%assert declaration
          (jazz:set-declaration-reference-declaration declaration-reference declaration)
          declaration))))


;;;
;;;; Unit
;;;


(jazz:define-class jazz:Unit-Declaration jazz:Declaration (constructor: jazz:allocate-unit-declaration)
  ((container getter: generate)
   (requires  getter: generate setter: generate)))


(define (jazz:new-unit-declaration name access parent requires)
  (define (determine-package)
    (let ((resource (jazz:requested-unit-resource)))
      (and resource
           (%%get-resource-package resource))))
  
  (let ((new-declaration (jazz:allocate-unit-declaration name #f #f access 'uptodate '() '() #f parent #f #f #f (determine-package) requires)))
    (jazz:setup-declaration new-declaration)
    new-declaration))


(define (jazz:parse-unit-declaration partial-form)
  (define (parse rest proc)
    (let ((first (jazz:source-code (%%car rest))))
      (if (%%memq first '(protected public))
          (proc (jazz:source-code (%%cadr rest)) first (%%cddr rest))
        (proc (jazz:source-code (%%car rest)) 'public (%%cdr rest)))))

  (define (collect-requires body)
    (let ((requires '()))
      (for-each (lambda (expr)
                  (if (and (%%pair? (jazz:source-code expr))
                           (%%eq? (jazz:source-code (%%car (jazz:source-code expr))) 'require))
                      (set! requires (append requires (map jazz:listify (jazz:filter-invoices (%%cdr (%%desourcify expr))))))))
                body)
      requires))
  
  (parse partial-form
    (lambda (name access body)
      (if (and (jazz:requested-unit-name) (%%neq? name (jazz:requested-unit-name)))
          (jazz:error "Unit at {s} is defining {s}" (jazz:requested-unit-name) name)
        (let ((requires (collect-requires body)))
          (let ((require-invoices (map (lambda (require)
                                         (jazz:parse-require require
                                           (lambda (require-name feature-requirement phase)
                                             (jazz:require-unit-src require-name)
                                             (jazz:new-require-invoice require-name phase))))
                                       requires)))
            (jazz:new-unit-declaration name access #f require-invoices)))))))


(jazz:define-method (jazz:outline-generate (jazz:Unit-Declaration declaration) output)
  (jazz:format output "(unit {a}" (jazz:get-lexical-binding-name declaration))
  #; ;; requires are not needed and create problems when outlined
  (for-each (lambda (require-invoice)
              (let ((name (jazz:get-require-invoice-name require-invoice))
                    (phase (jazz:get-require-invoice-phase require-invoice)))
                (jazz:format output "{%}  {s}" `(require (,name ,@(jazz:outline-generate-phase-list phase))))))
            (jazz:get-unit-declaration-requires declaration))
  (jazz:format output "){%}"))


;;;
;;;; Namespace
;;;


(jazz:define-class jazz:Namespace-Declaration jazz:Declaration ()
  ((lookups  getter: generate)
   (children getter: generate)
   (body     getter: generate setter: generate)))


(jazz:define-method (jazz:lookup-declaration (jazz:Namespace-Declaration namespace-declaration) symbol access source-declaration)
  (define (add-to-hits declaration)
    (%%when (and declaration source-declaration (jazz:analysis-mode?))
      (let ((hits-table (jazz:lexical-binding-hits declaration)))
        (%%table-set! hits-table (jazz:get-declaration-locator source-declaration) source-declaration))
      (%%when (%%is? declaration jazz:Autoload-Declaration)
        (jazz:set-analysis-data-autoload-reference (jazz:get-analysis-data (jazz:get-declaration-locator declaration)) declaration))))
  
  (let ((found (%%table-ref (jazz:get-access-lookup namespace-declaration access) symbol #f)))
    (jazz:add-to-module-references namespace-declaration found)
    (add-to-hits found)
    found))


(jazz:define-method (jazz:tree-fold (jazz:Namespace-Declaration declaration) down up here seed environment)
  (up declaration
      seed
      (jazz:tree-fold-list
        (jazz:get-namespace-declaration-body declaration) down up here (down declaration seed environment) environment)
      environment))


(define (jazz:get-private-lookup namespace-declaration)
  (jazz:get-access-lookup namespace-declaration jazz:private-access))

(define (jazz:get-public-lookup namespace-declaration)
  (jazz:get-access-lookup namespace-declaration jazz:public-access))

(define (jazz:get-protected-lookup namespace-declaration)
  (jazz:get-access-lookup namespace-declaration jazz:protected-access))


;;;
;;;; Module
;;;


(jazz:define-class jazz:Module-Declaration jazz:Namespace-Declaration (constructor: jazz:allocate-module-declaration)
  ((walker          getter: generate setter: generate)
   (container       getter: generate)
   (dialect-name    getter: generate)
   (dialect-invoice getter: generate)
   (requires        getter: generate setter: generate)
   (exports         getter: generate setter: generate)
   (imports         getter: generate setter: generate)
   (inclusions      getter: generate setter: generate)
   (local-macros    getter: generate)))


(define (jazz:new-module-declaration name access modifiers parent walker dialect-name dialect-invoice)
  (define (determine-package)
    (let ((resource (jazz:requested-unit-resource)))
      (and resource
           (%%get-resource-package resource))))
  
  (let ((new-declaration (jazz:allocate-module-declaration name #f #f access 'uptodate modifiers '() #f parent #f #f #f (jazz:make-access-lookups jazz:public-access) (jazz:new-queue) #f walker (determine-package) dialect-name dialect-invoice '() '() '() '() (%%make-table test: eq?))))
    (jazz:setup-declaration new-declaration)
    new-declaration))


(define (jazz:get-unit/module-container unit/module)
  (if (%%is? unit/module jazz:Unit-Declaration)
      (jazz:get-unit-declaration-container unit/module)
    (jazz:get-module-declaration-container unit/module)))


(define (jazz:get-unit/module-requires unit/module)
  (if (%%is? unit/module jazz:Unit-Declaration)
      (jazz:get-unit-declaration-requires unit/module)
    (jazz:get-module-declaration-requires unit/module)))


(define (jazz:add-module-require module-declaration require)
  (jazz:parse-require require
    (lambda (require-name feature-requirement phase)
      (define (find-require requires)
        (jazz:find-if (lambda (req)
                        (%%eq? (jazz:get-require-invoice-name req) require-name))
                      requires))
      
      (%%when (%%eq? phase 'syntax)
        (jazz:load-unit require-name))
      (let ((requires (jazz:get-module-declaration-requires module-declaration)))
        (if (%%not (find-require requires))
            (begin
              (jazz:require-unit-src require-name)
              (let ((require-invoice (jazz:new-require-invoice require-name phase)))
                (jazz:set-module-declaration-requires module-declaration (%%append requires (%%list require-invoice))))))))))


(define (jazz:add-module-import walker resume declaration form-src module-declaration module-invoice register?)
  (define (merge-invoice actual new)
    ;; todo
    #f)
  
  (%%when (%%eq? (jazz:get-module-invoice-phase module-invoice) 'syntax)
    (let ((module-declaration (jazz:get-module-invoice-module module-invoice)))
      (jazz:load-unit (jazz:get-lexical-binding-name module-declaration))))
  (%%when register?
    (let ((imports (jazz:get-module-declaration-imports module-declaration)))
      (let ((actual (jazz:find-module-invoice imports module-invoice)))
        (if actual
            (merge-invoice actual module-invoice)
          (jazz:set-module-declaration-imports module-declaration (%%append imports (%%list module-invoice)))))))
  (let ((private (jazz:get-private-lookup module-declaration))
        (mangler (jazz:generate-module-mangler (jazz:get-module-invoice-transformations module-invoice))))
    (cond (mangler
            (let ((imported-module-declaration (jazz:get-module-invoice-module module-invoice))
                  (imported (%%make-table test: eq?)))
              (table-for-each (lambda (key declaration)
                                (let ((mangled-key (mangler key)))
                                  (if mangled-key
                                      (%%table-set! imported mangled-key declaration))))
                              (jazz:get-public-lookup imported-module-declaration))
              (jazz:table-merge-reporting-conflicts! walker resume declaration form-src module-declaration "Import" private imported)))
          (else
           (let ((imported-module-declaration (jazz:get-module-invoice-module module-invoice)))
             (let ((imported (jazz:get-public-lookup imported-module-declaration)))
               (jazz:table-merge-reporting-conflicts! walker resume declaration form-src module-declaration "Import" private imported)))))))


;; reset everything imported for code analysis
(define (jazz:reset-module-imported module-declaration)
  (let ((private (jazz:get-private-lookup module-declaration)))
    (for-each (lambda (module-invoice)
                (let ((invoice-module (jazz:get-module-invoice-module module-invoice)))
                  (jazz:iterate-table private
                    (lambda (name declaration)
                      (%%when (%%eq? (jazz:get-declaration-toplevel declaration) invoice-module)
                        (%%table-clear private name))))))
              (jazz:get-module-declaration-imports module-declaration))))


(define (jazz:add-module-export walker resume declaration form-src module-declaration module-invoice)
  (define (merge-invoice actual new)
    (let ((actual-autoload (jazz:get-export-invoice-autoload actual))
          (new-autoload (jazz:get-export-invoice-autoload new)))
      (%%when new-autoload
        (jazz:set-export-invoice-autoload actual (if actual-autoload (jazz:union actual-autoload new-autoload test: jazz:autoload-reference=?) new-autoload)))))
  
  (define (add-to-module-exports declaration)
    (%%when (and declaration
                 (%%neq? module-declaration (jazz:get-declaration-toplevel declaration)))
      (let ((exports-table (jazz:get-module-declaration-walker-exports module-declaration)))
        (%%table-set! exports-table (jazz:get-declaration-locator declaration) declaration))))
  
  (%%when (%%eq? (jazz:get-module-invoice-phase module-invoice) 'syntax)
    (let ((module-declaration (jazz:resolve-reference (jazz:get-module-invoice-module module-invoice) module-declaration)))
      (jazz:load-unit (jazz:get-lexical-binding-name module-declaration))))
  (let ((exports (jazz:get-module-declaration-exports module-declaration)))
    (let ((actual (jazz:find-module-invoice exports module-invoice)))
      (if actual
          (merge-invoice actual module-invoice)
        (jazz:set-module-declaration-exports module-declaration (%%append exports (%%list module-invoice))))))
  (let ((public (jazz:get-public-lookup module-declaration))
        (mangler (jazz:generate-module-mangler (jazz:get-module-invoice-transformations module-invoice)))
        (autoload (jazz:get-export-invoice-autoload module-invoice))
        (symbols (jazz:get-export-invoice-symbols module-invoice)))
    (cond (autoload
           (let ((exported-module-reference (jazz:get-module-invoice-module module-invoice)))
             (for-each (lambda (declaration-reference)
                         (let ((name (jazz:reference-name (jazz:get-declaration-reference-name declaration-reference)))
                               (declaration (jazz:cache-autoload-reference declaration-reference module-declaration exported-module-reference))
                               (hubs? (jazz:get-autoload-reference-hubs? declaration-reference)))
                           (%%table-set! public name declaration)
                           (add-to-module-exports declaration)
                           (if hubs?
                               (let ((unit-name (jazz:get-declaration-reference-name exported-module-reference)))
                                 (let ((shape (jazz:module-shape walker resume declaration unit-name)))
                                   (for-each (lambda (category)
                                               (let ((category-name (%%cadr category)))
                                                 (%%when (%%eq? category-name name)
                                                   (let ((hub-names (%%cddr category)))
                                                     (for-each (lambda (hub-name)
                                                                 (let ((meta? (%%pair? hub-name)))
                                                                   (let ((hub-name (if meta? (%%car hub-name) hub-name))
                                                                         (locator (jazz:autoload-declaration-locator-heuristic declaration)))
                                                                     (let ((category-locator (if meta? (%%string->symbol (%%string-append (%%symbol->string locator) "~Class")) locator)))
                                                                       (let ((new-declaration (jazz:new-hub-declaration hub-name jazz:Any 'public 'uptodate '() '() module-declaration #f (%%list category-locator))))
                                                                         (let ((actual-declaration (%%table-ref public hub-name #f)))
                                                                           (if actual-declaration
                                                                               (if (%%class-is? actual-declaration jazz:hub-declaration-class)
                                                                                   (let ((actual-hubs (jazz:hub-declaration-hubs actual-declaration))
                                                                                         (actual-nodes (jazz:hub-declaration-nodes actual-declaration)))
                                                                                     (jazz:hub-declaration-hubs-set! actual-declaration (jazz:union actual-hubs (%%list new-declaration)))
                                                                                     (jazz:hub-declaration-nodes-set! actual-declaration (jazz:union actual-nodes (%%list category-locator)))
                                                                                     (add-to-module-exports new-declaration))
                                                                                 (jazz:error "Found conflicting declarations for {a}" hub-name))
                                                                             (begin
                                                                               (%%table-set! public hub-name new-declaration)
                                                                               (add-to-module-exports new-declaration)))))))))
                                                               hub-names)))))
                                             shape))))))
                       autoload)))
          (symbols
           (for-each (lambda (declaration-reference)
                       (let ((name (jazz:reference-name (jazz:get-declaration-reference-name declaration-reference))))
                         (let ((mangled-name (if mangler (mangler name) name)))
                           (if mangled-name
                               (let ((declaration (jazz:resolve-reference declaration-reference module-declaration)))
                                 (%%table-set! public mangled-name declaration)
                                 (add-to-module-exports declaration))))))
                     symbols))
          (mangler
           (let ((exported-module-declaration (jazz:resolve-reference (jazz:get-module-invoice-module module-invoice) module-declaration))
                 (exported (%%make-table test: eq?)))
             (table-for-each (lambda (key declaration)
                               (let ((mangled-key (mangler key)))
                                 (if mangled-key
                                     (%%table-set! exported mangled-key declaration))))
                             (jazz:get-public-lookup exported-module-declaration))
             (jazz:table-merge-reporting-conflicts! walker resume declaration form-src module-declaration "Export" public exported)
             (table-for-each (lambda (key declaration)
                               (add-to-module-exports declaration))
                             exported)))
          (else
           (let* ((exported-module-declaration (jazz:resolve-reference (jazz:get-module-invoice-module module-invoice) module-declaration))
                  (exported-table (jazz:get-public-lookup exported-module-declaration)))
             (jazz:table-merge-reporting-conflicts! walker resume declaration form-src module-declaration "Export" public exported-table)
             (table-for-each (lambda (key declaration)
                               (add-to-module-exports declaration))
                             exported-table))))))


(define (jazz:generate-module-mangler transformations)
  (define (only parameters)
    (lambda (key)
      (if (and key (%%memq key parameters))
          key
        #f)))
  
  (define (except parameters)
    (lambda (key)
      (if (and key (%%not (%%memq key parameters)))
          key
        #f)))
  
  (define (prefix parameters)
    (let ((prefix (%%car parameters))
          (explicit (%%cdr parameters)))
      (let ((prefix (cond ((%%keyword? prefix) (%%string-append (keyword->string prefix) ":"))
                          ((%%symbol? prefix) (%%symbol->string prefix))
                          ((%%string? prefix) prefix)
                          (else (jazz:error "Invalid module prefix")))))
        (if (%%null? explicit)
            (lambda (key)
              (if key
                  (%%string->symbol (%%string-append prefix (%%symbol->string key)))
                #f))
          (lambda (key)
            (if (and key (%%memq key explicit))
                (%%string->symbol (%%string-append prefix (%%symbol->string key)))
              key))))))
  
  (define (rename parameters)
    (let ((mapping (%%make-table test: eq?)))
      (for-each (lambda (conversion)
                  (let ((key (%%car conversion))
                        (value (%%cadr conversion)))
                    (%%table-set! mapping key value)))
                parameters)
      (lambda (key)
        (if key
            (%%table-ref mapping key key)
          #f))))
  
  (define (find-mangler transformation)
    (case (%%car transformation)
      ((only) only)
      ((except) except)
      ((prefix) prefix)
      ((rename) rename)
      (else (jazz:error "Invalid module operation"))))
  
  (if transformations
      (let iter ((scan transformations)
                 (mangler #f))
           (if (%%not (%%null? scan))
               (let ((transformation (%%car scan)))
                 (let ((proc ((find-mangler transformation) (%%cdr transformation))))
                   (iter (%%cdr scan)
                         (if mangler
                             (lambda (key)
                               (proc (mangler key)))
                           proc))))
             mangler))
    #f))


(define (jazz:table-merge-reporting-conflicts! walker resume declaration form-src module-declaration prefix table add)
  (define (effective-locator decl)
    (cond ((%%is? decl jazz:Export-Declaration)
           (jazz:get-export-declaration-symbol decl))
          ((%%is? decl jazz:Autoload-Declaration)
           (jazz:autoload-declaration-locator-heuristic decl))
          ((and jazz:hub-declaration-class (%%is? decl jazz:hub-declaration-class))
           (%%string->symbol (%%string-append (%%symbol->string (jazz:get-lexical-binding-name decl)) "<hub>")))
          (else
           (jazz:get-declaration-locator decl))))
  
  (define (merge/report-conflicts)
    (jazz:iterate-table add
      (lambda (key new)
        (let ((actual (%%table-ref table key #f)))
          (if (and jazz:hub-declaration-class
                   (%%is? new jazz:hub-declaration-class)
                   (%%is? actual jazz:hub-declaration-class))
              (%%unless (%%eq? new actual)
                (let ((new-hubs (jazz:hub-declaration-hubs new))
                      (new-nodes (jazz:hub-declaration-nodes new))
                      (actual-hubs (jazz:hub-declaration-hubs actual))
                      (actual-nodes (jazz:hub-declaration-nodes actual)))
                  (let ((name (jazz:get-lexical-binding-name actual))
                        (type (jazz:get-lexical-binding-type actual))
                        (access (jazz:get-declaration-access actual))
                        (compatibility (jazz:get-declaration-compatibility actual))
                        (modifiers (jazz:get-declaration-modifiers actual))
                        (hubs (jazz:union actual-hubs new-hubs))
                        (nodes (jazz:union actual-nodes new-nodes)))
                    (let ((hub (jazz:new-hub-declaration name type access compatibility modifiers '() declaration hubs nodes)))
                      (%%table-set! table key hub)))))
            (let ((new-locator (effective-locator new))
                  (actual-locator (effective-locator actual)))
              (%%when (%%neq? new-locator actual-locator)
                (jazz:walk-conflict walker resume declaration prefix key actual-locator new-locator form-src))))))))
  
  (let ((table-count (%%table-length table))
        (add-count (%%table-length add)))
    (%%table-merge! table add #f)
    (%%when (%%not (%%fx= (%%table-length table) (%%fx+ table-count add-count)))
      ;; Can report no conflicts if the same declaration was imported from different modules
      (merge/report-conflicts))))


(define (jazz:add-to-module-references declaration referenced-declaration)
  (%%when referenced-declaration
    (let ((module-declaration (jazz:get-declaration-toplevel declaration))
          (referenced-module-declaration (jazz:get-declaration-toplevel referenced-declaration)))
      (%%when (%%neq? module-declaration referenced-module-declaration)
        (if (and jazz:hub-declaration-class (%%class-is? referenced-declaration jazz:hub-declaration-class))
            (let ((module-declaration (jazz:get-declaration-toplevel declaration)))
              (let ((references-table (jazz:get-module-declaration-walker-references module-declaration)))
                (for-each (lambda (hub)
                            (%%table-set! references-table (jazz:get-declaration-locator hub) hub))
                          (jazz:hub-declaration-hubs referenced-declaration))))
          (let ((references-table (jazz:get-module-declaration-walker-references module-declaration)))
            (%%table-set! references-table (jazz:get-declaration-locator referenced-declaration) referenced-declaration)))))))


(define (jazz:generate-reference-list module-declaration)
  (define (lesser name1 name2)
    (if (or (%%null? name1) (%%null? name2))
        (%%null? name1)
      (let ((string1 (%%symbol->string (%%car name1)))
            (string2 (%%symbol->string (%%car name2))))
        (or (%%string<? string1 string2)
            (and (%%string=? string1 string2)
                 (lesser (%%cdr name1) (%%cdr name2)))))))
  
  (define (merge-sorted item sorted)
    (cond ((%%null? sorted)
           (%%list item))
          ((lesser item (%%car sorted))
           (%%cons item sorted))
          (else
           (%%cons (%%car sorted) (merge-sorted item (%%cdr sorted))))))
  
  (define (compose-name root-declaration declaration)
    (let iter ((declaration declaration)
               (composite-identifier '()))
         (if (%%eq? root-declaration declaration)
             composite-identifier
           (iter (jazz:get-declaration-parent declaration) (%%cons (jazz:get-lexical-binding-name declaration) composite-identifier)))))
  
  (let ((partition (%%make-table test: eq?)))
    (define (merge-declaration locator declaration)
      (let ((resolved-declaration (jazz:resolve-binding declaration)))
        (let ((module (jazz:get-declaration-toplevel resolved-declaration)))
          (%%table-set! partition module
            (merge-sorted (compose-name module resolved-declaration) (%%table-ref partition module '()))))))
    
    ;; necessary to merge exports because of runtime access through module-ref
    (jazz:iterate-table (jazz:get-module-declaration-walker-exports module-declaration) merge-declaration)
    (jazz:iterate-table (jazz:get-module-declaration-walker-references module-declaration) merge-declaration)
    (let iter ((in (%%table->list partition))
               (out '()))
         (if (%%null? in)
             out
           (let ((module-locator (jazz:get-declaration-locator (%%caar in)))
                 (declarations (map (lambda (declaration)
                                      (if (and (%%pair? declaration) (%%null? (%%cdr declaration)))
                                          (%%car declaration)
                                        declaration))
                                    (%%cdar in))))
             (iter (%%cdr in) (merge-sorted (%%cons module-locator declarations) out)))))))


(jazz:define-method (jazz:lookup-declaration (jazz:Module-Declaration declaration) symbol access source-declaration)
  ;; code to detect unreferenced imports
  (%%when (jazz:analysis-mode?)
    (for-each (lambda (module-invoice)
                (let ((imported-module-declaration (jazz:get-module-invoice-module module-invoice)))
                  (let ((imported (jazz:get-public-lookup imported-module-declaration)))
                    (%%when (%%table-ref imported symbol #f)
                      (jazz:set-import-invoice-hit? module-invoice #t)))))
              (jazz:get-module-declaration-imports declaration)))
  (nextmethod declaration symbol access source-declaration))


(jazz:define-method (jazz:emit-declaration (jazz:Module-Declaration declaration) walker resume environment)
  (let ((body-expansion (jazz:emit-namespace-statements (jazz:get-namespace-declaration-body declaration) declaration walker resume environment))
        (inclusions-expansion (jazz:emit-module-inclusions declaration walker resume environment))
        (literals-expansion (jazz:emit-module-literals declaration walker resume environment))
        (variables-expansion (jazz:emit-module-variables declaration walker resume environment))
        (statics-expansion (jazz:emit-module-statics declaration walker resume environment))
        (autoloads-expansion (jazz:emit-module-autoloads declaration walker resume environment))
        (registration-expansion (jazz:emit-module-registration declaration walker resume environment)))
    `(begin
       ,@(case (jazz:walk-for)
           ((eval) '())
           (else (jazz:declares 'module)))
       ,@(let ((queue (jazz:new-queue))
               (load-units (%%make-table test: eq?)))
           (define (enqueue-load-unit unit-name)
             (%%when (%%not (%%table-ref load-units unit-name #f))
               (%%table-set! load-units unit-name #t)
               (jazz:enqueue queue `(jazz:load-unit ',unit-name))))
           
           (let ((dialect-name (jazz:get-module-declaration-dialect-name declaration)))
             (%%when (%%neq? dialect-name 'foundation.dialect)
               (enqueue-load-unit dialect-name)))
           (for-each (lambda (require-invoice)
                       (enqueue-load-unit (jazz:get-require-invoice-name require-invoice)))
                     (jazz:get-module-declaration-requires declaration))
           (for-each (lambda (module-invoice)
                       (let ((symbols (jazz:get-export-invoice-symbols module-invoice))
                             (autoload (jazz:get-export-invoice-autoload module-invoice)))
                         (%%when (and (%%not symbols) (%%not autoload))
                           (let ((module-declaration (jazz:resolve-reference (jazz:get-module-invoice-module module-invoice) declaration))
                                 (phase (jazz:get-module-invoice-phase module-invoice)))
                             (%%when (and (%%neq? module-declaration declaration) (%%neq? phase 'syntax))
                               (enqueue-load-unit (jazz:get-lexical-binding-name module-declaration)))))))
                     (jazz:get-module-declaration-exports declaration))
           (for-each (lambda (module-invoice)
                       (let ((module-declaration (jazz:get-module-invoice-module module-invoice))
                             (phase (jazz:get-module-invoice-phase module-invoice)))
                         (%%when (and module-declaration (%%neq? phase 'syntax))
                           (enqueue-load-unit (jazz:get-lexical-binding-name module-declaration)))))
                     (jazz:get-module-declaration-imports declaration))
           (jazz:queue-list queue))
       ,@registration-expansion
       ,@inclusions-expansion
       ,@autoloads-expansion
       ,@literals-expansion
       ,@variables-expansion
       ,@statics-expansion
       ,@body-expansion)))


;; hack around warnings being reported more than once if a
;; variable mutation triggers the annotation reset mecanism
(define jazz:reported-warnings
  (%%make-table test: equal?))


(define (jazz:warning fmt-string . rest)
  (let ((message (apply jazz:format fmt-string rest)))
    (if (%%not (%%table-ref jazz:reported-warnings message #f))
        (begin
          (if (%%not (jazz:reporting?))
              (jazz:feedback "{a}" message))
          (jazz:report "{a}" message)
          (%%table-set! jazz:reported-warnings message #t)))))


(define (jazz:unsafe-warning fmt-string . rest)
  (let ((message (apply jazz:format fmt-string rest)))
    (if (%%not (%%table-ref jazz:reported-warnings message #f))
        (begin
          ;; always display warnings about unsafe code
          (jazz:feedback "{a}" message)
          (jazz:report "{a}" message)
          (%%table-set! jazz:reported-warnings message #t)))))


(define (jazz:keyword->symbol keyword)
  (%%string->symbol (%%keyword->string keyword)))


(define (jazz:parse-module partial-form)
  (define (parse-modifiers rest)
    (let ((first (jazz:source-code (%%car rest))))
      (if (%%memq first '(protected public))
          (values first (%%cdr rest))
        (values 'public rest))))
  
  (receive (access rest) (parse-modifiers partial-form)
    (let ((name (jazz:source-code (%%car rest)))
          (dialect-src (%%cadr rest))
          (dialect (jazz:desourcify (%%cadr rest)))
          (body (%%cddr rest)))
      (receive (dialect-name dialect-transformations) (jazz:parse-dialect dialect)
        (%%assert (%%symbol? name)
          (values name
                  access
                  '()
                  dialect-name
                  dialect-transformations
                  dialect-src
                  body))))))


(define (jazz:parse-script partial-form)
  (let ((dialect-src (%%car partial-form))
        (dialect (jazz:desourcify (%%car partial-form)))
        (body (%%cdr partial-form)))
    (receive (dialect-name dialect-transformations) (jazz:parse-dialect dialect)
      (values dialect-name
              dialect-transformations
              dialect-src
              body))))


(define (jazz:parse-dialect dialect)
  (cond ((%%keyword? dialect)
         (values (jazz:keyword->symbol dialect) #f))
        ((and (%%pair? dialect)
              (%%eq? (%%car dialect) 'dialect))
         (%%assert (%%pair? (%%cdr dialect))
           (let ((name (%%cadr dialect))
                 (transformations (%%cddr dialect)))
             (values name transformations))))
        (else
         (values dialect '()))))


(define (jazz:parse-module-invoice walker resume declaration form-src specification)
  (define (ill-formed)
    (jazz:walk-error walker resume declaration form-src "Ill-formed module invoice: {s}" specification))
  
  (if (%%not (%%pair? specification))
      (ill-formed)
    (let ((phase #f)
          (autoload #f)
          (transformation-queue (jazz:new-queue)))
      (let ((name (%%car specification)))
        (if (%%symbol? name)
            (for-each (lambda (spec)
                        (if (%%pair? spec)
                            (let ((option (%%car spec))
                                  (arguments (%%cdr spec)))
                              (case option
                                ((phase)    (set! phase (%%car arguments)))
                                ((only)     (jazz:enqueue transformation-queue spec))
                                ((except)   (jazz:enqueue transformation-queue spec))
                                ((prefix)   (jazz:enqueue transformation-queue spec))
                                ((rename)   (jazz:enqueue transformation-queue spec))
                                ((autoload) (set! autoload arguments))
                                ((cond)      #f)
                                (else       (ill-formed))))
                          (ill-formed)))
                      (%%cdr specification))
          (ill-formed))
        (values name phase autoload (jazz:queue-list transformation-queue))))))


(define (jazz:parse-module-declaration partial-form)
  (receive (name access modifiers dialect-name dialect-transformations dialect-src body) (jazz:parse-module partial-form)
    (if (and (jazz:requested-unit-name) (%%neq? name (jazz:requested-unit-name)))
        (jazz:error "Module at {s} is defining {s}" (jazz:requested-unit-name) name)
      (parameterize ((jazz:walk-context (jazz:new-walk-context #f name #f)))
        (let* ((dialect-invoice (and dialect-transformations (jazz:load-dialect-invoice dialect-name dialect-transformations)))
               (dialect (jazz:require-dialect dialect-name))
               (walker (jazz:dialect-walker dialect)))
          (let ((declaration (jazz:walk-module-declaration walker #f name access modifiers dialect-name dialect-invoice dialect-src body)))
            #; ;; todo validate outline errors
            (jazz:validate-walk-problems walker)
            declaration))))))


(define (jazz:walk-module-declaration walker actual name access modifiers dialect-name dialect-invoice dialect-src body)
  (let ((declaration (or actual (jazz:new-module-declaration name access modifiers #f walker dialect-name dialect-invoice))))
    (%%when dialect-invoice
      (jazz:add-module-import walker #f declaration dialect-src declaration dialect-invoice #f))
    ;; reset the walker if it was cached
    (jazz:set-module-declaration-walker declaration walker)
    (jazz:walk-declarations walker #f declaration (%%cons declaration (jazz:walker-environment walker)) body)
    (jazz:validate-walk-problems walker)
    declaration))


(define (jazz:rename-identifier-conflicts expressions environment)
  (jazz:tree-fold-list
    expressions
    (lambda (x seed env) env)
    (lambda (x seed child-seed env) seed)
    (lambda (x seed env)
      (%%when (%%is? x jazz:Binding-Reference)
        (let* ((var (jazz:get-binding-reference-binding x))
               (sym (jazz:unwrap-syntactic-closure (jazz:get-lexical-binding-name var))))
          (let lp1 ((e env))
               (cond ((pair? e)
                      (let lp2 ((ls (%%car e)) (found? #f))
                           (cond ((pair? ls)
                                  (let* ((binding (%%car ls))
                                         (same? (%%eq? var binding)))
                                    (cond ((and binding
                                                (%%not same?)
                                                (%%is? binding jazz:Variable)
                                                (%%not (jazz:get-symbol-binding-gensym binding))
                                                (%%eq? sym
                                                       (jazz:unwrap-syntactic-closure
                                                         (jazz:get-lexical-binding-name binding))))
                                           ;; if we shadow an existing declaration, gensym a unique symbol for it
                                           #; ;; debug
                                           (if (jazz:testing?)
                                               (jazz:debug shadow: binding 'by var))
                                           (jazz:set-symbol-binding-gensym binding (jazz:generate-symbol (%%symbol->string sym)))))
                                    (lp2 (%%cdr ls) (or found? same?))))
                                 ((%%not found?)
                                  (lp1 (%%cdr e))))))))))
      seed)
    #f
    (%%list environment)))


(define (jazz:generate-module partial-form #!optional (emit? #t))
  (receive (name access modifiers dialect-name dialect-transformations dialect-src body) (jazz:parse-module partial-form)
    (if (and (jazz:requested-unit-name) (%%neq? name (jazz:requested-unit-name)))
        (jazz:error "Module at {s} is defining {s}" (jazz:requested-unit-name) name)
      (parameterize ((jazz:walk-context (jazz:new-walk-context #f name #f)))
        (let* ((dialect-invoice (and dialect-transformations (jazz:load-dialect-invoice dialect-name dialect-transformations)))
               (dialect (jazz:require-dialect dialect-name))
               (walker (jazz:dialect-walker dialect))
               (resume #f)
               (actual (jazz:get-catalog-entry name))
               (body (jazz:dialect-wrap dialect body))
               (declaration (jazz:call-with-catalog-entry-lock name
                              (lambda ()
                                (let ((declaration (jazz:walk-module-declaration walker actual name access modifiers dialect-name dialect-invoice dialect-src body)))
                                  (jazz:set-catalog-entry name declaration)
                                  declaration))))
               (environment (%%cons declaration (jazz:walker-environment walker)))
               (body (jazz:walk-namespace walker resume declaration environment body)))
          (jazz:validate-walk-problems walker)
          (jazz:rename-identifier-conflicts body environment)
          (jazz:set-namespace-declaration-body declaration body)
          (if emit?
              (let ((emitted (jazz:emit-declaration declaration walker resume '())))
                (jazz:validate-walk-problems walker)
                emitted)
            declaration))))))


(define (jazz:generate-script partial-form #!optional (emit? #t))
  (let ((name (gensym 'script)))
    (receive (dialect-name dialect-transformations dialect-src body) (jazz:parse-script partial-form)
      (parameterize ((jazz:walk-context (jazz:new-walk-context #f name #f)))
        (let* ((dialect-invoice (and dialect-transformations (jazz:load-dialect-invoice dialect-name dialect-transformations)))
               (dialect (jazz:require-dialect dialect-name))
               (walker (jazz:dialect-walker dialect))
               (resume #f)
               (body (jazz:dialect-wrap dialect body))
               (declaration (jazz:walk-module-declaration walker #f name 'public '() dialect-name dialect-invoice dialect-src body))
               (environment (%%cons declaration (jazz:walker-environment walker)))
               (body (jazz:walk-namespace walker resume declaration environment body)))
          (jazz:validate-walk-problems walker)
          (jazz:rename-identifier-conflicts body environment)
          (jazz:set-namespace-declaration-body declaration body)
          (if emit?
              (let ((emitted (jazz:emit-declaration declaration walker resume '())))
                (jazz:validate-walk-problems walker)
                emitted)
            declaration))))))


(define (jazz:cond-expand walker resume declaration form-src cont)
  (if (and (%%pair? (jazz:source-code form-src))
           (%%eq? (jazz:source-code (%%car (jazz:source-code form-src))) 'cond-expand))
      (jazz:process-cond-expand walker resume declaration form-src cont)
    (cont form-src #t)))


(define (jazz:process-cond-expand walker resume declaration form-src proc)
  (define (valid-feature-requirement clause)
    (if (%%pair? clause)
        (let ((requirement (jazz:desourcify-all (%%car clause))))
          (if (or (%%symbol? requirement)
                  (and (%%pair? requirement)
                       (%%memq (%%car requirement) '(and or not))))
              requirement
            #f))
      #f))
  
  (let iter ((scan (%%cdr (jazz:source-code form-src))))
       (if (%%null? scan)
           (jazz:walk-error walker resume declaration form-src "Unfulfilled cond-expand")
         (let ((clause (jazz:source-code (%%car scan))))
           (let ((feature-requirement (valid-feature-requirement clause)))
             (if (%%not feature-requirement)
                 (jazz:error "Ill-formed cond-expand clause: {s}" (jazz:desourcify-all clause))
               (if (or (jazz:feature-satisfied? feature-requirement)
                       (%%eq? feature-requirement 'else))
                   (if (%%null? (%%cdr clause))
                       (proc #f #f)
                     (proc `(begin ,@(%%cdr clause)) #t))
                 (iter (%%cdr scan)))))))))


(define (jazz:walk-namespace walker resume declaration environment form-list)
  (let ((queue (jazz:new-queue)))
    (define (walk forms)
      (for-each (lambda (form-src)
                  (continuation-capture
                    (lambda (resume)
                      (jazz:cond-expand walker resume declaration form-src
                        (lambda (expr expr?)
                          (%%when expr?
                            (cond ((jazz:include-form? expr)
                                   (walk (jazz:include-forms (jazz:include-filename expr))))
                                  (else
                                   (jazz:enqueue queue (jazz:walk walker resume declaration environment expr))))))))))
                forms))
    
    (walk form-list)
    (jazz:queue-list queue)))


(define (jazz:load-dialect-invoice dialect-name dialect-transformations)
  (if (%%not (%%symbol? dialect-name))
      (jazz:error "Dialect must be a symbol: {s}" dialect-name)
    (if (%%eq? dialect-name 'foundation.dialect)
        #f
      (jazz:new-import-invoice
        dialect-name
        (jazz:outline-module dialect-name)
        'syntax
        dialect-transformations))))


(define (jazz:emit-module-inclusions module-declaration walker resume environment)
  (define (find-name name lst)
    (if (%%null? lst)
        #f
      (if (%%eq? (jazz:get-lexical-binding-name (%%car lst)) name)
          #t
        (find-name name (%%cdr lst)))))
  
  (let ((queue (jazz:new-queue)))
    (define (collect-declarations declaration)
      (for-each collect-declarations (jazz:get-declaration-inclusions declaration))
      ;; This name based test if a quick solution to the complex problem of a declaration
      ;; replacing another one where there are references to the old one. Should we then just
      ;; replace or destructively modify the old one and what if the type of the one replacing
      ;; is incompatible...
      (%%when (%%not (find-name (jazz:get-lexical-binding-name declaration) (jazz:queue-list queue)))
        (jazz:enqueue queue declaration)))
    
    (for-each collect-declarations (jazz:get-module-declaration-inclusions module-declaration))
    (map (lambda (declaration)
           (jazz:expand-referenced-declaration declaration))
         (jazz:queue-list queue))))


(define (jazz:emit-module-literals module-declaration walker resume environment)
  (map (lambda (info)
         (let ((name (%%car info))
               (value (%%cdr info)))
           `(define ,name ,(jazz:sourcified-form (jazz:emit-expression value module-declaration walker resume '())))))
       (jazz:get-module-declaration-walker-literals module-declaration)))


(define (jazz:emit-module-variables module-declaration walker resume environment)
  (map (lambda (variable)
         (let ((symbol (%%car variable))
               (value (%%cdr variable)))
           `(jazz:define-variable ,symbol ,value)))
       (jazz:queue-list (jazz:get-module-declaration-walker-variables module-declaration))))


(define (jazz:emit-module-statics module-declaration walker resume environment)
  (map (lambda (static)
         (let ((symbol (%%car static))
               (expr (%%cdr static)))
           `(jazz:define-variable ,symbol ,(jazz:sourcified-form (jazz:emit-expression expr module-declaration walker resume '())))))
       (jazz:queue-list (jazz:get-module-declaration-walker-statics module-declaration))))


(define (jazz:emit-module-autoloads module-declaration walker resume environment)
  (let ((queue (jazz:new-queue))
        (locators (%%make-table test: eq?)))
    (for-each (lambda (autoload-declaration)
                (let ((referenced-declaration (jazz:resolve-binding autoload-declaration)))
                  (let ((locator (jazz:autoload-locator referenced-declaration)))
                    (%%when (%%not (%%table-ref locators locator #f))
                      (%%table-set! locators locator #t)
                      (jazz:enqueue queue
                        (let ((declaration-locator (jazz:get-declaration-locator (jazz:get-declaration-toplevel referenced-declaration)))
                              (reference-locator (jazz:sourcified-form (jazz:emit-binding-reference referenced-declaration module-declaration walker resume environment))))
                          (if (%%symbol? reference-locator)
                              `(jazz:define-variable ,locator
                                 (jazz:cache-autoload
                                   ',declaration-locator
                                   ',reference-locator
                                   ',locator))
                            `(define ,locator
                               (let ((loaded? #f))
                                 (lambda ()
                                   (if (%%not loaded?)
                                       (begin
                                         (jazz:load-unit ',(jazz:get-declaration-locator (jazz:get-declaration-toplevel referenced-declaration)))
                                         (set! loaded? #t)))
                                   ,(jazz:sourcified-form (jazz:emit-binding-reference referenced-declaration module-declaration walker resume environment))))))))))))
              (jazz:get-module-declaration-walker-autoloads module-declaration))
    (jazz:sort-list (lambda (x y) (%%string<? (%%symbol->string (%%cadr x)) (%%symbol->string (%%cadr y)))) (jazz:queue-list queue))))


(define (jazz:emit-module-registration declaration walker resume environment)
  `((jazz:register-module ',(jazz:get-lexical-binding-name declaration) ',(jazz:get-declaration-access declaration)
      ',(let ((queue (jazz:new-queue)))
          (for-each (lambda (module-invoice)
                      (%%when (%%neq? (jazz:get-module-invoice-phase module-invoice) 'syntax)
                        (let ((autoload (jazz:get-export-invoice-autoload module-invoice))
                              (symbols (jazz:get-export-invoice-symbols module-invoice)))
                          (%%when (and (%%not autoload) (%%not symbols))
                            (jazz:enqueue queue (jazz:get-module-invoice-name module-invoice))))))
                    (jazz:get-module-declaration-exports declaration))
          (jazz:sort-list (lambda (x y) (%%string<? (%%symbol->string x) (%%symbol->string y))) (jazz:queue-list queue)))
      ',(let ((walker (jazz:get-module-declaration-walker declaration))
              (queue (jazz:new-queue)))
          (jazz:iterate-table (jazz:get-public-lookup declaration)
            (lambda (name decl)
              (%%when (or (%%eq? (jazz:get-declaration-toplevel decl) declaration)
                          ;; quick hack
                          (%%is? decl jazz:Autoload-Declaration))
                (let ((export (jazz:runtime-export walker decl)))
                  (if export
                      (jazz:enqueue queue (%%cons name export)))))))
          (jazz:sort-list (lambda (x y) (%%string<? (%%symbol->string (%%car x)) (%%symbol->string (%%car y)))) (jazz:queue-list queue))))))


(jazz:define-method (jazz:runtime-export (jazz:Walker walker) declaration)
  (cond ((%%is? declaration jazz:Export-Declaration)
         (jazz:get-declaration-locator declaration))
        ((%%is? declaration jazz:Autoload-Declaration)
         (let ((referenced-declaration (jazz:resolve-binding declaration)))
           (%%cons (jazz:get-declaration-locator (jazz:get-declaration-toplevel referenced-declaration))
                   (jazz:get-declaration-locator referenced-declaration))))
        (else
         #f)))


(jazz:define-method (jazz:outline-generate (jazz:Module-Declaration declaration) output)
  (let ((declarations (jazz:outline-generate-filter-access (jazz:resolve-bindings (jazz:queue-list (jazz:get-namespace-declaration-children declaration))) (jazz:get-public-lookup declaration))))
    (let ((name (jazz:get-lexical-binding-name declaration))
          (dialect-name (jazz:get-module-declaration-dialect-name declaration))
          (dialect-invoice (jazz:get-module-declaration-dialect-invoice declaration)))
      (jazz:format output "(module {s} {s}" name (if (%%not dialect-invoice) (string->keyword (symbol->string dialect-name)) dialect-name)))
    #; ;; requires are not needed and create problems when outlined
    (for-each (lambda (require-invoice)
                (let ((name (jazz:get-require-invoice-name require-invoice))
                      (phase (jazz:get-require-invoice-phase require-invoice)))
                  (jazz:format output "{%}  {s}" `(require (,name ,@(jazz:outline-generate-phase-list phase))))))
              (jazz:get-module-declaration-requires declaration))
    (for-each (lambda (module-invoice)
                (let ((name (jazz:get-module-invoice-name module-invoice))
                      (phase (jazz:get-module-invoice-phase module-invoice))
                      (transformations (jazz:get-module-invoice-transformations module-invoice)))
                  (if name
                      (let ((autoload (jazz:get-export-invoice-autoload module-invoice)))
                        (if autoload
                            (let ((autoload-names
                                    (map (lambda (declaration-reference)
                                           (let ((name (jazz:reference-name (jazz:get-declaration-reference-name declaration-reference))))
                                             (if (jazz:get-autoload-reference-hubs? declaration-reference)
                                                 (%%list name)
                                               name)))
                                         autoload)))
                              (jazz:format output "{%}  {s}" `(export (,name (autoload ,@autoload-names)))))
                          (jazz:format output "{%}  {s}" `(export (,name ,@(jazz:outline-generate-phase-list phase) ,@(jazz:outline-generate-transformations transformations)))))))))
              (jazz:get-module-declaration-exports declaration))
    (for-each (lambda (module-invoice)
                (let ((name (jazz:get-module-invoice-name module-invoice))
                      (phase (jazz:get-module-invoice-phase module-invoice))
                      (transformations (jazz:get-module-invoice-transformations module-invoice)))
                  (if name
                      (jazz:format output "{%}  {s}" `(import (,name ,@(jazz:outline-generate-phase-list phase) ,@(jazz:outline-generate-transformations transformations)))))))
              (jazz:get-module-declaration-imports declaration))
    (for-each (lambda (decl)
                (jazz:outline-generate decl output))
              declarations)
    (jazz:format output "){%}")))


(define (jazz:iterate-module-declaration module-declaration proc)
  (jazz:tree-fold
    module-declaration
    (lambda (obj seed env)
      (proc obj)
      env)
    (lambda (obj seed child-seed env)
      seed)
    (lambda (obj seed env)
      (proc obj)
      seed)
    #f
    #f))


;;;
;;;; Require Invoice
;;;


(jazz:define-class jazz:Require-Invoice jazz:Object (constructor: jazz:allocate-require-invoice)
  ((name  getter: generate)
   (phase getter: generate)))


(define (jazz:new-require-invoice name phase)
  (jazz:allocate-require-invoice name phase))


;;;
;;;; Module Invoice
;;;


(jazz:define-class jazz:Module-Invoice jazz:Object ()
  ((name            getter: generate)
   (module          getter: generate)
   (phase           getter: generate)
   (transformations getter: generate)))


(define (jazz:find-module-invoice invoices target)
  (let ((target-name (jazz:get-module-invoice-name target))
        (target-phase (jazz:get-module-invoice-phase target)))
    (jazz:find-if (lambda (invoice)
                    (and (%%eq? (jazz:get-module-invoice-name invoice) target-name)
                         (%%eq? (jazz:get-module-invoice-phase invoice) target-phase)))
                  invoices)))


;;;
;;;; Export Invoice
;;;


(jazz:define-class jazz:Export-Invoice jazz:Module-Invoice (constructor: jazz:allocate-export-invoice)
  ((autoload getter: generate setter: generate)
   (symbols  getter: generate setter: generate)))


(define (jazz:new-export-invoice name module phase transformations autoload symbols)
  (jazz:allocate-export-invoice name module phase transformations autoload symbols))


;;;
;;;; Import Invoice
;;;


(jazz:define-class jazz:Import-Invoice jazz:Module-Invoice (constructor: jazz:allocate-import-invoice)
  ((hit? getter: generate setter: generate)))


(define (jazz:new-import-invoice name module phase transformations)
  (jazz:allocate-import-invoice name module phase transformations #f))


;;;
;;;; Export
;;;


(jazz:define-class jazz:Export-Declaration jazz:Declaration (constructor: jazz:allocate-export-declaration)
  ((symbol getter: generate)))


(define (jazz:new-export-declaration name type access compatibility modifiers attributes parent symbol)
  (let ((new-declaration (jazz:allocate-export-declaration name type #f access compatibility modifiers attributes #f parent #f #f #f symbol)))
    (jazz:setup-declaration new-declaration)
    new-declaration))


(jazz:define-method (jazz:walk-binding-validate-call (jazz:Export-Declaration declaration) walker resume source-declaration operator arguments form-src)
  (jazz:unspecified))


(jazz:define-method (jazz:emit-declaration (jazz:Export-Declaration declaration) walker resume environment)
  (let ((name (jazz:get-lexical-binding-name declaration))
        (symbol (jazz:get-export-declaration-symbol declaration))
        (parent (jazz:get-declaration-parent declaration)))
    (%%assert (%%is? parent jazz:Module-Declaration))
    (if (jazz:get-generate? 'register)
        `(jazz:register-native ',(jazz:get-lexical-binding-name parent) ',name ',symbol)
      `(begin))))


(jazz:define-method (jazz:emit-binding-reference (jazz:Export-Declaration declaration) source-declaration walker resume environment)
  (jazz:new-code
    (jazz:get-export-declaration-symbol declaration)
    jazz:Any
    #f))


(define (jazz:walk-export-declaration walker resume declaration environment form-src)
  (define (walk-module-export export)
    (receive (module-name module-phase module-autoload module-transformations) (jazz:parse-module-invoice walker resume declaration form-src export)
      (let ((module-reference (jazz:new-module-reference module-name #f)))
        (jazz:new-export-invoice module-name
                                 module-reference
                                 module-phase
                                 module-transformations
                                 (if (%%not module-autoload)
                                     #f
                                   (map (lambda (clause)
                                          (if (%%symbol? clause)
                                              (jazz:new-autoload-reference clause #f #f #f)
                                            (%%assert (and (%%pair? clause)
                                                           (%%null? (%%cdr clause))
                                                           (%%symbol? (%%car clause)))
                                              (jazz:new-autoload-reference (%%car clause) #f #f #t))))
                                        module-autoload))
                                 #f))))
  
  (define (walk-exports exports)
    (let ((partition (jazz:partition exports symbol? assv)))
      (let ((symbols-exports (%%assq #t partition))
            (module-exports (%%assq #f partition)))
        (let ((symbol-references (if symbols-exports
                                     (map (lambda (symbol) (jazz:new-export-reference symbol #f #f)) (%%cdr symbols-exports))
                                   #f)))
          (%%append (if symbols-exports
                        (%%list (jazz:new-export-invoice #f #f #f #f #f symbol-references))
                      '())
                    (if module-exports
                        (map (lambda (export)
                               (walk-module-export export))
                             (%%cdr module-exports))
                      '()))))))
  
  (let ((form (%%desourcify form-src)))
    (let ((module-declaration (jazz:get-declaration-toplevel declaration)))
      (let ((export-invoices (walk-exports (jazz:filter-invoices (%%cdr form)))))
        (for-each (lambda (export-invoice)
                    (jazz:add-module-export walker resume declaration form-src module-declaration export-invoice))
                  export-invoices)))))


(define (jazz:walk-export walker resume declaration environment form-src)
  (jazz:new-begin #f '()))


(jazz:define-method (jazz:outline-extract (jazz:Export-Declaration declaration) meta)
  `(native ,@(jazz:outline-generate-modifiers declaration) ,(jazz:get-export-declaration-symbol declaration)))



;;;
;;;; Export Syntax
;;;


(jazz:define-class jazz:Export-Syntax-Declaration jazz:Declaration (constructor: jazz:allocate-export-syntax-declaration)
  ((symbol getter: generate)))


(define (jazz:new-export-syntax-declaration name type access compatibility modifiers attributes parent symbol)
  (let ((new-declaration (jazz:allocate-export-syntax-declaration name type #f access compatibility modifiers attributes #f parent #f #f #f symbol)))
    (jazz:setup-declaration new-declaration)
    new-declaration))


(jazz:define-method (jazz:walk-binding-validate-call (jazz:Export-Syntax-Declaration declaration) walker resume source-declaration operator arguments form-src)
  (jazz:unspecified))


(jazz:define-method (jazz:emit-declaration (jazz:Export-Syntax-Declaration declaration) walker resume environment)
  `(begin))


(jazz:define-method (jazz:emit-binding-reference (jazz:Export-Syntax-Declaration declaration) source-declaration walker resume environment)
  (jazz:new-code
    (jazz:get-export-syntax-declaration-symbol declaration)
    jazz:Any
    #f))


(jazz:define-method (jazz:outline-extract (jazz:Export-Syntax-Declaration declaration) meta)
  `(native-syntax ,@(jazz:outline-generate-modifiers declaration) ,(jazz:get-export-syntax-declaration-symbol declaration)))


;;;
;;;; Autoload
;;;


(jazz:define-class jazz:Autoload-Declaration jazz:Declaration (constructor: jazz:allocate-autoload-declaration)
  ((module           getter: generate)
   (exported-module  getter: generate)
   (declaration      getter: generate setter: generate)))


(define (jazz:new-autoload-declaration name type parent module-declaration exported-module)
  (let ((new-declaration (jazz:allocate-autoload-declaration name type #f 'public 'uptodate '() '() #f parent #f #f #f module-declaration exported-module #f)))
    (jazz:setup-declaration new-declaration)
    new-declaration))


(jazz:define-method (jazz:of-subtype? (jazz:Autoload-Declaration declaration) subtype)
  ;; not sure calling resolve here is correct
  (jazz:of-subtype? (jazz:resolve-binding declaration)
                    ;; hack not sure this is the right way
                    (jazz:resolve-type subtype)))


(jazz:define-method (jazz:specifiable? (jazz:Autoload-Declaration declaration))
  ;; quick hack
  #t)


(jazz:define-method (jazz:resolve-binding (jazz:Autoload-Declaration declaration))
  (or (jazz:get-autoload-declaration-declaration declaration)
      (let* ((exported-module (jazz:resolve-reference (jazz:get-autoload-declaration-exported-module declaration) (jazz:get-autoload-declaration-module declaration)))
             (name (jazz:get-lexical-binding-name declaration))
             (decl (jazz:lookup-declaration exported-module name jazz:public-access declaration)))
        (%%assertion decl (jazz:error "Unable to find autoload {s} in unit {s}" name (jazz:get-declaration-locator exported-module))
          (jazz:set-autoload-declaration-declaration declaration decl)
          decl))))


(jazz:define-method (jazz:walk-binding-validate-call (jazz:Autoload-Declaration declaration) walker resume source-declaration operator arguments form-src)
  (jazz:walk-binding-validate-call (jazz:resolve-binding declaration) walker resume source-declaration operator arguments form-src))


(jazz:define-method (jazz:emit-binding-reference (jazz:Autoload-Declaration declaration) source-declaration walker resume environment)
  (let ((referenced-declaration (jazz:resolve-binding declaration)))
    (jazz:new-code
      `(,(jazz:autoload-locator referenced-declaration))
      referenced-declaration
      #f)))


(jazz:define-method (jazz:emit-specifier (jazz:Autoload-Declaration declaration))
  (jazz:type->specifier declaration #t))


;; this heuristic used because we cannot call jazz:resolve-binding at various points
;; is not 100% correct if the autoload was obtained through a reexported module...
(define (jazz:autoload-declaration-locator-heuristic declaration)
  (jazz:compose-reference (jazz:get-declaration-reference-name (jazz:get-autoload-declaration-exported-module declaration)) (jazz:get-lexical-binding-name declaration)))


(define (jazz:autoload-locator referenced-declaration)
  (%%string->symbol (%%string-append (%%symbol->string (jazz:get-declaration-locator referenced-declaration))
                                     ":autoload")))


;;;
;;;; Literal
;;;


(jazz:define-class jazz:Literal-Class jazz:Class (metaclass: jazz:Class)
  ())


(jazz:define-class jazz:Literal jazz:Object (constructor: jazz:allocate-literal metaclass: jazz:Literal-Class)
  ((name      getter: generate)
   (arguments getter: generate)))


(jazz:define-variable-override jazz:new-literal
  (lambda (name arguments)
    (jazz:allocate-literal name arguments)))


(define (jazz:walk-literal-constant walker resume declaration environment literal)
  (let ((module-declaration (jazz:get-declaration-toplevel declaration)))
    (define (walk-literal walker resume declaration literal)
      (let ((environment (%%cons module-declaration (jazz:walker-environment walker))))
        (jazz:walk walker resume module-declaration environment
          (let ((name (jazz:get-literal-name literal))
                (arguments (jazz:get-literal-arguments literal)))
            (let ((constructor-reference (%%car (jazz:require-literal-constructor (%%desourcify name)))))
              `(,constructor-reference ,@(map (lambda (arg)
                                                `(quote ,arg))
                                              arguments)))))))

    ;; calling jazz:get-registered-literal to only register when not already there
    ;; doesnt work directly because some literals are interned and thus can be shared
    (let ((locator (jazz:generate-global-symbol "lit")))
      ;; it is important to register before walking so subliterals come before us
      (let ((info (%%cons locator #f)))
        (jazz:set-module-declaration-walker-literals module-declaration (%%cons info (jazz:get-module-declaration-walker-literals module-declaration)))
        (%%set-cdr! info (walk-literal walker resume declaration literal)))
      ;; this way of getting a reference to the literal's class is a quick solution
      (let ((literal-type (%%desourcify (jazz:get-literal-name literal))))
        (jazz:new-constant locator (jazz:lookup-reference walker resume declaration environment literal-type))))))


(define (jazz:make-symbolic-chars alist)
  (%%list->table
    (map (lambda (pair)
           (%%cons (%%car pair) (integer->char (%%cdr pair))))
         alist)
    test: eq?))


(define jazz:Symbolic-Chars
  (jazz:make-symbolic-chars
    '((zero              . #x00)
      (home              . #x01)
      (enter             . #x03)
      (end               . #x04)
      (info              . #x05)
      (backspace         . #x08)
      (tab               . #x09)
      (linefeed          . #x0A)
      (page-up           . #x0B)
      (page-down         . #x0C)
      (return            . #x0D)
      (escape            . #x1B)
      (left-arrow        . #x1C)
      (right-arrow       . #x1D)
      (up-arrow          . #x1E)
      (down-arrow        . #x1F)
      (space             . #x20)
      (exclamation-point . #x21)
      (double-quote      . #x22)
      (sharp             . #x23)
      (ampersand         . #x26)
      (quote             . #x27)
      (open-parenthesis  . #x28)
      (close-parenthesis . #x29)
      (times             . #x2A)
      (plus              . #x2B)
      (comma             . #x2C)
      (minus             . #x2D)
      (period            . #x2E)
      (slash             . #x2F)
      (colon             . #x3A)
      (semi-colon        . #x3B)
      (question-mark     . #x3F)
      (at                . #x40)
      (open-bracket      . #x5B)
      (backslash         . #x5C)
      (close-bracket     . #x5D)
      (exponential       . #x5E)
      (underscore        . #x5F)
      (backquote         . #x60)
      (open-brace        . #x7B)
      (close-brace       . #x7D)
      (delete            . #x7F)
      (copyright         . #xA9))))


(define (jazz:symbolic-char name)
  (%%table-ref jazz:Symbolic-Chars name #f))


;;;
;;;; Void
;;;


(jazz:define-class jazz:Void-Class jazz:Class (metaclass: jazz:Class)
  ())


(jazz:define-method (jazz:of-subtype? (jazz:Void-Class type) subtype)
  #f)


(jazz:define-method (jazz:emit-specifier (jazz:Void-Class type))
  'void)


(jazz:define-class jazz:Void jazz:Type (metaclass: jazz:Void-Class)
  ())


;;;
;;;; Opt
;;;


(jazz:define-class jazz:Opt-Type jazz:Type (metaclass: jazz:Class constructor: jazz:allocate-opt-type)
  ((type getter: generate)))


(define (jazz:new-opt-type type)
  (jazz:allocate-opt-type type))


(jazz:define-method (jazz:emit-specifier (jazz:Opt-Type type))
  (let ((type-specifier (jazz:type->specifier (jazz:get-opt-type-type type) #t)))
    (%%string->symbol (%%string-append "opt<" (%%symbol->string type-specifier) ">"))))


;;;
;;;; Key
;;;


(jazz:define-class jazz:Key-Type jazz:Type (metaclass: jazz:Class constructor: jazz:allocate-key-type)
  ((key  getter: generate)
   (type getter: generate)))


(define (jazz:new-key-type key type)
  (jazz:allocate-key-type key type))


(jazz:define-method (jazz:emit-specifier (jazz:Key-Type type))
  (let ((key (jazz:get-key-type-key type))
        (type-specifier (jazz:type->specifier (jazz:get-key-type-type type) #t)))
    (%%string->symbol (%%string-append "key<" (%%keyword->string key) ":" (%%symbol->string type-specifier) ">"))))


;;;
;;;; Rest
;;;


(jazz:define-class jazz:Rest-Type jazz:Type (metaclass: jazz:Class constructor: jazz:allocate-rest-type)
  ((type getter: generate)))


(define (jazz:new-rest-type type)
  (jazz:allocate-rest-type type))


(jazz:define-method (jazz:emit-specifier (jazz:Rest-Type type))
  (let ((type-specifier (jazz:type->specifier (jazz:get-rest-type-type type))))
    (%%string->symbol (%%string-append (%%symbol->string type-specifier) "*"))))


;;;
;;;; Function
;;;


;; should probably be unified with the Signature class


(jazz:define-class jazz:Function-Type jazz:Type (metaclass: jazz:Class constructor: jazz:allocate-function-type)
  ((mandatory  getter: generate)
   (positional getter: generate)
   (optional   getter: generate)
   (named      getter: generate)
   (rest       getter: generate)
   (result     getter: generate)))


(define (jazz:new-function-type positional optional named rest result)
  (let ((mandatory (%%length positional)))
    (jazz:allocate-function-type mandatory positional optional named rest result)))


(jazz:define-method (jazz:of-subtype? (jazz:Function-Type type) subtype)
  (or (jazz:of-subtype? jazz:Procedure subtype)
      (%%eq? (%%get-object-class subtype) jazz:Function-Type)))


(jazz:define-method (jazz:emit-specifier (jazz:Function-Type type))
  (let ((output (open-output-string)))
    (let ((first? #t))
      (for-each (lambda (type)
                  (if first?
                      (set! first? #f)
                    (write-char #\^ output))
                  (display (jazz:type->specifier type) output))
                (jazz:get-function-type-positional type))
      (let ((rest (jazz:get-function-type-rest type)))
        (%%when rest
          (%%when (%%not first?)
            (write-char #\^ output))
          (display (jazz:type->specifier rest) output))))
    (write-char #\: output)
    (display (jazz:type->specifier (jazz:get-function-type-result type)) output)
    (%%string->symbol (get-output-string output))))


(jazz:define-method (jazz:emit-cast (jazz:Function-Type type) value source-declaration walker resume environment)
  `(if (%%procedure? ,value)
       ,value
     (jazz:type-error ,value jazz:Procedure)))


(define (jazz:only-positional-function-type? function-type)
  (and (%%null? (jazz:get-function-type-optional function-type))
       (%%null? (jazz:get-function-type-named function-type))
       (%%not (jazz:get-function-type-rest function-type))))


(define (jazz:typed-function-type? function-type #!optional (skip-first-positional? #f))
  (define (typed? type)
    (%%neq? type jazz:Any))
  
  (let ((positional (jazz:get-function-type-positional function-type))
        (optional (jazz:get-function-type-optional function-type))
        (named (jazz:get-function-type-named function-type))
        (rest (jazz:get-function-type-rest function-type)))
    (or (jazz:some? typed? (if skip-first-positional? (%%cdr positional) positional))
        (jazz:some? typed? optional)
        (jazz:some? typed? named)
        (and rest (typed? rest)))))


;;;
;;;; Category
;;;


;; first draft. this type is used to support specializing new and the like


(jazz:define-class jazz:Category-Type jazz:Type (metaclass: jazz:Class constructor: jazz:allocate-category-type)
  ((declaration getter: generate)))


(define (jazz:new-category-type declaration)
  (jazz:allocate-category-type declaration))


;; quicky solution to stop casts on this type
(jazz:define-method (jazz:of-subtype? (jazz:Category-Type type) subtype)
  #t)


(jazz:define-method (jazz:emit-cast (jazz:Category-Type type) value source-declaration walker resume environment)
  value)


(jazz:define-method (jazz:emit-specifier (jazz:Category-Type type))
  (let ((output (open-output-string)))
    (display "category" output)
    (write-char #\< output)
    (display (jazz:type->specifier (jazz:get-category-type-declaration type) #t) output)
    (write-char #\> output)
    (%%string->symbol (get-output-string output))))


;;;
;;;; Values
;;;


(jazz:define-class jazz:Values-Type jazz:Type (metaclass: jazz:Class constructor: jazz:allocate-values-type)
  ((types getter: generate)))


(define (jazz:new-values-type types)
  (jazz:allocate-values-type types))


(jazz:define-method (jazz:emit-specifier (jazz:Values-Type type))
  (let ((output (open-output-string)))
    (display "values" output)
    (write-char #\< output)
    (let ((first? #t))
      (for-each (lambda (type)
                  (if first?
                      (set! first? #f)
                    (write-char #\/ output))
                  (display (jazz:type->specifier type) output))
                (jazz:get-values-type-types type)))
    (write-char #\> output)
    (%%string->symbol (get-output-string output))))


;;;
;;;; Restriction
;;;


(jazz:define-class jazz:Restriction-Type jazz:Type (metaclass: jazz:Class constructor: jazz:allocate-restriction-type)
  ((base getter: generate)
   (type getter: generate)))


(define (jazz:new-restriction-type base type)
  (jazz:allocate-restriction-type base type))


;;;
;;;; Complement
;;;


(jazz:define-class jazz:Complement-Type jazz:Type (metaclass: jazz:Class constructor: jazz:allocate-complement-type)
  ((type getter: generate)))


(define (jazz:new-complement-type type)
  (jazz:allocate-complement-type type))


;;;
;;;; Union
;;;


(jazz:define-class jazz:Union-Type jazz:Type (metaclass: jazz:Class constructor: jazz:allocate-union-type)
  ((types getter: generate)))


(define (jazz:new-union-type types)
  (jazz:allocate-union-type types))


;;;
;;;; Template
;;;


;; future work. just here to make sure specifier syntax can express them


(jazz:define-class jazz:Template-Type jazz:Type (metaclass: jazz:Class constructor: jazz:allocate-template-type)
  ((class getter: generate)
   (types getter: generate)))


(define (jazz:new-template-type class types)
  (jazz:allocate-template-type class types))


(jazz:define-method (jazz:emit-specifier (jazz:Template-Type type))
  (let ((output (open-output-string)))
    (display (jazz:type->specifier (jazz:get-template-type-class type)) output)
    (write-char #\< output)
    (let ((first? #t))
      (for-each (lambda (type)
                  (if first?
                      (set! first? #f)
                    (write-char #\/ output))
                  (display (jazz:type->specifier type) output))
                (jazz:get-template-type-types type)))
    (write-char #\> output)
    (%%string->symbol (get-output-string output))))


;;;
;;;; Nillable
;;;


(jazz:define-class jazz:Nillable-Type jazz:Type (metaclass: jazz:Class constructor: jazz:allocate-nillable-type)
  ((type getter: generate)))


(define (jazz:new-nillable-type type)
  (jazz:allocate-nillable-type type))


(jazz:define-method (jazz:of-subtype? (jazz:Nillable-Type type) subtype)
  (or (jazz:of-subtype? jazz:Boolean subtype)
      (jazz:of-subtype? (jazz:get-nillable-type-type type) subtype)
      ;; quick solution to be thought through
      (and (%%class-is? subtype jazz:Nillable-Type)
           (%%eq? (jazz:get-nillable-type-type type) (jazz:get-nillable-type-type subtype)))))


(jazz:define-method (jazz:emit-specifier (jazz:Nillable-Type type))
  (let ((type-specifier (jazz:type->specifier (jazz:get-nillable-type-type type) #t)))
    (%%string->symbol (%%string-append (%%symbol->string type-specifier) "+"))))


(jazz:define-method (jazz:emit-test (jazz:Nillable-Type type) value source-declaration walker resume environment)
  `(or (%%not ,value)
       ,(jazz:emit-test (jazz:get-nillable-type-type type) value source-declaration walker resume environment)))


(jazz:define-method (jazz:emit-cast (jazz:Nillable-Type type) value source-declaration walker resume environment)
  `(if ,(jazz:emit-test type value source-declaration walker resume environment)
       ,value
     (jazz:type-error ,value ',(jazz:emit-specifier type))))


;;;
;;;; Object
;;;


(define jazz:object-declaration? #f)

(set! jazz:object-declaration?
      (lambda (type)
        #f))


;;;
;;;; Any
;;;


(jazz:define-class jazz:Any-Class jazz:Class (metaclass: jazz:Class)
  ())


(jazz:define-method (jazz:of-subtype? (jazz:Any-Class type) subtype)
  #t)


(jazz:define-method (jazz:emit-specifier (jazz:Any-Class type))
  'any)


(jazz:define-method (jazz:emit-cast (jazz:Any-Class type) value source-declaration walker resume environment)
  value)


(jazz:define-class jazz:Any jazz:Type (metaclass: jazz:Any-Class)
  ())


;;;
;;;; Cast
;;;

;; Really not sure what is the right place to put the jazz:resolve-type
;; As a first try lets put them here at every type check


;; this should not have to be necessary for the code type but it seems it can also be false
(define (jazz:resolve-type-safe type)
  (and type
       (jazz:resolve-type type)))


(define (jazz:emit-type-cast code type source source-declaration walker resume environment)
  (let ((code-type (jazz:resolve-type-safe (jazz:get-code-type code)))
        (expected-type (jazz:resolve-type-safe type)))
    (cond ((or (%%not expected-type)
               (%%eq? expected-type jazz:Void)
               (%%subtype? code-type expected-type))
           #; ;; too much at the moment
           (%%when (and (or (jazz:reporting?) (jazz:warnings?)) (jazz:get-warn? 'optimizations))
             (jazz:warning "Warning: In {a}{a}: Redundant cast"
                           (jazz:get-declaration-locator source-declaration)
                           (jazz:present-expression-location source #f)))
           (jazz:sourcified-form code))
          ;; fixnum to flonum
          ((and (%%subtype? expected-type jazz:Flonum)
                (%%subtype? code-type jazz:Fixnum))
           (let ((code-emit (jazz:sourcified-form code)))
             (if (%%fixnum? (jazz:source-code code-emit))
                 (jazz:sourcify-if (%%fixnum->flonum (jazz:source-code code-emit)) (jazz:get-code-source code))
               `(%%fixnum->flonum ,(jazz:sourcified-form code)))))
          ;; ratnum to flonum
          ((and (%%subtype? expected-type jazz:Flonum)
                (%%subtype? code-type jazz:Ratnum))
           (let ((code-emit (jazz:sourcified-form code)))
             (if (%%ratnum? (jazz:source-code code-emit))
                 (jazz:sourcify-if (%%ratnum->flonum (jazz:source-code code-emit)) (jazz:get-code-source code))
               `(%%ratnum->flonum ,(jazz:sourcified-form code)))))
          ;; flonum to flovec
          ((and (%%subtype? expected-type jazz:Flovec)
                (%%subtype? code-type jazz:Flonum))
           (jazz:sourcified-form code))
          (else
           #; ;; incompatible cast test
           (%%unless (%%eq? code-type jazz:Any)
             (pp (jazz:format "Casting {a} to incompatible type {a}" code-type expected-type) (current-output-port)))
           (if (jazz:get-check? 'types)
               (let ((value (jazz:generate-symbol "val")))
                 (jazz:simplify-let
                   `(let ((,value ,(jazz:simplify-let `(let () ,(jazz:simplify-begin (jazz:sourcified-form code))))))
                      ,(jazz:emit-cast type value source-declaration walker resume environment))))
             (begin
               (%%when (%%subtype? expected-type jazz:Flonum)
                 (jazz:unsafe-warning "Unsafe: In {a}{a}: Unchecked untyped cast <fl> {a}"
                                      (jazz:get-declaration-locator source-declaration)
                                      (jazz:present-expression-location source #f)
                                      (jazz:desourcify-all (jazz:get-code-source code))))
               (jazz:sourcified-form code)))))))


(define (jazz:emit-implicit-cast code type)
  (let ((code-type (jazz:resolve-type-safe (jazz:get-code-type code)))
        (type (jazz:resolve-type-safe type)))
    (cond ((or (%%not type)
               (%%eq? type jazz:Void)
               (%%subtype? code-type type))
           (jazz:sourcified-form code))
          ;; fixnum to flonum
          ((and (%%subtype? type jazz:Flonum)
                (%%subtype? code-type jazz:Fixnum))
           (let ((code-emit (jazz:sourcified-form code)))
             (if (%%fixnum? (jazz:source-code code-emit))
                 (jazz:sourcify (%%fixnum->flonum (jazz:source-code code-emit)) code-emit)
               `(%%fixnum->flonum ,(jazz:sourcified-form code)))))
          ;; ratnum to flonum
          ((and (%%subtype? type jazz:Flonum)
                (%%subtype? code-type jazz:Ratnum))
           (let ((code-emit (jazz:sourcified-form code)))
             (if (%%ratnum? (jazz:source-code code-emit))
                 (jazz:sourcify (%%ratnum->flonum (jazz:source-code code-emit)) code-emit)
               `(%%ratnum->flonum ,(jazz:sourcified-form code)))))
          (else
           (jazz:sourcified-form code)))))


(define (jazz:emit-parameter-cast code type source-declaration walker resume environment)
  (if (jazz:get-check? 'types)
      (let ((type (jazz:resolve-type-safe type)))
        (if (or (%%not type) (%%eq? type jazz:Any) (%%object-class? type) (jazz:object-declaration? type))
            #f
          (let ((parameter (jazz:sourcified-form code)))
            (let ((cast (jazz:emit-cast type parameter source-declaration walker resume environment)))
              (if cast
                  (%%list parameter cast)
                #f)))))
    #f))


(define (jazz:emit-return-cast code type source source-declaration walker resume environment)
  (jazz:emit-type-cast code type source source-declaration walker resume environment))


;;;
;;;; Specifier
;;;


;; <any>
;; <Point+>                            ;; Nillable
;; <category<Cell>>                    ;; Category
;; <fx^fx^opt<fx>^key<k:fx>^fx*:pair>  ;; Function with positional, optional, named and rest parameters
;; <vector<fx>>                        ;; Template
;; <values<fx/fl>>                     ;; Values
;; <Object>
;; <Point>
;; <pair>
;; <fx>
;; <void>


;; interning the type is a quick solution to
;; types being compared using eq? and also saves
;; some memory because of the size of classes

(define jazz:fixed-types
  (%%make-table test: equal?))

(define (jazz:new-fixed-type class size)
  (let ((key (%%cons class size)))
    (or (%%table-ref jazz:fixed-types key #f)
        (let ((metaclass (%%get-object-class class))
              (name (%%get-category-identifier class)))
          (let ((type-class (jazz:new-type-class metaclass name class size)))
            (%%table-set! jazz:fixed-types key type-class)
            type-class)))))


(define jazz:fixed-specifiers
  (%%make-table test: eq?))

(define jazz:fixed-classes
  (%%make-table test: eq?))

(define jazz:fixed-constructors
  (%%make-table test: eq?))

(define jazz:fixed-makers
  (%%make-table test: eq?))


(define (jazz:setup-fixed-metadata specifier class fixed-class fixed-metaclass constructor maker)
  (%%table-set! jazz:fixed-specifiers specifier fixed-class)
  (%%table-set! jazz:fixed-classes class fixed-metaclass)
  (%%table-set! jazz:fixed-constructors constructor fixed-class)
  (%%table-set! jazz:fixed-makers maker fixed-class))


(define (jazz:specifier->fixed-class specifier)
  (%%table-ref jazz:fixed-specifiers specifier #f))

(define (jazz:class->fixed-metaclass class)
  (%%table-ref jazz:fixed-classes class #f))

(define (jazz:constructor->fixed-class locator)
  (%%table-ref jazz:fixed-constructors locator #f))

(define (jazz:maker->fixed-class locator)
  (%%table-ref jazz:fixed-makers locator #f))


(jazz:setup-fixed-metadata 'vector    jazz:Vector    jazz:FixedVector    jazz:FixedVector-Class    'scheme.runtime:vector             'scheme.runtime:make-vector)
(jazz:setup-fixed-metadata 's8vector  jazz:S8Vector  jazz:FixedS8Vector  jazz:FixedS8Vector-Class  'gambit.language.runtime:s8vector  'gambit.language.runtime:make-s8vector)
(jazz:setup-fixed-metadata 'u8vector  jazz:U8Vector  jazz:FixedU8Vector  jazz:FixedU8Vector-Class  'gambit.language.runtime:u8vector  'gambit.language.runtime:make-u8vector)
(jazz:setup-fixed-metadata 's16vector jazz:S16Vector jazz:FixedS16Vector jazz:FixedS16Vector-Class 'gambit.language.runtime:s16vector 'gambit.language.runtime:make-s16vector)
(jazz:setup-fixed-metadata 'u16vector jazz:U16Vector jazz:FixedU16Vector jazz:FixedU16Vector-Class 'gambit.language.runtime:u16vector 'gambit.language.runtime:make-u16vector)
(jazz:setup-fixed-metadata 's32vector jazz:S32Vector jazz:FixedS32Vector jazz:FixedS32Vector-Class 'gambit.language.runtime:s32vector 'gambit.language.runtime:make-s32vector)
(jazz:setup-fixed-metadata 'u32vector jazz:U32Vector jazz:FixedU32Vector jazz:FixedU32Vector-Class 'gambit.language.runtime:u32vector 'gambit.language.runtime:make-u32vector)
(jazz:setup-fixed-metadata 's64vector jazz:S64Vector jazz:FixedS64Vector jazz:FixedS64Vector-Class 'gambit.language.runtime:s64vector 'gambit.language.runtime:make-s64vector)
(jazz:setup-fixed-metadata 'u64vector jazz:U64Vector jazz:FixedU64Vector jazz:FixedU64Vector-Class 'gambit.language.runtime:u64vector 'gambit.language.runtime:make-u64vector)
(jazz:setup-fixed-metadata 'f32vector jazz:F32Vector jazz:FixedF32Vector jazz:FixedF32Vector-Class 'gambit.language.runtime:f32vector 'gambit.language.runtime:make-f32vector)
(jazz:setup-fixed-metadata 'f64vector jazz:F64Vector jazz:FixedF64Vector jazz:FixedF64Vector-Class 'gambit.language.runtime:f64vector 'gambit.language.runtime:make-f64vector)


(define (jazz:parse-specifier lst proc)
  (if (and (%%pair? lst) (jazz:specifier? (jazz:source-code (%%car lst))))
      (proc (jazz:source-code (%%car lst)) (%%car lst) (%%cdr lst))
    (proc #f #f lst)))


(define (jazz:walk-specifier walker resume declaration environment specifier)
  (let ((string (%%symbol->string specifier)))
    (let ((input (open-input-string string))
          (at 0))
      (define (ill-formed message)
        (let ((error-message (jazz:format "Ill-formed specifier {s} : at {a} : {a}" specifier (%%substring string 0 at) message)))
          (if (%%not walker)
              (jazz:error "{a}" error-message)
            (jazz:walk-error walker resume declaration #f error-message))))
      
      (define (peekc)
        (peek-char input))
      
      (define (readc)
        (let ((c (read-char input)))
          (set! at (%%fx+ at 1))
          c))
      
      (define (consume c)
        (if (%%not (%%eqv? (readc) c))
            (ill-formed (jazz:format "{s} expected" c))))
      
      (define (lookup-type name)
        (let ((type (or (jazz:lookup-primitive-type name)
                        (jazz:lookup-reference walker resume declaration environment name)
                        (ill-formed (jazz:format "{s} not found" name)))))
          (%%assertion (jazz:specifiable? type) (jazz:error "Invalid specifier: {s}" name)
            type)))

      (define (parse-until separator terminator)
        (let ((queue (jazz:new-queue)))
          (let iter ()
               (if (%%eqv? (peekc) terminator)
                   (begin
                     (readc)
                     (jazz:queue-list queue))
                 (begin
                   (jazz:enqueue queue (parse #t))
                   (let ((next (peekc)))
                     (cond ((%%eqv? next separator)
                            (readc)
                            (iter))
                           ((%%eqv? next terminator)
                            (iter))
                           (else
                            (ill-formed (jazz:format "{s} terminator expected" terminator))))))))))
      
      (define (parse-name)
        (let ((output (open-output-string)))
          (let iter ()
               (let ((c (peekc)))
                 (if (or (%%eof-object? c)
                         (%%eqv? c #\<)
                         (%%eqv? c #\>)
                         (%%eqv? c #\^)
                         (%%eqv? c #\*)
                         (%%eqv? c #\#)
                         (%%eqv? c #\:)
                         (%%eqv? c #\/)
                         (%%eqv? c #\+))
                     (%%string->symbol (get-output-string output))
                   (begin
                     (readc)
                     (write-char c output)
                     (iter)))))))
      
      (define (parse-string)
        (let ((output (open-output-string)))
          (let iter ()
               (let ((c (peekc)))
                 (if (or (%%eqv? c #\+)
                         (%%eqv? c #\>))
                     (get-output-string output)
                   (begin
                     (readc)
                     (write-char c output)
                     (iter)))))))
      
      
      (define (parse-atomic)
        (let ((name (parse-name)))
          (let ((next (peekc)))
            (case next
              ((#\+)
               (readc)
               (jazz:new-nillable-type (lookup-type name)))
              ((#\*)
               (readc)
               (jazz:new-rest-type (lookup-type name)))
              ((#\#)
               (readc)
               (let ((class (jazz:specifier->fixed-class name)))
                 (if class
                     (let ((str (parse-string)))
                       (let ((size (%%string->number str)))
                         (if (%%fixnum? size)
                             (if (%%eqv? (peekc) #\+)
                                 (begin
                                   (readc)
                                   (jazz:new-nillable-type (jazz:new-fixed-type class size)))
                               (jazz:new-fixed-type class size))
                           (ill-formed (jazz:format "size expected: {a}" str)))))
                   (ill-formed (jazz:format "Invalid fixed type: {s}" name)))))
              ((#\<)
               (readc)
               (cond ((%%eq? name 'opt)
                      (let ((type (parse #t)))
                        (consume #\>)
                        (jazz:new-opt-type type)))
                     ((%%eq? name 'key)
                      (let ((key (%%string->keyword (%%symbol->string (parse-name)))))
                        (consume #\:)
                        (let ((type (parse #t)))
                          (consume #\>)
                          (jazz:new-key-type key type))))
                     ((%%eq? name 'category)
                      (jazz:new-category-type (parse #t)))
                     ((%%eq? name 'values)
                      (jazz:new-values-type (parse-until #\/ #\>)))
                     (else
                      (jazz:new-template-type (lookup-type name) (parse-until #\/ #\>)))))
              (else
               (lookup-type name))))))
      
      (define (new-function-type parameters result)
        (define (split-parameters types proc)
          (if (%%null? types)
              (proc '() '() '() #f)
            (let ((last (jazz:last types)))
              (if (%%class-is? last jazz:Rest-Type)
                  (proc (jazz:butlast types) '() '() last)
                (proc types '() '() #f)))))
        
        (split-parameters parameters
          (lambda (positional optional named rest)
            (jazz:new-function-type positional optional named rest result))))
      
      (define (parse atomic?)
        (if (%%eqv? (peekc) #\<)
            (begin
              (readc)
              (let ((type (parse #f)))
                (consume #\>)
                type))
          (if (%%eqv? (peekc) #\:)
              (begin
                (readc)
                (new-function-type '() (parse #t)))
            (let ((type (parse-atomic)))
              (let ((next (peekc)))
                (case next
                  ((#\:)
                   (if atomic?
                       type
                     (begin
                       (readc)
                       (new-function-type (%%list type) (parse #t)))))
                  ((#\^)
                   (if atomic?
                       type
                     (begin
                       (readc)
                       (let ((parameters (%%cons type (parse-until #\^ #\:))))
                         (new-function-type parameters (parse #t))))))
                  (else
                   type)))))))
      
      (parse #f))))


(define (jazz:specifier->type walker resume declaration environment specifier)
  (if specifier
      (jazz:walk-specifier walker resume declaration environment specifier)
    #f))


(define (jazz:type->specifier type #!optional (internal? #f))
  (define (name->specifier name)
    (if (and internal? (%%symbol? name))
        name
      (jazz:name->specifier name)))
  
  (if (%%is? type jazz:Declaration)
      (name->specifier (jazz:get-lexical-binding-name type))
    (let ((symbol (jazz:emit-specifier type)))
      (if (jazz:specifier? symbol)
          symbol
        (name->specifier symbol)))))


;;;
;;;; Primitive Types
;;;


(define jazz:primitive-types
  (%%make-table test: eq?))


(define (jazz:add-primitive-type name type)
  (%%table-set! jazz:primitive-types name type))


(define (jazz:lookup-primitive-type name)
  (%%table-ref jazz:primitive-types name #f))


(jazz:add-primitive-type 'any          jazz:Any)
(jazz:add-primitive-type 'object       jazz:Object)
(jazz:add-primitive-type 'bool         jazz:Bool)
(jazz:add-primitive-type 'boolean      jazz:Boolean)
(jazz:add-primitive-type 'char         jazz:Char)
(jazz:add-primitive-type 'number       jazz:Number)
(jazz:add-primitive-type 'complex      jazz:Complex)
(jazz:add-primitive-type 'real         jazz:Real)
(jazz:add-primitive-type 'rational     jazz:Rational)
(jazz:add-primitive-type 'integer      jazz:Integer)
(jazz:add-primitive-type 'int          jazz:Integer)
(jazz:add-primitive-type 'fx           jazz:Fixnum)
(jazz:add-primitive-type 'rt           jazz:Ratnum)
(jazz:add-primitive-type 'fl           jazz:Flonum)
(jazz:add-primitive-type 'fv           jazz:Flovec)
(jazz:add-primitive-type 's64          jazz:S64)
(jazz:add-primitive-type 'list         jazz:List)
(jazz:add-primitive-type 'null         jazz:Null)
(jazz:add-primitive-type 'pair         jazz:Pair)
(jazz:add-primitive-type 'port         jazz:Port)
(jazz:add-primitive-type 'continuation jazz:Continuation)
(jazz:add-primitive-type 'procedure    jazz:Procedure)
(jazz:add-primitive-type 'string       jazz:String)
(jazz:add-primitive-type 'symbol       jazz:Symbol)
(jazz:add-primitive-type 'keyword      jazz:Keyword)
(jazz:add-primitive-type 'vector       jazz:Vector)
(jazz:add-primitive-type 's8vector     jazz:S8Vector)
(jazz:add-primitive-type 'u8vector     jazz:U8Vector)
(jazz:add-primitive-type 's16vector    jazz:S16Vector)
(jazz:add-primitive-type 'u16vector    jazz:U16Vector)
(jazz:add-primitive-type 's32vector    jazz:S32Vector)
(jazz:add-primitive-type 'u32vector    jazz:U32Vector)
(jazz:add-primitive-type 's64vector    jazz:S64Vector)
(jazz:add-primitive-type 'u64vector    jazz:U64Vector)
(jazz:add-primitive-type 'f32vector    jazz:F32Vector)
(jazz:add-primitive-type 'f64vector    jazz:F64Vector)
(jazz:add-primitive-type 'table        jazz:Table)
(jazz:add-primitive-type 'thread       jazz:Thread)
(jazz:add-primitive-type 'promise      jazz:Promise)
(jazz:add-primitive-type 'foreign      jazz:Foreign)
(jazz:add-primitive-type 'values       jazz:Values)
(jazz:add-primitive-type 'eof          jazz:EOF)
(jazz:add-primitive-type 'unspecified  jazz:Unspecified)
(jazz:add-primitive-type 'void         jazz:Void)


(define jazz:primitive-declarations
  (%%make-table test: eq?))


(define (jazz:add-primitive-declaration decl name)
  (%%table-set! jazz:primitive-declarations decl name))


(jazz:add-primitive-declaration jazz:Object       'Object)
(jazz:add-primitive-declaration jazz:Boolean      'Boolean)
(jazz:add-primitive-declaration jazz:Char         'Char)
(jazz:add-primitive-declaration jazz:Number       'Number)
(jazz:add-primitive-declaration jazz:Complex      'Complex)
(jazz:add-primitive-declaration jazz:Real         'Real)
(jazz:add-primitive-declaration jazz:Rational     'Rational)
(jazz:add-primitive-declaration jazz:Integer      'Integer)
(jazz:add-primitive-declaration jazz:Fixnum       'Fixnum)
(jazz:add-primitive-declaration jazz:Ratnum       'Ratnum)
(jazz:add-primitive-declaration jazz:Flonum       'Flonum)
(jazz:add-primitive-declaration jazz:List         'List)
(jazz:add-primitive-declaration jazz:Null         'Null)
(jazz:add-primitive-declaration jazz:Pair         'Pair)
(jazz:add-primitive-declaration jazz:Port         'Port)
(jazz:add-primitive-declaration jazz:Continuation 'Continuation)
(jazz:add-primitive-declaration jazz:Procedure    'Procedure)
(jazz:add-primitive-declaration jazz:String       'String)
(jazz:add-primitive-declaration jazz:Symbol       'Symbol)
(jazz:add-primitive-declaration jazz:Keyword      'Keyword)
(jazz:add-primitive-declaration jazz:Vector       'Vector)
(jazz:add-primitive-declaration jazz:S8Vector     'S8Vector)
(jazz:add-primitive-declaration jazz:U8Vector     'U8Vector)
(jazz:add-primitive-declaration jazz:S16Vector    'S16Vector)
(jazz:add-primitive-declaration jazz:U16Vector    'U16Vector)
(jazz:add-primitive-declaration jazz:S32Vector    'S32Vector)
(jazz:add-primitive-declaration jazz:U32Vector    'U32Vector)
(jazz:add-primitive-declaration jazz:S64Vector    'S64Vector)
(jazz:add-primitive-declaration jazz:U64Vector    'U64Vector)
(jazz:add-primitive-declaration jazz:F32Vector    'F32Vector)
(jazz:add-primitive-declaration jazz:F64Vector    'F64Vector)
(jazz:add-primitive-declaration jazz:Table        'Table)
(jazz:add-primitive-declaration jazz:Thread       'Thread)
(jazz:add-primitive-declaration jazz:Promise      'Promise)
(jazz:add-primitive-declaration jazz:Foreign      'Foreign)
(jazz:add-primitive-declaration jazz:Values       'Values)
(jazz:add-primitive-declaration jazz:EOF          'EOF)
(jazz:add-primitive-declaration jazz:Unspecified  'Unspecified)


;; quicky until we can somehow unify primitive types and declarations
(define (jazz:patch-type-until-unification type)
  (let ((name (%%table-ref jazz:primitive-declarations type #f)))
    (if name
        (let ((module-name 'jazz.language.runtime))
          (let ((module-declaration (jazz:get-catalog-entry module-name)))
            (if module-declaration
                (jazz:lookup-declaration module-declaration name jazz:public-access module-declaration)
              type)))
      type)))


;;;
;;;; Macro
;;;


(jazz:define-class jazz:Macro-Declaration jazz:Declaration (constructor: jazz:allocate-macro-declaration)
  ((signature getter: generate setter: generate)
   (body      getter: generate setter: generate)))


(define (jazz:new-macro-declaration name type access compatibility modifiers attributes parent signature)
  (let ((new-declaration (jazz:allocate-macro-declaration name type #f access compatibility modifiers attributes #f parent #f #f #f signature #f)))
    (jazz:setup-declaration new-declaration)
    new-declaration))


(jazz:define-method (jazz:walk-binding-expandable? (jazz:Macro-Declaration declaration))
  #t)


(jazz:define-method (jazz:walk-binding-expand-form (jazz:Macro-Declaration binding) walker resume declaration environment form-src)
  (let ((form (%%desourcify form-src)))
    (let ((locator (jazz:get-declaration-locator binding)))
      (if (%%eq? (jazz:get-declaration-toplevel binding) (jazz:get-declaration-toplevel declaration))
          (jazz:walk-error walker resume declaration form-src "Macros cannot be used from within the same file: {s}" locator)
        (let ((parent-declaration (jazz:get-declaration-parent binding)))
          (jazz:load-unit (jazz:get-declaration-locator (jazz:get-declaration-toplevel parent-declaration)))
          (let ((expander (jazz:need-macro locator)))
            (%%apply expander (%%cdr form))))))))


(jazz:define-method (jazz:emit-declaration (jazz:Macro-Declaration declaration) walker resume environment)
  (let ((locator (jazz:get-declaration-locator declaration))
        (signature (jazz:get-macro-declaration-signature declaration))
        (body (jazz:get-macro-declaration-body declaration)))
    (jazz:with-annotated-frame (jazz:annotate-signature signature)
      (lambda (frame)
        (let ((augmented-environment (%%cons frame environment)))
          (jazz:sourcify-deep-if
            `(jazz:define-macro ,(%%cons locator (jazz:emit-signature signature declaration walker resume augmented-environment))
               ,@(jazz:sourcified-form (jazz:emit-expression body declaration walker resume augmented-environment)))
            (jazz:get-declaration-source declaration)))))))


(define jazz:macro-modifiers
  '(((private protected package public) . private)
    ((deprecated undocumented uptodate) . uptodate)))


(define (jazz:parse-macro walker resume declaration rest)
  (receive (access compatibility modifiers rest) (jazz:parse-modifiers walker resume declaration jazz:macro-modifiers rest)
    (let* ((signature (jazz:source-code (%%car rest)))
           (body (%%cdr rest))
           (name (%%desourcify (%%car signature)))
           (type jazz:Any)
           (parameters (%%cdr signature)))
      (values name type access compatibility modifiers parameters body))))


(define (jazz:walk-macro-declaration walker resume declaration environment form-src)
  (receive (name type access compatibility modifiers parameters body) (jazz:parse-macro walker resume declaration (%%cdr (jazz:source-code form-src)))
    (let ((signature (jazz:walk-parameters walker resume declaration environment parameters #t #f)))
      (let ((new-declaration (or (jazz:find-declaration-child declaration name)
                                 (jazz:new-macro-declaration name type access compatibility modifiers '() declaration signature))))
        (jazz:set-declaration-source new-declaration form-src)
        (let ((effective-declaration (jazz:add-declaration-child walker resume declaration new-declaration)))
          effective-declaration)))))


(define (jazz:walk-macro walker resume declaration environment form-src)
  (receive (name type access compatibility modifiers parameters body) (jazz:parse-macro walker resume declaration (%%cdr (jazz:source-code form-src)))
    (let ((new-declaration (jazz:require-declaration declaration name)))
      (receive (signature augmented-environment) (jazz:walk-parameters walker resume declaration environment parameters #t #t)
        (jazz:set-macro-declaration-signature new-declaration signature)
        (jazz:set-macro-declaration-body new-declaration
          (jazz:walk-body walker resume new-declaration augmented-environment body))
        (jazz:set-declaration-source new-declaration form-src)
        new-declaration))))


(define (jazz:expand-macros walker resume declaration environment form-src)
  (if (%%not (%%pair? (jazz:source-code form-src)))
      form-src
    (let ((procedure-expr (jazz:source-code (%%car (jazz:source-code form-src)))))
      (let ((binding (jazz:lookup-procedure-binding walker resume declaration environment procedure-expr)))
        (if (and binding (jazz:walk-binding-expandable? binding))
            (let ((expansion (jazz:walk-binding-expand-form binding walker resume declaration environment form-src)))
              (jazz:expand-macros walker resume declaration environment expansion))
          form-src)))))


(define (jazz:lookup-procedure-binding walker resume declaration environment procedure-expr)
  (let ((binding
          (cond ((jazz:identifier? procedure-expr)
                 (jazz:lookup-symbol walker resume declaration environment procedure-expr))
                ((or (%%class-is? procedure-expr jazz:Special-Form)
                     (%%class-is? procedure-expr jazz:Declaration))
                 procedure-expr)
                ((%%class-is? procedure-expr jazz:Binding-Reference)
                 (let ((ref (jazz:get-binding-reference-binding procedure-expr)))
                   (and (or (%%class-is? ref jazz:Special-Form)
                            (%%class-is? ref jazz:Declaration))
                        ref)))
                (else #f))))
    (if (%%is? binding jazz:Export-Declaration)
        (jazz:require-declaration (jazz:get-declaration-toplevel binding) (jazz:get-lexical-binding-name binding))
      binding)))


(jazz:define-method (jazz:tree-fold (jazz:Macro-Declaration declaration) down up here seed environment)
  (here declaration seed environment)
  (jazz:tree-fold-list (jazz:get-signature-expressions (jazz:get-macro-declaration-signature declaration)) down up here seed environment)
  (jazz:tree-fold (jazz:get-macro-declaration-body declaration) down up here seed environment))


(jazz:define-method (jazz:outline-extract (jazz:Macro-Declaration declaration) meta)
  `(macro ,@(jazz:outline-generate-modifiers declaration)
          (,(jazz:get-lexical-binding-name declaration) ,@(jazz:outline-generate-signature (jazz:get-macro-declaration-signature declaration)))))


;;;
;;;; Local Macro
;;;


(jazz:define-class jazz:Local-Macro-Declaration jazz:Declaration (constructor: jazz:allocate-local-macro-declaration)
  ((signature getter: generate setter: generate)
   (body      getter: generate setter: generate)))


(define (jazz:new-local-macro-declaration name type access compatibility modifiers attributes parent signature)
  (let ((new-declaration (jazz:allocate-macro-declaration name type #f access compatibility modifiers attributes #f parent #f #f #f signature #f)))
    (jazz:setup-declaration new-declaration)
    new-declaration))


(define (jazz:need-local-macro module-declaration name)
  (or (%%table-ref (jazz:get-module-declaration-local-macros module-declaration) name #f)
      (jazz:error "Unable to find macro: {s}" name)))


(jazz:define-method (jazz:walk-binding-expandable? (jazz:Local-Macro-Declaration declaration))
  #t)


(jazz:define-method (jazz:walk-binding-expand-form (jazz:Local-Macro-Declaration binding) walker resume declaration environment form-src)
  (let ((form    (%%desourcify form-src))
        (locator (jazz:get-declaration-locator binding))
        (parent  (jazz:get-declaration-parent binding)))
    (let ((expander (jazz:need-local-macro parent locator)))
      (%%apply expander (%%cdr form)))))


(jazz:define-method (jazz:emit-declaration (jazz:Local-Macro-Declaration declaration) walker resume environment)
  `(begin))


(jazz:define-method (jazz:tree-fold (jazz:Local-Macro-Declaration declaration) down up here seed environment)
  (here declaration seed environment)
  (jazz:tree-fold-list (jazz:get-signature-expressions (jazz:get-local-macro-declaration-signature declaration)) down up here seed environment)
  (jazz:tree-fold (jazz:get-local-macro-declaration-body declaration) down up here seed environment))


(define (jazz:register-local-macro module-declaration name macro)
  (%%table-set! (jazz:get-module-declaration-local-macros module-declaration) name macro))


(define (jazz:walk-local-macro-declaration walker resume declaration environment form-src)
  (receive (name type access compatibility modifiers parameters body) (jazz:parse-macro walker resume declaration (%%cdr (jazz:source-code form-src)))
    (receive (signature augmented-environment) (jazz:walk-parameters walker resume declaration environment parameters #t #t)
      (let* ((new-declaration (or (jazz:find-declaration-child declaration name)
                                  (jazz:new-local-macro-declaration name type access compatibility modifiers '() declaration signature)))
             (walked-body (jazz:walk-body walker resume new-declaration augmented-environment body)))
        (jazz:set-local-macro-declaration-signature new-declaration signature)
        (jazz:set-local-macro-declaration-body new-declaration walked-body)
        (jazz:set-declaration-source new-declaration form-src)
        (jazz:register-local-macro declaration (jazz:get-declaration-locator new-declaration)
          (jazz:with-annotated-frame (jazz:annotate-signature signature)
            (lambda (frame)
              (let ((augmented-environment (%%cons frame environment)))
                (eval `(lambda ,(jazz:emit-signature signature declaration walker resume augmented-environment)
                         ,@(jazz:sourcified-form (jazz:emit-expression walked-body declaration walker resume augmented-environment))))))))
        (let ((effective-declaration (jazz:add-declaration-child walker resume declaration new-declaration)))
          effective-declaration)))))


(define (jazz:walk-local-macro walker resume declaration environment form-src)
  (receive (name type access compatibility modifiers parameters body) (jazz:parse-macro walker resume declaration (%%cdr (jazz:source-code form-src)))
    (jazz:require-declaration declaration name)))


;;;
;;;; Syntax
;;;


(jazz:define-class jazz:Syntax-Declaration jazz:Declaration (constructor: jazz:allocate-syntax-declaration)
  ((signature getter: generate setter: generate)
   (body      getter: generate setter: generate)))


(define (jazz:new-syntax-declaration name type access compatibility modifiers attributes parent signature syntax-form)
  (let ((new-declaration (jazz:allocate-syntax-declaration name type #f access compatibility modifiers attributes #f parent #f #f #f signature #f)))
    (jazz:setup-declaration new-declaration)
    new-declaration))


(jazz:define-method (jazz:emit-declaration (jazz:Syntax-Declaration declaration) walker resume environment)
  (let ((locator (jazz:get-declaration-locator declaration))
        (signature (jazz:get-syntax-declaration-signature declaration))
        (body (jazz:get-syntax-declaration-body declaration)))
    (jazz:with-annotated-frame (jazz:annotate-signature signature)
     (lambda (frame)
       (let ((augmented-environment (%%cons frame environment))
             (current-unit-name
               (jazz:get-declaration-locator
                 (jazz:get-declaration-toplevel declaration))))
         (jazz:sourcify-deep-if
           `(define ,locator
              (let* ((env
                       (%%list
                         (jazz:new-walk-frame
                           (jazz:get-dialect-bindings (jazz:get-dialect 'scheme)))
                         (jazz:new-walk-frame
                           (jazz:get-dialect-bindings (jazz:get-dialect 'foundation)))))
                     (env
                       (cond ((jazz:get-dialect 'jazz)
                              => (lambda (x)
                                   (cons (jazz:new-walk-frame (jazz:get-dialect-bindings x)) env)))
                             (else env)))
                     (env
                       (cond ((jazz:outline-module ',current-unit-name)
                              => (lambda (x) (cons x env)))
                             (else env)))
                     (tmp
                       (jazz:new-define-syntax-form
                         ',locator
                         ,@(jazz:sourcified-form (jazz:emit-expression body declaration walker resume augmented-environment))
                         env)))
                (jazz:register-macro ',locator tmp)
                tmp))
           (jazz:get-declaration-source declaration)))))))


(jazz:define-method (jazz:walk-binding-expandable? (jazz:Syntax-Declaration declaration))
  #t)


(jazz:define-method (jazz:tree-fold (jazz:Syntax-Declaration declaration) down up here seed environment)
  (here declaration seed environment)
  (jazz:tree-fold-list (jazz:get-signature-expressions (jazz:get-syntax-declaration-signature declaration)) down up here seed environment)
  (jazz:tree-fold (jazz:get-syntax-declaration-body declaration) down up here seed environment))


(jazz:define-method (jazz:outline-extract (jazz:Syntax-Declaration declaration) meta)
  `(syntax ,@(jazz:outline-generate-access-list declaration) ,(jazz:get-lexical-binding-name declaration)))


(define jazz:current-walker
  (%%make-parameter #f))

(define jazz:current-resume
  (%%make-parameter #f))

(define jazz:current-declaration
  (%%make-parameter #f))

(define jazz:current-declaration-name
  (%%make-parameter #f))

(define jazz:current-walk-error
  (%%make-parameter #f))


(define (jazz:with-walker-context walker resume declaration form-src thunk)
  (parameterize ((jazz:current-walker walker)
                 (jazz:current-resume resume)
                 (jazz:current-declaration declaration)
                 (jazz:current-walk-error (lambda (fmt-string rest)
                                            (apply jazz:walk-error walker resume declaration form-src fmt-string rest))))
    (thunk)))


(define (jazz:call-walk-error fmt-string . rest)
  (let ((proc (jazz:current-walk-error)))
    (if proc
        (proc fmt-string rest)
      (jazz:error "Invalid walk context"))))


(jazz:define-method (jazz:walk-binding-expand-form (jazz:Syntax-Declaration binding) walker resume declaration environment form-src)
  (let ((locator (jazz:get-declaration-locator binding)))
    (if (%%eq? (jazz:get-declaration-toplevel binding) (jazz:get-declaration-toplevel declaration))
        (jazz:walk-warning walker declaration form-src "Syntax shouldn't be used from within the same file: {s}" locator))
    (let ((parent-declaration (jazz:get-declaration-parent binding)))
      (jazz:load-unit (jazz:get-declaration-locator (jazz:get-declaration-toplevel parent-declaration)))
      (let* ((define-syntax-form (jazz:need-macro locator))
             (expander (jazz:get-syntax-form-expander define-syntax-form))
             (macro-environment (jazz:get-define-syntax-form-environment define-syntax-form)))
        (jazz:with-walker-context walker resume declaration form-src
          (lambda ()
            (expander form-src environment macro-environment)))))))


(define jazz:syntax-modifiers
  '(((private protected package public) . private)
    ((deprecated undocumented uptodate) . uptodate)))


(define (jazz:walk-syntax-declaration walker resume declaration environment form-src)
  (receive (access compatibility modifiers rest) (jazz:parse-modifiers walker resume declaration jazz:syntax-modifiers (%%cdr (jazz:source-code form-src)))
    (let ((name (jazz:source-code (%%car rest)))
          (name-src (%%car rest)))
      (let ((new-declaration (or (jazz:find-declaration-child declaration name)
                                 (jazz:new-syntax-declaration name jazz:Any access compatibility modifiers '() declaration #f #f))))
        (jazz:set-declaration-source new-declaration form-src)
        (jazz:set-declaration-name-source new-declaration name-src)
        (let ((effective-declaration (jazz:add-declaration-child walker resume declaration new-declaration)))
          effective-declaration)))))


(define (jazz:walk-syntax walker resume declaration environment form-src)
  (receive (access compatibility modifiers rest) (jazz:parse-modifiers walker resume declaration jazz:syntax-modifiers (%%cdr (jazz:source-code form-src)))
    (let* ((name (jazz:source-code (%%car rest)))
           (name-src (%%car rest))
           (body (%%cdr rest))
           (new-declaration (jazz:require-declaration declaration name)))
      (receive (signature augmented-environment) (jazz:walk-parameters walker resume declaration environment '() #t #t)
        (jazz:set-syntax-declaration-signature new-declaration signature)
        (jazz:set-syntax-declaration-body new-declaration
          (jazz:walk-body walker resume new-declaration augmented-environment body))
        (jazz:set-declaration-source new-declaration form-src)
        (jazz:set-declaration-name-source new-declaration name-src)
        new-declaration))))


;;;
;;;; Define-Syntax
;;;


(jazz:define-class jazz:Define-Syntax-Declaration jazz:Syntax-Declaration (constructor: jazz:allocate-define-syntax-declaration)
  ())


(define (jazz:new-define-syntax-declaration name type access compatibility modifiers attributes parent signature syntax-form)
  (let ((new-declaration (jazz:allocate-define-syntax-declaration name type #f access compatibility modifiers attributes #f parent #f #f #f signature #f)))
    (jazz:setup-declaration new-declaration)
    new-declaration))


(jazz:define-method (jazz:emit-declaration (jazz:Define-Syntax-Declaration declaration) walker resume environment)
  (let ((locator (jazz:get-declaration-locator declaration))
        (signature (jazz:get-syntax-declaration-signature declaration))
        (body (jazz:get-syntax-declaration-body declaration)))
    (jazz:with-annotated-frame (jazz:annotate-signature signature)
     (lambda (frame)
       (let ((augmented-environment (%%cons frame environment))
             (current-unit-name
               (jazz:get-declaration-locator
                 (jazz:get-declaration-toplevel declaration))))
         (jazz:sourcify-deep-if
           `(define ,locator
              (let* ((env
                       (%%list
                         (jazz:new-walk-frame
                           (jazz:get-dialect-bindings (jazz:get-dialect 'scheme)))
                         (jazz:new-walk-frame
                           (jazz:get-dialect-bindings (jazz:get-dialect 'foundation)))))
                     (env
                       (cond ((jazz:get-dialect 'jazz)
                              => (lambda (x)
                                   (cons (jazz:new-walk-frame (jazz:get-dialect-bindings x)) env)))
                             (else env)))
                     (env
                       (cond ((jazz:outline-module ',current-unit-name)
                              => (lambda (x) (cons x env)))
                             (else env)))
                     (tmp
                       (jazz:new-define-syntax-form
                         ',locator
                         ,@(jazz:sourcified-form (jazz:emit-expression body declaration walker resume augmented-environment))
                         env)))
                (jazz:register-macro ',locator tmp)
                tmp))
           (jazz:get-declaration-source declaration)))))))


(jazz:define-method (jazz:walk-binding-expand-form (jazz:Define-Syntax-Declaration binding) walker resume declaration environment form-src)
  (let ((locator (jazz:get-declaration-locator binding)))
    (if (%%eq? (jazz:get-declaration-toplevel binding) (jazz:get-declaration-toplevel declaration))
        (jazz:walk-warning walker declaration form-src "Syntax shouldn't be used from within the same file: {s}" locator))
    (let ((parent-declaration (jazz:get-declaration-parent binding)))
      (jazz:load-unit (jazz:get-declaration-locator (jazz:get-declaration-toplevel parent-declaration)))
      (let* ((define-syntax-form (jazz:need-macro locator))
             (expander (jazz:get-syntax-form-expander define-syntax-form))
             (macro-environment (jazz:get-define-syntax-form-environment define-syntax-form)))
        (jazz:with-walker-context walker resume declaration form-src
          (lambda ()
            (expander form-src environment macro-environment)))))))


(define (jazz:walk-define-syntax-declaration walker resume declaration environment form-src)
  (receive (access compatibility modifiers rest) (jazz:parse-modifiers walker resume declaration jazz:syntax-modifiers (%%cdr (jazz:source-code form-src)))
    (let ((name (jazz:source-code (%%car rest)))
          (name-src (%%car rest)))
      (let ((new-declaration (or (jazz:find-declaration-child declaration name)
                                 (jazz:new-define-syntax-declaration name jazz:Any access compatibility modifiers '() declaration '() #f))))
        (jazz:set-declaration-source new-declaration form-src)
        (jazz:set-declaration-name-source new-declaration name-src)
        (let ((effective-declaration (jazz:add-declaration-child walker resume declaration new-declaration)))
          effective-declaration)))))


(define (jazz:walk-define-syntax walker resume declaration environment form-src)
  (receive (access compatibility modifiers rest) (jazz:parse-modifiers walker resume declaration jazz:syntax-modifiers (%%cdr (jazz:source-code form-src)))
    (let* ((name (jazz:source-code (%%car rest)))
           (name-src (%%car rest))
           (body (%%cdr rest))
           (new-declaration (jazz:require-declaration declaration name)))
      (receive (signature augmented-environment) (jazz:walk-parameters walker resume declaration environment '() #t #t)
        (jazz:set-syntax-declaration-signature new-declaration signature)
        (jazz:set-syntax-declaration-body new-declaration
          (jazz:walk-body walker resume new-declaration augmented-environment body))
        (jazz:set-declaration-source new-declaration form-src)
        (jazz:set-declaration-name-source new-declaration name-src)
        new-declaration))))


;;;
;;;; Define-Local-Syntax
;;;


(jazz:define-class jazz:Define-Local-Syntax-Declaration jazz:Syntax-Declaration (constructor: jazz:allocate-define-local-syntax-declaration)
  ())


(define (jazz:new-define-local-syntax-declaration name type access compatibility modifiers attributes parent signature syntax-form)
  (let ((new-declaration (jazz:allocate-define-local-syntax-declaration name type #f access compatibility modifiers attributes #f parent #f #f #f signature #f)))
    (jazz:setup-declaration new-declaration)
    new-declaration))


(jazz:define-method (jazz:walk-binding-expandable? (jazz:Define-Local-Syntax-Declaration declaration))
  #t)


(jazz:define-method (jazz:walk-binding-expand-form (jazz:Define-Local-Syntax-Declaration binding) walker resume declaration environment form-src)
  (let* ((locator (jazz:get-declaration-locator binding))
         (define-local-syntax-form (jazz:need-local-macro (jazz:get-declaration-parent binding) locator)))
    (let ((expander (jazz:get-syntax-form-expander define-local-syntax-form))
          (macro-environment (jazz:get-define-local-syntax-form-environment define-local-syntax-form)))
      (jazz:with-walker-context walker resume declaration form-src
        (lambda ()
          (expander form-src environment macro-environment))))))


(jazz:define-method (jazz:emit-declaration (jazz:Define-Local-Syntax-Declaration declaration) walker resume environment)
  `(begin))


(define (jazz:walk-define-local-syntax-declaration walker resume declaration environment form-src)
  (receive (access compatibility modifiers rest) (jazz:parse-modifiers walker resume declaration jazz:syntax-modifiers (%%cdr (jazz:source-code form-src)))
    (let ((name (jazz:source-code (%%car rest)))
          (body (%%cdr rest)))
      (receive (signature augmented-environment) (jazz:walk-parameters walker resume declaration environment '() #t #t)
        (let* ((new-declaration (or (jazz:find-declaration-child declaration name)
                                    (jazz:new-define-local-syntax-declaration name jazz:Any access compatibility modifiers '() declaration signature #f)))
               (walked-body (jazz:walk-body walker resume new-declaration augmented-environment body)))
          (jazz:set-syntax-declaration-body new-declaration walked-body)
          (jazz:set-declaration-source new-declaration form-src)
          (jazz:register-local-macro declaration (jazz:get-declaration-locator new-declaration)
            (jazz:with-annotated-frame (jazz:annotate-signature signature)
              (lambda (frame)
                (let ((augmented-environment (%%cons frame environment))
                      (current-unit-name
                        (jazz:get-declaration-locator
                          (jazz:get-declaration-toplevel declaration))))
                  (let* ((env
                           (%%list
                             (jazz:new-walk-frame
                               (jazz:get-dialect-bindings (jazz:get-dialect 'scheme)))
                             (jazz:new-walk-frame
                               (jazz:get-dialect-bindings (jazz:get-dialect 'foundation)))))
                         (env
                           (cond ((jazz:get-dialect 'jazz)
                                  => (lambda (x)
                                       (cons (jazz:new-walk-frame (jazz:get-dialect-bindings x)) env)))
                                 (else env))))
                    (jazz:new-define-local-syntax-form
                      (jazz:get-declaration-locator new-declaration)
                      (eval (car (jazz:sourcified-form (jazz:emit-expression walked-body declaration walker resume augmented-environment))))
                      env))))))
          (let ((effective-declaration (jazz:add-declaration-child walker resume declaration new-declaration)))
            effective-declaration))))))


(define (jazz:walk-define-local-syntax walker resume declaration environment form-src)
  (receive (access compatibility modifiers rest) (jazz:parse-modifiers walker resume declaration jazz:syntax-modifiers (%%cdr (jazz:source-code form-src)))
    (jazz:require-declaration declaration (jazz:source-code (%%car rest)))))


;;;
;;;; Proclaim
;;;


(define (jazz:setup-proclaims context)
  (let ((table (jazz:get-walk-context-proclaims context)))
    (if jazz:debug-user?
        (%%table-set! table 'check (%%list 'types 'zero 'bounds 'lambda)))))


(define (jazz:get-proclaim proclaim-name)
  (%%table-ref (jazz:effective-proclaims) proclaim-name '()))


(define (jazz:set-proclaim proclaim-name value)
  (%%table-set! (jazz:effective-proclaims) proclaim-name value))


(define jazz:all-warnings
  '(optimizations))

(define jazz:all-checks
  '(types zero bounds lambda))

(define jazz:all-generates
  '(register))


(define (jazz:parse-proclaim-clause clause)
  (define (parse-value value clause)
    (if (%%pair? clause)
        (let ((kind (%%car clause))
              (arguments (%%cdr clause)))
          (values value kind arguments))
      #f))
  
  (if (%%pair? clause)
      (case (%%car clause)
        ((default)
         (parse-value 'default (%%cdr clause)))
        ((not)
         (parse-value 'off (%%cdr clause)))
        (else
         (parse-value 'on clause)))
    #f))


(define (jazz:proclaim clause)
  (receive (value kind arguments) (jazz:parse-proclaim-clause clause)
    (case kind
      ((warn)
       (let ((warnings (if (%%null? arguments) jazz:all-warnings arguments)))
         (for-each (lambda (warning)
                     (define (proclaim on?)
                       (if on?
                           (let ((proclaimed-warnings (jazz:get-proclaim 'warn)))
                             (if (%%not (%%memq warning proclaimed-warnings))
                                 (jazz:set-proclaim 'warn (%%cons warning proclaimed-warnings))))
                         (let ((proclaimed-warnings (jazz:get-proclaim 'warn)))
                           (if (%%memq warning proclaimed-warnings)
                               (jazz:set-proclaim 'warn (jazz:remove warning proclaimed-warnings))))))
                     
                     (case value
                       ((default)
                        (proclaim (case warning
                                    ((optimizations)
                                     #f))))
                       ((on)
                        (proclaim #t))
                       ((off)
                        (proclaim #f))))
                   warnings)))
      ((check)
       (let ((checks (if (%%null? arguments) jazz:all-checks arguments)))
         (for-each (lambda (check)
                     (define (proclaim on?)
                       (if on?
                           (let ((proclaimed-checks (jazz:get-proclaim 'check)))
                             (if (%%not (%%memq check proclaimed-checks))
                                 (jazz:set-proclaim 'check (%%cons check proclaimed-checks))))
                         (let ((proclaimed-checks (jazz:get-proclaim 'check)))
                           (if (%%memq check proclaimed-checks)
                               (jazz:set-proclaim 'check (jazz:remove check proclaimed-checks))))))
                     
                     (case value
                       ((default)
                        (proclaim (case check
                                    ((types zero bounds lambda)
                                     jazz:debug-user?))))
                       ((on)
                        (proclaim #t))
                       ((off)
                        (proclaim #f))))
                   checks)))
      ((generate)
       (let ((generates (if (%%null? arguments) jazz:all-generates arguments)))
         (for-each (lambda (generate)
                     (define (proclaim on?)
                       (if on?
                           (let ((proclaimed-generates (jazz:get-proclaim 'generate)))
                             (if (%%not (%%memq generate proclaimed-generates))
                                 (jazz:set-proclaim 'generate (%%cons generate proclaimed-generates))))
                         (let ((proclaimed-generates (jazz:get-proclaim 'generate)))
                           (if (%%memq generate proclaimed-generates)
                               (jazz:set-proclaim 'generate (jazz:remove generate proclaimed-generates))))))
                     
                     (case value
                       ((default)
                        (proclaim (case generate
                                    ((register)
                                     #f))))
                       ((on)
                        (proclaim #t))
                       ((off)
                        (proclaim #f))))
                   generates))))))


(define (jazz:get-warn? warning-name)
  (%%memq warning-name (jazz:get-proclaim 'warn)))

(define (jazz:get-check? check-name)
  (or jazz:debug-check?
      (%%memq check-name (jazz:get-proclaim 'check))))

(define (jazz:get-generate? generate-name)
  (%%memq generate-name (jazz:get-proclaim 'generate)))


(define jazz:current-proclaims
  (make-parameter #f))

(define (jazz:effective-proclaims)
  (or (jazz:current-proclaims)
      (jazz:get-walk-proclaims)))


;;;
;;;; Walk Context
;;;


(jazz:define-class jazz:Walk-Context jazz:Object (constructor: jazz:allocate-walk-context)
  ((policy    getter: generate)
   (locator   getter: generate)
   (pathname  getter: generate)
   (proclaims getter: generate)))


(define (jazz:new-walk-context policy locator pathname)
  (let ((context (jazz:allocate-walk-context policy locator pathname (%%make-table test: eq?))))
    (jazz:setup-proclaims context)
    context))


(define jazz:walk-context
  (%%make-parameter #f))


(define (jazz:get-walk-context)
  (jazz:walk-context))


(define (jazz:need-walk-context)
  (or (jazz:walk-context)
      (jazz:error "There is no active walk context")))


(define (jazz:get-walk-policy)
  (let ((context (jazz:get-walk-context)))
    (if (%%not context)
        #f
      (jazz:get-walk-context-policy context))))


(define (jazz:get-walk-locator)
  (let ((context (jazz:get-walk-context)))
    (if (%%not context)
        #f
      (jazz:get-walk-context-locator context))))


(define (jazz:get-walk-pathname)
  (let ((context (jazz:get-walk-context)))
    (if (%%not context)
        #f
      (jazz:get-walk-context-pathname context))))


(define (jazz:get-walk-proclaims)
  (let ((context (jazz:need-walk-context)))
    (jazz:get-walk-context-proclaims context)))


;;;
;;;; Walk Location
;;;


(jazz:define-class jazz:Walk-Location-Class jazz:Class ()
  ())


(jazz:define-class jazz:Walk-Location jazz:Object (metaclass: jazz:Walk-Location-Class constructor: jazz:allocate-walk-location)
  ((unit-locator        getter: generate)
   (declaration-locator getter: generate)
   (locat               getter: generate)
   (path                getter: generate)))


(define (jazz:new-walk-location unit-locator declaration-locator locat path)
  (jazz:allocate-walk-location unit-locator declaration-locator locat path))


(define (jazz:walk-location walker declaration locat)
  (jazz:new-walk-location
    (jazz:get-walk-locator)
    (jazz:get-declaration-locator declaration)
    locat
    (if locat (%%container->path (%%locat-container locat)) #f)))


;;;
;;;; Walk Source Not Found
;;;


(jazz:define-class jazz:Walk-Source-Not-Found jazz:Error (constructor: jazz:allocate-walk-source-not-found)
  ((unit-name getter: generate)))


(define (jazz:new-walk-source-not-found message unit-name)
  (jazz:allocate-walk-source-not-found message unit-name))


;;;
;;;; Walk Problem
;;;


(jazz:define-class jazz:Walk-Problem jazz:Error ()
  ((location getter: generate)))


;;;
;;;; Walk Problems
;;;


(jazz:define-class jazz:Walk-Problems jazz:Error (constructor: jazz:allocate-walk-problems)
  ((warnings getter: generate)
   (errors   getter: generate)))


(define (jazz:new-walk-problems message warnings errors)
  (jazz:allocate-walk-problems message warnings errors))


(jazz:define-method (jazz:get-detail (jazz:Walk-Problems problems))
  (define (add-details problems queue)
    (for-each (lambda (problem)
                (jazz:enqueue queue (jazz:new-exception-detail "Green" (jazz:present-exception problem) (jazz:get-walk-problem-location problem) '())))
              problems))
  
  (jazz:new-exception-detail "ErrorStop" "Walk problems encountered" #f
    (let ((all (%%append (jazz:get-walk-problems-warnings problems)
                         (jazz:get-walk-problems-errors problems))))
      (map (lambda (partition)
             (jazz:bind (unit-locator . problems) partition
               (let ((prefix (if (%%not unit-locator) -1 (%%string-length (%%symbol->string unit-locator)))))
                 (jazz:new-exception-detail "Document" (or unit-locator "<console>") #f
                   (let ((unit-details (jazz:new-queue)))
                     (for-each (lambda (partition)
                                 (jazz:bind (declaration-locator . problems) partition
                                   (if (%%fx= (%%string-length declaration-locator) prefix)
                                       (add-details problems unit-details)
                                     (jazz:enqueue unit-details
                                       (jazz:new-exception-detail "Project" (%%substring declaration-locator (%%fx+ prefix 1) (%%string-length declaration-locator)) #f
                                         (let ((declaration-details (jazz:new-queue)))
                                           (add-details problems declaration-details)
                                           (jazz:queue-list declaration-details)))))))
                               (jazz:partition-walk-problems-declaration problems))
                     (jazz:queue-list unit-details))))))
           (jazz:partition-walk-problems-unit all)))))


;;;
;;;; Walk Warning
;;;


(jazz:define-class jazz:Walk-Warning jazz:Walk-Problem (constructor: jazz:allocate-walk-warning)
  ())


(define (jazz:new-walk-warning location message)
  (jazz:allocate-walk-warning message location))


;;;
;;;; Walk Error
;;;


(jazz:define-class jazz:Walk-Error jazz:Walk-Problem (constructor: jazz:allocate-walk-error)
  ())


(define (jazz:new-walk-error location message)
  (jazz:allocate-walk-error message location))


;;;
;;;; Conflict Error
;;;


(jazz:define-class jazz:Conflict-Error jazz:Walk-Error (constructor: jazz:allocate-conflict-error)
  ((prefix getter: generate)
   (symbol getter: generate)
   (first  getter: generate)
   (second getter: generate)))


(define (jazz:new-conflict-error location prefix symbol first second)
  (jazz:allocate-conflict-error #f location prefix symbol first second))


(jazz:define-method (jazz:present-exception (jazz:Conflict-Error error))
  (jazz:format "{a} conflict: {s} {s} {s}"
               (jazz:get-conflict-error-prefix error)
               (jazz:get-conflict-error-symbol error)
               (jazz:get-conflict-error-first error)
               (jazz:get-conflict-error-second error)))


;;;
;;;; Unresolved Error
;;;


(jazz:define-class jazz:Unresolved-Error jazz:Walk-Error (constructor: jazz:allocate-unresolved-error)
  ((symbol getter: generate)))


(define (jazz:new-unresolved-error location symbol)
  (jazz:allocate-unresolved-error #f location symbol))


(jazz:define-method (jazz:present-exception (jazz:Unresolved-Error error))
  (jazz:format "Unresolved symbol: {s}"
               (jazz:get-unresolved-error-symbol error)))


;;;
;;;; Walk Frame
;;;


(jazz:define-class jazz:Walk-Frame jazz:Walk-Binding (constructor: jazz:allocate-walk-frame)
  ((bindings getter: generate)))


(define (jazz:new-walk-frame bindings)
  (jazz:allocate-walk-frame (if (%%table? bindings) (%%list bindings) bindings)))


(jazz:define-method (jazz:walk-binding-lookup (jazz:Walk-Frame binding) symbol source-declaration)
  (let iter ((scan (jazz:get-walk-frame-bindings binding)))
       (if (%%null? scan)
           #f
         (let ((table (%%car scan)))
           (or (%%table-ref table symbol #f)
               (iter (%%cdr scan)))))))


;;;
;;;; Signature
;;;


(jazz:define-class jazz:Signature jazz:Object (constructor: jazz:allocate-signature)
  ((mandatory   getter: generate)
   (positional  getter: generate)
   (optional    getter: generate)
   (named       getter: generate)
   (rest        getter: generate)
   (expressions getter: generate)))


(define (jazz:new-signature positional optional named rest expressions)
  (let ((mandatory (%%length positional)))
    (jazz:allocate-signature mandatory positional optional named rest expressions)))


(define (jazz:only-positional-signature? signature)
  (and (%%null? (jazz:get-signature-optional signature))
       (%%null? (jazz:get-signature-named signature))
       (%%not (jazz:get-signature-rest signature))))


(define (jazz:typed-signature? signature #!optional (skip-first-positional? #f))
  (define (typed? parameter)
    (jazz:get-lexical-binding-type parameter))
  
  (let ((positional (jazz:get-signature-positional signature))
        (optional (jazz:get-signature-optional signature))
        (named (jazz:get-signature-named signature))
        (rest (jazz:get-signature-rest signature)))
    (or (jazz:some? typed? (if skip-first-positional? (%%cdr positional) positional))
        (jazz:some? typed? optional)
        (jazz:some? typed? named)
        (and rest (typed? rest)))))


;;;
;;;; Symbol Binding
;;;


(jazz:define-class jazz:Symbol-Binding jazz:Lexical-Binding ()
  ((gensym getter: generate setter: generate)))


(jazz:define-method (jazz:emit-binding-symbol (jazz:Symbol-Binding binding) declaration environment)
  (or (jazz:get-symbol-binding-gensym binding)
      (jazz:unwrap-syntactic-closure (jazz:get-lexical-binding-name binding))))


;;;
;;;; Variable
;;;


(jazz:define-class jazz:Variable jazz:Symbol-Binding (constructor: jazz:allocate-variable)
  ((source           getter: generate)
   (specifier-source getter: generate)
   (reference-count  getter: generate setter: generate)))


(define (jazz:new-variable name type source specifier-source)
  (%%assertion (jazz:variable-name-valid? name) (jazz:error "Invalid variable name: {s}" (jazz:desourcify-all name))
    (jazz:allocate-variable name type #f #f source specifier-source 0)))


(define (jazz:variable-name-valid? name)
  (define (variable-name-valid-symbol? name)
    (and (%%symbol? name)
         (%%not (jazz:specifier? name))))
  
  (variable-name-valid-symbol?
    (if (jazz:syntactic-closure? name)
        (jazz:get-syntactic-closure-form name)
      name)))


(jazz:define-method (jazz:walk-binding-referenced (jazz:Variable binding))
  (jazz:set-variable-reference-count binding (%%fx+ (jazz:get-variable-reference-count binding) 1)))


(jazz:define-method (jazz:emit-binding-reference (jazz:Variable binding) source-declaration walker resume environment)
  (jazz:new-code
    (jazz:emit-binding-symbol binding source-declaration environment)
    (jazz:find-annotated-type binding environment)
    #f))


(jazz:define-method (jazz:walk-binding-assignable? (jazz:Variable binding))
  #t)


(jazz:define-method (jazz:emit-binding-assignment (jazz:Variable binding) value source-declaration walker resume environment form-src)
  (let ((value-code (jazz:emit-expression value source-declaration walker resume environment)))
    (receive (annotated-frame annotated-variable annotated-type) (jazz:find-annotated binding environment)
      (%%when (%%class-is? annotated-variable jazz:Annotated-Variable)
        (jazz:extend-annotated-type annotated-frame annotated-variable (jazz:get-code-type value-code))))
    (let ((binding-code (jazz:emit-binding-symbol binding source-declaration environment)))
      (jazz:new-code
        `(set! ,binding-code ,(jazz:emit-type-cast value-code (jazz:get-lexical-binding-type binding) source-declaration source-declaration walker resume environment))
        jazz:Any
        form-src))))


(define (jazz:register-variable declaration suffix value)
  (let ((module-declaration (jazz:get-declaration-toplevel declaration)))
    (let ((symbol (jazz:generate-global-symbol suffix)))
      (let ((variable (%%cons symbol value)))
        (jazz:enqueue (jazz:get-module-declaration-walker-variables module-declaration) variable)
        variable))))


(define (jazz:register-static declaration suffix expr)
  (let ((module-declaration (jazz:get-declaration-toplevel declaration)))
    (let ((symbol (jazz:generate-global-symbol suffix)))
      (let ((static (%%cons symbol expr)))
        (jazz:enqueue (jazz:get-module-declaration-walker-statics module-declaration) static)
        static))))


;;;
;;;; Parameter
;;;


(jazz:define-class jazz:Parameter jazz:Variable (constructor: jazz:allocate-parameter)
  ())


(define (jazz:new-parameter name type source specifier-source)
  (%%assertion (jazz:variable-name-valid? name) (jazz:error "Invalid variable name: {s}" (jazz:desourcify-all name))
    (jazz:allocate-parameter name type #f #f source specifier-source 0)))


(jazz:define-virtual (jazz:emit-check? (jazz:Parameter parameter)))
(jazz:define-virtual (jazz:emit-parameter (jazz:Parameter parameter) declaration walker resume environment))


(jazz:define-method (jazz:emit-check? (jazz:Parameter parameter))
  #t)


(jazz:define-method (jazz:emit-parameter (jazz:Parameter parameter) declaration walker resume environment)
  (jazz:emit-binding-symbol parameter declaration environment))


;;;
;;;; Dynamic Parameter
;;;


(jazz:define-class jazz:Dynamic-Parameter jazz:Parameter (constructor: jazz:allocate-dynamic-parameter)
  ((class getter: generate)))


(define (jazz:new-dynamic-parameter name type source specifier-source class)
  (jazz:allocate-dynamic-parameter name type #f #f source specifier-source 0 class))


(jazz:define-method (jazz:emit-parameter (jazz:Dynamic-Parameter parameter) declaration walker resume environment)
  (let ((class (jazz:get-dynamic-parameter-class parameter)))
    (%%list (jazz:sourcified-form (jazz:emit-expression class declaration walker resume environment)) (jazz:emit-binding-symbol parameter declaration environment))))


;;;
;;;; Optional Parameter
;;;


(jazz:define-class jazz:Optional-Parameter jazz:Parameter (constructor: jazz:allocate-optional-parameter)
  ((default getter: generate setter: generate)))


(define (jazz:new-optional-parameter name type source specifier-source default)
  (jazz:allocate-optional-parameter name type #f #f source specifier-source 0 default))


(jazz:define-method (jazz:emit-parameter (jazz:Optional-Parameter parameter) declaration walker resume environment)
  (let ((default (jazz:get-optional-parameter-default parameter)))
    (%%list (jazz:emit-binding-symbol parameter declaration environment) (jazz:sourcified-form (jazz:emit-expression default declaration walker resume environment)))))


;;;
;;;; Named Parameter
;;;


(jazz:define-class jazz:Named-Parameter jazz:Parameter (constructor: jazz:allocate-named-parameter)
  ((default getter: generate setter: generate)))


(define (jazz:new-named-parameter name type source specifier-source default)
  (jazz:allocate-named-parameter name type #f #f source specifier-source 0 default))


(jazz:define-method (jazz:emit-parameter (jazz:Named-Parameter parameter) declaration walker resume environment)
  (let ((default (jazz:get-named-parameter-default parameter)))
    (%%list (jazz:get-lexical-binding-name parameter) (jazz:sourcified-form (jazz:emit-expression default declaration walker resume environment)))))


(jazz:define-method (jazz:emit-binding-symbol (jazz:Named-Parameter parameter) declaration environment)
  (jazz:get-lexical-binding-name parameter))


;;;
;;;; Rest Parameter
;;;


(jazz:define-class jazz:Rest-Parameter jazz:Parameter (constructor: jazz:allocate-rest-parameter)
  ())


(define (jazz:new-rest-parameter name type source specifier-source)
  (jazz:allocate-rest-parameter name type #f #f source specifier-source 0))


(jazz:define-method (jazz:emit-parameter (jazz:Rest-Parameter parameter) declaration walker resume environment)
  (jazz:emit-binding-symbol parameter declaration environment))


;;;
;;;; Local-Variable-Binding
;;;


(jazz:define-class jazz:Local-Variable-Binding jazz:Lexical-Binding (constructor: jazz:allocate-local-variable-binding)
  ((variable getter: generate)))


(define (jazz:new-local-variable-binding type variable)
  (jazz:allocate-local-variable-binding variable type #f variable))


(jazz:define-method (jazz:emit-binding-reference (jazz:Local-Variable-Binding declaration) source-declaration walker resume environment)
  (jazz:new-code
    (jazz:get-local-variable-binding-variable declaration)
    jazz:Any
    #f))


;;;
;;;; Macro Symbol
;;;


(jazz:define-class jazz:Macro-Symbol jazz:Symbol-Binding (constructor: jazz:allocate-macro-symbol)
  ((getter getter: generate)
   (setter getter: generate)))


(define (jazz:new-macro-symbol name getter setter)
  (jazz:allocate-macro-symbol name #f #f #f getter setter))


#; ;; convert to walk / emit
(jazz:define-method (jazz:emit-binding-reference (jazz:Macro-Symbol binding) source-declaration walker resume environment)
  (let ((getter (jazz:get-macro-symbol-getter binding)))
    (jazz:walk walker resume source-declaration environment (getter))))


#; ;; convert to walk / emit
(jazz:define-method (jazz:walk-binding-assignable? (jazz:Macro-Symbol declaration))
  #t)


#; ;; convert to walk / emit
(jazz:define-method (jazz:emit-binding-assignment (jazz:Macro-Symbol binding) value source-declaration walker resume environment)
  (let ((setter (jazz:get-macro-symbol-setter binding)))
    (jazz:walk walker resume source-declaration environment (setter value))))


;;;
;;;; Form Binding
;;;


(jazz:define-class jazz:Form-Binding jazz:Lexical-Binding ()
  ())


;;;
;;;; Declaration Form
;;;


(jazz:define-class jazz:Declaration-Form jazz:Form-Binding (constructor: jazz:allocate-declaration-form)
  ((walk getter: generate)))


(define (jazz:new-declaration-form name walk)
  (jazz:allocate-declaration-form name #f #f walk))


;;;
;;;; Special Form
;;;


(define jazz:special-forms
  '())


(define (jazz:add-special-form symbol special-form)
  (set! jazz:special-forms (%%cons (%%cons symbol special-form) jazz:special-forms)))


(define (jazz:find-special-form symbol)
  #f
  #; ;; walk is undefined
  (let ((found (assq walk jazz:special-forms)))
    (if found (cdr found) #f)))


(jazz:define-class jazz:Special-Form jazz:Form-Binding (constructor: jazz:allocate-special-form)
  ((walk getter: generate)))


(define (jazz:new-special-form name walk)
  (jazz:allocate-special-form name #f #f walk))


(jazz:define-method (jazz:walk-binding-walkable? (jazz:Special-Form binding))
  #t)


(jazz:define-method (jazz:emit-binding-reference (jazz:Special-Form binding) source-declaration walker resume environment)
  (jazz:get-lexical-binding-name binding))


(jazz:define-method (jazz:walk-binding-walk-form (jazz:Special-Form binding) walker resume declaration environment form-src)
  (let ((walk-proc/symbol (jazz:get-special-form-walk binding)))
    (let ((walk (if (%%symbol? walk-proc/symbol)
                    ;; we should cache it
                    (jazz:find-special-form walk-proc/symbol)
                  walk-proc/symbol)))
      (walk walker resume declaration environment form-src))))


;;;
;;;; Macro Form
;;;


(jazz:define-class jazz:Macro-Form jazz:Form-Binding (constructor: jazz:allocate-macro-form)
  ((expander getter: generate)))


(define (jazz:new-macro-form name expander)
  (jazz:allocate-macro-form name #f #f expander))


(jazz:define-method (jazz:walk-binding-expandable? (jazz:Macro-Form binding))
  #t)


(jazz:define-method (jazz:walk-binding-expand-form (jazz:Macro-Form binding) walker resume declaration environment form-src)
  (let ((form (%%desourcify form-src)))
    (let ((expander (jazz:get-macro-form-expander binding)))
      (apply expander walker resume declaration environment (%%cdr form)))))


;;;
;;;; Syntax Form
;;;


(jazz:define-class jazz:Syntax-Form jazz:Form-Binding (constructor: jazz:allocate-syntax-form)
  ((expander getter: generate)))


(define (jazz:new-syntax-form name expander)
  (jazz:allocate-syntax-form name #f #f expander))


(jazz:define-method (jazz:walk-binding-expandable? (jazz:Syntax-Form binding))
  #t)


(jazz:define-method (jazz:walk-binding-expand-form (jazz:Syntax-Form binding) walker resume declaration environment form-src)
  (let ((expander (jazz:get-syntax-form-expander binding)))
    (expander walker resume declaration environment form-src)))


;;;
;;;; Define-Syntax Form
;;;


(jazz:define-class jazz:Define-Syntax-Form jazz:Syntax-Form (constructor: jazz:allocate-define-syntax-form)
  ((environment getter: generate)))


(define (jazz:new-define-syntax-form name expander environment)
  (jazz:allocate-define-syntax-form name #f #f expander environment))


(jazz:define-method (jazz:walk-binding-expand-form (jazz:Define-Syntax-Form binding) walker resume declaration environment form-src)
  (let ((expander (jazz:get-syntax-form-expander binding))
        (macro-environment (jazz:get-define-syntax-form-environment binding)))
    (jazz:with-walker-context walker resume declaration form-src
      (lambda ()
        (expander form-src environment macro-environment)))))


;;;
;;;; Define-Local-Syntax Form
;;;


(jazz:define-class jazz:Define-Local-Syntax-Form jazz:Syntax-Form (constructor: jazz:allocate-define-local-syntax-form)
  ((environment getter: generate)))


(define (jazz:new-define-local-syntax-form name expander environment)
  (jazz:allocate-define-local-syntax-form name #f #f expander environment))


(jazz:define-method (jazz:walk-binding-expand-form (jazz:Define-Local-Syntax-Form binding) walker resume declaration environment form-src)
  (let ((expander (jazz:get-syntax-form-expander binding))
        (macro-environment (jazz:get-define-local-syntax-form-environment binding)))
    (jazz:with-walker-context walker resume declaration form-src
      (lambda ()
        (expander form-src environment macro-environment)))))


;;;
;;;; Let Macro Form
;;;


(jazz:define-class jazz:Let-Macro-Form jazz:Form-Binding (constructor: jazz:allocate-let-macro-form)
  ((expander getter: generate)))


(define (jazz:new-let-macro-form name expander)
  (jazz:allocate-let-macro-form name #f #f expander))


(jazz:define-method (jazz:walk-binding-expandable? (jazz:Let-Macro-Form binding))
  #t)


(jazz:define-method (jazz:walk-binding-expand-form (jazz:Let-Macro-Form binding) walker resume declaration environment form-src)
  (let ((form (%%desourcify form-src)))
    (let ((expander (jazz:get-let-macro-form-expander binding)))
      (apply expander (%%cdr form)))))


;;;
;;;; Syntactic Closure
;;;


(jazz:define-class jazz:Syntactic-Closure jazz:Object (constructor: jazz:allocate-syntactic-closure)
  ((environment getter: generate)
   (variables   getter: generate)
   ;; quick solution to form often not containing source info
   ;; for example in (increase! x) (ask Alex Shinn for the correct solution)
   (expression  getter: generate)
   (form        getter: generate)))


(jazz:define-method (jazz:print-object (jazz:Syntactic-Closure sc) output detail)
  (jazz:format output "#<{a} {a} #{a}>"
               (%%get-category-identifier (%%get-object-class sc))
               (jazz:get-syntactic-closure-form sc)
               (jazz:object->serial sc)))


(define (jazz:syntactic-closure? x)
  (%%class-is? x jazz:Syntactic-Closure))


(define (jazz:make-syntactic-closure env vars expression form)
  (if (or (%%symbol? form)
          (%%pair? form)
          (and (%%source? form)
               (or (%%symbol? (%%source-code form))
                   (%%pair? (%%source-code form)))))
      (jazz:allocate-syntactic-closure env vars expression form)
    form))


(define (jazz:syntactic-closure-form sc)
  (jazz:get-syntactic-closure-form sc))


(define (jazz:unwrap-syntactic-closure x)
  (cond ((jazz:syntactic-closure? x)
         (jazz:unwrap-syntactic-closure (jazz:get-syntactic-closure-form x)))
        ((%%source? x)
         (jazz:unwrap-syntactic-closure (%%source-code x)))
        (else
         x)))


(define (jazz:strip-syntactic-closures x)
  (cond ((jazz:syntactic-closure? x)
         (jazz:strip-syntactic-closures (jazz:get-syntactic-closure-form x)))
        ((%%source? x)
         (jazz:strip-syntactic-closures (%%source-code x)))
        ((%%pair? x)
         (cons (jazz:strip-syntactic-closures (%%car x))
               (jazz:strip-syntactic-closures (%%cdr x))))
        ((%%vector? x)
         (%%list->vector (jazz:strip-syntactic-closures (%%vector->list x))))
        (else
         x)))


(define (jazz:strip-source-info x)
  (cond ((%%source? x)
         (jazz:strip-source-info (%%source-code x)))
        ((%%pair? x)
         (cons (jazz:strip-source-info (%%car x)) (jazz:strip-source-info (%%cdr x))))
        ((%%vector? x)
         (%%list->vector (jazz:strip-source-info (%%vector->list x))))
        (else
         x)))


(define (jazz:identifier? x)
  (cond ((jazz:syntactic-closure? x) (jazz:identifier? (jazz:syntactic-closure-form x)))
        ((%%source? x) (jazz:identifier? (%%source-code x)))
        (else (%%symbol? x))))


;; identifiers are equal if they resolve to the same lexical binding
(define (jazz:identifier=? x-env x y-env y)
  (define (lookup-identifier symbol environment)
    (if (jazz:composite-reference? symbol)
        symbol
      (jazz:find-in (lambda (binding)
                      (jazz:walk-binding-lookup binding symbol #f))
                    environment)))
  
  (define (binding-name x)
    (if (%%class-is? x jazz:Lexical-Binding)
        (jazz:get-lexical-binding-name x)
      x))
  
  (or (%%eq? x y)
      (let ((x^ (lookup-identifier (jazz:unwrap-syntactic-closure x) x-env))
            (y^ (lookup-identifier (jazz:unwrap-syntactic-closure y) y-env)))
        (if x^
            (%%eq? x^ y^)
          (and (%%not y^)
               (%%eq? (jazz:unwrap-syntactic-closure x)
                      (jazz:unwrap-syntactic-closure y)))))))


(define jazz:sc-macro-transformer
  (lambda (f)
    (lambda (expr usage-env macro-env)
      (jazz:make-syntactic-closure macro-env '() expr (f expr usage-env)))))


(define jazz:rsc-macro-transformer
  (lambda (f)
    (lambda (expr usage-env macro-env)
      (f expr macro-env))))


(define jazz:er-macro-transformer
  (lambda (f)
    (lambda (expr usage-env macro-env)
      (let ((rename
              (let ((renames '()))
                (lambda (identifier)
                  (cond ((assq identifier renames) => cdr)
                        (else
                         (let ((name (jazz:make-syntactic-closure macro-env '() expr identifier)))
                           (set! renames (cons (cons identifier name) renames))
                           name))))))
            (compare
              (lambda (x y)
                (jazz:identifier=? usage-env x usage-env y))))
        (f expr rename compare)))))


;;;
;;;; Annotated Variable
;;;


(jazz:define-class jazz:Annotated-Variable jazz:Object (constructor: jazz:allocate-annotated-variable)
  ((variable      getter: generate)
   (declared-type getter: generate)
   (type          getter: generate setter: generate)))


(define (jazz:new-annotated-variable variable declared-type type)
  (jazz:allocate-annotated-variable variable declared-type type))


;;;
;;;; Restricted Binding
;;;


(jazz:define-class jazz:Restricted-Binding jazz:Object (constructor: jazz:allocate-restricted-binding)
  ((binding getter: generate)
   (type    getter: generate)))


(define (jazz:new-restricted-binding binding type)
  (jazz:allocate-restricted-binding binding type))


;;;
;;;; Annotated Frame
;;;


(jazz:define-class jazz:Annotated-Frame jazz:Object (constructor: jazz:allocate-annotated-frame)
  ((variables getter: generate)
   (reset     getter: generate)))


(define (jazz:new-annotated-frame variables reset)
  (jazz:allocate-annotated-frame variables reset))


;; put those in a cond-expand when cond-expand supports multiple features
(define (jazz:inspect-annotated-variable variable)
  (let ((serial (jazz:object->serial-symbol variable)))
    (if (%%class-is? variable jazz:Restricted-Binding)
        `(:restricted
          ,(jazz:get-lexical-binding-name (jazz:get-restricted-binding-binding variable))
          ,(jazz:get-restricted-binding-type variable) serial)
      `(:variable
        ,(jazz:get-lexical-binding-name (jazz:get-annotated-variable-variable variable))
        ,(jazz:get-annotated-variable-type variable) serial))))


(define (jazz:inspect-annotated-frame frame)
  `(:frame
    ,@(map jazz:inspect-annotated-variable (jazz:get-annotated-frame-variables frame))))


(define (jazz:inspect-annotated-environment environment)
  `(:environment
    ,@(map jazz:inspect-annotated-frame environment)))


;;;
;;;; Code
;;;


(jazz:define-class jazz:Code jazz:Object (constructor: jazz:allocate-code)
  ((form   getter: generate)
   (type   getter: generate)
   (source getter: generate)))


(define (jazz:new-code form type source)
  (jazz:allocate-code form type source))


(define (jazz:codes-forms codes)
  (map (lambda (code)
         (jazz:sourcified-form code))
       codes))


(define (jazz:codes-types codes)
  (map (lambda (code)
         (jazz:get-code-type code))
       codes))


;; this approach is clearly costly in memory and is just to experiment
(define (jazz:sourcify-code code src)
  (if (or (%%not src) (%%not (%%source? src)))
      code
    (jazz:new-code (jazz:get-code-form code)
                   (jazz:get-code-type code)
                   src)))


;; temp try
(define (jazz:sourcified-form code)
  (let ((form (jazz:get-code-form code))
        (src (jazz:get-code-source code)))
    (jazz:sourcify-if form src)))


;; temp try... 2
(define (jazz:sourcified-form2 code src)
  (let ((form (jazz:get-code-form code)))
    (jazz:sourcify-if form src)))


;;;
;;;; Annotation
;;;


(define (jazz:annotate-signature signature)
  (let ((positional (jazz:get-signature-positional signature))
        (optional (jazz:get-signature-optional signature))
        (named (jazz:get-signature-named signature))
        (rest (jazz:get-signature-rest signature))
        (queue (jazz:new-queue)))
    (define (annotate parameter)
      (let ((declared-type (jazz:get-lexical-binding-type parameter)))
        (let ((type (or declared-type jazz:Any)))
          (jazz:enqueue queue (jazz:new-annotated-variable parameter declared-type type)))))
    (for-each annotate positional)
    (for-each annotate optional)
    (for-each annotate named)
    (%%when rest
      (annotate rest))
    (jazz:queue-list queue)))


;; not 100% sure about this special case of using the argument type when the parameter is typeless
(define (jazz:annotate-inlined-signature signature arguments)
  (let ((positional (jazz:get-signature-positional signature))
        (queue (jazz:new-queue)))
    (for-each (lambda (parameter argument)
                (let ((declared-type (jazz:get-lexical-binding-type parameter)))
                  (let ((type (or declared-type (jazz:get-code-type argument))))
                    (jazz:enqueue queue (jazz:new-annotated-variable parameter declared-type type)))))
              positional
              arguments)
    (jazz:queue-list queue)))


(define (jazz:annotate-bindings bindings)
  (map (lambda (binding)
         (let ((variable (%%car binding)))
           (let ((declared-type (jazz:get-lexical-binding-type variable)))
             (let ((type (or declared-type jazz:Void)))
               (jazz:new-annotated-variable variable declared-type type)))))
       bindings))


(define (jazz:annotate-receive parameters expr-type)
  (let ((expr-types (and (%%is? expr-type jazz:Values-Type) (jazz:get-values-type-types expr-type))))
    (map (lambda (parameter)
           (let ((expr-type #f))
             (%%when expr-types
               (set! expr-type (%%car expr-types))
               (set! expr-types (%%cdr expr-types)))
             (let ((declared-type (jazz:get-lexical-binding-type parameter)))
               (let ((type (or expr-type declared-type jazz:Any)))
                 (jazz:new-annotated-variable parameter declared-type type)))))
         parameters)))


(define (jazz:annotate-internal-defines internal-defines)
  (jazz:collect (lambda (internal-define)
                  (and (%%is-not? internal-define jazz:declare-class)
                       (let ((variable (jazz:get-internal-define-variable internal-define)))
                         (let ((declared-type (jazz:get-lexical-binding-type variable)))
                           (let ((type (or declared-type jazz:Any)))
                             (jazz:new-annotated-variable variable declared-type type))))))
                internal-defines))


(define (jazz:with-annotated-frame variables proc)
  (let ((reset #f))
    (continuation-capture
      (lambda (k)
        (set! reset k)))
    (proc (jazz:new-annotated-frame variables reset))))


(define (jazz:find-annotated variable environment)
  (let ((type #f))
    (let iter-frames ((frames environment))
      (if (%%null? frames)
          #f
        (let ((annotated-frame (%%car frames)))
          (or (let iter-variables ((variables
                                     (cond ((%%class-is? annotated-frame jazz:Annotated-Frame)
                                            (jazz:get-annotated-frame-variables annotated-frame))
                                           ((%%class-is? annotated-frame jazz:Variable)
                                            (%%list annotated-frame))
                                           (else
                                            '()))))
                   (if (%%null? variables)
                       #f
                     (let ((annotated-variable (%%car variables)))
                       (if (%%class-is? annotated-variable jazz:Restricted-Binding)
                           (let ((binding (jazz:get-restricted-binding-binding annotated-variable)))
                             ;; this is really for slots so i need to think about this
                             (if (and (%%class-is? binding jazz:Declaration) (%%eq? binding variable))
                                 (values #f annotated-variable (jazz:get-restricted-binding-type annotated-variable))
                               (begin
                                 ;; keep outermost type
                                 (if (and (%%not type) (%%eq? binding variable))
                                     (set! type (jazz:get-restricted-binding-type annotated-variable)))
                                 (iter-variables (%%cdr variables)))))
                         (if (%%eq? (jazz:get-annotated-variable-variable annotated-variable) variable)
                             (values annotated-frame annotated-variable (or type (jazz:get-annotated-variable-type annotated-variable)))
                           (iter-variables (%%cdr variables)))))))
              (iter-frames (%%cdr frames))))))))


(define (jazz:find-annotated-type binding environment)
  ;; big time kludge to test it out
  (if (%%class-is? binding jazz:Variable)
      (receive (frame variable type) (jazz:find-annotated binding environment)
        type)
    ;; here it is a slot declaration
    (let ((info (jazz:find-annotated binding environment)))
      (if info
          (receive (frame variable type) info
            type)
        (jazz:get-lexical-binding-type binding)))))


(define (jazz:extend-annotated-type frame variable new-type)
  (let ((declared-type (jazz:get-annotated-variable-declared-type variable))
        (actual-type (jazz:get-annotated-variable-type variable)))
    (if declared-type
        ;; here should validate that new-type is castable to declared-type and generate a cast
        #f
      ;; this should not happen as variables are always created initialized
      (if (%%eq? actual-type jazz:Void)
          (jazz:set-annotated-variable-type variable new-type)
        (%%when (%%not (%%subtype? new-type actual-type))
          ;; should probably just call jazz:extend-type but it currently
          ;; bugs recursively on unioning function types together...
          (let ((extended-type
                  (if (%%subtype? actual-type new-type)
                      new-type
                    ;; should find the most specific common supertype
                    jazz:Any)))
            (jazz:set-annotated-variable-type variable jazz:Any)
            ;;(jazz:debug 'reset (jazz:get-lexical-binding-name (jazz:get-annotated-variable-variable variable)) actual-type new-type extended-type)
            (let ((reset (jazz:get-annotated-frame-reset frame)))
              (continuation-return reset #f))))))))


(define (jazz:extend-type type1 type2)
  (cond ((or (%%not type1) (%%not type2))
         jazz:Any)
        ;; the void test should be integrated in the type framework
        ((or (%%eq? type1 jazz:Void) (%%subtype? type1 type2))
         type2)
        ;; the void test should be integrated in the type framework
        ((or (%%eq? type2 jazz:Void) (%%subtype? type2 type1))
         type1)
        (else
         ;; should find the most specific common supertype
         jazz:Any)))


(define (jazz:extend-types types)
  (if (%%null? types)
      jazz:Void
    (jazz:extend-type (%%car types) (jazz:extend-types (%%cdr types)))))


(define (jazz:type-union types)
  (jazz:new-union-type types))


(define (jazz:type-difference type1 type2)
  #f)


;;;
;;;; Problems
;;;


(define jazz:*raise-walk-problems?*
  (%%make-parameter #f))


(define (jazz:walk-warning walker declaration src fmt-string . rest)
  (let ((location (jazz:walk-location walker declaration (jazz:source-locat src)))
        (message (apply jazz:format fmt-string rest)))
    (jazz:walker-warning walker (jazz:new-walk-warning location message))))


(define (jazz:walk-error walker resume declaration src fmt-string . rest)
  (let ((location (jazz:walk-location walker declaration (jazz:source-locat src)))
        (message (apply jazz:format fmt-string rest)))
    (jazz:walker-error walker resume (jazz:new-walk-error location message))))


(define (jazz:walk-conflict walker resume declaration prefix symbol first second form-src)
  (let ((location (jazz:walk-location walker declaration (jazz:source-locat form-src))))
    (jazz:walker-error walker resume (jazz:new-conflict-error location prefix symbol first second))))


(define (jazz:walk-unresolved walker resume declaration symbol-src)
  (let ((location (jazz:walk-location walker declaration (jazz:source-locat symbol-src))))
    (jazz:walker-error walker resume (jazz:new-unresolved-error location (jazz:unwrap-syntactic-closure symbol-src)))))


(define (jazz:walker-warning walker warning)
  (cond ((jazz:*raise-walk-problems?*)
         (raise warning))
        ((jazz:warnings?)
         (jazz:set-walker-warnings walker (%%append (jazz:get-walker-warnings walker) (%%list warning))))))


(define (jazz:walker-error walker resume error)
  (cond ((jazz:*raise-walk-problems?*)
         (raise error))
        (else
         (jazz:set-walker-errors walker (%%append (jazz:get-walker-errors walker) (%%list error)))
         (if (and resume (jazz:delay-reporting?))
             (continuation-return resume (jazz:unspecified))
           (jazz:validate-walk-problems walker)))))


(define (jazz:validate-walk-problems walker)
  (let ((warnings (jazz:get-walker-warnings walker))
        (errors (jazz:get-walker-errors walker)))
    (%%when (or (%%not-null? warnings) (%%not-null? errors))
      (let ((output (open-output-string))
            (all (%%append warnings errors)))
        (jazz:format output "Walk problems encountered{%}")
        (for-each (lambda (partition)
                    (jazz:bind (unit-locator . problems) partition
                      (jazz:format output "  In {a}" (or unit-locator "<console>"))
                      (let ((prefix (if (%%not unit-locator) -1 (%%string-length (%%symbol->string unit-locator)))))
                        (for-each (lambda (partition)
                                    (jazz:bind (declaration-locator . problems) partition
                                      (let ((toplevel? (%%fx= (%%string-length declaration-locator) prefix)))
                                        (if (%%not toplevel?)
                                            (jazz:format output "{%}    At {a}"
                                              (%%substring declaration-locator (%%fx+ prefix 1) (%%string-length declaration-locator))))
                                        (for-each (lambda (problem)
                                                    (jazz:format output "{%}{a}    {a}"
                                                      (if toplevel? "" "  ")
                                                      (jazz:present-exception problem)))
                                                  problems))))
                                  (jazz:partition-walk-problems-declaration problems)))))
                  (jazz:partition-walk-problems-unit all))
        (let ((message (get-output-string output)))
          (raise (jazz:new-walk-problems message warnings errors)))))))


(define (jazz:partition-walk-problems-unit problems)
  (jazz:partition problems
                  (lambda (problem)
                    (jazz:get-walk-location-unit-locator (jazz:get-walk-problem-location problem)))
                  assv))


(define (jazz:partition-walk-problems-declaration problems)
  (jazz:partition problems
                  (lambda (problem)
                    (%%symbol->string (jazz:get-walk-location-declaration-locator (jazz:get-walk-problem-location problem))))
                  assoc))


;;;
;;;; Parse
;;;


(define (jazz:parse-modifiers walker resume declaration infos rest)
  (define (is-modifier? infos x)
    (cond ((%%null? infos) #f)
          ((%%memq x (%%caar infos)) #t)
          (else (is-modifier? (%%cdr infos) x))))
  
  (define (skip-modifiers infos ls)
    (define (skip orig ls)
      (if (%%pair? ls)
          (let ((expr (jazz:unwrap-syntactic-closure (%%car ls))))
            (if (is-modifier? infos expr)
                (skip (%%cons expr orig) (%%cdr ls))
              (values (jazz:reverse! orig) ls)))
        (values (%%reverse orig) ls)))
    
    (skip '() ls))
  
  (define (get-modifier names from to)
    (cond ((%%eq? from to)
           #f)
          ((%%memq (jazz:unwrap-syntactic-closure (%%car from)) names)
           (cond ((get-modifier names (%%cdr from) to)
                  => (lambda (x) (jazz:walk-error "Ambiguous modifiers: {s} {s}" walker resume declaration (%%car from) x)))
                 (else
                  (jazz:unwrap-syntactic-closure (%%car from)))))
          (else
           (get-modifier names (%%cdr from) to))))
  
  (let ((modifiers rest))
    (receive (originals rest) (skip-modifiers infos rest)
      (let lp ((ls infos) (res '()))
           (cond ((%%null? ls)
                  (%%apply values (%%reverse (%%cons rest (%%cons originals res)))))
                 ((get-modifier (%%caar ls) modifiers rest)
                  => (lambda (x) (lp (%%cdr ls) (%%cons x res))))
                 (else
                  (lp (%%cdr ls) (%%cons (%%cdar ls) res))))))))


;;;
;;;; Environment
;;;


(jazz:define-method (jazz:walker-declarations (jazz:Walker walker))
  (%%list (jazz:get-dialect-declarations (jazz:get-dialect 'foundation))))


(jazz:define-method (jazz:walker-bindings (jazz:Walker walker))
  (%%list (jazz:get-dialect-bindings (jazz:get-dialect 'foundation))))


(define (jazz:walker-declaration-environment walker)
  (or (jazz:get-walker-declarations walker)
      (let ((environment (%%list (jazz:new-walk-frame (jazz:walker-declarations walker)))))
        (jazz:set-walker-declarations walker environment)
        environment)))


(define (jazz:walker-environment walker)
  (or (jazz:get-walker-bindings walker)
      (let ((environment (%%list (jazz:new-walk-frame (jazz:walker-bindings walker)))))
        (jazz:set-walker-bindings walker environment)
        environment)))


;;;
;;;; Dependencies
;;;


(define (jazz:register-autoload-declaration module-declaration autoload-declaration)
  (let ((declarations (jazz:get-module-declaration-walker-autoloads module-declaration)))
    (%%when (%%not (%%memq autoload-declaration declarations))
      (jazz:set-module-declaration-walker-autoloads module-declaration (%%cons autoload-declaration declarations)))))


;;;
;;;; Lookup
;;;


(define (jazz:lookup-reference walker resume declaration environment symbol)
  (or (jazz:lookup-symbol walker resume declaration environment symbol)
      (jazz:walk-unresolved walker resume declaration symbol)))


;;;
;;;; Expression
;;;


(jazz:define-class jazz:Expression jazz:Object ()
  ((type   initialize: #f getter: generate)
   (source                getter: generate)))


(jazz:define-virtual (jazz:emit-expression (jazz:Expression expression) declaration walker resume environment))
(jazz:define-virtual (jazz:emit-call (jazz:Expression expression) arguments arguments-codes declaration walker resume environment))


(jazz:define-method (jazz:emit-expression (jazz:Expression expression) declaration walker resume environment)
  (jazz:error "Unable to emit code for: {s}" expression))


(jazz:define-method (jazz:emit-call (jazz:Expression expression) arguments arguments-codes declaration walker resume environment)
  (let ((operator (jazz:emit-expression expression declaration walker resume environment)))
    (jazz:new-code
      `(,(jazz:sourcified-form operator) ,@(jazz:codes-forms arguments-codes))
      jazz:Any
      #f)))


(define (jazz:emit-one-expression expression declaration walker resume environment)
  (jazz:emit-expression expression declaration walker resume environment))


(define (jazz:emit-expressions expressions declaration walker resume environment)
  (map (lambda (expression)
         (jazz:emit-expression expression declaration walker resume environment))
       expressions))


;; quick hack
(define (jazz:call-emit-expression expr declaration walker resume environment)
  (jazz:emit-expression expr declaration walker resume environment))


(define (jazz:tree-fold-list ls down up here seed environment)
  (if (null? ls)
      seed
    (jazz:tree-fold-list (cdr ls) down up here (jazz:tree-fold (car ls) down up here seed environment) environment)))


(define (jazz:present-expression-location expression-src operator-src)
  (define (present expr-src)
    (and expr-src
         (let ((src (if (jazz:syntactic-closure? expr-src)
                        (jazz:get-syntactic-closure-expression expr-src)
                      expr-src)))
           (if (%%source? src)
               (let ((location (jazz:locat->container/line/col (jazz:source-locat src))))
                 (%%string->symbol (%%string-append "@" (%%number->string (%%fx+ (%%cadr location) 1)) "." (%%number->string (%%fx+ (%%car (%%cddr location)) 1)))))
             #f))))
  
  (or (present expression-src)
      (present operator-src)
      ""))


;;;
;;;; Binding Reference
;;;


(jazz:define-class jazz:Binding-Reference jazz:Expression (constructor: jazz:allocate-binding-reference)
  ((binding getter: generate)))


(define (jazz:new-binding-reference symbol-src binding)
  (jazz:allocate-binding-reference #f symbol-src binding))


(jazz:define-method (jazz:emit-expression (jazz:Binding-Reference expression) declaration walker resume environment)
  (jazz:sourcify-code (jazz:emit-binding-reference (jazz:get-binding-reference-binding expression) declaration walker resume environment)
                      (jazz:get-expression-source expression)))


(jazz:define-method (jazz:emit-call (jazz:Binding-Reference expression) arguments arguments-codes declaration walker resume environment)
  (jazz:sourcify-code (jazz:emit-binding-call (jazz:get-binding-reference-binding expression) (jazz:get-expression-source expression) arguments arguments-codes declaration walker resume environment)
                      (jazz:get-expression-source expression)))


;;;
;;;; Body
;;;


(jazz:define-class jazz:Body jazz:Expression (constructor: jazz:allocate-body)
  ((internal-proclaims getter: generate)
   (internal-defines   getter: generate)
   (expressions        getter: generate)))


(define (jazz:new-body internal-proclaims internal-defines expressions)
  (jazz:allocate-body #f #f internal-proclaims internal-defines expressions))


(jazz:define-method (jazz:emit-expression (jazz:Body expression) declaration walker resume environment)
  (let ((internal-proclaims (jazz:get-body-internal-proclaims expression)))
    (define (emit)
      (let ((internal-defines (jazz:get-body-internal-defines expression))
            (expressions (jazz:get-body-expressions expression)))
        (jazz:with-annotated-frame (jazz:annotate-internal-defines internal-defines)
          (lambda (frame)
            (let ((augmented-environment (%%cons frame environment)))
              (let ((internal-defines (jazz:emit-expressions internal-defines declaration walker resume augmented-environment))
                    (expressions (jazz:emit-expressions expressions declaration walker resume augmented-environment)))
                (jazz:new-code
                  (%%append (jazz:codes-forms internal-defines)
                            (jazz:codes-forms expressions))
                  (if (%%null? expressions) jazz:Any (jazz:get-code-type (jazz:last expressions)))
                  #f)))))))
    
    (if (%%null? internal-proclaims)
        (emit)
      (parameterize ((jazz:current-proclaims (table-copy (jazz:effective-proclaims))))
        (for-each jazz:proclaim internal-proclaims)
        (emit)))))


(jazz:define-method (jazz:tree-fold (jazz:Body expression) down up here seed environment)
  (up expression
      seed
      (jazz:tree-fold-list
        (jazz:get-body-expressions expression) down up here
        (jazz:tree-fold-list
          (jazz:get-body-internal-defines expression) down up here
          (down expression seed environment)
          environment)
        environment)
      environment))


;;;
;;;; Internal-Define
;;;


(jazz:define-variable jazz:emit-unsafe)
(jazz:define-variable jazz:emit-safe)
(jazz:define-variable jazz:emit-safe/unsafe)


(jazz:define-class jazz:Internal-Define jazz:Expression (constructor: jazz:allocate-internal-define)
  ((variable getter: generate)
   (value    getter: generate)))


(define (jazz:new-internal-define source variable value type)
  (jazz:allocate-internal-define type source variable value))


(jazz:define-method (jazz:emit-expression (jazz:Internal-Define expression) declaration walker resume environment)
  (let ((variable (jazz:get-internal-define-variable expression))
        (value (jazz:get-internal-define-value expression))
        (type (jazz:get-expression-type expression))
        (source (jazz:get-expression-source expression)))
    (let ((type (jazz:get-lexical-binding-type variable)))
      (if (and (%%is? type jazz:Function-Type)
               ;; safe first iteration simplification
               (jazz:only-positional-function-type? type)
               (jazz:typed-function-type? type))
          (jazz:new-code
            `(define ,(jazz:emit-binding-symbol variable declaration environment)
               ,(jazz:sourcified-form (jazz:emit-unsafe value source declaration walker resume environment)))
            type
            source)
        (jazz:new-code
          `(define ,(jazz:emit-binding-symbol variable declaration environment)
             ,(jazz:sourcified-form (jazz:emit-expression value declaration walker resume environment)))
          type
          source)))))


(jazz:define-method (jazz:tree-fold (jazz:Internal-Define expression) down up here seed environment)
  (up expression
      seed
      (jazz:tree-fold
        (jazz:get-internal-define-value expression) down up here
        (down expression seed environment)
        environment)
      environment))


;;;
;;;; Internal-Define-Variable
;;;


(jazz:define-class jazz:Internal-Define-Variable jazz:Variable (constructor: jazz:allocate-internal-define-variable)
  ((signature getter: generate)))


(define (jazz:new-internal-define-variable name type source specifier-source signature)
  (%%assertion (jazz:variable-name-valid? name) (jazz:error "Invalid variable name: {s}" (jazz:desourcify-all name))
    (jazz:allocate-internal-define-variable name type #f #f source specifier-source 0 signature)))


(jazz:define-method (jazz:walk-binding-validate-call (jazz:Internal-Define-Variable declaration) walker resume source-declaration operator arguments form-src)
  (let ((signature (jazz:get-internal-define-variable-signature declaration)))
    (if signature
        (jazz:validate-arguments walker resume source-declaration declaration signature arguments form-src))))


;;;
;;;; Begin
;;;


(jazz:define-class jazz:Begin jazz:Expression (constructor: jazz:allocate-begin)
  ((expressions getter: generate)))


(define (jazz:new-begin source expressions)
  (jazz:allocate-begin #f source expressions))


(jazz:define-method (jazz:emit-expression (jazz:Begin expression) declaration walker resume environment)
  (let ((expressions (jazz:get-begin-expressions expression)))
    (let ((code (jazz:emit-statements-code expressions declaration walker resume environment)))
      (jazz:new-code
        `(begin ,@(jazz:sourcified-form code))
        (jazz:get-code-type code)
        (jazz:get-expression-source expression)))))


(jazz:define-method (jazz:tree-fold (jazz:Begin expression) down up here seed environment)
  (up expression
      seed
      (jazz:tree-fold-list
        (jazz:get-begin-expressions expression) down up here
        (down expression seed environment)
        environment)
      environment))


;;;
;;;; Declare
;;;


(jazz:define-variable jazz:declare-class)


;;;
;;;; Call
;;;


(jazz:define-variable jazz:emit-specialized-call)
(jazz:define-variable jazz:emit-new-call)
(jazz:define-variable jazz:emit-primitive-call)
(jazz:define-variable jazz:emit-inlined-call)
(jazz:define-variable jazz:emit-unsafe-call)


(jazz:define-class jazz:Call jazz:Expression (constructor: jazz:allocate-call)
  ((operator  getter: generate)
   (arguments getter: generate)))


(define (jazz:new-call source operator arguments)
  (jazz:allocate-call #f source operator arguments))


(define (jazz:call-emit expression declaration walker resume environment)
  (let ((operator (jazz:get-call-operator expression))
        (arguments (jazz:get-call-arguments expression)))
    (let ((locator (if (%%class-is? operator jazz:Binding-Reference)
                       (let ((binding (jazz:get-binding-reference-binding operator)))
                         (if (%%class-is? binding jazz:Declaration)
                             (jazz:get-declaration-locator binding)
                           #f))
                     #f))
          (arguments-codes (jazz:emit-expressions arguments declaration walker resume environment)))
      (jazz:sourcify-code
        (or (jazz:emit-specialized-call operator locator arguments arguments-codes expression declaration walker resume environment)
            (jazz:emit-new-call operator locator arguments arguments-codes declaration walker resume environment)
            (jazz:emit-primitive-call operator locator arguments arguments-codes declaration walker resume environment)
            (jazz:emit-inlined-call operator arguments-codes expression declaration walker resume environment)
            (jazz:emit-unsafe-call operator locator arguments arguments-codes declaration walker resume environment)
            (jazz:emit-call operator arguments arguments-codes declaration walker resume environment))
        (jazz:get-expression-source expression)))))


(jazz:define-method (jazz:emit-expression (jazz:Call expression) declaration walker resume environment)
  (or (jazz:call-emit expression declaration walker resume environment)
      (let ((operator (jazz:get-call-operator expression))
            (arguments (jazz:get-call-arguments expression)))
        (let ((arguments-codes (jazz:emit-expressions arguments declaration walker resume environment)))
          (jazz:sourcify-code
            (jazz:emit-call operator arguments arguments-codes declaration walker resume environment)
            (jazz:get-expression-source expression))))))


(jazz:define-method (jazz:tree-fold (jazz:Call expression) down up here seed environment)
  (up expression
      seed
      (jazz:tree-fold-list
        (cons (jazz:get-call-operator expression) (jazz:get-call-arguments expression))
        down up here
        (down expression seed environment)
        environment)
      environment))


(define (jazz:walk-call walker resume declaration environment procedure-binding form-src)
  (let ((operator (%%car (jazz:source-code form-src)))
        (arguments (%%cdr (jazz:source-code form-src))))
    (if procedure-binding
        (jazz:walk-binding-validate-call procedure-binding walker resume declaration operator (jazz:desourcify-all arguments) form-src))
    (jazz:new-call form-src
                   (continuation-capture
                     (lambda (resume)
                       (jazz:walk walker resume declaration environment operator)))
                   (jazz:walk-list walker resume declaration environment arguments))))


(define (jazz:validate-arguments walker resume source-declaration declaration signature arguments form-src)
  (let ((mandatory (jazz:get-signature-mandatory signature))
        (rest (jazz:get-signature-rest signature))
        (passed (%%length arguments))
        (name (jazz:get-lexical-binding-name declaration))
        ;; todo improve handling of optional and named parameters
        (rest? (or (jazz:get-signature-rest signature)
                   (%%not-null? (jazz:get-signature-optional signature))
                   (%%not-null? (jazz:get-signature-named signature)))))
    (cond ((and (%%not rest?) (%%fx> passed mandatory))
           (jazz:walk-error walker resume source-declaration form-src "Too many arguments for: {a}" name))
          ((%%fx< passed mandatory)
           (jazz:walk-error walker resume source-declaration form-src "Not enough arguments for: {a}" name)))))


;;;
;;;; Constant
;;;


(jazz:define-class jazz:Constant jazz:Expression (constructor: jazz:allocate-constant)
  ((expansion getter: generate)))


(define (jazz:new-constant expansion type)
  (jazz:allocate-constant type #f expansion))


(jazz:define-method (jazz:emit-expression (jazz:Constant expression) declaration walker resume environment)
  (jazz:new-code
    (jazz:get-constant-expansion expression)
    (jazz:get-expression-type expression)
    #f))


(define (jazz:walk-cond-expand walker resume declaration environment form-src)
  (jazz:process-cond-expand walker resume declaration form-src
    (lambda (form form?)
      (jazz:walk walker resume declaration environment (or form '(unspecified))))))


(define (jazz:walk-quote walker resume declaration environment form-src)
  (define (scheme-data? expr)
    (or (%%null? expr)
        (%%boolean? expr)
        (%%char? expr)
        (%%string? expr)
        (%%keyword? expr)
        (%%number? expr)
        (%%symbol? expr)
        (%%vector? expr)
        (and (%%pair? expr)
             (scheme-data? (%%car expr))
             (scheme-data? (%%cdr expr)))))
  
  (let ((form (%%cdr (jazz:strip-syntactic-closures form-src))))
    (if (or (%%null? form)
            (%%not-null? (%%cdr form)))
        (jazz:walk-error walker resume declaration form-src "Ill-formed quote")
      (let ((expression (%%car form)))
        (if (scheme-data? expression)
            (jazz:walk-constant walker resume declaration environment expression)
          (let ((module-declaration (jazz:get-declaration-toplevel declaration))
                (locator (jazz:generate-global-symbol "lit")))
            ;; it is important to register before walking so subliterals come before us
            (let ((info (%%cons locator #f)))
              (jazz:set-module-declaration-walker-literals module-declaration (%%cons info (jazz:get-module-declaration-walker-literals module-declaration)))
              (%%set-cdr! info (jazz:walk-quasiquote walker resume declaration environment form-src)))
            (jazz:new-constant locator jazz:Any)))))))


(define (jazz:walk-keyword walker keyword)
  (jazz:new-constant keyword jazz:Keyword))


(define (jazz:walk-enumerator walker enumerator)
  (jazz:new-constant (%%list 'quote enumerator) jazz:Symbol))


(define (jazz:walk-constant walker resume declaration environment form-src)
  (let ((form (jazz:source-code form-src)))
    (cond ((%%boolean? form)
           (jazz:new-constant form-src jazz:Boolean))
          ((%%char? form)
           (jazz:new-constant form-src jazz:Char))
          ((%%string? form)
           (jazz:new-constant form-src jazz:String))
          ((%%keyword? form)
           (jazz:new-constant form-src jazz:Keyword))
          ((%%fixnum? form)
           ;; quick hack to minimize the danger of the fixnum
           ;; primitive patterns that are not formally correct
           (if (%%fx<= form 4096)
               (jazz:new-constant form-src jazz:Fixnum)
             (jazz:new-constant form-src jazz:Integer)))
          ((%%ratnum? form)
           (jazz:new-constant form-src jazz:Ratnum))
          ((%%flonum? form)
           (jazz:new-constant form-src jazz:Flonum))
          ((%%complex? form)
           (jazz:new-constant form-src jazz:Complex))
          ((%%number? form)
           (jazz:new-constant form-src jazz:Number))
          ((%%symbol? form)
           (jazz:new-constant `(quote ,form-src) jazz:Symbol))
          ((%%null? form)
           (jazz:new-constant `(quote ,form-src) jazz:Null))
          ((%%pair? form)
           (jazz:new-constant `(quote ,form-src) jazz:Pair))
          ((%%vector? form)
           (jazz:new-constant `(quote ,form-src) jazz:Vector))
          ((%%s8vector? form)
           (jazz:new-constant `(quote ,form-src) jazz:S8Vector))
          ((%%u8vector? form)
           (jazz:new-constant `(quote ,form-src) jazz:U8Vector))
          ((%%s16vector? form)
           (jazz:new-constant `(quote ,form-src) jazz:S16Vector))
          ((%%u16vector? form)
           (jazz:new-constant `(quote ,form-src) jazz:U16Vector))
          ((%%s32vector? form)
           (jazz:new-constant `(quote ,form-src) jazz:S32Vector))
          ((%%u32vector? form)
           (jazz:new-constant `(quote ,form-src) jazz:U32Vector))
          ((%%s64vector? form)
           (jazz:new-constant `(quote ,form-src) jazz:S64Vector))
          ((%%u64vector? form)
           (jazz:new-constant `(quote ,form-src) jazz:U64Vector))
          ((%%f32vector? form)
           (jazz:new-constant `(quote ,form-src) jazz:F32Vector))
          ((%%f64vector? form)
           (jazz:new-constant `(quote ,form-src) jazz:F64Vector))
          ((%%values? form)
           (jazz:new-constant `(quote ,form-src) jazz:Values))
          ((or (%%box? form)
               (%%eq? form #!optional)
               (%%eq? form #!key)
               (%%eq? form #!rest)
               (%%eq? form #!void))
           (jazz:new-constant `(quote ,form-src) jazz:Any))
          ((%%is? form jazz:Literal)
           (jazz:walk-literal-constant walker resume declaration environment form))
          (else
           (jazz:walk-error walker resume declaration #f "Unable to walk constant: {s}" form)))))


;; extracted from jazz:walk logic
(define (jazz:constant? form)
  (%%not (or (%%symbol? form)
             (%%pair? form))))


;;;
;;;; Quasiquote
;;;


(jazz:define-class jazz:Quasiquote jazz:Expression (constructor: jazz:allocate-quasiquote)
  ((form        getter: generate)
   (expressions getter: generate)))


(define (jazz:new-quasiquote form expressions)
  (jazz:allocate-quasiquote #f #f form expressions))


(jazz:define-method (jazz:emit-expression (jazz:Quasiquote expression) declaration walker resume environment)
  (define (emit form)
    (if (%%pair? form)
        (if (or (%%eq? (%%car form) 'unquote)
                (%%eq? (%%car form) 'unquote-splicing))
            (%%list (%%car form) (jazz:sourcified-form (jazz:emit-expression (%%cadr form) declaration walker resume environment)))
          (%%cons (emit (%%car form)) (emit (%%cdr form))))
      form))
  
  (jazz:new-code
    (%%list 'quasiquote (emit (jazz:get-quasiquote-form expression)))
    jazz:List
    #f))


(define (jazz:walk-quasiquote walker resume declaration environment form-src)
  (let ((expressions (jazz:new-queue)))
    (define (walk form-src)
      (cond ((%%pair? (jazz:source-code form-src))
             (let ((head (%%car (jazz:source-code form-src)))
                   (tail (%%cdr (jazz:source-code form-src))))
               (let ((first (jazz:source-code head)))
                 (if (or (%%eq? first 'unquote)
                         (%%eq? first 'unquote-splicing))
                     (let ((expr (jazz:walk walker resume declaration environment (%%cadr (jazz:source-code form-src)))))
                       (jazz:enqueue expressions expr)
                       (%%list first expr))
                   (%%cons (walk-car head) (walk-cdr tail))))))
            ((%%is? (jazz:source-code form-src) jazz:Literal)
             (%%list 'unquote (jazz:walk walker resume declaration environment form-src)))
            (else
             form-src)))
    
    (define (walk-car form-src)
      (if (%%is? (jazz:source-code form-src) jazz:Literal)
          (%%list 'unquote (jazz:walk walker resume declaration environment form-src))
        (walk form-src)))
    
    (define (walk-cdr form-src)
      (if (%%is? (jazz:source-code form-src) jazz:Literal)
          (%%list (%%list 'unquote-splicing (jazz:walk walker resume declaration environment form-src)))
        (walk form-src)))
    
    (let ((form (walk (%%cadr (jazz:source-code form-src)))))
      (jazz:new-quasiquote form (jazz:queue-list expressions)))))


(jazz:define-method (jazz:tree-fold (jazz:Quasiquote expression) down up here seed environment)
  (up expression
      seed
      (jazz:tree-fold-list
        (jazz:get-quasiquote-expressions expression) down up here (down expression seed environment) environment)
      environment))


;;;
;;;; Assignment
;;;


(jazz:define-class jazz:Assignment jazz:Expression (constructor: jazz:allocate-assignment)
  ((binding       getter: generate)
   (value         getter: generate)
   (symbol-source getter: generate)))


(define (jazz:new-assignment source binding value symbol-src)
  (jazz:allocate-assignment #f source binding value symbol-src))


(jazz:define-method (jazz:emit-expression (jazz:Assignment expression) declaration walker resume environment)
  (jazz:emit-binding-assignment (jazz:get-assignment-binding expression) (jazz:get-assignment-value expression) declaration walker resume environment (jazz:get-expression-source expression)))


(jazz:define-method (jazz:tree-fold (jazz:Assignment expression) down up here seed environment)
  (up expression
      seed
      (jazz:tree-fold (jazz:get-assignment-value expression) down up here (down expression seed environment) environment)
      environment))


(jazz:define-method (jazz:walk-symbol-assignment (jazz:Walker walker) resume declaration environment symbol-src value form-src)
  (let ((binding (jazz:lookup-symbol walker resume declaration environment symbol-src)))
    (if binding
        (begin
          (jazz:walk-binding-validate-assignment binding walker resume declaration symbol-src)
          (jazz:new-assignment form-src binding (jazz:walk walker resume declaration environment value) symbol-src))
      (jazz:walk-free-assignment walker resume declaration symbol-src))))


(define (jazz:walk-free-assignment walker resume declaration symbol-src)
  (jazz:walk-unresolved walker resume declaration symbol-src))


;;;
;;;; Special Expression
;;;


(jazz:define-class jazz:Special-Expression jazz:Expression (constructor: jazz:allocate-special-expression)
  ((emit getter: generate)))


(define (jazz:new-special-expression emit)
  (jazz:allocate-special-expression #f #f emit))


(jazz:define-method (jazz:emit-expression (jazz:Special-Expression expression) declaration walker resume environment)
  (let ((emit (jazz:get-special-expression-emit expression)))
    (emit expression declaration environment)))


;;;
;;;; Walk-Failed Special
;;;


(jazz:define-class jazz:Walk-Failed-Special jazz:Expression (constructor: jazz:allocate-walk-failed)
  ((answer getter: generate)))


(define (jazz:new-walk-failed-special answer)
  (jazz:allocate-walk-failed #f #f answer))


(jazz:define-method (jazz:emit-expression (jazz:Walk-Failed-Special expression) declaration walker resume environment)
  (let ((answer (jazz:get-walk-failed-special-answer expression)))
    (jazz:new-code
      answer
      jazz:Boolean
      #f)))


;;;
;;;; Analysis Data
;;;


(jazz:define-class jazz:Analysis-Data jazz:Object (constructor: jazz:allocate-analysis-data)
  ((autoload-reference     getter: generate setter: generate)
   (declaration-references getter: generate setter: generate)))


(define (jazz:new-analysis-data)
  (jazz:allocate-analysis-data #f #f))


(define (jazz:get-analysis-data locator)
  (or (%%table-ref jazz:analysis-data locator #f)
      (let ((data (jazz:new-analysis-data)))
        (%%table-set! jazz:analysis-data locator data)
        data)))


;;;
;;;; Statement
;;;


(define (jazz:emit-namespace-statements statements declaration walker resume environment)
  (let ((queue (jazz:new-queue)))
    (for-each (lambda (statement)
                (if (%%class-is? statement jazz:Declaration)
                    (jazz:enqueue queue (jazz:emit-declaration statement walker resume environment))
                  (let ((code (jazz:emit-expression statement declaration walker resume environment)))
                    (if code
                        (jazz:enqueue queue (jazz:sourcified-form code))))))
              statements)
    (jazz:queue-list queue)))


(define (jazz:emit-statements-code statements declaration walker resume environment)
  (let ((last-type #f))
    (let ((emited
            (map (lambda (statement)
                   (if (%%class-is? statement jazz:Declaration)
                       (jazz:emit-declaration statement walker resume environment)
                     (let ((code (jazz:emit-expression statement declaration walker resume environment)))
                       (set! last-type (jazz:get-code-type code))
                       (jazz:sourcified-form code))))
                 statements)))
      (jazz:new-code emited last-type #f))))


;;;
;;;; Walk
;;;


(define (jazz:walk walker resume declaration environment form-src)
  (let ((form (jazz:source-code form-src)))
    (cond ((jazz:identifier? form)
           (jazz:walk-symbol walker resume declaration environment form-src))
          ((%%null? form)
           (jazz:walk-error walker resume declaration form-src "Ill-formed expression: ()"))
          ((%%pair? form)
           (jazz:walk-form walker resume declaration environment form-src))
          ((jazz:syntactic-closure? form)
           (jazz:walk walker resume declaration (append (jazz:get-syntactic-closure-environment form) environment) (jazz:get-syntactic-closure-form form)))
          (else
           (jazz:walk-constant walker resume declaration environment form-src)))))


(define (jazz:walk-list walker resume declaration environment form-list)
  (let ((queue (jazz:new-queue)))
    (for-each (lambda (form)
                (continuation-capture
                  (lambda (resume)
                    (jazz:enqueue queue (jazz:walk walker resume declaration environment form)))))
              form-list)
    (jazz:queue-list queue)))


(define (jazz:walk-body walker resume declaration environment form-list)
  (define (walk-internal-define environment form-src variable)
    (receive (name name-src specifier specifier-source value parameters) (jazz:parse-define walker resume declaration #t form-src)
      (let ((type (if specifier (jazz:walk-specifier walker resume declaration environment specifier) jazz:Any))
            (signature (and parameters (jazz:walk-parameters walker resume declaration environment parameters #t #f))))
        (let ((effective-type (if signature (jazz:signature->function-type signature type) type)))
          (jazz:new-internal-define form-src variable (jazz:walk walker resume declaration environment value) effective-type)))))
  
  (let ((internal-proclaims '())
        (internal-defines '()))
    (define (process form)
      (cond ((jazz:begin-form? form)
             (let ((state #f))
               (for-each (lambda (sub)
                           (let ((substate (process sub)))
                             (if (%%not state)
                                 (set! state substate)
                               (if (%%neq? substate state)
                                   (jazz:error "Inconsistant internal defines")))))
                         (%%cdr (jazz:source-code form)))
               state))
            ((jazz:proclaim-form? form)
             (set! internal-proclaims (%%append internal-proclaims (%%cdr (%%desourcify form))))
             'defines)
            ((or (jazz:define-form? form)
                 (jazz:declare-form? form))
             (set! internal-defines (%%cons form internal-defines))
             'defines)
            (else
             'expressions)))
    
    (let iter ((scan form-list))
      (if (or (%%null? scan)
              (%%eq? (process (%%car scan)) 'expressions))
          (if (%%null? internal-defines)
              (jazz:new-body internal-proclaims '() (jazz:walk-list walker resume declaration environment scan))
            (let ((variables (jazz:new-queue))
                  (augmented-environment environment)
                  (internal-defines (jazz:reverse! internal-defines)))
              (for-each (lambda (internal-define)
                          (if (or (jazz:declare-form? internal-define)
                                  (jazz:proclaim-form? internal-define))
                              (jazz:enqueue variables #f)
                            (let ((internal (%%cdr (jazz:source-code internal-define))))
                              (%%assertion (%%pair? internal) (jazz:walk-error walker resume declaration internal-define "Ill-formed internal define")
                                (let ((signature (jazz:source-code (%%car internal))))
                                  (let ((name (if (%%symbol? signature)
                                                  signature
                                                (jazz:source-code (%%car signature))))
                                        (signature (if (%%symbol? signature)
                                                       #f
                                                     (jazz:walk-parameters walker resume declaration environment (%%cdr signature) #t #f))))
                                    (let ((variable (jazz:new-internal-define-variable name #f #f #f signature)))
                                      (jazz:enqueue variables variable)
                                      (set! augmented-environment (%%cons variable augmented-environment)))))))))
                        internal-defines)
              (jazz:new-body internal-proclaims
                             (map (lambda (internal-define variable)
                                    (if (jazz:declare-form? internal-define)
                                        (jazz:walk walker resume declaration environment internal-define)
                                      (let ((internal-define (walk-internal-define augmented-environment internal-define variable)))
                                        ;; ideally we should specify the type when creating the variable
                                        ;; we could then remove the generated lexical-binding type setter
                                        (jazz:set-lexical-binding-type variable (jazz:get-expression-type internal-define))
                                        internal-define)))
                                  internal-defines
                                  (jazz:queue-list variables))
                             (jazz:walk-list walker resume declaration augmented-environment scan))))
        (iter (%%cdr scan))))))


(define (jazz:parse-define walker resume declaration walk? form-src)
  (let ((rest (%%cdr (jazz:source-code form-src))))
    (if (%%not (%%pair? rest))
        (jazz:walk-error walker resume declaration form-src "Ill-formed define: {s}" (jazz:desourcify-all form-src))
      (let ((first (jazz:source-code (%%car rest))))
        (cond ((%%symbol? first)
               (if (%%not (%%pair? (%%cdr rest)))
                   (jazz:walk-error walker resume declaration form-src "Ill-formed define: {s}" (jazz:desourcify-all form-src))
                 (let ((name (jazz:source-code (%%car rest)))
                       (name-src (%%car rest)))
                   (jazz:parse-specifier (%%cdr rest)
                     (lambda (specifier specifier-source rest)
                       (values name name-src specifier specifier-source (and walk? (%%car rest)) #f))))))
              ((and (%%pair? first)
                    (%%symbol? (jazz:source-code (%%car first))))
               (let ((name (jazz:source-code (%%car (jazz:source-code (%%car rest)))))
                     (name-src (%%car (jazz:source-code (%%car rest))))
                     (parameters (%%cdr (jazz:source-code (%%car rest)))))
                 (jazz:parse-specifier (%%cdr rest)
                   (lambda (specifier specifier-source body)
                     (let ((specifier-list (if specifier (%%list specifier) '())))
                       (values name name-src specifier specifier-source `(lambda ,parameters ,@specifier-list ,@body) parameters))))))
              (else
               (jazz:walk-error walker resume declaration form-src "Ill-formed define variable: {s}" (jazz:desourcify-all first))))))))


;; Until I unify signature and function type
;; Only positional parameters as a first draft
(define (jazz:signature->function-type signature result-type)
  (define (parameter-type parameter)
    (or (jazz:get-lexical-binding-type parameter)
        jazz:Any))
  
  (jazz:new-function-type
    (map parameter-type (jazz:get-signature-positional signature))
    (map parameter-type (jazz:get-signature-optional signature))
    (map parameter-type (jazz:get-signature-named signature))
    (let ((rest (jazz:get-signature-rest signature)))
      (and rest (parameter-type rest)))
    (or result-type jazz:Any)))


;;;
;;;; Symbol
;;;


(jazz:define-method (jazz:walk-symbol (jazz:Walker walker) resume declaration environment symbol-src)
  (let ((symbol (jazz:source-code symbol-src)))
    (let ((binding (jazz:lookup-symbol walker resume declaration environment symbol-src)))
      (if binding
          (cond ((or (jazz:walk-binding-walkable? binding)
                     (jazz:walk-binding-expandable? binding))
                 (jazz:walk-error walker resume declaration symbol-src "Illegal access to syntax: {s}" symbol))
                (else
                 (if (%%class-is? binding jazz:Variable)
                     (jazz:walk-binding-referenced binding))
                 (jazz:new-binding-reference symbol-src binding)))
        (jazz:walk-free-reference walker resume declaration symbol-src)))))


(define (jazz:walk-setbang walker resume declaration environment form-src)
  (if (%%not (%%fx= (%%length (jazz:source-code form-src)) 3))
      (jazz:walk-error walker resume declaration form-src "Ill-formed set!: {s}" (%%desourcify form-src))
    (let ((variable (%%cadr (jazz:source-code form-src)))
          (value (%%car (%%cddr (jazz:source-code form-src)))))
      (if (%%symbol? (jazz:unwrap-syntactic-closure variable))
          (jazz:walk-symbol-assignment walker resume declaration environment variable value form-src)
        (jazz:walk-error walker resume declaration variable "Illegal set! of {s}" (%%desourcify variable))))))


(define (jazz:lookup-symbol walker resume declaration environment symbol-src)
  (define (validate-compatibility walker declaration referenced-declaration)
    (if (%%eq? (jazz:get-declaration-compatibility referenced-declaration) 'deprecated)
        (let ((referenced-locator (jazz:get-declaration-locator referenced-declaration)))
          (jazz:walk-warning walker declaration symbol-src "Deprecated access to {s}" referenced-locator))))
  
  (let ((referenced-declaration
         (if (jazz:syntactic-closure? symbol-src)
             (or (jazz:lookup-environment walker resume declaration environment symbol-src symbol-src)
                 (jazz:lookup-environment walker resume declaration (jazz:get-syntactic-closure-environment symbol-src) symbol-src (jazz:syntactic-closure-form symbol-src))
                 (jazz:lookup-environment walker resume declaration (jazz:get-syntactic-closure-environment symbol-src) symbol-src (jazz:source-code (jazz:syntactic-closure-form symbol-src))))
           (jazz:lookup-environment walker resume declaration environment symbol-src (jazz:source-code symbol-src)))))
    (if (and referenced-declaration (%%class-is? referenced-declaration jazz:Declaration))
        (validate-compatibility walker declaration referenced-declaration))
    (if (%%class-is? referenced-declaration jazz:Autoload-Declaration)
        (let ((module (jazz:get-declaration-toplevel declaration)))
          (jazz:register-autoload-declaration module referenced-declaration)))
    (if (jazz:analysis-mode?)
        (jazz:lookup-analyse walker declaration symbol-src referenced-declaration))
    referenced-declaration))


(jazz:define-method (jazz:lookup-environment (jazz:Walker walker) resume declaration environment symbol-src symbol)
  (let ((raw-symbol (jazz:unwrap-syntactic-closure symbol)))
    (let lp ((env environment))
      (and (%%pair? env)
           (or (jazz:walk-binding-lookup (%%car env) symbol declaration)
               (lp (%%cdr env)))))))


(jazz:define-method (jazz:lookup-analyse (jazz:Walker walker) declaration symbol-src referenced-declaration)
  #f)


;;;
;;;; Reference
;;;


(define (jazz:walk-free-reference walker resume declaration symbol-src)
  (jazz:walk-unresolved walker resume declaration symbol-src))


;;;
;;;; Form
;;;


(jazz:define-method (jazz:walk-form (jazz:Walker walker) resume declaration environment form-src)
  (let ((procedure-expr (jazz:source-code (%%car (jazz:source-code form-src)))))
    (let ((binding (jazz:lookup-procedure-binding walker resume declaration environment procedure-expr)))
      (cond ;; special form
            ((and binding (jazz:walk-binding-walkable? binding))
             (jazz:walk-binding-walk-form binding walker resume declaration environment form-src))
            ;; macro
            ((and binding (jazz:walk-binding-expandable? binding))
             (let ((expansion (jazz:walk-binding-expand-form binding walker resume declaration environment form-src)))
               (jazz:walk walker resume declaration environment expansion)))
            ;; call
            (else
             (jazz:walk-call walker resume declaration environment binding form-src))))))


;;;
;;;; Include
;;;


(define (jazz:include-form? form)
  (and (%%pair? (jazz:source-code form))
       (%%eq? (jazz:source-code (%%car (jazz:source-code form))) 'include)))


(define (jazz:include-filename form)
  (jazz:source-code (%%cadr (jazz:source-code form))))


(define (jazz:include-resource filename)
  (let ((pathname (jazz:requested-pathname)))
    (if pathname
        (jazz:pathname-brother pathname filename)
      (let ((resource (jazz:requested-unit-resource)))
        (%%make-resource (%%get-resource-package resource)
                         (jazz:pathname-brother (%%get-resource-path resource) filename)
                         #f
                         #f)))))


(define (jazz:include-forms filename)
  (jazz:read-toplevel-forms (if (or (jazz:string-starts-with? filename "/")
                                    (jazz:string-starts-with? filename "~"))
                                filename
                              (jazz:include-resource filename))))


;;;
;;;; Require
;;;


(define (jazz:walk-require-declaration walker resume declaration environment form-src)
  (let ((form (%%desourcify form-src)))
    (let ((module-declaration (jazz:get-declaration-toplevel declaration)))
      (let ((requires (jazz:filter-invoices (%%cdr form))))
        (for-each (lambda (clause)
                    (%%unless (or (%%symbol? clause)
                                  (and (%%pair? clause)
                                       (%%symbol? (%%car clause))))
                      (jazz:walk-error walker resume declaration form-src "Ill-formed require clause: {s}" clause)))
                  requires)
        ;; so units with errors are not added to the module when evaluating code
        (%%when (%%eq? (jazz:walk-for) 'eval)
          (for-each jazz:load-unit requires))
        (for-each (lambda (require)
                    (jazz:add-module-require module-declaration (jazz:listify require)))
                  requires)))))


(define (jazz:walk-require walker resume declaration environment form-src)
  (jazz:new-begin #f '()))


;;;
;;;; Import
;;;


(define (jazz:walk-import-declaration walker resume declaration environment form-src)
  (define (walk-module-import import)
    (define (lookup-module name)
      (or (jazz:outline-module name error?: #f)
          (jazz:walk-unresolved walker resume declaration name)))
    
    (receive (module-name module-phase module-autoload module-transformations) (jazz:parse-module-invoice walker resume declaration form-src import)
      (jazz:new-import-invoice module-name
                               (lookup-module module-name)
                               module-phase
                               module-transformations)))
  
  (define (walk-imports imports)
    (map (lambda (import)
           (walk-module-import (jazz:listify import)))
         imports))
  
  (let ((form (%%desourcify form-src)))
    (let ((module-declaration (jazz:get-declaration-toplevel declaration)))
      (let ((import-invoices (walk-imports (jazz:filter-invoices (%%cdr form)))))
        ;; so units with errors are not added to the module when evaluating code
        #;
        ;; commented as this is not correct and creates problems in the
        ;; worker by loading code when only doing a code walk expansion
        (%%when (%%eq? (jazz:walk-for) 'eval)
          (for-each (lambda (import-invoice)
                      (let ((module-declaration (jazz:get-module-invoice-module import-invoice)))
                        (jazz:load-unit (jazz:get-lexical-binding-name module-declaration))))
                    import-invoices))
        (for-each (lambda (import-invoice)
                    (jazz:add-module-import walker resume declaration form-src module-declaration import-invoice #t))
                  import-invoices)))))


(define (jazz:walk-import walker resume declaration environment form-src)
  (jazz:new-begin #f '()))


;;;
;;;; Native
;;;


(define jazz:native-modifiers
  '(((private protected package public) . public)
    ((deprecated undocumented uptodate) . uptodate)))

(define jazz:native-keywords
  '())


(define (jazz:parse-native walker resume declaration form-src)
  (define (ill-formed)
    (jazz:walk-error walker resume declaration form-src "Ill-formed native: {s}" (%%desourcify form-src)))
  
  (let ((rest (%%cdr (jazz:source-code form-src))))
    (receive (access compatibility modifiers rest) (jazz:parse-modifiers walker resume declaration jazz:native-modifiers rest)
      (if (%%null? rest)
          (ill-formed)
        (let ((name (jazz:source-code (%%car rest)))
              (name-src (%%car rest)))
          (if (%%symbol? name)
              (jazz:parse-specifier (%%cdr rest)
                (lambda (specifier specifier-source rest)
                  (if (%%null? rest)
                      (values name name-src specifier access compatibility modifiers)
                    (ill-formed))))
            (ill-formed)))))))


(define (jazz:walk-native-declaration walker resume declaration environment form-src)
  (receive (name name-src specifier access compatibility modifiers) (jazz:parse-native walker resume declaration form-src)
    (receive (name symbol) (jazz:parse-exported-symbol declaration name)
      (let ((type (if specifier (jazz:walk-specifier walker resume declaration environment specifier) jazz:Any)))
        (let ((new-declaration (or (jazz:find-declaration-child declaration name)
                                   (jazz:new-export-declaration name type access compatibility modifiers '() declaration symbol))))
          (jazz:set-declaration-source new-declaration form-src)
          (jazz:set-declaration-name-source new-declaration name-src)
          (let ((effective-declaration (jazz:add-declaration-child walker resume declaration new-declaration)))
            effective-declaration))))))


(define (jazz:walk-native walker resume declaration environment form-src)
  (receive (name name-src specifier access compatibility modifiers) (jazz:parse-native walker resume declaration form-src)
    (receive (name symbol) (jazz:parse-exported-symbol declaration name)
      (jazz:require-declaration declaration name))))


;;;
;;;; Native Syntax
;;;


(define jazz:native-syntax-modifiers
  '(((private protected package public) . public)
    ((deprecated undocumented uptodate) . uptodate)))

(define jazz:native-syntax-keywords
  '())


(define (jazz:parse-native-syntax walker resume declaration form-src)
  (define (ill-formed)
    (jazz:walk-error walker resume declaration form-src "Ill-formed native-syntax: {s}" (%%desourcify form-src)))
  
  (let ((rest (%%cdr (jazz:source-code form-src))))
    (receive (access compatibility modifiers rest) (jazz:parse-modifiers walker resume declaration jazz:native-syntax-modifiers rest)
      (if (%%null? rest)
          (ill-formed)
        (let ((name (jazz:source-code (%%car rest)))
              (name-src (%%car rest)))
          (if (%%symbol? name)
              (jazz:parse-specifier (%%cdr rest)
                (lambda (specifier specifier-source rest)
                  (if (%%null? rest)
                      (values name name-src specifier access compatibility modifiers)
                    (ill-formed))))
            (ill-formed)))))))


(define (jazz:walk-native-syntax-declaration walker resume declaration environment form-src)
  (receive (name name-src specifier access compatibility modifiers) (jazz:parse-native-syntax walker resume declaration form-src)
    (receive (name symbol) (jazz:parse-exported-symbol declaration name)
      (let ((type (if specifier (jazz:walk-specifier walker resume declaration environment specifier) jazz:Any)))
        (let ((new-declaration (or (jazz:find-declaration-child declaration name)
                                   (jazz:new-export-syntax-declaration name type access compatibility modifiers '() declaration symbol))))
          (jazz:set-declaration-source new-declaration form-src)
          (jazz:set-declaration-name-source new-declaration name-src)
          (let ((effective-declaration (jazz:add-declaration-child walker resume declaration new-declaration)))
            effective-declaration))))))


(define (jazz:walk-native-syntax walker resume declaration environment form-src)
  (receive (name name-src specifier access compatibility modifiers) (jazz:parse-native-syntax walker resume declaration form-src)
    (receive (name symbol) (jazz:parse-exported-symbol declaration name)
      (jazz:require-declaration declaration name))))


;;;
;;;; Let-Syntax
;;;


(define (jazz:walk-let-syntax walker resume declaration environment form-src)
  (let* ((bindings-src (%%cadr (jazz:unwrap-syntactic-closure form-src)))
         (bindings (jazz:unwrap-syntactic-closure bindings-src))
         (body (%%cddr (jazz:unwrap-syntactic-closure form-src))))
    (%%assertion (or (%%null? bindings) (%%pair? bindings)) (jazz:walk-error walker resume declaration bindings-src "Ill-formed let-syntax bindings: {s}" bindings)
      (let ((augmented-environment
             (append
              (map
               (lambda (binding)
                 (let* ((name (jazz:source-code (%%car (jazz:unwrap-syntactic-closure binding))))
                        (value (%%cadr (jazz:unwrap-syntactic-closure binding)))
                        (expander-src (jazz:emit-expression (jazz:walk walker resume declaration environment value) declaration walker resume environment))
                        (expander (eval (jazz:get-code-form expander-src))))
                   (jazz:new-define-syntax-form name expander environment)))
               bindings)
              environment)))
        (jazz:new-begin form-src (jazz:walk-list walker resume declaration augmented-environment (jazz:unwrap-syntactic-closure body)))))))


;;;
;;;; Letrec-Syntax
;;;


(define (jazz:walk-letrec-syntax . args)
  (apply jazz:walk-let-syntax args))


;;;
;;;; Parameters
;;;


;; symbol : standard positional parameter
;; (specifier/non-symbol/non-keyword-expression symbol) : dynamic positional parameter
;; (symbol expression) : optional parameter
;; (keyword symbol expression) : named parameter
;; . symbol : rest parameter
(define (jazz:walk-parameters walker resume declaration environment parameters extended? walk? #!optional (first-positional-parameter #f))
  (let ((augmented-environment environment)
        (dsssl-section #f)
        (dynamics (jazz:new-queue))
        (positionals (jazz:new-queue))
        (optionals (jazz:new-queue))
        (keywords (jazz:new-queue))
        (expressions (jazz:new-queue)))
    (define (iterate-parameters scan sections)
      (define (augment-environment expression)
        (%%when walk? (set! augmented-environment (%%cons expression augmented-environment))))
      
      (define (parameter-section parameter)
        (or dsssl-section
            (if (%%pair? parameter)
                (let ((first (jazz:source-code (%%car parameter))))
                  (cond ((or (jazz:specifier? first)
                             ;; quicky support for autoload expression
                             (%%pair? first))
                         'dynamic)
                        ((%%keyword? first)
                         'keyword)
                        (else
                         'optional)))
              'positional)))
      
      (define (allowed? section)
        (case section
          ((positional) #t)
          ((dynamic) extended?)
          (else extended?)))
      
      (cond ((%%pair? scan)
             (let* ((parameter-src (%%car scan))
                    (parameter (jazz:source-code parameter-src)))
               (if (%%memq parameter '(#!optional #!key #!rest))
                   (begin
                     (set! dsssl-section parameter)
                     (iterate-parameters (%%cdr scan) sections))
                 (let ((section (parameter-section parameter)))
                   (cond ((%%not (allowed? section))
                          (jazz:walk-error walker resume declaration parameter-src "Ill-formed lambda parameter: {s}" (jazz:desourcify-all parameter-src)))
                         ((%%not (%%memq section sections))
                          (jazz:walk-error walker resume declaration parameter-src "Misplaced {s} parameter {s} in {s}" section (jazz:desourcify-all parameter-src) (jazz:desourcify-all parameters))))
                   (case section
                     ((dynamic)
                      ;; should compare specifier to dynamic specifier
                      (let* ((specifier (jazz:source-code (%%car parameter)))
                             (specifier-source (%%car parameter))
                             (code (if (jazz:specifier? specifier) (jazz:specifier->name specifier) (%%car parameter)))
                             (variable (jazz:source-code (%%cadr parameter)))
                             (dynamic-parameter (jazz:new-dynamic-parameter variable jazz:Any #f specifier-source (jazz:walk walker resume declaration augmented-environment code))))
                        (jazz:enqueue dynamics dynamic-parameter)
                        (augment-environment dynamic-parameter))
                      (iterate-parameters (%%cdr scan) (%%memq section sections)))
                     ((positional)
                      (jazz:parse-specifier (%%cdr scan)
                        (lambda (specifier specifier-source rest)
                          (define (make-positional-parameter)
                            (if (and first-positional-parameter
                                     (%%null? (jazz:queue-list positionals)))
                                (first-positional-parameter parameter specifier)
                              (let ((type (if specifier (jazz:walk-specifier walker resume declaration environment specifier) #f)))
                                (jazz:new-parameter parameter type #f specifier-source))))
                          
                          (let ((positional-parameter (make-positional-parameter)))
                            (jazz:enqueue positionals positional-parameter)
                            (augment-environment positional-parameter))
                          (iterate-parameters rest (%%memq section sections)))))
                     ((optional #!optional)
                      (jazz:parse-specifier (%%cdr parameter)
                        (lambda (specifier specifier-source rest)
                          (%%when (and walk? (%%not (%%fx= (%%length rest) 1)))
                            (jazz:walk-error walker resume declaration parameter-src "Ill-formed optional parameter: {s}" (jazz:desourcify-all parameter-src)))
                          (let ((variable (jazz:source-code (%%car parameter)))
                                (type (if specifier (jazz:walk-specifier walker resume declaration environment specifier) #f))
                                (default (if walk? (jazz:walk walker resume declaration augmented-environment (%%car rest)) #f)))
                            (%%when walk?
                              (jazz:enqueue expressions default))
                            (let ((optional-parameter (jazz:new-optional-parameter variable type #f specifier-source default)))
                              (jazz:enqueue optionals optional-parameter)
                              (augment-environment optional-parameter)))))
                      (iterate-parameters (%%cdr scan) (%%memq section sections)))
                     ((keyword #!key)
                      (%%when (%%not (%%pair? (%%cdr parameter)))
                        (jazz:walk-error walker resume declaration parameter-src "Ill-formed keyword parameter: {s}" (jazz:desourcify-all parameter-src)))
                      (jazz:parse-specifier (if (%%eq? section #!key) (%%cdr parameter) (%%cddr parameter))
                        (lambda (specifier specifier-source rest)
                          (%%when (and walk? (%%not (%%fx= (%%length rest) 1)))
                            (jazz:walk-error walker resume declaration parameter-src "Ill-formed keyword parameter: {s}" (jazz:desourcify-all parameter-src)))
                          (let ((keyword (if (%%eq? section #!key) (%%string->keyword (%%symbol->string (jazz:source-code (%%car parameter)))) (jazz:source-code (%%car parameter))))
                                (variable (if (%%eq? section #!key) (jazz:source-code (%%car parameter)) (jazz:source-code (%%cadr parameter))))
                                (type (if specifier (jazz:walk-specifier walker resume declaration environment specifier) #f))
                                (default (if walk? (jazz:walk walker resume declaration augmented-environment (%%car rest)) #f)))
                            (%%when (%%not (%%eq? (%%string->symbol (%%keyword->string keyword)) variable))
                              (jazz:walk-error walker resume declaration parameter-src "Mismatched key/name for keyword parameter: {s}" (jazz:desourcify-all parameter-src)))
                            (%%when walk?
                              (jazz:enqueue expressions default))
                            (let ((keyword-parameter (jazz:new-named-parameter variable type #f specifier-source default)))
                              (jazz:enqueue keywords keyword-parameter)
                              (augment-environment keyword-parameter)))))
                      (iterate-parameters (%%cdr scan) (%%memq section sections)))
                     ((#!rest)
                      (let ((rest parameter))
                        (%%when (%%not (%%symbol? rest))
                          (jazz:walk-error walker resume declaration scan "Ill-formed rest parameter: {s}" rest))
                        (let ((parameter-expression (jazz:new-rest-parameter rest jazz:List #f #f)))
                          (augment-environment parameter-expression)
                          parameter-expression))))))))
            ((%%not (%%null? scan))
             (let ((rest (jazz:source-code scan)))
               (%%when (%%not (%%symbol? rest))
                 (jazz:walk-error walker resume declaration scan "Ill-formed rest parameter: {s}" rest))
               (let ((parameter-expression (jazz:new-rest-parameter rest jazz:List #f #f)))
                 (augment-environment parameter-expression)
                 parameter-expression)))
            (else
             #f)))
    
    (let ((rest (iterate-parameters parameters '(dynamic positional optional #!optional keyword #!key #!rest)))
          (dynamics (jazz:queue-list dynamics))
          (positionals (jazz:queue-list positionals))
          (expressions (jazz:queue-list expressions)))
      (let ((signature
              (jazz:new-signature
                (append dynamics positionals)
                (jazz:queue-list optionals)
                (jazz:queue-list keywords)
                rest
                expressions)))
        (if walk?
            (values signature augmented-environment)
          signature)))))


(define (jazz:emit-signature-casts signature source-declaration walker resume environment)
  (let ((queue #f))
    (define (process parameter)
      (let ((type (jazz:get-lexical-binding-type parameter)))
        (if (and type (%%neq? type jazz:Any) (jazz:emit-check? parameter))
            (let ((cast (jazz:emit-parameter-cast (jazz:emit-binding-reference parameter source-declaration walker resume environment) type source-declaration walker resume environment)))
              (if cast
                  (begin
                    ;; optimize the by far more frequent case of signatures having no types
                    (if (%%not queue)
                        (set! queue (jazz:new-queue)))
                    (jazz:enqueue queue cast)))))))
    
    (for-each process (jazz:get-signature-positional signature))
    (for-each process (jazz:get-signature-optional signature))
    (for-each process (jazz:get-signature-named signature))
    (if (%%not queue)
        #f
      (jazz:queue-list queue))))


(define (jazz:add-signature-casts signature-casts body-emit)
  (if (or (%%not signature-casts)
          (%%null? signature-casts))
      `(,body-emit)
    `((let ,signature-casts
        ,body-emit))))


(define (jazz:emit-signature signature declaration walker resume environment)
  (let ((positional (jazz:get-signature-positional signature))
        (optional (jazz:get-signature-optional signature))
        (named (jazz:get-signature-named signature))
        (rest (jazz:get-signature-rest signature))
        (queue (jazz:new-queue)))
    (define (emit parameter)
      (jazz:enqueue queue (jazz:emit-parameter parameter declaration walker resume environment)))
    
    (for-each emit positional)
    (%%when (%%not (%%null? optional))
      (jazz:enqueue queue #!optional)
      (for-each emit optional))
    (%%when (%%not (%%null? named))
      (jazz:enqueue queue #!key)
      (for-each emit named))
    (%%when rest
      (jazz:enqueue queue #!rest)
      (emit rest))
    (jazz:queue-list queue)))


;;;
;;;; Walk Failed
;;;


(define (jazz:walk-walk-failed walker resume declaration environment form-src)
  (let ((form (%%cadr (%%desourcify form-src)))
        (rest (%%cddr (%%desourcify form-src))))
    (let ((problem-subclass
            (cond ((%%null? rest) jazz:Walk-Problem)
                  ((%%eq? (%%car rest) ':warning) jazz:Walk-Warning)
                  ((%%eq? (%%car rest) ':error) jazz:Walk-Error)
                  (else (jazz:error "Ill-formed walk-failed?")))))
      (let ((answer
              (jazz:catch-exception-filter
                (lambda (exc)
                  #t)
                (lambda (exc)
                  (and (%%object? exc)
                       (if (%%is? exc jazz:Walk-Problem)
                           (%%is? exc problem-subclass)
                         ;; because some walk problems still throw explicit errors
                         (jazz:is? exc jazz:Error))))
                (lambda ()
                  (parameterize ((jazz:*raise-walk-problems?* #t))
                    (jazz:walk walker resume declaration environment form)
                    #f)))))
        (jazz:new-walk-failed-special answer)))))


;;;
;;;; Catalog
;;;


(define jazz:Catalog
  (%%make-table test: eq?))


(define (jazz:get-catalog-table)
  jazz:Catalog)


(define (jazz:get-catalog-entry unit-name)
  (%%table-ref jazz:Catalog unit-name #f))


(define (jazz:set-catalog-entry unit-name entry)
  (%%table-set! jazz:Catalog unit-name entry))


(define (jazz:set-catalog-entry-status unit-name status)
  (let ((declaration (let ((entry (jazz:get-catalog-entry unit-name)))
                       (if (%%pair? entry)
                           (%%cdr entry)
                         entry))))
    (jazz:set-catalog-entry unit-name (if status (%%cons status declaration) declaration))))


(define (jazz:valid-catalog-entry unit-name)
  (let ((entry (jazz:get-catalog-entry unit-name)))
    (if (%%pair? entry)
        (jazz:circular-dependency-error unit-name (map cdr (jazz:current-load-stack)))
      entry)))


(define (jazz:release-catalog-entries)
  (jazz:iterate-table jazz:Catalog
    (lambda (unit-name entry)
      (if (%%pair? entry)
          (jazz:set-catalog-entry unit-name (%%cdr entry))))))


(define (jazz:call-with-catalog-entry-lock unit-name thunk)
  (jazz:call-with-load-lock
    (lambda ()
      (let ((entry (jazz:get-catalog-entry unit-name)))
        (if (%%pair? entry)
            (jazz:circular-dependency-error unit-name (map cdr (jazz:current-load-stack)))
          (parameterize ((jazz:current-load-stack (%%cons (%%cons ':walk unit-name) (jazz:current-load-stack))))
            (dynamic-wind
              (lambda ()
                (jazz:set-catalog-entry-status unit-name ':walking))
              thunk
              (lambda ()
                (if (%%pair? (jazz:get-catalog-entry unit-name))
                    (jazz:set-catalog-entry-status unit-name #f))))))))))


(define jazz:outline-feedback
  (%%make-parameter #f))


(define (jazz:bin->otl bin)
  (let ((path (%%get-resource-path bin)))
    (%%make-resource (%%get-resource-package bin)
                     (jazz:pathname-brother path (jazz:add-extension (jazz:pathname-base path) "otl"))
                     (%%get-resource-underscore? bin)
                     #f)))


(define jazz:outline-not-found-hook
  #f)

(define (jazz:get-outline-not-found-hook)
  jazz:outline-not-found-hook)

(define (jazz:set-outline-not-found-hook hook)
  (set! jazz:outline-not-found-hook hook))


(define jazz:outline-hook
  #f)

(define (jazz:get-outline-hook)
  jazz:outline-hook)

(define (jazz:set-outline-hook hook)
  (set! jazz:outline-hook hook))


(define (jazz:outline-unit unit-name #!key (error? #t))
  (define (load-toplevel-declaration)
    (let try-again ((first-try? #t))
      (jazz:with-unit-resources unit-name #f
        (lambda (src obj bin lib obj-uptodate? bin-uptodate? lib-uptodate? manifest)
          (let ((src (if (%%not src)
                         (if (%%not bin)
                             #f
                           (let ((otl (jazz:bin->otl bin)))
                             (let ((otl-path (jazz:resource-pathname otl)))
                               (if (file-exists? otl-path)
                                   otl
                                 #f))))
                       src)))
            (if (%%not src)
                (if (and jazz:unit-not-found-hook first-try?)
                    (begin
                      (jazz:outline-not-found-hook unit-name)
                      (try-again #f))
                  (if (%%not error?)
                      #f
                    (raise (jazz:new-walk-source-not-found (jazz:format "Unable to locate unit source: {s}" unit-name) unit-name))))
              (jazz:with-verbose (jazz:outline-verbose?) "outlining" (jazz:resource-pathname src)
                (lambda ()
                  ;; not reading the literals is necessary as reading a literal will load units
                  (let ((form (jazz:read-toplevel-form src read-literals?: #f)))
                    (parameterize ((jazz:requested-unit-name unit-name)
                                   (jazz:requested-unit-resource src)
                                   (jazz:requested-pathname #f)
                                   (jazz:walk-for 'interpret)
                                   (jazz:generate-symbol-for "%outline^")
                                   (jazz:generate-symbol-context unit-name))
                      (let ((kind (jazz:source-code (%%car (jazz:source-code form)))))
                        (case kind
                          ((unit)
                           (jazz:parse-unit-declaration (%%cdr (jazz:source-code form))))
                          ((module)
                           (jazz:parse-module-declaration (%%cdr (jazz:source-code form))))))))))))))))
  
  (or (let ((declaration (jazz:get-catalog-entry unit-name)))
        (if (%%pair? declaration)
            #f
          declaration))
      (jazz:call-with-catalog-entry-lock unit-name
        (lambda ()
          (let ((feedback (jazz:outline-feedback)))
            (if feedback
                (feedback unit-name)))
          (let ((declaration (load-toplevel-declaration)))
            (if (%%not declaration)
                (if error?
                    (jazz:error "Unable to locate unit declaration: {s}" unit-name))
              (jazz:set-catalog-entry unit-name
                                      (if jazz:outline-hook
                                          (jazz:outline-hook unit-name declaration)
                                        declaration)))
            declaration)))))


(define (jazz:outline-module unit-name #!key (error? #t))
  (let ((declaration (jazz:outline-unit unit-name error?: error?)))
    (%%assert (or (%%not declaration) (%%class-is? declaration jazz:Module-Declaration))
      declaration)))


(define jazz:read-literals?
  (%%make-parameter #t))


(define (jazz:read-toplevel-forms pathname/resource #!key (read-literals? #t))
  (let ((source (if (%%string? pathname/resource) pathname/resource (jazz:resource-pathname pathname/resource))))
    (jazz:with-extension-reader (jazz:pathname-extension source)
      (lambda ()
        (let ((char-encoding (if (%%string? pathname/resource) jazz:default-char-encoding (jazz:resource-char-encoding pathname/resource)))
              (eol-encoding 'cr-lf))
          (call-with-input-file (%%list path: source char-encoding: char-encoding eol-encoding: eol-encoding)
            (lambda (port)
              (parameterize ((jazz:read-literals? read-literals?))
                (jazz:read-source-all port)))))))))


(define (jazz:read-toplevel-form pathname/resource #!key (read-literals? #t) (script? #f))
  (let ((source (if (%%string? pathname/resource) pathname/resource (jazz:resource-pathname pathname/resource)))
        (all (jazz:read-toplevel-forms pathname/resource read-literals?: read-literals?)))
    (if (%%null? all)
        (jazz:error "Found empty unit declaration in {a}" source)
      (let ((form-src (%%car all))
            (extraneous? (%%not-null? (%%cdr all))))
        (if (and (%%pair? (jazz:source-code form-src)) (%%memq (jazz:source-code (%%car (jazz:source-code form-src))) (if script? '(script) '(unit module))))
            (if (%%not extraneous?)
                form-src
              (jazz:error "Found extraneous expressions after unit declaration in {a}" source))
          (jazz:error "Found invalid unit declaration in {a}" source))))))


(define (jazz:walk-unit unit-name)
  (jazz:generate-unit unit-name #f))


;; useful for units that cannot be walked
(define (jazz:walk/outline-unit unit-name)
  (or (jazz:walk-unit unit-name)
      (jazz:outline-unit unit-name)))


(define (jazz:generate-unit unit-name #!optional (emit? #t))
  (let ((src (jazz:find-unit-src unit-name)))
    (parameterize ((jazz:requested-unit-name unit-name)
                   (jazz:requested-unit-resource src)
                   (jazz:requested-pathname #f)
                   (jazz:walk-for 'interpret)
                   (jazz:generate-symbol-for "%")
                   (jazz:generate-symbol-context unit-name))
      (let ((form (jazz:read-toplevel-form src)))
        (case (jazz:source-code (%%car (jazz:source-code form)))
          ((unit)
           #f)
          ((module)
           (jazz:generate-module (%%cdr (jazz:source-code form)) emit?)))))))


;;;
;;;; Access
;;;


;; to debug sourcification
(define (resourcify)
  (define (transform-src src)
    (let ((code (cadr src))
          (line (- (caddr src) 1))
          (col (- (cadddr src) 1)))
      (##make-source (transform code) (##make-locat  "/Users/cartier/Devel/together/app/devel/jazz/lib/jazz.test/src/jazz/test/_test.jazz" (##make-filepos line col 0)))))
  
  (define (transform-list lst)
    (cons (transform (car lst))
          (transform (cdr lst))))
  
  (define (transform obj)
    (cond ((and (pair? obj)
                (eq? (car obj) 'source))
           (transform-src obj))
          ((%%pair? obj)
           (transform-list obj))
          (else
           obj)))
  
  (transform
    '(source begin 41 1)))


(jazz:define-syntax module
  (lambda (form-src)
    (let ((emit (jazz:generate-module (%%cdr (jazz:source-code form-src)))))
      (jazz:save-emit-if emit)
      emit)))


(jazz:define-syntax script
  (lambda (form-src)
    (jazz:generate-script (%%cdr (jazz:source-code form-src)))))


;;;
;;;; Module
;;;


(jazz:define-class-syntax jazz:Module jazz:Object (constructor: jazz:allocate-module accessors-type: macro)
  ((name    getter: generate)
   (access  getter: generate)
   (exports getter: generate)
   (entries getter: generate setter: generate)))


(jazz:define-class-runtime jazz:Module)


(define (jazz:new-module name access)
  (jazz:allocate-module name access (%%make-table test: eq?) #f))


;;;
;;;; Native
;;;


(jazz:define-class-syntax jazz:Native jazz:Field (constructor: jazz:allocate-native accessors-type: macro)
  ((symbol getter: generate)))


(jazz:define-class-runtime jazz:Native)


(define (jazz:new-native name symbol)
  (jazz:allocate-native name symbol))


(define (jazz:register-native module-name name symbol)
  (jazz:register-module-entry module-name name (jazz:new-native name symbol)))


;;;
;;;; Runtime Reference
;;;


(jazz:define-class-syntax jazz:Runtime-Reference jazz:Object (constructor: jazz:allocate-runtime-reference accessors-type: macro)
  ((resolver      getter: generate)
   (serialization getter: generate)))


(jazz:define-class-runtime jazz:Runtime-Reference)


(define (jazz:new-runtime-reference resolver serialization)
  (jazz:allocate-runtime-reference resolver serialization))


(define (jazz:resolve-runtime-reference runtime-reference)
  (%%debug-assert (%%is? runtime-reference jazz:Runtime-Reference)
    (let ((resolver (%%get-runtime-reference-resolver runtime-reference)))
      (resolver))))


(define (jazz:serialize-runtime-reference runtime-reference)
  (%%debug-assert (%%is? runtime-reference jazz:Runtime-Reference)
    (%%get-runtime-reference-serialization runtime-reference)))


(define (jazz:deserialize-runtime-reference serialization)
  (define (deserialize-module-private)
    (jazz:new-runtime-reference (lambda ()
                                  (let ((locator (%%cadr serialization)))
                                    (jazz:load-unit (jazz:reference-unit locator))
                                    (jazz:global-ref locator)))
                                serialization))
  
  (define (deserialize-module-public)
    (jazz:new-runtime-reference (lambda ()
                                  (let ((module-name (%%cadr serialization))
                                        (name (%%car (%%cddr serialization))))
                                    (jazz:module-ref module-name name)))
                                serialization))
  
  (or (if (%%pair? serialization)
          (case (%%car serialization)
            ((module-private) (deserialize-module-private))
            ((module-public) (deserialize-module-public))
            (else #f))
        #f)
      (jazz:error "Unable to deserialize runtime reference: {s}" serialization)))


;;;
;;;; Modules
;;;


(define jazz:Modules
  (%%make-table test: eq?))


(define (jazz:get-modules-table)
  jazz:Modules)


(define (jazz:register-module name access exported-modules exported-symbols)
  (let ((module (or (jazz:find-module name) (jazz:new-module name access))))
    (let ((exports (%%get-module-exports module)))
      (for-each (lambda (module-name)
                  (jazz:iterate-table (%%get-module-exports (jazz:require-module module-name))
                    (lambda (name info)
                      (%%table-set! exports name info))))
                exported-modules)
      (for-each (lambda (pair)
                  (let ((name (%%car pair))
                        (info (%%cdr pair)))
                    (%%table-set! exports name info)))
                exported-symbols)
      (%%table-set! jazz:Modules name module)
      module)))


(define (jazz:find-module name)
  (%%table-ref jazz:Modules name #f))


(define (jazz:require-module name)
  (jazz:load-unit name)
  (or (jazz:find-module name)
      (jazz:error "Unknown module: {s}" name)))


(define (jazz:get-module-entry module-name entry-name)
  (let ((entries (%%get-module-entries (jazz:find-module module-name))))
    (and entries (%%table-ref entries entry-name #f))))

(define (jazz:set-module-entry module-name entry-name entry)
  (let ((module (jazz:find-module module-name)))
    (let ((entries (%%get-module-entries module)))
      (if entries
          (%%table-set! entries entry-name entry)
        (let ((table (%%make-table test: eq?)))
          (%%set-module-entries module table)
          (%%table-set! table entry-name entry))))))

(define (jazz:register-module-entry module-name entry-name entry)
  (jazz:set-module-entry module-name entry-name entry))


(define (jazz:module-get module-name name #!key (not-found #f))
  (let ((module (jazz:require-module module-name)))
    (let ((info (%%table-ref (%%get-module-exports module) name #f)))
      (if info
          (if (%%symbol? info)
              (jazz:global-ref info)
            (jazz:bind (unit-name . locator) info
              (jazz:load-unit unit-name)
              (jazz:global-ref locator)))
        not-found))))


(define jazz:module-ref
  (let ((not-found (box #f)))
    (lambda (module-name name)
      (let ((obj (jazz:module-get module-name name not-found: not-found)))
        (if (%%eq? obj not-found)
            (jazz:error "Unable to find '{s} in: {s}" name module-name)
          obj)))))


(define (jazz:module-set! module-name name value)
  (let ((module (jazz:require-module module-name)))
    (let ((info (%%table-ref (%%get-module-exports module) name #f)))
      (if info
          (if (%%symbol? info)
              (jazz:global-set! info value)
            (jazz:bind (unit-name . locator) info
              (jazz:load-unit unit-name)
              (jazz:global-set! locator value)))
        (jazz:error "Unable to find '{s} in: {s}" name module-name)))))


;;;
;;;; Error
;;;


(define (jazz:type-error value type)
  (jazz:error "{s} expected: {s}" type value))


(define (jazz:dispatch-error field value category)
  (jazz:error "Inconsistent dispatch on method {a}: {s} is not of the expected {s} type" (%%get-field-name field) value (%%get-category-identifier category))))
