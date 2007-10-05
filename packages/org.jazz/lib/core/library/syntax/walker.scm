;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Libraries
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


;; Concepts
;; - lookup: direct lookup of a symbol returning a declaration
;; - resolve: references are resolved only on first usage
;; - expand take a source code expr and expand it to equivalent scheme code
;;   - walk: take a source code expr and generate its expression tree
;;   - emit: take an expression tree and emit implementation scheme code
;;
;; Notes
;; - It is key to have compile time reference loading identical to runtime library loading
;;   to ensure that runtime problems are detected at compile time exactly as they would occur
;; - Autoload declarations are treated specially as they are the only case where different
;;   references to the same declaration generate different code: direct access will expand to
;;   just the locator while an autoload access must add code to load the module
;;
;; Todo
;; - Is the extra indirection level of having declaration references really necessary?
;; - Convert and remove the temporary patch jazz.register-autoload that was used to implement
;;   the old load
;; - Think about the check order of imported modules. I do not like that they are checked in
;;   reversed order although with the new lookups the order should be irrelevant as it should be
;; - Cleanup the probably not usefull new method jazz.resolve-declaration that I added to get
;;   things working


(module core.library.syntax.walker


;;;
;;;; Walker
;;;


(define jazz.Warnings?
  #f)


(define jazz.Ignore-Errors?
  #f)


;;;
;;;; Walk Access
;;;


;; access internal to the module
(define jazz.private-access
  0)

;; access from external modules
(define jazz.public-access
  1)

;; access through inheritance
(define jazz.protected-access
  2)


(define (jazz.make-access-lookups access-level)
  (let ((lookups (%%make-vector (%%fx+ access-level 1))))
    (let iter ((n 0))
      (if (%%fx<= n access-level)
          (begin
            (%%vector-set! lookups n (%%make-hashtable eq?))
            (iter (%%fx+ n 1)))))
    lookups))


;;;
;;;; Walk Binding
;;;


(jazz.define-class jazz.Walk-Binding jazz.Object () jazz.Object-Class
  ())


(jazz.define-virtual (jazz.walk-binding-lookup (jazz.Walk-Binding binding) symbol))
(jazz.define-virtual (jazz.emit-binding-reference (jazz.Walk-Binding binding)))
(jazz.define-virtual (jazz.walk-binding-validate-call (jazz.Walk-Binding binding) walker resume source-declaration operator arguments))
(jazz.define-virtual (jazz.emit-binding-call (jazz.Walk-Binding binding) arguments))
(jazz.define-virtual (jazz.walk-binding-assignable? (jazz.Walk-Binding binding)))
(jazz.define-virtual (jazz.emit-binding-assignment (jazz.Walk-Binding binding) value))
(jazz.define-virtual (jazz.walk-binding-walkable? (jazz.Walk-Binding binding)))
(jazz.define-virtual (jazz.walk-binding-walk-form (jazz.Walk-Binding binding) walker resume declaration environment form))
(jazz.define-virtual (jazz.walk-binding-expandable? (jazz.Walk-Binding binding)))
(jazz.define-virtual (jazz.walk-binding-expand-form (jazz.Walk-Binding binding) walker resume declaration environment form))


(jazz.define-method (jazz.walk-binding-lookup (jazz.Walk-Binding binding) symbol)
  #f)


(jazz.define-method (jazz.emit-binding-reference (jazz.Walk-Binding binding))
  (jazz.void))


(jazz.define-method (jazz.walk-binding-validate-call (jazz.Walk-Binding binding) walker resume source-declaration operator arguments)
  (jazz.void))


(jazz.define-method (jazz.emit-binding-call (jazz.Walk-Binding binding) arguments)
  `(,(jazz.emit-binding-reference binding)
    ,@arguments))


(jazz.define-method (jazz.walk-binding-assignable? (jazz.Walk-Binding binding))
  #f)


(jazz.define-method (jazz.emit-binding-assignment (jazz.Walk-Binding binding) value)
  (jazz.void))


(jazz.define-method (jazz.walk-binding-walkable? (jazz.Walk-Binding binding))
  #f)


(jazz.define-method (jazz.walk-binding-expandable? (jazz.Walk-Binding binding))
  #f)


(jazz.encapsulate-class jazz.Walk-Binding)


;;;
;;;; Lexical Binding
;;;


(jazz.define-class jazz.Lexical-Binding jazz.Walk-Binding () jazz.Object-Class
  (name
   type))


(jazz.define-method (jazz.walk-binding-lookup (jazz.Lexical-Binding binding) symbol)
  (if (%%eq? (%%get-lexical-binding-name binding) symbol)
      binding
    #f))


(jazz.encapsulate-class jazz.Lexical-Binding)


;;;
;;;; Declaration
;;;


(jazz.define-class jazz.Declaration jazz.Lexical-Binding (name type) jazz.Object-Class
  (access
   compatibility
   attributes
   toplevel
   parent
   children
   locator))


(define (jazz.setup-declaration new-declaration)
  (%%set-declaration-locator new-declaration (%%apply jazz.compose-name (jazz.get-declaration-path new-declaration)))
  (let ((parent (%%get-declaration-parent new-declaration)))
    (%%set-declaration-toplevel new-declaration (if (%%not parent) new-declaration (%%get-declaration-toplevel parent)))))


(jazz.define-virtual (jazz.resolve-declaration (jazz.Declaration declaration)))


(jazz.define-method (jazz.resolve-declaration (jazz.Declaration declaration))
  declaration)


(jazz.define-virtual (jazz.lookup-declaration (jazz.Declaration declaration) symbol external?))


(define (jazz.get-declaration-path declaration)
  (letrec ((proc
            (lambda (declaration)
              (let ((name (%%get-lexical-binding-name declaration))
                    (parent (%%get-declaration-parent declaration)))
                (if (%%not parent)
                    (%%list name)
                  (%%cons name (proc parent)))))))
    (jazz.reverse! (proc declaration))))


(define (jazz.find-class-declaration declaration)
  (let iter ((decl declaration))
    (cond ((%%not decl)
           (jazz.error "Unable to find class declaration for {s}" declaration))
          ((jazz.is? decl jazz.Class-Declaration)
           decl)
          (else
           (iter (%%get-declaration-parent decl))))))


(jazz.define-method (jazz.walk-binding-lookup (jazz.Declaration binding) symbol)
  (jazz.lookup-declaration binding symbol #f))


(jazz.define-method (jazz.walk-binding-validate-call (jazz.Declaration declaration) walker resume source-declaration operator arguments)
  (jazz.walk-error walker resume source-declaration "{a} is not callable" (%%get-declaration-locator declaration)))


(jazz.define-method (jazz.lookup-declaration (jazz.Declaration declaration) symbol external?)
  #f)


(jazz.define-virtual (jazz.get-declaration-references (jazz.Declaration declaration)))


(jazz.define-method (jazz.get-declaration-references (jazz.Declaration declaration))
  '())


(jazz.define-virtual (jazz.emit-declaration (jazz.Declaration declaration)))


(jazz.define-method (jazz.emit-declaration (jazz.Declaration declaration))
  (jazz.error "Unable to emit: {s}" declaration))


(jazz.define-virtual (jazz.expand-referenced-declaration (jazz.Declaration declaration)))


(jazz.define-method (jazz.expand-referenced-declaration (jazz.Declaration declaration))
  '())


(jazz.encapsulate-class jazz.Declaration)


;;;
;;;; Declaration Reference
;;;


(jazz.define-class jazz.Declaration-Reference jazz.Object () jazz.Object-Class
  (name
   declaration))


(jazz.define-virtual (jazz.resolve-reference (jazz.Declaration-Reference declaration-reference) library-declaration))


(jazz.encapsulate-class jazz.Declaration-Reference)


;;;
;;;; Library Reference
;;;


(jazz.define-class jazz.Library-Reference jazz.Declaration-Reference (name declaration) jazz.Object-Class
  ())


(define (jazz.new-library-reference name declaration)
  (jazz.allocate-library-reference jazz.Library-Reference name declaration))


(jazz.define-method (jazz.resolve-reference (jazz.Library-Reference declaration-reference) library-declaration)
  (or (%%get-declaration-reference-declaration declaration-reference)
      (let ((declaration (jazz.locate-library-declaration (%%get-declaration-reference-name declaration-reference))))
        (%%set-declaration-reference-declaration declaration-reference declaration)
        declaration)))


(jazz.encapsulate-class jazz.Library-Reference)


;;;
;;;; Export Reference
;;;


(jazz.define-class jazz.Export-Reference jazz.Declaration-Reference (name declaration) jazz.Object-Class
  (library-reference))


(define (jazz.new-export-reference name declaration library-reference)
  (jazz.allocate-export-reference jazz.Export-Reference name declaration library-reference))


(jazz.define-method (jazz.resolve-reference (jazz.Declaration-Reference declaration-reference) library-declaration)
  (or (%%get-declaration-reference-declaration declaration-reference)
      (receive (name symbol) (jazz.parse-exported-symbol library-declaration (%%get-declaration-reference-name declaration-reference))
        (let ((declaration (jazz.new-export-declaration name type 'public 'uptodate '() #f symbol)))
          (%%set-declaration-reference-declaration declaration-reference declaration)
          declaration))))


(define (jazz.parse-exported-symbol library-declaration name)
  (if (jazz.composite-name? name)
      (values (jazz.last (jazz.split-identifier name)) name)
    (values name name)))


(jazz.encapsulate-class jazz.Export-Reference)


;;;
;;;; Autoload Reference
;;;


(jazz.define-class jazz.Autoload-Reference jazz.Export-Reference (name declaration library-reference) jazz.Object-Class
  ())


(define (jazz.new-autoload-reference name declaration library-reference)
  (jazz.allocate-autoload-reference jazz.Autoload-Reference name declaration library-reference))


(define (jazz.resolve-autoload-reference declaration-reference library-declaration exported-library-reference)
  (or (%%get-declaration-reference-declaration declaration-reference)
      (let* ((name (%%get-declaration-reference-name declaration-reference))
             (type #f)
             (declaration (jazz.new-autoload-declaration name type #f library-declaration exported-library-reference)))
        (%%assert declaration
          (%%set-declaration-reference-declaration declaration-reference declaration)
          declaration))))


(jazz.encapsulate-class jazz.Autoload-Reference)


;;;
;;;; Module
;;;


(jazz.define-class jazz.Module-Declaration jazz.Declaration (name type access compatibility attributes toplevel parent children locator) jazz.Object-Class
  (requires))


(define (jazz.new-module-declaration name parent requires)
  (let ((new-declaration (jazz.allocate-module-declaration jazz.Module-Declaration name #f 'public 'uptodate '() #f parent '() #f requires)))
    (jazz.setup-declaration new-declaration)
    new-declaration))


(jazz.encapsulate-class jazz.Module-Declaration)


;;;
;;;; Namespace
;;;


(jazz.define-class jazz.Namespace-Declaration jazz.Declaration (name type access compatibility attributes toplevel parent children locator) jazz.Object-Class
  (lookups
   body))


(define (jazz.find-declaration namespace-declaration name)
  (%%hashtable-ref (%%get-access-lookup namespace-declaration jazz.private-access) name #f))


(jazz.encapsulate-class jazz.Namespace-Declaration)


;;;
;;;; Library
;;;


(jazz.define-class jazz.Library-Declaration jazz.Namespace-Declaration (name type access compatibility attributes toplevel parent children locator lookups body) jazz.Object-Class
  (dialect
   requires
   exports
   imports
   literals
   variables
   references
   autoloads))


(define (jazz.new-library-declaration name parent dialect requires exports imports)
  (let ((new-declaration (jazz.allocate-library-declaration jazz.Library-Declaration name #f 'public 'uptodate '() #f parent '() #f (jazz.make-access-lookups jazz.public-access) #f dialect requires exports imports '() '() '() '())))
    (jazz.setup-declaration new-declaration)
    new-declaration))


(define (jazz.setup-library-lookups library-declaration)
  (define (ignore-duplicates key old new)
    #f)
  
  (let ((private (%%get-access-lookup library-declaration jazz.private-access)))
    (for-each (lambda (imported-library-invoice)
                (let ((only (%%get-library-invoice-only imported-library-invoice)))
                  (if only
                      ;; todo
                      #f
                    (let ((imported-library-declaration (%%get-library-invoice-library imported-library-invoice)))
                      (let ((imported (%%get-access-lookup imported-library-declaration jazz.public-access)))
                        (jazz.hashtable-merge private imported ignore-duplicates))))))
              (%%get-library-declaration-imports library-declaration)))
  
  (let ((public (%%get-access-lookup library-declaration jazz.public-access)))
    (for-each (lambda (exported-library-invoice)
                (let ((only (%%get-library-invoice-only exported-library-invoice))
                      (autoload (%%get-export-invoice-autoload exported-library-invoice)))
                  (cond (only
                          (for-each (lambda (declaration-reference)
                                      (let ((name (jazz.identifier-name (%%get-declaration-reference-name declaration-reference))))
                                        (%%hashtable-set! public name (jazz.resolve-reference declaration-reference library-declaration))))
                                    only))
                        (autoload
                          (let ((exported-library-reference (%%get-library-invoice-library exported-library-invoice)))
                            (for-each (lambda (declaration-reference)
                                        (let ((name (jazz.identifier-name (%%get-declaration-reference-name declaration-reference))))
                                          (%%hashtable-set! public name (jazz.resolve-autoload-reference declaration-reference library-declaration exported-library-reference))))
                                      autoload)))
                        (else
                         (let ((exported-library-declaration (jazz.resolve-reference (%%get-library-invoice-library exported-library-invoice) library-declaration)))
                           (jazz.hashtable-merge public (%%get-access-lookup exported-library-declaration jazz.public-access) ignore-duplicates))))))
              (%%get-library-declaration-exports library-declaration))))


(jazz.define-method (jazz.lookup-declaration (jazz.Library-Declaration declaration) symbol external?)
  (let ((access (if external? jazz.public-access jazz.private-access)))
    (%%hashtable-ref (%%vector-ref (%%get-namespace-declaration-lookups declaration) access)
                     symbol
                     #f)))


(jazz.define-method (jazz.emit-declaration (jazz.Library-Declaration declaration))
  (let ((body-expansion (jazz.emit-declarations/expressions (%%get-namespace-declaration-body declaration)))
        (references-expansion (jazz.expand-library-references declaration))
        (literals-expansion (jazz.expand-library-literals declaration))
        (variables-expansion (jazz.expand-library-variables declaration))
        (locator (%%get-declaration-locator declaration)))
    `(begin
       ,@(jazz.declarations 'library)
       ,@(let ((queue (jazz.new-queue)))
           (for-each (lambda (spec)
                       (jazz.parse-require spec
                         (lambda (module-name feature-requirement load phase)
                           (jazz.enqueue queue
                                         (if (%%not feature-requirement)
                                             `(jazz.load-module ',module-name)
                                           `(cond-expand
                                              (,feature-requirement (jazz.load-module ',module-name))
                                              (else)))))))
                     (%%get-library-declaration-requires declaration))
           (for-each (lambda (library-invoice)
                       (let ((only (%%get-library-invoice-only library-invoice))
                             (autoload (%%get-export-invoice-autoload library-invoice)))
                         (cond (only
                                 )
                               (autoload
                                 (let ((module-name (%%get-declaration-reference-name (%%get-library-invoice-library library-invoice))))
                                   (for-each (lambda (decl)
                                               (let ((name (jazz.identifier-name (%%get-declaration-reference-name decl))))
                                                 (jazz.enqueue queue `(jazz.register-autoload ',name ',module-name))))
                                             autoload)))
                               (else
                                (let ((library-declaration (jazz.resolve-reference (%%get-library-invoice-library library-invoice) declaration))
                                      (phase (%%get-library-invoice-phase library-invoice)))
                                  (%%when (and (%%neq? library-declaration declaration) (%%neq? phase 'syntax))
                                    (jazz.enqueue queue `(jazz.load-module ',(%%get-lexical-binding-name library-declaration)))))))))
                     (%%get-library-declaration-exports declaration))
           (for-each (lambda (library-invoice)
                       (let ((library-declaration (%%get-library-invoice-library library-invoice))
                             (phase (%%get-library-invoice-phase library-invoice)))
                         (%%when (and library-declaration (%%neq? phase 'syntax))
                           (jazz.enqueue queue `(jazz.load-module ',(%%get-lexical-binding-name library-declaration))))))
                     (%%get-library-declaration-imports declaration))
           (jazz.queue-list queue))
       ,@references-expansion
       ,@literals-expansion
       ,@variables-expansion
       ,@body-expansion
       (jazz.module-loaded ',locator))))


(jazz.encapsulate-class jazz.Library-Declaration)


;;;
;;;; Library Invoice
;;;


(jazz.define-class jazz.Library-Invoice jazz.Object () jazz.Object-Class
  (library
   phase
   version
   only
   except
   prefix
   rename))


(jazz.encapsulate-class jazz.Library-Invoice)


;;;
;;;; Export Invoice
;;;


(jazz.define-class jazz.Export-Invoice jazz.Library-Invoice (library phase version only except prefix rename) jazz.Object-Class
  (autoload))


(define (jazz.new-export-invoice library phase version only autoload)
  (jazz.allocate-export-invoice jazz.Export-Invoice library phase version only #f #f #f autoload))


(jazz.encapsulate-class jazz.Export-Invoice)


;;;
;;;; Import Invoice
;;;


(jazz.define-class jazz.Import-Invoice jazz.Library-Invoice (library phase version only except prefix rename) jazz.Object-Class
  ())


(define (jazz.new-import-invoice library phase version only)
  (jazz.allocate-import-invoice jazz.Import-Invoice library phase version only #f #f #f))


(jazz.encapsulate-class jazz.Import-Invoice)


;;;
;;;; Export
;;;


(jazz.define-class jazz.Export-Declaration jazz.Declaration (name type access compatibility attributes toplevel parent children locator) jazz.Object-Class
  (symbol))


(define (jazz.new-export-declaration name type access compatibility attributes parent symbol)
  (let ((new-declaration (jazz.allocate-export-declaration jazz.Export-Declaration name type access compatibility attributes #f parent '() #f symbol)))
    (jazz.setup-declaration new-declaration)
    new-declaration))


(jazz.define-method (jazz.walk-binding-validate-call (jazz.Export-Declaration declaration) walker resume source-declaration operator arguments)
  (jazz.void))


(jazz.define-method (jazz.emit-declaration (jazz.Export-Declaration declaration))
  `(begin))


(jazz.define-method (jazz.emit-binding-reference (jazz.Export-Declaration declaration))
  (%%get-export-declaration-symbol declaration))


(jazz.encapsulate-class jazz.Export-Declaration)


;;;
;;;; Autoload
;;;


(jazz.define-class jazz.Autoload-Declaration jazz.Declaration (name type access compatibility attributes toplevel parent children locator) jazz.Object-Class
  (library
   exported-library
   declaration))


(define (jazz.new-autoload-declaration name type parent library-declaration exported-library)
  (let ((new-declaration (jazz.allocate-autoload-declaration jazz.Autoload-Declaration name type 'public 'uptodate '() #f parent '() #f library-declaration exported-library #f)))
    (jazz.setup-declaration new-declaration)
    new-declaration))


(jazz.define-method (jazz.resolve-declaration (jazz.Autoload-Declaration declaration))
  (or (%%get-autoload-declaration-declaration declaration)
      (let* ((exported-library (jazz.resolve-reference (%%get-autoload-declaration-exported-library declaration) (%%get-autoload-declaration-library declaration)))
             (decl (jazz.lookup-declaration exported-library (%%get-lexical-binding-name declaration) #t)))
        (%%set-autoload-declaration-declaration declaration decl)
        decl)))


(jazz.define-method (jazz.lookup-declaration (jazz.Autoload-Declaration declaration) symbol external?)
  (let ((referenced-declaration (jazz.resolve-declaration declaration)))
    ;; not sure this assert is at the right place...
    (%%assertion referenced-declaration (jazz.format "Unable to find autoload: {s}" (%%get-lexical-binding-name declaration))
      (jazz.lookup-declaration referenced-declaration symbol external?))))


;; SHOULD delay getting the refered declaration and when getting it would be the right place to do the register-autoload
(jazz.define-method (jazz.emit-binding-reference (jazz.Autoload-Declaration declaration))
  (let ((library (%%get-autoload-declaration-library declaration)))
    (jazz.register-autoload-declaration library declaration)
    (let ((referenced-declaration (jazz.resolve-declaration declaration)))
      ;; not sure this assert is at the right place...
      (%%assertion referenced-declaration (jazz.format "Unable to find autoload: {s}" (%%get-lexical-binding-name declaration))
        `(begin
           (jazz.load-module ',(%%get-declaration-locator (%%get-declaration-toplevel referenced-declaration)))
           ,(jazz.emit-binding-reference referenced-declaration))))))


(jazz.encapsulate-class jazz.Autoload-Declaration)


;;;
;;;; Macro
;;;


(jazz.define-class jazz.Macro-Declaration jazz.Declaration (name type access compatibility attributes toplevel parent children locator) jazz.Object-Class
  (parameters
   body))


(define (jazz.new-macro-declaration name type access compatibility attributes parent parameters)
  (let ((new-declaration (jazz.allocate-macro-declaration jazz.Macro-Declaration name type access compatibility attributes #f parent '() #f parameters #f)))
    (jazz.setup-declaration new-declaration)
    new-declaration))


(jazz.define-method (jazz.walk-binding-expandable? (jazz.Macro-Declaration declaration))
  #t)


(jazz.define-method (jazz.walk-binding-expand-form (jazz.Macro-Declaration binding) walker resume declaration environment form)
  (let ((locator (%%get-declaration-locator binding)))
    (if (%%eq? (%%get-declaration-toplevel binding) (%%get-declaration-toplevel declaration))
        (jazz.walk-error walker resume declaration "Macros cannot be used from within the same file: {s}" locator)
      (let ((parent-declaration (%%get-declaration-parent binding)))
        (jazz.load-module (%%get-declaration-locator parent-declaration))
        (let ((expander (jazz.need-macro locator)))
          (%%apply expander (%%cdr form)))))))


(jazz.define-method (jazz.emit-declaration (jazz.Macro-Declaration declaration))
  (let ((locator (%%get-declaration-locator declaration))
        (parameters (%%get-macro-declaration-parameters declaration))
        (body (%%get-macro-declaration-body declaration)))
    `(jazz.define-macro ,(%%cons locator parameters)
       ,@(jazz.emit-expression body))))


(jazz.encapsulate-class jazz.Macro-Declaration)


;;;
;;;; Syntax
;;;


(jazz.define-class jazz.Syntax-Declaration jazz.Declaration (name type access compatibility attributes toplevel parent children locator) jazz.Object-Class
  (parameters
   body))


(define (jazz.new-syntax-declaration name type access compatibility attributes parent parameters)
  (let ((new-declaration (jazz.allocate-syntax-declaration jazz.Syntax-Declaration name type access compatibility attributes #f parent '() #f parameters #f)))
    (jazz.setup-declaration new-declaration)
    new-declaration))


(jazz.define-method (jazz.walk-binding-expandable? (jazz.Syntax-Declaration declaration))
  #t)


(jazz.define-method (jazz.walk-binding-expand-form (jazz.Syntax-Declaration binding) walker resume declaration environment form)
  (let ((locator (%%get-declaration-locator binding)))
    (if (%%eq? (%%get-declaration-toplevel binding) (%%get-declaration-toplevel declaration))
        (jazz.walk-error walker resume declaration "Syntaxes cannot be used from within the same file: {s}" locator)
      (let ((parent-declaration (%%get-declaration-parent binding)))
        (jazz.load-module (%%get-declaration-locator parent-declaration))
        (let ((expander (jazz.need-macro locator)))
          (%%apply expander (%%cdr form)))))))


(jazz.define-method (jazz.emit-declaration (jazz.Syntax-Declaration declaration))
  (let ((locator (%%get-declaration-locator declaration))
        (parameters (%%get-syntax-declaration-parameters declaration))
        (body (%%get-syntax-declaration-body declaration)))
    `(jazz.define-macro ,(%%cons locator parameters)
       ,@(jazz.emit-expression body))))


(jazz.encapsulate-class jazz.Syntax-Declaration)


;;;
;;;; C Type
;;;


(jazz.define-class jazz.C-Type-Declaration jazz.Declaration (name type access compatibility attributes toplevel parent children locator) jazz.Object-Class
  (kind
   expansion
   references))


(define (jazz.new-c-type-declaration name type access compatibility attributes parent kind expansion references)
  (let ((new-declaration (jazz.allocate-c-type-declaration jazz.C-Type-Declaration name type access compatibility attributes #f parent '() #f kind expansion references)))
    (jazz.setup-declaration new-declaration)
    new-declaration))


(jazz.define-method (jazz.get-declaration-references (jazz.C-Type-Declaration declaration))
  (%%get-c-type-declaration-references declaration))


(jazz.define-method (jazz.emit-declaration (jazz.C-Type-Declaration declaration))
  `(begin))


(jazz.define-method (jazz.expand-referenced-declaration (jazz.C-Type-Declaration declaration))
  (let ((locator (%%get-declaration-locator declaration))
        (expansion (%%get-c-type-declaration-expansion declaration)))
    `(c-define-type ,locator ,expansion)))


(jazz.encapsulate-class jazz.C-Type-Declaration)


;;;
;;;; C Definition
;;;


(jazz.define-class jazz.C-Definition-Declaration jazz.Declaration (name type access compatibility attributes toplevel parent children locator) jazz.Object-Class
  (signature
   parameter-types
   result-type
   c-name
   scope
   body))


(define (jazz.new-c-definition-declaration name type access compatibility attributes parent parameters parameter-types result-type c-name scope)
  (let ((new-declaration (jazz.allocate-c-definition-declaration jazz.C-Definition-Declaration name type access compatibility attributes #f parent '() #f (and parameters (jazz.new-signature parameters)) parameter-types result-type c-name scope #f)))
    (jazz.setup-declaration new-declaration)
    new-declaration))


(jazz.define-method (jazz.walk-binding-validate-call (jazz.C-Definition-Declaration declaration) walker resume source-declaration operator arguments)
  (let ((signature (%%get-c-definition-declaration-signature declaration)))
    (if signature
        (jazz.validate-arguments walker resume source-declaration declaration signature arguments))))


(jazz.define-method (jazz.emit-declaration (jazz.C-Definition-Declaration declaration))
  (let ((locator (%%get-declaration-locator declaration))
        (parameters (%%get-signature-parameters (%%get-c-definition-declaration-signature declaration)))
        (parameter-types (%%get-c-definition-declaration-parameter-types declaration))
        (result-type (%%get-c-definition-declaration-result-type declaration))
        (c-name (%%get-c-definition-declaration-c-name declaration))
        (scope (%%get-c-definition-declaration-scope declaration))
        (body (%%get-c-definition-declaration-body declaration)))
    `(c-define ,(%%cons locator parameters) ,parameter-types ,result-type ,c-name ,scope
       ,@(jazz.emit-expression body))))


(jazz.define-method (jazz.emit-binding-reference (jazz.C-Definition-Declaration declaration))
  (%%get-declaration-locator declaration))


(jazz.encapsulate-class jazz.C-Definition-Declaration)


;;;
;;;; Walk Context
;;;


(jazz.define-class jazz.Walk-Context jazz.Object () jazz.Object-Class
  (policy
   locator
   pathname))


(define (jazz.new-walk-context policy locator pathname)
  (jazz.allocate-walk-context jazz.Walk-Context policy locator pathname))


(define jazz.walk-context
  (make-parameter #f))


(define (jazz.get-walk-context)
  (jazz.walk-context))


(define (jazz.need-walk-context)
  (or (jazz.walk-context)
      (jazz.error "There is no active walk context")))


(define (jazz.get-walk-policy)
  (let ((context (jazz.get-walk-context)))
    (if (%%not context)
        #f
      (%%get-walk-context-policy context))))


(define (jazz.get-walk-locator)
  (let ((context (jazz.get-walk-context)))
    (if (%%not context)
        #f
      (%%get-walk-context-locator context))))


(define (jazz.get-walk-pathname)
  (let ((context (jazz.get-walk-context)))
    (if (%%not context)
        #f
      (%%get-walk-context-pathname context))))


(jazz.encapsulate-class jazz.Walk-Context)


;;;
;;;; Walk Location
;;;


(jazz.define-class jazz.Walk-Location jazz.Object () jazz.Object-Class
  (module-locator
   declaration-locator))


(define (jazz.new-walk-location module-locator declaration-locator)
  (jazz.allocate-walk-location jazz.Walk-Location module-locator declaration-locator))


(define (jazz.walk-location walker declaration)
  (jazz.new-walk-location
    (jazz.get-walk-locator)
    (%%get-declaration-locator declaration)))


(jazz.encapsulate-class jazz.Walk-Location)


;;;
;;;; Walk Error
;;;


(jazz.define-class jazz.Walk-Error jazz.Error (message) jazz.Object-Class
  (location))


(define (jazz.new-walk-error location message)
  (jazz.allocate-walk-error jazz.Walk-Error message location))


(jazz.define-method (jazz.present-exception (jazz.Walk-Error error))
  (%%get-error-message error))


(jazz.encapsulate-class jazz.Walk-Error)


;;;
;;;; Unresolved Error
;;;


(jazz.define-class jazz.Unresolved-Error jazz.Walk-Error (message location) jazz.Object-Class
  (symbol))


(define (jazz.new-unresolved-error location symbol)
  (jazz.allocate-unresolved-error jazz.Unresolved-Error #f location symbol))


(jazz.define-method (jazz.present-exception (jazz.Unresolved-Error error))
  (jazz.format "Unresolved symbol: {s}"
               (%%get-unresolved-error-symbol error)))


(jazz.encapsulate-class jazz.Unresolved-Error)


;;;
;;;; Walk Frame
;;;


(jazz.define-class jazz.Walk-Frame jazz.Walk-Binding () jazz.Object-Class
  (bindings))


(define (jazz.new-walk-frame bindings)
  (let ((hashtable (%%make-hashtable eq?)))
    (for-each (lambda (binding)
                (let ((name (%%get-lexical-binding-name binding)))
                  (%%hashtable-set! hashtable name binding)))
              bindings)
    (jazz.allocate-walk-frame jazz.Walk-Frame hashtable)))


(jazz.define-method (jazz.walk-binding-lookup (jazz.Walk-Frame binding) symbol)
  (%%hashtable-ref (%%get-walk-frame-bindings binding) symbol #f))


(jazz.encapsulate-class jazz.Walk-Frame)


;;;
;;;; Signature
;;;


(jazz.define-class jazz.Signature jazz.Object () jazz.Object-Class
  (parameters
   mandatory
   rest?))


(define (jazz.new-signature parameters)
  (let ((allocate
         (lambda (mandatory rest?)
           (jazz.allocate-signature jazz.Signature parameters mandatory rest?))))
    (if (%%symbol? parameters)
        (allocate 0 #t)
      (let ((rest (jazz.last-pair parameters)))
        (if (%%symbol? rest)
            (allocate (%%length (jazz.proper-list parameters)) #t)
          (allocate (%%length parameters) #f))))))


(jazz.encapsulate-class jazz.Signature)


;;;
;;;; Symbol Binding
;;;


(jazz.define-class jazz.Symbol-Binding jazz.Lexical-Binding (name type) jazz.Object-Class
  ())


(jazz.encapsulate-class jazz.Symbol-Binding)


;;;
;;;; Variable
;;;


(jazz.define-class jazz.Variable jazz.Symbol-Binding (name type) jazz.Object-Class
  ())


(define (jazz.new-variable name type)
  (jazz.allocate-variable jazz.Variable name type))


(jazz.define-method (jazz.emit-binding-reference (jazz.Variable binding))
  (%%get-lexical-binding-name binding))


(jazz.define-method (jazz.walk-binding-assignable? (jazz.Variable declaration))
  #t)


(jazz.define-method (jazz.emit-binding-assignment (jazz.Variable binding) value)
  `(set! ,(%%get-lexical-binding-name binding) ,(jazz.emit-expression value)))


(jazz.encapsulate-class jazz.Variable)


;;;
;;;; RestVariable
;;;


(jazz.define-class jazz.RestVariable jazz.Variable (name type) jazz.Object-Class
  ())


(define (jazz.new-restvariable name type)
  (jazz.allocate-restvariable jazz.RestVariable name type))


(jazz.encapsulate-class jazz.RestVariable)


;;;
;;;; NextMethodVariable
;;;


(jazz.define-class jazz.NextMethodVariable jazz.Variable (name type) jazz.Object-Class
  ())


(define (jazz.new-nextmethodvariable name type)
  (jazz.allocate-nextmethodvariable jazz.NextMethodVariable name type))


(jazz.define-method (jazz.emit-binding-reference (jazz.NextMethodVariable binding))
  (let ((name (%%get-lexical-binding-name binding))
        (self (jazz.lookup-self)))
    (if self
        `(lambda rest (apply ,name self rest))
      name)))


(jazz.encapsulate-class jazz.NextMethodVariable)


;;;
;;;; Self-Binding
;;;


;; Support for dialects that have an implicit self concept


(jazz.define-class jazz.Self-Binding jazz.Lexical-Binding (name type) jazz.Object-Class
  ())


(define (jazz.new-self-binding type)
  (jazz.allocate-self-binding jazz.Self-Binding 'self type))


(jazz.define-method (jazz.emit-binding-reference (jazz.Self-Binding declaration))
  'self)


(jazz.encapsulate-class jazz.Self-Binding)


;;;
;;;; Macro Symbol
;;;


(jazz.define-class jazz.Macro-Symbol jazz.Symbol-Binding (name type) jazz.Object-Class
  (getter
   setter))


(define (jazz.new-macro-symbol name getter setter)
  (jazz.allocate-macro-symbol jazz.Macro-Symbol name #f getter setter))


#; ;; convert to walk / emit
(jazz.define-method (jazz.emit-binding-reference (jazz.Macro-Symbol binding))
  (let ((getter (%%get-macro-symbol-getter binding)))
    (jazz.walk walker resume source-declaration environment (getter))))


#; ;; convert to walk / emit
(jazz.define-method (jazz.walk-binding-assignable? (jazz.Macro-Symbol declaration))
  #t)


#; ;; convert to walk / emit
(jazz.define-method (jazz.emit-binding-assignment (jazz.Macro-Symbol binding) value)
  (let ((setter (%%get-macro-symbol-setter binding)))
    (jazz.walk walker resume source-declaration environment (setter value))))


(jazz.encapsulate-class jazz.Macro-Symbol)


;;;
;;;; Form Binding
;;;


(jazz.define-class jazz.Form-Binding jazz.Lexical-Binding (name type) jazz.Object-Class
  ())


(jazz.encapsulate-class jazz.Form-Binding)


;;;
;;;; Special Form
;;;


(jazz.define-class jazz.Special-Form jazz.Form-Binding (name type) jazz.Object-Class
  (walk))


(define (jazz.new-special-form name walk)
  (jazz.allocate-special-form jazz.Special-Form name #f walk))


(jazz.define-method (jazz.walk-binding-walkable? (jazz.Special-Form binding))
  #t)


(jazz.define-method (jazz.walk-binding-walk-form (jazz.Special-Form binding) walker resume declaration environment form)
  (let ((walk (%%get-special-form-walk binding)))
    (walk walker resume declaration environment form)))


(jazz.encapsulate-class jazz.Special-Form)


;;;
;;;; Macro Form
;;;


(jazz.define-class jazz.Macro-Form jazz.Form-Binding (name type) jazz.Object-Class
  (expander))


(define (jazz.new-macro-form name expander)
  (jazz.allocate-macro-form jazz.Macro-Form name #f expander))


(jazz.define-method (jazz.walk-binding-expandable? (jazz.Macro-Form binding))
  #t)


(jazz.define-method (jazz.walk-binding-expand-form (jazz.Macro-Form binding) walker resume declaration environment form)
  (let ((expander (%%get-macro-form-expander binding)))
    (apply expander walker resume declaration environment (%%cdr form))))


(jazz.encapsulate-class jazz.Macro-Form)


;;;
;;;; Access
;;;


(jazz.define-class jazz.Access jazz.Object () jazz.Object-Class
  (name
   context))


(define (jazz.new-access name context)
  (jazz.allocate-access jazz.Access name context))


(define (jazz.walk-access walker resume declaration environment access)
  (let ((name (%%get-access-name access))
        (context (%%get-access-context access)))
    (if (%%eq? context 'self)
        (let ((slot-declaration (jazz.lookup-declaration (jazz.find-class-declaration declaration) name #f)))
          (%%assert (%%is? slot-declaration jazz.Slot-Declaration)
            (jazz.new-reference slot-declaration)))
      (jazz.new-slot-reference declaration name
        (jazz.walk walker resume declaration environment context)))))


(define (jazz.walk-access-assignment walker resume declaration environment access value)
  (let ((name (%%get-access-name access))
        (context (%%get-access-context access)))
    (if (%%eq? context 'self)
        (let ((slot-declaration (jazz.lookup-declaration (jazz.find-class-declaration declaration) name #f)))
          (%%assert (%%is? slot-declaration jazz.Slot-Declaration)
            (jazz.new-assignment slot-declaration (jazz.walk walker resume declaration environment value))))
      (jazz.new-slot-assignment declaration name
        (jazz.walk walker resume declaration environment context)
        (jazz.walk walker resume declaration environment value)))))


(jazz.encapsulate-class jazz.Access)


;;;
;;;; Walker
;;;


(jazz.define-class jazz.Walker jazz.Object () jazz.Object-Class
  (warnings
   errors))


;;;
;;;; Problems
;;;


(define (jazz.walk-warning walker declaration fmt-string . rest)
  (let ((location (jazz.walk-location walker declaration))
        (message (apply jazz.format fmt-string rest)))
    (jazz.walker-warning walker (jazz.new-walk-error location message))))


(define (jazz.walk-error walker resume declaration fmt-string . rest)
  (%%when (%%not jazz.Ignore-Errors?)
    (let ((location (jazz.walk-location walker declaration))
          (message (apply jazz.format fmt-string rest)))
      (jazz.walker-error walker resume (jazz.new-walk-error location message)))))


(define (jazz.walk-unresolved walker resume declaration symbol)
  (%%when (%%not jazz.Ignore-Errors?)
    (let ((location (jazz.walk-location walker declaration)))
      (jazz.walker-error walker resume (jazz.new-unresolved-error location symbol)))))


(define (jazz.walker-warning walker warning)
  (if jazz.Warnings?
      (%%set-walker-warnings walker (%%append (%%get-walker-warnings walker) (%%list warning)))))


(define (jazz.walker-error walker resume error)
  (%%set-walker-errors walker (%%append (%%get-walker-errors walker) (%%list error)))
  (if (and resume jazz.Delay-Reporting?)
      (resume (jazz.void))
    (jazz.validate-walk-problems walker)))


(define (jazz.validate-walk-problems walker)
  (let ((warnings (%%get-walker-warnings walker))
        (errors (%%get-walker-errors walker)))
    (%%when (or (%%not-null? warnings) (%%not-null? errors))
      (let ((output (open-output-string))
            (all (%%append warnings errors)))
        (jazz.format output "Walk problems encountered:{%}")
        (for-each (lambda (partition)
                    (jazz.bind (module-locator . all) partition
                      (jazz.format output "  In {a}" (or module-locator "<console>"))
                      (let ((prefix (if (%%not module-locator) -1 (%%string-length (%%symbol->string module-locator)))))
                        (for-each (lambda (exception)
                                    (let ((locator (%%symbol->string (%%get-walk-location-declaration-locator (%%get-walk-error-location exception)))))
                                      (jazz.format output "{%}    At {a}: {a}"
                                                   (if (%%fx= (%%string-length locator) prefix)
                                                       ""
                                                     (%%substring locator (%%fx+ prefix 1) (%%string-length locator)))
                                                   (jazz.present-exception exception))))
                                  all))))
                  (jazz.partition all (lambda (error)
                                        (%%get-walk-location-module-locator (%%get-walk-error-location error)))))
        (jazz.error "{a}" (get-output-string output))))))


;;;
;;;; Parse
;;;


(define jazz.unspecified
  'jazz.unspecified)


(define (jazz.unspecified? expr)
  (%%eq? expr jazz.unspecified))


(define (jazz.specified? expr)
  (%%neq? expr jazz.unspecified))


(define (jazz.parse-modifiers walker resume declaration infos rest)
  (let ((partitions (map (lambda (info) (%%cons info '())) infos))
        (done? #f))
    (%%while (and (%%not-null? rest) (%%not done?))
      (let ((target (%%car rest))
            (found? #f))
        (for-each (lambda (partition)
                    (let ((allowed (%%caar partition))
                          (default (%%cdar partition)))
                      (if (%%memq target allowed)
                          (begin
                            (set! found? #t)
                            (%%set-cdr! partition (%%cons target (%%cdr partition)))))))
                  partitions)
        (if (%%not found?)
            (set! done? #t)
          (set! rest (%%cdr rest)))))
    (%%apply values (%%append (map (lambda (partition)
                                     (let ((modifiers (%%cdr partition)))
                                       (cond ((%%null? modifiers) (%%cdar partition))
                                         ((%%null? (%%cdr modifiers)) (%%car modifiers))
                                         (else (jazz.walk-error walker resume declaration "Ambiguous modifiers: {s}" modifiers)))))
                                   partitions)
                              (%%list rest)))))


;;;
;;;; Module
;;;


(define (jazz.parse-module-declaration partial-form)
  (let ((name (%%car partial-form))
        (rest (%%cdr partial-form)))
    (jazz.parse-module rest
      (lambda (requires body)
        (jazz.new-module-declaration name #f requires)))))


;;;
;;;; Library
;;;


(define (jazz.parse-library partial-form)
  (let ((name (%%car partial-form))
        (dialect-name (%%cadr partial-form))
        (scan (%%cddr partial-form))
        (requires '())
        (exports '())
        (imports '()))
    (if (and (%%pair? scan)
             (%%pair? (%%car scan))
             (%%eq? (%%caar scan) 'require))
        (begin
          (set! requires (%%cdar scan))
          (set! scan (%%cdr scan))))
    (if (and (%%pair? scan)
             (%%pair? (%%car scan))
             (%%eq? (%%caar scan) 'export))
        (begin
          (set! exports (%%cdar scan))
          (set! scan (%%cdr scan))))
    (if (and (%%pair? scan)
             (%%pair? (%%car scan))
             (%%eq? (%%caar scan) 'import))
        (begin
          (set! imports (%%cdar scan))
          (set! scan (%%cdr scan))))
      (values name
              dialect-name
              (jazz.filter-features requires)
              (jazz.filter-features exports)
              (jazz.filter-features imports)
              scan)))


(define (jazz.parse-library-invoice specification)
  (%%assertion (%%pair? specification) (jazz.format "Ill-formed library invoice: {s}" specification)
    (let ((name (%%car specification))
          (scan (%%cdr specification))
          (version '())
          (load #f)
          (phase #f)
          (only #f)
          (autoload #f))
      (%%while (and (%%pair? scan)
                    (%%not (%%pair? (%%car scan))))
               (set! version (%%cons (%%car scan) version))
               (set! scan (%%cdr scan)))
      (if (and (%%pair? scan)
               (%%pair? (%%car scan))
               (%%eq? (%%caar scan) 'load))
          (begin
            (set! load (%%car (%%cdar scan)))
            (set! scan (%%cdr scan))))
      (if (and (%%pair? scan)
               (%%pair? (%%car scan))
               (%%eq? (%%caar scan) 'phase))
          (begin
            (set! phase (%%car (%%cdar scan)))
            (set! scan (%%cdr scan))))
      (if (and (%%pair? scan)
               (%%pair? (%%car scan))
               (%%eq? (%%caar scan) 'only))
          (begin
            (set! only (%%cdar scan))
            (set! scan (%%cdr scan))))
      (if (and (%%pair? scan)
               (%%pair? (%%car scan))
               (%%eq? (%%caar scan) 'autoload))
          (begin
            (set! autoload (%%cdar scan))
            (set! scan (%%cdr scan))))
      (values name load phase (%%reverse version) only autoload))))


(define (jazz.parse-library-declaration partial-form)
  (receive (name dialect-name requires exports imports body) (jazz.parse-library partial-form)
    (let ((walker (jazz.dialect-walker (jazz.load-dialect dialect-name))))
      (jazz.walk-library-declaration walker name dialect-name requires exports imports body))))


(define (jazz.walk-library-declaration walker name dialect-name requires exports imports body)
  (let ((exports (%%reverse (jazz.walk-library-exports walker exports)))
        (imports (%%reverse (jazz.walk-library-imports walker (jazz.add-dialect-import imports dialect-name)))))
    (let ((declaration (jazz.new-library-declaration name #f dialect-name requires exports imports)))
      (jazz.load-library-syntax declaration)
      (jazz.setup-library-lookups declaration)
      (jazz.walk-declarations walker #f declaration (%%cons declaration (jazz.walker-environment walker)) body)
      (jazz.validate-walk-problems walker)
      declaration)))


(define (jazz.load-library-syntax declaration)
  (for-each (lambda (library-invoice)
              (%%when (%%eq? (%%get-library-invoice-phase library-invoice) 'syntax)
                (let ((library-declaration (jazz.resolve-reference (%%get-library-invoice-library library-invoice) declaration)))
                  (jazz.load-module (%%get-lexical-binding-name library-declaration)))))
            (%%get-library-declaration-exports declaration))
  (for-each (lambda (library-invoice)
              (%%when (%%eq? (%%get-library-invoice-phase library-invoice) 'syntax)
                (let ((library-declaration (%%get-library-invoice-library library-invoice)))
                  (jazz.load-module (%%get-lexical-binding-name library-declaration)))))
            (%%get-library-declaration-imports declaration)))


(define (jazz.walk-library-exports walker exports)
  (let ((partition (jazz.partition exports symbol?)))
    (let ((symbols-exports (assq #t partition))
          (library-exports (assq #f partition)))
      (%%append (if symbols-exports
                    (%%list (jazz.new-export-invoice #f #f '() (map (lambda (symbol) (jazz.new-export-reference symbol #f #f)) (%%cdr symbols-exports)) #f))
                  '())
                (if library-exports
                    (map (lambda (export)
                           (receive (library-name library-load library-phase library-version library-only library-autoload) (jazz.parse-library-invoice export)
                             (let ((library-reference (jazz.new-library-reference library-name #f)))
                               (jazz.new-export-invoice library-reference
                                                        library-phase
                                                        library-version
                                                        (if (%%not library-only)
                                                            #f
                                                          (map (lambda (symbol)
                                                                 (jazz.new-export-reference symbol #f #f))
                                                               library-only))
                                                        (if (%%not library-autoload)
                                                            #f
                                                          (map (lambda (symbol)
                                                                 (jazz.new-autoload-reference symbol #f #f))
                                                               library-autoload))))))
                         (%%cdr library-exports))
                  '())))))


(define (jazz.walk-library-imports walker imports)
  (map (lambda (import)
         (receive (library-name library-load library-phase library-version library-only library-autoload) (jazz.parse-library-invoice import)
           (jazz.new-import-invoice (jazz.lookup-library walker #f #f '() library-name)
                                    library-phase
                                    library-version
                                    (if (%%not library-only)
                                        #f
                                      (map (lambda (symbol)
                                             (jazz.new-export-reference symbol #f #f))
                                           library-only)))))
       imports))


(define (jazz.expand-library partial-form)
  (jazz.emit-declaration (jazz.walk-library partial-form)))


(define (jazz.walk-library partial-form)
  (receive (name dialect-name requires exports imports body) (jazz.parse-library partial-form)
    (let* ((dialect (jazz.load-dialect dialect-name))
           (walker (jazz.dialect-walker dialect))
           (resume #f)
           (declaration (jazz.walk-library-declaration walker name dialect-name requires exports imports body))
           (environment (%%cons declaration (jazz.walker-environment walker))))
      (%%set-namespace-declaration-body declaration
        (jazz.walk-list walker resume declaration environment body))
      declaration)))


(define (jazz.load-dialect dialect-name)
  (if (%%not (%%symbol? dialect-name))
      (jazz.error "Dialect name must be a symbol: {s}" dialect-name)
    (begin
      (if (%%neq? dialect-name 'core)
          (jazz.load-module dialect-name))
      (jazz.require-dialect dialect-name))))


(define (jazz.add-dialect-import imports dialect-name)
  (if (%%eq? dialect-name 'core)
      imports
    (%%append imports (%%list (%%list dialect-name)))))


(define (jazz.expand-library-references library-declaration)
  (let ((queue (jazz.new-queue)))
    (letrec ((collect-declarations
              (lambda (declaration)
                (for-each collect-declarations (jazz.get-declaration-references declaration))
                (if (%%not (%%memq declaration (jazz.queue-list queue)))
                    (jazz.enqueue queue declaration)))))
      (for-each collect-declarations (%%get-library-declaration-references library-declaration))
      (map (lambda (declaration)
             (jazz.expand-referenced-declaration declaration))
           (jazz.queue-list queue)))))


(define (jazz.expand-library-literals library-declaration)
  (map (lambda (info)
         (let ((name (%%cadr info))
               (value (%%cddr info)))
           `(define ,name ,(jazz.emit-expression value))))
       (%%get-library-declaration-literals library-declaration)))


(define (jazz.expand-library-variables library-declaration)
  (map (lambda (variable)
         `(define ,variable #f))
       (%%get-library-declaration-variables library-declaration)))


;;;
;;;; Environment
;;;


(define (jazz.core-bindings)
  (%%list
    (jazz.new-special-form 'native jazz.walk-native)
    
    (jazz.new-macro-form 'macro    jazz.expand-macro)  (jazz.new-special-form '%macro  jazz.walk-%macro)
    (jazz.new-macro-form 'syntax   jazz.expand-syntax) (jazz.new-special-form '%syntax jazz.walk-%syntax)))


(jazz.define-virtual (jazz.walker-environment (jazz.Walker walker)))


(jazz.define-method (jazz.walker-environment (jazz.Walker walker))
  (%%list (jazz.new-walk-frame (jazz.core-bindings))))


;;;
;;;; Declaration
;;;


;; In order to be able to resolve internal declarations as we walk the code, the declaration
;; tree is pre-expanded including expanding macros when needed. Then, during the actual walk
;; we just find the declarations in this tree. This tree is also merged with any preexisting
;; declaration tree coming from the runtime catalog.


;; For a module, declaration will be #f
(define (jazz.walk/find-declaration walker resume name module declaration environment form)
  (if (%%not declaration)
      (jazz.merge-toplevel-declarations walker resume name module (jazz.walk-declaration walker resume #f environment form))
    (jazz.find-form-declaration declaration (%%cadr form))))


;; Note that this merging should not be done while recursively building the new-declaration
;; in case an error occurs in which case we do have to leave the original declarations intact
(define (jazz.merge-toplevel-declarations walker resume name module new-declaration)
  (let ((old-declaration (jazz.find-module-declaration name module)))
    (cond ((%%not old-declaration)
           new-declaration)
          (else
           (jazz.merge-declaration-into new-declaration old-declaration)
           old-declaration))))


(define (jazz.find-module-declaration name module)
  (let ((entry (jazz.get-validated-catalog-entry (jazz.compose-module-name name module))))
    (if (%%not entry)
        #f
      (%%get-declaration-parent entry))))


(define (jazz.set-module-declaration name module new-declaration)
  (let ((module-declaration (if (%%not module) new-declaration (jazz.find-declaration new-declaration module))))
    (jazz.set-catalog-entry (jazz.get-walk-locator) module-declaration)))


(define (jazz.compose-module-name name module)
  (if (%%not module)
      name
    (jazz.compose-name name module)))


(define (jazz.merge-declaration-into new-declaration old-declaration)
  (jazz.update-declaration-from old-declaration new-declaration)
  (for-each (lambda (new-child)
              (let ((name (%%get-lexical-binding-name new-child)))
                (let ((old-child (jazz.find-declaration old-declaration name)))
                  (if old-child
                      (jazz.merge-declaration-into new-child old-child)
                    (begin
                      (%%set-declaration-parent new-child old-declaration)
                      (%%set-declaration-children old-declaration (%%append (%%get-declaration-children old-declaration) (%%list new-child))))))))
            (%%get-declaration-children new-declaration)))


;; This should either update the old declaration or throw an error if it cannot
(define (jazz.update-declaration-from old-declaration new-declaration)
  #f)


(jazz.define-virtual (jazz.walk-declaration (jazz.Walker walker) resume declaration environment form))


(jazz.define-method (jazz.walk-declaration (jazz.Walker walker) resume declaration environment form)
  (if (%%pair? form)
      (let ((first (%%car form)))
        (case first
          ((native)  (jazz.walk-native-declaration  walker resume declaration environment form))
          ((%macro)  (jazz.walk-%macro-declaration  walker resume declaration environment form))
          ((%syntax) (jazz.walk-%syntax-declaration walker resume declaration environment form))
          (else      #f)))
    #f))


(define (jazz.walk-declarations walker resume declaration environment forms)
  (letrec ((walk
            (lambda (forms)
              (for-each (lambda (form)
                          (call/cc
                            (lambda (resume)
                              (let ((expansion (jazz.expand-macros walker resume declaration environment form)))
                                (if (jazz.begin-form? expansion)
                                    (walk (%%cdr expansion))
                                  (jazz.walk-declaration walker resume declaration environment expansion))))))
                        forms))))
      (walk forms)))


(define (jazz.add-declaration-child walker resume declaration child)
  (%%when declaration
    (%%set-declaration-children declaration (%%append (%%get-declaration-children declaration) (%%list child)))
    (let ((name (%%get-lexical-binding-name child)))
      (%%hashtable-set! (%%get-access-lookup declaration jazz.private-access) name child)
      (%%hashtable-set! (%%get-access-lookup declaration jazz.public-access) name child))))


(define (jazz.find-form-declaration namespace-declaration name)
  (let ((declaration (jazz.find-declaration namespace-declaration name)))
    (%%assertion declaration (jazz.format "Unable to find declaration: {a}" name)
      declaration)))


(define (jazz.begin-form? form)
  (and (%%pair? form)
       (%%eq? (%%car form) 'begin)))


(define (jazz.define-form? form)
  (and (%%pair? form)
       (%%eq? (%%car form) 'define)))


;;;
;;;; Dependencies
;;;


(define (jazz.register-autoload-declaration library-declaration autoload-declaration)
  (let ((declarations (%%get-library-declaration-autoloads library-declaration)))
    (%%when (%%not (%%memq autoload-declaration declarations))
      (%%set-library-declaration-autoloads library-declaration (%%cons autoload-declaration declarations)))))


;;;
;;;; Lookup
;;;


(define (jazz.lookup-library walker resume declaration environment name)
  (or (jazz.locate-library-declaration name #f)
      (jazz.walk-unresolved walker resume declaration name)))


(define (jazz.lookup-reference walker resume declaration environment symbol)
  (or (jazz.lookup-accessible/compatible-symbol walker resume declaration environment symbol)
      (jazz.walk-unresolved walker resume declaration symbol)))


;;;
;;;; Type
;;;


(jazz.define-class jazz.Type jazz.Object () jazz.Object-Class
  (name))


(define (jazz.new-type name)
  (jazz.allocate-type jazz.Type name))


(jazz.encapsulate-class jazz.Type)


;;;
;;;; Expression
;;;


(jazz.define-class jazz.Expression jazz.Object () jazz.Object-Class
  (type))


(jazz.define-virtual (jazz.emit-expression (jazz.Expression expression)))


(jazz.define-method (jazz.emit-expression (jazz.Expression expression))
  (jazz.error "Unable to emit code for: {s}" expression))


(jazz.define-virtual (jazz.emit-call (jazz.Expression expression) arguments))


(jazz.define-method (jazz.emit-call (jazz.Expression expression) arguments)
  `(,(jazz.emit-expression expression) ,@arguments))


(define (jazz.emit-expressions expressions)
  (map (lambda (expression)
         (jazz.emit-expression expression))
       expressions))


(jazz.encapsulate-class jazz.Expression)


;;;
;;;; Constant
;;;


(jazz.define-class jazz.Constant jazz.Expression (type) jazz.Object-Class
  (expansion))


(define (jazz.new-constant expansion)
  (jazz.allocate-constant jazz.Constant #f expansion))


(jazz.define-method (jazz.emit-expression (jazz.Constant expression))
  (%%get-constant-expansion expression))


(jazz.encapsulate-class jazz.Constant)


;;;
;;;; Quasiquote
;;;


(jazz.define-class jazz.Quasiquote jazz.Expression (type) jazz.Object-Class
  (form))


(define (jazz.new-quasiquote form)
  (jazz.allocate-quasiquote jazz.Quasiquote #f form))


(jazz.define-method (jazz.emit-expression (jazz.Quasiquote expression))
  (letrec ((emit
            (lambda (form)
              (if (%%pair? form)
                  (if (or (%%eq? (%%car form) 'unquote)
                          (%%eq? (%%car form) 'unquote-splicing))
                      (%%list (%%car form) (jazz.emit-expression (%%cadr form)))
                    (%%cons (emit (%%car form)) (emit (%%cdr form))))
                form))))
    (%%list 'quasiquote (emit (%%get-quasiquote-form expression)))))


(jazz.encapsulate-class jazz.Quasiquote)


;;;
;;;; Reference
;;;


(jazz.define-class jazz.Reference jazz.Expression (type) jazz.Object-Class
  (binding))


(define (jazz.new-reference binding)
  (jazz.allocate-reference jazz.Reference #f binding))


(jazz.define-method (jazz.emit-expression (jazz.Reference expression))
  (jazz.emit-binding-reference (%%get-reference-binding expression)))


(jazz.define-method (jazz.emit-call (jazz.Reference expression) arguments)
  (jazz.emit-binding-call (%%get-reference-binding expression) arguments))


(jazz.encapsulate-class jazz.Reference)


;;;
;;;; Assignment
;;;


(jazz.define-class jazz.Assignment jazz.Expression (type) jazz.Object-Class
  (binding
   value))


(define (jazz.new-assignment binding value)
  (jazz.allocate-assignment jazz.Assignment #f binding value))


(jazz.define-method (jazz.emit-expression (jazz.Assignment expression))
  (jazz.emit-binding-assignment (%%get-assignment-binding expression) (%%get-assignment-value expression)))


(jazz.encapsulate-class jazz.Assignment)


;;;
;;;; Lambda
;;;


(jazz.define-class jazz.Lambda jazz.Expression (type) jazz.Object-Class
  (parameters
   body))


(define (jazz.new-lambda parameters body)
  (jazz.allocate-lambda jazz.Lambda #f parameters body))


(jazz.define-method (jazz.emit-expression (jazz.Lambda expression))
  (let ((parameters (%%get-lambda-parameters expression))
        (body (%%get-lambda-body expression)))
    `(lambda ,(jazz.emit-parameters parameters)
       ,@(jazz.emit-expression body))))


(jazz.encapsulate-class jazz.Lambda)


;;;
;;;; Let
;;;


(jazz.define-class jazz.Let jazz.Expression (type) jazz.Object-Class
  (bindings
   body))


(define (jazz.new-let bindings body)
  (jazz.allocate-let jazz.Let #f bindings body))


(jazz.define-method (jazz.emit-expression (jazz.Let expression))
  (let ((bindings (%%get-let-bindings expression))
        (body (%%get-let-body expression)))
    `(let ,(map (lambda (binding)
                  (let ((variable (%%car binding))
                        (value (%%cdr binding)))
                    `(,(%%get-lexical-binding-name variable) ,(jazz.emit-expression value))))
                bindings)
       ,@(jazz.emit-expression body))))


(jazz.encapsulate-class jazz.Let)


;;;
;;;; Named Let
;;;


(jazz.define-class jazz.Named-Let jazz.Let (type bindings body) jazz.Object-Class
  (name))


(define (jazz.new-named-let name bindings body)
  (jazz.allocate-named-let jazz.Named-Let #f bindings body name))


(jazz.define-method (jazz.emit-expression (jazz.Named-Let expression))
  (let ((name (%%get-named-let-name expression))
        (bindings (%%get-let-bindings expression))
        (body (%%get-let-body expression)))
    `(let ,name ,(map (lambda (binding)
                  (let ((variable (%%car binding))
                        (value (%%cdr binding)))
                    `(,(%%get-lexical-binding-name variable) ,(jazz.emit-expression value))))
                bindings)
       ,@(jazz.emit-expression body))))


(jazz.encapsulate-class jazz.Named-Let)


;;;
;;;; Letstar
;;;


(jazz.define-class jazz.Letstar jazz.Expression (type) jazz.Object-Class
  (bindings
   body))


(define (jazz.new-letstar bindings body)
  (jazz.allocate-letstar jazz.Letstar #f bindings body))


(jazz.define-method (jazz.emit-expression (jazz.Letstar expression))
  (let ((bindings (%%get-letstar-bindings expression))
        (body (%%get-letstar-body expression)))
    `(let* ,(map (lambda (binding)
                   (let ((variable (%%car binding))
                         (value (%%cdr binding)))
                     `(,(%%get-lexical-binding-name variable) ,(jazz.emit-expression value))))
                 bindings)
       ,@(jazz.emit-expression body))))


(jazz.encapsulate-class jazz.Letstar)


;;;
;;;; Letrec
;;;


(jazz.define-class jazz.Letrec jazz.Expression (type) jazz.Object-Class
  (bindings
   body))


(define (jazz.new-letrec bindings body)
  (jazz.allocate-letrec jazz.Letrec #f bindings body))


(jazz.define-method (jazz.emit-expression (jazz.Letrec expression))
  (let ((bindings (%%get-letrec-bindings expression))
        (body (%%get-letrec-body expression)))
    `(letrec ,(map (lambda (binding)
                     (let ((variable (%%car binding))
                           (value (%%cdr binding)))
                       `(,(%%get-lexical-binding-name variable) ,(jazz.emit-expression value))))
                   bindings)
       ,@(jazz.emit-expression body))))


(jazz.encapsulate-class jazz.Letrec)


;;;
;;;; Receive
;;;


(jazz.define-class jazz.Receive jazz.Expression (type) jazz.Object-Class
  (variables
   expression
   body))


(define (jazz.new-receive variables expression body)
  (jazz.allocate-receive jazz.Receive #f variables expression body))


(jazz.define-method (jazz.emit-expression (jazz.Receive expression))
  (let ((variables (%%get-receive-variables expression))
        (expression (%%get-receive-expression expression))
        (body (%%get-receive-body expression)))
    `(receive ,(map (lambda (variable)
                      (%%get-lexical-binding-name variable))
                    variables)
              ,(jazz.emit-expression expression)
       ,@(jazz.emit-expression body))))


(jazz.encapsulate-class jazz.Receive)


;;;
;;;; Body
;;;


(jazz.define-class jazz.Body jazz.Expression (type) jazz.Object-Class
  (internal-defines
   expressions))


(define (jazz.new-body internal-defines expressions)
  (jazz.allocate-body jazz.Body #f internal-defines expressions))


(jazz.define-method (jazz.emit-expression (jazz.Body expression))
  (let ((internal-defines (%%get-body-internal-defines expression))
        (expressions (%%get-body-expressions expression)))
    (append (jazz.emit-expressions internal-defines)
            (jazz.emit-expressions expressions))))


(jazz.encapsulate-class jazz.Body)


;;;
;;;; Internal-Define
;;;


(jazz.define-class jazz.Internal-Define jazz.Expression (type) jazz.Object-Class
  (name
   value))


(define (jazz.new-internal-define name value)
  (jazz.allocate-internal-define jazz.Internal-Define #f name value))


(jazz.define-method (jazz.emit-expression (jazz.Internal-Define expression))
  (let ((name (%%get-internal-define-name expression))
        (value (%%get-internal-define-value expression)))
    `(define ,name
       ,(jazz.emit-expression value))))


(jazz.encapsulate-class jazz.Internal-Define)


;;;
;;;; Begin
;;;


(jazz.define-class jazz.Begin jazz.Expression (type) jazz.Object-Class
  (expressions))


(define (jazz.new-begin expressions)
  (jazz.allocate-begin jazz.Begin #f expressions))


(jazz.define-method (jazz.emit-expression (jazz.Begin expression))
  (let ((expressions (%%get-begin-expressions expression)))
    `(begin
       ,@(jazz.emit-declarations/expressions expressions))))


(jazz.encapsulate-class jazz.Begin)


;;;
;;;; Call
;;;


(jazz.define-class jazz.Call jazz.Expression (type) jazz.Object-Class
  (operator
   arguments))


(define (jazz.new-call operator arguments)
  (jazz.allocate-call jazz.Call #f operator arguments))


(jazz.define-method (jazz.emit-expression (jazz.Call expression))
  (let ((operator (%%get-call-operator expression))
        (arguments (%%get-call-arguments expression)))
    (or (jazz.inline-call operator arguments)
        (jazz.emit-call operator (jazz.emit-expressions arguments)))))


(define (jazz.inline-call operator arguments)
  (cond ((and (%%eq? (%%get-declaration-locator (%%get-reference-binding operator)) 'jazz.dialect.language.<)
              (jazz.every? (lambda (argument)
                             (%%eq? (%%get-lexical-binding-type (%%get-reference-binding argument)) 'fx))
                           arguments))
         `(##fx< ,@(jazz.emit-expressions arguments)))
        (else
         #f)))


(jazz.encapsulate-class jazz.Call)


;;;
;;;; If
;;;


(jazz.define-class jazz.If jazz.Expression (type) jazz.Object-Class
  (test
   yes
   no))


(define (jazz.new-if test yes no)
  (jazz.allocate-if jazz.If #f test yes no))


(jazz.define-method (jazz.emit-expression (jazz.If expression))
  `(if ,(jazz.emit-expression (%%get-if-test expression))
       ,(jazz.emit-expression (%%get-if-yes expression))
     (begin
       ,@(jazz.emit-expressions (%%get-if-no expression)))))


(jazz.encapsulate-class jazz.If)


;;;
;;;; Cond
;;;


(jazz.define-class jazz.Cond jazz.Expression (type) jazz.Object-Class
  (clauses))


(define (jazz.new-cond clauses)
  (jazz.allocate-cond jazz.Cond #f clauses))


(jazz.define-method (jazz.emit-expression (jazz.Cond expression))
  `(cond ,@(map (lambda (clause)
                  (let ((test (%%car clause))
                        (body (%%cdr clause)))
                    `(,(if (%%not test)
                           'else
                         (jazz.emit-expression test))
                      ,@(jazz.emit-expressions body))))
                (%%get-cond-clauses expression))))


(jazz.encapsulate-class jazz.Cond)


;;;
;;;; Case
;;;


(jazz.define-class jazz.Case jazz.Expression (type) jazz.Object-Class
  (target
   clauses))


(define (jazz.new-case target clauses)
  (jazz.allocate-case jazz.Case #f target clauses))


(jazz.define-method (jazz.emit-expression (jazz.Case expression))
  (let ((target (%%get-case-target expression))
        (clauses (%%get-case-clauses expression)))
    `(case ,(jazz.emit-expression target)
       ,@(map (lambda (clause)
                (let* ((tries (%%car clause))
                       (body (%%cdr clause)))
                  `(,tries ,@(jazz.emit-expressions body))))
              clauses))))


(jazz.encapsulate-class jazz.Case)


;;;
;;;; And
;;;


(jazz.define-class jazz.And jazz.Expression (type) jazz.Object-Class
  (expressions))


(define (jazz.new-and expressions)
  (jazz.allocate-and jazz.And #f expressions))


(jazz.define-method (jazz.emit-expression (jazz.And expression))
  `(and ,@(jazz.emit-expressions (%%get-and-expressions expression))))


(jazz.encapsulate-class jazz.And)


;;;
;;;; Or
;;;


(jazz.define-class jazz.Or jazz.Expression (type) jazz.Object-Class
  (expressions))


(define (jazz.new-or expressions)
  (jazz.allocate-or jazz.Or #f expressions))


(jazz.define-method (jazz.emit-expression (jazz.Or expression))
  `(or ,@(jazz.emit-expressions (%%get-or-expressions expression))))


(jazz.encapsulate-class jazz.Or)


;;;
;;;; C Include
;;;


(jazz.define-class jazz.C-Include jazz.Expression (type) jazz.Object-Class
  (name))


(define (jazz.new-c-include name)
  (jazz.allocate-c-include jazz.C-Include #f name))


(jazz.define-method (jazz.emit-expression (jazz.C-Include expression))
  (let ((name (%%get-c-include-name expression)))
    `(c-declare ,(%%string-append "#include " name))))


(jazz.encapsulate-class jazz.C-Include)


;;;
;;;; C Declare
;;;


(jazz.define-class jazz.C-Declare jazz.Expression (type) jazz.Object-Class
  (code))


(define (jazz.new-c-declare code)
  (jazz.allocate-c-declare jazz.C-Declare #f code))


(jazz.define-method (jazz.emit-expression (jazz.C-Declare expression))
  (let ((code (%%get-c-declare-code expression)))
    `(c-declare ,code)))


(jazz.encapsulate-class jazz.C-Declare)


;;;
;;;; C Initialize
;;;


(jazz.define-class jazz.C-Initialize jazz.Expression (type) jazz.Object-Class
  (code))


(define (jazz.new-c-initialize code)
  (jazz.allocate-c-initialize jazz.C-Initialize #f code))


(jazz.define-method (jazz.emit-expression (jazz.C-Initialize expression))
  (let ((code (%%get-c-initialize-code expression)))
    `(c-initialize ,code)))


(jazz.encapsulate-class jazz.C-Initialize)


;;;
;;;; C Function
;;;


(jazz.define-class jazz.C-Function jazz.Expression (type) jazz.Object-Class
  (expansion))


(define (jazz.new-c-function expansion)
  (jazz.allocate-c-function jazz.C-Function #f expansion))


(jazz.define-method (jazz.emit-expression (jazz.C-Function expression))
  (%%get-c-function-expansion expression))


(jazz.encapsulate-class jazz.C-Function)


;;;
;;;; Time
;;;


(jazz.define-class jazz.Time jazz.Expression (type) jazz.Object-Class
  (expression))


(define (jazz.new-time expression)
  (jazz.allocate-time jazz.Time #f expression))


(jazz.define-method (jazz.emit-expression (jazz.Time expression))
  (let ((expression (%%get-time-expression expression)))
    `(time
       ,(jazz.emit-expression expression))))


(jazz.encapsulate-class jazz.Time)


;;;
;;;; Emit
;;;


(define (jazz.emit-declaration/expression declaration/expression)
  (if (jazz.is? declaration/expression jazz.Declaration)
      (jazz.emit-declaration declaration/expression)
    (jazz.emit-expression declaration/expression)))


(define (jazz.emit-declarations/expressions list)
  (map jazz.emit-declaration/expression list))


;;;
;;;; Walk
;;;


(define (jazz.walk walker resume declaration environment form)
  (cond ((%%symbol? form)
         (jazz.walk-symbol walker resume declaration environment form))
        ((%%pair? form)
         (jazz.walk-form walker resume declaration environment form))
        ((%%is? form jazz.Access)
         (jazz.walk-access walker resume declaration environment form))
        (else
         (jazz.walk-constant walker resume declaration environment form))))


(define (jazz.walk-list walker resume declaration environment form-list)
  (let ((queue (jazz.new-queue)))
    (for-each (lambda (form)
                (call/cc
                  (lambda (resume)
                    (jazz.enqueue queue (jazz.walk walker resume declaration environment form)))))
              form-list)
    (jazz.queue-list queue)))


(define (jazz.walk-body walker resume declaration environment form-list)
  (let ((internal-defines '()))
    (letrec ((process
               (lambda (form)
                 (cond ((jazz.begin-form? form)
                        (let ((state #f))
                          (for-each (lambda (sub)
                                      (let ((substate (process sub)))
                                        (if (%%not state)
                                            (set! state substate)
                                          (if (%%neq? substate state)
                                              (jazz.error "Inconsistant internal defines")))))
                                    (%%cdr form))
                          state))
                       ((jazz.define-form? form)
                        (set! internal-defines (%%cons form internal-defines))
                        'defines)
                       (else
                        'expressions)))))
      (let iter ((scan form-list))
        (if (or (%%null? scan)
                (%%eq? (process (%%car scan)) 'expressions))
            (if (%%null? internal-defines)
                (jazz.new-body '() (jazz.walk-list walker resume declaration environment scan))
              (let ((augmented-environment environment))
                (for-each (lambda (internal-define)
                            (let ((signature (%%cadr internal-define)))
                              (let ((name (if (%%symbol? signature)
                                              signature
                                            (%%car signature))))
                                (set! augmented-environment (%%cons (jazz.new-variable name #f) augmented-environment)))))
                          internal-defines)
                (jazz.new-body (map (lambda (internal-define)
                                      (jazz.walk-internal-define walker resume declaration augmented-environment internal-define))
                                    internal-defines)
                               (jazz.walk-list walker resume declaration augmented-environment scan))))
          (iter (%%cdr scan)))))))


(define (jazz.walk-internal-define walker resume declaration augmented-environment form)
  (receive (name type value parameters) (jazz.parse-define walker resume declaration (%%cdr form))
    (jazz.new-internal-define name (jazz.walk walker resume declaration augmented-environment value))))


(define (jazz.parse-define walker resume declaration rest)
  (if (%%symbol? (%%car rest))
      (values (%%car rest) #f (%%cadr rest) #f)
    (let ((name (%%caar rest))
          (type #f)
          (parameters (%%cdar rest))
          (body (%%cdr rest)))
      (values name type `(lambda ,parameters ,@body) parameters))))


;;;
;;;; Constant
;;;


(define (jazz.walk-quote walker resume declaration environment form)
  (let ((expression (%%cadr form)))
    (if (%%null? expression)
        (jazz.new-constant '(quote ()))
      (jazz.walk-constant walker resume declaration environment expression))))


(define (jazz.walk-keyword walker keyword)
  (jazz.new-constant keyword))


(define (jazz.walk-enumerator walker enumerator)
  (jazz.new-constant (%%list 'quote enumerator)))


(define (jazz.walk-constant walker resume declaration environment form)
  (cond
    ((or (%%boolean? form)
         (%%char? form)
         (%%string? form)
         (%%keyword? form)
         (%%number? form)
         (%%null? form))
     (jazz.new-constant form))
    ((or (%%symbol? form)
         (%%vector? form)
         (jazz.scheme-pair-literal? form))
     (jazz.new-constant `(quote ,form)))
    (else
     (jazz.register-literal walker resume declaration form))))


(define (jazz.scheme-pair-literal? form)
  (letrec ((scheme-data?
             (lambda (expr)
               (or (%%null? expr)
                   (%%boolean? expr)
                   (%%char? expr)
                   (%%string? expr)
                   (%%keyword? expr)
                   (%%number? expr)
                   (%%symbol? expr)
                   (and (%%pair? expr) (scheme-data? (%%car expr)) (scheme-data? (%%cdr expr)))))))
    (and (%%pair? form)
         (scheme-data? form))))


;;;
;;;; Literal
;;;


(define jazz.Literal-Constructors
  (%%make-hashtable eq?))


(define (jazz.register-literal-constructor name constructor)
  (%%hashtable-set! jazz.Literal-Constructors name constructor))


(define (jazz.require-literal-constructor name)
  (or (%%hashtable-ref jazz.Literal-Constructors name #f)
      (jazz.error "Cannot construct literals of type {s}" name)))


(define (jazz.construct-literal lst)
  (if (%%null? lst)
      #f
    (let ((constructor (jazz.require-literal-constructor (%%car lst))))
      (%%apply constructor (%%cdr lst)))))


(define (jazz.register-literal walker resume declaration literal)
  ;; calling jazz.get-registered-literal to only register when not already there
  ;; doesnt work directly because some literals are interned and thus can be shared
  (let ((library-declaration (%%get-declaration-toplevel declaration)))
    (let ((name (jazz.generate-symbol (%%string-append (%%symbol->string (%%get-declaration-locator library-declaration)) ".lit"))))
      ;; it is important to register before any subliterals to ensure they come before us
      (let ((info (%%cons literal (%%cons name #f))))
        (%%set-library-declaration-literals library-declaration (%%cons info (%%get-library-declaration-literals library-declaration)))
        (%%set-cdr! (%%cdr info) (jazz.walk-literal walker resume declaration literal)))
      (jazz.new-constant name))))


(define (jazz.get-registered-literal library-declaration literal)
  (let ((pair (assq literal (%%get-library-declaration-literals library-declaration))))
    (if pair
        (%%cadr pair)
      #f)))


(define (jazz.walk-literal walker resume declaration literal)
  ;; this is a quick solution
  (let* ((library-declaration (%%get-declaration-toplevel declaration))
         (environment (%%cons library-declaration (jazz.walker-environment walker))))
    (jazz.walk walker resume library-declaration environment
      (cond ((%%pair? literal)
             `(cons ',(car literal) ',(cdr literal)))
            (else
             (jazz.dialect.language.fold-literal literal))))))


(define jazz.dialect.language.fold-literal
  #f)

(set! jazz.dialect.language.fold-literal #f)


(define (jazz.make-symbolic-chars alist)
  (%%alist->hashtable
    (map (lambda (pair)
           (%%cons (%%car pair) (integer->char (%%cdr pair))))
         alist)
    eq?))


(define jazz.Symbolic-Chars
  (jazz.make-symbolic-chars
    '((Zero              . #x00)
      (Home              . #x01)
      (Enter             . #x03)
      (End               . #x04)
      (Info              . #x05)
      (Backspace         . #x08)
      (Tab               . #x09)
      (Line-Feed         . #x0A)
      (Page-Up           . #x0B)
      (Page-Down         . #x0C)
      (Return            . #x0D)
      (Escape            . #x1B)
      (Left-Arrow        . #x1C)
      (Right-Arrow       . #x1D)
      (Up-Arrow          . #x1E)
      (Down-Arrow        . #x1F)
      (Space             . #x20)
      (Exclamation-Point . #x21)
      (Double-Quote      . #x22)
      (Sharp             . #x23)
      (Ampersand         . #x26)
      (Quote             . #x27)
      (Open-Parenthesis  . #x28)
      (Close-Parenthesis . #x29)
      (Times             . #x2A)
      (Plus              . #x2B)
      (Comma             . #x2C)
      (Minus             . #x2D)
      (Period            . #x2E)
      (Slash             . #x2F)
      (Colon             . #x3A)
      (Semi-Colon        . #x3B)
      (Question-Mark     . #x3F)
      (At                . #x40)
      (Open-Bracket      . #x5B)
      (Backslash         . #x5C)
      (Close-Bracket     . #x5D)
      (Exponential       . #x5E)
      (Underscore        . #x5F)
      (Backquote         . #x60)
      (Open-Brace        . #x7B)
      (Close-Brace       . #x7D)
      (Delete            . #x7F)
      (Copyright         . #xA9))))


(define (jazz.symbolic-char name)
  (%%hashtable-ref jazz.Symbolic-Chars name #f))


;;;
;;;; Variable
;;;


(define (jazz.register-variable declaration suffix)
  (let ((library-declaration (%%get-declaration-toplevel declaration)))
    (let ((variable (jazz.generate-symbol (%%string-append (%%symbol->string (%%get-declaration-locator library-declaration)) "." suffix))))
      (%%set-library-declaration-variables library-declaration (%%cons variable (%%get-library-declaration-variables library-declaration)))
      variable)))


;;;
;;;; Symbol
;;;


(define (jazz.walk-symbol walker resume declaration environment form)
  (cond ((%%keyword? form)
         (jazz.walk-keyword walker form))
        ((jazz.enumerator? form)
         (jazz.walk-enumerator walker form))
        ;; inline false (until compiler support for constants)
        ((%%eq? form 'false)
         (jazz.new-constant #f))
        ;; inline true (until compiler support for constants)
        ((%%eq? form 'true)
         (jazz.new-constant #t))
        (else
         (jazz.walk-symbol-reference walker resume declaration environment form))))


(define (jazz.walk-setbang walker resume declaration environment form)
  (let ((form (%%cadr form))
        (value (%%car (%%cddr form))))
    (cond ((%%symbol? form)
           (jazz.walk-symbol-assignment walker resume declaration environment form value))
          ((%%is? form jazz.Access)
           (jazz.walk-access-assignment walker resume declaration environment form value))
          (else
           (jazz.error "Illegal set! of {s}" form)))))


(define (jazz.lookup-symbol walker environment symbol)
  (if (jazz.composite-name? symbol)
      (jazz.lookup-composite walker environment symbol)
    (jazz.find-in (lambda (binding)
                    (jazz.walk-binding-lookup binding symbol))
                  environment)))


(define (jazz.lookup-composite walker environment name)
  (let* ((path (jazz.split-identifier name))
         (library-name (%%apply jazz.compose-name (jazz.butlast path)))
         (library-decl (jazz.locate-library-declaration library-name #f)))
    (if library-decl
        (jazz.lookup-subpath library-decl (%%list (jazz.last path)))
      #f)))


(define (jazz.lookup-subpath declaration subpath)
  (if (%%null? subpath)
      declaration
    (let ((subdecl (jazz.lookup-declaration declaration (%%car subpath) #t)))
      (if subdecl
          (jazz.lookup-subpath subdecl (%%cdr subpath))
        #f))))


(define (jazz.lookup-accessible/compatible-symbol walker resume declaration environment symbol)
  (let ((referenced-declaration (jazz.lookup-symbol walker environment symbol)))
    (if (and referenced-declaration (%%is? referenced-declaration jazz.Declaration))
        (begin
          (jazz.validate-access walker resume declaration referenced-declaration)
          (jazz.validate-compatibility walker declaration referenced-declaration)))
    referenced-declaration))


(jazz.define-virtual (jazz.validate-access (jazz.Walker walker) resume declaration referenced-declaration))


(jazz.define-method (jazz.validate-access (jazz.Walker walker) resume declaration referenced-declaration)
  (jazz.void))


(define (jazz.validate-compatibility walker declaration referenced-declaration)
  (if (%%eq? (%%get-declaration-compatibility referenced-declaration) 'deprecated)
      (let ((referenced-locator (%%get-declaration-locator referenced-declaration)))
        (jazz.walk-warning walker declaration "Deprecated access to {s}" referenced-locator))))


;;;
;;;; Reference
;;;


(define (jazz.walk-symbol-reference walker resume declaration environment symbol)
  (let ((binding (jazz.lookup-accessible/compatible-symbol walker resume declaration environment symbol)))
    (if binding
        (jazz.new-reference binding)
      (jazz.walk-free-reference walker resume declaration symbol))))


(jazz.define-virtual (jazz.walk-free-reference (jazz.Walker walker) resume declaration symbol))


(jazz.define-method (jazz.walk-free-reference (jazz.Walker walker) resume declaration symbol)
  (jazz.walk-unresolved walker resume declaration symbol))


;;;
;;;; Assignment
;;;


(define (jazz.walk-symbol-assignment walker resume declaration environment symbol value)
  (let ((binding (jazz.lookup-accessible/compatible-symbol walker resume declaration environment symbol)))
    (if binding
        (if (jazz.walk-binding-assignable? binding)
            (jazz.new-assignment binding (jazz.walk walker resume declaration environment value))
          (jazz.walk-error walker resume declaration "Illegal assignment to: {s}" binding))
      (jazz.walk-free-assignment walker resume declaration symbol))))


(jazz.define-virtual (jazz.walk-free-assignment (jazz.Walker walker) resume declaration symbol))


(jazz.define-method (jazz.walk-free-assignment (jazz.Walker walker) resume declaration symbol)
  (jazz.walk-unresolved walker resume declaration symbol))


;;;
;;;; Form
;;;


(jazz.define-virtual (jazz.walk-form (jazz.Walker walker) resume declaration environment form))


(jazz.define-method (jazz.walk-form (jazz.Walker walker) resume declaration environment form)
  (let ((procedure-expr (%%car form)))
    (let ((binding (and (%%symbol? procedure-expr) (jazz.lookup-accessible/compatible-symbol walker resume declaration environment procedure-expr))))
      ;; special form
      (if (and binding (jazz.walk-binding-walkable? binding))
          (jazz.walk-binding-walk-form binding walker resume declaration environment form)
        ;; macro
        (if (and binding (jazz.walk-binding-expandable? binding))
            (let ((expansion (jazz.walk-binding-expand-form binding walker resume declaration environment form)))
              (jazz.walk walker resume declaration environment expansion))
          ;; call
          (jazz.walk-call walker resume declaration environment binding form))))))


;;;
;;;; Macro
;;;


(define (jazz.lookup-macro-form walker resume declaration environment symbol)
  (let ((binding (jazz.lookup-accessible/compatible-symbol walker resume declaration environment symbol)))
    (if (and binding (jazz.walk-binding-expandable? binding))
        binding
      #f)))


(define (jazz.expand-macros walker resume declaration environment form)
  (if (%%not (%%pair? form))
      form
    (let ((procedure-expr (%%car form)))
      (let ((binding (and (%%symbol? procedure-expr) (jazz.lookup-macro-form walker resume declaration environment procedure-expr))))
        (if binding
            (let ((expansion (jazz.walk-binding-expand-form binding walker resume declaration environment form)))
              (jazz.expand-macros walker resume declaration environment expansion))
          form)))))


;;;
;;;; Call
;;;


(define (jazz.walk-call walker resume declaration environment procedure-binding form)
  (let ((operator (%%car form))
        (arguments (%%cdr form)))
    (if procedure-binding
        (jazz.walk-binding-validate-call procedure-binding walker resume declaration operator arguments))
    (jazz.new-call (jazz.walk walker resume declaration environment operator)
                   (jazz.walk-list walker resume declaration environment arguments))))


(jazz.define-virtual (jazz.validate-arguments (jazz.Walker walker) resume source-declaration declaration signature arguments))


(jazz.define-method (jazz.validate-arguments (jazz.Walker walker) resume source-declaration declaration signature arguments)
  (let ((mandatory (%%get-signature-mandatory signature))
        (rest? (%%get-signature-rest? signature))
        (passed (%%length arguments)))
    (if (if rest? (%%fx< passed mandatory) (%%not (%%fx= passed mandatory)))
        (let ((locator (%%get-declaration-locator declaration)))
          (jazz.walk-error walker resume source-declaration "Wrong number of arguments to {a} (passed {a} expected{a} {a})"
            locator passed (if rest? " at least" "") mandatory)))))


;;;
;;;; Native
;;;


(define jazz.native-modifiers
  '(((private protected public) . public)
    ((deprecated uptodate) . uptodate)))

(define jazz.native-keywords
  '())


(define (jazz.parse-native walker resume declaration rest)
  (receive (access compatibility rest) (jazz.parse-modifiers walker resume declaration jazz.native-modifiers rest)
    (let ((name (%%car rest))
          (type #f))
      (%%assert (%%null? (%%cdr rest))
        (values name type access compatibility)))))


(define (jazz.walk-native-declaration walker resume declaration environment form)
  (receive (name type access compatibility) (jazz.parse-native walker resume declaration (%%cdr form))
    (receive (name symbol) (jazz.parse-exported-symbol declaration name)
      (let ((new-declaration (jazz.new-export-declaration name type access compatibility '() declaration symbol)))
        (jazz.add-declaration-child walker resume declaration new-declaration)
        new-declaration))))


(define (jazz.walk-native walker resume declaration environment form)
  (receive (name type access compatibility) (jazz.parse-native walker resume declaration (%%cdr form))
    (receive (name symbol) (jazz.parse-exported-symbol declaration name)
      (jazz.find-form-declaration declaration name))))


;;;
;;;; Macro
;;;


(define jazz.macro-modifiers
  '(((private protected public) . private)
    ((deprecated uptodate) . uptodate)))


(define (jazz.parse-macro walker resume declaration rest)
  (receive (access compatibility rest) (jazz.parse-modifiers walker resume declaration jazz.macro-modifiers rest)
    (let* ((signature (%%car rest))
           (body (%%cdr rest))
           (name (%%car signature))
           (type #f)
           (parameters (%%cdr signature)))
      (values name type access compatibility parameters body))))


(define (jazz.expand-macro walker resume declaration environment . rest)
  (jazz.expand-macro-form walker resume declaration (%%cons 'macro rest)))


(define (jazz.expand-macro-form walker resume declaration form)
  (receive (name type access compatibility parameters body) (jazz.parse-macro walker resume declaration (%%cdr form))
    `(%macro ,name ,type ,access ,compatibility ,parameters ,body)))


(define (jazz.walk-%macro-declaration walker resume declaration environment form)
  (jazz.bind (name type access compatibility parameters body) (%%cdr form)
    (let ((new-declaration (jazz.new-macro-declaration name type access compatibility '() declaration parameters)))
      (jazz.add-declaration-child walker resume declaration new-declaration)
      new-declaration)))


(define (jazz.walk-%macro walker resume declaration environment form)
  (jazz.bind (name type access compatibility parameters body) (%%cdr form)
    (let* ((new-declaration (jazz.find-form-declaration declaration (%%cadr form)))
           (locator (%%get-declaration-locator new-declaration))
           (new-variables (jazz.parameters->variables parameters))
           (new-environment (%%append new-variables environment)))
      (%%set-macro-declaration-body new-declaration
        (jazz.walk-body walker resume new-declaration new-environment body))
      new-declaration)))


;;;
;;;; Syntax
;;;


(define jazz.syntax-modifiers
  '(((private protected public) . private)
    ((deprecated uptodate) . uptodate)))


(define (jazz.parse-syntax walker resume declaration rest)
  (receive (access compatibility rest) (jazz.parse-modifiers walker resume declaration jazz.syntax-modifiers rest)
    (let* ((signature (%%car rest))
           (body (%%cdr rest))
           (name (%%car signature))
           (type #f)
           (parameters (%%cdr signature)))
      (values name type access compatibility parameters body))))


(define (jazz.expand-syntax walker resume declaration environment . rest)
  (jazz.expand-syntax-form walker resume declaration (%%cons 'syntax rest)))


(define (jazz.expand-syntax-form walker resume declaration form)
  (receive (name type access compatibility parameters body) (jazz.parse-syntax walker resume declaration (%%cdr form))
    `(%syntax ,name ,type ,access ,compatibility ,parameters ,body)))


(define (jazz.walk-%syntax-declaration walker resume declaration environment form)
  (jazz.bind (name type access compatibility parameters body) (%%cdr form)
    (let ((new-declaration (jazz.new-syntax-declaration name type access compatibility '() declaration parameters)))
      (jazz.add-declaration-child walker resume declaration new-declaration)
      new-declaration)))


(define (jazz.walk-%syntax walker resume declaration environment form)
  (jazz.bind (name type access compatibility parameters body) (%%cdr form)
    (let* ((new-declaration (jazz.find-form-declaration declaration (%%cadr form)))
           (locator (%%get-declaration-locator new-declaration))
           (new-variables (jazz.parameters->variables parameters))
           (new-environment (%%append new-variables environment)))
      (%%set-syntax-declaration-body new-declaration
        (jazz.walk-body walker resume new-declaration new-environment body))
      new-declaration)))


;;;
;;;; Parameters
;;;


(define (jazz.parameters->variables parameters)
  (let ((queue (jazz.new-queue))
        (scan parameters))
    (%%while (%%not (%%null? scan))
      (cond ((%%pair? scan)
             (jazz.enqueue queue (jazz.new-variable (%%car scan) #f))
             (set! scan (%%cdr scan)))
            (else
             (jazz.enqueue queue (jazz.new-restvariable scan #f))
             (set! scan '()))))
    (jazz.queue-list queue)))


(define (jazz.variables->parameters variables)
  (let ((queue (jazz.new-queue)))
    (for-each (lambda (variable)
                (let ((name (%%get-lexical-binding-name variable)))
                  (if (%%is? variable jazz.RestVariable)
                      (jazz.enqueue-list queue name)
                    (jazz.enqueue queue name))))
              variables)
    (jazz.queue-list queue)))


(jazz.encapsulate-class jazz.Walker)


;;;
;;;; Catalog
;;;


(define jazz.Catalog
  (%%make-hashtable eq?))


(define (jazz.get-catalog)
  jazz.Catalog)


(define (jazz.get-catalog-entry module-name)
  (%%hashtable-ref jazz.Catalog module-name #f))


(define (jazz.set-catalog-entry module-name entry)
  (%%hashtable-set! jazz.Catalog module-name entry))


(define (jazz.locate-toplevel-declaration module-name #!optional (error? #t))
  (let ((entry (jazz.get-validated-catalog-entry module-name)))
    (or (jazz.get-catalog-entry module-name)
        (jazz.call-with-validate-circularity module-name
          (lambda ()
            (let ((declaration (jazz.load-toplevel-declaration module-name)))
              (if (%%not declaration)
                  (if error?
                      (jazz.error "Unable to locate module declaration: {s}" module-name))
                (jazz.set-catalog-entry module-name declaration))
              declaration))))))


(define (jazz.locate-library-declaration module-name #!optional (error? #t))
  (let ((declaration (jazz.locate-toplevel-declaration module-name error?)))
    (%%assert (%%is? declaration jazz.Library-Declaration)
      declaration)))


(define (jazz.call-with-validate-circularity module-name thunk)
  (dynamic-wind
    (lambda ()
      (jazz.set-catalog-entry module-name ':loading))
    thunk
    (lambda ()
      (if (%%eq? (jazz.get-catalog-entry module-name) ':loading)
          (jazz.set-catalog-entry module-name #f)))))


(define (jazz.get-validated-catalog-entry module-name)
  (let ((entry (jazz.get-catalog-entry module-name)))
    (if (%%eq? entry ':loading)
        (jazz.error "Circular dependency detected with {s}" module-name)
      entry)))


(define (jazz.load-toplevel-declaration module-name)
  (let ((filename (jazz.require-module-source (jazz.module-filename module-name))))
    (if jazz.parse-verbose?
        (jazz.parse-verbose filename))
    (let ((load-declaration
            (lambda ()
              (let ((form (jazz.read-toplevel-form filename)))
                (case (%%car form)
                  ((module)
                   (jazz.parse-module-declaration (%%cdr form)))
                  ((library)
                   (jazz.parse-library-declaration (%%cdr form))))))))
      (let ((declaration
              (if jazz.parse-verbose?
                  (parameterize ((jazz.load-indent-level (%%fx+ (jazz.load-indent-level) 2)))
                    (load-declaration))
                (load-declaration))))
        (if (and jazz.parse-verbose? jazz.done-verbose?)
            (jazz.parse-done-verbose filename))
        declaration))))


(define (jazz.parse-verbose filename)
  (display (make-string (jazz.load-indent-level) #\space))
  (display "; parsing ")
  (display filename)
  (display " ...")
  (newline))


(define (jazz.parse-done-verbose filename)
  (display (make-string (jazz.load-indent-level) #\space))
  (display "; done ")
  (newline))


(define jazz.parse-read? (make-parameter #f))


(define (jazz.read-toplevel-form filename . rest)
  (let ((parse-read? (if (%%null? rest) #t (%%car rest))))
    (let ((form
            (jazz.with-extension-reader (jazz.filename-extension filename)
              (lambda ()
                (call-with-input-file filename
                  (lambda (port)
                    (parameterize ((jazz.parse-read? parse-read?))
                      (read port))))))))
      (if (and (%%not (%%eof-object? form)) (%%memq (%%car form) '(module library)))
          form
        (jazz.error "Invalid module declaration in {a}: {s}" filename form)))))


;;;
;;;; Core Dialect
;;;


(jazz.define-class jazz.Core-Dialect jazz.Dialect () jazz.Object-Class
  ())


(define (jazz.new-core-dialect)
  (jazz.allocate-core-dialect jazz.Core-Dialect))


(jazz.define-method (jazz.dialect-walker (jazz.Core-Dialect dialect))
  (jazz.new-core-walker))


(jazz.encapsulate-class jazz.Core-Dialect)


;;;
;;;; Core Walker
;;;


(jazz.define-class jazz.Core-Walker jazz.Walker (warnings errors) jazz.Object-Class
  ())


(define (jazz.new-core-walker)
  (jazz.allocate-core-walker jazz.Core-Walker '() '()))


(jazz.encapsulate-class jazz.Core-Walker)


;;;
;;;; Register Core
;;;


(jazz.register-dialect 'core (jazz.new-core-dialect)))
