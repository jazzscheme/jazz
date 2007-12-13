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
            (%%vector-set! lookups n (%%make-table test: eq?))
            (iter (%%fx+ n 1)))))
    lookups))


;;;
;;;; Walk Binding
;;;


(jazz.Walk-Binding-implement)


(jazz.define-method (jazz.emit-type (jazz.Walk-Binding type) source-declaration environment)
  (%%get-code-form (jazz.emit-binding-reference type source-declaration environment)))


(jazz.define-virtual (jazz.walk-binding-lookup (jazz.Walk-Binding binding) symbol))
(jazz.define-virtual (jazz.walk-binding-referenced (jazz.Walk-Binding binding)))
(jazz.define-virtual (jazz.emit-binding-reference (jazz.Walk-Binding binding) source-declaration environment))
(jazz.define-virtual (jazz.walk-binding-validate-call (jazz.Walk-Binding binding) walker resume source-declaration operator arguments))
(jazz.define-virtual (jazz.emit-binding-call (jazz.Walk-Binding binding) arguments source-declaration environment))
(jazz.define-virtual (jazz.emit-inlined-binding-call (jazz.Walk-Binding binding) arguments source-declaration environment))
(jazz.define-virtual (jazz.walk-binding-validate-assignment (jazz.Walk-Binding binding) walker resume source-declaration))
(jazz.define-virtual (jazz.walk-binding-assignable? (jazz.Walk-Binding binding)))
(jazz.define-virtual (jazz.emit-binding-assignment (jazz.Walk-Binding binding) value source-declaration environment))
(jazz.define-virtual (jazz.walk-binding-walkable? (jazz.Walk-Binding binding)))
(jazz.define-virtual (jazz.walk-binding-walk-form (jazz.Walk-Binding binding) walker resume declaration environment form))
(jazz.define-virtual (jazz.walk-binding-expandable? (jazz.Walk-Binding binding)))
(jazz.define-virtual (jazz.walk-binding-expand-form (jazz.Walk-Binding binding) walker resume declaration environment form))


(jazz.define-method (jazz.walk-binding-lookup (jazz.Walk-Binding binding) symbol)
  #f)


(jazz.define-method (jazz.walk-binding-referenced (jazz.Walk-Binding binding))
  (jazz.unspecified))


(jazz.define-method (jazz.emit-binding-reference (jazz.Walk-Binding binding) source-declaration environment)
  (jazz.error "Unable to emit binding reference for: {s}" binding))


(jazz.define-method (jazz.walk-binding-validate-call (jazz.Walk-Binding binding) walker resume source-declaration operator arguments)
  (jazz.unspecified))


(jazz.define-method (jazz.emit-binding-call (jazz.Walk-Binding binding) arguments source-declaration environment)
  (let ((type (%%get-lexical-binding-type binding)))
    (jazz.new-code
      `(,(%%get-code-form (jazz.emit-binding-reference binding source-declaration environment))
        ,@(jazz.codes-forms arguments))
      (jazz.call-return-type type))))


(jazz.define-method (jazz.emit-inlined-binding-call (jazz.Walk-Binding binding) arguments source-declaration environment)
  #f)


(jazz.define-method (jazz.walk-binding-validate-assignment (jazz.Walk-Binding binding) walker resume source-declaration)
  (%%when (%%not (jazz.walk-binding-assignable? binding))
    (jazz.walk-error walker resume source-declaration "Illegal assignment to: {s}" binding)))


(jazz.define-method (jazz.walk-binding-assignable? (jazz.Walk-Binding binding))
  #f)


(jazz.define-method (jazz.emit-binding-assignment (jazz.Walk-Binding binding) value source-declaration environment)
  (jazz.unspecified))


(jazz.define-method (jazz.walk-binding-walkable? (jazz.Walk-Binding binding))
  #f)


(jazz.define-method (jazz.walk-binding-expandable? (jazz.Walk-Binding binding))
  #f)


(jazz.encapsulate-class jazz.Walk-Binding)


;;;
;;;; Lexical Binding
;;;


(jazz.Lexical-Binding-implement)


(jazz.define-method (jazz.walk-binding-lookup (jazz.Lexical-Binding binding) symbol)
  (if (%%eq? (%%get-lexical-binding-name binding) symbol)
      binding
    #f))


(jazz.encapsulate-class jazz.Lexical-Binding)


;;;
;;;; Declaration
;;;


(jazz.Declaration-implement)


(define (jazz.setup-declaration new-declaration)
  (let ((parent (%%get-declaration-parent new-declaration))
        (name (%%get-lexical-binding-name new-declaration)))
    (%%set-declaration-locator new-declaration (if (%%not parent) name (%%compose-name (%%get-declaration-locator parent) name)))
    (%%set-declaration-toplevel new-declaration (if (%%not parent) new-declaration (%%get-declaration-toplevel parent)))))


(jazz.define-virtual (jazz.resolve-declaration (jazz.Declaration declaration)))


(jazz.define-method (jazz.resolve-declaration (jazz.Declaration declaration))
  declaration)


(define (jazz.get-declaration-path declaration)
  (letrec ((proc
            (lambda (declaration)
              (let ((name (%%get-lexical-binding-name declaration))
                    (parent (%%get-declaration-parent declaration)))
                (if (%%not parent)
                    (%%list name)
                  (%%cons name (proc parent)))))))
    (jazz.reverse! (proc declaration))))


(jazz.define-method (jazz.walk-binding-lookup (jazz.Declaration binding) symbol)
  (jazz.lookup-declaration binding symbol #f))


(jazz.define-method (jazz.walk-binding-validate-call (jazz.Declaration declaration) walker resume source-declaration operator arguments)
  (jazz.walk-error walker resume source-declaration "{a} is not callable" (%%get-declaration-locator declaration)))


(jazz.define-virtual (jazz.lookup-declaration (jazz.Declaration declaration) symbol external?))


(jazz.define-method (jazz.lookup-declaration (jazz.Declaration declaration) symbol external?)
  #f)


(jazz.define-virtual (jazz.update-declaration (jazz.Declaration declaration) new-declaration))


(jazz.define-method (jazz.update-declaration (jazz.Declaration declaration) new-declaration)
  #f)


(jazz.define-virtual (jazz.get-declaration-references (jazz.Declaration declaration)))


(jazz.define-method (jazz.get-declaration-references (jazz.Declaration declaration))
  '())


(jazz.define-virtual (jazz.emit-declaration (jazz.Declaration declaration) environment))


(jazz.define-method (jazz.emit-declaration (jazz.Declaration declaration) environment)
  (jazz.error "Unable to emit: {s}" declaration))


(jazz.define-virtual (jazz.expand-referenced-declaration (jazz.Declaration declaration)))


(jazz.define-method (jazz.expand-referenced-declaration (jazz.Declaration declaration))
  '())


(jazz.define-virtual (jazz.fold-declaration (jazz.Declaration declaration) f k s))


(jazz.define-method (jazz.fold-declaration (jazz.Declaration declaration) f k s)
  (f declaration s))


(jazz.encapsulate-class jazz.Declaration)


;;;
;;;; Declaration Reference
;;;


(jazz.Declaration-Reference-implement)


(jazz.define-virtual (jazz.resolve-reference (jazz.Declaration-Reference declaration-reference) library-declaration))


(jazz.encapsulate-class jazz.Declaration-Reference)


;;;
;;;; Library Reference
;;;


(jazz.Library-Reference-implement)


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


(jazz.Export-Reference-implement)


(define (jazz.new-export-reference name declaration library-reference)
  (jazz.allocate-export-reference jazz.Export-Reference name declaration library-reference))


(jazz.define-method (jazz.resolve-reference (jazz.Declaration-Reference declaration-reference) library-declaration)
  (or (%%get-declaration-reference-declaration declaration-reference)
      (receive (name symbol) (jazz.parse-exported-symbol library-declaration (%%get-declaration-reference-name declaration-reference))
        (let ((declaration (jazz.new-export-declaration name #f 'public 'uptodate '() #f symbol)))
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


(jazz.Autoload-Reference-implement)


(define (jazz.new-autoload-reference name declaration library-reference)
  (jazz.allocate-autoload-reference jazz.Autoload-Reference name declaration library-reference))


(define (jazz.resolve-autoload-reference declaration-reference library-declaration exported-library-reference)
  (or (%%get-declaration-reference-declaration declaration-reference)
      (let* ((name (%%get-declaration-reference-name declaration-reference))
             (type jazz.Any)
             (declaration (jazz.new-autoload-declaration name type #f library-declaration exported-library-reference)))
        (%%assert declaration
          (%%set-declaration-reference-declaration declaration-reference declaration)
          declaration))))


(jazz.encapsulate-class jazz.Autoload-Reference)


;;;
;;;; Module
;;;


(jazz.Module-Declaration-implement)


(define (jazz.new-module-declaration name parent requires)
  (let ((new-declaration (jazz.allocate-module-declaration jazz.Module-Declaration name #f 'public 'uptodate '() #f parent #f requires)))
    (jazz.setup-declaration new-declaration)
    new-declaration))


(jazz.encapsulate-class jazz.Module-Declaration)


;;;
;;;; Namespace
;;;


(jazz.Namespace-Declaration-implement)


(define (jazz.find-declaration namespace-declaration name)
  (%%table-ref (%%get-access-lookup namespace-declaration jazz.private-access) name #f))


(jazz.encapsulate-class jazz.Namespace-Declaration)


;;;
;;;; Library
;;;


(jazz.Library-Declaration-implement)


(define (jazz.new-library-declaration name parent dialect requires exports imports)
  (let ((new-declaration (jazz.allocate-library-declaration jazz.Library-Declaration name #f 'public 'uptodate '() #f parent #f (jazz.make-access-lookups jazz.public-access) (%%make-table test: eq?) '() #f dialect requires exports imports #f '() (jazz.new-queue) '() '())))
    (jazz.setup-declaration new-declaration)
    new-declaration))


(define (jazz.setup-library-lookups library-declaration)
  (for-each (lambda (imported-library-invoice)
              (jazz.install-library-import library-declaration imported-library-invoice))
            (%%get-library-declaration-imports library-declaration))
  
  (for-each (lambda (exported-library-invoice)
              (jazz.install-library-export library-declaration exported-library-invoice))
            (%%get-library-declaration-exports library-declaration)))


(define (jazz.install-library-import library-declaration imported-library-invoice)
  (let ((private (%%get-access-lookup library-declaration jazz.private-access))
        (only (%%get-library-invoice-only imported-library-invoice)))
    (if only
        ;; todo
        #f
      (let ((imported-library-declaration (%%get-library-invoice-library imported-library-invoice)))
        (let ((imported (%%get-access-lookup imported-library-declaration jazz.public-access)))
          (%%table-merge! private imported))))))


(define (jazz.install-library-export library-declaration exported-library-invoice)
  (let ((public (%%get-access-lookup library-declaration jazz.public-access))
        (only (%%get-library-invoice-only exported-library-invoice))
        (autoload (%%get-export-invoice-autoload exported-library-invoice)))
    (cond (only
            (for-each (lambda (declaration-reference)
                        (let ((name (jazz.identifier-name (%%get-declaration-reference-name declaration-reference))))
                          (%%table-set! public name declaration-reference)))
                      only))
          (autoload
            (let ((exported-library-reference (%%get-library-invoice-library exported-library-invoice)))
              (for-each (lambda (declaration-reference)
                          (let ((name (jazz.identifier-name (%%get-declaration-reference-name declaration-reference))))
                            (%%table-set! public name (jazz.resolve-autoload-reference declaration-reference library-declaration exported-library-reference))))
                        autoload)))
          (else
           (let ((exported-library-declaration (jazz.resolve-reference (%%get-library-invoice-library exported-library-invoice) library-declaration)))
             (%%table-merge! public (%%get-access-lookup exported-library-declaration jazz.public-access)))))))


(jazz.define-method (jazz.lookup-declaration (jazz.Library-Declaration declaration) symbol external?)
  (let ((access (if external? jazz.public-access jazz.private-access)))
    (%%table-ref (%%vector-ref (%%get-namespace-declaration-lookups declaration) access)
                 symbol
                 #f)))


(jazz.define-method (jazz.update-declaration (jazz.Library-Declaration declaration) new-declaration)
  (let ((actual-imports (%%get-library-declaration-imports declaration)))
    (for-each (lambda (new-invoice)
                (let ((new-imported-library (%%get-library-invoice-library new-invoice)))
                  (let ((actual-invoice (jazz.find-if (lambda (invoice) (%%eq? (%%get-library-invoice-library invoice) new-imported-library))
                                                      actual-imports)))
                    (if actual-invoice
                        ;; need to merge new invoice
                        #f
                      (begin
                        (%%set-library-declaration-imports declaration (%%cons new-invoice (%%get-library-declaration-imports declaration)))
                        (jazz.install-library-import declaration new-invoice))))))
              (%%get-library-declaration-imports new-declaration)))
  (let ((actual-exports (%%get-library-declaration-exports declaration)))
    (for-each (lambda (new-invoice)
                (let ((new-exported-library (%%get-library-invoice-library new-invoice)))
                  (let ((actual-invoice (jazz.find-if (lambda (invoice) (%%eq? (%%get-library-invoice-library invoice) new-exported-library))
                                                      actual-exports)))
                    (if actual-invoice
                        ;; need to merge new invoice
                        #f
                      (begin
                        (%%set-library-declaration-exports declaration (%%cons new-invoice (%%get-library-declaration-exports declaration)))
                        (jazz.install-library-export declaration new-invoice))))))
              (%%get-library-declaration-exports new-declaration))))


(jazz.define-method (jazz.emit-declaration (jazz.Library-Declaration declaration) environment)
  (let ((body-expansion (jazz.emit-namespace-statements (%%get-namespace-declaration-body declaration) declaration environment))
        (references-expansion (jazz.expand-library-references declaration))
        (literals-expansion (jazz.expand-library-literals declaration))
        (variables-expansion (jazz.expand-library-variables declaration))
        (autoloads-expansion (jazz.expand-library-autoloads declaration environment))
        (locator (%%get-declaration-locator declaration)))
    `(begin
       ,@(jazz.declares 'library)
       ,@(let ((queue (jazz.new-queue)))
           (for-each (lambda (spec)
                       (jazz.parse-require spec
                         (lambda (module-name feature-requirement load phase)
                           (jazz.enqueue queue `(jazz.load-module ',module-name)))))
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
       ,@(case (jazz.walk-for)
           ((compile)
            `((define (__final-dispatch object field ignore type)
                (%%assertion (%%category-is? object type) (jazz.dispatch-error object type)
                  (%%final-dispatch object (%%get-method-implementation field))))
              (define (__class-dispatch object class-level implementation-rank type)
                (%%assertion (%%category-is? object type) (jazz.dispatch-error object type)
                  (%%class-dispatch object class-level implementation-rank)))
              (define (__interface-dispatch object interface-rank implementation-rank type)
                (%%assertion (%%category-is? object type) (jazz.dispatch-error object type)
                  (%%interface-dispatch object interface-rank implementation-rank)))
              (define __dispatchers
                (%%vector __final-dispatch __class-dispatch __interface-dispatch))))
           (else
            '()))
       ,@references-expansion
       ,@literals-expansion
       ,@variables-expansion
       ,@autoloads-expansion
       ,@body-expansion
       (jazz.module-loaded ',locator))))


(jazz.define-method (jazz.fold-declaration (jazz.Library-Declaration declaration) f k s)
  (f declaration (jazz.fold-statements (%%get-namespace-declaration-body declaration) f k s s)))


(jazz.encapsulate-class jazz.Library-Declaration)


;;;
;;;; Library Invoice
;;;


(jazz.Library-Invoice-implement)


(jazz.encapsulate-class jazz.Library-Invoice)


;;;
;;;; Export Invoice
;;;


(jazz.Export-Invoice-implement)


(define (jazz.new-export-invoice library phase version only autoload)
  (jazz.allocate-export-invoice jazz.Export-Invoice library phase version only #f #f #f autoload))


(jazz.encapsulate-class jazz.Export-Invoice)


;;;
;;;; Import Invoice
;;;


(jazz.Import-Invoice-implement)


(define (jazz.new-import-invoice library phase version only)
  (jazz.allocate-import-invoice jazz.Import-Invoice library phase version only #f #f #f))


(jazz.encapsulate-class jazz.Import-Invoice)


;;;
;;;; Export
;;;


(jazz.Export-Declaration-implement)


(define (jazz.new-export-declaration name type access compatibility attributes parent symbol)
  (let ((new-declaration (jazz.allocate-export-declaration jazz.Export-Declaration name type access compatibility attributes #f parent #f symbol)))
    (jazz.setup-declaration new-declaration)
    new-declaration))


(jazz.define-method (jazz.walk-binding-validate-call (jazz.Export-Declaration declaration) walker resume source-declaration operator arguments)
  (jazz.unspecified))


(jazz.define-method (jazz.emit-declaration (jazz.Export-Declaration declaration) environment)
  `(begin))


(jazz.define-method (jazz.emit-binding-reference (jazz.Export-Declaration declaration) source-declaration environment)
  (jazz.new-code
    (%%get-export-declaration-symbol declaration)
    jazz.Any))


(jazz.encapsulate-class jazz.Export-Declaration)


;;;
;;;; Autoload
;;;


(jazz.Autoload-Declaration-implement)


(define (jazz.new-autoload-declaration name type parent library-declaration exported-library)
  (let ((new-declaration (jazz.allocate-autoload-declaration jazz.Autoload-Declaration name type 'public 'uptodate '() #f parent #f library-declaration exported-library #f)))
    (jazz.setup-declaration new-declaration)
    new-declaration))


(jazz.define-method (jazz.of-subtype? (jazz.Autoload-Declaration declaration) subtype)
  ;; not sure calling resolve here is correct
  (jazz.of-subtype? (jazz.resolve-declaration declaration) subtype))


(jazz.define-method (jazz.resolve-declaration (jazz.Autoload-Declaration declaration))
  (or (%%get-autoload-declaration-declaration declaration)
      (let* ((exported-library (jazz.resolve-reference (%%get-autoload-declaration-exported-library declaration) (%%get-autoload-declaration-library declaration)))
             (name (%%get-lexical-binding-name declaration))
             (decl (jazz.lookup-declaration exported-library name #t)))
        (%%set-autoload-declaration-declaration declaration decl)
        (%%assertion decl (jazz.error "Unable to find autoload: {s}" name)
          decl))))


(jazz.define-method (jazz.emit-binding-reference (jazz.Autoload-Declaration declaration) source-declaration environment)
  (let ((referenced-declaration (jazz.resolve-declaration declaration)))
    (jazz.new-code
      `(,(jazz.autoload-locator referenced-declaration))
      (jazz.resolve-declaration declaration))))


(define (jazz.autoload-locator referenced-declaration)
  (%%string->symbol (%%string-append (%%symbol->string (%%get-declaration-locator referenced-declaration))
                                     ":autoload")))


(jazz.encapsulate-class jazz.Autoload-Declaration)


;;;
;;;; Void
;;;


(jazz.Void-Class-implement)


(jazz.define-method (jazz.of-subtype? (jazz.Void-Class type) subtype)
  #f)


(jazz.define-method (jazz.emit-specifier (jazz.Void-Class type))
  'void)


(jazz.encapsulate-class jazz.Void-Class)


(jazz.Void-implement)


(jazz.encapsulate-class jazz.Void)


;;;
;;;; Opt
;;;


(jazz.Opt-Type-implement)


(define (jazz.new-opt-type type)
  (jazz.allocate-opt-type jazz.Opt-Type type))


(jazz.define-method (jazz.emit-specifier (jazz.Opt-Type type))
  (let ((type-specifier (jazz.emit-specifier (%%get-opt-type-type type))))
    (%%string->symbol (%%string-append "opt<" (%%symbol->string type-specifier) ">"))))


(jazz.encapsulate-class jazz.Opt-Type)


;;;
;;;; Key
;;;


(jazz.Key-Type-implement)


(define (jazz.new-key-type key type)
  (jazz.allocate-key-type jazz.Key-Type key type))


(jazz.define-method (jazz.emit-specifier (jazz.Key-Type type))
  (let ((key (%%get-key-type-key type))
        (type-specifier (jazz.emit-specifier (%%get-key-type-type type))))
    (%%string->symbol (%%string-append "key<" (%%keyword->string key) ":" (%%symbol->string type-specifier) ">"))))


(jazz.encapsulate-class jazz.Key-Type)


;;;
;;;; Rest
;;;


(jazz.Rest-Type-implement)


(define (jazz.new-rest-type type)
  (jazz.allocate-rest-type jazz.Rest-Type type))


(jazz.define-method (jazz.emit-specifier (jazz.Rest-Type type))
  (let ((type-specifier (jazz.emit-specifier (%%get-rest-type-type type))))
    (%%string->symbol (%%string-append (%%symbol->string type-specifier) "*"))))


(jazz.encapsulate-class jazz.Rest-Type)


;;;
;;;; Function
;;;


;; should probably be unified with the Signature class


(jazz.Function-Type-implement)


(define (jazz.new-function-type positional optional named rest result)
  (let ((mandatory (%%length positional)))
    (jazz.allocate-function-type jazz.Function-Type mandatory positional optional named rest result)))


(jazz.define-method (jazz.of-subtype? (jazz.Function-Type type) subtype)
  (or (jazz.of-subtype? jazz.Procedure subtype)
      (%%eq? (%%get-object-class subtype) jazz.Function-Type)))


(jazz.define-method (jazz.emit-specifier (jazz.Function-Type type))
  (let ((output (open-output-string)))
    (let ((first? #t))
      (for-each (lambda (type)
                  (if first?
                      (set! first? #f)
                    (write-char #\^ output))
                  (display (jazz.emit-specifier type) output))
                (%%get-function-type-positional type))
      (let ((rest (%%get-function-type-rest type)))
        (%%when rest
          (%%when (%%not first?)
            (write-char #\^ output))
          (display (jazz.emit-specifier type) output))))
    (write-char #\: output)
    (display (jazz.emit-specifier (%%get-function-type-result type)) output)
    (%%string->symbol (get-output-string output))))


(jazz.define-method (jazz.emit-check (jazz.Function-Type type) value source-declaration environment)
  `(if (%%not (%%procedure? ,value))
       (jazz.type-error ,value jazz.Procedure)))


(jazz.encapsulate-class jazz.Function-Type)


;;;
;;;; Category
;;;


;; first draft. this type is used to support specializing new and the like


(jazz.Category-Type-implement)


(define (jazz.new-category-type declaration)
  (jazz.allocate-category-type jazz.Category-Type declaration))


;; quicky solution to stop casts on this type
(jazz.define-method (jazz.of-subtype? (jazz.Category-Type type) subtype)
  #t)


(jazz.define-method (jazz.emit-check (jazz.Category-Type type) value source-declaration environment)
  #f)


(jazz.define-method (jazz.emit-specifier (jazz.Category-Type type))
  (let ((output (open-output-string)))
    (display "category" output)
    (write-char #\< output)
    (display (jazz.emit-specifier (%%get-category-type-declaration type)) output)
    (write-char #\> output)
    (%%string->symbol (get-output-string output))))


(jazz.encapsulate-class jazz.Category-Type)


;;;
;;;; Values
;;;


(jazz.Values-Type-implement)


(define (jazz.new-values-type types)
  (jazz.allocate-values-type jazz.Values-Type types))


(jazz.define-method (jazz.emit-specifier (jazz.Values-Type type))
  (let ((output (open-output-string)))
    (display "values" output)
    (write-char #\< output)
    (let ((first? #t))
      (for-each (lambda (type)
                  (if first?
                      (set! first? #f)
                    (write-char #\/ output))
                  (display (jazz.emit-specifier type) output))
                (%%get-values-type-types type)))
    (write-char #\> output)
    (%%string->symbol (get-output-string output))))


(jazz.encapsulate-class jazz.Values-Type)


;;;
;;;; Restriction
;;;


(jazz.Restriction-Type-implement)


(define (jazz.new-restriction-type base type)
  (jazz.allocate-restriction-type jazz.Restriction-Type base type))


(jazz.encapsulate-class jazz.Restriction-Type)


;;;
;;;; Complement
;;;


(jazz.Complement-Type-implement)


(define (jazz.new-complement-type type)
  (jazz.allocate-complement-type jazz.Complement-Type type))


(jazz.encapsulate-class jazz.Complement-Type)


;;;
;;;; Union
;;;


(jazz.Union-Type-implement)


(define (jazz.new-union-type types)
  (jazz.allocate-union-type jazz.Union-Type types))


(jazz.encapsulate-class jazz.Union-Type)


;;;
;;;; Template
;;;


;; future work. just here to make sure specifier syntax can express them


(jazz.Template-Type-implement)


(define (jazz.new-template-type class types)
  (jazz.allocate-template-type jazz.Template-Type class types))


(jazz.define-method (jazz.emit-specifier (jazz.Template-Type type))
  (let ((output (open-output-string)))
    (display (jazz.emit-specifier (%%get-template-type-class type)) output)
    (write-char #\< output)
    (let ((first? #t))
      (for-each (lambda (type)
                  (if first?
                      (set! first? #f)
                    (write-char #\/ output))
                  (display (jazz.emit-specifier type) output))
                (%%get-template-type-types type)))
    (write-char #\> output)
    (%%string->symbol (get-output-string output))))


(jazz.encapsulate-class jazz.Template-Type)


;;;
;;;; Nillable
;;;


(jazz.Nillable-Type-implement)


(define (jazz.new-nillable-type type)
  (jazz.allocate-nillable-type jazz.Nillable-Type type))


(jazz.define-method (jazz.of-subtype? (jazz.Nillable-Type type) subtype)
  (or (jazz.of-subtype? jazz.Boolean subtype)
      (jazz.of-subtype? (%%get-nillable-type-type type) subtype)))


(jazz.define-method (jazz.emit-specifier (jazz.Nillable-Type type))
  (let ((type-specifier (jazz.emit-specifier (%%get-nillable-type-type type))))
    (%%string->symbol (%%string-append (%%symbol->string type-specifier) "+"))))


(jazz.define-method (jazz.emit-check (jazz.Nillable-Type type) value source-declaration environment)
  ;; for tests
  #f)


(jazz.encapsulate-class jazz.Nillable-Type)


;;;
;;;; Object
;;;


(define jazz.object-declaration? #f)

(set! jazz.object-declaration?
      (lambda (type)
        #f))


;;;
;;;; Any
;;;


(jazz.Any-Class-implement)


(jazz.define-method (jazz.of-subtype? (jazz.Any-Class type) subtype)
  #t)


(jazz.define-method (jazz.emit-specifier (jazz.Any-Class type))
  'any)


(jazz.define-method (jazz.emit-check (jazz.Any-Class type) value source-declaration environment)
  #f)


(jazz.encapsulate-class jazz.Any-Class)


(jazz.Any-implement)


(jazz.encapsulate-class jazz.Any)


;;;
;;;; Cast
;;;


;; Todo: unify the 2 versions of both procedures, where the only
;; difference is that we do not do the emit-check when in release


(cond-expand
  (release
    (define (jazz.emit-type-cast code type source-declaration environment)
      (if (or (%%not type) (%%subtype? (%%get-code-type code) type))
         (%%get-code-form code)
       (let ((value (jazz.generate-symbol "val")))
         ;; coded the flonum case here for now has it is the only castable type
         (if (%%eq? type jazz.Flonum)
             `(let ((,value ,(%%get-code-form code)))
                (if (%%fixnum? ,value)
                    (%%fixnum->flonum ,value)
                  ,value))
           (%%get-code-form code))))))
  (else
   (define (jazz.emit-type-cast code type source-declaration environment)
     (if (or (%%not type) (%%subtype? (%%get-code-type code) type))
         (%%get-code-form code)
       (let ((value (jazz.generate-symbol "val")))
         ;; coded the flonum case here for now has it is the only castable type
         (if (%%eq? type jazz.Flonum)
             `(let ((,value ,(%%get-code-form code)))
                (if (%%fixnum? ,value)
                    (%%fixnum->flonum ,value)
                  (begin
                    ,(jazz.emit-check type value source-declaration environment)
                    ,value)))
           `(let ((,value ,(%%get-code-form code)))
              ,(jazz.emit-check type value source-declaration environment)
              ,value)))))))


(cond-expand
  (release
    (define (jazz.emit-parameter-cast code type source-declaration environment)
      (if (or (%%not type) (%%eq? type jazz.Any) (%%object-class? type) (jazz.object-declaration? type))
         #f
       (let ((parameter (%%get-code-form code)))
         ;; coded the flonum case here for now has it is the only castable type
         (if (%%eq? type jazz.Flonum)
             `(if (%%fixnum? ,parameter)
                  (set! ,parameter (%%fixnum->flonum ,parameter)))
           #f)))))
  (else
   (define (jazz.emit-parameter-cast code type source-declaration environment)
     (if (or (%%not type) (%%eq? type jazz.Any) (%%object-class? type) (jazz.object-declaration? type))
         #f
       (let ((parameter (%%get-code-form code)))
         ;; coded the flonum case here for now has it is the only castable type
         (if (%%eq? type jazz.Flonum)
             `(if (%%fixnum? ,parameter)
                  (set! ,parameter (%%fixnum->flonum ,parameter))
                ,(jazz.emit-check type parameter source-declaration environment))
           (jazz.emit-check type parameter source-declaration environment)))))))


;;;
;;;; Specifier
;;;


;; <any>
;; <Point+>                            ;; Nillable
;; <Point<fx/pair>>                    ;; Template
;; <category<Cell>>                    ;; Category
;; <fx^fx^opt<fx>^key<k:fx>^fx*:pair>  ;; Function with positional, optional, named and rest parameters
;; <values<fx/fl>>                     ;; Values
;; <Object>
;; <Point>
;; <pair>
;; <fx>
;; <void>


(define (jazz.parse-specifier lst proc)
  (if (and (%%pair? lst) (jazz.specifier? (%%car lst)))
      (proc (%%car lst) (%%cdr lst))
    (proc #f lst)))


(define (jazz.walk-specifier walker resume declaration environment specifier)
  (let ((string (%%symbol->string specifier)))
    (let ((input (open-input-string string))
          (at 0))
      (define (ill-formed message)
        (let ((error-message (jazz.format "Ill-formed specifier {s} : at {a} : {a}" specifier (%%substring string 0 at) message)))
          (if (not walker)
              (jazz.error "{a}" error-message)
            (jazz.walk-error walker resume declaration error-message))))
      
      (define (peekc)
        (peek-char input))
      
      (define (readc)
        (let ((c (read-char input)))
          (set! at (%%fx+ at 1))
          c))
      
      (define (consume c)
        (if (%%not (%%eqv? (readc) c))
            (ill-formed (jazz.format "{s} expected" c))))
            
      (define (lookup-type name)
        (or (jazz.lookup-primitive-type name)
            (jazz.lookup-reference walker resume declaration environment name)
            (ill-formed (jazz.format "{s} not found" name))))

      (define (parse-until separator terminator)
        (let ((queue (jazz.new-queue)))
          (let iter ()
               (if (%%eqv? (peekc) terminator)
                   (begin
                     (readc)
                     (jazz.queue-list queue))
                 (begin
                   (jazz.enqueue queue (parse #t))
                   (let ((next (peekc)))
                     (cond ((%%eqv? next separator)
                            (readc)
                            (iter))
                           ((%%eqv? next terminator)
                            (iter))
                           (else
                            (ill-formed (jazz.format "{s} terminator expected" terminator))))))))))
      
      (define (parse-name)
        (let ((output (open-output-string)))
          (let iter ()
               (let ((c (peekc)))
                 (if (or (%%eof-object? c)
                         (%%eqv? c #\<)
                         (%%eqv? c #\>)
                         (%%eqv? c #\^)
                         (%%eqv? c #\*)
                         (%%eqv? c #\:)
                         (%%eqv? c #\/)
                         (%%eqv? c #\+))
                     (%%string->symbol (get-output-string output))
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
               (jazz.new-nillable-type (lookup-type name)))
              ((#\*)
               (readc)
               (jazz.new-rest-type (lookup-type name)))
              ((#\<)
               (readc)
               (cond ((%%eq? name 'opt)
                      (let ((type (parse #t)))
                        (consume #\>)
                        (jazz.new-opt-type type)))
                     ((%%eq? name 'key)
                      (let ((key (%%string->keyword (%%symbol->string (parse-name)))))
                        (consume #\:)
                        (let ((type (parse #t)))
                          (consume #\>)
                          (jazz.new-key-type key type))))
                     ((%%eq? name 'category)
                      (jazz.new-category-type (parse #t)))
                     ((%%eq? name 'values)
                      (jazz.new-values-type (parse-until #\/ #\>)))
                     (else
                      (jazz.new-template-type (lookup-type name) (parse-until #\/ #\>)))))
              (else
               (lookup-type name))))))
      
      (define (new-function-type parameters result)
        (define (split-parameters types proc)
          (if (%%null? types)
              (proc '() '() '() #f)
            (let ((last (jazz.last types)))
              (if (%%class-is? last jazz.Rest-Type)
                  (proc (jazz.butlast types) '() '() last)
                (proc types '() '() #f)))))
        
        (split-parameters parameters
          (lambda (positional optional named rest)
            (jazz.new-function-type positional optional named rest result))))
      
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
                       (new-function-type (list type) (parse #t)))))
                  ((#\^)
                   (if atomic?
                       type
                     (begin
                       (readc)
                       (let ((parameters (cons type (parse-until #\^ #\:))))
                         (new-function-type parameters (parse #t))))))
                  (else
                   type)))))))
      
      (parse #f))))


(define (jazz.specifier->type walker resume declaration environment specifier)
  (if specifier
      (jazz.walk-specifier walker resume declaration environment specifier)
    #f))


(define (jazz.type->specifier type)
  (let ((symbol (jazz.emit-specifier type)))
    (if (jazz.specifier? symbol)
        symbol
      (jazz.name->specifier symbol))))


;;;
;;;; Primitive Types
;;;


(define jazz.primitive-types
  (%%make-table test: eq?))


(%%table-set! jazz.primitive-types 'any       jazz.Any)
(%%table-set! jazz.primitive-types 'object    jazz.Object)
(%%table-set! jazz.primitive-types 'bool      jazz.Boolean)
(%%table-set! jazz.primitive-types 'char      jazz.Char)
(%%table-set! jazz.primitive-types 'number    jazz.Number)
(%%table-set! jazz.primitive-types 'complex   jazz.Complex)
(%%table-set! jazz.primitive-types 'real      jazz.Real)
(%%table-set! jazz.primitive-types 'rational  jazz.Rational)
(%%table-set! jazz.primitive-types 'int       jazz.Integer)
(%%table-set! jazz.primitive-types 'fx        jazz.Fixnum)
(%%table-set! jazz.primitive-types 'fl        jazz.Flonum)
(%%table-set! jazz.primitive-types 'list      jazz.List)
(%%table-set! jazz.primitive-types 'null      jazz.Null)
(%%table-set! jazz.primitive-types 'pair      jazz.Pair)
(%%table-set! jazz.primitive-types 'port      jazz.Port)
(%%table-set! jazz.primitive-types 'procedure jazz.Procedure)
(%%table-set! jazz.primitive-types 'foreign   jazz.Foreign)
(%%table-set! jazz.primitive-types 'string    jazz.String)
(%%table-set! jazz.primitive-types 'symbol    jazz.Symbol)
(%%table-set! jazz.primitive-types 'keyword   jazz.Keyword)
(%%table-set! jazz.primitive-types 'vector    jazz.Vector)
(%%table-set! jazz.primitive-types 'table     jazz.Table)
(%%table-set! jazz.primitive-types 'promise   jazz.Promise)
(%%table-set! jazz.primitive-types 'void      jazz.Void)


(define (jazz.lookup-primitive-type name)
  (%%table-ref jazz.primitive-types name #f))


(define jazz.primitive-declarations
  (list
    (cons jazz.Object    'Object)
    (cons jazz.Boolean   'Boolean)
    (cons jazz.Char      'Char)
    (cons jazz.Number    'Number)
    (cons jazz.Complex   'Complex)
    (cons jazz.Real      'Real)
    (cons jazz.Rational  'Rational)
    (cons jazz.Integer   'Integer)
    (cons jazz.Fixnum    'Fixnum)
    (cons jazz.Flonum    'Flonum)
    (cons jazz.List      'List)
    (cons jazz.Null      'Null)
    (cons jazz.Pair      'Pair)
    (cons jazz.Port      'Port)
    (cons jazz.Procedure 'Procedure)
    (cons jazz.Foreign   'Foreign)
    (cons jazz.String    'String)
    (cons jazz.Symbol    'Symbol)
    (cons jazz.Keyword   'Keyword)
    (cons jazz.Vector    'Vector)
    (cons jazz.Table     'Table)
    (cons jazz.Promise   'Promise)))


;; quicky until we can somehow unify primitive types and declarations
(define (jazz.patch-type-until-unification type)
  (let ((pair (assq type jazz.primitive-declarations)))
    (if pair
        (let ((library-declaration (jazz.get-catalog-entry 'jazz.dialect.language)))
          (if library-declaration
              (jazz.lookup-declaration library-declaration (%%cdr pair) #t)
            type))
      type)))


;;;
;;;; Macro
;;;


(jazz.Macro-Declaration-implement)


(define (jazz.new-macro-declaration name type access compatibility attributes parent signature)
  (let ((new-declaration (jazz.allocate-macro-declaration jazz.Macro-Declaration name type access compatibility attributes #f parent #f signature #f)))
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


(jazz.define-method (jazz.emit-declaration (jazz.Macro-Declaration declaration) environment)
  (let ((locator (%%get-declaration-locator declaration))
        (signature (%%get-macro-declaration-signature declaration))
        (body (%%get-macro-declaration-body declaration)))
    (jazz.with-annotated-frame (jazz.annotate-signature signature)
      (lambda (frame)
        (let ((augmented-environment (cons frame environment)))
          `(jazz.define-macro ,(%%cons locator (jazz.emit-signature signature declaration augmented-environment))
             ,@(%%get-code-form (jazz.emit-expression body declaration augmented-environment))))))))


(jazz.define-method (jazz.fold-declaration (jazz.Macro-Declaration declaration) f k s)
  (f declaration
     (k (%%get-macro-declaration-signature declaration)
        (k (jazz.fold-statement (%%get-macro-declaration-body declaration) f k s)
           s))))


(jazz.encapsulate-class jazz.Macro-Declaration)


;;;
;;;; Syntax
;;;


(jazz.Syntax-Declaration-implement)


(define (jazz.new-syntax-declaration name type access compatibility attributes parent signature)
  (let ((new-declaration (jazz.allocate-syntax-declaration jazz.Syntax-Declaration name type access compatibility attributes #f parent #f signature #f)))
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


(jazz.define-method (jazz.emit-declaration (jazz.Syntax-Declaration declaration) environment)
  (let ((locator (%%get-declaration-locator declaration))
        (signature (%%get-syntax-declaration-signature declaration))
        (body (%%get-syntax-declaration-body declaration)))
    (jazz.with-annotated-frame (jazz.annotate-signature signature)
      (lambda (frame)
        (let ((augmented-environment (cons frame environment)))
          `(jazz.define-macro ,(%%cons locator (jazz.emit-signature signature declaration augmented-environment))
             ,@(%%get-code-form (jazz.emit-expression body declaration augmented-environment))))))))


(jazz.define-method (jazz.fold-declaration (jazz.Syntax-Declaration declaration) f k s)
  (f declaration
     (k (%%get-syntax-declaration-signature declaration)
        (k (jazz.fold-statement (%%get-syntax-declaration-body declaration) f k s)
           s))))


(jazz.encapsulate-class jazz.Syntax-Declaration)


;;;
;;;; C Type
;;;


(jazz.C-Type-Declaration-implement)


(define (jazz.new-c-type-declaration name type access compatibility attributes parent kind expansion references)
  (let ((new-declaration (jazz.allocate-c-type-declaration jazz.C-Type-Declaration name type access compatibility attributes #f parent #f kind expansion references)))
    (jazz.setup-declaration new-declaration)
    new-declaration))


(jazz.define-method (jazz.get-declaration-references (jazz.C-Type-Declaration declaration))
  (%%get-c-type-declaration-references declaration))


(jazz.define-method (jazz.emit-declaration (jazz.C-Type-Declaration declaration) environment)
  `(begin))


(jazz.define-method (jazz.expand-referenced-declaration (jazz.C-Type-Declaration declaration))
  (let ((locator (%%get-declaration-locator declaration))
        (expansion (%%get-c-type-declaration-expansion declaration)))
    `(c-define-type ,locator ,expansion)))


(jazz.encapsulate-class jazz.C-Type-Declaration)


;;;
;;;; C Definition
;;;


(jazz.C-Definition-Declaration-implement)


(define (jazz.new-c-definition-declaration name type access compatibility attributes parent signature parameter-types result-type c-name scope)
  (let ((new-declaration (jazz.allocate-c-definition-declaration jazz.C-Definition-Declaration name type access compatibility attributes #f parent #f signature parameter-types result-type c-name scope #f)))
    (jazz.setup-declaration new-declaration)
    new-declaration))


(jazz.define-method (jazz.walk-binding-validate-call (jazz.C-Definition-Declaration declaration) walker resume source-declaration operator arguments)
  (let ((signature (%%get-c-definition-declaration-signature declaration)))
    (if signature
        (jazz.validate-arguments walker resume source-declaration declaration signature arguments))))


(jazz.define-method (jazz.emit-declaration (jazz.C-Definition-Declaration declaration) environment)
  (let ((locator (%%get-declaration-locator declaration))
        (signature (%%get-c-definition-declaration-signature declaration))
        (parameter-types (%%get-c-definition-declaration-parameter-types declaration))
        (result-type (%%get-c-definition-declaration-result-type declaration))
        (c-name (%%get-c-definition-declaration-c-name declaration))
        (scope (%%get-c-definition-declaration-scope declaration))
        (body (%%get-c-definition-declaration-body declaration)))
    (jazz.with-annotated-frame (jazz.annotate-signature signature)
      (lambda (frame)
        (let ((augmented-environment (cons frame environment)))
          `(c-define ,(%%cons locator (jazz.emit-signature signature declaration augmented-environment)) ,parameter-types ,result-type ,c-name ,scope
             ,@(%%get-code-form (jazz.emit-expression body declaration augmented-environment))))))))


(jazz.define-method (jazz.emit-binding-reference (jazz.C-Definition-Declaration declaration) source-declaration environment)
  (jazz.new-code
    (%%get-declaration-locator declaration)
    jazz.Any))


(jazz.encapsulate-class jazz.C-Definition-Declaration)


;;;
;;;; Walk Context
;;;


(jazz.Walk-Context-implement)


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


(jazz.Walk-Location-implement)


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


(jazz.Walk-Error-implement)


(define (jazz.new-walk-error location message)
  (jazz.allocate-walk-error jazz.Walk-Error message location))


(jazz.define-method (jazz.present-exception (jazz.Walk-Error error))
  (%%get-error-message error))


(jazz.encapsulate-class jazz.Walk-Error)


;;;
;;;; Unresolved Error
;;;


(jazz.Unresolved-Error-implement)


(define (jazz.new-unresolved-error location symbol)
  (jazz.allocate-unresolved-error jazz.Unresolved-Error #f location symbol))


(jazz.define-method (jazz.present-exception (jazz.Unresolved-Error error))
  (jazz.format "Unresolved symbol: {s}"
               (%%get-unresolved-error-symbol error)))


(jazz.encapsulate-class jazz.Unresolved-Error)


;;;
;;;; Walk Frame
;;;


(jazz.Walk-Frame-implement)


(define (jazz.new-walk-frame bindings)
  (let ((table (%%make-table test: eq?)))
    (for-each (lambda (binding)
                (let ((name (%%get-lexical-binding-name binding)))
                  (%%table-set! table name binding)))
              bindings)
    (jazz.allocate-walk-frame jazz.Walk-Frame table)))


(jazz.define-method (jazz.walk-binding-lookup (jazz.Walk-Frame binding) symbol)
  (%%table-ref (%%get-walk-frame-bindings binding) symbol #f))


(jazz.encapsulate-class jazz.Walk-Frame)


;;;
;;;; Signature
;;;


(jazz.Signature-implement)


(define (jazz.new-signature positional optional named rest)
  (let ((mandatory (%%length positional)))
    (jazz.allocate-signature jazz.Signature mandatory positional optional named rest)))


(define (jazz.only-positional? signature)
  (and (%%null? (%%get-signature-optional signature))
       (%%null? (%%get-signature-named signature))
       (%%not (%%get-signature-rest signature))))


(jazz.encapsulate-class jazz.Signature)


;;;
;;;; Symbol Binding
;;;


(jazz.Symbol-Binding-implement)


(jazz.encapsulate-class jazz.Symbol-Binding)


;;;
;;;; Variable
;;;


(jazz.Variable-implement)


(define (jazz.new-variable name type)
  (%%assertion (jazz.variable-name-valid? name) (jazz.error "Invalid variable name: {s}" name)
    (jazz.allocate-variable jazz.Variable name type 0)))


(define (jazz.variable-name-valid? name)
  (and (%%symbol? name)
       (%%not (jazz.specifier? name))))


(jazz.define-method (jazz.walk-binding-referenced (jazz.Variable binding))
  (%%set-variable-reference-count binding (%%fx+ (%%get-variable-reference-count binding) 1)))


(jazz.define-method (jazz.emit-binding-reference (jazz.Variable binding) source-declaration environment)
  (jazz.new-code
    (%%get-lexical-binding-name binding)
    (jazz.find-annotated-type binding environment)))


(jazz.define-method (jazz.walk-binding-assignable? (jazz.Variable declaration))
  #t)


(jazz.define-method (jazz.emit-binding-assignment (jazz.Variable binding) value source-declaration environment)
  (let ((value-code (jazz.emit-expression value source-declaration environment)))
    (receive (annotated-frame annotated-variable annotated-type) (jazz.find-annotated binding environment)
      (%%when (%%class-is? annotated-variable jazz.Annotated-Variable)
        (jazz.extend-annotated-type annotated-frame annotated-variable (%%get-code-type value-code))))
    (jazz.new-code
      `(set! ,(%%get-lexical-binding-name binding) ,(%%get-code-form value-code))
      jazz.Any)))


(jazz.encapsulate-class jazz.Variable)


;;;
;;;; NextMethod Variable
;;;


(jazz.NextMethod-Variable-implement)


(define (jazz.new-nextmethod-variable name type)
  (%%assertion (jazz.variable-name-valid? name) (jazz.error "Invalid variable name: {s}" name)
    (jazz.allocate-nextmethod-variable jazz.NextMethod-Variable name type 0)))


(jazz.define-method (jazz.emit-binding-reference (jazz.NextMethod-Variable binding) source-declaration environment)
  (let ((name (%%get-lexical-binding-name binding))
        (self (jazz.*self*)))
    (jazz.new-code
      (if self
          `(lambda rest (apply ,name ,(%%get-code-form self) rest))
        name)
      jazz.Any)))


(jazz.define-method (jazz.emit-binding-call (jazz.NextMethod-Variable binding) arguments source-declaration environment)
  (let ((name (%%get-lexical-binding-name binding))
        (type (%%get-lexical-binding-type binding))
        (self (jazz.*self*)))
    (if self
        (jazz.new-code
          `(,(%%get-lexical-binding-name binding)
            ,(%%get-code-form self)
            ,@(jazz.codes-forms arguments))
          (jazz.call-return-type type))
      (jazz.new-code
        `(,(%%get-lexical-binding-name binding)
          ,@(jazz.codes-forms arguments))
        (jazz.call-return-type type)))))


(jazz.encapsulate-class jazz.NextMethod-Variable)


;;;
;;;; Parameter
;;;


(jazz.Parameter-implement)


(define (jazz.new-parameter name type)
  (%%assertion (jazz.variable-name-valid? name) (jazz.error "Invalid variable name: {s}" name)
    (jazz.allocate-parameter jazz.Parameter name type 0)))


(jazz.define-virtual (jazz.emit-parameter (jazz.Parameter parameter) declaration environment))


(jazz.define-method (jazz.emit-parameter (jazz.Parameter parameter) declaration environment)
  (%%get-lexical-binding-name parameter))


(jazz.encapsulate-class jazz.Parameter)


;;;
;;;; Dynamic Parameter
;;;


(jazz.Dynamic-Parameter-implement)


(define (jazz.new-dynamic-parameter name type class)
  (jazz.allocate-dynamic-parameter jazz.Dynamic-Parameter name type 0 class))


(jazz.define-method (jazz.emit-parameter (jazz.Dynamic-Parameter parameter) declaration environment)
  (let ((class (%%get-dynamic-parameter-class parameter)))
    (%%list (%%get-code-form (jazz.emit-expression class declaration environment)) (%%get-lexical-binding-name parameter))))


(jazz.encapsulate-class jazz.Dynamic-Parameter)


;;;
;;;; Optional Parameter
;;;


(jazz.Optional-Parameter-implement)


(define (jazz.new-optional-parameter name type default)
  (jazz.allocate-optional-parameter jazz.Optional-Parameter name type 0 default))


(jazz.define-method (jazz.emit-parameter (jazz.Optional-Parameter parameter) declaration environment)
  (let ((default (%%get-optional-parameter-default parameter)))
    (%%list (%%get-lexical-binding-name parameter) (%%get-code-form (jazz.emit-expression default declaration environment)))))


(jazz.encapsulate-class jazz.Optional-Parameter)


;;;
;;;; Named Parameter
;;;


(jazz.Named-Parameter-implement)


(define (jazz.new-named-parameter name type default)
  (jazz.allocate-named-parameter jazz.Named-Parameter name type 0 default))


(jazz.define-method (jazz.emit-parameter (jazz.Named-Parameter parameter) declaration environment)
  (let ((default (%%get-named-parameter-default parameter)))
    (%%list (%%get-lexical-binding-name parameter) (%%get-code-form (jazz.emit-expression default declaration environment)))))


(jazz.encapsulate-class jazz.Named-Parameter)


;;;
;;;; Rest Parameter
;;;


(jazz.Rest-Parameter-implement)


(define (jazz.new-rest-parameter name type)
  (jazz.allocate-rest-parameter jazz.Rest-Parameter name type 0))


(jazz.define-method (jazz.emit-parameter (jazz.Rest-Parameter parameter) declaration environment)
  (%%get-lexical-binding-name parameter))


(jazz.encapsulate-class jazz.Rest-Parameter)


;;;
;;;; Self-Binding
;;;


;; Support for dialects that have an implicit self concept


(jazz.Self-Binding-implement)


(define (jazz.new-self-binding type)
  (jazz.allocate-self-binding jazz.Self-Binding 'self type))


(jazz.define-method (jazz.emit-binding-reference (jazz.Self-Binding declaration) source-declaration environment)
  (jazz.new-code
    'self
    (%%get-declaration-parent source-declaration)))


(jazz.encapsulate-class jazz.Self-Binding)


;;;
;;;; With-Self
;;;


(define jazz.*self*
  (make-parameter #f))


;;;
;;;; Macro Symbol
;;;


(jazz.Macro-Symbol-implement)


(define (jazz.new-macro-symbol name getter setter)
  (jazz.allocate-macro-symbol jazz.Macro-Symbol name #f getter setter))


#; ;; convert to walk / emit
(jazz.define-method (jazz.emit-binding-reference (jazz.Macro-Symbol binding) source-declaration environment)
  (let ((getter (%%get-macro-symbol-getter binding)))
    (jazz.walk walker resume source-declaration environment (getter))))


#; ;; convert to walk / emit
(jazz.define-method (jazz.walk-binding-assignable? (jazz.Macro-Symbol declaration))
  #t)


#; ;; convert to walk / emit
(jazz.define-method (jazz.emit-binding-assignment (jazz.Macro-Symbol binding) value source-declaration environment)
  (let ((setter (%%get-macro-symbol-setter binding)))
    (jazz.walk walker resume source-declaration environment (setter value))))


(jazz.encapsulate-class jazz.Macro-Symbol)


;;;
;;;; Form Binding
;;;


(jazz.Form-Binding-implement)


(jazz.encapsulate-class jazz.Form-Binding)


;;;
;;;; Special Form
;;;


(jazz.Special-Form-implement)


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


(jazz.Macro-Form-implement)


(define (jazz.new-macro-form name expander)
  (jazz.allocate-macro-form jazz.Macro-Form name #f expander))


(jazz.define-method (jazz.walk-binding-expandable? (jazz.Macro-Form binding))
  #t)


(jazz.define-method (jazz.walk-binding-expand-form (jazz.Macro-Form binding) walker resume declaration environment form)
  (let ((expander (%%get-macro-form-expander binding)))
    (apply expander walker resume declaration environment (%%cdr form))))


(jazz.encapsulate-class jazz.Macro-Form)


;;;
;;;; Annotated Variable
;;;


(jazz.Annotated-Variable-implement)


(define (jazz.new-annotated-variable variable declared-type type)
  (jazz.allocate-annotated-variable jazz.Annotated-Variable variable declared-type type))


(jazz.encapsulate-class jazz.Annotated-Variable)


;;;
;;;; Restricted Binding
;;;


(jazz.Restricted-Binding-implement)


(define (jazz.new-restricted-binding binding type)
  (jazz.allocate-restricted-binding jazz.Restricted-Binding binding type))


(jazz.encapsulate-class jazz.Restricted-Binding)


;;;
;;;; Annotated Frame
;;;


(jazz.Annotated-Frame-implement)


(define (jazz.new-annotated-frame variables reset)
  (jazz.allocate-annotated-frame jazz.Annotated-Frame variables reset))


(jazz.encapsulate-class jazz.Annotated-Frame)


;; put those in a cond-expand when cond-expand supports multiple features
(define (jazz.inspect-annotated-variable variable)
  (let ((serial (jazz.object->serial-symbol variable)))
    (if (%%class-is? variable jazz.Restricted-Binding)
        (list restricted: (%%get-lexical-binding-name (%%get-restricted-binding-binding variable)) (%%get-restricted-binding-type variable) serial)
      (list variable: (%%get-lexical-binding-name (%%get-annotated-variable-variable variable)) (%%get-annotated-variable-type variable) serial))))


(define (jazz.inspect-annotated-frame frame)
  (cons frame: (map jazz.inspect-annotated-variable (%%get-annotated-frame-variables frame))))


(define (jazz.inspect-annotated-environment environment)
  (cons environment: (map jazz.inspect-annotated-frame environment)))


;;;
;;;; Code
;;;


(jazz.Code-implement)


(define (jazz.new-code form type)
  (jazz.allocate-code jazz.Code form type))


(jazz.encapsulate-class jazz.Code)


(define (jazz.codes-forms codes)
  (map (lambda (code)
         (%%get-code-form code))
       codes))


(define (jazz.codes-types codes)
  (map (lambda (code)
         (%%get-code-type code))
       codes))


;;;
;;;; Annotation
;;;


(define (jazz.annotate-signature signature)
  (let ((positional (%%get-signature-positional signature))
        (optional (%%get-signature-optional signature))
        (named (%%get-signature-named signature))
        (rest (%%get-signature-rest signature))
        (queue (jazz.new-queue)))
    (define (annotate parameter)
      (let ((declared-type (%%get-lexical-binding-type parameter)))
        (let ((type (or declared-type jazz.Any)))
          (jazz.enqueue queue (jazz.new-annotated-variable parameter declared-type type)))))
    (for-each annotate positional)
    (for-each annotate optional)
    (for-each annotate named)
    (%%when rest
      (annotate rest))
    (jazz.queue-list queue)))


;; not 100% sure about this special case of using the argument type when the parameter is typeless
(define (jazz.annotate-inlined-signature signature arguments)
  (let ((positional (%%get-signature-positional signature))
        (queue (jazz.new-queue)))
    (for-each (lambda (parameter argument)
                (let ((declared-type (%%get-lexical-binding-type parameter)))
                  (let ((type (or declared-type (%%get-code-type argument))))
                    (jazz.enqueue queue (jazz.new-annotated-variable parameter declared-type type)))))
              positional
              arguments)
    (jazz.queue-list queue)))


(define (jazz.annotate-bindings bindings)
  (map (lambda (binding)
         (let ((variable (%%car binding))
               (value (%%cdr binding)))
           (let ((declared-type (%%get-lexical-binding-type variable)))
             (let ((type (or declared-type jazz.Void)))
               (jazz.new-annotated-variable variable declared-type type)))))
       bindings))


(define (jazz.annotate-receive parameters)
  (map (lambda (parameter)
         (let ((declared-type (%%get-lexical-binding-type parameter)))
           (let ((type (or declared-type jazz.Any)))
             (jazz.new-annotated-variable parameter declared-type type))))
       parameters))


(define (jazz.annotate-internal-defines internal-defines)
  (map (lambda (internal-define)
         (let ((variable (%%get-internal-define-variable internal-define)))
           (let ((declared-type (%%get-lexical-binding-type variable)))
             (let ((type (or declared-type jazz.Any)))
               (jazz.new-annotated-variable variable declared-type type)))))
       internal-defines))


(define (jazz.with-annotated-frame variables proc)
  (let ((reset #f))
    (call/cc
      (lambda (k)
        (set! reset k)))
    (proc (jazz.new-annotated-frame variables reset))))


(define (jazz.find-annotated variable environment)
  (let ((type #f))
    (let iter-frames ((frames environment))
      (if (%%null? frames)
          #f
        (let ((annotated-frame (%%car frames)))
          (or (let iter-variables ((variables (%%get-annotated-frame-variables annotated-frame)))
                (if (%%null? variables)
                    #f
                  (let ((annotated-variable (%%car variables)))
                    (if (%%class-is? annotated-variable jazz.Restricted-Binding)
                        (let ((binding (%%get-restricted-binding-binding annotated-variable)))
                          ;; this is really for slots so i need to think about this
                          (if (%%class-is? binding jazz.Declaration)
                              (values #f annotated-variable (%%get-restricted-binding-type annotated-variable))
                            (begin
                              ;; keep outermost type
                              (if (and (%%not type) (%%eq? binding variable))
                                  (set! type (%%get-restricted-binding-type annotated-variable)))
                              (iter-variables (%%cdr variables)))))
                      (if (%%eq? (%%get-annotated-variable-variable annotated-variable) variable)
                          (values annotated-frame annotated-variable (or type (%%get-annotated-variable-type annotated-variable)))
                        (iter-variables (%%cdr variables)))))))
              (iter-frames (%%cdr frames))))))))


(define (jazz.find-annotated-type binding environment)
  ;; big time kludge to test it out
  (if (%%class-is? binding jazz.Variable)
      (receive (frame variable type) (jazz.find-annotated binding environment)
        type)
    ;; here it is a slot declaration
    (let ((info (jazz.find-annotated binding environment)))
      (if info
          (receive (frame variable type) info
            type)
        (%%get-lexical-binding-type binding)))))


(define (jazz.extend-annotated-type frame variable new-type)
  (let ((declared-type (%%get-annotated-variable-declared-type variable))
        (actual-type (%%get-annotated-variable-type variable)))
    (if declared-type
        ;; here should validate that new-type is castable to declared-type and generate a cast
        #f
      ;; this should not happen as variables are always created initialized
      (if (%%eq? actual-type jazz.Void)
          (%%set-annotated-variable-type variable new-type)
        (%%when (%%not (%%subtype? new-type actual-type))
          ;; should probably just call jazz.extend-type but it currently
          ;; bugs recursively on unioning function types together...
          (let ((extended-type
                  (if (%%subtype? actual-type new-type)
                      new-type
                    ;; should find the most specific common supertype
                    jazz.Any)))
            (%%set-annotated-variable-type variable jazz.Any)
            ;;(jazz.debug 'reset (%%get-lexical-binding-name (%%get-annotated-variable-variable variable)) actual-type new-type extended-type)
            (let ((reset (%%get-annotated-frame-reset frame)))
              (reset #f))))))))


(define (jazz.extend-type type1 type2)
  (cond ((or (%%not type1) (%%not type2))
         jazz.Any)
        ((%%subtype? type1 type2)
         type2)
        ((%%subtype? type2 type1)
         type1)
        (else
         ;; should find the most specific common supertype
         jazz.Any)))


(define (jazz.type-union types)
  (jazz.new-union-type types))


(define (jazz.type-difference type1 type2)
  #f)


;;;
;;;; Walker
;;;


(jazz.Walker-implement)


;;;
;;;; Problems
;;;


(define (jazz.walk-warning walker declaration fmt-string . rest)
  (let ((location (jazz.walk-location walker declaration))
        (message (apply jazz.format fmt-string rest)))
    (jazz.walker-warning walker (jazz.new-walk-error location message))))


(define (jazz.walk-error walker resume declaration fmt-string . rest)
  (let ((location (jazz.walk-location walker declaration))
        (message (apply jazz.format fmt-string rest)))
    (jazz.walker-error walker resume (jazz.new-walk-error location message))))


(define (jazz.walk-unresolved walker resume declaration symbol)
  (let ((location (jazz.walk-location walker declaration)))
    (jazz.walker-error walker resume (jazz.new-unresolved-error location symbol))))


(define (jazz.walker-warning walker warning)
  (if jazz.warnings?
      (%%set-walker-warnings walker (%%append (%%get-walker-warnings walker) (%%list warning)))))


(define (jazz.walker-error walker resume error)
  (%%set-walker-errors walker (%%append (%%get-walker-errors walker) (%%list error)))
  (if (and resume jazz.delay-reporting?)
      (resume (jazz.unspecified))
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
    (if (%%neq? name (jazz.requested-module-name))
        (jazz.error "Module at {s} is defining {s}" (jazz.requested-module-name) name)
      (jazz.parse-module rest
        (lambda (requires body)
          (jazz.new-module-declaration name #f requires))))))


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
  (%%assertion (%%pair? specification) (jazz.error "Ill-formed library invoice: {s}" specification)
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
    (if (%%neq? name (jazz.requested-module-name))
        (jazz.error "Library at {s} is defining {s}" (jazz.requested-module-name) name)
      (let ((walker (jazz.dialect-walker (jazz.load-dialect dialect-name))))
        (jazz.walk-library-declaration walker #f name dialect-name requires exports imports body)))))


(define (jazz.walk-library-declaration walker actual name dialect-name requires exports imports body)
  (let ((exports (%%reverse (jazz.walk-library-exports walker exports)))
        (imports (%%reverse (jazz.walk-library-imports walker (jazz.add-dialect-import imports dialect-name)))))
    (let ((new-declaration (jazz.new-library-declaration name #f dialect-name requires exports imports)))
      (jazz.load-library-syntax new-declaration)
      (jazz.setup-library-lookups new-declaration)
      (let ((declaration (jazz.merge-declarations actual new-declaration)))
        (jazz.walk-declarations walker #f declaration (%%cons declaration (jazz.walker-environment walker)) body)
        (jazz.validate-walk-problems walker)
        declaration))))


(define (jazz.load-library-syntax declaration)
  (for-each (lambda (spec)
              (jazz.parse-require spec
                (lambda (module-name feature-requirement load phase)
                  (%%when (%%eq? phase 'syntax)
                    (jazz.load-module module-name)))))
            (%%get-library-declaration-requires declaration))
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
  (jazz.emit-declaration (jazz.walk-library partial-form) '()))


(define (jazz.walk-library partial-form)
  (receive (name dialect-name requires exports imports body) (jazz.parse-library partial-form)
    (if (%%neq? name (jazz.requested-module-name))
        (jazz.error "Library at {s} is defining {s}" (jazz.requested-module-name) name)
      (let* ((dialect (jazz.load-dialect dialect-name))
             (walker (jazz.dialect-walker dialect))
             (resume #f)
             (actual (jazz.get-catalog-entry name))
             (declaration (jazz.call-with-validate-circularity name
                            (lambda ()
                              (let ((declaration (jazz.walk-library-declaration walker actual name dialect-name requires exports imports body)))
                                (jazz.set-catalog-entry name declaration)
                                declaration))))
             (environment (%%cons declaration (jazz.walker-environment walker)))
             (body (jazz.walk-namespace walker resume declaration environment body)))
        (jazz.validate-walk-problems walker)
        (%%set-namespace-declaration-body declaration body)
        declaration))))


(define (jazz.cond-expand form)
  (if (and (%%pair? form)
           (%%eq? (%%car form) 'cond-expand))
      (let iter ((scan (%%cdr form)))
        (if (%%null? scan)
            (jazz.error "Unfulfilled cond-expand")
          (let ((clause (%%car scan)))
            (if (or (%%not (%%pair? clause))
                    (%%not (%%symbol? (%%car clause))))
                (jazz.error "Ill-formed cond-expand clause: {s}" clause)
              (let ((feature-requirement (%%car clause)))
                (if (or (jazz.feature-safisfied? feature-requirement)
                        (%%eq? feature-requirement 'else))
                    (%%cadr clause)
                  (iter (%%cdr scan))))))))
    form))


(define (jazz.walk-namespace walker resume declaration environment form-list)
  (let ((queue (jazz.new-queue)))
    (for-each (lambda (form)
                (call/cc
                  (lambda (resume)
                    (jazz.enqueue queue (jazz.walk walker resume declaration environment (jazz.cond-expand form))))))
              form-list)
    (jazz.queue-list queue)))


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
           `(define ,name ,(%%get-code-form (jazz.emit-expression value library-declaration '())))))
       (%%get-library-declaration-literals library-declaration)))


(define (jazz.expand-library-variables library-declaration)
  (map (lambda (variable)
         (let ((symbol (%%car variable))
               (value (%%cdr variable)))
           `(jazz.define-variable ,symbol ,value)))
       (jazz.queue-list (%%get-library-declaration-variables library-declaration))))


(define (jazz.expand-library-autoloads library-declaration environment)
  (map (lambda (autoload-declaration)
         (let ((referenced-declaration (jazz.resolve-declaration autoload-declaration)))
           (let ((locator (jazz.autoload-locator referenced-declaration)))
             `(define ,locator
                (let ((loaded? #f))
                  (lambda ()
                    (if (%%not loaded?)
                        (begin
                          (jazz.load-module ',(%%get-declaration-locator (%%get-declaration-toplevel referenced-declaration)))
                          (set! loaded? #t)))
                    ,(%%get-code-form (jazz.emit-binding-reference referenced-declaration library-declaration environment))))))))
       (%%get-library-declaration-autoloads library-declaration)))


;;;
;;;; Environment
;;;


(define (jazz.core-bindings)
  (%%list
    (jazz.new-special-form 'proclaim jazz.walk-proclaim)
    (jazz.new-special-form 'native   jazz.walk-native)
    (jazz.new-special-form 'macro    jazz.walk-macro)
    (jazz.new-special-form 'syntax   jazz.walk-syntax)))


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


(jazz.define-virtual (jazz.walk-declaration (jazz.Walker walker) resume declaration environment form))


(jazz.define-method (jazz.walk-declaration (jazz.Walker walker) resume declaration environment form)
  (if (%%pair? form)
      (let ((first (%%car form)))
        (case first
          ((native) (jazz.walk-native-declaration walker resume declaration environment form))
          ((macro)  (jazz.walk-macro-declaration  walker resume declaration environment form))
          ((syntax) (jazz.walk-syntax-declaration walker resume declaration environment form))
          (else     #f)))
    #f))


(define (jazz.walk-declarations walker resume declaration environment forms)
  (define (walk forms)
    (for-each (lambda (form)
                (call/cc
                  (lambda (resume)
                    (let ((expansion (jazz.expand-macros walker resume declaration environment (jazz.cond-expand form))))
                      (if (jazz.begin-form? expansion)
                          (walk (%%cdr expansion))
                        (jazz.walk-declaration walker resume declaration environment expansion))))))
              forms))
  
  (walk forms))


(define (jazz.add-declaration-child walker resume namespace-declaration child)
  (let ((name (%%get-lexical-binding-name child)))
    (let ((actual (%%table-ref (%%get-namespace-declaration-children-lookup namespace-declaration) name #f)))
      (if actual
          (jazz.merge-declarations actual child)
        (begin
          (%%set-namespace-declaration-children namespace-declaration (%%append (%%get-namespace-declaration-children namespace-declaration) (%%list child)))
          (%%table-set! (%%get-namespace-declaration-children-lookup namespace-declaration) name child)
          (%%table-set! (%%get-access-lookup namespace-declaration jazz.private-access) name child)
          (%%table-set! (%%get-access-lookup namespace-declaration jazz.public-access) name child)
          child)))))


(define (jazz.merge-declarations actual-declaration new-declaration)
  (if (not actual-declaration)
      new-declaration
    (begin
      (jazz.update-declaration actual-declaration new-declaration)
      actual-declaration)))


(define (jazz.find-form-declaration namespace-declaration name)
  (let ((declaration (jazz.find-declaration namespace-declaration name)))
    (%%assertion declaration (jazz.error "Unable to find declaration: {a}" name)
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
;;;; Expression
;;;


(jazz.Expression-implement)


(jazz.define-virtual (jazz.emit-expression (jazz.Expression expression) declaration environment))


(jazz.define-method (jazz.emit-expression (jazz.Expression expression) declaration environment)
  (jazz.error "Unable to emit code for: {s}" expression))


(jazz.define-virtual (jazz.emit-call (jazz.Expression expression) arguments declaration environment))


(jazz.define-method (jazz.emit-call (jazz.Expression expression) arguments declaration environment)
  (jazz.new-code
    `(,(%%get-code-form (jazz.emit-expression expression declaration environment)) ,@(jazz.codes-forms arguments))
    jazz.Any))


(jazz.define-virtual (jazz.fold-expression (jazz.Expression expression) f k s))


(jazz.define-method (jazz.fold-expression (jazz.Expression expression) f k s)
  (f expression s))


(define (jazz.emit-expressions expressions declaration environment)
  (map (lambda (expression)
         (jazz.emit-expression expression declaration environment))
       expressions))


(define (jazz.fold-expressions expressions f k s seed)
  (if (%%null? expressions)
      seed
    (k (jazz.fold-expression (%%car expressions) f k s)
       (jazz.fold-expressions (%%cdr expressions) f k s seed))))


(jazz.encapsulate-class jazz.Expression)


;;;
;;;; Proclaim
;;;


(jazz.Proclaim-implement)


(define (jazz.new-proclaim optimize?)
  (jazz.allocate-proclaim jazz.Proclaim #f optimize?))


(jazz.define-method (jazz.emit-expression (jazz.Proclaim expression) declaration environment)
  (let ((library-declaration (%%get-declaration-toplevel declaration)))
    (%%set-library-declaration-declares library-declaration (%%get-proclaim-optimize? expression)))
  #f)


(jazz.encapsulate-class jazz.Proclaim)


;;;
;;;; Constant
;;;


(jazz.Constant-implement)


(define (jazz.new-constant expansion type)
  (jazz.allocate-constant jazz.Constant type expansion))


(jazz.define-method (jazz.emit-expression (jazz.Constant expression) declaration environment)
  (jazz.new-code
    (%%get-constant-expansion expression)
    (%%get-expression-type expression)))


(jazz.define-method (jazz.fold-expression (jazz.Constant expression) f k s)
  (f expression
     (k (%%get-constant-expansion expression)
        s)))


(jazz.encapsulate-class jazz.Constant)


;;;
;;;; Delay
;;;


(jazz.Delay-implement)


(define (jazz.new-delay expression)
  (jazz.allocate-delay jazz.Delay #f expression))


(jazz.define-method (jazz.emit-expression (jazz.Delay expression) declaration environment)
  (let ((expression (%%get-delay-expression expression)))
    (jazz.new-code
      `(delay ,(%%get-code-form (jazz.emit-expression expression declaration environment)))
      jazz.Any)))


(jazz.encapsulate-class jazz.Delay)


;;;
;;;; Quasiquote
;;;


(jazz.Quasiquote-implement)


(define (jazz.new-quasiquote form)
  (jazz.allocate-quasiquote jazz.Quasiquote #f form))


(jazz.define-method (jazz.emit-expression (jazz.Quasiquote expression) declaration environment)
  (letrec ((emit
            (lambda (form)
              (if (%%pair? form)
                  (if (or (%%eq? (%%car form) 'unquote)
                          (%%eq? (%%car form) 'unquote-splicing))
                      (%%list (%%car form) (%%get-code-form (jazz.emit-expression (%%cadr form) declaration environment)))
                    (%%cons (emit (%%car form)) (emit (%%cdr form))))
                form))))
    (jazz.new-code
      (%%list 'quasiquote (emit (%%get-quasiquote-form expression)))
      jazz.List)))


(jazz.encapsulate-class jazz.Quasiquote)


;;;
;;;; Reference
;;;


(jazz.Reference-implement)


(define (jazz.new-reference binding)
  (jazz.allocate-reference jazz.Reference #f binding))


(jazz.define-method (jazz.emit-expression (jazz.Reference expression) declaration environment)
  (jazz.emit-binding-reference (%%get-reference-binding expression) declaration environment))


(jazz.define-method (jazz.emit-call (jazz.Reference expression) arguments declaration environment)
  (jazz.emit-binding-call (%%get-reference-binding expression) arguments declaration environment))


(jazz.encapsulate-class jazz.Reference)


;;;
;;;; Assignment
;;;


(jazz.Assignment-implement)


(define (jazz.new-assignment binding value)
  (jazz.allocate-assignment jazz.Assignment #f binding value))


(jazz.define-method (jazz.emit-expression (jazz.Assignment expression) declaration environment)
  (jazz.emit-binding-assignment (%%get-assignment-binding expression) (%%get-assignment-value expression) declaration environment))


(jazz.define-method (jazz.fold-expression (jazz.Assignment expression) f k s)
  (f expression
     (k (jazz.fold-expression (%%get-assignment-value expression) f k s)
        s)))


(jazz.encapsulate-class jazz.Assignment)


;;;
;;;; Lambda
;;;


(jazz.Lambda-implement)


(define (jazz.new-lambda type signature body)
  (jazz.allocate-lambda jazz.Lambda type signature body))


(jazz.define-method (jazz.emit-expression (jazz.Lambda expression) declaration environment)
  (let ((type (%%get-expression-type expression))
        (signature (%%get-lambda-signature expression))
        (body (%%get-lambda-body expression)))
    (jazz.with-annotated-frame (jazz.annotate-signature signature)
      (lambda (frame)
        (let ((augmented-environment (cons frame environment)))
          (let ((signature-output (jazz.emit-signature signature declaration augmented-environment)))
            (let ((body-code (jazz.emit-expression body declaration augmented-environment)))
              (jazz.new-code
                `(lambda ,signature-output
                   ,@(jazz.emit-signature-casts signature declaration augmented-environment)
                   ,(jazz.simplify-begin (jazz.emit-type-cast (jazz.new-code `(begin ,@(%%get-code-form body-code)) (%%get-code-type body-code)) type declaration environment)))
                (jazz.new-function-type '() '() '() #f (%%get-code-type body-code))))))))))


(jazz.define-method (jazz.fold-expression (jazz.Lambda expression) f k s)
  (f expression
     (k (%%get-lambda-signature expression)
        (k (jazz.fold-statement (%%get-lambda-body expression) f k s)
           s))))


(jazz.encapsulate-class jazz.Lambda)


;;;
;;;; Let
;;;


(jazz.Let-implement)


(define (jazz.new-let bindings body)
  (jazz.allocate-let jazz.Let #f bindings body))


(jazz.define-method (jazz.emit-expression (jazz.Let expression) declaration environment)
  (let ((bindings (%%get-let-bindings expression))
        (body (%%get-let-body expression)))
    (jazz.with-annotated-frame (jazz.annotate-bindings bindings)
      (lambda (frame)
        (let ((variables (%%get-annotated-frame-variables frame))
              (augmented-environment (cons frame environment)))
          (let ((bindings-output
                  (map (lambda (binding annotated-variable)
                         (let ((variable (%%car binding))
                               (value (%%cdr binding)))
                           (let ((value-code (jazz.emit-expression value declaration augmented-environment)))
                             (jazz.extend-annotated-type frame annotated-variable (%%get-code-type value-code))
                             `(,(%%get-lexical-binding-name variable) ,(jazz.emit-type-cast value-code (%%get-lexical-binding-type variable) declaration environment)))))
                       bindings
                       variables)))
            (let ((body-code (jazz.emit-expression body declaration augmented-environment)))
              (jazz.new-code
                `(let ,bindings-output
                   ,@(%%get-code-form body-code))
                (%%get-code-type body-code)))))))))


(jazz.define-method (jazz.fold-expression (jazz.Let expression) f k s)
  (f expression
     (k (jazz.fold-expression (%%get-let-body expression) f k s)
        s)))


(jazz.encapsulate-class jazz.Let)


;;;
;;;; Named Let
;;;


(jazz.Named-Let-implement)


(define (jazz.new-named-let variable bindings body)
  (jazz.allocate-named-let jazz.Named-Let #f bindings body variable))


(jazz.define-method (jazz.emit-expression (jazz.Named-Let expression) declaration environment)
  (let ((variable (%%get-named-let-variable expression))
        (bindings (%%get-let-bindings expression))
        (body (%%get-let-body expression)))
    (jazz.with-annotated-frame (cons (jazz.new-annotated-variable variable jazz.Any jazz.Any) (jazz.annotate-bindings bindings))
      (lambda (frame)
        (let ((variables (%%get-annotated-frame-variables frame))
              (augmented-environment (cons frame environment)))
          (let ((bindings-output
                  (map (lambda (binding annotated-variable)
                         (let ((variable (%%car binding))
                               (value (%%cdr binding)))
                           (let ((value-code (jazz.emit-expression value declaration augmented-environment)))
                             (jazz.extend-annotated-type frame annotated-variable (%%get-code-type value-code))
                             `(,(%%get-lexical-binding-name variable) ,(jazz.emit-type-cast value-code (%%get-lexical-binding-type variable) declaration environment)))))
                       bindings
                       (%%cdr variables))))
            (let ((body-code (jazz.emit-expression body declaration augmented-environment)))
              (jazz.new-code
                `(let ,(%%get-lexical-binding-name variable) ,bindings-output
                   ,@(%%get-code-form body-code))
                (%%get-code-type body-code)))))))))


(jazz.define-method (jazz.fold-expression (jazz.Named-Let expression) f k s)
  (f expression
     (k (jazz.fold-expression (%%get-let-body expression) f k s)
        s)))


(jazz.encapsulate-class jazz.Named-Let)


;;;
;;;; Letstar
;;;


(jazz.Letstar-implement)


(define (jazz.new-letstar bindings body)
  (jazz.allocate-letstar jazz.Letstar #f bindings body))


(jazz.define-method (jazz.emit-expression (jazz.Letstar expression) declaration environment)
  (let ((bindings (%%get-letstar-bindings expression))
        (body (%%get-letstar-body expression)))
    (jazz.with-annotated-frame (jazz.annotate-bindings bindings)
      (lambda (frame)
        (let ((variables (%%get-annotated-frame-variables frame))
              (augmented-environment (cons frame environment)))
          (let ((bindings-output
                  (map (lambda (binding annotated-variable)
                         (let ((variable (%%car binding))
                               (value (%%cdr binding)))
                           (let ((value-code (jazz.emit-expression value declaration augmented-environment)))
                             (jazz.extend-annotated-type frame annotated-variable (%%get-code-type value-code))
                             `(,(%%get-lexical-binding-name variable) ,(jazz.emit-type-cast value-code (%%get-lexical-binding-type variable) declaration environment)))))
                       bindings
                       variables)))
            (let ((body-code (jazz.emit-expression body declaration augmented-environment)))
              (jazz.new-code
                `(let* ,bindings-output
                   ,@(%%get-code-form body-code))
                (%%get-code-type body-code)))))))))


(jazz.define-method (jazz.fold-expression (jazz.Letstar expression) f k s)
  (f expression
     (k (jazz.fold-expression (%%get-letstar-body expression) f k s)
        s)))


(jazz.encapsulate-class jazz.Letstar)


;;;
;;;; Letrec
;;;


(jazz.Letrec-implement)


(define (jazz.new-letrec bindings body)
  (jazz.allocate-letrec jazz.Letrec #f bindings body))


(jazz.define-method (jazz.emit-expression (jazz.Letrec expression) declaration environment)
  (let ((bindings (%%get-letrec-bindings expression))
        (body (%%get-letrec-body expression)))
    (jazz.with-annotated-frame (jazz.annotate-bindings bindings)
      (lambda (frame)
        (let ((variables (%%get-annotated-frame-variables frame))
              (augmented-environment (cons frame environment)))
          (let ((bindings-output
                  (map (lambda (binding annotated-variable)
                         (let ((variable (%%car binding))
                               (value (%%cdr binding)))
                           (let ((value-code (jazz.emit-expression value declaration augmented-environment)))
                             (jazz.extend-annotated-type frame annotated-variable (%%get-code-type value-code))
                             `(,(%%get-lexical-binding-name variable) ,(jazz.emit-type-cast value-code (%%get-lexical-binding-type variable) declaration environment)))))
                       bindings
                       variables)))
            (let ((body-code (jazz.emit-expression body declaration augmented-environment)))
              (jazz.new-code
                `(letrec ,bindings-output
                   ,@(%%get-code-form body-code))
                (%%get-code-type body-code)))))))))


(jazz.define-method (jazz.fold-expression (jazz.Letrec expression) f k s)
  (f expression
     (k (jazz.fold-expression (%%get-letrec-body expression) f k s)
        s)))


(jazz.encapsulate-class jazz.Letrec)


;;;
;;;; Receive
;;;


(jazz.Receive-implement)


(define (jazz.new-receive variables expression body)
  (jazz.allocate-receive jazz.Receive #f variables expression body))


(jazz.define-method (jazz.emit-expression (jazz.Receive expression) declaration environment)
  (let ((variables (%%get-receive-variables expression))
        (expression (%%get-receive-expression expression))
        (body (%%get-receive-body expression)))
    (jazz.with-annotated-frame (jazz.annotate-receive variables)
      (lambda (frame)
        (let ((augmented-environment (cons frame environment)))
          (let ((expression-output (%%get-code-form (jazz.emit-expression expression declaration environment))))
            (let ((body-code (jazz.emit-expression body declaration augmented-environment)))
              (jazz.new-code
                `(receive ,(map (lambda (variable)
                                  (%%get-lexical-binding-name variable))
                                variables)
                   ,expression-output
                   ,@(%%get-code-form body-code))
                (%%get-code-type body-code)))))))))


(jazz.define-method (jazz.fold-expression (jazz.Receive expression) f k s)
  (f expression
     (k (jazz.fold-expression (%%get-receive-body expression) f k s)
        s)))


(jazz.encapsulate-class jazz.Receive)


;;;
;;;; Body
;;;


(jazz.Body-implement)


(define (jazz.new-body internal-defines expressions)
  (jazz.allocate-body jazz.Body #f internal-defines expressions))


(jazz.define-method (jazz.emit-expression (jazz.Body expression) declaration environment)
  (let ((internal-defines (%%get-body-internal-defines expression))
        (expressions (%%get-body-expressions expression)))
    (jazz.with-annotated-frame (jazz.annotate-internal-defines internal-defines)
      (lambda (frame)
        (let ((augmented-environment (cons frame environment)))
          (jazz.new-code
            (append (jazz.codes-forms (jazz.emit-expressions internal-defines declaration augmented-environment))
                    (jazz.codes-forms (jazz.emit-expressions expressions declaration augmented-environment)))
            jazz.Any))))))


(jazz.define-method (jazz.fold-expression (jazz.Body expression) f k s)
  (f expression
     (jazz.fold-statements (%%get-body-internal-defines expression) f k s
       (jazz.fold-statements (%%get-body-expressions expression) f k s s))))


(jazz.encapsulate-class jazz.Body)


;;;
;;;; Internal-Define
;;;


(jazz.Internal-Define-implement)


(define (jazz.new-internal-define variable value)
  (jazz.allocate-internal-define jazz.Internal-Define #f variable value))


(jazz.define-method (jazz.emit-expression (jazz.Internal-Define expression) declaration environment)
  (let ((variable (%%get-internal-define-variable expression))
        (value (%%get-internal-define-value expression)))
    (jazz.new-code
      `(define ,(%%get-lexical-binding-name variable)
         ,(%%get-code-form (jazz.emit-expression value declaration environment)))
      jazz.Any)))


(jazz.define-method (jazz.fold-expression (jazz.Internal-Define expression) f k s)
  (f expression
     (k (jazz.fold-statement (%%get-internal-define-value expression) f k s)
        s)))


(jazz.encapsulate-class jazz.Internal-Define)


;;;
;;;; Begin
;;;


(jazz.Begin-implement)


(define (jazz.new-begin expressions)
  (jazz.allocate-begin jazz.Begin #f expressions))


(jazz.define-method (jazz.emit-expression (jazz.Begin expression) declaration environment)
  (let ((expressions (%%get-begin-expressions expression)))
    (let ((code (jazz.emit-statements-code expressions declaration environment)))
      (jazz.new-code
        `(begin ,@(%%get-code-form code))
        (%%get-code-type code)))))


(jazz.define-method (jazz.fold-expression (jazz.Begin expression) f k s)
  (f expression
     (jazz.fold-expressions (%%get-begin-expressions expression) f k s s)))


(jazz.encapsulate-class jazz.Begin)


;;;
;;;; Do
;;;


(jazz.Do-implement)


(define (jazz.new-do bindings test result body)
  (jazz.allocate-do jazz.Do #f bindings test result body))


(jazz.define-method (jazz.emit-expression (jazz.Do expression) declaration environment)
  (let ((bindings (%%get-do-bindings expression))
        (test (%%get-do-test expression))
        (result (%%get-do-result expression))
        (body (%%get-do-body expression)))
    (jazz.with-annotated-frame (jazz.annotate-bindings bindings)
      (lambda (frame)
        (let ((variables (%%get-annotated-frame-variables frame))
              (augmented-environment (cons frame environment)))
          (let ((bindings-output
                  (map (lambda (binding annotated-variable)
                         (let ((variable (%%car binding))
                               (init (%%cadr binding))
                               (step (%%cddr binding)))
                           (let ((init-code (%%get-code-form (jazz.emit-expression init declaration augmented-environment)))
                                 (step-code-list (if step (list (%%get-code-form (jazz.emit-expression step declaration augmented-environment))) '())))
                             `(,(%%get-lexical-binding-name variable)
                               ,init-code
                               ,@step-code-list))))
                       bindings
                       variables)))
            (let ((test-code (jazz.emit-expression test declaration augmented-environment))
                  (result-code (jazz.emit-expression result declaration augmented-environment))
                  (body-code (jazz.emit-expression body declaration augmented-environment)))
              (jazz.new-code
                `(do ,bindings-output
                     (,(%%get-code-form test-code) ,@(%%get-code-form result-code))
                   ,@(%%get-code-form body-code))
                (%%get-code-type result-code)))))))))


(jazz.encapsulate-class jazz.Do)


;;;
;;;; Call
;;;


(jazz.Call-implement)


(define (jazz.new-call operator arguments)
  (jazz.allocate-call jazz.Call #f operator arguments))


(jazz.define-method (jazz.emit-expression (jazz.Call expression) declaration environment)
  (let ((operator (%%get-call-operator expression))
        (arguments (%%get-call-arguments expression)))
    (let ((locator (if (%%class-is? operator jazz.Reference)
                       (let ((binding (%%get-reference-binding operator)))
                         (if (%%class-is? binding jazz.Declaration)
                             (%%get-declaration-locator binding)
                           #f))
                     #f))
          (arguments-codes (jazz.emit-expressions arguments declaration environment)))
      (or (jazz.emit-specialized-call operator locator arguments arguments-codes declaration environment)
          (jazz.emit-primitive-new-call operator locator arguments arguments-codes declaration environment)
          (jazz.emit-primitive-call operator locator arguments arguments-codes declaration environment)
          (jazz.emit-inlined-call operator arguments-codes declaration environment)
          (jazz.emit-call operator arguments-codes declaration environment)))))


(jazz.define-method (jazz.fold-expression (jazz.Call expression) f k s)
  (f expression
     (k (jazz.fold-expression (%%get-call-operator expression) f k s)
        (jazz.fold-expressions (%%get-call-arguments expression) f k s s))))


(jazz.encapsulate-class jazz.Call)


(define (jazz.call-return-type operator-type)
  (if (%%is? operator-type jazz.Function-Type)
      (%%get-function-type-result operator-type)
    jazz.Any))


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


(define (jazz.emit-specialized-call operator locator arguments arguments-codes declaration environment)
  (if (%%not locator)
      #f
    (or (jazz.emit-specialized-locator locator arguments-codes environment)
        (if (%%class-is? operator jazz.Reference)
            (let ((binding (%%get-reference-binding operator)))
              (let ((specializers (jazz.get-specializers binding)))
                (let ((types (jazz.codes-types arguments-codes)))
                  (let iter ((scan specializers))
                    (if (%%null? scan)
                        (begin
                          (%%when (and jazz.warnings? (%%not (%%null? specializers)) (%%get-library-declaration-declares (%%get-declaration-toplevel declaration))
                                       ;; quicky to suppress duplicate warnings as for the moment those are both primitive and specialize
                                       (%%not (%%memq locator '(scheme.dialect.kernel.=
                                                                scheme.dialect.kernel.<
                                                                scheme.dialect.kernel.<=
                                                                scheme.dialect.kernel.>
                                                                scheme.dialect.kernel.>=
                                                                scheme.dialect.kernel.+
                                                                scheme.dialect.kernel.-
                                                                scheme.dialect.kernel.*
                                                                scheme.dialect.kernel./))))
                            (jazz.debug 'Warning: 'In (%%get-declaration-locator declaration) 'unable 'to 'match 'call 'to 'specialized (%%get-lexical-binding-name binding)))
                          ;; for debugging
                          (%%when (%%memq (%%get-lexical-binding-name binding) jazz.debug-specializers)
                            (jazz.debug 'Warning: 'In (%%get-declaration-locator declaration) 'unable 'to 'match 'call 'to 'specialized (%%get-lexical-binding-name binding) 'on types))
                          #f)
                      (let ((specializer (%%car scan)))
                        (let ((function-type (%%get-lexical-binding-type specializer)))
                          (if (jazz.match-signature? arguments types function-type)
                              (or (jazz.emit-inlined-binding-call specializer arguments-codes declaration environment)
                                  (jazz.new-code
                                    (let ((locator (%%get-declaration-locator specializer)))
                                      `(,locator ,@(jazz.codes-forms arguments-codes)))
                                    (%%get-function-type-result function-type)))
                            (iter (%%cdr scan))))))))))
          #f))))


;; quicky because classes are not yet defined at this point
(define jazz.emit-specialized-locator #f)

(set! jazz.emit-specialized-locator
      (lambda (locator arguments-codes environment)
        #f))


;;;
;;;; Primitive New
;;;


;; quicky because classes are not yet defined at this point
(define jazz.emit-primitive-new-call #f)

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
                             (list name (jazz.walk-specifier #f #f #f '() specifier))))
                         patterns))))
              jazz.primitive-patterns)
    (set! jazz.primitive-patterns table)))


(define (jazz.add-primitive-patterns operator patterns)
  (set! jazz.primitive-patterns (%%cons (%%cons operator patterns) jazz.primitive-patterns)))


(define (jazz.get-primitive-patterns locator)
  (%%table-ref jazz.primitive-patterns locator '()))


(jazz.add-primitive-patterns 'scheme.dialect.kernel.=            '((##fx=  <fx*:bool>)  (##fl=  <fl*:bool>)  (##= <number^number:bool>)))
(jazz.add-primitive-patterns 'scheme.dialect.kernel.<            '((##fx<  <fx*:bool>)  (##fl<  <fl*:bool>)))
(jazz.add-primitive-patterns 'scheme.dialect.kernel.<=           '((##fx<= <fx*:bool>)  (##fl<= <fl*:bool>)))
(jazz.add-primitive-patterns 'scheme.dialect.kernel.>            '((##fx>  <fx*:bool>)  (##fl>  <fl*:bool>)))
(jazz.add-primitive-patterns 'scheme.dialect.kernel.>=           '((##fx>= <fx*:bool>)  (##fl>= <fl*:bool>)))

(jazz.add-primitive-patterns 'scheme.dialect.kernel.+            '((##fx+  <fx*:fx>)    (##fl+  <fl*:fl>)    (##+ <int^int:int>) (##+ <number^number:number>)))
(jazz.add-primitive-patterns 'scheme.dialect.kernel.-            '((##fx-  <fx^fx*:fx>) (##fl-  <fl^fl*:fl>) (##- <int^int:int>) (##- <number^number:number>)))
(jazz.add-primitive-patterns 'scheme.dialect.kernel.*            '((##fx*  <fx*:fx>)    (##fl*  <fl*:fl>)    (##* <int^int:int>) (##* <number^number:number>)))
(jazz.add-primitive-patterns 'scheme.dialect.kernel./            '(                     (##fl/  <fl^fl*:fl>)                     (##/ <number^number:number>)))
(jazz.add-primitive-patterns 'scheme.dialect.kernel.quotient     '((##fxquotient <fx^fx:fx>)))

(jazz.add-primitive-patterns 'scheme.dialect.kernel.floor        '(                     (##flfloor    <fl:fl>)))
(jazz.add-primitive-patterns 'scheme.dialect.kernel.ceiling      '(                     (##flceiling  <fl:fl>)))
(jazz.add-primitive-patterns 'scheme.dialect.kernel.truncate     '(                     (##fltruncate <fl:fl>)))
(jazz.add-primitive-patterns 'scheme.dialect.kernel.round        '(                     (##flround    <fl:fl>)))

(jazz.add-primitive-patterns 'jazz.dialect.kernel.fx+            '((##fx+ <fx^fx:fx>)))
(jazz.add-primitive-patterns 'jazz.dialect.kernel.fx-            '((##fx- <fx^fx:fx>)))
(jazz.add-primitive-patterns 'jazz.dialect.kernel.fx*            '((##fx* <fx^fx:fx>)))

(jazz.add-primitive-patterns 'jazz.dialect.kernel.fl+            '(                     (##fl+ <fl^fl:fl>)))
(jazz.add-primitive-patterns 'jazz.dialect.kernel.fl-            '(                     (##fl- <fl^fl:fl>)))
(jazz.add-primitive-patterns 'jazz.dialect.kernel.fl*            '(                     (##fl* <fl^fl:fl>)))
(jazz.add-primitive-patterns 'jazz.dialect.kernel.fl/            '(                     (##fl/ <fl^fl:fl>)))

(jazz.add-primitive-patterns 'jazz.dialect.kernel.fixnum->flonum '((##fixnum->flonum <fx:fl>)))
(jazz.add-primitive-patterns 'jazz.dialect.kernel.flonum->fixnum '(                     (##flonum->fixnum <fl:fx>)))

(jazz.add-primitive-patterns 'scheme.dialect.kernel.not          '((##not  <any:bool>)))
(jazz.add-primitive-patterns 'scheme.dialect.kernel.eq?          '((##eq?  <any^any:bool>)))
(jazz.add-primitive-patterns 'scheme.dialect.kernel.eqv?         '((##eqv? <any^any:bool>)))

(jazz.add-primitive-patterns 'scheme.dialect.kernel.car          '((##car    <pair:any>)))
(jazz.add-primitive-patterns 'scheme.dialect.kernel.cdr          '((##cdr    <pair:any>)))
(jazz.add-primitive-patterns 'jazz.dialect.language.length       '((##length <list:int>)     (##vector-length <vector:int>)          (##string-length <string:int>)))
(jazz.add-primitive-patterns 'jazz.dialect.language.element      '((list-ref <list^int:any>) (##vector-ref    <vector^int:any>)      (##string-ref    <string^int:char>)))
(jazz.add-primitive-patterns 'jazz.dialect.language.set-element! '(                          (##vector-set!   <vector^int^any:void>) (##string-set!   <string^int^char:void>)))


(define (jazz.emit-primitive-call operator locator arguments arguments-codes declaration environment)
  (if (%%not locator)
      #f
    (let ((patterns (jazz.get-primitive-patterns locator)))
      (let ((types (jazz.codes-types arguments-codes)))
        (let iter ((scan patterns))
          (if (%%null? scan)
              (begin
                (%%when (and jazz.warnings? (%%not (%%null? patterns)) (%%get-library-declaration-declares (%%get-declaration-toplevel declaration))
                             ;; a bit extreme for now
                             (%%not (%%memq locator '(scheme.dialect.kernel.car
                                                      scheme.dialect.kernel.cdr))))
                  (jazz.debug 'Warning: 'In (%%get-declaration-locator declaration) 'unable 'to 'match 'call 'to 'primitive (jazz.identifier-name locator)))
                #f)
            (jazz.bind (name function-type) (%%car scan)
              (if (jazz.match-signature? arguments types function-type)
                  (jazz.new-code
                    `(,name ,@(jazz.codes-forms arguments-codes))
                    (%%get-function-type-result function-type))
                (iter (%%cdr scan))))))))))


;;;
;;;; Inlined Call
;;;


(define (jazz.emit-inlined-call operator arguments declaration environment)
  (if (%%class-is? operator jazz.Reference)
      (let ((binding (%%get-reference-binding operator)))
        (jazz.emit-inlined-binding-call binding arguments declaration environment))
    #f))


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
          (or (and (%%class-is? arg jazz.Reference)
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


(jazz.If-implement)


(define (jazz.new-if test yes no)
  (jazz.allocate-if jazz.If #f test yes no))


(define jazz.type-tests
  (%%make-table test: eq?))


(%%table-set! jazz.type-tests 'scheme.dialect.kernel.number?      jazz.Number)
(%%table-set! jazz.type-tests 'scheme.dialect.kernel.complex?     jazz.Complex)
(%%table-set! jazz.type-tests 'scheme.dialect.kernel.real?        jazz.Real)
(%%table-set! jazz.type-tests 'scheme.dialect.kernel.rational?    jazz.Rational)
(%%table-set! jazz.type-tests 'scheme.dialect.kernel.integer?     jazz.Integer)
(%%table-set! jazz.type-tests 'scheme.dialect.kernel.number?      jazz.Number)
(%%table-set! jazz.type-tests 'scheme.dialect.kernel.boolean?     jazz.Boolean)
;; not 100% correct because of Scheme's semantic for list?
(%%table-set! jazz.type-tests 'scheme.dialect.kernel.list?        jazz.List)
(%%table-set! jazz.type-tests 'scheme.dialect.kernel.null?        jazz.Null)
(%%table-set! jazz.type-tests 'scheme.dialect.kernel.pair?        jazz.Pair)
(%%table-set! jazz.type-tests 'scheme.dialect.kernel.symbol?      jazz.Symbol)
(%%table-set! jazz.type-tests 'scheme.dialect.kernel.char?        jazz.Char)
(%%table-set! jazz.type-tests 'scheme.dialect.kernel.string?      jazz.String)
(%%table-set! jazz.type-tests 'scheme.dialect.kernel.vector?      jazz.Vector)
(%%table-set! jazz.type-tests 'scheme.dialect.kernel.procedure?   jazz.Procedure)
(%%table-set! jazz.type-tests 'scheme.dialect.kernel.input-port?  jazz.Port)
(%%table-set! jazz.type-tests 'scheme.dialect.kernel.output-port? jazz.Port)
(%%table-set! jazz.type-tests 'jazz.dialect.kernel.fixnum?        jazz.Fixnum)
(%%table-set! jazz.type-tests 'jazz.dialect.kernel.flonum?        jazz.Flonum)
(%%table-set! jazz.type-tests 'jazz.dialect.kernel.keyword?       jazz.Keyword)
(%%table-set! jazz.type-tests 'jazz.dialect.kernel.object?        jazz.Object)
(%%table-set! jazz.type-tests 'jazz.dialect.kernel.category?      jazz.Category)
(%%table-set! jazz.type-tests 'jazz.dialect.kernel.class?         jazz.Class)
(%%table-set! jazz.type-tests 'jazz.dialect.kernel.interface?     jazz.Interface)
(%%table-set! jazz.type-tests 'jazz.dialect.kernel.field?         jazz.Field)
(%%table-set! jazz.type-tests 'jazz.dialect.kernel.slot?          jazz.Slot)
(%%table-set! jazz.type-tests 'jazz.dialect.kernel.method?        jazz.Method)
(%%table-set! jazz.type-tests 'jazz.dialect.kernel.table?         jazz.Table)
(%%table-set! jazz.type-tests 'jazz.dialect.kernel.foreign?       jazz.Foreign)


(define jazz.not-type-tests
  (%%make-table test: eq?))


(%%table-set! jazz.not-type-tests 'jazz.dialect.kernel.not-null? jazz.Null)


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
    env)
  
  (define (process-is expr type-expr env)
    (if (%%class-is? expr jazz.Reference)
        (let ((binding (%%get-reference-binding expr)))
          ;; all this needs bigtime cleanup
          (let ((extract-binding
                  (lambda ()
                    (cond ((%%class-is? binding jazz.Variable)
                           (receive (frame actual-variable actual-type) (jazz.find-annotated binding (%%car env))
                             (let ((origin (%%get-annotated-variable-variable actual-variable)))
                               (values origin actual-type))))
                          ;; this is really for slots so i need to think about this
                          ((%%class-is? binding jazz.Declaration)
                           (values binding (%%get-lexical-binding-type binding)))
                          (else
                           #f)))))
            (let ((info (extract-binding)))
              (if info
                  (receive (origin actual-type) info
                    (let ((yes-type (cond ((jazz.type? type-expr)
                                           type-expr)
                                          ((%%class-is? type-expr jazz.Reference)
                                           (let ((binding (%%get-reference-binding type-expr)))
                                             (if (%%class-is? binding jazz.Declaration)
                                                 (jazz.resolve-declaration binding)
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
                                    (cons (jazz.new-annotated-frame (list (jazz.new-restricted-binding origin yes-type)) #f) (%%car env))
                                  (%%car env)))
                              (no
                                (if no-type
                                    (cons (jazz.new-annotated-frame (list (jazz.new-restricted-binding origin no-type)) #f) (%%cdr env))
                                  (%%cdr env))))
                          (%%cons yes no)))))
                env))))
      env))
  
  (define (revenv env)
    (%%cons (%%cdr env) (%%car env)))
  
  (define (process-expr expr env)
    (cond ((%%class-is? expr jazz.And)
           (process-and (%%get-and-expressions expr) env))
          ((%%class-is? expr jazz.Or)
           (process-or (%%get-or-expressions expr) env))
          ((%%class-is? expr jazz.Lexical-Binding)
           )
          ((%%class-is? expr jazz.Call)
           (let ((operator (%%get-call-operator expr)))
             (if (%%class-is? operator jazz.Reference)
                 (let ((operator-binding (%%get-reference-binding operator)))
                   (if (%%class-is? operator-binding jazz.Declaration)
                       (let ((operator-locator (%%get-declaration-locator operator-binding))
                             (arguments (%%get-call-arguments expr)))
                         (let ((count (%%length arguments)))
                           (case operator-locator
                             ((scheme.dialect.kernel.not)
                              (if (%%fx= count 1)
                                  (process-not (%%car arguments) env)
                                env))
                             ((jazz.dialect.kernel.is?)
                              (if (%%fx= count 2)
                                  (process-is (%%car arguments) (%%cadr arguments) env)
                                env))
                             ((jazz.dialect.language.is-not?)
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
           env)))
  
  (process-expr test (cons environment environment)))


(jazz.define-method (jazz.emit-expression (jazz.If expression) declaration environment)
  (let ((test (%%get-if-test expression)))
    (jazz.bind (yes-environment . no-environment) (jazz.branch-types test environment)
      (let ((test (jazz.emit-expression test declaration environment))
            (yes (jazz.emit-expression (%%get-if-yes expression) declaration yes-environment))
            (no (jazz.emit-expression (%%get-if-no expression) declaration no-environment)))
        (jazz.new-code
          `(if ,(%%get-code-form test)
               ,(%%get-code-form yes)
             ,(jazz.simplify-begin (%%get-code-form no)))
          (jazz.extend-type (%%get-code-type yes) (%%get-code-type no)))))))


(jazz.define-method (jazz.fold-expression (jazz.If expression) f k s)
  (f expression
     (k (jazz.fold-expression (%%get-if-test expression) f k s)
        (k (jazz.fold-expression (%%get-if-yes expression) f k s)
           (jazz.fold-expressions (%%get-if-no expression) f k s s)))))


(jazz.encapsulate-class jazz.If)


;;;
;;;; Cond
;;;


(jazz.Cond-implement)


(define (jazz.new-cond clauses)
  (jazz.allocate-cond jazz.Cond #f clauses))


(define (k x)
  (cond ((not (list? x))
         #f)
        ((null? x)
         #f)
        (else
         (car x))))


(jazz.define-method (jazz.emit-expression (jazz.Cond expression) declaration environment)
  (jazz.new-code
    `(cond ,@(map (lambda (clause)
                    (let ((test (%%car clause))
                          (body (%%cdr clause)))
                      (jazz.bind (yes-environment . no-environment) (jazz.branch-types test environment)
                        (let ((output
                                `(,(if (%%not test)
                                       'else
                                     (%%get-code-form (jazz.emit-expression test declaration environment)))
                                  ,@(jazz.codes-forms (jazz.emit-expressions body declaration yes-environment)))))
                          (set! environment no-environment)
                          output))))
                  (%%get-cond-clauses expression)))
    jazz.Any))


(jazz.define-method (jazz.fold-expression (jazz.Cond expression) f k s)
  #f)


(jazz.encapsulate-class jazz.Cond)


;;;
;;;; Case
;;;


(jazz.Case-implement)


(define (jazz.new-case target clauses)
  (jazz.allocate-case jazz.Case #f target clauses))


(jazz.define-method (jazz.emit-expression (jazz.Case expression) declaration environment)
  (let ((target (%%get-case-target expression))
        (clauses (%%get-case-clauses expression)))
    (jazz.new-code
      `(case ,(%%get-code-form (jazz.emit-expression target declaration environment))
         ,@(map (lambda (clause)
                  (let* ((tries (%%car clause))
                         (body (%%cdr clause)))
                    `(,tries ,@(jazz.codes-forms (jazz.emit-expressions body declaration environment)))))
                clauses))
      jazz.Any)))


(jazz.define-method (jazz.fold-expression (jazz.Case expression) f k s)
  #f)


(jazz.encapsulate-class jazz.Case)


;;;
;;;; And
;;;


(jazz.And-implement)


(define (jazz.new-and expressions)
  (jazz.allocate-and jazz.And #f expressions))


(jazz.define-method (jazz.emit-expression (jazz.And expression) declaration environment)
  (jazz.new-code
    `(and ,@(jazz.codes-forms (jazz.emit-expressions (%%get-and-expressions expression) declaration environment)))
    jazz.Any))


(jazz.define-method (jazz.fold-expression (jazz.And expression) f k s)
  (f expression
     (jazz.fold-expressions (%%get-and-expressions expression) f k s s)))


(jazz.encapsulate-class jazz.And)


;;;
;;;; Or
;;;


(jazz.Or-implement)


(define (jazz.new-or expressions)
  (jazz.allocate-or jazz.Or #f expressions))


(jazz.define-method (jazz.emit-expression (jazz.Or expression) declaration environment)
  (jazz.new-code
    `(or ,@(jazz.codes-forms (jazz.emit-expressions (%%get-or-expressions expression) declaration environment)))
    jazz.Any))


(jazz.define-method (jazz.fold-expression (jazz.Or expression) f k s)
  (f expression
     (jazz.fold-expressions (%%get-or-expressions expression) f k s s)))


(jazz.encapsulate-class jazz.Or)


;;;
;;;; C Include
;;;


(jazz.C-Include-implement)


(define (jazz.new-c-include name)
  (jazz.allocate-c-include jazz.C-Include #f name))


(jazz.define-method (jazz.emit-expression (jazz.C-Include expression) declaration environment)
  (let ((name (%%get-c-include-name expression)))
    (jazz.new-code
      `(c-declare ,(%%string-append "#include " name))
      jazz.Any)))


(jazz.define-method (jazz.fold-expression (jazz.C-Include expression) f k s)
  #f)


(jazz.encapsulate-class jazz.C-Include)


;;;
;;;; C Declare
;;;


(jazz.C-Declare-implement)


(define (jazz.new-c-declare code)
  (jazz.allocate-c-declare jazz.C-Declare #f code))


(jazz.define-method (jazz.emit-expression (jazz.C-Declare expression) declaration environment)
  (let ((code (%%get-c-declare-code expression)))
    (jazz.new-code
      `(c-declare ,code)
      jazz.Any)))


(jazz.define-method (jazz.fold-expression (jazz.C-Declare expression) f k s)
  #f)


(jazz.encapsulate-class jazz.C-Declare)


;;;
;;;; C Initialize
;;;


(jazz.C-Initialize-implement)


(define (jazz.new-c-initialize code)
  (jazz.allocate-c-initialize jazz.C-Initialize #f code))


(jazz.define-method (jazz.emit-expression (jazz.C-Initialize expression) declaration environment)
  (let ((code (%%get-c-initialize-code expression)))
    (jazz.new-code
      `(c-initialize ,code)
      jazz.Any)))


(jazz.define-method (jazz.fold-expression (jazz.C-Initialize expression) f k s)
  #f)


(jazz.encapsulate-class jazz.C-Initialize)


;;;
;;;; C Function
;;;


(jazz.C-Function-implement)


(define (jazz.new-c-function expansion)
  (jazz.allocate-c-function jazz.C-Function #f expansion))


(jazz.define-method (jazz.emit-expression (jazz.C-Function expression) declaration environment)
  (jazz.new-code
    (%%get-c-function-expansion expression)
    jazz.Any))


(jazz.define-method (jazz.fold-expression (jazz.C-Function expression) f k s)
  #f)


(jazz.encapsulate-class jazz.C-Function)


;;;
;;;; Time
;;;


(jazz.Time-implement)


(define (jazz.new-time expression)
  (jazz.allocate-time jazz.Time #f expression))


(jazz.define-method (jazz.emit-expression (jazz.Time expression) declaration environment)
  (let ((expression (%%get-time-expression expression)))
    (jazz.new-code
      `(time
         ,(%%get-code-form (jazz.emit-expression expression declaration environment)))
      jazz.Any)))


(jazz.define-method (jazz.fold-expression (jazz.Time expression) f k s)
  #f)


(jazz.encapsulate-class jazz.Time)


;;;
;;;; Statement
;;;


(define (jazz.emit-namespace-statements statements declaration environment)
  (let ((queue (jazz.new-queue)))
    (for-each (lambda (statement)
                (if (%%class-is? statement jazz.Declaration)
                    (jazz.enqueue queue (jazz.emit-declaration statement environment))
                  (let ((code (jazz.emit-expression statement declaration environment)))
                    (if code
                        (jazz.enqueue queue (%%get-code-form code))))))
              statements)
    (jazz.queue-list queue)))


(define (jazz.emit-statements-code statements declaration environment)
  (let ((last-type #f))
    (let ((emited
            (map (lambda (statement)
                   (if (%%class-is? statement jazz.Declaration)
                       (jazz.emit-declaration statement environment)
                     (let ((code (jazz.emit-expression statement declaration environment)))
                       (set! last-type (%%get-code-type code))
                       (%%get-code-form code))))
                 statements)))
      (jazz.new-code emited last-type))))


(define (jazz.fold-statement statement f k s)
  (if (%%class-is? statement jazz.Declaration)
      (jazz.fold-declaration statement f k s)
    (jazz.fold-expression statement f k s)))


(define (jazz.fold-statements statements f k s seed)
  (if (%%null? statements)
      seed
    (k (jazz.fold-statement (%%car statements) f k s)
       (jazz.fold-statements (%%cdr statements) f k s seed))))


;;;
;;;; Debug
;;;


(define (ppl library-name)
  (pps (walk library-name)))


(define (pps statement)
  (define (present-declaration sta)
    (%%get-lexical-binding-name sta))
  
  (define (present-expression sta)
    (cond ((%%class-is? sta jazz.Constant)
           (%%get-constant-expansion sta))
          ((%%class-is? sta jazz.Reference)
           (%%get-lexical-binding-name (%%get-reference-binding sta)))
          (else
           (jazz.identifier-name (%%get-category-name (%%class-of sta))))))
  
  (pp
    (jazz.fold-statement (if (integer? statement) (jazz.serial-number->object statement) statement)
      (lambda (sta s)
        (let ((info (cond ((%%class-is? sta jazz.Declaration)
                           (present-declaration sta))
                          ((%%class-is? sta jazz.Expression)
                           (present-expression sta))
                          (else
                           sta))))
          (if (null? s)
              info
            (cons info s))))
      cons
      '())))


;;;
;;;; Walk
;;;


(define (jazz.walk walker resume declaration environment form)
  (cond ((%%symbol? form)
         (jazz.walk-symbol walker resume declaration environment form))
        ((%%pair? form)
         (jazz.walk-form walker resume declaration environment form))
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
              (let ((variables (jazz.new-queue))
                    (augmented-environment environment))
                (for-each (lambda (internal-define)
                            (let ((signature (%%cadr internal-define)))
                              (let ((name (if (%%symbol? signature)
                                              signature
                                            (%%car signature))))
                                (let ((variable (jazz.new-variable name #f)))
                                  (jazz.enqueue variables variable)
                                  (set! augmented-environment (%%cons variable augmented-environment))))))
                          internal-defines)
                (jazz.new-body (map (lambda (internal-define variable)
                                      (jazz.walk-internal-define walker resume declaration augmented-environment internal-define variable))
                                    internal-defines
                                    (jazz.queue-list variables))
                               (jazz.walk-list walker resume declaration augmented-environment scan))))
          (iter (%%cdr scan)))))))


(define (jazz.walk-internal-define walker resume declaration environment form variable)
  (receive (name specifier value parameters) (jazz.parse-define walker resume declaration (%%cdr form))
    (let ((type (if specifier (jazz.walk-specifier walker resume declaration environment specifier) jazz.Any)))
      (jazz.new-internal-define variable (jazz.walk walker resume declaration environment value)))))


(define (jazz.parse-define walker resume declaration rest)
  (if (%%symbol? (%%car rest))
      (let ((name (%%car rest)))
        (jazz.parse-specifier (%%cdr rest)
          (lambda (specifier rest)
            (values name specifier (%%car rest) #f))))
    (let ((name (%%caar rest))
          (parameters (%%cdar rest)))
      (jazz.parse-specifier (%%cdr rest)
        (lambda (specifier body)
          (let ((specifier-list (if specifier (%%list specifier) '())))
            (values name #f `(lambda ,parameters ,@specifier-list ,@body) parameters)))))))


;;;
;;;; Constant
;;;


(define (jazz.walk-quote walker resume declaration environment form)
  (let ((expression (%%cadr form)))
    (if (%%null? expression)
        (jazz.new-constant '(quote ()) jazz.Null)
      (jazz.walk-constant walker resume declaration environment expression))))


(define (jazz.walk-keyword walker keyword)
  (jazz.new-constant keyword jazz.Keyword))


(define (jazz.walk-enumerator walker enumerator)
  (jazz.new-constant (%%list 'quote enumerator) jazz.Symbol))


(define (jazz.walk-constant walker resume declaration environment form)
  (cond ((%%boolean? form)
         (jazz.new-constant form jazz.Boolean))
        ((%%char? form)
         (jazz.new-constant form jazz.Char))
        ((%%string? form)
         (jazz.new-constant form jazz.String))
        ((%%keyword? form)
         (jazz.new-constant form jazz.Keyword))
        ((%%fixnum? form)
         (jazz.new-constant form jazz.Fixnum))
        ((%%flonum? form)
         (jazz.new-constant form jazz.Flonum))
        ((%%number? form)
         (jazz.new-constant form jazz.Number))
        ((%%symbol? form)
         (jazz.new-constant `(quote ,form) jazz.Symbol))
        ((%%vector? form)
         (jazz.new-constant `(quote ,form) jazz.Vector))
        ((%%null? form)
         (jazz.new-constant `(quote ,form) jazz.Null))
        ((jazz.scheme-pair-literal? form)
         (jazz.new-constant `(quote ,form) jazz.Pair))
        (else
         (jazz.register-literal walker resume declaration environment form))))


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
  (%%make-table test: eq?))


(define (jazz.register-literal-constructor name constructor)
  (%%table-set! jazz.Literal-Constructors name constructor))


(define (jazz.require-literal-constructor name)
  (or (%%table-ref jazz.Literal-Constructors name #f)
      (jazz.error "Cannot construct literals of type {s}" name)))


(define (jazz.construct-literal lst)
  (if (%%null? lst)
      #f
    (let ((constructor (jazz.require-literal-constructor (%%car lst))))
      (%%apply constructor (%%cdr lst)))))


(define (jazz.register-literal walker resume declaration environment literal)
  ;; calling jazz.get-registered-literal to only register when not already there
  ;; doesnt work directly because some literals are interned and thus can be shared
  (let ((library-declaration (%%get-declaration-toplevel declaration)))
    (let ((name (jazz.generate-symbol (%%string-append (%%symbol->string (%%get-declaration-locator library-declaration)) ".lit"))))
      ;; it is important to register before any subliterals to ensure they come before us
      (let ((info (%%cons literal (%%cons name #f))))
        (%%set-library-declaration-literals library-declaration (%%cons info (%%get-library-declaration-literals library-declaration)))
        (%%set-cdr! (%%cdr info) (jazz.walk-literal walker resume declaration literal)))
      ;; this way of getting a reference to the literal's class is a big quicky mostly to test the concept
      (let ((class-name (jazz.identifier-name (%%get-category-name (%%class-of literal)))))
        (jazz.new-constant name (jazz.lookup-reference walker resume declaration environment class-name))))))


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
             ;; the rank of fold-literal is known to be 3 as it is the fourth method of Object
             ((%%class-dispatch literal 0 3) literal))))))


(define (jazz.make-symbolic-chars alist)
  (%%list->table
    (map (lambda (pair)
           (%%cons (%%car pair) (integer->char (%%cdr pair))))
         alist)
    eq?))


(define jazz.Symbolic-Chars
  (jazz.make-symbolic-chars
    '((zero              . #x00)
      (home              . #x01)
      (enter             . #x03)
      (end               . #x04)
      (info              . #x05)
      (backspace         . #x08)
      (tab               . #x09)
      (line-feed         . #x0A)
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


(define (jazz.symbolic-char name)
  (%%table-ref jazz.Symbolic-Chars name #f))


;;;
;;;; Variable
;;;


(define (jazz.register-variable declaration suffix value)
  (let ((library-declaration (%%get-declaration-toplevel declaration)))
    (let ((symbol (jazz.generate-symbol (%%string-append (%%symbol->string (%%get-declaration-locator library-declaration)) "." suffix))))
      (let ((variable (cons symbol value)))
        (jazz.enqueue (%%get-library-declaration-variables library-declaration) variable)
        variable))))


;;;
;;;; Symbol
;;;


(jazz.define-virtual (jazz.walk-symbol (jazz.Walker walker) resume declaration environment symbol))


(jazz.define-method (jazz.walk-symbol (jazz.Walker walker) resume declaration environment symbol)
  (cond ((jazz.enumerator? symbol)
         (jazz.walk-enumerator walker symbol))
        (else
         (jazz.walk-symbol-reference walker resume declaration environment symbol))))


(define (jazz.walk-setbang walker resume declaration environment form)
  (let ((form (%%cadr form))
        (value (%%car (%%cddr form))))
    (if (%%symbol? form)
        (jazz.walk-symbol-assignment walker resume declaration environment form value)
      (jazz.error "Illegal set! of {s}" form))))


(define (jazz.lookup-symbol walker environment symbol)
  (if (jazz.composite-name? symbol)
      (jazz.lookup-composite walker environment symbol)
    (jazz.find-in (lambda (binding)
                    (jazz.walk-binding-lookup binding symbol))
                  environment)))


(define (jazz.lookup-composite walker environment symbol)
  (receive (library-name name) (jazz.split-composite symbol)
    (let ((library-decl (jazz.locate-library-declaration library-name #f)))
      (if library-decl
          (jazz.lookup-subpath library-decl (%%list name))
        #f))))


(define (jazz.lookup-subpath declaration subpath)
  (if (%%null? subpath)
      declaration
    (let ((subdecl (jazz.lookup-declaration declaration (%%car subpath) #t)))
      (if subdecl
          (jazz.lookup-subpath subdecl (%%cdr subpath))
        #f))))


(define (jazz.lookup-accessible/compatible-symbol walker resume declaration environment symbol)
  (let ((referenced-declaration (jazz.lookup-symbol walker environment symbol)))
    (if (and referenced-declaration (%%class-is? referenced-declaration jazz.Declaration))
        (begin
          (jazz.validate-access walker resume declaration referenced-declaration)
          (jazz.validate-compatibility walker declaration referenced-declaration)))
    (if (%%class-is? referenced-declaration jazz.Autoload-Declaration)
        (let ((library (%%get-declaration-toplevel declaration)))
          (jazz.register-autoload-declaration library referenced-declaration)))
    referenced-declaration))


(jazz.define-virtual (jazz.validate-access (jazz.Walker walker) resume declaration referenced-declaration))


(jazz.define-method (jazz.validate-access (jazz.Walker walker) resume declaration referenced-declaration)
  (jazz.unspecified))


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
        (begin
          (if (%%class-is? binding jazz.Variable)
              (jazz.walk-binding-referenced binding))
          (jazz.new-reference binding))
      (jazz.walk-free-reference walker resume declaration symbol))))


(jazz.define-virtual (jazz.walk-free-reference (jazz.Walker walker) resume declaration symbol))


(jazz.define-method (jazz.walk-free-reference (jazz.Walker walker) resume declaration symbol)
  (jazz.walk-unresolved walker resume declaration symbol))


;;;
;;;; Assignment
;;;


(jazz.define-virtual (jazz.walk-symbol-assignment (jazz.Walker walker) resume declaration environment symbol value))


(jazz.define-method (jazz.walk-symbol-assignment (jazz.Walker walker) resume declaration environment symbol value)
  (let ((binding (jazz.lookup-accessible/compatible-symbol walker resume declaration environment symbol)))
    (if binding
        (begin
          (jazz.walk-binding-validate-assignment binding walker resume declaration)
          (jazz.new-assignment binding (jazz.walk walker resume declaration environment value)))
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
        (rest (%%get-signature-rest signature))
        (passed (%%length arguments)))
    (if (if rest (%%fx< passed mandatory) (%%not (%%fx= passed mandatory)))
        (let ((locator (%%get-declaration-locator declaration)))
          (jazz.walk-error walker resume source-declaration "Wrong number of arguments to {a} (passed {a} expected{a} {a})"
            locator passed (if rest " at least" "") mandatory)))))


;;;
;;;; Proclaim
;;;


;; quick first draft
(define (jazz.parse-proclaim walker resume declaration rest)
  (%%assert (and (%%pair? rest) (%%null? (%%cdr rest)))
    (let ((declare (%%car rest)))
      (cond ((%%equal? declare '(optimize))
             (values #t))
            ((%%equal? declare '(not optimize))
             (values #f))
            (else
             (jazz.walk-error "Ill-formed proclaim: {s}" rest))))))


(define (jazz.walk-proclaim walker resume declaration environment form)
  (receive (optimize) (jazz.parse-proclaim walker resume declaration (%%cdr form))
    (jazz.new-proclaim optimize)))


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
    (let ((name (%%car rest)))
      (jazz.parse-specifier (%%cdr rest)
        (lambda (specifier rest)
          (%%assert (%%null? rest)
            (values name specifier access compatibility)))))))


(define (jazz.walk-native-declaration walker resume declaration environment form)
  (receive (name specifier access compatibility) (jazz.parse-native walker resume declaration (%%cdr form))
    (receive (name symbol) (jazz.parse-exported-symbol declaration name)
      (let ((type (if specifier (jazz.walk-specifier walker resume declaration environment specifier) jazz.Any)))
        (let ((new-declaration (jazz.new-export-declaration name type access compatibility '() declaration symbol)))
          (let ((effective-declaration (jazz.add-declaration-child walker resume declaration new-declaration)))
            effective-declaration))))))


(define (jazz.walk-native walker resume declaration environment form)
  (receive (name specifier access compatibility) (jazz.parse-native walker resume declaration (%%cdr form))
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
           (type jazz.Any)
           (parameters (%%cdr signature)))
      (values name type access compatibility parameters body))))


(define (jazz.walk-macro-declaration walker resume declaration environment form)
  (receive (name type access compatibility parameters body) (jazz.parse-macro walker resume declaration (%%cdr form))
    (let ((signature (jazz.walk-parameters walker resume declaration environment parameters #f #f)))
      (let ((new-declaration (jazz.new-macro-declaration name type access compatibility '() declaration signature)))
        (let ((effective-declaration (jazz.add-declaration-child walker resume declaration new-declaration)))
          effective-declaration)))))


(define (jazz.walk-macro walker resume declaration environment form)
  (receive (name type access compatibility parameters body) (jazz.parse-macro walker resume declaration (%%cdr form))
    (let* ((new-declaration (jazz.find-form-declaration declaration name)))
      (receive (signature augmented-environment) (jazz.walk-parameters walker resume declaration environment parameters #f #t)
        (%%set-macro-declaration-signature new-declaration signature)
        (%%set-macro-declaration-body new-declaration
          (jazz.walk-body walker resume new-declaration augmented-environment body))
        new-declaration))))


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
           (type jazz.Any)
           (parameters (%%cdr signature)))
      (values name type access compatibility parameters body))))


(define (jazz.walk-syntax-declaration walker resume declaration environment form)
  (receive (name type access compatibility parameters body) (jazz.parse-syntax walker resume declaration (%%cdr form))
    (let ((signature (jazz.walk-parameters walker resume declaration environment parameters #f #f)))
      (let ((new-declaration (jazz.new-syntax-declaration name type access compatibility '() declaration signature)))
        (let ((effective-declaration (jazz.add-declaration-child walker resume declaration new-declaration)))
          effective-declaration)))))


(define (jazz.walk-syntax walker resume declaration environment form)
  (receive (name type access compatibility parameters body) (jazz.parse-syntax walker resume declaration (%%cdr form))
    (let* ((new-declaration (jazz.find-form-declaration declaration name)))
      (receive (signature augmented-environment) (jazz.walk-parameters walker resume declaration environment parameters #f #t)
        (%%set-syntax-declaration-signature new-declaration signature)
        (%%set-syntax-declaration-body new-declaration
          (jazz.walk-body walker resume new-declaration augmented-environment body))
        new-declaration))))


;;;
;;;; Parameters
;;;


;; symbol : standard positional parameter
;; (specifier/non-symbol/non-keyword-expression symbol) : dynamic positional parameter
;; (symbol expression) : optional parameter
;; (keyword symbol expression) : named parameter
;; . symbol : rest parameter
(define (jazz.walk-parameters walker resume declaration environment parameters extended? walk?)
  (let ((section 'positional)
        (positional (jazz.new-queue))
        (optional (jazz.new-queue))
        (named (jazz.new-queue))
        (rest #f)
        (augmented-environment environment))
    (let iter ((scan parameters))
      (cond ((%%null? scan))
            ;; rest parameter
            ((%%symbol? scan)
             (let ((parameter-expression (jazz.new-rest-parameter scan jazz.List)))
               (set! rest parameter-expression)
               (%%when walk?
                 (set! augmented-environment (%%cons parameter-expression augmented-environment)))))
            (else
             (let ((parameter (%%car scan)))
               (jazz.parse-specifier (%%cdr scan)
                 (lambda (specifier rest)
                   (cond ;; positional parameter
                         ((%%symbol? parameter)
                          (if (%%eq? section 'positional)
                              (let ((type (if specifier (jazz.walk-specifier walker resume declaration environment specifier) #f)))
                                (let ((positional-parameter (jazz.new-parameter parameter type)))
                                  (jazz.enqueue positional positional-parameter)
                                  (%%when walk?
                                    (set! augmented-environment (%%cons positional-parameter augmented-environment)))))
                            (jazz.walk-error walker resume declaration "Ill-formed lambda parameters: {s}" parameters)))
                         ((and extended? (%%pair? parameter))
                          (cond ;; dynamic parameter
                                ((or (jazz.specifier? (%%car parameter))
                                     ;; quicky support for autoload expression
                                     (%%pair? (%%car parameter)))
                                 (if (%%eq? section 'positional)
                                     (let* ((specifier/code (%%car parameter))
                                            (code (if (jazz.specifier? specifier/code) (jazz.specifier->name specifier/code) specifier/code))
                                            (variable (%%cadr parameter))
                                            (dynamic-parameter (jazz.new-dynamic-parameter variable jazz.Any (if walk? (jazz.walk walker resume declaration augmented-environment code) #f))))
                                       (jazz.enqueue positional dynamic-parameter)
                                       (%%when walk?
                                         (set! augmented-environment (%%cons dynamic-parameter augmented-environment))))
                                   (jazz.walk-error walker resume declaration "Ill-formed lambda parameters: {s}" parameters)))
                                ;; named parameter
                                ((%%keyword? (%%car parameter))
                                 (let ((keyword (%%car parameter))
                                       (variable (%%cadr parameter)))
                                   (jazz.parse-specifier (%%cdr parameter)
                                     (lambda (specifier rest)
                                       (let ((type (if specifier (jazz.walk-specifier walker resume declaration environment specifier) #f))
                                             (default (%%car (%%cddr parameter))))
                                         (set! section 'named)
                                         (if (%%eq? (%%string->symbol (%%keyword->string keyword)) variable)
                                             (let ((named-parameter (jazz.new-named-parameter variable type (if walk? (jazz.walk walker resume declaration augmented-environment default) #f))))
                                               (jazz.enqueue named named-parameter)
                                               (%%when walk?
                                                 (set! augmented-environment (%%cons named-parameter augmented-environment))))
                                           (jazz.walk-error walker resume declaration "Keyword parameter key and name must match: {s}" parameter)))))))
                                ;; optional parameter
                                (else
                                 (let ((variable (%%car parameter)))
                                   (jazz.parse-specifier (%%cdr parameter)
                                     (lambda (specifier rest)
                                       (let ((type (if specifier (jazz.walk-specifier walker resume declaration environment specifier) #f))
                                             (default (%%car rest)))
                                         (set! section 'optional)
                                         (let ((optional-parameter (jazz.new-optional-parameter variable type (if walk? (jazz.walk walker resume declaration augmented-environment default) #f))))
                                           (jazz.enqueue optional optional-parameter)
                                           (%%when walk?
                                             (set! augmented-environment (%%cons optional-parameter augmented-environment)))))))))))
                         (else
                          (jazz.walk-error walker resume declaration "Ill-formed lambda parameter: {s}" parameter)))
                   (iter rest)))))))
    (let ((signature
            (jazz.new-signature
              (jazz.queue-list positional)
              (jazz.queue-list optional)
              (jazz.queue-list named)
              rest)))
      (if walk?
          (values signature augmented-environment)
        signature))))


(define (jazz.emit-signature-casts signature source-declaration environment)
  (let ((queue #f))
    (define (process parameter)
      (let ((type (%%get-lexical-binding-type parameter)))
        ;; simple optimisation
        (if (and type (%%neq? type jazz.Any))
            (let ((cast (jazz.emit-parameter-cast (jazz.emit-binding-reference parameter source-declaration environment) type source-declaration environment)))
              (if cast
                  (begin
                    ;; optimize the by far more frequent case of signatures having no types
                    (if (not queue)
                        (set! queue (jazz.new-queue)))
                    (jazz.enqueue queue cast)))))))
    
    (for-each process (%%get-signature-positional signature))
    (for-each process (%%get-signature-optional signature))
    (for-each process (%%get-signature-named signature))
    (if (%%not queue)
        '()
      (jazz.queue-list queue))))


(define (jazz.emit-signature signature declaration environment)
  (let ((positional (%%get-signature-positional signature))
        (optional (%%get-signature-optional signature))
        (named (%%get-signature-named signature))
        (rest (%%get-signature-rest signature))
        (queue (jazz.new-queue)))
    (define (emit parameter)
      (jazz.enqueue queue (jazz.emit-parameter parameter declaration environment)))
    (for-each emit positional)
    (%%when (%%not (%%null? optional))
      (jazz.enqueue queue #!optional)
      (for-each emit optional))
    (%%when (%%not (%%null? named))
      (jazz.enqueue queue #!key)
      (for-each emit named))
    (%%when rest
      (jazz.enqueue queue #!rest)
      (emit rest))
    (jazz.queue-list queue)))


(jazz.encapsulate-class jazz.Walker)


;;;
;;;; Catalog
;;;


(define jazz.Catalog
  (%%make-table test: eq?))


(define (jazz.get-catalog)
  jazz.Catalog)


(define (jazz.get-catalog-entry module-name)
  (%%table-ref jazz.Catalog module-name #f))


(define (jazz.set-catalog-entry module-name entry)
  (%%table-set! jazz.Catalog module-name entry))


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
    (%%assert (%%class-is? declaration jazz.Library-Declaration)
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
  (let ((source (jazz.resource-pathname (jazz.find-module-src module-name))))
    (define (load-declaration)
      (let ((form (jazz.read-toplevel-form source)))
        (parameterize ((jazz.requested-module-name module-name))
          (case (%%car form)
            ((module)
             (jazz.parse-module-declaration (%%cdr form)))
            ((library)
             (jazz.parse-library-declaration (%%cdr form)))))))
    
    (jazz.with-verbose jazz.parse-verbose? "parsing" source
      (lambda ()
        (load-declaration)))))


(define jazz.parse-read? (make-parameter #f))


(define (jazz.read-toplevel-form source . rest)
  (let ((parse-read? (if (%%null? rest) #t (%%car rest))))
    (let ((form
            (jazz.with-extension-reader (jazz.pathname-extension source)
              (lambda ()
                (call-with-input-file source
                  (lambda (port)
                    (parameterize ((jazz.parse-read? parse-read?))
                      (read port))))))))
      (if (and (%%not (%%eof-object? form)) (%%memq (%%car form) '(module library)))
          form
        (jazz.error "Invalid module declaration in {a}: {s}" source form)))))


;;;
;;;; Core Dialect
;;;


(jazz.Core-Dialect-implement)


(define (jazz.new-core-dialect)
  (jazz.allocate-core-dialect jazz.Core-Dialect))


(jazz.define-method (jazz.dialect-walker (jazz.Core-Dialect dialect))
  (jazz.new-core-walker))


(jazz.encapsulate-class jazz.Core-Dialect)


;;;
;;;; Core Walker
;;;


(jazz.Core-Walker-implement)


(define (jazz.new-core-walker)
  (jazz.allocate-core-walker jazz.Core-Walker '() '()))


(jazz.encapsulate-class jazz.Core-Walker)


;;;
;;;; Register Core
;;;


(jazz.register-dialect 'core (jazz.new-core-dialect)))
