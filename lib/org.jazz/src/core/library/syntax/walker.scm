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


(jazz.define-class-runtime jazz.Walk-Binding)


(jazz.define-method (jazz.emit-type (jazz.Walk-Binding type) source-declaration environment)
  (jazz.sourcified-form (jazz.emit-binding-reference type source-declaration environment)))


(jazz.define-virtual-runtime (jazz.walk-binding-lookup (jazz.Walk-Binding binding) symbol))
(jazz.define-virtual-runtime (jazz.walk-binding-referenced (jazz.Walk-Binding binding)))
(jazz.define-virtual-runtime (jazz.emit-binding-reference (jazz.Walk-Binding binding) source-declaration environment))
(jazz.define-virtual-runtime (jazz.walk-binding-validate-call (jazz.Walk-Binding binding) walker resume source-declaration operator arguments))
(jazz.define-virtual-runtime (jazz.emit-binding-call (jazz.Walk-Binding binding) arguments source-declaration environment))
(jazz.define-virtual-runtime (jazz.emit-inlined-binding-call (jazz.Walk-Binding binding) arguments source-declaration environment))
(jazz.define-virtual-runtime (jazz.walk-binding-validate-assignment (jazz.Walk-Binding binding) walker resume source-declaration))
(jazz.define-virtual-runtime (jazz.walk-binding-assignable? (jazz.Walk-Binding binding)))
(jazz.define-virtual-runtime (jazz.emit-binding-assignment (jazz.Walk-Binding binding) value source-declaration environment))
(jazz.define-virtual-runtime (jazz.walk-binding-walkable? (jazz.Walk-Binding binding)))
(jazz.define-virtual-runtime (jazz.walk-binding-walk-form (jazz.Walk-Binding binding) walker resume declaration environment form-src))
(jazz.define-virtual-runtime (jazz.walk-binding-expandable? (jazz.Walk-Binding binding)))
(jazz.define-virtual-runtime (jazz.walk-binding-expand-form (jazz.Walk-Binding binding) walker resume declaration environment form-src))


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
      `(,(jazz.sourcified-form (jazz.emit-binding-reference binding source-declaration environment))
        ,@(jazz.codes-forms arguments))
      (jazz.call-return-type type)
      #f)))


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


(jazz.define-class-runtime jazz.Lexical-Binding)


(jazz.define-method (jazz.walk-binding-lookup (jazz.Lexical-Binding binding) symbol)
  (if (%%eq? (%%get-lexical-binding-name binding) symbol)
      binding
    #f))


(jazz.encapsulate-class jazz.Lexical-Binding)


;;;
;;;; Declaration
;;;


(jazz.define-class-runtime jazz.Declaration)


(define (jazz.setup-declaration new-declaration)
  (let ((parent (%%get-declaration-parent new-declaration))
        (name (%%get-lexical-binding-name new-declaration)))
    (%%set-declaration-locator new-declaration (if (%%not parent) name (%%compose-name (%%get-declaration-locator parent) name)))
    (%%set-declaration-toplevel new-declaration (if (%%not parent) new-declaration (%%get-declaration-toplevel parent)))))


(jazz.define-virtual-runtime (jazz.resolve-declaration (jazz.Declaration declaration)))


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


(jazz.define-virtual-runtime (jazz.lookup-declaration (jazz.Declaration declaration) symbol external?))


(jazz.define-method (jazz.lookup-declaration (jazz.Declaration declaration) symbol external?)
  #f)


(jazz.define-virtual-runtime (jazz.update-declaration (jazz.Declaration declaration) new-declaration))


(jazz.define-method (jazz.update-declaration (jazz.Declaration declaration) new-declaration)
  #f)


(jazz.define-virtual-runtime (jazz.get-declaration-references (jazz.Declaration declaration)))


(jazz.define-method (jazz.get-declaration-references (jazz.Declaration declaration))
  '())


(jazz.define-virtual-runtime (jazz.emit-declaration (jazz.Declaration declaration) environment))


(jazz.define-method (jazz.emit-declaration (jazz.Declaration declaration) environment)
  (jazz.error "Unable to emit: {s}" declaration))


(jazz.define-virtual-runtime (jazz.expand-referenced-declaration (jazz.Declaration declaration)))


(jazz.define-method (jazz.expand-referenced-declaration (jazz.Declaration declaration))
  '())


(jazz.define-virtual-runtime (jazz.fold-declaration (jazz.Declaration declaration) f k s))


(jazz.define-method (jazz.fold-declaration (jazz.Declaration declaration) f k s)
  (f declaration s))


(jazz.encapsulate-class jazz.Declaration)


;;;
;;;; Declaration Reference
;;;


(jazz.define-class-runtime jazz.Declaration-Reference)


(jazz.define-virtual-runtime (jazz.resolve-reference (jazz.Declaration-Reference declaration-reference) library-declaration))


(jazz.encapsulate-class jazz.Declaration-Reference)


;;;
;;;; Library Reference
;;;


(jazz.define-class-runtime jazz.Library-Reference)


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


(jazz.define-class-runtime jazz.Export-Reference)


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
      (values (jazz.identifier-name name) name)
    (values name name)))


(jazz.encapsulate-class jazz.Export-Reference)


;;;
;;;; Autoload Reference
;;;


(jazz.define-class-runtime jazz.Autoload-Reference)


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


(jazz.define-class-runtime jazz.Module-Declaration)


(define (jazz.new-module-declaration name parent requires)
  (let ((new-declaration (jazz.allocate-module-declaration jazz.Module-Declaration name #f 'public 'uptodate '() #f parent #f #f requires)))
    (jazz.setup-declaration new-declaration)
    new-declaration))


(jazz.encapsulate-class jazz.Module-Declaration)


;;;
;;;; Namespace
;;;


(jazz.define-class-runtime jazz.Namespace-Declaration)


(define (jazz.find-declaration namespace-declaration name)
  (%%table-ref (%%get-access-lookup namespace-declaration jazz.private-access) name #f))


(jazz.encapsulate-class jazz.Namespace-Declaration)


;;;
;;;; Library
;;;


(jazz.define-class-runtime jazz.Library-Declaration)


(define (jazz.new-library-declaration name parent dialect-name dialect-invoice requires exports imports)
  (let ((new-declaration (jazz.allocate-library-declaration jazz.Library-Declaration name #f 'public 'uptodate '() #f parent #f #f (jazz.make-access-lookups jazz.public-access) (%%make-table test: eq?) '() #f dialect-name dialect-invoice requires exports imports #f '() (jazz.new-queue) '() '())))
    (jazz.setup-declaration new-declaration)
    new-declaration))


(define (jazz.setup-library-lookups library-declaration)
  (let ((dialect-invoice (%%get-library-declaration-dialect-invoice library-declaration)))
    (%%when dialect-invoice
      (jazz.add-library-import library-declaration dialect-invoice)))
  (for-each (lambda (imported-library-invoice)
              (jazz.add-library-import library-declaration imported-library-invoice))
            (%%get-library-declaration-imports library-declaration))
  (for-each (lambda (exported-library-invoice)
              (jazz.add-library-export library-declaration exported-library-invoice))
            (%%get-library-declaration-exports library-declaration)))


(define (jazz.add-library-import library-declaration imported-library-invoice)
  (let ((private (%%get-access-lookup library-declaration jazz.private-access))
        (only (%%get-library-invoice-only imported-library-invoice)))
    (if only
        ;; todo
        #f
      (let ((imported-library-declaration (%%get-library-invoice-library imported-library-invoice)))
        (let ((imported (%%get-access-lookup imported-library-declaration jazz.public-access)))
          (jazz.table-merge-without-conflicts! library-declaration "imports" private imported))))))


(define (jazz.add-library-export library-declaration exported-library-invoice)
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
             (jazz.table-merge-without-conflicts! library-declaration "exports" public (%%get-access-lookup exported-library-declaration jazz.public-access)))))))


(define (jazz.table-merge-without-conflicts! library-declaration suffix table add)
  (let ((table-count (%%table-length table))
        (add-count (%%table-length add)))
    (%%table-merge! table add)
    (%%when (%%not (%%fx= (%%table-length table) (%%fx+ table-count add-count)))
      (let ((conflicts (jazz.find-actual-conflicts table add)))
        ;; Can be null if the same declaration has been imported from
        ;; different libraries. Maybe we should also do an error in that case...
        (%%when #f ;; temp commented because of bug (%%not (%%null? conflicts))
          (jazz.error "Conflicts detected in {a} {a}: {s}"
                      (%%get-lexical-binding-name library-declaration)
                      suffix
                      (map (lambda (conflict)
                             (%%list (%%car conflict)
                                     (%%get-declaration-locator (%%cadr conflict))
                                     (%%get-declaration-locator (%%car (%%cddr conflict)))))
                           conflicts)))))))


(define (jazz.find-actual-conflicts table add)
  (let ((lst '()))
    (%%iterate-table add
      (lambda (key value)
        (let ((actual (%%table-ref table key #f)))
          (%%when (%%neq? value actual)
            (set! lst (%%cons (%%list key value actual) lst))))))
    lst))


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
                        (jazz.add-library-import declaration new-invoice))))))
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
                        (%%set-library-declaration-exports declaration (%%append (%%get-library-declaration-exports declaration) (%%list new-invoice)))
                        (jazz.add-library-export declaration new-invoice))))))
              (%%get-library-declaration-exports new-declaration))))


(jazz.define-method (jazz.emit-declaration (jazz.Library-Declaration declaration) environment)
  (let ((body-expansion (jazz.emit-namespace-statements (%%get-namespace-declaration-body declaration) declaration environment))
        (references-expansion (jazz.expand-library-references declaration))
        (literals-expansion (jazz.expand-library-literals declaration))
        (variables-expansion (jazz.expand-library-variables declaration))
        (autoloads-expansion (jazz.expand-library-autoloads declaration environment))
        (locator (%%get-declaration-locator declaration)))
    `(begin
       ,@(case (jazz.walk-for)
           ((eval) '())
           (else (jazz.declares 'library)))
       ,@(let ((queue (jazz.new-queue)))
           (jazz.enqueue queue `(jazz.load-module 'core.library))
           (let ((dialect-name (%%get-library-declaration-dialect-name declaration)))
             (%%when (%%neq? dialect-name 'core)
               (jazz.enqueue queue `(jazz.load-module ',dialect-name))))
           (for-each (lambda (spec)
                       (jazz.parse-require spec
                         (lambda (module-name feature-requirement phase)
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
                                                 (let ((symbol-name (jazz.compose-name module-name name)))
                                                   (jazz.enqueue queue `(jazz.register-autoload ',name ',module-name
                                                                          (lambda ()
                                                                            (jazz.load-module ',module-name)
                                                                            ,symbol-name))))))
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
                (%%debug-assertion (%%category-is? object type) (jazz.dispatch-error field object type)
                  (%%final-dispatch object (%%get-method-implementation field))))
              (define (__class-dispatch object class-level implementation-rank type)
                (%%debug-assertion (%%category-is? object type) (jazz.dispatch-error #f object type)
                  (%%class-dispatch object class-level implementation-rank)))
              (define (__interface-dispatch object interface-rank implementation-rank type)
                (%%debug-assertion (%%category-is? object type) (jazz.dispatch-error #f object type)
                  (%%interface-dispatch object interface-rank implementation-rank)))
              (define __dispatchers
                (%%vector __final-dispatch __class-dispatch __interface-dispatch))))
           (else
            '()))
       ,@references-expansion
       ,@autoloads-expansion
       ,@literals-expansion
       ,@variables-expansion
       ,@body-expansion
       (jazz.module-loaded ',locator))))


(jazz.define-method (jazz.fold-declaration (jazz.Library-Declaration declaration) f k s)
  (f declaration (jazz.fold-statements (%%get-namespace-declaration-body declaration) f k s s)))


(jazz.encapsulate-class jazz.Library-Declaration)


;;;
;;;; Library Invoice
;;;


(jazz.define-class-runtime jazz.Library-Invoice)


(jazz.encapsulate-class jazz.Library-Invoice)


;;;
;;;; Export Invoice
;;;


(jazz.define-class-runtime jazz.Export-Invoice)


(define (jazz.new-export-invoice library phase version only autoload)
  (jazz.allocate-export-invoice jazz.Export-Invoice library phase version only #f #f #f autoload))


(jazz.encapsulate-class jazz.Export-Invoice)


;;;
;;;; Import Invoice
;;;


(jazz.define-class-runtime jazz.Import-Invoice)


(define (jazz.new-import-invoice library phase version only)
  (jazz.allocate-import-invoice jazz.Import-Invoice library phase version only #f #f #f))


(jazz.encapsulate-class jazz.Import-Invoice)


;;;
;;;; Export
;;;


(jazz.define-class-runtime jazz.Export-Declaration)


(define (jazz.new-export-declaration name type access compatibility attributes parent symbol)
  (let ((new-declaration (jazz.allocate-export-declaration jazz.Export-Declaration name type access compatibility attributes #f parent #f #f symbol)))
    (jazz.setup-declaration new-declaration)
    new-declaration))


(jazz.define-method (jazz.walk-binding-validate-call (jazz.Export-Declaration declaration) walker resume source-declaration operator arguments)
  (jazz.unspecified))


(jazz.define-method (jazz.emit-declaration (jazz.Export-Declaration declaration) environment)
  `(begin))


(jazz.define-method (jazz.emit-binding-reference (jazz.Export-Declaration declaration) source-declaration environment)
  (jazz.new-code
    (%%get-export-declaration-symbol declaration)
    jazz.Any
    #f))


(jazz.encapsulate-class jazz.Export-Declaration)


;;;
;;;; Autoload
;;;


(jazz.define-class-runtime jazz.Autoload-Declaration)


(define (jazz.new-autoload-declaration name type parent library-declaration exported-library)
  (let ((new-declaration (jazz.allocate-autoload-declaration jazz.Autoload-Declaration name type 'public 'uptodate '() #f parent #f #f library-declaration exported-library #f)))
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
      (jazz.resolve-declaration declaration)
      #f)))


(define (jazz.autoload-locator referenced-declaration)
  (%%string->symbol (%%string-append (%%symbol->string (%%get-declaration-locator referenced-declaration))
                                     ":autoload")))


(jazz.encapsulate-class jazz.Autoload-Declaration)


;;;
;;;; Void
;;;


(jazz.define-class-runtime jazz.Void-Class)


(jazz.define-method (jazz.of-subtype? (jazz.Void-Class type) subtype)
  #f)


(jazz.define-method (jazz.emit-specifier (jazz.Void-Class type))
  'void)


(jazz.encapsulate-class jazz.Void-Class)


(jazz.define-class-runtime jazz.Void)


(jazz.encapsulate-class jazz.Void)


;;;
;;;; Opt
;;;


(jazz.define-class-runtime jazz.Opt-Type)


(define (jazz.new-opt-type type)
  (jazz.allocate-opt-type jazz.Opt-Type type))


(jazz.define-method (jazz.emit-specifier (jazz.Opt-Type type))
  (let ((type-specifier (jazz.emit-specifier (%%get-opt-type-type type))))
    (%%string->symbol (%%string-append "opt<" (%%symbol->string type-specifier) ">"))))


(jazz.encapsulate-class jazz.Opt-Type)


;;;
;;;; Key
;;;


(jazz.define-class-runtime jazz.Key-Type)


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


(jazz.define-class-runtime jazz.Rest-Type)


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


(jazz.define-class-runtime jazz.Function-Type)


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


(jazz.define-class-runtime jazz.Category-Type)


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


(jazz.define-class-runtime jazz.Values-Type)


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


(jazz.define-class-runtime jazz.Restriction-Type)


(define (jazz.new-restriction-type base type)
  (jazz.allocate-restriction-type jazz.Restriction-Type base type))


(jazz.encapsulate-class jazz.Restriction-Type)


;;;
;;;; Complement
;;;


(jazz.define-class-runtime jazz.Complement-Type)


(define (jazz.new-complement-type type)
  (jazz.allocate-complement-type jazz.Complement-Type type))


(jazz.encapsulate-class jazz.Complement-Type)


;;;
;;;; Union
;;;


(jazz.define-class-runtime jazz.Union-Type)


(define (jazz.new-union-type types)
  (jazz.allocate-union-type jazz.Union-Type types))


(jazz.encapsulate-class jazz.Union-Type)


;;;
;;;; Template
;;;


;; future work. just here to make sure specifier syntax can express them


(jazz.define-class-runtime jazz.Template-Type)


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


(jazz.define-class-runtime jazz.Nillable-Type)


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


(jazz.define-class-runtime jazz.Any-Class)


(jazz.define-method (jazz.of-subtype? (jazz.Any-Class type) subtype)
  #t)


(jazz.define-method (jazz.emit-specifier (jazz.Any-Class type))
  'any)


(jazz.define-method (jazz.emit-check (jazz.Any-Class type) value source-declaration environment)
  #f)


(jazz.encapsulate-class jazz.Any-Class)


(jazz.define-class-runtime jazz.Any)


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
         (jazz.sourcified-form code)
       (let ((value (jazz.generate-symbol "val")))
         ;; coded the flonum case here for now has it is the only castable type
         (if (%%eq? type jazz.Flonum)
             `(let ((,value ,(jazz.sourcified-form code)))
                (if (%%fixnum? ,value)
                    (%%fixnum->flonum ,value)
                  ,value))
           (jazz.sourcified-form code))))))
  (else
   (define (jazz.emit-type-cast code type source-declaration environment)
     (if (or (%%not type) (%%subtype? (%%get-code-type code) type))
         (jazz.sourcified-form code)
       (let ((value (jazz.generate-symbol "val")))
         ;; coded the flonum case here for now has it is the only castable type
         (if (%%eq? type jazz.Flonum)
             `(let ((,value ,(jazz.sourcified-form code)))
                (if (%%fixnum? ,value)
                    (%%fixnum->flonum ,value)
                  (begin
                    ,(jazz.emit-check type value source-declaration environment)
                    ,value)))
           `(let ((,value ,(jazz.sourcified-form code)))
              ,(jazz.emit-check type value source-declaration environment)
              ,value)))))))


(cond-expand
  (release
    (define (jazz.emit-parameter-cast code type source-declaration environment)
      (if (or (%%not type) (%%eq? type jazz.Any) (%%object-class? type) (jazz.object-declaration? type))
         #f
       (let ((parameter (jazz.sourcified-form code)))
         ;; coded the flonum case here for now has it is the only castable type
         (if (%%eq? type jazz.Flonum)
             `(if (%%fixnum? ,parameter)
                  (set! ,parameter (%%fixnum->flonum ,parameter)))
           #f)))))
  (else
   (define (jazz.emit-parameter-cast code type source-declaration environment)
     (if (or (%%not type) (%%eq? type jazz.Any) (%%object-class? type) (jazz.object-declaration? type))
         #f
       (let ((parameter (jazz.sourcified-form code)))
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
  (if (and (%%pair? lst) (jazz.specifier? (%%source-code (%%car lst))))
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


(%%table-set! jazz.primitive-types 'any          jazz.Any)
(%%table-set! jazz.primitive-types 'object       jazz.Object)
(%%table-set! jazz.primitive-types 'bool         jazz.Boolean)
(%%table-set! jazz.primitive-types 'char         jazz.Char)
(%%table-set! jazz.primitive-types 'number       jazz.Number)
(%%table-set! jazz.primitive-types 'complex      jazz.Complex)
(%%table-set! jazz.primitive-types 'real         jazz.Real)
(%%table-set! jazz.primitive-types 'rational     jazz.Rational)
(%%table-set! jazz.primitive-types 'int          jazz.Integer)
(%%table-set! jazz.primitive-types 'fx           jazz.Fixnum)
(%%table-set! jazz.primitive-types 'fl           jazz.Flonum)
(%%table-set! jazz.primitive-types 'list         jazz.List)
(%%table-set! jazz.primitive-types 'null         jazz.Null)
(%%table-set! jazz.primitive-types 'pair         jazz.Pair)
(%%table-set! jazz.primitive-types 'port         jazz.Port)
(%%table-set! jazz.primitive-types 'continuation jazz.Continuation)
(%%table-set! jazz.primitive-types 'procedure    jazz.Procedure)
(%%table-set! jazz.primitive-types 'string       jazz.String)
(%%table-set! jazz.primitive-types 'symbol       jazz.Symbol)
(%%table-set! jazz.primitive-types 'keyword      jazz.Keyword)
(%%table-set! jazz.primitive-types 'vector       jazz.Vector)
(%%table-set! jazz.primitive-types 'u8vector     jazz.U8Vector)
(%%table-set! jazz.primitive-types 'table        jazz.Table)
(%%table-set! jazz.primitive-types 'thread       jazz.Thread)
(%%table-set! jazz.primitive-types 'promise      jazz.Promise)
(%%table-set! jazz.primitive-types 'foreign      jazz.Foreign)
(%%table-set! jazz.primitive-types 'values       jazz.Values)
(%%table-set! jazz.primitive-types 'eof          jazz.EOF)
(%%table-set! jazz.primitive-types 'unspecified  jazz.Unspecified)
(%%table-set! jazz.primitive-types 'void         jazz.Void)


(define (jazz.lookup-primitive-type name)
  (%%table-ref jazz.primitive-types name #f))


(define jazz.primitive-declarations
  (%%make-table test: eq?))


(%%table-set! jazz.primitive-declarations jazz.Object       'Object)
(%%table-set! jazz.primitive-declarations jazz.Boolean      'Boolean)
(%%table-set! jazz.primitive-declarations jazz.Char         'Char)
(%%table-set! jazz.primitive-declarations jazz.Number       'Number)
(%%table-set! jazz.primitive-declarations jazz.Complex      'Complex)
(%%table-set! jazz.primitive-declarations jazz.Real         'Real)
(%%table-set! jazz.primitive-declarations jazz.Rational     'Rational)
(%%table-set! jazz.primitive-declarations jazz.Integer      'Integer)
(%%table-set! jazz.primitive-declarations jazz.Fixnum       'Fixnum)
(%%table-set! jazz.primitive-declarations jazz.Flonum       'Flonum)
(%%table-set! jazz.primitive-declarations jazz.List         'List)
(%%table-set! jazz.primitive-declarations jazz.Null         'Null)
(%%table-set! jazz.primitive-declarations jazz.Pair         'Pair)
(%%table-set! jazz.primitive-declarations jazz.Port         'Port)
(%%table-set! jazz.primitive-declarations jazz.Continuation 'Continuation)
(%%table-set! jazz.primitive-declarations jazz.Procedure    'Procedure)
(%%table-set! jazz.primitive-declarations jazz.String       'String)
(%%table-set! jazz.primitive-declarations jazz.Symbol       'Symbol)
(%%table-set! jazz.primitive-declarations jazz.Keyword      'Keyword)
(%%table-set! jazz.primitive-declarations jazz.Vector       'Vector)
(%%table-set! jazz.primitive-declarations jazz.U8Vector     'U8Vector)
(%%table-set! jazz.primitive-declarations jazz.Table        'Table)
(%%table-set! jazz.primitive-declarations jazz.Thread       'Thread)
(%%table-set! jazz.primitive-declarations jazz.Promise      'Promise)
(%%table-set! jazz.primitive-declarations jazz.Foreign      'Foreign)
(%%table-set! jazz.primitive-declarations jazz.Values       'Values)
(%%table-set! jazz.primitive-declarations jazz.EOF          'EOF)
(%%table-set! jazz.primitive-declarations jazz.Unspecified  'Unspecified)


;; quicky until we can somehow unify primitive types and declarations
(define (jazz.patch-type-until-unification type)
  (let ((name (%%table-ref jazz.primitive-declarations type #f)))
    (if name
        (let ((library-declaration (jazz.get-catalog-entry 'jazz.dialect.language)))
          (if library-declaration
              (jazz.lookup-declaration library-declaration name #t)
            type))
      type)))


;;;
;;;; Macro
;;;


(jazz.define-class-runtime jazz.Macro-Declaration)


(define (jazz.new-macro-declaration name type access compatibility attributes parent signature)
  (let ((new-declaration (jazz.allocate-macro-declaration jazz.Macro-Declaration name type access compatibility attributes #f parent #f #f signature #f)))
    (jazz.setup-declaration new-declaration)
    new-declaration))


(jazz.define-method (jazz.walk-binding-expandable? (jazz.Macro-Declaration declaration))
  #t)


(jazz.define-method (jazz.walk-binding-expand-form (jazz.Macro-Declaration binding) walker resume declaration environment form-src)
  (let ((form (%%desourcify form-src)))
    (let ((locator (%%get-declaration-locator binding)))
      (if (%%eq? (%%get-declaration-toplevel binding) (%%get-declaration-toplevel declaration))
          (jazz.walk-error walker resume declaration "Macros cannot be used from within the same file: {s}" locator)
        (let ((parent-declaration (%%get-declaration-parent binding)))
          (jazz.load-module (%%get-declaration-locator parent-declaration))
          (let ((expander (jazz.need-macro locator)))
            (%%apply expander (%%cdr form))))))))


(jazz.define-method (jazz.emit-declaration (jazz.Macro-Declaration declaration) environment)
  (let ((locator (%%get-declaration-locator declaration))
        (signature (%%get-macro-declaration-signature declaration))
        (body (%%get-macro-declaration-body declaration)))
    (jazz.with-annotated-frame (jazz.annotate-signature signature)
      (lambda (frame)
        (let ((augmented-environment (cons frame environment)))
          (jazz.sourcify-if
            `(jazz.define-macro ,(%%cons locator (jazz.emit-signature signature declaration augmented-environment))
               ,@(jazz.sourcified-form (jazz.emit-expression body declaration augmented-environment)))
            (%%get-declaration-source declaration)))))))


(jazz.define-method (jazz.fold-declaration (jazz.Macro-Declaration declaration) f k s)
  (f declaration
     (k (%%get-macro-declaration-signature declaration)
        (k (jazz.fold-statement (%%get-macro-declaration-body declaration) f k s)
           s))))


(jazz.encapsulate-class jazz.Macro-Declaration)


;;;
;;;; Syntax
;;;


(jazz.define-class-runtime jazz.Syntax-Declaration)


(define (jazz.new-syntax-declaration name type access compatibility attributes parent signature)
  (let ((new-declaration (jazz.allocate-syntax-declaration jazz.Syntax-Declaration name type access compatibility attributes #f parent #f #f signature #f)))
    (jazz.setup-declaration new-declaration)
    new-declaration))


(jazz.define-method (jazz.walk-binding-expandable? (jazz.Syntax-Declaration declaration))
  #t)


(jazz.define-method (jazz.walk-binding-expand-form (jazz.Syntax-Declaration binding) walker resume declaration environment form-src)
  (let ((locator (%%get-declaration-locator binding)))
    (if (%%eq? (%%get-declaration-toplevel binding) (%%get-declaration-toplevel declaration))
        (jazz.walk-error walker resume declaration "Syntaxes cannot be used from within the same file: {s}" locator)
      (let ((parent-declaration (%%get-declaration-parent binding)))
        (jazz.load-module (%%get-declaration-locator parent-declaration))
        (let ((expander (jazz.need-macro locator)))
          (expander form-src))))))


(jazz.define-method (jazz.emit-declaration (jazz.Syntax-Declaration declaration) environment)
  (let ((locator (%%get-declaration-locator declaration))
        (signature (%%get-syntax-declaration-signature declaration))
        (body (%%get-syntax-declaration-body declaration)))
    (jazz.with-annotated-frame (jazz.annotate-signature signature)
      (lambda (frame)
        (let ((augmented-environment (cons frame environment)))
          (jazz.sourcify-if
            `(jazz.define-macro ,(%%cons locator (jazz.emit-signature signature declaration augmented-environment))
               ,@(jazz.sourcified-form (jazz.emit-expression body declaration augmented-environment)))
            (%%get-declaration-source declaration)))))))


(jazz.define-method (jazz.fold-declaration (jazz.Syntax-Declaration declaration) f k s)
  (f declaration
     (k (%%get-syntax-declaration-signature declaration)
        (k (jazz.fold-statement (%%get-syntax-declaration-body declaration) f k s)
           s))))


(jazz.encapsulate-class jazz.Syntax-Declaration)


;;;
;;;; C Type
;;;


(jazz.define-class-runtime jazz.C-Type-Declaration)


(define (jazz.new-c-type-declaration name type access compatibility attributes parent kind expansion base-type references c-to-scheme scheme-to-c declare)
  (let ((new-declaration (jazz.allocate-c-type-declaration jazz.C-Type-Declaration name type access compatibility attributes #f parent #f #f kind expansion base-type '() references c-to-scheme scheme-to-c declare)))
    (jazz.setup-declaration new-declaration)
    new-declaration))


(jazz.define-method (jazz.get-declaration-references (jazz.C-Type-Declaration declaration))
  (%%get-c-type-declaration-references declaration))


(jazz.define-method (jazz.emit-declaration (jazz.C-Type-Declaration declaration) environment)
  `(begin))


(jazz.define-method (jazz.expand-referenced-declaration (jazz.C-Type-Declaration declaration))
  (let ((locator (%%get-declaration-locator declaration))
        (expansion (%%get-c-type-declaration-expansion declaration))
        (c-to-scheme (%%get-c-type-declaration-c-to-scheme declaration))
        (scheme-to-c (%%get-c-type-declaration-scheme-to-c declaration)))
    `(c-define-type ,locator ,expansion ,@(if (and c-to-scheme scheme-to-c)
                                              (list c-to-scheme scheme-to-c #f)
                                            '()))))


(jazz.encapsulate-class jazz.C-Type-Declaration)


;;;
;;;; C Definition
;;;


(jazz.define-class-runtime jazz.C-Definition-Declaration)


(define (jazz.new-c-definition-declaration name type access compatibility attributes parent signature parameter-types result-type c-name scope)
  (let ((new-declaration (jazz.allocate-c-definition-declaration jazz.C-Definition-Declaration name type access compatibility attributes #f parent #f #f signature parameter-types result-type c-name scope #f)))
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
          (jazz.sourcify-if
            `(c-define ,(%%cons locator (jazz.emit-signature signature declaration augmented-environment)) ,parameter-types ,result-type ,c-name ,scope
               ,@(jazz.sourcified-form (jazz.emit-expression body declaration augmented-environment)))
            (%%get-declaration-source declaration)))))))


(jazz.define-method (jazz.emit-binding-reference (jazz.C-Definition-Declaration declaration) source-declaration environment)
  (jazz.new-code
    (%%get-declaration-locator declaration)
    jazz.Any
    #f))


(jazz.encapsulate-class jazz.C-Definition-Declaration)


;;;
;;;; Walk Context
;;;


(jazz.define-class-runtime jazz.Walk-Context)


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


(jazz.define-class-runtime jazz.Walk-Location)


(define (jazz.new-walk-location module-locator declaration-locator locat)
  (jazz.allocate-walk-location jazz.Walk-Location module-locator declaration-locator locat))


(define (jazz.walk-location walker declaration locat)
  (jazz.new-walk-location
    (jazz.get-walk-locator)
    (%%get-declaration-locator declaration)
    locat))


(jazz.encapsulate-class jazz.Walk-Location)


;;;
;;;; Walk Problem
;;;


(jazz.define-class-runtime jazz.Walk-Problem)


(jazz.encapsulate-class jazz.Walk-Problem)


;;;
;;;; Walk Problems
;;;


(jazz.define-class-runtime jazz.Walk-Problems)


(define (jazz.new-walk-problems message warnings errors)
  (jazz.allocate-walk-problems jazz.Walk-Problems message warnings errors))


(jazz.define-method (jazz.get-detail (jazz.Walk-Problems problems))
  (define (add-details problems queue)
    (for-each (lambda (problem)
                (jazz.enqueue queue (jazz.new-exception-detail "Green" (jazz.present-exception problem) (%%get-walk-problem-location problem) '())))
              problems))
  
  (jazz.new-exception-detail "ErrorStop" "Walk problems encountered:" #f
    (let ((all (%%append (%%get-walk-problems-warnings problems)
                         (%%get-walk-problems-errors problems))))
      (map (lambda (partition)
             (jazz.bind (module-locator . problems) partition
               (let ((prefix (if (%%not module-locator) -1 (%%string-length (%%symbol->string module-locator)))))
                 (jazz.new-exception-detail "Document" (or module-locator "<console>") #f
                   (let ((module-details (jazz.new-queue)))
                     (for-each (lambda (partition)
                                 (jazz.bind (declaration-locator . problems) partition
                                   (if (%%fx= (%%string-length declaration-locator) prefix)
                                       (add-details problems module-details)
                                     (jazz.enqueue module-details
                                       (jazz.new-exception-detail "Project" (%%substring declaration-locator (%%fx+ prefix 1) (%%string-length declaration-locator)) #f
                                         (let ((declaration-details (jazz.new-queue)))
                                           (add-details problems declaration-details)
                                           (jazz.queue-list declaration-details)))))))
                               (jazz.partition-walk-problems-declaration problems))
                     (jazz.queue-list module-details))))))
           (jazz.partition-walk-problems-module all)))))


(jazz.encapsulate-class jazz.Walk-Problems)


;;;
;;;; Walk Warning
;;;


(jazz.define-class-runtime jazz.Walk-Warning)


(define (jazz.new-walk-warning location message)
  (jazz.allocate-walk-warning jazz.Walk-Warning message location))


(jazz.encapsulate-class jazz.Walk-Warning)


;;;
;;;; Walk Error
;;;


(jazz.define-class-runtime jazz.Walk-Error)


(define (jazz.new-walk-error location message)
  (jazz.allocate-walk-error jazz.Walk-Error message location))


(jazz.encapsulate-class jazz.Walk-Error)


;;;
;;;; Unresolved Error
;;;


(jazz.define-class-runtime jazz.Unresolved-Error)


(define (jazz.new-unresolved-error location symbol)
  (jazz.allocate-unresolved-error jazz.Unresolved-Error #f location symbol))


(jazz.define-method (jazz.present-exception (jazz.Unresolved-Error error))
  (jazz.format "Unresolved symbol: {s}"
               (%%get-unresolved-error-symbol error)))


(jazz.encapsulate-class jazz.Unresolved-Error)


;;;
;;;; Walk Frame
;;;


(jazz.define-class-runtime jazz.Walk-Frame)


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


(jazz.define-class-runtime jazz.Signature)


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


(jazz.define-class-runtime jazz.Symbol-Binding)


(jazz.encapsulate-class jazz.Symbol-Binding)


;;;
;;;; Variable
;;;


(jazz.define-class-runtime jazz.Variable)


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
    (jazz.find-annotated-type binding environment)
    #f))


(jazz.define-method (jazz.walk-binding-assignable? (jazz.Variable declaration))
  #t)


(jazz.define-method (jazz.emit-binding-assignment (jazz.Variable binding) value source-declaration environment)
  (let ((value-code (jazz.emit-expression value source-declaration environment)))
    (receive (annotated-frame annotated-variable annotated-type) (jazz.find-annotated binding environment)
      (%%when (%%class-is? annotated-variable jazz.Annotated-Variable)
        (jazz.extend-annotated-type annotated-frame annotated-variable (%%get-code-type value-code))))
    (jazz.new-code
      `(set! ,(%%get-lexical-binding-name binding) ,(jazz.sourcified-form value-code))
      jazz.Any
      #f)))


(jazz.encapsulate-class jazz.Variable)


;;;
;;;; NextMethod Variable
;;;


(jazz.define-class-runtime jazz.NextMethod-Variable)


(define (jazz.new-nextmethod-variable name type)
  (%%assertion (jazz.variable-name-valid? name) (jazz.error "Invalid variable name: {s}" name)
    (jazz.allocate-nextmethod-variable jazz.NextMethod-Variable name type 0)))


(jazz.define-method (jazz.emit-binding-reference (jazz.NextMethod-Variable binding) source-declaration environment)
  (let ((name (%%get-lexical-binding-name binding))
        (self (jazz.*self*)))
    (jazz.new-code
      (if self
          `(lambda rest (apply ,name ,(jazz.sourcified-form self) rest))
        name)
      jazz.Any
      #f)))


(jazz.define-method (jazz.emit-binding-call (jazz.NextMethod-Variable binding) arguments source-declaration environment)
  (let ((name (%%get-lexical-binding-name binding))
        (type (%%get-lexical-binding-type binding))
        (self (jazz.*self*)))
    (if self
        (jazz.new-code
          `(,(%%get-lexical-binding-name binding)
            ,(jazz.sourcified-form self)
            ,@(jazz.codes-forms arguments))
          (jazz.call-return-type type)
          #f)
      (jazz.new-code
        `(,(%%get-lexical-binding-name binding)
          ,@(jazz.codes-forms arguments))
        (jazz.call-return-type type)
        #f))))


(jazz.encapsulate-class jazz.NextMethod-Variable)


;;;
;;;; Parameter
;;;


(jazz.define-class-runtime jazz.Parameter)


(define (jazz.new-parameter name type)
  (%%assertion (jazz.variable-name-valid? name) (jazz.error "Invalid variable name: {s}" name)
    (jazz.allocate-parameter jazz.Parameter name type 0)))


(jazz.define-virtual-runtime (jazz.emit-parameter (jazz.Parameter parameter) declaration environment))


(jazz.define-method (jazz.emit-parameter (jazz.Parameter parameter) declaration environment)
  (%%get-lexical-binding-name parameter))


(jazz.encapsulate-class jazz.Parameter)


;;;
;;;; Dynamic Parameter
;;;


(jazz.define-class-runtime jazz.Dynamic-Parameter)


(define (jazz.new-dynamic-parameter name type class)
  (jazz.allocate-dynamic-parameter jazz.Dynamic-Parameter name type 0 class))


(jazz.define-method (jazz.emit-parameter (jazz.Dynamic-Parameter parameter) declaration environment)
  (let ((class (%%get-dynamic-parameter-class parameter)))
    (%%list (jazz.sourcified-form (jazz.emit-expression class declaration environment)) (%%get-lexical-binding-name parameter))))


(jazz.encapsulate-class jazz.Dynamic-Parameter)


;;;
;;;; Optional Parameter
;;;


(jazz.define-class-runtime jazz.Optional-Parameter)


(define (jazz.new-optional-parameter name type default)
  (jazz.allocate-optional-parameter jazz.Optional-Parameter name type 0 default))


(jazz.define-method (jazz.emit-parameter (jazz.Optional-Parameter parameter) declaration environment)
  (let ((default (%%get-optional-parameter-default parameter)))
    (%%list (%%get-lexical-binding-name parameter) (jazz.sourcified-form (jazz.emit-expression default declaration environment)))))


(jazz.encapsulate-class jazz.Optional-Parameter)


;;;
;;;; Named Parameter
;;;


(jazz.define-class-runtime jazz.Named-Parameter)


(define (jazz.new-named-parameter name type default)
  (jazz.allocate-named-parameter jazz.Named-Parameter name type 0 default))


(jazz.define-method (jazz.emit-parameter (jazz.Named-Parameter parameter) declaration environment)
  (let ((default (%%get-named-parameter-default parameter)))
    (%%list (%%get-lexical-binding-name parameter) (jazz.sourcified-form (jazz.emit-expression default declaration environment)))))


(jazz.encapsulate-class jazz.Named-Parameter)


;;;
;;;; Rest Parameter
;;;


(jazz.define-class-runtime jazz.Rest-Parameter)


(define (jazz.new-rest-parameter name type)
  (jazz.allocate-rest-parameter jazz.Rest-Parameter name type 0))


(jazz.define-method (jazz.emit-parameter (jazz.Rest-Parameter parameter) declaration environment)
  (%%get-lexical-binding-name parameter))


(jazz.encapsulate-class jazz.Rest-Parameter)


;;;
;;;; Self-Binding
;;;


;; Support for dialects that have an implicit self concept


(jazz.define-class-runtime jazz.Self-Binding)


(define (jazz.new-self-binding type)
  (jazz.allocate-self-binding jazz.Self-Binding 'self type))


(jazz.define-method (jazz.emit-binding-reference (jazz.Self-Binding declaration) source-declaration environment)
  (jazz.new-code
    'self
    (%%get-declaration-parent source-declaration)
    #f))


(jazz.encapsulate-class jazz.Self-Binding)


;;;
;;;; With-Self
;;;


(define jazz.*self*
  (make-parameter #f))


;;;
;;;; Macro Symbol
;;;


(jazz.define-class-runtime jazz.Macro-Symbol)


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


(jazz.define-class-runtime jazz.Form-Binding)


(jazz.encapsulate-class jazz.Form-Binding)


;;;
;;;; Special Form
;;;


(jazz.define-class-runtime jazz.Special-Form)


(define (jazz.new-special-form name walk)
  (jazz.allocate-special-form jazz.Special-Form name #f walk))


(jazz.define-method (jazz.walk-binding-walkable? (jazz.Special-Form binding))
  #t)


(jazz.define-method (jazz.walk-binding-walk-form (jazz.Special-Form binding) walker resume declaration environment form-src)
  (let ((walk (%%get-special-form-walk binding)))
    (walk walker resume declaration environment form-src)))


(jazz.encapsulate-class jazz.Special-Form)


;;;
;;;; Macro Form
;;;


(jazz.define-class-runtime jazz.Macro-Form)


(define (jazz.new-macro-form name expander)
  (jazz.allocate-macro-form jazz.Macro-Form name #f expander))


(jazz.define-method (jazz.walk-binding-expandable? (jazz.Macro-Form binding))
  #t)


(jazz.define-method (jazz.walk-binding-expand-form (jazz.Macro-Form binding) walker resume declaration environment form-src)
  (let ((form (%%desourcify form-src)))
    (let ((expander (%%get-macro-form-expander binding)))
      (apply expander walker resume declaration environment (%%cdr form)))))


(jazz.encapsulate-class jazz.Macro-Form)


;;;
;;;; Syntax Form
;;;


(jazz.define-class-runtime jazz.Syntax-Form)


(define (jazz.new-syntax-form name expander)
  (jazz.allocate-syntax-form jazz.Syntax-Form name #f expander))


(jazz.define-method (jazz.walk-binding-expandable? (jazz.Syntax-Form binding))
  #t)


(jazz.define-method (jazz.walk-binding-expand-form (jazz.Syntax-Form binding) walker resume declaration environment form-src)
  (let ((expander (%%get-syntax-form-expander binding)))
    (expander walker resume declaration environment form-src)))


(jazz.encapsulate-class jazz.Syntax-Form)


;;;
;;;; Annotated Variable
;;;


(jazz.define-class-runtime jazz.Annotated-Variable)


(define (jazz.new-annotated-variable variable declared-type type)
  (jazz.allocate-annotated-variable jazz.Annotated-Variable variable declared-type type))


(jazz.encapsulate-class jazz.Annotated-Variable)


;;;
;;;; Restricted Binding
;;;


(jazz.define-class-runtime jazz.Restricted-Binding)


(define (jazz.new-restricted-binding binding type)
  (jazz.allocate-restricted-binding jazz.Restricted-Binding binding type))


(jazz.encapsulate-class jazz.Restricted-Binding)


;;;
;;;; Annotated Frame
;;;


(jazz.define-class-runtime jazz.Annotated-Frame)


(define (jazz.new-annotated-frame variables reset)
  (jazz.allocate-annotated-frame jazz.Annotated-Frame variables reset))


(jazz.encapsulate-class jazz.Annotated-Frame)


;; put those in a cond-expand when cond-expand supports multiple features
(define (jazz.inspect-annotated-variable variable)
  (let ((serial (jazz.object->serial-symbol variable)))
    (if (%%class-is? variable jazz.Restricted-Binding)
        `(:restricted
          ,(%%get-lexical-binding-name (%%get-restricted-binding-binding variable))
          ,(%%get-restricted-binding-type variable) serial)
      `(:variable
        ,(%%get-lexical-binding-name (%%get-annotated-variable-variable variable))
        ,(%%get-annotated-variable-type variable) serial))))


(define (jazz.inspect-annotated-frame frame)
  `(:frame
    ,@(map jazz.inspect-annotated-variable (%%get-annotated-frame-variables frame))))


(define (jazz.inspect-annotated-environment environment)
  `(:environment
    ,@(map jazz.inspect-annotated-frame environment)))


;;;
;;;; Code
;;;


(jazz.define-class-runtime jazz.Code)


(define (jazz.new-code form type source)
  (jazz.allocate-code jazz.Code form type source))


(jazz.encapsulate-class jazz.Code)


(define (jazz.codes-forms codes)
  (map (lambda (code)
         (jazz.sourcified-form code))
       codes))


(define (jazz.codes-types codes)
  (map (lambda (code)
         (%%get-code-type code))
       codes))


;; this approach is clearly costly in memory and is just to experiment
(define (jazz.sourcify-code code src)
  #;
  code
  (if (or (%%not src) (%%not (%%source? src)))
      code
    (jazz.new-code (%%get-code-form code)
                   (%%get-code-type code)
                   src)))


;; temp try
(define (jazz.sourcified-form code)
  #;
  (%%get-code-form code)
  (let ((form (%%get-code-form code))
        (src (%%get-code-source code)))
    (jazz.sourcify-if form src)))


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
    (continuation-capture
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
                          (if (and (%%class-is? binding jazz.Declaration) (%%eq? binding variable))
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
              (continuation-return reset #f))))))))


(define (jazz.extend-type type1 type2)
  (cond ((or (%%not type1) (%%not type2))
         jazz.Any)
        ;; the void test should be integrated in the type framework
        ((or (%%eq? type1 jazz.Void) (%%subtype? type1 type2))
         type2)
        ;; the void test should be integrated in the type framework
        ((or (%%eq? type2 jazz.Void) (%%subtype? type2 type1))
         type1)
        (else
         ;; should find the most specific common supertype
         jazz.Any)))


(define (jazz.extend-types types)
  (if (%%null? types)
      jazz.Void
    (jazz.extend-type (%%car types) (jazz.extend-types (%%cdr types)))))


(define (jazz.type-union types)
  (jazz.new-union-type types))


(define (jazz.type-difference type1 type2)
  #f)


;;;
;;;; Walker
;;;


(jazz.define-class-runtime jazz.Walker)


;;;
;;;; Problems
;;;


(define (jazz.walk-warning walker declaration fmt-string . rest)
  (let ((location (jazz.walk-location walker declaration #f))
        (message (apply jazz.format fmt-string rest)))
    (jazz.walker-warning walker (jazz.new-walk-warning location message))))


(define (jazz.walk-error walker resume declaration fmt-string . rest)
  (let ((location (jazz.walk-location walker declaration #f))
        (message (apply jazz.format fmt-string rest)))
    (jazz.walker-error walker resume (jazz.new-walk-error location message))))


(define (jazz.walk-unresolved walker resume declaration symbol-src)
  (let ((location (jazz.walk-location walker declaration (if (%%source? symbol-src) (%%source-locat symbol-src) #f))))
    (jazz.walker-error walker resume (jazz.new-unresolved-error location (%%source-code symbol-src)))))


(define (jazz.walker-warning walker warning)
  (if (jazz.warnings?)
      (%%set-walker-warnings walker (%%append (%%get-walker-warnings walker) (%%list warning)))))


(define (jazz.walker-error walker resume error)
  (%%set-walker-errors walker (%%append (%%get-walker-errors walker) (%%list error)))
  (if (and resume (jazz.delay-reporting?))
      (continuation-return resume (jazz.unspecified))
    (jazz.validate-walk-problems walker)))


(define (jazz.validate-walk-problems walker)
  (let ((warnings (%%get-walker-warnings walker))
        (errors (%%get-walker-errors walker)))
    (%%when (or (%%not-null? warnings) (%%not-null? errors))
      (let ((output (open-output-string))
            (all (%%append warnings errors)))
        (jazz.format output "Walk problems encountered:{%}")
        (for-each (lambda (partition)
                    (jazz.bind (module-locator . problems) partition
                      (jazz.format output "  In {a}" (or module-locator "<console>"))
                      (let ((prefix (if (%%not module-locator) -1 (%%string-length (%%symbol->string module-locator)))))
                        (for-each (lambda (partition)
                                    (jazz.bind (declaration-locator . problems) partition
                                      (let ((toplevel? (%%fx= (%%string-length declaration-locator) prefix)))
                                        (if (%%not toplevel?)
                                            (jazz.format output "{%}    At {a}"
                                              (%%substring declaration-locator (%%fx+ prefix 1) (%%string-length declaration-locator))))
                                        (for-each (lambda (problem)
                                                    (jazz.format output "{%}{a}    {a}"
                                                      (if toplevel? "" "  ")
                                                      (jazz.present-exception problem)))
                                                  problems))))
                                  (jazz.partition-walk-problems-declaration problems)))))
                  (jazz.partition-walk-problems-module all))
        (let ((message (get-output-string output)))
          (raise (jazz.new-walk-problems message warnings errors)))))))


(define (jazz.partition-walk-problems-module problems)
  (jazz.partition problems
                  (lambda (problem)
                    (%%get-walk-location-module-locator (%%get-walk-problem-location problem)))
                  assv))


(define (jazz.partition-walk-problems-declaration problems)
  (jazz.partition problems
                  (lambda (problem)
                    (%%symbol->string (%%get-walk-location-declaration-locator (%%get-walk-problem-location problem))))
                  assoc))


;;;
;;;; Parse
;;;


(define (jazz.parse-modifiers walker resume declaration infos rest)
  (let ((partitions (map (lambda (info) (%%cons info '())) infos))
        (done? #f))
    (%%while (and (%%not-null? rest) (%%not done?))
      (let ((target (%%desourcify (%%car rest)))
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
  (let ((name (%%source-code (%%car partial-form)))
        (dialect-name (%%source-code (%%cadr partial-form)))
        (scan (%%cddr partial-form))
        (requires '())
        (exports '())
        (imports '()))
    (if (and (%%pair? scan)
             (%%pair? (%%source-code (%%car scan)))
             (%%eq? (%%source-code (%%car (%%source-code (%%car scan)))) 'require))
        (begin
          (set! requires (%%cdr (%%desourcify (%%car scan))))
          (set! scan (%%cdr scan))))
    (if (and (%%pair? scan)
             (%%pair? (%%source-code (%%car scan)))
             (%%eq? (%%source-code (%%car (%%source-code (%%car scan)))) 'export))
        (begin
          (set! exports (%%cdr (%%desourcify (%%car scan))))
          (set! scan (%%cdr scan))))
    (if (and (%%pair? scan)
             (%%pair? (%%source-code (%%car scan)))
             (%%eq? (%%source-code (%%car (%%source-code (%%car scan)))) 'import))
        (begin
          (set! imports (%%cdr (%%desourcify (%%car scan))))
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
      (parameterize ((jazz.walk-context (jazz.new-walk-context #f name #f)))
        (let* ((dialect-invoice (jazz.load-dialect-invoice dialect-name))
               (dialect (jazz.require-dialect dialect-name))
               (walker (jazz.dialect-walker dialect)))
          (jazz.walk-library-declaration walker #f name dialect-name dialect-invoice requires exports imports body))))))


(define (jazz.walk-library-declaration walker actual name dialect-name dialect-invoice requires exports imports body)
  (let ((exports (jazz.walk-library-exports walker exports))
        (imports (jazz.walk-library-imports walker imports)))
    (let ((new-declaration (jazz.new-library-declaration name #f dialect-name dialect-invoice requires exports imports)))
      (jazz.load-library-syntax new-declaration)
      (jazz.setup-library-lookups new-declaration)
      (let ((declaration (jazz.merge-declarations actual new-declaration)))
        (jazz.walk-declarations walker #f declaration (%%cons declaration (jazz.walker-environment walker)) body)
        (jazz.validate-walk-problems walker)
        declaration))))


(define (jazz.load-library-syntax declaration)
  (for-each (lambda (spec)
              (jazz.parse-require spec
                (lambda (module-name feature-requirement phase)
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
  (let ((partition (jazz.partition exports symbol? assv)))
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
      (parameterize ((jazz.walk-context (jazz.new-walk-context #f name #f)))
        (let* ((dialect-invoice (jazz.load-dialect-invoice dialect-name))
               (dialect (jazz.require-dialect dialect-name))
               (walker (jazz.dialect-walker dialect))
               (resume #f)
               (actual (jazz.get-catalog-entry name))
               (declaration (jazz.call-with-catalog-entry-lock name
                              (lambda ()
                                (let ((declaration (jazz.walk-library-declaration walker actual name dialect-name dialect-invoice requires exports imports (jazz.desourcify-list body))))
                                  (jazz.set-catalog-entry name declaration)
                                  declaration))))
               (environment (%%cons declaration (jazz.walker-environment walker)))
               (body (jazz.walk-namespace walker resume declaration environment body)))
          (jazz.validate-walk-problems walker)
          (%%set-namespace-declaration-body declaration body)
          declaration)))))


(define (jazz.parse-module rest proc)
  (if (and (%%pair? rest)
           (%%pair? (%%source-code (%%car rest)))
           (%%eq? (%%source-code (%%car (%%source-code (%%car rest)))) 'require))
      (proc (jazz.filter-features (%%cdr (%%desourcify (%%car rest)))) (%%cdr rest))
    (proc '() rest)))


(define (jazz.cond-expand form-src cont)
  (if (and (%%pair? (%%source-code form-src))
           (%%eq? (%%source-code (%%car (%%source-code form-src))) 'cond-expand))
      (let iter ((scan (%%cdr (%%desourcify form-src))))
        (if (%%null? scan)
            (jazz.error "Unfulfilled cond-expand")
          (let ((clause (%%car scan)))
            (if (or (%%not (%%pair? clause))
                    (%%not (%%symbol? (%%car clause))))
                (jazz.error "Ill-formed cond-expand clause: {s}" clause)
              (let ((feature-requirement (%%car clause)))
                (if (or (jazz.feature-safisfied? feature-requirement)
                        (%%eq? feature-requirement 'else))
                    (if (%%null? (%%cdr clause))
                        (cont #f #f)
                      (cont (%%cadr clause) #t))
                  (iter (%%cdr scan))))))))
    (cont form-src #t)))


(define (jazz.walk-namespace walker resume declaration environment form-list)
  (let ((queue (jazz.new-queue)))
    (for-each (lambda (form-src)
                (continuation-capture
                  (lambda (resume)
                    (jazz.cond-expand form-src
                      (lambda (expr-src expr?)
                        (%%when expr?
                          (jazz.enqueue queue (jazz.walk walker resume declaration environment expr-src))))))))
              form-list)
    (jazz.queue-list queue)))


(define (jazz.load-dialect-invoice dialect-name)
  (if (%%not (%%symbol? dialect-name))
      (jazz.error "Dialect name must be a symbol: {s}" dialect-name)
    (if (%%eq? dialect-name 'core)
        #f
      (jazz.new-import-invoice
        (jazz.locate-library-declaration dialect-name)
        'syntax
        #f
        #f))))


(define (jazz.expand-library-references library-declaration)
  (let ((queue (jazz.new-queue)))
    (letrec ((collect-declarations
              (lambda (declaration)
                (for-each collect-declarations (jazz.get-declaration-references declaration))
                (%%when (%%not (%%memq declaration (jazz.queue-list queue)))
                  (jazz.enqueue queue declaration)))))
      (for-each collect-declarations (%%get-library-declaration-references library-declaration))
      (map (lambda (declaration)
             (jazz.expand-referenced-declaration declaration))
           (jazz.queue-list queue)))))


(define (jazz.expand-library-literals library-declaration)
  (map (lambda (info)
         (let ((name (%%cadr info))
               (value (%%cddr info)))
           `(define ,name ,(jazz.sourcified-form (jazz.emit-expression value library-declaration '())))))
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
                    ,(jazz.sourcified-form (jazz.emit-binding-reference referenced-declaration library-declaration environment))))))))
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


(jazz.define-virtual-runtime (jazz.walker-environment (jazz.Walker walker)))


(jazz.define-method (jazz.walker-environment (jazz.Walker walker))
  (%%list (jazz.new-walk-frame (jazz.core-bindings))))


;;;
;;;; Declaration
;;;


;; In order to be able to resolve internal declarations as we walk the code, the declaration
;; tree is pre-expanded including expanding macros when needed. Then, during the actual walk
;; we just find the declarations in this tree. This tree is also merged with any preexisting
;; declaration tree coming from the runtime catalog.


(jazz.define-virtual-runtime (jazz.walk-declaration (jazz.Walker walker) resume declaration environment form))


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
                (continuation-capture
                  (lambda (resume)
                    (jazz.cond-expand form
                      (lambda (expr expr?)
                        (%%when expr?
                          (let ((expansion (jazz.expand-macros walker resume declaration environment expr)))
                            (if (jazz.begin-form? expansion)
                                (walk (%%cdr expansion))
                              (jazz.walk-declaration walker resume declaration environment expansion)))))))))
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
  (and (%%pair? (%%source-code form))
       (%%eq? (%%source-code (%%car (%%source-code form))) 'begin)))


(define (jazz.define-form? form)
  (and (%%pair? (%%source-code form))
       (%%eq? (%%source-code (%%car (%%source-code form))) 'define)))


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


(jazz.define-class-runtime jazz.Expression)


(jazz.define-virtual-runtime (jazz.emit-expression (jazz.Expression expression) declaration environment))


(jazz.define-method (jazz.emit-expression (jazz.Expression expression) declaration environment)
  (jazz.error "Unable to emit code for: {s}" expression))


(jazz.define-virtual-runtime (jazz.emit-call (jazz.Expression expression) arguments declaration environment))


(jazz.define-method (jazz.emit-call (jazz.Expression expression) arguments declaration environment)
  (jazz.new-code
    `(,(jazz.sourcified-form (jazz.emit-expression expression declaration environment)) ,@(jazz.codes-forms arguments))
    jazz.Any
    #f))


(jazz.define-virtual-runtime (jazz.fold-expression (jazz.Expression expression) f k s))


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


(jazz.define-class-runtime jazz.Proclaim)


(define (jazz.new-proclaim optimize?)
  (jazz.allocate-proclaim jazz.Proclaim #f #f optimize?))


(jazz.define-method (jazz.emit-expression (jazz.Proclaim expression) declaration environment)
  (let ((library-declaration (%%get-declaration-toplevel declaration)))
    (%%set-library-declaration-declares library-declaration (%%get-proclaim-optimize? expression)))
  #f)


(jazz.encapsulate-class jazz.Proclaim)


;;;
;;;; Constant
;;;


(jazz.define-class-runtime jazz.Constant)


(define (jazz.new-constant expansion type)
  (jazz.allocate-constant jazz.Constant type #f expansion))


(jazz.define-method (jazz.emit-expression (jazz.Constant expression) declaration environment)
  (jazz.new-code
    (%%get-constant-expansion expression)
    (%%get-expression-type expression)
    #f))


(jazz.define-method (jazz.fold-expression (jazz.Constant expression) f k s)
  (f expression
     (k (%%get-constant-expansion expression)
        s)))


(jazz.encapsulate-class jazz.Constant)


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


(jazz.encapsulate-class jazz.Delay)


;;;
;;;; Quasiquote
;;;


(jazz.define-class-runtime jazz.Quasiquote)


(define (jazz.new-quasiquote form)
  (jazz.allocate-quasiquote jazz.Quasiquote #f #f form))


(jazz.define-method (jazz.emit-expression (jazz.Quasiquote expression) declaration environment)
  (letrec ((emit
            (lambda (form)
              (if (%%pair? form)
                  (if (or (%%eq? (%%car form) 'unquote)
                          (%%eq? (%%car form) 'unquote-splicing))
                      (%%list (%%car form) (jazz.sourcified-form (jazz.emit-expression (%%cadr form) declaration environment)))
                    (%%cons (emit (%%car form)) (emit (%%cdr form))))
                form))))
    (jazz.new-code
      (%%list 'quasiquote (emit (%%get-quasiquote-form expression)))
      jazz.List
      #f)))


(jazz.encapsulate-class jazz.Quasiquote)


;;;
;;;; Reference
;;;


(jazz.define-class-runtime jazz.Reference)


(define (jazz.new-reference binding)
  (jazz.allocate-reference jazz.Reference #f #f binding))


(jazz.define-method (jazz.emit-expression (jazz.Reference expression) declaration environment)
  (jazz.emit-binding-reference (%%get-reference-binding expression) declaration environment))


(jazz.define-method (jazz.emit-call (jazz.Reference expression) arguments declaration environment)
  (jazz.emit-binding-call (%%get-reference-binding expression) arguments declaration environment))


(jazz.encapsulate-class jazz.Reference)


;;;
;;;; Assignment
;;;


(jazz.define-class-runtime jazz.Assignment)


(define (jazz.new-assignment binding value)
  (jazz.allocate-assignment jazz.Assignment #f #f binding value))


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


(jazz.define-class-runtime jazz.Lambda)


(define (jazz.new-lambda type signature body)
  (jazz.allocate-lambda jazz.Lambda type #f signature body))


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
                   (let ()
                     ,(jazz.simplify-begin (jazz.emit-type-cast (jazz.new-code `(begin ,@(jazz.sourcified-form body-code)) (%%get-code-type body-code) #f) type declaration environment))))
                (jazz.new-function-type '() '() '() #f (%%get-code-type body-code))
                #f))))))))


(jazz.define-method (jazz.fold-expression (jazz.Lambda expression) f k s)
  (f expression
     (k (%%get-lambda-signature expression)
        (k (jazz.fold-statement (%%get-lambda-body expression) f k s)
           s))))


(jazz.encapsulate-class jazz.Lambda)


;;;
;;;; Let
;;;


(jazz.define-class-runtime jazz.Let)


(define (jazz.new-let bindings body)
  (jazz.allocate-let jazz.Let #f #f bindings body))


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
                   ,@(jazz.sourcified-form body-code))
                (%%get-code-type body-code)
                #f))))))))


(jazz.define-method (jazz.fold-expression (jazz.Let expression) f k s)
  (f expression
     (k (jazz.fold-expression (%%get-let-body expression) f k s)
        s)))


(jazz.encapsulate-class jazz.Let)


;;;
;;;; Named Let
;;;


(jazz.define-class-runtime jazz.Named-Let)


(define (jazz.new-named-let variable bindings body)
  (jazz.allocate-named-let jazz.Named-Let #f #f bindings body variable))


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
                   ,@(jazz.sourcified-form body-code))
                (%%get-code-type body-code)
                #f))))))))


(jazz.define-method (jazz.fold-expression (jazz.Named-Let expression) f k s)
  (f expression
     (k (jazz.fold-expression (%%get-let-body expression) f k s)
        s)))


(jazz.encapsulate-class jazz.Named-Let)


;;;
;;;; Letstar
;;;


(jazz.define-class-runtime jazz.Letstar)


(define (jazz.new-letstar bindings body)
  (jazz.allocate-letstar jazz.Letstar #f #f bindings body))


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
                   ,@(jazz.sourcified-form body-code))
                (%%get-code-type body-code)
                #f))))))))


(jazz.define-method (jazz.fold-expression (jazz.Letstar expression) f k s)
  (f expression
     (k (jazz.fold-expression (%%get-letstar-body expression) f k s)
        s)))


(jazz.encapsulate-class jazz.Letstar)


;;;
;;;; Letrec
;;;


(jazz.define-class-runtime jazz.Letrec)


(define (jazz.new-letrec bindings body)
  (jazz.allocate-letrec jazz.Letrec #f #f bindings body))


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
                   ,@(jazz.sourcified-form body-code))
                (%%get-code-type body-code)
                #f))))))))


(jazz.define-method (jazz.fold-expression (jazz.Letrec expression) f k s)
  (f expression
     (k (jazz.fold-expression (%%get-letrec-body expression) f k s)
        s)))


(jazz.encapsulate-class jazz.Letrec)


;;;
;;;; Receive
;;;


(jazz.define-class-runtime jazz.Receive)


(define (jazz.new-receive variables expression body)
  (jazz.allocate-receive jazz.Receive #f #f variables expression body))


(jazz.define-method (jazz.emit-expression (jazz.Receive expression) declaration environment)
  (let ((variables (%%get-receive-variables expression))
        (expression (%%get-receive-expression expression))
        (body (%%get-receive-body expression)))
    (jazz.with-annotated-frame (jazz.annotate-receive variables)
      (lambda (frame)
        (let ((augmented-environment (cons frame environment)))
          (let ((expression-output (jazz.sourcified-form (jazz.emit-expression expression declaration environment))))
            (let ((body-code (jazz.emit-expression body declaration augmented-environment)))
              (jazz.new-code
                `(receive ,(map (lambda (variable)
                                  (%%get-lexical-binding-name variable))
                                variables)
                   ,expression-output
                   ,@(jazz.sourcified-form body-code))
                (%%get-code-type body-code)
                #f))))))))


(jazz.define-method (jazz.fold-expression (jazz.Receive expression) f k s)
  (f expression
     (k (jazz.fold-expression (%%get-receive-body expression) f k s)
        s)))


(jazz.encapsulate-class jazz.Receive)


;;;
;;;; Body
;;;


(jazz.define-class-runtime jazz.Body)


(define (jazz.new-body internal-defines expressions)
  (jazz.allocate-body jazz.Body #f #f internal-defines expressions))


(jazz.define-method (jazz.emit-expression (jazz.Body expression) declaration environment)
  (let ((internal-defines (%%get-body-internal-defines expression))
        (expressions (%%get-body-expressions expression)))
    (jazz.with-annotated-frame (jazz.annotate-internal-defines internal-defines)
      (lambda (frame)
        (let ((augmented-environment (cons frame environment)))
          (jazz.new-code
            (append (jazz.codes-forms (jazz.emit-expressions internal-defines declaration augmented-environment))
                    (jazz.codes-forms (jazz.emit-expressions expressions declaration augmented-environment)))
            jazz.Any
            #f))))))


(jazz.define-method (jazz.fold-expression (jazz.Body expression) f k s)
  (f expression
     (jazz.fold-statements (%%get-body-internal-defines expression) f k s
       (jazz.fold-statements (%%get-body-expressions expression) f k s s))))


(jazz.encapsulate-class jazz.Body)


;;;
;;;; Internal-Define
;;;


(jazz.define-class-runtime jazz.Internal-Define)


(define (jazz.new-internal-define variable value)
  (jazz.allocate-internal-define jazz.Internal-Define #f #f variable value))


(jazz.define-method (jazz.emit-expression (jazz.Internal-Define expression) declaration environment)
  (let ((variable (%%get-internal-define-variable expression))
        (value (%%get-internal-define-value expression)))
    (jazz.new-code
      `(define ,(%%get-lexical-binding-name variable)
         ,(jazz.sourcified-form (jazz.emit-expression value declaration environment)))
      jazz.Any
      #f)))


(jazz.define-method (jazz.fold-expression (jazz.Internal-Define expression) f k s)
  (f expression
     (k (jazz.fold-statement (%%get-internal-define-value expression) f k s)
        s)))


(jazz.encapsulate-class jazz.Internal-Define)


;;;
;;;; Begin
;;;


(jazz.define-class-runtime jazz.Begin)


(define (jazz.new-begin expressions)
  (jazz.allocate-begin jazz.Begin #f #f expressions))


(jazz.define-method (jazz.emit-expression (jazz.Begin expression) declaration environment)
  (let ((expressions (%%get-begin-expressions expression)))
    (let ((code (jazz.emit-statements-code expressions declaration environment)))
      (jazz.new-code
        `(begin ,@(jazz.sourcified-form code))
        (%%get-code-type code)
        #f))))


(jazz.define-method (jazz.fold-expression (jazz.Begin expression) f k s)
  (f expression
     (jazz.fold-expressions (%%get-begin-expressions expression) f k s s)))


(jazz.encapsulate-class jazz.Begin)


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
              (augmented-environment (cons frame environment)))
          (let ((bindings-output
                  (map (lambda (binding annotated-variable)
                         (let ((variable (%%car binding))
                               (init (%%cadr binding))
                               (step (%%cddr binding)))
                           (let ((init-code (jazz.sourcified-form (jazz.emit-expression init declaration augmented-environment)))
                                 (step-code-list (if step (list (jazz.sourcified-form (jazz.emit-expression step declaration augmented-environment))) '())))
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
                     (,(jazz.sourcified-form test-code) ,@(jazz.sourcified-form result-code))
                   ,@(jazz.sourcified-form body-code))
                (%%get-code-type result-code)
                #f))))))))


(jazz.encapsulate-class jazz.Do)


;;;
;;;; Call
;;;


(jazz.define-class-runtime jazz.Call)


(define (jazz.new-call source operator arguments)
  (jazz.allocate-call jazz.Call #f source operator arguments))


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
      (jazz.sourcify-code
        (or (jazz.emit-specialized-call operator locator arguments arguments-codes declaration environment)
            (jazz.emit-primitive-new-call operator locator arguments arguments-codes declaration environment)
            (jazz.emit-primitive-call operator locator arguments arguments-codes declaration environment)
            (jazz.emit-inlined-call operator arguments-codes declaration environment)
            (jazz.emit-call operator arguments-codes declaration environment))
        (%%get-expression-source expression)))))


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
                          (%%when (and (jazz.warnings?) (%%not (%%null? specializers)) (%%get-library-declaration-declares (%%get-declaration-toplevel declaration))
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
                          (%%when (%%memq (%%get-lexical-binding-name binding) (jazz.debug-specializers))
                            (jazz.debug 'Warning: 'In (%%get-declaration-locator declaration) 'unable 'to 'match 'call 'to 'specialized (%%get-lexical-binding-name binding) 'on types))
                          #f)
                      (let ((specializer (%%car scan)))
                        (let ((function-type (%%get-lexical-binding-type specializer)))
                          (if (jazz.match-signature? arguments types function-type)
                              (or (jazz.emit-inlined-binding-call specializer arguments-codes declaration environment)
                                  (jazz.new-code
                                    (let ((locator (%%get-declaration-locator specializer)))
                                      `(,locator ,@(jazz.codes-forms arguments-codes)))
                                    (%%get-function-type-result function-type)
                                    #f))
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

;; only done in release as these can crash on a division by zero
(cond-expand
  (release
    (jazz.add-primitive-patterns 'scheme.dialect.kernel./        '(                     (##fl/  <fl^fl*:fl>)                     (##/ <number^number:number>)))
    (jazz.add-primitive-patterns 'scheme.dialect.kernel.quotient '((##fxquotient <fx^fx:fx>))))
  (else))

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
                (%%when (and (jazz.warnings?) (%%not (%%null? patterns)) (%%get-library-declaration-declares (%%get-declaration-toplevel declaration))
                             ;; a bit extreme for now
                             (%%not (%%memq locator '(scheme.dialect.kernel.car
                                                      scheme.dialect.kernel.cdr))))
                  (jazz.debug 'Warning: 'In (%%get-declaration-locator declaration) 'unable 'to 'match 'call 'to 'primitive (jazz.identifier-name locator)))
                #f)
            (jazz.bind (name function-type) (%%car scan)
              (if (jazz.match-signature? arguments types function-type)
                  (jazz.new-code
                    `(,name ,@(jazz.codes-forms arguments-codes))
                    (%%get-function-type-result function-type)
                    #f)
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


(jazz.define-class-runtime jazz.If)


(define (jazz.new-if test yes no)
  (jazz.allocate-if jazz.If #f #f test yes no))


(define jazz.type-tests
  (%%make-table test: eq?))


(%%table-set! jazz.type-tests 'scheme.dialect.kernel.number?       jazz.Number)
(%%table-set! jazz.type-tests 'scheme.dialect.kernel.complex?      jazz.Complex)
(%%table-set! jazz.type-tests 'scheme.dialect.kernel.real?         jazz.Real)
(%%table-set! jazz.type-tests 'scheme.dialect.kernel.rational?     jazz.Rational)
(%%table-set! jazz.type-tests 'scheme.dialect.kernel.integer?      jazz.Integer)
(%%table-set! jazz.type-tests 'scheme.dialect.kernel.number?       jazz.Number)
(%%table-set! jazz.type-tests 'scheme.dialect.kernel.boolean?      jazz.Boolean)
;; not 100% correct because of Scheme's semantic for list?
(%%table-set! jazz.type-tests 'scheme.dialect.kernel.list?         jazz.List)
(%%table-set! jazz.type-tests 'scheme.dialect.kernel.null?         jazz.Null)
(%%table-set! jazz.type-tests 'scheme.dialect.kernel.pair?         jazz.Pair)
(%%table-set! jazz.type-tests 'scheme.dialect.kernel.symbol?       jazz.Symbol)
(%%table-set! jazz.type-tests 'scheme.dialect.kernel.char?         jazz.Char)
(%%table-set! jazz.type-tests 'scheme.dialect.kernel.string?       jazz.String)
(%%table-set! jazz.type-tests 'scheme.dialect.kernel.vector?       jazz.Vector)
(%%table-set! jazz.type-tests 'jazz.dialect.kernel.u8vector?       jazz.U8Vector)
(%%table-set! jazz.type-tests 'jazz.dialect.kernel.continuation?   jazz.Continuation)
(%%table-set! jazz.type-tests 'scheme.dialect.kernel.procedure?    jazz.Procedure)
(%%table-set! jazz.type-tests 'scheme.dialect.kernel.input-port?   jazz.Port)
(%%table-set! jazz.type-tests 'scheme.dialect.kernel.output-port?  jazz.Port)
(%%table-set! jazz.type-tests 'scheme.dialect.kernel.eof-object?   jazz.EOF)
(%%table-set! jazz.type-tests 'jazz.dialect.kernel.fixnum?         jazz.Fixnum)
(%%table-set! jazz.type-tests 'jazz.dialect.kernel.flonum?         jazz.Flonum)
(%%table-set! jazz.type-tests 'jazz.dialect.kernel.keyword?        jazz.Keyword)
(%%table-set! jazz.type-tests 'jazz.dialect.kernel.object?         jazz.Object)
(%%table-set! jazz.type-tests 'jazz.dialect.kernel.category?       jazz.Category)
(%%table-set! jazz.type-tests 'jazz.dialect.kernel.class?          jazz.Class)
(%%table-set! jazz.type-tests 'jazz.dialect.kernel.interface?      jazz.Interface)
(%%table-set! jazz.type-tests 'jazz.dialect.kernel.field?          jazz.Field)
(%%table-set! jazz.type-tests 'jazz.dialect.kernel.slot?           jazz.Slot)
(%%table-set! jazz.type-tests 'jazz.dialect.kernel.method?         jazz.Method)
(%%table-set! jazz.type-tests 'jazz.dialect.kernel.table?          jazz.Table)
(%%table-set! jazz.type-tests 'jazz.dialect.kernel.thread?         jazz.Thread)
(%%table-set! jazz.type-tests 'jazz.dialect.kernel.foreign?        jazz.Foreign)
(%%table-set! jazz.type-tests 'jazz.dialect.kernel.values?         jazz.Values)
(%%table-set! jazz.type-tests 'jazz.dialect.kernel.unspecified?    jazz.Unspecified)


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
                (%%cons yes no))))
        env)))
  
  (define (extract-binding expr env)
    (if (%%class-is? expr jazz.Reference)
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
           (receive (origin actual-type) (extract-binding expr env)
             (if origin
                 (if (%%class-is? actual-type jazz.Nillable-Type)
                     (let ((yes (cons (jazz.new-annotated-frame (list (jazz.new-restricted-binding origin (%%get-nillable-type-type actual-type))) #f) (%%car env)))
                           (no (%%cdr env)))
                       (%%cons yes no))
                   env)
               env)))))
  
  (process-expr test (cons environment environment)))


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
          #f)))))


(jazz.define-method (jazz.fold-expression (jazz.If expression) f k s)
  (f expression
     (k (jazz.fold-expression (%%get-if-test expression) f k s)
        (k (jazz.fold-expression (%%get-if-yes expression) f k s)
           (jazz.fold-expressions (%%get-if-no expression) f k s s)))))


(jazz.encapsulate-class jazz.If)


;;;
;;;; Cond
;;;


(jazz.define-class-runtime jazz.Cond)


(define (jazz.new-cond clauses)
  (jazz.allocate-cond jazz.Cond #f #f clauses))


(jazz.define-method (jazz.emit-expression (jazz.Cond expression) declaration environment)
  (let ((clauses (%%get-cond-clauses expression)))
    (jazz.new-code
      `(cond ,@(let recurse ((clauses clauses)
                             (environment environment))
                    (if (null? clauses) '()
                      (let ((clause (car clauses)))
                        (let ((test (%%car clause))
                              (body (%%cdr clause)))
                          (jazz.bind (yes-environment . no-environment) (jazz.branch-types test environment)
                            (let ((output
                                    `(,(if (%%not test)
                                           'else
                                         (jazz.sourcified-form (jazz.emit-expression test declaration environment)))
                                      ,(jazz.sourcified-form (jazz.emit-expression body declaration yes-environment)))))
                              (cons output (recurse (cdr clauses) no-environment)))))))))
      (jazz.extend-types (map (lambda (clause)
                                (%%get-code-type
                                  (let ((body (%%cdr clause)))
                                    (jazz.emit-expression body declaration environment))))
                              clauses))
      #f)))


(jazz.define-method (jazz.fold-expression (jazz.Cond expression) f k s)
  #f)


(jazz.encapsulate-class jazz.Cond)


;;;
;;;; Case
;;;


(jazz.define-class-runtime jazz.Case)


(define (jazz.new-case target clauses)
  (jazz.allocate-case jazz.Case #f #f target clauses))


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
        #f))))


(jazz.define-method (jazz.fold-expression (jazz.Case expression) f k s)
  #f)


(jazz.encapsulate-class jazz.Case)


;;;
;;;; And
;;;


(jazz.define-class-runtime jazz.And)


(define (jazz.new-and expressions)
  (jazz.allocate-and jazz.And #f #f expressions))


(jazz.define-method (jazz.emit-expression (jazz.And expression) declaration environment)
  (jazz.new-code
    `(and ,@(jazz.codes-forms (jazz.emit-expressions (%%get-and-expressions expression) declaration environment)))
    jazz.Any
    #f))


(jazz.define-method (jazz.fold-expression (jazz.And expression) f k s)
  (f expression
     (jazz.fold-expressions (%%get-and-expressions expression) f k s s)))


(jazz.encapsulate-class jazz.And)


;;;
;;;; Or
;;;


(jazz.define-class-runtime jazz.Or)


(define (jazz.new-or expressions)
  (jazz.allocate-or jazz.Or #f #f expressions))


(jazz.define-method (jazz.emit-expression (jazz.Or expression) declaration environment)
  (jazz.new-code
    `(or ,@(jazz.codes-forms (jazz.emit-expressions (%%get-or-expressions expression) declaration environment)))
    jazz.Any
    #f))


(jazz.define-method (jazz.fold-expression (jazz.Or expression) f k s)
  (f expression
     (jazz.fold-expressions (%%get-or-expressions expression) f k s s)))


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
  #f)


(jazz.encapsulate-class jazz.Declare)


;;;
;;;; C Include
;;;


(jazz.define-class-runtime jazz.C-Include)


(define (jazz.new-c-include name)
  (jazz.allocate-c-include jazz.C-Include #f #f name))


(jazz.define-method (jazz.emit-expression (jazz.C-Include expression) declaration environment)
  (let ((name (%%get-c-include-name expression)))
    (jazz.new-code
      `(c-declare ,(%%string-append "#include " name))
      jazz.Any
      #f)))


(jazz.define-method (jazz.fold-expression (jazz.C-Include expression) f k s)
  #f)


(jazz.encapsulate-class jazz.C-Include)


;;;
;;;; C Declare
;;;


(jazz.define-class-runtime jazz.C-Declare)


(define (jazz.new-c-declare code)
  (jazz.allocate-c-declare jazz.C-Declare #f #f code))


(jazz.define-method (jazz.emit-expression (jazz.C-Declare expression) declaration environment)
  (let ((code (%%get-c-declare-code expression)))
    (jazz.new-code
      `(c-declare ,code)
      jazz.Any
      #f)))


(jazz.define-method (jazz.fold-expression (jazz.C-Declare expression) f k s)
  #f)


(jazz.encapsulate-class jazz.C-Declare)


;;;
;;;; C Named Declare
;;;


(jazz.define-class-runtime jazz.C-Named-Declare-Declaration)


(define (jazz.new-c-named-declare-declaration name type access compatibility attributes parent code)
  (let ((new-declaration (jazz.allocate-c-named-declare-declaration jazz.C-Named-Declare-Declaration name type access compatibility attributes #f parent #f #f code)))
    (jazz.setup-declaration new-declaration)
    new-declaration))


(jazz.define-method (jazz.emit-declaration (jazz.C-Named-Declare-Declaration declaration) environment)
  `(begin))


(jazz.define-method (jazz.expand-referenced-declaration (jazz.C-Named-Declare-Declaration declaration))
  (let ((code (%%get-c-named-declare-declaration-code declaration)))
    `(c-declare ,code)))


(jazz.encapsulate-class jazz.C-Named-Declare-Declaration)


;;;
;;;; C Initialize
;;;


(jazz.define-class-runtime jazz.C-Initialize)


(define (jazz.new-c-initialize code)
  (jazz.allocate-c-initialize jazz.C-Initialize #f #f code))


(jazz.define-method (jazz.emit-expression (jazz.C-Initialize expression) declaration environment)
  (let ((code (%%get-c-initialize-code expression)))
    (jazz.new-code
      `(c-initialize ,code)
      jazz.Any
      #f)))


(jazz.define-method (jazz.fold-expression (jazz.C-Initialize expression) f k s)
  #f)


(jazz.encapsulate-class jazz.C-Initialize)


;;;
;;;; C Function
;;;


(jazz.define-class-runtime jazz.C-Function)


(define (jazz.new-c-function expansion)
  (jazz.allocate-c-function jazz.C-Function #f #f expansion))


(jazz.define-method (jazz.emit-expression (jazz.C-Function expression) declaration environment)
  (jazz.new-code
    (%%get-c-function-expansion expression)
    jazz.Any
    #f))


(jazz.define-method (jazz.fold-expression (jazz.C-Function expression) f k s)
  #f)


(jazz.encapsulate-class jazz.C-Function)


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
  #f)


(jazz.encapsulate-class jazz.Parameterize)


;;;
;;;; Time Special
;;;


(jazz.define-class-runtime jazz.Time-Special)


(define (jazz.new-time-special expression)
  (jazz.allocate-time jazz.Time-Special #f #f expression))


(jazz.define-method (jazz.emit-expression (jazz.Time-Special expression) declaration environment)
  (let ((expression (%%get-time-special-expression expression)))
    (jazz.new-code
      `(time
         ,(jazz.sourcified-form (jazz.emit-expression expression declaration environment)))
      jazz.Any
      #f)))


(jazz.define-method (jazz.fold-expression (jazz.Time-Special expression) f k s)
  #f)


(jazz.encapsulate-class jazz.Time-Special)


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
                        (jazz.enqueue queue (jazz.sourcified-form code))))))
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
                       (jazz.sourcified-form code))))
                 statements)))
      (jazz.new-code emited last-type #f))))


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


#; ;; fix warnings
(define (ppl library-name)
  (pps (walk library-name)))


#; ;; fix warnings
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
    (jazz.fold-statement (if (integer? statement) (jazz.serial->object statement) statement)
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


(define (jazz.walk walker resume declaration environment form-src)
  (let ((form (%%source-code form-src)))
    (cond ((%%symbol? form)
           (jazz.walk-symbol walker resume declaration environment form-src))
          ((%%pair? form)
           (jazz.walk-form walker resume declaration environment form-src))
          (else
           (jazz.walk-constant walker resume declaration environment form-src)))))


(define (jazz.walk-list walker resume declaration environment form-list)
  (let ((queue (jazz.new-queue)))
    (for-each (lambda (form)
                (continuation-capture
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
                                    (%%cdr (%%source-code form)))
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
                            (let ((signature (%%cadr (%%desourcify internal-define))))
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


(define (jazz.walk-internal-define walker resume declaration environment form-src variable)
  (let ((form (%%desourcify form-src)))
    (receive (name specifier value parameters) (jazz.parse-define walker resume declaration (%%cdr (%%source-code form)))
      (let ((type (if specifier (jazz.walk-specifier walker resume declaration environment specifier) jazz.Any)))
        (jazz.new-internal-define variable (jazz.walk walker resume declaration environment value))))))


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


(define (jazz.walk-quote walker resume declaration environment form-src)
  (let ((form (%%desourcify form-src)))
    (let ((expression (%%cadr form)))
      (if (%%null? expression)
          (jazz.new-constant '(quote ()) jazz.Null)
        (jazz.walk-constant walker resume declaration environment expression)))))


(define (jazz.walk-keyword walker keyword)
  (jazz.new-constant keyword jazz.Keyword))


(define (jazz.walk-enumerator walker enumerator)
  (jazz.new-constant (%%list 'quote enumerator) jazz.Symbol))


(define (jazz.walk-constant walker resume declaration environment form-src)
  (let ((form (%%source-code form-src)))
    (cond ((%%boolean? form)
           (jazz.new-constant form-src jazz.Boolean))
          ((%%char? form)
           (jazz.new-constant form-src jazz.Char))
          ((%%string? form)
           (jazz.new-constant form-src jazz.String))
          ((%%keyword? form)
           (jazz.new-constant form-src jazz.Keyword))
          ((%%fixnum? form)
           (jazz.new-constant form-src jazz.Fixnum))
          ((%%flonum? form)
           (jazz.new-constant form-src jazz.Flonum))
          ((%%number? form)
           (jazz.new-constant form-src jazz.Number))
          ((%%symbol? form)
           (jazz.new-constant `(quote ,form-src) jazz.Symbol))
          ((%%vector? form)
           (jazz.new-constant `(quote ,form-src) jazz.Vector))
          ((%%u8vector? form)
           (jazz.new-constant `(quote ,form-src) jazz.U8Vector))
          ((%%values? form)
           (jazz.new-constant `(quote ,form-src) jazz.Values))
          ((%%null? form)
           (jazz.new-constant `(quote ,form-src) jazz.Null))
          ((jazz.scheme-pair-literal? form)
           (jazz.new-constant `(quote ,form-src) jazz.Pair))
          (else
           (jazz.register-literal walker resume declaration environment form)))))


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


(jazz.define-virtual-runtime (jazz.walk-symbol (jazz.Walker walker) resume declaration environment symbol-src))


(jazz.define-method (jazz.walk-symbol (jazz.Walker walker) resume declaration environment symbol-src)
  (let ((symbol (%%source-code symbol-src)))
    (cond ((jazz.enumerator? symbol)
           (jazz.walk-enumerator walker symbol))
          (else
           (jazz.walk-symbol-reference walker resume declaration environment symbol-src)))))


(define (jazz.walk-setbang walker resume declaration environment form-src)
  (let ((form (%%desourcify form-src)))
    (let ((form (%%cadr form))
          (value (%%car (%%cddr form))))
      (if (%%symbol? form)
          (jazz.walk-symbol-assignment walker resume declaration environment form value)
        (jazz.error "Illegal set! of {s}" form)))))


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


(define (jazz.lookup-accessible/compatible-symbol walker resume declaration environment symbol-src)
  (let ((referenced-declaration (jazz.lookup-symbol walker environment (%%source-code symbol-src))))
    (if (and referenced-declaration (%%class-is? referenced-declaration jazz.Declaration))
        (begin
          (jazz.validate-access walker resume declaration referenced-declaration)
          (jazz.validate-compatibility walker declaration referenced-declaration)))
    (if (%%class-is? referenced-declaration jazz.Autoload-Declaration)
        (let ((library (%%get-declaration-toplevel declaration)))
          (jazz.register-autoload-declaration library referenced-declaration)))
    referenced-declaration))


(jazz.define-virtual-runtime (jazz.validate-access (jazz.Walker walker) resume declaration referenced-declaration))


(jazz.define-method (jazz.validate-access (jazz.Walker walker) resume declaration referenced-declaration)
  (jazz.unspecified))


(define (jazz.validate-compatibility walker declaration referenced-declaration)
  (if (%%eq? (%%get-declaration-compatibility referenced-declaration) 'deprecated)
      (let ((referenced-locator (%%get-declaration-locator referenced-declaration)))
        (jazz.walk-warning walker declaration "Deprecated access to {s}" referenced-locator))))


;;;
;;;; Reference
;;;


(define (jazz.walk-symbol-reference walker resume declaration environment symbol-src)
  (let ((binding (jazz.lookup-accessible/compatible-symbol walker resume declaration environment symbol-src)))
    (if binding
        (begin
          (if (%%class-is? binding jazz.Variable)
              (jazz.walk-binding-referenced binding))
          (jazz.new-reference binding))
      (jazz.walk-free-reference walker resume declaration symbol-src))))


(jazz.define-virtual-runtime (jazz.walk-free-reference (jazz.Walker walker) resume declaration symbol-src))


(jazz.define-method (jazz.walk-free-reference (jazz.Walker walker) resume declaration symbol-src)
  (jazz.walk-unresolved walker resume declaration symbol-src))


;;;
;;;; Assignment
;;;


(jazz.define-virtual-runtime (jazz.walk-symbol-assignment (jazz.Walker walker) resume declaration environment symbol value))


(jazz.define-method (jazz.walk-symbol-assignment (jazz.Walker walker) resume declaration environment symbol value)
  (let ((binding (jazz.lookup-accessible/compatible-symbol walker resume declaration environment symbol)))
    (if binding
        (begin
          (jazz.walk-binding-validate-assignment binding walker resume declaration)
          (jazz.new-assignment binding (jazz.walk walker resume declaration environment value)))
      (jazz.walk-free-assignment walker resume declaration symbol))))


(jazz.define-virtual-runtime (jazz.walk-free-assignment (jazz.Walker walker) resume declaration symbol))


(jazz.define-method (jazz.walk-free-assignment (jazz.Walker walker) resume declaration symbol)
  (jazz.walk-unresolved walker resume declaration symbol))


;;;
;;;; Form
;;;


(jazz.define-virtual-runtime (jazz.walk-form (jazz.Walker walker) resume declaration environment form-src))


(jazz.define-method (jazz.walk-form (jazz.Walker walker) resume declaration environment form-src)
  (let ((form (%%source-code form-src)))
    (let ((procedure-expr (%%desourcify (%%car form))))
      (let ((binding (and (%%symbol? procedure-expr) (jazz.lookup-accessible/compatible-symbol walker resume declaration environment procedure-expr))))
        ;; special form
        (if (and binding (jazz.walk-binding-walkable? binding))
            (jazz.walk-binding-walk-form binding walker resume declaration environment form-src)
          ;; macro
          (if (and binding (jazz.walk-binding-expandable? binding))
              (let ((expansion (jazz.walk-binding-expand-form binding walker resume declaration environment form-src)))
                (jazz.walk walker resume declaration environment expansion))
            ;; call
            (jazz.walk-call walker resume declaration environment binding form-src)))))))


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


(define (jazz.walk-call walker resume declaration environment procedure-binding form-src)
  (let ((operator (%%desourcify (%%car (%%source-code form-src))))
        (arguments (%%cdr (%%source-code form-src))))
    (if procedure-binding
        (jazz.walk-binding-validate-call procedure-binding walker resume declaration operator (jazz.desourcify-list arguments)))
    (jazz.new-call form-src
                   (jazz.walk walker resume declaration environment operator)
                   (jazz.walk-list walker resume declaration environment arguments))))


(jazz.define-virtual-runtime (jazz.validate-arguments (jazz.Walker walker) resume source-declaration declaration signature arguments))


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


(define (jazz.walk-proclaim walker resume declaration environment form-src)
  (let ((form (%%desourcify form-src)))
    (receive (optimize) (jazz.parse-proclaim walker resume declaration (%%cdr form))
      (jazz.new-proclaim optimize))))


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


(define (jazz.walk-native walker resume declaration environment form-src)
  (let ((form (%%desourcify form-src)))
    (receive (name specifier access compatibility) (jazz.parse-native walker resume declaration (%%cdr form))
      (receive (name symbol) (jazz.parse-exported-symbol declaration name)
        (jazz.find-form-declaration declaration name)))))


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


(define (jazz.walk-macro walker resume declaration environment form-src)
  (let ((form (%%desourcify form-src)))
    (receive (name type access compatibility parameters body) (jazz.parse-macro walker resume declaration (%%cdr form))
      (let* ((new-declaration (jazz.find-form-declaration declaration name)))
        (receive (signature augmented-environment) (jazz.walk-parameters walker resume declaration environment parameters #f #t)
          (%%set-macro-declaration-signature new-declaration signature)
          (%%set-macro-declaration-body new-declaration
            (jazz.walk-body walker resume new-declaration augmented-environment body))
          (%%set-declaration-source new-declaration form-src)
          new-declaration)))))


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


(define (jazz.walk-syntax walker resume declaration environment form-src)
  (let ((form (%%desourcify form-src)))
    (receive (name type access compatibility parameters body) (jazz.parse-syntax walker resume declaration (%%cdr form))
      (let* ((new-declaration (jazz.find-form-declaration declaration name)))
        (receive (signature augmented-environment) (jazz.walk-parameters walker resume declaration environment parameters #f #t)
          (%%set-syntax-declaration-signature new-declaration signature)
          (%%set-syntax-declaration-body new-declaration
            (jazz.walk-body walker resume new-declaration augmented-environment body))
          (%%set-declaration-source new-declaration form-src)
          new-declaration)))))


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
                                       (if (null? (%%cdr rest))
                                           (let ((type (if specifier (jazz.walk-specifier walker resume declaration environment specifier) #f))
                                                 (default (%%car rest)))
                                             (set! section 'optional)
                                             (let ((optional-parameter (jazz.new-optional-parameter variable type (if walk? (jazz.walk walker resume declaration augmented-environment default) #f))))
                                               (jazz.enqueue optional optional-parameter)
                                               (%%when walk?
                                                 (set! augmented-environment (%%cons optional-parameter augmented-environment)))))
                                         (jazz.walk-error walker resume declaration "Ill-formed optional parameter: {s}" parameter))))))))
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
        #f
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


(define (jazz.call-with-catalog-entry-lock module-name thunk)
  (jazz.call-with-load-lock
    (lambda ()
      (dynamic-wind
        (lambda ()
          (jazz.set-catalog-entry module-name ':loading)
          (jazz.push-load-stack ':walk module-name))
        thunk
        (lambda ()
          (jazz.pop-load-stack)
          (if (%%eq? (jazz.get-catalog-entry module-name) ':loading)
              (jazz.set-catalog-entry module-name #f)))))))


(define (jazz.locate-toplevel-declaration module-name #!optional (error? #t))
  (let ((entry (jazz.get-catalog-entry module-name)))
    (case entry
      ((:loading)
       (jazz.error "Circular dependency detected with {s}" module-name))
      ((#f)
       (jazz.call-with-catalog-entry-lock module-name
         (lambda ()
           (let ((declaration (jazz.load-toplevel-declaration module-name)))
             (if (%%not declaration)
                 (if error?
                     (jazz.error "Unable to locate module declaration: {s}" module-name))
               (jazz.set-catalog-entry module-name declaration))
             declaration))))
      (else
       entry))))


(define (jazz.locate-library-declaration module-name #!optional (error? #t))
  (let ((declaration (jazz.locate-toplevel-declaration module-name error?)))
    (%%assert (%%class-is? declaration jazz.Library-Declaration)
      declaration)))


(define (jazz.load-toplevel-declaration module-name)
  (let ((src (jazz.find-module-src module-name '("jazz" "scm"))))
    (let ((source (jazz.resource-pathname src)))
      (define (load-declaration)
        (let ((form (jazz.read-toplevel-form source)))
          (parameterize ((jazz.requested-module-name module-name)
                         (jazz.requested-module-resource src))
            (case (%%car form)
              ((module)
               (jazz.parse-module-declaration (%%cdr form)))
              ((library)
               (jazz.parse-library-declaration (%%cdr form)))))))
      
      (jazz.with-verbose (jazz.parse-verbose?) "parsing" source
        (lambda ()
          (load-declaration))))))


(define jazz.parse-read?
  (make-parameter #f))


(define (jazz.read-toplevel-form source #!key (parse-read? #t) (read-source? #f))
  (receive (form extraneous?)
      (jazz.with-extension-reader (jazz.pathname-extension source)
         (lambda ()
           (call-with-input-file (list path: source eol-encoding: 'cr-lf)
             (lambda (port)
               (parameterize ((jazz.parse-read? parse-read?))
                 (if (not read-source?)
                     (let ((form (read port))
                           (extraneous? (%%not (%%eof-object? (read port)))))
                       (values form extraneous?))
                   (values (jazz.read-source port) #f)))))))
    (if (or read-source? (and (%%pair? form) (%%memq (%%car form) '(module library))))
        (if (%%not extraneous?)
            form
          (jazz.error "Found extraneous expressions after {a} definition in: {a}" (%%car form) source))
      (jazz.error "Invalid module declaration in {a}: {s}" source form))))


;;;
;;;; Core Dialect
;;;


(jazz.define-class-runtime jazz.Core-Dialect)


(define (jazz.new-core-dialect)
  (jazz.allocate-core-dialect jazz.Core-Dialect))


(jazz.define-method (jazz.dialect-name (jazz.Core-Dialect dialect))
  'core)


(jazz.define-method (jazz.dialect-walker (jazz.Core-Dialect dialect))
  (jazz.new-core-walker))


(jazz.encapsulate-class jazz.Core-Dialect)


;;;
;;;; Core Walker
;;;


(jazz.define-class-runtime jazz.Core-Walker)


(define (jazz.new-core-walker)
  (jazz.allocate-core-walker jazz.Core-Walker '() '()))


(jazz.encapsulate-class jazz.Core-Walker)


;;;
;;;; Register Core
;;;


(jazz.register-dialect 'core (jazz.new-core-dialect)))
