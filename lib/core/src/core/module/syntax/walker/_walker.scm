;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Modules
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
;; - It is key to have compile time reference loading identical to runtime module loading
;;   to ensure that runtime problems are detected at compile time exactly as they would occur
;; - Autoload declarations are treated specially as they are the only case where different
;;   references to the same declaration generate different code: direct access will expand to
;;   just the locator while an autoload access must add code to load the unit
;;
;; Todo
;; - Is the extra indirection level of having declaration references really necessary?
;; - Convert and remove the temporary patch jazz.register-autoload that was used to implement
;;   the old load
;; - Cleanup the probably not usefull new method jazz.resolve-binding that I added to get
;;   things working


(unit protected core.module.syntax.walker


;;;
;;;; Code Analysis
;;;


;; complex code analysis for tools
(define jazz.analysis-mode?
  (make-parameter #f))


;; declarations analysis table
(define jazz.analysis-data
  (%%make-table test: eq?))


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


(jazz.define-virtual-runtime (jazz.walk-binding-lookup (jazz.Walk-Binding binding) symbol source-declaration))
(jazz.define-virtual-runtime (jazz.walk-binding-referenced (jazz.Walk-Binding binding)))
(jazz.define-virtual-runtime (jazz.emit-binding-symbol (jazz.Walk-Binding binding) source-declaration environment))
(jazz.define-virtual-runtime (jazz.emit-binding-reference (jazz.Walk-Binding binding) source-declaration environment))
(jazz.define-virtual-runtime (jazz.walk-binding-validate-call (jazz.Walk-Binding binding) walker resume source-declaration operator arguments form-src))
(jazz.define-virtual-runtime (jazz.emit-binding-call (jazz.Walk-Binding binding) binding-src arguments source-declaration environment))
(jazz.define-virtual-runtime (jazz.emit-inlined-binding-call (jazz.Walk-Binding binding) arguments call source-declaration environment))
(jazz.define-virtual-runtime (jazz.walk-binding-validate-assignment (jazz.Walk-Binding binding) walker resume source-declaration symbol-src))
(jazz.define-virtual-runtime (jazz.walk-binding-assignable? (jazz.Walk-Binding binding)))
(jazz.define-virtual-runtime (jazz.emit-binding-assignment (jazz.Walk-Binding binding) value source-declaration environment))
(jazz.define-virtual-runtime (jazz.walk-binding-walkable? (jazz.Walk-Binding binding)))
(jazz.define-virtual-runtime (jazz.walk-binding-walk-form (jazz.Walk-Binding binding) walker resume declaration environment form-src))
(jazz.define-virtual-runtime (jazz.walk-binding-expandable? (jazz.Walk-Binding binding)))
(jazz.define-virtual-runtime (jazz.walk-binding-expand-form (jazz.Walk-Binding binding) walker resume declaration environment form-src))


(jazz.define-method (jazz.walk-binding-lookup (jazz.Walk-Binding binding) symbol source-declaration)
  #f)


(jazz.define-method (jazz.walk-binding-referenced (jazz.Walk-Binding binding))
  (jazz.unspecified))


(jazz.define-method (jazz.emit-binding-symbol (jazz.Walk-Binding binding) source-declaration environment)
  (jazz.error "Unable to emit binding symbol for: {s}" binding))


(jazz.define-method (jazz.emit-binding-reference (jazz.Walk-Binding binding) source-declaration environment)
  (jazz.error "Unable to emit binding reference for: {s}" binding))


(jazz.define-method (jazz.walk-binding-validate-call (jazz.Walk-Binding binding) walker resume source-declaration operator arguments form-src)
  (jazz.unspecified))


(jazz.define-method (jazz.emit-binding-call (jazz.Walk-Binding binding) binding-src arguments source-declaration environment)
  (let ((type (%%get-lexical-binding-type binding)))
    (jazz.new-code
      `(,(jazz.sourcified-form2 (jazz.emit-binding-reference binding source-declaration environment) binding-src)
        ,@(jazz.codes-forms arguments))
      (jazz.call-return-type type)
      #f)))


(jazz.define-method (jazz.emit-inlined-binding-call (jazz.Walk-Binding binding) arguments call source-declaration environment)
  #f)


(jazz.define-method (jazz.walk-binding-validate-assignment (jazz.Walk-Binding binding) walker resume source-declaration symbol-src)
  (%%when (%%not (jazz.walk-binding-assignable? binding))
    (jazz.walk-error walker resume source-declaration symbol-src "Illegal assignment to: {s}" (%%get-lexical-binding-name binding))))


(jazz.define-method (jazz.walk-binding-assignable? (jazz.Walk-Binding binding))
  #f)


(jazz.define-method (jazz.emit-binding-assignment (jazz.Walk-Binding binding) value source-declaration environment)
  (jazz.unspecified))


(jazz.define-method (jazz.walk-binding-walkable? (jazz.Walk-Binding binding))
  #f)


(jazz.define-method (jazz.walk-binding-expandable? (jazz.Walk-Binding binding))
  #f)


(jazz.encapsulate-class jazz.Walk-Binding)


(define (jazz.call-return-type operator-type)
  (if (%%is? operator-type jazz.Function-Type)
      (%%get-function-type-result operator-type)
    jazz.Any))


;;;
;;;; Lexical Binding
;;;


(jazz.define-class-runtime jazz.Lexical-Binding)


(jazz.define-virtual-runtime (jazz.resolve-binding (jazz.Lexical-Binding binding)))


(jazz.define-method (jazz.resolve-binding (jazz.Lexical-Binding binding))
  binding)


(jazz.define-method (jazz.print-object (jazz.Lexical-Binding binding) output detail)
  (jazz.format output "#<{a} {a} #{a}>"
               (%%get-category-identifier (%%get-object-class binding))
               (%%get-lexical-binding-name binding)
               (jazz.object->serial binding)))


(jazz.define-method (jazz.walk-binding-lookup (jazz.Lexical-Binding binding) symbol source-declaration)
  (if (%%eq? (%%get-lexical-binding-name binding) symbol)
      binding
    #f))


(define (jazz.get-lexical-binding-name binding)
  (%%get-lexical-binding-name binding))


(define (jazz.get-lexical-binding-hits binding)
  (or (%%get-lexical-binding-hits binding)
      (let ((table (%%make-table test: eq?)))
        (%%set-lexical-binding-hits binding table)
        table)))


(jazz.define-method (jazz.emit-binding-symbol (jazz.Lexical-Binding binding) declaration environment)
  (%%get-lexical-binding-name binding))


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


(define (jazz.get-declaration-path declaration)
  (letrec ((proc
            (lambda (declaration)
              (let ((name (%%get-lexical-binding-name declaration))
                    (parent (%%get-declaration-parent declaration)))
                (if (%%not parent)
                    (%%list name)
                  (%%cons name (proc parent)))))))
    (jazz.reverse! (proc declaration))))


(jazz.define-method (jazz.walk-binding-lookup (jazz.Declaration binding) symbol source-declaration)
  (jazz.lookup-declaration binding symbol jazz.private-access source-declaration))


(jazz.define-method (jazz.walk-binding-validate-call (jazz.Declaration declaration) walker resume source-declaration operator arguments form-src)
  (jazz.walk-error walker resume source-declaration form-src "{a} is not callable" (%%get-lexical-binding-name declaration)))


(jazz.define-virtual-runtime (jazz.lookup-declaration (jazz.Declaration declaration) symbol access source-declaration))


(jazz.define-method (jazz.lookup-declaration (jazz.Declaration declaration) symbol access source-declaration)
  #f)


(jazz.define-virtual-runtime (jazz.get-declaration-inclusions (jazz.Declaration declaration)))


(jazz.define-method (jazz.get-declaration-inclusions (jazz.Declaration declaration))
  '())


(jazz.define-virtual-runtime (jazz.get-nextmethod-signature (jazz.Declaration declaration)))


(jazz.define-method (jazz.get-nextmethod-signature (jazz.Declaration declaration))
  (jazz.error "No nextmethod signature for: {s}" declaration))


(jazz.define-virtual-runtime (jazz.emit-declaration (jazz.Declaration declaration) environment))


(jazz.define-method (jazz.emit-declaration (jazz.Declaration declaration) environment)
  (jazz.error "Unable to emit: {s}" declaration))


(jazz.define-virtual-runtime (jazz.expand-referenced-declaration (jazz.Declaration declaration)))


(jazz.define-method (jazz.expand-referenced-declaration (jazz.Declaration declaration))
  '())


(jazz.define-virtual-runtime (jazz.fold-declaration (jazz.Declaration declaration) f k s))


(jazz.define-method (jazz.fold-declaration (jazz.Declaration declaration) f k s)
  (f declaration s))


(define (jazz.declaration-result)
  (if (%%eq? (jazz.walk-for) 'eval)
      '((jazz.unspecified))
    '()))


(jazz.encapsulate-class jazz.Declaration)


;;;
;;;; Declaration Reference
;;;


(jazz.define-class-runtime jazz.Declaration-Reference)


(jazz.define-virtual-runtime (jazz.resolve-reference (jazz.Declaration-Reference declaration-reference) module-declaration))


(jazz.define-method (jazz.resolve-reference (jazz.Declaration-Reference declaration-reference) module-declaration)
  (or (%%get-declaration-reference-declaration declaration-reference)
      (receive (name symbol) (jazz.parse-exported-symbol module-declaration (%%get-declaration-reference-name declaration-reference))
        (let ((declaration (jazz.new-export-declaration name #f 'public 'uptodate '() #f symbol)))
          (%%set-declaration-reference-declaration declaration-reference declaration)
          declaration))))


(jazz.encapsulate-class jazz.Declaration-Reference)


;;;
;;;; Module Reference
;;;


(jazz.define-class-runtime jazz.Module-Reference)


(define (jazz.new-module-reference name declaration)
  (jazz.allocate-module-reference jazz.Module-Reference name declaration))


(jazz.define-method (jazz.resolve-reference (jazz.Module-Reference declaration-reference) module-declaration)
  (or (%%get-declaration-reference-declaration declaration-reference)
      (let ((declaration (jazz.outline-module (%%get-declaration-reference-name declaration-reference))))
        (%%set-declaration-reference-declaration declaration-reference declaration)
        declaration)))


(jazz.encapsulate-class jazz.Module-Reference)


;;;
;;;; Export Reference
;;;


(jazz.define-class-runtime jazz.Export-Reference)


(define (jazz.new-export-reference name declaration module-reference)
  (jazz.allocate-export-reference jazz.Export-Reference name declaration module-reference))


(jazz.define-method (jazz.resolve-reference (jazz.Export-Reference declaration-reference) module-declaration)
  (or (%%get-declaration-reference-declaration declaration-reference)
      (receive (name symbol) (jazz.parse-exported-symbol module-declaration (%%get-declaration-reference-name declaration-reference))
        (let ((locator (jazz.compose-name (%%get-lexical-binding-name module-declaration) name)))
          (let ((declaration (jazz.new-export-declaration name #f 'public 'uptodate '() #f locator)))
            (%%set-declaration-reference-declaration declaration-reference declaration)
            declaration)))))


(define (jazz.parse-exported-symbol module-declaration name)
  (if (jazz.composite-name? name)
      (values (jazz.identifier-name name) name)
    (values name name)))


(jazz.encapsulate-class jazz.Export-Reference)


;;;
;;;; Autoload Reference
;;;


(jazz.define-class-runtime jazz.Autoload-Reference)


(define (jazz.new-autoload-reference name declaration module-reference)
  (jazz.allocate-autoload-reference jazz.Autoload-Reference name declaration module-reference))


(define (jazz.resolve-autoload-reference declaration-reference module-declaration exported-module-reference)
  (or (%%get-declaration-reference-declaration declaration-reference)
      (let* ((name (%%get-declaration-reference-name declaration-reference))
             (type jazz.Any)
             (declaration (jazz.new-autoload-declaration name type #f module-declaration exported-module-reference)))
        (%%assert declaration
          (%%set-declaration-reference-declaration declaration-reference declaration)
          declaration))))


(jazz.encapsulate-class jazz.Autoload-Reference)


;;;
;;;; Unit
;;;


(jazz.define-class-runtime jazz.Unit-Declaration)


(define (jazz.new-unit-declaration name access parent requires)
  (let ((new-declaration (jazz.allocate-unit-declaration jazz.Unit-Declaration name #f #f access 'uptodate '() #f parent #f #f requires)))
    (jazz.setup-declaration new-declaration)
    new-declaration))


(jazz.define-method (jazz.fold-declaration (jazz.Unit-Declaration expression) f k s)
  (f expression s))


(jazz.encapsulate-class jazz.Unit-Declaration)


;;;
;;;; Namespace
;;;


(jazz.define-class-runtime jazz.Namespace-Declaration)


(jazz.define-method (jazz.lookup-declaration (jazz.Namespace-Declaration namespace-declaration) symbol access source-declaration)
  (define (add-to-module-references declaration)
    (%%when (and declaration
                 (%%neq? namespace-declaration (%%get-declaration-toplevel declaration)))
      (let* ((module-declaration (%%get-declaration-toplevel namespace-declaration))
             (references-table (%%get-module-declaration-walker-references module-declaration)))
        (%%when (%%neq? module-declaration (%%get-declaration-toplevel declaration))
          (%%table-set! references-table (%%get-declaration-locator declaration) declaration)))))
  
  (define (add-to-hits declaration)
    (%%when (and declaration source-declaration (jazz.analysis-mode?))
      (let ((hits-table (jazz.get-lexical-binding-hits declaration)))
        (%%table-set! hits-table (%%get-declaration-locator source-declaration) source-declaration))
      (%%when (%%is? declaration jazz.Autoload-Declaration)
        (%%set-analysis-data-autoload-reference (jazz.get-analysis-data (%%get-declaration-locator declaration)) declaration))))
  
  (let ((found (%%table-ref (%%get-access-lookup namespace-declaration access) symbol #f)))
    (add-to-module-references found)
    (add-to-hits found)
    found))


(jazz.define-method (jazz.fold-declaration (jazz.Namespace-Declaration declaration) f k s)
  (f declaration (jazz.fold-statements (%%get-namespace-declaration-body declaration) f k s s)))


(jazz.encapsulate-class jazz.Namespace-Declaration)


(define (jazz.get-private-lookup namespace-declaration)
  (%%get-access-lookup namespace-declaration jazz.private-access))

(define (jazz.get-public-lookup namespace-declaration)
  (%%get-access-lookup namespace-declaration jazz.public-access))

(define (jazz.get-protected-lookup namespace-declaration)
  (%%get-access-lookup namespace-declaration jazz.protected-access))



;;;
;;;; Module
;;;


(jazz.define-class-runtime jazz.Module-Declaration)


(define (jazz.new-module-declaration name access parent walker dialect-name dialect-invoice)
  (let ((new-declaration (jazz.allocate-module-declaration jazz.Module-Declaration name #f #f access 'uptodate '() #f parent #f #f (jazz.make-access-lookups jazz.public-access) (jazz.new-queue) #f walker dialect-name dialect-invoice '() '() '() (%%make-table test: eq?) '())))
    (jazz.setup-declaration new-declaration)
    new-declaration))


(define (jazz.add-module-require module-declaration require)
  (jazz.parse-require require
    (lambda (unit-name feature-requirement phase)
      (%%when (%%eq? phase 'syntax)
        (jazz.load-unit unit-name))))
  (%%set-module-declaration-requires module-declaration (%%append (%%get-module-declaration-requires module-declaration) (%%list require))))


(define (jazz.add-module-import module-declaration module-invoice register?)
  (define (merge-invoice actual new)
    ;; todo
    #f)
  
  (%%when (%%eq? (%%get-module-invoice-phase module-invoice) 'syntax)
    (let ((module-declaration (%%get-module-invoice-module module-invoice)))
      (jazz.load-unit (%%get-lexical-binding-name module-declaration))))
  (if register?
      (let ((imports (%%get-module-declaration-imports module-declaration)))
        (let ((actual (jazz.find-module-invoice imports module-invoice)))
          (if actual
              (merge-invoice actual module-invoice)
            (%%set-module-declaration-imports module-declaration (%%append imports (%%list module-invoice)))))))
  (let ((private (%%get-access-lookup module-declaration jazz.private-access))
        (only (%%get-module-invoice-only module-invoice)))
    (if only
        ;; todo
        #f
      (let ((imported-module-declaration (%%get-module-invoice-module module-invoice)))
        (let ((imported (%%get-access-lookup imported-module-declaration jazz.public-access)))
          (jazz.table-merge-reporting-conflicts! module-declaration "imports" private imported))))))


(define (jazz.add-module-export module-declaration module-invoice)
  (define (merge-invoice actual new)
    (let ((actual-autoload (%%get-export-invoice-autoload actual))
          (new-autoload (%%get-export-invoice-autoload new)))
      (%%when new-autoload
        (%%set-export-invoice-autoload actual (if actual-autoload (%%append actual-autoload new-autoload) new-autoload)))))
  
  (%%when (%%eq? (%%get-module-invoice-phase module-invoice) 'syntax)
    (let ((module-declaration (jazz.resolve-reference (%%get-module-invoice-module module-invoice) module-declaration)))
      (jazz.load-unit (%%get-lexical-binding-name module-declaration))))
  (let ((exports (%%get-module-declaration-exports module-declaration)))
    (let ((actual (jazz.find-module-invoice exports module-invoice)))
      (if actual
          (merge-invoice actual module-invoice)
        (%%set-module-declaration-exports module-declaration (%%append exports (%%list module-invoice))))))
  (let ((public (%%get-access-lookup module-declaration jazz.public-access))
        (only (%%get-module-invoice-only module-invoice))
        (autoload (%%get-export-invoice-autoload module-invoice)))
    (cond (only
           (for-each (lambda (declaration-reference)
                       (let ((name (jazz.identifier-name (%%get-declaration-reference-name declaration-reference))))
                         (%%table-set! public name (jazz.resolve-reference declaration-reference module-declaration))))
                     only))
          (autoload
           (let ((exported-module-reference (%%get-module-invoice-module module-invoice)))
             (for-each (lambda (declaration-reference)
                         (let ((name (jazz.identifier-name (%%get-declaration-reference-name declaration-reference))))
                           (%%table-set! public name (jazz.resolve-autoload-reference declaration-reference module-declaration exported-module-reference))))
                       autoload)))
          (else
           (let ((exported-module-declaration (jazz.resolve-reference (%%get-module-invoice-module module-invoice) module-declaration)))
             (jazz.table-merge-reporting-conflicts! module-declaration "exports" public (%%get-access-lookup exported-module-declaration jazz.public-access)))))))


(define (jazz.table-merge-reporting-conflicts! module-declaration suffix table add)
  (define (effective-declaration-locator decl)
    (cond ((%%is? decl jazz.Export-Declaration)
           (%%get-export-declaration-symbol decl))
          ((%%is? decl jazz.Autoload-Declaration)
           ;; this heuristic used because we cannot call jazz.resolve-binding at this point
           ;; is not 100% correct if the autoload was obtained through a reexported module...
           (jazz.compose-name (%%get-declaration-reference-name (%%get-autoload-declaration-exported-module decl)) (%%get-lexical-binding-name decl)))
          (else
           (%%get-declaration-locator decl))))
  
  (define (find-actual-conflicts)
    (let ((lst '()))
      (jazz.iterate-table add
        (lambda (key value)
          (let ((actual (%%table-ref table key #f)))
            (let ((value-locator (effective-declaration-locator value))
                  (actual-locator (effective-declaration-locator actual)))
              (%%when (%%neq? value-locator actual-locator)
                (set! lst (%%cons (%%list key value-locator actual-locator) lst)))))))
      lst))
  
  (let ((table-count (%%table-length table))
        (add-count (%%table-length add)))
    (%%table-merge! table add #f)
    (%%when (%%not (%%fx= (%%table-length table) (%%fx+ table-count add-count)))
      (let ((conflicts (find-actual-conflicts)))
        ;; Can be null if the same declaration has been imported from
        ;; different modules. Maybe we should also do an error in that case...
        (%%when (%%not (%%null? conflicts))
          (jazz.error "Import conflicts detected in {a} {a}: {s}"
                      (%%get-lexical-binding-name module-declaration)
                      suffix
                      conflicts))))))


(define (jazz.generate-reference-list module-declaration)
  (define (lesser name1 name2)
    (if (or (%%null? name1) (%%null? name2))
        (%%null? name1)
      (let ((string1 (%%symbol->string (car name1)))
            (string2 (%%symbol->string (car name2))))
        (or (%%string<? string1 string2)
            (and (%%string=? string1 string2)
                 (lesser (cdr name1) (cdr name2)))))))
  
  (define (merge-sorted item sorted)
    (cond ((%%null? sorted)
           (%%list item))
          ((lesser item (%%car sorted))
           (%%cons item sorted))
          (else
           (%%cons (%%car sorted) (merge-sorted item (%%cdr sorted))))))
  
  (define (compose-name root-declaration declaration)
    (let iter ((declaration declaration)
               (composite-name '()))
         (if (%%eq? root-declaration declaration)
             composite-name
           (iter (%%get-declaration-parent declaration) (%%cons (%%get-lexical-binding-name declaration) composite-name)))))
  
  (let ((partition (%%make-table test: eq?)))
    (jazz.iterate-table (%%get-module-declaration-walker-references module-declaration)
      (lambda (locator declaration)
        (let ((resolved-declaration (jazz.resolve-binding declaration)))
          (let ((module (%%get-declaration-toplevel resolved-declaration)))
            (%%table-set! partition module
              (merge-sorted (compose-name module resolved-declaration) (%%table-ref partition module '())))))))
    (let iter ((in (%%table->list partition))
               (out '()))
         (if (%%null? in)
             out
           (let ((module-locator (%%get-declaration-locator (%%caar in)))
                 (declarations (map (lambda (declaration)
                                      (if (and (%%pair? declaration) (%%null? (cdr declaration)))
                                          (car declaration)
                                        declaration))
                                    (%%cdar in))))
             (iter (%%cdr in) (merge-sorted (%%cons module-locator declarations) out)))))))


(jazz.define-method (jazz.lookup-declaration (jazz.Module-Declaration declaration) symbol access source-declaration)
  ;; code to detect unreferenced imports
  (%%when (jazz.analysis-mode?)
    (for-each (lambda (module-invoice)
                (let ((imported-module-declaration (%%get-module-invoice-module module-invoice)))
                  (let ((imported (%%get-access-lookup imported-module-declaration jazz.public-access)))
                    (%%when (%%table-ref imported symbol #f)
                      (%%set-import-invoice-hit? module-invoice #t)))))
              (%%get-module-declaration-imports declaration)))
  (nextmethod declaration symbol access source-declaration))


(jazz.define-method (jazz.emit-declaration (jazz.Module-Declaration declaration) environment)
  (let ((body-expansion (jazz.emit-namespace-statements (%%get-namespace-declaration-body declaration) declaration environment))
        (inclusions-expansion (jazz.emit-module-inclusions declaration))
        (literals-expansion (jazz.emit-module-literals declaration))
        (variables-expansion (jazz.emit-module-variables declaration))
        (autoloads-expansion (jazz.emit-module-autoloads declaration environment))
        (registration-expansion (jazz.emit-module-registration declaration environment)))
    `(begin
       ,@(case (jazz.walk-for)
           ((eval) '())
           (else (jazz.declares 'module)))
       ,@(let ((queue (jazz.new-queue))
               (load-units (%%make-table test: eq?)))
           (define (enqueue-load-unit unit-name)
             (%%when (%%not (%%table-ref load-units unit-name #f))
               (%%table-set! load-units unit-name #t)
               (jazz.enqueue queue `(jazz.load-unit ',unit-name))))
           
           (enqueue-load-unit 'core.module)
           (let ((dialect-name (%%get-module-declaration-dialect-name declaration)))
             (%%when (%%neq? dialect-name 'core)
               (enqueue-load-unit dialect-name)))
           (for-each (lambda (spec)
                       (jazz.parse-require spec
                         (lambda (unit-name feature-requirement phase)
                           (enqueue-load-unit unit-name))))
                     (%%get-module-declaration-requires declaration))
           (for-each (lambda (module-invoice)
                       (let ((only (%%get-module-invoice-only module-invoice))
                             (autoload (%%get-export-invoice-autoload module-invoice)))
                         (%%when (and (%%not only) (%%not autoload))
                           (let ((module-declaration (jazz.resolve-reference (%%get-module-invoice-module module-invoice) declaration))
                                 (phase (%%get-module-invoice-phase module-invoice)))
                             (%%when (and (%%neq? module-declaration declaration) (%%neq? phase 'syntax))
                               (enqueue-load-unit (%%get-lexical-binding-name module-declaration)))))))
                     (%%get-module-declaration-exports declaration))
           (let ((auto (jazz.new-queue))
                 (names (%%make-table test: eq?)))
             (for-each (lambda (module-invoice)
                         (let ((autoload (%%get-export-invoice-autoload module-invoice)))
                           (%%when autoload
                             (let ((unit-name (%%get-declaration-reference-name (%%get-module-invoice-module module-invoice))))
                               (for-each (lambda (decl)
                                           (let ((name (jazz.identifier-name (%%get-declaration-reference-name decl))))
                                             (%%when (%%not (%%table-ref names name #f))
                                               (%%table-set! names name #t)
                                               (let ((symbol-name (jazz.compose-name unit-name name)))
                                                 (jazz.enqueue auto `(jazz.register-autoload ',name ',unit-name
                                                                       (lambda ()
                                                                         (jazz.load-unit ',unit-name)
                                                                         ,symbol-name)))))))
                                         autoload)))))
                       (%%get-module-declaration-exports declaration))
             (jazz.enqueue-list queue (jazz.sort (jazz.queue-list auto) (lambda (x y) (%%string<? (%%symbol->string (%%cadr (%%cadr x))) (%%symbol->string (%%cadr (%%cadr y))))))))
           (for-each (lambda (module-invoice)
                       (let ((module-declaration (%%get-module-invoice-module module-invoice))
                             (phase (%%get-module-invoice-phase module-invoice)))
                         (%%when (and module-declaration (%%neq? phase 'syntax))
                           (enqueue-load-unit (%%get-lexical-binding-name module-declaration)))))
                     (%%get-module-declaration-imports declaration))
           (jazz.queue-list queue))
       ,@registration-expansion
       ,@inclusions-expansion
       ,@autoloads-expansion
       ,@literals-expansion
       ,@variables-expansion
       ,@body-expansion)))


(define (jazz.get-module-proclaim module-declaration proclaim-name default)
  (%%table-ref (%%get-module-declaration-proclaims module-declaration) proclaim-name default))


(define (jazz.set-module-proclaim module-declaration proclaim-name value)
  (%%table-set! (%%get-module-declaration-proclaims module-declaration) proclaim-name value))


(define jazz.all-warnings
  '(optimizations))


(define (jazz.proclaim module-declaration clause)
  (define (parse-not not? clause)
    (%%assert (%%pair? clause)
      (let ((kind (%%car clause))
            (parameters (%%cdr clause)))
        (values not? kind parameters))))
  
  (define (parse-clause clause)
    (%%assert (%%pair? clause)
      (if (%%eq? (%%car clause) 'not)
          (parse-not #t (%%cdr clause))
        (parse-not #f clause))))
  
  (receive (not? kind parameters) (parse-clause clause)
    (case kind
      ((warn)
       (let ((warnings (if (%%null? parameters) jazz.all-warnings parameters)))
         (for-each (lambda (warning)
                     (cond ((%%not (%%memq warning jazz.all-warnings))
                            (jazz.error "Unknown warning: {s}" warning))
                           ((%%not not?)
                            (let ((module-warnings (jazz.get-module-proclaim module-declaration 'warn '())))
                              (if (%%not (%%memq warning module-warnings))
                                  (jazz.set-module-proclaim module-declaration 'warn (%%cons warning module-warnings)))))
                           (else
                            (let ((module-warnings (jazz.get-module-proclaim module-declaration 'warn '())))
                              (if (%%memq warning module-warnings)
                                  (jazz.set-module-proclaim module-declaration 'warn (jazz.remove! warning module-warnings)))))))
                   warnings)))
       (else
        (jazz.error "Ill-formed proclaim: {s}" clause)))))


(define (jazz.get-module-warn? module-declaration warning-name)
  (%%memq warning-name (jazz.get-module-proclaim module-declaration 'warn '())))


(jazz.encapsulate-class jazz.Module-Declaration)


;;;
;;;; Module Invoice
;;;


(jazz.define-class-runtime jazz.Module-Invoice)


(jazz.encapsulate-class jazz.Module-Invoice)


(define (jazz.find-module-invoice invoices target)
  (let ((target-name (%%get-module-invoice-name target))
        (target-phase (%%get-module-invoice-phase target)))
    (jazz.find-if (lambda (invoice)
                    (and (%%eq? (%%get-module-invoice-name invoice) target-name)
                         (%%eq? (%%get-module-invoice-phase invoice) target-phase)))
                  invoices)))


;;;
;;;; Export Invoice
;;;


(jazz.define-class-runtime jazz.Export-Invoice)


(define (jazz.new-export-invoice name module phase version only autoload)
  (jazz.allocate-export-invoice jazz.Export-Invoice name module phase version only #f #f #f autoload))


(jazz.encapsulate-class jazz.Export-Invoice)


;;;
;;;; Import Invoice
;;;


(jazz.define-class-runtime jazz.Import-Invoice)


(define (jazz.new-import-invoice name module phase version only)
  (jazz.allocate-import-invoice jazz.Import-Invoice name module phase version only #f #f #f #f))


(jazz.encapsulate-class jazz.Import-Invoice)


;;;
;;;; Export
;;;


(jazz.define-class-runtime jazz.Export-Declaration)


(define (jazz.new-export-declaration name type access compatibility attributes parent symbol)
  (let ((new-declaration (jazz.allocate-export-declaration jazz.Export-Declaration name type #f access compatibility attributes #f parent #f #f symbol)))
    (jazz.setup-declaration new-declaration)
    new-declaration))


(jazz.define-method (jazz.walk-binding-validate-call (jazz.Export-Declaration declaration) walker resume source-declaration operator arguments form-src)
  (jazz.unspecified))


(jazz.define-method (jazz.emit-declaration (jazz.Export-Declaration declaration) environment)
  (let ((name (%%get-lexical-binding-name declaration))
        (symbol (%%get-export-declaration-symbol declaration))
        (parent (%%get-declaration-parent declaration)))
    (%%assert (%%is? parent jazz.Module-Declaration))
    `(jazz.register-native ',(%%get-lexical-binding-name parent) ',name ',symbol)))


(jazz.define-method (jazz.emit-binding-reference (jazz.Export-Declaration declaration) source-declaration environment)
  (jazz.new-code
    (%%get-export-declaration-symbol declaration)
    jazz.Any
    #f))


(jazz.define-method (jazz.fold-declaration (jazz.Export-Declaration declaration) f k s)
  (f declaration s))


(jazz.encapsulate-class jazz.Export-Declaration)


;;;
;;;; Export Syntax
;;;


(jazz.define-class-runtime jazz.Export-Syntax-Declaration)


(define (jazz.new-export-syntax-declaration name type access compatibility attributes parent symbol)
  (let ((new-declaration (jazz.allocate-export-syntax-declaration jazz.Export-Syntax-Declaration name type #f access compatibility attributes #f parent #f #f symbol)))
    (jazz.setup-declaration new-declaration)
    new-declaration))


(jazz.define-method (jazz.walk-binding-validate-call (jazz.Export-Syntax-Declaration declaration) walker resume source-declaration operator arguments form-src)
  (jazz.unspecified))


(jazz.define-method (jazz.emit-declaration (jazz.Export-Syntax-Declaration declaration) environment)
  `(begin))


(jazz.define-method (jazz.emit-binding-reference (jazz.Export-Syntax-Declaration declaration) source-declaration environment)
  (jazz.new-code
    (%%get-export-syntax-declaration-symbol declaration)
    jazz.Any
    #f))


(jazz.define-method (jazz.fold-declaration (jazz.Export-Syntax-Declaration declaration) f k s)
  (f declaration s))


(jazz.encapsulate-class jazz.Export-Syntax-Declaration)


;;;
;;;; Autoload
;;;


(jazz.define-class-runtime jazz.Autoload-Declaration)


(define (jazz.new-autoload-declaration name type parent module-declaration exported-module)
  (let ((new-declaration (jazz.allocate-autoload-declaration jazz.Autoload-Declaration name type #f 'public 'uptodate '() #f parent #f #f module-declaration exported-module #f)))
    (jazz.setup-declaration new-declaration)
    new-declaration))


(jazz.define-method (jazz.of-subtype? (jazz.Autoload-Declaration declaration) subtype)
  ;; not sure calling resolve here is correct
  (jazz.of-subtype? (jazz.resolve-binding declaration) subtype))


(jazz.define-method (jazz.resolve-binding (jazz.Autoload-Declaration declaration))
  (or (%%get-autoload-declaration-declaration declaration)
      (let* ((exported-module (jazz.resolve-reference (%%get-autoload-declaration-exported-module declaration) (%%get-autoload-declaration-module declaration)))
             (name (%%get-lexical-binding-name declaration))
             (decl (jazz.lookup-declaration exported-module name jazz.public-access declaration)))
        (%%assertion decl (jazz.error "Unable to find autoload {s} in unit {s}" name (%%get-declaration-locator exported-module))
          (%%set-autoload-declaration-declaration declaration decl)
          decl))))


(jazz.define-method (jazz.walk-binding-validate-call (jazz.Autoload-Declaration declaration) walker resume source-declaration operator arguments form-src)
  (jazz.walk-binding-validate-call (jazz.resolve-binding declaration) walker resume source-declaration operator arguments form-src))


(jazz.define-method (jazz.emit-binding-reference (jazz.Autoload-Declaration declaration) source-declaration environment)
  (let ((referenced-declaration (jazz.resolve-binding declaration)))
    (jazz.new-code
      `(,(jazz.autoload-locator referenced-declaration))
      (jazz.resolve-binding declaration)
      #f)))


(define (jazz.autoload-locator referenced-declaration)
  (%%string->symbol (%%string-append (%%symbol->string (%%get-declaration-locator referenced-declaration))
                                     ":autoload")))


(jazz.define-method (jazz.fold-declaration (jazz.Autoload-Declaration declaration) f k s)
  (f declaration s))


(jazz.encapsulate-class jazz.Autoload-Declaration)


;;;
;;;; Literal
;;;


(jazz.define-class-runtime jazz.Literal)


(define (jazz.new-literal name arguments)
  (jazz.allocate-literal jazz.Literal name arguments))


(jazz.encapsulate-class jazz.Literal)


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
  (if (and (%%pair? lst) (jazz.specifier? (jazz.source-code (%%car lst))))
      (proc (jazz.source-code (%%car lst)) (%%cdr lst))
    (proc #f lst)))


(define (jazz.walk-specifier walker resume declaration environment specifier)
  (let ((string (%%symbol->string specifier)))
    (let ((input (open-input-string string))
          (at 0))
      (define (ill-formed message)
        (let ((error-message (jazz.format "Ill-formed specifier {s} : at {a} : {a}" specifier (%%substring string 0 at) message)))
          (if (%%not walker)
              (jazz.error "{a}" error-message)
            (jazz.walk-error walker resume declaration #f error-message))))
      
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
(%%table-set! jazz.primitive-types 's8vector     jazz.S8Vector)
(%%table-set! jazz.primitive-types 'u8vector     jazz.U8Vector)
(%%table-set! jazz.primitive-types 's16vector    jazz.S16Vector)
(%%table-set! jazz.primitive-types 'u16vector    jazz.U16Vector)
(%%table-set! jazz.primitive-types 's32vector    jazz.S32Vector)
(%%table-set! jazz.primitive-types 'u32vector    jazz.U32Vector)
(%%table-set! jazz.primitive-types 's64vector    jazz.S64Vector)
(%%table-set! jazz.primitive-types 'u64vector    jazz.U64Vector)
(%%table-set! jazz.primitive-types 'f32vector    jazz.F32Vector)
(%%table-set! jazz.primitive-types 'f64vector    jazz.F64Vector)
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
(%%table-set! jazz.primitive-declarations jazz.S8Vector     'S8Vector)
(%%table-set! jazz.primitive-declarations jazz.U8Vector     'U8Vector)
(%%table-set! jazz.primitive-declarations jazz.S16Vector    'S16Vector)
(%%table-set! jazz.primitive-declarations jazz.U16Vector    'U16Vector)
(%%table-set! jazz.primitive-declarations jazz.S32Vector    'S32Vector)
(%%table-set! jazz.primitive-declarations jazz.U32Vector    'U32Vector)
(%%table-set! jazz.primitive-declarations jazz.S64Vector    'S64Vector)
(%%table-set! jazz.primitive-declarations jazz.U64Vector    'U64Vector)
(%%table-set! jazz.primitive-declarations jazz.F32Vector    'F32Vector)
(%%table-set! jazz.primitive-declarations jazz.F64Vector    'F64Vector)
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
        (let ((module-name (if (%%eq? name 'Object) 'jazz.dialect.language.object 'jazz.dialect.language.functional)))
          (let ((module-declaration (jazz.get-catalog-entry module-name)))
            (if module-declaration
                (jazz.lookup-declaration module-declaration name jazz.public-access module-declaration)
              type)))
      type)))


;;;
;;;; Macro
;;;


(jazz.define-class-runtime jazz.Macro-Declaration)


(define (jazz.new-macro-declaration name type access compatibility attributes parent signature)
  (let ((new-declaration (jazz.allocate-macro-declaration jazz.Macro-Declaration name type #f access compatibility attributes #f parent #f #f signature #f)))
    (jazz.setup-declaration new-declaration)
    new-declaration))


(jazz.define-method (jazz.walk-binding-expandable? (jazz.Macro-Declaration declaration))
  #t)


(jazz.define-method (jazz.walk-binding-expand-form (jazz.Macro-Declaration binding) walker resume declaration environment form-src)
  (let ((form (%%desourcify form-src)))
    (let ((locator (%%get-declaration-locator binding)))
      (if (%%eq? (%%get-declaration-toplevel binding) (%%get-declaration-toplevel declaration))
          (jazz.walk-error walker resume declaration form-src "Macros cannot be used from within the same file: {s}" locator)
        (let ((parent-declaration (%%get-declaration-parent binding)))
          (jazz.load-unit (%%get-declaration-locator (%%get-declaration-toplevel parent-declaration)))
          (let ((expander (jazz.need-macro locator)))
            (%%apply expander (%%cdr form))))))))


(jazz.define-method (jazz.emit-declaration (jazz.Macro-Declaration declaration) environment)
  (let ((locator (%%get-declaration-locator declaration))
        (signature (%%get-macro-declaration-signature declaration))
        (body (%%get-macro-declaration-body declaration)))
    (jazz.with-annotated-frame (jazz.annotate-signature signature)
      (lambda (frame)
        (let ((augmented-environment (%%cons frame environment)))
          (jazz.sourcify-if
            `(jazz.define-macro ,(%%cons locator (jazz.emit-signature signature declaration augmented-environment))
               ,@(jazz.sourcified-form (jazz.emit-expression body declaration augmented-environment)))
            (%%get-declaration-source declaration)))))))


(jazz.define-method (jazz.fold-declaration (jazz.Macro-Declaration declaration) f k s)
  (f declaration
     (k (jazz.fold-statement (%%get-macro-declaration-body declaration) f k s)
        s)))


(jazz.encapsulate-class jazz.Macro-Declaration)


;;;
;;;; Syntax
;;;


(jazz.define-class-runtime jazz.Syntax-Declaration)


(define (jazz.new-syntax-declaration name type access compatibility attributes parent signature)
  (let ((new-declaration (jazz.allocate-syntax-declaration jazz.Syntax-Declaration name type #f access compatibility attributes #f parent #f #f signature #f)))
    (jazz.setup-declaration new-declaration)
    new-declaration))


(jazz.define-method (jazz.walk-binding-expandable? (jazz.Syntax-Declaration declaration))
  #t)


(jazz.define-method (jazz.walk-binding-expand-form (jazz.Syntax-Declaration binding) walker resume declaration environment form-src)
  (let ((locator (%%get-declaration-locator binding)))
    (if (%%eq? (%%get-declaration-toplevel binding) (%%get-declaration-toplevel declaration))
        (jazz.walk-error walker resume declaration form-src "Syntaxes cannot be used from within the same file: {s}" locator)
      (let ((parent-declaration (%%get-declaration-parent binding)))
        (jazz.load-unit (%%get-declaration-locator (%%get-declaration-toplevel parent-declaration)))
        (let ((expander (jazz.need-macro locator)))
          (expander form-src))))))


(jazz.define-method (jazz.emit-declaration (jazz.Syntax-Declaration declaration) environment)
  (let ((locator (%%get-declaration-locator declaration))
        (signature (%%get-syntax-declaration-signature declaration))
        (body (%%get-syntax-declaration-body declaration)))
    (jazz.with-annotated-frame (jazz.annotate-signature signature)
      (lambda (frame)
        (let ((augmented-environment (%%cons frame environment)))
          (jazz.sourcify-if
            `(jazz.define-macro ,(%%cons locator (jazz.emit-signature signature declaration augmented-environment))
               ,@(jazz.sourcified-form (jazz.emit-expression body declaration augmented-environment)))
            (%%get-declaration-source declaration)))))))


(jazz.define-method (jazz.fold-declaration (jazz.Syntax-Declaration declaration) f k s)
  (f declaration
     (k (jazz.fold-statement (%%get-syntax-declaration-body declaration) f k s)
        s)))


(jazz.encapsulate-class jazz.Syntax-Declaration)


;;;
;;;; Define-Syntax
;;;


(jazz.define-class-runtime jazz.Define-Syntax-Declaration)


(define (jazz.new-define-syntax-declaration name type access compatibility attributes parent signature syntax-form)
  (let ((new-declaration (jazz.allocate-define-syntax-declaration jazz.Define-Syntax-Declaration name type #f access compatibility attributes #f parent #f #f signature #f)))
    (jazz.setup-declaration new-declaration)
    new-declaration))


(jazz.define-method (jazz.emit-declaration (jazz.Define-Syntax-Declaration declaration) environment)
  (let ((locator (%%get-declaration-locator declaration))
        (signature (%%get-syntax-declaration-signature declaration))
        (body (%%get-syntax-declaration-body declaration)))
    (jazz.with-annotated-frame (jazz.annotate-signature signature)
      (lambda (frame)
        (let ((augmented-environment (%%cons frame environment))
              (current-unit-name
               (%%get-declaration-locator
                (%%get-declaration-toplevel declaration))))
          (jazz.sourcify-if
            `(define ,locator
               (let* ((env
                       (%%list
                        (jazz.new-walk-frame
                         (%%get-dialect-bindings (jazz.get-dialect 'scheme)))
                        (jazz.new-walk-frame
                         (%%get-dialect-bindings (jazz.get-dialect 'core)))))
                      (env
                       (cond
                        ((jazz.get-dialect 'jazz)
                         => (lambda (x)
                              (cons (jazz.new-walk-frame (%%get-dialect-bindings x)) env)))
                        (else env)))
                      (env
                       (cond
                        ((jazz.outline-module ',current-unit-name)
                         => (lambda (x) (cons x env)))
                        (else env)))
                      (tmp (jazz.new-define-syntax-form
                           ',locator
                           ,@(jazz.sourcified-form (jazz.emit-expression body declaration augmented-environment))
                           env)))
                 (jazz.register-macro ',locator tmp)
                 tmp))
            (%%get-declaration-source declaration)))))))


(jazz.define-method (jazz.walk-binding-expand-form (jazz.Define-Syntax-Declaration binding) walker resume declaration environment form-src)
  (let ((locator (%%get-declaration-locator binding)))
    (if (%%eq? (%%get-declaration-toplevel binding) (%%get-declaration-toplevel declaration))
        (jazz.walk-warning walker resume declaration "Syntaxes shouldn't be used from within the same file: {s}" locator))
    (let ((parent-declaration (%%get-declaration-parent binding)))
      (jazz.load-unit (%%get-declaration-locator (%%get-declaration-toplevel parent-declaration)))
      (let* ((define-syntax-form (jazz.need-macro locator))
             (expander (%%get-syntax-form-expander define-syntax-form))
             (macro-environment (%%get-define-syntax-form-environment define-syntax-form)))
        (expander form-src environment macro-environment)))))


(jazz.encapsulate-class jazz.Define-Syntax-Declaration)


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


(define (jazz.new-walk-location unit-locator declaration-locator locat path)
  (jazz.allocate-walk-location jazz.Walk-Location unit-locator declaration-locator locat path))


(define (jazz.walk-location walker declaration locat)
  (jazz.new-walk-location
    (jazz.get-walk-locator)
    (%%get-declaration-locator declaration)
    locat
    (if locat (%%container->path (%%locat-container locat)) #f)))


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
  
  (jazz.new-exception-detail "ErrorStop" "Walk problems encountered" #f
    (let ((all (%%append (%%get-walk-problems-warnings problems)
                         (%%get-walk-problems-errors problems))))
      (map (lambda (partition)
             (jazz.bind (unit-locator . problems) partition
               (let ((prefix (if (%%not unit-locator) -1 (%%string-length (%%symbol->string unit-locator)))))
                 (jazz.new-exception-detail "Document" (or unit-locator "<console>") #f
                   (let ((unit-details (jazz.new-queue)))
                     (for-each (lambda (partition)
                                 (jazz.bind (declaration-locator . problems) partition
                                   (if (%%fx= (%%string-length declaration-locator) prefix)
                                       (add-details problems unit-details)
                                     (jazz.enqueue unit-details
                                       (jazz.new-exception-detail "Project" (%%substring declaration-locator (%%fx+ prefix 1) (%%string-length declaration-locator)) #f
                                         (let ((declaration-details (jazz.new-queue)))
                                           (add-details problems declaration-details)
                                           (jazz.queue-list declaration-details)))))))
                               (jazz.partition-walk-problems-declaration problems))
                     (jazz.queue-list unit-details))))))
           (jazz.partition-walk-problems-unit all)))))


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


(jazz.define-method (jazz.walk-binding-lookup (jazz.Walk-Frame binding) symbol source-declaration)
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


(jazz.define-method (jazz.emit-binding-symbol (jazz.Symbol-Binding binding) declaration environment)
  (or (%%get-symbol-binding-gensym binding)
      (unwrap-syntactic-closure (%%get-lexical-binding-name binding))))


(jazz.encapsulate-class jazz.Symbol-Binding)


;;;
;;;; Variable
;;;


(jazz.define-class-runtime jazz.Variable)


(define (jazz.new-variable name type)
  (%%assertion (jazz.variable-name-valid? name) (jazz.error "Invalid variable name: {s}" name)
    (jazz.allocate-variable jazz.Variable name type #f #f 0)))


(define (jazz.variable-name-valid? name)
  (define (variable-name-valid-symbol? name)
    (and (%%symbol? name)
         (%%not (jazz.specifier? name))))
  
  (variable-name-valid-symbol?
    (if (syntactic-closure? name)
        (%%get-syntactic-closure-form name)
      name)))


(jazz.define-method (jazz.walk-binding-referenced (jazz.Variable binding))
  (%%set-variable-reference-count binding (%%fx+ (%%get-variable-reference-count binding) 1)))


(jazz.define-method (jazz.emit-binding-reference (jazz.Variable binding) source-declaration environment)
  (jazz.new-code
    (jazz.emit-binding-symbol binding source-declaration environment)
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
      `(set! ,(jazz.emit-binding-symbol binding source-declaration environment)
             ,(jazz.sourcified-form value-code))
      jazz.Any
      #f)))


(jazz.encapsulate-class jazz.Variable)


;;;
;;;; NextMethod Variable
;;;


(jazz.define-class-runtime jazz.NextMethod-Variable)


(define (jazz.new-nextmethod-variable name type)
  (%%assertion (jazz.variable-name-valid? name) (jazz.error "Invalid variable name: {s}" name)
    (jazz.allocate-nextmethod-variable jazz.NextMethod-Variable name type #f #f 0)))


(jazz.define-method (jazz.emit-binding-reference (jazz.NextMethod-Variable binding) source-declaration environment)
  (let ((name (%%get-lexical-binding-name binding))
        (self (jazz.*self*)))
    (jazz.new-code
      (if self
          `(lambda rest (apply ,name ,(jazz.sourcified-form self) rest))
        name)
      jazz.Any
      #f)))


(jazz.define-method (jazz.walk-binding-validate-call (jazz.NextMethod-Variable declaration) walker resume source-declaration operator arguments form-src)
  (let ((signature (jazz.get-nextmethod-signature source-declaration)))
    (if signature
        (jazz.validate-arguments walker resume source-declaration declaration signature arguments form-src))))


(jazz.define-method (jazz.emit-binding-call (jazz.NextMethod-Variable binding) binding-src arguments source-declaration environment)
  (let ((name (%%get-lexical-binding-name binding))
        (type (%%get-lexical-binding-type binding))
        (self (jazz.*self*)))
    (if self
        (jazz.new-code
          `(,name
            ,(jazz.sourcified-form self)
            ,@(jazz.codes-forms arguments))
          (jazz.call-return-type type)
          #f)
      (jazz.new-code
        `(,name
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
    (jazz.allocate-parameter jazz.Parameter name type #f #f 0)))


(jazz.define-virtual-runtime (jazz.emit-parameter (jazz.Parameter parameter) declaration environment))


(jazz.define-method (jazz.emit-parameter (jazz.Parameter parameter) declaration environment)
  (jazz.emit-binding-symbol parameter declaration environment))


(jazz.encapsulate-class jazz.Parameter)


;;;
;;;; Dynamic Parameter
;;;


(jazz.define-class-runtime jazz.Dynamic-Parameter)


(define (jazz.new-dynamic-parameter name type class)
  (jazz.allocate-dynamic-parameter jazz.Dynamic-Parameter name type #f #f 0 class))


(jazz.define-method (jazz.emit-parameter (jazz.Dynamic-Parameter parameter) declaration environment)
  (let ((class (%%get-dynamic-parameter-class parameter)))
    (%%list (jazz.sourcified-form (jazz.emit-expression class declaration environment)) (jazz.emit-binding-symbol parameter declaration environment))))


(jazz.encapsulate-class jazz.Dynamic-Parameter)


;;;
;;;; Optional Parameter
;;;


(jazz.define-class-runtime jazz.Optional-Parameter)


(define (jazz.new-optional-parameter name type default)
  (jazz.allocate-optional-parameter jazz.Optional-Parameter name type #f #f 0 default))


(jazz.define-method (jazz.emit-parameter (jazz.Optional-Parameter parameter) declaration environment)
  (let ((default (%%get-optional-parameter-default parameter)))
    (%%list (jazz.emit-binding-symbol parameter declaration environment) (jazz.sourcified-form (jazz.emit-expression default declaration environment)))))


(jazz.encapsulate-class jazz.Optional-Parameter)


;;;
;;;; Named Parameter
;;;


(jazz.define-class-runtime jazz.Named-Parameter)


(define (jazz.new-named-parameter name type default)
  (jazz.allocate-named-parameter jazz.Named-Parameter name type #f #f 0 default))


(jazz.define-method (jazz.emit-parameter (jazz.Named-Parameter parameter) declaration environment)
  (let ((default (%%get-named-parameter-default parameter)))
    (%%list (%%get-lexical-binding-name parameter) (jazz.sourcified-form (jazz.emit-expression default declaration environment)))))


(jazz.define-method (jazz.emit-binding-symbol (jazz.Named-Parameter parameter) declaration environment)
  (%%get-lexical-binding-name parameter))


(jazz.encapsulate-class jazz.Named-Parameter)


;;;
;;;; Rest Parameter
;;;


(jazz.define-class-runtime jazz.Rest-Parameter)


(define (jazz.new-rest-parameter name type)
  (jazz.allocate-rest-parameter jazz.Rest-Parameter name type #f #f 0))


(jazz.define-method (jazz.emit-parameter (jazz.Rest-Parameter parameter) declaration environment)
  (jazz.emit-binding-symbol parameter declaration environment))


(jazz.encapsulate-class jazz.Rest-Parameter)


;;;
;;;; Self-Binding
;;;


;; Support for dialects that have an implicit self concept


(jazz.define-class-runtime jazz.Self-Binding)


(define (jazz.new-self-binding type)
  (jazz.allocate-self-binding jazz.Self-Binding 'self type #f))


(jazz.define-method (jazz.emit-binding-reference (jazz.Self-Binding declaration) source-declaration environment)
  (jazz.new-code
    'self
    (%%get-declaration-parent source-declaration)
    #f))


(jazz.encapsulate-class jazz.Self-Binding)


;;;
;;;; Dynamic-Self-Binding
;;;


(jazz.define-class-runtime jazz.Dynamic-Self-Binding)


(define (jazz.new-dynamic-self-binding type code)
  (jazz.allocate-dynamic-self-binding jazz.Dynamic-Self-Binding 'self type #f code))


(jazz.define-method (jazz.emit-binding-reference (jazz.Dynamic-Self-Binding declaration) source-declaration environment)
  (jazz.new-code
    (%%get-dynamic-self-binding-code declaration)
    (%%get-declaration-parent source-declaration)
    #f))


(jazz.encapsulate-class jazz.Dynamic-Self-Binding)


;;;
;;;; With-Self
;;;


(define jazz.*self*
  (make-parameter #f))


;;;
;;;; Local-Variable-Binding
;;;


(jazz.define-class-runtime jazz.Local-Variable-Binding)


(define (jazz.new-local-variable-binding type variable)
  (jazz.allocate-local-variable-binding jazz.Local-Variable-Binding variable type #f variable))


(jazz.define-method (jazz.emit-binding-reference (jazz.Local-Variable-Binding declaration) source-declaration environment)
  (jazz.new-code
    (%%get-local-variable-binding-variable declaration)
    jazz.Any
    #f))


(jazz.encapsulate-class jazz.Local-Variable-Binding)


;;;
;;;; Macro Symbol
;;;


(jazz.define-class-runtime jazz.Macro-Symbol)


(define (jazz.new-macro-symbol name getter setter)
  (jazz.allocate-macro-symbol jazz.Macro-Symbol name #f #f #f getter setter))


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


(define jazz.special-forms
  '())


(define (jazz.add-special-form symbol special-form)
  (set! jazz.special-forms (%%cons (%%cons symbol special-form) jazz.special-forms)))


(define (jazz.find-special-form symbol)
  #f
  #; ;; walk is undefined
  (let ((found (assq walk jazz.special-forms)))
    (if found (cdr found) #f)))


(jazz.define-class-runtime jazz.Special-Form)


(define (jazz.new-special-form name walk)
  (jazz.allocate-special-form jazz.Special-Form name #f #f walk))


(jazz.define-method (jazz.walk-binding-walkable? (jazz.Special-Form binding))
  #t)


(jazz.define-method (jazz.emit-binding-reference (jazz.Special-Form binding) source-declaration environment)
  (%%get-lexical-binding-name binding))


(jazz.define-method (jazz.walk-binding-walk-form (jazz.Special-Form binding) walker resume declaration environment form-src)
  (let ((walk-proc/symbol (%%get-special-form-walk binding)))
    (let ((walk (if (%%symbol? walk-proc/symbol)
                    ;; we should cache it
                    (jazz.find-special-form walk-proc/symbol)
                  walk-proc/symbol)))
      (walk walker resume declaration environment form-src))))


(jazz.encapsulate-class jazz.Special-Form)


;;;
;;;; Macro Form
;;;


(jazz.define-class-runtime jazz.Macro-Form)


(define (jazz.new-macro-form name expander)
  (jazz.allocate-macro-form jazz.Macro-Form name #f #f expander))


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
  (jazz.allocate-syntax-form jazz.Syntax-Form name #f #f expander))


(jazz.define-method (jazz.walk-binding-expandable? (jazz.Syntax-Form binding))
  #t)


(jazz.define-method (jazz.walk-binding-expand-form (jazz.Syntax-Form binding) walker resume declaration environment form-src)
  (let ((expander (%%get-syntax-form-expander binding)))
    (expander walker resume declaration environment form-src)))


(jazz.encapsulate-class jazz.Syntax-Form)


;;;
;;;; Define-Syntax Form
;;;


(jazz.define-class-runtime jazz.Define-Syntax-Form)


(define (jazz.new-define-syntax-form name expander environment)
  (jazz.allocate-define-syntax-form jazz.Define-Syntax-Form name #f #f expander environment))


(jazz.define-method (jazz.walk-binding-expand-form (jazz.Define-Syntax-Form binding) walker resume declaration environment form-src)
  (let ((expander (%%get-syntax-form-expander binding))
        (macro-environment (%%get-define-syntax-form-environment binding)))
    (expander form-src environment macro-environment)))


(jazz.encapsulate-class jazz.Define-Syntax-Form)


;;;
;;;; Syntactic Closure
;;;


(jazz.define-class-runtime jazz.Syntactic-Closure)


(define (syntactic-closure? x)
  (%%class-is? x jazz.Syntactic-Closure))

(define (make-syntactic-closure env vars form)
  (if (or (%%symbol? form) (%%pair? form)
          (and (%%source? form)
               (or (%%symbol? (%%source-code form))
                   (%%pair? (%%source-code form)))))
      (jazz.allocate-syntactic-closure jazz.Syntactic-Closure env vars form)
      form))

(define (syntactic-closure-form sc)
  (%%get-syntactic-closure-form sc))

(define (unwrap-syntactic-closure x)
  (cond
   ((syntactic-closure? x)
    (unwrap-syntactic-closure (%%get-syntactic-closure-form x)))
   ((%%source? x)
    (unwrap-syntactic-closure (%%source-code x)))
   (else x)))

(define (strip-syntactic-closures x)
  (cond
   ((syntactic-closure? x)
    (strip-syntactic-closures (%%get-syntactic-closure-form x)))
   ((%%source? x)
    (strip-syntactic-closures (%%source-code x)))
   ((%%pair? x)
    (cons (strip-syntactic-closures (%%car x))
          (strip-syntactic-closures (%%cdr x))))
   ((%%vector? x)
    (%%list->vector (strip-syntactic-closures (%%vector->list x))))
   (else x)))

(define (strip-source-info x)
  (cond
   ((%%source? x)
    (strip-source-info (%%source-code x)))
   ((%%pair? x)
    (cons (strip-source-info (%%car x)) (strip-source-info (%%cdr x))))
   ((%%vector? x)
    (%%list->vector (strip-source-info (%%vector->list x))))
   (else x)))

(define (identifier? x)
  (cond
   ((syntactic-closure? x) (identifier? (syntactic-closure-form x)))
   ((%%source? x) (identifier? (%%source-code x)))
   (else (symbol? x))))

;; identifiers are equal if they resolve to the same lexical binding
(define (identifier=? x-env x y-env y)
  (define (lookup-identifier symbol environment)
    (if (jazz.composite-name? symbol)
        symbol
        (jazz.find-in (lambda (binding)
                        (jazz.walk-binding-lookup binding symbol #f))
                      environment)))
  (define (binding-name x)
    (if (%%class-is? x jazz.Lexical-Binding)
        (%%get-lexical-binding-name x)
        x))
  (or (eq? x y)
      (let ((x^ (lookup-identifier (unwrap-syntactic-closure x) x-env))
            (y^ (lookup-identifier (unwrap-syntactic-closure y) y-env)))
        (if x^
            (eq? x^ y^)
            (and (not y^)
                 (eq? (unwrap-syntactic-closure x)
                      (unwrap-syntactic-closure y)))))))

(define sc-macro-transformer
  (lambda (f)
    (lambda (expr use-env mac-env)
      (make-syntactic-closure mac-env '() (f expr use-env)))))

(define rsc-macro-transformer
  (lambda (f)
    (lambda (expr use-env mac-env)
      (f expr mac-env))))

(define er-macro-transformer
  (lambda (f)
    (lambda (expr use-env mac-env)
      (let ((rename
             (let ((renames '()))
               (lambda (identifier)
                 (cond
                  ((assq identifier renames) => cdr)
                  (else
                   (let ((name (make-syntactic-closure mac-env '() identifier)))
                     (set! renames (cons (cons identifier name) renames))
                     name))))))
            (compare
             (lambda (x y) (identifier=? use-env x use-env y))))
        (f expr rename compare)))))


(jazz.encapsulate-class jazz.Syntactic-Closure)


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
  (if (or (%%not src) (%%not (%%source? src)))
      code
    (jazz.new-code (%%get-code-form code)
                   (%%get-code-type code)
                   src)))


;; temp try
(define (jazz.sourcified-form code)
  (let ((form (%%get-code-form code))
        (src (%%get-code-source code)))
    (jazz.sourcify-if form src)))


;; temp try... 2
(define (jazz.sourcified-form2 code src)
  (let ((form (%%get-code-form code)))
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
            (or (let iter-variables ((variables
                                      (cond
                                       ((%%class-is? annotated-frame jazz.Annotated-Frame)
                                        (%%get-annotated-frame-variables annotated-frame))
                                       ((%%class-is? annotated-frame jazz.Variable)
                                        (list annotated-frame))
                                       (else '()))))
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


(define (jazz.walk-warning walker declaration src fmt-string . rest)
  (let ((location (jazz.walk-location walker declaration (jazz.source-locat src)))
        (message (apply jazz.format fmt-string rest)))
    (jazz.walker-warning walker (jazz.new-walk-warning location message))))


(define (jazz.walk-error walker resume declaration src fmt-string . rest)
  (let ((location (jazz.walk-location walker declaration (jazz.source-locat src)))
        (message (apply jazz.format fmt-string rest)))
    (jazz.walker-error walker resume (jazz.new-walk-error location message))))


(define (jazz.walk-unresolved walker resume declaration symbol-src)
  (let ((location (jazz.walk-location walker declaration (jazz.source-locat symbol-src))))
    (jazz.walker-error walker resume (jazz.new-unresolved-error location (jazz.source-code symbol-src)))))


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
        (jazz.format output "Walk problems encountered{%}")
        (for-each (lambda (partition)
                    (jazz.bind (unit-locator . problems) partition
                      (jazz.format output "  In {a}" (or unit-locator "<console>"))
                      (let ((prefix (if (%%not unit-locator) -1 (%%string-length (%%symbol->string unit-locator)))))
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
                  (jazz.partition-walk-problems-unit all))
        (let ((message (get-output-string output)))
          (raise (jazz.new-walk-problems message warnings errors)))))))


(define (jazz.partition-walk-problems-unit problems)
  (jazz.partition problems
                  (lambda (problem)
                    (%%get-walk-location-unit-locator (%%get-walk-problem-location problem)))
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
  (define (is-modifier? infos x)
    (cond ((%%null? infos) #f)
          ((%%memq x (%%caar infos)) #t)
          (else (is-modifier? (%%cdr infos) x))))
  (define (skip-modifiers infos ls)
    (if (and (%%pair? ls) (is-modifier? infos (jazz.source-code (%%car ls))))
        (skip-modifiers infos (%%cdr ls))
        ls))
  (define (get-modifier names from to)
    (cond ((%%eq? from to) #f)
          ((%%memq (jazz.source-code (%%car from)) names)
           (cond ((get-modifier names (%%cdr from) to)
                  => (lambda (x) (jazz.walk-error "Ambiguous modifiers: {s} {s}" walker resume declaration (%%car from) x)))
                 (else (jazz.source-code (%%car from)))))
          (else (get-modifier names (%%cdr from) to))))
  (let ((modifiers rest)
        (rest (skip-modifiers infos rest)))
    (let lp ((ls infos) (res '()))
      (cond
       ((%%null? ls) (%%apply values (%%reverse (%%cons rest res))))
       ((get-modifier (%%caar ls) modifiers rest)
        => (lambda (x) (lp (%%cdr ls) (%%cons x res))))
       (else (lp (%%cdr ls) (%%cons (%%cdar ls) res)))))))


;;;
;;;; Unit
;;;


(define (jazz.parse-unit-declaration partial-form)
  (define (parse rest proc)
    (let ((first (jazz.source-code (%%car rest))))
      (if (%%memq first '(protected public))
          (proc (jazz.source-code (%%cadr rest)) first (%%cddr rest))
        (proc (jazz.source-code (%%car rest)) 'public (%%cdr rest)))))

  (define (collect-requires body)
    (let ((requires '()))
      (for-each (lambda (expr)
                  (if (and (%%pair? (jazz.source-code expr))
                           (%%eq? (jazz.source-code (%%car (jazz.source-code expr))) 'require))
                      (set! requires (append requires (map jazz.listify (jazz.filter-features (%%cdr (%%desourcify expr))))))))
                body)
      requires))
  
  (parse partial-form
    (lambda (name access body)
      (if (and (jazz.requested-unit-name) (%%neq? name (jazz.requested-unit-name)))
          (jazz.error "Unit at {s} is defining {s}" (jazz.requested-unit-name) name)
        (let ((requires (collect-requires body)))
          (jazz.new-unit-declaration name access #f requires))))))


;;;
;;;; Module
;;;


(define (jazz.parse-module partial-form)
  (define (parse-modifiers rest)
    (let ((first (jazz.source-code (%%car rest))))
      (if (%%memq first '(protected public))
          (values first (%%cdr rest))
        (values 'public rest))))
  
  (receive (access rest) (parse-modifiers partial-form)
    (let ((name (jazz.source-code (%%car rest)))
          (dialect-name (jazz.source-code (%%cadr rest)))
          (body (%%cddr rest)))
      (%%assert (%%symbol? name)
        (values name
                access
                dialect-name
                body)))))


(define (jazz.parse-module-invoice specification)
  (%%assertion (%%pair? specification) (jazz.error "Ill-formed module invoice: {s}" specification)
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


(define (jazz.parse-module-declaration partial-form)
  (receive (name access dialect-name body) (jazz.parse-module partial-form)
    (if (and (jazz.requested-unit-name) (%%neq? name (jazz.requested-unit-name)))
        (jazz.error "Module at {s} is defining {s}" (jazz.requested-unit-name) name)
      (parameterize ((jazz.walk-context (jazz.new-walk-context #f name #f)))
        (let* ((dialect-invoice (jazz.load-dialect-invoice dialect-name))
               (dialect (jazz.require-dialect dialect-name))
               (walker (jazz.dialect-walker dialect)))
          (jazz.walk-module-declaration walker #f name access dialect-name dialect-invoice body))))))


(define (jazz.walk-module-declaration walker actual name access dialect-name dialect-invoice body)
  (let ((declaration (or actual (jazz.new-module-declaration name access #f walker dialect-name dialect-invoice))))
    (%%when dialect-invoice
      (jazz.add-module-import declaration dialect-invoice #f))
    ;; reset the walker if it was cached
    (%%set-module-declaration-walker declaration walker)
    (jazz.walk-declarations walker #f declaration (%%cons declaration (jazz.walker-environment walker)) body)
    (jazz.validate-walk-problems walker)
    declaration))


(define (jazz.walk-module-export walker export)
  (receive (module-name module-load module-phase module-version module-only module-autoload) (jazz.parse-module-invoice export)
    (let ((module-reference (jazz.new-module-reference module-name #f)))
      (jazz.new-export-invoice module-name
                               module-reference
                               module-phase
                               module-version
                               (if (%%not module-only)
                                   #f
                                 (map (lambda (symbol)
                                        (jazz.new-export-reference symbol #f #f))
                                      module-only))
                               (if (%%not module-autoload)
                                   #f
                                 (map (lambda (symbol)
                                        (jazz.new-autoload-reference symbol #f #f))
                                      module-autoload))))))


(define (jazz.expand-module-source partial-form)
  (jazz.emit-declaration (jazz.walk-module partial-form) '()))


(define (jazz.rename-identifier-conflicts expressions environment)
  (jazz.tree-fold-list
   expressions
   (lambda (x seed env) env)
   (lambda (x seed child-seed env) seed)
   (lambda (x seed env)
     (cond
      ((%%is? x jazz.Reference)
       (let* ((var (%%get-reference-binding x))
              (sym (unwrap-syntactic-closure (%%get-lexical-binding-name var))))
         (let lp1 ((e env))
           (cond
            ((pair? e)
             (let lp2 ((ls (%%car e)) (found? #f))
               (cond
                ((pair? ls)
                 (let* ((binding (%%car ls))
                        (same? (eq? var binding)))
                   (cond
                    ((and binding
                          (not same?)
                          (%%is? binding jazz.Variable)
                          (not (%%get-symbol-binding-gensym binding))
                          (eq? sym
                               (unwrap-syntactic-closure
                                (%%get-lexical-binding-name binding))))
                     ;; if we shadow an existing declaration, gensym a unique
                     ;; symbol for it
                     ;;(display "shadow: ") (write binding) (display " by ") (write var) (newline)
                     (%%set-symbol-binding-gensym binding (jazz.generate-symbol (symbol->string sym)))))
                   (lp2 (%%cdr ls) (or found? same?))))
                ((not found?)
                 (lp1 (%%cdr e)))))))))))
     seed)
   #f
   (list environment)))


(define (jazz.walk-module partial-form)
  (receive (name access dialect-name body) (jazz.parse-module partial-form)
    (if (and (jazz.requested-unit-name) (%%neq? name (jazz.requested-unit-name)))
        (jazz.error "Module at {s} is defining {s}" (jazz.requested-unit-name) name)
      (parameterize ((jazz.walk-context (jazz.new-walk-context #f name #f)))
        (let* ((dialect-invoice (jazz.load-dialect-invoice dialect-name))
               (dialect (jazz.require-dialect dialect-name))
               (walker (jazz.dialect-walker dialect))
               (resume #f)
               (actual (jazz.get-catalog-entry name))
               (declaration (jazz.call-with-catalog-entry-lock name
                              (lambda ()
                                (let ((declaration (jazz.walk-module-declaration walker actual name access dialect-name dialect-invoice body)))
                                  (jazz.set-catalog-entry name declaration)
                                  declaration))))
               (environment (%%cons declaration (jazz.walker-environment walker)))
               (body (jazz.walk-namespace walker resume declaration environment body)))
          (jazz.validate-walk-problems walker)
          (jazz.rename-identifier-conflicts body environment)
          (%%set-namespace-declaration-body declaration body)
          declaration)))))


(define (jazz.cond-expand form-src cont)
  (if (and (%%pair? (jazz.source-code form-src))
           (%%eq? (jazz.source-code (%%car (jazz.source-code form-src))) 'cond-expand))
      (let iter ((scan (%%cdr (jazz.source-code form-src))))
        (if (%%null? scan)
            (jazz.error "Unfulfilled cond-expand")
          (let ((clause (jazz.source-code (%%car scan))))
            (if (or (%%not (%%pair? clause))
                    (%%not (%%symbol? (jazz.source-code (%%car clause)))))
                (jazz.error "Ill-formed cond-expand clause: {s}" (%%desourcify clause))
              (let ((feature-requirement (jazz.source-code (%%car clause))))
                (if (or (jazz.feature-satisfied? feature-requirement)
                        (%%eq? feature-requirement 'else))
                    (if (%%null? (%%cdr clause))
                        (cont #f #f)
                      (cont `(begin ,@(%%cdr clause)) #t))
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
        dialect-name
        (jazz.outline-module dialect-name)
        'syntax
        #f
        #f))))


(define (jazz.emit-module-inclusions module-declaration)
  (define (find-name name lst)
    (if (%%null? lst)
        #f
      (if (%%eq? (%%get-lexical-binding-name (%%car lst)) name)
          #t
        (find-name name (%%cdr lst)))))
  
  (let ((queue (jazz.new-queue)))
    (letrec ((collect-declarations
              (lambda (declaration)
                (for-each collect-declarations (jazz.get-declaration-inclusions declaration))
                ;; This name based test if a quick solution to the complex problem of a declaration
                ;; replacing another one where there are references to the old one. Should we then just
                ;; replace or destructively modify the old one and what if the type of the one replacing
                ;; is incompatible...
                (%%when (%%not (find-name (%%get-lexical-binding-name declaration) (jazz.queue-list queue)))
                  (jazz.enqueue queue declaration)))))
      (for-each collect-declarations (%%get-module-declaration-inclusions module-declaration))
      (map (lambda (declaration)
             (jazz.expand-referenced-declaration declaration))
           (jazz.queue-list queue)))))


(define (jazz.emit-module-literals module-declaration)
  (map (lambda (info)
         (let ((name (%%car info))
               (value (%%cdr info)))
           `(define ,name ,(jazz.sourcified-form (jazz.emit-expression value module-declaration '())))))
       (%%get-module-declaration-walker-literals module-declaration)))


(define (jazz.emit-module-variables module-declaration)
  (map (lambda (variable)
         (let ((symbol (%%car variable))
               (value (%%cdr variable)))
           `(jazz.define-variable ,symbol ,value)))
       (jazz.queue-list (%%get-module-declaration-walker-variables module-declaration))))


(define (jazz.emit-module-autoloads module-declaration environment)
  (let ((queue (jazz.new-queue))
        (locators (%%make-table test: eq?)))
    (for-each (lambda (autoload-declaration)
                (let ((referenced-declaration (jazz.resolve-binding autoload-declaration)))
                  (let ((locator (jazz.autoload-locator referenced-declaration)))
                    (%%when (%%not (%%table-ref locators locator #f))
                      (%%table-set! locators locator #t)
                      (jazz.enqueue queue
                        `(define ,locator
                           (let ((loaded? #f))
                             (lambda ()
                               (if (%%not loaded?)
                                   (begin
                                     (jazz.load-unit ',(%%get-declaration-locator (%%get-declaration-toplevel referenced-declaration)))
                                     (set! loaded? #t)))
                               ,(jazz.sourcified-form (jazz.emit-binding-reference referenced-declaration module-declaration environment))))))))))
              (%%get-module-declaration-walker-autoloads module-declaration))
    (jazz.sort (jazz.queue-list queue) (lambda (x y) (%%string<? (%%symbol->string (%%cadr x)) (%%symbol->string (%%cadr y)))))))


(define (jazz.emit-module-registration declaration environment)
  `((jazz.register-module ',(%%get-lexical-binding-name declaration) ',(%%get-declaration-access declaration)
      ',(let ((queue (jazz.new-queue)))
          (for-each (lambda (module-invoice)
                      (let ((only (%%get-module-invoice-only module-invoice))
                            (autoload (%%get-export-invoice-autoload module-invoice)))
                        (%%when (and (%%not only) (%%not autoload))
                          (jazz.enqueue queue (%%get-module-invoice-name module-invoice)))))
                    (%%get-module-declaration-exports declaration))
          (jazz.sort (jazz.queue-list queue) (lambda (x y) (%%string<? (%%symbol->string x) (%%symbol->string y)))))
      ',(let ((walker (%%get-module-declaration-walker declaration))
              (queue (jazz.new-queue)))
          (jazz.iterate-table-safe (%%get-access-lookup declaration jazz.public-access)
            (lambda (name decl)
              (%%when (or (%%eq? (%%get-declaration-toplevel decl) declaration)
                          ;; quick hack
                          (%%is? decl jazz.Autoload-Declaration))
                (let ((export (jazz.runtime-export walker decl)))
                  (if export
                      (jazz.enqueue queue (%%cons name export)))))))
          (jazz.sort (jazz.queue-list queue) (lambda (x y) (%%string<? (%%symbol->string (%%car x)) (%%symbol->string (%%car y)))))))))


(jazz.define-virtual-runtime (jazz.runtime-export (jazz.Walker walker) declaration))


(jazz.define-method (jazz.runtime-export (jazz.Walker walker) declaration)
  (cond ((%%is? declaration jazz.Export-Declaration)
         (%%get-declaration-locator declaration))
        ((%%is? declaration jazz.Autoload-Declaration)
         (let ((referenced-declaration (jazz.resolve-binding declaration)))
           (%%cons (%%get-declaration-locator (%%get-declaration-toplevel referenced-declaration))
                   (%%get-declaration-locator referenced-declaration))))
        (else
         #f)))


;;;
;;;; Environment
;;;


(jazz.define-virtual-runtime (jazz.walker-bindings (jazz.Walker walker)))


(jazz.define-method (jazz.walker-bindings (jazz.Walker walker))
  (%%get-dialect-bindings (jazz.get-dialect 'core)))


(jazz.define-virtual-runtime (jazz.walker-environment (jazz.Walker walker)))


(jazz.define-method (jazz.walker-environment (jazz.Walker walker))
  (%%list (jazz.new-walk-frame (jazz.walker-bindings walker))))


;;;
;;;; Declaration
;;;


;; In order to be able to resolve internal declarations as we walk the code, the declaration
;; tree is pre-expanded including expanding macros when needed. Then, during the actual walk
;; we just find the declarations in this tree. This tree is also merged with any preexisting
;; declaration tree coming from the runtime catalog.


(jazz.define-virtual-runtime (jazz.walk-declaration (jazz.Walker walker) resume declaration environment form-src))


(jazz.define-method (jazz.walk-declaration (jazz.Walker walker) resume declaration environment form-src)
  (if (%%pair? (jazz.source-code form-src))
      (let ((first (jazz.source-code (%%car (jazz.source-code form-src)))))
        (case first
          ((require)       (jazz.walk-require-declaration       walker resume declaration environment form-src))
          ((export)        (jazz.walk-export-declaration        walker resume declaration environment form-src))
          ((import)        (jazz.walk-import-declaration        walker resume declaration environment form-src))
          ((native)        (jazz.walk-native-declaration        walker resume declaration environment form-src))
          ((native-syntax) (jazz.walk-native-syntax-declaration walker resume declaration environment form-src))
          ((macro)         (jazz.walk-macro-declaration         walker resume declaration environment form-src))
          ((syntax)        (jazz.walk-syntax-declaration        walker resume declaration environment form-src))
          ((define-syntax) (jazz.walk-define-syntax-declaration walker resume declaration environment form-src))
          (else            #f)))
    #f))


(define (jazz.walk-declarations walker resume declaration environment forms)
  (define (walk forms)
    (for-each (lambda (form-src)
                (continuation-capture
                  (lambda (resume)
                    (jazz.cond-expand form-src
                      (lambda (expr expr?)
                        (%%when expr?
                          (let ((expansion (jazz.expand-macros walker resume declaration environment expr)))
                            (if (jazz.begin-form? expansion)
                                (walk (%%cdr (jazz.source-code expansion)))
                              (jazz.walk-declaration walker resume declaration environment expansion)))))))))
              forms))
  
  (walk forms))


(define (jazz.add-declaration-child walker resume namespace-declaration child)
  (let ((name (%%get-lexical-binding-name child)))
    ;; not 100% sure about this change
    ;; tests to try and implement redefine error
    #;
    (let ((decl (jazz.find-child-declaration namespace-declaration name)))
      (%%when (eq? (%%get-declaration-locator child) 'jazz.test.advise.abc)
        (continuation-capture
          (lambda (cont)
            (jazz.debug jazz.Load-Stack)
            (display-continuation-backtrace cont)
            (newline)
            (jazz.testing (append (or (jazz.testing) '()) (list cont)))))))
    #;
    (let ((decl (jazz.find-child-declaration namespace-declaration name)))
      (%%when decl
        (jazz.debug (%%get-declaration-locator decl))))
    (%%when (%%not (jazz.find-child-declaration namespace-declaration name))
      (jazz.enqueue (%%get-namespace-declaration-children namespace-declaration) child))
    (%%table-set! (%%get-access-lookup namespace-declaration jazz.private-access) name child)
    ;; for now everything not private is considered public
    (%%when (%%neq? (%%get-declaration-access child) 'private)
      (%%table-set! (%%get-access-lookup namespace-declaration jazz.public-access) name child))
    child))


(define (jazz.require-declaration namespace-declaration name)
  (let ((declaration (jazz.find-declaration namespace-declaration name)))
    (%%assertion declaration (jazz.error "Unable to find declaration: {a}" name)
      declaration)))


(define (jazz.find-declaration namespace-declaration name)
  (%%table-ref (%%get-access-lookup namespace-declaration jazz.private-access) name #f))


(define (jazz.find-child-declaration namespace-declaration name)
  (jazz.find-if (lambda (decl)
                  (%%eq? (%%get-lexical-binding-name decl) name))
                (jazz.queue-list (%%get-namespace-declaration-children namespace-declaration))))


(define (jazz.begin-form? form)
  (and (%%pair? (jazz.source-code form))
       (%%eq? (jazz.source-code (%%car (jazz.source-code form))) 'begin)))


(define (jazz.define-form? form)
  (and (%%pair? (jazz.source-code form))
       (%%eq? (jazz.source-code (%%car (jazz.source-code form))) 'define)))


;;;
;;;; Dependencies
;;;


(define (jazz.register-autoload-declaration module-declaration autoload-declaration)
  (let ((declarations (%%get-module-declaration-walker-autoloads module-declaration)))
    (%%when (%%not (%%memq autoload-declaration declarations))
      (%%set-module-declaration-walker-autoloads module-declaration (%%cons autoload-declaration declarations)))))


;;;
;;;; Lookup
;;;


(define (jazz.lookup-reference walker resume declaration environment symbol)
  (or (jazz.lookup-symbol walker resume declaration environment symbol)
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


(define (jazz.tree-fold-list ls down up here seed environment)
  (if (null? ls)
      seed
      (jazz.tree-fold-list (cdr ls) down up here (jazz.tree-fold (car ls) down up here seed environment) environment)))


(jazz.encapsulate-class jazz.Expression)


;;;
;;;; Reference
;;;


(jazz.define-class-runtime jazz.Reference)


(define (jazz.new-reference symbol-src binding)
  (jazz.allocate-reference jazz.Reference #f symbol-src binding))


(jazz.define-method (jazz.emit-expression (jazz.Reference expression) declaration environment)
  (jazz.sourcify-code (jazz.emit-binding-reference (%%get-reference-binding expression) declaration environment)
                      (%%get-expression-source expression)))


(jazz.define-method (jazz.emit-call (jazz.Reference expression) arguments declaration environment)
  (jazz.sourcify-code (jazz.emit-binding-call (%%get-reference-binding expression) (%%get-expression-source expression) arguments declaration environment)
                      (%%get-expression-source expression)))


(jazz.define-method (jazz.fold-expression (jazz.Reference expression) f k s)
  (f expression s))


(jazz.encapsulate-class jazz.Reference)


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
        (let ((augmented-environment (%%cons frame environment)))
          (jazz.new-code
            (%%append (jazz.codes-forms (jazz.emit-expressions internal-defines declaration augmented-environment))
                      (jazz.codes-forms (jazz.emit-expressions expressions declaration augmented-environment)))
            jazz.Any
            #f))))))


(jazz.define-method (jazz.fold-expression (jazz.Body expression) f k s)
  (f expression
     (jazz.fold-statements (%%get-body-internal-defines expression) f k s
       (jazz.fold-statements (%%get-body-expressions expression) f k s s))))


(jazz.define-method (jazz.tree-fold (jazz.Body expression) down up here seed environment)
  (up expression
      seed
      (jazz.tree-fold-list
       (%%get-body-expressions expression) down up here
       (jazz.tree-fold-list
        (%%get-body-internal-defines expression) down up here
        (down expression seed environment)
        environment)
       environment)
      environment))


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
      `(define ,(jazz.emit-binding-symbol variable declaration environment)
         ,(jazz.sourcified-form (jazz.emit-expression value declaration environment)))
      jazz.Any
      #f)))


(jazz.define-method (jazz.fold-expression (jazz.Internal-Define expression) f k s)
  (f expression
     (k (jazz.fold-statement (%%get-internal-define-value expression) f k s)
        s)))


(jazz.define-method (jazz.tree-fold (jazz.Internal-Define expression) down up here seed environment)
  (up expression
      seed
      (jazz.tree-fold
       (%%get-internal-define-value expression) down up here
       (down expression seed environment)
       environment)
      environment))


(jazz.encapsulate-class jazz.Internal-Define)


;;;
;;;; Begin
;;;


(jazz.define-class-runtime jazz.Begin)


(define (jazz.new-begin source expressions)
  (jazz.allocate-begin jazz.Begin #f source expressions))


(jazz.define-method (jazz.emit-expression (jazz.Begin expression) declaration environment)
  (let ((expressions (%%get-begin-expressions expression)))
    (let ((code (jazz.emit-statements-code expressions declaration environment)))
      (jazz.new-code
        `(begin ,@(jazz.sourcified-form code))
        (%%get-code-type code)
        (%%get-expression-source expression)))))


(jazz.define-method (jazz.fold-expression (jazz.Begin expression) f k s)
  (f expression
     (jazz.fold-statements (%%get-begin-expressions expression) f k s s)))


(jazz.define-method (jazz.tree-fold (jazz.Begin expression) down up here seed environment)
  (up expression
      seed
      (jazz.tree-fold-list
       (%%get-begin-expressions expression) down up here
       (down expression seed environment)
       environment)
      environment))


(jazz.encapsulate-class jazz.Begin)


;;;
;;;; Call
;;;


(jazz.define-variable jazz.emit-specialized-call)
(jazz.define-variable jazz.emit-primitive-new-call)
(jazz.define-variable jazz.emit-primitive-call)
(jazz.define-variable jazz.emit-inlined-call)


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
        (or (jazz.emit-specialized-call operator locator arguments arguments-codes expression declaration environment)
            (jazz.emit-primitive-new-call operator locator arguments arguments-codes declaration environment)
            (jazz.emit-primitive-call operator locator arguments arguments-codes declaration environment)
            (jazz.emit-inlined-call operator arguments-codes expression declaration environment)
            (jazz.emit-call operator arguments-codes declaration environment))
        (%%get-expression-source expression)))))


(jazz.define-method (jazz.fold-expression (jazz.Call expression) f k s)
  (f expression
     (k (jazz.fold-expression (%%get-call-operator expression) f k s)
        (jazz.fold-expressions (%%get-call-arguments expression) f k s s))))


(jazz.define-method (jazz.tree-fold (jazz.Call expression) down up here seed environment)
  (up expression
      seed
      (jazz.tree-fold-list
       (cons (%%get-call-operator expression) (%%get-call-arguments expression))
       down up here
       (down expression seed environment)
       environment)
      environment))


(jazz.encapsulate-class jazz.Call)


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


(jazz.define-method (jazz.tree-fold (jazz.Assignment expression) down up here seed environment)
  (up expression
      seed
      (jazz.tree-fold (%%get-assignment-value expression) down up here (down expression seed environment) environment)
      environment))


(jazz.encapsulate-class jazz.Assignment)


;;;
;;;; Analysis Data
;;;


(jazz.define-class-runtime jazz.Analysis-Data)


(define (jazz.new-analysis-data)
  (jazz.allocate-analysis-data jazz.Analysis-Data #f #f))


(jazz.encapsulate-class jazz.Analysis-Data)


(define (jazz.get-analysis-data locator)
  (or (%%table-ref jazz.analysis-data locator #f)
      (let ((data (jazz.new-analysis-data)))
        (%%table-set! jazz.analysis-data locator data)
        data)))


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
;;;; Walk
;;;

(define (jazz.walk walker resume declaration environment form-src)
  (let ((form (jazz.source-code form-src)))
    (cond ((identifier? form)
           (jazz.walk-symbol walker resume declaration environment form-src))
          ((%%pair? form)
           (jazz.walk-form walker resume declaration environment form-src))
          ((syntactic-closure? form)
           (jazz.walk walker resume declaration (append (%%get-syntactic-closure-environment form) environment) (%%get-syntactic-closure-form form)))
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
  (define (walk-internal-define environment form-src variable)
    (receive (name specifier value parameters) (jazz.parse-define walker resume declaration (%%cdr (jazz.source-code form-src)))
      (let ((type (if specifier (jazz.walk-specifier walker resume declaration environment specifier) jazz.Any)))
        (jazz.new-internal-define variable (jazz.walk walker resume declaration environment value)))))
  
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
                                    (%%cdr (jazz.source-code form)))
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
                                      (walk-internal-define augmented-environment internal-define variable))
                                    internal-defines
                                    (jazz.queue-list variables))
                               (jazz.walk-list walker resume declaration augmented-environment scan))))
          (iter (%%cdr scan)))))))


(define (jazz.parse-define walker resume declaration rest)
  (if (%%symbol? (jazz.source-code (%%car rest)))
      (let ((name (jazz.source-code (%%car rest))))
        (jazz.parse-specifier (%%cdr rest)
          (lambda (specifier rest)
            (values name specifier (%%car rest) #f))))
    (let ((name (jazz.source-code (%%car (jazz.source-code (%%car rest)))))
          (parameters (%%cdr (%%desourcify (%%car rest)))))
      (jazz.parse-specifier (%%cdr rest)
        (lambda (specifier body)
          (let ((specifier-list (if specifier (%%list specifier) '())))
            (values name #f `(lambda ,parameters ,@specifier-list ,@body) parameters)))))))


;;;
;;;; Constant
;;;


(define (jazz.walk-quote walker resume declaration environment form-src)
  (let ((form (strip-syntactic-closures form-src)))
    (let ((expression (%%cadr form)))
      (if (%%null? expression)
          (jazz.new-constant '(quote ()) jazz.Null)
        (jazz.walk-constant walker resume declaration environment expression)))))


(define (jazz.walk-keyword walker keyword)
  (jazz.new-constant keyword jazz.Keyword))


(define (jazz.walk-enumerator walker enumerator)
  (jazz.new-constant (%%list 'quote enumerator) jazz.Symbol))


(define (jazz.walk-constant walker resume declaration environment form-src)
  (let ((form (jazz.source-code form-src)))
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
          ((%%s8vector? form)
           (jazz.new-constant `(quote ,form-src) jazz.S8Vector))
          ((%%u8vector? form)
           (jazz.new-constant `(quote ,form-src) jazz.U8Vector))
          ((%%s16vector? form)
           (jazz.new-constant `(quote ,form-src) jazz.S16Vector))
          ((%%u16vector? form)
           (jazz.new-constant `(quote ,form-src) jazz.U16Vector))
          ((%%s32vector? form)
           (jazz.new-constant `(quote ,form-src) jazz.S32Vector))
          ((%%u32vector? form)
           (jazz.new-constant `(quote ,form-src) jazz.U32Vector))
          ((%%s64vector? form)
           (jazz.new-constant `(quote ,form-src) jazz.S64Vector))
          ((%%u64vector? form)
           (jazz.new-constant `(quote ,form-src) jazz.U64Vector))
          ((%%f32vector? form)
           (jazz.new-constant `(quote ,form-src) jazz.F32Vector))
          ((%%f64vector? form)
           (jazz.new-constant `(quote ,form-src) jazz.F64Vector))
          ((%%values? form)
           (jazz.new-constant `(quote ,form-src) jazz.Values))
          ((%%null? form)
           (jazz.new-constant `(quote ,form-src) jazz.Null))
          ((or (%%box? form)
               (%%eq? form #!optional)
               (%%eq? form #!key)
               (%%eq? form #!rest)
               (%%eq? form #!void))
           (jazz.new-constant `(quote ,form-src) jazz.Any))
          ((jazz.scheme-pair-literal? form)
           (jazz.new-constant `(quote ,form-src) jazz.Pair))
          (else
           (jazz.walk-literal/constant walker resume declaration environment form)))))


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
                   ;; will need to scan vectors when we want jazz literals inside vectors
                   (%%vector? expr)
                   (and (%%pair? expr) (scheme-data? (%%car expr)) (scheme-data? (%%cdr expr)))))))
    (and (%%pair? form)
         (scheme-data? form))))


;;;
;;;; Literal
;;;


(define (jazz.walk-literal/constant walker resume declaration environment literal/constant)
  (let ((module-declaration (%%get-declaration-toplevel declaration))
        (literal? (%%is? literal/constant jazz.Literal)))
    (define (walk-literal/constant walker resume declaration literal/constant)
      (let ((environment (%%cons module-declaration (jazz.walker-environment walker))))
        (jazz.walk walker resume module-declaration environment
          (cond (literal?
                  (let ((name (%%get-literal-name literal/constant))
                        (arguments (%%get-literal-arguments literal/constant)))
                    (let ((constructor-name (%%car (jazz.require-literal-constructor (%%desourcify name)))))
                      `(,constructor-name ,@(map (lambda (arg)
                                                   `(quote ,arg))
                                                 arguments)))))
                ((%%pair? literal/constant)
                 `(cons ',(%%car literal/constant) ',(%%cdr literal/constant)))
                (else
                 (jazz.walk-error walker resume declaration #f "Unable to walk constant: {s}" literal/constant))))))

    ;; calling jazz.get-registered-literal to only register when not already there
    ;; doesnt work directly because some literals are interned and thus can be shared
    (let ((locator (jazz.generate-global-symbol "lit")))
      ;; it is important to register before any subliterals to ensure they come before us
      (let ((info (%%cons locator #f)))
        (%%set-module-declaration-walker-literals module-declaration (%%cons info (%%get-module-declaration-walker-literals module-declaration)))
        (%%set-cdr! info (walk-literal/constant walker resume declaration literal/constant)))
      ;; this way of getting a reference to the literal's class is a quick solution
      (let ((literal-type (if literal?
                              (%%desourcify (%%get-literal-name literal/constant))
                            (jazz.identifier-name (%%get-category-identifier (jazz.class-of literal/constant))))))
        (jazz.new-constant locator (jazz.lookup-reference walker resume declaration environment literal-type))))))


(define (jazz.make-symbolic-chars alist)
  (%%list->table
    (map (lambda (pair)
           (%%cons (%%car pair) (integer->char (%%cdr pair))))
         alist)
    test: eq?))


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
  (let ((module-declaration (%%get-declaration-toplevel declaration)))
    (let ((symbol (jazz.generate-global-symbol suffix)))
      (let ((variable (%%cons symbol value)))
        (jazz.enqueue (%%get-module-declaration-walker-variables module-declaration) variable)
        variable))))


;;;
;;;; Symbol
;;;


(jazz.define-virtual-runtime (jazz.walk-symbol (jazz.Walker walker) resume declaration environment symbol-src))


(jazz.define-method (jazz.walk-symbol (jazz.Walker walker) resume declaration environment symbol-src)
  (let ((symbol (jazz.source-code symbol-src)))
    (cond ((jazz.enumerator? symbol)
           (jazz.walk-enumerator walker symbol))
          (else
           (jazz.walk-symbol-reference walker resume declaration environment symbol-src)))))


(define (jazz.walk-setbang walker resume declaration environment form-src)
  (let ((variable (%%cadr (jazz.source-code form-src)))
        (value (%%car (%%cddr (jazz.source-code form-src)))))
    (if (%%symbol? (jazz.source-code variable))
        (jazz.walk-symbol-assignment walker resume declaration environment variable value)
      (jazz.walk-error walker resume declaration variable "Illegal set! of {s}" (%%desourcify variable)))))


(define (jazz.special-form-name? symbol ls end)
  (and (pair? ls)
       (not (eq? ls end))
       (or (let ((binding (jazz.walk-binding-lookup (car ls) symbol #f)))
             (and binding
                  (jazz.object? binding)
                  (%%class-is? binding jazz.Special-Form)))
           (jazz.special-form-name? symbol (cdr ls) end))))


(define (jazz.lookup-symbol walker resume declaration environment symbol-src)
  (define (lookup-composite walker environment symbol)
    (receive (module-name name) (jazz.split-composite symbol)
      (let ((exported-module-reference (jazz.outline-module module-name)))
        (let ((decl (jazz.lookup-declaration exported-module-reference name jazz.public-access declaration)))
          (if decl
              (if (%%is? decl jazz.Autoload-Declaration)
                  decl
                (jazz.new-autoload-declaration name #f #f (%%get-declaration-toplevel declaration) (jazz.new-module-reference module-name #f)))
            (jazz.walk-error walker resume declaration symbol-src "Unable to find {s} in unit {s}" name module-name))))))
  
  (define (lookup walker environment symbol)
    (if (jazz.composite-name? symbol)
        (lookup-composite walker environment symbol)
      (let ((raw-symbol (unwrap-syntactic-closure symbol)))
        (let lp ((env environment))
          (and (%%pair? env)
               (or (jazz.walk-binding-lookup (%%car env) symbol declaration)
                   (lp (%%cdr env))))))))

  (define (validate-compatibility walker declaration referenced-declaration)
    (if (%%eq? (%%get-declaration-compatibility referenced-declaration) 'deprecated)
        (let ((referenced-locator (%%get-declaration-locator referenced-declaration)))
          (jazz.walk-warning walker declaration symbol-src "Deprecated access to {s}" referenced-locator))))
  
  (let ((referenced-declaration
         (if (syntactic-closure? symbol-src)
             (or (lookup walker environment symbol-src)
                 (lookup walker (%%get-syntactic-closure-environment symbol-src) (syntactic-closure-form symbol-src))
                 (lookup walker (%%get-syntactic-closure-environment symbol-src) (jazz.source-code (syntactic-closure-form symbol-src)))
                 )
           (lookup walker environment (jazz.source-code symbol-src)))))
    (if (and referenced-declaration (%%class-is? referenced-declaration jazz.Declaration))
        (validate-compatibility walker declaration referenced-declaration))
    (if (%%class-is? referenced-declaration jazz.Autoload-Declaration)
        (let ((module (%%get-declaration-toplevel declaration)))
          (jazz.register-autoload-declaration module referenced-declaration)))
    (if (jazz.analysis-mode?)
        (jazz.lookup-analyse walker declaration symbol-src referenced-declaration))
    referenced-declaration))


(jazz.define-virtual-runtime (jazz.lookup-analyse (jazz.Walker walker) declaration symbol-src referenced-declaration))


(jazz.define-method (jazz.lookup-analyse (jazz.Walker walker) declaration symbol-src referenced-declaration)
  #f)


;;;
;;;; Reference
;;;


(define (jazz.walk-symbol-reference walker resume declaration environment symbol-src)
  (let ((binding (jazz.lookup-symbol walker resume declaration environment symbol-src)))
    (if binding
        (begin
          (if (%%class-is? binding jazz.Variable)
              (jazz.walk-binding-referenced binding))
          (jazz.new-reference symbol-src binding))
        (jazz.walk-free-reference walker resume declaration symbol-src))))


(jazz.define-virtual-runtime (jazz.walk-free-reference (jazz.Walker walker) resume declaration symbol-src))


(jazz.define-method (jazz.walk-free-reference (jazz.Walker walker) resume declaration symbol-src)
  (jazz.walk-unresolved walker resume declaration symbol-src))


;;;
;;;; Assignment
;;;


(jazz.define-virtual-runtime (jazz.walk-symbol-assignment (jazz.Walker walker) resume declaration environment symbol-src value))


(jazz.define-method (jazz.walk-symbol-assignment (jazz.Walker walker) resume declaration environment symbol-src value)
  (let ((binding (jazz.lookup-symbol walker resume declaration environment symbol-src)))
    (if binding
        (begin
          (jazz.walk-binding-validate-assignment binding walker resume declaration symbol-src)
          (jazz.new-assignment binding (jazz.walk walker resume declaration environment value)))
      (jazz.walk-free-assignment walker resume declaration symbol-src))))


(jazz.define-virtual-runtime (jazz.walk-free-assignment (jazz.Walker walker) resume declaration symbol-src))


(jazz.define-method (jazz.walk-free-assignment (jazz.Walker walker) resume declaration symbol-src)
  (jazz.walk-unresolved walker resume declaration symbol-src))


;;;
;;;; Form
;;;


(jazz.define-virtual-runtime (jazz.walk-form (jazz.Walker walker) resume declaration environment form-src))


(jazz.define-method (jazz.walk-form (jazz.Walker walker) resume declaration environment form-src)
  (let ((procedure-expr (jazz.source-code (%%car (jazz.source-code form-src)))))
    (let ((binding
           (cond
            ((identifier? procedure-expr)
             (jazz.lookup-symbol walker resume declaration environment procedure-expr))
            ((or (%%class-is? procedure-expr jazz.Special-Form)
                 (%%class-is? procedure-expr jazz.Declaration))
             procedure-expr)
            ((%%class-is? procedure-expr jazz.Reference)
             (let ((ref (%%get-reference-binding procedure-expr)))
               (and (or (%%class-is? ref jazz.Special-Form)
                        (%%class-is? ref jazz.Declaration))
                    ref)))
            (else #f))))
      ;; special form
      (if (and binding (jazz.walk-binding-walkable? binding))
          (jazz.walk-binding-walk-form binding walker resume declaration environment form-src)
          ;; macro
          (if (and binding (jazz.walk-binding-expandable? binding))
              (let ((expansion (jazz.walk-binding-expand-form binding walker resume declaration environment form-src)))
                (jazz.walk walker resume declaration environment expansion))
              ;; call
              (jazz.walk-call walker resume declaration environment binding form-src))))))


;;;
;;;; Macro
;;;


(define (jazz.expand-macros walker resume declaration environment form-src)
  (define (lookup-macro-form symbol)
    (let ((binding (jazz.lookup-symbol walker resume declaration environment symbol)))
      (if (and binding (jazz.walk-binding-expandable? binding))
          binding
        #f)))
  
  (if (%%not (%%pair? (jazz.source-code form-src)))
      form-src
    (let ((procedure-expr (jazz.source-code (%%car (jazz.source-code form-src)))))
      (let ((binding (and (%%symbol? procedure-expr) (lookup-macro-form procedure-expr))))
        (if binding
            (let ((expansion (jazz.walk-binding-expand-form binding walker resume declaration environment form-src)))
              (jazz.expand-macros walker resume declaration environment expansion))
          form-src)))))


;;;
;;;; Call
;;;


(define (jazz.walk-call walker resume declaration environment procedure-binding form-src)
  (let ((operator (%%car (jazz.source-code form-src)))
        (arguments (%%cdr (jazz.source-code form-src))))
    (if procedure-binding
        (jazz.walk-binding-validate-call procedure-binding walker resume declaration operator (jazz.desourcify-all arguments) form-src))
    (jazz.new-call form-src
                   (continuation-capture
                     (lambda (resume)
                       (jazz.walk walker resume declaration environment operator)))
                   (jazz.walk-list walker resume declaration environment arguments))))


(jazz.define-virtual-runtime (jazz.validate-arguments (jazz.Walker walker) resume source-declaration declaration signature arguments form-src))


(jazz.define-method (jazz.validate-arguments (jazz.Walker walker) resume source-declaration declaration signature arguments form-src)
  (let ((mandatory (%%get-signature-mandatory signature))
        (rest (%%get-signature-rest signature))
        (passed (%%length arguments))
        (name (%%get-lexical-binding-name declaration))
        ;; todo improve handling of optional and named parameters
        (rest? (or (%%get-signature-rest signature)
                   (%%not-null? (%%get-signature-optional signature))
                   (%%not-null? (%%get-signature-named signature)))))
    (cond ((and (%%not rest?) (%%fx> passed mandatory))
           (jazz.walk-error walker resume source-declaration form-src "Too many arguments for: {a}" name))
          ((%%fx< passed mandatory)
           (jazz.walk-error walker resume source-declaration form-src "Not enough arguments for: {a}" name)))))


;;;
;;;; Require
;;;


(define (jazz.walk-require-declaration walker resume declaration environment form-src)
  (let ((form (%%desourcify form-src)))
    (let ((module-declaration (%%get-declaration-toplevel declaration)))
      (let ((requires (jazz.filter-features (%%cdr form))))
        ;; so units with errors are not added to the module when evaluating code
        (%%when (%%eq? (jazz.walk-for) 'eval)
          (for-each jazz.load-unit requires))
        (for-each (lambda (require)
                    (jazz.add-module-require module-declaration (jazz.listify require)))
                  requires)))))


(define (jazz.walk-require walker resume declaration environment form-src)
  (jazz.new-begin #f '()))


;;;
;;;; Export
;;;


(define (jazz.walk-export-declaration walker resume declaration environment form-src)
  (define (walk-exports exports)
    (let ((partition (jazz.partition exports symbol? assv)))
      (let ((symbols-exports (%%assq #t partition))
            (module-exports (%%assq #f partition)))
        (%%append (if symbols-exports
                      (%%list (jazz.new-export-invoice #f #f #f '() (map (lambda (symbol) (jazz.new-export-reference symbol #f #f)) (%%cdr symbols-exports)) #f))
                    '())
                  (if module-exports
                      (map (lambda (export)
                             (jazz.walk-module-export walker export))
                           (%%cdr module-exports))
                    '())))))
  
  (let ((form (%%desourcify form-src)))
    (let ((module-declaration (%%get-declaration-toplevel declaration)))
      (let ((export-invoices (walk-exports (jazz.filter-features (%%cdr form)))))
        (for-each (lambda (export-invoice)
                    (jazz.add-module-export module-declaration export-invoice))
                  export-invoices)))))


(define (jazz.walk-export walker resume declaration environment form-src)
  (jazz.new-begin #f '()))


;;;
;;;; Import
;;;


(define (jazz.walk-import-declaration walker resume declaration environment form-src)
  (define (jazz.walk-module-import import)
    (define (jazz.lookup-module name)
      (or (jazz.outline-module name error?: #f)
          (jazz.walk-unresolved walker resume declaration name)))
    
    (receive (module-name module-load module-phase module-version module-only module-autoload) (jazz.parse-module-invoice import)
      (jazz.new-import-invoice module-name
                               (jazz.lookup-module module-name)
                               module-phase
                               module-version
                               (if (%%not module-only)
                                   #f
                                 (map (lambda (symbol)
                                        (jazz.new-export-reference symbol #f #f))
                                      module-only)))))
  
  (define (walk-imports imports)
    (map (lambda (import)
           (jazz.walk-module-import (jazz.listify import)))
         imports))
  
  (let ((form (%%desourcify form-src)))
    (let ((module-declaration (%%get-declaration-toplevel declaration)))
      (let ((import-invoices (walk-imports (jazz.filter-features (%%cdr form)))))
        ;; so units with errors are not added to the module when evaluating code
        (%%when (%%eq? (jazz.walk-for) 'eval)
          (for-each (lambda (import-invoice)
                      (let ((module-declaration (%%get-module-invoice-module import-invoice)))
                        (jazz.load-unit (%%get-lexical-binding-name module-declaration))))
                    import-invoices))
        (for-each (lambda (import-invoice)
                    (jazz.add-module-import module-declaration import-invoice #t))
                  import-invoices)))))


(define (jazz.walk-import walker resume declaration environment form-src)
  (jazz.new-begin #f '()))


;;;
;;;; Native
;;;


(define jazz.native-modifiers
  '(((private protected package public) . public)
    ((deprecated undocumented uptodate) . uptodate)))

(define jazz.native-keywords
  '())


(define (jazz.parse-native walker resume declaration rest)
  (receive (access compatibility rest) (jazz.parse-modifiers walker resume declaration jazz.native-modifiers rest)
    (let ((name (%%car rest)))
      (jazz.parse-specifier (%%cdr rest)
        (lambda (specifier rest)
          (%%assert (%%null? rest)
            (values name specifier access compatibility)))))))


(define (jazz.walk-native-declaration walker resume declaration environment form-src)
  (let ((form (%%desourcify form-src)))
    (receive (name specifier access compatibility) (jazz.parse-native walker resume declaration (%%cdr form))
      (receive (name symbol) (jazz.parse-exported-symbol declaration name)
        (let ((type (if specifier (jazz.walk-specifier walker resume declaration environment specifier) jazz.Any)))
          (let ((new-declaration (or (jazz.find-child-declaration declaration name)
                                     (jazz.new-export-declaration name type access compatibility '() declaration symbol))))
            (let ((effective-declaration (jazz.add-declaration-child walker resume declaration new-declaration)))
              effective-declaration)))))))


(define (jazz.walk-native walker resume declaration environment form-src)
  (let ((form (%%desourcify form-src)))
    (receive (name specifier access compatibility) (jazz.parse-native walker resume declaration (%%cdr form))
      (receive (name symbol) (jazz.parse-exported-symbol declaration name)
        (jazz.require-declaration declaration name)))))


;;;
;;;; Native Syntax
;;;


(define jazz.native-syntax-modifiers
  '(((private protected package public) . public)
    ((deprecated undocumented uptodate) . uptodate)))

(define jazz.native-syntax-keywords
  '())


(define (jazz.parse-native-syntax walker resume declaration rest)
  (receive (access compatibility rest) (jazz.parse-modifiers walker resume declaration jazz.native-syntax-modifiers rest)
    (let ((name (%%car rest)))
      (jazz.parse-specifier (%%cdr rest)
        (lambda (specifier rest)
          (%%assert (%%null? rest)
            (values name specifier access compatibility)))))))


(define (jazz.walk-native-syntax-declaration walker resume declaration environment form-src)
  (let ((form (%%desourcify form-src)))
    (receive (name specifier access compatibility) (jazz.parse-native-syntax walker resume declaration (%%cdr form))
      (receive (name symbol) (jazz.parse-exported-symbol declaration name)
        (let ((type (if specifier (jazz.walk-specifier walker resume declaration environment specifier) jazz.Any)))
          (let ((new-declaration (or (jazz.find-child-declaration declaration name)
                                     (jazz.new-export-syntax-declaration name type access compatibility '() declaration symbol))))
            (let ((effective-declaration (jazz.add-declaration-child walker resume declaration new-declaration)))
              effective-declaration)))))))


(define (jazz.walk-native-syntax walker resume declaration environment form-src)
  (let ((form (%%desourcify form-src)))
    (receive (name specifier access compatibility) (jazz.parse-native-syntax walker resume declaration (%%cdr form))
      (receive (name symbol) (jazz.parse-exported-symbol declaration name)
        (jazz.require-declaration declaration name)))))


;;;
;;;; Macro
;;;


(define jazz.macro-modifiers
  '(((private protected package public) . private)
    ((deprecated undocumented uptodate) . uptodate)))


(define (jazz.parse-macro walker resume declaration rest)
  (receive (access compatibility rest) (jazz.parse-modifiers walker resume declaration jazz.macro-modifiers rest)
    (let* ((signature (jazz.source-code (%%car rest)))
           (body (%%cdr rest))
           (name (%%desourcify (%%car signature)))
           (type jazz.Any)
           (parameters (%%cdr signature)))
      (values name type access compatibility parameters body))))


(define (jazz.walk-macro-declaration walker resume declaration environment form-src)
  (receive (name type access compatibility parameters body) (jazz.parse-macro walker resume declaration (%%cdr (jazz.source-code form-src)))
    (let ((signature (jazz.walk-parameters walker resume declaration environment parameters #f #f)))
      (let ((new-declaration (or (jazz.find-child-declaration declaration name)
                                 (jazz.new-macro-declaration name type access compatibility '() declaration signature))))
        (%%set-declaration-source new-declaration form-src)
        (let ((effective-declaration (jazz.add-declaration-child walker resume declaration new-declaration)))
          effective-declaration)))))


(define (jazz.walk-macro walker resume declaration environment form-src)
  (receive (name type access compatibility parameters body) (jazz.parse-macro walker resume declaration (%%cdr (jazz.source-code form-src)))
    (let* ((new-declaration (jazz.require-declaration declaration name)))
      (receive (signature augmented-environment) (jazz.walk-parameters walker resume declaration environment parameters #f #t)
        (%%set-macro-declaration-signature new-declaration signature)
        (%%set-macro-declaration-body new-declaration
          (jazz.walk-body walker resume new-declaration augmented-environment body))
        (%%set-declaration-source new-declaration form-src)
        new-declaration))))


;;;
;;;; Syntax
;;;


(define jazz.syntax-modifiers
  '(((private protected package public) . private)
    ((deprecated undocumented uptodate) . uptodate)))


(define (jazz.parse-syntax walker resume declaration rest)
  (receive (access compatibility rest) (jazz.parse-modifiers walker resume declaration jazz.syntax-modifiers rest)
    (let* ((signature (jazz.source-code (%%car rest)))
           (body (%%cdr rest))
           (name (%%desourcify (%%car signature)))
           (type jazz.Any)
           (parameters (%%cdr signature)))
      (values name type access compatibility parameters body))))


(define (jazz.walk-syntax-declaration walker resume declaration environment form-src)
  (receive (name type access compatibility parameters body) (jazz.parse-syntax walker resume declaration (%%cdr (jazz.source-code form-src)))
    (let ((signature (jazz.walk-parameters walker resume declaration environment parameters #f #f)))
      (let ((new-declaration (or (jazz.find-child-declaration declaration name)
                                 (jazz.new-syntax-declaration name type access compatibility '() declaration signature))))
        (%%set-declaration-source new-declaration form-src)
        (let ((effective-declaration (jazz.add-declaration-child walker resume declaration new-declaration)))
          effective-declaration)))))


(define (jazz.walk-syntax walker resume declaration environment form-src)
  (receive (name type access compatibility parameters body) (jazz.parse-syntax walker resume declaration (%%cdr (jazz.source-code form-src)))
    (let* ((new-declaration (jazz.require-declaration declaration name)))
      (receive (signature augmented-environment) (jazz.walk-parameters walker resume declaration environment parameters #f #t)
        (%%set-syntax-declaration-signature new-declaration signature)
        (%%set-syntax-declaration-body new-declaration
          (jazz.walk-body walker resume new-declaration augmented-environment body))
        (%%set-declaration-source new-declaration form-src)
        new-declaration))))


;;;
;;;; Define-Syntax
;;;


(define (jazz.walk-define-syntax-declaration walker resume declaration environment form-src)
  (receive (access compatibility rest) (jazz.parse-modifiers walker resume declaration jazz.syntax-modifiers (%%cdr (jazz.source-code form-src)))
    (let ((name (jazz.source-code (%%car rest))))
      (let ((new-declaration
             (or (jazz.find-child-declaration declaration name)
                 (jazz.new-define-syntax-declaration name jazz.Any access compatibility '() declaration '() #f))))
        (%%set-declaration-source new-declaration form-src)
        (let ((effective-declaration (jazz.add-declaration-child walker resume declaration new-declaration)))
          effective-declaration)))))


(define (jazz.walk-define-syntax walker resume declaration environment form-src)
  (receive (access compatibility rest) (jazz.parse-modifiers walker resume declaration jazz.syntax-modifiers (%%cdr (jazz.source-code form-src)))
    (let* ((name (jazz.source-code (%%car rest)))
           (body (%%cdr rest))
           (new-declaration (jazz.require-declaration declaration name)))
      (receive (signature augmented-environment) (jazz.walk-parameters walker resume declaration environment '() #f #t)
        (%%set-syntax-declaration-signature new-declaration signature)
        (%%set-syntax-declaration-body new-declaration
          (jazz.walk-body walker resume new-declaration augmented-environment body))
        (%%set-declaration-source new-declaration form-src)
        new-declaration))))


;;;
;;;; Let-Syntax
;;;


(define (jazz.walk-let-syntax walker resume declaration environment form-src)
  (let* ((bindings-src (%%cadr (unwrap-syntactic-closure form-src)))
         (bindings (unwrap-syntactic-closure bindings-src))
         (body (%%cddr (unwrap-syntactic-closure form-src))))
    (%%assertion (or (%%null? bindings) (%%pair? bindings)) (jazz.walk-error walker resume declaration bindings-src "Ill-formed let-syntax bindings: {s}" bindings)
      (let ((augmented-environment
             (append
              (map
               (lambda (binding)
                 (let* ((name (jazz.source-code (%%car (unwrap-syntactic-closure binding))))
                        (value (%%cadr (unwrap-syntactic-closure binding)))
                        (expander-src (jazz.emit-expression (jazz.walk walker resume declaration environment value) declaration environment))
                        (expander (eval (%%get-code-form expander-src))))
                   (jazz.new-define-syntax-form name expander environment)))
               bindings)
              environment)))
        (jazz.new-begin form-src (jazz.walk-list walker resume declaration augmented-environment (unwrap-syntactic-closure body)))))))


;;;
;;;; Letrec-Syntax
;;;


(define (jazz.walk-letrec-syntax . args)
  (apply jazz.walk-let-syntax args))


;;;
;;;; Parameters
;;;


;; symbol : standard positional parameter
;; (specifier/non-symbol/non-keyword-expression symbol) : dynamic positional parameter
;; (symbol expression) : optional parameter
;; (keyword symbol expression) : named parameter
;; . symbol : rest parameter
(define (jazz.walk-parameters walker resume declaration environment parameters extended? walk?)
  (let ((augmented-environment environment)
        (dynamics (jazz.new-queue))
        (positionals (jazz.new-queue))
        (optionals (jazz.new-queue))
        (keywords (jazz.new-queue)))
    (define (iterate-parameters scan sections)
      (define (augment-environment expression)
        (%%when walk? (set! augmented-environment (%%cons expression augmented-environment))))
      
      (define (parameter-section parameter)
        (if (%%pair? parameter)
            (let ((first (jazz.source-code (%%car parameter))))
              (cond ((or (jazz.specifier? first)
                         ;; quicky support for autoload expression
                         (%%pair? first))
                     'dynamic)
                    ((%%keyword? first)
                     'keyword)
                    (else
                     'optional)))
          'positional))
      
      (define (allowed? section)
        (case section
          ((positional) #t)
          ((dynamic) extended?)
          (else extended?)))
      
      (cond ((%%pair? scan)
             (let* ((parameter-src (%%car scan))
                    (parameter (jazz.source-code parameter-src))
                    (section (parameter-section parameter)))
               (cond ((%%not (allowed? section))
                      (jazz.walk-error walker resume declaration parameter-src "Ill-formed lambda parameter: {s}" (jazz.desourcify parameter-src)))
                     ((%%not (memq section sections))
                      (jazz.walk-error walker resume declaration parameter-src "Misplaced {s} parameter {s} in {s}" section (jazz.desourcify parameter-src) (jazz.desourcify parameters))))
               (case section
                 ((dynamic)
                  ;; should compare specifier to dynamic specifier
                  (let* ((specifier (jazz.source-code (%%car parameter)))
                         (code (if (jazz.specifier? specifier) (jazz.specifier->name specifier) (%%car parameter)))
                         (variable (jazz.source-code (%%cadr parameter)))
                         (dynamic-parameter (jazz.new-dynamic-parameter variable jazz.Any (jazz.walk walker resume declaration augmented-environment code))))
                    (jazz.enqueue dynamics dynamic-parameter)
                    (augment-environment dynamic-parameter))
                  (iterate-parameters (%%cdr scan) (memq section sections)))
                 ((positional)
                  (jazz.parse-specifier (%%cdr scan)
                    (lambda (specifier rest)
                      (let* ((type (if specifier (jazz.walk-specifier walker resume declaration environment specifier) #f))
                             (positional-parameter (jazz.new-parameter parameter type)))
                        (jazz.enqueue positionals positional-parameter)
                        (augment-environment positional-parameter))
                      (iterate-parameters rest (memq section sections)))))
                 ((optional)
                  (jazz.parse-specifier (%%cdr parameter)
                    (lambda (specifier rest)
                      (%%when (%%not (%%fx= (%%length rest) 1))
                        (jazz.walk-error walker resume declaration parameter-src "Ill-formed optional parameter: {s}" (jazz.desourcify parameter-src)))
                      (let ((variable (jazz.source-code (%%car parameter)))
                            (type (if specifier (jazz.walk-specifier walker resume declaration environment specifier) #f))
                            (default (%%car rest)))
                        (let ((optional-parameter (jazz.new-optional-parameter variable type (if walk? (jazz.walk walker resume declaration augmented-environment default) #f))))
                          (jazz.enqueue optionals optional-parameter)
                          (augment-environment optional-parameter)))))
                  (iterate-parameters (%%cdr scan) (memq section sections)))
                 ((keyword)
                  (%%when (%%not (%%pair? (%%cdr parameter)))
                    (jazz.walk-error walker resume declaration parameter-src "Ill-formed keyword parameter: {s}" (jazz.desourcify parameter-src)))
                  (jazz.parse-specifier (%%cddr parameter)
                    (lambda (specifier rest)
                      (%%when (%%not (%%fx= (%%length rest) 1))
                        (jazz.walk-error walker resume declaration parameter-src "Ill-formed keyword parameter: {s}" (jazz.desourcify parameter-src)))
                      (let ((keyword (jazz.source-code (%%car parameter)))
                            (variable (jazz.source-code (%%cadr parameter)))
                            (type (if specifier (jazz.walk-specifier walker resume declaration environment specifier) #f))
                            (default (%%car rest)))
                        (%%when (%%not (%%eq? (%%string->symbol (%%keyword->string keyword)) variable))
                          (jazz.walk-error walker resume declaration parameter-src "Mismatched key/name for keyword parameter: {s}" (jazz.desourcify parameter-src)))
                        (let ((keyword-parameter (jazz.new-named-parameter variable type (if walk? (jazz.walk walker resume declaration augmented-environment default) #f))))
                          (jazz.enqueue keywords keyword-parameter)
                          (augment-environment keyword-parameter)))))
                  (iterate-parameters (%%cdr scan) (memq section sections))))))
            ((%%not (%%null? scan))
             (let ((rest (jazz.desourcify scan)))
               (%%when (%%not (%%symbol? rest))
                 (jazz.walk-error walker resume declaration scan "Ill-formed rest parameter: {s}" rest))
               (let ((parameter-expression (jazz.new-rest-parameter rest jazz.List)))
                 (augment-environment parameter-expression)
                 parameter-expression)))
            (else
             #f)))
    
    (let ((rest (iterate-parameters parameters '(dynamic positional optional keyword))))
      (let ((signature
              (jazz.new-signature
                (append (jazz.queue-list dynamics) (jazz.queue-list positionals))
                (jazz.queue-list optionals)
                (jazz.queue-list keywords)
                rest)))
        (if walk?
            (values signature augmented-environment)
          signature)))))


(define (jazz.emit-signature-casts signature source-declaration environment)
  (let ((queue #f))
    (define (process parameter)
      (let ((type (%%get-lexical-binding-type parameter)))
        ;; simple optimization
        (if (and type (%%neq? type jazz.Any))
            (let ((cast (jazz.emit-parameter-cast (jazz.emit-binding-reference parameter source-declaration environment) type source-declaration environment)))
              (if cast
                  (begin
                    ;; optimize the by far more frequent case of signatures having no types
                    (if (%%not queue)
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


(define (jazz.get-catalog-entry unit-name)
  (%%table-ref jazz.Catalog unit-name #f))


(define (jazz.set-catalog-entry unit-name entry)
  (%%table-set! jazz.Catalog unit-name entry))


(define (jazz.set-catalog-entry-status unit-name status)
  (let ((declaration (let ((entry (jazz.get-catalog-entry unit-name)))
                       (if (%%pair? entry)
                           (%%cdr entry)
                         entry))))
    (jazz.set-catalog-entry unit-name (if status (%%cons status declaration) declaration))))


(define (jazz.release-catalog-entries)
  (jazz.iterate-table jazz.Catalog
    (lambda (unit-name entry)
      (if (%%pair? entry)
          (jazz.set-catalog-entry unit-name (%%cdr entry))))))


(define (jazz.call-with-catalog-entry-lock unit-name thunk)
  (jazz.call-with-load-lock
    (lambda ()
      (dynamic-wind
        (lambda ()
          (jazz.set-catalog-entry-status unit-name ':walking)
          (jazz.push-load-stack ':walk unit-name))
        thunk
        (lambda ()
          (jazz.pop-load-stack)
          (if (%%pair? (jazz.get-catalog-entry unit-name))
              (jazz.set-catalog-entry-status unit-name #f)))))))


(define jazz.outline-feedback
  (make-parameter #f))


(define (jazz.outline-unit unit-name #!key (use-catalog? #t) (error? #t))
  (define (load-toplevel-declaration)
    (let ((src (jazz.find-unit-src unit-name '("jazz" "scm") error?)))
      (if (and (%%not src) (%%not error?))
          #f
        (jazz.with-verbose (jazz.outline-verbose?) "outlining" (jazz.resource-pathname src)
          (lambda ()
            ;; not reading the literals is necessary as reading a literal will load units
            (let ((form (jazz.read-toplevel-form src read-literals?: #f)))
              (parameterize ((jazz.requested-unit-name unit-name)
                             (jazz.requested-unit-resource src)
                             (jazz.walk-for 'interpret)
                             (jazz.generate-symbol-for "%outline^")
                             (jazz.generate-symbol-context unit-name)
                             (jazz.generate-symbol-counter 0))
                (let ((kind (jazz.source-code (%%car (jazz.source-code form)))))
                  (case kind
                    ((unit)
                     (jazz.parse-unit-declaration (%%cdr (jazz.source-code form))))
                    ((module)
                     (jazz.parse-module-declaration (%%cdr (jazz.source-code form)))))))))))))
  
  (if (not use-catalog?)
      (load-toplevel-declaration)
    (let ((entry (jazz.get-catalog-entry unit-name)))
      (let ((status (if (%%pair? entry) (%%car entry) #f))
            (declaration (if (%%pair? entry) (%%cdr entry) entry)))
        (if status
            (jazz.circular-dependency-error unit-name (map cdr jazz.Load-Stack))
          (or declaration
              (jazz.call-with-catalog-entry-lock unit-name
                (lambda ()
                  (let ((feedback (jazz.outline-feedback)))
                    (if feedback
                        (feedback unit-name)))
                  (let ((declaration (load-toplevel-declaration)))
                    (if (%%not declaration)
                        (if error?
                            (jazz.error "Unable to locate unit declaration: {s}" unit-name))
                      (jazz.set-catalog-entry unit-name declaration))
                    declaration)))))))))


(define (jazz.outline-module unit-name #!key (error? #t))
  (let ((declaration (jazz.outline-unit unit-name error?: error?)))
    (if (%%not error?)
        declaration
      (%%assert (%%class-is? declaration jazz.Module-Declaration)
        declaration))))


(define jazz.read-literals?
  (make-parameter #t))


(define (jazz.read-toplevel-form resource #!key (read-literals? #t))
  (let ((source (jazz.resource-pathname resource)))
    (jazz.with-extension-reader (jazz.pathname-extension source)
      (lambda ()
        (let ((char-encoding (jazz.resource-char-encoding resource))
              (eol-encoding 'cr-lf))
          (call-with-input-file (%%list path: source char-encoding: char-encoding eol-encoding: eol-encoding)
            (lambda (port)
              (parameterize ((jazz.read-literals? read-literals?))
                (let ((all (jazz.read-source-all port)))
                  (if (%%null? all)
                      (jazz.error "Found empty unit declaration in {a}" source)
                    (let ((form-src (%%car all))
                          (extraneous? (%%not-null? (%%cdr all))))
                      (if (and (%%pair? (jazz.source-code form-src)) (%%memq (jazz.source-code (%%car (jazz.source-code form-src))) '(unit module)))
                          (if (%%not extraneous?)
                              form-src
                            (jazz.error "Found extraneous expressions after unit declaration in {a}" source))
                        (jazz.error "Found invalid unit declaration in {a}" source)))))))))))))


(define (jazz.walk-unit unit-name)
  (let ((src (jazz.find-unit-src unit-name '("jazz" "scm"))))
    (parameterize ((jazz.requested-unit-name unit-name)
                   (jazz.requested-unit-resource src)
                   (jazz.walk-for 'interpret)
                   (jazz.generate-symbol-for "%")
                   (jazz.generate-symbol-context unit-name)
                   (jazz.generate-symbol-counter 0))
      (let ((form (jazz.read-toplevel-form src)))
        (case (jazz.source-code (%%car (jazz.source-code form)))
          ((unit)
           #f)
          ((module)
           (jazz.walk-module (%%cdr (jazz.source-code form)))))))))


;;;
;;;; Core Dialect
;;;


(jazz.define-class-runtime jazz.Core-Dialect)


(define (jazz.new-core-dialect)
  (jazz.allocate-core-dialect jazz.Core-Dialect '()))


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
  (jazz.allocate-core-walker jazz.Core-Walker '() '() '() (jazz.new-queue) (%%make-table test: eq?) '()))


(jazz.encapsulate-class jazz.Core-Walker))
