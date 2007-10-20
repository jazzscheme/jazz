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
            (%%vector-set! lookups n (%%make-hashtable eq?))
            (iter (%%fx+ n 1)))))
    lookups))


;;;
;;;; Walk Binding
;;;


(jazz.define-class jazz.Walk-Binding jazz.Type () jazz.Object-Class
  ())


(jazz.define-method (jazz.emit-type (jazz.Walk-Binding type) environment)
  (%%get-code-form (jazz.emit-binding-reference type environment)))


(jazz.define-virtual (jazz.walk-binding-lookup (jazz.Walk-Binding binding) symbol))
(jazz.define-virtual (jazz.emit-binding-reference (jazz.Walk-Binding binding) environment))
(jazz.define-virtual (jazz.walk-binding-validate-call (jazz.Walk-Binding binding) walker resume source-declaration operator arguments))
(jazz.define-virtual (jazz.emit-binding-call (jazz.Walk-Binding binding) arguments environment))
(jazz.define-virtual (jazz.emit-inlined-binding-call (jazz.Walk-Binding binding) arguments environment))
(jazz.define-virtual (jazz.walk-binding-assignable? (jazz.Walk-Binding binding)))
(jazz.define-virtual (jazz.walk-binding-assigned (jazz.Walk-Binding binding) assignment))
(jazz.define-virtual (jazz.emit-binding-assignment (jazz.Walk-Binding binding) value source-declaration environment))
(jazz.define-virtual (jazz.walk-binding-walkable? (jazz.Walk-Binding binding)))
(jazz.define-virtual (jazz.walk-binding-walk-form (jazz.Walk-Binding binding) walker resume declaration environment form))
(jazz.define-virtual (jazz.walk-binding-expandable? (jazz.Walk-Binding binding)))
(jazz.define-virtual (jazz.walk-binding-expand-form (jazz.Walk-Binding binding) walker resume declaration environment form))


(jazz.define-method (jazz.walk-binding-lookup (jazz.Walk-Binding binding) symbol)
  #f)


(jazz.define-method (jazz.emit-binding-reference (jazz.Walk-Binding binding) environment)
  (jazz.unspecified))


(jazz.define-method (jazz.walk-binding-validate-call (jazz.Walk-Binding binding) walker resume source-declaration operator arguments)
  (jazz.unspecified))


(jazz.define-method (jazz.emit-binding-call (jazz.Walk-Binding binding) arguments environment)
  (let ((type (%%get-lexical-binding-type binding)))
    (jazz.new-code
      `(,(%%get-code-form (jazz.emit-binding-reference binding environment))
        ,@(jazz.codes-forms arguments))
      (jazz.call-return-type type))))


(jazz.define-method (jazz.emit-inlined-binding-call (jazz.Walk-Binding binding) arguments environment)
  #f)


(jazz.define-method (jazz.walk-binding-assignable? (jazz.Walk-Binding binding))
  #f)


(jazz.define-method (jazz.walk-binding-assigned (jazz.Walk-Binding binding) assignment)
  (jazz.unspecified))


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


(jazz.define-method (jazz.walk-binding-lookup (jazz.Declaration binding) symbol)
  (jazz.lookup-declaration binding symbol #f))


(jazz.define-method (jazz.walk-binding-validate-call (jazz.Declaration declaration) walker resume source-declaration operator arguments)
  (jazz.walk-error walker resume source-declaration "{a} is not callable" (%%get-declaration-locator declaration)))


(jazz.define-method (jazz.lookup-declaration (jazz.Declaration declaration) symbol external?)
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


(jazz.define-class jazz.Autoload-Reference jazz.Export-Reference (name declaration library-reference) jazz.Object-Class
  ())


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
  (let ((new-declaration (jazz.allocate-library-declaration jazz.Library-Declaration name #f 'public 'uptodate '() #f parent '() #f (jazz.make-access-lookups jazz.public-access) #f dialect requires exports imports '() (jazz.new-queue) '() '())))
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


(jazz.define-method (jazz.emit-declaration (jazz.Library-Declaration declaration) environment)
  (let ((body-expansion (jazz.emit-statements (%%get-namespace-declaration-body declaration) declaration environment))
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
  (jazz.unspecified))


(jazz.define-method (jazz.emit-declaration (jazz.Export-Declaration declaration) environment)
  `(begin))


(jazz.define-method (jazz.emit-binding-reference (jazz.Export-Declaration declaration) environment)
  (jazz.new-code
    (%%get-export-declaration-symbol declaration)
    jazz.Any))


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


(jazz.define-method (jazz.of-subtype? (jazz.Autoload-Declaration type) subtype)
  ;; quicky to fill later on
  #f)


(jazz.define-method (jazz.resolve-declaration (jazz.Autoload-Declaration declaration))
  (or (%%get-autoload-declaration-declaration declaration)
      (let* ((exported-library (jazz.resolve-reference (%%get-autoload-declaration-exported-library declaration) (%%get-autoload-declaration-library declaration)))
             (name (%%get-lexical-binding-name declaration))
             (decl (jazz.lookup-declaration exported-library name #t)))
        (%%set-autoload-declaration-declaration declaration decl)
        (%%assertion decl (jazz.format "Unable to find autoload: {s}" name)
          decl))))


(jazz.define-method (jazz.emit-binding-reference (jazz.Autoload-Declaration declaration) environment)
  (let ((referenced-declaration (jazz.resolve-declaration declaration)))
    (jazz.new-code
      `(,(jazz.autoload-locator referenced-declaration))
      jazz.Any)))


(define (jazz.autoload-locator referenced-declaration)
  (%%string->symbol (%%string-append (%%symbol->string (%%get-declaration-locator referenced-declaration))
                                     ":autoload")))


(jazz.encapsulate-class jazz.Autoload-Declaration)


;;;
;;;; Void
;;;


(jazz.define-class jazz.Void-Class jazz.Class (name fields virtual-size ancestors descendants ascendant interfaces slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class
  ())


(jazz.define-method (jazz.of-subtype? (jazz.Void-Class type) subtype)
  #f)


(jazz.define-method (jazz.emit-specifier (jazz.Void-Class type))
  'void)


(jazz.encapsulate-class jazz.Void-Class)


(jazz.define-class jazz.Void jazz.Type () jazz.Void-Class
  ())


(jazz.encapsulate-class jazz.Void)


;;;
;;;; Function
;;;


;; will probably be unified with the Signature class


(jazz.define-class jazz.Function-Type jazz.Type () jazz.Class
  (parameters
   result))


(define (jazz.new-function-type parameters result)
  (jazz.allocate-function-type jazz.Function-Type parameters result))


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
                (%%get-function-type-parameters type)))
    (write-char #\: output)
    (display (jazz.emit-specifier (%%get-function-type-result type)) output)
    (%%string->symbol (get-output-string output))))


(jazz.define-method (jazz.emit-check (jazz.Function-Type type) value environment)
  `(if (%%not (%%procedure? ,value))
       (jazz.type-error ,value jazz.Procedure)))


(jazz.encapsulate-class jazz.Function-Type)


;;;
;;;; Template
;;;


;; future work. just here to make sure specifier syntax can express them


(jazz.define-class jazz.Template-Type jazz.Type () jazz.Class
  (class
   types))


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


(jazz.define-class jazz.Nillable-Type jazz.Type () jazz.Class
  (type))


(define (jazz.new-nillable-type type)
  (jazz.allocate-nillable-type jazz.Nillable-Type type))


(jazz.define-method (jazz.of-subtype? (jazz.Nillable-Type type) subtype)
  (or (jazz.of-subtype? jazz.Boolean subtype)
      (jazz.of-subtype? (%%get-nillable-type-type type) subtype)))


(jazz.define-method (jazz.emit-specifier (jazz.Nillable-Type type))
  (let ((type-specifier (jazz.emit-specifier (%%get-nillable-type-type type))))
    (%%string->symbol (%%string-append (%%symbol->string type-specifier) "+"))))


(jazz.define-method (jazz.emit-check (jazz.Nillable-Type type) value environment)
  ;; for tests
  #f)


(jazz.encapsulate-class jazz.Nillable-Type)


;;;
;;;; Any
;;;


(jazz.define-class jazz.Any-Class jazz.Class (name fields virtual-size ancestors descendants ascendant interfaces slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class
  ())


(jazz.define-method (jazz.of-subtype? (jazz.Any-Class type) subtype)
  #t)


(jazz.define-method (jazz.emit-specifier (jazz.Any-Class type))
  'any)


(jazz.define-method (jazz.emit-check (jazz.Any-Class type) value environment)
  #f)


(jazz.encapsulate-class jazz.Any-Class)


(jazz.define-class jazz.Any jazz.Type () jazz.Any-Class
  ())


(jazz.encapsulate-class jazz.Any)


;;;
;;;; Expect
;;;


;; No conversion beeing necessary between any types, expect is only a type check returning the same object


(define (jazz.emit-type-expect code type environment)
  (if (or (%%not type) (%%subtype? (%%get-code-type code) type))
      (%%get-code-form code)
    (let ((value (jazz.generate-symbol "val")))
      `(let ((,value ,(%%get-code-form code)))
         ,(jazz.emit-type-check code type environment #f)
         ,value))))


;;;
;;;; Check
;;;


(define (jazz.emit-type-check code type environment force-check?)
  (if (or (%%not type) (%%eq? type jazz.Any) (and (%%not force-check?) (%%subtype? (%%get-code-type code) type)))
      #f
    (let ((value (%%get-code-form code)))
      (jazz.emit-check type value environment))))


;;;
;;;; Cast
;;;


(define (jazz.emit-parameter-cast code type environment)
  (jazz.emit-type-check code type environment #t))


#; ;; todo
(define (jazz.emit-type-cast code type environment)
  (let ((value (jazz.generate-symbol "val")))
    `(let ((,value ,(%%get-code-form code)))
       ,(jazz.emit-cast type value)
       ,value)))


;;;
;;;; Error
;;;


(define (jazz.type-error value type)
  (jazz.error "{s} expected: {s}" type value))


;;;
;;;; Specifier
;;;


;; <any>
;; <Point<fx>+> ;; Nillable
;; <fx^fx:pair> ;; Function
;; <Object>
;; <Point>
;; <Point<fx>>  ;; Template
;; <pair>
;; <fx>
;; <void>

;; *** missing a specifier syntax for values
;; *** missing a syntax for optional named and rest (uniform type of rest elements) parameters


(define (jazz.parse-specifier lst proc)
  (if (and (%%pair? lst) (jazz.specifier? (%%car lst)))
      (proc (%%car lst) (%%cdr lst))
    (proc #f lst)))


(define (jazz.walk-specifier walker resume declaration environment specifier)
  (let ((input (open-input-string (%%symbol->string specifier))))
    (define (ill-formed)
      ;; temp test to make testing easier
      (if (null? walker)
          (jazz.error "Ill-formed specifier: {s}" specifier)
        (jazz.walk-error walker resume declaration "Ill-formed specifier: {s}" specifier)))
    
    (define (parse-name)
      (let ((output (open-output-string)))
        (let iter ()
          (let ((c (peek-char input)))
            (if (or (%%eof-object? c)
                    (%%eqv? c #\<)
                    (%%eqv? c #\>)
                    (%%eqv? c #\^)
                    (%%eqv? c #\:)
                    (%%eqv? c #\/)
                    (%%eqv? c #\+))
                (let ((name (%%string->symbol (get-output-string output))))
                  (or (jazz.lookup-primitive-type name)
                      (jazz.lookup-reference walker resume declaration environment name)
                      (ill-formed)))
              (begin
                (read-char input)
                (write-char c output)
                (iter)))))))
    
    (define (parse-until separator terminator)
      (let ((queue (jazz.new-queue)))
        (let iter ()
          (if (%%eqv? (peek-char input) terminator)
              (begin
                (read-char input)
                (jazz.queue-list queue))
            (begin
              (jazz.enqueue queue (parse #t))
              (let ((next (peek-char input)))
                (cond ((%%eqv? next separator)
                       (read-char input)
                       (iter))
                      ((%%eqv? next terminator)
                       (iter))
                      (else
                       (ill-formed)))))))))
    
    (define (parse atomic?)
      (if (%%eqv? (peek-char input) #\<)
          (begin
            (read-char input)
            (let ((type (parse #f)))
              (if (%%eqv? (read-char input) #\>)
                  type
                (ill-formed))))
        (if (%%eqv? (peek-char input) #\:)
            (begin
              (read-char input)
              (jazz.new-function-type '() (parse #t)))
          (let ((type (parse-name)))
            (let ((next (peek-char input)))
              (case next
                ((#\+)
                 (read-char input)
                 (jazz.new-nillable-type type))
                ((#\:)
                 (if atomic?
                     type
                   (begin
                     (read-char input)
                     (jazz.new-function-type (list type) (parse #t)))))
                ((#\^)
                 (if atomic?
                     type
                   (begin
                     (read-char input)
                     (let ((parameters (cons type (parse-until #\^ #\:))))
                       (jazz.new-function-type parameters (parse #t))))))
                ((#\<)
                 (read-char input)
                 (jazz.new-template-type type (parse-until #\/ #\>)))
                (else
                 type)))))))
    
    (parse #f)))


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
  (%%make-hashtable eq?))


(%%hashtable-set! jazz.primitive-types 'any       jazz.Any)
(%%hashtable-set! jazz.primitive-types 'bool      jazz.Boolean)
(%%hashtable-set! jazz.primitive-types 'char      jazz.Char)
(%%hashtable-set! jazz.primitive-types 'number    jazz.Number)
(%%hashtable-set! jazz.primitive-types 'int       jazz.Integer)
(%%hashtable-set! jazz.primitive-types 'real      jazz.Real)
(%%hashtable-set! jazz.primitive-types 'fx        jazz.Fixnum)
(%%hashtable-set! jazz.primitive-types 'fl        jazz.Flonum)
(%%hashtable-set! jazz.primitive-types 'list      jazz.List)
(%%hashtable-set! jazz.primitive-types 'null      jazz.Null)
(%%hashtable-set! jazz.primitive-types 'pair      jazz.Pair)
(%%hashtable-set! jazz.primitive-types 'port      jazz.Port)
(%%hashtable-set! jazz.primitive-types 'procedure jazz.Procedure)
(%%hashtable-set! jazz.primitive-types 'foreign   jazz.Foreign)
(%%hashtable-set! jazz.primitive-types 'string    jazz.String)
(%%hashtable-set! jazz.primitive-types 'symbol    jazz.Symbol)
(%%hashtable-set! jazz.primitive-types 'keyword   jazz.Keyword)
(%%hashtable-set! jazz.primitive-types 'vector    jazz.Vector)
(%%hashtable-set! jazz.primitive-types 'hashtable jazz.Hashtable)
(%%hashtable-set! jazz.primitive-types 'void      jazz.Void)


(define (jazz.lookup-primitive-type name)
  (%%hashtable-ref jazz.primitive-types name #f))


;;;
;;;; Macro
;;;


(jazz.define-class jazz.Macro-Declaration jazz.Declaration (name type access compatibility attributes toplevel parent children locator) jazz.Object-Class
  (signature
   body))


(define (jazz.new-macro-declaration name type access compatibility attributes parent signature)
  (let ((new-declaration (jazz.allocate-macro-declaration jazz.Macro-Declaration name type access compatibility attributes #f parent '() #f signature #f)))
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


(jazz.define-class jazz.Syntax-Declaration jazz.Declaration (name type access compatibility attributes toplevel parent children locator) jazz.Object-Class
  (signature
   body))


(define (jazz.new-syntax-declaration name type access compatibility attributes parent signature)
  (let ((new-declaration (jazz.allocate-syntax-declaration jazz.Syntax-Declaration name type access compatibility attributes #f parent '() #f signature #f)))
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


(jazz.define-class jazz.C-Definition-Declaration jazz.Declaration (name type access compatibility attributes toplevel parent children locator) jazz.Object-Class
  (signature
   parameter-types
   result-type
   c-name
   scope
   body))


(define (jazz.new-c-definition-declaration name type access compatibility attributes parent signature parameter-types result-type c-name scope)
  (let ((new-declaration (jazz.allocate-c-definition-declaration jazz.C-Definition-Declaration name type access compatibility attributes #f parent '() #f signature parameter-types result-type c-name scope #f)))
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


(jazz.define-method (jazz.emit-binding-reference (jazz.C-Definition-Declaration declaration) environment)
  (jazz.new-code
    (%%get-declaration-locator declaration)
    jazz.Any))


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
  (mandatory
   positional
   optional
   named
   rest))


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


(jazz.define-class jazz.Symbol-Binding jazz.Lexical-Binding (name type) jazz.Object-Class
  ())


(jazz.encapsulate-class jazz.Symbol-Binding)


;;;
;;;; Variable
;;;


(jazz.define-class jazz.Variable jazz.Symbol-Binding (name type) jazz.Object-Class
  (setters))


(define (jazz.new-variable name type)
  (jazz.allocate-variable jazz.Variable name type '()))


(jazz.define-method (jazz.emit-binding-reference (jazz.Variable binding) environment)
  (jazz.new-code
    (%%get-lexical-binding-name binding)
    (jazz.find-annotated-type binding environment)))


(jazz.define-method (jazz.walk-binding-assignable? (jazz.Variable declaration))
  #t)


(jazz.define-method (jazz.walk-binding-assigned (jazz.Variable binding) assignment)
  (%%set-variable-setters binding (cons assignment (%%get-variable-setters binding))))


(jazz.define-method (jazz.emit-binding-assignment (jazz.Variable binding) value source-declaration environment)
  (let ((value-code (jazz.emit-expression value source-declaration environment)))
    (receive (annotated-frame annotated-variable) (jazz.find-annotated binding environment)
      (jazz.extend-annotated-type annotated-frame annotated-variable (%%get-code-type value-code)))
    (jazz.new-code
      `(set! ,(%%get-lexical-binding-name binding) ,(%%get-code-form value-code))
      jazz.Any)))


(jazz.encapsulate-class jazz.Variable)


;;;
;;;; NextMethod Variable
;;;


(jazz.define-class jazz.NextMethod-Variable jazz.Variable (name type setters) jazz.Object-Class
  ())


(define (jazz.new-nextmethod-variable name type)
  (jazz.allocate-nextmethod-variable jazz.NextMethod-Variable name type '()))


(jazz.define-method (jazz.emit-binding-reference (jazz.NextMethod-Variable binding) environment)
  (let ((name (%%get-lexical-binding-name binding))
        (self (jazz.*self*)))
    (jazz.new-code
      (if self
          `(lambda rest (apply ,name ,self rest))
        name)
      jazz.Any)))


(jazz.encapsulate-class jazz.NextMethod-Variable)


;;;
;;;; Parameter
;;;


(jazz.define-class jazz.Parameter jazz.Variable (name type setters) jazz.Object-Class
  ())


(define (jazz.new-parameter name type)
  (jazz.allocate-parameter jazz.Parameter name type '()))


(jazz.define-virtual (jazz.emit-parameter (jazz.Parameter parameter) declaration environment))


(jazz.define-method (jazz.emit-parameter (jazz.Parameter parameter) declaration environment)
  (%%get-lexical-binding-name parameter))


(jazz.encapsulate-class jazz.Parameter)


;;;
;;;; Dynamic Parameter
;;;


(jazz.define-class jazz.Dynamic-Parameter jazz.Parameter (name type setters) jazz.Object-Class
  (class))


(define (jazz.new-dynamic-parameter name type class)
  (jazz.allocate-dynamic-parameter jazz.Dynamic-Parameter name type '() class))


(jazz.define-method (jazz.emit-parameter (jazz.Dynamic-Parameter parameter) declaration environment)
  (let ((class (%%get-dynamic-parameter-class parameter)))
    (%%list (%%get-code-form (jazz.emit-expression class declaration environment)) (%%get-lexical-binding-name parameter))))


(jazz.encapsulate-class jazz.Dynamic-Parameter)


;;;
;;;; Optional Parameter
;;;


(jazz.define-class jazz.Optional-Parameter jazz.Parameter (name type setters) jazz.Object-Class
  (default))


(define (jazz.new-optional-parameter name type default)
  (jazz.allocate-optional-parameter jazz.Optional-Parameter name type '() default))


(jazz.define-method (jazz.emit-parameter (jazz.Optional-Parameter parameter) declaration environment)
  (let ((default (%%get-optional-parameter-default parameter)))
    (%%list (%%get-lexical-binding-name parameter) (%%get-code-form (jazz.emit-expression default declaration environment)))))


(jazz.encapsulate-class jazz.Optional-Parameter)


;;;
;;;; Named Parameter
;;;


(jazz.define-class jazz.Named-Parameter jazz.Parameter (name type setters) jazz.Object-Class
  (default))


(define (jazz.new-named-parameter name type default)
  (jazz.allocate-named-parameter jazz.Named-Parameter name type '() default))


(jazz.define-method (jazz.emit-parameter (jazz.Named-Parameter parameter) declaration environment)
  (let ((default (%%get-named-parameter-default parameter)))
    (%%list (%%get-lexical-binding-name parameter) (%%get-code-form (jazz.emit-expression default declaration environment)))))


(jazz.encapsulate-class jazz.Named-Parameter)


;;;
;;;; Rest Parameter
;;;


(jazz.define-class jazz.Rest-Parameter jazz.Parameter (name type setters) jazz.Object-Class
  ())


(define (jazz.new-rest-parameter name type)
  (jazz.allocate-rest-parameter jazz.Rest-Parameter name type '()))


(jazz.define-method (jazz.emit-parameter (jazz.Rest-Parameter parameter) declaration environment)
  (%%get-lexical-binding-name parameter))


(jazz.encapsulate-class jazz.Rest-Parameter)


;;;
;;;; Self-Binding
;;;


;; Support for dialects that have an implicit self concept


(jazz.define-class jazz.Self-Binding jazz.Lexical-Binding (name type) jazz.Object-Class
  ())


(define (jazz.new-self-binding type)
  (jazz.allocate-self-binding jazz.Self-Binding 'self type))


(jazz.define-method (jazz.emit-binding-reference (jazz.Self-Binding declaration) environment)
  (jazz.new-code
    'self
    jazz.Any))


(jazz.encapsulate-class jazz.Self-Binding)


;;;
;;;; With-Self
;;;


(define jazz.*self*
  (make-parameter #f))


;;;
;;;; Macro Symbol
;;;


(jazz.define-class jazz.Macro-Symbol jazz.Symbol-Binding (name type) jazz.Object-Class
  (getter
   setter))


(define (jazz.new-macro-symbol name getter setter)
  (jazz.allocate-macro-symbol jazz.Macro-Symbol name #f getter setter))


#; ;; convert to walk / emit
(jazz.define-method (jazz.emit-binding-reference (jazz.Macro-Symbol binding) environment)
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
;;;; Annotated Variable
;;;


(jazz.define-class jazz.Annotated-Variable jazz.Object () jazz.Object-Class
  (variable
   type))


(define (jazz.new-annotated-variable variable type)
  (jazz.allocate-annotated-variable jazz.Annotated-Variable variable type))


(jazz.encapsulate-class jazz.Annotated-Variable)


;;;
;;;; Annotated Frame
;;;


(jazz.define-class jazz.Annotated-Frame jazz.Object () jazz.Object-Class
  (variables
   reset))


(define (jazz.new-annotated-frame variables reset)
  (jazz.allocate-annotated-frame jazz.Annotated-Frame variables reset))


(jazz.encapsulate-class jazz.Annotated-Frame)


;;;
;;;; Code
;;;


(jazz.define-class jazz.Code jazz.Object () jazz.Object-Class
  (form
   type))


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
      (let ((type (or (%%get-lexical-binding-type parameter) jazz.Any)))
        (jazz.enqueue queue (jazz.new-annotated-variable parameter type))))
    (for-each annotate positional)
    (for-each annotate optional)
    (for-each annotate named)
    (%%when rest
      (annotate rest))
    (jazz.queue-list queue)))


(define (jazz.annotate-bindings bindings)
  (map (lambda (binding)
         (let ((variable (%%car binding))
               (value (%%cdr binding)))
           (let ((type (or (%%get-lexical-binding-type variable) jazz.Void)))
             (jazz.new-annotated-variable variable type))))
       bindings))


(define (jazz.annotate-receive parameters)
  (map (lambda (parameter)
         (let ((type (or (%%get-lexical-binding-type parameter) jazz.Any)))
           (jazz.new-annotated-variable parameter type)))
       parameters))


(define (jazz.annotate-internal-defines internal-defines)
  (map (lambda (internal-define)
         (let ((variable (%%get-internal-define-variable internal-define)))
           (let ((type (or (%%get-lexical-binding-type variable) jazz.Any)))
             (jazz.new-annotated-variable variable type))))
       internal-defines))


(define (jazz.with-annotated-frame variables proc)
  (let ((reset #f))
    (call/cc
      (lambda (k)
        (set! reset k)))
    (proc (jazz.new-annotated-frame variables reset))))


(define (jazz.find-annotated variable environment)
  (let iter-frames ((frames environment))
    (if (%%null? frames)
        #f
      (let ((annotated-frame (%%car frames)))
        (or (let iter-variables ((variables (%%get-annotated-frame-variables annotated-frame)))
              (if (%%null? variables)
                  #f
                (let ((annotated-variable (%%car variables)))
                  (if (%%eq? (%%get-annotated-variable-variable annotated-variable) variable)
                      (values annotated-frame annotated-variable)
                    (iter-variables (%%cdr variables))))))
            (iter-frames (%%cdr frames)))))))


(define (jazz.find-annotated-type variable environment)
  (receive (frame variable) (jazz.find-annotated variable environment)
    (%%get-annotated-variable-type variable)))


(define (jazz.extend-annotated-type frame variable new-type)
  (let ((actual-type (%%get-annotated-variable-type variable)))
    ;; this should not happen as variables are always created initialized
    (if (%%eq? actual-type jazz.Void)
        (%%set-annotated-variable-type variable new-type)
      (%%when (%%not (%%subtype? new-type actual-type))
        ;; should probably just call jazz.type-union but it currently
        ;; bugs recursively on unioning function types together...
        (let ((extended-type
                (if (%%subtype? actual-type new-type)
                    new-type
                  ;; should find the most specific common supertype
                  jazz.Any)))
          (%%set-annotated-variable-type variable jazz.Any)
          ;;(jazz.debug 'reset (%%get-lexical-binding-name (%%get-annotated-variable-variable variable)) actual-type new-type extended-type)
          (let ((reset (%%get-annotated-frame-reset frame)))
            (reset #f)))))))


(define (jazz.type-union type1 type2)
  (cond ((or (%%not type1) (%%not type2))
         jazz.Any)
        ((%%subtype? type1 type2)
         type2)
        ((%%subtype? type2 type1)
         type1)
        (else
         ;; should find the most specific common supertype
         jazz.Any)))


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
    (if (%%neq? name (jazz.requested-module-name))
        (jazz.error "Library at {s} is defining {s}" (jazz.requested-module-name) name)
      (let ((walker (jazz.dialect-walker (jazz.load-dialect dialect-name))))
        (jazz.walk-library-declaration walker name dialect-name requires exports imports body)))))


(define (jazz.walk-library-declaration walker name dialect-name requires exports imports body)
  (let ((exports (%%reverse (jazz.walk-library-exports walker exports)))
        (imports (%%reverse (jazz.walk-library-imports walker (jazz.add-dialect-import imports dialect-name)))))
    (let ((declaration (jazz.new-library-declaration name #f dialect-name requires exports imports)))
      (jazz.load-library-syntax declaration)
      (jazz.setup-library-lookups declaration)
      (jazz.walk-declarations walker #f declaration (%%cons declaration (jazz.walker-environment walker)) body #t)
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
  (jazz.emit-declaration (jazz.walk-library partial-form) '()))


(define (jazz.walk-library partial-form)
  (receive (name dialect-name requires exports imports body) (jazz.parse-library partial-form)
    (if (%%neq? name (jazz.requested-module-name))
        (jazz.error "Library at {s} is defining {s}" (jazz.requested-module-name) name)
      (let* ((dialect (jazz.load-dialect dialect-name))
             (walker (jazz.dialect-walker dialect))
             (resume #f)
             (declaration (jazz.walk-library-declaration walker name dialect-name requires exports imports body))
             (environment (%%cons declaration (jazz.walker-environment walker)))
             (body (jazz.walk-toplevel walker resume declaration environment body)))
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


(define (jazz.walk-toplevel walker resume declaration environment form-list)
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
           `(define ,symbol ,value)))
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
                    ,(%%get-code-form (jazz.emit-binding-reference referenced-declaration environment))))))))
       (%%get-library-declaration-autoloads library-declaration)))


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


(define (jazz.walk-declarations walker resume declaration environment forms toplevel?)
  (define (walk forms)
    (for-each (lambda (form)
                (call/cc
                  (lambda (resume)
                    (let ((expansion (jazz.expand-macros walker resume declaration environment (if toplevel? (jazz.cond-expand form) form))))
                      (if (jazz.begin-form? expansion)
                          (walk (%%cdr expansion))
                        (jazz.walk-declaration walker resume declaration environment expansion))))))
              forms))
  
  (walk forms))


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
;;;; Expression
;;;


(jazz.define-class jazz.Expression jazz.Object () jazz.Object-Class
  (type))


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
;;;; Constant
;;;


(jazz.define-class jazz.Constant jazz.Expression (type) jazz.Object-Class
  (expansion))


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
;;;; Quasiquote
;;;


(jazz.define-class jazz.Quasiquote jazz.Expression (type) jazz.Object-Class
  (form))


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


(jazz.define-class jazz.Reference jazz.Expression (type) jazz.Object-Class
  (binding))


(define (jazz.new-reference binding)
  (jazz.allocate-reference jazz.Reference #f binding))


(jazz.define-method (jazz.emit-expression (jazz.Reference expression) declaration environment)
  (jazz.emit-binding-reference (%%get-reference-binding expression) environment))


(jazz.define-method (jazz.emit-call (jazz.Reference expression) arguments declaration environment)
  (jazz.emit-binding-call (%%get-reference-binding expression) arguments environment))


(jazz.encapsulate-class jazz.Reference)


;;;
;;;; Assignment
;;;


(jazz.define-class jazz.Assignment jazz.Expression (type) jazz.Object-Class
  (binding
   value))


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


(jazz.define-class jazz.Lambda jazz.Expression (type) jazz.Object-Class
  (signature
   body))


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
                   ,@(jazz.emit-signature-casts signature augmented-environment)
                   ,(jazz.simplify-begin (jazz.emit-type-expect (jazz.new-code `(begin ,@(%%get-code-form body-code)) (%%get-code-type body-code)) type environment)))
                (jazz.new-function-type '() (%%get-code-type body-code))))))))))


(jazz.define-method (jazz.fold-expression (jazz.Lambda expression) f k s)
  (f expression
     (k (%%get-lambda-signature expression)
        (k (jazz.fold-statement (%%get-lambda-body expression) f k s)
           s))))


(jazz.encapsulate-class jazz.Lambda)


;;;
;;;; Let
;;;


(jazz.define-class jazz.Let jazz.Expression (type) jazz.Object-Class
  (bindings
   body))


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
                             `(,(%%get-lexical-binding-name variable) ,(jazz.emit-type-expect value-code (%%get-lexical-binding-type variable) environment)))))
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


(jazz.define-class jazz.Named-Let jazz.Let (type bindings body) jazz.Object-Class
  (variable))


(define (jazz.new-named-let variable bindings body)
  (jazz.allocate-named-let jazz.Named-Let #f bindings body variable))


(jazz.define-method (jazz.emit-expression (jazz.Named-Let expression) declaration environment)
  (let ((variable (%%get-named-let-variable expression))
        (bindings (%%get-let-bindings expression))
        (body (%%get-let-body expression)))
    (jazz.with-annotated-frame (cons (jazz.new-annotated-variable variable jazz.Any) (jazz.annotate-bindings bindings))
      (lambda (frame)
        (let ((variables (%%get-annotated-frame-variables frame))
              (augmented-environment (cons frame environment)))
          (let ((bindings-output
                  (map (lambda (binding annotated-variable)
                         (let ((variable (%%car binding))
                               (value (%%cdr binding)))
                           (let ((value-code (jazz.emit-expression value declaration augmented-environment)))
                             (jazz.extend-annotated-type frame annotated-variable (%%get-code-type value-code))
                             `(,(%%get-lexical-binding-name variable) ,(jazz.emit-type-expect value-code (%%get-lexical-binding-type variable) environment)))))
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


(jazz.define-class jazz.Letstar jazz.Expression (type) jazz.Object-Class
  (bindings
   body))


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
                             `(,(%%get-lexical-binding-name variable) ,(jazz.emit-type-expect value-code (%%get-lexical-binding-type variable) environment)))))
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


(jazz.define-class jazz.Letrec jazz.Expression (type) jazz.Object-Class
  (bindings
   body))


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
                             `(,(%%get-lexical-binding-name variable) ,(jazz.emit-type-expect value-code (%%get-lexical-binding-type variable) environment)))))
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


(jazz.define-class jazz.Receive jazz.Expression (type) jazz.Object-Class
  (variables
   expression
   body))


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


(jazz.define-class jazz.Body jazz.Expression (type) jazz.Object-Class
  (internal-defines
   expressions))


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


(jazz.define-class jazz.Internal-Define jazz.Expression (type) jazz.Object-Class
  (variable
   value))


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


(jazz.define-class jazz.Begin jazz.Expression (type) jazz.Object-Class
  (expressions))


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
;;;; Call
;;;


(jazz.define-class jazz.Call jazz.Expression (type) jazz.Object-Class
  (operator
   arguments))


(define (jazz.new-call operator arguments)
  (jazz.allocate-call jazz.Call #f operator arguments))


(jazz.define-method (jazz.emit-expression (jazz.Call expression) declaration environment)
  (let ((operator (%%get-call-operator expression))
        (arguments (jazz.emit-expressions (%%get-call-arguments expression) declaration environment)))
    (or (jazz.emit-inlined-primitive operator arguments environment)
        (jazz.emit-inlined-call operator arguments environment)
        (jazz.emit-call operator arguments declaration environment))))


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
;;;; Inlined Primitive
;;;


(define jazz.inline-patterns
  (%%make-hashtable eq?))


;; We should really expand using %% primitives but this cannot be used directly as we want for
;; instance %%+ to expand into ##+ or + depending on safety level but because the code is expanded
;; in Jazz code, the + would get captured as we have no way of safely saying scheme.+ in Scheme...


;; To make this alot more clean would necessitate moving the inline into the walk phase so that the
;; result of inlining can be Jazz code. With this we could inline for instance (##length x) and ##length
;; would simply be an external typed as <list:int> which would do all type propagation automatically.
;; This is really difficult to achieve because as inlining can impact type inference it also needs
;; to be done at emit phase...


(%%hashtable-set! jazz.inline-patterns 'jazz.dialect.language.=      (list (list jazz.Fixnum '##fx=  jazz.Boolean) (list jazz.Flonum '##fl=  jazz.Boolean) (list jazz.Number '##= jazz.Boolean)))
(%%hashtable-set! jazz.inline-patterns 'jazz.dialect.language.<      (list (list jazz.Fixnum '##fx<  jazz.Boolean) (list jazz.Flonum '##fl<  jazz.Boolean) (list jazz.Number '<   jazz.Boolean)))
(%%hashtable-set! jazz.inline-patterns 'jazz.dialect.language.<=     (list (list jazz.Fixnum '##fx<= jazz.Boolean) (list jazz.Flonum '##fl<= jazz.Boolean) (list jazz.Number '<=  jazz.Boolean)))
(%%hashtable-set! jazz.inline-patterns 'jazz.dialect.language.>      (list (list jazz.Fixnum '##fx>  jazz.Boolean) (list jazz.Flonum '##fl>  jazz.Boolean) (list jazz.Number '>   jazz.Boolean)))
(%%hashtable-set! jazz.inline-patterns 'jazz.dialect.language.>=     (list (list jazz.Fixnum '##fx>= jazz.Boolean) (list jazz.Flonum '##fl>= jazz.Boolean) (list jazz.Number '>=  jazz.Boolean)))

(%%hashtable-set! jazz.inline-patterns 'jazz.dialect.language.+      (list (list jazz.Fixnum '##fx+ jazz.Fixnum)  (list jazz.Flonum '##fl+ jazz.Flonum)  (list jazz.Number '##+ jazz.Number)))
(%%hashtable-set! jazz.inline-patterns 'jazz.dialect.language.-      (list (list jazz.Fixnum '##fx- jazz.Fixnum)  (list jazz.Flonum '##fl- jazz.Flonum)  (list jazz.Number '##- jazz.Number)))
(%%hashtable-set! jazz.inline-patterns 'scheme.dialect.kernel.*      (list (list jazz.Fixnum '##fx* jazz.Fixnum)  (list jazz.Flonum '##fl* jazz.Flonum)  (list jazz.Number '##* jazz.Number)))
(%%hashtable-set! jazz.inline-patterns 'scheme.dialect.kernel./      (list                                        (list jazz.Flonum '##fl/ jazz.Flonum)  (list jazz.Number '##/ jazz.Number)))

(%%hashtable-set! jazz.inline-patterns 'scheme.dialect.kernel.car    (list (list jazz.Pair '##car    jazz.Any)))
(%%hashtable-set! jazz.inline-patterns 'scheme.dialect.kernel.cdr    (list (list jazz.Pair '##cdr    jazz.Any)))
(%%hashtable-set! jazz.inline-patterns 'jazz.dialect.language.length (list (list jazz.List '##length jazz.Integer) (list jazz.Vector '##vector-length jazz.Integer) (list jazz.String '##string-length jazz.Integer)))
(%%hashtable-set! jazz.inline-patterns 'jazz.dialect.language.element (list (list jazz.List jazz.Integer 'list-ref jazz.Any) (list jazz.Vector jazz.Integer '##vector-ref jazz.Any) (list jazz.String jazz.Integer '##string-ref jazz.Any)))


(define (jazz.emit-inlined-primitive operator arguments environment)
  (define (operator-locator)
    (if (%%is? operator jazz.Reference)
        (let ((binding (%%get-reference-binding operator)))
          (if (%%is? binding jazz.Declaration)
              (%%get-declaration-locator binding)
            #f))
      #f))
  
  (let ((locator (operator-locator)))
    (let ((patterns (%%hashtable-ref jazz.inline-patterns locator #f)))
      (if patterns
          (let ((types (jazz.codes-types arguments)))
            (let iter ((scan patterns))
              (if (%%null? scan)
                  #f
                (jazz.bind (inline-type inline-name inline-result) (%%car scan)
                  (if (jazz.every? (lambda (type)
                                     (and type (%%subtype? type inline-type)))
                                   types)
                      (jazz.new-code
                        `(,inline-name ,@(jazz.codes-forms arguments))
                        inline-result)
                    (iter (%%cdr scan)))))))
        #f))))


;;;
;;;; Inlined Call
;;;


(define (jazz.emit-inlined-call operator arguments environment)
  (if (%%is? operator jazz.Reference)
      (let ((binding (%%get-reference-binding operator)))
        (jazz.emit-inlined-binding-call binding arguments environment))
    #f))


;;;
;;;; If
;;;


(jazz.define-class jazz.If jazz.Expression (type) jazz.Object-Class
  (test
   yes
   no))


(define (jazz.new-if test yes no)
  (jazz.allocate-if jazz.If #f test yes no))


(jazz.define-method (jazz.emit-expression (jazz.If expression) declaration environment)
  (let ((test (jazz.emit-expression (%%get-if-test expression) declaration environment))
        (yes (jazz.emit-expression (%%get-if-yes expression) declaration environment))
        (no (jazz.emit-expression (%%get-if-no expression) declaration environment)))
    (jazz.new-code
      `(if ,(%%get-code-form test)
           ,(%%get-code-form yes)
         ,(jazz.simplify-begin (%%get-code-form no)))
      (jazz.type-union (%%get-code-type yes) (%%get-code-type no)))))


(jazz.define-method (jazz.fold-expression (jazz.If expression) f k s)
  (f expression
     (k (jazz.fold-expression (%%get-if-test expression) f k s)
        (k (jazz.fold-expression (%%get-if-yes expression) f k s)
           (jazz.fold-expressions (%%get-if-no expression) f k s s)))))


(jazz.encapsulate-class jazz.If)


;;;
;;;; Cond
;;;


(jazz.define-class jazz.Cond jazz.Expression (type) jazz.Object-Class
  (clauses))


(define (jazz.new-cond clauses)
  (jazz.allocate-cond jazz.Cond #f clauses))


(jazz.define-method (jazz.emit-expression (jazz.Cond expression) declaration environment)
  (jazz.new-code
    `(cond ,@(map (lambda (clause)
                    (let ((test (%%car clause))
                          (body (%%cdr clause)))
                      `(,(if (%%not test)
                             'else
                           (%%get-code-form (jazz.emit-expression test declaration environment)))
                        ,@(jazz.codes-forms (jazz.emit-expressions body declaration environment)))))
                  (%%get-cond-clauses expression)))
    jazz.Any))


(jazz.define-method (jazz.fold-expression (jazz.Cond expression) f k s)
  #f)


(jazz.encapsulate-class jazz.Cond)


;;;
;;;; Case
;;;


(jazz.define-class jazz.Case jazz.Expression (type) jazz.Object-Class
  (target
   clauses))


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


(jazz.define-class jazz.And jazz.Expression (type) jazz.Object-Class
  (expressions))


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


(jazz.define-class jazz.Or jazz.Expression (type) jazz.Object-Class
  (expressions))


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


(jazz.define-class jazz.C-Include jazz.Expression (type) jazz.Object-Class
  (name))


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


(jazz.define-class jazz.C-Declare jazz.Expression (type) jazz.Object-Class
  (code))


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


(jazz.define-class jazz.C-Initialize jazz.Expression (type) jazz.Object-Class
  (code))


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


(jazz.define-class jazz.C-Function jazz.Expression (type) jazz.Object-Class
  (expansion))


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


(jazz.define-class jazz.Time jazz.Expression (type) jazz.Object-Class
  (expression))


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


(define (jazz.emit-statements statements declaration environment)
  (map (lambda (statement)
         (if (%%is? statement jazz.Declaration)
             (jazz.emit-declaration statement environment)
           (%%get-code-form (jazz.emit-expression statement declaration environment))))
       statements))


(define (jazz.emit-statements-code statements declaration environment)
  (let ((last-type #f))
    (let ((emited
            (map (lambda (statement)
                   (if (%%is? statement jazz.Declaration)
                       (jazz.emit-declaration statement environment)
                     (let ((code (jazz.emit-expression statement declaration environment)))
                       (set! last-type (%%get-code-type code))
                       (%%get-code-form code))))
                 statements)))
      (jazz.new-code emited last-type))))


(define (jazz.fold-statement statement f k s)
  (if (%%is? statement jazz.Declaration)
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
    (cond ((%%is? sta jazz.Constant)
           (%%get-constant-expansion sta))
          ((%%is? sta jazz.Reference)
           (%%get-lexical-binding-name (%%get-reference-binding sta)))
          (else
           (jazz.identifier-name (%%get-category-name (%%class-of sta))))))
  
  (pp
    (jazz.fold-statement (if (integer? statement) (jazz.serial-number->object statement) statement)
      (lambda (sta s)
        (let ((info (cond ((%%is? sta jazz.Declaration)
                           (present-declaration sta))
                          ((%%is? sta jazz.Expression)
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
      (jazz.new-constant name #f))))


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
             (jazz.dispatch 'fold-literal literal))))))


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
        ;; inline false (until compiler support for constants)
        ((%%eq? symbol 'false)
         (jazz.new-constant #f jazz.Boolean))
        ;; inline true (until compiler support for constants)
        ((%%eq? symbol 'true)
         (jazz.new-constant #t jazz.Boolean))
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
    (if (%%is? referenced-declaration jazz.Autoload-Declaration)
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
        (jazz.new-reference binding)
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
        (if (jazz.walk-binding-assignable? binding)
            (let ((assignment (jazz.new-assignment binding (jazz.walk walker resume declaration environment value))))
              (jazz.walk-binding-assigned binding assignment)
              assignment)
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
        (rest (%%get-signature-rest signature))
        (passed (%%length arguments)))
    (if (if rest (%%fx< passed mandatory) (%%not (%%fx= passed mandatory)))
        (let ((locator (%%get-declaration-locator declaration)))
          (jazz.walk-error walker resume source-declaration "Wrong number of arguments to {a} (passed {a} expected{a} {a})"
            locator passed (if rest " at least" "") mandatory)))))


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
        (jazz.add-declaration-child walker resume declaration new-declaration)
        new-declaration)))))


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


(define (jazz.expand-macro walker resume declaration environment . rest)
  (jazz.expand-macro-form walker resume declaration (%%cons 'macro rest)))


(define (jazz.expand-macro-form walker resume declaration form)
  (receive (name type access compatibility parameters body) (jazz.parse-macro walker resume declaration (%%cdr form))
    `(%macro ,name ,type ,access ,compatibility ,parameters ,body)))


(define (jazz.walk-%macro-declaration walker resume declaration environment form)
  (jazz.bind (name type access compatibility parameters body) (%%cdr form)
    (let ((signature (jazz.walk-parameters walker resume declaration environment parameters #f #f)))
      (let ((new-declaration (jazz.new-macro-declaration name type access compatibility '() declaration signature)))
        (jazz.add-declaration-child walker resume declaration new-declaration)
        new-declaration))))


(define (jazz.walk-%macro walker resume declaration environment form)
  (jazz.bind (name type access compatibility parameters body) (%%cdr form)
    (let* ((new-declaration (jazz.find-form-declaration declaration (%%cadr form))))
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


(define (jazz.expand-syntax walker resume declaration environment . rest)
  (jazz.expand-syntax-form walker resume declaration (%%cons 'syntax rest)))


(define (jazz.expand-syntax-form walker resume declaration form)
  (receive (name type access compatibility parameters body) (jazz.parse-syntax walker resume declaration (%%cdr form))
    `(%syntax ,name ,type ,access ,compatibility ,parameters ,body)))


(define (jazz.walk-%syntax-declaration walker resume declaration environment form)
  (jazz.bind (name type access compatibility parameters body) (%%cdr form)
    (let ((signature (jazz.walk-parameters walker resume declaration environment parameters #f #f)))
      (let ((new-declaration (jazz.new-syntax-declaration name type access compatibility '() declaration signature)))
        (jazz.add-declaration-child walker resume declaration new-declaration)
        new-declaration))))


(define (jazz.walk-%syntax walker resume declaration environment form)
  (jazz.bind (name type access compatibility parameters body) (%%cdr form)
    (let* ((new-declaration (jazz.find-form-declaration declaration (%%cadr form))))
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
             (let ((parameter-expression (jazz.new-rest-parameter scan jazz.Any)))
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
                              (let ((type (if specifier (jazz.walk-specifier walker resume declaration environment specifier) jazz.Any)))
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
                                       (variable (%%cadr parameter))
                                       (default (%%car (%%cddr parameter))))
                                   (set! section 'named)
                                   (if (%%eq? (%%string->symbol (%%keyword->string keyword)) variable)
                                       (let ((named-parameter (jazz.new-named-parameter variable jazz.Any (if walk? (jazz.walk walker resume declaration augmented-environment default) #f))))
                                         (jazz.enqueue named named-parameter)
                                         (%%when walk?
                                           (set! augmented-environment (%%cons named-parameter augmented-environment))))
                                     (jazz.walk-error walker resume declaration "Keyword parameter key and name must match: {s}" parameter))))
                                ;; optional parameter
                                (else
                                 (let ((variable (%%car parameter))
                                       (default (%%cadr parameter)))
                                   (set! section 'optional)
                                   (let ((optional-parameter (jazz.new-optional-parameter variable jazz.Any (if walk? (jazz.walk walker resume declaration augmented-environment default) #f))))
                                     (jazz.enqueue optional optional-parameter)
                                     (%%when walk?
                                       (set! augmented-environment (%%cons optional-parameter augmented-environment))))))))
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


(define (jazz.emit-signature-casts signature environment)
  (let ((queue #f))
    (define (process parameter)
      (let ((type (%%get-lexical-binding-type parameter)))
        ;; simple optimisation
        (if (and type (%%neq? type jazz.Any))
            (let ((cast (jazz.emit-parameter-cast (jazz.emit-binding-reference parameter environment) type environment)))
              (if cast
                  (begin
                    ;; optimize the by far more frequent case of signatures having no types
                    (if (not queue)
                        (set! queue (jazz.new-queue)))
                    (jazz.enqueue queue cast)))))))
    
    (for-each process (%%get-signature-positional signature))
    (for-each process (%%get-signature-optional signature))
    (for-each process (%%get-signature-named signature))
    (let ((rest (%%get-signature-rest signature)))
      (if rest
          (process rest)))
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
  (let ((filename (jazz.require-module-source (jazz.determine-module-filename module-name))))
    (define (load-declaration)
      (let ((form (jazz.read-toplevel-form filename)))
        (parameterize ((jazz.requested-module-name module-name))
          (case (%%car form)
            ((module)
             (jazz.parse-module-declaration (%%cdr form)))
            ((library)
             (jazz.parse-library-declaration (%%cdr form)))))))
    
    (jazz.with-verbose jazz.parse-verbose? "parsing" filename
      (lambda ()
        (load-declaration)))))


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
