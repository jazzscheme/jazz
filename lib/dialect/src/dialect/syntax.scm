;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Dialect Syntax
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
;;;  Portions created by the Initial Developer are Copyright (C) 1996-2012
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


(unit protected dialect.syntax


;;;
;;;; Dialect
;;;


(jazz:define-class-syntax jazz:Dialect jazz:Object ()
  ((name         jazz:get-dialect-name         ())
   (declarations jazz:get-dialect-declarations ())
   (bindings     jazz:get-dialect-bindings     ())))


(jazz:define-virtual-syntax (jazz:dialect-walker (jazz:Dialect dialect)))


;;;
;;;; Walk Binding
;;;


(jazz:define-class-syntax jazz:Walk-Binding jazz:Type ()
  ())


(jazz:define-virtual-syntax (jazz:walk-binding-lookup (jazz:Walk-Binding binding) symbol source-declaration))
(jazz:define-virtual-syntax (jazz:walk-binding-referenced (jazz:Walk-Binding binding)))
(jazz:define-virtual-syntax (jazz:walk-binding-validate-call (jazz:Walk-Binding binding) walker resume source-declaration operator arguments form-src))
(jazz:define-virtual-syntax (jazz:walk-binding-validate-assignment (jazz:Walk-Binding binding) walker resume source-declaration symbol-src))
(jazz:define-virtual-syntax (jazz:walk-binding-assignable? (jazz:Walk-Binding binding)))
(jazz:define-virtual-syntax (jazz:walk-binding-walkable? (jazz:Walk-Binding binding)))
(jazz:define-virtual-syntax (jazz:walk-binding-walk-form (jazz:Walk-Binding binding) walker resume declaration environment form))
(jazz:define-virtual-syntax (jazz:walk-binding-expandable? (jazz:Walk-Binding binding)))
(jazz:define-virtual-syntax (jazz:walk-binding-expand-form (jazz:Walk-Binding binding) walker resume declaration environment form))
(jazz:define-virtual-syntax (jazz:emit-binding-symbol (jazz:Walk-Binding binding) source-declaration environment backend))
(jazz:define-virtual-syntax (jazz:emit-binding-reference (jazz:Walk-Binding binding) source-declaration environment backend))
(jazz:define-virtual-syntax (jazz:emit-binding-call (jazz:Walk-Binding binding) binding-src arguments source-declaration environment backend))
(jazz:define-virtual-syntax (jazz:emit-inlined-binding-call (jazz:Walk-Binding binding) arguments call source-declaration environment backend))
(jazz:define-virtual-syntax (jazz:emit-binding-assignment (jazz:Walk-Binding binding) value source-declaration environment backend))


;;;
;;;; Lexical Binding
;;;


(jazz:define-class-syntax jazz:Lexical-Binding jazz:Walk-Binding ()
  ((name jazz:get-lexical-binding-name ())
   (type jazz:get-lexical-binding-type ())
   (hits jazz:get-lexical-binding-hits jazz:set-lexical-binding-hits)))


(jazz:define-virtual-syntax (jazz:resolve-binding (jazz:Lexical-Binding binding)))


;;;
;;;; Declaration
;;;


(jazz:define-class-syntax jazz:Declaration jazz:Lexical-Binding ()
  ((access        jazz:get-declaration-access        ())
   (compatibility jazz:get-declaration-compatibility ())
   (attributes    jazz:get-declaration-attributes    ())
   (toplevel      jazz:get-declaration-toplevel      jazz:set-declaration-toplevel)
   (parent        jazz:get-declaration-parent        jazz:set-declaration-parent)
   (locator       jazz:get-declaration-locator       jazz:set-declaration-locator)
   (source        jazz:get-declaration-source        jazz:set-declaration-source)))


(jazz:define-virtual-syntax (jazz:compose-declaration-locator (jazz:Declaration declaration)))
(jazz:define-virtual-syntax (jazz:lookup-declaration (jazz:Declaration declaration) symbol access source-declaration))
(jazz:define-virtual-syntax (jazz:get-declaration-inclusions (jazz:Declaration declaration)))
(jazz:define-virtual-syntax (jazz:get-nextmethod-signature (jazz:Declaration declaration)))
(jazz:define-virtual-syntax (jazz:emit-declaration (jazz:Declaration declaration) environment backend))
(jazz:define-virtual-syntax (jazz:expand-referenced-declaration (jazz:Declaration declaration)))


;;;
;;;; Declaration Reference
;;;


(jazz:define-class-syntax jazz:Declaration-Reference jazz:Object ()
  ((name        jazz:get-declaration-reference-name        ())
   (declaration jazz:get-declaration-reference-declaration jazz:set-declaration-reference-declaration)))


(jazz:define-virtual-syntax (jazz:resolve-reference (jazz:Declaration-Reference declaration-reference) module-declaration))


;;;
;;;; Module Reference
;;;


(jazz:define-class-syntax jazz:Module-Reference jazz:Declaration-Reference (constructor: jazz:allocate-module-reference)
  ())


;;;
;;;; Export Reference
;;;


(jazz:define-class-syntax jazz:Export-Reference jazz:Declaration-Reference (constructor: jazz:allocate-export-reference)
  ((module-reference jazz:get-export-reference-module-reference ())))


;;;
;;;; Autoload Reference
;;;


(jazz:define-class-syntax jazz:Autoload-Reference jazz:Export-Reference (constructor: jazz:allocate-autoload-reference)
  ())


;;;
;;;; Unit
;;;


(jazz:define-class-syntax jazz:Unit-Declaration jazz:Declaration (constructor: jazz:allocate-unit-declaration)
  ((requires jazz:get-unit-declaration-requires jazz:set-unit-declaration-requires)))


;;;
;;;; Namespace
;;;


(jazz:define-class-syntax jazz:Namespace-Declaration jazz:Declaration ()
  ((lookups  jazz:get-namespace-declaration-lookups  ())
   (children jazz:get-namespace-declaration-children ())
   (body     jazz:get-namespace-declaration-body     jazz:set-namespace-declaration-body)))


(jazz:define-macro (jazz:get-access-lookup namespace-declaration access)
  `(%%vector-ref (jazz:get-namespace-declaration-lookups ,namespace-declaration) ,access))


;;;
;;;; Module
;;;


(jazz:define-class-syntax jazz:Module-Declaration jazz:Namespace-Declaration (constructor: jazz:allocate-module-declaration)
  ((walker          jazz:get-module-declaration-walker          jazz:set-module-declaration-walker)
   (dialect-name    jazz:get-module-declaration-dialect-name    ())
   (dialect-invoice jazz:get-module-declaration-dialect-invoice ())
   (requires        jazz:get-module-declaration-requires        jazz:set-module-declaration-requires)
   (exports         jazz:get-module-declaration-exports         jazz:set-module-declaration-exports)
   (imports         jazz:get-module-declaration-imports         jazz:set-module-declaration-imports)
   (proclaims       jazz:get-module-declaration-proclaims       ())
   (inclusions      jazz:get-module-declaration-inclusions      jazz:set-module-declaration-inclusions)
   (local-macros    jazz:get-module-declaration-local-macros    ())))


;;;
;;;; Module Invoice
;;;


(jazz:define-class-syntax jazz:Module-Invoice jazz:Object ()
  ((name       jazz:get-module-invoice-name    ())
   (module     jazz:get-module-invoice-module  ())
   (phase      jazz:get-module-invoice-phase   ())
   (version    jazz:get-module-invoice-version ())
   (only       jazz:get-module-invoice-only    ())
   (except     jazz:get-module-invoice-except  ())
   (prefix     jazz:get-module-invoice-prefix  ())
   (rename     jazz:get-module-invoice-rename  ())))


;;;
;;;; Export Invoice
;;;


(jazz:define-class-syntax jazz:Export-Invoice jazz:Module-Invoice (constructor: jazz:allocate-export-invoice)
  ((autoload jazz:get-export-invoice-autoload jazz:set-export-invoice-autoload)))


;;;
;;;; Import Invoice
;;;


(jazz:define-class-syntax jazz:Import-Invoice jazz:Module-Invoice (constructor: jazz:allocate-import-invoice)
  ((hit? jazz:get-import-invoice-hit? jazz:set-import-invoice-hit?)))


;;;
;;;; Export
;;;


(jazz:define-class-syntax jazz:Export-Declaration jazz:Declaration (constructor: jazz:allocate-export-declaration)
  ((symbol jazz:get-export-declaration-symbol ())))


;;;
;;;; Export Syntax
;;;


(jazz:define-class-syntax jazz:Export-Syntax-Declaration jazz:Declaration (constructor: jazz:allocate-export-syntax-declaration)
  ((symbol jazz:get-export-syntax-declaration-symbol ())))


;;;
;;;; Autoload
;;;


(jazz:define-class-syntax jazz:Autoload-Declaration jazz:Declaration (constructor: jazz:allocate-autoload-declaration)
  ((module           jazz:get-autoload-declaration-module           ())
   (exported-module  jazz:get-autoload-declaration-exported-module  ())
   (declaration      jazz:get-autoload-declaration-declaration      jazz:set-autoload-declaration-declaration)))


;;;
;;;; Literal
;;;


(jazz:define-class-syntax jazz:Literal jazz:Object (constructor: jazz:allocate-literal)
  ((name      jazz:get-literal-name      ())
   (arguments jazz:get-literal-arguments ())))


;;;
;;;; Macro
;;;


(jazz:define-class-syntax jazz:Macro-Declaration jazz:Declaration (constructor: jazz:allocate-macro-declaration)
  ((signature jazz:get-macro-declaration-signature jazz:set-macro-declaration-signature)
   (body      jazz:get-macro-declaration-body      jazz:set-macro-declaration-body)))


;;;
;;;; Local Macro
;;;


(jazz:define-class-syntax jazz:Local-Macro-Declaration jazz:Declaration (constructor: jazz:allocate-local-macro-declaration)
  ((signature jazz:get-local-macro-declaration-signature jazz:set-local-macro-declaration-signature)
   (body      jazz:get-local-macro-declaration-body      jazz:set-local-macro-declaration-body)))


;;;
;;;; Syntax
;;;


(jazz:define-class-syntax jazz:Syntax-Declaration jazz:Declaration (constructor: jazz:allocate-syntax-declaration)
  ((signature jazz:get-syntax-declaration-signature jazz:set-syntax-declaration-signature)
   (body      jazz:get-syntax-declaration-body      jazz:set-syntax-declaration-body)))


;;;
;;;; Define-Syntax
;;;


(jazz:define-class-syntax jazz:Define-Syntax-Declaration jazz:Syntax-Declaration (constructor: jazz:allocate-define-syntax-declaration)
  ())


;;;
;;;; Define-Local-Syntax
;;;


(jazz:define-class-syntax jazz:Define-Local-Syntax-Declaration jazz:Syntax-Declaration (constructor: jazz:allocate-define-local-syntax-declaration)
  ())


;;;
;;;; Void
;;;


(jazz:define-class-syntax jazz:Void-Class jazz:Class (metaclass: jazz:Class)
  ())


(jazz:define-class-syntax jazz:Void jazz:Type (metaclass: jazz:Void-Class)
  ())


;;;
;;;; Opt Type
;;;


(jazz:define-class-syntax jazz:Opt-Type jazz:Type (metaclass: jazz:Class constructor: jazz:allocate-opt-type)
  ((type jazz:get-opt-type-type ())))


;;;
;;;; Key Type
;;;


(jazz:define-class-syntax jazz:Key-Type jazz:Type (metaclass: jazz:Class constructor: jazz:allocate-key-type)
  ((key  jazz:get-key-type-key  ())
   (type jazz:get-key-type-type ())))


;;;
;;;; Rest Type
;;;


(jazz:define-class-syntax jazz:Rest-Type jazz:Type (metaclass: jazz:Class constructor: jazz:allocate-rest-type)
  ((type jazz:get-rest-type-type ())))


;;;
;;;; Function Type
;;;


(jazz:define-class-syntax jazz:Function-Type jazz:Type (metaclass: jazz:Class constructor: jazz:allocate-function-type)
  ((mandatory  jazz:get-function-type-mandatory  ())
   (positional jazz:get-function-type-positional ())
   (optional   jazz:get-function-type-optional   ())
   (named      jazz:get-function-type-named      ())
   (rest       jazz:get-function-type-rest       ())
   (result     jazz:get-function-type-result     ())))


;;;
;;;; Category Type
;;;


(jazz:define-class-syntax jazz:Category-Type jazz:Type (metaclass: jazz:Class constructor: jazz:allocate-category-type)
  ((declaration jazz:get-category-type-declaration ())))


;;;
;;;; Values Type
;;;


(jazz:define-class-syntax jazz:Values-Type jazz:Type (metaclass: jazz:Class constructor: jazz:allocate-values-type)
  ((types jazz:get-values-type-types ())))


;;;
;;;; Restriction Type
;;;


(jazz:define-class-syntax jazz:Restriction-Type jazz:Type (metaclass: jazz:Class constructor: jazz:allocate-restriction-type)
  ((base jazz:get-restriction-type-base ())
   (type jazz:get-restriction-type-type ())))


;;;
;;;; Complement Type
;;;


(jazz:define-class-syntax jazz:Complement-Type jazz:Type (metaclass: jazz:Class constructor: jazz:allocate-complement-type)
  ((type jazz:get-complement-type-type ())))


;;;
;;;; Union Type
;;;


(jazz:define-class-syntax jazz:Union-Type jazz:Type (metaclass: jazz:Class constructor: jazz:allocate-union-type)
  ((types jazz:get-union-type-types ())))


;;;
;;;; Template Type
;;;


(jazz:define-class-syntax jazz:Template-Type jazz:Type (metaclass: jazz:Class constructor: jazz:allocate-template-type)
  ((class jazz:get-template-type-class ())
   (types jazz:get-template-type-types ())))


;;;
;;;; Nillable Type
;;;


(jazz:define-class-syntax jazz:Nillable-Type jazz:Type (metaclass: jazz:Class constructor: jazz:allocate-nillable-type)
  ((type jazz:get-nillable-type-type ())))


;;;
;;;; Any
;;;


(jazz:define-class-syntax jazz:Any-Class jazz:Class (metaclass: jazz:Class)
  ())


(jazz:define-class-syntax jazz:Any jazz:Type (metaclass: jazz:Any-Class)
  ())


;;;
;;;; Walker
;;;


(jazz:define-class-syntax jazz:Walker jazz:Object ()
  ((declarations jazz:get-walker-declarations jazz:set-walker-declarations)
   (bindings     jazz:get-walker-bindings     jazz:set-walker-bindings)
   (warnings     jazz:get-walker-warnings     jazz:set-walker-warnings)
   (errors       jazz:get-walker-errors       jazz:set-walker-errors)
   (literals     jazz:get-walker-literals     jazz:set-walker-literals)
   (variables    jazz:get-walker-variables    ())
   (references   jazz:get-walker-references   ())
   (autoloads    jazz:get-walker-autoloads    jazz:set-walker-autoloads)))


(jazz:define-virtual-syntax (jazz:walker-declarations (jazz:Walker walker)))
(jazz:define-virtual-syntax (jazz:walker-bindings (jazz:Walker walker)))
(jazz:define-virtual-syntax (jazz:walk-form (jazz:Walker walker) resume declaration environment form))
(jazz:define-virtual-syntax (jazz:walk-symbol (jazz:Walker walker) resume declaration environment symbol-src))
(jazz:define-virtual-syntax (jazz:walk-symbol-assignment (jazz:Walker walker) resume declaration environment symbol-src value))
(jazz:define-virtual-syntax (jazz:validate-proclaim (jazz:Walker walker) resume declaration environment form-src))
(jazz:define-virtual-syntax (jazz:runtime-export (jazz:Walker walker) declaration))
(jazz:define-virtual-syntax (jazz:lookup-analyse (jazz:Walker walker) declaration symbol-src referenced-declaration))


;; provide virtual access to some walker slots via the module-declaration
(define (jazz:get-module-declaration-walker-literals lib-decl)
  (jazz:get-walker-literals (jazz:get-module-declaration-walker lib-decl)))
(define (jazz:set-module-declaration-walker-literals lib-decl value)
  (jazz:set-walker-literals (jazz:get-module-declaration-walker lib-decl) value))
(define (jazz:get-module-declaration-walker-variables lib-decl)
  (jazz:get-walker-variables (jazz:get-module-declaration-walker lib-decl)))
(define (jazz:get-module-declaration-walker-references lib-decl)
  (jazz:get-walker-references (jazz:get-module-declaration-walker lib-decl)))
(define (jazz:get-module-declaration-walker-autoloads lib-decl)
  (jazz:get-walker-autoloads (jazz:get-module-declaration-walker lib-decl)))
(define (jazz:set-module-declaration-walker-autoloads lib-decl value)
  (jazz:set-walker-autoloads (jazz:get-module-declaration-walker lib-decl) value))


;;;
;;;; Walk Context
;;;


(jazz:define-class-syntax jazz:Walk-Context jazz:Object (constructor: jazz:allocate-walk-context)
  ((policy   jazz:get-walk-context-policy   ())
   (locator  jazz:get-walk-context-locator  ())
   (pathname jazz:get-walk-context-pathname ())))


;;;
;;;; Walk Location
;;;


(jazz:define-class-syntax jazz:Walk-Location jazz:Object (constructor: jazz:allocate-walk-location)
  ((unit-locator        jazz:get-walk-location-unit-locator        ())
   (declaration-locator jazz:get-walk-location-declaration-locator ())
   (locat               jazz:get-walk-location-locat               ())
   (path                jazz:get-walk-location-path                ())))


;;;
;;;; Walk Problem
;;;


(jazz:define-class-syntax jazz:Walk-Problem jazz:Error ()
  ((location jazz:get-walk-problem-location ())))


;;;
;;;; Walk Problems
;;;


(jazz:define-class-syntax jazz:Walk-Problems jazz:Error (constructor: jazz:allocate-walk-problems)
  ((warnings jazz:get-walk-problems-warnings ())
   (errors   jazz:get-walk-problems-errors   ())))


;;;
;;;; Walk Warning
;;;


(jazz:define-class-syntax jazz:Walk-Warning jazz:Walk-Problem (constructor: jazz:allocate-walk-warning)
  ())


;;;
;;;; Walk Error
;;;


(jazz:define-class-syntax jazz:Walk-Error jazz:Walk-Problem (constructor: jazz:allocate-walk-error)
  ())


;;;
;;;; Unresolved Error
;;;


(jazz:define-class-syntax jazz:Unresolved-Error jazz:Walk-Error (constructor: jazz:allocate-unresolved-error)
  ((symbol jazz:get-unresolved-error-symbol ())))


;;;
;;;; Walk Frame
;;;


(jazz:define-class-syntax jazz:Walk-Frame jazz:Walk-Binding (constructor: jazz:allocate-walk-frame)
  ((bindings jazz:get-walk-frame-bindings ())))


;;;
;;;; Signature
;;;


(jazz:define-class-syntax jazz:Signature jazz:Object (constructor: jazz:allocate-signature)
  ((mandatory  jazz:get-signature-mandatory  ())
   (positional jazz:get-signature-positional ())
   (optional   jazz:get-signature-optional   ())
   (named      jazz:get-signature-named      ())
   (rest       jazz:get-signature-rest       ())))


;;;
;;;; Symbol Binding
;;;


(jazz:define-class-syntax jazz:Symbol-Binding jazz:Lexical-Binding ()
  ((gensym jazz:get-symbol-binding-gensym jazz:set-symbol-binding-gensym)))


;;;
;;;; Variable
;;;


(jazz:define-class-syntax jazz:Variable jazz:Symbol-Binding (constructor: jazz:allocate-variable)
  ((reference-count jazz:get-variable-reference-count jazz:set-variable-reference-count)))


;;;
;;;; Parameter
;;;


(jazz:define-class-syntax jazz:Parameter jazz:Variable (constructor: jazz:allocate-parameter)
  ())


(jazz:define-virtual-syntax (jazz:emit-parameter (jazz:Parameter parameter) declaration environment backend))


;;;
;;;; Dynamic Parameter
;;;


(jazz:define-class-syntax jazz:Dynamic-Parameter jazz:Parameter (constructor: jazz:allocate-dynamic-parameter)
  ((class jazz:get-dynamic-parameter-class ())))


;;;
;;;; Optional Parameter
;;;


(jazz:define-class-syntax jazz:Optional-Parameter jazz:Parameter (constructor: jazz:allocate-optional-parameter)
  ((default jazz:get-optional-parameter-default jazz:set-optional-parameter-default)))


;;;
;;;; Named Parameter
;;;


(jazz:define-class-syntax jazz:Named-Parameter jazz:Parameter (constructor: jazz:allocate-named-parameter)
  ((default jazz:get-named-parameter-default jazz:set-named-parameter-default)))


;;;
;;;; Rest Parameter
;;;


(jazz:define-class-syntax jazz:Rest-Parameter jazz:Parameter (constructor: jazz:allocate-rest-parameter)
  ())


;;;
;;;; Local-Variable-Binding
;;;


(jazz:define-class-syntax jazz:Local-Variable-Binding jazz:Lexical-Binding (constructor: jazz:allocate-local-variable-binding)
  ((variable jazz:get-local-variable-binding-variable ())))


;;;
;;;; Macro Symbol
;;;


(jazz:define-class-syntax jazz:Macro-Symbol jazz:Symbol-Binding (constructor: jazz:allocate-macro-symbol)
  ((getter jazz:get-macro-symbol-getter ())
   (setter jazz:get-macro-symbol-setter ())))


;;;
;;;; Form Binding
;;;


(jazz:define-class-syntax jazz:Form-Binding jazz:Lexical-Binding ()
  ())


;;;
;;;; Declaration Form
;;;


(jazz:define-class-syntax jazz:Declaration-Form jazz:Form-Binding (constructor: jazz:allocate-declaration-form)
  ((walk jazz:get-declaration-form-walk ())))


;;;
;;;; Special Form
;;;


(jazz:define-class-syntax jazz:Special-Form jazz:Form-Binding (constructor: jazz:allocate-special-form)
  ((walk jazz:get-special-form-walk ())))


;;;
;;;; Macro Form
;;;


(jazz:define-class-syntax jazz:Macro-Form jazz:Form-Binding (constructor: jazz:allocate-macro-form)
  ((expander jazz:get-macro-form-expander ())))


;;;
;;;; Syntax Form
;;;


(jazz:define-class-syntax jazz:Syntax-Form jazz:Form-Binding (constructor: jazz:allocate-syntax-form)
  ((expander jazz:get-syntax-form-expander ())))


;;;
;;;; Define-Syntax Form
;;;


(jazz:define-class-syntax jazz:Define-Syntax-Form jazz:Syntax-Form (constructor: jazz:allocate-define-syntax-form)
  ((environment jazz:get-define-syntax-form-environment ())))


;;;
;;;; Define-Local-Syntax Form
;;;


(jazz:define-class-syntax jazz:Define-Local-Syntax-Form jazz:Syntax-Form (constructor: jazz:allocate-define-local-syntax-form)
  ((environment jazz:get-define-local-syntax-form-environment ())))


;;;
;;;; Syntactic-Closure
;;;


(jazz:define-class-syntax jazz:Syntactic-Closure jazz:Object (constructor: jazz:allocate-syntactic-closure)
  ((environment jazz:get-syntactic-closure-environment ())
   (variables   jazz:get-syntactic-closure-variables   ())
   (form        jazz:get-syntactic-closure-form        ())))


;;;
;;;; Annotated Variable
;;;


(jazz:define-class-syntax jazz:Annotated-Variable jazz:Object (constructor: jazz:allocate-annotated-variable)
  ((variable      jazz:get-annotated-variable-variable      ())
   (declared-type jazz:get-annotated-variable-declared-type ())
   (type          jazz:get-annotated-variable-type          jazz:set-annotated-variable-type)))


;;;
;;;; Restricted Binding
;;;


(jazz:define-class-syntax jazz:Restricted-Binding jazz:Object (constructor: jazz:allocate-restricted-binding)
  ((binding jazz:get-restricted-binding-binding ())
   (type    jazz:get-restricted-binding-type    ())))


;;;
;;;; Annotated Frame
;;;


(jazz:define-class-syntax jazz:Annotated-Frame jazz:Object (constructor: jazz:allocate-annotated-frame)
  ((variables jazz:get-annotated-frame-variables ())
   (reset     jazz:get-annotated-frame-reset     ())))


;;;
;;;; Code
;;;


(jazz:define-class-syntax jazz:Code jazz:Object (constructor: jazz:allocate-code)
  ((form   jazz:get-code-form   ())
   (type   jazz:get-code-type   ())
   (source jazz:get-code-source ())))


;;;
;;;; Slot Access
;;;


;; this should be moved to jazz
(jazz:define-class-syntax jazz:Access jazz:Object (constructor: jazz:allocate-access)
  ((name    jazz:get-access-name    ())
   (context jazz:get-access-context ())))


;;;
;;;; Expression
;;;


(jazz:define-class-syntax jazz:Expression jazz:Object ()
  ((type   jazz:get-expression-type   ())
   (source jazz:get-expression-source ())))


(jazz:define-virtual-syntax (jazz:emit-expression (jazz:Expression expression) declaration environment backend))
(jazz:define-virtual-syntax (jazz:emit-call (jazz:Expression expression) arguments declaration environment backend))


;;;
;;;; Proclaim
;;;


(jazz:define-class-syntax jazz:Proclaim jazz:Expression (constructor: jazz:allocate-proclaim)
  ((clauses jazz:get-proclaim-clauses ())))


;;;
;;;; Constant
;;;


(jazz:define-class-syntax jazz:Constant jazz:Expression (constructor: jazz:allocate-constant)
  ((expansion jazz:get-constant-expansion ())))


;;;
;;;; Delay
;;;


(jazz:define-class-syntax jazz:Delay jazz:Expression (constructor: jazz:allocate-delay)
  ((expression jazz:get-delay-expression ())))


;;;
;;;; Quasiquote
;;;


(jazz:define-class-syntax jazz:Quasiquote jazz:Expression (constructor: jazz:allocate-quasiquote)
  ((form jazz:get-quasiquote-form ())))


;;;
;;;; Binding Reference
;;;


(jazz:define-class-syntax jazz:Binding-Reference jazz:Expression (constructor: jazz:allocate-binding-reference)
  ((binding jazz:get-reference-binding ())))


;;;
;;;; Assignment
;;;


(jazz:define-class-syntax jazz:Assignment jazz:Expression (constructor: jazz:allocate-assignment)
  ((binding jazz:get-assignment-binding ())
   (value   jazz:get-assignment-value   ())))


;;;
;;;; Body
;;;


(jazz:define-class-syntax jazz:Body jazz:Expression (constructor: jazz:allocate-body)
  ((internal-defines jazz:get-body-internal-defines ())
   (expressions      jazz:get-body-expressions      ())))


;;;
;;;; Internal Define
;;;


(jazz:define-class-syntax jazz:Internal-Define jazz:Expression (constructor: jazz:allocate-internal-define)
  ((variable jazz:get-internal-define-variable ())
   (value    jazz:get-internal-define-value    ())))


;;;
;;;; Begin
;;;


(jazz:define-class-syntax jazz:Begin jazz:Expression (constructor: jazz:allocate-begin)
  ((expressions jazz:get-begin-expressions ())))


;;;
;;;; Call
;;;


(jazz:define-class-syntax jazz:Call jazz:Expression (constructor: jazz:allocate-call)
  ((operator  jazz:get-call-operator  ())
   (arguments jazz:get-call-arguments ())))


;;;
;;;; If
;;;


(jazz:define-class-syntax jazz:If jazz:Expression (constructor: jazz:allocate-if)
  ((test jazz:get-if-test ())
   (yes  jazz:get-if-yes  ())
   (no   jazz:get-if-no   ())))


;;;
;;;; Cond
;;;


(jazz:define-class-syntax jazz:Cond jazz:Expression (constructor: jazz:allocate-cond)
  ((clauses jazz:get-cond-clauses ())))


;;;
;;;; Case
;;;


(jazz:define-class-syntax jazz:Case jazz:Expression (constructor: jazz:allocate-case)
  ((target  jazz:get-case-target  ())
   (clauses jazz:get-case-clauses ())))


;;;
;;;; And
;;;


(jazz:define-class-syntax jazz:And jazz:Expression (constructor: jazz:allocate-and)
  ((expressions jazz:get-and-expressions ())))


;;;
;;;; Or
;;;


(jazz:define-class-syntax jazz:Or jazz:Expression (constructor: jazz:allocate-or)
  ((expressions jazz:get-or-expressions ())))


;;;
;;;; Declare
;;;


(jazz:define-class-syntax jazz:Declare jazz:Expression (constructor: jazz:allocate-declare)
  ((declarations jazz:get-declare-declarations ())))


;;;
;;;; Parameterize
;;;


(jazz:define-class-syntax jazz:Parameterize jazz:Expression (constructor: jazz:allocate-parameterize)
  ((bindings jazz:get-parameterize-bindings ())
   (body     jazz:get-parameterize-body     ())))


;;;
;;;; Time Special
;;;


(jazz:define-class-syntax jazz:Time-Special jazz:Expression (constructor: jazz:allocate-time)
  ((expressions jazz:get-time-special-expressions ())))


;;;
;;;; Walk-Failed Special
;;;


(jazz:define-class-syntax jazz:Walk-Failed-Special jazz:Expression (constructor: jazz:allocate-walk-failed)
  ((answer jazz:get-walk-failed-special-answer ())))


;;;
;;;; Analysis Data
;;;


(jazz:define-class-syntax jazz:Analysis-Data jazz:Object (constructor: jazz:allocate-analysis-data)
  ((autoload-reference     jazz:get-analysis-data-autoload-reference     jazz:set-analysis-data-autoload-reference)
   (declaration-references jazz:get-analysis-data-declaration-references jazz:set-analysis-data-declaration-references))))
