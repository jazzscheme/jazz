;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Walker Classes
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


(module protected core.library.syntax.classes


;;;
;;;; Walk Binding
;;;


(jazz.define-class jazz.Walk-Binding jazz.Type () jazz.Object-Class ()
  ())


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


;;;
;;;; Lexical Binding
;;;


(jazz.define-class jazz.Lexical-Binding jazz.Walk-Binding () jazz.Object-Class ()
  ((name %%get-lexical-binding-name ())
   (type %%get-lexical-binding-type ())
   (hits %%get-lexical-binding-hits ())))


;;;
;;;; Declaration
;;;


(jazz.define-class jazz.Declaration jazz.Lexical-Binding (name type hits) jazz.Object-Class ()
  ((access        %%get-declaration-access        ())
   (compatibility %%get-declaration-compatibility ())
   (attributes    %%get-declaration-attributes    ())
   (toplevel      %%get-declaration-toplevel      %%set-declaration-toplevel)
   (parent        %%get-declaration-parent        %%set-declaration-parent)
   (locator       %%get-declaration-locator       %%set-declaration-locator)
   (source        %%get-declaration-source        %%set-declaration-source)))


(jazz.define-virtual (jazz.resolve-declaration (jazz.Declaration declaration)))
(jazz.define-virtual (jazz.lookup-declaration (jazz.Declaration declaration) symbol access))
(jazz.define-virtual (jazz.update-declaration (jazz.Declaration declaration) new-declaration))
(jazz.define-virtual (jazz.get-declaration-inclusions (jazz.Declaration declaration)))
(jazz.define-virtual (jazz.emit-declaration (jazz.Declaration declaration) environment))
(jazz.define-virtual (jazz.expand-referenced-declaration (jazz.Declaration declaration)))
(jazz.define-virtual (jazz.fold-declaration (jazz.Declaration declaration) f k s))


;;;
;;;; Declaration Reference
;;;


(jazz.define-class jazz.Declaration-Reference jazz.Object () jazz.Object-Class ()
  ((name        %%get-declaration-reference-name        ())
   (declaration %%get-declaration-reference-declaration %%set-declaration-reference-declaration)))


(jazz.define-virtual (jazz.resolve-reference (jazz.Declaration-Reference declaration-reference) library-declaration))


;;;
;;;; Library Reference
;;;


(jazz.define-class jazz.Library-Reference jazz.Declaration-Reference (name declaration) jazz.Object-Class jazz.allocate-library-reference
  ())


;;;
;;;; Export Reference
;;;


(jazz.define-class jazz.Export-Reference jazz.Declaration-Reference (name declaration) jazz.Object-Class jazz.allocate-export-reference
  ((library-reference %%get-export-reference-library-reference ())))


;;;
;;;; Autoload Reference
;;;


(jazz.define-class jazz.Autoload-Reference jazz.Export-Reference (name declaration library-reference) jazz.Object-Class jazz.allocate-autoload-reference
  ())


;;;
;;;; Module
;;;


(jazz.define-class jazz.Module-Declaration jazz.Declaration (name type hits access compatibility attributes toplevel parent locator source) jazz.Object-Class jazz.allocate-module-declaration
  ((requires %%get-module-declaration-requires ())))


;;;
;;;; Namespace
;;;


(jazz.define-class jazz.Namespace-Declaration jazz.Declaration (name type hits access compatibility attributes toplevel parent locator source) jazz.Object-Class ()
  ((lookups         %%get-namespace-declaration-lookups         ())
   (children-lookup %%get-namespace-declaration-children-lookup ())
   (body            %%get-namespace-declaration-body            %%set-namespace-declaration-body)))


(jazz.define-macro (%%get-access-lookup namespace-declaration access)
  `(%%vector-ref (%%get-namespace-declaration-lookups ,namespace-declaration) ,access))


;;;
;;;; Library
;;;


(jazz.define-class jazz.Library-Declaration jazz.Namespace-Declaration (name type hits access compatibility attributes toplevel parent locator source lookups children-lookup body) jazz.Object-Class jazz.allocate-library-declaration
  ((dialect-name    %%get-library-declaration-dialect-name    ())
   (dialect-invoice %%get-library-declaration-dialect-invoice ())
   (requires        %%get-library-declaration-requires        %%set-library-declaration-requires)
   (exports         %%get-library-declaration-exports         %%set-library-declaration-exports)
   (imports         %%get-library-declaration-imports         %%set-library-declaration-imports)
   (proclaims       %%get-library-declaration-proclaims       %%set-library-declaration-proclaims)
   (literals        %%get-library-declaration-literals        %%set-library-declaration-literals)
   (variables       %%get-library-declaration-variables       %%set-library-declaration-variables)
   (references      %%get-library-declaration-references      %%set-library-declaration-references)
   (inclusions      %%get-library-declaration-inclusions      %%set-library-declaration-inclusions)
   (autoloads       %%get-library-declaration-autoloads       %%set-library-declaration-autoloads)))


;;;
;;;; Library Invoice
;;;


(jazz.define-class jazz.Library-Invoice jazz.Object () jazz.Object-Class ()
  ((name       %%get-library-invoice-name    ())
   (library    %%get-library-invoice-library ())
   (phase      %%get-library-invoice-phase   ())
   (version    %%get-library-invoice-version ())
   (only       %%get-library-invoice-only    ())
   (except     %%get-library-invoice-except  ())
   (prefix     %%get-library-invoice-prefix  ())
   (rename     %%get-library-invoice-rename  ())))


;;;
;;;; Export Invoice
;;;


(jazz.define-class jazz.Export-Invoice jazz.Library-Invoice (name library phase version only except prefix rename) jazz.Object-Class jazz.allocate-export-invoice
  ((autoload %%get-export-invoice-autoload %%set-export-invoice-autoload)))


;;;
;;;; Import Invoice
;;;


(jazz.define-class jazz.Import-Invoice jazz.Library-Invoice (name library phase version only except prefix rename) jazz.Object-Class jazz.allocate-import-invoice
  ((hit? %%get-import-invoice-hit? %%set-import-invoice-hit?)))


;;;
;;;; Export
;;;


(jazz.define-class jazz.Export-Declaration jazz.Declaration (name type hits access compatibility attributes toplevel parent locator source) jazz.Object-Class jazz.allocate-export-declaration
  ((symbol %%get-export-declaration-symbol ())))


;;;
;;;; Autoload
;;;


(jazz.define-class jazz.Autoload-Declaration jazz.Declaration (name type hits access compatibility attributes toplevel parent locator source) jazz.Object-Class jazz.allocate-autoload-declaration
  ((library          %%get-autoload-declaration-library          ())
   (exported-library %%get-autoload-declaration-exported-library ())
   (declaration      %%get-autoload-declaration-declaration      %%set-autoload-declaration-declaration)))


;;;
;;;; Macro
;;;


(jazz.define-class jazz.Macro-Declaration jazz.Declaration (name type hits access compatibility attributes toplevel parent locator source) jazz.Object-Class jazz.allocate-macro-declaration
  ((signature %%get-macro-declaration-signature %%set-macro-declaration-signature)
   (body      %%get-macro-declaration-body      %%set-macro-declaration-body)))


;;;
;;;; Syntax
;;;


(jazz.define-class jazz.Syntax-Declaration jazz.Declaration (name type hits access compatibility attributes toplevel parent locator source) jazz.Object-Class jazz.allocate-syntax-declaration
  ((signature %%get-syntax-declaration-signature %%set-syntax-declaration-signature)
   (body      %%get-syntax-declaration-body      %%set-syntax-declaration-body)))


;;;
;;;; Void
;;;


(jazz.define-class jazz.Void-Class jazz.Class (name fields virtual-size ancestors descendants ascendant interfaces slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class ()
  ())


(jazz.define-class jazz.Void jazz.Type () jazz.Void-Class ()
  ())


;;;
;;;; Opt
;;;


(jazz.define-class jazz.Opt-Type jazz.Type () jazz.Class jazz.allocate-opt-type
  ((type %%get-opt-type-type ())))


;;;
;;;; Key
;;;


(jazz.define-class jazz.Key-Type jazz.Type () jazz.Class jazz.allocate-key-type
  ((key  %%get-key-type-key  ())
   (type %%get-key-type-type ())))


;;;
;;;; Rest
;;;


(jazz.define-class jazz.Rest-Type jazz.Type () jazz.Class jazz.allocate-rest-type
  ((type %%get-rest-type-type ())))


;;;
;;;; Function
;;;


(jazz.define-class jazz.Function-Type jazz.Type () jazz.Class jazz.allocate-function-type
  ((mandatory  %%get-function-type-mandatory  ())
   (positional %%get-function-type-positional ())
   (optional   %%get-function-type-optional   ())
   (named      %%get-function-type-named      ())
   (rest       %%get-function-type-rest       ())
   (result     %%get-function-type-result     ())))


;;;
;;;; Category
;;;


(jazz.define-class jazz.Category-Type jazz.Type () jazz.Class jazz.allocate-category-type
  ((declaration %%get-category-type-declaration ())))


;;;
;;;; Values
;;;


(jazz.define-class jazz.Values-Type jazz.Type () jazz.Class jazz.allocate-values-type
  ((types %%get-values-type-types ())))


;;;
;;;; Restriction
;;;


(jazz.define-class jazz.Restriction-Type jazz.Type () jazz.Class jazz.allocate-restriction-type
  ((base %%get-restriction-type-base ())
   (type %%get-restriction-type-type ())))


;;;
;;;; Complement
;;;


(jazz.define-class jazz.Complement-Type jazz.Type () jazz.Class jazz.allocate-complement-type
  ((type %%get-complement-type-type ())))


;;;
;;;; Union
;;;


(jazz.define-class jazz.Union-Type jazz.Type () jazz.Class jazz.allocate-union-type
  ((types %%get-union-type-types ())))


;;;
;;;; Template
;;;


(jazz.define-class jazz.Template-Type jazz.Type () jazz.Class jazz.allocate-template-type
  ((class %%get-template-type-class ())
   (types %%get-template-type-types ())))


;;;
;;;; Nillable
;;;


(jazz.define-class jazz.Nillable-Type jazz.Type () jazz.Class jazz.allocate-nillable-type
  ((type %%get-nillable-type-type ())))


;;;
;;;; Any
;;;


(jazz.define-class jazz.Any-Class jazz.Class (name fields virtual-size ancestors descendants ascendant interfaces slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class ()
  ())


(jazz.define-class jazz.Any jazz.Type () jazz.Any-Class ()
  ())


;;;
;;;; C Type
;;;


(jazz.define-class jazz.C-Type-Declaration jazz.Declaration (name type hits access compatibility attributes toplevel parent locator source) jazz.Object-Class jazz.allocate-c-type-declaration
  ((kind          %%get-c-type-declaration-kind          ())
   (expansion     %%get-c-type-declaration-expansion     ())
   (base-type     %%get-c-type-declaration-base-type     ())
   (pointer-types %%get-c-type-declaration-pointer-types %%set-c-type-declaration-pointer-types)
   (inclusions    %%get-c-type-declaration-inclusions    ())
   (c-to-scheme   %%get-c-type-declaration-c-to-scheme   ())
   (scheme-to-c   %%get-c-type-declaration-scheme-to-c   ())
   (declare       %%get-c-type-declaration-declare       ())))


;;;
;;;; C Definition
;;;


(jazz.define-class jazz.C-Definition-Declaration jazz.Declaration (name type hits access compatibility attributes toplevel parent locator source) jazz.Object-Class jazz.allocate-c-definition-declaration
  ((signature       %%get-c-definition-declaration-signature       %%set-c-definition-declaration-signature)
   (parameter-types %%get-c-definition-declaration-parameter-types ())
   (result-type     %%get-c-definition-declaration-result-type     ())
   (c-name          %%get-c-definition-declaration-c-name          ())
   (scope           %%get-c-definition-declaration-scope           ())
   (body            %%get-c-definition-declaration-body            %%set-c-definition-declaration-body)))


;;;
;;;; Walker
;;;


(jazz.define-class jazz.Walker jazz.Object () jazz.Object-Class ()
  ((warnings %%get-walker-warnings %%set-walker-warnings)
   (errors   %%get-walker-errors   %%set-walker-errors)))


(jazz.define-virtual (jazz.walker-environment (jazz.Walker walker)))
(jazz.define-virtual (jazz.walk-declaration (jazz.Walker walker) resume declaration environment form))
(jazz.define-virtual (jazz.walk-free-reference (jazz.Walker walker) resume declaration symbol))
(jazz.define-virtual (jazz.walk-symbol-assignment (jazz.Walker walker) resume declaration environment symbol value))
(jazz.define-virtual (jazz.walk-free-assignment (jazz.Walker walker) resume declaration symbol))
(jazz.define-virtual (jazz.walk-symbol (jazz.Walker walker) resume declaration environment symbol))
(jazz.define-virtual (jazz.walk-form (jazz.Walker walker) resume declaration environment form))
(jazz.define-virtual (jazz.validate-proclaim (jazz.Walker walker) resume declaration environment form-src))
(jazz.define-virtual (jazz.validate-arguments (jazz.Walker walker) resume source-declaration declaration signature arguments))


;;;
;;;; Walk Context
;;;


(jazz.define-class jazz.Walk-Context jazz.Object () jazz.Object-Class jazz.allocate-walk-context
  ((policy   %%get-walk-context-policy   ())
   (locator  %%get-walk-context-locator  ())
   (pathname %%get-walk-context-pathname ())))


;;;
;;;; Walk Location
;;;


(jazz.define-class jazz.Walk-Location jazz.Object () jazz.Object-Class jazz.allocate-walk-location
  ((module-locator      %%get-walk-location-module-locator      ())
   (declaration-locator %%get-walk-location-declaration-locator ())
   (locat               %%get-walk-location-locat               ())))


;;;
;;;; Walk Problem
;;;


(jazz.define-class jazz.Walk-Problem jazz.Error (message) jazz.Object-Class ()
  ((location %%get-walk-problem-location ())))


;;;
;;;; Walk Problems
;;;


(jazz.define-class jazz.Walk-Problems jazz.Error (message) jazz.Object-Class jazz.allocate-walk-problems
  ((warnings %%get-walk-problems-warnings ())
   (errors   %%get-walk-problems-errors   ())))


;;;
;;;; Walk Warning
;;;


(jazz.define-class jazz.Walk-Warning jazz.Walk-Problem (message location) jazz.Object-Class jazz.allocate-walk-warning
  ())


;;;
;;;; Walk Error
;;;


(jazz.define-class jazz.Walk-Error jazz.Walk-Problem (message location) jazz.Object-Class jazz.allocate-walk-error
  ())


;;;
;;;; Unresolved Error
;;;


(jazz.define-class jazz.Unresolved-Error jazz.Walk-Error (message location) jazz.Object-Class jazz.allocate-unresolved-error
  ((symbol %%get-unresolved-error-symbol ())))


;;;
;;;; Walk Frame
;;;


(jazz.define-class jazz.Walk-Frame jazz.Walk-Binding () jazz.Object-Class jazz.allocate-walk-frame
  ((bindings %%get-walk-frame-bindings ())))


;;;
;;;; Signature
;;;


(jazz.define-class jazz.Signature jazz.Object () jazz.Object-Class jazz.allocate-signature
  ((mandatory  %%get-signature-mandatory  ())
   (positional %%get-signature-positional ())
   (optional   %%get-signature-optional   ())
   (named      %%get-signature-named      ())
   (rest       %%get-signature-rest       ())))


;;;
;;;; Symbol Binding
;;;


(jazz.define-class jazz.Symbol-Binding jazz.Lexical-Binding (name type hits) jazz.Object-Class ()
  ())


;;;
;;;; Variable
;;;


(jazz.define-class jazz.Variable jazz.Symbol-Binding (name type hits) jazz.Object-Class jazz.allocate-variable
  ((reference-count %%get-variable-reference-count %%set-variable-reference-count)))


;;;
;;;; NextMethod Variable
;;;


(jazz.define-class jazz.NextMethod-Variable jazz.Variable (name type hits reference-count) jazz.Object-Class jazz.allocate-nextmethod-variable
  ())


;;;
;;;; Parameter
;;;


(jazz.define-class jazz.Parameter jazz.Variable (name type hits reference-count) jazz.Object-Class jazz.allocate-parameter
  ())


(jazz.define-virtual (jazz.emit-parameter (jazz.Parameter parameter) declaration environment))


;;;
;;;; Dynamic Parameter
;;;


(jazz.define-class jazz.Dynamic-Parameter jazz.Parameter (name type hits reference-count) jazz.Object-Class jazz.allocate-dynamic-parameter
  ((class %%get-dynamic-parameter-class ())))


;;;
;;;; Optional Parameter
;;;


(jazz.define-class jazz.Optional-Parameter jazz.Parameter (name type hits reference-count) jazz.Object-Class jazz.allocate-optional-parameter
  ((default %%get-optional-parameter-default %%set-optional-parameter-default)))


;;;
;;;; Named Parameter
;;;


(jazz.define-class jazz.Named-Parameter jazz.Parameter (name type hits reference-count) jazz.Object-Class jazz.allocate-named-parameter
  ((default %%get-named-parameter-default %%set-named-parameter-default)))


;;;
;;;; Rest Parameter
;;;


(jazz.define-class jazz.Rest-Parameter jazz.Parameter (name type hits reference-count) jazz.Object-Class jazz.allocate-rest-parameter
  ())


;;;
;;;; Self-Binding
;;;


(jazz.define-class jazz.Self-Binding jazz.Lexical-Binding (name type hits) jazz.Object-Class jazz.allocate-self-binding
  ())


;;;
;;;; Dynamic-Self-Binding
;;;


(jazz.define-class jazz.Dynamic-Self-Binding jazz.Lexical-Binding (name type hits) jazz.Object-Class jazz.allocate-dynamic-self-binding
  ((code %%get-dynamic-self-binding-code ())))


;;;
;;;; Macro Symbol
;;;


(jazz.define-class jazz.Macro-Symbol jazz.Symbol-Binding (name type hits) jazz.Object-Class jazz.allocate-macro-symbol
  ((getter %%get-macro-symbol-getter ())
   (setter %%get-macro-symbol-setter ())))


;;;
;;;; Form Binding
;;;


(jazz.define-class jazz.Form-Binding jazz.Lexical-Binding (name type hits) jazz.Object-Class ()
  ())


;;;
;;;; Special Form
;;;


(jazz.define-class jazz.Special-Form jazz.Form-Binding (name type hits) jazz.Object-Class jazz.allocate-special-form
  ((walk %%get-special-form-walk ())))


;;;
;;;; Macro Form
;;;


(jazz.define-class jazz.Macro-Form jazz.Form-Binding (name type hits) jazz.Object-Class jazz.allocate-macro-form
  ((expander %%get-macro-form-expander ())))


;;;
;;;; Syntax Form
;;;


(jazz.define-class jazz.Syntax-Form jazz.Form-Binding (name type hits) jazz.Object-Class jazz.allocate-syntax-form
  ((expander %%get-syntax-form-expander ())))


;;;
;;;; Annotated Variable
;;;


(jazz.define-class jazz.Annotated-Variable jazz.Object () jazz.Object-Class jazz.allocate-annotated-variable
  ((variable      %%get-annotated-variable-variable      ())
   (declared-type %%get-annotated-variable-declared-type ())
   (type          %%get-annotated-variable-type          %%set-annotated-variable-type)))


;;;
;;;; Restricted Binding
;;;


(jazz.define-class jazz.Restricted-Binding jazz.Object () jazz.Object-Class jazz.allocate-restricted-binding
  ((binding %%get-restricted-binding-binding ())
   (type    %%get-restricted-binding-type    ())))


;;;
;;;; Annotated Frame
;;;


(jazz.define-class jazz.Annotated-Frame jazz.Object () jazz.Object-Class jazz.allocate-annotated-frame
  ((variables %%get-annotated-frame-variables ())
   (reset     %%get-annotated-frame-reset     ())))


;;;
;;;; Code
;;;


(jazz.define-class jazz.Code jazz.Object () jazz.Object-Class jazz.allocate-code
  ((form   %%get-code-form   ())
   (type   %%get-code-type   ())
   (source %%get-code-source ())))


;;;
;;;; Slot Access
;;;


;; this should be moved to jazz
(jazz.define-class jazz.Access jazz.Object () jazz.Object-Class jazz.allocate-access
  ((name    %%get-access-name    ())
   (context %%get-access-context ())))


;;;
;;;; Expression
;;;


(jazz.define-class jazz.Expression jazz.Object () jazz.Object-Class ()
  ((type   %%get-expression-type   ())
   (source %%get-expression-source ())))


(jazz.define-virtual (jazz.emit-expression (jazz.Expression expression) declaration environment))
(jazz.define-virtual (jazz.emit-call (jazz.Expression expression) arguments declaration environment))
(jazz.define-virtual (jazz.fold-expression (jazz.Expression expression) f k s))


;;;
;;;; Proclaim
;;;


(jazz.define-class jazz.Proclaim jazz.Expression (type source) jazz.Object-Class jazz.allocate-proclaim
  ((clauses %%get-proclaim-clauses ())))


;;;
;;;; Constant
;;;


(jazz.define-class jazz.Constant jazz.Expression (type source) jazz.Object-Class jazz.allocate-constant
  ((expansion %%get-constant-expansion ())))


;;;
;;;; Delay
;;;


(jazz.define-class jazz.Delay jazz.Expression (type source) jazz.Object-Class jazz.allocate-delay
  ((expression %%get-delay-expression ())))


;;;
;;;; Quasiquote
;;;


(jazz.define-class jazz.Quasiquote jazz.Expression (type source) jazz.Object-Class jazz.allocate-quasiquote
  ((form %%get-quasiquote-form ())))


;;;
;;;; Reference
;;;


(jazz.define-class jazz.Reference jazz.Expression (type source) jazz.Object-Class jazz.allocate-reference
  ((binding %%get-reference-binding ())))


;;;
;;;; Assignment
;;;


(jazz.define-class jazz.Assignment jazz.Expression (type source) jazz.Object-Class jazz.allocate-assignment
  ((binding %%get-assignment-binding ())
   (value   %%get-assignment-value   ())))


;;;
;;;; Lambda
;;;


(jazz.define-class jazz.Lambda jazz.Expression (type source) jazz.Object-Class jazz.allocate-lambda
  ((signature %%get-lambda-signature ())
   (body      %%get-lambda-body      ())))


;;;
;;;; Let
;;;


(jazz.define-class jazz.Let jazz.Expression (type source) jazz.Object-Class jazz.allocate-let
  ((bindings %%get-let-bindings ())
   (body     %%get-let-body     ())))


;;;
;;;; Named Let
;;;


(jazz.define-class jazz.Named-Let jazz.Let (type source bindings body) jazz.Object-Class jazz.allocate-named-let
  ((variable %%get-named-let-variable ())))


;;;
;;;; Letstar
;;;


(jazz.define-class jazz.Letstar jazz.Expression (type source) jazz.Object-Class jazz.allocate-letstar
  ((bindings %%get-letstar-bindings ())
   (body     %%get-letstar-body     ())))


;;;
;;;; Letrec
;;;


(jazz.define-class jazz.Letrec jazz.Expression (type source) jazz.Object-Class jazz.allocate-letrec
  ((bindings %%get-letrec-bindings ())
   (body     %%get-letrec-body     ())))


;;;
;;;; Receive
;;;


(jazz.define-class jazz.Receive jazz.Expression (type source) jazz.Object-Class jazz.allocate-receive
  ((variables  %%get-receive-variables  ())
   (expression %%get-receive-expression ())
   (body       %%get-receive-body       ())))


;;;
;;;; Body
;;;


(jazz.define-class jazz.Body jazz.Expression (type source) jazz.Object-Class jazz.allocate-body
  ((internal-defines %%get-body-internal-defines ())
   (expressions      %%get-body-expressions      ())))


;;;
;;;; Internal Define
;;;


(jazz.define-class jazz.Internal-Define jazz.Expression (type source) jazz.Object-Class jazz.allocate-internal-define
  ((variable %%get-internal-define-variable ())
   (value    %%get-internal-define-value    ())))


;;;
;;;; Begin
;;;


(jazz.define-class jazz.Begin jazz.Expression (type source) jazz.Object-Class jazz.allocate-begin
  ((expressions %%get-begin-expressions ())))


;;;
;;;; Do
;;;


(jazz.define-class jazz.Do jazz.Expression (type source) jazz.Object-Class jazz.allocate-do
  ((bindings %%get-do-bindings ())
   (test     %%get-do-test     ())
   (result   %%get-do-result   ())
   (body     %%get-do-body     ())))


;;;
;;;; Call
;;;


(jazz.define-class jazz.Call jazz.Expression (type source) jazz.Object-Class jazz.allocate-call
  ((operator  %%get-call-operator  ())
   (arguments %%get-call-arguments ())))


;;;
;;;; If
;;;


(jazz.define-class jazz.If jazz.Expression (type source) jazz.Object-Class jazz.allocate-if
  ((test %%get-if-test ())
   (yes  %%get-if-yes ())
   (no   %%get-if-no ())))


;;;
;;;; Cond
;;;


(jazz.define-class jazz.Cond jazz.Expression (type source) jazz.Object-Class jazz.allocate-cond
  ((clauses %%get-cond-clauses ())))


;;;
;;;; Case
;;;


(jazz.define-class jazz.Case jazz.Expression (type source) jazz.Object-Class jazz.allocate-case
  ((target  %%get-case-target  ())
   (clauses %%get-case-clauses ())))


;;;
;;;; And
;;;


(jazz.define-class jazz.And jazz.Expression (type source) jazz.Object-Class jazz.allocate-and
  ((expressions %%get-and-expressions ())))


;;;
;;;; Or
;;;


(jazz.define-class jazz.Or jazz.Expression (type source) jazz.Object-Class jazz.allocate-or
  ((expressions %%get-or-expressions ())))


;;;
;;;; Declare
;;;


(jazz.define-class jazz.Declare jazz.Expression (type source) jazz.Object-Class jazz.allocate-declare
  ((declarations %%get-declare-declarations ())))


;;;
;;;; C Include
;;;


(jazz.define-class jazz.C-Include jazz.Expression (type source) jazz.Object-Class jazz.allocate-c-include
  ((name %%get-c-include-name ())))


;;;
;;;; C Declare
;;;


(jazz.define-class jazz.C-Declare jazz.Expression (type source) jazz.Object-Class jazz.allocate-c-declare
  ((code %%get-c-declare-code ())))


;;;
;;;; C Named Declare
;;;


(jazz.define-class jazz.C-Named-Declare-Declaration jazz.Declaration (name type hits access compatibility attributes toplevel parent locator source) jazz.Object-Class jazz.allocate-c-named-declare-declaration
  ((code %%get-c-named-declare-declaration-code ())))


;;;
;;;; C Initialize
;;;


(jazz.define-class jazz.C-Initialize jazz.Expression (type source) jazz.Object-Class jazz.allocate-c-initialize
  ((code %%get-c-initialize-code ())))


;;;
;;;; C Function
;;;


(jazz.define-class jazz.C-Function jazz.Expression (type source) jazz.Object-Class jazz.allocate-c-function
  ((expansion %%get-c-function-expansion ())))


;;;
;;;; Parameterize
;;;


(jazz.define-class jazz.Parameterize jazz.Expression (type source) jazz.Object-Class jazz.allocate-parameterize
  ((bindings %%get-parameterize-bindings ())
   (body     %%get-parameterize-body     ())))


;;;
;;;; Time Special
;;;


(jazz.define-class jazz.Time-Special jazz.Expression (type source) jazz.Object-Class jazz.allocate-time
  ((expression %%get-time-special-expression ())))


;;;
;;;; Core Dialect
;;;


(jazz.define-class jazz.Core-Dialect jazz.Dialect () jazz.Object-Class jazz.allocate-core-dialect
  ())


;;;
;;;; Core Walker
;;;


(jazz.define-class jazz.Core-Walker jazz.Walker (warnings errors) jazz.Object-Class jazz.allocate-core-walker
  ()))
