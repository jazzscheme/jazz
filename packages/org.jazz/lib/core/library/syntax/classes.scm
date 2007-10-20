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


(module core.library.syntax.classes


;;;
;;;; Walk Binding
;;;


(jazz.define-class-syntax jazz.Walk-Binding jazz.Type () jazz.Object-Class ()
  ())


(jazz.define-virtual-syntax (jazz.walk-binding-lookup (jazz.Walk-Binding binding) symbol))
(jazz.define-virtual-syntax (jazz.emit-binding-reference (jazz.Walk-Binding binding) environment))
(jazz.define-virtual-syntax (jazz.walk-binding-validate-call (jazz.Walk-Binding binding) walker resume source-declaration operator arguments))
(jazz.define-virtual-syntax (jazz.emit-binding-call (jazz.Walk-Binding binding) arguments environment))
(jazz.define-virtual-syntax (jazz.emit-inlined-binding-call (jazz.Walk-Binding binding) arguments environment))
(jazz.define-virtual-syntax (jazz.walk-binding-assignable? (jazz.Walk-Binding binding)))
(jazz.define-virtual-syntax (jazz.walk-binding-assigned (jazz.Walk-Binding binding) assignment))
(jazz.define-virtual-syntax (jazz.emit-binding-assignment (jazz.Walk-Binding binding) value source-declaration environment))
(jazz.define-virtual-syntax (jazz.walk-binding-walkable? (jazz.Walk-Binding binding)))
(jazz.define-virtual-syntax (jazz.walk-binding-walk-form (jazz.Walk-Binding binding) walker resume declaration environment form))
(jazz.define-virtual-syntax (jazz.walk-binding-expandable? (jazz.Walk-Binding binding)))
(jazz.define-virtual-syntax (jazz.walk-binding-expand-form (jazz.Walk-Binding binding) walker resume declaration environment form))


;;;
;;;; Lexical Binding
;;;


(jazz.define-class-syntax jazz.Lexical-Binding jazz.Walk-Binding () jazz.Object-Class ()
  ((name %%get-lexical-binding-name ())
   (type %%get-lexical-binding-type ())))


;;;
;;;; Declaration
;;;


(jazz.define-class-syntax jazz.Declaration jazz.Lexical-Binding (name type) jazz.Object-Class ()
  ((access        %%get-declaration-access        ())
   (compatibility %%get-declaration-compatibility ())
   (attributes    %%get-declaration-attributes    ())
   (toplevel      %%get-declaration-toplevel      %%set-declaration-toplevel)
   (parent        %%get-declaration-parent        %%set-declaration-parent)
   (children      %%get-declaration-children      %%set-declaration-children)
   (locator       %%get-declaration-locator       %%set-declaration-locator)))


(jazz.define-virtual-syntax (jazz.resolve-declaration (jazz.Declaration declaration)))
(jazz.define-virtual-syntax (jazz.lookup-declaration (jazz.Declaration declaration) symbol external?))
(jazz.define-virtual-syntax (jazz.get-declaration-references (jazz.Declaration declaration)))
(jazz.define-virtual-syntax (jazz.emit-declaration (jazz.Declaration declaration) environment))
(jazz.define-virtual-syntax (jazz.expand-referenced-declaration (jazz.Declaration declaration)))
(jazz.define-virtual-syntax (jazz.fold-declaration (jazz.Declaration declaration) f k s))


;;;
;;;; Declaration Reference
;;;


(jazz.define-class-syntax jazz.Declaration-Reference jazz.Object () jazz.Object-Class ()
  ((name        %%get-declaration-reference-name        ())
   (declaration %%get-declaration-reference-declaration %%set-declaration-reference-declaration)))


(jazz.define-virtual-syntax (jazz.resolve-reference (jazz.Declaration-Reference declaration-reference) library-declaration))


;;;
;;;; Library Reference
;;;


(jazz.define-class-syntax jazz.Library-Reference jazz.Declaration-Reference (name declaration) jazz.Object-Class jazz.allocate-library-reference
  ())


;;;
;;;; Export Reference
;;;


(jazz.define-class-syntax jazz.Export-Reference jazz.Declaration-Reference (name declaration) jazz.Object-Class jazz.allocate-export-reference
  ((library-reference %%get-export-reference-library-reference ())))


;;;
;;;; Autoload Reference
;;;


(jazz.define-class-syntax jazz.Autoload-Reference jazz.Export-Reference (name declaration library-reference) jazz.Object-Class jazz.allocate-autoload-reference
  ())


;;;
;;;; Module
;;;


(jazz.define-class-syntax jazz.Module-Declaration jazz.Declaration (name type access compatibility attributes toplevel parent children locator) jazz.Object-Class jazz.allocate-module-declaration
  ((requires %%get-module-declaration-requires ())))


;;;
;;;; Namespace
;;;


(jazz.define-class-syntax jazz.Namespace-Declaration jazz.Declaration (name type access compatibility attributes toplevel parent children locator) jazz.Object-Class ()
  ((lookups %%get-namespace-declaration-lookups ())
   (body    %%get-namespace-declaration-body    %%set-namespace-declaration-body)))


(jazz.define-macro (%%get-access-lookup namespace-declaration access)
  `(%%vector-ref (%%get-namespace-declaration-lookups ,namespace-declaration) ,access))


;;;
;;;; Library
;;;


(jazz.define-class-syntax jazz.Library-Declaration jazz.Namespace-Declaration (name type access compatibility attributes toplevel parent children locator lookups body) jazz.Object-Class jazz.allocate-library-declaration
  ((dialect    %%get-library-declaration-dialect    ())
   (requires   %%get-library-declaration-requires   ())
   (exports    %%get-library-declaration-exports    ())
   (imports    %%get-library-declaration-imports    ())
   (literals   %%get-library-declaration-literals   %%set-library-declaration-literals)
   (variables  %%get-library-declaration-variables  %%set-library-declaration-variables)
   (references %%get-library-declaration-references %%set-library-declaration-references)
   (autoloads  %%get-library-declaration-autoloads  %%set-library-declaration-autoloads)))


;;;
;;;; Library Invoice
;;;


(jazz.define-class-syntax jazz.Library-Invoice jazz.Object () jazz.Object-Class ()
  ((library    %%get-library-invoice-library ())
   (phase      %%get-library-invoice-phase   ())
   (version    %%get-library-invoice-version ())
   (only       %%get-library-invoice-only    ())
   (except     %%get-library-invoice-except  ())
   (prefix     %%get-library-invoice-prefix  ())
   (rename     %%get-library-invoice-rename  ())))


;;;
;;;; Export Invoice
;;;


(jazz.define-class-syntax jazz.Export-Invoice jazz.Library-Invoice (library phase version only except prefix rename) jazz.Object-Class jazz.allocate-export-invoice
  ((autoload %%get-export-invoice-autoload ())))


;;;
;;;; Import Invoice
;;;


(jazz.define-class-syntax jazz.Import-Invoice jazz.Library-Invoice (library phase version only except prefix rename) jazz.Object-Class jazz.allocate-import-invoice
  ())


;;;
;;;; Export
;;;


(jazz.define-class-syntax jazz.Export-Declaration jazz.Declaration (name type access compatibility attributes toplevel parent children locator) jazz.Object-Class jazz.allocate-export-declaration
  ((symbol %%get-export-declaration-symbol ())))


;;;
;;;; Autoload
;;;


(jazz.define-class-syntax jazz.Autoload-Declaration jazz.Declaration (name type access compatibility attributes toplevel parent children locator) jazz.Object-Class jazz.allocate-autoload-declaration
  ((library          %%get-autoload-declaration-library          ())
   (exported-library %%get-autoload-declaration-exported-library ())
   (declaration      %%get-autoload-declaration-declaration      %%set-autoload-declaration-declaration)))


;;;
;;;; Macro
;;;


(jazz.define-class-syntax jazz.Macro-Declaration jazz.Declaration (name type access compatibility attributes toplevel parent children locator) jazz.Object-Class jazz.allocate-macro-declaration
  ((signature %%get-macro-declaration-signature %%set-macro-declaration-signature)
   (body      %%get-macro-declaration-body      %%set-macro-declaration-body)))


;;;
;;;; Syntax
;;;


(jazz.define-class-syntax jazz.Syntax-Declaration jazz.Declaration (name type access compatibility attributes toplevel parent children locator) jazz.Object-Class jazz.allocate-syntax-declaration
  ((signature %%get-syntax-declaration-signature %%set-syntax-declaration-signature)
   (body      %%get-syntax-declaration-body      %%set-syntax-declaration-body)))


;;;
;;;; Void
;;;


(jazz.define-class-syntax jazz.Void-Class jazz.Class (name fields virtual-size ancestors descendants ascendant interfaces slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class ()
  ())


(jazz.define-class-syntax jazz.Void jazz.Type () jazz.Void-Class ()
  ())


;;;
;;;; Function
;;;


(jazz.define-class-syntax jazz.Function-Type jazz.Type () jazz.Class jazz.allocate-function-type
  ((parameters %%get-function-type-parameters ())
   (result     %%get-function-type-result     ())))


;;;
;;;; Template
;;;


(jazz.define-class-syntax jazz.Template-Type jazz.Type () jazz.Class jazz.allocate-template-type
  ((class %%get-template-type-class ())
   (types %%get-template-type-types ())))


;;;
;;;; Nillable
;;;


(jazz.define-class-syntax jazz.Nillable-Type jazz.Type () jazz.Class jazz.allocate-nillable-type
  ((type %%get-nillable-type-type ())))


;;;
;;;; Any
;;;


(jazz.define-class-syntax jazz.Any-Class jazz.Class (name fields virtual-size ancestors descendants ascendant interfaces slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class ()
  ())


(jazz.define-class-syntax jazz.Any jazz.Type () jazz.Class ()
  ())


;;;
;;;; C Type
;;;


(jazz.define-class-syntax jazz.C-Type-Declaration jazz.Declaration (name type access compatibility attributes toplevel parent children locator) jazz.Object-Class jazz.allocate-c-type-declaration
  ((kind       %%get-c-type-declaration-kind       ())
   (expansion  %%get-c-type-declaration-expansion  ())
   (references %%get-c-type-declaration-references ())))


;;;
;;;; C Definition
;;;


(jazz.define-class-syntax jazz.C-Definition-Declaration jazz.Declaration (name type access compatibility attributes toplevel parent children locator) jazz.Object-Class jazz.allocate-c-definition-declaration
  ((signature       %%get-c-definition-declaration-signature       %%set-c-definition-declaration-signature)
   (parameter-types %%get-c-definition-declaration-parameter-types ())
   (result-type     %%get-c-definition-declaration-result-type     ())
   (c-name          %%get-c-definition-declaration-c-name          ())
   (scope           %%get-c-definition-declaration-scope           ())
   (body            %%get-c-definition-declaration-body            %%set-c-definition-declaration-body)))


;;;
;;;; Walker
;;;


(jazz.define-class-syntax jazz.Walker jazz.Object () jazz.Object-Class ()
  ((warnings %%get-walker-warnings %%set-walker-warnings)
   (errors   %%get-walker-errors   %%set-walker-errors)))


(jazz.define-virtual-syntax (jazz.walker-environment (jazz.Walker walker)))
(jazz.define-virtual-syntax (jazz.walk-declaration (jazz.Walker walker) resume declaration environment form))
(jazz.define-virtual-syntax (jazz.validate-access (jazz.Walker walker) resume declaration referenced-declaration))
(jazz.define-virtual-syntax (jazz.walk-free-reference (jazz.Walker walker) resume declaration symbol))
(jazz.define-virtual-syntax (jazz.walk-symbol-assignment (jazz.Walker walker) resume declaration environment symbol value))
(jazz.define-virtual-syntax (jazz.walk-free-assignment (jazz.Walker walker) resume declaration symbol))
(jazz.define-virtual-syntax (jazz.walk-symbol (jazz.Walker walker) resume declaration environment symbol))
(jazz.define-virtual-syntax (jazz.walk-form (jazz.Walker walker) resume declaration environment form))
(jazz.define-virtual-syntax (jazz.validate-arguments (jazz.Walker walker) resume source-declaration declaration signature arguments))


;;;
;;;; Walk Context
;;;


(jazz.define-class-syntax jazz.Walk-Context jazz.Object () jazz.Object-Class jazz.allocate-walk-context
  ((policy   %%get-walk-context-policy   ())
   (locator  %%get-walk-context-locator  ())
   (pathname %%get-walk-context-pathname ())))


;;;
;;;; Walk Location
;;;


(jazz.define-class-syntax jazz.Walk-Location jazz.Object () jazz.Object-Class jazz.allocate-walk-location
  ((module-locator      %%get-walk-location-module-locator      ())
   (declaration-locator %%get-walk-location-declaration-locator ())))


;;;
;;;; Walk Error
;;;


(jazz.define-class-syntax jazz.Walk-Error jazz.Error (message) jazz.Object-Class jazz.allocate-walk-error
  ((location %%get-walk-error-location ())))


;;;
;;;; Unresolved Error
;;;


(jazz.define-class-syntax jazz.Unresolved-Error jazz.Walk-Error (message location) jazz.Object-Class jazz.allocate-unresolved-error
  ((symbol %%get-unresolved-error-symbol ())))


;;;
;;;; Walk Frame
;;;


(jazz.define-class-syntax jazz.Walk-Frame jazz.Walk-Binding () jazz.Object-Class jazz.allocate-walk-frame
  ((bindings %%get-walk-frame-bindings ())))


;;;
;;;; Signature
;;;


(jazz.define-class-syntax jazz.Signature jazz.Object () jazz.Object-Class jazz.allocate-signature
  ((mandatory  %%get-signature-mandatory  ())
   (positional %%get-signature-positional ())
   (optional   %%get-signature-optional   ())
   (named      %%get-signature-named      ())
   (rest       %%get-signature-rest       ())))


;;;
;;;; Symbol Binding
;;;


(jazz.define-class-syntax jazz.Symbol-Binding jazz.Lexical-Binding (name type) jazz.Object-Class ()
  ())


;;;
;;;; Variable
;;;


(jazz.define-class-syntax jazz.Variable jazz.Symbol-Binding (name type) jazz.Object-Class jazz.allocate-variable
  ((setters %%get-variable-setters %%set-variable-setters)))


;;;
;;;; NextMethod Variable
;;;


(jazz.define-class-syntax jazz.NextMethod-Variable jazz.Variable (name type setters) jazz.Object-Class jazz.allocate-nextmethod-variable
  ())


;;;
;;;; Parameter
;;;


(jazz.define-class-syntax jazz.Parameter jazz.Variable (name type setters) jazz.Object-Class jazz.allocate-parameter
  ())


(jazz.define-virtual-syntax (jazz.emit-parameter (jazz.Parameter parameter) declaration environment))


;;;
;;;; Dynamic Parameter
;;;


(jazz.define-class-syntax jazz.Dynamic-Parameter jazz.Parameter (name type setters) jazz.Object-Class jazz.allocate-dynamic-parameter
  ((class %%get-dynamic-parameter-class ())))


;;;
;;;; Optional Parameter
;;;


(jazz.define-class-syntax jazz.Optional-Parameter jazz.Parameter (name type setters) jazz.Object-Class jazz.allocate-optional-parameter
  ((default %%get-optional-parameter-default %%set-optional-parameter-default)))


;;;
;;;; Named Parameter
;;;


(jazz.define-class-syntax jazz.Named-Parameter jazz.Parameter (name type setters) jazz.Object-Class jazz.allocate-named-parameter
  ((default %%get-named-parameter-default %%set-named-parameter-default)))


;;;
;;;; Rest Parameter
;;;


(jazz.define-class-syntax jazz.Rest-Parameter jazz.Parameter (name type setters) jazz.Object-Class jazz.allocate-rest-parameter
  ())


;;;
;;;; Self-Binding
;;;


(jazz.define-class-syntax jazz.Self-Binding jazz.Lexical-Binding (name type) jazz.Object-Class jazz.allocate-self-binding
  ())


;;;
;;;; Macro Symbol
;;;


(jazz.define-class-syntax jazz.Macro-Symbol jazz.Symbol-Binding (name type) jazz.Object-Class jazz.allocate-macro-symbol
  ((getter %%get-macro-symbol-getter ())
   (setter %%get-macro-symbol-setter ())))


;;;
;;;; Form Binding
;;;


(jazz.define-class-syntax jazz.Form-Binding jazz.Lexical-Binding (name type) jazz.Object-Class ()
  ())


;;;
;;;; Special Form
;;;


(jazz.define-class-syntax jazz.Special-Form jazz.Form-Binding (name type) jazz.Object-Class jazz.allocate-special-form
  ((walk %%get-special-form-walk ())))


;;;
;;;; Macro Form
;;;


(jazz.define-class-syntax jazz.Macro-Form jazz.Form-Binding (name type) jazz.Object-Class jazz.allocate-macro-form
  ((expander %%get-macro-form-expander ())))


;;;
;;;; Annotated Variable
;;;


(jazz.define-class-syntax jazz.Annotated-Variable jazz.Object () jazz.Object-Class jazz.allocate-annotated-variable
  ((variable %%get-annotated-variable-variable ())
   (type     %%get-annotated-variable-type     %%set-annotated-variable-type)))


;;;
;;;; Annotated Frame
;;;


(jazz.define-class-syntax jazz.Annotated-Frame jazz.Object () jazz.Object-Class jazz.allocate-annotated-frame
  ((variables %%get-annotated-frame-variables ())
   (reset     %%get-annotated-frame-reset     ())))


;;;
;;;; Code
;;;


(jazz.define-class-syntax jazz.Code jazz.Object () jazz.Object-Class jazz.allocate-code
  ((form %%get-code-form ())
   (type %%get-code-type ())))


;;;
;;;; Slot Access
;;;


;; this should be moved to jazz
(jazz.define-class-syntax jazz.Access jazz.Object () jazz.Object-Class jazz.allocate-access
  ((name    %%get-access-name    ())
   (context %%get-access-context ())))


;;;
;;;; Expression
;;;


(jazz.define-class-syntax jazz.Expression jazz.Object () jazz.Object-Class ()
  ((type %%get-expression-type ())))


(jazz.define-virtual-syntax (jazz.emit-expression (jazz.Expression expression) declaration environment))
(jazz.define-virtual-syntax (jazz.emit-call (jazz.Expression expression) arguments declaration environment))
(jazz.define-virtual-syntax (jazz.fold-expression (jazz.Expression expression) f k s))


;;;
;;;; Constant
;;;


(jazz.define-class-syntax jazz.Constant jazz.Expression (type) jazz.Object-Class jazz.allocate-constant
  ((expansion %%get-constant-expansion ())))


;;;
;;;; Quasiquote
;;;


(jazz.define-class-syntax jazz.Quasiquote jazz.Expression (type) jazz.Object-Class jazz.allocate-quasiquote
  ((form %%get-quasiquote-form ())))


;;;
;;;; Reference
;;;


(jazz.define-class-syntax jazz.Reference jazz.Expression (type) jazz.Object-Class jazz.allocate-reference
  ((binding %%get-reference-binding ())))


;;;
;;;; Assignment
;;;


(jazz.define-class-syntax jazz.Assignment jazz.Expression (type) jazz.Object-Class jazz.allocate-assignment
  ((binding %%get-assignment-binding ())
   (value   %%get-assignment-value   ())))


;;;
;;;; Lambda
;;;


(jazz.define-class-syntax jazz.Lambda jazz.Expression (type) jazz.Object-Class jazz.allocate-lambda
  ((signature %%get-lambda-signature ())
   (body      %%get-lambda-body      ())))


;;;
;;;; Let
;;;


(jazz.define-class-syntax jazz.Let jazz.Expression (type) jazz.Object-Class jazz.allocate-let
  ((bindings %%get-let-bindings ())
   (body     %%get-let-body     ())))


;;;
;;;; Named Let
;;;


(jazz.define-class-syntax jazz.Named-Let jazz.Let (type bindings body) jazz.Object-Class jazz.allocate-named-let
  ((variable %%get-named-let-variable ())))


;;;
;;;; Letstar
;;;


(jazz.define-class-syntax jazz.Letstar jazz.Expression (type) jazz.Object-Class jazz.allocate-letstar
  ((bindings %%get-letstar-bindings ())
   (body     %%get-letstar-body     ())))


;;;
;;;; Letrec
;;;


(jazz.define-class-syntax jazz.Letrec jazz.Expression (type) jazz.Object-Class jazz.allocate-letrec
  ((bindings %%get-letrec-bindings ())
   (body     %%get-letrec-body     ())))


;;;
;;;; Receive
;;;


(jazz.define-class-syntax jazz.Receive jazz.Expression (type) jazz.Object-Class jazz.allocate-receive
  ((variables  %%get-receive-variables  ())
   (expression %%get-receive-expression ())
   (body       %%get-receive-body       ())))


;;;
;;;; Body
;;;


(jazz.define-class-syntax jazz.Body jazz.Expression (type) jazz.Object-Class jazz.allocate-body
  ((internal-defines %%get-body-internal-defines ())
   (expressions      %%get-body-expressions      ())))


;;;
;;;; Internal Define
;;;


(jazz.define-class-syntax jazz.Internal-Define jazz.Expression (type) jazz.Object-Class jazz.allocate-internal-define
  ((variable %%get-internal-define-variable ())
   (value    %%get-internal-define-value    ())))


;;;
;;;; Begin
;;;


(jazz.define-class-syntax jazz.Begin jazz.Expression (type) jazz.Object-Class jazz.allocate-begin
  ((expressions %%get-begin-expressions ())))


;;;
;;;; Call
;;;


(jazz.define-class-syntax jazz.Call jazz.Expression (type) jazz.Object-Class jazz.allocate-call
  ((operator  %%get-call-operator  ())
   (arguments %%get-call-arguments ())))


;;;
;;;; If
;;;


(jazz.define-class-syntax jazz.If jazz.Expression (type) jazz.Object-Class jazz.allocate-if
  ((test %%get-if-test ())
   (yes  %%get-if-yes ())
   (no   %%get-if-no ())))


;;;
;;;; Cond
;;;


(jazz.define-class-syntax jazz.Cond jazz.Expression (type) jazz.Object-Class jazz.allocate-cond
  ((clauses %%get-cond-clauses ())))


;;;
;;;; Case
;;;


(jazz.define-class-syntax jazz.Case jazz.Expression (type) jazz.Object-Class jazz.allocate-case
  ((target  %%get-case-target  ())
   (clauses %%get-case-clauses ())))


;;;
;;;; And
;;;


(jazz.define-class-syntax jazz.And jazz.Expression (type) jazz.Object-Class jazz.allocate-and
  ((expressions %%get-and-expressions ())))


;;;
;;;; Or
;;;


(jazz.define-class-syntax jazz.Or jazz.Expression (type) jazz.Object-Class jazz.allocate-or
  ((expressions %%get-or-expressions ())))


;;;
;;;; C Include
;;;


(jazz.define-class-syntax jazz.C-Include jazz.Expression (type) jazz.Object-Class jazz.allocate-c-include
  ((name %%get-c-include-name ())))


;;;
;;;; C Declare
;;;


(jazz.define-class-syntax jazz.C-Declare jazz.Expression (type) jazz.Object-Class jazz.allocate-c-declare
  ((code %%get-c-declare-code ())))


;;;
;;;; C Initialize
;;;


(jazz.define-class-syntax jazz.C-Initialize jazz.Expression (type) jazz.Object-Class jazz.allocate-c-initialize
  ((code %%get-c-initialize-code ())))


;;;
;;;; C Function
;;;


(jazz.define-class-syntax jazz.C-Function jazz.Expression (type) jazz.Object-Class jazz.allocate-c-function
  ((expansion %%get-c-function-expansion ())))


;;;
;;;; Time
;;;


(jazz.define-class-syntax jazz.Time jazz.Expression (type) jazz.Object-Class jazz.allocate-time
  ((expression %%get-time-expression ())))


;;;
;;;; Core Dialect
;;;


(jazz.define-class-syntax jazz.Core-Dialect jazz.Dialect () jazz.Object-Class jazz.allocate-core-dialect
  ())


;;;
;;;; Core Walker
;;;


(jazz.define-class-syntax jazz.Core-Walker jazz.Walker (warnings errors) jazz.Object-Class jazz.allocate-core-walker
  ()))
