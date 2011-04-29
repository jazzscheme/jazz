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
  ((name         getter: generate)
   (declarations getter: generate)
   (bindings     getter: generate)))


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
  ((name getter: generate)
   (type getter: generate)
   (hits getter: generate setter: generate)))


(jazz:define-virtual-syntax (jazz:resolve-binding (jazz:Lexical-Binding binding)))


;;;
;;;; Declaration
;;;


(jazz:define-class-syntax jazz:Declaration jazz:Lexical-Binding ()
  ((access        getter: generate)
   (compatibility getter: generate)
   (attributes    getter: generate)
   (toplevel      getter: generate setter: generate)
   (parent        getter: generate setter: generate)
   (locator       getter: generate setter: generate)
   (source        getter: generate setter: generate)))


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
  ((name        getter: generate)
   (declaration getter: generate setter: generate)))


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
  ((module-reference getter: generate)))


;;;
;;;; Autoload Reference
;;;


(jazz:define-class-syntax jazz:Autoload-Reference jazz:Export-Reference (constructor: jazz:allocate-autoload-reference)
  ())


;;;
;;;; Unit
;;;


(jazz:define-class-syntax jazz:Unit-Declaration jazz:Declaration (constructor: jazz:allocate-unit-declaration)
  ((requires getter: generate setter: generate)))


;;;
;;;; Namespace
;;;


(jazz:define-class-syntax jazz:Namespace-Declaration jazz:Declaration ()
  ((lookups  getter: generate)
   (children getter: generate)
   (body     getter: generate setter: generate)))


(jazz:define-macro (jazz:get-access-lookup namespace-declaration access)
  `(%%vector-ref (jazz:get-namespace-declaration-lookups ,namespace-declaration) ,access))


;;;
;;;; Module
;;;


(jazz:define-class-syntax jazz:Module-Declaration jazz:Namespace-Declaration (constructor: jazz:allocate-module-declaration)
  ((walker          getter: generate setter: generate)
   (dialect-name    getter: generate)
   (dialect-invoice getter: generate)
   (requires        getter: generate setter: generate)
   (exports         getter: generate setter: generate)
   (imports         getter: generate setter: generate)
   (proclaims       getter: generate)
   (inclusions      getter: generate setter: generate)
   (local-macros    getter: generate)))


;;;
;;;; Module Invoice
;;;


(jazz:define-class-syntax jazz:Module-Invoice jazz:Object ()
  ((name    getter: generate)
   (module  getter: generate)
   (phase   getter: generate)
   (version getter: generate)
   (only    getter: generate)
   (except  getter: generate)
   (prefix  getter: generate)
   (rename  getter: generate)))


;;;
;;;; Export Invoice
;;;


(jazz:define-class-syntax jazz:Export-Invoice jazz:Module-Invoice (constructor: jazz:allocate-export-invoice)
  ((autoload getter: generate setter: generate)))


;;;
;;;; Import Invoice
;;;


(jazz:define-class-syntax jazz:Import-Invoice jazz:Module-Invoice (constructor: jazz:allocate-import-invoice)
  ((hit? getter: generate setter: generate)))


;;;
;;;; Export
;;;


(jazz:define-class-syntax jazz:Export-Declaration jazz:Declaration (constructor: jazz:allocate-export-declaration)
  ((symbol getter: generate)))


;;;
;;;; Export Syntax
;;;


(jazz:define-class-syntax jazz:Export-Syntax-Declaration jazz:Declaration (constructor: jazz:allocate-export-syntax-declaration)
  ((symbol getter: generate)))


;;;
;;;; Autoload
;;;


(jazz:define-class-syntax jazz:Autoload-Declaration jazz:Declaration (constructor: jazz:allocate-autoload-declaration)
  ((module           getter: generate)
   (exported-module  getter: generate)
   (declaration      getter: generate setter: generate)))


;;;
;;;; Literal
;;;


(jazz:define-class-syntax jazz:Literal jazz:Object (constructor: jazz:allocate-literal)
  ((name      getter: generate)
   (arguments getter: generate)))


;;;
;;;; Macro
;;;


(jazz:define-class-syntax jazz:Macro-Declaration jazz:Declaration (constructor: jazz:allocate-macro-declaration)
  ((signature getter: generate setter: generate)
   (body      getter: generate setter: generate)))


;;;
;;;; Local Macro
;;;


(jazz:define-class-syntax jazz:Local-Macro-Declaration jazz:Declaration (constructor: jazz:allocate-local-macro-declaration)
  ((signature getter: generate setter: generate)
   (body      getter: generate setter: generate)))


;;;
;;;; Syntax
;;;


(jazz:define-class-syntax jazz:Syntax-Declaration jazz:Declaration (constructor: jazz:allocate-syntax-declaration)
  ((signature getter: generate setter: generate)
   (body      getter: generate setter: generate)))


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
  ((type getter: generate)))


;;;
;;;; Key Type
;;;


(jazz:define-class-syntax jazz:Key-Type jazz:Type (metaclass: jazz:Class constructor: jazz:allocate-key-type)
  ((key  getter: generate)
   (type getter: generate)))


;;;
;;;; Rest Type
;;;


(jazz:define-class-syntax jazz:Rest-Type jazz:Type (metaclass: jazz:Class constructor: jazz:allocate-rest-type)
  ((type getter: generate)))


;;;
;;;; Function Type
;;;


(jazz:define-class-syntax jazz:Function-Type jazz:Type (metaclass: jazz:Class constructor: jazz:allocate-function-type)
  ((mandatory  getter: generate)
   (positional getter: generate)
   (optional   getter: generate)
   (named      getter: generate)
   (rest       getter: generate)
   (result     getter: generate)))


;;;
;;;; Category Type
;;;


(jazz:define-class-syntax jazz:Category-Type jazz:Type (metaclass: jazz:Class constructor: jazz:allocate-category-type)
  ((declaration getter: generate)))


;;;
;;;; Values Type
;;;


(jazz:define-class-syntax jazz:Values-Type jazz:Type (metaclass: jazz:Class constructor: jazz:allocate-values-type)
  ((types getter: generate)))


;;;
;;;; Restriction Type
;;;


(jazz:define-class-syntax jazz:Restriction-Type jazz:Type (metaclass: jazz:Class constructor: jazz:allocate-restriction-type)
  ((base getter: generate)
   (type getter: generate)))


;;;
;;;; Complement Type
;;;


(jazz:define-class-syntax jazz:Complement-Type jazz:Type (metaclass: jazz:Class constructor: jazz:allocate-complement-type)
  ((type getter: generate)))


;;;
;;;; Union Type
;;;


(jazz:define-class-syntax jazz:Union-Type jazz:Type (metaclass: jazz:Class constructor: jazz:allocate-union-type)
  ((types getter: generate)))


;;;
;;;; Template Type
;;;


(jazz:define-class-syntax jazz:Template-Type jazz:Type (metaclass: jazz:Class constructor: jazz:allocate-template-type)
  ((class getter: generate)
   (types getter: generate)))


;;;
;;;; Nillable Type
;;;


(jazz:define-class-syntax jazz:Nillable-Type jazz:Type (metaclass: jazz:Class constructor: jazz:allocate-nillable-type)
  ((type getter: generate)))


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
  ((declarations getter: generate setter: generate)
   (bindings     getter: generate setter: generate)
   (warnings     getter: generate setter: generate)
   (errors       getter: generate setter: generate)
   (literals     getter: generate setter: generate)
   (variables    getter: generate)
   (references   getter: generate)
   (autoloads    getter: generate setter: generate)))


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
  ((policy   getter: generate)
   (locator  getter: generate)
   (pathname getter: generate)))


;;;
;;;; Walk Location
;;;


(jazz:define-class-syntax jazz:Walk-Location jazz:Object (constructor: jazz:allocate-walk-location)
  ((unit-locator        getter: generate)
   (declaration-locator getter: generate)
   (locat               getter: generate)
   (path                getter: generate)))


;;;
;;;; Walk Problem
;;;


(jazz:define-class-syntax jazz:Walk-Problem jazz:Error ()
  ((location getter: generate)))


;;;
;;;; Walk Problems
;;;


(jazz:define-class-syntax jazz:Walk-Problems jazz:Error (constructor: jazz:allocate-walk-problems)
  ((warnings getter: generate)
   (errors   getter: generate)))


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
  ((symbol getter: generate)))


;;;
;;;; Walk Frame
;;;


(jazz:define-class-syntax jazz:Walk-Frame jazz:Walk-Binding (constructor: jazz:allocate-walk-frame)
  ((bindings getter: generate)))


;;;
;;;; Signature
;;;


(jazz:define-class-syntax jazz:Signature jazz:Object (constructor: jazz:allocate-signature)
  ((mandatory  getter: generate)
   (positional getter: generate)
   (optional   getter: generate)
   (named      getter: generate)
   (rest       getter: generate)))


;;;
;;;; Symbol Binding
;;;


(jazz:define-class-syntax jazz:Symbol-Binding jazz:Lexical-Binding ()
  ((gensym getter: generate setter: generate)))


;;;
;;;; Variable
;;;


(jazz:define-class-syntax jazz:Variable jazz:Symbol-Binding (constructor: jazz:allocate-variable)
  ((reference-count getter: generate setter: generate)))


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
  ((class getter: generate)))


;;;
;;;; Optional Parameter
;;;


(jazz:define-class-syntax jazz:Optional-Parameter jazz:Parameter (constructor: jazz:allocate-optional-parameter)
  ((default getter: generate setter: generate)))


;;;
;;;; Named Parameter
;;;


(jazz:define-class-syntax jazz:Named-Parameter jazz:Parameter (constructor: jazz:allocate-named-parameter)
  ((default getter: generate setter: generate)))


;;;
;;;; Rest Parameter
;;;


(jazz:define-class-syntax jazz:Rest-Parameter jazz:Parameter (constructor: jazz:allocate-rest-parameter)
  ())


;;;
;;;; Local-Variable-Binding
;;;


(jazz:define-class-syntax jazz:Local-Variable-Binding jazz:Lexical-Binding (constructor: jazz:allocate-local-variable-binding)
  ((variable getter: generate)))


;;;
;;;; Macro Symbol
;;;


(jazz:define-class-syntax jazz:Macro-Symbol jazz:Symbol-Binding (constructor: jazz:allocate-macro-symbol)
  ((getter getter: generate)
   (setter getter: generate)))


;;;
;;;; Form Binding
;;;


(jazz:define-class-syntax jazz:Form-Binding jazz:Lexical-Binding ()
  ())


;;;
;;;; Declaration Form
;;;


(jazz:define-class-syntax jazz:Declaration-Form jazz:Form-Binding (constructor: jazz:allocate-declaration-form)
  ((walk getter: generate)))


;;;
;;;; Special Form
;;;


(jazz:define-class-syntax jazz:Special-Form jazz:Form-Binding (constructor: jazz:allocate-special-form)
  ((walk getter: generate)))


;;;
;;;; Macro Form
;;;


(jazz:define-class-syntax jazz:Macro-Form jazz:Form-Binding (constructor: jazz:allocate-macro-form)
  ((expander getter: generate)))


;;;
;;;; Syntax Form
;;;


(jazz:define-class-syntax jazz:Syntax-Form jazz:Form-Binding (constructor: jazz:allocate-syntax-form)
  ((expander getter: generate)))


;;;
;;;; Define-Syntax Form
;;;


(jazz:define-class-syntax jazz:Define-Syntax-Form jazz:Syntax-Form (constructor: jazz:allocate-define-syntax-form)
  ((environment getter: generate)))


;;;
;;;; Define-Local-Syntax Form
;;;


(jazz:define-class-syntax jazz:Define-Local-Syntax-Form jazz:Syntax-Form (constructor: jazz:allocate-define-local-syntax-form)
  ((environment getter: generate)))


;;;
;;;; Syntactic-Closure
;;;


(jazz:define-class-syntax jazz:Syntactic-Closure jazz:Object (constructor: jazz:allocate-syntactic-closure)
  ((environment getter: generate)
   (variables   getter: generate)
   (form        getter: generate)))


;;;
;;;; Annotated Variable
;;;


(jazz:define-class-syntax jazz:Annotated-Variable jazz:Object (constructor: jazz:allocate-annotated-variable)
  ((variable      getter: generate)
   (declared-type getter: generate)
   (type          getter: generate setter: generate)))


;;;
;;;; Restricted Binding
;;;


(jazz:define-class-syntax jazz:Restricted-Binding jazz:Object (constructor: jazz:allocate-restricted-binding)
  ((binding getter: generate)
   (type    getter: generate)))


;;;
;;;; Annotated Frame
;;;


(jazz:define-class-syntax jazz:Annotated-Frame jazz:Object (constructor: jazz:allocate-annotated-frame)
  ((variables getter: generate)
   (reset     getter: generate)))


;;;
;;;; Code
;;;


(jazz:define-class-syntax jazz:Code jazz:Object (constructor: jazz:allocate-code)
  ((form   getter: generate)
   (type   getter: generate)
   (source getter: generate)))


;;;
;;;; Slot Access
;;;


;; this should be moved to jazz
(jazz:define-class-syntax jazz:Access jazz:Object (constructor: jazz:allocate-access)
  ((name    getter: generate)
   (context getter: generate)))


;;;
;;;; Expression
;;;


(jazz:define-class-syntax jazz:Expression jazz:Object ()
  ((type   getter: generate)
   (source getter: generate)))


(jazz:define-virtual-syntax (jazz:emit-expression (jazz:Expression expression) declaration environment backend))
(jazz:define-virtual-syntax (jazz:emit-call (jazz:Expression expression) arguments declaration environment backend))


;;;
;;;; Proclaim
;;;


(jazz:define-class-syntax jazz:Proclaim jazz:Expression (constructor: jazz:allocate-proclaim)
  ((clauses getter: generate)))


;;;
;;;; Constant
;;;


(jazz:define-class-syntax jazz:Constant jazz:Expression (constructor: jazz:allocate-constant)
  ((expansion getter: generate)))


;;;
;;;; Delay
;;;


(jazz:define-class-syntax jazz:Delay jazz:Expression (constructor: jazz:allocate-delay)
  ((expression getter: generate)))


;;;
;;;; Quasiquote
;;;


(jazz:define-class-syntax jazz:Quasiquote jazz:Expression (constructor: jazz:allocate-quasiquote)
  ((form getter: generate)))


;;;
;;;; Binding Reference
;;;


(jazz:define-class-syntax jazz:Binding-Reference jazz:Expression (constructor: jazz:allocate-binding-reference)
  ((binding getter: generate)))


;;;
;;;; Assignment
;;;


(jazz:define-class-syntax jazz:Assignment jazz:Expression (constructor: jazz:allocate-assignment)
  ((binding getter: generate)
   (value   getter: generate)))


;;;
;;;; Body
;;;


(jazz:define-class-syntax jazz:Body jazz:Expression (constructor: jazz:allocate-body)
  ((internal-defines getter: generate)
   (expressions      getter: generate)))


;;;
;;;; Internal Define
;;;


(jazz:define-class-syntax jazz:Internal-Define jazz:Expression (constructor: jazz:allocate-internal-define)
  ((variable getter: generate)
   (value    getter: generate)))


;;;
;;;; Begin
;;;


(jazz:define-class-syntax jazz:Begin jazz:Expression (constructor: jazz:allocate-begin)
  ((expressions getter: generate)))


;;;
;;;; Call
;;;


(jazz:define-class-syntax jazz:Call jazz:Expression (constructor: jazz:allocate-call)
  ((operator  getter: generate)
   (arguments getter: generate)))


;;;
;;;; If
;;;


(jazz:define-class-syntax jazz:If jazz:Expression (constructor: jazz:allocate-if)
  ((test getter: generate)
   (yes  getter: generate)
   (no   getter: generate)))


;;;
;;;; Cond
;;;


(jazz:define-class-syntax jazz:Cond jazz:Expression (constructor: jazz:allocate-cond)
  ((clauses getter: generate)))


;;;
;;;; Case
;;;


(jazz:define-class-syntax jazz:Case jazz:Expression (constructor: jazz:allocate-case)
  ((target  getter: generate)
   (clauses getter: generate)))


;;;
;;;; And
;;;


(jazz:define-class-syntax jazz:And jazz:Expression (constructor: jazz:allocate-and)
  ((expressions getter: generate)))


;;;
;;;; Or
;;;


(jazz:define-class-syntax jazz:Or jazz:Expression (constructor: jazz:allocate-or)
  ((expressions getter: generate)))


;;;
;;;; Declare
;;;


(jazz:define-class-syntax jazz:Declare jazz:Expression (constructor: jazz:allocate-declare)
  ((declarations getter: generate)))


;;;
;;;; Parameterize
;;;


(jazz:define-class-syntax jazz:Parameterize jazz:Expression (constructor: jazz:allocate-parameterize)
  ((bindings getter: generate)
   (body     getter: generate)))


;;;
;;;; Time Special
;;;


(jazz:define-class-syntax jazz:Time-Special jazz:Expression (constructor: jazz:allocate-time)
  ((expressions getter: generate)))


;;;
;;;; Walk-Failed Special
;;;


(jazz:define-class-syntax jazz:Walk-Failed-Special jazz:Expression (constructor: jazz:allocate-walk-failed)
  ((answer getter: generate)))


;;;
;;;; Analysis Data
;;;


(jazz:define-class-syntax jazz:Analysis-Data jazz:Object (constructor: jazz:allocate-analysis-data)
  ((autoload-reference     getter: generate setter: generate)
   (declaration-references getter: generate setter: generate))))
