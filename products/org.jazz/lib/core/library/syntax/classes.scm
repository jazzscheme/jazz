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


(jazz.define-class jazz.Walk-Binding jazz.Object () jazz.Object-Class ()
  ())


;;;
;;;; Lexical Binding
;;;


(jazz.define-class jazz.Lexical-Binding jazz.Walk-Binding () jazz.Object-Class ()
  ((name %%get-lexical-binding-name ())))


;;;
;;;; Declaration
;;;


(jazz.define-class jazz.Declaration jazz.Lexical-Binding (name) jazz.Object-Class ()
  ((access        %%get-declaration-access        ())
   (compatibility %%get-declaration-compatibility ())
   (attributes    %%get-declaration-attributes    ())
   (toplevel      %%get-declaration-toplevel      %%set-declaration-toplevel)
   (parent        %%get-declaration-parent        %%set-declaration-parent)
   (children      %%get-declaration-children      %%set-declaration-children)))


;;;
;;;; Declaration Reference
;;;


(jazz.define-class jazz.Declaration-Reference jazz.Object () jazz.Object-Class ()
  ((name        %%get-declaration-reference-name        ())
   (declaration %%get-declaration-reference-declaration %%set-declaration-reference-declaration)))


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


(jazz.define-class jazz.Module-Declaration jazz.Declaration (name access compatibility attributes toplevel parent children) jazz.Object-Class jazz.allocate-module-declaration
  ((requires %%get-module-declaration-requires ())))


;;;
;;;; Namespace
;;;


(jazz.define-class jazz.Namespace-Declaration jazz.Declaration (name access compatibility attributes toplevel parent children) jazz.Object-Class ()
  ((lookup %%get-namespace-declaration-lookup ())))


;;;
;;;; Library
;;;


(jazz.define-class jazz.Library-Declaration jazz.Namespace-Declaration (name access compatibility attributes toplevel parent children lookup) jazz.Object-Class jazz.allocate-library-declaration
  ((dialect  %%get-library-declaration-dialect  ())
   (requires %%get-library-declaration-requires ())
   (exports  %%get-library-declaration-exports  ())
   (imports  %%get-library-declaration-imports  ())
   (exported %%get-library-declaration-exported %%set-library-declaration-exported)))


;;;
;;;; Library Invoice
;;;


(jazz.define-class jazz.Library-Invoice jazz.Object () jazz.Object-Class ()
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


(jazz.define-class jazz.Export-Invoice jazz.Library-Invoice (library phase version only except prefix rename) jazz.Object-Class jazz.allocate-export-invoice
  ((autoload %%get-export-invoice-autoload ())))


;;;
;;;; Import Invoice
;;;


(jazz.define-class jazz.Import-Invoice jazz.Library-Invoice (library phase version only except prefix rename) jazz.Object-Class jazz.allocate-import-invoice
  ())


;;;
;;;; Export
;;;


(jazz.define-class jazz.Export-Declaration jazz.Declaration (name access compatibility attributes toplevel parent children) jazz.Object-Class jazz.allocate-export-declaration
  ((symbol %%get-export-declaration-symbol ())))


;;;
;;;; Autoload
;;;


(jazz.define-class jazz.Autoload-Declaration jazz.Declaration (name access compatibility attributes toplevel parent children) jazz.Object-Class jazz.allocate-autoload-declaration
  ((declaration %%get-autoload-declaration-declaration ())))


;;;
;;;; Macro
;;;


(jazz.define-class jazz.Macro-Declaration jazz.Declaration (name access compatibility attributes toplevel parent children) jazz.Object-Class jazz.allocate-macro-declaration
  ())


;;;
;;;; Syntax
;;;


(jazz.define-class jazz.Syntax-Declaration jazz.Declaration (name access compatibility attributes toplevel parent children) jazz.Object-Class jazz.allocate-syntax-declaration
  ())


;;;
;;;; C Type
;;;


(jazz.define-class jazz.C-Type-Declaration jazz.Declaration (name access compatibility attributes toplevel parent children) jazz.Object-Class jazz.allocate-c-type-declaration
  ((expansion  %%get-c-type-declaration-expansion  ())
   (references %%get-c-type-declaration-references ())))


;;;
;;;; C Definition
;;;


(jazz.define-class jazz.C-Definition-Declaration jazz.Declaration (name access compatibility attributes toplevel parent children) jazz.Object-Class jazz.allocate-c-definition-declaration
  ((signature %%get-c-definition-declaration-signature ())))


;;;
;;;; Walker
;;;


(jazz.define-class jazz.Walker jazz.Object () jazz.Object-Class ()
  ((warnings              %%get-walker-warnings              %%set-walker-warnings)
   (errors                %%get-walker-errors                %%set-walker-errors)
   ;; this will probably become a generic references field later on...
   ;; (also probably move references field from c-type-declaration to declaration...)
   (literals              %%get-walker-literals              %%set-walker-literals)
   (c-references          %%get-walker-c-references          %%set-walker-c-references)
   (direct-dependencies   %%get-walker-direct-dependencies   %%set-walker-direct-dependencies)
   (autoload-declarations %%get-walker-autoload-declarations %%set-walker-autoload-declarations)))


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
   (declaration-locator %%get-walk-location-declaration-locator ())))


;;;
;;;; Walk Error
;;;


(jazz.define-class jazz.Walk-Error jazz.Error (message) jazz.Object-Class jazz.allocate-walk-error
  ((location %%get-walk-error-location ())))


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
;;;; Walk Signature
;;;


(jazz.define-class jazz.Walk-Signature jazz.Object () jazz.Object-Class jazz.allocate-walk-signature
  ((parameters %%get-walk-signature-parameters ())
   (mandatory  %%get-walk-signature-mandatory  ())
   (rest?      %%get-walk-signature-rest?      ())))


;;;
;;;; Symbol Binding
;;;


(jazz.define-class jazz.Symbol-Binding jazz.Lexical-Binding (name) jazz.Object-Class ()
  ())


;;;
;;;; Variable
;;;


(jazz.define-class jazz.Variable jazz.Symbol-Binding (name) jazz.Object-Class jazz.allocate-variable
  ())


;;;
;;;; RestVariable
;;;


(jazz.define-class jazz.RestVariable jazz.Variable (name) jazz.Object-Class jazz.allocate-restvariable
  ())


;;;
;;;; NextMethodVariable
;;;


(jazz.define-class jazz.NextMethodVariable jazz.Variable (name) jazz.Object-Class jazz.allocate-nextmethodvariable
  ())


;;;
;;;; Self-Binding
;;;


(jazz.define-class jazz.Self-Binding jazz.Lexical-Binding (name) jazz.Object-Class jazz.allocate-self-binding
  ())


;;;
;;;; Macro Symbol
;;;


(jazz.define-class jazz.Macro-Symbol jazz.Symbol-Binding (name) jazz.Object-Class jazz.allocate-macro-symbol
  ((getter %%get-macro-symbol-getter ())
   (setter %%get-macro-symbol-setter ())))


;;;
;;;; Form Binding
;;;


(jazz.define-class jazz.Form-Binding jazz.Lexical-Binding (name) jazz.Object-Class ()
  ())


;;;
;;;; Special Form
;;;


(jazz.define-class jazz.Special-Form jazz.Form-Binding (name) jazz.Object-Class jazz.allocate-special-form
  ((walk %%get-special-form-walk ())))


;;;
;;;; Macro Form
;;;


(jazz.define-class jazz.Macro-Form jazz.Form-Binding (name) jazz.Object-Class jazz.allocate-macro-form
  ((expander %%get-macro-form-expander ())))


;;;
;;;; Reference
;;;


;; this should be moved to jazz
(jazz.define-class jazz.Reference jazz.Object () jazz.Object-Class jazz.allocate-reference
  ((form    %%get-reference-form    ())
   (context %%get-reference-context ())))


;;;
;;;; Core Dialect
;;;


(jazz.define-class jazz.Core-Dialect jazz.Dialect () jazz.Object-Class jazz.allocate-core-dialect
  ())


;;;
;;;; Core Walker
;;;


(jazz.define-class jazz.Core-Walker jazz.Walker (warnings errors literals c-references direct-dependencies autoload-declarations) jazz.Object-Class jazz.allocate-core-walker
  ()))
