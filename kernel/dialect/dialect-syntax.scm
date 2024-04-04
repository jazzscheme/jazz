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
;;;  Portions created by the Initial Developer are Copyright (C) 1996-2018
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


(block dialect.dialect-syntax


;;;
;;;; Dialect
;;;


(jazz:define-virtual-syntax (jazz:dialect-walker (jazz:Dialect dialect)))
(jazz:define-virtual-syntax (jazz:dialect-wrap (jazz:Dialect dialect) body))


;;;
;;;; Walk Binding
;;;


(jazz:define-virtual-syntax (jazz:walk-binding-lookup (jazz:Walk-Binding binding) symbol source-declaration))
(jazz:define-virtual-syntax (jazz:walk-binding-referenced (jazz:Walk-Binding binding)))
(jazz:define-virtual-syntax (jazz:walk-binding-validate-call (jazz:Walk-Binding binding) walker resume source-declaration operator arguments form-src))
(jazz:define-virtual-syntax (jazz:walk-binding-validate-assignment (jazz:Walk-Binding binding) walker resume source-declaration symbol-src))
(jazz:define-virtual-syntax (jazz:walk-binding-assignable? (jazz:Walk-Binding binding)))
(jazz:define-virtual-syntax (jazz:walk-binding-walkable? (jazz:Walk-Binding binding)))
(jazz:define-virtual-syntax (jazz:walk-binding-walk-form (jazz:Walk-Binding binding) walker resume declaration environment form))
(jazz:define-virtual-syntax (jazz:walk-binding-expandable? (jazz:Walk-Binding binding)))
(jazz:define-virtual-syntax (jazz:walk-binding-expand-form (jazz:Walk-Binding binding) walker resume declaration environment form))
(jazz:define-virtual-syntax (jazz:emit-binding-symbol (jazz:Walk-Binding binding) source-declaration environment))
(jazz:define-virtual-syntax (jazz:emit-binding-reference (jazz:Walk-Binding binding) source-declaration walker resume environment))
(jazz:define-virtual-syntax (jazz:emit-binding-call (jazz:Walk-Binding binding) binding-src arguments arguments-codes source-declaration walker resume environment))
(jazz:define-virtual-syntax (jazz:emit-inlined-binding-call (jazz:Walk-Binding binding) arguments call source-declaration walker resume environment))
(jazz:define-virtual-syntax (jazz:emit-binding-assignment (jazz:Walk-Binding binding) value source-declaration walker resume environment form-src))


;;;
;;;; Lexical Binding
;;;


(jazz:define-virtual-syntax (jazz:resolve-binding (jazz:Lexical-Binding binding)))


;;;
;;;; Declaration
;;;


(jazz:define-virtual-syntax (jazz:compose-declaration-locator (jazz:Declaration declaration)))
(jazz:define-virtual-syntax (jazz:lookup-declaration (jazz:Declaration declaration) symbol access source-declaration))
(jazz:define-virtual-syntax (jazz:get-declaration-inclusions (jazz:Declaration declaration)))
(jazz:define-virtual-syntax (jazz:get-nextmethod-signature (jazz:Declaration declaration)))
(jazz:define-virtual-syntax (jazz:emit-declaration (jazz:Declaration declaration) walker resume environment))
(jazz:define-virtual-syntax (jazz:expand-referenced-declaration (jazz:Declaration declaration)))
(jazz:define-virtual-syntax (jazz:outline-generate (jazz:Declaration declaration) output))
(jazz:define-virtual-syntax (jazz:outline-extract (jazz:Declaration declaration) meta))


;;;
;;;; Declaration Reference
;;;


(jazz:define-virtual-syntax (jazz:resolve-reference (jazz:Declaration-Reference declaration-reference) module-declaration))


;;;
;;;; Namespace
;;;


(jazz:define-macro (jazz:get-access-lookup namespace-declaration access)
  `(%%vector-ref (jazz:get-namespace-declaration-lookups ,namespace-declaration) ,access))


;;;
;;;; Walker
;;;


(jazz:define-virtual-syntax (jazz:walker-declarations (jazz:Walker walker)))
(jazz:define-virtual-syntax (jazz:walker-bindings (jazz:Walker walker)))
(jazz:define-virtual-syntax (jazz:walk-form (jazz:Walker walker) resume declaration environment form))
(jazz:define-virtual-syntax (jazz:walk-symbol (jazz:Walker walker) resume declaration environment symbol-src))
(jazz:define-virtual-syntax (jazz:walk-symbol-assignment (jazz:Walker walker) resume declaration environment symbol-src value form-src))
(jazz:define-virtual-syntax (jazz:validate-proclaim (jazz:Walker walker) resume declaration form-src))
(jazz:define-virtual-syntax (jazz:runtime-export (jazz:Walker walker) declaration))
(jazz:define-virtual-syntax (jazz:lookup-environment (jazz:Walker walker) resume declaration environment symbol-src symbol))
(jazz:define-virtual-syntax (jazz:lookup-analyse (jazz:Walker walker) declaration symbol-src referenced-declaration))


;;;
;;;; Parameter
;;;


(jazz:define-virtual-syntax (jazz:emit-check? (jazz:Parameter parameter)))
(jazz:define-virtual-syntax (jazz:emit-parameter (jazz:Parameter parameter) declaration walker resume environment))


;;;
;;;; Expression
;;;


(jazz:define-virtual-syntax (jazz:emit-expression (jazz:Expression expression) declaration walker resume environment))
(jazz:define-virtual-syntax (jazz:emit-call (jazz:Expression expression) arguments arguments-codes declaration walker resume environment)))
