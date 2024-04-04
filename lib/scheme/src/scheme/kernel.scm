;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Scheme Core Kernel
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


(module scheme.kernel scheme


;;;
;;;; Annotate
;;;


(native jazz:find-annotated)
(native jazz:find-annotated-type)
(native jazz:annotate-let)


;;;
;;;; Class
;;;


(native jazz:method-implementation-name)
(native jazz:add-core-method-node)
(native jazz:find-nextmethod)
(native jazz:emit-one-expression)
(native jazz:emit-expressions)
(native jazz:emit-statements-code)
(native jazz:emit)
(native jazz:new-code)
(native jazz:object-size)
(native jazz:new-object)
(native jazz:set-object-class)
(native jazz:get-class-level)
(native jazz:get-object-slot)
(native jazz:set-object-slot)
(native jazz:reference-name)
(native jazz:new-core-class)
(native jazz:set-core-class)
(native jazz:tree-fold)
(native jazz:tree-fold-list)


;;;
;;;; Dialect
;;;


(native jazz:get-dialect)
(native jazz:require-dialect)
(native jazz:register-declaration)
(native jazz:register-binding)
(native jazz:new-declaration-form)
(native jazz:new-special-form)
(native jazz:setup-declaration)
(native jazz:get-declaration-path)
(native jazz:get-declaration-toplevel)
(native jazz:walk)
(native jazz:walk-list)
(native jazz:get-and-expressions)
(native jazz:get-expression-source)
(native jazz:compose-helper)
(native jazz:new-special-expression)


;;;
;;;; Module
;;;


(native jazz:iterate-module-declaration)


;;;
;;;; Walker
;;;


(native jazz:parse-modifiers)
(native jazz:parse-keywords)
(native jazz:current-walker)
(native jazz:current-resume)
(native jazz:current-declaration)
(native jazz:current-declaration-name))
