;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Specific Methods
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
;;;  The Initial Developer of the Original Code is Stephane Le Cornec.
;;;  Portions created by the Initial Developer are Copyright (C) 1996-2008
;;;  the Initial Developer. All Rights Reserved.
;;;
;;;  Contributor(s):
;;;    Guillaume Cartier
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


;; dynamic-signature - classes of the dynamic parameters
;; implementation - lambda to call
;; best-ancestor - nextmethod specific
;; ancestor-specifics - nextmethod specific + other direct ancestors
;; descendant-specifics - reverse relation


(unit protected core.generic.syntax.specific


(jazz.define-class jazz.Specific jazz.Object () jazz.Object-Class jazz.allocate-specific
  ((dynamic-signature    %%get-specific-dynamic-signature    %%set-specific-dynamic-signature)
   (implementation       %%get-specific-implementation       %%set-specific-implementation)
   (ancestor-specifics   %%get-specific-ancestor-specifics   %%set-specific-ancestor-specifics)
   (descendant-specifics %%get-specific-descendant-specifics %%set-specific-descendant-specifics)))


(jazz.define-class-runtime jazz.Specific)


(jazz.define-macro (jazz.define-specific . rest)
  (%%apply jazz.expand-define-specific rest))


(jazz.encapsulate-class jazz.Specific))
