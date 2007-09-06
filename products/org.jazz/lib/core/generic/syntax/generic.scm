;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Generic Methods
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
;;;  Portions created by the Initial Developer are Copyright (C) 1996-2006
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


(module core.generic.syntax.generic


(jazz.define-class-syntax jazz.Generic jazz.Object () jazz.Object-Class jazz.allocate-generic
  ((locator           %%get-generic-locator           ())
   (name              %%get-generic-name              ())
   (root-specific     %%get-generic-root-specific     %%set-generic-root-specific)
   (pending-specifics %%get-generic-pending-specifics %%set-generic-pending-specifics)))


(jazz.define-class jazz.Generic jazz.Object () jazz.Object-Class
  (locator
   name
   root-specific
   pending-specifics))


(jazz.define-macro (jazz.define-generic . rest)
  (apply jazz.expand-define-generic rest))


; 5.7 = 5.9 = z + by-class (5.2 + 0.7)
; 7.6 = 7.8 = z + by-root  (5.2 + 2.6)
; 8.3 = 8.5 = z + by-root + by-class (Presario X1000 for 154000 calls)
(jazz.define-macro (%%need-specific-implementation generic object)
  `(%%class-dispatch (%%class-of ,object) (%%get-generic-name ,generic))
  ;; `(%%get-specific-implementation (jazz.dispatch-from-root (%%class-of ,object) ,generic))
  )


(jazz.define-macro (%%class-dispatch class name)
  `(%%hashtable-ref (%%get-class-dispatch-table ,class) ,name #f))


(jazz.encapsulate-class jazz.Generic))
