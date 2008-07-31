;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Runtime Settings
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


(jazz.kernel-declare)


;;;
;;;; Verbose
;;;


(jazz.define-variable jazz.load-verbose?
  #f)

(jazz.define-variable jazz.parse-verbose?
  #f)

(jazz.define-variable jazz.done-verbose?
  #f)

(jazz.define-variable jazz.compile-verbose?
  #f)


;;;
;;;; Walker
;;;


;; code walker warnings
(jazz.define-variable jazz.warnings?
  #f)

;; Set to #f to debug the walker itself
(jazz.define-variable jazz.delay-reporting?
  #t)


;;;
;;;; Print
;;;


;; Print Jazz objects by calling their print method?
(jazz.define-variable jazz.use-print?
  #t)


;;;
;;;; Debug
;;;


(cond-expand
  (release
    (jazz.define-variable jazz.inline-definitions?
      #t))
  (else
   (jazz.define-variable jazz.inline-definitions?
      #f)))


(jazz.define-variable jazz.debug-specializers
  '())


(jazz.define-variable jazz.debugger?
  #t)


;;;
;;;; Profile
;;;


(jazz.define-variable jazz.profile-walker?
  #f)


;;;
;;;; Product
;;;


(jazz.define-variable jazz.profile
  #f)


;; to enable timing loading of jedi
(jazz.define-variable jazz.run-loop?
  #t)
