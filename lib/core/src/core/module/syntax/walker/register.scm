;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Register
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


(unit protected core.module.syntax.walker.register


;;;
;;;; Register Core
;;;


(jazz:define-dialect core
  (jazz:new-core-dialect))


(jazz:define-walker-special require             core jazz:walk-require)
(jazz:define-walker-special export              core jazz:walk-export)
(jazz:define-walker-special import              core jazz:walk-import)
(jazz:define-walker-special proclaim            core jazz:walk-proclaim)
(jazz:define-walker-special native              core jazz:walk-native)
(jazz:define-walker-special native-syntax       core jazz:walk-native-syntax)
(jazz:define-walker-special macro               core jazz:walk-macro)
(jazz:define-walker-special local-macro         core jazz:walk-local-macro)
(jazz:define-walker-special syntax              core jazz:walk-syntax)
(jazz:define-walker-special define-syntax       core jazz:walk-define-syntax)
(jazz:define-walker-special define-local-syntax core jazz:walk-define-local-syntax)
(jazz:define-walker-special let-syntax          core jazz:walk-let-syntax)
(jazz:define-walker-special letrec-syntax       core jazz:walk-letrec-syntax)
(jazz:define-walker-special reference           core jazz:walk-reference)
(jazz:define-walker-special walk-failed?        core jazz:walk-walk-failed))
