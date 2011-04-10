;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Foundation Dialect Runtime
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


(unit protected foundation.syntax.dialect.runtime


;;;
;;;; Dialect
;;;


(jazz:define-class-runtime jazz:Foundation-Dialect)


(define (jazz:new-foundation-dialect name)
  (jazz:allocate-foundation-dialect jazz:Foundation-Dialect name '()))


(jazz:define-method (jazz:dialect-walker (jazz:Foundation-Dialect dialect))
  (jazz:new-foundation-walker))


(jazz:encapsulate-class jazz:Foundation-Dialect)


;;;
;;;; Register
;;;


(jazz:define-dialect foundation
  (jazz:new-foundation-dialect 'foundation))


(jazz:define-walker-special require             foundation jazz:walk-require)
(jazz:define-walker-special export              foundation jazz:walk-export)
(jazz:define-walker-special import              foundation jazz:walk-import)
(jazz:define-walker-special proclaim            foundation jazz:walk-proclaim)
(jazz:define-walker-special native              foundation jazz:walk-native)
(jazz:define-walker-special native-syntax       foundation jazz:walk-native-syntax)
(jazz:define-walker-special macro               foundation jazz:walk-macro)
(jazz:define-walker-special local-macro         foundation jazz:walk-local-macro)
(jazz:define-walker-special syntax              foundation jazz:walk-syntax)
(jazz:define-walker-special define-syntax       foundation jazz:walk-define-syntax)
(jazz:define-walker-special define-local-syntax foundation jazz:walk-define-local-syntax)
(jazz:define-walker-special let-syntax          foundation jazz:walk-let-syntax)
(jazz:define-walker-special letrec-syntax       foundation jazz:walk-letrec-syntax)
(jazz:define-walker-special reference           foundation jazz:walk-reference)
(jazz:define-walker-special walk-failed?        foundation jazz:walk-walk-failed))
