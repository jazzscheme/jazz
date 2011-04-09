;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Base Dialect Runtime
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


(unit protected dialect.base.syntax.dialect.runtime


;;;
;;;; Dialect
;;;


(jazz:define-class-runtime jazz:Base-Dialect)


(define (jazz:new-base-dialect name)
  (jazz:allocate-base-dialect jazz:Base-Dialect name '()))


(jazz:define-method (jazz:dialect-walker (jazz:Base-Dialect dialect))
  (jazz:new-base-walker))


(jazz:encapsulate-class jazz:Base-Dialect)


;;;
;;;; Register
;;;


(jazz:define-dialect base
  (jazz:new-base-dialect 'base))


(jazz:define-walker-special require             base jazz:walk-require)
(jazz:define-walker-special export              base jazz:walk-export)
(jazz:define-walker-special import              base jazz:walk-import)
(jazz:define-walker-special proclaim            base jazz:walk-proclaim)
(jazz:define-walker-special native              base jazz:walk-native)
(jazz:define-walker-special native-syntax       base jazz:walk-native-syntax)
(jazz:define-walker-special macro               base jazz:walk-macro)
(jazz:define-walker-special local-macro         base jazz:walk-local-macro)
(jazz:define-walker-special syntax              base jazz:walk-syntax)
(jazz:define-walker-special define-syntax       base jazz:walk-define-syntax)
(jazz:define-walker-special define-local-syntax base jazz:walk-define-local-syntax)
(jazz:define-walker-special let-syntax          base jazz:walk-let-syntax)
(jazz:define-walker-special letrec-syntax       base jazz:walk-letrec-syntax)
(jazz:define-walker-special reference           base jazz:walk-reference)
(jazz:define-walker-special walk-failed?        base jazz:walk-walk-failed))
