;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Jazz Install
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
;;;  Portions created by the Initial Developer are Copyright (C) 1996-2015
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


(unit jazz.install


;;;
;;;; Literals
;;;


;; having the literal class as a literal is a bit strange but usefull for marshalling
(jazz:define-literal Literal          jazz.literals:construct-literal)
(jazz:define-literal Box              jazz.literals:construct-box)
(jazz:define-literal Point            jazz.literals:construct-point)
(jazz:define-literal Point$fl$        jazz.literals:construct-point$fl$)
(jazz:define-literal Distance         jazz.literals:construct-distance)
(jazz:define-literal Dimension        jazz.literals:construct-dimension)
(jazz:define-literal Dimension$fl$    jazz.literals:construct-dimension$fl$)
(jazz:define-literal Cell             jazz.literals:construct-cell)
(jazz:define-literal Rect             jazz.literals:construct-rect)
(jazz:define-literal Rect$fl$         jazz.literals:construct-rect$fl$)
(jazz:define-literal Range            jazz.literals:construct-range)
(jazz:define-literal Exception-Detail jazz.literals:construct-exception-detail)
(jazz:define-literal Walk-Location    jazz.literals:construct-walk-location))
