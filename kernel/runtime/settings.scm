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


(cond-expand
  (gambit
    (declare (block)
             (standard-bindings)
             (extended-bindings)
             (not safe)))
  (else))


;;;
;;;; Verbose
;;;


(define jazz.load-verbose?
  #t)

(define jazz.parse-verbose?
  #f)

(define jazz.done-verbose?
  #f)

(define jazz.compile-verbose?
  #f)


;;;
;;;; Walker
;;;


;; code walker warnings
(define jazz.warnings?
  #f)

;; Set to #f to debug the walker itself
(define jazz.delay-reporting?
  #t)


;;;
;;;; Print
;;;


;; Print Jazz objects by calling their print method?
(define jazz.use-print?
  #t)


;;;
;;;; Debug
;;;


(cond-expand
  (release
    (define jazz.inline-definitions?
      #t))
  (else
   (define jazz.inline-definitions?
      #f)))


;; general purpose debugging parameter
(define jazz.debug?
  (make-parameter #f))


(define jazz.debug-specializers
  '())


;;;
;;;; Profile
;;;


(define jazz.profile-walker?
  #f)


;;;
;;;; App
;;;


(define jazz.default-app
  #f)


(define jazz.default-username
  #f)


;; to enable timing loading of jedi
(define jazz.run-loop?
  #t)
