;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Kernel Architecture
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


(include "~~/lib/_gambit#.scm")


(cond-expand
  (gambit
    (declare (block)
             (standard-bindings)
             (extended-bindings)))
  (else))


;;;
;;;; Features
;;;


(define-macro (jazz.install-features)
  (for-each (lambda (feature)
              (set! ##cond-expand-features (##cons feature ##cond-expand-features)))
            jazz.architecture)
  `(for-each (lambda (feature)
               (set! ##cond-expand-features (##cons feature ##cond-expand-features)))
             ',jazz.architecture))


(jazz.install-features)


;;;
;;;; System
;;;


(cond-expand
  (gambit
    (define jazz.system 'gambit)))


;;;
;;;; Platform
;;;


(cond-expand
  (mac
    (define jazz.platform 'mac))
  (windows
    (define jazz.platform 'windows))
  (x11
    (define jazz.platform 'x11)))


;;;
;;;; Processor
;;;


(cond-expand
  (intel
    (define jazz.processor 'intel)))


;;;
;;;; Safety
;;;


;; core - core debug mode with tests to make the core safe
;; debug - standard debug mode with tests to make user code safe
;; release - release mode without tests for stable user code


(cond-expand
  (core
    (define jazz.safety 'core))
  (debug
    (define jazz.safety 'debug))
  (release
    (define jazz.safety 'release)))


(define jazz.debug-core?
  (eq? jazz.safety 'core))

(define jazz.debug-user?
  (not (eq? jazz.safety 'release)))
