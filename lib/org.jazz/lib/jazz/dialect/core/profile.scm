;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Profiling
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


(module jazz.dialect.core.profile


(jazz.define-variable profile-start!)
(jazz.define-variable profile-stop!)
(jazz.define-variable profile-reset!)
(jazz.define-variable write-profile-report)


;;;
;;;; Statprof
;;;


(define (jazz.load-statprof)
  (if (not jazz.statprof-loaded?)
      (begin
        (jazz.load-module 'statprof)
        (set! jazz.statprof-loaded? #t))))


(define (jazz.start-statprof)
  (jazz.load-statprof)
  (profile-start!))


(define (jazz.stop-statprof)
  (jazz.load-statprof)
  (profile-stop!))


(define (jazz.reset-statprof)
  (jazz.load-statprof)
  (profile-reset!))


(define jazz.report-statprof
  (let ((n 0))
    (lambda (#!optional (name #f))
      (jazz.load-statprof)
      (let ((port (open-output-string)))
        (display (or name "report") port)
        (display "_" port)
        (display n port)
        (display ".spr" port)
        (set! n (+ n 1))
        (let ((pathname (get-output-string port)))
          (write-profile-report pathname)
          (profile-reset!)
          pathname))))))
