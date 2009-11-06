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


(unit protected jazz.dialect.core.profile


(jazz.define-variable active-profile)
(jazz.define-variable profile-total)
(jazz.define-variable profile-unknown)
(jazz.define-variable profile-calls)
(jazz.define-variable profile-new)
(jazz.define-variable profile-reset!)
(jazz.define-variable profile-start!)
(jazz.define-variable profile-stop!)
(jazz.define-variable profile-running?)


;;;
;;;; Statprof
;;;


(define jazz.statprof-loaded?
  #f)


(define (jazz.load-statprof)
  (if (%%not jazz.statprof-loaded?)
      (begin
        (jazz.load-unit 'statprof)
        (set! jazz.statprof-loaded? #t))))


(define (jazz.active-profile)
  (jazz.load-statprof)
  (active-profile))


(define (jazz.profile-total profile)
  (jazz.load-statprof)
  (profile-total profile))


(define (jazz.profile-unknown profile)
  (jazz.load-statprof)
  (profile-unknown profile))


(define (jazz.profile-calls profile)
  (jazz.load-statprof)
  (profile-calls profile))


(define (jazz.new-profile . rest)
  (jazz.load-statprof)
  (%%apply profile-new rest))


(define (jazz.reset-profile)
  (jazz.load-statprof)
  (profile-reset!))


(define (jazz.start-profile)
  (jazz.load-statprof)
  (profile-start!))


(define (jazz.stop-profile)
  (jazz.load-statprof)
  (profile-stop!))


(define (jazz.profile-running?)
  (jazz.load-statprof)
  (profile-running?)))
