;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Verbose
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
;;;  Portions created by the Initial Developer are Copyright (C) 1996-2018
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


(##actlog-stop)


;;;
;;;; Time
;;;


(define jazz:current-seconds
  (let ((f64vec (^#f64vector 0.)))
    (lambda ()
      (declare (not interrupts-enabled))
      (^#get-current-time! f64vec 0)
      (^#f64vector-ref f64vec 0))))


(define jazz:current-monotonic
  (let ((u64vec (^#u64vector 0)))
    (lambda ()
      (declare (not interrupts-enabled))
      (^#get-monotonic-time! u64vec 0)
      (^#u64vector-ref u64vec 0))))


;;;
;;;; Boot
;;;


;; put here to be as early as possible in the boot
(define jazz:kernel-boot
  (jazz:current-seconds))


;;;
;;;; Verbose
;;;


(define jazz:kernel-verbose?
  #f)


(define (jazz:verbose-kernel unit)
  (if jazz:kernel-verbose?
      (let ((port (console-port)))
        (display "; loading " port)
        (display unit port)
        (display "..." port)
        (newline port)
        (force-output port))))
