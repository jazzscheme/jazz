;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Features
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


(jazz:verbose-kernel 'kernel.features)


(define-macro (jazz:feature name)
  `(if ,name
       '(,name)
     '()))


(define jazz:c-features
  (append
    ;; os
    (jazz:feature USE_POSIX)
    (jazz:feature USE_WIN32)
    ;; pumps
    (jazz:feature USE_PUMPS)
    ;; real time
    (jazz:feature USE_clock_gettime_realtime)
    (jazz:feature USE_getclock)
    (jazz:feature USE_GetSystemTimeAsFileTime)
    (jazz:feature USE_gettimeofday)
    (jazz:feature USE_ftime)
    (jazz:feature USE_time)
    (jazz:feature USE_CLASSIC_MACOS)
    ;; monotonic time
    (jazz:feature USE_mach_absolute_time)
    (jazz:feature USE_QueryPerformanceCounter)
    (jazz:feature USE_clock_gettime_monotonic)
    ;; sleep
    (jazz:feature USE_nanosleep)
    (jazz:feature USE_Sleep)
    (jazz:feature USE_sleep)
    ;; heartbeat
    (jazz:feature USE_setitimer)
    (jazz:feature USE_dos_setvect)
    (jazz:feature USE_DosStartTimer)
    (jazz:feature USE_VInstall)
    (jazz:feature USE_CreateThread)
    ;; select
    (jazz:feature USE_MsgWaitForMultipleObjects)
    (jazz:feature USE_poll)
    (jazz:feature USE_ppoll)
    (jazz:feature USE_select)))


;;;
;;;; Features
;;;


(define-macro (jazz:install-features)
  (let ((features `(jazz Jazz JAZZ jazzscheme JazzScheme JAZZSCHEME ,jazz:kernel-system ,jazz:kernel-platform ,jazz:kernel-compiler ,jazz:kernel-processor ,jazz:kernel-windowing ,jazz:kernel-safety ,@jazz:kernel-features ,@(if jazz:kernel-track-memory? '(track) '()))))
    (for-each (lambda (feature)
                (if feature
                    (^#cond-expand-features (append (^#cond-expand-features) (list feature)))))
              features)
    `(for-each (lambda (feature)
                 (if feature
                     (^#cond-expand-features (append (^#cond-expand-features) (list feature)))))
               ',features)))


(jazz:install-features)
