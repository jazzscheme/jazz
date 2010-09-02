;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Profiler Support Implementation
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


(unit protected profiler.implementation


(declare (proper-tail-calls))


;;;
;;;; Settings
;;;


(jazz.define-setting default-profiler
  #f)

(jazz.define-setting default-profiler-depth
  2)

(jazz.define-setting default-profiler-ignored-procedures
  (%%list
    ##dynamic-env-bind
    ##call-with-values
    ##thread-start-action!
    ##primordial-exception-handler-hook))

(jazz.define-setting default-profiler-ignored-modules
  '())


;;;
;;;; Profiler
;;;


(define (make-profiler name start stop)
  (%%vector 'profiler name start stop default-profiler-ignored-procedures default-profiler-ignored-modules))


(define (profiler-name profiler)
  (%%vector-ref profiler 1))

(define (profiler-start profiler)
  (%%vector-ref profiler 2))

(define (profiler-stop profiler)
  (%%vector-ref profiler 3))

(define (profiler-ignored-procedures profiler)
  (%%vector-ref profiler 4))

(define (profiler-ignored-procedures-set! profiler ignored-procedures)
  (%%vector-set! profiler 4 ignored-procedures))

(define (profiler-ignored-modules profiler)
  (%%vector-ref profiler 5))

(define (profiler-ignored-modules-set! profiler ignored-modules)
  (%%vector-set! profiler 5 ignored-modules))


(define (profiler-ignore-procedure? profiler procedure)
  (%%memq procedure (profiler-ignored-procedures profiler)))

(define (profiler-ignore-module? profiler module)
  (%%memq module (profiler-ignored-modules profiler)))

(define (profiler-ignore-procedure profiler procedure)
  (if (%%not (profiler-ignore-procedure? profiler procedure))
      (profiler-ignored-procedures-set! profiler (%%cons procedure (profiler-ignored-procedures profiler)))))

(define (profiler-ignore-module profiler module)
  (if (%%not (profiler-ignore-module? profiler module))
      (profiler-ignored-modules-set! profiler (%%cons module (profiler-ignored-modules profiler)))))


;;;
;;;; Profile
;;;


(define (make-profile profiler depth)
  (%%vector 'profile profiler depth #f 0 0 0 0 0 (%%make-table test: equal?) 0))


(define (profile-profiler profile)
  (%%vector-ref profile 1))

(define (profile-depth profile)
  (%%vector-ref profile 2))

(define (profile-frame-count profile)
  (%%vector-ref profile 3))

(define (profile-frame-count-set! profile count)
  (%%vector-set! profile 3 count))

(define (profile-frame-duration profile)
  (%%vector-ref profile 4))

(define (profile-frame-duration-set! profile count)
  (%%vector-set! profile 4 count))

(define (profile-total-count profile)
  (%%vector-ref profile 5))

(define (profile-total-count-set! profile total)
  (%%vector-set! profile 5 total))

(define (profile-total-duration profile)
  (%%vector-ref profile 6))

(define (profile-total-duration-set! profile total)
  (%%vector-set! profile 6 total))

(define (profile-unknown-count profile)
  (%%vector-ref profile 7))

(define (profile-unknown-count-set! profile unknown)
  (%%vector-set! profile 7 unknown))

(define (profile-unknown-duration profile)
  (%%vector-ref profile 8))

(define (profile-unknown-duration-set! profile unknown)
  (%%vector-set! profile 8 unknown))

(define (profile-calls profile)
  (%%vector-ref profile 9))

(define (profile-user-data profile)
  (%%vector-ref profile 10))

(define (profile-user-data-set! profile user-data)
  (%%vector-set! profile 10 user-data))


(define (new-profile #!key (profiler #f) (depth #f))
  (make-profile (or profiler (default-profiler)) (or depth (default-profiler-depth))))


;;;
;;;; Profile Call
;;;


(define (make-profile-call)
  (%%cons 0 0))


(define (profile-call-count call)
  (%%car call))

(define (profile-call-count-set! call count)
  (%%set-car! call count))

(define (profile-call-duration call)
  (%%cdr call))

(define (profile-call-duration-set! call duration)
  (%%set-cdr! call duration))


;;;
;;;; Active
;;;


(define profiler-on?
  (make-parameter #f))


(define active-profile
  (make-parameter #f))


(define (reset-profile)
  (let ((active (active-profile)))
    (active-profile (make-profile (profile-profiler active) (profile-depth active)))))


;;;
;;;; Frames
;;;


(define (profile-frames profile)
  (or (profile-frame-count profile) 1))


;;;
;;;; Calls
;;;


(define (profile-call profile name)
  (let ((calls (profile-calls profile)))
    (or (%%table-ref calls name #f)
        (let ((call (make-profile-call)))
          (%%table-set! calls name call)
          call))))


(define (profile-register-call profile stack duration)
  (profile-total-count-set! profile (+ (profile-total-count profile) 1))
  (profile-total-duration-set! profile (+ (profile-total-duration profile) duration))
  (if (%%not stack)
      (begin
        (profile-unknown-count-set! profile (+ (profile-unknown-count profile) 1))
        (profile-unknown-duration-set! profile (+ (profile-unknown-duration profile) duration)))
    (let ((call (profile-call profile stack)))
      (profile-call-count-set! call (+ (profile-call-count call) 1))
      (profile-call-duration-set! call (+ (profile-call-duration call) duration)))))


;;;
;;;; Counter
;;;


(define (seconds->milliseconds x)
  (inexact->exact (round (* x 1000))))


(define profiler-performance-frequency
  #f)

(set! profiler-performance-frequency
      (lambda ()
        1000))


(define profiler-performance-counter
  #f)

(set! profiler-performance-counter
      (lambda ()
        (seconds->milliseconds (real-time))))


(define (profiler-performance-frequency-set! proc)
  (set! profiler-performance-frequency proc))


(define (profiler-performance-counter-set! proc)
  (set! profiler-performance-counter proc))


;;;
;;;; Run
;;;


(define (with-profiling profile thunk)
  (start-profiler profile)
  (parameterize ((active-profile profile))
    (let ((start (profiler-performance-counter)))
      (thunk)
      (let ((duration (- (profiler-performance-counter) start)))
        (profile-frame-count-set! profile (+ (or (profile-frame-count profile) 0) 1))
        (profile-frame-duration-set! profile (+ (profile-frame-duration profile) duration)))))
  (stop-profiler profile))


(define (start-profiler profile)
  (let ((start (profiler-start (profile-profiler profile))))
    (if start
        (start profile))))


(define (stop-profiler profile)
  (let ((stop (profiler-stop (profile-profiler profile))))
    (if stop
        (stop profile))))


;;;
;;;; Stack
;;;


(define (identify-call cont depth profiler)
  (define (continuation-creator cont)
    (let ((proc (%%continuation-creator cont)))
      (if (%%closure? proc)
          (%%closure-code proc)
        proc)))
  
  (define (continuation-next-interesting cont)
    (let loop ((current-cont cont))
         (if current-cont
             (if (or (profiler-ignore-procedure? profiler (continuation-creator current-cont))
                     (let ((current-location (%%continuation-locat current-cont)))
                       (and current-location (profiler-ignore-module? profiler (%%locat-container current-location)))))
                 (loop (%%continuation-next current-cont))
               current-cont)
           #f)))
  
  (define (continuation-next-distinct cont creator)
    (let ((creator-name (jazz.procedure-name creator)))
      (let loop ((current-cont (%%continuation-next cont)))
           (if current-cont
               (let ((current-creator (continuation-creator current-cont)))
                 (if (%%eq? creator-name (jazz.procedure-name current-creator))
                     (loop (%%continuation-next current-cont))
                   current-cont))
             #f))))
  
  (define (identify-location locat)
    ;; copy of jazz.locat->file/line/col
    (let ((container (and locat (%%locat-container locat))))
      (if container
          (let ((filepos (%%position->filepos (%%locat-position locat))))
            (let ((line (%%filepos-line filepos))
                  (col (%%filepos-col filepos)))
              (%%list container line col)))
        #f)))
  
  (define (identify cont d)
    (let ((cont (and cont (%%fx< d depth) (continuation-next-interesting cont))))
      (if (%%not cont)
          '()
        (let ((creator (continuation-creator cont))
              (location (identify-location (%%continuation-locat cont))))
          (%%cons (%%list creator location)
                  (identify (continuation-next-distinct cont creator)
                            (%%fx+ d 1)))))))
  
  (identify cont 0)))
