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


(unit protected profiler.implementation


(declare (proper-tail-calls))


;;;
;;;; Settings
;;;


(jazz:define-setting default-profiler
  'statprof)

(jazz:define-setting default-profiler-ignored-procedures
  (%%list
    ##dynamic-env-bind
    ##call-with-values
    ##primordial-exception-handler-hook))

(jazz:define-setting default-profiler-ignored-modules
  '())


;;;
;;;; Profiler
;;;


(define (make-profiler name start-func stop-func default-depth)
  (%%vector 'profiler name start-func stop-func default-depth (default-profiler-ignored-procedures) (default-profiler-ignored-modules)))


(define (profiler-type profiler)
  (%%vector-ref profiler 1))

(define (profiler-start-func profiler)
  (%%vector-ref profiler 2))

(define (profiler-stop-func profiler)
  (%%vector-ref profiler 3))

(define (profiler-default-depth profiler)
  (%%vector-ref profiler 4))

(define (profiler-default-depth-set! profiler default-depth)
  (%%vector-set! profiler 4 default-depth))

(define (profiler-ignored-procedures profiler)
  (%%vector-ref profiler 5))

(define (profiler-ignored-procedures-set! profiler ignored-procedures)
  (%%vector-set! profiler 5 ignored-procedures))

(define (profiler-ignored-modules profiler)
  (%%vector-ref profiler 6))

(define (profiler-ignored-modules-set! profiler ignored-modules)
  (%%vector-set! profiler 6 ignored-modules))


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


(define (profiler-procedure-ignore? profiler)
  (lambda (procedure)
    (profiler-ignore-procedure? profiler procedure)))

(define (profiler-module-ignore? profiler)
  (lambda (module)
    (profiler-ignore-module? profiler module)))


;;;
;;;; Profile
;;;


(define (make-profile label profiler depth)
  (let ((frames-count 0)
        (frames-duration 0)
        (calls-count 0)
        (calls-duration 0)
        (start-info #f)
        (stop-info #f)
        (calls (%%make-table test: equal?)))
    (%%vector 'profile label profiler depth frames-count frames-duration calls-count calls-duration start-info stop-info calls)))


(define (profile-label profile)
  (%%vector-ref profile 1))

(define (profile-profiler profile)
  (%%vector-ref profile 2))

(define (profile-profiler-set! profile profiler)
  (%%vector-set! profile 2 profiler))

(define (profile-depth profile)
  (%%vector-ref profile 3))

(define (profile-depth-set! profile depth)
  (%%vector-set! profile 3 depth))

(define (profile-frames-count profile)
  (%%vector-ref profile 4))

(define (profile-frames-count-set! profile count)
  (%%vector-set! profile 4 count))

(define (profile-frames-duration profile)
  (%%vector-ref profile 5))

(define (profile-frames-duration-set! profile count)
  (%%vector-set! profile 5 count))

(define (profile-calls-count profile)
  (%%vector-ref profile 6))

(define (profile-calls-count-set! profile total)
  (%%vector-set! profile 6 total))

(define (profile-calls-duration profile)
  (%%vector-ref profile 7))

(define (profile-calls-duration-set! profile total)
  (%%vector-set! profile 7 total))

(define (profile-process-start-info profile)
  (%%vector-ref profile 8))

(define (profile-process-start-info-set! profile time)
  (%%vector-set! profile 8 time))

(define (profile-process-stop-info profile)
  (%%vector-ref profile 9))

(define (profile-process-stop-info-set! profile time)
  (%%vector-set! profile 9 time))

(define (profile-calls profile)
  (%%vector-ref profile 10))

(define (profile-calls-set! profile calls)
  (%%vector-set! profile 10 calls))


(define (new-profile #!key (label #f) (profiler #f) (depth #f))
  (let ((profiler (or profiler (jazz:require-service (default-profiler)))))
    (make-profile label profiler (or depth (profiler-default-depth profiler)))))


;;;
;;;; Profiles
;;;


(define *profiles*
  (%%make-table test: equal?))

(define *selected-profile*
  #f)


(define (get-profiles)
  *profiles*)

(define (get-profile-names)
  (let ((names '()))
    (jazz:table-iterate *profiles*
      (lambda (name profile)
        (set! names (%%cons name names))))
    names))


(define (get-selected-profile)
  *selected-profile*)

(define (set-selected-profile name)
  (set! *selected-profile* name))


(define (find-profile name)
  (or (%%table-ref *profiles* name #f)
      (if (%%not name)
          (let ((profile (new-profile)))
            (%%table-set! *profiles* name profile)
            profile)
        #f)))


(define (find-selected-profile)
  (find-profile (get-selected-profile)))


(define (find/new-profile label . rest)
  (or (find-profile label)
      (let ((profile (apply new-profile label: label rest)))
        (register-profile profile)
        profile)))


(define (with-profile label thunk . rest)
  (let ((profile (apply find/new-profile label rest)))
    (with-profiling profile
      thunk)))


(define (register-profile profile)
  (%%table-set! *profiles* (profile-label profile) profile))

(define (unregister-profile name)
  (%%table-clear *profiles* name))


(define (reset-profiles)
  (set! *profiles* (%%make-table test: equal?))
  (set! *selected-profile* #f))


;;;
;;;; Active
;;;


(define profiler-on?
  (%%make-parameter #f))

(define active-profile
  (%%make-parameter #f))


(define *current-profile* #f)

(define (get-current-profile)
  (if (not *current-profile*)
      (set! *current-profile* (new-profile)))
  *current-profile*)


(define (reset-profile profile profiler depth)
  (if profiler
      (profile-profiler-set! profile profiler))
  (if depth
      (profile-depth-set! profile depth))
  (profile-frames-count-set! profile 0)
  (profile-frames-duration-set! profile 0)
  (profile-calls-count-set! profile 0)
  (profile-calls-duration-set! profile 0)
  (profile-process-stop-info-set! profile #f)
  (profile-calls-set! profile (%%make-table test: equal?)))


(define (profile-register-call profile stack duration)
  (profile-calls-count-set! profile (+ (profile-calls-count profile) 1))
  (profile-calls-duration-set! profile (+ (profile-calls-duration profile) duration))
  (let* ((calls (profile-calls profile))
         (key (or stack '((<unknown> #f))))
         (call (%%table-ref calls key #f)))
    (if call
        (%%table-set! calls key
          (list (+ 1 (car call)) (+ (cadr call) duration)))
      (%%table-set! calls key (list 1 duration)))))


(define (profiler-real-time)
  (* (real-time) 1000))


(define (profiler-real-time-set! x)
  (set! profiler-real-time x))


;;;
;;;; Run
;;;


(define (process-statistics)
  (%%process-statistics))


(define (with-profiling profile thunk)
  (start-profiler profile)
  (parameterize ((active-profile profile))
    (let ((result (thunk)))
      (stop-profiler profile)
      result)))


(define (start-profiler profile)
  (profile-process-start-info-set! profile (process-statistics))
  (let ((start (profiler-start-func (profile-profiler profile))))
    (if start
        (start profile))))


(define (stop-profiler profile)
  (profile-process-stop-info-set! profile (process-statistics))
  (profile-frames-count-set! profile (+ (profile-frames-count profile) 1))
  (let ((real-time (secs->msecs
                     (- (f64vector-ref (profile-process-stop-info profile) 2)
                        (f64vector-ref (profile-process-start-info profile) 2)))))
    (profile-frames-duration-set! profile (+ (profile-frames-duration profile) real-time)))
  (let ((stop (profiler-stop-func (profile-profiler profile))))
    (if stop
        (stop profile))))


(define (profile-process-info profile)
  (let ((at-start (profile-process-start-info profile)))
    (if at-start
        (let ((at-end (or (profile-process-stop-info profile) at-start)))
          (let ((user-time (secs->msecs
                             (- (f64vector-ref at-end 0)
                                (f64vector-ref at-start 0))))
                (sys-time (secs->msecs
                            (- (f64vector-ref at-end 1)
                               (f64vector-ref at-start 1))))
                (real-time (secs->msecs
                             (- (f64vector-ref at-end 2)
                                (f64vector-ref at-start 2))))
                (gc-real-time (secs->msecs
                                (- (f64vector-ref at-end 5)
                                   (f64vector-ref at-start 5))))
                (bytes-allocated (%%flonum->exact-int
                                   (- (- (f64vector-ref at-end 7)
                                         (f64vector-ref at-start 7))
                                      (f64vector-ref at-end 9)))))
            (list user-time sys-time real-time gc-real-time bytes-allocated)))
      (list 0 0 0 0 0))))


(define (secs->msecs x)
  (inexact->exact (round (* x 1000)))))
