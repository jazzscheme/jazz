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


(jazz:define-setting default-profiler
  'statprof)

(jazz:define-setting default-profiler-ignored-procedures
  (%%list
    ##dynamic-env-bind
    ##call-with-values
    ##thread-start-action!
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


;;;
;;;; Profile
;;;


(define (make-profile label profiler depth)
  (%%vector 'profile label profiler depth 0 0 #f '()))


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

(define (profile-calls-count profile)
  (%%vector-ref profile 4))

(define (profile-calls-count-set! profile total)
  (%%vector-set! profile 4 total))

(define (profile-calls-duration profile)
  (%%vector-ref profile 5))

(define (profile-calls-duration-set! profile total)
  (%%vector-set! profile 5 total))

(define (profile-process-info profile)
  (%%vector-ref profile 6))

(define (profile-process-info-set! profile time)
  (%%vector-set! profile 6 time))

(define (profile-calls profile)
  (%%vector-ref profile 7))

(define (profile-calls-set! profile calls)
  (%%vector-set! profile 7 calls))


(define (new-profile #!key (label #f) (profiler #f) (depth #f))
  (let ((profiler (or profiler (jazz:require-service (default-profiler)))))
   (make-profile label profiler (or depth (profiler-default-depth profiler)))))


;;;
;;;; Profiles
;;;


(define *profiles*
  (make-table test: equal?))

(define *selected-profile*
  #f)


(define (get-profiles)
  *profiles*)

(define (get-profile-names)
  (let ((names '()))
    (jazz:iterate-table *profiles*
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


(define (register-profile profile)
  (%%table-set! *profiles* (profile-label profile) profile))

(define (unregister-profile name)
  (%%table-clear *profiles* name))


;;;
;;;; Active
;;;


(define profiler-on?
  (make-parameter #f))

(define active-profile
  (make-parameter #f))

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
  (profile-calls-count-set! profile 0)
  (profile-calls-duration-set! profile 0)
  (profile-calls-set! profile '()))


(define (profile-register-call profile stack duration)
  (profile-calls-count-set! profile (+ (profile-calls-count profile) 1))
  (profile-calls-duration-set! profile (+ (profile-calls-duration profile) duration))
  (profile-calls-set! profile
                      (cons (list (or stack '((<unknown> #f))) duration) (profile-calls profile))))


(define (profiler-real-time)
  (* (real-time) 1000))


(define (profiler-real-time-set! x)
  (set! profiler-real-time x))


;;;
;;;; Run
;;;


(define (with-profiling profile thunk)
  (start-profiler profile)
  (parameterize ((active-profile profile))
    (let ((result (thunk)))
      (stop-profiler profile)
      result)))


(define (start-profiler profile)
  (profile-process-info-set! profile (##process-statistics))
  (let ((start (profiler-start-func (profile-profiler profile))))
    (if start
        (start profile))))


(define (stop-profiler profile)
  (define (secs->msecs x)
          (##inexact->exact (##round (##* x 1000))))

  (if (profile-process-info profile)
      (let* ((at-start (profile-process-info profile))
             (at-end (##process-statistics))
             (user-time
              (secs->msecs
               (##- (##f64vector-ref at-end 0)
                    (##f64vector-ref at-start 0))))
             (sys-time
              (secs->msecs
               (##- (##f64vector-ref at-end 1)
                    (##f64vector-ref at-start 1))))
             (gc-real-time
              (secs->msecs
               (##- (##f64vector-ref at-end 5)
                    (##f64vector-ref at-start 5))))
             (bytes-allocated
              (##flonum.->exact-int
               (##- (##- (##f64vector-ref at-end 7)
                         (##f64vector-ref at-start 7))
                    (##f64vector-ref at-end 9)))))
        (profile-process-info-set! profile (list user-time sys-time gc-real-time bytes-allocated))))
  (let ((stop (profiler-stop-func (profile-profiler profile))))
    (if stop
        (stop profile))))


;;;
;;;; Stack
;;;


(define (get-cont-stack-for-profile cont depth profiler)
  (define (continuation-creator cont)
    (let ((proc (%%continuation-creator cont)))
      (if (and proc (%%closure? proc))
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
    (let ((creator-name (jazz:procedure-name creator)))
      (let loop ((current-cont (%%continuation-next cont)))
           (if current-cont
               (let ((current-creator (continuation-creator current-cont)))
                 (if (%%eq? creator-name (jazz:procedure-name current-creator))
                     (loop (%%continuation-next current-cont))
                   current-cont))
             #f))))
  
  (define (identify-location locat)
    ;; copy of jazz:locat->file/line/col
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
