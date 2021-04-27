;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Internal
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


(block kernel.internal


;;;
;;;; Closure
;;;


(define (jazz:closure-environment closure)
  ;; to do interpreted
  (if (%%interp-procedure? closure)
      '()
    (let ((len (%%closure-length closure)))
      (let iter ((n 1) (env '()))
           (if (%%fx>= n len)
               env
             (iter (%%fx+ n 1) (%%cons (%%closure-ref closure n) env)))))))


;;;
;;;; Code
;;;


(define-macro (macro-code-run-fixed c rte)
  `(%%danger macro-code-run-fixed
     (let (($$code ,c))
       ((##vector-ref $$code 1) $$code ,rte))))

(define (jazz:code-run c rte)
  (macro-code-run-fixed c rte))


;;;
;;;; Compiler
;;;


(define (jazz:compiler-present?)
  (jazz:global-bound? '##gambcomp))


;;;
;;;; Devel
;;;


(define (jazz:set-gambitdir! dir)
  (jazz:check-string dir 1 (jazz:set-gambitdir! dir)
    (##set-gambitdir! dir)))


;;;
;;;; Eval
;;;


(define (jazz:allow-inner-global-define?-set! val)
  (##allow-inner-global-define?-set! val))


;; copy-pasted from gambit's repl and changed to no-windind
(define (eval-within-no-winding runner src cont repl-context receiver)
  (define (run c rte)
    (%%continuation-graft-no-winding
      cont
      (lambda ()
        (jazz:repl-context-bind
          repl-context
          (lambda ()
            (receiver
              (let ((rte rte))
                (runner
                  (lambda ()
                    (jazz:code-run c rte))))))))))
  
  (##define-macro (macro-make-rte-from-list rte lst)
    `(%%list->vector (%%cons ,rte ,lst)))
  
  (%%danger eval-within-no-winding
    (let ((src2 (%%sourcify src (%%make-source #f #f))))
      (cond ((%%interp-continuation? cont)
             (let* (($code (##interp-continuation-code cont))
                    (cte (jazz:code-cte $code))
                    (rte (##interp-continuation-rte cont)))
               (run (##compile-inner cte src2) rte)))
            ((##with-no-result-expected-toplevel-continuation? cont)
             (run (##compile-top ##interaction-cte src2) #f))
            (else
             (let* ((locals (##continuation-locals cont))
                    (cte (##cte-frame (##cte-top-cte ##interaction-cte)
                                      (##map ##car locals)))
                    (rte (macro-make-rte-from-list #f (##map ##cdr locals))))
               (run (##compile-inner cte src2) rte)))))))


(define (jazz:eval-within-no-winding runner expr cont)
  (continuation-capture
    (lambda (return)
      (eval-within-no-winding
        runner
        expr
        cont
        (jazz:current-repl-context)
        (lambda (results)
          (call-with-values
            (lambda ()
              results)
            (lambda results
              (%%continuation-return-no-winding return (%%car results)))))))))


;;;
;;;; Exception
;;;


(define (jazz:primordial-exception-handler-hook-ref)
  ##primordial-exception-handler-hook)

(define (jazz:primordial-exception-handler-hook-set! hook)
  (jazz:check-procedure hook 1 (jazz:primordial-exception-handler-hook-set! hook)
    (set! ##primordial-exception-handler-hook hook)))

(define (jazz:thread-end-with-uncaught-exception! exc)
  (##thread-end-with-uncaught-exception! exc))


(define (jazz:repl-exception-handler-hook exc other)
  (##repl-exception-handler-hook exc other))


;;;
;;;; Exit
;;;


;; make exit overridable as a temporary hack around
;; libgit2 corrupting exiting process on some windows
(define jazz:_exit
  ##exit)

(define (jazz:_exit-set! proc)
  (set! jazz:_exit proc))


(define (jazz:exit #!optional (status 0))
  (jazz:check-fixnum status 1 (jazz:exit status)
    (jazz:_exit status)))

(define (jazz:exit-no-jobs #!optional (status 1))
  (jazz:check-fixnum status 1 (jazz:exit-no-jobs status)
    (begin
      (##clear-exit-jobs!)
      (jazz:_exit status))))

(define (jazz:exit-cleanup)
  (##exit-cleanup))


;;;
;;;; File
;;;


(define jazz:os-bat-extension-string-saved
  ##os-bat-extension-string-saved)


;;;
;;;; Gambit
;;;


(define (jazz:gambitjazz?)
  (and (jazz:global-bound? '##gambitjazz?)
       (jazz:global-ref '##gambitjazz?)))


;;;
;;;; Heartbeat
;;;


(define jazz:thread-heartbeat!
  ##thread-heartbeat!)


;;;
;;;; Job
;;;


(define (jazz:exit-jobs)
  ##exit-jobs)

(define (jazz:add-exit-job! thunk)
  (jazz:check-procedure thunk 1 (jazz:add-exit-job! thunk)
    (##add-exit-job! thunk)))

(define (jazz:clear-exit-jobs!)
  (##clear-exit-jobs!))


;;;
;;;; Module
;;;


(define (jazz:load-required-module module-ref)
  (##load-required-module module-ref))

(define (jazz:load-object-file pathname quiet?)
  (jazz:check-string pathname 1 (jazz:load-object-file pathname quiet?)
    (##load-object-file pathname quiet?)))

(define (jazz:object-file-module-descrs result)
  (%%danger jazz:object-file-module-descrs
    (##vector-ref result 0)))

(define (jazz:register-module-descrs-and-load! module-descrs)
  (##register-module-descrs-and-load! module-descrs))


(define (jazz:main-set! proc)
  (jazz:check-procedure proc 1 (jazz:main-set! proc)
    (##main-set! proc)))


;;;
;;;; Port
;;;


(define jazz:stdin-port
  ##stdin-port)

(define jazz:stdout-port
  ##stdout-port)

(define jazz:stderr-port
  ##stderr-port)


;;;
;;;; Repl
;;;


(define (jazz:repl-debug-main)
  (##repl-debug-main))


;;;
;;;; Source
;;;


(define jazz:source1-marker
  ##source1-marker)

(define jazz:source2-marker
  ##source2-marker)


(define jazz:wrap-datum
  ##wrap-datum)

(define jazz:unwrap-datum
  ##unwrap-datum)


(define (jazz:wrap-datum-set! proc)
  (jazz:check-procedure proc 1 (jazz:wrap-datum-set! proc)
    (set! ##wrap-datum proc)))

(define (jazz:unwrap-datum-set! proc)
  (jazz:check-procedure proc 1 (jazz:unwrap-datum-set! proc)
    (set! ##unwrap-datum proc)))

(define (jazz:sourcify-aux1-set! proc)
  (jazz:check-procedure proc 1 (jazz:sourcify-aux1-set! proc)
    (set! ##sourcify-aux1 proc)))

(define (jazz:sourcify-aux2-set! proc)
  (jazz:check-procedure proc 1 (jazz:sourcify-aux2-set! proc)
    (set! ##sourcify-aux2 proc)))


(define (jazz:expand-source-set! proc)
  (jazz:check-procedure proc 1 (jazz:expand-source-set! proc)
    (##expand-source-set! proc)))


;;;
;;;; Stack
;;;


(define jazz:hidden-frames
  (%%list
    ##dynamic-env-bind))

(define (jazz:hidden-frame? frame)
  (%%memq frame jazz:hidden-frames))

(define (jazz:hidden-frame?-set! predicate)
  (set! jazz:hidden-frame? predicate))


(define (jazz:iterate-continuation-stack cont depth proc)
  (let iter ((d 0)
             (cont cont))
       (if (or (%%not depth) (%%fx< d depth))
           (and cont
                (begin
                  (proc cont)
                  (iter (%%fx+ d 1)
                        (%%continuation-next-frame cont #f)))))))


(define (jazz:iterate-var-val var val-or-box cte proc)
  (%%danger jazz:iterate-var-val
    (cond ((##var-i? var)
           (jazz:iterate-var-val-aux (##var-i-name var)
                                     val-or-box
                                     #t
                                     cte
                                     proc))
          ((##var-c-boxed? var)
           (jazz:iterate-var-val-aux (##var-c-name var)
                                     (%%unbox val-or-box)
                                     #t
                                     cte
                                     proc))
          (else
           (jazz:iterate-var-val-aux (##var-c-name var)
                                     val-or-box
                                     #f
                                     cte
                                     proc)))))


(define (jazz:iterate-var-val-aux var val mutable? cte proc)
  (define (remove-quote val)
    (if (and (pair? val)
             (eq? (%%car val) 'quote)
             (%%not (%%null? (%%cdr val))))
        (%%cadr val)
      val))
  
  (%%danger jazz:iterate-var-val-aux
    (proc
      (%%list (%%object->string var)
              (cond ((jazz:absent-object? val)
                     '<absent>)
                    ((jazz:unbound-object? val)
                     '<unbound>)
                    ((%%procedure? val)
                     (remove-quote
                       (if (##cte-top? cte)
                           (##inverse-eval-in-env val cte)
                         (##inverse-eval-in-env val (##cte-parent-cte cte)))))
                    (else
                     val))
              mutable?))))


(define (jazz:iterate-continuation-dynamic-environment cont reference-name proc)
  (define (iterate-parameters lst cte proc)
    (let iter ((lst lst))
         (if (%%pair? lst)
             (let* ((param-val (%%car lst))
                    (param (%%car param-val))
                    (val (%%cdr param-val)))
               (if (%%not (##hidden-parameter? param))
                   (let ((name (reference-name (##inverse-eval-in-env param cte))))
                     (jazz:iterate-var-val-aux (%%list name) val #t cte proc)))
               (iter (%%cdr lst))))))
  
  (%%danger jazz:iterate-continuation-dynamic-environment
    (and cont
         (iterate-parameters
           (##dynamic-env->list (jazz:continuation-denv cont))
           (if (%%interp-continuation? cont)
               (let (($code (##interp-continuation-code cont)))
                 (jazz:code-cte $code))
             ##interaction-cte)
           proc))))


(define (jazz:iterate-continuation-lexical-environment cont proc)
  (define (iterate-rte cte rte proc)
    (let loop1 ((c cte)
                (r rte))
         (cond ((##cte-top? c))
               ((##cte-frame? c)
                (let loop2 ((vars (##cte-frame-vars c))
                            (vals (%%cdr (%%vector->list r))))
                     (if (%%pair? vars)
                         (let ((var (%%car vars)))
                           (if (%%not (##hidden-local-var? var))
                               (jazz:iterate-var-val var (%%car vals) c proc))
                           (loop2 (%%cdr vars)
                                  (%%cdr vals)))
                       (loop1 (##cte-parent-cte c)
                              (jazz:rte-up r)))))
               (else
                (loop1 (##cte-parent-cte c)
                       r)))))
  
  (define (iterate-vars lst cte proc)
    (let iter ((lst lst))
         (if (%%pair? lst)
             (let* ((var-val (%%car lst))
                    (var (%%car var-val))
                    (val (%%cdr var-val)))
               (jazz:iterate-var-val var val cte proc)
               (iter (%%cdr lst))))))
  
  (define (iterate-locals lst cte proc)
    (and lst
         (iterate-vars lst cte proc)))
  
  (%%danger jazz:iterate-continuation-lexical-environment
    (and cont
         (if (%%interp-continuation? cont)
             (let (($code (##interp-continuation-code cont))
                   (rte (##interp-continuation-rte cont)))
               (iterate-rte (jazz:code-cte $code) rte proc)
               (jazz:code-cte $code))
           (begin
             (iterate-locals (%%continuation-locals cont) ##interaction-cte proc)
             ##interaction-cte)))))


;;;
;;;; Step
;;;


(define (jazz:install-step-handler proc)
  (define (handler leapable? $code rte execute-body . other)
    (##step-off)
    (jazz:process-step
      proc
      $code
      (lambda ()
        (%%apply execute-body (%%cons $code (%%cons rte other))))))
  
  (%%danger jazz:install-step-handler
    (let ((cs (##current-stepper)))
      (vector-set! cs 0 (vector handler handler handler handler handler handler handler))
      (void))))


(define (jazz:process-step proc $code execute)
  (%%danger jazz:process-step
    (proc
      (##code-locat $code)
      (lambda (cmd)
        (case cmd
          ((step)
           (##step-on)
           (execute))
          ((leap) ;; does this really work????
           (let ((result (execute)))
             (##step-on)
             result))
          ((continue)
           (execute)))))))


;;;
;;;; Symbol
;;;


(define (jazz:set-gensym-counter! val)
  (jazz:check-fixnum val 1 (jazz:set-gensym-counter! val)
    (set! ##gensym-counter val)))


;;;
;;;; Thread With Stack
;;;


(define jazz:word-size
  (let ((word-size #f))
    (lambda ()
      (or word-size
          (let ((size
                  (%%danger jazz:word-size
                    (%%u8vector-length '#(0)))))
            (set! word-size size)
            size)))))


(define (jazz:header-get obj)
  (%%danger jazz:header-get
    (if (= (jazz:word-size) 4)
        (%%u32vector-ref obj -1)
      (%%u64vector-ref obj -1))))

(define (jazz:header-set! obj h)
  (%%danger jazz:header-set!
    (if (= (jazz:word-size) 4)
        (%%u32vector-set! obj -1 h)
      (%%u64vector-set! obj -1 h))))


(define (jazz:make-thread-with-stack stack-len thunk)
  (%%danger jazz:make-thread-with-stack
    (let* ((pt ##primordial-thread)
           (thread-len (%%vector-length pt))
           (total-len (+ thread-len stack-len))
           (thread (make-vector total-len #f)))
      (%%vector-set! thread 0 (%%vector-ref pt 0))
      (jazz:header-set! thread
                        (+ (bitwise-and (jazz:header-get pt) -4)
                           (bitwise-and (jazz:header-get thread) 3)))
      (thread-init! thread thunk)
      (thread-specific-set! thread (+ thread-len 1))
      thread)))


;; work around PERM not being mutable
(define (jazz:thread-local-set! vec n value)
  (%%danger jazz:thread-local-set!
    (%%f64vector-set! vec n value)))


;;;
;;;; Timeout
;;;


;; copied from _repl
(define (jazz:write-timeout to moment port)
  (%%write-string " " port)
  (let* ((expiry (%%fl- to moment))
         (e (%%fl/ (%%flround (%%fl* 10.0 expiry)) 10.0)))
    (%%write (if (%%integer? e) (%%inexact->exact e) e) port))
  (%%write-string "s" port))


;;;
;;;; Unspecified
;;;


(define jazz:Unspecified-Value
  (void))


(jazz:define-macro (%%unspecified)
  'jazz:Unspecified-Value)


(jazz:define-macro (%%unspecified? value)
  `(%%eq? ,value jazz:Unspecified-Value))


(jazz:define-macro (%%specified? value)
  `(%%neq? ,value jazz:Unspecified-Value))


;;;
;;;; Values
;;;


(jazz:define-macro (%%values-ref values n)
  (if jazz:debug-core?
      (%%force-uniqueness (values n)
        `(%%check-values ,values 1 (%%values-ref ,values ,n)
           (if (%%fx< ,n (^#vector-length ,values))
               (^#vector-ref ,values ,n)
             (error "Out of bounds"))))
    `(%%vector-ref ,values ,n)))

(jazz:define-macro (%%values-set! values n obj)
  (if jazz:debug-core?
      (%%force-uniqueness (values n)
        `(%%check-values ,values 1 (%%values-set! ,values ,n ,obj)
           (if (%%fx< ,n (^#vector-length ,values))
               (^#vector-ref ,values ,n ,obj)
             (error "Out of bounds"))))
    `(%%vector-set! ,values ,n ,obj))))
