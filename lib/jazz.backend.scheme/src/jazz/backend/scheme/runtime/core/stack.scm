;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Stack
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
;;;  Portions created by the Initial Developer are Copyright (C) 1996-2012
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


(unit protected jazz.backend.scheme.runtime.core.stack


;;;
;;;; Gambit
;;;


(cond-expand
  (gambit
    (define (jazz:procedure-name procedure)
      (%%procedure-name procedure))
    
    (define (jazz:procedure-name-set! proc)
      (set! jazz:procedure-name proc))
    
    (define (jazz:procedure-locat procedure)
      (%%procedure-locat procedure))
    
    
    (define (jazz:closure? obj)
      (%%closure? obj))
    
    (define (jazz:closure-code closure)
      (%%closure-code closure))
    
    (define (jazz:closure-length closure)
      (%%closure-length closure))
    
    (define (jazz:closure-ref closure n)
      (%%closure-ref closure n))
    
    
    (define jazz:hidden-frames
      (%%list
        ##dynamic-env-bind
        ##thread-start-action!))
    
    (define (jazz:hidden-frame? frame)
      (%%memq frame jazz:hidden-frames))
    
    (define (jazz:hidden-frame?-set! predicate)
      (set! jazz:hidden-frame? predicate))
    
    
    (define (jazz:get-continuation-stack cont depth)
      (let ((queue (jazz:new-queue)))
        (let iter ((d 0)
                   (cont cont))
             (if (or (%%not depth) (%%fx< d depth))
                 (and cont
                      (begin
                        (jazz:enqueue queue cont)
                        (iter (%%fx+ d 1)
                              (%%continuation-next-frame cont #f))))))
        (jazz:queue-list queue)))
    
    
    (define (jazz:collect-var-val var val-or-box cte queue)
      (cond ((##var-i? var)
             (jazz:collect-var-val-aux (##var-i-name var)
                                       val-or-box
                                       #t
                                       cte
                                       queue))
            ((##var-c-boxed? var)
             (jazz:collect-var-val-aux (##var-c-name var)
                                       (##unbox val-or-box)
                                       #t
                                       cte
                                       queue))
            (else
             (jazz:collect-var-val-aux (##var-c-name var)
                                       val-or-box
                                       #f
                                       cte
                                       queue))))
    
    
    (define (jazz:collect-var-val-aux var val mutable? cte queue)
      
      (define (remove-quote val)
        (if (and (pair? val)
                 (eq? (##car val) 'quote)
                 (##not (##null? (##cdr val))))
            (##cadr val)
          val))
      
      (jazz:enqueue queue
                    (%%list (##object->string var)
                            (cond ((jazz:absent-object? val)
                                   '<absent>)
                                  ((jazz:unbound-object? val)
                                   '<unbound>)
                                  ((##procedure? val)
                                   (remove-quote
                                     (if (##cte-top? cte)
                                         (##inverse-eval-in-env val cte)
                                       (##inverse-eval-in-env val (##cte-parent-cte cte)))))
                                  (else
                                   val))
                            mutable?)))
    
    
    (define (jazz:get-continuation-dynamic-environment cont)
      
      (define (collect-parameters lst cte queue)
        (let iter ((lst lst))
             (if (%%pair? lst)
                 (let* ((param-val (%%car lst))
                        (param (%%car param-val))
                        (val (%%cdr param-val)))
                   (if (%%not (##hidden-parameter? param))
                       (let ((name (jazz:reference-name (##inverse-eval-in-env param cte))))
                         (jazz:collect-var-val-aux (%%list name) val #t cte queue)))
                   (iter (%%cdr lst))))))
      
      (let ((queue (jazz:new-queue)))
        (and cont
             (collect-parameters
               (##dynamic-env->list (jazz:continuation-denv cont))
               (if (%%interp-continuation? cont)
                   (let (($code (##interp-continuation-code cont)))
                     (jazz:code-cte $code))
                 ##interaction-cte)
               queue))
        (jazz:queue-list queue)))
    
    
    (define (jazz:get-continuation-lexical-environment cont)
      
      (define (collect-rte cte rte queue)
        (let loop1 ((c cte)
                    (r rte))
             (cond ((##cte-top? c))
               ((##cte-frame? c)
                (let loop2 ((vars (##cte-frame-vars c))
                            (vals (%%cdr (%%vector->list r))))
                     (if (%%pair? vars)
                         (let ((var (%%car vars)))
                           (if (%%not (##hidden-local-var? var))
                               (jazz:collect-var-val var (%%car vals) c queue))
                           (loop2 (%%cdr vars)
                                  (%%cdr vals)))
                       (loop1 (##cte-parent-cte c)
                              (jazz:rte-up r)))))
               (else
                (loop1 (##cte-parent-cte c)
                       r)))))
      
      (define (collect-vars lst cte queue)
        (let iter ((lst lst))
             (if (%%pair? lst)
                 (let* ((var-val (%%car lst))
                        (var (%%car var-val))
                        (val (%%cdr var-val)))
                   (jazz:collect-var-val var val cte queue)
                   (iter (%%cdr lst))))))
      
      (define (collect-locals lst cte queue)
        (and lst
             (collect-vars lst cte queue)))
      
      (let ((queue (jazz:new-queue)))
        (and cont
             (if (%%interp-continuation? cont)
                 (let (($code (##interp-continuation-code cont))
                       (rte (##interp-continuation-rte cont)))
                   (collect-rte (jazz:code-cte $code) rte queue)
                   (jazz:code-cte $code))
               (begin
                 (collect-locals (%%continuation-locals cont) ##interaction-cte queue)
                 ##interaction-cte)))
        (jazz:queue-list queue)))
    
    
    (define (jazz:get-continuation-location cont)
      (jazz:locat->container/line/col (%%continuation-locat cont)))
    
    
    (define (jazz:interpreted-continuation? cont)
      (%%interp-continuation? cont))
    
    
    (define (jazz:with-repl-context cont thunk)
      (let ((prev-context (%%thread-repl-context-get!)))
        (let ((context
                (jazz:make-repl-context
                  (%%fx+ (jazz:repl-context-level prev-context) 1)
                  0
                  cont
                  cont
                  #f
                  prev-context
                  #f)))
          (jazz:repl-context-bind
            context
            thunk))))
    
    
    (define (jazz:repl #!optional (write-reason #f))
      (begin
        (%%repl write-reason)
        #f))
    
    
    (define (jazz:repl-debug #!optional (write-reason #f) (toplevel? #f))
      (begin
        (%%repl-debug write-reason toplevel?)
        #f))
    
    
    ;;;
    ;;;; Eval
    ;;;
    
    
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
        `(##list->vector (##cons ,rte ,lst)))
      
      (let ((src2 (##sourcify src (##make-source #f #f))))
        (cond ((##interp-continuation? cont)
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
                 (run (##compile-inner cte src2) rte))))))
    
    
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
    
    
    (define (jazz:repl-result-history-add result)
      (let ((channel (%%thread-repl-channel-get! (%%current-thread))))
        (%%repl-channel-result-history-add channel result)))
    
    
    ;;;
    ;;;; Debugging
    ;;;
    
    
    (define (jazz:inspect-repl-context context)
      `(:repl-context
        ,(jazz:repl-context-level context)
        ,(jazz:repl-context-depth context)
        ,(jazz:repl-context-cont context)
        ,(jazz:repl-context-initial-cont context)
        ,(jazz:repl-context-prev-level context)
        ,(jazz:repl-context-prev-depth context))))
  
  (else)))
