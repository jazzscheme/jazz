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


(module jazz.dialect.core.stack


;;;
;;;; Gambit
;;;


(cond-expand
  (gambit
    (include "~~/lib/_gambit#.scm")
    
    
    (define (jazz.continuation-capture proc)
      (##continuation-capture proc))
    
    
    (define (jazz.get-continuation-stack cont depth)
      (let ((queue (jazz.new-queue)))
        (let iter ((d 0)
                   (cont cont))
          (if (or (not depth) (< d depth))
              (and cont
                   (begin
                     (jazz.enqueue queue (jazz.get-frame cont))
                     (iter (+ d 1)
                           (##continuation-next-frame cont #f))))))
        (jazz.queue-list queue)))
    
    
    (define (jazz.get-frame cont)
      (list (jazz.get-frame-name cont)
            (jazz.get-frame-environment cont)
            (jazz.get-frame-variables cont)))
    
    
    (define (jazz.get-frame-name cont)
      (let ((creator (##continuation-creator cont)))
        (if creator
            (##procedure-name creator)
          "(interaction)")))
    
    
    (define (jazz.collect-var-val var val cte queue)
      (jazz.enqueue queue
                    (cons (##object->string var)
                          (if (##cte-top? cte)
                              (##inverse-eval-in-env val cte)
                            (##inverse-eval-in-env val (##cte-parent-cte cte))))))
    
    
    (define (jazz.get-frame-environment cont)
      
      (define (collect-parameters lst cte queue)
        (let iter ((lst lst))
             (if (##pair? lst)
                 (let* ((param-val (##car lst))
                        (param (##car param-val))
                        (val (##cdr param-val)))
                   (if (##not (##hidden-parameter? param))
                       (let ((x
                               (##inverse-eval-in-env param cte)))
                         (jazz.collect-var-val (##list x) val cte queue)))
                   (iter (##cdr lst))))))
      
      (let ((queue (jazz.new-queue)))
        (and cont
             (collect-parameters
               (##dynamic-env->list (macro-continuation-denv cont))
               (if (##interp-continuation? cont)
                   (let (($code (##interp-continuation-code cont))
                         (rte (##interp-continuation-rte cont)))
                     (macro-code-cte $code))
                 ##interaction-cte)
               queue))
        (jazz.queue-list queue)))
    
    
    (define (jazz.get-frame-variables cont)
      
      (define (collect-rte cte rte queue)
        (let loop1 ((c cte)
                    (r rte))
             (cond ((##cte-top? c))
               ((##cte-frame? c)
                (let loop2 ((vars (##cte-frame-vars c))
                            (vals (##cdr (##vector->list r))))
                     (if (##pair? vars)
                         (let ((var (##car vars)))
                           (if (##not (##hidden-local-var? var))
                               (jazz.collect-var-val var (##car vals) c queue))
                           (loop2 (##cdr vars)
                                  (##cdr vals)))
                       (loop1 (##cte-parent-cte c)
                              (macro-rte-up r)))))
               (else
                (loop1 (##cte-parent-cte c)
                       r)))))
      
      (define (collect-vars lst cte queue)
        (let iter ((lst lst))
             (if (##pair? lst)
                 (let* ((var-val (##car lst))
                        (var (##car var-val))
                        (val (##cdr var-val)))
                   (jazz.collect-var-val var val cte queue)
                   (iter (##cdr lst))))))
      
      (define (collect-locals lst cte queue)
        (and lst
             (collect-vars lst cte queue)))
      
      (let ((queue (jazz.new-queue)))
        (and cont
             (if (##interp-continuation? cont)
                 (let (($code (##interp-continuation-code cont))
                       (rte (##interp-continuation-rte cont)))
                   (collect-rte (macro-code-cte $code) rte queue)
                   (macro-code-cte $code))
               (begin
                 (collect-locals (##continuation-locals cont) ##interaction-cte queue)
                 ##interaction-cte)))
        (jazz.queue-list queue)))
    
    
    (define (jazz.current-repl-context)
      (macro-current-repl-context))
    
    
    (define (jazz.repl-context-level context)
      (macro-repl-context-level context))
    
    (define (jazz.repl-context-depth context)
      (macro-repl-context-depth context))
    
    (define (jazz.repl-context-cont context)
      (macro-repl-context-cont context))
    
    (define (jazz.repl-context-initial-cont context)
      (macro-repl-context-initial-cont context))
        
    (define (jazz.repl-context-prev-level context)
      (macro-repl-context-prev-level context))
    
    (define (jazz.repl-context-prev-depth context)
      (macro-repl-context-prev-depth context))

    
    (define (jazz.with-repl-context cont thunk)
      (let ((prev-context (##thread-repl-context-get!)))
        (let ((context
                (macro-make-repl-context
                  (##fixnum.+ (macro-repl-context-level prev-context) 1)
                  0
                  cont
                  cont
                  prev-context
                  #f)))
          (macro-dynamic-bind repl-context
            context
            thunk))))
    
    
    (define (jazz.repl)
      (begin
        (##repl)
        #f))
    
    
    ;;;
    ;;;; Debugging
    ;;;
    
    
    (define (jazz.inspect-repl-context context)
      `(:repl-context
        ,(jazz.repl-context-level context)
        ,(jazz.repl-context-depth context)
        ,(jazz.repl-context-cont context)
        ,(jazz.repl-context-initial-cont context)
        ,(jazz.repl-context-prev-level context)
        ,(jazz.repl-context-prev-depth context))))
  
  (else)))
