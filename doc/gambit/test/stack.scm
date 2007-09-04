;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Stack Manipulation
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


(include "~~/src/lib/header.scm")


(define (new-queue)
  (box '()))


(define (enqueue queue obj)
  (set-box! queue (cons obj (unbox queue))))


(define (queue-list queue)
  (reverse (unbox queue)))


(define (get-continuation-stack cont)
  (let ((queue (new-queue)))
    (let loop ((depth 0)
               (cont cont))
      (and cont
           (begin
             (enqueue queue (get-frame cont))
             (loop (+ depth 1)
                   (##continuation-next cont)))))
    (queue-list queue)))


(define (get-frame cont)
  (cons (get-frame-name cont)
        (get-frame-variables cont)))


(define (get-frame-name cont)
  (let ((creator (##continuation-creator cont)))
    (if creator
        (##procedure-name creator)
      "(interaction)")))


(define (get-frame-variables cont)
  
  (define var-width
    100)

  (define (collect-var-val var val cte queue)
    (enqueue queue
      (cons (##object->string var)
            (##object->string
             (if (##cte-top? cte)
                 (##inverse-eval-in-env val cte)
               (##inverse-eval-in-env val (##cte-parent-cte cte)))
             var-width))))

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
                         (collect-var-val var (##car vals) c queue))
                     (loop2 (##cdr vars)
                      (##cdr vals)))
                 (loop1 (##cte-parent-cte c)
                   (macro-rte-up r)))))
            (else
             (loop1 (##cte-parent-cte c)
                    r)))))

  (define (collect-vars lst cte queue)
    (let loop ((lst lst))
      (if (##pair? lst)
          (let* ((var-val (##car lst))
                 (var (##car var-val))
                 (val (##cdr var-val)))
            (collect-var-val var val cte queue)
            (loop (##cdr lst))))))

  (define (collect-locals lst cte queue)
    (and lst
         (collect-vars lst cte queue)))

  (define (collect-parameters lst cte queue)
    (let loop ((lst lst))
      (if (##pair? lst)
          (let* ((param-val (##car lst))
                 (param (##car param-val))
                 (val (##cdr param-val)))
            (if (##not (##hidden-parameter? param))
                (let ((x
                       (##inverse-eval-in-env param cte)))
                  (collect-var-val (##list x) val cte queue)))
            (loop (##cdr lst))))))

  (let ((queue (new-queue)))
    (and cont
         (collect-parameters
           (##dynamic-env->list (macro-continuation-denv cont))
           (if (##interp-continuation? cont)
               (let (($code (##interp-continuation-code cont))
                     (rte (##interp-continuation-rte cont)))
                 (collect-rte (macro-code-cte $code) rte queue)
                 (macro-code-cte $code))
             (begin
               (collect-locals (##continuation-locals cont) ##interaction-cte queue)
               ##interaction-cte))
           queue))
    (queue-list queue)))
