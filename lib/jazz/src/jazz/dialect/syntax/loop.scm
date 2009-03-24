;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Loop
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


(library protected jazz.dialect.syntax.loop scheme


(import (jazz.dialect.kernel.boot)
        (jazz.dialect.syntax.either (phase syntax))
        (jazz.dialect.syntax.increase (phase syntax))
        (jazz.dialect.syntax.bind (phase syntax))
        (jazz.dialect.syntax.macros (phase syntax)))


;;;
;;;; Samples
;;;


#; ;; @syntax
(loop (repeat n)
      (do (bell)))

#; ;; @expansion
(let ((rpt0 n))
  (while (and (> rpt0 0))
    (bell)
    (decrease! rpt0)))


#; ;; @syntax
(loop (for x from 0 below (upper-bound))
      (for y in (get-list))
      (sum (* x y))
      (do (debug x))
      (finally (message-box "Done")))

#; ;; @expansion
(let ((x <fx> 0)
      (end0 <fx> (upper-bound))
      (for1 <Object> (get-list))
      (y <Object> #f)
      (res2 <fx> 0))
  (while (and (< x end0) (not (eq? for1 #f)))
    (set! y (car for1))
    (increase! res2 (* x y))
    (debug x)
    (finally (message-box "Done"))
    (increase! x 1)
    (set! for1 (cdr for1)))
  res2)


#; ;; @syntax
(loop (for x in list)
      (some (test? x)))

#; ;; @expansion
(let ((for0 <Object> list)
      (x <Object>)
      (res1 <bool> #f))
  (while (and (not (eq? for0 #f)))
    (set! x (car for0))
    (when (test? x)
      (set! res1 #t))
    (set! for0 (cdr for0)))
  res1)


#; ;; @syntax
(loop (for n in lst)
      (when (even? n)
        (return n))
      (debug n))

#; ;; @expansion
(let ((for0 <Object> lst)
      (n <Object>)
      (ext1 <bool> #f))
  (while (and (not ext1) (not (eq? for0 #f)))
    (set! n (car for0))
    (set! for0 (cdr for0))
    (when (even? n)
      (set! ext1 #t))
    (when (not ext1)
      (debug n)))
  n)


#; ;; @syntax
(loop (for n in (naturals 0 10))
      (when (even? n)
        (collect (* n n) in even))
      (when (odd? n)
        (sum (+ n n) in odd))
      (finally (list even odd)))


(syntax (loop form-src)
  (let ((clauses (cdr (source-code form-src))))
    (sourcify-if
      (expand-loop clauses)
      form-src)))


(define noobject
  (list 'noobject))


(define (expand-loop clauses)
  (let ((bindings    '())
        (return      noobject)
        (exit        noobject)
        (tests       '())
        (withs       '())
        (befores     '())
        (actions     '())
        (afters      '())
        (epilogue    '())
        (finally     '())
        (unique-rank 0))
    
  
  ;;;
  ;;;; Expand
  ;;;


  (define (expand clauses)
    (set! actions (process-clauses clauses))
    (expand-loop))
    
  
  ;;;
  ;;;; Clauses
  ;;;

  
  (define (process-clauses clauses)
    (let ((actions (new-queue)))
      (while (not-null? clauses)
        (let ((clause (car clauses)))
          (if (not (pair? (source-code clause)))
              (add-action clause actions)
            (bind (key . rest) (source-code clause)
              (case (source-code key)
                ((with)    (process-with    actions rest))
                ((for)     (process-for     actions rest))
                ((repeat)  (process-repeat  actions rest))
                ((some)    (process-some    actions rest))
                ((every)   (process-every   actions rest))
                ((when)    (process-when    actions rest))
                ((unless)  (process-unless  actions rest))
                ((do)      (process-do      actions rest))
                ((sum)     (process-sum     actions rest))
                ((collect) (process-collect actions rest))
                ((return)  (process-return  actions rest))
                ((finally) (process-finally actions rest))
                (else    (add-action clause actions))))))
        (set! clauses (cdr clauses)))
      (queue-list actions)))


  (define (expand-loop)
    `(let* ,bindings
       (while (and ,@tests)
         ,@befores
         (let* ,withs
           ,@actions)
         ,@afters)
       ,@epilogue
       ,@(if (eq? return noobject)
             finally
           `((if ,exit
                 ,return
               (begin
                 ,@finally))))))
  
  
  (define (unique prefix)
    (let ((symbol (string->symbol (string-append prefix (->string unique-rank)))))
      (increase! unique-rank)
      symbol))
  
  
  (define Unbound
    (cons #f #f))
  
  
  (define (add-binding variable type . rest)
    (let ((value (if (null? rest) Unbound (car rest))))
      (let ((binding (cons variable (cons type (if (eq? value Unbound) (list #f) (list value))))))
        (set! bindings (append bindings (list binding))))
      variable))
  
  
  (define (add-with with)
    (set! withs (append withs (list with))))
  
  
  (define (get-return/exit)
    (when (eq? return noobject)
      (let ((ret (unique "ret"))
            (ext (unique "ext")))
        (add-binding ret '<Object+> #f)
        (add-binding ext '<bool> '#f)
        (add-initial-test (list 'not ext))
        (set! return ret)
        (set! exit ext)))
    (values return exit))
  
  
  (define (exit-safe actions)
    (if (eq? exit noobject)
        actions
      `((when (not ,exit)
          ,@actions))))
  
  
  (define (add-test test)
    (set! tests (append tests (list test))))
  
  
  (define (add-initial-test test)
    (set! tests (cons test tests)))
  
  
  (define (add-before before)
    (set! befores (append befores (exit-safe (list before)))))
  
  
  (define (add-action action actions)
    (add-actions (list action) actions))
  
  
  (define (add-actions action-list actions)
    (enqueue-list actions (exit-safe action-list)))
  
  
  (define (add-after after)
    (set! afters (append afters (exit-safe (list after)))))
  
  
  (define (add-epilogue expr)
    (set! epilogue (append epilogue (list expr))))
  
  
  (define (set-finally lst)
    (set! finally lst))
  
  
  ;;;
  ;;;; with
  ;;;


  (define (process-with actions rest)
    (add-with rest))
  
  
  ;;;
  ;;;; for
  ;;;


  (define (process-for actions rest)
    (receive (variable type key rest) (parse-for rest)
      (case (source-code key)
        ((in)
         (bind (lst . rest) rest
           (let ((for (unique "for")))
             (add-binding for '<Object> lst)
             (add-binding variable (either type '<Object>))
             (add-test (list 'not (list 'null? for)))
             (add-before (list 'set! variable (list 'car for)))
             (add-before (list 'set! for (list 'cdr for)))
             (when (not-null? rest)
               (bind (keyword value) rest
                 (case (source-code keyword)
                   ((remainder)
                    (add-binding value '<Object+>)
                    (add-before (list 'set! value for)))
                   (else (error "Unknown for in keyword: {t}" keyword))))))))
        ((in-sequence)
         (bind (iterator) rest
           (let ((val (unique "val"))
                 (itr (unique "itr")))
             (add-binding val '<Object> iterator)
             (add-binding itr '<Iterator> (list 'if (list 'is? val 'Iterator) val (list 'iterate-sequence val)))
             (add-binding variable '<Object> #f)
             (add-test (list 'not (list 'done?~ itr)))
             (add-before (list 'set! variable (list 'get-next~ itr))))))
        ((in-vector)
         (bind (vector . rest) rest
           (let ((vec (unique "vec"))
                 (for (unique "for"))
                 (len (unique "len")))
             (add-binding vec '<vector> vector)
             (add-binding for '<fx> 0)
             (add-binding len '<fx> (list 'length vec))
             (add-binding variable (either type '<Object>))
             (add-test (list '< for len))
             (add-before (list 'set! variable (list 'element vec for)))
             (add-before (list 'set! for (list '+ for 1))))))
        ((in-properties)
         (bind (keyword value) (source-code variable)
           (bind (lst) rest
             (let ((for (unique "for")))
               (add-binding for '<Object> lst)
               (add-binding keyword '<Object> #f)
               (add-binding value '<Object> #f)
               (add-test (list 'not (list 'null? for)))
               (add-before (list 'set! keyword (list 'car for)))
               (add-before (list 'set! for (list 'cdr for)))
               (add-before (list 'set! value (list 'car for)))
               (add-before (list 'set! for (list 'cdr for)))))))
        ((from)
         (bind (from . rest) rest
           (let ((to #f)
                 (test #f)
                 (update 'increase!)
                 (by 1)
                 (scan rest))
             (while (not-null? scan)
               (let ((key (source-code (car scan))))
                 (case key
                   ((to) (set! to (cadr scan)) (set! test '<=) (set! scan (cddr scan)))
                   ((below) (set! to (cadr scan)) (set! test '<) (set! scan (cddr scan)))
                   ((downto) (set! to (cadr scan)) (set! test '>=) (set! update 'decrease!) (set! scan (cddr scan)))
                   ((by) (set! by (cadr scan)) (set! scan (cddr scan)))
                   (else (error "Unknown for keyword: {t}" key)))))
             (add-binding variable '<fx> from)
             (when to
               (let ((end (if (symbol? to) to (unique "end"))))
                 (when (not (eq? end to))
                   (add-binding end '<fx> to))
                 (add-test (list test variable end))))
             (add-after (list update variable by)))))
        ((first)
         (bind (first . rest) rest
           (add-binding variable '<Object> first)
           (when (not-null? rest)
             (bind (then-key then) rest
               (add-after (list 'set! variable then))))))
        (else
         (error "Unknown for keyword: {t}" (source-code key))))))
  
  
  (define (parse-for rest)
    (bind (variable . rest) rest
      (if (specifier? (source-code (car rest)))
          (bind (type key . rest) rest
            (values variable type key rest))
        (bind (key . rest) rest
          (values variable #f key rest)))))
  
  
  ;;;
  ;;;; repeat
  ;;;


  (define (process-repeat actions rest)
    (bind (count) rest
      (let ((rpt (unique "rpt")))
        (add-binding rpt '<fx> count)
        (add-test (list '> rpt 0))
        (add-after (list 'decrease! rpt)))))
  
  
  ;;;
  ;;;; some
  ;;;


  (define (process-some actions rest)
    (bind (what . rest) rest
      (let ((res (if (null? rest) (unique "res") (cadr rest))))
        (add-binding res '<bool> '#f)
        (add-test (list 'not res))
        (add-action (list 'when what (list 'set! res #t)) actions)
        (set-finally (list res)))))
  
  
  ;;;
  ;;;; every
  ;;;


  (define (process-every actions rest)
    (bind (what . rest) rest
      (let ((res (if (null? rest) (unique "res") (cadr rest))))
        (add-binding res '<bool> '#t)
        (add-test res)
        (add-action (list 'when (list 'not what) (list 'set! res #f)) actions)
        (set-finally (list res)))))
  
  
  ;;;
  ;;;; when
  ;;;


  (define (process-when actions rest)
    (bind (test . body) rest
      (let ((when-actions (process-clauses body)))
        (add-action `(when ,test
                       ,@when-actions)
                    actions))))
  
  
  ;;;
  ;;;; unless
  ;;;


  (define (process-unless actions rest)
    (bind (test . body) rest
      (let ((unless-actions (process-clauses body)))
        (add-action `(unless ,test
                       ,@unless-actions)
                    actions))))
  
  
  ;;;
  ;;;; do
  ;;;


  (define (process-do actions rest)
    (add-actions rest actions))
  
  
  ;;;
  ;;;; sum
  ;;;


  (define (process-sum actions rest)
    (bind (what . rest) rest
      (let ((res (if (null? rest) (unique "res") (cadr rest))))
        (add-binding res '<fx> 0)
        (add-action (list 'increase! res what) actions)
        (set-finally (list res)))))
  
  
  ;;;
  ;;;; collect
  ;;;


  (define (process-collect actions rest)
    (bind (what . rest) rest
      (let ((res (if (null? rest) (unique "res") (cadr rest)))
            (ptr (unique "ptr"))
            (cns (unique "cns")))
        (add-binding res '<list> ''())
        (add-binding ptr '<list> ''())
        (add-binding cns '<list+>)
        (add-action (list 'set! cns (list 'cons what ''())) actions)
        (add-action (list 'if (list 'jazz.null? ptr)
                          (list 'begin
                                (list 'set! ptr cns)
                                (list 'set! res ptr))
                          (list 'set-cdr! ptr cns)
                          (list 'set! ptr cns))
                    actions)
        (set-finally (list res)))))
  
  
  ;;;
  ;;;; return
  ;;;


  (define (process-return actions rest)
    (receive (ret ext) (get-return/exit)
      (add-action (list 'set! ret (car rest)) actions)
      (add-action (list 'set! ext #t) actions)))
  
  
  ;;;
  ;;;; finally
  ;;;


  (define (process-finally actions rest)
    (set-finally rest))
  
  
  ;;;
  ;;;; expand
  ;;;
  
  
  (expand clauses))))
