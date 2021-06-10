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


(module protected jazz.language.syntax.loop scheme


(export loop)

(import (jazz.language.runtime.kernel)
        (jazz.language.syntax.increase (phase syntax))
        (jazz.language.syntax.bind (phase syntax))
        (jazz.language.syntax.macros (phase syntax)))


(native private jazz:->string)
(native private jazz:error)


;;;
;;;; Loop
;;;


(define-syntax loop
  (lambda (form-src usage-environment macro-environment)
    (let ((clauses (cdr (source-code form-src))))
      (sourcify-deep-if
        (loop-expand clauses)
        form-src))))


(define (loop-expand clauses)
  (let ((globals     '())
        (variables   '())
        (tests       '())
        (bindings    '())
        (alterations '())
        (iterations  '())
        (finally     '())
        (unique-rank 0))
    
  
  ;;;
  ;;;; Expand
  ;;;


  (define (expand)
    (process-clauses #f clauses)
    (expand-loop))
    
  
  ;;;
  ;;;; Clauses
  ;;;

  
  (define (process-clauses test clauses)
    (for-each (lambda (clause)
                (if (not (pair? (source-code clause)))
                    (process-actions test (list clause))
                  (bind (key . rest) (source-code clause)
                    (case (source-code key)
                      ((with)    (process-with    test rest))
                      ((repeat)  (process-repeat  test rest))
                      ((for)     (process-for     test rest))
                      ((when)    (process-when    test rest))
                      ((unless)  (process-unless  test rest))
                      ((if)      (process-if      test rest))
                      ((do)      (process-do      test rest))
                      ((sum)     (process-sum     test rest))
                      ((collect) (process-collect test rest))
                      ((some)    (process-some    test rest))
                      ((every)   (process-every   test rest))
                      ((return)  (process-return  test rest))
                      ((finally) (process-finally test rest))
                      (else      (process-actions test (list clause)))))))
              clauses))
  
  
  (define (process-actions test actions)
    (add-action
      (if (not test)
          (simplify-begin `(begin ,@actions))
        `(when ,(car test) ,@actions))))
  
  
  (define (unique prefix)
    (let ((symbol (string->symbol (string-append prefix (->string unique-rank)))))
      (increase! unique-rank)
      symbol))


  (define (expand-loop)
    (let ((iter (unique "iter")))
      (expand-globals
        (expand-variables iter
          (expand-tests
            (expand-bindings
              (if (null? alterations)
                  '(unspecified)
                (simplify-begin
                  `(begin
                     ,@(expand-alterations iter)))))
            finally)))))
  
  
  (define (expand-globals inner)
    (define (process scan)
      (if (null? scan)
          inner
        `(let (,(car scan))
           ,(process (cdr scan)))))
    
    (process globals))
  
  
  (define (expand-variables iter inner)
    (if (null? variables)
        inner
      `(let (,iter ,@variables)
         (declare (proper-tail-calls))
         ,inner)))
  
  
  (define (expand-tests yes no)
    (if (null? tests)
        (simplify-begin `(begin ,yes ,@no))
      `(if ,(simplify-and `(and ,@tests))
           ,yes
         ,@no)))
  
  
  (define (expand-bindings body)
    (define (process scan)
      (if (null? scan)
          body
        (bind (var type value) (car scan)
          `(let ((,var ,type ,value))
             ,(process (cdr scan))))))
    
    (process bindings))
  
  
  (define (expand-alterations iter)
    (define (process scan)
      (if (null? scan)
          (if (null? variables)
              '()
            `((,iter ,@(expand-iterations))))
        (let ((alter (car scan)))
          (if (procedure? alter)
              (alter (process (cdr scan)))
            (bind (var value) alter
              `((let ((,var ,value))
                  ,@(process (cdr scan)))))))))
    
    (process alterations))
  
  
  (define (expand-iterations)
    (map (lambda (var)
           (let ((name (car var)))
             (let ((pair (assq name iterations)))
               (if pair
                   (cadr pair)
                 (error "Unable to find iteration for: {s}" name)))))
         variables))
  
  
  (define (add-global variable type . rest)
    (let ((binding (cons variable (cons type (if (null? rest) (list #f) rest)))))
      (set! globals (append globals (list binding)))))
  
  
  (define (add-variable variable type value)
    (let ((binding (list variable type value)))
      (set! variables (append variables (list binding)))))
  
  
  (define (add-test test)
    (set! tests (append tests (list test))))
  
  
  (define (add-binding variable type . rest)
    (let ((binding (cons variable (cons type (if (null? rest) (list #f) rest)))))
      (set! bindings (append bindings (list binding)))))
  
  
  (define (add-action action)
    (add-control
      (lambda (remain)
        (cons action remain))))
  
  
  (define (add-control alter)
    (set! alterations (append alterations (list alter))))
  
  
  (define (add-alteration variable value)
    (let ((alter (list variable value)))
      (set! alterations (append alterations (list alter)))))
  
  
  (define (add-iteration variable value)
    (let ((iter (list variable value)))
      (set! iterations (append iterations (list iter)))))
  
  
  (define (set-finally lst)
    (if (null? finally)
        (set! finally lst)
      (error "More than one return path found in loop")))
  
  
  (define (no-test test)
    (if test
        (error "Invalid clauses found in loop test: {s}" (desourcify-all (cdr test)))))
  
  
  (define (test-if test yes no)
    (if (not test)
        yes
      `(if ,(car test)
           ,yes
         ,no)))
  
  
  (define (test-and test value)
    (if (not test)
        value
      `(and ,(car test) ,value)))
  
  
  (define (simplify-and form)
    (if (and (pair? (cdr form))
             (null? (cddr form)))
        (cadr form)
      form))
  
  
  ;;;
  ;;;; with
  ;;;


  (define (process-with test rest)
    (no-test test)
    (bind (variable equal value) rest
      (add-global variable '<Object> value)))
  
  
  ;;;
  ;;;; repeat
  ;;;


  (define (process-repeat test rest)
    (no-test test)
    (bind (count) rest
      (let ((rpt (unique "rpt")))
        (add-variable rpt '<fx> count)
        (add-test `(> ,rpt 0))
        (add-iteration rpt `(- ,rpt 1)))))
  
  
  ;;;
  ;;;; for
  ;;;


  (define (process-for test rest)
    (define (parse-for rest)
      (bind (variable . rest) rest
        (if (specifier? (source-code (car rest)))
            (bind (type key . rest) rest
              (values variable type key rest))
          (bind (key . rest) rest
            (values variable #f key rest)))))
    
    (no-test test)
    (receive (variable type key rest) (parse-for rest)
      (case (source-code key)
        ((in)
         (bind (lst . rest) rest
           (let ((for (unique "for")))
             (add-variable for '<list> lst)
             (add-test `(not (null? ,for)))
             (add-binding variable (or type '<Object>) `(car ,for))
             (add-iteration for `(cdr ,for))
             (when (not-null? rest)
               (bind (keyword value) rest
                 (case (source-code keyword)
                   ((remainder)
                    (add-binding value '<Object+> `(cdr ,for)))
                   (else (error "Unknown for in keyword: {t}" keyword))))))))
        ((in-vector)
         (bind (vector . rest) rest
           (let ((vec (unique "vec"))
                 (for (unique "for"))
                 (len (unique "len")))
             (add-global vec '<vector> vector)
             (add-global len '<fx> `(length ,vec))
             (add-variable for '<fx> 0)
             (add-binding variable (or type '<Object>) `(element ,vec ,for))
             (add-test `(< ,for ,len))
             (add-iteration for `(+ ,for 1)))))
        ((in-sequence)
         (bind (iterator) rest
           (let ((val (unique "val"))
                 (itr (unique "itr")))
             (add-global val '<Object> iterator)
             (add-variable itr '<Iterator> `(if (is? ,val Iterator) ,val (iterator ,val)))
             (add-binding variable '<Object> `(get-next ,itr))
             (add-test `(not (done? ,itr)))
             (add-iteration itr itr))))
        ((in-properties)
         (bind (keyword value) (source-code variable)
           (bind (lst) rest
             (let ((for (unique "for")))
               (add-variable for '<Object> lst)
               (add-test `(not (null? ,for)))
               (add-binding keyword '<Object> `(car ,for))
               (add-binding value '<Object> `(cadr ,for))
               (add-iteration for `(cddr ,for))))))
        ((from)
         (bind (from . rest) rest
           (let ((type (or type '<fx>))
                 (to #f)
                 (test #f)
                 (oper '+)
                 (by 1)
                 (scan rest))
             (while (not-null? scan)
               (let ((key (source-code (car scan))))
                 (case key
                   ((to) (set! to (cadr scan)) (set! test '<=) (set! scan (cddr scan)))
                   ((below) (set! to (cadr scan)) (set! test '<) (set! scan (cddr scan)))
                   ((downto) (set! to (cadr scan)) (set! test '>=) (set! oper '-) (set! scan (cddr scan)))
                   ((above) (set! to (cadr scan)) (set! test '>) (set! oper '-) (set! scan (cddr scan)))
                   ((end) (set! to (cadr scan)) (set! test '<=) (set! scan (cddr scan)))
                   ((by) (set! by (cadr scan)) (set! scan (cddr scan)))
                   (else (error "Unknown for keyword: {t}" (desourcify-all key))))))
             (add-variable variable type from)
             (when to
               (let ((end (if (or (symbol? (source-code to)) (number? (source-code to))) to (unique "end"))))
                 (when (not (eq? end to))
                   (add-global end type to))
                 (add-test (list test variable end))))
             (add-iteration variable (list oper variable by)))))
        ((start)
         (bind (start e end b by) rest
           (let ((type '<fx>)
                 (inc (unique "inc"))
                 (inc? (unique "inc?"))
                 (s (unique "s"))
                 (e (unique "e"))
                 (src (unique "src"))
                 (dst (unique "dst"))
                 (test (unique "test")))
             (add-global inc type by)
             (add-global inc? '<bool> `(> ,inc 0))
             (add-global s type start)
             (add-global e type end)
             (add-global src type `(if ,inc? ,s ,e))
             (add-global dst type `(if ,inc? ,e ,s))
             (add-global test '<Object> `(if ,inc? <= >=))
             (add-variable variable type src)
             (add-test (list test variable dst))
             (add-iteration variable (list '+ variable inc)))))
        ((first)
         (bind (first . rest) rest
           (add-variable variable '<Object> first)
           (add-iteration variable
                          (if (null? rest)
                              variable
                            (bind (then-key then) rest
                              then)))))
        ((init)
         (bind (init t test i iter) rest
           (add-variable variable (or type '<Object>) init)
           (add-test test)
           (add-iteration variable iter)))
        ((=)
         (let ((value (car rest)))
           (add-binding variable '<Object> value)))
        (else
         (error "Unknown for keyword: {t}" (desourcify-all key))))))
  
  
  ;;;
  ;;;; when
  ;;;


  (define (process-when test rest)
    (no-test test)
    (bind (test . body) rest
      (let ((var (unique "test")))
        (add-binding var '<Object> test)
        (process-clauses (cons var test) body))))
  
  
  ;;;
  ;;;; unless
  ;;;


  (define (process-unless test rest)
    (no-test test)
    (bind (test . body) rest
      (let ((var (unique "test")))
        (add-binding var '<Object> `(not ,test))
        (process-clauses (cons var test) body))))
  
  
  ;;;
  ;;;; if
  ;;;


  (define (process-if test rest)
    (no-test test)
    (bind (test yes . no) rest
      (let ((var (unique "test")))
        (add-binding var '<Object> test)
        (process-clauses (cons var test) (list yes)))
      (let ((var (unique "test")))
        (add-binding var '<Object> `(not ,test))
        (process-clauses (cons var test) no))))
  
  
  ;;;
  ;;;; do
  ;;;


  (define (process-do test rest)
    (process-actions test rest))
  
  
  ;;;
  ;;;; sum
  ;;;


  (define (process-sum test rest)
    (bind (what . rest) rest
      (let ((into (if (null? rest) #f (cadr rest))))
        (let ((res (or into (unique "res"))))
          (add-variable res '<fx> 0)
          (add-alteration res (test-if test `(+ ,res ,what) res))
          (add-iteration res res)
          (when (not into)
            (set-finally (list res)))))))
  
  
  ;;;
  ;;;; collect
  ;;;


  (define (process-collect test rest)
    (bind (what . rest) rest
      (let ((into (if (null? rest) #f (cadr rest))))
        (let ((res (or into (unique "res")))
              (ptr (unique "ptr"))
              (cns (unique "cns"))
              (nul (unique "nul")))
          (add-variable res '<list> ''())
          (add-variable ptr '<list> ''())
          (add-binding cns '<list+> (test-and test `(cons ,what '())))
          (add-binding nul '<bool> (test-and test `(null? ,ptr)))
          (add-alteration ptr (test-if test `(if ,nul
                                                 ,cns
                                               (begin
                                                 (set-cdr! ,ptr ,cns)
                                                 ,cns))
                                ptr))
          (add-alteration res (test-if test `(if ,nul
                                                 ,ptr
                                               ,res)
                                res))
          (add-iteration ptr ptr)
          (add-iteration res res)
          (when (not into)
            (set-finally (list res)))))))
  
  
  ;;;
  ;;;; some
  ;;;


  (define (process-some test rest)
    (no-test test)
    (bind (what . rest) rest
      (let ((into (if (null? rest) #f (cadr rest))))
        (let ((res (or into (unique "res"))))
          (add-variable res '<bool> '#f)
          (add-test `(not ,res))
          (add-iteration res `(or ,res ,what))
          (when (not into)
            (set-finally (list res)))))))
  
  
  ;;;
  ;;;; every
  ;;;


  (define (process-every test rest)
    (no-test test)
    (bind (what . rest) rest
      (let ((into (if (null? rest) #f (cadr rest))))
        (let ((res (or into (unique "res"))))
          (add-variable res '<bool> '#t)
          (add-test res)
          (add-iteration res `(and ,res ,what))
          (when (not into)
            (set-finally (list res)))))))
  
  
  ;;;
  ;;;; return
  ;;;


  (define (process-return test rest)
    (let ((effective
            (if (null? rest)
                finally
              rest)))
      (if (not test)
          (add-control
            (lambda (remain)
              effective))
        (add-control
          (lambda (remain)
            `((if ,(car test)
                  ,(simplify-begin `(begin ,@effective))
                ,@remain)))))))
  
  
  ;;;
  ;;;; finally
  ;;;


  (define (process-finally test rest)
    (set-finally rest))
  
  
  ;;;
  ;;;; expand
  ;;;
  
  
  (expand))))
