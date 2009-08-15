;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Kernel Header
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


(include "~~lib/_gambit#.scm")


;;;
;;;; Macro
;;;


(define-runtime-macro (jazz.define-macro pattern . rest)

  (define (form-size parms)
    (let loop ((lst parms) (n 1))
      (cond ((pair? lst)
             (let ((parm (car lst)))
               (if (memq parm '(#!optional #!key #!rest))
                 (- n)
                 (loop (cdr lst)
                       (+ n 1)))))
            ((null? lst)
             n)
            (else
             (- n)))))

  (let ((src `(lambda ,(cdr pattern) ,@rest)))
    `(begin
       (##define-macro ,pattern
         ,@rest)
       (##top-cte-add-macro!
        ##interaction-cte
        ',(car pattern)
        (##make-macro-descr
          #f
          ',(form-size (cdr pattern))
          ,src
          #f))
       (jazz.register-macro ',(car pattern)
         ,src))))


;;;
;;;; Syntax
;;;


(define-runtime-macro (jazz.define-syntax name expander)
  `(begin
     (##define-syntax ,name
       ,expander)
     (##top-cte-add-macro!
      ##interaction-cte
      ',name
      (##make-macro-descr
        #t
        -1
        ,expander
        #f))
     (jazz.register-macro ',name
       ,expander)))


;;;
;;;; Types
;;;


(define jazz.subtype-vector       (macro-subtype-vector))
(define jazz.subtype-pair         (macro-subtype-pair))
(define jazz.subtype-ratnum       (macro-subtype-ratnum))
(define jazz.subtype-cpxnum       (macro-subtype-cpxnum))
(define jazz.subtype-symbol       (macro-subtype-symbol))
(define jazz.subtype-keyword      (macro-subtype-keyword))
(define jazz.subtype-continuation (macro-subtype-continuation))
(define jazz.subtype-procedure    (macro-subtype-procedure))
(define jazz.subtype-string       (macro-subtype-string))
(define jazz.subtype-flonum       (macro-subtype-flonum))
(define jazz.subtype-bignum       (macro-subtype-bignum))
(define jazz.subtype-foreign      (macro-subtype-foreign))
(define jazz.subtype-s8vector     (macro-subtype-s8vector))
(define jazz.subtype-u8vector     (macro-subtype-u8vector))
(define jazz.subtype-s16vector    (macro-subtype-s16vector))
(define jazz.subtype-u16vector    (macro-subtype-u16vector))
(define jazz.subtype-s32vector    (macro-subtype-s32vector))
(define jazz.subtype-u32vector    (macro-subtype-u32vector))
(define jazz.subtype-s64vector    (macro-subtype-s64vector))
(define jazz.subtype-u64vector    (macro-subtype-u64vector))
(define jazz.subtype-f32vector    (macro-subtype-f32vector))
(define jazz.subtype-f64vector    (macro-subtype-f64vector))
(define jazz.subtype-boxvalues    (macro-subtype-boxvalues))


;;;
;;;; Code
;;;


(##define-macro (macro-code-run-fixed c rte)
  `(let (($$code ,c))
     ((##vector-ref $$code 1) $$code ,rte)))


(define (jazz.code-cte c)
  (macro-code-cte c))

(define (jazz.code-run c rte)
  (macro-code-run-fixed c rte))


;;;
;;;; Environment
;;;


(define (jazz.rte-up r)
  (macro-rte-up r))


;;;
;;;; Binding
;;;


(define (jazz.repl-context-bind val thunk)
  (macro-dynamic-bind repl-context val thunk))


;;;
;;;; Continuation
;;;


(define (jazz.continuation-denv cont)
  (macro-continuation-denv cont))


;;;
;;;; Repl
;;;


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

(define (jazz.make-repl-context level depth cont initial-cont reason prev-level prev-depth)
  (macro-make-repl-context level depth cont initial-cont reason prev-level prev-depth))


;;;
;;;; Readtable
;;;


(define (jazz.readtable-named-char-table rt)
  (macro-readtable-named-char-table rt))

(define (jazz.readtable-named-char-table-set! rt nc)
  (macro-readtable-named-char-table-set! rt nc))

(define (jazz.readtable-start-syntax rt)
  (macro-readtable-start-syntax rt))


;;;
;;;; Readenv
;;;


(define (jazz.readenv? obj)
  (macro-readenv? obj))

(define (jazz.readenv-port re)
  (macro-readenv-port re))

(define (jazz.readenv-wrap re x)
  (macro-readenv-wrap re x))


;;;
;;;; Writeenv
;;;


(define (jazz.writeenv-port we)
  (macro-writeenv-port we))


;;;
;;;; Thread
;;;


(define (jazz.btq-owner mutex)
  (macro-btq-owner mutex))
