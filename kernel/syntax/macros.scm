;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Kernel Macros
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


(include "~~/lib/_gambit#.scm")


(define jazz.Macros
  (make-table test: eq?))


(define (jazz.register-macro name macro)
  (table-set! jazz.Macros name macro))


(define (jazz.get-macro name)
  (table-ref jazz.Macros name #f))


(define (jazz.need-macro name)
  (or (jazz.get-macro name)
      (jazz.error "Unable to find macro: {s}" name)))


(define (jazz.expand-macro form)
  (apply (jazz.need-macro (car form)) (cdr form)))


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
