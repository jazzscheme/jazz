;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Macros
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
;;;  Portions created by the Initial Developer are Copyright (C) 1996-2007
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


(library jazz.dialect.syntax.macros scheme


(import (jazz.dialect.kernel))


(syntax (constant name value)
  `(definition ,name ,value))


(syntax (enumeration name . declarations)
  (let ((definitions (map (lambda (declaration) `(definition ,@declaration)) declarations)))
    `(begin ,@definitions)))


(syntax (when test . body)
  `(if ,test
       (begin
         ,@body)
     #f))


(syntax (unless test . body)
  `(if (not ,test)
       (begin ,@body)
     #f))


(syntax (prog1 returned . body)
  (let ((value (generate-symbol)))
    `(let ((,value ,returned))
       (begin ,@body)
       ,value)))


(syntax (unwind-protect body . protection)
  `(dynamic-wind (function dynamic () #f)
                 (function dynamic () ,body)
                 (function dynamic () ,@protection)))


(syntax (catch type . body)
  (cond ((symbol? type)
         `(call-with-catch ,type (lambda (x) x)
            (lambda ()
              ,@body)))
        ((pair? type)
         `(call-with-catch ,(car type) (lambda (,(cadr type)) ,@(cddr type))
            (lambda ()
              ,@body)))
        (else
         (error "Ill-formed type in catch, {t}" type))))


(syntax (~ name object)
  (with-expression-value object
    (lambda (obj)
      `(lambda rest
         (apply (dispatcher ',name ,obj) ,obj rest)))))


(macro (form>> form)
  `(jml->form ',form))


;; @macro (push! x (f)) @expansion (set! x (cons x (f)))

(define (expand-push! location value)
  (list 'set! location (list 'cons value location)))


;; @macro (pop! x) @expansion (set! x (cdr x))

(define (expand-pop! location)
  (list 'set! location (list 'cdr location)))


(define (expand-assert first rest)
  (if (null? rest)
      (let* ((expr first)
             (message (append "Assertion " (->string expr :text) " failed")))
        (list 'unless expr (list 'error "{a}" message)))
    (let* ((expr (car rest))
           (message (->string expr :text))
           (proc first))
      (list 'unless expr (list proc message)))))


(define (expand-assert-type expr type)
  (let ((value (generate-symbol)))
    (cons 'let*
          (cons (list (list value expr))
                (list (list 'when (list 'is-not? value type) (list 'error "{s} is not of the expected {s} type" value (list 'type-name type)))
                      value)))))


(define (expand-error? body)
  (let ((err (generate-symbol "err")))
    (list 'catch
          (list 'Error err #t)
          (cons 'begin body)
          #f))))
