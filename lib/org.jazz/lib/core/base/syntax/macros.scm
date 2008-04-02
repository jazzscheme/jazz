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


(module core.base.syntax.macros


(jazz.define-macro (%%boolean expr)
  `(if ,expr #t #f))


(jazz.define-macro (%%not-null? expr)
  `(%%not (%%null? ,expr)))


;;;
;;;; When
;;;


(jazz.define-macro (%%when test . body)
  `(if ,test
       (begin
         ,@body)
     #f))


;;;
;;;; While
;;;


(jazz.define-macro (%%while test . body)
  (let ((iter (jazz.generate-symbol "iter")))
    `(let ,iter ()
       (if ,test
           (begin
             ,@body
             (,iter))))))


;;;
;;;; Assert
;;;


(jazz.define-macro (%%core-assert . rest)
  (apply jazz.expand-%%assert jazz.debug-core? rest))


(jazz.define-macro (%%core-assertion . rest)
  (apply jazz.expand-%%assertion jazz.debug-core? rest))


(jazz.define-macro (%%debug-assert . rest)
  (apply jazz.expand-%%assert jazz.debug-user? rest))


(jazz.define-macro (%%debug-assertion . rest)
  (apply jazz.expand-%%assertion jazz.debug-user? rest))


(jazz.define-macro (%%assert . rest)
  (apply jazz.expand-%%assert #t rest))


(jazz.define-macro (%%assertion . rest)
  (apply jazz.expand-%%assertion #t rest))


(define (jazz.expand-%%assert test? assertion . body)
  (let ((message (let ((port (open-output-string)))
                   (display "Assertion " port)
                   (write assertion port)
                   (display " failed" port)
                   (get-output-string port))))
    (apply jazz.expand-%%assertion test? assertion (list 'error message) body)))


(define (jazz.expand-%%assertion test? assertion action . body)
  (if test?
      `(if (%%not ,assertion)
           ,action
         ,(jazz.simplify-begin
            `(begin
               ,@body)))
    (jazz.simplify-begin `(begin ,@body))))


;;;
;;;; Bind
;;;


; @macro (bind ((a . r) b . c) tree (%%list a b c r))
; @expansion
; (let ((tree0 tree))
;   (let ((car1 (%%car tree0))
;         (cdr2 (%%cdr tree0)))
;     (let ((a (%%car car1))
;           (r (%%cdr car1)))
;       (let ((b (%%car cdr2))
;             (c (%%cdr cdr2)))
;         (%%list a b c r)))))


(jazz.define-macro (jazz.bind bindings tree . body)

  (define (expand-car bindings tree body)
    (let ((car-binding (%%car bindings)))
      (cond ((%%symbol? car-binding)
             `((let ((,car-binding (%%car ,tree)))
                 ,@(expand-cdr bindings tree body))))
            ((%%pair? car-binding)
             (let ((car-symbol (jazz.generate-symbol "car")))
               `((if (%%null? ,tree)
                     (jazz.error "Unable to bind")
                   (let ((,car-symbol (%%car ,tree)))
                     ,@(expand-car car-binding car-symbol
                         (expand-cdr bindings tree body))))))))))

  (define (expand-cdr bindings tree body)
    (let ((cdr-binding (%%cdr bindings)))
      (cond ((%%null? cdr-binding)
             body)
            ((%%symbol? cdr-binding)
             `((let ((,cdr-binding (%%cdr ,tree)))
                 ,@body)))
            ((%%pair? cdr-binding)
             (let ((cdr-symbol (jazz.generate-symbol "cdr")))
               `((let ((,cdr-symbol (%%cdr ,tree)))
                   ,@(expand-car cdr-binding cdr-symbol body))))))))
  
  (let ((tree-symbol (jazz.generate-symbol "tree")))
    `(let ((,tree-symbol ,tree))
       ,@(expand-car bindings tree-symbol body))))


;;;
;;;; Compose
;;;


(jazz.define-macro (%%compose-name s1 s2)
  `(%%string->symbol (%%string-append (%%symbol->string ,s1) "." (%%symbol->string ,s2)))))
