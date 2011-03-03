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


(unit protected core.base.syntax.macros


(jazz:define-syntax %%boolean
  (lambda (src)
    (let ((expr (%%cadr (jazz:source-code src))))
      `(if ,expr #t #f))))


(jazz:define-syntax %%not-null?
  (lambda (src)
    (let ((expr (%%cadr (jazz:source-code src))))
      `(%%not (%%null? ,expr)))))


;;;
;;;; When
;;;


(jazz:define-syntax %%when
  (lambda (src)
    (let ((test (%%cadr (jazz:source-code src)))
          (body (%%cddr (jazz:source-code src))))
      `(if ,test
           (begin
             ,@body)
         #f))))


;;;
;;;; While
;;;


(jazz:define-syntax %%while
  (lambda (src)
    (let ((test (%%cadr (jazz:source-code src)))
          (body (%%cddr (jazz:source-code src)))
          (iter (jazz:generate-symbol "iter")))
      `(let ,iter ()
         (if ,test
             (begin
               ,@body
               (,iter)))))))


;;;
;;;; Assert
;;;


(jazz:define-syntax %%core-assert
  (lambda (src)
    (jazz:expand-%%assert jazz:debug-core? src)))


(jazz:define-syntax %%core-assertion
  (lambda (src)
    (jazz:expand-%%assertion jazz:debug-core? src)))


(jazz:define-syntax %%debug-assert
  (lambda (src)
    (jazz:expand-%%assert jazz:debug-user? src)))


(jazz:define-syntax %%debug-assertion
  (lambda (src)
    (jazz:expand-%%assertion jazz:debug-user? src)))


(jazz:define-syntax %%assert
  (lambda (src)
    (jazz:expand-%%assert #t src)))


(jazz:define-syntax %%assertion
  (lambda (src)
    (jazz:expand-%%assertion #t src)))


(define (jazz:expand-%%assert test? src)
  (let ((assertion (%%cadr (jazz:source-code src)))
        (body (%%cddr (jazz:source-code src))))
    (let ((message (let ((port (open-output-string)))
                     (display "Assertion " port)
                     (write (%%desourcify assertion) port)
                     (display " failed" port)
                     (get-output-string port)))
          (effective-body (if (%%null? body) (%%list (%%list '%%unspecified)) body)))
      (jazz:expand-%%assertion-body test? assertion (%%list 'error message) effective-body))))


(define (jazz:expand-%%assertion test? src)
  (let ((assertion (%%cadr (jazz:source-code src)))
        (action (%%car (%%cddr (jazz:source-code src))))
        (body (%%cdr (%%cddr (jazz:source-code src)))))
    (let ((effective-body (if (%%null? body) (%%list (%%list '%%unspecified)) body)))
      (jazz:expand-%%assertion-body test? assertion action effective-body))))


(define (jazz:expand-%%assertion-body test? assertion action body)
  (if test?
      `(if (%%not ,assertion)
           ,action
         ,(jazz:simplify-begin
            `(begin
               ,@body)))
    (jazz:simplify-begin `(begin ,@body))))


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


(jazz:define-macro (jazz:bind bindings tree . body)

  (define (expand-car bindings tree body)
    (let ((car-binding (%%car bindings)))
      (cond ((%%symbol? car-binding)
             `((let ((,car-binding (%%car ,tree)))
                 ,@(expand-cdr bindings tree body))))
            ((%%pair? car-binding)
             (let ((car-symbol (jazz:generate-symbol "car")))
               `((if (%%null? ,tree)
                     (jazz:error "Unable to bind")
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
             (let ((cdr-symbol (jazz:generate-symbol "cdr")))
               `((let ((,cdr-symbol (%%cdr ,tree)))
                   ,@(expand-car cdr-binding cdr-symbol body))))))))
  
  (let ((tree-symbol (jazz:generate-symbol "tree")))
    `(let ((,tree-symbol ,tree))
       ,@(expand-car bindings tree-symbol body))))


;;;
;;;; Compose
;;;


(jazz:define-macro (%%compose-identifier s1 s2)
  `(%%string->symbol (%%string-append (%%symbol->string ,s1) "." (%%symbol->string ,s2))))


(jazz:define-macro (%%compose-reference s1 s2)
  `(%%string->symbol (%%string-append (%%symbol->string ,s1) ":" (%%symbol->string ,s2)))))
