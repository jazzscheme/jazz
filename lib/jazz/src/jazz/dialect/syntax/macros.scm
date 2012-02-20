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


(module protected jazz.dialect.syntax.macros scheme


(import (jazz.dialect.kernel)
        (scheme.syntax-rules (phase syntax)))


(native private jazz:->string)
(native private jazz:error)


(syntax public (submodule form-src)
  (let ((name (cadr (source-code form-src)))
        (body (cddr (source-code form-src))))
    (sourcify-if
      `(begin
         ,@body)
      form-src)))


(syntax public (constant form-src)
  (let ((name (cadr (source-code form-src)))
        (value (caddr (source-code form-src))))
    (sourcify-if
      `(definition public ,name ,value)
      form-src)))


(define-syntax package expand-body
  (syntax-rules ()
    ((_)
     (unspecified))
    ((_ expr ...)
     (begin expr ...))))


#; ;; old
(syntax public (when form-src)
  (let ((test (cadr (source-code form-src)))
        (body (cddr (source-code form-src))))
    (sourcify-if
      `(if ,test
           (begin
             ,@(if (null? body)
                   (list (list 'unspecified))
                 body))
         #f)
      form-src)))


(define-syntax public when
  (syntax-rules ()
    ((when test expr ...)
     (if test
         (expand-body expr ...)
       #f))))


#; ;; old
(syntax public (unless form-src)
  (let ((test (cadr (source-code form-src)))
        (body (cddr (source-code form-src))))
    (sourcify-if
      `(if (not ,test)
           (begin ,@body)
         #f)
      form-src)))


(define-syntax public unless
  (syntax-rules ()
    ((unless test expr ...)
     (when (not test) expr ...))))


#; ;; old
(syntax public (prog1 form-src)
  (let ((returned (cadr (source-code form-src)))
        (body (cddr (source-code form-src)))
        (value (generate-symbol)))
    (sourcify-if
      `(let ((,value ,returned))
         (begin ,@body)
         ,value)
      form-src)))


(define-syntax public prog1
  (syntax-rules ()
    ((prog1 returned expr ...)
     (let ((value returned))
       (expand-body expr ...)
       value))))


#; ;; old
(syntax public (while form-src)
  (let ((test (cadr (source-code form-src)))
        (body (cddr (source-code form-src)))
        (iter (generate-symbol "iter")))
    (sourcify-if
      `(let (,iter)
         (if ,test
             (begin
               ,@body
               (,iter))))
      form-src)))


(define-syntax public while
  (syntax-rules ()
    ((while test expr ...)
     (let (iterate)
       (if test
           (begin
             expr ...
             (iterate)))))))


#; ;; old
(syntax public (unwind-protect form-src)
  (let ((body (cadr (source-code form-src)))
        (protection (cddr (source-code form-src))))
    (sourcify-if
      `(dynamic-wind (lambda () #f)
                     (lambda () ,body)
                     (lambda () ,@protection))
      form-src)))


(define-syntax public unwind-protect
  (syntax-rules ()
    ((unwind-protect body protection ...)
     (dynamic-wind (lambda () #f)
                   (lambda () body)
                   (lambda () protection ...)))))


;; @syntax (catch X (f)) @expansion (call-with-catch X (lambda (exc) exc) (lambda () (f)))
;; @syntax (catch (X y (g y)) (f)) @expansion (call-with-catch X (lambda (y) (g y)) (lambda () (f)))

(syntax public (catch form-src)
  (if (null? (cdr (unwrap-syntactic-closure form-src)))
      (error "Ill-formed catch")
    (let ((predicate/type (cadr (source-code form-src)))
          (body (cddr (source-code form-src))))
      (sourcify-if
        (cond ((symbol? (source-code predicate/type))
               `(call-with-catch ,predicate/type (lambda (exc) exc)
                  (lambda ()
                    ,@body)))
              ((pair? (source-code predicate/type))
               `(call-with-catch ,(car (source-code predicate/type)) (lambda (,(source-code (cadr (source-code predicate/type)))) ,@(cddr (source-code predicate/type)))
                  (lambda ()
                    ,@body)))
              (else
               (error "Ill-formed predicate/type in catch: {t}" (desourcify predicate/type))))
        form-src))))


(syntax public (~ form-src)
  (let ((name (source-code (cadr (source-code form-src))))
        (object (car (cddr (source-code form-src)))))
    (sourcify-if
      (with-uniqueness object
        (lambda (obj)
          `(lambda rest
             (apply (dispatch (class-of ,obj) ',name) ,obj rest))))
      form-src)))


(syntax public (local-context form-src)
  (let ((names (cdr (source-code form-src))))
    (sourcify-if
      `(list ,@(map (lambda (name)
                      `(cons ',(source-code name) ,name))
                    names))
      form-src)))


;; @macro (push! x (f)) @expansion (set! x (cons x (f)))

(define (expand-push! location value)
  (list 'set! location (list 'cons value location)))


;; @macro (pop! x) @expansion (set! x (cdr x))

(define (expand-pop! location)
  (list 'set! location (list 'cdr location)))


(define (expand-assert first rest)
  (if (null? rest)
      (let* ((expr first)
             (message (string-append "Assertion " (->string expr :text) " failed")))
        (list 'unless expr (list 'error "{a}" message)))
    (let* ((expr (car rest))
           (message (->string expr :text))
           (proc first))
      (list 'unless expr (list proc message)))))


(define (expand-assert-type expr type)
  (let ((value (generate-symbol)))
    (cons 'let*
          (cons (list (list value expr))
                (list (list 'when (list 'is-not? value type) (list 'error "{s} is not of the expected {s} type" value (list 'category-identifier type)))
                      value)))))


(define (expand-error? body)
  (let ((err (generate-symbol "err")))
    (list 'catch
          (list 'Error err #t)
          (cons 'begin body)
          #f)))


;;;
;;;; C Interface
;;;


(syntax public (c-constant form-src)
  (let ((name (cadr (source-code form-src)))
        (value (caddr (source-code form-src))))
    (sourcify-if
      `(definition public ,name ,value)
      form-src)))


(syntax public (c-enumeration form-src)
  (let ((name (cadr (source-code form-src)))
        (declarations (cddr (source-code form-src))))
    (sourcify-if
      (let ((definitions (map (lambda (declaration) `(definition public ,@(source-code declaration))) declarations)))
        `(begin ,@definitions))
      form-src))))
