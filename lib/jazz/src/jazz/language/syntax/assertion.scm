;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Assertions
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
;;;  Portions created by the Initial Developer are Copyright (C) 1996-2015
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


(module protected jazz.language.syntax.assertion scheme


(export assert
        assertion
        allege
        debug-assert
        debug-assertion)


(define-syntax assert
  (lambda (form-src usage-environment macro-environment)
    (sourcify-deep-if
      ;; we really want assertions in release and not in a new distribution safety
      (expand-assert-test #t form-src)
      form-src)))


(define-syntax assertion
  (lambda (form-src usage-environment macro-environment)
    (sourcify-deep-if
      ;; we really want assertions in release and not in a new distribution safety
      (expand-assertion-test #t form-src)
      form-src)))


(define-syntax allege
  (lambda (form-src usage-environment macro-environment)
    (let ((test (cadr (source-code form-src)))
          (body (cddr (source-code form-src))))
      (cond ((null? body)
             (let ((temp (generate-symbol "temp")))
               (sourcify-deep-if
                 (if (symbol? (source-code test))
                     `(%allege ,test ,test)
                   `(let ((,temp ,test))
                      (%allege ,temp ,temp)))
                 form-src)))
            ((null? (cdr body))
             (sourcify-deep-if
               `(%allege ,test ,(car body))
               form-src))
            (else
             (sourcify-deep-if
               `(%allege ,test (begin ,@body))
               form-src))))))


(define-syntax debug-assert
  (lambda (form-src usage-environment macro-environment)
    (sourcify-deep-if
      (expand-assert-test debug-user? form-src)
      form-src)))


(define-syntax debug-assertion
  (lambda (form-src usage-environment macro-environment)
    (sourcify-deep-if
      (expand-assertion-test debug-user? form-src)
      form-src)))


(define (expand-assert-test test? src)
  (let ((assertion (cadr (source-code src)))
        (body (cddr (source-code src))))
    (let ((message (let ((port (open-output-string)))
                     (display "Assertion " port)
                     (write (desourcify assertion) port)
                     (display " failed" port)
                     (get-output-string port))))
      (expand-assertion-body test? assertion (list 'error message) body))))


(define (expand-assertion-test test? src)
  (let ((assertion (cadr (source-code src)))
        (action (car (cddr (source-code src))))
        (body (cdr (cddr (source-code src)))))
    (expand-assertion-body test? assertion action body)))


(define (expand-assertion-body test? assertion action body)
  (let ((body (if (not (null? body)) body '((unspecified)))))
    (if test?
        `(if (not ,assertion)
             ,action
           ,(simplify-begin
              `(begin
                 ,@body)))
      (simplify-begin `(begin ,@body))))))
