;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; With
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


(module protected jazz.language.syntax.with scheme


(export with)

(import (jazz.language.runtime.kernel))


(define (expand-one binding body)
  (define (parse-binding proc)
    (let ((variable (car (source-code binding)))
          (specifier (binding-specifier binding)))
      (if specifier
          (proc variable specifier (caddr (source-code binding)))
        (proc variable '<Object> (cadr (source-code binding))))))
  
  (parse-binding
    (lambda (variable specifier value)
      `(let ((,variable ,specifier ,value))
         (dynamic-wind (lambda () #f)
                       (lambda () ,@body)
                       (lambda () (close~ ,variable)))))))


(define (expand-with bindings body)
  (if (null? bindings)
      `(let () ,@body)
    (expand-one (car bindings) (list (expand-with (cdr bindings) body)))))


(define-syntax with
  (lambda (form-src usage-environment macro-environment)
    (let ((bindings (source-code (cadr (source-code form-src))))
          (body (cddr (source-code form-src))))
      (sourcify-if
        (expand-with bindings body)
        form-src)))))
