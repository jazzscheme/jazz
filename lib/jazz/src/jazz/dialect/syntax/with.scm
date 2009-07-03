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


(library protected jazz.dialect.syntax.with scheme


(import (jazz.dialect.kernel.boot))


;; should prolly expand into a let to be consistant with the naming
(syntax public (with form-src)
  (let ((bindings (source-code (cadr (source-code form-src))))
        (body (cddr (source-code form-src))))
    (sourcify-if
      `(let* ,bindings
         (prog1 (begin ,@body)
           ,@(map (lambda (binding)
                    `(release~ ,(source-code (car (source-code binding)))))
                  (reverse bindings))))
      form-src)))


;; note that this is a quick not correct solution as in (with ((rect ... rect ...)) ...)
;; the second rect will incorrectly refer to the first rect
(syntax public (with-closed form-src)
  (let ((bindings (source-code (cadr (source-code form-src))))
        (body (cddr (source-code form-src))))
    (sourcify-if
      `(let (,@(map (lambda (binding)
                      (let ((specifier (or (binding-specifier binding) '<Object>)))
                        `(,(source-code (car (source-code binding))) ,specifier #f)))
                    bindings))
         ,@(map (lambda (binding)
                  (let ((variable (source-code (car (source-code binding)))))
                    (if (binding-specifier binding)
                        `(set! ,variable ,(caddr (source-code binding)))
                      `(set! ,variable ,(cadr (source-code binding))))))
                bindings)
         (dynamic-wind (function () #f)
                       (function () ,@body)
                       (function () ,@(map (lambda (binding)
                                             (let ((variable (source-code (car (source-code binding)))))
                                               `(if ,variable
                                                    (close~ ,variable))))
                                           bindings))))
      form-src))))
