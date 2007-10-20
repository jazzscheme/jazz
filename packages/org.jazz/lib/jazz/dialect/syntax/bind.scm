;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Bind
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
;;;    Stephane Le Cornec
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


(library jazz.dialect.syntax.bind scheme


(import (jazz.dialect.kernel))


; @macro (bind ((a . r) b . c) tree (list a b c r))
; @expansion
; (let ((tree0 tree))
;   (let ((car1 (car tree0))
;         (cdr2 (cdr tree0)))
;     (let ((a (car car1))
;           (r (cdr car1)))
;       (let ((b (car cdr2))
;             (c (cdr cdr2)))
;         (list a b c r)))))


(define-macro (bind bindings tree . body)
  (with-expression-value tree
    (lambda (tree-value)
      `(begin
         ,@(expand-bind-car bindings tree-value body)))))


(define (expand-bind-car bindings tree body)
  (let ((car-binding (car bindings)))
    (cond ((symbol? car-binding)
           `((let ((,car-binding (car ,tree)))
               ,@(expand-bind-cdr bindings tree body))))
          ((pair? car-binding)
           (let ((car-symbol (generate-symbol "car")))
             `((let ((,car-symbol (car ,tree)))
                 ,@(expand-bind-car car-binding car-symbol
                     (expand-bind-cdr bindings tree body)))))))))


(define (expand-bind-cdr bindings tree body)
  (let ((cdr-binding (cdr bindings)))
    (cond ((null? cdr-binding)
           body)
          ((symbol? cdr-binding)
           `((let ((,cdr-binding (cdr ,tree)))
               ,@body)))
          ((pair? cdr-binding)
           (let ((cdr-symbol (generate-symbol "cdr")))
             `((let ((,cdr-symbol (cdr ,tree)))
                 ,@(expand-bind-car cdr-binding cdr-symbol body)))))))))
