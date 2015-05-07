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
;;;  Portions created by the Initial Developer are Copyright (C) 1996-2015
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


(module protected jazz.language.syntax.bind scheme


(export bind
        bind-vector)

(import (jazz.language.runtime.kernel))


(native private jazz:naturals)


; @syntax (bind ((a . r) b . c) tree (list a b c r))
; @expansion
; (let ((val tree))
;   (let ((car1 (car tree0))
;         (cdr2 (cdr tree0)))
;     (let ((a (car car1))
;           (r (cdr car1)))
;       (let ((b (car cdr2))
;             (c (cdr cdr2)))
;         (list a b c r)))))


(define-syntax bind
  (lambda (form-src usage-environment macro-environment)
    (define (expand-bind-car bindings tree body)
      (let ((car-binding (car bindings))
            (cdr-binding (cdr bindings)))
        (cond ((symbol? car-binding)
               (let ((specifier (binding-specifier bindings)))
                 (if specifier
                     `((let ((,car-binding ,specifier (car ,tree)))
                         ,@(expand-bind-cdr (cdr cdr-binding) tree body)))
                   `((let ((,car-binding (car ,tree)))
                       ,@(expand-bind-cdr cdr-binding tree body))))))
              ((pair? car-binding)
               (let ((car-symbol (generate-symbol "car")))
                 `((let ((,car-symbol (car ,tree)))
                     ,@(expand-bind-car car-binding car-symbol
                         (expand-bind-cdr cdr-binding tree body)))))))))
    
    (define (expand-bind-cdr cdr-binding tree body)
      (cond ((null? cdr-binding)
             body)
            ((symbol? cdr-binding)
             `((let ((,cdr-binding (cdr ,tree)))
                 ,@body)))
            ((pair? cdr-binding)
             (let ((cdr-symbol (generate-symbol "cdr")))
               `((let ((,cdr-symbol (cdr ,tree)))
                   ,@(expand-bind-car cdr-binding cdr-symbol body)))))))
    
    (let ((bindings (desourcify (cadr (source-code form-src))))
          (tree (car (cddr (source-code form-src))))
          (body (cdr (cddr (source-code form-src)))))
      (sourcify-if
        (with-uniqueness tree
          (lambda (tree-value)
            `(begin
               ,@(expand-bind-car bindings tree-value body))))
        form-src))))


(define-syntax bind-vector
  (lambda (form-src usage-environment macro-environment)
    (let ((bindings (source-code (cadr (source-code form-src))))
          (vector (car (cddr (source-code form-src))))
          (body (cdr (cddr (source-code form-src))))
          (vec (generate-symbol "vec")))
      (sourcify-if
        `(let ((,vec ,vector))
           (let ,(map (lambda (variable rank)
                        `(,variable (vector-ref ,vec ,rank)))
                        bindings
                        (naturals 0 (length bindings)))
             ,@body))
        form-src)))))
