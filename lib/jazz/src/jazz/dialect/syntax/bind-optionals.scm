;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Bind-optionals
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


(library protected jazz.dialect.syntax.bind-optionals scheme


(import (jazz.dialect.kernel.boot))


; @syntax (bind-optionals ((a 2)) rest a)
; @expansion (let ((a (if (not-null? rest) (car rest) 2))) a)

; @syntax (bind-optionals ((a 2) (b 3)) rest (list a b))
; @expansion
; (let ((a 2) (b 3) (__scan rest))
;   (if (not-null? __scan)
;       (begin
;         (set! a (car __scan))
;         (set! __scan (cdr __scan))))
;   (if (not-null? __scan)
;       (begin
;         (set! b (car __scan))
;         (set! __scan (cdr __scan))))
;   (list a b))


(syntax public (bind-optionals form-src)
  (let ((bindings (source-code (cadr (source-code form-src))))
        (rest (car (cddr (source-code form-src))))
        (body (cdr (cddr (source-code form-src))))
        (scan (generate-symbol "scan"))
        (prog (generate-symbol "prog")))
    (sourcify-if
      `(let ((,scan ,rest))
         (let* ,(map (lambda (binding)
                       (let* ((variable (source-code (car (source-code binding))))
                              (specifier (binding-specifier binding))
                              (default (if specifier (caddr (source-code binding)) (cadr (source-code binding))))
                              (value `(if (null? ,scan) ,default (let ((,prog (car ,scan)))
                                                                   (set! ,scan (cdr ,scan))
                                                                   ,prog))))
                         (if specifier
                             `(,variable ,specifier ,value)
                           `(,variable ,value))))
                     (proper-list bindings))
           (if (not-null? ,scan)
               (error "Too many arguments for bind-optionals"))
           ,@body))
      form-src))))
