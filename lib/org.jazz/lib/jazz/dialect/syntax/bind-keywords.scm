;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Bind-keywords
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


(library jazz.dialect.syntax.bind-keywords scheme


(import (jazz.dialect.kernel.boot))


; @macro (bind-keywords ((a 2) (b 3)) rest (list b: 5))
; @expansion
; (let ((r (box-list rest)))
;   (let ((a (find-keyword a: r (lambda () 2)))
;         (b (find-keyword b: r (lambda () 3))))
;     (if (unbox-list r)
;         (error "Unexpected keywords: {s}" (unbox-list r)))
;     (list b: 5)))


(define-macro (bind-keywords bindings rest . body)
  (let ((box (generate-symbol "box"))
        (bnd (new-queue))
        (oth (last-tail bindings)))
    `(let ((,box (box-list ,rest)))
       (let* ,(map (lambda (binding)
                     (let* ((variable (car binding))
                            (specific (binding-specifier binding))
                            (default (if specific (caddr binding) (cadr binding))))
                       (if specific
                           `(,variable ,specific (find-keyword ',(string->keyword (symbol->string variable)) ,box (lambda () ,default))))
                       `(,variable (find-keyword ',(string->keyword (symbol->string variable)) ,box (lambda () ,default)))))
                   (proper-list bindings))
         ,@(if (symbol? oth)
               `((let ((,oth (unbox-list ,box)))
                   ,@body))
             `((if (not-null? (unbox-list ,box))
                   (error "Unexpected keywords: {s}" (unbox-list ,box)))
               ,@body)))))))
