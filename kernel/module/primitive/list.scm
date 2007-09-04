;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; List Primitives
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


(cond-expand
  (gambit
    (define-macro (%%null? obj)
      (if (jazz.safe?)
          `(null? ,obj)
        `(##null? ,obj)))
    
    (define-macro (%%pair? obj)
      (if (jazz.safe?)
          `(pair? ,obj)
        `(##pair? ,obj)))
    
    (define-macro (%%car pair)
      (if (jazz.safe?)
          `(car ,pair)
        `(##car ,pair)))
    
    (define-macro (%%cdr pair)
      (if (jazz.safe?)
          `(cdr ,pair)
        `(##cdr ,pair)))
    
    (define-macro (%%set-car! pair val)
      (if (jazz.safe?)
          `(set-car! ,pair ,val)
        `(##set-car! ,pair ,val)))
    
    (define-macro (%%set-cdr! pair val)
      (if (jazz.safe?)
          `(set-cdr! ,pair ,val)
        `(##set-cdr! ,pair ,val)))
    
    (define-macro (%%caar pair)
      (if (jazz.safe?)
          `(caar ,pair)
        `(##caar ,pair)))
    
    (define-macro (%%cadr pair)
      (if (jazz.safe?)
          `(cadr ,pair)
        `(##cadr ,pair)))
    
    (define-macro (%%cdar pair)
      (if (jazz.safe?)
          `(cdar ,pair)
        `(##cdar ,pair)))
    
    (define-macro (%%cddr pair)
      (if (jazz.safe?)
          `(cddr ,pair)
        `(##cddr ,pair)))
    
    (define-macro (%%length lst)
      (if (jazz.safe?)
          `(length ,lst)
        `(##length ,lst)))
    
    (define-macro (%%memq obj lst)
      (if (jazz.safe?)
          `(memq ,obj ,lst)
        `(##memq ,obj ,lst)))
    
    (define-macro (%%memv obj lst)
      `(memv ,obj ,lst))
    
    (define-macro (%%cons x y)
      (if (jazz.safe?)
          `(cons ,x ,y)
        `(##cons ,x ,y)))
    
    (define-macro (%%list . rest)
      (if (jazz.safe?)
          `(list ,@rest)
        `(##list ,@rest)))
    
    (define-macro (%%append x y)
      (if (jazz.safe?)
          `(append ,x ,y)
        `(##append ,x ,y)))
    
    (define-macro (%%reverse lst)
      (if (jazz.safe?)
          `(reverse ,lst)
        `(##reverse ,lst))))
  
  (else
   (define-macro (%%memq obj lst)
     `(memq ,obj ,lst))))
