;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Kernel 1
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


(library jazz.dialect.kernel.kernel1 scheme


(export (jazz.dialect.kernel.kernel0))
(import (jazz.dialect.kernel.kernel0))


(define false
  #f)

(define true
  #t)


(define nil
  #f)


(define null
  '())


;;;
;;;; List
;;;


(macro (%%car obj)
  `(car ,obj))

(macro (%%cdr obj)
  `(cdr ,obj))


(macro (%%set-car! obj val)
  `(set-car! ,obj ,val))

(macro (%%set-cdr! obj val)
  `(set-cdr! ,obj ,val))


(define (car obj)
  (if (scheme.null? obj)
      null
    (scheme.car obj)))


(define (cdr obj)
  (if (scheme.null? obj)
      null
    (scheme.cdr obj))))
