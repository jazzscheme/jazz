;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Vector Primitives
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
    (define-macro (%%vector? obj)
      (if (jazz.safe?)
          `(vector? ,obj)
        `(##vector? ,obj)))
   
   (define-macro (%%vector . rest)
     (if (jazz.safe?)
         `(vector ,@rest)
       `(##vector ,@rest)))
   
   (define-macro (%%make-vector size)
     (if (jazz.safe?)
         `(make-vector ,size)
       `(##make-vector ,size)))
   
   (define-macro (%%vector-length vector)
     (if (jazz.safe?)
         `(vector-length ,vector)
       `(##vector-length ,vector)))
   
   (define-macro (%%vector-ref vector n)
     (if (jazz.safe?)
         `(vector-ref ,vector ,n)
       `(##vector-ref ,vector ,n)))
   
   (define-macro (%%vector-set! vector n value)
     (if (jazz.safe?)
         `(vector-set! ,vector ,n ,value)
       `(##vector-set! ,vector ,n ,value))))

  (else
   (define-macro (%%vector? obj)
     `(vector? ,obj))
   
   (define-macro (%%vector . rest)
     `(vector ,@rest))
   
   (define-macro (%%make-vector size)
     `(make-vector ,size))
   
   (define-macro (%%vector-length vector)
     `(vector-length ,vector))
   
   (define-macro (%%vector-ref vector n)
     `(vector-ref ,vector ,n))
   
   (define-macro (%%vector-set! vector n value)
     `(vector-set! ,vector ,n ,value))))
