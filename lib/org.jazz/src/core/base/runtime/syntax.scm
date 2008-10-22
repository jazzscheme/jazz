;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Syntax
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


(module core.base.runtime.syntax


(define (jazz.source? expr)
  (%%source? expr))

(define (jazz.source-code expr)
  (%%source-code expr))

(define (jazz.source-locat expr)
  (%%source-locat expr))

(define (jazz.desourcify expr)
  (%%desourcify expr))

(define (jazz.desourcify-list lst)
  (map jazz.desourcify lst))

(define (jazz.sourcify expr src)
  (%%sourcify expr src))

(define (jazz.sourcify-if expr src)
  (if (jazz.source? src)
      (jazz.sourcify expr src)
    expr))

(define (jazz.locat-container locat)
  (%%locat-container locat))

(define (jazz.locat-position locat)
  (%%locat-position locat))

(define (jazz.container->file container)
  (%%container->file container))

(define (jazz.position->filepos position)
  (%%position->filepos position))

(define (jazz.filepos-line filepos)
  (%%filepos-line filepos))

(define (jazz.filepos-col filepos)
  (%%filepos-col filepos))

(define (jazz.locat->file/line/col locat)
  (if locat
      (let ((file (%%container->file (%%locat-container locat))))
        (if file
            (let ((filepos (%%position->filepos (%%locat-position locat))))
              (let ((line (%%filepos-line filepos))
                    (col (%%filepos-col filepos)))
                (list file line col)))
          #f))
    #f))


;;;
;;;; Debug
;;;


(define (jazz.present-source obj)
  
  (define (present-src src)
    (let ((code (##source-code src))
          (pos (##locat-position (##source-locat src))))
      (##vector 'source
                (jazz.present-source code)
                (##fixnum.+ (##filepos-line pos) 1)
                (##fixnum.+ (##filepos-col pos) 1))))

  (define (present-list lst)
    (cond ((##pair? lst)
           (##cons (jazz.present-source (##car lst))
                   (present-list (##cdr lst))))
          ((##null? lst)
           '())
          (else
           (jazz.present-source lst))))

  (define (present-vector vect)
    (let* ((len (##vector-length vect))
           (x (##make-vector len 0)))
      (let loop ((i (##fixnum.- len 1)))
        (if (##fixnum.< i 0)
            x
          (begin
            (##vector-set! x i (jazz.present-source (##vector-ref vect i)))
            (loop (##fixnum.- i 1)))))))

  (cond ((##source? obj)
         (present-src obj))
        ((##pair? obj)
         (present-list obj))
        ((##vector? obj)
         (present-vector obj))
        (else
         obj))))
