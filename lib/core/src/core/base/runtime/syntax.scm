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


(unit protected core.base.runtime.syntax


(define (jazz.source? obj)
  (%%source? obj))


(define (jazz.source-code expr)
  (if (%%source? expr)
      (%%source-code expr)
    expr))


(define (jazz.source-locat expr)
  (if (%%source? expr)
      (%%source-locat expr)
    #f))


(define (jazz.desourcify expr)
  (%%desourcify expr))


(define (jazz.desourcify-all expr)
  
  (define (desourcify-source src)
    (desourcify-all (%%source-code src)))

  (define (desourcify-list lst)
    (cond ((%%pair? lst)
           (%%cons (desourcify-all (%%car lst))
                   (desourcify-list (%%cdr lst))))
          ((%%null? lst)
           '())
          (else
           (desourcify-all lst))))

  (define (desourcify-vector vect)
    (let* ((len (%%vector-length vect))
           (x (%%make-vector len 0)))
      (let loop ((i (%%fx- len 1)))
        (if (%%fx< i 0)
          x
          (begin
            (%%vector-set! x i (desourcify-all (%%vector-ref vect i)))
            (loop (%%fx- i 1)))))))
  
  (define (desourcify-box box)
    (%%box (desourcify-all (%%unbox box))))
  
  (define (desourcify-all expr)
    (cond ((%%source? expr)
           (desourcify-source expr))
          ((%%pair? expr)
           (desourcify-list expr))
          ((%%vector? expr)
           (desourcify-vector expr))
          ((%%box? expr)
           (desourcify-box expr))
          (else
           expr)))
  
  (desourcify-all expr))


(define (jazz.sourcify expr src)
  (%%sourcify expr src))


(define (jazz.sourcify-if expr src)
  (if (jazz.source? src)
      (jazz.sourcify expr src)
    expr))


(define (jazz.sourcify-list lst src)
  (map (lambda (expr)
         (jazz.sourcify-if (jazz.desourcify-all expr) src))
       lst))


(define (jazz.locat-container locat)
  (%%locat-container locat))


(define (jazz.locat-position locat)
  (%%locat-position locat))


(define (jazz.locat->file/line/col locat)
  (let ((file (and locat (%%container->path (%%locat-container locat)))))
    (if file
        (let ((filepos (%%position->filepos (%%locat-position locat))))
          (let ((line (%%filepos-line filepos))
                (col (%%filepos-col filepos)))
            (%%list file line col)))
      #f)))


(define (jazz.container->path container)
  (%%container->path container))


(define (jazz.position->filepos position)
  (%%position->filepos position))


(define (jazz.filepos-line filepos)
  (%%filepos-line filepos))


(define (jazz.filepos-col filepos)
  (%%filepos-col filepos))


;;;
;;;; Emit
;;;


(define (jazz.save-emit-if emit)
  (%%when (and (jazz.save-emit?) (jazz.compiled-source))
    (parameterize ((current-readtable jazz.scheme-readtable))
      (call-with-output-file (list path: (jazz.binary-with-extension (jazz.compiled-source) ".scm") eol-encoding: (jazz.platform-eol-encoding jazz.kernel-platform))
        (lambda (port)
          (pretty-print (jazz.present-source emit) port))))))


;;;
;;;; Debug
;;;


(define (jazz.present-source obj)
  
  (define (present-src src)
    (let ((code (jazz.source-code src))
          (pos (%%locat-position (%%source-locat src))))
      (%%vector 'source
                (jazz.present-source code)
                (%%fx+ (%%filepos-line pos) 1)
                (%%fx+ (%%filepos-col pos) 1))))

  (define (present-list lst)
    (cond ((%%pair? lst)
           (%%cons (jazz.present-source (%%car lst))
                   (present-list (%%cdr lst))))
          ((%%null? lst)
           '())
          (else
           (jazz.present-source lst))))

  (define (present-vector vect)
    (let* ((len (%%vector-length vect))
           (x (%%make-vector len 0)))
      (let loop ((i (%%fx- len 1)))
        (if (%%fx< i 0)
            x
          (begin
            (%%vector-set! x i (jazz.present-source (%%vector-ref vect i)))
            (loop (%%fx- i 1)))))))

  (cond ((%%source? obj)
         (present-src obj))
        ((%%pair? obj)
         (present-list obj))
        ((%%vector? obj)
         (present-vector obj))
        (else
         obj))))
