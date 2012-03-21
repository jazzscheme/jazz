;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Records
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


(block kernel.record


(jazz:kernel-declares)


(cond-expand
  (gambit
    (define jazz:structure-offset
      0)
    
    (define jazz:structure-marker
      (list 'structure))
    
    (define jazz:record-offset
      (%%fx+ jazz:structure-offset 1))
    
    (jazz:define-macro (%%subtype-jazz)
      7)
    
    ;; this function is here for structure / class unification
    (jazz:define-macro (%%jazz? expr)
      `(##jazz? ,expr))
    
    ;; this function is here for structure / class unification
    (jazz:define-macro (%%jazzify expr)
      `(##subtype-set! ,expr (%%subtype-jazz)))
    
    #; ;; defined as functions until structure / class unification
    (jazz:define-macro (%%record? expr)
      `(and (##vector? ,expr)
            (##fx> (##vector-length ,expr) 0)
            (##symbol? (##vector-ref ,expr 0))))
    
    #; ;; defined as functions until structure / class unification
    (jazz:define-macro (%%record structure . rest)
      `(%%jazzify (%%vector ,structure ,@rest)))
    
    #; ;; defined as functions until structure / class unification
    (jazz:define-macro (%%make-record structure size)
      (let ((record (jazz:generate-symbol "record")))
        `(let ((,record (%%jazzify (%%make-vector ,size (%%unspecified)))))
           (%%set-record-structure ,record ,structure)
           ,record)))
    
    (jazz:define-macro (%%record-length record)
      `(##vector-length ,record))
    
    (jazz:define-macro (%%record-ref record n)
      `(##vector-ref ,record ,n))
    
    (jazz:define-macro (%%record-set! record n value)
      `(##vector-set! ,record ,n ,value)))
  
  (else
    (define %%record-marker
      'record)
    
    (define jazz:marker-offset
      0)
    
    (define jazz:structure-offset
      (%%fx+ jazz:marker-offset 1))
    
    (define jazz:record-offset
      (%%fx+ jazz:structure-offset 1))
    
    (jazz:define-macro (%%record? expr)
      `(and (%%vector? ,expr)
            (%%fx> (%%record-length ,expr) 0)
            (%%eq? (%%record-ref expr 0) %%record-marker)))
    
    (jazz:define-macro (%%record structure . rest)
      `(%%vector %%record-marker structure ,@rest))
    
    (jazz:define-macro (%%make-record structure size)
      (let ((record (jazz:generate-symbol "record")))
        `(let ((,record (%%make-vector ,size (%%unspecified))))
           (%%record-set! ,record jazz:marker-offset %%record-marker)
           (%%set-record-structure ,record ,structure)
           ,record)))
    
    (jazz:define-macro (%%record-length record)
      `(##vector-length ,record))
    
    (jazz:define-macro (%%record-ref record n)
      `(##vector-ref ,record ,n))
    
    (jazz:define-macro (%%record-set! record n value)
      `(##vector-set! ,record ,n ,value))))


(jazz:define-macro (%%get-record-structure record)
  `(%%record-ref ,record ,jazz:structure-offset))

(jazz:define-macro (%%set-record-structure record structure)
  `(%%record-set! ,record ,jazz:structure-offset ,structure))


;; defined as functions until structure / class unification
(define (%%record-structure? expr)
  (and (##pair? expr)
       (##eq? (##cdr expr) jazz:structure-marker)))


;; defined as functions until structure / class unification
(define (%%make-record-structure name)
  (%%cons name jazz:structure-marker))


;; defined as functions until structure / class unification
(define (%%record? expr)
  (and (##vector? expr)
       (##fx> (##vector-length expr) 0)
       (%%record-structure? (##vector-ref expr 0))))


(jazz:define-macro (%%record name . rest)
  `(%%vector (%%make-record-structure ,name) ,@rest))


(jazz:define-macro (%%class-record class . rest)
  `(%%jazzify (%%vector ,class ,@rest)))


(jazz:define-macro (%%make-record name size)
  (let ((record (jazz:generate-symbol "record")))
    `(let ((,record (%%make-vector ,size (%%unspecified))))
       (%%set-record-structure ,record (%%make-record-structure ,name))
       ,record)))


(define (jazz:record->vector record)
  (let* ((size (%%record-length record))
         (content (%%make-vector size)))
    (let iter ((n 0))
         (if (%%fx< n size)
             (begin
               (%%vector-set! content n (%%record-ref record n))
               (iter (%%fx+ n 1)))))
    content)))
