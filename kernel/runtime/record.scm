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
;;;  Portions created by the Initial Developer are Copyright (C) 1996-2015
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


(define jazz:structure-offset
  0)

(define jazz:structure-marker
  (list 'structure))

(define jazz:record-offset
  (%%fx+ jazz:structure-offset 1))

(jazz:define-macro (%%subtype-jazz)
  7)

(jazz:define-macro (%%subtype-jazzstruct)
  16)

;; this function is here for structure / class unification
(jazz:define-macro (%%jazz? expr)
  (jazz:with-uniqueness expr
    (lambda (expr)
      (if (##global-var? '##jazzstruct?)
          `(or (##jazz? ,expr) (##jazzstruct? ,expr))
        `(##jazz? ,expr)))))

;; this function is here for structure / class unification
(jazz:define-macro (%%jazzstruct? expr)
  (if (##global-var? '##jazzstruct?)
      `(##jazzstruct? ,expr)
    #f))

;; this function is here for structure / class unification
(jazz:define-macro (%%jazzify expr)
  `(##subtype-set! ,expr (%%subtype-jazz)))

;; this function is here for structure / class unification
(jazz:define-macro (%%jazzstructify expr)
  `(##subtype-set! ,expr (%%subtype-jazzstruct)))

#; ;; defined as functions until structure / class unification
(jazz:define-macro (%%record? expr)
  `(or (%%jazz? ,expr)
       (and (%%vector? ,expr)
            (%%fx> (%%vector-length ,expr) 0)
            (%%symbol? (%%vector-ref ,expr 0)))))

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
  `(%%vector-length ,record))

(jazz:define-macro (%%record-ref record n)
  (if jazz:debug-core?
      (jazz:with-uniqueness record
        (lambda (rec)
          `(if (%%not (%%record? ,rec))
               (jazz:not-record-error ,rec)
             (%%vector-ref ,rec ,n))))
    `(%%vector-ref ,record ,n)))

(jazz:define-macro (%%record-set! record n value)
  (if jazz:debug-core?
      (jazz:with-uniqueness record
        (lambda (rec)
          `(if (%%not (%%record? ,rec))
               (jazz:not-record-error ,rec)
             (%%vector-set! ,rec ,n ,value))))
    `(%%vector-set! ,record ,n ,value)))


(jazz:define-macro (%%get-record-structure record)
  `(%%record-ref ,record ,jazz:structure-offset))

(jazz:define-macro (%%set-record-structure record structure)
  `(%%record-set! ,record ,jazz:structure-offset ,structure))


(define (jazz:jazz? obj)
  (%%jazz? obj))

(define (jazz:jazzify obj)
  (%%jazzify obj))

(define (jazz:jazzstruct? obj)
  (%%jazzstruct? obj))

(define (jazz:jazzstructify obj)
  (%%jazzstructify obj))


;; defined as functions until structure / class unification
(define (%%record-structure? expr)
  (and (%%pair? expr)
       ;; eq? doesn't work probably due to records read by the reader
       (%%equal? (%%cdr expr) jazz:structure-marker)))


;; defined as functions until structure / class unification
(define (%%make-record-structure name)
  (%%cons name jazz:structure-marker))


(jazz:define-macro (%%record-structure-name expr)
  `(%%car (%%get-record-structure ,expr)))


;; defined as functions until structure / class unification
(define (%%record? expr)
  (or (%%jazz? expr)
      (and (%%vector? expr)
           (%%fx> (%%vector-length expr) 0)
           (%%record-structure? (%%vector-ref expr 0)))))


(jazz:define-macro (%%record name . rest)
  `(%%vector (%%make-record-structure ,name) ,@rest))


(jazz:define-macro (%%class-record class . rest)
  `(%%jazzify (%%vector ,class ,@rest)))


(jazz:define-macro (%%make-record name size)
  (let ((record (jazz:generate-symbol "record")))
    `(let ((,record (%%make-vector ,size (%%unspecified))))
       (%%set-record-structure ,record (%%make-record-structure ,name))
       ,record)))


(define (jazz:not-record-error obj)
  (jazz:error "Jazz record expected: {s}" obj))


(define (jazz:outside-record-error obj rnk)
  (jazz:error "Invalid access to record outside its bounds: {s}" obj))


(define (jazz:record->vector record)
  (let* ((size (%%record-length record))
         (content (%%make-vector size)))
    (let iter ((n 0))
         (if (%%fx< n size)
             (begin
               (%%vector-set! content n (%%record-ref record n))
               (iter (%%fx+ n 1)))))
    content)))
