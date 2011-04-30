;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Objects
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


(block kernel.object


(jazz:kernel-declares)


;;;
;;;; Statistics
;;;


;; should be turned automatically off in production level
(define jazz:instances-statistics?
  #t)


(define jazz:instances-statistics
  (if jazz:instances-statistics?
      (%%make-table test: eq?)
    #f))


(define (jazz:register-instance class obj)
  (let ((keep (jazz:keep-instances-statistics)))
    (if keep
        (let ((name (if class (##vector-ref class 1) #f)))
          (case keep
            ((count)
             (%%table-set! jazz:instances-statistics name
               (%%fx+ (%%table-ref jazz:instances-statistics name 0)
                      1)))
            ((list)
             (%%table-set! jazz:instances-statistics name
               (%%cons obj (%%table-ref jazz:instances-statistics name '())))))))))


(jazz:define-macro (%%register-instance class obj)
  (if jazz:instances-statistics?
      `(jazz:register-instance ,class ,obj)
    #f))


;;;
;;;; Object
;;;


(define jazz:record-descriptor
  0)


(define jazz:record-size
  (%%fx+ jazz:record-descriptor 1))


(jazz:define-macro (%%get-record-descriptor record)
  `(%%record-ref ,record ,jazz:record-descriptor))

(jazz:define-macro (%%set-record-descriptor record descriptor)
  `(%%record-set! ,record ,jazz:record-descriptor ,descriptor))


(cond-expand
  (gambit
    (jazz:define-macro (%%subtype-jazz)
      7)
    
    (jazz:define-macro (%%object? expr)
      `(##jazz? ,expr))
    
    (jazz:define-macro (%%object class . rest)
      (jazz:with-uniqueness class
        (lambda (cls)
          (if jazz:instances-statistics?
              (let ((obj (jazz:generate-symbol "obj")))
                `(let ((,obj (##subtype-set! (%%vector ,cls ,@rest) (%%subtype-jazz))))
                   (%%register-instance ,cls ,obj)
                   ,obj))
            `(##subtype-set! (%%vector ,cls ,@rest) (%%subtype-jazz))))))
    
    (define (jazz:new-object class . rest)
      (let ((obj (%%list->vector (%%cons class rest))))
        (##subtype-set! obj (%%subtype-jazz))
        obj))
    
    (jazz:define-macro (%%make-object class size)
      (jazz:with-uniqueness class
        (lambda (cls)
          (let ((obj (jazz:generate-symbol "obj")))
            `(let ((,obj (##subtype-set! (%%make-vector ,size (%%unspecified)) (%%subtype-jazz))))
               (%%set-object-class ,obj ,cls)
               (%%register-instance ,cls ,obj)
               ,obj)))))
    
    (jazz:define-macro (%%object-length object)
      `(##vector-length ,object))
    
    (jazz:define-macro (%%object-ref object n)
      `(##vector-ref ,object ,n))
    
    (jazz:define-syntax %%object-set!
      (lambda (src)
        (let ((object (%%cadr (%%source-code src)))
              (n (%%car (%%cddr (%%source-code src))))
              (value (%%cadr (%%cddr (%%source-code src)))))
          `(##vector-set! ,object ,n ,value))))
    
    (define (jazz:object->vector object)
      (let* ((size (%%object-length object))
             (content (%%make-vector size)))
        (let iter ((n 0))
             (if (%%fx< n size)
                 (begin
                   (%%vector-set! content n (%%object-ref object n))
                   (iter (%%fx+ n 1)))))
        content)))
  
  (else
   (jazz:define-macro (%%object? expr)
     `(and (%%vector? ,expr)
           (%%fx> (%%object-length ,expr) 0)
           (%%eq? (%%object-ref expr 0) %%object-marker)))
   
   (jazz:define-macro (%%object . rest)
     `(%%vector %%object-marker ,@rest))
   
   (jazz:define-macro (%%make-object size)
     (let ((object (jazz:generate-symbol "object")))
       `(let ((,object (%%make-vector ,size (%%unspecified))))
          (%%object-set! ,object 0 %%object-marker)
          ,object)))
   
   (jazz:define-macro (%%object-length vector)
     `(##vector-length ,vector))
   
   (jazz:define-macro (%%object-ref vector n)
     `(##vector-ref ,vector ,n))
   
   (jazz:define-macro (%%object-set! vector n value)
     `(##vector-set! ,vector ,n ,value))))


(jazz:define-macro (%%record-ref record n)
  `(##vector-ref ,record ,n))

(jazz:define-macro (%%record-set! record n value)
  `(##vector-set! ,record ,n ,value)))
