;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Object Syntax
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


(unit protected core.class.syntax.object


;;;
;;;; Object
;;;


(cond-expand
  (gambit
    (define %%object-content
      0))
  
  (else
   (define %%object-marker
     'jazz:object)
   
   (define %%object-content
     1)))


(define jazz:object-class
  %%object-content)


(define jazz:object-size
  (%%fx+ jazz:object-class 1))


(cond-expand
  (gambit
    (jazz:define-macro (%%object? expr)
      `(%%jazz? ,expr))
    
    (jazz:define-macro (%%object class . rest)
      (jazz:with-uniqueness class
        (lambda (cls)
          `(%%jazzify (%%vector ,cls ,@rest)))))
    
    (define (jazz:new-object class . rest)
      (let ((obj (%%list->vector (%%cons class rest))))
        (%%jazzify obj)
        obj))
    
    (jazz:define-macro (%%make-object class size)
      (jazz:with-uniqueness class
        (lambda (cls)
          (let ((obj (jazz:generate-symbol "obj")))
            `(let ((,obj (%%jazzify (%%make-vector ,size (%%unspecified)))))
               (%%set-object-class ,obj ,cls)
               ,obj)))))
    
    (jazz:define-macro (%%object-length object)
      (if jazz:debug-core?
          (jazz:with-uniqueness object
            (lambda (obj)
              `(%%core-assertion (%%object? ,obj) (jazz:not-object-error ,obj)
                 (##vector-length ,obj))))
        `(##vector-length ,object)))
    
    (jazz:define-macro (%%object-ref object n)
      (if jazz:debug-core?
          (jazz:with-uniqueness object
            (lambda (obj)
              (jazz:with-uniqueness n
                (lambda (rnk)
                  `(%%core-assertion (%%object? ,obj) (jazz:not-object-error ,obj)
                     (##vector-ref ,obj ,rnk)
                     #; ;; costly test for a very low probability class of bugs
                     (%%core-assertion (%%fx< ,rnk (##vector-length ,obj)) (jazz:outside-object-error ,obj ,rnk)
                       (##vector-ref ,obj ,rnk)))))))
        `(##vector-ref ,object ,n)))
    
    (jazz:define-syntax %%object-set!
      (lambda (src)
        (let ((object (%%cadr (%%source-code src)))
              (n (%%car (%%cddr (%%source-code src))))
              (value (%%cadr (%%cddr (%%source-code src)))))
          (if jazz:debug-core?
              (jazz:with-uniqueness object
                (lambda (obj)
                  (jazz:with-uniqueness n
                    (lambda (rnk)
                      `(%%core-assertion (%%object? ,obj) (jazz:not-object-error ,obj)
                         (##vector-set! ,obj ,rnk ,value)
                         #; ;; costly test for a very low probability class of bugs
                         (%%core-assertion (%%fx< ,rnk (##vector-length ,obj)) (jazz:outside-object-error ,obj ,rnk)
                           (##vector-set! ,obj ,rnk ,value)))))))
            `(##vector-set! ,object ,n ,value)))))
    
    (jazz:define-macro (%%assert-class object class . body)
      `(%%core-assertion (jazz:object-of-class? ,object ,class) (jazz:expected-error ,class ,object)
         ,@body)))
  
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
     `(%%vector-length ,vector))
   
   (jazz:define-macro (%%object-ref vector n)
     `(%%vector-ref ,vector ,n))
   
   (jazz:define-macro (%%object-set! vector n value)
     `(%%vector-set! ,vector ,n ,value))))


(jazz:define-macro (%%get-object-class object)
  `(%%object-ref ,object ,jazz:object-class))


(jazz:define-macro (%%set-object-class object class)
  `(%%object-set! ,object ,jazz:object-class ,class)))
