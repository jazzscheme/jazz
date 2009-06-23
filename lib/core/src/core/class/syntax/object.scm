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


(module protected core.class.syntax.object


;; should be turned automatically off in production level
(define jazz.instances-statistics?
  #t)


(define jazz.instances-statistics
  (if jazz.instances-statistics?
      (%%make-table test: eq?)
    #f))


(define (jazz.register-instance class obj)
  (let ((keep (jazz.keep-instances-statistics)))
    (if keep
        (let ((name (if class (##vector-ref class 1) #f)))
          (case keep
            ((count)
             (%%table-set! jazz.instances-statistics name
               (%%fx+ (%%table-ref jazz.instances-statistics name 0)
                      1)))
            ((list)
             (%%table-set! jazz.instances-statistics name
               (cons obj (%%table-ref jazz.instances-statistics name '())))))))))


(jazz.define-macro (%%register-instance class obj)
  (if jazz.instances-statistics?
      `(jazz.register-instance ,class ,obj)
    #f))


(cond-expand
  (gambit
    (define %%object-content
      0))
  
  (else
   (define %%object-marker
     'jazz.object)
   
   (define %%object-content
     1)))


(define jazz.object-class
  %%object-content)


(define jazz.object-size
  (%%fx+ jazz.object-class 1))


(cond-expand
  (gambit
    (jazz.define-macro (%%subtype-jazz)
      7)
    
    (jazz.define-macro (%%object? expr)
      `(##jazz? ,expr))
    
    (jazz.define-macro (%%object class . rest)
      (jazz.with-uniqueness class
        (lambda (cls)
          (if jazz.instances-statistics?
              (let ((obj (jazz.generate-symbol "obj")))
                `(let ((,obj (##subtype-set! (##vector ,cls ,@rest) (%%subtype-jazz))))
                   (%%register-instance ,cls ,obj)
                   ,obj))
            `(##subtype-set! (##vector ,cls ,@rest) (%%subtype-jazz))))))
    
    (jazz.define-macro (%%make-object class size)
      (jazz.with-uniqueness class
        (lambda (cls)
          (let ((obj (jazz.generate-symbol "obj")))
            `(let ((,obj (##subtype-set! (%%make-vector ,size) (%%subtype-jazz))))
               (%%set-object-class ,obj ,cls)
               (%%register-instance ,cls ,obj)
               ,obj)))))
    
    (jazz.define-macro (%%object-length object)
      (if jazz.debug-core?
          (jazz.with-uniqueness object
            (lambda (obj)
              `(%%core-assertion (%%object? ,obj) (jazz.not-object-error ,obj)
                 (##vector-length ,obj))))
        `(##vector-length ,object)))
    
    (jazz.define-macro (%%object-ref object n)
      (if jazz.debug-core?
          (jazz.with-uniqueness object
            (lambda (obj)
              (jazz.with-uniqueness n
                (lambda (rnk)
                  `(%%core-assertion (%%object? ,obj) (jazz.not-object-error ,obj)
                     (##vector-ref ,obj ,n)
                     #; ;; costly test for a very low probability class of bugs
                     (%%core-assertion (##fixnum.< ,rnk (##vector-length ,obj)) (jazz.outside-object-error ,obj ,rnk)
                       (##vector-ref ,obj ,n)))))))
        `(##vector-ref ,object ,n)))
    
    (jazz.define-macro (%%object-set! object n value)
      (if jazz.debug-core?
          (jazz.with-uniqueness object
            (lambda (obj)
              (jazz.with-uniqueness n
                (lambda (rnk)
                  `(%%core-assertion (%%object? ,obj) (jazz.not-object-error ,obj)
                     (##vector-set! ,obj ,n ,value)
                     #; ;; costly test for a very low probability class of bugs
                     (%%core-assertion (##fixnum.< ,rnk (##vector-length ,obj)) (jazz.outside-object-error ,obj ,rnk)
                       (##vector-set! ,obj ,n ,value)))))))
        `(##vector-set! ,object ,n ,value))))
  
  (else
   (jazz.define-macro (%%object? expr)
     `(and (%%vector? ,expr)
           (%%fx> (%%object-length ,expr) 0)
           (%%eq? (%%object-ref expr 0) %%object-marker)))
   
   (jazz.define-macro (%%object . rest)
     `(%%vector %%object-marker ,@rest))
   
   (jazz.define-macro (%%make-object size)
     (let ((object (jazz.generate-symbol "object")))
       `(let ((,object (%%make-vector ,size)))
          (%%object-set! ,object 0 %%object-marker)
          ,object)))
   
   (jazz.define-macro (%%object-length vector)
     `(%%vector-length ,vector))
   
   (jazz.define-macro (%%object-ref vector n)
     `(%%vector-ref ,vector ,n))
   
   (jazz.define-macro (%%object-set! vector n value)
     `(%%vector-set! ,vector ,n ,value))))


(jazz.define-macro (%%get-object-slot object slot-offset)
  `(%%object-ref ,object ,slot-offset))


(jazz.define-macro (%%set-object-slot object slot-offset value)
  `(%%object-set! ,object ,slot-offset ,value)))
