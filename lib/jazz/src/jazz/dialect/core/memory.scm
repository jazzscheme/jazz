;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Memory
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


(module protected jazz.dialect.core.memory


(define (jazz.gc)
  (%%gc))


;;;
;;;; Memory
;;;


(define (jazz.process-memory)
  (let ((vec (##process-statistics)))
    (let ((heap       (f64vector-ref vec 15))
          (alloc      (f64vector-ref vec 16))
          (live       (f64vector-ref vec 17))
          (movable    (f64vector-ref vec 18))
          (nonmovable (f64vector-ref vec 19)))
      (values (inexact->exact heap)
              (inexact->exact live)
              (inexact->exact movable)
              (inexact->exact nonmovable)))))


(define (jazz.symbols-memory)
  (let ((count 0)
        (chars 0))
    (for-each (lambda (lst)
                (set! count (%%fx+ count (%%length lst)))
                (for-each (lambda (s)
                            (set! chars (%%fx+ chars (%%string-length (%%symbol->string s)))))
                          lst))
              (map (lambda (s)
                     (let loop ((s s) (lst '()))
                          (if (%%symbol? s)
                              (loop (%%vector-ref s 2) (%%cons s lst))
                            (%%reverse lst))))
                   (%%vector->list (##symbol-table))))
    (values count chars)))


;;;
;;;; Classes
;;;


(define (jazz.classes-statistics)
  (let ((nb-classes 0) (sz-classes 0)
        (nb-interfaces 0) (sz-interfaces 0)
        (nb-slots 0) (sz-slots 0)
        (nb-methods 0) (sz-methods 0))
    (define (process-class class)
      (set! nb-classes (%%fx+ nb-classes 1))
      (set! sz-classes (%%fx+ sz-classes (fx+ (jazz.vector-size class)
                                              (jazz.table-size (%%get-category-fields class))
                                              (jazz.vector-size (%%get-category-ancestors class))
                                              (jazz.list-size (%%get-category-descendants class))
                                              (jazz.list-size (%%get-class-interfaces class))
                                              (jazz.list-size (%%get-class-instance-slots class))
                                              (jazz.vector-size (%%get-class-core-vtable class))
                                              (jazz.vector-vector-size (%%get-class-class-table class))
                                              (jazz.vector-vector-size (%%get-class-interface-table class)))))
      (%%iterate-table (%%get-category-fields class)
        (lambda (name field)
          (cond ((jazz.is? field jazz.Slot) (process-slot field))
                ((jazz.is? field jazz.Method) (process-method field)))))
      (for-each process-class (%%get-category-descendants class)))
    
    (define (process-slot slot)
      (set! nb-slots (%%fx+ nb-slots 1))
      (set! sz-slots (%%fx+ sz-slots (jazz.vector-size slot))))
    
    (define (process-method method)
      (set! nb-methods (%%fx+ nb-methods 1))
      (set! sz-methods (%%fx+ sz-methods (jazz.vector-size method))))
    
    (process-class jazz.Object)
    
    (values
      nb-classes sz-classes
      nb-interfaces sz-interfaces
      nb-slots sz-slots
      nb-methods sz-methods)))


;;;
;;;; Instances
;;;


(define (jazz.class-instances-count class)
  (let ((count 0))
    (let iter ((class class))
      (set! count (%%fx+ count (%%table-ref jazz.instances-statistics (%%get-category-name class) 0)))
      (for-each iter (%%get-category-descendants class)))
    count))


(define (jazz.class-instances-size class)
  (let ((size 0))
    (let iter ((class class))
      (set! size (%%fx+ size (%%fx* (%%table-ref jazz.instances-statistics (%%get-category-name class) 0)
                                    (%%get-class-instance-size class))))
      (for-each iter (%%get-category-descendants class)))
    size))


;;;
;;;; Sizes
;;;


(define jazz.word-bytes   4)
(define jazz.f64-bytes    8)
(define jazz.pair-bytes  12)
(define jazz.table-bytes 32)


(define (jazz.vector-size v)
  (%%fx+ jazz.word-bytes (%%fx* jazz.word-bytes (%%vector-length v))))

(define (jazz.safe-vector-size v)
  (if (%%vector? v)
      (jazz.vector-size v)
    0))

(define (jazz.f64vector-size v)
  (%%fx+ jazz.word-bytes (%%fx* jazz.f64-bytes (f64vector-length v))))

(define (jazz.vector-vector-size v)
  (if (%%not v)
      0
    (%%fx+ (jazz.vector-size v)
           (let iter ((n 0) (size 0))
                (if (%%fx< n (%%vector-length v))
                    (let ((v (%%vector-ref v n)))
                      (iter (%%fx+ n 1) (%%fx+ size (if v (jazz.vector-size v) 0))))
                  size)))))

(define (jazz.list-size l)
  (%%fx* jazz.pair-bytes (%%length l)))

(define (jazz.table-size t)
  (let ((gcht1 (%%vector-ref t 3))
        (gcht2 (%%vector-ref t 5))
        (floats (%%vector-ref t 4)))
    (%%fx* jazz.word-bytes
           (fx+ 1 (%%vector-length t)
             ;; 1 (%%vector-length floats)
             (if (##gc-hash-table? gcht1) (%%fx+ 1 (%%vector-length gcht1)) 0)
             (if (##gc-hash-table? gcht2) (%%fx+ 1 (%%vector-length gcht2)) 0))))))
