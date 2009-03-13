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


(module jazz.dialect.core.memory


(define (jazz.gc)
  (%%gc))


(define (jazz.process-statistics)
  (##process-statistics))


(define (jazz.symbols-statistics)
  (let ((count 0)
        (chars 0))
    (for-each (lambda (lst)
                (set! count (+ count (length lst)))
                (for-each (lambda (s)
                            (set! chars (+ chars (string-length (symbol->string s)))))
                          lst))
              (map (lambda (s)
                     (let loop ((s s) (lst '()))
                          (if (symbol? s)
                              (loop (##vector-ref s 2) (cons s lst))
                            (reverse lst))))
                   (vector->list (##symbol-table))))
    (list count chars)))


(define (jazz.classes-statistics)
  (let ((word-bytes 4)
        (f64-bytes 8)
        (pair-bytes 12)
        (table-bytes 32)
        (nb-classes 0) (sz-classes 0)
        (nb-interfaces 0) (sz-interfaces 0)
        (nb-slots 0) (sz-slots 0)
        (nb-methods 0) (sz-methods 0))
    (define (vector-size v)
      (+ word-bytes (* word-bytes (##vector-length v))))
    
    (define (safe-vector-size v)
      (if (vector? v)
          (vector-size v)
        0))
    
    (define (f64vector-size v)
      (+ word-bytes (* f64-bytes (f64vector-length v))))
    
    (define (vector-vector-size v)
      (if (not v)
          0
        (+ (vector-size v)
           (let iter ((n 0) (size 0))
                (if (< n (vector-length v))
                    (let ((v (vector-ref v n)))
                      (iter (+ n 1) (+ size (if v (vector-size v) 0))))
                  size)))))
    
    (define (list-size l)
      (* pair-bytes (length l)))
    
    (define (table-size t)
      (+ table-bytes
         (safe-vector-size (##vector-ref t 3))
         (f64vector-size (##vector-ref t 4))
         (safe-vector-size (##vector-ref t 5))))
    
    (define (process-class class)
      (set! nb-classes (+ nb-classes 1))
      (set! sz-classes (+ sz-classes (+ (vector-size class)
                                        (table-size (%%get-category-fields class))
                                        (vector-size (%%get-category-ancestors class))
                                        (list-size (%%get-category-descendants class))
                                        (list-size (%%get-class-interfaces class))
                                        (list-size (%%get-class-slots class))
                                        (vector-size (%%get-class-core-vtable class))
                                        (vector-vector-size (%%get-class-class-table class))
                                        (vector-vector-size (%%get-class-interface-table class)))))
      (%%iterate-table (%%get-category-fields class)
        (lambda (name field)
          (cond ((jazz.is? field jazz.Slot) (process-slot field))
                ((jazz.is? field jazz.Method) (process-method field)))))
      (for-each process-class (%%get-category-descendants class)))
    
    (define (process-slot slot)
      (set! nb-slots (+ nb-slots 1))
      (set! sz-slots (+ sz-slots (vector-size slot))))
    
    (define (process-method method)
      (set! nb-methods (+ nb-methods 1))
      (set! sz-methods (+ sz-methods (vector-size method))))
    
    (process-class jazz.Object)
    
    (values
      nb-classes sz-classes
      nb-interfaces sz-interfaces
      nb-slots sz-slots
      nb-methods sz-methods))))
