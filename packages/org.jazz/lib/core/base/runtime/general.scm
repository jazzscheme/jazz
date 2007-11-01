;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; General
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


(module core.base.runtime.general


(define (jazz.boolean expr)
  (%%boolean expr))


(define (jazz.not-null? expr)
  (%%not-null? expr))


;;;
;;;; List
;;;


(define (jazz.find-in proc lst)
  (let iter ((scan lst))
    (if (%%null? scan)
        #f
      (or (proc (%%car scan))
          (iter (%%cdr scan))))))


(define (jazz.find-if predicate lst)
  (let iter ((scan lst))
    (if (%%null? scan)
        #f
      (let ((value (%%car scan)))
        (if (predicate value)
            value
          (iter (%%cdr scan)))))))


(define (jazz.collect-if predicate lst)
  (let ((result '()))
    (let iter ((scan lst))
      (if (%%not-null? scan)
          (begin
            (iter (%%cdr scan))
            (let ((value (%%car scan)))
              (if (predicate value)
                  (set! result (%%cons value result)))))))
    result))


(define (jazz.find-char-reversed c str)
  (let iter ((pos (%%fx- (%%string-length str) 1)))
    (cond ((%%fx< pos 0)
           #f)
          ((char=? (%%string-ref str pos) c)
           pos)
          (else
           (iter (%%fx- pos 1))))))


(define (jazz.getprop plist target)
  (let iter ((scan plist))
    (cond ((%%null? scan)
           #f)
          ((%%eqv? (%%car scan) target)
           scan)
          (else
           (iter (%%cddr scan))))))


(define (jazz.getf plist target #!optional (not-found #f))
  (let ((pair (jazz.getprop plist target)))
    (if pair
        (%%cadr pair)
      not-found)))


(define (jazz.simplify-begin form)
  (if (and (%%pair? form)
           (%%eq? (%%car form) 'begin)
           (%%pair? (%%cdr form))
           (%%null? (%%cddr form)))
      (%%cadr form)
    form))


(define jazz.reverse!
  reverse)


(define (jazz.list-copy lst)
  (map (lambda (obj) obj) lst))


(define (jazz.last-pair lst)
  (%%while (and (%%pair? lst) (%%not-null? (%%cdr lst)))
    (set! lst (%%cdr lst)))
  lst)


(define (jazz.last lst)
  (%%car (jazz.last-pair lst)))


(define (jazz.remove-duplicates lst)
  (let ((result '()))
    (let iter ((scan lst))
      (if (%%not (%%null? scan))
          (begin
            (iter (%%cdr scan))
            (let ((value (%%car scan)))
              (if (%%not (%%memv value result))
                  (set! result (%%cons value result)))))))
    result))


(define (jazz.partition lst key)
  (let iter ((scan lst))
    (if (null? scan)
        '()
      (let* ((partition (iter (cdr scan)))
             (element (car scan))
             (category (key element))
             (set (assv category partition)))
        (if (not set)
            (cons (cons category (list element)) partition)
          (begin
            (set-cdr! set (cons element (cdr set)))
            partition))))))


;;;
;;;; String
;;;


(define (jazz.memstring char string)
  (let ((len (%%string-length string)))
    (let iter ((n 0))
      (cond ((%%fx= n len)
             #f)
            ((%%eqv? (%%string-ref string n) char)
             #t)
            (else
             (iter (%%fx+ n 1)))))))


(define (jazz.split-string str separator)
  (let ((lst '())
        (end (%%string-length str)))
    (let iter ((pos (%%fx- end 1)))
      (if (%%fx> pos 0)
          (begin
            (if (%%eqv? (%%string-ref str pos) separator)
                (begin
                  (set! lst (%%cons (%%substring str (%%fx+ pos 1) end) lst))
                  (set! end pos)))
            (iter (%%fx- pos 1))))
        (%%cons (%%substring str 0 end) lst))))


(define (jazz.join-strings strings separator)
  (let ((output (open-output-string)))
    (display (%%car strings) output)
    (for-each (lambda (string)
                (display separator output)
                (display string output))
              (%%cdr strings))
    (get-output-string output)))


;;;
;;;; Vector
;;;


(define (jazz.vector-for-each proc vector)
  (let ((len (%%vector-length vector)))
    (let iter ((n 0))
      (if (%%fx< n len)
          (begin
            (proc (%%vector-ref vector n))
            (iter (%%fx+ n 1)))))))


(define (jazz.vector-memq? obj vector)
  (let ((len (%%vector-length vector)))
    (let iter ((n 0))
      (if (%%fx< n len)
          (if (%%eq? (%%vector-ref vector n) obj)
              #t
            (iter (%%fx+ n 1)))
        #f))))


(define (jazz.resize-vector vector size)
  (let ((new-vector (%%make-vector size #f)))
    (let iter ((offset (- (min size (%%vector-length vector)) 1)))
      (%%when (>= offset 0)
        (%%vector-set! new-vector offset (%%vector-ref vector offset))
        (iter (- offset 1))))
    new-vector))


;;;
;;;; Symbol
;;;


(define (jazz.identifier-name identifier)
  (let* ((str (%%symbol->string identifier))
         (pos (jazz.find-char-reversed #\. str)))
    (if (%%not pos)
        identifier
      (%%string->symbol (%%substring str (%%fx+ pos 1) (%%string-length str))))))


(define (jazz.naturals from to)
  (let ((lst '())
        (n from))
    (%%while (%%fx< n to)
      (set! lst (%%cons n lst))
      (set! n (%%fx+ n 1)))
    (jazz.reverse! lst)))


;;;
;;;; Hashtable
;;;


;; no support for duplicates special case needed
(define (jazz.hashtable-merge into from)
  (%%iterate-hashtable from
    (lambda (key value)
      (if (%%not (%%hashtable-ref into key #f))
          (%%hashtable-set! into key value))))))
