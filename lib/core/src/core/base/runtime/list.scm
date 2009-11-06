;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Lists
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


(unit protected core.base.runtime.list


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


(define (jazz.getprop plist target)
  (let iter ((scan plist))
    (cond ((%%null? scan)
           #f)
          ((%%eqv? (%%car scan) target)
           scan)
          (else
           (iter (%%cddr scan))))))


(define (jazz.getf plist target #!key (not-found #f))
  (let ((pair (jazz.getprop plist target)))
    (if pair
        (%%cadr pair)
      not-found)))


(define jazz.reverse!
  reverse)


(define (jazz.list-copy lst)
  (map (lambda (obj) obj) lst))


(define (jazz.last-tail lst)
  (if (%%pair? lst)
      (let iter ((scan lst))
        (let ((tail (%%cdr scan)))
          (cond ((%%pair? tail)
                 (iter tail))
                ((%%null? tail)
                 scan)
                (else
                 tail))))
    lst))


(define (jazz.last-pair lst)
  (if (%%pair? lst)
      (let iter ((scan lst))
        (let ((tail (%%cdr scan)))
          (if (%%pair? tail)
              (iter tail)
            scan)))
    lst))


(define (jazz.last lst)
  (%%car (jazz.last-pair lst)))


(define (jazz.remove-duplicates lst)
  (let iter ((scan lst))
    (if (%%not-null? scan)
        (let ((value (%%car scan))
              (result (iter (%%cdr scan))))
          (if (%%memv value result)
              result
            (%%cons value result)))
      '())))


(define (jazz.partition lst key associate)
  (let iter ((scan lst))
    (if (%%null? scan)
        '()
      (let* ((partition (iter (%%cdr scan)))
             (element (%%car scan))
             (category (key element))
             (set (associate category partition)))
        (if (%%not set)
            (%%cons (%%cons category (%%list element)) partition)
          (begin
            (%%set-cdr! set (%%cons element (%%cdr set)))
            partition))))))


(define (jazz.rassq obj alist)
  (let iter ((rest alist))
    (cond ((%%null? rest)
           #f)
          ((%%eq? obj (%%cdar rest))
           (%%car rest))
          (else
           (iter (%%cdr rest))))))


(define (jazz.butlast lst)
  (if (%%null? (%%cdr lst))
      '()
    (%%cons (%%car lst) (jazz.butlast (%%cdr lst)))))


(define (jazz.naturals from to)
  (let ((lst '())
        (n from))
    (%%while (%%fx< n to)
      (set! lst (%%cons n lst))
      (set! n (%%fx+ n 1)))
    (jazz.reverse! lst)))


(define (jazz.remove! target lst)
  (%%while (and (%%not-null? lst) (%%eqv? target (%%car lst)))
    (set! lst (%%cdr lst)))
  (if (%%null? lst)
      '()
    (begin
      (let ((previous lst)
            (scan (%%cdr lst)))
        (%%while (%%not-null? scan)
          (if (%%eqv? target (%%car scan))
              (begin
                (set! scan (%%cdr scan))
                (%%set-cdr! previous scan))
            (begin
              (set! previous scan)
              (set! scan (%%cdr scan))))))
      lst))))
