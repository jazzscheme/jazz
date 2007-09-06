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
  (let loop ((scan lst))
    (if (%%null? scan)
        #f
      (or (proc (%%car scan))
          (loop (%%cdr scan))))))


(define (jazz.find-if predicate lst)
  (let loop ((scan lst))
    (if (%%null? scan)
        #f
      (let ((value (%%car scan)))
        (if (predicate value)
            value
          (loop (%%cdr scan)))))))


(define (jazz.collect-if predicate lst)
  (let ((result '()))
    (let loop ((scan lst))
      (if (%%not-null? scan)
          (begin
            (loop (%%cdr scan))
            (let ((value (%%car scan)))
              (if (predicate value)
                  (set! result (%%cons value result)))))))
    result))


(define (jazz.find-char-reversed c str)
  (let loop ((pos (%%fixnum- (%%string-length str) 1)))
    (cond ((%%fixnum< pos 0)
           #f)
          ((char=? (%%string-ref str pos) c)
           pos)
          (else
           (loop (%%fixnum- pos 1))))))


(define (jazz.getprop plist target)
  (let loop ((scan plist))
    (cond ((%%null? scan)
           #f)
          ((%%eqv? (%%car scan) target)
           scan)
          (else
           (loop (%%cddr scan))))))


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
    (let loop ((scan lst))
      (if (%%not (%%null? scan))
          (begin
            (loop (%%cdr scan))
            (let ((value (%%car scan)))
              (if (%%not (%%memv value result))
                  (set! result (%%cons value result)))))))
    result))


;;;
;;;; String
;;;


(define (jazz.memstring char string)
  (let ((len (%%string-length string)))
    (let loop ((n 0))
      (cond ((%%fixnum= n len)
             #f)
            ((%%eqv? (%%string-ref string n) char)
             #t)
            (else
             (loop (%%fixnum+ n 1)))))))


(define (jazz.split-string str separator)
  (let ((lst '())
        (end (%%string-length str)))
    (let loop ((pos (%%fixnum- end 1)))
      (if (%%fixnum> pos 0)
          (begin
            (if (%%eqv? (%%string-ref str pos) separator)
                (begin
                  (set! lst (%%cons (%%substring str (%%fixnum+ pos 1) end) lst))
                  (set! end pos)))
            (loop (%%fixnum- pos 1))))
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
;;;; Symbol
;;;


(define (jazz.identifier-name identifier)
  (let* ((str (%%symbol->string identifier))
         (pos (jazz.find-char-reversed #\. str)))
    (if (%%not pos)
        identifier
      (%%string->symbol (%%substring str (%%fixnum+ pos 1) (%%string-length str))))))


(define (jazz.naturals from to)
  (let ((lst '())
        (n from))
    (%%while (%%fixnum< n to)
      (set! lst (%%cons n lst))
      (set! n (%%fixnum+ n 1)))
    (jazz.reverse! lst)))


(define (jazz.bit-or . rest)
  (%%apply bitwise-ior rest)))
