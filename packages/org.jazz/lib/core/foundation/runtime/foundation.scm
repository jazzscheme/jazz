;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Foundation Code
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
;;;    Stephane Le Cornec
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


(module core.foundation.runtime.foundation


;;;
;;;; General
;;;

        
(define (jazz.rassq obj alist)
  (let iter ((rest alist))
    (cond 
      ((%%null? rest)
       #f)
      ((%%eq? obj (%%cdar rest))
       (%%car rest))
      (else
       (iter (%%cdr rest))))))


(define (jazz.every? predicate lst)
  (let iter ((scan lst))
    (cond ((%%null? scan)
           #t)
          ((%%not (predicate (%%car scan)))
           #f)
          (else
           (iter (%%cdr scan))))))


(define (jazz.butlast lst)
  (let ((queue (jazz.new-queue)))
    (%%while (%%not (%%null? (%%cdr lst)))
      (jazz.enqueue queue (%%car lst))
      (set! lst (%%cdr lst)))
    (jazz.queue-list queue)))


(define (jazz.listify obj)
  (if (list? obj)
      obj
    (%%list obj)))


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
      lst)))


(define (jazz.remove-specifiers-quicky form)
  (let ((queue (jazz.new-queue)))
    (let iter ((scan form))
      (if (%%pair? scan)
          (begin
            (if (%%not (jazz.specifier? (%%car scan)))
                (jazz.enqueue queue (%%car scan)))
            (iter (%%cdr scan)))
        (if (%%not (%%null? scan))
            (jazz.enqueue-list queue scan))))
    (jazz.queue-list queue)))


(define (jazz.specifier? expr)
  (and (%%symbol? expr)
       (let ((str (%%symbol->string expr)))
         (and (%%fx> (%%string-length str) 2)
              (%%eqv? (%%string-ref str 0) #\<)
              (%%eqv? (%%string-ref str (%%fx- (%%string-length str) 1)) #\>)))))


(define (jazz.specifier->name specifier)
  (let ((extract
          (lambda (string)
            (%%substring string 1 (%%fx- (%%string-length string) 1)))))
    (%%string->symbol (extract (%%symbol->string specifier)))))


(define (jazz.name->specifier name)
  (%%string->symbol (%%string-append "<" (%%symbol->string name) ">")))


(define (jazz.partition lst key)
  (let ((partitions (jazz.new-queue)))
    (for-each (lambda (element)
                (let* ((category (key element))
                       (partition (assv category (jazz.queue-list partitions))))
                  (if (%%not partition)
                      (let ((elements (jazz.new-queue)))
                        (jazz.enqueue elements element)
                        (jazz.enqueue partitions (%%cons category elements)))
                    (jazz.enqueue (%%cdr partition) element))))
              lst)
    (map (lambda (partition)
           (%%cons (%%car partition) (jazz.queue-list (%%cdr partition))))
         (jazz.queue-list partitions))))


;;;
;;;; Enumerator
;;;


(define (jazz.enumerator? obj)
  (and (%%symbol? obj)
       (%%eqv? (%%string-ref (%%symbol->string obj) 0) #\:)))


;;;
;;;; List
;;;


(cond-expand
  (blues
    (define jazz.list-copy
      copy))
  (else))


(define (jazz.proper-list lst)
  (let ((proper (jazz.new-queue)))
    (%%while (%%pair? lst)
      (jazz.enqueue proper (%%car lst))
      (set! lst (%%cdr lst)))
    (jazz.queue-list proper)))


;;;
;;;; Reader
;;;


(define (jazz.skip-whitespace port)
  (%%while (char-whitespace? (peek-char port))
    (read-char port)))


(define (jazz.read-delimited port delimiter)
  (let ((queue (jazz.new-queue)))
    (jazz.skip-whitespace port)
    (%%while (%%not (%%eqv? (peek-char port) delimiter))
      (jazz.enqueue queue (read port))
      (jazz.skip-whitespace port))
    (read-char port)
    (jazz.queue-list queue)))


(define (jazz.read-until test port)
  (let ((expr '())
        (queue (jazz.new-queue))
        (done? #f))
    (%%while (%%not done?)
      (let ((expr (read port)))
        (if (test expr)
            (set! done? #t)
          (jazz.enqueue queue expr))))
    (jazz.queue-list queue)))


(define (jazz.read-content port)
  (jazz.read-until eof-object? port)))
