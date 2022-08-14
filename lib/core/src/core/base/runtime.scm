;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Base Runtime
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
;;;  Portions created by the Initial Developer are Copyright (C) 1996-2018
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


(unit protected core.base.runtime


;;;
;;;; Boolean
;;;


(define (jazz:boolean expr)
  (%%boolean expr))


(define (jazz:not-null? expr)
  (%%not-null? expr))


;;;
;;;; Keyword
;;;


(define (jazz:keyword? obj)
  (%%keyword? obj))


(define (jazz:keyword->string keyword)
  (%%keyword->string keyword))


(define (jazz:string->keyword string)
  (%%string->keyword string))


;;;
;;;; List
;;;


(define (jazz:find-in proc lst)
  (declare (proper-tail-calls))
  (let iter ((scan lst))
    (if (%%null? scan)
        #f
      (or (proc (%%car scan))
          (iter (%%cdr scan))))))


(define (jazz:find-if predicate lst)
  (declare (proper-tail-calls))
  (let iter ((scan lst))
    (if (%%null? scan)
        #f
      (let ((value (%%car scan)))
        (if (predicate value)
            value
          (iter (%%cdr scan)))))))


(define (jazz:find-rank element lst)
  (declare (proper-tail-calls))
  (let iter ((scan lst) (rank 0))
       (cond ((%%null? scan)
              #f)
             ((%%eq? (%%car scan) element)
              rank)
             (else
              (iter (%%cdr scan) (%%fx+ rank 1))))))


;; from srfi-1
(define (jazz:reverse! lst)
  (let loop ((lst lst) (ans '()))
    (if (%%null? lst)
        ans
      (let ((tail (%%cdr lst)))
        (%%set-cdr! lst ans)
        (loop tail lst)))))


(define (jazz:list-copy lst)
  (map (lambda (obj) obj) lst))


(define (jazz:last-tail lst)
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


(define (jazz:last-pair lst)
  (if (%%pair? lst)
      (let iter ((scan lst))
        (let ((tail (%%cdr scan)))
          (if (%%pair? tail)
              (iter tail)
            scan)))
    lst))


(define (jazz:last lst)
  (%%car (jazz:last-pair lst)))


(define (jazz:remove-duplicates lst)
  (let iter ((scan lst))
    (if (%%not-null? scan)
        (let ((value (%%car scan))
              (result (iter (%%cdr scan))))
          (if (%%memv value result)
              result
            (%%cons value result)))
      '())))


(define (jazz:proper-list lst)
  (let iter ((scan lst))
    (if (pair? scan)
        (cons (car scan) (iter (cdr scan)))
      '())))


(define (jazz:partition lst key associate)
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


(define (jazz:rassq obj alist)
  (let iter ((rest alist))
    (cond ((%%null? rest)
           #f)
          ((%%eq? obj (%%cdar rest))
           (%%car rest))
          (else
           (iter (%%cdr rest))))))


(define (jazz:butlast lst)
  (if (%%null? (%%cdr lst))
      '()
    (%%cons (%%car lst) (jazz:butlast (%%cdr lst)))))


(define (jazz:remove! target lst)
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


;;;
;;;; Reader
;;;


(define (jazz:read-source-all port #!optional (container #f) (line #f) (col #f))
  (if container
      (%%input-port-names-set! port (lambda (port) container)))
  (if line
      (%%input-port-line-set! port (%%fx+ line 1)))
  (if col
      (%%input-port-column-set! port (%%fx+ col 1)))
  
  (let ((begin-vector
          (%%read-all-as-a-begin-expr-from-port
            port
            (%%current-readtable)
            jazz:wrap-datum&
            jazz:unwrap-datum&
            (jazz:readtable-start-syntax (%%current-readtable))
            #f)))
    (%%cdr (%%source-code (%%vector-ref begin-vector 1)))))


(define (jazz:read-source-first port #!optional (container #f) (line #f) (col #f))
  (%%car (jazz:read-source-all port container line col)))


;;;
;;;; Writer
;;;


(define (jazz:output-port-width-set! port width)
  (jazz:character-port-output-width-set! port (lambda (port) width))
  (void))


;;;
;;;; Serial
;;;


(define (jazz:object->serial obj)
  (object->serial-number obj))


(define (jazz:serial->object number #!optional (default #!void))
  (if (%%eq? default #!void)
      (serial-number->object number)
    (serial-number->object number default)))


;; for debugging
(define (jazz:object->serial-symbol obj)
  (%%string->symbol (%%string-append "#" (%%number->string (jazz:object->serial obj)))))


;;;
;;;; String
;;;


(define (jazz:memstring char string)
  (let ((len (%%string-length string)))
    (let iter ((n 0))
      (cond ((%%fx= n len)
             #f)
            ((%%eqv? (%%string-ref string n) char)
             #t)
            (else
             (iter (%%fx+ n 1)))))))


;;;
;;;; Identifier
;;;


(define (jazz:identifier-name identifier)
  (%%assert (%%symbol? identifier)
    (let* ((str (%%symbol->string identifier))
           (pos (jazz:string-find-reversed str #\.)))
      (if (%%not pos)
          identifier
        (%%string->symbol (%%substring str (%%fx+ pos 1) (%%string-length str)))))))


;;;
;;;; Reference
;;;


(define (jazz:reference-unit reference)
  (%%assert (%%symbol? reference)
    (let* ((str (%%symbol->string reference))
           (pos (jazz:string-find-reversed str #\:)))
      (if (%%not pos)
          #f
        (%%string->symbol (%%substring str 0 pos))))))


(define (jazz:reference-name reference)
  (%%assert (%%symbol? reference)
    (let* ((str (%%symbol->string reference))
           (pos (jazz:string-find-reversed str #\:)))
      (if (%%not pos)
          reference
        (%%string->symbol (%%substring str (%%fx+ pos 1) (%%string-length str)))))))


;;;
;;;; Helper
;;;


(define (jazz:compose-helper s1 s2)
  (%%compose-helper s1 s2))


;;;
;;;; Specifier
;;;


(define (jazz:specifier? expr)
  (and (%%symbol? expr)
       (let ((str (%%symbol->string expr)))
         (let ((len (%%string-length str)))
           (and (%%fx> len 2)
                (%%eqv? (%%string-ref str 0) #\<)
                (%%eqv? (%%string-ref str (%%fx- len 1)) #\>))))))


(define (jazz:specifier->name specifier)
  (let ((extract
          (lambda (string)
            (%%substring string 1 (%%fx- (%%string-length string) 1)))))
    (%%string->symbol (extract (%%symbol->string specifier)))))


(define (jazz:name->specifier name)
  (%%string->symbol (%%string-append "<" (%%symbol->string name) ">")))


(define (jazz:binding-specifier binding)
  (let ((cdr-binding (%%cdr (jazz:source-code binding))))
    (and (%%pair? cdr-binding)
         (jazz:specifier? (jazz:source-code (%%car cdr-binding)))
         (jazz:source-code (%%car cdr-binding)))))


;;;
;;;; Enumerator
;;;


(define (jazz:enumerator? obj)
  (and (%%symbol? obj)
       (%%eqv? (%%string-ref (%%symbol->string obj) 0) #\:)))


(define (jazz:enumerator->symbol enumerator)
  (let ((name (%%symbol->string enumerator)))
    (%%string->symbol (%%substring name 1 (%%string-length name)))))


(define (jazz:symbol->enumerator symbol)
  (%%string->symbol (%%string-append ":" (%%symbol->string symbol))))


;;;
;;;; Source
;;;


(define (jazz:source? obj)
  (%%source? obj))


(define (jazz:source-code expr)
  (if (%%source? expr)
      (%%source-code expr)
    expr))


(define (jazz:source-locat expr)
  (if (%%source? expr)
      (jazz:source-locat& expr)
    #f))


(define (jazz:desourcify expr)
  (%%desourcify expr))


(define (jazz:desourcify-all expr)
  (define (desourcify-source src)
    (desourcify-all (%%source-code src)))

  (define (desourcify-list lst)
    (cond ((%%pair? lst)
           (%%cons (desourcify-all (%%car lst))
                   (desourcify-list (%%cdr lst))))
          ((%%null? lst)
           '())
          (else
           (desourcify-all lst))))

  (define (desourcify-vector vect)
    (let* ((len (%%vector-length vect))
           (x (%%make-vector len 0)))
      (let loop ((i (%%fx- len 1)))
        (if (%%fx< i 0)
          x
          (begin
            (%%vector-set! x i (desourcify-all (%%vector-ref vect i)))
            (loop (%%fx- i 1)))))))
  
  (define (desourcify-box bx)
    (%%box (desourcify-all (%%unbox bx))))
  
  (define (desourcify-all expr)
    (cond ((%%source? expr)
           (desourcify-source expr))
          ((%%pair? expr)
           (desourcify-list expr))
          ((%%vector? expr)
           (desourcify-vector expr))
          ((%%box? expr)
           (desourcify-box expr))
          (else
           expr)))
  
  (desourcify-all expr))


(define (jazz:sourcify expr src)
  (%%sourcify expr src))


(define (jazz:sourcify-if expr src)
  (if (jazz:source? src)
      (jazz:sourcify expr src)
    expr))


(define (jazz:sourcify-deep expr src)
  (%%sourcify-deep expr src))


(define (jazz:sourcify-deep-if expr src)
  (if (jazz:source? src)
      (jazz:sourcify-deep expr src)
    expr))


(define (jazz:sourcify-list lst src)
  (map (lambda (expr)
         (jazz:sourcify-if (jazz:desourcify-all expr) src))
       lst))


(define (jazz:locat-container locat)
  (%%locat-container locat))


(define (jazz:locat-position locat)
  (%%locat-position locat))


(define (jazz:locat-start locat)
  (jazz:locat-start& locat))

(define (jazz:locat-end locat)
  (jazz:locat-end& locat))


(define (jazz:locat->container/line/col locat)
  (let ((container (and locat (%%locat-container locat))))
    (if container
        (let ((filepos (%%position->filepos (%%locat-position locat))))
          (let ((line (%%filepos-line filepos))
                (col (%%filepos-col filepos)))
            (%%list container line col)))
      #f)))


(define (jazz:locat->path/container/start/end& locat)
  (let ((container (%%locat-container locat)))
    (let ((path (%%container->path container))
          (startpos (%%position->filepos (jazz:locat-start& locat)))
          (endpos (%%position->filepos (jazz:locat-end& locat))))
      (%%list path
              container
              (%%filepos-line startpos)
              (%%filepos-col startpos)
              (%%filepos-line endpos)
              (%%filepos-col endpos)))))


(define (jazz:container->path container)
  (%%container->path container))


(define (jazz:position->filepos position)
  (%%position->filepos position))


(define (jazz:filepos-line filepos)
  (%%filepos-line filepos))


(define (jazz:filepos-col filepos)
  (%%filepos-col filepos))


;;;
;;;; Emit
;;;


(define jazz:save-emit-to
  (make-parameter #f))


(define (jazz:save-emit-if emit)
  (%%when (or (jazz:save-emit-to) (and (jazz:save-emit?) (jazz:compiled-source)))
    (parameterize ((current-readtable jazz:scheme-readtable))
      (let ((path (or (jazz:save-emit-to) (jazz:binary-with-extension (jazz:compiled-source) ".scm"))))
        (call-with-output-file (list path: path eol-encoding: (jazz:platform-eol-encoding jazz:kernel-platform))
          (lambda (port)
            (pretty-print (jazz:desourcify-all emit) port)))))))


;;;
;;;; Debug
;;;


(define (jazz:extract-location src)
  (cond ((%%not src)
         src)
        ((jazz:source&? src)
         (let ((locat (jazz:source-locat& src)))
           (let ((start (jazz:locat-start& locat))
                 (end (jazz:locat-end& locat)))
             (%%vector (%%locat-container locat)
                       (%%fx+ (%%filepos-line start) 1)
                       (%%fx+ (%%filepos-col start) 1)
                       (%%fx+ (%%filepos-line end) 1)
                       (%%fx+ (%%filepos-col end) 1)))))
        (else
         (let ((locat (%%source-locat src)))
           (let ((pos (%%locat-position locat)))
             (%%vector (%%locat-container locat)
                       (%%fx+ (%%filepos-line pos) 1)
                       (%%fx+ (%%filepos-col pos) 1)))))))


(define (jazz:present-source obj)
  (define (present-src src)
    (let ((code (jazz:source-code src)))
      (if (jazz:source&? src)
          (let ((locat (jazz:source-locat& src)))
            (let ((start (jazz:locat-start& locat))
                  (end (jazz:locat-end& locat)))
              (%%vector 'source
                        (jazz:present-source code)
                        (%%locat-container locat)
                        (%%fx+ (%%filepos-line start) 1)
                        (%%fx+ (%%filepos-col start) 1)
                        (%%fx+ (%%filepos-line end) 1)
                        (%%fx+ (%%filepos-col end) 1))))
        (let ((locat (%%source-locat src)))
          (let ((pos (%%locat-position locat)))
            (%%vector 'source
                      (jazz:present-source code)
                      (%%locat-container locat)
                      (%%fx+ (%%filepos-line pos) 1)
                      (%%fx+ (%%filepos-col pos) 1)))))))

  (define (present-list lst)
    (cond ((%%pair? lst)
           (%%cons (jazz:present-source (%%car lst))
                   (present-list (%%cdr lst))))
          ((%%null? lst)
           '())
          (else
           (jazz:present-source lst))))

  (define (present-vector vect)
    (let* ((len (%%vector-length vect))
           (x (%%make-vector len 0)))
      (let loop ((i (%%fx- len 1)))
        (if (%%fx< i 0)
            x
          (begin
            (%%vector-set! x i (jazz:present-source (%%vector-ref vect i)))
            (loop (%%fx- i 1)))))))

  (cond ((%%source? obj)
         (present-src obj))
        ((%%pair? obj)
         (present-list obj))
        ((%%vector? obj)
         (present-vector obj))
        (else
         obj)))


;;;
;;;; Unspecified
;;;


(define (jazz:unspecified)
  (%%unspecified))


(define (jazz:unspecified? expr)
  (%%unspecified? expr))


(define (jazz:specified? expr)
  (%%not (%%unspecified? expr)))


;;;
;;;; Values
;;;


(define (jazz:values? obj)
  (%%values? obj))


(define (jazz:values-ref values n)
  (%%values-ref values n))


(define (jazz:values-set! values n obj)
  (%%values-set! values n obj))


;;;
;;;; Vector
;;;


(define (jazz:vector-for-each proc vector)
  (let ((len (%%vector-length vector)))
    (let iter ((n 0))
      (if (%%fx< n len)
          (begin
            (proc (%%vector-ref vector n))
            (iter (%%fx+ n 1)))))))


(define (jazz:vector-memq? obj vector)
  (let ((len (%%vector-length vector)))
    (let iter ((n 0))
      (if (%%fx< n len)
          (if (%%eq? (%%vector-ref vector n) obj)
              #t
            (iter (%%fx+ n 1)))
        #f))))


(define (jazz:vector-memv? obj vector)
  (let ((len (%%vector-length vector)))
    (let iter ((n 0))
      (if (%%fx< n len)
          (if (%%eqv? (%%vector-ref vector n) obj)
              #t
            (iter (%%fx+ n 1)))
        #f))))


(define (jazz:resize-vector vector size)
  (let ((new-vector (%%make-vector size #f)))
    (let iter ((offset (%%fx- (min size (%%vector-length vector)) 1)))
      (%%when (%%fx>= offset 0)
        (%%vector-set! new-vector offset (%%vector-ref vector offset))
        (iter (%%fx- offset 1))))
    new-vector)))
