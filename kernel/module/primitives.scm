;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Primitives
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


(cond-expand
  (gambit
    (declare (block)
             (standard-bindings)
             (extended-bindings)
             (not safe)))
  (else))


;;;
;;;; Boolean
;;;


(cond-expand
  (gambit
   (define-macro (%%boolean? obj)
     `(boolean? ,obj))
   
   (define-macro (%%not expr)
     (if jazz.debug-core?
         `(not ,expr)
       `(##not ,expr))))
  
  (else))


;;;
;;;; Char
;;;


(cond-expand
  (gambit
    (define-macro (%%char? obj)
      (if jazz.debug-core?
          `(char? ,obj)
        `(##char? ,obj))))

  (else))


;;;
;;;; Complex
;;;


(cond-expand
  (gambit
    (define-macro (%%complex? obj)
      (if jazz.debug-core?
          `(complex? ,obj)
        `(##complex? ,obj))))

  (else))


;;;
;;;; Control
;;;


(cond-expand
  (gambit
   (define-macro (%%procedure? obj)
     (if jazz.debug-core?
         `(procedure? ,obj)
       `(##procedure? ,obj)))
   
   (define-macro (%%apply proc lst)
     (if jazz.debug-core?
         `(apply ,proc ,lst)
       `(##apply ,proc ,lst))))
  
  (else))


;;;
;;;; Equality
;;;


(cond-expand
  (gambit
   (define-macro (%%eq? x y)
     (if jazz.debug-core?
         `(eq? ,x ,y)
       `(##eq? ,x ,y)))
   
   (define-macro (%%neq? x y)
     `(%%not (%%eq? ,x ,y)))
   
   (define-macro (%%eqv? x y)
     (if jazz.debug-core?
         `(eqv? ,x ,y)
       `(##eqv? ,x ,y)))
   
   (define-macro (%%equal? x y)
     (if jazz.debug-core?
         `(equal? ,x ,y)
       `(##equal? ,x ,y))))
  
  (else
   (define-macro (%%eq? x y)
     `(eq? ,x ,y))))


;;;
;;;; Fixnum
;;;


(cond-expand
  (gambit
   (define-macro (%%fixnum? obj)
     (if jazz.debug-core?
         `(fixnum? ,obj)
       `(##fixnum? ,obj)))
   
   (define-macro (%%fixnum->flonum x)
     (if jazz.debug-core?
         `(fixnum->flonum ,x)
       `(##fixnum->flonum ,x)))
   
   (define-macro (%%fx= x y)
     (if jazz.debug-core?
         `(= ,x ,y)
       `(##fixnum.= ,x ,y)))
   
   (define-macro (%%fx< x y)
     (if jazz.debug-core?
         `(< ,x ,y)
       `(##fixnum.< ,x ,y)))
   
   (define-macro (%%fx<= x y)
     (if jazz.debug-core?
         `(<= ,x ,y)
       `(##fixnum.<= ,x ,y)))
   
   (define-macro (%%fx> x y)
     (if jazz.debug-core?
         `(> ,x ,y)
       `(##fixnum.> ,x ,y)))
   
   (define-macro (%%fx>= x y)
     (if jazz.debug-core?
         `(>= ,x ,y)
       `(##fixnum.>= ,x ,y)))
   
   (define-macro (%%fx+ x y)
     (if jazz.debug-core?
         `(+ ,x ,y)
       `(##fixnum.+ ,x ,y)))
   
   (define-macro (%%fx- x y)
     (if jazz.debug-core?
         `(- ,x ,y)
       `(##fixnum.- ,x ,y)))
   
   (define-macro (%%fx* x y)
     (if jazz.debug-core?
         `(* ,x ,y)
       `(##fixnum.* ,x ,y)))
   
   (define-macro (%%fxquotient x y)
     (if jazz.debug-core?
         `(quotient ,x ,y)
       `(##fxquotient ,x ,y)))))


;;;
;;;; Flonum
;;;


(cond-expand
  (gambit
   (define-macro (%%flonum? obj)
     (if jazz.debug-core?
         `(flonum? ,obj)
       `(##flonum? ,obj)))
   
   (define-macro (%%fl= x y)
     (if jazz.debug-core?
         `(= ,x ,y)
       `(##flonum.= ,x ,y)))
   
   (define-macro (%%fl< x y)
     (if jazz.debug-core?
         `(< ,x ,y)
       `(##flonum.< ,x ,y)))
   
   (define-macro (%%fl<= x y)
     (if jazz.debug-core?
         `(<= ,x ,y)
       `(##flonum.<= ,x ,y)))
   
   (define-macro (%%fl> x y)
     (if jazz.debug-core?
         `(> ,x ,y)
       `(##flonum.> ,x ,y)))
   
   (define-macro (%%fl>= x y)
     (if jazz.debug-core?
         `(>= ,x ,y)
       `(##flonum.>= ,x ,y)))
   
   (define-macro (%%fl+ x y)
     (if jazz.debug-core?
         `(+ ,x ,y)
       `(##flonum.+ ,x ,y)))
   
   (define-macro (%%fl- x y)
     (if jazz.debug-core?
         `(- ,x ,y)
       `(##flonum.- ,x ,y)))
   
   (define-macro (%%fl* x y)
     (if jazz.debug-core?
         `(* ,x ,y)
       `(##flonum.* ,x ,y)))
   
   (define-macro (%%fl/ x y)
     (if jazz.debug-core?
         `(/ ,x ,y)
       `(##flonum./ ,x ,y))))

  (else
   (define-macro (%%fl> x y)
     `(> ,x ,y))
   
   (define-macro (%%fl+ x y)
     `(+ ,x ,y))))


;;;
;;;; Foreign
;;;


(cond-expand
  (gambit
   (define-macro (%%foreign? obj)
     `(##foreign? ,obj)))
  
  (else))


;;;
;;;; Keyword
;;;


(cond-expand
  (gambit
    (define-macro (%%keyword? obj)
      (if jazz.debug-core?
          `(keyword? ,obj)
        `(##keyword? ,obj)))
   
    (define-macro (%%string->keyword str)
      (if jazz.debug-core?
          `(string->keyword ,str)
        `(##string->keyword ,str)))
   
   (define-macro (%%keyword->string keyword)
     (if jazz.debug-core?
         `(keyword->string ,keyword)
       `(##keyword->string ,keyword))))

  (else))


;;;
;;;; List
;;;


(cond-expand
  (gambit
    (define-macro (%%null? obj)
      (if jazz.debug-core?
          `(null? ,obj)
        `(##null? ,obj)))
    
    (define-macro (%%pair? obj)
      (if jazz.debug-core?
          `(pair? ,obj)
        `(##pair? ,obj)))
    
    (define-macro (%%car pair)
      (if jazz.debug-core?
          `(car ,pair)
        `(##car ,pair)))
    
    (define-macro (%%cdr pair)
      (if jazz.debug-core?
          `(cdr ,pair)
        `(##cdr ,pair)))
    
    (define-macro (%%set-car! pair val)
      (if jazz.debug-core?
          `(set-car! ,pair ,val)
        `(##set-car! ,pair ,val)))
    
    (define-macro (%%set-cdr! pair val)
      (if jazz.debug-core?
          `(set-cdr! ,pair ,val)
        `(##set-cdr! ,pair ,val)))
    
    (define-macro (%%caar pair)
      (if jazz.debug-core?
          `(caar ,pair)
        `(##caar ,pair)))
    
    (define-macro (%%cadr pair)
      (if jazz.debug-core?
          `(cadr ,pair)
        `(##cadr ,pair)))
    
    (define-macro (%%cdar pair)
      (if jazz.debug-core?
          `(cdar ,pair)
        `(##cdar ,pair)))
    
    (define-macro (%%cddr pair)
      (if jazz.debug-core?
          `(cddr ,pair)
        `(##cddr ,pair)))
    
    (define-macro (%%length lst)
      (if jazz.debug-core?
          `(length ,lst)
        `(##length ,lst)))
    
    (define-macro (%%memq obj lst)
      (if jazz.debug-core?
          `(memq ,obj ,lst)
        `(##memq ,obj ,lst)))
    
    (define-macro (%%memv obj lst)
      `(memv ,obj ,lst))
    
    (define-macro (%%cons x y)
      (if jazz.debug-core?
          `(cons ,x ,y)
        `(##cons ,x ,y)))
    
    (define-macro (%%list . rest)
      (if jazz.debug-core?
          `(list ,@rest)
        `(##list ,@rest)))
    
    (define-macro (%%append x y)
      (if jazz.debug-core?
          `(append ,x ,y)
        `(##append ,x ,y)))
    
    (define-macro (%%reverse lst)
      (if jazz.debug-core?
          `(reverse ,lst)
        `(##reverse ,lst)))
    
    (define-macro (%%list->vector lst)
      (if jazz.debug-core?
          `(list->vector ,lst)
        `(##list->vector ,lst))))
  
  (else
   (define-macro (%%memq obj lst)
     `(memq ,obj ,lst))))


;;;
;;;; Number
;;;


(cond-expand
  (gambit
    (define-macro (%%number? obj)
      (if jazz.debug-core?
          `(number? ,obj)
        `(##number? ,obj)))
    
    (define-macro (%%integer? obj)
      (if jazz.debug-core?
          `(integer? ,obj)
        `(##integer? ,obj)))
    
    (define-macro (%%real? obj)
      (if jazz.debug-core?
          `(real? ,obj)
        `(##real? ,obj)))
    
    (define-macro (%%number->string n)
      (if jazz.debug-core?
          `(number->string ,n)
        `(##number->string ,n))))

  (else))


;;;
;;;; Port
;;;


(cond-expand
  (gambit
    (define-macro (%%port? obj)
      (if jazz.debug-core?
          `(port? ,obj)
        `(##port? ,obj)))
    
    (define-macro (%%eof-object? obj)
      (if jazz.debug-core?
          `(eof-object? ,obj)
        `(##eof-object? ,obj))))
  
  (else))


;;;
;;;; Rational
;;;


(cond-expand
  (gambit
    (define-macro (%%rational? obj)
      (if jazz.debug-core?
          `(rational? ,obj)
        `(##rational? ,obj))))

  (else))


;;;
;;;; String
;;;


(cond-expand
  (gambit
    (define-macro (%%string? obj)
      (if jazz.debug-core?
          `(string? ,obj)
        `(##string? ,obj)))
   
    (define-macro (%%string-length str)
      (if jazz.debug-core?
          `(string-length ,str)
        `(##string-length ,str)))
   
   (define-macro (%%string-ref str pos)
     (if jazz.debug-core?
         `(string-ref ,str ,pos)
       `(##string-ref ,str ,pos)))
   
   (define-macro (%%string-set! str pos val)
     (if jazz.debug-core?
         `(string-set! ,str ,pos ,val)
       `(##string-set! ,str ,pos ,val)))
   
   (define-macro (%%substring str start end)
     (if jazz.debug-core?
         `(substring ,str ,start ,end)
       `(##substring ,str ,start ,end)))
   
   (define-macro (%%string-append . rest)
     (if jazz.debug-core?
         `(string-append ,@rest)
       `(##string-append ,@rest))))

  (else))


;;;
;;;; Symbol
;;;


(cond-expand
  (gambit
    (define-macro (%%symbol? obj)
      (if jazz.debug-core?
          `(symbol? ,obj)
        `(##symbol? ,obj)))
   
    (define-macro (%%string->symbol str)
      (if jazz.debug-core?
          `(string->symbol ,str)
        `(##string->symbol ,str)))
   
   (define-macro (%%symbol->string symbol)
     (if jazz.debug-core?
         `(symbol->string ,symbol)
       `(##symbol->string ,symbol))))

  (else))


;;;
;;;; Syntax
;;;


(cond-expand
  (gambit
    (define-macro (jazz.define-syntax name transformer)
      `(##define-syntax ,name
         ,transformer))
    
    (define (%%source-code expr)
      (if (##source? expr)
          (##source-code expr)
        expr))
    
    (define (%%desourcify expr)
      (##desourcify expr))
    
    (define (%%sourcify expr src)
      (##sourcify expr src))))


;;;
;;;; Table
;;;


(cond-expand
  (gambit
    (define-macro (%%table? obj)
      `(table? ,obj))
    
    (define-macro (%%make-table #!key (test eq?) (hash #f))
      `(if (eq? ,hash #f)
           (make-table test: ,test)
         (make-table test: ,test hash: ,hash)))
    
    (define-macro (%%table-ref table key default)
      (if jazz.debug-core?
          `(table-ref ,table ,key ,default)
        `(##table-ref ,table ,key ,default)))
    
    (define-macro (%%table-set! table key value)
      (if jazz.debug-core?
          `(table-set! ,table ,key ,value)
        `(##table-set! ,table ,key ,value)))
    
    (define-macro (%%table-clear table key)
      `(table-set! ,table ,key))
    
    (define-macro (%%table-keys table)
      `(map car (table->list ,table)))
    
    (define-macro (%%table-length table)
      `(table-length ,table))
    
    (define-macro (%%iterate-table table proc)
      `(table-for-each ,proc ,table))
    
    (define-macro (%%table-merge! into from)
      `(##table-merge! ,into ,from))
    
    (define-macro (%%alist->table alist test)
      `(list->table ,alist test: ,test))
    
    (define-macro (%%table->alist table)
      `(table->list ,table))
    
    (define-macro (%%table-entries table)
      `(table-length ,table))
    
    (define-macro (%%copy-table table)
      `(table-copy ,table)))
  
  (else))


;;;
;;;; Values
;;;


(cond-expand
  (gambit
    (define-macro (%%values? obj)
      `(##values? ,obj)))
  (else))


;;;
;;;; Vector
;;;


(cond-expand
  (gambit
    (define-macro (%%vector? obj)
      (if jazz.debug-core?
          `(vector? ,obj)
        `(##vector? ,obj)))
   
   (define-macro (%%vector . rest)
     (if jazz.debug-core?
         `(vector ,@rest)
       `(##vector ,@rest)))
   
   (define-macro (%%make-vector size . rest)
     (if jazz.debug-core?
         `(make-vector ,size ,@rest)
       `(##make-vector ,size ,@rest)))
   
   (define-macro (%%vector-length vector)
     (if jazz.debug-core?
         `(vector-length ,vector)
       `(##vector-length ,vector)))
   
   (define-macro (%%vector-ref vector n)
     (if jazz.debug-core?
         `(vector-ref ,vector ,n)
       `(##vector-ref ,vector ,n)))
   
   (define-macro (%%vector-set! vector n value)
     (if jazz.debug-core?
         `(vector-set! ,vector ,n ,value)
       `(##vector-set! ,vector ,n ,value)))
   
   (define-macro (%%vector-copy vector . rest)
     (if jazz.debug-core?
         `(vector-copy ,vector ,@rest)
       `(##vector-copy ,vector ,@rest)))
   
   (define-macro (%%vector->list vector)
     (if jazz.debug-core?
         `(vector->list ,vector)
       `(##vector->list ,vector))))

  (else
   (define-macro (%%vector? obj)
     `(vector? ,obj))
   
   (define-macro (%%vector . rest)
     `(vector ,@rest))
   
   (define-macro (%%make-vector size . rest)
     `(make-vector ,size ,@rest))
   
   (define-macro (%%vector-length vector)
     `(vector-length ,vector))
   
   (define-macro (%%vector-ref vector n)
     `(vector-ref ,vector ,n))
   
   (define-macro (%%vector-set! vector n value)
     `(vector-set! ,vector ,n ,value))
   
   (define-macro (%%vector-copy vector . rest)
     `(vector-copy ,vector ,@rest))))
