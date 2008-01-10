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
   (jazz.define-macro (%%boolean? obj)
     `(boolean? ,obj))
   
   (jazz.define-macro (%%not expr)
     (if jazz.debug-core?
         `(not ,expr)
       `(##not ,expr))))
  
  (else))


;;;
;;;; Char
;;;


(cond-expand
  (gambit
    (jazz.define-macro (%%char? obj)
      (if jazz.debug-core?
          `(char? ,obj)
        `(##char? ,obj)))
    
    (jazz.define-macro (%%char=? c1 c2)
      (if jazz.debug-core?
          `(char=? ,c1 ,c2)
        `(##char=? ,c1 ,c2))))

  (else))


;;;
;;;; Complex
;;;


(cond-expand
  (gambit
    (jazz.define-macro (%%complex? obj)
      (if jazz.debug-core?
          `(complex? ,obj)
        `(##complex? ,obj))))

  (else))


;;;
;;;; Control
;;;


(cond-expand
  (gambit
   (jazz.define-macro (%%procedure? obj)
     (if jazz.debug-core?
         `(procedure? ,obj)
       `(##procedure? ,obj)))
   
   (jazz.define-macro (%%apply proc lst)
     (if jazz.debug-core?
         `(apply ,proc ,lst)
       `(##apply ,proc ,lst))))
  
  (else))


;;;
;;;; Equality
;;;


(cond-expand
  (gambit
   (jazz.define-macro (%%eq? x y)
     (if jazz.debug-core?
         `(eq? ,x ,y)
       `(##eq? ,x ,y)))
   
   (jazz.define-macro (%%neq? x y)
     `(%%not (%%eq? ,x ,y)))
   
   (jazz.define-macro (%%eqv? x y)
     (if jazz.debug-core?
         `(eqv? ,x ,y)
       `(##eqv? ,x ,y)))
   
   (jazz.define-macro (%%equal? x y)
     (if jazz.debug-core?
         `(equal? ,x ,y)
       `(##equal? ,x ,y))))
  
  (else
   (jazz.define-macro (%%eq? x y)
     `(eq? ,x ,y))))


;;;
;;;; Fixnum
;;;


(cond-expand
  (gambit
   (jazz.define-macro (%%fixnum? obj)
     (if jazz.debug-core?
         `(fixnum? ,obj)
       `(##fixnum? ,obj)))
   
   (jazz.define-macro (%%fixnum->flonum x)
     (if jazz.debug-core?
         `(fixnum->flonum ,x)
       `(##fixnum->flonum ,x)))
   
   (jazz.define-macro (%%fx= x y)
     (if jazz.debug-core?
         `(= ,x ,y)
       `(##fixnum.= ,x ,y)))
   
   (jazz.define-macro (%%fx< x y)
     (if jazz.debug-core?
         `(< ,x ,y)
       `(##fixnum.< ,x ,y)))
   
   (jazz.define-macro (%%fx<= x y)
     (if jazz.debug-core?
         `(<= ,x ,y)
       `(##fixnum.<= ,x ,y)))
   
   (jazz.define-macro (%%fx> x y)
     (if jazz.debug-core?
         `(> ,x ,y)
       `(##fixnum.> ,x ,y)))
   
   (jazz.define-macro (%%fx>= x y)
     (if jazz.debug-core?
         `(>= ,x ,y)
       `(##fixnum.>= ,x ,y)))
   
   (jazz.define-macro (%%fx+ x y)
     (if jazz.debug-core?
         `(+ ,x ,y)
       `(##fixnum.+ ,x ,y)))
   
   (jazz.define-macro (%%fx- x y)
     (if jazz.debug-core?
         `(- ,x ,y)
       `(##fixnum.- ,x ,y)))
   
   (jazz.define-macro (%%fx* x y)
     (if jazz.debug-core?
         `(* ,x ,y)
       `(##fixnum.* ,x ,y)))
   
   (jazz.define-macro (%%fxquotient x y)
     (if jazz.debug-core?
         `(quotient ,x ,y)
       `(##fxquotient ,x ,y)))))


;;;
;;;; Flonum
;;;


(cond-expand
  (gambit
   (jazz.define-macro (%%flonum? obj)
     (if jazz.debug-core?
         `(flonum? ,obj)
       `(##flonum? ,obj)))
   
   (jazz.define-macro (%%fl= x y)
     (if jazz.debug-core?
         `(= ,x ,y)
       `(##flonum.= ,x ,y)))
   
   (jazz.define-macro (%%fl< x y)
     (if jazz.debug-core?
         `(< ,x ,y)
       `(##flonum.< ,x ,y)))
   
   (jazz.define-macro (%%fl<= x y)
     (if jazz.debug-core?
         `(<= ,x ,y)
       `(##flonum.<= ,x ,y)))
   
   (jazz.define-macro (%%fl> x y)
     (if jazz.debug-core?
         `(> ,x ,y)
       `(##flonum.> ,x ,y)))
   
   (jazz.define-macro (%%fl>= x y)
     (if jazz.debug-core?
         `(>= ,x ,y)
       `(##flonum.>= ,x ,y)))
   
   (jazz.define-macro (%%fl+ x y)
     (if jazz.debug-core?
         `(+ ,x ,y)
       `(##flonum.+ ,x ,y)))
   
   (jazz.define-macro (%%fl- x y)
     (if jazz.debug-core?
         `(- ,x ,y)
       `(##flonum.- ,x ,y)))
   
   (jazz.define-macro (%%fl* x y)
     (if jazz.debug-core?
         `(* ,x ,y)
       `(##flonum.* ,x ,y)))
   
   (jazz.define-macro (%%fl/ x y)
     (if jazz.debug-core?
         `(/ ,x ,y)
       `(##flonum./ ,x ,y))))

  (else
   (jazz.define-macro (%%fl> x y)
     `(> ,x ,y))
   
   (jazz.define-macro (%%fl+ x y)
     `(+ ,x ,y))))


;;;
;;;; Foreign
;;;


(cond-expand
  (gambit
   (jazz.define-macro (%%foreign? obj)
     `(##foreign? ,obj)))
  
  (else))


;;;
;;;; Keyword
;;;


(cond-expand
  (gambit
    (jazz.define-macro (%%keyword? obj)
      (if jazz.debug-core?
          `(keyword? ,obj)
        `(##keyword? ,obj)))
   
    (jazz.define-macro (%%string->keyword str)
      (if jazz.debug-core?
          `(string->keyword ,str)
        `(##string->keyword ,str)))
   
   (jazz.define-macro (%%keyword->string keyword)
     (if jazz.debug-core?
         `(keyword->string ,keyword)
       `(##keyword->string ,keyword))))

  (else))


;;;
;;;; List
;;;


(cond-expand
  (gambit
    (jazz.define-macro (%%null? obj)
      (if jazz.debug-core?
          `(null? ,obj)
        `(##null? ,obj)))
    
    (jazz.define-macro (%%pair? obj)
      (if jazz.debug-core?
          `(pair? ,obj)
        `(##pair? ,obj)))
    
    (jazz.define-macro (%%car pair)
      (if jazz.debug-core?
          `(car ,pair)
        `(##car ,pair)))
    
    (jazz.define-macro (%%cdr pair)
      (if jazz.debug-core?
          `(cdr ,pair)
        `(##cdr ,pair)))
    
    (jazz.define-macro (%%set-car! pair val)
      (if jazz.debug-core?
          `(set-car! ,pair ,val)
        `(##set-car! ,pair ,val)))
    
    (jazz.define-macro (%%set-cdr! pair val)
      (if jazz.debug-core?
          `(set-cdr! ,pair ,val)
        `(##set-cdr! ,pair ,val)))
    
    (jazz.define-macro (%%caar pair)
      (if jazz.debug-core?
          `(caar ,pair)
        `(##caar ,pair)))
    
    (jazz.define-macro (%%cadr pair)
      (if jazz.debug-core?
          `(cadr ,pair)
        `(##cadr ,pair)))
    
    (jazz.define-macro (%%cdar pair)
      (if jazz.debug-core?
          `(cdar ,pair)
        `(##cdar ,pair)))
    
    (jazz.define-macro (%%cddr pair)
      (if jazz.debug-core?
          `(cddr ,pair)
        `(##cddr ,pair)))
    
    (jazz.define-macro (%%length lst)
      (if jazz.debug-core?
          `(length ,lst)
        `(##length ,lst)))
    
    (jazz.define-macro (%%memq obj lst)
      (if jazz.debug-core?
          `(memq ,obj ,lst)
        `(##memq ,obj ,lst)))
    
    (jazz.define-macro (%%memv obj lst)
      `(memv ,obj ,lst))
    
    (jazz.define-macro (%%assq obj alist)
      (if jazz.debug-core?
          `(assq ,obj ,alist)
        `(##assq ,obj ,alist)))
    
    (jazz.define-macro (%%assv obj alist)
      (if jazz.debug-core?
          `(assv ,obj ,alist)
        `(##assv ,obj ,alist)))
    
    (jazz.define-macro (%%assoc obj alist)
      (if jazz.debug-core?
          `(assoc ,obj ,alist)
        `(##assoc ,obj ,alist)))
    
    (jazz.define-macro (%%cons x y)
      (if jazz.debug-core?
          `(cons ,x ,y)
        `(##cons ,x ,y)))
    
    (jazz.define-macro (%%list . rest)
      (if jazz.debug-core?
          `(list ,@rest)
        `(##list ,@rest)))
    
    (jazz.define-macro (%%append x y)
      (if jazz.debug-core?
          `(append ,x ,y)
        `(##append ,x ,y)))
    
    (jazz.define-macro (%%reverse lst)
      (if jazz.debug-core?
          `(reverse ,lst)
        `(##reverse ,lst)))
    
    (jazz.define-macro (%%list->vector lst)
      (if jazz.debug-core?
          `(list->vector ,lst)
        `(##list->vector ,lst))))
  
  (else
   (jazz.define-macro (%%memq obj lst)
     `(memq ,obj ,lst))))


;;;
;;;; Number
;;;


(cond-expand
  (gambit
    (jazz.define-macro (%%number? obj)
      (if jazz.debug-core?
          `(number? ,obj)
        `(##number? ,obj)))
    
    (jazz.define-macro (%%integer? obj)
      (if jazz.debug-core?
          `(integer? ,obj)
        `(##integer? ,obj)))
    
    (jazz.define-macro (%%real? obj)
      (if jazz.debug-core?
          `(real? ,obj)
        `(##real? ,obj)))
    
    (jazz.define-macro (%%number->string n)
      (if jazz.debug-core?
          `(number->string ,n)
        `(##number->string ,n))))

  (else))


;;;
;;;; Port
;;;


(cond-expand
  (gambit
    (jazz.define-macro (%%port? obj)
      (if jazz.debug-core?
          `(port? ,obj)
        `(##port? ,obj)))
    
    (jazz.define-macro (%%eof-object? obj)
      (if jazz.debug-core?
          `(eof-object? ,obj)
        `(##eof-object? ,obj))))
  
  (else))


;;;
;;;; Rational
;;;


(cond-expand
  (gambit
    (jazz.define-macro (%%rational? obj)
      (if jazz.debug-core?
          `(rational? ,obj)
        `(##rational? ,obj))))

  (else))


;;;
;;;; String
;;;


(cond-expand
  (gambit
    (jazz.define-macro (%%string? obj)
      (if jazz.debug-core?
          `(string? ,obj)
        `(##string? ,obj)))
    
    (jazz.define-macro (%%string=? str1 str2)
      (if jazz.debug-core?
          `(string=? ,str1 ,str2)
        `(##string=? ,str1 ,str2)))
    
    (jazz.define-macro (%%string-ci=? str1 str2)
      (if jazz.debug-core?
          `(string-ci=? ,str1 ,str2)
        `(##string-ci=? ,str1 ,str2)))
   
    (jazz.define-macro (%%string-length str)
      (if jazz.debug-core?
          `(string-length ,str)
        `(##string-length ,str)))
   
   (jazz.define-macro (%%string-ref str pos)
     (if jazz.debug-core?
         `(string-ref ,str ,pos)
       `(##string-ref ,str ,pos)))
   
   (jazz.define-macro (%%string-set! str pos val)
     (if jazz.debug-core?
         `(string-set! ,str ,pos ,val)
       `(##string-set! ,str ,pos ,val)))
   
   (jazz.define-macro (%%substring str start end)
     (if jazz.debug-core?
         `(substring ,str ,start ,end)
       `(##substring ,str ,start ,end)))
   
   (jazz.define-macro (%%string-append . rest)
     (if jazz.debug-core?
         `(string-append ,@rest)
       `(##string-append ,@rest))))

  (else))


;;;
;;;; Symbol
;;;


(cond-expand
  (gambit
    (jazz.define-macro (%%symbol? obj)
      (if jazz.debug-core?
          `(symbol? ,obj)
        `(##symbol? ,obj)))
   
    (jazz.define-macro (%%string->symbol str)
      (if jazz.debug-core?
          `(string->symbol ,str)
        `(##string->symbol ,str)))
   
   (jazz.define-macro (%%symbol->string symbol)
     (if jazz.debug-core?
         `(symbol->string ,symbol)
       `(##symbol->string ,symbol))))

  (else))


;;;
;;;; Syntax
;;;


(cond-expand
  (gambit
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
    (jazz.define-macro (%%table? obj)
      `(table? ,obj))
    
    (jazz.define-macro (%%make-table #!key (test eq?) (hash #f))
      `(if (eq? ,hash #f)
           (make-table test: ,test)
         (make-table test: ,test hash: ,hash)))
    
    (jazz.define-macro (%%table-ref table key default)
      (if jazz.debug-core?
          `(table-ref ,table ,key ,default)
        `(##table-ref ,table ,key ,default)))
    
    (jazz.define-macro (%%table-set! table key value)
      (if jazz.debug-core?
          `(table-set! ,table ,key ,value)
        `(##table-set! ,table ,key ,value)))
    
    (jazz.define-macro (%%table-clear table key)
      `(table-set! ,table ,key))
    
    (jazz.define-macro (%%table-keys table)
      `(map car (table->list ,table)))
    
    (jazz.define-macro (%%table-length table)
      `(table-length ,table))
    
    (jazz.define-macro (%%iterate-table table proc)
      `(table-for-each ,proc ,table))
    
    (jazz.define-macro (%%table-merge! table additions #!optional (additions-takes-precedence? #f))
      (if jazz.debug-core?
          `(table-merge! ,table ,additions ,additions-takes-precedence?)
        `(##table-merge! ,table ,additions ,additions-takes-precedence?)))
    
    (jazz.define-macro (%%list->table alist test)
      `(list->table ,alist test: ,test))
    
    (jazz.define-macro (%%table->list table)
      `(table->list ,table))
    
    (jazz.define-macro (%%table-entries table)
      `(table-length ,table))
    
    (jazz.define-macro (%%copy-table table)
      `(table-copy ,table)))
  
  (else))


;;;
;;;; Values
;;;


(cond-expand
  (gambit
    (jazz.define-macro (%%values? obj)
      `(##values? ,obj)))
  (else))


;;;
;;;; Vector
;;;


(cond-expand
  (gambit
    (jazz.define-macro (%%vector? obj)
      (if jazz.debug-core?
          `(vector? ,obj)
        `(##vector? ,obj)))
   
   (jazz.define-macro (%%vector . rest)
     (if jazz.debug-core?
         `(vector ,@rest)
       `(##vector ,@rest)))
   
   (jazz.define-macro (%%make-vector size . rest)
     (if jazz.debug-core?
         `(make-vector ,size ,@rest)
       `(##make-vector ,size ,@rest)))
   
   (jazz.define-macro (%%vector-length vector)
     (if jazz.debug-core?
         `(vector-length ,vector)
       `(##vector-length ,vector)))
   
   (jazz.define-macro (%%vector-ref vector n)
     (if jazz.debug-core?
         `(vector-ref ,vector ,n)
       `(##vector-ref ,vector ,n)))
   
   (jazz.define-macro (%%vector-set! vector n value)
     (if jazz.debug-core?
         `(vector-set! ,vector ,n ,value)
       `(##vector-set! ,vector ,n ,value)))
   
   (jazz.define-macro (%%vector-copy vector . rest)
     (if jazz.debug-core?
         `(vector-copy ,vector ,@rest)
       `(##vector-copy ,vector ,@rest)))
   
   (jazz.define-macro (%%vector->list vector)
     (if jazz.debug-core?
         `(vector->list ,vector)
       `(##vector->list ,vector))))

  (else
   (jazz.define-macro (%%vector? obj)
     `(vector? ,obj))
   
   (jazz.define-macro (%%vector . rest)
     `(vector ,@rest))
   
   (jazz.define-macro (%%make-vector size . rest)
     `(make-vector ,size ,@rest))
   
   (jazz.define-macro (%%vector-length vector)
     `(vector-length ,vector))
   
   (jazz.define-macro (%%vector-ref vector n)
     `(vector-ref ,vector ,n))
   
   (jazz.define-macro (%%vector-set! vector n value)
     `(vector-set! ,vector ,n ,value))
   
   (jazz.define-macro (%%vector-copy vector . rest)
     `(vector-copy ,vector ,@rest))))
