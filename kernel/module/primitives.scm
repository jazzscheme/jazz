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
     (if (jazz.safe?)
         `(not ,expr)
       `(##not ,expr))))
  
  (else))


;;;
;;;; Char
;;;


(cond-expand
  (gambit
    (define-macro (%%char? obj)
      (if (jazz.safe?)
          `(char? ,obj)
        `(##char? ,obj))))

  (else))


;;;
;;;; Complex
;;;


(cond-expand
  (gambit
    (define-macro (%%complex? obj)
      (if (jazz.safe?)
          `(complex? ,obj)
        `(##complex? ,obj))))

  (else))


;;;
;;;; Control
;;;


(cond-expand
  (gambit
   (define-macro (%%procedure? obj)
     (if (jazz.safe?)
         `(procedure? ,obj)
       `(##procedure? ,obj)))
   
   (define-macro (%%apply proc lst)
     (if (jazz.safe?)
         `(apply ,proc ,lst)
       `(##apply ,proc ,lst))))
  
  (else))


;;;
;;;; Equality
;;;


(cond-expand
  (gambit
   (define-macro (%%eq? x y)
     (if (jazz.safe?)
         `(eq? ,x ,y)
       `(##eq? ,x ,y)))
   
   (define-macro (%%neq? x y)
     `(%%not (%%eq? ,x ,y)))
   
   (define-macro (%%eqv? x y)
     (if (jazz.safe?)
         `(eqv? ,x ,y)
       `(##eqv? ,x ,y)))
   
   (define-macro (%%equal? x y)
     (if (jazz.safe?)
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
     (if (jazz.safe?)
         `(fixnum? ,obj)
       `(##fixnum? ,obj)))
   
   (define-macro (%%fx= x y)
     (if (jazz.safe?)
         `(= ,x ,y)
       `(##fixnum.= ,x ,y)))
   
   (define-macro (%%fx< x y)
     (if (jazz.safe?)
         `(< ,x ,y)
       `(##fixnum.< ,x ,y)))
   
   (define-macro (%%fx<= x y)
     (if (jazz.safe?)
         `(<= ,x ,y)
       `(##fixnum.<= ,x ,y)))
   
   (define-macro (%%fx> x y)
     (if (jazz.safe?)
         `(> ,x ,y)
       `(##fixnum.> ,x ,y)))
   
   (define-macro (%%fx>= x y)
     (if (jazz.safe?)
         `(>= ,x ,y)
       `(##fixnum.>= ,x ,y)))
   
   (define-macro (%%fx+ x y)
     (if (jazz.safe?)
         `(+ ,x ,y)
       `(##fixnum.+ ,x ,y)))
   
   (define-macro (%%fx- x y)
     (if (jazz.safe?)
         `(- ,x ,y)
       `(##fixnum.- ,x ,y)))
   
   (define-macro (%%fx* x y)
     (if (jazz.safe?)
         `(* ,x ,y)
       `(##fixnum.* ,x ,y))))

  (else
   (define-macro (%%fx> x y)
     `(> ,x ,y))
   
   (define-macro (%%fx+ x y)
     `(+ ,x ,y))))


;;;
;;;; Flonum
;;;


(cond-expand
  (gambit
   (define-macro (%%flonum? obj)
     (if (jazz.safe?)
         `(flonum? ,obj)
       `(##flonum? ,obj)))
   
   (define-macro (%%fl= x y)
     (if (jazz.safe?)
         `(= ,x ,y)
       `(##flonum.= ,x ,y)))
   
   (define-macro (%%fl< x y)
     (if (jazz.safe?)
         `(< ,x ,y)
       `(##flonum.< ,x ,y)))
   
   (define-macro (%%fl<= x y)
     (if (jazz.safe?)
         `(<= ,x ,y)
       `(##flonum.<= ,x ,y)))
   
   (define-macro (%%fl> x y)
     (if (jazz.safe?)
         `(> ,x ,y)
       `(##flonum.> ,x ,y)))
   
   (define-macro (%%fl>= x y)
     (if (jazz.safe?)
         `(>= ,x ,y)
       `(##flonum.>= ,x ,y)))
   
   (define-macro (%%fl+ x y)
     (if (jazz.safe?)
         `(+ ,x ,y)
       `(##flonum.+ ,x ,y)))
   
   (define-macro (%%fl- x y)
     (if (jazz.safe?)
         `(- ,x ,y)
       `(##flonum.- ,x ,y)))
   
   (define-macro (%%fl* x y)
     (if (jazz.safe?)
         `(* ,x ,y)
       `(##flonum.* ,x ,y)))
   
   (define-macro (%%fl/ x y)
     (if (jazz.safe?)
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
;;;; Hashtable
;;;


(cond-expand
  (gambit
    (define-macro (%%hashtable? obj)
      `(table? ,obj))
    
    (define-macro (%%make-hashtable test #!optional (hash #f))
      `(if (eq? ,hash #f)
           (make-table test: ,test)
         (make-table test: ,test hash: ,hash)))
    
    (define-macro (%%hashtable-ref hashtable key default)
      (if (jazz.safe?)
          `(table-ref ,hashtable ,key ,default)
        `(##table-ref ,hashtable ,key ,default)))
    
    (define-macro (%%hashtable-set! hashtable key value)
      (if (jazz.safe?)
          `(table-set! ,hashtable ,key ,value)
        `(##table-set! ,hashtable ,key ,value)))
    
    (define-macro (%%hashtable-clear hashtable key)
      `(table-set! ,hashtable ,key))
    
    (define-macro (%%hashtable-keys hashtable)
      `(map car (table->list ,hashtable)))
    
    (define-macro (%%hashtable-length hashtable)
      `(table-length ,hashtable))
    
    (define-macro (%%iterate-hashtable hashtable proc)
      `(table-for-each ,proc ,hashtable))
    
    (define-macro (%%alist->hashtable alist test)
      `(list->table ,alist test: ,test))
    
    (define-macro (%%hashtable->alist hashtable)
      `(table->list ,hashtable))
    
    (define-macro (%%hashtable-entries hashtable)
      `(table-length ,hashtable))
    
    (define-macro (%%copy-hashtable hashtable)
      `(table-copy ,hashtable)))
  
  (else))


;;;
;;;; Keyword
;;;


(cond-expand
  (gambit
    (define-macro (%%keyword? obj)
      (if (jazz.safe?)
          `(keyword? ,obj)
        `(##keyword? ,obj)))
   
    (define-macro (%%string->keyword str)
      (if (jazz.safe?)
          `(string->keyword ,str)
        `(##string->keyword ,str)))
   
   (define-macro (%%keyword->string keyword)
     (if (jazz.safe?)
         `(keyword->string ,keyword)
       `(##keyword->string ,keyword))))

  (else))


;;;
;;;; List
;;;


(cond-expand
  (gambit
    (define-macro (%%null? obj)
      (if (jazz.safe?)
          `(null? ,obj)
        `(##null? ,obj)))
    
    (define-macro (%%pair? obj)
      (if (jazz.safe?)
          `(pair? ,obj)
        `(##pair? ,obj)))
    
    (define-macro (%%car pair)
      (if (jazz.safe?)
          `(car ,pair)
        `(##car ,pair)))
    
    (define-macro (%%cdr pair)
      (if (jazz.safe?)
          `(cdr ,pair)
        `(##cdr ,pair)))
    
    (define-macro (%%set-car! pair val)
      (if (jazz.safe?)
          `(set-car! ,pair ,val)
        `(##set-car! ,pair ,val)))
    
    (define-macro (%%set-cdr! pair val)
      (if (jazz.safe?)
          `(set-cdr! ,pair ,val)
        `(##set-cdr! ,pair ,val)))
    
    (define-macro (%%caar pair)
      (if (jazz.safe?)
          `(caar ,pair)
        `(##caar ,pair)))
    
    (define-macro (%%cadr pair)
      (if (jazz.safe?)
          `(cadr ,pair)
        `(##cadr ,pair)))
    
    (define-macro (%%cdar pair)
      (if (jazz.safe?)
          `(cdar ,pair)
        `(##cdar ,pair)))
    
    (define-macro (%%cddr pair)
      (if (jazz.safe?)
          `(cddr ,pair)
        `(##cddr ,pair)))
    
    (define-macro (%%length lst)
      (if (jazz.safe?)
          `(length ,lst)
        `(##length ,lst)))
    
    (define-macro (%%memq obj lst)
      (if (jazz.safe?)
          `(memq ,obj ,lst)
        `(##memq ,obj ,lst)))
    
    (define-macro (%%memv obj lst)
      `(memv ,obj ,lst))
    
    (define-macro (%%cons x y)
      (if (jazz.safe?)
          `(cons ,x ,y)
        `(##cons ,x ,y)))
    
    (define-macro (%%list . rest)
      (if (jazz.safe?)
          `(list ,@rest)
        `(##list ,@rest)))
    
    (define-macro (%%append x y)
      (if (jazz.safe?)
          `(append ,x ,y)
        `(##append ,x ,y)))
    
    (define-macro (%%reverse lst)
      (if (jazz.safe?)
          `(reverse ,lst)
        `(##reverse ,lst))))
  
  (else
   (define-macro (%%memq obj lst)
     `(memq ,obj ,lst))))


;;;
;;;; Number
;;;


(cond-expand
  (gambit
    (define-macro (%%number? obj)
      (if (jazz.safe?)
          `(number? ,obj)
        `(##number? ,obj)))
    
    (define-macro (%%integer? obj)
      (if (jazz.safe?)
          `(integer? ,obj)
        `(##integer? ,obj)))
    
    (define-macro (%%real? obj)
      (if (jazz.safe?)
          `(real? ,obj)
        `(##real? ,obj))))

  (else))


;;;
;;;; Port
;;;


(cond-expand
  (gambit
    (define-macro (%%port? obj)
      (if (jazz.safe?)
          `(port? ,obj)
        `(##port? ,obj)))
    
    (define-macro (%%eof-object? obj)
      (if (jazz.safe?)
          `(eof-object? ,obj)
        `(##eof-object? ,obj))))
  
  (else))


;;;
;;;; Rational
;;;


(cond-expand
  (gambit
    (define-macro (%%rational? obj)
      (if (jazz.safe?)
          `(rational? ,obj)
        `(##rational? ,obj))))

  (else))


;;;
;;;; String
;;;


(cond-expand
  (gambit
    (define-macro (%%string? obj)
      (if (jazz.safe?)
          `(string? ,obj)
        `(##string? ,obj)))
   
    (define-macro (%%string-length str)
      (if (jazz.safe?)
          `(string-length ,str)
        `(##string-length ,str)))
   
   (define-macro (%%string-ref str pos)
     (if (jazz.safe?)
         `(string-ref ,str ,pos)
       `(##string-ref ,str ,pos)))
   
   (define-macro (%%string-set! str pos val)
     (if (jazz.safe?)
         `(string-set! ,str ,pos ,val)
       `(##string-set! ,str ,pos ,val)))
   
   (define-macro (%%substring str start end)
     (if (jazz.safe?)
         `(substring ,str ,start ,end)
       `(##substring ,str ,start ,end)))
   
   (define-macro (%%string-append . rest)
     (if (jazz.safe?)
         `(string-append ,@rest)
       `(##string-append ,@rest))))

  (else))


;;;
;;;; Symbol
;;;


(cond-expand
  (gambit
    (define-macro (%%symbol? obj)
      (if (jazz.safe?)
          `(symbol? ,obj)
        `(##symbol? ,obj)))
   
    (define-macro (%%string->symbol str)
      (if (jazz.safe?)
          `(string->symbol ,str)
        `(##string->symbol ,str)))
   
   (define-macro (%%symbol->string symbol)
     (if (jazz.safe?)
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
      (if (jazz.safe?)
          `(vector? ,obj)
        `(##vector? ,obj)))
   
   (define-macro (%%vector . rest)
     (if (jazz.safe?)
         `(vector ,@rest)
       `(##vector ,@rest)))
   
   (define-macro (%%make-vector size)
     (if (jazz.safe?)
         `(make-vector ,size)
       `(##make-vector ,size)))
   
   (define-macro (%%vector-length vector)
     (if (jazz.safe?)
         `(vector-length ,vector)
       `(##vector-length ,vector)))
   
   (define-macro (%%vector-ref vector n)
     (if (jazz.safe?)
         `(vector-ref ,vector ,n)
       `(##vector-ref ,vector ,n)))
   
   (define-macro (%%vector-set! vector n value)
     (if (jazz.safe?)
         `(vector-set! ,vector ,n ,value)
       `(##vector-set! ,vector ,n ,value))))

  (else
   (define-macro (%%vector? obj)
     `(vector? ,obj))
   
   (define-macro (%%vector . rest)
     `(vector ,@rest))
   
   (define-macro (%%make-vector size)
     `(make-vector ,size))
   
   (define-macro (%%vector-length vector)
     `(vector-length ,vector))
   
   (define-macro (%%vector-ref vector n)
     `(vector-ref ,vector ,n))
   
   (define-macro (%%vector-set! vector n value)
     `(vector-set! ,vector ,n ,value))))
