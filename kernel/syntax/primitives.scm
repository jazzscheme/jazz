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


(include "~~/lib/_gambit#.scm")


(jazz.kernel-declares)


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
;;;; Continuation
;;;


(cond-expand
  (gambit
   (jazz.define-syntax %%continuation?
     (lambda (src)
       (let ((obj (##cadr (##source-code src))))
         (if jazz.debug-core?
             `(continuation? ,obj)
           `(##continuation? ,obj)))))
   
   (jazz.define-syntax %%continuation-capture
     (lambda (src)
       (let ((proc (##cadr (##source-code src))))
         (if jazz.debug-core?
             `(continuation-capture ,proc)
           `(##continuation-capture ,proc)))))
   
   (jazz.define-syntax %%continuation-graft
     (lambda (src)
       (let ((cont (##cadr (##source-code src)))
             (proc (##car (##cddr (##source-code src)))))
         (if jazz.debug-core?
             `(continuation-graft ,cont ,proc)
           `(##continuation-graft ,cont ,proc)))))
   
   (jazz.define-syntax %%continuation-return
     (lambda (src)
       (let ((cont (##cadr (##source-code src)))
             (values (##cddr (##source-code src))))
         (if jazz.debug-core?
             `(continuation-return ,cont ,@values)
           `(##continuation-return ,cont ,@values)))))
   
   (define (%%continuation-graft-no-winding cont values)
     (##continuation-graft-no-winding cont values))
   
   (define (%%continuation-return-no-winding cont values)
     (##continuation-return-no-winding cont values))
   
   (define (%%interp-continuation? cont)
     (##interp-continuation? cont))
   
   (define (%%continuation-creator cont)
     (if (%%continuation? cont)
         (##continuation-creator cont)
       (error "CONTINUATION expected" cont)))
   
   (define (%%continuation-locat cont)
     (if (%%continuation? cont)
         (##continuation-locat cont)
       (error "CONTINUATION expected" cont)))
   
   (define (%%continuation-locals cont)
     (if (%%continuation? cont)
         (##continuation-locals cont)
       (error "CONTINUATION expected" cont)))
   
   (define (%%continuation-next cont)
     (if (%%continuation? cont)
         (##continuation-next cont)
       (error "CONTINUATION expected" cont)))
   
   (define (%%continuation-first-frame cont all-frames?)
     (##continuation-first-frame cont all-frames?))
   
   (define (%%continuation-next-frame cont all-frames?)
     (##continuation-next-frame cont all-frames?)))
  
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
;;;; Eval
;;;


(cond-expand
  (gambit
   (define (%%load path script-callback clone-cte? raise-os-exception? quiet?)
     (if (##string? path)
         (##load path script-callback clone-cte? raise-os-exception? quiet?)
       (error "STRING expected" path)))))


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
     `(##foreign? ,obj))
   
   (define (%%still-obj-refcount-dec! foreign)
     (if (%%foreign? foreign)
         (##still-obj-refcount-dec! foreign)
       (error "FOREIGN expected" foreign)))
   
   (define (%%still-obj-refcount-inc! foreign)
     (if (%%foreign? foreign)
         (##still-obj-refcount-inc! foreign)
       (error "FOREIGN expected" foreign))))
  
  (else))


;;;
;;;; Interruption
;;;


(cond-expand
  (gambit
    (define (%%interrupt-handler code)
      (##interrupt-handler code))
    
    (define (%%interrupt-vector-set! code handler)
      (##interrupt-vector-set! code handler)))
  
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
    
    (jazz.define-macro (%%member obj lst)
      (if jazz.debug-core?
          `(member ,obj ,lst)
        `(##member ,obj ,lst)))
    
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
;;;; Memory
;;;


(cond-expand
  (gambit
    (jazz.define-macro (%%gc)
      `(##gc))))


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
        `(##eof-object? ,obj)))
    
    (define (%%input-port-names-set! port names)
      (if (%%port? port)
          ;; hack to set the names of the port until there is a setter
          (##vector-set! port 4 names)
        (error "PORT expected" port)))
    
    (define (%%input-port-line-set! port line)
      (if (%%port? port)
          (##input-port-line-set! port line)
        (error "PORT expected" port)))
    
    (define (%%input-port-column-set! port col)
      (if (%%port? port)
          (##input-port-column-set! port col)
        (error "PORT expected" port)))
    
    (define (%%read-all-as-a-begin-expr-from-port port readtable wrap unwrap start-syntax close-port?)
      (if (%%port? port)
          (##read-all-as-a-begin-expr-from-port port readtable wrap unwrap start-syntax close-port?)
        (error "PORT expected" port)))
    
    (define (%%write-string str port)
      (if (%%port? port)
          (##write-string str port)
        (error "PORT expected" port))))
  
  (else))


;;;
;;;; Procedure
;;;


(cond-expand
  (gambit
    (define (%%procedure-name procedure)
      (if (##procedure? procedure)
          (##procedure-name procedure)
        (error "PROCEDURE expected" procedure))))
  
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
;;;; Readenv
;;;


(cond-expand
  (gambit
    (jazz.define-macro (%%readenv? obj)
      `(macro-readenv? ,obj))
    
    (define (%%readenv-current-filepos readenv)
      (if (%%readenv? readenv)
          (##readenv-current-filepos readenv)
        (error "READENV expected" readenv)))
    
    (define (%%wrap-datum readenv expr)
      (if (%%readenv? readenv)
          (##wrap-datum readenv expr)
        (error "READENV expected" readenv)))
    
    (define (%%unwrap-datum readenv expr)
      (if (%%readenv? readenv)
          (##unwrap-datum readenv expr)
        (error "READENV expected" readenv)))
    
    (define (%%build-list readenv allow-improper? start-pos close)
      (if (%%readenv? readenv)
          (##build-list readenv allow-improper? start-pos close)
        (error "READENV expected" readenv)))
    
    (define (%%read-datum-or-label-or-none-or-dot readenv)
      (if (%%readenv? readenv)
          (##read-datum-or-label-or-none-or-dot readenv)
        (error "READENV expected" readenv))))

  (else))


;;;
;;;; Readtable
;;;


(cond-expand
  (gambit
    (jazz.define-macro (%%readtable? obj)
      (if jazz.debug-core?
          `(readtable? ,obj)
        `(##readtable? ,obj)))
    
    (jazz.define-macro (%%current-readtable)
      `(##current-readtable))
    
    (define (%%readtable-copy readtable)
      (if (%%readtable? readtable)
          (##readtable-copy readtable)
        (error "READTABLE expected" readtable)))
    
    (define (%%readtable-char-class-set! readtable c delimiter? handler)
      (if (%%readtable? readtable)
          (##readtable-char-class-set! readtable c delimiter? handler)
        (error "READTABLE expected" readtable)))
    
    (define (%%readtable-char-sharp-handler-set! readtable c handler)
      (if (%%readtable? readtable)
          (##readtable-char-sharp-handler-set! readtable c handler)
        (error "READTABLE expected" readtable))))

  (else))


;;;
;;;; Repl
;;;


(cond-expand
  (gambit
    (define (%%repl #!optional (write-reason #f))
      (if (or (##not write-reason)
              (##procedure? write-reason))
          (##repl write-reason)
        (error "PROCEDURE expected" write-reason)))
    
    (define (%%thread-repl-context-get!)
      (##thread-repl-context-get!))
    
    (define (%%thread-repl-channel-get! thread)
      (##thread-repl-channel-get! thread))
    
    (define (%%repl-channel-result-history-add channel result)
      (##repl-channel-result-history-add channel result)))
  
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
       `(##string-append ,@rest)))
   
   (define (%%string-shrink! str len)
     (if (%%string? str)
         (##string-shrink! str len)
       (error "STRING expected" str))))

  (else))


;;;
;;;; Structure
;;;


(cond-expand
  (gambit
    (define (%%structure-type structure)
      (##structure-type structure)))
  
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
       `(##symbol->string ,symbol)))
   
   (jazz.define-macro (%%unbound? obj)
     `(##unbound? ,obj))
   
   (define (%%global-var? symbol)
     (if (%%symbol? symbol)
         (##global-var? symbol)
       (error "SYMBOL expected" symbol)))
   
   (define (%%global-var-ref symbol)
     (if (%%symbol? symbol)
         (##global-var-ref symbol)
       (error "SYMBOL expected" symbol))))

  (else))


;;;
;;;; Syntax
;;;


(cond-expand
  (gambit
    (define (%%source? expr)
      (##source? expr))
    
    (define (%%source-code expr)
      (if (##source? expr)
          (##source-code expr)
        expr))
    
    (define (%%source-locat src)
      (if (##source? src)
          (##source-locat src)
        (error "SOURCE expected" src)))
    
    (define (%%desourcify expr)
      (##desourcify expr))
    
    (define (%%make-source code locat)
      (##make-source code locat))
    
    (define (%%sourcify expr src)
      (if (##source? src)
          (##sourcify expr src)
        (error "SOURCE expected" src)))
    
    (define (%%locat? expr)
      (##locat? expr))
    
    (define (%%locat-container locat)
      (if (##locat? locat)
          (##locat-container locat)
        (error "LOCAT expected" locat)))
    
    (define (%%locat-position locat)
      (if (##locat? locat)
          (##locat-position locat)
        (error "LOCAT expected" locat)))
    
    (define (%%container->path container)
      (##container->path container))
    
    (define (%%position->filepos position)
      (##position->filepos position))
    
    (define (%%filepos-line filepos)
      (##filepos-line filepos))
    
    (define (%%filepos-col filepos)
      (##filepos-col filepos))))


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
;;;; Thread
;;;


(cond-expand
  (gambit
    (jazz.define-macro (%%thread? obj)
      `(thread? ,obj))
    
    (jazz.define-macro (%%current-thread)
      `(##current-thread)))
  
  (else))


;;;
;;;; Unspecified
;;;


(cond-expand
  (gambit
    (define jazz.Unspecified-Value
      (void)))
  
  (else
    (define jazz.Unspecified-Value
      (%%list 'jazz.unspecified))))


(jazz.define-macro (%%unspecified)
  'jazz.Unspecified-Value)


(jazz.define-macro (%%unspecified? value)
  `(%%eq? ,value jazz.Unspecified-Value))


(jazz.define-macro (%%specified? value)
  `(%%neq? ,value jazz.Unspecified-Value))


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
       `(##vector->list ,vector)))
   
   (jazz.define-macro (%%u8vector? obj)
     (if jazz.debug-core?
         `(u8vector? ,obj)
       `(##u8vector? ,obj))))

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
