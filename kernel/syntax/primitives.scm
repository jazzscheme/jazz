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


(block kernel.primitives


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
;;;; Box
;;;


(cond-expand
  (gambit
   (jazz.define-macro (%%box? obj)
     (if jazz.debug-core?
         `(box? ,obj)
       `(##box? ,obj)))
   
   (jazz.define-macro (%%box obj)
     (if jazz.debug-core?
         `(box ,obj)
       `(##box ,obj)))
   
   (jazz.define-macro (%%unbox box)
     (if jazz.debug-core?
         `(unbox ,box)
       `(##unbox ,box))))
  
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
        `(##char=? ,c1 ,c2)))
    
    (jazz.define-macro (%%char<=? c1 c2)
      (if jazz.debug-core?
          `(char<=? ,c1 ,c2)
        `(##char<=? ,c1 ,c2))))

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
   
   (jazz.define-macro (%%continuation-graft-no-winding cont values)
     (%%force-uniqueness (cont values)
       `(%%check-continuation ,cont 1 (%%continuation-graft-no-winding ,cont ,values)
          (##continuation-graft-no-winding ,cont ,values))))
   
   (jazz.define-macro (%%continuation-return-no-winding cont values)
     (%%force-uniqueness (cont values)
       `(%%check-continuation ,cont 1 (%%continuation-return-no-winding ,cont ,values)
          (##continuation-return-no-winding ,cont ,values))))
   
   (jazz.define-macro (%%continuation-creator cont)
     (%%force-uniqueness (cont)
       `(%%check-continuation ,cont 1 (%%continuation-creator ,cont)
          (##continuation-creator ,cont))))
   
   (jazz.define-macro (%%continuation-locat cont)
     (%%force-uniqueness (cont)
       `(%%check-continuation ,cont 1 (%%continuation-locat ,cont)
          (##continuation-locat ,cont))))
   
   (jazz.define-macro (%%continuation-locals cont)
     (%%force-uniqueness (cont)
       `(%%check-continuation ,cont 1 (%%continuation-locals ,cont)
          (##continuation-locals ,cont))))
   
   (jazz.define-macro (%%continuation-next cont)
     (%%force-uniqueness (cont)
       `(%%check-continuation ,cont 1 (%%continuation-next ,cont)
          (##continuation-next ,cont))))
   
   (jazz.define-macro (%%continuation-first-frame cont all-frames?)
     (%%force-uniqueness (cont all-frames?)
       `(%%check-continuation ,cont 1 (%%continuation-first-frame ,cont ,all-frames?)
          (##continuation-first-frame ,cont ,all-frames?))))
   
   (jazz.define-macro (%%continuation-next-frame cont all-frames?)
     (%%force-uniqueness (cont all-frames?)
       `(%%check-continuation ,cont 1 (%%continuation-next-frame ,cont ,all-frames?)
          (##continuation-next-frame ,cont ,all-frames?))))
   
   (jazz.define-macro (%%interp-continuation? cont)
     (%%force-uniqueness (cont)
       `(%%check-continuation ,cont 1 (%%interp-continuation? ,cont)
          (##interp-continuation? ,cont)))))
  
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
   (jazz.define-macro (%%load path script-callback clone-cte? raise-os-exception? quiet?)
     (%%force-uniqueness (path script-callback clone-cte? raise-os-exception? quiet?)
       `(##load ,path ,script-callback ,clone-cte? ,raise-os-exception? ,quiet?))))
  
  (else))


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
   
   (jazz.define-macro (%%still-obj-refcount-dec! foreign)
     (%%force-uniqueness (foreign)
       `(%%check-foreign ,foreign 1 (%%still-obj-refcount-dec! ,foreign)
          (##still-obj-refcount-dec! ,foreign))))
   
   (jazz.define-macro (%%still-obj-refcount-inc! foreign)
     (%%force-uniqueness (foreign)
       `(%%check-foreign ,foreign 1 (%%still-obj-refcount-inc! ,foreign)
          (##still-obj-refcount-inc! ,foreign)))))
  
  (else))


;;;
;;;; Interruption
;;;


(cond-expand
  (gambit
    (jazz.define-macro (%%interrupt-handler code)
      `(##interrupt-handler ,code))
    
    (jazz.define-macro (%%interrupt-vector-set! code handler)
      `(##interrupt-vector-set! ,code ,handler)))
  
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
    
    (jazz.define-macro (%%remove elem lst)
      (%%force-uniqueness (elem lst)
       `(%%check-list ,lst 2 (%%remove ,elem ,lst)
          (##remove ,elem ,lst))))
    
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
        `(##number->string ,n)))
   
    (jazz.define-macro (%%string->number str)
      (if jazz.debug-core?
          `(string->number ,str)
        `(##string->number ,str))))

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
    
    (jazz.define-macro (%%input-port-names-set! port names)
      (%%force-uniqueness (port names)
        `(%%check-port ,port 1 (%%input-port-names-set! ,port ,names)
           ;; hack to set the names of the port until there is a setter
           (##vector-set! ,port 4 ,names))))
    
    (jazz.define-macro (%%input-port-line-set! port line)
      (%%force-uniqueness (port line)
        `(%%check-port ,port 1 (%%input-port-line-set! ,port ,line)
          (##input-port-line-set! ,port ,line))))
    
    (jazz.define-macro (%%input-port-column-set! port col)
      (%%force-uniqueness (port col)
        `(%%check-port ,port 1 (%%input-port-column-set! ,port ,col)
           (##input-port-column-set! ,port ,col))))
    
    (jazz.define-macro (%%read-all-as-a-begin-expr-from-port port readtable wrap unwrap start-syntax close-port?)
      (%%force-uniqueness (port readtable wrap unwrap start-syntax close-port?)
        `(%%check-port ,port 1 (%%read-all-as-a-begin-expr-from-port ,port ,readtable ,wrap ,unwrap ,start-syntax ,close-port?)
           (%%check-readtable ,readtable 2 (%%read-all-as-a-begin-expr-from-port ,port ,readtable ,wrap ,unwrap ,start-syntax ,close-port?)
             (##read-all-as-a-begin-expr-from-port ,port ,readtable ,wrap ,unwrap ,start-syntax ,close-port?)))))
    
    (jazz.define-macro (%%write-string str port)
      (%%force-uniqueness (str port)
        `(%%check-string ,str 1 (%%write-string ,str ,port)
           (%%check-port ,port 2 (%%write-string ,str ,port)
             (##write-string ,str ,port))))))
  
  (else))


;;;
;;;; Procedure
;;;


(cond-expand
  (gambit
    (jazz.define-macro (%%procedure-name procedure)
      (%%force-uniqueness (procedure)
        `(%%check-procedure ,procedure 1 (%%procedure-name ,procedure)
           (##procedure-name ,procedure)))))
  
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
    
    (jazz.define-macro (%%readenv-current-filepos readenv)
      (%%force-uniqueness (readenv)
        `(%%check-readenv ,readenv 1 (%%readenv-current-filepos ,readenv)
           (##readenv-current-filepos ,readenv))))
    
    (jazz.define-macro (%%build-list readenv allow-improper? start-pos close)
      (%%force-uniqueness (readenv allow-improper? start-pos close)
        `(%%check-readenv ,readenv 1 (%%build-list ,readenv ,allow-improper? ,start-pos ,close)
           (##build-list ,readenv ,allow-improper? ,start-pos ,close))))
    
    (jazz.define-macro (%%read-datum-or-label-or-none-or-dot readenv)
      (%%force-uniqueness (readenv)
        `(%%check-readenv ,readenv 1 (%%read-datum-or-label-or-none-or-dot ,readenv)
           (##read-datum-or-label-or-none-or-dot ,readenv)))))

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
    
    (jazz.define-macro (%%readtable-copy readtable)
      (%%force-uniqueness (readtable)
        `(%%check-readtable ,readtable 1 (%%readtable-copy ,readtable)
           (##readtable-copy ,readtable))))
    
    (jazz.define-macro (%%readtable-char-class-set! readtable c delimiter? handler)
      (%%force-uniqueness (readtable c delimiter? handler)
        `(%%check-readtable ,readtable 1 (%%readtable-char-class-set! ,readtable ,c ,delimiter? ,handler)
           (##readtable-char-class-set! ,readtable ,c ,delimiter? ,handler))))
    
    (jazz.define-macro (%%readtable-char-sharp-handler-set! readtable c handler)
      (%%force-uniqueness (readtable c handler)
        `(%%check-readtable ,readtable 1 (%%readtable-char-sharp-handler-set! ,readtable ,c ,handler)
           (##readtable-char-sharp-handler-set! ,readtable ,c ,handler)))))

  (else))


;;;
;;;; Repl
;;;


(cond-expand
  (gambit
    (jazz.define-macro (%%repl #!optional (write-reason #f))
      `(##repl ,write-reason))
    
    (jazz.define-macro (%%thread-repl-context-get!)
      `(##thread-repl-context-get!))
    
    (jazz.define-macro (%%thread-repl-channel-get! thread)
      `(##thread-repl-channel-get! ,thread))
    
    (jazz.define-macro (%%repl-channel-result-history-add channel result)
      `(##repl-channel-result-history-add ,channel ,result)))
  
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
    
    (jazz.define-macro (%%string<? str1 str2)
      (if jazz.debug-core?
          `(string<? ,str1 ,str2)
        `(##string<? ,str1 ,str2)))
   
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
   
   (jazz.define-macro (%%string-shrink! str len)
     (%%force-uniqueness (str len)
       `(%%check-string ,str 1 (%%string-shrink! ,str ,len)
          (##string-shrink! ,str ,len)))))

  (else))


;;;
;;;; Structure
;;;


(cond-expand
  (gambit
    (jazz.define-macro (%%structure? obj)
      `(##structure? ,obj))
    
    (jazz.define-macro (%%structure-type structure)
      `(##structure-type ,structure)))
  
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
   
   (jazz.define-macro (%%global-var? symbol)
     (%%force-uniqueness (symbol)
       `(%%check-symbol ,symbol 1 (%%global-var? ,symbol)
          (##global-var? ,symbol))))
   
   (jazz.define-macro (%%global-var-ref symbol)
     (%%force-uniqueness (symbol)
       `(%%check-symbol ,symbol 1 (%%global-var-ref ,symbol)
          (##global-var-ref ,symbol))))
   
   (jazz.define-macro (%%global-var-set! symbol value)
     (%%force-uniqueness (symbol)
       `(%%check-symbol ,symbol 1 (%%global-var-ref ,symbol)
          (##global-var-set! ,symbol ,value)))))

  (else))


;;;
;;;; Syntax
;;;


(cond-expand
  (gambit
    (jazz.define-macro (%%source? expr)
      `(##source? ,expr))
    
    (jazz.define-macro (%%source-code src)
      (%%force-uniqueness (src)
        `(%%check-source ,src 1 (%%source-code ,src)
           (##source-code ,src))))
    
    (jazz.define-macro (%%source-locat src)
      (%%force-uniqueness (src)
        `(%%check-source ,src 1 (%%source-locat ,src)
           (##source-locat ,src))))
    
    (jazz.define-macro (%%desourcify expr)
      `(##desourcify ,expr))
    
    (jazz.define-macro (%%make-source code locat)
      `(##make-source ,code ,locat))
    
    (jazz.define-macro (%%sourcify expr src)
      (%%force-uniqueness (expr src)
        `(%%check-source ,src 2 (%%sourcify ,expr ,src)
           (##sourcify ,expr ,src))))
    
    (jazz.define-macro (%%locat? expr)
      `(##locat? ,expr))
    
    (jazz.define-macro (%%locat-container locat)
      (%%force-uniqueness (locat)
        `(%%check-locat ,locat 1 (%%locat-container ,locat)
           (##locat-container ,locat))))
    
    (jazz.define-macro (%%locat-position locat)
      (%%force-uniqueness (locat)
        `(%%check-locat ,locat 1 (%%locat-position ,locat)
           (##locat-position ,locat))))
    
    (jazz.define-macro (%%container->path container)
      `(##container->path ,container))
    
    (jazz.define-macro (%%position->filepos position)
      `(##position->filepos ,position))
    
    (jazz.define-macro (%%filepos-line filepos)
      (%%force-uniqueness (filepos)
        `(%%check-fixnum ,filepos 1 (%%filepos-line ,filepos)
           (##filepos-line ,filepos))))
    
    (jazz.define-macro (%%filepos-col filepos)
      (%%force-uniqueness (filepos)
        `(%%check-fixnum ,filepos 1 (%%filepos-col ,filepos)
           (##filepos-col ,filepos))))))


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
    
    (jazz.define-macro (%%list->table alist . rest)
      `(list->table ,alist ,@rest))
    
    (jazz.define-macro (%%table->list table)
      `(table->list ,table))
    
    (jazz.define-macro (%%table-entries table)
      `(map cdr (table->list ,table)))
    
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
   
   (jazz.define-macro (%%s8vector? obj)
     (if jazz.debug-core?
         `(s8vector? ,obj)
       `(##s8vector? ,obj)))
   
   (jazz.define-macro (%%u8vector? obj)
     (if jazz.debug-core?
         `(u8vector? ,obj)
       `(##u8vector? ,obj)))
   
   (jazz.define-macro (%%s16vector? obj)
     (if jazz.debug-core?
         `(s16vector? ,obj)
       `(##s16vector? ,obj)))
   
   (jazz.define-macro (%%u16vector? obj)
     (if jazz.debug-core?
         `(u16vector? ,obj)
       `(##u16vector? ,obj)))
   
   (jazz.define-macro (%%s32vector? obj)
     (if jazz.debug-core?
         `(s32vector? ,obj)
       `(##s32vector? ,obj)))
   
   (jazz.define-macro (%%u32vector? obj)
     (if jazz.debug-core?
         `(u32vector? ,obj)
       `(##u32vector? ,obj)))
   
   (jazz.define-macro (%%s64vector? obj)
     (if jazz.debug-core?
         `(s64vector? ,obj)
       `(##s64vector? ,obj)))
   
   (jazz.define-macro (%%u64vector? obj)
     (if jazz.debug-core?
         `(u64vector? ,obj)
       `(##u64vector? ,obj)))
   
   (jazz.define-macro (%%f32vector? obj)
     (if jazz.debug-core?
         `(f32vector? ,obj)
       `(##f32vector? ,obj)))
   
   (jazz.define-macro (%%f64vector? obj)
     (if jazz.debug-core?
         `(f64vector? ,obj)
       `(##f64vector? ,obj))))

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
     `(vector-copy ,vector ,@rest)))))
