;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Dialect Expressions
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
;;;  Portions created by the Initial Developer are Copyright (C) 1996-2015
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


(unit protected dialect.expression


;;;
;;;; Proclaim
;;;


(jazz:define-class jazz:Proclaim jazz:Expression (constructor: jazz:allocate-proclaim)
  ((clauses getter: generate)))


(define (jazz:new-proclaim clauses)
  (jazz:allocate-proclaim #f #f clauses))


(jazz:define-method (jazz:emit-expression (jazz:Proclaim expression) declaration environment backend)
  (let ((clauses (jazz:get-proclaim-clauses expression))
        (module-declaration (jazz:get-declaration-toplevel declaration)))
    (for-each (lambda (clause)
                (jazz:proclaim module-declaration clause))
              clauses))
  #f)


(jazz:define-method (jazz:validate-proclaim (jazz:Walker walker) resume declaration environment form-src)
  (if (%%not (%%class-is? declaration jazz:Module-Declaration))
      (jazz:walk-error walker resume declaration form-src "For now, proclaim can only be used at the module level")))


(define (jazz:walk-proclaim walker resume declaration environment form-src)
  (jazz:validate-proclaim walker resume declaration environment form-src)
  (let ((form (%%desourcify form-src)))
    (let ((clauses (%%cdr form)))
      (jazz:new-proclaim clauses))))


;;;
;;;; Delay
;;;


(jazz:define-class jazz:Delay jazz:Expression (constructor: jazz:allocate-delay)
  ((expression getter: generate)))


(define (jazz:new-delay expression)
  (jazz:allocate-delay #f #f expression))


(jazz:define-method (jazz:emit-expression (jazz:Delay expression) declaration environment backend)
  (let ((expr (jazz:emit-expression (jazz:get-delay-expression expression) declaration environment backend)))
    (jazz:new-code
      (jazz:emit 'delay backend expression declaration environment expr)
      jazz:Any
      #f)))


;;;
;;;; Specialized Call
;;;


(define jazz:*specializers*
  (%%make-table test: eq?))


(define (jazz:add-specializer specialized-declaration specializer)
  (%%table-set! jazz:*specializers* specialized-declaration
    (%%append (%%table-ref jazz:*specializers* specialized-declaration '())
              (%%list specializer))))


(define (jazz:get-specializers binding)
  (%%table-ref jazz:*specializers* binding '()))


(jazz:define-variable-override jazz:emit-specialized-call
  (lambda (operator locator arguments arguments-codes call declaration environment backend)
    (if (%%not locator)
        #f
      (or (jazz:emit-specialized-locator locator arguments-codes environment backend)
          (if (%%class-is? operator jazz:Binding-Reference)
              (let ((binding (jazz:get-binding-reference-binding operator)))
                (let ((specializers (jazz:get-specializers binding)))
                  (let ((types (jazz:codes-types arguments-codes)))
                    (let iter ((scan specializers))
                         (if (%%null? scan)
                             (begin
                               ;; a bit too much for now
                               #;
                               (%%when (and (jazz:warnings?) (%%not (%%null? specializers)) (jazz:get-module-warn? (jazz:get-declaration-toplevel declaration) 'optimizations)
                                         ;; quicky to suppress duplicate warnings as for the moment those are both primitive and specialize
                                         (%%not (%%memq locator '(scheme.language.runtime.kernel:=
                                                                   scheme.language.runtime.kernel:<
                                                                   scheme.language.runtime.kernel:<=
                                                                   scheme.language.runtime.kernel:>
                                                                   scheme.language.runtime.kernel:>=
                                                                   scheme.language.runtime.kernel:+
                                                                   scheme.language.runtime.kernel:-
                                                                   scheme.language.runtime.kernel:*
                                                                   scheme.language.runtime.kernel:/))))
                                 (jazz:feedback "Warning: In {a}{a}: Unable to match call to specialized {a}"
                                                (jazz:get-declaration-locator declaration)
                                                (jazz:present-expression-location operator)
                                                (jazz:get-lexical-binding-name binding)))
                               ;; for debugging
                               ;; a bit too much for now
                               #;
                               (%%when (%%memq (jazz:get-lexical-binding-name binding) (jazz:debug-specializers))
                                 (jazz:feedback "Warning: In {a}{a}: Unable to match call to specialized {a} on {a}"
                                                (jazz:get-declaration-locator declaration)
                                                (jazz:present-expression-location operator)
                                                (jazz:get-lexical-binding-name binding)
                                                types))
                               #f)
                           (let ((specializer (%%car scan)))
                             (let ((function-type (jazz:get-lexical-binding-type specializer)))
                               (if (jazz:match-signature? arguments types function-type)
                                   (or (jazz:emit-inlined-binding-call specializer arguments-codes call declaration environment backend)
                                       (begin
                                         (jazz:add-to-module-references (jazz:get-declaration-toplevel declaration) specializer)
                                         (jazz:new-code
                                           (let ((locator (jazz:get-declaration-locator specializer)))
                                             `(,locator ,@(jazz:codes-forms arguments-codes)))
                                           (jazz:get-function-type-result function-type)
                                           #f)))
                                 (iter (%%cdr scan))))))))))
            #f)))))


;; quicky because classes are not yet defined at this point
(jazz:define-variable jazz:emit-specialized-locator)

(jazz:define-variable-override jazz:emit-specialized-locator
  (lambda (locator arguments-codes environment backend)
    #f))


;;;
;;;; New Call
;;;


(jazz:define-variable-override jazz:emit-new-call
  (lambda (operator locator arguments arguments-codes declaration environment backend)
    #f))


;;;
;;;; Primitive Call
;;;


(jazz:define-variable-override jazz:emit-primitive-call
  (lambda (operator locator arguments arguments-codes declaration environment backend)
    (jazz:emit 'primitive-call backend operator locator arguments arguments-codes declaration environment)))


;;;
;;;; Inlined Call
;;;


(jazz:define-variable-override jazz:emit-inlined-call
  (lambda (operator arguments call declaration environment backend)
    (if (%%class-is? operator jazz:Binding-Reference)
        (let ((binding (jazz:get-binding-reference-binding operator)))
          (jazz:emit-inlined-binding-call binding arguments call declaration environment backend))
      #f)))


;;;
;;;; Signature
;;;


(define (jazz:match-signature? arguments argument-types function-type)
  (let ((argcount (%%length argument-types))
        (mandatory (jazz:get-function-type-mandatory function-type))
        (positional (jazz:get-function-type-positional function-type))
        (optional (jazz:get-function-type-optional function-type))
        (named (jazz:get-function-type-named function-type))
        (rest (jazz:get-function-type-rest function-type)))
    (define (match? arg type expect)
      (if (%%class-is? expect jazz:Category-Type)
          (or (and (%%class-is? arg jazz:Binding-Reference)
                   (%%eq? (jazz:get-binding-reference-binding arg) (jazz:get-category-type-declaration expect)))
              (and (%%class-is? type jazz:Category-Type)
                   (%%eq? (jazz:get-category-type-declaration type) (jazz:get-category-type-declaration expect))))
        (%%subtype? (or type jazz:Any) expect)))
    
    (define (match-positional?)
      (and (%%fx>= argcount mandatory)
           ;; this crude test for optional and named needs to be refined
           (or (%%fx<= argcount mandatory) (%%not (%%null? optional)) (%%not (%%null? named)) rest)
           (let iter ((args arguments)
                      (types argument-types)
                      (expected positional))
                (cond ((%%null? expected)
                       #t)
                      ((match? (%%car args) (%%car types) (%%car expected))
                       (iter (%%cdr args) (%%cdr types) (%%cdr expected)))
                      (else
                       #f)))))
    
    (define (match-rest?)
      (or (%%not rest)
          (let ((expect (jazz:get-rest-type-type rest)))
            (jazz:every? (lambda (type)
                           ;; should validate that type is not a category-type
                           (match? #f type expect))
                         (list-tail argument-types mandatory)))))
    
    (and (match-positional?)
         ;; testing the presence of optional named this way is a quicky that needs to be refined
         (or (%%not (%%null? optional))
             (%%not (%%null? named))
             (match-rest?)))))


;;;
;;;; If
;;;


(jazz:define-class jazz:If jazz:Expression (constructor: jazz:allocate-if)
  ((test getter: generate)
   (yes  getter: generate)
   (no   getter: generate)))


(define (jazz:new-if source test yes no)
  (jazz:allocate-if #f source test yes no))


(define jazz:type-tests
  (%%make-table test: eq?))


(%%table-set! jazz:type-tests 'scheme.language.runtime.kernel:number?      jazz:Number)
(%%table-set! jazz:type-tests 'scheme.language.runtime.kernel:complex?     jazz:Complex)
(%%table-set! jazz:type-tests 'scheme.language.runtime.kernel:real?        jazz:Real)
(%%table-set! jazz:type-tests 'scheme.language.runtime.kernel:rational?    jazz:Rational)
(%%table-set! jazz:type-tests 'scheme.language.runtime.kernel:integer?     jazz:Integer)
(%%table-set! jazz:type-tests 'scheme.language.runtime.kernel:number?      jazz:Number)
(%%table-set! jazz:type-tests 'scheme.language.runtime.kernel:boolean?     jazz:Boolean)
;; not 100% correct because of Scheme's semantic for list?
(%%table-set! jazz:type-tests 'scheme.language.runtime.kernel:list?        jazz:List)
(%%table-set! jazz:type-tests 'scheme.language.runtime.kernel:null?        jazz:Null)
(%%table-set! jazz:type-tests 'scheme.language.runtime.kernel:pair?        jazz:Pair)
(%%table-set! jazz:type-tests 'scheme.language.runtime.kernel:symbol?      jazz:Symbol)
(%%table-set! jazz:type-tests 'scheme.language.runtime.kernel:char?        jazz:Char)
(%%table-set! jazz:type-tests 'scheme.language.runtime.kernel:string?      jazz:String)
(%%table-set! jazz:type-tests 'scheme.language.runtime.kernel:vector?      jazz:Vector)
(%%table-set! jazz:type-tests 'jazz.language.runtime.kernel:s8vector?      jazz:S8Vector)
(%%table-set! jazz:type-tests 'jazz.language.runtime.kernel:u8vector?      jazz:U8Vector)
(%%table-set! jazz:type-tests 'jazz.language.runtime.kernel:s16vector?     jazz:S16Vector)
(%%table-set! jazz:type-tests 'jazz.language.runtime.kernel:u16vector?     jazz:U16Vector)
(%%table-set! jazz:type-tests 'jazz.language.runtime.kernel:s32vector?     jazz:S32Vector)
(%%table-set! jazz:type-tests 'jazz.language.runtime.kernel:u32vector?     jazz:U32Vector)
(%%table-set! jazz:type-tests 'jazz.language.runtime.kernel:s64vector?     jazz:S64Vector)
(%%table-set! jazz:type-tests 'jazz.language.runtime.kernel:u64vector?     jazz:U64Vector)
(%%table-set! jazz:type-tests 'jazz.language.runtime.kernel:f32vector?     jazz:F32Vector)
(%%table-set! jazz:type-tests 'jazz.language.runtime.kernel:f64vector?     jazz:F64Vector)
(%%table-set! jazz:type-tests 'jazz.language.runtime.kernel:continuation?  jazz:Continuation)
(%%table-set! jazz:type-tests 'scheme.language.runtime.kernel:procedure?   jazz:Procedure)
(%%table-set! jazz:type-tests 'scheme.language.runtime.kernel:input-port?  jazz:Port)
(%%table-set! jazz:type-tests 'scheme.language.runtime.kernel:output-port? jazz:Port)
(%%table-set! jazz:type-tests 'scheme.language.runtime.kernel:eof-object?  jazz:EOF)
(%%table-set! jazz:type-tests 'jazz.language.runtime.kernel:fixnum?        jazz:Fixnum)
(%%table-set! jazz:type-tests 'jazz.language.runtime.kernel:flonum?        jazz:Flonum)
(%%table-set! jazz:type-tests 'jazz.language.runtime.kernel:keyword?       jazz:Keyword)
(%%table-set! jazz:type-tests 'jazz.language.runtime.kernel:object?        jazz:Object)
(%%table-set! jazz:type-tests 'jazz.language.runtime.kernel:category?      jazz:Category)
(%%table-set! jazz:type-tests 'jazz.language.runtime.kernel:class?         jazz:Class)
(%%table-set! jazz:type-tests 'jazz.language.runtime.kernel:interface?     jazz:Interface)
(%%table-set! jazz:type-tests 'jazz.language.runtime.kernel:field?         jazz:Field)
(%%table-set! jazz:type-tests 'jazz.language.runtime.kernel:slot?          jazz:Slot)
(%%table-set! jazz:type-tests 'jazz.language.runtime.kernel:method?        jazz:Method)
(%%table-set! jazz:type-tests 'jazz.language.runtime.kernel:table?         jazz:Table)
(%%table-set! jazz:type-tests 'jazz.language.runtime.kernel:thread?        jazz:Thread)
(%%table-set! jazz:type-tests 'jazz.language.runtime.kernel:foreign?       jazz:Foreign)
(%%table-set! jazz:type-tests 'jazz.language.runtime.kernel:values?        jazz:Values)
(%%table-set! jazz:type-tests 'jazz.language.runtime.kernel:unspecified?   jazz:Unspecified)


(define jazz:not-type-tests
  (%%make-table test: eq?))


(%%table-set! jazz:not-type-tests 'jazz.language.runtime.kernel:not-null? jazz:Null)


(define (jazz:restrict-type base type)
  (jazz:new-restriction-type base type))


(define (jazz:restriction-of? type class)
  (and (%%class-is? type jazz:Restriction-Type)
       (%%class-is? (jazz:get-restriction-type-type type) class)))


(define (jazz:complement-type base type)
  (if (and (jazz:restriction-of? type jazz:Complement-Type)
           (%%eq? (jazz:get-restriction-type-base type) base))
      (jazz:get-complement-type-type (jazz:get-restriction-type-type type))
    (jazz:new-restriction-type base (jazz:new-complement-type type))))


(define (jazz:branch-types test environment)
  (define (process-not expr env)
    (revenv (process-expr expr env)))
  
  (define (process-and expr-list env)
    (if (and (%%not-null? expr-list)
             (%%null? (%%cdr expr-list)))
        (process-expr (%%car expr-list) env)
      (let iter ((scan expr-list) (augmented env))
        (if (%%null? scan)
            augmented
          (let ((expr (%%car scan)))
            (let ((newenv (process-expr expr augmented)))
              (iter (%%cdr scan) (%%cons (%%car newenv) (%%car newenv)))))))))
  
  (define (process-or expr-list env)
    (if (and (%%not-null? expr-list)
             (%%null? (%%cdr expr-list)))
        (process-expr (%%car expr-list) env)
      (let iter ((scan expr-list) (augmented env))
        (if (%%null? scan)
            augmented
          (let ((expr (%%car scan)))
            (let ((newenv (process-expr expr augmented)))
              (iter (%%cdr scan) (%%cons (%%cdr newenv) (%%cdr newenv)))))))))
  
  (define (process-is expr type-expr env)
    (receive (origin actual-type) (extract-binding expr env)
      (if origin
          (let ((yes-type (cond ((jazz:type? type-expr)
                                 type-expr)
                                ((%%class-is? type-expr jazz:Binding-Reference)
                                 (let ((binding (jazz:get-binding-reference-binding type-expr)))
                                   (if (%%class-is? binding jazz:Declaration)
                                       (jazz:resolve-binding binding)
                                     #f)))
                                (else
                                 #f))))
            ;; quick try for fun
            (let ((no-type
                    (if (%%eq? actual-type jazz:List)
                        (cond ((%%eq? yes-type jazz:Null)
                               jazz:Pair)
                              ((%%eq? yes-type jazz:Pair)
                               jazz:Null)
                              (else
                               #f))
                      #f)))
              (let ((yes
                      (if yes-type
                          (%%cons (jazz:new-annotated-frame (%%list (jazz:new-restricted-binding origin yes-type)) #f) (%%car env))
                        (%%car env)))
                    (no
                      (if no-type
                          (%%cons (jazz:new-annotated-frame (%%list (jazz:new-restricted-binding origin no-type)) #f) (%%cdr env))
                        (%%cdr env))))
                (%%cons yes no))))
        env)))
  
  (define (extract-binding expr env)
    (if (%%class-is? expr jazz:Binding-Reference)
        (let ((binding (jazz:get-binding-reference-binding expr)))
          (cond ((%%class-is? binding jazz:Variable)
                 (receive (frame actual-variable actual-type) (jazz:find-annotated binding (%%car env))
                   (let ((origin (jazz:get-annotated-variable-variable actual-variable)))
                     (values origin actual-type))))
                ;; this is really for slots so i need to think about this
                ((%%class-is? binding jazz:Declaration)
                 (values binding (jazz:get-lexical-binding-type binding)))
                (else
                 (values #f #f))))
      (values #f #f)))
  
  (define (revenv env)
    (%%cons (%%cdr env) (%%car env)))
  
  (define (process-expr expr env)
    (cond ((%%class-is? expr jazz:And)
           (process-and (jazz:get-and-expressions expr) env))
          ((%%class-is? expr jazz:Or)
           (process-or (jazz:get-or-expressions expr) env))
          ((%%class-is? expr jazz:Call)
           (let ((operator (jazz:get-call-operator expr)))
             (if (%%class-is? operator jazz:Binding-Reference)
                 (let ((operator-binding (jazz:get-binding-reference-binding operator)))
                   (if (%%class-is? operator-binding jazz:Declaration)
                       (let ((operator-locator (jazz:get-declaration-locator operator-binding))
                             (arguments (jazz:get-call-arguments expr)))
                         (let ((count (%%length arguments)))
                           (case operator-locator
                             ((scheme.language.runtime.kernel:not)
                              (if (%%fx= count 1)
                                  (process-not (%%car arguments) env)
                                env))
                             ((jazz.language.runtime.kernel:is?)
                              (if (%%fx= count 2)
                                  (process-is (%%car arguments) (%%cadr arguments) env)
                                env))
                             ((jazz.language.runtime.functional:is-not?)
                              (if (%%fx= count 2)
                                  (revenv (process-is (%%car arguments) (%%cadr arguments) env))
                                env))
                             (else
                              (if (%%fx= count 1)
                                  (let ((class (%%table-ref jazz:type-tests operator-locator #f)))
                                    (if class
                                        (process-is (%%car arguments) class env)
                                      (let ((class (%%table-ref jazz:not-type-tests operator-locator #f)))
                                        (if class
                                            (revenv (process-is (%%car arguments) class env))
                                          env))))
                                env)))))
                     env))
               env)))
          (else
           (receive (origin actual-type) (extract-binding expr env)
             (if origin
                 (if (%%class-is? actual-type jazz:Nillable-Type)
                     (let ((yes (%%cons (jazz:new-annotated-frame (%%list (jazz:new-restricted-binding origin (jazz:get-nillable-type-type actual-type))) #f) (%%car env)))
                           (no (%%cdr env)))
                       (%%cons yes no))
                   env)
               env)))))
  
  (process-expr test (%%cons environment environment)))


(jazz:define-method (jazz:emit-expression (jazz:If expression) declaration environment backend)
  (let ((test (jazz:get-if-test expression)))
    (jazz:bind (yes-environment . no-environment) (jazz:branch-types test environment)
      (let ((test (jazz:emit-expression test declaration environment backend))
            (yes (jazz:emit-expression (jazz:get-if-yes expression) declaration yes-environment backend))
            (no (jazz:emit-expression (jazz:get-if-no expression) declaration no-environment backend)))
        (jazz:new-code
          (jazz:emit 'if backend expression declaration environment test yes no)
          (jazz:extend-type (jazz:get-code-type yes) (jazz:get-code-type no))
          (jazz:get-expression-source expression))))))


(jazz:define-method (jazz:tree-fold (jazz:If expression) down up here seed environment)
  (up expression
      seed
      (jazz:tree-fold
        (jazz:get-if-no expression) down up here
        (jazz:tree-fold
          (jazz:get-if-yes expression) down up here
          (jazz:tree-fold
            (jazz:get-if-test expression) down up here (down expression seed environment) environment)
          environment)
        environment)
      environment))


;;;
;;;; Cond
;;;


(jazz:define-class jazz:Cond jazz:Expression (constructor: jazz:allocate-cond)
  ((clauses getter: generate)))


(define (jazz:new-cond source clauses)
  (jazz:allocate-cond #f source clauses))


(jazz:define-method (jazz:emit-expression (jazz:Cond expression) declaration environment backend)
  (let ((clauses (jazz:get-cond-clauses expression)))
    (let ((clauses
            (let recurse ((clauses clauses)
                          (environment environment))
                 (if (%%null? clauses)
                     '()
                   (let ((clause (%%car clauses)))
                     (let ((test (%%car clause))
                           (arrow? (%%cadr clause))
                           (body (%%cddr clause)))
                       (jazz:bind (yes-environment . no-environment) (jazz:branch-types test environment)
                         (let ((test (and test (jazz:emit-expression test declaration environment backend)))
                               (body (and body (jazz:emit-expression body declaration yes-environment backend))))
                           (let ((output
                                   `(,(if (%%not test)
                                          'else
                                        (jazz:sourcified-form test))
                                     ,@(if arrow?
                                           `(=>)
                                         '())
                                     ,@(if body
                                           (%%list (jazz:sourcified-form body))
                                         '())))
                                 (type
                                   (cond (arrow? jazz:Any)
                                         (body (jazz:get-code-type body))
                                         (test (jazz:get-code-type test))
                                         (else jazz:Any))))
                             (%%cons (%%cons output type) (recurse (%%cdr clauses) no-environment)))))))))))
      (jazz:new-code
        (jazz:emit 'cond backend expression declaration environment (map car clauses))
        (jazz:extend-types (map cdr clauses))
        (jazz:get-expression-source expression)))))


(jazz:define-method (jazz:tree-fold (jazz:Cond expression) down up here seed environment)
  (up expression
      seed
      (let fold ((ls (jazz:get-cond-clauses expression))
                 (seed (down expression seed environment)))
           (if (null? ls)
               seed
             (let* ((clause (%%car ls))
                    (test (%%car clause))
                    (body (%%cddr clause))
                    (seed (if body (jazz:tree-fold body down up here seed environment) seed)))
               (fold (%%cdr ls)
                     (if (%%not test)
                         body
                       (jazz:tree-fold test down up here seed environment))))))
      environment))


;;;
;;;; Case
;;;


(jazz:define-class jazz:Case jazz:Expression (constructor: jazz:allocate-case)
  ((target  getter: generate)
   (clauses getter: generate)))


(define (jazz:new-case source target clauses)
  (jazz:allocate-case #f source target clauses))


(jazz:define-method (jazz:emit-expression (jazz:Case expression) declaration environment backend)
  (let ((target (jazz:get-case-target expression))
        (clauses (jazz:get-case-clauses expression)))
    (let ((target-emit (jazz:emit-expression target declaration environment backend))
          (clauses-emit (map (lambda (clause)
                               (let ((body (%%cdr clause)))
                                 (jazz:emit-expression body declaration environment backend)))
                             clauses)))
      (jazz:new-code
        (jazz:emit 'case backend expression declaration environment target-emit clauses clauses-emit)
        (jazz:extend-types (map (lambda (emited-clause)
                                  (jazz:get-code-type emited-clause))
                                clauses-emit))
        (jazz:get-expression-source expression)))))


(jazz:define-method (jazz:tree-fold (jazz:Case expression) down up here seed environment)
  (up expression
      seed
      (jazz:tree-fold-list
        (map cdr (jazz:get-case-clauses expression)) down up here
        (jazz:tree-fold (jazz:get-case-target expression) down up here (down expression seed environment) environment)
        environment)
      environment))


;;;
;;;; And
;;;


(jazz:define-class jazz:And jazz:Expression (constructor: jazz:allocate-and)
  ((expressions getter: generate)))


(define (jazz:new-and source expressions)
  (jazz:allocate-and #f source expressions))


(jazz:define-method (jazz:emit-expression (jazz:And expression) declaration environment backend)
  (let ((expressions
          (let recurse ((expressions (jazz:get-and-expressions expression))
                        (environment environment))
               (if (%%null? expressions)
                   '()
                 (let ((expression (%%car expressions)))
                   (jazz:bind (yes-environment . no-environment) (jazz:branch-types expression environment)
                     (let ((code (jazz:emit-expression expression declaration environment backend)))
                       (%%cons code (recurse (%%cdr expressions) yes-environment)))))))))
    (let ((type (cond ((%%null? expressions) jazz:Any)
                      ((%%null? (%%cdr expressions)) (jazz:get-code-type (%%car expressions)))
                      (else (let ((last-type (jazz:get-code-type (jazz:last expressions))))
                              (if (%%eq? last-type jazz:Any)
                                  jazz:Any
                                (jazz:new-nillable-type last-type)))))))
      (jazz:new-code
        (jazz:emit 'and backend expression declaration environment expressions)
        type
        (jazz:get-expression-source expression)))))


(jazz:define-method (jazz:tree-fold (jazz:And expression) down up here seed environment)
  (up expression
      seed
      (jazz:tree-fold-list
        (jazz:get-and-expressions expression) down up here (down expression seed environment) environment)
      environment))


;;;
;;;; Or
;;;


(jazz:define-class jazz:Or jazz:Expression (constructor: jazz:allocate-or)
  ((expressions getter: generate)))


(define (jazz:new-or source expressions)
  (jazz:allocate-or #f source expressions))


(jazz:define-method (jazz:emit-expression (jazz:Or expression) declaration environment backend)
  (let ((expressions
          (let recurse ((expressions (jazz:get-or-expressions expression))
                        (environment environment))
               (if (%%null? expressions)
                   '()
                 (let ((expression (%%car expressions)))
                   (jazz:bind (yes-environment . no-environment) (jazz:branch-types expression environment)
                     (let ((code (jazz:emit-expression expression declaration environment backend)))
                       (%%cons code (recurse (%%cdr expressions) no-environment)))))))))
    (let ((type (cond ((%%null? expressions) jazz:Any)
                      ((%%null? (%%cdr expressions)) (jazz:get-code-type (%%car expressions)))
                      (else (let ((last-type (jazz:get-code-type (jazz:last expressions))))
                              (if (jazz:every? (lambda (code)
                                                 (let ((type (jazz:get-code-type code)))
                                                   (and (%%is? type jazz:Nillable-Type)
                                                        (%%eq? (jazz:get-nillable-type-type type) last-type))))
                                               (jazz:butlast expressions))
                                  last-type
                                jazz:Any))))))
      (jazz:new-code
        (jazz:emit 'or backend expression declaration environment expressions)
        type
        (jazz:get-expression-source expression)))))


(jazz:define-method (jazz:tree-fold (jazz:Or expression) down up here seed environment)
  (up expression
      seed
      (jazz:tree-fold-list
        (jazz:get-or-expressions expression) down up here (down expression seed environment) environment)
      environment))


;;;
;;;; Declare
;;;


(jazz:define-class jazz:Declare jazz:Expression (constructor: jazz:allocate-declare)
  ((declarations getter: generate)))


(define (jazz:new-declare declarations)
  (jazz:allocate-declare #f #f declarations))


(jazz:define-method (jazz:emit-expression (jazz:Declare expression) declaration environment backend)
  (jazz:new-code
    (jazz:emit 'declare backend expression declaration environment)
    jazz:Any
    #f))


;;;
;;;; Parameterize
;;;


(jazz:define-class jazz:Parameterize jazz:Expression (constructor: jazz:allocate-parameterize)
  ((bindings getter: generate)
   (body     getter: generate)))


(define (jazz:new-parameterize bindings body)
  (jazz:allocate-parameterize #f #f bindings body))


(jazz:define-method (jazz:emit-expression (jazz:Parameterize expression) declaration environment backend)
  (let ((body (jazz:get-parameterize-body expression)))
    (let ((body-code (jazz:emit-expression body declaration environment backend)))
      (jazz:new-code
        (jazz:emit 'parameterize backend expression declaration environment body-code)
        (jazz:get-code-type body-code)
        #f))))


(jazz:define-method (jazz:tree-fold (jazz:Parameterize expression) down up here seed environment)
  (let ((seed2 (jazz:tree-fold-list (map cdr (jazz:get-parameterize-bindings expression)) down up here (down expression seed environment) environment)))
    (up expression
        seed
        (jazz:tree-fold (jazz:get-parameterize-body expression) down up here seed2 environment)
        environment))))
