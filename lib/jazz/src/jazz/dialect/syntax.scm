;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Jazz Dialect Syntax
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


(module protected jazz.dialect.syntax scheme


(export (scheme.syntax))

(import (scheme.syntax-rules (phase syntax))
        (gambit (only bitwise-ior open-output-string get-output-string string->keyword))
        (jazz.dialect.kernel))


(native private jazz:->string)
(native private jazz:error)
(native private jazz:getf)
(native private jazz:last-tail)
(native private jazz:last)
(native private jazz:butlast)
(native private jazz:naturals)
(native private jazz:current-declaration)
(native private jazz:current-declaration-name)
(native private jazz:system-format)


;;;
;;;; assertion
;;;


(export assert
        assertion
        allege
        debug-assert
        debug-assertion)


(define-syntax assert
  (lambda (form-src usage-environment macro-environment)
    (sourcify-deep-if
      ;; we really want assertions in release and not in a new distribution safety
      (expand-assert-test #t form-src)
      form-src)))


(define-syntax assertion
  (lambda (form-src usage-environment macro-environment)
    (sourcify-deep-if
      ;; we really want assertions in release and not in a new distribution safety
      (expand-assertion-test #t form-src)
      form-src)))


(define-syntax allege
  (lambda (form-src usage-environment macro-environment)
    (let ((test (cadr (source-code form-src)))
          (body (cddr (source-code form-src))))
      (cond ((null? body)
             (let ((temp (generate-symbol "temp")))
               (sourcify-deep-if
                 (if (symbol? (source-code test))
                     `(%allege ,test ,test)
                   `(let ((,temp ,test))
                      (%allege ,temp ,temp)))
                 form-src)))
            ((null? (cdr body))
             (sourcify-deep-if
               `(%allege ,test ,(car body))
               form-src))
            (else
             (sourcify-deep-if
               `(%allege ,test (begin ,@body))
               form-src))))))


(define-syntax debug-assert
  (lambda (form-src usage-environment macro-environment)
    (sourcify-deep-if
      (expand-assert-test debug-user? form-src)
      form-src)))


(define-syntax debug-assertion
  (lambda (form-src usage-environment macro-environment)
    (sourcify-deep-if
      (expand-assertion-test debug-user? form-src)
      form-src)))


(define (expand-assert-test test? src)
  (let ((assertion (cadr (source-code src)))
        (body (cddr (source-code src))))
    (let ((message (let ((port (open-output-string)))
                     (display "Assertion " port)
                     (write (desourcify assertion) port)
                     (display " failed" port)
                     (get-output-string port))))
      (expand-assertion-body test? assertion (list 'error message) body))))


(define (expand-assertion-test test? src)
  (let ((assertion (cadr (source-code src)))
        (action (car (cddr (source-code src))))
        (body (cdr (cddr (source-code src)))))
    (expand-assertion-body test? assertion action body)))


(define (expand-assertion-body test? assertion action body)
  (let ((body (if (not (null? body)) body '((unspecified)))))
    (if test?
        `(if (not ,assertion)
             ,action
           ,(simplify-begin
              `(begin
                 ,@body)))
      (simplify-begin `(begin ,@body)))))


;;;
;;;; attributes
;;;


(export attributes)


; @macro
; (attributes (nullable?)
;   (slot key      initialize #f)
;   (slot criteria initialize #f))


; @expansion
; (begin
;   (slot key initialize #f)
;   (slot criteria initialize #f)
;   (method override (get-attributes self)
;     '(nullable?
;       key
;       criteria))
;   (method override (get self attribute)
;     (case attribute
;       ((key) key)
;       ((criteria) criteria)
;       (else (nextmethod self attribute))))
;   (method override (set self attribute value)
;     (case attribute
;       ((key) (set! key value))
;       ((criteria) (set! criteria value))
;       (else (nextmethod self attribute value))))
;   (begin
;     (method public (get-key self)
;       ...)
;     (method public (set-key self value)
;       (set! key value)))
;   (begin
;     (method public (get-criteria self)
;       ...)
;     (method public (set-criteria self value)
;       (set! criteria value))))


(define-macro (attributes . form)
  (define (slot-name attribute)
    (string->symbol (string-append "_" (symbol->string (cadr attribute)))))
  
  (let ((inherited (car form))
        (attributes (cdr form))
        (attribute (generate-symbol "attr"))
        (value (generate-symbol "val")))
    `(begin
       #;
       ,@(expand-marshalling form)
       ,@(map (lambda (attribute)
                (let ((slot-name (slot-name attribute)))
                  `(slot ,slot-name)))
              attributes)
       (method override (get-attributes self)
         ',(append inherited (map cadr attributes)))
       (method override (get-attribute self ,attribute)
         (case ,attribute
           ,@(map (lambda (attribute)
                    (let ((name (cadr attribute))
                          (slot-name (slot-name attribute)))
                      `((,name) ,slot-name)))
                  attributes)
           (else (nextmethod self ,attribute))))
       (method override (set-attribute self ,attribute ,value)
         (case ,attribute
           ,@(map (lambda (attribute)
                    (let ((name (cadr attribute))
                          (slot-name (slot-name attribute)))
                      `((,name) (set! ,slot-name ,value))))
                  attributes)
           (else (nextmethod self ,attribute ,value))))
       (method override (get-attribute-default self ,attribute)
         (case ,attribute
           ,@(map (lambda (attribute)
                    (let ((name (cadr attribute)))
                      (parse-specifier (cddr attribute)
                        (lambda (specifier specifier-source rest)
                          (let ((initialize (getf rest 'initialize '(unspecified))))
                            `((,name) ,initialize))))))
                  attributes)
           (else (nextmethod self ,attribute))))
       (method override (get-attribute-no-default self ,attribute)
         (let ((,value (get-attribute self ,attribute)))
           (if (specified? ,value)
               ,value
             (let (iterate (scan (get-ascendants self)))
               (if (not-null? scan)
                   (let ((,value (get-attribute-no-default (car scan) ,attribute)))
                     (if (specified? ,value)
                         ,value
                       (iterate (cdr scan))))
                 (unspecified))))))
       (method override (attribute=? self ,attribute x y)
         (case ,attribute
           ,@(let ((clauses (new-queue)))
               (for-each (lambda (attribute)
                           (let ((name (cadr attribute)))
                             (parse-specifier (cddr attribute)
                               (lambda (specifier specifier-source rest)
                                 (let ((test (getf rest 'test)))
                                   (if test
                                       (enqueue clauses `((,name) (,test x y)))))))))
                         attributes)
               (queue-list clauses))
           (else (nextmethod self ,attribute x y))))
       (method override (get self ,attribute)
         (case ,attribute
           ,@(map (lambda (attribute)
                    (let ((name (cadr attribute)))
                      (let ((getter (string->symbol (system-format "get-{a}" name))))
                        `((,name) (,getter self)))))
                  attributes)
           (else (nextmethod self ,attribute))))
       (method override (set self ,attribute ,value)
         (case ,attribute
           ,@(map (lambda (attribute)
                    (let ((name (cadr attribute))
                          (slot-name (slot-name attribute)))
                      `((,name) (set! ,slot-name ,value))))
                  attributes)
           (else (nextmethod self ,attribute ,value))))
       ,@(map (lambda (attribute)
                (let ((name (cadr attribute))
                      (slot-name (slot-name attribute)))
                  (parse-specifier (cddr attribute)
                    (lambda (specifier specifier-source rest)
                      (let ((getter (string->symbol (system-format "get-{a}" name)))
                            (setter (string->symbol (system-format "set-{a}" name))))
                        `(begin
                           (method public (,getter self) ,@(if specifier (list specifier) '())
                             (let ((,value (get-attribute-no-default self ',name)))
                               (if (specified? ,value)
                                   ,value
                                 (get-attribute-default self ',name))))
                           (method public (,setter self ,value ,@(if specifier (list specifier) '()))
                             (set! ,slot-name ,value))))))))
              attributes))))


;;;
;;;; bind
;;;


(export bind
        bind-vector
        bind-values)


; @syntax (bind ((a . r) b . c) tree (list a b c r))
; @expansion
; (let ((val tree))
;   (let ((car1 (car tree0))
;         (cdr2 (cdr tree0)))
;     (let ((a (car car1))
;           (r (cdr car1)))
;       (let ((b (car cdr2))
;             (c (cdr cdr2)))
;         (list a b c r)))))


(define-syntax bind
  (lambda (form-src usage-environment macro-environment)
    (define (expand-bind-car bindings tree body)
      (let ((car-binding (car bindings))
            (cdr-binding (cdr bindings)))
        (cond ((symbol? car-binding)
               (let ((specifier (binding-specifier bindings)))
                 (if specifier
                     `((let ((,car-binding ,specifier (car ,tree)))
                         ,@(expand-bind-cdr (cdr cdr-binding) tree body)))
                   `((let ((,car-binding (car ,tree)))
                       ,@(expand-bind-cdr cdr-binding tree body))))))
              ((pair? car-binding)
               (let ((car-symbol (generate-symbol "car")))
                 `((let ((,car-symbol (car ,tree)))
                     ,@(expand-bind-car car-binding car-symbol
                         (expand-bind-cdr cdr-binding tree body)))))))))
    
    (define (expand-bind-cdr cdr-binding tree body)
      (cond ((null? cdr-binding)
             body)
            ((symbol? cdr-binding)
             `((let ((,cdr-binding (cdr ,tree)))
                 ,@body)))
            ((pair? cdr-binding)
             (let ((cdr-symbol (generate-symbol "cdr")))
               `((let ((,cdr-symbol (cdr ,tree)))
                   ,@(expand-bind-car cdr-binding cdr-symbol body)))))))
    
    (let ((bindings (desourcify (cadr (source-code form-src))))
          (tree (car (cddr (source-code form-src))))
          (body (cdr (cddr (source-code form-src)))))
      (sourcify-deep-if
        (with-uniqueness tree
          (lambda (tree-value)
            `(begin
               ,@(expand-bind-car bindings tree-value body))))
        form-src))))


(define-syntax bind-vector
  (lambda (form-src usage-environment macro-environment)
    (let ((bindings (source-code (cadr (source-code form-src))))
          (vector (car (cddr (source-code form-src))))
          (body (cdr (cddr (source-code form-src))))
          (vec (generate-symbol "vec")))
      (define (parse-bindings)
        (let (iter (scan bindings) (bindings '()))
          (if (null? scan)
              (reverse bindings)
            (let ((specifier (binding-specifier scan)))
              (if specifier
                  (iter (cddr scan) (cons (cons (car scan) specifier) bindings))
                (iter (cdr scan) (cons (cons (car scan) #f) bindings)))))))
      
      (sourcify-deep-if
        (let ((bindings (parse-bindings)))
          `(let ((,vec ,vector))
             (let ,(map (lambda (binding rank)
                          (let ((variable (car binding))
                                (type (cdr binding)))
                            `(,variable ,@(if type (list type) '()) (vector-ref ,vec ,rank))))
                        bindings
                        (naturals 0 (length bindings)))
               ,@body)))
        form-src))))


(define-syntax bind-values
  (lambda (form-src usage-environment macro-environment)
    (let ((bindings (source-code (cadr (source-code form-src))))
          (values (car (cddr (source-code form-src))))
          (body (cdr (cddr (source-code form-src))))
          (v (generate-symbol "v")))
      (define (parse-bindings)
        (let (iter (scan bindings) (bindings '()))
          (if (null? scan)
              (reverse bindings)
            (let ((specifier (binding-specifier scan)))
              (if specifier
                  (iter (cddr scan) (cons (cons (car scan) specifier) bindings))
                (iter (cdr scan) (cons (cons (car scan) #f) bindings)))))))
      
      (sourcify-deep-if
        (let ((bindings (parse-bindings)))
          `(let ((,v ,values))
             (let ,(map (lambda (binding rank)
                          (let ((variable (car binding))
                                (type (cdr binding)))
                            `(,variable ,@(if type (list type) '()) (values-ref ,v ,rank))))
                        bindings
                        (naturals 0 (length bindings)))
               ,@body)))
        form-src))))


;;;
;;;; bind-optionals
;;;


(export bind-optionals)


; @syntax (bind-optionals ((a 2)) rest a)
; @expansion (let ((a (if (not-null? rest) (car rest) 2))) a)

; @syntax (bind-optionals ((a 2) (b 3)) rest (list a b))
; @expansion
; (let ((a 2) (b 3) (__scan rest))
;   (if (not-null? __scan)
;       (begin
;         (set! a (car __scan))
;         (set! __scan (cdr __scan))))
;   (if (not-null? __scan)
;       (begin
;         (set! b (car __scan))
;         (set! __scan (cdr __scan))))
;   (list a b))


(define-syntax bind-optionals
  (lambda (form-src usage-environment macro-environment)
    (let ((bindings (source-code (cadr (source-code form-src))))
          (rest (car (cddr (source-code form-src))))
          (body (cdr (cddr (source-code form-src))))
          (scan (generate-symbol "scan"))
          (prog (generate-symbol "prog")))
      (sourcify-deep-if
        `(let ((,scan ,rest))
           (let* ,(map (lambda (binding)
                         (let* ((variable (source-code (car (source-code binding))))
                                (specifier (binding-specifier binding))
                                (default (if specifier (caddr (source-code binding)) (cadr (source-code binding))))
                                (value `(if (null? ,scan) ,default (let ((,prog (car ,scan)))
                                                                     (set! ,scan (cdr ,scan))
                                                                     ,prog))))
                           (if specifier
                               `(,variable ,specifier ,value)
                             `(,variable ,value))))
                       (proper-list bindings))
             (if (not-null? ,scan)
                 (error "Too many arguments for bind-optionals"))
             ,@body))
        form-src))))


;;;
;;;; bind-keywords
;;;


(export bind-keywords)


; @syntax (bind-keywords ((a 2) (b 3)) rest (list b: 5))
; @expansion
; (let ((r (box-list rest)))
;   (let ((a (find-keyword a: r (lambda () 2)))
;         (b (find-keyword b: r (lambda () 3))))
;     (if (unbox-list r)
;         (error "Unexpected keywords: {s}" (unbox-list r)))
;     (list b: 5)))


(define-syntax bind-keywords
  (lambda (form-src usage-environment macro-environment)
    (let ((bindings (source-code (cadr (source-code form-src))))
          (rest (car (cddr (source-code form-src))))
          (body (cdr (cddr (source-code form-src)))))
      (let ((box (generate-symbol "box"))
            (bnd (proper-list bindings))
            (oth (last-tail bindings)))
        (let ((last (last bnd)))
          (let ((bnd (if (boolean? (source-code last)) (butlast bnd) bnd))
                (allow-other-keys? (if (boolean? (source-code last)) last #f)))
            (sourcify-deep-if
              `(let ((,box (box-list ,rest)))
                 (let* ,(map (lambda (binding)
                               (let* ((variable (source-code (car (source-code binding))))
                                      (specifier (binding-specifier binding))
                                      (default (if specifier (caddr (source-code binding)) (cadr (source-code binding)))))
                                 (if specifier
                                     `(,variable ,specifier (find-keyword ',(string->keyword (symbol->string variable)) ,box (lambda () ,default) ,allow-other-keys?)))
                                 `(,variable (find-keyword ',(string->keyword (symbol->string variable)) ,box (lambda () ,default) ,allow-other-keys?))))
                             bnd)
                   ,@(if (symbol? (source-code oth))
                         `((let ((,(source-code oth) (unbox-list ,box)))
                             ,@body))
                       `((if (not-null? (unbox-list ,box))
                             (error "Unexpected keywords: {s}" (unbox-list ,box)))
                         ,@body))))
              form-src)))))))


;;;
;;;; ecase
;;;


(export ecase
        nucase)


;; @syntax (ecase target ((a) 1) ((b c) 2) (else 3))
;; @expansion (let ((sym8 target))
;;             (cond ((eqv? sym8 a) 1)
;;               ((or (eqv? sym8 b) (eqv? sym8 c)) 2)
;;               (else 3)))


(define-syntax ecase
  (lambda (form-src usage-environment macro-environment)
    (let ((form (cdr (source-code form-src))))
      (if (null? form)
          (error "Ill-formed ecase")
        (let ((target (car form))
              (clauses (cdr form)))
          (if (null? clauses)
              (error "Ill-formed ecase")
            (sourcify-deep-if
              (with-uniqueness target
                (lambda (symbol)
                  `(cond ,@(map (lambda (clause)
                                  (let ((selector (car (source-code clause)))
                                        (body (cdr (source-code clause))))
                                    (cond ((eq? (source-code selector) 'else)
                                           (cons 'else body))
                                          ((pair? (source-code selector))
                                           (cons (cons 'or (map (lambda (value)
                                                                  (if (integer? (source-code value))
                                                                      (list '= symbol value)
                                                                    (list 'eqv? symbol value)))
                                                                (source-code selector)))
                                                 body))
                                          (else
                                           (error "Ill-formed selector list: {s}" (desourcify selector))))))
                                clauses))))
              form-src)))))))


;; quick try
(define-syntax nucase
  (lambda (form-src usage-environment macro-environment)
    (let ((form (cdr (source-code form-src))))
      (if (null? form)
          (error "Ill-formed nucase")
        (let ((target (car form))
              (clauses (cdr form)))
          (if (null? clauses)
              (error "Ill-formed nucase")
            (sourcify-deep-if
              (with-uniqueness target
                (lambda (symbol)
                  `(cond ,@(map (lambda (clause)
                                  (let ((selector (car (source-code clause)))
                                        (body (cdr (source-code clause))))
                                    (cond ((eq? (source-code selector) 'else)
                                           (cons 'else body))
                                          ((pair? (source-code selector))
                                           (cons (cons 'or (map (lambda (value)
                                                                  (list 'nu=? symbol value))
                                                                (source-code selector)))
                                                 body))
                                          (else
                                           (error "Ill-formed selector list: {s}" (desourcify selector))))))
                                clauses))))
              form-src)))))))


;;;
;;;; enumeration
;;;


(export enumeration)


(define-syntax enumeration
  (lambda (form-src usage-environment macro-environment)
    (let ((enumeration-name (cadr (source-code form-src)))
          (names (cddr (source-code form-src))))
      `(begin
         (class ,enumeration-name extends Enumeration-Member)
         ,@(map (lambda (name-src)
                  (let ((name (source-code name-src)))
                    `(define ,(string->symbol (string-append (symbol->string (source-code enumeration-name)) ":" (symbol->string name))) (new ,enumeration-name ',name))))
                names)))))


;;;
;;;; increase
;;;


(export increase!
        decrease!
        multiply!
        divide!
        bitwise-ior!)


(define-syntax increase!
  (syntax-rules ()
    ((_ location)
     (increase! location 1))
    ((_ location increment)
     (set! location (+ location increment)))))


(define-syntax decrease!
  (syntax-rules ()
    ((_ location)
     (decrease! location 1))
    ((_ location increment)
     (set! location (- location increment)))))


(define-syntax multiply!
  (syntax-rules ()
    ((_ location factor)
     (set! location (* location factor)))))


(define-syntax divide!
  (syntax-rules ()
    ((_ location factor)
     (set! location (/ location factor)))))


(define-syntax bitwise-ior!
  (syntax-rules ()
    ((_ location factor)
     (set! location (bitwise-ior location factor)))))


;;;
;;;; marshall
;;;


(export expand-state-marshalling)


(define (expand-state-marshalling form)
  (define (call-getter attribute)
    (string->symbol (string-append "get-" (symbol->string (cadr attribute)))))
  
  (let ((inherited (car form))
        (attributes (cdr form))
        (attribute (generate-symbol "attr"))
        (value (generate-symbol "val")))
    `((method meta override (marshall-object self object)
        (serialize-object (class-of object)
                          (vector ,@(map (lambda (attribute)
                                           `(,(call-getter attribute) object))
                                         attributes))))
      (method meta override (unmarshall-object self content)
        (allocate ,(current-declaration-name)
                  ,@(map (lambda (attribute n)
                           `(vector-ref content ,n))
                         attributes
                         (naturals 0 (length attributes))))))))


;;;
;;;; state
;;;


(export state)


; @macro
; (state ()
;   (slot width  <fx> getter generate)
;   (slot height <fx> getter generate))


; @expansion
; (begin
;   (method meta override (marshall-object obj)
;     (vector (get-width obj)
;             (get-height obj)))
;   (method meta override (unmarshall-object vec)
;     (new Dimension
;       (vector-ref vec 0)
;       (vector-ref vec 1)))
;   (slot width <fx> getter generate)
;   (slot height <fx> getter generate))

(define-macro (state . form)
  (let ((attributes (cdr form)))
    `(begin
       ,@(expand-state-marshalling form)
       ,@attributes)))


;;;
;;;; typecase
;;;


(export typecase)


; @syntax (typecase target (a 1) ((b c) 2) (else 3))
; @expansion (let ((sym8 target))
;              (cond ((eqv? sym8 a) 1)
;                    ((or (eqv? sym8 b) (eqv? sym8 c)) 2)
;                    (else 3)))


(define-syntax typecase
  (lambda (form-src usage-environment macro-environment)
    (if (null? (cdr (source-code form-src)))
        (error "Ill-formed typecase")
      (let ((target (cadr (source-code form-src)))
            (clauses (cddr (source-code form-src))))
        (sourcify-deep-if
          (with-uniqueness target
            (lambda (variable)
              `(cond ,@(map (lambda (clause)
                              (let ((selector (car (source-code clause)))
                                    (body (cdr (source-code clause))))
                                (cond ((eq? (source-code selector) 'else)
                                       `(else ,@body))
                                      ((pair? (source-code selector))
                                       `((or ,@(map (lambda (value)
                                                      `(is? ,variable ,value))
                                                    (source-code selector)))
                                         ,@body))
                                      (else
                                       (error "Ill-formed selector list: {s}" (desourcify selector))))))
                            clauses))))
          form-src)))))


;;;
;;;; with
;;;


(export with)


(define (expand-one binding body)
  (define (parse-binding proc)
    (let ((variable (car (source-code binding)))
          (specifier (binding-specifier binding)))
      (if specifier
          (proc variable specifier (caddr (source-code binding)))
        (proc variable '<Object> (cadr (source-code binding))))))
  
  (parse-binding
    (lambda (variable specifier value)
      `(let ((,variable ,specifier ,value))
         (dynamic-wind (lambda () #f)
                       (lambda () ,@body)
                       (lambda () (close ,variable)))))))


(define (expand-with bindings body)
  (if (null? bindings)
      `(let () ,@body)
    (expand-one (car bindings) (list (expand-with (cdr bindings) body)))))


(define-syntax with
  (lambda (form-src usage-environment macro-environment)
    (let ((bindings (source-code (cadr (source-code form-src))))
          (body (cddr (source-code form-src))))
      (sourcify-deep-if
        (expand-with bindings body)
        form-src))))


;;;
;;;; macros
;;;


(export submodule
        constant
        ;; this is a quick hack around when generating
        ;; Unresolved symbol: expand-body in binaries,
        ;; but not in Yownu scripting for some reason!
        expand-body
        when
        jazzdoc
        unless
        prog1
        while
        unlikely
        unwind-protect
        catch
        ~
        ~
        local-context)


(define-syntax submodule
  (lambda (form-src usage-environment macro-environment)
    (let ((name (cadr (source-code form-src)))
          (body (cddr (source-code form-src))))
      (sourcify-deep-if
        `(begin
           ,@body)
        form-src))))


(define-syntax constant
  (lambda (form-src usage-environment macro-environment)
    (define (parse-modifiers proc)
      (let ((expr (cadr (source-code form-src))))
        (if (eq? (source-code expr) 'inline)
            (proc 'inline (cddr (source-code form-src)))
          (proc 'onsite (cdr (source-code form-src))))))
    
    (parse-modifiers
      (lambda (expansion form-src)
        (let ((name (car (source-code form-src))))
          (parse-specifier (cdr (source-code form-src))
            (lambda (specifier specifier-source body)
              (let ((value (car body)))
                (sourcify-deep-if
                  `(definition public ,expansion ,name ,@(if specifier (list specifier) '()) ,value)
                  form-src)))))))))


(define-syntax expand-body
  (syntax-rules ()
    ((_)
     (unspecified))
    ((_ expr ...)
     ;; useful to find code using the return value
     ;; but incorrect as it breaks proper tail call
     ;; (unspecific expr ...)
     (begin expr ...))))


(define-syntax when
  (syntax-rules ()
    ((when test expr ...)
     (if test
         (expand-body expr ...)))))


(define-syntax unless
  (syntax-rules ()
    ((unless test expr ...)
     (when (not test) expr ...))))


(define-syntax prog1
  (syntax-rules ()
    ((prog1 returned expr ...)
     (let ((value returned))
       expr ...
       value))))


(define-syntax while
  (syntax-rules ()
    ((while test expr ...)
     (let (iterate)
       (if test
           (begin
             expr ...
             (iterate)))))))


(define-syntax unlikely
  (syntax-rules ()
    ((unlikely expr)
     expr)))


(define-syntax unwind-protect
  (syntax-rules ()
    ((unwind-protect body protection ...)
     (dynamic-wind (lambda () #f)
                   (lambda () body)
                   (lambda () protection ...)))))


;; @syntax (catch X (f)) @expansion (call-with-catch X (lambda (exc) exc) (lambda () (f)))
;; @syntax (catch (X y (g y)) (f)) @expansion (call-with-catch X (lambda (y) (g y)) (lambda () (f)))

(define-syntax catch
  (lambda (form-src usage-environment macro-environment)
    (if (null? (cdr (unwrap-syntactic-closure form-src)))
        (error "Ill-formed catch")
      (let ((predicate/type (cadr (source-code form-src)))
            (body (cddr (source-code form-src))))
        (sourcify-deep-if
          (cond ((symbol? (source-code predicate/type))
                 `(call-with-catch ,predicate/type (lambda (exc) exc)
                    (lambda ()
                      ,@body)))
                ((pair? (source-code predicate/type))
                 `(call-with-catch ,(car (source-code predicate/type)) (lambda (,(source-code (cadr (source-code predicate/type)))) ,@(cddr (source-code predicate/type)))
                    (lambda ()
                      ,@body)))
                (else
                 (error "Ill-formed predicate/type in catch: {t}" (desourcify predicate/type))))
          form-src)))))


(define-syntax jazzdoc
  (syntax-rules ()
    ((jazzdoc expr ...)
     (unspecified))))


(define-syntax ~
  (lambda (form-src usage-environment macro-environment)
    (let ((name (source-code (cadr (source-code form-src))))
          (object (car (cddr (source-code form-src)))))
      (sourcify-deep-if
        (with-uniqueness object
          (lambda (obj)
            `(lambda rest
               (apply (dispatch (class-of ,obj) ',name) ,obj rest))))
        form-src))))


;; converted method references
(define-syntax ~
  (lambda (form-src usage-environment macro-environment)
    (let ((name (source-code (cadr (source-code form-src))))
          (object (car (cddr (source-code form-src)))))
      (sourcify-deep-if
        (with-uniqueness object
          (lambda (obj)
            `(lambda rest
               (apply (dispatch (class-of ,obj) ',name) ,obj rest))))
        form-src))))


(define-syntax local-context
  (lambda (form-src usage-environment macro-environment)
    (let ((names (cdr (source-code form-src))))
      (sourcify-deep-if
        `(list ,@(map (lambda (name)
                        `(cons ',(source-code name) ,name))
                      names))
        form-src))))


;; @macro (push! x (f)) @expansion (set! x (cons x (f)))

(define (expand-push! location value)
  (list 'set! location (list 'cons value location)))


;; @macro (pop! x) @expansion (set! x (cdr x))

(define (expand-pop! location)
  (list 'set! location (list 'cdr location)))


(define (expand-assert first rest)
  (if (null? rest)
      (let* ((expr first)
             (message (string-append "Assertion " (->string expr ':text) " failed")))
        (list 'unless expr (list 'error "{a}" message)))
    (let* ((expr (car rest))
           (message (->string expr ':text))
           (proc first))
      (list 'unless expr (list proc message)))))


(define (expand-assert-type expr type)
  (let ((value (generate-symbol)))
    (cons 'let*
          (cons (list (list value expr))
                (list (list 'when (list 'is-not? value type) (list 'error "{s} is not of the expected {s} type" value (list 'category-identifier type)))
                      value)))))


(define (expand-error? body)
  (let ((err (generate-symbol "err")))
    (list 'catch
          (list 'Error err #t)
          (cons 'begin body)
          #f)))


;;;
;;;; template
;;;


#; (


@w
(export template
        instantiate-butlast)


@w
(define-macro (template type)
  `(specialize inline (butlast seq ,type) ,type
     (subseq seq 0 (- (length seq) 1))))


(template
  
  (class Cell<T> extends Object
    
    (slot row T getter generate)
    (slot col T getter generate)))


(instantiate Cell<fx>)


(template (butlast<T> seq T) T
  (subseq seq 0 (- (cardinality seq) 1)))


(instantiate butlast<string>)


(specialize (butlast seq <string>) <string>
  (subseq seq 0 (- (length seq) 1)))


@w
(define-macro (instantiate-butlast type)
  `(specialize inline (butlast seq ,type) ,type
     (subseq seq 0 (- (length seq) 1)))))


;;;
;;;; templates
;;;


(export instantiate-for-each
        instantiate-butlast
        instantiate-find
        instantiate-find-in
        instantiate-starts-with?
        instantiate-ends-with?)


(define-macro (instantiate-for-each name T)
  `(specialize as ,name (for-each proc seq ,T)
     (let ((end (- (cardinality seq) 1)))
       (let (iterate (n 0))
         (when (<= n end)
           (proc (element seq n))
           (iterate (+ n 1)))))))


(define-macro (instantiate-butlast T)
  `(specialize (butlast seq ,T) ,T
     (subseq seq 0 (- (cardinality seq) 1))))


;; #f should be #f when this is moved into a jazz dialect file
;; using <fx> is not 100% correct and should also be part of the template or better have smarter inferences
(define-macro (instantiate-find name T)
  `(specialize as ,name (find seq ,T target (key: key #f) (test: test #f) (start: start #f) (end: end #f) (reversed?: reversed? #f)) <int+>
     (let ((len (cardinality seq))
           (test (or test eqv?))
           (inside (if (not reversed?) <= >=))
           (next (if (not reversed?) + -)))
       (let ((start <fx> (or start (if (not reversed?) 0 (- len 1))))
             (end <fx> (or end (if (not reversed?) (- len 1) 0))))
         (let (iterate (n start))
           (if (inside n end)
               (let ((obj (element seq n)))
                 (if (test (if key (key obj) obj) target)
                     n
                   (iterate (next n 1))))
             #f))))))


;; #f should be #f when this is moved into a jazz dialect file
;; using <fx> is not 100% correct and should also be part of the template or better have smarter inferences
(define-macro (instantiate-find-in name T)
  `(specialize as ,name (find-in seq ,T target (key: key #f) (test: test #f) (start: start #f) (end: end #f) (reversed?: reversed? #f))
     (let ((len (cardinality seq))
           (test (or test eqv?))
           (inside (if (not reversed?) <= >=))
           (next (if (not reversed?) + -)))
       (let ((start <fx> (or start (if (not reversed?) 0 (- len 1))))
             (end <fx> (or end (if (not reversed?) (- len 1) 0))))
         (let (iterate (n start))
           (if (inside n end)
               (let ((obj (element seq n)))
                 (if (test (if key (key obj) obj) target)
                     obj
                   (iterate (next n 1))))
             #f))))))


(define-macro (instantiate-starts-with? T)
  `(specialize (starts-with? seq ,T target ,T) <bool>
     (let ((slen (cardinality seq))
           (tlen (cardinality target)))
       (and (>= slen tlen)
            (= (subseq seq 0 tlen) target)))))


(define-macro (instantiate-ends-with? T)
  `(specialize (ends-with? seq ,T target ,T) <bool>
     (let ((slen (cardinality seq))
           (tlen (cardinality target)))
       (and (>= slen tlen)
            (= (subseq seq (- slen tlen) slen) target))))))
