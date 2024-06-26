;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Generic Syntax
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
;;;  The Initial Developer of the Original Code is Stephane Le Cornec.
;;;  Portions created by the Initial Developer are Copyright (C) 1996-2018
;;;  the Initial Developer. All Rights Reserved.
;;;
;;;  Contributor(s):
;;;    Guillaume Cartier
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


(block core.generic-syntax


;;;
;;;; Expansion
;;;


(define (jazz:dynamic-parameter? parameter)
  (and (%%pair? parameter)
       (let ((type (%%car parameter)))
         (or (%%pair? type)
             (jazz:composite-identifier? type)))
       (%%pair? (%%cdr parameter))))


(define (jazz:dynamic-parameter-type parameter)
  (let ((type (%%car parameter)))
    (if (jazz:specifier? type)
        (jazz:specifier->name type)
      type)))


(define (jazz:dynamic-parameter-types parameters)
  (let iterate ((parameters parameters))
       (if (and (%%pair? parameters)
                (jazz:dynamic-parameter? (%%car parameters)))
           (%%cons (jazz:dynamic-parameter-type (%%car parameters))
                   (iterate (%%cdr parameters)))
         '())))


(define (jazz:dynamic-parameter-name parameter)
  (%%cadr parameter))


(define (jazz:dynamic-parameter-names parameters)
  (let iterate ((parameters parameters))
       (if (and (%%pair? parameters)
                (jazz:dynamic-parameter? (%%car parameters)))
           (%%cons (jazz:dynamic-parameter-name (%%car parameters))
                   (iterate (%%cdr parameters)))
         '())))


(define (jazz:generic-parameters parameters)
  (let iterate ((parameters parameters)
                (result '()))
       (if (%%pair? parameters)
           (let ((parameter (%%car parameters)))
             (cond ((jazz:dynamic-parameter? parameter)
                    (iterate (%%cdr parameters) (%%cons (jazz:dynamic-parameter-name parameter) result)))
                   ((%%symbol? parameter)
                    (iterate (%%cdr parameters) (%%cons parameter result)))
                   (else ; #!xxx -> parsing delayed to specific
                    (values (jazz:reverse! result) parameters))))
         (values (jazz:reverse! result) parameters))))


(define (jazz:specific-parameters parameters)
  (let iterate ((parameters parameters))
       (if (%%pair? parameters)
           (let ((parameter (%%car parameters)))
             (cond ((jazz:dynamic-parameter? parameter)
                    (%%cons (jazz:dynamic-parameter-name parameter) (iterate (%%cdr parameters))))
                   (else
                    parameters)))
         parameters)))


;;;
;;;; Generic
;;;


(define (jazz:expand-define-generic signature . body)
  (let* ((generic-method-locator (%%car signature))
         (parameters (%%cdr signature))
         (dynamic-signature (jazz:dynamic-parameter-types parameters))
         (formal-signature (jazz:specific-parameters parameters))
         (specific-implementation-locator (jazz:implementation-locator generic-method-locator dynamic-signature))
         (generic-locator (jazz:generic-object-locator generic-method-locator))
         (gensym-generic (jazz:generate-symbol "generic"))
         (gensym-specific (jazz:generate-symbol "specific")))
    (receive (mandatory-parameters extra-parameters) (jazz:generic-parameters parameters)
      (let ((gensym-rest (if (%%null? extra-parameters) '() (jazz:generate-symbol "rest"))))
        `(begin
           (define ,specific-implementation-locator
             ,(if (%%null? body)
                  `(lambda ,formal-signature
                     (jazz:error "Generic {a} is abstract" ,generic-method-locator))
                `(lambda ,formal-signature
                   ,@body)))
           (define ,generic-locator
             ;; we do not reprocess generic - aka you must restart to change its signature
             (if (jazz:global-bound? ',generic-locator)
                 (let ((,gensym-generic (jazz:global-ref ',generic-locator)))
                   (jazz:generic-reset ,gensym-generic ,specific-implementation-locator)
                   ,gensym-generic)
               (jazz:new-generic ',generic-method-locator (lambda () (%%list ,@dynamic-signature)) ,specific-implementation-locator)))
           (define ,generic-method-locator
             (lambda (,@mandatory-parameters . ,gensym-rest)
               (%%when (%%not (%%null? (jazz:get-generic-pending-specifics ,generic-locator)))
                 (jazz:process-pending-specifics ,generic-locator))
               (let ((,gensym-specific (%%specific-dispatch ,generic-locator (%%list ,@(map (lambda (parameter) `(jazz:class-of ,parameter))
                                                                                            (jazz:dynamic-parameter-names parameters))))))
                 ,(if (%%null? extra-parameters)
                      `(,gensym-specific ,@mandatory-parameters)
                    `(apply ,gensym-specific ,@mandatory-parameters ,gensym-rest))))))))))


(define (jazz:generic-object-locator locator)
  (%%string->symbol (%%string-append (%%symbol->string locator) "!generic")))


;;;
;;;; Specific
;;;


(define (jazz:expand-define-specific signature modifier . body)
  (let* ((root? (%%eq? modifier 'root))
         (generic-method-locator (%%car signature))
         (parameters (%%cdr signature))
         (dynamic-signature (jazz:dynamic-parameter-types parameters))
         (formal-signature (jazz:specific-parameters parameters))
         (specific-implementation-locator (jazz:implementation-locator generic-method-locator dynamic-signature))
         (generic-locator (jazz:generic-object-locator generic-method-locator))
         (gensym-specific (jazz:generate-symbol "specific"))
         (gensym-lambda (jazz:generate-symbol "lambda"))
         (nextmethod-bindings (if root? (%%list) (%%list `(nextmethod (jazz:get-specific-implementation (%%car (jazz:get-specific-ancestor-specifics ,gensym-specific))))))))
    `(define ,specific-implementation-locator
       (let* ((,gensym-specific (jazz:new-specific (lambda () (%%list ,@dynamic-signature)) #f))
              (,gensym-lambda (lambda ,formal-signature
                                (let (,@nextmethod-bindings)
                                  ,@body))))
         (jazz:set-specific-implementation ,gensym-specific ,gensym-lambda)
         (jazz:register-specific ,generic-locator ,gensym-specific)
         ,gensym-lambda))))


(define (jazz:implementation-locator generic-locator dynamic-signature)
  (let ((generic-string (%%symbol->string generic-locator))
        (dynamic-signature-strings (map (lambda (class/call)
                                          (%%symbol->string (if (%%pair? class/call) (%%car class/call) class/call)))
                                        dynamic-signature)))
    (%%string->symbol (%%string-append generic-string ":implementation:" (jazz:join-strings dynamic-signature-strings "/")))))


;;;
;;;; Generic
;;;


;; locator - a.b.c.foo
;; name - foo
;; root-specific - (maybe abstract) specific matching generic signature
;; pending-specifics - delayed evaluation at boot -> signatures are proc (unnecessary now?)


(jazz:define-class jazz:Generic jazz:Object (constructor: jazz:allocate-generic)
  ((locator              getter: generate)
   (name                 getter: generate)
   (root-specific        getter: generate setter: generate)
   (pending-specifics    getter: generate setter: generate)))


(jazz:define-macro (jazz:define-generic . rest)
  (%%apply jazz:expand-define-generic rest))


(jazz:define-macro (%%specific-dispatch generic dynamic-classes)
  `(jazz:get-specific-implementation (jazz:dispatch-from-root ,generic ,dynamic-classes)))


;;;
;;;; Specific
;;;


;; dynamic-signature - classes of the dynamic parameters
;; implementation - lambda to call
;; best-ancestor - nextmethod specific
;; ancestor-specifics - nextmethod specific + other direct ancestors
;; descendant-specifics - reverse relation


(jazz:define-class jazz:Specific jazz:Object (constructor: jazz:allocate-specific)
  ((dynamic-signature    getter: generate setter: generate)
   (implementation       getter: generate setter: generate)
   (ancestor-specifics   getter: generate setter: generate)
   (descendant-specifics getter: generate setter: generate)))


(jazz:define-macro (jazz:define-specific . rest)
  (%%apply jazz:expand-define-specific rest)))
