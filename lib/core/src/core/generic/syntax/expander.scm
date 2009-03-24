;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Generic Expander
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
;;;  Portions created by the Initial Developer are Copyright (C) 1996-2008
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


(module protected core.generic.syntax.expander


(define (jazz.dynamic-parameter? parameter)
  (and (%%pair? parameter)
       (%%pair? (%%cdr parameter))))


(define (jazz.dynamic-parameter-type parameter)
  (let ((type (%%car parameter)))
    (if (jazz.specifier? type)
        (jazz.specifier->name type)
      type)))


(define (jazz.dynamic-parameter-types parameters)
  (let iterate ((parameters parameters))
       (if (and (%%pair? parameters)
                (jazz.dynamic-parameter? (%%car parameters)))
           (%%cons (jazz.dynamic-parameter-type (%%car parameters))
                   (iterate (%%cdr parameters)))
         '())))


(define (jazz.dynamic-parameter-name parameter)
  (%%cadr parameter))


(define (jazz.dynamic-parameter-names parameters)
  (let iterate ((parameters parameters))
       (if (and (%%pair? parameters)
                (jazz.dynamic-parameter? (%%car parameters)))
           (%%cons (jazz.dynamic-parameter-name (%%car parameters))
                   (iterate (%%cdr parameters)))
         '())))


(define (jazz.generic-parameters parameters)
  (let iterate ((parameters parameters)
                (result '()))
       (if (%%pair? parameters)
           (let ((parameter (%%car parameters)))
             (cond ((jazz.dynamic-parameter? parameter)
                    (iterate (%%cdr parameters) (%%cons (jazz.dynamic-parameter-name parameter) result)))
                   ((%%symbol? parameter)
                    (iterate (%%cdr parameters) (%%cons parameter result)))
                   (else ; #!xxx -> parsing delayed to specific
                    (values (jazz.reverse! result) parameters))))
         (values (jazz.reverse! result) parameters))))


(define (jazz.specific-parameters parameters)
  (let iterate ((parameters parameters))
       (if (%%pair? parameters)
           (let ((parameter (%%car parameters)))
             (cond ((jazz.dynamic-parameter? parameter)
                    (%%cons (jazz.dynamic-parameter-name parameter) (iterate (%%cdr parameters))))
                   (else
                    parameters)))
         parameters)))


;;;
;;;; Generic
;;;


(define (jazz.expand-define-generic signature . body)
  (let* ((generic-method-locator (%%car signature))
         (parameters (%%cdr signature))
         (dynamic-signature (jazz.dynamic-parameter-types parameters))
         (formal-signature (jazz.specific-parameters parameters))
         (specific-implementation-locator (jazz.implementation-locator generic-method-locator dynamic-signature))
         (generic-locator (jazz.generic-object-locator generic-method-locator))
         (gensym-generic (jazz.generate-symbol "generic"))
         (gensym-specific (jazz.generate-symbol "specific")))
    (receive (mandatory-parameters extra-parameters) (jazz.generic-parameters parameters)
      (let ((gensym-rest (if (%%null? extra-parameters) '() (jazz.generate-symbol "rest"))))
        `(begin
           (define ,specific-implementation-locator
             ,(if (%%null? body)
                  `(lambda ,formal-signature
                     (jazz.error "Generic {a} is abstract" ,generic-method-locator))
                `(lambda ,formal-signature
                   ,@body)))
           (define ,generic-locator
             ;; we do not reprocess generic - aka you must restart to change its signature
             (if (jazz.global-variable? ',generic-locator)
                 (let ((,gensym-generic (jazz.global-value ',generic-locator)))
                   (jazz.generic-reset ,gensym-generic ,specific-implementation-locator)
                   ,gensym-generic)
               (jazz.new-generic ',generic-method-locator (lambda () (%%list ,@dynamic-signature)) ,specific-implementation-locator)))
           (define ,generic-method-locator
             (lambda (,@mandatory-parameters . ,gensym-rest)
               (%%when (%%not (%%null? (%%get-generic-pending-specifics ,generic-locator)))
                 (jazz.process-pending-specifics ,generic-locator))
               (let ((,gensym-specific (%%specific-dispatch ,generic-locator (list ,@(map (lambda (parameter) `(%%class-of ,parameter))
                                                                                          (jazz.dynamic-parameter-names parameters))))))
                 ,(if (%%null? extra-parameters)
                      `(,gensym-specific ,@mandatory-parameters)
                    `(apply ,gensym-specific ,@mandatory-parameters ,gensym-rest))))))))))


(define (jazz.generic-object-locator locator)
  (%%string->symbol (%%string-append (%%symbol->string locator) "!generic")))


;;;
;;;; Specific
;;;


(define (jazz.expand-define-specific signature modifier . body)
  (let* ((root? (%%eq? modifier 'root))
         (generic-method-locator (%%car signature))
         (parameters (%%cdr signature))
         (dynamic-signature (jazz.dynamic-parameter-types parameters))
         (formal-signature (jazz.specific-parameters parameters))
         (specific-implementation-locator (jazz.implementation-locator generic-method-locator dynamic-signature))
         (generic-locator (jazz.generic-object-locator generic-method-locator))
         (gensym-specific (jazz.generate-symbol "specific"))
         (gensym-lambda (jazz.generate-symbol "lambda"))
         (nextmethod-bindings (if root? (list) (list `(nextmethod (%%get-specific-implementation (%%car (%%get-specific-ancestor-specifics ,gensym-specific))))))))
    `(define ,specific-implementation-locator
       (let* ((,gensym-specific (jazz.new-specific (lambda () (%%list ,@dynamic-signature)) #f))
              (,gensym-lambda (lambda ,formal-signature
                                (let (,@nextmethod-bindings)
                                  ,@body))))
         (%%set-specific-implementation ,gensym-specific ,gensym-lambda)
         (jazz.register-specific ,generic-locator ,gensym-specific)
         ,gensym-lambda))))


(define (jazz.implementation-locator generic-locator dynamic-signature)
  (let ((generic-string (%%symbol->string generic-locator))
        (dynamic-signature-strings (map (lambda (class/call)
                                          (symbol->string (if (%%pair? class/call) (car class/call) class/call)))
                                        dynamic-signature)))
    (%%string->symbol (%%string-append generic-string ":implementation:" (jazz.join-strings dynamic-signature-strings "/"))))))
