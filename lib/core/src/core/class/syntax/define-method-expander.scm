;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Define Method Expander
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


(module core.class.syntax.define-method-expander


(define (jazz.expand-define-virtual signature bootstrap-type?)
  (let* ((name (%%car signature))
         (parameters (%%cdr signature))
         (class-name (%%caar parameters))
         (object-parameter (%%cadr (%%car parameters)))
         (extra-parameters (%%cdr parameters))
         (implementation-name (jazz.method-implementation-name class-name name))
         (rank-name (jazz.method-rank-name implementation-name))
         (is-test (if bootstrap-type? 'jazz.bootstrap-type? '%%class-is?)))
    `(jazz.define-macro (,name ,object-parameter ,@extra-parameters)
       (if (%%symbol? ,object-parameter)
           (%%list '%%core-assertion (list ',is-test ,object-parameter ',class-name) (list 'jazz.error (jazz.format "{s} expected in calling {s}: {s}" ',class-name ',name ,object-parameter))
             (%%list (%%list '%%vector-ref (%%list '%%get-class-core-vtable (%%list '%%get-object-class ,object-parameter)) ',rank-name)
                     ,object-parameter
                     ,@extra-parameters))
         (jazz.with-uniqueness ,object-parameter
           (lambda (obj)
             (%%list '%%core-assertion (list ',is-test obj ',class-name) (list 'jazz.error (jazz.format "{s} expected in calling {s}: {s}" ',class-name ',name ,object-parameter))
               (%%list (%%list '%%vector-ref (%%list '%%get-class-core-vtable (%%list '%%get-object-class obj)) ',rank-name)
                       obj
                       ,@extra-parameters))))))))


(define (jazz.expand-define-virtual-runtime signature)
  (let* ((name (%%car signature))
         (parameters (%%cdr signature))
         (class-name (%%caar parameters))
         (implementation-name (jazz.method-implementation-name class-name name))
         (rank-name (jazz.method-rank-name implementation-name)))
    `(define ,rank-name
       (jazz.register-virtual-name ,class-name ',name))))


(define (jazz.expand-define-method signature body)
  (let* ((name (%%car signature))
         (parameters (%%cdr signature))
         (class-name (%%caar parameters))
         (object-parameter (%%cadr (%%car parameters)))
         (extra-parameters (%%cdr parameters))
         (implementation-name (jazz.method-implementation-name class-name name)))
    `(begin
       (define ,implementation-name
         (let ((nextmethod (jazz.find-nextmethod ,class-name ',name)))
           (lambda (,object-parameter ,@extra-parameters)
             ,@body)))
       (jazz.register-method ,class-name ',name ,implementation-name))))


(define (jazz.method-implementation-name class-name name)
  (let ((name (jazz.last (jazz.split-string (%%symbol->string name) #\.))))
    (%%string->symbol (%%string-append (%%symbol->string class-name) "." name))))


(define (jazz.method-rank-name implementation-name)
  (%%string->symbol (%%string-append (%%symbol->string implementation-name) "!rank")))


(define (jazz.inherited-name? class method-name)
  (let ((ascendant (%%get-class-ascendant class)))
    (and ascendant
         (%%memq method-name (%%get-class-core-virtual-names ascendant)))))


(define (jazz.register-virtual-name class method-name)
  (%%set-class-core-virtual-names class
    (%%append (%%get-class-core-virtual-names class)
              (%%list method-name)))
  (%%fx- (%%length (%%get-class-core-virtual-names class))
         1))


(define (jazz.register-method class method-name method-implementation)
  (if (jazz.inherited-name? class method-name)
      (%%set-class-core-method-alist class
        (%%append (%%get-class-core-method-alist class)
                  (%%list (%%cons method-name method-implementation))))
    (%%set-class-core-virtual-alist class
      (%%append (%%get-class-core-virtual-alist class)
                (%%list (%%cons method-name method-implementation))))))


(define (jazz.get-method-rank class method-name)
  (let iter ((scan (%%get-class-core-virtual-names class))
             (rank 0))
    (if (%%eq? (%%car scan) method-name)
        rank
      (iter (%%cdr scan) (%%fx+ rank 1)))))


(define (jazz.get-method-implementation class method-name)
  (%%vector-ref (%%get-class-core-vtable class)
                (jazz.get-method-rank class method-name)))


(define (jazz.find-nextmethod class method-name)
  (if (jazz.inherited-name? class method-name)
      (jazz.get-method-implementation (%%get-class-ascendant class) method-name)
    (lambda (obj . rest)
      (jazz.error "No nextmethod for {s} on {s}" obj method-name)))))
