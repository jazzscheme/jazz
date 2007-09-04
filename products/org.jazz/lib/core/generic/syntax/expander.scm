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
;;;  Portions created by the Initial Developer are Copyright (C) 1996-2006
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


(module core.generic.syntax.expander


;;;
;;;; Generic
;;;


(define (jazz.expand-define-generic signature)
  (let* ((method-locator (%%car signature))
         (parameters (%%cdr signature))
         (parameter-names (%%cons (jazz.specific-parameter-name (%%car parameters)) (%%cdr parameters)))
         (parameter-access (%%list (jazz.specific-parameter-access (%%car parameters))))
         (rest-parameter (jazz.last-pair parameter-names))
         (other-parameters (jazz.proper-list parameter-names))
         (generic-locator (jazz.generic-object-locator method-locator))
         (gensym-specific (jazz.generate-symbol "specific")))
    `(begin
       (define ,generic-locator
         ;; this is a quicky that needs to be well tought out
         (if (jazz.global-variable? ',generic-locator)
             (jazz.global-value ',generic-locator)
           (jazz.new-generic ',method-locator (lambda () (%%list ,@parameter-access)))))
       (define ,method-locator
         (lambda ,parameter-names
           (%%when (%%not (%%null? (%%get-generic-pending-specifics ,generic-locator)))
             (jazz.update-generic ,generic-locator))
           (let ((,gensym-specific (%%need-specific-implementation ,generic-locator ,(%%car parameter-names))))
             ,(if (%%symbol? rest-parameter)
                  `(apply ,gensym-specific ,@other-parameters ,rest-parameter)
                `(,gensym-specific ,@parameter-names))))))))


(define (jazz.generic-object-locator locator)
  (%%string->symbol (%%string-append (%%symbol->string locator) "!generic")))


;;;
;;;; Specific
;;;


(define (jazz.expand-define-specific signature . body)
  (let* ((method-locator (%%car signature))
         (parameters (%%cdr signature))
         (parameter-access (%%list (jazz.specific-parameter-access (%%car parameters))))
         (parameter-names (%%cons (jazz.specific-parameter-name (%%car parameters)) (%%cdr parameters)))
         (generic-locator (jazz.generic-object-locator method-locator))
         (specific-locator (jazz.implementation-name method-locator parameter-access))
         (gensym-specific (jazz.generate-symbol "specific"))
         (gensym-lambda (jazz.generate-symbol "lambda")))
    `(define ,specific-locator
       (let* ((,gensym-specific (jazz.new-specific (lambda () (%%list ,@parameter-access)) #f))
              (,gensym-lambda (lambda ,parameter-names
                                (let ((nextmethod (%%get-specific-next-implementation ,gensym-specific)))
                                  ,@body))))
         (%%set-specific-implementation ,gensym-specific ,gensym-lambda)
         (jazz.register-specific ,generic-locator ,gensym-specific)
         ,gensym-lambda))))


(define (jazz.specific-parameter-access parameter)
  (if (%%pair? parameter)
      (%%car parameter)
    #f))


(define (jazz.specific-parameter-name parameter)
  (if (%%pair? parameter)
      (%%cadr parameter)
    parameter))


(define (jazz.implementation-name name parameter-access)
  (let ((virtual-types (map jazz.access->name parameter-access)))
    ;; special case to make implementation name more readable until a jazz debugger
    (if (= (length virtual-types) 1)
        (let ((name (jazz.last (jazz.split-identifier name))))
          (%%string->symbol (%%string-append (car virtual-types) "." (symbol->string name) ":implementation")))
      (%%string->symbol (%%string-append (%%symbol->string name) ":implementation." (jazz.join-strings virtual-types "/"))))))


(define (jazz.access->name access)
  (cond ((%%string? access)
         access)
        ((%%symbol? access)
         (%%symbol->string access))
        ((and (%%pair? access) (%%not (%%null? (%%cdr access))))
         (jazz.access->name (%%cadr access)))
        (else
         (jazz.error "Unable to extract name from: {s}" access))))


;;;
;;;; Debug
;;;


(define (jazz.display-tree generic)
  (let ((getit (lambda (specific) (%%car (%%get-specific-signature specific)))))
    (letrec ((display-specific
               (lambda (specific level)
                 (let ((previous-specifics (%%get-specific-previous-specifics specific)))
                   (write (%%list level
                                  (getit specific)
                                  (%%get-specific-next-specific specific)
                                  specific
                                  previous-specifics))
                   (newline)
                   (for-each (lambda (specific)
                               (display-specific specific (%%fixnum+ level 1)))
                             previous-specifics)))))
      (display-specific (%%get-generic-root-specific generic) 0)))))
