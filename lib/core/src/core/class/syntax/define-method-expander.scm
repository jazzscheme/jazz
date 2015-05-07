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


(unit protected core.class.syntax.define-method-expander


(define (jazz:expand-define-virtual-syntax signature bootstrap-type?)
  (let* ((name (jazz:reference-name (%%car signature)))
         (macro-name (%%car signature))
         (parameters (%%cdr signature))
         (class-name (%%caar parameters))
         (object-parameter (%%cadr (%%car parameters)))
         (extra-parameters (%%cdr parameters))
         (class-level-name (%%compose-helper class-name 'core-level))
         (implementation-name (jazz:method-implementation-name class-name name))
         (method-rank-name (jazz:method-rank-name implementation-name))
         (is-test (if bootstrap-type? 'jazz:bootstrap-type? '%%class-is?))
         (object (jazz:generate-symbol "object")))
    `(jazz:define-macro (,macro-name ,object-parameter ,@extra-parameters)
       (if (%%symbol? ,object-parameter)
           (%%list '%%core-assertion (%%list ',is-test ,object-parameter ',class-name) (%%list 'jazz:error (jazz:format "{s} expected in calling {s}: {s}" ',class-name ',name ,object-parameter))
             (%%list (%%list '%%class-dispatch (%%list '%%get-object-class ,object-parameter) ',class-level-name ',method-rank-name)
                     ,object-parameter
                     ,@extra-parameters))
         (jazz:with-uniqueness ,object-parameter
           (lambda (,object)
             (%%list '%%core-assertion (%%list ',is-test ,object ',class-name) (%%list 'jazz:error (jazz:format "{s} expected in calling {s}: {s}" ',class-name ',name ,object-parameter))
               (%%list (%%list '%%class-dispatch (%%list '%%get-object-class ,object) ',class-level-name ',method-rank-name)
                       ,object
                       ,@extra-parameters))))))))


(define (jazz:expand-define-virtual signature)
  (let* ((name (jazz:reference-name (%%car signature)))
         (parameters (%%cdr signature))
         (class-name (%%caar parameters))
         (implementation-name (jazz:method-implementation-name class-name name))
         (rank-name (jazz:method-rank-name implementation-name)))
    `(begin
       (define ,rank-name
         (jazz:add-core-virtual-method ,class-name ',name)))))


(define (jazz:expand-define-method signature body)
  (let* ((name (jazz:reference-name (%%car signature)))
         (parameters (%%cdr signature))
         (class-name (%%caar parameters))
         (object-parameter (%%cadr (%%car parameters)))
         (extra-parameters (%%cdr parameters))
         (implementation-name (jazz:method-implementation-name class-name name)))
    `(begin
       (define ,implementation-name
         (let ((nextmethod (jazz:find-nextmethod ,class-name ',name)))
           (lambda (,object-parameter ,@extra-parameters)
             ,@body)))
       (jazz:add-core-method-node ,class-name ',name ,implementation-name))))


(define (jazz:method-implementation-name class-name name)
  (let ((name (jazz:last (jazz:split-string (%%symbol->string name) #\.))))
    (%%string->symbol (%%string-append (%%symbol->string class-name) "^" name))))


(define (jazz:method-rank-name implementation-name)
  (%%compose-helper implementation-name 'rank)))
