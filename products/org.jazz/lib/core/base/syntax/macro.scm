;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Macros
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


(module core.base.syntax.macro


(define jazz.Macros
  (%%make-hashtable eq?))


(define (jazz.register-macro name macro)
  (%%hashtable-set! jazz.Macros name macro))


(define (jazz.get-macro name)
  (%%hashtable-ref jazz.Macros name #f))


(define (jazz.need-macro name)
  (or (jazz.get-macro name)
      (jazz.error "Unable to find macro: {s}" name)))


(define (jazz.expand-global-macro form)
  (apply (jazz.need-macro (%%car form)) (%%cdr form)))


(define-macro (jazz.define-macro signature . body)
  `(begin
     (define-macro ,signature
       ,@body)
     (jazz.register-macro ',(%%car signature)
       (lambda ,(%%cdr signature)
         ,@body))))


(define-macro (jazz.define-syntax signature . body)
  `(begin
     (##define-syntax ,(%%car signature)
       (lambda ,(%%cadr signature)
         ,@body))
     #;
     (jazz.register-macro ',(%%car signature)
       (lambda ,(%%cdr signature)
         ,@body))))


(define jazz.generate-symbol
  (let ((unique 0))
    (lambda rest
      (let ((prefix (if (%%null? rest) "sym" (%%car rest)))
            (port (open-output-string)))
        (display "__" port)
        (display prefix port)
        (display unique port)
        (set! unique (%%fixnum+ unique 1))
        (%%string->symbol (get-output-string port)))))))
