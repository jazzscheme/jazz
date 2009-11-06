;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Dialect Syntax
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


(unit protected core.library.syntax.dialect


(jazz.define-class-runtime jazz.Dialect)


(jazz.define-virtual-runtime (jazz.dialect-name (jazz.Dialect dialect)))
(jazz.define-virtual-runtime (jazz.dialect-walker (jazz.Dialect dialect)))


(jazz.define-method (jazz.dialect-name (jazz.Dialect dialect))
  #f)


(jazz.define-method (jazz.dialect-walker (jazz.Dialect dialect))
  #f)


(jazz.encapsulate-class jazz.Dialect)


;;;
;;;; Dialects
;;;


(define jazz.Dialects
  (%%make-table test: eq?))


(define (jazz.get-dialect name)
  (%%table-ref jazz.Dialects name #f))


(define (jazz.require-dialect name)
  (or (jazz.get-dialect name)
      (jazz.error "Unknown dialect: {s}" name)))


(define (jazz.register-dialect name dialect)
  (%%table-set! jazz.Dialects name dialect))


(jazz.define-macro (jazz.define-dialect name dialect)
  `(jazz.register-dialect ',name ,dialect))


;;;
;;;; Bindings
;;;


(define (jazz.register-binding dialect-name binding)
  (let ((dialect (jazz.get-dialect dialect-name)))
    (%%set-dialect-bindings dialect (%%cons binding (%%get-dialect-bindings dialect)))))


(jazz.define-macro (jazz.define-walker-special name dialect-name method)
  `(jazz.register-binding ',dialect-name (jazz.new-special-form ',name ,method)))


(jazz.define-macro (jazz.define-walker-syntax name dialect-name method)
  `(jazz.register-binding ',dialect-name (jazz.new-syntax-form ',name ,method)))


(jazz.define-macro (jazz.define-walker-macro name dialect-name method)
  `(jazz.register-binding ',dialect-name (jazz.new-macro-form ',name ,method))))
