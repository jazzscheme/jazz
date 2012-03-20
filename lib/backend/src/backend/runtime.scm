;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Backend Runtime
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


(unit backend.runtime


;;;
;;;; Backend
;;;


(jazz:define-class-runtime jazz:Backend)


(define (jazz:new-backend name)
  (jazz:allocate-backend jazz:Backend name (%%make-table test: eq?)))


(define (jazz:get-backend-binding backend name)
  (%%table-ref (%%get-backend-bindings backend) name #f))

(define (jazz:require-backend-binding backend name)
  (or (jazz:get-backend-binding backend name)
      (jazz:error "Unknown {a} backend binding: {s}" (%%get-backend-name backend) name)))


(define (jazz:add-backend-binding backend name binding)
  (%%table-set! (%%get-backend-bindings backend) name binding))


(define (jazz:emit binding backend . rest)
  (apply (jazz:require-backend-binding backend binding) backend rest))


;;;
;;;; Backends
;;;


(define jazz:Backends
  (%%make-table test: eq?))


(define (jazz:get-backend name)
  (%%table-ref jazz:Backends name #f))


(define (jazz:require-backend name)
  (or (jazz:get-backend name)
      (jazz:error "Unknown backend: {s}" name)))


(define (jazz:register-backend backend)
  (let ((name (%%get-backend-name backend)))
    (%%table-set! jazz:Backends name backend)))


(define (jazz:register-emit backend-name emit emitter)
  (jazz:add-backend-binding (jazz:require-backend backend-name) emit emitter)))
