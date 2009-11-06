;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Library Runtime
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


(unit protected core.library.runtime


(require (core.library.runtime.autoload))


;;;
;;;; Library
;;;


(jazz.define-class jazz.Library jazz.Object () jazz.Object-Class jazz.allocate-library
  ((name    %%get-library-name    ())
   (exports %%get-library-exports ())))


(jazz.define-class-runtime jazz.Library)


(define (jazz.new-library name exports)
  (jazz.allocate-library jazz.Library name exports))


(jazz.encapsulate-class jazz.Library)


;;;
;;;; Libraries
;;;


(define jazz.Libraries
  (%%make-table test: eq?))


(define (jazz.register-library name exports-list)
  (let ((library (jazz.new-library name (%%list->table exports-list test: eq?))))
    (%%table-set! jazz.Libraries name library)
    library))


(define (jazz.get-library name)
  (jazz.load-unit name)
  (%%table-ref jazz.Libraries name #f))


(define (jazz.require-library name)
  (or (jazz.get-library name)
      (jazz.error "Unknown public library: {s}" name)))


(define (jazz.library-get library-name name #!key (not-found #f))
  (let ((library (jazz.require-library library-name)))
    (let ((info (%%table-ref (%%get-library-exports library) name #f)))
      (if info
          (if (%%symbol? info)
              (jazz.global-value info)
            (jazz.bind (unit-name . locator) info
              (jazz.load-unit unit-name)
              (jazz.global-value locator)))
        not-found))))


(define jazz.library-ref
  (let ((not-found (box #f)))
    (lambda (library-name name)
      (let ((obj (jazz.library-get library-name name not-found: not-found)))
        (if (%%eq? obj not-found)
            (jazz.error "Unable to find '{s} in: {s}" name library-name)
          obj)))))


;;;
;;;; Error
;;;


(define (jazz.type-error value type)
  (jazz.error "{s} expected: {s}" type value))


(define (jazz.dispatch-error field value category)
  (jazz.error "Inconsistent dispatch on method {a}: {s} is not of the expected {s} type" (%%get-field-name field) value (%%get-category-name category))))
