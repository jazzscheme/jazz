;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Scheme Backend Runtime Syntax
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


(unit protected backend.scheme.runtime.syntax


;;;
;;;; Module
;;;


(jazz:define-class-syntax jazz:Module jazz:Object (constructor: jazz:allocate-module accessors-type: macro)
  ((name    getter: generate)
   (access  getter: generate)
   (exports getter: generate)
   (entries getter: generate setter: generate)))


(jazz:define-class-runtime jazz:Module)


(define (jazz:new-module name access)
  (jazz:allocate-module name access (%%make-table test: eq?) #f))


;;;
;;;; Native
;;;


(jazz:define-class-syntax jazz:Native jazz:Field (constructor: jazz:allocate-native accessors-type: macro)
  ((symbol getter: generate)))


(jazz:define-class-runtime jazz:Native)


(define (jazz:new-native name symbol)
  (jazz:allocate-native name symbol))


(define (jazz:register-native module-name name symbol)
  (jazz:register-module-entry module-name name (jazz:new-native name symbol)))


;;;
;;;; Runtime Reference
;;;


(jazz:define-class-syntax jazz:Runtime-Reference jazz:Object (constructor: jazz:allocate-runtime-reference accessors-type: macro)
  ((resolver      getter: generate)
   (serialization getter: generate)))


(jazz:define-class-runtime jazz:Runtime-Reference)


(define (jazz:new-runtime-reference resolver serialization)
  (jazz:allocate-runtime-reference resolver serialization))


(define (jazz:resolve-runtime-reference runtime-reference)
  (%%debug-assert (%%is? runtime-reference jazz:Runtime-Reference)
    (let ((resolver (%%get-runtime-reference-resolver runtime-reference)))
      (resolver))))


(define (jazz:serialize-runtime-reference runtime-reference)
  (%%debug-assert (%%is? runtime-reference jazz:Runtime-Reference)
    (%%get-runtime-reference-serialization runtime-reference)))


(define (jazz:deserialize-runtime-reference serialization)
  (define (deserialize-module-private)
    (jazz:new-runtime-reference (lambda ()
                                  (let ((locator (%%cadr serialization)))
                                    (jazz:load-unit (jazz:reference-unit locator))
                                    (jazz:global-ref locator)))
                                serialization))
  
  (define (deserialize-module-public)
    (jazz:new-runtime-reference (lambda ()
                                  (let ((module-name (%%cadr serialization))
                                        (name (%%car (%%cddr serialization))))
                                    (jazz:module-ref module-name name)))
                                serialization))
  
  (or (if (%%pair? serialization)
          (case (%%car serialization)
            ((module-private) (deserialize-module-private))
            ((module-public) (deserialize-module-public))
            (else #f))
        #f)
      (jazz:error "Unable to deserialize runtime reference: {s}" serialization)))


;;;
;;;; Modules
;;;


(define jazz:Modules
  (%%make-table test: eq?))


(define (jazz:get-modules-table)
  jazz:Modules)


(define (jazz:register-module name access exported-modules exported-symbols)
  (let ((module (or (jazz:find-module name) (jazz:new-module name access))))
    (let ((exports (%%get-module-exports module)))
      (for-each (lambda (module-name)
                  (jazz:iterate-table (%%get-module-exports (jazz:require-module module-name))
                    (lambda (name info)
                      (%%table-set! exports name info))))
                exported-modules)
      (for-each (lambda (pair)
                  (let ((name (%%car pair))
                        (info (%%cdr pair)))
                    (%%table-set! exports name info)))
                exported-symbols)
      (%%table-set! jazz:Modules name module)
      module)))


(define (jazz:find-module name)
  (%%table-ref jazz:Modules name #f))


(define (jazz:require-module name)
  (jazz:load-unit name)
  (or (jazz:find-module name)
      (jazz:error "Unknown module: {s}" name)))


(define (jazz:get-module-entry module-name entry-name)
  (let ((entries (%%get-module-entries (jazz:find-module module-name))))
    (and entries (%%table-ref entries entry-name #f))))

(define (jazz:set-module-entry module-name entry-name entry)
  (let ((module (jazz:find-module module-name)))
    (let ((entries (%%get-module-entries module)))
      (if entries
          (%%table-set! entries entry-name entry)
        (let ((table (%%make-table test: eq?)))
          (%%set-module-entries module table)
          (%%table-set! table entry-name entry))))))

(define (jazz:register-module-entry module-name entry-name entry)
  (jazz:set-module-entry module-name entry-name entry))


(define (jazz:module-get module-name name #!key (not-found #f))
  (let ((module (jazz:require-module module-name)))
    (let ((info (%%table-ref (%%get-module-exports module) name #f)))
      (if info
          (if (%%symbol? info)
              (jazz:global-ref info)
            (jazz:bind (unit-name . locator) info
              (jazz:load-unit unit-name)
              (jazz:global-ref locator)))
        not-found))))


(define jazz:module-ref
  (let ((not-found (box #f)))
    (lambda (module-name name)
      (let ((obj (jazz:module-get module-name name not-found: not-found)))
        (if (%%eq? obj not-found)
            (jazz:error "Unable to find '{s} in: {s}" name module-name)
          obj)))))


(define (jazz:module-set! module-name name value)
  (let ((module (jazz:require-module module-name)))
    (let ((info (%%table-ref (%%get-module-exports module) name #f)))
      (if info
          (if (%%symbol? info)
              (jazz:global-set! info value)
            (jazz:bind (unit-name . locator) info
              (jazz:load-unit unit-name)
              (jazz:global-set! locator value)))
        (jazz:error "Unable to find '{s} in: {s}" name module-name)))))


;;;
;;;; Error
;;;


(define (jazz:type-error value type)
  (jazz:error "{s} expected: {s}" type value))


(define (jazz:dispatch-error field value category)
  (jazz:error "Inconsistent dispatch on method {a}: {s} is not of the expected {s} type" (%%get-field-name field) value (%%get-category-identifier category))))
