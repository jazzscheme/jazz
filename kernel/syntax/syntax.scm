;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Kernel Syntax
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


(jazz.kernel-declare)


;;;
;;;; Debug
;;;


(jazz.define-macro (jazz.block-tail-call form)
  (if jazz.debug-user?
      `(begin
         ,form
         #f)
    form))


;;;
;;;; Compile
;;;


(define jazz.compile-options
  (let ((base-options
          (if (%%memq jazz.safety '(core debug))
              '(debug-environments)
            ;; should probably remove when the core is very stable
            '(debug-environments))))
    (if (%%memq 'source jazz.options)
        (cons 'debug-source base-options)
      base-options)))


;;;
;;;; Variable
;;;


(jazz.define-macro (jazz.define-variable name . rest)
  (let ((expr (if (%%null? rest) #f (%%car rest))))
    `(begin
       (define ,name #f)
       (set! ,name ,expr))))


;;;
;;;; Repository
;;;


;; A repository is where packages are located. The system comes with the following already
;; present repositories: build, install, jazz and user. Note that repository order is important
;; as it defines search precedence.


(jazz.define-macro (%%make-repository name directory)
  `(%%vector 'repository ,name ,directory #f))


(jazz.define-macro (%%repository-name repository)
  `(%%vector-ref ,repository 1))

(jazz.define-macro (%%repository-directory repository)
  `(%%vector-ref ,repository 2))

(jazz.define-macro (%%repository-packages-table repository)
  `(%%vector-ref ,repository 3))

(jazz.define-macro (%%repository-packages-table-set! repository packages-table)
  `(%%vector-set! ,repository 3 ,packages-table))


;;;
;;;; Package
;;;


;; A package is the deployment unit that groups together related resources. Packages are
;; discovered automatically and their order within their repository should not be relevant.


(jazz.define-macro (%%make-package repository name root path install products)
  `(%%vector 'package ,repository ,name ,root ,path ,install ,products (%%make-table test: eq?)))


(jazz.define-macro (%%package-repository package)
  `(%%vector-ref ,package 1))

(jazz.define-macro (%%package-name package)
  `(%%vector-ref ,package 2))

(jazz.define-macro (%%package-root package)
  `(%%vector-ref ,package 3))

(jazz.define-macro (%%package-path package)
  `(%%vector-ref ,package 4))

(jazz.define-macro (%%package-install package)
  `(%%vector-ref ,package 5))

(jazz.define-macro (%%package-products package)
  `(%%vector-ref ,package 6))

(jazz.define-macro (%%package-autoloads package)
  `(%%vector-ref ,package 7))


;;;
;;;; Product
;;;


;; A product is the runtime implementation of some user level entity that can be run and updated / built.


(jazz.define-macro (%%make-product name title icon run update build)
  `(%%vector 'product ,name ,title ,icon ,run ,update ,build))


(jazz.define-macro (%%product-name product)
  `(%%vector-ref ,product 1))

(jazz.define-macro (%%product-title product)
  `(%%vector-ref ,product 2))

(jazz.define-macro (%%product-icon product)
  `(%%vector-ref ,product 3))

(jazz.define-macro (%%product-run product)
  `(%%vector-ref ,product 4))

(jazz.define-macro (%%product-update product)
  `(%%vector-ref ,product 5))

(jazz.define-macro (%%product-build product)
  `(%%vector-ref ,product 6))


;;;
;;;; Resource
;;;


;; A resource is a triplet (package path . extension) representing a resource inside
;; a package. Compilation will use the path part to put the binary outputs under the
;; build subdir of the architecture directory to enable a cross-compilation scheme.


(jazz.define-macro (%%make-resource package path extension)
  `(%%vector 'resource ,package ,path ,extension))


(jazz.define-macro (%%resource-package resource)
  `(%%vector-ref ,resource 1))

(jazz.define-macro (%%resource-path resource)
  `(%%vector-ref ,resource 2))

(jazz.define-macro (%%resource-extension resource)
  `(%%vector-ref ,resource 3))


;;;
;;;; Digest
;;;


(jazz.define-macro (%%make-digest hash time identical?)
  `(%%vector 'digest ,hash ,time ,identical?))


(jazz.define-macro (%%digest-hash digest)
  `(%%vector-ref ,digest 1))

(jazz.define-macro (%%digest-cached-time digest)
  `(%%vector-ref ,digest 2))

(jazz.define-macro (%%digest-cached-time-set! digest time)
  `(%%vector-set! ,digest 2 ,time))

(jazz.define-macro (%%digest-cached-identical? digest)
  `(%%vector-ref ,digest 3))

(jazz.define-macro (%%digest-cached-identical?-set! digest identical?)
  `(%%vector-set! ,digest 3 ,identical?))


;;;
;;;; Manifest
;;;


(jazz.define-macro (%%make-manifest name digest)
  `(%%vector 'manifest ,name ,digest))


(jazz.define-macro (%%manifest-name manifest)
  `(%%vector-ref ,manifest 1))

(jazz.define-macro (%%manifest-digest manifest)
  `(%%vector-ref ,manifest 2))


;;;
;;;; Module
;;;


(jazz.define-syntax module
  (lambda (src)
    (let ((form (%%source-code src)))
      (let ((name (%%source-code (%%cadr form)))
            (rest (%%cddr form)))
        (if (%%neq? name (jazz.requested-module-name))
            (jazz.error "Module at {s} is defining {s}" (jazz.requested-module-name) name)
          (jazz.expand-module name rest))))))
