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


(block kernel.syntax


(jazz:kernel-declares)


;;;
;;;; Setting
;;;


(jazz:define-macro (jazz:define-setting name . rest)
  (let ((expr (if (%%null? rest) #f (%%car rest)))
        (global (jazz:generate-symbol (symbol->string name))))
    `(begin
       (define ,global ,expr)
       (define (,name . rest)
         (if (%%null? rest)
             ,global
           (set! ,global (%%car rest)))))))


;;;
;;;; Variable
;;;


(jazz:define-macro (jazz:define-variable name . rest)
  (let ((expr (if (%%null? rest) #f (%%car rest))))
    `(begin
       (define ,name #f)
       (set! ,name ,expr))))


;;;
;;;; Repository
;;;


;; A repository is where packages are located. The system comes with the following already
;; present repositories: build and jazz: Note that repository order is important as it
;; defines search precedence.


(jazz:define-macro (%%make-repository name directory library-root library-directory binary?)
  `(%%vector 'repository ,name ,directory ,library-root ,library-directory ,binary? #f))


(jazz:define-macro (%%repository-name repository)
  `(%%vector-ref ,repository 1))

(jazz:define-macro (%%repository-directory repository)
  `(%%vector-ref ,repository 2))

(jazz:define-macro (%%repository-library-root repository)
  `(%%vector-ref ,repository 3))

(jazz:define-macro (%%repository-library-directory repository)
  `(%%vector-ref ,repository 4))

(jazz:define-macro (%%repository-binary? repository)
  `(%%vector-ref ,repository 5))

(jazz:define-macro (%%repository-packages-table repository)
  `(%%vector-ref ,repository 6))

(jazz:define-macro (%%repository-packages-table-set! repository packages-table)
  `(%%vector-set! ,repository 6 ,packages-table))


;;;
;;;; Package
;;;


;; A package is the deployment unit that groups together related resources. Packages are
;; discovered automatically and their order within their repository should not be relevant.


(jazz:define-macro (%%make-package repository name parent library-root library-path units-root units-path install char-encoding products profiles project)
  `(%%vector 'package ,repository ,name ,parent ,library-root ,library-path ,units-root ,units-path ,install ,char-encoding ,products ,profiles ,project (%%make-table test: eq?)))


(jazz:define-macro (%%package-repository package)
  `(%%vector-ref ,package 1))

(jazz:define-macro (%%package-name package)
  `(%%vector-ref ,package 2))

(jazz:define-macro (%%package-parent package)
  `(%%vector-ref ,package 3))

(jazz:define-macro (%%package-library-root package)
  `(%%vector-ref ,package 4))

(jazz:define-macro (%%package-library-path package)
  `(%%vector-ref ,package 5))

(jazz:define-macro (%%package-units-root package)
  `(%%vector-ref ,package 6))

(jazz:define-macro (%%package-units-path package)
  `(%%vector-ref ,package 7))

(jazz:define-macro (%%package-install package)
  `(%%vector-ref ,package 8))

(jazz:define-macro (%%package-char-encoding package)
  `(%%vector-ref ,package 9))

(jazz:define-macro (%%package-products package)
  `(%%vector-ref ,package 10))

(jazz:define-macro (%%package-profiles package)
  `(%%vector-ref ,package 11))

(jazz:define-macro (%%package-profiles-set! package profiles)
  `(%%vector-set! ,package 11 profiles))

(jazz:define-macro (%%package-project package)
  `(%%vector-ref ,package 12))


;;;
;;;; Product
;;;


;; A product is the runtime implementation of some user level entity that can be run and updated / built.


(jazz:define-macro (%%make-product name title icon run test update build build-library package descriptor)
  `(%%vector 'product ,name ,title ,icon ,run ,test ,update ,build ,build-library ,package ,descriptor))

(jazz:define-macro (%%product-name product)
  `(%%vector-ref ,product 1))

(jazz:define-macro (%%product-title product)
  `(%%vector-ref ,product 2))

(jazz:define-macro (%%product-icon product)
  `(%%vector-ref ,product 3))

(jazz:define-macro (%%product-run product)
  `(%%vector-ref ,product 4))

(jazz:define-macro (%%product-test product)
  `(%%vector-ref ,product 5))

(jazz:define-macro (%%product-update product)
  `(%%vector-ref ,product 6))

(jazz:define-macro (%%product-build product)
  `(%%vector-ref ,product 7))

(jazz:define-macro (%%product-build-library product)
  `(%%vector-ref ,product 8))

(jazz:define-macro (%%product-package product)
  `(%%vector-ref ,product 9))
 
(jazz:define-macro (%%product-descriptor product)
  `(%%vector-ref ,product 10))

 
;;;
;;;; Resource
;;;


;; A resource is a triplet (package path . extension) representing a resource inside
;; a package. Compilation will use the path part to put the binary outputs under the
;; build subdir of the architecture directory to enable a cross-compilation scheme.


(jazz:define-macro (%%make-resource package path extension)
  `(%%vector 'resource ,package ,path ,extension))


(jazz:define-macro (%%resource-package resource)
  `(%%vector-ref ,resource 1))

(jazz:define-macro (%%resource-path resource)
  `(%%vector-ref ,resource 2))

(jazz:define-macro (%%resource-extension resource)
  `(%%vector-ref ,resource 3))


;;;
;;;; Image unit
;;;

  
(jazz:define-macro (%%make-image-unit load-proc compile-time-hash)
  `(%%vector 'image-unit ,load-proc ,compile-time-hash))

(jazz:define-macro (%%image-unit-load-proc image-unit)
  `(%%vector-ref ,image-unit 1))

(jazz:define-macro (%%image-unit-compile-time-hash image-unit)
  `(%%vector-ref ,image-unit 2))


;;;
;;;; Digest
;;;


(jazz:define-macro (%%make-digest pathname hash time)
  `(%%vector 'digest ,pathname ,hash ,time))


(jazz:define-macro (%%digest-pathname digest)
  `(%%vector-ref ,digest 1))

(jazz:define-macro (%%digest-pathname-set! digest pathname)
  `(%%vector-set! ,digest 1 ,pathname))

(jazz:define-macro (%%digest-hash digest)
  `(%%vector-ref ,digest 2))

(jazz:define-macro (%%digest-hash-set! digest hash)
  `(%%vector-set! ,digest 2 ,hash))

(jazz:define-macro (%%digest-time digest)
  `(%%vector-ref ,digest 3))

(jazz:define-macro (%%digest-time-set! digest time)
  `(%%vector-set! ,digest 3 ,time))


;;;
;;;; Manifest
;;;


(jazz:define-macro (%%make-manifest name version compile-time-hash source-digests references)
  `(%%vector 'manifest ,name ,version ,compile-time-hash ,source-digests ,references))


(jazz:define-macro (%%manifest-name manifest)
  `(%%vector-ref ,manifest 1))

(jazz:define-macro (%%manifest-version manifest)
  `(%%vector-ref ,manifest 2))

(jazz:define-macro (%%manifest-version-set! manifest version)
  `(%%vector-set! ,manifest 2 ,version))

(jazz:define-macro (%%manifest-compile-time-hash manifest)
  `(%%vector-ref ,manifest 3))

(jazz:define-macro (%%manifest-compile-time-hash-set! manifest compile-time-hash)
  `(%%vector-set! ,manifest 3 ,compile-time-hash))

(jazz:define-macro (%%manifest-source-digests manifest)
  `(%%vector-ref ,manifest 4))

(jazz:define-macro (%%manifest-source-digests-set! manifest source-digests)
  `(%%vector-set! ,manifest 4 ,source-digests))

(jazz:define-macro (%%manifest-references manifest)
  `(%%vector-ref ,manifest 5))

(jazz:define-macro (%%manifest-references-set! manifest references)
  `(%%vector-set! ,manifest 5 ,references))


;;;
;;;; Unit
;;;


(jazz:define-syntax unit
  (lambda (form-src)
    (jazz:expand-unit-source (%%cdr (jazz:source-code form-src)))))


(jazz:define-syntax require
  (lambda (form-src)
    (jazz:expand-require (%%cdr (jazz:source-code form-src))))))
