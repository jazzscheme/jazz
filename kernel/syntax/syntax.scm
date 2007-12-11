;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Module Syntax
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


;;;
;;;; Repository
;;;


;; A repository is where packages are located. For now there are only a fixed hardcoded
;; number of repositories. Note that repository order is important as it defines search
;; precedence.


(define-macro (%%make-repository name directory binary?)
  `(%%vector 'repository ,name ,directory ,binary? #f))


(define-macro (%%repository-name repository)
  `(%%vector-ref ,repository 1))

(define-macro (%%repository-directory repository)
  `(%%vector-ref ,repository 2))

(define-macro (%%repository-binary? repository)
  `(%%vector-ref ,repository 3))

(define-macro (%%repository-packages-table repository)
  `(%%vector-ref ,repository 4))

(define-macro (%%repository-packages-table-set! repository packages-table)
  `(%%vector-set! ,repository 4 ,packages-table))


;;;
;;;; Package
;;;


;; A package is the deployment unit that groups together related resources. It is also
;; uniformly used to group development resources such as source files. Packages are
;; discovered automatically and their order within their repository should not be relevant.


(define-macro (%%make-package repository name root path)
  `(%%vector 'package ,repository ,name ,root ,path))


(define-macro (%%package-repository package)
  `(%%vector-ref ,package 1))

(define-macro (%%package-name package)
  `(%%vector-ref ,package 2))

(define-macro (%%package-root package)
  `(%%vector-ref ,package 3))

(define-macro (%%package-path package)
  `(%%vector-ref ,package 4))


;;;
;;;; Resource
;;;


;; A resource is a triplet (package path . extension) representing a resource location
;; inside a package. Compilation will use the path part to put the binary outputs under
;; the _build subdir of the architecture directory to enable a cross-compilation scheme.


(define-macro (%%make-resource package path extension)
  `(%%vector 'resource ,package ,path ,extension))


(define-macro (%%resource-package resource)
  `(%%vector-ref ,resource 1))

(define-macro (%%resource-path resource)
  `(%%vector-ref ,resource 2))

(define-macro (%%resource-extension resource)
  `(%%vector-ref ,resource 3))


;;;
;;;; Digest
;;;


(define-macro (%%make-digest hash time identical?)
  `(%%vector 'digest ,hash ,time ,identical?))


(define-macro (%%digest-hash digest)
  `(%%vector-ref ,digest 1))

(define-macro (%%digest-cached-time digest)
  `(%%vector-ref ,digest 2))

(define-macro (%%digest-cached-time-set! digest time)
  `(%%vector-set! ,digest 2 ,time))

(define-macro (%%digest-cached-identical? digest)
  `(%%vector-ref ,digest 3))

(define-macro (%%digest-cached-identical?-set! digest identical?)
  `(%%vector-set! ,digest 3 ,identical?))


;;;
;;;; Manifest
;;;


(define-macro (%%make-manifest name digest)
  `(%%vector 'manifest ,name ,digest))


(define-macro (%%manifest-name manifest)
  `(%%vector-ref ,manifest 1))

(define-macro (%%manifest-digest manifest)
  `(%%vector-ref ,manifest 2))
