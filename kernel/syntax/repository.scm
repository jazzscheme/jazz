;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Kernel Repository
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


(block kernel.repository


;;;
;;;; Repository
;;;


;; A repository is where packages are located. The system comes with the following already
;; present repositories: build and jazz: Note that repository order is important as it
;; defines search precedence.


(jazz:define-structure Repository () (constructor: %%make-repository predicate: jazz:repository? accessors-type: macro)
  ((name              getter: generate setter: generate)
   (directory         getter: generate)
   (library-root      getter: generate)
   (library-directory getter: generate)
   (binary?           getter: generate)
   (packages-table    getter: generate setter: generate)
   (dependencies      getter: generate)))


;;;
;;;; Package
;;;


;; A package is the deployment unit that groups together related resources. Packages are
;; discovered automatically and their order within their repository should not be relevant.


(jazz:define-structure Package () (constructor: %%make-package predicate: jazz:package? accessors-type: macro)
  ((repository    getter: generate)
   (name          getter: generate)
   (parent        getter: generate)
   (library-root  getter: generate)
   (library-path  getter: generate)
   (units-root    getter: generate)
   (units-path    getter: generate)
   (install       getter: generate)
   (char-encoding getter: generate)
   (products      getter: generate)
   (profiles      getter: generate setter: generate)
   (project       getter: generate)
   (title         getter: generate)
   (description   getter: generate)
   (authors       getter: generate)
   (stage         getter: generate)))


;;;
;;;; Product
;;;


;; A product is the runtime implementation of some user level entity that can be run, updated, built and installed.


(jazz:define-structure Product () (constructor: %%make-product accessors-type: macro)
  ((name            getter: generate)
   (title           getter: generate)
   (icon            getter: generate)
   (run             getter: generate)
   (test            getter: generate)
   (update          getter: generate)
   (build           getter: generate)
   (options         getter: generate)
   (library-options getter: generate)
   (install         getter: generate)
   (deploy          getter: generate)
   (package         getter: generate)
   (descriptor      getter: generate)))

 
;;;
;;;; Resource
;;;


;; A resource is a triplet (package path . extension) representing a resource inside
;; a package. Compilation will use the path part to put the binary outputs under the
;; build subdir of the architecture directory to enable a cross-compilation scheme.


(jazz:define-structure Resource () (constructor: %%make-resource accessors-type: macro)
  ((package     getter: generate)
   (path        getter: generate)
   (underscore? getter: generate)
   (extension   getter: generate)))


;;;
;;;; Image-Unit
;;;


(jazz:define-structure Image-Unit () (constructor: %%make-image-unit accessors-type: macro)
  ((load-proc         getter: generate)
   (compile-time-hash getter: generate)))


;;;
;;;; Digest
;;;


(jazz:define-structure Digest () (constructor: %%make-digest accessors-type: macro)
  ((pathname getter: generate setter: generate)
   (hash     getter: generate setter: generate)
   (seconds  getter: generate setter: generate)))


;;;
;;;; Manifest
;;;


(jazz:define-structure Manifest () (constructor: %%make-manifest accessors-type: macro)
  ((name              getter: generate)
   (version           getter: generate setter: generate)
   (compile-time-hash getter: generate setter: generate)
   (source-digests    getter: generate setter: generate)
   (references        getter: generate setter: generate)))


;;;
;;;; Unit
;;;


(jazz:define-syntax unit
  (lambda (form-src)
    (jazz:expand-unit-source (%%cdr (jazz:source-code form-src)))))


(jazz:define-syntax require
  (lambda (form-src)
    (jazz:expand-require (%%cdr (jazz:source-code form-src))))))
