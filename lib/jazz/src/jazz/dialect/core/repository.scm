;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Repository
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


(unit protected jazz.dialect.core.repository


;;;
;;;; Repository
;;;


(define (jazz:repository-name repository)
  (%%repository-name repository))

(define (jazz:repository-title repository)
  (%%symbol->string (%%repository-name repository)))

(define (jazz:repository-directory repository)
  (%%repository-directory repository))

(define (jazz:repository-library-root repository)
  (%%repository-library-root repository))

(define (jazz:repository-library-directory repository)
  (%%repository-library-directory repository))

(define (jazz:repository-binary? repository)
  (%%repository-binary? repository))


;;;
;;;; Package
;;;


(define (jazz:package-repository package)
  (%%package-repository package))

(define (jazz:package-name package)
  (%%package-name package))

(define (jazz:package-directory package)
  (jazz:package-root-pathname package ""))

(define (jazz:package-products package)
  (%%package-products package))

(define (jazz:package-profiles package)
  (%%package-profiles package))

(define (jazz:package-profiles-set! package profiles)
  (%%package-profiles-set! package profiles))

(define (jazz:package-project package)
  (%%package-project package)))
