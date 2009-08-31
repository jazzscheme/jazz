;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Development
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
;;;    Stephane Le Cornec
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


(module core.library.development


(require (core.base))


;;;
;;;; Parse
;;;


(define (jazz.parse module-name)
  (let ((source (jazz.resource-pathname (jazz.find-module-src module-name #f))))
    (let ((form (jazz.read-toplevel-form source)))
      (pretty-print form))))


(define (jazz.parse-source module-name)
  (parameterize ((jazz.walk-for 'walk))
    (let* ((src (jazz.find-module-src module-name #f))
           (source (jazz.resource-pathname src))
           (syntax (jazz.read-toplevel-form source read-source?: #t)))
      (pretty-print
        (jazz.present-source
          syntax)))))


;;;
;;;; Expand
;;;


(define (jazz.expand-module module-name #!key (walk-for #f))
  (parameterize ((jazz.walk-for (or walk-for 'walk)))
    (let* ((src (jazz.find-module-src module-name #f))
           (source (jazz.resource-pathname src))
           (form (jazz.read-toplevel-form source read-source?: #t))
           (kind (jazz.source-code (car (jazz.source-code form))))
           (rest (cdr (jazz.source-code form))))
      (parameterize ((jazz.requested-module-name module-name)
                     (jazz.requested-module-resource src))
        (case kind
          ((module) (jazz.expand-module-source rest))
          ((library) (jazz.expand-library-source rest)))))))


(define (jazz.expand module-name . rest)
  (apply jazz.expand-to-port module-name (current-output-port) rest))


(define (jazz.expand-to-file module-name #!key (file #f) #!rest rest)
  (call-with-output-file (or file "x.scm")
    (lambda (port)
      (apply jazz.expand-to-port module-name port rest))))


(define (jazz.expand-to-port module-name port . rest)
  (pretty-print
    (jazz.desourcify-all (apply jazz.expand-module module-name rest))
    port))


(define (jazz.expand-source module-name . rest)
  (pretty-print
    (jazz.present-source
      (apply jazz.expand-module module-name rest))))


;;;
;;;; Lookup
;;;


(define (jazz.lookup library-name name access)
  (jazz.lookup-declaration (jazz.walk-module library-name) name access #f)))
