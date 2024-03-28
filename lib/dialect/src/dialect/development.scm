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
;;;  Portions created by the Initial Developer are Copyright (C) 1996-2018
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


(unit dialect.development


(require (core.base))


;;;
;;;; Parse
;;;


(define (jazz:parse unit-name)
  (let ((src (jazz:find-unit-src unit-name)))
    (let ((form-src (jazz:read-toplevel-form src)))
      (pretty-print
        (jazz:desourcify-all form-src)))))


(define (jazz:parse-source unit-name)
  (parameterize ((jazz:walk-for 'interpret))
    (let* ((src (jazz:find-unit-src unit-name))
           (form-src (jazz:read-toplevel-form src)))
      (pretty-print
        (jazz:present-source form-src)))))


;;;
;;;; Shape
;;;


(define (jazz:shape-unit unit-name)
  (jazz:load-unit 'jazz.dialect)
  (let ((shape (jazz:module-shape #f #f #f unit-name)))
    (pp shape)))


;;;
;;;; Walk
;;;


(define (jazz:walk-describe unit-name)
  (jazz:load-unit 'jazz.backend.describe)
  (let ((port (current-output-port)))
    (jazz:output-port-width-set! port 160)
    (jazz:expand-to-port unit-name port backend: 'describe)))


;;;
;;;; Expand
;;;


(define (jazz:expanding-unit unit-name thunk #!key (walk-for #f))
  (let ((src (jazz:find-unit-src unit-name)))
    (parameterize ((jazz:walk-for (or walk-for 'walk))
                   (jazz:generate-symbol-for "%")
                   (jazz:generate-symbol-context unit-name)
                   (jazz:requested-unit-name unit-name)
                   (jazz:requested-unit-resource src)
                   (jazz:requested-pathname #f))
      (thunk))))


(define (jazz:expand-unit unit-name #!key (backend 'scheme) (walk-for #f))
  (jazz:expanding-unit unit-name
    (lambda ()
      (let ((form (jazz:read-toplevel-form (jazz:requested-unit-resource))))
        (jazz:expand-form form unit-name: unit-name backend: backend walk-for: walk-for)))
     walk-for: walk-for))


(define (jazz:expand-form form #!key (unit-name #f) (backend 'scheme) (walk-for #f))
  (parameterize ((jazz:walk-for (or walk-for 'walk)))
    (let ((kind (jazz:source-code (car (jazz:source-code form))))
          (rest (cdr (jazz:source-code form))))
      (parameterize ((jazz:generate-symbol-for "%")
                     (jazz:generate-symbol-context (or unit-name (gensym))))
        (case kind
          ((unit) (jazz:expand-unit-source rest))
          ((module) (jazz:generate-module rest backend))
          ((script) (jazz:generate-script rest backend))
          (else (jazz:error "Ill-formed toplevel form: {s}" kind)))))))


(define (jazz:expand-to unit-name . rest)
  (let ((port (current-output-port)))
    (jazz:output-port-width-set! port 160)
    (apply jazz:expand-to-port unit-name port rest)))


(define (jazz:expand-to-port unit-name port . rest)
  (pretty-print
    (jazz:desourcify-all (apply jazz:expand-unit unit-name rest))
    port))


(define (jazz:expand-to-file unit-name #!key (file #f) #!rest rest)
  (parameterize ((current-readtable jazz:scheme-readtable))
    (call-with-output-file (or file "x.scm")
      (lambda (port)
        (apply jazz:expand-to-port unit-name port rest)))))


(define (jazz:expand-source unit-name . rest)
  (pretty-print
    (jazz:present-source
      (apply jazz:expand-unit unit-name rest))))


(define (jazz:expanding-script path thunk)
  (parameterize ((jazz:walk-for 'walk)
                 (jazz:generate-symbol-for "%")
                 (jazz:generate-symbol-context (gensym))
                 (jazz:requested-unit-name #f)
                 (jazz:requested-unit-resource #f)
                 (jazz:requested-pathname path))
    (thunk)))


(define (jazz:expand-script path #!key (backend 'scheme))
  (jazz:expanding-script path
    (lambda ()
      (let ((form (jazz:read-toplevel-form path script?: #t)))
        (let ((rest (cdr (jazz:source-code form))))
          (jazz:generate-script rest backend))))))


;;;
;;;; Emit
;;;


(define (jazz:emit-unit unit-name)
  (jazz:expand-unit unit-name))


;;;
;;;; Check
;;;


(define (jazz:check-unit unit-name)
  (jazz:warnings? #t)
  (jazz:expand-unit unit-name))


;;;
;;;; Verify
;;;


(define (jazz:verify-unit unit-name)
  (jazz:iterate-module-declaration (jazz:walk-unit unit-name)
    pp))


;;;
;;;; Lookup
;;;


(define (jazz:lookup module-name name access)
  (jazz:lookup-declaration (jazz:walk-unit module-name) name access #f)))
