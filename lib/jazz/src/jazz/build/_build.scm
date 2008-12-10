;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Build
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


(module jazz.build


(require (core.base))


;;;
;;;; Expand
;;;


(define (expand library-name)
  (expand-to-port library-name (current-output-port)))


(define (expand-source library-name)
  (let* ((src (jazz.find-module-src library-name #f))
         (source (jazz.resource-pathname src))
         (syntax (jazz.read-toplevel-form source parse-read?: #f read-source?: #t)))
    (pretty-print
      (jazz.present-source
        syntax))))


(define (expand-syntax library-name)
  (let* ((src (jazz.find-module-src library-name #f))
         (source (jazz.resource-pathname src))
         (syntax (jazz.read-toplevel-form source parse-read?: #f read-source?: #t)))
    (pretty-print
      (jazz.present-source
        (parameterize ((jazz.requested-module-name library-name)
                       (jazz.requested-module-resource src))
          (case 'library ;; kind
            ;; todo ((module) (jazz.expand-module (car rest) (cdr rest)))
            ((library) (jazz.expand-library (%%cdr (jazz.source-code syntax))))))))))


(define (expand-to-file library-name . rest)
  (let ((filename (if (null? rest) "x.scm" (car rest))))
    (call-with-output-file filename
      (lambda (port)
        (expand-to-port library-name port)))))


(define (expand-to-port library-name port)
  (let* ((src (jazz.find-module-src library-name #f))
         (source (jazz.resource-pathname src))
         (form (jazz.read-toplevel-form source parse-read?: #f))
         (kind (car form))
         (rest (cdr form)))
    (pretty-print
      (parameterize ((jazz.requested-module-name library-name)
                     (jazz.requested-module-resource src))
        (case kind
          ((module) (jazz.expand-module (car rest) (cdr rest)))
          ((library) (jazz.expand-library rest))))
      port))))
