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
;;;  Portions created by the Initial Developer are Copyright (C) 1996-2006
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


;;;
;;;; Compile
;;;


;; Generates an intermediate jscm expansion file. This is usefull for debugging until
;; we implement the library macro as a source transformation like for module. This will
;; probably be a very complex task
(define (jazz.compile-jazz-module module-name)
  (let ((src (jazz.find-module-src module-name)))
    (jazz.with-src/bin src
      (lambda (src bin bin-uptodate?)
        (if (or (not bin) (not bin-uptodate?))
            (let ((build-package (jazz.copy-package (%%resource-package src))))
              (let ((jscm (%%make-resource build-package (%%resource-path src) "jscm")))
                (jazz.create-directories (jazz.resource-build-dir jscm))
                (expand-to-file module-name (jazz.resource-pathname jscm))
                (parameterize ((current-readtable jazz.jazz-readtable))
                  (jazz.compile-source jscm module-name digest: (jazz.resource-digest src))))))))))


;;;
;;;; Expand
;;;


(define (expand library-name)
  (expand-to-port library-name (current-output-port)))


(define (expand-to-file library-name . rest)
  (let ((filename (if (null? rest) "x.scm" (car rest))))
    (call-with-output-file filename
      (lambda (port)
        (expand-to-port library-name port)))))


(define (expand-to-port library-name port)
  (let ((source (jazz.resource-pathname (jazz.find-module-src library-name))))
    (let ((form (jazz.read-toplevel-form source #f)))
      (let ((kind (car form))
            (rest (cdr form)))
        (pretty-print (parameterize ((jazz.requested-module-name library-name))
                        (case kind
                          ((module) (jazz.expand-module (car rest) (cdr rest)))
                          ((library) (jazz.expand-library rest))))
                      port))))))
