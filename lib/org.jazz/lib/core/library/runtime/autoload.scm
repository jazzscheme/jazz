;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Autoload Support
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


;; The global autoload table is a temporary solution to Jazz needing global access
;; to classes by name for forms. This problem has to be well thought out...


(module core.library.runtime.autoload


(define jazz.Autoloads
  (%%make-table test: eq?))


(define (jazz.get-autoloads)
  jazz.Autoloads)


(define (jazz.get-autoload name)
  (%%table-ref jazz.Autoloads name #f))


(define (jazz.set-autoload name module-name loader)
  (%%table-set! jazz.Autoloads name (%%cons module-name loader)))


(define (jazz.require-autoload name)
  (or (jazz.get-autoload name)
      (jazz.error "Unable to find autoload {s}" name)))


(define (jazz.register-autoload name module-name loader)
  (let ((actual (jazz.get-autoload name)))
    (if (or (%%not actual) (%%eq? (%%car actual) module-name))
        (jazz.set-autoload name module-name loader)
      (jazz.error "Conflict detected for autoload {s} between {s} and {s}" name (%%car actual) module-name)))
  (let ((package (%%resource-package (jazz.requested-module-resource))))
    (jazz.register-package-autoload package name module-name loader)))


(define (jazz.autoload name)
  (let ((autoload-info (jazz.require-autoload name)))
    ((%%cdr autoload-info))))


(define (jazz.autoreload name)
  (let ((autoload-info (jazz.require-autoload name)))
    (jazz.reload-module (%%car autoload-info))
    ((%%cdr autoload-info)))))
