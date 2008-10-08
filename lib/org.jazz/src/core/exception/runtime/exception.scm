;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Exceptions
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


(module core.exception.runtime.exception


;;;
;;;; Exception
;;;


(jazz.define-class-runtime jazz.Exception)


(jazz.define-method (jazz.print-object (jazz.Exception exception) output detail)
  (let ((message (jazz.get-message exception)))
    (jazz.format output "#<exception #{a}" (jazz.object->serial exception))
    (if message
        (jazz.format output " {s}" message))
    (jazz.format output ">")))


(jazz.define-virtual-runtime (jazz.present-exception (jazz.Exception exception)))
(jazz.define-virtual-runtime (jazz.get-message (jazz.Exception exception)))
(jazz.define-virtual-runtime (jazz.get-detail (jazz.Exception exception)))


(jazz.define-method (jazz.present-exception (jazz.Exception exception))
  (let ((output (open-output-string)))
    (jazz.format output "This exception was raised: {s}" exception)
    (get-output-string output)))

(jazz.define-method (jazz.get-message (jazz.Exception exception))
  #f)

(jazz.define-method (jazz.get-detail (jazz.Exception exception))
  #f)


(jazz.encapsulate-class jazz.Exception)


;;;
;;;; Exception Detail
;;;


(jazz.define-class-runtime jazz.Exception-Detail)


(define (jazz.new-exception-detail icon title location children)
  (jazz.allocate-exception-detail jazz.Exception-Detail icon title location children))


(jazz.encapsulate-class jazz.Exception-Detail)


;;;
;;;; Scheme
;;;


(define (jazz.exception-reason exc)
  (let ((output (open-output-string)))
    (jazz.display-exception exc output)
    (get-output-string output)))


(define (jazz.exception-detail exc)
  (if (and (%%object? exc)
           (%%is? exc jazz.Exception))
      (jazz.get-detail exc)
    #f))


;;;
;;;; Gambit Hook
;;;


(let ((previous-hook ##display-exception-hook))
  (set! ##display-exception-hook
        (lambda (exc port)
          (if (and (%%object? exc)
                   (%%is? exc jazz.Exception))
              (begin
                (display (jazz.present-exception exc) port)
                (newline port))
            (previous-hook exc port))))))
