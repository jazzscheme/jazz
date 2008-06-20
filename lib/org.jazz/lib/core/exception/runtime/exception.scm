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


(jazz.define-class-runtime jazz.Exception)


(jazz.define-virtual-runtime (jazz.present-exception (jazz.Exception exception)))
(jazz.define-virtual-runtime (jazz.get-details (jazz.Exception exception)))


(jazz.define-method (jazz.present-exception (jazz.Exception exception))
  #f)

(jazz.define-method (jazz.get-details (jazz.Exception exception))
  #f)


(jazz.encapsulate-class jazz.Exception)


(define (jazz.exception-reason exc)
  (let ((output (open-output-string)))
    (jazz.display-exception exc output)
    (get-output-string output)))


(define (jazz.exception-details exc)
  (if (and (%%object? exc)
           (%%is? exc jazz.Exception))
      (jazz.get-details exc)
    #f)))
