;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Exception Handling
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
;;;    Marc Feeley
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


(module jazz.dialect.core.exception


;;;
;;;; Default
;;;


(define jazz.default-exception-handler
  (jazz.current-exception-handler))


(define (jazz.with-default-exception-handler thunk)
  (jazz.with-exception-handler
    jazz.default-exception-handler
    thunk))


;;;
;;;; Propagatable
;;;


(define (jazz.with-propagatable-exception-catcher handler thunk)
  ;; Calls "thunk" and returns whatever "thunk" returns, unless
  ;; it raises an exception. In that case the handler is called
  ;; with 3 arguments:
  ;; 1 - the exception that was raised
  ;; 2 - the propagate procedure
  ;; 3 - the debug procedure
  (##continuation-capture
    (lambda (catcher-cont)
      (with-exception-handler
        (lambda (exc)
          (##continuation-capture
            (lambda (raise-cont)
              (##continuation-graft
                catcher-cont
                (lambda ()
                  (handler exc
                           (lambda ()
                             (let ((eh (current-exception-handler)))
                               (##continuation-graft
                                 raise-cont
                                 (lambda () (eh exc)))))
                           (lambda ()
                             (##display-exception-in-context
                               exc
                               raise-cont
                               (repl-output-port))
                             (##continuation-graft
                               raise-cont
                               ##repl))))))))
        thunk)))))
