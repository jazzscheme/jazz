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


(module core.jazz.runtime.exception


(define jazz.default-exception-handler
  (jazz.current-exception-handler))


(define (jazz.with-default-exception-handler thunk)
  (jazz.with-exception-handler
    jazz.default-exception-handler
    thunk))


(define (jazz.with-safe-exception-handler thunk)
  (call/cc
    (lambda (recursive-error)
      (jazz.with-exception-handler
        (lambda (exc)
          (recursive-error (jazz.format "Recursive error: {a}" (jazz.exception-reason exc))))
        thunk))))


(define (jazz.with-jazz-exception-handler thunk)
  (jazz.with-exception-handler
    (lambda (exc)
      (jazz.continuation-capture
        (lambda (continuation)
          (let ((reason (jazz.exception-reason exc)))
            (jazz.invoke-debugger 'jazz.debugger-error 'error reason continuation)))))
    thunk))


(define (jazz.break #!optional (reason #f))
  (jazz.continuation-capture
    (lambda (continuation)
      (jazz.invoke-debugger 'jazz.debugger-break 'break reason continuation))))
  

(define (jazz.invoke-debugger message kind reason continuation)
  (jazz.with-default-exception-handler
    (lambda ()
      (let* ((process (jazz.system.process.get-process))
             (debugger (and process (jazz.system.process.Process.get-debugger process))))
        (if (not debugger)
            (jazz.raise (list kind reason))
          (jazz.debug.invoke-debugger kind reason continuation)))))))
