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


(module core.base.runtime.exception


(cond-expand
  (chicken
    (define (jazz.current-exception-handler)
      (current-exception-handler))
    
    (define (jazz.with-exception-handler proc thunk)
      (call/cc
        (lambda (return)
          (with-exception-handler
            (lambda (exc)
              (return (proc exc)))
            thunk))))
    
    (define (jazz.exception-reason exc)
      (let ((location  ((condition-property-accessor 'exc 'location) exc))
            (message   ((condition-property-accessor 'exc 'message) exc))
            (arguments ((condition-property-accessor 'exc 'arguments) exc)))
        (if (not location)
            (jazz.format "{a}: {l}" message arguments)
          (jazz.format "({a}) {a}: {l}" location message arguments)))))
  
  (gambit
    (define (jazz.current-exception-handler)
      (current-exception-handler))
    
    (define (jazz.with-exception-handler proc thunk)
      (with-exception-handler
        proc
        thunk))
    
    (define (jazz.with-exception-catcher proc thunk)
      (with-exception-catcher
        proc
        thunk))
    
    (define (jazz.exception-reason exc)
      (let ((output (open-output-string)))
        (display (##exception->kind exc) output)
        (display " -- " output)
        (##display-exception exc output)
        (get-output-string output)))
    
    (define (jazz.raise obj)
      (raise obj)))
  
  (else)))
