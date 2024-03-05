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
;;;  Portions created by the Initial Developer are Copyright (C) 1996-2018
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


(unit protected core.exception.runtime


;;;
;;;; Exception
;;;


(jazz:define-class jazz:Exception jazz:Object ()
  ())


(jazz:define-method (jazz:print-object (jazz:Exception exception) output detail)
  (let ((message (jazz:exception-message exception)))
    (jazz:format output "#<exception #{a}" (jazz:object->serial exception))
    (if message
        (jazz:format output " {s}" message))
    (jazz:format output ">")))


(jazz:define-virtual (jazz:present-exception (jazz:Exception exception)))
(jazz:define-virtual (jazz:exception-message (jazz:Exception exception)))
(jazz:define-virtual (jazz:get-detail (jazz:Exception exception)))


(jazz:define-method (jazz:present-exception (jazz:Exception exception))
  (let ((output (open-output-string)))
    (jazz:format output "This exception was raised: {s}" exception)
    (get-output-string output)))


(jazz:define-method (jazz:exception-message (jazz:Exception exception))
  #f)


(jazz:define-method (jazz:get-detail (jazz:Exception exception))
  #f)


;;;
;;;; Exception Detail
;;;


(jazz:define-class jazz:Exception-Detail-Class jazz:Class ()
  ())


(jazz:define-class jazz:Exception-Detail jazz:Object (metaclass: jazz:Exception-Detail-Class constructor: jazz:allocate-exception-detail)
  ((icon     getter: generate)
   (title    getter: generate)
   (location getter: generate)
   (children getter: generate)))


(define (jazz:new-exception-detail icon title location children)
  (jazz:allocate-exception-detail icon title location children))


;;;
;;;; System Exception
;;;


(jazz:define-class jazz:System-Exception jazz:Exception ()
  ((exception getter: generate)))


(jazz:define-method (jazz:present-exception (jazz:System-Exception exception))
  (jazz:exception-reason (jazz:get-system-exception-exception exception)))


;;;
;;;; Scheme
;;;


(define (jazz:exception-reason exc)
  (let ((output (open-output-string)))
    (jazz:debug-port-setup-width output)
    (display-exception exc output)
    (let ((str (get-output-string output)))
      (let ((len (string-length str)))
        (if (and (%%fx> len 0)
                 (%%eqv? (%%string-ref str (%%fx- len 1)) #\newline))
            (%%substring str 0 (%%fx- len 1))
          str)))))


(define (jazz:exception-detail exc)
  (if (and (%%object? exc)
           (%%is? exc jazz:Exception))
      (jazz:get-detail exc)
    #f))


(define (jazz:exception-locat exc cont)
  (%%exception->locat exc cont))


;;;
;;;; Gambit Hook
;;;


(let ((previous-hook (%%display-exception-hook-ref)))
  (%%display-exception-hook-set!
    (lambda (exc port)
      (if (and (%%object? exc)
               (%%is? exc jazz:Exception))
          (begin
            (display (jazz:present-exception exc) port)
            (newline port))
        (previous-hook exc port)))))


;;;
;;;; Handle
;;;


(define (jazz:handle-exception-filter filter handler thunk)
  (let ((previous-handler (current-exception-handler)))
    (%%continuation-capture
      (lambda (catcher-cont)
        (with-exception-handler
          (lambda (exc)
            (with-exception-handler
              (lambda (handler-exc)
                (%%continuation-graft catcher-cont
                  (lambda ()
                    (previous-handler handler-exc))))
              (lambda ()
                (if (filter exc)
                    (let ((value (handler exc)))
                      (%%continuation-return catcher-cont value))
                  (previous-handler exc)))))
          thunk)))))


;;;
;;;; Catch
;;;


(define (jazz:catch-exception-filter filter catcher thunk)
  (let ((previous-handler (current-exception-handler)))
    (%%continuation-capture
      (lambda (catcher-cont)
        (with-exception-handler
          (lambda (exc)
            (if (with-exception-handler
                  (lambda (filter-exc)
                    (%%continuation-graft catcher-cont
                      (lambda ()
                        (previous-handler filter-exc))))
                  (lambda ()
                    (filter exc)))
                (%%continuation-graft catcher-cont
                  (lambda ()
                    (catcher exc)))
              (previous-handler exc)))
          thunk)))))


;;;
;;;; Error
;;;


(jazz:define-class jazz:Error jazz:Exception (constructor: jazz:allocate-error)
  ((message getter: generate)))


(define (jazz:new-error message)
  (jazz:allocate-error message))


(jazz:define-method (jazz:exception-message (jazz:Error error))
  (jazz:get-error-message error))


(jazz:define-method (jazz:present-exception (jazz:Error error))
  (jazz:get-error-message error))


(define (jazz:raise-jazz-error fmt-string . rest)
  (declare (proper-tail-calls))
  (let ((message (apply jazz:format fmt-string rest)))
    (raise (jazz:new-error message))))


(set! jazz:error jazz:raise-jazz-error))
