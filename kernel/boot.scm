;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Jazz Kernel
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


;;;
;;;; Safety
;;;


;; safe - scheme kernel debug mode with tests to make the kernel safe
;; debug - standard debug mode with tests to make jazz user code safe
;; release - release mode without tests for user stable code
(define jazz.safety-level
  (make-parameter 'debug))


(define (jazz.safe?)
  (memq (jazz.safety-level) '(safe)))


(define (jazz.debug?)
  (memq (jazz.safety-level) '(safe debug)))


;;;
;;;; Processor
;;;


(define jazz.Processor
  'intel)


;;;
;;;; Build
;;;


(define jazz.Use-Bin-Directory?
  #t)


(define jazz.Use-Build-Suffix?
  #f)


(define jazz.build-suffix
  (string-append (cond-expand
                   (blues "b")
                   (chicken "c")
                   (gambit "g")
                   (else "u"))
                 (case jazz.Processor
                   ((intel) "i"))
                 (case (jazz.safety-level)
                   ((safe) "s")
                   ((debug) "d")
                   ((release) "r"))))


;;;
;;;; Jazz
;;;


(define jazz.parse-verbose?
  #f)

(define jazz.done-verbose?
  #f)


(define jazz.Use-Print?
  #t)


(define jazz.profile?
  (make-parameter #f))

(define (profile thunk)
  (parameterize ((jazz.profile? #t))
    (thunk)))


(define jazz.compile-options
  '(debug))


(cond-expand
  (gambit
    (define (jazz.declarations)
      `((declare (block)
                 (standard-bindings)
                 (extended-bindings)
                 ,@(if (jazz.safe?)
                       '()
                     `((not safe)))))))
  (else
    '()))


;;;
;;;; Load
;;;


(define jazz.load-verbose?
  (make-parameter #t))


(define (jazz.load-filename filename)
  (##load filename (lambda rest #f) #f #t))


;;;
;;;; Module
;;;


(define (jazz.load-module-system)
  (jazz.load-filename "kernel/module/primitives")
  (jazz.load-filename "kernel/module/syntax")
  (jazz.load-filename "kernel/module/runtime"))


;;;
;;;; Kernel
;;;


(define (jazz.load-kernel)
  (jazz.load-module-system)
  (jazz.register-reader-extensions 'jazz.dialect (lambda () jazz.jazz-readtable) '("jazz" "fusion")))
