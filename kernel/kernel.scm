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


;; core - core debug mode with tests to make the core safe
;; debug - standard debug mode with tests to make user code safe
;; release - release mode without tests for stable user code


(cond-expand
  (core
    (define jazz.debug-core?
      #t)
    (define jazz.debug-user?
      #t))
  (debug
    (define jazz.debug-core?
      #f)
    (define jazz.debug-user?
      #t))
  (release
    (define jazz.debug-core?
      #f)
    (define jazz.debug-user?
      #f)))


;;;
;;;; Declare
;;;


(cond-expand
  (gambit
    (define (jazz.declares kind)
      `((declare (block)
                 (standard-bindings)
                 (extended-bindings)
                 ,@(if jazz.debug-user?
                       '()
                     `((not safe)))))))
  
  (else))


;;;
;;;; Sources
;;;


(define jazz.Kernel-Sources
  (list
    "kernel/syntax/primitives"
    "kernel/syntax/syntax"
    "kernel/syntax/module"
    "kernel/syntax/module-expander"
    "kernel/runtime/digest"
    "kernel/runtime/runtime"))


(define (jazz.load-kernel-sources)
  (for-each (lambda (path)
              (jazz.load (string-append "../../" path)))
            jazz.Kernel-Sources))


;;;
;;;; Kernel
;;;


(define jazz.boot-kernel
  (let ((loaded? #f))
    (lambda ()
      (if (not loaded?)
          (begin
            (jazz.load-kernel)
            (set! loaded? #t))))))


(define (jazz.load-kernel)
  (jazz.load-kernel-sources)
  (jazz.register-reader-extensions 'jazz.dialect (lambda () jazz.jazz-readtable) '("jazz")))


;; todo
(define (jazz.build-kernel)
  #f)
