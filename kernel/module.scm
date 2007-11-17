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
                 ,@(if ;; a first approximation on having different declarations for kernel modules
                       ;; of course this solution would treat any user module like a kernel one...
                       (case kind
                         ((module) jazz.debug-core?)
                         ((library) jazz.debug-user?))
                       '()
                     `((not safe)))))))
  
  (else))


;;;
;;;; Load
;;;


(define (jazz.load-filename filename)
  (##load filename (lambda rest #f) #f #t))


(define jazz.load-indent
  (make-parameter 0))


(define (jazz.with-verbose flag action filename proc)
  (define (verbose-load)
    (display (make-string (jazz.load-indent) #\space))
    (display "; ")
    (display action)
    (display " ")
    (display filename)
    (display " ...")
    (newline))
  
  (define (verbose-done)
    (display (make-string (jazz.load-indent) #\space))
    (display "; done ")
    (display " ...")
    (newline))
  
  (if flag
      (begin
        (verbose-load)
        (let ((result
                (parameterize ((jazz.load-indent (+ (jazz.load-indent) 2)))
                  (proc))))
          (if jazz.done-verbose?
              (verbose-done))
          result))
    (proc)))


;;;
;;;; Module
;;;


(define (jazz.load-module-system)
  (define (kernel-load filename)
    (jazz.with-verbose jazz.load-verbose? "loading" filename
      (lambda ()
        (jazz.load-filename filename))))
  
  (kernel-load "../../kernel/module/primitives")
  (kernel-load "../../kernel/module/syntax")
  (kernel-load "../../kernel/module/runtime")
  
  ;; for now this is the best solution I found to guaranty that the kernel
  ;; can be loaded fully interpreted without having to do any build but at
  ;; the same time also load a compiled .o file from the bin dir if present
  (jazz.with-load-src/bin "../../kernel/module/runtime"
    (lambda (src)
      #f)
    (lambda (bin)
      (jazz.with-verbose jazz.load-verbose? "loading" (substring bin 5 (string-length bin))
        (lambda ()
          (jazz.load-filename bin))))))


;;;
;;;; Kernel
;;;


(define (jazz.load-kernel)
  (jazz.load-module-system)
  (jazz.register-reader-extensions 'jazz.dialect (lambda () jazz.jazz-readtable) '("jazz")))


(define jazz.boot-kernel
  (let ((loaded? #f))
    (lambda ()
      (if (not loaded?)
          (begin
            (jazz.load-kernel)
            (set! loaded? #t))))))
