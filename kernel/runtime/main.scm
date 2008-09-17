;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Kernel Main
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


(jazz.kernel-declares)


;;;
;;;; Forward
;;;


(jazz.define-variable jazz.compile-module-internal)
(jazz.define-variable jazz.build-module-internal)


(define (jazz.compile-module . rest)
  (jazz.load-module 'core.library)
  (jazz.load-module 'core.module.build)
  (apply jazz.compile-module-internal rest))

(define (jazz.build-module . rest)
  (jazz.load-module 'core.library)
  (jazz.load-module 'core.module.build)
  (apply jazz.build-module-internal rest))


;;;
;;;; Compile
;;;


(define (jazz.compile name)
  (jazz.load-module 'core.library)
  (jazz.load-module 'core.module.build)
  (jazz.compile-module name))


;;;
;;;; Main
;;;


(define jazz.debug?
  #f)

(define jazz.initialization-file
  "~/.jazz/.jazzini")


(define (jazz.process-main)
  (define (warn-missing-argument-for-option opt)
    (jazz.repl-main
      (lambda (output-port)
        (##write-string
          "*** WARNING -- Missing argument for option \""
          output-port)
        (##write-string opt output-port)
        (##write-string "\"\n" output-port)
        #t)))
  
  (define (option? arg)
    (and (##fixnum.< 0 (##string-length arg))
         (or (##char=? (##string-ref arg 0) #\-)
             (##char=? (##string-ref arg 0) #\/))))
             
  (define (convert-option arg)
    (##substring arg 1 (##string-length arg)))

  (define (split-command-line
           arguments
           options-with-no-args
           options-with-args
           cont)
    (let loop ((args arguments)
               (rev-options '()))
      (if (and (##pair? args)
               (option? (##car args)))
          (let ((opt (convert-option (##car args)))
                (rest (##cdr args)))
            (cond ((##member opt options-with-no-args)
                   (loop rest
                         (##cons (##cons opt #f) rev-options)))
                  ((##member opt options-with-args)
                   (if (##pair? rest)
                       (loop (##cdr rest)
                             (##cons (##cons opt (##car rest)) rev-options))
                     (begin
                       (warn-missing-argument-for-option opt)
                       (loop rest rev-options))))
                  (else
                   (cont (##reverse rev-options) args))))
        (cont (##reverse rev-options) args))))
  
  (define (process-initialization-file)
    (if (file-exists? jazz.initialization-file)
        (jazz.load jazz.initialization-file)))
  
  (define (with-debug-exception-handler thunk)
    (let ((current-handler (current-exception-handler)))
      (with-exception-handler
        (lambda (exc)
          (jazz.debug-exception exc (console-port) (jazz.debug-build?) (jazz.debug-build?))
          (current-handler exc))
        thunk)))
  
  (split-command-line (%%cdr (command-line)) '() '("run" "make" "build" "compile" "debug")
    (lambda (options remaining)
      (define (get-option name)
        (let ((pair (%%assoc name options)))
          (if pair
              (%%cdr pair)
            #f)))
      
      (let ((run (get-option "run"))
            (make (get-option "make"))
            (build (get-option "build"))
            (compile (get-option "compile"))
            (debug? (get-option "debug")))
        (set! jazz.debug? debug?)
        (process-initialization-file)
        (jazz.install-repositories)
        (cond (run
               (jazz.run-product (%%string->symbol run)))
              (jazz.product
               (jazz.run-product jazz.product))
              (make
               (with-debug-exception-handler
                 (lambda ()
                   (jazz.make-product (%%string->symbol make)))))
              (build
               (with-debug-exception-handler
                 (lambda ()
                   (jazz.build-product (%%string->symbol build)))))
              (compile
               (with-debug-exception-handler
                 (lambda ()
                   (jazz.compile (%%string->symbol compile)))))
              (else
               (jazz.repl-main #f)))))))


(define (jazz.repl-main warnings)
  (current-input-port (repl-input-port))
  (current-output-port (repl-output-port))
  (current-error-port (repl-output-port))
  (##repl-debug
    (lambda (first output-port)
      (if warnings
          (warnings output-port))
      (display "Jazz " output-port)
      (display jazz.version output-port)
      (newline output-port)
      (newline output-port)
      (force-output output-port)
      #f)))
