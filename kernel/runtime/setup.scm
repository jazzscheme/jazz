;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Kernel Setup
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
(jazz.define-variable jazz.get-submodule-names-internal)


(define (jazz.compile-module . rest)
  (jazz.load-module 'core.library)
  (jazz.load-module 'core.module.builder)
  (%%apply jazz.compile-module-internal rest))

(define (jazz.build-module . rest)
  (jazz.load-module 'core.library)
  (jazz.load-module 'core.module.builder)
  (%%apply jazz.build-module-internal rest))

(define (jazz.get-submodule-names . rest)
  (jazz.load-module 'core.library)
  (jazz.load-module 'core.module.builder)
  (%%apply jazz.get-submodule-names-internal rest))


;;;
;;;; Hooks
;;;


(define (jazz.path->container-hook path)
  ;; store module name instead of path if available
  (jazz.find-pathname-module path))


(define (jazz.container->path-hook container)
  (cond ;; relocate if kernel was moved from source built directory
        ((and (%%string? container)
              jazz.kernel-source-built
              jazz.kernel-source
              (jazz.string-starts-with? container jazz.kernel-source-built))
         (%%string-append jazz.kernel-source
                          (%%substring container
                                       (%%string-length jazz.kernel-source-built)
                                       (%%string-length container))))
        ;; find path from module name
        ((%%symbol? container)
         (let ((src (jazz.find-module-src container #f #f)))
           (if src
               (jazz.resource-pathname src)
             #f)))
        (else
         #f)))


(define (jazz.container->id-hook container)
  (if (%%symbol? container)
      (%%symbol->string container)
    #f))


(set! ##path->container-hook jazz.path->container-hook)
(set! ##container->path-hook jazz.container->path-hook)
(set! ##container->id-hook jazz.container->id-hook)


;;;
;;;; Common
;;;


(define jazz.debugger
  #f)

(define jazz.link
  #f)

(define jazz.link-options
  #f)

(define jazz.jobs
  #f)

(define jazz.jazzini-file
  "~/.jazz/.jazzini")

(define jazz.buildini-file
  "~/.jazz/.buildini")

(define jazz.warnings
  #f)


(define (jazz.process-jazzini-file)
  (if (file-exists? jazz.jazzini-file)
      (jazz.load jazz.jazzini-file)))


;;;
;;;; Library
;;;


(define (jazz.library-main)
  (jazz.process-jazzini-file)
  (jazz.setup-repositories))


;;;
;;;; DynLibs
;;;

(define jazz.currently-loading-library-procs)

(define (jazz.load-composite-libraries)
  (define (get-object-serial ext)
    (and (%%fx>= (%%string-length ext) 3)
         (let* ((ns (%%substring ext 2 (%%string-length ext)))
                (n (string->number ns)))
           (and (%%string=? ".o" (%%substring ext 0 2))
                (%%number? n)
                n))))
  
  (define (for-each-file-in-directory dir proc)
    (let ((p (open-directory dir)))
      (let loop ()
           (let ((filename (read p)))
             (if (%%string? filename)
                 (begin
                   (proc filename)
                   (loop)))))
      (close-input-port p)))
  
  (define libraries (make-table))
  
  ; scan the directory for library files
  (for-each-file-in-directory jazz.kernel-install
    (lambda (filename)
      (let ((lib-name (path-strip-extension filename))
            (serial (get-object-serial (path-extension filename))))
        (if serial
            (let ((prev (%%table-ref libraries lib-name #f)))
              (if (or (and prev (%%fx> serial prev))
                      (%%not prev))
                  (%%table-set! libraries lib-name serial)))))))
  
  ; register all the libraries found
  (table-for-each
    (lambda (lib serial)
      (let* ((pathname (path-normalize (string-append jazz.kernel-install "/" lib ".o" (%%number->string serial))))
             (lib (##load-object-file pathname #t)))
        (if (and (%%vector? lib)
                 (%%vector? (%%vector-ref lib 0)))
            (begin
              (set! jazz.currently-loading-library-procs (%%vector-ref lib 0))
              ((%%vector-ref (%%vector-ref (%%vector-ref lib 0) 0) 1)))
          (jazz.feedback "WARNING: failed to load library " pathname))))
    libraries))


; this function is called from the library header when loading
(define (jazz.register-image-modules lib-name modules)
  (define (index-for-each proc args n)
    (if (%%not (%%null? args))
        (begin
          (proc (%%car args) n)
          (index-for-each proc (%%cdr args) (%%fx+ n 1)))))
  
  (index-for-each
    (lambda (module i)
      (let ((name (%%car module))
            (load-proc (%%vector-ref (%%vector-ref jazz.currently-loading-library-procs i) 1))
            (compile-time-hash (%%cadr module)))
        (jazz.set-image-module
          name
          load-proc
          compile-time-hash)))
    modules
    1)
  
  ;(jazz.feedback (string-append "LIB: " (%%symbol->string lib-name) " (" (%%number->string (%%length modules)) " modules)"))
)


;;;
;;;; Executable
;;;


(define (jazz.executable-main)
  (define (missing-argument-for-option opt)
    (set! jazz.warnings
          (lambda (output-port)
            (%%write-string
              "*** WARNING -- Missing argument for option \""
              output-port)
            (%%write-string opt output-port)
            (%%write-string "\"\n" output-port)
            #t))
    (jazz.repl-main))
  
  (define (number-argument arg)
    (if (%%string? arg)
        (%%string->number arg)
      arg))
  
  (define (symbol-argument arg)
    (if (%%string? arg)
        (%%string->symbol arg)
      arg))
  
  (define (process-buildini-file)
    (if (file-exists? jazz.buildini-file)
        (jazz.load jazz.buildini-file)))
  
  (jazz.split-command-line (%%cdr (command-line)) '("debug") '("load" "test" "run" "update" "build" "make" "compile" "debugger" "link" "jobs") missing-argument-for-option
    (lambda (options remaining)
      (let ((load (jazz.get-option "load" options))
            (test (jazz.get-option "test" options))
            (run (jazz.get-option "run" options))
            (update (jazz.get-option "update" options))
            (build (jazz.get-option "build" options))
            (make (jazz.get-option "make" options))
            (compile (jazz.get-option "compile" options))
            (debugger (jazz.get-option "debugger" options))
            (link (symbol-argument (jazz.get-option "link" options)))
            (jobs (number-argument (jazz.get-option "jobs" options))))
        ;; until the library syntax doesn't generate global defines
        (set! ##allow-inner-global-define? #t)
        (set! jazz.debugger debugger)
        (set! jazz.link (or link (jazz.build-link)))
        (set! jazz.link-options (jazz.parse-link jazz.link))
        (set! jazz.jobs jobs)
        (jazz.process-jazzini-file)
        (jazz.setup-repositories)
        (if (or (jazz.get-option "debug" options)
                (%%eqv? jobs 0))
            (jazz.debug-build? #t))
        (cond (load
                (jazz.load-composite-libraries)
                (jazz.load-module (%%string->symbol load)))
              (test
                (jazz.load-composite-libraries)
                (jazz.test-product (%%string->symbol test)))
              (run
                (jazz.load-composite-libraries)
                (jazz.run-product (%%string->symbol run)))
              (jazz.product
                (jazz.load-composite-libraries)
                (jazz.run-product jazz.product))
              (compile
                (process-buildini-file)
                (jazz.compile-module (%%string->symbol compile)))
              (update
                (process-buildini-file)
                (jazz.update-product (%%string->symbol update)))
              (make
                (process-buildini-file)
                (jazz.make-product (%%string->symbol make)))
              (build
                (process-buildini-file)
                (jazz.subprocess-build-products))
              (else
                (jazz.repl-main)))))))


(define (jazz.repl-main)
  (current-input-port (repl-input-port))
  (current-output-port (repl-output-port))
  (current-error-port (repl-output-port))
  (%%repl
    (lambda (first output-port)
      (if jazz.warnings
          (jazz.warnings output-port))
      (display "JazzScheme Kernel v" output-port)
      (display (jazz.present-version jazz.kernel-version) output-port)
      (newline output-port)
      (newline output-port)
      (force-output output-port)
      #f)))
