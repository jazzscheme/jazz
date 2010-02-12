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


(jazz.define-variable jazz.compile-unit-internal)
(jazz.define-variable jazz.custom-compile-unit-internal)
(jazz.define-variable jazz.build-unit-internal)
(jazz.define-variable jazz.get-subunit-names-internal)


(define (jazz.compile-unit . rest)
  (jazz.load-unit 'core.module)
  (jazz.load-unit 'core.unit.builder)
  (%%apply jazz.compile-unit-internal rest))

(define (jazz.custom-compile-unit . rest)
  (jazz.load-unit 'core.module)
  (jazz.load-unit 'core.unit.builder)
  (%%apply jazz.custom-compile-unit-internal rest))

(define (jazz.build-unit . rest)
  (jazz.load-unit 'core.module)
  (jazz.load-unit 'core.unit.builder)
  (%%apply jazz.build-unit-internal rest))

(define (jazz.get-subunit-names . rest)
  (jazz.load-unit 'core.module)
  (jazz.load-unit 'core.unit.builder)
  (%%apply jazz.get-subunit-names-internal rest))


;;;
;;;; Hooks
;;;


(define (jazz.path->container-hook path)
  ;; store unit name instead of path if available
  (jazz.find-pathname-unit path))


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
        ;; find path from unit name
        ((%%symbol? container)
         (let ((src (jazz.find-unit-src container #f #f)))
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

(define (jazz.link-objects?)
  (%%memq 'objects jazz.link-options))

(define (jazz.link-libraries?)
  (%%memq 'libraries jazz.link-options))
   
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


(define jazz.image-load-counter 0)
(define jazz.object-load-counter 0)
(define jazz.interpreted-load-counter 0)


(define (jazz.increment-image-load-counter)
  (set! jazz.image-load-counter (%%fx+ 1 jazz.image-load-counter)))
(define (jazz.increment-object-load-counter)
  (set! jazz.object-load-counter (%%fx+ 1 jazz.object-load-counter)))
(define (jazz.increment-interpreted-load-counter)
  (set! jazz.interpreted-load-counter (%%fx+ 1 jazz.interpreted-load-counter)))


;;;
;;;; Library
;;;


(define (jazz.library-main)
  (jazz.process-jazzini-file)
  (jazz.setup-repositories))


;;;
;;;; Dynamic Libraries
;;;

(define jazz.currently-loading-library-procs)

(define (jazz.load-libraries)
  (define libraries (%%make-table test: eq?))
  (define (add-library package-name library-filename) 
    (%%table-set! libraries package-name library-filename))
  
  ; find the libraries
  (jazz.iterate-packages #t
    (lambda (package)
      (let ((products (%%package-products package)))
        (for-each
          (lambda (product-descriptor)
            (let ((product-name (jazz.product-descriptor-name product-descriptor)))           
              (or (%%table-ref libraries product-name #f)
                  (jazz.with-numbered-pathname 
                    (string-append (jazz.product-library-name-base package product-name) "." jazz.Library-Extension) #f 1
                    (lambda (filename exists?)
                      (if exists?
                          (add-library product-name filename)))))))
          products))))
      
  ; register all the libraries found
  (table-for-each
    (lambda (product-name library-filename)
      (let* ((pathname (path-normalize library-filename))
             (lib (##load-object-file pathname #t)))
        (if (and (%%vector? lib)
                 (%%vector? (%%vector-ref lib 0)))
            (begin
              (set! jazz.currently-loading-library-procs (%%vector-ref lib 0))
              ((%%vector-ref (%%vector-ref (%%vector-ref lib 0) 0) 1)))
          (jazz.feedback "WARNING: failed to load library {a}" pathname))))
    libraries))


; this function is called from the library header when loading
(define (jazz.register-image-units lib-name units)
  (define (index-for-each proc args n)
    (if (%%not (%%null? args))
        (begin
          (proc (%%car args) n)
          (index-for-each proc (%%cdr args) (%%fx+ n 1)))))
  
  (index-for-each
    (lambda (unit i)
      (let ((name (%%car unit))
            (load-proc (%%vector-ref (%%vector-ref jazz.currently-loading-library-procs i) 1))
            (compile-time-hash (%%cadr unit)))
        (jazz.set-image-unit
          name
          load-proc
          compile-time-hash)))
    units
    1)
  
;  (jazz.feedback (string-append "LIB: " (%%symbol->string lib-name) " (" (%%number->string (%%length units)) " units)"))
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
  
  (jazz.split-command-line (%%cdr (command-line)) '("debug" "force") '("eval" "load" "test" "run" "update" "build" "make" "compile" "debugger" "link" "jobs") missing-argument-for-option
    (lambda (options remaining)
      (let ((debug? (jazz.get-option "debug" options))
            (force? (jazz.get-option "force" options))
            (ev (jazz.get-option "eval" options))
            (load (jazz.get-option "load" options))
            (test (jazz.get-option "test" options))
            (run (jazz.get-option "run" options))
            (update (jazz.get-option "update" options))
            (build (jazz.get-option "build" options))
            (make (jazz.get-option "make" options))
            (compile (jazz.get-option "compile" options))
            (debugger (jazz.get-option "debugger" options))
            (link (symbol-argument (jazz.get-option "link" options)))
            (jobs (number-argument (jazz.get-option "jobs" options))))
        (define (setup-kernel)
          (set! ##allow-inner-global-define? #t)
          (set! jazz.debugger debugger)
          (jazz.process-jazzini-file)
          (jazz.setup-repositories))
          
        (define (setup-runtime)
          (setup-kernel)
          (jazz.load-libraries))
        
        (define (setup-build)
          (setup-kernel)
          (process-buildini-file)
          (set! jazz.link (or link (jazz.build-link)))
          (set! jazz.link-options (jazz.parse-link jazz.link))
          (set! jazz.jobs jobs)
          (if (or debug?
                  (%%eqv? jobs 0))
              (jazz.debug-build? #t)))
        
        (define (process-buildini-file)
          (if (file-exists? jazz.buildini-file)
              (jazz.load jazz.buildini-file)))
        
        (cond (ev
                (setup-runtime)
                (eval (call-with-input-string ev read)))
              (load
                (setup-runtime)
                (jazz.load-unit (%%string->symbol load)))
              (test
                (setup-runtime)
                (jazz.test-product (%%string->symbol test)))
              (run
                (setup-runtime)
                (jazz.run-product (%%string->symbol run)))
              (jazz.product
                (setup-runtime)
                (jazz.run-product jazz.product))
              (compile
                (setup-build)
                (jazz.custom-compile-unit (%%string->symbol compile) force?: force?))
              (update
                (setup-build)
                (jazz.update-product (%%string->symbol update)))
              (make
                (setup-build)
                (jazz.make-product (%%string->symbol make)))
              (build
                (setup-build)
                (jazz.subprocess-build-products))
              (else
                (setup-runtime)
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
