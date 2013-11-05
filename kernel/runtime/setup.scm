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
;;;  Portions created by the Initial Developer are Copyright (C) 1996-2012
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


(block kernel.setup


(jazz:kernel-declares)


;;;
;;;; Forward
;;;


(jazz:define-variable jazz:compile-unit-internal)
(jazz:define-variable jazz:custom-compile-unit-internal)
(jazz:define-variable jazz:build-unit-internal)
(jazz:define-variable jazz:get-subunit-names-internal)


(define (jazz:compile-unit . rest)
  (jazz:load-build)
  (%%apply jazz:compile-unit-internal rest))

(define (jazz:custom-compile-unit . rest)
  (jazz:load-build)
  (%%apply jazz:custom-compile-unit-internal rest))

(define (jazz:build-unit . rest)
  (jazz:load-build)
  (%%apply jazz:build-unit-internal rest))

(define (jazz:get-subunit-names . rest)
  (jazz:load-build)
  (%%apply jazz:get-subunit-names-internal rest))


;;;
;;;; Hooks
;;;


(define jazz:path->container-override
  (make-parameter #f))


;; store unit name instead of path if available
(define (jazz:path->container-hook path)
  (let ((override (jazz:path->container-override)))
    (or (and override (override path))
        (jazz:find-pathname-unit path))))


(define (jazz:container->path-hook container)
  (cond ;; relocate if kernel was moved from source built directory
        ((and (%%string? container)
              jazz:kernel-source-built
              jazz:kernel-source
              (jazz:string-starts-with? container jazz:kernel-source-built))
         (%%string-append jazz:kernel-source
                          (%%substring container
                                       (%%string-length jazz:kernel-source-built)
                                       (%%string-length container))))
        ;; find path from unit name
        ((%%symbol? container)
         (let ((src (jazz:find-unit-src container #f #f)))
           (if src
               (jazz:resource-pathname src)
             #f)))
        (else
         #f)))


(define (jazz:container->id-hook container)
  (if (%%symbol? container)
      (%%symbol->string container)
    #f))


(set! ##path->container-hook jazz:path->container-hook)
(set! ##container->path-hook jazz:container->path-hook)
(set! ##container->id-hook jazz:container->id-hook)


;;;
;;;; Common
;;;


(jazz:define-variable jazz:image-load-counter 0)
(jazz:define-variable jazz:object-load-counter 0)
(jazz:define-variable jazz:interpreted-load-counter 0)


(define (jazz:increment-image-load-counter)
  (set! jazz:image-load-counter (%%fx+ 1 jazz:image-load-counter)))
(define (jazz:increment-object-load-counter)
  (set! jazz:object-load-counter (%%fx+ 1 jazz:object-load-counter)))
(define (jazz:increment-interpreted-load-counter)
  (set! jazz:interpreted-load-counter (%%fx+ 1 jazz:interpreted-load-counter)))


;;;
;;;; Compiler
;;;


(define (jazz:compiler-present?)
  (jazz:global-bound? '##gambc-cc))


;;;
;;;; Library
;;;


(define (jazz:library-main)
  (if jazz:kernel-source-access?
      (begin
        (jazz:setup-settings)
        (jazz:process-jazzini #t)))
  (jazz:prepare-repositories)
  (jazz:setup-repositories))


;;;
;;;; Dynamic Libraries
;;;


(jazz:define-variable jazz:currently-loading-library-procs #f)


(define (jazz:load-libraries)
  (define libraries (%%make-table test: eq?))
  (define (add-library product-name library-filename)
    (if (%%table-ref libraries product-name #f)
        (jazz:error "Found duplicate library: {s}" product-name)
      (%%table-set! libraries product-name library-filename)))
  
  ; find the libraries
  (jazz:iterate-packages #t
    (lambda (package)
      (let ((products (%%get-package-products package)))
        (for-each (lambda (product-descriptor)
                    (let ((product-name (jazz:product-descriptor-name product-descriptor)))
                      (jazz:with-numbered-pathname
                        (string-append (jazz:product-library-name-base package product-name) "." jazz:Library-Extension) #f 1
                        (lambda (filename exists?)
                          (if exists?
                              (add-library product-name filename))))))
                  products))))
  
  ; register all the libraries found
  (jazz:iterate-table libraries
    (lambda (product-name library-filename)
      (let* ((pathname (path-normalize library-filename))
             (lib (##load-object-file pathname #t)))
        (if (and (%%vector? lib)
                 (%%vector? (%%vector-ref lib 0)))
            (begin
              (set! jazz:currently-loading-library-procs (%%vector-ref lib 0))
              ((%%vector-ref (%%vector-ref (%%vector-ref lib 0) 0) 1)))
          (jazz:feedback "WARNING: failed to load library {a}" pathname))))))


; this function is called from the library header when loading
(define (jazz:register-image-units lib-name units)
  (define (index-for-each proc args n)
    (if (%%not (%%null? args))
        (begin
          (proc (%%car args) n)
          (index-for-each proc (%%cdr args) (%%fx+ n 1)))))
  
  (index-for-each
    (lambda (unit i)
      (let ((name (%%car unit))
            (load-proc (%%vector-ref (%%vector-ref jazz:currently-loading-library-procs i) 1))
            (compile-time-hash (%%cadr unit)))
        (jazz:set-image-unit
          name
          load-proc
          compile-time-hash)))
    units
    1)
  
;  (jazz:feedback (string-append "LIB: " (%%symbol->string lib-name) " (" (%%number->string (%%length units)) " units)"))
)


;;;
;;;; Executable
;;;


(define (jazz:executable-main)
  (define (missing-argument-for-option opt)
    (set! jazz:warnings
          (lambda (output-port)
            (%%write-string
              "*** WARNING -- Missing argument for option \""
              output-port)
            (%%write-string opt output-port)
            (%%write-string "\"\n" output-port)
            #t))
    (jazz:repl-main))
  
  (define (number-argument arg)
    (if (%%string? arg)
        (%%string->number arg)
      arg))
  
  (define (symbol-argument arg)
    (if (%%string? arg)
        (%%string->symbol arg)
      arg))
  
  (let ((exit-code (jazz:split-command-line (%%cdr (command-line)) '("nosource" "debug" "force" "subbuild" "keep-c" "expansion" "gvm" "emit" "dry") '("build-repository" "jazz-repository" "repositories" "dependencies" "eval" "load" "test" "run" "update" "make" "build" "install" "expand" "compile" "debugger" "link" "jobs" "port") missing-argument-for-option
                     (lambda (commands options remaining)
                       (let ((nosource? (jazz:get-option "nosource" options))
                             (debug? (jazz:get-option "debug" options))
                             (force? (jazz:get-option "force" options))
                             (subbuild? (jazz:get-option "subbuild" options))
                             (keep-c? (jazz:get-option "keep-c" options))
                             (expansion? (jazz:get-option "expansion" options))
                             (gvm? (jazz:get-option "gvm" options))
                             (emit? (jazz:get-option "emit" options))
                             (dry? (jazz:get-option "dry" options))
                             (build-repository (jazz:get-option "build-repository" options))
                             (jazz-repository (jazz:get-option "jazz-repository" options))
                             (repositories (jazz:get-option "repositories" options))
                             (dependencies (jazz:get-option "dependencies" options))
                             (ev (jazz:get-option "eval" options))
                             (load (jazz:get-option "load" options))
                             (test (jazz:get-option "test" options))
                             (run (jazz:get-option "run" options))
                             (update (jazz:get-option "update" options))
                             (make (jazz:get-option "make" options))
                             (build (jazz:get-option "build" options))
                             (install (jazz:get-option "install" options))
                             (expand (jazz:get-option "expand" options))
                             (compile (jazz:get-option "compile" options))
                             (debugger (jazz:get-option "debugger" options))
                             (link (symbol-argument (jazz:get-option "link" options)))
                             (jobs (number-argument (jazz:get-option "jobs" options)))
                             (port (number-argument (jazz:get-option "port" options))))
                         (define (setup-kernel)
                           (if (and jazz:kernel-install (jazz:global-bound? '##set-gambcdir!))
                               (let ((gambcdir (jazz:absolutize-directory jazz:kernel-install jazz:gambit-dir)))
                                 (if (and gambcdir (jazz:directory-exists? gambcdir))
                                     (let ((setter (jazz:global-ref '##set-gambcdir!)))
                                       (setter gambcdir)))))
                           (set! ##allow-inner-global-define? #t)
                           (set! jazz:debugger debugger)
                           (if nosource?
                               (set! jazz:kernel-source-access? #f))
                           (if jazz:kernel-source-access?
                               (begin
                                 (jazz:setup-settings)
                                 (jazz:process-jazzini #t))))
                         
                         (define (locate-dependencies root-path)
                           (and root-path
                                (let ((dynamic-file (%%string-append root-path ".dependencies")))
                                  (and (jazz:file-exists? dynamic-file)
                                       dynamic-file))))
                         
                         (define (setup-repositories)
                           (if build-repository (jazz:build-repository build-repository))
                           (if jazz-repository (jazz:jazz-repository jazz-repository))
                           (if repositories (jazz:repositories repositories))
                           (let ((dynamic-file (or dependencies
                                                   (locate-dependencies (jazz:build-repository))
                                                   (locate-dependencies jazz:kernel-install))))
                             (if dynamic-file
                                 (jazz:dependencies (call-with-input-file dynamic-file read))))
                           (jazz:prepare-repositories)
                           (jazz:setup-repositories))
                         
                         (define (setup-runtime)
                           (setup-kernel)
                           (setup-repositories)
                           (jazz:load-libraries))
                         
                         (define (setup-build)
                           (setup-kernel)
                           (jazz:process-buildini #t)
                           (setup-repositories)
                           (set! jazz:link (or link (jazz:build-link)))
                           (set! jazz:link-options (jazz:parse-link jazz:link))
                           (set! jazz:jobs jobs)
                           (if (not jazz:kernel-interpreted?)
                               (jazz:disable-crash-window))
                           (if (or debug? (%%eqv? jobs 0) dry?)
                               (jazz:debug-build? #t))
                           (if keep-c?
                               (set! jazz:compile-options (%%cons 'keep-c (%%cons 'track-scheme jazz:compile-options))))
                           (if expansion?
                               (set! jazz:compile-options (%%cons 'expansion jazz:compile-options)))
                           (if gvm?
                               (set! jazz:compile-options (%%cons 'gvm jazz:compile-options)))
                           (if emit?
                               (jazz:save-emit? #t))
                           (if dry?
                               (jazz:dry-run? #t)))
                         
                         (define (setup-install)
                           (setup-build))
                         
                         (define (run-scripts lst)
                           (jazz:load-foundation)
                           (let iter ((scan lst))
                             (if (%%not (%%null? scan))
                                 (let ((arg (%%car scan)))
                                   (if (%%not (jazz:option? arg))
                                       (begin
                                         (let ((path (if (jazz:pathname-extension arg) arg (jazz:add-extension arg "jazz"))))
                                           (if (file-exists? path)
                                               (jazz:load-script path)))
                                         (iter (%%cdr scan))))))))
                         
                         (cond (ev
                                (setup-runtime)
                                (eval (call-with-input-string ev read)))
                               (load
                                (setup-runtime)
                                (jazz:load-unit (%%string->symbol load)))
                               (test
                                (setup-runtime)
                                (jazz:test-product (%%string->symbol test)))
                               (run
                                (setup-runtime)
                                (jazz:run-product (%%string->symbol run)))
                               (jazz:product
                                (setup-runtime)
                                (jazz:run-product jazz:product))
                               (expand
                                (setup-build)
                                (jazz:load-unit 'foundation)
                                (jazz:load-unit 'dialect.development)
                                ((jazz:global-ref 'jazz:expand) (%%string->symbol expand)))
                               (compile
                                (setup-build)
                                (for-each (lambda (name)
                                            (jazz:custom-compile-unit (%%string->symbol name) force?: force?))
                                          (jazz:split-string compile #\;)))
                               (update
                                (setup-build)
                                (jazz:update-product (%%string->symbol update)))
                               (make
                                (setup-build)
                                (jazz:make-product (%%string->symbol make)))
                               (subbuild?
                                (setup-build)
                                (jazz:subprocess-build-products port))
                               (build
                                (setup-build)
                                (jazz:build-product (%%string->symbol build)))
                               (install
                                (setup-install)
                                (jazz:install-product (%%string->symbol install)))
                               ((or (%%not (%%null? commands))
                                    (%%not (%%null? remaining)))
                                (setup-runtime)
                                (run-scripts (%%append commands remaining)))
                               (else
                                (if debug?
                                    (setup-build)
                                  (setup-runtime))
                                (jazz:repl-main))))))))
    (exit (if (integer? exit-code) exit-code 0))))


(define (jazz:repl-main)
  (current-input-port (repl-input-port))
  (current-output-port (repl-output-port))
  (current-error-port (repl-output-port))
  (%%repl
    (lambda (first output-port)
      (if jazz:warnings
          (jazz:warnings output-port))
      (display "JazzScheme Kernel v" output-port)
      (display (jazz:present-version jazz:kernel-version) output-port)
      (newline output-port)
      (newline output-port)
      (force-output output-port)
      #f))))
