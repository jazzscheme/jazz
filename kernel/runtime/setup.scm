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
;;;  Portions created by the Initial Developer are Copyright (C) 1996-2015
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
;;;; Callable
;;;


(c-declare #<<c-end
#include <stdbool.h>

bool exitCallable = false;
c-end
)


(define jazz:set-exit-callable
  (c-lambda (bool) void
    #<<c-end
    exitCallable = ___arg1;
c-end
))


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
         (let ((src (jazz:find-unit-src container error?: #f)))
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
  (jazz:global-bound? '##gambcomp))


;;;
;;;; Exit Jobs
;;;


(define (jazz:make-exit-jobs-safe)
  (let ((lst (fifo->list ##exit-jobs)))
    (##clear-exit-jobs!)
    (for-each jazz:add-exit-job! lst)))


(define (jazz:add-exit-job! thunk)
  (##add-exit-job!
    (jazz:safe-exit-job thunk)))


(define (jazz:safe-exit-job thunk)
  (lambda ()
    (with-exception-catcher
      (lambda (exc)
        (let ((dir (or jazz:jazz-settings-directory "~/.jazz/")))
          (jazz:create-directory dir)
          (call-with-output-file (%%string-append dir "exit.exception")
            (lambda (port)
              (write exc port)
              (newline port)
              (newline port)
              (display-exception exc port)
              (newline port)
              (newline port)
              (continuation-capture
                (lambda (k)
                  (display-continuation-backtrace k port #f #f 500 500)))
              (force-output port))))
        (##clear-exit-jobs!)
        (##exit 1))
      thunk)))


;;;
;;;; Library
;;;


(define (jazz:library-main)
  (if jazz:kernel-source-access?
      (begin
        (jazz:setup-settings)
        (jazz:process-jazzini #t)))
  (jazz:make-exit-jobs-safe)
  (jazz:prepare-repositories)
  (jazz:setup-repositories))


;;;
;;;; Dynamic Libraries
;;;


(define (jazz:load-libraries)
  (define libraries (%%make-table test: eq?))
  (define (add-library product-name library-filename)
    (if (%%table-ref libraries product-name #f)
        #f #;
        ;; quick patch for multiple binary repositories
        ;; ideally we should probably load the most recent library but
        ;; the patch is still correct as libraries validate against the digest of
        ;; the source file and multiple binary repositories are only used for development
        (jazz:error "Found duplicate library: {s}" product-name)
      (%%table-set! libraries product-name library-filename)))
  
  ; find the libraries
  (jazz:iterate-packages #t
    (lambda (package)
      (let ((products (%%get-package-products package)))
        (for-each (lambda (product-descriptor)
                    (let ((product-name (jazz:product-descriptor-name product-descriptor)))
                      (jazz:with-numbered-pathname (string-append (jazz:product-library-name-base package product-name) "." jazz:Library-Extension) #f 1
                        (lambda (filename exists?)
                          (if exists?
                              (add-library product-name filename))))))
                  products))))
  
  ; load the libraries found
  (jazz:iterate-table libraries
    (lambda (product-name library-filename)
      (let ((pathname (path-normalize library-filename)))
        (jazz:load-binary pathname #t)
        (let ((header-name (%%string->symbol (string-append jazz:product-uniqueness-prefix (%%symbol->string product-name)))))
          (##load-required-module header-name))))))


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
            (compile-time-hash (%%cadr unit)))
        (let ((unique-module-name (%%string->symbol (%%string-append jazz:lib-uniqueness-prefix (%%symbol->string name)))))
          (let ((load-proc (lambda () (##load-required-module unique-module-name))))
            (jazz:set-image-unit
              name
              load-proc
              compile-time-hash)))))
    units
    1)
  
;  (jazz:feedback (string-append "LIB: " (%%symbol->string lib-name) " (" (%%number->string (%%length units)) " units)"))
)


;;;
;;;; Executable
;;;


(declare (proper-tail-calls))


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
  
  ;; c -> compile
  ;; e -> eval
  ;; g -> gambit
  ;; l -> load
  ;; r -> run
  ;; t -> test
  ;; v -> version
  ;; x -> expand
  ;; commented out to get proper tail call into the repl
  ;; (let ((exit-code ...)))
  (jazz:split-command-line (jazz:command-arguments) '("v" "version" "nosource" "debug" "force" "sweep" "worker" "keep-c" "track-scheme" "expansion" "gvm" "emit" "dry" "g" "gambit") '("build-repository" "jazz-repository" "repositories" "dependencies" "e" "eval" "l" "load" "t" "test" "r" "run" "update" "make" "build" "install" "deploy" "x" "expand" "c" "compile" "target" "debugger" "link" "j" "jobs" "port" "dialect") missing-argument-for-option
    (lambda (commands options remaining)
      (let ((version? (or (jazz:get-option "v" options) (jazz:get-option "version" options)))
            (nosource? (jazz:get-option "nosource" options))
            (debug? (jazz:get-option "debug" options))
            (force? (jazz:get-option "force" options))
            (sweep? (jazz:get-option "sweep" options))
            (worker? (jazz:get-option "worker" options))
            (keep-c? (jazz:get-option "keep-c" options))
            (track-scheme? (jazz:get-option "track-scheme" options))
            (expansion? (jazz:get-option "expansion" options))
            (gvm? (jazz:get-option "gvm" options))
            (emit? (jazz:get-option "emit" options))
            (dry? (jazz:get-option "dry" options))
            (gambit? (or (jazz:get-option "g" options) (jazz:get-option "gambit" options)))
            (build-repository (jazz:get-option "build-repository" options))
            (jazz-repository (jazz:get-option "jazz-repository" options))
            (repositories (jazz:get-option "repositories" options))
            (dependencies (jazz:get-option "dependencies" options))
            (ev (or (jazz:get-option "e" options) (jazz:get-option "eval" options)))
            (load (or (jazz:get-option "l" options) (jazz:get-option "load" options)))
            (test (or (jazz:get-option "t" options) (jazz:get-option "test" options)))
            (run (or (jazz:get-option "r" options) (jazz:get-option "run" options)))
            (update (jazz:get-option "update" options))
            (make (jazz:get-option "make" options))
            (build (jazz:get-option "build" options))
            (install (jazz:get-option "install" options))
            (deploy (jazz:get-option "deploy" options))
            (expand (or (jazz:get-option "x" options) (jazz:get-option "expand" options)))
            (compile (or (jazz:get-option "c" options) (jazz:get-option "compile" options)))
            (target (symbol-argument (jazz:get-option "target" options)))
            (debugger (jazz:get-option "debugger" options))
            (link (symbol-argument (jazz:get-option "link" options)))
            (jobs (number-argument (or (jazz:get-option "j" options) (jazz:get-option "jobs" options))))
            (port (number-argument (jazz:get-option "port" options)))
            (dialect (symbol-argument (jazz:get-option "dialect" options))))
        (define (setup-kernel)
          (if (and jazz:kernel-install (jazz:global-bound? '##set-gambitdir!))
              (let ((gambitdir (jazz:absolutize-directory jazz:kernel-install jazz:gambit-dir)))
                (if (and gambitdir (jazz:directory-exists? gambitdir))
                    (let ((setter (jazz:global-ref '##set-gambitdir!)))
                      (setter gambitdir)))))
          (set! ##allow-inner-global-define? #t)
          (set! jazz:debugger debugger)
          (if nosource?
              (set! jazz:kernel-source-access? #f))
          (if jazz:kernel-source-access?
              (begin
                (jazz:setup-settings)
                (jazz:process-jazzini #t)))
          (jazz:make-exit-jobs-safe))
        
        #; ;; dynamic-dependencies
        (define (locate-dependencies root-path)
          (and root-path
               (let ((dynamic-file (%%string-append root-path ".dependencies")))
                 (and (jazz:file-exists? dynamic-file)
                      dynamic-file))))
        
        (define (setup-repositories make?)
          (if build-repository (jazz:build-repository build-repository))
          (if jazz-repository (jazz:jazz-repository jazz-repository))
          (if repositories (jazz:repositories repositories))
          #; ;; dynamic-dependencies
          (let ((dynamic-file (or dependencies
                                  (locate-dependencies (jazz:build-repository))
                                  (locate-dependencies jazz:kernel-install))))
            (if dynamic-file
                (jazz:dependencies (call-with-input-file dynamic-file read))))
          (jazz:prepare-repositories)
          (let ((needs-sweep (and make? (or sweep? (jazz:build-repository-needs-sweep)))))
            (if needs-sweep
                (jazz:sweep-build-repository needs-sweep)))
          (jazz:setup-repositories))
        
        (define (setup-runtime)
          (setup-kernel)
          (setup-repositories #f)
          (jazz:load-libraries)
          ;; to test cross compiling REMOVE CODE WHEN DONE
          (setup-target))
        
        (define (setup-build #!optional (make? #f))
          (setup-kernel)
          (jazz:process-buildini #t)
          (setup-repositories make?)
          (set! jazz:link (or link (jazz:build-link)))
          (set! jazz:link-options (jazz:parse-link jazz:link))
          (set! jazz:jobs jobs)
          (if (not jazz:kernel-interpreted?)
              (jazz:disable-crash-window))
          (if (or debug? (%%eqv? jobs 0) dry?)
              (jazz:debug-build? #t))
          (if keep-c?
              (set! jazz:compile-options (%%cons 'keep-c (%%cons 'track-scheme jazz:compile-options))))
          (if track-scheme?
              (set! jazz:compile-options (%%cons 'track-scheme jazz:compile-options)))
          (if expansion?
              (set! jazz:compile-options (%%cons 'expansion jazz:compile-options)))
          (if gvm?
              (set! jazz:compile-options (%%cons 'gvm jazz:compile-options)))
          (if emit?
              (jazz:save-emit? #t))
          (if dry?
              (jazz:dry-run? #t))
          (setup-target))
        
        (define (setup-target)
          (if target
              (let ((configuration (jazz:find-named-configuration target)))
                (if (%%not configuration)
                    (jazz:error "Unknown configuration: {s}" target)
                  (begin
                    (jazz:build-target target)
                    (jazz:build-configuration configuration)
                    ;; quick hack load everything needed for compilation before changing repositories
                    ;; this will clearly be missing user-defined syntax
                    (jazz:load-unit 'foundation)
                    (jazz:load-unit 'jazz)
                    (jazz:load-unit 'jazz.language.syntax)
                    (jazz:load-unit 'core.unit.runtime)
                    (jazz:load-unit 'core.unit.build)
                    (jazz:load-unit 'scheme.syntax-rules)
                    (jazz:load-unit 'jazz.core)
                    (jazz:load-unit 'jazz.foreign)
                    (jazz:load-unit 'jazz.foreign.syntax)
                    (jazz:load-unit 'jazz.platform.types-syntax)
                    (let ((target-platform (jazz:get-configuration-platform configuration))
                          (target-processor (jazz:get-configuration-processor configuration)))
                      (if (%%eq? target-platform 'ios)
                          (##cond-expand-features (jazz:remove 'cocoa (##cond-expand-features))))
                      (##cond-expand-features (append (##cond-expand-features) (list target-platform)))
                      (if target-processor
                          (##cond-expand-features (append (##cond-expand-features) (list target-processor)))))
                    (jazz:TARGET-HACK?-set! #t)
                    (let ((old jazz:Build-Repository))
                      (set! jazz:Build-Repository (jazz:make-repository 'Build "lib" (%%string-append jazz:kernel-source (jazz:get-configuration-destination configuration)) binary?: #t dynamic?: #t))
                      (set! jazz:Repositories (%%append (jazz:remove old jazz:Repositories) (%%list jazz:Build-Repository))))
                    (set! jazz:*binary-packages-cache* (%%make-table test: eq?)))))))
        
        (define (setup-install)
          (setup-build))
        
        (define (setup-deploy)
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
                                 (jazz:load-script path)
                               (jazz:error "Can't find file {s}" path)))
                           (iter (%%cdr scan))))))))
        
        (define (show-version)
          (##write-string "Gambit" ##stdout-port)
          (##write-string " " ##stdout-port)
          (##write-string (system-version-string) ##stdout-port)
          (##write-string " " ##stdout-port)
          (##write (system-stamp) ##stdout-port)
          (##write-string " " ##stdout-port)
          (##write-string (system-type-string) ##stdout-port)
          (##write-string " " ##stdout-port)
          (##write (configure-command-string) ##stdout-port)
          (##newline ##stdout-port))
        
        (cond (version?
               (show-version))
              (ev
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
               (setup-build #t)
               (exit (jazz:make-product (%%string->symbol make))))
              (worker?
               (setup-build)
               (jazz:worker-process port))
              (build
               (setup-build)
               (let ((name (%%string->symbol build)))
                 (define (assert-build-configuration)
                   (if (%%not (jazz:build-configuration))
                       (jazz:error "Building a kernel requires an explicit target")))
                 
                 (case name
                   ((kernel)
                    (assert-build-configuration)
                    (jazz:build-kernel))
                   ((kernellib)
                    (assert-build-configuration)
                    (jazz:build-kernel image: 'library))
                   (else
                    (jazz:build-product name)))))
              (install
               (setup-install)
               (jazz:install-product (%%string->symbol install)))
              (deploy
               (setup-deploy)
               (jazz:deploy-product (%%string->symbol deploy)))
              ((or (%%not (%%null? commands))
                   (%%not (%%null? remaining)))
               (setup-runtime)
               (run-scripts (%%append commands remaining)))
              (else
               (if debug?
                   (setup-build)
                 (setup-runtime))
               (jazz:repl-main gambit? dialect))))))
  #; ;; see above commentary about proper tail call
  (exit (if (integer? exit-code) exit-code 0)))


;;;
;;;; REPL
;;;


(define jazz:expansion-context
  'terminal)

(define jazz:expansion-dialect
  'jazz)


(define (jazz:repl-main #!optional (gambit? #f) (dialect #f))
  (jazz:setup-readtable)
  (jazz:setup-expansion-hook)
  (if gambit?
      (set! jazz:expansion-context #f))
  (if dialect
      (if (eq? dialect 'none)
          (set! jazz:expansion-context #f)
        (set! jazz:expansion-dialect dialect)))
  (current-input-port (repl-input-port))
  (current-output-port (repl-output-port))
  (current-error-port (repl-output-port))
  (parameterize ((jazz:walk-for 'terminal)
                 (jazz:requested-unit-name 'terminal)
                 (jazz:generate-symbol-for "&")
                 (jazz:generate-symbol-context 'terminal)
                 (jazz:generate-symbol-counter 0))
    (##repl-debug
      (lambda (first output-port)
        (if jazz:warnings
            (jazz:warnings output-port))
        (display "JazzScheme v" output-port)
        (display (jazz:present-version jazz:kernel-version) output-port)
        (newline output-port)
        (newline output-port)
        (force-output output-port)
        #f)
      #t)))


(define (jazz:setup-readtable)
  (input-port-readtable-set! (repl-input-port) jazz:jazz-readtable))


(define (jazz:setup-expansion-hook)
  (set! ##expand-source (lambda (src)
                          (let ((expansion (jazz:expand-src src)))
                            (if (jazz:debug-expansion?)
                                (pp (list src '---> expansion)))
                            expansion))))


(define (jazz:expand-src src)
  (if (%%eq? (jazz:walk-for) 'terminal)
      (let ((code (%%desourcify src)))
        (if (and (%%pair? code) (%%eq? (%%car code) 'in))
            (if (%%null? (%%cdr code))
                (%%sourcify
                  `',jazz:expansion-context
                  src)
              (let ((context (%%cadr code)))
                (set! jazz:expansion-context context)
                (%%sourcify
                  `',context
                  src)))
          (cond ((%%not jazz:expansion-context)
                 src)
                (else
                 (jazz:load-foundation)
                 (%%sourcify
                   `(module ,jazz:expansion-context ,jazz:expansion-dialect ,src)
                   src)))))
    src)))
