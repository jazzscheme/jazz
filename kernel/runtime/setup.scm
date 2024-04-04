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


(block kernel.setup


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
;;;; Permissions
;;;


(cond-expand
  (windows)
  (else
   (c-declare #<<end-of-code
#include <sys/stat.h>
end-of-code
)
   (define jazz:file-executable?
     (c-lambda (char-string) bool
     #<<end-of-code
       struct stat sb;
       stat(___arg1, &sb);
       ___return(sb.st_mode & S_IXUSR);
end-of-code
))
   (set! jazz:file-permissions
         (c-lambda (char-string) int
     #<<end-of-code
       struct stat sb;
       stat(___arg1, &sb);
       ___return(sb.st_mode);
end-of-code
))
   (set! jazz:file-permissions-set!
         (c-lambda (char-string int) void
     #<<end-of-code
       chmod(___arg1, ___arg2);
end-of-code
))))


;;;
;;;; Forward
;;;


(jazz:define-variable jazz:compile-unit-internal)
(jazz:define-variable jazz:custom-compile-unit-internal)
(jazz:define-variable jazz:build-unit-internal)
(jazz:define-variable jazz:get-subunit-names-internal)
(jazz:define-variable jazz:hub-declaration-class)
(jazz:define-variable jazz:new-hub-declaration)
(jazz:define-variable jazz:hub-declaration-hubs)
(jazz:define-variable jazz:hub-declaration-hubs-set!)
(jazz:define-variable jazz:hub-declaration-nodes)
(jazz:define-variable jazz:hub-declaration-nodes-set!)
(jazz:define-variable jazz:module-shape)
(jazz:define-variable jazz:manifest-ignore?)
(jazz:define-variable jazz:manifest-valid?)


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
;;;; Container
;;;


(define jazz:path->container-override
  (%%make-parameter #f))


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


(%%path->container-hook-set! jazz:path->container-hook)
(%%container->path-hook-set! jazz:container->path-hook)
(%%container->id-hook-set! jazz:container->id-hook)


;;;
;;;; Source
;;;


(define jazz:wrap-datum&
  (if (jazz:gambitjazz?)
      (lambda (re x)
        (if (%%source? x)
            x
          (jazz:make-source& x (jazz:readenv->locat& re))))
    jazz:wrap-datum))

(define jazz:unwrap-datum&
  (if (jazz:gambitjazz?)
      (lambda (re x)
        (%%source-code x))
    jazz:unwrap-datum))


(define jazz:readenv->locat&
  (if (jazz:gambitjazz?)
      (lambda (re)
        (let ((container
                (or (jazz:readenv-container re)
                    (let ((c
                            (%%port-name->container
                              (%%port-name (jazz:readenv-port re)))))
                      (jazz:readenv-container-set! re c)
                      c))))
          (jazz:make-locat& container
                            (%%filepos->position
                              (jazz:readenv-filepos re))
                            (%%filepos->position
                              (%%readenv-current-filepos re)))))
    (lambda (re)
      (jazz:unimplemented))))


(define jazz:source&?
  (if (jazz:gambitjazz?)
      (lambda (src)
        (and (%%source? src)
             (%%fx= (%%vector-length src) 6)))
    (lambda (src)
      #f)))

(define jazz:make-source&
  (if (jazz:gambitjazz?)
      (lambda (code locat)
        (%%vector jazz:source1-marker
                  code
                  (if locat (%%locat-container locat) #f)
                  (if locat (jazz:locat-start& locat) #f)
                  (if locat (jazz:locat-end& locat) #f)
                  'text))
    (lambda (code locat)
      (jazz:unimplemented))))

(define jazz:sourcify-aux1&
  (if (jazz:gambitjazz?)
      (lambda (code src)
        (if (jazz:source&? src)
            (%%vector jazz:source1-marker
                      code
                      (%%vector-ref src 2)
                      (%%vector-ref src 3)
                      (%%vector-ref src 4)
                      'syntax)
          (%%vector jazz:source1-marker
                    code
                    (%%vector-ref src 2)
                    (%%vector-ref src 3))))
    (lambda (code src)
      (jazz:unimplemented))))

(define jazz:sourcify-aux2&
  (if (jazz:gambitjazz?)
      (lambda (code src)
        (if (jazz:source&? src)
            (%%vector jazz:source2-marker
                      code
                      (%%vector-ref src 2)
                      (%%vector-ref src 3)
                      (%%vector-ref src 4)
                      'syntax)
          (%%vector jazz:source2-marker
                    code
                    (%%vector-ref src 2)
                    (%%vector-ref src 3))))
    (lambda (code src)
      (jazz:unimplemented))))

(define jazz:source-locat&
  (if (jazz:gambitjazz?)
      (lambda (src)
        (if (jazz:source&? src)
            (let ((container (%%vector-ref src 2)))
              (if container
                  (jazz:make-locat& container
                                    (%%vector-ref src 3)
                                    (%%vector-ref src 4))
                #f))
          (let ((container (%%vector-ref src 2)))
            (if container
                (%%make-locat container
                              (%%vector-ref src 3))
              #f))))
    (lambda (src)
      (%%source-locat src))))

(define jazz:source-origin
  (if (jazz:gambitjazz?)
      (lambda (src)
        (%%vector-ref src 5))
    (lambda (src)
      (jazz:unimplemented))))

(define jazz:text-source?
  (if (jazz:gambitjazz?)
      (lambda (src)
        (and (jazz:source&? src)
             (%%eq? (jazz:source-origin src) 'text)))
    (lambda (src)
      (jazz:unimplemented))))


(define jazz:locat&?
  (if (jazz:gambitjazz?)
      (lambda (locat)
        (%%fx= (%%vector-length locat) 3))
    (lambda (locat)
      (jazz:unimplemented))))

(define jazz:make-locat&
  (if (jazz:gambitjazz?)
      (lambda (container start end)
        (%%vector container start end))
    (lambda (container start end)
      (jazz:unimplemented))))

(define jazz:locat-start&
  (lambda (locat)
    (%%locat-position locat)))

(define jazz:locat-end&
  (if (jazz:gambitjazz?)
      (lambda (locat)
        (let ((container (%%vector-ref locat 0)))
          (if (%%source? container)
              (jazz:locat-end& (%%source-locat container))
            (%%vector-ref locat 2))))
    (lambda (locat)
      (%%locat-position locat))))


(if (jazz:gambitjazz?)
    (begin
      (jazz:wrap-datum-set! jazz:wrap-datum&)
      (jazz:unwrap-datum-set! jazz:unwrap-datum&)
      (jazz:sourcify-aux1-set! jazz:sourcify-aux1&)
      (jazz:sourcify-aux2-set! jazz:sourcify-aux2&)))


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
;;;; Exit Jobs
;;;


(define (jazz:make-exit-jobs-safe)
  (let ((lst (jazz:fifo->list (jazz:exit-jobs))))
    (jazz:clear-exit-jobs!)
    (for-each jazz:add-safe-exit-job! lst)))


(define (jazz:add-safe-exit-job! thunk)
  (jazz:add-exit-job!
    (jazz:safe-exit-job thunk)))


(define (jazz:safe-exit-job thunk)
  (lambda ()
    (with-exception-catcher
      (lambda (exc)
        (let ((dir (or jazz:jazz-settings-directory (%%string-append (jazz:home-directory) "/.jazz/"))))
          (jazz:create-directory dir)
          (call-with-output-file (%%string-append dir "exit.exception")
            (lambda (port)
              (write exc port)
              (newline port)
              (newline port)
              (jazz:debug-port-setup-width port)
              (display-exception exc port)
              (newline port)
              (newline port)
              (continuation-capture
                (lambda (k)
                  (display-continuation-backtrace k port #f #f 500 500)))
              (force-output port))))
        (jazz:clear-exit-jobs!)
        (jazz:exit 1))
      thunk)))


;;;
;;;; Library
;;;


(define (jazz:library-main)
  (jazz:process-settings)
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
                      (jazz:with-numbered-pathname (string-append (jazz:product-library-name-base package (%%symbol->string product-name)) "." jazz:Library-Extension) #f 1
                        (lambda (filename exists?)
                          (if exists?
                              (add-library product-name filename))))))
                  products))))
  
  ; load the libraries found
  (jazz:iterate-table libraries
    (lambda (product-name library-filename)
      (let ((pathname (path-normalize library-filename)))
        (jazz:load-binary pathname #t)))))


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
          (let ((load-proc (lambda () (jazz:load-required-module unique-module-name))))
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
  
  (define (expression-argument arg)
    (if (%%string? arg)
        (call-with-input-string arg read)
      arg))
  
  ;; c -> compile
  ;; e -> eval
  ;; f -> force
  ;; g -> gambit
  ;; k -> check
  ;; l -> load
  ;; p -> parse
  ;; r -> run
  ;; s -> sourcify
  ;; t -> test
  ;; v -> version
  ;; x -> expand
  ;; y -> verify
  (jazz:with-quit
    (lambda ()
      (jazz:split-command-line (jazz:command-arguments)
                               '("v" "version" "nosource" "debug" "f" "force" "force-outlines" "sweep" "worker" "reporting" "keep-c" "track-scheme" "expansion" "gvm" "emit" "dry" "g" "gambit")
                               '("build-repository" "jazz-repository" "repositories" "dependencies" "recompile-references" "e" "eval" "i" "interpret" "l" "load" "t" "test" "r" "run" "update" "make" "build" "download" "install" "deploy" "p" "parse" "s" "sourcify" "shape" "x" "expand" "k" "check" "y" "verify" "c" "compile" "report" "target" "d" "debugger" "link" "j" "jobs" "port" "load-feedback" "load-expected" "m" "module" "dialect" "listen")
                               missing-argument-for-option
        (lambda (commands options remaining)
          (let ((version? (or (jazz:find-option "v" options) (jazz:find-option "version" options)))
                (nosource? (jazz:find-option "nosource" options))
                (debug? (jazz:find-option "debug" options))
                (force? (or (jazz:find-option "f" options) (jazz:find-option "force" options)))
                (sweep? (jazz:find-option "sweep" options))
                (worker? (jazz:find-option "worker" options))
                (keep-c? (jazz:find-option "keep-c" options))
                (track-scheme? (jazz:find-option "track-scheme" options))
                (expansion? (jazz:find-option "expansion" options))
                (gvm? (jazz:find-option "gvm" options))
                (emit? (jazz:find-option "emit" options))
                (dry? (jazz:find-option "dry" options))
                (force-outlines? (jazz:find-option "force-outlines" options))
                (gambit? (or (jazz:find-option "g" options) (jazz:find-option "gambit" options)))
                (build-repository (jazz:find-option "build-repository" options))
                (jazz-repository (jazz:find-option "jazz-repository" options))
                (repositories (jazz:find-option "repositories" options))
                (dependencies (jazz:find-option "dependencies" options))
                (recompile-references (expression-argument (jazz:find-option "recompile-references" options)))
                (ev (or (jazz:find-option "e" options) (jazz:find-option "eval" options)))
                (load (or (jazz:find-option "l" options) (jazz:find-option "load" options)))
                (interpret (or (jazz:find-option "i" options) (jazz:find-option "interpret" options)))
                (test (or (jazz:find-option "t" options) (jazz:find-option "test" options)))
                (run (or (jazz:find-option "r" options) (jazz:find-option "run" options)))
                (update (jazz:find-option "update" options))
                (make (jazz:find-option "make" options))
                (build (jazz:find-option "build" options))
                (download (jazz:find-option "download" options))
                (install (jazz:find-option "install" options))
                (deploy (jazz:find-option "deploy" options))
                (parse (or (jazz:find-option "p" options) (jazz:find-option "parse" options)))
                (sourcify (or (jazz:find-option "s" options) (jazz:find-option "sourcify" options)))
                (shape (jazz:find-option "shape" options))
                (expand (or (jazz:find-option "x" options) (jazz:find-option "expand" options)))
                (check (or (jazz:find-option "k" options) (jazz:find-option "check" options)))
                (verify (or (jazz:find-option "y" options) (jazz:find-option "verify" options)))
                (compile (or (jazz:find-option "c" options) (jazz:find-option "compile" options)))
                (report (jazz:find-option "report" options))
                (reporting? (jazz:find-option "reporting" options))
                (target (symbol-argument (jazz:find-option "target" options)))
                (debugger (or (jazz:find-option "d" options) (jazz:find-option "debugger" options)))
                (link (symbol-argument (jazz:find-option "link" options)))
                (jobs (number-argument (or (jazz:find-option "j" options) (jazz:find-option "jobs" options))))
                (port (number-argument (jazz:find-option "port" options)))
                (load-feedback (number-argument (jazz:find-option "load-feedback" options)))
                (load-expected (number-argument (jazz:find-option "load-expected" options)))
                (module (symbol-argument (or (jazz:find-option "m" options) (jazz:find-option "module" options))))
                (dialect (symbol-argument (jazz:find-option "dialect" options))))
            (define (setup-kernel)
              (if jazz:kernel-install
                  (let ((gambitdir (jazz:absolutize-directory jazz:kernel-install jazz:gambit-dir)))
                    (if (and gambitdir (file-exists? gambitdir))
                        (jazz:set-gambitdir! gambitdir))))
              (jazz:allow-inner-global-define?-set! #t)
              (set! jazz:debugger debugger)
              (if load-feedback
                  (jazz:load-feedback-setup (open-tcp-client load-feedback) load-expected))
              (if nosource?
                  (set! jazz:kernel-source-access? #f))
              (jazz:process-settings)
              (jazz:make-exit-jobs-safe))
            
            #; ;; dynamic-dependencies
            (define (locate-dependencies root-path)
              (and root-path
                   (let ((dynamic-file (%%string-append root-path ".dependencies")))
                     (and (file-exists? dynamic-file)
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
              (jazz:setup-kernel-source)
              (setup-kernel)
              (jazz:process-buildini #t)
              (setup-repositories make?)
              (set! jazz:link (or link (jazz:build-link)))
              (set! jazz:link-options (jazz:parse-link jazz:link))
              (set! jazz:jobs jobs)
              (set! jazz:recompile-references recompile-references)
              (if (not jazz:kernel-interpreted?)
                  (jazz:disable-crash-window))
              (if (or debug? (%%eqv? jobs 0) recompile-references dry? force-outlines?)
                  (jazz:debug-build? #t))
              (if keep-c?
                  (set! jazz:compile-options (%%cons 'keep-c jazz:compile-options)))
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
              (if force-outlines?
                  (jazz:force-outlines? #t))
              (if report
                  (jazz:setup-report report))
              (if reporting?
                  (jazz:setup-reporting))
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
                        (jazz:load-unit 'jazz)
                        (jazz:load-unit 'jazz.dialect.syntax)
                        (jazz:load-unit 'core.unit.build)
                        (jazz:load-unit 'scheme.syntax-rules)
                        (jazz:load-unit 'jazz.core)
                        (jazz:load-unit 'jazz.foreign)
                        (jazz:load-unit 'jazz.foreign.syntax)
                        (let ((target-platform (jazz:get-configuration-platform configuration))
                              (target-processor (jazz:get-configuration-processor configuration)))
                          (if (%%eq? target-platform 'ios)
                              (%%cond-expand-features (jazz:remove 'cocoa (%%cond-expand-features))))
                          (%%cond-expand-features (append (%%cond-expand-features) (list target-platform)))
                          (if target-processor
                              (%%cond-expand-features (append (%%cond-expand-features) (list target-processor)))))
                        (jazz:TARGET-HACK?-set! #t)
                        (let ((old jazz:Build-Repository))
                          ;; quick hack around the fact that the kernel doesn't know the directory from which it was built
                          ;; this directory would be the project or solution or ... when I do a complete overhaul to all this
                          (define (determine-root)
                            (if (jazz:string-ends-with? jazz:kernel-source "/jazz/")
                                (substring jazz:kernel-source 0 (- (string-length jazz:kernel-source) (string-length "jazz/")))
                              jazz:kernel-source))
                          
                          (set! jazz:Build-Repository (jazz:make-repository 'Build "lib" (%%string-append (determine-root) (jazz:get-configuration-destination configuration)) binary?: #t dynamic?: #t))
                          (set! jazz:Repositories (%%append (jazz:remove old jazz:Repositories) (%%list jazz:Build-Repository))))
                        (set! jazz:*binary-packages-cache* (%%make-table test: eq?)))))))
            
            (define (setup-dowload)
              (setup-build))
            
            (define (setup-install)
              (setup-build))
            
            (define (setup-deploy)
              (setup-build))
            
            (define (run-scripts lst)
              (let iter ((scan lst))
                   (if (%%not (%%null? scan))
                       (let ((arg (%%car scan)))
                         (if (%%not (jazz:option? arg))
                             (begin
                               (run-script arg)
                               (iter (%%cdr scan))))))))
            
            (define (run-script arg)
              (let ((path (add-jazz arg)))
                (if (file-exists? path)
                    (jazz:load-script path)
                  (jazz:error "Unable to find script {s}" path))))
            
            (define (add-jazz path)
              (if (jazz:pathname-extension path)
                  path
                (jazz:add-extension path "jazz")))
            
            (define (show-version)
              (%%write-string "Gambit" jazz:stdout-port)
              (%%write-string " " jazz:stdout-port)
              (%%write-string (system-version-string) jazz:stdout-port)
              (%%write-string " " jazz:stdout-port)
              (%%write (system-stamp) jazz:stdout-port)
              (%%write-string " " jazz:stdout-port)
              (%%write-string (system-type-string) jazz:stdout-port)
              (%%write-string " " jazz:stdout-port)
              (%%write (configure-command-string) jazz:stdout-port)
              (%%newline jazz:stdout-port))
            
            (cond (version?
                   (show-version))
                  (ev
                   (setup-runtime)
                   (eval (call-with-input-string ev read)))
                  (load
                   (setup-runtime)
                   (jazz:load-unit (%%string->symbol load)))
                  (interpret
                   (setup-runtime)
                   (jazz:setup-script)
                   (cond-expand
                     (mac
                      ;; hack around open setting the current directory arbitrarily to /
                      (if (and (%%equal? (current-directory) "/")
                               (%%not (jazz:string-starts-with? interpret "/")))
                          (let ((path (add-jazz (%%string-append jazz:kernel-bundle-install interpret))))
                            (if (file-exists? path)
                                (run-script path)
                              ;; there will be no controlling terminal so no error possible
                              (let ((port (console-port)))
                                (display (jazz:format "Unable to find script {s}" path) port)
                                (newline port)
                                (force-output port)
                                (exit))))
                        (run-script interpret)))
                     (else
                      (run-script interpret))))
                  (test
                   (setup-runtime)
                   (jazz:test-product (%%string->symbol test)))
                  (run
                   (setup-runtime)
                   (jazz:run-product (%%string->symbol run)))
                  (jazz:product
                   (setup-runtime)
                   (jazz:run-product jazz:product))
                  (parse
                   (setup-build)
                   ((jazz:global-ref 'jazz:parse-source) (%%string->symbol parse)))
                  (sourcify
                   (setup-build)
                   ((jazz:global-ref 'jazz:expand-source) (%%string->symbol sourcify)))
                  (shape
                   (setup-build)
                   ((jazz:global-ref 'jazz:shape-unit) (%%string->symbol shape)))
                  (expand
                   (setup-build)
                   ((jazz:global-ref 'jazz:expand-to) (%%string->symbol expand)))
                  (check
                   (setup-build)
                   ((jazz:global-ref 'jazz:check-unit) (%%string->symbol check)))
                  (verify
                   (setup-build)
                   ((jazz:global-ref 'jazz:verify-unit) (%%string->symbol verify)))
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
                  (download
                   (setup-dowload)
                   (jazz:download-product (%%string->symbol download)))
                  (install
                   (setup-install)
                   (jazz:install-product (%%string->symbol install)))
                  (deploy
                   (setup-deploy)
                   (jazz:deploy-product (%%string->symbol deploy)))
                  ((or (%%not (%%null? commands))
                       (%%not (%%null? remaining)))
                   (setup-runtime)
                   (jazz:setup-script)
                   (run-scripts (%%append commands remaining)))
                  (else
                   (if debug?
                       (setup-build)
                     (setup-runtime))
                   (jazz:repl-main gambit? module dialect)))))))))


;;;
;;;; Quit
;;;


(define jazz:quit-marker
  '(quit))


(define jazz:quit-thread
  #f)


(define (jazz:with-quit thunk)
  (%%continuation-capture
    (lambda (cont)
      (set! jazz:quit-thread (current-thread))
      (let ((previous-handler (current-exception-handler)))
        (with-exception-handler
          (lambda (exc)
            (if (and (%%pair? exc) (%%eq? (%%car exc) jazz:quit-marker))
                (let ((exit-status (%%cdr exc)))
                  (%%continuation-graft cont
                    (lambda ()
                      (exit exit-status))))
              (previous-handler exc)))
          thunk)))))


;; make quit overridable as a temporary hack around
;; libgit2 corrupting exiting process on some windows
(define jazz:quit
  (lambda (#!optional (status 0))
    (define (raise-quit)
      (raise (%%cons jazz:quit-marker status)))
    
    (if (%%eq? (%%current-thread) jazz:quit-thread)
        (raise-quit)
      (##thread-int! jazz:quit-thread raise-quit))))

(define (jazz:quit-set! proc)
  (set! jazz:quit proc))


;;;
;;;; REPL
;;;


(define jazz:expansion-module
  'terminal)

(define jazz:expansion-dialect
  'jazz)


(define (jazz:repl-main #!optional (gambit? #f) (module #f) (dialect #f))
  (jazz:setup-readtable)
  (jazz:setup-expansion-hook)
  (if gambit?
      (set! jazz:expansion-module #f))
  (if module
      (set! jazz:expansion-module module))
  (if dialect
      (if (eq? dialect 'none)
          (set! jazz:expansion-module #f)
        (set! jazz:expansion-dialect dialect)))
  (current-input-port (repl-input-port))
  (current-output-port (repl-output-port))
  (current-error-port (repl-output-port))
  (parameterize ((jazz:walk-for 'terminal)
                 (jazz:requested-unit-name jazz:expansion-module)
                 (jazz:generate-symbol-for "&")
                 (jazz:generate-symbol-context 'terminal))
    (%%repl-debug
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
  (jazz:expand-source-set!
    (lambda (src)
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
                  `',jazz:expansion-module
                  src)
              (let ((module (%%cadr code)))
                (set! jazz:expansion-module module)
                (%%sourcify
                  `',module
                  src)))
          (cond ((%%not jazz:expansion-module)
                 src)
                (else
                 (if (%%neq? jazz:expansion-module 'terminal)
                     (begin
                       (jazz:load-unit jazz:expansion-module)
                       ((jazz:global-ref 'jazz:outline-unit) jazz:expansion-module)))
                 (%%sourcify
                   `(module ,jazz:expansion-module ,jazz:expansion-dialect ,src)
                   src)))))
    src))


(jazz:add-safe-exit-job!
  (lambda ()
    (if jazz:*report-port*
        (begin
          (close-port jazz:*report-port*)
          (set! jazz:*report-port* #f))))))
