;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Kernel Build
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


(block kernel.build


;;;
;;;; Gambit
;;;


(define (jazz:gambitcomp op
                         output-dir
                         input-filenames
                         output-filename
                         cc-options
                         ld-options-prelude
                         ld-options
                         options)
  ;; bongo
  (##gambuild 'C
              op
              output-dir
              (%%memq 'verbose options)
              (##gambuild-params
                #f ;; bongo ;; base-library
                (##map (lambda (path) (##path-relative-to-dir path output-dir))
                       input-filenames)
                ;; input-filenames
                output-filename
                #f ;; bongo ;; cc
                cc-options
                ld-options-prelude
                ld-options
                #f ;; bongo ;; pkg-config
                #f ;; bongo ;; pkg-config-path
                #f ;; bongo ;; meta-info-file
                ))
  #; ;; bongo
  (if (jazz:gambitjazz?)
      (##gambcomp 'C
                  op
                  output-dir
                  input-filenames
                  output-filename
                  (%%memq 'verbose options)
                  (%%memq 'hide-console options)
                  (%%list (%%cons "CC_OPTIONS" cc-options)
                          (%%cons "LD_OPTIONS_PRELUDE" ld-options-prelude)
                          (%%cons "LD_OPTIONS" ld-options)))
    (##gambcomp 'C
                op
                output-dir
                input-filenames
                output-filename
                (%%memq 'verbose options)
                (%%list (%%cons "CC_OPTIONS" cc-options)
                        (%%cons "LD_OPTIONS_PRELUDE" ld-options-prelude)
                        (%%cons "LD_OPTIONS" ld-options)))))


;;;
;;;; Compiler
;;;


(define (jazz:compiler-name)
  (if (eq? jazz:kernel-compiler 'c++)
      "g++"
    "gcc"))


(define (jazz:compiler-extension)
  (if (eq? jazz:kernel-compiler 'c++)
      "cpp"
    "c"))


(define (jazz:language-extension language)
  (let ((compiler jazz:kernel-compiler))
    (case language
      ((objc) (case compiler ((c) "m") ((c++) "mm")))
      ((c) (case compiler ((c) "c") ((c++) "cpp")))
      (else (jazz:error "Unknown language: {s}" language)))))


;;;
;;;; Version
;;;


(define (jazz:for-each-jazz-version proc)
  (for-each proc (jazz:get-jazz-versions)))


(define (jazz:for-each-higher-jazz-version version proc)
  (let iter ((jazz-versions (jazz:get-jazz-versions)))
    (if (%%not (%%null? jazz-versions))
        (let ((jazz-version (%%car jazz-versions)))
          (if (%%fx> (jazz:get-version-number jazz-version) version)
              (begin
                (proc jazz-version)
                (iter (%%cdr jazz-versions))))))))


(define (jazz:kernel/product-needs-rebuild? version-file)
  (receive (version gambit-version gambit-stamp) (jazz:load-version-file version-file)
    (if (%%not version)
        #t
      (or (%%not (jazz:gambit-uptodate? gambit-version gambit-stamp))
          (let ((rebuild? #f))
            (jazz:for-each-higher-jazz-version version
              (lambda (jazz-version)
                (if (%%memq (jazz:get-version-rebuild jazz-version) '(kernel all))
                    (set! rebuild? #t))))
            rebuild?)))))


(define (jazz:kernel/product-architecture-needs-rebuild? version-file)
  (receive (version gambit-version gambit-stamp) (jazz:load-version-file version-file)
    (if (%%not version)
        #t
      (or (%%not (jazz:gambit-uptodate? gambit-version gambit-stamp))
          (let ((rebuild-architecture? #f))
            (jazz:for-each-higher-jazz-version version
              (lambda (jazz-version)
                (if (or (%%memq (jazz:get-version-rebuild jazz-version) '(kernel all))
                        (jazz:get-version-recompile jazz-version)
                        (jazz:get-version-recompile-references jazz-version))
                    (set! rebuild-architecture? #t))))
            rebuild-architecture?)))))


(define (jazz:build-repository-needs-sweep-impl)
  (let ((dir (%%string-append jazz:kernel-install "build/kernel/")))
    (let ((swept-file (%%string-append dir "version-swept")))
      (define (determine-version)
        (if (file-exists? swept-file)
            (receive (version gambit-version gambit-stamp) (jazz:load-version-file swept-file)
              version)
          (let ((version-file (%%string-append dir "kernel/version")))
            (if (file-exists? version-file)
                (receive (version gambit-version gambit-stamp) (jazz:load-version-file version-file)
                  version)
              #f))))
      
      (let ((version (determine-version)))
        (if (%%not version)
            #f
          (let ((sweep #f))
            (jazz:for-each-higher-jazz-version version
              (lambda (jazz-version)
                (let ((version-sweep (jazz:get-version-sweep jazz-version)))
                  (if version-sweep
                      (set! sweep version-sweep)))))
            (if sweep
                (begin
                  (jazz:create-directories dir)
                  (jazz:save-version-file swept-file)))
            sweep))))))


(define (jazz:manifest-needs-rebuild?-impl manifest)
  (let ((name (%%get-manifest-name manifest))
        (version (%%get-manifest-version manifest)))
    (let ((rebuild? #f))
      (jazz:for-each-higher-jazz-version version
        (lambda (jazz-version)
          (let ((rebuild (jazz:get-version-rebuild jazz-version))
                (recompile (jazz:get-version-recompile jazz-version)))
            (if (or (%%eq? rebuild 'all)
                    (and recompile (%%memq name recompile)))
                (set! rebuild? #t)))))
      rebuild?)))


(define (jazz:load-version-file version-file)
  (if (file-exists? version-file)
      (call-with-input-file (%%list path: version-file eol-encoding: 'cr-lf)
        (lambda (input)
          (let ((version (read input))
                (gambit-version (read input))
                (gambit-stamp (read input)))
            (values version gambit-version gambit-stamp))))
    (values #f #f #f)))


(define (jazz:save-version-file version-file #!optional (version #f))
  (call-with-output-file (list path: version-file eol-encoding: (jazz:platform-eol-encoding jazz:kernel-platform))
    (lambda (output)
      (write (or version (jazz:get-jazz-version-number)) output)
      (newline output)
      (write (system-version) output)
      (newline output)
      (write (system-stamp) output)
      (newline output))))


;;;
;;;; Kernel
;;;


(define (jazz:build-kernel-impl #!key (image #f))
  (let ((configuration (jazz:build-configuration))
        (image (or image 'executable)))
    (let ((name (jazz:get-configuration-name configuration))
          (system (jazz:get-configuration-system configuration))
          (platform (jazz:get-configuration-platform configuration))
          (compiler (jazz:get-configuration-compiler configuration))
          (processor (jazz:get-configuration-processor configuration))
          (windowing (jazz:get-configuration-windowing configuration))
          (safety (jazz:get-configuration-safety configuration))
          (optimize? (jazz:get-configuration-optimize? configuration))
          (debug-environments? (jazz:get-configuration-debug-environments? configuration))
          (debug-location? (jazz:get-configuration-debug-location? configuration))
          (debug-source? (jazz:get-configuration-debug-source? configuration))
          (debug-foreign? (jazz:get-configuration-debug-foreign? configuration))
          (track-memory? (jazz:get-configuration-track-memory? configuration))
          (mutable-bindings? (jazz:get-configuration-mutable-bindings? configuration))
          (include-compiler? (not (eq? image 'library)))
          (kernel-interpret? (jazz:get-configuration-kernel-interpret? configuration))
          (destination (jazz:get-configuration-destination configuration))
          (destination-directory (jazz:configuration-directory configuration))
          (features (jazz:get-configuration-features configuration))
          (properties (jazz:get-configuration-properties configuration)))
      (jazz:build-image #f
                        system:                system
                        platform:              platform
                        compiler:              compiler
                        processor:             processor
                        windowing:             windowing
                        safety:                safety
                        optimize?:             optimize?
                        debug-environments?:   debug-environments?
                        debug-location?:       debug-location?
                        debug-source?:         debug-source?
                        debug-foreign?:        debug-foreign?
                        track-memory?:         track-memory?
                        mutable-bindings?:     mutable-bindings?
                        include-compiler?:     include-compiler?
                        kernel-interpret?:     kernel-interpret?
                        destination:           destination
                        destination-directory: destination-directory
                        features:              features
                        properties:            properties
                        image:                 image
                        kernel?:               #t
                        console?:              #t))))


;;;
;;;; Image
;;;


(define jazz:*changed-units*
  '())


(define (jazz:get-changed-units-impl)
  jazz:*changed-units*)

(define (jazz:push-changed-units-impl unit)
  (set! jazz:*changed-units* (cons unit jazz:*changed-units*)))

(define (jazz:reset-changed-units-impl)
  (set! jazz:*changed-units* '()))


(define (jazz:build-image-impl product
          #!key
          (system jazz:kernel-system)
          (platform jazz:kernel-platform)
          (compiler jazz:kernel-compiler)
          (processor jazz:kernel-processor)
          (windowing jazz:kernel-windowing)
          (safety jazz:kernel-safety)
          (optimize? jazz:kernel-optimize?)
          (debug-environments? jazz:kernel-debug-environments?)
          (debug-location? jazz:kernel-debug-location?)
          (debug-source? jazz:kernel-debug-source?)
          (debug-foreign? jazz:kernel-debug-foreign?)
          (track-memory? jazz:kernel-track-memory?)
          (mutable-bindings? jazz:kernel-mutable-bindings?)
          (include-compiler? #f)
          (kernel-interpret? #f)
          (source jazz:kernel-source)
          (destination jazz:kernel-destination)
          (destination-directory jazz:kernel-install)
          (features jazz:kernel-features)
          (properties jazz:kernel-properties)
          (executable #f)
          (resources #f)
          (bundle #f)
          (image #f)
          (kernel? #f)
          (console? #f)
          (minimum-heap #f)
          (maximum-heap #f)
          (static-libraries '())
          (feedback jazz:feedback))
  (let ((product-name (if (%%not product) "kernel" (%%symbol->string product)))
        (gambit-library (if include-compiler? "gambitgsc" "gambit"))
        (library-image? (%%eq? image 'library))
        (static-info (jazz:static-info static-libraries)))
    (let ((image-name (if (%%not product) "jazz" product-name))
          (gambit-dir (path-normalize "~~/"))
          (source-dir (jazz:relativise-directory "./" "./" source))
          (build-dir (if product (%%get-repository-directory jazz:Build-Repository) destination-directory))
          (kernel-dir (string-append destination-directory "build/kernel/"))
          (product-dir (string-append destination-directory "build/products/" product-name "/"))
          (image-dir (if (and bundle (eq? windowing 'cocoa) (not library-image?))
                         (%%string-append destination-directory bundle ".app" "/Contents/MacOS/")
                       destination-directory))
          (static-units (%%car static-info))
          (static-options (%%cadr static-info))
          (static-languages (%%cddr static-info)))
      (define mac?
        (eq? platform 'mac))
      
      (define ios?
        (and (jazz:build-configuration) (eq? (jazz:get-configuration-platform (jazz:build-configuration)) 'ios)))
      
      (define ios-processor
        (and ios? (or (jazz:get-configuration-processor (jazz:build-configuration)) 'x86)))
      
      (define ios-architecture
        (and ios? (case ios-processor
                    ((x86) 'x86_64)
                    (else 'arm64))))
      
      (define ios-sysroot
        (and ios? (case ios-architecture
                ((x86_64) "/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneSimulator.platform/Developer/SDKs/iPhoneSimulator.sdk")
                (else "/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS.sdk"))))
      
      (define ios-custom-cc
        (and ios? "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/clang"))
      
      (define ios-custom-cc-options
        (and ios? (list "-arch" (symbol->string ios-architecture) "-fmessage-length=0" "-I/Users/magnan/Downloads/couchbase-lite-ios-enterprise_1-3/CouchbaseLite.framework/Headers" "-fdiagnostics-show-note-include-stack" "-fmacro-backtrace-limit=0" "-Wno-c++11-compat-deprecated-writable-strings" "-fmodules" "-gmodules"
                    "-Wnon-modular-include-in-framework-module" "-Werror=non-modular-include-in-framework-module" "-Wno-trigraphs" "-fpascal-strings" "-O0"
                    "-fno-common" "-fno-objc-arc" "-Wno-missing-field-initializers" "-Wno-missing-prototypes" "-Werror=return-type" "-Wunreachable-code"
                    "-Werror=deprecated-objc-isa-usage" "-Werror=objc-root-class" "-Wno-missing-braces" "-Wparentheses" "-Wswitch" "-Wempty-body"
                    "-Wconditional-uninitialized" "-Wno-unknown-pragmas" "-Wno-shadow" "-Wno-four-char-constants" "-Wno-conversion" "-Wconstant-conversion"
                    "-Wint-conversion" "-Wbool-conversion" "-Wenum-conversion" "-Wno-newline-eof"
                    "-isysroot" ios-sysroot "-fasm-blocks" "-fstrict-aliasing" "-Wdeprecated-declarations" "-mios-simulator-version-min=9.0" "-Wno-sign-conversion")))
      
      (define ios-gambit-include-dir
        (path-expand "~~ios/include"))
      
      (define ios-gambit-lib-dir
        (path-expand "~~ios/lib"))
      
      (define (source-file path)
        (%%string-append source-dir path))
      
      (define (build-file path)
        (%%string-append build-dir path))
      
      (define (dest-file path)
        (%%string-append destination-directory path))
      
      (define (kernel-file path)
        (%%string-append kernel-dir path))
      
      (define (product-file path)
        (%%string-append product-dir path))
      
      (define (print line output)
        (display line output)
        (newline output))
      
      (define (feedback-message fmt-string . rest)
        (if feedback
            (apply feedback fmt-string rest)))
      
      (define (compile-unit rebuild? name dir output)
        (let ((src (string-append dir name ".scm"))
              (dst (string-append output name "." (jazz:compiler-extension)))
              (digest (string-append output name "." jazz:Digest-Extension))
              (mnf (string-append output name "." jazz:Manifest-Extension)))
          (let ((hash-changed? (%%not (jazz:manifest-uptodate? src (jazz:load-updated-manifest name digest mnf src)))))
            (if (or rebuild? hash-changed? (%%not (file-exists? dst)))
                (let ((path (%%string-append dir name))
                      (options `(,@(if debug-environments? '(debug-environments) '())
                                 ,@(if debug-location? '(debug-location) '())
                                 ,@(if debug-source? '(debug-source) '())
                                 ,@(if jazz:enable-track-scheme? '(track-scheme) '()))))
                  ;; standardize path as it will be the path stored in debugging information
                  (let ((standardized-path (jazz:pathname-standardize (path-normalize path))))
                    (jazz:push-changed-units path)
                    (feedback-message "; compiling {a}..." path)
                    (if (not (jazz:dry-run?))
                        (begin
                          (parameterize ((jazz:walk-for 'compile))
                            (compile-file-to-target standardized-path options: options output: dst))
                          (if ios?
                              (let ((custom-cc-options ios-custom-cc-options))
                                (jazz:call-process
                                  (list
                                    path: ios-custom-cc
                                    arguments: `(,@custom-cc-options ,(%%string-append "-I" ios-gambit-include-dir) "-D___DYNAMIC" "-c" "-o" ,(string-append output name ".o") ,dst))))
                            (let ((cc-options (case platform
                                                ((mac) (case processor
                                                         ((silicon) "-D___DYNAMIC -mmacosx-version-min=11")
                                                         (else "-D___DYNAMIC -mmacosx-version-min=10.12")))
                                                (else "-D___DYNAMIC"))))
                              ;; reduce long compilation time for purely syntactic files
                              (let ((cc-options (if (or (equal? name "primitives")
                                                        (equal? name "unsafe"))
                                                    (string-append "-O0 -U___SINGLE_HOST " cc-options)
                                                  cc-options)))
                                (compile-file dst options: (%%cons 'obj options) cc-options: cc-options))))
                          (jazz:update-manifest-compile-time name digest mnf src #f))))
                  #t)
              #f))))
      
      (define (with-version-file version-file proc)
        (let ((rebuild? (jazz:kernel/product-needs-rebuild? version-file))
              (rebuild-architecture? (jazz:kernel/product-architecture-needs-rebuild? version-file))
              (was-touched? #f))
          (define (touch)
            (if (file-exists? version-file)
                (delete-file version-file))
            (set! was-touched? #t))
          
          (define (touched?)
            was-touched?)
          
          (proc rebuild? rebuild-architecture? touch touched?)
          (if (or was-touched? (%%not (file-exists? version-file)))
              (jazz:save-version-file version-file))
          was-touched?))
      
      (define (kernel-seconds)
        (let ((version-file (kernel-file "version")))
          (if (file-exists? version-file)
              (jazz:file-last-modification-seconds version-file)
            #f)))
      
      ;;;
      ;;;; Kernel
      ;;;
      
      (define (build-kernel)
        (let ((version-file (kernel-file "version")))
          (let ((version-swept-file (kernel-file "version-swept")))
            (if (%%not (file-exists? version-swept-file))
                (receive (version gambit-version gambit-stamp) (jazz:load-version-file version-file)
                  (jazz:save-version-file version-swept-file version))))
          (with-version-file version-file
            (lambda (rebuild? rebuild-architecture? touch touched?)
              (compile-kernel rebuild? rebuild-architecture? touch touched?)))))
      
      (define (compile-kernel rebuild? rebuild-architecture? touch touched?)
        (let ((architecture? (generate-architecture rebuild? rebuild-architecture?)))
          (define (compile-kernel-file name)
            (if (compile-unit rebuild? name kernel-dir kernel-dir)
                (touch)))
          
          (define (compile-source-file path name)
            (if (compile-unit rebuild?
                              name
                              (%%string-append (source-file "kernel/") path)
                              (kernel-file path))
                (touch)))
          
          (if kernel?
              (begin
                ;; load architecture
                (load (kernel-file "_architecture"))
                
                ;; reload syntax to account for architecture
                ;; no architecture-dependent syntax in header so do not load
                ;; it as it is very time consuming because of gambit's header
                ;; (load (source-file "kernel/syntax/header"))
                (load (source-file "kernel/syntax/macro"))
                (load (source-file "kernel/syntax/unsafe"))
                (load (source-file "kernel/syntax/verbose"))
                (load (source-file "kernel/syntax/declares"))
                (load (source-file "kernel/syntax/features"))
                (load (source-file "kernel/syntax/block"))
                (load (source-file "kernel/syntax/foreign"))
                (load (source-file "kernel/syntax/expansion"))
                (load (source-file "kernel/syntax/primitives"))
                (load (source-file "kernel/syntax/internal"))
                (load (source-file "kernel/syntax/structure"))
                (load (source-file "kernel/syntax/syntax"))
                (load (source-file "kernel/syntax/runtime"))
                (load (source-file "kernel/runtime/base"))
                (load (source-file "kernel/runtime/record"))
                (load (source-file "kernel/syntax/repository"))))
          
          (if architecture?
              (compile-kernel-file "_architecture"))
          
          (compile-source-file "syntax/" "header")
          (compile-source-file "syntax/" "macro")
          (compile-source-file "syntax/" "unsafe")
          (compile-source-file "syntax/" "verbose")
          (compile-source-file "syntax/" "declares")
          (compile-source-file "syntax/" "features")
          (compile-source-file "syntax/" "block")
          (compile-source-file "syntax/" "foreign")
          (compile-source-file "syntax/" "expansion")
          (compile-source-file "syntax/" "primitives")
          (compile-source-file "syntax/" "internal")
          (compile-source-file "syntax/" "structure")
          (compile-source-file "syntax/" "syntax")
          (compile-source-file "syntax/" "runtime")
          (compile-source-file "runtime/" "logging")
          (compile-source-file "runtime/" "base")
          (compile-source-file "runtime/" "record")
          (compile-source-file "syntax/" "repository")
          (compile-source-file "runtime/" "crash")
          ;; to test cross compiling
          (compile-source-file "runtime/" "configuration")
          (compile-source-file "runtime/" "embedded")
          (compile-source-file "runtime/" "version")
          (compile-source-file "runtime/" "binary")
          (compile-source-file "runtime/" "common")
          (compile-source-file "runtime/" "settings")
          (compile-source-file "runtime/" "advise")
          (if include-compiler?
              (compile-source-file "runtime/" "build"))
          (compile-source-file "runtime/" "install")
          (compile-source-file "runtime/" "digest")
          (compile-source-file "runtime/" "unit")
          (compile-source-file "runtime/" "readtable")
          (compile-source-file "runtime/" "setup")))
      
      (define (generate-architecture rebuild? rebuild-architecture?)
        (let ((file (kernel-file "_architecture.scm")))
          (if (or rebuild? rebuild-architecture? (%%not (file-exists? file)))
              (begin
                (feedback-message "; generating {a}..." file)
                (call-with-output-file (list path: file eol-encoding: (jazz:platform-eol-encoding jazz:kernel-platform))
                  (lambda (output)
                    (jazz:print-architecture #f system platform compiler processor windowing safety optimize? debug-environments? debug-location? debug-source? debug-foreign? track-memory? mutable-bindings? destination features properties output)))
                #t)
            #f)))
      
      ;;;
      ;;;; Product
      ;;;
      
      (define (build-product)
        (with-version-file (product-file "version")
          (lambda (rebuild? rebuild-architecture? touch touched?)
            (prepare-bundle rebuild?)
            (compile-product rebuild? touch touched?))))
      
      (define (prepare-bundle rebuild?)
        (if (and bundle (eq? windowing 'cocoa) (not library-image?))
            (let ((bundle-default-src (jazz:package-pathname (%%get-product-package (jazz:get-product product)) (%%string-append "bundles/" bundle "/")))
                  (bundle-prod-src (and (%%memq 'prod features) (jazz:package-pathname (%%get-product-package (jazz:get-product product)) (%%string-append "bundles/" bundle "_prod" "/"))))
                  (bundle-test-src (and (%%memq 'test features) (jazz:package-pathname (%%get-product-package (jazz:get-product product)) (%%string-append "bundles/" bundle "_test" "/"))))
                  (bundle-triage-src (and (%%memq 'triage features) (jazz:package-pathname (%%get-product-package (jazz:get-product product)) (%%string-append "bundles/" bundle "_triage" "/"))))
                  (bundle-dst (build-file (%%string-append bundle ".app" "/"))))
              (let ((bundle-src (cond ((and bundle-prod-src
                                            (file-exists? bundle-prod-src))
                                       bundle-prod-src)
                                      ((and bundle-test-src
                                            (file-exists? bundle-test-src))
                                       bundle-test-src)
                                      ((and bundle-triage-src
                                            (file-exists? bundle-triage-src))
                                       bundle-triage-src)
                                      (else
                                       bundle-default-src))))
                (if (or rebuild?
                        (not (file-exists? bundle-dst)))
                    (begin
                      (feedback-message "; preparing bundle...")
                      (if (file-exists? bundle-dst)
                          (jazz:delete-directory bundle-dst))
                      (jazz:copy-directory bundle-src bundle-dst)
                      ;; quick hack because git doesnt support empty directories
                      (let ((macos-dir (%%string-append bundle-dst "/Contents/MacOS/")))
                        (if (not (file-exists? macos-dir))
                            (create-directory macos-dir)))))))))
      
      (define (compile-product rebuild? touch touched?)
        (let ((kernel-seconds (kernel-seconds))
              (product? (generate-product rebuild?))
              (main? (generate-main rebuild?)))
          (define (compile-product-file name)
            (if (compile-unit rebuild? name product-dir product-dir)
                (touch)))
          
          (if product?
              (compile-product-file (product-filename)))
          (if main?
              (compile-product-file (main-filename)))
          
          (if (generate-resources rebuild?)
              (touch))
          
          ;;;
          ;;;; Create Link File
          ;;;
          
          (let ((link-file (link-file)))
            (if (or rebuild?
                    (%%not (file-exists? link-file))
                    (or (%%not kernel-seconds) (< (jazz:file-last-modification-seconds link-file) kernel-seconds))
                    (touched?))
                (let ((base-files `(,(kernel-file "_architecture")
                                    ,(product-file (product-filename))
                                    ,(kernel-file "syntax/header")
                                    ,(kernel-file "syntax/macro")
                                    ,(kernel-file "syntax/unsafe")
                                    ,(kernel-file "syntax/verbose")
                                    ,(kernel-file "syntax/declares")
                                    ,(kernel-file "syntax/features")
                                    ,(kernel-file "syntax/block")
                                    ,(kernel-file "syntax/foreign")
                                    ,(kernel-file "syntax/expansion")
                                    ,(kernel-file "syntax/primitives")
                                    ,(kernel-file "syntax/internal")
                                    ,(kernel-file "syntax/structure")
                                    ,(kernel-file "syntax/syntax")
                                    ,(kernel-file "syntax/runtime")
                                    ,(kernel-file "runtime/logging")
                                    ,(kernel-file "runtime/base")
                                    ,(kernel-file "runtime/record")
                                    ,(kernel-file "syntax/repository")
                                    ,(kernel-file "runtime/crash")
                                    ;; to test cross compiling
                                    ,(kernel-file "runtime/configuration")
                                    ,(kernel-file "runtime/embedded")
                                    ,(kernel-file "runtime/version")
                                    ,(kernel-file "runtime/binary")
                                    ,(kernel-file "runtime/common")
                                    ,(kernel-file "runtime/settings")
                                    ,(kernel-file "runtime/advise")
                                    ,@(if include-compiler?
                                          `(,(kernel-file "runtime/build"))
                                        '())
                                    ,(kernel-file "runtime/install")
                                    ,(kernel-file "runtime/digest")
                                    ,(kernel-file "runtime/unit")
                                    ,(kernel-file "runtime/readtable")
                                    ,(kernel-file "runtime/setup"))))
                  (let ((link-info (product-file (%%string-append image-name ".link"))))
                    (call-with-output-file (list path: link-info eol-encoding: (jazz:platform-eol-encoding jazz:kernel-platform))
                      (lambda (output)
                        (for-each (lambda (file)
                                    (write file output)
                                    (newline output))
                                  base-files))))
                  (let ((files (append base-files (list (product-file (main-filename))))))
                    (feedback-message "; creating link file...")
                    (if library-image?
                        (link-flat files output: link-file warnings?: #f)
                      (link-incremental files output: link-file base: (string-append "~~lib/_" gambit-library)))))))
          
          ;;;
          ;;;; Link Image
          ;;;
          
          (if (or rebuild?
                  (%%not (file-exists? (image-file)))
                  (or (%%not kernel-seconds) (< (jazz:file-last-modification-seconds (image-file)) kernel-seconds))
                  (touched?))
              (link-image))))
      
      (define (link-file)
        (if library-image?
            (product-file (%%string-append image-name ".o1." (jazz:compiler-extension)))
          (product-file (%%string-append image-name "." (jazz:compiler-extension)))))
      
      (define (generate-product rebuild?)
        (let ((file (product-file (%%string-append (product-filename) ".scm"))))
          (if (or rebuild? (%%not (file-exists? file)))
              (begin
                (feedback-message "; generating {a}..." file)
                (call-with-output-file (list path: file eol-encoding: (jazz:platform-eol-encoding jazz:kernel-platform))
                  (lambda (output)
                    (jazz:print-variable 'jazz:product product output)
                    (newline output)
                    (jazz:print-variable 'jazz:image (or image 'executable) output)
                    (newline output)
                    (jazz:print-variable 'jazz:built (jazz:pathname-normalize image-dir) output)
                    (newline output)
                    (jazz:print-variable 'jazz:bundle-depth (and bundle (eq? windowing 'cocoa) (not library-image?) 3) output)
                    (newline output)
                    (jazz:print-variable 'jazz:gambit-dir (if library-image? (jazz:pathname-normalize gambit-dir) (jazz:relativise-directory image-dir "./" gambit-dir)) output)
                    (newline output)
                    (jazz:print-variable 'jazz:source-built (jazz:pathname-standardize (path-normalize source)) output)
                    (newline output)
                    (jazz:print-variable 'jazz:source (if library-image? (jazz:pathname-normalize source) (jazz:determine-source-repository image-dir source)) output)
                    (newline output)
                    (jazz:print-variable 'jazz:binary-repositories (jazz:determine-binary-repositories image-dir library-image?) output)
                    (newline output)
                    (jazz:print-variable 'jazz:source-repositories (jazz:determine-source-repositories image-dir library-image?) output)
                    (newline output)
                    (jazz:print-variable 'jazz:source-access? (jazz:build-source-access?) output)
                    (newline output)
                    (jazz:print-variable 'jazz:jazzini-access? (jazz:build-jazzini-access?) output)
                    (newline output)
                    (jazz:print-variable 'jazz:windows-homedir (jazz:build-windows-homedir) output)
                    (newline output)
                    (jazz:print-variable 'jazz:single-objects? (jazz:build-single-objects?) output)
                    (newline output)
                    (jazz:print-variable 'jazz:jazz-versions (map jazz:record->vector (jazz:get-jazz-versions)) output)
                    (newline output)
                    (jazz:print-variable 'jazz:jazz-updates (map jazz:record->vector (jazz:get-jazz-updates)) output)))
                #t)
            #f)))
      
      (define (generate-main rebuild?)
        (let ((file (product-file (%%string-append (main-filename) ".scm"))))
          (if (or rebuild? (%%not (file-exists? file)))
              (begin
                (feedback-message "; generating {a}..." file)
                (call-with-output-file (list path: file eol-encoding: 'lf)
                  (lambda (output)
                    (cond (library-image?
                           (display "(jazz:verbose-kernel 'kernel.main)" output)
                           (newline output)
                           (newline output)
                           (display "(jazz:library-main)" output)
                           (newline output))
                          (else
                           (display "#!gsi -:darR,t8,f8,-8" output)
                           (if minimum-heap
                               (begin
                                 (display ",m" output)
                                 (display minimum-heap output)))
                           (if maximum-heap
                               (begin
                                 (display ",h" output)
                                 (display maximum-heap output)))
                           (newline output)
                           (newline output)
                           (display "(jazz:verbose-kernel 'kernel.main)" output)
                           (newline output)
                           (newline output)
                           (display "(define (jazz:main)" output)
                           (newline output)
                           (display "  (jazz:executable-main)" output)
                           (newline output)
                           (display "  (jazz:exit-cleanup))" output)
                           (newline output)
                           (newline output)
                           (display "(jazz:main-set! jazz:main)" output)
                           (newline output)))))
                #t)
            #f)))
      
      (define (product-filename)
        (if library-image?
            "_libproduct"
          "_product"))
      
      (define (main-filename)
        (if library-image?
            "_libmain"
          "_main"))
      
      (define (generate-resources rebuild?)
        (case platform
          ((windows)
           (with-resources
             (lambda (resources rc res)
               (let ((rcfile (%%string-append resources "/" rc)))
                 (if (or rebuild?
                         (jazz:file-needs-update? rcfile res))
                     (begin
                       (jazz:push-changed-units rcfile)
                       (feedback-message "; compiling {a}..." rcfile)
                       (jazz:call-process (list path: "windres" arguments: (list rc "-o" res) directory: resources))
                       #t)
                   #f)))))
          (else
           #f)))
      
      (define (resource-files)
        (case platform
          ((windows)
           (with-resources
             (lambda (resources rc res)
               (%%list (jazz:quote-pathname res platform)))))
          (else
           '())))
      
      (define (with-resources proc)
        (case platform
          ((windows)
           (let ()
             (define (split resources rcname)
               (let ((resources-prod (and (%%memq 'prod features) (%%string-append resources "_prod")))
                     (resources-test (and (%%memq 'test features) (%%string-append resources "_test")))
                     (resources-triage (and (%%memq 'triage features) (%%string-append resources "_triage"))))
                 (let ((resources (cond ((and resources-prod
                                              (file-exists? resources-prod))
                                         resources-prod)
                                        ((and resources-test
                                              (file-exists? resources-test))
                                         resources-test)
                                        ((and resources-triage
                                              (file-exists? resources-triage))
                                         resources-triage)
                                        (else
                                         resources))))
                   (let ((rc (%%string-append rcname ".rc"))
                         (res (%%string-append (jazz:pathname-normalize (product-file "")) product-name "res.o")))
                     (proc resources rc res)))))
             
             (if resources
                 (split (jazz:package-pathname (%%get-product-package (jazz:get-product product)) resources) product-name)
               (split (source-file "kernel/resources/windows") "jazz"))))
          (else
           #f)))
      
      (define (gambit-link-libraries)
        (if (%%not library-image?)
            `("-lgambit" ,(%%string-append "-l" gambit-library))
          '()))
      
      (define (link-libraries)
        (case platform
          ((windows)
           '("-lws2_32"))
          ((linux)
           (case (jazz:linux-family)
             ((bsd) '("-lm" "-lutil"))
             (else '("-lm" "-ldl" "-lutil"))))
          (else
           '())))
      
      (define (link-options)
        (if library-image?
            (case platform
              ((windows) '("-shared" "-D___DYNAMIC"))
              (else '("-bundle" "-D___DYNAMIC")))
          (case platform
            ((windows)
             (if console?
                 '("-mconsole")
               '("-mwindows")))
            ((mac)
             (case processor
               ((silicon) '("-headerpad_max_install_names" "-mmacosx-version-min=11"))
               (else '("-headerpad_max_install_names" "-mmacosx-version-min=10.12"))))
            (else
             '()))))
      
      (define (link-image)
        (let ((kernel-dir (jazz:pathname-normalize (jazz:pathname-dir (image-file))))
              (kernel-name (jazz:pathname-name (image-file)))
              (base-files `(,(kernel-file "_architecture.o")
                            ,(product-file (string-append (product-filename) ".o"))
                            ,(kernel-file "syntax/header.o")
                            ,(kernel-file "syntax/macro.o")
                            ,(kernel-file "syntax/unsafe.o")
                            ,(kernel-file "syntax/verbose.o")
                            ,(kernel-file "syntax/declares.o")
                            ,(kernel-file "syntax/features.o")
                            ,(kernel-file "syntax/block.o")
                            ,(kernel-file "syntax/foreign.o")
                            ,(kernel-file "syntax/expansion.o")
                            ,(kernel-file "syntax/primitives.o")
                            ,(kernel-file "syntax/internal.o")
                            ,(kernel-file "syntax/structure.o")
                            ,(kernel-file "syntax/syntax.o")
                            ,(kernel-file "syntax/runtime.o")
                            ,(kernel-file "runtime/logging.o")
                            ,(kernel-file "runtime/base.o")
                            ,(kernel-file "runtime/record.o")
                            ,(kernel-file "syntax/repository.o")
                            ,(kernel-file "runtime/crash.o")
                            ;; to test cross compiling
                            ,(kernel-file "runtime/configuration.o")
                            ,(kernel-file "runtime/embedded.o")
                            ,(kernel-file "runtime/version.o")
                            ,(kernel-file "runtime/binary.o")
                            ,(kernel-file "runtime/common.o")
                            ,(kernel-file "runtime/settings.o")
                            ,(kernel-file "runtime/advise.o")
                            ,@(if include-compiler?
                                  `(,(kernel-file "runtime/build.o"))
                                '())
                            ,(kernel-file "runtime/install.o")
                            ,(kernel-file "runtime/digest.o")
                            ,(kernel-file "runtime/unit.o")
                            ,(kernel-file "runtime/readtable.o")
                            ,(kernel-file "runtime/setup.o")))
              (main-file (product-file (string-append (main-filename) ".o")))
              (link-file (link-file)))
          (let ((c-files (append base-files (%%list main-file link-file))))
            (feedback-message "; linking {a}..." (if library-image? "library" "executable"))
            (jazz:create-directories kernel-dir)
            ;; static kernel
            (jazz:call-process
              (list
                path: "ar"
                arguments: `("-rcs"
                             ,(product-file (string-append image-name ".a"))
                             ,@base-files)))
            (if ios?
                (let ((custom-cc-options (cons "-bundle" ios-custom-cc-options))
                      (link-options (case compiler ((c++) '("-lc++")) (else '()))))
                  (jazz:invoke-process
                    (list
                      path: ios-custom-cc
                      arguments: `(,@custom-cc-options ,@link-options ,(%%string-append "-I" ios-gambit-include-dir) ,(%%string-append "-L" ios-gambit-lib-dir) ,@c-files "-o" ,(string-append kernel-dir "/" kernel-name)))))
              (jazz:gambitcomp
                'exe
                (jazz:pathname-normalize build-dir)
                c-files
                (string-append kernel-dir "/" kernel-name)
                (string-append "-I" (jazz:quote-pathname (path-strip-trailing-directory-separator (path-normalize "~~include")) platform))
                ""
                (jazz:join-strings `(,(string-append "-L" (jazz:quote-pathname (path-strip-trailing-directory-separator (path-normalize "~~lib")) platform))
                                     ,@(gambit-link-libraries)
                                     ,@(link-libraries)
                                     ,@(resource-files)
                                     ,@(link-options))
                                   " ")
                '()))
            (case windowing
              ((cocoa)
               (jazz:call-process
                 (list
                   path: "install_name_tool"
                   arguments: `("-add_rpath" "@executable_path/Libraries" ,(image-file))))
               ;; add gstreamer quick hack around install_name_tool to many open files
               (jazz:call-process
                 (list
                   path: "install_name_tool"
                   arguments: `("-add_rpath" "@executable_path/Libraries/gstreamer" ,(image-file))))
               (if (and bundle (not library-image?))
                   (begin
                     (jazz:call-process
                       (list
                         path: "install_name_tool"
                         arguments: `("-add_rpath" "@executable_path/../Libraries" ,(image-file))))
                     ;; add gstreamer quick hack around install_name_tool to many open files
                     (jazz:call-process
                       (list
                         path: "install_name_tool"
                         arguments: `("-add_rpath" "@executable_path/../Libraries/gstreamer" ,(image-file))))
                     (jazz:call-process
                       (list
                         path: "install_name_tool"
                         arguments: `("-add_rpath" "@executable_path/../../../Libraries" ,(image-file))))
                     ;; add gstreamer quick hack around install_name_tool to many open files
                     (jazz:call-process
                       (list
                         path: "install_name_tool"
                         arguments: `("-add_rpath" "@executable_path/../../../Libraries/gstreamer" ,(image-file))))))))
            (let ((id (getenv "JAZZ_APPLE_DEVELOPER_ID" #f)))
              (if id
                  (cond (mac?
                         (feedback-message "; signing {a}..." (if library-image? "library" "executable"))
                         (let ((entitlements (getenv "JAZZ_APPLE_ENTITLEMENTS" #f)))
                           (let ((entitlements-arguments (if (not entitlements)
                                                             '()
                                                           (list "--entitlements" entitlements))))
                             (jazz:call-process
                               (list
                                 path: "/usr/bin/codesign"
                                 arguments: `("--options" "runtime" ,@entitlements-arguments "--timestamp" "--sign" ,id ,(string-append kernel-dir "/" kernel-name)))))))
                        (ios?
                         (feedback-message "; signing {a}..." (if library-image? "library" "executable"))
                         (jazz:call-process
                           (list
                             path: "/usr/bin/codesign"
                             arguments: `("--force" "--sign" ,id "--preserve-metadata=identifier,entitlements" "--timestamp=none" ,(string-append kernel-dir "/" kernel-name)))))))))))
      
      (define (image-file)
        (cond ((and bundle (eq? windowing 'cocoa) (not library-image?))
               (build-file (%%string-append bundle ".app" "/Contents/MacOS/" (or executable image-name))))
              (library-image?
               (build-file (jazz:add-extension (or executable image-name) "o1")))
              (else
               (build-file (jazz:add-extension (or executable image-name) (jazz:executable-extension platform))))))
      
      ;;;
      ;;;; Configuration
      ;;;
      
      (define (generate-configuration)
        (let ((file (dest-file ".configuration")))
          (if (%%not (file-exists? file))
              (begin
                (jazz:feedback "; generating {a}..." file)
                (jazz:save-configuration #f system platform compiler processor windowing safety optimize? debug-environments? debug-location? debug-source? debug-foreign? track-memory? mutable-bindings? kernel-interpret? destination features properties file jazz:kernel-platform)))))
      
      ;;;
      ;;;; Kernel Interpret
      ;;;
      
      (define (print-absolute/relative-path-variable variable directory output)
        (receive (rel-dir relative?) (jazz:maybe-relativise-directory destination-directory "./" directory)
          (if relative?
              (jazz:print-expression-variable variable `(string-append install-dir ,rel-dir) output)
            (jazz:print-variable variable (jazz:pathname-standardize (path-normalize rel-dir)) output))))
      
      (define (generate-kernel-interpret)
        (let ((file (dest-file "jazz-interpret")))
          (if (%%not (file-exists? file))
              (begin
                (jazz:feedback "; generating {a}..." file)
                (call-with-output-file (list path: file eol-encoding: (jazz:platform-eol-encoding jazz:kernel-platform))
                  (lambda (output)
                    (print "#!/bin/sh" output)
                    (newline output)
                    (print "REL=$(dirname \"$0\")" output)
                    (newline output)
                    (receive (gambit-dir relative?) (jazz:maybe-relativise-directory destination-directory "./" gambit-dir)
                      (let ((relative-prefix (if relative? "$REL/" "")))
                        (print (string-append "GAM=" relative-prefix gambit-dir) output)
                        (print (string-append "GSC=" relative-prefix gambit-dir "bin/gsc") output)))
                    (print "SCM=$REL/jazz-interpret.scm" output)
                    (newline output)
                    (print "exec \"$GSC\" -:dar,=\"$GAM\" -i \"$SCM\" \"$@\"" output))))))
        (let ((file (dest-file "jazz-interpret.scm")))
          (if (%%not (file-exists? file))
              (begin
                (jazz:feedback "; generating {a}..." file)
                (call-with-output-file (list path: file eol-encoding: (jazz:platform-eol-encoding jazz:kernel-platform))
                  (lambda (output)
                    (print "#!gsc -:dar" output)
                    (newline output)
                    (newline output)
                    (print "(define install-dir" output)
                    (print "  (path-directory (path-normalize (car (command-line)))))" output)
                    (newline output)
                    (newline output)
                    (jazz:print-architecture #t system platform compiler processor windowing safety optimize? debug-environments? debug-location? debug-source? debug-foreign? track-memory? mutable-bindings? destination features properties output)
                    (newline output)
                    (jazz:print-variable 'jazz:kernel-interpreted? #t output)
                    (newline output)
                    (jazz:print-variable 'jazz:product #f output)
                    (newline output)
                    (jazz:print-variable 'jazz:image (or image 'executable) output)
                    (newline output)
                    (jazz:print-expression-variable 'jazz:built 'install-dir output)
                    (newline output)
                    (jazz:print-variable 'jazz:bundle-depth (and bundle (eq? windowing 'cocoa) (not library-image?) 3) output)
                    (newline output)
                    (print-absolute/relative-path-variable 'jazz:gambit-dir gambit-dir output)
                    (newline output)
                    (jazz:print-variable 'jazz:source-built (jazz:pathname-standardize (path-normalize source)) output)
                    (newline output)
                    (print-absolute/relative-path-variable 'jazz:source source output)
                    (newline output)
                    (jazz:print-variable 'jazz:binary-repositories (jazz:determine-binary-repositories destination-directory) output)
                    (newline output)
                    (jazz:print-variable 'jazz:source-repositories (jazz:determine-source-repositories destination-directory) output)
                    (newline output)
                    (jazz:print-variable 'jazz:source-access? (jazz:build-source-access?) output)
                    (newline output)
                    (jazz:print-variable 'jazz:jazzini-access? (jazz:build-jazzini-access?) output)
                    (newline output)
                    (jazz:print-variable 'jazz:windows-homedir (jazz:build-windows-homedir) output)
                    (newline output)
                    (jazz:print-variable 'jazz:single-objects? (jazz:build-single-objects?) output)
                    (newline output)
                    (jazz:print-variable 'jazz:jazz-updates (map jazz:record->vector (jazz:get-jazz-updates)) output)
                    (newline output)
                    (newline output)
                    (display "(load (string-append jazz:source \"kernel/boot\"))" output)
                    (newline output)
                    (display "(jazz:load-kernel #t)" output)
                    (newline output)
                    (display "(jazz:executable-main)" output)
                    (newline output)))))))
      
      (jazz:invoke-build-setup platform safety optimize? source destination properties)
      
      (jazz:create-directories product-dir feedback: feedback)
      (jazz:create-directories (kernel-file "syntax/") feedback: feedback)
      (jazz:create-directories (kernel-file "runtime/") feedback: feedback)
      
      (if kernel?
          (generate-configuration))
      
      (build-kernel)
      (build-product)
      
      (if kernel-interpret?
          (generate-kernel-interpret)))))


;;;
;;;; Utilities
;;;


(define (jazz:invoke-build-setup platform safety optimize? source destination properties)
  (let ((proc (jazz:build-setup)))
    (if proc
        (proc platform safety optimize? source destination properties)
      (let ((binaries? (jazz:getf properties binaries?: #f)))
        (jazz:build-source-access? (jazz:getf properties source-access?: (not binaries?)))
        (jazz:build-single-objects? (jazz:getf properties single-objects?: binaries?))))))


(define (jazz:determine-binary-repositories destination-directory #!optional (absolutize? #f))
  (jazz:determine-repositories destination-directory
    (jazz:build-binary-repositories)
    absolutize?))


(define (jazz:determine-source-repositories destination-directory #!optional (absolutize? #f))
  (jazz:determine-repositories destination-directory
    (jazz:repositories)
    absolutize?))


(define (jazz:determine-repositories destination-directory repositories #!optional (absolutize? #f))
  (if repositories
      (jazz:collect (lambda (path)
                      (jazz:determine-source-repository destination-directory path absolutize?))
                    (jazz:split-string repositories #\;))
    '()))


(define (jazz:determine-source-repository destination-directory path #!optional (absolutize? #f))
  (if (or absolutize? (jazz:absolutize-sources?))
      (jazz:absolutize-directory "./" path)
    (jazz:relativise-directory destination-directory "./" path)))


;;;
;;;; Pkg Config
;;;


(define (jazz:pkg-config what libname #!key (return-status? #f))
  (let ((string-port (open-output-string))
        (process-port (open-process (%%list path: "pkg-config" arguments: (%%list what libname)))))
    (let ((status (process-status process-port)))
      (if return-status?
          status
        (if (%%fx= status 0)
            (begin
              (jazz:pipe-no-return process-port string-port)
              (get-output-string string-port))
          (jazz:error "failed"))))))

(define (jazz:pkg-config-exists? libname)
  (= (jazz:pkg-config "--exists" libname return-status?: #t) 0))

(define (jazz:pkg-config-version libname)
  (jazz:pkg-config "--modversion" libname))

(define (jazz:pkg-config-cflags libname)
  (jazz:pkg-config "--cflags" libname))

(define (jazz:pkg-config-libs libname)
  (jazz:pkg-config "--libs" libname))

(define (jazz:pipe-no-return input output)
  (declare (proper-tail-calls))
  (let iterate ()
    (let ((c (read-char input)))
      (if (%%not (or (eof-object? c) (%%eq? #\newline c)))
          (begin
            (write-char c output)
            (iterate))))))


;;;
;;;; Static
;;;


(define (jazz:static-info library-names)
  (let ((units '())
        (ld-options '())
        (unit-language '()))
    
    (define (remove-duplicates list)
      (map car (table->list (list->table (map (lambda (x) (%%cons x #f)) list)))))
    
    (define (process-units descriptor library-name)
      (let ((update (jazz:cond-expanded-product-descriptor-update library-name descriptor)))
        (let ((sub-units (remove-duplicates (%%apply append (map jazz:get-subunit-names update)))))
          (set! units (append units sub-units)))))
    
    (define (process-options descriptor library-options)
      (if library-options
          (let ()
            (define (add-language unit-name language)
              (set! unit-language (cons (cons unit-name language) unit-language)))
            
            (set! ld-options (append ld-options (let ((ld-options (library-options descriptor add-language)))
                                                  (if (string? ld-options)
                                                      (jazz:split-string ld-options #\space)
                                                    ld-options)))))))
    
    (for-each (lambda (library-name)
                (let ((product (jazz:get-product library-name)))
                  (let ((descriptor (%%get-product-descriptor product))
                        (library-options (%%get-product-library-options product)))
                    (process-units descriptor library-name)
                    (process-options descriptor library-options))))
              library-names)
    (cons units (cons ld-options unit-language))))


(define (jazz:make-static-loader file library units)
  (call-with-output-file (list path: file eol-encoding: (jazz:platform-eol-encoding jazz:kernel-platform))
    (lambda (port)
      (display (string-append "(jazz:register-image-units '" (%%symbol->string library) " '(") port)
      (newline port)
      (for-each (lambda (unit-name)
                  (jazz:with-unit-resources unit-name #f
                    (lambda (src obj bin lib obj-uptodate? bin-uptodate? lib-uptodate? manifest)
                      (display (string-append "  (" (%%symbol->string unit-name) " ") port)
                      (write (%%get-manifest-compile-time-hash manifest) port)
                      (display ")" port)
                      (newline port))))
                units)
      (display "))" port)
      (newline port))))


;;;
;;;; Library
;;;


(define (jazz:build-library-impl product-name descriptor
          #!key
          (options '())
          (platform jazz:kernel-platform)
          (destination-directory jazz:kernel-install)
          (feedback jazz:feedback))
  (define (feedback-message fmt-string . rest)
    (if feedback
        (apply feedback fmt-string rest)))
  
  (define (link-options)
    (define (platform-options link-options platform)
      (let ((platform-options-pair (%%assq platform link-options)))
        (if (%%pair? platform-options-pair)
            (%%cdr platform-options-pair)
          #f)))
    
    (define (expand-link-option opt)
      (define (expand-libdir dir)
        (define (prefix p s) ; if p is a prefix of s, return what follows p in s
          (let ((p-length (%%string-length p))
                (s-length (%%string-length s)))
            (if (and (%%fx>= s-length p-length)
                     (%%string=? p (substring s 0 p-length)))
                (%%substring s p-length s-length)
              #f)))
        (if (%%string? dir)
            (let* ((jazz-relative-dir (prefix "~~jazz" dir))
                   (quoted-dir
                     (if jazz-relative-dir
                         (jazz:quote-jazz-pathname jazz-relative-dir)
                       (jazz:quote-pathname dir platform))))
              (%%list (%%string-append "-L" quoted-dir)))
          (jazz:error "Ill-formed libdir parameter in product link options")))
      
      (cond ((%%string? opt)
             (%%list opt))
            (else
             (case (%%car opt)
               ((pkg-config)
                (jazz:split-string (%%apply jazz:pkg-config (%%cdr opt)) #\space))
               ((libdir)
                (%%apply expand-libdir (%%cdr opt)))
               (else '())))))
    
    (let ((link-options-pair (%%assq 'link-options options)))
      (let ((raw-options
              (or (and (%%pair? link-options-pair)
                       (or (platform-options (%%cdr link-options-pair) platform)
                           (platform-options (%%cdr link-options-pair) 'else)))
                  '())))
        (%%apply append (map expand-link-option raw-options)))))
 
  ; is the lib up-to-date according to the lib manifest?
  (define (library-manifest-uptodate? loader sub-units)
    (define digest-table (%%make-table test: eq?))
    
    (define (load-image-units-manifest)
      (if (file-exists? loader)
          (begin
            (set! jazz:register-image-units
                  (lambda (lib-name units)
                    (for-each (lambda (unit)
                                (%%table-set! digest-table (%%car unit) (%%cadr unit)))
                              units)))
            (load loader))))
    
    (define (unit-uptodate? unit-name)
      (let ((image-unit-compile-time-hash (%%table-ref digest-table unit-name #f)))
        (and image-unit-compile-time-hash
             (jazz:with-unit-resources unit-name #f
               (lambda (src obj bin lib obj-uptodate? bin-uptodate? lib-uptodate? manifest)
                 (let ((src-pathname (jazz:resource-pathname src)))
                   (let ((digest (and manifest (jazz:manifest-uptodate? src-pathname manifest))))
                     (and (%%not (jazz:manifest-needs-rebuild? manifest))
                          (%%string=? image-unit-compile-time-hash (%%get-digest-hash digest))))))))))
    
    (define (subunits-uptodate? units)
      (or (%%null? units)
          (and (unit-uptodate? (%%car units))
               (subunits-uptodate? (%%cdr units)))))
            
    (load-image-units-manifest)
    (subunits-uptodate? sub-units))
  
  (let* ((product (jazz:get-product product-name))
         (package (%%get-product-package product))
         (library-base (jazz:relocate-product-library-name-base jazz:Build-Repository package (%%symbol->string product-name)))
         (library-dir (jazz:pathname-dir library-base))
         (static-info (jazz:static-info (list product-name))))
    (jazz:with-numbered-pathname (string-append library-base "." jazz:Library-Extension) #t 1
      (lambda (library-o1 o1-exists?)
        (let* ((static-dir (string-append library-dir "static/"))
               (static-lib (string-append static-dir (%%symbol->string product-name) ".a"))
               (static-o1 (string-append static-dir (jazz:pathname-name library-o1)))
               (linkfile (string-append static-o1 "." (jazz:compiler-extension)))
               (loader (string-append static-dir jazz:Library-Manifest-Name))
               (loader-name (string-append jazz:product-uniqueness-prefix (%%symbol->string product-name)))
               (loader-s (string-append loader ".scm"))
               (loader-c (string-append loader "." (jazz:compiler-extension)))
               (loader-o (string-append loader ".o"))
               (sub-units (%%car static-info))
               (ld-options (%%cadr static-info))
               (unit-language (%%cddr static-info))
               (linked-files (map (lambda (subunit-name)
                                    (jazz:with-unit-resources subunit-name #f
                                      (lambda (src obj bin lib obj-uptodate? bin-uptodate? lib-uptodate? manifest)
                                        (let ((extension (let ((pair (%%assq subunit-name unit-language)))
                                                           (if (%%not pair)
                                                               (jazz:compiler-extension)
                                                             (jazz:language-extension (%%cdr pair))))))
                                          (string-append (jazz:resource-pathname obj) "." extension)))))
                                  sub-units)))
          (define (build-library)
            (jazz:load/create-build-package package)
            (jazz:make-static-loader loader-s product-name sub-units)
            ;; bongo
            (compile-file-to-target loader-s output: loader-c options: (list (list 'module-ref loader-name)))
            (compile-file loader-c options: '(obj) cc-options: "-D___DYNAMIC ")
            
            (jazz:call-process
              (list
                path: "ar"
                arguments: `("-rcs"
                             ,static-lib
                             ,loader-o
                             ,@(map (lambda (subunit-name)
                                      (jazz:with-unit-resources subunit-name #f
                                        (lambda (src obj bin lib obj-uptodate? bin-uptodate? lib-uptodate? manifest)
                                          (%%string-append (jazz:resource-pathname obj) ".o"))))
                                    sub-units))))
            
            (let ((link-info (string-append static-dir (%%symbol->string product-name) ".link")))
              (call-with-output-file (list path: link-info eol-encoding: (jazz:platform-eol-encoding jazz:kernel-platform))
                (lambda (output)
                  (for-each (lambda (file)
                              (write file output)
                              (newline output))
                            (%%cons loader-c linked-files)))))
            
            (if (jazz:link-libraries?)
                (begin
                  ;(feedback-message "; creating link file...")
                  (link-flat (%%cons loader-c
                                     (map (lambda (module)
                                            (cons module '((preload . #f))))
                                          linked-files))
                             output: linkfile
                             warnings?: #f)
                  (feedback-message "; linking library... ({a} units)" (%%number->string (%%length sub-units)))
                  (jazz:call-process
                    (list
                      path: (jazz:compiler-name)
                      arguments: `(,@(case platform
                                       ((windows) '("-shared" "-D___DYNAMIC"))
                                       (else '("-bundle" "-D___DYNAMIC")))
                                   ,loader-o
                                   ,@(cond-expand
                                       (windows
                                         (map (lambda (subunit-name)
                                                  (jazz:with-unit-resources subunit-name #f
                                                    (lambda (src obj bin lib obj-uptodate? bin-uptodate? lib-uptodate? manifest)
                                                      (%%string-append (jazz:resource-pathname obj) ".o"))))
                                                sub-units))
                                       (else
                                        (%%list static-lib)))
                                   ,linkfile
                                   ;; patch because of a warning when C++ is used
                                   "-w"
                                   "-o" ,static-o1
                                   ,(string-append "-I" (jazz:pathname-standardize (path-strip-trailing-directory-separator (path-normalize "~~include"))))
                                   ,(string-append "-L" (jazz:pathname-standardize (path-strip-trailing-directory-separator (path-normalize "~~lib"))))
                                   ,@ld-options
                                   ,@(link-options))))
                  ;; cleanup
                  (for-each delete-file (%%list loader-c loader-o linkfile))
                  (rename-file static-o1 library-o1)))
            #t)
          
          (jazz:create-directories static-dir)
          (or (and o1-exists? (library-manifest-uptodate? loader-s sub-units))
              (build-library)))))))


(define (jazz:print-architecture for-kernel-interpret? system platform compiler processor windowing safety optimize? debug-environments? debug-location? debug-source? debug-foreign? track-memory? mutable-bindings? destination features properties output)
  (jazz:print-variable 'jazz:kernel-interpreted? #f output)
  (newline output)
  (jazz:print-variable 'jazz:kernel-system system output)
  (newline output)
  (jazz:print-variable 'jazz:kernel-platform platform output)
  (newline output)
  (jazz:print-variable 'jazz:kernel-compiler compiler output)
  (newline output)
  (jazz:print-variable 'jazz:kernel-processor processor output)
  (newline output)
  (jazz:print-variable 'jazz:kernel-windowing windowing output)
  (newline output)
  (jazz:print-variable 'jazz:kernel-safety safety output)
  (newline output)
  (jazz:print-variable 'jazz:kernel-optimize? optimize? output)
  (newline output)
  (jazz:print-variable 'jazz:kernel-debug-environments? debug-environments? output)
  (newline output)
  (jazz:print-variable 'jazz:kernel-debug-location? debug-location? output)
  (newline output)
  (jazz:print-variable 'jazz:kernel-debug-source? debug-source? output)
  (newline output)
  (jazz:print-variable 'jazz:kernel-debug-foreign? debug-foreign? output)
  (newline output)
  (jazz:print-variable 'jazz:kernel-track-memory? track-memory? output)
  (newline output)
  (jazz:print-variable 'jazz:kernel-mutable-bindings? mutable-bindings? output)
  (newline output)
  (jazz:print-variable 'jazz:kernel-destination destination output)
  (newline output)
  (jazz:print-variable 'jazz:kernel-features features output)
  (newline output)
  (jazz:print-variable 'jazz:kernel-properties properties output)
  (newline output)
  (jazz:print-variable 'jazz:kernel-version (jazz:get-jazz-version-number) output))


(define (jazz:print-variable variable value output)
  (display "(define " output)
  (display variable output)
  (newline output)
  (display "  " output)
  (if (or (%%symbol? value)
          (list? value))
      (display "'" output))
  (write value output)
  (display ")" output)
  (newline output))


(define (jazz:print-expression-variable variable expression output)
  (display "(define " output)
  (display variable output)
  (newline output)
  (display "  " output)
  (write expression output)
  (display ")" output)
  (newline output))


;;;
;;;; Override
;;;


(jazz:define-variable-override jazz:build-repository-needs-sweep jazz:build-repository-needs-sweep-impl)
(jazz:define-variable-override jazz:manifest-needs-rebuild? jazz:manifest-needs-rebuild?-impl)
(jazz:define-variable-override jazz:get-changed-units jazz:get-changed-units-impl)
(jazz:define-variable-override jazz:push-changed-units jazz:push-changed-units-impl)
(jazz:define-variable-override jazz:reset-changed-units jazz:reset-changed-units-impl)
(jazz:define-variable-override jazz:build-kernel jazz:build-kernel-impl)
(jazz:define-variable-override jazz:build-image jazz:build-image-impl)
(jazz:define-variable-override jazz:build-library jazz:build-library-impl))
