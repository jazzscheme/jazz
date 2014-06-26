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


(block kernel.build


(jazz:kernel-declares)


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


(define (jazz:load-version-file version-file)
  (if (file-exists? version-file)
      (call-with-input-file (%%list path: version-file eol-encoding: 'cr-lf)
        (lambda (input)
          (let ((version (read input))
                (gambit-version (read input))
                (gambit-stamp (read input)))
            (values version gambit-version gambit-stamp))))
    (values #f #f #f)))


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
          (windowing jazz:kernel-windowing)
          (safety jazz:kernel-safety)
          (optimize? jazz:kernel-optimize?)
          (debug-environments? jazz:kernel-debug-environments?)
          (debug-location? jazz:kernel-debug-location?)
          (debug-source? jazz:kernel-debug-source?)
          (debug-foreign? jazz:kernel-debug-foreign?)
          (mutable-bindings? jazz:kernel-mutable-bindings?)
          (include-compiler? #f)
          (kernel-interpret? #f)
          (source jazz:kernel-source)
          (destination jazz:kernel-destination)
          (destination-directory jazz:kernel-install)
          (properties jazz:kernel-properties)
          (executable #f)
          (resources #f)
          (image #f)
          (kernel? #f)
          (console? #f)
          (minimum-heap #f)
          (maximum-heap #f)
          (feedback jazz:feedback))
  (let ((product-name (if (%%not product) "kernel" (%%symbol->string product)))
        (gambit-library (if include-compiler? "gambcgsc" "gambc"))
        (library-image? (%%eq? image 'library)))
    (let ((image-name (if (%%not product) "jazz" product-name))
          (gambit-dir (path-normalize "~~/"))
          (source-dir (jazz:relativise-directory "./" "./" source))
          (build-dir (if product (%%get-repository-directory jazz:Build-Repository) destination-directory))
          (kernel-dir (string-append destination-directory "build/kernel/"))
          (product-dir (string-append destination-directory "build/products/" product-name "/")))
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
      
      (define (compile-file rebuild? name dir output)
        (let ((src (string-append dir name ".scm"))
              (dst (string-append output name ".c"))
              (digest (string-append output name "." jazz:Digest-Extension))
              (mnf (string-append output name "." jazz:Manifest-Extension)))
          (let ((hash-changed? (%%not (jazz:manifest-uptodate? src (jazz:load-updated-manifest name digest mnf src)))))
            (if (or rebuild? hash-changed? (%%not (jazz:file-exists? dst)))
                (let ((path (%%string-append dir name))
                      (options `(,@(if debug-environments? '(debug-environments) '())
                                 ,@(if debug-location? '(debug-location) '())
                                 ,@(if debug-source? '(debug-source) '()))))
                  ;; standardize path as it will be the path stored in debugging information
                  (let ((standardized-path (jazz:pathname-standardize (path-normalize path))))
                    (jazz:push-changed-units path)
                    (feedback-message "; compiling {a}..." path)
                    (if (not (jazz:dry-run?))
                        (begin
                          (compile-file-to-target standardized-path options: options output: output)
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
              (call-with-output-file (list path: version-file eol-encoding: (jazz:platform-eol-encoding jazz:kernel-platform))
                (lambda (output)
                  (write (jazz:get-jazz-version-number) output)
                  (newline output)
                  (write (system-version) output)
                  (newline output)
                  (write (system-stamp) output)
                  (newline output))))
          was-touched?))
      
      (define (kernel-time)
        (let ((version-file (kernel-file "version")))
          (if (file-exists? version-file)
              (jazz:file-modification-time version-file)
            #f)))
      
      ;;;
      ;;;; Kernel
      ;;;
      
      (define (build-kernel)
        (with-version-file (kernel-file "version")
          (lambda (rebuild? rebuild-architecture? touch touched?)
            (compile-kernel rebuild? rebuild-architecture? touch touched?))))
      
      (define (compile-kernel rebuild? rebuild-architecture? touch touched?)
        (let ((architecture? (generate-architecture rebuild? rebuild-architecture?)))
          (define (compile-kernel-file name)
            (if (compile-file rebuild? name kernel-dir kernel-dir)
                (touch)))
          
          (define (compile-source-file path name)
            (if (compile-file rebuild?
                              name
                              (%%string-append (source-file "kernel/") path)
                              (kernel-file path))
                (touch)))
          
          (if kernel?
              (begin
                ;; load architecture
                (load (kernel-file "_architecture"))
                
                ;; load syntax
                (load (source-file "kernel/syntax/header"))
                (load (source-file "kernel/syntax/macro"))
                (load (source-file "kernel/syntax/block"))
                (load (source-file "kernel/syntax/foreign"))
                (load (source-file "kernel/syntax/expansion"))
                (load (source-file "kernel/syntax/features"))
                (load (source-file "kernel/syntax/declares"))
                (load (source-file "kernel/syntax/primitives"))
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
          (compile-source-file "syntax/" "block")
          (compile-source-file "syntax/" "foreign")
          (compile-source-file "syntax/" "expansion")
          (compile-source-file "syntax/" "features")
          (compile-source-file "syntax/" "declares")
          (compile-source-file "syntax/" "primitives")
          (compile-source-file "syntax/" "structure")
          (compile-source-file "syntax/" "syntax")
          (compile-source-file "syntax/" "runtime")
          (compile-source-file "runtime/" "logging")
          (compile-source-file "runtime/" "base")
          (compile-source-file "runtime/" "record")
          (compile-source-file "syntax/" "repository")
          (compile-source-file "runtime/" "crash")
          (compile-source-file "runtime/" "version")
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
                    (jazz:print-architecture system platform windowing safety optimize? debug-environments? debug-location? debug-source? debug-foreign? mutable-bindings? destination properties output)))
                #t)
            #f)))
      
      ;;;
      ;;;; Product
      ;;;
      
      (define (build-product)
        (with-version-file (product-file "version")
          (lambda (rebuild? rebuild-architecture? touch touched?)
            (compile-product rebuild? touch touched?))))
      
      (define (compile-product rebuild? touch touched?)
        (let ((kernel-time (kernel-time))
              (product? (generate-product rebuild?))
              (main? (generate-main rebuild?)))
          (define (compile-product-file name)
            (if (compile-file rebuild? name product-dir product-dir)
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
                    (or (%%not kernel-time) (< (jazz:file-modification-time link-file) kernel-time))
                    (touched?))
                (let ((files `(,(kernel-file "_architecture")
                               ,(product-file (product-filename))
                               ,(kernel-file "syntax/header")
                               ,(kernel-file "syntax/macro")
                               ,(kernel-file "syntax/block")
                               ,(kernel-file "syntax/foreign")
                               ,(kernel-file "syntax/expansion")
                               ,(kernel-file "syntax/features")
                               ,(kernel-file "syntax/declares")
                               ,(kernel-file "syntax/primitives")
                               ,(kernel-file "syntax/structure")
                               ,(kernel-file "syntax/syntax")
                               ,(kernel-file "syntax/runtime")
                               ,(kernel-file "runtime/logging")
                               ,(kernel-file "runtime/base")
                               ,(kernel-file "runtime/record")
                               ,(kernel-file "syntax/repository")
                               ,(kernel-file "runtime/crash")
                               ,(kernel-file "runtime/version")
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
                               ,(kernel-file "runtime/setup")
                               ,(product-file (main-filename)))))
                  (feedback-message "; creating link file...")
                  (if library-image?
                      (link-flat files output: link-file warnings?: #f)
                    (link-incremental files output: link-file base: (string-append "~~lib/_" gambit-library))))))
          
          ;;;
          ;;;; Link Image
          ;;;
          
          (if (or rebuild?
                  (%%not (file-exists? (image-file)))
                  (or (%%not kernel-time) (< (jazz:file-modification-time (image-file)) kernel-time))
                  (touched?))
              (link-image))))
      
      (define (link-file)
        (if library-image?
            (product-file (%%string-append product-name ".o1.c"))
          (product-file (%%string-append product-name ".c"))))
      
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
                    (jazz:print-variable 'jazz:built (jazz:pathname-normalize destination-directory) output)
                    (newline output)
                    (jazz:print-variable 'jazz:gambit-dir (if library-image? (jazz:pathname-normalize gambit-dir) (jazz:relativise-directory destination-directory "./" gambit-dir)) output)
                    (newline output)
                    (jazz:print-variable 'jazz:source-built (jazz:pathname-standardize (path-normalize source)) output)
                    (newline output)
                    (jazz:print-variable 'jazz:source (if library-image? (jazz:pathname-normalize source) (jazz:relativise-directory destination-directory "./" source)) output)
                    (newline output)
                    (jazz:print-variable 'jazz:binary-repositories (if library-image? #f (jazz:determine-binary-repositories destination-directory)) output)
                    (newline output)
                    (jazz:print-variable 'jazz:source-repositories (if library-image? #f (jazz:determine-source-repositories destination-directory)) output)
                    (newline output)
                    (jazz:print-variable 'jazz:source-access? (jazz:build-source-access?) output)
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
                           (display "(define (jazz:main)" output)
                           (newline output)
                           (display "  (jazz:executable-main))" output)
                           (newline output)
                           (newline output)
                           (display "(##main-set! jazz:main)" output)
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
               (let ((rc (%%string-append rcname ".rc"))
                     (res (%%string-append (jazz:pathname-normalize (product-file "")) product-name "res.o")))
                 (proc resources rc res)))
             
             (if resources
                 (split (jazz:package-pathname (%%get-product-package (jazz:get-product product)) resources) product-name)
               (split (source-file "etc/resources/windows") "jazz"))))
          (else
           #f)))
      
      (define (gambit-link-libraries)
        (if (%%not library-image?)
            `("-lgambc" ,(%%string-append "-l" gambit-library))
          '()))
      
      (define (link-libraries)
        (case platform
          ((windows)
           '("-lws2_32"))
          ((unix)
           (case (jazz:unix-family)
             ((bsd) '("-lm" "-lutil"))
             (else '("-lm" "-ldl" "-lutil"))))
          (else
           '())))
      
      (define (link-options)
        (if library-image?
            (case platform
              ((windows) '("-Wl,--large-address-aware" "-shared" "-D___DYNAMIC"))
              (else '("-bundle" "-D___DYNAMIC")))
          (case platform
            ((windows)
             (cons "-Wl,--large-address-aware"
                   (if console?
                       '("-mconsole")
                     '("-mwindows"))))
            (else
             '()))))
      
      (define (link-image)
        (let ((kernel-dir (jazz:pathname-normalize (jazz:pathname-dir (image-file))))
              (kernel-name (jazz:pathname-name (image-file)))
              (c-files `(,(kernel-file "_architecture.c")
                         ,(product-file (string-append (product-filename) ".c"))
                         ,(kernel-file "syntax/header.c")
                         ,(kernel-file "syntax/macro.c")
                         ,(kernel-file "syntax/block.c")
                         ,(kernel-file "syntax/foreign.c")
                         ,(kernel-file "syntax/expansion.c")
                         ,(kernel-file "syntax/features.c")
                         ,(kernel-file "syntax/declares.c")
                         ,(kernel-file "syntax/primitives.c")
                         ,(kernel-file "syntax/structure.c")
                         ,(kernel-file "syntax/syntax.c")
                         ,(kernel-file "syntax/runtime.c")
                         ,(kernel-file "runtime/logging.c")
                         ,(kernel-file "runtime/base.c")
                         ,(kernel-file "runtime/record.c")
                         ,(kernel-file "syntax/repository.c")
                         ,(kernel-file "runtime/crash.c")
                         ,(kernel-file "runtime/version.c")
                         ,(kernel-file "runtime/common.c")
                         ,(kernel-file "runtime/settings.c")
                         ,(kernel-file "runtime/advise.c")
                         ,@(if include-compiler?
                               `(,(kernel-file "runtime/build.c"))
                             '())
                         ,(kernel-file "runtime/install.c")
                         ,(kernel-file "runtime/digest.c")
                         ,(kernel-file "runtime/unit.c")
                         ,(kernel-file "runtime/readtable.c")
                         ,(kernel-file "runtime/setup.c")
                         ,(product-file (string-append (main-filename) ".c"))
                         ,(link-file))))
          (feedback-message "; linking {a}..." (if library-image? "library" "executable"))
          (jazz:create-directories kernel-dir)
          (##gambc-cc
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
            #f)
          (case platform
            ((windows)
             (if (jazz:build-single-objects?)
                 (jazz:obliterate-PE-timestamp (image-file) 'EXE))))))
      
      (define (image-file)
        (if library-image?
            (build-file (jazz:add-extension (or executable image-name) "o1"))
          (build-file (jazz:add-extension (or executable image-name) (jazz:executable-extension platform)))))
      
      ;;;
      ;;;; Configuration
      ;;;
      
      (define (generate-configuration)
        (let ((file (dest-file ".configuration")))
          (if (%%not (file-exists? file))
              (begin
                (jazz:feedback "; generating {a}..." file)
                (jazz:save-configuration #f system platform windowing safety optimize? debug-environments? debug-location? debug-source? debug-foreign? mutable-bindings? kernel-interpret? destination properties file jazz:kernel-platform)))))
      
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
                    (print "exec \"$GSC\" -:=\"$GAM\" -i \"$SCM\" \"$@\"" output))))))
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
                    (jazz:print-architecture system platform windowing safety optimize? debug-environments? debug-location? debug-source? debug-foreign? mutable-bindings? destination properties output)
                    (newline output)
                    (jazz:print-variable 'jazz:kernel-interpreted? #t output)
                    (newline output)
                    (jazz:print-variable 'jazz:product #f output)
                    (newline output)
                    (jazz:print-variable 'jazz:image (or image 'executable) output)
                    (newline output)
                    (jazz:print-expression-variable 'jazz:built 'install-dir output)
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
      
      (define (interpret-properties properties)
        (jazz:build-source-access? (jazz:getf properties source-access?: #t))
        (jazz:build-single-objects? (jazz:getf properties single-objects?: #f)))
      
      (jazz:invoke-build-setup platform safety optimize? source destination properties)
      (interpret-properties properties)
      
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
        (proc platform safety optimize? source destination properties))))


(define (jazz:determine-binary-repositories destination-directory)
  (jazz:determine-repositories destination-directory
    (jazz:build-binary-repositories)))


(define (jazz:determine-source-repositories destination-directory)
  (jazz:determine-repositories destination-directory
    (jazz:repositories)))


(define (jazz:determine-repositories destination-directory repositories)
  (if repositories
      (jazz:collect (lambda (path)
                      (jazz:relativise-directory destination-directory "./" path))
                    (jazz:split-string repositories #\;))
    '()))


;;;
;;;; PE-Timestamps
;;;


(define (jazz:obliterate-PE-timestamp pathname type)
  (define (get-checksum-offset)
    (let ((port (open-input-file pathname)))
      (input-port-byte-position port #x22D)
      (let ((b1 (read-u8 port)))
        (let ((b2 (read-u8 port)))
          (let ((result (+ (* #x10000 b2)
                           (*   #x100 b1)
                           (*    #x04 1))))
            (close-port port)
            result)))))
  
  (define (fill-bytes-offset port offset size byte-value)
    (output-port-byte-position port offset)
    (let loop ((i 0))
         (if (< i size)
             (begin
               (write-u8 byte-value port)
               (loop (+ i 1))))))
  
  (let ((patches `((#x88 4)
                   (#xD8 4)
                   .
                   ,(case type
                      ((DLL dll)
                       `((,(get-checksum-offset) 2)))
                      (else
                       '())))))
    (let ((dll-port (open-output-file `(path: ,pathname truncate: #f))))
      (map (lambda (patch)
             (let ((offset (car patch))
                   (size (cadr patch)))
               (fill-bytes-offset dll-port offset size 0)))
           patches)
      (close-port dll-port))))


;;;
;;;; Pkg Config
;;;


(define (jazz:pkg-config what libname)
  (let ((string-port (open-output-string))
        (process-port (open-process (%%list path: "pkg-config" arguments: (%%list what libname)))))
    (if (%%fx= (process-status process-port) 0)
        (begin
          (jazz:pipe-no-return process-port string-port)
          (get-output-string string-port))
      (jazz:error "failed"))))

(define (jazz:pkg-config-cflags libname)
  (jazz:pkg-config "--cflags" libname))

(define (jazz:pkg-config-libs libname)
  (jazz:pkg-config "--libs" libname))

(define (jazz:pkg-config-version libname)
  (jazz:pkg-config "--modversion" libname))

(define (jazz:pipe-no-return input output)
  (declare (proper-tail-calls))
  (let iterate ()
    (let ((c (read-char input)))
      (if (%%not (or (eof-object? c) (%%eq? #\newline c)))
          (begin
            (write-char c output)
            (iterate))))))


;;;
;;;; Libraries
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
  
  (define (remove-duplicates list)
    (map car (table->list (list->table (map (lambda (x) (%%cons x #f)) list)))))
  
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
  (define (library-manifest-uptodate? header sub-units)
    (define digest-table (%%make-table test: eq?))
      
    (define (load-image-units-manifest)
      (if (jazz:file-exists? header)
          (begin
            (set! jazz:register-image-units
                  (lambda (lib-name units)
                    (for-each (lambda (unit)
                                (%%table-set! digest-table (%%car unit) (%%cadr unit)))
                              units)))
            (load header))))
    
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
  
  (define (make-library-header header- library sub-units)
    (call-with-output-file (list path: header- eol-encoding: (jazz:platform-eol-encoding jazz:kernel-platform))
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
                  sub-units)
        (display "))" port)
        (newline port))))
  
  (let* ((product (jazz:get-product product-name))
         (package (%%get-product-package product))
         (update (jazz:cond-expanded-product-descriptor-update product-name descriptor))
         (library-base (jazz:relocate-product-library-name-base jazz:Build-Repository package product-name))
         (library-dir (jazz:pathname-dir library-base)))
    (jazz:with-numbered-pathname (string-append library-base "." jazz:Library-Extension) #t 1
      (lambda (library-o1 o1-exists?)
        (let* ((linkfile (string-append library-o1 ".c"))
               (header (string-append library-base "." jazz:Library-Manifest-Extension))
               (header-c (string-append header ".c"))
               (header-o (string-append header ".o"))
               (sub-units (remove-duplicates (%%apply append (map jazz:get-subunit-names update)))))
          
          (define (build-library)
            (jazz:create-build-package package)
            (make-library-header header product-name sub-units)
            (compile-file-to-target header output: header-c)
            (compile-file header-c options: '(obj) cc-options: "-D___BIND_LATE ")
            
            (feedback-message "; creating link file...")
            (link-flat (%%cons header
                               (map (lambda (subunit-name)
                                      (jazz:with-unit-resources subunit-name #f
                                        (lambda (src obj bin lib obj-uptodate? bin-uptodate? lib-uptodate? manifest)
                                          (jazz:resource-pathname obj))))
                                    sub-units))
                       output: linkfile
                       warnings?: #f)
            
            (feedback-message "; linking library... ({a} units)" (%%number->string (%%length sub-units)))
            (jazz:call-process
              (list
                path: "gcc"
                arguments: `(,@(case platform
                                 ((windows) '("-Wl,--large-address-aware" "-shared" "-D___DYNAMIC"))
                                 (else '("-bundle" "-D___DYNAMIC")))
                             ,header-o
                             ,@(map (lambda (subunit-name)
                                      (jazz:with-unit-resources subunit-name #f
                                        (lambda (src obj bin lib obj-uptodate? bin-uptodate? lib-uptodate? manifest)
                                          (%%string-append (jazz:resource-pathname obj) ".o"))))
                                    sub-units)
                             ,linkfile
                             "-o" ,library-o1
                             ,(string-append "-I" (jazz:quote-pathname (path-strip-trailing-directory-separator (path-normalize "~~include")) platform))
                             ,(string-append "-L" (jazz:quote-pathname (path-strip-trailing-directory-separator (path-normalize "~~lib")) platform))
                             ,@(link-options))))
            (case platform
              ((windows)
               (if jazz:single-objects?
                   (jazz:obliterate-PE-timestamp library-o1 'DLL))))
            (map delete-file (%%list header-c header-o linkfile))
            #t)

          (or (and o1-exists? (library-manifest-uptodate? header sub-units))
              (build-library)))))))


(define (jazz:print-architecture system platform windowing safety optimize? debug-environments? debug-location? debug-source? debug-foreign? mutable-bindings? destination properties output)
  (jazz:print-variable 'jazz:kernel-interpreted? #f output)
  (newline output)
  (jazz:print-variable 'jazz:kernel-system system output)
  (newline output)
  (jazz:print-variable 'jazz:kernel-platform platform output)
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
  (jazz:print-variable 'jazz:kernel-mutable-bindings? mutable-bindings? output)
  (newline output)
  (jazz:print-variable 'jazz:kernel-destination destination output)
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
;;;; Setup
;;;


(jazz:define-variable-override jazz:manifest-needs-rebuild? jazz:manifest-needs-rebuild?-impl)
(jazz:define-variable-override jazz:get-changed-units jazz:get-changed-units-impl)
(jazz:define-variable-override jazz:push-changed-units jazz:push-changed-units-impl)
(jazz:define-variable-override jazz:reset-changed-units jazz:reset-changed-units-impl)
(jazz:define-variable-override jazz:build-image jazz:build-image-impl)
(jazz:define-variable-override jazz:build-library jazz:build-library-impl))
