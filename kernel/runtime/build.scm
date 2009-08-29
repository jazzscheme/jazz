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
;;;; Version
;;;


(define (jazz.for-each-source-version proc)
  (for-each proc (jazz.get-source-versions)))


(define (jazz.for-each-higher-source-version version proc)
  (let iter ((source-versions (jazz.get-source-versions)))
    (if (not (null? source-versions))
        (let ((source-version (car source-versions)))
          (if (> (jazz.version-number source-version) version)
              (begin
                (proc source-version)
                (iter (cdr source-versions))))))))


(define (jazz.kernel/product-needs-rebuild? version-file)
  (receive (version gambit-version gambit-stamp) (jazz.load-version-file version-file)
    (if (not version)
        #t
      (or (not (jazz.gambit-uptodate? gambit-version gambit-stamp))
          (let ((rebuild? #f))
            (jazz.for-each-higher-source-version version
              (lambda (source-version)
                (if (memq (jazz.version-rebuild source-version) '(kernel all))
                    (set! rebuild? #t))))
            rebuild?)))))


(define (jazz.kernel/product-architecture-needs-rebuild? version-file)
  (receive (version gambit-version gambit-stamp) (jazz.load-version-file version-file)
    (if (not version)
        #t
      (or (not (jazz.gambit-uptodate? gambit-version gambit-stamp))
          (let ((rebuild-architecture? #f))
            (jazz.for-each-higher-source-version version
              (lambda (source-version)
                (if (or (memq (jazz.version-rebuild source-version) '(kernel all))
                        (jazz.version-recompile source-version))
                    (set! rebuild-architecture? #t))))
            rebuild-architecture?)))))


(define (jazz.load-version-file version-file)
  (if (file-exists? version-file)
      (call-with-input-file (list path: version-file eol-encoding: 'cr-lf)
        (lambda (input)
          (let ((version (read input))
                (gambit-version (read input))
                (gambit-stamp (read input)))
            (values version gambit-version gambit-stamp))))
    (values #f #f #f)))


(define (jazz.manifest-needs-rebuild?-impl manifest)
  (let ((name (%%manifest-name manifest))
        (version (%%manifest-version manifest)))
    (let ((rebuild? #f))
      (jazz.for-each-higher-source-version version
        (lambda (source-version)
          (let ((rebuild (jazz.version-rebuild source-version))
                (recompile (jazz.version-recompile source-version)))
            (if (or (eq? rebuild 'all)
                    (and recompile (memq name recompile)))
                (set! rebuild? #t)))))
      rebuild?)))


;;;
;;;; Image
;;;


(define (jazz.build-image-impl product
          #!key
          (system jazz.kernel-system)
          (platform jazz.kernel-platform)
          (windowing jazz.kernel-windowing)
          (safety jazz.kernel-safety)
          (optimize? jazz.kernel-optimize?)
          (debug-environments? jazz.kernel-debug-environments?)
          (debug-location? jazz.kernel-debug-location?)
          (debug-source? jazz.kernel-debug-source?)
          (include-compiler? #f)
          (interpret-kernel? #f)
          (source jazz.kernel-source)
          (source-access? jazz.source-access?)
          (destination jazz.kernel-destination)
          (destination-directory jazz.kernel-install)
          (image #f)
          (kernel? #f)
          (console? #f)
          (minimum-heap #f)
          (maximum-heap #f)
          (feedback jazz.feedback))
  (let ((product-name (if (not product) "kernel" (symbol->string product)))
        (gambit-library (if include-compiler? "gambcgsc" "gambc"))
        (library-image? (eq? image 'library)))
    (let ((kernel-dir (string-append destination-directory "build/kernel/"))
          (product-dir (string-append destination-directory "build/products/" product-name "/")))
      (define (source-file path)
        (string-append source path))
      
      (define (build-file path)
        (string-append destination-directory path))
      
      (define (kernel-file path)
        (string-append kernel-dir path))
      
      (define (product-file path)
        (string-append product-dir path))
      
      (define (print line output)
        (display line output)
        (newline output))
      
      (define (feedback-message fmt-string . rest)
        (if feedback
            (apply feedback fmt-string rest)))
      
      (define (compile-file rebuild? name dir output)
        (let ((src (string-append dir name ".scm"))
              (dst (string-append output name ".c"))
              (mnf (string-append output name "." jazz.Manifest-Extension)))
          (let ((hash-changed? (not (jazz.manifest-uptodate? (jazz.load-updated-manifest name mnf src)))))
            (if (or rebuild? hash-changed? (not (jazz.file-exists? dst)))
                (let ((path (string-append dir name))
                      (options `(,@(if debug-environments? '(debug-environments) '())
                                 ,@(if debug-location? '(debug-location) '())
                                 ,@(if debug-source? '(debug-source) '()))))
                  ;; standardize path as it will be the path stored in debugging information
                  (let ((standardized-path (jazz.pathname-standardize (path-normalize path))))
                    (feedback-message "; compiling {a}..." path)
                    (compile-file-to-c standardized-path options: options output: output)
                    (jazz.update-manifest-compile-time name mnf src #f))
                  #t)
              #f))))
      
      (define (with-version-file version-file proc)
        (let ((rebuild? (jazz.kernel/product-needs-rebuild? version-file))
              (rebuild-architecture? (jazz.kernel/product-architecture-needs-rebuild? version-file))
              (was-touched? #f))
          (define (touch)
            (if (file-exists? version-file)
                (delete-file version-file))
            (set! was-touched? #t))
          
          (define (touched?)
            was-touched?)
          
          (proc rebuild? rebuild-architecture? touch touched?)
          (if (or was-touched? (not (file-exists? version-file)))
              (call-with-output-file version-file
                (lambda (output)
                  (write (jazz.get-source-version-number) output)
                  (newline output)
                  (write (system-version) output)
                  (newline output)
                  (write (system-stamp) output)
                  (newline output))))
          was-touched?))
      
      (define (kernel-time)
        (let ((version-file (kernel-file "version")))
          (if (file-exists? version-file)
              (jazz.file-modification-time version-file)
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
                              (string-append (source-file "kernel/") path)
                              (kernel-file path))
                (touch)))
          
          (if kernel?
              (begin
                ;; load architecture
                (load (kernel-file "_architecture"))
                
                ;; load syntax
                (load (source-file "kernel/syntax/header"))
                (load (source-file "kernel/syntax/macros"))
                (load (source-file "kernel/syntax/expansion"))
                (load (source-file "kernel/syntax/features"))
                (load (source-file "kernel/syntax/declares"))
                (load (source-file "kernel/syntax/primitives"))
                (load (source-file "kernel/syntax/syntax"))
                (load (source-file "kernel/syntax/runtime"))))
          
          (if architecture?
              (compile-kernel-file "_architecture"))
          
          (compile-source-file "syntax/" "header")
          (compile-source-file "syntax/" "macros")
          (compile-source-file "syntax/" "expansion")
          (compile-source-file "syntax/" "features")
          (compile-source-file "syntax/" "declares")
          (compile-source-file "syntax/" "primitives")
          (compile-source-file "syntax/" "syntax")
          (compile-source-file "syntax/" "runtime")
          (compile-source-file "runtime/" "base")
          (compile-source-file "runtime/" "common")
          (if include-compiler?
              (compile-source-file "runtime/" "build"))
          (compile-source-file "runtime/" "settings")
          (compile-source-file "runtime/" "install")
          (compile-source-file "runtime/" "digest")
          (compile-source-file "runtime/" "module")
          (compile-source-file "runtime/" "setup")))
      
      (define (generate-architecture rebuild? rebuild-architecture?)
        (let ((file (kernel-file "_architecture.scm")))
          (if (or rebuild? rebuild-architecture? (not (file-exists? file)))
              (begin
                (feedback-message "; generating {a}..." file)
                (call-with-output-file file
                  (lambda (output)
                    (jazz.print-architecture system platform windowing safety optimize? debug-environments? debug-location? debug-source? destination output)))
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
                    (not (file-exists? link-file))
                    (or (not kernel-time) (< (jazz.file-modification-time link-file) kernel-time))
                    (touched?))
                (let ((files `(,(kernel-file "_architecture")
                               ,(product-file (product-filename))
                               ,(kernel-file "syntax/header")
                               ,(kernel-file "syntax/macros")
                               ,(kernel-file "syntax/expansion")
                               ,(kernel-file "syntax/features")
                               ,(kernel-file "syntax/declares")
                               ,(kernel-file "syntax/primitives")
                               ,(kernel-file "syntax/syntax")
                               ,(kernel-file "syntax/runtime")
                               ,(kernel-file "runtime/base")
                               ,(kernel-file "runtime/common")
                               ,@(if include-compiler?
                                     `(,(kernel-file "runtime/build"))
                                   '())
                               ,(kernel-file "runtime/settings")
                               ,(kernel-file "runtime/install")
                               ,(kernel-file "runtime/digest")
                               ,(kernel-file "runtime/module")
                               ,(kernel-file "runtime/setup")
                               ,(product-file (main-filename)))))
                  (feedback-message "; creating link file...")
                  (if library-image?
                      (link-flat files output: link-file)
                    (link-incremental files output: link-file base: (string-append "~~lib/_" gambit-library))))))
          
          ;;;
          ;;;; Link Image
          ;;;
          
          (if (or rebuild?
                  (not (file-exists? (image-file)))
                  (or (not kernel-time) (< (jazz.file-modification-time (image-file)) kernel-time))
                  (touched?))
              (link-image))))
      
      (define (link-file)
        (if library-image?
            (product-file (string-append product-name ".o1.c"))
          (product-file (string-append product-name ".c"))))
      
      (define (generate-product rebuild?)
        (let ((file (product-file (string-append (product-filename) ".scm"))))
          (if (or rebuild? (not (file-exists? file)))
              (begin
                (feedback-message "; generating {a}..." file)
                (call-with-output-file file
                  (lambda (output)
                    (jazz.print-variable 'jazz.product product output)
                    (newline output)
                    (jazz.print-variable 'jazz.image (or image 'executable) output)
                    (newline output)
                    (jazz.print-variable 'jazz.built (jazz.pathname-normalize destination-directory) output)
                    (newline output)
                    (jazz.print-variable 'jazz.source-built (jazz.pathname-standardize (path-normalize source)) output)
                    (newline output)
                    (jazz.print-variable 'jazz.source (if library-image? (jazz.pathname-normalize source) (jazz.relativise-directory source destination-directory)) output)
                    (newline output)
                    (jazz.print-variable 'jazz.source-access? source-access? output)))
                #t)
            #f)))
      
      (define (generate-main rebuild?)
        (let ((file (product-file (string-append (main-filename) ".scm"))))
          (if (or rebuild? (not (file-exists? file)))
              (begin
                (feedback-message "; generating {a}..." file)
                (call-with-output-file file
                  (lambda (output)
                    (cond (library-image?
                            (display "(jazz.library-main)" output)
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
                           (display "(define (jazz.main)" output)
                           (newline output)
                           (display "  (jazz.executable-main))" output)
                           (newline output)
                           (newline output)
                           (display "(##main-set! jazz.main)" output)
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
           (let ((file (product-file (string-append product-name ".ico"))))
             (if (or rebuild? (not (file-exists? file)))
                 (begin
                   (jazz.copy-file (source-file "etc/resources/windows/jazz.ico") file feedback: feedback)
                   #t)
               #f)))
          (else
           #f)))
      
      (define (resource-files)
        (case platform
          ((windows)
           (let ()
             (define (resource-file name)
               (string-append (source-file "etc/resources/windows/") name "res.o"))
             
             (let ((file (resource-file product-name)))
               (if (file-exists? file)
                   (list (jazz.quote-gcc-pathname file platform))
                 (list (jazz.quote-gcc-pathname (resource-file "jazz") platform))))))
          (else
           '())))
      
      (define (gambit-link-libraries)
        (if (not library-image?)
            `("-lgambc" ,(string-append "-l" gambit-library))
          '()))
      
      (define (link-libraries)
        (case platform
          ((windows)
           '("-lws2_32"))
          ((unix)
           '("-lm" "-ldl" "-lutil"))
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
            (else
             '()))))
      
      (define (link-image)
        (feedback-message "; linking {a}..." (if library-image? "library" "executable"))
        (jazz.call-process
          "gcc"
          `(,(jazz.quote-gcc-pathname (kernel-file "_architecture.c") platform)
            ,(jazz.quote-gcc-pathname (product-file (string-append (product-filename) ".c")) platform)
            ,(jazz.quote-gcc-pathname (kernel-file "syntax/header.c") platform)
            ,(jazz.quote-gcc-pathname (kernel-file "syntax/macros.c") platform)
            ,(jazz.quote-gcc-pathname (kernel-file "syntax/expansion.c") platform)
            ,(jazz.quote-gcc-pathname (kernel-file "syntax/features.c") platform)
            ,(jazz.quote-gcc-pathname (kernel-file "syntax/declares.c") platform)
            ,(jazz.quote-gcc-pathname (kernel-file "syntax/primitives.c") platform)
            ,(jazz.quote-gcc-pathname (kernel-file "syntax/syntax.c") platform)
            ,(jazz.quote-gcc-pathname (kernel-file "syntax/runtime.c") platform)
            ,(jazz.quote-gcc-pathname (kernel-file "runtime/base.c") platform)
            ,(jazz.quote-gcc-pathname (kernel-file "runtime/common.c") platform)
            ,@(if include-compiler?
                  `(,(jazz.quote-gcc-pathname (kernel-file "runtime/build.c") platform))
                '())
            ,(jazz.quote-gcc-pathname (kernel-file "runtime/settings.c") platform)
            ,(jazz.quote-gcc-pathname (kernel-file "runtime/install.c") platform)
            ,(jazz.quote-gcc-pathname (kernel-file "runtime/digest.c") platform)
            ,(jazz.quote-gcc-pathname (kernel-file "runtime/module.c") platform)
            ,(jazz.quote-gcc-pathname (kernel-file "runtime/setup.c") platform)
            ,(jazz.quote-gcc-pathname (product-file (string-append (main-filename) ".c")) platform)
            ,(jazz.quote-gcc-pathname (link-file) platform)
            ,@(resource-files)
            ,(string-append "-I" (jazz.quote-gcc-pathname (path-strip-trailing-directory-separator (path-expand "~~include")) platform))
            ,(string-append "-L" (jazz.quote-gcc-pathname (path-strip-trailing-directory-separator (path-expand "~~lib")) platform))
            ,@(gambit-link-libraries)
            ,@(link-libraries)
            ,@(link-options)
            "-o" ,(jazz.quote-gcc-pathname (image-file) platform))))
      
      (define (image-file)
        (if library-image?
            (build-file (string-append product-name ".o1"))
          (build-file (string-append product-name (jazz.executable-extension platform)))))
      
      ;;;
      ;;;; Configuration
      ;;;
      
      (define (generate-configuration)
        (let ((file (build-file ".configuration")))
          (if (not (file-exists? file))
              (begin
                (jazz.feedback "; generating {a}..." file)
                (call-with-output-file file
                  (lambda (output)
                    (jazz.print-configuration #f system platform windowing safety optimize? debug-environments? debug-location? debug-source? interpret-kernel? source-access? destination output)))))))
      
      ;;;
      ;;;; Gambcini
      ;;;
      
      (define (generate-gambcini)
        (let ((file (build-file ".gambcini")))
          (if (not (file-exists? file))
              (begin
                (jazz.feedback "; generating {a}..." file)
                (call-with-output-file file
                  (lambda (output)
                    (print ";;;==============" output)
                    (print ";;;  JazzScheme" output)
                    (print ";;;==============" output)
                    (print ";;;" output)
                    (print ";;;; Gambit Ini" output)
                    (print ";;;" output)
                    (newline output)
                    (newline output)
                    (jazz.print-architecture system platform windowing safety optimize? debug-environments? debug-location? debug-source? destination output)
                    (newline output)
                    (jazz.print-variable 'jazz.product #f output)
                    (newline output)
                    (jazz.print-variable 'jazz.image (or image 'executable) output)
                    (newline output)
                    (jazz.print-variable 'jazz.built "." output)
                    (newline output)
                    (jazz.print-variable 'jazz.source-built (jazz.pathname-standardize (path-normalize source)) output)
                    (newline output)
                    (jazz.print-variable 'jazz.source (jazz.relativise-directory source destination-directory) output)
                    (newline output)
                    (jazz.print-variable 'jazz.source-access? source-access? output)
                    (newline output)
                    (newline output)
                    (display "(load (string-append jazz.source \"kernel/boot\"))" output)
                    (newline output)))))))
      
      (jazz.create-directories product-dir feedback: feedback)
      (jazz.create-directories (kernel-file "syntax/") feedback: feedback)
      (jazz.create-directories (kernel-file "runtime/") feedback: feedback)
      
      (if kernel?
          (generate-configuration))
      
      (build-kernel)
      (build-product)
      
      (if interpret-kernel?
          (generate-gambcini)))))


(define (jazz.executable-extension platform)
  (case platform
    ((windows)
     ".exe")
    (else
     "")))


(define (jazz.print-architecture system platform windowing safety optimize? debug-environments? debug-location? debug-source? destination output)
  (jazz.print-variable 'jazz.kernel-system system output)
  (newline output)
  (jazz.print-variable 'jazz.kernel-platform platform output)
  (newline output)
  (jazz.print-variable 'jazz.kernel-windowing windowing output)
  (newline output)
  (jazz.print-variable 'jazz.kernel-safety safety output)
  (newline output)
  (jazz.print-variable 'jazz.kernel-optimize? optimize? output)
  (newline output)
  (jazz.print-variable 'jazz.kernel-debug-environments? debug-environments? output)
  (newline output)
  (jazz.print-variable 'jazz.kernel-debug-location? debug-location? output)
  (newline output)
  (jazz.print-variable 'jazz.kernel-debug-source? debug-source? output)
  (newline output)
  (jazz.print-variable 'jazz.kernel-destination destination output)
  (newline output)
  (jazz.print-variable 'jazz.kernel-version (jazz.get-source-version-number) output))


(define (jazz.print-variable variable value output)
  (display "(define " output)
  (display variable output)
  (newline output)
  (display "  " output)
  (if (or (symbol? value)
          (list? value))
      (display "'" output))
  (write value output)
  (display ")" output)
  (newline output))


;;;
;;;; Setup
;;;


(set! jazz.manifest-needs-rebuild? jazz.manifest-needs-rebuild?-impl)
(set! jazz.build-image jazz.build-image-impl)
