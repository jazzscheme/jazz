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


(block kernel.build


(jazz.kernel-declares)


;;;
;;;; Version
;;;


(define (jazz.for-each-source-version proc)
  (for-each proc (jazz.get-source-versions)))


(define (jazz.for-each-higher-source-version version proc)
  (let iter ((source-versions (jazz.get-source-versions)))
    (if (%%not (%%null? source-versions))
        (let ((source-version (%%car source-versions)))
          (if (%%fx> (jazz.version-number source-version) version)
              (begin
                (proc source-version)
                (iter (%%cdr source-versions))))))))


(define (jazz.kernel/product-needs-rebuild? version-file)
  (receive (version gambit-version gambit-stamp) (jazz.load-version-file version-file)
    (if (%%not version)
        #t
      (or (%%not (jazz.gambit-uptodate? gambit-version gambit-stamp))
          (let ((rebuild? #f))
            (jazz.for-each-higher-source-version version
              (lambda (source-version)
                (if (%%memq (jazz.version-rebuild source-version) '(kernel all))
                    (set! rebuild? #t))))
            rebuild?)))))


(define (jazz.kernel/product-architecture-needs-rebuild? version-file)
  (receive (version gambit-version gambit-stamp) (jazz.load-version-file version-file)
    (if (%%not version)
        #t
      (or (%%not (jazz.gambit-uptodate? gambit-version gambit-stamp))
          (let ((rebuild-architecture? #f))
            (jazz.for-each-higher-source-version version
              (lambda (source-version)
                (if (or (%%memq (jazz.version-rebuild source-version) '(kernel all))
                        (jazz.version-recompile source-version))
                    (set! rebuild-architecture? #t))))
            rebuild-architecture?)))))


(define (jazz.load-version-file version-file)
  (if (file-exists? version-file)
      (call-with-input-file (%%list path: version-file eol-encoding: 'cr-lf)
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
            (if (or (%%eq? rebuild 'all)
                    (and recompile (%%memq name recompile)))
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
          (resources #f)
          (image #f)
          (kernel? #f)
          (console? #f)
          (minimum-heap #f)
          (maximum-heap #f)
          (feedback jazz.feedback))
  (let ((product-name (if (%%not product) "kernel" (%%symbol->string product)))
        (gambit-library (if include-compiler? "gambcgsc" "gambc"))
        (library-image? (%%eq? image 'library)))
    (let ((build-directory (if product (%%repository-directory jazz.Build-Repository) destination-directory))
          (kernel-dir (string-append destination-directory "build/kernel/"))
          (product-dir (string-append destination-directory "build/products/" product-name "/")))
      (define (source-file path)
        (%%string-append source path))
      
      (define (build-file path)
        (%%string-append build-directory path))
      
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
              (digest (string-append output name "." jazz.Digest-Extension))
              (mnf (string-append output name "." jazz.Manifest-Extension)))
          (let ((hash-changed? (%%not (jazz.manifest-uptodate? src (jazz.load-updated-manifest name digest mnf src)))))
            (if (or rebuild? hash-changed? (%%not (jazz.file-exists? dst)))
                (let ((path (%%string-append dir name))
                      (options `(,@(if debug-environments? '(debug-environments) '())
                                 ,@(if debug-location? '(debug-location) '())
                                 ,@(if debug-source? '(debug-source) '()))))
                  ;; standardize path as it will be the path stored in debugging information
                  (let ((standardized-path (jazz.pathname-standardize (path-normalize path))))
                    (feedback-message "; compiling {a}..." path)
                    (compile-file-to-c standardized-path options: options output: output)
                    (jazz.update-manifest-compile-time name digest mnf src #f))
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
          (if (or was-touched? (%%not (file-exists? version-file)))
              (call-with-output-file (list path: version-file eol-encoding: (jazz.platform-eol-encoding jazz.kernel-platform))
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
                (load (source-file "kernel/syntax/expansion"))
                (load (source-file "kernel/syntax/features"))
                (load (source-file "kernel/syntax/declares"))
                (load (source-file "kernel/syntax/primitives"))
                (load (source-file "kernel/syntax/syntax"))
                (load (source-file "kernel/syntax/runtime"))))
          
          (if architecture?
              (compile-kernel-file "_architecture"))
          
          (compile-source-file "syntax/" "header")
          (compile-source-file "syntax/" "macro")
          (compile-source-file "syntax/" "block")
          (compile-source-file "syntax/" "expansion")
          (compile-source-file "syntax/" "features")
          (compile-source-file "syntax/" "declares")
          (compile-source-file "syntax/" "primitives")
          (compile-source-file "syntax/" "syntax")
          (compile-source-file "syntax/" "runtime")
          (compile-source-file "runtime/" "base")
          (compile-source-file "runtime/" "crash")
          (compile-source-file "runtime/" "common")
          (compile-source-file "runtime/" "settings")
          (if include-compiler?
              (compile-source-file "runtime/" "build"))
          (compile-source-file "runtime/" "install")
          (compile-source-file "runtime/" "digest")
          (compile-source-file "runtime/" "unit")
          (compile-source-file "runtime/" "setup")))
      
      (define (generate-architecture rebuild? rebuild-architecture?)
        (let ((file (kernel-file "_architecture.scm")))
          (if (or rebuild? rebuild-architecture? (%%not (file-exists? file)))
              (begin
                (feedback-message "; generating {a}..." file)
                (call-with-output-file (list path: file eol-encoding: (jazz.platform-eol-encoding jazz.kernel-platform))
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
                    (%%not (file-exists? link-file))
                    (or (%%not kernel-time) (< (jazz.file-modification-time link-file) kernel-time))
                    (touched?))
                (let ((files `(,(kernel-file "_architecture")
                               ,(product-file (product-filename))
                               ,(kernel-file "syntax/header")
                               ,(kernel-file "syntax/macro")
                               ,(kernel-file "syntax/block")
                               ,(kernel-file "syntax/expansion")
                               ,(kernel-file "syntax/features")
                               ,(kernel-file "syntax/declares")
                               ,(kernel-file "syntax/primitives")
                               ,(kernel-file "syntax/syntax")
                               ,(kernel-file "syntax/runtime")
                               ,(kernel-file "runtime/base")
                               ,(kernel-file "runtime/crash")
                               ,(kernel-file "runtime/common")
                               ,(kernel-file "runtime/settings")
                               ,@(if include-compiler?
                                     `(,(kernel-file "runtime/build"))
                                   '())
                               ,(kernel-file "runtime/install")
                               ,(kernel-file "runtime/digest")
                               ,(kernel-file "runtime/unit")
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
                  (or (%%not kernel-time) (< (jazz.file-modification-time (image-file)) kernel-time))
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
                (call-with-output-file (list path: file eol-encoding: (jazz.platform-eol-encoding jazz.kernel-platform))
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
        (let ((file (product-file (%%string-append (main-filename) ".scm"))))
          (if (or rebuild? (%%not (file-exists? file)))
              (begin
                (feedback-message "; generating {a}..." file)
                (call-with-output-file (list path: file eol-encoding: 'lf)
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
           (with-resources
             (lambda (resources rc res)
               (let ((rcfile (%%string-append resources "/" rc)))
                 (if (or rebuild?
                         (jazz.file-needs-update? rcfile res))
                     (begin
                       (feedback-message "; compiling {a}..." rcfile)
                       (jazz.call-process "windres" (list rc "-o" res) directory: resources)
                       #t)
                   #f)))))
          (else
           #f)))
      
      (define (resource-files)
        (case platform
          ((windows)
           (with-resources
             (lambda (resources rc res)
               (%%list (jazz.quote-gcc-pathname res platform)))))
          (else
           '())))
      
      (define (with-resources proc)
        (case platform
          ((windows)
           (let ()
             (define (split resources rcname)
               (let ((rc (%%string-append rcname ".rc"))
                     (res (%%string-append (jazz.pathname-normalize (product-file "")) product-name "res.o")))
                 (proc resources rc res)))
             
             (if resources
                 (split (jazz.package-pathname (%%product-package (jazz.get-product product)) resources) product-name)
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
            ,(jazz.quote-gcc-pathname (kernel-file "syntax/macro.c") platform)
            ,(jazz.quote-gcc-pathname (kernel-file "syntax/block.c") platform)
            ,(jazz.quote-gcc-pathname (kernel-file "syntax/expansion.c") platform)
            ,(jazz.quote-gcc-pathname (kernel-file "syntax/features.c") platform)
            ,(jazz.quote-gcc-pathname (kernel-file "syntax/declares.c") platform)
            ,(jazz.quote-gcc-pathname (kernel-file "syntax/primitives.c") platform)
            ,(jazz.quote-gcc-pathname (kernel-file "syntax/syntax.c") platform)
            ,(jazz.quote-gcc-pathname (kernel-file "syntax/runtime.c") platform)
            ,(jazz.quote-gcc-pathname (kernel-file "runtime/base.c") platform)
            ,(jazz.quote-gcc-pathname (kernel-file "runtime/crash.c") platform)
            ,(jazz.quote-gcc-pathname (kernel-file "runtime/common.c") platform)
            ,(jazz.quote-gcc-pathname (kernel-file "runtime/settings.c") platform)
            ,@(if include-compiler?
                  `(,(jazz.quote-gcc-pathname (kernel-file "runtime/build.c") platform))
                '())
            ,(jazz.quote-gcc-pathname (kernel-file "runtime/install.c") platform)
            ,(jazz.quote-gcc-pathname (kernel-file "runtime/digest.c") platform)
            ,(jazz.quote-gcc-pathname (kernel-file "runtime/unit.c") platform)
            ,(jazz.quote-gcc-pathname (kernel-file "runtime/setup.c") platform)
            ,(jazz.quote-gcc-pathname (product-file (string-append (main-filename) ".c")) platform)
            ,(jazz.quote-gcc-pathname (link-file) platform)
            ,@(resource-files)
            ,(string-append "-I" (jazz.quote-gcc-pathname (path-strip-trailing-directory-separator (path-expand "~~include")) platform))
            ,(string-append "-L" (jazz.quote-gcc-pathname (path-strip-trailing-directory-separator (path-expand "~~lib")) platform))
            ,@(gambit-link-libraries)
            ,@(link-libraries)
            ,@(link-options)
            "-o" ,(jazz.quote-gcc-pathname (image-file) platform)))
        (case platform
          ((windows)
           (if (jazz.build-single-objects?)
               (jazz.obliterate-PE-timestamp (image-file) 'EXE)))))
      
      (define (image-file)
        (if library-image?
            (build-file (jazz.add-extension product-name "o1"))
          (build-file (jazz.add-extension product-name (jazz.executable-extension platform)))))
      
      ;;;
      ;;;; Configuration
      ;;;
      
      (define (generate-configuration)
        (let ((file (dest-file ".configuration")))
          (if (%%not (file-exists? file))
              (begin
                (jazz.feedback "; generating {a}..." file)
                (call-with-output-file (list path: file eol-encoding: (jazz.platform-eol-encoding jazz.kernel-platform))
                  (lambda (output)
                    (jazz.print-configuration #f system platform windowing safety optimize? debug-environments? debug-location? debug-source? interpret-kernel? source-access? destination output)))))))
      
      ;;;
      ;;;; Gambcini
      ;;;
      
      (define (generate-gambcini)
        (let ((file (dest-file ".gambcini")))
          (if (%%not (file-exists? file))
              (begin
                (jazz.feedback "; generating {a}..." file)
                (call-with-output-file (list path: file eol-encoding: (jazz.platform-eol-encoding jazz.kernel-platform))
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


;;;
;;;; PE-Timestamps
;;;


(define (jazz.obliterate-PE-timestamp pathname type)
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


(define (jazz.pkg-config what libname)
  (let ((string-port (open-output-string))
        (process-port (open-process (%%list path: "pkg-config" arguments: (%%list what libname)))))
    (if (%%fx= (process-status process-port) 0)
        (begin
          (jazz.pipe-no-return process-port string-port)
          (get-output-string string-port))
      (jazz.error "failed"))))

(define (jazz.pkg-config-cflags libname)
  (jazz.pkg-config "--cflags" libname))

(define (jazz.pkg-config-libs libname)
  (jazz.pkg-config "--libs" libname))

(define (jazz.pkg-config-version libname)
  (jazz.pkg-config "--modversion" libname))

(define (jazz.pipe-no-return input output)
  (let iterate ()
    (let ((c (read-char input)))
      (if (%%not (or (eof-object? c) (%%eq? #\newline c)))
          (begin
            (write-char c output)
            (iterate))))))


;;;
;;;; Libraries
;;;


(define (jazz.build-library-impl product-name descriptor
          #!key
          (options '())
          (platform jazz.kernel-platform)
          (destination-directory jazz.kernel-install)
          (feedback jazz.feedback))
  
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
                         (jazz.quote-jazz-gcc-pathname jazz-relative-dir)
                       (jazz.quote-gcc-pathname dir platform))))
              (%%list (%%string-append "-L" quoted-dir)))
          (jazz.error "ill-formed libdir parameter in product link options")))
      
      (cond ((%%string? opt)
             (%%list opt))
            (else
             (case (%%car opt)
               ((pkg-config)
                (jazz.split-string (%%apply jazz.pkg-config (%%cdr opt)) #\space))
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
      (if (jazz.file-exists? header)
          (begin
            (set! jazz.register-image-units
                  (lambda (lib-name units)
                    (for-each (lambda (unit)
                                (%%table-set! digest-table (%%car unit) (%%cadr unit)))
                              units)))
            (load header))))
    
    (define (unit-uptodate? unit-name)
      (let ((image-unit-compile-time-hash (%%table-ref digest-table unit-name #f)))
        (and image-unit-compile-time-hash
             (jazz.with-unit-resources unit-name #f
               (lambda (src obj bin lib obj-uptodate? bin-uptodate? lib-uptodate? manifest)
                 (let ((src-pathname (jazz.resource-pathname src)))
                   (let ((digest (and manifest (jazz.manifest-uptodate? src-pathname manifest))))
                     (and (%%not (jazz.manifest-needs-rebuild? manifest))
                          (%%string=? image-unit-compile-time-hash (%%digest-hash digest))))))))))
    
    (define (subunits-uptodate? units)
      (or (%%null? units)
          (and (unit-uptodate? (%%car units))
               (subunits-uptodate? (%%cdr units)))))
            
    (load-image-units-manifest)
    (subunits-uptodate? sub-units))
  
  (define (make-library-header header- library sub-units)
    (call-with-output-file (list path: header- eol-encoding: (jazz.platform-eol-encoding jazz.kernel-platform))
      (lambda (port)
        (display (string-append "(jazz.register-image-units '" (%%symbol->string library) " '(") port)
        (newline port)
        (for-each (lambda (unit-name)
                    (jazz.with-unit-resources unit-name #f
                      (lambda (src obj bin lib obj-uptodate? bin-uptodate? lib-uptodate? manifest)
                        (display (string-append "  (" (%%symbol->string unit-name) " ") port)
                        (write (%%manifest-compile-time-hash manifest) port)
                        (display ")" port)
                        (newline port))))
                  sub-units)
        (display "))" port)
        (newline port))))
  
  (let* ((product (jazz.get-product product-name))
         (package (%%product-package product))
         (update (jazz.cond-expand-each (jazz.ill-formed-field-error "update" product-name)
                                        (jazz.product-descriptor-update descriptor)))
         (library-base (jazz.relocate-product-library-name-base jazz.Build-Repository package product-name))
         (library-dir (jazz.pathname-dir library-base)))
    (jazz.with-numbered-pathname (string-append library-base "." jazz.Library-Extension) #t 1
      (lambda (library-o1 o1-exists?)
        (let* ((linkfile (string-append library-o1 ".c"))
               (header (string-append library-base "." jazz.Library-Manifest-Extension))
               (header-c (string-append header ".c"))
               (header-o (string-append header ".o"))
               (sub-units (remove-duplicates (%%apply append (map jazz.get-subunit-names update)))))
          
          (define (build-library)
            (jazz.create-build-package package)
            (make-library-header header product-name sub-units)
            (compile-file-to-c header output: header-c)
            (compile-file header-c options: '(obj) cc-options: "-D___BIND_LATE ")
            
            (feedback-message "; creating link file...")
            (link-flat (%%cons header
                               (map (lambda (subunit-name)
                                      (jazz.with-unit-resources subunit-name #f
                                        (lambda (src obj bin lib obj-uptodate? bin-uptodate? lib-uptodate? manifest)
                                          (jazz.resource-pathname obj))))
                                    sub-units))
                       output: linkfile
                       warnings?: #f)
            
            (feedback-message "; linking library... ({a} units)" (%%number->string (%%length sub-units)))
            (jazz.call-process
              "gcc"
              `(,@(case platform
                    ((windows) '("-shared" "-D___DYNAMIC"))
                    (else '("-bundle" "-D___DYNAMIC")))
                ,header-o
                ,@(map (lambda (subunit-name)
                         (jazz.with-unit-resources subunit-name #f
                           (lambda (src obj bin lib obj-uptodate? bin-uptodate? lib-uptodate? manifest)
                             (%%string-append (jazz.resource-pathname obj) ".o"))))
                       sub-units)
                ,linkfile
                "-o" ,library-o1
                ,(string-append "-I" (jazz.quote-gcc-pathname (path-strip-trailing-directory-separator (path-expand "~~include")) platform))
                ,(string-append "-L" (jazz.quote-gcc-pathname (path-strip-trailing-directory-separator (path-expand "~~lib")) platform))
                ,@(link-options)))
            (case platform
              ((windows)
               (if (jazz.build-single-objects?)
                   (jazz.obliterate-PE-timestamp library-o1 'DLL))))
            (map delete-file (%%list header-c header-o linkfile))
            #t)

          (or (and o1-exists? (library-manifest-uptodate? header sub-units))
              (build-library)))))))


(define (jazz.executable-extension platform)
  (case platform
    ((windows)
     "exe")
    (else
     #f)))


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
  (if (or (%%symbol? value)
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
(set! jazz.build-library jazz.build-library-impl))
