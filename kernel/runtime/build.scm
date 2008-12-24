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


;;;
;;;; Versions
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


(define (jazz.manifest-needs-rebuild? manifest)
  (let ((name (%%manifest-name manifest))
        (version (%%manifest-version manifest)))
    ;; test is for backward compatibility and could be removed in the future
    (or (not version)
        (let ((rebuild? #f))
          (jazz.for-each-higher-source-version version
            (lambda (source-version)
              (let ((rebuild (jazz.version-rebuild source-version))
                    (recompile (jazz.version-recompile source-version)))
                (if (or (eq? rebuild 'all)
                        (and recompile (memq name recompile)))
                    (set! rebuild? #t)))))
          rebuild?))))


;;;
;;;; Executable
;;;


(define (jazz.build-executable product
          #!key
          (system jazz.kernel-system)
          (platform jazz.kernel-platform)
          (windowing jazz.kernel-windowing)
          (safety jazz.kernel-safety)
          (optimize? jazz.kernel-optimize?)
          (include-source? jazz.kernel-include-source?)
          (interpret? jazz.kernel-interpret?)
          (source jazz.source)
          (source-access? jazz.source-access?)
          (destination jazz.kernel-destination)
          (destination-directory jazz.kernel-install)
          (kernel? #f)
          (console? #f)
          (minimum-heap #f)
          (maximum-heap #f)
          (feedback jazz.feedback))
  (let ((product-name (if (not product) "jazz" (symbol->string product))))
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
              (dst (string-append output name ".c")))
          (if (or rebuild? (jazz.file-needs-update? src dst))
              (let ((path (string-append dir name))
                    (options '(debug-location debug-environments)))
                ;; standardize path as it will be the path stored in debugging information
                (let ((standardized-path (jazz.pathname-standardize (path-normalize path))))
                  (feedback-message "; compiling {a}..." path)
                  (compile-file-to-c standardized-path options: options output: output))
                #t)
            #f)))
      
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
          (compile-source-file "runtime/" "build")
          (compile-source-file "runtime/" "settings")
          (compile-source-file "runtime/" "install")
          (compile-source-file "runtime/" "digest")
          (compile-source-file "runtime/" "kernel")
          (compile-source-file "runtime/" "main")))
      
      (define (generate-architecture rebuild? rebuild-architecture?)
        (let ((file (kernel-file "_architecture.scm")))
          (if (or rebuild? rebuild-architecture? (not (file-exists? file)))
              (begin
                (feedback-message "; generating {a}..." file)
                (call-with-output-file file
                  (lambda (output)
                    (jazz.print-architecture system platform windowing safety optimize? include-source? interpret? destination output)))
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
              (compile-product-file "_product"))
          (if main?
              (compile-product-file "_main"))
          
          (if (generate-resources rebuild?)
              (touch))
          
          ;;;
          ;;;; Link Kernel
          ;;;
          
          (let ((link-file (product-file (string-append product-name ".c"))))
            (if (or rebuild?
                    (not (file-exists? link-file))
                    (or (not kernel-time) (< (jazz.file-modification-time link-file) kernel-time))
                    (touched?))
                (begin
                  (feedback-message "; linking kernel...")
                  (link-incremental (list (kernel-file "_architecture")
                                          (product-file "_product")
                                          (kernel-file "syntax/header")
                                          (kernel-file "syntax/macros")
                                          (kernel-file "syntax/expansion")
                                          (kernel-file "syntax/features")
                                          (kernel-file "syntax/declares")
                                          (kernel-file "syntax/primitives")
                                          (kernel-file "syntax/syntax")
                                          (kernel-file "syntax/runtime")
                                          (kernel-file "runtime/base")
                                          (kernel-file "runtime/build")
                                          (kernel-file "runtime/settings")
                                          (kernel-file "runtime/install")
                                          (kernel-file "runtime/digest")
                                          (kernel-file "runtime/kernel")
                                          (kernel-file "runtime/main")
                                          (product-file "_main"))
                                    output: link-file
                                    base: "~~/lib/_gambcgsc"))))
          
          ;;;
          ;;;; Link Executable
          ;;;
          
          (if (or rebuild?
                  (not (file-exists? (executable-name)))
                  (or (not kernel-time) (< (jazz.file-modification-time (executable-name)) kernel-time))
                  (touched?))
              (link-executable))))
      
      (define (generate-product rebuild?)
        (let ((file (product-file "_product.scm")))
          (if (or rebuild? (not (file-exists? file)))
              (begin
                (feedback-message "; generating {a}..." file)
                (call-with-output-file file
                  (lambda (output)
                    (jazz.print-variable 'jazz.product product output)
                    (newline output)
                    (jazz.print-variable 'jazz.built (jazz.pathname-normalize destination-directory) output)
                    (newline output)
                    (jazz.print-variable 'jazz.source-built (jazz.pathname-standardize (path-normalize source)) output)
                    (newline output)
                    (jazz.print-variable 'jazz.source (jazz.relativise-directory source destination-directory) output)
                    (newline output)
                    (jazz.print-variable 'jazz.source-access? source-access? output)))
                #t)
            #f)))
      
      (define (generate-main rebuild?)
        (let ((file (product-file "_main.scm")))
          (if (or rebuild? (not (file-exists? file)))
              (begin
                (feedback-message "; generating {a}..." file)
                (call-with-output-file file
                  (lambda (output)
                    (display "#!gsi -:dar,t8,f8,-8" output)
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
                    (display "  (jazz.process-main))" output)
                    (newline output)
                    (newline output)
                    (display "(##main-set! jazz.main)" output)
                    (newline output)))
                #t)
            #f)))
      
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
      
      (define (link-libraries)
        (case platform
          ((windows)
           '("-lws2_32"))
          ((unix)
           '("-lm" "-ldl" "-lutil"))
          (else
           '())))
      
      (define (link-options)
        (case platform
          ((windows)
           (if console?
               '("-mconsole")
             '("-mwindows")))
          (else
           '())))
      
      (define (link-executable)
        (feedback-message "; linking executable...")
        (jazz.call-process
          "gcc"
          `(,(jazz.quote-gcc-pathname (kernel-file "_architecture.c") platform)
            ,(jazz.quote-gcc-pathname (product-file "_product.c") platform)
            ,(jazz.quote-gcc-pathname (kernel-file "syntax/header.c") platform)
            ,(jazz.quote-gcc-pathname (kernel-file "syntax/macros.c") platform)
            ,(jazz.quote-gcc-pathname (kernel-file "syntax/expansion.c") platform)
            ,(jazz.quote-gcc-pathname (kernel-file "syntax/features.c") platform)
            ,(jazz.quote-gcc-pathname (kernel-file "syntax/declares.c") platform)
            ,(jazz.quote-gcc-pathname (kernel-file "syntax/primitives.c") platform)
            ,(jazz.quote-gcc-pathname (kernel-file "syntax/syntax.c") platform)
            ,(jazz.quote-gcc-pathname (kernel-file "syntax/runtime.c") platform)
            ,(jazz.quote-gcc-pathname (kernel-file "runtime/base.c") platform)
            ,(jazz.quote-gcc-pathname (kernel-file "runtime/build.c") platform)
            ,(jazz.quote-gcc-pathname (kernel-file "runtime/settings.c") platform)
            ,(jazz.quote-gcc-pathname (kernel-file "runtime/install.c") platform)
            ,(jazz.quote-gcc-pathname (kernel-file "runtime/digest.c") platform)
            ,(jazz.quote-gcc-pathname (kernel-file "runtime/kernel.c") platform)
            ,(jazz.quote-gcc-pathname (kernel-file "runtime/main.c") platform)
            ,(jazz.quote-gcc-pathname (product-file "_main.c") platform)
            ,(jazz.quote-gcc-pathname (product-file (string-append product-name ".c")) platform)
            ,@(resource-files)
            ,(string-append "-I" (jazz.quote-gcc-pathname (path-expand "~~/include") platform))
            ,(string-append "-L" (jazz.quote-gcc-pathname (path-expand "~~/lib") platform))
            "-lgambc" "-lgambcgsc" ,@(link-libraries)
            ,@(link-options)
            "-o" ,(jazz.quote-gcc-pathname (build-file product-name) platform))))
      
      (define (executable-name)
        (build-file (string-append product-name (jazz.executable-extension platform))))
      
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
                    (jazz.print-configuration #f system platform windowing safety optimize? include-source? interpret? source-access? destination output)))))))
      
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
                    (jazz.print-architecture system platform windowing safety optimize? include-source? interpret? destination output)
                    (newline output)
                    (jazz.print-variable 'jazz.product #f output)
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
      
      (if interpret?
          (generate-gambcini)))))


(define (jazz.executable-extension platform)
  (case platform
    ((windows)
     ".exe")
    (else
     "")))


(define (jazz.print-architecture system platform windowing safety optimize? include-source? interpret? destination output)
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
  (jazz.print-variable 'jazz.kernel-include-source? include-source? output)
  (newline output)
  (jazz.print-variable 'jazz.kernel-interpret? interpret? output)
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
;;;; Feedback
;;;


(define jazz.build-feedback
  jazz.feedback)


;;;
;;;; List
;;;


(define (jazz.collect-if predicate lst)
  (let iter ((scan lst))
    (if (%%not (%%null? scan))
        (let ((value (%%car scan)))
          (if (predicate value)
              (%%cons value (iter (%%cdr scan)))
            (iter (%%cdr scan))))
      '())))


;;;
;;;; String
;;;


(define (jazz.string-find str c)
  (let ((len (%%string-length str)))
    (let iter ((n 0))
      (cond ((%%fx>= n len)
             #f)
            ((%%char=? (%%string-ref str n) c)
             n)
            (else
             (iter (%%fx+ n 1)))))))


(define (jazz.string-replace str old new)
  (let ((cpy (string-copy str)))
    (let iter ((n (%%fx- (%%string-length cpy) 1)))
      (if (%%fx>= n 0)
          (begin
            (if (%%eqv? (%%string-ref cpy n) old)
                (%%string-set! cpy n new))
            (iter (%%fx- n 1)))))
    cpy))


(define (jazz.string-starts-with? str target)
  (let ((sl (%%string-length str))
        (tl (%%string-length target)))
    (and (%%fx>= sl tl)
         (%%string=? (%%substring str 0 tl) target))))


(define (jazz.string-ends-with? str target)
  (let ((sl (%%string-length str))
        (tl (%%string-length target)))
    (and (%%fx>= sl tl)
         (%%string=? (%%substring str (%%fx- sl tl) sl) target))))


(define (jazz.split-string str separator)
  (let ((lst '())
        (end (%%string-length str)))
    (let iter ((pos (%%fx- end 1)))
      (if (%%fx> pos 0)
          (begin
            (if (%%eqv? (%%string-ref str pos) separator)
                (begin
                  (set! lst (%%cons (%%substring str (%%fx+ pos 1) end) lst))
                  (set! end pos)))
            (iter (%%fx- pos 1))))
        (%%cons (%%substring str 0 end) lst))))


(define (jazz.join-strings strings separator)
  (let ((output (open-output-string)))
    (display (%%car strings) output)
    (for-each (lambda (string)
                (display separator output)
                (display string output))
              (%%cdr strings))
    (get-output-string output)))


;;;
;;;; Pathname
;;;


(define jazz.executable-directory
  #f)


(define jazz.pathname-exists?
  file-exists?)


(define (jazz.file-modification-time pathname)
  (time->seconds (file-last-modification-time pathname)))


(define (jazz.copy-file src dst #!key (feedback #f))
  (if (jazz.file-needs-update? src dst)
      (begin
        (if feedback
            (feedback "; copying {a}..." src))
        (if (file-exists? dst)
            (delete-file dst))
        (copy-file src dst))))


(define (jazz.file-needs-update? src dst)
  (or (%%not (file-exists? dst))
      (> (jazz.file-modification-time src)
         (jazz.file-modification-time dst))))


(define (jazz.pathname-standardize path)
  (jazz.string-replace path #\\ #\/))


(define (jazz.pathname-normalize path #!optional (error? #t))
  (if (%%not (jazz.pathname-exists? path))
      (if error?
          (jazz.error "No such directory: {s}" path)
        #f)
    (let ((len (%%string-length path)))
      (let ((dir? (jazz.string-ends-with? path "/")))
        (let ((normalized (path-normalize (if dir? (%%substring path 0 (%%fx- len 1)) path))))
          (let ((standardized (jazz.pathname-standardize normalized)))
            (if (and dir? (%%not (jazz.string-ends-with? standardized "/")))
                (%%string-append standardized "/")
              standardized)))))))


(define (jazz.relativise-directory dir basedir)
  (let ((dir (jazz.pathname-normalize dir))
        (basedir (jazz.pathname-normalize basedir)))
    (let ((len (%%string-length dir))
          (baselen (%%string-length basedir)))
      (if (and (%%fx>= baselen len)
               (%%string=? (%%substring basedir 0 len) dir))
          (let ((suffix (%%substring basedir len baselen))
                (relative-dir ""))
            (let iter ((n (%%fx- (%%string-length suffix) 1)))
              (if (%%fx>= n 0)
                  (begin
                    (if (%%eqv? (%%string-ref suffix n) #\/)
                        (set! relative-dir (%%string-append relative-dir "../")))
                    (iter (%%fx- n 1)))))
            relative-dir)
        dir))))


(define (jazz.quote-gcc-pathname pathname platform)
  (case platform
    ((windows)
     (string-append "\"" pathname "\""))
    (else
     ;; quoting is only necessary on windows as arguments are passed explicitly in unix
     pathname)))
