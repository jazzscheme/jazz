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
;;;  Portions created by the Initial Developer are Copyright (C) 1996-2006
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
;;;; Executable
;;;


(define (jazz.build-executable product
          #!key
          (system jazz.system)
          (platform jazz.platform)
          (windowing jazz.windowing)
          (safety jazz.safety)
          (options jazz.options)
          (install jazz.install)
          (source jazz.source)
          (kernel? #f)
          (console? #f)
          (feedback jazz.feedback))
  (let ((product-name (if (not product) "jazz" (symbol->string product))))
    (let ((product-dir (string-append install "build/_products/" product-name "/")))
      (define (source-file path)
        (string-append source path))
      
      (define (install-file path)
        (string-append install path))
      
      (define (product-file path)
        (string-append product-dir path))
      
      (define (feedback-message fmt-string . rest)
        (if feedback
            (apply feedback fmt-string rest)))
      
      (define (generate-architecture)
        (let ((file (product-file "architecture.scm")))
          (if (not (file-exists? file))
              (begin
                (feedback-message "; generating {a}..." file)
                (call-with-output-file file
                  (lambda (output)
                    (jazz.print-architecture system platform windowing safety options output)))
                #t)
            #f)))
      
      (define (generate-product)
        (let ((file (product-file "product.scm")))
          (if (not (file-exists? file))
              (begin
                (feedback-message "; generating {a}..." file)
                (call-with-output-file file
                  (lambda (output)
                    (jazz.print-variable 'jazz.product product output)
                    (newline output)
                    (jazz.print-variable 'jazz.version jazz.version output)
                    (newline output)
                    (jazz.print-variable 'jazz.install (jazz.pathname-normalize install) output)
                    (newline output)
                    (jazz.print-variable 'jazz.source (jazz.relativise-directory source install) output)))
                #t)
            #f)))
      
      (define (generate-resources)
        (case platform
          ((windows)
           (let ((file (product-file (string-append product-name ".ico"))))
             (if (not (file-exists? file))
                 (begin
                   (jazz.copy-file (source-file "etc/resources/windows/jazz.ico") file feedback: feedback)
                   #t)
               #f)))
          (else
           #f)))
      
      (define (compile-kernel)
        (let ((architecture? (generate-architecture))
              (product? (generate-product))
              (latest #f))
          (define (compile-file name dir output)
            (let ((src (string-append dir name ".scm"))
                  (dst (string-append output name ".c")))
              (if (jazz.file-needs-update? src dst)
                  (let ((path (string-append dir name))
                        (options (if (eq? safety 'release) '() '(debug))))
                    (feedback-message "; compiling {a}..." path)
                    (compile-file-to-c path options: options output: output)))
              (let ((seconds (time->seconds (file-last-modification-time dst))))
                (if (or (not latest) (> seconds latest))
                    (set! latest seconds)))))
          
          (define (compile-kernel-file path name)
            (compile-file name
                          (string-append (source-file "kernel/") path)
                          (install-file (string-append "build/_kernel/" path))))
          
          (generate-resources)
          
          (if kernel?
              (begin
                ;; load architecture
                (load (product-file "architecture"))
                
                ;; load syntax
                (load (source-file "kernel/syntax/macros"))
                (load (source-file "kernel/syntax/features"))
                (load (source-file "kernel/syntax/declares"))
                (load (source-file "kernel/syntax/primitives"))
                (load (source-file "kernel/syntax/syntax"))
                (load (source-file "kernel/syntax/runtime"))))
          
          (if architecture?
              (compile-file "architecture" product-dir product-dir))
          (if product?
              (compile-file "product" product-dir product-dir))
          (compile-kernel-file "syntax/" "macros")
          (compile-kernel-file "syntax/" "features")
          (compile-kernel-file "syntax/" "declares")
          (compile-kernel-file "syntax/" "primitives")
          (compile-kernel-file "syntax/" "syntax")
          (compile-kernel-file "syntax/" "runtime")
          (compile-kernel-file "runtime/" "build")
          (compile-kernel-file "runtime/" "settings")
          (compile-kernel-file "runtime/" "install")
          (compile-kernel-file "runtime/" "digest")
          (compile-kernel-file "runtime/" "kernel")
          (compile-kernel-file "runtime/" "main")
          
          (let ((link-file (product-file (string-append product-name ".c"))))
            (if (or (not (file-exists? link-file))
                    (> latest (time->seconds (file-last-modification-time link-file))))
                (begin
                  (feedback-message "; linking kernel...")
                  (link-incremental (list (product-file "architecture")
                                          (product-file "product")
                                          (install-file "build/_kernel/syntax/macros")
                                          (install-file "build/_kernel/syntax/features")
                                          (install-file "build/_kernel/syntax/declares")
                                          (install-file "build/_kernel/syntax/primitives")
                                          (install-file "build/_kernel/syntax/syntax")
                                          (install-file "build/_kernel/syntax/runtime")
                                          (install-file "build/_kernel/runtime/build")
                                          (install-file "build/_kernel/runtime/settings")
                                          (install-file "build/_kernel/runtime/install")
                                          (install-file "build/_kernel/runtime/digest")
                                          (install-file "build/_kernel/runtime/kernel")
                                          (install-file "build/_kernel/runtime/main"))
                                    output: link-file
                                    base: "~~/lib/_gambcgsc")
                  #t)
              #f))))
      
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
        (jazz.execute-process
          "gcc"
          `(,(jazz.quote-gcc-pathname (product-file "architecture.c") platform)
            ,(jazz.quote-gcc-pathname (product-file "product.c") platform)
            ,(jazz.quote-gcc-pathname (install-file "build/_kernel/syntax/macros.c") platform)
            ,(jazz.quote-gcc-pathname (install-file "build/_kernel/syntax/features.c") platform)
            ,(jazz.quote-gcc-pathname (install-file "build/_kernel/syntax/declares.c") platform)
            ,(jazz.quote-gcc-pathname (install-file "build/_kernel/syntax/primitives.c") platform)
            ,(jazz.quote-gcc-pathname (install-file "build/_kernel/syntax/syntax.c") platform)
            ,(jazz.quote-gcc-pathname (install-file "build/_kernel/syntax/runtime.c") platform)
            ,(jazz.quote-gcc-pathname (install-file "build/_kernel/runtime/build.c") platform)
            ,(jazz.quote-gcc-pathname (install-file "build/_kernel/runtime/settings.c") platform)
            ,(jazz.quote-gcc-pathname (install-file "build/_kernel/runtime/install.c") platform)
            ,(jazz.quote-gcc-pathname (install-file "build/_kernel/runtime/digest.c") platform)
            ,(jazz.quote-gcc-pathname (install-file "build/_kernel/runtime/kernel.c") platform)
            ,(jazz.quote-gcc-pathname (install-file "build/_kernel/runtime/main.c") platform)
            ,(jazz.quote-gcc-pathname (product-file (string-append product-name ".c")) platform)
            ,@(resource-files)
            ,(string-append "-I" (jazz.quote-gcc-pathname (path-expand "~~/include") platform))
            ,(string-append "-L" (jazz.quote-gcc-pathname (path-expand "~~/lib") platform))
            "-lgambc" "-lgambcgsc" ,@(link-libraries)
            ,@(link-options)
            "-o" ,(jazz.quote-gcc-pathname (install-file product-name) platform))))
      
      (define (executable-name)
        (case platform
          ((windows)
           (install-file (string-append product-name ".exe")))
          (else
           (install-file product-name))))
      
      (jazz.create-directories product-dir feedback: feedback)
      (jazz.create-directories (install-file "build/_kernel/syntax/") feedback: feedback)
      (jazz.create-directories (install-file "build/_kernel/runtime/") feedback: feedback)
      
      (if (or (compile-kernel)
              (not (file-exists? (executable-name))))
          (link-executable)))))


(define (jazz.print-architecture system platform windowing safety options output)
  (jazz.print-variable 'jazz.system system output)
  (newline output)
  (jazz.print-variable 'jazz.platform platform output)
  (newline output)
  (jazz.print-variable 'jazz.windowing windowing output)
  (newline output)
  (jazz.print-variable 'jazz.safety safety output)
  (newline output)
  (jazz.print-variable 'jazz.options options output))


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


(define (jazz.feedback fmt-string . rest)
  (display (apply jazz.format fmt-string rest))
  (newline)
  (force-output))


;;;
;;;; String
;;;


(define (jazz.string-replace str old new)
  (let ((cpy (string-copy str)))
    (let iter ((n (%%fx- (%%string-length cpy) 1)))
      (if (%%fx>= n 0)
          (begin
            (if (%%eqv? (%%string-ref cpy n) old)
                (%%string-set! cpy n new))
            (iter (%%fx- n 1)))))
    cpy))


(define (jazz.string-ends-with? str c)
  (%%eqv? (%%string-ref str (%%fx- (%%string-length str) 1)) c))


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
      (> (time->seconds (file-last-modification-time src))
         (time->seconds (file-last-modification-time dst)))))


(define (jazz.create-directory dir #!key (feedback #f))
  (if (%%not (file-exists? dir))
      (begin
        (if feedback
            (feedback "; creating {a}..." dir))
        (create-directory dir))))


(define (jazz.create-directories dir #!key (feedback #f))
  (let ((path (%%reverse (jazz.split-string dir #\/))))
    (let iter ((scan (if (%%equal? (%%car path) "") (%%cdr path) path)))
      (if (%%not (%%null? scan))
          (begin
            (iter (%%cdr scan))
            (let ((subdir (jazz.join-strings (%%reverse scan) "/")))
              (if (%%not (file-exists? subdir))
                  (jazz.create-directory subdir feedback: feedback))))))))


(define (jazz.pathname-normalize path)
  (let ((len (%%string-length path)))
    (let ((dir? (jazz.string-ends-with? path #\/)))
      (let ((normalized (path-normalize (if dir? (%%substring path 0 (%%fx- len 1)) path))))
        (let ((slashified (jazz.string-replace normalized #\\ #\/)))
          (if (and dir? (%%not (jazz.string-ends-with? slashified #\/)))
              (%%string-append slashified "/")
            slashified))))))


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


;;;
;;;; Process
;;;


(define (jazz.execute-process path arguments #!optional (directory #f))
  (let ((port (open-process
                (%%list
                  path: path
                  arguments: arguments
                  directory: (or directory (current-directory))
                  stdin-redirection: #f
                  stdout-redirection: #f
                  stderr-redirection: #f))))
    (let ((code (process-status port)))
      (if (%%not (%%fx= code 0))
          (jazz.error "failed")))))
