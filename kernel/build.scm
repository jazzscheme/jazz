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
;;;; Configurations
;;;


(define jazz.configurations-file
  ".configurations")


(define jazz.configurations
  '())

(define jazz.active-configuration-name
  #f)


(define (jazz.require-configuration)
  (or (jazz.find-configuration jazz.active-configuration-name)
      (jazz.error "Please use configure to specify an active configuration")))

(define (jazz.require-named-configuration name)
  (or (jazz.find-configuration name)
      (jazz.error "Unable to find configuration: {s}" name)))


(define (jazz.find-configuration name)
  (let ((pair (jazz.find-configuration-pair name)))
    (if (not pair)
        #f
      (car pair))))

(define (jazz.find-configuration-pair name)
  (let iter ((configurations jazz.configurations))
    (if (null? configurations)
        #f
      (let ((configuration (car configurations)))
        (if (eq? (jazz.configuration-name configuration) name)
            configurations
          (iter (cdr configurations)))))))


(define (jazz.make-configuration name system platform safety options directory)
  (vector 'configuration name system platform safety options directory))

(define (jazz.configuration-name configuration)
  (vector-ref configuration 1))

(define (jazz.configuration-system configuration)
  (vector-ref configuration 2))

(define (jazz.configuration-platform configuration)
  (vector-ref configuration 3))

(define (jazz.configuration-safety configuration)
  (vector-ref configuration 4))

(define (jazz.configuration-options configuration)
  (vector-ref configuration 5))

(define (jazz.configuration-directory configuration)
  (vector-ref configuration 6))


(define (jazz.new-configuration
          #!key
          (name #f)
          (system #f)
          (platform #f)
          (safety #f)
          (options '())
          (directory #f))
  (jazz.make-configuration
    (jazz.validate-name name)
    (jazz.validate-system system)
    (jazz.validate-platform platform)
    (jazz.validate-safety safety)
    (jazz.validate-options options)
    (jazz.validate-directory directory)))


(define (jazz.register-configuration configuration)
  (let ((pair (jazz.find-configuration-pair (jazz.configuration-name configuration))))
    (if pair
        (set-car! pair configuration)
      (set! jazz.configurations (append jazz.configurations (list configuration))))))


(define (jazz.load-configurations)
  (if (file-exists? jazz.configurations-file)
      (call-with-input-file jazz.configurations-file
        (lambda (input)
          (define (read-configuration input)
            (let ((list (read input)))
              (if (eof-object? list)
                  list
                (apply jazz.new-configuration list))))
          
          (set! jazz.configurations (read-all input read-configuration))))))


(define (jazz.save-configurations)
  (call-with-output-file jazz.configurations-file
    (lambda (output)
      (define (print-configuration configuration)
        (define first?
          #t)
        
        (define (print-property property value)
          (if first?
              (set! first? #f)
            (display " " output))
          (display property output)
          (display " " output)
          (write value output))
        
        (let ((name (jazz.configuration-name configuration))
              (system (jazz.configuration-system configuration))
              (platform (jazz.configuration-platform configuration))
              (safety (jazz.configuration-safety configuration))
              (options (jazz.configuration-options configuration))
              (directory (jazz.configuration-directory configuration)))
          (display "(" output)
          (if name
              (print-property name: name))
          (print-property system: system)
          (print-property platform: platform)
          (print-property safety: safety)
          (if (not (null? options))
              (print-property options: options))
          (if directory
              (print-property directory: directory))
          (display ")" output)
          (newline output)))
      
      (for-each print-configuration jazz.configurations))))


;;;
;;;; Configure
;;;


(define (jazz.configure name system platform safety options directory)
  (jazz.with-stop
    (lambda ()
      (let ((name (jazz.require-name name))
            (system (jazz.require-system system))
            (platform (jazz.require-platform platform))
            (safety (jazz.require-safety safety))
            (options (jazz.require-options options))
            (directory (jazz.require-directory directory)))
        (jazz.register-configuration (jazz.new-configuration
                                       name: name
                                       system: system
                                       platform: platform
                                       safety: safety
                                       options: options
                                       directory: directory))
        (jazz.save-configurations)
        (set! jazz.active-configuration-name name)
        (if name
            (jazz.print "name: {s}" name))
        (jazz.print "system: {s}" system)
        (jazz.print "platform: {s}" platform)
        (jazz.print "safety: {s}" safety)
        (if (not (null? options))
            (jazz.print "options: {s}" options))
        (if directory
            (jazz.print "directory: {s}" directory)))))
  (void))


;;;
;;;; Name
;;;


(define (jazz.require-name name)
  name)


(define (jazz.validate-name name)
  (if (or (not name) (and (symbol? name) (string-alphanumeric? (symbol->string name))))
      name
    (jazz.error "Invalid name: {s}" name)))


;;;
;;;; Features
;;;


(define (jazz.unspecified-feature feature)
  (jazz.error "Please specify the {a}" feature))


;;;
;;;; System
;;;


(cond-expand
  (gambit
    (define jazz.default-system
      'gambit))
  (else
    (define jazz.default-system
      #f)))

(define jazz.valid-systems
  '(gambit))


(define (jazz.require-system system)
  (or system jazz.default-system (jazz.unspecified-feature "system")))


(define (jazz.validate-system system)
  (if (memq system jazz.valid-systems)
      system
    (jazz.error "Invalid system: {s}" system)))


(define (jazz.system-name system)
  (case system
    ((gambit) "Gambit")))


;;;
;;;; Platform
;;;


(define jazz.valid-platforms
  '(mac
    windows
    x11))


(define (jazz.require-platform platform)
  (or platform (jazz.guess-platform)))


(define (jazz.guess-platform)
  (case (cadr (system-type))
    ;; mac version not yet available
    ;; ((apple) 'mac)
    ((pc) 'windows)
    (else 'x11)))


(define (jazz.validate-platform platform)
  (if (memq platform jazz.valid-platforms)
      platform
    (jazz.error "Invalid platform: {s}" platform)))


(define (jazz.platform-name platform)
  (case platform
    ((mac) "Mac")
    ((windows) "Windows")
    ((x11) "X11")))


;;;
;;;; Safety
;;;


(define jazz.default-safety
  'debug)

(define jazz.valid-safeties
  '(core
    debug
    release))


(define (jazz.require-safety safety)
  (or safety jazz.default-safety (jazz.unspecified-feature "safety")))


(define (jazz.validate-safety safety)
  (if (memq safety jazz.valid-safeties)
      safety
    (jazz.error "Invalid safety: {s}" safety)))


(define (jazz.safety-name safety)
  (case safety
    ((core) "Core")
    ((debug) "Debug")
    ((release) "Release")))


;;;
;;;; Options
;;;


(define jazz.valid-options
  '(interpret
    profile))


(define (jazz.require-options options)
  options)


(define (jazz.validate-options options)
  (if (list? options)
      (begin
        (for-each (lambda (option)
                    (if (not (memq option jazz.valid-options))
                        (jazz.error "Invalid option: {s}" option)))
                  options)
        options)
    (jazz.error "Invalid options: {s}" options)))


(define (jazz.profile-name options)
  (if (memq 'profile options)
      "Profile"
    ""))


;;;
;;;; Directory
;;;


(define (jazz.require-directory directory)
  directory)


(define (jazz.validate-directory directory)
  (cond ((not directory)
         #f)
        ((and (string? directory) (> (string-length directory) 0))
         (let ((len (string-length directory)))
           (if (eqv? (string-ref directory (- len 1)) #\/)
               directory
             (string-append directory "/"))))
        (else
         (jazz.error "Invalid directory: {s}" directory))))


(define (jazz.effective-directory configuration)
  (or (jazz.configuration-directory configuration)
      (string-append "bin/"
                     (jazz.system-name (jazz.configuration-system configuration))
                     (jazz.platform-name (jazz.configuration-platform configuration))
                     (jazz.safety-name (jazz.configuration-safety configuration))
                     (jazz.profile-name (jazz.configuration-options configuration))
                     "/")))


;;;
;;;; Make
;;;


(define jazz.default-target
  'jazz)


(define (jazz.make target)
  (if (jazz.with-stop
        (lambda ()
          (let ((target (or target jazz.default-target)))
            (let ((name (symbol->string target)))
              (let ((pos (jazz.string-find name #\@)))
                (if (not pos)
                    (jazz.make-target target (jazz.require-configuration))
                  (let ((target
                          (if (= pos 0)
                              jazz.default-target
                            (string->symbol (substring name 0 pos))))
                        (configuration
                          (if (= (+ pos 1) (string-length name))
                              (jazz.require-configuration)
                            (jazz.require-named-configuration (string->symbol (substring name (+ pos 1) (string-length name)))))))
                    (jazz.make-target target configuration))))))
          #t))
      (void)
    (begin
      (jazz.print "failed")
      (void))))


(define (jazz.make-target target configuration)
  (case target
    ((kernel) (jazz.make-kernel configuration))
    ((core) (jazz.make-core configuration))
    ((jazz) (jazz.make-jazz configuration))
    ((jedi) (jazz.make-jedi configuration))
    (else (jazz.error "Unknown target: {s}" target))))


(define (jazz.make-target-recursive target configuration)
  (let ((configuration-name (jazz.configuration-name configuration)))
    (let ((target-argument (symbol->string target))
          (configuration-argument (if configuration-name (symbol->string configuration-name) "#f")))
      (jazz.shell-command (string-append "gsc -e \"(jazz.recursive-make '" target-argument " '" configuration-argument ")\"")))))


(define (jazz.recursive-make target configuration-name)
  (if (jazz.with-stop
        (lambda ()
          (let ((configuration (jazz.require-named-configuration configuration-name)))
            (case target
              ((kernel) (jazz.make-kernel-recursive configuration))
              (else (jazz.error "Unknown target: {s}" target))))
          #t))
      0
    1))


(define (jazz.target-needs-update? src dst)
  (or (not (file-exists? dst))
      (> (time->seconds (file-last-modification-time src))
         (time->seconds (file-last-modification-time dst)))))


;;;
;;;; Kernel
;;;


(define (jazz.make-kernel configuration)
  (jazz.print "making kernel")
  (jazz.make-target-recursive 'kernel configuration))


(define (jazz.make-kernel-recursive configuration)
  (let ((system (jazz.configuration-system configuration))
        (platform (jazz.configuration-platform configuration))
        (safety (jazz.configuration-safety configuration))
        (options (jazz.configuration-options configuration))
        (directory (jazz.effective-directory configuration)))
    (define (conffile path)
      (string-append directory path))
    
    (define (create-confdir dir)
      (jazz.create-directory (conffile dir)))
    
    (define (print line output)
      (display line output)
      (newline output))
    
    (define (print-variable variable value output)
      (display "(define " output)
      (display variable output)
      (newline output)
      (display "  '" output)
      (write value output)
      (display ")" output)
      (newline output))
    
    (define (print-architecture output)
      (print-variable 'jazz.system system output)
      (newline output)
      (print-variable 'jazz.platform platform output)
      (newline output)
      (print-variable 'jazz.safety safety output)
      (newline output)
      (print-variable 'jazz.options options output))
    
    (define (generate-architecture)
      (let ((file (conffile "build/kernel/architecture.scm")))
        (if (not (file-exists? file))
            (begin
              (jazz.print "; generating {a} ..." file)
              (call-with-output-file file
                print-architecture)
              #t)
          #f)))
    
    (define (compile-kernel)
      (let ((architecture? (generate-architecture))
            (needs-linking? #f))
        (define (compile-file name dir output)
          (let ((src (string-append dir name ".scm"))
                (dst (string-append output name ".c")))
            (if (jazz.target-needs-update? src dst)
                (begin
                  (let ((path (string-append dir name)))
                    (jazz.print "; compiling {a} ..." path)
                    (compile-file-to-c path output: output))
                  (set! needs-linking? #t)))))
        
        (define (compile-kernel-file path name)
          (compile-file name
                        (string-append "kernel/" path)
                        (conffile (string-append "build/kernel/" path))))
        
        (load (conffile "build/kernel/architecture"))
        (load "kernel/syntax/macros")
        (load "kernel/syntax/features")
        (load "kernel/syntax/primitives")
        (load "kernel/syntax/syntax")
        (load "kernel/syntax/runtime")
        
        (if architecture?
            (compile-file "architecture"
                          (conffile "build/kernel/")
                          (conffile "build/kernel/")))
        (compile-kernel-file "syntax/" "macros")
        (compile-kernel-file "syntax/" "features")
        (compile-kernel-file "syntax/" "primitives")
        (compile-kernel-file "syntax/" "syntax")
        (compile-kernel-file "syntax/" "runtime")
        (compile-kernel-file "runtime/" "config")
        (compile-kernel-file "runtime/" "digest")
        (compile-kernel-file "runtime/" "kernel")
        (compile-kernel-file "runtime/" "main")
            
        (if needs-linking?
            (begin
              (jazz.print "; linking kernel ...")
              (link-incremental (list (conffile "build/kernel/architecture")
                                      (conffile "build/kernel/syntax/macros")
                                      (conffile "build/kernel/syntax/features")
                                      (conffile "build/kernel/syntax/primitives")
                                      (conffile "build/kernel/syntax/syntax")
                                      (conffile "build/kernel/syntax/runtime")
                                      (conffile "build/kernel/runtime/config")
                                      (conffile "build/kernel/runtime/digest")
                                      (conffile "build/kernel/runtime/kernel")
                                      (conffile "build/kernel/runtime/main"))
                                output: (conffile "build/kernel/runtime/jazz.c")
                                base: "~~/lib/_gambcgsc")
              #t)
          #f)))
    
    (define (link-libraries)
      (case platform
        ((windows)
         "-lws2_32 ")
        (else
         "")))
    
    (define (link-options)
      (case platform
        ((windows)
         "-mconsole ")
        (else
         "")))
    
    (define (link-kernel)
      (jazz.print "; linking executable ...")
      (jazz.shell-command
        (string-append
          "gcc "
          (conffile "build/kernel/architecture.c ")
          (conffile "build/kernel/syntax/macros.c ")
          (conffile "build/kernel/syntax/features.c ")
          (conffile "build/kernel/syntax/primitives.c ")
          (conffile "build/kernel/syntax/syntax.c ")
          (conffile "build/kernel/syntax/runtime.c ")
          (conffile "build/kernel/runtime/config.c ")
          (conffile "build/kernel/runtime/digest.c ")
          (conffile "build/kernel/runtime/kernel.c ")
          (conffile "build/kernel/runtime/main.c ")
          (conffile "build/kernel/runtime/jazz.c ")
          "-I" (path-expand "~~/include") " "
          "-L" (path-expand "~~/lib") " "
          "-lgambc -lgambcgsc " (link-libraries)
          (link-options)
          "-o " (conffile "jazz"))))
    
    (define (executable-name)
      (case platform
        ((windows)
         (conffile "jazz.exe"))
        (else
         (conffile "jazz"))))
    
    (define (copy-platform-file src dst)
      (if (jazz.target-needs-update? src dst)
          (begin
            (jazz.print "; copying {a} ..." src)
            (copy-file src dst))))
    
    (define (copy-platform-files)
      (case platform
        ((windows)
         (copy-platform-file "foreign/cairo/lib/windows/libcairo-2.dll" (conffile "libcairo-2.dll"))
         (copy-platform-file "foreign/png/lib/windows/libpng13.dll" (conffile "libpng13.dll"))
         (copy-platform-file "foreign/zlib/lib/windows/zlib1.dll" (conffile "zlib1.dll")))))
    
    (define (generate-gambcini)
      (let ((file (conffile ".gambcini")))
        (if (not (file-exists? file))
            (begin
              (jazz.print "; generating {a} ..." file)
              (call-with-output-file file
                (lambda (output)
                  (print ";;;" output)
                  (print ";;;===============" output)
                  (print ";;;  Jazz System" output)
                  (print ";;;===============" output)
                  (print ";;;" output)
                  (print ";;;; Gambit Ini" output)
                  (print ";;;" output)
                  (newline output)
                  (newline output)
                  (print-architecture output)
                  (newline output)
                  (newline output)
                  (display "(load \"../../kernel/boot\")" output)
                  (newline output)))))))
    
    (jazz.create-directory "bin/")
    (jazz.create-directory directory)
    
    (create-confdir "build/")
    (create-confdir "build/kernel/")
    (create-confdir "build/kernel/syntax/")
    (create-confdir "build/kernel/runtime/")
    
    (if (or (compile-kernel)
            (not (file-exists? (executable-name))))
        (link-kernel))
    
    (copy-platform-files)
    
    (if (memq 'interpret options)
        (generate-gambcini))))


;;;
;;;; Jazz
;;;


(define (jazz.jazz-make target configuration)
  (let ((directory (jazz.effective-directory configuration)))
    (define (conffile path)
      (string-append directory path))
    
    (jazz.print "making {a}" target)
    (jazz.shell-command (string-append "jazz -make " (symbol->string target)) directory)))


(define (jazz.make-core configuration)
  (jazz.make-kernel configuration)
  (jazz.jazz-make 'core configuration))


(define (jazz.make-jazz configuration)
  (jazz.make-core configuration)
  (jazz.jazz-make 'jazz configuration))


(define (jazz.make-jedi configuration)
  (jazz.make-jazz configuration)
  (jazz.jazz-make 'jedi configuration))


;;;
;;;; Format
;;;


(define (jazz.format fmt-string . arguments)
  (let ((output (open-output-string)))
    (jazz.format-to output fmt-string arguments)
    (get-output-string output)))


(define (jazz.format-to output fmt-string arguments)
  (let ((control (open-input-string fmt-string))
        (done? #f))
    (define (format-directive)
      (let ((directive (read control)))
        (read-char control)
        (case directive
          ((a)
           (display (car arguments) output)
           (set! arguments (cdr arguments)))
          ((s)
           (write (car arguments) output)
           (set! arguments (cdr arguments)))
          ((%)
           (newline output)))))
    
    (let iter ()
      (let ((c (read-char control)))
        (if (not (eof-object? c))
            (begin
              (cond ((eqv? c #\~)
                     (write-char (read-char control) output))
                    ((eqv? c #\{)
                     (format-directive))
                    (else
                     (write-char c output)))
              (iter)))))))


;;;
;;;; Print
;;;


(define (jazz.print fmt-string . rest)
  (display (apply jazz.format fmt-string rest))
  (newline)
  (force-output))


;;;
;;;; Error
;;;


(define jazz.stop-continuation
  (make-parameter #f))


(define (jazz.with-stop thunk)
  (call/cc
    (lambda (stop)
      (parameterize ((jazz.stop-continuation stop))
        (thunk)))))


(define (jazz.stop)
  ((jazz.stop-continuation) #f))


(define (jazz.error fmt-string . rest)
  (apply jazz.print fmt-string rest)
  (jazz.stop))


(cond-expand
  (gambit
    (current-exception-handler
      (lambda (exc)
        (##display-exception exc (current-output-port))
        (let ((stop (jazz.stop-continuation)))
          (if stop
              (stop #f)))))))


;;;
;;;; String
;;;


(define (jazz.string-find str c)
  (let ((len (string-length str)))
    (let iter ((n 0))
      (cond ((>= n len)
             #f)
            ((char=? (string-ref str n) c)
             n)
            (else
             (iter (+ n 1)))))))


(define (string-alphanumeric? str)
  (let iter ((n (- (string-length str) 1)))
    (if (< n 0)
        #t
      (let ((c (string-ref str n)))
        (if (or (char-alphabetic? c)
                (char-numeric? c))
            (iter (- n 1))
          #f)))))


;;;
;;;; Pathname
;;;


(define (jazz.create-directory dir)
  (if (not (file-exists? dir))
      (begin
        (jazz.print "; creating {a} ..." dir)
        (create-directory dir))))


;;;
;;;; Process
;;;


(define (jazz.shell-command command #!optional (directory #f))
  (let ((code (shell-command command (or directory (current-directory)))))
    (if (not (= code 0))
        (jazz.stop))))


;;;
;;;; Load
;;;


(jazz.load-configurations)
