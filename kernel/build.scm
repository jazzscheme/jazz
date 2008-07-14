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
;;;; Version
;;;


(define jazz.version
  "2.0b1")


(define jazz.gambit-version
  402008)


(define jazz.gambit-stamp
  1212469675)


(define (jazz.validate-version)
  (define (wrong-version message)
    (display message)
    (newline)
    (exit 1))
  
  (if (not jazz.gambit-stamp)
      (if (< (system-version) jazz.gambit-version)
          (wrong-version (jazz.format "Jazz needs Gambit version {a} or higher{%}Please see INSTALL for details" jazz.gambit-version)))
    (if (and (<= (system-version) jazz.gambit-version)
             (< (system-stamp) jazz.gambit-stamp))
        (wrong-version (jazz.format "Jazz needs Gambit version {a} stamp {a} or higher{%}Please see INSTALL for details" jazz.gambit-version jazz.gambit-stamp)))))


;;;
;;;; Configuration
;;;


(define (jazz.make-configuration name system platform windowing safety options install)
  (vector 'configuration name system platform windowing safety options install))

(define (jazz.configuration-name configuration)
  (vector-ref configuration 1))

(define (jazz.configuration-system configuration)
  (vector-ref configuration 2))

(define (jazz.configuration-platform configuration)
  (vector-ref configuration 3))

(define (jazz.configuration-windowing configuration)
  (vector-ref configuration 4))

(define (jazz.configuration-safety configuration)
  (vector-ref configuration 5))

(define (jazz.configuration-options configuration)
  (vector-ref configuration 6))

(define (jazz.configuration-install configuration)
  (vector-ref configuration 7))


(define (jazz.new-configuration
          #!key
          (name #f)
          (system #f)
          (platform #f)
          (windowing #f)
          (safety #f)
          (options '())
          (install #f))
  (jazz.make-configuration
    (jazz.validate-name name)
    (jazz.validate-system system)
    (jazz.validate-platform platform)
    (jazz.validate-windowing windowing)
    (jazz.validate-safety safety)
    (jazz.validate-options options)
    (jazz.validate-install install)))


;;;
;;;; Configurations
;;;


(define jazz.configurations-file
  "~/.jazz/.configurations")


(define jazz.configurations
  '())


(define (jazz.list-configurations)
  (for-each jazz.describe-configuration (jazz.sorted-configurations)))


(define (jazz.require-configuration name)
  (or (jazz.find-configuration name)
      (if (not name)
          (jazz.error "Unable to find default configuration")
        (jazz.error "Unable to find configuration: {s}" name))))

(define (jazz.require-default-configuration)
  (jazz.require-configuration #f))


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


(define (jazz.sorted-configurations)
  (jazz.sort jazz.configurations
             (lambda (c1 c2)
               (let ((n1 (jazz.configuration-name c1))
                     (n2 (jazz.configuration-name c2)))
                 (cond ((not n1)
                        #t)
                       ((not n2)
                        #f)
                       (else
                        (string-ci<? (symbol->string n1) (symbol->string n2))))))))


(define (jazz.register-configuration configuration)
  (let ((pair (jazz.find-configuration-pair (jazz.configuration-name configuration))))
    (if pair
        (set-car! pair configuration)
      (set! jazz.configurations (append jazz.configurations (list configuration)))))
  (jazz.save-configurations))


(define (jazz.delete-configuration name)
  (set! jazz.configurations
        (jazz.delete name jazz.configurations
          (lambda (c1 c2)
            (eq? (jazz.configuration-name c1)
                 (jazz.configuration-name c2)))))
  (jazz.save-configurations))


(define (jazz.load-configurations)
  (if (file-exists? jazz.configurations-file)
      (call-with-input-file (list path: jazz.configurations-file eol-encoding: 'cr-lf)
        (lambda (input)
          (define (read-configuration input)
            (let ((list (read input)))
              (if (eof-object? list)
                  list
                (apply jazz.new-configuration list))))
          
          (set! jazz.configurations (read-all input read-configuration))))))


(define (jazz.save-configurations)
  (jazz.create-directories "~/.jazz" feedback: jazz.feedback)
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
              (windowing (jazz.configuration-windowing configuration))
              (safety (jazz.configuration-safety configuration))
              (options (jazz.configuration-options configuration))
              (install (jazz.configuration-install configuration)))
          (display "(" output)
          (if name
              (print-property name: name))
          (print-property system: system)
          (print-property platform: platform)
          (if windowing
              (print-property windowing: windowing))
          (print-property safety: safety)
          (if (not (null? options))
              (print-property options: options))
          (if install
              (print-property install: install))
          (display ")" output)
          (newline output)))
      
      (for-each print-configuration (jazz.sorted-configurations)))))


(define (jazz.describe-configuration configuration)
  (let ((name (jazz.configuration-name configuration))
        (system (jazz.configuration-system configuration))
        (platform (jazz.configuration-platform configuration))
        (windowing (jazz.configuration-windowing configuration))
        (safety (jazz.configuration-safety configuration))
        (options (jazz.configuration-options configuration))
        (install (jazz.configuration-install configuration)))
    (jazz.feedback "{a}" (or name "<default>"))
    (jazz.feedback "  system: {s}" system)
    (jazz.feedback "  platform: {s}" platform)
    (if windowing
        (jazz.feedback "  windowing: {s}" windowing))
    (jazz.feedback "  safety: {s}" safety)
    (if (not (null? options))
        (jazz.feedback "  options: {s}" options))
    (if install
        (jazz.feedback "  install: {s}" install))))


;;;
;;;; Configure
;;;


(define (jazz.configure
          #!key
          (name #f)
          (system #f)
          (platform #f)
          (windowing #f)
          (safety #f)
          (options '())
          (install #f))
  (let* ((name (jazz.require-name name))
         (system (jazz.require-system system))
         (platform (jazz.require-platform platform))
         (windowing (jazz.require-windowing platform windowing))
         (safety (jazz.require-safety safety))
         (options (jazz.require-options options))
         (install (jazz.require-install install)))
    (let ((configuration
            (jazz.new-configuration
              name: name
              system: system
              platform: platform
              windowing: windowing
              safety: safety
              options: options
              install: install)))
      (jazz.register-configuration configuration)
      (jazz.describe-configuration configuration))))


;;;
;;;; Name
;;;


(define (jazz.require-name name)
  name)


(define (jazz.validate-name name)
  (if (or (not name) (and (symbol? name) (jazz.string-alphanumeric? (symbol->string name))))
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
    unix))


(define (jazz.require-platform platform)
  (or platform (jazz.guess-platform)))


(define (jazz.guess-platform)
  (let ((system (cadr (system-type)))
        (os (caddr (system-type))))
    (cond ((eq? system 'apple) 'mac)
          ((eq? os 'linux-gnu) 'unix)
          (else 'windows))))


(define (jazz.validate-platform platform)
  (if (memq platform jazz.valid-platforms)
      platform
    (jazz.error "Invalid platform: {s}" platform)))


(define (jazz.platform-name platform)
  (case platform
    ((mac) "Mac")
    ((windows) "Windows")
    ((unix) "Unix")))


;;;
;;;; Windowing
;;;


(define jazz.valid-windowings
  '(carbon
    #f
    x11))


(define (jazz.require-windowing platform windowing)
  (or windowing (jazz.guess-windowing platform)))


(define (jazz.guess-windowing platform)
  (case platform
    ((mac) 'x11) ;; until carbon is ready
    ((windows) #f)
    ((unix) 'x11)))


(define (jazz.validate-windowing windowing)
  (if (memq windowing jazz.valid-windowings)
      windowing
    (jazz.error "Invalid windowing: {s}" windowing)))


(define (jazz.windowing-name windowing)
  (if (not windowing)
      ""
    (case windowing
      ((carbon) "Carbon")
      ((x11) "X11"))))


;;;
;;;; Safety
;;;


(define jazz.default-safety
  'release)

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
  '(source
    interpret))


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


(define (jazz.source-option-name options)
  (if (memq 'source options)
      "Source"
    ""))


;;;
;;;; Install
;;;


(define (jazz.require-install install)
  install)


(define (jazz.validate-install install)
  (cond ((not install)
         #f)
        ((and (string? install) (> (string-length install) 0))
         (let ((len (string-length install)))
           (if (eqv? (string-ref install (- len 1)) #\/)
               install
             (string-append install "/"))))
        (else
         (jazz.error "Invalid install: {s}" install))))


(define (jazz.install-directory configuration)
  (or (jazz.configuration-install configuration)
      (string-append "bin/"
                     (jazz.system-name (jazz.configuration-system configuration))
                     (jazz.platform-name (jazz.configuration-platform configuration))
                     (jazz.windowing-name (jazz.configuration-windowing configuration))
                     (jazz.safety-name (jazz.configuration-safety configuration))
                     (jazz.source-option-name (jazz.configuration-options configuration))
                     "/")))


;;;
;;;; Make
;;;


(define jazz.default-target
  'jazz)


(define (jazz.make #!optional (target #f))
  (let ((target (or target jazz.default-target)))
    (let ((name (symbol->string target)))
      (let ((pos (jazz.string-find name #\@)))
        (if (not pos)
            (jazz.make-target target (jazz.require-default-configuration))
          (let ((target
                  (if (= pos 0)
                      jazz.default-target
                    (string->symbol (substring name 0 pos))))
                (configuration
                  (if (= (+ pos 1) (string-length name))
                      (jazz.require-default-configuration)
                    (jazz.require-configuration (string->symbol (substring name (+ pos 1) (string-length name)))))))
            (jazz.make-target target configuration)))))))


(define (jazz.make-target target configuration)
  (case target
    ((kernel) (jazz.make-kernel configuration))
    ((core) (jazz.make-core configuration))
    ((jazz) (jazz.make-jazz configuration))
    ((platform) (jazz.make-platform configuration))
    ((all) (jazz.make-all configuration))
    (else (jazz.make-product target configuration))))


;;;
;;;; Build
;;;


(define (jazz.build-recursive target configuration)
  (let ((configuration-name (jazz.configuration-name configuration))
        (gsc-path (if (eq? (jazz.configuration-platform configuration) 'windows)
                      "gsc"
                    "gsc-script")))
    (let ((target-argument (symbol->string target))
          (configuration-argument (if configuration-name (symbol->string configuration-name) "#f")))
      (jazz.execute-process gsc-path (list "-:dq-" "build" target-argument configuration-argument)))))


(define (jazz.build target configuration-name)
  (let ((configuration (jazz.require-configuration configuration-name)))
    (case target
      ((kernel) (jazz.build-kernel configuration))
      (else (jazz.error "Unknown build target: {s}" target))))
  (exit))


;;;
;;;; Kernel
;;;


(define (jazz.make-kernel configuration)
  (jazz.feedback "making kernel")
  (jazz.build-recursive 'kernel configuration))


(define (jazz.build-kernel configuration)
  (let ((system (jazz.configuration-system configuration))
        (platform (jazz.configuration-platform configuration))
        (windowing (jazz.configuration-windowing configuration))
        (safety (jazz.configuration-safety configuration))
        (options (jazz.configuration-options configuration))
        (install (jazz.install-directory configuration))
        (source "./"))
    (define (install-file path)
      (string-append install path))
    
    (define (generate-gambcini)
      (let ((file (install-file ".gambcini")))
        (if (not (file-exists? file))
            (begin
              (jazz.feedback "; generating {a}..." file)
              (call-with-output-file file
                (lambda (output)
                  (jazz.print ";;;" output)
                  (jazz.print ";;;===============" output)
                  (jazz.print ";;;  Jazz System" output)
                  (jazz.print ";;;===============" output)
                  (jazz.print ";;;" output)
                  (jazz.print ";;;; Gambit Ini" output)
                  (jazz.print ";;;" output)
                  (newline output)
                  (newline output)
                  (jazz.print-architecture system platform windowing safety options output)
                  (newline output)
                  (jazz.print-variable 'jazz.product #f output)
                  (newline output)
                  (jazz.print-variable 'jazz.install "." output)
                  (newline output)
                  (jazz.print-variable 'jazz.source (jazz.relativise-directory source install) output)
                  (newline output)
                  (newline output)
                  (display "(load (string-append jazz.source \"kernel/boot\"))" output)
                  (newline output)))))))
    
    (jazz.build-executable #f
      system:    system
      platform:  platform
      windowing: windowing
      safety:    safety
      options:   options
      install:   install
      source:    source
      kernel?:   #t
      console?:  #t)
    
    (if (memq 'interpret options)
        (generate-gambcini))))


;;;
;;;; Jazz
;;;


(define (jazz.jazz-make target configuration)
  (let ((install (jazz.install-directory configuration))
        (platform (jazz.configuration-platform configuration)))
    (define (install-file path)
      (string-append install path))
    
    (define (jazz-path)
      (case platform
        ((windows)
         (install-file "jazz"))
        (else
         "./jazz")))
    
    (jazz.feedback "making {a}" target)
    (jazz.execute-process (jazz-path) (list "-:dq-" "-build" (symbol->string target)) install)))


(define (jazz.make-core configuration)
  (jazz.make-kernel configuration)
  (jazz.jazz-make 'core configuration))


(define (jazz.make-jazz configuration)
  (jazz.make-core configuration)
  (jazz.jazz-make 'jazz configuration))


(define (jazz.make-platform configuration)
  (jazz.make-jazz configuration)
  (jazz.jazz-make 'platform configuration))


(define (jazz.make-product target configuration)
  ;; not 100% correct as it should really be jazz.make-jazz but this has to be
  ;; consistent with jazz.load-product-definition which is temporarely patched
  (jazz.make-platform configuration)
  (jazz.jazz-make target configuration))


;;;
;;;; All
;;;


(define (jazz.make-all configuration)
  (jazz.make-platform configuration)
  (jazz.jazz-make 'all configuration))


;;;
;;;; Output
;;;


(define (jazz.print line output)
  (display line output)
  (newline output))


(define (jazz.debug . rest)
  (jazz.print rest (console-port)))


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
;;;; List
;;;


(define (jazz.filter pred lis)
  (let recur ((lis lis))
    (if (null? lis) lis
      (let ((head (car lis))
            (tail (cdr lis)))
        (if (pred head)
            (let ((new-tail (recur tail)))
              (if (eq? tail new-tail) lis
                (cons head new-tail)))
          (recur tail))))))


(define (jazz.delete x lis test)
  (jazz.filter (lambda (y) (not (test x y))) lis))


(define (jazz.sort l smaller)
  (define (merge-sort l)
    (define (merge l1 l2)
      (cond ((null? l1) l2)
            ((null? l2) l1)
            (else
             (let ((e1 (car l1)) (e2 (car l2)))
               (if (smaller e1 e2)
                   (cons e1 (merge (cdr l1) l2))
                 (cons e2 (merge l1 (cdr l2))))))))
    
    (define (split l)
      (if (or (null? l) (null? (cdr l)))
          l
        (cons (car l) (split (cddr l)))))
    
    (if (or (null? l) (null? (cdr l)))
        l
      (let* ((l1 (merge-sort (split l)))
             (l2 (merge-sort (split (cdr l)))))
        (merge l1 l2))))
  
  (merge-sort l))


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


(define (jazz.string-alphanumeric? str)
  (let iter ((n (- (string-length str) 1)))
    (if (< n 0)
        #t
      (let ((c (string-ref str n)))
        (if (or (char-alphabetic? c)
                (char-numeric? c))
            (iter (- n 1))
          #f)))))


;;;
;;;; Error
;;;


(define (jazz.error fmt-string . rest)
  (let ((error-string (apply jazz.format fmt-string rest)))
    (error error-string)))


;;;
;;;; Repl
;;;


(define jazz.prompt
  "$ ")

(define jazz.debug-build-system?
  #f)


(define (jazz.build-system-repl)
  (let ((console (console-port)))
    (jazz.print (jazz.format "Jazz {a} Build System" jazz.version) console)
    (force-output console)
    (let loop ()
      (newline console)
      (display jazz.prompt console)
      (force-output console)
      (let ((command (read-line console)))
        (continuation-capture
          (lambda (stop)
            (with-exception-handler
              (lambda (exc)
                (jazz.debug-exception exc console #t jazz.debug-build-system?)
                (continuation-return stop #f))
              (lambda ()
                (jazz.process-command command console)))))
        (loop)))))


(define (jazz.process-command command output)
  (call-with-input-string command
    (lambda (input)
      (let ((command (read input))
            (arguments (read-all input read)))
        (case command
          ((list) (jazz.list-command arguments output))
          ((delete) (jazz.delete-command arguments output))
          ((configure) (jazz.configure-command arguments output))
          ((make) (jazz.make-command arguments output))
          ((help ?) (jazz.help-command arguments output))
          ((quit) (jazz.quit-command arguments output))
          (else (jazz.error "Unknown command: {s}" command)))))))


(define (jazz.list-command arguments output)
  (jazz.list-configurations))


(define (jazz.delete-command arguments output)
  (let ((name (if (null? arguments) #f (car arguments))))
    (jazz.delete-configuration (jazz.require-configuration name))
    (jazz.list-configurations)))


(define (jazz.configure-command arguments output)
  (apply jazz.configure arguments))


(define (jazz.make-command arguments output)
  (apply jazz.make arguments))


(define (jazz.help-command arguments output)
  (jazz.print "Commands are" output)
  (jazz.print "  list" output)
  (jazz.print "  delete [configuration]" output)
  (jazz.print "  configure [name:] [system:] [platform:] [windowing:] [safety:] [options:] [install:]" output)
  (jazz.print "  make [target]" output)
  (jazz.print "  help or ?" output)
  (jazz.print "  quit" output))


(define (jazz.quit-command arguments output)
  (exit))


;;;
;;;; Boot
;;;


(define (jazz.build-system-boot)
  (define (fatal message)
    (display message)
    (newline)
    (force-output)
    (exit 1))
  
  (let ((command-arguments (cdr (command-line))))
    (if (null? command-arguments)
        (jazz.build-system-repl)
      (let ((command (car command-arguments)))
        (if (equal? command "build")
            (let ((arguments (cdr command-arguments)))
              (if (= (length arguments) 2)
                  (let ((target-argument (car arguments))
                        (configuration-argument (cadr arguments)))
                    (let ((target (string->symbol target-argument))
                          (configuration-name (if (equal? configuration-argument "#f") #f (string->symbol configuration-argument))))
                      (jazz.build target configuration-name)))
                (fatal (jazz.format "Ill-formed build command: {s}" command-arguments))))
          (fatal (jazz.format "Unknown build system command: {s}" command)))))))


;;;
;;;; Kernel
;;;


(define jazz.system
  'gambit)

(define jazz.platform
  #f)

(define jazz.windowing
  #f)

(define jazz.safety
  'debug)


(load "kernel/syntax/macros")
(load "kernel/syntax/features")
(load "kernel/syntax/declares")
(load "kernel/syntax/primitives")
(load "kernel/runtime/build")


;;;
;;;; Initialize
;;;


(jazz.validate-version)
(jazz.load-configurations)
(jazz.build-system-boot)
