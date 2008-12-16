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


(define (jazz.setup-versions)
  (set! jazz.source-versions-file "kernel/versions")
  (jazz.validate-gambit-version))


(define (jazz.validate-gambit-version)
  (define (wrong-version message)
    (display message)
    (newline)
    (exit 1))
  
  (if (not (jazz.gambit-uptodate? (system-version) (system-stamp)))
      (let ((gambit-version (jazz.get-gambit-version))
            (gambit-stamp (jazz.get-gambit-stamp)))
        (let ((stamp (if gambit-stamp (jazz.format " stamp {a}" gambit-stamp) "")))
          (wrong-version
            (jazz.format "JazzScheme needs Gambit version {a}{a} or higher to build{%}See INSTALL for details on installing the latest version of Gambit"
                         gambit-version
                         stamp
                         gambit-stamp))))))


;;;
;;;; Configuration
;;;


(define (jazz.make-configuration name system platform windowing safety optimize? include-source? interpret? source destination)
  (vector 'configuration name system platform windowing safety optimize? include-source? interpret? source destination))

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

(define (jazz.configuration-optimize? configuration)
  (vector-ref configuration 6))

(define (jazz.configuration-include-source? configuration)
  (vector-ref configuration 7))

(define (jazz.configuration-interpret? configuration)
  (vector-ref configuration 8))

(define (jazz.configuration-source? configuration)
  (vector-ref configuration 9))

(define (jazz.configuration-destination configuration)
  (vector-ref configuration 10))


(define (jazz.new-configuration
          #!key
          (name #f)
          (system #f)
          (platform #f)
          (windowing #f)
          (safety #f)
          (optimize? #t)
          (include-source? #f)
          (interpret? #f)
          (source #t)
          (destination #f))
  (jazz.make-configuration
    (jazz.validate-name name)
    (jazz.validate-system system)
    (jazz.validate-platform platform)
    (jazz.validate-windowing windowing)
    (jazz.validate-safety safety)
    (jazz.validate-optimize? optimize?)
    (jazz.validate-include-source? include-source?)
    (jazz.validate-interpret? interpret?)
    (jazz.validate-source source)
    (jazz.validate-destination destination)))


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
  (let ((configuration
          (let ((pair (jazz.find-configuration-pair name)))
            (if (not pair)
                #f
              (car pair)))))
    ;; special case to support make of binaries
    (if (and (not name) (not configuration))
        (let ((configuration-dir (jazz.destination-directory #f "bin:" "./")))
          (let ((configuration-file (string-append configuration-dir ".configuration")))
            (if (file-exists? configuration-file)
                (jazz.load-configuration-file configuration-file)
              #f)))
      configuration)))

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


(define (jazz.load-configuration-file file)
  (call-with-input-file (list path: file eol-encoding: 'cr-lf)
    (lambda (input)
      (apply jazz.new-configuration (read input)))))


(define (jazz.save-configurations)
  (jazz.create-directories "~/.jazz" feedback: jazz.feedback)
  (call-with-output-file jazz.configurations-file
    (lambda (output)
      (for-each (lambda (configuration)
                  (jazz.print-configuration
                    (jazz.configuration-name configuration)
                    (jazz.configuration-system configuration)
                    (jazz.configuration-platform configuration)
                    (jazz.configuration-windowing configuration)
                    (jazz.configuration-safety configuration)
                    (jazz.configuration-optimize? configuration)
                    (jazz.configuration-include-source? configuration)
                    (jazz.configuration-interpret? configuration)
                    (jazz.configuration-source? configuration)
                    (jazz.configuration-destination configuration)
                    output))
                (jazz.sorted-configurations)))))


(define (jazz.describe-configuration configuration)
  (let ((name (jazz.configuration-name configuration))
        (system (jazz.configuration-system configuration))
        (platform (jazz.configuration-platform configuration))
        (windowing (jazz.configuration-windowing configuration))
        (safety (jazz.configuration-safety configuration))
        (optimize? (jazz.configuration-optimize? configuration))
        (include-source? (jazz.configuration-include-source? configuration))
        (interpret? (jazz.configuration-interpret? configuration))
        (source? (jazz.configuration-source? configuration))
        (destination (jazz.configuration-destination configuration)))
    (jazz.feedback "{a}" (or name "<default>"))
    (jazz.feedback "  system: {s}" system)
    (jazz.feedback "  platform: {s}" platform)
    (if windowing
        (jazz.feedback "  windowing: {s}" windowing))
    (jazz.feedback "  safety: {s}" safety)
    (if (not optimize?)
        (jazz.feedback "  optimize?: {s}" optimize?))
    (if include-source?
        (jazz.feedback "  include-source?: {s}" include-source?))
    (if interpret?
        (jazz.feedback "  interpret?: {s}" interpret?))
    (if (not (eqv? source? #t))
        (jazz.feedback "  source?: {s}" source?))
    (if destination
        (jazz.feedback "  destination: {s}" destination))))


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
          (optimize? #t)
          (include-source? #f)
          (interpret? #f)
          (source #t)
          (destination #f))
  (let* ((name (jazz.require-name name))
         (system (jazz.require-system system))
         (platform (jazz.require-platform platform))
         (windowing (jazz.require-windowing platform windowing))
         (safety (jazz.require-safety safety))
         (optimize? (jazz.require-optimize? optimize?))
         (include-source? (jazz.require-include-source? include-source?))
         (interpret? (jazz.require-interpret? interpret?))
         (source (jazz.require-source source))
         (destination (jazz.require-destination destination)))
    (let ((configuration
            (jazz.new-configuration
              name: name
              system: system
              platform: platform
              windowing: windowing
              safety: safety
              optimize?: optimize?
              include-source?: include-source?
              interpret?: interpret?
              source: source
              destination: destination)))
      (jazz.register-configuration configuration)
      (jazz.describe-configuration configuration))))


;;;
;;;; Name
;;;


(define (jazz.require-name name)
  name)


(define (jazz.validate-name name)
  (if (or (not name) (and (symbol? name) (jazz.valid-filename? (symbol->string name))))
      name
    (jazz.error "Invalid name: {s}" name)))


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


;;;
;;;; Optimize
;;;


(define jazz.valid-optimize
  '(#f
    #t))


(define (jazz.require-optimize? optimize)
  optimize)


(define (jazz.validate-optimize? optimize)
  (if (memq optimize jazz.valid-optimize)
      optimize
    (jazz.error "Invalid optimize?: {s}" optimize)))


;;;
;;;; Include-Source
;;;


(define jazz.valid-include-source
  '(#f
    #t))


(define (jazz.require-include-source? include-source)
  include-source)


(define (jazz.validate-include-source? include-source)
  (if (memq include-source jazz.valid-include-source)
      include-source
    (jazz.error "Invalid include-source?: {s}" include-source)))


;;;
;;;; Interpret
;;;


(define jazz.valid-interpret
  '(#f
    #t))


(define (jazz.require-interpret? interpret)
  interpret)


(define (jazz.validate-interpret? interpret)
  (if (memq interpret jazz.valid-interpret)
      interpret
    (jazz.error "Invalid interpret?: {s}" interpret)))


;;;
;;;; Source
;;;


(define (jazz.require-source source)
  source)


(define (jazz.validate-source source)
  (if (or (eqv? source #f)
          (eqv? source #t))
      source
    (jazz.error "Invalid source: {s}" source)))


;;;
;;;; Destination
;;;


(define (jazz.require-destination destination)
  destination)


(define (jazz.validate-destination destination)
  (if (or (not destination) (and (string? destination)
                                 (jazz.parse-destination destination
                                   (lambda (alias title)
                                     (and (or (not alias) (memq alias '(user jazz bin)))
                                          (or (not title) (jazz.valid-filename? title)))))))
      destination
    (jazz.error "Invalid destination: {s}" destination)))


(define (jazz.configuration-directory configuration)
  (jazz.destination-directory
    (jazz.configuration-name configuration)
    (jazz.configuration-destination configuration)
    "./"))


(define (jazz.configuration-file configuration)
  (let ((dir (jazz.configuration-directory configuration)))
    (string-append dir ".configuration")))


;;;
;;;; Features
;;;


(define (jazz.unspecified-feature feature)
  (jazz.error "Please specify the {a}" feature))


;;;
;;;; Make
;;;


(define jazz.default-target
  'jazz)


(define (jazz.make symbols)
  (define (make-symbol symbol)
    (let ((name (symbol->string symbol)))
      (let ((pos (jazz.string-find name #\@)))
        (if (not pos)
            (jazz.make-target symbol (jazz.require-default-configuration))
          (let ((target
                  (if (= pos 0)
                      jazz.default-target
                    (string->symbol (substring name 0 pos))))
                (configuration
                  (if (= (+ pos 1) (string-length name))
                      (jazz.require-default-configuration)
                    (jazz.require-configuration (string->symbol (substring name (+ pos 1) (string-length name)))))))
            (jazz.make-target target configuration))))))
  
  (let iter ((scan (if (null? symbols)
                       (list jazz.default-target)
                     symbols)))
    (if (not (null? scan))
        (let ((symbol (car scan)))
          (make-symbol symbol)
          (let ((tail (cdr scan)))
            (if (not (null? tail))
                (newline (console-port)))
            (iter tail))))))


(define (jazz.make-target target configuration)
  (case target
    ((clean) (jazz.make-clean configuration))
    ((install) (jazz.make-install configuration))
    ((kernel) (jazz.make-kernel configuration))
    (else (jazz.make-product target configuration))))


;;;
;;;; Build
;;;


(define (jazz.build-recursive target configuration)
  (let ((configuration-name (jazz.configuration-name configuration)))
    (let ((target-argument (symbol->string target))
          (configuration-argument-list (if configuration-name (list (symbol->string configuration-name)) (list)))
          (gsc-path (if (eq? (jazz.configuration-platform configuration) 'windows)
                        "gsc"
                      "gsc-script")))
      (jazz.call-process gsc-path `("-:dq-" "-build" ,target-argument ,@configuration-argument-list)))))


(define (jazz.build target configuration-name)
  (case target
    ((kernel) (jazz.build-kernel configuration-name))
    (else (jazz.error "Unknown build target: {s}" target)))
  (exit))


;;;
;;;; Clean
;;;


(define (jazz.make-clean configuration)
  (jazz.feedback "make clean")
  (let ((dest (jazz.configuration-directory configuration)))
    (define (empty-dir dir level)
      (for-each (lambda (name)
                  (let ((path (string-append dir name)))
                    (if (< level 2)
                        (jazz.feedback "; deleting {a}..." path))
                    (delete-file path)))
                (jazz.directory-files dir))
      (for-each (lambda (name)
                  (let ((path (string-append dir name "/")))
                    (if (< level 2)
                        (jazz.feedback "; deleting {a}..." path))
                    (delete-dir path (+ level 1))))
                (jazz.directory-directories dir)))
    
    (define (delete-dir dir level)
      (empty-dir dir level)
      (delete-directory dir))
    
    (if (file-exists? dest)
        (delete-dir dest 0))))


;;;
;;;; Install
;;;


(define (jazz.make-install configuration)
  (jazz.error "Make install is not supported. See INSTALL for details"))


;;;
;;;; Kernel
;;;


(define (jazz.make-kernel configuration)
  (jazz.feedback "make kernel")
  (jazz.build-recursive 'kernel configuration))


(define (jazz.build-kernel #!optional (configuration-name #f))
  (define (build configuration)
    (let ((name (jazz.configuration-name configuration))
          (system (jazz.configuration-system configuration))
          (platform (jazz.configuration-platform configuration))
          (windowing (jazz.configuration-windowing configuration))
          (safety (jazz.configuration-safety configuration))
          (optimize? (jazz.configuration-optimize? configuration))
          (include-source? (jazz.configuration-include-source? configuration))
          (interpret? (jazz.configuration-interpret? configuration))
          (source "./")
          (source-access? (jazz.configuration-source? configuration))
          (destination (jazz.configuration-destination configuration))
          (destination-directory (jazz.configuration-directory configuration)))
      (jazz.build-executable #f
        system:                system
        platform:              platform
        windowing:             windowing
        safety:                safety
        optimize?:             optimize?
        include-source?:       include-source?
        interpret?:            interpret?
        source:                source
        source-access?:        source-access?
        destination:           destination
        destination-directory: destination-directory
        kernel?:               #t
        console?:              #t)))
  
    (let ((configuration (jazz.require-configuration configuration-name)))
      (let ((configuration-file (jazz.configuration-file configuration)))
        (if (file-exists? configuration-file)
            (build (jazz.load-configuration-file configuration-file))
          (build configuration)))))


;;;
;;;; Product
;;;


(define (jazz.make-product product configuration)
  (jazz.make-kernel configuration)
  (jazz.product-make product configuration))


(define (jazz.product-make product configuration)
  (let ((destdir (jazz.configuration-directory configuration))
        (platform (jazz.configuration-platform configuration)))
    (define (build-file path)
      (string-append destdir path))
    
    (define (jazz-path)
      (case platform
        ((windows)
         (build-file "jazz"))
        (else
         "./jazz")))
    
    (jazz.call-process (jazz-path) (list "-:dq-" "-make" (symbol->string product)) destdir)))


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
;;;; Pathname
;;;


(define (jazz.valid-filename? str)
  (let iter ((n (- (string-length str) 1)))
    (if (< n 0)
        #t
      (let ((c (string-ref str n)))
        (if (or (char-alphabetic? c)
                (char-numeric? c)
                (memv c '(#\- #\_)))
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
  "% ")

(define jazz.debug-build-system?
  #f)


(define (jazz.build-system-repl)
  (let ((console (console-port)))
    (jazz.print (jazz.format "JazzScheme Build System v{a}" (jazz.present-version (jazz.get-source-version-number))) console)
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
  (jazz.make arguments))


(define (jazz.help-command arguments output)
  (jazz.print "Commands are" output)
  (jazz.print "  list" output)
  (jazz.print "  delete [configuration]" output)
  (jazz.print "  configure [name:] [system:] [platform:] [windowing:] [safety:] [optimize?:] [include-source?:] [interpret?:] [destination:]" output)
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
  
  (define (option? arg)
    (and (< 0 (string-length arg))
         (or (char=? (string-ref arg 0) #\-)
             (char=? (string-ref arg 0) #\/))))
  
  (define (convert-option arg)
    (substring arg 1 (string-length arg)))
  
  (let ((command-arguments (cdr (command-line))))
    (if (null? command-arguments)
        (jazz.build-system-repl)
      (let ((arg (car command-arguments)))
        (cond ((and (option? arg)
                    (equal? (convert-option arg) "debug"))
               (##repl-debug-main))
              ((or (and (option? arg)
                        (equal? (convert-option arg) "build"))
                   ;; support the old format so we can git bisect
                   ;; and remove when enough time has passed...
                   (equal? arg "build"))
               (let ((arguments (cdr command-arguments)))
                 (define (build target-argument configuration-argument)
                   (let ((target (string->symbol target-argument))
                         (configuration-name (and configuration-argument (string->symbol configuration-argument))))
                     (jazz.build target configuration-name)))
                 
                 (case (length arguments)
                   ((1)
                    (build (car arguments) #f))
                   ((2)
                    (build (car arguments) (cadr arguments)))
                   (else
                    (fatal (jazz.format "Ill-formed build command: {s}" command-arguments))))))
              (fatal (jazz.format "Unknown build system command: {s}" arg)))))))


;;;
;;;; Kernel
;;;


(define jazz.kernel-system
  'gambit)

(define jazz.kernel-platform
  #f)

(define jazz.kernel-windowing
  #f)

(define jazz.kernel-safety
  'debug)

(define jazz.kernel-optimize?
  #t)

(define jazz.kernel-include-source?
  #f)

(define jazz.kernel-interpret?
  #f)

(define jazz.kernel-destination
  #f)


(load "kernel/syntax/macros")
(load "kernel/syntax/expansion")
(load "kernel/syntax/features")
(load "kernel/syntax/declares")
(load "kernel/syntax/primitives")
(load "kernel/runtime/build")


;;;
;;;; Initialize
;;;


(jazz.setup-versions)
(jazz.load-configurations)
(jazz.build-system-boot)
