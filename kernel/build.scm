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
                                          (if (eq? alias 'bin)
                                              (not title)
                                            (and title (jazz.valid-filename? title))))))))
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


(define (jazz.parse-target/configuration str)
  (let ((pos (jazz.string-find str #\@)))
    (if (not pos)
        (values (string->symbol str) (jazz.require-default-configuration))
      (let ((target
              (if (= pos 0)
                  jazz.default-target
                (string->symbol (substring str 0 pos))))
            (configuration
              (if (= (+ pos 1) (string-length str))
                  (jazz.require-default-configuration)
                (jazz.require-configuration (string->symbol (substring str (+ pos 1) (string-length str)))))))
        (values target configuration)))))


(define (jazz.make symbols)
  (define (make-symbol symbol)
    (let ((name (symbol->string symbol)))
      (receive (target configuration) (jazz.parse-target/configuration name)
        (jazz.make-target target configuration))))
  
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
    ((cleankernel) (jazz.make-cleankernel configuration))
    ((kernel) (jazz.make-kernel configuration))
    ((install) (jazz.make-install configuration))
    (else (jazz.make-product target configuration))))


(define (jazz.make-local target configuration)
  (case target
    ((clean) (jazz.make-clean configuration))
    ((cleankernel) (jazz.make-cleankernel configuration))
    ((kernel) (jazz.build-kernel configuration))
    ((install) (jazz.make-install configuration))
    (else (jazz.make-product target configuration))))


;;;
;;;; Build
;;;


(define (jazz.build-recursive target configuration)
  (let ((configuration-name (jazz.configuration-name configuration)))
    (let ((argument (if configuration-name
                        (jazz.format "{a}@{a}" target configuration-name)
                      (symbol->string target)))
          (gsc-path (if (eq? (jazz.configuration-platform configuration) 'windows)
                        "gsc"
                      "gsc-script")))
      (jazz.call-process gsc-path `("-:dq-" "-make" ,argument)))))


;;;
;;;; Clean
;;;


(define (jazz.make-clean configuration)
  (jazz.feedback "make clean")
  (jazz.delete-directory (jazz.configuration-directory configuration)))


(define (jazz.make-cleankernel configuration)
  (jazz.feedback "make cleankernel")
  (let ((dir (jazz.configuration-directory configuration)))
    (if (file-exists? dir)
        (jazz.empty-directory dir '("lib")))))


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


(define (jazz.build-kernel #!optional (configuration #f))
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
  
    (let ((configuration (or configuration (jazz.require-default-configuration))))
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


(define (jazz.collect-if predicate lst)
  (let iter ((scan lst))
    (if (not (null? scan))
        (let ((value (car scan)))
          (if (predicate value)
              (cons value (iter (cdr scan)))
            (iter (cdr scan))))
      '())))


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


(define (jazz.string-replace str old new)
  (let ((cpy (string-copy str)))
    (let iter ((n (- (string-length cpy) 1)))
      (if (>= n 0)
          (begin
            (if (eqv? (string-ref cpy n) old)
                (string-set! cpy n new))
            (iter (- n 1)))))
    cpy))


(define (jazz.string-ends-with? str target)
  (let ((sl (string-length str))
        (tl (string-length target)))
    (and (>= sl tl)
         (string=? (substring str (- sl tl) sl) target))))


(define (jazz.split-string str separator)
  (let ((lst '())
        (end (string-length str)))
    (let iter ((pos (- end 1)))
      (if (> pos 0)
          (begin
            (if (eqv? (string-ref str pos) separator)
                (begin
                  (set! lst (cons (substring str (+ pos 1) end) lst))
                  (set! end pos)))
            (iter (- pos 1))))
        (cons (substring str 0 end) lst))))


(define (jazz.join-strings strings separator)
  (let ((output (open-output-string)))
    (display (car strings) output)
    (for-each (lambda (string)
                (display separator output)
                (display string output))
              (cdr strings))
    (get-output-string output)))


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


(define (jazz.delete-directory dir #!optional (level 0))
  (if (file-exists? dir)
      (begin
        (jazz.empty-directory dir #f level)
        (delete-directory dir))))


(define (jazz.empty-directory dir #!optional (ignored #f) (level 0))
  (for-each (lambda (name)
              (if (or (not ignored) (not (member name ignored)))
                  (let ((path (string-append dir name)))
                    (if (< level 2)
                        (jazz.feedback "; deleting {a}..." path))
                    (delete-file path))))
            (jazz.directory-files dir))
  (for-each (lambda (name)
              (if (or (not ignored) (not (member name ignored)))
                  (let ((path (string-append dir name "/")))
                    (if (< level 2)
                        (jazz.feedback "; deleting {a}..." path))
                    (jazz.delete-directory path (+ level 1)))))
            (jazz.directory-directories dir)))


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

(define jazz.display-exception?
  #t)

(define jazz.display-backtrace?
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
                (jazz.debug-exception exc console jazz.display-exception? jazz.display-backtrace?)
                (continuation-return stop #f))
              (lambda ()
                (jazz.process-command command console)))))
        (loop)))))


(define (jazz.process-command command output)
  (if (eof-object? command)
      (jazz.quit-command '() output)
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
            (else (jazz.error "Unknown command: {s}" command))))))))


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
               (jazz.load-kernel-build)
               (##repl-debug-main))
              ((and (option? arg)
                    (equal? (convert-option arg) "make"))
               (let ((arguments (cdr command-arguments)))
                 (case (length arguments)
                   ((1)
                    (let ((argument (car arguments)))
                      (receive (target configuration) (jazz.parse-target/configuration argument)
                        (jazz.load-kernel-build)
                        (jazz.make-local target configuration)
                        (exit))))
                   (else
                    (fatal (jazz.format "Ill-formed make command: {s}" command-arguments))))))
              ((and (option? arg)
                    (equal? (convert-option arg) "help"))
               (let ((console (console-port)))
                 (jazz.print (jazz.format "JazzScheme Build System v{a}" (jazz.present-version (jazz.get-source-version-number))) console)
                 (jazz.print "" console)
                 (jazz.print "Usage: gsc [options]" console)
                 (jazz.print "" console)
                 (jazz.print "Options: " console)
                 (jazz.print "  -make <target@configuration> Make <target> for <configuration> where both parts are optional" console)
                 (jazz.print "  -help                        Display help information" console))
               (exit))
              (else
               (fatal (jazz.format "Unknown build system command: {s}" arg))))))))


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


(define (jazz.load-kernel-base)
  (load "kernel/runtime/base"))


(define (jazz.load-kernel-build)
  (load "kernel/syntax/header")
  (load "kernel/syntax/macros")
  (load "kernel/syntax/expansion")
  (load "kernel/syntax/features")
  (load "kernel/syntax/declares")
  (load "kernel/syntax/primitives")
  (load "kernel/runtime/build"))


;;;
;;;; Initialize
;;;


(jazz.load-kernel-base)
(jazz.setup-versions)
(jazz.load-configurations)
(jazz.build-system-boot)
