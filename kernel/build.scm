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
  (define (validate-gambit-version)
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
  
  (set! jazz.source-versions-file "kernel/versions")
  (validate-gambit-version))


;;;
;;;; Configuration
;;;


(define (jazz.make-configuration name system platform windowing safety optimize? debug-environments? debug-location? debug-source? interpret-kernel? source-access? destination)
  (vector 'configuration name system platform windowing safety optimize? debug-environments? debug-location? debug-source? interpret-kernel? source-access? destination))

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

(define (jazz.configuration-debug-environments? configuration)
  (vector-ref configuration 7))

(define (jazz.configuration-debug-location? configuration)
  (vector-ref configuration 8))

(define (jazz.configuration-debug-source? configuration)
  (vector-ref configuration 9))

(define (jazz.configuration-interpret-kernel? configuration)
  (vector-ref configuration 10))

(define (jazz.configuration-source-access? configuration)
  (vector-ref configuration 11))

(define (jazz.configuration-destination configuration)
  (vector-ref configuration 12))


(define (jazz.new-configuration
          #!key
          (name #f)
          (system #f)
          (platform #f)
          (windowing #f)
          (safety #f)
          (optimize? #t)
          (debug-environments? #t)
          (debug-location? #t)
          (debug-source? #f)
          (interpret-kernel? #f)
          (source-access? #t)
          (destination #f))
  (jazz.make-configuration
    (jazz.validate-name name)
    (jazz.validate-system system)
    (jazz.validate-platform platform)
    (jazz.validate-windowing windowing)
    (jazz.validate-safety safety)
    (jazz.validate-optimize? optimize?)
    (jazz.validate-debug-environments? debug-environments?)
    (jazz.validate-debug-location? debug-location?)
    (jazz.validate-debug-source? debug-source?)
    (jazz.validate-interpret-kernel? interpret-kernel?)
    (jazz.validate-source-access? source-access?)
    (jazz.validate-destination destination)))


;;;
;;;; Configurations
;;;


(define jazz.anonymous-configuration-file
  "./.configuration")

(define jazz.named-configurations-file
  "~/.jazz/.configurations")

(define jazz.configurations
  '())


(define (jazz.list-configurations)
  (for-each jazz.describe-configuration (jazz.sort-configurations jazz.configurations)))


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


(define (jazz.sort-configurations configurations)
  (jazz.sort configurations
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
  (let ((name (jazz.configuration-name configuration)))
    (let ((pair (jazz.find-configuration-pair name)))
      (if pair
          (set-car! pair configuration)
        (set! jazz.configurations (append jazz.configurations (list configuration))))))
  (jazz.save-configurations))


(define (jazz.delete-configuration name)
  (set! jazz.configurations
        (jazz.delete name jazz.configurations
          (lambda (c1 c2)
            (eq? (jazz.configuration-name c1)
                 (jazz.configuration-name c2)))))
  (jazz.save-configurations))


(define (jazz.load-configurations)
  (if (file-exists? jazz.named-configurations-file)
      (call-with-input-file (list path: jazz.named-configurations-file eol-encoding: 'cr-lf)
        (lambda (input)
          (define (read-configuration input)
            (let ((list (read input)))
              (if (eof-object? list)
                  list
                (apply jazz.new-configuration list))))
          
          (set! jazz.configurations (read-all input read-configuration)))))
  (if (file-exists? jazz.anonymous-configuration-file)
      (jazz.register-configuration (jazz.load-configuration-file jazz.anonymous-configuration-file))))


(define (jazz.load-configuration-file file)
  (call-with-input-file (list path: file eol-encoding: 'cr-lf)
    (lambda (input)
      (apply jazz.new-configuration (read input)))))


(define (jazz.save-configurations)
  (define (split-configurations configurations)
    (let split ((configurations configurations) (anonymous #f) (named '()))
         (if (null? configurations)
             (values anonymous named)
           (let ((configuration (car configurations)))
             (if (not (jazz.configuration-name configuration))
                 (split (cdr configurations) configuration named)
               (split (cdr configurations) anonymous (cons configuration named)))))))
  
  (define (print-configuration configuration output)
    (jazz.print-configuration
      (jazz.configuration-name configuration)
      (jazz.configuration-system configuration)
      (jazz.configuration-platform configuration)
      (jazz.configuration-windowing configuration)
      (jazz.configuration-safety configuration)
      (jazz.configuration-optimize? configuration)
      (jazz.configuration-debug-environments? configuration)
      (jazz.configuration-debug-location? configuration)
      (jazz.configuration-debug-source? configuration)
      (jazz.configuration-interpret-kernel? configuration)
      (jazz.configuration-source-access? configuration)
      (jazz.configuration-destination configuration)
      output))
  
  (receive (anonymous named) (split-configurations jazz.configurations)
    (if anonymous
        (call-with-output-file (list path: jazz.anonymous-configuration-file eol-encoding: (jazz.platform-eol-encoding (jazz.guess-platform)))
          (lambda (output)
            (print-configuration anonymous output)))
      (if (file-exists? jazz.anonymous-configuration-file)
          (delete-file jazz.anonymous-configuration-file)))
    (let ((configurations (jazz.sort-configurations named)))
      (if (not (null? configurations))
          (begin
            (jazz.create-directories "~/.jazz" feedback: jazz.feedback)
            (call-with-output-file (list path: jazz.named-configurations-file eol-encoding: (jazz.platform-eol-encoding (jazz.guess-platform)))
              (lambda (output)
                (for-each (lambda (configuration)
                            (print-configuration configuration output))
                          configurations))))))))


(define (jazz.describe-configuration configuration)
  (let ((name (jazz.configuration-name configuration))
        (system (jazz.configuration-system configuration))
        (platform (jazz.configuration-platform configuration))
        (windowing (jazz.configuration-windowing configuration))
        (safety (jazz.configuration-safety configuration))
        (optimize? (jazz.configuration-optimize? configuration))
        (debug-environments? (jazz.configuration-debug-environments? configuration))
        (debug-location? (jazz.configuration-debug-location? configuration))
        (debug-source? (jazz.configuration-debug-source? configuration))
        (interpret-kernel? (jazz.configuration-interpret-kernel? configuration))
        (source-access? (jazz.configuration-source-access? configuration))
        (destination (jazz.configuration-destination configuration)))
    (jazz.feedback "{a}" (or name "<default>"))
    (jazz.feedback "  system: {s}" system)
    (jazz.feedback "  platform: {s}" platform)
    (if windowing
        (jazz.feedback "  windowing: {s}" windowing))
    (jazz.feedback "  safety: {s}" safety)
    (if (not optimize?)
        (jazz.feedback "  optimize?: {s}" optimize?))
    (if (not debug-environments?)
        (jazz.feedback "  debug-environments?: {s}" debug-environments?))
    (if (not debug-location?)
        (jazz.feedback "  debug-location?: {s}" debug-location?))
    (if debug-source?
        (jazz.feedback "  debug-source?: {s}" debug-source?))
    (if interpret-kernel?
        (jazz.feedback "  interpret-kernel?: {s}" interpret-kernel?))
    (if (not (eqv? source-access? #t))
        (jazz.feedback "  source-access?: {s}" source-access?))
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
          (debug-environments? #t)
          (debug-location? #t)
          (debug-source? #f)
          (interpret-kernel? #f)
          (source-access? #t)
          (destination #f))
  (let* ((name (jazz.require-name name))
         (system (jazz.require-system system))
         (platform (jazz.require-platform platform))
         (windowing (jazz.require-windowing platform windowing))
         (safety (jazz.require-safety safety))
         (optimize? (jazz.require-optimize? optimize?))
         (debug-environments? (jazz.require-debug-environments? debug-environments?))
         (debug-location? (jazz.require-debug-location? debug-location?))
         (debug-source? (jazz.require-debug-source? debug-source?))
         (interpret-kernel? (jazz.require-interpret-kernel? interpret-kernel?))
         (source-access? (jazz.require-source-access? source-access?))
         (destination (jazz.require-destination destination)))
    (let ((configuration
            (jazz.new-configuration
              name: name
              system: system
              platform: platform
              windowing: windowing
              safety: safety
              optimize?: optimize?
              debug-environments?: debug-environments?
              debug-location?: debug-location?
              debug-source?: debug-source?
              interpret-kernel?: interpret-kernel?
              source-access?: source-access?
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


(define (jazz.guess-platform)
  (let ((system (cadr (system-type)))
        (os (caddr (system-type))))
    (cond ((eq? system 'apple) 'mac)
          ((eq? os 'linux-gnu) 'unix)
          (else 'windows))))


(define (jazz.require-platform platform)
  (or platform (jazz.guess-platform)))


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
  (define (guess-windowing platform)
    (case platform
      ((mac) 'x11) ;; until carbon is ready
      ((windows) #f)
      ((unix) 'x11)))
  
  (or windowing (guess-windowing platform)))


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
;;;; Debug-Environments
;;;


(define jazz.valid-debug-environments
  '(#f
    #t))


(define (jazz.require-debug-environments? debug-environments)
  debug-environments)


(define (jazz.validate-debug-environments? debug-environments)
  (if (memq debug-environments jazz.valid-debug-environments)
      debug-environments
    (jazz.error "Invalid debug-environments?: {s}" debug-environments)))


;;;
;;;; Debug-Location
;;;


(define jazz.valid-debug-location
  '(#f
    #t))


(define (jazz.require-debug-location? debug-location)
  debug-location)


(define (jazz.validate-debug-location? debug-location)
  (if (memq debug-location jazz.valid-debug-location)
      debug-location
    (jazz.error "Invalid debug-location?: {s}" debug-location)))


;;;
;;;; Debug-Source
;;;


(define jazz.valid-debug-source
  '(#f
    #t))


(define (jazz.require-debug-source? debug-source)
  debug-source)


(define (jazz.validate-debug-source? debug-source)
  (if (memq debug-source jazz.valid-debug-source)
      debug-source
    (jazz.error "Invalid debug-source?: {s}" debug-source)))


;;;
;;;; Interpret-Kernel
;;;


(define jazz.valid-interpret-kernel
  '(#f
    #t))


(define (jazz.require-interpret-kernel? interpret-kernel)
  interpret-kernel)


(define (jazz.validate-interpret-kernel? interpret-kernel)
  (if (memq interpret-kernel jazz.valid-interpret-kernel)
      interpret-kernel
    (jazz.error "Invalid interpret-kernel?: {s}" interpret-kernel)))


;;;
;;;; Source Access
;;;


(define (jazz.require-source-access? source-access)
  source-access)


(define (jazz.validate-source-access? source-access)
  (if (or (eqv? source-access #f)
          (eqv? source-access #t))
      source-access
    (jazz.error "Invalid source-access?: {s}" source-access)))


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
  'all)


(define (jazz.make-symbols symbols local?)
  (define (parse-target/configuration/image str proc)
    (let ((colon (jazz.string-find str #\:)))
      (if (not colon)
          (parse-target/configuration str
            (lambda (target configuration)
              (proc target configuration #f)))
        (let ((image
                (if (= (+ colon 1) (string-length str))
                    #f
                  (standardize-image (string->symbol (substring str (+ colon 1) (string-length str)))))))
          (parse-target/configuration (substring str 0 colon)
            (lambda (target configuration)
              (proc target configuration image)))))))
  
  (define (parse-target/configuration str proc)
    (let ((at (jazz.string-find str #\@)))
      (if (not at)
          (proc (if (string=? str "") jazz.default-target (string->symbol str)) (jazz.require-default-configuration))
        (let ((target
                (if (= at 0)
                    jazz.default-target
                  (string->symbol (substring str 0 at))))
              (configuration
                (if (= (+ at 1) (string-length str))
                    (jazz.require-default-configuration)
                  (jazz.require-configuration (string->symbol (substring str (+ at 1) (string-length str)))))))
          (proc target configuration)))))
  
  (define (standardize-image image)
    (cond ((memv image '(lib library)) 'library)
          ((memv image '(exe executable)) 'executable)
          (else (jazz.error "Unknown image type: {s}" image))))
  
  (define (parse-symbols proc)
    (let iter ((scan symbols)
               (syms '())
               (options '()))
         (if (null? scan)
             (proc (reverse syms) options)
           (let ((obj (car scan)))
             (cond ((not (or (symbol? obj) (keyword? obj)))
                    (jazz.error "Invalid make target: {s}" obj))
                   ((or (keyword? obj) (and (symbol? obj) (eqv? #\- (string-ref (symbol->string obj) 0))))
                    (if (not (pair? (cdr scan)))
                        (jazz.error "Missing value for make option: {s}" obj)
                      (iter (cddr scan) syms (cons (cons obj (cadr scan)) options))))
                   (else
                    (iter (cdr scan) (cons obj syms) options)))))))
  
  (define (make-symbol symbol link jobs)
    (let ((name (symbol->string symbol)))
      (parse-target/configuration/image name
        (lambda (target configuration image)
          (make-target target configuration image link jobs local?)))))
  
  (define (make-target target configuration image link jobs local?)
    (case target
      ((clean) (jazz.make-clean configuration))
      ((cleankernel) (jazz.make-cleankernel configuration))
      ((cleanobject) (jazz.make-cleanobject configuration))
      ((cleanlibrary) (jazz.make-cleanlibrary configuration))
      ((kernel) (jazz.make-kernel configuration image local?))
      ((install) (jazz.make-install configuration))
      (else (jazz.make-product target configuration link jobs))))
  
  (parse-symbols
    (lambda (symbols options)
      (let ((link #f)
            (jobs #f))
        (for-each (lambda (option)
                    (case (car option)
                      ((link: -link)
                       (let ((value (cdr option)))
                         (jazz.parse-link value) ;; for early validation
                         (set! link value)))
                      ((j: jobs: -j -jobs)
                       (let ((value (cdr option)))
                         (if (and (fixnum? value) (>= value 1))
                             (set! jobs value)
                           (jazz.error "Invalid jobs option: {s}" value))))
                      (else
                       (jazz.error "Invalid make option: {s}" (car option)))))
                  options)
        (let iter ((scan (if (null? symbols)
                             (list jazz.default-target)
                           symbols)))
             (if (not (null? scan))
                 (let ((symbol (car scan)))
                   (make-symbol symbol link jobs)
                   (let ((tail (cdr scan)))
                     (if (not (null? tail))
                         (newline (console-port)))
                     (iter tail)))))))))


(define (jazz.make symbol)
  (jazz.make-symbols (list symbol) #t))


;;;
;;;; Clean
;;;


(define (jazz.make-clean configuration)
  (define delete-feedback
    (jazz.delete-feedback 1))
  
  (jazz.feedback "make clean")
  (let ((dir (jazz.configuration-directory configuration)))
    (if (file-exists? dir)
        (jazz.delete-directory dir
                               0
                               #f
                               #f
                               delete-feedback))))


(define (jazz.make-cleankernel configuration)
  (define delete-feedback
    (jazz.delete-feedback 1))
  
  (jazz.feedback "make cleankernel")
  (let ((dir (jazz.configuration-directory configuration)))
    (if (file-exists? dir)
        (jazz.delete-directory dir
                               0
                               #f
                               (lambda (dir level)
                                 (if (string=? (jazz.pathname-name dir) "lib")
                                     #f
                                   (jazz.empty-directory dir
                                                         level
                                                         #f
                                                         #f
                                                         delete-feedback)))
                               delete-feedback))))


(define (jazz.make-cleanobject configuration)
  (define delete-feedback
    (jazz.delete-feedback 2))
  
  (define (object-file? file level)
    (let ((ext (jazz.pathname-extension file)))
      (or (jazz.extension? ext "c")
          (jazz.extension? ext "mnf")
          (jazz.extension? ext "o")
          (jazz.numeric-extension? ext "o"))))
  
  (define (empty-objects dir level)
    (jazz.empty-directory dir
                          level
                          object-file?
                          empty-objects
                          delete-feedback)
    (jazz.cleanup-package dir level delete-feedback))
  
  (jazz.feedback "make cleanobject")
  (let ((dir (jazz.configuration-directory configuration)))
    (if (file-exists? dir)
        (jazz.delete-directory dir
                               0
                               (lambda (file level)
                                 #f)
                               (lambda (dir level)
                                 (if (string=? (jazz.pathname-name dir) "lib")
                                     (jazz.empty-directory dir
                                                           level
                                                           (lambda (file level)
                                                             #f)
                                                           empty-objects
                                                           delete-feedback)
                                   #f))
                               delete-feedback))))


(define (jazz.make-cleanlibrary configuration)
  (define delete-feedback
    (jazz.delete-feedback 2))
  
  (define (library-file? file level)
    (let ((ext (jazz.pathname-extension file)))
      (or (jazz.extension? ext "lmf")
          (jazz.numeric-extension? ext "l"))))
  
  (define (empty-libraries dir level)
    (jazz.empty-directory dir
                          level
                          library-file?
                          empty-libraries
                          delete-feedback)
    (jazz.cleanup-package dir level delete-feedback))
  
  (jazz.feedback "make cleanlibrary")
  (let ((dir (jazz.configuration-directory configuration)))
    (if (file-exists? dir)
        (jazz.delete-directory dir
                               0
                               (lambda (file level)
                                 #f)
                               (lambda (dir level)
                                 (if (string=? (jazz.pathname-name dir) "lib")
                                     (jazz.empty-directory dir
                                                           level
                                                           (lambda (file level)
                                                             #f)
                                                           empty-libraries
                                                           delete-feedback)
                                   #f))
                               delete-feedback))))


;; doing this as an after scan of the content seems to break on Windows (of course!)
;; when an explorer is displaying folders that will be deleted
;; we could try to do it in one scan and also verify behavior on other platforms
(define (jazz.cleanup-package dir level feedback)
  (let ((content (jazz.directory-content dir)))
    (case (length content)
      ((0) #t)
      ((1) (let ((name (car content)))
             (if (string=? name ".package")
                 (let ((path (string-append dir name)))
                   (feedback path level)
                   (delete-file path)
                   #t)
               #f)))
      (else #f))))


(define (jazz.delete-feedback depth)
  (lambda (path level)
    (if (<= level depth)
        (jazz.feedback "; deleting {a}..." path))))


;;;
;;;; Install
;;;


(define (jazz.make-install configuration)
  (jazz.error "Make install is not supported. See INSTALL for details"))


;;;
;;;; Kernel
;;;


(define (jazz.make-kernel configuration image local?)
  (define (build-kernel configuration image)
    (define (build configuration)
      (let ((name (jazz.configuration-name configuration))
            (system (jazz.configuration-system configuration))
            (platform (jazz.configuration-platform configuration))
            (windowing (jazz.configuration-windowing configuration))
            (safety (jazz.configuration-safety configuration))
            (optimize? (jazz.configuration-optimize? configuration))
            (debug-environments? (jazz.configuration-debug-environments? configuration))
            (debug-location? (jazz.configuration-debug-location? configuration))
            (debug-source? (jazz.configuration-debug-source? configuration))
            (interpret-kernel? (jazz.configuration-interpret-kernel? configuration))
            (source "./")
            (source-access? (jazz.configuration-source-access? configuration))
            (destination (jazz.configuration-destination configuration))
            (destination-directory (jazz.configuration-directory configuration)))
        (jazz.build-image #f
                          system:                system
                          platform:              platform
                          windowing:             windowing
                          safety:                safety
                          optimize?:             optimize?
                          debug-environments?:   debug-environments?
                          debug-location?:       debug-location?
                          debug-source?:         debug-source?
                          include-compiler?:     #t
                          interpret-kernel?:     interpret-kernel?
                          source:                source
                          source-access?:        source-access?
                          destination:           destination
                          destination-directory: destination-directory
                          image:                 image
                          kernel?:               #t
                          console?:              #t)))
    
    (jazz.feedback "make kernel")
    (let ((configuration (or configuration (jazz.require-default-configuration))))
      (let ((configuration-file (jazz.configuration-file configuration)))
        (if (file-exists? configuration-file)
            (build (jazz.load-configuration-file configuration-file))
          (build configuration)))))
  
  (define (build-recursive target configuration image)
    (let ((configuration-name (jazz.configuration-name configuration)))
      (let ((argument (string-append (if configuration-name
                                         (jazz.format "{a}@{a}" target configuration-name)
                                       (symbol->string target))
                                     (if image
                                         (string-append ":" (symbol->string image))
                                       "")))
            (gsc-path (if (eq? (jazz.configuration-platform configuration) 'windows)
                          "gsc"
                        "gsc-script")))
        (jazz.call-process gsc-path `("-:dq-" "make" ,argument)))))
  
  (if local?
      (build-kernel configuration image)
    (build-recursive 'kernel configuration image)))


;;;
;;;; Product
;;;


(define (jazz.make-product product configuration link jobs)
  (jazz.make-kernel configuration #f #f)
  (jazz.call-process
     (string-append (jazz.configuration-directory configuration) "kernel")
     `("-:dq-" "-make"
       ,(symbol->string product)
       ,@(if link `("-link" ,(symbol->string link)) '())
       ,@(if jobs `("-jobs" ,(number->string jobs)) '()))))


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
  (define (format-to output fmt-string arguments)
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

  (let ((output (open-output-string)))
    (format-to output fmt-string arguments)
    (get-output-string output)))


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


(define (jazz.delete-directory directory #!optional (level 0) (delete-file? #f) (delete-directory? #f) (feedback #f))
  (if (jazz.empty-directory directory level delete-file? delete-directory? feedback)
      (begin
        (if feedback
            (feedback directory level))
        (delete-directory directory)
        #t)
    #f))


(define (jazz.empty-directory directory #!optional (level 0) (delete-file? #f) (delete-directory? #f) (feedback #f))
  (define (default-delete-file? file level)
    #t)
  
  (define (default-delete-directory? dir level)
    (jazz.empty-directory dir level delete-file? delete-directory? feedback))
  
  (let ((empty? #t))
    (for-each (lambda (name)
                (let ((file (string-append directory name)))
                  (if ((or delete-file? default-delete-file?) file level)
                      (begin
                        (if feedback
                            (feedback file level))
                        (delete-file file))
                    (set! empty? #f))))
              (jazz.directory-files directory))
    (for-each (lambda (name)
                (let ((dir (string-append directory name "/")))
                  (if ((or delete-directory? default-delete-directory?) dir (+ level 1))
                      (begin
                        (if feedback
                            (feedback dir level))
                        (delete-directory dir))
                    (set! empty? #f))))
              (jazz.directory-directories directory))
    empty?))


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
  (define (process-command command output)
    (if (eof-object? command)
        (quit-command '() output)
      (call-with-input-string command
        (lambda (input)
          (let ((command (read input)))
            (if (eof-object? command)
                #f
              (begin
                (let ((arguments (read-all input read)))
                  (case command
                    ((list) (list-command arguments output))
                    ((delete) (delete-command arguments output))
                    ((configure) (configure-command arguments output))
                    ((make) (make-command arguments output))
                    ((help ?) (help-command arguments output))
                    ((quit) (quit-command arguments output))
                    (else (jazz.error "Unknown command: {s}" command))))
                #t)))))))
  
  (define (list-command arguments output)
    (jazz.list-configurations))
  
  (define (delete-command arguments output)
    (let ((name (if (null? arguments) #f (car arguments))))
      (jazz.delete-configuration (jazz.require-configuration name))
      (jazz.list-configurations)))
  
  (define (configure-command arguments output)
    (apply jazz.configure arguments))
  
  (define (make-command arguments output)
    (jazz.make-symbols arguments #f))
  
  (define (help-command arguments output)
    (jazz.print "Commands:" output)
    (jazz.print "  configure [name:] [system:] [platform:] [windowing:] [safety:] [optimize?:] [debug-environments?:] [debug-location?:] [debug-source?:] [interpret-kernel?:] [destination:]" output)
    (jazz.print "  make [target | clean | cleankernel | cleanobject | cleanlibrary]@[configuration]:[image]" output)
    (jazz.print "  list" output)
    (jazz.print "  delete [configuration]" output)
    (jazz.print "  help or ?" output)
    (jazz.print "  quit" output))
  
  (define (quit-command arguments output)
    (exit))
  
  (define (debug-exception exc console)
    (if jazz.display-exception?
        (display-exception exc console))
    (if jazz.display-backtrace?
        (continuation-capture
          (lambda (cont)
            (display-continuation-backtrace cont console #t #t 0 1000)))))
  
  (let ((console (console-port)))
    (jazz.print (jazz.format "JazzScheme Build System v{a}" (jazz.present-version (jazz.get-source-version-number))) console)
    (force-output console)
    (let loop ((newline? #t))
      (if newline?
          (newline console))
      (display jazz.prompt console)
      (force-output console)
      (let ((command (read-line console))
            (processed? #f))
        (continuation-capture
          (lambda (stop)
            (with-exception-handler
              (lambda (exc)
                (debug-exception exc console)
                (continuation-return stop #f))
              (lambda ()
                (set! processed? (process-command command console))))))
        (loop processed?)))))


;;;
;;;; Boot
;;;


(define (jazz.build-system-boot)
  (define (fatal message)
    (display message)
    (newline)
    (force-output)
    (exit 1))
  
  (define (missing-argument-for-option opt)
    (fatal (jazz.format "Missing argument for option: {a}" opt)))
  
  (define (read-argument arg)
    (call-with-input-string (list init: arg)
      read))
  
  (define (string-option name options)
    (let ((opt (jazz.get-option name options)))
      (if (not opt)
          #f
        opt)))
  
  (define (symbol-option name options)
    (let ((opt (jazz.get-option name options)))
      (if (not opt)
          #f
        (string->symbol opt))))
  
  (define (boolean-option name options default)
    (let ((opt (jazz.get-option name options)))
      (cond ((not opt)
             default)
            ((string-ci=? opt "false")
             #f)
            ((string-ci=? opt "true")
             #t)
            (else
             (fatal (jazz.format "Invalid boolean argument for option: {a}" name))))))
  
  (define (unknown-option opt)
    (fatal (jazz.format "Unknown option: {a}" opt)))
  
  (let ((command-arguments (cdr (command-line))))
    (if (null? command-arguments)
        (jazz.build-system-repl)
      (let ((action (car command-arguments))
            (arguments (cdr command-arguments)))
        (cond ((equal? action "list")
               (jazz.list-configurations)
               (exit))
              ((equal? action "delete")
               (let ((name (if (null? arguments) #f (read-argument (car arguments)))))
                 (jazz.delete-configuration (jazz.require-configuration name))
                 (jazz.list-configurations))
               (exit))
              ((equal? action "configure")
               (jazz.split-command-line arguments '() '("name" "system" "platform" "windowing" "safety" "optimize" "debug-environments" "debug-location" "debug-source" "interpret-kernel" "destination") missing-argument-for-option
                 (lambda (options remaining)
                   (if (null? remaining)
                       (let ((name (symbol-option "name" options))
                             (system (symbol-option "system" options))
                             (platform (symbol-option "platform" options))
                             (windowing (symbol-option "windowing" options))
                             (safety (symbol-option "safety" options))
                             (optimize (boolean-option "optimize" options #t))
                             (debug-environments (boolean-option "debug-environments" options #t))
                             (debug-location (boolean-option "debug-location" options #t))
                             (debug-source (boolean-option "debug-source" options #f))
                             (interpret-kernel (boolean-option "interpret-kernel" options #f))
                             (destination (string-option "destination" options)))
                         (jazz.configure name: name system: system platform: platform windowing: windowing safety: safety optimize?: optimize debug-environments?: debug-environments debug-location?: debug-location debug-source?: debug-source interpret-kernel?: interpret-kernel destination: destination)
                         (exit))
                     (unknown-option (car remaining))))))
              ((equal? action "make")
               (jazz.load-kernel-build)
               (jazz.make-symbols (map read-argument arguments) #t)
               (exit))
              ((or (equal? action "help") (equal? action "?"))
               (let ((console (console-port)))
                 (jazz.print "Usage:" console)
                 (jazz.print "  gsc configure [-name] [-system] [-platform] [-windowing] [-safety] [-optimize] [-debug-environments] [-debug-location] [-debug-source] [-interpret-kernel] [-destination]" console)
                 (jazz.print "  gsc make [target | clean | cleankernel | cleanobject | cleanlibrary]@[configuration]:[image]" console)
                 (jazz.print "  gsc list" console)
                 (jazz.print "  gsc delete [configuration]" console)
                 (jazz.print "  gsc help or ?" console)
                 (jazz.print "  gsc debug" console))
               (exit))
              ((equal? action "debug")
               (jazz.load-kernel-build)
               (##repl-debug-main))
              (else
               (fatal (jazz.format "Unknown build system action: {a}" action))))))))


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

(define jazz.kernel-debug-environments?
  #f)

(define jazz.kernel-debug-location?
  #f)

(define jazz.kernel-debug-source?
  #f)

(define jazz.kernel-destination
  #f)


(define (jazz.load-kernel-base)
  (load "kernel/runtime/base"))


(define (jazz.load-kernel-build)
  (load "kernel/syntax/header")
  (load "kernel/syntax/macro")
  (load "kernel/syntax/block")
  (load "kernel/syntax/expansion")
  (load "kernel/syntax/features")
  (load "kernel/syntax/declares")
  (load "kernel/syntax/primitives")
  (load "kernel/syntax/syntax")
  (load "kernel/runtime/common")
  (load "kernel/runtime/digest")
  (load "kernel/runtime/build"))


;;;
;;;; Initialize
;;;


(jazz.load-kernel-base)
(jazz.setup-versions)
(jazz.load-configurations)
(jazz.build-system-boot)
