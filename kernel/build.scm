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
;;;  Portions created by the Initial Developer are Copyright (C) 1996-2018
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
;;;; Dependencies
;;;


(define jazz:all-dependencies
  #f)


(define (jazz:get-dependencies)
  (or jazz:all-dependencies
      (let ((dependencies (let ((file ".dependencies"))
                            (if (file-exists? file)
                                (call-with-input-file file (lambda (port) (read-all port read)))
                              '()))))
        (set! jazz:all-dependencies dependencies)
        dependencies)))


(define (jazz:find-dependency name)
  (jazz:find-if (lambda (dep)
                  (eq? (jazz:dependency-name dep) name))
                (jazz:get-dependencies)))


(define (jazz:require-dependency name)
  (or (jazz:find-dependency name)
      (error "Unknown dependency" name)))


(define (jazz:dependency-name dep)
  (car dep))

(define (jazz:dependency-directory dep)
  (cadr dep))

(define (jazz:dependency-url dep)
  (caddr dep))


(define (jazz:clone-dependencies names)
  (define (clone-dependency name)
    (let ((dep (jazz:require-dependency name)))
      (jazz:invoke-process
         (list
           path: "git"
           arguments: `("clone" ,(jazz:dependency-url dep) ,(jazz:dependency-directory dep))))))
  
  (for-each clone-dependency names))


;;;
;;;; Versions
;;;


(define (jazz:setup-versions)
  (set! jazz:jazz-versions-file (string-append jazz:source "kernel/versions")))


;;;
;;;; Option
;;;


(define jazz:unspecified-option-value
  (list 'unspecified-option))


(define (jazz:unspecified-option)
  jazz:unspecified-option-value)


(define (jazz:specified-option? option)
  (not (eq? option jazz:unspecified-option-value)))


(define (jazz:or-option . rest)
  (let iter ((scan rest))
    (if (null? scan)
        default
      (let ((option (car scan)))
        (if (jazz:specified-option? option)
            option
          (iter (cdr scan)))))))


;;;
;;;; Options
;;;


(define-macro (jazz:define-option name default)
  (let ((global (gensym name)))
    `(begin
       (define ,global ,default)
       (define (,name . rest)
         (if (null? rest)
             ,global
           (set! ,global (car rest)))))))


(jazz:define-option jazz:default-name
  #f)

(jazz:define-option jazz:default-system
  'gambit)

(jazz:define-option jazz:default-platform
  (jazz:unspecified-option))

(jazz:define-option jazz:default-compiler
  'c)

(jazz:define-option jazz:default-processor
  (jazz:unspecified-option))

(jazz:define-option jazz:default-windowing
  (jazz:unspecified-option))

(jazz:define-option jazz:default-safety
  'release)

(jazz:define-option jazz:default-optimize?
  #t)

(jazz:define-option jazz:default-debug-environments?
  #t)

(jazz:define-option jazz:default-debug-location?
  #t)

(jazz:define-option jazz:default-debug-source?
  #f)

(jazz:define-option jazz:default-debug-foreign?
  #f)

(jazz:define-option jazz:default-track-memory?
  #f)

(jazz:define-option jazz:default-mutable-bindings?
  #f)

(jazz:define-option jazz:default-kernel-interpret?
  #f)

(jazz:define-option jazz:default-destination
  #f)

(jazz:define-option jazz:default-features
  '())

(jazz:define-option jazz:default-properties
  '())


(jazz:define-option jazz:default-target
  'all)


;;;
;;;; Configuration
;;;


(define (jazz:new-configuration
          #!key
          (name (jazz:unspecified-option))
          (system (jazz:unspecified-option))
          (platform (jazz:unspecified-option))
          (compiler (jazz:unspecified-option))
          (processor (jazz:unspecified-option))
          (windowing (jazz:unspecified-option))
          (safety (jazz:unspecified-option))
          (optimize? (jazz:unspecified-option))
          (debug-environments? (jazz:unspecified-option))
          (debug-location? (jazz:unspecified-option))
          (debug-source? (jazz:unspecified-option))
          (debug-foreign? (jazz:unspecified-option))
          (track-memory? (jazz:unspecified-option))
          (mutable-bindings? (jazz:unspecified-option))
          (kernel-interpret? (jazz:unspecified-option))
          (destination (jazz:unspecified-option))
          (features (jazz:unspecified-option))
          (properties (jazz:unspecified-option))
          (local? #f))
  (jazz:make-configuration
    name
    system
    platform
    compiler
    processor
    windowing
    safety
    optimize?
    debug-environments?
    debug-location?
    debug-source?
    debug-foreign?
    track-memory?
    mutable-bindings?
    kernel-interpret?
    destination
    features
    properties
    local?))


(define (jazz:validate-configuration
          template
          #!key
          (name (jazz:unspecified-option))
          (system (jazz:unspecified-option))
          (platform (jazz:unspecified-option))
          (compiler (jazz:unspecified-option))
          (processor (jazz:unspecified-option))
          (windowing (jazz:unspecified-option))
          (safety (jazz:unspecified-option))
          (optimize? (jazz:unspecified-option))
          (debug-environments? (jazz:unspecified-option))
          (debug-location? (jazz:unspecified-option))
          (debug-source? (jazz:unspecified-option))
          (debug-foreign? (jazz:unspecified-option))
          (track-memory? (jazz:unspecified-option))
          (mutable-bindings? (jazz:unspecified-option))
          (kernel-interpret? (jazz:unspecified-option))
          (destination (jazz:unspecified-option))
          (features (jazz:unspecified-option))
          (properties (jazz:unspecified-option))
          (local? #f))
  (let* ((name (jazz:validate-name (jazz:require-name name template)))
         (system (jazz:validate-system (jazz:require-system system template)))
         (platform (jazz:validate-platform (jazz:require-platform platform template)))
         (compiler (jazz:validate-compiler (jazz:require-compiler compiler template)))
         (processor (jazz:validate-processor (jazz:require-processor processor template)))
         (windowing (jazz:validate-windowing (jazz:require-windowing platform windowing template)))
         (safety (jazz:validate-safety (jazz:require-safety safety template)))
         (optimize? (jazz:validate-optimize? (jazz:require-optimize? optimize? template)))
         (debug-environments? (jazz:validate-debug-environments? (jazz:require-debug-environments? debug-environments? template)))
         (debug-location? (jazz:validate-debug-location? (jazz:require-debug-location? debug-location? template)))
         (debug-source? (jazz:validate-debug-source? (jazz:require-debug-source? debug-source? template)))
         (debug-foreign? (jazz:validate-debug-foreign? (jazz:require-debug-foreign? debug-foreign? template)))
         (track-memory? (jazz:validate-track-memory? (jazz:require-track-memory? track-memory? template)))
         (mutable-bindings? (jazz:validate-mutable-bindings? (jazz:require-mutable-bindings? mutable-bindings? template)))
         (kernel-interpret? (jazz:validate-kernel-interpret? (jazz:require-kernel-interpret? kernel-interpret? template)))
         (destination (jazz:validate-destination (jazz:require-destination destination template)))
         (features (jazz:validate-features (jazz:require-features features template)))
         (properties (jazz:validate-properties (jazz:require-properties properties template))))
    (jazz:make-configuration
      name
      system
      platform
      compiler
      processor
      windowing
      safety
      optimize?
      debug-environments?
      debug-location?
      debug-source?
      debug-foreign?
      track-memory?
      mutable-bindings?
      kernel-interpret?
      destination
      features
      properties
      local?)))


;;;
;;;; Configurations
;;;


(define jazz:anonymous-configuration-file
  "./.configuration")

(define jazz:configurations
  '())


(define (jazz:list-configurations)
  (for-each jazz:describe-configuration (jazz:sort-configurations jazz:configurations)))


(define (jazz:require-configuration name)
  (or (jazz:find-configuration name)
      (if (not name)
          (jazz:error "Unable to find default configuration")
        (jazz:error "Unable to find configuration: {s}" name))))

(define (jazz:require-default-configuration)
  (or (let ((jazconf (getenv "JAZCONF" #f)))
        (if jazconf
            (jazz:find-configuration (string->symbol jazconf))
          (jazz:find-configuration #f)))
      (begin
        (jazz:feedback "configure")
        (jazz:configure jazz:unspecified-configuration)
        (jazz:require-configuration #f))))


(define (jazz:find-configuration name)
  (let ((configuration
          (let ((pair (jazz:find-configuration-pair name)))
            (if (not pair)
                #f
              (car pair)))))
    ;; special case to support make of binaries
    (if (and (not name) (not configuration))
        (let ((configuration-dir (jazz:destination-directory #f "bin:" "./")))
          (let ((configuration-file (string-append configuration-dir ".configuration")))
            (if (file-exists? configuration-file)
                (jazz:load-configuration configuration-file)
              #f)))
      configuration)))

(define (jazz:find-configuration-pair name)
  (let iter ((configurations jazz:configurations))
    (if (null? configurations)
        #f
      (let ((configuration (car configurations)))
        (if (eq? (jazz:get-configuration-name configuration) name)
            configurations
          (iter (cdr configurations)))))))


(define (jazz:sort-configurations configurations)
  (jazz:sort-list (lambda (c1 c2)
                    (let ((n1 (jazz:get-configuration-name c1))
                          (n2 (jazz:get-configuration-name c2)))
                      (cond ((not n1)
                             #t)
                            ((not n2)
                             #f)
                            (else
                             (string-ci<? (symbol->string n1) (symbol->string n2))))))
                  configurations))


(define (jazz:register-configuration configuration)
  (let ((name (jazz:get-configuration-name configuration)))
    (let ((pair (jazz:find-configuration-pair name)))
      (if pair
          (set-car! pair configuration)
        (set! jazz:configurations (append jazz:configurations (list configuration))))))
  (jazz:save-configurations))


(define (jazz:delete-configuration name)
  (set! jazz:configurations
        (jazz:delete name jazz:configurations
          (lambda (c1 c2)
            (eq? (jazz:get-configuration-name c1)
                 (jazz:get-configuration-name c2)))))
  (jazz:save-configurations))


(define (jazz:load-configurations)
  (if (file-exists? jazz:named-configurations-file)
      (call-with-input-file (list path: jazz:named-configurations-file eol-encoding: 'cr-lf)
        (lambda (input)
          (define (read-configuration input)
            (let ((list (read input)))
              (if (eof-object? list)
                  list
                (apply jazz:validate-configuration jazz:unspecified-configuration list))))
          
          (set! jazz:configurations (read-all input read-configuration)))))
  (if (file-exists? jazz:local-configurations-file)
      (call-with-input-file (list path: jazz:local-configurations-file eol-encoding: 'cr-lf)
        (lambda (input)
          (define (read-configuration input)
            (let ((list (read input)))
              (if (eof-object? list)
                  list
                (apply jazz:validate-configuration jazz:unspecified-configuration local?: #t list))))
          
          (for-each jazz:register-configuration (read-all input read-configuration)))))
  (if (file-exists? jazz:anonymous-configuration-file)
      (jazz:register-configuration (jazz:load-configuration jazz:anonymous-configuration-file))))


(define (jazz:load-configuration file)
  (define (parse-properties data)
    (if (and (pair? data)
             (eq? (car data) 'configuration))
        (caddr data)
      data))
  
  (call-with-input-file (list path: file eol-encoding: 'cr-lf)
    (lambda (input)
      (let ((data (read input)))
        ;; quicky until a full-fledged jazz:versioned-file similar to jazz:versioned-directory
        ;; removing jazz:convert-configuration-205001 buggy for absolute path ("c:/" -> "c//")
        (apply jazz:validate-configuration jazz:unspecified-configuration (parse-properties data))))))


(define (jazz:save-configurations)
  (define (split-configurations configurations)
    (let split ((configurations configurations) (anonymous #f) (local '()) (named '()))
         (if (null? configurations)
             (values anonymous local named)
           (let ((configuration (car configurations)))
             (cond ((not (jazz:get-configuration-name configuration))
                    (split (cdr configurations) configuration local named))
                   ((jazz:get-configuration-local? configuration)
                    (split (cdr configurations) anonymous (cons configuration local) named))
                   (else
                    (split (cdr configurations) anonymous local (cons configuration named))))))))
  
  (define (save-configuration configuration file system-platform)
    (jazz:save-configuration
      (jazz:get-configuration-name configuration)
      (jazz:get-configuration-system configuration)
      (jazz:get-configuration-platform configuration)
      (jazz:get-configuration-compiler configuration)
      (jazz:get-configuration-processor configuration)
      (jazz:get-configuration-windowing configuration)
      (jazz:get-configuration-safety configuration)
      (jazz:get-configuration-optimize? configuration)
      (jazz:get-configuration-debug-environments? configuration)
      (jazz:get-configuration-debug-location? configuration)
      (jazz:get-configuration-debug-source? configuration)
      (jazz:get-configuration-debug-foreign? configuration)
      (jazz:get-configuration-track-memory? configuration)
      (jazz:get-configuration-mutable-bindings? configuration)
      (jazz:get-configuration-kernel-interpret? configuration)
      (jazz:get-configuration-destination configuration)
      (jazz:get-configuration-features configuration)
      (jazz:get-configuration-properties configuration)
      file
      system-platform))
  
  (define (print-configuration configuration output)
    (jazz:print-configuration
      (jazz:get-configuration-name configuration)
      (jazz:get-configuration-system configuration)
      (jazz:get-configuration-platform configuration)
      (jazz:get-configuration-compiler configuration)
      (jazz:get-configuration-processor configuration)
      (jazz:get-configuration-windowing configuration)
      (jazz:get-configuration-safety configuration)
      (jazz:get-configuration-optimize? configuration)
      (jazz:get-configuration-debug-environments? configuration)
      (jazz:get-configuration-debug-location? configuration)
      (jazz:get-configuration-debug-source? configuration)
      (jazz:get-configuration-debug-foreign? configuration)
      (jazz:get-configuration-track-memory? configuration)
      (jazz:get-configuration-mutable-bindings? configuration)
      (jazz:get-configuration-kernel-interpret? configuration)
      (jazz:get-configuration-destination configuration)
      (jazz:get-configuration-features configuration)
      (jazz:get-configuration-properties configuration)
      output))
  
  (receive (anonymous local named) (split-configurations jazz:configurations)
    (if anonymous
        (save-configuration anonymous jazz:anonymous-configuration-file (jazz:guess-platform))
      (if (file-exists? jazz:anonymous-configuration-file)
          (delete-file jazz:anonymous-configuration-file)))
    (let ((configurations (jazz:sort-configurations named)))
      (if (not (null? configurations))
          (let ((path jazz:named-configurations-file))
            (jazz:create-directories (jazz:pathname-dir path) feedback: jazz:feedback)
            (call-with-output-file (list path: path eol-encoding: (jazz:platform-eol-encoding (jazz:guess-platform)))
              (lambda (output)
                (for-each (lambda (configuration)
                            (print-configuration configuration output)
                            (newline output))
                          configurations))))))))


(define (jazz:describe-configuration configuration)
  (let ((name (jazz:get-configuration-name configuration))
        (system (jazz:get-configuration-system configuration))
        (platform (jazz:get-configuration-platform configuration))
        (compiler (jazz:get-configuration-compiler configuration))
        (processor (jazz:get-configuration-processor configuration))
        (windowing (jazz:get-configuration-windowing configuration))
        (safety (jazz:get-configuration-safety configuration))
        (optimize? (jazz:get-configuration-optimize? configuration))
        (debug-environments? (jazz:get-configuration-debug-environments? configuration))
        (debug-location? (jazz:get-configuration-debug-location? configuration))
        (debug-source? (jazz:get-configuration-debug-source? configuration))
        (debug-foreign? (jazz:get-configuration-debug-foreign? configuration))
        (track-memory? (jazz:get-configuration-track-memory? configuration))
        (mutable-bindings? (jazz:get-configuration-mutable-bindings? configuration))
        (kernel-interpret? (jazz:get-configuration-kernel-interpret? configuration))
        (destination (jazz:get-configuration-destination configuration))
        (features (jazz:get-configuration-features configuration))
        (properties (jazz:get-configuration-properties configuration)))
    (jazz:feedback "{a}" (or name "<default>"))
    (jazz:feedback "  system: {s}" system)
    (jazz:feedback "  platform: {s}" platform)
    (jazz:feedback "  compiler: {s}" compiler)
    (jazz:feedback "  processor: {s}" processor)
    (jazz:feedback "  windowing: {s}" windowing)
    (jazz:feedback "  safety: {s}" safety)
    (jazz:feedback "  optimize?: {s}" optimize?)
    (jazz:feedback "  debug-environments?: {s}" debug-environments?)
    (jazz:feedback "  debug-location?: {s}" debug-location?)
    (jazz:feedback "  debug-source?: {s}" debug-source?)
    (jazz:feedback "  debug-foreign?: {s}" debug-foreign?)
    (jazz:feedback "  track-memory?: {s}" track-memory?)
    (jazz:feedback "  mutable-bindings?: {s}" mutable-bindings?)
    (jazz:feedback "  kernel-interpret?: {s}" kernel-interpret?)
    (jazz:feedback "  destination: {s}" destination)
    (jazz:feedback "  features: {s}" features)
    (jazz:feedback "  properties: {s}" properties)))


;;;
;;;; Configure
;;;


(define (jazz:configure
          template
          #!key
          (name (jazz:unspecified-option))
          (system (jazz:unspecified-option))
          (platform (jazz:unspecified-option))
          (compiler (jazz:unspecified-option))
          (processor (jazz:unspecified-option))
          (windowing (jazz:unspecified-option))
          (safety (jazz:unspecified-option))
          (optimize? (jazz:unspecified-option))
          (debug-environments? (jazz:unspecified-option))
          (debug-location? (jazz:unspecified-option))
          (debug-source? (jazz:unspecified-option))
          (debug-foreign? (jazz:unspecified-option))
          (track-memory? (jazz:unspecified-option))
          (mutable-bindings? (jazz:unspecified-option))
          (kernel-interpret? (jazz:unspecified-option))
          (destination (jazz:unspecified-option))
          (features (jazz:unspecified-option))
          (properties (jazz:unspecified-option)))
  (let ((configuration
          (jazz:validate-configuration
            template
            name: name
            system: system
            platform: platform
            compiler: compiler
            processor: processor
            windowing: windowing
            safety: safety
            optimize?: optimize?
            debug-environments?: debug-environments?
            debug-location?: debug-location?
            debug-source?: debug-source?
            debug-foreign?: debug-foreign?
            track-memory?: track-memory?
            mutable-bindings?: mutable-bindings?
            kernel-interpret?: kernel-interpret?
            destination: destination
            features: features
            properties: properties)))
    (jazz:register-configuration configuration)
    (jazz:describe-configuration configuration)))


;;;
;;;; Name
;;;


(define (jazz:require-name name template)
  (jazz:or-option name (jazz:get-configuration-name template) (jazz:default-name)))


(define (jazz:validate-name name)
  (if (or (not name) (and (symbol? name) (jazz:valid-filename? (symbol->string name))))
      name
    (jazz:error "Invalid name: {s}" name)))


;;;
;;;; System
;;;


(define jazz:valid-systems
  '(gambit))


(define (jazz:require-system system template)
  (jazz:or-option system (jazz:get-configuration-system template) (jazz:default-system)))


(define (jazz:validate-system system)
  (if (memq system jazz:valid-systems)
      system
    (jazz:error "Invalid system: {s}" system)))


;;;
;;;; Platform
;;;


(define jazz:valid-platforms
  '(mac
    ios
    windows
    unix))


(define (jazz:guess-platform)
  (let ((system (cadr (system-type)))
        (os (caddr (system-type))))
    (cond ((eq? system 'apple)
           'mac)
          ((jazz:unix-family)
           'unix)
          (else
           'windows))))


(define (jazz:require-platform platform template)
  (jazz:or-option platform (jazz:get-configuration-platform template) (jazz:default-platform) (jazz:guess-platform)))


(define (jazz:validate-platform platform)
  (if (memq platform jazz:valid-platforms)
      platform
    (jazz:error "Invalid platform: {s}" platform)))


;;;
;;;; Compiler
;;;


(define jazz:valid-compilers
  '(c
    c++))


(define (jazz:guess-compiler)
  'c)


(define (jazz:require-compiler compiler template)
  (jazz:or-option compiler (jazz:get-configuration-compiler template) (jazz:default-compiler) (jazz:guess-compiler)))


(define (jazz:validate-compiler compiler)
  (if (memq compiler jazz:valid-compilers)
      compiler
    (jazz:error "Invalid compiler: {s}" compiler)))


;;;
;;;; Processor
;;;


(define jazz:valid-processors
  '(#f
    x86
    arm))


(define (jazz:guess-processor)
  #f)


(define (jazz:require-processor processor template)
  (jazz:or-option processor (jazz:get-configuration-processor template) (jazz:default-processor) (jazz:guess-processor)))


(define (jazz:validate-processor processor)
  (if (memq processor jazz:valid-processors)
      processor
    (jazz:error "Invalid processor: {s}" processor)))


;;;
;;;; Windowing
;;;


(define jazz:valid-windowings
  '(cocoa
    #f
    x11))


(define (jazz:require-windowing platform windowing template)
  (define (guess-windowing platform)
    (case platform
      ((mac) 'cocoa)
      ((windows) #f)
      ((unix) 'x11)))
  
  (jazz:or-option windowing (jazz:get-configuration-windowing template) (jazz:default-windowing) (guess-windowing platform)))


(define (jazz:validate-windowing windowing)
  (if (memq windowing jazz:valid-windowings)
      windowing
    (jazz:error "Invalid windowing: {s}" windowing)))


;;;
;;;; Safety
;;;


(define jazz:valid-safeties
  '(core
    debug
    develop
    release
    sealed))


(define (jazz:require-safety safety template)
  (jazz:or-option safety (jazz:get-configuration-safety template) (jazz:default-safety)))


(define (jazz:validate-safety safety)
  (if (memq safety jazz:valid-safeties)
      safety
    (jazz:error "Invalid safety: {s}" safety)))


;;;
;;;; Optimize
;;;


(define jazz:valid-optimize
  '(#f
    #t))


(define (jazz:require-optimize? optimize template)
  (jazz:or-option optimize (jazz:get-configuration-optimize? template) (jazz:default-optimize?)))


(define (jazz:validate-optimize? optimize)
  (if (memq optimize jazz:valid-optimize)
      optimize
    (jazz:error "Invalid optimize?: {s}" optimize)))


;;;
;;;; Debug-Environments
;;;


(define jazz:valid-debug-environments
  '(#f
    #t))


(define (jazz:require-debug-environments? debug-environments template)
  (jazz:or-option debug-environments (jazz:get-configuration-debug-environments? template) (jazz:default-debug-environments?)))


(define (jazz:validate-debug-environments? debug-environments)
  (if (memq debug-environments jazz:valid-debug-environments)
      debug-environments
    (jazz:error "Invalid debug-environments?: {s}" debug-environments)))


;;;
;;;; Debug-Location
;;;


(define jazz:valid-debug-location
  '(#f
    #t))


(define (jazz:require-debug-location? debug-location template)
  (jazz:or-option debug-location (jazz:get-configuration-debug-location? template) (jazz:default-debug-location?)))


(define (jazz:validate-debug-location? debug-location)
  (if (memq debug-location jazz:valid-debug-location)
      debug-location
    (jazz:error "Invalid debug-location?: {s}" debug-location)))


;;;
;;;; Debug-Source
;;;


(define jazz:valid-debug-source
  '(#f
    #t))


(define (jazz:require-debug-source? debug-source template)
  (jazz:or-option debug-source (jazz:get-configuration-debug-source? template) (jazz:default-debug-source?)))


(define (jazz:validate-debug-source? debug-source)
  (if (memq debug-source jazz:valid-debug-source)
      debug-source
    (jazz:error "Invalid debug-source?: {s}" debug-source)))


;;;
;;;; Debug-Foreign
;;;


(define jazz:valid-debug-foreign
  '(#f
    #t))


(define (jazz:require-debug-foreign? debug-foreign template)
  (jazz:or-option debug-foreign (jazz:get-configuration-debug-foreign? template) (jazz:default-debug-foreign?)))


(define (jazz:validate-debug-foreign? debug-foreign)
  (if (memq debug-foreign jazz:valid-debug-foreign)
      debug-foreign
    (jazz:error "Invalid debug-foreign?: {s}" debug-foreign)))


;;;
;;;; Track-Memory
;;;


(define jazz:valid-track-memory
  '(#f
    #t))


(define (jazz:require-track-memory? track-memory template)
  (jazz:or-option track-memory (jazz:get-configuration-track-memory? template) (jazz:default-track-memory?)))


(define (jazz:validate-track-memory? track-memory)
  (if (memq track-memory jazz:valid-track-memory)
      track-memory
    (jazz:error "Invalid track-memory?: {s}" track-memory)))


;;;
;;;; Mutable-Bindings
;;;


(define jazz:valid-mutable-bindings
  '(#f
    #t))


(define (jazz:require-mutable-bindings? mutable-bindings template)
  (jazz:or-option mutable-bindings (jazz:get-configuration-mutable-bindings? template) (jazz:default-mutable-bindings?)))


(define (jazz:validate-mutable-bindings? mutable-bindings)
  (if (memq mutable-bindings jazz:valid-mutable-bindings)
      mutable-bindings
    (jazz:error "Invalid mutable-bindings?: {s}" mutable-bindings)))


;;;
;;;; Kernel-Interpret
;;;


(define jazz:valid-kernel-interpret
  '(#f
    #t))


(define (jazz:require-kernel-interpret? kernel-interpret template)
  (jazz:or-option kernel-interpret (jazz:get-configuration-kernel-interpret? template) (jazz:default-kernel-interpret?)))


(define (jazz:validate-kernel-interpret? kernel-interpret)
  (if (memq kernel-interpret jazz:valid-kernel-interpret)
      kernel-interpret
    (jazz:error "Invalid kernel-interpret?: {s}" kernel-interpret)))


;;;
;;;; Destination
;;;


(define (jazz:require-destination destination template)
  (jazz:or-option destination (jazz:get-configuration-destination template) (jazz:default-destination)))


(define (jazz:validate-destination destination)
  (if (or (not destination) (string? destination))
      destination
    (jazz:error "Invalid destination: {s}" destination)))


(define (jazz:configuration-file configuration)
  (let ((dir (jazz:configuration-directory configuration)))
    (string-append dir ".configuration")))


;;;
;;;; Features
;;;


(define (jazz:require-features features template)
  (jazz:or-option features (jazz:get-configuration-features template) (jazz:default-features)))


(define (jazz:validate-features features)
  (if (list? features)
      features
    (jazz:error "Invalid features: {s}" features)))


;;;
;;;; Properties
;;;


(define (jazz:require-properties properties template)
  (jazz:or-option properties (jazz:get-configuration-properties template) (jazz:default-properties)))


(define (jazz:validate-properties properties)
  (if (list? properties)
      properties
    (jazz:error "Invalid properties: {s}" properties)))


;;;
;;;; Symbol
;;;


(define (jazz:parse-symbols lst proc)
  (let iter ((scan lst)
             (symbols '()))
       (if (null? scan)
           (proc (reverse symbols) '())
         (let ((obj (car scan)))
           (if (or (keyword? obj) (and (symbol? obj) (eqv? #\- (string-ref (symbol->string obj) 0))))
               (proc (reverse symbols) (map jazz:stringify scan))
             (iter (cdr scan) (cons obj symbols)))))))


(define (jazz:parse-symbol symbol proc)
  (define (parse-target/configuration/image str proc)
    (let ((colon (jazz:string-find str #\:)))
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
    (let ((at (jazz:string-find str #\@)))
      (if (not at)
          (proc (if (string=? str "") (jazz:default-target) (string->symbol str)) (jazz:require-default-configuration))
        (let ((target
                (if (= at 0)
                    (jazz:default-target)
                  (string->symbol (substring str 0 at))))
              (configuration
                (if (= (+ at 1) (string-length str))
                    (jazz:require-default-configuration)
                  (jazz:require-configuration (string->symbol (substring str (+ at 1) (string-length str)))))))
          (proc target configuration)))))
  
  (define (standardize-image image)
    (cond ((memv image '(lib library)) 'library)
          ((memv image '(exe executable)) 'executable)
          (else (jazz:error "Unknown image type: {s}" image))))
  
  (let ((name (symbol->string symbol)))
    (parse-target/configuration/image name proc)))


;;;
;;;; Make
;;;


(define (jazz:make-symbols symbols local?)
  (define (make-symbol symbol arguments)
    (jazz:parse-symbol symbol
      (lambda (target configuration image)
        (make-target target configuration image arguments local?))))
  
  (define (make-target target configuration image arguments local?)
    (case target
      ((clean) (jazz:make-clean configuration))
      ((cleankernel) (jazz:make-cleankernel configuration))
      ((cleanproducts) (jazz:make-cleanproducts configuration))
      ((cleanobject) (jazz:make-cleanobject configuration))
      ((cleanlibrary) (jazz:make-cleanlibrary configuration))
      ((kernel) (jazz:make-kernel configuration image local?))
      ((install) (jazz:make-install configuration))
      (else (jazz:make-product target configuration arguments))))
  
  (jazz:parse-symbols symbols
    (lambda (symbols arguments)
      (let iter ((scan (if (null? symbols)
                           (list (jazz:default-target))
                         symbols)))
           (if (not (null? scan))
               (let ((symbol (car scan)))
                 (and (make-symbol symbol arguments)
                      (let ((tail (cdr scan)))
                        (if (not (null? tail))
                            (newline (console-port)))
                        (iter tail)))))))))


(define (jazz:make symbol)
  (jazz:make-symbols (list symbol) #t))


;;;
;;;; Install
;;;


(define (jazz:install-symbols symbols local?)
  (define (install-symbol symbol)
    (jazz:parse-symbol symbol
      (lambda (target configuration image)
        (install-target target configuration image '() local?))))
  
  (define (install-target target configuration image arguments local?)
    (jazz:install-product target configuration arguments))
  
  (jazz:parse-symbols symbols
    (lambda (symbols arguments)
      (let iter ((scan (if (null? symbols)
                           (list (jazz:default-target))
                         symbols)))
           (if (not (null? scan))
               (let ((symbol (car scan)))
                 (install-symbol symbol)
                 (let ((tail (cdr scan)))
                   (if (not (null? tail))
                       (newline (console-port)))
                   (iter tail))))))))


;;;
;;;; Deploy
;;;


(define (jazz:deploy-symbol symbol arguments)
  (jazz:parse-symbol symbol
    (lambda (target configuration image)
      (jazz:deploy-product target configuration arguments))))


;;;
;;;; Run
;;;


(define (jazz:run-symbol symbol arguments)
  (jazz:parse-symbol symbol
    (lambda (target configuration image)
      (jazz:run-product target configuration arguments))))


;;;
;;;; Test
;;;


(define (jazz:test-symbol symbol arguments)
  (jazz:parse-symbol symbol
    (lambda (target configuration image)
      (jazz:test-product target configuration arguments))))


;;;
;;;; Clean
;;;


(define (jazz:clean-toplevel-file? file level)
  (define ignored-toplevel-files
    '(".gitignore"))
  
  (not (member (jazz:pathname-name file) ignored-toplevel-files)))


(define (jazz:clean-toplevel-directory? dir level)
  (define ignored-toplevel-dirnames
    '(".git"))
  
  (not (member (jazz:pathname-name dir) ignored-toplevel-dirnames)))


(define (jazz:ignored-libraries-directory configuration)
  (if (eq? (jazz:get-configuration-platform configuration) 'mac)
      "Libraries"
    "lib"))


(define (jazz:make-clean configuration)
  (define delete-feedback
    (jazz:delete-feedback 1))
  
  (jazz:feedback "make clean")
  (let ((dir (jazz:configuration-directory configuration)))
    (if (file-exists? dir)
        (jazz:delete-directory dir
                               0
                               jazz:clean-toplevel-file?
                               (lambda (dir level)
                                 (if (jazz:clean-toplevel-directory? dir level)
                                     (jazz:empty-directory dir
                                                           level
                                                           #f
                                                           #f
                                                           delete-feedback)
                                   #f))
                               delete-feedback))))


(define (jazz:make-cleankernel configuration)
  (define delete-feedback
    (jazz:delete-feedback 1))
  
  (jazz:feedback "make cleankernel")
  (let ((dir (jazz:configuration-directory configuration))
        (ignored-lib (jazz:ignored-libraries-directory configuration)))
    (if (file-exists? dir)
        (jazz:delete-directory dir
                               0
                               jazz:clean-toplevel-file?
                               (lambda (dir level)
                                 (if (and (jazz:clean-toplevel-directory? dir level)
                                          (not (string=? (jazz:pathname-name dir) ignored-lib)))
                                     (jazz:empty-directory dir
                                                           level
                                                           #f
                                                           #f
                                                           delete-feedback)
                                   #f))
                               delete-feedback))))


(define (jazz:make-cleanproducts configuration)
  (define delete-feedback
    (jazz:delete-feedback 0))
  
  (jazz:feedback "make cleanproducts")
  (let ((dir (jazz:configuration-directory configuration)))
    (if (file-exists? dir)
        (let ((products-dir (string-append dir "build/products/")))
          (if (file-exists? products-dir)
              (jazz:delete-directory products-dir
                                     0
                                     #f
                                     #f
                                     delete-feedback))))))


(define (jazz:make-cleanobject configuration)
  (define delete-feedback
    (jazz:delete-feedback 2))
  
  (define (object-file? file level)
    (let ((ext (jazz:pathname-extension file)))
      (or (jazz:extension? ext "c")
          (jazz:extension? ext "cpp")
          (jazz:extension? ext "dgs")
          (jazz:extension? ext "mnf")
          (jazz:extension? ext "otl")
          (jazz:extension? ext "o")
          (jazz:numeric-extension? ext "o"))))
  
  (define (empty-objects dir level)
    (jazz:empty-directory dir
                          level
                          object-file?
                          empty-objects
                          delete-feedback)
    (jazz:cleanup-package dir level delete-feedback))
  
  (jazz:feedback "make cleanobject")
  (let ((dir (jazz:configuration-directory configuration))
        (ignored-lib (jazz:ignored-libraries-directory configuration)))
    (if (file-exists? dir)
        (jazz:delete-directory dir
                               0
                               (lambda (file level)
                                 #f)
                               (lambda (dir level)
                                 (if (string=? (jazz:pathname-name dir) ignored-lib)
                                     (jazz:empty-directory dir
                                                           level
                                                           (lambda (file level)
                                                             #f)
                                                           empty-objects
                                                           delete-feedback)
                                   #f))
                               delete-feedback))))


(define (jazz:make-cleanlibrary configuration)
  (define delete-feedback
    (jazz:delete-feedback 2))
  
  (define (library-file? file level)
    (let ((base (jazz:pathname-base file))
          (ext (jazz:pathname-extension file)))
      (or (and (jazz:string-ends-with? base "lmf")
               (jazz:extension? ext "scm"))
          (jazz:numeric-extension? ext "l"))))
  
  (define (empty-libraries dir level)
    (let ((static-dir (string-append dir "static/")))
      (if (file-exists? static-dir)
          (begin
            (jazz:feedback "; deleting {a}..." static-dir)
            (jazz:delete-directory static-dir))))
    (jazz:empty-directory dir
                          level
                          library-file?
                          empty-libraries
                          delete-feedback)
    (jazz:cleanup-package dir level delete-feedback))
  
  (jazz:feedback "make cleanlibrary")
  (let ((dir (jazz:configuration-directory configuration))
        (ignored-lib (jazz:ignored-libraries-directory configuration)))
    (if (file-exists? dir)
        (jazz:delete-directory dir
                               0
                               (lambda (file level)
                                 #f)
                               (lambda (dir level)
                                 (if (string=? (jazz:pathname-name dir) ignored-lib)
                                     (jazz:empty-directory dir
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
(define (jazz:cleanup-package dir level feedback)
  (let ((content (jazz:directory-content dir)))
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


(define (jazz:delete-feedback depth)
  (lambda (path level)
    (if (<= level depth)
        (jazz:feedback "; deleting {a}..." path))))


;;;
;;;; Install
;;;


(define (jazz:make-install configuration)
  (jazz:error "Make install is not supported. See INSTALL for details"))


;;;
;;;; Kernel
;;;


(define (jazz:make-kernel configuration image local?)
  (define (build-kernel configuration image)
    (define (build configuration)
      (let ((configuration (jazz:invoke-build-configure configuration)))
        (let ((name (jazz:get-configuration-name configuration))
              (system (jazz:get-configuration-system configuration))
              (platform (jazz:get-configuration-platform configuration))
              (compiler (jazz:get-configuration-compiler configuration))
              (processor (jazz:get-configuration-processor configuration))
              (windowing (jazz:get-configuration-windowing configuration))
              (safety (jazz:get-configuration-safety configuration))
              (optimize? (jazz:get-configuration-optimize? configuration))
              (debug-environments? (jazz:get-configuration-debug-environments? configuration))
              (debug-location? (jazz:get-configuration-debug-location? configuration))
              (debug-source? (jazz:get-configuration-debug-source? configuration))
              (debug-foreign? (jazz:get-configuration-debug-foreign? configuration))
              (track-memory? (jazz:get-configuration-track-memory? configuration))
              (mutable-bindings? (jazz:get-configuration-mutable-bindings? configuration))
              (include-compiler? (not (eq? image 'library)))
              (kernel-interpret? (jazz:get-configuration-kernel-interpret? configuration))
              (source jazz:source)
              (destination (jazz:get-configuration-destination configuration))
              (destination-directory (jazz:configuration-directory configuration))
              (features (jazz:get-configuration-features configuration))
              (properties (jazz:get-configuration-properties configuration)))
          (jazz:build-image #f
                            system:                system
                            platform:              platform
                            compiler:              compiler
                            processor:             processor
                            windowing:             windowing
                            safety:                safety
                            optimize?:             optimize?
                            debug-environments?:   debug-environments?
                            debug-location?:       debug-location?
                            debug-source?:         debug-source?
                            debug-foreign?:        debug-foreign?
                            track-memory?:         track-memory?
                            mutable-bindings?:     mutable-bindings?
                            include-compiler?:     include-compiler?
                            kernel-interpret?:     kernel-interpret?
                            source:                source
                            destination:           destination
                            destination-directory: destination-directory
                            features:              features
                            properties:            properties
                            image:                 image
                            kernel?:               #t
                            console?:              #t))))
    
    (define (compare-configuration configuration-jazz configuration-kernel)
      (define (compare-parameter name proc)
        (if (not (equal? (proc configuration-jazz) (proc configuration-kernel)))
            (list name (proc configuration-jazz) (proc configuration-kernel))
          #f))
      
      (or (compare-parameter system:              jazz:get-configuration-system)
          (compare-parameter platform:            jazz:get-configuration-platform)
          (compare-parameter compiler:            jazz:get-configuration-compiler)
          (compare-parameter processor:           jazz:get-configuration-processor)
          (compare-parameter windowing:           jazz:get-configuration-windowing)
          (compare-parameter safety:              jazz:get-configuration-safety)
          (compare-parameter optimize?:           jazz:get-configuration-optimize?)
          (compare-parameter debug-environments?: jazz:get-configuration-debug-environments?)
          (compare-parameter debug-location?:     jazz:get-configuration-debug-location?)
          (compare-parameter debug-source?:       jazz:get-configuration-debug-source?)
          (compare-parameter debug-foreign:       jazz:get-configuration-debug-foreign?)
          (compare-parameter track-memory:        jazz:get-configuration-track-memory?)
          (compare-parameter mutable-bindings:    jazz:get-configuration-mutable-bindings?)
          (compare-parameter kernel-interpret?:   jazz:get-configuration-kernel-interpret?)
          (compare-parameter destination:         jazz:get-configuration-destination)
          (compare-parameter features:            jazz:get-configuration-features)
          (compare-parameter properties:          jazz:get-configuration-properties)))
    
    (jazz:feedback "make kernel")
    (let ((configuration (or configuration (jazz:require-default-configuration))))
      (let ((configuration-file (jazz:configuration-file configuration)))
        (if (file-exists? configuration-file)
            (let ((file-configuration (jazz:load-configuration configuration-file)))
              (let ((difference (compare-configuration configuration file-configuration)))
                (if difference
                    (jazz:error "Configuration mismatch in {a}" difference)
                  (build file-configuration))))
          (build configuration)))))
  
  (define (build-recursive target configuration image)
    (let ((configuration-name (jazz:get-configuration-name configuration)))
      (let ((jaz (string-append jazz:source "jaz"))
            (argument (string-append (if configuration-name
                                         (jazz:format "{a}@{a}" target configuration-name)
                                       (symbol->string target))
                                     (if image
                                         (string-append ":" (symbol->string image))
                                       ""))))
        (jazz:call-process (list path: "sh" arguments: `(,jaz "make" ,argument))))))
  
  (if local?
      (build-kernel configuration image)
    (build-recursive 'kernel configuration image)))


(define (jazz:invoke-build-configure configuration)
  (let ((proc (jazz:build-configure)))
    (if proc
        (proc configuration)
      configuration)))


;;;
;;;; Product
;;;


(define (jazz:make-product product configuration arguments)
  (jazz:make-kernel configuration #f #f)
  (= 0 (jazz:invoke-process
         (list
           path: (string-append (jazz:configuration-directory configuration) "jazz")
           arguments: `("-:daD"
                        "-make"
                        ,(symbol->string product)
                        #; ;; dynamic-dependencies
                        ,@(let ((dependencies (string-append (current-directory) ".dependencies")))
                            (if (file-exists? dependencies)
                                `("-dependencies" ,dependencies)
                              '()))
                        ,@arguments)))))


(define (jazz:install-product product configuration arguments)
  (jazz:call-process
     (list
       path: (string-append (jazz:configuration-directory configuration) "jazz")
       arguments: `("-install"
                    ,(symbol->string product)
                    ,@arguments))))


(define (jazz:deploy-product product configuration arguments)
  (jazz:call-process
     (list
       path: (string-append (jazz:configuration-directory configuration) "jazz")
       arguments: `("-deploy"
                    ,(symbol->string product)
                    ,@arguments))))


(define (jazz:run-product product configuration arguments)
  (jazz:call-process
     (list
       path: (string-append (jazz:configuration-directory configuration) "jazz")
       arguments: `("-run"
                    ,(symbol->string product)
                    ,@arguments))))


(define (jazz:test-product product configuration arguments)
  (jazz:call-process
     (list
       path: (string-append (jazz:configuration-directory configuration) "jazz")
       arguments: `("-test"
                    ,(symbol->string product)
                    ,@arguments))))


;;;
;;;; Output
;;;


(define (jazz:print line output)
  (display line output)
  (newline output))


(define (jazz:debug . rest)
  (jazz:print rest (console-port)))


;;;
;;;; Format
;;;


(define (jazz:format fmt-string . arguments)
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


(define (jazz:find-if predicate lst)
  (let iter ((scan lst))
    (if (null? scan)
        #f
      (let ((value (car scan)))
        (if (predicate value)
            value
          (iter (cdr scan)))))))


(define (jazz:collect-if predicate lst)
  (let iter ((scan lst))
    (if (not (null? scan))
        (let ((value (car scan)))
          (if (predicate value)
              (cons value (iter (cdr scan)))
            (iter (cdr scan))))
      '())))


(define (jazz:filter pred lis)
  (let recur ((lis lis))
    (if (null? lis) lis
      (let ((head (car lis))
            (tail (cdr lis)))
        (if (pred head)
            (let ((new-tail (recur tail)))
              (if (eq? tail new-tail) lis
                (cons head new-tail)))
          (recur tail))))))


(define (jazz:delete x lis test)
  (jazz:filter (lambda (y) (not (test x y))) lis))


;;;
;;;; String
;;;


(define (jazz:string-find str c)
  (let ((len (string-length str)))
    (let iter ((n 0))
      (cond ((>= n len)
             #f)
            ((char=? (string-ref str n) c)
             n)
            (else
             (iter (+ n 1)))))))


(define (jazz:string-ends-with? str target)
  (let ((sl (string-length str))
        (tl (string-length target)))
    (and (>= sl tl)
         (string=? (substring str (- sl tl) sl) target))))


(define (jazz:split-string str separator)
  (let ((lst '())
        (end (string-length str)))
    (let iter ((pos (- end 1)))
      (if (>= pos 0)
          (begin
            (if (eqv? (string-ref str pos) separator)
                (begin
                  (set! lst (cons (substring str (+ pos 1) end) lst))
                  (set! end pos)))
            (iter (- pos 1))))
        (cons (substring str 0 end) lst))))


(define (jazz:join-strings strings separator)
  (let ((output (open-output-string)))
    (if (pair? strings)
        (begin
          (display (car strings) output)
          (for-each (lambda (string)
                      (display separator output)
                      (display string output))
                    (cdr strings))))
    (get-output-string output)))


(define (jazz:stringify expr)
  (if (string? expr)
      expr
    (let ((output (open-output-string)))
      (display expr output)
      (get-output-string output))))


;;;
;;;; Pathname
;;;


(define (jazz:valid-filename? str)
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


(define (jazz:error fmt-string . rest)
  (let ((error-string (apply jazz:format fmt-string rest)))
    (error error-string)))


;;;
;;;; Repl
;;;


(define jazz:prompt
  "% ")

(define jazz:display-exception?
  #t)

(define jazz:display-backtrace?
  #f)


(define (jazz:build-system-repl)
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
                    ((install) (install-command arguments output))
                    ((deploy) (deploy-command arguments output))
                    ((run) (run-command arguments output))
                    ((test) (test-command arguments output))
                    ((help ?) (help-command arguments output))
                    ((quit) (quit-command arguments output))
                    (else (jazz:error "Unknown command: {s}" command))))
                #t)))))))
  
  (define (list-command arguments output)
    (jazz:list-configurations))
  
  (define (delete-command arguments output)
    (let ((name (if (null? arguments) #f (car arguments))))
      (jazz:delete-configuration (jazz:require-configuration name))
      (jazz:list-configurations)))
  
  (define (configure-command arguments output)
    (if (and (pair? arguments)
             (symbol? (car arguments)))
        (let ((template (jazz:configuration-template (car arguments))))
          (apply jazz:configure template (cdr arguments)))
      (apply jazz:configure jazz:unspecified-configuration arguments)))
  
  (define (make-command arguments output)
    (jazz:setup-kernel-build)
    (jazz:make-symbols arguments #f))
  
  (define (install-command arguments output)
    (jazz:setup-kernel-install)
    (jazz:install-symbols arguments #f))
  
  (define (deploy-command arguments output)
    (jazz:setup-kernel-install)
    (jazz:deploy-symbol (car arguments) (map jazz:stringify (cdr arguments))))
  
  (define (run-command arguments output)
    (jazz:setup-kernel-install)
    (jazz:run-symbol (car arguments) (map jazz:stringify (cdr arguments))))
  
  (define (test-command arguments output)
    (jazz:setup-kernel-install)
    (jazz:test-symbol (car arguments) (map jazz:stringify (cdr arguments))))
  
  (define (help-command arguments output)
    (jazz:print "Commands:" output)
    (jazz:print "  configure [name:] [system:] [platform:] [compiler:] [processor:] [windowing:] [safety:] [optimize?:] [debug-environments?:] [debug-location?:] [debug-source?:] [debug-foreign?:] [track-memory?:] [mutable-bindings?:] [kernel-interpret?:] [destination:] [features:] [properties:]" output)
    (jazz:print "  make [target | clean | cleankernel | cleanproducts | cleanobject | cleanlibrary]@[configuration]:[image]" output)
    (jazz:print "  install [target]" output)
    (jazz:print "  deploy [target]" output)
    (jazz:print "  run [target]" output)
    (jazz:print "  test [target]" output)
    (jazz:print "  list" output)
    (jazz:print "  delete [configuration]" output)
    (jazz:print "  help or ?" output)
    (jazz:print "  quit" output))
  
  (define (quit-command arguments output)
    (exit))
  
  (define (debug-exception exc console)
    (if jazz:display-exception?
        (display-exception exc console))
    (if jazz:display-backtrace?
        (continuation-capture
          (lambda (cont)
            (display-continuation-backtrace cont console #t #t 100 100)))))
  
  (let ((console (console-port)))
    (jazz:print (jazz:format "JazzScheme Build System v{a}" (jazz:present-version (jazz:get-jazz-version-number))) console)
    (force-output console)
    (jazz:setup-settings)
    (jazz:load-configurations)
    (jazz:process-jazini)
    (let loop ((newline? #t))
      (if newline?
          (newline console))
      (display jazz:prompt console)
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


(define (jazz:build-system-boot)
  (define (fatal message)
    (display message)
    (newline)
    (force-output)
    (exit 1))
  
  (define (missing-argument-for-option opt)
    (fatal (jazz:format "Missing argument for option: {a}" opt)))
  
  (define (read-argument arg)
    (call-with-input-string (list init: arg)
      read))
  
  (define (string-option name options)
    (let ((opt (jazz:find-option name options)))
      (if (not opt)
          (jazz:unspecified-option)
        opt)))
  
  (define (symbol-option name options)
    (let ((opt (jazz:find-option name options)))
      (if (not opt)
          (jazz:unspecified-option)
        (string->symbol opt))))
  
  (define (boolean-option name options)
    (let ((opt (jazz:find-option name options)))
      (cond ((not opt)
             (jazz:unspecified-option))
            ((string-ci=? opt "false")
             #f)
            ((string-ci=? opt "true")
             #t)
            (else
             (fatal (jazz:format "Invalid boolean argument for option: {a}" name))))))
  
  (define (list-option name options)
    (let ((opt (jazz:find-option name options)))
      (if (not opt)
          (jazz:unspecified-option)
        opt)))
  
  (define (unknown-option opt)
    (fatal (jazz:format "Unknown option: {a}" opt)))
  
  (let ((command-arguments (jazz:command-arguments)))
    (if (null? command-arguments)
        (jazz:build-system-repl)
      (let ((action (car command-arguments))
            (arguments (cdr command-arguments)))
        (jazz:setup-settings)
        (jazz:load-configurations)
        (jazz:process-jazini)
        (cond ((equal? action "clone")
               (jazz:clone-dependencies (map read-argument arguments))
               (exit))
              ((equal? action "list")
               (jazz:list-configurations)
               (exit))
              ((equal? action "delete")
               (let ((name (if (null? arguments) #f (read-argument (car arguments)))))
                 (jazz:delete-configuration (jazz:require-configuration name))
                 (jazz:list-configurations))
               (exit))
              ((equal? action "configure")
               (let ()
                 (define (configure template arguments)
                   (jazz:split-command-line arguments '() '("name" "system" "platform" "compiler" "processor" "windowing" "safety" "optimize" "debug-environments" "debug-location" "debug-source" "debug-foreign" "track-memory" "mutable-bindings" "kernel-interpret" "destination" "features" "properties") missing-argument-for-option
                     (lambda (commands options remaining)
                       (if (null? remaining)
                           (let ((name (symbol-option "name" options))
                                 (system (symbol-option "system" options))
                                 (platform (symbol-option "platform" options))
                                 (compiler (symbol-option "compiler" options))
                                 (processor (symbol-option "processor" options))
                                 (windowing (symbol-option "windowing" options))
                                 (safety (symbol-option "safety" options))
                                 (optimize (boolean-option "optimize" options))
                                 (debug-environments (boolean-option "debug-environments" options))
                                 (debug-location (boolean-option "debug-location" options))
                                 (debug-source (boolean-option "debug-source" options))
                                 (debug-foreign (boolean-option "debug-foreign" options))
                                 (track-memory (boolean-option "track-memory" options))
                                 (mutable-bindings (boolean-option "mutable-bindings" options))
                                 (kernel-interpret (boolean-option "kernel-interpret" options))
                                 (destination (string-option "destination" options))
                                 (features (list-option "features" options))
                                 (properties (list-option "properties" options)))
                             (jazz:configure template name: name system: system platform: platform compiler: compiler processor: processor windowing: windowing safety: safety optimize?: optimize debug-environments?: debug-environments debug-location?: debug-location debug-source?: debug-source debug-foreign?: debug-foreign track-memory?: track-memory mutable-bindings?: mutable-bindings kernel-interpret?: kernel-interpret destination: destination features: features properties: properties)
                             (exit))
                         (unknown-option (car remaining))))))
                 
                 (if (and (pair? arguments)
                          (not (jazz:option? (car arguments))))
                     (let ((template (jazz:configuration-template (string->symbol (car arguments)))))
                       (configure template (cdr arguments)))
                   (configure jazz:unspecified-configuration arguments))))
              ((equal? action "make")
               (jazz:setup-kernel-build)
               (if (jazz:make-symbols (map read-argument arguments) #t)
                   (exit 0)
                 (exit 1)))
              ((equal? action "install")
               (jazz:setup-kernel-install)
               (jazz:install-symbols (map read-argument arguments) #t)
               (exit))
              ((equal? action "deploy")
               (jazz:setup-kernel-install)
               (jazz:deploy-symbol (read-argument (car arguments)) (cdr arguments)))
              ((equal? action "run")
               (jazz:setup-kernel-install)
               (jazz:run-symbol (read-argument (car arguments)) (cdr arguments))
               (exit))
              ((equal? action "test")
               (jazz:setup-kernel-install)
               (jazz:test-symbol (read-argument (car arguments)) (cdr arguments))
               (exit))
              ((or (equal? action "help") (equal? action "?"))
               (let ((console (console-port)))
                 (jazz:print "Usage:" console)
                 (jazz:print "  jaz configure [-name] [-system] [-platform] [-compiler] [-processor] [-windowing] [-safety] [-optimize] [-debug-environments] [-debug-location] [-debug-source] [-kernel-interpret] [-destination] [-features] [-properties]" console)
                 (jazz:print "  jaz make [target | clean | cleankernel | cleanproducts | cleanobject | cleanlibrary]@[configuration]:[image]" console)
                 (jazz:print "  jaz install" console)
                 (jazz:print "  jaz deploy" console)
                 (jazz:print "  jaz run" console)
                 (jazz:print "  jaz test" console)
                 (jazz:print "  jaz list" console)
                 (jazz:print "  jaz delete [configuration]" console)
                 (jazz:print "  jaz help or ?" console)
                 (jazz:print "  jaz debug" console))
               (exit))
              ((equal? action "debug")
               (jazz:setup-kernel-build)
               (jazz:repl-debug-main))
              (else
               (fatal (jazz:format "Unknown build system action: {a}" action))))))))


;;;
;;;; Kernel
;;;


(define jazz:kernel-system
  'gambit)

(define jazz:kernel-platform
  #f)

(define jazz:kernel-compiler
  'c)

(define jazz:kernel-processor
  #f)

(define jazz:kernel-windowing
  #f)

(define jazz:kernel-safety
  'develop)

(define jazz:kernel-optimize?
  #t)

(define jazz:kernel-debug-environments?
  #f)

(define jazz:kernel-debug-location?
  #f)

(define jazz:kernel-debug-source?
  #f)

(define jazz:kernel-debug-foreign?
  #f)

(define jazz:kernel-track-memory?
  #f)

(define jazz:kernel-mutable-bindings?
  #f)

(define jazz:kernel-destination
  #f)

(define jazz:kernel-features
  '())

(define jazz:kernel-properties
  '())


(define jazz:setup-kernel-build
  (let ((kernel-build-setup? #f))
    (lambda ()
      (if (not kernel-build-setup?)
          (begin
            (jazz:process-buildini #f)
            (set! kernel-build-setup? #t))))))


(define (jazz:setup-kernel-install)
  (jazz:setup-kernel-build))


(define (jazz:process-jazini)
  (jazz:load-global/local ".jazini"))


;;;
;;;; Load
;;;


(load (string-append jazz:source "kernel/boot"))
(jazz:load-kernel #f)


;;;
;;;; Templates
;;;


(define jazz:unspecified-configuration
  (jazz:new-configuration
    name: (jazz:unspecified-option)
    system: (jazz:unspecified-option)
    platform: (jazz:unspecified-option)
    compiler: (jazz:unspecified-option)
    processor: (jazz:unspecified-option)
    windowing: (jazz:unspecified-option)
    safety: (jazz:unspecified-option)
    optimize?: (jazz:unspecified-option)
    debug-environments?: (jazz:unspecified-option)
    debug-location?: (jazz:unspecified-option)
    debug-source?: (jazz:unspecified-option)
    debug-foreign?: (jazz:unspecified-option)
    track-memory?: (jazz:unspecified-option)
    mutable-bindings?: (jazz:unspecified-option)
    kernel-interpret?: (jazz:unspecified-option)
    destination: (jazz:unspecified-option)
    features: (jazz:unspecified-option)
    properties: (jazz:unspecified-option)))

(define jazz:core-configuration
  (jazz:new-configuration
    name: 'c
    system: (jazz:unspecified-option)
    platform: (jazz:unspecified-option)
    compiler: (jazz:unspecified-option)
    processor: (jazz:unspecified-option)
    windowing: (jazz:unspecified-option)
    safety: 'core
    optimize?: #f
    debug-environments?: #t
    debug-location?: #t
    debug-source?: #f
    debug-foreign?: #f
    track-memory?: (jazz:unspecified-option)
    mutable-bindings?: (jazz:unspecified-option)
    kernel-interpret?: #t
    destination: "build/core"
    features: (jazz:unspecified-option)
    properties: (jazz:unspecified-option)))

(define jazz:debug-configuration
  (jazz:new-configuration
    name: 'd
    system: (jazz:unspecified-option)
    platform: (jazz:unspecified-option)
    compiler: (jazz:unspecified-option)
    processor: (jazz:unspecified-option)
    windowing: (jazz:unspecified-option)
    safety: 'debug
    optimize?: #f
    debug-environments?: #t
    debug-location?: #t
    debug-source?: #f
    debug-foreign?: #f
    track-memory?: (jazz:unspecified-option)
    mutable-bindings?: (jazz:unspecified-option)
    kernel-interpret?: #t
    destination: "build/debug"
    features: (jazz:unspecified-option)
    properties: (jazz:unspecified-option)))

(define jazz:develop-configuration
  (jazz:new-configuration
    name: 'l
    system: (jazz:unspecified-option)
    platform: (jazz:unspecified-option)
    compiler: (jazz:unspecified-option)
    processor: (jazz:unspecified-option)
    windowing: (jazz:unspecified-option)
    safety: 'develop
    optimize?: #f
    debug-environments?: #t
    debug-location?: #t
    debug-source?: #f
    debug-foreign?: #f
    track-memory?: (jazz:unspecified-option)
    mutable-bindings?: (jazz:unspecified-option)
    kernel-interpret?: #t
    destination: "build/develop"
    features: (jazz:unspecified-option)
    properties: (jazz:unspecified-option)))

(define jazz:release-configuration
  (jazz:new-configuration
    name: 'r
    system: (jazz:unspecified-option)
    platform: (jazz:unspecified-option)
    compiler: (jazz:unspecified-option)
    processor: (jazz:unspecified-option)
    windowing: (jazz:unspecified-option)
    safety: 'release
    optimize?: #f
    debug-environments?: #t
    debug-location?: #t
    debug-source?: #f
    debug-foreign?: #f
    track-memory?: (jazz:unspecified-option)
    mutable-bindings?: (jazz:unspecified-option)
    kernel-interpret?: #t
    destination: "build/release"
    features: (jazz:unspecified-option)
    properties: (jazz:unspecified-option)))

(define jazz:sealed-configuration
  (jazz:new-configuration
    name: 's
    system: (jazz:unspecified-option)
    platform: (jazz:unspecified-option)
    compiler: (jazz:unspecified-option)
    processor: (jazz:unspecified-option)
    windowing: (jazz:unspecified-option)
    safety: 'sealed
    optimize?: #t
    debug-environments?: #f
    debug-location?: #f
    debug-source?: #f
    debug-foreign?: #f
    track-memory?: (jazz:unspecified-option)
    mutable-bindings?: (jazz:unspecified-option)
    kernel-interpret?: #f
    destination: "build/sealed"
    features: (jazz:unspecified-option)
    properties: (jazz:unspecified-option)))


(define jazz:configuration-templates
  `((c . ,jazz:core-configuration)
    (d . ,jazz:debug-configuration)
    (l . ,jazz:develop-configuration)
    (r . ,jazz:release-configuration)
    (s . ,jazz:sealed-configuration)))


(define (jazz:configuration-template name)
  (let ((pair (assq name jazz:configuration-templates)))
    (if pair
        (cdr pair)
      (jazz:error "Unknown configuration template: {s}" name))))


;;;
;;;; Initialize
;;;


(jazz:setup-versions)
(jazz:build-system-boot)
