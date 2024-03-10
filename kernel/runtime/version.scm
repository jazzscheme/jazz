;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Kernel Version
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


(block kernel.version


;;;
;;;; Version
;;;


(jazz:define-structure Version () (constructor: jazz:make-version predicate: jazz:version?)
  ((number               getter: generate)
   (gambit-version       getter: generate)
   (gambit-stamp         getter: generate)
   (rebuild              getter: generate)
   (recompile            getter: generate)
   (recompile-references getter: generate)
   (sweep                getter: generate)
   (update               getter: generate)
   (description          getter: generate)))


(define (jazz:new-version
          #!key
          (version #f)
          (gambit-version #f)
          (gambit-stamp #f)
          (rebuild #f)
          (recompile #f)
          (recompile-references #f)
          (sweep #f)
          (update #f)
          (description #f))
  (jazz:make-version
    version
    gambit-version
    gambit-stamp
    rebuild
    recompile
    recompile-references
    sweep
    update
    description))


(define (jazz:split-version number)
  (let ((str (number->string number)))
    (let ((len (string-length str)))
      (let ((major (string->number (substring str 0 (- len 5))))
            (minor (string->number (substring str (- len 5) (- len 3))))
            (revision (string->number (substring str (- len 3) len))))
        (values major minor revision)))))


(define (jazz:present-version number)
  (receive (major minor revision) (jazz:split-version number)
    (string-append (number->string major)
                   "."
                   (number->string minor)
                   "."
                   (number->string revision))))


;;;
;;;; Versions
;;;


(define jazz:jazz-versions-file
  #f)

(define jazz:jazz-versions-cache
  #f)

(define jazz:jazz-version-number
  #f)

(define jazz:jazz-gambit-version
  #f)

(define jazz:jazz-gambit-stamp
  #f)


(define (jazz:load-versions-file file)
  (if (and file (file-exists? file))
      (call-with-input-file (list path: file eol-encoding: 'cr-lf)
        (lambda (input)
          (map (lambda (arguments)
                 (apply jazz:new-version arguments))
               (read-all input read))))
    #f))


(define jazz:load-jazz-versions
  (let ((loaded? #f))
    (lambda ()
      (define (determine-jazz-versions-file)
        (or jazz:jazz-versions-file
            (and jazz:jazz-source (string-append jazz:jazz-source "kernel/versions"))))
      
      (define (setup-jazz-gambit-version/stamp)
        (if jazz:jazz-versions-cache
            (let iter ((jazz-versions jazz:jazz-versions-cache))
              (if (not (null? jazz-versions))
                  (let ((jazz-version (car jazz-versions)))
                    (let ((gambit-version (jazz:get-version-gambit-version jazz-version))
                          (gambit-stamp (jazz:get-version-gambit-stamp jazz-version)))
                      (if gambit-version
                          (begin
                            (set! jazz:jazz-gambit-version gambit-version)
                            (set! jazz:jazz-gambit-stamp gambit-stamp))
                        (iter (cdr jazz-versions)))))))))
      
      (if (not loaded?)
          (begin
            (set! jazz:jazz-versions-cache (or (jazz:load-versions-file (determine-jazz-versions-file)) jazz:jazz-versions))
            (set! jazz:jazz-version-number (jazz:get-version-number (car jazz:jazz-versions-cache)))
            (setup-jazz-gambit-version/stamp)
            (set! loaded? #t))))))


(define (jazz:get-jazz-versions)
  (jazz:load-jazz-versions)
  jazz:jazz-versions-cache)


(define (jazz:get-jazz-version-number)
  (jazz:load-jazz-versions)
  jazz:jazz-version-number)


(define (jazz:get-jazz-gambit-version)
  (jazz:load-jazz-versions)
  jazz:jazz-gambit-version)


(define (jazz:get-jazz-gambit-stamp)
  (jazz:load-jazz-versions)
  jazz:jazz-gambit-stamp)


(define (jazz:gambit-jazz? gambit-vendor)
  (and gambit-vendor
       (string=? gambit-vendor "Jazz")))

(define (jazz:gambit-uptodate? gambit-version gambit-stamp)
  (let ((jazz-gambit-version (jazz:get-jazz-gambit-version))
        (jazz-gambit-stamp (jazz:get-jazz-gambit-stamp)))
    (or (not jazz-gambit-version)
        (> gambit-version jazz-gambit-version)
        (if jazz-gambit-stamp
            (>= gambit-stamp jazz-gambit-stamp)
          (>= gambit-version jazz-gambit-version)))))


;;;
;;;; Update
;;;


(jazz:define-structure Update () (constructor: jazz:make-update)
  ((version     getter: generate)
   (targets     getter: generate)
   (description getter: generate)))


(define (jazz:new-update
          #!key
          (version #f)
          (targets #f)
          (description #f))
  (jazz:make-update
    version
    targets
    description))


;;;
;;;; Updates
;;;


(define jazz:jazz-updates-file
  #f)

(define jazz:jazz-updates-cache
  #f)


(define (jazz:get-jazz-updates)
  (define (determine-jazz-updates-file)
    (or jazz:jazz-updates-file
        (and jazz:jazz-source (string-append jazz:jazz-source "kernel/updates"))))
  
  (define (load-updates)
    (let ((file (determine-jazz-updates-file)))
      (if (and file (file-exists? file))
          (call-with-input-file (list path: file eol-encoding: 'cr-lf)
            (lambda (input)
              (jazz:list->updates (read-all input read))))
        #f)))
  
  (if (not jazz:jazz-updates-cache)
      (set! jazz:jazz-updates-cache (or (load-updates) jazz:jazz-updates '())))
  jazz:jazz-updates-cache)


(define (jazz:list->updates lst)
  (map (lambda (arguments)
         (apply jazz:new-update arguments))
       lst))


(define (jazz:for-each-update target updates proc)
  (let iter ((updates updates))
    (if (not (null? updates))
        (let ((update (car updates)))
          (let ((targets (jazz:get-update-targets update)))
            (if (and targets (if (symbol? targets)
                                 (eq? target targets)
                               (memq target targets)))
                (proc update))
            (iter (cdr updates)))))))


(define (jazz:versioned-directory root target updates converter #!key (feedback? #t))
  (define (determine-version)
    (let ((uptodate? #t))
      (continuation-capture
        (lambda (return)
          (jazz:for-each-update target updates
            (lambda (update)
              (let ((version-number (jazz:get-update-version update)))
                (let ((version-dir (version-directory version-number)))
                  (if (file-exists? version-dir)
                      (continuation-return return (values uptodate? version-number))
                    (set! uptodate? #f))))))
          (values uptodate? #f)))))
  
  (define (version-directory version-number)
    (if (not version-number)
        root
      (string-append root (jazz:present-version version-number) "/")))
  
  (jazz:create-directories root)
  (receive (uptodate? current-version-number) (determine-version)
    (if uptodate?
        (version-directory current-version-number)
      (let ((current-dir (version-directory current-version-number))
            (conversion-dir (string-append root "conversion/")))
        (if (file-exists? conversion-dir)
            (begin
              (if feedback?
                  (jazz:feedback "; deleting {a}..." conversion-dir))
              (jazz:delete-directory conversion-dir)
              ;; workaround to yet another windows bug
              (thread-sleep! .1)))
        (if feedback?
            (jazz:feedback "; converting {a}..." target))
        (jazz:copy-directory current-dir conversion-dir feedback: (and feedback? (lambda (src level) (if (<= level 1) (jazz:feedback "; copying {a}..." src)))))
        (let iter ((working-version-number current-version-number))
             (let ((converted-version-number (converter conversion-dir working-version-number)))
               (if converted-version-number
                   (iter converted-version-number)
                 (let ((dir (version-directory working-version-number)))
                   (if feedback?
                       (jazz:feedback "; {a} converted to version {a}" target (jazz:present-version working-version-number)))
                   (rename-file conversion-dir dir)
                   dir))))))))


(define (jazz:versioned-version target updates)
  (continuation-capture
    (lambda (return)
      (jazz:for-each-update target updates
        (lambda (update)
          (continuation-return return (jazz:get-update-version update))))
      #f)))


;;;
;;;; Settings
;;;


(define jazz:jazz-settings-directory
  #f)

(define jazz:jazz-settings-version
  #f)

(define jazz:named-configurations-file
  #f)

(define jazz:local-configurations-file
  "./.configurations")


(define (jazz:setup-settings)
  (let ((settings-root (%%string-append (jazz:home-directory) "/.jazz/")))
    (cond ((not (file-exists? settings-root))
           (let ((version (jazz:versioned-version 'settings (jazz:get-jazz-updates))))
             (if version
                 (begin
                   (set! jazz:jazz-settings-directory (string-append settings-root (jazz:present-version version) "/"))
                   (set! jazz:jazz-settings-version version)
                   (set! jazz:named-configurations-file (string-append jazz:jazz-settings-directory ".configurations"))))))
          (else
           (set! jazz:jazz-settings-directory (jazz:versioned-directory settings-root 'settings (jazz:get-jazz-updates) jazz:convert-settings))
           (set! jazz:jazz-settings-version (jazz:versioned-version 'settings (jazz:get-jazz-updates)))
           (set! jazz:named-configurations-file (string-append jazz:jazz-settings-directory ".configurations"))))))


(define (jazz:convert-settings dir old)
  (define (convert-initial)
    (jazz:convert-configurations dir
      (lambda (configurations)
        (map jazz:convert-configuration-205000 configurations)))
    205000)
  
  (define (convert-205000)
    (jazz:convert-configurations dir
      (lambda (configurations)
        (map jazz:convert-configuration-205001 configurations)))
    205001)
  
  (define (convert-205001)
    205002)
  
  (define (convert-205002)
    205003)
  
  (case old
    ((#f) (convert-initial))
    ((205000) (convert-205000))
    ((205001) (convert-205001))
    ((205002) (convert-205002))
    (else #f)))


(define (jazz:convert-configurations dir converter)
  (let ((configurations-file (string-append dir ".configurations")))
    (define (read-configurations)
      (call-with-input-file (list path: configurations-file eol-encoding: 'cr-lf)
        (lambda (input)
          (read-all input read))))
    
    (define (write-configurations configurations)
      (call-with-output-file (list path: configurations-file #; eol-encoding: #; (jazz:platform-eol-encoding (jazz:guess-platform)))
        (lambda (output)
          (for-each (lambda (configuration)
                      (write configuration output)
                      (newline output))
                    configurations))))
    
    (if (file-exists? configurations-file)
        (write-configurations (converter (read-configurations))))))


(define (jazz:convert-configuration-205000 configuration)
  (jazz:convert-properties configuration
    (lambda (property value)
      (case property
        ((interpret-kernel?:) (list kernel-interpret?: value))
        ((source-access?:) (list))
        (else (list property value))))))


(define (jazz:convert-configuration-205001 configuration)
  (jazz:convert-properties configuration
    (lambda (property value)
      (case property
        ((destination:) (list destination: (if (string? value) (jazz:string-replace value #\: #\/) value)))
        (else (list property value))))))


(define (jazz:convert-properties plist converter)
  (let iter ((scan plist) (result '()))
    (if (null? scan)
        result
      (iter (cddr scan) (append result (converter (car scan) (cadr scan)))))))


(define (jazz:load-global/local filename)
  (let ((global (string-append jazz:jazz-settings-directory filename))
        (local filename))
    (jazz:load-if-exists global)
    (jazz:load-if-exists local)))


;;;
;;;; Configurations
;;;

;; to test cross compiling


(define jazz:named-configurations
  #f)


(define (jazz:get-named-configurations)
  (or jazz:named-configurations
      (let ((configurations
              (if (file-exists? jazz:named-configurations-file)
                  (call-with-input-file (list path: jazz:named-configurations-file eol-encoding: 'cr-lf)
                    (lambda (input)
                      (define (read-configuration input)
                        (let ((list (read input)))
                          (if (eof-object? list)
                              list
                            (apply jazz:new-named-configuration list))))
                
                      (read-all input read-configuration)))
                '())))
        (set! jazz:named-configurations configurations)
        configurations)))


(define (jazz:find-named-configuration name)
  (let iter ((configurations (jazz:get-named-configurations)))
    (if (null? configurations)
        #f
      (let ((configuration (car configurations)))
        (if (eq? (jazz:get-configuration-name configuration) name)
            configuration
          (iter (cdr configurations)))))))


(define (jazz:new-named-configuration
          #!key
          (name (%%unspecified))
          (system (%%unspecified))
          (platform (%%unspecified))
          (compiler (%%unspecified))
          (processor (%%unspecified))
          (windowing (%%unspecified))
          (safety (%%unspecified))
          (optimize? (%%unspecified))
          (debug-environments? (%%unspecified))
          (debug-location? (%%unspecified))
          (debug-source? (%%unspecified))
          (debug-foreign? (%%unspecified))
          (track-memory? (%%unspecified))
          (mutable-bindings? (%%unspecified))
          (kernel-interpret? (%%unspecified))
          (destination (%%unspecified))
          (features (%%unspecified))
          (properties (%%unspecified))
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


;;;
;;;; Configuration
;;;


(define (jazz:save-configuration name system platform compiler processor windowing safety optimize? debug-environments? debug-location? debug-source? debug-foreign? track-memory? mutable-bindings? kernel-interpret? destination features properties file system-platform)
  (call-with-output-file (list path: file eol-encoding: (jazz:platform-eol-encoding system-platform))
    (lambda (output)
      (display "(configuration " output)
      (display (jazz:get-jazz-version-number) output)
      (newline output)
      (newline output)
      (display "  " output)
      (jazz:print-configuration name system platform compiler processor windowing safety optimize? debug-environments? debug-location? debug-source? debug-foreign? track-memory? mutable-bindings? kernel-interpret? destination features properties output)
      (display ")" output)
      (newline output))))


(define (jazz:print-configuration name system platform compiler processor windowing safety optimize? debug-environments? debug-location? debug-source? debug-foreign? track-memory? mutable-bindings? kernel-interpret? destination features properties output)
  (define first?
    #t)
  
  (define (print-property property value)
    (if first?
        (set! first? #f)
      (display " " output))
    (write property output)
    (display " " output)
    (write value output))
  
  (display "(" output)
  (print-property name: name)
  (print-property system: system)
  (print-property platform: platform)
  (print-property compiler: compiler)
  (print-property processor: processor)
  (print-property windowing: windowing)
  (print-property safety: safety)
  (print-property optimize?: optimize?)
  (print-property debug-environments?: debug-environments?)
  (print-property debug-location?: debug-location?)
  (print-property debug-source?: debug-source?)
  (print-property debug-foreign?: debug-foreign?)
  (print-property track-memory?: track-memory?)
  (print-property mutable-bindings?: mutable-bindings?)
  (print-property kernel-interpret?: kernel-interpret?)
  (print-property destination: destination)
  (print-property features: features)
  (print-property properties: properties)
  (display ")" output))


;;;
;;;; Link
;;;


(define (jazz:parse-link link)
  (define link-options
    '(("obj" objects)
      ("objects" objects)
      ("lib" libraries)
      ("libraries" libraries)
      ("obj/lib" objects libraries)
      ("objects/libraries" objects libraries)
      ("static" objects static)
      ("all" objects libraries static)))
  
  (define (invalid option)
    (jazz:error "Invalid link option: {a}" option))
  
  (if (symbol? link)
      (let ((lst (jazz:split-string (symbol->string link) #\/))
            (result '()))
        (for-each (lambda (option)
                    (let ((pair (assoc option link-options)))
                      (if pair
                          (let ((options (cdr pair)))
                            (for-each (lambda (option)
                                        (if (not (member option result))
                                            (set! result (cons option result))))
                                      options))
                        (invalid option))))
                  lst)
        result)
    (invalid link)))


;;;
;;;; Destination
;;;


(define (jazz:destination-directory name destination dir)
  (jazz:dirname-normalize
    (or destination "bin"))))
