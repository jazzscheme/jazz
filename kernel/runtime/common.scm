;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Kernel Common
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


(block kernel.common


;;;
;;;; Load
;;;


(define (jazz:load-file pathname . rest)
  (let ((quiet? (if (%%null? rest) #f (%%car rest))))
    (%%load pathname (lambda rest #f) #f #t #f quiet?))
  (void))


(define (jazz:load-binary pathname . rest)
  ;; because we statically link the same .o into the .o1 and .l1
  ;; we need to hack the module-descrs to remove the name conflict
  ;; it might be possible to only register the most recent one..
  (define (hack-module-descrs module-descrs)
    (let loop ((i (%%fx- (%%vector-length module-descrs) 1)))
         (if (%%fx>= i 0)
             (let ((module-descr (%%vector-ref module-descrs i)))
               (let ((name (%%symbol->string (%%vector-ref module-descr 0))))
                 (if (jazz:string-starts-with? name jazz:bin-uniqueness-prefix)
                     (##vector-set! module-descr 0 (%%string->symbol (%%string-append jazz:lib-uniqueness-prefix (%%substring name (%%string-length jazz:bin-uniqueness-prefix) (%%string-length name)))))))
               (loop (%%fx- i 1))))))
  
  (let ((quiet? (if (%%null? rest) #f (%%car rest))))
    (let ((result (jazz:load-object-file pathname quiet?)))
      (if (%%not (%%vector? result))
          (jazz:error "Unable to load {a}" pathname)
        (let ((module-descrs (jazz:object-file-module-descrs result 0)))
          (if (%%not (%%vector? module-descrs))
              (jazz:error "Unable to load {a}: {a}" pathname module-descrs)
            (begin
              (hack-module-descrs module-descrs)
              (jazz:register-module-descrs-and-load! module-descrs)))))))
  (void))


;;;
;;;; Configure
;;;


(define jazz:enable-track-scheme?
  (jazz:string-contains? (configure-command-string) "--enable-track-scheme"))

(define jazz:enable-debug-garbage-collect?
  (jazz:string-contains? (configure-command-string) "--enable-debug-garbage-collect"))


;;;
;;;; Configuration
;;;


(jazz:define-variable jazz:debugger
  #f)

(jazz:define-variable jazz:compile-options
  `(,@(if jazz:kernel-debug-environments? '(debug-environments) '())
    ,@(if jazz:kernel-debug-location? '(debug-location) '())
    ,@(if jazz:kernel-debug-source? '(debug-source) '())
    ,@(if jazz:enable-track-scheme? '(track-scheme) '())))

(jazz:define-variable jazz:link
  #f)

(jazz:define-variable jazz:link-options
  '(objects))

(jazz:define-variable jazz:jobs
  #f)

(jazz:define-variable jazz:warnings
  #f)

(define jazz:recompile-references
  #f)

(define (jazz:link-objects?)
  (%%memq 'objects jazz:link-options))

(define (jazz:link-libraries?)
  (%%memq 'libraries jazz:link-options))

(define (jazz:link-static?)
  (%%memq 'static jazz:link-options))

(define jazz:jazzini
  ".jazzini")

(define jazz:buildini
  ".buildini")


(define (jazz:load-if-exists file)
  (if (file-exists? file)
      (jazz:load-file file)))


(define (jazz:load-configuration-files filename install?)
  (let ((global (and jazz:jazz-settings-directory (%%string-append jazz:jazz-settings-directory filename)))
        (local filename))
    (if install?
        (jazz:load-if-exists (jazz:install-path filename)))
    (if global
        (jazz:load-if-exists global))
    (jazz:load-if-exists local)))


(define (jazz:process-jazzini install?)
  (jazz:load-configuration-files jazz:jazzini install?))

(define (jazz:process-buildini install?)
  (jazz:load-configuration-files jazz:buildini install?))


(define (jazz:process-settings)
  (if jazz:kernel-source-access?
      (begin
        (jazz:setup-settings)
        (jazz:process-jazzini #t))
    (if jazz:kernel-jazzini-access?
        (jazz:load-if-exists (jazz:install-path jazz:jazzini)))))


;;;
;;;; Build
;;;


(jazz:define-variable jazz:manifest-needs-rebuild?
  (lambda rest
    #f))

(jazz:define-variable jazz:build-repository-needs-sweep
  (lambda rest
    #f))

(jazz:define-variable jazz:get-changed-units #f)
(jazz:define-variable jazz:push-changed-units #f)
(jazz:define-variable jazz:reset-changed-units #f)
(jazz:define-variable jazz:build-kernel #f)
(jazz:define-variable jazz:build-image #f)
(jazz:define-variable jazz:build-library #f)


;;;
;;;; Custom build
;;;


(define (jazz:custom-compile/build unit-specs #!key (unit #f) (pre-build #f) (force? #f))
  (jazz:load-build)
  (if unit
      (let ((compile-args (assq unit unit-specs)))
        (if compile-args
            (apply jazz:compile-unit `(,@compile-args force?: ,force?))
          (jazz:compile-unit unit force?: force?)))
    (begin
      (if pre-build
          (pre-build))
      (for-each (lambda (compile-args)
                  (apply jazz:compile-unit `(,@compile-args force?: ,force?)))
                unit-specs))))


;;;
;;;; Feedback
;;;


(define jazz:build-feedback
  jazz:feedback)


;;;
;;;; List
;;;


(define (jazz:listify obj)
  (if (or (%%null? obj) (%%pair? obj))
      obj
    (%%list obj)))


(define (jazz:getprop plist target)
  (let iter ((scan plist))
    (cond ((%%null? scan)
           #f)
          ((%%eqv? (%%car scan) target)
           scan)
          (else
           (iter (%%cddr scan))))))


(define (jazz:getf plist target #!optional (not-found #f))
  (let ((pair (jazz:getprop plist target)))
    (if pair
        (%%cadr pair)
      not-found)))


(define (jazz:collect-if predicate lst)
  (declare (proper-tail-calls))
  (let iter ((scan lst))
    (if (%%not (%%null? scan))
        (let ((value (%%car scan)))
          (if (predicate value)
              (%%cons value (iter (%%cdr scan)))
            (iter (%%cdr scan))))
      '())))


(define (jazz:collect test lst)
  (declare (proper-tail-calls))
  (let iter ((scan lst))
    (if (%%not (%%null? scan))
        (let ((value (test (%%car scan))))
          (if value
              (%%cons value (iter (%%cdr scan)))
            (iter (%%cdr scan))))
      '())))


(define (jazz:remove item lst #!key (test #f))
  (declare (proper-tail-calls))
  (let iter ((scan lst))
    (if (%%not (%%null? scan))
        (let ((value (%%car scan)))
          (if (if test
                  (test value item)
                (%%eq? value item))
              (iter (%%cdr scan))
            (%%cons value (iter (%%cdr scan)))))
      '())))


(define jazz:kernel-collect-if jazz:collect-if)


;;;
;;;; String
;;;


(define (jazz:string-find-reversed str c)
  (declare (proper-tail-calls))
  (let iter ((n (%%fx- (%%string-length str) 1)))
    (cond ((%%fx< n 0)
           #f)
          ((%%char=? (%%string-ref str n) c)
           n)
          (else
           (iter (%%fx- n 1))))))


(define (jazz:string-starts-with-ci? str target)
  (let ((sl (%%string-length str))
        (tl (%%string-length target)))
    (and (%%fx>= sl tl)
         (%%string-ci=? (%%substring str 0 tl) target))))


(define (jazz:string-ends-with? str target)
  (let ((sl (%%string-length str))
        (tl (%%string-length target)))
    (and (%%fx>= sl tl)
         (%%string=? (%%substring str (%%fx- sl tl) sl) target))))


(define (jazz:string-numeric? str)
  (let iter ((n (%%fx- (%%string-length str) 1)))
       (if (%%fx>= n 0)
           (let ((c (%%string-ref str n)))
             (if (%%memv c '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
                 (iter (%%fx- n 1))
               #f))
         #t)))


(define (jazz:split-string str separator)
  (declare (proper-tail-calls))
  (let ((lst '())
        (end (%%string-length str)))
    (let iter ((pos (%%fx- end 1)))
      (if (%%fx>= pos 0)
          (begin
            (if (%%eqv? (%%string-ref str pos) separator)
                (begin
                  (set! lst (%%cons (%%substring str (%%fx+ pos 1) end) lst))
                  (set! end pos)))
            (iter (%%fx- pos 1))))
        (%%cons (%%substring str 0 end) lst))))


(define (jazz:join-strings strings separator)
  (let ((output (open-output-string)))
    (if (%%pair? strings)
        (begin
          (display (%%car strings) output)
          (for-each (lambda (string)
                      (display separator output)
                      (display string output))
                    (%%cdr strings))))
    (get-output-string output)))


;;;
;;;; Symbol
;;;


(define (jazz:compose-identifier . rest)
  (%%string->symbol (jazz:join-strings (map symbol->string rest) ".")))


(define (jazz:compose-reference . rest)
  (%%string->symbol (jazz:join-strings (map symbol->string rest) ":")))


;;;
;;;; Table
;;;


(define (jazz:iterate-table-unsafe table proc)
  (%%table-for-each proc table))


(define (jazz:search-table-unsafe table proc)
  (table-search proc table))


;; safe versions of table-for-each and table-search


(define (jazz:iterate-table table proc)
  (for-each (lambda (pair)
              (proc (%%car pair) (%%cdr pair)))
            (%%table->list table)))


(define (jazz:search-table table proc)
  (declare (proper-tail-calls))
  (let loop ((scan (%%table->list table)))
    (if (%%null? scan)
        #f
      (let ((pair (%%car scan)))
        (let ((result (proc (%%car pair) (%%cdr pair))))
          (or result (loop (%%cdr scan))))))))


;;;
;;;; Pathname
;;;


(jazz:define-variable jazz:executable-path
  #f)

(jazz:define-variable jazz:executable-directory
  #f)


(define (jazz:pathname-name pathname)
  (let ((pos (jazz:string-find-reversed pathname #\/))
        (len (%%string-length pathname)))
    (cond ((%%not pos)
           pathname)
          ((%%fx= pos (%%fx- len 1))
           (jazz:pathname-name (%%substring pathname 0 pos)))
          (else
           (%%substring pathname (%%fx+ pos 1) len)))))


(define (jazz:pathname-base pathname)
  (let ((name (jazz:pathname-name pathname)))
    (let ((pos (jazz:string-find-reversed name #\.)))
      (if pos
          (%%substring name 0 pos)
        name))))


(define (jazz:pathname-spine pathname)
  (let ((pos (jazz:string-find-reversed pathname #\.)))
    (if pos
        (%%substring pathname 0 pos)
      pathname)))


(define (jazz:pathname-extension pathname)
  (let ((name (jazz:pathname-name pathname)))
    (let ((pos (jazz:string-find-reversed name #\.)))
      (if pos
          (%%substring name (%%fx+ pos 1) (%%string-length name))
        #f))))


(define (jazz:pathname-brother pathname name)
  (let ((pos (jazz:string-find-reversed pathname #\/))
        (len (%%string-length pathname)))
    (cond ((%%not pos)
           name)
          (else
           (%%string-append (%%substring pathname 0 (%%fx+ pos 1)) name)))))


(define (jazz:extension? extension target)
  (or (and (%%not extension) (%%not target))
      (and extension
           target
           (%%string=? extension target))))


(define (jazz:numeric-extension? extension prefix)
  (and extension
       (jazz:string-starts-with? extension prefix)
       (jazz:string-numeric? (%%substring extension (%%string-length prefix) (%%string-length extension)))))


(define (jazz:executable-extension platform)
  (case platform
    ((windows)
     "exe")
    (else
     #f)))


(define (jazz:pathname-dir pathname)
  (let ((pos (jazz:string-find-reversed pathname #\/)))
    (if (%%not pos)
        #f
      (%%substring pathname 0 (%%fx+ pos 1)))))


(define (jazz:file-last-access-seconds pathname)
  (time->seconds (file-last-access-time pathname)))

(define (jazz:file-last-modification-seconds pathname)
  (time->seconds (file-last-modification-time pathname)))


(define (jazz:add-extension filename extension)
  (if (%%not extension)
      filename
    (%%string-append filename "." extension)))


(cond-expand
  (windows)
  (else
   ;; redefined in compiled code
   (define jazz:file-permissions
     #f)
   
   (define jazz:file-permissions-set!
     #f)
   
   (set! jazz:file-permissions
     (lambda (filename)
       (error "Called too early")))
   
   (set! jazz:file-permissions-set!
     (lambda (filename permissions)
       (error "Called too early")))))


(define (jazz:copy-file src dst)
  (cond-expand
    (windows
     (copy-file src dst))
    (else
     (let ((permissions (jazz:file-permissions src)))
       (copy-file src dst)
       (jazz:file-permissions-set! dst permissions)))))


(define (jazz:copy-file-if-needed src dst #!key (feedback #f))
  (if (jazz:file-needs-update? src dst)
      (begin
        (if feedback
            (feedback "; copying {a}..." src))
        (if (file-exists? dst)
            (delete-file dst))
        (jazz:copy-file src dst))))


(define (jazz:copy-files-if-needed src dst #!key (feedback #f))
  (if (not (file-exists? dst))
      (begin
        (if feedback
            (feedback "; copying {a}..." src))
        (create-directory dst)))
  (for-each (lambda (file)
              (let ((src-pathname (string-append src "/" file))
                    (dst-pathname (string-append dst "/" file)))
                (case (jazz:pathname-type src-pathname)
                  ((file)
                   (jazz:copy-file-if-needed src-pathname dst-pathname feedback: #f)))))
            (directory-files src)))


(define (jazz:copy&sign-if-needed src dst #!key (feedback #f))
  (if (jazz:file-needs-update? src dst)
      (begin
        (cond-expand
          (mac
           (let ((apple-id (getenv "JAZZ_APPLE_DEVELOPER_ID" #f))
                 (entitlements (getenv "JAZZ_APPLE_ENTITLEMENTS" #f)))
             (if apple-id
                 (begin
                   (if feedback
                       (feedback "; copying & signing {a}..." src))
                   (if (file-exists? dst)
                       (delete-file dst))
                   (jazz:copy-file src dst)
                   (jazz:codesign #t #f apple-id dst))
               (jazz:copy-file-if-needed src dst feedback: feedback))))
          (else
           (jazz:copy-file-if-needed src dst feedback: feedback))))))


(define (jazz:codesign-required?)
  (cond-expand
    (mac (getenv "JAZZ_APPLE_DEVELOPER_ID" #f))
    (else #f)))


(define (jazz:codesign-if bin)
  (cond-expand
    (mac
     (let ((apple-id (getenv "JAZZ_APPLE_DEVELOPER_ID" #f))
           (entitlements (getenv "JAZZ_APPLE_ENTITLEMENTS" #f)))
       (if apple-id
           (jazz:codesign #t #f apple-id bin))))
    (else
     #f)))


(define (jazz:codesign mac? ios? apple-id bin #!key (entitlements #f))
  (cond (mac?
         (let ((entitlements-arguments (if (not entitlements)
                                           '()
                                         (list "--entitlements" entitlements))))
           (jazz:call-process
             (list
               path: "/usr/bin/codesign"
               arguments: `("--options" "runtime" ,@entitlements-arguments "--timestamp" "--sign" ,apple-id ,bin)))))
        (ios?
         (jazz:call-process
           (list
             path: "/usr/bin/codesign"
             arguments: `("--force" "--sign" ,apple-id "--preserve-metadata=identifier,entitlements" "--timestamp=none" ,bin))))))


(define (jazz:file-needs-update? src dst)
  (or (%%not (file-exists? dst))
      (> (jazz:file-last-modification-seconds src)
         (jazz:file-last-modification-seconds dst))))


(define (jazz:maybe-relativise-directory basedir rootdir targdir)
  (let ((targdir (jazz:pathname-normalize targdir))
        (basedir (jazz:pathname-normalize basedir))
        (rootdir (jazz:pathname-normalize rootdir)))
    (let ((targlen (%%string-length targdir))
          (baselen (%%string-length basedir))
          (rootlen (%%string-length rootdir)))
      (if (and (%%fx>= baselen rootlen)
               (%%string=? (%%substring basedir 0 rootlen) rootdir)
               (%%fx>= targlen rootlen)
               (%%string=? (%%substring targdir 0 rootlen) rootdir))
          (let ((suffix (%%substring basedir rootlen baselen))
                (relative-dir #f))
            (let iter ((n (%%fx- (%%string-length suffix) 1)))
              (if (%%fx>= n 0)
                  (begin
                    (if (%%eqv? (%%string-ref suffix n) #\/)
                        (set! relative-dir (%%string-append (or relative-dir "") "../")))
                    (iter (%%fx- n 1)))))
            (values (%%string-append (or relative-dir "./") (%%substring targdir rootlen targlen)) #t))
        (values targdir #f)))))


(define (jazz:relativise-directory basedir rootdir targdir)
  (receive (directory relative?) (jazz:maybe-relativise-directory basedir rootdir targdir)
    directory))


(define (jazz:absolutize-directory basedir reldir)
  (if (or (jazz:string-starts-with? reldir "./")
          (jazz:string-starts-with? reldir "../"))
      (jazz:pathname-normalize (%%string-append basedir reldir) #f)
    (jazz:pathname-normalize reldir #f)))


(define (jazz:parent-directory dir)
  (path-directory (path-strip-trailing-directory-separator dir)))


(define (jazz:nth-parent-directory dir n)
  (let iter ((n n)
             (dir dir))
       (if (%%fx= n 0)
           dir
         (iter (%%fx- n 1) (jazz:parent-directory dir)))))


(define (jazz:quote-pathname pathname #!optional (platform jazz:kernel-platform))
  (case platform
    ((windows)
     (string-append "\"" pathname "\""))
    (else
     ;; quoting is only necessary on windows as arguments are passed explicitly in linux
     pathname)))


(define (jazz:quote-jazz-pathname suffix)
  (jazz:quote-pathname (jazz:jazz-pathname suffix)))


(define (jazz:jazz-pathname suffix)
  (path-expand (string-append jazz:kernel-source suffix)))


;; patch around ld: warning: text-based stub file x.tbd and library file x are out of sync. Falling back to library file for linking.
(define (jazz:patch-mac-ld-warnings cc-flags)
  (let ((sysroot "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk"))
    (if (file-exists? sysroot)
        (string-append "-isysroot " sysroot " " cc-flags)
      cc-flags)))


(define (jazz:build-dynamic-path destination-root source-path)
  (let ((root (path-strip-trailing-directory-separator destination-root))
        (branch (path-strip-directory (path-strip-trailing-directory-separator source-path)))
        (repo (path-strip-directory (path-strip-trailing-directory-separator (jazz:parent-directory source-path)))))
    (string-append root "/" repo "/" branch "/")))


;;;
;;;; Restricted
;;;


(define-macro (jazz:check-restricted name)
  (let ((unrestricted (%%string->symbol (%%string-append (%%symbol->string name) "-unrestricted"))))
    `(let ((,unrestricted ,name))
       (jazz:filesystem-restrict)
       (let ((eq-restricted (if (eq? ,unrestricted ,name) '*** '---)))
         (jazz:filesystem-unrestrict)
         (let ((eq-unrestricted (if (eq? ,unrestricted ,name) '--- '$$$)))
           (pp (list eq-restricted eq-unrestricted ',name)))))))


(define (jazz:validate-restricted)
  (jazz:check-restricted create-directory)
  (jazz:check-restricted create-fifo)
  (jazz:check-restricted create-link)
  (jazz:check-restricted create-symbolic-link)
  (jazz:check-restricted rename-file)
  (jazz:check-restricted copy-file)
  (jazz:check-restricted delete-file)
  (jazz:check-restricted delete-directory)
  (jazz:check-restricted directory-files)
  (jazz:check-restricted file-exists?)
  (jazz:check-restricted file-info)
  (jazz:check-restricted file-type)
  (jazz:check-restricted file-device)
  (jazz:check-restricted file-inode)
  (jazz:check-restricted file-mode)
  (jazz:check-restricted file-number-of-links)
  (jazz:check-restricted file-owner)
  (jazz:check-restricted file-group)
  (jazz:check-restricted file-size)
  (jazz:check-restricted file-last-access-time)
  (jazz:check-restricted file-last-modification-time)
  (jazz:check-restricted file-last-change-time)
  (jazz:check-restricted file-attributes)
  (jazz:check-restricted file-creation-time)
  (jazz:check-restricted file-last-access-and-modification-times-set!)
  (jazz:check-restricted open-file)
  (jazz:check-restricted open-input-file)
  (jazz:check-restricted open-output-file)
  (jazz:check-restricted call-with-input-file)
  (jazz:check-restricted call-with-output-file)
  (jazz:check-restricted with-input-from-file)
  (jazz:check-restricted with-output-to-file)
  (jazz:check-restricted open-directory))


;;;
;;;; Digest
;;;


(define (jazz:updated-digest-source? digest src-filepath)
  (let ((seconds (jazz:file-last-modification-seconds src-filepath)))
    (if (= seconds (%%get-digest-seconds digest))
        #f
      (begin
        (%%set-digest-seconds digest seconds)
        (%%set-digest-hash digest (digest-file src-filepath 'SHA-1))
        #t))))


;;;
;;;; Manifest
;;;


(define jazz:Manifest-Extension
  "mnf")

(define jazz:Digest-Extension
  "dgs")


(define (jazz:load-source-digests digest-filepath)
  (if (file-exists? digest-filepath)
      (call-with-input-file (%%list path: digest-filepath eol-encoding: 'cr-lf)
        (lambda (input)
          (let ((digest-forms (read input)))
            (map (lambda (form)
                   (let ((pathname (%%car (%%cdr form)))
                         (source-hash (%%cadr (%%cdr form)))
                         (source-time (%%car (%%cddr (%%cdr form)))))
                     (%%make-digest pathname source-hash source-time)))
                 (%%cdr digest-forms)))))
    '()))


(define (jazz:load-manifest digest-filepath manifest-filepath)
  (if (file-exists? manifest-filepath)
      (call-with-input-file (%%list path: manifest-filepath eol-encoding: 'cr-lf)
        (lambda (input)
          (let ((form (read input)))
            (let ((name (%%cadr form))
                  (version-form (%%assq 'version (%%cddr form)))
                  (digest-form (%%assq 'digest (%%cddr form)))
                  (references-form (%%assq 'references (%%cddr form))))
              (let (;; test is for backward compatibility and could be removed in the future
                    (version (if version-form (%%cadr version-form) #f))
                    (compile-time-hash (%%cadr digest-form))
                    (source-digests (jazz:load-source-digests digest-filepath))
                    (references (if references-form (%%cdr references-form) #f)))
                (%%make-manifest name version compile-time-hash source-digests references))))))
    #f))


(define (jazz:save-manifest filepath manifest)
  (let ((name (%%get-manifest-name manifest))
        (version (%%get-manifest-version manifest))
        (references (%%get-manifest-references manifest)))
    (jazz:create-directories (jazz:pathname-dir filepath))
    (call-with-output-file (list path: filepath eol-encoding: (jazz:platform-eol-encoding jazz:kernel-platform))
      (lambda (output)
        (display "(manifest " output)
        (display name output)
        (newline output)
        (newline output)
        (display "  (version " output)
        (write version output)
        (display ")" output)
        (newline output)
        (display "  (digest " output)
        (write (%%get-manifest-compile-time-hash manifest) output)
        (if references
            (begin
              (display ")" output)
              (newline output)
              (display "  (references" output)
              (if (%%pair? references)
                  (begin
                    (display " " output)
                    (write (%%car references) output)
                    (for-each (lambda (module-references)
                                (newline output)
                                (display "              " output)
                                (write module-references output))
                              (%%cdr references))))))
        (display "))" output)
        (newline output)))))


(define (jazz:filter proc lst)
  (if (%%null? lst)
      '()
    (let ((head (%%car lst)))
      (if (proc head)
          (%%cons head (jazz:filter proc (%%cdr lst)))
        (jazz:filter proc (%%cdr lst))))))


(define (jazz:save-digest filepath manifest)
  (define (save-digest digests)
    (jazz:create-directories (jazz:pathname-dir filepath))
    (call-with-output-file (list path: filepath eol-encoding: (jazz:platform-eol-encoding jazz:kernel-platform))
      (lambda (output)
        (display "(digest" output)
        (newline output)
        (for-each (lambda (digest)
                    (newline output)
                    (display "  (file " output)
                    (write (jazz:pathname-normalize (%%get-digest-pathname digest)) output)
                    (display " " output)
                    (write (%%get-digest-hash digest) output)
                    (display " " output)
                    (write (%%get-digest-seconds digest) output)
                    (display ")" output))
                  digests)
        (display ")" output)
        (newline output))))
  
  (let ((existing-files-digests (jazz:filter (lambda (digest)
                                               (file-exists? (%%get-digest-pathname digest)))
                                             (%%get-manifest-source-digests manifest))))
    ;; not deleting the digest when there is no existing files makes
    ;; it more robust when multiple processes access the same digest
    (save-digest existing-files-digests)))


(define (jazz:find-source-digest src-pathname manifest)
  (define (find-digest)
    (let ((pathname (jazz:pathname-normalize src-pathname)))
      (let iter ((digests (%%get-manifest-source-digests manifest)))
           (if (%%pair? digests)
               (let ((digest (%%car digests)))
                 (if (jazz:path=? pathname (%%get-digest-pathname digest))
                     digest
                   (iter (%%cdr digests))))
             #f))))
  
  (define (new-digest)
    (let ((digest (%%make-digest (jazz:pathname-normalize src-pathname) "" 0)))
      (%%set-manifest-source-digests manifest (%%cons digest (%%get-manifest-source-digests manifest)))
      digest))
  
  (or (find-digest) (new-digest)))


(define (jazz:manifest-uptodate? src-pathname manifest)
  (let ((digest (jazz:find-source-digest (jazz:pathname-normalize src-pathname) manifest)))
    (and (%%string=? (%%get-digest-hash digest) (%%get-manifest-compile-time-hash manifest))
         digest)))


(define (jazz:load/create-manifest name digest-filepath manifest-filepath)
  (or (jazz:load-manifest digest-filepath manifest-filepath)
      (%%make-manifest name jazz:kernel-version "" '() #f)))


(define (jazz:load-updated-manifest name digest-filepath manifest-filepath src-filepath)
  (let ((manifest (jazz:load/create-manifest name digest-filepath manifest-filepath)))
    (let ((digest (and src-filepath (jazz:find-source-digest src-filepath manifest))))
      (if (and digest
               (jazz:updated-digest-source? digest src-filepath))
          (jazz:save-digest digest-filepath manifest))
      manifest)))


(define (jazz:update-manifest-compile-time name digest-filepath manifest-filepath src-filepath updated-references)
  (let ((manifest (jazz:load/create-manifest name digest-filepath manifest-filepath)))
    (%%set-manifest-version manifest jazz:kernel-version)
    (let ((digest (jazz:find-source-digest src-filepath manifest)))
      (jazz:updated-digest-source? digest src-filepath)
      (if updated-references
          (%%set-manifest-references manifest updated-references))
      (%%set-manifest-compile-time-hash manifest (%%get-digest-hash digest))
      (jazz:save-manifest manifest-filepath manifest))))


;;;
;;;; Library
;;;


(define jazz:bin-uniqueness-prefix "bin:")
(define jazz:lib-uniqueness-prefix "lib:")
(define jazz:product-uniqueness-prefix "product:")


(define jazz:Library-Extension "l")
(define jazz:Library-Manifest-Extension "lmf"))
