;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Kernel Base
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
;;;; Source
;;;


(define jazz.jazz-source
  jazz.source)


;;;
;;;; Version
;;;


(define (jazz.make-version number gambit-version gambit-stamp rebuild recompile update description)
  (vector 'version number gambit-version gambit-stamp rebuild recompile update description))

(define (jazz.version-number version)
  (vector-ref version 1))

(define (jazz.version-gambit-version version)
  (vector-ref version 2))

(define (jazz.version-gambit-stamp version)
  (vector-ref version 3))

(define (jazz.version-rebuild version)
  (vector-ref version 4))

(define (jazz.version-recompile version)
  (vector-ref version 5))

(define (jazz.version-update version)
  (vector-ref version 6))

(define (jazz.version-description version)
  (vector-ref version 7))


(define (jazz.new-version
          #!key
          (version #f)
          (gambit-version #f)
          (gambit-stamp #f)
          (rebuild #f)
          (recompile #f)
          (update #f)
          (description #f))
  (jazz.make-version
    version
    gambit-version
    gambit-stamp
    rebuild
    recompile
    update
    description))


(define (jazz.split-version number)
  (let ((str (number->string number)))
    (let ((len (string-length str)))
      (let ((major (string->number (substring str 0 (- len 5))))
            (minor (string->number (substring str (- len 5) (- len 3))))
            (revision (string->number (substring str (- len 3) len))))
        (values major minor revision)))))


(define (jazz.present-version number)
  (receive (major minor revision) (jazz.split-version number)
    (string-append (number->string major)
                   "."
                   (number->string minor)
                   "."
                   (number->string revision))))


;;;
;;;; Versions
;;;


(define jazz.jazz-versions-file
  #f)

(define jazz.jazz-versions
  #f)

(define jazz.jazz-version-number
  #f)

(define jazz.jazz-gambit-version
  #f)

(define jazz.jazz-gambit-stamp
  #f)


(define jazz.load-jazz-versions
  (let ((loaded? #f))
    (lambda ()
      (define (determine-jazz-versions-file)
        (or jazz.jazz-versions-file
            (and jazz.jazz-source (string-append jazz.jazz-source "kernel/versions"))))
      
      (define (load-versions)
        (let ((file (determine-jazz-versions-file)))
          (if (and file (file-exists? file))
              (call-with-input-file (list path: file eol-encoding: 'cr-lf)
                (lambda (input)
                  (set! jazz.jazz-versions (list->versions (read-all input read)))
                  (set! jazz.jazz-version-number (jazz.version-number (car jazz.jazz-versions))))))))
      
      (define (list->versions lst)
        (map (lambda (arguments)
               (apply jazz.new-version arguments))
             lst))
      
      (define (setup-jazz-gambit-version/stamp)
        (if jazz.jazz-versions
            (let iter ((jazz-versions jazz.jazz-versions))
              (if (not (null? jazz-versions))
                  (let ((jazz-version (car jazz-versions)))
                    (let ((gambit-version (jazz.version-gambit-version jazz-version))
                          (gambit-stamp (jazz.version-gambit-stamp jazz-version)))
                      (if gambit-version
                          (begin
                            (set! jazz.jazz-gambit-version gambit-version)
                            (set! jazz.jazz-gambit-stamp gambit-stamp))
                        (iter (cdr jazz-versions)))))))))
      
      (if (not loaded?)
          (begin
            (load-versions)
            (setup-jazz-gambit-version/stamp)
            (set! loaded? #t))))))


(define (jazz.get-jazz-versions)
  (jazz.load-jazz-versions)
  jazz.jazz-versions)


;; because the kernel/versions file is not available to applications
;; we should probably burn the content or some other format in the image
;; although probably only the kernel/updates file needs to be burned in
(define (jazz.kludged-get-jazz-versions)
  (or (jazz.get-jazz-versions) '()))


(define (jazz.get-jazz-version-number)
  (jazz.load-jazz-versions)
  jazz.jazz-version-number)


(define (jazz.get-jazz-gambit-version)
  (jazz.load-jazz-versions)
  jazz.jazz-gambit-version)


(define (jazz.get-jazz-gambit-stamp)
  (jazz.load-jazz-versions)
  jazz.jazz-gambit-stamp)


(define (jazz.gambit-jazz? gambit-vendor)
  (and gambit-vendor
       (string=? gambit-vendor "Jazz")))

(define (jazz.gambit-uptodate? gambit-version gambit-stamp)
  (let ((jazz-gambit-version (jazz.get-jazz-gambit-version))
        (jazz-gambit-stamp (jazz.get-jazz-gambit-stamp)))
    (or (not jazz-gambit-version)
        (> gambit-version jazz-gambit-version)
        (if jazz-gambit-stamp
            (>= gambit-stamp jazz-gambit-stamp)
          (>= gambit-version jazz-gambit-version)))))


;;;
;;;; Update
;;;


(define (jazz.make-update version targets description)
  (vector 'update version targets description))

(define (jazz.update-version update)
  (vector-ref update 1))

(define (jazz.update-targets update)
  (vector-ref update 2))

(define (jazz.update-description update)
  (vector-ref update 3))


(define (jazz.new-update
          #!key
          (version #f)
          (targets #f)
          (description #f))
  (jazz.make-update
    version
    targets
    description))


;;;
;;;; Updates
;;;


(define jazz.jazz-updates-file
  #f)

(define jazz.jazz-updates
  #f)


(define jazz.load-jazz-updates
  (let ((loaded? #f))
    (lambda ()
      (define (determine-jazz-updates-file)
        (or jazz.jazz-updates-file
            (and jazz.jazz-source (string-append jazz.jazz-source "kernel/updates"))))
      
      (define (load-updates)
        (let ((file (determine-jazz-updates-file)))
          (if (and file (file-exists? file))
              (call-with-input-file (list path: file eol-encoding: 'cr-lf)
                (lambda (input)
                  (set! jazz.jazz-updates (jazz.list->updates (read-all input read))))))))
      
      (if (not loaded?)
          (begin
            (load-updates)
            (set! loaded? #t))))))


(define (jazz.get-jazz-updates)
  (jazz.load-jazz-updates)
  jazz.jazz-updates)


;; because the kernel/updates file is not available to applications
;; we should probably burn the content or some other format in the image
(define (jazz.kludged-get-jazz-updates)
  (or (jazz.get-jazz-updates) '()))


(define (jazz.list->updates lst)
  (map (lambda (arguments)
         (apply jazz.new-update arguments))
       lst))


(define (jazz.for-each-lower-update target updates proc)
  (let iter ((updates updates))
    (if (not (null? updates))
        (let ((update (car updates)))
          (let ((targets (jazz.update-targets update)))
            (if (and targets (if (symbol? targets)
                                 (eq? target targets)
                               (memq target targets)))
                (proc update))
            (iter (cdr updates)))))))


(define (jazz.versioned-directory root target updates converter)
  (define (determine-version)
    (let ((uptodate? #t))
      (continuation-capture
        (lambda (return)
          (jazz.for-each-lower-update target updates
            (lambda (update)
              (let ((version-number (jazz.update-version update)))
                (let ((version-dir (version-directory version-number)))
                  (if (file-exists? version-dir)
                      (continuation-return return (values uptodate? version-number))
                    (set! uptodate? #f))))))
          (values uptodate? #f)))))
  
  (define (version-directory version-number)
    (if (not version-number)
        root
      (string-append root (jazz.present-version version-number) "/")))
  
  (receive (uptodate? current-version-number) (determine-version)
    (if uptodate?
        (version-directory current-version-number)
      (let ((current-dir (version-directory current-version-number))
            (convertion-dir (string-append root "convertion/")))
        (if (file-exists? convertion-dir)
            (begin
              (jazz.feedback "; deleting {a}..." convertion-dir)
              (jazz.delete-directory convertion-dir)
              ;; workaround to yet another windows bug
              (thread-sleep! .1)))
        (jazz.feedback "; converting {a}..." target)
        (jazz.copy-directory current-dir convertion-dir feedback: (lambda (src level) (if (<= level 1) (jazz.feedback "; copying {a}..." src))))
        (let iter ((working-version-number current-version-number))
             (let ((converted-version-number (converter convertion-dir working-version-number)))
               (if converted-version-number
                   (iter converted-version-number)
                 (let ((dir (version-directory working-version-number)))
                   (jazz.feedback "; {a} converted to version {a}" target (jazz.present-version working-version-number))
                   (rename-file convertion-dir dir)
                   dir))))))))


;;;
;;;; Settings
;;;


(define jazz.jazz-settings-directory
  #f)

(define jazz.named-configurations-file
  #f)


(define (jazz.setup-settings)
  (set! jazz.jazz-settings-directory (jazz.versioned-directory "~/.jazz/" 'settings (jazz.kludged-get-jazz-updates) jazz.convert-settings))
  (set! jazz.named-configurations-file (string-append jazz.jazz-settings-directory ".configurations")))


(define (jazz.convert-settings dir old)
  (define (convert-initial)
    (jazz.convert-configurations dir
      (lambda (configurations)
        (map jazz.convert-configuration-205000 configurations)))
    205000)
  
  (case old
    ((#f) (convert-initial))
    (else #f)))


(define (jazz.convert-configurations dir converter)
  (let ((configurations-file (string-append dir ".configurations")))
    (define (read-configurations)
      (call-with-input-file (list path: configurations-file eol-encoding: 'cr-lf)
        (lambda (input)
          (read-all input read))))
    
    (define (write-configurations configurations)
      (call-with-output-file (list path: configurations-file #; eol-encoding: #; (jazz.platform-eol-encoding (jazz.guess-platform)))
        (lambda (output)
          (for-each (lambda (configuration)
                      (write configuration output)
                      (newline output))
                    configurations))))
    
    (if (file-exists? configurations-file)
        (write-configurations (converter (read-configurations))))))


(define (jazz.convert-configuration-205000 configuration)
  (jazz.convert-properties configuration
    (lambda (property value)
      (case property
        ((interpret-kernel?:) (list kernel-interpret?: value))
        ((source-access?:) (list))
        (else (list property value))))))


(define (jazz.convert-properties plist converter)
  (let iter ((scan plist) (result '()))
    (if (null? scan)
        result
      (iter (cddr scan) (append result (converter (car scan) (cadr scan)))))))


(define (jazz.load-global/local-configurations filename)
  (define (load-if-exists file)
    (if (file-exists? file)
        (load file)))
  
  (let ((global (string-append jazz.jazz-settings-directory filename))
        (local filename))
    (load-if-exists global)
    (load-if-exists local)))


;;;
;;;; Configuration
;;;


(define (jazz.save-configuration name system platform windowing safety optimize? debug-environments? debug-location? debug-source? mutable-bindings? kernel-interpret? destination file system-platform)
  (call-with-output-file (list path: file eol-encoding: (jazz.platform-eol-encoding system-platform))
    (lambda (output)
      (display "(configuration " output)
      (display (jazz.get-jazz-version-number) output)
      (newline output)
      (newline output)
      (display "  " output)
      (jazz.print-configuration name system platform windowing safety optimize? debug-environments? debug-location? debug-source? mutable-bindings? kernel-interpret? destination output)
      (display ")" output)
      (newline output))))


(define (jazz.print-configuration name system platform windowing safety optimize? debug-environments? debug-location? debug-source? mutable-bindings? kernel-interpret? destination output)
  (define first?
    #t)
  
  (define (print-property property value)
    (if first?
        (set! first? #f)
      (display " " output))
    (display property output)
    (display " " output)
    (write value output))
  
  (display "(" output)
  (print-property name: name)
  (print-property system: system)
  (print-property platform: platform)
  (print-property windowing: windowing)
  (print-property safety: safety)
  (print-property optimize?: optimize?)
  (print-property debug-environments?: debug-environments?)
  (print-property debug-location?: debug-location?)
  (print-property debug-source?: debug-source?)
  (print-property mutable-bindings?: mutable-bindings?)
  (print-property kernel-interpret?: kernel-interpret?)
  (print-property destination: destination)
  (display ")" output))


;;;
;;;; Link
;;;


(define (jazz.parse-link link)
  (define link-options
    '(("obj" objects)
      ("objects" objects)
      ("lib" libraries)
      ("libraries" libraries)
      ("all" objects libraries)))
  
  (define (invalid option)
    (jazz.error "Invalid link option: {a}" option))
  
  (if (symbol? link)
      (let ((lst (jazz.split-string (symbol->string link) #\/))
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


(define jazz.user-build-directory
  #f)


(define (jazz.get-user-build-directory)
  (define (user-build-directory)
    (let ((dir "~/jazz_user/build/"))
      (jazz.create-directories dir)
      (jazz.pathname-normalize dir)))
  
  (or jazz.user-build-directory
      (let ((dir (user-build-directory)))
        (set! jazz.user-build-directory dir)
        dir)))


(define (jazz.parse-destination dest proc)
  (if (not dest)
      (proc #f #f)
    (let ((pos (jazz.string-find dest #\:))
          (len (string-length dest)))
      (if (not pos)
          (proc #f dest)
        (proc (if (= pos 0)
                  #f
                (string->symbol (substring dest 0 pos)))
              (if (= pos (- len 1))
                  #f
                (substring dest (+ pos 1) len)))))))


(define (jazz.destination-directory name destination dir)
  (jazz.parse-destination (cond (destination destination)
                                (name (jazz.format ":{a}" name))
                                (else "bin:"))
    (lambda (alias dirname)
      (case (or alias 'user)
        ((bin) (string-append dir "bin/"))
        ((build) (string-append dir "build/" dirname "/"))
        ((user) (string-append (jazz.get-user-build-directory) dirname "/"))))))


;;;
;;;; List
;;;


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


(define (jazz.string-replace str old new)
  (declare (proper-tail-calls))
  (let ((cpy (string-copy str)))
    (let iter ((n (- (string-length cpy) 1)))
      (if (>= n 0)
          (begin
            (if (eqv? (string-ref cpy n) old)
                (string-set! cpy n new))
            (iter (- n 1)))))
    cpy))


;;;
;;;; Pathname
;;;


(define jazz.pathname-type
  file-type)

(define jazz.pathname-exists?
  file-exists?)


(define (jazz.pathname-standardize path)
  (jazz.string-replace path #\\ #\/))


(define (jazz.pathname-normalize path #!optional (error? #t))
  (if (not (jazz.pathname-exists? path))
      (if error?
          (jazz.error "No such directory: {s}" path)
        #f)
    (let ((len (string-length path)))
      (let ((dir? (jazz.string-ends-with? path "/")))
        (let ((normalized (path-normalize (if dir? (substring path 0 (- len 1)) path))))
          (let ((standardized (jazz.pathname-standardize normalized)))
            (if (and dir? (not (jazz.string-ends-with? standardized "/")))
                (string-append standardized "/")
              standardized)))))))


(define (jazz.dirname-normalize dir)
  (if (jazz.string-ends-with? dir "/")
      dir
    (string-append dir "/")))


(cond-expand
  (windows
    (define (jazz.path=? path1 path2)
      (string-ci=? path1 path2)))
  (else
   (define (jazz.path=? path1 path2)
     (string=? path1 path2))))


(define (jazz.create-directory dir #!key (feedback #f))
  (if (not (file-exists? dir))
      (begin
        (if feedback
            (feedback "; creating {a}..." dir))
        (create-directory dir))))


(define (jazz.create-directories dir #!key (feedback #f))
  (let ((path (reverse (jazz.split-string dir #\/))))
    (let iter ((scan (if (equal? (car path) "") (cdr path) path)))
      (if (not (null? scan))
          (begin
            (iter (cdr scan))
            (let ((subdir (jazz.join-strings (reverse scan) "/")))
              (if (not (file-exists? subdir))
                  (jazz.create-directory subdir feedback: feedback))))))))


(define (jazz.directory-content directory)
  (directory-files (if (string? directory)
                       (list path: directory ignore-hidden: 'dot-and-dot-dot)
                     directory)))


(define (jazz.directory-files directory)
  (jazz.collect-if (lambda (name)
                     (eq? (jazz.pathname-type (string-append directory name)) 'regular))
                   (jazz.directory-content directory)))


(define (jazz.directory-directories directory)
  (jazz.collect-if (lambda (name)
                     (eq? (jazz.pathname-type (string-append directory name)) 'directory))
                   (jazz.directory-content directory)))


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


(define (jazz.copy-directory src dst #!key (copy? #t) (feedback #f))
  (define (copy src dst level)
    (if feedback
        (feedback src level))
    (let ((src-content (jazz.directory-content src)))
      (jazz.create-directories dst)
      (for-each (lambda (name)
                  (let ((sub-src (string-append src name))
                        (sub-dst (string-append dst name)))
                    (if (or (eq? copy? #t) (copy? name level))
                        (case (jazz.pathname-type sub-src)
                          ((regular)
                           (copy-file sub-src sub-dst))
                          ((directory)
                           (copy (string-append sub-src "/") (string-append sub-dst "/") (+ level 1)))))))
                src-content)))
  
  (copy src dst 0))


(define (jazz.platform-eol-encoding platform)
  (case platform
    ((windows) 'cr-lf)
    (else 'lf)))


;;;
;;;; Command Line
;;;


(define (jazz.option? arg)
  (and (> (string-length arg) 0)
       (char=? (string-ref arg 0) #\-)))


(define (jazz.convert-option arg)
  (let ((len (string-length arg)))
    (let ((start (if (and (>= len 2) (equal? (substring arg 0 2) "--"))
                     2
                   1)))
      (substring arg start len))))


(define (jazz.option=? arg option)
  (and (jazz.option? arg)
       (equal? (jazz.convert-option arg) option)))


(define (jazz.get-option name options)
  (let ((pair (assoc name options)))
    (if pair
        (cdr pair)
      #f)))


(define (jazz.split-command-line arguments options-with-no-args options-with-args missing-argument-for-option cont)
  (let loop ((args arguments)
             (rev-options '()))
    (if (and (pair? args)
             (jazz.option? (car args)))
        (let ((opt (jazz.convert-option (car args)))
              (rest (cdr args)))
          (cond ((member opt options-with-no-args)
                 (loop rest
                       (cons (cons opt #t) rev-options)))
                ((member opt options-with-args)
                 (if (pair? rest)
                     (loop (cdr rest)
                           (cons (cons opt (car rest)) rev-options))
                   (begin
                     (missing-argument-for-option opt)
                     (loop rest rev-options))))
                (else
                 (cont (reverse rev-options) args))))
      (cont (reverse rev-options) args))))


;;;
;;;; Process
;;;


(define (jazz.call-process path arguments #!key (directory #f))
  (let ((port (open-process
                (list
                  path: path
                  arguments: arguments
                  directory: directory
                  stdin-redirection: #f
                  stdout-redirection: #f
                  stderr-redirection: #f))))
    (let ((code (process-status port)))
      (if (not (= code 0))
          (jazz.error "failed")))))


;;;
;;;; Feedback
;;;


(define (jazz.feedback fmt-string . rest)
  (display (apply jazz.format fmt-string rest))
  (newline)
  (force-output))


;;;
;;;; Crash
;;;


(define jazz.crash-reporter
  #f)


(define (jazz.set-crash-reporter proc)
  (set! jazz.crash-reporter proc))
