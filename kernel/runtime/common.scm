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


(block kernel.common


(jazz.kernel-declares)


;;;
;;;; Load
;;;


(cond-expand
  (gambit
    (define (jazz.load pathname . rest)
      (let ((quiet? (if (%%null? rest) #f (%%car rest))))
        (%%load pathname (lambda rest #f) #f #t quiet?))
      (void)))
  
  (else
    (define (jazz.load pathname . rest)
      (load pathname))))


;;;
;;;; Configuration
;;;


(jazz.define-variable jazz.debugger
  #f)

(jazz.define-variable jazz.compile-options
  `(,@(if jazz.kernel-debug-environments? '(debug-environments) '())
    ,@(if jazz.kernel-debug-location? '(debug-location) '())
    ,@(if jazz.kernel-debug-source? '(debug-source) '())))

(jazz.define-variable jazz.link
  #f)

(jazz.define-variable jazz.link-options
  #f)

(jazz.define-variable jazz.jobs
  #f)

(jazz.define-variable jazz.warnings
  #f)

(define (jazz.link-objects?)
  (%%memq 'objects jazz.link-options))

(define (jazz.link-libraries?)
  (%%memq 'libraries jazz.link-options))

(define jazz.jazzini
  ".jazzini")

(define jazz.buildini
  ".buildini")


(define (jazz.load-configuration-files filename install?)
  (define (load-if-exists file)
    (if (file-exists? file)
        (jazz.load file)))
  
  (let ((global (%%string-append jazz.jazz-settings-directory filename))
        (local filename))
    (if install?
        (load-if-exists (jazz.install-path filename)))
    (load-if-exists global)
    (load-if-exists local)))


(define (jazz.process-jazzini install?)
  (jazz.load-configuration-files jazz.jazzini install?))

(define (jazz.process-buildini install?)
  (jazz.load-configuration-files jazz.buildini install?))


;;;
;;;; Build
;;;


(jazz.define-variable jazz.manifest-needs-rebuild?
  (lambda rest
    #f))

(jazz.define-variable jazz.get-changed-units #f)
(jazz.define-variable jazz.push-changed-units #f)
(jazz.define-variable jazz.reset-changed-units #f)
(jazz.define-variable jazz.build-image #f)
(jazz.define-variable jazz.build-library #f)


;;;
;;;; Custom build
;;;


(define (jazz.custom-compile/build unit-specs #!key (unit #f) (pre-build #f) (force? #f))
  (jazz.load-unit 'core.unit.builder)
  (if unit
      (let ((compile-args (assq unit unit-specs)))
        (if compile-args
            (apply jazz.compile-unit `(,@compile-args force?: ,force?))
          (jazz.error "Custom compile failed")))
    (begin
      (if pre-build
          (pre-build))
      (for-each (lambda (compile-args)
                  (apply jazz.compile-unit `(,@compile-args force?: ,force?)))
                unit-specs))))


;;;
;;;; Feedback
;;;


(define jazz.build-feedback
  jazz.feedback)


;;;
;;;; List
;;;


(define (jazz.listify obj)
  (if (or (%%null? obj) (%%pair? obj))
      obj
    (%%list obj)))


(define (jazz.collect-if predicate lst)
  (declare (proper-tail-calls))
  (let iter ((scan lst))
    (if (%%not (%%null? scan))
        (let ((value (%%car scan)))
          (if (predicate value)
              (%%cons value (iter (%%cdr scan)))
            (iter (%%cdr scan))))
      '())))


(define (jazz.collect test lst)
  (declare (proper-tail-calls))
  (let iter ((scan lst))
    (if (%%not (%%null? scan))
        (let ((value (test (%%car scan))))
          (if value
              (%%cons value (iter (%%cdr scan)))
            (iter (%%cdr scan))))
      '())))


(define (jazz.remove item lst)
  (declare (proper-tail-calls))
  (let iter ((scan lst))
    (if (%%not (%%null? scan))
        (let ((value (%%car scan)))
          (if (%%eq? value item)
              (iter (%%cdr scan))
            (%%cons value (iter (%%cdr scan)))))
      '())))


;;;
;;;; String
;;;


(define (jazz.string-find str c #!optional (start 0))
  (declare (proper-tail-calls))
  (let ((len (%%string-length str)))
    (let iter ((n start))
      (cond ((%%fx>= n len)
             #f)
            ((%%char=? (%%string-ref str n) c)
             n)
            (else
             (iter (%%fx+ n 1)))))))


(define (jazz.string-find-reversed str c)
  (declare (proper-tail-calls))
  (let iter ((n (%%fx- (%%string-length str) 1)))
    (cond ((%%fx< n 0)
           #f)
          ((%%char=? (%%string-ref str n) c)
           n)
          (else
           (iter (%%fx- n 1))))))


(define (jazz.string-starts-with? str target)
  (let ((sl (%%string-length str))
        (tl (%%string-length target)))
    (and (%%fx>= sl tl)
         (%%string=? (%%substring str 0 tl) target))))


(define (jazz.string-starts-with-ci? str target)
  (let ((sl (%%string-length str))
        (tl (%%string-length target)))
    (and (%%fx>= sl tl)
         (%%string-ci=? (%%substring str 0 tl) target))))


(define (jazz.string-ends-with? str target)
  (let ((sl (%%string-length str))
        (tl (%%string-length target)))
    (and (%%fx>= sl tl)
         (%%string=? (%%substring str (%%fx- sl tl) sl) target))))


(define (jazz.string-numeric? str)
  (let iter ((n (%%fx- (%%string-length str) 1)))
       (if (%%fx>= n 0)
           (let ((c (%%string-ref str n)))
             (if (%%memv c '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
                 (iter (%%fx- n 1))
               #f))
         #t)))


(define (jazz.split-string str separator)
  (declare (proper-tail-calls))
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
;;;; Symbol
;;;


(cond-expand
  (chicken
    (require 'lolevel)

    (define (jazz.global-bound? symbol)
      (global-bound? symbol))
    
    (define (jazz.global-ref symbol)
      (global-ref symbol)))
  
  (gambit
    (define (jazz.global-bound? symbol)
      (and (%%global-var? symbol)
           (%%not (%%unbound? (%%global-var-ref symbol)))))
    
    (define (jazz.global-ref symbol)
      (%%global-var-ref symbol))
    
    (define (jazz.global-set! symbol value)
      (%%global-var-set! symbol value)))
  
  (else))


(define (jazz.break-identifier identifier)
  (let ((str (%%symbol->string identifier)))
    (let ((n (jazz.string-find-reversed str #\.)))
      (if (%%not n)
          (values #f identifier)
        (values (%%string->symbol (%%substring str 0 n))
                (%%string->symbol (%%substring str (%%fx+ n 1) (%%string-length str))))))))


(define (jazz.break-reference identifier)
  (let ((str (%%symbol->string identifier)))
    (let ((n (jazz.string-find-reversed str #\:)))
      (if (%%not n)
          (values #f identifier)
        (values (%%string->symbol (%%substring str 0 n))
                (%%string->symbol (%%substring str (%%fx+ n 1) (%%string-length str))))))))


(define (jazz.compose-identifier . rest)
  (%%string->symbol (jazz.join-strings (map symbol->string rest) ".")))


(define (jazz.compose-reference . rest)
  (%%string->symbol (jazz.join-strings (map symbol->string rest) ":")))


;;;
;;;; Table
;;;


(define (jazz.iterate-table table proc)
  (%%table-for-each proc table))


(define (jazz.iterate-table-safe table proc)
  (for-each (lambda (pair)
              (proc (%%car pair) (%%cdr pair)))
            (%%table->list table)))


;;;
;;;; Pathname
;;;


(jazz.define-variable jazz.executable-directory
  #f)


(define (jazz.pathname-name pathname)
  (let ((pos (jazz.string-find-reversed pathname #\/))
        (len (%%string-length pathname)))
    (cond ((%%not pos)
           pathname)
          ((%%fx= pos (%%fx- len 1))
           (jazz.pathname-name (%%substring pathname 0 pos)))
          (else
           (%%substring pathname (%%fx+ pos 1) len)))))


(define (jazz.pathname-base pathname)
  (let ((name (jazz.pathname-name pathname)))
    (let ((pos (jazz.string-find-reversed name #\.)))
      (if pos
          (%%substring name 0 pos)
        name))))


(define (jazz.pathname-extension pathname)
  (let ((name (jazz.pathname-name pathname)))
    (let ((pos (jazz.string-find-reversed name #\.)))
      (if pos
          (%%substring name (%%fx+ pos 1) (%%string-length name))
        #f))))


(define (jazz.extension? extension target)
  (or (and (%%not extension) (%%not target))
      (and extension
           target
           (%%string=? extension target))))


(define (jazz.numeric-extension? extension prefix)
  (and extension
       (jazz.string-starts-with? extension prefix)
       (jazz.string-numeric? (%%substring extension (%%string-length prefix) (%%string-length extension)))))


(define (jazz.executable-extension platform)
  (case platform
    ((windows)
     "exe")
    (else
     #f)))


(define (jazz.pathname-dir pathname)
  (let ((pos (jazz.string-find-reversed pathname #\/)))
    (if (%%not pos)
        #f
      (%%substring pathname 0 (%%fx+ pos 1)))))


(define (jazz.file-modification-time pathname)
  (time->seconds (file-last-modification-time pathname)))


(define (jazz.add-extension filename extension)
  (if (%%not extension)
      filename
    (%%string-append filename "." extension)))


(define (jazz.copy-file src dst #!key (feedback #f))
  (if (jazz.file-needs-update? src dst)
      (begin
        (if feedback
            (feedback "; copying {a}..." src))
        (if (file-exists? dst)
            (delete-file dst))
        (copy-file src dst))))


(define (jazz.copy-files src dst #!key (feedback #f))
  (if (not (file-exists? dst))
      (begin
        (if feedback
            (feedback "; copying {a}..." src))
        (create-directory dst)))
  (for-each (lambda (file)
              (let ((src-pathname (string-append src "/" file))
                    (dst-pathname (string-append dst "/" file)))
                (case (file-info-type (file-info src-pathname))
                  ((regular)
                   (jazz.copy-file src-pathname dst-pathname feedback: #f)))))
            (directory-files src)))


(define (jazz.file-needs-update? src dst)
  (or (%%not (file-exists? dst))
      (> (jazz.file-modification-time src)
         (jazz.file-modification-time dst))))


(define (jazz.relativise-directory basedir rootdir targdir)
  (let ((targdir (jazz.pathname-normalize targdir))
        (basedir (jazz.pathname-normalize basedir))
        (rootdir (jazz.pathname-normalize rootdir)))
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
            (%%string-append (or relative-dir "./") (%%substring targdir rootlen targlen)))
        targdir))))


(define (jazz.absolutize-directory basedir reldir)
  (if (or (jazz.string-starts-with? reldir "./")
          (jazz.string-starts-with? reldir "../"))
      (jazz.pathname-normalize (%%string-append basedir reldir) #f)
    (jazz.pathname-normalize reldir #f)))


(define (jazz.quote-pathname pathname #!optional (platform jazz.kernel-platform))
  (case platform
    ((windows)
     (string-append "\"" pathname "\""))
    (else
     ;; quoting is only necessary on windows as arguments are passed explicitly in unix
     pathname)))


(define (jazz.quote-jazz-pathname suffix)
  (jazz.quote-pathname (path-expand (string-append jazz.kernel-source suffix))))


(cond-expand
  (gambit
    (define jazz.file-exists?
      file-exists?))
  
  (else))


;;;
;;;; Digest
;;;


(define (jazz.updated-digest-source? digest src-filepath)
  (let ((time (jazz.file-modification-time src-filepath)))
    (if (= time (%%digest-time digest))
        #f
      (begin
        (%%digest-time-set! digest time)
        (%%digest-hash-set! digest (digest-file src-filepath 'sha-1))
        #t))))


;;;
;;;; Manifest
;;;


(define jazz.Manifest-Extension
  "mnf")

(define jazz.Digest-Extension
  "dgs")


(define (jazz.load-source-digests digest-filepath)
  (if (jazz.file-exists? digest-filepath)
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


(define (jazz.load-manifest digest-filepath manifest-filepath)
  (if (jazz.file-exists? manifest-filepath)
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
                    (source-digests (jazz.load-source-digests digest-filepath))
                    (references (if references-form (%%cdr references-form) #f)))
                (%%make-manifest name version compile-time-hash source-digests references))))))
    #f))


(define (jazz.save-manifest filepath manifest)
  (let ((name (%%manifest-name manifest))
        (version (%%manifest-version manifest))
        (references (%%manifest-references manifest)))
    (jazz.create-directories (jazz.pathname-dir filepath))
    (call-with-output-file (list path: filepath eol-encoding: (jazz.platform-eol-encoding jazz.kernel-platform))
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
        (write (%%manifest-compile-time-hash manifest) output)
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


(define (filter proc lst)
  (if (%%null? lst)
      '()
    (let ((head (%%car lst)))
      (if (proc head)
          (%%cons head (filter proc (%%cdr lst)))
        (filter proc (%%cdr lst))))))


(define (jazz.save-digest filepath manifest)
  (define (delete-digest-file)
    (if (file-exists? filepath)
        (delete-file filepath)))
  
  (define (save-digest digests)
    (jazz.create-directories (jazz.pathname-dir filepath))
    (call-with-output-file (list path: filepath eol-encoding: (jazz.platform-eol-encoding jazz.kernel-platform))
      (lambda (output)
        (display "(digest" output)
        (newline output)
        (for-each (lambda (digest)
                    (newline output)
                    (display "  (file " output)
                    (write (jazz.pathname-normalize (%%digest-pathname digest)) output)
                    (display " " output)
                    (write (%%digest-hash digest) output)
                    (display " " output)
                    (write (%%digest-time digest) output)
                    (display ")" output))
                  digests)
        (display ")" output)
        (newline output))))
  
  (let ((existing-files-digests (filter (lambda (digest)
                                          (file-exists? (%%digest-pathname digest)))
                                        (%%manifest-source-digests manifest))))
    (if (%%null? existing-files-digests)
        (delete-digest-file)
      (save-digest existing-files-digests))))


(define (jazz.find-source-digest src-pathname manifest)
  (define (find-digest)
    (let ((pathname (jazz.pathname-normalize src-pathname)))
      (let iter ((digests (%%manifest-source-digests manifest)))
           (if (%%pair? digests)
               (let ((digest (%%car digests)))
                 (if (jazz.path=? pathname (%%digest-pathname digest))
                     digest
                   (iter (%%cdr digests))))
             #f))))
  
  (define (new-digest)
    (let ((digest (%%make-digest (jazz.pathname-normalize src-pathname) "" 0)))
      (%%manifest-source-digests-set! manifest (%%cons digest (%%manifest-source-digests manifest)))
      digest))
  
  (or (find-digest) (new-digest)))


(define (jazz.manifest-uptodate? src-pathname manifest)
  (let ((digest (jazz.find-source-digest (jazz.pathname-normalize src-pathname) manifest)))
    (and (%%string=? (%%digest-hash digest) (%%manifest-compile-time-hash manifest))
         digest)))


(define (jazz.load/create-manifest name digest-filepath manifest-filepath)
  (or (jazz.load-manifest digest-filepath manifest-filepath)
      (%%make-manifest name jazz.kernel-version "" '() #f)))


(define (jazz.load-updated-manifest name digest-filepath manifest-filepath src-filepath)
  (let ((manifest (jazz.load/create-manifest name digest-filepath manifest-filepath)))
    (let ((digest (and src-filepath (jazz.find-source-digest src-filepath manifest))))
      (if (and digest
               (jazz.updated-digest-source? digest src-filepath))
          (jazz.save-digest digest-filepath manifest))
      manifest)))


(define (jazz.update-manifest-compile-time name digest-filepath manifest-filepath src-filepath updated-references)
  (let ((manifest (jazz.load/create-manifest name digest-filepath manifest-filepath)))
    (%%manifest-version-set! manifest jazz.kernel-version)
    (let ((digest (jazz.find-source-digest src-filepath manifest)))
      (jazz.updated-digest-source? digest src-filepath)
      (if updated-references
          (%%manifest-references-set! manifest updated-references))
      (%%manifest-compile-time-hash-set! manifest (%%digest-hash digest))
      (jazz.save-manifest manifest-filepath manifest))))


;;;
;;;; Library
;;;


(define jazz.Library-Extension "l")
(define jazz.Library-Manifest-Extension "lmf"))
