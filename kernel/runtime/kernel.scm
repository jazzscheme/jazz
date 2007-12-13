;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Kernel Runtime
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


(cond-expand
  (gambit
    (declare (block)
             (standard-bindings)
             (extended-bindings)))
  (else))


;;;
;;;; Output
;;;


(jazz.define-variable jazz.display display)
(jazz.define-variable jazz.write write)


;;;
;;;; Format
;;;


(define (jazz.format . rest)
  (jazz.parse-format rest
    (lambda (port fmt-string arguments)
      (case port
        ((:string)
         (let ((output (open-output-string)))
           (jazz.format-to output fmt-string arguments)
           (get-output-string output)))
        (else
         (jazz.format-to port fmt-string arguments))))))


(define (jazz.parse-format rest proc)
  (if (string? (%%car rest))
      (proc ':string (%%car rest) (%%cdr rest))
    (proc (%%car rest) (%%cadr rest) (%%cddr rest))))


(define (jazz.format-to output fmt-string arguments)
  (let* ((control (open-input-string fmt-string))
         (done? #f)
         (format-directive
          (lambda ()
            (let ((directive (read control)))
              (read-char control)
              (case directive
                ((a)
                 (jazz.display (%%car arguments) output)
                 (set! arguments (%%cdr arguments)))
                ((s)
                 (jazz.write (%%car arguments) output)
                 (set! arguments (%%cdr arguments)))
                ((t)
                 (jazz.write (%%car arguments) output)
                 (set! arguments (%%cdr arguments)))
                ((l)
                 (let ((first? #t))
                   (for-each (lambda (element)
                               (if first?
                                   (set! first? #f)
                                 (display " " output))
                               (jazz.display element output))
                             (%%car arguments)))
                 (set! arguments (%%cdr arguments)))
                ((%)
                 (newline output))
                (else
                 (jazz.kernel-error "Unknown format directive:" directive)))))))
    (let iter ()
      (let ((c (read-char control)))
        (if (%%not (%%eof-object? c))
            (begin
              (cond ((%%eqv? c #\~)
                     (write-char (read-char control) output))
                    ((%%eqv? c #\{)
                     (format-directive))
                    (else
                     (write-char c output)))
              (iter)))))))


;;;
;;;; Error
;;;


(define (jazz.kernel-error . rest)
  (apply error rest))


(define (jazz.error fmt-string . rest)
  (let ((error-string (apply jazz.format fmt-string rest)))
    (block-tail-call
      (error error-string))))


;;;
;;;; List
;;;


(define (jazz.collect-if predicate lst)
  (let iter ((scan lst))
    (if (%%not (%%null? scan))
        (let ((value (%%car scan)))
          (if (predicate value)
              (%%cons value (iter (%%cdr scan)))
            (iter (%%cdr scan))))
      '())))


(define (jazz.some? predicate lst)
  (let iter ((scan lst))
    (if (%%null? scan)
        #f
      (or (predicate (%%car scan))
          (iter (%%cdr scan))))))


(define (jazz.every? predicate lst)
  (let iter ((scan lst))
    (or (%%null? scan)
        (and (predicate (%%car scan))
             (iter (%%cdr scan))))))


;;;
;;;; String
;;;


(define (jazz.string-find-reversed str c)
  (let iter ((n (%%fx- (%%string-length str) 1)))
    (cond ((%%fx< n 0)
           #f)
          ((%%char=? (%%string-ref str n) c)
           n)
          (else
           (iter (%%fx- n 1))))))


(define (jazz.string-replace str old new)
  (let ((cpy (string-copy str)))
    (let iter ((n (%%fx- (%%string-length cpy) 1)))
      (if (%%fx>= n 0)
          (begin
            (if (%%eqv? (%%string-ref cpy n) old)
                (%%string-set! cpy n new))
            (iter (%%fx- n 1)))))
    cpy))


;;;
;;;; Pathname
;;;


(define (jazz.pathname-dir pathname)
  (let ((pos (jazz.string-find-reversed pathname #\/)))
    (if (%%not pos)
        #f
      (%%substring pathname 0 (%%fx+ pos 1)))))


(define (jazz.pathname-name pathname)
  (let ((pos (jazz.string-find-reversed pathname #\/)))
    (if (%%not pos)
        pathname
      (%%substring pathname (%%fx+ pos 1) (%%string-length pathname)))))


(define (jazz.pathname-base pathname)
  (let ((name (jazz.pathname-name pathname)))
    (let ((pos (jazz.string-find-reversed name #\.)))
      (if pos
          (%%substring name 0 pos)
        name))))


(define (jazz.pathname-extension pathname)
  (let ((pos (jazz.string-find-reversed pathname #\.)))
    (if pos
        (%%substring pathname (%%fx+ pos 1) (%%string-length pathname))
      #f)))


(cond-expand
  (gambit
    (define jazz.pathname-type
      file-type)
    
    (define jazz.file-exists?
      file-exists?)
    
    (define (jazz.file-modification-time pathname)
      (time->seconds (file-last-modification-time pathname)))
    
    (define jazz.file-copy
      copy-file)
    
    (define jazz.directory-exists?
      file-exists?)
    
    (define jazz.directory-create
      create-directory)
    
    (define jazz.directory-content
      directory-files)
    
    (define (jazz.directory-files directory)
      (jazz.collect-if (lambda (name)
                         (eq? (jazz.pathname-type (%%string-append directory name)) 'regular))
                       (jazz.directory-content directory)))
    
    (define (jazz.directory-directories directory)
      (jazz.collect-if (lambda (name)
                         (eq? (jazz.pathname-type (%%string-append directory name)) 'directory))
                       (jazz.directory-content directory))))
  
  (else))


;;;
;;;; Repository
;;;


(define jazz.Build-Repository
  (%%make-repository 'build "./_build/" #t))

(define jazz.App-Repository
  (%%make-repository 'app "./_app/" #t))

(define jazz.Lib-Repository
  (%%make-repository 'lib "../../lib/" #f))

(define jazz.User-Repository
  (%%make-repository 'user "~/jazz/lib/" #f))


(define jazz.Repositories
  (%%list
    jazz.Build-Repository
    jazz.App-Repository
    jazz.Lib-Repository
    jazz.User-Repository))


(define (jazz.repository-pathname repository path)
  (%%string-append (%%repository-directory repository)
                   path))


(define (jazz.repository-packages-table repository)
  (or (%%repository-packages-table repository)
      (let ((table (%%make-table test: eq?)))
        (%%repository-packages-table-set! repository table)
        (for-each (lambda (package)
                    (%%table-set! table (%%package-name package) package))
                  (jazz.repository-discover-packages repository))
        table)))


(define (jazz.repository-packages repository)
  (let ((table (jazz.repository-packages-table repository))
        (packages '()))
    (%%iterate-table table
      (lambda (name package)
        (set! packages (%%cons package packages))))
    packages))


(define (jazz.repository-find-package repository package-name)
  (%%table-ref (jazz.repository-packages-table repository) package-name #f))


(define (jazz.repository-discover-packages repository)
  (let ((repository-directory (%%repository-directory repository)))
    (if (jazz.directory-exists? repository-directory)
        (let iter ((dirnames (jazz.directory-directories repository-directory))
                   (packages '()))
          (if (%%null? dirnames)
              packages
            (let ((dirname (%%car dirnames)))
              (let ((directory (%%string-append repository-directory dirname "/")))
                (let ((package-pathname (%%string-append directory dirname "." jazz.Package-Extension)))
                  (if (jazz.file-exists? package-pathname)
                      (iter (%%cdr dirnames) (cons (jazz.load-package repository (%%string->symbol dirname) package-pathname) packages))
                    (iter (%%cdr dirnames) packages)))))))
      '())))


(define (jazz.load-package repository package-name package-pathname)
  (call-with-input-file package-pathname
    (lambda (input)
      (let ((form (read input)))
        (let ((name (%%cadr form))
              (alist (%%cddr form)))
          (if (%%eq? name package-name)
              (let ((root (assq 'root alist)))
                (jazz.make-package repository name (if root (%%cadr root) #f)))
            (jazz.error "Package at {s} is defining: {s}" package-pathname name)))))))


;;;
;;;; Package
;;;


(define jazz.Package-Extension
  "pck")


(define (jazz.make-package repository name root)
  (let ((path (if (%%not root)
                  (%%symbol->string name)
                (%%string-append (%%symbol->string name) "/" root))))
    (%%make-package repository name root path)))


(define (jazz.package-pathname package path)
  (jazz.repository-pathname (%%package-repository package)
    (%%string-append (%%package-path package)
                     "/"
                     path)))


(define (jazz.find-module-src module-name . rest)
  (let ((error? (if (%%null? rest) #t (%%car rest))))
    (let ((path (jazz.name->path module-name)))
      (let iter-repo ((repositories jazz.Repositories))
        (if (%%null? repositories)
            (if error?
                (jazz.error "Unable to find module: {s}" module-name)
              #f)
          (let iter ((packages (jazz.repository-packages (%%car repositories))))
            (if (%%null? packages)
                (iter-repo (%%cdr repositories))
              (or (jazz.package-find-src (%%car packages) path)
                  (iter (%%cdr packages))))))))))


(define (jazz.package-find-src package path)
  (define (try path)
    (define (try-extension extension)
      (if (jazz.file-exists? (jazz.package-pathname package (%%string-append path "." extension)))
          (%%make-resource package path extension)
        #f))
    
    (or (try-extension "jazz")
        (try-extension "scm")))
  
  (if (jazz.directory-exists? (jazz.package-pathname package path))
      (try (%%string-append path "/_" (jazz.pathname-name path)))
    (try path)))


(define (jazz.find-bin path)
  (let iter-repo ((repositories jazz.Repositories)
                  (found #f)
                  (foundtime #f))
    (if (%%null? repositories)
        found
      (let iter ((packages (jazz.repository-packages (%%car repositories)))
                 (found found)
                 (foundtime found))
        (if (%%null? packages)
            (iter-repo (%%cdr repositories) found foundtime)
          (let ((bin (jazz.package-find-bin (%%car packages) path)))
            (let ((bintime (and bin (jazz.file-modification-time (jazz.resource-pathname bin)))))
              (if (or (%%not found) (and bintime (> bintime foundtime)))
                  (iter (%%cdr packages) bin bintime)
                (iter (%%cdr packages) found foundtime)))))))))


(define (jazz.package-find-bin package path)
  (define (try n)
    (%%string-append "o" (number->string n)))
  
  (define (exists? extension)
    (jazz.file-exists? (jazz.package-pathname package (%%string-append path "." extension))))
  
  (let ((o1 (try 1)))
    (if (%%not (exists? o1))
        #f
      (let iter ((next 2)
                 (last-extension o1))
        (let ((next-extension (try next)))
          (if (exists? next-extension)
              (iter (%%fx+ next 1) next-extension)
            (%%make-resource package path last-extension)))))))


;;;
;;;; Resource
;;;


(define (jazz.resource-pathname resource)
  (jazz.package-pathname (%%resource-package resource)
    (jazz.resource-package-pathname resource)))


(define (jazz.resource-package-pathname resource)
  (%%string-append (%%resource-path resource)
                   "."
                   (%%resource-extension resource)))


(define (jazz.name->path resource-name)
  (jazz.string-replace (%%symbol->string resource-name) #\. #\/))


(define (jazz.path->name resource-name)
  (%%string->symbol (jazz.string-replace resource-name #\/ #\.)))


;;;
;;;; Digest
;;;


(define (jazz.resource-digest src)
  (let ((pathname (jazz.resource-pathname src)))
    (%%make-digest (digest-file pathname 'sha-1)
                   (jazz.file-modification-time pathname)
                   #t)))


(define (jazz.bin-determine/cache-uptodate? src manifest manifest-package)
  (let ((pathname (jazz.resource-pathname src))
        (digest (%%manifest-digest manifest)))
    (let ((hash (%%digest-hash digest))
          (cached-time (%%digest-cached-time digest))
          (cached-identical? (%%digest-cached-identical? digest))
          (time (jazz.file-modification-time pathname)))
      (if (= time cached-time)
          cached-identical?
        (let ((identical? (%%string=? hash (digest-file pathname 'sha-1))))
          (%%digest-cached-time-set! digest time)
          (%%digest-cached-identical?-set! digest identical?)
          (jazz.save-manifest (%%make-resource manifest-package (%%resource-path src) jazz.Manifest-Extension) manifest)
          identical?)))))


;;;
;;;; Manifest
;;;


(define jazz.Manifest-Extension
  "mnf")


(define (jazz.load-manifest bin)
  (let ((resource (%%make-resource (%%resource-package bin) (%%resource-path bin) jazz.Manifest-Extension)))
    (let ((pathname (jazz.resource-pathname resource)))
      (if (jazz.file-exists? pathname)
          (call-with-input-file pathname
            (lambda (input)
              (let ((form (read input)))
                (let ((name (%%cadr form))
                      (digest-form (%%assq 'digest (%%cddr form))))
                  (let ((hash (%%cadr digest-form))
                        (cached-time (%%car (%%cddr digest-form)))
                        (cached-identical? (%%cadr (%%cddr digest-form))))
                    (%%make-manifest name (%%make-digest hash cached-time cached-identical?)))))))
        #f))))


(define (jazz.save-manifest resource manifest)
  (let ((name (%%manifest-name manifest))
        (digest (%%manifest-digest manifest)))
    (call-with-output-file (jazz.resource-pathname resource)
      (lambda (output)
        (display "(manifest " output)
        (display name output)
        (newline output)
        (newline output)
        (display "  (digest " output)
        (write (%%digest-hash digest) output)
        (display " " output)
        (write (%%digest-cached-time digest) output)
        (display " " output)
        (write (%%digest-cached-identical? digest) output)
        (display "))" output)
        (newline output)))))


;;;
;;;; Load
;;;


(cond-expand
  (gambit
    (define (jazz.load pathname . rest)
      (let ((quiet? (if (null? rest) #f (car rest))))
        (##load pathname (lambda rest #f) #f #t quiet?))
      (void)))
  
  (else
    (define (jazz.load pathname . rest)
      (load pathname))))


(define jazz.load-indent
  (make-parameter 0))


(define (jazz.load-resource resource . rest)
  (let ((quiet? (if (null? rest) #f (car rest))))
    (jazz.with-verbose jazz.load-verbose? "loading" (jazz.resource-package-pathname resource)
      (lambda ()
        (jazz.load (jazz.resource-pathname resource) quiet?)))))


(define (jazz.with-verbose flag action path proc)
  (define (verbose-load)
    (display (make-string (jazz.load-indent) #\space))
    (display "; ")
    (display action)
    (display " ")
    (display path)
    (display " ...")
    (newline)
    (force-output))
  
  (define (verbose-done)
    (display (make-string (jazz.load-indent) #\space))
    (display "; done ")
    (display " ...")
    (newline)
    (force-output))
  
  (if flag
      (begin
        (verbose-load)
        (let ((result
                (parameterize ((jazz.load-indent (+ (jazz.load-indent) 2)))
                  (proc))))
          (if jazz.done-verbose?
              (verbose-done))
          result))
    (proc)))


(define jazz.walk-for
  (make-parameter #f))


(define (jazz.with-src/bin src proc)
  (let ((bin (jazz.find-bin (%%resource-path src))))
    (let ((manifest (and bin (jazz.load-manifest bin))))
      (let ((bin-uptodate?
              (and manifest
                   (jazz.bin-determine/cache-uptodate? src manifest (%%resource-package bin)))))
        (proc src bin bin-uptodate?)))))


(define (jazz.load-src src)
  (parameterize ((jazz.walk-for 'interpret))
    (jazz.load-resource src)))


(define (jazz.load-bin bin quiet?)
  (parameterize ((jazz.walk-for 'interpret))
    (jazz.load-resource bin quiet?)))


(define (jazz.load-source src)
  (jazz.with-src/bin src
    (lambda (src bin bin-uptodate?)
      (if bin-uptodate?
          (let ((quiet? (or (%%not src) (%%string=? (%%resource-extension src) "jazz"))))
            (jazz.load-bin bin quiet?))
        (jazz.with-extension-reader (%%resource-extension src)
          (lambda ()
            (jazz.load-src src)))))))


;;;
;;;; Build
;;;


(define (jazz.resource-build-dir resource)
  (let ((package (%%resource-package resource))
        (dir (jazz.pathname-dir (%%resource-path resource))))
    (jazz.repository-pathname jazz.Build-Repository
      (%%string-append (%%package-path package) "/" dir))))


;;;
;;;; State
;;;


(define jazz.Unloaded-State
  #f)

(define jazz.Loading-State
  '(loading))

(define jazz.Loaded-State
  '(loaded))


;;;
;;;; Environment
;;;


(define jazz.Environment
  (%%make-table test: eq?))


(define (jazz.get-environment)
  jazz.Environment)


(define (jazz.get-environment-module name)
  (%%table-ref jazz.Environment name jazz.Unloaded-State))


(define (jazz.set-environment-module name module)
  (%%table-set! jazz.Environment name module))


;;;
;;;; Module
;;;


(define jazz.requested-module-name
  (make-parameter #f))


(define (jazz.load-module module-name)
  (let ((module-state (jazz.get-environment-module module-name)))
    (cond ((%%eq? module-state jazz.Unloaded-State)
           (dynamic-wind
             (lambda ()
               (jazz.set-environment-module module-name jazz.Loading-State))
             (lambda ()
               (parameterize ((jazz.requested-module-name module-name))
                 (block-tail-call
                   (jazz.load-source (jazz.find-module-src module-name)))))
             (lambda ()
               (if (%%eq? (jazz.get-environment-module module-name) jazz.Loading-State)
                   (jazz.set-environment-module module-name jazz.Unloaded-State)))))
          ((%%eq? module-state jazz.Loading-State)
           (jazz.error "Circular loading of module: {s}" module-name)))))


(define (jazz.module-loaded? module-name)
  (let ((module (jazz.get-environment-module module-name)))
    (and module (%%neq? module ':loading))))


(define (jazz.module-loaded module-name)
  (jazz.set-environment-module module-name jazz.Loaded-State))


(define (jazz.unload-module module-name)
  (jazz.set-environment-module module-name jazz.Unloaded-State))


(define (jazz.reload-module module-name)
  (jazz.unload-module module-name)
  (jazz.load-module module-name))


;;;
;;;; Reader
;;;


(define jazz.Extension-Readers
  (%%make-table test: equal?))


(define (jazz.get-extension-reader extension)
  (%%table-ref jazz.Extension-Readers extension #f))


(define (jazz.with-extension-reader extension thunk)
  (let ((reader-info (jazz.get-extension-reader extension)))
    (if reader-info
        (let ((dialect-name (%%car reader-info))
              (readtable-getter (%%cdr reader-info)))
          (jazz.load-module dialect-name)
          (parameterize ((current-readtable (readtable-getter)))
            (thunk)))
      (thunk))))


(define (jazz.register-reader-extension dialect-name readtable-getter extension)
  (%%table-set! jazz.Extension-Readers extension (%%cons dialect-name readtable-getter)))


(define (jazz.register-reader-extensions dialect-name readtable-getter extensions)
  (for-each (lambda (extension)
              (jazz.register-reader-extension dialect-name readtable-getter extension))
            extensions))


;;;
;;;; Init
;;;


;; should probably be the packages that register reader extensions...


(jazz.define-variable jazz.jazz-readtable)


(jazz.register-reader-extensions 'jazz.dialect (lambda () jazz.jazz-readtable) '("jazz"))


(if (file-exists? "~/jazz/jazzini.scm")
    (jazz.load "~/jazz/jazzini"))
