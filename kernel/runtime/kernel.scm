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


(jazz.kernel-declares)


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
  (let ((control (open-input-string fmt-string))
        (done? #f))
    (define (format-directive)
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
           (jazz.kernel-error "Unknown format directive:" directive)))))
    
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
;;;; Exception
;;;


(define jazz.pristine-thread-continuation
  (thread-join!
    (thread-start!
      (make-thread
        (lambda ()
          (continuation-capture
            (lambda (cont)
              cont)))))))


;;;
;;;; Error
;;;


(define (jazz.kernel-error . rest)
  (apply error rest))


(define (jazz.raise-system-error fmt-string . rest)
  (let ((error-string (apply jazz.format fmt-string rest)))
    (error error-string)))


(define jazz.error #f)
(set! jazz.error jazz.raise-system-error)


(define (jazz.primitive-type-error num type proc args)
  (error (jazz.format "(Argument {a}) {a} expected :" num type) (cons proc args)))


;;;
;;;; List
;;;


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


;;;
;;;; Symbol
;;;


(cond-expand
  (chicken
    (require 'lolevel)

    (define (jazz.global-variable? symbol)
      (global-bound? symbol))
    
    (define (jazz.global-value symbol)
      (global-ref symbol)))
  
  (gambit
    (define (jazz.global-variable? symbol)
      (and (%%global-var? symbol)
           (%%not (%%unbound? (%%global-var-ref symbol)))))
    
    (define (jazz.global-value symbol)
      (%%global-var-ref symbol)))
  
  (else))


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
    (define jazz.file-exists?
      file-exists?)
    
    (define jazz.file-delete
      delete-file)
    
    (define jazz.file-copy
      copy-file)
    
    (define jazz.directory-exists?
      file-exists?)
    
    (define jazz.directory-create
      create-directory))
  
  (else))


;;;
;;;; Product
;;;


(define jazz.kernel-built
  jazz.built)


(define jazz.kernel-install
  (or (and jazz.executable-directory (jazz.executable-directory))
      (jazz.pathname-normalize jazz.built)))


(define jazz.kernel-source-built
  jazz.source-built)


(define jazz.kernel-source
  ;; kernel always needs source access to build
  (if (or jazz.source-access? (not jazz.product))
      ;; when the install directory is a subdirectory of the source use a .. notation
      (if (jazz.string-starts-with? jazz.source "../")
          (jazz.pathname-normalize (%%string-append jazz.kernel-install jazz.source) #f)
        (jazz.pathname-normalize jazz.source #f))
    #f))


(define (jazz.jazz-product)
  jazz.product)


(define (jazz.jazz-profile)
  (jazz.profile))


(define jazz.profile
  (make-parameter #f))


;; to enable timing loading of jedi
(define jazz.run-loop?
  (make-parameter #t))


;;;
;;;; Repository
;;;


(define jazz.Repository-Filename
  ".repository")


(define (jazz.repository? obj)
  (and (%%vector? obj)
       (%%fx> (%%vector-length obj) 0)
       (%%eq? (%%vector-ref obj 0) 'repository)))


(define (jazz.make-repository name dirname dir subdir packages-root #!key (binary? #f) (error? #t))
  (if (and dir (%%not (jazz.directory-exists? dir)))
      (jazz.create-directories dir))
  (if (and dir (jazz.directory-exists? dir))
      (let ((directory (%%string-append (jazz.pathname-normalize dir) (or subdir ""))))
        (if (%%not (jazz.directory-exists? directory))
            (begin
              (jazz.create-directories directory)
              (call-with-output-file (%%string-append directory jazz.Repository-Filename)
                (lambda (output)
                  (display "(repository " output)
                  (display name output)
                  (newline output)
                  (newline output)
                  (display "  (root " output)
                  (write packages-root output)
                  (display "))" output)
                  (newline output)))))
        (let ((packages-directory (if (%%not packages-root) directory (%%string-append directory packages-root "/"))))
          (%%make-repository name directory packages-root packages-directory binary?)))
    (if error?
        (jazz.error "{a} directory is inexistant: {a}" dirname dir)
      #f)))


(define jazz.Bin-Repository
  (jazz.make-repository 'Bin "Bin" jazz.kernel-install #f "lib" binary?: #t))

(define jazz.Jazz-Repository
  (jazz.make-repository 'Jazz "Jazz" jazz.kernel-source #f "lib" error?: #f))

(define jazz.Opt-Repository
  (jazz.make-repository 'Opt "Opt" "~/" "jazz_opt/" "lib"))

(define jazz.User-Repository
  (jazz.make-repository 'User "User" "~/" "jazz_user/" "lib"))


(define (jazz.all-repositories)
  (define (listify repository)
    (if repository
        (%%list repository)
      '()))
  
  `(,@(listify jazz.Bin-Repository)
    ,@(listify jazz.Jazz-Repository)
    ,@(listify jazz.Opt-Repository)
    ,@(listify jazz.User-Repository)))


(define jazz.Repositories
  (jazz.all-repositories))


(define (jazz.get-repositories)
  jazz.Repositories)


(define (jazz.load-repository directory)
  (let ((repository-file (%%string-append directory jazz.Repository-Filename)))
    (call-with-input-file (list path: repository-file eol-encoding: 'cr-lf)
      (lambda (input)
        (let ((form (read input)))
          (let ((name (%%cadr form))
                (alist (%%cddr form)))
            (let ((directory (jazz.pathname-normalize directory))
                  (packages-pair (assq 'root alist)))
              (let ((packages-root (if packages-pair (%%cadr packages-pair) #f)))
                (let ((packages-directory (if (%%not packages-root) directory (%%string-append directory packages-root "/"))))
                (%%make-repository name directory packages-root packages-directory #f))))))))))


(define (jazz.install-repository directory)
  (let ((repository (jazz.load-repository directory)))
    (set! jazz.Repositories (%%append jazz.Repositories (%%list repository)))
    (jazz.setup-repository repository)
    repository))


(define (jazz.uninstall-repository repository)
  (set! jazz.Repositories (%%remove repository jazz.Repositories)))


(define (jazz.find-repository name)
  (let iter ((repositories jazz.Repositories))
    (if (%%null? repositories)
        #f
      (let ((repository (%%car repositories)))
        (if (eq? (%%repository-name repository) name)
            repository
          (iter (%%cdr repositories)))))))


(define (jazz.find-package package-name)
  (let iter ((repositories jazz.Repositories))
    (if (%%null? repositories)
        #f
      (let ((repository (%%car repositories)))
        (if (%%repository-binary? repository)
            (iter (%%cdr repositories))
          (let ((package (jazz.repository-find-package repository package-name)))
            (if package
                package
              (iter (%%cdr repositories)))))))))


(define (jazz.repository-pathname repository path)
  (%%string-append (%%repository-packages-directory repository)
                   path))


(define (jazz.repository-packages-table repository)
  (or (%%repository-packages-table repository)
      (let ((table (%%make-table test: eq?)))
        (%%repository-packages-table-set! repository table)
        (jazz.repository-install-packages repository)
        table)))


(define (jazz.setup-repositories)
  (for-each jazz.setup-repository jazz.Repositories))


(define (jazz.setup-repository repository)
  (let ((table (jazz.repository-packages-table repository)))
    (%%iterate-table table
      (lambda (name package)
        (jazz.setup-package package)))))


(define (jazz.repository-packages repository)
  (let ((table (jazz.repository-packages-table repository))
        (packages '()))
    (%%iterate-table table
      (lambda (name package)
        (set! packages (%%cons package packages))))
    packages))


(define (jazz.repository-find-package repository package-name)
  (%%table-ref (jazz.repository-packages-table repository) package-name #f))


(define (jazz.repository-install-packages repository)
  (let ((table (%%repository-packages-table repository))
        (packages (jazz.repository-discover-packages repository)))
    (for-each (lambda (package)
                (%%table-set! table (%%package-name package) package))
              packages)
    packages))


(define (jazz.repository-discover-packages repository)
  (let ((table (%%repository-packages-table repository))
        (repository-packages-directory (%%repository-packages-directory repository)))
    (if (jazz.directory-exists? repository-packages-directory)
        (let iter ((dirnames (jazz.directory-directories repository-packages-directory))
                   (packages '()))
          (if (%%null? dirnames)
              packages
            (let ((dirname (%%car dirnames)))
              (let ((directory (%%string-append repository-packages-directory dirname "/")))
                (let ((package-pathname (%%string-append directory jazz.Package-Filename)))
                  (if (jazz.file-exists? package-pathname)
                      (let ((package-name (%%string->symbol dirname)))
                        (if (%%table-ref table package-name #f)
                            (iter (%%cdr dirnames) packages)
                          (iter (%%cdr dirnames) (cons (jazz.load-package repository package-name package-pathname) packages))))
                    (iter (%%cdr dirnames) packages)))))))
      '())))


(define (jazz.repository-add-package repository package)
  (let ((table (jazz.repository-packages-table repository)))
    (%%table-set! table (%%package-name package) package)))


(define (jazz.repository-remove-package repository package)
  (let ((table (jazz.repository-packages-table repository)))
    (%%table-clear table (%%package-name package))))


(define (jazz.load-package repository package-name package-pathname)
  (call-with-input-file (list path: package-pathname eol-encoding: 'cr-lf)
    (lambda (input)
      (let ((form (read input)))
        (let ((name (%%cadr form))
              (alist (%%cddr form)))
          (if (%%eq? name package-name)
              (let ((root (assq 'root alist))
                    (install (assq 'install alist))
                    (products (assq 'products alist))
                    (profiles (assq 'profiles alist))
                    (project (assq 'project alist)))
                (jazz.make-package repository name
                  (if root (%%cadr root) #f)
                  (if install (%%cadr install) #f)
                  (if products (%%cdr products) '())
                  (if profiles (%%cdr profiles) '())
                  (if project (%%cadr project) #f)))
            (jazz.error "Package at {s} is defining: {s}" package-pathname name)))))))


(define (jazz.setup-package package)
  (let ((install (%%package-install package)))
    (if install
        (jazz.load-module install))))


(define (jazz.inspect-install)
  (define (inspect-path path)
    `(:path ,path ,(path-expand path)))
  
  (define (inspect-repository repository)
    `(:repository
      ,(%%repository-name repository)
      ,(%%repository-packages-directory repository)
      ,@(map inspect-package (jazz.repository-packages repository))))
  
  (define (inspect-package package)
    `(:package
      ,(%%package-name package)
      ,(%%package-modules-root package)
      ,(%%package-modules-path package)))
  
  `(,(inspect-path "./")
    ,(inspect-path "~/")
    ,@(map inspect-repository jazz.Repositories)))


;;;
;;;; Package
;;;


(define jazz.Package-Filename
  ".package")


(define (jazz.package? obj)
  (and (%%vector? obj)
       (%%fx> (%%vector-length obj) 0)
       (%%eq? (%%vector-ref obj 0) 'package)))


(define (jazz.make-package repository name root install products profiles project)
  (let ((path (if (%%not root)
                  (%%symbol->string name)
                (%%string-append (%%symbol->string name) "/" root))))
    (%%make-package repository name root path install products profiles project)))


(define (jazz.package-pathname package path)
  (jazz.repository-pathname (%%package-repository package)
    (%%string-append (%%package-modules-path package)
                     "/"
                     path)))


(define (jazz.iterate-resources module-name proc)
  (let ((path (jazz.name->path module-name)))
    (let iter-repo ((repositories jazz.Repositories))
      (if (%%not (%%null? repositories))
          (let iter ((packages (jazz.repository-packages (%%car repositories))))
            (if (%%null? packages)
                (iter-repo (%%cdr repositories))
              (let ((package (%%car packages)))
                (proc package path)
                (iter (%%cdr packages)))))))))


(define (jazz.get-package-autoload package name)
  (%%table-ref (%%package-autoloads package) name #f))


(define (jazz.set-package-autoload package name module-name loader)
  (%%table-set! (%%package-autoloads package) name (%%cons module-name loader)))


(define (jazz.register-package-autoload package name module-name loader)
  (let ((actual (jazz.get-package-autoload package name)))
    (if (or (%%not actual) (%%eq? (%%car actual) module-name))
        (jazz.set-package-autoload package name module-name loader)
      (jazz.error "Conflict detected for autoload {s} in package {s} between {s} and {s}" name (%%package-name package) (%%car actual) module-name))))


(define (jazz.module-autoload module-name name)
  (define (find-autoload resource)
    (if (%%not resource)
        #f
      (let ((package (%%resource-package resource)))
        (if (%%not package)
            #f
          (let ((autoload (jazz.get-package-autoload package name)))
            (if (%%not autoload)
                #f
              (%%cdr autoload)))))))
  
  (jazz.load-module module-name)
  (jazz.with-module-src/bin module-name #f
    (lambda (src bin bin-uptodate?)
      (let ((src-autoload (find-autoload src))
            (bin-autoload (find-autoload bin)))
        (cond (bin-autoload (bin-autoload))
              (src-autoload (src-autoload))
              (else (jazz.error "Unable to find autoload {s} in package {s}" name module-name)))))))


(define (jazz.find-resource pathname)
  (let iter-repo ((repositories jazz.Repositories))
    (if (%%null? repositories)
        #f
      (let iter ((packages (jazz.repository-packages (%%car repositories))))
        (if (%%null? packages)
            (iter-repo (%%cdr repositories))
          (let ((package (%%car packages)))
            (let ((package-pathname (jazz.package-pathname package "")))
              (let ((pathname-length (%%string-length pathname))
                    (package-length (%%string-length package-pathname)))
                (if (and (%%fx<= package-length pathname-length)
                         (%%string=? (%%substring pathname 0 package-length) package-pathname))
                    (let* ((path (%%substring pathname package-length pathname-length))
                           (extension (and path (jazz.pathname-extension path))))
                      ;; remove extension
                      (if extension
                          (set! path (%%substring path 0 (%%fx- (%%string-length path) (%%fx+ 1 (%%string-length extension))))))
                      ;; remove redundant underscore prefixed name
                      (let ((len (%%string-length path))
                            (pos (jazz.string-find-reversed path #\/)))
                        (if pos
                            (let ((name-pos (%%fx+ pos 2)))
                              (if (and (%%fx< name-pos len) (%%eqv? (%%string-ref path (%%fx+ pos 1)) #\_))
                                  (let* ((name (%%substring path name-pos len))
                                         (name-length (%%string-length name))
                                         (previous-pos (%%fx- pos name-length)))
                                    (if (and (%%fx>= previous-pos 0) (%%string=? (%%substring path previous-pos pos) name))
                                        (set! path (%%substring path 0 pos))))))))
                      (%%make-resource package path extension))
                  (iter (%%cdr packages)))))))))))


(define (jazz.find-pathname-module pathname)
  (let ((resource (jazz.find-resource pathname)))
    (if resource
        (jazz.path->name (%%resource-path resource))
      #f)))


;;;
;;;; Profiles
;;;


(define (jazz.gather-profiles)
  (let iter-repo ((repositories jazz.Repositories) (profiles '()))
    (if (%%null? repositories)
        profiles
      (let iter ((packages (jazz.repository-packages (%%car repositories))) (profiles profiles))
        (if (%%null? packages)
            (iter-repo (%%cdr repositories) profiles)
          (let ((package-profiles (%%package-profiles (%%car packages))))
            (iter (%%cdr packages) (%%append package-profiles profiles))))))))


(define (jazz.make-profile name module-name)
  `(,name (module ,module-name)))


(define (jazz.profile-name profile)
  (%%car profile))

(define (jazz.profile-title profile)
  (symbol->string (jazz.profile-name profile)))

(define (jazz.profile-module profile)
  (%%cadr (%%assq 'module (%%cdr profile))))


;;;
;;;; Module
;;;


(define (jazz.package-find-src package path extensions)
  (define (try path)
    (define (try-extension extension)
      (if (jazz.file-exists? (jazz.package-pathname package (%%string-append path "." extension)))
          (%%make-resource package path extension)
        #f))
    
    (let iter ((extensions (or extensions '("scm" "jazz"))))
      (if (%%null? extensions)
          #f
        (or (try-extension (%%car extensions))
            (iter (%%cdr extensions))))))
  
  (if (jazz.directory-exists? (jazz.package-pathname package path))
      (try (%%string-append path "/_" (jazz.pathname-name path)))
    (try path)))


(define (jazz.package-find-bin package path)
  (define (try path)
    (define (extension n)
      (%%string-append "o" (number->string n)))
    
    (define (exists? extension)
      (jazz.file-exists? (jazz.package-pathname package (%%string-append path "." extension))))
    
    (let ((o1 (extension 1)))
      (if (%%not (exists? o1))
          #f
        (let iter ((next 2)
                   (previous-extension o1))
          (let ((next-extension (extension next)))
            (if (exists? next-extension)
                (iter (%%fx+ next 1) next-extension)
              (%%make-resource package path previous-extension)))))))
  
  (if (jazz.directory-exists? (jazz.package-pathname package path))
      (try (%%string-append path "/_" (jazz.pathname-name path)))
    (try path)))


(define (jazz.find-module-src module-name extensions . rest)
  (let ((error? (if (%%null? rest) #t (%%car rest))))
    (continuation-capture
      (lambda (return)
        (jazz.iterate-resources module-name
          (lambda (package path)
            (let ((src (jazz.package-find-src package path extensions)))
              (if src
                  (begin
                    #; ;; test
                    (jazz.validate-repository-unicity (%%package-repository package)
                                                      module-name
                                                      (lambda (package)
                                                        (jazz.package-find-src package path extensions)))
                    (continuation-return return src))))))
        (if error?
            (jazz.error "Unable to find module: {s}" module-name)
          #f)))))


(define (jazz.with-module-src/bin module-name extensions proc)
  (let ((bin #f)
        (src #f))
    (continuation-capture
      (lambda (return)
        (jazz.iterate-resources module-name
          (lambda (package path)
            (if (%%not bin)
                (set! bin (jazz.package-find-bin package path)))
            (if (%%not src)
                (set! src (jazz.package-find-src package path extensions)))
            (if src
                (continuation-return return #f))))))
    #; ;; test
    (if bin
        (jazz.validate-repository-unicity (%%package-repository (%%resource-package bin))
                                          (jazz.path->name (%%resource-path bin))
                                          (lambda (package)
                                            (jazz.package-find-bin package (%%resource-path bin)))))
    #; ;; test
    (if src
        (jazz.validate-repository-unicity (%%package-repository (%%resource-package src))
                                          (jazz.path->name (%%resource-path src))
                                          (lambda (package)
                                            (jazz.package-find-src package (%%resource-path src) extensions))))
    (let ((manifest (and bin (jazz.load-manifest bin))))
      (let ((bin-uptodate?
              (and bin (or (%%not src)
                           (and manifest
                                (not (jazz.manifest-needs-rebuild? manifest))
                                (jazz.bin-determine/cache-uptodate? src manifest (%%resource-package bin)))))))
        (proc src bin bin-uptodate?)))))


(define (jazz.module-uptodate-binary? module-name)
  (jazz.with-module-src/bin module-name #f
    (lambda (src bin bin-uptodate?)
      bin-uptodate?)))


(define (jazz.validate-repository-unicity repository module-name proc)
  (if (%%not (jazz.repository-unique? repository proc))
      (jazz.error "Found duplicate resource in {a} repository: {s}"
                  (or (%%repository-name repository) "anonymous")
                  module-name)))


(define (jazz.repository-unique? repository proc)
  (let iter ((packages (jazz.repository-packages repository))
             (found? #f))
    (if (%%null? packages)
        #t
      (let ((package (%%car packages)))
        (if (proc package)
            (if found?
                #f
              (iter (%%cdr packages) #t))
          (iter (%%cdr packages) found?))))))


;;;
;;;; Debug
;;;


(define (jazz.setup-debuggee)
  (jazz.load-module 'core.library)
  (jazz.load-module 'jazz)
  (jazz.load-module 'jazz.debuggee)
  (jazz.load-module 'jazz.debuggee.Debuggee-Frame)
  (jazz.load-module 'jazz.debuggee.Debuggee-Process)
  (jazz.load-module 'jazz.debuggee.Debuggee-Stop)
  (jazz.load-module 'jazz.debuggee.Debuggee-Thread)
  (jazz.load-module 'jazz.debuggee.stub)
  (jazz.load-module 'jazz.debugger.debuggers.jazz.stub)
  (jazz.load-module 'jazz.debugger.debuggers.jazz.stub-autoload)
  (jazz.load-module 'jazz.debuggee.setup))


;;;
;;;; Product
;;;


(define (jazz.find-product-descriptor name)
  (let iter-repo ((repositories jazz.Repositories))
    (if (%%null? repositories)
        #f
      (let ((repository (%%car repositories)))
        (if (%%repository-binary? repository)
            (iter-repo (%%cdr repositories))
          (let iter ((packages (jazz.repository-packages (%%car repositories))))
            (if (%%null? packages)
                (iter-repo (%%cdr repositories))
              (let ((package (%%car packages)))
                (let ((pair (%%assq name (%%package-products package))))
                  (if pair
                      pair
                    (iter (%%cdr packages))))))))))))


(define (jazz.product-descriptor-name descriptor)
  (%%car descriptor))


(define (jazz.product-descriptor-module descriptor)
  (let ((pair (%%assq 'module (%%cdr descriptor))))
    (if pair
        (%%cadr pair)
      #f)))


(define (jazz.product-descriptor-title descriptor)
  (let ((pair (%%assq 'title (%%cdr descriptor))))
    (if pair
        (%%cadr pair)
      #f)))


(define (jazz.product-descriptor-icon descriptor)
  (let ((pair (%%assq 'icon (%%cdr descriptor))))
    (if pair
        (%%cadr pair)
      #f)))


(define (jazz.product-descriptor-run descriptor)
  (let ((pair (%%assq 'run (%%cdr descriptor))))
    (if pair
        (%%cdr pair)
      #f)))


(define (jazz.product-descriptor-update descriptor)
  (let ((pair (%%assq 'update (%%cdr descriptor))))
    (if pair
        (%%cdr pair)
      #f)))


(define (jazz.product-descriptor-build descriptor)
  (let ((pair (%%assq 'build (%%cdr descriptor))))
    (if pair
        (%%cdr pair)
      #f)))


(define (jazz.product-descriptor-dependencies descriptor)
  (let ((pair (%%assq 'dependencies (%%cdr descriptor))))
    (if pair
        (%%cdr pair)
      '())))


(define jazz.Products-Table
  (make-table test: eq?))

(define jazz.Products-Run-Table
  (make-table test: eq?))


(define jazz.process-name
  #f)

(define jazz.process-title
  #f)

(define jazz.process-icon
  #f)

(define jazz.process-version
  #f)


(define (jazz.current-process-name)
  jazz.process-name)

(define (jazz.current-process-name-set! name)
  (set! jazz.process-name name))

(define (jazz.current-process-title)
  jazz.process-title)

(define (jazz.current-process-title-set! title)
  (set! jazz.process-title title))

(define (jazz.current-process-icon)
  jazz.process-icon)

(define (jazz.current-process-icon-set! icon)
  (set! jazz.process-icon icon))

(define (jazz.current-process-version)
  jazz.process-version)

(define (jazz.current-process-version-set! version)
  (set! jazz.process-version version))


(define (jazz.current-process-present)
  (or (jazz.current-process-title)
      (let ((name (jazz.current-process-name)))
        (if name
            (symbol->string name)
          #f))))


(define (jazz.register-product name #!key (title #f) (icon #f) (run #f) (update #f) (build #f))
  (table-set! jazz.Products-Table name (%%make-product name title icon run update build (jazz.find-product-descriptor name))))


(define (jazz.get-registered-product name)
  (or (table-ref jazz.Products-Table name #f)
      (jazz.error "Unable to find registered product: {s}" name)))


(define (jazz.get-product-descriptor name)
  (let ((descriptor (jazz.find-product-descriptor name)))
    (if descriptor
        descriptor
      (jazz.error "Unable to find product: {s}" name))))


(define (jazz.get-product name)
  (let ((descriptor (jazz.get-product-descriptor name)))
    (let ((module (jazz.product-descriptor-module descriptor)))
      (if module
          (begin
            (jazz.load-module module)
            (jazz.get-registered-product name))
        (let ((title (jazz.product-descriptor-title descriptor))
              (icon (jazz.product-descriptor-icon descriptor)))
          (%%make-product name title icon
            #f
            jazz.update-product-descriptor
            jazz.build-product-descriptor
            descriptor))))))


(define (jazz.setup-product name)
  (if (not jazz.debugger)
      (jazz.get-product name)
    (begin
      (set! jazz.process-name name)
      (jazz.setup-debuggee)
      (let ((product (jazz.get-product name)))
        (let ((descriptor (%%product-descriptor product)))
          (set! jazz.process-name name)
          (set! jazz.process-title (or (%%product-title product) (jazz.product-descriptor-title descriptor)))
          (set! jazz.process-icon (or (%%product-icon product) (jazz.product-descriptor-icon descriptor)))
          (jazz.load-module 'jazz.debuggee.update)
          product)))))


(define (jazz.register-product-run name proc)
  (table-set! jazz.Products-Run-Table name proc))


(define (jazz.get-registered-run name)
  (or (table-ref jazz.Products-Run-Table name #f)
      (jazz.error "Unable to find registered run: {s}" name)))


(define (jazz.run-product name)
  (let ((product (jazz.setup-product name)))
    (let ((run (%%product-run product))
          (descriptor (%%product-descriptor product)))
      (if run
          (run descriptor)
        (jazz.run-product-descriptor descriptor)))))


(define (jazz.run-product-descriptor descriptor)
  (let ((name (jazz.product-descriptor-name descriptor))
        (run (jazz.product-descriptor-run descriptor)))
    (if run
        (begin
          (for-each jazz.load-module run)
          (let ((proc (jazz.get-registered-run name)))
            (proc descriptor)))
      (jazz.error "Product is not runnable: {s}" name))))


(define (jazz.update-product name)
  (let ((product (jazz.setup-product name)))
    (let ((update (%%product-update product))
          (descriptor (%%product-descriptor product)))
      (if update
          (update descriptor)
        (jazz.update-product-descriptor descriptor)))))


(define (jazz.update-product-descriptor descriptor)
  (let ((update (jazz.product-descriptor-update descriptor)))
    (if update
        (for-each jazz.build-module update)
      (jazz.error "Product is not updateable: {s}" (jazz.product-descriptor-name descriptor)))))


(define (jazz.build-product name)
  (let ((product (jazz.setup-product name)))
    (let ((build (%%product-build product))
          (descriptor (%%product-descriptor product)))
      (jazz.feedback "make {a}" name)
      (jazz.load-module 'core.library)
      (jazz.load-module 'core.module.builder)
      (if build
          (build descriptor)
        (jazz.build-product-descriptor descriptor)))))


(define (jazz.build-product-descriptor descriptor)
  (jazz.update-product-descriptor descriptor)
  (let ((build (jazz.product-descriptor-build descriptor)))
    (if build
        (for-each (lambda (obj)
                    (if (%%symbol? obj)
                        (jazz.build-executable obj)
                      (apply jazz.build-executable obj)))
                  build))))


(define (jazz.make-product name)
  (let ((install jazz.kernel-install)
        (platform jazz.kernel-platform)
        (made '()))
    (define (install-file path)
      (%%string-append install path))
    
    (define (jazz-path)
      (case platform
        ((windows)
         (install-file "jazz"))
        (else
         "./jazz")))
    
    (define (build name)
      (jazz.call-process (jazz-path) (%%list "-:dq-" "-build" (%%symbol->string name)) install))
    
    (define (make name)
      (if (%%not (%%memq name made))
          (let ((descriptor (jazz.get-product-descriptor name)))
            (let ((dependencies (jazz.product-descriptor-dependencies descriptor)))
              (for-each make-dependencies dependencies)
              (make-parallel dependencies)
              (build name)
              (set! made (%%cons name made))))))
    
    (define (make-parallel names)
      (for-each make names)
      #; ;; wait buggy
      (let ((port (open-vector))
            (parallel (jazz.parallel-build-processes?))
            (running 0))
        (for-each (lambda (name)
                    (if (%%not (%%memq name made))
                        (begin
                          (let iter ()
                            (if (%%fx>= running parallel)
                                (begin
                                  (read port)
                                  (set! running (%%fx- running 1))
                                  (iter))))
                          (set! running (%%fx+ running 1))
                          (thread-start!
                            (make-thread
                              (lambda ()
                                (make name)
                                (write name port)))))))
                  names)))
    
    (define (make-dependencies name)
      (let ((descriptor (jazz.get-product-descriptor name)))
        (for-each make (jazz.product-descriptor-dependencies descriptor))))
    
    (make name)))


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
          (call-with-input-file (list path: pathname eol-encoding: 'cr-lf)
            (lambda (input)
              (let ((form (read input)))
                (let ((name (%%cadr form))
                      (version-form (%%assq 'version (%%cddr form)))
                      (digest-form (%%assq 'digest (%%cddr form))))
                  (let (;; test is for backward compatibility and could be removed in the future
                        (version (if version-form (%%cadr version-form) #f))
                        (hash (%%cadr digest-form))
                        (cached-time (%%car (%%cddr digest-form)))
                        (cached-identical? (%%cadr (%%cddr digest-form))))
                    (%%make-manifest name version (%%make-digest hash cached-time cached-identical?)))))))
        #f))))


(define (jazz.save-manifest resource manifest)
  (let ((name (%%manifest-name manifest))
        (version (%%manifest-version manifest))
        (digest (%%manifest-digest manifest)))
    (call-with-output-file (jazz.resource-pathname resource)
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
        (%%load pathname (lambda rest #f) #f #t quiet?))
      (void)))
  
  (else
    (define (jazz.load pathname . rest)
      (load pathname))))


(define jazz.load-indent
  (make-parameter 0))


(define (jazz.load-resource resource . rest)
  (let ((quiet? (if (null? rest) #f (car rest))))
    (jazz.with-verbose (jazz.load-verbose?) "loading" (jazz.resource-package-pathname resource)
      (lambda ()
        (jazz.load (jazz.resource-pathname resource) quiet?)))))


(define (jazz.with-verbose flag action path proc)
  (define (verbose-load)
    (display (make-string (jazz.load-indent) #\space))
    (display "; ")
    (display action)
    (display " ")
    (display path)
    (display "...")
    (newline)
    (force-output))
  
  (define (verbose-done)
    (display (make-string (jazz.load-indent) #\space))
    (display "; done ")
    (display "...")
    (newline)
    (force-output))
  
  (if flag
      (begin
        (verbose-load)
        (let ((result
                (parameterize ((jazz.load-indent (+ (jazz.load-indent) 2)))
                  (proc))))
          (if (jazz.done-verbose?)
              (verbose-done))
          result))
    (proc)))


(define jazz.walk-for
  (make-parameter #f))


(define (jazz.load-module-src/bin module-name)
  (jazz.with-module-src/bin module-name #f
    (lambda (src bin bin-uptodate?)
      (parameterize ((jazz.requested-module-name module-name)
                     (jazz.requested-module-resource (if bin-uptodate? bin src))
                     (jazz.walk-for 'load))
        (cond (bin-uptodate?
               (let ((quiet? (or (%%not src) (%%string=? (%%resource-extension src) "jazz"))))
                 (jazz.load-resource bin quiet?)))
              (src
               (if (jazz.warn-interpreted?)
                   (jazz.feedback "Warning: Loading {a} interpreted" module-name))
               (jazz.with-extension-reader (%%resource-extension src)
                 (lambda ()
                   (jazz.load-resource src))))
              (else
               (jazz.error "Unable to find module: {s}" module-name)))))))


;;;
;;;; Build
;;;


(define (jazz.resource-build-dir resource)
  (let ((package (%%resource-package resource))
        (dir (jazz.pathname-dir (%%resource-path resource))))
    (jazz.repository-pathname jazz.Bin-Repository
      (if dir
          (%%string-append (%%package-modules-path package) "/" dir)
        (%%package-modules-path package)))))


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


(define jazz.Load-Mutex
  (make-mutex 'load))

(define jazz.Load-Thread
  #f)

(define jazz.Load-Stack
  '())


(define jazz.requested-module-name
  (make-parameter #f))

(define jazz.requested-module-resource
  (make-parameter #f))


(define (jazz.get-load-mutex)
  jazz.Load-Mutex)

(define (jazz.get-load-stack)
  jazz.Load-Stack)


(define (jazz.push-load-stack mode module-name)
  (set! jazz.Load-Stack (cons (cons mode module-name) jazz.Load-Stack)))


(define (jazz.pop-load-stack)
  (set! jazz.Load-Stack (cdr jazz.Load-Stack)))


(define (jazz.call-with-load-lock thunk)
  (if (%%eq? jazz.Load-Thread (current-thread))
      (thunk)
    (dynamic-wind
      (lambda ()
        (mutex-lock! jazz.Load-Mutex)
        (set! jazz.Load-Thread (current-thread)))
      thunk
      (lambda ()
        (set! jazz.Load-Thread #f)
        (mutex-unlock! jazz.Load-Mutex)))))


(define (jazz.module-loaded? module-name)
  (%%eq? (jazz.get-environment-module module-name) jazz.Loaded-State))


(define (jazz.load-module module-name)
  (let ((module-state (jazz.get-environment-module module-name)))
    (if (not (%%eq? module-state jazz.Loaded-State))
        (jazz.call-with-load-lock ; module-state might change while suspended
          (lambda ()
            (let ((module-state (jazz.get-environment-module module-name)))
              (cond ((%%eq? module-state jazz.Loading-State)
                     (jazz.error "Circular loading of module: {s}" module-name))
                    ((%%eq? module-state jazz.Unloaded-State)
                     (dynamic-wind
                       (lambda ()
                         (jazz.set-environment-module module-name jazz.Loading-State)
                         (jazz.push-load-stack ':load module-name))
                       (lambda ()
                         (jazz.load-module-src/bin module-name)
                         (jazz.set-environment-module module-name jazz.Loaded-State))
                       (lambda ()
                         (jazz.pop-load-stack)
                         (if (%%eq? (jazz.get-environment-module module-name) jazz.Loading-State)
                             (jazz.set-environment-module module-name jazz.Unloaded-State))))))))))))


(define (jazz.unload-module module-name)
  (if (mutex-lock! jazz.Load-Mutex)
      (begin
        (jazz.set-environment-module module-name jazz.Unloaded-State)
        (mutex-unlock! jazz.Load-Mutex))
    ;; reacquire mutex
    (jazz.unload-module module-name)))


(define (jazz.reload-module module-name)
  (jazz.unload-module module-name)
  (jazz.load-module module-name))


(jazz.define-variable jazz.for-each-submodule)


;;;
;;;; Reader
;;;


(define jazz.Extension-Readers
  (%%make-table test: equal?))


(define (jazz.get-extension-reader extension)
  (%%table-ref jazz.Extension-Readers extension #f))


(define (jazz.with-extension-reader extension thunk)
  (let ((readtable-getter (jazz.get-extension-reader extension)))
    (if readtable-getter
        (parameterize ((current-readtable (readtable-getter)))
          (thunk))
      (thunk))))


(define (jazz.register-reader-extension extension readtable-getter)
  (%%table-set! jazz.Extension-Readers extension readtable-getter))


;;;
;;;; Scheme
;;;


(define jazz.scheme-readtable
  (%%current-readtable))

(jazz.register-reader-extension "scm"
  (lambda ()
    jazz.scheme-readtable))
