;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Kernel Unit
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
;;;    Stephane Le Cornec
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


(block kernel.unit


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
  (define (parse-format rest proc)
    (if (%%string? (%%car rest))
        (proc ':string (%%car rest) (%%cdr rest))
      (proc (%%car rest) (%%cadr rest) (%%cddr rest))))
  
  (define (format-to output fmt-string arguments)
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
  
  (parse-format rest
    (lambda (port fmt-string arguments)
      (case port
        ((:string)
         (let ((output (open-output-string)))
           (format-to output fmt-string arguments)
           (get-output-string output)))
        (else
         (format-to port fmt-string arguments))))))


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
  (%%apply error rest))


(define (jazz.raise-system-error fmt-string . rest)
  (let ((error-string (apply jazz.format fmt-string rest)))
    (error error-string)))


(jazz.define-variable jazz.error
  jazz.raise-system-error)


(define (jazz.primitive-type-error num type proc args)
  (error (jazz.format "(Argument {a}) {a} expected :" num type) (%%cons proc args)))


;;;
;;;; List
;;;


(define (jazz.some? predicate lst)
  (declare (proper-tail-calls))
  (let iter ((scan lst))
    (if (%%null? scan)
        #f
      (or (predicate (%%car scan))
          (iter (%%cdr scan))))))


(define (jazz.every? predicate lst)
  (declare (proper-tail-calls))
  (let iter ((scan lst))
    (or (%%null? scan)
        (and (predicate (%%car scan))
             (iter (%%cdr scan))))))


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


;;;
;;;; Pathname
;;;


(cond-expand
  (gambit
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
;;;; Marker
;;;


(define (jazz.marker? obj)
  (or (%%eq? obj #!optional)
      (%%eq? obj #!key)
      (%%eq? obj #!rest)))


;;;
;;;; Product
;;;


(define jazz.kernel-built
  jazz.built)


(define jazz.kernel-install
  (or (and (%%eq? jazz.image 'executable) jazz.executable-directory (jazz.executable-directory))
      (jazz.pathname-normalize jazz.built)))


(define (jazz.install-path filename)
  (if (%%not filename)
      jazz.kernel-install
    (%%string-append jazz.kernel-install filename)))


(define jazz.kernel-source-built
  jazz.source-built)


(define jazz.kernel-source
  ;; kernel always needs source access to build
  (if (or jazz.source-access? (%%not jazz.product))
      (jazz.absolutize-directory jazz.kernel-install jazz.source)
    #f))


(set! jazz.jazz-source jazz.kernel-source)


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


(jazz.define-variable jazz.Repositories
  '())

(jazz.define-variable jazz.Build-Repository
  #f)


(define (jazz.get-repositories)
  jazz.Repositories)

(define (jazz.get-build-repository)
  jazz.Build-Repository)


(define (jazz.prepare-repositories)
  (define (all-repositories build jazz user repositories)
    (define (listify repository)
      (if repository
          (%%list repository)
        '()))
    
    `(,@(if repositories
            (map jazz.load-repository (jazz.split-string repositories #\;))
          '())
      ,@(listify build)
      ,@(listify jazz)
      ,@(listify user)))
  
  (let ((build (jazz.make-repository 'Build (or (jazz.build-repository) jazz.kernel-install) "lib" binary?: #t create?: #t))
        (jazz (jazz.make-repository 'Jazz (or (jazz.jazz-repository) jazz.kernel-source) "lib"))
        (user (jazz.make-repository 'User (or (jazz.user-repository) "~/jazz_user/") "lib" create?: #t))
        (repositories (jazz.repositories)))
    (set! jazz.Build-Repository build)
    (set! jazz.Repositories (%%append jazz.Repositories (all-repositories build jazz user repositories)))))


(define (jazz.make-repository name directory library-root #!key (binary? #f) (create? #f))
  (define (create-repository repository-file)
    (call-with-output-file (list path: repository-file eol-encoding: (jazz.platform-eol-encoding jazz.kernel-platform))
      (lambda (output)
        (display "(repository " output)
        (display name output)
        (newline output)
        (newline output)
        (if binary?
            (begin
              (display "  (binary? #t)" output)
              (newline output)))
        (display "  (library " output)
        (write library-root output)
        (display "))" output)
        (newline output))))
  
  (define (repository-inexistant)
    (jazz.error "{a} repository is inexistant: {a}" name directory))
  
  (if (or (%%not directory) (%%equal? directory "none"))
      #f
    (let ((directory (jazz.dirname-normalize directory)))
      (let ((repository-file (%%string-append directory jazz.Repository-Filename)))
        (cond ((jazz.file-exists? repository-file)
               (jazz.load-repository directory))
              (create?
               (jazz.create-directories directory)
               (create-repository repository-file)
               (if (jazz.file-exists? repository-file)
                   (jazz.load-repository directory)
                 (repository-inexistant)))
              (else
               (repository-inexistant)))))))


(define (jazz.load-repository directory #!key (error? #t))
  (define (load-repository directory)
    (let ((repository-file (%%string-append directory jazz.Repository-Filename)))
      (call-with-input-file (%%list path: repository-file eol-encoding: 'cr-lf)
        (lambda (input)
          (let ((form (read input)))
            (let ((name (%%cadr form))
                  (alist (%%cddr form)))
              (let ((directory (jazz.pathname-normalize directory))
                    (binary-pair (%%assq 'binary? alist))
                    (library-pair (%%assq 'library alist)))
                (let ((binary? (if binary-pair (%%cadr binary-pair) #f))
                      (library-root (if library-pair (%%cadr library-pair) #f)))
                  (let ((library-directory (if (%%not library-root) directory (%%string-append directory library-root "/"))))
                    (%%make-repository name directory library-root library-directory binary?))))))))))
  
  (define (repository-inexistant)
    (jazz.error "Repository is inexistant: {a}" directory))
  
  (let ((directory (jazz.dirname-normalize directory)))
    (cond ((jazz.directory-exists? directory)
           (load-repository directory))
          (error?
           (repository-inexistant))
          (else
           #f))))


(define (jazz.install-repository directory)
  (let ((repository (jazz.load-repository directory)))
    (set! jazz.Repositories (%%append jazz.Repositories (%%list repository)))
    (if jazz.setup-repositories-called?
        (jazz.setup-repository repository))
    repository))


(define (jazz.install-repository-if-exists directory)
  (if (jazz.directory-exists? directory)
      (jazz.install-repository directory)))


(define (jazz.uninstall-repository repository)
  (set! jazz.Repositories (%%remove repository jazz.Repositories)))


(define (jazz.find-repository name)
  (let iter ((repositories jazz.Repositories))
    (if (%%null? repositories)
        #f
      (let ((repository (%%car repositories)))
        (if (%%eq? (%%repository-name repository) name)
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
  (%%string-append (%%repository-library-directory repository)
                   path))


(define (jazz.repository-packages-table repository)
  (or (%%repository-packages-table repository)
      (let ((table (%%make-table test: eq?)))
        (%%repository-packages-table-set! repository table)
        (jazz.repository-install-packages repository)
        table)))


(jazz.define-variable jazz.setup-repositories-called?
  #f)


(define (jazz.setup-repositories)
  (for-each jazz.setup-repository jazz.Repositories)
  (set! jazz.setup-repositories-called? #t))


(define (jazz.setup-repository repository)
  (let ((table (jazz.repository-packages-table repository)))
    (jazz.iterate-table-safe table
      (lambda (name package)
        (jazz.setup-package package)))))


(define (jazz.repository-packages repository)
  (let ((table (jazz.repository-packages-table repository))
        (packages '()))
    (jazz.iterate-table table
      (lambda (name package)
        (set! packages (%%cons package packages))))
    packages))


(define (jazz.repository-find-package repository package-name)
  (%%table-ref (jazz.repository-packages-table repository) package-name #f))


(define (jazz.repository-install-packages repository)
  (define (repository-discover-packages repository)
    (let ((table (%%repository-packages-table repository)))
      (define (discover-packages parent library-directory packages)
        (if (jazz.directory-exists? library-directory)
            (let iter ((dirnames (jazz.directory-directories library-directory))
                       (packages packages))
                 (if (%%null? dirnames)
                     packages
                   (let ((dirname (%%car dirnames)))
                     (let ((directory (%%string-append library-directory dirname "/")))
                       (let ((package-pathname (%%string-append directory jazz.Package-Filename)))
                         (if (jazz.file-exists? package-pathname)
                             (let ((package-name (%%string->symbol dirname)))
                               (if (%%table-ref table package-name #f)
                                   (iter (%%cdr dirnames) packages)
                                 (let ((package (jazz.load-package repository parent package-name package-pathname)))
                                   (iter (%%cdr dirnames) (%%cons package (let ((library-path (%%package-library-path package)))
                                                                            (if library-path
                                                                                (let ((library-directory (jazz.repository-pathname (%%package-repository package) (%%string-append library-path "/"))))
                                                                                  (discover-packages package library-directory packages))
                                                                              packages)))))))
                           (iter (%%cdr dirnames) packages)))))))
          packages))
      
      (discover-packages #f (%%repository-library-directory repository) '())))
  
  (let ((table (%%repository-packages-table repository))
        (packages (repository-discover-packages repository)))
    (for-each (lambda (package)
                (%%table-set! table (%%package-name package) package))
              packages)
    packages))


(define (jazz.repository-add-package repository package)
  (let ((table (jazz.repository-packages-table repository)))
    (%%table-set! table (%%package-name package) package)))


(define (jazz.repository-remove-package repository package)
  (let ((table (jazz.repository-packages-table repository)))
    (%%table-clear table (%%package-name package))))


(define (jazz.load-package repository parent package-name package-pathname)
  (call-with-input-file (%%list path: package-pathname eol-encoding: 'cr-lf)
    (lambda (input)
      (let ((form (read input)))
        (let ((name (%%cadr form))
              (alist (%%cddr form)))
          (if (%%eq? name package-name)
              (let ((library (%%assq 'library alist))
                    (root (%%assq 'root alist))
                    (install (%%assq 'install alist))
                    (char-encoding (%%assq 'char-encoding alist))
                    (products (%%assq 'products alist))
                    (profiles (%%assq 'profiles alist))
                    (project (%%assq 'project alist)))
                (let ((package
                        (jazz.make-package repository name parent
                          (if library (%%cadr library) #f)
                          (if root (%%cadr root) #f)
                          (if install (%%cadr install) #f)
                          (if char-encoding (%%cadr char-encoding) #f)
                          (if products (%%cdr products) '())
                          (if profiles (%%cdr profiles) '())
                          (if project (%%cadr project) #f))))
                  (jazz.cache-package-roots package)
                  package))
            (jazz.error "Package at {s} is defining: {s}" package-pathname name)))))))


(define (jazz.create-build-package package)
  (let* ((name (%%package-name package))
         (parent (%%package-parent package))
         (bin-parent (if parent (jazz.create-build-package parent) #f))
         (dir (%%string-append (if parent (%%string-append (%%package-library-path parent) "/") "") (%%symbol->string name) "/"))
         (path (%%string-append dir jazz.Package-Filename))
         (src (jazz.repository-pathname (%%package-repository package) path))
         (dst (jazz.repository-pathname jazz.Build-Repository path)))
    (define (load-package)
      (let ((package (jazz.load-package jazz.Build-Repository bin-parent name dst)))
        (%%table-set! (%%repository-packages-table jazz.Build-Repository)
                      name
                      package)
        package))
    
    (if (and (jazz.file-exists? dst) (>= (jazz.file-modification-time dst) (jazz.file-modification-time src)))
        (or (jazz.repository-find-package jazz.Build-Repository name)
            (load-package))
      (begin
        (jazz.create-directories (jazz.repository-pathname jazz.Build-Repository dir))
        (if (jazz.file-exists? dst)
            (jazz.file-delete dst))
        (jazz.file-copy src dst)
        (load-package)))))


(define (jazz.setup-package package)
  (let ((install (%%package-install package)))
    (if install
        (jazz.load-unit install))))


(define (jazz.inspect-install)
  (define (inspect-path path)
    `(:path ,path ,(path-expand path)))
  
  (define (inspect-repository repository)
    `(:repository
      ,(%%repository-name repository)
      ,(%%repository-library-directory repository)
      ,@(map inspect-package (jazz.repository-packages repository))))
  
  (define (inspect-package package)
    `(:package
      ,(%%package-name package)
      ,(%%package-units-root package)
      ,(%%package-units-path package)))
  
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


(define (jazz.make-package repository name parent library-root units-root install char-encoding products profiles project)
  (let ((library-path (if (%%not library-root)
                          #f
                        (%%string-append (%%symbol->string name) "/" library-root)))
        (units-path (if (%%not units-root)
                        (%%symbol->string name)
                      (%%string-append (%%symbol->string name) "/" units-root))))
    (%%make-package repository name parent library-root library-path units-root units-path install char-encoding products profiles project)))


(define (jazz.package-root package)
  (let ((parent (%%package-parent package)))
    (%%string-append (let ((library-root (%%repository-library-root (%%package-repository package))))
                       (if library-root
                           (%%string-append library-root "/")
                         ""))
                     (if parent (%%string-append (%%package-library-path parent) "/") "")
                     (%%symbol->string (%%package-name package)))))


(define (jazz.package-pathname package path)
  (jazz.repository-pathname (%%package-repository package)
                            (%%string-append (%%symbol->string (%%package-name package)) "/" path)))


(define (jazz.package-root-pathname package path)
  (jazz.relocate-package-pathname (%%package-repository package) package path))


(define (jazz.relocate-package-pathname repository package path)
  (let ((parent (%%package-parent package)))
    (jazz.repository-pathname repository
      (%%string-append (if parent (%%string-append (%%package-library-path parent) "/") "")
                       (%%package-units-path package)
                       "/"
                       (or path "")))))


(define (jazz.iterate-packages binary? proc)
  (let iter-repo ((repositories jazz.Repositories))
    (if (%%not (%%null? repositories))
        (let ((repository (%%car repositories)))
          (if (%%neq? binary? (%%repository-binary? repository))
              (iter-repo (%%cdr repositories))
            (let iter ((packages (jazz.repository-packages repository)))
              (if (%%null? packages)
                  (iter-repo (%%cdr repositories))
                (let ((package (%%car packages)))
                  (proc package)
                  (iter (%%cdr packages))))))))))


(define (jazz.find-resource pathname)
  (let iter-repo ((repositories jazz.Repositories))
    (if (%%null? repositories)
        #f
      (let iter ((packages (jazz.repository-packages (%%car repositories))))
        (if (%%null? packages)
            (iter-repo (%%cdr repositories))
          (let ((package (%%car packages)))
            (let ((package-pathname (jazz.package-root-pathname package "")))
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


(define (jazz.descendant-unit? unit-name descendant-name)
  (let ((unit (%%symbol->string unit-name))
        (descendant (%%symbol->string descendant-name)))
    (let ((unit-length (%%string-length unit))
          (descendant-length (%%string-length descendant)))
      (and (%%fx> descendant-length unit-length)
           (%%string=? (%%substring descendant 0 unit-length) unit)
           (%%eqv? (%%string-ref descendant unit-length) #\.)))))


(define (jazz.find-pathname-unit pathname)
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
      (let ((repository (%%car repositories)))
        (if (%%repository-binary? repository)
            (iter-repo (%%cdr repositories) profiles)
          (let iter ((packages (jazz.repository-packages repository)) (profiles profiles))
            (if (%%null? packages)
                (iter-repo (%%cdr repositories) profiles)
              (let ((package (%%car packages)))
                (let ((package-profiles (%%package-profiles package)))
                  (iter (%%cdr packages) (%%append (map (lambda (package-profile)
                                                          (%%cons package package-profile))
                                                        package-profiles)
                                                   profiles)))))))))))


(define (jazz.make-profile name unit-name)
  `(,name (unit ,unit-name)))


(define (jazz.profile-name profile)
  (%%car profile))

(define (jazz.profile-title profile)
  (%%symbol->string (jazz.profile-name profile)))

(define (jazz.profile-appl profile)
  (let ((pair (%%assq 'appl (%%cdr profile))))
    (if (%%not pair)
        (jazz.error "Unable to find appl in profile: {s}" profile)
      (%%cdr pair))))

(define (jazz.profile-unit profile)
  (let ((pair (%%assq 'unit (%%cdr profile))))
    (if (%%not pair)
        (jazz.error "Unable to find unit in profile: {s}" profile)
      (%%cadr pair))))


;;;
;;;; Unit
;;;


(define (lower-case-unit-name? unit-name)
  (define (first-char-of-last-name str)
    (let loop ((offset (%%fx- (%%string-length str) 1)))
         (let ((ch (%%string-ref str offset)))
           (cond
             ((%%char=? #\. (%%string-ref str offset))
              (if (%%fx< (%%fx+ offset 1) (%%string-length str))
                  (%%string-ref str (%%fx+ 1 offset))
                (jazz.error "unit name {a} ends with a ." str)))
             ((%%fx= offset 0)
              ch)
             (else
              (loop (%%fx- offset 1)))))))
  
  (let ((first-char (first-char-of-last-name (%%symbol->string unit-name))))
    (and (%%char<=? #\a first-char) (%%char<=? first-char #\z))))


(define (jazz.find-unit-src unit-name extensions . rest)
  (define (find-src package path)
    (define (try path)
      (define (try-extension extension)
        (if (jazz.file-exists? (jazz.package-root-pathname package (%%string-append path "." extension)))
            (%%make-resource package path extension)
          #f))
      
      (let iter ((extensions (or extensions '("jazz" "scm"))))
           (if (%%null? extensions)
               #f
             (or (try-extension (%%car extensions))
                 (iter (%%cdr extensions))))))
    
    (if (and (jazz.directory-exists? (jazz.package-root-pathname package path))
             (lower-case-unit-name? unit-name))
        (try (%%string-append path "/_" (jazz.pathname-name path)))
      (try path)))
  
  (let ((error? (if (%%null? rest) #t (%%car rest))))
    (continuation-capture
      (lambda (return)
        (let ((path (jazz.name->path unit-name)))
          (for-each (lambda (package)
                      (let ((src (find-src package path)))
                        (if src
                            (continuation-return return src))))
                    (jazz.cached-packages jazz.*source-packages-cache* unit-name))
          (jazz.iterate-packages #f
            (lambda (package)
              (let ((src (find-src package path)))
                (if src
                    (begin
                      (jazz.cache-package jazz.*source-packages-cache* unit-name package)
                      #; ;; test
                      (jazz.validate-repository-unicity (%%package-repository package)
                                                        unit-name
                                                        (lambda (package)
                                                          (find-src package path)))
                      (continuation-return return src)))))))
        (if error?
            (jazz.error "Unable to find unit: {s}" unit-name)
          #f)))))


(define (jazz.with-unit-resources unit-name extensions proc)
  (define force-interpreted?
    (let ((interpreted? (jazz.force-interpreted?)))
      (cond ((%%boolean? interpreted?)
             interpreted?)
            ((%%symbol? interpreted?)
             (%%eq? unit-name interpreted?))
            (else
             (%%memv unit-name interpreted?)))))
  
  (define (find-unit-binaries src)
    (define (find package path extension)
      (define (try path)
        ;; we only test .o1 and let gambit find the right file by returning no extension when found
        (if (jazz.file-exists? (jazz.package-root-pathname package (%%string-append path extension)))
            (%%make-resource package path #f)
          #f))
      
      (if (and (jazz.directory-exists? (jazz.package-root-pathname package path))
               (lower-case-unit-name? unit-name))
          (try (%%string-append path "/_" (jazz.pathname-name path)))
        (try path)))
    
    (define (find-uptodate package path)
      (let ((obj (find package path ".o"))
            (bin (find package path ".o1")))
        (let ((manifest (let ((obj/bin (or obj bin)))
                          (and obj/bin
                               (jazz.load-updated-manifest
                                 unit-name
                                 (jazz.digest-pathname (%%resource-package obj/bin) obj/bin)
                                 (jazz.manifest-pathname (%%resource-package obj/bin) obj/bin)
                                 (and src (jazz.resource-pathname src)))))))
          (let ((uptodate? (or (%%not src)
                               (and manifest
                                    (jazz.manifest-uptodate? (jazz.resource-pathname src) manifest)
                                    (%%not (jazz.manifest-needs-rebuild? manifest))))))
            (if uptodate?
                (values obj
                        (if force-interpreted? #f bin)
                        manifest)
              (values #f #f #f))))))
    
    (continuation-capture
      (lambda (return)
        (let ((path (jazz.name->path unit-name)))
          (for-each (lambda (package)
                      (receive (obj bin manifest) (find-uptodate package path)
                        (if manifest
                            (continuation-return return (values obj bin manifest)))))
                    (jazz.cached-packages jazz.*binary-packages-cache* unit-name))
          (jazz.iterate-packages #t
            (lambda (package)
              (receive (obj bin manifest) (find-uptodate package path)
                (if manifest
                    (begin
                      (jazz.cache-package jazz.*binary-packages-cache* unit-name package)
                      #; ;; test
                      (jazz.validate-repository-unicity (%%package-repository package)
                                                        unit-name
                                                        (lambda (package)
                                                          (find-bin package path)))
                      (continuation-return return (values obj bin manifest))))))))
        (values #f #f #f))))
  
  (let ((src (jazz.find-unit-src unit-name extensions #f))
        (image-unit (jazz.get-image-unit unit-name)))
    (receive (obj bin manifest) (find-unit-binaries src)
      (let ((obj-uptodate? (if obj #t #f))
            (bin-uptodate? (if bin #t #f))
            (lib-uptodate?
              (and image-unit
                   (or (%%not (or src bin))
                       ; validate lib against manifest
                       (and manifest
                            (jazz.image-unit-uptodate? image-unit src manifest)
                            (%%not (jazz.manifest-needs-rebuild? manifest))
                            (%%not force-interpreted?))
                       ; validate lib against source
                       (and src
                            (jazz.image-unit-uptodate-src? image-unit src)
                            (%%not force-interpreted?))))))
        (proc src
              obj
              bin
              (if image-unit (%%image-unit-load-proc image-unit) #f)
              obj-uptodate?
              bin-uptodate?
              lib-uptodate?
              manifest)))))


(define (jazz.unit-status unit-name)
  (jazz.with-unit-resources unit-name #f
    (lambda (src obj bin load-proc obj-uptodate? bin-uptodate? lib-uptodate? manifest)
      (let ((loaded? (jazz.unit-loaded? unit-name)))
        (display "unit ") (display unit-name) (newline)
        (if src
            (begin
              (display "source hash: ")
              (display (digest-file (jazz.resource-pathname src) 'sha-1))
              (newline))
          (begin (display "source file not found") (newline)))
        (display "obj-uptodate? ") (display obj-uptodate?) (newline)
        (display "bin-uptodate? ") (display bin-uptodate?) (newline)
        (display "lib-uptodate? ") (display lib-uptodate?) (newline)
        (display "loaded? ") (display loaded?) (newline)))))


(define (jazz.unit-uptodate-binary? unit-name)
  (jazz.with-unit-resources unit-name #f
    (lambda (src obj bin lib obj-uptodate? bin-uptodate? lib-uptodate? manifest)
      (or bin-uptodate? lib-uptodate?))))


(define (jazz.image-unit-uptodate? image-unit src manifest)
  (let ((digest (jazz.find-source-digest (jazz.resource-pathname src) manifest)))
    (and digest (%%string=? (%%image-unit-compile-time-hash image-unit) (%%digest-hash digest)))))


(define (jazz.image-unit-uptodate-src? image-unit src)
  (let ((source-hash (digest-file (jazz.resource-pathname src) 'sha-1)))
    (%%string=? (%%image-unit-compile-time-hash image-unit) source-hash)))


(define (jazz.validate-repository-unicity repository unit-name proc)
  (define (repository-unique? repository proc)
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
  
  (if (%%not (repository-unique? repository proc))
      (jazz.error "Found duplicate resource in {a} repository: {s}"
                  (or (%%repository-name repository) "anonymous")
                  unit-name)))


;;;
;;;; Cache
;;;


;; This is a cache of
;;   - every subdirectory of level 2 and
;;   - every subdirectory of level 1 that contains files
;; It associates the directory name as a symbol to a list of packages containing it
;; Stopping at level 2 is a heuristic decision to minimize cache size and the need to refresh
;; it while going deep enough so directory names are unique in a high percentage of cases


(define jazz.*binary-packages-cache*
  (%%make-table test: eq?))

(define jazz.*source-packages-cache*
  (%%make-table test: eq?))


(define (jazz.cache-package cache unit-name package)
  (jazz.with-cached-prefix unit-name
    (lambda (prefix singleton-prefix)
      (let ((packages (%%table-ref cache prefix '())))
        (if (%%not (%%memq package packages))
            (%%table-set! cache prefix (%%cons package packages)))))))


(define (jazz.cached-packages cache unit-name)
  (jazz.with-cached-prefix unit-name
    (lambda (prefix singleton-prefix)
      (or (%%table-ref cache prefix #f)
          (if singleton-prefix
              (%%table-ref cache singleton-prefix '())
            '())))))


;; return as symbols the first 1 or 2 parts and the first part if there exactly 2 parts
(define (jazz.with-cached-prefix unit-name proc)
  (let ((name (%%symbol->string unit-name)))
    (let ((first-period (jazz.string-find name #\.)))
      (if first-period
          (let ((second-period (jazz.string-find name #\. (%%fx+ first-period 1))))
            (if second-period
                (proc (%%string->symbol (%%substring name 0 second-period)) #f)
              (proc unit-name (%%string->symbol (%%substring name 0 first-period)))))
        (proc unit-name #f)))))


;; cache every subdirectory of level 2 and every subdirectory of level 1 that contains files
(define (jazz.cache-package-roots package)
  (let ((cache (if (%%repository-binary? (%%package-repository package))
                   jazz.*binary-packages-cache*
                 jazz.*source-packages-cache*))
        (toplevel-dir (jazz.package-root-pathname package "")))
    (if (jazz.directory-exists? toplevel-dir)
        (for-each (lambda (first-part)
                    (let ((first-dir (string-append toplevel-dir first-part "/")))
                      (if (jazz.directory-exists? first-dir)
                          (let ((has-files? #f))
                            (for-each (lambda (second-part)
                                        (let ((second-path (%%string-append first-dir second-part)))
                                          (case (jazz.pathname-type second-path)
                                            ((regular) (set! has-files? #t))
                                            ((directory) (let ((unit-name (%%string->symbol (string-append first-part "." second-part))))
                                                           (jazz.cache-package cache unit-name package))))))
                                      (jazz.directory-content first-dir))
                            (if has-files?
                                (let ((unit-name (%%string->symbol first-part)))
                                  (jazz.cache-package cache unit-name package)))))))
                  (jazz.directory-directories toplevel-dir)))))


;;;
;;;; Debug
;;;


(define (jazz.load-debuggee)
  (jazz.load-debuggee-units)
  (jazz.load-unit 'jazz.debuggee.setup))


(define (jazz.load-debuggee-units)
  (jazz.load-unit 'core.module)
  (jazz.load-unit 'jazz)
  (jazz.load-unit 'jazz.debuggee)
  (jazz.load-unit 'jazz.debuggee.Debuggee-Frame)
  (jazz.load-unit 'jazz.debuggee.Debuggee-Process)
  (jazz.load-unit 'jazz.debuggee.Debuggee-Stop)
  (jazz.load-unit 'jazz.debuggee.Debuggee-Thread)
  (jazz.load-unit 'jazz.debuggee.stub)
  (jazz.load-unit 'jazz.debugger.jazz.stub)
  (jazz.load-unit 'jazz.debugger.jazz.stub-autoload))


;;;
;;;; Product
;;;


(define (jazz.find-product-descriptor name)
  (let ((binary-package #f)
        (binary-descriptor #f))
    (let iter-repo ((repositories jazz.Repositories))
      (if (%%null? repositories)
          (values binary-package binary-descriptor)
        (let ((repository (%%car repositories)))
          (let iter ((packages (jazz.repository-packages (%%car repositories))))
            (if (%%null? packages)
                (iter-repo (%%cdr repositories))
              (let ((package (%%car packages)))
                (let ((pair (%%assq name (%%package-products package))))
                  (if pair
                      (let ((alias (jazz.product-descriptor-alias pair)))
                        (cond (alias
                               (jazz.find-product-descriptor alias))
                              ((%%repository-binary? repository)
                               (set! binary-package package)
                               (set! binary-descriptor pair)
                               (iter (%%cdr packages)))
                              (else
                               (values package pair))))
                    (iter (%%cdr packages))))))))))))


(define (jazz.product-descriptor-name descriptor)
  (%%car descriptor))

(define (jazz.product-descriptor-alias descriptor)
  (let ((pair (%%assq 'alias (%%cdr descriptor))))
    (if pair
        (%%cadr pair)
      #f)))

(define (jazz.product-descriptor-unit descriptor)
  (let ((pair (%%assq 'unit (%%cdr descriptor))))
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

(define (jazz.product-descriptor-test descriptor)
  (let ((pair (%%assq 'test (%%cdr descriptor))))
    (if pair
        (%%cdr pair)
      #f)))

(define (jazz.product-descriptor-update descriptor)
  (let ((pair (%%assq 'update (%%cdr descriptor))))
    (if pair
        (%%cdr pair)
      '())))

(define (jazz.product-descriptor-build descriptor)
  (let ((pair (%%assq 'build (%%cdr descriptor))))
    (if pair
        (%%cdr pair)
      #f)))

(define (jazz.product-descriptor-library descriptor)
  (let ((pair (%%assq 'library (%%cdr descriptor))))
    (if pair
        (%%cdr pair)
      #f)))


(define (jazz.product-descriptor-dependencies descriptor)
  (let ((pair (%%assq 'dependencies (%%cdr descriptor))))
    (if pair
        (%%cdr pair)
      '())))


(define jazz.Products-Table
  (%%make-table test: eq?))

(define jazz.Products-Run-Table
  (%%make-table test: eq?))


(jazz.define-variable jazz.process-name
  #f)

(jazz.define-variable jazz.process-title
  #f)

(jazz.define-variable jazz.process-icon
  #f)

(jazz.define-variable jazz.process-version
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
            (%%symbol->string name)
          #f))))


(define (jazz.register-product name #!key (title #f) (icon #f) (run #f) (test #f) (update #f) (build #f) (library #f))
  (receive (package descriptor) (jazz.find-product-descriptor name)
    (%%table-set! jazz.Products-Table name (%%make-product name title icon run test update build library package descriptor))))


(define (jazz.get-product-descriptor name)
  (receive (package descriptor) (jazz.find-product-descriptor name)
    (if (and package descriptor)
        (values package descriptor)
      (jazz.error "Unable to find product: {s}" name))))


(define (jazz.get-product name)
  (define (get-registered-product name)
    (or (%%table-ref jazz.Products-Table name #f)
        (jazz.error "Unable to find registered product: {s}" name)))

  (receive (package descriptor) (jazz.get-product-descriptor name)
    (let ((name (jazz.product-descriptor-name descriptor)) ; because of aliases
          (unit (jazz.product-descriptor-unit descriptor)))
      (if unit
          (begin
            (jazz.load-unit unit)
            (get-registered-product name))
        (let ((title (jazz.product-descriptor-title descriptor))
              (icon (jazz.product-descriptor-icon descriptor)))
          (%%make-product name title icon
            #f
            #f
            jazz.update-product-descriptor
            jazz.build-product-descriptor
            jazz.build-library-descriptor
            package
            descriptor))))))


(define (jazz.setup-product name)
  (if (%%not jazz.debugger)
      (jazz.get-product name)
    (begin
      (set! jazz.process-name name)
      (jazz.load-debuggee)
      (let ((product (jazz.get-product name)))
        (let ((descriptor (%%product-descriptor product)))
          (set! jazz.process-name name)
          (set! jazz.process-title (or (%%product-title product) (jazz.product-descriptor-title descriptor)))
          (set! jazz.process-icon (or (%%product-icon product) (jazz.product-descriptor-icon descriptor)))
          (jazz.load-unit 'jazz.debuggee.update)
          product)))))


(define (jazz.register-product-run name proc)
  (%%table-set! jazz.Products-Run-Table name proc))


(define (jazz.run-product name)
  (define (run-product-descriptor descriptor)
    (let ((name (jazz.product-descriptor-name descriptor))
          (run (jazz.product-descriptor-run descriptor)))
      (if run
          (begin
            (for-each jazz.load-unit run)
            (let ((proc (get-registered-run name)))
              (proc descriptor)))
        (jazz.error "Product is not runnable: {s}" name))))
  
  (define (get-registered-run name)
    (or (%%table-ref jazz.Products-Run-Table name #f)
        (jazz.error "Unable to find registered run: {s}" name)))
  
  (let ((product (jazz.setup-product name)))
    (let ((run (%%product-run product))
          (descriptor (%%product-descriptor product)))
      (if run
          (run descriptor)
        (run-product-descriptor descriptor)))))


(define (jazz.test-product name)
  (define (test-product-descriptor descriptor)
    (let ((name (jazz.product-descriptor-name descriptor))
          (test (jazz.product-descriptor-test descriptor)))
      (if test
          (for-each jazz.load-unit test)
        (jazz.error "Product is not testable: {s}" name))))
  
  (let ((product (jazz.setup-product name)))
    (let ((test (%%product-test product))
          (descriptor (%%product-descriptor product)))
      (if test
          (test descriptor)
        (test-product-descriptor descriptor)))))


(define (jazz.ill-formed-field-error field-name product-name)
  (lambda ()
    (jazz.error "ill-formed {a} field in product descriptor for product {a}" field-name product-name)))


(define (jazz.cond-expand-each error-proc updates)
  (define (apply-cond-expand exp)
    (cond ((and (%%pair? exp)
                (%%pair? (%%car exp)))
           (let ((clause (%%car exp)))
             (cond ((jazz.feature-satisfied? (%%car clause))
                    (%%cdr clause))
                   ((%%eq? 'else (%%car clause))
                    (%%cdr clause))
                   (else
                    (apply-cond-expand (%%cdr exp))))))
          ((null? exp)
           '())
          (else
           (error-proc))))

  (define (expand-update update)
    (cond ((%%symbol? update)
           (%%list update))
          ((and (%%pair? update)
                (%%eq? 'cond (%%car update)))
           (apply-cond-expand (%%cdr update)))
          (else
           (error-proc))))
  
  (%%apply append (map expand-update updates)))


(define (jazz.update-product name)
  (let ((product (jazz.setup-product name)))
    (let ((update (%%product-update product))
          (descriptor (%%product-descriptor product)))
      (if update
          (update descriptor)
        (jazz.update-product-descriptor descriptor)))))


(define (jazz.update-product-descriptor descriptor)
  (let* ((name (jazz.product-descriptor-name descriptor))
         (update (jazz.cond-expand-each (jazz.ill-formed-field-error "update" name)
                                        (jazz.product-descriptor-update descriptor))))
    (for-each jazz.build-unit update)))


(define (jazz.build-product name)
  (let ((product (jazz.setup-product name)))
    (let ((build (%%product-build product))
          (build-library (%%product-build-library product))
          (descriptor (%%product-descriptor product)))
      (jazz.feedback "make {a}" name)
      (jazz.load-unit 'core.module)
      (jazz.load-unit 'core.unit.builder)
            
      (if build
          (build descriptor)
        (jazz.build-product-descriptor descriptor))
      
      (if (jazz.link-libraries?)
          (if build-library
              (build-library descriptor)
            (jazz.build-library-descriptor descriptor))))))


(define (jazz.build-product-descriptor descriptor #!key (unit #f) (force? #f))
  (define (build-product)
    (jazz.update-product-descriptor descriptor)
    (let ((build (jazz.product-descriptor-build descriptor)))
      (if build
          (for-each (lambda (obj)
                      (if (%%symbol? obj)
                          (jazz.build-image obj)
                        (%%apply jazz.build-image obj)))
                    build))))
  
  (if unit
      (jazz.compile-unit unit force?: force?)
    (build-product)))


(define (jazz.build-library-descriptor descriptor)
  (let ((library (jazz.product-descriptor-library descriptor)))
    (if library
        (jazz.build-library (jazz.product-descriptor-name descriptor) descriptor options: library)
      (jazz.build-library (jazz.product-descriptor-name descriptor) descriptor))))


(define jazz.end-make-marker
  "***end-make***")


(define (jazz.make-product name)
  (let ((subproduct-table (make-table))
        (subproduct-table-mutex (make-mutex 'subproduct-table-mutex))
        (active-processes '())
        (free-processes '())
        (outdated-processes '())
        (process-mutex (make-mutex 'process-mutex))
        (process-condition (make-condition-variable 'process-condition))
        (output-mutex (make-mutex 'output-mutex))
        (stop-build? #f))
    
    (define (grab-build-process)
      (mutex-lock! process-mutex)
      (let ((active-count (%%length active-processes))
            (jobs (or jazz.jobs (jazz.build-jobs))))
        (cond ((%%pair? free-processes)
               (let ((process (%%car free-processes)))
                 (set! free-processes (%%cdr free-processes))
                 (mutex-unlock! process-mutex)
                 process))
              ((%%fx< active-count jobs)
               (let ((process (open-process
                                (list
                                  path: (jazz.install-path "kernel")
                                  arguments: `("-:dq-" "-subbuild"
                                               "-link" ,(%%symbol->string jazz.link)
                                               ,@(if (%%memq 'keep-c jazz.compile-options) `("-keep-c") '())
                                               ,@(if (%%memq 'expansion jazz.compile-options) `("-expansion") '())
                                               ,@(if (jazz.save-emit?) `("-emit") '())
                                               ,@(if (jazz.build-repository) `("-build-repository" ,(jazz.build-repository)) '())
                                               ,@(if (jazz.jazz-repository) `("-jazz-repository" ,(jazz.jazz-repository)) '())
                                               ,@(if (jazz.user-repository) `("-user-repository" ,(jazz.user-repository)) '())
                                               ,@(if (jazz.repositories) `("-repositories" ,(jazz.repositories)) '())
                                               "-jobs" "1")
                                  stdin-redirection: #t
                                  stdout-redirection: #t
                                  stderr-redirection: #f))))
                 (set! active-processes (%%cons process active-processes))
                 (mutex-unlock! process-mutex)
                 process))
              (else
               (mutex-unlock! process-mutex process-condition)
               (grab-build-process)))))
    
    (define (build name)
      (let ((process (grab-build-process)))
        (define (atomic-output line)
          (mutex-lock! output-mutex)
          (display line)
          (newline)
          (force-output)
          (mutex-unlock! output-mutex))
        
        (define (build-process-died)
          (mutex-lock! process-mutex)
          (set! active-processes (jazz.remove process active-processes))
          (set! stop-build? #t)
          (mutex-unlock! process-mutex)
          (condition-variable-signal! process-condition))
        
        (define (build-process-ended product-modified?)
          (mutex-lock! process-mutex)
          (let ((key-product? (%%memq name '(core jazz))))
            (if (and key-product? product-modified?)
                (begin
                  (set! outdated-processes active-processes))))
          (if (%%memq process outdated-processes)
              (begin
                  (set! active-processes (jazz.remove process active-processes))
                  (set! outdated-processes (jazz.remove process outdated-processes))
                  (send-command process #f))
            (set! free-processes (%%cons process free-processes)))
            (mutex-unlock! process-mutex)
            (condition-variable-signal! process-condition))
        
        (if stop-build?
            (build-process-ended #f)
          (begin
            (send-command process name)
            (let iter ((product-modified? #f))
                 (let ((line (read-line process)))
                   (if (%%not (or (eof-object? line) (%%equal? line jazz.end-make-marker)))
                       (let ((count (%%string-length line)))
                         (if (%%fx> count 0)
                             (begin
                               (atomic-output line)
                               (if (and (%%fx>= count 11) (%%string=? (%%substring line 0 11) "; compiling"))
                                   (iter #t)
                                 (iter product-modified?)))
                           (build-process-ended product-modified?)))
                     (build-process-died))))))))
    
    (define (local-make name)
      (for-each (lambda (subname)
                  (if (%%not (%%table-ref subproduct-table subname #f))
                      (begin
                        (make subname)
                        (%%table-set! subproduct-table subname #t))))
                (receive (package descriptor) (jazz.get-product-descriptor name)
                  (jazz.cond-expand-each (jazz.ill-formed-field-error "dependencies" name)
                                         (jazz.product-descriptor-dependencies descriptor))))
      (jazz.build-product name))
    
    (define (remote-make name)
      (for-each thread-join!
                (map (lambda (name)
                       (mutex-lock! subproduct-table-mutex)
                       (let ((thread (or (%%table-ref subproduct-table name #f)
                                         (let ((thread (thread-start! (make-thread (lambda ()
                                                                                     (make name))))))
                                           (%%table-set! subproduct-table name thread)
                                           thread))))
                         (mutex-unlock! subproduct-table-mutex)
                         thread))
                     (receive (package descriptor) (jazz.get-product-descriptor name)
                       (jazz.cond-expand-each (jazz.ill-formed-field-error "dependencies" name)
                                              (jazz.product-descriptor-dependencies descriptor)))))
      (build name))
    
    (define (make name)
      (if (jazz.debug-build?)
          (local-make name)
        (remote-make name)))
    
    (define (send-command process name)
      (write name process)
      (newline process)
      (force-output process))
    
    (parameterize ((current-user-interrupt-handler
                     (lambda ()
                       (set! stop-build? #t))))
      (dynamic-wind
        (lambda () #f)
        (lambda () (make name))
        (lambda () (for-each (lambda (process) (send-command process #f)) active-processes))))))


(define (jazz.subprocess-build-products)
  (parameterize ((current-user-interrupt-handler
                   (lambda ()
                     (display jazz.end-make-marker)
                     (newline)
                     (force-output)
                     #f)))
    (let iter ()
         (let ((product (read)))
           (if product
               (begin
                 (jazz.build-product product)
                 (newline)
                 (force-output)
                 (iter)))))))


;;;
;;;; Resource
;;;


(define jazz.default-char-encoding
  'UTF)


(define (jazz.resource-char-encoding resource)
  (or (%%package-char-encoding (%%resource-package resource))
      jazz.default-char-encoding))


(define (jazz.resource-pathname resource)
  (jazz.package-root-pathname (%%resource-package resource)
    (jazz.resource-package-pathname resource)))


(define (jazz.resource-package-pathname resource)
  (let ((ext (%%resource-extension resource)))
    (if (%%not ext)
        (%%resource-path resource)
      (%%string-append (%%resource-path resource) "." ext))))


(define (jazz.name->path resource-name)
  (jazz.string-replace (%%symbol->string resource-name) #\. #\/))


(define (jazz.path->name resource-name)
  (%%string->symbol (jazz.string-replace resource-name #\/ #\.)))


(define (jazz.binary-with-extension src extension)
  (let* ((bindir (jazz.resource-build-dir src))
         (pathname (jazz.resource-pathname src))
         (name-base (jazz.pathname-base pathname)))
    (string-append bindir name-base extension)))


(define (jazz.probe-numbered-pathname pathname n)
  (let ((candidate (string-append pathname (%%number->string n))))
    (if (%%not (file-exists? candidate))
        candidate
      (jazz.probe-numbered-pathname pathname (%%fx+ n 1)))))


(define (jazz.for-each-numbered-pathname pathname n0 proc)
  (let iter ((n n0))
       (let ((candidate (string-append pathname (%%number->string n))))
         (if (file-exists? candidate)
             (begin
               (iter (%%fx+ n 1))
               (proc candidate))))))


(define (jazz.with-numbered-pathname pathname fresh? n0 proc)
  (let iter ((n n0) (exists? #f))
       (let ((candidate (string-append pathname (%%number->string n))))
         (if (%%not (file-exists? candidate))
             (if fresh?
                 (proc candidate exists?)
               (if exists?
                   (proc (string-append pathname (%%number->string (%%fx- n 1))) exists?)
                 (proc (string-append pathname (%%number->string n0)) exists?)))
           (iter (%%fx+ n 1) #t)))))


(define (jazz.product-library-name-base package product-name)
  (jazz.relocate-product-library-name-base (%%package-repository package) package product-name))


(define (jazz.relocate-product-library-name-base repository package product-name)
  (define (build-dir package)
    (let ((parent (%%package-parent package)))
      (jazz.repository-pathname repository
        (%%string-append (if parent (%%string-append (%%package-library-path parent) "/") "")
                         (%%symbol->string (%%package-name package))))))
  
  (string-append (build-dir package)
                 "/"
                 (%%symbol->string product-name)))


;;;
;;;; Manifest
;;;


(define (jazz.manifest-pathname package resource)
  (jazz.package-root-pathname package
                              (%%string-append (%%resource-path resource)
                                               "."
                                               jazz.Manifest-Extension)))


(define (jazz.digest-pathname package resource)
  (jazz.package-root-pathname package
                              (%%string-append (%%resource-path resource)
                                               "."
                                               jazz.Digest-Extension)))


;;;
;;;; Load
;;;


(define jazz.load-indent
  (make-parameter 0))


(define (jazz.load-resource resource . rest)
  (let ((quiet? (if (%%null? rest) #f (%%car rest))))
    (jazz.with-verbose (jazz.load-verbose?) "loading" (jazz.resource-package-pathname resource)
      (lambda ()
        (jazz.load (%%list
                     path: (jazz.resource-pathname resource)
                     char-encoding: (jazz.resource-char-encoding resource))
                   quiet?)))))


(define (jazz.with-verbose flag action path proc)
  (let ((port (console-port)))
    (define (verbose-load)
      (display (make-string (jazz.load-indent) #\space) port)
      (display "; " port)
      (display action port)
      (display " " port)
      (display path port)
      (display "..." port)
      (newline port)
      (force-output port))
    
    (define (verbose-done)
      (display (make-string (jazz.load-indent) #\space) port)
      (display "; done " port)
      (display "..." port)
      (newline port)
      (force-output port))
    
    (if flag
        (begin
          (verbose-load)
          (let ((result
                  (parameterize ((jazz.load-indent (%%fx+ (jazz.load-indent) 2)))
                    (proc))))
            (if (jazz.done-verbose?)
                (verbose-done))
            result))
      (proc))))


;; #f interpret compile eval
(define jazz.walk-for
  (make-parameter #f))


(define (jazz.load-unit-src/bin unit-name)
  (jazz.with-unit-resources unit-name #f
    (lambda (src obj bin load-proc obj-uptodate? bin-uptodate? lib-uptodate? manifest)
      (parameterize ((jazz.requested-unit-name unit-name)
                     (jazz.requested-unit-resource (if bin-uptodate? bin src)))
        (cond (lib-uptodate?
                (jazz.increment-image-load-counter)
                (jazz.with-verbose (jazz.load-verbose?) "loading" (symbol->string unit-name)
                  (lambda ()
                    (load-proc))))
              (bin-uptodate?
                (jazz.increment-object-load-counter)
                (let ((quiet? (or (%%not src) (let ((ext (%%resource-extension src)))
                                                (and ext (%%string=? ext "jazz"))))))
                  (jazz.load-resource bin quiet?)))
              (src
                (jazz.increment-interpreted-load-counter)
                (let ((warn (jazz.warn-interpreted?)))
                  (if warn
                      (case warn
                        ((error)
                         (jazz.error "Loading {a} interpreted" unit-name))
                        ((stack)
                         (jazz.feedback "Warning: Loading {a} interpreted" unit-name)
                         (pp jazz.Load-Stack))
                        (else
                         (jazz.feedback "Warning: Loading {a} interpreted" unit-name)
                         (if (and (%%pair? warn) (%%memq unit-name warn))
                             (pp jazz.Load-Stack))))))
                (parameterize ((jazz.walk-for 'interpret)
                               (jazz.generate-symbol-for "&")
                               (jazz.generate-symbol-context unit-name)
                               (jazz.generate-symbol-counter 0))
                  (jazz.with-extension-reader (%%resource-extension src)
                    (lambda ()
                      (jazz.load-resource src)))))
              (else
               (jazz.error "Unable to find unit: {s}" unit-name)))))))


;;;
;;;; Build
;;;


(define (jazz.resource-build-dir resource)
  (jazz.relocate-resource jazz.Build-Repository resource))


(define (jazz.relocate-resource repository resource)
  (let ((package (%%resource-package resource))
        (dir (jazz.pathname-dir (%%resource-path resource))))
    (jazz.relocate-package-pathname repository package dir)))


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


(define (jazz.get-environment-unit name)
  (%%table-ref jazz.Environment name jazz.Unloaded-State))


(define (jazz.set-environment-unit name unit)
  (%%table-set! jazz.Environment name unit))


;;;
;;;; Image Units
;;;


(define jazz.Image-Units
  (%%make-table test: eq?))

(define (jazz.get-image-unit name)
  (%%table-ref jazz.Image-Units name #f))

(define (jazz.set-image-unit name load-proc compile-time-hash)
  (%%table-set! jazz.Image-Units name (%%make-image-unit load-proc compile-time-hash)))


;;;
;;;; Unit
;;;


(define jazz.Load-Mutex
  (make-mutex 'load))

(jazz.define-variable jazz.Load-Thread
  #f)

(jazz.define-variable jazz.Load-Stack
  '())


(define jazz.requested-unit-name
  (make-parameter #f))

(define jazz.requested-unit-resource
  (make-parameter #f))


(define jazz.compiled-source
  (make-parameter #f))


(define (jazz.get-load-mutex)
  jazz.Load-Mutex)

(define (jazz.get-load-thread)
  jazz.Load-Thread)

(define (jazz.get-load-stack)
  jazz.Load-Stack)


(define (jazz.push-load-stack mode unit-name)
  (set! jazz.Load-Stack (%%cons (%%cons mode unit-name) jazz.Load-Stack)))


(define (jazz.pop-load-stack)
  (set! jazz.Load-Stack (%%cdr jazz.Load-Stack)))


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


(define (jazz.unit-loaded? unit-name)
  (%%eq? (jazz.get-environment-unit unit-name) jazz.Loaded-State))


(define (jazz.circular-dependency-error unit-name unit-name-list)
  (define (take-until predicate list)
    (let loop ((in list)
               (out '()))
         (if (%%null? in)
             out
           (let ((elem (%%car in)))
             (if (predicate elem)
                 (%%cons elem out)
               (loop (%%cdr in)
                     (%%cons elem out)))))))
  
  (let ((circular-unit-list (append (take-until
                                      (lambda (name) (%%eq? unit-name name))
                                      unit-name-list)
                                    (%%list unit-name))))
    (jazz.error "Circular dependency detected: {a}"
                (jazz.join-strings (map symbol->string circular-unit-list)
                                   " -> "))))


(define (jazz.load-unit unit-name)
  (let ((unit-state (jazz.get-environment-unit unit-name)))
    (if (%%not (%%eq? unit-state jazz.Loaded-State))
        (jazz.call-with-load-lock ; unit-state might change while suspended
          (lambda ()
            (let ((unit-state (jazz.get-environment-unit unit-name)))
              (cond ((%%eq? unit-state jazz.Loading-State)
                     (jazz.circular-dependency-error unit-name (map cdr (jazz.get-load-stack))))
                    ((%%eq? unit-state jazz.Unloaded-State)
                     (dynamic-wind
                       (lambda ()
                         (jazz.set-environment-unit unit-name jazz.Loading-State)
                         (jazz.push-load-stack ':load unit-name))
                       (lambda ()
                         (jazz.load-unit-src/bin unit-name)
                         (jazz.set-environment-unit unit-name jazz.Loaded-State))
                       (lambda ()
                         (jazz.pop-load-stack)
                         (if (%%eq? (jazz.get-environment-unit unit-name) jazz.Loading-State)
                             (jazz.set-environment-unit unit-name jazz.Unloaded-State))))))))))))


(define (jazz.unload-unit unit-name)
  (if (mutex-lock! jazz.Load-Mutex)
      (begin
        (jazz.set-environment-unit unit-name jazz.Unloaded-State)
        (mutex-unlock! jazz.Load-Mutex))
    ;; reacquire mutex
    (jazz.unload-unit unit-name)))


(define (jazz.reload-unit unit-name)
  (jazz.unload-unit unit-name)
  (jazz.load-unit unit-name))


;;;
;;;; Service
;;;


(define jazz.Services
  (%%make-table test: eq?))


(define (jazz.register-service name thunk)
  (%%table-set! jazz.Services name thunk))


(define (jazz.get-service name)
  (let ((symbol/proc (%%table-ref jazz.Services name #f)))
    (if (%%symbol? symbol/proc)
        (begin
          (jazz.load-unit symbol/proc)
          (set! symbol/proc (%%table-ref jazz.Services name #f))))
    (if symbol/proc
        (symbol/proc)
      #f)))


(define (jazz.require-service name)
  (or (jazz.get-service name)
      (error "Unknown service: {s}" name)))


;;;
;;;; Literal
;;;


(define jazz.Literal-Constructors
  (%%make-table test: eq?))


(define (jazz.register-literal-constructor name contructor-name constructor)
  (%%table-set! jazz.Literal-Constructors name (%%cons contructor-name constructor)))


(define (jazz.require-literal-constructor name)
  (or (%%table-ref jazz.Literal-Constructors name #f)
      (jazz.error "Cannot construct literals of type {s}" name)))


(jazz.define-macro (jazz.define-literal name contructor-name)
  (receive (contructor-module ignore) (jazz.split-composite contructor-name)
    `(jazz.register-literal-constructor ',name ',contructor-name
       (lambda (arguments)
         (jazz.load-unit ',contructor-module)
         (%%apply (jazz.global-ref ',contructor-name) arguments)))))


(define (jazz.construct-literal name arguments)
  (let ((constructor (%%cdr (jazz.require-literal-constructor name))))
    (constructor arguments)))


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
    jazz.scheme-readtable)))
