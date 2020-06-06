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
;;;  Portions created by the Initial Developer are Copyright (C) 1996-2018
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


;;;
;;;; Output
;;;


(jazz:define-variable jazz:display display)
(jazz:define-variable jazz:write write)


;;;
;;;; Format
;;;


(define (jazz:format . rest)
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
             (jazz:display (%%car arguments) output)
             (set! arguments (%%cdr arguments)))
            ((s)
             (jazz:write (%%car arguments) output)
             (set! arguments (%%cdr arguments)))
            ((t)
             (jazz:write (%%car arguments) output)
             (set! arguments (%%cdr arguments)))
            ((l)
             (let ((first? #t))
               (for-each (lambda (element)
                           (if first?
                               (set! first? #f)
                             (display " " output))
                           (jazz:display element output))
                         (%%car arguments)))
             (set! arguments (%%cdr arguments)))
            ((%)
             (newline output))
            (else
             (jazz:kernel-error "Unknown format directive:" directive)))))
      
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


(define jazz:system-format
  jazz:format)


;;;
;;;; Exception
;;;


(define jazz:pristine-thread-continuation
  (thread-join!
    (thread-start!
      (%%make-thread
        (lambda ()
          (continuation-capture
            (lambda (cont)
              cont)))))))


;;;
;;;; Error
;;;


(define (jazz:kernel-error . rest)
  (%%apply error rest))


(define (jazz:raise-system-error fmt-string . rest)
  (let ((error-string (apply jazz:format fmt-string rest)))
    (error error-string)))


(jazz:define-variable jazz:error
  jazz:raise-system-error)


(define (jazz:primitive-type-error num type proc args)
  (error (jazz:format "(Argument {a}) {a} expected :" num type) (%%cons proc args)))


;;;
;;;; List
;;;


(define (jazz:some? predicate lst)
  (declare (proper-tail-calls))
  (let iter ((scan lst))
    (if (%%null? scan)
        #f
      (or (predicate (%%car scan))
          (iter (%%cdr scan))))))


(define (jazz:every? predicate lst)
  (declare (proper-tail-calls))
  (let iter ((scan lst))
    (or (%%null? scan)
        (and (predicate (%%car scan))
             (iter (%%cdr scan))))))


(define jazz:kernel-some? jazz:some?)
(define jazz:kernel-every? jazz:every?)


;;;
;;;; Marker
;;;


(define (jazz:marker? obj)
  (or (%%eq? obj #!optional)
      (%%eq? obj #!key)
      (%%eq? obj #!rest)))


;;;
;;;; Product
;;;


(define jazz:kernel-built
  jazz:built)


(define jazz:kernel-path
  (and (%%eq? jazz:image 'executable) jazz:executable-path (jazz:executable-path)))


(define jazz:kernel-install
  (or (and (cond-expand (ios #t) (else (%%eq? jazz:image 'executable))) jazz:executable-directory (jazz:executable-directory))
      (and (jazz:global-bound? '*kernel-install*)
           (jazz:pathname-normalize (jazz:global-ref '*kernel-install*)))
      (if (file-exists? jazz:built)
          (jazz:pathname-normalize jazz:built)
        (error "Unable to determine kernel install"))))


(define jazz:kernel-bundle-contents
  (if jazz:bundle-depth
      (jazz:nth-parent-directory jazz:kernel-install 1)
    #f))


(define jazz:kernel-bundle-root
  (if jazz:bundle-depth
      (jazz:nth-parent-directory jazz:kernel-install (%%fx- jazz:bundle-depth 1))
    #f))


(define jazz:kernel-bundle-install
  (if jazz:bundle-depth
      (jazz:nth-parent-directory jazz:kernel-install jazz:bundle-depth)
    #f))


(define (jazz:install-path filename)
  (if (%%not filename)
      jazz:kernel-install
    (%%string-append jazz:kernel-install filename)))


(define jazz:kernel-source-built
  jazz:source-built)


(define jazz:kernel-source-accessible?
  jazz:source-access?)


(define jazz:kernel-source-access?
  jazz:source-access?)


(define jazz:kernel-jazzini-access?
  jazz:jazzini-access?)


(define jazz:kernel-source
  (if (and jazz:kernel-source-access? jazz:source)
      (jazz:absolutize-directory jazz:kernel-install jazz:source)
    #f))


(set! jazz:jazz-source jazz:kernel-source)


;; quick solution to build needing source
(define (jazz:setup-kernel-source)
  (set! jazz:kernel-source-access? #t)
  (set! jazz:kernel-source (jazz:absolutize-directory jazz:kernel-install jazz:source))
  (set! jazz:jazz-source jazz:kernel-source))


(define jazz:kernel-binary-repositories
  jazz:binary-repositories)


(define jazz:kernel-source-repositories
  jazz:source-repositories)


(define jazz:kernel-windows-homedir
  ;; the build system needs access to home
  (if (%%not jazz:product)
      #f
    jazz:windows-homedir))


(define (jazz:jazz-product)
  jazz:product)


(define (jazz:jazz-profile)
  (jazz:profile))


(define jazz:profile
  (%%make-parameter #f))


;; to enable timing loading of an application
(define jazz:run-loop?
  (%%make-parameter #t))


;;;
;;;; Repository
;;;


(define jazz:Repository-Filename
  ".repository")


(jazz:define-variable jazz:Repositories
  '())

(jazz:define-variable jazz:Build-Repository
  #f)


(define (jazz:repositories-get)
  jazz:Repositories)

(define (jazz:build-repository-get)
  jazz:Build-Repository)


(define (jazz:prepare-repositories)
  (define (all-repositories build)
    (let ((source-access? jazz:kernel-source-access?))
      (let ((jazz (and source-access? (jazz:make-repository 'Jazz "lib" (or (jazz:jazz-repository) jazz:kernel-source))))
            (binary-repositories (or jazz:kernel-binary-repositories '()))
            (source-repositories (or (and source-access? jazz:kernel-source-repositories) '()))
            (binary-dynamic-repositories '()) ;; dynamic-dependencies (or (jazz:dependencies) '())
            (source-dynamic-repositories '()) ;; dynamic-dependencies (or (and source-access? (jazz:dependencies)) '())
            (repositories (jazz:repositories)))
        (define (listify repository)
          (if repository
              (%%list repository)
            '()))
        
        (let ((binary-dynamic-root (jazz:parent-directory (jazz:parent-directory jazz:kernel-install)))
              (source-dynamic-root (jazz:parent-directory (jazz:parent-directory jazz:source))))
          (if (and (or binary-dynamic-root (null? binary-dynamic-repositories))
                   (or source-dynamic-root (null? source-dynamic-repositories)))
              (let ((repositories-list (if repositories
                                           (map jazz:load-repository (jazz:split-string repositories #\;))
                                         '()))
                    (binary-list (jazz:collect (lambda (path)
                                                 (let ((dir (jazz:absolutize-directory jazz:kernel-install path)))
                                                   (jazz:load-repository dir name: 'Binaries error?: #f)))
                                               binary-repositories))
                    (source-list (jazz:collect (lambda (path)
                                                 (let ((dir (jazz:absolutize-directory jazz:kernel-install path)))
                                                   (and dir (jazz:load-repository dir error?: #f))))
                                               source-repositories))
                    #; ;; dynamic-dependencies
                    (dynamic-binary-list (jazz:collect (lambda (entry)
                                                         (let ((name (%%car entry))
                                                               (branch (%%cadr entry)))
                                                           ;; add source dynamic repositories in $SOURCE/$REPO/$BRANCH
                                                           (let ((path (%%string-append source-dynamic-root (jazz:join-strings (list name branch) #\/))))
                                                             (jazz:load-repository path name: name error?: #f))))
                                                       source-dynamic-repositories))
                    #; ;; dynamic-dependencies
                    (dynamic-source-list (jazz:collect (lambda (entry)
                                                         (let ((name (%%car entry))
                                                               (branch (%%cadr entry)))
                                                           ;; add binary dynamic repositories in $BINARY/$REPO/$BRANCH
                                                           (let ((path (%%string-append binary-dynamic-root (jazz:join-strings (list name branch) #\/))))
                                                             (jazz:load-repository path name: name error?: #f))))
                                                       binary-dynamic-repositories))
                    (build-list (if build (%%list build) '()))
                    (jazz-list (if jazz (%%list jazz) '())))
                (append repositories-list binary-list source-list #; dynamic-binary-list #; dynamic-source-list build-list jazz-list))
            (jazz:error "Invalid .dependencies"))))))
  
  (define (build-directory/lib)
    (if (%%eq? jazz:kernel-platform 'mac)
        (cond ((and jazz:kernel-bundle-install (file-exists? (%%string-append jazz:kernel-bundle-install "Libraries/lib")))
               (values jazz:kernel-bundle-install "Libraries/lib"))
              ((and jazz:kernel-bundle-contents (file-exists? (%%string-append jazz:kernel-bundle-contents "Libraries/lib")))
               (values jazz:kernel-bundle-contents "Libraries/lib"))
              (else
               (values (or (jazz:build-repository) jazz:kernel-install) "Libraries/lib")))
      (values (or (jazz:build-repository) jazz:kernel-install) "lib")))
  
  (receive (directory lib) (build-directory/lib)
    (let ((build (jazz:make-repository 'Build lib directory binary?: #t dynamic?: #t)))
      (set! jazz:Build-Repository build)
      (set! jazz:Repositories (%%append jazz:Repositories (all-repositories build))))))


(define (jazz:make-repository name library directory #!key (binary? #f) (dynamic? #f))
  (define (create-repository repository-file)
    (call-with-output-file (list path: repository-file eol-encoding: (jazz:platform-eol-encoding jazz:kernel-platform))
      (lambda (output)
        (display "(repository " output)
        (display name output)
        (if (or binary? library)
            (begin
              (newline output)
              (newline output)))
        (if binary?
            (begin
              (display "  (binary? #t)" output)
              (newline output)))
        (if library
            (begin
              (display "  (library " output)
              (write library output)
              (display ")" output)))
        (display ")" output)
        (newline output))))
  
  (define (repository-form)
    `(repository ,name
       ,@(if binary?
             `((binary? ,binary?))
           '())
       ,@(if library
             `((library ,library))
           '())))
  
  (define (repository-inexistant)
    (jazz:error "{a} repository is inexistant: {a}" name directory))
  
  (if (or (%%not directory) (%%equal? directory "none"))
      #f
    (let ((directory (jazz:dirname-normalize directory)))
      (let ((repository-file (%%string-append directory jazz:Repository-Filename)))
        (cond ((file-exists? repository-file)
               (jazz:load-repository directory))
              (dynamic?
               (if (%%not jazz:product)
                   (begin
                     (jazz:create-directories directory)
                     (create-repository repository-file)
                     (if (file-exists? repository-file)
                         (jazz:load-repository directory)
                       (repository-inexistant)))
                 (jazz:load-repository-form directory (repository-form))))
              (else
               (repository-inexistant)))))))


(define (jazz:load-repository directory #!key (name #f) (error? #t))
  (define (load-repository directory)
    (if (file-exists? directory)
        (let ((repository-file (%%string-append directory jazz:Repository-Filename)))
          (if (file-exists? repository-file)
              (call-with-input-file (%%list path: repository-file eol-encoding: 'cr-lf)
                (lambda (input)
                  (let ((form (read input)))
                    (jazz:load-repository-form directory form name: name))))
            #f))
      #f))
  
  (define (repository-inexistant)
    (jazz:error "Repository is inexistant: {a}" directory))
  
  (let ((directory (jazz:dirname-normalize directory)))
    (or (load-repository directory)
        (if error?
            (repository-inexistant)
          #f))))


(define (jazz:load-repository-form directory form #!key (name #f))
  (let ((name (or name (%%cadr form)))
        (alist (%%cddr form)))
    (let ((directory (jazz:pathname-normalize directory))
          (binary-pair (%%assq 'binary? alist))
          (library-pair (%%assq 'library alist))
          (dependencies-pair (%%assq 'dependencies alist)))
      (let ((binary? (if binary-pair (%%cadr binary-pair) #f))
            (library-root (if library-pair (%%cadr library-pair) #f))
            (dependencies (if dependencies-pair (%%cdr dependencies-pair) '())))
        (let ((library-directory (if (%%not library-root) directory (%%string-append directory library-root "/"))))
          (%%make-repository name directory library-root library-directory binary? #f dependencies))))))


(define (jazz:install-repository directory/repository #!key (name #f))
  (let ((repository (if (jazz:repository? directory/repository) directory/repository (jazz:load-repository directory/repository name: name))))
    (set! jazz:Repositories (%%append jazz:Repositories (%%list repository)))
    (if jazz:setup-repositories-called?
        (jazz:setup-repository repository))
    repository))


(define (jazz:install-repository-if-exists directory #!key (name #f))
  (if (file-exists? directory)
      (jazz:install-repository directory name: name)))


(define (jazz:uninstall-repository repository)
  (set! jazz:Repositories (%%remove repository jazz:Repositories)))


(define (jazz:require-repository name)
  (or (jazz:find-repository name)
      (jazz:error "Unknown repository: {s}" name)))


(define (jazz:find-repository name)
  (let iter ((repositories jazz:Repositories))
    (if (%%null? repositories)
        #f
      (let ((repository (%%car repositories)))
        (if (%%eq? (%%get-repository-name repository) name)
            repository
          (iter (%%cdr repositories)))))))


(define (jazz:find-package package-name)
  (let iter ((repositories jazz:Repositories))
    (if (%%null? repositories)
        #f
      (let ((repository (%%car repositories)))
        (if (%%get-repository-binary? repository)
            (iter (%%cdr repositories))
          (let ((package (jazz:repository-find-package repository package-name)))
            (if package
                package
              (iter (%%cdr repositories)))))))))


(define (jazz:repository-pathname repository path)
  (%%string-append (%%get-repository-library-directory repository)
                   path))


(define (jazz:repository-packages-table repository)
  (or (%%get-repository-packages-table repository)
      (let ((table (%%make-table test: eq?)))
        (%%set-repository-packages-table repository table)
        (jazz:repository-install-packages repository)
        table)))


(define (jazz:sweep-build-repository remove-packages)
  (if jazz:Build-Repository
      (begin
        (jazz:feedback "; sweeping binaries...")
        (let ((table (jazz:repository-packages-table jazz:Build-Repository)))
          (jazz:iterate-table-safe table
            (lambda (name package)
              ;; when each repository has its own build repository we can then safely remove the dangling binary package
              (cond ((jazz:find-package name)
                     (jazz:sweep-build-package package))
                    ((and (pair? remove-packages) (memq name remove-packages))
                     (jazz:remove-build-package name)))))))))


(define (jazz:sweep-build-package package)
  (let ((units (%%apply append (map (lambda (descriptor)
                                      (let ((name (jazz:product-descriptor-name descriptor)))
                                        (let ((update (jazz:cond-expanded-product-descriptor-update name descriptor)))
                                          (%%apply append (map (lambda (unit-name)
                                                                 (jazz:get-subunit-names unit-name walk-continue?: #t))
                                                               update)))))
                                    (%%get-package-products package)))))
    (let ((resources (map (lambda (unit-name) (jazz:find-unit-src unit-name)) units)))
      (let ((referenced (map (lambda (resource) (%%get-resource-path resource)) resources))
            (root (jazz:package-root-pathname package "")))
        (define (sweep-file pathname path)
          (let ((spine (jazz:pathname-spine path)))
            (if (%%not (jazz:some? (lambda (ref)
                                     (%%equal? ref spine))
                                   referenced))
                (begin
                  (jazz:feedback "; removing {a}..." path)
                  (if (%%not (jazz:dry-run?))
                      (delete-file pathname))))))
        
        (define (sweep-directory pathname path)
          (if (%%not (or (%%equal? path "")
                         (jazz:some? (lambda (ref)
                                       (jazz:string-starts-with? ref path))
                                     referenced)))
              (begin
                (jazz:feedback "; removing {a}..." path)
                (if (%%not (jazz:dry-run?))
                    (jazz:delete-directory pathname)))
            (for-each (lambda (name)
                        (let ((pathname (%%string-append pathname name))
                              (path (%%string-append path name)))
                          (case (jazz:pathname-type pathname)
                            ((file)
                             (sweep-file pathname path))
                            ((directory)
                             (sweep-directory (%%string-append pathname "/") (%%string-append path "/"))))))
                      (jazz:directory-content pathname))))
        
        (if (jazz:pathname-exists? root)
            (sweep-directory root ""))))))


(define (jazz:remove-build-package name)
  (if jazz:Build-Repository
      (let ((library-directory (%%get-repository-library-directory jazz:Build-Repository)))
        (let ((package-directory (%%string-append library-directory (%%symbol->string name) "/")))
          (if (file-exists? package-directory)
              (begin
                (jazz:feedback "; removing {a}..." name)
                (if (%%not (jazz:dry-run?))
                    (begin
                      (jazz:delete-directory package-directory)
                      (%%table-clear (jazz:repository-packages-table jazz:Build-Repository) name)))))))))


(jazz:define-variable jazz:setup-repositories-called?
  #f)


(define (jazz:setup-repositories)
  (for-each jazz:setup-repository jazz:Repositories)
  (set! jazz:setup-repositories-called? #t))


(define (jazz:setup-repository repository)
  (let ((table (jazz:repository-packages-table repository)))
    (jazz:iterate-table-safe table
      (lambda (name package)
        (jazz:setup-package package)))
    #; ;; this removes the randomness of scanning an eq? table
       ;; and can sometimes be usefull for debugging purposes
    (for-each (lambda (pair)
                (jazz:setup-package (%%cdr pair)))
              (jazz:sort-list (lambda (x y)
                                (%%string<? (%%symbol->string (%%car x))
                                            (%%symbol->string (%%car y))))
                              (%%table->list table)))))


(define (jazz:repository-packages repository)
  (let ((table (jazz:repository-packages-table repository))
        (packages '()))
    (jazz:iterate-table table
      (lambda (name package)
        (set! packages (%%cons package packages))))
    packages))


(define (jazz:repository-find-package repository package-name)
  (%%table-ref (jazz:repository-packages-table repository) package-name #f))


(define (jazz:repository-install-packages repository)
  (define (repository-discover-packages repository)
    (let ((table (%%get-repository-packages-table repository)))
      (define (discover-packages parent library-directory packages)
        (if (file-exists? library-directory)
            (let iter ((dirnames (jazz:directory-directories library-directory))
                       (packages packages))
                 (if (%%null? dirnames)
                     packages
                   (let ((dirname (%%car dirnames)))
                     (let ((directory (%%string-append library-directory dirname "/")))
                       (let ((package-pathname (%%string-append directory jazz:Package-Filename)))
                         (if (file-exists? package-pathname)
                             (let ((package-name (%%string->symbol dirname)))
                               (if (%%table-ref table package-name #f)
                                   (iter (%%cdr dirnames) packages)
                                 (let ((package (if (%%eq? repository jazz:Build-Repository)
                                                    (let ((source-package (jazz:find-package package-name)))
                                                      (if source-package
                                                          (jazz:load/create-build-package source-package)
                                                        ;; when each repository has its own build repository we can then safely remove the dangling binary package
                                                        (jazz:load-package repository parent package-name package-pathname)))
                                                  (jazz:load-package repository parent package-name package-pathname))))
                                   (iter (%%cdr dirnames) (%%cons package (let ((library-path (%%get-package-library-path package)))
                                                                            (if library-path
                                                                                (let ((library-directory (jazz:repository-pathname (%%get-package-repository package) (%%string-append library-path "/"))))
                                                                                  (discover-packages package library-directory packages))
                                                                              packages)))))))
                           (iter (%%cdr dirnames) packages)))))))
          packages))
      
      (discover-packages #f (%%get-repository-library-directory repository) '())))
  
  (let ((table (%%get-repository-packages-table repository))
        (packages (repository-discover-packages repository)))
    (for-each (lambda (package)
                (%%table-set! table (%%get-package-name package) package))
              packages)
    packages))


(define (jazz:repository-add-package repository package)
  (let ((table (jazz:repository-packages-table repository)))
    (%%table-set! table (%%get-package-name package) package)))


(define (jazz:repository-remove-package repository package)
  (let ((table (jazz:repository-packages-table repository)))
    (%%table-clear table (%%get-package-name package))))


(define (jazz:load-package repository parent package-name package-pathname)
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
                    (project (%%assq 'project alist))
                    (title (%%assq 'title alist))
                    (description (%%assq 'description alist))
                    (authors (%%assq 'authors alist))
                    (stage (%%assq 'stage alist)))
                (let ((package
                        (jazz:make-package repository name parent
                          (if library (%%cadr library) #f)
                          (if root (%%cadr root) #f)
                          (if install (%%cadr install) #f)
                          (if char-encoding (%%cadr char-encoding) #f)
                          (if products (%%cdr products) '())
                          (if profiles (%%cdr profiles) '())
                          (if project (%%cadr project) #f)
                          (if title (%%cadr title) #f)
                          (if description (%%cadr description) #f)
                          (if authors (%%cdr authors) #f)
                          (if stage (%%cadr stage) #f))))
                  (jazz:cache-package-roots package)
                  package))
            (jazz:error "Package at {s} is defining: {s}" package-pathname name)))))))


(define (jazz:load/create-build-package package)
  (let* ((name (%%get-package-name package))
         (parent (%%get-package-parent package))
         (bin-parent (if parent (jazz:load/create-build-package parent) #f))
         (dir (%%string-append (if parent (%%string-append (%%get-package-library-path parent) "/") "") (%%symbol->string name) "/"))
         (path (%%string-append dir jazz:Package-Filename))
         (src (jazz:repository-pathname (%%get-package-repository package) path))
         (dst (jazz:repository-pathname jazz:Build-Repository path)))
    (define (updated-uptodate?)
      (if (file-exists? dst)
          (if (= (jazz:file-last-modification-seconds src) (jazz:file-last-modification-seconds dst))
              #t
            (if (%%string=? (digest-file src 'SHA-1) (digest-file dst 'SHA-1))
                (begin
                  (file-last-access-and-modification-times-set! dst (file-last-access-time src) (file-last-modification-time src))
                  #t)
              #f))
        #f))
    
    (define (load-package)
      (let ((package (jazz:load-package jazz:Build-Repository bin-parent name dst)))
        (%%table-set! (jazz:repository-packages-table jazz:Build-Repository)
                      name
                      package)
        package))
    
    (if (updated-uptodate?)
        (or (jazz:repository-find-package jazz:Build-Repository name)
            (load-package))
      (begin
        (jazz:create-directories (jazz:repository-pathname jazz:Build-Repository dir))
        (if (file-exists? dst)
            (delete-file dst))
        (copy-file src dst)
        (load-package)))))


(define (jazz:setup-package package)
  (let ((install (%%get-package-install package)))
    (if install
        (jazz:load-unit install))))


(define (jazz:inspect-install)
  (define (inspect-path path)
    `(:path ,path ,(path-expand path)))
  
  (define (inspect-repository repository)
    `(:repository
      ,(%%get-repository-name repository)
      ,(%%get-repository-library-directory repository)
      ,@(map inspect-package (jazz:repository-packages repository))))
  
  (define (inspect-package package)
    `(:package
      ,(%%get-package-name package)
      ,(%%get-package-units-root package)
      ,(%%get-package-units-path package)))
  
  `(,(inspect-path "./")
    ,(inspect-path "~/")
    ,@(map inspect-repository jazz:Repositories)))


;;;
;;;; Package
;;;


(define jazz:Package-Filename
  ".package")


(define (jazz:make-package repository name parent library-root units-root install char-encoding products profiles project title description authors stage)
  (let ((library-path (if (%%not library-root)
                          #f
                        (%%string-append (%%symbol->string name) "/" library-root)))
        (units-path (if (%%not units-root)
                        (%%symbol->string name)
                      (%%string-append (%%symbol->string name) "/" units-root))))
    (%%make-package repository name parent library-root library-path units-root units-path install char-encoding products profiles project title description authors stage)))


(define (jazz:package-root package)
  (let ((parent (%%get-package-parent package)))
    (%%string-append (let ((library-root (%%get-repository-library-root (%%get-package-repository package))))
                       (if library-root
                           (%%string-append library-root "/")
                         ""))
                     (if parent (%%string-append (%%get-package-library-path parent) "/") "")
                     (%%symbol->string (%%get-package-name package)))))


(define (jazz:package-pathname package path)
  (jazz:repository-pathname (%%get-package-repository package)
                            (%%string-append (%%symbol->string (%%get-package-name package)) "/" path)))


(define (jazz:package-root-pathname package path)
  (jazz:relocate-package-pathname (%%get-package-repository package) package path))


(define (jazz:relocate-package-pathname repository package path)
  (let ((parent (%%get-package-parent package)))
    (jazz:repository-pathname repository
      (%%string-append (if parent (%%string-append (%%get-package-library-path parent) "/") "")
                       (%%get-package-units-path package)
                       "/"
                       (or path "")))))


(define (jazz:iterate-packages binary? proc)
  (let iter-repo ((repositories jazz:Repositories))
    (if (%%not (%%null? repositories))
        (let ((repository (%%car repositories)))
          (if (%%neq? binary? (%%get-repository-binary? repository))
              (iter-repo (%%cdr repositories))
            (let iter ((packages (jazz:repository-packages repository)))
              (if (%%null? packages)
                  (iter-repo (%%cdr repositories))
                (let ((package (%%car packages)))
                  (proc package)
                  (iter (%%cdr packages))))))))))


(define (jazz:find-resource pathname)
  (let iter-repo ((repositories jazz:Repositories))
    (if (%%null? repositories)
        #f
      (let iter ((packages (jazz:repository-packages (%%car repositories))))
        (if (%%null? packages)
            (iter-repo (%%cdr repositories))
          (let ((package (%%car packages)))
            (let ((package-pathname (jazz:package-root-pathname package "")))
              (let ((pathname-length (%%string-length pathname))
                    (package-length (%%string-length package-pathname)))
                (if (and (%%fx<= package-length pathname-length)
                         (%%string=? (%%substring pathname 0 package-length) package-pathname))
                    (let* ((path (%%substring pathname package-length pathname-length))
                           (underscore? #f)
                           (extension (and path (jazz:pathname-extension path))))
                      ;; remove extension
                      (if extension
                          (set! path (%%substring path 0 (%%fx- (%%string-length path) (%%fx+ 1 (%%string-length extension))))))
                      ;; remove redundant underscore prefixed name
                      (let ((len (%%string-length path))
                            (pos (jazz:string-find-reversed path #\/)))
                        (if pos
                            (let ((name-pos (%%fx+ pos 2)))
                              (if (and (%%fx< name-pos len) (%%eqv? (%%string-ref path (%%fx+ pos 1)) #\_))
                                  (let* ((name (%%substring path name-pos len))
                                         (name-length (%%string-length name))
                                         (previous-pos (%%fx- pos name-length)))
                                    (if (and (%%fx>= previous-pos 0) (%%string=? (%%substring path previous-pos pos) name))
                                        (begin
                                          (set! path (%%substring path 0 pos))
                                          (set! underscore? #t))))))))
                      (%%make-resource package path underscore? extension))
                  (iter (%%cdr packages)))))))))))


(define (jazz:find-pathname-unit pathname)
  (let ((resource (jazz:find-resource pathname)))
    (if resource
        (jazz:path->name (%%get-resource-path resource))
      #f)))


;;;
;;;; Profiles
;;;


(define (jazz:gather-profiles)
  (let iter-repo ((repositories jazz:Repositories) (profiles '()))
    (if (%%null? repositories)
        profiles
      (let ((repository (%%car repositories)))
        (if (%%get-repository-binary? repository)
            (iter-repo (%%cdr repositories) profiles)
          (let iter ((packages (jazz:repository-packages repository)) (profiles profiles))
            (if (%%null? packages)
                (iter-repo (%%cdr repositories) profiles)
              (let ((package (%%car packages)))
                (let ((package-profiles (%%get-package-profiles package)))
                  (iter (%%cdr packages) (%%append (map (lambda (package-profile)
                                                          (%%cons package package-profile))
                                                        package-profiles)
                                                   profiles)))))))))))


(define (jazz:make-profile name unit-name)
  `(,name (unit ,unit-name)))


(define (jazz:profile-name profile)
  (%%car profile))

(define (jazz:profile-title profile)
  (%%symbol->string (jazz:profile-name profile)))

(define (jazz:profile-appl profile)
  (let ((pair (%%assq 'appl (%%cdr profile))))
    (if (%%not pair)
        #f
      (%%cadr pair))))

(define (jazz:profile-unit profile)
  (let ((pair (%%assq 'unit (%%cdr profile))))
    (if (%%not pair)
        (jazz:error "Unable to find unit in profile: {s}" profile)
      (%%cadr pair))))


;;;
;;;; Unit
;;;


(define (jazz:lower-case-unit-name? unit-name)
  (define (first-char-of-last-name str)
    (let loop ((offset (%%fx- (%%string-length str) 1)))
         (let ((ch (%%string-ref str offset)))
           (cond ((%%char=? #\. (%%string-ref str offset))
                  (if (%%fx< (%%fx+ offset 1) (%%string-length str))
                      (%%string-ref str (%%fx+ 1 offset))
                    (jazz:error "Unit name {a} ends with a ." str)))
                 ((%%fx= offset 0)
                  ch)
                 (else
                  (loop (%%fx- offset 1)))))))
  
  (let ((first-char (first-char-of-last-name (%%symbol->string unit-name))))
    (and (%%char<=? #\a first-char) (%%char<=? first-char #\z))))


(define (jazz:find-unit-src unit-name #!key (extensions #f) (error? #t))
  (define (find-src package path)
    (define (try path underscore?)
      (define (try-extension extension)
        (if (file-exists? (jazz:package-root-pathname package (%%string-append path "." extension)))
            (%%make-resource package path underscore? extension)
          #f))
      
      (let iter ((extensions (or extensions '("jazz" "scm"))))
           (if (%%null? extensions)
               #f
             (or (try-extension (%%car extensions))
                 (iter (%%cdr extensions))))))
    
    (if (and (file-exists? (jazz:package-root-pathname package path))
             ;; why was this arbitrary test necessary?
             #;
             (jazz:lower-case-unit-name? unit-name))
        (try (%%string-append path "/_" (jazz:pathname-name path)) #t)
      (try path #f)))
  
  (continuation-capture
    (lambda (return)
      (let ((path (jazz:name->path unit-name)))
        (for-each (lambda (package)
                    (let ((src (find-src package path)))
                      (if src
                          (continuation-return return src))))
                  (jazz:cached-packages jazz:*source-packages-cache* unit-name))
        (jazz:iterate-packages #f
          (lambda (package)
            (let ((src (find-src package path)))
              (if src
                  (begin
                    (jazz:cache-package jazz:*source-packages-cache* unit-name package)
                    #; ;; test
                    (jazz:validate-repository-unicity (%%get-package-repository package)
                                                      unit-name
                                                      (lambda (package)
                                                        (find-src package path)))
                    (continuation-return return src)))))))
      (if error?
          (jazz:error "Unable to find unit: {s}" unit-name)
        #f))))


(define (jazz:require-unit-src unit-name)
  (jazz:find-unit-src unit-name error?: #t))


(define (jazz:with-unit-resources unit-name extensions proc)
  (define force-interpreted?
    (let ((interpreted? (jazz:force-interpreted?)))
      (cond ((%%boolean? interpreted?)
             interpreted?)
            ((%%symbol? interpreted?)
             (%%eq? unit-name interpreted?))
            (else
             (%%memv unit-name interpreted?)))))
  
  (define (find-unit-binaries src)
    (define (find package path extension)
      (define (try path underscore?)
        ;; we only test .o1 and let gambit find the right file by returning no extension when found
        (if (file-exists? (jazz:package-root-pathname package (%%string-append path extension)))
            (%%make-resource package path underscore? #f)
          #f))
      
      (if (and (file-exists? (jazz:package-root-pathname package path))
               (jazz:lower-case-unit-name? unit-name))
          (try (%%string-append path "/_" (jazz:pathname-name path)) #t)
        (try path #f)))
    
    (define (find-uptodate package path)
      (let ((obj (find package path ".o"))
            (bin (find package path ".o1")))
        (let ((manifest (let ((obj/bin (or obj bin)))
                          (and obj/bin
                               (jazz:load-updated-manifest
                                 unit-name
                                 (jazz:digest-pathname (%%get-resource-package obj/bin) obj/bin)
                                 (jazz:manifest-pathname (%%get-resource-package obj/bin) obj/bin)
                                 (and src (jazz:resource-pathname src)))))))
          (let ((uptodate? (or (%%not src)
                               (and manifest
                                    (jazz:manifest-uptodate? (jazz:resource-pathname src) manifest)
                                    (%%not (jazz:manifest-needs-rebuild? manifest))))))
            (if uptodate?
                (values obj
                        (if force-interpreted? #f bin)
                        manifest)
              (values #f #f #f))))))
    
    (continuation-capture
      (lambda (return)
        (let ((path (jazz:name->path unit-name)))
          (for-each (lambda (package)
                      (receive (obj bin manifest) (find-uptodate package path)
                        (if manifest
                            (continuation-return return (values obj bin manifest)))))
                    (jazz:cached-packages jazz:*binary-packages-cache* unit-name))
          (jazz:iterate-packages #t
            (lambda (package)
              (receive (obj bin manifest) (find-uptodate package path)
                (if manifest
                    (begin
                      (jazz:cache-package jazz:*binary-packages-cache* unit-name package)
                      #; ;; test
                      (jazz:validate-repository-unicity (%%get-package-repository package)
                                                        unit-name
                                                        (lambda (package)
                                                          (find-bin package path)))
                      (continuation-return return (values obj bin manifest))))))))
        (values #f #f #f))))
  
  (let ((src (jazz:find-unit-src unit-name extensions: extensions error?: #f))
        (image-unit (jazz:get-image-unit unit-name)))
    (receive (obj bin manifest) (find-unit-binaries src)
      (let ((obj-uptodate? (if obj #t #f))
            (bin-uptodate? (if bin #t #f))
            (lib-uptodate?
              (and image-unit
                   (or (%%not (or src bin))
                       ; validate lib against manifest
                       (and manifest
                            (jazz:image-unit-uptodate? image-unit src manifest)
                            (%%not (jazz:manifest-needs-rebuild? manifest))
                            (%%not force-interpreted?))
                       ; validate lib against source
                       (and src
                            (jazz:image-unit-uptodate-src? image-unit src)
                            (%%not force-interpreted?))))))
        (proc src
              obj
              bin
              (if image-unit (%%get-image-unit-load-proc image-unit) #f)
              obj-uptodate?
              bin-uptodate?
              lib-uptodate?
              manifest)))))


(define (jazz:unit-status unit-name #!key (all? #f))
  (jazz:with-unit-resources unit-name #f
    (lambda (src obj bin load-proc obj-uptodate? bin-uptodate? lib-uptodate? manifest)
      (let ((loaded? (jazz:unit-loaded? unit-name)))
        (display "unit ") (display unit-name) (newline)
        ;(display "src package pathname: ") (and src (display (jazz:resource-package-pathname src))) (newline)
        ;(display "bin package pathname: ") (and bin (display (jazz:resource-package-pathname bin))) (newline)
        (if src
            (begin
              (display "source hash: ")
              (display (digest-file (jazz:resource-pathname src) 'SHA-1))
              (newline))
          (begin (display "source not found") (newline)))
        (if all?
            (begin
              (if obj (begin (display "obj ") (display (jazz:resource-pathname obj)) (newline)))
              (if bin (begin (display "bin ") (display (jazz:resource-pathname bin)) (newline)))))
        (display "obj-uptodate? ") (display obj-uptodate?) (newline)
        (display "bin-uptodate? ") (display bin-uptodate?) (newline)
        (display "lib-uptodate? ") (display lib-uptodate?) (newline)
        (if (and bin jazz:manifest-valid?)
            (begin
              (display "manifest-valid? ") (display (jazz:manifest-valid? bin)) (newline)))
        (display "loaded? ") (display loaded?) (newline)))))


(define (jazz:unit-uptodate-binary? unit-name)
  (jazz:with-unit-resources unit-name #f
    (lambda (src obj bin lib obj-uptodate? bin-uptodate? lib-uptodate? manifest)
      (or bin-uptodate? lib-uptodate?))))


(define (jazz:image-unit-uptodate? image-unit src manifest)
  (let ((digest (jazz:find-source-digest (jazz:resource-pathname src) manifest)))
    (and digest (%%string=? (%%get-image-unit-compile-time-hash image-unit) (%%get-digest-hash digest)))))


(define (jazz:image-unit-uptodate-src? image-unit src)
  (let ((source-hash (digest-file (jazz:resource-pathname src) 'SHA-1)))
    (%%string=? (%%get-image-unit-compile-time-hash image-unit) source-hash)))


(define (jazz:validate-repository-unicity repository unit-name proc)
  (define (repository-unique? repository proc)
    (let iter ((packages (jazz:repository-packages repository))
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
      (jazz:error "Found duplicate resource in {a} repository: {s}"
                  (or (%%get-repository-name repository) "anonymous")
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


(define jazz:*binary-packages-cache*
  (%%make-table test: eq?))

(define jazz:*source-packages-cache*
  (%%make-table test: eq?))


(define (jazz:cache-package cache unit-name package)
  (jazz:with-cached-prefix unit-name
    (lambda (prefix singleton-prefix)
      (let ((packages (%%table-ref cache prefix '())))
        (if (%%not (%%memq package packages))
            (%%table-set! cache prefix (%%cons package packages)))))))


(define (jazz:cached-packages cache unit-name)
  (jazz:with-cached-prefix unit-name
    (lambda (prefix singleton-prefix)
      (or (%%table-ref cache prefix #f)
          (if singleton-prefix
              (%%table-ref cache singleton-prefix '())
            '())))))


;; return as symbols the first 1 or 2 parts and the first part if there exactly 2 parts
(define (jazz:with-cached-prefix unit-name proc)
  (let ((name (%%symbol->string unit-name)))
    (let ((first-period (jazz:string-find name #\.)))
      (if first-period
          (let ((second-period (jazz:string-find name #\. (%%fx+ first-period 1))))
            (if second-period
                (proc (%%string->symbol (%%substring name 0 second-period)) #f)
              (proc unit-name (%%string->symbol (%%substring name 0 first-period)))))
        (proc unit-name #f)))))


;; cache every subdirectory of level 2 and every subdirectory of level 1 that contains files
(define (jazz:cache-package-roots package)
  (let ((cache (if (%%get-repository-binary? (%%get-package-repository package))
                   jazz:*binary-packages-cache*
                 jazz:*source-packages-cache*))
        (toplevel-dir (jazz:package-root-pathname package "")))
    (if (file-exists? toplevel-dir)
        (for-each (lambda (first-part)
                    (let ((first-dir (string-append toplevel-dir first-part "/")))
                      (if (file-exists? first-dir)
                          (let ((has-files? #f))
                            (for-each (lambda (second-part)
                                        (let ((second-path (%%string-append first-dir second-part)))
                                          (case (jazz:pathname-type second-path)
                                            ((file) (set! has-files? #t))
                                            ((directory) (let ((unit-name (%%string->symbol (string-append first-part "." second-part))))
                                                           (jazz:cache-package cache unit-name package))))))
                                      (jazz:directory-content first-dir))
                            (if has-files?
                                (let ((unit-name (%%string->symbol first-part)))
                                  (jazz:cache-package cache unit-name package)))))))
                  (jazz:directory-directories toplevel-dir)))))


;;;
;;;; Debug
;;;


;; it is critical that these units be explicitly loaded up front
;; else we can get very hard to debug locking on the load mutex


(define (jazz:load-debuggee)
  (jazz:load-debuggee-units)
  (jazz:load-unit 'jazz.debuggee.setup)
  (jazz:load-unit 'jazz.debuggee.update))


(define (jazz:load-debuggee-units)
  (jazz:load-foundation)
  (jazz:load-unit 'jazz)
  (jazz:load-unit 'jazz.debuggee)
  (jazz:load-unit 'jazz.debuggee.base)
  (jazz:load-unit 'jazz.debuggee.stub)
  (jazz:load-unit 'jazz.debuggee.jazz)
  (jazz:load-unit 'jazz.debuggee.jazz.autoload)
  (jazz:load-unit 'jazz.debuggee.jazz.autoload-external)
  (jazz:load-unit 'jazz.debuggee.jazz.Jazz-Debuggee-Frame)
  (jazz:load-unit 'jazz.debuggee.jazz.Jazz-Debuggee-Process)
  (jazz:load-unit 'jazz.debuggee.jazz.Jazz-Debuggee-Restart)
  (jazz:load-unit 'jazz.debuggee.jazz.Jazz-Debuggee-Stop)
  (jazz:load-unit 'jazz.debuggee.jazz.Jazz-Debuggee-Thread)
  (jazz:load-unit 'jazz.debugger.stub))


;;;
;;;; Product
;;;


(define (jazz:find-product-descriptor name)
  (define (find-descriptor products)
    (let iter ((scan products))
         (if (%%null? scan)
             #f
           (let ((product (%%car scan)))
             (if (jazz:product-descriptor-named? product name)
                 product
               (iter (%%cdr scan)))))))
  
  (let ((binary-package #f)
        (binary-descriptor #f))
    (let iter-repo ((repositories jazz:Repositories))
      (if (%%null? repositories)
          (values binary-package binary-descriptor)
        (let ((repository (%%car repositories)))
          (let iter ((packages (jazz:repository-packages (%%car repositories))))
            (if (%%null? packages)
                (iter-repo (%%cdr repositories))
              (let ((package (%%car packages)))
                (let ((pair (find-descriptor (%%get-package-products package))))
                  (if pair
                      (cond ((%%get-repository-binary? repository)
                             (set! binary-package package)
                             (set! binary-descriptor pair)
                             (iter (%%cdr packages)))
                            (else
                             (values package pair)))
                    (iter (%%cdr packages))))))))))))


(define (jazz:product-descriptor-named? descriptor name)
  (or (%%eq? (jazz:product-descriptor-name descriptor) name)
      (%%eq? (jazz:product-descriptor-alias descriptor) name)))


(define (jazz:product-descriptor-name descriptor)
  (%%car descriptor))

(define (jazz:product-descriptor-alias descriptor)
  (let ((pair (%%assq 'alias (%%cdr descriptor))))
    (if pair
        (%%cadr pair)
      #f)))

(define (jazz:product-descriptor-title descriptor)
  (let ((pair (%%assq 'title (%%cdr descriptor))))
    (if pair
        (%%cadr pair)
      #f)))

(define (jazz:product-descriptor-prefix descriptor)
  (let ((pair (%%assq 'prefix (%%cdr descriptor))))
    (if pair
        (%%cadr pair)
      #f)))

(define (jazz:product-descriptor-description descriptor)
  (let ((pair (%%assq 'description (%%cdr descriptor))))
    (if pair
        (%%cadr pair)
      #f)))

(define (jazz:product-descriptor-icon descriptor)
  (let ((pair (%%assq 'icon (%%cdr descriptor))))
    (if pair
        (%%cadr pair)
      #f)))

(define (jazz:product-descriptor-run descriptor)
  (let ((pair (%%assq 'run (%%cdr descriptor))))
    (if pair
        (%%cdr pair)
      #f)))

(define (jazz:product-descriptor-switches descriptor)
  (let ((pair (%%assq 'switches (%%cdr descriptor))))
    (if pair
        (%%cdr pair)
      #f)))

(define (jazz:product-descriptor-product descriptor)
  (let ((pair (%%assq 'product (%%cdr descriptor))))
    (if pair
        (%%cadr pair)
      #f)))

(define (jazz:product-descriptor-test descriptor)
  (let ((pair (%%assq 'test (%%cdr descriptor))))
    (if pair
        (%%cdr pair)
      #f)))

(define (jazz:product-descriptor-update descriptor)
  (let ((pair (%%assq 'update (%%cdr descriptor))))
    (if pair
        (%%cdr pair)
      '())))

(define (jazz:product-descriptor-build descriptor)
  (let ((pair (%%assq 'build (%%cdr descriptor))))
    (if pair
        (%%cdr pair)
      #f)))

(cond-expand
  (cocoa
    (define (jazz:product-descriptor-build-bundle descriptor)
      (let ((alist (jazz:product-descriptor-build descriptor)))
        (if alist
            (let ((pair (%%assq (jazz:product-descriptor-name descriptor) alist)))
              (if pair
                  (jazz:getf (%%cdr pair) bundle:)
                #f))
          #f))))
  (else
    (define (jazz:product-descriptor-build-bundle descriptor)
      #f)))

(define (jazz:product-descriptor-library descriptor)
  (let ((pair (%%assq 'library (%%cdr descriptor))))
    (if pair
        (%%cdr pair)
      #f)))


(define (jazz:product-descriptor-dependencies descriptor)
  (let ((pair (%%assq 'dependencies (%%cdr descriptor))))
    (if pair
        (%%cdr pair)
      '())))


(define (jazz:cond-expanded-product-descriptor-update product-name product-descriptor)
  (jazz:cond-expand-map (jazz:ill-formed-field-error "update" product-name)
                        (jazz:product-descriptor-update product-descriptor)))


(define (jazz:cond-expanded-product-descriptor-dependencies product-name product-descriptor)
  (jazz:cond-expand-map (jazz:ill-formed-field-error "dependencies" product-name)
                        (jazz:product-descriptor-dependencies product-descriptor)))


(define jazz:Products-Table
  (%%make-table test: eq?))

(define jazz:Products-Run-Table
  (%%make-table test: eq?))


(define (jazz:register-run name proc)
  (%%table-set! jazz:Products-Run-Table name proc))


(define (jazz:registered-run name)
  (or (%%table-ref jazz:Products-Run-Table name #f)
      (jazz:error "Unable to find registered run: {s}" name)))


(define (jazz:run-registered name)
  (let ((proc (jazz:registered-run name)))
    (proc)))


(jazz:define-variable jazz:process-product
  #f)

(jazz:define-variable jazz:process-name
  #f)

(jazz:define-variable jazz:process-title
  #f)

(jazz:define-variable jazz:process-prefix
  #f)

(jazz:define-variable jazz:process-traits
  #f)

(jazz:define-variable jazz:process-icon
  #f)

(jazz:define-variable jazz:process-version
  #f)


(define (jazz:current-process-product)
  jazz:process-product)

(define (jazz:current-process-name)
  jazz:process-name)

(define (jazz:current-process-name-set! name)
  (set! jazz:process-name name))

(define (jazz:current-process-title)
  jazz:process-title)

(define (jazz:current-process-title-set! title)
  (set! jazz:process-title title))

(define (jazz:current-process-prefix)
  jazz:process-prefix)

(define (jazz:current-process-prefix-set! prefix)
  (set! jazz:process-prefix prefix))

(define (jazz:current-process-traits)
  jazz:process-traits)

(define (jazz:current-process-traits-set! traits)
  (set! jazz:process-traits traits))

(define (jazz:current-process-icon)
  jazz:process-icon)

(define (jazz:current-process-icon-set! icon)
  (set! jazz:process-icon icon))

(define (jazz:current-process-version)
  jazz:process-version)

(define (jazz:current-process-version-set! version)
  (set! jazz:process-version version))


(define (jazz:current-process-present)
  (or (jazz:current-process-title)
      (let ((name (jazz:current-process-name)))
        (if name
            (%%symbol->string name)
          #f))))


(define (jazz:register-product name #!key (title #f) (icon #f) (run #f) (test #f) (update #f) (build #f) (options #f) (library-options #f) (install #f) (deploy #f))
  (receive (package descriptor) (jazz:find-product-descriptor name)
    (%%table-set! jazz:Products-Table name (%%make-product name title icon run test update build options library-options install deploy package descriptor))))


(define (jazz:get-product-descriptor name)
  (receive (package descriptor) (jazz:find-product-descriptor name)
    (if (and package descriptor)
        (values package descriptor)
      (jazz:error "Unable to find product: {s}" name))))


(define (jazz:get-product name)
  (define (get-registered-product name)
    (or (%%table-ref jazz:Products-Table name #f)
        (jazz:error "Unable to find registered product: {s}" name)))

  (receive (package descriptor) (jazz:get-product-descriptor name)
    (let ((name (jazz:product-descriptor-name descriptor)) ; because of aliases
          (unit (jazz:product-descriptor-product descriptor)))
      (if unit
          (begin
            (jazz:load-unit unit)
            (get-registered-product name))
        (let ((title (jazz:product-descriptor-title descriptor))
              (icon (jazz:product-descriptor-icon descriptor)))
          (%%make-product name title icon
            #f
            #f
            jazz:update-product-descriptor
            jazz:build-product-descriptor
            #f
            #f
            #f
            #f
            package
            descriptor))))))


(define (jazz:setup-product name)
  (let ((product (jazz:get-product name)))
    (let ((descriptor (%%get-product-descriptor product)))
      (set! jazz:process-product name)
      (set! jazz:process-name name)
      (set! jazz:process-title (or (%%get-product-title product) (jazz:product-descriptor-title descriptor)))
      (set! jazz:process-prefix (jazz:product-descriptor-prefix descriptor))
      (set! jazz:process-icon (or (%%get-product-icon product) (jazz:product-descriptor-icon descriptor)))
      (let ((switches (jazz:product-descriptor-switches descriptor)))
        (if switches
            (jazz:add-kernel-runtime-switches switches))))
    (if jazz:debugger
        (jazz:load-debuggee))
    product))


#; ;; dynamic-dependencies
(define (jazz:adjust-build-repository product)
  ;; dynamic repositories - build location mirrors the source location
  ;; we want to build in $BINARY/$REPO/$BRANCH
  ;; where: kernel.exe is in $BINARY/jazz/master
  ;; where: source repo is in $SOURCE/$REPO/$BRANCH
  ;; jazz:Build-Repository is the destination
  ;; jazz:Repositories is used to locate which files are up-to-date
  (if (jazz:getf jazz:kernel-properties dynamic?:)
      (let ((product-source-directory (%%get-repository-directory (%%get-package-repository (%%get-product-package product))))
            (kernel-root-directory (path-directory (path-strip-trailing-directory-separator
                                                     (path-directory (path-strip-trailing-directory-separator
                                                                       jazz:kernel-install))))))
        (let ((product-build-directory (jazz:build-dynamic-path kernel-root-directory product-source-directory))
              (current-build-directory (%%get-repository-directory jazz:Build-Repository)))
          (if (not (equal? product-build-directory current-build-directory))
              (let ((new-build-repository (jazz:make-repository 'Build "lib" product-build-directory binary?: #t dynamic?: #t)))
                (jazz:setup-repository new-build-repository)
                (set! jazz:Repositories (cons new-build-repository (jazz:remove jazz:Build-Repository jazz:Repositories)))
                (set! jazz:Build-Repository new-build-repository)))))))


(define (jazz:register-product-run name proc)
  (jazz:register-run name proc))


(define (jazz:run-product name)
  (define (run-product-descriptor descriptor)
    (let ((name (jazz:product-descriptor-name descriptor))
          (run (jazz:product-descriptor-run descriptor)))
      (if run
          (begin
            (for-each jazz:load-unit run)
            (let ((proc (jazz:registered-run name)))
              (proc descriptor)))
        (jazz:error "Product is not runnable: {s}" name))))
  
  (let ((product (jazz:setup-product name)))
    (let ((run (%%get-product-run product))
          (descriptor (%%get-product-descriptor product)))
      (if run
          (run descriptor)
        (run-product-descriptor descriptor)))))


(define (jazz:test-product name)
  (define (test-product-descriptor descriptor)
    (let ((name (jazz:product-descriptor-name descriptor))
          (test (jazz:product-descriptor-test descriptor)))
      (if test
          (for-each jazz:load-unit test)
        (jazz:error "Product is not testable: {s}" name))))
  
  (let ((product (jazz:setup-product name)))
    (let ((test (%%get-product-test product))
          (descriptor (%%get-product-descriptor product)))
      (if test
          (test descriptor)
        (test-product-descriptor descriptor)))))


(define (jazz:ill-formed-field-error field-name product-name)
  (lambda ()
    (jazz:error "ill-formed {a} field in product descriptor for product {a}" field-name product-name)))


(define (jazz:cond-expand-map error-proc updates)
  (define (apply-cond-expand exp)
    (cond ((and (%%pair? exp)
                (%%pair? (%%car exp)))
           (let ((clause (%%car exp)))
             (cond ((jazz:feature-satisfied? (%%car clause))
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


(define (jazz:update-product name)
  (let ((product (jazz:setup-product name)))
    #; ;; dynamic-dependencies
    (jazz:adjust-build-repository product)
    (let ((update (%%get-product-update product))
          (descriptor (%%get-product-descriptor product)))
      (if update
          (update descriptor)
        (jazz:update-product-descriptor descriptor)))))


(define (jazz:update-product-descriptor descriptor)
  (let ((name (jazz:product-descriptor-name descriptor)))
    (let ((update (jazz:cond-expanded-product-descriptor-update name descriptor)))
      (for-each jazz:build-unit update))))


(define (jazz:build-product name)
  (jazz:load-build)
  (let ((product (jazz:setup-product name)))
    #; ;; dynamic-dependencies
    (jazz:adjust-build-repository product)
    (let ((build (%%get-product-build product))
          (descriptor (%%get-product-descriptor product)))
      (jazz:feedback "make {a}" name)
      (if build
          (build descriptor)
        (jazz:build-product-descriptor descriptor))
      (if (or (jazz:link-libraries?) (jazz:link-static?))
          (jazz:build-library-descriptor descriptor)))))


(define (jazz:build-product-descriptor descriptor #!key (unit #f) (skip-references? #f) (force? #f))
  (define (build-product)
    (jazz:update-product-descriptor descriptor)
    (let ((build (jazz:product-descriptor-build descriptor)))
      (if build
          (for-each (lambda (obj)
                      (if (%%symbol? obj)
                          (jazz:build-image obj)
                        (%%apply jazz:build-image obj)))
                    build))))
  
  (if unit
      (jazz:compile-unit unit skip-references?: skip-references? force?: force?)
    (build-product)))


(define (jazz:build-library-descriptor descriptor)
  (let ((library (jazz:product-descriptor-library descriptor)))
    (let ((options (or library '())))
      (jazz:build-library (jazz:product-descriptor-name descriptor) descriptor options: library))))


(define (jazz:install-product name)
  (let ((product (jazz:setup-product name)))
    (let ((install (%%get-product-install product)))
      (if install
          (let ((descriptor (%%get-product-descriptor product)))
            (jazz:feedback "install {a}" name)
            (jazz:load-install)
            (install descriptor))))))


(define (jazz:deploy-product name)
  (let ((product (jazz:setup-product name)))
    (let ((deploy (%%get-product-deploy product)))
      (if deploy
          (let ((descriptor (%%get-product-descriptor product)))
            (jazz:load-deploy)
            (deploy descriptor))))))


;; changed to 0 until parallel build is fully debugged
(define jazz:jobs-default
  0)


;;;
;;;; Master
;;;


(define (jazz:make-product name)
  (let ((subproduct-table (%%make-table test: eq?))
        (subproduct-table-mutex (%%make-mutex 'subproduct-table-mutex))
        (active-processes '())
        (free-processes '())
        (outdated-processes '())
        (process-mutex (%%make-mutex 'process-mutex))
        (process-condition-variable (%%make-condition-variable 'process-condition-variable))
        (listening-port (open-tcp-server 0))
        (output-mutex (%%make-mutex 'output-mutex))
        (jobs (or jazz:jobs (jazz:build-jobs) jazz:jobs-default))
        (stop-build? #f))
    (define (key-product? name)
      (%%memq name '(backend
                     backend.scheme
                     contrib.irregex
                     core
                     dialect
                     foundation
                     foundation.backend.scheme
                     jazz
                     jazz.backend.scheme
                     scheme
                     scheme.backend.scheme
                     scheme.core)))
    
    (define (atomic-output line)
      (mutex-lock! output-mutex)
      (display line)
      (newline)
      (force-output)
      (mutex-unlock! output-mutex))
    
    (define (end-port/echo port/echo)
      (let ((established-port (%%car port/echo))
            (echo-thread (%%cdr port/echo)))
        (write '() established-port)
        (force-output established-port)
        (close-port established-port)
        (thread-join! echo-thread)))
    
    (define (grab-build-process)
      (mutex-lock! process-mutex)
      (let ((active-count (%%length active-processes)))
        (cond ((%%pair? free-processes)
               (let ((port/echo (%%car free-processes)))
                 (set! free-processes (%%cdr free-processes))
                 (mutex-unlock! process-mutex)
                 port/echo))
              ((%%fx< active-count jobs)
               (let ((process (open-process
                                (list
                                  path: (jazz:install-path "jazz")
                                  arguments: `(,(if (jazz:worker-repl?)
                                                    "-:darQ"
                                                  "-:daqQ-")
                                               "-worker"
                                               "-link" ,(%%symbol->string jazz:link)
                                               ,@(if (%%memq 'keep-c jazz:compile-options) `("-keep-c") '())
                                               ,@(if (%%memq 'track-scheme jazz:compile-options) `("-track-scheme") '())
                                               ,@(if (%%memq 'expansion jazz:compile-options) `("-expansion") '())
                                               ,@(if (jazz:build-target) `("-target" ,(symbol->string (jazz:build-target))) '())
                                               ,@(if (jazz:save-emit?) `("-emit") '())
                                               ,@(if (jazz:build-repository) `("-build-repository" ,(jazz:build-repository)) '())
                                               ,@(if (jazz:jazz-repository) `("-jazz-repository" ,(jazz:jazz-repository)) '())
                                               ,@(if (jazz:repositories) `("-repositories" ,(jazz:repositories)) '())
                                               #; ;; dynamic-dependencies
                                               ,@(let ((dependencies (string-append (current-directory) ".dependencies")))
                                                   (if (file-exists? dependencies)
                                                       `("-dependencies" ,dependencies)
                                                     '()))
                                               ,@(if (jazz:reporting?) `("-reporting") '())
                                               "-jobs" "1"
                                               "-port" ,(number->string (socket-info-port-number (tcp-server-socket-info listening-port))))
                                  stdin-redirection: #t
                                  stdout-redirection: #t
                                  stderr-redirection: #t))))
                 (let ((established-port (read listening-port))
                       (echo-thread (thread-start! (%%make-thread (lambda ()
                                                                    (let iter ()
                                                                         (let ((line (read-line process)))
                                                                           (if (not (eof-object? line))
                                                                               (begin
                                                                                 (atomic-output line)
                                                                                 (iter))))))))))
                   (let ((port/echo (%%cons established-port echo-thread)))
                     (set! active-processes (%%cons port/echo active-processes))
                     (mutex-unlock! process-mutex)
                     port/echo))))
              (else
               (mutex-unlock! process-mutex process-condition-variable)
               (grab-build-process)))))
    
    (define (build name)
      (let ((port/echo (grab-build-process)))
        (define (build-process-problem)
          (mutex-lock! process-mutex)
          (set! active-processes (jazz:remove port/echo active-processes))
          (set! stop-build? #t)
          (mutex-unlock! process-mutex)
          (condition-variable-signal! process-condition-variable))
        
        (define (build-process-ended changes)
          (mutex-lock! process-mutex)
          (if (and (key-product? name) (%%not (%%null? changes)))
              (set! outdated-processes active-processes))
          (if (%%memq port/echo outdated-processes)
              (begin
                (set! active-processes (jazz:remove port/echo active-processes))
                (set! outdated-processes (jazz:remove port/echo outdated-processes))
                (end-port/echo port/echo))
            (set! free-processes (%%cons port/echo free-processes)))
          (mutex-unlock! process-mutex)
          (condition-variable-signal! process-condition-variable))
        
        (if stop-build?
            (build-process-ended #f)
          (let ((established-port (%%car port/echo)))
            (send-build-command established-port name)
            (let iter ()
                 (let ((form (read established-port)))
                   (if (eof-object? form)
                       (build-process-problem)
                     (let ((command (%%car form))
                           (arguments (%%cdr form)))
                       (case command
                         ((changes)
                          (build-process-ended arguments))
                         ((report)
                          (jazz:report "{a}" arguments)
                          (iter))
                         ((exception)
                          (build-process-problem)
                          (iter))
                         (else
                          (error "Unknown master command" command)))))))))))
    
    (define (local-make name)
      (for-each (lambda (subname)
                  (if (%%not (%%table-ref subproduct-table subname #f))
                      (begin
                        (make subname)
                        (%%table-set! subproduct-table subname #t))))
                (receive (package descriptor) (jazz:get-product-descriptor name)
                  (jazz:cond-expanded-product-descriptor-dependencies name descriptor)))
      (jazz:build-product name))
    
    (define (remote-make name)
      (for-each thread-join!
                (map (lambda (name)
                       (mutex-lock! subproduct-table-mutex)
                       (let ((thread (or (%%table-ref subproduct-table name #f)
                                         (let ((thread (thread-start! (%%make-thread (lambda ()
                                                                                       (make name))))))
                                           (%%table-set! subproduct-table name thread)
                                           thread))))
                         (mutex-unlock! subproduct-table-mutex)
                         thread))
                     (receive (package descriptor) (jazz:get-product-descriptor name)
                       (jazz:cond-expanded-product-descriptor-dependencies name descriptor))))
      (build name))
    
    (define (make name)
      (if (or (%%not jobs) (%%eqv? jobs 0) (jazz:debug-build?))
          (local-make name)
        (remote-make name)))
    
    (define (send-build-command established-port name)
      (write `(build ,name) established-port)
      (newline established-port)
      (force-output established-port))
    
    (parameterize ((current-user-interrupt-handler
                     (lambda ()
                       (set! stop-build? #t)
                       (for-each end-port/echo active-processes)
                       (for-each end-port/echo free-processes)
                       (exit))))
      (make name))
    (for-each end-port/echo free-processes)
    (if stop-build? 1 0)))


;;;
;;;; Worker
;;;


(define jazz:worker-port
  #f)

(define jazz:worker-product
  #f)


(define (jazz:worker-process port)
  (declare (proper-tail-calls))
  (let ((repl? (jazz:worker-repl?))
        (previous-handler (current-exception-handler))
        (established-port (open-tcp-client port)))
    (define (build product)
      (set! jazz:worker-product product)
      (jazz:reset-changed-units)
      (jazz:build-product product)
      (write (cons 'changes (jazz:get-changed-units)) established-port)
      (force-output established-port))
    
    (set! jazz:worker-port established-port)
    (let iter ()
         (let ((form (read established-port)))
           (if (or (eof-object? form)
                   (null? form))
               (close-port established-port)
             (let ((command (%%car form))
                   (arguments (%%cdr form)))
               (%%continuation-capture
                 (lambda (cont)
                   (with-exception-handler
                     (lambda (exc)
                       (if repl?
                           (begin
                             ;; closing the port would terminate the parent
                             ;; process making the console port unavailable
                             ;; (close-port established-port)
                             (write '(exception) established-port)
                             (force-output established-port)
                             (previous-handler exc))
                         (%%continuation-graft cont
                           (lambda ()
                             (if jazz:worker-product
                                 (begin
                                   (display "*** ERROR BUILDING ")
                                   (display jazz:worker-product)
                                   (display " -- ")))
                             (display-exception exc)
                             (force-output)
                             (exit)))))
                     (lambda ()
                       (case command
                         ((build)
                          (build (%%car arguments)))
                         (else
                          (error "Unknown worker command" command)))
                       (iter)))))))))))


;;;
;;;; Resource
;;;


(define jazz:default-char-encoding
  'UTF)


(define (jazz:resource-char-encoding resource)
  (or (%%get-package-char-encoding (%%get-resource-package resource))
      jazz:default-char-encoding))


(define (jazz:resource-package resource)
  (%%get-resource-package resource))


(define (jazz:resource-pathname resource)
  (jazz:package-root-pathname (%%get-resource-package resource)
    (jazz:resource-package-pathname resource)))


(define (jazz:resource-package-pathname resource)
  (let ((ext (%%get-resource-extension resource)))
    (if (%%not ext)
        (%%get-resource-path resource)
      (%%string-append (%%get-resource-path resource) "." ext))))


(define (jazz:name->path resource-name)
  (jazz:string-replace (%%symbol->string resource-name) #\. #\/))


(define (jazz:path->name resource-name)
  (%%string->symbol (jazz:string-replace resource-name #\/ #\.)))


(define (jazz:binary-with-extension src extension)
  (let* ((bindir (jazz:resource-build-dir src))
         (pathname (jazz:resource-pathname src))
         (name-base (jazz:pathname-base pathname)))
    (string-append bindir name-base extension)))


(define (jazz:probe-numbered-pathname pathname n)
  (let ((candidate (string-append pathname (%%number->string n))))
    (if (%%not (file-exists? candidate))
        candidate
      (jazz:probe-numbered-pathname pathname (%%fx+ n 1)))))


(define (jazz:for-each-numbered-pathname pathname n0 proc)
  (let iter ((n n0))
       (let ((candidate (string-append pathname (%%number->string n))))
         (if (file-exists? candidate)
             (begin
               (iter (%%fx+ n 1))
               (proc candidate))))))


(define (jazz:with-numbered-pathname pathname fresh? n0 proc)
  (let iter ((n n0) (exists? #f))
       (let ((candidate (string-append pathname (%%number->string n))))
         (if (%%not (file-exists? candidate))
             (if fresh?
                 (proc candidate exists?)
               (if exists?
                   (proc (string-append pathname (%%number->string (%%fx- n 1))) exists?)
                 (proc (string-append pathname (%%number->string n0)) exists?)))
           (iter (%%fx+ n 1) #t)))))


(define (jazz:product-library-name-base package path)
  (jazz:relocate-product-library-name-base (%%get-package-repository package) package path))


(define (jazz:relocate-product-library-name-base repository package path)
  (define (build-dir package)
    (let ((parent (%%get-package-parent package)))
      (jazz:repository-pathname repository
        (%%string-append (if parent (%%string-append (%%get-package-library-path parent) "/") "")
                         (%%symbol->string (%%get-package-name package))))))
  
  (string-append (build-dir package)
                 "/"
                 path))


;;;
;;;; Manifest
;;;


(define (jazz:manifest-pathname package resource)
  (jazz:package-root-pathname package
                              (%%string-append (%%get-resource-path resource)
                                               "."
                                               jazz:Manifest-Extension)))


(define (jazz:digest-pathname package resource)
  (jazz:package-root-pathname package
                              (%%string-append (%%get-resource-path resource)
                                               "."
                                               jazz:Digest-Extension)))


;;;
;;;; Load
;;;


(define jazz:load-indent
  (%%make-parameter 0))


(define (jazz:load-resource unit-name loading resource . rest)
  (let ((quiet? (if (%%null? rest) #f (%%car rest))))
    (if (jazz:unit-verbose?)
        (pretty-print unit-name))
    (jazz:with-verbose (jazz:load-verbose?) loading (jazz:resource-pathname resource)
      (lambda ()
        (jazz:load-file (%%list
                          path: (jazz:resource-pathname resource)
                          char-encoding: (jazz:resource-char-encoding resource))
                        quiet?)))))


(define jazz:verbose-port
  #f)

(define (jazz:set-verbose-port port)
  (set! jazz:verbose-port port))


(define (jazz:with-verbose flag action path proc)
  (let ((port (or jazz:verbose-port (repl-output-port))))
    (define (verbose-load)
      (display (%%make-string (jazz:load-indent) #\space) port)
      (display "; " port)
      (display action port)
      (display " " port)
      (display path port)
      (display "..." port)
      (newline port)
      (force-output port))
    
    (define (verbose-done)
      (display (%%make-string (jazz:load-indent) #\space) port)
      (display "; done " port)
      (display "..." port)
      (newline port)
      (force-output port))
    
    (if flag
        (begin
          (verbose-load)
          (let ((result
                  (parameterize ((jazz:load-indent (%%fx+ (jazz:load-indent) 2)))
                    (proc))))
            (if (jazz:done-verbose?)
                (verbose-done))
            result))
      (proc))))


(define jazz:load-interpreted-hook
  #f)

(define (jazz:get-load-interpreted-hook)
  jazz:load-interpreted-hook)

(define (jazz:set-load-interpreted-hook hook)
  (set! jazz:load-interpreted-hook hook))


;; quick hack so that once we switch repositories to target
;; we don't load any binary code anymore as it will be ios!
(define jazz:TARGET-HACK?
  #f)

(define (jazz:TARGET-HACK?-set! flag)
  (set! jazz:TARGET-HACK? flag))


(define (jazz:load-unit-src/bin unit-name #!key (force-source? #f))
  (jazz:with-unit-resources unit-name #f
    (lambda (src obj bin load-proc obj-uptodate? bin-uptodate? lib-uptodate? manifest)
      (if jazz:TARGET-HACK?
          (begin
            (set! obj #f)
            (set! bin #f)
            (set! load-proc #f)
            (set! obj-uptodate? #f)
            (set! bin-uptodate? #f)
            (set! lib-uptodate? #f)
            (set! manifest #f)))
      (parameterize ((jazz:requested-unit-name unit-name)
                     (jazz:requested-unit-resource (if bin-uptodate? bin src))
                     (jazz:requested-pathname #f))
        (cond ((and lib-uptodate? (not force-source?))
               (jazz:increment-image-load-counter)
               (jazz:with-verbose (jazz:load-verbose?) "loading lib" (symbol->string unit-name)
                 (lambda ()
                   (parameterize ((jazz:walk-for 'runtime))
                     (load-proc)))))
              ((and bin-uptodate? (not force-source?))
               (jazz:increment-object-load-counter)
               (let ((quiet? (or (%%not src) (let ((ext (%%get-resource-extension src)))
                                               (and ext (%%string=? ext "jazz"))))))
                 (parameterize ((jazz:walk-for 'runtime))
                   (jazz:load-resource unit-name "loading bin" bin quiet?))))
              (src
                (if (or (%%not jazz:load-interpreted-hook)
                        (%%not (jazz:load-interpreted-hook unit-name)))
                    (begin
                      (jazz:increment-interpreted-load-counter)
                      (let ((warn (jazz:warn-interpreted?)))
                        (if warn
                            (case warn
                              ((error)
                               (jazz:error "Loading {a} interpreted" unit-name))
                              ((stack)
                               (jazz:feedback "Warning: Loading {a} interpreted" unit-name)
                               (pp (jazz:current-load-stack)))
                              (else
                               (jazz:feedback "Warning: Loading {a} interpreted" unit-name)
                               (if (and (%%pair? warn) (%%memq unit-name warn))
                                   (pp (jazz:current-load-stack)))))))
                      (parameterize ((jazz:walk-for 'interpret)
                                     (jazz:generate-symbol-for "&")
                                     (jazz:generate-symbol-context unit-name)
                                     (jazz:generate-symbol-counter 0))
                        (jazz:with-extension-reader (%%get-resource-extension src)
                          (lambda ()
                            (jazz:load-resource unit-name "loading src" src)))))))
              (else
               (if force-source?
                   (jazz:error "Unable to find unit source: {s}" unit-name)
                 (jazz:error "Unable to find unit: {s}" unit-name))))))))


(define (jazz:unit-loadable? unit-name)
  (jazz:with-unit-resources unit-name #f
    (lambda (src obj bin load-proc obj-uptodate? bin-uptodate? lib-uptodate? manifest)
      (or lib-uptodate? bin-uptodate? src))))


(define (jazz:unit-obj-uptodate? unit-name)
  (jazz:with-unit-resources unit-name #f
    (lambda (src obj bin load-proc obj-uptodate? bin-uptodate? lib-uptodate? manifest)
      obj-uptodate?)))


(define (jazz:load-foundation)
  (jazz:load-unit 'foundation.dialect))


(define (jazz:load-runtime)
  (jazz:load-foundation)
  (jazz:load-unit 'core.unit.runtime))


(define (jazz:load-build)
  (jazz:load-foundation)
  (jazz:load-unit 'core.unit.runtime)
  (jazz:load-unit 'core.unit.build))


(define (jazz:load-install)
  (jazz:load-build))


(define (jazz:load-deploy)
  (jazz:load-build))


;;;
;;;; Evaluate
;;;


(define jazz:evaluate-forms-hook
  #f)

(define (jazz:get-evaluate-forms-hook)
  jazz:evaluate-forms-hook)

(define (jazz:set-evaluate-forms-hook hook)
  (set! jazz:evaluate-forms-hook hook))


;;;
;;;; Console
;;;


(define jazz:console-evaluate-hook
  #f)

(define (jazz:get-console-evaluate-hook)
  jazz:console-evaluate-hook)

(define (jazz:set-console-evaluate-hook hook)
  (set! jazz:console-evaluate-hook hook))


;;;
;;;; Build
;;;


(define (jazz:resource-build-dir resource)
  (jazz:relocate-resource jazz:Build-Repository resource))


(define (jazz:relocate-resource repository resource)
  (let ((package (%%get-resource-package resource))
        (dir (jazz:pathname-dir (%%get-resource-path resource))))
    (jazz:relocate-package-pathname repository package dir)))


;;;
;;;; State
;;;


(define jazz:Unloaded-State
  #f)

(define jazz:Loading-State
  '(loading))

(define jazz:Loaded-State
  '(loaded))

(define jazz:Error-State
  '(error))


;;;
;;;; Environment
;;;


(define jazz:Environment
  (%%make-table test: eq?))


(define (jazz:get-environment-table)
  jazz:Environment)


(define (jazz:get-environment-unit name)
  (%%table-ref jazz:Environment name jazz:Unloaded-State))


(define (jazz:set-environment-unit name unit)
  (%%table-set! jazz:Environment name unit))


;;;
;;;; Image Units
;;;


(define jazz:Image-Units
  (%%make-table test: eq?))

(define (jazz:get-image-unit name)
  (%%table-ref jazz:Image-Units name #f))

(define (jazz:set-image-unit name load-proc compile-time-hash)
  (%%table-set! jazz:Image-Units name (%%make-image-unit load-proc compile-time-hash)))


;;;
;;;; Foreign
;;;


(define jazz:foreign-libraries
  (%%make-table test: eq?))


(define (jazz:get-foreign-libraries)
  jazz:foreign-libraries)


(define (jazz:register-foreign-libraries unit-name . libraries)
  (%%table-set! jazz:foreign-libraries unit-name libraries))


(define (jazz:registered-foreign-libraries unit-name)
  (%%table-ref jazz:foreign-libraries unit-name #f))


(define (jazz:logging-foreign-libraries unit-name)
  (if (and jazz:kernel-debug-foreign? (jazz:logging?))
      (let ((libraries (jazz:registered-foreign-libraries unit-name)))
        (if libraries
            (jazz:logging-line (jazz:format ";**** libraries {l}" libraries))))))


;;;
;;;; Unit
;;;


(define jazz:Load-Mutex
  (%%make-mutex 'load))


(jazz:define-variable jazz:Load-Thread
  #f)


(define jazz:current-load-stack
  (%%make-parameter '()))


(define jazz:requested-unit-name
  (%%make-parameter #f))

(define jazz:requested-unit-resource
  (%%make-parameter #f))

(define jazz:requested-pathname
  (%%make-parameter #f))


(define jazz:compiled-source
  (%%make-parameter #f))


(define (jazz:get-load-mutex)
  jazz:Load-Mutex)

(define (jazz:get-load-thread)
  jazz:Load-Thread)


(define (jazz:call-with-load-lock thunk)
  (if (%%eq? jazz:Load-Thread (current-thread))
      (thunk)
    (dynamic-wind
      (lambda ()
        (mutex-lock! jazz:Load-Mutex)
        (set! jazz:Load-Thread (current-thread)))
      thunk
      (lambda ()
        (set! jazz:Load-Thread #f)
        (mutex-unlock! jazz:Load-Mutex)))))


(define (jazz:unit-loaded? unit-name)
  (%%eq? (jazz:get-environment-unit unit-name) jazz:Loaded-State))


(define (jazz:circular-dependency-error unit-name unit-name-list)
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
    (jazz:error "Circular dependency detected: {a}"
                (jazz:join-strings (map symbol->string circular-unit-list)
                                   " -> "))))


(define jazz:load-feedback-port
  #f)


(define jazz:load-feedback-mutex
  (make-mutex 'load-feedback))

(define (jazz:with-load-feedback-mutex thunk)
  (mutex-lock! jazz:load-feedback-mutex)
  (thunk)
  (mutex-unlock! jazz:load-feedback-mutex))


(define (jazz:load-feedback unit-name)
  (jazz:with-load-feedback-mutex
    (lambda ()
      (if jazz:load-feedback-port
          (with-exception-catcher
            (lambda (exc)
              (jazz:load-feedback-close))
            (lambda ()
              (write unit-name jazz:load-feedback-port)
              (newline jazz:load-feedback-port)
              (force-output jazz:load-feedback-port)))))))


(define (jazz:load-feedback-done)
  (jazz:with-load-feedback-mutex
    (lambda ()
      (if jazz:load-feedback-port
          (begin
            (write '(done) jazz:load-feedback-port)
            (newline jazz:load-feedback-port)
            (force-output jazz:load-feedback-port)
            (jazz:load-feedback-close))))))


(define (jazz:load-feedback-close)
  (with-exception-catcher
    (lambda (exc)
      #f)
    (lambda ()
      (close-port jazz:load-feedback-port)))
  (set! jazz:load-feedback-port #f))


(define (jazz:load-unit unit-name)
  (if (%%symbol? unit-name)
      (let ((unit-state (jazz:get-environment-unit unit-name)))
        (if (%%not (%%eq? unit-state jazz:Loaded-State))
            (jazz:call-with-load-lock ; unit-state might change while suspended
              (lambda ()
                (jazz:load-feedback unit-name)
                (let ((unit-state (jazz:get-environment-unit unit-name)))
                  (cond ((%%eq? unit-state jazz:Loading-State)
                         (jazz:circular-dependency-error unit-name (map cdr (jazz:current-load-stack))))
                        ((or (%%eq? unit-state jazz:Unloaded-State)
                             (%%eq? unit-state jazz:Error-State))
                         (parameterize ((jazz:current-load-stack (%%cons (%%cons ':load unit-name) (jazz:current-load-stack))))
                           (dynamic-wind
                             (lambda ()
                               (jazz:set-environment-unit unit-name jazz:Loading-State))
                             (lambda ()
                               (jazz:logging-foreign-libraries unit-name)
                               (jazz:load-unit-src/bin unit-name force-source?: (%%eq? unit-state jazz:Error-State))
                               (let ((unit-state (jazz:get-environment-unit unit-name)))
                                 (if (%%procedure? unit-state)
                                     (unit-state unit-name)))
                               (jazz:set-environment-unit unit-name jazz:Loaded-State))
                             (lambda ()
                               (if (%%eq? (jazz:get-environment-unit unit-name) jazz:Loading-State)
                                   (jazz:set-environment-unit unit-name jazz:Error-State))))))))))))
    (jazz:error "Unit name expected: {a}" unit-name)))


(define (jazz:load-hook unit-name hook)
  (jazz:call-with-load-lock
    (lambda ()
      (let ((unit-state (jazz:get-environment-unit unit-name)))
        (if (%%eq? unit-state jazz:Loaded-State)
            (jazz:error "Unit already loaded: {a}" unit-name)
          (jazz:set-environment-unit unit-name hook))))))


(define jazz:current-script-arguments
  (%%make-parameter '()))


(define jazz:load-script-hook
  #f)

(define (jazz:get-load-script-hook)
  jazz:load-script-hook)

(define (jazz:set-load-script-hook hook)
  (set! jazz:load-script-hook hook))


(define (jazz:load-script path)
  (if (or (%%not jazz:load-script-hook)
          (%%not (jazz:load-script-hook path)))
      (parameterize ((jazz:walk-for 'interpret)
                     (jazz:generate-symbol-for "&")
                     (jazz:generate-symbol-context (gensym))
                     (jazz:generate-symbol-counter 0)
                     (jazz:requested-unit-name #f)
                     (jazz:requested-unit-resource #f)
                     (jazz:requested-pathname path))
        (jazz:with-extension-reader (jazz:pathname-extension path)
          (lambda ()
            (jazz:load-file (%%list
                              path: path
                              char-encoding: 'UTF)
                            #t))))))


(define (jazz:unload-unit unit-name)
  (if (mutex-lock! jazz:Load-Mutex)
      (begin
        (jazz:set-environment-unit unit-name jazz:Unloaded-State)
        (mutex-unlock! jazz:Load-Mutex))
    ;; reaquire mutex
    (jazz:unload-unit unit-name)))


(define (jazz:reload-unit unit-name)
  (jazz:unload-unit unit-name)
  (jazz:load-unit unit-name))


;;;
;;;; Service
;;;


(define jazz:Services
  (%%make-table test: eq?))


(define (jazz:register-service name thunk)
  (%%table-set! jazz:Services name thunk))


(define (jazz:find-service name)
  (let ((symbol/proc (%%table-ref jazz:Services name #f)))
    (if (%%symbol? symbol/proc)
        (begin
          (jazz:load-unit symbol/proc)
          (set! symbol/proc (%%table-ref jazz:Services name #f))))
    (if symbol/proc
        (symbol/proc)
      #f)))


(define (jazz:require-service name)
  (or (jazz:find-service name)
      (error "Unknown service: {s}" name)))


;;;
;;;; Coupler
;;;


(define jazz:Couplers
  (%%make-table test: eq?))


(define (jazz:register-coupler name thunk)
  (%%table-set! jazz:Couplers name
    (cons thunk
          (%%table-ref jazz:Couplers name '()))))


(define (jazz:get-couplers name)
  (%%table-ref jazz:Couplers name '()))


(define (jazz:activate-couplers name)
  (for-each (lambda (proc)
              (proc))
            (jazz:get-couplers name))
  (%%table-clear jazz:Couplers name))


;;;
;;;; Literal
;;;


(define jazz:Literal-Constructors
  (%%make-table test: eq?))


(define (jazz:register-literal-constructor name constructor-reference constructor)
  (%%table-set! jazz:Literal-Constructors name (%%cons constructor-reference constructor)))


(define (jazz:require-literal-constructor name)
  (or (%%table-ref jazz:Literal-Constructors name #f)
      (jazz:error "Cannot construct literals of type {s}" name)))


(jazz:define-macro (jazz:define-literal name constructor-reference)
  (receive (constructor-module constructor-name) (jazz:break-reference constructor-reference)
    (let ((constructor-locator (jazz:compose-reference constructor-module constructor-name)))
      `(jazz:register-literal-constructor ',name ',constructor-reference
         (lambda (arguments)
           (jazz:load-unit ',constructor-module)
           (%%apply (jazz:global-ref ',constructor-locator) arguments))))))


(define (jazz:construct-literal name arguments)
  (let ((constructor (%%cdr (jazz:require-literal-constructor name))))
    (constructor arguments)))


(define jazz:Literal-Walkers
  (%%make-table test: eq?))


(define (jazz:register-literal-walker name walker-locator walker)
  (%%table-set! jazz:Literal-Walkers name (%%cons walker-locator walker)))


(jazz:define-macro (jazz:define-literal-walker name walker-reference)
  (receive (walker-module walker-name) (jazz:break-reference walker-reference)
    (let ((walker-locator (jazz:compose-reference walker-module walker-name)))
      `(jazz:register-literal-walker ',name ',walker-locator
         (lambda (arguments proc)
           (jazz:load-unit ',walker-module)
           ((jazz:global-ref ',walker-locator) arguments proc))))))


(define (jazz:walk-literal name arguments proc)
  (let ((info (%%table-ref jazz:Literal-Walkers name #f)))
    (if info
        (let ((walker (%%cdr info)))
          (walker arguments proc)))))


;;;
;;;; Reader
;;;


(define jazz:Extension-Readers
  (%%make-table test: equal?))


(define (jazz:get-extension-reader extension)
  (%%table-ref jazz:Extension-Readers extension #f))


(define (jazz:with-extension-reader extension thunk)
  (let ((readtable-getter (jazz:get-extension-reader extension)))
    (if readtable-getter
        (parameterize ((current-readtable (readtable-getter)))
          (thunk))
      (thunk))))


(define (jazz:with-resource-reader resource thunk)
  (jazz:with-extension-reader (%%get-resource-extension resource) thunk))


(define (jazz:register-reader-extension extension readtable-getter)
  (%%table-set! jazz:Extension-Readers extension readtable-getter))


;;;
;;;; Scheme
;;;


(define jazz:scheme-readtable
  (%%current-readtable))


(jazz:register-reader-extension "scm"
  (lambda ()
    jazz:scheme-readtable))


(jazz:register-reader-extension "jazz"
  (lambda ()
    (jazz:load-foundation)
    (jazz:load-unit 'jazz.dialect)
    jazz:jazz-readtable)))
