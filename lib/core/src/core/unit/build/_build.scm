;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Build
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
;;;  Portions created by the Initial Developer are Copyright (C) 1996-2012
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


(unit core.unit.build


(require (core.base)
         (dialect))


;;;
;;;; Manifest
;;;


(define (jazz:manifest-references-valid? bin)
  (define (get-manifest)
    (let ((digest-filepath (jazz:digest-pathname (%%get-resource-package bin) bin))
          (manifest-filepath (jazz:manifest-pathname (%%get-resource-package bin) bin)))
      (jazz:load-manifest digest-filepath manifest-filepath)))
  
  (define (module-references-valid? version lst)
    (define (recompile-reference? module-locator module-references)
      (%%continuation-capture
        (lambda (return)
          (jazz:for-each-higher-jazz-version version
            (lambda (jazz-version)
              (let ((recompile-references (jazz:get-version-recompile-references jazz-version)))
                (if (and recompile-references
                         (jazz:some? (lambda (recompile-reference)
                                       (if (%%symbol? recompile-reference)
                                           (%%eq? recompile-reference module-locator)
                                         (and (%%eq? (%%car recompile-reference) module-locator)
                                              (jazz:some? (lambda (recompile-symbol)
                                                            (%%memq recompile-symbol module-references))
                                                          (%%cdr recompile-reference)))))
                                     recompile-references))
                    (%%continuation-return return #t)))))
          #f)))
    
    (let ((module-locator (%%car lst))
          (module-references (%%cdr lst)))
      (and (%%not (recompile-reference? module-locator module-references))
           (let ((module-declaration (jazz:outline-module module-locator error?: #f)))
             (and module-declaration
                  (jazz:every? (lambda (symbol)
                                 (let ((found (if (%%pair? symbol)
                                                  (let iter ((symbols symbol)
                                                             (declaration module-declaration))
                                                       (cond ((%%not declaration)
                                                              #f)
                                                             ((%%pair? symbols)
                                                              (iter (%%cdr symbols) (jazz:find-declaration declaration (%%car symbols))))
                                                             (else
                                                              declaration)))
                                                (jazz:find-public-declaration module-declaration symbol))))
                                   (and found
                                        (%%eq? (jazz:get-lexical-binding-name (jazz:get-declaration-toplevel found)) module-locator))))
                               module-references))))))
  
  (let ((manifest (get-manifest)))
    (if manifest
        (let ((version (%%get-manifest-version manifest))
              (references (%%get-manifest-references manifest)))
          (if references
              (jazz:every? (lambda (lst)
                             (module-references-valid? version lst))
                           references)
            #f))
      #f)))


;;;
;;;; Compile
;;;


(jazz:define-variable-override jazz:compile-unit-internal
  (lambda (unit-name #!key (output-language #f) (options #f) (cc-options #f) (ld-options #f) (force? #f))
    (jazz:with-unit-resources unit-name #f
      (lambda (src obj bin lib obj-uptodate? bin-uptodate? lib-uptodate? manifest)
        (if src
            (parameterize ((jazz:requested-unit-name unit-name)
                           (jazz:requested-unit-resource src))
              (jazz:compile-source src obj bin obj-uptodate? bin-uptodate? unit-name output-language: output-language options: options cc-options: cc-options ld-options: ld-options force?: force?))
          (%%unless (and bin (file-exists? (jazz:resource-pathname (jazz:bin->otl bin))))
            (jazz:error "Unable to find source for: {s}" unit-name)))))))


;; this function should be unified with jazz:compile-unit-internal
;; (being careful about jazz:find-unit-product overhead)
(jazz:define-variable-override jazz:custom-compile-unit-internal
  (lambda (unit-name #!key (force? #f))
    (let ((product (jazz:find-unit-product unit-name)))
      (let ((build (and product
                        (%%get-product-build product))))
        (if build
            (build (%%get-product-descriptor product) unit: unit-name force?: force?)
          (jazz:compile-unit unit-name force?: force?))))))


(define (jazz:find-unit-product unit-name)
  (let ((src (jazz:find-unit-src unit-name #f)))
    (let ((package (%%get-resource-package src)))
      (let ((products (%%get-package-products package)))
        (continuation-capture
          (lambda (return)
            (for-each
              (lambda (product-descriptor)
                (let* ((product-name (jazz:product-descriptor-name product-descriptor))
                       (product (jazz:get-product product-name))
                       (update-descriptor (jazz:product-descriptor-update product-descriptor)))
                  (let ((update (jazz:cond-expanded-product-descriptor-update product-name product-descriptor)))
                    (for-each (lambda (unit)
                                (jazz:for-each-subunit unit
                                  (lambda (sub-unit declaration phase)
                                    (if (eq? sub-unit unit-name)
                                        (continuation-return return product)))))
                              update))))
              products)
            #f))))))


(define jazz:wrap-single-host-cc-options
  (let ((gcc-4-2?
          (cond-expand
            (windows #f)
            (else (zero? (shell-command "gcc --version | grep -q 4.2."))))))
    (lambda (str)
      (if (or jazz:debug-user? gcc-4-2?) (string-append "-U___SINGLE_HOST " str) str))))


(define (jazz:compile-source src obj bin obj-uptodate? bin-uptodate? manifest-name #!key (output-language #f) (options #f) (cc-options #f) (ld-options #f) (force? #f))
  (let ((references-valid? (and (or obj-uptodate? bin-uptodate?) (jazz:manifest-references-valid? (or obj bin)))))
    (let ((options (or options jazz:compile-options))
          (cc-options (jazz:wrap-single-host-cc-options (or cc-options "")))
          (ld-options (or ld-options ""))
          (update-obj? (or force? (not obj-uptodate?) (not references-valid?)))
          (update-bin? (or force? (not bin-uptodate?) (not references-valid?))))
      (let ((compile? (or update-obj? (and update-bin? (jazz:link-objects?)))))
        (if compile?
            (let ((package (%%get-resource-package src))
                  (pathname (jazz:resource-pathname src))
                  (bindir (jazz:resource-build-dir src)))
              (let ((build-package (jazz:create-build-package package)))
                (jazz:create-directories bindir)
                (jazz:with-extension-reader (%%get-resource-extension src)
                  (lambda ()
                    (parameterize ((jazz:walk-for 'compile))
                      (jazz:compile-file src bin update-obj? update-bin? build-package output-language: output-language options: options cc-options: cc-options ld-options: ld-options unit-name: manifest-name)))))))
        (if (or compile? (jazz:force-outlines?))
            (let ((path (jazz:binary-with-extension src ".otl")))
              (jazz:create-directories (jazz:pathname-dir path))
              (call-with-output-file (list path: path eol-encoding: (jazz:platform-eol-encoding jazz:kernel-platform))
                (lambda (output)
                  (let ((declaration (jazz:outline-unit manifest-name)))
                    (jazz:outline-generate declaration output))))))))))


(define (jazz:compile-file src bin update-obj? update-bin? build-package #!key (output-language #f) (options #f) (cc-options #f) (ld-options #f) (unit-name #f) (platform jazz:kernel-platform))
  (define bin-pathname-base
    (jazz:binary-with-extension src ""))
  
  (define bin-extension
    (case (or output-language 'c)
      ((objc) ".m")
      (else ".c")))
  
  (define (compile)
    (let ((unique-module-name (%%string-append jazz:unit-uniqueness-prefix (%%symbol->string unit-name)))
          (src-pathname (jazz:resource-pathname src))
          (bin-output (string-append bin-pathname-base bin-extension)))
      (parameterize ((jazz:generate-symbol-for "^")
                     (jazz:generate-symbol-context unit-name)
                     (jazz:generate-symbol-counter 0)
                     (jazz:compiled-source src))
        ;; temporary until a cleaner solution
        (set! ##gensym-counter -1)
        (if (not (and (compile-file-to-target src-pathname output: bin-output options: options module-name: unique-module-name)
                      (compile-file bin-output options: (%%cons 'obj options) cc-options: (string-append "-D___DYNAMIC " cc-options))))
            (jazz:error "compilation failed")))))
  
  (define (update-manifest)
    (let ((digest-filepath (jazz:digest-pathname build-package src))
          (manifest-filepath (jazz:manifest-pathname build-package src))
          (src-filepath (jazz:resource-pathname src))
          (references (let ((module-declaration (jazz:get-catalog-entry unit-name)))
                        (cond ((%%is? module-declaration jazz:Module-Declaration)
                               (jazz:generate-reference-list module-declaration))
                              ((%%is? module-declaration jazz:Unit-Declaration)
                               '())
                              (else ; pure scheme
                               '())))))
      (jazz:update-manifest-compile-time unit-name digest-filepath manifest-filepath src-filepath references)))
  
  (define (delete-o1-file file)
    (with-exception-catcher
      (lambda (exc)
        (jazz:error "Compilation failed while deleting binary file {a}. A process may be using it." file))
      (lambda ()
        (delete-file file))))
  
  (define (delete-o1-files)
    (jazz:for-each-numbered-pathname (string-append bin-pathname-base ".o") 1 delete-o1-file))
  
  (define (determine-o1)
    (if jazz:single-objects?
        (begin
          (delete-o1-files)
          (string-append bin-pathname-base ".o1"))
      (jazz:probe-numbered-pathname (string-append bin-pathname-base ".o") 1)))
  
  (define (link-o1)
    (let ((bin-o1 (determine-o1)))
      (let ((bin-output (string-append bin-pathname-base bin-extension))
            (linkfile (string-append bin-o1 bin-extension)))
        (link-flat (%%list bin-output) output: linkfile warnings?: #f)
        (let ((exit-status
                (##gambc-cc
                  'dyn
                  (jazz:resource-build-dir src)
                  (%%list linkfile (string-append bin-pathname-base ".o"))
                  bin-o1
                  cc-options
                  ""
                  ld-options
                  #f)))
          (if (not (= exit-status 0))
              (jazz:error "C compilation failed while linking module"))
          (case platform
            ((windows)
             (if jazz:single-objects?
                 (jazz:obliterate-PE-timestamp bin-o1 'DLL))))
          (delete-file linkfile)))))
  
  (let ((will-link? (and update-bin? (or (jazz:link-objects?) (and bin (not jazz:single-objects?))))))
    (let ((will-compile? (and update-obj? (or will-link? (jazz:link-libraries?))))
          (dry? (jazz:dry-run?)))
      (if (or will-compile? will-link?)
          (let ((path (%%get-resource-path src)))
            (jazz:push-changed-units path)
            (display "; compiling ")
            (display path)
            (display "...")
            (newline)
            (force-output)))
      (if (not dry?)
          (begin
            (if will-compile? (compile))
            (if update-bin?
                (if will-link?
                    (link-o1)
                  (delete-o1-files)))
            (if will-compile? (update-manifest)))))))


;;;
;;;; Build
;;;


(jazz:define-variable-override jazz:build-unit-internal
  (lambda (unit-name)
    (jazz:for-each-subunit unit-name
      (lambda (unit-name declaration phase)
        (jazz:compile-unit unit-name)))))


;;;
;;;; Unit
;;;


(jazz:define-variable-override jazz:get-subunit-names-internal
  (lambda (parent-name)
    (let* ((sub-units '())
           (proc (lambda (unit-name declaration phase)
                   (set! sub-units (%%cons unit-name sub-units)))))
      (jazz:for-each-subunit parent-name proc)
      sub-units))))
