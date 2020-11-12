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
                                        (or (%%eq? (jazz:get-lexical-binding-name (jazz:get-declaration-toplevel found)) module-locator)
                                            ;; GAZOUM mega quick dirty hack around the problem that because hubs do not
                                            ;; trigger import conflicts then this code is totally brittle and it is possible
                                            ;; to find a hub in one module and next time in another thus triggering incessant
                                            ;; recompilations
                                            (and jazz:manifest-ignore?
                                                 (jazz:manifest-ignore? found))))))
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


(jazz:define-variable-override jazz:manifest-valid?
  jazz:manifest-references-valid?)


;;;
;;;; Compile
;;;


(jazz:define-variable-override jazz:compile-unit-internal
  (lambda (unit-name #!key (output-language #f) (options #f) (custom-cc #f) (cc-options #f) (ld-options #f) (rpaths #f) (skip-references? #f) (force? #f))
    (jazz:with-unit-resources unit-name #f
      (lambda (src obj bin lib obj-uptodate? bin-uptodate? lib-uptodate? manifest)
        (if src
            (parameterize ((jazz:requested-unit-name unit-name)
                           (jazz:requested-unit-resource src)
                           (jazz:requested-pathname #f))
              (jazz:compile-source src obj bin obj-uptodate? bin-uptodate? unit-name output-language: output-language options: options custom-cc: custom-cc cc-options: cc-options ld-options: ld-options rpaths: rpaths skip-references?: skip-references? force?: force?))
          (%%unless (and bin (file-exists? (jazz:resource-pathname (jazz:bin->otl bin))))
            (jazz:error "Unable to find source for: {s}" unit-name)))))))


;; this function should be unified with jazz:compile-unit-internal
;; (being careful about jazz:find-unit-product overhead)
(jazz:define-variable-override jazz:custom-compile-unit-internal
  (lambda (unit-name #!key (options #f) (skip-references? #f) (force? #f))
    (let ((product (jazz:find-unit-product unit-name)))
      (let ((build (and product
                        (%%get-product-build product))))
        (if build
            (build (%%get-product-descriptor product) unit: unit-name skip-references?: skip-references? force?: force?)
          (jazz:compile-unit unit-name options: options skip-references?: skip-references? force?: force?))))))


(define (jazz:find-unit-product unit-name)
  (let ((src (jazz:find-unit-src unit-name)))
    (let ((package (%%get-resource-package src)))
      (let ((products (%%get-package-products package)))
        (continuation-capture
          (lambda (return)
            ;; first go through all units in all updates
            (for-each (lambda (product-descriptor)
                        (let* ((product-name (jazz:product-descriptor-name product-descriptor))
                               (product (jazz:get-product product-name)))
                          (let ((update (jazz:cond-expanded-product-descriptor-update product-name product-descriptor)))
                            (for-each (lambda (unit)
                                        (if (eq? unit unit-name)
                                            (continuation-return return product)))
                                      update))))
                      products)
            ;; then do the much more costly subunits scan
            (for-each (lambda (product-descriptor)
                        (let* ((product-name (jazz:product-descriptor-name product-descriptor))
                               (product (jazz:get-product product-name)))
                          (let ((update (jazz:cond-expanded-product-descriptor-update product-name product-descriptor)))
                            (for-each (lambda (unit)
                                        (jazz:for-each-subunit unit
                                          (lambda (sub-unit declaration phase)
                                            (if (eq? sub-unit unit-name)
                                                (continuation-return return product)))))
                                      update))))
                      products)
            #f))))))


(define (jazz:find-unit-options unit-name)
  (let ((options (%%get-product-options (jazz:find-unit-product unit-name))))
    (and options
         (let ((pair (%%assq unit-name options)))
           (and pair
                (%%cdr pair))))))


;; at the moment building large files like functional in develop is much too
;; slow because of the safe declare, hence this least of two evils solution
(define (jazz:wrap-single-host-cc-options str)
  (if (or (eq? jazz:kernel-safety 'core)
          (eq? jazz:kernel-safety 'debug)
          (eq? jazz:kernel-safety 'develop))
      (string-append "-U___SINGLE_HOST " str)
    str))


;; compilation time is just too long in core
(define (jazz:wrap-no-optimize-cc-options str)
  (if (eq? jazz:kernel-safety 'core)
      (string-append "-O0 " str)
    str))


;; hack around some antivirus like avast and avg
;; returning sometimes an evo-gen false positive
(define (jazz:wrap-antivirus-hack-around manifest-name str)
  (cond-expand
    (windows (case manifest-name
               ((jazz.ui.view.Tab-View)
                (string-append "-Os " str))
               ((jazz.project.project.Project)
                (string-append "-Os " str))
               (else
                str)))
    (else str)))


(define jazz:custom-cc-path
  (let ((cache (make-table test: eq?)))
    (lambda (custom-cc)
      (or (%%table-ref cache custom-cc #f)
          (let ((path
                  (case custom-cc
                    ((llvm)
                     (string-append "/usr/bin/" (jazz:compiler-name))))))
            (%%table-set! cache custom-cc path)
            path)))))

(define jazz:custom-cc-arguments
  (let ((cache (make-table test: eq?)))
    (lambda (custom-cc)
      (or (%%table-ref cache custom-cc #f)
          (let ((path (%%string-append (%%path-expand "~~bin") "gambcomp-C" jazz:os-bat-extension-string-saved)))
            (define (read-info argument)
              (let ((port (open-process (list path: path arguments: (list argument)))))
                (let ((info (jazz:remove "" (jazz:split-string (read-line port) #\space) test: equal?)))
                  (close-port port)
                  info)))
            
            ;; too many differences between LLVM and GCC to use FLAGS_DYN
            ;; this list was copied from a Gambit LLVM build
            (define jazz:llvm-arguments
              '("-O1"
                "-Wno-unused"
                "-Wno-write-strings"
                "-Wdisabled-optimization"
                "-fno-trapping-math"
                "-fno-math-errno"
                "-fwrapv"
                "-fno-strict-aliasing"
                "-fomit-frame-pointer"
                "-fPIC"
                "-fno-common"
                "-mieee-fp"
                ;; "-fschedule-insns2"
                ;; "-mpc64"
                ))

            (let ((flags jazz:llvm-arguments)
                  (defs (read-info "DEFS_DYN")))
              (let ((arguments (%%append flags defs)))
                (%%table-set! cache custom-cc arguments)
                arguments)))))))


(define (jazz:compile-source src obj bin obj-uptodate? bin-uptodate? manifest-name #!key (output-language #f) (options #f) (custom-cc #f) (cc-options #f) (ld-options #f) (rpaths #f) (skip-references? #f) (force? #f))
  (let ((references-valid? (or skip-references? (and (or obj-uptodate? bin-uptodate?) (jazz:manifest-references-valid? (or obj bin))))))
    (let ((options (or options jazz:compile-options))
          (cc-options (jazz:wrap-single-host-cc-options (jazz:wrap-no-optimize-cc-options (jazz:wrap-antivirus-hack-around manifest-name (or cc-options "")))))
          (ld-options (or ld-options ""))
          (update-obj? (or force? (not obj-uptodate?) (not references-valid?)))
          (update-bin? (or force? (not bin-uptodate?) (not references-valid?))))
      (let ((compile? (or update-obj? (and update-bin? (jazz:link-objects?)))))
        (if compile?
            (let ((package (%%get-resource-package src))
                  (pathname (jazz:resource-pathname src))
                  (bindir (jazz:resource-build-dir src)))
              (let ((build-package (jazz:load/create-build-package package)))
                (jazz:create-directories bindir)
                (jazz:with-extension-reader (%%get-resource-extension src)
                  (lambda ()
                    (parameterize ((jazz:walk-for 'compile))
                      (jazz:compile-source-file src bin update-obj? update-bin? build-package output-language: output-language options: options custom-cc: custom-cc cc-options: cc-options ld-options: ld-options rpaths: rpaths unit-name: manifest-name)))))))
        (if (or compile? (jazz:force-outlines?))
            (let ((path (jazz:binary-with-extension src ".otl")))
              (jazz:create-directories (jazz:pathname-dir path))
              (call-with-output-file (list path: path eol-encoding: (jazz:platform-eol-encoding jazz:kernel-platform))
                (lambda (output)
                  (let ((declaration (jazz:outline-unit manifest-name)))
                    (jazz:outline-generate declaration output))))))))))


(define (jazz:compile-source-file src bin update-obj? update-bin? build-package #!key (output-language #f) (options #f) (custom-cc #f) (cc-options #f) (ld-options #f) (rpaths #f) (unit-name #f) (platform jazz:kernel-platform))
  (define bin-pathname-base
    (jazz:binary-with-extension src ""))
  
  (define bin-extension
    (%%string-append "." (jazz:language-extension (or output-language 'c))))
  
  (define mac?
    (eq? jazz:kernel-platform 'mac))
  
  (define ios?
    (and (jazz:build-configuration) (eq? (jazz:get-configuration-platform (jazz:build-configuration)) 'ios)))
  
  (define ios-processor
    (and ios? (or (jazz:get-configuration-processor (jazz:build-configuration)) 'x86)))
  
  (define ios-architecture
    (and ios? (case ios-processor
                ((x86) 'x86_64)
                (else 'arm64))))
  
  (define ios-sysroot
    (and ios? (case ios-architecture
                ((x86_64) "/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneSimulator.platform/Developer/SDKs/iPhoneSimulator.sdk")
                (else "/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS.sdk"))))
  
  (define ios-custom-cc
    (and ios? "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/clang"))
  
  (define ios-custom-cc-options
    (and ios? (list "-arch" (symbol->string ios-architecture) "-fmessage-length=0" "-I/Users/magnan/Downloads/couchbase-lite-ios-enterprise_1-3/CouchbaseLite.framework/Headers" "-fdiagnostics-show-note-include-stack" "-fmacro-backtrace-limit=0" "-Wno-c++11-compat-deprecated-writable-strings" "-fmodules" "-gmodules"
      "-Wnon-modular-include-in-framework-module" "-Werror=non-modular-include-in-framework-module" "-Wno-trigraphs" "-fpascal-strings" "-O0"
      "-fno-common" "-fno-objc-arc" "-Wno-shift-negative-value" "-Wno-missing-field-initializers" "-Wno-missing-prototypes" "-Werror=return-type" "-Wunreachable-code"
      "-Werror=deprecated-objc-isa-usage" "-Werror=objc-root-class" "-Wno-missing-braces" "-Wparentheses" "-Wswitch" "-Wempty-body"
      "-Wconditional-uninitialized" "-Wno-unknown-pragmas" "-Wno-shadow" "-Wno-four-char-constants" "-Wno-conversion" "-Wconstant-conversion"
      "-Wint-conversion" "-Wbool-conversion" "-Wenum-conversion" "-Wno-newline-eof"
      "-isysroot" ios-sysroot "-fasm-blocks" "-fstrict-aliasing" "-Wdeprecated-declarations" "-mios-simulator-version-min=9.0" "-Wno-sign-conversion")))
  
  (define ios-gambit-include-dir
    (path-expand "~~ios/include"))
  
  (define (compile)
    (let ((unique-module-name (%%string-append jazz:bin-uniqueness-prefix (%%symbol->string unit-name)))
          (src-pathname (jazz:resource-pathname src))
          (bin-output (string-append bin-pathname-base bin-extension))
          (link-file-output (string-append bin-pathname-base "_" bin-extension)))
      (parameterize ((jazz:generate-symbol-for "^")
                     (jazz:generate-symbol-context unit-name)
                     (jazz:compiled-source src))
        ;; temporary until a cleaner solution
        (jazz:set-gensym-counter! -1)
        (if (not (and (compile-file-to-target src-pathname output: bin-output options: options module-name: unique-module-name)
                      (if ios?
                          (let ((custom-cc-options ios-custom-cc-options))
                            (jazz:call-process
                              (list
                                path: ios-custom-cc
                                arguments: `(,@custom-cc-options ,(%%string-append "-I" ios-gambit-include-dir) "-D___DYNAMIC" ,@(jazz:split-string cc-options #\space) "-c" "-o" ,(string-append bin-pathname-base ".o") ,bin-output)
                                show-console: #f)))
                        (if custom-cc
                            (let ((gambit-include-dir (path-expand "~~include")))
                              (case custom-cc
                                ((llvm)
                                 (jazz:call-process
                                   (list
                                     path: (jazz:custom-cc-path custom-cc)
                                     arguments: `(,@(jazz:custom-cc-arguments custom-cc) ,(%%string-append "-I" gambit-include-dir) ,@(jazz:split-string cc-options #\space) "-c" "-o" ,(string-append bin-pathname-base ".o") ,bin-output)
                                     show-console: #f)))))
                          (compile-file bin-output options: (%%cons 'obj options) cc-options: (string-append "-D___DYNAMIC " cc-options))))))
            (jazz:error "Compilation failed")))))
  
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
                (if ios?
                    (let ((custom-cc-options (cons "-bundle" ios-custom-cc-options))
                          (link-options (case jazz:kernel-compiler ((c++) '("-lc++" "-framework" "Foundation" "-framework" "UIKit" "-framework" "CoreText")) (else '()))))
                      (jazz:invoke-process
                        (list
                          path: ios-custom-cc
                          arguments: `(,@custom-cc-options ,@link-options ,(%%string-append "-I" ios-gambit-include-dir) ,@(jazz:split-string cc-options #\space) ,@(jazz:split-string ld-options #\space) ,linkfile ,(string-append bin-pathname-base ".o") "-o" ,bin-o1)
                          show-console: #f)))
                  (jazz:gambitcomp
                    'dyn
                    (jazz:resource-build-dir src)
                    (%%list linkfile (string-append bin-pathname-base ".o"))
                    bin-o1
                    cc-options
                    ""
                    (case platform
                      ((mac)
                       (string-append ld-options " -headerpad_max_install_names"))
                      (else
                       ld-options))
                    options))))
          (if (not (= exit-status 0))
              (jazz:error "C compilation failed while linking module"))
          (let ((id (and (jazz:global-bound? 'apple-developer-id)
                         (jazz:global-ref 'apple-developer-id))))
            (if id
                (cond #;
                      (mac?
                       (jazz:call-process
                         (list
                           path: "/usr/bin/codesign"
                           arguments: `("--sign" ,id ,bin-o1)
                           show-console: #f)))
                      (ios?
                       (jazz:call-process
                         (list
                           path: "/usr/bin/codesign"
                           arguments: `("--force" "--sign" ,id "--preserve-metadata=identifier,entitlements" "--timestamp=none" ,bin-o1)
                           show-console: #f))))))
          (case platform
            ((mac)
             (if rpaths
                 (for-each (lambda (rpath)
                             (jazz:call-process
                               (list
                                 path: "install_name_tool"
                                 arguments: `("-add_rpath" ,rpath ,bin-o1))))
                           rpaths))))
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


(define (jazz:build-unit-internal-impl unit-name)
  (jazz:for-each-subunit unit-name
    (lambda (unit-name declaration phase)
      (jazz:compile-unit unit-name))))


;;;
;;;; Unit
;;;


(define (jazz:get-subunit-names-internal-impl parent-name #!key (walk-continue? #f))
  (let ((sub-units '()))
    (let ((proc (lambda (unit-name declaration phase)
                  (set! sub-units (%%cons unit-name sub-units)))))
      (parameterize ((jazz:for-each-subunit-continue
                       (and walk-continue? (lambda (unit-name exc)
                                             (if (%%is? exc jazz:Walk-Problems)
                                                 (set! sub-units (%%cons unit-name sub-units)))))))
        (jazz:for-each-subunit parent-name proc))
      sub-units)))


;;;
;;;; Override
;;;


(jazz:define-variable-override jazz:build-unit-internal jazz:build-unit-internal-impl)
(jazz:define-variable-override jazz:get-subunit-names-internal jazz:get-subunit-names-internal-impl))
