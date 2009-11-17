;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Builder
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


(unit protected core.unit.builder


(require (core.base)
         (core.module))

;;;
;;;; Manifest
;;;


(define (jazz.manifest-references-valid? bin)
  (define (get-manifest-references)
    (let ((manifest-filepath (jazz.manifest-pathname (%%resource-package bin) bin)))
      (let ((manifest (jazz.load-manifest manifest-filepath)))
        (and manifest (%%manifest-references manifest)))))
  
  (define (module-references-valid? lst)
    (let ((module-locator (%%car lst))
          (module-references (%%cdr lst)))
      (let ((module-declaration (jazz.outline-module module-locator error?: #f)))
        (and module-declaration
             (jazz.every? (lambda (symbol)
                            (let ((found (if (%%pair? symbol)
                                             (let iter ((symbols symbol)
                                                        (declaration module-declaration))
                                                  (cond ((%%not declaration)
                                                         #f)
                                                        ((%%pair? symbols)
                                                         (iter (%%cdr symbols) (jazz.find-declaration declaration (%%car symbols))))
                                                        (else
                                                         declaration)))
                                           (jazz.find-declaration module-declaration symbol))))
                              (and found
                                   (%%eq? (%%get-lexical-binding-name (%%get-declaration-toplevel found)) module-locator)
                                   (%%neq? (%%get-declaration-access found) 'private))))
                          module-references)))))
  
  (let ((references (get-manifest-references)))
    (if references
        (jazz.every? module-references-valid? references)
      #f)))


;;;
;;;; Compile
;;;


(define (jazz.compile-unit-internal unit-name #!key (options #f) (cc-options #f) (ld-options #f) (force? #f))
  (jazz.with-unit-resources unit-name #f
    (lambda (src obj bin lib obj-uptodate? bin-uptodate? lib-uptodate? manifest)
      (parameterize ((jazz.requested-unit-name unit-name)
                     (jazz.requested-unit-resource src))
        (jazz.compile-source src obj bin obj-uptodate? bin-uptodate? unit-name options: options cc-options: cc-options ld-options: ld-options force?: force?)))))


(define jazz.wrap-single-host-cc-options
  (let ((gcc-4-2?
          (cond-expand
            (windows #f)
            (else (zero? (shell-command "gcc --version | grep -q 4.2."))))))
    (lambda (str)
      (if gcc-4-2? (string-append "-U___SINGLE_HOST " str) str))))

(define (jazz.compile-source src obj bin obj-uptodate? bin-uptodate? manifest-name #!key (options #f) (cc-options #f) (ld-options #f) (force? #f))
  (let ((options (or options jazz.compile-options))
        (cc-options (jazz.wrap-single-host-cc-options (or cc-options "")))
        (ld-options (or ld-options "")))
    (if (or force? (%%not (if (jazz.link-objects?)
                              (and obj-uptodate? bin-uptodate? (jazz.manifest-references-valid? bin))
                            (and obj-uptodate? (jazz.manifest-references-valid? obj)))))
        (let ((package (%%resource-package src))
              (path (%%resource-path src))
              (pathname (jazz.resource-pathname src))
              (bindir (jazz.resource-build-dir src)))
          (let ((build-package (jazz.find-build-package (%%package-name package))))
            (display "; compiling ")
            (display path)
            (display "...")
            (newline)
            (force-output)
            (jazz.create-directories bindir)
            (jazz.with-extension-reader (%%resource-extension src)
              (lambda ()
                (parameterize ((jazz.walk-for 'compile))
                  #;(compile-file pathname output: bindir options: options cc-options: cc-options ld-options: ld-options)
                  (jazz.compile-file src obj-uptodate? options: options cc-options: cc-options ld-options: ld-options unit-name: manifest-name))))
            (let ((manifest-filepath (jazz.manifest-pathname build-package src))
                  (src-filepath (jazz.resource-pathname src))
                  (references (let ((module-declaration (jazz.get-catalog-entry manifest-name)))
                                (cond ((%%is? module-declaration jazz.Module-Declaration)
                                       (jazz.generate-reference-list module-declaration))
                                      ((%%is? module-declaration jazz.Unit-Declaration)
                                       '())
                                      (else ; pure scheme
                                       '())))))
              (jazz.update-manifest-compile-time manifest-name manifest-filepath src-filepath references)))))))




(define (jazz.compile-file src link-only? #!key (options #f) (cc-options #f) (ld-options #f) (unit-name #f) (platform jazz.kernel-platform))
  (let* ((pathname (jazz.resource-pathname src))
         (bin- (jazz.binary-with-extension src ""))
         (bin-c (string-append bin- ".c"))
         (bin-o (string-append bin- ".o"))
         (bin-o1 (jazz.probe-numbered-pathname bin-o 1))
         (linkfile (string-append bin-o1 ".c")))

    (if (%%not link-only?)
        (begin
          (let ((patched-module-name (%%string-append "JAZZUNIT" (%%symbol->string unit-name))))
            (compile-file-to-c pathname output: bin-c options: options module-name: patched-module-name))
          (compile-file bin-c options: (%%cons 'obj options) cc-options: (string-append "-D___BIND_LATE " cc-options))))

    (if (jazz.link-objects?)
        (begin
          (link-flat (%%list bin-) output: linkfile warnings?: #f)
          (let ((exit-status
                  (##gambc-cc
                    'dyn
                    (jazz.resource-build-dir src)
                    (%%list linkfile bin-o)
                    bin-o1
                    cc-options
                    ""
                    ld-options
                    #f)))
            (if (not (= exit-status 0))
                (jazz.error "C compilation failed while linking module"))
            (delete-file linkfile)))
      
      ; else delete the .o1, .o2... files
      (jazz.for-each-numbered-pathname bin-o 1 delete-file))))

  
(define (jazz.find-build-package name)
  (jazz.repository-find-package jazz.Bin-Repository name))


;;;
;;;; Build
;;;


(define (jazz.build-unit-internal unit-name)
  (jazz.for-each-subunit unit-name
    (lambda (unit-name declaration phase)
      (jazz.compile-unit unit-name))))


;;;
;;;; Unit
;;;

(define (jazz.get-subunit-names-internal parent-name)
  (let* ((sub-units '())
         (proc (lambda (unit-name declaration phase)
                 (set! sub-units (%%cons unit-name sub-units)))))
    (jazz.for-each-subunit parent-name proc)
    sub-units))


(define (jazz.for-each-subunit parent-name proc)
  ;; temporary solution to the fact that exports can be present multiple times
  ;; if the unit is loaded interpreted or if dynamic evaluations where done
  (let ((subunits '()))
    (let iter ((unit-name parent-name) (phase #f) (toplevel? #t))
      (define (process-require require)
        (jazz.parse-require require
          (lambda (unit-name feature-requirement phase)
            (iter unit-name phase #f))))
      
      (if (%%not (%%memq unit-name subunits))
          (begin
            (set! subunits (%%cons unit-name subunits))
            (let ((declaration (jazz.outline-unit unit-name)))
              (if (or toplevel? (%%eq? (%%get-declaration-access declaration) 'protected))
                  (begin
                    (if (and (%%not toplevel?) (%%not (jazz.descendant-unit? parent-name unit-name)))
                        (jazz.error "Illegal access from {a} to protected unit {a}" parent-name unit-name))
                    (proc unit-name declaration phase)
                    (if (jazz.is? declaration jazz.Unit-Declaration)
                        (for-each process-require (%%get-unit-declaration-requires declaration))
                      (begin
                        (for-each process-require (%%get-module-declaration-requires declaration))
                        (for-each (lambda (export)
                                    (let ((reference (%%get-module-invoice-module export)))
                                      (if reference
                                          (let ((name (%%get-declaration-reference-name reference))
                                                (phase (%%get-module-invoice-phase export)))
                                            (iter name phase #f)))))
                                  (%%get-module-declaration-exports declaration)))))))))))))
