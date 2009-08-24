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


(module protected core.module.builder


(require (core.base)
         (core.library))


;;;
;;;; Manifest
;;;


(define (jazz.manifest-references-valid? bin)
  (define (get-manifest-references)
    (let ((manifest-filepath (jazz.manifest-pathname (%%resource-package bin) bin)))
      (let ((manifest (jazz.load-manifest manifest-filepath)))
        (and manifest (%%manifest-references manifest)))))
  
  (define (library-references-valid? lst)
    (let ((library-locator (%%car lst))
          (library-references (%%cdr lst)))
      (let ((library-declaration (jazz.outline-library library-locator #f)))
        (and library-declaration
             (jazz.every? (lambda (symbol)
                            (let ((found (if (%%pair? symbol)
                                             (let iter ((symbols symbol)
                                                        (declaration library-declaration))
                                                  (cond ((%%not declaration)
                                                         #f)
                                                        ((%%pair? symbols)
                                                         (iter (%%cdr symbols) (jazz.find-declaration declaration (%%car symbols))))
                                                        (else
                                                         declaration)))
                                           (jazz.find-declaration library-declaration symbol))))
                              (and found 
                                   (%%eq? (%%get-lexical-binding-name (%%get-declaration-toplevel found)) library-locator)
                                   (%%neq? (%%get-declaration-access found) 'private))))
                          library-references)))))
  
  (let ((references (get-manifest-references)))
    (if references
        (jazz.every? library-references-valid? references)
      #f)))


;;;
;;;; Compile
;;;


(define (jazz.compile-module-internal module-name #!key (options #f) (cc-options #f) (ld-options #f) (force? #f))
  (jazz.with-module-src/bin module-name #f
    (lambda (src bin bin-uptodate?)
      (parameterize ((jazz.requested-module-name module-name)
                     (jazz.requested-module-resource src))
        (jazz.compile-source src bin bin-uptodate? module-name options: options cc-options: cc-options ld-options: ld-options force?: force?)))))


(define (jazz.compile-source src bin bin-uptodate? manifest-name #!key (options #f) (cc-options #f) (ld-options #f) (force? #f))
  (let ((options (or options jazz.compile-options))
        (cc-options (or cc-options ""))
        (ld-options (or ld-options "")))
    (if (or force? (%%not (and bin bin-uptodate? (jazz.manifest-references-valid? bin))))
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
                  (compile-file pathname output: bindir options: options cc-options: cc-options ld-options: ld-options))))
            (let ((manifest-filepath (jazz.manifest-pathname build-package src))
                  (src-filepath (jazz.resource-pathname src))
                  (references (let ((library-declaration (jazz.get-catalog-entry manifest-name)))
                                (cond ((%%is? library-declaration jazz.Library-Declaration)
                                       (jazz.generate-reference-list library-declaration))
                                      ((%%is? library-declaration jazz.Module-Declaration)
                                       '())
                                      (else ; pure scheme
                                       '())))))
              (jazz.update-manifest-compile-time manifest-name manifest-filepath src-filepath references)))))))


(define (jazz.find-build-package name)
  (jazz.repository-find-package jazz.Bin-Repository name))


;;;
;;;; Build
;;;


(define (jazz.build-module-internal module-name)
  (jazz.for-each-submodule module-name
    (lambda (module-name declaration phase)
      (jazz.compile-module module-name))))


;;;
;;;; Module
;;;


(define (jazz.for-each-submodule parent-name proc)
  ;; temporary solution to the fact that exports can be present multiple times
  ;; if the module is loaded interpreted or if dynamic evaluations where done
  (let ((submodules '()))
    (let iter ((module-name parent-name) (phase #f) (toplevel? #t))
      (define (process-require require)
        (jazz.parse-require require
          (lambda (module-name feature-requirement phase)
            (iter module-name phase #f))))
      
      (if (%%not (%%memq module-name submodules))
          (begin
            (set! submodules (%%cons module-name submodules))
            (let ((declaration (jazz.outline-module module-name)))
              (if (or toplevel? (%%eq? (%%get-declaration-access declaration) 'protected))
                  (begin
                    (if (and (%%not toplevel?) (%%not (jazz.descendant-module? parent-name module-name)))
                        (jazz.error "Illegal access from {a} to protected module {a}" parent-name module-name))
                    (proc module-name declaration phase)
                    (if (jazz.is? declaration jazz.Module-Declaration)
                        (for-each process-require (%%get-module-declaration-requires declaration))
                      (begin
                        (for-each process-require (%%get-library-declaration-requires declaration))
                        (for-each (lambda (export)
                                    (let ((reference (%%get-library-invoice-library export)))
                                      (if reference
                                          (let ((name (%%get-declaration-reference-name reference))
                                                (phase (%%get-library-invoice-phase export)))
                                            (iter name phase #f)))))
                                  (%%get-library-declaration-exports declaration)))))))))))))
