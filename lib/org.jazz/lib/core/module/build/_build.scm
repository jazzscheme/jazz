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


(module core.module.build


(require (core.base)
         (core.library))


;;;
;;;; Compile
;;;


(define (jazz.compile-module-internal module-name #!key (options #f) (cc-options #f) (ld-options #f) (force? #f))
  (jazz.with-module-src/bin module-name
    (lambda (src bin bin-uptodate?)
      (parameterize ((jazz.requested-module-name module-name)
                     (jazz.requested-module-resource src))
        (jazz.compile-source src bin bin-uptodate? module-name options: options cc-options: cc-options ld-options: ld-options force?: force?)))))


(define (jazz.compile-source src bin bin-uptodate? manifest-name #!key (options #f) (cc-options #f) (ld-options #f) (force? #f))
  (let ((options (or options jazz.compile-options))
        (cc-options (or cc-options ""))
        (ld-options (or ld-options "")))
    (if (or force? (%%not bin) (%%not bin-uptodate?))
        (let ((package (%%resource-package src))
              (path (%%resource-path src))
              (pathname (jazz.resource-pathname src))
              (bindir (jazz.resource-build-dir src)))
          (define (compile)
            (let ((build-package (jazz.copy-package package)))
              (display "; compiling ")
              (display path)
              (display "...")
              (force-output)
              (jazz.create-directories bindir)
              (jazz.with-extension-reader (%%resource-extension src)
                (lambda ()
                  (parameterize ((jazz.walk-for 'compile))
                    (compile-file pathname output: bindir options: options cc-options: cc-options ld-options: ld-options))))
              (let ((manifest-resource (%%make-resource build-package path jazz.Manifest-Extension))
                    (manifest-digest (jazz.resource-digest src)))
                (jazz.save-manifest manifest-resource (%%make-manifest manifest-name manifest-digest)))))
          (if jazz.compile-verbose?
              (let ((before (time->seconds (current-time))))
                (compile)
                (let ((after (time->seconds (current-time))))
                  (let ((delta (- after before)))
                    (display " ")
                    (display (/ (truncate (* delta 10)) 10))
                    (display "s"))))
            (compile))
          (newline)
          (force-output)))))


(define (jazz.copy-package package)
  (let* ((name (%%package-name package))
         (dir (%%string-append (%%symbol->string name) "/"))
         (path (%%string-append dir (%%symbol->string name) "." jazz.Package-Extension))
         (src (jazz.repository-pathname (%%package-repository package) path))
         (dst (jazz.repository-pathname jazz.Build-Repository path)))
    (if (and (jazz.file-exists? dst) (>= (jazz.file-modification-time dst) (jazz.file-modification-time src)))
        (jazz.repository-find-package jazz.Build-Repository name)
      (begin
        (jazz.create-directories (jazz.repository-pathname jazz.Build-Repository dir))
        (if (jazz.file-exists? dst)
            (jazz.file-delete dst))
        (jazz.file-copy src dst)
        (let ((package (jazz.load-package jazz.Build-Repository name dst)))
          (%%table-set! (%%repository-packages-table jazz.Build-Repository)
                        name
                        package)
          package)))))


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


(set! jazz.for-each-submodule
     (lambda (module-name proc)
       (let iter ((module-name module-name)
                  (feature-requirement #f)
                  (phase #f))
         (let ((declaration (jazz.locate-toplevel-declaration module-name)))
           (proc module-name declaration phase)
           (if (jazz.is? declaration jazz.Module-Declaration)
               (for-each (lambda (require)
                           (jazz.parse-require require iter))
                         (%%get-module-declaration-requires declaration))
             (begin
               (for-each (lambda (require)
                           (jazz.parse-require require iter))
                         (%%get-library-declaration-requires declaration))
               (for-each (lambda (export)
                           (let ((reference (%%get-library-invoice-library export)))
                             (if reference
                                 (let ((name (%%get-declaration-reference-name reference))
                                       (phase (%%get-library-invoice-phase export)))
                                   (iter name #f phase)))))
                         (%%get-library-declaration-exports declaration)))))))))
