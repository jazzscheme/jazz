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
;;;  Portions created by the Initial Developer are Copyright (C) 1996-2006
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


(define (jazz.compile-module module-name #!key (options #f) (cc-options #f) (ld-options #f) (force? #f))
  (let ((src (jazz.find-module-src module-name)))
    (parameterize ((jazz.requested-module-name module-name))
      (jazz.compile-source src module-name options: options cc-options: cc-options ld-options: ld-options force?: force?))))


(define (jazz.compile-source src manifest-name #!key (options #f) (cc-options #f) (ld-options #f) (digest #f) (force? #f))
  (let ((options (or options jazz.compile-options))
        (cc-options (or cc-options ""))
        (ld-options (or ld-options "")))
    (jazz.with-src/bin src
      (lambda (src bin bin-uptodate?)
        (if (or force? (%%not bin) (%%not bin-uptodate?))
            (let ((package (%%resource-package src))
                  (path (%%resource-path src))
                  (pathname (jazz.resource-pathname src))
                  (bindir (jazz.resource-build-dir src)))
              (jazz.compile-verbose path)
              (jazz.create-directories bindir)
              (jazz.copy-package package)
              (jazz.with-extension-reader (%%resource-extension src)
                (lambda ()
                  (parameterize ((jazz.walk-for 'compile))
                    (compile-file pathname output: bindir options: options cc-options: cc-options ld-options: ld-options))))
              (let ((manifest-resource (%%make-resource (jazz.repository-find-package jazz.Build-Repository (%%package-name package)) path jazz.Manifest-Extension))
                    ;; remove explicit digest passing when gambit bug that forces us to generate a jscm is fixed
                    (manifest-digest (or digest (jazz.resource-digest src))))
                (jazz.save-manifest manifest-resource (%%make-manifest manifest-name manifest-digest)))))))))


(define (jazz.compile-verbose path)
  (display "; compiling ")
  (display path)
  (display " ...")
  (newline)
  (force-output))


(define (jazz.copy-package package)
  (let ((name (%%package-name package)))
    (let ((path (%%string-append (%%symbol->string name) "/" (%%symbol->string name) "." jazz.Package-Extension)))
      (let ((src (jazz.repository-pathname (%%package-repository package) path))
            (dst (jazz.repository-pathname jazz.Build-Repository path)))
        (if (or (%%not (jazz.file-exists? dst)) (< (jazz.file-modification-time dst) (jazz.file-modification-time src)))
            (begin
              (jazz.file-copy src dst)
              (%%table-set! (%%repository-packages-table jazz.Build-Repository)
                            name
                            (jazz.load-package jazz.Build-Repository name dst))))))))


;;;
;;;; Build
;;;


(define (jazz.build-module module-name)
  (jazz.for-each-submodule module-name
    (lambda (module-name declaration phase)
      (jazz.compile-module module-name))))


;;;
;;;; Module
;;;


(define (jazz.for-each-submodule module-name proc)
  (let iter ((module-name module-name)
             (feature-requirement #f)
             (phase #f))
    (let ((declaration (jazz.locate-toplevel-declaration module-name)))
      (proc module-name declaration phase)
      (if (jazz.is? declaration jazz.Module-Declaration)
          (for-each (lambda (require)
                      (jazz.parse-require require iter))
                    (%%get-module-declaration-requires declaration))
        (for-each (lambda (require)
                    (jazz.parse-require require iter))
                  (%%get-library-declaration-requires declaration)))))))
