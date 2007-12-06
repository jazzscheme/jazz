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
      (jazz.compile-source-path src options: options cc-options: cc-options ld-options: ld-options force?: force?))))


(define (jazz.compile-source-path src #!key (options #f) (cc-options #f) (ld-options #f) (force? #f))
  (let ((options (or options jazz.compile-options))
        (cc-options (or cc-options ""))
        (ld-options (or ld-options ""))
        (bin (jazz.path-find-binary src))
        (bindir (jazz.path-bin-dir src)))
    (let ((srctime (jazz.path-modification-time src))
          (bintime (and bin (jazz.path-modification-time bin))))
      (if (or force? (not bintime) (> srctime bintime))
          (let ((name (jazz.path-name src))
                (filename (jazz.path-filename src)))
            (jazz.compile-verbose name)
            (jazz.create-directories bindir)
            (jazz.with-extension-reader (jazz.path-extension src)
              (lambda ()
                (parameterize ((jazz.walk-for 'compile))
                  (compile-file filename output: bindir options: options cc-options: cc-options ld-options: ld-options))))
            (let ((base (jazz.filename-name name))
                  (digest (digest-file filename 'sha-1)))
              (call-with-output-file (%%string-append bindir base ".jpck")
                (lambda (output)
                  (jazz.format output "(package {a}{%}{%}" name)
                  (jazz.format output "  (digest {s})){%}" digest)))))))))


(define (jazz.compile-verbose name)
  (display "; compiling ")
  (display name)
  (display " ...")
  (newline))


;;;
;;;; Build
;;;


(define (jazz.build-module module-name)
  (jazz.for-each-submodule module-name
    (lambda (module-name declaration load phase)
      (%%when (not (eq? load 'interpreted))
        (jazz.compile-module module-name)))))


;;;
;;;; Module
;;;


(define (jazz.for-each-submodule module-name proc)
  (let iter ((module-name module-name)
             (feature-requirement #f)
             (load #f)
             (phase #f))
    (let ((declaration (jazz.locate-toplevel-declaration module-name)))
      (proc module-name declaration load phase)
      (if (jazz.is? declaration jazz.Module-Declaration)
          (for-each (lambda (require)
                      (jazz.parse-require require iter))
                    (%%get-module-declaration-requires declaration))
        (for-each (lambda (require)
                    (jazz.parse-require require iter))
                  (%%get-library-declaration-requires declaration)))))))
