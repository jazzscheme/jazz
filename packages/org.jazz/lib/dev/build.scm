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


(module dev.build


(require (core.foundation)
         (core.library))


;;;
;;;; Compile
;;;


(define (jazz.compile-library-with-options library-name options)
  (let ((filename (jazz.determine-module-filename library-name)))
    (let ((src (jazz.require-module-source filename)))
      (jazz.with-extension-reader (jazz.filename-extension src)
        (lambda ()
          (compile-file src options))))))


(define (jazz.compile-library-with-flags library-name #!key (options #f) (cc-flags #f) (ld-flags #f) (force? #f))
  (let ((filename (jazz.determine-module-filename library-name)))
    (jazz.compile-filename-with-flags filename options: options cc-flags: cc-flags ld-flags: ld-flags force?: force?)))


(define (jazz.compile-filename-with-flags filename #!key (options #f) (cc-flags #f) (ld-flags #f) (force? #f) (source? #f))
  (let ((options (or options jazz.compile-options))
        (cc-flags (or cc-flags ""))
        (ld-flags (or ld-flags "")))
    (let ((directory (jazz.split-filename filename (lambda (dir file) dir))))
      (jazz.build-bin-dir directory)
      (let* ((src (if source? filename (jazz.require-module-source filename)))
             (bin (jazz.determine-module-binary filename))
             (bindir (jazz.determine-module-bindir filename))
             (srctime (time->seconds (file-last-modification-time src)))
             (bintime (and bin (time->seconds (file-last-modification-time bin)))))
        (if (or force? (not bintime) (> srctime bintime))
            (begin
              (jazz.compile-verbose filename)
              (jazz.with-extension-reader (jazz.filename-extension src)
                (lambda ()
                  (parameterize ((jazz.walk-for 'compile))
                    (compile-file-to src bindir options cc-flags ld-flags))))))))))


(define (jazz.compile-library-to-c library-name)
  (let* ((filename (jazz.determine-module-filename library-name))
         (source (jazz.require-module-source filename))
         (c-file (string-append filename ".c")))
    (if (file-exists? c-file)
        (delete-file c-file))
    (compile-file-to-c source)))


(define (jazz.compile-library library-name)
  (let ((filename (jazz.require-module-source (jazz.determine-module-filename library-name))))
    (jazz.compile-filename-with-flags filename)))


(define (jazz.compile-verbose filename)
  (display "; compiling ")
  (display filename)
  (display " ...")
  (newline))


;;;
;;;; Build
;;;


(define (jazz.create-directories dirname)
  (let ((path (%%reverse (jazz.split-string dirname #\/))))
    (let iter ((scan (if (%%equal? (%%car path) "") (%%cdr path) path)))
      (if (%%not (%%null? scan))
          (begin
            (iter (%%cdr scan))
            (let ((subdir (jazz.join-strings (%%reverse scan) "/")))
              (if (%%not (file-exists? subdir))
                  (create-directory subdir))))))))


(define (jazz.build-bin-dir directory)
  (jazz.create-directories (%%string-append "_obj/" directory)))


(define (jazz.build-kernel)
  (jazz.build-bin-dir "kernel/module")
  (jazz.compile-filename-with-flags "kernel/module/runtime"))


(define (jazz.build-module module-name)
  (jazz.for-each-submodule module-name
    (lambda (module-name declaration load phase)
      (%%when (not (eq? load 'interpreted))
        (let* ((filename (jazz.determine-module-filename module-name))
               (directory (jazz.split-filename filename (lambda (dir file) dir))))
          (jazz.build-bin-dir directory)
          (jazz.compile-filename-with-flags filename))))))


(define (jazz.for-each-submodule module-name proc)
  (let iter ((module-name module-name)
             (feature-requirement #f)
             (load #f)
             (phase #f))
    (let ((declaration (jazz.locate-toplevel-declaration module-name)))
      (proc module-name declaration load phase)
      (cond ((eq? (%%get-lexical-binding-name declaration) module-name)
             (if (jazz.is? declaration jazz.Module-Declaration)
                 (for-each (lambda (require)
                             (jazz.parse-require require iter))
                           (%%get-module-declaration-requires declaration))
               (for-each (lambda (require)
                           (jazz.parse-require require iter))
                         (%%get-library-declaration-requires declaration))))
            (else
             (jazz.set-catalog-entry module-name #f)
             (error "Inconsistant module name in" module-name))))))


;;;
;;;; Clean
;;;


#; ;; doesn't work
(define (jazz.clean-module module-name)
  (jazz.for-each-submodule module-name
    (lambda (declaration load phase)
      (if (not (eq? load 'interpreted))
          (let* ((filename (jazz.determine-module-filename module-name))
                 (bindir (jazz.determine-module-bindir filename)))
            (let iter ((n 0))
                 (let ((bin (%%string-append bindir filename ".o" (number->string n))))
                   (if (file-exists? bin)
                       (begin
                         (write (list 'deleting bin)) (newline)
                         (delete-file bin)
                         (iter (%%fx+ n 1))))))))))))
