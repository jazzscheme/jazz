;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Git Product
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


(unit jazz.git.product


;;;
;;;; Build
;;;


(cond-expand
  (cocoa
   (define jazz:git-units
     (let ((git-include-path  (jazz:quote-jazz-pathname "lib/jazz.git/foreign/mac/libgit2/include"))
           (git-lib-path      (jazz:quote-jazz-pathname "lib/jazz.git/foreign/mac/libgit2/lib")))
       `((jazz.git.foreign
           cc-options: ,(jazz:patch-mac-ld-warnings (string-append "-I" git-include-path))
           ld-options: ,(string-append "-L" git-lib-path " -lgit2.1.1.0"))))))
  (windows
   (define jazz:git-units
     (let ((git-include-path  (jazz:quote-jazz-pathname "lib/jazz.git/foreign/windows/libgit2/include"))
           (git-lib-path      (jazz:quote-jazz-pathname "lib/jazz.git/foreign/windows/libgit2/lib"))
           (zlib-include-path (jazz:quote-jazz-pathname "lib/jazz.git/foreign/windows/zlib/include")))
       `((jazz.git.foreign
           cc-options: ,(string-append "-I" git-include-path " -I" zlib-include-path)
           ld-options: ,(string-append "-L" git-lib-path " -lgit2 -lws2_32"))))))
  (else
   (define jazz:git-units
     (let ((git-include-path  (jazz:quote-jazz-pathname "lib/jazz.git/foreign/linux/libgit2/include"))
           (git-lib-path      (jazz:quote-jazz-pathname "lib/jazz.git/foreign/linux/libgit2/lib")))
       `((jazz.git.foreign
           cc-options: ,(string-append "-I" git-include-path)
           ld-options: ,(string-append "-Wl,-rpath,$ORIGIN/../../../../.." " -L" git-lib-path " -lgit2")))))))


(cond-expand
  (cocoa
   (define jazz:platform-files
     (list (cons "lib/jazz.git/foreign/mac/libgit2/lib/libgit2.1.1.0.dylib" "Libraries/libgit2.1.1.0.dylib"))))
  (windows
   (define jazz:platform-files
     (list (cons "lib/jazz.git/foreign/windows/libgit2/lib/git2.dll" "git2.dll"))))
  (else
   (define jazz:platform-files
     (list (cons "lib/jazz.git/foreign/linux/libgit2/lib/libgit2.so.1.3" "libgit2.so.1.3")))))


(define (jazz:copy-platform-files)
  (let ((source jazz:kernel-source)
        (build (%%get-repository-directory jazz:Build-Repository)))
    (define (source-file path)
      (string-append source path))
    
    (define (build-file path)
      (string-append build path))
    
    (for-each (lambda (info)
                (let ((source (car info))
                      (build (cdr info)))
                  (jazz:copy-file-if-needed (source-file source) (build-file build) feedback: jazz:feedback)))
              jazz:platform-files)))


(define (jazz:build-git descriptor #!key (unit #f) (skip-references? #f) (force? #f))
  (let ((unit-specs jazz:git-units))
    (jazz:custom-compile/build unit-specs unit: unit pre-build: jazz:copy-platform-files force?: force?)
    (if (or (not unit) (not (assq unit unit-specs)))
        (jazz:build-product-descriptor descriptor unit: unit force?: force?))))


(define (jazz:git-library-options descriptor add-language)
  (cond-expand
    (cocoa
      (let ((git-lib-path (jazz:jazz-pathname "lib/jazz.git/foreign/mac/libgit2/lib")))
        (list (string-append "-L" git-lib-path) "-lgit2.1.1.0" "-lgambit")))
    (windows
      (let ((git-lib-path (jazz:jazz-pathname "lib/jazz.git/foreign/windows/libgit2/lib")))
        (list (string-append "-L" git-lib-path) "-lgit2")))
    (else
     '())))


;;;
;;;; Register
;;;


(jazz:register-product 'jazz.git
  build: jazz:build-git
  library-options: jazz:git-library-options))
