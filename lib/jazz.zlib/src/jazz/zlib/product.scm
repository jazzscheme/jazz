;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Zlib Product
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
;;;  Portions created by the Initial Developer are Copyright (C) 1996-2015
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


(unit jazz.zlib.product


;;;
;;;; Build
;;;


(cond-expand
  (cocoa
   (define jazz:zlib-units
     '((jazz.zlib.foreign cc-options: "-fpermissive" ld-options: "-lz.1"))))
  (windows
   (define jazz:zlib-units
     (let ((zlib-include-path  (jazz:quote-jazz-pathname "lib/jazz.zlib/foreign/windows/zlib/include"))
           (zlib-lib-path      (jazz:quote-jazz-pathname "lib/jazz.zlib/foreign/windows/zlib/lib")))
       `((jazz.zlib.foreign
           cc-options: ,(string-append "-I" zlib-include-path " -fpermissive")
           ld-options: ,(string-append "-L" zlib-lib-path " -lz"))))))
  (else
   (define jazz:zlib-units
     '((jazz.zlib.foreign cc-options: "-fpermissive")))))


(cond-expand
  (cocoa
   (define jazz:platform-files
     '()))
  (windows
   (define jazz:platform-files
     (list (cons "lib/jazz.zlib/foreign/windows/zlib/lib/zlib1.dll" "zlib1.dll"))))
  (else
   (define jazz:platform-files
     '())))


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
                  (jazz:copy-file (source-file source) (build-file build) feedback: jazz:feedback)))
              jazz:platform-files)))


(define (jazz:build-zlib descriptor #!key (unit #f) (force? #f))
  (let ((unit-specs jazz:zlib-units))
    (jazz:custom-compile/build unit-specs unit: unit pre-build: jazz:copy-platform-files force?: force?)
    (if (or (not unit) (not (assq unit unit-specs)))
        (jazz:build-product-descriptor descriptor unit: unit force?: force?))))


(define (jazz:build-zlib-library descriptor)
  (let ((ld-options
          (cond-expand
            (cocoa
              (list "-lz.1"))
            (windows
              (let ((zlib-lib-path (jazz:jazz-pathname "lib/jazz.zlib/foreign/windows/zlib/lib")))
                (list (string-append "-L" zlib-lib-path) "-lz")))
            (else
             '()))))
    (jazz:build-library (jazz:product-descriptor-name descriptor) descriptor ld-options: ld-options)))


;;;
;;;; Register
;;;


(jazz:register-product 'jazz.zlib
  build: jazz:build-zlib
  build-library: jazz:build-zlib-library))
