;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Freetype Product
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


(unit jazz.freetype.product


;;;
;;;; Build
;;;


(cond-expand
  (cocoa
    (define jazz:freetype-flags
      (let ((freetype-include-path (jazz:quote-jazz-pathname "lib/jazz.freetype/foreign/mac/freetype/include"))
            (freetype-lib-path     (jazz:quote-jazz-pathname "lib/jazz.freetype/foreign/mac/freetype/lib"))
            (png-lib-path          (jazz:quote-jazz-pathname "lib/jazz.cairo/foreign/mac/png/lib")))
        (let ((cc-flags (string-append "-I" freetype-include-path))
              (ld-flags (string-append "-L" freetype-lib-path " -L" png-lib-path " -lfreetype.6")))
          (list cc-flags ld-flags)))))
  (windows
    (define jazz:freetype-flags
      (let ((freetype-include-path (jazz:quote-jazz-pathname "lib/jazz.stream/foreign/windows/gstreamer/include/freetype2"))
            (gstreamer-lib-path    (jazz:quote-jazz-pathname "lib/jazz.stream/foreign/windows/gstreamer/lib")))
        (let ((cc-flags (string-append "-I" freetype-include-path))
              (ld-flags (string-append "-L" gstreamer-lib-path " -lfreetype")))
          (list cc-flags ld-flags)))))
  (else
    (define jazz:freetype-flags
      (let ((freetype-include-path (jazz:quote-jazz-pathname "lib/jazz.freetype/foreign/linux/freetype/include"))
            (freetype-lib-path     (jazz:quote-jazz-pathname "lib/jazz.freetype/foreign/linux/freetype/lib"))
            (png-lib-path          (jazz:quote-jazz-pathname "lib/jazz.cairo/foreign/linux/png/lib")))
        (let ((cc-flags (string-append "-I" freetype-include-path))
              (ld-flags (string-append "-Wl,-rpath,$ORIGIN/../../../../.." " -L" freetype-lib-path " -L" png-lib-path " -lfreetype")))
          (list cc-flags ld-flags))))))


(define jazz:freetype-units
  (jazz:bind (cc-flags ld-flags) jazz:freetype-flags
    `((jazz.freetype cc-options: ,cc-flags ld-options: ,ld-flags))))


(cond-expand
  (cocoa
   (define jazz:platform-files
     (list (cons "lib/jazz.freetype/foreign/mac/freetype/lib/libfreetype.6.dylib" "Libraries/libfreetype.6.dylib"))))
  (windows
   (define jazz:platform-files
     '()))
  (else
   (define jazz:platform-files
     (list (cons "lib/jazz.freetype/foreign/linux/freetype/lib/libfreetype.so.6" "libfreetype.so.6")))))


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


(define (jazz:build-freetype descriptor #!key (unit #f) (skip-references? #f) (force? #f))
  (let ((unit-specs jazz:freetype-units))
    (jazz:custom-compile/build unit-specs unit: unit pre-build: jazz:copy-platform-files force?: force?)
    (if (or (not unit) (not (assq unit unit-specs)))
        (jazz:build-product-descriptor descriptor))))


(define (jazz:freetype-library-options descriptor add-language)
  (cond-expand
    (cocoa
      (let ((freetype-lib-path (jazz:jazz-pathname "lib/jazz.freetype/foreign/mac/freetype/lib"))
            (png-lib-path (jazz:jazz-pathname "lib/jazz.cairo/foreign/mac/png/lib")))
        (string-append "-L" freetype-lib-path " -L" png-lib-path " -lfreetype.6")))
    (windows
      (let ((freetype-lib-path (jazz:jazz-pathname "lib/jazz.freetype/foreign/windows/freetype/lib")))
        (string-append "-L" freetype-lib-path " -lfreetype")))
    (else
     (jazz:pkg-config-libs "freetype2"))))


;;;
;;;; Register
;;;


(jazz:register-product 'jazz.freetype
  build: jazz:build-freetype
  library-options: jazz:freetype-library-options))
