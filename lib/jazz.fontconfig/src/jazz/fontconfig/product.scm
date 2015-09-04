;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Fontconfig Product
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


(unit jazz.fontconfig.product


;;;
;;;; Build
;;;


(cond-expand
  (cocoa
    (define jazz:fontconfig-units
      (let ((fontconfig-include-path (jazz:quote-jazz-pathname "lib/jazz.fontconfig/foreign/mac/fontconfig/include"))
            (fontconfig-lib-path     (jazz:quote-jazz-pathname "lib/jazz.fontconfig/foreign/mac/fontconfig/lib"))
            (freetype-include-path   (jazz:quote-jazz-pathname "lib/jazz.fontconfig/foreign/mac/freetype/include"))
            (freetype-lib-path       (jazz:quote-jazz-pathname "lib/jazz.fontconfig/foreign/mac/freetype/lib"))
            (png-include-path        (jazz:quote-jazz-pathname "lib/jazz.fontconfig/foreign/mac/png/include"))
            (png-lib-path            (jazz:quote-jazz-pathname "lib/jazz.fontconfig/foreign/mac/png/lib")))
        (let ((cc-flags (string-append "-I" fontconfig-include-path " -I" freetype-include-path " -I" png-include-path))
              (ld-flags (string-append "-L" fontconfig-lib-path " -L" freetype-lib-path " -L" png-lib-path " -lfontconfig.1")))
          `((jazz.fontconfig cc-options: ,cc-flags ld-options: ,ld-flags))))))
  (windows
    (define jazz:fontconfig-units
      (let ((fontconfig-include-path (jazz:quote-jazz-pathname "lib/jazz.fontconfig/foreign/windows/fontconfig/include"))
            (fontconfig-lib-path     (jazz:quote-jazz-pathname "lib/jazz.fontconfig/foreign/windows/fontconfig/lib"))
            (freetype-include-path   (jazz:quote-jazz-pathname "lib/jazz.fontconfig/foreign/windows/freetype/include"))
            (freetype-lib-path       (jazz:quote-jazz-pathname "lib/jazz.fontconfig/foreign/windows/freetype/lib"))
            (expat-lib-path          (jazz:quote-jazz-pathname "lib/jazz.fontconfig/foreign/windows/expat/lib"))
            (png-lib-path            (jazz:quote-jazz-pathname "lib/jazz.fontconfig/foreign/windows/png/lib"))
            (zlib-lib-path           (jazz:quote-jazz-pathname "lib/jazz.fontconfig/foreign/windows/zlib/lib")))
        (let ((cc-flags (string-append "-I" fontconfig-include-path " -I" freetype-include-path))
              (ld-flags (string-append "-L" fontconfig-lib-path " -L" freetype-lib-path " -L" expat-lib-path " -L" png-lib-path " -L" zlib-lib-path " -lfontconfig")))
          `((jazz.fontconfig cc-options: ,cc-flags ld-options: ,ld-flags))))))
  (else
    (define jazz:fontconfig-units
      (let ((cc-flags (jazz:pkg-config-cflags "fontconfig"))
            (ld-flags (jazz:pkg-config-libs "fontconfig")))
        `((jazz.fontconfig cc-options: ,cc-flags ld-options: ,ld-flags))))))


(cond-expand
  (cocoa
   (define jazz:platform-files
     (list (cons "lib/jazz.fontconfig/foreign/mac/fontconfig/lib/libfontconfig.1.dylib" "libfontconfig.1.dylib"))))
  (windows
   (define jazz:platform-files
     (list (cons "lib/jazz.fontconfig/foreign/windows/fontconfig/lib/libfontconfig-1.dll" "libfontconfig-1.dll"))))
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


(define (jazz:build-fontconfig descriptor #!key (unit #f) (force? #f))
  (let ((unit-specs jazz:fontconfig-units))
    (jazz:custom-compile/build unit-specs unit: unit pre-build: jazz:copy-platform-files force?: force?)
    (if (or (not unit) (not (assq unit unit-specs)))
        (jazz:build-product-descriptor descriptor))))


;;;
;;;; Register
;;;


(jazz:register-product 'jazz.fontconfig
  build: jazz:build-fontconfig))
