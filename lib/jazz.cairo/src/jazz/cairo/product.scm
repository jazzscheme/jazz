;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Cairo Product
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


(unit jazz.cairo.product


;;;
;;;; Build
;;;


(define (jazz:parse-dot-version version)
  (let ((version (map string->number (jazz:split-string version #\.))))
    (let ((major (%%car version))
          (minor (%%cadr version))
          (build (caddr version)))
      (values major minor build))))


(cond-expand
  (mac
    (define jazz:custom-cc
      "/usr/bin/gcc")
    
    (define jazz:custom-cc-options
      '("-O1" "-Wno-unused" "-Wno-write-strings" "-fno-math-errno" "-fno-strict-aliasing" "-fwrapv" "-fomit-frame-pointer" "-fPIC" "-fno-common")))
  (else))


(define (jazz:guess-cairo-name)
  (if (jazz:pkg-config-exists? "cairo-ft")
      "cairo-ft"
    "cairo"))


(cond-expand
  (cocoa
    (define jazz:cairo-units
      (let ((cairo-include-path      (jazz:quote-jazz-pathname "lib/jazz.cairo/foreign/mac/cairo/include/cairo"))
            (pixman-include-path     (jazz:quote-jazz-pathname "lib/jazz.cairo/foreign/mac/pixman/include"))
            (fontconfig-include-path (jazz:quote-jazz-pathname "lib/jazz.cairo/foreign/mac/fontconfig/include"))
            (freetype-include-path   (jazz:quote-jazz-pathname "lib/jazz.cairo/foreign/mac/freetype/include"))
            (png-include-path        (jazz:quote-jazz-pathname "lib/jazz.cairo/foreign/mac/png/include"))
            (cairo-lib-path          (jazz:quote-jazz-pathname "lib/jazz.cairo/foreign/mac/cairo/lib"))
            (pixman-lib-path         (jazz:quote-jazz-pathname "lib/jazz.cairo/foreign/mac/pixman/lib"))
            (fontconfig-lib-path     (jazz:quote-jazz-pathname "lib/jazz.cairo/foreign/mac/fontconfig/lib"))
            (freetype-lib-path       (jazz:quote-jazz-pathname "lib/jazz.cairo/foreign/mac/freetype/lib"))
            (png-lib-path            (jazz:quote-jazz-pathname "lib/jazz.cairo/foreign/mac/png/lib")))
        (let ((cc-flags (string-append "-I" cairo-include-path " -I" pixman-include-path " -I" fontconfig-include-path " -I" freetype-include-path " -I" png-include-path))
              (ld-flags (string-append "-L" cairo-lib-path " -L" pixman-lib-path " -L" fontconfig-lib-path " -L" freetype-lib-path " -L" png-lib-path " -lcairo.2")))
          `((jazz.cairo                cc-options: ,cc-flags ld-options: ,ld-flags)
            (jazz.cairo.cairo-base     cc-options: ,cc-flags ld-options: ,ld-flags)
            (jazz.cairo.cairo-quartz   cc-options: ,cc-flags ld-options: ,ld-flags custom-cc: ,jazz:custom-cc custom-cc-options: ,jazz:custom-cc-options)
            (jazz.cairo.cairo-freetype cc-options: ,cc-flags ld-options: ,ld-flags))))))
  (windows
    (define jazz:cairo-units
      (let ((cairo-include-path      (jazz:quote-jazz-pathname "lib/jazz.cairo/foreign/windows/cairo/include"))
            (pixman-include-path     (jazz:quote-jazz-pathname "lib/jazz.cairo/foreign/windows/pixman/include"))
            (fontconfig-include-path (jazz:quote-jazz-pathname "lib/jazz.cairo/foreign/windows/fontconfig/include"))
            (freetype-include-path   (jazz:quote-jazz-pathname "lib/jazz.cairo/foreign/windows/freetype/include"))
            (expat-include-path      (jazz:quote-jazz-pathname "lib/jazz.cairo/foreign/windows/expat/include"))
            (png-include-path        (jazz:quote-jazz-pathname "lib/jazz.cairo/foreign/windows/png/include"))
            (zlib-include-path       (jazz:quote-jazz-pathname "lib/jazz.cairo/foreign/windows/zlib/include"))
            (cairo-lib-path          (jazz:quote-jazz-pathname "lib/jazz.cairo/foreign/windows/cairo/lib"))
            (pixman-lib-path         (jazz:quote-jazz-pathname "lib/jazz.cairo/foreign/windows/pixman/lib"))
            (fontconfig-lib-path     (jazz:quote-jazz-pathname "lib/jazz.cairo/foreign/windows/fontconfig/lib"))
            (freetype-lib-path       (jazz:quote-jazz-pathname "lib/jazz.cairo/foreign/windows/freetype/lib"))
            (expat-lib-path          (jazz:quote-jazz-pathname "lib/jazz.cairo/foreign/windows/expat/lib"))
            (png-lib-path            (jazz:quote-jazz-pathname "lib/jazz.cairo/foreign/windows/png/lib"))
            (zlib-lib-path           (jazz:quote-jazz-pathname "lib/jazz.cairo/foreign/windows/zlib/lib")))
        (let ((cc-flags (string-append "-I" cairo-include-path " -I" pixman-include-path " -I" fontconfig-include-path " -I" freetype-include-path " -I" expat-include-path " -I" png-include-path " -I" zlib-include-path))
              (ld-flags (string-append "-L" cairo-lib-path " -L" pixman-lib-path " -L" fontconfig-lib-path " -L" freetype-lib-path " -L" expat-lib-path " -L" png-lib-path " -L" zlib-lib-path " -mwindows -lcairo -lfreetype")))
          `((jazz.cairo                cc-options: ,cc-flags ld-options: ,ld-flags)
            (jazz.cairo.cairo-base     cc-options: ,cc-flags ld-options: ,ld-flags)
            (jazz.cairo.cairo-freetype cc-options: ,cc-flags ld-options: ,ld-flags)
            (jazz.cairo.cairo-logfont  cc-options: ,cc-flags ld-options: ,ld-flags)
            (jazz.cairo.cairo-windows  cc-options: ,cc-flags ld-options: ,(string-append ld-flags " -lMsimg32")))))))
  (x11
    (define jazz:cairo-units
      (let ((cairo-name (jazz:guess-cairo-name)))
        (receive (major minor build) (jazz:parse-dot-version (jazz:pkg-config-version cairo-name))
          (if (%%fx< minor 4)
              (jazz:error "Cairo 1.4 or higher needed")
            (let ((cc-flags (jazz:pkg-config-cflags cairo-name))
                  (ld-flags (jazz:pkg-config-libs cairo-name)))
              `((jazz.cairo                cc-options: ,cc-flags ld-options: ,ld-flags)
                (jazz.cairo.cairo-base     cc-options: ,cc-flags ld-options: ,ld-flags)
                (jazz.cairo.cairo-x11      cc-options: ,cc-flags ld-options: ,ld-flags)
                (jazz.cairo.cairo-freetype cc-options: ,cc-flags ld-options: ,ld-flags)))))))))


(cond-expand
  (cocoa
   (define jazz:platform-files
     (list (cons "lib/jazz.cairo/foreign/mac/cairo/lib/libcairo.2.dylib" "libcairo.2.dylib")
           (cons "lib/jazz.cairo/foreign/mac/pixman/lib/libpixman-1.0.dylib" "libpixman-1.0.dylib")
           (cons "lib/jazz.cairo/foreign/mac/png/lib/libpng16.16.dylib" "libpng16.16.dylib"))))
  (windows
   (define jazz:platform-files
     (list (cons "lib/jazz.cairo/foreign/windows/cairo/lib/libcairo-2.dll" "libcairo-2.dll")
           (cons "lib/jazz.cairo/foreign/windows/pixman/lib/libpixman-1-0.dll" "libpixman-1-0.dll")
           (cons "lib/jazz.cairo/foreign/windows/expat/lib/libexpat-1.dll" "libexpat-1.dll")
           (cons "lib/jazz.cairo/foreign/windows/png/lib/libpng16-16.dll" "libpng16-16.dll"))))
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


(define (jazz:build-cairo descriptor #!key (unit #f) (force? #f))
  (let ((unit-specs jazz:cairo-units))
    (jazz:custom-compile/build unit-specs unit: unit pre-build: jazz:copy-platform-files force?: force?)
    (if (or (not unit) (not (assq unit unit-specs)))
        (jazz:build-product-descriptor descriptor))))


;;;
;;;; Register
;;;


(jazz:register-product 'jazz.cairo
  build: jazz:build-cairo))
