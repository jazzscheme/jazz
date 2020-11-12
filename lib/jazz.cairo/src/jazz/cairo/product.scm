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
      'llvm))
  (else))


(define (jazz:guess-cairo-name)
  (if (jazz:pkg-config-exists? "cairo-ft")
      "cairo-ft"
    "cairo"))


(cond-expand
  (ios
    (define (jazz:cairo-flags quoter)
      (let (;; use those include as they where tailored for ios
            (cairo-include-path      (quoter "lib/jazz.cairo/foreign/ios/cairo/include/cairo"))
            (pixman-include-path     (quoter "lib/jazz.cairo/foreign/mac/pixman/include"))
            (png-include-path        (quoter "lib/jazz.cairo/foreign/mac/png/include"))
            (cairo-lib-path          (quoter "lib/jazz.cairo/foreign/ios/cairo/lib"))
            (pixman-lib-path         (quoter "lib/jazz.cairo/foreign/ios/pixman/lib"))
            (png-lib-path            (quoter "lib/jazz.cairo/foreign/ios/png/lib"))
            (zlib-lib-path           (quoter (cond-expand
                                               (x86 "lib/jazz.zlib/foreign/ios/lib/x86")
                                               (arm "lib/jazz.zlib/foreign/ios/lib/arm")))))
        (let ((cc-flags (string-append "-I" cairo-include-path " -I" pixman-include-path " -I" png-include-path))
              (ld-flags (string-append "-L" cairo-lib-path " -L" pixman-lib-path " -L" png-lib-path " -L" zlib-lib-path " -framework CoreFoundation -framework CoreGraphics -framework CoreText -lcairo -lpixman-1 -lpng16 -lz")))
          `((jazz.cairo cc-options: ,cc-flags ld-options: ,ld-flags output-language: objc))))))
  (cocoa
    (define (jazz:cairo-flags quoter)
      (let ((cairo-include-path      (quoter "lib/jazz.cairo/foreign/mac/cairo/include/cairo"))
            (pixman-include-path     (quoter "lib/jazz.cairo/foreign/mac/pixman/include"))
            (fontconfig-include-path (quoter "lib/jazz.fontconfig/foreign/mac/fontconfig/include"))
            (freetype-include-path   (quoter "lib/jazz.freetype/foreign/mac/freetype/include"))
            (png-include-path        (quoter "lib/jazz.cairo/foreign/mac/png/include"))
            (cairo-lib-path          (quoter "lib/jazz.cairo/foreign/mac/cairo/lib"))
            (pixman-lib-path         (quoter "lib/jazz.cairo/foreign/mac/pixman/lib"))
            (fontconfig-lib-path     (quoter "lib/jazz.fontconfig/foreign/mac/fontconfig/lib"))
            (freetype-lib-path       (quoter "lib/jazz.freetype/foreign/mac/freetype/lib"))
            (png-lib-path            (quoter "lib/jazz.cairo/foreign/mac/png/lib")))
        (let ((cc-flags (string-append "-I" cairo-include-path " -I" pixman-include-path " -I" fontconfig-include-path " -I" freetype-include-path " -I" png-include-path))
              (ld-flags (string-append "-L" cairo-lib-path " -L" pixman-lib-path " -L" fontconfig-lib-path " -L" freetype-lib-path " -L" png-lib-path " -lfreetype.6" " -lcairo.2")))
          (list (jazz:patch-mac-ld-warnings cc-flags) ld-flags)))))
  (windows
    (define (jazz:cairo-flags quoter)
      (let ((cairo-include-path      (quoter "lib/jazz.cairo/foreign/windows/cairo/include"))
            (pixman-include-path     (quoter "lib/jazz.cairo/foreign/windows/pixman/include"))
            (fontconfig-include-path (quoter "lib/jazz.fontconfig/foreign/windows/fontconfig/include"))
            (freetype-include-path   (quoter "lib/jazz.freetype/foreign/windows/freetype/include"))
            (expat-include-path      (quoter "lib/jazz.cairo/foreign/windows/expat/include"))
            (png-include-path        (quoter "lib/jazz.cairo/foreign/windows/png/include"))
            (zlib-include-path       (quoter "lib/jazz.zlib/foreign/windows/zlib/include"))
            (cairo-lib-path          (quoter "lib/jazz.cairo/foreign/windows/cairo/lib"))
            (pixman-lib-path         (quoter "lib/jazz.cairo/foreign/windows/pixman/lib"))
            (fontconfig-lib-path     (quoter "lib/jazz.fontconfig/foreign/windows/fontconfig/lib"))
            (freetype-lib-path       (quoter "lib/jazz.freetype/foreign/windows/freetype/lib"))
            (expat-lib-path          (quoter "lib/jazz.cairo/foreign/windows/expat/lib"))
            (png-lib-path            (quoter "lib/jazz.cairo/foreign/windows/png/lib"))
            (zlib-lib-path           (quoter "lib/jazz.zlib/foreign/windows/zlib/lib")))
        (let ((cc-flags (string-append "-I" cairo-include-path " -I" pixman-include-path " -I" fontconfig-include-path " -I" freetype-include-path " -I" expat-include-path " -I" png-include-path " -I" zlib-include-path))
              (ld-flags (string-append "-L" cairo-lib-path " -L" pixman-lib-path " -L" fontconfig-lib-path " -L" freetype-lib-path " -L" expat-lib-path " -L" png-lib-path " -L" zlib-lib-path " -mwindows -lcairo -lfreetype")))
          (list cc-flags ld-flags)))))
  (x11
    (define (jazz:cairo-flags quoter)
      (let ((cairo-include-path      (quoter "lib/jazz.cairo/foreign/linux/cairo/include/cairo"))
            (pixman-include-path     (quoter "lib/jazz.cairo/foreign/linux/pixman/include"))
            (fontconfig-include-path (quoter "lib/jazz.fontconfig/foreign/linux/fontconfig/include"))
            (freetype-include-path   (quoter "lib/jazz.freetype/foreign/linux/freetype/include"))
            (png-include-path        (quoter "lib/jazz.cairo/foreign/linux/png/include"))
            (cairo-lib-path          (quoter "lib/jazz.cairo/foreign/linux/cairo/lib"))
            (pixman-lib-path         (quoter "lib/jazz.cairo/foreign/linux/pixman/lib"))
            (fontconfig-lib-path     (quoter "lib/jazz.fontconfig/foreign/linux/fontconfig/lib"))
            (freetype-lib-path       (quoter "lib/jazz.freetype/foreign/linux/freetype/lib"))
            (png-lib-path            (quoter "lib/jazz.cairo/foreign/linux/png/lib")))
        (let ((cc-flags (string-append "-I" cairo-include-path " -I" pixman-include-path " -I" fontconfig-include-path " -I" freetype-include-path " -I" png-include-path))
              (ld-flags (string-append "-Wl,-rpath,$ORIGIN/../../../../.." " -L" cairo-lib-path " -L" pixman-lib-path " -L" fontconfig-lib-path " -L" freetype-lib-path " -L" png-lib-path " -lfreetype" " -lcairo")))
          (list cc-flags ld-flags))))))


(cond-expand
  (ios
    (define jazz:cairo-units
      (jazz:bind (cc-flags ld-flags) (jazz:cairo-flags jazz:quote-jazz-pathname)
        `((jazz.cairo cc-options: ,cc-flags ld-options: ,ld-flags output-language: objc)))))
  (cocoa
    (define jazz:cairo-units
      (jazz:bind (cc-flags ld-flags) (jazz:cairo-flags jazz:quote-jazz-pathname)
        `((jazz.cairo cc-options: ,cc-flags ld-options: ,ld-flags custom-cc: ,jazz:custom-cc)))))
  (windows
    (define jazz:cairo-units
      (jazz:bind (cc-flags ld-flags) (jazz:cairo-flags jazz:quote-jazz-pathname)
        `((jazz.cairo cc-options: ,cc-flags ld-options: ,(string-append ld-flags " -lMsimg32"))))))
  (x11
    (define jazz:cairo-units
      (jazz:bind (cc-flags ld-flags) (jazz:cairo-flags jazz:quote-jazz-pathname)
        `((jazz.cairo cc-options: ,cc-flags ld-options: ,ld-flags))))))


(cond-expand
  (ios
    (define jazz:platform-files
     '()))
  (cocoa
   (define jazz:platform-files
     (list (cons "lib/jazz.cairo/foreign/mac/cairo/lib/libcairo.2.dylib" "Libraries/libcairo.2.dylib")
           (cons "lib/jazz.cairo/foreign/mac/pixman/lib/libpixman-1.0.dylib" "Libraries/libpixman-1.0.dylib")
           (cons "lib/jazz.cairo/foreign/mac/png/lib/libpng16.16.dylib" "Libraries/libpng16.16.dylib"))))
  (windows
   (define jazz:platform-files
     (list (cons "lib/jazz.cairo/foreign/windows/cairo/lib/libcairo-2.dll" "libcairo-2.dll")
           (cons "lib/jazz.cairo/foreign/windows/pixman/lib/libpixman-1-0.dll" "libpixman-1-0.dll")
           (cons "lib/jazz.cairo/foreign/windows/expat/lib/libexpat-1.dll" "libexpat-1.dll")
           (cons "lib/jazz.cairo/foreign/windows/png/lib/libpng16-16.dll" "libpng16-16.dll"))))
  (else
   (define jazz:platform-files
     (list (cons "lib/jazz.cairo/foreign/linux/cairo/lib/libcairo.so.2" "libcairo.so.2")
           (cons "lib/jazz.cairo/foreign/linux/pixman/lib/libpixman-1.so.0" "libpixman-1.so.0")
           (cons "lib/jazz.cairo/foreign/linux/png/lib/libpng16.so.16" "libpng16.so.16")
           (cons "lib/jazz.cairo/foreign/linux/bz2/lib/libbz2.so.1.0" "libbz2.so.1.0")))))


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


(define (jazz:build-cairo descriptor #!key (unit #f) (skip-references? #f) (force? #f))
  (let ((unit-specs jazz:cairo-units))
    (jazz:custom-compile/build unit-specs unit: unit pre-build: jazz:copy-platform-files force?: force?)
    (if (or (not unit) (not (assq unit unit-specs)))
        (jazz:build-product-descriptor descriptor))))


(define (jazz:cairo-library-options descriptor add-language)
  (cond-expand
    (ios
      (add-language 'jazz.platform.cocoa.foreign 'objc))
    (else))
  (jazz:bind (cc-flags ld-flags) (jazz:cairo-flags jazz:jazz-pathname)
    ld-flags))


;;;
;;;; Register
;;;


(jazz:register-product 'jazz.cairo
  build: jazz:build-cairo
  library-options: jazz:cairo-library-options))
