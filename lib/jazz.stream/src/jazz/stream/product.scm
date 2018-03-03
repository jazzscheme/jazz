;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Stream Product
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


(unit jazz.stream.product


;;;
;;;; Build
;;;


(cond-expand
  (cocoa
    (define jazz:stream-flags
      (let ((cc-flags (jazz:pkg-config-cflags "gstreamer-1.0 gstreamer-app-1.0 gstreamer-pbutils-1.0"))
            (ld-flags (jazz:pkg-config-libs "gstreamer-1.0 gstreamer-app-1.0 gstreamer-pbutils-1.0")))
        (list cc-flags ld-flags)))
    #; ;; brew-based-private-deployment
    (define jazz:stream-flags
      (let ((gstreamer-include-path  (jazz:quote-jazz-pathname "lib/jazz.stream/foreign/mac/gstreamer/include/gstreamer-1.0"))
            (gstreamer-lib-path      (jazz:quote-jazz-pathname "lib/jazz.stream/foreign/mac/gstreamer/lib"))
            (glib-include-path       (jazz:quote-jazz-pathname "lib/jazz.stream/foreign/mac/glib/include/glib-2.0"))
            (glibconfig-include-path (jazz:quote-jazz-pathname "lib/jazz.stream/foreign/mac/glib/include"))
            (glib-lib-path           (jazz:quote-jazz-pathname "lib/jazz.stream/foreign/mac/glib/lib"))
            (gettext-include-path    (jazz:quote-jazz-pathname "lib/jazz.stream/foreign/mac/gettext/include"))
            (gettext-lib-path        (jazz:quote-jazz-pathname "lib/jazz.stream/foreign/mac/gettext/lib")))
        (let ((cc-flags (string-append "-I" gstreamer-include-path " -I" glib-include-path " -I" glibconfig-include-path " -I" gettext-include-path))
              (ld-flags (string-append "-L" gstreamer-lib-path " -L" glib-lib-path " -L" gettext-lib-path " -lgstreamer-1.0.0 -lgobject-2.0.0 -lglib-2.0.0 -lintl.8 -Wl,-framework -Wl,CoreFoundation")))
          (list cc-flags ld-flags)))))
  (windows
    (define jazz:stream-flags
      (let ((gstreamer-include-path  (jazz:quote-jazz-pathname "lib/jazz.stream/foreign/windows/gstreamer/include/gstreamer-1.0"))
            (gstreamer-lib-path      (jazz:quote-jazz-pathname "lib/jazz.stream/foreign/windows/gstreamer/lib"))
            (glib-include-path       (jazz:quote-jazz-pathname "lib/jazz.stream/foreign/windows/glib/include/glib-2.0"))
            (glibconfig-include-path (jazz:quote-jazz-pathname "lib/jazz.stream/foreign/windows/glib/include"))
            (glib-lib-path           (jazz:quote-jazz-pathname "lib/jazz.stream/foreign/windows/glib/lib"))
            (gettext-include-path    (jazz:quote-jazz-pathname "lib/jazz.stream/foreign/windows/gettext/include"))
            (gettext-lib-path        (jazz:quote-jazz-pathname "lib/jazz.stream/foreign/windows/gettext/lib")))
        (let ((cc-flags (string-append "-I" gstreamer-include-path " -I" glib-include-path " -I" glibconfig-include-path " -I" gettext-include-path))
              (ld-flags (string-append "-L" gstreamer-lib-path " -L" glib-lib-path " -L" gettext-lib-path " -lgstreamer")))
          (list cc-flags ld-flags)))))
  (else
    (define jazz:stream-flags
      (let ((cc-flags (jazz:pkg-config-cflags "gstreamer-1.0"))
            (ld-flags (jazz:pkg-config-libs "gstreamer-1.0")))
        (list cc-flags ld-flags)))))


(define jazz:stream-units
  (jazz:bind (cc-flags ld-flags) jazz:stream-flags
    `((jazz.stream.foreign cc-options: ,cc-flags ld-options: ,ld-flags))))


(cond-expand
  (cocoa
   (define jazz:platform-files
     '())
   #; ;; brew-based-private-deployment
   (define jazz:platform-files
     (list (cons "lib/jazz.stream/foreign/mac/gstreamer/lib/libgstreamer-1.0.0.dylib" "libgstreamer-1.0.0.dylib")
           (cons "lib/jazz.stream/foreign/mac/glib/lib/libglib-2.0.0.dylib" "libglib-2.0.0.dylib")
           (cons "lib/jazz.stream/foreign/mac/glib/lib/libgobject-2.0.0.dylib" "libgobject-2.0.0.dylib")
           (cons "lib/jazz.stream/foreign/mac/glib/lib/libgmodule-2.0.0.dylib" "libgmodule-2.0.0.dylib")
           (cons "lib/jazz.stream/foreign/mac/gettext/lib/libintl.8.dylib" "libintl.8.dylib"))))
  (windows
   (define jazz:platform-files
     (list (cons "lib/jazz.stream/foreign/windows/gstreamer/lib/libgstreamer-1.dll" "libgstreamer-1.dll")
           (cons "lib/jazz.stream/foreign/windows/glib/lib/libglib-2.dll" "libglib-2.dll")
           (cons "lib/jazz.stream/foreign/windows/glib/lib/libgobject-2.dll" "libgobject-2.dll")
           (cons "lib/jazz.stream/foreign/windows/glib/lib/libgmodule-2.dll" "libgmodule-2.dll")
           (cons "lib/jazz.stream/foreign/windows/gettext/lib/libintl-8.dll" "libintl-8.dll"))))
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


(define (jazz:build-stream descriptor #!key (unit #f) (force? #f))
  (let ((unit-specs jazz:stream-units))
    (jazz:custom-compile/build unit-specs unit: unit pre-build: jazz:copy-platform-files force?: force?)
    (if (or (not unit) (not (assq unit unit-specs)))
        (jazz:build-product-descriptor descriptor))))


(define (jazz:stream-library-options descriptor add-language)
  (cond-expand
    (cocoa
     (jazz:pkg-config-libs "gstreamer-1.0")
      #; ;; brew-based-private-deployment
      (let ((gstreamer-lib-path (jazz:jazz-pathname "lib/jazz.stream/foreign/mac/gstreamer/lib"))
            (glib-lib-path (jazz:jazz-pathname "lib/jazz.stream/foreign/mac/glib/lib"))
            (gettext-lib-path (jazz:jazz-pathname "lib/jazz.stream/foreign/mac/gettext/lib")))
        (string-append "-L" gstreamer-lib-path " -L" glib-lib-path " -L" gettext-lib-path " -lgstreamer-1.0.0 -lgobject-2.0.0 -lglib-2.0.0 -lintl.8")))
    (windows
      (let ((gstreamer-lib-path (jazz:jazz-pathname "lib/jazz.stream/foreign/windows/gstreamer/lib"))
            (glib-lib-path (jazz:jazz-pathname "lib/jazz.stream/foreign/windows/glib/lib"))
            (gettext-lib-path (jazz:jazz-pathname "lib/jazz.stream/foreign/windows/gettext/lib")))
        (string-append "-L" gstreamer-lib-path " -L" glib-lib-path " -L" gettext-lib-path " -lgstreamer")))
    (else
     (jazz:pkg-config-libs "gstreamer-1.0"))))


;;;
;;;; Register
;;;


(jazz:register-product 'jazz.stream
  title: "Stream"
  build: jazz:build-stream
  library-options: jazz:stream-library-options))
