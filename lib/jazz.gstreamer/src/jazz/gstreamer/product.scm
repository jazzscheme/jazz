;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; GStreamer Product
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


(unit jazz.gstreamer.product


;;;
;;;; Build
;;;


(cond-expand
  (cocoa
    (define jazz:gstreamer-flags
      (let ((gstreamer-include-path (jazz:quote-jazz-pathname "lib/jazz.gstreamer/foreign/mac/gstreamer/include"))
            (gstreamer-lib-path     (jazz:quote-jazz-pathname "lib/jazz.gstreamer/foreign/mac/gstreamer/lib")))
        (let ((cc-flags (string-append "-I" gstreamer-include-path " -fpermissive"))
              (ld-flags (string-append "-L" gstreamer-lib-path " -lgstreamer.1.1.0")))
          (list cc-flags ld-flags)))))
  (windows
    (define jazz:gstreamer-flags
      (let ((gstreamer-include-path (jazz:quote-jazz-pathname "lib/jazz.gstreamer/foreign/windows/gstreamer/include"))
            (gstreamer-lib-path     (jazz:quote-jazz-pathname "lib/jazz.gstreamer/foreign/windows/gstreamer/lib")))
        (let ((cc-flags (string-append "-I" gstreamer-include-path " -fpermissive"))
              (ld-flags (string-append "-L" gstreamer-lib-path " -lgstreamer")))
          (list cc-flags ld-flags)))))
  (else
    (define jazz:gstreamer-flags
      (let ((cc-flags (string-append (jazz:pkg-config-cflags "gstreamer") " -fpermissive"))
            (ld-flags (jazz:pkg-config-libs "gstreamer")))
        (list cc-flags ld-flags)))))


(define jazz:gstreamer-units
  (jazz:bind (cc-flags ld-flags) jazz:gstreamer-flags
    `((jazz.gstreamer.foreign cc-options: ,cc-flags ld-options: ,ld-flags))))


(cond-expand
  (cocoa
   (define jazz:platform-files
     (list (cons "lib/jazz.gstreamer/foreign/mac/gstreamer/lib/libgstreamer.1.1.0.dylib" "libgstreamer.1.1.0.dylib"))))
  (windows
   (define jazz:platform-files
     (list (cons "lib/jazz.gstreamer/foreign/windows/gstreamer/lib/libgstreamer-1.dll" "libgstreamer-1.dll"))))
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


(define (jazz:build-gstreamer descriptor #!key (unit #f) (force? #f))
  (let ((unit-specs jazz:gstreamer-units))
    (jazz:custom-compile/build unit-specs unit: unit pre-build: jazz:copy-platform-files force?: force?)
    (if (or (not unit) (not (assq unit unit-specs)))
        (jazz:build-product-descriptor descriptor))))


(define (jazz:gstreamer-library-options descriptor add-language)
  (cond-expand
    (cocoa
      (let ((gstreamer-lib-path (jazz:jazz-pathname "lib/jazz.gstreamer/foreign/mac/gstreamer/lib")))
        (string-append "-L" gstreamer-lib-path " -lgstreamer.1.1.0")))
    (windows
      (let ((gstreamer-lib-path (jazz:jazz-pathname "lib/jazz.gstreamer/foreign/windows/gstreamer/lib")))
        (string-append "-L" gstreamer-lib-path " -lgstreamer")))
    (else
     (jazz:pkg-config-libs "gstreamer"))))


;;;
;;;; Register
;;;


(jazz:register-product 'jazz.gstreamer
  build: jazz:build-gstreamer
  library-options: jazz:gstreamer-library-options))
