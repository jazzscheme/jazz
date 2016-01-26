;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Platform Product
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


(unit jazz.platform.product


;;;
;;;; Build
;;;


(cond-expand
  (mac
    (define jazz:custom-cc
      (string-append "/usr/bin/" (jazz:compiler-name)))
    
    (define jazz:custom-cc-options
      '("-O1" "-Wno-unused" "-Wno-write-strings" "-fno-math-errno" "-fno-strict-aliasing" "-fwrapv" "-fomit-frame-pointer" "-fPIC" "-fno-common")))
  (else))


(cond-expand
  (mac
    (define jazz:types-units
      `((jazz.platform.types-syntax)
        (jazz.platform.types custom-cc: ,jazz:custom-cc custom-cc-options: ,jazz:custom-cc-options))))
  (else
   (define jazz:types-units
     '((jazz.platform.types-syntax)
       (jazz.platform.types)))))


(define jazz:windows-units
  (let ((pdh-include-path   (jazz:quote-jazz-pathname "lib/jazz.platform/foreign/windows/pdh/include"))
        (pdh-lib-path       (jazz:quote-jazz-pathname "lib/jazz.platform/foreign/windows/pdh/lib"))
        (base-windows-cc-options "-DUNICODE -D_WIN32_WINNT=0x0502 -fpermissive"))
    `((jazz.platform.windows)
      (jazz.platform.windows.WinDef      cc-options: ,base-windows-cc-options ld-options: "-mwindows")
      (jazz.platform.windows.WinTypes    cc-options: ,base-windows-cc-options ld-options: "-mwindows")
      (jazz.platform.windows.WinBase     cc-options: ,base-windows-cc-options ld-options: "-mwindows")
      (jazz.platform.windows.WinNT       cc-options: ,base-windows-cc-options ld-options: "-mwindows")
      (jazz.platform.windows.WinKernel   cc-options: ,base-windows-cc-options ld-options: "-mwindows")
      (jazz.platform.windows.WinGDI      cc-options: ,base-windows-cc-options ld-options: "-mwindows")
      (jazz.platform.windows.WinIDL      cc-options: ,base-windows-cc-options ld-options: "-mwindows")
      (jazz.platform.windows.WinMM       cc-options: ,base-windows-cc-options ld-options: "-mwindows -lwinmm")
      (jazz.platform.windows.WinUser     cc-options: ,base-windows-cc-options ld-options: "-mwindows")
      (jazz.platform.windows.WinShell    cc-options: ,base-windows-cc-options ld-options: "-mwindows")
      (jazz.platform.windows.WinCtrl     cc-options: ,base-windows-cc-options ld-options: "-mwindows")
      (jazz.platform.windows.WinDlg      cc-options: ,base-windows-cc-options ld-options: "-mwindows")
      (jazz.platform.windows.WinPerf     cc-options: ,(string-append "-I" pdh-include-path " " base-windows-cc-options) ld-options: ,(string-append "-L" pdh-lib-path " -mwindows -lpdh"))
      (jazz.platform.windows.WinPSAPI    cc-options: ,(string-append "-I" pdh-include-path " " base-windows-cc-options) ld-options: ,(string-append "-L" pdh-lib-path " -mwindows -lpsapi")))))


(cond-expand
  (x11
   (define jazz:x11-units
     (let ((cc-flags (string-append (jazz:pkg-config-cflags "x11") " -fpermissive"))
           (ld-flags (jazz:pkg-config-libs "x11")))
       `((jazz.platform.x11 cc-options: ,cc-flags ld-options: ,ld-flags)
         (jazz.platform.x11.x11-types)))))
   (else))


(cond-expand
  (ios
   (define jazz:ios-units
     `((jazz.platform.ios.foreign ld-options: "-framework CoreFoundation -framework CoreGraphics" custom-cc: ,jazz:custom-cc custom-cc-options: ,jazz:custom-cc-options output-language: objc))))
  (else))


(cond-expand
  (cocoa
   (define jazz:cocoa-units
     `((jazz.platform.cocoa.foreign ld-options: "-framework Cocoa -framework OpenGL -framework IOKit" custom-cc: ,jazz:custom-cc custom-cc-options: ,jazz:custom-cc-options output-language: objc))))
  (else))


(cond-expand
  (cocoa
   (define jazz:platform-files
     '()))
  (windows
   (define jazz:platform-files
     (list (cons "lib/jazz.platform/foreign/windows/gcc/lib/libgcc_s_dw2-1.dll" "libgcc_s_dw2-1.dll")
           (cons "lib/jazz.platform/foreign/windows/gcc/lib/libstdc++-6.dll" "libstdc++-6.dll"))))
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


(cond-expand
  (ios
    (define (jazz:build-platform descriptor #!key (unit #f) (force? #f))
      (let ((unit-specs `((jazz.platform)
                          ,@jazz:types-units
                          ,@jazz:ios-units)))
        (jazz:custom-compile/build unit-specs unit: unit pre-build: jazz:copy-platform-files force?: force?)
        (if (or (not unit) (not (assq unit unit-specs)))
            (jazz:build-product-descriptor descriptor)))))
  (cocoa
    (define (jazz:build-platform descriptor #!key (unit #f) (force? #f))
      (let ((unit-specs `((jazz.platform)
                          ,@jazz:types-units
                          ,@jazz:cocoa-units)))
        (jazz:custom-compile/build unit-specs unit: unit pre-build: jazz:copy-platform-files force?: force?)
        (if (or (not unit) (not (assq unit unit-specs)))
            (jazz:build-product-descriptor descriptor)))))
  (windows
    (define (jazz:build-platform descriptor #!key (unit #f) (force? #f))
      (let ((unit-specs `((jazz.platform)
                          ,@jazz:types-units
                          ,@jazz:windows-units)))
        (jazz:custom-compile/build unit-specs unit: unit pre-build: jazz:copy-platform-files force?: force?)
        (if (or (not unit) (not (assq unit unit-specs)))
            (jazz:build-product-descriptor descriptor)))))
  (x11
    (define (jazz:build-platform descriptor #!key (unit #f) (force? #f))
      (let ((unit-specs `((jazz.platform)
                          ,@jazz:types-units
                          ,@jazz:x11-units)))
        (jazz:custom-compile/build unit-specs unit: unit pre-build: jazz:copy-platform-files force?: force?)
        (if (or (not unit) (not (assq unit unit-specs)))
            (jazz:build-product-descriptor descriptor unit: unit force?: force?))))))


;;;
;;;; Register
;;;


(jazz:register-product 'jazz.platform
  build: jazz:build-platform))
