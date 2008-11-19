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
;;;  Portions created by the Initial Developer are Copyright (C) 1996-2008
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


(module jazz.product.platform


;;;
;;;; Build
;;;
  

(define (jazz.build-types)
  (jazz.compile-module 'jazz.platform.types))


(cond-expand
  (carbon
    (define (jazz.build-cairo)
      (receive (major minor build) (jazz.parse-dot-version (jazz.pkg-config-version "cairo-ft"))
        (if (< minor 4)
            (jazz.error "Cairo 1.4 or higher needed")
          (let ((cc-flags (jazz.pkg-config-cflags "cairo-ft"))
                (ld-flags (jazz.pkg-config-libs "cairo-ft")))
            (jazz.compile-module 'jazz.platform.cairo                cc-options: cc-flags ld-options: ld-flags)
            (jazz.compile-module 'jazz.platform.cairo.cairo-carbon   cc-options: cc-flags ld-options: ld-flags)
            (jazz.compile-module 'jazz.platform.cairo.cairo-freetype cc-options: cc-flags ld-options: ld-flags))))))
  (windows
    (define (jazz.build-cairo)
      (let ((cairo-include-path (jazz.quote-jazz-gcc-pathname "foreign/cairo/include"))
            (cairo-lib-path     (jazz.quote-jazz-gcc-pathname "foreign/cairo/lib/windows")))
        (jazz.compile-module 'jazz.platform.cairo cc-options: (string-append "-I" cairo-include-path) ld-options: (string-append "-L" cairo-lib-path " -lcairo")))))
  (x11
    (define (jazz.build-cairo)
      (receive (major minor build) (jazz.parse-dot-version (jazz.pkg-config-version "cairo-ft"))
        (if (< minor 4)
            (jazz.error "Cairo 1.4 or higher needed")
          (let ((cc-flags (jazz.pkg-config-cflags "cairo-ft"))
                (ld-flags (jazz.pkg-config-libs "cairo-ft")))
            (jazz.compile-module 'jazz.platform.cairo                cc-options: cc-flags ld-options: ld-flags)
            (jazz.compile-module 'jazz.platform.cairo.cairo-x11      cc-options: cc-flags ld-options: ld-flags)
            (jazz.compile-module 'jazz.platform.cairo.cairo-freetype cc-options: cc-flags ld-options: ld-flags)))))))


(cond-expand
  (windows
    (define (jazz.build-font)
      (jazz.build-logfont)))
  (else
    (define (jazz.build-font)
      (jazz.build-freetype))))


(define (jazz.build-freetype)
  (let ((cc-flags (jazz.pkg-config-cflags "freetype2"))
        (ld-flags (jazz.pkg-config-libs "freetype2")))
    (jazz.compile-module 'jazz.platform.freetype cc-options: cc-flags ld-options: ld-flags)))


(define (jazz.build-logfont)
  (let ((cairo-include-path (jazz.quote-jazz-gcc-pathname "foreign/cairo/include"))
        (cairo-lib-path     (jazz.quote-jazz-gcc-pathname "foreign/cairo/lib/windows")))
    (jazz.compile-module 'jazz.platform.cairo.cairo-logfont cc-options: (string-append "-I" cairo-include-path) ld-options: (string-append "-L" cairo-lib-path " -lcairo"))))


(define (jazz.build-carbon)
  (jazz.load-module 'core.module.build)
  (jazz.compile-module 'jazz.platform.carbon              ld-options: "-framework Carbon")
  (jazz.compile-module 'jazz.platform.carbon.carbon-types ld-options: "-framework Carbon"))


(define (jazz.build-windows)
  (let ((cairo-include-path   (jazz.quote-jazz-gcc-pathname "foreign/cairo/include"))
        (cairo-lib-path       (jazz.quote-jazz-gcc-pathname "foreign/cairo/lib/windows"))
        (windows-include-path (jazz.quote-jazz-gcc-pathname "foreign/windows/include"))
        (windows-lib-path     (jazz.quote-jazz-gcc-pathname "foreign/windows/lib"))
        (base-windows-cc-options "-DUNICODE -D_WIN32_WINNT=0x0502"))
    (jazz.load-module 'core.module.build)
    (jazz.compile-module 'jazz.platform.windows.WinDef      cc-options: base-windows-cc-options ld-options: "-mwindows")
    (jazz.compile-module 'jazz.platform.windows.WinTypes    cc-options: base-windows-cc-options ld-options: "-mwindows")
    (jazz.compile-module 'jazz.platform.windows.WinBase     cc-options: base-windows-cc-options ld-options: "-mwindows")
    (jazz.compile-module 'jazz.platform.windows.WinNT       cc-options: base-windows-cc-options ld-options: "-mwindows")
    (jazz.compile-module 'jazz.platform.windows.WinKernel   cc-options: base-windows-cc-options ld-options: "-mwindows")
    (jazz.compile-module 'jazz.platform.windows.WinGDI      cc-options: base-windows-cc-options ld-options: "-mwindows")
    (jazz.compile-module 'jazz.platform.windows.WinMM       cc-options: base-windows-cc-options ld-options: "-mwindows -lwinmm")
    (jazz.compile-module 'jazz.platform.windows.WinUser     cc-options: base-windows-cc-options ld-options: "-mwindows")
    (jazz.compile-module 'jazz.platform.windows.WinShell    cc-options: base-windows-cc-options ld-options: "-mwindows")
    (jazz.compile-module 'jazz.platform.windows.WinCtrl     cc-options: base-windows-cc-options ld-options: "-mwindows")
    (jazz.compile-module 'jazz.platform.windows.WinDlg      cc-options: base-windows-cc-options ld-options: "-mwindows")
    (jazz.compile-module 'jazz.platform.windows.WinPerf     cc-options: (string-append "-I" windows-include-path " " base-windows-cc-options) ld-options: (string-append "-L" windows-lib-path " -mwindows -lpdh"))
    (jazz.compile-module 'jazz.platform.cairo.cairo-windows cc-options: (string-append "-I" cairo-include-path) ld-options: (string-append "-L" cairo-lib-path " -lcairo"))
    (jazz.compile-module 'jazz.platform.crash.windows       cc-options: base-windows-cc-options ld-options: "-mwindows")
    (jazz.compile-module 'jazz.system.platform.windows)))


(define (jazz.build-com)
  (jazz.compile-module 'jazz.platform.windows.com cc-options: "-DUNICODE" ld-options: "-mwindows -lole32 -loleaut32")
  (jazz.compile-module 'jazz.platform.windows.com.DAO cc-options: "-DUNICODE" ld-options: "-mwindows -lole32")
  (jazz.compile-module 'jazz.platform.windows.com.ADODB cc-options: "-DUNICODE" ld-options: "-mwindows -lole32") ; -u CoCreateInstance
  ;; for test (jazz.compile-module 'jazz.platform.windows.com.ADODB2 cc-options: "-DUNICODE" ld-options: "-mwindows -lole32")
  )


(define (jazz.build-x11)
  (jazz.load-module 'core.module.build)
  (jazz.compile-module 'jazz.platform.x11 cc-options: "-I/usr/X11R6/include" ld-options: "-L/usr/X11R6/lib -lX11")
  (jazz.compile-module 'jazz.platform.x11.x11-types))


(cond-expand
 (mac
  (define (jazz.build-clipboard)
    (jazz.compile-module 'jazz.platform.carbon.carbon-types ld-options: "-framework Carbon")
    (jazz.compile-module 'jazz.platform.carbon.clipboard ld-options: "-framework Carbon")))
 (else
  (define (jazz.build-clipboard) #f)))


(cond-expand
  (carbon
    (define (jazz.build-platform)
      (jazz.build-types)
      (jazz.build-cairo)
      (jazz.build-font)
      (jazz.build-carbon)
      (jazz.build-clipboard)))
  (windows
    (define (jazz.build-platform)
      (let ((install jazz.kernel-install)
            (source jazz.source))
        (define (install-file path)
          (string-append install path))
        
        (define (source-file path)
          (string-append source path))
        
        (define (copy-platform-files)
          (jazz.copy-file (source-file "foreign/cairo/lib/windows/libcairo-2.dll") (install-file "libcairo-2.dll") feedback: jazz.feedback)
          (jazz.copy-file (source-file "foreign/png/lib/windows/libpng13.dll") (install-file "libpng13.dll") feedback: jazz.feedback)
          (jazz.copy-file (source-file "foreign/zlib/lib/windows/zlib1.dll") (install-file "zlib1.dll") feedback: jazz.feedback)
          (jazz.copy-file (source-file "foreign/pixman/lib/windows/libpixman-1-0.dll") (install-file "libpixman-1-0.dll") feedback: jazz.feedback))
        
        (copy-platform-files)
        (jazz.build-types)
        (jazz.build-cairo)
        (jazz.build-font)
        (jazz.build-windows)
        (jazz.build-com))))
  (x11
    (define (jazz.build-platform)
      (jazz.build-types)
      (jazz.build-cairo)
      (jazz.build-font)
      (jazz.build-x11)
      (jazz.build-clipboard))))


(define (jazz.quote-jazz-gcc-pathname suffix)
  (jazz.quote-gcc-pathname (path-expand (string-append jazz.kernel-source suffix)) jazz.kernel-platform))


(define (jazz.parse-dot-version version)
  (let ((version (map string->number (jazz.split-string version #\.))))
    (let ((major (car version))
          (minor (cadr version))
          (build (caddr version)))
      (values major minor build))))


(define (jazz.pkg-config what libname)
  (let ((string-port (open-output-string))
        (process-port (open-process (list path: "pkg-config" arguments: (list what libname)))))
    (if (%%fx= (process-status process-port) 0)
        (begin
          (jazz.pipe-no-return process-port string-port)
          (get-output-string string-port))
      (jazz.error "failed"))))

(define (jazz.pkg-config-cflags libname)
  (jazz.pkg-config "--cflags" libname))

(define (jazz.pkg-config-libs libname)
  (jazz.pkg-config "--libs" libname))

(define (jazz.pkg-config-version libname)
  (jazz.pkg-config "--modversion" libname))

(define (jazz.pipe-no-return input output)
  (let iterate ()
    (let ((c (read-char input)))
      (if (not (or (eof-object? c) (eq? #\newline c)))
          (begin
            (write-char c output)
            (iterate))))))


;;;
;;;; Register
;;;


(jazz.register-product 'platform
  build: jazz.build-platform))
