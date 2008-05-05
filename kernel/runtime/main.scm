;;#!gsi -:dar,m250000
;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Kernel Main
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


(jazz.kernel-declare)


;;;
;;;; Forward
;;;


(jazz.define-variable jazz.compile-module-internal)
(jazz.define-variable jazz.build-module-internal)


(define (jazz.compile-module . rest)
  (jazz.load-module 'core.module.build)
  (apply jazz.compile-module-internal rest))

(define (jazz.build-module . rest)
  (jazz.load-module 'core.module.build)
  (apply jazz.build-module-internal rest))


;;;
;;;; Core
;;;


(define (jazz.build-core)
  (jazz.load-module 'core.library)
  (jazz.build-module 'core.base)
  (jazz.build-module 'core.class)
  (jazz.build-module 'core.generic)
  (jazz.build-module 'core.library)
  (jazz.build-module 'core.module)
  (jazz.build-module 'statprof))


;;;
;;;; Jazz
;;;


(define (jazz.build-jazz)
  (jazz.build-core)
  (jazz.build-module 'jazz))


;;;
;;;; Platform
;;;
  

(define (jazz.build-types)
  (jazz.compile-module 'jazz.platform.types))


(cond-expand
  (carbon
    (define (jazz.build-cairo)
      (receive (major minor build) (jazz.parse-dot-version (jazz.pkg-config-version "cairo"))
        (if (< minor 4)
            (jazz.error "Cairo 1.4 or higher needed")
          (let ((cc-flags (jazz.pkg-config-cflags "cairo"))
                (ld-flags (jazz.pkg-config-libs "cairo")))
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
      (receive (major minor build) (jazz.parse-dot-version (jazz.pkg-config-version "cairo"))
        (if (< minor 4)
            (jazz.error "Cairo 1.4 or higher needed")
          (let ((cc-flags (jazz.pkg-config-cflags "cairo"))
                (ld-flags (jazz.pkg-config-libs "cairo")))
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
    (jazz.compile-module 'jazz.system.platform.windows)))


(define (jazz.build-com)
  (jazz.compile-module 'jazz.platform.windows.com.ComTypes options: '(keep-c) cc-options: "-DUNICODE" ld-options: "-mwindows -loleaut32")
  (jazz.compile-module 'jazz.platform.windows.com.ComUtils options: '(keep-c) cc-options: "-DUNICODE" ld-options: "-mwindows -lole32")
  (jazz.compile-module 'jazz.platform.windows.com.DAO options: '(keep-c) cc-options: "-DUNICODE" ld-options: "-mwindows -lole32")) ; -u CoCreateInstance
  

(define (jazz.build-x11)
  (jazz.load-module 'core.module.build)
  (jazz.compile-module 'jazz.platform.x11 cc-options: "-I/usr/X11R6/include" ld-options: "-L/usr/X11R6/lib -lX11")
  (jazz.compile-module 'jazz.platform.x11.x11-types))


(cond-expand
  (carbon
    (define (jazz.build-platform)
      (jazz.load-module 'core.library)
      (jazz.load-module 'scheme.dialect)
      (jazz.build-types)
      (jazz.build-cairo)
      (jazz.build-font)
      (jazz.build-carbon)))
  (windows
    (define (jazz.build-platform)
      (let ((install jazz.install)
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
        (jazz.load-module 'core.library)
        (jazz.load-module 'scheme.dialect)
        (jazz.build-types)
        (jazz.build-cairo)
        (jazz.build-font)
        (jazz.build-windows)
        (jazz.build-com))))
  (x11
    (define (jazz.build-platform)
      (jazz.load-module 'core.library)
      (jazz.load-module 'scheme.dialect)
      (jazz.build-types)
      (jazz.build-cairo)
      (jazz.build-font)
      (jazz.build-x11))))


(define (jazz.load-literals)
  (jazz.load-module 'core.library)
  (jazz.load-module 'jazz)
  (jazz.load-module 'jazz.literals))


(define (jazz.load-platform)
  (jazz.load-literals)
  (jazz.load-module 'jazz.platform)
  (jazz.load-module 'jazz.platform.literals))


(define (jazz.quote-jazz-gcc-pathname suffix)
  (jazz.quote-gcc-pathname (path-expand (string-append jazz.jazz-source suffix)) jazz.platform))


(define (jazz.parse-dot-version version)
  (let ((version (map string->number (jazz.split-string (jazz.pkg-config-version "cairo") #\.))))
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
;;;; Build
;;;


(define (jazz.build name)
  (case name
    ((core) (jazz.build-core))
    ((jazz) (jazz.build-jazz))
    ((platform) (jazz.build-platform))
    ((all) (jazz.build-all))
    (else (jazz.build-product name))))


;;;
;;;; Jazz
;;;


(define (jazz)
  (jazz.load-module 'core.library)
  (jazz.load-module 'jazz))


;;;
;;;; Jedi
;;;


(define (jedi)
  (jazz.run-product 'jedi))


;;;
;;;; All
;;;


(define jazz.All-Modules
  '(jazz
    jazz.access
    jazz.build
    jazz.builder
    jazz.catalog
    jazz.console
    jazz.debuggee
    jazz.debugger
    jazz.designer
    jazz.development
    jazz.doc
    jazz.dialect
    jazz.groupware
    jazz.ide
    jazz.io
    jazz.jml
    jazz.jrm
    jazz.language.c
    jazz.language.clike
    jazz.language.commonlisp
    jazz.language.csharp
    jazz.language.css
    jazz.language.html
    jazz.language.java
    jazz.language.javascript
    jazz.language.jazz
    jazz.language.jazz.debuggee
    jazz.language.jml
    jazz.language.lisp
    jazz.language.lua
    jazz.language.properties
    jazz.language.python
    jazz.language.scheme
    jazz.language.sql
    jazz.language.xml
    jazz.library
    jazz.library.component
    jazz.library.listener
    jazz.library.node
    jazz.library.shell
    jazz.license
    jazz.media
    jazz.network
    jazz.platform
    jazz.platform.literals
    jazz.profile
    jazz.recorder
    jazz.repository
    jazz.resources
    jazz.runtime
    jazz.schema
    jazz.snow
    jazz.system
    jazz.system.application
    jazz.system.process
    jazz.test
    jazz.ui
    jazz.ui.activity
    jazz.ui.clipboard
    jazz.ui.development
    jazz.ui.dialog
    jazz.ui.graphic.font
    jazz.ui.history
    jazz.ui.image
    jazz.ui.login
    jazz.ui.look
    jazz.ui.menu
    jazz.ui.offscreen
    jazz.ui.resizer
    jazz.ui.view
    jazz.ui.window
    jazz.ui.workspace
    jazz.utilities
    jazz.xml
    time))


(define (jazz.build-all)
  (define (compile module-name)
    (jazz.for-each-submodule module-name
      (lambda (module-name declaration phase)
        (jazz.compile-module module-name))))
  
  (jazz.load-module 'core.library)
  (jazz.load-module 'core.module.build)
  (jazz.load-platform)
  (for-each compile jazz.All-Modules))


;;;
;;;; Exception
;;;


(let ((default-exception-handler (current-exception-handler)))
  (current-exception-handler
    (lambda (exc)
      (if (tty? (current-output-port))
          (begin
            (jazz.set-terminal-title)
            (jazz.bring-terminal-to-front)))
      (default-exception-handler exc))))


(define (jazz.set-terminal-title)
  (display "\033]0;Terminal\007"))

(define (jazz.bring-terminal-to-front)
  (display "\033[5t"))


;;;
;;;; Main
;;;


(define jazz.debug-build?
  #f)


(define (jazz.main)
  (define (warn-missing-argument-for-option opt)
    (jazz.repl-main
      (lambda (output-port)
        (##write-string
          "*** WARNING -- Missing argument for option \""
          output-port)
        (##write-string opt output-port)
        (##write-string "\"\n" output-port)
        #t)))
  
  (define (option? arg)
    (and (##fixnum.< 0 (##string-length arg))
         (or (##char=? (##string-ref arg 0) #\-)
             (##char=? (##string-ref arg 0) #\/))))
             
  (define (convert-option arg)
    (##substring arg 1 (##string-length arg)))

  (define (split-command-line
           arguments
           options-with-no-args
           options-with-args
           cont)
    (let loop ((args arguments)
               (rev-options '()))
      (if (and (##pair? args)
               (option? (##car args)))
          (let ((opt (convert-option (##car args)))
                (rest (##cdr args)))
            (cond ((##member opt options-with-no-args)
                   (loop rest
                         (##cons (##cons opt #f) rev-options)))
                  ((##member opt options-with-args)
                   (if (##pair? rest)
                       (loop (##cdr rest)
                             (##cons (##cons opt (##car rest)) rev-options))
                     (begin
                       (warn-missing-argument-for-option opt)
                       (loop rest rev-options))))
                  (else
                   (cont (##reverse rev-options) args))))
        (cont (##reverse rev-options) args))))
  
  (split-command-line (%%cdr (command-line)) '() '("run" "build")
    (lambda (options remaining)
      (define (get-option name)
        (let ((pair (%%assoc name options)))
          (if pair
              (%%cdr pair)
            #f)))
      
      (let ((run (get-option "run"))
            (build (get-option "build")))
        (cond (run
               (jazz.run-product (%%string->symbol run)))
              (jazz.product
               (jazz.run-product jazz.product))
              (build
                (let ((current-handler (current-exception-handler)))
                  (with-exception-handler
                    (lambda (exc)
                      (jazz.debug-exception exc (console-port) jazz.debug-build? jazz.debug-build?)
                      (current-handler exc))
                    (lambda ()
                      (jazz.build (%%string->symbol build))))))
              (else
               (jazz.repl-main #f)))))))


(define (jazz.repl-main warnings)
  (current-input-port (repl-input-port))
  (current-output-port (repl-output-port))
  (current-error-port (repl-output-port))
  (##repl-debug
    (lambda (first output-port)
      (if warnings
          (warnings output-port))
      (display "Jazz " output-port)
      (display jazz.version output-port)
      (newline output-port)
      (newline output-port)
      (force-output output-port)
      #f)))


(##main-set! jazz.main)
