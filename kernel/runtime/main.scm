#!gsi -:dar,m250000
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
;;;  Portions created by the Initial Developer are Copyright (C) 1996-2006
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


(jazz.define-variable jazz.compile-module
  (lambda rest
    (jazz.load-module 'core.module.build)
    (apply jazz.compile-module rest)))

(jazz.define-variable jazz.build-module
  (lambda rest
    (jazz.load-module 'core.module.build)
    (apply jazz.build-module rest)))

(jazz.define-variable jazz.system.run-product)


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
  (jazz.build-module 'scheme.dialect)
  (jazz.build-module 'jazz.dialect)
  (jazz.compile-module 'jazz.dialect.language))


;;;
;;;; Platform
;;;
  

(define (jazz.build-types)
  (jazz.compile-module 'jazz.platform.types))


(cond-expand
  (carbon
   (define (jazz.build-cairo)
     (let ((cc-flags (jazz.pkg-config-cflags "cairo"))
           (ld-flags (jazz.pkg-config-libs "cairo")))
       (jazz.compile-module 'jazz.platform.cairo                cc-options: cc-flags ld-options: ld-flags)
       (jazz.compile-module 'jazz.platform.cairo.cairo-carbon   cc-options: cc-flags ld-options: ld-flags)
       (jazz.compile-module 'jazz.platform.cairo.cairo-freetype cc-options: cc-flags ld-options: ld-flags))))
  (windows
   (define (jazz.build-cairo)
     (let ((cairo-include-path (jazz.quote-jazz-gcc-pathname "foreign/cairo/include"))
           (cairo-lib-path     (jazz.quote-jazz-gcc-pathname "foreign/cairo/lib/windows")))
       (jazz.compile-module 'jazz.platform.cairo cc-options: (string-append "-I" cairo-include-path) ld-options: (string-append "-L" cairo-lib-path " -lcairo")))))
  (x11
   (define (jazz.build-cairo)
     (let ((cc-flags (jazz.pkg-config-cflags "cairo"))
           (ld-flags (jazz.pkg-config-libs "cairo")))
       (jazz.compile-module 'jazz.platform.cairo                cc-options: cc-flags ld-options: ld-flags)
       (jazz.compile-module 'jazz.platform.cairo.cairo-x11      cc-options: cc-flags ld-options: ld-flags)
       (jazz.compile-module 'jazz.platform.cairo.cairo-freetype cc-options: cc-flags ld-options: ld-flags)))))


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
  (let ((cairo-include-path (jazz.quote-jazz-gcc-pathname "foreign/cairo/include"))
        (cairo-lib-path     (jazz.quote-jazz-gcc-pathname "foreign/cairo/lib/windows")))
    (jazz.load-module 'core.module.build)
    (jazz.compile-module 'jazz.platform.windows.WinDef      cc-options: "-DUNICODE" ld-options: "-mwindows")
    (jazz.compile-module 'jazz.platform.windows.WinTypes    cc-options: "-DUNICODE" ld-options: "-mwindows")
    (jazz.compile-module 'jazz.platform.windows.WinBase     cc-options: "-DUNICODE" ld-options: "-mwindows")
    (jazz.compile-module 'jazz.platform.windows.WinNT       cc-options: "-DUNICODE" ld-options: "-mwindows")
    (jazz.compile-module 'jazz.platform.windows.WinKernel   cc-options: "-DUNICODE" ld-options: "-mwindows")
    (jazz.compile-module 'jazz.platform.windows.WinGDI      cc-options: "-DUNICODE" ld-options: "-mwindows")
    (jazz.compile-module 'jazz.platform.windows.WinMM       cc-options: "-DUNICODE" ld-options: "-mwindows -lwinmm")
    (jazz.compile-module 'jazz.platform.windows.WinUser     cc-options: "-DUNICODE" ld-options: "-mwindows")
    (jazz.compile-module 'jazz.platform.windows.WinShell    cc-options: "-DUNICODE" ld-options: "-mwindows")
    (jazz.compile-module 'jazz.platform.windows.WinCtrl     cc-options: "-DUNICODE" ld-options: "-mwindows")
    (jazz.compile-module 'jazz.platform.windows.WinDlg      cc-options: "-DUNICODE" ld-options: "-mwindows")
    (jazz.compile-module 'jazz.platform.cairo.cairo-windows cc-options: (string-append "-I" cairo-include-path) ld-options: (string-append "-L" cairo-lib-path " -lcairo"))
    (jazz.compile-module 'jazz.system.platform.windows)))
  

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
      (jazz.load-module 'core.library)
      (jazz.load-module 'scheme.dialect)
      (jazz.build-types)
      (jazz.build-cairo)
      (jazz.build-font)
      (jazz.build-windows)))
  (x11
    (define (jazz.build-platform)
      (jazz.load-module 'core.library)
      (jazz.load-module 'scheme.dialect)
      (jazz.build-types)
      (jazz.build-cairo)
      (jazz.build-font)
      (jazz.build-x11))))


(define (jazz.load-platform)
  (jazz.load-module 'core.library)
  (jazz.load-module 'jazz)
  (jazz.load-module 'jazz.literals)
  (jazz.load-module 'jazz.platform)
  (jazz.load-module 'jazz.platform.literals))


(define (jazz.quote-jazz-gcc-pathname suffix)
  (jazz.quote-gcc-pathname (path-expand (string-append (jazz.jazz-directory) suffix)) jazz.platform))


(define (jazz.pkg-config what libname)
  (let ((port (open-output-string)))
    (jazz.pipe-no-return (open-process (list path: "pkg-config" arguments: (list what libname))) port)
    (get-output-string port)))

(define (jazz.pkg-config-cflags libname)
  (jazz.pkg-config "--cflags" libname))

(define (jazz.pkg-config-libs libname)
  (jazz.pkg-config "--libs" libname))


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
;;;; Jedi
;;;


(define (jedi)
  (jazz.run-product 'jedi))


;;;
;;;; All
;;;


(define jazz.All-Modules
  '(;;jazz.access
    jazz.build
    jazz.builder
    jazz.catalog
    jazz.console
    jazz.debuggee
    jazz.debugger
    jazz.designer
    jazz.development
    jazz.dialect
    jazz.groupware
    jazz.ide
    jazz.io
    jazz.jml
    jazz.language.c
    jazz.language.clike
    jazz.language.commonlisp
    jazz.language.csharp
    jazz.language.css
    jazz.language.html
    jazz.language.java
    jazz.language.javascript
    jazz.language.jazz
    jazz.language.jml
    jazz.language.lisp
    jazz.language.lua
    jazz.language.properties
    jazz.language.python
    jazz.language.scheme
    jazz.language.sql
    jazz.language.xml
    jazz.library
    jazz.license
    jazz.media
    jazz.network
    jazz.platform
    jazz.profile
    jazz.recorder
    jazz.repository
    jazz.resources
    jazz.runtime
    jazz.schema
    jazz.snow
    jazz.system
    jazz.system.process
    jazz.test
    jazz.ui
    jazz.ui.clipboard
    jazz.ui.image
    jazz.ui.menu
    jazz.ui.view
    jazz.ui.window
    jazz.utilities))


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
          (jazz.bring-terminal-to-front))
      (default-exception-handler exc))))


(define (jazz.bring-terminal-to-front)
  (display "\033[5t"))


;;;
;;;; Main
;;;


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
               (##repl-debug
                 (lambda (first output-port)
                   (jazz.run-product (%%string->symbol run)))))
              (jazz.product
               (##repl-debug
                 (lambda (first output-port)
                   (jazz.run-product jazz.product))))
              (build
               (jazz.build (%%string->symbol build)))
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
