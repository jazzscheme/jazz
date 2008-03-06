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


(jazz.define-variable jazz.compile-module)
(jazz.define-variable jazz.build-module)
(jazz.define-variable jazz.system.boot-app)


;;;
;;;; Compile
;;;


(define (cmodule module-name #!key (cc-options #f) (ld-options #f))
  (jazz.load-module 'core.module.build)
  (jazz.compile-module module-name cc-options: cc-options ld-options: ld-options))


;;;
;;;; Build
;;;


(define (bmodule module-name)
  (jazz.load-module 'core.module.build)
  (jazz.build-module module-name))


(define (bcore)
  (jazz.load-module 'core.library)
  (bmodule 'core.base)
  (bmodule 'core.class)
  (bmodule 'core.generic)
  (bmodule 'core.library)
  (bmodule 'core.module)
  (bmodule 'statprof))


(define (bjazz)
  (bcore)
  (bmodule 'scheme.dialect)
  (bmodule 'jazz.dialect)
  (cmodule 'jazz.dialect.language))


;;;
;;;; Platform
;;;


(define (pipe-no-return input output)
  (let iterate ()
    (let ((c (read-char input)))
      (if (not (or (eof-object? c) (eq? #\newline c)))
          (begin
            (write-char c output)
            (iterate))))))


(cond-expand
  (windows
   (define (bcairo)
     (define cairo-include-path (path-expand (string-append (jazz.jazz-directory) "foreign/cairo/include")))
     (define cairo-lib-path     (path-expand (string-append (jazz.jazz-directory) "foreign/cairo/lib/windows")))
     (cmodule 'jazz.platform.cairo cc-options: (string-append "-I" cairo-include-path) ld-options: (string-append "-L" cairo-lib-path " -lcairo"))))
  (x11
   (define (bcairo)
     (let ((cc-flags-port (open-output-string))
           (ld-flags-port (open-output-string)))
       (pipe-no-return (open-process (list path: "pkg-config" arguments: (list "--cflags" "cairo"))) cc-flags-port)
       (pipe-no-return (open-process (list path: "pkg-config" arguments: (list "--libs" "cairo"))) ld-flags-port)
       (let ((cc-flags (get-output-string cc-flags-port))
	     (ld-flags (get-output-string ld-flags-port)))
	 (cmodule 'jazz.platform.cairo cc-options: cc-flags ld-options: ld-flags)
	 (cmodule 'jazz.platform.cairo.cairo-x11      cc-options: cc-flags ld-options: ld-flags)
	 (cmodule 'jazz.platform.cairo.cairo-freetype cc-options: cc-flags ld-options: ld-flags)))))
  (carbon
   (define (bcairo)
     (let ((cc-flags-port (open-output-string))
           (ld-flags-port (open-output-string)))
       (pipe-no-return (open-process (list path: "pkg-config" arguments: (list "--cflags" "cairo"))) cc-flags-port)
       (pipe-no-return (open-process (list path: "pkg-config" arguments: (list "--libs" "cairo"))) ld-flags-port)
       (let ((cc-flags (get-output-string cc-flags-port))
	     (ld-flags (get-output-string ld-flags-port)))
	 (cmodule 'jazz.platform.cairo cc-options: cc-flags ld-options: ld-flags)
	 (cmodule 'jazz.platform.cairo.cairo-carbon      cc-options: cc-flags ld-options: ld-flags)
	 (cmodule 'jazz.platform.cairo.cairo-freetype cc-options: cc-flags ld-options: ld-flags))))))


(define (bfreetype)
  (let ((cc-flags-port (open-output-string))
	(ld-flags-port (open-output-string)))
    (pipe-no-return (open-process (list path: "pkg-config" arguments: (list "--cflags" "freetype2"))) cc-flags-port)
    (pipe-no-return (open-process (list path: "pkg-config" arguments: (list "--libs" "freetype2"))) ld-flags-port)
    (let ((cc-flags (get-output-string cc-flags-port))
	  (ld-flags (get-output-string ld-flags-port)))
      (cmodule 'jazz.platform.freetype cc-options: cc-flags ld-options: ld-flags))))


(define (blogfont)
  (define cairo-include-path (path-expand (string-append (jazz.jazz-directory) "foreign/cairo/include")))
  (define cairo-lib-path     (path-expand (string-append (jazz.jazz-directory) "foreign/cairo/lib/windows")))
  (cmodule 'jazz.platform.cairo.cairo-logfont cc-options: (string-append "-I" cairo-include-path) ld-options: (string-append "-L" cairo-lib-path " -lcairo")))


(cond-expand
  (windows
    (define (bfont)
      (blogfont)))
  (else
    (define (bfont)
      (bfreetype))))


(define (bwindows)
  (define cairo-include-path (path-expand (string-append (jazz.jazz-directory) "foreign/cairo/include")))
  (define cairo-lib-path     (path-expand (string-append (jazz.jazz-directory) "foreign/cairo/lib/windows")))
  (jazz.load-module 'core.module.build)
  (cmodule 'jazz.platform.windows.WinDef      cc-options: "-DUNICODE" ld-options: "-mwindows")
  (cmodule 'jazz.platform.windows.WinTypes    cc-options: "-DUNICODE" ld-options: "-mwindows")
  (cmodule 'jazz.platform.windows.WinBase     cc-options: "-DUNICODE" ld-options: "-mwindows")
  (cmodule 'jazz.platform.windows.WinNT       cc-options: "-DUNICODE" ld-options: "-mwindows")
  (cmodule 'jazz.platform.windows.WinKernel   cc-options: "-DUNICODE" ld-options: "-mwindows")
  (cmodule 'jazz.platform.windows.WinGDI      cc-options: "-DUNICODE" ld-options: "-mwindows")
  (cmodule 'jazz.platform.windows.WinMM       cc-options: "-DUNICODE" ld-options: "-mwindows -lwinmm")
  (cmodule 'jazz.platform.windows.WinUser     cc-options: "-DUNICODE" ld-options: "-mwindows")
  (cmodule 'jazz.platform.windows.WinShell    cc-options: "-DUNICODE" ld-options: "-mwindows")
  (cmodule 'jazz.platform.windows.WinCtrl     cc-options: "-DUNICODE" ld-options: "-mwindows")
  (cmodule 'jazz.platform.windows.WinDlg      cc-options: "-DUNICODE" ld-options: "-mwindows")
  (cmodule 'jazz.platform.cairo.cairo-windows cc-options: (string-append "-I" cairo-include-path) ld-options: (string-append "-L" cairo-lib-path " -lcairo"))
  (cmodule 'jazz.system.platform.windows))
  

(define (bx11)
  (jazz.load-module 'core.module.build)
  (cmodule 'jazz.platform.x11                  cc-options: "-I/usr/X11R6/include" ld-options: "-L/usr/X11R6/lib -lX11")
  (cmodule 'jazz.platform.x11.x11-types))


(define (bcarbon)
  (jazz.load-module 'core.module.build)
  (cmodule 'jazz.platform.carbon                ld-options: "-framework Carbon")
  (cmodule 'jazz.platform.carbon.carbon-types   ld-options: "-framework Carbon"))
  

(define (btypes)
  (cmodule 'jazz.platform.types))


(cond-expand
  (windows
    (define (bplatform)
      (jazz.load-module 'core.library)
      (jazz.load-module 'scheme.dialect)
      (btypes)
      (bcairo)
      (bfont)
      (bwindows)))
  (x11
    (define (bplatform)
      (jazz.load-module 'core.library)
      (jazz.load-module 'scheme.dialect)
      (btypes)
      (bcairo)
      (bfont)
      (bx11)))
  (carbon
    (define (bplatform)
      (jazz.load-module 'core.library)
      (jazz.load-module 'scheme.dialect)
      (btypes)
      (bcairo)
      (bfont)
      (bcarbon))))


(define (lplatform)
  (jazz.load-module 'core.library)
  (jazz.load-module 'jazz)
  (jazz.load-module 'jazz.literals)
  (jazz.load-module 'jazz.platform)
  (jazz.load-module 'jazz.platform.literals))


;;;
;;;; Make
;;;


(define (jazz.make-target target)
  (call/cc
    (lambda (stop)
      (with-exception-handler
        (lambda (exc)
          (##display-exception exc (current-output-port))
          (stop #f))
        (lambda ()
          (jazz.make target)
          #t)))))


(define (jazz.make target)
  (case target
    ((core) (bcore))
    ((jazz) (bjazz))
    ((all) (ball))
    ((platform) (bplatform))
    ((jedi) (bjedi))
    (else (jazz.error "Unknown target: {s}" target))))


;;;
;;;; App
;;;


(define (boot-app name)
  (lplatform)
  (jazz.load-module name)
  (jazz.system.boot-app name))


;;;
;;;; Jedi
;;;


(define Jedi-Critical-Modules
  '(;; utilities
    jazz.io
    jazz.literals
    jazz.utilities
    time.implementation
    
    ;; component
    jazz.library.component.Component
    jazz.library.component.Branch
    jazz.library.component.Form
    
    ;; view
    jazz.ui.dialog
    jazz.ui.view
    jazz.ui.layout.Figure
    jazz.ui.view.Drawing
    jazz.ui.view.View
    jazz.ui.view.Scrollbar
    jazz.ui.view.Layout-View
    jazz.ui.view.Container
    jazz.ui.view.Root-View
    jazz.ui.view.Caption-Root
    jazz.ui.view.Frame-Root
    jazz.ui.view.Docked-Root
    jazz.ui.view.Toplevel-Root
    jazz.ui.view.Image-Tool
    jazz.ui.view.Tool-Button
    jazz.ui.window
    jazz.ui.window.Window
    jazz.ui.window.View-Player
    jazz.ui.window.Frame
    jazz.ui.window.Stage
    jazz.ui.window.Pad-Window
    jazz.ui.offscreen
    jazz.ui.graphic.Color
    jazz.ui.graphic.Pen
    jazz.ui.graphic.Surface
    jazz.ui.image.Image
    jazz.ui.image.Portfolio
    jazz.platform
    
    ;; explorer
    jazz.ui.text.Text-Explorer
    jazz.ui.text.Code-Explorer
    jazz.language.jazz.text.Jazz-Explorer
    jazz.language.lisp.text.Lisp-Explorer
    jazz.language.scheme.text.Scheme-Explorer
    
    ;; text
    jazz.ui.graphic.font.Font
    jazz.ui.graphic.Font-Metrics
    jazz.library.node
    jazz.library.exemplar
    jazz.ui.text.Format
    jazz.ui.text.Paragraph
    jazz.ui.text.Line
    jazz.ui.text.Run
    jazz.ui.text.Style
    jazz.ui.text.Text-Style
    jazz.ui.outline.Outline-Row
    jazz.ui.outline.Outline-View
    jazz.ui.text.Text-View
    jazz.ui.text.Code-Text-View
    jazz.ui.text.Text-Colorizer
    jazz.language.jazz.text.Jazz-Text-View
    jazz.language.lisp.text.Lisp-Text-View
    
    ;; catalog
    jazz.catalog.catalog.Catalog
    jazz.catalog.catalog.Filing-Catalog
    jazz.catalog.catalog.Indexed-Catalog
    jazz.catalog.entry.Catalog-Entry
    jazz.catalog.entry.Indexed-Entry
    jazz.catalog.entry.File-Entry
    jazz.catalog.parser.File-Parser
    jazz.language.lisp.catalog.Lisp-Entry
    jazz.language.lisp.catalog.Lisp-File-Entry
    jazz.language.lisp.parser.Lisp-Parser
    jazz.language.scheme.parser.Scheme-Parser
    jazz.language.jazz.parser.Jazz-Parser
    
    ;; tree
    jazz.ui.tree.Tree-View
    jazz.ui.tree.Tree-Column
    jazz.ui.tree.Tree-Row
    
    ;; application
    jazz.system.process.Process
    jazz.system.application.Application
    jazz.ui.workspace
    jazz.ui.workspace.Workspace-Preferences
    jazz.ide.IDE
    jedi.application.Jedi
    
    ;; jml
    jazz.jml
    jazz.jml.parser.JML-Parser
    jazz.jml.model.JML-Node
    jazz.jml.model.JML-Element
    jazz.jml.model.JML-Text
    
    ;; compare
    jazz.groupware.compare.Compare-Directories
    jazz.groupware.compare.Compare-Text-View
    jazz.groupware.compare.Compare-Texts
    jazz.groupware.compare.Compare-Trees
    jazz.groupware.compare.Directory-Comparer
    jazz.groupware.compare.Text-Comparer
    jazz.groupware.compare.Tree-Comparer))


(cond-expand
  (windows
    (define Jedi-Critical-Platform-Modules
      '(jazz.ui.window.platform.windows)))
  (x11
    (define Jedi-Critical-Platform-Modules
      '(jazz.ui.window.platform.x11)))
  (else
    (define Jedi-Critical-Platform-Modules
      '())))


(define (bjedi)
  (bjazz)
  (bplatform)
  (lplatform)
  (for-each cmodule Jedi-Critical-Modules)
  (for-each cmodule Jedi-Critical-Platform-Modules))


(define (jedi)
  (boot-app 'jedi))


;;;
;;;; All
;;;


(define All
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


(define (ball)
  (define (compile module-name)
    (jazz.for-each-submodule module-name
      (lambda (module-name declaration phase)
        (cmodule module-name))))
  
  (jazz.load-module 'core.library)
  (jazz.load-module 'core.module.build)
  (lplatform)
  (for-each compile All))


;;;
;;;; Exception
;;;


(let ((default-exception-handler (current-exception-handler)))
  (current-exception-handler
    (lambda (exc)
      (jazz.bring-terminal-to-front)
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
  
  (split-command-line (%%cdr (command-line)) '() '("app" "make")
    (lambda (options remaining)
      (define (get-option name)
        (let ((pair (%%assoc name options)))
          (if pair
              (%%cdr pair)
            #f)))
      
      (let ((app (get-option "app"))
            (make (get-option "make")))
        (cond (app
               (##repl-debug
                 (lambda (first output-port)
                   (boot-app (%%string->symbol app)))))
              (jazz.app
               (##repl-debug
                 (lambda (first output-port)
                   (boot-app jazz.app))))
              (make
               (exit
                 (if (jazz.make-target (%%string->symbol make))
                     0
                   1)))
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
