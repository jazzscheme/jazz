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


(cond-expand
  (gambit
    (declare (block)
             (standard-bindings)
             (extended-bindings)
             (not safe)))
  (else))


;;;
;;;; Forward
;;;


(jazz.define-variable jazz.compile-module)
(jazz.define-variable jazz.compile-jazz-module)
(jazz.define-variable jazz.build-module)
(jazz.define-variable jazz.system.boot-app)


;;;
;;;; Compile
;;;


(define (cmodule module-name #!key (cc-options #f) (ld-options #f))
  (jazz.load-module 'core.module.build)
  (jazz.compile-module module-name cc-options: cc-options ld-options: ld-options))


(define (cjazz module-name)
  ;; Seems the new gambit functionality has a bug
  ;; It will crash when compiling jazz.utilities
  ;; Also not that until we have adapted the library
  ;; macro to add source code annotations, this is
  ;; very usefull for tools like statprof
  (if #t ;; (memq 'debug jazz.compile-options)
      (cjazzmodule module-name)
    (cmodule module-name)))


(define (cjazzmodule module-name)
  (jazz.load-module 'core.module.build)
  (jazz.load-module 'jazz.build)
  (jazz.compile-jazz-module module-name))


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
  (cjazz 'jazz.dialect.language))


;;;
;;;; Platform
;;;


(cond-expand
  (windows
   (define (bcairo)
     (cmodule 'jazz.platform.cairo cc-options: "-IC:/jazz/dev/jazz/foreign/cairo/include" ld-options: "-LC:/jazz/dev/jazz/foreign/cairo/lib/windows -lcairo")))
  ((and x11 unix)
   (define (bcairo)
     (cmodule 'jazz.platform.cairo cc-options: "-I/u/lasallej/cairo/include -I/u/lasallej/cairo/include/cairo" ld-options: "-L/u/lasallej/cairo/lib -lcairo")
     (cmodule 'jazz.platform.cairo.cairo-x11      cc-options: "-I/u/lasallej/cairo/include -I/u/lasallej/cairo/include/cairo" ld-options: "-L/u/lasallej/cairo/lib -lcairo")
     (cmodule 'jazz.platform.cairo.cairo-freetype cc-options: "-I/usr/include/freetype2 -I/u/lasallej/cairo/include -I/u/lasallej/cairo/include/cairo" ld-options: "-L/u/lasallej/cairo/lib -lcairo")))
  ((and x11 mac)
   (define (bcairo)
     (define cairo-include-path      (path-expand "../../foreign/cairo/include/macosx"))
     (define cairo-lib-path          (path-expand "../../foreign/cairo/lib/macosx"))
     (define png-lib-path            (path-expand "../../foreign/png/lib/macosx"))
     (define xrender-lib-path        (path-expand "../../foreign/xrender/lib/macosx"))
     (define freetype-lib-path       (path-expand "../../foreign/freetype/lib/macosx"))
     (define freetype-include-path   (path-expand "../../foreign/freetype/include/freetype2"))
     (define fontconfig-lib-path       (path-expand "../../foreign/fontconfig/lib/macosx"))
     (define fontconfig-include-path (path-expand "../../foreign/fontconfig/include"))
     (cmodule 'jazz.platform.cairo                cc-options: (string-append "-I" cairo-include-path) ld-options: (string-append "-L" cairo-lib-path " -L" freetype-lib-path " -L" png-lib-path " -L" xrender-lib-path " -L" fontconfig-lib-path " -lcairo"))
     (cmodule 'jazz.platform.cairo.cairo-x11      cc-options: (string-append "-I" cairo-include-path) ld-options: (string-append "-L" cairo-lib-path " -L" freetype-lib-path " -L" png-lib-path " -L" xrender-lib-path " -L" fontconfig-lib-path " -lcairo"))
     (cmodule 'jazz.platform.cairo.cairo-freetype cc-options: (string-append "-I" cairo-include-path " -I" freetype-include-path " -I" fontconfig-include-path) ld-options: (string-append "-L" cairo-lib-path " -L" freetype-lib-path " -L" png-lib-path " -L" xrender-lib-path " -L" fontconfig-lib-path " -lcairo")))))


(cond-expand
 (unix
  (define (bfreetype)
    (cmodule 'jazz.platform.freetype cc-options: "-I/usr/include/freetype2" ld-options: "-lfreetype")))
 (mac
  (define (bfreetype)
    (define freetype-include-path (path-expand "../../foreign/freetype/include/freetype2"))
    (define freetype-lib-path     (path-expand "../../foreign/freetype/lib/macosx"))
    (cmodule 'jazz.platform.freetype cc-options: (string-append "-I" freetype-include-path) ld-options: (string-append "-L" freetype-lib-path " -lfreetype")))))


(define (blogfont)
  (cmodule 'jazz.platform.cairo.cairo-logfont cc-options: "-IC:/jazz/dev/jazz/foreign/cairo/include" ld-options: "-LC:/jazz/dev/jazz/foreign/cairo/lib/windows -lcairo"))


(cond-expand
  (windows
    (define (bfont)
      (blogfont)))
  (else
    (define (bfont)
      (bfreetype))))


(define (bwindows)
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
  (cmodule 'jazz.platform.cairo.cairo-windows cc-options: "-IC:/jazz/dev/jazz/foreign/cairo/include" ld-options: "-LC:/jazz/dev/jazz/foreign/cairo/lib/windows -lcairo")
  (cjazz 'jazz.system.platform.windows))
  

(cond-expand
 (unix
  (define (bx11)
    (jazz.load-module 'core.module.build) 
    (cmodule 'jazz.platform.x11                  cc-options: "-I/usr/X11R6/include" ld-options: "-L/usr/X11R6/lib64 -lX11")
    (cmodule 'jazz.platform.x11.x11-types)))
 (mac
  (define (bx11)
    (jazz.load-module 'core.module.build) 
    (cmodule 'jazz.platform.x11                  cc-options: "-I/usr/X11R6/include" ld-options: "-L/usr/X11R6/lib -lX11")
    (cmodule 'jazz.platform.x11.x11-types))))


(define (btypes)
  (cmodule 'jazz.platform.types))


(cond-expand
  (windows
    (define (bplatform)
      (bjazz)
      (btypes)
      (bcairo)
      (bfont)
      (bwindows)))
  (x11
    (define (bplatform)
      (bjazz)
      (btypes)
      (bcairo)
      (bfont)
      (bx11))))


(define (lplatform)
  (jazz.load-module 'core.library)
  (jazz.load-module 'jazz)
  (jazz.load-module 'jazz.literals)
  (jazz.load-module 'jazz.platform)
  (jazz.load-module 'jazz.platform.literals))


;;;
;;;; Make
;;;


(define (jazz.make target)
  (case target
    ((core) (bcore))
    ((jazz) (bjazz))
    ((platform) (bplatform))
    ((jedi) (bjedi))
    ((p4) (bp4))
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
      '(jazz.ui.window.platform.x11))))


(define (bjedi)
  (bjazz)
  (bplatform)
  (lplatform)
  (for-each cjazz Jedi-Critical-Modules)
  (for-each cjazz Jedi-Critical-Platform-Modules))


(define (jedi)
  (boot-app 'jedi))


;;;
;;;; P4
;;;


(define (bp4)
  (bjazz)
  (bplatform))


;;;
;;;; Main
;;;


(define jazz.version
  "2.0a1")


(cond-expand
  (x11
    (define (jazz.set-library-environment!)
      (setenv "DYLD_LIBRARY_PATH" (path-expand "./"))))
  (windows
    (define (jazz.set-library-environment!)
      #f)))

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
               (boot-app (%%string->symbol app)))
              (jazz.app
               (boot-app jazz.app))
              (make
               (jazz.make (%%string->symbol make)))
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
