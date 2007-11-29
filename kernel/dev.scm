;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Jazz Debugging
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


(generate-proper-tail-calls #f)
(display-environment-set! #t)


(define (block-tail-call)
  #f)


;;;
;;;; Load
;;;


(define (l module-name)
  (jazz.boot-kernel)
  (jazz.load-module module-name)
  (void))


(define (rl module-name)
  (jazz.boot-kernel)
  (jazz.reload-module module-name)
  (void))


(define (ll)
  (l 'core.library))


(define (ld)
  (ll)
  (l 'core.dev))


(define (lj)
  (ll)
  (l 'jazz))


(define (li)
  (lj)
  (l 'jazz.literals))


(define (la)
  (li)
  (l 'jazz.platform)
  (l 'jazz.platform.literals)
  (jazz.platform.initialize-aliases))


(define (lt)
  (ll)
  (rl 'test))


(define (lut)
  (ll)
  (rl 'user.test))


(define (lex module-name)
  (ef module-name)
  (load "x"))


(define (llex)
  (lex 'jazz.dialect.language))


;;;
;;;; Parse
;;;


(define (parse library-name)
  (jazz.locate-toplevel-declaration library-name))


(define (lookup library-name name)
  (jazz.lookup-declaration (parse library-name) name #t))


;;;
;;;; Walk
;;;


(define (walk library-name)
  (let ((source (jazz.path-filename (jazz.find-module-src library-name))))
    (let ((form (jazz.read-toplevel-form source #f)))
      (jazz.walk-library (cdr form)))))


(define (ppt)
  (ll)
  (ppl 'test))


;;;
;;;; Expand
;;;


(define (em module-name)
  (expand-module module-name))


(define (expand-module module-name)
  (let ((source (jazz.path-filename (jazz.find-module-src module-name))))
    (let ((form (jazz.read-toplevel-form source #f)))
      (let ((name (cadr form))
            (rest (cddr form)))
        (pretty-print (jazz.expand-module name rest) (current-output-port))))))


(define (e library-name)
  (ll)
  (expand library-name))


(define (ec library-name)
  (parameterize ((jazz.walk-for 'compile))
    (e library-name)))


(define (ef library-name)
  (ll)
  (expand-to-file library-name))


(define (ee library-name)
  (ld)
  (jazz.compile-module library-name options: (list 'expansion)))


(define (eg library-name)
  (ld)
  (jazz.compile-module library-name options: (list 'gvm)))


(define (expand library-name)
  (expand-to-port library-name (current-output-port)))


(define (expand-to-file library-name . rest)
  (let ((filename (if (null? rest) "x.scm" (car rest))))
    (call-with-output-file filename
      (lambda (port)
        (expand-to-port library-name port)))))


(define (expand-to-port library-name port)
  (let ((source (jazz.path-filename (jazz.find-module-src library-name))))
    (let ((form (jazz.read-toplevel-form source #f)))
      (let ((kind (car form))
            (rest (cdr form)))
        (pretty-print (parameterize ((jazz.requested-module-name library-name))
                        (case kind
                          ((module) (jazz.expand-module (car rest) (cdr rest)))
                          ((library) (jazz.expand-library rest))))
                      port)))))


(define (el)
  (ef 'jazz.dialect.language))

(define (et)
  (e 'test))

(define (etc)
  (ec 'test))


;;;
;;;; Compile
;;;


(define (cmodule module-name cc-options ld-options)
  (ld)
  (jazz.compile-module module-name cc-options: cc-options ld-options: ld-options))


(define (cm module-name)
  (ld)
  (jazz.compile-module module-name))


(define (cj module-name)
  ;; seems the new gambit functionality has a bug
  (if #t ;; (memq 'debug jazz.compile-options)
      (cjscm module-name)
    (cm module-name)))


;; Generates an intermediate jscm expansion file. This is usefull for debugging until
;; we implement the library macro as a source transformation like for module. This will
;; probably be a very complex task
(define (cjscm module-name)
  (ld)
  (lj)
  (let* ((jazz (jazz.find-module-src module-name))
         (jscm (jazz.make-path jazz.bin-package (jazz.path-name jazz) "jscm"))
         (bin (jazz.path-find-binary jscm))
         (jazztime (jazz.path-modification-time jazz))
         (jscmtime (jazz.path-modification-time jscm))
         (bintime (jazz.path-modification-time bin)))
    (if (or (not jscmtime) (> jazztime jscmtime)
            (not bintime) (> jscmtime bintime))
        (begin
          (jazz.create-directories (jazz.path-bin-dir jscm))
          (expand-to-file module-name (jazz.path-filename jscm))
          (parameterize ((current-readtable jazz.jazz-readtable))
            (jazz.compile-source-path jscm))))))


;;;
;;;; Build
;;;


(define (bkernel)
  (ld)
  (jazz.build-kernel))


(define (bmodule module-name)
  (ld)
  (jazz.build-module module-name))


(define (bcore)
  (ll)
  (bkernel)
  (bmodule 'core.base)
  (bmodule 'core.class)
  (bmodule 'core.foundation)
  (bmodule 'core.generic)
  (bmodule 'core.library))


(define (blang)
  (cj 'jazz.dialect.language))


(define (bjazz)
  (bcore)
  (bmodule 'scheme.dialect)
  (bmodule 'jazz.dialect)
  (blang))


(cond-expand
  (windows
    (define (bcairo)
      (cmodule 'jazz.platform.cairo "-IC:/jazz/dev/jazz/include/cairo" "-LC:/jazz/dev/jazz/lib/cairo -lcairo")))
  (x11
    (define (bcairo)
      (cmodule 'jazz.platform.cairo "-I/opt/local/include/cairo -I/opt/local/include" "-L/opt/local/lib -lcairo"))))


(define (bfreetype)
  (cmodule 'jazz.platform.freetype "-I/opt/local/include -I/opt/local/include/freetype2" "-L/opt/local/lib -lfreetype")
  (cmodule 'jazz.platform.cairo.cairo-freetype "-I/opt/local/include -I/opt/local/include/freetype2 -I/opt/local/include/cairo" "-L/opt/local/lib -lcairo"))


(define (blogfont)
  (cmodule 'jazz.platform.cairo.cairo-logfont "-IC:/jazz/dev/jazz/include/cairo" "-LC:/jazz/dev/jazz/lib/cairo -lcairo"))


(cond-expand
  (freetype
    (define (bfont)
      (bfreetype)))
  (logfont
    (define (bfont)
      (blogfont))))


(define (bwindows)
  (ld)
  (cmodule 'jazz.platform.windows.WinDef "-D UNICODE" "-mwindows")
  (cmodule 'jazz.platform.windows.WinTypes "-D UNICODE" "-mwindows")
  (cmodule 'jazz.platform.windows.WinBase "-D UNICODE" "-mwindows")
  (cmodule 'jazz.platform.windows.WinNT  "-D UNICODE" "-mwindows")
  (cmodule 'jazz.platform.windows.WinKernel "-D UNICODE" "-mwindows")
  (cmodule 'jazz.platform.windows.WinGDI "-D UNICODE" "-mwindows")
  (cmodule 'jazz.platform.windows.WinUser "-D UNICODE" "-mwindows -lUser32")
  (cmodule 'jazz.platform.windows.WinShell "-D UNICODE" "-mwindows")
  (cmodule 'jazz.platform.windows.WinCtrl "-D UNICODE" "-mwindows")
  (cmodule 'jazz.platform.windows.WinDlg "-D UNICODE" "-mwindows")
  (cmodule 'jazz.platform.cairo.cairo-windows "-IC:/jazz/dev/jazz/include/cairo" "-LC:/jazz/dev/jazz/lib/cairo -lcairo")
  (cj 'jazz.system.platform.windows))
  

(define (bx11)
  (ld) 
  (cmodule 'jazz.platform.x11 "-I/usr/X11R6/include" "-L/usr/X11R6/lib -lX11")
  (cmodule 'jazz.platform.freetype "-I/opt/local/include -I/opt/local/include/freetype2" "-L/opt/local/lib -lfreetype")
  (cmodule 'jazz.platform.cairo.cairo-x11 "-I/opt/local/include/cairo" "-L/opt/local/lib -lcairo")
  (cmodule 'jazz.platform.cairo.cairo-freetype "-I/opt/local/include/cairo -I/opt/local/include -I/opt/local/include/freetype2" "-L/opt/local/lib -lcairo")
  (cmodule 'jazz.platform.cairo "-I/opt/local/include/cairo" "-L/opt/local/lib -lcairo"))


(cond-expand
  (windows
    (define (bplatform)
      (bwindows)))
  (x11
    (define (bplatform)
      (bx11))))


;; build everything that needs to be compiled
(define (bcmp)
  (ld)
  (bcairo)
  (bfont)
  (bplatform))


(define (build)
  (bjazz)
  (bcairo)
  (bfont)
  (bplatform))

(define (b)
  (build))


(define Util
  '(jazz.io
    jazz.literals
    jazz.utilities
    time.implementation))

(define Comp
  '(jazz.library.component.Component
    jazz.library.component.Branch
    jazz.library.component.Form))

(define View
  '(jazz.ui.dialog
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
    jazz.ui.window.platform.windows
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
    jazz.platform))

(define Expl
  '(jazz.ui.text.Text-Explorer
    jazz.ui.text.Code-Explorer
    jazz.language.jazz.text.Jazz-Explorer
    jazz.language.lisp.text.Lisp-Explorer
    jazz.language.scheme.text.Scheme-Explorer))

(define Text
  '(jazz.ui.graphic.font.Font
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
    jazz.language.lisp.text.Lisp-Text-View))

(define Cat
  '(jazz.catalog.catalog.Catalog
    jazz.catalog.catalog.Filing-Catalog
    jazz.catalog.catalog.Indexed-Catalog
    jazz.catalog.entry.Catalog-Entry
    jazz.catalog.entry.Indexed-Entry
    jazz.catalog.entry.Lisp-Entry
    jazz.catalog.entry.File-Entry
    jazz.catalog.entry.Lisp-File-Entry
    jazz.catalog.parser.File-Parser
    jazz.catalog.parser.Lisp-Parser
    jazz.catalog.parser.Scheme-Parser
    jazz.catalog.parser.Jazz-Parser))

(define Tree
  '(jazz.ui.tree.Tree-View
    jazz.ui.tree.Tree-Column
    jazz.ui.tree.Tree-Row))

(define Appl
  '(jazz.process.Process
    jazz.application.Application
    jazz.application.platform.windows
    jazz.ui.workspace
    jazz.ui.workspace.Workspace-Preferences
    jazz.ide.IDE
    jedi.application.Jedi))

(define JML
  '(jazz.jml
    jazz.jml.parser.JML-Parser
    jazz.jml.model.JML-Node
    jazz.jml.model.JML-Element
    jazz.jml.model.JML-Text))

(define Compare
  '(jazz.groupware.compare.Compare-Directories
    jazz.groupware.compare.Compare-Text-View
    jazz.groupware.compare.Compare-Texts
    jazz.groupware.compare.Compare-Trees
    jazz.groupware.compare.Directory-Comparer
    jazz.groupware.compare.Text-Comparer
    jazz.groupware.compare.Tree-Comparer))

(define Extra
  '(digest.implementation))


(define (butil)
  (for-each cj Util))

(define (bcomp)
  (for-each cj Comp))

(define (bview)
  (for-each cj View))

(define (bexpl)
  (for-each cj Expl))

(define (btext)
  (for-each cj Text))

(define (bcat)
  (for-each cj Cat))

(define (btree)
  (for-each cj Tree))

(define (bappl)
  (for-each cj Appl))

(define (bjml)
  (for-each cj JML))

(define (bcompare)
  (for-each cj Compare))

(define (bextra)
  (for-each cj Extra))


(define (bjedi)
  (la)
  (build)
  (butil)
  (bcomp)
  (bview)
  (bexpl)
  (btext)
  (bcat)
  (btree)
  (bappl)
  (bjml)
  (bcompare)
  (bextra))

(define (bjd)
  (bjedi))


;;;
;;;; Debug
;;;


(define (cat)
  (table->list jazz.Catalog))


(define (env)
  (table->list jazz.Environment))


;; inspect jazz object
(define (i obj)
  (jazz.inspect-object (if (integer? obj) (jazz.serial-number->object obj) obj)))


;; inspect annotated environment
(define (ienv env)
  (jazz.inspect-annotated-environment env))


;; slot value
(define (sv obj slot-name)
  (jazz.slot-value obj slot-name))


;;;
;;;; Profile
;;;


;; this is not nice but it is for now the only way to share
;; usage of statprof between the scheme code and the jazz code


(define jazz.statprof-loaded?
  #f)


(define profile-start! #f) (set! profile-start! #f)
(define profile-stop! #f) (set! profile-stop! #f)
(define profile-reset! #f) (set! profile-reset! #f)
(define write-profile-report #f) (set! write-profile-report #f)


(define (spl)
  (if (not jazz.statprof-loaded?)
      (begin
        (jazz.load-module 'statprof)
        (set! jazz.statprof-loaded? #t))))


(define (spb)
  (spl)
  (profile-start!))

(define (spe)
  (spl)
  (profile-stop!))

(define (spr)
  (spl)
  (profile-reset!))

(define (spp flag thunk)
  (if (not flag)
      (thunk)
    (dynamic-wind
      spb
      thunk
      spe)))

(define (spw . rest)
  (let ((name (if (null? rest) "report.spr" (car rest))))
    (spl)
    (write-profile-report name)))


;;;
;;;; Application
;;;


(define (jedi)
  (lj)
  (l 'jazz.platform.literals)
  (l 'jazz.system.boot))

(define (j)
  (jedi))


(define (r . rest)
  (if (not (null? rest))
      (rl (car rest)))
  (jazz.process.Process.Process.run-loop (jazz.dialect.language.get-process)))
