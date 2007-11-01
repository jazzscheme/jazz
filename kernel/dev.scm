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
;;;; Test
;;;


(define (s specifier)
  (jazz.type->specifier (jazz.walk-specifier #f #f #f '() specifier)))


(define (sbuggy)
  (s '<fx<fx/<fx^fx:fx>/fx>^fx:fx>))


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
  (l 'dev))


(define (lj)
  (ll)
  (l 'jazz))


(define (la)
  (lj)
  (l 'jazz.literals)
  (l 'jazz.platform.literals))


(define (lit)
  (lj)
  (l 'jazz.literals))


(define (lt)
  (ll)
  (rl 'test))


;; jazz.dialect.language
;; jazz.ui.text.Paragraph
;; jazz.ui.text.Text-View
;; jazz.ui.text.Text-Explorer
;; jazz.ui.text.Code-Explorer
;; jazz.jazz.text.Lisp-Explorer
;; jazz.jazz.text.Scheme-Explorer
;; jazz.jazz.text.Jazz-Explorer
(define (rll)
  (rl 'jazz.dialect.language))

(define (rlp)
  (rl 'jazz.ui.text.Paragraph))

(define (rlt)
  (rl 'jazz.ui.text.Text-View))

(define (rlte)
  (rl 'jazz.ui.text.Text-Explorer))

(define (rlce)
  (rl 'jazz.ui.text.Code-Explorer))

(define (rlle)
  (rl 'jazz.jazz.text.Lisp-Explorer))

(define (rlse)
  (rl 'jazz.jazz.text.Scheme-Explorer))

(define (rlje)
  (rl 'jazz.jazz.text.Jazz-Explorer))


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


;;;
;;;; Walk
;;;


(define (walk library-name)
  (let ((source (jazz.determine-module-source (jazz.determine-module-filename library-name))))
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
  (let ((source (jazz.determine-module-source (jazz.determine-module-filename module-name))))
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
  (jazz.compile-library-with-options library-name (list 'expansion)))


(define (eg library-name)
  (ld)
  (jazz.compile-library-with-options library-name (list 'gvm)))


(define (expand library-name)
  (expand-to-port library-name (current-output-port)))


(define (expand-to-file library-name . rest)
  (let ((filename (if (null? rest) "x.scm" (car rest))))
    (call-with-output-file filename
      (lambda (port)
        (expand-to-port library-name port)))))


(define (expand-to-port library-name port)
  (let ((source (jazz.determine-module-source (jazz.determine-module-filename library-name))))
    (let ((form (jazz.read-toplevel-form source #f)))
      (let ((kind (car form))
            (rest (cdr form)))
        (pretty-print (parameterize ((jazz.requested-module-name library-name))
                        (case kind
                          ((module) (jazz.expand-module (car rest) (cdr rest)))
                          ((library) (jazz.expand-library rest))))
                      port)))))


(define (et)
  (e 'test))


;;;
;;;; Compile
;;;


(define compiled-libs
  '((core.base.syntax.readtable "" "") (core.class.runtime.output-hook "" "")))

(define compiled-libs-windows
  (append compiled-libs
          '((jazz.platform.cairo "-Ic:/mingw/include/cairo/" "-Lc:/mingw/lib -lcairo"))))

(define compiled-libs-linux
  (append compiled-libs 
          '((jazz.platform.x11 "" "-lX11") 
            (jazz.platform.cairo "-I/usr/local/include/cairo -I/usr/include/freetype2" "-L/usr/local/lib -lcairo")
            (jazz.platform.freetype "-I/usr/include/freetype2" "-lfreetype"))))


(define (c library-name)
  (jazz.compile-library-with-flags library-name))


(define (cflag module-name cc-flags ld-flags)
  (jazz.compile-library-with-flags module-name cc-flags: cc-flags ld-flags: ld-flags))


(define (bwindows)
  (for-each (lambda (x) (jazz.compile-library-with-flags (car x) cc-flags: (cadr x) ld-flags: (caddr x)))
            compiled-libs-windows))


(define (blinux)
  (for-each (lambda (x) (jazz.compile-library-with-flags (car x) cc-flags: (cadr x) ld-flags: (caddr x)))
            compiled-libs-linux))


(define (cx)
  (cflag 'jazz.platform.x11 "" "-lX11"))


(define (cf)
  (cflag 'jazz.platform.freetype "-I/usr/include/freetype2" "-lfreetype"))


(define (cl)
  (ld)
  (c 'jazz.dialect.language))


(define (ct)
  (ld)
  (jazz.compile-library-with-flags 'test force?: #t))

(define (cffi)
  (ld)
  (jazz.compile-library-with-flags 'test.cffi force?: #t))


;;;
;;;; Build
;;;


(define (bkernel)
  (ld)
  (jazz.build-kernel))


(define (bmodule module-name)
  (ld)
  (jazz.build-module module-name))


(define (bjazz)
  (ll)
  (bkernel)
  (bmodule 'core.base)
  (bmodule 'core.class)
  (bmodule 'core.foundation)
  (bmodule 'core.generic)
  (bmodule 'core.library)
  (bmodule 'scheme.dialect)
  (bmodule 'jazz.dialect))


(define (bwin)
  (ld)
  (cflag 'jazz.platform.windows.WinDef "-D UNICODE" "-mwindows")
  (cflag 'jazz.platform.windows.WinTypes "-D UNICODE" "-mwindows")
  (cflag 'jazz.platform.windows.WinBase "-D UNICODE" "-mwindows")
  (cflag 'jazz.platform.windows.WinNT  "-D UNICODE" "-mwindows")
  (cflag 'jazz.platform.windows.WinKernel "-D UNICODE" "-mwindows")
  (cflag 'jazz.platform.windows.WinGDI "-D UNICODE" "-mwindows")
  (cflag 'jazz.platform.windows.WinUser "-D UNICODE" "-mwindows -lUser32")
  (cflag 'jazz.platform.windows.WinShell "-D UNICODE" "-mwindows")
  (cflag 'jazz.platform.windows.WinCtrl "-D UNICODE" "-mwindows")
  (cflag 'jazz.platform.windows.WinDlg "-D UNICODE" "-mwindows"))


(define (bcairo-windows-freetype)
  (cflag 'jazz.platform.freetype "-IC:/jazz/dev/jazz/include/freetype2" "-LC:/jazz/dev/jazz/lib/freetype -lfreetype")
  (cflag 'jazz.platform.cairo.cairo-windows "-IC:/jazz/dev/jazz/include/cairo" "-LC:/jazz/dev/jazz/lib/cairo -lcairo")
  (cflag 'jazz.platform.cairo.cairo-freetype "-IC:/jazz/dev/jazz/include/cairo" "-LC:/jazz/dev/jazz/lib/cairo -lcairo")
  (cflag 'jazz.platform.cairo "-IC:/jazz/dev/jazz/include/cairo" "-LC:/jazz/dev/jazz/lib/cairo -lcairo"))


(define (bcairo-windows-logfont)
  (cflag 'jazz.platform.cairo.cairo-windows "-IC:/jazz/dev/jazz/include/cairo" "-LC:/jazz/dev/jazz/lib/cairo -lcairo")
  (cflag 'jazz.platform.cairo.cairo-logfont "-IC:/jazz/dev/jazz/include/cairo" "-LC:/jazz/dev/jazz/lib/cairo -lcairo")
  (cflag 'jazz.platform.cairo "-IC:/jazz/dev/jazz/include/cairo" "-LC:/jazz/dev/jazz/lib/cairo -lcairo"))
  
  
(define (bwin/freetype)
  (bwin)
  (bcairo-windows-freetype))


(define (bwin/logfont)
  (bwin)
  (bcairo-windows-logfont))


(cond-expand
  (freetype
    (define (bui)
      (bwin/freetype)))
  (logfont
    (define (bui)
      (bwin/logfont))))


(define (bx)
  (ld) 
  (cflag 'jazz.platform.x11 "-I/usr/X11R6/include" "-L/usr/X11R6/lib -lX11")
  (cflag 'jazz.platform.freetype "-I/opt/local/include -I/opt/local/include/freetype2" "-L/opt/local/lib -lfreetype")
  (cflag 'jazz.platform.cairo.cairo-x11 "-I/opt/local/include/cairo" "-L/opt/local/lib -lcairo")
  (cflag 'jazz.platform.cairo.cairo-freetype "-I/opt/local/include/cairo -I/opt/local/include -I/opt/local/include/freetype2" "-L/opt/local/lib -lcairo")
  (cflag 'jazz.platform.cairo "-I/opt/local/include/cairo" "-L/opt/local/lib -lcairo"))


(define (bffi)
  (ld)
  (jazz.compile-library-with-flags 'jazz.platform.windows.WinUser cc-flags: "-D UNICODE" ld-flags: "-mwindows -lUser32" force?: #t)
  (jazz.compile-library-with-flags 'jazz.platform.cairo.cairo-win32 cc-flags: "-IC:/jazz/dev/jazz/bin/cairo/include" ld-flags: "-LC:/jazz/dev/jazz/bin/cairo/lib -lcairo" force?: #t))


(define (ball)
  (bjazz)
  (bwin/logfont))


(define (ballx)
  (bjazz)
  (bx))


(define (cj module-name)
  (ld)
  (lj)
  (let* ((file (jazz.determine-module-filename module-name))
         (jazz (jazz.determine-module-source file))
         (jscm (string-append file ".jscm"))
         (jscmfile (string-append "_obj/" jscm))
         (jscmdir (jazz.split-filename jscmfile (lambda (dir name) dir)))
         (bin (jazz.determine-module-binary file))
         (jazztime (time->seconds (file-last-modification-time jazz)))
         (jscmtime (and (file-exists? jscmfile) (time->seconds (file-last-modification-time jscmfile))))
         (bintime (and bin (file-exists? bin) (time->seconds (file-last-modification-time bin)))))
    (if (or (not jscmtime) (> jazztime jscmtime)
            (not bintime) (> jscmtime bintime))
        (begin
          (jazz.create-directories jscmdir)
          (expand-to-file module-name jscmfile)
          (parameterize ((current-readtable jazz.jazz-readtable))
            (jazz.compile-filename-with-flags jscm source: jscmfile))))))


(define Lang
  '(jazz.dialect.language))

(define Util
  '(jazz.utilities
    jazz.library.utility.Range
    jazz.library.utility.shapes
    jazz.io))

(define Comp
  '(jazz.library.component.Component))

(define View
  '(jazz.ui.dialog
    jazz.ui.view
    jazz.ui.window
    jazz.ui.layout.Figure
    jazz.ui.view.Drawing
    jazz.ui.view.View
    jazz.ui.view.Scrollbar
    jazz.ui.window.Window
    jazz.ui.window.View-Player
    jazz.ui.graphic.Surface
    jazz.ui.image.Image
    jazz.ui.image.Portfolio
    jazz.platform))

(define Expl
  '(jazz.ui.text.Text-Explorer
    jazz.ui.text.Code-Explorer
    jazz.jazz.text.Lisp-Explorer
    jazz.jazz.text.Scheme-Explorer
    jazz.jazz.text.Jazz-Explorer))

(define Text
  '(jazz.ui.graphic.font.Font
    jazz.ui.graphic.Font-Metrics
    jazz.library.element.Node
    jazz.library.exemplar.Exemplar
    jazz.library.exemplar.Exemplar-Domain
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
    jazz.jazz.text.Lisp-Text-View
    jazz.jazz.text.Jazz-Text-View
    jazz.ui.text.Text-Colorizer))

(define Cat
  '(jazz.catalog.catalog.Catalog
    jazz.catalog.catalog.Filing-Catalog
    jazz.catalog.catalog.Indexed-Catalog
    jazz.builder.package.Package-Catalog
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
    jazz.application.Application))

(define JML
  '(jazz.jml
    jazz.jml.parser.JML-Parser))


(define (blang)
  (for-each cj Lang))

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


(define (btest)
  (la)
  (ball)
  (blang)
  (bcomp)
  (bexpl)
  (btext))


(define (bjd)
  (la)
  (ball)
  (blang)
  (butil)
  (bcomp)
  (bview)
  (bexpl)
  (btext)
  (bcat)
  (btree)
  (bappl)
  (bjml))


;;;
;;;; Clean
;;;


(define (cln module-name)
  (ld)
  (let* ((file (jazz.determine-module-filename module-name))
         (jscm (string-append file ".jscm"))
         (bin (jazz.determine-module-binary file)))
    (define (delete-if-exists file)
      (if (file-exists? file)
          (begin
            (display "; deleting ")
            (display file)
            (display " ...")
            (newline)
            (delete-file file))))
    
    (delete-if-exists jscm)
    (if bin
        (delete-if-exists bin))))


(define (cjz)
  (clang)
  (cutil)
  (cui)
  (cexpl)
  (ctext)
  (ctree)
  (cappl)
  (cjml))

(define (clang)
  (for-each cln Lang))

(define (cutil)
  (for-each cln Util))

(define (cview)
  (for-each cln view))

(define (cexpl)
  (for-each cln Expl))

(define (ctext)
  (for-each cln Text))

(define (ctree)
  (for-each cln Tree))

(define (cappl)
  (for-each cln Appl))

(define (cjml)
  (for-each cln JML))


;;;
;;;; Debug
;;;


(define (cat)
  (table->list jazz.Catalog))


(define (env)
  (table->list jazz.Environment))


(define (d obj)
  (jazz.object-content (if (integer? obj) (jazz.serial-number->object obj) obj)))


(define (denv env)
  (jazz.debug-annotated-environment env))


;;;
;;;; Profile
;;;


;; this is not correct but it is for now the only way to share
;; usage of statprof between the scheme code and the jazz code


(define jazz.statprof-loaded?
  #f)


(define profile-start! #f) (set! profile-start! #f)
(define profile-stop! #f) (set! profile-stop! #f)
(define write-profile-report #f) (set! write-profile-report #f)


(define (spl)
  (if (not jazz.statprof-loaded?)
      (begin
        (load "../../contrib/statprof/statprof")
        (set! jazz.statprof-loaded? #t))))


(define (spb)
  (spl)
  (profile-start!))

(define (spe)
  (spl)
  (profile-stop!))

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
    (write-sexp-profile-report name)))


;;;
;;;; Application
;;;


(define (j)
  (lj)
  (l 'jazz.platform.literals)
  (l 'test.boot))


(define (tj)
  (set! jazz.run-loop? #f)
  (time (j)))


(define (r . rest)
  (if (not (null? rest))
      (rl (car rest)))
  (jazz.process.Process.Process.run-loop (jazz.dialect.language.get-process)))


;;;
;;;; Tests
;;;


(define (tx)
  (lj)
  (l 'jazz.platform.literals)
  (l 'test.window))
