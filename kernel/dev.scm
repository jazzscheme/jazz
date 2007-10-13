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


(define (lt)
  (l 'test))


(define (lex module-name)
  (ef module-name)
  (load "x"))


(define (llex)
  (lex jazz.dialect.language))


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
        (pretty-print (case kind
                        ((module) (jazz.expand-module (car rest) (cdr rest)))
                        ((library) (jazz.expand-library rest)))
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


(define (ccw)
  (cflag 'jazz.platform.cairo.cairo-win32 "-IC:/jazz/dev/jazz/include/cairo" "-LC:/jazz/dev/jazz/lib/cairo -lcairo"))

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


(define (bcairo)
  (ccw))


(define (bffi)
  (ld)
  (jazz.compile-library-with-flags 'jazz.platform.windows.WinUser cc-flags: "-D UNICODE" ld-flags: "-mwindows -lUser32" force?: #t)
  (jazz.compile-library-with-flags 'jazz.platform.cairo.cairo-win32 cc-flags: "-IC:/jazz/dev/jazz/bin/cairo/include" ld-flags: "-LC:/jazz/dev/jazz/bin/cairo/lib -lcairo" force?: #t))


(define (ball)
  (bjazz)
  (bui))


(define (bui)
  (bwin)
  (ccw))


(define (cj module-name)
  (ld)
  (lj)
  (let* ((file (jazz.determine-module-filename module-name))
         (jazz (jazz.determine-module-source file))
         (jscmfile (string-append file ".jscm"))
         (jscm (string-append "../../" jscmfile))
         (bin (jazz.determine-module-binary file))
         (jazztime (time->seconds (file-last-modification-time jazz)))
         (jscmtime (and (file-exists? jscm) (time->seconds (file-last-modification-time jscm))))
         (bintime (and bin (file-exists? bin) (time->seconds (file-last-modification-time bin)))))
    (if (or (not jscmtime) (> jazztime jscmtime)
            (not bintime) (> jscmtime bintime))
        (begin
          (expand-to-file module-name jscm)
          (parameterize ((current-readtable jazz.jazz-readtable))
            (jazz.compile-filename-with-flags jscmfile source?: #t))))))


(define Lang
  '(jazz.dialect.language))

(define Util
  '(jazz.utilities))

(define View
  '(jazz.library.component.Component
    jazz.ui.dialog
    jazz.ui.view
    jazz.ui.window
    jazz.ui.layout.Figure
    jazz.ui.view.Drawing
    jazz.ui.view.View
    jazz.ui.view.Scrollbar
    jazz.ui.window.Window
    jazz.ui.window.View-Player
    jazz.ui.graphic.Cairo-Win32-Surface
    jazz.ui.image.Image
    jazz.ui.image.Portfolio
    jazz.platform))

(define Expl
  '(jazz.library.utility.Cell
    jazz.ui.text.Paragraph
    jazz.ui.text.Text-View
    jazz.ui.text.Text-Explorer
    jazz.ui.text.Code-Explorer
    jazz.jazz.text.Lisp-Explorer
    jazz.jazz.text.Jazz-Explorer))

(define Text
  '(jazz.ui.graphic.Font
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
    jazz.ui.outline.Outline-View
    jazz.ui.text.Text-View
    jazz.ui.text.Code-Text-View
    jazz.jazz.text.Lisp-Text-View
    jazz.jazz.text.Jazz-Text-View
    jazz.ui.text.Text-Colorizer))

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


(define (bjz)
  (blang)
  (butil)
  (bui)
  (bexpl)
  (btext)
  (btree)
  (bappl)
  (bjml))

(define (blang)
  (for-each cj Lang))

(define (butil)
  (for-each cj Util))

(define (bview)
  (for-each cj View))

(define (bexpl)
  (for-each cj Expl))

(define (btext)
  (for-each cj Text))

(define (btree)
  (for-each cj Tree))

(define (bappl)
  (for-each cj Appl))

(define (bjml)
  (for-each cj JML))


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


(define (oc obj)
  (jazz.object-content (if (integer? obj) (jazz.serial-number->object obj) obj)))


;;;
;;;; Profile
;;;


(define (lsp)
  (load "contrib/statprof/statprof"))


(define (spb)
  (profile-start!))

(define (spe)
  (profile-stop!))

(define (spw name)
  (write-profile-report name))


;;;
;;;; Application
;;;


(define (j)
  (lj)
  (l 'jazz.platform.literals)
  (l 'test.boot))


(define (lp . rest)
  (if (not (null? rest))
      (rl (car rest)))
  (jazz.process.Process.Process.run-loop (jazz.dialect.language.get-process)))
