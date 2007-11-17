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
  (l 'core.dev))


(define (lj)
  (ll)
  (l 'jazz))


(define (la)
  (lj)
  (l 'jazz.literals)
  (l 'jazz.platform.literals))


(define (lt)
  (ll)
  (rl 'test))


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
  (let ((source (jazz.determine-module-source (jazz.determine-module-filename library-name))))
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


(define (cm module-name cc-options ld-options)
  (jazz.compile-module module-name cc-options: cc-options ld-options: ld-options))


;; patch around a gambit bug
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
            (jazz.compile-filename jscm source: jscmfile))))))


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


(define (bjazz)
  (bcore)
  (bmodule 'scheme.dialect)
  (bmodule 'jazz.dialect)
  (cj 'jazz.dialect.language))


(cond-expand
  (windows
    (define (bcairo)
      (cm 'jazz.platform.cairo "-IC:/jazz/dev/jazz/include/cairo" "-LC:/jazz/dev/jazz/lib/cairo -lcairo")))
  (x11
    (define (bcairo)
      (cm 'jazz.platform.cairo "-I/opt/local/include/cairo -I/opt/local/include" "-L/opt/local/lib -lcairo"))))


(define (bfreetype)
  (cm 'jazz.platform.freetype "-I/opt/local/include -I/opt/local/include/freetype2" "-L/opt/local/lib -lfreetype")
  (cm 'jazz.platform.cairo.cairo-freetype "-I/opt/local/include -I/opt/local/include/freetype2 -I/opt/local/include/cairo" "-L/opt/local/lib -lcairo"))


(define (blogfont)
  (cm 'jazz.platform.cairo.cairo-logfont "-IC:/jazz/dev/jazz/include/cairo" "-LC:/jazz/dev/jazz/lib/cairo -lcairo"))


(cond-expand
  (freetype
    (define (bfont)
      (bfreetype)))
  (logfont
    (define (bfont)
      (blogfont))))


(define (bwindows)
  (ld)
  (cm 'jazz.platform.windows.WinDef "-D UNICODE" "-mwindows")
  (cm 'jazz.platform.windows.WinTypes "-D UNICODE" "-mwindows")
  (cm 'jazz.platform.windows.WinBase "-D UNICODE" "-mwindows")
  (cm 'jazz.platform.windows.WinNT  "-D UNICODE" "-mwindows")
  (cm 'jazz.platform.windows.WinKernel "-D UNICODE" "-mwindows")
  (cm 'jazz.platform.windows.WinGDI "-D UNICODE" "-mwindows")
  (cm 'jazz.platform.windows.WinUser "-D UNICODE" "-mwindows -lUser32")
  (cm 'jazz.platform.windows.WinShell "-D UNICODE" "-mwindows")
  (cm 'jazz.platform.windows.WinCtrl "-D UNICODE" "-mwindows")
  (cm 'jazz.platform.windows.WinDlg "-D UNICODE" "-mwindows")
  (cm 'jazz.platform.cairo.cairo-windows "-IC:/jazz/dev/jazz/include/cairo" "-LC:/jazz/dev/jazz/lib/cairo -lcairo")
  (cj 'jazz.system.platform.windows))
  

(define (bx11)
  (ld) 
  (cm 'jazz.platform.x11 "-I/usr/X11R6/include" "-L/usr/X11R6/lib -lX11")
  (cm 'jazz.platform.freetype "-I/opt/local/include -I/opt/local/include/freetype2" "-L/opt/local/lib -lfreetype")
  (cm 'jazz.platform.cairo.cairo-x11 "-I/opt/local/include/cairo" "-L/opt/local/lib -lcairo")
  (cm 'jazz.platform.cairo.cairo-freetype "-I/opt/local/include/cairo -I/opt/local/include -I/opt/local/include/freetype2" "-L/opt/local/lib -lcairo")
  (cm 'jazz.platform.cairo "-I/opt/local/include/cairo" "-L/opt/local/lib -lcairo"))


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


(define Lang
  '(jazz.dialect.language))

(define Util
  '(jazz.utilities
    jazz.io))

(define Comp
  '(jazz.library.component.Component))

(define View
  '(jazz.ui.dialog
    jazz.ui.view
    jazz.ui.layout.Figure
    jazz.ui.view.Drawing
    jazz.ui.view.View
    jazz.ui.view.Scrollbar
    jazz.ui.window
    jazz.ui.window.platform.windows
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
    jazz.application.Application
    jazz.ui.ide.IDE
    jedi.application.Jedi
    jazz.application.platform.windows))

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


(define (bjedi)
  (la)
  (build)
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
  (for-each cln Lang)
  (for-each cln Util)
  (for-each cln view)
  (for-each cln Expl)
  (for-each cln Text)
  (for-each cln Tree)
  (for-each cln Appl)
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
        (load "../../contrib/statprof/statprof")
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
