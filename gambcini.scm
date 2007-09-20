;;;
;;;===============
;;;  Jazz System
;;;===============
;;;
;;;; gambitcini
;;;


;;(include "~/gambit/lib/header.scm")


(generate-proper-tail-calls #f)
(display-environment-set! #t)


;;;
;;;; Boot
;;;


(define jazz.boot-kernel
  (let ((loaded? #f))
    (lambda ()
      (if (not loaded?)
          (begin
            (load "boot")
            (jazz.load-kernel)
            (set! loaded? #t))))))


(define (b)
  (jazz.boot-kernel))


(define (bl)
  (jazz.boot-kernel)
  (jazz.load-module 'core.library))


(define (bd)
  (bl)
  (jazz.load-module 'dev))


;;;
;;;; Load
;;;


(define-macro (l module-name #!optional (reload? #t))
  `(begin
     (bl)
     (,(if reload? 'jazz.reload-module 'jazz.load-module) ',module-name)))


(define-macro (rl module-name)
  `(begin
     (bl)
     (jazz.reload-module ',module-name)))


(define-macro (ul module-name)
  `(jazz.unload-module ',module-name))


(define-macro (rln name)
  `(begin
     (bl)
     (jazz.reload-module (jazz.get-autoload ',name))))


(define-macro (lex module-name)
  `(begin
     (ef ,module-name)
     (load "x")))


(define (ll)
  (bl)
  (jazz.load-module 'jazz))


(define (lj)
  (bl)
  (jazz.load-module 'jazz)
  (jazz.load-module 'jazz.literals)
  (jazz.load-module 'jazz.platform.literals))


(define (lt)
  (l test))


(define (tffi)
  (rs 'test.cffi)
  (l test.cffi)
  (l test))


;;;
;;;; Parse
;;;


(define (for-each-module proc)
  (call-with-input-file "Modules.fusion"
    (lambda (reader)
      (let iter ((expr (read reader)))
        (if (not (eof-object? expr))
            (begin
              (proc expr)
              (iter (read reader))))))))


;;;
;;;; Expand
;;;


(define-macro (em module-name)
  `(expand-module ',module-name))


(define (expand-module module-name)
  (let ((source (jazz.determine-module-source (jazz.module-filename module-name))))
    (let ((form (jazz.read-toplevel-form source #f)))
      (let ((name (cadr form))
            (rest (cddr form)))
        (pretty-print (jazz.expand-module name rest) (current-output-port))))))


(define-macro (e library-name)
  `(begin
     (bl)
     (expand ',library-name)))


(define-macro (ef library-name)
  `(begin
     (bl)
     (expand-to-file ',library-name)))


(define-macro (ee library-name)
  `(begin
     (bd)
     (jazz.compile-library-expansion ',library-name)))


(define (expand library-name)
  (expand-to-port library-name (current-output-port)))


(define (expand-to-file library-name . rest)
  (let ((filename (if (null? rest) "x.scm" (car rest))))
    (call-with-output-file filename
      (lambda (port)
        (expand-to-port library-name port)))))


(define (expand-to-port library-name port)
  (let ((source (jazz.determine-module-source (jazz.module-filename library-name))))
    (let ((form (jazz.read-toplevel-form source #f)))
      (let ((kind (car form))
            (rest (cdr form)))
        (pretty-print (case kind
                        ((module) (jazz.expand-module (car rest) (cdr rest)))
                        ((library) (jazz.walk-library rest)))
                      port)))))


(define (et)
  (e test))

(define (ea)
  (e test.a))

(define (effi)
  (e test.cffi))


(define (est)
  (e scheme.test))


(define (ejt)
  (e jazz.test))


;;;
;;;; Lookup
;;;


(define-macro (ld library-name)
  `(jazz.locate-toplevel-declaration ',library-name))


(define-macro (lk library-name name #!optional (external? #t))
  `(jazz.lookup-declaration (jazz.locate-toplevel-declaration ',library-name) ',name ,external?))


(define-macro (lk2 library-name name access)
  `(jazz.lookup-declaration2 (jazz.locate-toplevel-declaration ',library-name) ',name ,access))


(define (locate-fresh library-name)
  (rs library-name)
  (jazz.locate-toplevel-declaration library-name))


(define (rs library-name)
  (jazz.set-catalog-entry library-name #f))


;;;
;;;; Declarations
;;;


(define-macro (d library-name)
  `(pretty-print-library-name ',library-name))


(define (pretty-print-library-name library-name)
  (l core.class)
  (pretty-print-library (locate-fresh library-name)))


(define (pretty-print-library library)
  (let iter ((declaration library)
             (level 0))
    (display (make-string (* level 2) #\space))
    (display declaration)
    (display " ")
    (display (jazz.get-declaration-name declaration))
    (newline)
    (for-each (lambda (subdecl)
                (iter subdecl (+ level 1)))
              (%%get-declaration-children declaration))))


(define (dl library)
  (pretty-print-library library))


(define (dfa)
  (d jazz.test.a))


;;;
;;;; Compile
;;;


(define compiled-libs '((core.base.syntax.readtable "" "") (core.class.runtime.output-hook "" "")))
(define compiled-libs-windows (append compiled-libs
                                      '((jazz.platform.cairo "-Ic:/mingw/include/cairo/" "-Lc:/mingw/lib -lcairo"))))
(define compiled-libs-linux (append compiled-libs 
                                  '((jazz.platform.x11 "" "-lX11") 
                                    (jazz.platform.cairo "-I/usr/local/include/cairo -I/usr/include/freetype2" "-L/usr/local/lib -lcairo")
                                    (jazz.platform.freetype "-I/usr/include/freetype2" "-lfreetype"))))


(define compiled-libs-linux
  (append compiled-libs 
          '((jazz.platform.x11 "" "-lX11") 
            (jazz.platform.cairo "-I/usr/local/include/cairo -I/usr/include/freetype2" "-L/usr/local/lib -lcairo")
            (jazz.platform.freetype "-I/usr/include/freetype2" "-lfreetype"))))


(define-macro (c library-name)
  `(jazz.compile-library-with-flags ',library-name))


(define-macro (cflag module-name cc-flags ld-flags)
  `(jazz.compile-library-with-flags ',module-name cc-flags: ,cc-flags ld-flags: ,ld-flags))


(define (bwindows)
  (for-each (lambda (x) (jazz.compile-library-with-flags (car x) cc-flags: (cadr x) ld-flags: (caddr x)))
            compiled-libs-windows))


(define (blinux)
  (for-each (lambda (x) (jazz.compile-library-with-flags (car x) cc-flags: (cadr x) ld-flags: (caddr x)))
            compiled-libs-linux))


(define (cx)
  (cflag jazz.platform.x11 "" "-lX11"))


(define (ccl)
  (cflag jazz.platform.cairo "-I/usr/local/include/cairo -I/usr/include/freetype2" "-L/usr/local/lib -lcairo"))


(define (ccw)
  (cflag jazz.platform.cairo.cairo-win32 "-IC://jazz//dev//jazz//bin//cairo//include" "-LC://jazz//dev//jazz//bin//cairo//lib -lcairo"))

(define (cf)
  (cflag jazz.platform.freetype "-I/usr/include/freetype2" "-lfreetype"))


(define (cl)
  (bd)
  (c jazz.dialect.language))


(define (ct)
  (bd)
  (jazz.compile-library-with-flags 'test force?: #t))

(define (cffi)
  (bd)
  (jazz.compile-library-with-flags 'test.cffi force?: #t))


;;;
;;;; Build
;;;


(define (bkernel)
  (bd)
  (jazz.build-kernel))


(define-macro (bmodule module-name)
  `(begin
     (bd)
     (jazz.build-module ',module-name)))


(define (bj)
  (ll)
  (bkernel)
  (bmodule core.base)
  (bmodule core.class)
  (bmodule core.foundation)
  (bmodule core.generic)
  (bmodule core.library)
  (bmodule scheme.dialect)
  (bmodule jazz.dialect))


(define (bwin)
  (bd)
  (cflag jazz.platform.windows.WinDef "-D UNICODE" "-mwindows")
  (cflag jazz.platform.windows.WinTypes "-D UNICODE" "-mwindows")
  (cflag jazz.platform.windows.WinBase "-D UNICODE" "-mwindows")
  (cflag jazz.platform.windows.WinNT  "-D UNICODE" "-mwindows")
  (cflag jazz.platform.windows.WinKernel "-D UNICODE" "-mwindows")
  (cflag jazz.platform.windows.WinGDI "-D UNICODE" "-mwindows")
  (cflag jazz.platform.windows.WinUser "-D UNICODE" "-mwindows -lUser32")
  (cflag jazz.platform.windows.WinShell "-D UNICODE" "-mwindows")
  (cflag jazz.platform.windows.WinCtrl "-D UNICODE" "-mwindows")
  (cflag jazz.platform.windows.WinDlg "-D UNICODE" "-mwindows"))


(define (ffi)
  (bd)
  (jazz.compile-library-with-flags 'jazz.platform.windows.WinUser cc-flags: "-D UNICODE" ld-flags: "-mwindows -lUser32" force?: #t)
  (jazz.compile-library-with-flags 'jazz.platform.cairo.cairo-win32 cc-flags: "-IC://jazz//dev//jazz//bin//cairo//include" ld-flags: "-LC://jazz//dev//jazz//bin//cairo//lib -lcairo" force?: #t))


(define (ball)
  (bj)
  (bwin)
  (ccw))


(define (cj module-name)
  (bd)
  (lj)
  (let* ((file (jazz.module-filename module-name))
         (jazz (jazz.determine-module-source file))
         (jscm (string-append file ".jscm"))
         (jazztime (time->seconds (file-last-modification-time jazz)))
         (jscmtime (and (file-exists? jscm) (time->seconds (file-last-modification-time jscm)))))
    (if (or (not jscmtime) (> jazztime jscmtime))
        (begin
          (expand-to-file module-name jscm)
          (parameterize ((current-readtable jazz.jazz-readtable))
            (jazz.compile-filename-with-flags jscm source?: #t))))))


(define Lang
  '(jazz.dialect.language))

(define UI
  '(jazz.library.component.Component
    jazz.ui.layout.Figure
    jazz.ui.view.Drawing
    jazz.ui.view.View
    jazz.ui.graphic.Font
    jazz.process.Process
    jazz.application.Application
    jazz.ui.graphic.Cairo-Win32-Surface
    jazz.library.element.Node
    jazz.library.exemplar.Exemplar
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
    jazz.jazz.text.Lisp-Explorer
    jazz.jazz.text.Jazz-Explorer
    jazz.ui.window.Window
    jazz.ui.window.View-Player
    jazz.jml
    jazz.jml.parser.JML-Parser))


(define (bjz)
  (blang)
  (bui))

(define (blang)
  (for-each cj Lang))

(define (bui)
  (for-each cj UI))


;;;
;;;; Clean
;;;


(define (cln module-name)
  (bd)
  (let* ((file (jazz.module-filename module-name))
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
  (cui))

(define (clang)
  (for-each cln Lang))

(define (cui)
  (for-each cln UI))


;;;
;;;; Macro
;;;


(define-macro (egm form)
  `(jazz.expand-global-macro ',form))


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
;;;; Time
;;;


(define (ltext)
  (lj)
  (l jazz.ui.text.Text-View))


(define (ttl)
  (lj)
  (time (ltext)))


(define (ttp)
  (lj)
  (profile (lambda () (ltext))))


(define (ttr)
  (time (let ((filename "packages/org.jazz/lib/jazz/ui/text/Text-View.fusion"))
          (jazz.with-extension-reader (jazz.filename-extension filename)
            (lambda ()
              (call-with-input-file filename
                (lambda (port)
                  (read port)
                  #t)))))))


(define (tte)
  (ef jazz.ui.text.Text-View))

(define (ttre)
  (time (let ((filename "x.scm"))
          (call-with-input-file filename
            (lambda (port)
              (read port)
              #t)))))

(define (ttc)
  (time (load "x")))


(define (trc)
  (time (call-with-input-file "c.fusion" (lambda (port) (read port) #t))))

(define (trx)
  (time (call-with-input-file "x.scm" (lambda (port) (read port) #t))))

(define (tlx)
  (time (load "x.scm")))


(define (bpf)
  (bd)
  (c test.performance.common)
  (c test.performance.a)
  (c test.performance.b))

(define (lpf)
  (bl)
  (jazz.load-module 'test.performance))

(define (pf)
  (lpf)
  (l test.performance.time))


;;;
;;;; Profile
;;;


(define (lsp)
  (load "statprof"))


(define (spb)
  (profile-start!))

(define (spe)
  (profile-stop!))

(define (spw name)
  (write-profile-report name))


;;;
;;;; Eval
;;;


(define-macro (E expr)
  (lj)
  `(library test jazz
     (import (dev)
             (jazz.jml)
             (jazz.library)
             (jazz.literals)
             (jazz.platform)
             (jazz.platform.windows)
             (jazz.ui)
             (jazz.ui.view)
             (jazz.ui.window)
             (jazz.utilities)
             (jedi))
     (set! ? ,expr)
     (debug ?)))


(define-macro (O . rest)
  `(call-with-output-file "output.scm"
     (lambda (p)
       ,@rest)))


;;;
;;;; Validation
;;;


(define (val)
  (lj)
  (jazz.load-module 'jazz.validate.validate.Validation-Suite)
  (let ((suite (jazz.new jazz.validate.validate.Validation-Suite.Validation-Suite)))
    (jazz.validate.validate.Validation-Suite.Validation-Suite.validate suite)))


;;;
;;;; Application
;;;


(define (sv obj name)
  (jazz.slot-value obj name))


(define (t . rest)
  (apply test.test rest))


(define (b)
  (lj)
  (jazz.load-module 'jazz.platform.literals)
  (l test.boot))


(define-macro (lp . rest)
  `(begin
     ,@(if (null? rest)
           '()
         `((l ,(car rest))))
     (jazz.process.Process.Process.run-loop (jazz.dialect.language.get-process))))


(define (tl)
  (jazz.platform.get-toplevel))


(define (gv? c)
  (jazz.library.component.Component.Component.get-visible? c))


(define (sv? c f)
  (jazz.library.component.Component.Component.set-visible? c f))


(define (gsz w)
  (jazz.ui.layout.Figure.Figure.get-size w))


(define (ssz w sz)
  (jazz.ui.layout.Figure.Figure.set-size w sz))


(define (gwsz w)
  (jazz.ui.window.Window.Window.get-window-size w))


(define (close obj)
  (jazz.dialect.language.Object.close obj))


(define (ctl)
  (close (tl)))


(define (stl)
  (sv? (tl) #t))


(define (nd w h)
  (jazz.new jazz.library.utility.Dimension.Dimension w h))


(define (rstl)
  (ssz (tl) (nd 800 600)))
