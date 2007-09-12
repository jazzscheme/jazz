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


(define (L)
  (load "t"))


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


(define (bd)
  (jazz.boot-kernel)
  (jazz.load-module 'dev))


(define (bl)
  (jazz.boot-kernel)
  (jazz.load-module 'core.library))


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


;;;
;;;; Parse
;;;


(define (for-each-module proc)
  (call-with-input-file "Modules.fusion"
    (lambda (reader)
      (let loop ((expr (read reader)))
        (if (not (eof-object? expr))
            (begin
              (proc expr)
              (loop (read reader))))))))


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
  (jazz.set-catalog-entry library-name #f)
  (jazz.locate-toplevel-declaration library-name))


;;;
;;;; Declarations
;;;


(define-macro (d library-name)
  `(pretty-print-library-name ',library-name))


(define (pretty-print-library-name library-name)
  (l core.class)
  (pretty-print-library (locate-fresh library-name)))


(define (pretty-print-library library)
  (let loop ((declaration library)
             (level 0))
    (display (make-string (* level 2) #\space))
    (display declaration)
    (display " ")
    (display (jazz.get-declaration-name declaration))
    (newline)
    (for-each (lambda (subdecl)
                (loop subdecl (+ level 1)))
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


(define-macro (cflag module-name c-flags ld-flags)
  `(jazz.compile-library-with-flags ',module-name #f ,c-flags ,ld-flags))


(define (bwindows)
  (for-each (lambda (x) (jazz.compile-library-with-flags (car x) #f (cadr x) (caddr x)))
            compiled-libs-windows))


(define (blinux)
  (for-each (lambda (x) (jazz.compile-library-with-flags (car x) #f (cadr x) (caddr x)))
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


(define (ell)
  (expand-to-file 'jazz.dialect.language "_language.scm"))

(define (cll)
  (bd)
  (parameterize ((current-readtable jazz.jazz-readtable))
    (jazz.compile-filename-with-flags "_language")))


;;;
;;;; Build
;;;


(define (bkernel)
  (bd)
  (jazz.build-kernel))


(define-macro (bmodule module-name)
  `(begin
     (bd)
     (bl)
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
  (cflag jazz.platform.windows.WinShell "-D UNICODE" "-mwindows"))


(define (ball)
  (bj)
  (bwin)
  (ccw))


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
  (time (let ((filename "products/org.jazz/lib/jazz/ui/text/Text-View.fusion"))
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


(define (lp)
  (jazz.process.Process.Process.run-loop (jazz.dialect.language.get-process)))


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
