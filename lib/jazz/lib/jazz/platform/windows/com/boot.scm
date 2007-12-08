;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; COM dev boot
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
;;;  The Initial Developer of the Original Code is Stephane Le Cornec.
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


;;;
;;;; gambcext.scm
;;;


;(define copass 's)
;(let ((boot "c:/dev/aaScratch/com/boot.scm"))
;  (if (file-exists? boot)
;      (load boot)
;    (begin
;      (write (string-append "WARNING: cannot find " boot))
;      (newline))))


;;;
;;;; settings
;;;


(define load-path "c:/dev/aaScratch/com/")


(define copass-s-files
  (list "type-table" "coutils" "cotype" "costruct" "coexternal"))


(define copass-s-runtime-files
  (list "coruntime"))


(define copass-dual-runtime-files
  (list "codebug" "untyped" "types" "cotypes" "costructs" "IUnknown" "DAO"))


(define copass-c-runtime-files
  (list "coruntime-c"))


(define c-lib-mapping
  '(("coruntime-c" "ole32") ("cotypes" "oleaut32") ("costructs" "oleaut32")))



;;;
;;;; load
;;;


(define (c-load file-name)
  (debug-load (string-append file-name "-c.o1")))


(define (s-load file-name)
  (set! copass 's)
  (debug-load file-name))


(define (debug-load file)
  (current-directory load-path)
  (display (string-append "--load " file))
  (newline)
  (load file))


;;;
;;;; compile
;;;


(define (debug-compile base-name suffix)
  (current-directory load-path)
  (display (string-append "--compile " base-name))
  (newline)
  (let ((libs (let ((found (assoc base-name c-lib-mapping)))
                (if found (cdr found) '())))
        (linklist (list (string-append base-name suffix)))
        (cfile (string-append base-name suffix ".c"))
        (linkfile (string-append base-name suffix ".o1.c"))
        (outfile (string-append base-name suffix ".o1")))
    (if (not (dynamic-wind (lambda () (set! copass 'c))
                           (lambda () (compile-file-to-c base-name '() cfile))
                           (lambda () (set! copass 's))))
        (error "compile failed" base-name)
      (begin
        (link-flat linklist linkfile)
        (shell-command (apply string-append
                              "gcc -shared -D___DYNAMIC -DUNICODE "
                              cfile " "
                              linkfile " -o "
                              outfile
                              (map (lambda (lib) (string-append " -l" lib))
                                   libs)))))))


;;;
;;;; expand
;;;


(define (c-expand file-name)
  (current-directory load-path)
  (dynamic-wind (lambda () (set! copass 'c))
                (lambda () (compile-file file-name '(expansion)))
                (lambda () (set! copass 's))))


(define (s-expand file-name)
  (current-directory load-path)
  (lambda () (set! copass 's))
  (compile-file file-name '(expansion)))


;;;
;;;; dev
;;;


(define (cobuild-load)
  (current-directory load-path)
  (for-each debug-load copass-s-files)
  (for-each debug-load copass-s-runtime-files)
  (for-each (lambda (file)
              (debug-compile file "-c")
              (c-load file)
              (s-load file))
            copass-dual-runtime-files)
  (for-each (lambda (file) (debug-compile file "")) copass-c-runtime-files)
  (for-each debug-load copass-c-runtime-files))


(define (co-load)
  (current-directory load-path)
  (for-each debug-load copass-s-files)
  (for-each debug-load copass-s-runtime-files)
  (for-each (lambda (file)
              (c-load file)
              (s-load file))
            copass-dual-runtime-files)
  (for-each debug-load copass-c-runtime-files))


(define (co-reload)
  (current-directory load-path)
  (for-each debug-load copass-s-files)
  (for-each debug-load copass-s-runtime-files)
  (for-each s-load copass-dual-runtime-files))


(define (clean)
  (current-directory load-path)
  (shell-command "del *.c")
  (shell-command "del *.o*"))


;;;
;;;; settings
;;;


(define (aaz)
  (clean)
  (load "coutils")
  (load "cotype"))


(define (aa0)
  (cobuild-load))


(define (aal)
  (co-load))


(define (aa)
  (co-reload))


(define (aa-c)
  (set! copass 'c))


(define (aa-s)
  (set! copass 's))


(define (aat)
  (load "cotest"))


(define aaf "costructs")
(define (aas n)
  (case n
    ((1) (debug-compile aaf "-c"))
    ((2) (c-load aaf))
    ((3) (s-load aaf))))
