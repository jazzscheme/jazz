;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Module Runtime
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
             (extended-bindings)))
  (else))


;;;
;;;; Error
;;;


(define (jazz.kernel-error . rest)
  (apply error rest))


;;;
;;;; String
;;;


(define (jazz.string-find-reversed str c)
  (let iter ((n (%%fx- (%%string-length str) 1)))
    (cond ((%%fx< n 0)
           #f)
          ((%%char=? (%%string-ref str n) c)
           n)
          (else
           (iter (%%fx- n 1))))))


(define (jazz.string-replace str old new)
  (let ((cpy (string-copy str)))
    (let iter ((n (%%fx- (%%string-length cpy) 1)))
      (if (%%fx>= n 0)
          (begin
            (if (%%eqv? (%%string-ref cpy n) old)
                (%%string-set! cpy n new))
            (iter (%%fx- n 1)))))
    cpy))


;;;
;;;; File
;;;


(cond-expand
  (gambit
    (define (jazz.file-exists? filename)
      (file-exists? filename))
    
    (define (jazz.directory-exists? filename)
      (file-exists? filename))
    
    (define (jazz.file-last-modification-time filename)
      (time->seconds (file-last-modification-time filename))))
  
  (else))


;;;
;;;; Filename
;;;


(define (jazz.filename-dir filename)
  (let ((pos (jazz.string-find-reversed filename #\/)))
    (if (%%not pos)
        #f
      (%%substring filename 0 (%%fx+ pos 1)))))


(define (jazz.filename-name filename)
  (let ((pos (jazz.string-find-reversed filename #\/)))
    (if (%%not pos)
        filename
      (%%substring filename (%%fx+ pos 1) (%%string-length filename)))))


(define (jazz.filename-extension filename)
  (let ((pos (jazz.string-find-reversed filename #\.)))
    (if pos
        (%%substring filename (%%fx+ pos 1) (%%string-length filename))
      #f)))


;;;
;;;; Load
;;;


(cond-expand
  (gambit
    (define jazz.walk-for
      (make-parameter #f))
    
    
    #; ;; new
    (define (jazz.with-path-src/bin src proc)
      (let ((bin (jazz.path-find-binary src)))
        (let ((srctime (jazz.path-modification-time src))
              (binpck (and bin (jazz.find-bin-package bin))))
          (let ((latest 
                  (if (or (%%not binpck)
                          (%%not (%%string=? (jazz.package-digest binpck))))
                      'src
                    'bin)))
            (proc src bin latest)))))
    
    
    (define (jazz.with-path-src/bin src proc)
      (let ((bin (jazz.path-find-binary src)))
        (let ((srctime (jazz.path-modification-time src))
              (bintime (and bin (jazz.path-modification-time bin))))
          (let ((latest 
                  (if (or (%%not bintime) (> srctime bintime))
                      'src
                    'bin)))
            (proc src bin latest)))))
    
    
    (define (jazz.path-modification-time path)
      (let ((filename (jazz.path-filename path)))
        (if (%%not (jazz.file-exists? filename))
            #f
          (jazz.file-last-modification-time filename))))
    
    
    (define (jazz.load-src src)
      (parameterize ((jazz.walk-for 'interpret))
        (jazz.load-path src)))
    
    
    (define (jazz.load-bin bin quiet?)
      (parameterize ((jazz.walk-for 'interpret))
        (jazz.load-path bin quiet?)))
    
    
    (define (jazz.load-source-path src)
      (jazz.with-path-src/bin src
        (lambda (src bin latest)
          (case latest
            ((src)
             (jazz.with-extension-reader (jazz.path-extension src)
               (lambda ()
                 (jazz.load-src src))))
            ((bin)
             (let ((quiet? (or (%%not src) (%%string=? (jazz.path-extension src) "jazz"))))
               (jazz.load-bin bin quiet?)))))))
    
    
    (define jazz.bin-package
      "_obj/")

    
    (define (jazz.path-bin-dir path)
      (let ((dir (jazz.filename-dir (jazz.path-name path))))
        (if (not dir)
            jazz.bin-package
          (%%string-append jazz.bin-package dir))))

    
    (define (jazz.path-find-binary path)
      (let ((name (jazz.path-name path)))
        (define (try n)
          (%%string-append "o" (number->string n)))
        
        (define (exists? extension)
          (jazz.file-exists? (%%string-append jazz.bin-package name "." extension)))
        
        (let ((o1 (try 1)))
          (if (%%not (exists? o1))
              #f
            (let iter ((next 2)
                       (last-extension o1))
              (let ((next-extension (try next)))
                (if (exists? next-extension)
                    (iter (%%fx+ next 1) next-extension)
                  (jazz.make-path jazz.bin-package name last-extension))))))))
    
    
    (define (jazz.find-bin-package bin)
      (let ((pck (jazz.make-path (jazz.path-package bin) (jazz.path-name bin) "jpck")))
        (let ((pck-filename (jazz.path-filename pck)))
          (if (jazz.file-exists? pck-filename)
              (call-with-input-file pck-filename
                read)
            #f)))))
  (else))


;;;
;;;; Packages
;;;


(define jazz.Packages
  '("../../packages/org.jazz/lib/"
    "../../packages/org.jazz.website/lib/"
    "../../packages/org.jedi/lib/"
    "../../packages/contrib/"
    "../../packages/srfi/"
    "../../packages/snow/"
    "~/jazz/lib/"))


(define (jazz.register-package package)
  (set! jazz.Packages (cons package jazz.Packages)))


(define (jazz.find-module-src module-name . rest)
  (let ((error? (if (%%null? rest) #t (%%car rest))))
    (let ((name (jazz.string-replace (%%symbol->string module-name) #\. #\/)))
      (let ((base (jazz.filename-name name)))
        (let iter ((scan jazz.Packages))
          (if (%%null? scan)
              (if error?
                  (jazz.kernel-error "Unable to find module:" module-name)
                #f)
            (let ((package (%%car scan)))
              (define (try name)
                (define (try-extension extension)
                  (if (jazz.file-exists? (%%string-append package name "." extension))
                      (jazz.make-path package name extension)
                    #f))
                
                (or (try-extension "scm")
                    (try-extension "jazz")))
              
              (or (if (jazz.directory-exists? (%%string-append package name))
                      (try (%%string-append name "/_" base))
                    (try name))
                  (iter (%%cdr scan))))))))))


;;;
;;;; Request
;;;


(define jazz.requested-module-name
  (make-parameter #f))


;;;
;;;; States
;;;


(define jazz.Unloaded-State
  #f)

(define jazz.Loading-State
  '(loading))

(define jazz.Loaded-State
  '(loaded))


;;;
;;;; Environment
;;;


(define jazz.Environment
  (%%make-table test: eq?))


(define (jazz.get-environment)
  jazz.Environment)


(define (jazz.get-environment-module name)
  (%%table-ref jazz.Environment name jazz.Unloaded-State))


(define (jazz.set-environment-module name module)
  (%%table-set! jazz.Environment name module))


;;;
;;;; Load
;;;


(define (jazz.load-module module-name)
  (let ((module-state (jazz.get-environment-module module-name)))
    (cond ((%%eq? module-state jazz.Unloaded-State)
           (dynamic-wind
             (lambda ()
               (jazz.set-environment-module module-name jazz.Loading-State))
             (lambda ()
               (parameterize ((jazz.requested-module-name module-name))
                 (jazz.load-source-path (jazz.find-module-src module-name))
                 (block-tail-call)))
             (lambda ()
               (if (%%eq? (jazz.get-environment-module module-name) jazz.Loading-State)
                   (jazz.set-environment-module module-name jazz.Unloaded-State)))))
          ((%%eq? module-state jazz.Loading-State)
           (jazz.kernel-error "Circular loading of module:" module-name)))))


(define (jazz.module-loaded? module-name)
  (let ((module (jazz.get-environment-module module-name)))
    (and module (%%neq? module ':loading))))


(define (jazz.module-loaded module-name)
  (jazz.set-environment-module module-name jazz.Loaded-State))


(define (jazz.unload-module module-name)
  (jazz.set-environment-module module-name jazz.Unloaded-State))


(define (jazz.reload-module module-name)
  (jazz.unload-module module-name)
  (jazz.load-module module-name))


;;;
;;;; Reader
;;;


(define jazz.Extension-Readers
  (%%make-table test: equal?))


(define (jazz.get-extension-reader extension)
  (%%table-ref jazz.Extension-Readers extension #f))


(define (jazz.with-extension-reader extension thunk)
  (let ((reader-info (jazz.get-extension-reader extension)))
    (if reader-info
        (let ((dialect-name (%%car reader-info))
              (readtable-getter (%%cdr reader-info)))
          (jazz.load-module dialect-name)
          (parameterize ((current-readtable (readtable-getter)))
            (thunk)))
      (thunk))))


(define (jazz.register-reader-extension dialect-name readtable-getter extension)
  (%%table-set! jazz.Extension-Readers extension (%%cons dialect-name readtable-getter)))


(define (jazz.register-reader-extensions dialect-name readtable-getter extensions)
  (for-each (lambda (extension)
              (jazz.register-reader-extension dialect-name readtable-getter extension))
            extensions))
