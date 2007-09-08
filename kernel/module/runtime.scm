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
             (extended-bindings)
             (not safe)))
  (else))


;;;
;;;; Error
;;;


(define (jazz.kernel-error . rest)
  (apply error rest))


;;;
;;;; File
;;;


(cond-expand
  (gambit
    (define (jazz.file-exists? filename)
      (file-exists? filename))
    
    (define (jazz.directory-exists? filename)
      (file-exists? filename)))
  
  (else))


;;;
;;;; Util
;;;


(define (jazz.find-char-reversed c str)
  (let loop ((n (%%fixnum- (%%string-length str) 1)))
    (cond ((char=? (%%string-ref str n) c)
           n)
          ((%%fixnum> n 0)
           (loop (%%fixnum- n 1)))
          (else
           #f))))


(define (jazz.split-filename filename proc)
  (let ((pos (jazz.find-char-reversed #\/ filename)))
    (if (%%not pos)
        (proc "" filename)
      (proc (%%substring filename 0 (%%fixnum+ pos 1))
            (%%substring filename (%%fixnum+ pos 1) (%%string-length filename))))))


;;;
;;;; Load
;;;


(cond-expand
  (blues
    (define (jazz.load filename)
      (load filename)))
  (chicken
    (define (jazz.load filename)
      (load filename)))
  (gambit
    (define (jazz.load filename)
      (jazz.load-file filename))
    
    
    (define (jazz.load-file filename)
      (jazz.with-load-src/bin filename
        jazz.load-src
        jazz.load-bin))
    
    
    (define (jazz.with-file-src/bin filename proc)
      (let ((src (jazz.determine-module-source filename))
            (bin (jazz.determine-module-binary filename)))
        (let ((srctime (and src (time->seconds (file-last-modification-time src))))
              (bintime (and bin (time->seconds (file-last-modification-time bin)))))
          (proc src srctime bin bintime))))
    
    
    (define (jazz.with-load-src/bin filename src-proc bin-proc)
      (jazz.with-file-src/bin filename
        (lambda (src srctime bin bintime)
          (cond ((and (%%not srctime) (%%not bintime))
                 (error "Unable to find file:" filename))
                ((and srctime (or (%%not bintime) (> srctime bintime)))
                 (src-proc src))
                (else
                 (bin-proc bin))))))
    
    
    (define jazz.load-indent-level
      (make-parameter 0))
    
    
    (define (jazz.load-src src)
      (jazz.load-verbose src)
      (parameterize ((jazz.load-indent-level (+ (jazz.load-indent-level) 2)))
        (jazz.load-filename src))
      (if jazz.done-verbose?
          (jazz.load-done-verbose src)))
    
    
    (define (jazz.load-bin bin)
      (jazz.load-verbose bin)
      (parameterize ((jazz.load-indent-level (+ (jazz.load-indent-level) 2)))
        (jazz.load-filename bin))
      (if jazz.done-verbose?
          (jazz.load-done-verbose bin)))
    
    
    (define (jazz.load-verbose filename)
      (display (make-string (jazz.load-indent-level) #\space))
      (display "; loading ")
      (display filename)
      (display " ...")
      (newline))
    
    
    (define (jazz.load-done-verbose filename)
      (display (make-string (jazz.load-indent-level) #\space))
      (display "; done ")
      (display " ...")
      (newline))
    
    
    (define (jazz.require-module-source filename)
      (or (jazz.determine-module-source filename)
          (error "Unable to find: " filename)))
    
    
    (define (jazz.determine-module-source filename)
      (let ((try
              (lambda (ext)
                (let ((path (%%string-append filename "." ext)))
                  (if (file-exists? path)
                      path
                    #f)))))
        (or (try "scm")
            (try "jazz")
            (try "fusion"))))
    
    
    (define (jazz.determine-module-bindir filename)
      (jazz.split-filename filename
        (lambda (dirname name)
          (cond (jazz.Use-Bin-Directory?
                 (%%string-append "bin/_obj/" jazz.build-suffix "/" dirname))
                (jazz.Use-Build-Suffix?
                 (%%string-append dirname "_bin/" jazz.build-suffix "/"))
                (else
                 dirname)))))
    
    
    (define (jazz.determine-module-binary filename)
      (jazz.split-filename filename
        (lambda (dirname name)
          (let ((try
                  (lambda (n)
                    (cond (jazz.Use-Bin-Directory?
                           (%%string-append "bin/_obj/" jazz.build-suffix "/" dirname name ".o" (number->string n)))
                          (jazz.Use-Build-Suffix?
                           (%%string-append dirname "_bin/" jazz.build-suffix "/" name ".o" (number->string n)))
                          (else
                           (%%string-append dirname name ".o" (number->string n)))))))
            (let ((o1 (try 1)))
              (if (%%not (file-exists? o1))
                  #f
                (let loop ((next 2)
                           (last-path o1))
                     (let ((next-path (try next)))
                       (if (file-exists? next-path)
                           (loop (%%fixnum+ next 1) next-path)
                         last-path))))))))))
  (else
    (define (jazz.load filename)
      (load filename))))


;;;
;;;; Manifest
;;;


;; Quick draft. Code is not really correct as it uses the parent folder
;; to determine in which prefix a file is located which can be not correct
(define jazz.Library-Prefixes
  '("products/org.jazz/lib/"
    "products/org.jedi/lib/"))


(define (jazz.module-filename module-name)
  (let ((path (jazz.string-replace (%%symbol->string module-name) #\. #\/)))
    (jazz.split-filename path
      (lambda (dir name)
        (let loop ((scan jazz.Library-Prefixes))
          (if (null? scan)
              (jazz.kernel-error "Unable to find module:" module-name)
            (let ((prefix (car scan)))
              (let ((prefixed-path (%%string-append prefix path))
                    (prefixed-dir (%%string-append prefix dir)))
                (cond ((jazz.directory-exists? prefixed-path)
                       (%%string-append prefixed-path "/_" name))
                      ((and (not (equal? dir "")) (jazz.directory-exists? prefixed-dir))
                       prefixed-path)
                      (else
                       (loop (cdr scan))))))))))))


(define (jazz.string-replace str old new)
  (let ((cpy (string-copy str)))
    (let loop ((n (%%fixnum- (%%string-length cpy) 1)))
      (if (%%fixnum>= n 0)
          (begin
            (if (%%eqv? (%%string-ref cpy n) old)
                (%%string-set! cpy n new))
            (loop (%%fixnum- n 1)))))
    cpy))


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
  (%%new-hashtable ':eq?))


(define (jazz.get-environment)
  jazz.Environment)


(define (jazz.get-environment-module name)
  (%%hashtable-ref jazz.Environment name jazz.Unloaded-State))


(define (jazz.set-environment-module name module)
  (%%hashtable-set! jazz.Environment name module))


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
               (jazz.load-module-file (jazz.module-filename module-name)))
             (lambda ()
               (if (%%eq? (jazz.get-environment-module module-name) jazz.Loading-State)
                   (jazz.set-environment-module module-name jazz.Unloaded-State)))))
          ((%%eq? module-state jazz.Loading-State)
           (jazz.kernel-error "Circular loading of module:" module-name)))))


(define (jazz.load-module-file filename)
  (jazz.with-load-src/bin filename
    (lambda (src)
      (jazz.with-extension-reader (jazz.filename-extension src)
        (lambda ()
          (jazz.load-src src))))
    jazz.load-bin))


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


(define (jazz.filename-extension filename)
  (let ((pos (jazz.find-char-reversed #\. filename)))
    (if pos
        (%%substring filename (%%fixnum+ pos 1) (%%string-length filename))
      #f)))


;;;
;;;; Read
;;;


(define jazz.Extension-Readers
  (%%new-hashtable ':equal?))


(define (jazz.get-extension-reader extension)
  (%%hashtable-ref jazz.Extension-Readers extension #f))


(define (jazz.with-extension-reader extension thunk)
  (let ((reader-info (jazz.get-extension-reader extension)))
    (if reader-info
        (let ((dialect-name (%%car reader-info))
              (readtable-getter (%%cdr reader-info)))
          (jazz.load-module dialect-name)
          (let ((old-readtable ##main-readtable))
            (dynamic-wind
              (lambda ()
                (set! c#**main-readtable (readtable-getter)))
              (lambda ()
                ;; unfortunately due to a Gambit bug this code only works when calling load
                ;; and not when calling compile-file hence the manual setting of ##main-readtable
                (parameterize ((current-readtable (readtable-getter)))
                  (thunk)))
              (lambda ()
                (set! c#**main-readtable old-readtable)))))
      (thunk))))


(define (jazz.register-reader-extension dialect-name readtable-getter extension)
  (%%hashtable-set! jazz.Extension-Readers extension (%%cons dialect-name readtable-getter)))


(define (jazz.register-reader-extensions dialect-name readtable-getter extensions)
  (for-each (lambda (extension)
              (jazz.register-reader-extension dialect-name readtable-getter extension))
            extensions))


(set! jazz.load-src
      (let ((base-load jazz.load-src))
        (lambda (filename)
          (jazz.with-extension-reader (jazz.filename-extension filename)
            (lambda ()
              (base-load filename))))))
