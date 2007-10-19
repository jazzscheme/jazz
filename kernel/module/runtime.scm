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
  (let iter ((n (%%fx- (%%string-length str) 1)))
    (cond ((char=? (%%string-ref str n) c)
           n)
          ((%%fx> n 0)
           (iter (%%fx- n 1)))
          (else
           #f))))


(define (jazz.split-filename filename proc)
  (let ((pos (jazz.find-char-reversed #\/ filename)))
    (if (%%not pos)
        (proc "" filename)
      (proc (%%substring filename 0 (%%fx+ pos 1))
            (%%substring filename (%%fx+ pos 1) (%%string-length filename))))))


;;;
;;;; Load
;;;


(cond-expand
  (gambit
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
    
    
    (define jazz.walk-for
      (make-parameter #f))
    
    
    (define (jazz.load-src src)
      (jazz.with-verbose jazz.load-verbose? "loading" (%%substring src 6 (%%string-length src))
        (lambda ()
          (parameterize ((jazz.walk-for 'interpret))
            (jazz.load-filename src)))))
    
    
    (define (jazz.load-bin bin)
      (jazz.with-verbose jazz.load-verbose? "loading" (%%substring bin 5 (%%string-length bin))
        (lambda ()
          (parameterize ((jazz.walk-for 'interpret))
            (jazz.load-filename bin)))))
    
    
    (define (jazz.require-module-source filename)
      (or (jazz.determine-module-source filename)
          (error "Unable to find: " filename)))
    
    
    (define (jazz.determine-module-source filename)
      (let ((try
              (lambda (ext)
                (let ((path (%%string-append "../../" filename "." ext)))
                  (if (file-exists? path)
                      path
                    #f)))))
        (or (try "scm")
            (try "jazz")
            (try "fusion")
            (try "jscm"))))
    
    
    (define (jazz.determine-module-bindir filename)
      (jazz.split-filename filename
        (lambda (dirname name)
          (%%string-append "_obj/" dirname))))
    
    
    (define (jazz.determine-module-binary filename)
      (let ((try
              (lambda (n)
                (%%string-append "_obj/" filename ".o" (number->string n)))))
        (let ((o1 (try 1)))
          (if (%%not (file-exists? o1))
              #f
            (let iter ((next 2)
                       (last-path o1))
                 (let ((next-path (try next)))
                   (if (file-exists? next-path)
                       (iter (%%fx+ next 1) next-path)
                     last-path))))))))
  (else))


;;;
;;;; Manifest
;;;


(define jazz.PackageDirs
  '("packages/org.jazz/lib/"
    "packages/org.jedi/lib/"))


;; Quick draft. The code is not really correct as it uses the parent folder
;; to determine in which prefix a file is located which can potentially be not
;; correct. Also, library roots of packages should be determined automatically
(define (jazz.determine-module-filename module-name)
  (let ((path (jazz.string-replace (%%symbol->string module-name) #\. #\/)))
    (jazz.split-filename path
      (lambda (dir name)
        (let iter ((scan jazz.PackageDirs))
          (if (%%null? scan)
              (jazz.kernel-error "Unable to find module:" module-name)
            (let ((prefix (%%car scan)))
              (let ((prefixed-path (%%string-append prefix path))
                    (prefixed-dir (%%string-append prefix dir)))
                (cond ((jazz.directory-exists? (%%string-append "../../" prefixed-path))
                       (%%string-append prefixed-path "/_" name))
                      ((and (%%not (%%equal? dir "")) (jazz.directory-exists? (%%string-append "../../" prefixed-dir)))
                       prefixed-path)
                      (else
                       (iter (%%cdr scan))))))))))))


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
  (%%make-hashtable eq?))


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
               (parameterize ((jazz.requested-module-name module-name))
                 (jazz.load-source-file (jazz.determine-module-filename module-name))))
             (lambda ()
               (if (%%eq? (jazz.get-environment-module module-name) jazz.Loading-State)
                   (jazz.set-environment-module module-name jazz.Unloaded-State)))))
          ((%%eq? module-state jazz.Loading-State)
           (jazz.kernel-error "Circular loading of module:" module-name)))))


(define (jazz.load-source-file filename)
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
        (%%substring filename (%%fx+ pos 1) (%%string-length filename))
      #f)))


;;;
;;;; Read
;;;


(define jazz.Extension-Readers
  (%%make-hashtable equal?))


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
