;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Kernel Runtime
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
;;;  Portions created by the Initial Developer are Copyright (C) 1996-2008
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


(jazz.kernel-declares)


;;;
;;;; Build
;;;


(define jazz.manifest-needs-rebuild? #f)
(set! jazz.manifest-needs-rebuild?
      (lambda rest
        #f))

(define jazz.build-image #f)
(set! jazz.build-image #f)

(define jazz.build-library  #f)
(set! jazz.build-library #f)


;;;
;;;; Feedback
;;;


(define jazz.build-feedback
  jazz.feedback)


;;;
;;;; List
;;;


(define (jazz.listify obj)
  (if (or (%%null? obj) (%%pair? obj))
      obj
    (%%list obj)))


(define (jazz.collect-if predicate lst)
  (declare (proper-tail-calls))
  (let iter ((scan lst))
    (if (%%not (%%null? scan))
        (let ((value (%%car scan)))
          (if (predicate value)
              (%%cons value (iter (%%cdr scan)))
            (iter (%%cdr scan))))
      '())))


(define (jazz.remove item lst)
  (declare (proper-tail-calls))
  (let iter ((scan lst))
    (if (%%not (%%null? scan))
        (let ((value (%%car scan)))
          (if (%%eq? value item)
              (iter (%%cdr scan))
            (%%cons value (iter (%%cdr scan)))))
      '())))


;;;
;;;; String
;;;


(define (jazz.string-find str c #!optional (start 0))
  (declare (proper-tail-calls))
  (let ((len (%%string-length str)))
    (let iter ((n start))
      (cond ((%%fx>= n len)
             #f)
            ((%%char=? (%%string-ref str n) c)
             n)
            (else
             (iter (%%fx+ n 1)))))))


(define (jazz.string-replace str old new)
  (declare (proper-tail-calls))
  (let ((cpy (string-copy str)))
    (let iter ((n (%%fx- (%%string-length cpy) 1)))
      (if (%%fx>= n 0)
          (begin
            (if (%%eqv? (%%string-ref cpy n) old)
                (%%string-set! cpy n new))
            (iter (%%fx- n 1)))))
    cpy))


(define (jazz.string-starts-with? str target)
  (let ((sl (%%string-length str))
        (tl (%%string-length target)))
    (and (%%fx>= sl tl)
         (%%string=? (%%substring str 0 tl) target))))


(define (jazz.string-ends-with? str target)
  (let ((sl (%%string-length str))
        (tl (%%string-length target)))
    (and (%%fx>= sl tl)
         (%%string=? (%%substring str (%%fx- sl tl) sl) target))))


(define (jazz.split-string str separator)
  (declare (proper-tail-calls))
  (let ((lst '())
        (end (%%string-length str)))
    (let iter ((pos (%%fx- end 1)))
      (if (%%fx> pos 0)
          (begin
            (if (%%eqv? (%%string-ref str pos) separator)
                (begin
                  (set! lst (%%cons (%%substring str (%%fx+ pos 1) end) lst))
                  (set! end pos)))
            (iter (%%fx- pos 1))))
        (%%cons (%%substring str 0 end) lst))))


(define (jazz.join-strings strings separator)
  (let ((output (open-output-string)))
    (display (%%car strings) output)
    (for-each (lambda (string)
                (display separator output)
                (display string output))
              (%%cdr strings))
    (get-output-string output)))


;;;
;;;; Symbol
;;;


(define (jazz.split-composite identifier)
  (let ((str (%%symbol->string identifier)))
    (let ((n (jazz.string-find-reversed str #\.)))
      (if (%%not n)
          (values #f identifier)
        (values (%%string->symbol (%%substring str 0 n))
                (%%string->symbol (%%substring str (%%fx+ n 1) (%%string-length str))))))))


;;;
;;;; Pathname
;;;


(define jazz.executable-directory
  #f)


(define (jazz.file-modification-time pathname)
  (time->seconds (file-last-modification-time pathname)))


(define (jazz.copy-file src dst #!key (feedback #f))
  (if (jazz.file-needs-update? src dst)
      (begin
        (if feedback
            (feedback "; copying {a}..." src))
        (if (file-exists? dst)
            (delete-file dst))
        (copy-file src dst))))


(define (jazz.file-needs-update? src dst)
  (or (%%not (file-exists? dst))
      (> (jazz.file-modification-time src)
         (jazz.file-modification-time dst))))


(define (jazz.relativise-directory dir basedir)
  (let ((dir (jazz.pathname-normalize dir))
        (basedir (jazz.pathname-normalize basedir)))
    (let ((len (%%string-length dir))
          (baselen (%%string-length basedir)))
      (if (and (%%fx>= baselen len)
               (%%string=? (%%substring basedir 0 len) dir))
          (let ((suffix (%%substring basedir len baselen))
                (relative-dir ""))
            (let iter ((n (%%fx- (%%string-length suffix) 1)))
              (if (%%fx>= n 0)
                  (begin
                    (if (%%eqv? (%%string-ref suffix n) #\/)
                        (set! relative-dir (%%string-append relative-dir "../")))
                    (iter (%%fx- n 1)))))
            relative-dir)
        dir))))


(define (jazz.quote-gcc-pathname pathname platform)
  (case platform
    ((windows)
     (string-append "\"" pathname "\""))
    (else
     ;; quoting is only necessary on windows as arguments are passed explicitly in unix
     pathname)))


(define (jazz.quote-jazz-gcc-pathname suffix)
  (jazz.quote-gcc-pathname (path-expand (string-append jazz.kernel-source suffix)) jazz.kernel-platform))


(cond-expand
  (gambit
    (define jazz.file-exists?
      file-exists?))
  
  (else))


;;;
;;;; Digest
;;;


(define (jazz.updated-digest-source? digest src-filepath)
  (let ((time (jazz.file-modification-time src-filepath)))
    (if (= time (%%digest-source-time digest))
        #f
      (begin
        (%%digest-source-time-set! digest time)
        (%%digest-source-hash-set! digest (digest-file src-filepath 'sha-1))
        #t))))


;;;
;;;; Manifest
;;;


(define jazz.Manifest-Extension
  "mnf")


(define (jazz.load-manifest filepath)
  (let ((pathname filepath))
    (if (jazz.file-exists? filepath)
        (call-with-input-file (%%list path: filepath eol-encoding: 'cr-lf)
          (lambda (input)
            (let ((form (read input)))
              (let ((name (%%cadr form))
                    (version-form (%%assq 'version (%%cddr form)))
                    (digest-form (%%assq 'digest (%%cddr form)))
                    (references-form (%%assq 'references (%%cddr form))))
                (let (;; test is for backward compatibility and could be removed in the future
                      (version (if version-form (%%cadr version-form) #f))
                      (compile-time-hash (%%cadr digest-form))
                      (source-hash (%%car (%%cddr digest-form)))
                      (source-time (%%cadr (%%cddr digest-form)))
                      (references (if references-form (%%cdr references-form) #f)))
                  (if (%%boolean? source-time)
                      #f
                    (%%make-manifest name version (%%make-digest compile-time-hash source-hash source-time) references)))))))
      #f)))


(define (jazz.save-manifest filepath manifest)
  (let ((name (%%manifest-name manifest))
        (version (%%manifest-version manifest))
        (digest (%%manifest-digest manifest))
        (references (%%manifest-references manifest)))
    (call-with-output-file filepath
      (lambda (output)
        (display "(manifest " output)
        (display name output)
        (newline output)
        (newline output)
        (display "  (version " output)
        (write version output)
        (display ")" output)
        (newline output)
        (display "  (digest " output)
        (write (%%digest-compile-time-hash digest) output)
        (display " " output)
        (write (%%digest-source-hash digest) output)
        (display " " output)
        (write (%%digest-source-time digest) output)
        (if references
            (begin
              (display ")" output)
              (newline output)
              (display "  (references" output)
              (if (%%pair? references)
                  (begin
                    (display " " output)
                    (write (%%car references) output)
                    (for-each (lambda (module-references)
                                (newline output)
                                (display "              " output)
                                (write module-references output))
                              (%%cdr references))))))
        (display "))" output)
        (newline output)))))


(define (jazz.manifest-uptodate? manifest)
  (let ((digest (%%manifest-digest manifest)))
    (%%string=? (%%digest-source-hash digest) (%%digest-compile-time-hash digest))))


(define (jazz.load/create-manifest name manifest-filepath)
  (or (jazz.load-manifest manifest-filepath)
      (%%make-manifest name jazz.kernel-version (%%make-digest "" "" 0) #f)))


(define (jazz.load-updated-manifest name manifest-filepath src-filepath)
  (let ((manifest (jazz.load/create-manifest name manifest-filepath)))
    (let ((digest (%%manifest-digest manifest)))
      (if (jazz.updated-digest-source? digest src-filepath)
          (jazz.save-manifest manifest-filepath manifest))
      manifest)))


(define (jazz.update-manifest-compile-time name manifest-filepath src-filepath updated-references)
  (let ((manifest (jazz.load/create-manifest name manifest-filepath)))
    (%%manifest-version-set! manifest jazz.kernel-version)
    (let ((digest (%%manifest-digest manifest)))
      (jazz.updated-digest-source? digest src-filepath)
      (if updated-references
          (%%manifest-references-set! manifest updated-references))
      (%%digest-compile-time-hash-set! digest (%%digest-source-hash digest))
      (jazz.save-manifest manifest-filepath manifest))))

;;;
;;;; Library
;;;


(define jazz.Library-Extension "l")
(define jazz.Library-Manifest-Extension "lmf")
