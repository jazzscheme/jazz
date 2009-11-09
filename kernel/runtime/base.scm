;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Kernel Base
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


;;;
;;;; Version
;;;


(define (jazz.make-version number gambit-version gambit-stamp rebuild recompile description)
  (vector 'version number gambit-version gambit-stamp rebuild recompile description))

(define (jazz.version-number version)
  (vector-ref version 1))

(define (jazz.version-gambit-version version)
  (vector-ref version 2))

(define (jazz.version-gambit-stamp version)
  (vector-ref version 3))

(define (jazz.version-rebuild version)
  (vector-ref version 4))

(define (jazz.version-recompile version)
  (vector-ref version 5))

(define (jazz.version-description version)
  (vector-ref version 6))


(define (jazz.new-version
          #!key
          (version #f)
          (gambit-version #f)
          (gambit-stamp #f)
          (rebuild #f)
          (recompile #f)
          (description #f))
  (jazz.make-version
    version
    gambit-version
    gambit-stamp
    rebuild
    recompile
    description))


(define (jazz.split-version number)
  (let ((str (number->string number)))
    (let ((len (string-length str)))
      (let ((major (string->number (substring str 0 (- len 5))))
            (minor (string->number (substring str (- len 5) (- len 3))))
            (revision (string->number (substring str (- len 3) len))))
        (values major minor revision)))))


(define (jazz.present-version number)
  (receive (major minor revision) (jazz.split-version number)
    (string-append (number->string major)
                   "."
                   (number->string minor)
                   "."
                   (number->string revision)
                   " beta")))


;;;
;;;; Versions
;;;


(define jazz.source-versions-file
  #f)

(define jazz.source-versions
  #f)

(define jazz.source-version-number
  #f)

(define jazz.gambit-version
  #f)

(define jazz.gambit-stamp
  #f)


(define jazz.load-source-versions
  (let ((loaded? #f))
    (lambda ()
      (define (determine-source-versions-file)
        (or jazz.source-versions-file
            (and jazz.kernel-source (string-append jazz.kernel-source "kernel/versions"))))
      
      (define (load-versions)
        (let ((file (determine-source-versions-file)))
          (if (and file (file-exists? file))
              (call-with-input-file (list path: file eol-encoding: 'cr-lf)
                (lambda (input)
                  (define (read-version input)
                    (let ((list (read input)))
                      (if (eof-object? list)
                          list
                        (apply jazz.new-version list))))
                  (set! jazz.source-versions (read-all input read-version))
                  (set! jazz.source-version-number (jazz.version-number (car jazz.source-versions))))))))
      
      (define (setup-gambit-version/stamp)
        (if jazz.source-versions
            (let iter ((source-versions jazz.source-versions))
              (if (not (null? source-versions))
                  (let ((source-version (car source-versions)))
                    (let ((gambit-version (jazz.version-gambit-version source-version))
                          (gambit-stamp (jazz.version-gambit-stamp source-version)))
                      (if gambit-version
                          (begin
                            (set! jazz.gambit-version gambit-version)
                            (set! jazz.gambit-stamp gambit-stamp))
                        (iter (cdr source-versions)))))))))
      
      (if (not loaded?)
          (begin
            (load-versions)
            (setup-gambit-version/stamp)
            (set! loaded? #t))))))


(define (jazz.get-source-versions)
  (jazz.load-source-versions)
  jazz.source-versions)


(define (jazz.get-source-version-number)
  (jazz.load-source-versions)
  jazz.source-version-number)


(define (jazz.get-gambit-version)
  (jazz.load-source-versions)
  jazz.gambit-version)


(define (jazz.get-gambit-stamp)
  (jazz.load-source-versions)
  jazz.gambit-stamp)


(define (jazz.gambit-uptodate? system-version system-stamp)
  (let ((gambit-version (jazz.get-gambit-version))
        (gambit-stamp (jazz.get-gambit-stamp)))
    (if gambit-version
        (if (not gambit-stamp)
            (>= system-version gambit-version)
          (or (> system-version gambit-version)
              (>= system-stamp gambit-stamp)))
      #t)))


;;;
;;;; Configuration
;;;


(define (jazz.print-configuration name system platform windowing safety optimize? debug-environments? debug-location? debug-source? interpret-kernel? source-access? destination output)
  (define first?
    #t)
  
  (define (print-property property value)
    (if first?
        (set! first? #f)
      (display " " output))
    (display property output)
    (display " " output)
    (write value output))
  
  (display "(" output)
  (if name
      (print-property name: name))
  (print-property system: system)
  (print-property platform: platform)
  (if windowing
      (print-property windowing: windowing))
  (print-property safety: safety)
  (if (not optimize?)
      (print-property optimize?: optimize?))
  (if (not debug-environments?)
      (print-property debug-environments?: debug-environments?))
  (if (not debug-location?)
      (print-property debug-location?: debug-location?))
  (if debug-source?
      (print-property debug-source?: debug-source?))
  (if interpret-kernel?
      (print-property interpret-kernel?: interpret-kernel?))
  (if (not (eqv? source-access? #t))
      (print-property source-access?: source-access?))
  (if destination
      (print-property destination: destination))
  (display ")" output)
  (newline output))


;;;
;;;; Link
;;;


(define (jazz.parse-link link)
  (define link-options
    '(("executables" executables)
      ("libraries" libraries)
      ("modules" modules)
      ("images" executables libraries)
      ("all" executables libraries modules)))
  
  (define (invalid option)
    (jazz.error "Invalid link option: {a}" option))
  
  (if (symbol? link)
      (let ((lst (jazz.split-string (symbol->string link) #\/))
            (result '()))
        (for-each (lambda (option)
                    (let ((pair (assoc option link-options)))
                      (if pair
                          (let ((options (cdr pair)))
                            (for-each (lambda (option)
                                        (if (not (member option result))
                                            (set! result (cons option result))))
                                      options))
                        (invalid option))))
                  lst)
        result)
    (invalid link)))


;;;
;;;; Destination
;;;


(define jazz.user-build-directory
  #f)


(define (jazz.get-user-build-directory)
  (define (user-build-directory)
    (let ((dir "~/jazz_user/build/"))
      (jazz.create-directories dir)
      (jazz.pathname-normalize dir)))
  
  (or jazz.user-build-directory
      (let ((dir (user-build-directory)))
        (set! jazz.user-build-directory dir)
        dir)))


(define (jazz.parse-destination dest proc)
  (if (not dest)
      (proc #f #f)
    (let ((pos (jazz.string-find dest #\:))
          (len (string-length dest)))
      (if (not pos)
          (proc #f dest)
        (proc (if (= pos 0)
                  #f
                (string->symbol (substring dest 0 pos)))
              (if (= pos (- len 1))
                  #f
                (substring dest (+ pos 1) len)))))))


(define (jazz.destination-directory name destination source)
  (jazz.parse-destination (cond (destination destination)
                                (name (jazz.format ":{a}" name))
                                (else "bin:"))
    (lambda (alias dirname)
      (case (or alias 'user)
        ((user) (string-append (jazz.get-user-build-directory) dirname "/"))
        ((jazz) (string-append source "build/" dirname "/"))
        ((bin) (string-append source "bin/"))))))


;;;
;;;; Pathname
;;;


(define jazz.pathname-type
  file-type)

(define jazz.pathname-exists?
  file-exists?)


(define (jazz.pathname-standardize path)
  (jazz.string-replace path #\\ #\/))


(define (jazz.pathname-normalize path #!optional (error? #t))
  (if (not (jazz.pathname-exists? path))
      (if error?
          (jazz.error "No such directory: {s}" path)
        #f)
    (let ((len (string-length path)))
      (let ((dir? (jazz.string-ends-with? path "/")))
        (let ((normalized (path-normalize (if dir? (substring path 0 (- len 1)) path))))
          (let ((standardized (jazz.pathname-standardize normalized)))
            (if (and dir? (not (jazz.string-ends-with? standardized "/")))
                (string-append standardized "/")
              standardized)))))))


(define (jazz.create-directory dir #!key (feedback #f))
  (if (not (file-exists? dir))
      (begin
        (if feedback
            (feedback "; creating {a}..." dir))
        (create-directory dir))))


(define (jazz.create-directories dir #!key (feedback #f))
  (let ((path (reverse (jazz.split-string dir #\/))))
    (let iter ((scan (if (equal? (car path) "") (cdr path) path)))
      (if (not (null? scan))
          (begin
            (iter (cdr scan))
            (let ((subdir (jazz.join-strings (reverse scan) "/")))
              (if (not (file-exists? subdir))
                  (jazz.create-directory subdir feedback: feedback))))))))


(define (jazz.directory-content directory)
  (directory-files (if (string? directory)
                       (list path: directory ignore-hidden: 'dot-and-dot-dot)
                     directory)))


(define (jazz.directory-files directory)
  (jazz.collect-if (lambda (name)
                     (eq? (jazz.pathname-type (string-append directory name)) 'regular))
                   (jazz.directory-content directory)))


(define (jazz.directory-directories directory)
  (jazz.collect-if (lambda (name)
                     (eq? (jazz.pathname-type (string-append directory name)) 'directory))
                   (jazz.directory-content directory)))


;;;
;;;; Command Line
;;;


(define (jazz.option? arg)
  (and (< 0 (string-length arg))
       (or (char=? (string-ref arg 0) #\-)
           (char=? (string-ref arg 0) #\/))))


(define (jazz.convert-option arg)
  (substring arg 1 (string-length arg)))


(define (jazz.option=? arg option)
  (and (jazz.option? arg)
       (equal? (jazz.convert-option arg) option)))


(define (jazz.get-option name options)
  (let ((pair (assoc name options)))
    (if pair
        (cdr pair)
      #f)))


(define (jazz.split-command-line arguments options-with-no-args options-with-args missing-argument-for-option cont)
  (let loop ((args arguments)
             (rev-options '()))
    (if (and (pair? args)
             (jazz.option? (car args)))
        (let ((opt (jazz.convert-option (car args)))
              (rest (cdr args)))
          (cond ((member opt options-with-no-args)
                 (loop rest
                       (cons (cons opt #t) rev-options)))
                ((member opt options-with-args)
                 (if (pair? rest)
                     (loop (cdr rest)
                           (cons (cons opt (car rest)) rev-options))
                   (begin
                     (missing-argument-for-option opt)
                     (loop rest rev-options))))
                (else
                 (cont (reverse rev-options) args))))
      (cont (reverse rev-options) args))))


;;;
;;;; Process
;;;


(define (jazz.call-process path arguments #!optional (directory #f))
  (let ((port (open-process
                (list
                  path: path
                  arguments: arguments
                  directory: (or directory (current-directory))
                  stdin-redirection: #f
                  stdout-redirection: #f
                  stderr-redirection: #f))))
    (let ((code (process-status port)))
      (if (not (= code 0))
          (jazz.error "failed")))))


;;;
;;;; Feedback
;;;


(define (jazz.feedback fmt-string . rest)
  (display (apply jazz.format fmt-string rest))
  (newline)
  (force-output))
