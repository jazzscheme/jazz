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
;;;  Portions created by the Initial Developer are Copyright (C) 1996-2018
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


(block kernel.base


;;;
;;;; Exception
;;;


(define (jazz:unimplemented . rest)
  (if (null? rest)
      (error "Unimplemented functionality")
    (error "Unimplemented functionality:" (car rest))))


;;;
;;;; Number
;;;


(define (jazz:naturals from to)
  (let iter ((n (%%fx- to 1)) (lst '()))
       (if (%%fx< n from)
           lst
         (iter (%%fx- n 1) (%%cons n lst)))))


;;;
;;;; List
;;;


(define (jazz:getprop plist target)
  (let iter ((scan plist))
    (cond ((%%null? scan)
           #f)
          ((%%eqv? (%%car scan) target)
           scan)
          (else
           (iter (%%cddr scan))))))


(define (jazz:getf plist target #!optional (not-found #f))
  (let ((pair (jazz:getprop plist target)))
    (if pair
        (%%cadr pair)
      not-found)))


(define (jazz:sort-list smaller l #!key (key #f))
  (declare (proper-tail-calls))
  (declare (optimize-dead-local-variables))
  (declare (inline))
  (declare (inlining-limit 1000))
  (define (apply-key key object)
    (if (not key)
        object
      (key object)))

  (define (merge-sort l)
    (define (merge l1 l2)
      (cond ((%%null? l1) l2)
            ((%%null? l2) l1)
            (else
             (let ((e1 (%%car l1)) (e2 (%%car l2)))
               (if (smaller (apply-key key e1) (apply-key key e2))
                   (%%cons e1 (merge (%%cdr l1) l2))
                 (%%cons e2 (merge l1 (%%cdr l2))))))))
    
    (define (split l)
      (if (or (%%null? l) (%%null? (%%cdr l)))
          l
        (%%cons (%%car l) (split (%%cddr l)))))
    
    (if (or (%%null? l) (%%null? (%%cdr l)))
        l
      (let* ((l1 (merge-sort (split l)))
             (l2 (merge-sort (split (%%cdr l)))))
        (merge l1 l2))))
  
  (merge-sort l))


;;;
;;;; String
;;;


(define (jazz:string-find str c #!optional (start 0))
  (declare (proper-tail-calls))
  (let ((len (%%string-length str)))
    (let iter ((n start))
      (cond ((%%fx>= n len)
             #f)
            ((%%char=? (%%string-ref str n) c)
             n)
            (else
             (iter (%%fx+ n 1)))))))


(define (jazz:string-replace str old new)
  (declare (proper-tail-calls))
  (let ((cpy (string-copy str)))
    (let iter ((n (%%fx- (%%string-length cpy) 1)))
      (if (%%fx>= n 0)
          (begin
            (if (%%eqv? (%%string-ref cpy n) old)
                (%%string-set! cpy n new))
            (iter (%%fx- n 1)))))
    cpy))


(define (jazz:string-starts-with? str target)
  (let ((sl (%%string-length str))
        (tl (%%string-length target)))
    (and (%%fx>= sl tl)
         (%%string=? (%%substring str 0 tl) target))))


(define (jazz:string-search str target)
  (let ((sl (%%string-length str))
        (tl (%%string-length target)))
    (let iter ((n 0))
      (let ((end (%%fx+ n tl)))
        (if (%%fx<= end sl)
            (if (%%string=? (%%substring str n end) target)
                n
              (iter (%%fx+ n 1)))
          #f)))))


(define (jazz:string-contains? str target)
  (if (jazz:string-search str target)
      #t
    #f))


(define (jazz:string-downcase str)
  (list->string (map char-downcase (string->list str))))


;;;
;;;; Symbol
;;;


(define (jazz:global-bound? symbol)
  (and (%%global-var? symbol)
       (%%not (%%unbound? (%%global-var-ref symbol)))))

(define (jazz:global-ref symbol)
  (%%global-var-ref symbol))

(define (jazz:global-set! symbol value)
  (%%global-var-set! symbol value))

(define (jazz:global-unbind! symbol)
  (%%global-var-unbind! symbol))


(define (jazz:global-setting name default)
  (if (jazz:global-bound? name)
      (jazz:global-ref name)
    default))


(define (jazz:break-reference identifier)
  (let ((str (%%symbol->string identifier)))
    (let ((n (jazz:string-find str #\:)))
      (if (%%not n)
          (values identifier #f)
        (values (%%string->symbol (%%substring str 0 n))
                (%%string->symbol (%%substring str (%%fx+ n 1) (%%string-length str))))))))


(define (jazz:split-colon str)
  (let ((n (jazz:string-find str #\:)))
    (if (%%not n)
        (values str #f)
      (values (%%substring str 0 n)
              (%%substring str (%%fx+ n 1) (%%string-length str))))))


(define (jazz:unsafe-locator locator)
  (%%string->symbol (%%string-append "%%" (%%symbol->string locator))))


;;;
;;;; Pathname
;;;


(define (jazz:pathname-type pathname)
  (let ((type (file-type pathname)))
    (if (eq? type 'regular)
        'file
      type)))

(define (jazz:pathname-link? pathname)
  (let ((info (file-info pathname #f)))
    (%%eq? (file-info-type info) 'symbolic-link)))

(define jazz:pathname-exists?
  file-exists?)


(define (jazz:pathname-standardize path)
  (jazz:string-replace path #\\ #\/))


(define (jazz:pathname-normalize path #!optional (error? #t))
  (if (%%not (jazz:pathname-exists? path))
      (if error?
          (jazz:error "No such directory: {s}" path)
        #f)
    (let ((len (%%string-length path)))
      (let ((dir? (jazz:string-ends-with? path "/")))
        (let ((normalized (path-normalize (if dir? (%%substring path 0 (%%fx- len 1)) path))))
          (let ((standardized (jazz:pathname-standardize normalized)))
            (if (and dir? (%%not (jazz:string-ends-with? standardized "/")))
                (string-append standardized "/")
              standardized)))))))


(define (jazz:dirname-normalize dir)
  (if (jazz:string-ends-with? dir "/")
      dir
    (string-append dir "/")))


(cond-expand
  (windows
    (define (jazz:path=? path1 path2)
      (%%string-ci=? path1 path2)))
  (else
   (define (jazz:path=? path1 path2)
     (%%string=? path1 path2))))


(define (jazz:create-directory dir #!key (feedback #f))
  (if (%%not (file-exists? dir))
      (begin
        (if feedback
            (feedback "; creating {a}..." dir))
        (create-directory dir))))


(define (jazz:create-directories dir #!key (feedback #f))
  (let ((path (%%reverse (jazz:split-string dir #\/))))
    (let iter ((scan (if (%%equal? (%%car path) "") (%%cdr path) path)))
      (if (%%not (%%null? scan))
          (begin
            (iter (%%cdr scan))
            (let ((subdir (jazz:join-strings (%%reverse scan) "/")))
              (if (%%not (file-exists? subdir))
                  (jazz:create-directory subdir feedback: feedback))))))))


(define (jazz:directory-content directory)
  (directory-files (if (%%string? directory)
                       (%%list path: directory ignore-hidden: 'dot-and-dot-dot)
                     directory)))


(define (jazz:directory-files directory)
  (jazz:collect-if (lambda (name)
                     (%%eq? (jazz:pathname-type (string-append directory name)) 'file))
                   (jazz:directory-content directory)))


(define (jazz:directory-directories directory)
  (jazz:collect-if (lambda (name)
                     (%%eq? (jazz:pathname-type (string-append directory name)) 'directory))
                   (jazz:directory-content directory)))


(define (jazz:directory-collect directory type)
  (jazz:collect-if (lambda (name)
                     (%%eq? (jazz:pathname-type (string-append directory name)) type))
                   (jazz:directory-content directory)))


(define (jazz:delete-directory directory #!optional (level 0) (delete-file? #f) (delete-directory? #f) (feedback #f))
  (if (jazz:empty-directory directory level delete-file? delete-directory? feedback)
      (begin
        (if feedback
            (feedback directory level))
        (delete-directory directory)
        #t)
    #f))


(define (jazz:empty-directory directory #!optional (level 0) (delete-file? #f) (delete-directory? #f) (feedback #f))
  (define (default-delete-file? file level)
    #t)
  
  (define (default-delete-directory? dir level)
    (jazz:empty-directory dir level delete-file? delete-directory? feedback))
  
  (let ((empty? #t))
    (for-each (lambda (name)
                (let ((file (string-append directory name)))
                  (if ((or delete-file? default-delete-file?) file level)
                      (begin
                        (if feedback
                            (feedback file level))
                        (delete-file file))
                    (set! empty? #f))))
              (jazz:directory-files directory))
    (for-each (lambda (name)
                (let ((dir (string-append directory name "/")))
                  (if ((or delete-directory? default-delete-directory?) dir (%%fx+ level 1))
                      (begin
                        (if feedback
                            (feedback dir level))
                        (delete-directory dir))
                    (set! empty? #f))))
              (jazz:directory-directories directory))
    empty?))


(define (jazz:copy-directory src dst #!key (copy? #t) (feedback #f))
  (define (copy src dst level)
    (if feedback
        (feedback src level))
    (let ((src-content (jazz:directory-content src)))
      (jazz:create-directories dst)
      (for-each (lambda (name)
                  (let ((sub-src (string-append src name))
                        (sub-dst (string-append dst name)))
                    (if (or (%%eq? copy? #t) (copy? name level))
                        (case (jazz:pathname-type sub-src)
                          ((file)
                           (copy-file sub-src sub-dst))
                          ((directory)
                           (copy (string-append sub-src "/") (string-append sub-dst "/") (%%fx+ level 1)))))))
                src-content)))
  
  (copy src dst 0))


(define (jazz:platform-eol-encoding platform)
  (case platform
    ((windows) 'cr-lf)
    (else 'lf)))


;; work around "~" not always existing on windows
(cond-expand
  (windows
   (define jazz:home-directory
     (let ((home-dir #f))
       (lambda ()
         (define (home-heuristic)
           (if jazz:kernel-windows-homedir
               (if (jazz:string-starts-with? jazz:kernel-windows-homedir ".")
                   (string-append jazz:kernel-install jazz:kernel-windows-homedir)
                 (let ((dir jazz:kernel-windows-homedir))
                   (if (%%not (file-exists? dir))
                       (create-directory dir))
                   dir))
             (if (file-exists? "~")
                 "~"
               (jazz:error "Unable to find homedir"))))
         
         (or home-dir
             (let ((home (home-heuristic)))
               (set! home-dir home)
               home))))))
  (else
   (define jazz:home-directory
     (lambda ()
       "~"))))


;;;
;;;; Command Line
;;;


(define (jazz:option? arg)
  (and (%%fx> (%%string-length arg) 0)
       (%%char=? (%%string-ref arg 0) #\-)))


(define (jazz:option-name arg)
  (let ((len (%%string-length arg)))
    (let ((start (if (and (%%fx>= len 2) (%%equal? (%%substring arg 0 2) "--"))
                     2
                   1)))
      (%%substring arg start len))))


(define (jazz:option=? arg option)
  (and (jazz:option? arg)
       (%%equal? (jazz:option-name arg) option)))


(define (jazz:find-option name options #!optional (default #f))
  (let ((pair (%%assoc name options)))
    (if pair
        (%%cdr pair)
      default)))


(define jazz:*command-arguments*
  #f)


(cond-expand
  (mac
    (define (jazz:effective-command-argument? arg)
      (%%not (jazz:string-starts-with? arg "-psn"))))
  (else
    (define (jazz:effective-command-argument? arg)
      #t)))


(define (jazz:command-executable)
  (%%car (command-line)))


(define (jazz:command-arguments)
  (or jazz:*command-arguments*
      (let ((arguments (jazz:collect-if jazz:effective-command-argument? (%%cdr (command-line)))))
        (set! jazz:*command-arguments* arguments)
        arguments)))


(define jazz:kernel-runtime-switches
  '("nosource"))


(define (jazz:add-kernel-runtime-switch switch)
  (set! jazz:kernel-runtime-switches (%%cons switch jazz:kernel-runtime-switches)))


(define (jazz:add-kernel-runtime-switches switches)
  (set! jazz:kernel-runtime-switches (%%append switches jazz:kernel-runtime-switches)))


(define (jazz:split-command-line arguments switches options missing-argument-for-option cont)
  (declare (proper-tail-calls))
  (define (split-commands arguments)
    (let iter ((commands '()) (arguments arguments))
      (if (or (%%null? arguments)
              (jazz:option? (%%car arguments)))
          (values (%%reverse commands) arguments)
        (iter (%%cons (%%car arguments) commands) (%%cdr arguments)))))
  
  (receive (commands arguments) (split-commands arguments)
    (let loop ((args arguments)
               (rev-options '()))
      (if (and (%%pair? args)
               (jazz:option? (%%car args)))
          (let ((opt (jazz:option-name (%%car args)))
                (rest (%%cdr args)))
            (cond ((%%member opt switches)
                   (loop rest
                         (%%cons (%%cons opt #t) rev-options)))
                  ((%%member opt options)
                   (if (%%pair? rest)
                       (loop (%%cdr rest)
                             (%%cons (%%cons opt (%%car rest)) rev-options))
                     (begin
                       (if missing-argument-for-option
                           (missing-argument-for-option opt))
                       (loop rest rev-options))))
                  (else
                   (cont commands (%%reverse rev-options) args))))
        (cont commands (%%reverse rev-options) args)))))


;;;
;;;; Platform
;;;


(define (jazz:linux-family)
  (let ((os (caddr (system-type))))
    (let ((str (symbol->string os)))
      (cond ((eq? os 'linux-gnu)
             'linux)
            ((or (jazz:string-starts-with? str "openbsd")
                 (jazz:string-starts-with? str "freebsd")
                 (jazz:string-starts-with? str "netbsd"))
             'bsd)
            (else
             #f)))))


;;;
;;;; Process
;;;


(define (jazz:->open-process-settings path-or-settings)
  (if (%%string? path-or-settings)
      (%%list 'path: path-or-settings)
    path-or-settings))


(define (jazz:invoke-process path-or-settings)
  (let ((port (open-process `(,@(jazz:->open-process-settings path-or-settings)
                              stdin-redirection: #f
                              stdout-redirection: #f
                              stderr-redirection: #f))))
    (process-status port)))


(define (jazz:call-process path-or-settings)
  (let ((code (jazz:invoke-process path-or-settings)))
    (if (not (= code 0))
        (jazz:error "Process failed ({a}): {s}" code path-or-settings))))


;;;
;;;; Feedback
;;;


(define (jazz:feedback fmt-string . rest)
  (display (apply jazz:format fmt-string rest))
  (newline)
  (force-output))


;;;
;;;; Report
;;;


(define jazz:*reporting?*
  #f)


(define jazz:*report-port*
  #f)


(define (jazz:reporting?)
  jazz:*reporting?*)


;; worker
(define (jazz:setup-reporting)
  (set! jazz:*reporting?* #t))


;; master
(define (jazz:setup-report file)
  (set! jazz:*reporting?* #t)
  (set! jazz:*report-port* (open-output-file file)))


(define (jazz:report fmt-string . rest)
  (let ((message (apply jazz:format fmt-string rest)))
    (cond (jazz:worker-port
           (write (cons 'report message) jazz:worker-port)
           (force-output jazz:worker-port))
          (jazz:*report-port*
           (let ((port jazz:*report-port*))
             (display message port)
             (newline port)
             (force-output port))))))


;;;
;;;; Crash
;;;


(define jazz:crash-reporter
  #f)


(define (jazz:set-crash-reporter proc)
  (set! jazz:crash-reporter proc)))
