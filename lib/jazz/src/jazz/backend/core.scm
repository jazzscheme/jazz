;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Jazz Core
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


(unit protected jazz.backend.core


;;;
;;;; Continuation
;;;


(declare (proper-tail-calls))


(define (jazz:continuation-graft-no-winding cont proc)
  (jazz:check-continuation cont 1 (continuation-graft-no-winding cont proc)
    (%%continuation-graft-no-winding cont proc)))


(define (jazz:continuation-parent cont)
  (jazz:check-continuation cont 1 (continuation-parent cont)
    (%%continuation-parent cont)))


(define (jazz:continuation-creator cont)
  (jazz:check-continuation cont 1 (continuation-creator cont)
    (%%continuation-creator cont)))


(define (jazz:continuation-locat cont)
  (jazz:check-continuation cont 1 (continuation-locat cont)
    (%%continuation-locat cont)))


(define (jazz:continuation-next cont)
  (jazz:check-continuation cont 1 (continuation-next cont)
    (%%continuation-next cont)))


(define (jazz:continuation-backtrace cont depth procedure-ignore? module-ignore?)
  (define (continuation-creator cont)
    (let ((proc (%%continuation-creator cont)))
      (if (and proc (%%closure? proc))
          (%%closure-code proc)
        proc)))
  
  (define (continuation-next-interesting cont)
    (let loop ((current-cont cont))
         (if current-cont
             (if (or (and procedure-ignore? (procedure-ignore? (continuation-creator current-cont)))
                     (let ((current-location (%%continuation-locat current-cont)))
                       (and current-location module-ignore? (module-ignore? (%%locat-container current-location)))))
                 (loop (%%continuation-next current-cont))
               current-cont)
           #f)))
  
  (define (continuation-next-distinct cont creator)
    (let loop ((current-cont (%%continuation-next cont)))
         (if current-cont
             (if (%%eq? creator (continuation-creator current-cont))
                 (loop (%%continuation-next current-cont))
               current-cont)
           #f)))
  
  (define (identify-location locat)
    (let ((container (and locat (%%locat-container locat))))
      (if container
          (let ((filepos (%%position->filepos (%%locat-position locat))))
            (let ((line (%%filepos-line filepos))
                  (col (%%filepos-col filepos)))
              (%%list container line col)))
        #f)))
  
  (define (identify cont d)
    (let ((cont (and cont (or (%%not depth) (%%fx< d depth)) (continuation-next-interesting cont))))
      (if (%%not cont)
          '()
        (let ((creator (continuation-creator cont))
              (location (identify-location (%%continuation-locat cont))))
          (%%cons (%%list creator location)
                  (identify (continuation-next-distinct cont creator)
                            (%%fx+ d 1)))))))
  
  (identify cont 0))


;;;
;;;; Debug
;;;


;; inspect a Jazz object
(define (inspect obj)
  (jazz:inspect-object (if (integer? obj) (jazz:serial->object obj) obj)))


;; run the message loop
(define (run-loop)
  (let ((get-process (jazz:global-ref 'jazz.process:current-process))
        (run-loop (jazz:global-ref 'jazz.process.Process:Process:run-loop)))
    (run-loop (get-process))))


;; resume the message loop
(define (resume)
  (let ((get-process (jazz:global-ref 'jazz.process:current-process))
        (invoke-resume-loop (jazz:global-ref 'jazz.process.Process:Process:invoke-resume-loop)))
    (invoke-resume-loop (get-process))))


;; start a scheme repl
(define (start-scheme-repl #!key (select? #t))
  (jazz:load-unit 'jazz)
  (jazz:load-unit 'jazz.debuggee)
  ((jazz:module-ref 'jazz.debuggee 'set-default-context) #f)
  ((jazz:module-ref 'jazz 'start-repl) readtable: jazz:scheme-readtable select?: select?))


;;;
;;;; Devel
;;;


(define (jazz:subtype obj)
  (%%subtype obj))


;;;
;;;; Global
;;;


(jazz:define-macro (jazz:define-global name)
  (let ((getter (%%string->symbol (%%string-append "get-" (%%symbol->string name))))
        (setter (%%string->symbol (%%string-append "set-" (%%symbol->string name))))
        (value (jazz:generate-symbol "value")))
    `(begin
       (define ,name
         #f)
       (define (,getter)
         ,name)
       (define (,setter ,value)
         (set! ,name ,value)))))


;;;
;;;; ? and %
;;;


(jazz:define-global ?)
(jazz:define-global ?a)
(jazz:define-global ?b)
(jazz:define-global ?c)
(jazz:define-global ?d)
(jazz:define-global ?e)
(jazz:define-global ?f)
(jazz:define-global ?g)
(jazz:define-global ?h)
(jazz:define-global ?i)
(jazz:define-global ?j)
(jazz:define-global ?k)
(jazz:define-global ?l)
(jazz:define-global ?m)
(jazz:define-global ?n)
(jazz:define-global ?o)
(jazz:define-global ?p)
(jazz:define-global ?q)
(jazz:define-global ?r)
(jazz:define-global ?s)
(jazz:define-global ?t)
(jazz:define-global ?u)
(jazz:define-global ?v)
(jazz:define-global ?w)
(jazz:define-global ?x)
(jazz:define-global ?y)
(jazz:define-global ?z)
(jazz:define-global ?0)
(jazz:define-global ?1)
(jazz:define-global ?2)
(jazz:define-global ?3)
(jazz:define-global ?4)
(jazz:define-global ?5)
(jazz:define-global ?6)
(jazz:define-global ?7)
(jazz:define-global ?8)
(jazz:define-global ?9)


(jazz:define-global %)
(jazz:define-global %a)
(jazz:define-global %b)
(jazz:define-global %c)
(jazz:define-global %d)
(jazz:define-global %e)
(jazz:define-global %f)
(jazz:define-global %g)
(jazz:define-global %h)
(jazz:define-global %i)
(jazz:define-global %j)
(jazz:define-global %k)
(jazz:define-global %l)
(jazz:define-global %m)
(jazz:define-global %n)
(jazz:define-global %o)
(jazz:define-global %p)
(jazz:define-global %q)
(jazz:define-global %r)
(jazz:define-global %s)
(jazz:define-global %t)
(jazz:define-global %u)
(jazz:define-global %v)
(jazz:define-global %w)
(jazz:define-global %x)
(jazz:define-global %y)
(jazz:define-global %z)
(jazz:define-global %0)
(jazz:define-global %1)
(jazz:define-global %2)
(jazz:define-global %3)
(jazz:define-global %4)
(jazz:define-global %5)
(jazz:define-global %6)
(jazz:define-global %7)
(jazz:define-global %8)
(jazz:define-global %9)


(jazz:define-global $)
(jazz:define-global $a)
(jazz:define-global $b)
(jazz:define-global $c)
(jazz:define-global $d)
(jazz:define-global $e)
(jazz:define-global $f)
(jazz:define-global $g)
(jazz:define-global $h)
(jazz:define-global $i)
(jazz:define-global $j)
(jazz:define-global $k)
(jazz:define-global $l)
(jazz:define-global $m)
(jazz:define-global $n)
(jazz:define-global $o)
(jazz:define-global $p)
(jazz:define-global $q)
(jazz:define-global $r)
(jazz:define-global $s)
(jazz:define-global $t)
(jazz:define-global $u)
(jazz:define-global $v)
(jazz:define-global $w)
(jazz:define-global $x)
(jazz:define-global $y)
(jazz:define-global $z)
(jazz:define-global $0)
(jazz:define-global $1)
(jazz:define-global $2)
(jazz:define-global $3)
(jazz:define-global $4)
(jazz:define-global $5)
(jazz:define-global $6)
(jazz:define-global $7)
(jazz:define-global $8)
(jazz:define-global $9)


;;;
;;;; Hook
;;;


(define (jazz:get-exception-hook)
  (jazz:primordial-exception-handler-hook-ref))

(define (jazz:set-exception-hook hook)
  (jazz:primordial-exception-handler-hook-set! hook))


(define (jazz:invoke-exception-hook hook exc)
  (hook exc jazz:thread-end-with-uncaught-exception!))


;;;
;;;; System
;;;


(define (jazz:system-exception-hook exc other)
  (jazz:repl-exception-handler-hook exc other))


;;;
;;;; Terminal
;;;


(define (jazz:set-terminal-title)
  (display "\033]0;Terminal\007" (repl-output-port)))

(define (jazz:bring-terminal-to-front)
  (display "\033[5t" (repl-output-port)))

(define (jazz:clear-terminal)
  (display "\033[H\033[J" (repl-output-port)))


;;;
;;;; Memory
;;;


(define (jazz:gc)
  (%%gc))


(define (jazz:gc-count)
  (%%flonum->fixnum (f64vector-ref (%%process-statistics) 6)))


(define (jazz:gc-statistics)
  (let ((vec (%%process-statistics)))
    (values (f64vector-ref vec 3)
            (f64vector-ref vec 4)
            (f64vector-ref vec 5)
            (%%flonum->fixnum (f64vector-ref vec 6)))))


(define (jazz:add-gc-interrupt-job! thunk)
  (%%add-gc-interrupt-job! thunk))


(define (jazz:clear-gc-interrupt-jobs!)
  (%%clear-gc-interrupt-jobs!))


(define (jazz:last-gc-real-time)
  (f64vector-ref (%%process-statistics) 14))


;;;
;;;; Heap
;;;


(define (jazz:get-min-heap)
  (%%get-min-heap))

(define (jazz:set-min-heap! bytes)
  (%%set-min-heap! bytes))

(define (jazz:get-max-heap)
  (%%get-max-heap))

(define (jazz:set-max-heap! bytes)
  (%%set-max-heap! bytes))


(define (jazz:process-memory)
  (let ((vec (%%process-statistics)))
    (let ((last_gc_heap_size (f64vector-ref vec 15))
          (last_gc_live      (f64vector-ref vec 17))
          (last_gc_movable   (f64vector-ref vec 18))
          (last_gc_still     (f64vector-ref vec 19)))
      (values last_gc_heap_size
              last_gc_live
              last_gc_movable
              last_gc_still))))


(define (jazz:symbols-memory)
  (let ((count 0)
        (chars 0))
    (for-each (lambda (lst)
                (set! count (%%fx+ count (%%length lst)))
                (for-each (lambda (s)
                            (set! chars (%%fx+ chars (%%string-length (%%symbol->string s)))))
                          lst))
              (map (lambda (s)
                     (let loop ((s s) (lst '()))
                          (if (%%symbol? s)
                              (loop (%%vector-ref s 2) (%%cons s lst))
                            (%%reverse lst))))
                   (%%vector->list (%%symbol-table))))
    (values count chars)))


(define (jazz:symbol-table)
  (%%symbol-table))

(define (jazz:keyword-table)
  (%%keyword-table))


(define (jazz:bytes-allocated! floats i)
  (%%get-bytes-allocated! floats i))


(define (jazz:get-live-percent)
  (%%get-live-percent))


(define (jazz:raise-heap-overflow-exception)
  (%%raise-heap-overflow-exception))


;;;
;;;; Still
;;;


(define jazz:still8-minimum
  2048)

(define jazz:still16-minimum
  1024)

(define jazz:still32-minimum
  512)

(define jazz:still64-minimum
  256)


;;;
;;;; Classes
;;;


(define (jazz:classes-statistics)
  (let ((nb-classes 0) (sz-classes 0)
        (nb-interfaces 0) (sz-interfaces 0)
        (nb-slots 0) (sz-slots 0)
        (nb-methods 0) (sz-methods 0))
    (define (process-class class)
      (set! nb-classes (%%fx+ nb-classes 1))
      (set! sz-classes (%%fx+ sz-classes 
                              (jazz:vector-size class)
                              (jazz:table-size (%%get-category-fields class))
                              (jazz:vector-size (%%get-category-ancestors class))
                              (jazz:list-size (%%get-category-descendants class))
                              (jazz:list-size (%%get-class-interfaces class))
                              (jazz:list-size (%%get-class-instance-slots class))
                              (jazz:vector-vector-size (%%get-class-class-table class))
                              (jazz:vector-vector-size (%%get-class-interface-table class))))
      (jazz:table-iterate (%%get-category-fields class)
        (lambda (name field)
          (cond ((jazz:is? field jazz:Slot) (process-slot field))
                ((jazz:is? field jazz:Method) (process-method field)))))
      (for-each process-class (%%get-category-descendants class)))
    
    (define (process-slot slot)
      (set! nb-slots (%%fx+ nb-slots 1))
      (set! sz-slots (%%fx+ sz-slots (jazz:vector-size slot))))
    
    (define (process-method method)
      (set! nb-methods (%%fx+ nb-methods 1))
      (set! sz-methods (%%fx+ sz-methods (jazz:vector-size method))))
    
    (process-class jazz:Object)
    
    (values
      nb-classes sz-classes
      nb-interfaces sz-interfaces
      nb-slots sz-slots
      nb-methods sz-methods)))


;;;
;;;; Sizes
;;;


(define jazz:word-bytes   4)
(define jazz:f64-bytes    8)
(define jazz:pair-bytes  12)
(define jazz:table-bytes 32)


(define (jazz:vector-size v)
  (%%fx+ jazz:word-bytes (%%fx* jazz:word-bytes (%%vector-length v))))

(define (jazz:safe-vector-size v)
  (if (%%vector? v)
      (jazz:vector-size v)
    0))

(define (jazz:f64vector-size v)
  (%%fx+ jazz:word-bytes (%%fx* jazz:f64-bytes (f64vector-length v))))

(define (jazz:vector-vector-size v)
  (if (%%not v)
      0
    (%%fx+ (jazz:vector-size v)
           (let iter ((n 0) (size 0))
                (if (%%fx< n (%%vector-length v))
                    (let ((v (%%vector-ref v n)))
                      (iter (%%fx+ n 1) (%%fx+ size (if v (jazz:vector-size v) 0))))
                  size)))))

(define (jazz:list-size l)
  (%%fx* jazz:pair-bytes (%%length l)))

(define (jazz:table-size t)
  (let ((gcht1 (%%vector-ref t 3))
        (gcht2 (%%vector-ref t 5))
        (floats (%%vector-ref t 4)))
    (%%fx* jazz:word-bytes
           (%%fx+
             1 (%%vector-length t)
             ;; 1 (%%vector-length floats)
             (if (%%gc-hash-table? gcht1) (%%fx+ 1 (%%vector-length gcht1)) 0)
             (if (%%gc-hash-table? gcht2) (%%fx+ 1 (%%vector-length gcht2)) 0)))))


;;;
;;;; Features
;;;


(define (jazz:cond-expand-features)
  (%%cond-expand-features))


(define (jazz:cond-expand-features-set! features)
  (%%cond-expand-features features))


(define (jazz:cond-expand-features-add! feature)
  (%%cond-expand-features (append (%%cond-expand-features) (list feature))))


;;;
;;;; Fixnum
;;;


;; not very efficient but have to test for now

(define (jazz:fixnum->flonum n)
  (if (%%fixnum? n)
      (%%fixnum->flonum n)
    (jazz:type-error n jazz:Fixnum)))

(define (jazz:flonum->fixnum n)
  (if (%%flonum? n)
      (%%flonum->fixnum n)
    (jazz:type-error n jazz:Flonum)))


(define (jazz:->flonum value)
  (cond ((%%flonum? value)
         value)
        ((%%fixnum? value)
         (%%fixnum->flonum value))
        ((%%ratnum? value)
         (%%ratnum->flonum value))
        (else
         (jazz:type-error value jazz:Flonum))))


(define (jazz:arithmetic-shift-left x y)
  (arithmetic-shift x y))

(define (jazz:arithmetic-shift-right x y)
  (arithmetic-shift x (- y)))


;;;
;;;; Flonum
;;;


(define (jazz:flalloc)
  (%%flalloc))

(define (jazz:flref fl ignore)
  (%%flref fl ignore))

(define (jazz:flset! fl ignore val)
  (%%flset! fl ignore val))


;;;
;;;; Random
;;;


(define jazz:random-integer-65536
  (let* ((rs (make-random-source))
         (ri (random-source-make-integers rs)))
    (random-source-randomize! rs)
    (lambda ()
      (ri 65536))))


;;;
;;;; Pathname
;;;


(define (jazz:current-directory . rest)
  (if (%%null? rest)
      (let ((dir (current-directory)))
        (jazz:pathname-normalize
          (if (and (jazz:filesystem-allowed? dir)
                   (jazz:pathname-exists? dir))
              dir
            jazz:kernel-root)))
    (let ((dir (%%car rest)))
      (current-directory dir))))

(define (jazz:with-current-directory dir thunk)
  (parameterize ((current-directory dir))
    (thunk)))


;;;
;;;; Port
;;;


(define (jazz:standard-input-port)
  jazz:stdin-port)

(define (jazz:standard-output-port)
  jazz:stdout-port)

(define (jazz:standard-error-port)
  jazz:stderr-port)


(define (jazz:write-string str port)
  (%%write-string str port))


;;;
;;;; Terminal
;;;


(cond-expand
  (windows
   (define (jazz:terminal-available?)
     ;; terminal is always available on windows
     #t)
   
   (define (jazz:filesystem-authorize-terminal)
     #f))
  (else
   (define jazz:cached-unix-controlling-terminal?
     (jazz:unspecified))
   
   (define (jazz:unix-controlling-terminal?)
     (if (jazz:unspecified? jazz:cached-unix-controlling-terminal?)
         (set! jazz:cached-unix-controlling-terminal?
               (jazz:catch-exception-filter
                 os-exception?
                 (lambda (exception)
                   #f)
                 (lambda ()
                   (close-port (open-file "/dev/tty"))
                   #t))))
     jazz:cached-unix-controlling-terminal?)
   
   (define (jazz:terminal-available?)
     (jazz:unix-controlling-terminal?))
   
   (define (jazz:filesystem-authorize-terminal)
     (jazz:filesystem-authorize "/dev/tty"))))


;;;
;;;; Console
;;;


(define (jazz:get-console-port)
  (if (jazz:global-bound? 'jazz.language.runtime:current-console-port)
      ((jazz:global-ref 'jazz.language.runtime:current-console-port))
    #f))


;;;
;;;; Readtable
;;;


(define (jazz:make-standard-readtable)
  (%%make-standard-readtable))

(define (jazz:readtable-copy readtable)
  (%%readtable-copy readtable))

(define (jazz:readtable-char-delimiter? readtable c)
  (%%readtable-char-delimiter? readtable c))

(define (jazz:readtable-char-delimiter?-set! readtable c delimiter?)
  (%%readtable-char-delimiter?-set! readtable c delimiter?))

(define (jazz:readtable-char-handler readtable c)
  (%%readtable-char-handler readtable c))

(define (jazz:readtable-char-handler-set! readtable c handler)
  (%%readtable-char-handler-set! readtable c handler))

(define (jazz:readtable-char-sharp-handler readtable c)
  (%%readtable-char-sharp-handler readtable c))

(define (jazz:readtable-char-sharp-handler-set! readtable c handler)
  (%%readtable-char-sharp-handler-set! readtable c handler))

(define (jazz:readtable-char-class-set! readtable c delimiter? handler)
  (%%readtable-char-class-set! readtable c delimiter? handler))

(define (jazz:readtable-escaped-char-table readtable)
  (%%readtable-escaped-char-table readtable))

(define (jazz:readtable-escaped-char-table-set! readtable table)
  (%%readtable-escaped-char-table-set! readtable table))


(define (jazz:char-symbol char)
  (let ((table (jazz:readtable-named-char-table jazz:jazz-readtable)))
    (let ((res (jazz:rassq char table)))
      (and res (%%car res)))))


;;;
;;;; General
;;;


(define (jazz:eof-object)
  #!eof)

(define (jazz:read-proper-line port)
  (let ((line (read-line port #\newline #t)))
    (if (eof-object? line)
        (values #f #f)
      (let ((len (%%string-length line)))
        (if (and (%%fx> len 0) (%%eqv? (%%string-ref line (%%fx- len 1)) #\newline))
            (values (%%string-shrink! line (%%fx- len 1)) #t)
          (values line #f))))))

(define (jazz:with-readtable readtable thunk)
  (parameterize ((current-readtable readtable))
    (thunk)))


(define (jazz:six-types)
  (%%six-types-ref))

(define (jazz:six-types-set! lst)
  (%%six-types-set! lst))

(define (jazz:print-marker)
  #f)


(define (jazz:skip-whitespace port)
  (%%while (char-whitespace? (peek-char port))
    (read-char port)))


(define (jazz:read-delimited port delimiter)
  (let ((queue (jazz:new-queue)))
    (jazz:skip-whitespace port)
    (%%while (%%not (%%eqv? (peek-char port) delimiter))
      (jazz:enqueue queue (read port))
      (jazz:skip-whitespace port))
    (read-char port)
    (jazz:queue-list queue)))


(define (jazz:read-until test port)
  (let ((expr '())
        (queue (jazz:new-queue))
        (done? #f))
    (%%while (%%not done?)
      (let ((expr (read port)))
        (if (test expr)
            (set! done? #t)
          (jazz:enqueue queue expr))))
    (jazz:queue-list queue)))


(define (jazz:read-content port)
  (jazz:read-until eof-object? port))


;;;
;;;; Repository
;;;


(define (jazz:repository-name repository)
  (%%get-repository-name repository))

(define (jazz:repository-title repository)
  (%%symbol->string (%%get-repository-name repository)))

(define (jazz:repository-directory repository)
  (%%get-repository-directory repository))

(define (jazz:repository-library-root repository)
  (%%get-repository-library-root repository))

(define (jazz:repository-library-directory repository)
  (%%get-repository-library-directory repository))

(define (jazz:repository-binary? repository)
  (%%get-repository-binary? repository))

(define (jazz:repository-dependencies repository)
  (%%get-repository-dependencies repository))


;;;
;;;; Package
;;;


(define (jazz:package-repository package)
  (%%get-package-repository package))

(define (jazz:package-name package)
  (%%get-package-name package))

(define (jazz:package-directory package)
  (jazz:package-root-pathname package ""))

(define (jazz:package-products package)
  (%%get-package-products package))

(define (jazz:package-profiles package)
  (%%get-package-profiles package))

(define (jazz:package-profiles-set! package profiles)
  (%%set-package-profiles package profiles))

(define (jazz:package-project package)
  (%%get-package-project package))

(define (jazz:package-title package)
  (%%get-package-title package))

(define (jazz:package-description package)
  (%%get-package-description package))

(define (jazz:package-authors package)
  (%%get-package-authors package))

(define (jazz:package-stage package)
  (%%get-package-stage package))


;;;
;;;; Resource
;;;


(define (jazz:resource-package resource)
  (%%get-resource-package resource))

(define (jazz:resource-path resource)
  (%%get-resource-path resource))

(define (jazz:resource-extension resource)
  (%%get-resource-extension resource))


;;;
;;;; Stack
;;;


(define (jazz:procedure-name procedure)
  (jazz:check-procedure procedure 1 (procedure-name procedure)
    (%%procedure-name procedure)))

(define (jazz:procedure-name-set! proc)
  (set! jazz:procedure-name proc))

(define (jazz:procedure-locat procedure)
  (jazz:check-procedure procedure 1 (procedure-locat procedure)
    (%%procedure-locat procedure)))


(define (jazz:closure? obj)
  (and (%%procedure? obj)
       (%%closure? obj)))

(define (jazz:closure-code closure)
  (jazz:check-closure closure 1 (closure-code closure)
    (%%closure-code closure)))

(define (jazz:closure-length closure)
  (jazz:check-closure closure 1 (closure-length closure)
    (%%closure-length closure)))

(define (jazz:closure-ref closure n)
  (jazz:check-closure closure 1 (closure-ref closure)
    (%%closure-ref closure n)))


(define (jazz:get-continuation-location cont)
  (jazz:locat->container/line/col (%%continuation-locat cont)))


(define (jazz:interpreted-continuation? cont)
  (%%interp-continuation? cont))


(define (jazz:get-continuation-stack cont depth)
  (let ((queue (jazz:new-queue)))
    (jazz:iterate-continuation-stack cont depth
      (lambda (cont)
        (jazz:enqueue queue cont)))
    (jazz:queue-list queue)))


(define (jazz:get-continuation-dynamic-environment cont)
  (let ((queue (jazz:new-queue)))
    (jazz:iterate-continuation-dynamic-environment cont jazz:reference-name
      (lambda (info)
        (jazz:enqueue queue info)))
    (jazz:queue-list queue)))


(define (jazz:get-continuation-lexical-environment cont)
  (let ((queue (jazz:new-queue)))
    (jazz:iterate-continuation-lexical-environment cont
      (lambda (info)
        (jazz:enqueue queue info)))
    (jazz:queue-list queue)))


(define (jazz:with-repl-context cont thunk)
  (let ((prev-context (%%thread-repl-context-get!)))
    (let ((context
            (jazz:make-repl-context
              (%%fx+ (jazz:repl-context-level prev-context) 1)
              0
              cont
              cont
              #f
              prev-context
              #f)))
      (jazz:repl-context-bind
        context
        thunk))))


(define (jazz:repl #!optional (write-reason #f))
  (begin
    (%%repl write-reason)
    #f))


(define (jazz:repl-debug #!optional (write-reason #f) (toplevel? #f))
  (begin
    (%%repl-debug write-reason toplevel?)
    #f))


(define (jazz:repl-result-history-add result)
  (let ((channel (%%thread-repl-channel-get! (%%current-thread))))
    (%%repl-channel-result-history-add channel result)))


;;;
;;;; Debugging
;;;


(define (jazz:inspect-repl-context context)
  `(:repl-context
    ,(jazz:repl-context-level context)
    ,(jazz:repl-context-depth context)
    ,(jazz:repl-context-cont context)
    ,(jazz:repl-context-initial-cont context)
    ,(jazz:repl-context-prev-level context)
    ,(jazz:repl-context-prev-depth context)))


;;;
;;;; Structure
;;;


(define (jazz:kind? obj)
  (%%type? obj))

(define (jazz:kind-id type)
  (%%type-id type))

(define (jazz:kind-name type)
  (%%type-name type))

(define (jazz:kind-flags type)
  (%%type-flags type))

(define (jazz:kind-super type)
  (%%type-super type))

(define (jazz:kind-length type)
  (%%type-field-count type))

(define (jazz:kind-fields type)
  (let loop ((index 1)
             (fields (%%type-all-fields type))
             (alist '()))
       (if (%%pair? fields)
           (let* ((name (%%car fields))
                  (rest (%%cdr fields))
                  (options (%%car rest))
                  (rest (%%cdr rest))
                  (val (%%car rest))
                  (rest (%%cdr rest)))
             (loop (%%fx+ index 1)
                   rest
                   (%%cons (%%list name index options val)
                           alist)))
         (jazz:reverse! alist))))


(define (jazz:structure? obj)
  (%%structure? obj))

(define (jazz:structure-kind obj)
  (%%structure-type obj))

(define (jazz:structure-ref obj i type)
  (%%structure-ref obj i type #f))

(define (jazz:structure-set! obj val i type)
  (%%structure-set! obj val i type #f))


;;;
;;;; Process
;;;


(define (jazz:switch? arg)
  (and (%%fx> (%%string-length arg) 0)
       (%%eqv? (%%string-ref arg 0) #\-)))


(define (jazz:switch-name arg)
  (let ((len (%%string-length arg)))
    (let ((start (if (and (%%fx>= len 2) (%%equal? (%%substring arg 0 2) "--"))
                     2
                   1)))
      (%%substring arg start len))))


(define (jazz:command-argument name #!key (error? #t))
  (define (skip-scripts arguments)
    (let iter ((arguments arguments))
      (if (or (%%null? arguments)
              (jazz:switch? (%%car arguments)))
          arguments
        (iter (%%cdr arguments)))))
  
  (if (eq? jazz:image 'executable)
      (let ((all (jazz:command-arguments)))
        (let iter ((arguments (skip-scripts all)))
          (if (%%null? arguments)
              #f
            (let ((arg (%%car arguments)))
              (cond ((%%member (jazz:switch-name arg) jazz:kernel-runtime-switches)
                     (iter (%%cdr arguments)))
                    ((or (%%not (jazz:switch? arg))
                         (%%null? (%%cdr arguments)))
                     (if error?
                         (jazz:error "Unable to parse command line: {a}" all)
                       #f))
                    ((%%equal? name (jazz:switch-name arg))
                     (%%cadr arguments))
                    (else
                     (iter (%%cddr arguments))))))))
    #f))


(define (jazz:command-argument? name)
  (if (eq? jazz:image 'executable)
      (let ((all (jazz:command-arguments)))
        (let iter ((arguments all))
          (if (%%null? arguments)
              #f
            (let ((arg (%%car arguments)))
              (if (and (jazz:switch? arg)
                       (%%equal? name (jazz:switch-name arg)))
                  #t
                (iter (%%cdr arguments)))))))
    #f))


;;;
;;;; Serialized
;;;


(define-type serialized
  id: 16E8E4BA-FD59-4E7A-A219-DFE934B6CC18

  (class read-only:)
  (content read-only:))


;;;
;;;; Table
;;;


(define (jazz:table-clear table key)
  (%%debug-assert (%%table? table)
    (%%table-clear table key)))


(define (jazz:map-table proc table)
  (%%debug-assert (%%table? table)
    (let ((queue (jazz:new-queue)))
      (jazz:table-iterate table
        (lambda (key value)
          (jazz:enqueue queue (proc key value))))
      (jazz:queue-list queue))))


(define (jazz:list->table alist #!key (test equal?))
  (%%list->table alist test: test))


(define (jazz:table->list table)
  (%%debug-assert (%%table? table)
    (%%table->list table)))


;;;
;;;; Interrupts
;;;


(define (jazz:interrupt-vector-set! code handler)
  (%%interrupt-vector-set! code handler))


(define jazz:*interrupts-enabled?*
  #t)

(define (jazz:interrupts-enabled?)
  jazz:*interrupts-enabled?*)

(define (jazz:disable-interrupts!)
  (%%disable-interrupts!)
  (set! jazz:*interrupts-enabled?* #f))

(define (jazz:enable-interrupts!)
  (%%enable-interrupts!)
  (set! jazz:*interrupts-enabled?* #t))


;;;
;;;; Heartbeat
;;;


(define jazz:get-heartbeat-interval
  (let ((vec (%%f64vector 0.)))
    (lambda ()
      (declare (not interrupts-enabled))
      (%%get-heartbeat-interval! vec 0)
      (%%f64vector-ref vec 0))))

(define (jazz:set-heartbeat-interval! seconds)
  (%%set-heartbeat-interval! seconds))


;;;
;;;; Thread
;;;


(define (jazz:thread-int! thread thunk)
  (if (%%not (jazz:thread-active? thread))
      (jazz:error "Thread is not active")
    (##thread-int! thread thunk)))


(define (jazz:thread-state-active? state)
  (or (thread-state-running? state)
      (thread-state-waiting? state)))


(define (jazz:thread-continuation thread)
  (declare (not interrupts-enabled))
  (if (jazz:thread-state-active? (thread-state thread))
      (let ((cont (jazz:thread-cont thread)))
        ;; hack - gambit thread init with dummy continuation
        (if (%%not (%%eq? (%%vector-ref cont 0) #!void))
            cont
          #f))
    #f))


(define (jazz:thread-mutexes thread)
  (declare (not interrupts-enabled))
  (let ((mutexes '()))
    (let iter ((mutex (^#vector-ref thread 2)) (mutexes '()))
         (if (%%eq? mutex thread)
             mutexes
           (iter (^#vector-ref mutex 2) (cons mutex mutexes))))))


;;;
;;;; Time
;;;


(define jazz:current-systime
  current-time)

(define (jazz:current-seconds! floats i)
  (%%get-current-time! floats i))

(define jazz:systime?
  time?)

(define jazz:systime->seconds
  time->seconds)

(define jazz:seconds->systime
  seconds->time)


;; adapted from ##display-gc-report
(define (jazz:present-absolute-seconds secs)
  (declare (not safe))
  
  (define (scale x m)
    (##flonum->exact-int (##flround (##fl* x m))))
  
  (define (show x*1000 unit)
    (define (decimals d)
      (let* ((n (##round (##/ x*1000 (##expt 10 (##fx- 3 d)))))
             (n-str (##number->string n 10))
             (n-str-len (##string-length n-str))
             (str (if (##fx< n-str-len d)
                      (##string-append
                        (##make-string (##fx- d n-str-len) #\0)
                        n-str)
                    n-str))
             (len (##string-length str))
             (split (##fx- len d)))
        (##string-append
          (if (##fx= d 0)
              str
            (##string-append (##substring str 0 split)
              "."
              (##substring str split len)))
          unit)))
    
    (cond ((##< x*1000 10000)
           (decimals 1))
          (else
           (decimals 0))))
  
  (let ((us (scale secs 1.0e9)))
    (if (##< us 1000000)
        (show us "us")
      (let ((ms (scale secs 1.0e6)))
        (if (##< ms 1000000)
            (show ms "ms")
          (let ((s (scale secs 1.0e3)))
            (show s "s")))))))


(define (jazz:present-seconds secs)
  (if (>= secs 0.)
      (jazz:present-absolute-seconds secs)
    (%%string-append "-" (jazz:present-absolute-seconds (- secs)))))


;; adapted from ##display-gc-report
(define (jazz:present-bytes bytes)
  (declare (not safe))
  
  (define (scale x m)
    (##flonum->exact-int (##flround (##fl* x m))))
  
  (define (show x*1000 unit)
    (define (decimals d)
      (let* ((n (##round (##/ x*1000 (##expt 10 (##fx- 3 d)))))
             (n-str (##number->string n 10))
             (n-str-len (##string-length n-str))
             (str (if (##fx< n-str-len d)
                      (##string-append
                        (##make-string (##fx- d n-str-len) #\0)
                        n-str)
                    n-str))
             (len (##string-length str))
             (split (##fx- len d)))
        (##string-append
          (if (##fx= d 0)
              str
            (##string-append (##substring str 0 split)
              "."
              (##substring str split len)))
          unit)))
    
    (cond ((##< x*1000 10000)
           (decimals 1))
          (else
           (decimals 0))))
  
  (if (##< bytes 1024)
      (##string-append (##number->string bytes) "B")
    (let ((bytes (##exact->inexact bytes)))
      (let ((k (scale bytes 9.765625e-1)))
        (if (##< k 1024000)
            (show k "K")
          (let ((m (scale bytes 9.5367431640625e-4)))
            (if (##< m 1024000)
                (show m "M")
              (let ((g (scale bytes 9.313225746154785e-7)))
                (show g "G")))))))))


;;;
;;;; UDP
;;;


(define (jazz:udp-socket-tos port)
  (##udp-socket-tos port))

(define (jazz:udp-socket-tos-set! port size)
  (##udp-socket-tos-set! port size))

(define (jazz:udp-socket-receive-buffer-size port)
  (##udp-socket-receive-buffer-size port))

(define (jazz:udp-socket-receive-buffer-size-set! port size)
  (##udp-socket-receive-buffer-size-set! port size))

(define (jazz:udp-socket-send-buffer-size port)
  (##udp-socket-send-buffer-size port))

(define (jazz:udp-socket-send-buffer-size-set! port size)
  (##udp-socket-send-buffer-size-set! port size))

(define (jazz:udp-socket-send-again-count port)
  (##udp-socket-send-again-count port)))
