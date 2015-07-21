;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; System
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
;;;  Portions created by the Initial Developer are Copyright (C) 1996-2015
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


(unit protected jazz.backend.scheme.runtime.core.system


;;;
;;;; Process
;;;


(cond-expand
  (gambit
    (define jazz:open-process open-process)
    (define jazz:process-status process-status)
    (define jazz:exit exit)
    (define jazz:exit-jobs ##exit-jobs)
    (define jazz:add-exit-job! ##add-exit-job!)
    (define jazz:clear-exit-jobs! ##clear-exit-jobs!))
  
  (else))


(define (jazz:switch? arg)
  (and (%%fx> (%%string-length arg) 0)
       (%%eqv? (%%string-ref arg 0) #\-)))


(define (jazz:switch-name arg)
  (let ((len (%%string-length arg)))
    (let ((start (if (and (%%fx>= len 2) (%%equal? (%%substring arg 0 2) "--"))
                     2
                   1)))
      (%%substring arg start len))))


(define jazz:kernel-runtime-options-with-no-args
  '("nosource"))


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
              (cond ((%%member (jazz:switch-name arg) jazz:kernel-runtime-options-with-no-args)
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
  (content read-only:)))
