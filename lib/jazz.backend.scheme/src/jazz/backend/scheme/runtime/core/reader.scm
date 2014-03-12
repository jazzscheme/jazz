;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Jazz Reader
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
;;;  Portions created by the Initial Developer are Copyright (C) 1996-2012
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


(unit protected jazz.backend.scheme.runtime.core.reader


;;;
;;;; Readtable
;;;


(define (jazz:readtable? obj)
  (%%readtable? obj))

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


(cond-expand
  (gambit
    (define (jazz:eof-object)
      #!eof)
    
    (define jazz:read-u8 read-u8)
    (define jazz:write-u8 write-u8)
    (define jazz:read-subu8vector read-subu8vector)
    (define jazz:write-subu8vector write-subu8vector)
    
    (define jazz:read-line
      read-line)
    
    (define (jazz:read-proper-line port)
      (let ((line (read-line port #\newline #t)))
        (if (eof-object? line)
            (values #f #f)
          (let ((len (%%string-length line)))
            (if (and (%%fx> len 0) (%%eqv? (%%string-ref line (%%fx- len 1)) #\newline))
                (values (%%string-shrink! line (%%fx- len 1)) #t)
              (values line #f))))))
    
    (define jazz:read-all
      read-all)
    
    (define (jazz:with-readtable readtable thunk)
      (parameterize ((current-readtable readtable))
        (thunk))))
  
  (else))


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
  (jazz:read-until eof-object? port)))
