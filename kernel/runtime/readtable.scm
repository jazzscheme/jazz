;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Kernel Readtable
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


(block kernel.readtable


(define (jazz:make-jazz-readtable)
  (let ((readtable (readtable-max-unescaped-char-set (%%make-standard-readtable) #\U0010ffff)))
    (jazz:jazzify-readtable! readtable)
    readtable))


(define (jazz:jazzify-readtable! readtable)
  (jazz:readtable-named-char-table-set! readtable (%%append (jazz:readtable-named-char-table readtable) jazz:named-chars))
  (%%readtable-char-class-set! readtable #\[ #t jazz:read-tensor)
  (%%readtable-char-class-set! readtable #\{ #t jazz:read-literal)
  (%%readtable-char-class-set! readtable #\@ #t jazz:read-comment)
  (%%readtable-char-sharp-handler-set! readtable #\" jazz:read-delimited-string)
  (%%readtable-char-sharp-handler-set! readtable #\/ jazz:read-syntax-string))


(define jazz:named-chars
  '(("nul"                  . #\x00)
    ("home"                 . #\x01)
    ("enter"                . #\x03)
    ("end"                  . #\x04)
    ("info"                 . #\x05)
    ("backspace"            . #\x08)
    ("tab"                  . #\x09)
    ("page-up"              . #\x0B)
    ("page-down"            . #\x0C)
    ("escape"               . #\x1B)
    ("left-arrow"           . #\x1C)
    ("right-arrow"          . #\x1D)
    ("up-arrow"             . #\x1E)
    ("down-arrow"           . #\x1F)
    ("space"                . #\x20)
    ("exclamation-mark"     . #\x21)
    ("double-quote"         . #\x22)
    ("sharp"                . #\x23)
    ("ampersand"            . #\x26)
    ("quote"                . #\x27)
    ("open-parenthesis"     . #\x28)
    ("close-parenthesis"    . #\x29)
    ("times"                . #\x2A)
    ("plus"                 . #\x2B)
    ("comma"                . #\x2C)
    ("minus"                . #\x2D)
    ("period"               . #\x2E)
    ("slash"                . #\x2F)
    ("colon"                . #\x3A)
    ("semi-colon"           . #\x3B)
    ("equal"                . #\x3D)
    ("question-mark"        . #\x3F)
    ("at"                   . #\x40)
    ("open-bracket"         . #\x5B)
    ("backslash"            . #\x5C)
    ("close-bracket"        . #\x5D)
    ("exponential"          . #\x5E)
    ("underscore"           . #\x5F)
    ("backquote"            . #\x60)
    ("open-brace"           . #\x7B)
    ("vertical-bar"         . #\x7C)
    ("close-brace"          . #\x7D)
    ("tilde"                . #\x7E)
    ("delete"               . #\x7F)
    ("copyright"            . #\xA9)))


(define (jazz:read-tensor re c)
  (declare (proper-tail-calls))
  (let ((port (jazz:readenv-port re))
        (start-pos (%%readenv-current-filepos re)))
    (read-char port)
    (let ((lst (%%build-list re #t start-pos #\])))
      (jazz:readenv-wrap re
                         (if (%%null? lst)
                             (%%vector)
                           (let ((first (%%car lst))
                                 (len (%%length lst)))
                             (if (%%f64vector? (jazz:source-code first))
                                 (let ((vec (%%make-vector len)))
                                   (let loop ((lst lst) (n 0))
                                        (if (%%not (%%null? lst))
                                            (begin
                                              (%%vector-set! vec n (jazz:source-code (%%car lst)))
                                              (loop (%%cdr lst) (%%fx+ n 1)))))
                                   `(tensor ,vec))
                               (let ((vec (%%make-f64vector len)))
                                 (let loop ((lst lst) (n 0))
                                      (if (%%not (%%null? lst))
                                          (begin
                                            (%%f64vector-set! vec n (%%exact->inexact (jazz:source-code (%%car lst))))
                                            (loop (%%cdr lst) (%%fx+ n 1)))))
                                 vec))))))))


(define jazz:in-expression-comment?
  (%%make-parameter #f))


(define jazz:read-literals?
  (%%make-parameter #t))

(define jazz:read-literal-hook
  (%%make-parameter #f))


(jazz:define-variable jazz:new-literal)


(define (jazz:read-literal re c)
  (let ((port (jazz:readenv-port re))
        (start-pos (%%readenv-current-filepos re)))
    (read-char port)
    (if (%%eqv? (peek-char port) #\@)
        (jazz:error "Trying to read an unreadable literal")
      (let ((lst (%%build-list re #t start-pos #\})))
        (jazz:readenv-wrap re
                           (cond ;; do not read
                                 ((or (%%not (jazz:read-literals?)) (jazz:in-expression-comment?))
                                  #f)
                                 ;; empty literal
                                 ((%%null? lst)
                                  (%%unspecified))
                                 ;; walk
                                 ((jazz:walk-for)
                                  (jazz:load-foundation)
                                  (jazz:new-literal (%%car lst) (%%cdr lst)))
                                 ;; read
                                 (else
                                  (let ((name (%%car lst))
                                        (arguments (%%cdr lst))
                                        (hook (jazz:read-literal-hook)))
                                    (or (and hook (hook name arguments))
                                        (jazz:construct-literal name arguments))))))))))


(define (jazz:read-comment re c)
  (let ((port (jazz:readenv-port re)))
    (parameterize ((jazz:in-expression-comment? #t))
      (read-char port)
      (if (%%memv (peek-char port) '(#\space #\return #\newline))
          (read-char port) ; comment space
        (read port))       ; comment name
      (read port))         ; commented expr
    (%%read-datum-or-label-or-none-or-dot re)))


(define (jazz:read-delimited-string re next start-pos)
  (let ((port (jazz:readenv-port re)))
    (read-char port)
    (let ((output (open-output-string)))
      (let iter ()
           (let ((c (read-char port)))
             (cond ((%%eof-object? c)
                    #f)
                   ((%%eqv? c #\\)
                    (let ((escaped (read-char port)))
                      (case escaped
                        ((#\()
                         (write-char #\x02 output)
                         (iter))
                        ((#\))
                         (write-char #\x03 output)
                         (iter))
                        (else
                         (write-char escaped output)
                         (iter)))))
                   ((and (%%eqv? c #\")
                         (%%eqv? (peek-char port) #\#))
                    (read-char port)
                    (jazz:readenv-wrap re (get-output-string output)))
                   (else
                    (write-char c output)
                    (iter))))))))


(define (jazz:read-syntax-string re next start-pos)
  (let ((port (jazz:readenv-port re)))
    (read-char port)
    (let iter ()
         (let ((c (read-char port)))
           (cond ((%%eof-object? c)
                  #f)
                 ((%%eqv? c #\/)
                  (let ((output (open-output-string)))
                    (let iter ()
                         (let ((c (read-char port)))
                           (cond ((%%eof-object? c)
                                  #f)
                                 ((%%eqv? c #\/)
                                  (let sub ()
                                       (let ((next (read-char port)))
                                         (cond ((%%eqv? next #\/)
                                                (cond ((%%eqv? (peek-char port) #\#)
                                                       (read-char port)
                                                       (jazz:readenv-wrap re (get-output-string output)))
                                                      (else
                                                       (write-char c output)
                                                       (sub))))
                                               (else
                                                (write-char c output)
                                                (write-char next output)
                                                (iter))))))
                                 (else
                                  (write-char c output)
                                  (iter)))))))
                 ((or (%%eqv? c #\newline) (%%eqv? c #\return))
                  (jazz:error "Invalid syntax string"))
                 (else
                  (iter)))))))


(define jazz:jazz-readtable
  (jazz:make-jazz-readtable))


(define (jazz:with-jazz-readtable thunk)
  (parameterize ((current-readtable jazz:jazz-readtable))
    (thunk))))
