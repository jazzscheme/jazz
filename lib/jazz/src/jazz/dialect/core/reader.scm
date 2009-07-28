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


(module protected jazz.dialect.core.reader


;;;
;;;; Jazz
;;;


(cond-expand
  (gambit
    (define (jazz.make-jazz-readtable)
      (let ((readtable (readtable-max-unescaped-char-set (%%readtable-copy ##main-readtable) #\U0010ffff)))
        (jazz.jazzify-readtable! readtable)
        readtable))
    
    
    (define (jazz.jazzify-readtable! readtable)
      (jazz.readtable-named-char-table-set! readtable (%%append (jazz.readtable-named-char-table readtable) jazz.named-chars))
      (%%readtable-char-class-set! readtable #\{ #t jazz.read-literal)
      (%%readtable-char-class-set! readtable #\@ #t jazz.read-comment)
      (%%readtable-char-sharp-handler-set! readtable #\" jazz.read-delimited-string))
    
    
    (define jazz.named-chars
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
        ("close-brace"          . #\x7D)
        ("tilde"                . #\x7E)
        ("delete"               . #\x7F)
        ("copyright"            . #\xA9)))
    
    
    (define jazz.in-expression-comment?
      (make-parameter #f))
    
    
    (define (jazz.read-literal re c)
      (let ((port (jazz.readenv-port re))
            (start-pos (%%readenv-current-filepos re)))
        (read-char port)
        (if (%%eqv? (peek-char port) #\@)
            (jazz.error "Trying to read an unreadable literal")
          (let ((lst (%%build-list re #t start-pos #\})))
            (jazz.readenv-wrap re
              (if (or (%%not (jazz.read-literals?)) (jazz.in-expression-comment?))
                  #f
                (jazz.construct-literal (map (lambda (expr) (%%desourcify expr)) lst))))))))
    
    
    (define (jazz.read-comment re c)
      (let ((port (jazz.readenv-port re)))
        (parameterize ((jazz.in-expression-comment? #t))
          (read-char port)
          (read port)   ; comment name
          (read port))  ; commented expr
        (%%read-datum-or-label-or-none-or-dot re)))
    
    
    (define (jazz.read-delimited-string re next start-pos)
      (let ((port (jazz.readenv-port re)))
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
                     (jazz.readenv-wrap re (get-output-string output)))
                    (else
                     (write-char c output)
                     (iter))))))))
    
    
    (define jazz.jazz-readtable
      (jazz.make-jazz-readtable))
    
    
    (define (jazz.with-jazz-readtable thunk)
      (parameterize ((current-readtable jazz.jazz-readtable))
        (thunk)))
    
    
    (define (jazz.char-symbol char)
      (let ((table (jazz.readtable-named-char-table jazz.jazz-readtable)))
        (let ((res (jazz.rassq char table)))
          (and res (%%car res))))))
  
  
  (else))


;;;
;;;; General
;;;


(cond-expand
  (gambit
    (define (jazz.eof-object)
      #!eof)
    
    (define jazz.read-u8 read-u8)
    (define jazz.write-u8 write-u8)
    (define jazz.read-subu8vector read-subu8vector)
    (define jazz.write-subu8vector write-subu8vector)
    
    (define jazz.read-line
      read-line)
    
    (define (jazz.read-proper-line port)
      (let ((line (read-line port #\newline #t)))
        (if (eof-object? line)
            (values #f #f)
          (let ((len (%%string-length line)))
            (if (and (%%fx> len 0) (%%eqv? (%%string-ref line (%%fx- len 1)) #\newline))
                (values (%%string-shrink! line (%%fx- len 1)) #t)
              (values line #f))))))
    
    (define jazz.read-all
      read-all)
    
    (define (jazz.with-readtable readtable thunk)
      (parameterize ((current-readtable readtable))
        (thunk))))
  
  (else))


(define (jazz.skip-whitespace port)
  (%%while (char-whitespace? (peek-char port))
    (read-char port)))


(define (jazz.read-delimited port delimiter)
  (let ((queue (jazz.new-queue)))
    (jazz.skip-whitespace port)
    (%%while (%%not (%%eqv? (peek-char port) delimiter))
      (jazz.enqueue queue (read port))
      (jazz.skip-whitespace port))
    (read-char port)
    (jazz.queue-list queue)))


(define (jazz.read-until test port)
  (let ((expr '())
        (queue (jazz.new-queue))
        (done? #f))
    (%%while (%%not done?)
      (let ((expr (read port)))
        (if (test expr)
            (set! done? #t)
          (jazz.enqueue queue expr))))
    (jazz.queue-list queue)))


(define (jazz.read-content port)
  (jazz.read-until eof-object? port)))
