;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Format
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


(module core.base.runtime.format


(define (jazz.format . rest)
  (jazz.parse-format rest
    (lambda (port fmt-string arguments)
      (case port
        ((:string)
         (let ((output (open-output-string)))
           (jazz.format-to output fmt-string arguments)
           (get-output-string output)))
        ((:console)
         (jazz.format-to (current-output-port) fmt-string arguments))
        (else
         (jazz.format-to port fmt-string arguments))))))


(define (jazz.parse-format rest proc)
  (if (string? (%%car rest))
      (proc ':string (%%car rest) (%%cdr rest))
    (proc (%%car rest) (%%cadr rest) (%%cddr rest))))


(define jazz.lowlevel-format
  jazz.format)


(define (jazz.format-to output fmt-string arguments)
  (let* ((control (open-input-string fmt-string))
         (done? #f)
         (format-directive
          (lambda ()
            (let ((directive (read control)))
              (read-char control)
              (case directive
                ((a)
                 (jazz.display (%%car arguments) output)
                 (set! arguments (%%cdr arguments)))
                ((s)
                 (jazz.write (%%car arguments) output)
                 (set! arguments (%%cdr arguments)))
                ((t)
                 (jazz.write (%%car arguments) output)
                 (set! arguments (%%cdr arguments)))
                ((p)
                 (jazz.print (%%car arguments) output (%%cadr arguments))
                 (set! arguments (%%cddr arguments)))
                ((l)
                 (let ((first? #t))
                   (for-each (lambda (element)
                               (if first?
                                   (set! first? #f)
                                 (display " " output))
                               (jazz.display element output))
                             (%%car arguments)))
                 (set! arguments (%%cdr arguments)))
                ((%)
                 (newline output))
                (else
                 (error "Unknown format directive" directive)))))))
    (%%while (%%not done?)
      (let ((c (read-char control)))
        (if (eof-object? c)
            (set! done? #t)
          (cond ((%%eqv? c #\~)
                 (write-char (read-char control) output))
                ((%%eqv? c #\{)
                 (format-directive))
                (else
                 (write-char c output)))))))))
