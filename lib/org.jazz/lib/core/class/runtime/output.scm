;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Output
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


(module core.class.runtime.output


(define jazz.dialect.language.Object.print
  #f)

(set! jazz.dialect.language.Object.print #f)


(define jazz.output-mode
  ':reader)


(set! jazz.display
      (lambda (value output)
        (jazz.output-value value output ':human)))


(set! jazz.write
      (lambda (value output)
        (jazz.output-value value output ':reader)))


(define (jazz.print value output detail)
  (case detail
    ((:human) (display value output))
    ((:reader :text :describe) (write value output))
    (else (jazz.error "Unknown print detail: {s}" detail))))


(define (jazz.->string value)
  (cond ((%%unspecified? value)
         "<unspecified>")
        ((%%values? value)
         "<values>")
        (else
         (let ((output (open-output-string)))
           (jazz.output-value value output jazz.output-mode)
           (get-output-string output)))))


(define (jazz.output-value value output detail)
  (cond ((or (%%null? value) (%%pair? value))
         (jazz.output-list value output detail))
        ((jazz.primitive? value)
         (jazz.print value output detail))
        ((and jazz.use-print? jazz.dialect.language.Object.print)
         ;; the rank of print is known to be 2 as it is the third method of Object
         ((%%class-dispatch value 0 2) value output detail))
        (else
         (jazz.write-jazz output value))))


(define (jazz.output-list lst output detail)
  (display "(" output)
  (jazz.output-list-content lst output detail)
  (display ")" output))


(define (jazz.output-list-content lst output detail)
  (if (%%not (%%null? lst))
      (let ((scan lst)
            (done? #f))
        (%%while (and (%%not done?) (%%not (%%null? scan)))
          (jazz.output-value (%%car scan) output detail)
          (set! scan (%%cdr scan))
          (if (%%not (%%null? scan))
              (if (%%pair? scan)
                  (display " " output)
                (begin
                  (display " . " output)
                  (jazz.output-value scan output detail)
                  (set! done? #t))))))))


(define (jazz.debug expr . rest)
  (display (jazz.->string expr))
  (for-each (lambda (expr)
              (display " ")
              (display (jazz.->string expr)))
            rest)
  (newline)
  (force-output))


(define jazz.terminal
  jazz.debug)


(define (jazz.bootstrap-output-value value output)
  (display (jazz.->string value) output))


(cond-expand
  (gambit
    (set! jazz.write-jazz
          (lambda (port obj)
            (if (and jazz.use-print? jazz.dialect.language.Object.print)
                ;; the rank of print is known to be 2 as it is the third method of Object
                ((%%class-dispatch obj 0 2) obj port ':reader)
              (let ((serial (object->serial-number obj)))
                (display "#<jazz #" port)
                (display serial port)
                (display ">" port))))))
  (else))


(cond-expand
  (chicken
    (define (jazz.pretty-print expr . rest)
      (apply pretty-print expr rest)))

  (gambit
    (define (jazz.pretty-print expr . rest)
      (apply pretty-print expr rest)))

  (else
   (define (jazz.pretty-print expr . rest)
     (display expr)
     (newline)))))
