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


(module core.class.runtime.output


;; BIG TIME SECURITY
(define primordial-exception-handler
  (current-exception-handler))

(if jazz.Debug-Print?
    (current-exception-handler
      (lambda (exc)
        (if jazz.Debug-Print?
            (set! jazz.Use-Print? #f))
        (primordial-exception-handler exc))))


(define jazz.dialect.language.Object.print
  #f)

(set! jazz.dialect.language.Object.print #f)


(define jazz.output-mode
  ':reader)


(define (jazz.display value output)
  (jazz.output-value value output ':human))


(define (jazz.write value output)
  (jazz.output-value value output ':reader))


(define (jazz.print value output detail)
  (case detail
    ((:human) (display value output))
    ((:reader :text :describe) (write value output))
    (else (jazz.error "Unknown print detail: {s}" detail))))


(define (jazz.->string value)
  (cond ((%%void? value)
         "<void>")
        ((%%values? value)
         "<values>")
        (else
         (let ((output (open-output-string)))
           (jazz.output-value value output jazz.output-mode)
           (get-output-string output)))))


(define (jazz.output-value value output detail)
  (cond ((list? value)
         (jazz.output-list value output detail))
        ((jazz.primitive? value)
         (jazz.print value output detail))
        ((and jazz.Use-Print? jazz.dialect.language.Object.print)
         (jazz.dialect.language.Object.print value output detail))
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
              (if (list? scan)
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
  (newline))


(define (jazz.bootstrap-output-value value output)
  (display (jazz.->string value) output))


(cond-expand
  (gambit
    (set! jazz.write-jazz
      (lambda (port obj)
        (if (and jazz.Use-Print? jazz.dialect.language.Object.print)
            (jazz.dialect.language.Object.print obj port ':reader)
          (let ((class-name (%%get-unit-name (%%get-object-class obj)))
                (serial-number (object->serial-number obj)))
            (display "#<jazz " port)
            (display class-name port)
            (display " #" port)
            (display serial-number port)
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
