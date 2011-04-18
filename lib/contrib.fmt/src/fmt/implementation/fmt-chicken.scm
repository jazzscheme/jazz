;;;; fmt-chicken.scm -- Chicken fmt extension
;;
;; Copyright (c) 2007-2011 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(module
 fmt
 (new-fmt-state
  fmt fmt-start fmt-if fmt-capture fmt-let fmt-bind fmt-null
  fmt-ref fmt-set! fmt-add-properties! fmt-set-property!
  fmt-col fmt-set-col! fmt-row fmt-set-row!
  fmt-radix fmt-set-radix! fmt-precision fmt-set-precision!
  fmt-properties fmt-set-properties! fmt-width fmt-set-width!
  fmt-writer fmt-set-writer! fmt-port fmt-set-port!
  fmt-decimal-sep fmt-set-decimal-sep!
  copy-fmt-state
  fmt-file fmt-try-fit cat apply-cat nl fl nl-str
  fmt-join fmt-join/last fmt-join/dot
  fmt-join/prefix fmt-join/suffix fmt-join/range
  pad pad/right pad/left pad/both trim trim/left trim/both trim/length
  fit fit/left fit/both tab-to space-to wrt wrt/unshared dsp
  pretty pretty/unshared slashified maybe-slashified
  num num/si num/fit num/comma radix fix decimal-align ellipses
  num/roman num/old-roman
  upcase downcase titlecase pad-char comma-char decimal-char
  with-width wrap-lines fold-lines justify
  make-string-fmt-transformer
  make-space make-nl-space display-to-string write-to-string
  fmt-columns columnar tabular line-numbers
  mantissa+exponent
  )

(import scheme chicken)
(require-extension ports srfi-1 srfi-69)
(require-library srfi-13 extras data-structures)
(import foreign
        (except srfi-13 string-tokenize)
        (only extras read-line)
        (only data-structures string-split))

(define (make-eq?-table) (make-hash-table eq?))

(cond-expand
 (compiling-xxxxx         ; clause disabled because many tests fail
  (cond-expand
   (big-endian
    (define %mantissa
      (foreign-lambda*
       number ((double f))
       "unsigned long long *n = (unsigned long long*)&f;
        return((*n) >> 12uLL);"))
    (define %exponent
      (foreign-lambda*
       number ((double f))
       "unsigned long long *n = (unsigned long long*)&f;
        return(((*n) >> 1uLL) & ((1uLL<<11uLL)-1uLL));")))
   (else  ;; little-endian
    (define %mantissa
      (foreign-lambda*
       number ((double f))
       "unsigned long long *n = (unsigned long long*)&f;
        return((*n) & ((1uLL<<52uLL)-1uLL));"))
    (define %exponent
      (foreign-lambda*
       number ((double f))
       "unsigned long long *n = (unsigned long long*)&f;
        return(((*n) >> 52uLL) & ((1uLL<<11uLL)-1uLL));"))))
  (define (mantissa+exponent num)
    (let ((e (%exponent num))
          (m (%mantissa num)))
      (cond
       ((= e #x7FF)
        (list 0 0))
       ((zero? e)
        (list m e))
       (else
        (list (+ m (* (arithmetic-shift 1 22)
                      (arithmetic-shift 1 30)))
              (- e #x3FF 52)))))))
 (else
  (define (mantissa+exponent num . opt)
    (if (zero? num)
        (list 0 0)
        (let-optionals* opt ((base 2) (mant-size 52) (exp-size 11))
          (let* ((bot (expt base mant-size))
                 (top (* base bot)))
            (let lp ((n num) (e 0))
              (cond
                ((>= n top) (lp (quotient n base) (+ e 1)))
                ((< n bot) (lp (* n base) (- e 1)))
                (else (list n e))))))))
  ))

(include "fmt.scm")
(include "fmt-pretty.scm")

;; Override string-tokenize (which does not support unicode on Chicken)
;; and use string-split so utf8 byte sequences are not treated as whitespace.
(define (string-tokenize s)
  (string-split s))
(include "fmt-column.scm")

)
