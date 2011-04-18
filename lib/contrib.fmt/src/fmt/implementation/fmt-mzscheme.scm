;;;; fmt-mzscheme.scm -- MzScheme fmt extension
;;
;; Copyright (c) 2007-2009 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(module fmt mzscheme
        (require (only (lib "1.ss" "srfi")
                       find
                       fold
                       length+
                       remove
                       filter
                       every)
                 (lib "6.ss" "srfi")
                 (only (lib "13.ss" "srfi")
                       substring/shared
                       string-index
                       string-index-right
                       string-count
                       string-concatenate
                       string-concatenate-reverse
                       string-tokenize
                       string-pad
                       string-prefix?
                       string-suffix?)
                 (lib "23.ss" "srfi")
                 "let-optionals.ss"
                 "mantissa.ss")
        (provide
         new-fmt-state
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
         upcase downcase titlecase pad-char comma-char decimal-char
         with-width wrap-lines fold-lines justify
         make-string-fmt-transformer
         make-space make-nl-space display-to-string write-to-string
         fmt-columns columnar tabular line-numbers
         )

(define (make-eq?-table) (make-hash-table))
(define hash-table-ref/default hash-table-get)
(define hash-table-set! hash-table-put!)
(define hash-table-walk hash-table-for-each)

        ;; -- insert fmt.scm here --

