;;;; fmt-scheme48.scm - package descriptions for Scheme48
;;
;; Load this file with
;;
;;   ,config ,load fmt-scheme48.scm
;;
;; And the following packages will be available to ,open:
;;
;;   fmt
;;   fmt-c
;;   fmt-color
;;   fmt-unicode

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; this is just used for shared write

(define-interface srfi-69-interface
  (export
   make-hash-table hash-table-ref hash-table-ref/default
   hash-table-set! hash-table-update! hash-table-copy
   hash-table-fold hash-table-walk hash-table-keys hash-table-values
   hash-table->alist alist->hash-table hash-table-merge!
   hash-table-delete!))

(define-structure srfi-69 srfi-69-interface
  (optimize auto-integrate)
  (open scheme srfi-9 srfi-23)
  (files srfi-69))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the main formatting library

(define-interface fmt-interface
  (export
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
   ))

(define-structure fmt fmt-interface
  (optimize auto-integrate)
  (open scheme bitwise srfi-1 srfi-6 srfi-13 srfi-23 srfi-69 ascii)
  (begin (define (make-eq?-table) (make-hash-table eq?))
         (define integer->char ascii->char)
         (define char->integer char->ascii))
  (files let-optionals
         read-line
         string-ports
         mantissa
         fmt
         fmt-pretty
         fmt-column
         ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; formatting/generating C code

(define-interface fmt-c-interface
  (export
   fmt-in-macro? fmt-expression? fmt-return? fmt-default-type
   fmt-newline-before-brace? fmt-braceless-bodies?
   fmt-indent-space fmt-switch-indent-space fmt-op fmt-gen
   c-in-expr c-in-stmt c-in-test
   c-paren c-maybe-paren c-type c-literal? c-literal char->c-char
   c-struct c-union c-class c-enum c-typedef c-cast
   c-expr c-expr/sexp c-apply c-op c-indent c-current-indent-string
   c-wrap-stmt c-open-brace c-close-brace
   c-block c-braced-block c-begin
   c-fun c-var c-prototype c-param c-param-list
   c-while c-for c-if c-switch
   c-case c-case/fallthrough c-default
   c-break c-continue c-return c-goto c-label
   c-static c-const c-extern c-volatile c-auto c-restrict c-inline
   c++ c-- c+ c- c* c/ c% c& c^ c~ c! c&& c<< c>> c== c!=
   c< c> c<= c>= c= c+= c-= c*= c/= c%= c&= c^= c<<= c>>=
   c++/post c--/post c. c->
   c-bit-or c-or c-bit-or=
   cpp-if cpp-ifdef cpp-ifndef cpp-elif cpp-endif cpp-undef
   cpp-include cpp-define cpp-wrap-header cpp-pragma cpp-line
   cpp-error cpp-warning cpp-stringify cpp-sym-cat
   c-comment c-block-comment c-attribute
   ))

(define-structure fmt-c fmt-c-interface
  (optimize auto-integrate)
  (open scheme fmt srfi-1 srfi-13 srfi-23 ascii)
  (begin (define integer->char ascii->char)
         (define char->integer char->ascii))
  (files fmt-c))

(define-structure fmt-js
  (export js-expr js-function js-var js-comment js-array js-object js=== js>>>)
  (optimize auto-integrate)
  (open scheme fmt fmt-c)
  (files fmt-js))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; simple color utilities (ANSI and HTML)

(define-interface fmt-color-interface
  (export fmt-in-html fmt-color fmt-in-html? fmt-use-html-font?
          fmt-colored fmt-red fmt-blue fmt-green fmt-cyan fmt-yellow
          fmt-magenta fmt-black fmt-white fmt-bold fmt-underline))

(define-structure fmt-color fmt-color-interface
  (optimize auto-integrate)
  (open scheme fmt bitwise ascii)
  (begin (define integer->char ascii->char)
         (define char->integer char->ascii))
  (files fmt-color))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; unicode

(define-interface fmt-unicode-interface
  (export unicode-char-width unicode-string-width fmt-unicode))

(define-structure fmt-unicode fmt-unicode-interface
  (optimize auto-integrate)
  (open scheme fmt bitwise byte-vectors ascii)
  (begin (define u8vector byte-vector)
         (define u8vector-ref byte-vector-ref)
         (define integer->char ascii->char)
         (define char->integer char->ascii))
  (files fmt-unicode))

