;;;; fmt-c-gauche.scm -- fmt-c for Gauche
;;
;; Copyright (c) 2007 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(define-module text.fmt.c
  (use srfi-1)
  (use srfi-6)
  (use srfi-13)
  (use text.fmt)
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
   c++ c-- c+ c- c* c/ c% c& |c\|| c^ c~ c! c&& |c\|\|| c<< c>> c== c!=
   c< c> c<= c>= c= c+= c-= c*= c/= c%= c&= |c\|=| c^= c<<= c>>= ++c --c
   c++/post c--/post c. c->
   c-bit-or c-or c-bit-or=
   cpp-if cpp-ifdef cpp-ifndef cpp-elif cpp-endif cpp-undef
   cpp-include cpp-define cpp-wrap-header cpp-pragma cpp-line
   cpp-error cpp-warning cpp-stringify cpp-sym-cat
   c-comment c-block-comment c-attribute
   ))
(select-module text.fmt.c)

