;;;============================================================================

;;; File: "fixnum.scm", Time-stamp: <2007-09-01 17:11:16 feeley>

;;; Copyright (c) 2006-2007 by Marc Feeley, All Rights Reserved.

;;;============================================================================

;;; Provides operations on fixnums.

(define-macro (snow-fxand x y) `(fxand ,x ,y))
(define-macro (snow-fxior x y) `(fxior ,x ,y))
(define-macro (snow-fxxor x y) `(fxxor ,x ,y))
(define-macro (snow-fxnot x)   `(fxnot ,x))

(define-macro (snow-fxarithmetic-shift-left x n)
  `(fxarithmetic-shift-left ,x ,n))

(define-macro (snow-fxarithmetic-shift-right x n)
  `(fxarithmetic-shift-right ,x ,n))