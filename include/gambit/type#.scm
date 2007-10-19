;;;============================================================================

;;; File: "type#.scm", Time-stamp: <2007-06-05 23:10:42 feeley>

;;; Copyright (c) 1994-2007 by Marc Feeley, All Rights Reserved.

;;;============================================================================

;; General object representation.

;; Type tags.

(##define-macro (macro-type-fixnum)   0)
(##define-macro (macro-type-subtyped) 1)
(##define-macro (macro-type-special)  2)
(##define-macro (macro-type-pair)     3)

;; Subtype tags.

(##define-macro (macro-subtype-vector)       0)
(##define-macro (macro-subtype-pair)         1)
(##define-macro (macro-subtype-ratnum)       2)
(##define-macro (macro-subtype-cpxnum)       3)
(##define-macro (macro-subtype-structure)    4)
(##define-macro (macro-subtype-boxvalues)    5)
(##define-macro (macro-subtype-meroon)       6)
(##define-macro (macro-subtype-jazz)         7)

(##define-macro (macro-subtype-symbol)       8)
(##define-macro (macro-subtype-keyword)      9)
(##define-macro (macro-subtype-frame)        10)
(##define-macro (macro-subtype-continuation) 11)
(##define-macro (macro-subtype-promise)      12)
(##define-macro (macro-subtype-weak)         13)
(##define-macro (macro-subtype-procedure)    14)
(##define-macro (macro-subtype-return)       15)

(##define-macro (macro-subtype-foreign)      18)
(##define-macro (macro-subtype-string)       19)
(##define-macro (macro-subtype-s8vector)     20)
(##define-macro (macro-subtype-u8vector)     21)
(##define-macro (macro-subtype-s16vector)    22)
(##define-macro (macro-subtype-u16vector)    23)
(##define-macro (macro-subtype-s32vector)    24)
(##define-macro (macro-subtype-u32vector)    25)
(##define-macro (macro-subtype-f32vector)    26)

;; for alignment these 5 must be last:
(##define-macro (macro-subtype-s64vector)    27)
(##define-macro (macro-subtype-u64vector)    28)
(##define-macro (macro-subtype-f64vector)    29)
(##define-macro (macro-subtype-flonum)       30)
(##define-macro (macro-subtype-bignum)       31)

(##define-macro (macro-subtype-ovector? x) `(##fixnum.< ,x 8))
(##define-macro (macro-subtype-bvector? x) `(##fixnum.< 16 ,x))
