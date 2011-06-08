;;;; fmt-js-gauche.scm -- fmt-js for Gauche
;;
;; Copyright (c) 2011 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(define-module text.fmt.js
  (use srfi-1)
  (use srfi-6)
  (use srfi-13)
  (use text.fmt)
  (use text.fmt.c)
  (export js-expr js-function js-var js-comment js-array js-object js=== js>>>))
(select-module text.fmt.js)

