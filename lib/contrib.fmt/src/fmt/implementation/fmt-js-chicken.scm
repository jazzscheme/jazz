;;;; fmt-js-chicken.scm -- fmt-js for Chicken
;;
;; Copyright (c) 2011 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(require-extension fmt fmt-c)

(module
 fmt-js
 (js-expr js-function js-var js-comment js-array js-object js=== js>>>)

(import scheme chicken fmt fmt-c)

(include "fmt-js.scm")

)
