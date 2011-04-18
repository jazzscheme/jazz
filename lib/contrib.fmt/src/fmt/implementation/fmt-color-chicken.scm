;;;; fmt-color-chicken.scm -- fmt-c for Chicken
;;
;; Copyright (c) 2007 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(require-extension fmt)

(module
 fmt-color
 (fmt-red
  fmt-blue
  fmt-green
  fmt-cyan
  fmt-yellow
  fmt-magenta
  fmt-white
  fmt-black
  fmt-bold
  fmt-underline
  fmt-color
  fmt-in-html
  )

(import scheme chicken fmt)

(include "fmt-color.scm")

)
