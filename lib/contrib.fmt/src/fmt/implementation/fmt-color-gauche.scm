;;;; fmt-color-gauche.scm -- fmt-color for Gauche
;;
;; Copyright (c) 2007 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(define-module text.fmt.color
  (use text.fmt)
  (export fmt-in-html fmt-color fmt-in-html? fmt-use-html-font?
          fmt-colored fmt-red fmt-blue fmt-green fmt-cyan fmt-yellow
          fmt-magenta fmt-black fmt-white fmt-bold fmt-underline))
(select-module text.fmt.color)

