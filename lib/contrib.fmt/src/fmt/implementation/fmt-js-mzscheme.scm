;;;; fmt-js-mzscheme.scm -- fmt-js for MzScheme
;;
;; Copyright (c) 2012 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(module fmt-js mzscheme
 (require "fmt.ss"
          "fmt-c.ss")

 (provide
  js-expr js-function js-var
  js=== js>>> js-comment js-array js-pair js-object)

 ;; -- insert fmt-js.scm here --
 