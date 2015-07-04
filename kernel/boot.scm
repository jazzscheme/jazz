;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Kernel Boot
;;;
;;;  The contents of this file are subject to the Mozilla Public License Version
;;;  1.1 (the "License"); you may not use this file except in compliance with
;;;  the License. You may obtain a copy of the License at
;;;  http://www.mozilla.org/MPL/
;;;
;;;  Software distributed under the License is distributed on an "AS IS" basis,
;;;  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
;;;  for the specific language governing rights and limitations under the
;;;  License.
;;;
;;;  The Original Code is JazzScheme.
;;;
;;;  The Initial Developer of the Original Code is Guillaume Cartier.
;;;  Portions created by the Initial Developer are Copyright (C) 1996-2015
;;;  the Initial Developer. All Rights Reserved.
;;;
;;;  Contributor(s):
;;;
;;;  Alternatively, the contents of this file may be used under the terms of
;;;  the GNU General Public License Version 2 or later (the "GPL"), in which
;;;  case the provisions of the GPL are applicable instead of those above. If
;;;  you wish to allow use of your version of this file only under the terms of
;;;  the GPL, and not to allow others to use your version of this file under the
;;;  terms of the MPL, indicate your decision by deleting the provisions above
;;;  and replace them with the notice and other provisions required by the GPL.
;;;  If you do not delete the provisions above, a recipient may use your version
;;;  of this file under the terms of any one of the MPL or the GPL.
;;;
;;;  See www.jazzscheme.org for details.


(define jazz:Kernel
  (list
    "kernel/syntax/verbose"
    "kernel/syntax/header"
    "kernel/syntax/macro"
    "kernel/syntax/block"
    "kernel/syntax/expansion"
    "kernel/syntax/features"
    "kernel/syntax/declares"
    "kernel/syntax/primitives"
    "kernel/syntax/structure"
    "kernel/syntax/syntax"
    "kernel/syntax/runtime"
    "kernel/runtime/settings"
    "kernel/runtime/base"
    "kernel/runtime/record"
    "kernel/syntax/repository"
    "kernel/runtime/configuration"
    "kernel/runtime/version"
    "kernel/runtime/common"
    "kernel/runtime/advise"
    "kernel/runtime/build"
    "kernel/runtime/digest"))


(define jazz:Kernel-Setup
  '("kernel/runtime/unit"
    "kernel/runtime/readtable"
    "kernel/runtime/setup"))


(define jazz:load-kernel
  (let ((loaded? #f))
    (lambda (setup? #!optional (interpret? #t))
      (define verbose?
        #f)
      
      (define (load-file file)
        (if verbose?
            (begin
              (display file)
              (newline)
              (force-output)))
        (load (string-append jazz:source file)))
      
      (define (load-files files)
        (for-each load-file files))
      
      ;; hack around loading header being very
      ;; time consuming because of gambit's header
      (let ((path (string-append jazz:source "kernel/syntax/header"))
            (src (string-append jazz:source "kernel/syntax/header.scm"))
            (o1 (string-append jazz:source "kernel/syntax/header.o1")))
        (if (not (file-exists? o1))
            (compile-file path)
          (if (> (time->seconds (file-last-modification-time src))
                 (time->seconds (file-last-modification-time o1)))
              (begin
                (delete-file o1)
                (compile-file path)))))
      
      (if (not loaded?)
          (begin
            (if (not interpret?)
                (load-file "kernel/runtime/product"))
            (load-files jazz:Kernel)
            (if setup?
                (load-files jazz:Kernel-Setup))
            (set! loaded? #t))))))
