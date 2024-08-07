;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Declares
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
;;;  Portions created by the Initial Developer are Copyright (C) 1996-2018
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


(declare
  (block)
  (standard-bindings)
  (extended-bindings)
  (not safe))


(jazz:verbose-kernel 'kernel.declares)


(define jazz:not-standard-bindings
  '(not standard-bindings
        open-input-file
        open-output-file
        call-with-input-file
        call-with-output-file
        with-input-from-file
        with-output-to-file))


(jazz:define-macro (jazz:kernel-declares)
  `(declare
     ,@(if jazz:debug-core?
           '()
         '((block)))
     
     ,@`((standard-bindings)
         ,jazz:not-standard-bindings
         (extended-bindings))
     
     ,@(if #f
           #; ;; this inline declaration or the one below breaks
              ;; compilation of jazz.git.foreign for some reason
           (eq? jazz:kernel-safety 'sealed)
           '()
         '((not inline)))
     
     #; ;; don't think we need those for the kernel
     ,@(if jazz:kernel-optimize?
           '()
         '((not proper-tail-calls)
           (not optimize-dead-local-variables)))
     
     ,@(if jazz:debug-user?
           '()
         '((not safe)))))


(define (jazz:declares kind)
  `((declare
      ;; block is only really usefull for units coded in a
      ;; style where control remains mostly inside the unit
      ;; using block can give an noticable performance gain
      ;; in certain cases but breaks dynamic code reevaluation
      ,@(if (or (and (eq? kind 'unit)
                     (or (eq? jazz:kernel-safety 'release)
                         (eq? jazz:kernel-safety 'sealed)))
                (and (eq? kind 'module)
                     (eq? jazz:kernel-safety 'sealed)))
            '((block))
          '())
      
      ,@`((standard-bindings)
          ,jazz:not-standard-bindings
          (extended-bindings))
      
      ;; inlining can have a huge impact on compilation time
      ;; and really bloat the size of the generated .o1 files
      ,@(if #f
            #; ;; this inline declaration or the one below breaks
            ;; compilation of jazz.git.foreign for some reason
            (eq? jazz:kernel-safety 'sealed)
            '()
          '((not inline)))
      
      ,@(if (or jazz:kernel-optimize?
                (eq? jazz:kernel-safety 'sealed))
            '()
          '((not proper-tail-calls)
            (not optimize-dead-local-variables)))
      
      ,@(if jazz:debug-user?
            '()
          '((not safe))))))
