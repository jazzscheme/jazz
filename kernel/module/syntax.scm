;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Module Syntax
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
;;;  Portions created by the Initial Developer are Copyright (C) 1996-2006
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


(cond-expand
  (gambit
    (declare (block)
             (standard-bindings)
             (extended-bindings)
             (not safe)))
  (else))


(define-macro (module name . rest)
  (jazz.expand-module name rest))


(define (jazz.parse-module rest proc)
  (if (and (%%pair? rest)
           (%%pair? (%%car rest))
           (%%eq? (%%caar rest) 'require))
      (proc (%%cdar rest) (%%cdr rest))
    (proc '() rest)))


(define (jazz.parse-require require proc)
  (let ((name (%%car require))
        (scan (%%cdr require))
        (load #f)
        (phase 'runtime))
    (if (and (%%pair? scan)
             (%%pair? (%%car scan))
             (%%eq? (%%caar scan) 'load))
        (begin
          (set! load (%%car (%%cdar scan)))
          (set! scan (%%cdr scan))))
    (if (and (%%pair? scan)
             (%%pair? (%%car scan))
             (%%eq? (%%caar scan) 'phase))
        (begin
          (set! phase (%%car (%%cdar scan)))
          (set! scan (%%cdr scan))))
    (proc name
          load
          phase)))


(define (jazz.expand-module name rest)
  (jazz.parse-module rest
    (lambda (requires body)
      `(begin
         ,@(jazz.declarations)
         ,@(map (lambda (require)
                  (jazz.parse-require require
                    (lambda (module-name load phase)
                      `(jazz.load-module ',module-name))))
                requires)
         ,@body
         (jazz.module-loaded ',name)))))
