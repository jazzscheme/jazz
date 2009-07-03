;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Either
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
;;;  Portions created by the Initial Developer are Copyright (C) 1996-2008
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


(library protected jazz.dialect.syntax.either scheme


(import (jazz.dialect.kernel.boot)
        (jazz.dialect.syntax.bind)
        (jazz.dialect.syntax.macros))


;; @syntax (either code result) @expansion (if code code result)
;; @syntax (either (f) (g) (h)) @expansion (let ((sym15 (f))) (if sym15 sym15 (let ((sym16 (g))) (if sym16 sym16 (h)))))

(syntax public (either form-src)
  (let ((expressions (cdr (source-code form-src))))
    (if (null? expressions)
        (error "Not enough arguments for either")
      (sourcify-if
        (let ((scan expressions)
              (complex? #f))
          (while (not (null? (cdr scan)))
            (when (not (symbol? (source-code (car scan))))
                  (set! complex? #t))
            (set! scan (cdr scan)))
          (if (not complex?)
              (letrec ((proc
                         (lambda (pair)
                           (bind (expr . rest) pair
                             (if (null? rest)
                                 expr
                               (list 'if expr expr (proc rest)))))))
                (proc expressions))
            (letrec ((proc
                       (lambda (pair)
                         (bind (expr . rest) pair
                           (if (null? rest)
                               expr
                             (let ((symbol (generate-symbol)))
                               (list 'let (list (list symbol expr)) (list 'if symbol symbol (proc rest)))))))))
              (proc expressions))))
        form-src)))))
