;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Quasiquote Expansion
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
;;;  Portions created by the Initial Developer are Copyright (C) 1996-2007
;;;  the Initial Developer. All Rights Reserved.
;;;
;;;  Contributor(s):
;;;    Stephane Le Cornec
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


(in ?)


(class Quasiquote-Expander extends Object

  
  @macro (quasiquote quoted) @expansion (quote quoted)
  @macro (quasiquote a (unquote b)) @expansion (list (quote a) b)
  @macro (quasiquote a (unquote-splicing b)) @expansion (append (list (quote a)) b)
  
  
  (method meta (expand-special-qq expr)
    (if (pair? expr)
        (let ((head (car expr)))
          (if (eq? (unwrap-syntax head) 'quasiquote)
              (error "Recursive quasiquote is not yet implemented")
            (if (eq? (unwrap-syntax head) 'unquote)
                (if (not-nil? (cddr expr))
                    (error "Too many arguments for unquote")
                  (cadr expr))
              (if (eq? (unwrap-syntax head) 'unquote-splicing)
                  (if (not-nil? (cddr expr))
                      (error "Too many arguments for unquote-splicing")
                    expr)
                (expand-pair-qq expr)))))
      (if (reference? expr)
          (expand-reference-qq expr)
        nil)))
  
  
  (method meta (expand-reference-qq expr)
    (let ((form (reference-form expr))
          (context (reference-context expr))
          (test (function dynamic (atom)
                  (and (pair? atom)
                       (eq? (unwrap-syntax (car atom)) 'unquote-splicing))))
          (proc (function dynamic (best second)
                  (if (and (pair? best)
                           (eq? (unwrap-syntax (car best)) 'unquote-splicing))
                      (error "Illegal unquote-splicing in reference")
                    (if best best (list 'quote second))))))
      (list 'new-reference
            (proc (expand-special-qq form) form)
            (proc (expand-special-qq context) context))))
  
  
  (method meta (expand-pair-qq expr)
    (let ((head-atom (expand-special-qq (car expr)))
          (tail-atom (expand-special-qq (cdr expr))))
      (if (and (nil? head-atom) (nil? tail-atom))
          nil
        (let* ((test (function dynamic (atom)
                       (and (pair? atom)
                            (eq? (unwrap-syntax (car atom)) 'unquote-splicing))))
               (proc (function dynamic (expr)
                       (if (nil? expr)
                           nil
                         (list 'quote expr))))
               (oper (if (test head-atom) 'append 'cons))
               (head (if head-atom
                         (if (test head-atom)
                             (cadr head-atom)
                           head-atom)
                       (proc (car expr))))
               (tail (if tail-atom
                         (if (test tail-atom)
                             (error "Illegal unquote-splicing at end of list")
                           tail-atom)
                       (proc (cdr expr)))))
          (list oper head tail)))))
  
  
  ;; kept separate to handle qq within qq (future dev?)
  (method meta public (expand expressions)
    (if (nil? expressions)
        (error "Not enough arguments for quasiquote")
      (let ((expr (expand-special-qq (car expressions))))
        (if expr
            expr
          (cons 'quote expressions))))))
