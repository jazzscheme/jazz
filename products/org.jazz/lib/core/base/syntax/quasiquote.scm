;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Quasiquote
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


(module core.base.syntax.quasiquote


(cond-expand
  (blues
    ;; kept separate to handle qq within qq (future dev?)
    (define (jazz.expand-quasiquote expressions)
      (if (null? expressions)
          (error "Not enough arguments for quasiquote")
        (let ((expr (jazz.expand-special-qq (car expressions))))
          (if (not (null? expr))
              expr
            (cons 'quote expressions)))))
    
    
    (define (jazz.expand-special-qq expr)
      (if (pair? expr)
          (let ((head (car expr)))
            (if (eq? head 'quasiquote)
                (error "Recursive quasiquote is not yet implemented")
              (if (eq? head 'unquote)
                  (if (not (null? (cddr expr)))
                      (error "Too many arguments for unquote")
                    (cadr expr))
                (if (eq? head 'unquote-splicing)
                    (if (not (null? (cddr expr)))
                        (error "Too many arguments for unquote-splicing")
                      expr)
                  (jazz.expand-pair-qq expr)))))
        '()))
    
    
    (define (jazz.expand-pair-qq expr)
      (let ((head-atom (jazz.expand-special-qq (car expr)))
            (tail-atom (jazz.expand-special-qq (cdr expr))))
        (if (and (null? head-atom) (null? tail-atom))
            '()
          (let* ((test (lambda (atom)
                         (and (pair? atom)
                              (eq? (car atom) 'unquote-splicing))))
                 (proc (lambda (expr)
                         (if (null? expr)
                             '()
                           (list 'quote expr))))
                 (head (if (not (null? head-atom))
                           (if (test head-atom)
                               (cadr head-atom)
                             head-atom)
                         (proc (car expr))))
                 (tail (if (not (null? tail-atom))
                           (if (test tail-atom)
                               (error "Illegal unquote-splicing at end of list")
                             tail-atom)
                         (proc (cdr expr)))))
            (if (test head-atom)
                (if (null? tail)
                    head
                  (list 'append head tail))
              (list 'cons head tail)))))))
  
  
  (else)))
