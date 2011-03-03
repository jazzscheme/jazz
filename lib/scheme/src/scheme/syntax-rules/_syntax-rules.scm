;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Scheme Syntax-Rules
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


(module scheme.syntax-rules scheme


(native private jazz:generate-symbol)
(native private jazz:source?)
(native private jazz:error)


(define-syntax public syntax-rules
  (rsc-macro-transformer
   (lambda (expr mac-env)
     (define (rename x) (make-syntactic-closure mac-env '() x))
     (define (compare x y) (identifier=? mac-env x mac-env y))
     (let* ((expr (unwrap-syntactic-closure expr))
            (srfi-46? (identifier? (cadr expr)))
            (lits (unwrap-syntactic-closure
                   (if srfi-46? (caddr expr) (cadr expr))))
            (forms (unwrap-syntactic-closure
                    (if srfi-46? (cdddr expr) (cddr expr))))
            (ellipse (if srfi-46? (cadr expr) '...))
            (count 0)
            (_er-macro-transformer (rename 'er-macro-transformer))
            (_unwrap-syntactic-closure (rename 'unwrap-syntactic-closure))
            (_strip-source-info (rename 'strip-source-info))
            (_strip-syntactic-closures (rename 'strip-syntactic-closures))
            (_lambda (rename 'lambda))      (_let (rename 'let))
            (_begin (rename 'begin))        (_if (rename 'if))
            (_and (rename 'and))            (_or (rename 'or))
            (_eq? (rename 'eq?))            (_equal? (rename 'equal?))
            (_car (rename 'car))            (_cdr (rename 'cdr))
            (_cons (rename 'cons))          (_pair? (rename 'pair?))
            (_null? (rename 'null?))        (_quote (rename 'quote))
            (_apply (rename 'apply))        (_append (rename 'append))
            (_map (rename 'map))            (_vector? (rename 'vector?))
            (_list? (rename 'list?))        (_lp (rename 'lp))
            ;;(_res (rename 'res)) (_display (rename 'display)) (_write (rename 'write))
            (_reverse (rename 'reverse))
            (_vector->list (rename 'vector->list))
            (_list->vector (rename 'list->vector))
            (_expr (generate-symbol "expr"))
            (_rename (generate-symbol "rename"))
            (_compare (generate-symbol "compare")))
       (define (next-v)
         (set! count (+ count 1))
         (generate-symbol
          (string-append "v_" (number->string count))))
       (define (any pred ls)
         (and (pair? ls) (or (pred (car ls)) (any pred (cdr ls)))))
       (define (expand-pattern pat tmpl)
         (let lp ((p (cdr (unwrap-syntactic-closure pat)))
                  (x `(,_cdr (,_unwrap-syntactic-closure ,_expr)))
                  (dim 0)
                  (vars '())
                  (k (lambda (vars) (or (expand-template tmpl vars) `(,_begin #f)))))
           (let ((v (next-v)))
             `(,_let ((,v ,x))
                ,(cond
                  ((identifier? p)
                   (if (any (lambda (l) (compare p l)) lits)
                       `(,_and (,_compare ,v (,_quote ,p)) ,(k vars))
                       `(,_let ((,p ,v)) ,(k (cons (cons p dim) vars)))))
                  (else
                   (let ((p (unwrap-syntactic-closure p)))
                     (cond
                      ((ellipse? p)
                       (cond
                        ((not (null? (cddr p)))
                         (error "syntax-rules: non-trailing ellipse: {s}" pat))
                        ((identifier? (car p))
                         `(,_and (,_list? (,_unwrap-syntactic-closure ,v))
                                 (,_let ((,(car p) ,v))
                                   ,(k (cons (cons (car p) (+ 1 dim)) vars)))))
                        (else
                         (let* ((w (next-v))
                                (new-vars (all-vars (car p) (+ dim 1)))
                                (ls-vars (map (lambda (x)
                                                (generate-symbol
                                                 (string-append
                                                  (symbol->string
                                                   (unwrap-syntactic-closure (car x)))
                                                  "-ls")))
                                              new-vars))
                                (once
                                 (lp (car p)
                                     `(,_car (,_unwrap-syntactic-closure ,w))
                                     (+ dim 1)
                                     '()
                                     (lambda (_)
                                       `(,_lp (,_cdr (,_unwrap-syntactic-closure ,w))
                                              ,@(map (lambda (x l) `(,_cons ,(car x) ,l))
                                                     new-vars
                                                     ls-vars))))))
                           `(,_let ,_lp ((,w ,v)
                                         ,@(map (lambda (x) (list x '())) ls-vars))
                                   (,_if (,_null? ,w)
                                       (,_let ,(map (lambda (x l) `(,(car x) (,_reverse (,_unwrap-syntactic-closure ,l))))
                                                    new-vars
                                                    ls-vars)
                                         ,(k (append new-vars vars)))
                                       (,_and (,_pair? (,_unwrap-syntactic-closure ,w))
                                              ,once)))))))
                      ((pair? p)
                       `(,_and (,_pair? (,_unwrap-syntactic-closure ,v))
                               ,(lp (car p)
                                    `(,_car (,_unwrap-syntactic-closure ,v))
                                    dim
                                    vars
                                    (lambda (vars)
                                      (lp (cdr p)
                                          `(,_cdr (,_unwrap-syntactic-closure ,v))
                                          dim vars k)))))
                      ((vector? p)
                       `(,_and (,_vector? (,_unwrap-syntactic-closure ,v))
                               ,(lp (vector->list p)
                                    `(,_vector->list (,_unwrap-syntactic-closure ,v))
                                    dim vars k)))
                      ((null? p)
                       `(,_and (,_null? ,v) ,(k vars)))
                      (else
                       `(,_and (,_equal? (,_strip-source-info ,v) ,p) ,(k vars)))
                      ))))))))
       (define (ellipse-quote? x)
         (and (pair? (unwrap-syntactic-closure x))
              (compare ellipse (car (unwrap-syntactic-closure x)))))
       (define (ellipse? x)
         (and (pair? x)
              (pair? (unwrap-syntactic-closure (cdr x)))
              (compare ellipse (car (unwrap-syntactic-closure (cdr x))))))
       (define (ellipse-depth x)
         (if (ellipse? x)
             (+ 1 (ellipse-depth (cdr x)))
             0))
       (define (ellipse-tail x)
         (if (ellipse? x)
             (ellipse-tail (unwrap-syntactic-closure (cdr x)))
             (cdr x)))
       (define (assoc-id id ls)
         (and (pair? ls)
              (if (compare id (caar ls))
                  (car ls)
                  (assoc-id id (cdr ls)))))
       (define (all-vars x dim)
         (let lp ((x x) (dim dim) (vars '()))
           (cond ((identifier? x)
                  (if (any (lambda (l) (compare x l)) lits)
                      vars
                      (cons (cons x dim) vars)))
                 ((or (syntactic-closure? x) (source? x))
                  (lp (unwrap-syntactic-closure x) dim vars))
                 ((ellipse? x) (lp (car x) (+ dim 1) vars))
                 ((pair? x) (lp (car x) dim (lp (cdr x) dim vars)))
                 ((vector? x) (lp (vector->list x) dim vars))
                 (else vars))))
       (define (free-vars x vars dim)
         (let lp ((x x) (free '()))
           (cond
            ((identifier? x)
             (if (and (not (any (lambda (f) (compare f x)) free))
                      (let ((cell (assoc-id x vars)))
                        (and cell (>= (cdr cell) dim))))
                 (cons x free)
                 free))
            ((or (syntactic-closure? x) (source? x))
             (lp (unwrap-syntactic-closure x) free))
            ((pair? x) (lp (car x) (lp (cdr x) free)))
            ((vector? x) (lp (vector->list x) free))
            (else free))))
       (define (expand-template tmpl vars)
         (let lp ((t tmpl) (dim 0))
           (cond
            ((identifier? t)
             (if (syntactic-closure? t)
                 t
                 (let ((cell (assoc-id t vars)))
                   (if cell
                       (if (<= (cdr cell) dim)
                           t
                           (error "syntax-rules: too few ...'s: {s}" tmpl))
                       `(,_rename (,_quote ,t))))))
            ((or (syntactic-closure? t) (source? t))
             (lp (unwrap-syntactic-closure t) dim))
            ((pair? t)
             (cond
              ((ellipse-quote? t)
               ;; (... ...) => ..., (... ... ...) => (... ...)
               (if (null? (cddr t)) (cadr t) (cdr t)))
              ((ellipse? t)
               (let* ((depth (ellipse-depth t))
                      (ell-dim (+ dim depth))
                      (ell-vars (free-vars (car t) vars ell-dim)))
                 (if (null? ell-vars)
                     (error "syntax-rules: too many ...'s: {s} in {s}" t tmpl)
                     (let* ((once (lp (car t) ell-dim))
                            (nest (if (and (null? (cdr ell-vars))
                                           (identifier? once)
                                           (eq? once (car vars)))
                                      once ;; shortcut
                                      `(,_map
                                        (,_lambda ,ell-vars ,once)
                                        ,@(map
                                           (lambda (x)
                                             `(,_unwrap-syntactic-closure ,x))
                                           ell-vars))))
                            (many (do ((d depth (- d 1))
                                       (many nest
                                             (list _apply _append many)))
                                      ((= d 1) many))))
                       (if (null? (ellipse-tail t))
                           many ;; shortcut
                           `(,_append ,many ,(lp (ellipse-tail t) dim)))))))
              (else
               `(,_cons ,(lp (car t) dim) ,(lp (cdr t) dim)))))
            ((vector? t) (list _list->vector (lp (vector->list t) dim)))
            ((null? t) (list _quote '()))
            (else t))))
       (let ((res `(,_er-macro-transformer
                    (,_lambda (,_expr ,_rename ,_compare)
                      (,_or
                       ,@(map
                          (lambda (clause)
                            (let ((clause (unwrap-syntactic-closure clause)))
                              (if (not (and (pair? clause) (pair? (cdr clause))))
                                  (error "syntax-rules: bad clause: {s}" clause)
                                  (expand-pattern (car clause) (cadr clause)))))
                          forms)
                       (error "syntax-rules: no expansion: {s}" ,_expr))
                      ))))
         ;; (cond
         ;;  ((memq (caar (strip-syntactic-closures (car forms))) '(match))
         ;;   (display "expansion: ") (write (strip-syntactic-closures res)) (newline)))
         res))))))
