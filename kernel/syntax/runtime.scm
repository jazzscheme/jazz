;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Syntax Runtime
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


(block kernel.runtime


;;;
;;;; Conditional
;;;


(define (jazz:valid-conditional-requirement clause)
  (if (%%pair? clause)
      (let ((requirement (%%car clause)))
        (if (or (%%symbol? requirement)
                (and (%%pair? requirement)
                     (%%memq (%%car requirement) '(and or not))))
            requirement
          #f))
    #f))


(define (jazz:conditional-satisfied? name requirement features)
  (cond ((%%symbol? requirement)
         (%%memq requirement features))
        ((and (%%pair? requirement)
              (%%eq? (%%car requirement) 'and))
         (jazz:every? jazz:feature-satisfied? (%%cdr requirement)))
        ((and (%%pair? requirement)
              (%%eq? (%%car requirement) 'or))
         (jazz:some? jazz:feature-satisfied? (%%cdr requirement)))
        ((and (%%pair? requirement)
              (%%eq? (%%car requirement) 'not)
              (%%pair? (%%cdr requirement)))
         (%%not (jazz:feature-satisfied? (%%cadr requirement))))
        (else
         (error (jazz:format "Invalid {a} requirement" name requirement)))))


(define (jazz:process-conditional name conditional features fulfill? proc)
  (let ((clauses (%%cdr conditional)))
    (let iter ((scan clauses))
         (if (%%null? scan)
             (if fulfill?
                 (error (jazz:format "Unfulfilled {a}" name)))
           (let ((clause (%%car scan)))
             (let ((requirement (jazz:valid-conditional-requirement clause)))
               (if (%%not requirement)
                   (error (jazz:format "Ill-formed {a} clause: {s}" name clause))
                 (if (or (jazz:conditional-satisfied? name requirement features)
                         (%%eq? requirement 'else))
                     (proc (%%cdr clause))
                   (iter (%%cdr scan))))))))))


;;;
;;;; Feature
;;;


(define (jazz:feature-satisfied? feature-requirement)
  (jazz:conditional-satisfied? 'cond-expand feature-requirement (%%cond-expand-features)))


;;;
;;;; Invoice
;;;


(define (jazz:filter-invoices invoices)
  (define (extract-feature-requirement invoice)
    (if (and (%%pair? invoice)
             (%%not (%%null? (%%cdr invoice)))
             (%%pair? (%%cadr invoice))
             (%%eq? (%%car (%%cadr invoice)) 'cond))
        (%%cadr (%%cadr invoice))
      #f))
  
  (%%apply append
           (map (lambda (invoice)
                  (let ((feature-requirement (extract-feature-requirement invoice)))
                    (cond ((%%not feature-requirement)
                           (%%list invoice))
                          ((jazz:feature-satisfied? feature-requirement)
                           (%%list (%%cons (%%car invoice) (%%cddr invoice))))
                          (else
                           '()))))
                invoices)))


;;;
;;;; Unit
;;;


(define (jazz:expand-unit-source rest)
  (define (parse rest proc)
    (let ((first (jazz:source-code (%%car rest))))
      (if (%%memq first '(protected public))
          (proc (jazz:source-code (%%cadr rest)) first (%%cddr rest))
        (proc (jazz:source-code (%%car rest)) 'public (%%cdr rest)))))
  
  (parse rest
    (lambda (name access body)
      (if (%%not (%%symbol? name))
          (jazz:error "Unit name must be a symbol: {s}" name)
        (if (and (jazz:requested-unit-name) (%%neq? name (jazz:requested-unit-name)))
            (jazz:error "Unit at {s} is defining {s}" (jazz:requested-unit-name) name)
          `(begin
             ,@(jazz:declares 'unit)
             ,@body))))))


;;;
;;;; Simplify
;;;


(define (jazz:simplify-begin form)
  (if (and (%%pair? form)
           (%%eq? (%%car form) 'begin)
           (%%pair? (%%cdr form))
           (%%null? (%%cddr form)))
      (%%cadr form)
    form))


(define (jazz:simplify-let form)
  (if (and (%%pair? form)
           (%%eq? (%%car form) 'let)
           (%%pair? (%%cdr form))
           (%%pair? (%%cddr form)))
      (let ((bindings (%%cadr form))
            (body (%%cddr form)))
        (cond ((and (%%eqv? bindings '())
                    (%%null? (%%cdr body))
                    (let ((expr (%%car body)))
                      ;; inner define needs the let
                      (%%not (and (%%pair? expr)
                                  (%%eq? (%%car expr) 'define)))
                      ;; begin might start with a define needing the let
                      (%%not (and (%%pair? expr)
                                  (%%eq? (%%car expr) 'begin)))))
               (%%car body))
              ((and (%%pair? bindings)
                    (%%null? (%%cdr bindings))
                    (let ((binding (%%car bindings)))
                      (and (%%pair? binding)
                           (%%pair? (%%cdr binding))
                           (%%null? (%%cddr binding))
                           (%%symbol? (%%car binding))
                           (eq? (%%car binding) (%%car body)))))
               (%%car (%%cdar bindings)))
              (else
               form)))
    form))


;;;
;;;; Require
;;;


(define (jazz:expand-require rest)
  (jazz:simplify-begin
    `(begin
       ,@(map (lambda (require)
                (jazz:parse-require (jazz:listify require)
                  (lambda (unit-name feature-requirement phase)
                    #; ;; buggy
                    (if (%%eq? phase 'syntax)
                        (jazz:load-unit unit-name))
                    `(jazz:load-unit ',unit-name))))
              (jazz:filter-invoices (map (lambda (src) (%%desourcify src)) rest))))))


(define (jazz:parse-require require proc)
  (let ((name (%%car require))
        (scan (%%cdr require))
        (feature-requirement #f)
        (phase 'runtime))
    (if (and (%%pair? scan)
             (%%pair? (%%car scan))
             (%%eq? (%%caar scan) 'cond))
        (begin
          (set! feature-requirement (%%car (%%cdar scan)))
          (set! scan (%%cdr scan))))
    (if (and (%%pair? scan)
             (%%pair? (%%car scan))
             (%%eq? (%%caar scan) 'phase))
        (begin
          (set! phase (%%car (%%cdar scan)))
          (set! scan (%%cdr scan))))
    (proc name
          feature-requirement
          phase))))
