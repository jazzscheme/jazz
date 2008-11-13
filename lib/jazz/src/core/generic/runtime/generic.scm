;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Generic Methods
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
;;;  The Initial Developer of the Original Code is Stephane Le Cornec.
;;;  Portions created by the Initial Developer are Copyright (C) 1996-2008
;;;  the Initial Developer. All Rights Reserved.
;;;
;;;  Contributor(s):
;;;    Guillaume Cartier
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


(module core.generic.runtime.generic


(define (jazz.new-generic locator dynamic-signature root-proc)
  (let* ((name (jazz.identifier-name locator))
         (generic (jazz.allocate-generic jazz.Generic locator name #f '())))
    (jazz.generic-make-root generic dynamic-signature root-proc)
    generic))


(define (jazz.generic-reset generic root-proc)
  (jazz.generic-make-root generic (%%get-specific-dynamic-signature (%%get-generic-root-specific generic)) root-proc))


(define (jazz.generic-make-root generic dynamic-signature root-proc)
  (let ((root-specific (jazz.new-specific dynamic-signature (or root-proc (lambda rest (apply jazz.invalid-generic-call generic rest))))))
    (%%set-generic-root-specific generic root-specific)
    (%%set-generic-pending-specifics generic (%%cons root-specific (%%get-generic-pending-specifics generic)))))


(define (jazz.invalid-generic-call generic . rest)
  (let ((dynamic-parameters (let iter ((signature (%%get-specific-dynamic-signature (%%get-generic-root-specific generic)))
                                       (rest rest))
                                 (if (and (%%pair? signature) (%%pair? rest))
                                     (%%cons (%%class-of (%%car rest))
                                             (iter (%%cdr signature) (%%cdr rest)))
                                   '()))))
    (jazz.generic-error generic dynamic-parameters)))


(define (jazz.generic-error generic signature)
  (let ((name (%%get-generic-name generic))
        (root-signature (%%get-specific-dynamic-signature (%%get-generic-root-specific generic))))
    (jazz.error "Generic {a} cannot take {a}" (%%cons name root-signature) signature)))



;;;
;;;; Register
;;;


(define (jazz.register-specific generic specific)
  (%%set-generic-pending-specifics generic (%%cons specific (%%get-generic-pending-specifics generic))))


;;;
;;;; Update
;;;


(define (jazz.process-pending-specifics generic)
  ;; root must be done first - already present in the hierarchy
  (jazz.resolve-signature (%%get-generic-root-specific generic))
  (for-each (lambda (specific)
              (jazz.resolve-signature specific)
              (jazz.insert/replace-specific generic specific))
            (%%get-generic-pending-specifics generic))
  (%%set-generic-pending-specifics generic '()))


(define (jazz.resolve-signature specific)
  (let ((signature/proc (%%get-specific-dynamic-signature specific)))
    (%%when (%%procedure? signature/proc)
      (%%set-specific-dynamic-signature specific (signature/proc)))))


(define (jazz.insert/replace-specific generic specific)
  (define (put-best-first specifics)
    (let iter ((scan (%%cdr specifics))
               (best (%%car specifics))
               (others '()))
         (if (%%pair? scan)
             (let ((specific (%%car scan)))
               (if (jazz.specific-better? specific best)
                   (iter (%%cdr scan) specific (%%cons best others))
                 (iter (%%cdr scan) best (%%cons specific others))))
           (%%cons best others))))
  (let* ((dynamic-signature (%%get-specific-dynamic-signature specific))
         (matches (jazz.gather-dynamic-signature-ancestors generic dynamic-signature)))
    (cond ((%%not matches)
           (jazz.generic-error generic dynamic-signature))
          ((%%pair? matches)
           ;; new node - replace ancestor/descendant links with ancestor/specific specific/descendant links
           ;; new node - keep nextmethod specific as first ancestor
           (let ((ancestors (put-best-first matches))
                 (descendant-specifics '()))
             (%%set-specific-ancestor-specifics specific ancestors)
             (for-each (lambda (ancestor)
                         (let* ((ancestor-signature (%%get-specific-dynamic-signature ancestor))
                                (partition (jazz.partition (%%get-specific-descendant-specifics ancestor)
                                                           (lambda (descendant)
                                                             (let ((descendant-signature (%%get-specific-dynamic-signature descendant)))
                                                               (%%eq? 'ordered (jazz.dynamic-signature-compare descendant-signature dynamic-signature))))
                                                           assv))
                                (descendant-partition (assq #t partition))
                                (descendants (if descendant-partition (cdr descendant-partition) '()))
                                (brother-partition (assq #f partition))
                                (brothers (if brother-partition (cdr brother-partition) '())))
                           (for-each (lambda (descendant)
                                       (let ((best (%%car (%%get-specific-ancestor-specifics descendant))))
                                         (cond ((%%eq? ancestor best)
                                                (%%set-specific-ancestor-specifics descendant
                                                                                   (%%cons specific (%%cdr (%%get-specific-ancestor-specifics descendant)))))
                                               ((jazz.specific-better? specific best)
                                                (%%set-specific-ancestor-specifics descendant
                                                                                   (%%cons specific (jazz.remove! ancestor (%%get-specific-ancestor-specifics descendant)))))
                                               (else
                                                (%%set-specific-ancestor-specifics descendant
                                                                                   (%%cons best (%%cons specific (jazz.remove! ancestor (%%cdr (%%get-specific-ancestor-specifics descendant)))))))))
                                       (%%when (%%not (%%memq descendant descendant-specifics))
                                         (set! descendant-specifics (%%cons descendant descendant-specifics))))
                                     descendants)
                           (%%set-specific-descendant-specifics ancestor (%%cons specific brothers))))
                       ancestors)
             (%%set-specific-descendant-specifics specific descendant-specifics)))
          (else
           ;; node already exists - since lexical nextmethod uses new specific we cannot just copy implementation into old node
           (let ((perfect-match matches))
             (%%when (%%eq? perfect-match (%%get-generic-root-specific generic))
               (%%set-generic-root-specific generic specific))
             (let ((ancestors (%%get-specific-ancestor-specifics perfect-match)))
               (%%set-specific-ancestor-specifics specific ancestors)
               (let iter ((ancestors ancestors))
                    (if (%%pair? ancestors)
                        (if (%%eq? perfect-match (%%car ancestors))
                            (%%set-car! ancestors specific)
                          (iter (%%cdr ancestors))))))
             (let ((descendants (%%get-specific-descendant-specifics perfect-match)))
               (%%set-specific-descendant-specifics specific descendants)
               (let iter ((descendants descendants))
                    (if (%%pair? descendants)
                        (if (%%eq? perfect-match (%%car descendants))
                            (%%set-car! descendants specific)
                          (iter (%%cdr descendants)))))))))))


;;;
;;;; Compare
;;;


(define (jazz.gather-dynamic-signature-ancestors generic dynamic-signature)
  (let ((perfect-match #f))
    (or (let iter ((specifics (list (%%get-generic-root-specific generic)))
                   (partial-matches '()))
             (if (%%pair? specifics)
                 (let ((specific (%%car specifics)))
                   (case (jazz.dynamic-signature-compare dynamic-signature (%%get-specific-dynamic-signature specific))
                     ((equal)
                      (set! perfect-match specific)
                      #f)
                     ((ordered)
                      (let ((found-in-descendants (iter (%%get-specific-descendant-specifics specific) partial-matches)))
                        (if perfect-match
                            #f
                          (iter (%%cdr specifics) (or found-in-descendants
                                                      (if (%%memq specific partial-matches)
                                                          partial-matches
                                                        (%%cons specific partial-matches)))))))
                     (else
                      (iter (%%cdr specifics) partial-matches))))
               (if (%%null? partial-matches)
                   #f
                 partial-matches)))
        perfect-match)))


(define (jazz.dynamic-signature-compare descendant-signature ancestor-signature)
  (let iter ((descendant-signature descendant-signature)
             (ancestor-signature ancestor-signature)
             (match 'equal))
       (if (or (null? descendant-signature) (null? ancestor-signature))
           (if (and (null? descendant-signature) (null? ancestor-signature))
               match
             'unordered)
         (cond ((%%eq? (%%car descendant-signature) (%%car ancestor-signature))
                (iter (%%cdr descendant-signature) (%%cdr ancestor-signature) match))
               ((jazz.subcategory? (%%car descendant-signature) (%%car ancestor-signature))
                (if (%%eq? match 'reverse-ordered)
                    'unordered
                  (iter (%%cdr descendant-signature) (%%cdr ancestor-signature) 'ordered)))
               ((jazz.subcategory? (%%car ancestor-signature) (%%car descendant-signature))
                (if (%%eq? match 'ordered)
                    'unordered
                  (iter (%%cdr descendant-signature) (%%cdr ancestor-signature) 'reverse-ordered)))
               (else
                'unordered)))))

;; not used
(define (jazz.dynamic-signature-equal? dynamic-signature1 dynamic-signature2)
  (let iter ((dynamic-signature1 dynamic-signature1)
             (dynamic-signature2 dynamic-signature2))
       (if (or (null? dynamic-signature1) (null? dynamic-signature2))
           (and (null? dynamic-signature1) (null? dynamic-signature2))
         (and (%%eq? (%%car dynamic-signature1) (%%car dynamic-signature2))
              (iter (%%cdr dynamic-signature1) (%%cdr dynamic-signature2))))))


(define (jazz.specific-better? specific1 specific2)
  (let iter ((signature1 (%%get-specific-dynamic-signature specific1))
             (signature2 (%%get-specific-dynamic-signature specific2)))
       (or (> (%%get-class-level (%%car signature1)) (%%get-class-level (%%car signature2)))
           (iter (%%cdr signature1) (%%cdr signature2)))))


;;;
;;;; Debug
;;;


(define (jazz.display-tree generic)
  (%%when (%%not (%%null? (%%get-generic-pending-specifics generic)))
    (jazz.process-pending-specifics generic))
  (let iterate ((specifics (list (%%get-generic-root-specific generic)))
                (level 0))
       (for-each (lambda (specific)
                   (write (%%list level
                                  specific
                                  (%%get-specific-dynamic-signature specific)
                                  (%%get-specific-ancestor-specifics specific)
                                  (%%get-specific-descendant-specifics specific)))
                   (newline))
                 specifics)
       (for-each (lambda (specific)
                   (write (%%list level specific))
                   (newline)
                   (iterate (%%get-specific-descendant-specifics specific) (+ level 1)))
                 specifics)))


;;;
;;;; Dispatch
;;;


(define (jazz.dispatch-from-root generic dynamic-classes)
  (let ((matches (jazz.gather-dynamic-signature-ancestors generic dynamic-classes)))
    (cond ((%%not matches)
           (jazz.generic-error generic dynamic-classes))
          ((%%pair? matches)
           (%%car matches))
          (else
           matches)))))
