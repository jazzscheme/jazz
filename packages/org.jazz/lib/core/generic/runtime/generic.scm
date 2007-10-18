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
;;;  Portions created by the Initial Developer are Copyright (C) 1996-2006
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


(define (jazz.new-generic locator mandatory-parameters signature-proc)
  (let ((name (jazz.identifier-name locator)))
    (let ((root-specific (jazz.new-specific #f signature-proc (lambda rest (jazz.error "No specific method to call {a} on {s}" name rest)))))
      (jazz.allocate-generic jazz.Generic locator name mandatory-parameters root-specific '()))))


;;;
;;;; Register
;;;


(define (jazz.register-specific generic specific)
  (let ((g (%%get-generic-mandatory-parameters generic))
        (s (%%get-specific-mandatory-parameters specific))
        (name (%%get-generic-name generic)))
    (%%assertion (or ;; until special treatment for initialize
                     (%%eq? name 'initialize)
                     (and (%%not g) (%%not s))
                     (and g s (%%fx= (%%length g) (%%length s))))
                 (jazz.format "Inconsistant mandatory parameters for {a}: {a} and {a}" name g s)
      (%%set-generic-pending-specifics generic (%%cons specific (%%get-generic-pending-specifics generic))))))


;;;
;;;; Update
;;;


(define (jazz.update-generic generic)
  (jazz.resolve-signature (%%get-generic-root-specific generic))
  (for-each (lambda (specific)
              (jazz.resolve-signature specific)
              (jazz.insert-specific generic specific))
            (%%get-generic-pending-specifics generic))
  (%%set-generic-pending-specifics generic '())
  (jazz.clear-dispatch-tables generic)
  (jazz.update-dispatch-tables generic))


(define (jazz.resolve-signature specific)
  (let ((signature/proc (%%get-specific-signature specific)))
    (%%when (%%procedure? signature/proc)
      (%%set-specific-signature specific (signature/proc)))))


(define (jazz.insert-specific generic specific)
  (letrec ((replace-specific (lambda (replaced-specific)
                               (let ((replace-root? (%%eq? replaced-specific (%%get-generic-root-specific generic)))
                                     (moved-specifics (%%get-specific-previous-specifics replaced-specific))
                                     (next-specific (%%get-specific-next-specific replaced-specific)))
                                 (%%when replace-root?
                                   (%%set-generic-root-specific generic specific))
                                 (%%when next-specific
                                   (%%set-specific-previous-specifics next-specific
                                                                         (%%cons specific (jazz.remove! replaced-specific
                                                                                                      (%%get-specific-previous-specifics next-specific)))))
                                 (common-proc moved-specifics next-specific))))
           (insert-specific (lambda (next-specific)
                              (let* ((partition (jazz.partition (%%get-specific-previous-specifics next-specific)
                                                                (lambda (previous-specific)
                                                                  (jazz.specific-next? specific previous-specific))))
                                     (keep (assq #f partition))
                                     (kept-specifics (if keep (%%cons specific (%%cdr keep)) (%%list specific)))
                                     (move (assq #t partition))
                                     (moved-specifics (if move (%%cdr move) '())))
                                (%%set-specific-previous-specifics next-specific kept-specifics)
                                (common-proc moved-specifics next-specific))))
           (common-proc (lambda (moved-specifics next-specific)
                          (%%set-specific-previous-specifics specific moved-specifics)
                          (%%set-specific-next-specific specific next-specific)
                          (%%when next-specific
                            (%%set-specific-next-implementation specific (%%get-specific-implementation next-specific)))
                          (let ((implementation (%%get-specific-implementation specific)))
                            (for-each (lambda (previous-specific)
                                        (%%set-specific-next-specific previous-specific specific)
                                        (%%set-specific-next-implementation previous-specific implementation))
                                      moved-specifics)))))
    (let ((next-specific (jazz.find-specific-from-root generic specific)))
      (if (jazz.specific-dispatch-equal? next-specific specific)
          (replace-specific next-specific)
        (insert-specific next-specific)))))


(define (jazz.clear-dispatch-tables generic)
  (let* ((name (%%get-generic-name generic))
         (specific (%%get-generic-root-specific generic))
         (class (%%car (%%get-specific-signature specific))))
    (jazz.iterate-descendants-tree class
      (lambda (subclass)
        (%%when (and (jazz.class? subclass)
                     (jazz.class-dispatch-safe subclass name))
          (%%hashtable-set! (%%get-class-dispatch-table subclass) name #f))))))


(define (jazz.update-dispatch-tables generic)
  (let ((name (%%get-generic-name generic)))
    (let iter ((specific (%%get-generic-root-specific generic)))
      (let ((class (%%car (%%get-specific-signature specific))))
        (jazz.update-dispatch-table class name (%%get-specific-implementation specific)))
      (for-each iter
                (%%get-specific-previous-specifics specific)))))


;;;
;;;; Compare
;;;


(define (jazz.find-specific-from-root generic specific)
  (let iter ((loop-specific (%%get-generic-root-specific generic)))
       (let ((found (jazz.find-if
                      (lambda (next-specific)
                        (jazz.specific-next? next-specific specific))
                      (%%get-specific-previous-specifics loop-specific))))
         (if found
             (iter found)
           loop-specific))))


(define (jazz.specific-next? next-specific specific)
  (let ((next-signature (%%get-specific-signature next-specific))
        (signature (%%get-specific-signature specific)))
    (jazz.subcategory? (%%car signature) (%%car next-signature))))


(define (jazz.specific-dispatch-equal? specific1 specific2)
  (let ((signature1 (%%get-specific-signature specific1))
        (signature2 (%%get-specific-signature specific2)))
    (%%eq? (%%car signature1) (%%car signature2))))


;;;
;;;; Dispatch
;;;


(define (jazz.dispatch-from-root class generic)
  (let iter ((previous-found (%%get-generic-root-specific generic)))
       (let ((found (jazz.find-if
                      (lambda (next-specific)
                        (let ((next-signature (%%get-specific-signature next-specific)))
                          (jazz.subcategory? class (%%car next-signature))))
                      (%%get-specific-previous-specifics previous-found))))
         (if found
             (iter found)
           previous-found))))


(define (jazz.dispatch name object)
  (or (%%class-dispatch (%%class-of object) name)
      (jazz.error "Unable to find specific for {s} on {s}" name object)))


(define (jazz.class-dispatch-safe class name)
  (let ((dispatch-table (%%get-class-dispatch-table class)))
    (and dispatch-table
         (%%hashtable-ref dispatch-table name #f)))))
