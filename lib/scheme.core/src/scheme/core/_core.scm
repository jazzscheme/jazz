;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Scheme Core
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
;;;  Portions created by the Initial Developer are Copyright (C) 1996-2012
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


(module scheme.core scheme


(require (core.class))


(export (scheme.core.kernel))
(import (scheme.core.kernel)
        (scheme.syntax (phase syntax)))


;;;
;;;; Native
;;;


(native private jazz:naturals)
(native private jazz:Object-Class)


;;;
;;;; Class
;;;


(define-macro (define-class name ascendant-name inherited-slot-names class-name constructor slots)
  (let* ((class-accessor (if (null? class-name) #f class-name))
         (ascendant-accessor (if (null? ascendant-name) #f ascendant-name))
         (ascendant-size (length inherited-slot-names))
         (slot-names (map car slots))
         (all-slot-names (append inherited-slot-names slot-names))
         (all-variables (map (lambda (slot-name) (generate-symbol (symbol->string slot-name))) all-slot-names))
         (all-length (length all-slot-names))
         (instance-size (+ object-size all-length))
         (class-level-name (compose-helper name 'core-level)))
    `(begin
       ,@(if (null? constructor)
             '()
           `((export ,constructor)
             (define ,constructor new-object)))
       ,@(map (lambda (slot rank)
                (let ((slot-name (car slot))
                      (slot-getter (cadr slot))
                      (slot-setter (car (cddr slot))))
                  `(begin
                     ,@(if (null? slot-getter)
                           '()
                         `((export ,slot-getter)
                           (define (,slot-getter object)
                             (get-object-slot object ,rank))))
                     ,@(if (null? slot-setter)
                           '()
                         `((export ,slot-setter)
                           (define (,slot-setter object value)
                             (set-object-slot object ,rank value)))))))
              slots
              (naturals (+ object-size ascendant-size) instance-size))
       (define ,name
         (new-core-class ,class-accessor ',name (make-table test: eq?) ,ascendant-accessor ',slot-names ,instance-size))
       (define ,class-level-name
         (get-class-level ,name))
       (set-core-class ',(reference-name name) ,name))))


;;;
;;;; Method
;;;


(define-macro (define-method signature . body)
  (let* ((name (car signature))
         (parameters (cdr signature))
         (class-name (caar parameters))
         (object-parameter (cadr (car parameters)))
         (extra-parameters (cdr parameters))
         (implementation-name (method-implementation-name class-name name)))
    `(begin
       (define ,implementation-name
         (let ((nextmethod (find-nextmethod ,class-name ',name)))
           (lambda (,object-parameter ,@extra-parameters)
             ,@body)))
       (add-core-method-node ,class-name ',name ,implementation-name)))))
