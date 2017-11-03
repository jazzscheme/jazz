;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Attributes
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
;;;  Portions created by the Initial Developer are Copyright (C) 1996-2015
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


(module protected jazz.language.syntax.attributes gambit


(export attributes)

(import (jazz.language.runtime.kernel))


(native private jazz:getf)
(native private jazz:system-format)


; @macro
; (attributes (nullable?)
;   (slot key      initialize #f)
;   (slot criteria initialize #f))


; @expansion
; (begin
;   (slot key initialize #f)
;   (slot criteria initialize #f)
;   (method override (get-attributes self)
;     '(nullable?
;       key
;       criteria))
;   (method override (get self attribute)
;     (case attribute
;       ((key) key)
;       ((criteria) criteria)
;       (else (nextmethod self attribute))))
;   (method override (set self attribute value)
;     (case attribute
;       ((key) (set! key value))
;       ((criteria) (set! criteria value))
;       (else (nextmethod self attribute value))))
;   (begin
;     (method public (get-key self)
;       ...)
;     (method public (set-key self value)
;       (set! key value)))
;   (begin
;     (method public (get-criteria self)
;       ...)
;     (method public (set-criteria self value)
;       (set! criteria value))))


(define-macro (attributes . form)
  (define (slot-name attribute)
    (string->symbol (string-append "_" (symbol->string (cadr attribute)))))
  
  (let ((inherited (car form))
        (attributes (cdr form))
        (attribute (generate-symbol "attr"))
        (value (generate-symbol "val")))
    `(begin
       #;
       ,@(expand-marshalling form)
       ,@(map (lambda (attribute)
                (let ((slot-name (slot-name attribute)))
                  `(slot ,slot-name)))
              attributes)
       (method override (get-attributes self)
         ',(append inherited (map cadr attributes)))
       (method override (get-attribute self ,attribute)
         (case ,attribute
           ,@(map (lambda (attribute)
                    (let ((name (cadr attribute))
                          (slot-name (slot-name attribute)))
                      `((,name) ,slot-name)))
                  attributes)
           (else (nextmethod self ,attribute))))
       (method override (set-attribute self ,attribute ,value)
         (case ,attribute
           ,@(map (lambda (attribute)
                    (let ((name (cadr attribute))
                          (slot-name (slot-name attribute)))
                      `((,name) (set! ,slot-name ,value))))
                  attributes)
           (else (nextmethod self ,attribute ,value))))
       (method override (get-attribute-default self ,attribute)
         (case ,attribute
           ,@(map (lambda (attribute)
                    (let ((name (cadr attribute)))
                      (parse-specifier (cddr attribute)
                        (lambda (specifier rest)
                          (let ((initialize (getf rest 'initialize '(unspecified))))
                            `((,name) ,initialize))))))
                  attributes)
           (else (nextmethod self ,attribute))))
       (method override (get-attribute-no-default self ,attribute)
         (let ((,value (get-attribute self ,attribute)))
           (if (specified? ,value)
               ,value
             (let (iterate (scan (get-ascendants self)))
               (if (not-null? scan)
                   (let ((,value (get-attribute-no-default (car scan) ,attribute)))
                     (if (specified? ,value)
                         ,value
                       (iterate (cdr scan))))
                 (unspecified))))))
       (method override (attribute=? self ,attribute x y)
         (case ,attribute
           ,@(let ((clauses (new-queue)))
               (for-each (lambda (attribute)
                           (let ((name (cadr attribute)))
                             (parse-specifier (cddr attribute)
                               (lambda (specifier rest)
                                 (let ((test (getf rest 'test)))
                                   (if test
                                       (enqueue clauses `((,name) (,test x y)))))))))
                         attributes)
               (queue-list clauses))
           (else (nextmethod self ,attribute x y))))
       (method override (get self ,attribute)
         (case ,attribute
           ,@(map (lambda (attribute)
                    (let ((name (cadr attribute)))
                      (let ((getter (string->symbol (system-format "get-{a}" name))))
                        `((,name) (,getter self)))))
                  attributes)
           (else (nextmethod self ,attribute))))
       (method override (set self ,attribute ,value)
         (case ,attribute
           ,@(map (lambda (attribute)
                    (let ((name (cadr attribute))
                          (slot-name (slot-name attribute)))
                      `((,name) (set! ,slot-name ,value))))
                  attributes)
           (else (nextmethod self ,attribute ,value))))
       ,@(map (lambda (attribute)
                (let ((name (cadr attribute))
                      (slot-name (slot-name attribute)))
                  (parse-specifier (cddr attribute)
                    (lambda (specifier rest)
                      (let ((getter (string->symbol (system-format "get-{a}" name)))
                            (setter (string->symbol (system-format "set-{a}" name))))
                        `(begin
                           (method public (,getter self) ,@(if specifier (list specifier) '())
                             (let ((,value (get-attribute-no-default self ',name)))
                               (if (specified? ,value)
                                   ,value
                                 (get-attribute-default self ',name))))
                           (method public (,setter self ,value ,@(if specifier (list specifier) '()))
                             (set! ,slot-name ,value))))))))
              attributes)))))
