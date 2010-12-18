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


(module protected jazz.dialect.syntax.attributes scheme


(import (jazz.dialect.kernel))


(native private jazz.getf)
(native private jazz.system-format)


; @macro
; (attributes (nullable?)
;   (key      initialize #f)
;   (criteria initialize #f))


; @expansion
; (begin
;   (slot key initialize #f)
;   (slot criteria initialize #f)
;   (method override (get-attributes)
;     '(nullable?
;       key
;       criteria))
;   (method override (get attribute)
;     (case attribute
;       ((key) key)
;       ((criteria) criteria)
;       (else (nextmethod attribute))))
;   (method override (set attribute value)
;     (case attribute
;       ((key) (set! key value))
;       ((criteria) (set! criteria value))
;       (else (nextmethod attribute value))))
;   (begin
;     (method public (get-key)
;       ...)
;     (method public (set-key value)
;       (set! key value)))
;   (begin
;     (method public (get-criteria)
;       ...)
;     (method public (set-criteria value)
;       (set! criteria value))))


(macro public (attributes . form)
  (define (slot-name attribute)
    (string->symbol (string-append "_" (symbol->string (car attribute)))))
  
  (let ((inherited (car form))
        (attributes (cdr form))
        (attribute (generate-symbol "attr"))
        (value (generate-symbol "val")))
    `(begin
       ,@(map (lambda (attribute)
                (let ((slot-name (slot-name attribute)))
                  `(slot ,slot-name)))
              attributes)
       (method override (get-attributes)
         ',(append inherited (map car attributes)))
       (method override (get-attribute ,attribute)
         (case ,attribute
           ,@(map (lambda (attribute)
                    (let ((name (car attribute))
                          (slot-name (slot-name attribute)))
                      `((,name) ,slot-name)))
                  attributes)
           (else (nextmethod ,attribute))))
       (method override (set-attribute ,attribute ,value)
         (case ,attribute
           ,@(map (lambda (attribute)
                    (let ((name (car attribute))
                          (slot-name (slot-name attribute)))
                      `((,name) (set! ,slot-name ,value))))
                  attributes)
           (else (nextmethod ,attribute ,value))))
       (method override (get-attribute-default ,attribute)
         (case ,attribute
           ,@(map (lambda (attribute)
                    (let ((name (car attribute)))
                      (parse-specifier (cdr attribute)
                        (lambda (specifier rest)
                          (let ((initialize (getf rest 'initialize not-found: '(unspecified))))
                            `((,name) ,initialize))))))
                  attributes)
           (else (nextmethod ,attribute))))
       (method override (get-attribute-no-default ,attribute)
         (let ((,value (get-attribute ,attribute)))
           (if (specified? ,value)
               ,value
             (let (iterate (scan (get-ascendants)))
               (if (not-null? scan)
                   (let ((,value (get-attribute-no-default~ (car scan) ,attribute)))
                     (if (specified? ,value)
                         ,value
                       (iterate (cdr scan))))
                 (unspecified))))))
       (method override (attribute=? ,attribute x y)
         (case ,attribute
           ,@(let ((clauses (new-queue)))
               (for-each (lambda (attribute)
                           (let ((name (car attribute)))
                             (parse-specifier (cdr attribute)
                               (lambda (specifier rest)
                                 (let ((test (getf rest 'test)))
                                   (if test
                                       (enqueue clauses `((,name) (,test x y)))))))))
                         attributes)
               (queue-list clauses))
           (else (nextmethod ,attribute x y))))
       (method override (get ,attribute)
         (case ,attribute
           ,@(map (lambda (attribute)
                    (let ((name (car attribute)))
                      (let ((getter (string->symbol (system-format "get-{a}" name))))
                        `((,name) (,getter)))))
                  attributes)
           (else (nextmethod ,attribute))))
       (method override (set ,attribute ,value)
         (case ,attribute
           ,@(map (lambda (attribute)
                    (let ((name (car attribute))
                          (slot-name (slot-name attribute)))
                      `((,name) (set! ,slot-name ,value))))
                  attributes)
           (else (nextmethod ,attribute ,value))))
       ,@(map (lambda (attribute)
                (let ((name (car attribute))
                      (slot-name (slot-name attribute)))
                  (let ((getter (string->symbol (system-format "get-{a}" name)))
                        (setter (string->symbol (system-format "set-{a}" name))))
                    `(begin
                       (method public (,getter)
                         (let ((,value (get-attribute-no-default ',name)))
                           (if (specified? ,value)
                               ,value
                             (get-attribute-default ',name))))
                       (method public (,setter ,value)
                         (set! ,slot-name ,value))))))
              attributes)))))
