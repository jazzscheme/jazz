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


(library protected jazz.dialect.syntax.attributes scheme


(import (jazz.dialect.kernel)
        (jazz.dialect.syntax.macros))


(native jazz.getf)
(native jazz.system-format)


; @macro
; (attributes (nullable?)
;             (key      initialize #f)
;             (criteria initialize #f))


; @expansion
; (begin
;   (definition (node-properties)
;     (quote
;       (nullable?:
;        key:
;        criteria:)))
;   (slot key initialize #f)
;   (slot criteria initialize #f)
;   (method (get-value property)
;           (case property
;             ((key:) key)
;             ((criteria:) criteria)
;             (else (nextmethod property))))
;   (method (set-value property value)
;           (case property
;             ((key:) (set! key value))
;             ((criteria:) (set! criteria value))
;             (else (nextmethod property value))))
;   (begin
;     (method public (get-key)
;       key)
;     (method public (set-key value)
;       (set-property key: value)))
;   (begin
;     (method public (get-criteria)
;       criteria)
;     (method public (set-criteria value)
;       (set-property criteria: value))))


(macro public (attributes . form)
  (define unspecified
    (cons #f #f))
  
  (define (symbol->keyword symbol)
    (string->keyword (symbol->string symbol)))
  
  (let ((inherited (car form))
        (properties (cdr form))
        (property (generate-symbol "prop"))
        (value (generate-symbol "val")))
    `(begin
       (method (node-properties)
         ',(map symbol->keyword (append inherited (map car properties))))
       ,@(map (lambda (property)
                (let ((name (car property)))
                  (parse-specifier (cdr property)
                    (lambda (specifier rest)
                      (let ((init (getf rest 'initialize not-found: unspecified)))
                        (if (eq? init unspecified)
                            `(slot ,name)
                          `(slot ,name initialize ,init)))))))
              properties)
       (method (get-value ,property)
         (case ,property
           ,@(map (lambda (property)
                    (let ((name (car property)))
                      (list (list (symbol->keyword name)) name)))
                  properties)
           (else (nextmethod ,property))))
       (method (set-value ,property ,value)
         (case ,property
           ,@(map (lambda (property)
                    (let ((name (car property)))
                      (list (list (symbol->keyword name)) (list 'set! name value))))
                  properties)
           (else (nextmethod ,property ,value))))
       ,@(map (lambda (property)
                (let* ((name (car property))
                       (getter (string->symbol (system-format "get-{a}" name)))
                       (setter (string->symbol (system-format "set-{a}" name))))
                  `(begin
                     (method public (,getter)
                       ,name)
                     (method public (,setter ,value)
                       (set-property ,(symbol->keyword name) ,value)))))
              properties)))))
