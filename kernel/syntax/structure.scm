;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Structures
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


(block kernel.structure


(define jazz:structure-info
  (%%make-table test: eq?))


(jazz:define-macro (jazz:define-structure name ascendant-name options slots)
  (define (downcase str)
    (let ((down (list->string (map char-downcase (string->list str)))))
      (if (jazz:string-starts-with? down "jazz:")
          (%%substring down 5 (%%string-length down))
        down)))
  
  (define (standardize-slot slot)
    (if (%%symbol? slot)
        (%%list slot)
      slot))
  
  (define (parse-slot slot prefix downcase-name)
    (define (slot-getter slot)
      (parse-accessor slot getter: "get-"))
    
    (define (slot-setter slot)
      (parse-accessor slot setter: "set-"))
    
    (define (parse-accessor slot accessor-key accessor-prefix)
      (define (generate-accessor)
        (%%string->symbol (%%string-append prefix accessor-prefix downcase-name "-" (%%symbol->string (%%car slot)))))
      
      (let ((accessor (or (jazz:getf (%%cdr slot) accessor-key #f)
                          (jazz:getf (%%cdr slot) accessors: #f))))
        (cond ((%%eq? accessor #t)
               #f)
              ((%%eq? accessor 'generate)
               (generate-accessor))
              (else
               accessor))))
    
    (let ((slot (standardize-slot slot)))
      (values (slot-name slot)
              (slot-getter slot)
              (slot-setter slot))))
  
  (define (slot-name slot)
    (%%car slot))
  
  (let* ((downcase-name (downcase (%%symbol->string name)))
         (constructor (jazz:getf options constructor: #f))
         (constructor-class? (jazz:getf options constructor-class?: #f))
         (predicate (jazz:getf options predicate: #f))
         (accessors-type (jazz:getf options accessors-type: 'function))
         (accessors-prefix (case accessors-type ((macro) "%%") (else "jazz:")))
         (ascendant-accessor (if (%%null? ascendant-name) #f ascendant-name))
         (inherited-slot-names (if (%%null? ascendant-name) '() (%%table-ref jazz:structure-info ascendant-name)))
         (ascendant-size (%%length inherited-slot-names))
         (slot-names (map (lambda (slot) (slot-name (standardize-slot slot))) slots))
         (all-slot-names (%%append inherited-slot-names slot-names))
         (all-length (%%length all-slot-names))
         (record-size (%%fx+ jazz:record-offset all-length)))
    (%%table-set! jazz:structure-info name all-slot-names)
    `(begin
       ,@(if (%%not constructor)
             '()
           (if constructor-class?
               `((define (,constructor class ,@all-slot-names)
                   (%%class-record class ,@all-slot-names)))
             `((define (,constructor ,@all-slot-names)
                 (%%record ',name ,@all-slot-names)))))
       ,@(if (%%not predicate)
             '()
           `((define (,predicate obj)
               (and (%%record? obj) (%%eq? (%%record-structure-name obj) ',name)))))
       ,@(map (lambda (slot rank)
                (receive (slot-name slot-getter slot-setter) (parse-slot slot accessors-prefix downcase-name)
                  `(begin
                     ,@(cond ((%%not slot-getter)
                              '())
                             (else
                              (case accessors-type
                                ((macro)
                                 `((jazz:define-macro (,slot-getter record)
                                     (%%list '%%record-ref record ,rank))))
                                (else
                                 `((define (,slot-getter record)
                                     (%%record-ref record ,rank)))))))
                     ,@(cond ((%%not slot-setter)
                              '())
                             (else
                              (case accessors-type
                                ((macro)
                                 `((jazz:define-macro (,slot-setter record value)
                                     (%%list '%%record-set! record ,rank value))))
                                (else
                                 `((define (,slot-setter record value)
                                     (%%record-set! record ,rank value))))))))))
              slots
              (jazz:naturals (%%fx+ jazz:record-offset ascendant-size) record-size))
       (%%table-set! jazz:structure-info ',name ',all-slot-names)))))
