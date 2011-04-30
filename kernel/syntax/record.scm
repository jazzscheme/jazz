;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Records
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


(block kernel.record


(jazz:kernel-declares)


(define jazz:class-info
  (make-table test: eq?))


(jazz:define-macro (jazz:define-record name ascendant-name class-options slots)
  (define (downcase str)
    (list->string (map char-downcase (string->list str))))
  
  (define (standardize-slot slot)
    (if (%%symbol? slot)
        (%%list slot)
      slot))
  
  (define (slot-name slot)
    (%%car slot))
  
  (define (slot-getter slot prefix downcase-name)
    (define (generate-getter)
      (%%string->symbol (%%string-append prefix "get-" downcase-name "-" (%%symbol->string (slot-name slot)))))
    
    (let ((getter (jazz:getf (%%cdr slot) getter: #f)))
      (if (%%not getter)
          (let ((accessors (jazz:getf (%%cdr slot) accessors: #f)))
            (if (%%not accessors)
                (generate-getter)
              (if (%%eq? accessors #t)
                  #f
                (generate-getter))))
        (if (%%eq? getter #t)
            #f
          (if (%%eq? getter 'generate)
              (generate-getter)
            getter)))))
  
  (define (slot-setter slot prefix downcase-name)
    (define (generate-setter)
      (%%string->symbol (%%string-append prefix "set-" downcase-name "-" (%%symbol->string (slot-name slot)))))
    
    (let ((setter (jazz:getf (%%cdr slot) setter: #f)))
      (if (%%not setter)
          (let ((accessors (jazz:getf (%%cdr slot) accessors: #f)))
            (if (%%not accessors)
                (generate-setter)
              (if (%%eq? accessors #t)
                  #f
                (generate-setter))))
        (if (%%eq? setter #t)
            #f
          (if (%%eq? setter 'generate)
              (generate-setter)
            setter)))))
  
  (define (parse-slot slot accessors-prefix downcase-name)
    (let ((slot (standardize-slot slot)))
      (values (%%car slot)
              (slot-getter slot accessors-prefix downcase-name)
              (slot-setter slot accessors-prefix downcase-name))))
  
  (let* ((downcase-name (downcase (%%symbol->string name)))
         (record? (jazz:getf class-options record?: #f))
         (class? (%%not record?))
         (constructor (jazz:getf class-options constructor:))
         (accessors-type (jazz:getf class-options accessors-type: 'function))
         (accessors-prefix (case accessors-type ((macro) "%%") (else "jazz:")))
         (ascendant-accessor (if (%%null? ascendant-name) #f ascendant-name))
         (inherited-slot-names (if (%%null? ascendant-name) '() (%%table-ref jazz:class-info ascendant-name)))
         (ascendant-size (%%length inherited-slot-names))
         (slot-names (map (lambda (slot) (slot-name (standardize-slot slot))) slots))
         (all-slot-names (%%append inherited-slot-names slot-names))
         (all-length (%%length all-slot-names))
         (instance-size (%%fx+ jazz:record-size all-length)))
    (%%table-set! jazz:class-info name all-slot-names)
    `(begin
       ,@(if (%%not constructor)
             '()
           `((jazz:define-macro (,constructor ,@all-slot-names)
               (%%list '%%object '',name ,@all-slot-names))))
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
              (jazz:naturals (%%fx+ jazz:record-size ascendant-size) instance-size))
       (%%table-set! jazz:class-info ',name ',all-slot-names)))))
