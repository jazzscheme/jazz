;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Define Class
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


(unit protected core.class.syntax.define-class


(define jazz:class-info
  (%%make-table test: eq?))


(jazz:define-macro (jazz:define-class name ascendant-name class-options slots)
  (let* ((metaclass-name (jazz:getf class-options metaclass: #t))
         (constructor (jazz:getf class-options constructor:))
         (metaclass-accessor (cond ((%%not metaclass-name) #f) ((%%eq? metaclass-name #t) 'jazz:Object-Class) (else metaclass-name)))
         (ascendant-accessor (if (%%null? ascendant-name) #f ascendant-name))
         (inherited-slot-names (if (%%null? ascendant-name) '() (%%table-ref jazz:class-info ascendant-name)))
         (ascendant-size (%%length inherited-slot-names))
         (slot-names (map car slots))
         (all-slot-names (%%append inherited-slot-names slot-names))
         (all-variables (map (lambda (slot-name) (jazz:generate-symbol (%%symbol->string slot-name))) all-slot-names))
         (all-length (%%length all-slot-names))
         (instance-size (%%fx+ jazz:object-size all-length))
         (class-level-name (%%compose-helper name 'core-level)))
    (%%table-set! jazz:class-info name all-slot-names)
    `(begin
       ;; this is necessary as the getter/setter type assertions will refer to
       ;; the class that is only defined later in the runtime implementation file
       ,@(if jazz:debug-core?
             `((jazz:define-variable ,name))
           '())
       ,@(if (%%not constructor)
             '()
           `((jazz:define-macro (,constructor class ,@all-variables)
               (%%list '%%object class ,@all-variables))))
       ,@(map (lambda (slot rank)
                (let ((slot-name (%%car slot))
                      (slot-getter (%%cadr slot))
                      (slot-setter (%%car (%%cddr slot))))
                  `(begin
                     ,@(cond ((%%null? slot-getter)
                              '())
                             (jazz:debug-core?
                               `((define (,slot-getter object)
                                   (%%core-assertion (jazz:object-of-class? object ,name) (jazz:expected-error ,name object)
                                     (%%object-ref object ,rank)))))
                             ((jazz:string-starts-with? (%%symbol->string slot-getter) "jazz:")
                              `((define (,slot-getter object)
                                  (%%object-ref object ,rank))))
                             (else
                              `((jazz:define-macro (,slot-getter object)
                                  (%%list '%%object-ref object ,rank)))))
                     ,@(cond ((%%null? slot-setter)
                              '())
                             (jazz:debug-core?
                               `((define (,slot-setter object value)
                                   (%%core-assertion (jazz:object-of-class? object ,name) (jazz:expected-error ,name object)
                                     (%%object-set! object ,rank value)))))
                             ((jazz:string-starts-with? (%%symbol->string slot-setter) "jazz:")
                              `((define (,slot-setter object value)
                                  (%%object-set! object ,rank value))))
                             (else
                              `((jazz:define-macro (,slot-setter object value)
                                  (%%list '%%object-set! object ,rank value))))))))
              slots
              (jazz:naturals (%%fx+ jazz:object-size ascendant-size) instance-size))
       (%%table-set! jazz:class-info ',name ',all-slot-names)
       (jazz:define-macro (,(jazz:define-class-runtime-helper name))
         `(begin
            (define ,',name
              (jazz:new-core-class ,',metaclass-accessor ',',name (%%make-table test: eq?) ,',ascendant-accessor ',',slot-names ,',instance-size))
            (define ,',class-level-name
              (%%get-class-level ,',name))
            (jazz:set-core-class ',',(jazz:reference-name name) ,',name)
            (jazz:validate-inherited-slots ',',name ,',ascendant-accessor ',',inherited-slot-names))))))


(jazz:define-macro (jazz:define-class-runtime name)
  `(,(jazz:define-class-runtime-helper name)))


(define (jazz:define-class-runtime-helper name)
  (%%compose-helper name 'runtime)))
