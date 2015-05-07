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


(unit protected core.class.syntax.define-class


(jazz:define-structure Class-Info () (constructor: jazz:make-class-info)
  ((metaclass-accessor getter: generate)
   (ascendant-accessor getter: generate)
   (constructor        getter: generate)
   (constructor-type   getter: generate)
   (accessors-type     getter: generate)
   (slots              getter: generate)
   (slot-names         getter: generate)
   (all-slot-names     getter: generate)
   (instance-size      getter: generate)))


(define jazz:class-info
  (%%make-table test: eq?))


(jazz:define-macro (jazz:define-class name ascendant-name options slots)
  `(begin
     (jazz:define-class-syntax ,name ,ascendant-name ,options ,slots)
     (jazz:define-class-runtime ,name)))


(jazz:define-macro (jazz:define-class-syntax name ascendant-name options slots)
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
    (define (slot-initialize slot)
      (jazz:getf (%%cdr slot) initialize: (jazz:unspecified)))
    
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
      (%%list (slot-name slot)
              (slot-initialize slot)
              (slot-getter slot)
              (slot-setter slot))))
  
  (define (slot-name slot)
    (%%car slot))
  
  (let* ((downcase-name (downcase (%%symbol->string name)))
         (metaclass-name (jazz:getf options metaclass: #f))
         (constructor (jazz:getf options constructor: #f))
         (constructor-type (jazz:getf options constructor-type: #f))
         (accessors-type (jazz:getf options accessors-type: 'function))
         (accessors-prefix (case accessors-type ((macro) "%%") (else "jazz:")))
         (metaclass-accessor (if (%%not metaclass-name) 'jazz:Object-Class metaclass-name))
         (ascendant-accessor (if (%%null? ascendant-name) #f ascendant-name))
         (ascendant-info (%%table-ref jazz:class-info ascendant-name #f))
         (ascendant-slot-names (if (%%null? ascendant-name) '() (jazz:get-class-info-all-slot-names ascendant-info)))
         (ascendant-size (%%length ascendant-slot-names))
         (slots (map (lambda (slot) (parse-slot slot accessors-prefix downcase-name)) slots))
         (slot-names (map slot-name slots))
         (all-slot-names (%%append ascendant-slot-names slot-names))
         (all-length (%%length all-slot-names))
         (instance-size (%%fx+ jazz:object-size all-length)))
    (%%table-set! jazz:class-info name (jazz:make-class-info metaclass-accessor ascendant-accessor constructor constructor-type accessors-type slots slot-names all-slot-names instance-size))
    `(begin
       ,@(map (lambda (slot rank)
                (jazz:bind (slot-name slot-initialize slot-getter slot-setter) slot
                  `(begin
                     ,@(cond ((%%not slot-getter)
                              '())
                             (else
                              (case accessors-type
                                ((macro)
                                 `((jazz:define-macro (,slot-getter object)
                                     (jazz:with-uniqueness object
                                       (lambda (obj)
                                         `(%%assert-class ,obj ,',name
                                            (%%object-ref ,obj ,,rank)))))))
                                (else
                                 '()))))
                     ,@(cond ((%%not slot-setter)
                              '())
                             (else
                              (case accessors-type
                                ((macro)
                                 `((jazz:define-macro (,slot-setter object value)
                                     (jazz:with-uniqueness object
                                       (lambda (obj)
                                         `(%%assert-class ,obj ,',name
                                            (%%object-set! ,obj ,,rank ,value)))))))
                                (else
                                 '())))))))
              slots
              (jazz:naturals (%%fx+ jazz:object-size ascendant-size) instance-size))
       (%%table-set! jazz:class-info ',name (jazz:make-class-info ',metaclass-accessor ',ascendant-accessor ',constructor ',constructor-type ',accessors-type ',slots ',slot-names ',all-slot-names ',instance-size)))))


(jazz:define-macro (jazz:define-class-runtime name #!optional (bootstrap? #f))
  (let ((class-info (%%table-ref jazz:class-info name))
        (class-level-name (%%compose-helper name 'core-level)))
    (let ((metaclass-accessor (and (%%not bootstrap?) (jazz:get-class-info-metaclass-accessor class-info)))
          (ascendant-accessor (jazz:get-class-info-ascendant-accessor class-info)))
      (let ((ascendant-info (if (%%not ascendant-accessor) #f (%%table-ref jazz:class-info ascendant-accessor))))
        (let ((ascendant-slot-names (if (%%not ascendant-info) '() (jazz:get-class-info-all-slot-names ascendant-info)))
              (constructor (jazz:get-class-info-constructor class-info))
              (constructor-type (jazz:get-class-info-constructor-type class-info))
              (accessors-type (jazz:get-class-info-accessors-type class-info))
              (slots (jazz:get-class-info-slots class-info))
              (slot-names (jazz:get-class-info-slot-names class-info))
              (all-slot-names (jazz:get-class-info-all-slot-names class-info))
              (instance-size (jazz:get-class-info-instance-size class-info)))
          (let ((ascendant-size (%%length ascendant-slot-names))
                (all-variables (map (lambda (slot-name) (jazz:generate-symbol (%%symbol->string slot-name))) all-slot-names))
                (class-symbol (jazz:generate-symbol "class"))
                (obj-symbol (jazz:generate-symbol "obj"))
                (rest-symbol (jazz:generate-symbol "rest")))
            `(begin
               ,@(map (lambda (slot rank)
                        (jazz:bind (slot-name slot-initialize slot-getter slot-setter) slot
                          `(begin
                             ,@(cond ((%%not slot-getter)
                                      '())
                                     (else
                                      (case accessors-type
                                        ((macro)
                                         '())
                                        (else
                                         `((define (,slot-getter object)
                                             (%%assert-class object ,name
                                               (%%object-ref object ,rank))))))))
                             ,@(cond ((%%not slot-setter)
                                      '())
                                     (else
                                      (case accessors-type
                                        ((macro)
                                         '())
                                        (else
                                         `((define (,slot-setter object value)
                                             (%%assert-class object ,name
                                               (%%object-set! object ,rank value)))))))))))
                      slots
                      (jazz:naturals (%%fx+ jazz:object-size ascendant-size) instance-size))
               (define ,name
                 (jazz:new-core-class ,metaclass-accessor ',name (%%make-table test: eq?) ,ascendant-accessor))
               (define ,class-level-name
                 (%%get-class-level ,name))
               ,@(if (%%not constructor)
                     '()
                   (case constructor-type
                     ((keyword)
                      `((define ,constructor
                          (jazz:new-constructor ,name))))
                     (else
                      `((define (,constructor ,@all-variables)
                          (%%object ,name ,@all-variables))))))
               ,@(if bootstrap?
                     '()
                   (jazz:expand-slots name slots))
               (jazz:set-core-class ',(jazz:reference-name name) ,name))))))))


(jazz:define-macro (jazz:define-class-bootstrap name)
  (let ((class-info (%%table-ref jazz:class-info name)))
    (let ((metaclass-accessor (jazz:get-class-info-metaclass-accessor class-info))
          (ascendant-accessor (jazz:get-class-info-ascendant-accessor class-info))
          (slots (jazz:get-class-info-slots class-info)))
      `(begin
         (%%set-object-class ,name ,metaclass-accessor)
         ,@(if (%%not ascendant-accessor)
               '()
             `((%%set-class-instance-slots ,name (%%get-class-instance-slots ,ascendant-accessor))
               (%%set-class-instance-size ,name (%%get-class-instance-size ,ascendant-accessor))))
         ,@(jazz:expand-slots name slots)))))


(define (jazz:expand-slots name slots)
  (let ((obj-symbol (jazz:generate-symbol "obj")))
    (map (lambda (slot)
           (jazz:bind (slot-name slot-initialize slot-getter slot-setter) slot
             (if (jazz:unspecified? slot-initialize)
                 `(jazz:add-slot ,name ',slot-name #f #t)
               `(jazz:add-slot ,name ',slot-name (lambda (,obj-symbol) ,slot-initialize) #t))))
         slots))))
