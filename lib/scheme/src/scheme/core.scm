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


(module scheme.core scheme


(export (scheme.kernel))
(import (scheme.kernel))


(native jazz:Any)


;;;
;;;; Native
;;;


(native private pp)
;; quick hack
(native private table-ref)
;; quick hack
(native private table-set!)
(native private jazz:getf)
(native private jazz:naturals)
(native private jazz:specified?)
(native private jazz:unspecified?)
(native private jazz:unspecified)
(native private jazz:Object-Class)
(native jazz:class-info)
(native jazz:get-class-info-all-slot-names)
(native jazz:object-size)
(native jazz:make-class-info)
(native jazz:add-slot)
(native jazz:codes-forms)


(define-special-form (%%object walker resume declaration environment form-src)
  (let ((expressions (walk-list walker resume declaration environment (cdr (source-code form-src)))))
    (new-special-expression
      (lambda (expression declaration environment)
        (let ((expressions (emit-expressions expressions declaration walker resume environment)))
          (new-code
            `(%%object ,@(codes-forms expressions))
            Any
            form-src))))))


(define-special-form (%%object-ref walker resume declaration environment form-src)
  (let ((expressions (walk-list walker resume declaration environment (cdr (source-code form-src)))))
    (new-special-expression
      (lambda (expression declaration environment)
        (let ((expressions (emit-expressions expressions declaration walker resume environment)))
          (new-code
            `(%%object-ref ,@(codes-forms expressions))
            Any
            form-src))))))


(define-special-form (%%object-set! walker resume declaration environment form-src)
  (let ((expressions (walk-list walker resume declaration environment (cdr (source-code form-src)))))
    (new-special-expression
      (lambda (expression declaration environment)
        (let ((expressions (emit-expressions expressions declaration walker resume environment)))
          (new-code
            `(%%object-set! ,@(codes-forms expressions))
            Any
            form-src))))))


;;;
;;;; Class
;;;


(define-macro (define-class name ascendant-name options slots)
  (define (downcase str)
    (list->string (map char-downcase (string->list str))))
  
  (define (standardize-slot slot)
    (if (symbol? slot)
        (list slot)
      slot))
  
  (define (parse-slot slot downcase-name)
    (define (slot-initialize slot)
      (getf (cdr slot) initialize: (unspecified)))
    
    (define (slot-getter slot)
      (parse-accessor slot getter: "get-"))
    
    (define (slot-setter slot)
      (parse-accessor slot setter: "set-"))
    
    (define (parse-accessor slot accessor-key accessor-prefix)
      (define (generate-accessor)
        (string->symbol (string-append accessor-prefix downcase-name "-" (symbol->string (car slot)))))
      
      (let ((accessor (or (getf (cdr slot) accessor-key #f)
                          (getf (cdr slot) accessors: #f))))
        (cond ((eq? accessor #t)
               #f)
              ((eq? accessor 'generate)
               (generate-accessor))
              (else
               accessor))))
    
    (let ((slot (standardize-slot slot)))
      (list (slot-name slot)
            (slot-initialize slot)
            (slot-getter slot)
            (slot-setter slot))))
  
  (define (slot-name slot)
    (car slot))
  
  (define (expand-slots slots)
    (let ((obj-symbol (generate-symbol "obj")))
      (map (lambda (slot)
             (let ((slot-name (list-ref slot 0))
                   (slot-initialize (list-ref slot 1))
                   (slot-getter (list-ref slot 2))
                   (slot-setter (list-ref slot 3)))
               (let ((initialize
                       (and (specified? slot-initialize)
                            `(lambda (,obj-symbol)
                               ,slot-initialize))))
                 `(add-slot ,name ',slot-name ,initialize #t))))
           slots)))
  
  (let* ((downcase-name (downcase (symbol->string name)))
         (metaclass-name (getf options metaclass: #f))
         (constructor (getf options constructor: #f))
         (constructor-type #f)
         (accessors-type (getf options accessors-type: 'function))
         (metaclass-accessor (if (not metaclass-name) 'Object-Class metaclass-name))
         (ascendant-accessor (if (null? ascendant-name) #f ascendant-name))
         (jazz-ascendant-name (string->symbol (string-append "jazz:" (symbol->string ascendant-name))))
         (ascendant-info (or (table-ref class-info ascendant-name #f) (table-ref class-info jazz-ascendant-name #f)))
         (ascendant-slot-names (if (null? ascendant-name) '() (get-class-info-all-slot-names ascendant-info)))
         (ascendant-size (length ascendant-slot-names))
         (slots (map (lambda (slot) (parse-slot slot downcase-name)) slots))
         (slot-names (map slot-name slots))
         (all-slot-names (append ascendant-slot-names slot-names))
         (all-length (length all-slot-names))
         (instance-size (+ object-size all-length))
         (all-variables (map (lambda (slot-name) (generate-symbol (symbol->string slot-name))) all-slot-names)))
    (table-set! class-info name (make-class-info metaclass-accessor ascendant-accessor constructor constructor-type accessors-type slots slot-names all-slot-names instance-size))
    `(begin
       ;; is this necessary?
       ;; also a quick hack for non-initialized slots creating a problem because of #!void in expansion
       ;;(table-set! class-info ',name (make-class-info ',metaclass-accessor ',ascendant-accessor ',constructor ',constructor-type ',accessors-type ',slots ',slot-names ',all-slot-names ',instance-size))
       ,@(map (lambda (slot rank)
                (let ((slot-name (list-ref slot 0))
                      (slot-initialize (list-ref slot 1))
                      (slot-getter (list-ref slot 2))
                      (slot-setter (list-ref slot 3)))
                  `(begin
                     ,@(if (not slot-getter)
                           '()
                         `((export ,slot-getter)
                           (define (,slot-getter object)
                             (%%object-ref object ,rank))))
                     ,@(if (not slot-setter)
                           '()
                         `((export ,slot-setter)
                           (define (,slot-setter object value)
                             (%%object-set! object ,rank value)))))))
              slots
              (naturals (+ object-size ascendant-size) instance-size))
       (export ,name)
       (define ,name
         (new-core-class ,metaclass-accessor ',name (make-table test: eq?) ,ascendant-accessor))
       ,@(if (not constructor)
             '()
           `((export ,constructor)
             (define (,constructor ,@all-variables)
               (%%object ,name ,@all-variables))))
       ,@(expand-slots slots))))


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
