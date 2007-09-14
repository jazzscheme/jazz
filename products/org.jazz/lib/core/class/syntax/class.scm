;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Class Syntax
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
;;;  Portions created by the Initial Developer are Copyright (C) 1996-2006
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


(module core.class.syntax.class


(define jazz.unit-name
  jazz.object-size)

(define jazz.unit-fields
  (+ jazz.unit-name 1))

(define jazz.unit-ancestors
  (+ jazz.unit-fields 1))

(define jazz.unit-descendants
  (+ jazz.unit-ancestors 1))

(define jazz.class-ascendant
  (+ jazz.unit-descendants 1))

(define jazz.class-interfaces
  (+ jazz.class-ascendant 1))

(define jazz.class-slots
  (+ jazz.class-interfaces 1))

(define jazz.class-instance-size
  (+ jazz.class-slots 1))

(define jazz.class-level
  (+ jazz.class-instance-size 1))

(define jazz.class-dispatch-table
  (+ jazz.class-level 1))

(define jazz.class-core-method-alist
  (+ jazz.class-dispatch-table 1))

(define jazz.class-core-virtual-alist
  (+ jazz.class-core-method-alist 1))

(define jazz.class-core-virtual-names
  (+ jazz.class-core-virtual-alist 1))

(define jazz.class-core-vtable
  (+ jazz.class-core-virtual-names 1))

(define jazz.class-class-table
  (+ jazz.class-core-vtable 1))

(define jazz.class-interface-table
  (+ jazz.class-class-table 1))


(define jazz.class-size
  (+ jazz.class-interface-table 1))


(jazz.define-macro (%%get-unit-name unit)
  `(%%object-ref ,unit ,jazz.unit-name))


(jazz.define-macro (%%get-unit-fields unit)
  `(%%object-ref ,unit ,jazz.unit-fields))


(jazz.define-macro (%%set-unit-fields unit fields)
  `(%%object-set! ,unit ,jazz.unit-fields ,fields))


(jazz.define-macro (%%get-unit-ancestors unit)
  `(%%object-ref ,unit ,jazz.unit-ancestors))


(jazz.define-macro (%%set-unit-ancestors unit ancestors)
  `(%%object-set! ,unit ,jazz.unit-ancestors ,ancestors))


(jazz.define-macro (%%get-unit-descendants unit)
  `(%%object-ref ,unit ,jazz.unit-descendants))


(jazz.define-macro (%%set-unit-descendants unit ancestors)
  `(%%object-set! ,unit ,jazz.unit-descendants ,ancestors))


(jazz.define-macro (%%get-class-ascendant class)
  `(%%object-ref ,class ,jazz.class-ascendant))


(jazz.define-macro (%%get-class-interfaces class)
  `(%%object-ref ,class ,jazz.class-interfaces))


(jazz.define-macro (%%get-class-slots class)
  `(%%object-ref ,class ,jazz.class-slots))


(jazz.define-macro (%%set-class-slots class slots)
  `(%%object-set! ,class ,jazz.class-slots ,slots))


(jazz.define-macro (%%get-class-instance-size class)
  `(%%object-ref ,class ,jazz.class-instance-size))


(jazz.define-macro (%%set-class-instance-size class size)
  `(%%object-set! ,class ,jazz.class-instance-size ,size))


(jazz.define-macro (%%get-class-level class)
  `(%%object-ref ,class ,jazz.class-level))


(jazz.define-macro (%%set-class-level class size)
  `(%%object-set! ,class ,jazz.class-level ,size))


(jazz.define-macro (%%get-class-dispatch-table class)
  `(%%object-ref ,class ,jazz.class-dispatch-table))


(jazz.define-macro (%%set-class-dispatch-table class vtable)
  `(%%object-set! ,class ,jazz.class-dispatch-table ,vtable))


(jazz.define-macro (%%get-class-core-method-alist class)
  `(%%object-ref ,class ,jazz.class-core-method-alist))


(jazz.define-macro (%%set-class-core-method-alist class vtable)
  `(%%object-set! ,class ,jazz.class-core-method-alist ,vtable))


(jazz.define-macro (%%get-class-core-virtual-alist class)
  `(%%object-ref ,class ,jazz.class-core-virtual-alist))


(jazz.define-macro (%%set-class-core-virtual-alist class vtable)
  `(%%object-set! ,class ,jazz.class-core-virtual-alist ,vtable))


(jazz.define-macro (%%get-class-core-virtual-names class)
  `(%%object-ref ,class ,jazz.class-core-virtual-names))


(jazz.define-macro (%%set-class-core-virtual-names class vtable)
  `(%%object-set! ,class ,jazz.class-core-virtual-names ,vtable))


(jazz.define-macro (%%get-class-core-vtable class)
  `(%%object-ref ,class ,jazz.class-core-vtable))


(jazz.define-macro (%%set-class-core-vtable class vtable)
  `(%%object-set! ,class ,jazz.class-core-vtable ,vtable))


(jazz.define-macro (%%get-class-class-table class)
  `(%%object-ref ,class ,jazz.class-class-table))


(jazz.define-macro (%%set-class-class-table class vtable)
  `(%%object-set! ,class ,jazz.class-class-table ,vtable))


(jazz.define-macro (%%get-class-interface-table class)
  `(%%object-ref ,class ,jazz.class-interface-table))


(jazz.define-macro (%%set-class-interface-table class vtable)
  `(%%object-set! ,class ,jazz.class-interface-table ,vtable))


(jazz.define-macro (%%get-object-class object)
  `(%%object-ref ,object ,jazz.object-class))


(jazz.define-macro (%%set-object-class object class)
  `(%%object-set! ,object ,jazz.object-class ,class))


(jazz.define-macro (%%subtype? target unit)
  `(%%memq ,unit (%%get-unit-ancestors ,target)))


(jazz.define-macro (%%subclass? target class)
  `(%%memq ,class (%%get-unit-ancestors ,target)))


(cond-expand
  #; ;; experimental and should really be coded in C with Marc's help
  (gambit
    (define jazz.subtypes
      (make-vector 32)))
  
  (else
   (jazz.define-macro (%%class-of expr)
     (let ((expand
             (lambda (symbol)
               `(if (%%object? ,symbol)
                    (%%get-object-class ,symbol)
                  (jazz.class-of-native ,symbol)))))
       (if (%%symbol? expr)
           (expand expr)
         (let ((symbol (jazz.generate-symbol "value")))
           (%%list 'let (%%list (%%list symbol expr))
             (expand symbol))))))))


(jazz.define-macro (%%is? object unit)
  `(%%subtype? (%%class-of ,object) ,unit)))
