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

(define jazz.class-class-table
  (+ jazz.class-dispatch-table 1))

(define jazz.class-interface-table
  (+ jazz.class-class-table 1))


(define jazz.class-size
  (+ jazz.class-interface-table 1))


(define-macro (%%get-unit-name unit)
  `(%%object-ref ,unit ,jazz.unit-name))


(define-macro (%%get-unit-fields unit)
  `(%%object-ref ,unit ,jazz.unit-fields))


(define-macro (%%set-unit-fields unit fields)
  `(%%object-set! ,unit ,jazz.unit-fields ,fields))


(define-macro (%%get-unit-ancestors unit)
  `(%%object-ref ,unit ,jazz.unit-ancestors))


(define-macro (%%set-unit-ancestors unit ancestors)
  `(%%object-set! ,unit ,jazz.unit-ancestors ,ancestors))


(define-macro (%%get-unit-descendants unit)
  `(%%object-ref ,unit ,jazz.unit-descendants))


(define-macro (%%set-unit-descendants unit ancestors)
  `(%%object-set! ,unit ,jazz.unit-descendants ,ancestors))


(define-macro (%%get-class-ascendant class)
  `(%%object-ref ,class ,jazz.class-ascendant))


(define-macro (%%get-class-interfaces class)
  `(%%object-ref ,class ,jazz.class-interfaces))


(define-macro (%%get-class-slots class)
  `(%%object-ref ,class ,jazz.class-slots))


(define-macro (%%set-class-slots class slots)
  `(%%object-set! ,class ,jazz.class-slots ,slots))


(define-macro (%%get-class-instance-size class)
  `(%%object-ref ,class ,jazz.class-instance-size))


(define-macro (%%set-class-instance-size class size)
  `(%%object-set! ,class ,jazz.class-instance-size ,size))


(define-macro (%%get-class-level class)
  `(%%object-ref ,class ,jazz.class-level))


(define-macro (%%set-class-level class size)
  `(%%object-set! ,class ,jazz.class-level ,size))


(define-macro (%%get-class-dispatch-table class)
  `(%%object-ref ,class ,jazz.class-dispatch-table))


(define-macro (%%set-class-dispatch-table class vtable)
  `(%%object-set! ,class ,jazz.class-dispatch-table ,vtable))


(define-macro (%%get-class-class-table class)
  `(%%object-ref ,class ,jazz.class-class-table))


(define-macro (%%set-class-class-table class vtable)
  `(%%object-set! ,class ,jazz.class-class-table ,vtable))


(define-macro (%%get-class-interface-table class)
  `(%%object-ref ,class ,jazz.class-interface-table))


(define-macro (%%set-class-interface-table class vtable)
  `(%%object-set! ,class ,jazz.class-interface-table ,vtable))


(define-macro (%%get-object-class object)
  `(%%object-ref ,object ,jazz.object-class))


(define-macro (%%set-object-class object class)
  `(%%object-set! ,object ,jazz.object-class ,class))


(define-macro (%%subtype? target unit)
  `(%%memq ,unit (%%get-unit-ancestors ,target)))


(define-macro (%%subclass? target class)
  `(%%memq ,class (%%get-unit-ancestors ,target)))


(define-macro (%%class-of expr)
  (let ((expand
          (lambda (symbol)
            `(if (%%object? ,symbol)
                 (%%get-object-class ,symbol)
               (jazz.class-of-native ,symbol)))))
    (if (%%symbol? expr)
        (expand expr)
      (let ((symbol (jazz.generate-symbol "value")))
        (%%list 'let (%%list (%%list symbol expr))
          (expand symbol))))))


(define-macro (%%is? object unit)
  `(%%subtype? (%%class-of ,object) ,unit)))
