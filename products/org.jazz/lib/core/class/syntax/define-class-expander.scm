;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Define Class Expander
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


(module core.class.syntax.define-class-expander


(define (jazz.parse-define-class ascendant-name inherited-slot-names class-name slots proc)
  (let* ((class-accessor (if (%%null? class-name) #f class-name))
         (ascendant-accessor (if (%%null? ascendant-name) #f ascendant-name))
         (ascendant-size (%%length inherited-slot-names))
         (slot-names (map car slots))
         (all-slot-names (%%append inherited-slot-names slot-names))
         (all-variables (map (lambda (slot-name) (jazz.generate-symbol slot-name)) all-slot-names))
         (all-length (%%length all-slot-names))
         (instance-size all-length)
         (vector-size (%%fixnum+ jazz.object-size instance-size)))
    (proc class-accessor ascendant-accessor ascendant-size slot-names all-variables instance-size vector-size)))


(define (jazz.parse-define-class-runtime ascendant-name inherited-slot-names class-name slot-names proc)
  (let* ((class-accessor (if (%%null? class-name) #f class-name))
         (ascendant-accessor (if (%%null? ascendant-name) #f ascendant-name))
         (ascendant-size (%%length inherited-slot-names))
         (all-slot-names (%%append inherited-slot-names slot-names))
         (all-variables (map (lambda (slot-name) (jazz.generate-symbol slot-name)) all-slot-names))
         (all-length (%%length all-slot-names))
         (instance-size all-length)
         (vector-size (%%fixnum+ jazz.object-size instance-size)))
    (proc class-accessor ascendant-accessor ascendant-size slot-names all-variables instance-size vector-size)))


(define (jazz.expand-define-class name ascendant-name inherited-slot-names class-name constructor slots)
  (jazz.parse-define-class ascendant-name inherited-slot-names class-name slots
    (lambda (class-accessor ascendant-accessor ascendant-size slot-names all-variables instance-size vector-size)
      `(begin
         ,@(if (%%null? constructor)
               '()
             `((jazz.define-macro (,constructor class ,@all-variables)
                 (%%list '%%object class ,@all-variables))))
         ,@(map (lambda (slot rank)
                  (let ((slot-name (%%car slot))
                        (slot-getter (%%cadr slot))
                        (slot-setter (%%car (%%cddr slot)))
                        (object (jazz.generate-symbol "object"))
                        (value (jazz.generate-symbol "value")))
                    `(begin
                       ,@(if (%%null? slot-getter)
                             '()
                           (if (jazz.safe?)
                               `((define (,slot-getter ,object)
                                   (%%safe-assert (%%object-of-class? ,object ,name)
                                     (%%object-ref ,object ,rank))))
                             `((jazz.define-macro (,slot-getter ,object)
                                 (%%list '%%safe-assert (%%list '%%object-of-class? ,object ',name)
                                   (%%list '%%object-ref ,object ,rank))))))
                       ,@(if (%%null? slot-setter)
                             '()
                           (if (jazz.safe?)
                               `((define (,slot-setter ,object ,value)
                                   (%%safe-assert (%%object-of-class? ,object ,name)
                                     (%%object-set! ,object ,rank ,value))))
                             `((jazz.define-macro (,slot-setter ,object ,value)
                                 (%%list '%%safe-assert (%%list '%%object-of-class? ,object ',name)
                                   (%%list '%%object-set! ,object ,rank ,value)))))))))
                slots
                (jazz.naturals (%%fixnum+ jazz.object-size ascendant-size) vector-size))))))


(define (jazz.expand-define-class-runtime name ascendant-name inherited-slot-names class-name slot-names)
  (jazz.parse-define-class-runtime ascendant-name inherited-slot-names class-name slot-names
    (lambda (class-accessor ascendant-accessor ascendant-size slot-names all-variables instance-size vector-size)
      `(begin
         (define ,name
           (jazz.new-core-class ,class-accessor ',name (%%new-hashtable ':eq?) ,ascendant-accessor ',slot-names ,instance-size))
         (jazz.set-core-class ',(jazz.identifier-name name) ,name)
         (jazz.validate-inherited-slots ',name ,ascendant-accessor ',inherited-slot-names))))))
