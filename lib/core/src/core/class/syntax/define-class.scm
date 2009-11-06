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


(unit protected core.class.syntax.define-class


(jazz.define-macro (jazz.define-class name ascendant-name inherited-slot-names class-name constructor slots)
  (define (jazz.parse-define-class ascendant-name inherited-slot-names class-name slots proc)
    (let* ((class-accessor (if (%%null? class-name) #f class-name))
           (ascendant-accessor (if (%%null? ascendant-name) #f ascendant-name))
           (ascendant-size (%%length inherited-slot-names))
           (slot-names (map car slots))
           (all-slot-names (%%append inherited-slot-names slot-names))
           (all-variables (map (lambda (slot-name) (jazz.generate-symbol (%%symbol->string slot-name))) all-slot-names))
           (all-length (%%length all-slot-names))
           (instance-size (%%fx+ jazz.object-size all-length)))
      (proc class-accessor ascendant-accessor ascendant-size slot-names all-variables instance-size)))
  
  (jazz.parse-define-class ascendant-name inherited-slot-names class-name slots
    (lambda (class-accessor ascendant-accessor ascendant-size slot-names all-variables instance-size)
      `(begin
         ;; this is necessary as the getter/setter type assertions will refer to
         ;; the class that is only defined later in the runtime implementation file
         ,@(if jazz.debug-core?
               `((jazz.define-variable ,name))
             '())
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
                           (if jazz.debug-core?
                               `((define (,slot-getter ,object)
                                   (%%core-assertion (jazz.object-of-class? ,object ,name) (jazz.expected-error ,name ,object)
                                     (%%object-ref ,object ,rank))))
                             `((jazz.define-macro (,slot-getter ,object)
                                 (%%list '%%object-ref ,object ,rank)))))
                       ,@(if (%%null? slot-setter)
                             '()
                           (if jazz.debug-core?
                               `((define (,slot-setter ,object ,value)
                                   (%%core-assertion (jazz.object-of-class? ,object ,name) (jazz.expected-error ,name ,object)
                                     (%%object-set! ,object ,rank ,value))))
                             `((jazz.define-macro (,slot-setter ,object ,value)
                                 (%%list '%%object-set! ,object ,rank ,value))))))))
                slots
                (jazz.naturals (%%fx+ jazz.object-size ascendant-size) instance-size))
         (jazz.define-macro (,(%%string->symbol (%%string-append (%%symbol->string name) "-implement")))
           `(begin
             (define ,',name
               (jazz.new-core-class ,',class-accessor ',',name (%%make-table test: eq?) ,',ascendant-accessor ',',slot-names ,',instance-size))
             (jazz.set-core-class ',',(jazz.identifier-name name) ,',name)
             (jazz.validate-inherited-slots ',',name ,',ascendant-accessor ',',inherited-slot-names)))))))


(jazz.define-macro (jazz.define-class-runtime name)
  `(,(%%string->symbol (%%string-append (%%symbol->string name) "-implement")))))
