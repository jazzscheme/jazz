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


(unit protected core.class.syntax.class


(define jazz.category-identifier
  jazz.object-size)

(define jazz.category-fields
  (%%fx+ jazz.category-identifier 1))

(define jazz.category-virtual-size
  (%%fx+ jazz.category-fields 1))

(define jazz.category-ancestors
  (%%fx+ jazz.category-virtual-size 1))

(define jazz.category-descendants
  (%%fx+ jazz.category-ancestors 1))

(define jazz.class-ascendant
  (%%fx+ jazz.category-descendants 1))

(define jazz.class-interfaces
  (%%fx+ jazz.class-ascendant 1))

(define jazz.class-slots
  (%%fx+ jazz.class-interfaces 1))

(define jazz.class-instance-slots
  (%%fx+ jazz.class-slots 1))

(define jazz.class-instance-size
  (%%fx+ jazz.class-instance-slots 1))

(define jazz.class-level
  (%%fx+ jazz.class-instance-size 1))

(define jazz.class-dispatch-table
  (%%fx+ jazz.class-level 1))

(define jazz.class-core-method-alist
  (%%fx+ jazz.class-dispatch-table 1))

(define jazz.class-core-virtual-alist
  (%%fx+ jazz.class-core-method-alist 1))

(define jazz.class-core-virtual-names
  (%%fx+ jazz.class-core-virtual-alist 1))

(define jazz.class-core-vtable
  (%%fx+ jazz.class-core-virtual-names 1))

(define jazz.class-class-table
  (%%fx+ jazz.class-core-vtable 1))

(define jazz.class-interface-table
  (%%fx+ jazz.class-class-table 1))


(define jazz.class-size
  (%%fx+ jazz.class-interface-table 1))


(jazz.define-macro (%%get-category-identifier category)
  `(%%object-ref ,category ,jazz.category-identifier))


(jazz.define-macro (%%set-category-identifier category identifier)
  `(%%object-set! ,category ,jazz.category-identifier ,identifier))


(jazz.define-macro (%%get-category-fields category)
  `(%%object-ref ,category ,jazz.category-fields))


(jazz.define-macro (%%set-category-fields category fields)
  `(%%object-set! ,category ,jazz.category-fields ,fields))


(jazz.define-macro (%%get-category-virtual-size category)
  `(%%object-ref ,category ,jazz.category-virtual-size))


(jazz.define-macro (%%set-category-virtual-size category virtual-size)
  `(%%object-set! ,category ,jazz.category-virtual-size ,virtual-size))


(jazz.define-macro (%%get-category-ancestors category)
  `(%%object-ref ,category ,jazz.category-ancestors))


(jazz.define-macro (%%set-category-ancestors category ancestors)
  `(%%object-set! ,category ,jazz.category-ancestors ,ancestors))


(jazz.define-macro (%%get-category-descendants category)
  `(%%object-ref ,category ,jazz.category-descendants))


(jazz.define-macro (%%set-category-descendants category descendants)
  `(%%object-set! ,category ,jazz.category-descendants ,descendants))


(jazz.define-macro (%%get-class-ascendant class)
  `(%%object-ref ,class ,jazz.class-ascendant))


(jazz.define-macro (%%set-class-ascendant class ascendant)
  `(%%object-set! ,class ,jazz.class-ascendant ,ascendant))


(jazz.define-macro (%%get-class-interfaces class)
  `(%%object-ref ,class ,jazz.class-interfaces))


(jazz.define-macro (%%set-class-interfaces class interfaces)
  `(%%object-set! ,class ,jazz.class-interfaces ,interfaces))


(jazz.define-macro (%%get-class-slots class)
  `(%%object-ref ,class ,jazz.class-slots))


(jazz.define-macro (%%set-class-slots class slots)
  `(%%object-set! ,class ,jazz.class-slots ,slots))


(jazz.define-macro (%%get-class-instance-slots class)
  `(%%object-ref ,class ,jazz.class-instance-slots))


(jazz.define-macro (%%set-class-instance-slots class slots)
  `(%%object-set! ,class ,jazz.class-instance-slots ,slots))


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


(jazz.define-macro (%%final-dispatch class implementation)
  implementation)


(jazz.define-macro (%%class-dispatch class class-level implementation-rank)
  `(%%vector-ref (%%vector-ref (%%get-class-class-table ,class) ,class-level) ,implementation-rank))


(jazz.define-macro (%%interface-dispatch class interface-rank implementation-rank)
  `(%%vector-ref (%%vector-ref (%%get-class-interface-table ,class) ,interface-rank) ,implementation-rank))


(cond-expand
  (gambit
    ;; This macro enables jazz to bootstrap fully interpreted
    ;; Note that we need in the pure scheme version to implement every type that can potentially be used by
    ;; Jazz code so that really means every type if we want to be a superset of the underlying scheme system
    (jazz.define-macro (%%class-of-impl obj)
      (case (jazz.walk-for)
        ((compile)
         `(%%c-class-of-impl ,obj))
        (else
         `(%%scheme-class-of-impl ,obj))))
    
    (jazz.define-macro (%%c-class-of-impl obj)
      `(or (let ()
             (declare (extended-bindings))
             (##c-code #<<end-of-c-code
{
    ___SCMOBJ obj = ___ARG1;
    if (___MEM_ALLOCATED(obj))
    {
        int subtype = (*___UNTAG(obj) & ___SMASK) >> ___HTB;
        if (subtype == ___sJAZZ)
            ___RESULT = ___VECTORREF(obj,0);
        else if (subtype == ___sSTRUCTURE)
            ___RESULT = ___FAL;
        else
            ___RESULT = ___BODY_AS(___ARG2,___tSUBTYPED)[subtype];
    }
    else if (___FIXNUMP(obj))
        ___RESULT = ___ARG3;
    else if (obj >= 0)
        ___RESULT = ___ARG4;
    else
        ___RESULT = ___BODY_AS(___ARG5,___tSUBTYPED)[___INT(___FAL - obj)];
}
end-of-c-code
    ,obj                    ;; ___ARG1
    jazz.subtypes           ;; ___ARG2
    jazz.Fixnum             ;; ___ARG3
    jazz.Char               ;; ___ARG4
    jazz.specialtypes       ;; ___ARG5
    ))
           (jazz.structure-type ,obj)))
    
    (jazz.define-macro (%%scheme-class-of-impl obj)
      `(cond ((%%object? ,obj)       (%%get-object-class ,obj))
             ((%%boolean? ,obj)      jazz.Boolean)
             ((%%char? ,obj)         jazz.Char)
             ((%%fixnum? ,obj)       jazz.Fixnum)
             ((%%flonum? ,obj)       jazz.Flonum)
             ((%%integer? ,obj)      jazz.Integer)
             ((%%rational? ,obj)     jazz.Rational)
             ((%%real? ,obj)         jazz.Real)
             ((%%complex? ,obj)      jazz.Complex)
             ((%%number? ,obj)       jazz.Number)
             ((%%null? ,obj)         jazz.Null)
             ((%%pair? ,obj)         jazz.Pair)
             ((%%string? ,obj)       jazz.String)
             ((%%vector? ,obj)       jazz.Vector)
             ((%%s8vector? ,obj)     jazz.S8Vector)
             ((%%u8vector? ,obj)     jazz.U8Vector)
             ((%%s16vector? ,obj)    jazz.S16Vector)
             ((%%u16vector? ,obj)    jazz.U16Vector)
             ((%%s32vector? ,obj)    jazz.S32Vector)
             ((%%u32vector? ,obj)    jazz.U32Vector)
             ((%%s64vector? ,obj)    jazz.S64Vector)
             ((%%u64vector? ,obj)    jazz.U64Vector)
             ((%%f32vector? ,obj)    jazz.F32Vector)
             ((%%f64vector? ,obj)    jazz.F64Vector)
             ((%%symbol? ,obj)       jazz.Symbol)
             ((%%keyword? ,obj)      jazz.Keyword)
             ((%%port? ,obj)         jazz.Port)
             ((%%continuation? ,obj) jazz.Continuation)
             ((%%procedure? ,obj)    jazz.Procedure)
             ((%%foreign? ,obj)      jazz.Foreign)
             ((%%values? ,obj)       jazz.Values)
             ((%%eof-object? ,obj)   jazz.EOF)
             ((%%unspecified? ,obj)  jazz.Unspecified)
             ((jazz.marker? ,obj)    jazz.Marker)
             (else
              (or (jazz.structure-type ,obj)
                  (jazz.error "Unable to get class of {s}" ,obj))))))
  
  (else
    (jazz.define-macro (%%class-of-impl obj)
      `(%%scheme-class-of-impl ,obj)))))
 