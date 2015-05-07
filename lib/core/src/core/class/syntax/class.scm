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


(unit protected core.class.syntax.class


;;;
;;;; Object
;;;


(jazz:define-structure jazz:Object () ()
  ())


;;;
;;;; Type
;;;


(jazz:define-structure jazz:Type jazz:Object ()
  ())


;;;
;;;; Category
;;;


(jazz:define-structure jazz:Category jazz:Type (accessors-type: macro)
  ((identifier   accessors: generate)
   (fields       accessors: generate)
   (virtual-size accessors: generate)
   (ancestors    accessors: generate)
   (descendants  accessors: generate)))


;;;
;;;; Class
;;;


(jazz:define-structure jazz:Class jazz:Category (constructor: %%allocate-class constructor-structure?: #t accessors-type: macro)
  ((ascendant       accessors: generate)
   (interfaces      accessors: generate)
   (slots           accessors: generate)
   (instance-slots  accessors: generate)
   (instance-size   accessors: generate)
   (level           accessors: generate)
   (virtual-names   accessors: generate)
   (class-table     accessors: generate)
   (interface-table accessors: generate)))


;;;
;;;; Interface
;;;


(jazz:define-structure jazz:Interface jazz:Category (constructor: %%allocate-interface constructor-structure?: #t accessors-type: macro)
  ((ascendants getter: generate)
   (rank       getter: generate)))


;;;
;;;; Field
;;;


(jazz:define-structure jazz:Field jazz:Object (accessors-type: macro)
  ((name getter: generate)))


;;;
;;;; Slot
;;;


(jazz:define-structure jazz:Slot jazz:Field (constructor: %%allocate-slot constructor-structure?: #t accessors-type: macro)
  ((offset     getter: generate)
   (initialize getter: generate setter: generate)))


;;;
;;;; Dispatch
;;;


(jazz:define-macro (%%final-dispatch class implementation)
  implementation)


(jazz:define-macro (%%class-dispatch class class-level implementation-rank)
  `(%%vector-ref (%%vector-ref (%%get-class-class-table ,class) ,class-level) ,implementation-rank))


(jazz:define-macro (%%interface-dispatch class interface-rank implementation-rank)
  `(%%vector-ref (%%vector-ref (%%get-class-interface-table ,class) ,interface-rank) ,implementation-rank))


(cond-expand
  (gambit
    ;; This macro enables jazz to bootstrap fully interpreted
    ;; Note that we need in the pure scheme version to implement every type that can potentially be used by
    ;; Jazz code so that really means every type if we want to be a superset of the underlying scheme system
    (jazz:define-macro (%%class-of-impl obj)
      (case (jazz:walk-for)
        ((compile)
         `(%%c-class-of-impl ,obj))
        (else
         `(%%scheme-class-of-impl ,obj))))
    
    (jazz:define-macro (%%c-class-of-impl obj)
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
    jazz:subtypes           ;; ___ARG2
    jazz:Fixnum             ;; ___ARG3
    jazz:Char               ;; ___ARG4
    jazz:specialtypes       ;; ___ARG5
    ))
           (jazz:structure-type ,obj)))
    
    (jazz:define-macro (%%scheme-class-of-impl obj)
      `(cond ((%%object? ,obj)       (%%get-object-class ,obj))
             ((%%boolean? ,obj)      jazz:Boolean)
             ((%%char? ,obj)         jazz:Char)
             ((%%fixnum? ,obj)       jazz:Fixnum)
             ((%%flonum? ,obj)       jazz:Flonum)
             ((%%integer? ,obj)      jazz:Integer)
             ((%%rational? ,obj)     jazz:Rational)
             ((%%real? ,obj)         jazz:Real)
             ((%%complex? ,obj)      jazz:Complex)
             ((%%number? ,obj)       jazz:Number)
             ((%%null? ,obj)         jazz:Null)
             ((%%pair? ,obj)         jazz:Pair)
             ((%%string? ,obj)       jazz:String)
             ((%%vector? ,obj)       jazz:Vector)
             ((%%s8vector? ,obj)     jazz:S8Vector)
             ((%%u8vector? ,obj)     jazz:U8Vector)
             ((%%s16vector? ,obj)    jazz:S16Vector)
             ((%%u16vector? ,obj)    jazz:U16Vector)
             ((%%s32vector? ,obj)    jazz:S32Vector)
             ((%%u32vector? ,obj)    jazz:U32Vector)
             ((%%s64vector? ,obj)    jazz:S64Vector)
             ((%%u64vector? ,obj)    jazz:U64Vector)
             ((%%f32vector? ,obj)    jazz:F32Vector)
             ((%%f64vector? ,obj)    jazz:F64Vector)
             ((%%symbol? ,obj)       jazz:Symbol)
             ((%%keyword? ,obj)      jazz:Keyword)
             ((%%port? ,obj)         jazz:Port)
             ((%%continuation? ,obj) jazz:Continuation)
             ((%%procedure? ,obj)    jazz:Procedure)
             ((%%foreign? ,obj)      jazz:Foreign)
             ((%%values? ,obj)       jazz:Values)
             ((%%eof-object? ,obj)   jazz:EOF)
             ((%%unspecified? ,obj)  jazz:Unspecified)
             ((jazz:marker? ,obj)    jazz:Marker)
             (else
              (or (jazz:structure-type ,obj)
                  (jazz:error "Unable to get class of {s}" ,obj))))))
  
  (else
    (jazz:define-macro (%%class-of-impl obj)
      `(%%scheme-class-of-impl ,obj)))))
 