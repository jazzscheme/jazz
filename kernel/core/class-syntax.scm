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


(block core.class-syntax


;;;
;;;; Errors
;;;


(jazz:define-variable jazz:object-of-class?)


(define (jazz:expected-error class obj)
  (jazz:error "{a} expected: {s}" class obj))


;;;
;;;; Object
;;;


(define %%object-content
  0)


(define jazz:object-class
  %%object-content)


(define jazz:object-size
  (%%fx+ jazz:object-class 1))


(jazz:define-macro (%%assert-class object class . body)
  `(%%core-assertion (jazz:object-of-class? ,object ,class) (jazz:expected-error ,class ,object)
     ,@body))


(jazz:define-macro (%%get-object-class object)
  `(%%object-ref ,object ,jazz:object-class))


(jazz:define-macro (%%set-object-class object class)
  `(%%object-set! ,object ,jazz:object-class ,class))


(jazz:define-macro (%%new class . rest)
  (jazz:with-uniqueness class
    (lambda (cls)
      (let ((obj (jazz:generate-symbol "obj")))
        `(%%debug-assert (%%class? ,cls)
           (let ((,obj (%%make-object ,cls (%%get-class-instance-size ,cls))))
             (jazz:initialize-slots ,obj)
             ((%%class-dispatch ,cls 0 0) ,obj ,@rest)
             ,obj))))))


;;;
;;;; Define Class
;;;


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


(jazz:define-macro (jazz:define-class-runtime name #!optional (bootstrap-type? #f))
  (let ((class-info (%%table-ref jazz:class-info name))
        (class-level-name (%%compose-helper name 'core-level)))
    (let ((metaclass-accessor (and (%%not bootstrap-type?) (jazz:get-class-info-metaclass-accessor class-info)))
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
               ,@(if bootstrap-type?
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
         slots)))


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


(jazz:define-structure jazz:Class jazz:Category (constructor: %%allocate-class constructor-class?: #t accessors-type: macro)
  ((ascendant       accessors: generate)
   (interfaces      accessors: generate)
   (slots           accessors: generate)
   (instance-slots  accessors: generate)
   (instance-size   accessors: generate)
   (level           accessors: generate)
   (virtual-names   accessors: generate)
   (class-table     accessors: generate)
   (interface-table accessors: generate)
   (user-data       accessors: generate)))


;;;
;;;; Interface
;;;


(jazz:define-structure jazz:Interface jazz:Category (constructor: %%allocate-interface constructor-class?: #t accessors-type: macro)
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


(jazz:define-structure jazz:Slot jazz:Field (constructor: %%allocate-slot constructor-class?: #t accessors-type: macro)
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
  `(or (c-code ,(%%string-append #<<end-of-c-code
{
    ___SCMOBJ obj = ___ARG1;
    if (___MEM_ALLOCATED(obj))
    {
        int subtype = (*___UNTAG(obj) & ___SMASK) >> ___HTB;
end-of-c-code

(if (jazz:gambitjazz?)
    #<<end-of-c-code
        if (subtype == ___sJAZZ || subtype == ___sJAZZSTRUCT)
end-of-c-code
    #<<end-of-c-code
        if (subtype == ___sJAZZ)
end-of-c-code
)

#<<end-of-c-code
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
)
    ,obj                    ;; ___ARG1
    jazz:subtypes           ;; ___ARG2
    jazz:Fixnum             ;; ___ARG3
    jazz:Char               ;; ___ARG4
    jazz:specialtypes       ;; ___ARG5
    )
       (jazz:structure-type ,obj)))


(jazz:define-macro (%%scheme-class-of-impl obj)
  `(cond ((%%object? ,obj)       (%%get-object-class ,obj))
         ((%%boolean? ,obj)      jazz:Boolean)
         ((%%char? ,obj)         jazz:Char)
         ((%%fixnum? ,obj)       jazz:Fixnum)
         ((%%ratnum? ,obj)       jazz:Ratnum)
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
              (jazz:error "Unable to get class of {s}" ,obj)))))


;;;
;;;; Define Method
;;;


(define (jazz:expand-define-virtual-syntax signature bootstrap-type?)
  (let* ((name (jazz:reference-name (%%car signature)))
         (macro-name (%%car signature))
         (parameters (%%cdr signature))
         (class-name (%%caar parameters))
         (object-parameter (%%cadr (%%car parameters)))
         (extra-parameters (%%cdr parameters))
         (class-level-name (%%compose-helper class-name 'core-level))
         (implementation-name (jazz:method-implementation-name class-name name))
         (method-rank-name (jazz:method-rank-name implementation-name))
         (is-test (if bootstrap-type? 'jazz:bootstrap-type? '%%class-is?))
         (object (jazz:generate-symbol "object")))
    `(jazz:define-macro (,macro-name ,object-parameter ,@extra-parameters)
       (if (%%symbol? ,object-parameter)
           (%%list '%%core-assertion (%%list ',is-test ,object-parameter ',class-name) (%%list 'jazz:error (jazz:format "{s} expected in calling {s}: {s}" ',class-name ',name ,object-parameter))
             (%%list (%%list '%%class-dispatch (%%list '%%get-object-class ,object-parameter) ',class-level-name ',method-rank-name)
                     ,object-parameter
                     ,@extra-parameters))
         (jazz:with-uniqueness ,object-parameter
           (lambda (,object)
             (%%list '%%core-assertion (%%list ',is-test ,object ',class-name) (%%list 'jazz:error (jazz:format "{s} expected in calling {s}: {s}" ',class-name ',name ,object-parameter))
               (%%list (%%list '%%class-dispatch (%%list '%%get-object-class ,object) ',class-level-name ',method-rank-name)
                       ,object
                       ,@extra-parameters))))))))


(define (jazz:expand-define-virtual signature)
  (let* ((name (jazz:reference-name (%%car signature)))
         (parameters (%%cdr signature))
         (class-name (%%caar parameters))
         (implementation-name (jazz:method-implementation-name class-name name))
         (rank-name (jazz:method-rank-name implementation-name)))
    `(define ,rank-name
       (jazz:add-core-virtual-method ,class-name ',name))))


(define (jazz:expand-define-method signature body)
  (let* ((name (jazz:reference-name (%%car signature)))
         (parameters (%%cdr signature))
         (class-name (%%caar parameters))
         (object-parameter (%%cadr (%%car parameters)))
         (extra-parameters (%%cdr parameters))
         (implementation-name (jazz:method-implementation-name class-name name)))
    `(begin
       (define ,implementation-name
         (let ((nextmethod (jazz:find-nextmethod ,class-name ',name)))
           (lambda (,object-parameter ,@extra-parameters)
             ,@body)))
       (jazz:add-core-method-node ,class-name ',name ,implementation-name))))


(define (jazz:method-implementation-name class-name name)
  (let ((name (jazz:last (jazz:split-string (%%symbol->string name) #\.))))
    (%%string->symbol (%%string-append (%%symbol->string class-name) "^" name))))


(define (jazz:method-rank-name implementation-name)
  (%%compose-helper implementation-name 'rank))


(jazz:define-macro (jazz:define-virtual-syntax signature . rest)
  (let ((bootstrap-type? (if (%%null? rest) #f (%%car rest))))
    (jazz:expand-define-virtual-syntax signature bootstrap-type?)))


(jazz:define-macro (jazz:define-virtual signature)
  (jazz:expand-define-virtual signature))


(jazz:define-syntax jazz:define-method
  (lambda (src)
    (jazz:bind (signature . body) (%%cdr (%%source-code src))
      (jazz:expand-define-method (%%desourcify signature) body)))))
