;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Primitives
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


(block kernel.primitives


(declare (not safe))


;;;
;;;; Boolean
;;;


(jazz:define-synto (%%boolean? obj)
  (if jazz:debug-core?
      `(boolean? ,obj)
    `(^#boolean? ,obj)))

(jazz:define-synto (%%not expr)
  (if jazz:debug-core?
      `(not ,expr)
    `(^#not ,expr)))


;;;
;;;; Box
;;;


(jazz:define-synto (%%box? obj)
  (if jazz:debug-core?
      `(box? ,obj)
    `(^#box? ,obj)))

(jazz:define-synto (%%box obj)
  (if jazz:debug-core?
      `(box ,obj)
    `(^#box ,obj)))

(jazz:define-synto (%%unbox box)
  (if jazz:debug-core?
      `(unbox ,box)
    `(^#unbox ,box)))


;;;
;;;; Char
;;;


(jazz:define-synto (%%char? obj)
  (if jazz:debug-core?
      `(char? ,obj)
    `(^#char? ,obj)))

(jazz:define-synto (%%char=? c1 c2)
  (if jazz:debug-core?
      `(char=? ,c1 ,c2)
    `(^#char=? ,c1 ,c2)))

(jazz:define-synto (%%char<=? c1 c2)
  (if jazz:debug-core?
      `(char<=? ,c1 ,c2)
    `(^#char<=? ,c1 ,c2)))


;;;
;;;; Closure
;;;


(jazz:define-synto (%%closure? obj)
  `(^#closure? ,obj))

(jazz:define-synto (%%closure-code closure)
  (%%force-uniqueness (closure)
    `(%%check-closure ,closure 1 (%%closure-code ,closure)
       (^#closure-code ,closure))))

(jazz:define-synto (%%closure-length closure)
  (%%force-uniqueness (closure)
    `(%%check-closure ,closure 1 (%%closure-length ,closure)
       (^#closure-length ,closure))))

(jazz:define-synto (%%closure-ref closure n)
  (%%force-uniqueness (closure)
    `(%%check-closure ,closure 1 (%%closure-ref ,closure ,n)
       (%%check-fixnum ,n 2 (%%closure-ref ,closure ,n)
         (^#closure-ref ,closure ,n)))))


;;;
;;;; Complex
;;;


(jazz:define-synto (%%complex? obj)
  (if jazz:debug-core?
      `(complex? ,obj)
    `(^#complex? ,obj)))


;;;
;;;; Container
;;;


(jazz:define-synto (%%path->container-hook-set! hook)
  (%%force-uniqueness (hook)
    `(%%check-procedure ,hook 1 (%%path->container-hook-set! ,hook)
       (^#path->container-hook-set! ,hook))))

(jazz:define-synto (%%container->path-hook-set! hook)
  (%%force-uniqueness (hook)
    `(%%check-procedure ,hook 1 (%%container->path-hook-set! ,hook)
       (^#container->path-hook-set! ,hook))))

(jazz:define-synto (%%container->id-hook-set! hook)
  (%%force-uniqueness (hook)
    `(%%check-procedure ,hook 1 (%%container->id-hook-set! ,hook)
       (^#container->id-hook-set! ,hook))))


;;;
;;;; Continuation
;;;


(jazz:define-synto (%%continuation? obj)
  (if jazz:debug-core?
      `(continuation? ,obj)
    `(^#continuation? ,obj)))

(jazz:define-synto (%%continuation-capture proc)
  (if jazz:debug-core?
      `(continuation-capture ,proc)
    `(^#continuation-capture ,proc)))

(jazz:define-synto (%%continuation-graft cont proc)
  (if jazz:debug-core?
      `(continuation-graft ,cont ,proc)
    `(^#continuation-graft ,cont ,proc)))

(jazz:define-synto (%%continuation-return cont . values)
  (if jazz:debug-core?
      `(continuation-return ,cont ,@values)
    `(^#continuation-return ,cont ,@values)))

(jazz:define-synto (%%continuation-graft-no-winding cont values)
  (%%force-uniqueness (cont values)
    `(%%check-continuation ,cont 1 (%%continuation-graft-no-winding ,cont ,values)
       (^#continuation-graft-no-winding ,cont ,values))))

(jazz:define-synto (%%continuation-return-no-winding cont values)
  (%%force-uniqueness (cont values)
    `(%%check-continuation ,cont 1 (%%continuation-return-no-winding ,cont ,values)
       (^#continuation-return-no-winding ,cont ,values))))

(jazz:define-synto (%%continuation-parent cont)
  (%%force-uniqueness (cont)
    `(%%check-continuation ,cont 1 (%%continuation-parent ,cont)
       (^#continuation-parent ,cont))))

(jazz:define-synto (%%continuation-creator cont)
  (%%force-uniqueness (cont)
    `(%%check-continuation ,cont 1 (%%continuation-creator ,cont)
       (^#continuation-creator ,cont))))

(jazz:define-synto (%%continuation-locat cont)
  (%%force-uniqueness (cont)
    `(%%check-continuation ,cont 1 (%%continuation-locat ,cont)
       (^#continuation-locat ,cont))))

(jazz:define-synto (%%continuation-locals cont)
  (%%force-uniqueness (cont)
    `(%%check-continuation ,cont 1 (%%continuation-locals ,cont)
       (^#continuation-locals ,cont))))

(jazz:define-synto (%%continuation-next cont)
  (%%force-uniqueness (cont)
    `(%%check-continuation ,cont 1 (%%continuation-next ,cont)
       (^#continuation-next ,cont))))

(jazz:define-synto (%%continuation-first-frame cont all-frames?)
  (%%force-uniqueness (cont all-frames?)
    `(%%check-continuation ,cont 1 (%%continuation-first-frame ,cont ,all-frames?)
       (^#continuation-first-frame ,cont ,all-frames?))))

(jazz:define-synto (%%continuation-next-frame cont all-frames?)
  (%%force-uniqueness (cont all-frames?)
    `(%%check-continuation ,cont 1 (%%continuation-next-frame ,cont ,all-frames?)
       (^#continuation-next-frame ,cont ,all-frames?))))

(jazz:define-synto (%%interp-continuation? cont)
  (%%force-uniqueness (cont)
    `(%%check-continuation ,cont 1 (%%interp-continuation? ,cont)
       (^#interp-continuation? ,cont))))


;;;
;;;; Control
;;;


(jazz:define-synto (%%apply proc lst)
  (if jazz:debug-core?
      `(apply ,proc ,lst)
    `(^#apply ,proc ,lst)))


;;;
;;;; Equality
;;;


(jazz:define-synto (%%eq? x y)
  (if jazz:debug-core?
      `(eq? ,x ,y)
    `(^#eq? ,x ,y)))

(jazz:define-synto (%%neq? x y)
  `(%%not (%%eq? ,x ,y)))

(jazz:define-synto (%%eqv? x y)
  (if jazz:debug-core?
      `(eqv? ,x ,y)
    `(^#eqv? ,x ,y)))

(jazz:define-synto (%%equal? x y)
  (if jazz:debug-core?
      `(equal? ,x ,y)
    `(^#equal? ,x ,y)))


;;;
;;;; Eval
;;;


(jazz:define-synto (%%load path script-callback clone-cte? raise-os-exception? linker-name quiet?)
  ;; DANGER : temporary hack until proper primitive exists
  `(^#load ,path ,script-callback ,clone-cte? ,raise-os-exception? ,linker-name ,quiet?))


;;;
;;;; Exactness
;;;


(jazz:define-synto (%%exact->inexact obj)
  (if jazz:debug-core?
      `(exact->inexact ,obj)
    `(^#exact->inexact ,obj)))


(jazz:define-synto (%%inexact->exact obj)
  (if jazz:debug-core?
      `(inexact->exact ,obj)
    `(^#inexact->exact ,obj)))


;;;
;;;; Exception
;;;


(jazz:define-synto (%%exception->locat exc cont)
  (%%force-uniqueness (exc cont)
    `(%%check-continuation ,cont 2 (%%exception->locat ,exc ,cont)
       (^#exception->locat ,exc ,cont))))


(jazz:define-synto (%%display-exception-hook-ref)
  `##display-exception-hook)

(jazz:define-synto (%%display-exception-hook-set! hook)
  (%%force-uniqueness (hook)
    `(%%check-procedure ,hook 1 (%%display-exception-hook-set! ,hook)
       (^#display-exception-hook-set! ,hook))))


(jazz:define-synto (%%raise-heap-overflow-exception)
  `(##raise-heap-overflow-exception))


;;;
;;;; Features
;;;


(jazz:define-synto (%%cond-expand-features . rest)
  `(^#cond-expand-features ,@rest))


;;;
;;;; Fixnum
;;;


(jazz:define-synto (%%fixnum? obj)
  (if jazz:debug-core?
      `(fixnum? ,obj)
    `(^#fixnum? ,obj)))

(jazz:define-synto (%%fixnum->flonum x)
  (%%force-uniqueness (x)
    `(%%check-fixnum ,x 1 (%%fixnum->flonum ,x)
       (^#fixnum->flonum ,x))))

(jazz:define-synto (%%fx= . rest)
  (if jazz:debug-core?
      `(= ,@rest)
    `(^#fx= ,@rest)))

(jazz:define-synto (%%fx< . rest)
  (if jazz:debug-core?
      `(< ,@rest)
    `(^#fx< ,@rest)))

(jazz:define-synto (%%fx<= . rest)
  (if jazz:debug-core?
      `(<= ,@rest)
    `(^#fx<= ,@rest)))

(jazz:define-synto (%%fx> . rest)
  (if jazz:debug-core?
      `(> ,@rest)
    `(^#fx> ,@rest)))

(jazz:define-synto (%%fx>= . rest)
  (if jazz:debug-core?
      `(>= ,@rest)
    `(^#fx>= ,@rest)))

(jazz:define-synto (%%fx+ . rest)
  (if jazz:debug-core?
      `(+ ,@rest)
    `(^#fx+ ,@rest)))

(jazz:define-synto (%%fx- . rest)
  (if jazz:debug-core?
      `(- ,@rest)
    `(^#fx- ,@rest)))

(jazz:define-synto (%%fx* . rest)
  (if jazz:debug-core?
      `(* ,@rest)
    `(^#fx* ,@rest)))

(jazz:define-synto (%%fxabs x)
  (if jazz:debug-core?
      `(fxabs ,x)
    `(^#fxabs ,x)))

(jazz:define-synto (%%fxquotient x y)
  (if jazz:debug-core?
      `(quotient ,x ,y)
    `(^#fxquotient ,x ,y)))

(jazz:define-synto (%%fxmin . rest)
  (if jazz:debug-core?
      `(fxmin ,@rest)
    `(^#fxmin ,@rest)))

(jazz:define-synto (%%fxmax . rest)
  (if jazz:debug-core?
      `(fxmax ,@rest)
    `(^#fxmax ,@rest)))

(jazz:define-synto (%%fxmodulo . rest)
  (if jazz:debug-core?
      `(fxmodulo ,@rest)
    `(^#fxmodulo ,@rest)))

(jazz:define-synto (%%fxeven? . rest)
  (if jazz:debug-core?
      `(fxeven? ,@rest)
    `(^#fxeven? ,@rest)))

(jazz:define-synto (%%fxodd? . rest)
  (if jazz:debug-core?
      `(fxodd? ,@rest)
    `(^#fxodd? ,@rest)))

(jazz:define-synto (%%fxnot . rest)
  (if jazz:debug-core?
      `(fxnot ,@rest)
    `(^#fxnot ,@rest)))

(jazz:define-synto (%%fxand . rest)
  (if jazz:debug-core?
      `(fxand ,@rest)
    `(^#fxand ,@rest)))

(jazz:define-synto (%%fxior . rest)
  (if jazz:debug-core?
      `(fxior ,@rest)
    `(^#fxior ,@rest)))

(jazz:define-synto (%%fxxor . rest)
  (if jazz:debug-core?
      `(fxxor ,@rest)
    `(^#fxxor ,@rest)))

(jazz:define-synto (%%fxarithmetic-shift . rest)
  (if jazz:debug-core?
      `(fxarithmetic-shift ,@rest)
    `(^#fxarithmetic-shift ,@rest)))

(jazz:define-synto (%%fxarithmetic-shift-left . rest)
  (if jazz:debug-core?
      `(fxarithmetic-shift-left ,@rest)
    `(^#fxarithmetic-shift-left ,@rest)))

(jazz:define-synto (%%fxarithmetic-shift-right . rest)
  (if jazz:debug-core?
      `(fxarithmetic-shift-right ,@rest)
    `(^#fxarithmetic-shift-right ,@rest)))

(jazz:define-synto (%%fxbetween? n lower upper)
  (%%force-uniqueness (n)
    (if jazz:debug-core?
        `(and (>= ,n ,lower)
              (<= ,n ,upper))
      `(and (^#fx>= ,n ,lower)
            (^#fx<= ,n ,upper)))))


;;;
;;;; Flonum
;;;


(jazz:define-synto (%%flonum? obj)
  (if jazz:debug-flonum?
      `(flonum? ,obj)
    `(^#flonum? ,obj)))

(jazz:define-synto (%%flnan? obj)
  (if jazz:debug-flonum?
      `(flnan? ,obj)
    `(^#flnan? ,obj)))

(jazz:define-synto (%%flonum->fixnum x)
  (%%force-uniqueness (x)
    `(%%check-flonum ,x 1 (%%flonum->fixnum ,x)
       (^#flonum->fixnum ,x))))

(jazz:define-synto (%%fl= . rest)
  (if jazz:debug-flonum?
      `(= ,@rest)
    `(^#fl= ,@rest)))

(jazz:define-synto (%%fl< . rest)
  (if jazz:debug-flonum?
      `(< ,@rest)
    `(^#fl< ,@rest)))

(jazz:define-synto (%%fl<= . rest)
  (if jazz:debug-flonum?
      `(<= ,@rest)
    `(^#fl<= ,@rest)))

(jazz:define-synto (%%fl> . rest)
  (if jazz:debug-flonum?
      `(> ,@rest)
    `(^#fl> ,@rest)))

(jazz:define-synto (%%fl>= . rest)
  (if jazz:debug-flonum?
      `(>= ,@rest)
    `(^#fl>= ,@rest)))

(jazz:define-synto (%%fl+ . rest)
  (if jazz:debug-flonum?
      `(+ ,@rest)
    `(^#fl+ ,@rest)))

(jazz:define-synto (%%fl- . rest)
  (if jazz:debug-flonum?
      `(- ,@rest)
    `(^#fl- ,@rest)))

(jazz:define-synto (%%fl* . rest)
  (if jazz:debug-flonum?
      `(* ,@rest)
    `(^#fl* ,@rest)))

(jazz:define-synto (%%fl/ . rest)
  (if jazz:debug-flonum?
      `(/ ,@rest)
    `(^#fl/ ,@rest)))

(jazz:define-synto (%%flfloor x)
  (if jazz:debug-flonum?
      `(flfloor ,x)
    `(^#flfloor ,x)))

(jazz:define-synto (%%flceiling x)
  (if jazz:debug-flonum?
      `(flceiling ,x)
    `(^#flceiling ,x)))

(jazz:define-synto (%%fltruncate x)
  (if jazz:debug-flonum?
      `(fltruncate ,x)
    `(^#fltruncate ,x)))

(jazz:define-synto (%%flround x)
  (if jazz:debug-flonum?
      `(flround ,x)
    `(^#flround ,x)))

(jazz:define-synto (%%flabs x)
  (if jazz:debug-flonum?
      `(flabs ,x)
    `(^#flabs ,x)))

(jazz:define-synto (%%flsqrt x)
  (if jazz:debug-flonum?
      `(flsqrt ,x)
    `(^#flsqrt ,x)))

(jazz:define-synto (%%flexpt x y)
  (if jazz:debug-flonum?
      `(flexpt ,x ,y)
    `(^#flexpt ,x ,y)))

(jazz:define-synto (%%flsquare x)
  (if jazz:debug-flonum?
      `(flsquare ,x)
    `(^#flsquare ,x)))

(jazz:define-synto (%%flsin x)
  (if jazz:debug-flonum?
      `(flsin ,x)
    `(^#flsin ,x)))

(jazz:define-synto (%%flcos x)
  (if jazz:debug-flonum?
      `(flcos ,x)
    `(^#flcos ,x)))

(jazz:define-synto (%%fltan x)
  (if jazz:debug-flonum?
      `(fltan ,x)
    `(^#fltan ,x)))

(jazz:define-synto (%%flasin x)
  (if jazz:debug-flonum?
      `(flasin ,x)
    `(^#flasin ,x)))

(jazz:define-synto (%%flacos x)
  (if jazz:debug-flonum?
      `(flacos ,x)
    `(^#flacos ,x)))

(jazz:define-synto (%%flatan . rest)
  (if jazz:debug-flonum?
      `(flatan ,@rest)
    `(^#flatan ,@rest)))

(jazz:define-synto (%%flmin . rest)
  (if jazz:debug-flonum?
      `(flmin ,@rest)
    `(^#flmin ,@rest)))

(jazz:define-synto (%%flmax . rest)
  (if jazz:debug-flonum?
      `(flmax ,@rest)
    `(^#flmax ,@rest)))

(jazz:define-synto (%%flalloc)
  `(^#subtype-set! (^#f64vector 0.) jazz:subtype-flonum))

(jazz:define-synto (%%flref fl ignore)
  `(^#f64vector-ref ,fl 0))

(jazz:define-synto (%%flset! fl ignore val)
  `(^#f64vector-set! ,fl 0 ,val))

(jazz:define-synto (%%flonum->exact-int fl)
  (%%force-uniqueness (fl)
    `(%%check-flonum ,fl 1 (%%flonum->exact-int ,fl)
       (^#flonum->exact-int ,fl))))

(jazz:define-synto (%%flbetween? n lower upper)
  (%%force-uniqueness (n)
    (if jazz:debug-core?
        `(and (>= ,n ,lower)
              (<= ,n ,upper))
      `(and (^#fl>= ,n ,lower)
            (^#fl<= ,n ,upper)))))


;;;
;;;; Foreign
;;;


(jazz:define-synto (%%foreign? obj)
  `(^#foreign? ,obj))

;(jazz:define-synto (%%still-obj-refcount foreign)
;  )

(jazz:define-synto (%%still-obj-refcount-dec! foreign)
  (%%force-uniqueness (foreign)
    `(%%check-foreign ,foreign 1 (%%still-obj-refcount-dec! ,foreign)
       (^#still-obj-refcount-dec! ,foreign))))

(jazz:define-synto (%%still-obj-refcount-inc! foreign)
  (%%force-uniqueness (foreign)
    `(%%check-foreign ,foreign 1 (%%still-obj-refcount-inc! ,foreign)
       (^#still-obj-refcount-inc! ,foreign))))

(jazz:define-synto (%%still-copy obj)
  `(^#still-copy ,obj))


;;;
;;;; GC
;;;


(jazz:define-synto (%%add-gc-interrupt-job! thunk)
  (%%force-uniqueness (thunk)
    `(%%check-procedure ,thunk 1 (%%add-gc-interrupt-job! ,thunk)
       (^#add-gc-interrupt-job! ,thunk))))

(jazz:define-synto (%%clear-gc-interrupt-jobs!)
  `(^#clear-gc-interrupt-jobs!))


;;;
;;;; Heartbeat
;;;


(jazz:define-synto (%%get-heartbeat-interval! f64vect i)
  (%%force-uniqueness (f64vect i)
    `(%%check-f64vector ,f64vect 1 (%%get-heartbeat-interval! ,f64vect ,i)
       (%%check-fixnum ,i 2 (%%get-heartbeat-interval! ,f64vect ,i)
         (^#get-heartbeat-interval! ,f64vect ,i)))))

(jazz:define-synto (%%set-heartbeat-interval! seconds)
  (%%force-uniqueness (seconds)
    `(%%check-flonum ,seconds 1 (%%set-heartbeat-interval! ,seconds)
       (^#set-heartbeat-interval! ,seconds))))


;;;
;;;; Homogeneous
;;;


;; use at your own risk versions that do not initialize memory
(jazz:define-synto (%%allocate-vector    size . rest) `(%%tracking (^#make-vector    ,size ,@rest)))
(jazz:define-synto (%%allocate-s8vector  size . rest) `(%%tracking (^#make-s8vector  ,size ,@rest)))
(jazz:define-synto (%%allocate-u8vector  size . rest) `(%%tracking (^#make-u8vector  ,size ,@rest)))
(jazz:define-synto (%%allocate-s16vector size . rest) `(%%tracking (^#make-s16vector ,size ,@rest)))
(jazz:define-synto (%%allocate-u16vector size . rest) `(%%tracking (^#make-u16vector ,size ,@rest)))
(jazz:define-synto (%%allocate-s32vector size . rest) `(%%tracking (^#make-s32vector ,size ,@rest)))
(jazz:define-synto (%%allocate-u32vector size . rest) `(%%tracking (^#make-u32vector ,size ,@rest)))
(jazz:define-synto (%%allocate-s64vector size . rest) `(%%tracking (^#make-s64vector ,size ,@rest)))
(jazz:define-synto (%%allocate-u64vector size . rest) `(%%tracking (^#make-u64vector ,size ,@rest)))
(jazz:define-synto (%%allocate-f32vector size . rest) `(%%tracking (^#make-f32vector ,size ,@rest)))
(jazz:define-synto (%%allocate-f64vector size . rest) `(%%tracking (^#make-f64vector ,size ,@rest)))


;; these versions initialize memory
(jazz:define-synto (%%make-vector    size . rest) `(%%tracking (make-vector    ,size ,@rest)))
(jazz:define-synto (%%make-s8vector  size . rest) `(%%tracking (make-s8vector  ,size ,@rest)))
(jazz:define-synto (%%make-u8vector  size . rest) `(%%tracking (make-u8vector  ,size ,@rest)))
(jazz:define-synto (%%make-s16vector size . rest) `(%%tracking (make-s16vector ,size ,@rest)))
(jazz:define-synto (%%make-u16vector size . rest) `(%%tracking (make-u16vector ,size ,@rest)))
(jazz:define-synto (%%make-s32vector size . rest) `(%%tracking (make-s32vector ,size ,@rest)))
(jazz:define-synto (%%make-u32vector size . rest) `(%%tracking (make-u32vector ,size ,@rest)))
(jazz:define-synto (%%make-s64vector size . rest) `(%%tracking (make-s64vector ,size ,@rest)))
(jazz:define-synto (%%make-u64vector size . rest) `(%%tracking (make-u64vector ,size ,@rest)))
(jazz:define-synto (%%make-f32vector size . rest) `(%%tracking (make-f32vector ,size ,@rest)))
(jazz:define-synto (%%make-f64vector size . rest) `(%%tracking (make-f64vector ,size ,@rest)))


(jazz:define-synto (%%s8vector . rest) (if jazz:debug-core? `(s8vector ,@rest) `(^#s8vector ,@rest)))
(jazz:define-synto (%%subs8vector . rest) (if jazz:debug-core? `(subs8vector ,@rest) `(%%tracking (^#subs8vector ,@rest))))
(jazz:define-synto (%%s8vector-length vec) (if jazz:debug-core? `(s8vector-length ,vec) `(^#s8vector-length ,vec)))
(jazz:define-synto (%%s8vector-ref vec n) (if jazz:debug-core? `(s8vector-ref ,vec ,n) `(^#s8vector-ref ,vec ,n)))
(jazz:define-synto (%%s8vector-set! vec n value) (if jazz:debug-core? `(s8vector-set! ,vec ,n ,value) `(^#s8vector-set! ,vec ,n ,value)))


(jazz:define-synto (%%u8vector . rest) (if jazz:debug-core? `(u8vector ,@rest) `(^#u8vector ,@rest)))
(jazz:define-synto (%%subu8vector . rest) (if jazz:debug-core? `(subu8vector ,@rest) `(%%tracking (^#subu8vector ,@rest))))
(jazz:define-synto (%%u8vector-length vec) (if jazz:debug-core? `(u8vector-length ,vec) `(^#u8vector-length ,vec)))
(jazz:define-synto (%%u8vector-ref vec n) (if jazz:debug-core? `(u8vector-ref ,vec ,n) `(^#u8vector-ref ,vec ,n)))
(jazz:define-synto (%%u8vector-set! vec n value) (if jazz:debug-core? `(u8vector-set! ,vec ,n ,value) `(^#u8vector-set! ,vec ,n ,value)))


(jazz:define-synto (%%s16vector . rest) (if jazz:debug-core? `(s16vector ,@rest) `(^#s16vector ,@rest)))
(jazz:define-synto (%%subs16vector . rest) (if jazz:debug-core? `(subs16vector ,@rest) `(%%tracking (^#subs16vector ,@rest))))
(jazz:define-synto (%%s16vector-length vec) (if jazz:debug-core? `(s16vector-length ,vec) `(^#s16vector-length ,vec)))
(jazz:define-synto (%%s16vector-ref vec n) (if jazz:debug-core? `(s16vector-ref ,vec ,n) `(^#s16vector-ref ,vec ,n)))
(jazz:define-synto (%%s16vector-set! vec n value) (if jazz:debug-core? `(s16vector-set! ,vec ,n ,value) `(^#s16vector-set! ,vec ,n ,value)))


(jazz:define-synto (%%u16vector . rest) (if jazz:debug-core? `(u16vector ,@rest) `(^#u16vector ,@rest)))
(jazz:define-synto (%%subu16vector . rest) (if jazz:debug-core? `(subu16vector ,@rest) `(%%tracking (^#subu16vector ,@rest))))
(jazz:define-synto (%%u16vector-length vec) (if jazz:debug-core? `(u16vector-length ,vec) `(^#u16vector-length ,vec)))
(jazz:define-synto (%%u16vector-ref vec n) (if jazz:debug-core? `(u16vector-ref ,vec ,n) `(^#u16vector-ref ,vec ,n)))
(jazz:define-synto (%%u16vector-set! vec n value) (if jazz:debug-core? `(u16vector-set! ,vec ,n ,value) `(^#u16vector-set! ,vec ,n ,value)))


(jazz:define-synto (%%s32vector . rest) (if jazz:debug-core? `(s32vector ,@rest) `(^#s32vector ,@rest)))
(jazz:define-synto (%%subs32vector . rest) (if jazz:debug-core? `(subs32vector ,@rest) `(%%tracking (^#subs32vector ,@rest))))
(jazz:define-synto (%%s32vector-length vec) (if jazz:debug-core? `(s32vector-length ,vec) `(^#s32vector-length ,vec)))
(jazz:define-synto (%%s32vector-ref vec n) (if jazz:debug-core? `(s32vector-ref ,vec ,n) `(^#s32vector-ref ,vec ,n)))
(jazz:define-synto (%%s32vector-set! vec n value) (if jazz:debug-core? `(s32vector-set! ,vec ,n ,value) `(^#s32vector-set! ,vec ,n ,value)))


(jazz:define-synto (%%u32vector . rest) (if jazz:debug-core? `(u32vector ,@rest) `(^#u32vector ,@rest)))
(jazz:define-synto (%%subu32vector . rest) (if jazz:debug-core? `(subu32vector ,@rest) `(%%tracking (^#subu32vector ,@rest))))
(jazz:define-synto (%%u32vector-length vec) (if jazz:debug-core? `(u32vector-length ,vec) `(^#u32vector-length ,vec)))
(jazz:define-synto (%%u32vector-ref vec n) (if jazz:debug-core? `(u32vector-ref ,vec ,n) `(^#u32vector-ref ,vec ,n)))
(jazz:define-synto (%%u32vector-set! vec n value) (if jazz:debug-core? `(u32vector-set! ,vec ,n ,value) `(^#u32vector-set! ,vec ,n ,value)))


(jazz:define-synto (%%s64vector . rest) (if jazz:debug-core? `(s64vector ,@rest) `(^#s64vector ,@rest)))
(jazz:define-synto (%%subs64vector . rest) (if jazz:debug-core? `(subs64vector ,@rest) `(%%tracking (^#subs64vector ,@rest))))
(jazz:define-synto (%%s64vector-length vec) (if jazz:debug-core? `(s64vector-length ,vec) `(^#s64vector-length ,vec)))
(jazz:define-synto (%%s64vector-ref vec n) (if jazz:debug-core? `(s64vector-ref ,vec ,n) `(^#s64vector-ref ,vec ,n)))
(jazz:define-synto (%%s64vector-set! vec n value) (if jazz:debug-core? `(s64vector-set! ,vec ,n ,value) `(^#s64vector-set! ,vec ,n ,value)))


(jazz:define-synto (%%u64vector . rest) (if jazz:debug-core? `(u64vector ,@rest) `(^#u64vector ,@rest)))
(jazz:define-synto (%%subu64vector . rest) (if jazz:debug-core? `(subu64vector ,@rest) `(%%tracking (^#subu64vector ,@rest))))
(jazz:define-synto (%%u64vector-length vec) (if jazz:debug-core? `(u64vector-length ,vec) `(^#u64vector-length ,vec)))
(jazz:define-synto (%%u64vector-ref vec n) (if jazz:debug-core? `(u64vector-ref ,vec ,n) `(^#u64vector-ref ,vec ,n)))
(jazz:define-synto (%%u64vector-set! vec n value) (if jazz:debug-core? `(u64vector-set! ,vec ,n ,value) `(^#u64vector-set! ,vec ,n ,value)))


(jazz:define-synto (%%f32vector . rest) (if jazz:debug-core? `(f32vector ,@rest) `(^#f32vector ,@rest)))
(jazz:define-synto (%%subf32vector . rest) (if jazz:debug-core? `(subf32vector ,@rest) `(%%tracking (^#subf32vector ,@rest))))
(jazz:define-synto (%%f32vector-length vec) (if jazz:debug-core? `(f32vector-length ,vec) `(^#f32vector-length ,vec)))
(jazz:define-synto (%%f32vector-ref vec n) (if jazz:debug-core? `(f32vector-ref ,vec ,n) `(^#f32vector-ref ,vec ,n)))
(jazz:define-synto (%%f32vector-set! vec n value) (if jazz:debug-core? `(f32vector-set! ,vec ,n ,value) `(^#f32vector-set! ,vec ,n ,value)))


(jazz:define-synto (%%f64vector . rest) (if jazz:debug-core? `(f64vector ,@rest) `(^#f64vector ,@rest)))
(jazz:define-synto (%%subf64vector . rest) (if jazz:debug-core? `(subf64vector ,@rest) `(%%tracking (^#subf64vector ,@rest))))
(jazz:define-synto (%%f64vector-length vec) (if jazz:debug-core? `(f64vector-length ,vec) `(^#f64vector-length ,vec)))
(jazz:define-synto (%%f64vector-ref vec n) (if jazz:debug-core? `(f64vector-ref ,vec ,n) `(^#f64vector-ref ,vec ,n)))
(jazz:define-synto (%%f64vector-set! vec n value) (if jazz:debug-core? `(f64vector-set! ,vec ,n ,value) `(^#f64vector-set! ,vec ,n ,value)))


;;;
;;;; Interrupts
;;;


(define TERMINATE-INTERRUPT  1)
(define HEARTBEAT-INTERRUPT  2)
(define USER-INTERRUPT       3)
(define GC-INTERRUPT         4)
(define HIGH-LEVEL-INTERRUPT 5)


(jazz:define-synto (%%interrupt-handler code)
  `(%%danger %%interrupt-handler
     (^#interrupt-handler ,code)))

(jazz:define-synto (%%interrupt-vector-set! code handler)
  `(%%danger %%interrupt-vector-set!
     (^#interrupt-vector-set! ,code ,handler)))


(jazz:define-synto (%%disable-interrupts!)
  `(^#disable-interrupts!))

(jazz:define-synto (%%enable-interrupts!)
  `(^#enable-interrupts!))


;;;
;;;; Kernel
;;;


(jazz:define-synto (%%subtype obj)
  `(%%danger %%subtype
     (^#subtype ,obj)))


;;;
;;;; Keyword
;;;


(jazz:define-synto (%%keyword? obj)
  (if jazz:debug-core?
      `(keyword? ,obj)
    `(^#keyword? ,obj)))

(jazz:define-synto (%%string->keyword str)
  (if jazz:debug-core?
      `(string->keyword ,str)
    `(^#string->keyword ,str)))

(jazz:define-synto (%%keyword->string keyword)
  (if jazz:debug-core?
      `(keyword->string ,keyword)
    `(^#keyword->string ,keyword)))

(jazz:define-synto (%%keyword-table)
  `(^#keyword-table))


;;;
;;;; List
;;;


(jazz:define-synto (%%null? obj)
  (if jazz:debug-core?
      `(null? ,obj)
    `(^#null? ,obj)))

(jazz:define-synto (%%pair? obj)
  (if jazz:debug-core?
      `(pair? ,obj)
    `(^#pair? ,obj)))

(jazz:define-synto (%%car pair)
  (if jazz:debug-core?
      `(car ,pair)
    `(^#car ,pair)))

(jazz:define-synto (%%cdr pair)
  (if jazz:debug-core?
      `(cdr ,pair)
    `(^#cdr ,pair)))

(jazz:define-synto (%%set-car! pair val)
  (if jazz:debug-core?
      `(set-car! ,pair ,val)
    `(^#set-car! ,pair ,val)))

(jazz:define-synto (%%set-cdr! pair val)
  (if jazz:debug-core?
      `(set-cdr! ,pair ,val)
    `(^#set-cdr! ,pair ,val)))

(jazz:define-synto (%%caar pair)
  (if jazz:debug-core?
      `(caar ,pair)
    `(^#caar ,pair)))

(jazz:define-synto (%%cadr pair)
  (if jazz:debug-core?
      `(cadr ,pair)
    `(^#cadr ,pair)))

(jazz:define-synto (%%cdar pair)
  (if jazz:debug-core?
      `(cdar ,pair)
    `(^#cdar ,pair)))

(jazz:define-synto (%%cddr pair)
  (if jazz:debug-core?
      `(cddr ,pair)
    `(^#cddr ,pair)))

(jazz:define-synto (%%length lst)
  (if jazz:debug-core?
      `(length ,lst)
    `(^#length ,lst)))

(jazz:define-synto (%%memq obj lst)
  (if jazz:debug-core?
      `(memq ,obj ,lst)
    `(^#memq ,obj ,lst)))

(jazz:define-synto (%%memv obj lst)
  `(memv ,obj ,lst))

(jazz:define-synto (%%member obj lst)
  (if jazz:debug-core?
      `(member ,obj ,lst)
    `(^#member ,obj ,lst)))

(jazz:define-synto (%%assq obj alist)
  (if jazz:debug-core?
      `(assq ,obj ,alist)
    `(^#assq ,obj ,alist)))

(jazz:define-synto (%%assv obj alist)
  (if jazz:debug-core?
      `(assv ,obj ,alist)
    `(^#assv ,obj ,alist)))

(jazz:define-synto (%%assoc obj alist)
  (if jazz:debug-core?
      `(assoc ,obj ,alist)
    `(^#assoc ,obj ,alist)))

(jazz:define-synto (%%cons x y)
  (if jazz:debug-core?
      `(cons ,x ,y)
    `(^#cons ,x ,y)))

(jazz:define-synto (%%list . rest)
  (if jazz:debug-core?
      `(list ,@rest)
    `(^#list ,@rest)))

(jazz:define-synto (%%append x y)
  (if jazz:debug-core?
      `(append ,x ,y)
    `(^#append ,x ,y)))

(jazz:define-synto (%%remove elem lst)
  (%%force-uniqueness (elem lst)
    `(%%check-list ,lst 2 (%%remove ,elem ,lst)
       (^#remove ,elem ,lst))))

(jazz:define-synto (%%reverse lst)
  (if jazz:debug-core?
      `(reverse ,lst)
    `(^#reverse ,lst)))

(jazz:define-synto (%%list->vector lst)
  (if jazz:debug-core?
      `(list->vector ,lst)
    `(^#list->vector ,lst)))


;;;
;;;; Memory
;;;


(jazz:define-synto (%%gc)
  `(^#gc))

(jazz:define-synto (%%get-bytes-allocated! floats i)
  (%%force-uniqueness (floats i)
    `(%%check-f64vector ,floats 1 (%%get-bytes-allocated! ,floats ,i)
       (%%check-fixnum ,i 2 (%%get-bytes-allocated! ,floats ,i)
         (^#get-bytes-allocated! ,floats ,i)))))

(jazz:define-synto (%%get-live-percent)
  `(^#get-live-percent))


;;;
;;;; Number
;;;


(jazz:define-synto (%%number? obj)
  (if jazz:debug-core?
      `(number? ,obj)
    `(^#number? ,obj)))

(jazz:define-synto (%%integer? obj)
  (if jazz:debug-core?
      `(integer? ,obj)
    `(^#integer? ,obj)))

(jazz:define-synto (%%real? obj)
  (if jazz:debug-core?
      `(real? ,obj)
    `(^#real? ,obj)))

(jazz:define-synto (%%= x y)
  (if jazz:debug-core?
      `(= ,x ,y)
    `(^#= ,x ,y)))

(jazz:define-synto (%%+ x y)
  (if jazz:debug-core?
      `(+ ,x ,y)
    `(^#+ ,x ,y)))

(jazz:define-synto (%%- . rest)
  (if jazz:debug-core?
      `(- ,@rest)
    `(^#- ,@rest)))

(jazz:define-synto (%%* x y)
  (if jazz:debug-core?
      `(* ,x ,y)
    `(^#* ,x ,y)))

(jazz:define-synto (%%/ x y)
  (if jazz:debug-core?
      `(/ ,x ,y)
    `(^#/ ,x ,y)))

(jazz:define-synto (%%number->string n)
  (if jazz:debug-core?
      `(number->string ,n)
    `(^#number->string ,n)))

(jazz:define-synto (%%string->number str)
  (if jazz:debug-core?
      `(string->number ,str)
    `(^#string->number ,str)))


;;;
;;;; Object
;;;


(jazz:define-synto (%%object->string n)
  (if jazz:debug-core?
      `(object->string ,n)
    `(^#object->string ,n)))


;;;
;;;; Parameter
;;;


(jazz:define-synto (%%make-parameter . rest)
  `(%%tracking
     (make-parameter ,@rest)))


;;;
;;;; Path
;;;


(jazz:define-synto (%%path-expand path)
  (%%force-uniqueness (path)
    `(%%check-string ,path 1 (%%path-expand ,path)
       (^#path-expand ,path))))


;;;
;;;; Port
;;;


(jazz:define-synto (%%port? obj)
  (if jazz:debug-core?
      `(port? ,obj)
    `(^#port? ,obj)))

(jazz:define-synto (%%port-name port)
  (%%force-uniqueness (port)
    `(%%check-port ,port 1 (%%port-name ,port)
       (^#port-name ,port))))

(jazz:define-synto (%%port-name->container obj)
  `(^#port-name->container ,obj))

(jazz:define-synto (%%eof-object? obj)
  (if jazz:debug-core?
      `(eof-object? ,obj)
    `(^#eof-object? ,obj)))

(jazz:define-synto (%%input-port-names-set! port names)
  (%%force-uniqueness (port names)
    `(%%check-port ,port 1 (%%input-port-names-set! ,port ,names)
       (%%danger %%input-port-names-set!
         (^#vector-set! ,port 4 ,names)))))

(jazz:define-synto (%%input-port-line-set! port line)
  (%%force-uniqueness (port line)
    `(%%check-port ,port 1 (%%input-port-line-set! ,port ,line)
       `(%%check-fixnum ,line 2 (%%input-port-line-set! ,port ,line)
          (^#input-port-line-set! ,port ,line)))))

(jazz:define-synto (%%input-port-column-set! port col)
  (%%force-uniqueness (port col)
    `(%%check-port ,port 1 (%%input-port-column-set! ,port ,col)
       `(%%check-fixnum ,col 2 (%%input-port-column-set! ,port ,col)
          (^#input-port-column-set! ,port ,col)))))

(jazz:define-synto (%%read-all-as-a-begin-expr-from-port port readtable wrap unwrap start-syntax close-port?)
  (%%force-uniqueness (port readtable wrap unwrap start-syntax close-port?)
    `(%%check-port ,port 1 (%%read-all-as-a-begin-expr-from-port ,port ,readtable ,wrap ,unwrap ,start-syntax ,close-port?)
       (%%check-readtable ,readtable 2 (%%read-all-as-a-begin-expr-from-port ,port ,readtable ,wrap ,unwrap ,start-syntax ,close-port?)
         (^#read-all-as-a-begin-expr-from-port ,port ,readtable ,wrap ,unwrap ,start-syntax ,close-port?)))))

(jazz:define-synto (%%write obj port)
  (%%force-uniqueness (obj port)
    `(%%check-port ,port 2 (%%write ,obj ,port)
       (^#write ,obj ,port))))

(jazz:define-synto (%%write-string str port)
  (%%force-uniqueness (str port)
    `(%%check-string ,str 1 (%%write-string ,str ,port)
       (%%check-port ,port 2 (%%write-string ,str ,port)
         (^#write-string ,str ,port)))))

(jazz:define-synto (%%newline port)
  (%%force-uniqueness (port)
    `(%%check-port ,port 2 (%%newline ,port)
       (^#newline ,port))))

(jazz:define-synto (%%default-wr we obj)
  (%%force-uniqueness (we obj)
    `(%%check-writeenv ,we 1 (%%default-wr ,we ,obj)
       (^#default-wr ,we ,obj))))

(jazz:define-synto (%%wr-set! proc)
  (%%force-uniqueness (proc)
    `(%%check-procedure ,proc 1 (%%wr-set! ,proc)
       (^#wr-set! ,proc))))


;;;
;;;; Procedure
;;;


(jazz:define-synto (%%procedure? obj)
  (if jazz:debug-core?
      `(procedure? ,obj)
    `(^#procedure? ,obj)))

(jazz:define-synto (%%procedure-name procedure)
  (%%force-uniqueness (procedure)
    `(%%check-procedure ,procedure 1 (%%procedure-name ,procedure)
       (^#procedure-name ,procedure))))

(jazz:define-synto (%%procedure-locat procedure)
  (%%force-uniqueness (procedure)
    `(%%check-procedure ,procedure 1 (%%procedure-locat ,procedure)
       (^#procedure-locat ,procedure))))

(jazz:define-synto (%%interp-procedure? obj)
  `(^#interp-procedure? ,obj))


;;;
;;;; Process
;;;


(jazz:define-synto (%%process-statistics)
  `(^#process-statistics))

(jazz:define-synto (%%get-min-heap)
  `(^#get-min-heap))

(jazz:define-synto (%%set-min-heap! bytes)
  (%%force-uniqueness (bytes)
    `(%%check-fixnum ,bytes 1 (%%set-min-heap! ,bytes)
       (^#set-min-heap! ,bytes))))

(jazz:define-synto (%%get-max-heap)
  `(^#get-max-heap))

(jazz:define-synto (%%set-max-heap! bytes)
  (%%force-uniqueness (bytes)
    `(%%check-fixnum ,bytes 1 (%%set-max-heap! ,bytes)
       (^#set-max-heap! ,bytes))))


;;;
;;;; Rational
;;;


(jazz:define-synto (%%rational? obj)
  (if jazz:debug-core?
      `(rational? ,obj)
    `(^#rational? ,obj)))

(jazz:define-synto (%%ratnum? obj)
  `(^#ratnum? ,obj))

(jazz:define-synto (%%ratnum->flonum x)
  (%%force-uniqueness (x)
    `(%%check-ratnum ,x 1 (%%ratnum->flonum ,x)
       (^#ratnum->flonum ,x))))


;;;
;;;; Readenv
;;;


(jazz:define-synto (%%readenv? obj)
  `(macro-readenv? ,obj))

(jazz:define-synto (%%readenv-current-filepos readenv)
  (%%force-uniqueness (readenv)
    `(%%check-readenv ,readenv 1 (%%readenv-current-filepos ,readenv)
       (^#readenv-current-filepos ,readenv))))

(jazz:define-synto (%%build-list readenv allow-improper? start-pos close)
  (%%force-uniqueness (readenv allow-improper? start-pos close)
    `(%%check-readenv ,readenv 1 (%%build-list ,readenv ,allow-improper? ,start-pos ,close)
       (^#build-list ,readenv ,allow-improper? ,start-pos ,close))))

(jazz:define-synto (%%read-datum-or-label-or-none-or-dot readenv)
  (%%force-uniqueness (readenv)
    `(%%check-readenv ,readenv 1 (%%read-datum-or-label-or-none-or-dot ,readenv)
       (^#read-datum-or-label-or-none-or-dot ,readenv))))


;;;
;;;; Readtable
;;;


(jazz:define-synto (%%readtable? obj)
  (if jazz:debug-core?
      `(readtable? ,obj)
    `(^#readtable? ,obj)))

(jazz:define-synto (%%current-readtable)
  `(^#current-readtable))

(jazz:define-synto (%%make-standard-readtable)
  `(^#make-standard-readtable))

(jazz:define-synto (%%readtable-copy readtable)
  (%%force-uniqueness (readtable)
    `(%%check-readtable ,readtable 1 (%%readtable-copy ,readtable)
       (^#readtable-copy ,readtable))))

(jazz:define-synto (%%readtable-char-delimiter? readtable c)
  (%%force-uniqueness (readtable c)
    `(%%check-readtable ,readtable 1 (%%readtable-char-delimiter? ,readtable ,c)
       (%%check-char ,c 2 (%%readtable-char-delimiter? ,readtable ,c)
         (^#readtable-char-delimiter? ,readtable ,c)))))

(jazz:define-synto (%%readtable-char-delimiter?-set! readtable c delimiter?)
  (%%force-uniqueness (readtable c delimiter?)
    `(%%check-readtable ,readtable 1 (%%readtable-char-delimiter?-set! ,readtable ,c ,delimiter?)
       (%%check-char ,c 2 (%%readtable-char-delimiter?-set! ,readtable ,c ,delimiter?)
         (^#readtable-char-delimiter?-set! ,readtable ,c ,delimiter?)))))

(jazz:define-synto (%%readtable-char-handler readtable c)
  (%%force-uniqueness (readtable c)
    `(%%check-readtable ,readtable 1 (%%readtable-char-handler ,readtable ,c)
       (%%check-char ,c 2 (%%readtable-char-handler ,readtable ,c)
         (^#readtable-char-handler ,readtable ,c)))))

(jazz:define-synto (%%readtable-char-handler-set! readtable c handler)
  (%%force-uniqueness (readtable c handler)
    `(%%check-readtable ,readtable 1 (%%readtable-char-handler-set! ,readtable ,c ,handler)
       (%%check-char ,c 2 (%%readtable-char-handler-set! ,readtable ,c ,handler)
         (^#readtable-char-handler-set! ,readtable ,c ,handler)))))

(jazz:define-synto (%%readtable-char-sharp-handler readtable c)
  (%%force-uniqueness (readtable c)
    `(%%check-readtable ,readtable 1 (%%readtable-char-sharp-handler ,readtable ,c)
       (%%check-char ,c 2 (%%readtable-char-sharp-handler ,readtable ,c)
         (^#readtable-char-sharp-handler ,readtable ,c)))))

(jazz:define-synto (%%readtable-char-sharp-handler-set! readtable c handler)
  (%%force-uniqueness (readtable c handler)
    `(%%check-readtable ,readtable 1 (%%readtable-char-sharp-handler-set! ,readtable ,c ,handler)
       (%%check-char ,c 2 (%%readtable-char-sharp-handler-set! ,readtable ,c ,handler)
         (^#readtable-char-sharp-handler-set! ,readtable ,c ,handler)))))

(jazz:define-synto (%%readtable-char-class-set! readtable c delimiter? handler)
  (%%force-uniqueness (readtable c delimiter? handler)
    `(%%check-readtable ,readtable 1 (%%readtable-char-class-set! ,readtable ,c ,delimiter? ,handler)
       (%%check-char ,c 2 (%%readtable-char-class-set! ,readtable ,c ,delimiter? ,handler)
         (^#readtable-char-class-set! ,readtable ,c ,delimiter? ,handler)))))

(jazz:define-synto (%%readtable-escaped-char-table readtable)
  (%%force-uniqueness (readtable)
    `(%%check-readtable ,readtable 1 (%%readtable-escaped-char-table ,readtable)
       (%%danger %%readtable-escaped-char-table
         (^#vector-ref ,readtable 3)))))

(jazz:define-synto (%%readtable-escaped-char-table-set! readtable table)
  (%%force-uniqueness (readtable table)
    `(%%check-readtable ,readtable 1 (%%readtable-escaped-char-table-set! ,readtable ,table)
       (%%danger %%readtable-escaped-char-table-set!
         (^#vector-set! ,readtable 3 ,table)))))


;;;
;;;; Repl
;;;


(jazz:define-synto (%%repl . rest)
  (let ((write-reason (if (##null? rest) #f (##car rest))))
    `(^#repl ,write-reason)))

(jazz:define-synto (%%repl-debug . rest)
  (let ((write-reason (if (##null? rest) #f (##car rest)))
        (toplevel? (if (or (##null? rest) (##null? (##cdr rest))) #f (##cadr rest))))
    `(^#repl-debug ,write-reason ,toplevel?)))

(jazz:define-synto (%%thread-repl-context-get!)
  `(^#thread-repl-context-get!))

(jazz:define-synto (%%thread-repl-channel-get! thread)
  (%%force-uniqueness (thread)
    `(%%check-thread ,thread 1 (%%thread-repl-channel-get! ,thread)
       (^#thread-repl-channel-get! ,thread))))

(jazz:define-synto (%%repl-channel-result-history-add channel result)
  `(^#repl-channel-result-history-add ,channel ,result))


;;;
;;;; Six
;;;


(jazz:define-synto (%%six-types-ref)
  `##six-types)


(jazz:define-synto (%%six-types-set! lst)
  (%%force-uniqueness (lst)
    `(%%check-list ,lst 1 (%%six-types-set! ,lst)
       (^#six-types-set! ,lst))))


;;;
;;;; String
;;;


(jazz:define-synto (%%string? obj)
  (if jazz:debug-core?
      `(string? ,obj)
    `(^#string? ,obj)))

(jazz:define-synto (%%string . rest)
  (if jazz:debug-core?
      `(string ,@rest)
    `(^#string ,@rest)))

(jazz:define-synto (%%make-string size . rest)
  `(%%tracking
     (make-string ,size ,@rest)))

(jazz:define-synto (%%string=? str1 str2)
  (if jazz:debug-core?
      `(string=? ,str1 ,str2)
    `(^#string=? ,str1 ,str2)))

(jazz:define-synto (%%string-ci=? str1 str2)
  (if jazz:debug-core?
      `(string-ci=? ,str1 ,str2)
    `(^#string-ci=? ,str1 ,str2)))

(jazz:define-synto (%%string<? str1 str2)
  (if jazz:debug-core?
      `(string<? ,str1 ,str2)
    `(^#string<? ,str1 ,str2)))

(jazz:define-synto (%%string-length str)
  (if jazz:debug-core?
      `(string-length ,str)
    `(^#string-length ,str)))

(jazz:define-synto (%%string-ref str pos)
  (if jazz:debug-core?
      `(string-ref ,str ,pos)
    `(^#string-ref ,str ,pos)))

(jazz:define-synto (%%string-set! str pos val)
  (if jazz:debug-core?
      `(string-set! ,str ,pos ,val)
    `(^#string-set! ,str ,pos ,val)))

(jazz:define-synto (%%substring str start end)
  (if jazz:debug-core?
      `(substring ,str ,start ,end)
    `(^#substring ,str ,start ,end)))

(jazz:define-synto (%%string-append . rest)
  (if jazz:debug-core?
      `(string-append ,@rest)
    `(^#string-append ,@rest)))

(jazz:define-synto (%%string-shrink! str len)
  (%%force-uniqueness (str len)
    `(%%check-string ,str 1 (%%string-shrink! ,str ,len)
       (%%check-fixnum ,len 2 (%%string-shrink! ,str ,len)
         (^#string-shrink! ,str ,len)))))


;;;
;;;; Structure
;;;


(jazz:define-synto (%%structure? obj)
  `(^#structure? ,obj))

(jazz:define-synto (%%structure-type structure)
  (%%force-uniqueness (structure)
    `(%%check-structure ,structure 1 (%%structure-type ,structure)
       (^#structure-type ,structure))))

(jazz:define-synto (%%structure-ref structure i type proc)
  (%%force-uniqueness (structure)
    `(%%check-structure ,structure 1 (%%structure-ref ,structure ,i ,type ,proc)
       (^#structure-ref ,structure ,i ,type ,proc))))

(jazz:define-synto (%%structure-set! structure val i type proc)
  (%%force-uniqueness (structure)
    `(%%check-structure ,structure 1 (%%structure-set! ,structure ,val ,i ,type ,proc)
       (^#structure-set! ,structure ,val ,i ,type ,proc))))


;;;
;;;; Symbol
;;;


(jazz:define-synto (%%symbol? obj)
  (if jazz:debug-core?
      `(symbol? ,obj)
    `(^#symbol? ,obj)))

(jazz:define-synto (%%string->symbol str)
  (if jazz:debug-core?
      `(string->symbol ,str)
    `(^#string->symbol ,str)))

(jazz:define-synto (%%symbol->string symbol)
  (if jazz:debug-core?
      `(symbol->string ,symbol)
    `(^#symbol->string ,symbol)))

(jazz:define-synto (%%unbound? obj)
  `(^#unbound? ,obj))

(jazz:define-synto (%%global-var? symbol)
  (%%force-uniqueness (symbol)
    `(%%check-symbol ,symbol 1 (%%global-var? ,symbol)
       (^#global-var? ,symbol))))

(jazz:define-synto (%%global-var-ref symbol)
  (%%force-uniqueness (symbol)
    `(%%check-symbol ,symbol 1 (%%global-var-ref ,symbol)
       (^#global-var-ref ,symbol))))

(jazz:define-synto (%%global-var-set! symbol value)
  (%%force-uniqueness (symbol)
    `(%%check-symbol ,symbol 1 (%%global-var-set! ,symbol ,value)
       (^#global-var-set! ,symbol ,value))))

(jazz:define-synto (%%global-var-unbind! symbol)
  (%%force-uniqueness (symbol)
    `(%%check-symbol ,symbol 1 (%%global-var-unbind! ,symbol)
       (^#global-var-set! ,symbol #!unbound))))

(jazz:define-synto (%%symbol-table)
  `(^#symbol-table))


;;;
;;;; Source
;;;


(jazz:define-synto (%%source? expr)
  `(^#source? ,expr))

(jazz:define-synto (%%source-code src)
  (%%force-uniqueness (src)
    `(%%check-source ,src 1 (%%source-code ,src)
       (^#source-code ,src))))

(jazz:define-synto (%%source-locat src)
  (%%force-uniqueness (src)
    `(%%check-source ,src 1 (%%source-locat ,src)
       (^#source-locat ,src))))

(jazz:define-synto (%%desourcify expr)
  `(^#desourcify ,expr))

(jazz:define-synto (%%make-source code locat)
  (%%force-uniqueness (code locat)
    `(%%check-locat ,locat 2 (%%make-source ,code ,locat)
       (^#make-source ,code ,locat))))

(jazz:define-synto (%%sourcify expr src)
  (%%force-uniqueness (expr src)
    `(%%check-source ,src 2 (%%sourcify ,expr ,src)
       (^#sourcify ,expr ,src))))

(jazz:define-synto (%%sourcify-deep expr src)
  (%%force-uniqueness (expr src)
    `(%%check-source ,src 2 (%%sourcify-deep ,expr ,src)
       (^#sourcify-deep ,expr ,src))))

(jazz:define-synto (%%locat? expr)
  `(^#locat? ,expr))

(jazz:define-synto (%%make-locat container position)
  (%%force-uniqueness (position)
    `(%%check-fixnum ,position 2 (%%make-locat ,container ,position)
       (^#make-locat ,container ,position))))

(jazz:define-synto (%%locat-container locat)
  (%%force-uniqueness (locat)
    `(%%check-locat ,locat 1 (%%locat-container ,locat)
       (^#locat-container ,locat))))

(jazz:define-synto (%%locat-position locat)
  (%%force-uniqueness (locat)
    `(%%check-locat ,locat 1 (%%locat-position ,locat)
       (^#locat-position ,locat))))

(jazz:define-synto (%%container->path container)
  `(^#container->path ,container))

(jazz:define-synto (%%position->filepos position)
  (%%force-uniqueness (position)
    `(%%check-fixnum ,position 1 (%%position->filepos ,position)
       (^#position->filepos ,position))))

(jazz:define-synto (%%filepos->position filepos)
  (%%force-uniqueness (filepos)
    `(%%check-fixnum ,filepos 1 (%%filepos->position ,filepos)
       (^#filepos->position ,filepos))))

(jazz:define-synto (%%filepos-line filepos)
  (%%force-uniqueness (filepos)
    `(%%check-fixnum ,filepos 1 (%%filepos-line ,filepos)
       (^#filepos-line ,filepos))))

(jazz:define-synto (%%filepos-col filepos)
  (%%force-uniqueness (filepos)
    `(%%check-fixnum ,filepos 1 (%%filepos-col ,filepos)
       (^#filepos-col ,filepos))))


;;;
;;;; Table
;;;


(jazz:define-synto (%%table? obj)
  `(table? ,obj))

(jazz:define-synto (%%make-table . rest)
  `(%%tracking
     (make-table ,@rest)))

(jazz:define-synto (%%table-ref table key . rest)
  (if jazz:debug-core?
      `(table-ref ,table ,key ,@rest)
    `(^#table-ref ,table ,key ,@rest)))

(jazz:define-synto (%%table-set! table key value)
  (if jazz:debug-core?
      `(table-set! ,table ,key ,value)
    `(^#table-set! ,table ,key ,value)))

(jazz:define-synto (%%table-clear table key)
  `(table-set! ,table ,key))

(jazz:define-synto (%%table-length table)
  `(table-length ,table))

(jazz:define-synto (%%table-for-each proc table)
  `(table-for-each ,proc ,table))

(jazz:define-synto (%%table-merge! table additions . rest)
  (let ((additions-takes-precedence? (if (##null? rest) #f (##car rest))))
    (if jazz:debug-core?
        `(table-merge! ,table ,additions ,additions-takes-precedence?)
      `(^#table-merge! ,table ,additions ,additions-takes-precedence?))))

(jazz:define-synto (%%list->table alist . rest)
  `(list->table ,alist ,@rest))

(jazz:define-synto (%%table->list table)
  `(table->list ,table))

(jazz:define-synto (%%copy-table table)
  `(table-copy ,table))


(jazz:define-synto (%%gc-hash-table? obj)
  `(^#gc-hash-table? ,obj))


;;;
;;;; Thread
;;;


(jazz:define-synto (%%thread? obj)
  `(thread? ,obj))

(jazz:define-synto (%%make-thread . rest)
  `(%%tracking
     (make-thread ,@rest)))

(jazz:define-synto (%%primordial-thread-ref)
  `##primordial-thread)

(jazz:define-synto (%%current-thread)
  `(^#current-thread))

(jazz:define-synto (%%make-mutex . rest)
  `(%%tracking
     (make-mutex ,@rest)))

(jazz:define-synto (%%make-condition-variable . rest)
  `(%%tracking
     (make-condition-variable ,@rest)))


;;;
;;;; Time
;;;


(jazz:define-synto (%%get-current-time! floats i)
  (%%force-uniqueness (floats i)
    `(%%check-f64vector ,floats 1 (%%get-current-time! ,floats ,i)
       (%%check-fixnum ,i 2 (%%get-current-time! ,floats ,i)
         (^#get-current-time! ,floats ,i)))))


;;;
;;;; Type
;;;


(jazz:define-synto (%%type? obj)
  `(^#type? ,obj))

(jazz:define-synto (%%type-id type)
  (%%force-uniqueness (type)
    `(%%check-type ,type 1 (%%type-id ,type)
       (^#type-id type))))

(jazz:define-synto (%%type-name type)
  (%%force-uniqueness (type)
    `(%%check-type ,type 1 (%%type-name ,type)
       (^#type-name type))))

(jazz:define-synto (%%type-flags type)
  (%%force-uniqueness (type)
    `(%%check-type ,type 1 (%%type-flags ,type)
       (^#type-flags type))))

(jazz:define-synto (%%type-super type)
  (%%force-uniqueness (type)
    `(%%check-type ,type 1 (%%type-super ,type)
       (^#type-super type))))

(jazz:define-synto (%%type-field-count type)
  (%%force-uniqueness (type)
    `(%%check-type ,type 1 (%%type-field-count ,type)
       (^#type-field-count type))))

(jazz:define-synto (%%type-all-fields type)
  (%%force-uniqueness (type)
    `(%%check-type ,type 1 (%%type-all-fields ,type)
       (^#type-all-fields type))))


;;;
;;;; Values
;;;


(jazz:define-synto (%%values? obj)
  `(^#values? ,obj))


;;;
;;;; Vector
;;;


(jazz:define-synto (%%vector? obj)
  (if jazz:debug-core?
      `(vector? ,obj)
    `(^#vector? ,obj)))

(jazz:define-synto (%%vector . rest)
  (if jazz:debug-core?
      `(vector ,@rest)
    `(^#vector ,@rest)))

(jazz:define-synto (%%vector-length vector)
  (if jazz:debug-core?
      `(vector-length ,vector)
    `(^#vector-length ,vector)))

(jazz:define-synto (%%vector-ref vector n)
  (if jazz:debug-core?
      `(vector-ref ,vector ,n)
    `(^#vector-ref ,vector ,n)))

(jazz:define-synto (%%vector-set! vector n value)
  (if jazz:debug-core?
      `(vector-set! ,vector ,n ,value)
    `(^#vector-set! ,vector ,n ,value)))

(jazz:define-synto (%%vector-copy vector . rest)
  (if jazz:debug-core?
      `(vector-copy ,vector ,@rest)
    `(^#vector-copy ,vector ,@rest)))

(jazz:define-synto (%%vector->list vector)
  (if jazz:debug-core?
      `(vector->list ,vector)
    `(^#vector->list ,vector)))

(jazz:define-synto (%%s8vector? obj)
  (if jazz:debug-core?
      `(s8vector? ,obj)
    `(^#s8vector? ,obj)))

(jazz:define-synto (%%u8vector? obj)
  (if jazz:debug-core?
      `(u8vector? ,obj)
    `(^#u8vector? ,obj)))

(jazz:define-synto (%%s16vector? obj)
  (if jazz:debug-core?
      `(s16vector? ,obj)
    `(^#s16vector? ,obj)))

(jazz:define-synto (%%u16vector? obj)
  (if jazz:debug-core?
      `(u16vector? ,obj)
    `(^#u16vector? ,obj)))

(jazz:define-synto (%%s32vector? obj)
  (if jazz:debug-core?
      `(s32vector? ,obj)
    `(^#s32vector? ,obj)))

(jazz:define-synto (%%u32vector? obj)
  (if jazz:debug-core?
      `(u32vector? ,obj)
    `(^#u32vector? ,obj)))

(jazz:define-synto (%%s64vector? obj)
  (if jazz:debug-core?
      `(s64vector? ,obj)
    `(^#s64vector? ,obj)))

(jazz:define-synto (%%u64vector? obj)
  (if jazz:debug-core?
      `(u64vector? ,obj)
    `(^#u64vector? ,obj)))

(jazz:define-synto (%%f32vector? obj)
  (if jazz:debug-core?
      `(f32vector? ,obj)
    `(^#f32vector? ,obj)))

(jazz:define-synto (%%f64vector? obj)
  (if jazz:debug-core?
      `(f64vector? ,obj)
    `(^#f64vector? ,obj)))


;;;
;;;; Will
;;;


(jazz:define-synto (%%make-will . rest)
  `(%%tracking
     (make-will ,@rest))))
