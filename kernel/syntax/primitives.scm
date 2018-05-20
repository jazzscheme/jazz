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


(jazz:kernel-declares)


;;;
;;;; Boolean
;;;


(jazz:define-macro (%%boolean? obj)
  (if jazz:debug-core?
      `(boolean? ,obj)
    `(^#boolean? ,obj)))

(jazz:define-macro (%%not expr)
  (if jazz:debug-core?
      `(not ,expr)
    `(^#not ,expr)))


;;;
;;;; Box
;;;


(jazz:define-macro (%%box? obj)
  (if jazz:debug-core?
      `(box? ,obj)
    `(^#box? ,obj)))

(jazz:define-macro (%%box obj)
  (if jazz:debug-core?
      `(box ,obj)
    `(^#box ,obj)))

(jazz:define-macro (%%unbox box)
  (if jazz:debug-core?
      `(unbox ,box)
    `(^#unbox ,box)))


;;;
;;;; Char
;;;


(jazz:define-macro (%%char? obj)
  (if jazz:debug-core?
      `(char? ,obj)
    `(^#char? ,obj)))

(jazz:define-macro (%%char=? c1 c2)
  (if jazz:debug-core?
      `(char=? ,c1 ,c2)
    `(^#char=? ,c1 ,c2)))

(jazz:define-macro (%%char<=? c1 c2)
  (if jazz:debug-core?
      `(char<=? ,c1 ,c2)
    `(^#char<=? ,c1 ,c2)))


;;;
;;;; Closure
;;;


(jazz:define-syntax %%closure?
  (lambda (src)
    (let ((obj (cadr (jazz:source-code src))))
      `(^#closure? ,obj))))

(jazz:define-macro (%%closure-code closure)
  (%%force-uniqueness (closure)
    `(%%check-closure ,closure 1 (%%closure-code ,closure)
       (^#closure-code ,closure))))

(jazz:define-macro (%%closure-length closure)
  (%%force-uniqueness (closure)
    `(%%check-closure ,closure 1 (%%closure-length ,closure)
       (^#closure-length ,closure))))

(jazz:define-macro (%%closure-ref closure n)
  (%%force-uniqueness (closure)
    `(%%check-closure ,closure 1 (%%closure-ref ,closure ,n)
       (%%check-fixnum ,n 2 (%%closure-ref ,closure ,n)
         (^#closure-ref ,closure ,n)))))


;;;
;;;; Complex
;;;


(jazz:define-macro (%%complex? obj)
  (if jazz:debug-core?
      `(complex? ,obj)
    `(^#complex? ,obj)))


;;;
;;;; Container
;;;


(jazz:define-macro (%%path->container-hook-set! hook)
  (%%force-uniqueness (hook)
    `(%%check-procedure ,hook 1 (%%path->container-hook-set! ,hook)
       (^#path->container-hook-set! ,hook))))

(jazz:define-macro (%%container->path-hook-set! hook)
  (%%force-uniqueness (hook)
    `(%%check-procedure ,hook 1 (%%container->path-hook-set! ,hook)
       (^#container->path-hook-set! ,hook))))

(jazz:define-macro (%%container->id-hook-set! hook)
  (%%force-uniqueness (hook)
    `(%%check-procedure ,hook 1 (%%container->id-hook-set! ,hook)
       (^#container->id-hook-set! ,hook))))


;;;
;;;; Continuation
;;;


(jazz:define-syntax %%continuation?
  (lambda (src)
    (let ((obj (cadr (jazz:source-code src))))
      (if jazz:debug-core?
          `(continuation? ,obj)
        `(^#continuation? ,obj)))))

(jazz:define-syntax %%continuation-capture
  (lambda (src)
    (let ((proc (cadr (jazz:source-code src))))
      (if jazz:debug-core?
          `(continuation-capture ,proc)
        `(^#continuation-capture ,proc)))))

(jazz:define-syntax %%continuation-graft
  (lambda (src)
    (let ((cont (cadr (jazz:source-code src)))
          (proc (car (cddr (jazz:source-code src)))))
      (if jazz:debug-core?
          `(continuation-graft ,cont ,proc)
        `(^#continuation-graft ,cont ,proc)))))

(jazz:define-syntax %%continuation-return
  (lambda (src)
    (let ((cont (cadr (jazz:source-code src)))
          (values (cddr (jazz:source-code src))))
      (if jazz:debug-core?
          `(continuation-return ,cont ,@values)
        `(^#continuation-return ,cont ,@values)))))

(jazz:define-macro (%%continuation-graft-no-winding cont values)
  (%%force-uniqueness (cont values)
    `(%%check-continuation ,cont 1 (%%continuation-graft-no-winding ,cont ,values)
       (^#continuation-graft-no-winding ,cont ,values))))

(jazz:define-macro (%%continuation-return-no-winding cont values)
  (%%force-uniqueness (cont values)
    `(%%check-continuation ,cont 1 (%%continuation-return-no-winding ,cont ,values)
       (^#continuation-return-no-winding ,cont ,values))))

(jazz:define-macro (%%continuation-parent cont)
  (%%force-uniqueness (cont)
    `(%%check-continuation ,cont 1 (%%continuation-parent ,cont)
       (^#continuation-parent ,cont))))

(jazz:define-macro (%%continuation-creator cont)
  (%%force-uniqueness (cont)
    `(%%check-continuation ,cont 1 (%%continuation-creator ,cont)
       (^#continuation-creator ,cont))))

(jazz:define-macro (%%continuation-locat cont)
  (%%force-uniqueness (cont)
    `(%%check-continuation ,cont 1 (%%continuation-locat ,cont)
       (^#continuation-locat ,cont))))

(jazz:define-macro (%%continuation-locals cont)
  (%%force-uniqueness (cont)
    `(%%check-continuation ,cont 1 (%%continuation-locals ,cont)
       (^#continuation-locals ,cont))))

(jazz:define-macro (%%continuation-next cont)
  (%%force-uniqueness (cont)
    `(%%check-continuation ,cont 1 (%%continuation-next ,cont)
       (^#continuation-next ,cont))))

(jazz:define-macro (%%continuation-first-frame cont all-frames?)
  (%%force-uniqueness (cont all-frames?)
    `(%%check-continuation ,cont 1 (%%continuation-first-frame ,cont ,all-frames?)
       (^#continuation-first-frame ,cont ,all-frames?))))

(jazz:define-macro (%%continuation-next-frame cont all-frames?)
  (%%force-uniqueness (cont all-frames?)
    `(%%check-continuation ,cont 1 (%%continuation-next-frame ,cont ,all-frames?)
       (^#continuation-next-frame ,cont ,all-frames?))))

(jazz:define-macro (%%interp-continuation? cont)
  (%%force-uniqueness (cont)
    `(%%check-continuation ,cont 1 (%%interp-continuation? ,cont)
       (^#interp-continuation? ,cont))))


;;;
;;;; Control
;;;


(jazz:define-macro (%%apply proc lst)
  (if jazz:debug-core?
      `(apply ,proc ,lst)
    `(^#apply ,proc ,lst)))


;;;
;;;; Equality
;;;


(jazz:define-macro (%%eq? x y)
  (if jazz:debug-core?
      `(eq? ,x ,y)
    `(^#eq? ,x ,y)))

(jazz:define-macro (%%neq? x y)
  `(%%not (%%eq? ,x ,y)))

(jazz:define-macro (%%eqv? x y)
  (if jazz:debug-core?
      `(eqv? ,x ,y)
    `(^#eqv? ,x ,y)))

(jazz:define-macro (%%equal? x y)
  (if jazz:debug-core?
      `(equal? ,x ,y)
    `(^#equal? ,x ,y)))


;;;
;;;; Eval
;;;


;; DANGER : temporary hack until proper primitive exists
(jazz:define-macro (%%load path script-callback clone-cte? raise-os-exception? linker-name quiet?)
  `(^#load ,path ,script-callback ,clone-cte? ,raise-os-exception? ,linker-name ,quiet?))


;;;
;;;; Exact
;;;


(jazz:define-macro (%%inexact->exact obj)
  (if jazz:debug-core?
      `(inexact->exact ,obj)
    `(^#inexact->exact ,obj)))


;;;
;;;; Exception
;;;


(jazz:define-macro (%%exception->locat exc cont)
  (%%force-uniqueness (exc cont)
    `(%%check-continuation ,cont 2 (%%exception->locat ,exc ,cont)
       (^#exception->locat ,exc ,cont))))


(jazz:define-macro (%%display-exception-hook-ref)
  `##display-exception-hook)

(jazz:define-macro (%%display-exception-hook-set! hook)
  (%%force-uniqueness (hook)
    `(%%check-procedure ,hook 1 (%%display-exception-hook-set! ,hook)
       (^#display-exception-hook-set! ,hook))))


(jazz:define-macro (%%raise-heap-overflow-exception)
  `(##raise-heap-overflow-exception))


;;;
;;;; Features
;;;


(jazz:define-macro (%%cond-expand-features . rest)
  `(^#cond-expand-features ,@rest))


;;;
;;;; Fixnum
;;;


(jazz:define-macro (%%fixnum? obj)
  (if jazz:debug-core?
      `(fixnum? ,obj)
    `(^#fixnum? ,obj)))

(jazz:define-macro (%%fixnum->flonum x)
  (%%force-uniqueness (x)
    `(%%check-fixnum ,x 1 (%%fixnum->flonum ,x)
       (^#fixnum->flonum ,x))))

(jazz:define-macro (%%fx= . rest)
  (if jazz:debug-core?
      `(= ,@rest)
    `(^#fx= ,@rest)))

(jazz:define-macro (%%fx< . rest)
  (if jazz:debug-core?
      `(< ,@rest)
    `(^#fx< ,@rest)))

(jazz:define-macro (%%fx<= . rest)
  (if jazz:debug-core?
      `(<= ,@rest)
    `(^#fx<= ,@rest)))

(jazz:define-macro (%%fx> . rest)
  (if jazz:debug-core?
      `(> ,@rest)
    `(^#fx> ,@rest)))

(jazz:define-macro (%%fx>= . rest)
  (if jazz:debug-core?
      `(>= ,@rest)
    `(^#fx>= ,@rest)))

(jazz:define-macro (%%fx+ . rest)
  (if jazz:debug-core?
      `(+ ,@rest)
    `(^#fx+ ,@rest)))

(jazz:define-macro (%%fx- . rest)
  (if jazz:debug-core?
      `(- ,@rest)
    `(^#fx- ,@rest)))

(jazz:define-macro (%%fx* . rest)
  (if jazz:debug-core?
      `(* ,@rest)
    `(^#fx* ,@rest)))

(jazz:define-macro (%%fxabs x)
  (if jazz:debug-core?
      `(fxabs ,x)
    `(^#fxabs ,x)))

(jazz:define-macro (%%fxquotient x y)
  (if jazz:debug-core?
      `(quotient ,x ,y)
    `(^#fxquotient ,x ,y)))

(jazz:define-macro (%%fxmin . rest)
  (if jazz:debug-core?
      `(fxmin ,@rest)
    `(^#fxmin ,@rest)))

(jazz:define-macro (%%fxmax . rest)
  (if jazz:debug-core?
      `(fxmax ,@rest)
    `(^#fxmax ,@rest)))

(jazz:define-macro (%%fxmodulo . rest)
  (if jazz:debug-core?
      `(fxmodulo ,@rest)
    `(^#fxmodulo ,@rest)))

(jazz:define-macro (%%fxeven? . rest)
  (if jazz:debug-core?
      `(fxeven? ,@rest)
    `(^#fxeven? ,@rest)))

(jazz:define-macro (%%fxodd? . rest)
  (if jazz:debug-core?
      `(fxodd? ,@rest)
    `(^#fxodd? ,@rest)))

(jazz:define-macro (%%fxnot . rest)
  (if jazz:debug-core?
      `(fxnot ,@rest)
    `(^#fxnot ,@rest)))

(jazz:define-macro (%%fxand . rest)
  (if jazz:debug-core?
      `(fxand ,@rest)
    `(^#fxand ,@rest)))

(jazz:define-macro (%%fxior . rest)
  (if jazz:debug-core?
      `(fxior ,@rest)
    `(^#fxior ,@rest)))

(jazz:define-macro (%%fxxor . rest)
  (if jazz:debug-core?
      `(fxxor ,@rest)
    `(^#fxxor ,@rest)))

(jazz:define-macro (%%fxarithmetic-shift . rest)
  (if jazz:debug-core?
      `(fxarithmetic-shift ,@rest)
    `(^#fxarithmetic-shift ,@rest)))

(jazz:define-macro (%%fxarithmetic-shift-left . rest)
  (if jazz:debug-core?
      `(fxarithmetic-shift-left ,@rest)
    `(^#fxarithmetic-shift-left ,@rest)))

(jazz:define-macro (%%fxarithmetic-shift-right . rest)
  (if jazz:debug-core?
      `(fxarithmetic-shift-right ,@rest)
    `(^#fxarithmetic-shift-right ,@rest)))

(jazz:define-macro (%%fxbetween? n lower upper)
  (%%force-uniqueness (n)
    `(and (>= ,n ,lower)
          (<= ,n ,upper))))


;;;
;;;; Flonum
;;;


(jazz:define-macro (%%flonum? obj)
  (if jazz:debug-flonum?
      `(flonum? ,obj)
    `(^#flonum? ,obj)))

(jazz:define-macro (%%flnan? obj)
  (if jazz:debug-flonum?
      `(flnan? ,obj)
    `(^#flnan? ,obj)))

(jazz:define-macro (%%flonum->fixnum x)
  (%%force-uniqueness (x)
    `(%%check-flonum ,x 1 (%%flonum->fixnum ,x)
       (^#flonum->fixnum ,x))))

(jazz:define-macro (%%fl= . rest)
  (if jazz:debug-flonum?
      `(= ,@rest)
    `(^#fl= ,@rest)))

(jazz:define-macro (%%fl< . rest)
  (if jazz:debug-flonum?
      `(< ,@rest)
    `(^#fl< ,@rest)))

(jazz:define-macro (%%fl<= . rest)
  (if jazz:debug-flonum?
      `(<= ,@rest)
    `(^#fl<= ,@rest)))

(jazz:define-macro (%%fl> . rest)
  (if jazz:debug-flonum?
      `(> ,@rest)
    `(^#fl> ,@rest)))

(jazz:define-macro (%%fl>= . rest)
  (if jazz:debug-flonum?
      `(>= ,@rest)
    `(^#fl>= ,@rest)))

(jazz:define-macro (%%fl+ . rest)
  (if jazz:debug-flonum?
      `(+ ,@rest)
    `(^#fl+ ,@rest)))

(jazz:define-macro (%%fl- . rest)
  (if jazz:debug-flonum?
      `(- ,@rest)
    `(^#fl- ,@rest)))

(jazz:define-macro (%%fl* . rest)
  (if jazz:debug-flonum?
      `(* ,@rest)
    `(^#fl* ,@rest)))

(jazz:define-macro (%%fl/ . rest)
  (if jazz:debug-flonum?
      `(/ ,@rest)
    `(^#fl/ ,@rest)))

(jazz:define-macro (%%flfloor x)
  (if jazz:debug-flonum?
      `(flfloor ,x)
    `(^#flfloor ,x)))

(jazz:define-macro (%%flceiling x)
  (if jazz:debug-flonum?
      `(flceiling ,x)
    `(^#flceiling ,x)))

(jazz:define-macro (%%fltruncate x)
  (if jazz:debug-flonum?
      `(fltruncate ,x)
    `(^#fltruncate ,x)))

(jazz:define-macro (%%flround x)
  (if jazz:debug-flonum?
      `(flround ,x)
    `(^#flround ,x)))

(jazz:define-macro (%%flabs x)
  (if jazz:debug-flonum?
      `(flabs ,x)
    `(^#flabs ,x)))

(jazz:define-macro (%%flsqrt x)
  (if jazz:debug-flonum?
      `(flsqrt ,x)
    `(^#flsqrt ,x)))

(jazz:define-macro (%%flexpt x y)
  (if jazz:debug-flonum?
      `(flexpt ,x ,y)
    `(^#flexpt ,x ,y)))

(jazz:define-macro (%%flsquare x)
  (if jazz:debug-flonum?
      `(flsquare ,x)
    `(^#flsquare ,x)))

(jazz:define-macro (%%flsin x)
  (if jazz:debug-flonum?
      `(flsin ,x)
    `(^#flsin ,x)))

(jazz:define-macro (%%flcos x)
  (if jazz:debug-flonum?
      `(flcos ,x)
    `(^#flcos ,x)))

(jazz:define-macro (%%fltan x)
  (if jazz:debug-flonum?
      `(fltan ,x)
    `(^#fltan ,x)))

(jazz:define-macro (%%flasin x)
  (if jazz:debug-flonum?
      `(flasin ,x)
    `(^#flasin ,x)))

(jazz:define-macro (%%flacos x)
  (if jazz:debug-flonum?
      `(flacos ,x)
    `(^#flacos ,x)))

(jazz:define-macro (%%flatan . rest)
  (if jazz:debug-flonum?
      `(flatan ,@rest)
    `(^#flatan ,@rest)))

(jazz:define-macro (%%flmin . rest)
  (if jazz:debug-flonum?
      `(flmin ,@rest)
    `(^#flmin ,@rest)))

(jazz:define-macro (%%flmax . rest)
  (if jazz:debug-flonum?
      `(flmax ,@rest)
    `(^#flmax ,@rest)))

(jazz:define-macro (%%flalloc)
  `(^#subtype-set! (^#f64vector 0.) jazz:subtype-flonum))

(jazz:define-macro (%%flref fl ignore)
  `(^#f64vector-ref ,fl 0))

(jazz:define-macro (%%flset! fl ignore val)
  `(^#f64vector-set! ,fl 0 ,val))

(jazz:define-macro (%%flonum->exact-int fl)
  (%%force-uniqueness (fl)
    `(%%check-flonum ,fl 1 (%%flonum->exact-int ,fl)
       (^#flonum->exact-int ,fl))))


;;;
;;;; Foreign
;;;


(jazz:define-macro (%%foreign? obj)
  `(^#foreign? ,obj))

;(jazz:define-macro (%%still-obj-refcount foreign)
;  )

(jazz:define-macro (%%still-obj-refcount-dec! foreign)
  (%%force-uniqueness (foreign)
    `(%%check-foreign ,foreign 1 (%%still-obj-refcount-dec! ,foreign)
       (^#still-obj-refcount-dec! ,foreign))))

(jazz:define-macro (%%still-obj-refcount-inc! foreign)
  (%%force-uniqueness (foreign)
    `(%%check-foreign ,foreign 1 (%%still-obj-refcount-inc! ,foreign)
       (^#still-obj-refcount-inc! ,foreign))))

(jazz:define-macro (%%still-copy obj)
  `(^#still-copy ,obj))


;;;
;;;; GC
;;;


(jazz:define-macro (%%add-gc-interrupt-job! thunk)
  (%%force-uniqueness (thunk)
    `(%%check-procedure ,thunk 1 (%%add-gc-interrupt-job! ,thunk)
       (^#add-gc-interrupt-job! ,thunk))))

(jazz:define-macro (%%clear-gc-interrupt-jobs!)
  `(^#clear-gc-interrupt-jobs!))


;;;
;;;; Heartbeat
;;;


(jazz:define-macro (%%get-heartbeat-interval! u64vect i)
  (%%force-uniqueness (u64vect i)
    `(%%check-f64vector ,u64vect 1 (%%get-heartbeat-interval! ,u64vect ,i)
       (%%check-fixnum ,i 2 (%%get-heartbeat-interval! ,u64vect ,i)
         (^#get-heartbeat-interval! ,u64vect ,i)))))

(jazz:define-macro (%%set-heartbeat-interval! seconds)
  (%%force-uniqueness (seconds)
    `(%%check-flonum ,seconds 1 (%%set-heartbeat-interval! ,seconds)
       (^#set-heartbeat-interval! ,seconds))))


;;;
;;;; Homogeneous
;;;


;; use at your own risk versions that do not initialize memory
(jazz:define-macro (%%allocate-vector    size . rest) `(%%tracking (^#make-vector    ,size ,@rest)))
(jazz:define-macro (%%allocate-s8vector  size . rest) `(%%tracking (^#make-s8vector  ,size ,@rest)))
(jazz:define-macro (%%allocate-u8vector  size . rest) `(%%tracking (^#make-u8vector  ,size ,@rest)))
(jazz:define-macro (%%allocate-s16vector size . rest) `(%%tracking (^#make-s16vector ,size ,@rest)))
(jazz:define-macro (%%allocate-u16vector size . rest) `(%%tracking (^#make-u16vector ,size ,@rest)))
(jazz:define-macro (%%allocate-s32vector size . rest) `(%%tracking (^#make-s32vector ,size ,@rest)))
(jazz:define-macro (%%allocate-u32vector size . rest) `(%%tracking (^#make-u32vector ,size ,@rest)))
(jazz:define-macro (%%allocate-s64vector size . rest) `(%%tracking (^#make-s64vector ,size ,@rest)))
(jazz:define-macro (%%allocate-u64vector size . rest) `(%%tracking (^#make-u64vector ,size ,@rest)))
(jazz:define-macro (%%allocate-f32vector size . rest) `(%%tracking (^#make-f32vector ,size ,@rest)))
(jazz:define-macro (%%allocate-f64vector size . rest) `(%%tracking (^#make-f64vector ,size ,@rest)))


;; these versions initialize memory
(jazz:define-macro (%%make-vector    size . rest) `(%%tracking (make-vector    ,size ,@rest)))
(jazz:define-macro (%%make-s8vector  size . rest) `(%%tracking (make-s8vector  ,size ,@rest)))
(jazz:define-macro (%%make-u8vector  size . rest) `(%%tracking (make-u8vector  ,size ,@rest)))
(jazz:define-macro (%%make-s16vector size . rest) `(%%tracking (make-s16vector ,size ,@rest)))
(jazz:define-macro (%%make-u16vector size . rest) `(%%tracking (make-u16vector ,size ,@rest)))
(jazz:define-macro (%%make-s32vector size . rest) `(%%tracking (make-s32vector ,size ,@rest)))
(jazz:define-macro (%%make-u32vector size . rest) `(%%tracking (make-u32vector ,size ,@rest)))
(jazz:define-macro (%%make-s64vector size . rest) `(%%tracking (make-s64vector ,size ,@rest)))
(jazz:define-macro (%%make-u64vector size . rest) `(%%tracking (make-u64vector ,size ,@rest)))
(jazz:define-macro (%%make-f32vector size . rest) `(%%tracking (make-f32vector ,size ,@rest)))
(jazz:define-macro (%%make-f64vector size . rest) `(%%tracking (make-f64vector ,size ,@rest)))


(jazz:define-macro (%%s8vector . rest) (if jazz:debug-core? `(s8vector ,@rest) `(^#s8vector ,@rest)))
(jazz:define-macro (%%s8vector-length vec) (if jazz:debug-core? `(s8vector-length ,vec) `(^#s8vector-length ,vec)))
(jazz:define-macro (%%s8vector-ref vec n) (if jazz:debug-core? `(s8vector-ref ,vec ,n) `(^#s8vector-ref ,vec ,n)))
(jazz:define-macro (%%s8vector-set! vec n value) (if jazz:debug-core? `(s8vector-set! ,vec ,n ,value) `(^#s8vector-set! ,vec ,n ,value)))


(jazz:define-macro (%%u8vector . rest) (if jazz:debug-core? `(u8vector ,@rest) `(^#u8vector ,@rest)))
(jazz:define-macro (%%u8vector-length vec) (if jazz:debug-core? `(u8vector-length ,vec) `(^#u8vector-length ,vec)))
(jazz:define-macro (%%u8vector-ref vec n) (if jazz:debug-core? `(u8vector-ref ,vec ,n) `(^#u8vector-ref ,vec ,n)))
(jazz:define-macro (%%u8vector-set! vec n value) (if jazz:debug-core? `(u8vector-set! ,vec ,n ,value) `(^#u8vector-set! ,vec ,n ,value)))


(jazz:define-macro (%%s16vector . rest) (if jazz:debug-core? `(s16vector ,@rest) `(^#s16vector ,@rest)))
(jazz:define-macro (%%s16vector-length vec) (if jazz:debug-core? `(s16vector-length ,vec) `(^#s16vector-length ,vec)))
(jazz:define-macro (%%s16vector-ref vec n) (if jazz:debug-core? `(s16vector-ref ,vec ,n) `(^#s16vector-ref ,vec ,n)))
(jazz:define-macro (%%s16vector-set! vec n value) (if jazz:debug-core? `(s16vector-set! ,vec ,n ,value) `(^#s16vector-set! ,vec ,n ,value)))


(jazz:define-macro (%%u16vector . rest) (if jazz:debug-core? `(u16vector ,@rest) `(^#u16vector ,@rest)))
(jazz:define-macro (%%u16vector-length vec) (if jazz:debug-core? `(u16vector-length ,vec) `(^#u16vector-length ,vec)))
(jazz:define-macro (%%u16vector-ref vec n) (if jazz:debug-core? `(u16vector-ref ,vec ,n) `(^#u16vector-ref ,vec ,n)))
(jazz:define-macro (%%u16vector-set! vec n value) (if jazz:debug-core? `(u16vector-set! ,vec ,n ,value) `(^#u16vector-set! ,vec ,n ,value)))


(jazz:define-macro (%%s32vector . rest) (if jazz:debug-core? `(s32vector ,@rest) `(^#s32vector ,@rest)))
(jazz:define-macro (%%s32vector-length vec) (if jazz:debug-core? `(s32vector-length ,vec) `(^#s32vector-length ,vec)))
(jazz:define-macro (%%s32vector-ref vec n) (if jazz:debug-core? `(s32vector-ref ,vec ,n) `(^#s32vector-ref ,vec ,n)))
(jazz:define-macro (%%s32vector-set! vec n value) (if jazz:debug-core? `(s32vector-set! ,vec ,n ,value) `(^#s32vector-set! ,vec ,n ,value)))


(jazz:define-macro (%%u32vector . rest) (if jazz:debug-core? `(u32vector ,@rest) `(^#u32vector ,@rest)))
(jazz:define-macro (%%u32vector-length vec) (if jazz:debug-core? `(u32vector-length ,vec) `(^#u32vector-length ,vec)))
(jazz:define-macro (%%u32vector-ref vec n) (if jazz:debug-core? `(u32vector-ref ,vec ,n) `(^#u32vector-ref ,vec ,n)))
(jazz:define-macro (%%u32vector-set! vec n value) (if jazz:debug-core? `(u32vector-set! ,vec ,n ,value) `(^#u32vector-set! ,vec ,n ,value)))


(jazz:define-macro (%%s64vector . rest) (if jazz:debug-core? `(s64vector ,@rest) `(^#s64vector ,@rest)))
(jazz:define-macro (%%s64vector-length vec) (if jazz:debug-core? `(s64vector-length ,vec) `(^#s64vector-length ,vec)))
(jazz:define-macro (%%s64vector-ref vec n) (if jazz:debug-core? `(s64vector-ref ,vec ,n) `(^#s64vector-ref ,vec ,n)))
(jazz:define-macro (%%s64vector-set! vec n value) (if jazz:debug-core? `(s64vector-set! ,vec ,n ,value) `(^#s64vector-set! ,vec ,n ,value)))


(jazz:define-macro (%%u64vector . rest) (if jazz:debug-core? `(u64vector ,@rest) `(^#u64vector ,@rest)))
(jazz:define-macro (%%u64vector-length vec) (if jazz:debug-core? `(u64vector-length ,vec) `(^#u64vector-length ,vec)))
(jazz:define-macro (%%u64vector-ref vec n) (if jazz:debug-core? `(u64vector-ref ,vec ,n) `(^#u64vector-ref ,vec ,n)))
(jazz:define-macro (%%u64vector-set! vec n value) (if jazz:debug-core? `(u64vector-set! ,vec ,n ,value) `(^#u64vector-set! ,vec ,n ,value)))


(jazz:define-macro (%%f32vector . rest) (if jazz:debug-core? `(f32vector ,@rest) `(^#f32vector ,@rest)))
(jazz:define-macro (%%f32vector-length vec) (if jazz:debug-core? `(f32vector-length ,vec) `(^#f32vector-length ,vec)))
(jazz:define-macro (%%f32vector-ref vec n) (if jazz:debug-core? `(f32vector-ref ,vec ,n) `(^#f32vector-ref ,vec ,n)))
(jazz:define-macro (%%f32vector-set! vec n value) (if jazz:debug-core? `(f32vector-set! ,vec ,n ,value) `(^#f32vector-set! ,vec ,n ,value)))


(jazz:define-macro (%%f64vector . rest) (if jazz:debug-core? `(f64vector ,@rest) `(^#f64vector ,@rest)))
(jazz:define-macro (%%f64vector-length vec) (if jazz:debug-core? `(f64vector-length ,vec) `(^#f64vector-length ,vec)))
(jazz:define-macro (%%f64vector-ref vec n) (if jazz:debug-core? `(f64vector-ref ,vec ,n) `(^#f64vector-ref ,vec ,n)))
(jazz:define-macro (%%f64vector-set! vec n value) (if jazz:debug-core? `(f64vector-set! ,vec ,n ,value) `(^#f64vector-set! ,vec ,n ,value)))


;;;
;;;; Interrupts
;;;


(define TERMINATE-INTERRUPT  1)
(define HEARTBEAT-INTERRUPT  2)
(define USER-INTERRUPT       3)
(define GC-INTERRUPT         4)
(define HIGH-LEVEL-INTERRUPT 5)


;; DANGER : temporary hack until proper primitive exists
(jazz:define-macro (%%interrupt-handler code)
  `(^#interrupt-handler ,code))

;; DANGER : temporary hack until proper primitive exists
(jazz:define-macro (%%interrupt-vector-set! code handler)
  `(^#interrupt-vector-set! ,code ,handler))


(jazz:define-macro (%%disable-interrupts!)
  `(^#disable-interrupts!))

(jazz:define-macro (%%enable-interrupts!)
  `(^#enable-interrupts!))


;;;
;;;; Kernel
;;;


;; DANGER : temporary hack until proper primitive exists
(jazz:define-macro (%%subtype obj)
  `(^#subtype ,obj))


;;;
;;;; Keyword
;;;


(jazz:define-macro (%%keyword? obj)
  (if jazz:debug-core?
      `(keyword? ,obj)
    `(^#keyword? ,obj)))

(jazz:define-macro (%%string->keyword str)
  (if jazz:debug-core?
      `(string->keyword ,str)
    `(^#string->keyword ,str)))

(jazz:define-macro (%%keyword->string keyword)
  (if jazz:debug-core?
      `(keyword->string ,keyword)
    `(^#keyword->string ,keyword)))

(jazz:define-macro (%%keyword-table)
  `(^#keyword-table))


;;;
;;;; List
;;;


(jazz:define-macro (%%null? obj)
  (if jazz:debug-core?
      `(null? ,obj)
    `(^#null? ,obj)))

(jazz:define-macro (%%pair? obj)
  (if jazz:debug-core?
      `(pair? ,obj)
    `(^#pair? ,obj)))

(jazz:define-macro (%%car pair)
  (if jazz:debug-core?
      `(car ,pair)
    `(^#car ,pair)))

(jazz:define-macro (%%cdr pair)
  (if jazz:debug-core?
      `(cdr ,pair)
    `(^#cdr ,pair)))

(jazz:define-macro (%%set-car! pair val)
  (if jazz:debug-core?
      `(set-car! ,pair ,val)
    `(^#set-car! ,pair ,val)))

(jazz:define-macro (%%set-cdr! pair val)
  (if jazz:debug-core?
      `(set-cdr! ,pair ,val)
    `(^#set-cdr! ,pair ,val)))

(jazz:define-macro (%%caar pair)
  (if jazz:debug-core?
      `(caar ,pair)
    `(^#caar ,pair)))

(jazz:define-macro (%%cadr pair)
  (if jazz:debug-core?
      `(cadr ,pair)
    `(^#cadr ,pair)))

(jazz:define-macro (%%cdar pair)
  (if jazz:debug-core?
      `(cdar ,pair)
    `(^#cdar ,pair)))

(jazz:define-macro (%%cddr pair)
  (if jazz:debug-core?
      `(cddr ,pair)
    `(^#cddr ,pair)))

(jazz:define-macro (%%length lst)
  (if jazz:debug-core?
      `(length ,lst)
    `(^#length ,lst)))

(jazz:define-macro (%%memq obj lst)
  (if jazz:debug-core?
      `(memq ,obj ,lst)
    `(^#memq ,obj ,lst)))

(jazz:define-macro (%%memv obj lst)
  `(memv ,obj ,lst))

(jazz:define-macro (%%member obj lst)
  (if jazz:debug-core?
      `(member ,obj ,lst)
    `(^#member ,obj ,lst)))

(jazz:define-macro (%%assq obj alist)
  (if jazz:debug-core?
      `(assq ,obj ,alist)
    `(^#assq ,obj ,alist)))

(jazz:define-macro (%%assv obj alist)
  (if jazz:debug-core?
      `(assv ,obj ,alist)
    `(^#assv ,obj ,alist)))

(jazz:define-macro (%%assoc obj alist)
  (if jazz:debug-core?
      `(assoc ,obj ,alist)
    `(^#assoc ,obj ,alist)))

(jazz:define-macro (%%cons x y)
  (if jazz:debug-core?
      `(cons ,x ,y)
    `(^#cons ,x ,y)))

(jazz:define-macro (%%list . rest)
  (if jazz:debug-core?
      `(list ,@rest)
    `(^#list ,@rest)))

(jazz:define-macro (%%append x y)
  (if jazz:debug-core?
      `(append ,x ,y)
    `(^#append ,x ,y)))

(jazz:define-macro (%%remove elem lst)
  (%%force-uniqueness (elem lst)
    `(%%check-list ,lst 2 (%%remove ,elem ,lst)
       (^#remove ,elem ,lst))))

(jazz:define-macro (%%reverse lst)
  (if jazz:debug-core?
      `(reverse ,lst)
    `(^#reverse ,lst)))

(jazz:define-macro (%%list->vector lst)
  (if jazz:debug-core?
      `(list->vector ,lst)
    `(^#list->vector ,lst)))


;;;
;;;; Memory
;;;


(jazz:define-macro (%%gc)
  `(^#gc))

(jazz:define-macro (%%get-bytes-allocated! floats i)
  (%%force-uniqueness (floats i)
    `(%%check-f64vector ,floats 1 (%%get-bytes-allocated! ,floats ,i)
       (%%check-fixnum ,i 2 (%%get-bytes-allocated! ,floats ,i)
         (^#get-bytes-allocated! ,floats ,i)))))

(jazz:define-macro (%%get-live-percent)
  `(^#get-live-percent))


;;;
;;;; Number
;;;


(jazz:define-macro (%%number? obj)
  (if jazz:debug-core?
      `(number? ,obj)
    `(^#number? ,obj)))

(jazz:define-macro (%%integer? obj)
  (if jazz:debug-core?
      `(integer? ,obj)
    `(^#integer? ,obj)))

(jazz:define-macro (%%real? obj)
  (if jazz:debug-core?
      `(real? ,obj)
    `(^#real? ,obj)))

(jazz:define-macro (%%= x y)
  (if jazz:debug-core?
      `(= ,x ,y)
    `(^#= ,x ,y)))

(jazz:define-macro (%%+ x y)
  (if jazz:debug-core?
      `(+ ,x ,y)
    `(^#+ ,x ,y)))

(jazz:define-macro (%%- . rest)
  (if jazz:debug-core?
      `(- ,@rest)
    `(^#- ,@rest)))

(jazz:define-macro (%%* x y)
  (if jazz:debug-core?
      `(* ,x ,y)
    `(^#* ,x ,y)))

(jazz:define-macro (%%/ x y)
  (if jazz:debug-core?
      `(/ ,x ,y)
    `(^#/ ,x ,y)))

(jazz:define-macro (%%number->string n)
  (if jazz:debug-core?
      `(number->string ,n)
    `(^#number->string ,n)))

(jazz:define-macro (%%string->number str)
  (if jazz:debug-core?
      `(string->number ,str)
    `(^#string->number ,str)))


;;;
;;;; Object
;;;


(jazz:define-macro (%%object->string n)
  (if jazz:debug-core?
      `(object->string ,n)
    `(^#object->string ,n)))


;;;
;;;; Parameter
;;;


(jazz:define-macro (%%make-parameter . rest)
  `(%%tracking
     (make-parameter ,@rest)))


;;;
;;;; Path
;;;


(jazz:define-macro (%%path-expand path)
  (%%force-uniqueness (path)
    `(%%check-string ,path 1 (%%path-expand ,path)
       (^#path-expand ,path))))


;;;
;;;; Port
;;;


(jazz:define-macro (%%port? obj)
  (if jazz:debug-core?
      `(port? ,obj)
    `(^#port? ,obj)))

(jazz:define-macro (%%port-name port)
  (%%force-uniqueness (port)
    `(%%check-port ,port 1 (%%port-name ,port)
       (^#port-name ,port))))

(jazz:define-macro (%%port-name->container obj)
  `(^#port-name->container ,obj))

(jazz:define-macro (%%eof-object? obj)
  (if jazz:debug-core?
      `(eof-object? ,obj)
    `(^#eof-object? ,obj)))

(jazz:define-macro (%%input-port-names-set! port names)
  (%%force-uniqueness (port names)
    `(%%check-port ,port 1 (%%input-port-names-set! ,port ,names)
       ;; DANGER : temporary hack until proper primitive exists
       (^#vector-set! ,port 4 ,names))))

(jazz:define-macro (%%input-port-line-set! port line)
  (%%force-uniqueness (port line)
    `(%%check-port ,port 1 (%%input-port-line-set! ,port ,line)
       `(%%check-fixnum ,line 2 (%%input-port-line-set! ,port ,line)
          (^#input-port-line-set! ,port ,line)))))

(jazz:define-macro (%%input-port-column-set! port col)
  (%%force-uniqueness (port col)
    `(%%check-port ,port 1 (%%input-port-column-set! ,port ,col)
       `(%%check-fixnum ,col 2 (%%input-port-column-set! ,port ,col)
          (^#input-port-column-set! ,port ,col)))))

(jazz:define-macro (%%read-all-as-a-begin-expr-from-port port readtable wrap unwrap start-syntax close-port?)
  (%%force-uniqueness (port readtable wrap unwrap start-syntax close-port?)
    `(%%check-port ,port 1 (%%read-all-as-a-begin-expr-from-port ,port ,readtable ,wrap ,unwrap ,start-syntax ,close-port?)
       (%%check-readtable ,readtable 2 (%%read-all-as-a-begin-expr-from-port ,port ,readtable ,wrap ,unwrap ,start-syntax ,close-port?)
         (^#read-all-as-a-begin-expr-from-port ,port ,readtable ,wrap ,unwrap ,start-syntax ,close-port?)))))

(jazz:define-macro (%%write obj port)
  (%%force-uniqueness (obj port)
    `(%%check-port ,port 2 (%%write ,obj ,port)
       (^#write ,obj ,port))))

(jazz:define-macro (%%write-string str port)
  (%%force-uniqueness (str port)
    `(%%check-string ,str 1 (%%write-string ,str ,port)
       (%%check-port ,port 2 (%%write-string ,str ,port)
         (^#write-string ,str ,port)))))

(jazz:define-macro (%%newline port)
  (%%force-uniqueness (port)
    `(%%check-port ,port 2 (%%newline ,port)
       (^#newline ,port))))

(jazz:define-macro (%%default-wr we obj)
  (%%force-uniqueness (we obj)
    `(%%check-writeenv ,we 1 (%%default-wr ,we ,obj)
       (^#default-wr ,we ,obj))))

(jazz:define-macro (%%wr-set! proc)
  (%%force-uniqueness (proc)
    `(%%check-procedure ,proc 1 (%%wr-set! ,proc)
       (^#wr-set! ,proc))))


;;;
;;;; Procedure
;;;


(jazz:define-macro (%%procedure? obj)
  (if jazz:debug-core?
      `(procedure? ,obj)
    `(^#procedure? ,obj)))

(jazz:define-macro (%%procedure-name procedure)
  (%%force-uniqueness (procedure)
    `(%%check-procedure ,procedure 1 (%%procedure-name ,procedure)
       (^#procedure-name ,procedure))))

(jazz:define-macro (%%procedure-locat procedure)
  (%%force-uniqueness (procedure)
    `(%%check-procedure ,procedure 1 (%%procedure-locat ,procedure)
       (^#procedure-locat ,procedure))))

(jazz:define-macro (%%interp-procedure? obj)
  `(^#interp-procedure? ,obj))


;;;
;;;; Process
;;;


(jazz:define-macro (%%process-statistics)
  `(^#process-statistics))

(jazz:define-macro (%%get-min-heap)
  `(^#get-min-heap))

(jazz:define-macro (%%set-min-heap! bytes)
  (%%force-uniqueness (bytes)
    `(%%check-fixnum ,bytes 1 (%%set-min-heap! ,bytes)
       (^#set-min-heap! ,bytes))))

(jazz:define-macro (%%get-max-heap)
  `(^#get-max-heap))

(jazz:define-macro (%%set-max-heap! bytes)
  (%%force-uniqueness (bytes)
    `(%%check-fixnum ,bytes 1 (%%set-max-heap! ,bytes)
       (^#set-max-heap! ,bytes))))


;;;
;;;; Rational
;;;


(jazz:define-macro (%%rational? obj)
  (if jazz:debug-core?
      `(rational? ,obj)
    `(^#rational? ,obj)))

(jazz:define-macro (%%ratnum? obj)
  `(^#ratnum? ,obj))

(jazz:define-macro (%%ratnum->flonum x)
  (%%force-uniqueness (x)
    `(%%check-ratnum ,x 1 (%%ratnum->flonum ,x)
       (^#ratnum->flonum ,x))))


;;;
;;;; Readenv
;;;


(jazz:define-macro (%%readenv? obj)
  `(macro-readenv? ,obj))

(jazz:define-macro (%%readenv-current-filepos readenv)
  (%%force-uniqueness (readenv)
    `(%%check-readenv ,readenv 1 (%%readenv-current-filepos ,readenv)
       (^#readenv-current-filepos ,readenv))))

(jazz:define-macro (%%build-list readenv allow-improper? start-pos close)
  (%%force-uniqueness (readenv allow-improper? start-pos close)
    `(%%check-readenv ,readenv 1 (%%build-list ,readenv ,allow-improper? ,start-pos ,close)
       (^#build-list ,readenv ,allow-improper? ,start-pos ,close))))

(jazz:define-macro (%%read-datum-or-label-or-none-or-dot readenv)
  (%%force-uniqueness (readenv)
    `(%%check-readenv ,readenv 1 (%%read-datum-or-label-or-none-or-dot ,readenv)
       (^#read-datum-or-label-or-none-or-dot ,readenv))))


;;;
;;;; Readtable
;;;


(jazz:define-macro (%%readtable? obj)
  (if jazz:debug-core?
      `(readtable? ,obj)
    `(^#readtable? ,obj)))

(jazz:define-macro (%%current-readtable)
  `(^#current-readtable))

(jazz:define-macro (%%make-standard-readtable)
  `(^#make-standard-readtable))

(jazz:define-macro (%%readtable-copy readtable)
  (%%force-uniqueness (readtable)
    `(%%check-readtable ,readtable 1 (%%readtable-copy ,readtable)
       (^#readtable-copy ,readtable))))

(jazz:define-macro (%%readtable-char-delimiter? readtable c)
  (%%force-uniqueness (readtable c)
    `(%%check-readtable ,readtable 1 (%%readtable-char-delimiter? ,readtable ,c)
       (%%check-char ,c 2 (%%readtable-char-delimiter? ,readtable ,c)
         (^#readtable-char-delimiter? ,readtable ,c)))))

(jazz:define-macro (%%readtable-char-delimiter?-set! readtable c delimiter?)
  (%%force-uniqueness (readtable c delimiter?)
    `(%%check-readtable ,readtable 1 (%%readtable-char-delimiter?-set! ,readtable ,c ,delimiter?)
       (%%check-char ,c 2 (%%readtable-char-delimiter?-set! ,readtable ,c ,delimiter?)
         (^#readtable-char-delimiter?-set! ,readtable ,c ,delimiter?)))))

(jazz:define-macro (%%readtable-char-handler readtable c)
  (%%force-uniqueness (readtable c)
    `(%%check-readtable ,readtable 1 (%%readtable-char-handler ,readtable ,c)
       (%%check-char ,c 2 (%%readtable-char-handler ,readtable ,c)
         (^#readtable-char-handler ,readtable ,c)))))

(jazz:define-macro (%%readtable-char-handler-set! readtable c handler)
  (%%force-uniqueness (readtable c handler)
    `(%%check-readtable ,readtable 1 (%%readtable-char-handler-set! ,readtable ,c ,handler)
       (%%check-char ,c 2 (%%readtable-char-handler-set! ,readtable ,c ,handler)
         (^#readtable-char-handler-set! ,readtable ,c ,handler)))))

(jazz:define-macro (%%readtable-char-sharp-handler readtable c)
  (%%force-uniqueness (readtable c)
    `(%%check-readtable ,readtable 1 (%%readtable-char-sharp-handler ,readtable ,c)
       (%%check-char ,c 2 (%%readtable-char-sharp-handler ,readtable ,c)
         (^#readtable-char-sharp-handler ,readtable ,c)))))

(jazz:define-macro (%%readtable-char-sharp-handler-set! readtable c handler)
  (%%force-uniqueness (readtable c handler)
    `(%%check-readtable ,readtable 1 (%%readtable-char-sharp-handler-set! ,readtable ,c ,handler)
       (%%check-char ,c 2 (%%readtable-char-sharp-handler-set! ,readtable ,c ,handler)
         (^#readtable-char-sharp-handler-set! ,readtable ,c ,handler)))))

(jazz:define-macro (%%readtable-char-class-set! readtable c delimiter? handler)
  (%%force-uniqueness (readtable c delimiter? handler)
    `(%%check-readtable ,readtable 1 (%%readtable-char-class-set! ,readtable ,c ,delimiter? ,handler)
       (%%check-char ,c 2 (%%readtable-char-class-set! ,readtable ,c ,delimiter? ,handler)
         (^#readtable-char-class-set! ,readtable ,c ,delimiter? ,handler)))))

(jazz:define-macro (%%readtable-escaped-char-table readtable)
  (%%force-uniqueness (readtable)
    `(%%check-readtable ,readtable 1 (%%readtable-escaped-char-table ,readtable)
       ;; DANGER : temporary hack until proper primitive exists
       (^#vector-ref ,readtable 3))))

(jazz:define-macro (%%readtable-escaped-char-table-set! readtable table)
  (%%force-uniqueness (readtable table)
    `(%%check-readtable ,readtable 1 (%%readtable-escaped-char-table-set! ,readtable ,table)
       ;; DANGER : temporary hack until proper primitive exists
       (^#vector-set! ,readtable 3 ,table))))


;;;
;;;; Repl
;;;


(jazz:define-macro (%%repl #!optional (write-reason #f))
  `(^#repl ,write-reason))

(jazz:define-macro (%%repl-debug #!optional (write-reason #f) (toplevel? #f))
  `(^#repl-debug ,write-reason ,toplevel?))

(jazz:define-macro (%%thread-repl-context-get!)
  `(^#thread-repl-context-get!))

(jazz:define-macro (%%thread-repl-channel-get! thread)
  (%%force-uniqueness (thread)
    `(%%check-thread ,thread 1 (%%thread-repl-channel-get! ,thread)
       (^#thread-repl-channel-get! ,thread))))

(jazz:define-macro (%%repl-channel-result-history-add channel result)
  `(^#repl-channel-result-history-add ,channel ,result))


;;;
;;;; Six
;;;


(jazz:define-macro (%%six-types-ref)
  `##six-types)


(jazz:define-macro (%%six-types-set! lst)
  (%%force-uniqueness (lst)
    `(%%check-list ,lst 1 (%%six-types-set! ,lst)
       (^#six-types-set! ,lst))))


;;;
;;;; String
;;;


(jazz:define-macro (%%string? obj)
  (if jazz:debug-core?
      `(string? ,obj)
    `(^#string? ,obj)))

(jazz:define-macro (%%make-string size . rest)
  `(%%tracking
     (make-string ,size ,@rest)))

(jazz:define-macro (%%string=? str1 str2)
  (if jazz:debug-core?
      `(string=? ,str1 ,str2)
    `(^#string=? ,str1 ,str2)))

(jazz:define-macro (%%string-ci=? str1 str2)
  (if jazz:debug-core?
      `(string-ci=? ,str1 ,str2)
    `(^#string-ci=? ,str1 ,str2)))

(jazz:define-macro (%%string<? str1 str2)
  (if jazz:debug-core?
      `(string<? ,str1 ,str2)
    `(^#string<? ,str1 ,str2)))

(jazz:define-macro (%%string-length str)
  (if jazz:debug-core?
      `(string-length ,str)
    `(^#string-length ,str)))

(jazz:define-macro (%%string-ref str pos)
  (if jazz:debug-core?
      `(string-ref ,str ,pos)
    `(^#string-ref ,str ,pos)))

(jazz:define-macro (%%string-set! str pos val)
  (if jazz:debug-core?
      `(string-set! ,str ,pos ,val)
    `(^#string-set! ,str ,pos ,val)))

(jazz:define-macro (%%substring str start end)
  (if jazz:debug-core?
      `(substring ,str ,start ,end)
    `(^#substring ,str ,start ,end)))

(jazz:define-macro (%%string-append . rest)
  (if jazz:debug-core?
      `(string-append ,@rest)
    `(^#string-append ,@rest)))

(jazz:define-macro (%%string-shrink! str len)
  (%%force-uniqueness (str len)
    `(%%check-string ,str 1 (%%string-shrink! ,str ,len)
       (%%check-fixnum ,len 2 (%%string-shrink! ,str ,len)
         (^#string-shrink! ,str ,len)))))


;;;
;;;; Structure
;;;


(jazz:define-macro (%%structure? obj)
  `(^#structure? ,obj))

(jazz:define-macro (%%structure-type structure)
  (%%force-uniqueness (structure)
    `(%%check-structure ,structure 1 (%%structure-type ,structure)
       (^#structure-type ,structure))))

(jazz:define-macro (%%structure-ref structure i type proc)
  (%%force-uniqueness (structure)
    `(%%check-structure ,structure 1 (%%structure-ref ,structure ,i ,type ,proc)
       (^#structure-ref ,structure ,i ,type ,proc))))

(jazz:define-macro (%%structure-set! structure val i type proc)
  (%%force-uniqueness (structure)
    `(%%check-structure ,structure 1 (%%structure-set! ,structure ,val ,i ,type ,proc)
       (^#structure-set! ,structure ,val ,i ,type ,proc))))


;;;
;;;; Symbol
;;;


(jazz:define-macro (%%symbol? obj)
  (if jazz:debug-core?
      `(symbol? ,obj)
    `(^#symbol? ,obj)))

(jazz:define-macro (%%string->symbol str)
  (if jazz:debug-core?
      `(string->symbol ,str)
    `(^#string->symbol ,str)))

(jazz:define-macro (%%symbol->string symbol)
  (if jazz:debug-core?
      `(symbol->string ,symbol)
    `(^#symbol->string ,symbol)))

(jazz:define-macro (%%unbound? obj)
  `(^#unbound? ,obj))

(jazz:define-macro (%%global-var? symbol)
  (%%force-uniqueness (symbol)
    `(%%check-symbol ,symbol 1 (%%global-var? ,symbol)
       (^#global-var? ,symbol))))

(jazz:define-macro (%%global-var-ref symbol)
  (%%force-uniqueness (symbol)
    `(%%check-symbol ,symbol 1 (%%global-var-ref ,symbol)
       (^#global-var-ref ,symbol))))

(jazz:define-macro (%%global-var-set! symbol value)
  (%%force-uniqueness (symbol)
    `(%%check-symbol ,symbol 1 (%%global-var-set! ,symbol ,value)
       (^#global-var-set! ,symbol ,value))))

(jazz:define-macro (%%global-var-unbind! symbol)
  (%%force-uniqueness (symbol)
    `(%%check-symbol ,symbol 1 (%%global-var-unbind! ,symbol)
       (^#global-var-set! ,symbol #!unbound))))

(jazz:define-macro (%%symbol-table)
  `(^#symbol-table))


;;;
;;;; Source
;;;


(jazz:define-macro (%%source? expr)
  `(^#source? ,expr))

(jazz:define-macro (%%source-code src)
  (%%force-uniqueness (src)
    `(%%check-source ,src 1 (%%source-code ,src)
       (^#source-code ,src))))

(jazz:define-macro (%%source-locat src)
  (%%force-uniqueness (src)
    `(%%check-source ,src 1 (%%source-locat ,src)
       (^#source-locat ,src))))

(jazz:define-macro (%%desourcify expr)
  `(^#desourcify ,expr))

(jazz:define-macro (%%make-source code locat)
  (%%force-uniqueness (code locat)
    `(%%check-locat ,locat 2 (%%make-source ,code ,locat)
       (^#make-source ,code ,locat))))

(jazz:define-macro (%%sourcify expr src)
  (%%force-uniqueness (expr src)
    `(%%check-source ,src 2 (%%sourcify ,expr ,src)
       (^#sourcify ,expr ,src))))

(jazz:define-macro (%%sourcify-deep expr src)
  (%%force-uniqueness (expr src)
    `(%%check-source ,src 2 (%%sourcify-deep ,expr ,src)
       (^#sourcify-deep ,expr ,src))))

(jazz:define-macro (%%locat? expr)
  `(^#locat? ,expr))

(jazz:define-macro (%%make-locat container position)
  (%%force-uniqueness (position)
    `(%%check-fixnum ,position 2 (%%make-locat ,container ,position)
       (^#make-locat ,container ,position))))

(jazz:define-macro (%%locat-container locat)
  (%%force-uniqueness (locat)
    `(%%check-locat ,locat 1 (%%locat-container ,locat)
       (^#locat-container ,locat))))

(jazz:define-macro (%%locat-position locat)
  (%%force-uniqueness (locat)
    `(%%check-locat ,locat 1 (%%locat-position ,locat)
       (^#locat-position ,locat))))

(jazz:define-macro (%%container->path container)
  `(^#container->path ,container))

(jazz:define-macro (%%position->filepos position)
  (%%force-uniqueness (position)
    `(%%check-fixnum ,position 1 (%%position->filepos ,position)
       (^#position->filepos ,position))))

(jazz:define-macro (%%filepos->position filepos)
  (%%force-uniqueness (filepos)
    `(%%check-fixnum ,filepos 1 (%%filepos->position ,filepos)
       (^#filepos->position ,filepos))))

(jazz:define-macro (%%filepos-line filepos)
  (%%force-uniqueness (filepos)
    `(%%check-fixnum ,filepos 1 (%%filepos-line ,filepos)
       (^#filepos-line ,filepos))))

(jazz:define-macro (%%filepos-col filepos)
  (%%force-uniqueness (filepos)
    `(%%check-fixnum ,filepos 1 (%%filepos-col ,filepos)
       (^#filepos-col ,filepos))))


;;;
;;;; Table
;;;


(jazz:define-macro (%%table? obj)
  `(table? ,obj))

(jazz:define-macro (%%make-table . rest)
  `(%%tracking
     (make-table ,@rest)))

(jazz:define-macro (%%table-ref table key . rest)
  (if jazz:debug-core?
      `(table-ref ,table ,key ,@rest)
    `(^#table-ref ,table ,key ,@rest)))

(jazz:define-macro (%%table-set! table key value)
  (if jazz:debug-core?
      `(table-set! ,table ,key ,value)
    `(^#table-set! ,table ,key ,value)))

(jazz:define-macro (%%table-clear table key)
  `(table-set! ,table ,key))

(jazz:define-macro (%%table-keys table)
  `(map car (table->list ,table)))

(jazz:define-macro (%%table-length table)
  `(table-length ,table))

(jazz:define-macro (%%table-for-each proc table)
  `(table-for-each ,proc ,table))

(jazz:define-macro (%%table-merge! table additions #!optional (additions-takes-precedence? #f))
  (if jazz:debug-core?
      `(table-merge! ,table ,additions ,additions-takes-precedence?)
    `(^#table-merge! ,table ,additions ,additions-takes-precedence?)))

(jazz:define-macro (%%list->table alist . rest)
  `(list->table ,alist ,@rest))

(jazz:define-macro (%%table->list table)
  `(table->list ,table))

(jazz:define-macro (%%table-entries table)
  `(map cdr (table->list ,table)))

(jazz:define-macro (%%copy-table table)
  `(table-copy ,table))


(jazz:define-macro (%%gc-hash-table? obj)
  `(^#gc-hash-table? ,obj))


;;;
;;;; Thread
;;;


(jazz:define-macro (%%thread? obj)
  `(thread? ,obj))

(jazz:define-macro (%%make-thread . rest)
  `(%%tracking
     (make-thread ,@rest)))

(jazz:define-macro (%%primordial-thread-ref)
  `##primordial-thread)

(jazz:define-macro (%%current-thread)
  `(^#current-thread))

(jazz:define-macro (%%make-mutex . rest)
  `(%%tracking
     (make-mutex ,@rest)))

(jazz:define-macro (%%make-condition-variable . rest)
  `(%%tracking
     (make-condition-variable ,@rest)))


;;;
;;;; Time
;;;


(jazz:define-macro (%%get-current-time! floats i)
  (%%force-uniqueness (floats i)
    `(%%check-f64vector ,floats 1 (%%get-current-time! ,floats ,i)
       (%%check-fixnum ,i 2 (%%get-current-time! ,floats ,i)
         (^#get-current-time! ,floats ,i)))))


;;;
;;;; Type
;;;


(jazz:define-macro (%%type? obj)
  `(^#type? ,obj))

(jazz:define-macro (%%type-id type)
  (%%force-uniqueness (type)
    `(%%check-type ,type 1 (%%type-id ,type)
       (^#type-id type))))

(jazz:define-macro (%%type-name type)
  (%%force-uniqueness (type)
    `(%%check-type ,type 1 (%%type-name ,type)
       (^#type-name type))))

(jazz:define-macro (%%type-flags type)
  (%%force-uniqueness (type)
    `(%%check-type ,type 1 (%%type-flags ,type)
       (^#type-flags type))))

(jazz:define-macro (%%type-super type)
  (%%force-uniqueness (type)
    `(%%check-type ,type 1 (%%type-super ,type)
       (^#type-super type))))

(jazz:define-macro (%%type-field-count type)
  (%%force-uniqueness (type)
    `(%%check-type ,type 1 (%%type-field-count ,type)
       (^#type-field-count type))))

(jazz:define-macro (%%type-all-fields type)
  (%%force-uniqueness (type)
    `(%%check-type ,type 1 (%%type-all-fields ,type)
       (^#type-all-fields type))))


;;;
;;;; Values
;;;


(jazz:define-macro (%%values? obj)
  `(^#values? ,obj))


;;;
;;;; Vector
;;;


(jazz:define-macro (%%vector? obj)
  (if jazz:debug-core?
      `(vector? ,obj)
    `(^#vector? ,obj)))

(jazz:define-macro (%%vector . rest)
  (if jazz:debug-core?
      `(vector ,@rest)
    `(^#vector ,@rest)))

(jazz:define-macro (%%vector-length vector)
  (if jazz:debug-core?
      `(vector-length ,vector)
    `(^#vector-length ,vector)))

(jazz:define-macro (%%vector-ref vector n)
  (if jazz:debug-core?
      `(vector-ref ,vector ,n)
    `(^#vector-ref ,vector ,n)))

(jazz:define-macro (%%vector-set! vector n value)
  (if jazz:debug-core?
      `(vector-set! ,vector ,n ,value)
    `(^#vector-set! ,vector ,n ,value)))

(jazz:define-macro (%%vector-copy vector . rest)
  (if jazz:debug-core?
      `(vector-copy ,vector ,@rest)
    `(^#vector-copy ,vector ,@rest)))

(jazz:define-macro (%%vector->list vector)
  (if jazz:debug-core?
      `(vector->list ,vector)
    `(^#vector->list ,vector)))

(jazz:define-macro (%%s8vector? obj)
  (if jazz:debug-core?
      `(s8vector? ,obj)
    `(^#s8vector? ,obj)))

(jazz:define-macro (%%u8vector? obj)
  (if jazz:debug-core?
      `(u8vector? ,obj)
    `(^#u8vector? ,obj)))

(jazz:define-macro (%%s16vector? obj)
  (if jazz:debug-core?
      `(s16vector? ,obj)
    `(^#s16vector? ,obj)))

(jazz:define-macro (%%u16vector? obj)
  (if jazz:debug-core?
      `(u16vector? ,obj)
    `(^#u16vector? ,obj)))

(jazz:define-macro (%%s32vector? obj)
  (if jazz:debug-core?
      `(s32vector? ,obj)
    `(^#s32vector? ,obj)))

(jazz:define-macro (%%u32vector? obj)
  (if jazz:debug-core?
      `(u32vector? ,obj)
    `(^#u32vector? ,obj)))

(jazz:define-macro (%%s64vector? obj)
  (if jazz:debug-core?
      `(s64vector? ,obj)
    `(^#s64vector? ,obj)))

(jazz:define-macro (%%u64vector? obj)
  (if jazz:debug-core?
      `(u64vector? ,obj)
    `(^#u64vector? ,obj)))

(jazz:define-macro (%%f32vector? obj)
  (if jazz:debug-core?
      `(f32vector? ,obj)
    `(^#f32vector? ,obj)))

(jazz:define-macro (%%f64vector? obj)
  (if jazz:debug-core?
      `(f64vector? ,obj)
    `(^#f64vector? ,obj)))


;;;
;;;; Will
;;;


(jazz:define-macro (%%make-will . rest)
  `(%%tracking
     (make-will ,@rest))))
