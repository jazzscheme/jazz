(module test.ptm

(c-define-type POINT (type "POINT"))
(c-define-type POINT* (pointer POINT #f "___POINT_free___"))
(c-define-type INT int)

(define POINT-make
  (c-lambda
    ()
    POINT*
    "___result_voidstar = calloc(1,sizeof(POINT));"))

(c-declare #<<c-declare-end
___SCMOBJ ___POINT_free___(void* ptr)
{
    free(ptr);
    return ___FIX(___NO_ERR);
}
c-declare-end
)

(define POINT-sizeof
  (c-lambda () unsigned-int "___result = sizeof(POINT);"))

(define POINT-x-ref
  (c-lambda (POINT*) INT "___result = ___arg1->x;"))
(define POINT-x-set!
  (c-lambda (POINT* INT) void "___arg1->x = ___arg2;"))
(define POINT-y-ref
  (c-lambda (POINT*) INT "___result = ___arg1->y;"))
(define POINT-y-set!
  (c-lambda (POINT* INT) void "___arg1->y = ___arg2;")))
