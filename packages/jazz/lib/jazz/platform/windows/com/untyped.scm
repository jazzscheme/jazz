(include "coall-macro.scm")

(c-pass
 (begin
   (c-define-type UNTYPED (pointer "void" #f))
   (c-define-type UNTYPED* (pointer UNTYPED #f))
   (c-define-type UNTYPED+ (pointer UNTYPED #f))
   (c-define-type VOID (pointer "void"))
   (c-define-type VOID* (pointer VOID))
   (c-define-type VOID+ (pointer VOID))))


(c-pass-define UNTYPED*-alloc
 (c-lambda (unsigned-int) UNTYPED* "___result = calloc(1,___arg1);"))
(c-pass-define UNTYPED*-free
 (c-lambda (UNTYPED*) void "free(___arg1);"))
(c-pass-define UNTYPED+-alloc
 (c-lambda (unsigned-int unsigned-int) UNTYPED+ "___result = calloc(___arg1, ___arg2);"))
(c-pass-define UNTYPED+-free
 (c-lambda (UNTYPED+) void "free(___arg1);"))


(c-pass-define VOID*-sizeof
 ((c-lambda () unsigned-int "___result = sizeof(void*);")))
(s-pass-define VOID*-alloc
   (lambda () (UNTYPED*-alloc VOID*-sizeof)))
(s-pass-define VOID*-free
   UNTYPED*-free)
