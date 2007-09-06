(module test.performance.syntax


(jazz.define-class-syntax Z jazz.Object () jazz.Object-Class allocate-z
  ())


(jazz.define-virtual-syntax (f-vtable (Z z) n))
(jazz.define-virtual-syntax (g-vtable (Z z) n))


(jazz.define-class-syntax W Z () jazz.Object-Class allocate-w
  ())


(jazz.define-virtual-syntax (h (W w))))
