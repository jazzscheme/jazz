(module test.performance.syntax


(jazz.define-class Z jazz.Object () jazz.Object-Class allocate-z
  ())


(jazz.define-virtual (f-vtable (Z z) n))
(jazz.define-virtual (g-vtable (Z z) n))


(jazz.define-class W Z () jazz.Object-Class allocate-w
  ())


(jazz.define-virtual (h (W w))))
