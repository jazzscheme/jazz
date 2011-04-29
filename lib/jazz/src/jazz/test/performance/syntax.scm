(unit test.performance.syntax


(jazz:define-class Z jazz:Object (constructor: allocate-z)
  ())


(jazz:define-virtual (f-vtable (Z z) n))
(jazz:define-virtual (g-vtable (Z z) n))


(jazz:define-class W Z (constructor: allocate-w)
  ())


(jazz:define-virtual (h (W w))))
