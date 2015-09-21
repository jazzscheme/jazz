(unit test.performance.time


(define iterations
  100000000)


;(time (ff iterations))
;(time (gg iterations))
;(time (f-module-block/standard/not-safe (new-x) iterations))
;(time (f-module-block/runtime/safe (new-x) iterations))
;(time (f-module-separate/runtime/safe (new-x) iterations))
;(time (f-module-block/runtime/notsafe (new-x) iterations))
(time (f-module (new-x) iterations))
(time (f-separate (new-x) iterations))
(time (f-generic (new-x) iterations))
(time (f-vtable (new-z) iterations))
)
