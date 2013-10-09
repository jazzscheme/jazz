(module gambit.ext jazz


(require gambit.walk)


(native new-register)
(native register-ref)
(native register-set!)
(native register-length)
(native iterate-register)

(native make-domain)
(native domain-copies)
(native domain-bytes-copied)
(native domain-bytes-copied-set!)

(native walk-object!)
(native walk-continue)
(native walk-prune)
(native walk-abort)

(native copy-to)

(native MOVABLE0)
(native MOVABLE1)
(native MOVABLE2)
(native STILL)
(native PERM)

(native gc-hash-table?)

(native mem-allocated?)
(native mem-allocated-kind)
(native mem-allocated-size)

(native walk-interned-symbols)
(native walk-interned-keywords)

(native symbol-name))
