(module gambit.ext jazz


(require gambit.walk)


(native make-domain)
(native domain-copies)
(native domain-bytes-copied)
(native copy-to)
(native update-reachable!)

(native MOVABLE0)
(native MOVABLE1)
(native MOVABLE2)
(native STILL)
(native PERM)

(native mem-allocated-kind))
