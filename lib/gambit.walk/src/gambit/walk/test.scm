(unit gambit.walk.test

(require (gambit.walk))

(pp (mem-allocated-kind '(a b c)))
(pp (mem-allocated-kind (##copy-object (list 1 2 3) PERM (##make-domain))))
  
(test-walk))
