(##define-macro (macro-make-code code-prc cte src stepper subcodes . lst)
  `(let (($code (##vector #f ,code-prc ,cte ,src ,stepper ,@subcodes ,@lst)))
     $code))

(##define-macro (^ n)
  `(##vector-ref $code ,(+ n 5)))

(##define-macro (macro-code-run c)
  `(let (($$code ,c))
     ((##vector-ref $$code 1) $$code rte)))

(##define-macro (macro-make-cprc . body)
  `(let ()
     (##declare (not inline) (not interrupts-enabled) (environment-map))
     (lambda ($code rte)
       (let (($$continue
              (lambda ($code rte)
                (##declare (inline))
                ,@body)))
         (##declare (interrupts-enabled))
         ($$continue $code rte)))))

(##define-macro (macro-make-gen params . def)
  `(let ()
     (##declare (not inline))
     (lambda (cte tail? src ,@params) ,@def)))

(##define-macro (macro-make-rte rte . lst)
  `(##vector ,rte ,@lst))

(define ##cprc-prc-req2
  (macro-make-cprc
   (macro-lambda-step! ()
     (letrec ((proc
               (lambda (#!optional (arg1 (macro-absent-obj))
                                   (arg2 (macro-absent-obj))
                        #!rest arg3-and-up)

                 (##define-macro (execute)
                   `(if (or (##eq? arg2 (macro-absent-obj))
                            (##not (##null? arg3-and-up)))
                      (let ((args
                             (cond ((##eq? arg1 (macro-absent-obj))
                                    '())
                                   ((##eq? arg2 (macro-absent-obj))
                                    (##list arg1))
                                   (else
                                    (##cons arg1
                                            (##cons arg2 arg3-and-up))))))
                        (##check-heap-limit)
                        (##first-argument $code rte)
                        (##raise-wrong-number-of-arguments-exception
                         proc
                         args))
                      (let* (($code (^ 0))
                             (rte (macro-make-rte rte proc arg1 arg2)))
                        (##first-argument #f) ; make sure $code and rte are in environment-map
                        (##check-heap-limit)
                        (macro-code-run $code))))

                 (let ((entry-hook (^ 1)))
                   (if entry-hook
                     (let* ((args
                              (##cons arg1
                                      (##cons arg2 arg3-and-up)))
                            (exec
                             (lambda () (execute))))
                       (##check-heap-limit)
                       (##first-argument $code rte)
                       (entry-hook proc args exec))
                     (execute))))))

       (##check-heap-limit)
       (##first-argument ; keep $code and rte in environment-map
         proc
         $code
         rte)))))

(define ##cprc-app2-red ;; proper tail calls
  (macro-make-cprc
   (let* ((oper (macro-code-run (^ 0)))
          (arg1 (macro-code-run (^ 1)))
          (arg2 (macro-code-run (^ 2))))
     (oper arg1 arg2))))

(define ##cprc-app2-sub ;; not proper tail calls
  (macro-make-cprc
   (let* ((oper (macro-code-run (^ 0)))
          (arg1 (macro-code-run (^ 1)))
          (arg2 (macro-code-run (^ 2))))
     (##subproblem-apply2 $code rte oper arg1 arg2))))

(define ##subproblem-apply2
  (let ()
    (##declare (not inline) (not interrupts-enabled) (environment-map))
    (lambda ($code rte proc arg1 arg2)
      (macro-call-step! (proc arg1 arg2)
        (##first-argument
         (proc arg1 arg2)
         $code
         rte)))))
