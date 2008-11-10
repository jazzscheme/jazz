
(use irregex extras)

;; read and parse input
(let* ((content (read-string))
       (file-length (string-length content))
       (content (irregex-replace/all (irregex "^>.*$|\n" 'multi-line) content))
       (content-length (string-length content)))

  ;; count patterns
  (for-each
   (lambda (pattern)
     (let ((irx (irregex pattern 'ci)))
       (let lp ((start 0) (i 0))
         (let ((m (irregex-search irx content start)))
           (cond
            (m (lp (irregex-match-end m 0) (+ i 1)))
            (else (display pattern) (display " ") (write i) (newline))))))) 
   '("agggtaaa|tttaccct"
     "[cgt]gggtaaa|tttaccc[acg]"
     "a[act]ggtaaa|tttacc[agt]t"
     "ag[act]gtaaa|tttac[agt]ct"
     "agg[act]taaa|ttta[agt]cct"
     "aggg[acg]aaa|ttt[cgt]ccct"
     "agggt[cgt]aa|tt[acg]accct"
     "agggta[cgt]a|t[acg]taccct"
     "agggtaa[cgt]|[acg]ttaccct"
     ))

  ;; replace IUB codes
  (let* ((codes
          '((#\B . "(c|g|t)")   (#\D . "(a|g|t)")
            (#\H . "(a|c|t)")   (#\K . "(g|t)")
            (#\M . "(a|c)")     (#\N . "(a|c|g|t)")
            (#\R . "(a|g)")     (#\S . "(c|g)")
            (#\V . "(a|c|g)")   (#\W . "(a|t)")
            (#\Y . "(c|t)")
            ))
         (irx (irregex `(,(list->string (map car codes)))))
         (content
          (irregex-replace/all
           irx
           content
           (lambda (m)
             (cdr (assv (string-ref content (irregex-match-start m 0))
                        codes))))))

    ;; final output
    (newline)
    (write file-length) (newline)
    (write content-length) (newline)
    (write (string-length content)) (newline)
    ))

