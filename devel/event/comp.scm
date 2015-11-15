(define (comp name lang cc)
  (let ((src (string-append name "." "scm"))
        (c (string-append name "." lang))
        (o (string-append name "." "o"))
        (o1c (string-append name "." "o1" "." lang))
        (o1 (string-append name "." "o1")))
    (compile-file-to-target src output: c)
    (let ((gambit-include-dir (path-expand "~~include")))
      (let ((port (open-process
                    (list
                      path: cc
                      arguments: `("-I" ,gambit-include-dir "-D___DYNAMIC" "-Wno-write-strings" "-c" "-o" ,o ,c)))))
        (process-status port)))
    (link-flat (list c) output: o1c warnings?: #f)
    (##gambcomp 'C
                'dyn
                "."
                (list o1c o)
                o1
                #f
                (list (cons "CC_OPTIONS" "")
                      (cons "LD_OPTIONS" "-framework Cocoa")))
    (pp (list 'compiled name))))

(define (cmac)
  (comp "mac" "mm" "/usr/bin/g++"))
