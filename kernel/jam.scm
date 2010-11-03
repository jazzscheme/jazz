#!gsc -:dq-

(define jazz.source
  (path-directory (path-strip-trailing-directory-separator (path-directory (path-normalize (car (command-line)))))))

(load (string-append jazz.source "kernel/build"))
