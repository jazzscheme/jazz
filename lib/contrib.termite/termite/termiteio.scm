;; override Gambit's input/output procedures

;; *** maybe this should instead use Marc's approach to port
;; serialization... or maybe not (isn't as preserving of the ports
;; semantics, like buffering and behavior on closing ports...)

;; ugly hack: NAME will be backed up to a "gambit-" prefixed name, and
;; all occurences of NAME in VALUE will be replaced by the newly
;; prefixed name
(define-macro (define-backup name value)

  (define (join . args)
    (string->symbol
     (apply string-append (map symbol->string args))))
  
  (define (subst orig new tree)
    (cond
     ((eq? tree orig) new)
     ((pair? tree)
      (cons (subst orig new (car tree))
            (subst orig new (cdr tree))))
     (else tree)))
  
  (let ((replace (join 'gambit- name)))
    `(begin
       (define ,replace ,name)
       (define ,name ,(subst name replace value)))))

;; R5RS
(define-backup call-with-input-file
  (lambda (filename proc)
    (call-with-input-file filename
      (lambda (port)
        (proc (spawn-input-port port #f))))))

(define-backup call-with-output-file 
  (lambda (filename proc)
    (call-with-output-file filename
      (lambda (port)
        (proc (spawn-output-port port #f))))))

(define-backup input-port?
  (lambda (obj)
    (termite-input-port? obj)))

(define-backup output-port?
  (lambda (obj)
    (termite-output-port? obj)))

(define-backup with-input-from-file
  (lambda (filename thunk)
    (call-with-input-file filename
      (lambda (port)
        (parameterize ((current-termite-input-port port))
          (thunk))))))

(define-backup with-output-to-file
  (lambda (filename thunk)
    (call-with-output-file filename
      (lambda (port)
        (parameterize ((current-termite-output-port port))
          (thunk))))))

(define-backup open-input-file
  (lambda (filename)
    (spawn-input-port 
     (open-input-file filename) 
     #f)))

(define-backup open-output-file
  (lambda (filename)
    (spawn-output-port
     (open-output-file filename)
     #f)))

(define-backup close-input-port
  (lambda (port)
    (! (termite-input-port-pid port) (lambda (port) 
                (close-input-port port)
                (halt!)))))

(define-backup close-output-port
  (lambda (port)
    (! (termite-output-port-pid port) (lambda (port) 
              (close-output-port port)
              (halt!)))))

(define-backup read
  (lambda (#!optional (port (current-termite-input-port)))
    (!? (termite-input-port-pid port) read)))

(define-backup read-char
  (lambda (#!optional (port (current-termite-input-port)))
    (!? (termite-input-port-pid port) read-char)))

(define-backup peek-char
  (lambda (#!optional (port (current-termite-input-port)))
    (!? (termite-input-port-pid port) peek-char)))

;; eof-object? obj

(define-backup char-ready?
  (lambda (#!optional (port (current-termite-input-port)))
    (!? (termite-input-port-pid port) char-ready?)))

(define-backup write
  (lambda (obj #!optional (port (current-termite-output-port)))
    (! (termite-output-port-pid port)
       (lambda (port) (write obj port)))))

(define-backup display
  (lambda (obj #!optional (port (current-termite-output-port)))
    (! (termite-output-port-pid port)
       (lambda (port) (display obj port)))))

(define-backup newline
  (lambda (#!optional (port (current-termite-output-port)))
    (! (termite-output-port-pid port) newline)))

(define-backup write-char
  (lambda (char #!optional (port (current-termite-output-port)))
    (! (termite-output-port-pid port)
       (lambda (port) (write-char char port)))))

;; not r5rs

(define-backup read-line
  (lambda (#!optional (port (current-termite-input-port))
                      (delimiter #\newline))
    (!? (termite-input-port-pid port) (lambda (port)
                                        (read-line port delimiter)))))

(define-backup force-output
  (lambda (#!optional (port (current-termite-output-port)))
    (! (termite-output-port-pid port) force-output)))

(define-backup pretty-print
  (lambda (obj #!optional (port (current-termite-output-port)))
    (! (termite-output-port-pid port)
       (lambda (port) (pretty-print obj port)))))

(define pp pretty-print)

(define-backup print
  (lambda (obj #!optional (port (current-termite-output-port)))
    (! (termite-output-port-pid port)
       (lambda (port) (print port: port obj)))))
