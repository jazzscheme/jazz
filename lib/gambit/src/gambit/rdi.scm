;;; File: "rdi.scm"

;;;----------------------------------------------------------------------------

(module gambit.rdi

(define default-remote-debugger-address "localhost")
(define default-remote-debugger-port-num 8000)

(define (split-address str)
  (call-with-input-string
      str
    (lambda (port)
      (let* ((x (read-all port (lambda (port) (read-line port #\:))))
             (len (length x)))
        (cond ((<= len 1)
               (cons (if (= len 1)
                         (car x)
                         default-remote-debugger-address)
                     default-remote-debugger-port-num))
              ((= len 2)
               (let ((address (car x))
                     (port-num (string->number (cadr x) 10)))
                 (if (and port-num
                          (exact? port-num)
                          (integer? port-num)
                          (>= port-num 1)
                          (<= port-num 65535))
                     (cons (if (string=? address "")
                               default-remote-debugger-address
                               address)
                           port-num)
                     #f)))
              (else
               #f))))))

;;;----------------------------------------------------------------------------

(define-type rdi
  address
  port-num
  seq-num
  call-table
  connection
  writer-thread
)

(define (rdi-create-client remote-debugger-address)
  (and remote-debugger-address
       (let ((x (split-address remote-debugger-address)))
         (if (not x)
             (error "invalid remote debugger address")
             (let* ((address
                     (car x))
                    (port-num
                     (cdr x))
                    (rdi
                     (make-rdi
                      address
                      port-num
                      0
                      (make-table)
                      #f
                      #f))
                    (writer-thread
                     (rdi-create-writer-thread rdi)))
               (rdi-writer-thread-set! rdi writer-thread)
               (thread-start! writer-thread)
               rdi)))))

(define (rdi-create-server remote-debugger-port-num)
  (let* ((address
          #f)
         (port-num
          (or remote-debugger-port-num
              default-remote-debugger-port-num))
         (rdi
          (make-rdi
           address
           port-num
           0
           (make-table)
           #f
           #f))
         (writer-thread
          (rdi-create-writer-thread rdi)))
    (rdi-writer-thread-set! rdi writer-thread)
    (thread-start! writer-thread)
    rdi))

(define (rdi-force-connection rdi)
  (or (rdi-connection rdi)
      (if (rdi-address rdi)
          (rdi-open-client rdi)
          (rdi-open-server rdi))))

(define rdi-version1 '());(gambit-debuggee-version 0))
(define rdi-version2 '());(gambit-debugger-version 0))

(define (allow-sharing! port)
  (output-port-readtable-set!
   port
   (readtable-sharing-allowed?-set
    (output-port-readtable port)
    'serialize))
  (input-port-readtable-set!
   port
   (readtable-sharing-allowed?-set
    (input-port-readtable port)
    'serialize)))

(define (rdi-open-client rdi)
  (let ((connection
         (open-tcp-client
          (list server-address: (rdi-address rdi)
                port-number: (rdi-port-num rdi)))))

    (allow-sharing! connection)

    (write rdi-version1 connection)
    (force-output connection)

    (let ((response (read connection)))
      (if (not (equal? response rdi-version2))
          (error "unexpected debugger version")
          (let ((reader-thread (rdi-create-reader-thread rdi connection)))
            (rdi-connection-set! rdi connection)
            (thread-start! reader-thread)
            connection)))))

(define (rdi-open-server rdi)
  (let ((listen-port
         (open-tcp-server
          (list port-number: (rdi-port-num rdi)
                reuse-address: #t))))
    (let ((connection
           (read listen-port)))
      (close-port listen-port)
      (allow-sharing! connection)
      (let ((request (read connection)))
        (if (not (equal? request rdi-version1))
            (error "unexpected debuggee version")
            (begin

              (write rdi-version2 connection)
              (force-output connection)

              (let ((reader-thread (rdi-create-reader-thread rdi connection)))
                (rdi-connection-set! rdi connection)
                (thread-start! reader-thread)
                connection)))))))

(define (rdi-create-reader-thread rdi connection)
  (make-thread
   (lambda ()
     (let loop ()
       (let ((msg (rdi-recv rdi connection)))
         (if (not (eof-object? msg))
             (begin
               (thread-send (rdi-writer-thread rdi) msg)
               (loop)))))
     (thread-send (rdi-writer-thread rdi) '(reader-thread-terminated)))))

(define (rdi-create-writer-thread rdi)
  (make-thread
   (lambda ()
     (let loop ()
       (let ((msg (thread-receive)))
         (and (rdi-handle-message rdi msg)
              (loop)))))))

(define (rdi-new-seqnum rdi)
  (let ((seq-num (+ (rdi-seq-num rdi) 1)))
    (rdi-seq-num-set! rdi seq-num)
    seq-num))

(define (rdi-handle-message rdi msg)
  (if (pair? msg)

      (case (car msg)

        ((reader-thread-terminated)
         (pretty-print
          '(rdi reader-thread is terminating)
          ##stdout-port)
         #t)

        ((terminate)
         (pretty-print
          '(rdi writer-thread is terminating)
          ##stdout-port)
         #f)

        ((call)
         (let* ((seq-num
                 (cadr msg))
                (call
                 (caddr msg))
                (result
                 (apply (rdi-function (car call))
                        (cdr call))))
           (rdi-send rdi (list 'return seq-num result))
           #t))

        ((remote-call)
         (let* ((result-mutex (cadr msg))
                (call (caddr msg))
                (seq-num (rdi-new-seqnum rdi)))
           (rdi-send rdi (list 'call seq-num call))
           (table-set! (rdi-call-table rdi)
                       seq-num
                       result-mutex)
           #t))

        ((return)
         (let* ((seq-num (cadr msg))
                (result (caddr msg))
                (call-table (rdi-call-table rdi))
                (result-mutex (table-ref call-table seq-num #f)))
           (if (not result-mutex)
               (error "invalid call sequence number")
               (begin
                 (table-set! call-table seq-num)
                 (mutex-specific-set! result-mutex result)
                 (mutex-unlock! result-mutex)
                 #t))))

        (else
         (pretty-print
          (list 'unhandled-message msg)
          ##stdout-port)
         #t))

      #f))

(define (rdi-send rdi msg)
  (let ((connection (rdi-connection rdi)))
    (write msg connection)
    (force-output connection)))

(define (rdi-recv rdi connection)
  (read connection))

(define (rdi-remote-call rdi fn . args)
  (rdi-force-connection rdi)
  (let ((result-mutex (make-mutex 'remote-call)))
    (mutex-lock! result-mutex) ;; result not ready yet...
    (thread-send
     (rdi-writer-thread rdi)
     (list 'remote-call result-mutex (cons fn args)))
    (mutex-lock! result-mutex) ;; wait until result is ready
    (mutex-specific result-mutex)))

;;;-----------------------------------------------------------------------------

(define-type $object
  id: cc36b50e-dde5-4716-a65f-c63607346345
  extender: define-type-of-object
)

(define (object->$object obj)
  (cond ((thread? obj)
         (thread->$thread obj))
        ((pair? obj)
         (cons (object->$object (car obj))
               (object->$object (cdr obj))))
        ((vector? obj)
         (list->vector
          (map object->$object
               (vector->list obj))))
        ((or (number? obj)
             (string? obj)
             (symbol? obj)
             (boolean? obj)
             (null? obj))
         obj)
        (else
         (error "object->$object can't handle" obj))))
        
(define ($object->object $obj)
  (cond (($thread? $obj)
         ($thread->thread $obj))
        ((pair? $obj)
         (cons ($object->object (car $obj))
               ($object->object (cdr $obj))))
        ((vector? $obj)
         (list->vector
          (map $object->object
               (vector->list $obj))))
        ((or (number? $obj)
             (string? $obj)
             (symbol? $obj)
             (boolean? $obj)
             (null? $obj))
         $obj)
        (else
         (error "$object->object can't handle" $obj))))
        
;;;-----------------------------------------------------------------------------

(define-type-of-object $thread
  id: ad057bf6-e636-4ebb-84e7-d3360cf9f618
  sn
)

(define (thread->$thread t)
  (make-$thread (object->serial-number t)))

(define ($thread->thread t)
  (serial-number->object ($thread-sn t)))

;;;-----------------------------------------------------------------------------

;; Placeholders for future serialized types.

(define-type-of-object $thread2
  id: 4fa1bdce-6659-4f3f-b97d-e214c8dd7669
)

(define-type-of-object $thread3
  id: 3be155d5-40a5-45c6-b0d9-3bec23511149
)

(define-type-of-object $thread4
  id: 9f3df8b2-3a08-40e1-bcf3-fcaa498ed5ef
)

(define-type-of-object $thread5
  id: c78bf9e7-7b1a-42e6-80e7-f910a20c2bec
)

(define-type-of-object $thread6
  id: 1509fbab-8c72-428e-a399-49f603efe376
)

(define-type-of-object $thread7
  id: 5ad85a01-4674-4d50-9c52-14fdc708b3d9
)

(define-type-of-object $thread8
  id: 71b7d71d-b5dc-454f-9c2a-76109e9c0413
)

(define rdi-function #f)

(define (rdi-init fn)
  (set! rdi-function fn))

)

;;;-----------------------------------------------------------------------------
