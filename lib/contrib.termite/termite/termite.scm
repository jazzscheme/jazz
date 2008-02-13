;; Copyright (C) 2005-2008 by Guillaume Germain, All Rights Reserved.
;; File: "termite.scm"

;; this is the main file for the Termite system
(##namespace ("termite#"))

(##include "~~/lib/gambit#.scm")
(##include "termite#.scm")

;; ----------------------------------------------------------------------------
;; System configuration & global data

(define *termite-cookie* (getenv "TERMITE_COOKIE" #f))

(define current-node (lambda () (error "uninitialized node")))

(define *global-mutex* (make-mutex "global termite mutex"))

(define *foreign->local* (make-table weak-values: #t))
(define *local->foreign* (make-table weak-keys: #t))

;; Get the current time in seconds.
(define (now)
  (time->seconds 
   (current-time)))

;; (c-declare "#include <time.h>")
;; 
;; Used by the logger to get a formatted version of the current time.
;; (define formatted-current-time
;;   (c-lambda () char-string
;;     "time_t t;
;;      struct tm* tm;
;;      char str[64];
;;      time(&t);
;;      tm = localtime(&t);
;;      strftime(str, 64, \"20%y-%m-%d %H:%M:%S\", tm);
;;      ___result = str;"))

;; TODO Fix this procedure
(define (formatted-current-time) (number->string (now)))


;; ----------------------------------------------------------------------------
;; Datatypes

(define (process? obj) (thread? obj))
(define (process-links pid) (thread-specific pid))
(define (process-links-set! pid obj) (thread-specific-set! pid obj))

;; universal pid
(define-type upid
  id: 9e096e09-8c66-4058-bddb-e061f2209838
  tag
  node)

;; nodes
(define-type node
  id: 8992144e-4f3e-4ce4-9d01-077576f98bc5
  read-only:
  ip
  port)

;; * Test whether 'obj' is a pid.
(define (pid? obj)
  (or (process? obj) (upid? obj)))


;; It might be better to integrate with Gambit's exception mechanism
(define-type termite-exception
  id: 6a3a285f-02c4-49ac-b00a-aa57b1ad02cf
  origin
  reason
  object)


;; ----------------------------------------------------------------------------
;; process manipulation primitives

;; * Get the pid of the current process.
(define self current-thread)

;; * Get the pid of a service on a remote node 'node' which has been
;; registered with |register| to the name 'service-name'.
(define (remote-service node service-name)
  (make-upid service-name node))

;; Base exception handler for Termite processes.
(define (base-exception-handler e)
  (##continuation-capture
   (lambda (k)
     (let ((log-crash 
            (lambda (e)
              (termite-log
               'error
               (call-with-output-string ""
                 (lambda (port)
                   (display "#|\n" port)
                   (##display-exception-in-context
                    e
                    k
                    port)
                   (##cmd-b 0 k port)
                   (display "|#\n" port)))))))
       (cond
        ;; Propagated Termite exception?
        ((termite-exception? e)
         (if (not (eq? (termite-exception-reason e) 'normal))
			 (log-crash (termite-exception-object e)))
         (for-each
          (lambda (pid) (! pid e))
          (process-links (self)))
         (halt!))
        ;; Gambit exception in the current process
        (else
         (log-crash e)
         (for-each
          (lambda (pid)
            (! pid (make-termite-exception (self) 'failure e)))
          (process-links (self)))
         (halt!)))))))


;; * Start a new process executing the code in 'thunk'.
(define (spawn thunk #!key (links '()))
  (let ((t (make-thread
            (lambda ()
              (with-exception-handler
               base-exception-handler
               thunk)
              (shutdown!)))))
    (thread-specific-set! t links)
    (thread-start! t)
    t))


(define (spawn-linked-to to thunk)
  (spawn thunk links: (list to)))


;; * Start a new process with a bidirectional link to the current
;; process.
(define (spawn-link thunk)
  (let ((pid (spawn thunk links: (list (self)))))
    (outbound-link pid)
    pid))


;; * Start a new process on remote node 'node', executing the code in
;; 'thunk'.
(define (remote-spawn node thunk #!key (links '()))
  (if (equal? node (current-node))
      (spawn thunk links: links)
      (!? (remote-service node 'spawner)
          (list 'spawn thunk links))))


;; * Start a new process on remote node 'node', with a bidirectional
;; link to the current process.
(define (remote-spawn-link node thunk)
  (let ((pid (remote-spawn node thunk links: (list (self)))))
    (outbound-link pid)
    pid))


;; * Cleanly stop the execution of the current process.  Linked
;; processes will receive a "normal" exit message.
(define (shutdown!)
  (for-each
   (lambda (pid)
     (! pid (make-termite-exception (self) 'normal #f)))
   (process-links (self)))
  (halt!))

;; this is *not* nice: it wont propagate the exit message to the other
;; processes
(define (halt!)
  (thread-terminate! (current-thread)))


;; * Forcefully terminate a local process.  Warning: it only works on
;; local processes!  This should be used with extreme caution (ie it
;; should /not/ be used).
(define (%terminate! victim)
  (thread-terminate! victim)
  (for-each
   (lambda (link)
     (! link (make-termite-exception victim 'terminated #f)))
   (process-links victim)))


;; NOTE: WAIT-FOR and ALIVE? should be grouped in a more general
;; procedure able to determine the status of a process (alive, dead,
;; waiting, etc.) and the procedure should work on remote processes

;; * Wait for the end of a process 'pid'.  Does not return anything.
;; Warning: will not work on remote processes.
(define (%wait-for pid)
  (with-exception-catcher
   (lambda (e)
     (void))
   (lambda ()
     (thread-join! pid)
     (void))))


;; Check whether the process 'pid' is still alive.  Warning: will not
;; work on remote processes.
(define (%alive? pid)
  (with-exception-catcher
   (lambda (e)
     (join-timeout-exception? e))
   (lambda ()
     (thread-join! pid 0)
     #f)))


;; ----------------------------------------------------------------------------
;; Sending messages

;; * Send a message 'msg' to 'pid'.  This means that the message will
;; be enqueued in the mailbox of the destination process.
;; 
;; Delivery of the message is unreliable in theory, but in practice
;; local messages will always be delivered, and remote messages will
;; not be delivered only if the connection is currently broken to the
;; remote node, or if the remote node is down.
;;
;; Note that you will not get an error or an exception if the message
;; doesn't get there: you need to handle errors yourself.
(define (! to msg)
  (cond
   ((process? to)
    (thread-send to msg))
   ((upid? to)
    (thread-send dispatcher (list 'relay to msg)))
   (else
    (error "invalid-message-destination" to))))


;; ----------------------------------------------------------------------------
;; Receiving messages

;; incorrect, because it doesn't handle exception messages
;; (define ? thread-receive)

;; * Retrieve the first message from the mailbox of the current
;; process.  If no message is available, the process will block until
;; a message is received.  If 'timeout' is specified, the process will
;; only block for that amount of time, and then raise an exception.
;; It is possible to also pass the 'default' argument to return a
;; value instead of raising an exception.
(define (? . opt) ;; TODO: inefficient, fix
  (match opt
    (()
     (recv
       (msg msg)))
    
    ((timeout)
     (recv
       (msg msg)
       (after timeout (thread-receive 0))))
    
    ((timeout default)
     (recv
       (msg msg)
       (after timeout default)))))


;; benchmark to see if faster...
;; (define (? #!optional (timeout +inf.0) (default (lambda (thread-receive 0))))
;;   (with-exception-catcher
;;    (lambda (exception)
;;      (if (mailbox-receive-timeout-exception? exception)
;;          (default)
;;          (raise exception)))
;;    (lambda ()
;;      (thread-receive timeout))))



;; * Retrieve the first message from the mailbox of the current
;; process that satisfised the predicate 'pred?'.  If no message
;; qualifies, the process will block until a message satisfying the
;; predicate is received.  If 'timeout' is specified, the process will
;; only block for that amount of time, and then raise an exception.
;; It is possible to also pass the 'default' argument to return a
;; value instead of raising an exception.
;; TODO: inefficient, fix
(define (?? pred? . opt)
  (match opt
    (()
     (recv
       (msg (where (pred? msg)) msg)))

    ((timeout)
     (recv
       (msg (where (pred? msg)) msg)
       (after timeout (thread-receive 0))))

    ((timeout default)
     (recv
       (msg (where (pred? msg)) msg)
       (after timeout default)))))


;; ----------------------------------------------------------------------------
;; Higher-order concurrency primitives

;; * Send a "synchronous" message to a process.  The message will be
;; annotated with a tag and the pid of the current process, therefore
;; sending a message of the form '(from tag msg)'.  The server
;; receiving the message must specifically handle that format of
;; message, and reply with a message of the form '(tag reply)'.
;;
;; Like for the |?| and |??| message retrieving operators, it is
;; possible to specify a 'timeout' to limit the amount of time to wait
;; for a reply, and a 'default' value to return if no reply has been
;; received.
;; RPC
(define (!? pid msg . opt)
  (let ((tag (make-tag)))
    (! pid (list (self) tag msg))

    (match opt
      (()
       (recv
         ((,tag reply) reply)))

      ((timeout)
       (recv
         ((,tag reply) reply)
         (after timeout (raise 'timeout))))

      ((timeout default)
       (recv
         ((,tag reply) reply)
         (after timeout default))))))


;; * Evaluate a 'thunk' on a remote node and return the result of that
;; evaluation.  Just like for |!?|, |?| and |??|, it is possible to
;; specify a 'timeout' and a 'default' argument.
(define (on node thunk)
  (let ((tag (make-tag))
        (from (self)))
    (remote-spawn node
      (lambda ()
        (! from (list tag (thunk)))))
    (recv
      ((,tag reply) reply))))


;; ----------------------------------------------------------------------------
;; Links and exception handling

;; Default callback for received exceptions.
(define (handle-exception-message event)
  (raise event))

;; * Link another process 'pid' /to/ the current one: any exception
;; not being caught by the remote process and making it crash will be
;; propagated to the current process.
(define (inbound-link pid)
  (! linker (list 'link pid (self))))


;; * Link the current process /to/ another process 'pid': any
;; exception not being caught by the current process will be
;; propagated to the remote process.
(define (outbound-link pid)
  (let* ((links (process-links (self))))
    (if (not (memq pid links))
        (process-links-set! (self) (cons pid links)))))


;; * Link bidirectionally the current process with another process
;; 'pid': any exception not being caught in any of the two processes
;; will be propagated to the other one.
(define (full-link pid)
  (inbound-link  pid)
  (outbound-link pid))


;; ----------------------------------------------------------------------------
;; Termite I/O

;; Wraps 'pid's representing Gambit output ports.
(define-type termite-output-port
  id: b0c30401-474c-4e83-94b4-d516e00fe363
  unprintable:
  pid)

;; Wraps 'pid's representing Gambit input ports.
(define-type termite-input-port
  id: ebb22fcb-ca61-4765-9896-49e6716471c3
  unprintable:
  pid)

;; Start a process representing a Gambit output port.
(define (spawn-output-port port #!optional (serialize? #f))
  (output-port-readtable-set!
   port
   (readtable-sharing-allowed?-set
    (output-port-readtable port)
    serialize?))

  (make-termite-output-port
   (spawn
     (lambda ()
       (let loop ()
         (recv
           (proc
            (where (procedure? proc))
            (proc port))
           (x (warning "unknown message sent to output port: " x)))
         (loop))))))

;; Start a process representing a Gambit input port.
(define (spawn-input-port port #!optional (serialize? #f))
  (input-port-readtable-set!
   port
   (readtable-sharing-allowed?-set
    (input-port-readtable port)
    serialize?))

  (make-termite-input-port
   (spawn
     (lambda ()
       (let loop ()
         (recv
           ((from token proc)
            (where (procedure? proc))
            (! from (list token (proc port))))
           (x (warning "unknown message sent to input port: " x)))
         (loop))))))

;; IO parameterization
;; (define current-termite-input-port (make-parameter #f))
;; (define current-termite-output-port (make-parameter #f))

;; insert IO overrides
;; (include "termiteio.scm")


;; ----------------------------------------------------------------------------
;; Distribution

;; Convert a 'pid'
(define (pid->upid obj)
  (mutex-lock! *global-mutex*)
  (cond
   ((table-ref *local->foreign* obj #f)
    => (lambda (x)
         (mutex-unlock! *global-mutex*)
         x))
   (else
    (let ((upid (make-upid (make-tag) (current-node))))
      (table-set! *local->foreign* obj upid)
      (table-set! *foreign->local* upid obj)
      (mutex-unlock! *global-mutex*)
      upid))))

(define (serialize-hook obj)
  (cond
   ((process? obj)
    (pid->upid obj))

   ;; (presumably) unserializable object so instead of crashing, we
   ;; set them to #f

   ;; note: could be handled somewhat more elegantly, see the
   ;; 'distr-comp' example in Gambit's sources (would it keep the same
   ;; semantics though?)
   ((or (thread? obj) (port? obj)) 
    #f)

   (else obj)))

(define (upid->pid obj)
  (cond
   ((table-ref *foreign->local* obj #f)
    => (lambda (pid) pid))
   ((and (symbol? (upid-tag obj))
         (resolve (upid-tag obj)))
    => (lambda (pid) pid))
   (else
    (error "don't know how to upid->pid"))))

(define (deserialize-hook obj)
  (cond
   ((and (upid? obj)
         (equal? (upid-node obj)
                 (current-node)))
    (upid->pid obj))
   (else obj)))


(define (serialize obj port)
  (let* ((serialized-obj
          (object->u8vector obj serialize-hook))
         (len
          (u8vector-length serialized-obj))
         (serialized-len
          (u8vector (bitwise-and len #xff)
                    (bitwise-and (arithmetic-shift len -8) #xff)
                    (bitwise-and (arithmetic-shift len -16) #xff)
                    (bitwise-and (arithmetic-shift len -24) #xff))))

    (begin
      (write-subu8vector serialized-len 0 4 port)
      (write-subu8vector serialized-obj 0 len port))))


(define (deserialize port)
  (let* ((serialized-len
          (u8vector 0 0 0 0))
         (n
          (read-subu8vector serialized-len 0 4 port)))

    (cond ((= 0 n)
           #!eof)
          ((not (= 4 n))
           (error "deserialization error"))
          (else
           (let* ((len
                   (+ (u8vector-ref serialized-len 0)
                      (arithmetic-shift (u8vector-ref serialized-len 1) 8)
                      (arithmetic-shift (u8vector-ref serialized-len 2) 16)
                      (arithmetic-shift (u8vector-ref serialized-len 3) 24)))
                  (serialized-obj
                   (make-u8vector len))
                  (n
                   (read-subu8vector serialized-obj 0 len port)))

             (if (not (eqv? len n))
		 (begin
		   (error "deserialization error"
                     (list len: len n: n)))
                 (let ((obj (u8vector->object serialized-obj deserialize-hook)))
                   (if (vector? obj)
                       (vector->list obj)
                       obj))))))))

(define (start-serializing-output-port port)
  (spawn-link
    (lambda ()
      (let loop ()
        (recv
          (('write data)
           ;; debug
           ;; (debug out: data)
           (serialize data port)
           (force-output port)) ;; io override

          (msg
           (warning "serializing-output-port ignored message: " msg)))
        (loop)))))


(define (start-serializing-active-input-port port receiver)
  (spawn-link
    (lambda ()
      (let loop ()
        (let ((data (deserialize port)))
          ;; to receive exceptions...
          (? 0 'ok)
          ;; debug
          ;; (debug in: data)
          (if (eof-object? data) (shutdown!))
          (! receiver (list (self) data))
          (loop))))))


;; a tcp server listens on a certain port for new tcp connection
;; requests, and call ON-CONNECT to deal with those new connections.
(define (start-tcp-server tcp-port-number on-connect)
  (spawn
    (lambda ()
      (let ((tcp-server-port (open-tcp-server (list
                                               port-number: tcp-port-number
                                               coalesce: #f))))
        (let loop () 
          (on-connect (read tcp-server-port)) ;; io override
          (loop))))))


;; MESSENGERs act as proxies for sockets to other nodes

;; initiate a new bidirectional connection to another node important:
;; caller is responsible for registering it with the dispatcher
(define (initiate-messenger node)
  (spawn
    (lambda ()
      (with-exception-catcher
        (lambda (e)
          (! dispatcher (list 'unregister (self)))
          (shutdown!))

        (lambda ()
          (let ((socket (open-tcp-client
                         (list server-address: (node-ip   node)
                               port-number:    (node-port node)
                               coalesce:       #f))))
            ;; the real interesting part
            (let ((in  (start-serializing-active-input-port socket (self)))
                  (out (start-serializing-output-port socket)))

              (! out (list 'write (current-node)))

              (messenger-loop node in out))))))))


;; start a MESSENGER for an 'inbound' connection (another node
;; initiated the bidirectional connection, see initiate-messenger)
(define (start-messenger socket)
  (spawn
    (lambda ()
      (with-exception-catcher
       (lambda (e)
         (! dispatcher (list 'unregister (self)))
         (shutdown!))

       (lambda ()
         (let ((in  (start-serializing-active-input-port socket (self)))
               (out (start-serializing-output-port socket)))
           (recv
             ((,in node)
              ;; registering messenger to local dispatcher
              (! dispatcher (list 'register (self) node))
              (messenger-loop node in out)))))))))


(define (messenger-loop node in out)
  (recv
    ;; incoming message
    ((,in ('relay id message))
     (let ((to (upid->pid (make-upid id (current-node)))))
       (! to message)))

    ;; outgoing message
    (('relay to message)
     ;; 'to' is a upid
     (let* ((id (upid-tag to))
            ;; (node (upid-node to))
            ;; (ip   (node-ip node))
            ;; (port (node-port node))
            )
       (! out (list 'write (list 'relay id message)))))

    ;; unknown message
    (msg
     (warning "messenger-loop ignored message: " msg)))

  (messenger-loop node in out))


;; the DISPATCHER dispatches messages to the right MESSENGER, it keeps
;; track of known remote nodes
(define (start-dispatcher)
  (spawn (lambda () (dispatcher-loop '()))))


;; the KNOWN-NODES of the DISPATCHER-LOOP is an a-list of NODE => MESSENGER
(define (dispatcher-loop known-nodes)
  (recv
    (('register messenger node)
     (dispatcher-loop
      (cons (cons node messenger) known-nodes)))

    (('unregister messenger)
     (dispatcher-loop
      (remove (lambda (m) (equal? (cdr m) messenger)) known-nodes)))

    (('relay upid message)
     (let ((node (upid-node upid)))
       (cond
        ;; the message should be sent locally (should not happen here)
        ((equal? node (current-node))
         ;; we ignore this message...
         (warning
          "A message was dropped because it was dispatched to the local node:"
          message: message)
         (dispatcher-loop known-nodes))

        ;; the message is destined to a pid on a known node
        ((assoc node known-nodes)
         => (lambda (messenger)
              (! (cdr messenger) (list 'relay upid message))
              (dispatcher-loop known-nodes)))

        ;; unconnected node, must connect
        (else
         (let ((messenger (initiate-messenger node)))
           (! messenger (list 'relay upid message))
           (dispatcher-loop
            (cons (cons node messenger) known-nodes)))))))

    (msg
     (warning "dispatcher ignored message: " msg) ;; uh...
     (dispatcher-loop known-nodes))))


;; ----------------------------------------------------------------------------
;; Services

;; LINKER (to establish exception-propagation links between processes)
(define (start-linker)
  (spawn linker-loop))

(define (linker-loop)
  (recv
    (('link from to)
     (cond
      ((process? from)
       (process-links-set! from (cons to (process-links from)))) ;;;;;;;;;;
      ((upid? from)
       (! (remote-service (upid-node from) 'linker)
          (list 'link from to)))
      (else
       (error "in linker-loop: unknown object"))))
    (msg
     (warning "linker ignored message: " msg)))
  (linker-loop))

;; another way to write it...
;; 
;; (define linker-plugin
;;   (make-simple-server-plugin
;;    (lambda (term state)
;;      (match term
;;        (('link from to)
;;         (let ((ref (pid-ref from)))
;;           (cond
;;            ((upid? ref)
;;             (! (remote-service (upid-node ref) 'linker)
;;                (list 'link from to)))
;;            (error "linker-plugin: unknown object"))))))))


;; Remote spawning
;; the SPAWNER answers remote-spawn request
(define (start-spawner)
  (spawn spawner-loop))

(define (spawner-loop)
  (recv
    ((from tag ('spawn thunk links))
     (! from (list tag (spawn thunk links: links))))

    (msg
     (warning "spawner ignored message: " msg)))
  (spawner-loop))


;; the NAMESERVER is used to implement a mutable global env. for
;; process names
(define (start-nameserver)
  (spawn nameserver-loop))

(define (nameserver-loop)

  (define the-register (make-dict))

  (let loop ()
    (recv
      (('register name pid)
       (dict-set! the-register name pid))

      (('unregister name pid)
       (dict-set! the-register name))

      ((from tag ('resolve name))
       (! from (list tag (dict-ref the-register name))))

      (msg
       (warning "nameserver ignored message: " msg)))

    (loop)))

(define (register name pid)
  (! nameserver (list 'register name pid)))

(define (unregister name pid)
  (! nameserver (list 'unregister name pid)))

(define (resolve name)
  (!? nameserver (list 'resolve name)))

;; ----------------------------------------------------------------------------
;; Erlang/OTP-like behavior for "generic servers" and "event handlers"

(include "otp/gen_server.scm")
(include "otp/gen_event.scm")


;; ----------------------------------------------------------------------------
;; Some datastrutures

(include "data.scm")


;; ----------------------------------------------------------------------------
;; Migration

;; Task moves away, lose identity
(define (migrate-task node)
  (call/cc
   (lambda (k)
     (remote-spawn node (lambda () (k #t)))
     (halt!))))

;; Task moves away, leave a proxy behind
(define (migrate/proxy node)
  (define (proxy pid)
    (let loop ()
      (! pid (?))
      (loop)))
  (call/cc
   (lambda (k)
     (proxy
      (remote-spawn-link node (lambda () (k #t)))))))


;; ----------------------------------------------------------------------------
;; A logging facility for Termite

;; (Ideally, this should be included with the services, but the
;; writing style is much different.  Eventually, the other services
;; might use similar style.)

(define (report-event event port)
  (display (list ";; --- " (formatted-current-time) " ---\n") port)
  (display (list "Event type: " (car event) "\n") port)
  (display (list "In process: " (cadr event) "\n") port)
  (display (list (cddr event) "\n") port)
  (force-output port)
  port)

(define file-output-log-handler
  (make-event-handler
   ;; init
   (lambda (args)
     (match args
       ((filename) 
        (open-output-file (list path: filename
                                create: 'maybe
                                append: #t)))))
   ;; event
   report-event
   ;; call
   (lambda (term port)
     (values (void) port))
   ;; shutdown
   (lambda (reason port)
     (close-output-port port))))


;; 'type' is a keyword (error warning info debug)
(define (termite-log type message)
  (if logger
      (event-manager:notify logger (list type (self) message))))

(define (warning . terms)
  (termite-log 'warning terms))

(define (info . terms)
  (termite-log 'info terms))

(define (debug . terms)
  (termite-log 'debug terms))


;; --------------------
;; Services (initialized at INIT)
;;(define stdout #f)
(define spawner #f)
(define linker #f)
(define nameserver #f)
(define dispatcher #f)
(define tcp-listener #f)
(define logger #f)
;; --------------------


;; ----------------------------------------------------------------------------
;; Initialization

(define (node-init node)
  (process-links-set! (self) '())
  (set! current-node (lambda () node))

  ;; (current-termite-input-port
  ;;  (spawn-input-port (current-input-port)))
  ;; 
  ;; (current-termite-output-port
  ;;  (spawn-output-port (current-output-port)))

  ;; --------------------
  ;; Services
  ;;(set! stdout (spawn-output-port (current-output-port) #f))
  (set! spawner (start-spawner))
  (set! linker (start-linker))
  (set! nameserver (start-nameserver))
  (set! dispatcher (start-dispatcher))
  (set! tcp-listener
        (start-tcp-server (node-port (current-node)) start-messenger))
  (set! logger 
        (let ((logger (event-manager:start)))
          ;; (event-manager:add-handler logger
          ;;                            (make-simple-event-handler
          ;;                             report-event
          ;;                             (current-error-port)))
          (event-manager:add-handler logger
                                     file-output-log-handler
                                     "termite.log")
          logger))
  
  ;; registering the accessible exterior services to the nameserver
  (register 'nameserver nameserver)
  (register 'spawner spawner)
  (register 'linker linker)
  
  (random-source-randomize! default-random-source)

  'ok)

;; quick init
(define (init)
  (node-init (make-node "localhost" 3000)))

;; quick hack... (see tsi)
(define (init-from-shell-script)
  (with-exception-catcher
   (lambda (e)
     (##display-exception e (current-error-port))
     (error (string-append "usage: "
                           " [ip-address [tcp port#]]")))
   (lambda ()
     (match (command-line)
       ((_ _ _ _)
        (init))

       ((_ _ _ _ ip)
        (node-init (make-node ip 3000)))

       ((_ _ _ _ ip port#)
        (node-init (make-node ip (string->number port#))))))))


;; Some convenient definitions
(define node1 (make-node "localhost" 3000))
(define node2 (make-node "localhost" 3001))

