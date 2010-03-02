;==============================================================================

; File: "http.scm", Time-stamp: <2009-03-13 12:05:07 feeley>

; Copyright (c) 2005-2008 by Marc Feeley, All Rights Reserved.

;==============================================================================

(unit webserver.implementation.http

(##include "~~lib/gambit#.scm")

(declare
  (standard-bindings)
  (extended-bindings)
  (block)
  (not safe)
)

;==============================================================================

; URI parsing.

(define hex-digit
  (lambda (str i)
    (let ((n (char->integer (string-ref str i))))
      (cond ((and (>= n 48) (<= n 57))
             (- n 48))
            ((and (>= n 65) (<= n 70))
             (- n 55))
            ((and (>= n 97) (<= n 102))
             (- n 87))
            (else
             #f)))))

(define hex-octet
  (lambda (str i)
    (let ((n1 (hex-digit str i)))
      (and n1
           (let ((n2 (hex-digit str (+ i 1))))
             (and n2
                  (+ (* n1 16) n2)))))))

(define plausible-hex-escape?
  (lambda (str end j)
    (and (< (+ j 2) end)
         (not (control-or-space-char? (string-ref str (+ j 1))))
         (not (control-or-space-char? (string-ref str (+ j 2)))))))

(define control-or-space-char?
  (lambda (c)
    (or (not (char<? #\space c))
        (not (char<? c #\x7f)))))

(define excluded-char?
  (lambda (c)
    (or (not (char<? #\space c))
        (not (char<? c #\x7f))
        (char=? c #\<)
        (char=? c #\>)
        (char=? c #\#)
        (char=? c #\%)
        (char=? c #\")
        (char=? c #\{)
        (char=? c #\})
        (char=? c #\|)
        (char=? c #\\)
        (char=? c #\^)
        (char=? c #\[)
        (char=? c #\])
        (char=? c #\`))))

(define extract-escaped
  (lambda (str start n)
    (let ((result (make-string n)))
      (let loop ((i start) (j 0))
        (if (< j n)
            (let ((c (string-ref str i)))
              (if (char=? c #\%)
                  (let ((n (hex-octet str (+ i 1))))
                    (and n
                         (begin
                           (string-set! result j (integer->char n))
                           (loop (+ i 3)
                                 (+ j 1)))))
                  (begin
                    (string-set! result j (if (char=? c #\+) #\space c))
                    (loop (+ i 1)
                          (+ j 1)))))
            result)))))

(define-type uri
  id: 62788556-c247-11d9-9598-00039301ba52

  scheme
  authority
  path
  query
  fragment
)

(define parse-uri
  (lambda (str start end decode? cont)
    (let ((uri (make-uri #f #f "" #f #f)))

      (define extract-string
        (lambda (i j n)
          (if decode?
              (extract-escaped str i n)
              (substring str i j))))

      (define extract-query
        (lambda (i j n)
          (if decode?
              (parse-uri-query
               str
               i
               j
               decode?
               (lambda (bindings end)
                 bindings))
              (substring str i j))))

      (define state0 ; possibly inside the "scheme" part
        (lambda (i j n)
          (if (< j end)
              (let ((c (string-ref str j)))
                (cond ((char=? c #\:)
                       (if (= n 0)
                           (state2 j (+ j 1) 1) ; the ":" is in the "path" part
                           (let ((scheme (extract-string i j n)))
                             (and scheme
                                  (begin
                                    (uri-scheme-set! uri scheme)
                                    (if (and (< (+ j 2) end)
                                             (char=? (string-ref str (+ j 1))
                                                     #\/)
                                             (char=? (string-ref str (+ j 2))
                                                     #\/))
                                        (state1 (+ j 3) (+ j 3) 0)
                                        (state2 (+ j 1) (+ j 1) 0)))))))
                      ((char=? c #\/)
                       (if (and (= n 0)
                                (< (+ j 1) end)
                                (char=? (string-ref str (+ j 1)) #\/))
                           (state1 (+ j 2) (+ j 2) 0)
                           (state2 i (+ j 1) (+ n 1))))
                      ((char=? c #\?)
                       (let ((path (extract-string i j n)))
                         (and path
                              (begin
                                (uri-path-set! uri path)
                                (state3 (+ j 1) (+ j 1) 0)))))
                      ((char=? c #\#)
                       (let ((path (extract-string i j n)))
                         (and path
                              (begin
                                (uri-path-set! uri path)
                                (state4 (+ j 1) (+ j 1) 0)))))
                      ((char=? c #\%)
                       (and (plausible-hex-escape? str end j)
                            (state0 i (+ j 3) (+ n 1))))
                      ((control-or-space-char? c)
                       (let ((path (extract-string i j n)))
                         (and path
                              (begin
                                (uri-path-set! uri path)
                                j))))
                      (else
                       (state0 i (+ j 1) (+ n 1)))))
              (let ((path (extract-string i j n)))
                (and path
                     (begin
                       (uri-path-set! uri path)
                       j))))))

      (define state1 ; inside the "authority" part
        (lambda (i j n)
          (if (< j end)
              (let ((c (string-ref str j)))
                (cond ((char=? c #\/)
                       (let ((authority (extract-string i j n)))
                         (and authority
                              (begin
                                (uri-authority-set! uri authority)
                                (state2 j (+ j 1) 1)))))
                      ((char=? c #\?)
                       (let ((authority (extract-string i j n)))
                         (and authority
                              (begin
                                (uri-authority-set! uri authority)
                                (state3 (+ j 1) (+ j 1) 0)))))
                      ((char=? c #\#)
                       (let ((authority (extract-string i j n)))
                         (and authority
                              (begin
                                (uri-authority-set! uri authority)
                                (state4 (+ j 1) (+ j 1) 0)))))
                      ((char=? c #\%)
                       (and (plausible-hex-escape? str end j)
                            (state1 i (+ j 3) (+ n 1))))
                      ((control-or-space-char? c)
                       (let ((authority (extract-string i j n)))
                         (and authority
                              (begin
                                (uri-authority-set! uri authority)
                                j))))
                      (else
                       (state1 i (+ j 1) (+ n 1)))))
              (let ((authority (extract-string i j n)))
                (and authority
                     (begin
                       (uri-authority-set! uri authority)
                       j))))))

      (define state2 ; inside the "path" part
        (lambda (i j n)
          (if (< j end)
              (let ((c (string-ref str j)))
                (cond ((char=? c #\?)
                       (let ((path (extract-string i j n)))
                         (and path
                              (begin
                                (uri-path-set! uri path)
                                (state3 (+ j 1) (+ j 1) 0)))))
                      ((char=? c #\#)
                       (let ((path (extract-string i j n)))
                         (and path
                              (begin
                                (uri-path-set! uri path)
                                (state4 (+ j 1) (+ j 1) 0)))))
                      ((char=? c #\%)
                       (and (plausible-hex-escape? str end j)
                            (state2 i (+ j 3) (+ n 1))))
                      ((control-or-space-char? c)
                       (let ((path (extract-string i j n)))
                         (and path
                              (begin
                                (uri-path-set! uri path)
                                j))))
                      (else
                       (state2 i (+ j 1) (+ n 1)))))
              (let ((path (extract-string i j n)))
                (and path
                     (begin
                       (uri-path-set! uri path)
                       j))))))

      (define state3 ; inside the "query" part
        (lambda (i j n)
          (if (< j end)
              (let ((c (string-ref str j)))
                (cond ((char=? c #\#)
                       (let ((query (extract-query i j n)))
                         (and query
                              (begin
                                (uri-query-set! uri query)
                                (state4 (+ j 1) (+ j 1) 0)))))
                      ((char=? c #\%)
                       (and (plausible-hex-escape? str end j)
                            (state3 i (+ j 3) (+ n 1))))
                      ((control-or-space-char? c)
                       (let ((query (extract-query i j n)))
                         (and query
                              (begin
                                (uri-query-set! uri query)
                                j))))
                      (else
                       (state3 i (+ j 1) (+ n 1)))))
              (let ((query (extract-query i j n)))
                (and query
                     (begin
                       (uri-query-set! uri query)
                       j))))))

      (define state4 ; inside the "fragment" part
        (lambda (i j n)
          (if (< j end)
              (let ((c (string-ref str j)))
                (cond ((char=? c #\%)
                       (and (plausible-hex-escape? str end j)
                            (state4 i (+ j 3) (+ n 1))))
                      ((control-or-space-char? c)
                       (let ((fragment (extract-string i j n)))
                         (and fragment
                              (begin
                                (uri-fragment-set! uri fragment)
                                j))))
                      (else
                       (state4 i (+ j 1) (+ n 1)))))
              (let ((fragment (extract-string i j n)))
                (and fragment
                     (begin
                       (uri-fragment-set! uri fragment)
                       j))))))

      (let ((i (state0 start start 0)))
        (cont (and i uri)
              (or i start))))))

(define parse-uri-query
  (lambda (str start end decode? cont)
    (let ((rev-bindings '()))

      (define extract-string
        (lambda (i j n)
          (if decode?
              (extract-escaped str i n)
              (substring str i j))))

      (define state0
        (lambda (i j n)
          (if (< j end)
            (let ((c (string-ref str j)))
              (cond ((char=? c #\%)
                     (and (plausible-hex-escape? str end j)
                          (state0 i
                                  (+ j 3)
                                  (+ n 1))))
                    ((char=? c #\=)
                     (let ((name (extract-string i j n)))
                       (and name
                            (let ((j (+ j 1)))
                              (state1 j
                                      j
                                      0
                                      name)))))
                    ((char=? c #\&)
                     #f)
                    ((excluded-char? c)
                     (if (= n 0)
                         j
                         #f))
                    (else
                     (state0 i
                             (+ j 1)
                             (+ n 1)))))
            (if (= n 0)
                j
                #f))))

      (define state1
        (lambda (i j n name)
          (if (< j end)
            (let ((c (string-ref str j)))
              (cond ((char=? c #\%)
                     (and (plausible-hex-escape? str end j)
                          (state1 i
                                  (+ j 3)
                                  (+ n 1)
                                  name)))
                    ((char=? c #\&)
                     (let ((val (extract-string i j n)))
                       (and val
                            (let ((j (+ j 1)))
                              (set! rev-bindings
                                    (cons (cons name val) rev-bindings))
                              (and (< j end)
                                   (state0 j
                                           j
                                           0))))))
                    ((char=? c #\=)
                     #f)
                    ((excluded-char? c)
                     (let ((val (extract-string i j n)))
                       (and val
                            (begin
                              (set! rev-bindings
                                    (cons (cons name val) rev-bindings))
                              j))))
                    (else
                     (state1 i
                             (+ j 1)
                             (+ n 1)
                             name))))
            (let ((val (extract-string i j n)))
              (and val
                   (begin
                     (set! rev-bindings
                           (cons (cons name val) rev-bindings))
                     j))))))

      (let ((i (state0 start start 0)))
        (cont (and i (reverse rev-bindings))
              (or i start))))))

;==============================================================================

; x-www-form-urlencoded decoding.

(define decode-x-www-form-urlencoded
  (lambda (str)
    (let ((n (string-length str)))

      (define extract
        (lambda (start len)
          (let ((s (make-string len)))
            (let loop ((i start) (j 0))
              (if (< j len)
                  (let ((c (string-ref str i)))
                    (cond ((char=? c #\%)
                           (cond ((hex (+ i 1))
                                  =>
                                  (lambda (x)
                                    (string-set! s j (integer->char x))
                                    (loop (+ i 3) (+ j 1))))
                                 (else
                                  #f)))
                          ((char=? c #\+)
                           (string-set! s j #\space)
                           (loop (+ i 1) (+ j 1)))
                          (else
                           (string-set! s j c)
                           (loop (+ i 1) (+ j 1)))))
                  s)))))

      (define hex
        (lambda (i)
          (if (< (+ i 1) n)
              (let ((h1 (nibble i))
                    (h2 (nibble (+ i 1))))
                (and h1 h2 (+ (* h1 16) h2)))
              #f)))

      (define nibble
        (lambda (i)
          (let ((c (string-ref str i)))
            (cond ((and (char>=? c #\0) (char<=? c #\9))
                   (- (char->integer c) (char->integer #\0)))
                  ((and (char>=? c #\a) (char<=? c #\f))
                   (+ 10 (- (char->integer c) (char->integer #\a))))
                  ((and (char>=? c #\A) (char<=? c #\F))
                   (+ 10 (- (char->integer c) (char->integer #\A))))
                  (else
                   #f)))))

      (define state0 ; at beginning of string
        (lambda (i rev-fields)
          (if (< i n)
              (state1 i
                      i
                      0
                      rev-fields)
              (reverse rev-fields))))

      (define state1 ; in field name
        (lambda (i start len rev-fields)
          (if (< i n)
              (let ((c (string-ref str i)))
                (cond ((char=? c #\=)
                       (state2 (+ i 1)
                               (+ i 1)
                               0
                               (extract start len)
                               rev-fields))
                      ((char=? c #\%)
                       (and (hex (+ i 1))
                            (state1 (+ i 3)
                                    start
                                    (+ len 1)
                                    rev-fields)))
                      (else
                       (state1 (+ i 1)
                               start
                               (+ len 1)
                               rev-fields))))
              #f)))

      (define state2 ; in field value
        (lambda (i start len name rev-fields)

          (define end-of-field
            (lambda ()
              (cons (cons name (extract start len))
                    rev-fields)))

          (if (< i n)
              (let ((c (string-ref str i)))
                (cond ((char=? c #\&)
                       (state1 (+ i 1)
                               (+ i 1)
                               0
                               (end-of-field)))
                      ((char=? c #\%)
                       (and (hex (+ i 1))
                            (state2 (+ i 3)
                                    start
                                    (+ len 1)
                                    name
                                    rev-fields)))
                      (else
                       (state2 (+ i 1)
                               start
                               (+ len 1)
                               name
                               rev-fields))))
              (reverse (end-of-field)))))

      (state0 0 '()))))

;==============================================================================

; HTTP server.

(define-type server
  id: c69165bd-c13f-11d9-830f-00039301ba52

  port-number
  timeout
  dispatcher
)

(define make-http-server
  (lambda (#!key
           (port-number 8080)
           (timeout     300)
           (dispatcher  default-dispatcher))
    (make-server
     port-number
     timeout
     dispatcher)))

(define default-dispatcher
  (lambda (connection method uri parameters headers)
    (reply connecton "404 NOT FOUND" "Server not configured." '())))

(define http-server-start!
  (lambda (server)
    (let ((server-port
           (open-tcp-server
            (list server-address: '#u8(127 0 0 1) ; on localhost interface only
                  port-number: (server-port-number server)
                  backlog: 128
                  reuse-address: #t
                  char-encoding: 'ISO-8859-1))))
      (thread-start!
       (make-thread
        (lambda ()
          (let loop ()
            (let ((connection (read server-port)))
              (thread-start!
               (make-thread
                (lambda ()
                  (serve-connection server connection)))))
            (loop))))))))

(define serve-connection
  (lambda (server connection)
    (input-port-timeout-set! connection 300)
    (output-port-timeout-set! connection 300)
    (let ((error (lambda ()
                   (reply connection
                          "400 Bad Request"
                          "400 Bad Request")))
          (req (permissive-read-line connection)))
      (if (string? req)
          (let* ((first-space
                  (find-char-pos req #\space))
                 (method
                  (and first-space
                       (substring req 0 first-space))))
            (if method
                (parse-uri
                 req
                 (+ first-space 1)
                 (string-length req)
                 #t
                 (lambda (uri i)
                   (if (and uri
                            (char=? (string-ref req i) #\space))
                       (let ((protocol
                              (substring
                               req
                               (+ i 1)
                               (string-length req)))
                             (headers (read-headers connection)))
                         (let* ((content
                                 (read-content connection headers))
                                (parameters
                                 (let ((x (assoc "Content-Type" headers)))
                                   (if (and ;; needs to get parameters from both
                                        x
                                        (string=?
                                         (cdr x)
                                         "application/x-www-form-urlencoded"))
                                       (decode-x-www-form-urlencoded content)
                                       (uri-query uri)))))
                           ((server-dispatcher server)
                            connection
                            method
                            uri
                            parameters
                            headers)))
                       (error))))
                (error)))
          (error)))))

(define read-headers
  (lambda (connection)
    (let loop ((headers '()))
      (let ((line (permissive-read-line connection)))
        (cond ((not line)
               #f)
              ((= (string-length line) 0)
               headers)
              (else
               (let ((header
                      (let ((pos (find-char-pos line #\:)))
                        (and pos
                             (< (+ pos 1) (string-length line))
                             (char=? #\space (string-ref line (+ pos 1)))
                             (cons (substring line 0 pos)
                                   (substring line
                                              (+ pos 2)
                                              (string-length line)))))))
                 (if header
                     (loop (cons header headers))
                     #f))))))))

(define read-content
  (lambda (connection headers)
    (let ((cl
           (cond ((assoc "Content-Length" headers)
                  =>
                  (lambda (x)
                    (let ((n (string->number (cdr x))))
                      (and n (integer? n) (exact? n) n))))
                 (else
                  #f))))
      (if cl
          (let ((str (make-string cl)))
            (let ((n (read-substring str 0 cl connection)))
              (if (= n cl)
                  str
                  "")))
          ""))))

(define reply
  (lambda (connection status reply-body headers-out)
    (let ((eol "\r\n"))
      (print port: connection
       (list "HTTP/1.1 " status eol
             "Content-Length: " (string-length reply-body) eol
             "Content-Type: text/html; charset=ISO-8859-1" eol
             "Connection: close" eol
             eol)))
    (print port: connection reply-body)
    (close-port connection)))

;-------------------------------------------------------------------------------

(define permissive-read-line
  (lambda (port)
    (let ((s (read-line port)))
      (if (and (string? s)
               (> (string-length s) 0)
               (char=? (string-ref s (- (string-length s) 1)) #\return))
          ; efficient version of (substring s 0 (- (string-length s) 1))
          (begin (##string-shrink! s (- (string-length s) 1)) s)
          s))))

(define find-char-pos
  (lambda (str char)
    (let loop ((i 0))
      (if (< i (string-length str))
          (if (char=? char (string-ref str i))
              i
              (loop (+ i 1)))
          #f))))
)

;; (define test-dispatcher
;;   (lambda (connection method uri parameters headers)
;;     (pretty-print "Dispatching request")
;;     (reply connection "200 OK" "Hello World" '())))

;; (define test-server
;;   (make-http-server
;;    dispatcher: test-dispatcher))

;; (http-server-start! test-server)
