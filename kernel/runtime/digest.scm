;;;============================================================================

;;; File: "digest.scm", Time-stamp: <2009-02-19 16:26:49 feeley>

;;; Copyright (c) 2005-2009 by Marc Feeley, All Rights Reserved.

;;;============================================================================

;;; Contains procedures to compute message digests.

(##include "~~lib/gambit#.scm")

(declare
 (standard-bindings)
 (extended-bindings)
 (not inline)
 (not safe))

(define-macro (fx+ . args) `(##fx+ ,@args))
(define-macro (fx- . args) `(##fx- ,@args))
(define-macro (fx* . args) `(##fx* ,@args))
(define-macro (fxquotient . args) `(##fxquotient ,@args))
(define-macro (fxmin . args) `(##fxmin ,@args))
(define-macro (fx= . args) `(##fx= ,@args))
(define-macro (fx< . args) `(##fx< ,@args))
(define-macro (fx> . args) `(##fx> ,@args))
(define-macro (fx<= . args) `(##fx<= ,@args))
(define-macro (fx>= . args) `(##fx>= ,@args))
(define-macro (fxnot . args) `(##fxnot ,@args))
(define-macro (fxand . args) `(##fxand ,@args))
(define-macro (fxior . args) `(##fxior ,@args))
(define-macro (fxxor . args) `(##fxxor ,@args))
(define-macro (fxarithmetic-shift-right . args) `(##fxarithmetic-shift-right ,@args))
(define-macro (fxarithmetic-shift-left . args) `(##fxarithmetic-shift-left ,@args))
(define-macro (make-vector . args) `(##make-vector ,@args))
(define-macro (make-u8vector . args) `(##make-u8vector ,@args))
(define-macro (u8vector . args) `(##u8vector ,@args))
(define-macro (u8vector-length . args) `(##u8vector-length ,@args))
(define-macro (u8vector-ref . args) `(##u8vector-ref ,@args))
(define-macro (u8vector-set! . args) `(##u8vector-set! ,@args))
(define-macro (read-subu8vector . args) `(##read-subu8vector ,@args))
(define-macro (string-append . args) `(##string-append ,@args))
(define-macro (make-string . args) `(##make-string ,@args))
(define-macro (open-input-file . args) `(##open-input-file ,@args))
(define-macro (close-input-port . args) `(##close-input-port ,@args))
(define-macro (number->string . args) `(##number->string ,@args))

;;;----------------------------------------------------------------------------

(define-type digest
  id: digest-f65996cb-c1aa-4ee9-86cd-1af55b5ddb74
  end
  update
  state)

;;;----------------------------------------------------------------------------

(define (hash-block->hex-string hb big-endian? width)

  (define (hex x)
    (string-ref "0123456789abcdef" (fxand x 15)))

  (let* ((len (fxquotient width 16))
         (n (fx* len 4))
         (str (make-string n)))
    (let loop ((i (fx- len 1)) (j (fx- n 4)))
      (if (fx< i 0)
          str
          (let ((x (vector-ref hb (if big-endian? (fxxor i 1) i)))
                (j1 (if big-endian? (fx+ j 0) (fx+ j 2)))
                (j2 (if big-endian? (fx+ j 2) (fx+ j 0))))
            (string-set!
             str
             (fx+ j1 0)
             (hex (fxarithmetic-shift-right x 12)))
            (string-set!
             str
             (fx+ j1 1)
             (hex (fxarithmetic-shift-right x 8)))
            (string-set!
             str
             (fx+ j2 0)
             (hex (fxarithmetic-shift-right x 4)))
            (string-set!
             str
             (fx+ j2 1)
             (hex (fxarithmetic-shift-right x 0)))
            (loop (fx- i 1) (fx- j 4)))))))

(define (hash-block->u8vector hb big-endian? width)
  (let* ((len (fxquotient width 16))
         (n (fx* len 2))
         (u8vect (make-u8vector n 0)))
    (let loop ((i (fx- len 1)) (j (fx- n 2)))
      (if (fx< i 0)
          u8vect
          (let ((x (vector-ref hb (if big-endian? (fxxor i 1) i)))
                (j1 (if big-endian? (fx+ j 0) (fx+ j 1)))
                (j2 (if big-endian? (fx+ j 1) (fx+ j 0))))
            (u8vector-set! u8vect j1 (fxarithmetic-shift-right x 8))
            (u8vector-set! u8vect j2 (fxand #xff x))
            (loop (fx- i 1) (fx- j 2)))))))

(define-macro (LO var)
  (string->symbol
   (string-append (symbol->string var) "-" (symbol->string 'L))))

(define-macro (HI var)
  (string->symbol
   (string-append (symbol->string var) "-" (symbol->string 'H))))

(define-macro (wlet var lo hi body)
  `(let ((,(string->symbol
            (string-append (symbol->string var) "-" (symbol->string 'L)))
          ,lo)
         (,(string->symbol
            (string-append (symbol->string var) "-" (symbol->string 'H)))
          ,hi))
     ,body))

(define-macro (cast-u16 x)
  `(fxand #xffff ,x))

(define-macro (shift-left-u16 n shift)
  `(fxarithmetic-shift-left
    (fxand ,n ,(fx- (expt 2 (fx- 16 shift)) 1))
    ,shift))

(define-macro (wshr dst w r body)
  (if (fx< r 16)
      `(wlet ,dst
             (fxior
              (fxarithmetic-shift-right (LO ,w) ,r)
              (shift-left-u16 (HI ,w) ,(fx- 16 r)))
             (fxarithmetic-shift-right (HI ,w) ,r)
             ,body)
      `(wlet ,dst
             (fxarithmetic-shift-right (HI ,w) ,(fx- r 16))
             0
             ,body)))

(define-macro (wrot dst w r body)
  (if (fx< r 16)
      `(wlet ,dst
             (fxior
              (shift-left-u16 (LO ,w) ,r)
              (fxarithmetic-shift-right (HI ,w) ,(fx- 16 r)))
             (fxior
              (shift-left-u16 (HI ,w) ,r)
              (fxarithmetic-shift-right (LO ,w) ,(fx- 16 r)))
             ,body)
      `(wlet ,dst
             (fxior
              (shift-left-u16 (HI ,w) ,(fx- r 16))
              (fxarithmetic-shift-right (LO ,w) ,(fx- 32 r)))
             (fxior
              (shift-left-u16 (LO ,w) ,(fx- r 16))
              (fxarithmetic-shift-right (HI ,w) ,(fx- 32 r)))
             ,body)))

(define-macro (wadd dst a b body)
  `(wlet R
         (fx+ (LO ,a) (LO ,b))
         (fx+ (HI ,a) (HI ,b))
         (wlet ,dst
               (cast-u16 (LO R))
               (cast-u16
                (fx+ (HI R)
                     (fxarithmetic-shift-right (LO R) 16)))
               ,body)))

(define-macro (wxor dst a b body)
  `(wlet ,dst
         (fxxor (LO ,a) (LO ,b))
         (fxxor (HI ,a) (HI ,b))
         ,body))

(define-macro (wior dst a b body)
  `(wlet ,dst
         (fxior (LO ,a) (LO ,b))
         (fxior (HI ,a) (HI ,b))
         ,body))

(define-macro (wand dst a b body)
  `(wlet ,dst
         (fxand (LO ,a) (LO ,b))
         (fxand (HI ,a) (HI ,b))
         ,body))

(define-macro (wnot dst a body)
  `(wlet ,dst
         (fxnot (LO ,a))
         (fxnot (HI ,a))
         ,body))

(define-macro (wref dst v i body)
  (if (number? i)
      `(wlet ,dst
             (vector-ref ,v ,(fx+ (fx* 2 i) 0))
             (vector-ref ,v ,(fx+ (fx* 2 i) 1))
             ,body)
      `(wlet ,dst
             (vector-ref ,v (fx+ (fx* 2 ,i) 0))
             (vector-ref ,v (fx+ (fx* 2 ,i) 1))
             ,body)))

(define-macro (wset v i x)
  (if (number? i)
      `(begin
         (vector-set! ,v ,(fx+ (fx* 2 i) 0) (LO ,x))
         (vector-set! ,v ,(fx+ (fx* 2 i) 1) (HI ,x)))
      `(begin
         (vector-set! ,v (fx+ (fx* 2 ,i) 0) (LO ,x))
         (vector-set! ,v (fx+ (fx* 2 ,i) 1) (HI ,x)))))

;;;----------------------------------------------------------------------------

(define-type block-digest
  id: block-digest-f65996cb-c1aa-4ee9-86cd-1af55b5ddb74
  hash-update
  hash
  block
  block-pos
  bit-pos
  big-endian?
  width)

(define (convert-hash-block digest result-type)
  (let* ((bd (digest-state digest))
         (hash (block-digest-hash bd)))
    (case result-type
      ((hex)
       (hash-block->hex-string
        hash
        (block-digest-big-endian? bd)
        (block-digest-width bd)))
      ((u8vector)
       (hash-block->u8vector
        hash
        (block-digest-big-endian? bd)
        (block-digest-width bd)))
      (else
       (error "unsupported digest result-type" result-type)))))

(define (process-last-block digest)
  (let* ((bd
          (digest-state digest))
         (block-pos
          (block-digest-block-pos bd))
         (bit-pos
          (block-digest-bit-pos bd))
         (buf
          (make-u8vector 8 0)))

    (digest-update-u8 digest #x80) ;; add byte-aligned 1 bit

    (let ((zero-padding-bytes
           (fxquotient
            (fxand 511 (fx- 448 (block-digest-bit-pos bd)))
            8)))
      (let loop1 ((n zero-padding-bytes))
        (if (fx< 0 n)
            (let ((m (fxmin 8 n)))
              (digest-update-subu8vector
               digest
               buf
               0
               m) ;; add 0 bits
              (loop1 (fx- n m))))))

    (u8vector-set!
     buf
     0
     (fxand #xff bit-pos))

    (u8vector-set!
     buf
     1
     (fxior
      (fxarithmetic-shift-left (fxand #x7f block-pos) 1)
      (fxand #x01 (fxarithmetic-shift-right bit-pos 8))))

    (let loop2 ((i 2)
                (n (fxarithmetic-shift-right block-pos 7)))
      (if (fx> n 0)
          (begin
            (u8vector-set! buf i (fxand #xff n))
            (loop2 (fx+ i 1)
                   (fxarithmetic-shift-right n 8)))))

    (if (block-digest-big-endian? bd)
        (let loop3 ((i 3))
          (if (fx>= i 0)
              (let ((t (u8vector-ref buf i)))
                (u8vector-set! buf i (u8vector-ref buf (fx- 7 i)))
                (u8vector-set! buf (fx- 7 i) t)
                (loop3 (fx- i 1))))))

    (digest-update-subu8vector digest buf 0 8)));; add message length (in bits)

(define (end-block-digest digest result-type)
  (process-last-block digest)
  (convert-hash-block digest result-type))

(define (digest-update-block-digest digest u8vect start end)
  (let* ((bd (digest-state digest))
         (block (block-digest-block bd)))

    (define (aligned8 i bit-pos)

      ;; bit-pos is a multiple of 8

      (if (fx< i end)
          (let ((j (fxarithmetic-shift-right bit-pos 4)))
            (if (fx= 0 (fxand bit-pos 15))
                (begin
                  (if (block-digest-big-endian? bd)
                      (let ((j (fxxor j 1)))
                        (vector-set!
                         block
                         j
                         (fxarithmetic-shift-left
                          (u8vector-ref u8vect i)
                          8)))
                      (vector-set!
                       block
                       j
                       (u8vector-ref u8vect i)))
                  (let ((new-bit-pos (fx+ bit-pos 8)))
                    (aligned8 (fx+ i 1) new-bit-pos)))
                (begin
                  (if (block-digest-big-endian? bd)
                      (let ((j (fxxor j 1)))
                        (vector-set!
                         block
                         j
                         (fx+ (vector-ref block j)
                              (u8vector-ref u8vect i))))
                      (vector-set!
                       block
                       j
                       (fx+ (vector-ref block j)
                            (fxarithmetic-shift-left
                             (u8vector-ref u8vect i)
                             8))))
                  (let ((new-bit-pos (fx+ bit-pos 8)))
                    (if (fx= 512 new-bit-pos)
                      (begin
                        ((block-digest-hash-update bd) digest)
                        (block-digest-block-pos-set!
                         bd
                         (fx+ (block-digest-block-pos bd) 1))
                        (aligned16 (fx+ i 1) 0))
                      (aligned16 (fx+ i 1) new-bit-pos))))))
          (block-digest-bit-pos-set! bd bit-pos)))

    (define (aligned16 i bit-pos)

      ;; bit-pos is a multiple of 16

      (if (fx< (fx+ i 1) end)
          (let ((j (fxarithmetic-shift-right bit-pos 4)))
            (if (block-digest-big-endian? bd)
                (let ((j (fxxor j 1)))
                  (vector-set!
                   block
                   j
                   (fx+
                    (fxarithmetic-shift-left
                     (u8vector-ref u8vect i)
                     8)
                    (u8vector-ref u8vect (fx+ i 1)))))
                (vector-set!
                 block
                 j
                 (fx+
                  (fxarithmetic-shift-left
                   (u8vector-ref u8vect (fx+ i 1))
                   8)
                  (u8vector-ref u8vect i))))
            (let ((new-bit-pos (fx+ bit-pos 16)))
              (if (fx= 512 new-bit-pos)
                  (begin
                    ((block-digest-hash-update bd) digest)
                    (block-digest-block-pos-set!
                     bd
                     (fx+ (block-digest-block-pos bd) 1))
                    (aligned16 (fx+ i 2) 0))
                  (aligned16 (fx+ i 2) new-bit-pos))))
          (aligned8 i bit-pos)))

    (let ((bit-pos (block-digest-bit-pos bd)))
      (cond ((fx= 0 (fxand bit-pos 15)) ;; 16 bit boundary?
             (aligned16 start bit-pos))
            (else
             ;; (fx= 0 (fxand bit-pos 7)) ;; 8 bit boundary?
             (aligned8 start bit-pos))))))

;;;----------------------------------------------------------------------------

;; SHA-1 digest.

(define (hash-block-init-sha-1)
  (vector #x2301 #x6745
          #xab89 #xefcd
          #xdcfe #x98ba
          #x5476 #x1032
          #xe1f0 #xc3d2))

(define (digest-update-sha-1 digest)
  (let* ((bd (digest-state digest))
         (hash (block-digest-hash bd))
         (block (block-digest-block bd)))
    (wref OLDA hash 0
    (wref OLDB hash 1
    (wref OLDC hash 2
    (wref OLDD hash 3
    (wref OLDE hash 4
    (let loop ((j 0)
               (A-L OLDA-L) (A-H OLDA-H)
               (B-L OLDB-L) (B-H OLDB-H)
               (C-L OLDC-L) (C-H OLDC-H)
               (D-L OLDD-L) (D-H OLDD-H)
               (E-L OLDE-L) (E-H OLDE-H))

      (define (stage1)
        (if (fx< j 16)

            (wref T1 block j
            (stage2 T1-L T1-H))

            (wref T1 block (fx- j 3)
            (wref T2 block (fx- j 8)
            (wxor T3 T1 T2
            (wref T4 block (fx- j 14)
            (wxor T5 T3 T4
            (wref T6 block (fx- j 16)
            (wxor T7 T5 T6
            (wrot X T7 1
            (begin
              (wset block j X)
              (stage2 X-L X-H))))))))))))

      (define (stage2 X-L X-H)
        (cond ((fx< j 20)
               (wand T1 B C
               (wnot T2 B
               (wand T3 D T2
               (wior T4 T1 T3
               (wlet T5 #x7999 #x5a82
               (wadd T6 T4 T5
               (stage3 X-L X-H T6-L T6-H))))))))
              ((fx< j 40)
               (wxor T1 B C
               (wxor T2 D T1
               (wlet T3 #xeba1 #x6ed9
               (wadd T4 T2 T3
               (stage3 X-L X-H T4-L T4-H))))))
              ((fx< j 60)
               (wand T1 B C
               (wand T2 B D
               (wior T3 T1 T2
               (wand T4 C D
               (wior T5 T3 T4
               (wlet T6 #xbcdc #x8f1b
               (wadd T7 T5 T6
               (stage3 X-L X-H T7-L T7-H)))))))))
              (else
               (wxor T1 B C
               (wxor T2 D T1
               (wlet T3 #xc1d6 #xca62
               (wadd T4 T2 T3
               (stage3 X-L X-H T4-L T4-H))))))))

      (define (stage3 X-L X-H Y-L Y-H)
        (wrot T1 A 5
        (wadd T2 E T1
        (wadd T3 X T2
        (wadd T4 Y T3
        (wrot T5 B 30
        (loop (fx+ j 1)
              T4-L T4-H
              A-L A-H
              T5-L T5-H
              C-L C-H
              D-L D-H)))))))

      (if (fx< j 80)

          (stage1)

          (begin
            (wadd NEWA A OLDA (wset hash 0 NEWA))
            (wadd NEWB B OLDB (wset hash 1 NEWB))
            (wadd NEWC C OLDC (wset hash 2 NEWC))
            (wadd NEWD D OLDD (wset hash 3 NEWD))
            (wadd NEWE E OLDE (wset hash 4 NEWE))))))))))))

(define (open-digest-sha-1)
  (make-digest
   end-block-digest
   digest-update-block-digest
   (make-block-digest
    digest-update-sha-1
    (hash-block-init-sha-1)
    (make-vector 160 0)
    0
    0
    #t
    160)))

;;;----------------------------------------------------------------------------

(define (open-digest algorithm)
  (case algorithm
    ((sha-1 SHA-1)
     (open-digest-sha-1))
    (else
     (error "unknown hashing algorithm" algorithm))))

(define-macro (digest-default-result-type) ''hex)

(define (close-digest
         digest
         #!optional
         (result-type (digest-default-result-type)))
  ((digest-end digest) digest result-type))

(define (digest-update-subu8vector digest u8vect start end)
  ((digest-update digest) digest u8vect start end))

(define zero-u8vector (make-u8vector 4 0))

(define (get-zero-u8vector) zero-u8vector)

(define (digest-update-u8 digest n) ;; assumes n is a fixnum
  (digest-update-subu8vector
   digest
   (if (eqv? n 0)
       (get-zero-u8vector)
       (make-u8vector 1 (fxand n #xff)))
   0
   1))

(define (digest-string
         str
         algorithm
         #!optional
         (result-type (digest-default-result-type)))
  (digest-substring
   str
   0
   (string-length str)
   algorithm
   result-type))

(define (digest-substring
         str
         start
         end
         algorithm
         #!optional
         (result-type (digest-default-result-type)))
  (let* ((len (fx- end start))
         (u8vect (make-u8vector len)))
    (let loop ((i 0))
      (if (fx< i len)
          (begin
            (u8vector-set! u8vect i (char->integer (string-ref str i)))
            (loop (fx+ i 1)))
          (digest-subu8vector u8vect 0 len algorithm result-type)))))

(define (digest-u8vector
         u8vect
         algorithm
         #!optional
         (result-type (digest-default-result-type)))
  (digest-subu8vector
   u8vect
   0
   (u8vector-length u8vect)
   algorithm
   result-type))

(define (digest-subu8vector
         u8vect
         start
         end
         algorithm
         #!optional
         (result-type (digest-default-result-type)))
  (let ((digest (open-digest algorithm)))
    (digest-update-subu8vector digest u8vect start end)
    (close-digest digest result-type)))

(define (digest-file
         filename
         algorithm
         #!optional
         (result-type (digest-default-result-type)))
  (let ((digest (open-digest algorithm)))
    (let* ((in (open-input-file filename))
           (bufsize 1024)
           (buf (make-u8vector bufsize)))
      (let loop ()
        (let ((n (read-subu8vector buf 0 bufsize in)))
          (if (fx= n 0)
              (begin
                (close-input-port in)
                (close-digest digest result-type))
              (begin
                (digest-update-subu8vector digest buf 0 n)
                (loop))))))))

;;;============================================================================
