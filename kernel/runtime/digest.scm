;==============================================================================

; File: "digest.scm", Time-stamp: <2007-04-04 11:28:52 feeley>

; Copyright (c) 2005-2007 by Marc Feeley, All Rights Reserved.

;==============================================================================

(##include "~~lib/gambit#.scm")

(declare
  (standard-bindings)
  (extended-bindings)
  (block)
  (not safe)
  ;(fixnum)
)

;==============================================================================

; Utilities to perform 32 bit operations using fixnums.

(define-macro (make-hash-block n) `(##make-u16vector ,n 0))
(define-macro (hash-block . args) `(##u16vector ,@args))
(define-macro (hash-block-ref hb i) `(##u16vector-ref ,hb ,i))
(define-macro (hash-block-set! hb i x) `(##u16vector-set! ,hb ,i ,x))

(define (hash-block->hex-string hb)

  (define (hex x)
    (##string-ref "0123456789abcdef" (##fixnum.bitwise-and x 15)))

  (let* ((len (u16vector-length hb))
         (n (##fixnum.* len 4))
         (str (##make-string n)))
    (let loop ((i (##fixnum.- len 1)) (j (##fixnum.- n 4)))
      (if (##fixnum.< i 0)
          str
          (let ((x (hash-block-ref hb i)))
            (##string-set!
             str
             (##fixnum.+ j 0)
             (hex (##fixnum.arithmetic-shift-right x 4)))
            (##string-set!
             str
             (##fixnum.+ j 1)
             (hex (##fixnum.arithmetic-shift-right x 0)))
            (##string-set!
             str
             (##fixnum.+ j 2)
             (hex (##fixnum.arithmetic-shift-right x 12)))
            (##string-set!
             str
             (##fixnum.+ j 3)
             (hex (##fixnum.arithmetic-shift-right x 8)))
            (loop (##fixnum.- i 1) (##fixnum.- j 4)))))))

(define-macro (LO var)
  (string->symbol (string-append (symbol->string var) "-L")))

(define-macro (HI var)
  (string->symbol (string-append (symbol->string var) "-H")))

(define-macro (wlet var lo hi body)
  `(let ((,(string->symbol (string-append (symbol->string var) "-L")) ,lo)
         (,(string->symbol (string-append (symbol->string var) "-H")) ,hi))
     ,body))

(define-macro (cast-u16 x)
  `(##fixnum.bitwise-and #xffff ,x))

(define-macro (wrot dst w r body)
  (if (< r 16)
      `(wlet ,dst
             (cast-u16
              (##fixnum.bitwise-ior
               (##fixnum.arithmetic-shift-left (LO ,w) ,r)
               (##fixnum.arithmetic-shift-right (HI ,w) ,(- 16 r))))
             (cast-u16
              (##fixnum.bitwise-ior
               (##fixnum.arithmetic-shift-left (HI ,w) ,r)
               (##fixnum.arithmetic-shift-right (LO ,w) ,(- 16 r))))
             ,body)
      `(wlet ,dst
             (cast-u16
              (##fixnum.bitwise-ior
               (##fixnum.arithmetic-shift-left (HI ,w) ,(- r 16))
               (##fixnum.arithmetic-shift-right (LO ,w) ,(- 32 r))))
             (cast-u16
              (##fixnum.bitwise-ior
               (##fixnum.arithmetic-shift-left (LO ,w) ,(- r 16))
               (##fixnum.arithmetic-shift-right (HI ,w) ,(- 32 r))))
             ,body)))

(define-macro (wadd dst a b body)
  `(wlet R
         (##fixnum.+ (LO ,a) (LO ,b))
         (##fixnum.+ (HI ,a) (HI ,b))
         (wlet ,dst
               (cast-u16 (LO R))
               (cast-u16
                (##fixnum.+ (HI R)
                            (##fixnum.arithmetic-shift-right (LO R) 16)))
               ,body)))

(define-macro (wxor dst a b body)
  `(wlet ,dst
         (##fixnum.bitwise-xor (LO ,a) (LO ,b))
         (##fixnum.bitwise-xor (HI ,a) (HI ,b))
         ,body))

(define-macro (wior dst a b body)
  `(wlet ,dst
         (##fixnum.bitwise-ior (LO ,a) (LO ,b))
         (##fixnum.bitwise-ior (HI ,a) (HI ,b))
         ,body))

(define-macro (wand dst a b body)
  `(wlet ,dst
         (##fixnum.bitwise-and (LO ,a) (LO ,b))
         (##fixnum.bitwise-and (HI ,a) (HI ,b))
         ,body))

(define-macro (wnot dst a body)
  `(wlet ,dst
         (##fixnum.bitwise-not (LO ,a))
         (##fixnum.bitwise-not (HI ,a))
         ,body))

;------------------------------------------------------------------------------

; Generic interface.

(define-type digest
  id: 1ce13de0-ccaa-4627-94be-b13eaa2c32e6
  close-digest
  hash-update
  hash
  block
  block-pos
  bit-pos
)

(define block-bit-length 512)

(define (open-digest algorithm)
  (case algorithm
    ((sha-1 SHA-1)
     (open-sha-1-digest))
    (else
     (error "unimplemented hashing algorithm" algorithm))))

(define (close-digest digest
                      #!optional
                      (result-type 'hex-string))
  ((digest-close-digest digest) digest result-type))

(define (digest-update-subu8vector digest u8vect start end)
  (let ((block (digest-block digest)))

    (define (aligned8 i bit-pos)

      ; bit-pos is a multiple of 8

      (if (##fixnum.< i end)
          (let ((j (##fixnum.arithmetic-shift-right bit-pos 4)))
            (if (##fixnum.= 0 (##fixnum.bitwise-and bit-pos 15))
                (begin
                  (if #t
                      (hash-block-set!
                       block
                       j
                       (##u8vector-ref u8vect i))
                      (let ((j (##fixnum.bitwise-xor j 1)))
                        (hash-block-set!
                         block
                         j
                         (##u8vector-ref u8vect i))))
                  (let ((new-bit-pos (##fixnum.+ bit-pos 8)))
                    (aligned8 (##fixnum.+ i 1) new-bit-pos)))
                (begin
                  (if #t
                      (hash-block-set!
                       block
                       j
                       (##fixnum.+ (hash-block-ref block j)
                                   (##fixnum.arithmetic-shift-left
                                    (##u8vector-ref u8vect i)
                                    8)))
                      (let ((j (##fixnum.bitwise-xor j 1)))
                        (hash-block-set!
                         block
                         j
                         (##fixnum.+ (hash-block-ref block j)
                                     (##fixnum.arithmetic-shift-left
                                      (##u8vector-ref u8vect i)
                                      8)))))
                  (let ((new-bit-pos (##fixnum.+ bit-pos 8)))
                    (if (##fixnum.= block-bit-length new-bit-pos)
                      (begin
                        ((digest-hash-update digest) digest)
                        (digest-block-pos-set!
                         digest
                         (##fixnum.+ (digest-block-pos digest) 1))
                        (aligned16 (##fixnum.+ i 1) 0))
                      (aligned16 (##fixnum.+ i 1) new-bit-pos))))))
          (digest-bit-pos-set! digest bit-pos)))

    (define (aligned16 i bit-pos)

      ; bit-pos is a multiple of 16

      (if (##fixnum.< (##fixnum.+ i 1) end)
          (let ((j (##fixnum.arithmetic-shift-right bit-pos 4)))
            (hash-block-set!
             block
             j
             (##fixnum.+
              (##fixnum.arithmetic-shift-left
               (##u8vector-ref u8vect (##fixnum.+ i 1))
               8)
              (##u8vector-ref u8vect i)))
            (let ((new-bit-pos (##fixnum.+ bit-pos 16)))
              (if (##fixnum.= block-bit-length new-bit-pos)
                  (begin
                    ((digest-hash-update digest) digest)
                    (digest-block-pos-set!
                     digest
                     (##fixnum.+ (digest-block-pos digest) 1))
                    (aligned16 (##fixnum.+ i 2) 0))
                  (aligned16 (##fixnum.+ i 2) new-bit-pos))))
          (aligned8 i bit-pos)))

;    (set! aligned16 aligned8)

    (let ((bit-pos (digest-bit-pos digest)))
      (cond ((##fixnum.= 0 (##fixnum.bitwise-and bit-pos 15)) ; u16 boundary?
             (aligned16 start bit-pos))
            (else
             ; (##fixnum.= 0 (##fixnum.bitwise-and bit-pos 7)) ; u8 boundary?
             (aligned8 start bit-pos))))))

(define zero-u8vector '#u8(0 0 0 0 0 0 0 0))

(define (digest-update-u8 digest n)
  (digest-update-subu8vector
   digest
   (if (##eqv? n 0)
       zero-u8vector
       (##u8vector n))
   0
   1))

(define (digest-update-u16-le digest n)
  (digest-update-subu8vector
   digest
   (if (##eqv? n 0)
       zero-u8vector
       (##u8vector (##fixnum.bitwise-and n #xff)
                   (##fixnum.arithmetic-shift-right n 8)))
   0
   2))

(define (digest-update-u16-be digest n)
  (digest-update-subu8vector
   digest
   (if (##eqv? n 0)
       zero-u8vector
       (##u8vector (##fixnum.arithmetic-shift-right n 8)
                   (##fixnum.bitwise-and n #xff)))
   0
   2))

(define (digest-update-u32-le digest n)
  (digest-update-subu8vector
   digest
   (if (##eqv? n 0)
       zero-u8vector
       (let ((lo16 (##bitwise-and n #xffff)); might overflow fixnums
             (hi16 (##arithmetic-shift n -16)))
         (##u8vector (##fixnum.bitwise-and lo16 #xff)
                     (##fixnum.arithmetic-shift-right lo16 8)
                     (##fixnum.bitwise-and hi16 #xff)
                     (##fixnum.arithmetic-shift-right hi16 8))))
   0
   4))

(define (digest-update-u32-be digest n)
  (digest-update-subu8vector
   digest
   (if (##eqv? n 0)
       zero-u8vector
       (let ((lo16 (##bitwise-and n #xffff)); might overflow fixnums
             (hi16 (##arithmetic-shift n -16)))
         (##u8vector (##fixnum.arithmetic-shift-right hi16 8)
                     (##fixnum.bitwise-and hi16 #xff)
                     (##fixnum.arithmetic-shift-right lo16 8)
                     (##fixnum.bitwise-and lo16 #xff))))
   0
   4))

(define (digest-update-u64-le digest n)
  (digest-update-u32-le digest (##bitwise-and n #xffffffff)); might overflow fixnums
  (digest-update-u32-le digest (##arithmetic-shift n -32)))

(define (digest-update-u64-be digest n)
  (digest-update-u32-be digest (##arithmetic-shift n -32)); might overflow fixnums
  (digest-update-u32-be digest (##bitwise-and n #xffffffff)))

(define (digest-string str
                       algorithm
                       #!optional
                       (result-type 'hex-string))
  (digest-substring str
                    0
                    (##string-length str)
                    algorithm
                    result-type))

(define (digest-substring str
                          start
                          end
                          algorithm
                          #!optional
                          (result-type 'hex-string))
  (let ((port (##open-output-u8vector
               '(init: #u8()
                 char-encoding: UTF-8))))
    (##write-substring str start end port)
    (let ((u8vect (##get-output-u8vector port)))
      (##close-output-port port)
      (digest-u8vector u8vect algorithm result-type))))

(define (digest-u8vector u8vect
                         algorithm
                         #!optional
                         (result-type 'hex-string))
  (digest-subu8vector u8vect
                      0
                      (##u8vector-length u8vect)
                      algorithm
                      result-type))

(define (digest-subu8vector u8vect
                            start
                            end
                            algorithm
                            #!optional
                            (result-type 'hex-string))
  (let ((digest (open-digest algorithm)))
    (digest-update-subu8vector digest u8vect start end)
    (close-digest digest result-type)))

(define (digest-file filename
                     algorithm
                     #!optional
                     (result-type 'hex-string))
  (let* ((digest (open-digest algorithm))
         (port (open-input-file filename))
         (u8vect (##make-u8vector 1024 0)))
    (let loop ()
      (let ((n (##read-subu8vector u8vect 0 (##u8vector-length u8vect) port)))
        (if (##fixnum.> n 0)
            (begin
              (digest-update-subu8vector digest u8vect 0 n)
              (loop))
            (begin
              (##close-input-port port)
              (close-digest digest result-type)))))))

;==============================================================================

; SHA-1 algorithm.

(define (open-sha-1-digest)
  (make-digest
   sha-1-close-digest
   sha-1-hash-update
   (sha-1-hash-block-init)
   (make-hash-block 160)
   0
   0))

(define (sha-1-close-digest digest result-type)
  (let* ((block-pos
          (digest-block-pos digest))
         (bit-pos
          (digest-bit-pos digest))
         (msg-length
          (+ bit-pos (* block-bit-length block-pos)))) ; might overflow fixnums

    (digest-update-subu8vector digest '#u8(#x80) 0 1) ; add 1 bit

    (let ((zero-padding-bytes
           (##fixnum.quotient
            (##fixnum.bitwise-and 511
                                  (##fixnum.- 448 (digest-bit-pos digest)))
            8)))
      (let loop ((n zero-padding-bytes))
        (if (##fixnum.< 0 n)
            (let ((m (##fixnum.min 8 n)))
              (digest-update-subu8vector digest zero-u8vector 0 m) ; add 0 bits
              (loop (##fixnum.- n m))))))

    (digest-update-u64-be digest msg-length) ; add message length (in bits)

    (hash-block->hex-string (digest-hash digest))))






(define (hex n)
  (substring (number->string (+ n (expt 2 32)) 16) 1 9))

(define (u32 n)
  (bitwise-and #xffffffff n))

(define (rol num cnt)
  (u32 (+ (arithmetic-shift num cnt)
          (arithmetic-shift num (- cnt 32)))))

(define (swap x)
  (let ((h (quotient x (expt 2 8)))
        (l (modulo x (expt 2 8))))
    (+ (arithmetic-shift l 8) h)))

(define (split x)
  (list (swap (quotient x (expt 2 16)))
        (swap (modulo x (expt 2 16)))))

(define (merge x)
  (+ (arithmetic-shift (swap (car x)) 16)
     (swap (cadr x))))

(define (vect-ref hb i)
  (let ((h (swap (hash-block-ref hb (+ (* 2 i) 0))))
        (l (swap (hash-block-ref hb (+ (* 2 i) 1)))))
    (+ (arithmetic-shift h 16) l)))
        
(define (vect-set! hb i x)
  (let ((h (swap (quotient x (expt 2 16))))
        (l (swap (modulo x (expt 2 16)))))
    (hash-block-set! hb (+ (* 2 i) 0) h)
    (hash-block-set! hb (+ (* 2 i) 1) l)))
        
(define (->hash-block vect)
  (list->u16vector (apply append (map split (vector->list vect)))))

(define (<-hash-block hb)
  (define (cvt lst)
    (if (null? lst)
        '()
        (cons (merge lst) (cvt (cddr lst)))))
  (list->vector (cvt (u16vector->list hb))))

(define (hash-block->hex hb)
  (apply string-append
         (map hex
              (vector->list (<-hash-block hb)))))

;(trace ->hash-block <-hash-block)

(define (sha-1-hash-block-init)
  (->hash-block
   (vector #x67452301
           #xefcdab89
           #x98badcfe
           #x10325476
           #xc3d2e1f0)))

(define (sha-1-hash-update digest)
  (let* ((hash (digest-hash digest))
         (block (digest-block digest)))
    (let ((OLDA (vect-ref hash 0))
          (OLDB (vect-ref hash 1))
          (OLDC (vect-ref hash 2))
          (OLDD (vect-ref hash 3))
          (OLDE (vect-ref hash 4)))
      (let loop ((j 0)
                 (j2 0)
                 (A OLDA)
                 (B OLDB)
                 (C OLDC)
                 (D OLDD)
                 (E OLDE))
        (if (< j 80)

            (let ((X
                   (if (< j 16)

                       (wlet T1
                             (hash-block-ref block (##fixnum.+ j2 1))
                             (hash-block-ref block (##fixnum.+ j2 0))
                       (wlet T2 (swap T1-L) (swap T1-H)
                             (+ (arithmetic-shift T2-H 16) T2-L)))

                       (wlet T1
                             (hash-block-ref block (##fixnum.- j2 5))
                             (hash-block-ref block (##fixnum.- j2 6))
                       (wlet T2
                             (hash-block-ref block (##fixnum.- j2 15))
                             (hash-block-ref block (##fixnum.- j2 16))
                       (wxor T3 T1 T2
                       (wlet T4
                             (hash-block-ref block (##fixnum.- j2 27))
                             (hash-block-ref block (##fixnum.- j2 28))
                       (wxor T5 T3 T4
                       (wlet T6
                             (hash-block-ref block (##fixnum.- j2 31))
                             (hash-block-ref block (##fixnum.- j2 32))
                       (wxor T7 T5 T6
                       (wlet T8 (swap T7-L) (swap T7-H)
                       (wrot X T8 1
                       (let ((X (+ (arithmetic-shift X-H 16) X-L)))
                         (vect-set! block j X)
                         X)))))))))))))

              (let ((T
                     (u32 (+ (rol A 5)
                             E
                             X
                             (cond ((< j 20)
                                    (+ #x5a827999
                                       (bitwise-ior
                                        (bitwise-and B C)
                                        (bitwise-and (bitwise-not B) D))))
                                   ((< j 40)
                                    (+ #x6ed9eba1
                                       (bitwise-xor B C D)))
                                   ((< j 60)
                                    (+ #x8f1bbcdc
                                       (bitwise-ior
                                        (bitwise-and B C)
                                        (bitwise-and B D)
                                        (bitwise-and C D))))
                                   (else
                                    (+ #xca62c1d6
                                       (bitwise-xor B C D))))))))
                (loop (+ j 1)
                      (+ j2 2)
                      T
                      A
                      (rol B 30)
                      C
                      D)))

            (let ((NEWA (u32 (+ A OLDA)))
                  (NEWB (u32 (+ B OLDB)))
                  (NEWC (u32 (+ C OLDC)))
                  (NEWD (u32 (+ D OLDD)))
                  (NEWE (u32 (+ E OLDE))))
              (vect-set! hash 0 NEWA)
              (vect-set! hash 1 NEWB)
              (vect-set! hash 2 NEWC)
              (vect-set! hash 3 NEWD)
              (vect-set! hash 4 NEWE)))))))

;==============================================================================

; Tests.

#;
(begin

(define sha-1-test-vectors
  '(
    ("" 0 ""
     "da39a3ee5e6b4b0d3255bfef95601890afd80709")
    ; from RFC 3174:
    ("" 0 "abc"
     "a9993e364706816aba3e25717850c26c9cd0d89d")
    ("" 0 "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"
     "84983e441c3bd26ebaae4aa1f95129e5e54670f1")
#;    ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" 10000 ""
     "34aa973cd4c4daa4f61eeb2bdbad27316534016f")
    ("0123456701234567012345670123456701234567012345670123456701234567" 10 ""
     "dea356a2cddd90c7a7ecedc5ebb563934f460452")
    ))

(define (test)

  (define (test algorithm vectors)

    (display "*** ====================== testing ")
    (write algorithm)
    (display " ======================\n")

    (for-each
     (lambda (v)
       (let ((str1 (car v))
             (repeat (cadr v))
             (str2 (caddr v))
             (expect (cadddr v)))
         (let ((md
                (if (= repeat 0)
                    (digest-string str2 algorithm)
                    (let ((u8vect1
                           (list->u8vector
                            (map char->integer (string->list str1))))
                          (u8vect2
                           (list->u8vector
                            (map char->integer (string->list str2))))
                          (digest
                           (open-digest algorithm)))
                      (let loop ((i 0))
                        (if (< i repeat)
                            (begin
                              (digest-update-subu8vector
                               digest
                               u8vect1
                               0
                               (u8vector-length u8vect1))
                              (loop (+ i 1)))
                            (begin
                              (digest-update-subu8vector
                               digest
                               u8vect2
                               0
                               (u8vector-length u8vect2))
                              (close-digest digest))))))))
           (if (equal? md expect)
               (begin
                 (display "*** passed ")
                 (write v)
                 (newline))
               (error (string-append (symbol->string algorithm)
                                     " hashing error")
                      v
                      md)))))
     vectors)

    (display (string-append "*** passed all "
                            (symbol->string algorithm)
                            " tests\n")))

  (test 'sha-1 sha-1-test-vectors)
)

(test))

;==============================================================================

#|
SHA-1 bitwise test vectors (Re: RSA Test)

Jim Gillogly jim at acm.org 
Tue, 23 Feb 1999 09:19:40 -0800
Previous message: Mark Thomas + ECHELON?
Next message: Encrypted sessions
Messages sorted by: [ date ] [ thread ] [ subject ] [ author ]
"Hani Almansour" <Almansour@bigfoot.com> wrote:
> I have implementation for RSA, SHA, MD5 and I want to test it. is there a
> fast way to test the output of any one of these encryption or if there is a
> program that test the output.

For the basic SHA-1 and MD5 you can use the test vectors published in
the specifications to see whether you have the basic idea right.
However, there are a lot of places to go wrong if you're implementing
the full SHA-1, which is defined for arbitrary bit strings.  Francois
Grieu and I have agreed on a number of SHA-1 bit strings and their hashes
to test problem areas where the internal buffers fill and roll over.  This
should shake out most of your bugs.

In the following we use the notation bitstring#n to mean a bitstring
repeated n (in decimal) times, and we use | for concatenation.  Therefore
110#3|1 is 1101101101.

110#148|11  : CE7387AE 577337BE 54EA94F8 2C842E8B E76BC3E1
110#149     : DE244F06 3142CB2F 4C903B7F 7660577F 9E0D8791
110#149|1   : A3D29824 27AE39C8 920CA5F4 99D6C2BD 71EBF03C
110#149|11  : 351AAB58 FF93CF12 AF7D5A58 4CFC8F7D 81023D10

110#170     : 99638692 1E480D4E 2955E727 5DF3522C E8F5AB6E
110#170|1   : BB5F4AD4 8913F51B 157EB985 A5C2034B 8243B01B
110#170|11  : 9E92C554 2237B957 BA2244E8 141FDB66 DEC730A5
110#171     : 2103E454 DA4491F4 E32DD425 A3341DC9 C2A90848

011#490     : B4B18049 DE405027 528CD9E7 4B2EC540 D4E6F06B
011#490|0   : 34C63356 B3087427 20AB9669 14EB0FC9 26E4294B
011#490|01  : 75FACE18 02B9F84F 326368AB 06E73E05 02E9EA34
011#491     : 7C2C3D62 F6AEC28D 94CDF93F 02E739E7 490698A1

Here is a set near 2^32 bits to test the roll-over in the length
field from one to two 32-bit words:

110#1431655764|11 1eef5a18 969255a3 b1793a2a 955c7ec2 8cd221a5
110#1431655765|   7a1045b9 14672afa ce8d90e6 d19b3a6a da3cb879
110#1431655765|1  d5e09777 a94f1ea9 240874c4 8d9fecb6 b634256b
110#1431655765|11 eb256904 3c3014e5 1b2862ae 6eb5fb4e 0b851d99

011#1431655764|01 4CB0C4EF 69143D5B F34FC35F 1D4B19F6 ECCAE0F2
011#1431655765    47D92F91 1FC7BB74 DE00ADFC 4E981A81 05556D52
011#1431655765|0  A3D7438C 589B0B93 2AA91CC2 446F06DF 9ABC73F0
011#1431655765|01 3EEE3E1E 28DEDE2C A444D68D A5675B2F AAAB3203

There are lots of cases where one might go wrong, so if you're
likely to do a partial-byte implementation you might want to
hang onto these test vectors, which were performed with quite
different implementations.

-- 
	Jim Gillogly
	Sterday, 3 Rethe S.R. 1999, 17:11
	12.19.5.17.8, 9 Lamat 1 Kayab, Sixth Lord of Night


Previous message: Mark Thomas + ECHELON?
Next message: Encrypted sessions
Messages sorted by: [ date ] [ thread ] [ subject ] [ author ]
|#
