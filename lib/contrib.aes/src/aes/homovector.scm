;;;============================================================================

;;; File: "homovector.scm", Time-stamp: <2007-04-05 00:51:55 feeley>

;;; Copyright (c) 2006-2007 by Marc Feeley, All Rights Reserved.

;;;============================================================================

;; Provides procedures to operate on homogeneous vectors, including
;; a subset of SRFI 4.


(define-macro (snow-make-u8vector n . fill)
  `(make-u8vector ,n ,@fill))

(define-macro (snow-u8vector . lst)
  `(u8vector ,@lst))

(define-macro (snow-u8vector? obj)
  `(u8vector? ,obj))

(define-macro (snow-u8vector-length u8vect)
  `(u8vector-length ,u8vect))

(define-macro (snow-u8vector-ref u8vect i)
  `(u8vector-ref ,u8vect ,i))

(define-macro (snow-u8vector-set! u8vect i x)
  `(u8vector-set! ,u8vect ,i ,x))

(define-macro (snow-u8vector->list u8vect)
  `(u8vector->list ,u8vect))

(define-macro (snow-list->u8vector lst)
  `(list->u8vector ,lst))

(define-macro (snow-make-u16vector n . fill)
  `(make-u16vector ,n ,@fill))

(define-macro (snow-copy-u16vector u16vect)
  `(u16vector-copy ,u16vect))

(define-macro (snow-u16vector . lst)
  `(u16vector ,@lst))

(define-macro (snow-u16vector? obj)
  `(u16vector? ,obj))

(define-macro (snow-u16vector-length u16vect)
  `(u16vector-length ,u16vect))

(define-macro (snow-u16vector-ref u16vect i)
  `(u16vector-ref ,u16vect ,i))

(define-macro (snow-u16vector-set! u16vect i x)
  `(u16vector-set! ,u16vect ,i ,x))

(define-macro (snow-u16vector->list u16vect)
  `(u16vector->list ,u16vect))

(define-macro (snow-list->u16vector lst)
  `(list->u16vector ,lst))


;;;============================================================================

;; System dependencies.


;;;----------------------------------------------------------------------------

(define (snow-subu8vector-move! src src-start src-end dst dst-start)

  ;; Copy direction must be selected in case src and dst are the same vector.

  (if (< src-start dst-start)
      (let loop1 ((i (- src-end 1))
                  (j (- (+ dst-start (- src-end src-start)) 1)))
        (if (< i src-start)
            dst
            (begin
              (snow-u8vector-set! dst j (snow-u8vector-ref src i))
              (loop1 (- i 1)
                     (- j 1)))))
      (let loop2 ((i src-start)
                  (j dst-start))
        (if (< i src-end)
            (begin
              (snow-u8vector-set! dst j (snow-u8vector-ref src i))
              (loop2 (+ i 1)
                     (+ j 1)))
            dst))))

(define (snow-subu8vector u8vect start end)
  (snow-subu8vector-move!
   u8vect
   start
   end
   (snow-make-u8vector (max (- end start) 0))
   0))

(define (snow-subu16vector-move! src src-start src-end dst dst-start)

  ;; Copy direction must be selected in case src and dst are the same vector.

  (if (< src-start dst-start)
      (let loop1 ((i (- src-end 1))
                  (j (- (+ dst-start (- src-end src-start)) 1)))
        (if (< i src-start)
            dst
            (begin
              (snow-u16vector-set! dst j (snow-u16vector-ref src i))
              (loop1 (- i 1)
                     (- j 1)))))
      (let loop2 ((i src-start)
                  (j dst-start))
        (if (< i src-end)
            (begin
              (snow-u16vector-set! dst j (snow-u16vector-ref src i))
              (loop2 (+ i 1)
                     (+ j 1)))
            dst))))

(define (snow-subu16vector u16vect start end)
  (snow-subu16vector-move!
   u16vect
   start
   end
   (snow-make-u16vector (max (- end start) 0))
   0))

(define (snow-ISO-8859-1-substring->u8vector str start end)
  (let* ((len (- end start))
         (u8vect (snow-make-u8vector len)))
    (let loop ((i 0))
      (if (< i len)
          (begin
            (snow-u8vector-set!
             u8vect
             i
             (char->integer (string-ref str (+ start i))))
            (loop (+ i 1)))
          u8vect))))

(define (snow-ISO-8859-1-string->u8vector str)
  (snow-ISO-8859-1-substring->u8vector
   str
   0
   (string-length str)))

(define (snow-subu8vector->ISO-8859-1-string u8vect start end)
  (let* ((len (- end start))
         (str (make-string len)))
    (let loop ((i 0))
      (if (< i len)
          (begin
            (string-set!
             str
             i
             (integer->char (snow-u8vector-ref u8vect (+ start i))))
            (loop (+ i 1)))
          str))))

(define (snow-u8vector->ISO-8859-1-string u8vect)
  (snow-subu8vector->ISO-8859-1-string
   u8vect
   0
   (snow-u8vector-length u8vect)))

(define (snow-hex-substring->u8vector str start end)

    (define (char->digit c)
      (cond ((and (char>=? c #\0) (char<=? c #\9))
             (- (char->integer c) (char->integer #\0)))
            ((and (char>=? c #\a) (char<=? c #\f))
             (+ 10 (- (char->integer c) (char->integer #\a))))
            ((and (char>=? c #\A) (char<=? c #\F))
             (+ 10 (- (char->integer c) (char->integer #\A))))
            (else
             #f)))

  (let ((n (- end start)))
    (if (odd? n)
        (error "string length must be even")
        (let* ((len (quotient n 2))
               (u8vect (snow-make-u8vector len)))
          (let loop ((i 0) (j (- len 1)))
            (if (>= j 0)
                (let ((hi4 (char->digit (string-ref str i)))
                      (lo4 (char->digit (string-ref str (+ i 1)))))
                  (if (or (not hi4)
                          (not lo4))
                      (error "string must contain hex digits only")
                      (begin
                        (snow-u8vector-set!
                         u8vect
                         j
                         (+ (* 16 hi4) lo4))
                        (loop (+ i 2) (- j 1)))))
                u8vect))))))

(define (snow-hex-string->u8vector str)
  (snow-hex-substring->u8vector
   str
   0
   (string-length str)))

(define (snow-subu8vector->hex-string u8vect start end)

  (define (digit->char d)
    (string-ref "0123456789abcdef" d))

  (let* ((len (- end start))
         (n (* len 2))
         (str (make-string n)))
    (let loop ((i 0) (j (- len 1)))
      (if (>= j 0)
          (let ((x (snow-u8vector-ref u8vect j)))
            (string-set! str i (digit->char (quotient x 16)))
            (string-set! str (+ i 1) (digit->char (modulo x 16)))
            (loop (+ i 2) (- j 1)))
          str))))

(define (snow-u8vector->hex-string u8vect)
  (snow-subu8vector->hex-string
   u8vect
   0
   (snow-u8vector-length u8vect)))

(define (snow-apply-u8vector-append lst)

  (define (append-rest-at i lst)
    (if (pair? lst)
        (let* ((src (car lst))
               (len (snow-u8vector-length src))
               (dst (append-rest-at (+ i len) (cdr lst))))
          (snow-subu8vector-move! src 0 len dst i)
          dst)
        (snow-make-u8vector i)))

  (append-rest-at 0 lst))

(define (snow-u8vector-append . lst)
  (snow-apply-u8vector-append lst))

;;;============================================================================
