(include "coall-macro.scm")


;;;
;;;; debug Functions
;;;


(c-pass-define IsBadReadPtr
 (c-lambda (unsigned-long) bool "___result = IsBadReadPtr((void*)___arg1,4);"))
(c-pass-define ReadPtr
 (c-lambda (unsigned-long) unsigned-long "___result = *(unsigned long*) ___arg1;"))


;;;
;;;; debug Functions
;;;


(s-pass-define ? #f)
(s-pass-define (peek address)
 (if (IsBadReadPtr address)
     (error "Illegal read address" address)
   (set! ? (ReadPtr address)))
 ?)
(s-pass-define (peek-n address n)
 (set! ? (map (lambda (n)
                (if (IsBadReadPtr address)
                    #f
                  (ReadPtr address)))
              (naturals 0 n)))
 ?)
