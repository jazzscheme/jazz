(define (naturals x y)
  (cond ((> x y) (error "naturals" x y))
        ((= x y) '())
        (else (cons x (naturals (+ 1 x) y)))))


(define (zero-fill str n)
  (let ((len (string-length str)))
    (if (> n len)
        (string-append (make-string (- n len) #\0) str)
      str)))


(define last-hresult 0)
(define (validate-hresult hresult)
  (case hresult
    ((0)
     #t)
    ((#x80004002)
     (error "ERROR - E_NOINTERFACE" hresult))
    ((#x80040154)
     (error "ERROR - class not registered?" hresult))
    ((#x800401f0)
     (error "ERROR - CO_E_NOTINITIALIZED" hresult))
    ((#x80070057)
     (error "ERROR - E_INVALIDARG" hresult))
    ((#x800a0d5d)
     (error "ERROR - wrong value type" hresult))
    (else
     (error (string-append "ERROR - " (number->string hresult 16))))))


;;;
;;;; COM Functions
;;;


(define COINIT_APARTMENTTHREADED 2)
(define CLSCTX_SERVER (+ 1 4 16))


(define (CoInitializeEx n/a flags)
  (validate-hresult (%%CoInitializeEx #f flags)))


(define (CoCreateInstance class interface)
  (let ((class-guid (GUID-encode class))
        (interface-guid (GUID-encode interface))
        (coptr* (VT_UNKNOWN-enref #f)))
    (let ((res (%%CoCreateInstance class-guid #f CLSCTX_SERVER interface-guid coptr*)))
      (GUID-free class-guid)
      (GUID-free interface-guid)
      (validate-hresult res)
      (VT_UNKNOWN-deref coptr*))))
