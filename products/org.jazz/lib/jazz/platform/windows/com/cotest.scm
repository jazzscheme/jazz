(define zz0? #f)
(define (z0)
  (if zz0?
      "already inited"
    (begin
      (LoadLibrary "ole32")
      (CoInitializeEx #f COINIT_APARTMENTTHREADED)
      (set! zz0? #t))))


(define zz1 #f)
(define (z1)
  (if zz0?
      (set! zz1 (CoCreateInstance DAO-DBEngine-class DAO-DBEngine-interface))
    (error "zz0?" zz0?))
  zz1)


(define zz2 #f)
(define (z2)
  (if zz1
      (set! zz2 (get-Workspaces zz1))
    (error "zz1" zz1))
  zz2)


(define zz3 #f)
(define (z3)
  (if zz2
      (set! zz3 (get-Item (car zz2) 0))
    (error "zz2" zz2))
  zz3)


(define zz4 #f)
(define (z4)
  (if zz3
      (set! zz4 (OpenDatabase (car zz3) "c:\\dev\\db.mdb" 0 #f ""))
    (error "zz3" zz3))
  zz4)


(define (z9)
  (if zz4 (Close (car zz4)))
  (if zz4 (begin (CoRelease (car zz4)) (set! zz4 #f)))
  (if zz3 (begin (CoRelease (car zz3)) (set! zz3 #f)))
  (if zz2 (begin (CoRelease (car zz2)) (set! zz2 #f)))
  (if zz1 (begin (CoRelease zz1) (set! zz1 #f))))


;;;
;;;; calls
;;;

(define (get-Workspaces coptr) ; _DBEngine
  (let ((p1 (VT_UNKNOWN-enref #f)))
    (let ((res (%%get-Workspaces coptr p1)))
      (validate-hresult res)
      (list (VT_UNKNOWN-deref p1)))))


(define (get-Item coptr in1) ; Workspaces
  (let ((p1 (VT_VARIANT-encode in1))
        (p2 (VT_UNKNOWN-enref #f)))
    (let ((res (%%get-Item coptr p1 p2)))
      (validate-hresult res)
      (VT_VARIANT-free p1)
      (list (VT_UNKNOWN-deref p2)))))


(define (OpenDatabase coptr in1 in2 in3 in4) ; Workspace
  (let ((p1 (VT_BSTR-encode in1))
        (p2 (VT_VARIANT-encode in2))
        (p3 (VT_VARIANT-encode in3))
        (p4 (VT_VARIANT-encode in4))
        (p5 (VT_UNKNOWN-enref #f)))
    (let ((res (%%OpenDatabase coptr p1 p2 p3 p4 p5)))
      (validate-hresult res)
      (VT_BSTR-free p1)
      (VT_VARIANT-free p2)
      (VT_VARIANT-free p3)
      (VT_VARIANT-free p4)
      (list (VT_UNKNOWN-deref p5)))))


(define (Close coptr) ; Database
  (let ((res (%%Close coptr)))
    (validate-hresult res) (list)))
