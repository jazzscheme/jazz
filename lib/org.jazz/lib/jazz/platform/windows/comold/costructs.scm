(include "coall-macro.scm")


(codeclare)


;;;
;;;; GUID
;;;


(costruct GUID ()
 (DWORD Data1)
 (WORD Data2)
 (WORD Data3)
 (BYTE Data4 8))


(s-pass-define (GUID-set! guid str)
  (GUID-Data1-set! guid (string->number (substring str 0 8) 16))
  (GUID-Data2-set! guid (string->number (substring str 9 13) 16))
  (GUID-Data3-set! guid (string->number (substring str 14 18) 16))
  (let ((data4+ (GUID-Data4-ref guid))
        (string->byte (lambda (offset)
                        (string->number (substring str offset (+ 2 offset)) 16))))
    (for-each (lambda (n byte) (BYTE+-set! data4+ n byte))
              (naturals 0 8)
              (map string->byte '(19 21 24 26 28 30 32 34)))))


(s-pass-define (GUID-get guid)
  (let* ((proc (lambda (i n)
                 (zero-fill (number->string i 16) n)))
         (data4+ (GUID-Data4-ref guid))
         (proc4 (lambda (n) (proc (BYTE+-get data4+ n) 2))))
    (apply string-append
           (proc (GUID-Data1-get guid) 8) "-"
           (proc (GUID-Data2-get guid) 4) "-"
           (proc (GUID-Data3-get guid) 4) "-"
           (proc4 0) (proc4 1) "-"
           (map proc4 (naturals 2 8)))))


;;;
;;;; VT_CY
;;;


(costruct VT_CY (ctype: "CY")
; (long-long int64)
 (unsigned-long Lo)
 (long Hi))


(s-pass-define (VT_CY-set! cy data)
  #f)


(s-pass-define (VT_CY-get cy)
  0)


;;;
;;;; VT_DECIMAL
;;;

 
(costruct VT_DECIMAL (ctype: "DECIMAL")
 (ULONG Hi32)
 (BYTE scale)
 (BYTE sign)
 (USHORT signscale)
 (ULONG Hi32)
; (unsigned-long-long Lo64)
 (ULONG Lo32)
 (ULONG Mid32))


(s-pass-define (VT_DECIMAL-set! decimal data)
  #f)


(s-pass-define (VT_DECIMAL-get decimal)
  0)


;;;
;;;; VT_VARIANT
;;;

 
(costruct VT_VARIANT (ctype: "VARIANT" generate-foreign?: custom pass-by: value)
 (VARTYPE vt)
 (long lVal)               ; rw VT_I4
 (short iVal)              ; r  VT_I2
 (float fltVal)            ; r  VT_R4
 (double dblVal)           ; rw VT_R8
 (VARIANT_BOOL boolVal)    ; rw VT_BOOL
 (SCODE scode)             ;  w VT_ERROR
; (CY cyVal)                ; r  VT_CY
 (DATE date)               ; r  VT_DATE
 (VT_BSTR bstrVal)            ; rw VT_BSTR
; (IUnknown* punkVal)       ;  w VT_UNKNOWN
 (VARIANT_BOOL* pboolVal)  ; r  VT_BYREF + VT_BOOL
 (INT intVal)              ; r  VT_INT
; (DECIMAL decVal)          ; r  VT_DECIMAL
)


(c-pass-define VariantInit
 (c-lambda (VT_VARIANT) void "VariantInit"))


(c-pass-define VariantClear
 (c-lambda (VT_VARIANT) HRESULT "VariantClear"))


(s-pass-define VT_VARIANT-alloc
 (lambda ()
   (let ((variant (VT_VARIANT-cast (UNTYPED*-alloc VT_VARIANT-sizeof))))
     (VariantInit variant)
     variant)))


(s-pass-define VT_VARIANT-free
 (lambda (variant)
   (validate-hresult (VariantClear variant))
   (UNTYPED*-free variant)))


(s-pass-define (VT_VARIANT-set! variant value)
  (let ((VT_I4 3)
        (VT_R8 5)
        (VT_BSTR 8)
        (VT_ERROR 10)
        (VT_BOOL 11)
        (VT_UNKNOWN 13)
        (DISP_E_PARAMNOTFOUND -1))
    (cond
     ((null? value)
      (VT_VARIANT-vt-set! variant VT_ERROR)
      (VT_VARIANT-scode-set! variant DISP_E_PARAMNOTFOUND))
     ((boolean? value)
      (VT_VARIANT-vt-set! variant VT_BOOL)
      (VT_VARIANT-boolVal-set! variant (if value -1 0)))
     ((integer? value)
      (VT_VARIANT-vt-set! variant VT_I4)
      (VT_VARIANT-lVal-set! variant value))
     ((real? value)
      (VT_VARIANT-vt-set! variant VT_R8)
      (VT_VARIANT-dblVal-set! variant value))
;     ((string? value)
;      (VT_VARIANT-vt-set! variant VT_BSTR)
;      (VT_VARIANT-bstrVal-set! variant (VT_BSTR-encode value)))
;     (else
;      (VT_VARIANT-vt-set! variant VT_UNKNOWN)
;      (VT_VARIANT-punkVal-set! variant value))
     )))


(s-pass-define (VT_VARIANT-get variant)
  (let ((VT_EMPTY 0)
        (VT_NULL 1) 
        (VT_I2 2)
        (VT_I4 3)
        (VT_R4 4)
        (VT_R8 5)
        (VT_CY 6)
        (VT_DATE 7)
        (VT_BSTR 8)
        (VT_BOOL 11)
        (VT_DECIMAL 14)
        (VT_INT 22)
        (VT_BYREF #x4000))
    (case (VT_VARIANT-vt-get variant)
      ((VT_EMPTY)
       #f)
      ((VT_NULL)
       '())
      ((VT_I2)
       (VT_VARIANT-iVal-get variant))
      ((VT_I4)
       (VT_VARIANT-lVal-get variant))
      ((VT_R4)
       (VT_VARIANT-fltVal-get variant))
      ((VT_R8)
       (VT_VARIANT-dblVal-get variant))
;    ((VT_CY)
;     (VT_VARIANT-cyVal-get variant))
      ((VT_DATE)
       (VT_VARIANT-date-get variant))
;    ((VT_BSTR)
;     (VT_VARIANT-bstrVal-get variant))
      ((VT_BOOL)
       (not (eqv? 0 (VT_VARIANT-boolVal-get variant))))
;    ((VT_DECIMAL)
;     (VT_VARIANT-decVal-get variant))
      ((VT_INT)
       (VT_VARIANT-intVal-get variant))
      (((+ VT_BOOL VT_BYREF))
       (not (eqv? 0 (VARIANT_BOOL*-get (VT_VARIANT-pboolVal-get variant)))))
      (else
       (error "unknown variant")))))


(s-pass-define (VT_VARIANT-encode data)
  (let ((variant (VT_VARIANT-alloc)))
    (VT_VARIANT-set! variant data)
    variant))


(s-pass-define (VT_VARIANT-decode variant)
  (let ((data (VT_VARIANT-get variant)))
    (VT_VARIANT-free foreign) data))
