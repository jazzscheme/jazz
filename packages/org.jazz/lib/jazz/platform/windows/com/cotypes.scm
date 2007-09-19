(include "coall-macro.scm")


(codeclare)


(cotype VT_HRESULT (stype: unsigned-long ctype: "HRESULT"))
(cotype VT_PTR (ctype: "void" stype*: VT_PTR ctype*: "void*" generate-pointer?: #t))
(cotype VT_I2 (stype: short ctype: "short" stype*: VT_PTR ctype*: "void*" generate-pointer?: #t))
(cotype VT_UNKNOWN (ctype: "IUnknown" stype*: VT_PTR ctype*: "void*" generate-pointer?: #t generate-foreign?: #t))


;;;
;;;; VT_BSTR
;;;


(cotype VT_BSTR (ctype: "wchar_t" stype*: VT_PTR ctype*: "void*" generate-foreign?: custom))


;; VT_BSTR-alloc undefined
;; VT_BSTR-set! undefined

(c-pass-define VT_BSTR-free
  (let ((SysFreeString (c-lambda (VT_BSTR) void "SysFreeString")))
    (lambda (bstr)
      (SysFreeString bstr))))


(c-pass-define VT_BSTR-get
  (let ((cast (c-lambda (VT_BSTR) wchar_t-string "___result = (wchar_t*)___arg1;")))
    (lambda (bstr)
      (cast bstr))))


(c-pass-define VT_BSTR-encode
  (let ((SysAllocString (c-lambda (wchar_t-string) VT_BSTR "SysAllocString")))
    (lambda (str)
      (SysAllocString str))))


(s-pass-define VT_BSTR-decode
  (lambda (bstr)
    (let ((str (VT_BSTR-get bstr)))
      (VT_BSTR-free bstr)
      str)))
