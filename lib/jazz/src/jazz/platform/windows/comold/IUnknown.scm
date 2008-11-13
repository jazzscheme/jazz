(include "coall-macro.scm")


(codeclare)


;;;
;;;; IUnknown
;;;


(com-external 0 VT_HRESULT (CoQuery (in GUID) (out VT_PTR VT_UNKNOWN)))
(com-external 1 ULONG (CoAddRef))
(com-external 2 ULONG (CoRelease))
