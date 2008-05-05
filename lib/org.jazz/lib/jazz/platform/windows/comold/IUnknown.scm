(include "coall-macro.scm")


(codeclare)


;;;
;;;; IUnknown
;;;


(coexternal 0 VT_HRESULT (CoQuery (in GUID) (out VT_PTR VT_UNKNOWN)))
(coexternal 1 ULONG (CoAddRef))
(coexternal 2 ULONG (CoRelease))
