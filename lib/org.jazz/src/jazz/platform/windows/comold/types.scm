(include "coall-macro.scm")


(codeclare)


(cotype DWORD (stype: unsigned-int32))
(cotype WORD (stype: unsigned-int16))
(cotype BYTE (stype: unsigned-int8 generate-array?: #t))

(cotype INT (stype: int))
;(cotype LONGLONG (stype: long-long))

(cotype USHORT (stype: unsigned-short))
(cotype ULONG (stype: unsigned-long))
;(cotype ULONGLONG (stype: unsigned-long-long))

(cotype HRESULT (stype: unsigned-long))
(cotype HANDLE (ctype: "void"))


(cotype VARTYPE (stype: unsigned-short))
(cotype VARIANT_BOOL (stype: short ctype: "short" generate-pointer?: #t))
(cotype SCODE (stype: long))
(cotype DATE (stype: double))
