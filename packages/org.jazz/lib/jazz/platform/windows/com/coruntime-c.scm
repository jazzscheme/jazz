(include "coall-macro.scm")


(codeclare)


;;;
;;;; DLL Functions
;;;


(define LoadLibrary
  (c-lambda (wchar_t-string) HANDLE "LoadLibrary"))
(define FreeLibrary
  (c-lambda (HANDLE) void "FreeLibrary"))


;;;
;;;; COM Functions
;;;


(define %%CoInitializeEx
  (c-lambda (VOID* DWORD) HRESULT "CoInitializeEx"))


(define %%CoCreateInstance
  (c-lambda (GUID VT_UNKNOWN DWORD GUID VT_PTR) HRESULT "CoCreateInstance"))
