(include "coall-macro.scm")


(codeclare)


(s-pass-define DAO-DBEngine-class
  "00000100-0000-0010-8000-00aa006d2ea4")
(s-pass-define DAO-DBEngine-interface
  "00000021-0000-0010-8000-00aa006d2ea4")


; _DBEngine
(com-external 15 VT_HRESULT (get-Workspaces (out VT_PTR VT_UNKNOWN)))
;(get-Workspaces coptr)


; Workspaces
(com-external 12 VT_HRESULT (get-Item (in VT_VARIANT) (out VT_PTR VT_UNKNOWN)))
;(get-Item coptr n)


; Workspace
(com-external 22 VT_HRESULT (OpenDatabase (in VT_BSTR) (in VT_VARIANT) (in VT_VARIANT) (in VT_VARIANT) (out VT_PTR VT_UNKNOWN)))
;(OpenDatabase coptr name options read-only connect-string)


; Database
(com-external 22 VT_HRESULT (Close))
;(Close coptr)
