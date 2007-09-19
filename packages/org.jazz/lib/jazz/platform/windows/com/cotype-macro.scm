(jazz.define-macro (cotype type options)
  (cotype-macro type options))


(jazz.define-macro (codeclare)
  (codeclare-macro))


;; type              used for TYPE-xxx definitions
;; type*             used for TYPE*-xxx definitions
;; type+             used for TYPE+-xxx definitions
;;;
;;;; allowed options
;;;
;; stype             default to TYPE, must be set for native
;; stype*            default to <stype>*, used for c-lambda parameters
;; stype+            default to <stype>+, used for c-lambda parameters
;; ctype             default to "TYPE" ("TYPE*" for foreign)
;; ctype*            default to "<type>*"
;; ctype+            default to "<ctype>*"
;; generate-foreign? default to #f (no FOREIGN definitions)
;; generate-pointer? default to #f (no POINTER definitions)
;; generate-array?   default to #f (no ARRAY definitions)
;; untyped?          default to #f, used for UNTYPED type

;; NATIVE vs FOREIGN types
;;
;; NATIVE
;; (c-define-type WORD unsigned-int16)
;; Native types are directly mapped to Gambit types.
;;
;; FOREIGN
;; (c-define-type VT_BSTR (pointer "wchar_t"))
;; Foreign types are wrapped in pointers for Gambit.
;; All structs and unions are foreign types.
;; Macro will provide (TYPE-alloc TYPE-free TYPE-encode TYPE-decode)
;; (TYPE-alloc) is called to create the foreign.
;; (TYPE-free foreign) must be called to free the allocated memory.
;;
;; FOREIGN type custom conversion
;; If we provide (TYPE-set! foreign native), we can use (TYPE-encode native)
;; to create a foreign.
;; If we provide (TYPE-get foreign), we can use (TYPE-decode foreign)
;; to extract its value.


;; POINTER and ARRAY lambdas
;; TYPE-sizeof contains the sizeof of the C type
;;
;; POINTER
;; (TYPE*-alloc) is called to create a C pointer
;; (TYPE*-free ptr) must be called to free the allocated memory.
;; (TYPE*-get ptr) to extract the content
;; (TYPE*-set! ptr data) to set the content
;; (TYPE-enref data) to wrap in a pointer (alloc + set!)
;; (TYPE-deref ptr) to unwrap from the pointer (get + free)
;;
;; ARRAY
;; (TYPE+-alloc n) is called to create a C pointer
;; (TYPE+-free array) must be called to free the allocated memory.
;; (TYPE+-get array n) to extract the content (defined to (TYPE-get (TYPE+-ref)) for foreign)
;; (TYPE+-set! array n data) to set the content (defined to (TYPE-set! (TYPE+-ref)) for foreign)
;; (TYPE+-ref array n) to get foreign types
