(define-macro (costruct type options . members)
  (costruct-macro type options members))


;;;
;;;; allowed options
;;;
;; see cotype
;; All structs default to (foreign?: #t)

;; SIMPLE members
;; (TYPE-SUBNAME-get struct) to extract the content
;; (TYPE-SUBNAME-set! struct data) to set the content
;;
;; ARRAY members
;; (TYPE-SUBNAME-ref struct) to get the array
;; TYPE-SUBNAME-size contains the length of the array
