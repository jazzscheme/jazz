(define (symbol-append . rest)
  (string->symbol (apply string-append (map symbol->string rest))))


(define (keyword-value keyword list default)
  (let ((found (memq keyword list)))
    (if found (cadr found) default)))
