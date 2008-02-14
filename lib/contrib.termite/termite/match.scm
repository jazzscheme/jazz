(define-macro (match datum . clauses)
			  (let ((tmp (gensym))
					(fail (gensym)))

				`(let* ((,tmp ,datum)
						(,fail (lambda () 
								 (raise 
								   (list bad-match: ,tmp)))))
				   ,(compile-pattern-match 
					  #f 
					  `(,fail)
					  clauses
					  tmp))))
