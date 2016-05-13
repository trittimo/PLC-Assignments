(define-datatype proc-val proc-val?
	(prim-proc (name symbol?))
	(closure (params (list-of symbol?)) (varargs ...) (bodies (list-of expression?)) (env environment?))
	(c-proc (k continuation?)))

(define (apply-prim-proc prim-proc-name vals k)
	(case prim-proc-name
		((call/cc)
			(apply-proc (car vals) (list (c-proc k)) k))))

