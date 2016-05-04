(define (apply-continuation k . v) (apply k v))

(define (subst-left-cps-2args new old slist e? k)
	(letrec
		((helper (lambda (slist k)
			(cond 
				((null? slist) (apply-continuation k #f slist))
				((not (pair? slist))
					(if (e? slist old)
						(apply-continuation k #t new)
						(apply-continuation k #f slist)))
				(else
					(helper (car slist)
						(lambda (x ls)
							(if x
								(apply-continuation k #t (cons ls (cdr slist)))
								(helper (cdr slist)
									(lambda (y ls)
										(apply-continuation k y (cons (car slist) ls))))))))))))
		(helper slist k)))