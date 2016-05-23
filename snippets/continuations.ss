(define (apply-k k . args) (apply k args))

(define (map-cps proc ls k)
	(if (null? ls)
		(apply-k k '())
		(proc
			(car ls)
			(lambda (x)
				(map-cps
					proc
					(cdr ls)
					(lambda (y) (apply-k k (cons x y))))))))