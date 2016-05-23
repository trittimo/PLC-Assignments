(define (apply-k k . args) (apply k args))

(define (map-cps proc ls k)
	(if (null? ls)
		(apply-k k '())
		(map-cps (lambda (x)
			(