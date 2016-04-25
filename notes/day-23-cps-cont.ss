
; Keeps the code in its original form, but messier
(define (intersection-cps los1 los2 k)
	(if (null? los1)
		(apply-continuation k '())
		(memq-cps (car los1) los2
			(lambda (car-in-los2?)
				(if car-in-los2?
					(intersection-cps 
						(cdr los1) 
						los2 
						(lambda (intersection-with-cdr) (apply-continuation k (cons (car los1) intersection-with-cdr))))
					(intersection-cps (cdr los1) los2 k))))))

; This way is 'better' (LOL)
(define (intersection-cps los1 los2 k)
	(if (null? los1)
		(apply-continuation k '())
		(intersection-cps 
			(cdr los1) 
			los2 
			(lambda (intersection-with-cdr)
				(memq-cps 
					(car los1)
					(lambda (is-in?)
						(apply-continuation k
							(if is-in? 
								(apply-continuation k (cons (car los1) los2))
								intersection-with-cdr))))))))
(define 1st cadr)
(define 2nd caddr)
(define 3rd cadddr)

(define (free-vars-cps exp)
	(cond
		((symbol? exp) (apply-continuation k (list exp)))
		((eq? (lst exp) 'lambda)
			(free-vars-cps (3rd exp) (lambda (free-vars-of-body) (remove-cps (car (2nd exp)) free-vars-of-body k))))

		(else (free-vars-cps (1st exp) 
			(lambda (free-vars-from-1st) 
				(free-vars-cps (2nd exp) 
					(lambda (free-vars-from-2nd) (union-cps free-vars-from-2nd free-vars-from-1st k))))))))


(define (apply-continuation k v) (k v))

(define (memq-cps sym ls k)
	(cond 
		((null? ls) (apply-continuation k #f))
		((eq? sym (car ls)) (apply-continuation k #t))
		(else (memq-cps sym (cdr ls) k))))