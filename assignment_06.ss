(define (curry2 procedure)
	(lambda (a)
		(lambda (b)
			(procedure a b))))

(define (compose a b . c)
	(if (null? c)
		(lambda (x) (a (b x)))
		(compose a (compose b (car c)))))

(define curried-compose (curry2 compose))

(define (make-list n x)
	(if (= n 0)
		'()
		(cons x (make-list (sub1 n) x))))

(define make-list-c (curry2 make-list))

(define (let->application expr)
	(let ([args (cadr expr)] [body (caddr expr)])
		(append (list (list 'lambda (map car args) body)) (map cadr args))))

(define (let*->let-nolet expr args)
	(if (null? (cdr expr))
		(append (list 'let (list (car expr))) args)
		(append (list 'let (list (car expr))) (list (let*->let-nolet (cdr expr) args)))))

(define (let*->let expr)
	(let*->let-nolet (cadr expr) (cddr expr)))

(define (filter-in pred? ls)
	(if (null? ls)
		'()
		(if (pred? (car ls))
			(cons (car ls) (filter-in pred? (cdr ls)))
			(filter-in pred? (cdr ls)))))

(define (filter-out pred? ls)
	(filter-in (lambda (x) (not (pred? x))) ls))

(define (sort-list-of-symbols los)
	(map string->symbol (list-sort string<? (map symbol->string los))))

(define (invert ls)
	(map (lambda (x) (list (cadr x) (car x))) ls))

(define (list-index pr ls a)
		(if (null? ls)
			#f
			(if (pr (car ls))
				a
				(list-index pr (cdr ls) (add1 a)))))

(define (vector-index pr v)
	(list-index pr (vector->list v) 0))