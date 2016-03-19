(define fact
	(lambda (n)
		(if (> n 1)
		    (* n (fact (- n 1)))
		    1)))

(define choose
	(lambda (n k)
		(/ (fact n) (* (fact k) (fact (- n k))))))

(define range
	(lambda (a b)
		(if (>= a b)
			'()
			(cons a (range (add1 a) b)))))

(define set?
	(lambda (ls)
		(if (null? ls) #t
			(if (member (car ls) (cdr ls))
				#f
				(set? (cdr ls))))))

(define sum-of-squares
	(lambda (ls)
		(if (null? ls) 0
			(+ (* (car ls) (car ls)) (sum-of-squares (cdr ls))))))

(define make-vec-from-points
	(lambda (p1 p2)
		(list (- (car p2) (car p1)) (- (cadr p2) (cadr p1)) (- (caddr p2) (caddr p1)))))

(define dot-product
	(lambda (v1 v2)
		(+ (* (car v1) (car v2)) (* (cadr v1) (cadr v2)) (* (caddr v1) (caddr v2)))))

(define vec-length
	(lambda (vec)
		(sqrt (+ (* (car vec) (car vec)) (* (cadr vec) (cadr vec)) (* (caddr vec) (caddr vec))))))

(define distance
	(lambda (p1 p2)
		(vec-length (make-vec-from-points p1 p2))))

(define cross-product
	(lambda (v1 v2)
		(let ([a (car v1)] [b (cadr v1)] [c (caddr v1)]
			  [d (car v2)] [e (cadr v2)] [f (caddr v2)])
		(list (- (* b f) (* e c)) (- (* c d) (* a f)) (- (* a e) (* b d))))))

(define parallel?
	(lambda (v1 v2)
		(equal? (cross-product v1 v2) '(0 0 0))))

(define collinear?
	(lambda (p1 p2 p3)
		(let ([a (make-vec-from-points p1 p2)] [b (make-vec-from-points p1 p3)])
			(parallel? a b))))