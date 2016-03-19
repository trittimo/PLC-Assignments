(define (and-list ls)
	(if (null? ls)
		#t
		(and (car ls) (and-list (cdr ls)))))

(define (set? ls)
	(and (list? ls)(or (null? ls) (and (not (member (car ls) (cdr ls))) (set? (cdr ls))))))

(define (multi-set? ls)
	(define (is-proper-pair? pair)
		(and 
			(list? pair) 
			(or (null? pair) (= (length pair) 2)) 
			(symbol? (car pair)) 
			(number? (cadr pair)) 
			(> (cadr pair) 0)))
	(and (list? ls) (not (member #f (map is-proper-pair? ls))) (set? (map car ls))))

(define (ms-size ms)
	(apply + (map cadr ms)))

(define (matrix-ref m row col)
	(list-ref (list-ref m row) col))

(define (matrix? obj)
	(and 
		(list? obj)
		(or
			(null? obj)
			(and 
				(andmap list? obj)
				(apply = (map length obj))
				(not (member 0 (map length obj)))))))

(define (matrix-transpose m)
	(if (null? (car m))
		'()
		(cons (map car m) (matrix-transpose (map cdr m)))))

(define (last ls)
	; not sure if allowed, but would have done: (list-ref (sub1 (length ls)) ls)
	(if (= (length ls) 1)
		(car ls)
		(last (cdr ls))))

(define (all-but-last ls)
	(if (= (length ls) 1)
		'()
		(cons (car ls) (all-but-last (cdr ls)))))