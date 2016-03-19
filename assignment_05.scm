(define (sort-intervals ls)
	(sort (lambda (a b) (<= (car a) (car b))) ls))

(define (combine a b)
	(list (apply min (append a b)) (apply max (append a b))))

(define (intersects? a b)
	(or 
		(<= (car b) (cadr a) (cadr b))
		(<= (car a) (cadr b) (cadr a))))

(define (collapse ls)
	(cond
		([= (length ls) 0]
			'())
		([= (length ls) 1]
			ls)
		(else
			(if (intersects? (car ls) (cadr ls))
				(collapse (cons (combine (car ls) (cadr ls)) (cddr ls)))
				(cons (car ls) (collapse (cdr ls)))))))

(define (minimize-interval-list ls)
	(collapse (sort-intervals ls)))

(define (exists? pred ls)
	(if (null? ls)
		#f
		(or (pred (car ls)) (exists? pred (cdr ls)))))

(define (list-index pred ls)
	(define (list-index-helper ls accum)
		(if (null? ls)
			#f
			(if (pred (car ls))
				accum
				(list-index-helper (cdr ls) (add1 accum)))))
	(list-index-helper ls 0))

(define (fact n)
		(if (> n 1)
		    (* n (fact (- n 1)))
		    1))

(define (choose n k)
		(/ (fact n) (* (fact k) (fact (- n k)))))

(define (pascal-row n ls)
	(if (= (length ls) n)
		(cons 1 ls)
		(pascal-row n (cons (choose n (length ls)) ls))))

(define (pascal-triangle n)
	(cond
		((< n 0) '())
		((= n 0) '((1)))
		(else
			(cons (pascal-row n '()) (pascal-triangle (sub1 n))))))

(define (product set1 set2)
	(if (null? set1)
		'()
		(append (map (lambda(x) (list (car set1) x)) set2) (product (cdr set1) set2))))

(define (max-edges n)
	(/ (* n (sub1 n)) 2))

(define (valid-node? n g)
	(and 
		(not (member (car n) (cadr n)))
		(andmap (lambda (x) (member (car n) (cadr x))) g)))

(define (complete? g)
	(or (null? g) (not (member #f (map (lambda (x) (valid-node? x (remove x g))) g)))))

(define (complete ls)
	(map (lambda (x) (list x (remove x ls))) ls))

(define (replace old new ls)
	(cond
		((null? ls) '())
		((equal? (car ls) old) (cons new (replace old new (cdr ls))))
		(else (cons (car ls) (replace old new (cdr ls))))))

(define (remove-first element ls)
	(if (null? ls)
		'()
		(if (equal? element (car ls)) (cdr ls)
		(cons (car ls) (remove-first element (cdr ls))))))

(define (remove-last element ls)
	(reverse (remove-first element (reverse ls))))