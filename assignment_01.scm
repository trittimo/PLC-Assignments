(define Fahrenheit->Celsius
	(lambda (t)
    	(* (- t 32) (/ 5 9))))

(define interval-contains?
	(lambda (interval n)
		(and (>= n (car interval)) (<= n (cadr interval)))))

(define interval-intersects?
	(lambda (a b)
		(or 
			;b[first] <= a[last] <= b[last]
			(<= (car b) (cadr a) (cadr b))
			;a[first] <= b[last] <= a[last]
			(<= (car a) (cadr b) (cadr a)))))

(define interval-union
	(lambda (a b)
		(if (interval-intersects? a b)
		    (list (list (min (car a) (car b)) (max (cadr a) (cadr b))))
		    (list a b))))

(define divisible-by-7?
	(lambda (n)
		(eq? (modulo n 7) 0)))

(define ends-with-7?
	(lambda (n)
		(eq? (modulo (- n 7) 10) 0)))

(define 1st car)
(define 2nd cadr)
(define 3rd caddr)