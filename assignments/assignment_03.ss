;;; A few functions from assignment 2 to make our life easier
(define make-vec-from-points
	(lambda (p1 p2)
		(list (- (car p2) (car p1)) (- (cadr p2) (cadr p1)) (- (caddr p2) (caddr p1)))))

(define vec-length
	(lambda (vec)
		(sqrt (+ (* (car vec) (car vec)) (* (cadr vec) (cadr vec)) (* (caddr vec) (caddr vec))))))

(define distance
	(lambda (p1 p2)
		(vec-length (make-vec-from-points p1 p2))))

(define set?
	(lambda (ls)
		(if (not (list? ls)) #f
			(if (null? ls) #t
				(if (member (car ls) (cdr ls))
					#f
					(set? (cdr ls)))))))

(define remove-duplicates
	(lambda (ls)
		(if (null? ls) '()
			(if (member (car ls) (cdr ls))
				(remove-duplicates (cdr ls))
				(cons (car ls) (remove-duplicates (cdr ls)))))))


;;; Assignment 3 start
(define nearest-point
	(lambda (p ls)
		(letrec ([find-point (lambda (ls nearest)
			(if (null? ls) nearest
				(if (< (distance p (car ls)) (distance p nearest))
					(find-point (cdr ls) (car ls))
					(find-point (cdr ls) nearest))))])
		(find-point (cdr ls) (car ls)))))

(define union
	(lambda (s1 s2)
		(if (null? s1) s2
			(if (member (car s1) s2)
				(union (cdr s1) s2)
				(union (cdr s1) (cons (car s1) s2))))))

(define intersection
	(lambda (s1 s2)
		(if (null? s1) '()
			(if (member (car s1) s2)
				(append (list (car s1)) (intersection (cdr s1) s2))
				(intersection (cdr s1) s2)))))

(define subset?
	(lambda (s1 s2)
		(if (null? s1) #t
			(if (member (car s1) s2)
				(subset? (cdr s1) s2)
				#f))))

(define relation?
	(lambda (obj)
		(if (not (set? obj)) #f
			(if (null? obj) #t
				(if (not (list? (car obj))) #f
					(if (= 2 (length (car obj)))
						(relation? (cdr obj))
						#f))))))

(define domain
	(lambda (r)
		(remove-duplicates (map car r))))


; for each in domain
; is (x x) in r?
; for each in range
; is (y y) in r?

(define reflexive?
	(lambda (r)
		(letrec ([domain (map car r)] [range (map cadr r)]
			  [check (lambda (ls) 
			  	(if (null? ls) #t
			  		(and (member (list (car ls) (car ls)) r) (check (cdr ls)))))])
		(and (check domain) (check range)))))

(define hailstone-step-count
	(lambda (n)
		(if (= n 1) 0
			(+ 1 (hailstone-step-count
				(if (even? n) (/ n 2) (+ 1 (* 3 n))))))))