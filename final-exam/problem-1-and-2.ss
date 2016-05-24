(define (determinant m)
	(define (mlist n a)
		(if (= n 0)
			'()
			(if a
				(cons 0 (mlist (- n 1) a))
				(append (mlist (- n 1) a) (list (- n 1))))))
	(define (remove-at-index ls i)
		(if (= i 0)
			(if (null? ls) '() (cdr ls))
			(cons (car ls) (remove-at-index (cdr ls) (- i 1)))))
	(define (minor m i j)
		(map (lambda (x) (remove-at-index x j)) (remove-at-index m i)))
	(let loop ((m m))
		(cond
			((= (length m) 1) (caar m))
			((= (length m) 2) (- (* (caar m) (cadadr m)) (* (cadar m) (caadr m))))
			(else
				(apply +
					(map (lambda (i j) (* (expt -1 (+ i j)) (list-ref (list-ref m i) j) (loop (minor m i j)))) (mlist (length m) #t) (mlist (length m) #f)))))))

(define (apply-k k . a) (apply k a))

(define (tree-mult-cps ls k)
	(cond
		((null? ls) (apply-k k 1))
		((not (list? (car ls)))
			(tree-mult-cps (cdr ls) (lambda (x) (apply-k k (* (car ls) x)))))
		(else (tree-mult-cps (car ls) (lambda (x) (tree-mult-cps (cdr ls) (lambda (y) (apply-k k (* x y)))))))))

(define (tree-mult ls)
	(tree-mult-cps ls (lambda (x) x)))