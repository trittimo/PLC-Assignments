; Problem 1
(define (sn-list-recur base listproc symproc)
	(define (helper ls)
		(cond
			((null? ls) base)
			((list? (car ls)) (listproc (helper (car ls)) (helper (cdr ls))))
			(else (symproc (car ls) (helper (cdr ls))))))
	helper)

; Problem 1a
(define (sn-list-sum snlist)
	((sn-list-recur 0 + +) snlist))

; Problem 1b
(define (sn-list-map proc snlist)
	((sn-list-recur '()
		(lambda (x y) (cons x y))
		(lambda (x y) (cons (proc x) y))) snlist))

; Problem 1c
(define (sn-list-paren-count snlst)
	((sn-list-recur 2
		+
		(lambda (x y) y)) snlst))

; Problem 1d
(define (sn-list-reverse snlst)
	((sn-list-recur '()
		(lambda (x y) (append y (list x)))
		(lambda (x y) (append y (list x)))) snlst))

; Problem 1e
(define (sn-list-occur s snlst)
	((sn-list-recur 0 + (lambda (x y) (if (eq? x s) (+ 1 y) y))) snlst))

; Problem 1f
(define (sn-list-depth snlst)
	((sn-list-recur 1
		(lambda (x y) (max (+ 1 x) y))
		(lambda (x y) y)) snlst))

; Problem 2
(define (bt-recur proc)
	(define (helper ls)
		(cond
			((and (list? (cadr ls)) (list? (caddr ls))) (proc (car ls) (helper (cadr ls)) (helper (caddr ls))))
			((list? (cadr ls)) (proc (car ls) (helper (cadr ls)) (caddr ls)))
			((list? (caddr ls)) (proc (car ls) (cadr ls) (helper (caddr ls))))
			(else (proc (car ls) (cadr ls) (caddr ls)))))
	helper)

; Problem 2a
(define (bt-sum bt)
	(if (number? bt) bt
	((bt-recur (lambda (s l r) (+ l r))) bt)))

; Problem 2b
(define (bt-inorder bt)
	(if (or (symbol? bt) (number? bt)) '()
		((bt-recur (lambda (s l r)
			(cond ((and (number? l) (number? r)) (list s))
				((number? l) (cons s r))
				((number? r) (append l (list s)))
				(else (append l (cons s r)))))) bt)))

; Problem 3 helper
(define compose
	(case-lambda
		(() (lambda (x) x))
		((first . rest)
		(let ((composed-rest (apply compose rest)))
		(lambda (x) (first (composed-rest x)))))))


; Problem 3
(define (make-c...r s)
	(apply compose (map eval (map (lambda (x) (if (eq? #\a x) 'car 'cdr)) (string->list s)))))

; Problem 4 helper
(define (make-stack)
		(let ((stk '()))
			(lambda (msg . args)
				(case msg
					((empty?) (null? stk))
					((push) (set! stk (cons (car args) stk)))
					((pop) (let ((top (car stk)))
						(set! stk (cdr stk)) top))
					(else (errorf 'stack "illegal message to stack object: ~a" msg))))))

; Problem 4
(define (make-slist-leaf-iterator slist)
	(define (makestack)
		(let ((s (make-stack))) (s 'push slist) s))
	(letrec ((stack (makestack)) (iter (lambda ()
		(if (stack 'empty?) #f
			(let ((top (stack 'pop)))
				(cond
					((null? top) (iter))
					((list? top) (begin (stack 'push (cdr top)) (stack 'push (car top)) (iter)))
					(else top))))))) iter))