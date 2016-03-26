(define (slist-map proc slist)
	(map (lambda (x)
		(cond ((list? x) (slist-map proc x)) (else (proc x)))) slist))

(define (slist-reverse slist)
	(reverse (map (lambda (x)
		(cond ((list? x) (slist-reverse x)) (else x))) slist)))

(define (slist-paren-count slist)
	(cond 
		((null? slist) 2)
		((list? (car slist)) (+ (slist-paren-count (car slist)) (slist-paren-count (cdr slist))))
		(else (slist-paren-count (cdr slist)))))

(define (slist-depth slist)
	(cond
		((symbol? slist) 0)
		((null? slist) 1)
		((list? slist) 
		(+ 1 (apply max (map slist-depth slist))))))

(define (flatten ls)
	(cond
		((null? ls) '())
		((list? (car ls)) (append (flatten (car ls)) (flatten (cdr ls))))
		(else (cons (car ls) (flatten (cdr ls))))))

(define (slist-symbols-at-depth slist d)
	(flatten (cond
		((= d 1) (filter symbol? slist))
		((> d 1) (map (lambda (x) (if (list? x) (slist-symbols-at-depth x (sub1 d)) '())) slist)))))

(define (group-by-two ls)
	(cond
		((null? ls) '())
		((null? (cdr ls)) (list (list (car ls))))
		(else (cons (list (car ls) (cadr ls)) (group-by-two (cddr ls))))))

(define (group-by-n ls n)
	(if (null? ls) '()
	(reverse (fold-right (lambda (next groups)
		(if (< (length (car groups)) n)
			(cons (append (car groups) (list next)) (cdr groups))
			(cons (list next) groups))) '(()) (reverse ls)))))

(define (subst-helper new old slist comp)
	(fold-left (lambda (data next) (let ((done (car data)) (ls (cadr data)))
		(cond
			(done (list #t (append ls (list next))))
			((list? next)
				(let ((result (subst-helper new old next comp)))
					(list (car result) (append ls (list (cadr result))))))
			((comp old next) (list #t (append ls (list new))))
			(else (list #f (append ls (list next))))))) '(#f ()) slist))

(define (subst-leftmost new old slist comp)
	(cadr (subst-helper new old slist comp)))