; Problem 1a
(define (bound-vars e)
	(define (check-exp e varlist)
		(cond
			((null? e) '())
			((symbol? e) (if (member e varlist) (list e) '()))
			((list? (car e)) (append (check-exp (car e) varlist) (check-exp (cdr e) varlist)))
			((eq? 'lambda (car e)) (append (check-exp (caddr e) (append varlist (cadr e)))
										(check-exp (cdddr e) varlist)))
			((member (car e) varlist) (append (list e) (check-exp (cdr e) varlist)))
			(else (check-exp (cdr e) varlist))))
	(check-exp e '()))

; Helper for problem 1b
(define (make-set ls)
	(cond
		((null? ls) ls)
		((member (car ls) (cdr ls)) (make-set (cdr ls)))
		(else (cons (car ls) (make-set (cdr ls))))))

; Problem 1b
(define (free-vars e)
	(define (check-exp e varlist)
		(cond
			((null? e) '())
			((symbol? e) (if (member e varlist) '() (list e)))
			((list? (car e)) (append (check-exp (car e) varlist) (check-exp (cdr e) varlist)))
			((eq? 'lambda (car e)) (append (check-exp (caddr e) (append varlist (cadr e)))
										(check-exp (cdddr e) varlist)))
			((not (member (car e) varlist)) (append (list (car e)) (check-exp (cdr e) varlist)))
			(else (check-exp (cdr e) varlist))))
	(make-set (check-exp e '())))

; Helper for problem 2a
(define (member? x ls) 
	(if (list? ls)
		(not (not (member x ls)))
		(eq? x ls)))

; Helper for 2a
(define (expand-let exp)
	(append (list (list 'lambda (map car (cadr exp)) (caddr exp))) (map cadr (cadr exp))))

(define (expand-let* exp)
	(define (helper vars)
		(if (null? vars) (caddr exp)
			(list (list 'lambda (list (caar vars)) (helper (cdr vars))) (cadar vars))))
	(helper (cadr exp)))

; Problem 2a
(define (occurs-free? var exp)
	(cond
		((symbol? exp) (eq? var exp))
		((eq? (car exp) 'lambda)
			(and (not (member? var (cadr exp)))
				(occurs-free? var (caddr exp))))
		((eq? (car exp) 'let) (occurs-free? var (expand-let exp)))
		((eq? (car exp) 'let*) (occurs-free? var (expand-let* exp)))
		((eq? (car exp) 'set!) (eq? (caddr exp) var))
		(else (ormap (lambda (x) (occurs-free? var x)) exp))))

; Problem 2b
(define (occurs-bound? var exp)
	(cond
		((symbol? exp) #f)
		((and (list? exp) (symbol? (car exp)) (null? (cdr exp))) #f)
		((eq? (car exp) 'lambda)
			(or (occurs-bound? var (caddr exp))
				(and (member? var (cadr exp))
					(occurs-free? var (caddr exp)))))
		((eq? (car exp) 'let) (occurs-bound? var (expand-let exp)))
		((eq? (car exp) 'let*) (occurs-bound? var (expand-let* exp)))
		((eq? (car exp) 'set!) #f)
		(else (or (occurs-bound? var (car exp))
				(occurs-bound? var (cadr exp))))))