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

; Problem 3
(define (lexical-address exp)
	(define (replace-bound var varlist)
		(cond
			((null? varlist) (list ': 'free var))
			((eq? var (caar varlist)) (cons ': (cdar varlist)))
		(else (replace-bound var (cdr varlist)))))

	(define (get-index x ls)
		(if (eq? x (car ls)) 0 (+ 1 (get-index x (cdr ls)))))

	(define (increment except varlist original)
		(cond
			((null? varlist) (map (lambda (x) (list x 0 (get-index x original))) except))
			((member (caar varlist) except) 
				(cons (list (caar varlist) 0 (get-index (caar varlist) original)) 
					(increment (remove (caar varlist) except) (cdr varlist) original)))
			(else (cons (list (caar varlist) (+ 1 (cadar varlist)) (caddar varlist)) (increment except (cdr varlist) original)))))

	(define (helper exp varlist)
		(cond
			((symbol? exp) (replace-bound exp varlist))
			((eq? (car exp) 'lambda)
				(list 'lambda (cadr exp) (helper (caddr exp) (increment (cadr exp) varlist (cadr exp)))))
			((eq? (car exp) 'let)
				(list 'let 
				(map (lambda (x) (list (car x) (helper (cadr x) varlist))) (cadr exp))
				(helper (caddr exp) (increment (map car (cadr exp)) varlist (map car (cadr exp))))))
			((eq? (car exp) 'if)
				(cons 'if (map (lambda (x) (helper x varlist)) (cdr exp))))
			((eq? (car exp) 'set!)
				(list 'set! (cadr exp) (helper (caddr exp) varlist)))
			(else (map (lambda (x) (helper x varlist)) exp))))

	(helper exp '()))

; Problem 4
(define (un-lexical-address exp)
	(define (helper exp varlist)
		(cond
			((symbol? exp) exp)
			((eq? (car exp) 'lambda)
				(append (list 'lambda (cadr exp)) (helper (cddr exp) (cons (cadr exp) varlist))))
			((eq? (car exp) 'let)
				(list 
					'let 
					(map (lambda (x) (list (car x) (helper (cadr x) varlist))) (cadr exp)) 
					(helper (caddr exp) (cons (map car (cadr exp)) varlist))))
			((eq? (car exp) 'if)
				(cons 'if (map (lambda (x) (helper x varlist)) (cdr exp))))
			((eq? (car exp) 'set!) 
				(cons 'set! (map (lambda (x) (helper x varlist)) (cdr exp))))
			((eq? (car exp) ':)
						(if (eq? (cadr exp) 'free)
							(caddr exp)
							(list-ref (list-ref varlist (cadr exp)) (caddr exp))))
			(else
				(map (lambda (x) (helper x varlist)) exp))))
			
	(helper exp '()))