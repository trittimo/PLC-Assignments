; This is a parser for simple Scheme expressions, such as those in EOPL, 3.1 thru 3.3.

; You will want to replace this with your parser that includes more expression types, more options for these types, and error-checking.

; Procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)
(define 4th cadddr)

(define (list-is-2-long? ls) (and (list? ls) (= (length ls) 2)))

(define (parse-exp datum)
	(cond
		((symbol? datum) (var-exp datum))
		((number? datum) (lit-exp datum))
		((boolean? datum) (lit-exp datum))
		((string? datum) (lit-exp datum))
		((vector? datum) (lit-exp datum))
		((pair? datum)
			(cond
				((eqv? (1st datum) 'quote)
					(lit-exp (2nd datum)))
				((eqv? (1st datum) 'lambda)
					(cond
						((< (length datum) 3) (eopl:error 'parse-exp (format "incorrect number of arguments in lambda: ~s" datum)))
						((and (not (symbol? (2nd datum))) (not (andmap symbol? (2nd datum))))
							(eopl:error 'parse-exp (format "lambda arguments are not symbols: ~s" (2nd datum))))
						(else (lambda-exp (2nd datum) (map parse-exp (cddr datum))))))
				((eqv? (1st datum) 'if)
					(if (< (length datum) 3)
						(eopl:error 'parse-exp "incorrect number of arguments in if")
						(if-exp (parse-exp (2nd datum)) (parse-exp (3rd datum)) 
							(if (> (length datum) 3) (parse-exp (4th datum)) (empty-exp)))))
				((and (eqv? (1st datum) 'let) (symbol? (2nd datum)))
					(cond
						((< (length datum) 4) (eopl:error 'parse-exp (format "incorrect number of arguments to named let: ~s" datum)))
						((not (list? (3rd datum))) (eopl:error 'parse-exp (format "not a proper list: ~s" (3rd datum))))
						((not (andmap list-is-2-long? (3rd datum))) (eopl:error 'parse-exp (format "not all proper lists: ~s" (3rd datum))))
						((not (andmap symbol? (map 1st (3rd datum)))) (eopl:error 'parse-exp (format "first members must be symbols: ~s" (3rd datum))))
						(else
							(named-let-exp
								(2nd datum)
								(map (lambda (x) (list (parse-exp (1st x)) (parse-exp (2nd x)))) (3rd datum))
								(map parse-exp (cdddr datum))))))
				((eqv? (1st datum) 'let)
					(cond
						((< (length datum) 3) (eopl:error 'parse-exp (format "incorrect number of arguments to let: ~s" datum)))
						((not (list? (2nd datum))) (eopl:error 'parse-exp (format "not a proper list: ~s" (2nd datum))))
						((not (andmap list-is-2-long? (2nd datum))) (eopl:error 'parse-exp (format "not all proper lists: ~s" (2nd datum))))
						((not (andmap symbol? (map 1st (2nd datum)))) (eopl:error 'parse-exp (format "first members must be symbols: ~s" (2nd datum))))
						(else
							(let-exp 
								(map (lambda (x) (list (parse-exp (1st x)) (parse-exp (2nd x)))) (2nd datum))
								(map parse-exp (cddr datum))))))
				((eqv? (1st datum) 'letrec)
					(cond
						((< (length datum) 3) (eopl:error 'parse-exp (format "incorrect number of arguments to letrec: ~s" datum)))
						((not (list? (2nd datum))) (eopl:error 'parse-exp (format "not a proper list: ~s" (2nd datum))))
						((not (andmap list-is-2-long? (2nd datum))) (eopl:error 'parse-exp (format "not all proper lists: ~s" (2nd datum))))
						((not (andmap symbol? (map 1st (2nd datum)))) (eopl:error 'parse-exp (format "first members must be symbols: ~s" (2nd datum))))
						(else
							(letrec-exp 
								(map (lambda (x) (list (parse-exp (1st x)) (parse-exp (2nd x)))) (2nd datum))
								(map parse-exp (cddr datum))))))
				((eqv? (1st datum) 'let*)
					(cond
						((< (length datum) 3) (eopl:error 'parse-exp (format "incorrect number of arguments to let*: ~s" datum)))
						((not (list? (2nd datum))) (eopl:error 'parse-exp (format "not a proper list: ~s" (2nd datum))))
						((not (andmap list-is-2-long? (2nd datum))) (eopl:error 'parse-exp (format "not all proper lists: ~s" (2nd datum))))
						((not (andmap symbol? (map 1st (2nd datum)))) (eopl:error 'parse-exp (format "first members must be symbols: ~s" (2nd datum))))
						(else
							(let*-exp 
								(map (lambda (x) (list (parse-exp (1st x)) (parse-exp (2nd x)))) (2nd datum))
								(map parse-exp (cddr datum))))))
				((eqv? (1st datum) 'set!)
					(cond
						((not (= (length datum) 3)) (eopl:error 'parse-exp (format "incorrect number of arguments to set!: ~s" datum)))
						(else
							(set!-exp (2nd datum) (parse-exp (3rd datum))))))
				((not (list? datum))
					(eopl:error 'parse-exp (format "Datum '~s' is not a proper list" datum)))
				(else (app-exp (parse-exp (1st datum))
					(map parse-exp (cdr datum))))))
		(else (eopl:error 'parse-exp "bad expression: ~s" datum))))

(define (unparse-exp exp)
	(cases expression exp
		(set!-exp (id assignment)
			(list 'set! id (unparse-exp assignment)))
		(named-let-exp (id assigned bodies)
			(append (list 'let id)
				(cons (map (lambda (x) (list (unparse-exp (1st x)) (unparse-exp (2nd x)))) assigned)
					(map unparse-exp bodies))))
		(let*-exp (assigned bodies)
			(cons 'let* 
				(cons (map (lambda (x) (list (unparse-exp (1st x)) (unparse-exp (2nd x)))) assigned)
				(map unparse-exp bodies))))
		(letrec-exp (assigned bodies)
			(cons 'letrec
				(cons (map (lambda (x) (list (unparse-exp (1st x)) (unparse-exp (2nd x)))) assigned)
				(map unparse-exp bodies))))
		(let-exp (assigned bodies)
			(cons 'let 
				(cons (map (lambda (x) (list (unparse-exp (1st x)) (unparse-exp (2nd x)))) assigned)
				(map unparse-exp bodies))))
		(empty-exp () '())
		(if-exp (comp true false)
			(if (and (list? false) (eqv? (1st false) 'empty-exp))
				(list 'if (unparse-exp comp) (unparse-exp true))
				(list 'if (unparse-exp comp) (unparse-exp true) (unparse-exp false))))
		(var-exp (id) id)
		(lit-exp (num) num)
		(lambda-exp (los body) (append (list 'lambda los) (map unparse-exp body)))
		(app-exp (rator rand) 
			(cons (unparse-exp rator) (map unparse-exp rand)))))









