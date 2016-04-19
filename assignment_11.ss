(load "chez-init.ss")
;(load "tests/A11-test-code.ss")


; Problem 1a
(define-syntax my-let
	(syntax-rules ()
		((_ ((x v) ...) e1 e2 ...) ; an expression of This form...
			((lambda (x ...) e1 e2 ...) v ...))
		((_ name ((x v) ...) e1 e2 ...)
			(letrec ((name (lambda (x ...) e1 e2 ...))) (name v ...)))
		)) ; is expanded To This

; Problem 1b
(define-syntax my-or
	(syntax-rules ()
		((_) #f)
		((_ x) x)
		((_ x y ...) (let ((a x)) (if a a (my-or y ...))))))

; Problem 1c
(define-syntax +=
	(syntax-rules ()
		((_ x y) (begin (set! x (+ x y)) x))))

; Problem 1d
(define-syntax return-first
	(syntax-rules ()
		((_ x y ...) (begin (let ((a x)) y ... a)))))

; Problem #2 Helper
; from EoPL, page 50
(define-datatype bintree bintree?
	(leaf-node (num integer?))
	(interior-node (key symbol?) (left-tree bintree?) (right-tree bintree?)))

; Problem #2
(define (bintree-to-list Tree)
	(cases bintree Tree
		(leaf-node (datum) (list 'leaf-node datum))
		(interior-node (key left right) (list 'interior-node key (bintree-to-list left) (bintree-to-list right)))))

; Problem 3 helper
(define (biggest a b) (if (> (cadr a) (cadr b)) a b))

; Problem 3 helper
(define (larger a b sym)
	(cond
		((and (number? a) (list? b))
			(if (> (+ a (cadr b)) (cadr b))
				(list sym (+ a (cadr b)))
				b))
		((and (list? a) (number? b))
			(if (> (+ (cadr a) b) (cadr a))
				(list sym (+ (cadr a) b))
				a))
		(else
			(if (> (+ (cadr a) (cadr b)) (cadr (biggest a b)))
				(list sym (+ (cadr a) (cadr b)))
				(biggest a b)))))

; Problem 3 helper
(define (max-rec T h)
		(cases bintree T
			(leaf-node (datum) (list h datum))
			(interior-node (key left right)
				(cond
					((and (number? (cadr left)) (number? (cadr right)))
						(list key (+ (cadr left) (cadr right))))
					((number? (cadr right))
						(larger (max-rec left h) (cadr right) key))
					((number? (cadr left))
						(larger (cadr left) (max-rec right h) key))
					(else (larger (max-rec left h) (max-rec right h) key))))))

; Problem 3
(define (max-interior T)
	(cases bintree T
		(leaf-node (datum) #f)
		(interior-node (key left right)
			(cond
				((and (number? (cadr left)) (number? (cadr right))) key)
				((number? (cadr left)) (car (max-rec right (cadr right))))
				((number? (cadr right)) (car (max-rec left (cadr left))))
				(else
					(let ((l (max-rec left (cadr left))) (r (max-rec right (cadr right))))
						(if (>= (cadr l) (cadr r)) (car l) (car r))))))))

; Everything below here is problem 4
(define-datatype expression expression?
	(set!-exp (id symbol?) (assignment expression?))
	(named-let-exp (id symbol?) (assigned list?) (bodies list?))
	(letrec-exp (assigned list?) (bodies list?))
	(let*-exp (assigned list?) (bodies list?))
	(let-exp (assigned list?) (bodies list?))
	(empty-exp)
	(if-exp (comp expression?) (true expression?) (false expression?))
	(lit-exp (num (lambda (x) (or (number? x) (boolean? x) (symbol? x) (string? x) (list? x) (vector? x)))))
	(var-exp (id symbol?))
	(lambda-exp (los (lambda (x) (or (list? x) (pair? x) (symbol? x)))) (body list?))
	(app-exp (rator expression?) (rand (lambda (x) (andmap expression? x)))))


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
					(lit-exp datum))
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