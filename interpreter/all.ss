(load "chez-init.ss") 

;-------------------+
;                   |
;      HELPERS      |
;                   |
;-------------------+

; General helpers to make thing a bit more sane
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)
(define 4th cadddr)

; Used in define-datatype where we don't actually care about the type
(define (scheme-value? x) #t)

; Used in parse-exp where we check to ensure that certain expressions are lists where length = 2
(define (list-is-2-long? ls) (and (list? ls) (= (length ls) 2)))

; Gets the 'list' part of a list-pair [i.e. (get-list '(a b c . d)) -> (a b c)]
(define (get-list ls)
	(if (pair? ls)
		(cons (car ls) (get-list (cdr ls)))
		'()))

; Gets the 'pair' part of a list-pair [i.e. (get-last '(a b c . d)) -> d]
(define (get-last ls)
  (if (pair? ls)
  	(get-last (cdr ls))
	  ls))

;-------------------+
;                   |
;     DATATYPES     |
;                   |
;-------------------+

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
	(lambda-exp 
		(los (lambda (x) (or (list? x) (pair? x) (symbol? x)))) 
		(varargs (list-of symbol?))
		(body list?))
	(app-exp (rator expression?) (rand (lambda (x) (andmap expression? x)))))

(define-datatype environment environment?
	(empty-env-record)
	(extended-env-record
	 (syms (list-of scheme-value?))
	 (vals (list-of scheme-value?))
	 (env environment?)))

(define-datatype proc-val proc-val?
	(prim-proc (name symbol?))
	(closure
		(params (list-of scheme-value?))
		(varargs (list-of scheme-value?))
		(bodies (list-of expression?))
		(env environment?)))


;-------------------+
;                   |
;      PARSER       |
;                   |
;-------------------+

(define (parse-exp datum)
	(cond
		((symbol? datum) (var-exp datum))
		((number? datum) (lit-exp datum))
		((boolean? datum) (lit-exp datum))
		((string? datum) (lit-exp datum))
		((vector? datum) (lit-exp datum))
		((pair? datum)
			(cond
				((has-expansion? datum) (parse-exp (expand-exp datum)))
				((eqv? (1st datum) 'quote)
					(lit-exp (2nd datum)))
				((eqv? (1st datum) 'lambda)
					(cond
						((< (length datum) 3) (eopl:error 'parse-exp (format "incorrect number of arguments in lambda: ~s" datum)))
						((and (not (symbol? (2nd datum))) (not (or (pair? (2nd datum)) (andmap symbol? (2nd datum)))))
							(eopl:error 'parse-exp (format "lambda arguments are not symbols: ~s" (2nd datum))))
						((symbol? (2nd datum)) (lambda-exp '() (list (2nd datum)) (map parse-exp (cddr datum))))
						((list? (2nd datum)) (lambda-exp (2nd datum) '() (map parse-exp (cddr datum))))
						(else (lambda-exp (get-list (2nd datum)) (list (get-last (2nd datum))) (map parse-exp (cddr datum))))))
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
		(and-exp (args)
			(if (null? args)
				'(and)
				(cons 'and (unparse-exp args))))
		(or-exp (args)
			(if (null? args)
				'(or)
				(cons 'or (unparse-exp args))))
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
		(lambda-exp (los vararg body) 
			(cond 
				((and (= (length los) 0) (not (null? vararg))) ; (lambda x (car x))
					(append (list 'lambda (car vararg)) (map unparse-exp body)))
				((not (null? vararg)) ; (lambda (a b . c) stuff...)
					(append (list 'lambda (cons los (car vararg))) (map unparse-exp body)))
				(else (append (list 'lambda los) (map unparse-exp body)))))
		(app-exp (rator rand) 
			(cons (unparse-exp rator) (map unparse-exp rand)))))

;-----------------------+
;                       |
;   SYNTAX EXPANSION    |
;                       |
;-----------------------+

(define (or-expansion datum)
	(if (null? datum)
		#f
		(list 'if (car datum) (car datum) (or-expansion (cdr datum)))))

(define (and-expansion datum)
	(cond
		((null? datum) #t)
		((null? (cdr datum)) (car datum))
		(else (list 'if (car datum) (and-expansion (cdr datum)) #f))))

(define (cond-expansion datum) #f)
(define (let*-expansion datum) #f)
(define (begin-expansion datum) #f)
(define (case-expansion datum) #f)

(define (expand-exp datum)
	(let ((rest (cdr datum)))
		(case (1st datum)
			((or) (or-expansion rest))
			((and) (and-expansion rest))
			((cond) (cond-expansion rest))
			((let*) (let*-expansion rest))
			((begin) (begin-expansion rest))
			((case) (case-expansion rest)))))

(define (has-expansion? datum)
	(member (1st datum) '( or and cond let* begin case )))

;-----------------------+
;                       |
;      ENVIRONMENT      |
;                       |
;-----------------------+

(define (empty-env)
	(empty-env-record))

(define (extend-env syms vals env)
	(extended-env-record syms vals env))

(define (list-find-position sym los)
	(if (symbol? (car los))
		(list-index (lambda (xsym) (eqv? sym xsym)) los)
		(list-index (lambda (xsym) (eqv? sym xsym)) (map cadr los))))

(define (list-index pred ls)
	(cond
		((null? ls) #f)
		((pred (car ls)) 0)
		(else 
			(let ((list-index-r (list-index pred (cdr ls))))
				(if (number? list-index-r)
					(+ 1 list-index-r)
					#f)))))

; succeed and fail are procedures applied if the var is or isn't found, respectively.
(define (apply-env env sym succeed fail)
	(cases environment env
		(empty-env-record ()
			(fail))
		(extended-env-record (syms vals env)
			(let ((pos (list-find-position sym syms)))
							(if (number? pos)
						(succeed (list-ref vals pos))
						(apply-env env sym succeed fail))))))

;-------------------+
;                   |
;   INTERPRETER     |
;                   |
;-------------------+

; top-level-eval evaluates a form in the global environment
(define top-level-eval
	(lambda (form)
		(eval-exp form init-env)))

(define (get-app-args paramslen args)
	(if (= paramslen 0)
		(list args)
		(cons (car args) (get-app-args (sub1 paramslen) (cdr args)))))

(define (eval-exp exp env)
	(cases expression exp
		(if-exp (comp true false)
			(cases expression false
				(empty-exp () (if (eval-exp comp env) (eval-exp true env)))
				(else (if (eval-exp comp env) (eval-exp true env) (eval-exp false env)))))
		(lit-exp (datum) datum)
		(var-exp (id)
			(apply-env env id
				(lambda (x) x)
				(lambda ()
					(apply-env global-env id (lambda (x) x) 
						(lambda () (error 'apply-env "variable ~s is not bound" id))))))
		(app-exp (rator rands)
			(let ((proc-value (eval-exp rator env)) (args (eval-rands rands env)))
					(cases proc-val proc-value
						(prim-proc (op) (apply-proc proc-value args))
						(closure (params varargs bodies env)
							(cond
								((null? varargs) (apply-proc proc-value args))
								((null? params) (apply-proc proc-value (list args)))
								(else (apply-proc proc-value (get-app-args (length params) args))))))))
		(lambda-exp (params varargs bodies)
			(closure params varargs bodies env))
		(let-exp (assigned bodies)
			(eval-bodies bodies (extend-env (map car assigned) (eval-rands (map cadr assigned) env) env)))
		(else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp))))

(define (eval-rands rands env)
	(map (lambda (x) (eval-exp x env)) rands))

(define (apply-proc proc-value args)
	(cases proc-val proc-value
		(prim-proc (op) (apply-prim-proc op args))
		(closure (params varargs bodies env) (eval-bodies bodies (extend-env (append params varargs) args env)))
		(else (error 'apply-proc "Attempt to apply bad procedure: ~s" proc-value))))

(define (eval-bodies bodies env)
	(let loop ((bodies bodies))
		(if (null? (cdr bodies))
			(eval-exp (car bodies) env)
			(begin
				(eval-exp (car bodies) env)
				(loop (cdr bodies))))))

(define *prim-proc-names* 
 	'(+ - * / add1 sub1 cons = not zero? list procedure? null? 
	>= <= > < eq? equal? length list->vector list? pair? 
	vector->list number? cdr cadr car caar cadar symbol? 
	vector? display set-car! set-cdr! map apply vector-ref
	vector vector-set! ))

(define init-env (extend-env *prim-proc-names* (map prim-proc *prim-proc-names*) (empty-env)))

(define global-env init-env)

(define (make-map-proc proc)
	(lambda (x) (apply-proc proc (list x))))

(define (get-apply-list args)
	(if (null? (cdr args))
		(car args)
		(cons (car args) (get-apply-list (cdr args)))))

(define apply-prim-proc
	(lambda (prim-proc args)
		(case prim-proc
			((vector-set!) (vector-set! (1st args) (2nd args) (3rd args)))
			((vector) (apply vector args))
			((map) (map (make-map-proc (1st args)) (2nd args)))
			((apply) (apply-proc (1st args) (get-apply-list (cdr args))))
			((vector-ref) (vector-ref (1st args) (2nd args)))
			((set-cdr!) (set-cdr! (1st args) (2nd args)))
			((set-car!) (set-car! (1st args) (2nd args)))
			((vector?) (vector? (1st args)))
			((symbol?) (symbol? (1st args)))
			((cdr) (cdr (1st args)))
			((caar) (caar (1st args)))
			((cadar) (cadar (1st args)))
			((car) (car (1st args)))
			((cadr) (cadr (1st args)))
			((number?) (number? (1st args)))
			((vector->list) (vector->list (1st args)))
			((pair?) (pair? (1st args)))
			((list?) (list? (1st args)))
			((+) (apply + args))
			((-) (apply - args))
			((*) (apply * args))
			((/) (apply / args))
			((add1) (+ (1st args) 1))
			((sub1) (- (1st args) 1))
			((cons) (cons (1st args) (2nd args)))
			((=) (= (1st args) (2nd args)))
			((not) (not (1st args)))
			((zero?) (zero? (1st args)))
			;((list) (map (lambda (x) (eval-exp x global-env)) args))
			((list) args)
			((procedure?)
				(cond
					((not (list? (1st args))) #f)
					((eq? (caar args) 'prim-proc) (exists (lambda (x) (eq? x (cadar args))) *prim-proc-names*))
					(else (eq? (caar args) 'closure))))
			((display) (display (1st args)))
			((null?) (null? (1st args)))
			((>=) (>= (1st args) (2nd args)))
			((<=) (<= (1st args) (2nd args)))
			((>) (> (1st args) (2nd args)))
			((<) (< (1st args) (2nd args)))
			((eq?) (eq? (1st args) (2nd args)))
			((equal?) (equal? (1st args) (2nd args)))
			((length) (length (1st args)))
			((list->vector) (list->vector (1st args)))
			(else (error 'apply-prim-proc 
						"Bad primitive procedure name: ~s" 
						prim-op)))))

(define (rep)
	(display "--> ")
	(let ((answer (top-level-eval (parse-exp (read)))))
		(eopl:pretty-print answer) (newline)
		(rep)))

(define (eval-one-exp x) (top-level-eval (parse-exp x)))

;-------------------+
;                   |
;	    TODO        |
;                   |
;-------------------+

; HIGH PRIORITY
; cond syntax
; begin syntax
; while syntax
; case syntax
; let, let* syntax expansion

; MEDIUM PRIORITY
; Use make-c...r instead of mapping car, cadr, etc. directly

; LOW PRIORITY
; Make datatype checks implementation independent (i.e. don't use (eq? (car type) 'lit-exp))
; (rep) may need to display some values differently