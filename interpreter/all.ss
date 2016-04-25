;:  Single-file version of the interpreter.
;; Easier to submit to server, probably harder to use in the development process

(load "chez-init.ss") 

;-------------------+
;                   |
;    DATATYPES      |
;                   |
;-------------------+

;; Parsed expression datatypes

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
	
;; environment type definitions

(define scheme-value?
	(lambda (x) #t))

(define-datatype environment environment?
	(empty-env-record)
	(extended-env-record
	 (syms (list-of scheme-value?))
	 (vals (list-of scheme-value?))
	 (env environment?)))

; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.
(define-datatype proc-val proc-val?
	[prim-proc (name symbol?)]
	[closure (params (list-of scheme-value?)) (bodies (list-of expression?)) (env environment?)])
	 
	

;-------------------+
;                   |
;    PARSER         |
;                   |
;-------------------+

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


;-----------------------+
;                       |
;      ENVIRONMENT      |
;                       |
;-----------------------+

; Environment definitions for CSSE 304 Scheme interpreter.  Based on EoPL section 2.3

(define empty-env
	(lambda ()
		(empty-env-record)))

(define extend-env
	(lambda (syms vals env)
		(extended-env-record syms vals env)))

(define list-find-position
	(lambda (sym los)
		(if (symbol? (car los))
			(list-index (lambda (xsym) (eqv? sym xsym)) los)
			(list-index (lambda (xsym) (eqv? sym xsym)) (map cadr los)))))

(define list-index
	(lambda (pred ls)
		(cond
		 ((null? ls) #f)
		 ((pred (car ls)) 0)
		 (else (let ((list-index-r (list-index pred (cdr ls))))
			 (if (number? list-index-r)
		 (+ 1 list-index-r)
		 #f))))))

(define apply-env
	(lambda (env sym succeed fail) ; succeed and fail are procedures applied if the var is or isn't found, respectively.
		(cases environment env
			(empty-env-record ()
				(fail))
			(extended-env-record (syms vals env)
				(let ((pos (list-find-position sym syms)))
								(if (number? pos)
							(succeed (list-ref vals pos))
							(apply-env env sym succeed fail)))))))





;-----------------------+
;                       |
;   SYNTAX EXPANSION    |
;                       |
;-----------------------+



; To be added later









;-------------------+
;                   |
;   INTERPRETER    |
;                   |
;-------------------+
; top-level-eval evaluates a form in the global environment

(define top-level-eval
	(lambda (form)
		; later we may add things that are not expressions.
		(eval-exp form init-env)))

; eval-exp is the main component of the interpreter

(define eval-exp
	(lambda (exp env)
		(cases expression exp
			[if-exp (comp true false) (if (eval-exp comp env) (eval-exp true env) (eval-exp false env))]
			[lit-exp (datum) datum]
			[var-exp (id)
				(apply-env env id
					(lambda (x) x)
					(lambda ()
						(apply-env global-env id (lambda (x) x) 
							(lambda () (error 'apply-env "variable ~s is not bound" id)))))]
			[app-exp (rator rands)
				(let ([proc-value (eval-exp rator env)]
							[args (eval-rands rands env)])
					(apply-proc proc-value args))]
			[lambda-exp (params bodies)
				(closure params bodies env)]
			[let-exp (assigned bodies)
				(eval-bodies bodies (extend-env (map car assigned) (eval-rands (map cadr assigned) env) env))]
			[else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))

; evaluate the list of operands, putting results into a list

(define eval-rands
	(lambda (rands env)
		(map (lambda (x) (eval-exp x env)) rands)))

;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.  
;  User-defined procedures will be added later.

(define apply-proc
	(lambda (proc-value args)
		(cases proc-val proc-value
			[prim-proc (op) (apply-prim-proc op args)]
			[closure (params bodies env) (eval-bodies bodies (extend-env params args env))]
			[else (error 'apply-proc "Attempt to apply bad procedure: ~s" proc-value)])))

(define (eval-bodies bodies env)
	(let loop ([bodies bodies])
		(if (null? (cdr bodies))
			(eval-exp (car bodies) env)
			(begin
				(eval-exp (car bodies) env)
				(loop (cdr bodies))))))

; TODO Extend this to use make c...r from previous assignment
(define *prim-proc-names* '(+ - * / add1 sub1 cons = not zero? list procedure? null? 
									>= <= > < eq? equal? length list->vector list? pair? 
									vector->list number? cdr cadr car caar cadar symbol? 
									vector? display set-car! set-cdr! map apply vector-ref ))

(define init-env         ; for now, our initial global environment only contains 
	(extend-env            ; procedure names.  Recall that an environment associates
		 *prim-proc-names*   ;  a value (not an expression) with an identifier.
		 (map prim-proc      
					*prim-proc-names*)
		 (empty-env)))

(define global-env init-env)

; Usually an interpreter must define each 
; built-in procedure individually.  We are "cheating" a little bit.

(define (mapa proc)
	(lambda (x)
                (apply-proc proc (list x))))

(define (applya proc)
  (lambda (x y)
		(apply-proc proc (list x))))

; TODO Extend to use make c...r
(define apply-prim-proc
	(lambda (prim-proc args)
		(case prim-proc
			[(map) (map (mapa (1st args)) (2nd args))]
			[(apply) 
                        (display args)
                        (apply (applya (1st args)) (2nd args))]
			[(vector-ref) (vector-ref (1st args) (2nd args))]
			[(set-cdr!) (set-cdr! (1st args) (2nd args))]
			[(set-car!) (set-car! (1st args) (2nd args))]
			[(vector?) (vector? (1st args))]
			[(symbol?) (symbol? (1st args))]
			[(cdr) (cdr (1st args))]
			[(caar) (caar (1st args))]
			[(cadar) (cadar (1st args))]
			[(car) (car (1st args))]
			[(cadr) (cadr (1st args))]
			[(number?) (number? (1st args))]
			[(vector->list) (vector->list (1st args))]
			[(pair?) (pair? (1st args))]
			[(list?) (list? (1st args))]
			[(+) (apply + args)]
			[(-) (apply - args)]
			[(*) (apply * args)]
			[(/) (apply / args)]
			[(add1) (+ (1st args) 1)]
			[(sub1) (- (1st args) 1)]
			[(cons) (cons (1st args) (2nd args))]
			[(=) (= (1st args) (2nd args))]
			[(not) (not (1st args))]
			[(zero?) (zero? (1st args))]
			[(list) args]
			[(procedure?)
				(cond
					[(not (list? (1st args))) #f]
					[(eq? (caar args) 'prim-proc) (exists (lambda (x) (eq? x (cadar args))) *prim-proc-names*)]
					[else (eq? (caar args) 'closure)])]
			[(display) (display (1st args))]
			[(null?) (null? (1st args))]
			[(>=) (>= (1st args) (2nd args))]
			[(<=) (<= (1st args) (2nd args))]
			[(>) (> (1st args) (2nd args))]
			[(<) (< (1st args) (2nd args))]
			[(eq?) (eq? (1st args) (2nd args))]
			[(equal?) (equal? (1st args) (2nd args))]
			[(length) (length (1st args))]
			[(list->vector) (list->vector (1st args))]
			[else (error 'apply-prim-proc 
						"Bad primitive procedure name: ~s" 
						prim-op)])))

(define rep      ; "read-eval-print" loop.
	(lambda ()
		(display "--> ")
		;; notice that we don't save changes to the environment...
		(let ([answer (top-level-eval (parse-exp (read)))])
			;; TODO: are there answers that should display differently?
			(eopl:pretty-print answer) (newline)
			(rep))))  ; tail-recursive, so stack doesn't grow.

(define eval-one-exp
	(lambda (x) (top-level-eval (parse-exp x))))