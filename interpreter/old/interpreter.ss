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
				(begin (display (map car assigned))
				(eval-bodies bodies (extend-env (map car assigned) (eval-rands (map cadr assigned) env) env)))]
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
									vector->list number? cdr cadr car caar cadar symbol? vector? ))

(define init-env         ; for now, our initial global environment only contains 
	(extend-env            ; procedure names.  Recall that an environment associates
		 *prim-proc-names*   ;  a value (not an expression) with an identifier.
		 (map prim-proc      
					*prim-proc-names*)
		 (empty-env)))

(define global-env init-env)

; Usually an interpreter must define each 
; built-in procedure individually.  We are "cheating" a little bit.

; TODO Extend to use make c...r
(define apply-prim-proc
	(lambda (prim-proc args)
		(case prim-proc
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
			[(procedure?) (procedure? (1st args))]
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










