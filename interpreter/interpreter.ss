;-------------------+
;                   |
;   INTERPRETER     |
;                   |
;-------------------+

(define (top-level-eval form)
   (eval-exp form init-env (identity)))

(define (get-app-args paramslen args)
   (if (= paramslen 0)
      (list args)
      (cons (car args) (get-app-args (sub1 paramslen) (cdr args)))))

(define (set-ref! ls pos val)
   (if (= pos 0)
      (begin (set-car! ls val) ls)
      (begin (set-cdr! ls (set-ref! (cdr ls) (sub1 pos) val)) ls)))

(define (set-global-val id assignment)
   (cases environment (unbox global-env)
      (extended-env-record (syms vals env)
            (extended-env-record (cons id syms) (cons assignment vals) env))
      (else (unbox global-env))))

(define (replace-val env id assignment k)
   (cases environment (unbox env)
      (extended-env-record (syms vals env)
         (let ((pos (list-find-position id syms)))
            (if (number? pos)
               (extended-env-record syms (set-ref! vals pos assignment) env)
               (extended-env-record syms vals (box (replace-val env id assignment))))))
      (else (set-global-val id assignment))))

(define (eval-exp exp env k)
   (cases expression exp
      (set!-exp (id assignment)
         (set-box! env (replace-val env id (eval-exp assignment env))))
      (if-exp (comp true false)
         (eval-exp comp env (test-k true false env k)))
      (lit-exp (datum) (apply-k k datum))
      (var-exp (id) (apply-env env id k))
      (app-exp (rator rands)
         (eval-exp rator env (rator-k rands env k)))
      (lambda-exp (params varargs bodies)
         (apply-k k (closure params varargs bodies env)))
      (let-exp (assigned bodies)
         (apply-k k (eval-bodies bodies (extend-env (map car assigned) (eval-rands (map cadr assigned) env) env))))
      (else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp))))

(define (eval-rands rands env k)
   ; TODO
   (apply-k k (map (lambda (x) (eval-exp x env (identity))) rands)))

(define (apply-proc proc-value args k)
   (apply-k k (cases proc-val proc-value
      (prim-proc (op) (apply-prim-proc op args k))
      (closure (params varargs bodies env) (eval-bodies bodies (extend-env (append params varargs) args env) k))
      (else (error 'apply-proc (format "Attempt to apply bad procedure: ~s" proc-value))))))

(define (eval-bodies bodies env k)
   (let loop ((bodies bodies))
      (if (null? (cdr bodies))
         (eval-exp (car bodies) env k)
         (begin
            (eval-exp (car bodies) env k)
            (loop (cdr bodies))))))

(define *prim-proc-names* 
   '(+ - * / add1 sub1 cons = not zero? list procedure? null? 
   >= <= > < eq? equal? length list->vector list? pair? 
   vector->list number? cdr cadr car caar cadar symbol? 
   vector? display set-car! set-cdr! map apply vector-ref
   vector vector-set! member quotient append list-tail eqv? assq newline ))

(define init-env (extend-env *prim-proc-names* (map prim-proc *prim-proc-names*) (empty-env)))

(define global-env init-env)

(define (reset-global-env)
      (set! global-env (extend-env *prim-proc-names* (map prim-proc *prim-proc-names*) (empty-env))))

(define (make-map-proc proc)
   (lambda (x) (apply-proc proc (list x))))

(define (get-apply-list args)
   (if (null? (cdr args))
      (car args)
      (cons (car args) (get-apply-list (cdr args)))))

(define apply-prim-proc
   (lambda (prim-proc args k)
      (case prim-proc
         ((newline) (apply-k k (newline)))
         ((assq) (apply-k k (assq (1st args) (2nd args))))
         ((eqv?) (apply-k k (eqv? (1st args) (2nd args))))
         ((list-tail) (apply-k k (list-tail (1st args) (2nd args))))
         ((append) (apply-k k (apply append args)))
         ((member) (apply-k k (member (1st args) (2nd args))))
         ((vector-set!) (apply-k k (vector-set! (1st args) (2nd args) (3rd args))))
         ((vector) (apply-k k (apply vector args)))
         ((map) (apply-k k (map (make-map-proc (1st args)) (2nd args))))
         ((apply) (apply-k k (apply-proc (1st args) (get-apply-list (cdr args)))))
         ((vector-ref) (apply-k k (vector-ref (1st args) (2nd args))))
         ((set-cdr!) (apply-k k (set-cdr! (1st args) (2nd args))))
         ((set-car!) (apply-k k (set-car! (1st args) (2nd args))))
         ((vector?) (apply-k k (vector? (1st args))))
         ((symbol?) (apply-k k (symbol? (1st args))))
         ((cdr) (apply-k k (cdr (1st args))))
         ((caar) (apply-k k (caar (1st args))))
         ((cadar) (apply-k k (cadar (1st args))))
         ((car) (apply-k k (car (1st args))))
         ((cadr) (apply-k k (cadr (1st args))))
         ((number?) (apply-k k (apply-k k (number? (1st args)))))
         ((vector->list) (apply-k k (vector->list (1st args))))
         ((pair?) (apply-k k (pair? (1st args))))
         ((list?) (apply-k k (list? (1st args))))
         ((+) (apply-k k (apply + args)))
         ((-) (apply-k k (apply - args)))
         ((*) (apply-k k (apply * args)))
         ((/) (apply-k k (apply / args)))
         ((quotient) (apply-k k (apply quotient args)))
         ((add1) (apply-k k (+ (1st args) 1)))
         ((sub1) (apply-k k (- (1st args) 1)))
         ((cons) (apply-k k (cons (1st args) (2nd args))))
         ((=) (apply-k k (= (1st args) (2nd args))))
         ((not) (apply-k k (not (1st args))))
         ((zero?) (apply-k k (zero? (1st args))))
         ;((list) (map (lambda (x) (eval-exp x global-env)) args))
         ((list) (apply-k k args))
         ((procedure?)
            (apply-k k (cond
               ((not (list? (1st args))) #f)
               ((eq? (caar args) 'prim-proc) (exists (lambda (x) (eq? x (cadar args))) *prim-proc-names*))
               (else (eq? (caar args) 'closure)))))
         ((display) (apply-k k (display (1st args))))
         ((null?) (apply-k k (null? (1st args))))
         ((>=) (apply-k k (>= (1st args) (2nd args))))
         ((<=) (apply-k k (<= (1st args) (2nd args))))
         ((>) (apply-k k (> (1st args) (2nd args))))
         ((<) (apply-k k (< (1st args) (2nd args))))
         ((eq?) (apply-k k (eq? (1st args) (2nd args))))
         ((equal?) (apply-k k (equal? (1st args) (2nd args))))
         ((length) (apply-k k (length (1st args))))
         ((list->vector) (apply-k k (list->vector (1st args))))
         (else (error 'apply-prim-proc 
                  (format "Bad primitive procedure name: ~s" 
                  prim-op))))))

(define (rep)
   (display "--> ")
   (let ((answer (top-level-eval (parse-exp (read)))))
      (eopl:pretty-print answer) (newline)
      (rep)))

(define (eval-one-exp x) (top-level-eval (parse-exp x)))