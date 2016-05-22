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

(define (set-ref! ls pos val)
   (if (= pos 0)
      (begin (set-car! ls val) ls)
      (begin (set-cdr! ls (set-ref! (cdr ls) (sub1 pos) val)) ls)))

(define (set-global-val id assignment)
   (cases environment (unbox global-env)
      (extended-env-record (syms vals env)
            (extended-env-record (cons id syms) (cons assignment vals) env))
      (else (unbox global-env))))

(define (replace-val env id assignment)
   (cases environment (unbox env)
      (extended-env-record (syms vals env)
         (let ((pos (list-find-position id syms)))
            (if (number? pos)
               (extended-env-record syms (set-ref! vals pos assignment) env)
               (extended-env-record syms vals (box (replace-val env id assignment))))))
      (else (set-global-val id assignment))))

(define (eval-exp exp env)
   (cases expression exp
      (set!-exp (id assignment)
         (set-box! env (replace-val env id (eval-exp assignment env))))
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
                  (lambda () (error 'apply-env (format "variable ~s is not bound" id)))))))
      (app-exp (rator rands)
         (let ((proc-value (eval-exp rator env)) (args (eval-rands rands env)))
            (if (not (proc-val? proc-value))
               (apply proc-value args)
               (cases proc-val proc-value
                  (prim-proc (op) (apply-proc proc-value args))
                  (closure (params varargs bodies env)
                     (cond
                        ((and (null? varargs) (null? params)) (apply-proc proc-value args))
                        ((null? varargs) (apply-proc proc-value args))
                        ((null? params) (apply-proc proc-value (list args)))
                        (else (apply-proc proc-value (get-app-args (length params) args)))))))))
      (lambda-exp (params varargs bodies)
         (closure params varargs bodies env))
      (let-exp (assigned bodies)
         (eval-bodies bodies (extend-env (map car assigned) (eval-rands (map cadr assigned) env)env)))
      (else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp))))

(define (eval-rands rands env)
   (map (lambda (x) (eval-exp x env)) rands))

(define (apply-proc proc-value args)
   (cases proc-val proc-value
      (prim-proc (op) (apply-prim-proc op args))
      (closure (params varargs bodies env) (eval-bodies bodies (extend-env (append params varargs) args env)))
      (else (error 'apply-proc (format "Attempt to apply bad procedure: ~s" proc-value)))))

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
   vector vector-set! member quotient append list-tail eqv? assq newline call/cc ))

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
   (lambda (prim-proc args)
      (case prim-proc
         ((call/cc) (call/cc (lambda (x) (apply-proc (1st args) (list x)))))
         ((newline) (newline))
         ((assq) (assq (1st args) (2nd args)))
         ((eqv?) (eqv? (1st args) (2nd args)))
         ((list-tail) (list-tail (1st args) (2nd args)))
         ((append) (apply append args))
         ((member) (member (1st args) (2nd args)))
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
         ((quotient) (apply quotient args))
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
                  (format "Bad primitive procedure name: ~s" 
                  prim-op))))))

(define (rep)
   (display "--> ")
   (let ((answer (top-level-eval (parse-exp (read)))))
      (eopl:pretty-print answer) (newline)
      (rep)))

(define (eval-one-exp x) (top-level-eval (parse-exp x)))