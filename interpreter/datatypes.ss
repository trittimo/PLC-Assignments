
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

; (define-datatype expression expression?
;   [var-exp        ; variable references
;    (id symbol?)]
;   [lit-exp        ; "Normal" data.  Did I leave out any types?
;    (datum
;     (lambda (x)
;       (ormap 
;        (lambda (pred) (pred x))
;        (list number? vector? boolean? symbol? string? pair? null?))))]
;   [app-exp        ; applications
;    (rator expression?)
;    (rands (list-of expression?))]  
;   )

	
; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.

(define-datatype proc-val proc-val?
  [prim-proc
   (name symbol?)])
	 
	 
	 
	
;; environment type definitions

(define scheme-value?
  (lambda (x) #t))

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vals (list-of scheme-value?))
   (env environment?)))