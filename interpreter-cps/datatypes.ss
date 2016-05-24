;-------------------+
;                   |
;       DATATYPES   |
;                   |
;-------------------+

(define-datatype expression expression?
   (set!-exp (id symbol?) (assignment expression?))
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
      (env box?)))

(define-datatype proc-val proc-val?
   (prim-proc (name symbol?))
   (c-proc (k continuation?))
   (closure
      (params (list-of scheme-value?))
      (varargs (list-of scheme-value?))
      (bodies (list-of expression?))
      (env box?)))