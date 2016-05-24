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
            ((eqv? (1st datum) 'continue) (continue-exp))
            ((eqv? (1st datum) 'while) (while-exp (parse-exp (2nd datum)) (map parse-exp (cddr datum))))
            ((has-expansion? datum) (parse-exp (expand-exp datum)))
            ((eqv? (1st datum) 'quote)
               (lit-exp (2nd datum)))
            ((eqv? (1st datum) 'lambda)
               (cond
                  ((< (length datum) 3) (eopl:error 'parse-exp "incorrect number of arguments in lambda: ~s" datum))
                  ((and (not (symbol? (2nd datum))) (not (or (pair? (2nd datum)) (andmap symbol? (2nd datum)))))
                     (eopl:error 'parse-exp "lambda arguments are not symbols: ~s" (2nd datum)))
                  ((symbol? (2nd datum)) (lambda-exp '() (list (2nd datum)) (map parse-exp (cddr datum))))
                  ((list? (2nd datum)) (lambda-exp (2nd datum) '() (map parse-exp (cddr datum))))
                  (else (lambda-exp (get-list (2nd datum)) (list (get-last (2nd datum))) (map parse-exp (cddr datum))))))
            ((eqv? (1st datum) 'if)
               (if (< (length datum) 3)
                  (eopl:error 'parse-exp "incorrect number of arguments in if")
                  (if-exp (parse-exp (2nd datum)) (parse-exp (3rd datum)) 
                     (if (> (length datum) 3) (parse-exp (4th datum)) (empty-exp)))))
            ((eqv? (1st datum) 'set!)
               (cond
                  ((not (= (length datum) 3)) (eopl:error 'parse-exp "incorrect number of arguments to set!: ~s" datum))
                  (else
                     (set!-exp (2nd datum) (parse-exp (3rd datum))))))
            ((not (list? datum))
               (eopl:error 'parse-exp "Datum '~s' is not a proper list" datum))
            (else (app-exp (parse-exp (1st datum))
               (map parse-exp (cdr datum))))))
      (else (eopl:error 'parse-exp "bad expression: ~s" datum))))

(define (unparse-exp exp)
   (cases expression exp
      (set!-exp (id assignment)
         (list 'set! id (unparse-exp assignment)))
      (empty-exp () '())
      (if-exp (comp true false)
         (if (and (list? false) (eqv? (1st false) 'empty-exp))
            (list 'if (unparse-exp comp) (unparse-exp true))
            (list 'if (unparse-exp comp) (unparse-exp true) (unparse-exp false))))
      (var-exp (id) id)
      (lit-exp (num) num)
      (lambda-exp (los vararg body)
         (cond 
            ((and (= (length los) 0) (not (null? vararg)))
               (append (list 'lambda (car vararg)) (map unparse-exp body)))
            ((not (null? vararg))
               (append (list 'lambda (cons los (car vararg))) (map unparse-exp body)))
            (else (append (list 'lambda los) (map unparse-exp body)))))
      (app-exp (rator rand)
         (cons (unparse-exp rator) (map unparse-exp rand)))))