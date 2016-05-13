;-----------------------+
;                       |
;   SYNTAX EXPANSION    |
;                       |
;-----------------------+

(define (expand-or datum)
   (if (null? datum)
      #f
      (list 'let (list (list 'ans (car datum))) (list 'if 'ans 'ans (expand-or (cdr datum))))))

(define (expand-and datum)
   (cond
      ((null? datum) #t)
      ((null? (cdr datum)) (car datum))
      (else (list 'if (car datum) (expand-and (cdr datum)) #f))))

(define (expand-cond datum)
   (cond
      ((null? datum) )
      ((eqv? (caar datum) 'else) (cdar datum))
      (else (list (append (list 'if (caar datum) (cadar datum)) (expand-cond (cdr datum)))))))

(define (expand-let* datum)
   (cond
      ((null? (1st datum)) (list (append (list 'lambda (list)) (cdr datum))))
      ((null? (cdr (1st datum))) (list (append (list 'lambda (list (caaar datum))) (cdr datum)) (cadaar datum)))
      (else (list (list 'lambda (list (caaar datum)) (expand-let* (cons (cdar datum) (cdr datum)))) (cadaar datum)))))

(define (expand-begin datum)
   (list (append (list 'lambda '()) datum)))

(define (expand-case datum)
   (letrec ((expr (1st datum)) 
            (loop (lambda (datum)
               (cond
                  ((null? datum))
                  ((eqv? (caar datum) 'else) (cdar datum))
                  (else (list (append (list 'if (list 'member expr (cons 'list (caar datum))) (cadar datum)) (loop (cdr datum)))))))))
      (loop (cdr datum))))

(define (expand-while datum)
   (list 'let (list (list 'loop (list 'lambda '(func) 
      (list 'if (1st datum) 
         (append (cons 'begin (cdr datum)) 
            (list (list 'func 'func))))))) 
      (list 'loop 'loop)))

(define (expand-letrec datum)
   (append (list 'let)
      (append (append (list (map (lambda (x) (list (car x) #f)) (cadr datum)))
              (map (lambda (x) (list 'set! (car x) (cadr x))) (cadr datum)))
              (cddr datum))))

(define (expand-named-let datum)
   (let ((temps (generate-temporaries (length (2nd datum)))) (args (map car (2nd datum))))
      (list 'let (map list temps (map cadr (2nd datum)))
         (list 'letrec
            (cons (list (1st datum) (append (list 'lambda args) (cddr datum)))
               (map list args temps))
            (cons (1st datum) args)))))

(define (expand-define datum)
   (append (list 'set!) datum))

(define (expand-exp datum)
   (let ((rest (cdr datum)))
      (case (1st datum)
         ((while) (expand-while rest))
         ((or) (expand-or rest))
         ((and) (expand-and rest))
         ((cond) (car (expand-cond rest)))
         ((let*) (expand-let* rest))
         ((begin) (expand-begin rest))
         ((case) (car (expand-case rest)))
         ((letrec) (expand-letrec datum))
         ((let) (expand-named-let rest))
         ((define) (expand-define rest))
         )))

(define (has-expansion? datum)
   (or (member (1st datum) '( or and cond let* begin case while letrec define ))
      (and (eqv? (1st datum) 'let) (symbol? (2nd datum)))))