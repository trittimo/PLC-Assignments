;-----------------------+
;                       |
;      ENVIRONMENT      |
;                       |
;-----------------------+

(define (empty-env)
   (box (empty-env-record)))

(define (extend-env syms vals env)
   (box (extended-env-record syms vals env)))

(define (list-find-position sym los)
   (if (and (not (null? los)) (symbol? (car los)))
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
   (cases environment (unbox env)
      (empty-env-record ()
         (fail))
      (extended-env-record (syms vals env)
         (let ((pos (list-find-position sym syms)))
                     (if (number? pos)
                        (succeed (list-ref vals pos))
                        (apply-env env sym succeed fail))))))