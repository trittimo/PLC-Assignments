(define (apply-continuation k . v) (apply k v))

; Problem 1a
(define (member?-cps item ls k)
    (cond
        ((null? ls) (apply-continuation k #f))
        ((eqv? (car ls) item) (apply-continuation k #t))
        (else (apply-continuation member?-cps item (cdr ls) k))))

; Problem 1b
(define (set?-cps ls k)
    (cond
        ((null? ls) (apply-continuation k #t))
        ((not (pair? ls)) (apply-continuation k #f))
        (else (member?-cps (car ls) (cdr ls)
            (lambda (result)
                (if result
                    (apply-continuation k #f)
                    (apply-continuation set?-cps (cdr ls) k)))))))



; Helpers for 1c
(define (1st-cps ls k)
    (apply-continuation k (car ls)))

(define (map-cps proc ls k)
    (if (null? ls)
        (apply-continuation k ls)
        (apply-continuation proc (car ls)
            (lambda (x) (map-cps proc (cdr ls)
                (lambda (y) (apply-continuation k (cons x y))))))))

(define (set-of-cps ls k)
    (if (null? ls) 
        (apply-continuation k '())
        (apply-continuation member?-cps (car ls) (cdr ls)
            (lambda (x)
                (if x 
                    (apply-continuation set-of-cps (cdr ls) k)
                    (apply-continuation set-of-cps (cdr ls)
                        (lambda (x) (apply-continuation k (cons (car ls) x)))))))))

; Problem 1c
(define (domain-cps rel k)
    (map-cps 1st-cps rel (lambda (x) (set-of-cps x (lambda (y) (apply-continuation k y))))))

; Problem 1d
(define (make-cps proc)
    (lambda (arg k)
        (apply-continuation k (proc arg))))

; Problem 1e
(define (andmap-cps pred ls k)
    (if (null? ls)
        (apply-continuation k #t)
        (apply-continuation pred (car ls)
            (lambda (x)
                (if x (apply-continuation andmap-cps pred (cdr ls) k) (apply-continuation k #f))))))

(define (or-cps a b k)
    (apply-continuation k (or a b)))

; Problem 1f
(define (cps-snlist-recur base item-proc-cps ls-proc-cps)
    (letrec ((helper (lambda (ls k)
        (if (null? ls)
            (apply-continuation k base)
            (let ((c (car ls)))
                (or-cps (pair? c) (null? c)
                    (lambda (x)
                        (if x
                            (apply-continuation helper c
                                (lambda (y) (apply-continuation helper (cdr ls) (lambda (z) (apply-continuation ls-proc-cps y z k)))))
                            (apply-continuation helper (cdr ls) (lambda (y) (apply-continuation item-proc-cps c y k)))))))))))
        helper))

;(define +-cps
;    (lambda (a b k)
;        (apply-continuation k (+ a b))))

;(define sn-list-sum-cps
;    (cps-snlist-recur 0 +-cps +-cps))

(define (reverse-cps x y k)
    (apply-continuation k (append y (list x))))

(define sn-list-reverse-cps
    (cps-snlist-recur '() reverse-cps reverse-cps))