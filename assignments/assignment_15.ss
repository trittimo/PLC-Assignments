; Why is the time savings (compared to fib) for the above definition of fib-memo less dramatic than the time
;   savings for the definition of fib-memo in the Day 21 PowerPoint slides?
; This is because we don't store a "max" n that fib has run, which means that we are required to check 
;   the hashmap (which takes O(n) time) for a key, regardless of the input. By comparison, the fib-memo from 
;   day 21 ppt stored the maximum run n value, so it took only O(1) to check if we needed to run fib(n)

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

; Problem 1f helper
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

(define (reverse-cps x y k)
    (apply-continuation k (append y (list x))))

(define sn-list-reverse-cps
    (cps-snlist-recur '() reverse-cps reverse-cps))

(define sn-list-depth-cps
    (cps-snlist-recur 1
        (lambda (x y k)
            (apply-continuation k y))
        (lambda (x y k)
            (apply-continuation k (max (add1 x) y)))))

(define (sn-list-occur-cps item ls proc)
    ((cps-snlist-recur 0
        (lambda (x y k)
            (if (eqv? x item)
                (apply-continuation k (add1 y))
                (apply-continuation k y)))
        (lambda (x y k)
            (apply-continuation k (+ x y)))) ls proc))

; Problem 2
(define (memoize proc hash equiv?)
    (let ((table (make-hashtable hash equiv?)))
        (lambda args
            (if (hashtable-contains? table args)
                (hashtable-ref table args "Invalid Index!")
                (let ((result (apply proc args)))
                    (hashtable-set! table args result)
                    result)))))


; Problem 3 helper
(define-syntax with-values
    (syntax-rules ()
        ((_ expr consumer)
            (call-with-values
                (lambda () expr)
                consumer))))

(define-syntax mv-let
    (syntax-rules ()
        ((_ ((x ...) e0) e1 e2 ...)
            (with-values e0 (lambda (x ...) e1 e2 ...)))))

; Problem 3
(define (subst-leftmost-helper new old slist comp)
    (cond
        ((symbol? slist) (if (comp slist old) (values #t new) (values #f slist)))
        ((null? slist) (values #f '()))
        (else
            (mv-let ((done newls) (subst-leftmost-helper new old (car slist) comp))
                (if done
                    (values #t (cons newls (cdr slist)))
                    (mv-let ((done continue) (subst-leftmost-helper new old (cdr slist) comp))
                        (values done (cons newls continue))))))))

(define (subst-leftmost new old slist comp)
    (mv-let ((replaced newls) (subst-leftmost-helper new old slist comp))
        newls))