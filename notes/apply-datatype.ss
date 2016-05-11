(load "chez-init.ss")

(define-datatype continuation continuation?
    (init-k)
    (append-k (l1 (list-of symbol?)) (k continuation?))

(define (append-k k . v)
    (cases continuation k
        (init-k ()
            (pretty-print v)
            (read-flatten-print))
        (append-k (l1 k) (apply-k k (cons (car l1) v)))

(define append-cps
    (lambda (l1 l2 k)
        (if (null? l1)
            (apply-k k l2)
            (append-cps (cdr l1) l2 (append-k l1 k)))))