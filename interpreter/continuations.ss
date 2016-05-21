(define-datatype continuation continuation?
      (identity)
      (test-k (then-exp expression?) (else-exp expression?) (env box?) (k continuation?))
      (rator-k (rands (list-of expression?)) (env box?) (k continuation?))
      (rands-k (proc-value scheme-value?) (k continuation?))
      (extended-env-record-k (env box?) (vals list?) (sym symbol?) (k continuation?)))

(define (apply-k k val)
   (cases continuation k
      (identity () val)
         (test-k (then-exp else-exp env k)
            (cases expression else-exp
               (empty-exp ()
                  (if val
                     (eval-exp then-exp env k)))
               (else
                  (if val
                     (eval-exp then-exp env k)
                     (eval-exp else-exp env k)))))
         (rator-k (rands env k)
            (eval-rands rands env (rands-k val k)))
         (rands-k (proc-value k)
            (apply-proc proc-value val k))
         (extended-env-record-k (env vals sym k)
            (if (number? val)
               (apply-k k (list-ref vals val))
               (apply-env env sym k)))
      (else (eopl:error 'apply-k "apply-k not implemented for form '~s'" k))))

;(define (map-cps proc ls k)
;      (if (null? ls)
;            (apply-k k ls)
;            (apply-k k proc (car ls)
;                  (lambda (x) (map-cps proc (cdr ls)
;                        (lambda (y) (apply-k k (cons x y))))))))