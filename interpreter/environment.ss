;-----------------------+
;                       |
;      ENVIRONMENT      |
;                       |
;-----------------------+

(define (empty-env)
 (box (empty-env-record)))

(define (extend-env syms vals env)
 (box (extended-env-record syms vals env)))

; succeed and fail are procedures applied if the var is or isn't found, respectively.
(define (apply-env env sym k)
   (cases environment (unbox env)
      (empty-env-record ()
         (apply-k k (eopl:error 'apply-env "variable ~s is not bound" sym)))
      (extended-env-record (syms vals env)
         (list-find-position sym syms (extended-env-record-k env vals sym k)))))