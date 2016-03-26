(define (sn-list-sum snlist)
	(fold-left (lambda (accum next)
		(if (list? next) (+ accum (sn-list-sum next)) (+ accum next))) 
	0 snlist))

(define (sn-list-map proc snlst)
	(fold-right (lambda (next new)
		(if (list? next)
			(cons (sn-list-map proc next) new)
			(cons (proc next) new))) '() snlst))

(define (sn-list-paren-count snlst)
	(+ 2 (fold-left (lambda (accum next)
		(if (list? next) (+ accum (sn-list-paren-count next)) accum)) 0 snlst)))

(define (sn-list-reverse snlst)
	(fold-left (lambda (new next)
		(if (list? next) (cons (sn-list-reverse next) new) (cons next new))) '() snlst))

(define (sn-list-occur s snlst)
	(fold-left (lambda (count next)
		(cond
			((list? next) (+ count (sn-list-occur s next)))
			((eq? s next) (add1 count))
			(else count))) 0 snlst))

(define (sn-list-depth slist)
	(cond
		((number? slist) 0)
		((symbol? slist) 0)
		((null? slist) 1)
		((list? slist) 
		(+ 1 (apply max (map sn-list-depth slist))))))

(define (bt-recur proc bt)
	(let ((left (cadr bt)) (right (caddr bt)) (node (car bt)))
		(cond
			((and (list? left) (list? right)) (list (bt-recur proc left) (bt-recur proc right)))
			((list? left) (list (bt-recur proc left) (proc right)))
			((list? right) (list (proc left) (bt-recur proc right)))
			(else (list (proc left) (proc right))))))

(define (bt-sum bt)
	(if (number? bt) bt
		(sn-list-sum (bt-recur (lambda (x) x) bt))))

;(define (bt-inorder bt)
;	