(define fact-acc
	(lambda (n acc)
			(if (zero? n)
					acc
					(fact-acc (- n 1)
						(* n acc)))))

(define apply-continuation
	(lambda (k v)
	(k v)))

(define fact-cps
	(lambda (n k)
		(if (zero? n)
			(apply-continuation k 1)
			(fact-cps (- n 1)
				(lambda (v) (apply-continuation k (* n v)))))))