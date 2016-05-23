(define (fib n)
	(cond
		((= n 2) 1)
		((<= n 1) 1)
		(else (+ (fib (- n 1)) (fib (- n 2))))))

(define (simple n)
	(let ((tick-count 0) (result 0))
		(let loop ((eng (make-engine (lambda () (fib n)))))
			(if (procedure? eng)
				(loop (eng 20
					(lambda (t v)
						(display "ticks taken: ")
						(display (+ t tick-count))
						(display "\n")
						v)
					(lambda (x) (set! tick-count (+ tick-count 20)) x)))
				(set! result eng))) result))