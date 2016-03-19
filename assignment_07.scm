; copies into a vector, starting at index k and going until k = len-1, 
; and using (refproc k) to get the element to insert at index k
(define (vector-copy v k len refproc)
	(if (< k len)
		(begin
			(vector-set! v k (refproc k))
			(vector-copy v (add1 k) len refproc))))

(define (vector-append-list v ls)
	(let ([newv (make-vector (+ (length ls) (vector-length v)))])
		(vector-copy newv 0 (vector-length v) (lambda (k) (vector-ref v k)))
		(vector-copy newv (vector-length v) (vector-length newv) (lambda (k) (list-ref ls (- k (vector-length v)))))
		newv))