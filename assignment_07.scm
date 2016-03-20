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

(define (qsort pred ls)
	(define (partition ls comp)
		(cond
			([null? ls] '()) ; if null, we're done -> everything will cons to the empty list
			([comp (car ls)] (cons (car ls) (partition (cdr ls) comp))) ; if first (comp) piv, append it to the partition
			(else (partition (cdr ls) comp)))) ; otherwise, recursively call with the cdr : the car is discarded 
											   ; and will be dealt with in the other calls to partition from qsort

	; First, we check for empty list : if so, we're done
	; Otherwise, we take the car as the pivot, and then we want to use the following
	; take a to be the qsort of the partition of ls, using each element < pivot. discards all elements >= than pivot.
	; take b to be the partition of the ls, using each element = pivot. discards all other elements.
	; take c to be qsort of partition of ls, using each element > pivot. discards all elements <= than pivot.
	; take d to be (append a b)
	; the final result is (append d c)
	; this works since we discard all elements >= pivot and, and then sort the elements left in. 
	; We then append the elements equal to the pivot (their order doesn't matter, so no qsort needed)
	; Lastly, we discard all elements <= pivot, and then sort the elements left in. Once appended in the proper order,
	; we have our final list

	(if (null? ls) '()
		(let ([pivot (car ls)])
			(append 
				(append 
					(qsort pred (partition ls (lambda (x) (pred x pivot))))
					(partition ls (lambda (x) (equal? x pivot))))
					(qsort pred (partition ls (lambda (x) (pred pivot x))))))))