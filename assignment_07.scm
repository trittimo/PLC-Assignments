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

(define (get-nodes nodes ls)
	(define (get-node s ls)
		(if (equal? s (caar ls))
			(car ls)
			(get-node s (cdr ls))))
	(map (lambda (x) (get-node x ls)) (cadr nodes)))

(define (check v ls)
	(if (null? ls)
		#t
		(if (member (car ls) v)
			(check v (cdr ls))
			#f)))

(define (connected? ls)
	(let dfs ([stack (list (car ls))] [visited '()])
		(if (null? stack) ; we're done
			(check visited ls)
			(let ([n (car stack)])
				(if (member n visited)
					(dfs (cdr stack) visited)
					(dfs (append (get-nodes n ls) stack) (cons n visited)))))))


(define (reverse-it ls)
	(define (rev ls newlist)
		(if (null? ls)
			newlist
			(rev (cdr ls) (cons (car ls) newlist))))
	(rev ls '()))

(define (empty-BST)
	'())

(define (empty-BST? obj)
	(null? obj))


(define (BST-insert num bst)
	(if (null? bst)
		(list num '() '())
		(cond
			((< num (car bst))
				(list (car bst) (BST-insert num (cadr bst)) (caddr bst)))
			((> num (car bst)) (list (car bst) (cadr bst) (BST-insert num (caddr bst))))
			((= num (car bst)) bst))))

(define (BST-util? obj smallest largest)
		(and (list? obj)
			(or (empty-BST? obj)
				(and (number? (car obj))
					 (= 3 (length obj))
					 (list? (cadr obj))
					 (list? (caddr obj))
					 (> (car obj) smallest)
					 (< (car obj) largest)
					 (BST-util? (cadr obj) smallest (car obj))
					 (BST-util? (caddr obj) (car obj) largest)))))

(define (BST? obj)
	(and (list? obj)
		(or (empty-BST? obj)
			(and (number? (car obj))
				 (= 3 (length obj))
				 (list? (cadr obj))
				 (list? (caddr obj))
				 (BST-util? (cadr obj) -999999 (car obj)) 
				 (BST-util? (caddr obj) (car obj) 999999)))))

(define (BST-element bst)
	(car bst))
(define (BST-left bst)
	(cadr bst))
(define (BST-right bst)
	(caddr bst))

(define (BST-insert-nodes bst nums)
	(if (null? nums)
		bst
		(BST-insert-nodes (BST-insert (car nums) bst) (cdr nums))))

(define (BST-contains? bst num)
	(cond
		((null? bst) #f)
		((= (car bst) num) #t)
		((< (car bst) num) (BST-contains? (caddr bst) num))
		((> (car bst) num) (BST-contains? (cadr bst) num))))

(define (BST-inorder bst)
	(if (null? bst)
		'()
		(append (BST-inorder (BST-left bst)) (cons (car bst) (BST-inorder (BST-right bst))))))

(define (map-by-position fn-list arg-list)
	(map (lambda (x y) (x y)) fn-list arg-list))

(define (bt-leaf-sum T)
	(let sum ([E T] [accum 0])
		(cond
			((number? E) (+ accum T))
			((list? E) (+ accum (bt-leaf-sum (cadr E)) (bt-leaf-sum (caddr E)))))))

(define (bt-inorder-list T)
	(cond
		((number? T) '())
		((list? T) (append (bt-inorder-list (cadr T)) (cons (car T) (bt-inorder-list (caddr T)))))))

(define (bt-max T)
	(if (number? T) T
	(let get-max ([E T] [largest -99999])
		(cond
			((and (number? E) (> E largest)) E)
			((number? E) largest)
			((list? E) (max (get-max (cadr E) largest) (get-max (caddr E) largest)))))))

(define n? number?) (define l? list?)

(define (biggest a b) (if (> (cadr a) (cadr b)) a b))

(define (larger a b sym)
	(cond
		((and (n? a) (l? b))
			(if (> (+ a (cadr b)) (cadr b))
				(list sym (+ a (cadr b)))
				b))
		((and (l? a) (n? b))
			(if (> (+ (cadr a) b) (cadr a))
				(list sym (+ (cadr a) b))
				a))
		(else
			(if (> (+ (cadr a) (cadr b)) (cadr (biggest a b)))
				(list sym (+ (cadr a) (cadr b)))
				(biggest a b)))))

(define (bt-max-interior T)
	(define (max-rec t h)
		(cond
			((n? t) (list h t))
			((and (n? (cadr t)) (n? (caddr t)))
				(list (car t) (+ (cadr t) (caddr t))))
			((and (l? (cadr t)) (n? (caddr t)))
				(larger (max-rec (cadr t) h) (caddr t) (car t)))
			((and (n? (cadr t)) (l? (caddr t)))
				(larger (cadr t) (max-rec (caddr t) h) (car t)))
			(else (larger (max-rec (cadr t) h) (max-rec (caddr t) h) (car t)))))
	(cond
		((and (n? (cadr T)) (n? (caddr T))) (car T))
		((and (n? (cadr T)) (l? (caddr T))) (car (max-rec (caddr T) (car (caddr T)))))
		((and (l? (cadr T)) (n? (caddr T))) (car (max-rec (cadr T) (car (cadr T)))))
		(else
			(let ([l (max-rec (cadr T) (car (cadr T)))] [r (max-rec (caddr T) (car (caddr T)))])
				(if (>= (cadr l) (cadr r)) (car l) (car r))))))