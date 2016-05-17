;-------------------+
;                   |
;      HELPERS      |
;                   |
;-------------------+

; General helpers to make thing a bit more sane
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)
(define 4th cadddr)

; Used in define-datatype where we don't actually care about the type
(define (scheme-value? x) #t)

; Used in parse-exp where we check to ensure that certain expressions are lists where length = 2
(define (list-is-2-long? ls) (and (list? ls) (= (length ls) 2)))

; Gets the 'list' part of a list-pair [i.e. (get-list '(a b c . d)) -> (a b c)]
(define (get-list ls)
   (if (pair? ls)
      (cons (car ls) (get-list (cdr ls)))
      '()))

; Gets the 'pair' part of a list-pair [i.e. (get-last '(a b c . d)) -> d]
(define (get-last ls)
  (if (pair? ls)
   (get-last (cdr ls))
     ls))

; Helper for named-let expansion - generates a list of temporary vars (i.e. (t0 t1 t2 t3)) of the given length
(define (generate-temporaries len)
   (map (lambda (x)
      (string->symbol (string-append "t" (number->string x)))) (iota len)))

(define compose
	(case-lambda
		(() (lambda (x) x))
		((first . rest)
		(let ((composed-rest (apply compose rest)))
		(lambda (x) (first (composed-rest x)))))))

; Returns a lambda that will apply car/cdr to the list
(define (make-c...r s)
	(apply compose (map (lambda (x)
    (cond
      ((eq? #\a x) car)
      ((eq? #\d x) cadr)
      (else (eopl:error 'make-c...r "Unexpected character encountered in make-c...r")))) (string->list s))))