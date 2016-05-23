;;;      COROUTINE EXAMPLES
;;;   In the file coroutines.ss

(define call/cc call-with-current-continuation)

(define example  ; Adapted from EoPL, first edition, Chapter 9
  (lambda ()
    (call/cc
     (lambda  (return-cont)
       (let ((co1 'undefined)
             (co2 'undefined))
         (set! co1 (make-coroutine
                    (lambda (init-val1)
                      (display " 1-a ")
                      (display init-val1)
                      (set! init-val1
                            (resume co2 (+ 1 init-val1)))
                      (display " 1-b ")
                      (display init-val1)
                      (set! init-val1
                            (resume co2 (+ 1 init-val1)))
	                    (display " 1-c ")
                      (return-cont init-val1))))
         (set! co2 (make-coroutine
                    (lambda  (init-val2)
                      (display " 2-a ")
                      (display init-val2)
                      (set! init-val2
                            (resume co1 (+ 1 init-val2)))
                      (display " 2-b ")
                      (display init-val2)
                      (set! init-val2
                            (resume co1 (+ 1 init-val2)))
	                    (display " 2-c "))))
         (co1 33))))))
;; >(example)
;;  1-a 33 2-a 34 1-b 35 2-b 36 1-c 37

;;;                        COROUTINE IMPLEMENTATION

(define resume 'resume-undefined)

(define make-coroutine
  (lambda (body)
    (let ((local-continuation 'local-continuation-undefined))
      (letrec
          ((newcoroutine
            (lambda  (value) (local-continuation value)))
           (localresume
            (lambda  (continuation value)
              (let ((value (call/cc (lambda (k)
                                      (set! local-continuation k)
                                      (continuation value)))))
                (set! resume localresume)
                value))))
        (call/cc
         (lambda (exit)
           (body (localresume exit newcoroutine))
           (error 'co-routine "fell off end of coroutine")))))))

;; Using coroutines to determine whether two binary trees (Scheme s-lists) 
;; have the same fringe, (I.e. Does (flatten T1) equal (flatten T2)?)
;; without building a new data structure containing all of the entries.

(define make-sf-coroutine
  (lambda (driver tree)
    (make-coroutine
     (lambda (init-value)
       (letrec ((traverse
                 (lambda (tree)
                   (if (pair? tree)
                       (begin
                         (traverse (car tree))
                         (if (pair? (cdr tree))
                             (traverse (cdr tree))))
                       (resume driver tree)))))
         (traverse tree)
         (resume driver #f))))))

(define same-fringe
  (lambda (tree1 tree2)
    (call/cc
     (lambda (return-cont)
       (let ((co1 '()) (co2 '()) (driver '()))
         (set! driver
               (make-coroutine
                (lambda (init-value)
                  (let loop ()
                    (let ((leaf1 (resume co1 'whocares))
                          (leaf2 (resume co2 'whocare2)))
                      (if (equal? leaf1 leaf2)
                          (if (eq? leaf1 #f) (return-cont #t) (loop))
                          (return-cont #f)))))))
         (set! co1 (make-sf-coroutine driver tree1))
         (set! co2 (make-sf-coroutine driver tree2))
         (driver 'Whatsittoya?))))))


;; > (same-fringe '((1) (2 ((3)))) '((1 (2) 3)))
;; #t
;; > (same-fringe '((1) (2 ((3)))) '((1 2 4)))
;; #f
;; > (same-fringe '(1 (2)) '(1))
;; #f
;; > (same-fringe '() '())
;; #t
;; > (same-fringe '() '(()))
;; #t
;; > (same-fringe '((1 (((2))))) '((((((1))) 2))))
;; #t