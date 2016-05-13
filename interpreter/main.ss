(load "chez-init.ss")

(define (load-all)
	(load "helpers.ss")
	(load "datatypes.ss")
	(load "parse.ss")
	(load "syntax-expansion.ss")
	(load "environment.ss")
	(load "interpreter.ss"))

(define l load-all)

(load-all)