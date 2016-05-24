(load "chez-init.ss")

(define (load-all)
	(load "helpers.ss")
	(load "datatypes.ss")
	(load "parse.ss")
	(load "syntax-expansion.ss")
	(load "continuations.ss")
	(load "environment.ss")
	(load "interpreter.ss"))

(define l load-all)

(define (test test-number)
	(load (string-append "../tests/A" (number->string test-number) "-test-code.ss"))
	(l)
	(r))

(load-all)