#lang info
(define collection "in-out-logged")
(define deps '("base" racket/format racket/string ))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/in-out-logged.scrbl" ())))
(define pkg-desc "Run some code with log messages before and after")
(define version "0.1")
(define pkg-authors '("David K. Storrs"))

(define test-omit-paths '("test.rkt"))
