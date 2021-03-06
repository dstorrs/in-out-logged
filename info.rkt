#lang info

(define collection "in-out-logged")
(define deps '("base"))
(define build-deps '("sandbox-lib" "scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/in-out-logged.scrbl" ())))
(define pkg-desc "Run some code with log messages before and after")
(define pkg-authors '("David K. Storrs"))

(define test-omit-paths '("test.rkt"))

(define version "0.4")
