#lang racket/base

(require racket/format  racket/string (for-syntax racket/base  syntax/parse))


(provide (all-defined-out))

(define-syntax (in/out-logged stx)
  (define-splicing-syntax-class k/v
    (pattern (~seq k v)))
  (syntax-parse stx
    [(_ (name:str (~alt (~optional (~seq #:to logger:expr))
                        (~optional (~seq #:at level))
                        (~once (~seq #:with format-str:str)))
                  ...
                  data ...)
        code:expr ...)
     #'(let ()
         (log-message (~? logger (current-logger)) (~? level 'debug)
                      (format (format "entering ~a. ~a" name format-str)
                              data ...))
         (begin0
             (let () code ...)
           (log-message (~? logger (current-logger)) (~? level 'debug)
                        (format (format "leaving ~a. ~a" name format-str)
                                data ...))))]

    [(_ (name:str (~alt (~optional (~seq #:to logger:expr))
                        (~optional (~seq #:at level)))
                  ...
                  data:k/v ...)
        code:expr ...)
     #'(let ([data-str (let* ([keys  (list data.k ...)]
                              [vals  (list data.v ...)]
                              [width (if (null? keys)
                                         0
                                         (apply max (map string-length keys)))])
                         (cond [(null? keys) ""]
                               [else
                                (string-join
                                 (for/list ([k keys]
                                            [v vals])
                                   (~a #:separator "\t" "" (~a k #:width width) v))
                                 "\n")]))])
         (log-message (~? logger (current-logger)) (~? level 'debug)
                      (format "entering ~a~a"
                              name
                              (if (non-empty-string? data-str)
                                  (format ". args:\n~a" data-str)
                                  "")))
         (begin0
             (let () code ...)
           (log-message (~? logger (current-logger)) (~? level 'debug)
                        (format "leaving ~a~a"
                                name
                                (if (non-empty-string? data-str)
                                    (format ". args:\n~a" data-str)
                                    "")))))]))

(module+ main
  (define-logger foo)
  (define-logger bar)

  (define (on-complete op . args)
    (log-foo-debug "in on-complete")
    (apply op args))


  (displayln "For the following tests, output is sent to (current-logger) at default
level (i.e. 'debug)\n")

  (displayln "return literal")
  (in/out-logged ("literal") 'ok)

  (displayln "\non-complete function")
  (in/out-logged ("on-complete") (on-complete + 1 2 3))

  (displayln "\n\nFor the following tests, we call (on-complete) and output is
explicitly sent to foo-logger.  Obviously you could instead
parameterize foo-logger into current-logger.\n")

  (in/out-logged ("on-complete" #:to foo-logger) (on-complete + 1 2 3))

  (displayln "\n#:at 'info level")
  (in/out-logged ("on-complete" #:to foo-logger #:at 'info) (on-complete + 1 2 3))

  (displayln "\nvarying order of keywords")
  (in/out-logged ("on-complete" #:at 'debug #:to foo-logger ) (on-complete + 1 2 3))

  (displayln "\n\nFor the following tests, we call (on-complete), output is explicitly
sent to foo-logger, and we include arguments to be displayed
in the 'entering' message")

  (displayln "\ndefault format style")
  (in/out-logged ("on-complete" #:at 'debug #:to foo-logger
                  "time" (current-seconds)
                  "thread-id" 17)
                 (on-complete + 1 2 3))

  (displayln "\nsame as above, reversed order of keywords")
  (in/out-logged ("on-complete"
                  #:to foo-logger
                  #:at 'debug
                  "time" (current-inexact-milliseconds))
                 (on-complete + 1 2 5))

  (displayln "\nusing bar-logger, using a specified format string")
  (in/out-logged ("on-complete"  #:to bar-logger #:at 'debug #:with "time is: ~a, username is: ~a." (current-seconds) 'bob)
                 (on-complete + 1 2 3))

  (displayln "\n\nTesting multiple value return")

  (in/out-logged ("values"
                  #:at 'error
                  #:with "time is: ~a, username is: ~a." (current-inexact-milliseconds) 'bob)
                 (values 1 2))
  )
