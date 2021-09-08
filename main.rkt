#lang racket/base

(require  racket/string (for-syntax racket/base  syntax/parse))


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
         (define result (let () code ...))
         (log-message (~? logger (current-logger)) (~? level 'debug)
                      (format "leaving ~a" name))
         result)]

    [(_ (name:str (~alt (~optional (~seq #:to logger:expr))
                        (~optional (~seq #:at level)))
                  ...
                  data:k/v ...)
        code:expr ...)
     #'(let ([data-str (string-join (for/list ([k (list data.k ...)]
                                               [v (list data.v ...)])
                                      (format "~a  ~a" k v))
                                    "\n")])
         (log-message (~? logger (current-logger)) (~? level 'debug)
                      (format "entering ~a~a"
                              name
                              (if (non-empty-string? data-str)
                                  (format ". args:\n~a" data-str)
                                  "")))
         (define result (let () code ...))
         (log-message (~? logger (current-logger)) (~? level 'debug)
                      (format "leaving ~a" name))
         result)]))

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
  (in/out-logged ("on-complete" #:at 'debug #:to foo-logger 'a 1 'b 2 'c 3)
                 (on-complete + 1 2 3))

  (displayln "\nsame as above, reversed order of keywords")
  (in/out-logged ("on-complete"  #:to foo-logger #:at 'debug  'a 1 'b 2 'c 3)
                 (on-complete + 1 2 3))

  (displayln "\nusing bar-logger, using a specified format string")
  (in/out-logged ("on-complete"  #:to bar-logger #:at 'debug #:with "data is: ~a ~a" 'a 1)
                 (on-complete + 1 2 3))
  )
