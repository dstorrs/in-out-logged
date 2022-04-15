#lang racket/base

(require racket/format
         racket/string
         racket/function
         racket/sequence
         (for-syntax racket/base syntax/parse)
         )

(provide (all-defined-out))

(define-syntax (in/out-logged stx)
  (define-splicing-syntax-class non-kw-argument
    #:description "non-keyword argument"
    (pattern (~seq (~peek (~not x:keyword)) val)))

  (syntax-parse stx
    [(_ (func-name:expr (~alt (~optional (~seq #:to logger:expr))
                             (~optional (~seq #:at level))
                             (~optional (~seq #:with fstr:str))
                             (~optional (~seq #:results (r1 result-names ...)))) ; 1+ names
                       ...
                       data:non-kw-argument ...)
        code:expr ...)
     #'(let* ([lst  (list data.val ...)]
              [format-str "~a ~a. ~a~a"] ; entering/leaving, func-name, results, args
              [arg-str (~? (apply (curry format fstr) lst)
                           (let* ([item lst]
                                  [len  (length lst)])
                             (cond [(zero? len) ""] ; no arguments
                                   [(odd? len)  (~a " " (apply (curry ~a #:separator " ") lst))]
                                   [(even? len) ; key/value pairs
                                    (let-values ([(keys vals width)
                                                  (for/fold ([keys '()]
                                                             [vals '()]
                                                             [width 0])
                                                            ([v (in-slice 2 lst)])
                                                    (define k (~a (car v)))
                                                    (values (cons k keys)
                                                            (cons (cadr v) vals)
                                                            (max width (string-length k))))])
                                      (string-join
                                       (cons ""
                                             (for/list ([k (reverse keys)]
                                                        [v (reverse vals)])
                                               (~a #:separator "\t" "" (~a k #:width width) v)))
                                       "\n"))])))]
              )
         (~? (let ()
               (log-message (~? logger (current-logger))
                            (~? level 'debug)
                            (string-trim (format format-str "entering" func-name "" arg-str)))
               (define-values (r1 result-names ...) (let () code ...))
               (log-message (~? logger (current-logger))
                            (~? level 'debug)
                            (string-trim (format format-str
                                    "leaving"
                                    func-name
                                    (~?
                                     (string-join
                                      (cons "results: "
                                            (for/list ([item (list r1 result-names ...)])
                                              (~v item))))
                                     "")
                                                 arg-str)))
               (values r1 result-names ...))



             (begin
               (log-message (~? logger (current-logger))
                            (~? level 'debug)
                            (format format-str "entering" func-name "" arg-str))
               (begin0
                   (let () code ...)
                 (log-message (~? logger (current-logger))
                              (~? level 'debug)
                              (format format-str
                                      "leaving"
                                      func-name
                                      ""
                                      arg-str)))))
         )]))

(module+ main
  (define-logger foo)
  (define-logger bar)

  (log-foo-debug "STARTING THE TEST RUN")
  (define (on-complete op . args)
    (log-foo-debug "in on-complete")
    (apply op args))


  (displayln "For the following tests, output is sent to (current-logger) at default level (i.e. 'debug).  This means you won't see any output unless you're running with, e.g. PLTSTDERR='debug', in which case you'll get debug output from the GC system and other racket internals\n")

  (in/out-logged
   ("show-results 1 value, verifying single execution only" #:to foo-logger)
   ((λ ()
      (displayln "testing for multiple eval")
      'x)))

  (in/out-logged
   ("show-results 1 value, #:results (x) before #:to" #:results (x) #:to foo-logger )
   'x)

  (in/out-logged
   ("show-results 2 vals" #:to foo-logger #:results (x y))
   (values 'x 'y))

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
  (in/out-logged ("on-complete"  #:to bar-logger #:at 'debug #:with "time is: ~a, username is: ~a." (current-inexact-milliseconds) 'bob)
                 (on-complete + 1 2 3))

  (displayln "\n\nTesting multiple value return")

  (in/out-logged ("values"
                  #:at 'debug
                  #:with "time is: ~a, username is: ~a." (current-inexact-milliseconds) 'bob)
                 (values 1 2))
  ) ; module
