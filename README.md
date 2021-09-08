in-out-logged
=============

Run a chunk of code with log messages around it to announce when it begins and when it ends.  Return the result of the code.

```

  (define-logger foo)
  (define-logger bar)

  (define (on-complete op . args)
    (log-foo-debug "in on-complete")
    (apply op args))


  (displayln "For the following tests, output is sent to (current-logger) at default level (i.e. 'debug)")
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

  (displayln "\n\nFor the following tests, we call (on-complete), output is explicitly sent to foo-logger, and we include arguments to be displayed in the 'entering' message")
  
  (displayln "\ndefault format style")
  (in/out-logged ("on-complete" #:at 'debug #:to foo-logger 'a 1 'b 2 'c 3)
                 (on-complete + 1 2 3))

  (displayln "\nsame as above, reversed order of keywords")
  (in/out-logged ("on-complete"  #:to foo-logger #:at 'debug  'a 1 'b 2 'c 3)
                 (on-complete + 1 2 3))

  (displayln "\nusing bar-logger, using a specified format string")
  (in/out-logged ("on-complete"  #:to bar-logger #:at 'debug #:with "data is: ~a ~a" 'a 1)
                 (on-complete + 1 2 3))
  
  (displayln "\n\nTesting multiple value return")

  (in/out-logged ("multiple-return" #:to bar-logger) (values 1 2))
  
```

This produces the following output:

```
$ PLTSTDERR="debug@foo debug@bar error" racket main.rkt

For the following tests, output is sent to (current-logger) at default
level (i.e. 'debug) [NOTE for this README file:  We have set the 
default reporting	level to 'error' so no in/out logs will be shown but 
the logs	inside the on-complete function will be since they are sent 
to foo-logger.]

return literal
'ok

on-complete function
foo: in on-complete
6


For the following tests, we call (on-complete) and output is explicitly sent to foo-logger.  Obviously you could instead parameterize foo-logger into current-logger.

foo: entering on-complete
foo: in on-complete
foo: leaving on-complete
6

#:at 'info level
foo: entering on-complete
foo: in on-complete
foo: leaving on-complete
6

varying order of keywords
foo: entering on-complete
foo: in on-complete
foo: leaving on-complete
6


For the following tests, we call (on-complete), output is explicitly sent to foo-logger, and we include arguments to be displayed in the 'entering' message

default format style
foo: entering on-complete. args:
a  1
b  2
c  3
foo: in on-complete
foo: leaving on-complete
6

same as above, reversed order of keywords
foo: entering on-complete. args:
a  1
b  2
c  3
foo: in on-complete
foo: leaving on-complete
6

using bar-logger, using a specified format string
bar: entering on-complete. data is: a 1
foo: in on-complete
bar: leaving on-complete
6


Testing multiple value return
bar: entering multiple-return
bar: leaving multiple-return
1
2
```
