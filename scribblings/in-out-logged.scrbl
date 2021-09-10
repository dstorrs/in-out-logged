#lang scribble/manual


@title{in-out-logged}
@author{David K. Storrs}

@defmodule[in-out-logged]

@section{DESCRIPTION}

Macro:  in/out-logged

Wraps a chunk of code such that log messages are output when the code begins and ends.

Returns the result of the code.  

in/out-logged can be given a series of arguments that are key/value pairs.  These will be
displayed in the 'in' and 'out messages.  A good use case for this is to output the
current-inexact-nanoseconds to get a rough approximation of how long the code takes when
you don't want to do a full profiler run.

Instead of providing key/value pairs you can use the #:with keyword to provide a format
string and a series of values to format into it.

@section{SYNOPSIS}

@racketblock[
 (in/out-logged (#:to <logger-name> #:at <logger-level> <key/value pair> ...)
   code ...)
   
 (in/out-logged (#:to <logger-name> #:at <logger-level> #:with <format-str> <value> ...)
   code ...)
	      ]
	      
All keywords are optional but #:with must be last if it appears.

By default output is sent to (current-logger) at 'debug level.

Valid levels are as per the Racket logging system.  In decreasing order of importance:

@racketblock[
  'fatal, 'error, 'warning, 'info, and 'debug.
]

@section{EXAMPLES}

@racketblock[
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
]

When run as follows:

  @racketblock[PLTSTDERR="debug@foo debug@bar error" racket main.rkt]

This outputs:

@racketblock[
For the following tests, output is sent to (current-logger) at default
level (i.e. 'debug)

return literal
'ok

on-complete function
foo: in on-complete
6


For the following tests, we call (on-complete) and output is
explicitly sent to foo-logger.  Obviously you could instead
parameterize foo-logger into current-logger.

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


For the following tests, we call (on-complete), output is explicitly
sent to foo-logger, and we include arguments to be displayed
in the 'entering' message

default format style
foo: entering on-complete. args:
	time     	1631296675
	thread-id	17
foo: in on-complete
foo: leaving on-complete. args:
	time     	1631296675
	thread-id	17
6

same as above, reversed order of keywords
foo: entering on-complete. args:
	time	1631296675879.532
foo: in on-complete
foo: leaving on-complete. args:
	time	1631296675879.532
8

using bar-logger, using a specified format string
bar: entering on-complete. time is: 1631296675, username is: bob.
foo: in on-complete
bar: leaving on-complete. time is: 1631296675, username is: bob.
6


Testing multiple value return
entering values. time is: 1631296675905.897, username is: bob.
leaving values. time is: 1631296675905.909, username is: bob.
1
2
]