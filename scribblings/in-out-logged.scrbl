#lang scribble/manual

@(require (for-label racket)
          racket/sandbox
          scribble/example)

@title{in-out-logged}
@author{David K. Storrs}

@defmodule[in-out-logged]

@section{Description}

Macro:  @racket[in/out-logged]

Wraps a chunk of code such that log messages are output when the code begins and ends.

Returns the result of the code.

@racket[in/out-logged] can optionally be given a series of key/value pairs.
These will be displayed in the `in' and `out' messages.  A good use case for
this is to output the @racket[current-inexact-nanoseconds] to get a rough
approximation of how long the code takes when you don't want to do a full
profiler run.

Instead of providing key/value pairs you can use the @racket[#:with] keyword to provide a
format string and a series of values to format into it.

You can also have the log messages include the return values of the wrapped code.

@section{Synopsis}

@(define eval
   (call-with-trusted-sandbox-configuration
    (lambda ()
      (parameterize ([sandbox-output 'string]
                     [sandbox-error-output 'string]
                     [sandbox-memory-limit 50])
        (make-evaluator 'racket)))))

@racketblock[
 (in/out-logged (#:to <logger-name>
                 #:at <logger-level>
                 #:results (id ...+)
                 <key/value pair> ...)
    code ...)

 (in/out-logged (#:to <logger-name>
                 #:at <logger-level>
                 #:results (id ...+)
                 #:with <format-str> <value> ...)
   code ...)
]

All keywords are optional but @racket[#:with] must be last if it appears.

By default output is sent to @racket[(current-logger)] at @racket['debug] level.

Valid levels are as per the Racket logging system.  In decreasing order of importance:

@racketblock[
  'fatal 'error 'warning 'info 'debug
]

@section{Examples}

@racketblock[

 (define-logger foo)

 (define (on-complete op . args)
   (log-foo-debug "message from inside on-complete")
   (apply op args))

 (code:comment "log around a literal value")
 > (in/out-logged ("literal") 'ok)
entering literal. 
leaving literal. 

 (code:comment "return the result of a function call")
 > (in/out-logged
     ("on-complete" #:at 'error #:to foo-logger)
     "a pointless string to show multiple expressions in the body"
     (on-complete + 1 2 3))
entering on-complete. 
message from inside on-complete
leaving on-complete. 

 > (in/out-logged ("on-complete, explicitly foo-logger" #:to foo-logger) (on-complete + 1 2 3))
foo: entering on-complete, explicitly foo-logger. 
message from inside on-complete
foo: leaving on-complete, explicitly foo-logger. 

 (code:comment "log in/out messages at 'info level")
 > (in/out-logged ("on-complete at 'info level" #:to foo-logger #:at 'info) (on-complete + 1 2 3))
foo: entering on-complete at 'info level. 
message from inside on-complete
foo: leaving on-complete at 'info level. 

 (code:comment "order of keywords doesn't matter")
 > (in/out-logged ("on-complete, vary keyword order" #:at 'debug #:to foo-logger )
                  (on-complete + 1 2 3))
foo: entering on-complete, vary keyword order.
message from inside on-complete
foo: leaving on-complete, vary keyword order.

 (code:comment "For convenience, we'll set foo as our default logger")
 > (current-logger foo-logger)

 (code:comment "include arguments to be displayed in the 'entering' message. By default they are shown as key/val pairs")
 > (in/out-logged ("on-complete with arguments"
                   "time" (current-seconds)
                   "thread-id" 17)
                  (on-complete + 1 2 3))
foo: entering on-complete with arguments. 
	time     	1642022093
	thread-id	17
foo: message from inside on-complete
foo: leaving on-complete with arguments. 
	time     	1642022093
	thread-id	17

 (code:comment "use a specified format string instead of the default key/value pairs")
 > (in/out-logged ("on-complete with an explicit format string"
                   #:with "time is: ~a, username is: ~a." (current-seconds) 'bob)
                  (on-complete + 1 2 3))
foo: entering on-complete with an explicit format string. time is: 1642022093, username is: bob.
foo: message from inside on-complete
foo: leaving on-complete with an explicit format string. time is: 1642022093, username is: bob.

 (code:comment "multiple value return is fine. also, let's use 'error level for demonstration")
 > (in/out-logged ("values at 'error level" #:at 'error)
                  (values 1 2))
foo: entering values at 'error level. 
foo: leaving values at 'error level. 

 (code:comment "display the results of the code in the 'leaving' message")
 > (in/out-logged ("values and show multiple returns" #:results (x y))
                  (values 1 2))
foo: entering values and show multiple returns.
foo: leaving values and show multiple returns. results: (values 1 2)

 (code:comment "display the results of the code in the 'leaving' message, only one result")
 > (in/out-logged ("values and show one return" #:results (x y))
                  'ok)
foo: entering values and show one return.
foo: leaving values and show one return. results: (values 'ok)


]
