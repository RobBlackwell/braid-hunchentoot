braid-hunchentoot
=================

An adapter that allows Common Lisp braid middleware to be used with the
Hunchentoot web server.

See
[braid](https://github.com/RobBlackwell/braid).

	(ql:quickload "braid-hunchentoot")

	(defun my-request-handler (request) (braid:make-http-response :body "Hello World"))

	(braid-hunchentoot:run-hunchentoot 'my-request-handler)

	;; Now browse to 8080
	
THIS IS EXPERIMENTAL CODE THAT IS SUBJECT TO CHANGE. I welcome
feedback, but it's probably too early to consider including in
Quicklisp yet. That doesnt stop you trying it with quicklisp by using
[local-projects](http://www.quicklisp.org/beta/faq.html).

Rob Blackwell    
November 2014

