;;;; braid-hunchentoot.lisp

(in-package #:braid-hunchentoot)

(defun hunchentoot-request-to-braid-request (request)
  "Converts a hunchentoot request object to a braid request."
  (braid:make-request (hunchentoot:request-uri request) 
		     :method (hunchentoot:request-method request) 
		     :headers (hunchentoot:headers-in request)
		     :body (hunchentoot:raw-post-data :request request)))

(defun braid-response-to-hunchentoot-reply (response reply)
  "Converts a braid response object to a hunchentoot reply object."
  (loop for (name . value) in (message-headers response) do
       (setf (hunchentoot:header-out name hunchentoot:reply reply) value))
  (setf (hunchentoot:return-code reply) (response-status-code response))
  (message-body response))

(defclass braid-acceptor (hunchentoot:easy-acceptor)
  ((handler :initarg :handler)))

(defmethod handler ((self braid-acceptor))
  (let ((h (slot-value self 'handler)))
    (if (symbolp h)
	(symbol-function h)
	h)))

(defmethod hunchentoot:acceptor-dispatch-request ((acceptor braid-acceptor) request)
  (let ((response (funcall (handler acceptor) 
			   (hunchentoot-request-to-braid-request request))))
    (braid-response-to-hunchentoot-reply response hunchentoot:*reply*)))
  
(defvar *default-acceptor* nil)

(defun run-hunchentoot (handler &key (port 8080))
  "Runs a Hunchentoot web server to serve the given handler."
  (setf *default-acceptor* (make-instance 'braid-acceptor :handler handler :port port))
  (hunchentoot:start *default-acceptor*))

(defun stop-hunchentoot (&optional (acceptor *default-acceptor*))
  "Stops the Hunchentoot web server."
  (hunchentoot:stop acceptor))











