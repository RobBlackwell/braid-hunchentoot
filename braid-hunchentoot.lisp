;;;; braid-hunchentoot.lisp

(in-package #:braid-hunchentoot)

(defun hunchentoot-request-to-braid-request (request)
  "Converts a hunchentoot request object to a braid request."
  (braid:make-request
	 :uri (hunchentoot:request-uri request) 
	 :request-method (hunchentoot:request-method request) 
	 :headers (alist-plist (hunchentoot:headers-in request))
	 :body (hunchentoot:raw-post-data :request request)))

(defun ensure-integer (x)
	""
	(if (stringp x)
			(parse-integer x)
			x))

(defun braid-response-to-hunchentoot-reply (response reply)
  "Converts a braid response object to a hunchentoot reply object."
	(loop for (name value) on (headers response) by #'cddr do 
			 (if (eq name :content-length)
					 (setf (hunchentoot:header-out name reply) (ensure-integer value))
					 (setf (hunchentoot:header-out name reply) value)))
  (setf (hunchentoot:return-code reply) (status response))
  (body response))

(defclass braid-acceptor (hunchentoot:easy-acceptor)
  ((handler :initarg :handler)))

(defmethod handler ((self braid-acceptor))
  (let ((h (slot-value self 'handler)))
    (if (symbolp h)
	(symbol-function h)
	h)))

(defmethod hunchentoot:acceptor-dispatch-request ((acceptor braid-acceptor) request)
  (let ((response
					(funcall (handler acceptor) 
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












