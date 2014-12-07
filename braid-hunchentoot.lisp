;;;; braid-hunchentoot.lisp

(in-package #:braid-hunchentoot)

(defun hunchentoot-request-to-braid-request (request)
  "Converts a hunchentoot request object to a braid request."
  (braid:make-http-request
   :uri (hunchentoot:request-uri request) 
   :method (hunchentoot:request-method request) 
   :headers (alist-plist (hunchentoot:headers-in request))
   :body (hunchentoot:raw-post-data :request request)))

(defun ensure-integer (x)
  ""
  (if (stringp x)
      (parse-integer x)
      x))

(defun braid-response-to-hunchentoot-reply (response reply)
  "Converts a braid response object to a hunchentoot reply object."
  (loop for (name value) on (braid:http-message-headers response) by #'cddr do 
       (if (eq name :content-length)
	   (setf (hunchentoot:header-out name reply) (ensure-integer value))
	   (setf (hunchentoot:header-out name reply) value)))
  (setf (hunchentoot:return-code reply) (braid:http-response-status response))
  (braid:http-message-body response))

(defclass braid-acceptor (hunchentoot:easy-acceptor)
  ((request-handler :initarg :request-handler :accessor request-handler)))

(defmethod hunchentoot:acceptor-dispatch-request ((acceptor braid-acceptor) request)
  (let ((response
	 (funcall (request-handler acceptor) 
		  (hunchentoot-request-to-braid-request request))))
    (braid-response-to-hunchentoot-reply response hunchentoot:*reply*)))
  
(defvar *default-acceptor* nil)

(defun run-hunchentoot (request-handler &key (port 8080))
  "Runs a Hunchentoot web server to serve the given handler."
  (setf *default-acceptor* (make-instance 'braid-acceptor :request-handler request-handler :port port))
  (hunchentoot:start *default-acceptor*))

(defun stop-hunchentoot (&optional (acceptor *default-acceptor*))
  "Stops the Hunchentoot web server."
  (hunchentoot:stop acceptor))












