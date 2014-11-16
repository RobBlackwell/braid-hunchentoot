(ql:quickload "braid-hunchentoot")

(defun my-handler (request) (braid:make-response :body "Hello World"))

(defun my-handler (request) "hello Rob")

(defun my-handler (request)
	(braid:make-response :body (format nil "~a" request)))

(braid-hunchentoot:run-hunchentoot 'my-handler)

;; Now browse to 8080

;; (braid-hunchentoot:stop-hunchentoot)
