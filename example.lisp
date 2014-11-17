(ql:quickload "braid-hunchentoot")

(defun my-handler (request) (braid:make-response :body "Hello World"))

(defun my-handler (request) "Hello Rob")

(defun my-handler (request)
	(braid:make-response :body (format nil "~s" request)))

(defun my-handler (request)
	(braid:make-response :headers '(:content-type "application/json" :x-ms-version "2") :body "1")) 

(braid-hunchentoot:run-hunchentoot 'my-handler)

;; Now browse to 8080

;; (braid-hunchentoot:stop-hunchentoot)
