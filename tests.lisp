(ql:quickload "braid-hunchentoot")

(defun my-request-handler (request)
	(braid:make-response :body "Hello World from Braid on Hunchentoot."))

(defun test1 ()
	(braid-hunchentoot:run-hunchentoot 'my-request-handler))

;; (test1)
;; Now browse to 8080

;; (braid-hunchentoot:stop-hunchentoot)
