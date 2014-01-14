;;;; braid-hunchentoot.asd

(asdf:defsystem #:braid-hunchentoot
  :version "0.0.1"
  :author "Rob Blackwell"
  :description ""
  :serial t
  :depends-on (#:alexandria 
	       #:hunchentoot
	       #:braid)
  :components ((:file "package")
	       (:file "braid-hunchentoot")))
