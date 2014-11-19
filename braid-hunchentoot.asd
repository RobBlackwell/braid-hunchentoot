;;;; braid-hunchentoot.asd

(asdf:defsystem #:braid-hunchentoot
  :version "0.0.1"
  :author "Rob Blackwell"
  :description "An adapter that allows the Braid HTTP abstraction to
  be used with the hunchentoot web server."
  :serial t
  :depends-on (#:alexandria 
							 #:hunchentoot
							 #:braid)
  :components ((:file "package")
							 (:file "braid-hunchentoot")))
