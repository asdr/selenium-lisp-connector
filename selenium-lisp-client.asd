;;;; selenium-lisp-client.asd

(asdf:defsystem #:selenium-lisp-client
  :serial t
  :depends-on (#:drakma)
  :components ((:file "package")
               (:file "selenium-lisp-client")))

