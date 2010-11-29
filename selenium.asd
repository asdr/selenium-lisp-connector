;;;; selenium.asd

(asdf:defsystem #:selenium
  :serial t
  :depends-on (#:drakma)
  :components ((:file "package")
               (:file "selenium")))

