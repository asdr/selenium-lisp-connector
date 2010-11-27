;;;; package.lisp

(defpackage #:selenium-lisp-client
  (:use #:cl #:drakma)
  (:export #:selenium-start
	   #:selenium-stop
	   #:selenium-open
	   #:selenium-type
	   #:selenium-click
	   #:start-test))

