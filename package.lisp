;;;; package.lisp

(defpackage #:selenium
  (:use #:cl #:drakma)
  (:export #:start
	   #:stop
	   #:open
	   #:type
	   #:click
	   #:start-test))

(in-package :selenium)