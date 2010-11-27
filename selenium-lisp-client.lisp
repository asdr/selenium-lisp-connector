;;;; selenium-lisp-client.lisp

(in-package #:selenium-lisp-client)

(defclass selenium ()
  ((host :initarg :host
	 :initform "127.0.0.1"
	 :accessor selenium-host)
   (port :initarg :port
	 :initform 4444
	 :accessor selenium-port)
   (session-id :initform nil
	       :accessor selenium-session-id)
   (browser-command :initarg :browser
		    :initform "*googlechrome"
		    :reader selenium-browser)
   (browser-url :initarg :url
		:accessor selenium-url
		:initform "")
   (extension-js :initarg :ext-js
		 :accessor selenium-ext-js
		 :initform "")))

;(defgeneric request (selenium)
;  "Sends POST request and collects response from selenium server")


;;if there is a traling / character
;;into host string, remove...			  
(defun eleminate-last-slash (host)
  (if (char= (car (reverse (coerce host 'list)))
	     #\/)
      (subseq host 0 (1- (length host)))
    host))


(defmethod get-request (selenium verb &rest args)
  (let ((url (concatenate 'string
			   "http://"
			   (eleminate-last-slash (selenium-host selenium))			   
			   ":"
			   (format nil "~A" (selenium-port selenium))			   
			   "/selenium-server/driver/"))
	(params nil))

    ;debug:
    ;(format t "ARGS: ~A~%" args)
    
    ;debug:
    ;(format t "URL: ~A~%" url)

    (push (cons "cmd" verb) params)
    (let ((i 1)
	  (sid (selenium-session-id selenium)))
      (dolist (elt args)
	  (push (cons (format nil "~A" i)
		      (format nil "~A" elt))
		params)
	  (incf i))
      (when sid
	(push (cons "sessionId" sid)
	      params)))

    ;debug:
    ;(format t "PARAMS: ~A~%" params)

    (multiple-value-bind (body-or-stream status-code headers uri
			  stream must-close reason-phrase)
        (http-request url
		      :method :post
		      :parameters params
		      :content-type "application/x-www-urlencoded; charset=utf-8")
     
      (when (equal (subseq body-or-stream 0 2) "OK")
	body-or-stream))))

;macro here?
;dont think so... but how else can i ?
(defmacro get-string (selenium verb &body args)
  `(let ((response (get-request ,selenium
				,verb
				,@args)))
    (subseq response 3)))

;;not yet implemented
(defmethod get-string-array (selenium verb &rest args)
  nil)

(defmethod selenium-start(selenium)
  (let ((response (get-string  selenium
			       "getNewBrowserSession"
			       (selenium-browser selenium)
			       (selenium-url selenium)
			       (selenium-ext-js selenium))))
    (when response
      (setf (selenium-session-id selenium)
	    response))))

(defmethod selenium-stop(selenium)
  (get-request selenium "testComplete")
  (setf (selenium-session-id selenium) nil))

(defmethod selenium-open(selenium url)
  (get-request selenium "open" url))

;;name?
;;daha neler...
(defmethod selenium-wait-for-page-to-load (selenium timeout)
  (get-request selenium "waitForPageToLoad" timeout))

(defmethod selenium-type (selenium locator value)
  (get-request selenium "type" locator value))

(defmethod selenium-click (selenium locator)
  (get-request selenium "click" locator))

(defmethod selenium-get-title (selenium)
  (get-request selenium "getTitle"))


(defun start-test ()
  (let ((selenium1 (make-instance 'selenium
			   :host "127.0.0.1/"
			   :port 4444
			   :url "http://www.google.com/")))
    
    (selenium-start selenium1)
    (format t "~A~%" (selenium-host selenium1))
    (format t "~A~%" (selenium-port selenium1))
    (format t "~A~%" (selenium-url selenium1))
    (format t "~A~%" (selenium-session-id selenium1))
    (format t "~A~%" (selenium-browser selenium1))
    (format t "~A~%" (selenium-ext-js selenium1))
    (selenium-open selenium1 "http://www.google.com/webhp")
    (selenium-type selenium1 "q" "hello world")
    (selenium-click selenium1 "btnG")
    (selenium-wait-for-page-to-load selenium1 5000)
    (format t "~A~%" (selenium-get-title selenium1))
    (selenium-stop selenium1)
    ))
