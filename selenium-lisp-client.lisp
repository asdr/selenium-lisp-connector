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

(defmethod request (selenium verb args)
  (let ((url (concatenate 'string 
			   "http://"
			   
			   ;;if there is a traling / character
			   ;;into host string, remove...
			   ;;this control shouldnt be here,
			   ;;TODO: 
			   (let ((host (selenium-host selenium)))
			     (if (char= (car (reverse (coerce host 'list)))
				    #\/)
				 (subseq host 0 (1- (length host)))
			       host))

			   ":"

			   (format nil "~A" (selenium-port selenium))

			   "/selenium-server/driver/"))
	(params nil))
    
    (push (cons "cmd" verb) params)
    (let ((i 1)
	  (sid (selenium-session-id selenium)))
      (dolist (elt args)
	(when elt
	  (push (cons (format nil "~A" i)
		      (format nil "~A" elt))
		params)
	  (incf i)))
      (when sid
	(push (cons "sessionId" sid)
	      params)))
      
    (multiple-value-bind (body-or-stream
			  status-code     ;;
			  headers         ;; we're not 
			  uri             ;; using these
			  stream          ;; right now,
			  must-close      ;; but we will...
			  reason-phrase)  ;;
      (http-request url
		    :method :post
		    :parameters params
		    :content-type "application/x-www-urlencoded; charset=utf-8")
      (when (equal (subseq body-or-stream 0 2) "OK")
	body-or-stream))))

(defmethod selenium-start(selenium)
  (let ((response (request selenium
			   "getNewBrowserSession"
			   (list (selenium-browser selenium)
				 (selenium-url selenium)
				 ;(selenium-ext-js selenium)
				 ))))
    (when response
      (setf (selenium-session-id selenium)
	    (subseq response 3)))))

(defmethod selenium-stop(selenium)
  (request selenium
	   "testComplete"
	   nil)
  (setf (selenium-session-id selenium)
	nil))

(defmethod selenium-open(selenium url)
  (request selenium
	   "open"
	   (list url)))

(defmethod selenium-wait-for-page-to-load (selenium timeout)
  (request selenium
	   "waitForPageToLoad"
	   (list timeout)))


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
    (selenium-wait-for-page-to-load selenium1 5000)
    (selenium-stop selenium1)
    ))
			   
			   




