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

(defmethod selenium-double-click (selenium locator)
  (get-request selenium "doubleClick" locator))

(defmethod selenium-context-menu (selenium locator)
  (get-request selenium "contextMenu" locator))

(defmethod selenium-click-at (selenium locator coord)
  (get-request selenium "clickAt" locator coord))

(defmethod selenium-double-click-at (selenium locator coord)
  (get-request selenium "doubleClickAt" locator coord))

(defmethod selenium-context-menu-at (selenium locator coord)
  (get-request selenium "contextMenuAt" locator coord))

(defmethod selenium-fire-event (selenium locator event-name)
  (get-request selenium "fireEvent" locator event-name))

(defmethod selenium-focus (selenium locator)
  (get-request selenium "focus" locator))

(defmethod selenium-key-press (selenium locator key-sequence)
  (get-request selenium "keyPress" locator key-sequence))

(defmethod selenium-shift-key-down (selenium)
  (get-request selenium "shiftKeyDown"))

(defmethod selenium-shift-key-up (selenium)
  (get-request selenium "shiftKeyUp"))

(defmethod selenium-meta-key-down (selenium)
  (get-request selenium "metaKeyDown"))

(defmethod selenium-meta-key-up (selenium)
  (get-request selenium "metaKeyUp"))

(defmethod selenium-alt-key-down (selenium)
  (get-request selenium "altKeyDown"))

(defmethod selenium-alt-key-up (selenium)
  (get-request selenium "altKeyUp"))

(defmethod selenium-control-key-down (selenium)
  (get-request selenium "controlKeyDown"))

(defmethod selenium-control-key-up (selenium)
  (get-request selenium "controlKeyUp"))

(defmethod selenium-key-down (selenium locator key-sequence)
  (get-request selenium "keyDown" locator key-sequence))

(defmethod selenium-key-up (selenium locator key-sequence)
  (get-request selenium "keyUp" locator key-sequence))

(defmethod selenium-mouse-over (selenium locator)
  (get-request selenium "mouseOver" locator))

(defmethod selenium-mouse-out (selenium locator)
  (get-request selenium "mouseOut" locator))

(defmethod selenium-mouse-down (selenium locator)
  (get-request selenium "mouseDown" locator))

(defmethod selenium-mouse-down-right (selenium locator)
  (get-request selenium "mouseDownRight" locator))

(defmethod selenium-mouse-down-at (selenium locator coord)
  (get-request selenium "mouseDownAt" locator coord))

(defmethod selenium-mouse-down-right-at (selenium locator coord)
  (get-request selenium "mouseDownRightAt" locator coord))

(defmethod selenium-mouse-up (selenium locator)
  (get-request selenium "mouseUp" locator))

(defmethod selenium-mouse-up-right (selenium locator)
  (get-request selenium "mouseUpRight" locator))

(defmethod selenium-mouse-up-at (selenium locator coord)
  (get-request selenium "mouseUpAt" locator coord))

(defmethod selenium-mouse-up-right-at (selenium locator coord)
  (get-request selenium "mouseUpRightAt" locator coord))

(defmethod selenium-mouse-move (selenium locator)
  (get-request selenium "mouseMove" locator))

(defmethod selenium-mouse-move-at (selenium locator coord)
  (get-request selenium "mouseMoveAt" locator coord))

(defmethod selenium-type-keys (selenium locator value)
  (get-request selenium "typeKeys" locator value))

(defmethod selenium-set-speed (selenium value)
  (get-request selenium "setSpeed" value))

(defmethod selenium-get-speed (selenium)
  (get-request selenium "getSpeed"))

(defmethod selenium-check (selenium locator)
  (get-request selenium "check" locator))

(defmethod selenium-uncheck (selenium locator)
  (get-request selenium "uncheck" locator))

(defmethod selenium-select (selenium select-locator option-locator)
  (get-request selenium "select" select-locator option-locator))

(defmethod selenium-add-selection (selenium locator option-locator)
  (get-request selenium "addSelection" locator option-locator))

(defmethod selenium-remove-selection (selenium locator option-locator)
  (get-request selenium "removeSelection" locator option-locator))

(defmethod selenium-remove-all-selections (selenium locator)
  (get-request selenium "removeAllSelections" locator))

(defmethod selenium-submit (selenium form-locator)
  (get-request selenium "submit" form-locator))

(defmethod selenium-open-window (selenium url window-id)
  (get-request selenium "openWindow" url window-id))

(defmethod selenium-select-window (selenium window-id)
  (get-request selenium "selectWindow" window-id))

(defmethod selenium-select-pop-up (selenium window-id)
  (get-request selenium "selectPopUp" window-id))

(defmethod selenium-deselect-pop-up (selenium)
  (get-request selenium "deselectPopUp"))

(defmethod selenium-select-frame (selenium locator)
  (get-request selenium "selectFrame" locator))

(defmethod selenium-wait-for-pop-up (selenium window-id timeout)
  (get-request selenium "waitForPopUp" window-id timeout))

(defmethod selenium-choose-cancel-on-next-confirmation (selenium)
  (get-request selenium "chooseCancelOnNextConfirmation"))

(defmethod selenium-choose-ok-on-next-confirmation (selenium)
  (get-request selenium "chooseOkOnNextConfirmation"))

(defmethod selenium-answer-on-next-prompt (selenium answer)
  (get-request selenium "answerOnNextPrompt" answer))

(defmethod selenium-go-back (selenium)
  (get-request selenium "goBack"))

(defmethod selenium-refresh (selenium)
  (get-request selenium "refresh"))

(defmethod selenium-close (selenium)
  (get-request selenium "close"))

(defmethod selenium-alert-present? (selenium)
  (get-string selenium "isAlertPresent"))

(defmethod selenium-prompt-present? (selenium)
  (get-string selenium "isPromptPresent"))

(defmethod selenium-confirmation-present? (selenium)
  (get-string selenium "isConfirmationPresent"))

(defmethod selenium-get-alert (selenium)
  (get-string selenium "getAlert"))

(defmethod selenium-get-confirmation (selenium)
  (get-string selenium "getConfirmation"))

(defmethod selenium-get-prompt (selenium)
  (get-string selenium "getPrompt"))

(defmethod selenium-get-location (selenium)
  (get-string selenium "getLocation"))

(defmethod selenium-get-title (selenium)
  (get-string selenium "getTitle"))

(defmethod selenium-get-body-text (selenium)
  (get-string selenium "getBodyText"))

(defmethod selenium-get-value (selenium locator)
  (get-string selenium "getValue" locator))




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
    (selenium-stop selenium1)))
