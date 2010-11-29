;;;; selenium.lisp

(in-package :selenium)

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

(defgeneric request (selenium verb &rest args)
  (:documentation "Sends POST request and collects response from selenium server"))

(defgeneric open-page (selenium url)
  (:documentation "open"))

;;if there is a traling / character
;;into host string, remove...			  
(defun eleminate-last-slash (host)
  (if (char= (car (reverse (coerce host 'list)))
	     #\/)
      (subseq host 0 (1- (length host)))
    host))


(defmethod request (selenium verb &rest args)
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
  `(let ((response (request ,selenium
			    ,verb
			    ,@args)))
     (subseq response 3)))

(defmethod session-start((sel selenium))
  (let ((response (get-string  sel
			       "getNewBrowserSession"
			       (selenium-browser sel)
			       (selenium-url sel)
			       (selenium-ext-js sel))))
    (when response
      (setf (selenium-session-id sel)
	    response))))

(defmethod session-stop((sel selenium))
  (request sel "testComplete")
  (setf (selenium-session-id sel) nil))

(defmethod open-page((sel selenium) url)
  (request sel "open" url))

;;name?
;;daha neler...
(defmethod wait-for-page-to-load ((sel selenium) timeout)
  (request sel "waitForPageToLoad" timeout))

(defmethod type-string ((sel selenium) locator value)
   (request sel "type" locator value))

(defmethod click ((sel selenium) locator)
  (request sel "click" locator))

(defmethod double-click ((sel selenium) locator)
  (request sel "doubleClick" locator))

(defmethod context-menu ((sel selenium) locator)
  (request sel "contextMenu" locator))

(defmethod click-at ((sel selenium) locator coord)
  (request sel "clickAt" locator coord))

(defmethod double-click-at ((sel selenium) locator coord)
  (request sel "doubleClickAt" locator coord))

(defmethod context-menu-at ((sel selenium) locator coord)
  (request sel "contextMenuAt" locator coord))

(defmethod fire-event ((sel selenium) locator event-name)
  (request sel "fireEvent" locator event-name))

(defmethod focus ((sel selenium) locator)
  (request sel "focus" locator))

(defmethod key-press ((sel selenium) locator key-sequence)
  (request sel "keyPress" locator key-sequence))

(defmethod shift-key-down ((sel selenium))
  (request sel "shiftKeyDown"))

(defmethod shift-key-up ((sel selenium))
  (request sel "shiftKeyUp"))

(defmethod meta-key-down ((sel selenium))
  (request sel "metaKeyDown"))

(defmethod meta-key-up ((sel selenium))
  (request sel "metaKeyUp"))

(defmethod alt-key-down ((sel selenium))
  (request sel "altKeyDown"))

(defmethod alt-key-up ((sel selenium))
  (request sel "altKeyUp"))

(defmethod control-key-down ((sel selenium))
  (request sel "controlKeyDown"))

(defmethod control-key-up ((sel selenium))
  (request sel "controlKeyUp"))

(defmethod key-down ((sel selenium) locator key-sequence)
  (request sel "keyDown" locator key-sequence))

(defmethod key-up ((sel selenium) locator key-sequence)
  (request sel "keyUp" locator key-sequence))

(defmethod mouse-over ((sel selenium) locator)
  (request sel "mouseOver" locator))

(defmethod mouse-out ((sel selenium) locator)
  (request sel "mouseOut" locator))

(defmethod mouse-down ((sel selenium) locator)
  (request sel "mouseDown" locator))

(defmethod mouse-down-right ((sel selenium) locator)
  (request sel "mouseDownRight" locator))

(defmethod mouse-down-at ((sel selenium) locator coord)
  (request sel "mouseDownAt" locator coord))

(defmethod mouse-down-right-at ((sel selenium) locator coord)
  (request sel "mouseDownRightAt" locator coord))

(defmethod mouse-up ((sel selenium) locator)
  (request sel "mouseUp" locator))

(defmethod mouse-up-right ((sel selenium) locator)
  (request sel "mouseUpRight" locator))

(defmethod mouse-up-at ((sel selenium) locator coord)
  (request sel "mouseUpAt" locator coord))

(defmethod mouse-up-right-at ((sel selenium) locator coord)
  (request sel "mouseUpRightAt" locator coord))

(defmethod mouse-move ((sel selenium) locator)
  (request sel "mouseMove" locator))

(defmethod mouse-move-at ((sel selenium) locator coord)
  (request sel "mouseMoveAt" locator coord))

(defmethod type-keys ((sel selenium) locator value)
  (request sel "typeKeys" locator value))

(defmethod set-speed ((sel selenium) value)
  (request sel "setSpeed" value))

(defmethod get-speed ((sel selenium))
  (get-string sel "getSpeed"))

(defmethod check ((sel selenium) locator)
  (request sel "check" locator))

(defmethod uncheck ((sel selenium) locator)
  (request sel "uncheck" locator))

(defmethod select ((sel selenium) select-locator option-locator)
  (request sel "select" select-locator option-locator))

(defmethod add-selection ((sel selenium) locator option-locator)
  (request sel "addSelection" locator option-locator))

(defmethod remove-selection ((sel selenium) locator option-locator)
  (request sel "removeSelection" locator option-locator))

(defmethod remove-all-selections ((sel selenium) locator)
  (request sel "removeAllSelections" locator))

(defmethod submit ((sel selenium) form-locator)
  (request sel "submit" form-locator))

(defmethod open-window ((sel selenium) url window-id)
  (request sel "openWindow" url window-id))

(defmethod select-window ((sel selenium) window-id)
  (request sel "selectWindow" window-id))

(defmethod select-pop-up ((sel selenium) window-id)
  (request sel "selectPopUp" window-id))

(defmethod deselect-pop-up ((sel selenium))
  (request sel "deselectPopUp"))

(defmethod select-frame ((sel selenium) locator)
  (request sel "selectFrame" locator))

(defmethod wait-for-pop-up ((sel selenium) window-id timeout)
  (request sel "waitForPopUp" window-id timeout))

(defmethod choose-cancel-on-next-confirmation ((sel selenium))
  (request sel "chooseCancelOnNextConfirmation"))

(defmethod choose-ok-on-next-confirmation ((sel selenium))
  (request sel "chooseOkOnNextConfirmation"))

(defmethod answer-on-next-prompt ((sel selenium) answer)
  (request sel "answerOnNextPrompt" answer))

(defmethod go-back ((sel selenium))
  (request sel "goBack"))

(defmethod refresh ((sel selenium))
  (request sel "refresh"))

(defmethod close-pop-up ((sel selenium))
  (request sel "close"))

(defmethod alert-present? ((sel selenium))
  (get-string sel "isAlertPresent"))

(defmethod prompt-present? ((sel selenium))
  (get-string sel "isPromptPresent"))

(defmethod confirmation-present? ((sel selenium))
  (get-string sel "isConfirmationPresent"))

(defmethod get-alert ((sel selenium))
  (get-string sel "getAlert"))

(defmethod get-confirmation ((sel selenium))
  (get-string sel "getConfirmation"))

(defmethod get-prompt ((sel selenium))
  (get-string sel "getPrompt"))

(defmethod get-location ((sel selenium))
  (get-string sel "getLocation"))

(defmethod get-title ((sel selenium))
  (get-string sel "getTitle"))

(defmethod get-body-text ((sel selenium))
  (get-string sel "getBodyText"))

(defmethod get-value ((sel selenium) locator)
  (get-string sel "getValue" locator))




(defun start-test ()
  (let ((selenium1 (make-instance 'selenium
				  :host "localhost" 
				  :port 6868 
				  :url "http://sinop/")))
    
    (session-start selenium1)
    ;(format t "~A~%" (selenium-host selenium1))
    ;(format t "~A~%" (selenium-port selenium1))
    ;(format t "~A~%" (selenium-url selenium1))
    ;(format t "~A~%" (selenium-session-id selenium1))
    ;(format t "~A~%" (selenium-browser selenium1))
    ;(format t "~A~%" (selenium-ext-js selenium1))
    (open-page selenium1 "http://sinop/fs_qa/main.html?name=uts_grid2")
    (type-string selenium1 "q" "hello world")
    (click selenium1 "btnG")
    (wait-for-page-to-load selenium1 5000)
    (format t "~A~%" (get-title selenium1))
    (session-stop selenium1)))
