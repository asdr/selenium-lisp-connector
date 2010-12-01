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

(defun split-string (string &key (ch #\Space) (start 0))
  (let ((p1 (position ch string :start start)))
    (if (null p1)
	(list (subseq string start))
      (cons (subseq string start p1)
	          (split-string string :ch ch :start (1+ p1))))))

(defmethod request (selenium verb &rest args)
  (let ((url (concatenate 'string
			   "http://"
			   (eleminate-last-slash (selenium-host selenium))			   
			   ":"
			   (format nil "~A" (selenium-port selenium))			   
			   "/selenium-server/driver/"))
	(params nil))

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

(defmethod get-text ((sel selenium) locator)
  (get-string sel "getText" locator))

(defmethod highlight ((sel selenium) locator)
  (request sel "highlight" locator))

(defmethod get-eval ((sel selenium) script)
  (get-string sel "getEval" script))

(defmethod checked? ((sel selenium) locator)
  (get-string sel "isChecked" locator))

(defmethod get-table ((sel selenium) table-cell-address)
  (get-string sel "getTable" table-cell-address))

(defmethod get-selected-labels ((sel selenium) select-locator)
  (split-string (get-string sel "getSelectedLabels" select-locator) :ch #\,))

(defmethod get-selected-label ((sel selenium) select-locator)
  (get-string sel "getSelectedLabel" select-locator))

(defmethod get-selected-values ((sel selenium) select-locator)
  (split-string (get-string sel "getSelectedValues" select-locator) :ch #\,))

(defmethod get-selected-value ((sel selenium) select-locator)
  (get-string sel "getSelectedValue" select-locator))

(defmethod get-selected-indexes ((sel selenium) select-locator)
  (split-string (get-string sel "getSelectedIndexes" select-locator) :ch #\,))

(defmethod get-selected-index ((sel selenium) select-locator)
  (get-string sel "getSelectedIndex" select-locator))
 
(defmethod get-selected-id ((sel selenium) select-locator)
  (get-string sel "getSelectedId" select-locator))

(defmethod get-selected-ids ((sel selenium) select-locator)
  (split-string (get-string sel "getSelectedIds" select-locator) :ch #\,))

(defmethod something-selected? ((sel selenium) select-locator)
  (get-string sel "isSomethingSelected" select-locator))

(defmethod get-select-options ((sel selenium) select-locator)
  (split-string (get-string sel "getSelectOptions" select-locator)))

(defmethod get-attribute ((sel selenium) locator)
  (get-string sel "getAttribute" locator))

(defmethod text-present? ((sel selenium) pattern)
  (get-string sel "isTextPresent" pattern))

(defmethod element-present? ((sel selenium) locator)
  (get-string sel "isElementPresent" locator))

(defmethod visible? ((sel selenium) locator)
  (get-string sel "isVisible" locator))

(defmethod editable? ((sel selenium) locator)
  (get-string sel "isEditable" locator))

(defmethod get-all-buttons ((sel selenium))
  (split-string (get-string sel "getAllButtons") :ch #\,))

(defmethod get-all-links ((sel selenium))
  (split-string (get-string sel "getAllLinks") :ch #\,))

(defmethod get-all-fields ((sel selenium))
  (split-string (get-string sel "getAllFields") :ch #\,))

(defmethod get-attribute-from-all-windows ((sel selenium) attribute)
  (split-string (get-string sel "getAttributeFromAllWindows" attribute) :ch #\,))

(defmethod dragdrop ((sel selenium) locator movementsString)
  (request sel "dragdrop" locator movementsString))

(defmethod set-mouse-speed ((sel selenium) pixels)
  (request sel "setMouseSpeed" pixels))

(defmethod get-mouse-speed ((sel selenium))
  (get-string sel "getMouseSpeed"))

(defmethod drag-and-drop ((sel selenium) locator movementsString)
  (request sel "dragAndDrop" locator movementsString))

(defmethod drag-and-drop-to-object ((sel selenium) locator-to-be-drag destination-locator)
  (request sel "dragAndDropToObject" locator-to-be-drag destination-locator))

(defmethod window-focus ((sel selenium))
  (request sel "windowFocus"))

(defmethod window-maximize ((sel selenium))
  (request sel "windowMaximize"))

(defmethod get-all-window-ids ((sel selenium))
  (split-string (get-string sel "getAllWindowIds") :ch #\,))

(defmethod get-all-window-names ((sel selenium))
  (split-string (get-string sel "getAllWindowNames") :ch #\,))

(defmethod get-all-window-titles ((sel selenium))
  (split-string (get-string sel "getAllWindowTitles") :ch #\,))

(defmethod get-html-source ((sel selenium))
  (get-string sel "getHtmlsource"))

(defmethod set-cursor-position ((sel selenium) locator position)
  (request sel "setCursorPosition" locator position))

(defmethod get-element-index ((sel selenium) locator)
  (get-string sel "getElementIndex" locator))

(defmethod ordered? ((sel selenium) locator1 locator2)
  (get-string sel "isOrdered" locator1 locator2))

(defmethod get-element-position-left ((sel selenium) locator)
  (get-string sel "getElementPositionLeft" locator))

(defmethod get-element-position-top ((sel selenium) locator)
  (get-string sel "getElementPositionTop" locator))

(defmethod get-element-width ((sel selenium) locator)
  (get-string sel "getElementWidth" locator))

(defmethod get-element-height ((sel selenium) locator)
  (get-string sel "getElementHeight" locator))

(defmethod get-cursor-position ((sel selenium) locator)
  (get-string sel "getCursorPosition" locator))

(defmethod get-expression ((sel selenium) exp)
  (get-string sel "getExpression" exp))

(defmethod get-xpath-count ((sel selenium) xpath)
  (get-string sel "getXpathCount" xpath))

(defmethod assign-id ((sel selenium) locator identifier)
  (request sel "assignId" locator identifier))

(defmethod allow-native-xpath ((sel selenium) allow)
  (request sel "allowNativeXpath" allow))

(defmethod ignore-attributes-without-value ((sel selenium) ignore)
  (request sel "ignoreAttributesWithoutValue" ignore))

(defmethod wait-for-condition ((sel selenium) script timeout)
  (request sel "waitForCondition" script timeout))

(defmethod set-timeout ((sel selenium) timeout)
  (request sel "setTimeout" timeout))

(defmethod wait-for-frame-to-load ((sel selenium) frame-address timeout)
  (request sel "waitForFrameToLoad" frame-address timeout))

(defmethod get-cookie ((sel selenium))
  (get-string sel "getCookie"))

(defmethod get-cookie-by-name ((sel selenium) name)
  (get-string sel "getCookieByName" name))

(defmethod cookie-present? ((sel selenium) name)
  (get-string sel "isCookiePresent" name))

(defmethod create-cookie ((sel selenium) name-value-pair options)
  (request sel "createCookie" name-value-pair options))

(defmethod delete-cookie ((sel selenium) name options)
  (request sel "deleteCookie" name options))

(defmethod delete-all-visible-cookies ((sel selenium))
  (request sel "deleteAllVisibleCookies"))

(defmethod set-browser-log-level ((sel selenium) log-level)
  (request sel "setBrowserLogLevel" log-level))

(defmethod run-script ((sel selenium) script)
  (request sel "runScript" script))

(defmethod add-location-strategy ((sel selenium) strategy-name function-definition)
  (request sel "addLocationStrategy" strategy-name function-definition))

(defmethod capture-page-screenshot ((sel selenium) filename args)
  (request sel "captureEntirePageScreenshot" filename args))

(defmethod rollup ((sel selenium) rollup-name args)
  (request sel "rollup" rollup-name args))

(defmethod add-script ((sel selenium) script-content &optional script-tag-id)
  (request sel "addScript" script-content script-tag-id))

(defmethod remove-script ((sel selenium) script-tag-id)
  (request sel "removeScript" script-tag-id))

(defmethod use-xpath-library ((sel selenium) library-name)
  (request sel "useXpathLibrary" library-name))

(defmethod set-context ((sel selenium) context)
  (request sel "setContext" context))

(defmethod attach-file ((sel selenium) field-locator file-locator)
  (request sel "attachFile" field-locator file-locator))

(defmethod capture-screenshot ((sel selenium) filename)
  (request sel "captureScreenshot" filename))

(defmethod capture-screenshot-to-string ((sel selenium))
  (get-string sel "captureScreenshotToString"))

(defmethod capture-network-traffic ((sel selenium) traffic-type)
  (get-string sel "captureNetworkTraffic" traffic-type))

(defmethod add-custom-request-header ((sel selenium) key value)
  (request sel "addCustomRequestHeader" key value))

(defmethod capture-page-screenshot-to-string ((sel selenium) args)
  (get-string sel "captureEntirePageScreenshotToString" args))

(defmethod shutdown-selenium-server ((sel selenium))
  (request sel "shutDownSeleniumServer"))

(defmethod retrieve-last-remote-control-logs ((sel selenium))
  (get-string sel "retrieveLastRemoteControlLogs"))

(defmethod key-down-native ((sel selenium) keycode)
  (request sel "keyDownNative" keycode))

(defmethod key-up-native ((sel selenium) keycode)
  (request sel "keyUpNative" keycode))

(defmethod key-press-native ((sel selenium) keycode)
  (request sel "keyPressNative" keycode))

(defun start-test ()
  (let ((selenium1 (make-instance 'selenium
				  :host "localhost" 
				  :port 4444
				  :url "http://www.milliyet.com.tr/")))
    
    (session-start selenium1)
    (open-page selenium1 "http://www.milliyet.com.tr")
    (type-string selenium1 "q" "hello world")
    (click selenium1 "btnG")
    (wait-for-page-to-load selenium1 5000)
    (format t "~A~%" (get-title selenium1))
    (session-stop selenium1)))

(defun test1(selenium)
  "returns a test method"
  (lambda ()
    (open-page selenium "http://www.google.com/")
    (type-string selenium "q" "hello world")
    (click selenium "btnG")
    (wait-for-page-to-load selenium 5000)
    (format t "~A~%" (get-title selenium))
    (session-stop selenium)))