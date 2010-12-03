;;;; selenium.lisp

(in-package :selenium)

(defvar *selenium-host* "localhost")
(defvar *selenium-port* 4444)
(defvar *session-id* nil)
(defvar *browser* "*googlechrome")
(defvar *startup-url* nil)
(defvar *extension-js* nil)

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

(defun request (verb &rest args)
  (let ((url (concatenate 'string
			  "http://"
			  (eleminate-last-slash *selenium-host*)	   
			  ":"
			  (format nil "~A" *selenium-port*)		   
			  "/selenium-server/driver/"))
	(params nil))
    
    (push (cons "cmd" verb) params)
    (let ((i 1))
      (dolist (elt args)
	(push (cons (format nil "~A" i)
		    (format nil "~A" elt))
	      params)
	(incf i))
      (when *session-id*
	(push (cons "sessionId" *session-id*)
	      params)))

    (let ((body-or-stream (http-request url
					:method :post
					:parameters params
					:content-type "application/x-www-urlencoded; charset=utf-8")))
      (if (equal (subseq body-or-stream 0 2) "OK")
	  body-or-stream
	(format t "~A" body-or-stream)))))

;macro here?
;dont think so... but how else can i ?
(defmacro get-string (verb &body args)
  `(let ((response (request ,verb
			    ,@args)))
     (subseq response 3)))

(defun session-setup (&key host port browser url ext-js)
  (setf *selenium-host* host
	*selenium-port* port
	*session-id* nil
	*browser* browser
	*startup-url* url
	*extension-js* ext-js))

(defmacro  with-new-session ((&key (host *selenium-host*) 
				   (port *selenium-port*) 
				   (browser *browser*)
				   (url *startup-url*)
				   (ext-js *extension-js*)
				   (persist nil)) &body body)
  `(let ((*selenium-host* ,host)
	 (*selenium-port* ,port)
	 (*session-id* nil)
	 (*browser* ,browser)
	 (*startup-url* ,url)
	 (*extension-js* ,ext-js))
     (session-start)
     ,@body
     (if (null ,persist)
	 (session-stop)
       (setf *selenium-host* ,host
	     *selenium-port* ,port
	     *browser* ,browser
	     *startup-url* ,url
	     *extension-js* ,ext-js))))

(defun session-start(&key (browser *browser*)
			  (url *startup-url*)
			  (ext-js *extension-js*))
  (let ((response (get-string  "getNewBrowserSession"
			       browser
			       url
			       ext-js)))
    (when response
      (setf *session-id* response))))

(defun session-stop()
  (request "testComplete")
  (setf *session-id* nil))

(defun open-page(url)
  (request "open" url))

;;name?
;;daha neler...
(defun wait-for-page-to-load (timeout)
  (request "waitForPageToLoad" timeout))

(defun type-string (locator value)
   (request "type" locator value))

(defun click (locator)
  (request "click" locator))

(defun double-click (locator)
  (request "doubleClick" locator))

(defun context-menu (locator)
  (request "contextMenu" locator))

(defun click-at (locator coord)
  (request "clickAt" locator coord))

(defun double-click-at (locator coord)
  (request "doubleClickAt" locator coord))

(defun context-menu-at (locator coord)
  (request "contextMenuAt" locator coord))

(defun fire-event (locator event-name)
  (request "fireEvent" locator event-name))

(defun focus (locator)
  (request "focus" locator))

(defun key-press (locator key-sequence)
  (request "keyPress" locator key-sequence))

(defun shift-key-down ()
  (request "shiftKeyDown"))

(defun shift-key-up ()
  (request "shiftKeyUp"))

(defun meta-key-down ()
  (request "metaKeyDown"))

(defun meta-key-up ()
  (request "metaKeyUp"))

(defun alt-key-down ()
  (request "altKeyDown"))

(defun alt-key-up ()
  (request "altKeyUp"))

(defun control-key-down ()
  (request "controlKeyDown"))

(defun control-key-up ()
  (request "controlKeyUp"))

(defun key-down (locator key-sequence)
  (request "keyDown" locator key-sequence))

(defun key-up (locator key-sequence)
  (request "keyUp" locator key-sequence))

(defun mouse-over (locator)
  (request "mouseOver" locator))

(defun mouse-out (locator)
  (request "mouseOut" locator))

(defun mouse-down (locator)
  (request "mouseDown" locator))

(defun mouse-down-right (locator)
  (request "mouseDownRight" locator))

(defun mouse-down-at (locator coord)
  (request "mouseDownAt" locator coord))

(defun mouse-down-right-at (locator coord)
  (request "mouseDownRightAt" locator coord))

(defun mouse-up (locator)
  (request "mouseUp" locator))

(defun mouse-up-right (locator)
  (request "mouseUpRight" locator))

(defun mouse-up-at (locator coord)
  (request "mouseUpAt" locator coord))

(defun mouse-up-right-at (locator coord)
  (request "mouseUpRightAt" locator coord))

(defun mouse-move (locator)
  (request "mouseMove" locator))

(defun mouse-move-at (locator coord)
  (request "mouseMoveAt" locator coord))

(defun type-keys (locator value)
  (request "typeKeys" locator value))

(defun set-speed (value)
  (request "setSpeed" value))

(defun get-speed ()
  (get-string "getSpeed"))

(defun check (locator)
  (request "check" locator))

(defun uncheck (locator)
  (request "uncheck" locator))

(defun select (select-locator option-locator)
  (request "select" select-locator option-locator))

(defun add-selection (locator option-locator)
  (request "addSelection" locator option-locator))

(defun remove-selection (locator option-locator)
  (request "removeSelection" locator option-locator))

(defun remove-all-selections (locator)
  (request "removeAllSelections" locator))

(defun submit (form-locator)
  (request "submit" form-locator))

(defun open-window (url window-id)
  (request "openWindow" url window-id))

(defun select-window (window-id)
  (request "selectWindow" window-id))

(defun select-pop-up (window-id)
  (request "selectPopUp" window-id))

(defun deselect-pop-up ()
  (request "deselectPopUp"))

(defun select-frame (locator)
  (request "selectFrame" locator))

(defun wait-for-pop-up (window-id timeout)
  (request "waitForPopUp" window-id timeout))

(defun choose-cancel-on-next-confirmation ()
  (request "chooseCancelOnNextConfirmation"))

(defun choose-ok-on-next-confirmation ()
  (request "chooseOkOnNextConfirmation"))

(defun answer-on-next-prompt (answer)
  (request "answerOnNextPrompt" answer))

(defun go-back ()
  (request "goBack"))

(defun refresh ()
  (request "refresh"))

(defun close-pop-up ()
  (request "close"))

(defun alert-present? ()
  (get-string "isAlertPresent"))

(defun prompt-present? ()
  (get-string "isPromptPresent"))

(defun confirmation-present? ()
  (get-string "isConfirmationPresent"))

(defun get-alert ()
  (get-string "getAlert"))

(defun get-confirmation ()
  (get-string "getConfirmation"))

(defun get-prompt ()
  (get-string "getPrompt"))

(defun get-location ()
  (get-string "getLocation"))

(defun get-title ()
  (get-string "getTitle"))

(defun get-body-text ()
  (get-string "getBodyText"))

(defun get-value (locator)
  (get-string "getValue" locator))

(defun get-text (locator)
  (get-string "getText" locator))

(defun highlight (locator)
  (request "highlight" locator))

(defun get-eval (script)
  (get-string "getEval" script))

(defun checked? (locator)
  (get-string "isChecked" locator))

(defun get-table (table-cell-address)
  (get-string "getTable" table-cell-address))

(defun get-selected-labels (select-locator)
  (split-string (get-string "getSelectedLabels" select-locator) :ch #\,))

(defun get-selected-label (select-locator)
  (get-string "getSelectedLabel" select-locator))

(defun get-selected-values (select-locator)
  (split-string (get-string "getSelectedValues" select-locator) :ch #\,))

(defun get-selected-value (select-locator)
  (get-string "getSelectedValue" select-locator))

(defun get-selected-indexes (select-locator)
  (split-string (get-string "getSelectedIndexes" select-locator) :ch #\,))

(defun get-selected-index (select-locator)
  (get-string "getSelectedIndex" select-locator))
 
(defun get-selected-id (select-locator)
  (get-string "getSelectedId" select-locator))

(defun get-selected-ids (select-locator)
  (split-string (get-string "getSelectedIds" select-locator) :ch #\,))

(defun something-selected? (select-locator)
  (get-string "isSomethingSelected" select-locator))

(defun get-select-options (select-locator)
  (split-string (get-string "getSelectOptions" select-locator)))

(defun get-attribute (locator)
  (get-string "getAttribute" locator))

(defun text-present? (pattern)
  (get-string "isTextPresent" pattern))

(defun element-present? (locator)
  (get-string "isElementPresent" locator))

(defun visible? (locator)
  (get-string "isVisible" locator))

(defun editable? (locator)
  (get-string "isEditable" locator))

(defun get-all-buttons ()
  (split-string (get-string "getAllButtons") :ch #\,))

(defun get-all-links ()
  (split-string (get-string "getAllLinks") :ch #\,))

(defun get-all-fields ()
  (split-string (get-string "getAllFields") :ch #\,))

(defun get-attribute-from-all-windows (attribute)
  (split-string (get-string "getAttributeFromAllWindows" attribute) :ch #\,))

(defun dragdrop (locator movementsString)
  (request "dragdrop" locator movementsString))

(defun set-mouse-speed (pixels)
  (request "setMouseSpeed" pixels))

(defun get-mouse-speed ()
  (get-string "getMouseSpeed"))

(defun drag-and-drop (locator movementsString)
  (request "dragAndDrop" locator movementsString))

(defun drag-and-drop-to-object (locator-to-be-drag destination-locator)
  (request "dragAndDropToObject" locator-to-be-drag destination-locator))

(defun window-focus ()
  (request "windowFocus"))

(defun window-maximize ()
  (request "windowMaximize"))

(defun get-all-window-ids ()
  (split-string (get-string "getAllWindowIds") :ch #\,))

(defun get-all-window-names ()
  (split-string (get-string "getAllWindowNames") :ch #\,))

(defun get-all-window-titles ()
  (split-string (get-string "getAllWindowTitles") :ch #\,))

(defun get-html-source ()
  (get-string "getHtmlsource"))

(defun set-cursor-position (locator position)
  (request "setCursorPosition" locator position))

(defun get-element-index (locator)
  (get-string "getElementIndex" locator))

(defun ordered? (locator1 locator2)
  (get-string "isOrdered" locator1 locator2))

(defun get-element-position-left (locator)
  (get-string "getElementPositionLeft" locator))

(defun get-element-position-top (locator)
  (get-string "getElementPositionTop" locator))

(defun get-element-width (locator)
  (get-string "getElementWidth" locator))

(defun get-element-height (locator)
  (get-string "getElementHeight" locator))

(defun get-cursor-position (locator)
  (get-string "getCursorPosition" locator))

(defun get-expression (exp)
  (get-string "getExpression" exp))

(defun get-xpath-count (xpath)
  (get-string "getXpathCount" xpath))

(defun assign-id (locator identifier)
  (request "assignId" locator identifier))

(defun allow-native-xpath (allow)
  (request "allowNativeXpath" allow))

(defun ignore-attributes-without-value (ignore)
  (request "ignoreAttributesWithoutValue" ignore))

(defun wait-for-condition (script timeout)
  (request "waitForCondition" script timeout))

(defun set-timeout (timeout)
  (request "setTimeout" timeout))

(defun wait-for-frame-to-load (frame-address timeout)
  (request "waitForFrameToLoad" frame-address timeout))

(defun get-cookie ()
  (get-string "getCookie"))

(defun get-cookie-by-name (name)
  (get-string "getCookieByName" name))

(defun cookie-present? (name)
  (get-string "isCookiePresent" name))

(defun create-cookie (name-value-pair options)
  (request "createCookie" name-value-pair options))

(defun delete-cookie (name options)
  (request "deleteCookie" name options))

(defun delete-all-visible-cookies ()
  (request "deleteAllVisibleCookies"))

(defun set-browser-log-level (log-level)
  (request "setBrowserLogLevel" log-level))

(defun run-script (script)
  (request "runScript" script))

(defun add-location-strategy (strategy-name function-definition)
  (request "addLocationStrategy" strategy-name function-definition))

(defun capture-page-screenshot (filename args)
  (request "captureEntirePageScreenshot" filename args))

(defun rollup (rollup-name args)
  (request "rollup" rollup-name args))

(defun add-script (script-content &optional script-tag-id)
  (request "addScript" script-content script-tag-id))

(defun remove-script (script-tag-id)
  (request "removeScript" script-tag-id))

(defun use-xpath-library (library-name)
  (request "useXpathLibrary" library-name))

(defun set-context (context)
  (request "setContext" context))

(defun attach-file (field-locator file-locator)
  (request "attachFile" field-locator file-locator))

(defun capture-screenshot (filename)
  (request "captureScreenshot" filename))

(defun capture-screenshot-to-string ()
  (get-string "captureScreenshotToString"))

(defun capture-network-traffic (traffic-type)
  (get-string "captureNetworkTraffic" traffic-type))

(defun add-custom-request-header (key value)
  (request "addCustomRequestHeader" key value))

(defun capture-page-screenshot-to-string (args)
  (get-string "captureEntirePageScreenshotToString" args))

(defun shutdown-selenium-server ()
  (request "shutDownSeleniumServer"))

(defun retrieve-last-remote-control-logs ()
  (get-string "retrieveLastRemoteControlLogs"))

(defun key-down-native (keycode)
  (request "keyDownNative" keycode))

(defun key-up-native (keycode)
  (request "keyUpNative" keycode))

(defun key-press-native (keycode)
  (request "keyPressNative" keycode))

(defun test1 ()
  (with-new-session (:url "http://www.google.com")
    (open-page "http://www.goole.com")
    (type-string "q" "hello world")
    (click "btnG")
    (wait-for-page-to-load 5000)
    (format t "~A~%" (get-title))))