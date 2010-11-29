;;;; package.lisp

(defpackage #:selenium
  (:use #:cl #:drakma)
  (:export #:session-start
	   #:session-stop
	   #:open-page
	   #:type-string
	   #:click
	   #:start-test
	   #:wait-for-page-to-load
	   #:double-click
	   #:context-menu
	   #:fire-event
	   #:focus
	   #:key-press
	   #:shif-key-down
	   #:shif-key-up
	   #:meta-key-down
	   #:meta-key-up
	   #:alt-key-down
	   #:alt-key-up
	   #:control-key-down
	   #:control-key-up
	   #:key-down
	   #:key-up
	   #:mouse-over
	   #:mouse-out
	   #:mouse-down
	   #:mouse-down-right
	   #:mouse-up
	   #:mouse-up-right
	   #:mouse-move
	   #:type-keys
	   #:set-speed
	   #:get-speed
	   #:check
	   #:uncheck
	   #:select
	   #:add-selection
	   #:remove-selection
	   #:remove-all-selections
	   #:submit
	   #:open-window
	   #:select-window
	   #:select-pop-up
	   #:deselect-pop-up
	   #:select-frame
	   #:wait-for-pop-up
	   #:choose-cancel-on-next-confirmation
	   #:choose-ok-on-next-confirmation
	   #:answer-on-next-prompt
	   #:go-back
	   #:refresh
	   #:close-pop-up
	   #:alert-present?
	   #:prompt-present?
	   #:confirmation-present?
	   #:get-prompt
	   #:get-confirmation
	   #:get-alert
	   #:get-location
	   #:get-title
	   #:get-body-text
	   #:get-value))

(in-package :selenium)