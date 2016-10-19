
(defpackage #:ftw.icon
  (:use #:cl #:cffi #:ftw))

(in-package #:ftw.icon)

;;; This file shows how to create a custom icon.
;;; It creates a new icon in the WM_CREATE handler and sets it as the icon for
;;; the window class using SET-CLASS-POINTER. 

;; This is the "ying" symbol taken from MSDN 
;; https://msdn.microsoft.com/en-gb/library/windows/desktop/ms648051(v=vs.85).aspx#_win32_Creating_an_Icon
(defvar *test-icon* nil)

(defun test-create-icon ()
  (unless *test-icon* 
    (setf *test-icon*
	  (create-icon #(#xFF #xFF #xFF #xFF 
			 #xFF #xFF #xC3 #xFF   
			 #xFF #xFF #x00 #xFF   
			 #xFF #xFE #x00 #x7F   
			 
			 #xFF #xFC #x00 #x1F   
			 #xFF #xF8 #x00 #x0F   
			 #xFF #xF8 #x00 #x0F   
			 #xFF #xF0 #x00 #x07   
			 
			 #xFF #xF0 #x00 #x03   
			 #xFF #xE0 #x00 #x03   
			 #xFF #xE0 #x00 #x01   
			 #xFF #xE0 #x00 #x01   
			 
			 #xFF #xF0 #x00 #x01   
			 #xFF #xF0 #x00 #x00   
			 #xFF #xF8 #x00 #x00   
			 #xFF #xFC #x00 #x00   
			 
			 #xFF #xFF #x00 #x00   
			 #xFF #xFF #x80 #x00   
			 #xFF #xFF #xE0 #x00   
			 #xFF #xFF #xE0 #x01   
			 
			 #xFF #xFF #xF0 #x01   
			 #xFF #xFF #xF0 #x01   
			 #xFF #xFF #xF0 #x03   
			 #xFF #xFF #xE0 #x03   
			 
			 #xFF #xFF #xE0 #x07   
			 #xFF #xFF #xC0 #x0F   
			 #xFF #xFF #xC0 #x0F   
			 #xFF #xFF #x80 #x1F   
			 
			 #xFF #xFF #x00 #x7F   
			 #xFF #xFC #x00 #xFF   
			 #xFF #xF8 #x03 #xFF   
			 #xFF #xFC #x3F #xFF)
		       #(#x00 #x00 #x00 #x00   
			 #x00 #x00 #x00 #x00   
			 #x00 #x00 #x00 #x00   
			 #x00 #x00 #x00 #x00   
			 
			 #x00 #x00 #x00 #x00   
			 #x00 #x00 #x00 #x00   
			 #x00 #x00 #x00 #x00   
			 #x00 #x00 #x38 #x00   
			 
			 #x00 #x00 #x7C #x00   
			 #x00 #x00 #x7C #x00   
			 #x00 #x00 #x7C #x00   
			 #x00 #x00 #x38 #x00   
			 
			 #x00 #x00 #x00 #x00   
			 #x00 #x00 #x00 #x00   
			 #x00 #x00 #x00 #x00   
			 #x00 #x00 #x00 #x00   
			 
			 #x00 #x00 #x00 #x00   
			 #x00 #x00 #x00 #x00   
			 #x00 #x00 #x00 #x00   
			 #x00 #x00 #x00 #x00   
			 
			 #x00 #x00 #x00 #x00   
			 #x00 #x00 #x00 #x00   
			 #x00 #x00 #x00 #x00   
			 #x00 #x00 #x00 #x00   
			 
			 #x00 #x00 #x00 #x00   
			 #x00 #x00 #x00 #x00   
			 #x00 #x00 #x00 #x00   
			 #x00 #x00 #x00 #x00   
			 
			 #x00 #x00 #x00 #x00   
			 #x00 #x00 #x00 #x00   
			 #x00 #x00 #x00 #x00   
			 #x00 #x00 #x00 #x00)
		       :bits-per-pixel 1)))
  *test-icon*)

(defwndproc test-icon-wndproc (hwnd msg wparam lparam)
  (switch msg
    ((const +wm-create+)
     (set-class-pointer hwnd :icon (test-create-icon)))
    ((const +wm-destroy+)
     (destroy-icon *test-icon*)
     (setf *test-icon* nil)
     (post-quit-message)))
  (default-window-proc hwnd msg wparam lparam))

(defun test-icon ()
  (default-message-loop (callback test-icon-wndproc) 
      :class-name "TEST_ICON" 
      :title "Test icon" ))

