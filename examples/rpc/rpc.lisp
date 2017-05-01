;;;; Copyright (c) Frank James 2016 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

;;; RPC client. Requires frpc2 available from http://github.com/fjames86/frpc2.

(defpackage #:ftw.rpc
  (:use #:cl #:ftw)
  (:export #:rpc))

(in-package #:ftw.rpc)

(defparameter *clt*
  (make-instance 'frpc2:udp-client :timeout 0))

(defun rpc-create (hwnd)
  (create-button "Discover" :parent hwnd
		 :x 25 :y 25 :width 75 :height 23)
  (let ((h (create-window :listbox
			  :ex-styles ftw::+ws-ex-clientedge+
			  :styles (logior ftw::+ws-visible+ ftw::+ws-child+)
			  :x 25 :y 50 :width 200 :height 200
			  :parent hwnd
			  :menu 1)))
    (set-default-font h)
    (add-hwnd 'ip-lb h 1))
  nil)

(defun rpc-command (hwnd id)
  (declare (ignore hwnd id))
  (send-message (hwnd-by-name 'ip-lb) ftw::+lb-resetcontent+ 0 0)
  (frpc2:send-rpc *clt* #'drx:encode-void nil #'drx:decode-void
		  100000 2 0))

(defun rpc-event-cb (clt)
  (frpc2:recv-rpc clt)
  (with-wide-string (ws (fsocket:sockaddr-string (frpc2:udp-client-addr clt)))
    (send-message (hwnd-by-name 'ip-lb) ftw::+lb-addstring+ 0 ws)))

(defwndproc rpc-wndproc (hwnd msg wparam lparam)
  (switch msg
    (ftw::+wm-create+
     (rpc-create hwnd))
    (ftw::+wm-command+
     (rpc-command hwnd (loword wparam)))
    (ftw::+wm-close+
     (post-quit-message)))
  (default-window-proc hwnd msg wparam lparam))

(defun rpc ()
  (default-message-loop-multiple 'rpc-wndproc
      :class-name "FTW_RPC"
      :title "RPC Client"
      :handle-procs (list (list (fsocket::poll-context-event (frpc2::udp-client-pc *clt*))
				#'rpc-event-cb
				*clt*))))


