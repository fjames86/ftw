
;;; This file defines useful utility functions and macros to simplify some common tasks.

(in-package #:ftw)

(defun set-default-font (hwnd &optional font)
  "Send a WM_SETFONT message to the window with the specified font or default GUI font." 
  (send-message hwnd (const +wm-setfont+) :wparam (or font (get-stock-object :default-gui-font))))

(defun default-message-loop (wndproc &key class-name title width height)
  "Standard message loop. Defines a new window class with :arrow cursor and 3d-face background,
creates an overlapped, visible  window of this class. Shows, updates and sets this window to 
the foreground. Then loops, processing messages, until a WM_QUIT message is received.
" 
  (let ((cname (or class-name "FTW_MAIN_CLASS")))
    (register-class cname 
                    wndproc 
                    :cursor (load-cursor :arrow)
                    :background (get-sys-color-brush :3d-face))
    (let ((hwnd (create-window cname 
                               :window-name (or title cname)
                               :styles (logior-consts +ws-overlappedwindow+ +ws-visible+)
                               :x 100 :y 100 :width (or width 400) :height (or height 300)))
          (msg (make-msg)))
      (unless hwnd (return-from default-message-loop nil))
      
      (show-window hwnd)
      (update-window hwnd)
      (set-foreground-window hwnd)
      (do ((done nil))
          (done)
        (let ((r (get-message msg)))
          (cond
            ((zerop r) (setf done t))
            (t
             (translate-message msg)
             (dispatch-message msg))))))))


(defun message-poll (&optional timeout)
  "Wait for messages to be available in the message queue." 
  (msg-wait-for-multiple-objects :timeout timeout
                                 :mask (logior-consts +qs-allevents+)))


