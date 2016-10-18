
;;; This file defines a CLOS based layer to simplify defining and working 
;;; with gui elements. 

(in-package #:ftw)

(defun set-default-font (hwnd &optional font)
  (send-message hwnd (const +wm-setfont+) :wparam (or font (get-stock-object :default-gui-font))))

(defun default-message-loop (wndproc &key class-name title width height)
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
      (do ((done nil))
          (done)
        (let ((r (get-message msg)))
          (cond
            ((zerop r) (setf done t))
            (t
             (translate-message msg)
             (dispatch-message msg))))))))


(defun message-poll (&optional timeout)
  (msg-wait-for-multiple-objects :timeout timeout
                                 :mask (logior-consts +qs-allevents+)))


