

;;; This file defines useful utility functions and macros to simplify some common tasks.

(in-package #:ftw)

(defun default-message-loop (wndproc &key class-name title width height background icon)
  "Standard message loop. Defines a new window class with :arrow cursor and 3d-face background,
creates an overlapped, visible  window of this class. Shows, updates and sets this window to 
the foreground. Then loops, processing messages, until a WM_QUIT message is received.
" 
  (let ((cname (or class-name "FTW_MAIN_CLASS")))
    (register-class cname 
                    wndproc
                    :icon icon
                    :cursor (load-cursor :arrow)
                    :background (or background (get-sys-color-brush :3d-face)))
    (let ((hwnd (create-window cname 
                               :window-name (or title cname)
                               :styles (logior +ws-overlappedwindow+ +ws-visible+)
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


(defun generate-bitmap-resource (filename)
  "Parse a bitmap file and generate Lisp code so that the resource can be embedded
within a Lisp program rather than having to deliver the image separately. 
The function prints out the code to be inserted into your project.

FILENAME ::= path to a .bmp bitmap file on your system. 
This function loads the data and parses it for width/height information. It then 
prints out a Lisp form which should be pasted into your code for use as a bitmap handle.
This allows the programmer to embed images without having to deliver them as separate files. 
" 
  (with-open-file (f filename :direction :input :element-type '(unsigned-byte 8))
    (let ((len (file-length f)))
      (let ((bmp (make-array len :element-type '(unsigned-byte 8))))
	(read-sequence bmp f)

	;; extract dimensions from header -- see MSDN page for more info on bitmap stuctures 
	;; https://msdn.microsoft.com/en-us/library/windows/desktop/dd183391(v=vs.85).aspx
	(let ((offset (nibbles:ub32ref/le bmp 10))
	      (width (nibbles:sb32ref/le bmp 18))
	      (height (nibbles:sb32ref/le bmp 22))
	      (planes (nibbles:ub16ref/le bmp 26))
	      (bits-per-pixel (nibbles:ub16ref/le bmp 28)))
	  
	  (format t "(defvar NAME~%")
	  (format t "        (create-bitmap ~A ~A ~A ~A~%#(        " 
		  width height planes bits-per-pixel)
	  (dotimes (i (- (length bmp) offset))
	    (when (and (not (zerop i)) (zerop (mod i 16)))
	      (format t "~%                        "))
	    (format t "#x~2,'0X " (aref bmp (+ offset i))))
	  (format t ")))~%"))

	nil))))


(defvar *default-message-font* 
  (create-font-indirect (nonclientmetrics-message-font (system-parameters-info (const +spi-getnonclientmetrics+)))))

(defun get-default-font ()
  "Returns the default system message font." 
  *default-message-font*)

(defun set-default-font (hwnd &optional font)
  "Send a WM_SETFONT message to the window with the specified font. 
If FONT is not specified, the default system message font is used.
" 
  (send-message hwnd (const +wm-setfont+) :wparam (or font (get-default-font))))


