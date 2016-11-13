
(defpackage #:ftw.scrollbar
  (:use #:cl #:ftw))

(in-package #:ftw.scrollbar)

(defparameter *xclient* 0)
(defparameter *yclient* 0)
(defparameter *xclientmax* 0)
(defparameter *xchar* 0)
(defparameter *ychar* 0)
(defparameter *xupper* 0)
(defparameter *xpos* 0)
(defparameter *ypos* 0)

(defparameter *lines*
  '(
    "anteater" "bear" "cougar" 
    "dingo"     "elephant"  "falcon" 
    "gazelle"   "hyena"     "iguana" 
    "jackal"    "kangaroo"  "llama" 
    "moose"     "newt"      "octopus" 
    "penguin"   "quail"     "rat" 
    "squid"     "tortoise"  "urus" 
    "vole"      "walrus"    "xylophone" 
    "yak"       "zebra"
    "This line contains words, but no character. Go figure."
    ""))

(defwndproc scrollbar-wndproc (hwnd msg wparam lparam)
  (switch msg
    (ftw::+wm-create+
     (with-dc (hdc hwnd)
       (let ((tm (get-text-metrics hdc)))
	 (setf *xchar* (getf tm :avcharwidth)
	       *xupper* (truncate (* (if (zerop (logand (getf tm :pitch-and-family) #x1))
					 2
					 3)
				     *xchar*)
				  2)
	       *ychar* (+ (getf tm :height) (getf tm :external-leading)))))
     (setf *xclientmax* (+ (* 48 *xchar*) (* 12 *xupper*))))
    (ftw::+wm-destroy+
     (post-quit-message))
    (ftw::+wm-size+
     (let ((yclient (hiword lparam))
	   (xclient (loword lparam)))
       (set-scroll-info hwnd :vert
			:min 0 :max (1- (length *lines*))
			:page (truncate yclient *ychar*)
			:redraw t)
       (set-scroll-info hwnd :horz
			:min 0 :max (+ 2 (truncate *xclientmax* *xchar*))
			:page (truncate xclient *xchar*)
			:redraw t)))
    (ftw::+wm-hscroll+
     (let ((si (get-scroll-info hwnd :horz)))
       (let ((xpos (getf si :pos)))
	 (switch (loword wparam)
	   (ftw::+sb-lineleft+
	    (decf (getf si :pos)))
	   (ftw::+sb-lineright+
	    (incf (getf si :pos)))
	   (ftw::+sb-pageleft+
	    (decf (getf si :pos) (getf si :page)))
	   (ftw::+sb-pageright+
	    (incf (getf si :pos) (getf si :page)))
	   (ftw::+sb-thumbtrack+
	    (setf (getf si :pos) (getf si :trackpos))))
	 (set-scroll-info hwnd :horz :pos (getf si :pos))
	 (setf si (get-scroll-info hwnd :horz))
	 (unless (= xpos (getf si :pos))
	   (scroll-window-ex hwnd
			     (* *xchar* (- xpos (getf si :pos)))
			     0
			     :flags '(:erase :invalidate))))))
    (ftw::+wm-vscroll+
     (let ((si (get-scroll-info hwnd :vert)))
       (let ((ypos (getf si :pos)))
	 (switch (loword wparam)
	   (ftw::+sb-top+
	    (setf (getf si :pos) (getf si :min)))
	   (ftw::+sb-bottom+
	    (setf (getf si :pos) (getf si :max)))
	   (ftw::+sb-lineup+
	    (decf (getf si :pos)))
	   (ftw::+sb-linedown+
	    (incf (getf si :pos)))
	   (ftw::+sb-pageup+
	    (decf (getf si :pos) (getf si :page)))
	   (ftw::+sb-pagedown+
	    (incf (getf si :pos) (getf si :page)))
	   (ftw::+sb-thumbtrack+
	    (setf (getf si :pos) (getf si :trackpos))))
	 (set-scroll-info hwnd :vert :pos (getf si :pos))
	 (setf si (get-scroll-info hwnd :vert))
	 (unless (= ypos (getf si :pos))
	   (scroll-window-ex hwnd
			     0
			     (* *ychar* (- ypos (getf si :pos)))
			     :flags '(:erase :invalidate))))))
    (ftw::+wm-paint+
     (with-paint (hwnd hdc ps)
       (let (xpos ypos firstline lastline)
	 (let ((si (get-scroll-info hwnd :horz)))
	   (setf xpos (getf si :pos)))
	 (let ((si (get-scroll-info hwnd :vert)))
	   (setf ypos (getf si :pos)))
	 (setf firstline (max 0 (+ ypos (truncate (rect-top (paintstruct-paint ps)) *ychar*)))
	       lastline (min (1- (length *lines*)) (+ ypos (truncate (rect-bottom (paintstruct-paint ps)) *ychar*))))

	 (do ((i firstline (1+ i)))
	     ((> i lastline))
	   (let ((x (* *xchar* (- 1 xpos)))
		 (y (* *ychar* (- i ypos))))
	     (text-out hdc (nth i *lines*) x y)))))))
  (default-window-proc hwnd msg wparam lparam))

(defun scrollbar ()
  (default-message-loop 'scrollbar-wndproc
      :class-name "FTW_SCROLLBAR"
      :title "Scrollbar"
      :width 300 :height 400))

    
