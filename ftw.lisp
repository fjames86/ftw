;;;; Copyright (c) Frank James 2016 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.


;;; This file defines useful utility functions and macros to simplify some common tasks.

(in-package #:ftw)


(defvar *accel* nil
  "FTW's global accelerator table.")

(defun set-accelerator-table (&optional entries)
  "Destroy the existing accelerator table (if any) and set new table.
ENTRIES ::= new accelerator table to set. 

The existing accelerator table is always destroyed. If ENTRIES is non-nil 
then a new table is set.
"
  (when *accel*
    (destroy-accelerator-table *accel*)
    (setf *accel* nil))
  (when entries 
    (setf *accel* (create-accelerator-table entries))))


(defun default-message-loop (wndproc &key class-name title width height background icon icon-small)
  "Standard message loop. Defines a new window class with :arrow cursor and 3d-face background,
creates an overlapped, visible  window of this class. Shows, updates and sets this window to 
the foreground. Then loops, processing messages, until a WM_QUIT message is received.

Also processes accelerator keys set using SET-ACCELERATOR-TABLE.
" 
  (let ((cname (or class-name "FTW_MAIN_CLASS")))
    (register-class cname 
                    wndproc
                    :icon icon
		    :icon-small icon-small
                    :cursor (load-cursor :arrow)
                    :background (or background (get-sys-color-brush :3d-face)))
    (let ((hwnd (create-window cname 
                               :window-name (or title cname)
			       :ex-styles (logior-consts +ws-ex-appwindow+)
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
            ((or (null *accel*)
                 (zerop (translate-accelerator hwnd *accel* msg)))
             (translate-message msg)
             (dispatch-message msg))))))))


(defun message-poll (&optional timeout)
  "Wait for messages to be available in the message queue." 
  (msg-wait-for-multiple-objects :timeout timeout
                                 :mask (logior-consts +qs-allevents+)))


(defun create-bitmap-resource (width height planes bits-per-pixel data)
  "Create a colour bitmap. Data should be a vector of (unsigned-byte 8) of
the correct length and alignment for the colour data." 
  (let ((bm (create-dib-section nil width height planes bits-per-pixel)))
    (set-di-bits nil bm width height planes bits-per-pixel data)
    bm))

(defun generate-bitmap-resource (filename &optional (stream *standard-output*) name)
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
	  
	  (format stream "(defvar ~A~%" (or name "NAME"))
	  (format stream "        (create-bitmap-resource ~A ~A ~A ~A~%#(        " 
		  width height planes bits-per-pixel)

	  ;; bitmap stores it as aa rr gg bb
	  ;; we want it as bb gg rr aa
	  ;; BUT: we need to use premultiplied alpha 
	  (do ((i 0 (+ i 4)))
	      ((= i (- (length bmp) offset)))
	    (let ((aa (aref bmp (+ offset i 0)))
		  (bb (aref bmp (+ offset i 1)))
		  (gg (aref bmp (+ offset i 2)))
		  (rr (aref bmp (+ offset i 3))))
	      (setf (aref bmp (+ offset i 0))
		    (truncate (* bb aa) #xff)
		    (aref bmp (+ offset i 1))
		    (truncate (* gg aa) #xff)
		    (aref bmp (+ offset i 2))
		    (truncate (* rr aa) #xff)
		    (aref bmp (+ offset i 3))
		    aa)))
	  
	  (dotimes (i (- (length bmp) offset))	    
	    (when (and (not (zerop i)) (zerop (mod i 16)))
	      (when (zerop (mod i 256))
		(terpri stream))
	      (format stream "~%                        "))
	    (when (zerop (mod i 4))
	      (format stream " "))
	    (format stream "#x~2,'0X " (aref bmp (+ offset i))))
	  (format stream ")))~%"))

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
  (send-message hwnd (const +wm-setfont+) (or font (get-default-font)) 0))


(defun generate-icon-resource (filename &optional (stream *standard-output*) name)
  "Generate Lisp code for a given icon so that it can be embedded into 
Lisp code. This means you don't have to deliver the icon file separately. 
This is equivalent to the .rc resources you link with when writing C.

FILENAME ::= path to a .ico file containing the icon you want to use.
Prints out code which should be included into your project.
"
  (with-open-file (f filename :direction :input :element-type '(unsigned-byte 8))
    (let ((len (file-length f)))
      (let ((ico (make-array len :element-type '(unsigned-byte 8))))
	(read-sequence ico f)

	;; icon file header -- see wikipedia entry for details 
	(let ((type (nibbles:ub16ref/le ico 2))
	      (width (aref ico 6))
	      (height (aref ico 7))
	      (planes (nibbles:ub16ref/le ico 10))
	      (bits-per-pixel (nibbles:ub16ref/le ico 12))
;;	      (size (nibbles:ub32ref/le ico 14))
	      (offset (+ (nibbles:ub32ref/le ico 18) 40)))

	  (unless (= type 1) (error "Expected type 2 got ~A" type))
	  
	  (format stream "(defvar ~A~%" (or name "NAME"))
	  (format stream "        (create-icon ~A ~A ~A ~A~%"
		  width height planes bits-per-pixel)
	  (format stream "                     (make-array ~A :element-type '(unsigned-byte 8))~%"
		  (- (length ico) offset))
	  (format stream "                     #(")
	  (dotimes (i (- (length ico) offset))
	    (when (and (not (zerop i)) (zerop (mod i 16)))
	      (format stream "~%                        "))
	    (format stream "#x~2,'0X " (aref ico (+ offset i))))
	  (format stream ")))~%"))

	nil))))

(defun generate-cursor-resource (filename &optional (stream *standard-output*) name)
    "Generate Lisp code for a given cursor so that it can be embedded into 
Lisp code. This means you don't have to deliver the cursor file separately. 
This is equivalent to the .rc resources you link with when writing C.

FILENAME ::= path to a .cur file containing the cursor you want to use.
Prints out code which should be included into your project.
"

  (with-open-file (f filename :direction :input :element-type '(unsigned-byte 8))
    (let ((len (file-length f)))
      (let ((ico (make-array len :element-type '(unsigned-byte 8))))
	(read-sequence ico f)

	;; icon file header -- see wikipedia entry for details 
	(let ((type (nibbles:ub16ref/le ico 2))
	      (width (aref ico 6))
	      (height (aref ico 7))
	      (x (nibbles:ub16ref/le ico 10))
	      (y (nibbles:ub16ref/le ico 12))
;;	      (size (nibbles:ub32ref/le ico 14))
	      (offset (+ (nibbles:ub32ref/le ico 18) 40)))

	  (unless (= type 2) (error "Expected type 2 got ~A" type))
	  
	  (format stream "(defvar ~A~%" (or name "NAME"))
	  (format stream "        (create-cursor ~A ~A ~A ~A~%"
		  x y width height )
	  (format stream "                     (make-array ~A :element-type '(unsigned-byte 8))~%"
		  (- (length ico) offset))
	  (format stream "                     #(")
	  (dotimes (i (- (length ico) offset))
	    (when (and (not (zerop i)) (zerop (mod i 16)))
	      (format stream "~%                        "))
	    (format stream "#x~2,'0X " (aref ico (+ offset i))))
	  (format stream ")))~%"))

	nil))))

(defun generate-resource-file (filename resources &key package)
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (format stream "~%")
    (format stream "(in-package #:~A)~%" (or package (package-name *package*)))
    (format stream "~%")
    (dolist (resource resources)
      (destructuring-bind (res-type &rest res-args) resource
	(ecase res-type
	  (:icon
	   (destructuring-bind (name icon-filename) res-args 
	     (generate-icon-resource icon-filename stream name)))
	  (:cursor
	   (destructuring-bind (name cursor-filename) res-args
	     (generate-cursor-resource cursor-filename stream name)))
	  (:bitmap
	   (destructuring-bind (name bitmap-filename) res-args
	     (generate-bitmap-resource bitmap-filename stream name))))))
    (format stream "~%")))

(defun get-client-size (hwnd)
  "Get width and height of the hwnd. Returns (values width height)." 
  (let ((r (get-client-rect hwnd)))
    (values (getf r :right 0)
            (getf r :bottom 0))))


(defun add-menu-bar (hwnd menus)
  "Add menu bar to the window. 
MENUS ::= MENU* 
MENU ::= type flags &key name id children
where 
TYPE ::= :menu | :item | :check | :radio 
FLAGS ::= list of flags to be passed to append-menu 
NAME ::= string naming the item 
ID ::= integer identifier 
CHILDREN ::= MENU* menu children 
" 
;; Example 
;; (add-menu-bar `((:menu (:popup) :name "&File"
;; 		 :children 
;; 		 ((:item (:string) 
;; 			 :name ,(format nil "&Find~ACtrl+F" #\tab)
;; 			 :id 1)
;; 		  (:item (:separator))
;; 		  (:item (:string) 
;; 			 :name ,(format nil "&Quit~ACtrl+Q" #\tab)
;; 			 :id 2)))))

  (labels ((process-menu (parent menu)
             (destructuring-bind (type flags &key name id children) menu
               (ecase type
                 (:menu
                  (let ((m (create-menu)))
                    (dolist (child children)
                      (process-menu m child))
                    (append-menu parent flags m name)))
                 (:item
                  (append-menu parent flags (or id 0) name))
		 (:check
		  (check-menu-item parent (or id 0) (member :checked flags)))
		 (:radio
		  (check-menu-radio-item parent
					 (first flags) (second flags)
					 (or id 0)))))))
    (let ((bar (create-menu)))
      (dolist (menu menus)
        (process-menu bar menu))

      (set-menu hwnd bar))))


(defun set-window-to-center (hwnd)
  (let ((rect (get-window-rect hwnd)))
    (destructuring-bind (&key (left 0) (right 0) (top 0) (bottom 0)) rect 
      (set-window-pos hwnd
                      :topmost 
                      (truncate (- (get-system-metrics :cx-screen)
                                   (- right left))
                                2)
                      (truncate (- (get-system-metrics :cy-screen)
                                   (- bottom top))
                                2)
                      0
                      0
                      '(:no-size)))))


(defmacro with-double-buffering ((var hwnd) &body body)
  "Evaluate body in a WITH-PAINT context where VAR is bound to an in-memory HDC
which is blitted onto the hwnd's DC as the final step. This prevents flickering 
when drawing lots of small items on the screen."
  (alexandria:with-gensyms (gbm gold gwidth gheight ghdc gps)
    `(with-paint (,hwnd ,ghdc ,gps)
       (let ((,gwidth (rect-right (paintstruct-paint ,gps)))
	     (,gheight (rect-bottom (paintstruct-paint ,gps))))
	 (with-compatible-dc (,var ,ghdc)
	   (let* ((,gbm (create-compatible-bitmap ,ghdc ,gwidth ,gheight))
		  (,gold (select-object ,var ,gbm)))
	     (unwind-protect (progn ,@body)	     
	       (bit-blt ,ghdc 0 0 ,var 0 0 
			:width ,gwidth
			:height ,gheight 
			:raster-op :srccopy)	     
	       (select-object ,var ,gold)
	       (delete-object ,gbm))))))))


(defmacro with-printer-dc ((var device-name &optional document-name) &body body)
  "Evaluate the body in a context with VAR bound to an HDC for the printer named by DEVICE-NAME. 
The body should consist of a series of PRINT-PAGE forms. Any other forms in body are evaulated but 
do not contribute to the page to be printed. 

For examples see examples/printer.
" 
  `(let ((,var (create-dc ,device-name)))
     (unwind-protect
          (macrolet ((print-page (&body body)
                       `(progn (start-page ,',var)
                               ,@body
                               (end-page ,',var))))
            (start-doc ,var ,(or document-name "Document"))
            ,@body
            (end-doc ,var))
       (delete-dc ,var))))
