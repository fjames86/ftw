
;;;; This defines a simple gui which shows current state of the Lisp image
;;;; symbols, memory etc

(defpackage #:ftw.climage
  (:use #:cl #:cffi #:ftw)
  (:export #:climage))

(in-package #:ftw.climage)

(defparameter *windows* nil)

(defclass window ()
  ((hwnd :initarg :hwnd :initform nil :reader window-hwnd)
   (name :initarg :name :reader window-name)
   (id :initarg :id :initform 0 :reader window-id)))
(defmethod print-object ((win window) stream)
  (print-unreadable-object (win stream :type t)
    (format stream ":NAME ~S :ID ~S :HWND ~X" 
	    (window-name win)
	    (window-id win)
	    (when (window-hwnd win) 
	      (pointer-address (window-hwnd win))))))

(defun add-window (win)
  (push win *windows*))
(defun window-by-hwnd (hwnd)
  (find hwnd *windows* :key #'window-hwnd :test #'pointer-eq))
(defun window-by-name (name)
  (find name *windows* :key #'window-name :test #'eq))
(defun window-by-id (id)
  (find id *windows* :key #'window-id :test #'=))

(defvar *pkg-list* nil)
(defvar *sym-list* nil)
(defun get-sym-list (pkg)
  (let ((syms nil))
    (do-external-symbols (sym pkg)
      (pushnew sym syms))
    syms))
(defun change-to-package (pkg)
  (let ((win (window-by-name 'pkg-listbox))
	(index (position pkg *pkg-list* :test #'string-equal)))
    (send-message (window-hwnd win)
		  (const +lb-setcursel+)
		  :wparam index)))
(defun change-to-sym (sym)
  (let ((win (window-by-name 'sym-listbox))
	(index (position sym *sym-list* :test #'string-equal)))
    (send-message (window-hwnd win)
		  (const +lb-setcursel+)
		  :wparam index)))
  
(defun add-menu-bar (hwnd menus)
  (labels ((process-menu (parent menu)
	     (destructuring-bind (type sym flags &key name id children) menu
	       (ecase type
		 (:menu
		  (let ((m (create-menu)))
		    (dolist (child children)
		      (process-menu m child))
		    (append-menu parent flags m name)))
		 (:item
		  (append-menu parent flags (or id 0) name)
		  (add-window (make-instance 'window :name sym :id (or id 0))))))))

    (let ((bar (create-menu)))
      (dolist (menu menus)
	(process-menu bar menu))

      (set-menu hwnd bar))))

(defparameter *id-counter* 0)
(defun genid ()
  (incf *id-counter*)
  *id-counter*)

(defvar *accel* nil)

(defun climage-create (hwnd cs)
  "On creation we do: 
 * add menu
 * add static windows for labels etc
 * add listbox for packages 
 * add listbox for symbols 
"
  (declare (ignore cs))

  (setf *windows* nil)
  
  (add-menu-bar hwnd `((:menu file-menu (:popup) :name "&File"
			      :children 
			      ((:item find-menu-item (:string) 
				      :name ,(format nil "&Find~ACtrl+F" #\tab)
				      :id ,(genid))
			       (:item separator1 (:separator))
			       (:item quit-menu-item (:string) 
                          :name ,(format nil "&Quit~ACtrl+Q" #\tab)
                          :id ,(genid))))
		       (:menu image-menu (:popup) :name "&Image"
			      :children
			      ((:item room-menu-item (:string)
				      :name "&Room"
				      :id ,(genid))
			       (:item separator2 (:separator))
			       (:item threads-menu-item (:string)
				      :name "&Threads" :id ,(genid))))))


  ;; create accelerator table for the find menu item 
  (setf *accel*
        (create-accelerator-table
         `(((:control :virtual-key) :keyf ,(window-id (window-by-name 'find-menu-item)))
           ((:control :virtual-key) :keyq ,(window-id (window-by-name 'quit-menu-item))))))
  
  ;; package listbox
  (let ((h (create-window :static
			  :window-name "Packages"
			  :styles (logior-consts +ws-visible+ +ws-child+)
			  :x 25 :y 25 :width 200 :height 20
			  :parent hwnd)))
    (add-window (make-instance 'window :name 'pkg-label :hwnd h))
    (set-default-font h))
    
  (let* ((id (genid))
	 (h 
	  (create-window :listbox
                     :styles (logior-consts +ws-child+ +ws-visible+ +lbs-notify+ +ws-vscroll+
                                            +ws-tabstop+)
			 :x 25 :y 50 :width 200 :height 350
			 :parent hwnd
			 :menu id)))
    (add-window (make-instance 'window :name 'pkg-listbox :hwnd h :id id))
    (set-default-font h)
      
    (setf *pkg-list* (mapcar #'package-name (list-all-packages)))
    (dolist (pkg *pkg-list*)
      (with-wide-string (s pkg)
	(send-message h (const +lb-addstring+) :lparam s))))
    
  ;; symbol listbox 
  (let ((h (create-window :static
			  :window-name "Symbols"
			  :styles (logior-consts +ws-visible+ +ws-child+)
			  :x 250 :y 25 :width 300 :height 20
			  :parent hwnd)))
    (add-window (make-instance 'window :name 'sym-label :hwnd h))
    (set-default-font h))

  (let* ((id (genid))
	 (h 
	  (create-window :listbox
			 :styles (logior-consts +ws-child+ +ws-visible+ +lbs-notify+ +ws-vscroll+ +ws-tabstop+)
			 :x 250 :y 50 :width 300 :height 350
			 :parent hwnd
			 :menu id)))
    (add-window (make-instance 'window :name 'sym-listbox :hwnd h :id id))
    (set-default-font h)

    (setf *sym-list* (get-sym-list (first *pkg-list*)))
    (dolist (sym *sym-list*)
      (with-wide-string (s (symbol-name sym))
	(send-message h (const +lb-addstring+) :lparam s))))
    
  (let ((h 
	 (create-window :static
			:window-name ""
			:styles (logior-consts +ws-child+ +ws-visible+)
			:x 575 :y 50 :width 400 :height 350
			:parent hwnd)))
    (add-window (make-instance 'window :name 'sym-static :hwnd h))
    (set-default-font h)))  

(defwndproc find-dlgproc (hwnd msg wparam lparam)
  (declare (ignore lparam))
  (switch msg
    ((const +wm-initdialog+)
     1)
    ((const +wm-command+)
     (when (= (loword wparam) 1)
       ;; save away the string the user chose
       (let ((sym (get-window-text (get-dialog-item hwnd 3))))
	 ;; parse the string to retrieve the package
	 (let ((colon-index (position #\: sym)))
	   (when colon-index
	     (change-to-package (subseq sym 0 colon-index)))

	   ;; set to symbol
	   (change-to-sym 
	    (if colon-index
		(subseq sym (1+ colon-index))
		sym))))
       (end-dialog hwnd))
     1)
    (t 
     0)))

(defun climage-command (hwnd wparam lparam)
  (declare (ignore lparam))
  (let ((window (window-by-id (loword wparam))))
    (when window 
      (case (window-name window)
	(quit-menu-item 
	 (send-message hwnd (const +wm-close+)))
	(pkg-listbox 
	 (when (= (hiword wparam) (const +lbn-selchange+))
	   (let ((sel (send-message (window-hwnd window) (const +lb-getcursel+)))
		 (sym-listbox (window-by-name 'sym-listbox)))
	     ;; clear listbox and insert all symbols 
	     (send-message (window-hwnd sym-listbox) (const +lb-resetcontent+))
	     
	     (setf *sym-list* (get-sym-list (nth sel *pkg-list*)))
	     (dolist (sym *sym-list*)
	       (with-wide-string (s (symbol-name sym))
		 (send-message (window-hwnd sym-listbox) (const +lb-addstring+) :lparam s)))
	     (invalidate-rect (window-hwnd sym-listbox) nil t))))
	(sym-listbox 
	 (when (= (hiword wparam) (const +lbn-selchange+))
	   (let ((sel (send-message (window-hwnd window) (const +lb-getcursel+))))
	     ;; print info about the symbol
	     (set-window-text (window-hwnd (window-by-name 'sym-static))
			      (with-output-to-string (s)
				(describe (nth sel *sym-list*) s))))))
	(find-menu-item
	 (dialog-box (callback find-dlgproc)
		     `((:class-name :button
			:id 1
			:x 10 :y 35 :cx 40 :cy 10
			:styles ,(logior-consts +ws-child+ +ws-visible+
						+bs-pushbutton+ +ws-tabstop+)
			:title "OK")
		       (:class-name :static
			:x 10 :y 10 :cx 40 :cy 10
			:id 2
			:styles ,(logior-consts +ws-child+ +ws-visible+
						+ss-left+)
			:title "Symbol")
		       (:class-name :edit
			:x 50 :y 10 :cx 50 :cy 10
			:id 3
			:styles ,(logior-consts +ws-child+ +ws-visible+ +ws-tabstop+)))
		     :hwnd hwnd
		     :styles (logior-consts +ws-popup+ +ws-border+ +ws-sysmenu+
					    +ds-modalframe+ +ws-caption+
					    +ws-visible+
					    +ds-setfont+)
		     :title "Find Dialog"
		     :point-size 10 :font "Tahoma" 
		     :x 50 :y 50 :cx 125 :cy 75))))))

(defun climage-size (hwnd wparam lparam)
  (declare (ignore wparam hwnd))
  (let ((width (loword lparam))
        (height (hiword lparam)))

    (let ((pkg-listbox (window-by-name 'pkg-listbox)))
      (when pkg-listbox
	(set-window-pos (window-hwnd pkg-listbox) :top 25 50 200 (- height 100))))
    
    (let ((sym-listbox (window-by-name 'sym-listbox)))
      (when sym-listbox
	(set-window-pos (window-hwnd sym-listbox) :top 250 50 300 (- height 100))))
    
    (let ((sym-static (window-by-name 'sym-static)))
      (when sym-static 
	(set-window-pos (window-hwnd sym-static) :top 575 50 (- width 600) (- height 100))
	(invalidate-rect (window-hwnd sym-static) nil t)))))

    
(defwndproc climage-wndproc (hwnd msg wparam lparam)
;;  (format t "MSG: ~S WPARAM ~S LPARAM ~S~%" msg wparam lparam)
  (switch msg
    ((const +wm-create+)
     (climage-create hwnd (foreign-createstruct (make-pointer lparam))))
    ((const +wm-command+)
     (climage-command hwnd wparam lparam))
    ((const +wm-size+)
     (climage-size hwnd wparam lparam))
    ((const +wm-destroy+)
     (destroy-accelerator-table *accel*)
     (setf *accel* nil)
     (post-quit-message)))
  (default-window-proc hwnd msg wparam lparam))


(defun climage ()
  (register-class "CLIMAGE" 
                  (callback climage-wndproc)
                  :background (get-sys-color-brush :3d-face)
                  :icon (load-icon :winlogo)
                  :cursor (load-cursor :arrow))
  (let ((hwnd (create-window "CLIMAGE" 
                             :window-name "Common Lisp Image" 
                             :styles '(:overlapped-window :visible)
                             :x 100 :y 100 :width 800 :height 400))
        (msg (make-msg)))
    (unwind-protect
         (progn
           (show-window hwnd)
           (update-window hwnd)
           (do ((done nil))
               (done)
             (let ((r (get-message msg)))
               (cond
                 ((= r 0) (setf done t))
                 ((zerop (translate-accelerator hwnd *accel* msg))
                  (translate-message msg)
                  (dispatch-message msg))))))
      (unregister-class "CLIMAGE"))))



