
(in-package #:ftw)

(defmacro switch (value &rest clauses)
  (let ((gvalue (gensym)))
    `(let ((,gvalue ,value))
       (cond
	 ,@(mapcar (lambda (clause)
		     (destructuring-bind (test &body body) clause
		       (cond
			 ((eq test 't)
			  `(t ,@body))
			 (t
			  `((= ,gvalue ,test) ,@body)))))
		   clauses)))))

(defmacro with-wide-string ((var string) &body body)
  `(with-foreign-string (,var ,string :encoding :ucs-2)
     ,@body))

(defmacro mergeflags (place &rest flags)
  (let ((gs (gensym))
	(gx (gensym))
	(gplace (gensym)))
    `(let ((,gplace ,place))
       (if (listp ,gplace)
	   (let ((,gs 0))
	     (dolist (,gx ,gplace)
	       (setf ,gs
		     (logior ,gs
			     (if (integerp ,gx)
				 ,gx
				 (ecase ,gx
				   ,@(mapcar (lambda (f)
					       `(,(car f) ,(cadr f)))
					     flags))))))
	     ,gs)
	   ,gplace))))

;; we start of by defining all the primitives we need.
;; These include GetMessage and associated structures.

(define-foreign-library user32
  (t (:default "User32")))

(use-foreign-library user32)

(define-foreign-library gdi32
  (t (:default "Gdi32")))

(use-foreign-library gdi32)

(define-foreign-library comctl32
  (t (:default "Comctl32")))

(use-foreign-library comctl32)

(define-foreign-library comdlg32
  (t (:default "ComDlg32")))

(use-foreign-library comdlg32)

;; -------------------- for errors ----------------------------

(defcfun (%format-message "FormatMessageA" :convention :stdcall)
    :uint32
  (flags :uint32)
  (source :pointer)
  (msg-id :uint32)
  (lang-id :uint32)
  (buffer :pointer)
  (size :uint32)
  (args :pointer))

(defun format-message (code)
  "Use FormatMessage to convert the error code into a system-defined string."
  (with-foreign-object (buffer :uint8 1024)
    (let ((n (%format-message #x00001000
			      (null-pointer)
			      code
			      0
			      buffer
			      1024
			      (null-pointer))))
      (if (= n 0)
	  (error "Failed to format message")
	  (foreign-string-to-lisp buffer
				  :count (- n 2))))))

(define-condition win-error (error)
  ((code :initform 0 :initarg :code :reader win-error-code))
  (:report (lambda (condition stream)
	     (format stream "ERROR ~A: ~A" 
		     (win-error-code condition)
		     (format-message (win-error-code condition))))))
	   
(defcfun (%get-last-error "GetLastError" :convention :stdcall) :long)

(defun get-last-error ()
  (let ((code (%get-last-error)))
    (unless (zerop code)
      (error 'win-error :code code))))

(defun memset (p size &optional (val 0))
  (dotimes (i size)
    (setf (mem-aref p :uint8 i) val)))

;; ----------------------------------------------


(defcstruct point
  (x :uint32)
  (y :uint32))
(defun foreign-point (p)
  (list (foreign-slot-value p '(:struct point) 'x)
        (foreign-slot-value p '(:struct point) 'y)))
(defun point-foreign (point p)
  (setf (foreign-slot-value p '(:struct point) 'x)
        (first point)
        (foreign-slot-value p '(:struct point) 'y)
        (second point)))

        
(defctype wparam ;; :pointer)
    #+(or amd64 x64 x86-64):uint64
    #-(or amd64 x64 x86-64):uint32)

;; actually lparam is supposed to be signed 
(defctype lparam
    #+(or amd64 x64 x86-64):uint64    
    #-(or amd64 x64 x86-64):uint32)

(defcstruct msg
  (hwnd :pointer)
  (message :uint32)
  (wparam wparam)
  (lparam lparam)
  (time :uint32)
  (pt (:struct point)))

(defcfun (%get-message "GetMessageW" :convention :stdcall)
    :int32
  (msg :pointer)
  (hwnd :pointer)
  (filter-min :uint32)
  (filter-max :uint32))

(defstruct msg
  hwnd
  message
  wparam
  lparam
  time
  pt)

;; translate msg structs to/from foreign 
(defun msg-foreign (msg p)
  (setf (foreign-slot-value p '(:struct msg) 'hwnd) (msg-hwnd msg)
	(foreign-slot-value p '(:struct msg) 'message) (msg-message msg)
	(foreign-slot-value p '(:struct msg) 'wparam) (msg-wparam msg)
	(foreign-slot-value p '(:struct msg) 'lparam) (msg-lparam msg)
	(foreign-slot-value p '(:struct msg) 'time) (msg-time msg))
  (let ((pt (foreign-slot-pointer p '(:struct msg) 'pt)))
    (setf (foreign-slot-value pt '(:struct point) 'x) (first (msg-pt msg))
	  (foreign-slot-value pt '(:struct point) 'y) (second (msg-pt msg))))
  p)

(defun foreign-msg (p msg)
  (setf (msg-hwnd msg) (foreign-slot-value p '(:struct msg) 'hwnd)
	(msg-message msg) (foreign-slot-value p '(:struct msg) 'message)
	(msg-wparam msg) (foreign-slot-value p '(:struct msg) 'wparam)
	(msg-lparam msg) (foreign-slot-value p '(:struct msg) 'lparam)
	(msg-time msg) (foreign-slot-value p '(:struct msg) 'time))
  (let ((pt (foreign-slot-pointer p '(:struct msg) 'pt)))
    (setf (msg-pt msg)
	  (list (foreign-slot-value pt '(:struct point) 'x)
		(foreign-slot-value pt '(:struct point) 'y))))
  msg)


(defun get-message (msg &optional hwnd filter-min filter-max)
  "Get a message from the message queue and store it in the input msg structure.
HWND ::= window hwnd.
MSG ::= uninitialized msg structure, will be filled in on successful return." 
  (with-foreign-object (m '(:struct msg))
    (let ((res (%get-message m
			     (or hwnd (null-pointer))
			     (or filter-min 0)
			     (or filter-max 0))))
      (when (= res -1) (get-last-error))
      
      (foreign-msg m msg)

      res)))

(defcfun (%translate-message "TranslateMessage" :convention :stdcall)
    :uint32
  (msg :pointer))

(defun translate-message (msg)
  "Translate keypress messages." 
  (with-foreign-object (m '(:struct msg))
    (msg-foreign msg m)
    (%translate-message m)))
      
(defctype lresult
    #+(or amd64 x64 x86-64):uint64
    #-(or amd64 x64 x86-64):uint32)

(defcfun (%dispatch-message "DispatchMessageW" :convention :stdcall)
    lresult 
  (msg :pointer))

(defun dispatch-message (msg)
  "Dispatch message." 
  (with-foreign-object (m '(:struct msg))
    (msg-foreign msg m)
    (%dispatch-message m)))

(defcfun (%post-quit-message "PostQuitMessage" :convention :stdcall)
    :void
  (exit-code :uint32))

(defun post-quit-message (&optional exit-code)
  "Post the quit message to the message queue." 
  (%post-quit-message (or exit-code 0))
  0)

(defcfun (%post-message "PostMessageW" :convention :stdcall)
    :uint32
  (hwnd :pointer)
  (msg :uint32)
  (wparam wparam)
  (lparam lparam))

(defun post-message (hwnd msg &key wparam lparam)
  "Post the message to the specified hwnd and return immediately. 
MSG ::= integer specifying the message to post.
HWND ::= window hwnd to post message to. If not supplied the message is 
posted to the special broadcast hwnd (0xffff). 
WPARAM, LPARAM ::= additional message data.
"
  (%post-message (or hwnd (make-pointer #xffff))
                 msg
                 (or wparam 0)
                 (or lparam 0)))
    
(defcfun (%send-message "SendMessageW" :convention :stdcall)
    :uint32
  (hwnd :pointer)
  (msg :uint32)
  (wparam wparam)
  (lparam lparam))

(defun send-message (hwnd msg &key wparam lparam)
  "Send a message to a specific window and wait for it to be processed. 
This function does not return until the message has been processed. 
MSG ::= message type.
HWND ::= window hwnd. If not supplied the broadcast hwnd (0xffff) is used.
WPARAM, LPARAM ::= additional message data.
" 
  (%send-message (or hwnd (make-pointer #xffff))
		 msg
		 (cond
		   ((null wparam) 0)
		   ((pointerp wparam) (pointer-address wparam))
		   (t wparam))
		 (cond
		   ((null lparam) 0)
		   ((pointerp lparam) (pointer-address lparam))
		   (t lparam))))

(defcfun (%send-message-timeout "SendMessageTimeoutW" :convention :stdcall)
    lparam
  (hwnd :pointer)
  (msg :uint32)
  (wparam wparam)
  (lparam lparam)
  (flags :uint32)
  (timeout :uint32)
  (result :pointer))
       
(defun send-message-timeout (hwnd msg &key wparam lparam flags timeout)
  (with-foreign-object (result :uint32)
    (let ((ret (%send-message-timeout (or hwnd (make-pointer #xffff))
				      msg
				      (or wparam 0)
				      (or lparam 0)
				      (mergeflags flags 
						  (:abort-if-hung 2)
						  (:block 1)
						  (:normal 0)
						  (:no-timeout-if-not-hung 8)
						  (:error-on-exit #x20))
				      (or timeout 0)
				      result)))
      (if (zerop ret)
	  (get-last-error)		 
	  (values ret (mem-aref result :uint32))))))

(defcfun (%register-window-message "RegisterWindowMessageW" :convention :stdcall)
    :uint32
  (name :pointer))

(defun register-window-message (name)
  (with-wide-string (s name)
    (let ((r (%register-window-message s)))
      (if (zerop r)
	  (get-last-error)
	  r))))

(defcfun (%get-active-window "GetActiveWindow" :convention :stdcall)
    :pointer)

(defun get-active-window ()
  "Returns the window hwnd of the current thread's message queue, or nil if there is no window." 
  (let ((p (%get-active-window)))
    (if (null-pointer-p p)
	nil
	p)))

(defmacro defwndproc (name (hwnd umsg wparam lparam) &body body)
  "Define a Window Proc callback for use with a window class definition." 
  `(defcallback ,name lresult
       ((,hwnd :pointer)
	(,umsg :uint32)
	(,wparam wparam)
	(,lparam lparam))
     ,@body))

(defcstruct wndclassex
  (size :uint32)
  (style :uint32)
  (wndproc :pointer) ;; callback defined by defwndproc 
  (cls-extra :int32)
  (wnd-extra :int32)
  (instance :pointer)
  (icon :pointer)
  (cursor :pointer)
  (brush :pointer)
  (menu-name :pointer)
  (class-name :pointer)
  (icon-small :pointer))
  
(defcfun (%register-class "RegisterClassExW" :convention :stdcall)
    :uint16
  (class :pointer))

(defcfun (%get-module-handle "GetModuleHandleW" :convention :stdcall)
    :pointer
  (module-name :pointer))

(defun get-module-handle (&optional module-name)
  (with-wide-string (s (or module-name ""))
    (%get-module-handle (if module-name s (null-pointer)))))

(defcfun (%unregister-class "UnregisterClassW" :convention :stdcall)
    :uint32
  (class-name :pointer)
  (instance :pointer))

(defun unregister-class (class-name)
  (with-wide-string (cls-name class-name)
    (%unregister-class cls-name (get-module-handle))))

(defun register-class (class-name wndproc &key styles icon icon-small cursor background menu-name)

  ;; unregister it first by force
  (unregister-class class-name)
  
  (with-foreign-object (wnd '(:struct wndclassex))
    (with-wide-string (cls-name class-name)
      (memset wnd (foreign-type-size '(:struct wndclassex)))
            
      (setf (foreign-slot-value wnd '(:struct wndclassex) 'size)
	    (foreign-type-size '(:struct wndclassex))
	    
	    (foreign-slot-value wnd '(:struct wndclassex) 'style)
	    (mergeflags styles 
			(:byte-align-client #x1000)
			(:byte-align-window #x2000)
			(:class-dc #x0040)
			(:double-click #x0008)
			(:drop-shadow #x00020000)
			(:global-class #x4000)
			(:hredraw #x0002)
			(:no-close #x0200)
			(:owndc #x0020)
			(:parent-dc #x0080)
			(:save-bits #x0800)
			(:vredraw #x0001)))
      
      (setf (foreign-slot-value wnd '(:struct wndclassex) 'wndproc)
	    wndproc

	    (foreign-slot-value wnd '(:struct wndclassex) 'instance)
	    (get-module-handle)

	    (foreign-slot-value wnd '(:struct wndclassex) 'icon)
	    (or icon (null-pointer))

	    (foreign-slot-value wnd '(:struct wndclassex) 'icon-small)
	    (or icon-small (null-pointer))

	    (foreign-slot-value wnd '(:struct wndclassex) 'cursor)
	    (or cursor (load-cursor :arrow))

	    (foreign-slot-value wnd '(:struct wndclassex) 'brush)
	    (or background (get-sys-color-brush :3d-face))

	    (foreign-slot-value wnd '(:struct wndclassex) 'menu-name)
	    (or menu-name (null-pointer)))
	    

      (setf (foreign-slot-value wnd '(:struct wndclassex) 'class-name)
	    cls-name)

      (%register-class wnd))))

(defcfun (%create-window-ex "CreateWindowExW" :convention :stdcall)
    :pointer
  (ex-style :uint32)
  (class-name :pointer)
  (window-name :pointer)
  (style :uint32)
  (x :uint32)
  (y :uint32)
  (width :uint32)
  (height :uint32)
  (parent :pointer)
  (menu :pointer)
  (instance :pointer)
  (param :pointer))

(defun create-window (class-name &key window-name styles ex-styles x y width height parent menu instance param)
  (with-wide-string (cls-name (cond
				((symbolp class-name)
				 (ecase class-name
				   (:button "BUTTON")
				   (:combobox "COMBOBOX")
				   (:edit "EDIT")
				   (:listbox "LISTBOX")
				   (:mdi-client "MDICLIENT")
				   (:rich-edit "RichEdit")
				   (:rich-edit-class "RICHEXIT_CLASS")
				   (:scroll-bar "SCROLLBAR")
				   (:static "STATIC")
				   (:status "msctls_statusbar32")))
				(t
				 class-name)))
    (with-wide-string (wnd-name (or window-name ""))
      (let ((hwnd
	     (%create-window-ex (mergeflags ex-styles
					    (:accept-files #x10)
					    (:app-window #x40000)
					    (:client-edge #x200)
					    (:composited #x02000000)
					    (:context-help #x400)
					    (:control-parent #x10000)
					    (:dialog-modal-frame #x1)
					    (:layered #x80000)
					    (:layout-rtl #x00400000)
					    (:left 0)
					    (:left-scroll-bar #x4000)
					    (:mdi-child #x40)
					    (:no-activate #x08000000)
					    (:no-inherit-layout #x00100000)
					    (:no-parent-help #x4)
					    (:no-redirection-bitmap #x00200000)
					    (:right #x1000)
					    (:rtl-reading #x2000)
					    (:static-edge #x20000)
					    (:tool-window #x80)
					    (:topmost #x8)
					    (:transparent #x20)
					    (:window-edge #x100))
				cls-name
				(if window-name wnd-name (null-pointer))
				(mergeflags styles
					    (:border #x00800000)
					    (:caption #x00c00000)
					    (:child #x40000000)
					    (:clip-children #x02000000)
					    (:clip-siblings #x04000000)
					    (:disabled #x08000000)
					    (:dialog-frame #x00400000)
					    (:group #x00020000)
					    (:hscroll #x00100000)
					    (:maximize #x01000000)
					    (:maximize-box #x00010000)
					    (:minimize #x20000000)
					    (:minimize-box #x00020000)
					    (:overlapped #x00000000)
					    (:popup #x80000000)
					    (:sizebox #x00040000)
					    (:sysmenu #x00080000)
					    (:tabstop #x00010000)
					    (:visible #x10000000)
					    (:vscroll #x00200000)
					    (:es-center #x0001)
					    (:es-right #x0002)
					    (:es-multiline #x0004)
					    (:ss-bitmap 14)
					    (:bs-checkbox 2)
					    (:tbs-autoticks 1)
					    (:overlapped-window
					     (logior 0
						     #x00c00000
						     #x00080000
						     #x00040000
						     #x00020000
						     #x00010000)))
				(or x +cw-usedefault+)
				(or y +cw-usedefault+)
				(or width +cw-usedefault+)
				(or height +cw-usedefault+)
				(or parent (null-pointer))
				(cond
				  ((null menu) (null-pointer))
				  ((integerp menu) (make-pointer menu))
				  ((pointerp menu) menu)
				  (t (error "Menu must be pointer or integer")))
				(or instance (get-module-handle))
				(or param (null-pointer)))))
	(if (null-pointer-p hwnd)
	    (get-last-error)
	    hwnd)))))

(defcfun (%message-box "MessageBoxW" :convention :stdcall)
    :int32
  (hwnd :pointer)
  (text :pointer)
  (caption :pointer)
  (type :uint32))

(defun message-box (&key hwnd text caption
		      (button :ok) icon default-button modality
		      right-justify topmost service-notification)
  "Create a message box and block until user clicks a button.

HWND ::= parent window hwnd. If not specified the message box has no hwnd.
TEXT ::= msgbox text.
CAPTION ::= msgbox caption.
BUTTON ::= button configuration.
ICON ::= msgbox icon.
DEFAULT-BUTTON ::= button which is selected by default.
MODALITY ::= msgbox modality.
RIGHT-JUSTIFY ::= if true, text is right justified.
TOPMOST ::= if true, msgbox set to topmost window.
SERVICE-NOTIFICATION ::= if true, msgbox is created on desktop by service.

Return is keywork specifying button user clicked." 
  (with-wide-string (text-p (or text ""))
    (with-wide-string (caption-p (or caption ""))
      (let ((res (%message-box (or hwnd (null-pointer))
			       (if text text-p (null-pointer))
			       (if caption caption-p (null-pointer))
			       (let ((type 0))
				 (setf type
				       (logior type
					       (ecase button
						 (:abort-retry-ignore #x00000002)
						 (:cancel-try-continue #x00000006)
						 (:help #x00004000)
						 (:ok #x00000000)
						 (:ok-cancel #x00000001)
						 (:retry-cancel #x00000005)
						 (:yes-no #x00000004)
						 (:yes-no-cancel #x00000003))))
				 (when icon
				   (setf type
					 (logior type
						 (ecase icon
						   (:asterisk #x00000040)
						   (:warning #x00000030)
						   (:question #x00000020)
						   (:error #x00000010)))))
				 
				 (when default-button
				   (setf type
					 (logior type
						 (ecase default-button
						   (:button1 #x00000000)
						   (:button2 #x00000100)
						   (:button3 #x00000200)
						   (:button4 #x00000300)))))
				 
				 (when modality
				   (setf type
					 (logior type
						 (ecase modality
						   (:app-modal #x00000000)
						   (:system-modal #x00001000)
						   (:task-modal #x00002000)))))
				 
				 (when right-justify
				   (setf type (logior type #x00080000)))
				 
				 (when topmost
				   (setf type (logior type #x00040000)))
				 
				 (when service-notification
				   (setf type (logior type #x00200000)))
				 
				 type))))
	(case res
	  (3 :abort)
	  (2 :cancel)
	  (11 :continue)
	  (5 :ignore)
	  (7 :no)
	  (1 :ok)
	  (4 :retry)
	  (10 :try-again)
	  (6 :yes)
	  (otherwise res))))))

(defcfun (%default-window-proc "DefWindowProcW" :convention :stdcall)
    lresult
  (hwnd :pointer)
  (msg :uint32)
  (wparam wparam)
  (lparam lparam))

(defun default-window-proc (hwnd msg wparam lparam)
  (%default-window-proc hwnd msg wparam lparam))

(defcfun (%show-window "ShowWindow" :convention :stdcall)
    :boolean
  (hwnd :pointer)
  (cmd :uint32))

(defun show-window (hwnd &optional cmd)
  (%show-window hwnd
		(ecase (or cmd :show-normal)
		(:force-minimize 11)
		(:hide 0)
		(:maximize 3)
		(:minimize 6)
		(:restore 9)
		(:show 5)
		(:show-default 10)
		(:show-maximized 3)
		(:show-minimized 2)
		(:show-minimized-noactive 7)
		(:show-noactive 8)
		(:show-noactivate 4)
		(:show-normal 1))))

(defcfun (%update-window "UpdateWindow" :convention :stdcall)
    :boolean
  (hwnd :pointer))

(defun update-window (hwnd)
  (%update-window hwnd))

(defcfun (%destroy-window "DestroyWindow" :convention :stdcall)
    :boolean
  (hwnd :pointer))

(defun destroy-window (hwnd)
  (%destroy-window hwnd))

(defcfun (%get-stock-object "GetStockObject" :convention :stdcall)
    :pointer
  (object :uint32))

(defun get-stock-object (object)
  (let ((obj 0))
    (when (symbolp object)
      (setf obj
	    (ecase object
	      (:white-brush 0)
	      (:light-gray-brush 1)
	      (:light-grey-brush 1)
	      (:gray-brush 2)
	      (:grey-brush 2)
	      (:dark-gray-brush 3)
	      (:dark-grey-brush 3)
	      (:black-brush 4)
	      (:null-brush 5)
	      (:white-pen 6)
	      (:black-pen 7)
	      (:null-pen 8)
	      (:oem-fixed-font 10)
	      (:ansi-fixed-font 11)
	      (:ansi-var-font 12)
	      (:system-font 13)
	      (:device-default-font 14)
	      (:default-palette 15)
	      (:system-fixed-font 16)
	      (:default-gui-font 17)
	      (:dc-brush 18)
	      (:dc-pen 19))))
    (let ((b (%get-stock-object obj)))
      (if (null-pointer-p b)
	  nil
	  b))))

(defcfun (%get-sys-color-brush "GetSysColorBrush" :convention :stdcall) :pointer
  (index :int32))

(defcfun (%get-sys-color "GetSysColor" :convention :stdcall) :uint32
  (index :int32))

(defun sys-color-index (name)
  (ecase name
    (:3d-dark-shadow 21)
    (:3d-face 15)
    (:3d-highlight 20)
    (:3d-light 22)
    (:3d-shadow 16)
    (:active-border 10)
    (:active-caption 2)
    (:app-workspace 12)
    (:background 1)
    (:button-face 15)
    (:button-highlight 20)
    (:button-shadow 16)
    (:button-text 18)
    (:caption-text 9)
    (:desktop 1)
    (:gradient-active-caption 27)
    (:gradient-inactive-caption 28)
    (:gray-text 17)
    (:grey-text 17)
    (:highlight 13)
    (:highlight-text 14)
    (:hotlight 26)
    (:inactive-border 11)
    (:inactive-caption 3)
    (:inactive-caption-text 19)
    (:info-background 24)
    (:info-text 23)
    (:menu 4)
    (:menu-highlight 29)
    (:menu-bar 30)
    (:menu-text 7)
    (:scrollbar 0)
    (:window 5)
    (:window-frame 6)
    (:window-text 8)))

(defun get-sys-color-brush (name)
  (%get-sys-color-brush (sys-color-index name)))

(defun get-sys-color (name)
  (%get-sys-color (sys-color-index name)))


(defcfun (%load-icon "LoadIconW" :convention :stdcall)
    :pointer
  (handle :pointer)
  (name :pointer))

(defun load-icon (name &optional handle)
  (cond
    ((symbolp name)
     (%load-icon (or handle (null-pointer))
		 (make-pointer
		  (ecase name
		    (:application 32512)
		    (:asterisk 32516)
		    (:error 32513)
		    (:exclamation 32515)
		    (:hand 32513)
		    (:information 32516)
		    (:question 32514)
		    (:shield 32518)
		    (:warning 32515)
		    (:winlogo 32517)))))
    (t
     (with-wide-string (s name)
       (%load-icon (or handle (null-pointer))
		  s)))))

(defcfun (%load-cursor "LoadCursorW" :convention :stdcall)
    :pointer
  (hinstance :pointer)
  (name :pointer))

(defun load-cursor (name &optional instance)
  (cond
    ((symbolp name)
     (%load-cursor (or instance (null-pointer))
		   (make-pointer 
		    (ecase name
		      (:appstarting 32650)
		      (:arrow 32512)
		      (:cross 32515)
		      (:hand 32649)
		      (:help 32651)
		      (:ibeam 32513)
		      (:icon 32641)
		      (:no 32648)
		      (:size 32640)
		      (:sizeall 32646)
		      (:sizenesw 32643)
		      (:sizens 32645)
		      (:sizenwse 32642)
		      (:sizewe 32644)
		      (:uparrow 32516)
		      (:wait 32514)))))
    (t
     (with-wide-string (s name)
       (%load-cursor (or instance (null-pointer))
		     s)))))

(defcfun (%load-image "LoadImageW" :convention :stdcall)
    :pointer
  (instance :pointer)
  (name :pointer)
  (type :uint32)
  (cx :int32)
  (cy :int32)
  (load :uint32))

(defun load-image (string &key width height instance type flags)
  (unless flags (setf flags '(:load-from-file)))
	  
  (with-wide-string (s string)
    (let ((h (%load-image (or instance (null-pointer))
			  s
			  (ecase (or type :bitmap)
			    (:bitmap 0)
			    (:cursor 2)
			    (:icon 1))
			  (or width 0)
			  (or height 0)
			  (mergeflags flags
				      (:create-dib-section #x2000)
				      (:default-color 0)
				      (:default-size #x40)
				      (:load-from-file #x10)
				      (:load-map-3d-color #x1000)
				      (:load-transparent #x20)
				      (:monochrome #x1)
				      (:shared #x8000)
				      (:vga-color #x80)))))
      (if (null-pointer-p h)
          (error "Failed to load image")
          h))))

(defcfun (%in-send-message "InSendMessage" :convention :stdcall)
    :boolean)

(defun in-send-message-p ()
  (%in-send-message))

(defcfun (%enum-child-windows "EnumChildWindows" :convention :stdcall)
    :boolean
  (hwnd :pointer)
  (proc :pointer)
  (lparam lparam))

(defvar *enum-child-windows* nil)

(defcallback %enum-child-windows-cb :boolean
    ((hwnd :pointer) (lparam lparam))
  (declare (ignore lparam))
  (push hwnd *enum-child-windows*)
  t)

(defun enum-child-windows (&optional hwnd)
  (setf *enum-child-windows* nil)
  (%enum-child-windows (or hwnd (null-pointer))
		       (callback %enum-child-windows-cb)
		       0)
  *enum-child-windows*)
  
(defcfun (%find-window "FindWindowW" :convention :stdcall)
    :pointer
  (class-name :pointer)
  (window-name :pointer))

(defun find-window (class-name window-name)
  (with-wide-string (c class-name)
    (with-wide-string (w window-name)
      (%find-window c w))))

(defcfun (%get-parent "GetParent" :convention :stdcall)
    :pointer
  (hwnd :pointer))

(defun get-parent (hwnd)
  (%get-parent hwnd))

(defcfun (%get-window-info "GetWindowInfo" :convention :stdcall)
    :boolean
  (hwnd :pointer)
  (pwi :pointer))

(defcstruct rect
  (left :int32)
  (top :int32)
  (right :int32)
  (bottom :int32))
(defun make-rect (&key left top right bottom)
  (list :left (or left 0)
	:top (or top 0)
	:right (or right 0)
	:bottom (or bottom 0)))
(defun rect-left (rect) (getf rect :left))
(defun rect-right (rect) (getf rect :right))
(defun rect-top (rect) (getf rect :top))
(defun rect-bottom (rect) (getf rect :bottom))

(defun rect-foreign (rect p)
  (destructuring-bind (&key left top right bottom) rect 
    (setf (foreign-slot-value p '(:struct rect) 'left) (or left 0)
	  (foreign-slot-value p '(:struct rect) 'top) (or top 0)
	  (foreign-slot-value p '(:struct rect) 'right) (or right 0)
	  (foreign-slot-value p '(:struct rect) 'bottom) (or bottom 0)))
  p)
(defun foreign-rect (p rect)
  (setf (getf rect :left)
	(foreign-slot-value p '(:struct rect) 'left)
	(getf rect :top)
	(foreign-slot-value p '(:struct rect) 'top)
	(getf rect :right)
	(foreign-slot-value p '(:struct rect) 'right)
	(getf rect :bottom)
	(foreign-slot-value p '(:struct rect) 'bottom))
  rect)

(defcstruct window-info
  (size :uint32)
  (window (:struct rect))
  (client (:struct rect))
  (style :uint32)
  (ex-style :uint32)
  (status :uint32)
  (x-borders :uint32)
  (y-borders :uint32)
  (type :uint16)
  (version :uint16))

(defstruct info
  size window client style ex-style status borders type version)

(defun get-window-info (hwnd)
  (with-foreign-object (pwi '(:struct window-info))
    (unless (%get-window-info hwnd pwi)
      (get-last-error))

    (make-info :size (foreign-slot-value pwi '(:struct window-info) 'size)
	       :window (let ((p (foreign-slot-pointer pwi '(:struct window-info) 'window)))
			 (list :left (foreign-slot-value p '(:struct rect) 'left)
			       :top (foreign-slot-value p '(:struct rect) 'top)
			       :right (foreign-slot-value p '(:struct rect) 'right)
			       :bottom (foreign-slot-value p '(:struct rect) 'bottom)))
	       :client (let ((p (foreign-slot-pointer pwi '(:struct window-info) 'client)))
			 (list :left (foreign-slot-value p '(:struct rect) 'left)
			       :top (foreign-slot-value p '(:struct rect) 'top)
			       :right (foreign-slot-value p '(:struct rect) 'right)
			       :bottom (foreign-slot-value p '(:struct rect) 'bottom)))
	       :style (foreign-slot-value pwi '(:struct window-info) 'style)
	       :ex-style (foreign-slot-value pwi '(:struct window-info) 'ex-style)
	       :status (foreign-slot-value pwi '(:struct window-info) 'status)
	       :borders (list (foreign-slot-value pwi '(:struct window-info) 'x-borders)
			      (foreign-slot-value pwi '(:struct window-info) 'y-borders))
	       :type (foreign-slot-value pwi '(:struct window-info) 'type)
	       :version (foreign-slot-value pwi '(:struct window-info) 'version))))
	       

(defcfun (%move-window "MoveWindow" :convention :stdcall)
    :boolean
  (hwnd :pointer)
  (x :int32)
  (y :int32)
  (width :int32)
  (height :int32)
  (repaint :boolean))

(defun move-window (hwnd x y width height &optional repaint)
  (%move-window hwnd x y width height repaint))


(defcfun (%set-window-text "SetWindowTextW" :convention :stdcall)
    :boolean
  (hwnd :pointer)
  (string :pointer))

(defun set-window-text (hwnd text)
  (with-wide-string (s text)
    (%set-window-text hwnd s)))

(defcfun (%set-window-pos "SetWindowPos" :convention :stdcall)
    :boolean
  (hwnd :pointer)
  (hwnd-insert-after :pointer)
  (x :int32)
  (y :int32)
  (cx :int32)
  (cy :int32)
  (flags :uint32))

(defun negval (value)
  (+ (1+ value)
     #+(or amd64 x64 x86-64)#xffffffffffffffff
     #-(or amd64 x64 x86-64)#xffffffff))
  
(defun set-window-pos (hwnd hwnd-insert-after x y cx cy &optional flags)
  (%set-window-pos hwnd
		   (cond
		     ((pointerp hwnd-insert-after)
		      hwnd-insert-after)
		     ((keywordp hwnd-insert-after)		       
		      (make-pointer (ecase hwnd-insert-after
				      (:no-topmost (negval -2))
				      (:topmost (negval -1))
				      (:top 0)
				      (:bottom 1))))
		     ((null hwnd-insert-after) (null-pointer))
		     (t (error "HWND-INSERT-AFTER must be an hwnd or symbol.")))
		   x y cx cy
		   (mergeflags flags
			       (:async-window-pos #x4000)
			       (:defer-erase #x2000)
			       (:draw-frame #x20)
			       (:frame-changed #x20)
			       (:hide-window #x80)
			       (:no-activate #x10)
			       (:no-copy-bits #x100)
			       (:no-move #x2)
			       (:no-owner-zorder #x200)
			       (:no-redraw #x8)
			       (:no-reposition #x200)
			       (:no-send-changing #x400)
			       (:no-size 1)
			       (:no-zorder 4)
			       (:show-window #x40))))

(defcfun (%invalidate-rect "InvalidateRect" :convention :stdcall)
    :boolean
  (hwnd :pointer)
  (rect :pointer)
  (erase :boolean))

(defun invalidate-rect (hwnd &optional rect erase)
  (with-foreign-object (r '(:struct rect))
    (when rect (rect-foreign rect r))
    (%invalidate-rect (or hwnd (null-pointer))
		      (if rect r (null-pointer))
		      erase)))

(defcfun (%begin-paint "BeginPaint" :convention :stdcall)
    :pointer
  (hwnd :pointer)
  (info :pointer))

(defcstruct paintstruct
  (hdc :pointer)
  (erase :boolean)
  (paint (:struct rect))
  (restore :boolean)
  (inc-update :boolean)
  (reserved :uint8 :count 32))

(defstruct paintstruct
  hdc erase paint restore inc-update reserved)
(defun paintstruct-foreign (ps p)
  (setf (foreign-slot-value p '(:struct paintstruct) 'hdc)
	(paintstruct-hdc ps)
	(foreign-slot-value p '(:struct paintstruct) 'erase)
	(paintstruct-erase ps)
	(foreign-slot-value p '(:struct paintstruct) 'restore)
	(paintstruct-restore ps)
	(foreign-slot-value p '(:struct paintstruct) 'inc-update)
	(paintstruct-inc-update ps))
  (let ((rect (paintstruct-paint ps))
	(r (foreign-slot-pointer p '(:struct paintstruct) 'paint)))
    (rect-foreign rect r))
  (let ((rs (foreign-slot-pointer p '(:struct paintstruct) 'reserved)))
    (dotimes (i 32)
      (setf (mem-aref rs :uint8 i)
	    (aref (paintstruct-reserved ps) i))))
  p)
(defun foreign-paintstruct (p ps)
  (setf (paintstruct-hdc ps)
	(foreign-slot-value p '(:struct paintstruct) 'hdc)
	(paintstruct-erase ps)
	(foreign-slot-value p '(:struct paintstruct) 'erase)
	(paintstruct-paint ps)
	(foreign-slot-value p '(:struct paintstruct) 'restore)
	(paintstruct-inc-update ps)
	(foreign-slot-value p '(:struct paintstruct) 'inc-update))
  (let ((r (foreign-slot-pointer p '(:struct paintstruct) 'paint))
	(rect (make-rect)))
    (foreign-rect r rect)
    (setf (paintstruct-paint ps) rect))
  (let ((rs (foreign-slot-pointer p '(:struct paintstruct) 'reserved)))
    (dotimes (i 32)
      (setf (aref (paintstruct-reserved ps) i)
	    (mem-aref rs :uint8 i))))
  ps)
	
(defun begin-paint (hwnd)
  (with-foreign-object (p '(:struct paintstruct))
    (let ((res (%begin-paint hwnd p))
	  (info (make-paintstruct :reserved
				  (make-array 32 :element-type '(unsigned-byte 8)))))
      (foreign-paintstruct p info)
      (values res info))))
    

(defcfun (%end-paint "EndPaint" :convention :stdcall)
    :boolean
  (hwnd :pointer)
  (info :pointer))

(defun end-paint (hwnd info)
  (with-foreign-object (p '(:struct paintstruct))
    (paintstruct-foreign info p)
    (%end-paint hwnd p)))

(defmacro with-paint ((hwnd hdc-var &optional ps-var) &body body)
  (let ((gps (or ps-var (gensym)))
        (ghwnd (gensym)))
    `(let ((,ghwnd ,hwnd))
       (multiple-value-bind (,hdc-var ,gps) (begin-paint ,ghwnd)
         (unwind-protect (progn ,@body)
           (end-paint ,ghwnd ,gps))))))

(defcfun (%draw-caption "DrawCaption" :convention :stdcall)
    :boolean
  (hwnd :pointer)
  (hdc :pointer)
  (rect :pointer)
  (flags :uint32))

(defun draw-caption (hwnd hdc rect &optional flags)
  (with-foreign-object (r '(:struct rect))
    (rect-foreign rect r)
    (%draw-caption hwnd hdc r
		   (mergeflags flags
			       (:active #x1)
			       (:buttons #x1000)
			       (:gradient #x20)
			       (:icon #x4)
			       (:inbutton #x10)
			       (:smallcap #x2)
			       (:text #x8)))))

(defcfun (%draw-focus-rect "DrawFocusRect" :convention :stdcall)
    :boolean
  (hdc :pointer)
  (rect :pointer))

(defun draw-focus-rect (hdc rect)
  (with-foreign-object (r '(:struct rect))
    (rect-foreign rect r)
    (%draw-focus-rect hdc r)))


(defcfun (%draw-edge "DrawEdge" :convention :stdcall)
    :boolean
  (hdc :pointer)
  (rect :pointer)
  (edge :uint32)
  (flags :uint32))

(defun draw-edge (hdc rect &key inner-edge outer-edge flags)
  (with-foreign-object (r '(:struct rect))
    (rect-foreign rect r)
    (%draw-edge hdc
		r 
		(logior (ecase (or inner-edge :raised)
			  (:raised 4)
			  (:sunk 8))
			(ecase (or outer-edge :raised)
			  (:raised 1)
			  (:sunk 2)))
		(mergeflags flags
			    (:left #x01)
			    (:top #x02)
			    (:right #x04)
			    (:bottom #x08)
			    (:diagonal #x10)
			    (:middle #x800)
			    (:soft #x1000)
			    (:adjust #x2000)
			    (:flat #x4000)
			    (:mono #x8000)			    
			    (:rect #x0f)))))
    
(defcfun (%redraw-window "RedrawWindow" :convention :stdcall)
    :boolean
  (hwnd :pointer)
  (rect :pointer)
  (hrgn :pointer)
  (flags :uint32))

(defun redraw-window (hwnd &key rect hrgn flags)
  (with-foreign-object (r '(:struct rect))
    (when rect (rect-foreign rect r))
    (%redraw-window hwnd
		    (if rect r (null-pointer))
		    (if hrgn hrgn (null-pointer))
		    (mergeflags flags
				(:erase 4)
				(:frame 1024)
				(:interal-paint 2)
				(:invalidate 1)
				(:no-erase 32)
				(:no-frame 2048)
				(:no-internal-paint 16)
				(:validate 8)
				(:erase-now 512)
				(:update-now 256)
				(:all-children 128)
				(:no-children 64)))))

(defcfun (%text-out "TextOutW") :boolean
  (hdc :pointer)
  (x :int32)
  (y :int32)
  (text :pointer)
  (count :int32))

(defun text-out (hdc text x y)
  (with-wide-string (s text)
    (%text-out hdc x y s (length text))))

(defcfun (%get-client-rect "GetClientRect" :convention :stdcall)
    :boolean
  (hwnd :pointer)
  (rect :pointer))

(defun get-client-rect (hwnd)
  (with-foreign-object (r '(:struct rect))
    (%get-client-rect hwnd r)
    (foreign-rect r (make-rect))))

(defcfun (%flash-window-ex "FlashWindowEx" :convention :stdcall)
    :boolean
  (info :pointer))

(defcstruct flashinfo
  (size :uint32)
  (hwnd :pointer)
  (flags :uint32)
  (count :uint32)
  (timeout :uint32))

(defun flash-window (hwnd &key flags count timeout)
  (unless flags (setf flags '(:all)))
  (with-foreign-object (info '(:struct flashinfo))
    (setf (foreign-slot-value info '(:struct flashinfo) 'size)
	  (foreign-type-size '(:struct flashinfo))
	  (foreign-slot-value info '(:struct flashinfo) 'hwnd)
	  hwnd
	  (foreign-slot-value info '(:struct flashinfo) 'flags)
	  (mergeflags flags
		      (:all 3)
		      (:caption 1)
		      (:stop 0)
		      (:timer 4)
		      (:timer-no-foreground #xc)
		      (:tray 2))
	  (foreign-slot-value info '(:struct flashinfo) 'count)
	  (or count 1)
	  (foreign-slot-value info '(:struct flashinfo) 'timeout)
	  (or timeout 0))
    (%flash-window-ex info)))

(defcfun (%message-beep "MessageBeep" :convention :stdcall)
    :boolean
  (type :uint32))

(defun message-beep (&optional type)
  (%message-beep (ecase (or type :simple)
		   (:simple #xffffffff)
		   (:information #x40)
		   (:warning #x30)
		   (:question #x20)
		   (:error #x10)
		   (:ok 0))))

(defcfun (%client-to-screen "ClientToScreen" :convention :stdcall) :boolean
  (hwnd :pointer)
  (point :pointer))

(defun client-to-screen (hwnd x y)
  (with-foreign-object (p '(:struct point))
    (setf (foreign-slot-value p '(:struct point) 'x) x
	  (foreign-slot-value p '(:struct point) 'y) y)
    (%client-to-screen hwnd p)
    (list (foreign-slot-value p '(:struct point) 'x)
	  (foreign-slot-value p '(:struct point) 'y))))

(defcfun (%get-window-rect "GetWindowRect" :convention :stdcall)
    :boolean
  (hwnd :pointer)
  (rect :pointer))

(defun get-window-rect (hwnd)
  (with-foreign-object (r '(:struct rect))
    (let ((result (%get-window-rect hwnd r))
	  (rect (make-rect)))
      (cond
	(result 
	 (foreign-rect r rect)
	 rect)
	(t
	 (get-last-error))))))

(defcfun (%get-system-metrics "GetSystemMetrics" :convention :stdcall)
    :int32
  (index :int32))

(defun get-system-metrics (name)
  (%get-system-metrics
   (ecase name
     (:arrange 56)
     (:clean-boot 67)
     (:monitors 80)
     (:mouse-buttons 43)
     (:convertible-slate-mode #x2003)
     (:cx-border 5)
     (:sx-cursor 13)
     (:sx-dialog-frame 7)
     (:sx-double-click 36)
     (:cx-drag 68)
     (:cx-edge 45)
     (:cx-fixed-frame 45)
     (:cx-focus-border 83)
     (:cx-frame 32)
     (:cx-fullscreen 16)
     (:cx-hscroll 21)
     (:cx-hthumb 10)
     (:cx-icon 11)
     (:cx-icon-spacing 38)
     (:cx-maximized 61)
     (:cx-maxtrack 59)
     (:cx-menucheck 71)
     (:cx-menusize 54)
     (:cx-min 28)
     (:cx-minimized 57)
     (:cx-minspacing 47)
     (:cx-mintrack 34)
     (:cx-padded-border 92)
     (:cx-screen 0)
     (:cx-size 30)
     (:cx-sizeframe 32)
     (:cx-small-icon 49)
     (:cx-small-size 52)
     (:cx-virtual-screen 78)
     (:cx-vscroll 2)
     (:cy-border 6)
     (:cy-caption 4)
     (:cy-cursor 14)
     (:cy-dialog-frame 8)
     (:cy-double-click 37)
     (:cy-drag 69)
     (:cy-edge 46)
     (:cy-fixed-frame 8)
     (:cy-focus-border 84)
     (:cy-frame 33)
     (:cy-fullscreen 17)
     (:cy-hscroll 3)
     (:cy-icon 12)
     (:cy-icon-spacing 39)
     (:cy-kanji-window 18)
     (:cy-maximized 62)
     (:cy-maxtrack 60)
     (:cy-menu 15)
     (:cy-menu-check 72)
     (:cy-menu-size 55)
     (:cy-min 29)
     (:cy-minimized 58)
     (:cy-minspacing 48)
     (:cy-mintrack 35)
     (:cy-screen 1)
     (:cy-size 31)
     (:cy-sizeframe 33)
     (:cy-small-caption 51)
     (:cy-small-icon 50)
     (:cy-small-size 53)
     (:cy-virtual-screen 79)
     (:cy-vscroll 20)
     (:cy-vthumb 9)
     (:dbcs-enabled 42)
     (:debug 22)
     (:digitizer 94)
     (:imm-enabled 82)
     (:maximum-touches 95)
     (:media-center 87)
     (:menu-drop-alignment 40)
     (:mideast-enabled 74)
     (:mouse-present 19)
     (:mouse-horizonal-wheel-present 91)
     (:mouse-wheel-present 75)
     (:network 63)
     (:pen-windows 41)
     (:remote-control #x2001)
     (:remote-session #x1000)
     (:same-display-format 81)
     (:secure 44)
     (:server-2008-r2 89)
     (:show-sounds 70)
     (:shutting-down #x2000)
     (:slow-machine 73)
     (:starter 88)
     (:swap-button 23)
     (:system-locked #x2004)
     (:tablet-pc 86)
     (:xvirtualscreen 76)
     (:yvirtualscreen 77))))
		  


(defcfun (%register-hot-key "RegisterHotKey" :convention :stdcall)
    :boolean
  (hwnd :pointer)
  (id :int32)
  (fsModifiers :uint32)
  (vk :uint32))

(defun register-hot-key (hwnd id key &optional modifiers)
  (%register-hot-key hwnd
		     id
		     (mergeflags modifiers
				 (:alt 1)
				 (:control 2)
				 (:no-repeat #x4000)
				 (:shift 4)
				 (:win 8))
		     key))

(defcfun (%unregister-hot-key "UnregisterHotKey" :convention :stdcall)
    :boolean
  (hwnd :pointer)
  (id :int32))

(defun unregister-hot-key (hwnd id)
  (%unregister-hot-key hwnd id))


(defcfun (%create-solid-brush "CreateSolidBrush" :convention :stdcall) :pointer
  (color :uint32))

(defun encode-rgb (r g b)
  (logior r (ash g 8) (ash b 16)))
(defun decode-rdb (rgb)
  (values (logand rgb #xff)
	  (ash (logand rgb #xff00) -8)
	  (ash (logand rgb #xff0000) -16)))

(defun create-solid-brush (rgb)
  (%create-solid-brush rgb))

(defcfun (%create-pen "CreatePen" :convention :stdcall) :pointer
  (style :int32)
  (width :int32)
  (color :uint32))

(defun create-pen (style color &optional width)
  (%create-pen (mergeflags style
                           (:solid 0)
                           (:dash 1)
                           (:dot 2)
                           (:dash-dot 3)
                           (:dash-dot-dot 4)
                           (:null 5)
                           (:inside-frame 6))
               (or width 0)
               color))
		 


(defvar *virtual-keys*
  '((:lbutton 1)
    (:rbutton 2)
    (:cancel 3)
    (:mbutton 4)
    (:xbutton1 5)
    (:xbutton 2 6)
    (:back 8)
    (:tab 9)
    (:clear #xc)
    (:return #xd)
    (:shift #x10)
    (:control #x11)
    (:menu #x12)
    (:pause #x13)
    (:capital #x14)
    (:kana #x15)
    (:escape #x1b)
    (:accept #x1e)
    (:space #x20)
    (:prior #x21)
    (:next #x22)
    (:end #x23)
    (:home #x24)
    (:left #x25)
    (:up #x26)
    (:right #x27)
    (:down #x28)
    (:select #x29)
    (:print #x2a)
    (:execute #x2b)
    (:snapshot #x2c) ;; print screen key 
    (:insert #x2d)
    (:delete #x2e)
    (:help #x2f)
    (:key0 #x30)
    (:key1 #x31)
    (:key2 #x32)
    (:key3 #x33)
    (:key4 #x34)
    (:key5 #x35)
    (:key6 #x36)
    (:key7 #x37)
    (:key8 #x38)
    (:key9 #x39)
    (:keya #x41)
    (:keyb #x42)
    (:keyc #x43)
    (:keyd #x44)
    (:keye #x45)
    (:keyf #x46)
    (:keyg #x47)
    (:keyh #x48)
    (:keyi #x49)
    (:keyj #x4a)
    (:keyk #x4b)
    (:keyl #x4c)
    (:keym #x4d)
    (:keyn #x4e)
    (:keyo #x4f)
    (:keyp #x50)
    (:keyq #x51)
    (:keyr #x52)
    (:keys #x53)
    (:keyt #x54)
    (:keyu #x55)
    (:keyv #x56)
    (:keyw #x57)
    (:keyx #x58)
    (:keyy #x59)
    (:keyz #x5a)
    (:lwin #x5b)
    (:rwin #x5c)
    (:apps #x5d)
    (:sleep #x5f)
    (:numpad0 #x60)
    (:numpad1 #x61)
    (:numpad2 #x62)
    (:numpad3 #x63)
    (:numpad4 #x64)
    (:numpad5 #x65)
    (:numpad6 #x66)
    (:numpad7 #x67)
    (:numpad8 #x68)
    (:numpad9 #x69)
    (:multiply #x6a)
    (:add #x6b)
    (:separator #x6c)
    (:subtract #x6d)
    (:decimal #x6e)
    (:divide #x6f)
    (:f1 #x70)
    (:f2 #x71)
    (:f3 #x72)
    (:f4 #x73)
    (:f5 #x74)
    (:f6 #x75)
    (:f7 #x76)
    (:f8 #x77)
    (:f9 #x78)
    (:f10 #x79)
    (:f11 #x7a)
    (:f12 #x7b)
    (:f13 #x7c)
    (:f14 #x7d)
    (:f15 #x7e)
    (:f16 #x7f)
    (:f17 #x80)
    (:f18 #x81)
    (:f19 #x82)
    (:f20 #x83)
    (:f21 #x84)
    (:f22 #x85)
    (:f23 #x86)
    (:f24 #x87)
    (:numlock #x90)
    (:scrolllock 91)
    (:lshift #xa0)
    (:rshift #xa1)
    (:lcontrol #xa2)
    (:rcontrol #xa3)
    (:lmenu #xa4)
    (:rmenu #xa5)
    (:browser-back #xa6)
    (:browser-forward #xa7)
    (:browser-refresh #xa8)
    (:browser-stop #xa9)
    (:browser-search #xaa)
    (:browser-favorite #xab)
    (:browser-home #xac)
    (:volume-mute #xad)
    (:volume-down #xae)
    (:volume-up #xaf)
    (:plus #xbb)
    (:comma #xbc)
    (:minus #xbd)
    (:period #xbe)
    (:oem1 #xba) ;; ;: on most us keyboards
    (:oem2 #xbf) ;; /?
    (:oem3 #xc0) ;; ~
    (:oem4 #xdb) ;; [{
    (:oem5 #xdc) ;; \|
    (:oem6 #xdd) ;; ]}
    (:oem7 #xde) ;; '" on us keyboards
    (:oem8 #xdf)
    (:oem10 #xe2)))

(defun virtual-key-code (name)
  (let ((code (find name *virtual-keys* :key #'car)))
    (second code)))

(defun virtual-code-key (code)
  (let ((name (find code *virtual-keys* :key #'second)))
    (if name
	(car name)
	code)))


(defcfun (%create-menu "CreateMenu" :convention :stdcall) :pointer)
(defun create-menu ()
  (%create-menu))

(defcfun (%append-menu "AppendMenuW" :convention :stdcall) :boolean
  (menu :pointer)
  (flags :uint32)
  (new-item :pointer)
  (name :pointer))

(defun append-menu (menu flags new-item &optional name)
  (with-wide-string (s (or name ""))
    (%append-menu menu
		  (mergeflags flags
			      (:bitmap #x4)
			      (:checked #x8)
			      (:disabled #x2)
			      (:enabled #x0)
			      (:grayed #x1)
			      (:menubar-break #x20)
			      (:menu-break #x40)
			      (:owner-draw #x100)
			      (:popup #x10)
			      (:separator #x800)
			      (:string 0)
			      (:unchecked 0))
		  (cond
		    ((integerp new-item)
		     (make-pointer new-item))
		    ((pointerp new-item)
		     new-item)
		    (t (error "new-item must be integer or pointer")))
		  s)))


(defcfun (%set-menu "SetMenu" :convention :stdcall)
    :boolean
  (hwnd :pointer)
  (menu :pointer))

(defun set-menu (hwnd menu)
  (%set-menu hwnd menu))
			   
(defcfun (%create-popup-menu "CreatePopupMenu" :convention :stdcall)
    :pointer)

(defun create-popup-menu ()
  (%create-popup-menu))

(defcfun (%delete-menu "DeleteMenu" :convention :stdcall)
    :boolean
  (menu :pointer)
  (pos :uint32)
  (flags :uint32))

(defun delete-menu (menu pos &optional by-position)
  (%delete-menu menu
		pos
		(if by-position #x400 0)))

(defcfun (%destroy-menu "DestroyMenu" :convention :stdcall) :boolean
  (menu :pointer))

(defun destroy-menu (menu)
  (%destroy-menu menu))
  
(defcfun (%track-popup-menu-ex "TrackPopupMenuEx" :convention :stdcall)
    :boolean
  (menu :pointer)
  (flags :uint32)
  (x :int32)
  (y :int32)
  (hwnd :pointer)
  (params :pointer))

(defcstruct tpmparams
  (size :uint32)
  (exclude (:struct rect)))

(defun track-popup-menu (menu x y hwnd &key flags exclude-rect)
  (with-foreign-object (params '(:struct tpmparams))
    (%track-popup-menu-ex menu
			  (mergeflags flags
				      (:center-align #x4)
				      (:left-align 0)
				      (:right-align #x08)
				      (:bottom-align #x20)
				      (:top-align 0)
				      (:vcenter-align #x10)
				      (:no-notify #x80)
				      (:return-command #x100)
				      (:left-button 0)
				      (:right-button #x02)
				      (:right-to-left-animation #x800)
				      (:left-to-right-animation #x400)
				      (:no-animation #x4000)
				      (:bottom-to-top-animation #x2000)
				      (:top-to-bottom-animation #x1000)
				      (:horizontal 0)
				      (:vertical #x40))
			  x
			  y
			  hwnd
			  (cond
			    (exclude-rect
			     (setf (foreign-slot-value params
						       '(:struct tpmparams)
						       'size)
				   (foreign-type-size '(:struct tpmparams)))
			     (rect-foreign exclude-rect
					   (foreign-slot-pointer params
								 '(:struct tpmparams)
								 'params)))
			    (t (null-pointer))))))




(defcfun (%check-menu-item "CheckMenuItem" :convention :stdcall)
    :uint32
  (menu :pointer)
  (item :uint32)
  (check :uint32))

(defun check-menu-item (menu item &optional checked by-position)
  (%check-menu-item menu item
		    (cond
		      (by-position
		       (logior #x400 
			       (if checked #x8 0)))
		      (t (if checked 8 0)))))

(defcfun (%check-menu-radio-item "CheckMenuRadioItem" :convention :stdcall)
    :boolean
  (menu :pointer)
  (first :uint32)
  (last :uint32)
  (check :uint32)
  (flags :uint32))

(defun check-menu-radio-item (menu first-id last-id check-id &optional by-position)
  (%check-menu-radio-item menu
			  first-id
			  last-id
			  check-id
			  (if by-position #x400 0)))
			      

(defcfun (%get-menu-state "GetMenuState" :convention :stdcall)
    :uint32
  (menu :pointer)
  (id :uint32)
  (flags :uint32))

(defun get-menu-state (menu id &optional by-position)
  (let ((res (%get-menu-state menu
			      id
			      (if by-position #x400 0))))
    (when (= res #xffffffff) (return-from get-menu-state nil))

    (let ((flags nil))
      (dolist (pair '((:checked #x8)
		      (:disabled #x2)
		      (:grayed 1)
		      (:highligt #x80)
		      (:menu-break #x40)
		      (:owner-draw #x100)
		      (:popup #x10)
		      (:separator #x800)))
	(unless (zerop (logand res (second pair)))
	  (push (first pair) flags)))
      (when (zerop (logand res (logior #x2 #x1)))
	(push :enabled flags))
      (when (zerop (logand res (logior #x4 #x100)))
	(push :string flags))
      (when (zerop (logand res #x8))
	(push :unchecked flags))
      (when (zerop (logand res #x80))
	(push :unhighlight flags))
      flags)))

(defcfun (%init-common-controls "InitCommonControls" :convention :stdcall) :void)
(defcfun (%init-common-controls-ex "InitCommonControlsEx" :convention :stdcall) :boolean
  (ctls :pointer))
(defcstruct initcommoncontrolsex
  (size :uint32)
  (id :uint32))
(defun init-common-controls (&optional ids)
  (cond
    ((null ids) (%init-common-controls))
    (t
     (unless (listp ids) (setf ids (list ids)))
     (with-foreign-object (c '(:struct initcommoncontrolsex))
       (setf (foreign-slot-value c '(:struct initcommoncontrolsex) 'size)
	     (foreign-type-size '(:struct initcommoncontrolsex))
	     (foreign-slot-value c '(:struct initcommoncontrolsex) 'id)
	     (mergeflags ids
			 (:animate 128)
			 (:bar 4)
			 (:cool 1024)
			 (:date 256)
			 (:hotkey 64)
			 (:internet 2048)
			 (:link #x8000)
			 (:listview 1)
			 (:native-font-control 8192)
			 (:page-scroller 4096)
			 (:progress 32)
			 (:standard #x4000)
			 (:tab 8)
			 (:updown 16)
			 (:comboboxex 512)
			 (:win95 255)))
       (%init-common-controls-ex c)))))
			  
(defcfun (%choose-color "ChooseColorW" :convention :stdcall) :boolean
  (p :pointer))

(defcstruct choosecolor
  (size :uint32)
  (owner :pointer)
  (instance :pointer)
  (result :uint32)
  (custom-colors :pointer)
  (flags :uint32)
  (data lparam)
  (proc :pointer)
  (name :pointer))

(defun choose-color (&key hwnd instance initial-color custom-colors flags)
  (with-foreign-object (c '(:struct choosecolor))
    (with-foreign-object (cc :uint32 16)
      (memset c (foreign-type-size '(:struct choosecolor)))
      (memset cc (* 16 (foreign-type-size :uint32)))

      (when custom-colors
	(do ((i 0 (1+ i))
	     (clist custom-colors (cdr clist)))
	    ((or (= i 16) (null clist)))
	  (setf (mem-aref cc :uint32 i) (car clist))))

      (setf (foreign-slot-value c '(:struct choosecolor) 'size)
	    (foreign-type-size '(:struct choosecolor))

	    (foreign-slot-value c '(:struct choosecolor) 'owner)
	    (or hwnd (null-pointer))

	    (foreign-slot-value c '(:struct choosecolor) 'instance)
	    (or instance (null-pointer))

	    (foreign-slot-value c '(:struct choosecolor) 'result)
	    (or initial-color (encode-rgb 0 0 0))

	    (foreign-slot-value c '(:struct choosecolor) 'custom-colors)
	    cc
	    
	    (foreign-slot-value c '(:struct choosecolor) 'flags)
	    (mergeflags flags
			(:any-color #x100)
			(:full-open 2)
			(:prevent-full-open 4)
			(:rgb-init 1)
			(:show-help 8)
			(:solid-color #x80)))

      (let ((result (%choose-color c)))
	(values result
		(foreign-slot-value c '(:struct choosecolor) 'result)
		(do ((i 0 (1+ i))
		     (clist nil))
		    ((= i 16) (nreverse clist))
		  (push (mem-aref cc :uint32 i) clist)))))))



(defcfun (%get-open-file-name "GetOpenFileNameW" :convention :stdcall)
    :boolean
  (p :pointer))

(defcfun (%get-save-file-name "GetSaveFileNameW" :convention :stdcall)
    :boolean
  (p :pointer))

(defcstruct openfilename
  (size :uint32)
  (owner :pointer)
  (instance :pointer)
  (filter :pointer)
  (custom-filter :string)
  (max-custom-filter :uint32)
  (filter-index :uint32)
  (file :pointer)
  (max-file :uint32)
  (file-title :pointer)
  (max-file-title :uint32)
  (initial-dir :pointer)
  (title :pointer)
  (flags :uint32)
  (file-offset :uint16)
  (file-extension :uint16)
  (def-ext :pointer)
  (cust-data lparam)
  (hook :pointer)
  (template-name :pointer)
  (reserved1 :pointer)
  (reserved2 :uint32)
  (flags-ex :uint32))

(defun extract-null-strings (buffer length)
  (do ((i 1 (1+ i))
       (start 0)
       (strings nil))
      ((>= i length) (nreverse strings))
    (when (= (mem-aref buffer :uint16 i) 0)
      (cond
	((= start i)
	 (setf i length))
	(t 
	 (push (foreign-string-to-lisp buffer
				       :offset (* start 2)
				       :count (* (- i start) 2)
				       :encoding :ucs-2le)
	       strings)
	 (setf start (1+ i)))))))

(defun setup-openfilename (ofn custom-filter-buffer file-name fstr
			   &key hwnd instance filters custom-filter file
				 filter-index initial-dir title flags)
  (when filters
    (lisp-string-to-foreign (with-output-to-string (s)
			      (dolist (filter filters)
				(destructuring-bind (text filt) filter
				  (princ text s)
				  (princ (code-char 0) s)
				  (princ filt s)
				  (princ (code-char 0) s)))
			      (princ (code-char 0) s)
			      (princ (code-char 0) s))
			    fstr
			    1024
			    :encoding :ucs-2le))
  
  (when custom-filter
    (destructuring-bind (text filt) custom-filter
      (lisp-string-to-foreign (with-output-to-string (s)
				(princ text s)
				(princ (code-char 0) s)
				(princ filt s)
				(princ (code-char 0) s))
			      custom-filter-buffer
			      1024
			      :encoding :ucs-2le)))

  
  (if file
      (lisp-string-to-foreign file file-name 1024
			      :encoding :ucs-2le)
      (memset file-name 1024))
  (memset ofn (foreign-type-size '(:struct openfilename)))
  (memset custom-filter-buffer 1024)
      
  (setf (foreign-slot-value ofn '(:struct openfilename) 'size)
	(foreign-type-size '(:struct openfilename))
	
	(foreign-slot-value ofn '(:struct openfilename) 'owner)
	(or hwnd (null-pointer))
	
	(foreign-slot-value ofn '(:struct openfilename) 'instance)
	(or instance (null-pointer))
	
	(foreign-slot-value ofn '(:struct openfilename) 'filter)
	(if filters fstr (null-pointer))
	
	(foreign-slot-value ofn '(:struct openfilename) 'custom-filter)
	(if custom-filter custom-filter-buffer (null-pointer))
	
	(foreign-slot-value ofn '(:struct openfilename) 'max-custom-filter)
	512
	
	(foreign-slot-value ofn '(:struct openfilename) 'filter-index)
	(or filter-index 0)
	
	(foreign-slot-value ofn '(:struct openfilename) 'file)
	file-name
	
	(foreign-slot-value ofn '(:struct openfilename) 'max-file)
	512
	
	(foreign-slot-value ofn '(:struct openfilename) 'initial-dir)
	(if initial-dir initial-dir (null-pointer))
	
	(foreign-slot-value ofn '(:struct openfilename) 'title)
	(if title title (null-pointer))
	
	(foreign-slot-value ofn '(:struct openfilename) 'flags)
	(mergeflags flags
		    (:allow-multiselect #x200)
		    (:create-prompt #x2000)
		    (:dont-add-to-recent #x02000000)
		    (:enable-resizing #x00800000)
		    (:explorer #x00080000)
		    (:extension-different #x400)
		    (:file-must-exist #x1000)
		    (:force-show-hidden #x10000000)
		    (:hide-readonly #x4)
		    (:long-names #x00200000)
		    (:no-change-dir #x8)
		    (:no-dereference-links #x00100000)
		    (:no-long-names #x00040000)
		    (:no-network-button #x00020000)
		    (:no-readonly-return #x8000)
		    (:no-test-file-create #x00010000)
		    (:no-validate #x100)
		    (:overwrite-prompt #x2)
		    (:path-must-exist #x800)
		    (:readonly #x1)
		    (:share-aware #x4000)
		    (:show-help #x10))))

(defun get-open-file-name (&key hwnd instance filters custom-filter file 
			     filter-index initial-dir title flags)
  (with-foreign-objects ((ofn '(:struct openfilename))
			 (custom-filter-buffer :uint16 512)
			 (file-name :uint16 512)
			 (fstr :uint16 512))
    (with-wide-string (initial-dir-str (or initial-dir ""))
      (with-wide-string (title-str (or title ""))
	
	(setup-openfilename ofn custom-filter-buffer file-name fstr
			    :file file
			    :hwnd hwnd :instance instance
			    :filters filters :custom-filter custom-filter
			    :filter-index filter-index
			    :initial-dir (if initial-dir initial-dir-str (null-pointer))
			    :title (if title title-str (null-pointer))
			    :flags flags)

	(let ((result (%get-open-file-name ofn)))
	  
	  (values result
		  (when result
		    (if (member :allow-multiselect flags)
			(extract-null-strings file-name 512)
			(list (foreign-string-to-lisp file-name
						      :max-chars 512
						      :encoding :ucs-2le))))
		  (when (and result
			     (not (zerop (mem-aref custom-filter-buffer
						   :uint16))))
		    (foreign-string-to-lisp custom-filter-buffer
					    :max-chars 512
					    :encoding :ucs-2le))))))))
		
(defun get-save-file-name (&key hwnd instance filters custom-filter file 
			     filter-index initial-dir title flags)
  (with-foreign-objects ((ofn '(:struct openfilename))
			 (custom-filter-buffer :uint16 512)
			 (file-name :uint16 512)
			 (fstr :uint16 512))
    (with-wide-string (initial-dir-str (or initial-dir ""))
      (with-wide-string (title-str (or title ""))
	
	(setup-openfilename ofn custom-filter-buffer file-name fstr
			    :file file 
			    :hwnd hwnd :instance instance
			    :filters filters :custom-filter custom-filter
			    :filter-index filter-index
			    :initial-dir (if initial-dir initial-dir-str (null-pointer))
			    :title (if title title-str (null-pointer))
			    :flags flags)

	(let ((result (%get-save-file-name ofn)))
	  
	  (values result
		  (when result
		    (if (member :allow-multiselect flags)
			(extract-null-strings file-name 512)
			(list (foreign-string-to-lisp file-name
						      :max-chars 512
						      :encoding :ucs-2le))))
		  (when (and result
			     (not (zerop (mem-aref custom-filter-buffer
						   :uint16))))
		    (foreign-string-to-lisp custom-filter-buffer
					    :max-chars 512
					    :encoding :ucs-2le))))))))
	      
	    
			      
(defcfun (%set-bk-color "SetBkColor" :convention :stdcall) :uint32
  (hdc :pointer)
  (color :uint32))

(defun set-background-color (hdc rgb)
  (%set-bk-color hdc rgb))

(defun set-bk-color (hdc rgb)
  (%set-bk-color hdc rgb))

(defcfun (%ext-text-out "ExtTextOutW" :convention :stdcall) :boolean
  (hdc :pointer)
  (x :int32)
  (y :int32)
  (options :uint32)
  (rect :pointer)
  (string :pointer)
  (count :uint32)
  (dc :pointer))

(defun ext-text-out (hdc string x y &key options rect)
  (with-foreign-object (r '(:struct rect))
    (with-wide-string (str string)
      (if rect
	  (rect-foreign rect r)
	  (memset r (foreign-type-size '(:struct rect))))
      (%ext-text-out hdc
		     x
		     y
		     (mergeflags options
				 (:clipped 4)
				 (:glyph-index #x10)
				 (:ignore-language #x1000)
				 (:numerics-latin #x800)
				 (:numerics-local #x400)
				 (:opaque #x2)
				 (:pdy #x2000)
				 (:rtl-reading #x80))
		     r
		     str
		     (length string)
		     (null-pointer)))))

(defcfun (%is-dlg-button-checked "IsDlgButtonChecked" :convention :stdcall) :uint32
  (hwnd :pointer)
  (id :int32))

(defun is-dialog-button-checked (hwnd id)
  (let ((res (%is-dlg-button-checked hwnd id)))
    (switch res
      (2 :indeterminate)
      (1 :checked)
      (0 :unchecked))))

(defcfun (%check-dlg-button "CheckDlgButton" :convention :stdcall) :boolean
  (hwnd :pointer)
  (id :int32)
  (check :uint32))

(defun check-dialog-button (hwnd id &optional check)
  (let ((res (%check-dlg-button hwnd
				id
				(ecase (or check :unchecked)
				  (:checked 1)
				  (:unchecked 0)
				  (:indeterminate 2)))))
    (if res
	nil
	(get-last-error))))

(defcfun (%select-object "SelectObject" :convention :stdcall) :pointer
  (hdc :pointer)
  (obj :pointer))

(defun select-object (hdc obj)
  (%select-object hdc obj))

(defcfun (%delete-object "DeleteObject" :convention :stdcall) :boolean
  (obj :pointer))

(defun delete-object (obj)
  (let ((res (%delete-object obj)))
    (if res
	nil
	(get-last-error))))

(defcfun (%set-timer "SetTimer" :convention :stdcall) :pointer
  (hwnd :pointer)
  (event :pointer)
  (elapse :uint32)
  (proc :pointer))

(defun set-timer (&key hwnd elapse replace-timer)
  (%set-timer (or hwnd (null-pointer))
	      (cond
		((null replace-timer) (null-pointer))
		((integerp replace-timer) (make-pointer replace-timer))
		((pointerp replace-timer) replace-timer)
		(t (error "Replace-timer must be an integer or pointer")))
	      (or elapse #xffffffff)
	      (null-pointer)))

(defcfun (%kill-timer "KillTimer" :convention :stdcall) :boolean
  (hwnd :pointer)
  (event-id :pointer))

(defun kill-timer (timer &optional hwnd)
  (let ((res (%kill-timer (or hwnd (null-pointer))
			  (cond
			    ((integerp timer) (make-pointer timer))
			    ((pointerp timer) timer)
			    (t (error "Timer must be integer or pointer"))))))
    (if res
	nil
	(get-last-error))))

(defcfun (%rectangle "Rectangle" :convention :stdcall) :boolean
  (hwd :pointer)
  (left :int32)
  (top :int32)
  (right :int32)
  (bottom :int32))

(defun rectangle (hdc left top right bottom)
  (%rectangle hdc left top right bottom))

(defcfun (%draw-text "DrawTextW" :convention :stdcall) :int32
  (hdc :pointer)
  (text :pointer)
  (count :int32)
  (rect :pointer)
  (format :uint32))

(defun draw-text (hdc string rect &optional format)
  (with-foreign-object (r '(:struct rect))
    (with-wide-string (s string)
      (rect-foreign rect r)
      (%draw-text hdc
		  s
		  (length string)
		  r
		  (mergeflags format
			      (:bottom 8)
			      (:calc-rect 1024)
			      (:center 1)
			      (:edit-control 8192)
			      (:end-ellipsis 32768)
			      (:expand-tabs 64)
			      (:internal 4096)
			      (:left 0)
			      (:modify-string 65536)
			      (:no-clip 256)
			      (:no-prefix 2048)
			      (:path-ellipsis 16384)
			      (:right 2)
			      (:rtl-reading 131072)
			      (:tab-stop 128)
			      (:top 0)
			      (:vcenter 4)
			      (:word-break 16)
			      (:word-ellipsis #x40000))))))


(defcfun (%set-pixel "SetPixel" :convention :stdcall) :uint32
  (hdc :pointer)
  (x :int32)
  (y :int32)
  (color :uint32))

(defun set-pixel (hdc x y rgb)
  (%set-pixel hdc x y rgb))


(defcfun (%move-to-ex "MoveToEx" :convention :stdcall) :boolean
  (hdc :pointer)
  (x :int32)
  (y :int32)
  (point :pointer))

(defun move-to (hdc x y)
  (%move-to-ex hdc x y (null-pointer)))

(defcfun (%line-to "LineTo" :convention :stdcall) :boolean
  (hdc :pointer)
  (x :int32)
  (y :int32))

(defun line-to (hdc x y)
  (%line-to hdc x y))

(defcfun (%poly-bezier "PolyBezier" :convention :stdcall) :boolean
  (hdc :pointer)
  (points :pointer)
  (count :uint32))

(defun poly-bezier (hdc points)
  (with-foreign-object (plist '(:struct point) (length points))
    (do ((i 0 (1+ i))
	 (ps points (cdr ps)))
	((null ps))
      (let ((p (mem-aptr plist '(:struct point) i)))
	(setf (foreign-slot-value p '(:struct point) 'x)
	      (first (car ps))
	      (foreign-slot-value p '(:struct point) 'y)
	      (second (car ps)))))
    (let ((res (%poly-bezier hdc plist (length points))))
      (if res
	  nil
	  (get-last-error)))))

(defcfun (%create-hatch-brush "CreateHatchBrush" :convention :stdcall) :pointer
  (style :int32)
  (color :uint32))

(defun create-hatch-brush (color &optional style)
  (%create-hatch-brush (if style
                           (mergeflags style 
                                       (:horizontal 0)
                                       (:vertical 1)
                                       (:forward-diagonal 2)
                                       (:backward-diagonal 3)
                                       (:cross 4)
                                       (:diagonal-cross 5))
                           0)
                       color))

(defcfun (%create-pattern-brush "CreatePatternBrush" :convention :stdcall) :pointer
  (bitmap :pointer))

(defun create-pattern-brush (bitmap)
  (%create-pattern-brush bitmap))

(defcfun (%create-bitmap "CreateBitmap" :convention :stdcall) :pointer
  (width :int32)
  (height :int32)
  (planes :uint32)
  (bits-per-pixel :uint32)
  (bits :pointer))

(defun create-bitmap (width height planes bits-per-pixel data)
  (with-foreign-object (bp :uint8 (length data))
    (dotimes (i (length data))
      (setf (mem-aref bp :uint8 i) (aref data i)))
    (%create-bitmap width height planes bits-per-pixel bp)))

(defcfun (%get-window-text-length "GetWindowTextLengthW" :convention :stdcall)
    :int32 
  (hwnd :pointer))

(defun get-window-text-length (hwnd)
  (%get-window-text-length hwnd))

(defcfun (%get-window-text "GetWindowTextW" :convention :stdcall)
    :int32
  (hwnd :pointer)
  (buffer :pointer)
  (len :int32))

(defun get-window-text (hwnd)
  (with-foreign-object (buffer :uint8 1024)
    (let ((len (%get-window-text hwnd buffer 1024)))
      (if (and (>= len 0) (<= len 512))
	  (foreign-string-to-lisp buffer
				  :count (* len 2)
				  :encoding :ucs-2le)
	  (error "Buffer too small?")))))


(defcstruct toolinfo
  (size :uint32)
  (flags :uint32)
  (hwnd :pointer)
  (id lparam)
  (rect (:struct rect))
  (instance :pointer)
  (text :pointer)
  (lparam lparam))
(defmacro with-tool-info ((var hwnd text rect &key flags id instance lparam) &body body)
  (let ((gtext (gensym))
	(gp (gensym)))
    `(with-foreign-object (,var '(:struct toolinfo))
       (with-wide-string (,gtext ,text)
	 (memset ,var (foreign-type-size '(:struct toolinfo)))
	 (setf (foreign-slot-value ,var '(:struct toolinfo) 'size)
	       (foreign-type-size '(:struct toolinfo))
	       
	       (foreign-slot-value ,var '(:struct toolinfo) 'flags)
	       ,(if flags flags 0)
	       
	       (foreign-slot-value ,var '(:struct toolinfo) 'hwnd)
	       ,hwnd
	       
	       (foreign-slot-value ,var '(:struct toolinfo) 'id)
	       ,(if id id 0)
	       
	       (foreign-slot-value ,var '(:struct toolinfo) 'instance)
	       ,(if instance instance `(null-pointer))
	       
	       (foreign-slot-value ,var '(:struct toolinfo) 'text)
	       ,gtext
	       
	       (foreign-slot-value ,var '(:struct toolinfo) 'lparam)
	       ,(if lparam lparam 0))
	 (let ((,gp (foreign-slot-pointer ,var '(:struct toolinfo) 'rect)))
	   (rect-foreign ,rect ,gp))
	 
	 ,@body))))

(defun make-long (low high)
  (logior (logand low #xffff)
	  (ash (logand high #xffff) 16)))
(defun make-lparam (low high)
  (make-long low high))
(defun loword (lparam)
  (logand lparam #xffff))
(defun hiword (lparam)
  (ash (logand lparam #xffff0000) -16))

;; (defun hd (pointer count)
;;   (dotimes (i count)
;;     (format t "~2,'0X " (mem-aref pointer :uint8 i)))
;;   (terpri))

(defcstruct nmhdr
  (hwnd :pointer)
  (id lparam)
  (code :uint32))
(defun foreign-nmhdr (p)
  (list :hwnd (foreign-slot-value p '(:struct nmhdr) 'hwnd)
	:id (foreign-slot-value p '(:struct nmhdr) 'id)
	:code (foreign-slot-value p '(:struct nmhdr) 'code)))
(defun nmhdr-foreign (nmhdr p)
  (setf (foreign-slot-value p '(:struct nmhdr) 'hwnd)
	(getf nmhdr :hwnd)
	(foreign-slot-value p '(:struct nmhdr) 'id)
	(getf nmhdr :id)
	(foreign-slot-value p '(:struct nmhdr) 'code)
	(getf nmhdr :code)))

(defcstruct updown
  (nmhdr (:struct nmhdr))
  (pos :int32)
  (delta :int32))

(defun foreign-updown (p)
  (list :nmhdr (foreign-nmhdr p)
	:pos (foreign-slot-value p '(:struct updown) 'pos)
	:delta (foreign-slot-value p '(:struct updown) 'delta)))

(defcstruct systemtime
  (year :uint16)
  (month :uint16)
  (day-of-week :uint16)
  (day :uint16)
  (hour :uint16)
  (minute :uint16)
  (second :uint16)
  (milli :uint16))
(defstruct systemtime
  year month day-of-week day hour minute second milli)

(defun foreign-systemtime (p)
  (make-systemtime
   :year (foreign-slot-value p '(:struct systemtime) 'year)
   :month (foreign-slot-value p '(:struct systemtime) 'month)
   :day-of-week (foreign-slot-value p '(:struct systemtime) 'day-of-week)
   :day (foreign-slot-value p '(:struct systemtime) 'day)
   :hour (foreign-slot-value p '(:struct systemtime) 'hour)
   :minute (foreign-slot-value p '(:struct systemtime) 'minute)
   :second (foreign-slot-value p '(:struct systemtime) 'second)
   :milli (foreign-slot-value p '(:struct systemtime) 'milli)))
(defun systemtime-foreign (st p)
  (setf (foreign-slot-value p '(:struct systemtime) 'year)
	(systemtime-year st)
	(foreign-slot-value p '(:struct systemtime) 'month)
	(systemtime-month st)
	(foreign-slot-value p '(:struct systemtime) 'day-of-week)
	(systemtime-day-of-week st)
	(foreign-slot-value p '(:struct systemtime) 'day)
	(systemtime-day st)
	(foreign-slot-value p '(:struct systemtime) 'hour)
	(systemtime-hour st)
	(foreign-slot-value p '(:struct systemtime) 'minute)
	(systemtime-minute st)
	(foreign-slot-value p '(:struct systemtime) 'second)
	(systemtime-second st)
	(foreign-slot-value p '(:struct systemtime) 'milli)
	(systemtime-milli st)))	

(defcstruct tcitem
  (mask :uint32)
  (state :uint32)
  (state-mask :uint32)
  (text :pointer)
  (max :int32)
  (image :int32)
  (lparam lparam))
(defstruct tcitem
  mask state state-mask
  text image lparam)

(defun foreign-tcitem (p)
  (make-tcitem :mask (foreign-slot-value p '(:struct tcitem) 'mask)
	       :state (foreign-slot-value p '(:struct tcitem) 'state)
	       :state-mask (foreign-slot-value p '(:struct tcitem) 'state-mask)
	       :text (let ((tp (foreign-slot-value p '(:struct tcitem) 'text)))
		       (unless (null-pointer-p tp)
			 (foreign-string-to-lisp
			  tp :encoding :ucs-2le)))
	       :image (foreign-slot-value p '(:struct tcitem) 'image)
	       :lparam (foreign-slot-value p '(:struct tcitem) 'lparam)))
(defun tcitem-foreign (p &key mask state state-mask text image lparam)
  (memset p (foreign-type-size '(:struct tcitem)))
  (when mask
    (setf (foreign-slot-value p '(:struct tcitem) 'mask) mask))
  (when state
    (setf (foreign-slot-value p '(:struct tcitem) 'state) state))
  (when state-mask
    (setf (foreign-slot-value p '(:struct tcitem) 'state-mask) state-mask))
  (when text 
    (setf (foreign-slot-value p '(:struct tcitem) 'text) text))
  (when image
    (setf (foreign-slot-value p '(:struct tcitem) 'image) image))
  (when lparam
    (setf (foreign-slot-value p '(:struct tcitem) 'lparam) lparam))
  p)
   
(defcfun (%create-font "CreateFontW" :convention :stdcall)
    :pointer
  (height :int32)
  (width :int32)
  (escapement :int32)
  (orientation :int32)
  (weight :int32)
  (italic :uint32)
  (underline :uint32)
  (strikeout :uint32)
  (charset :uint32)
  (output-precision :uint32)
  (clip-precision :uint32)
  (quality :uint32)
  (pitch-and-family :uint32)
  (face :pointer))

(defun create-font (name &key height width escapement orientation
			   weight italic underline strikeout
			   charset output-precision clip-precision
			   quality pitch-and-family)
  (with-wide-string (s name)
    (%create-font (or height 10)
		  (or width 0)
		  (or escapement 0)
		  (or orientation 0)
		  (or weight 0)
		  (if italic 1 0)
		  (if underline 1 0)
		  (if strikeout 1 0)
		  (or charset 0)
		  (or output-precision 0)
		  (or clip-precision 0)
		  (or quality 0)
		  (or pitch-and-family 0)
		  s)))

;; (defcfun (%enum-font-families "EnumFontFamiliesEx" :convention :stdcall)
;;     :int32
;;   (hdc :pointer)
;;   (logfont :pointer)
;;   (proc :pointer)
;;   (lparam lparam)
;;   (flags :uint32))

;; (defun enum-font-families (hdc)
;;   (with-foreign-object (lf '(:struct logfont))
    
  
(defcfun (%set-bk-mode "SetBkMode" :convention :stdcall) :int32
  (hdc :pointer)
  (mode :int32))

(defun set-bk-mode (hdc &optional mode)
  (%set-bk-mode hdc
		(ecase (or mode :opaque)
		  (:transparent 1)
		  (:opaque 2))))

(defcfun (%ext-create-pen "ExtCreatePen" :convention :stdcall) :pointer
  (style :uint32)
  (width :uint32)
  (brush :pointer)
  (style-count :uint32)
  (styles :pointer))

(defcstruct logbrush
  (style :uint32)
  (color :uint32)
  (hatch :pointer))
(defun foreign-logbrush (p)
  (list :style (foreign-slot-value p '(:struct logbrush) 'style)
        :color (foreign-slot-value p '(:struct logbrush) 'color)
        :hatch (foreign-slot-value p '(:struct logbrush) 'hatch)))
        
(defun ext-create-pen (style &key width styles brush-style brush-color brush-hatch)
  (with-foreign-object (lb '(:struct logbrush))
    (memset lb (foreign-type-size '(:struct logbrush)))

    (when brush-style
      (setf (foreign-slot-value lb '(:struct logbrush) 'style) brush-style))

    (when brush-color
      (setf (foreign-slot-value lb '(:struct logbrush) 'color) brush-color))

    (when brush-hatch
      (setf (foreign-slot-value lb '(:struct logbrush) 'hatch)
           (make-pointer brush-hatch)))

    (with-foreign-object (stlist :uint32 16)
      (do ((i 0 (1+ i))
           (st styles (cdr st)))
          ((or (null st) (= i 16)))
        (setf (mem-aref stlist :uint32 i) (nth i styles)))

      (let ((res (%ext-create-pen style 
                                  (or width 1)
                                  lb
                                  (if styles (min (length styles) 16) 0)
                                  (if styles stlist (null-pointer)))))
        (if (null-pointer-p res)
            (get-last-error)
            res)))))

                        
(defcfun (%polygon "Polygon" :convention :stdcall) :boolean
  (hdc :pointer)
  (points :pointer)
  (count :int32))

(defun polygon (hdc points)
  (with-foreign-object (plist '(:struct point) (length points))
    (do ((i 0 (1+ i))
         (p points (cdr p)))
        ((null p))
      (point-foreign (car p) (mem-aptr plist '(:struct point) i)))
    (let ((res (%polygon hdc plist (length points))))
      (unless res (get-last-error)))))

(defcfun (%ellipse "Ellipse" :convention :stdcall)
    :boolean
  (hdc :pointer)
  (left :int32)
  (top :int32)
  (right :int32)
  (bottom :int32))

(defun ellipse (hdc left top right bottom)
  (let ((res (%ellipse hdc left top right bottom)))
    (unless res (get-last-error))))



(defcfun (%round-rect "RoundRect" :convention :stdcall)
    :boolean
  (hdc :pointer)
  (left :int32)
  (top :int32)
  (right :int32)
  (bottom :int32)
  (width :int32)
  (height :int32))

(defun round-rect (hdc left top right bottom width height)
  (let ((res (%round-rect hdc left top right bottom width height)))
    (unless res (get-last-error))))

(defcfun (%chord "Chord" :convention :stdcall) :boolean
  (hdc :pointer)
  (left :int32)
  (top :int32)
  (right :int32)
  (bottom :int32)
  (x1 :int32)
  (y1 :int32)
  (x2 :int32)
  (y2 :int32))

(defun chord (hdc left top right bottom x1 y1 x2 y2)
  (let ((res (%chord hdc left top right bottom x1 y1 x2 y2)))
    (unless res (get-last-error))))


(defcfun (%polyline "Polyline" :convention :stdcall)
    :boolean
  (hdc :pointer)
  (points :pointer)
  (count :int32))

(defun polyline (hdc points)
  (with-foreign-object (plist '(:struct point) (length points))
    (do ((i 0 (1+ i))
         (p points (cdr p)))
        ((null p))
      (point-foreign (car p) (mem-aptr plist '(:struct point) i)))
    (let ((res (%polyline hdc plist (length points))))
      (unless res (get-last-error)))))

(defcfun (%bit-blt "BitBlt" :convention :stdcall)
    :boolean
  (hdc-dest :pointer)
  (x-dest :int32)
  (y-dest :int32)
  (width :int32)
  (height :int32)
  (hdc-source :pointer)
  (x-source :int32)
  (y-source :int32)
  (rop :uint32))

(defun bit-blt (hdc-dest x-dest y-dest hdc-source x-source y-source
                &key width height raster-op)
  (let ((res (%bit-blt hdc-dest x-dest y-dest
                       (or width 0) (or height 0)
                       hdc-source x-source y-source
                       (cond
                         ((null raster-op) #x42)
                         ((integerp raster-op)
                          raster-op)
                         ((keywordp raster-op)
                          (ecase raster-op
                            (:blackness #x42)
                            (:captureblt #x40000000)
                            (:dstinvert #x00550009)
                            (:mergecopy #x00c000ca)
                            (:mergepaint #x00bb0226)
                            (:no-mirror-bitmap #x80000000)
                            (:not-src-copy #x00330008)
                            (:not-src-erase #x001100a6)
                            (:patcopy #x00f00021)
                            (:patinvert #x005a0049)
                            (:patpaint #x00fb0a09)
                            (:srcand #x008800c6)
                            (:srccopy #x00cc0020)
                            (:srcerase #x00440328)
                            (:srcinvert #x00660046)
                            (:srcpaint #x00ee0086)
                            (:whiteness #x00ff0062)))
                         (t (error "RASTER-OP must be keyword or integer."))))))
    (unless res (get-last-error))))

(defcfun (%create-compatible-dc "CreateCompatibleDC" :convention :stdcall)
    :pointer
  (hdc :pointer))

(defun create-compatible-dc (hdc)
  (let ((ret (%create-compatible-dc hdc)))
    (if (null-pointer-p ret)
        (get-last-error)
        ret)))
                 
(defcfun (%get-object "GetObjectW" :convention :stdcall)
    :int32
  (obj :pointer)
  (count :int32)
  (buffer :pointer))


(defcstruct bitmap
  (type :int32)
  (width :int32)
  (height :int32)
  (width-bytes :int32)
  (planes :int32)
  (bits-per-pixel :int32)
  (bits :pointer))
(defstruct bitmap
  type width height width-bytes planes bits-per-pixel bits)
(defun foreign-bitmap (p)
  (make-bitmap :type (foreign-slot-value p '(:struct bitmap) 'type)
               :width (foreign-slot-value p '(:struct bitmap) 'width)
               :height (foreign-slot-value p '(:struct bitmap) 'height)
               :width-bytes (foreign-slot-value p '(:struct bitmap) 'width-bytes)
               :planes (foreign-slot-value p '(:struct bitmap) 'planes)
               :bits-per-pixel (foreign-slot-value p '(:struct bitmap) 'bits-per-pixel)
               :bits (foreign-slot-value p '(:struct bitmap) 'bits)))
               
(defcstruct bitmap-info-header
  (size :uint32)
  (width :int32)
  (height :int32)
  (planes :uint16)
  (bit-count :uint16)
  (compression :uint32)
  (size-image :uint32)
  (x-pixels-per-meter :int32)
  (y-pixel-per-meter :int32)
  (color-used :uint32)
  (color-important :uint32))

(defcstruct dibsection
  (bitmap (:struct bitmap))
  (header (:struct bitmap-info-header))
  (bit-fields :uint32 :count 3)
  (section :pointer)
  (offset :uint32))

(defcstruct extlogpen
  (pen-style :uint32)
  (width :uint32)
  (brush-style :uint32)
  (color :uint32)
  (hatch :pointer)
  (num-entries :uint32)
  (style-entry :uint32)) ;; actually this is the start of a tailing array 
(defstruct extlogpen
  pen-style width brush-style color hatch entries)
(defun foreign-extlogpen (p)
  (let ((num-entries (foreign-slot-value p '(:struct extlogpen) 'num-entries)))
    (make-extlogpen :pen-style (foreign-slot-value p '(:struct extlogpen) 'pen-style)
                    :width (foreign-slot-value p '(:struct extlogpen) 'width)
                    :brush-style (foreign-slot-value p '(:struct extlogpen) 'brush-style)
                    :color (foreign-slot-value p '(:struct extlogpen) 'color)
                    :hatch (foreign-slot-value p '(:struct extlogpen) 'hatch)
                    :entries (do ((i 0 (1+ i))
                                  (ptr (foreign-slot-pointer p '(:struct extlogpen) 'style-entry))
                                  (elist nil))
                                 ((= i num-entries) elist)
                               (push (mem-aref ptr :uint32 i) elist)))))
                  
(defcstruct logpen
  (style :uint32)
  (width (:struct point))
  (color :uint32))
(defun foreign-logpen (p)
  (list :style (foreign-slot-value p '(:struct logpen) 'style)
        :width (foreign-point (foreign-slot-pointer p '(:struct logpen) 'width))
        :color (foreign-slot-value p '(:struct logpen) 'color)))
        
(defcstruct logfont
  (height :int32)
  (width :int32)
  (escapement :int32)
  (orientation :int32)
  (weight :int32)
  (italic :uint8)
  (underline :uint8)
  (strikeout :uint8)
  (charset :uint8)
  (out-precision :uint8)
  (clip-precision :uint8)
  (quality :uint8)
  (pitch-and-family :uint8)
  (name :uint16 :count 32))
(defstruct logfont
  height width escapement orientation weight italic underline strikeout
  charset out-precision clip-precision quality pitch-and-family name)
(defun foreign-logfont (p)
  (make-logfont
   :height (foreign-slot-value p '(:struct logfont) 'height)
   :width (foreign-slot-value p '(:struct logfont) 'width)
   :escapement (foreign-slot-value p '(:struct logfont) 'escapement)
   :orientation (foreign-slot-value p '(:struct logfont) 'orientation)
   :weight (foreign-slot-value p '(:struct logfont) 'weight)
   :italic (foreign-slot-value p '(:struct logfont) 'italic)
   :underline (foreign-slot-value p '(:struct logfont) 'underline)
   :strikeout (foreign-slot-value p '(:struct logfont) 'strikeout)
   :charset (foreign-slot-value p '(:struct logfont) 'charset)
   :out-precision (foreign-slot-value p '(:struct logfont) 'out-precision)
   :clip-precision (foreign-slot-value p '(:struct logfont) 'clip-precision)
   :quality (foreign-slot-value p '(:struct logfont) 'quality)
   :pitch-and-family (foreign-slot-value p '(:struct logfont) 'pitch-and-family)
   :name (foreign-string-to-lisp
          (foreign-slot-pointer p '(:struct logfont) 'name)
          :encoding :ucs-2le)))
   
(defun get-object (obj type)
  (let ((count (%get-object obj 0 (null-pointer))))
    (when (zerop count) (get-last-error))
    (with-foreign-object (buffer :uint8 count)
      (let ((res (%get-object obj count buffer)))
        (when (zerop res) (get-last-error))
        (ecase type
          (:bitmap (foreign-bitmap buffer))           
          (:dibsection (foreign-bitmap buffer))
          (:palette (mem-aref buffer :uint32))
          (:ext-pen (foreign-extlogpen buffer))
          (:pen (foreign-logpen buffer))
          (:brush (foreign-logbrush buffer))
          (:font (foreign-logfont buffer)))))))

(defcfun (%delete-dc "DeleteDC" :convention :stdcall)
    :boolean
  (hdc :pointer))

(defun delete-dc (hdc)
  (%delete-dc hdc))

(defcfun (%system-parameters-info "SystemParametersInfoW" :convention :stdcall)
    :boolean
  (action :uint32)
  (count :uint32)
  (buffer :pointer)
  (winini :uint32))

(defcstruct nonclientmetrics
  (size :uint32)
  (border-width :int32)
  (scroll-width :int32)
  (scroll-height :int32)
  (caption-width :int32)
  (caption-height :int32)
  (caption-font (:struct logfont))
  (small-caption-width :int32)
  (small-caption-height :int32)
  (small-caption-font (:struct logfont))
  (menu-width :int32)
  (menu-height :int32)
  (menu-font (:struct logfont))
  (status-font (:struct logfont))
  (message-font (:struct logfont))
  (padded-border-width :int32))
(defstruct nonclientmetrics
  size border-width scroll-width scroll-height
  caption-width caption-height caption-font
  small-caption-width small-caption-height small-caption-font
  menu-width menu-height menu-font status-font message-font padded-border-width)
(defun foreign-nonclientmetrics (p)
  (make-nonclientmetrics
   :size (foreign-slot-value p '(:struct nonclientmetrics) 'size)
   :border-width (foreign-slot-value p '(:struct nonclientmetrics) 'border-width)
   :scroll-width (foreign-slot-value p '(:struct nonclientmetrics) 'scroll-width)
   :scroll-height (foreign-slot-value p '(:struct nonclientmetrics) 'scroll-height)
   :caption-width (foreign-slot-value p '(:struct nonclientmetrics) 'caption-width)
   :caption-height (foreign-slot-value p '(:struct nonclientmetrics) 'caption-height)
   :caption-font (foreign-logfont (foreign-slot-pointer p '(:struct nonclientmetrics) 'caption-font))
   :small-caption-width (foreign-slot-value p '(:struct nonclientmetrics) 'small-caption-width)
   :small-caption-height (foreign-slot-value p '(:struct nonclientmetrics) 'small-caption-height)
   :small-caption-font (foreign-logfont (foreign-slot-pointer p '(:struct nonclientmetrics) 'small-caption-font))
   :menu-width (foreign-slot-value p '(:struct nonclientmetrics) 'menu-width)
   :menu-height (foreign-slot-value p '(:struct nonclientmetrics) 'menu-height)
   :menu-font (foreign-logfont (foreign-slot-pointer p '(:struct nonclientmetrics) 'menu-font))
   :status-font (foreign-logfont (foreign-slot-pointer p '(:struct nonclientmetrics) 'status-font))
   :message-font (foreign-logfont (foreign-slot-pointer p '(:struct nonclientmetrics) 'message-font))
   :padded-border-width (foreign-slot-value p '(:struct nonclientmetrics) 'padded-border-width)))

   
(defun system-parameters-info (action &optional param winini)
  (declare (ignore param))
  (switch action
    (+spi-getnonclientmetrics+
     (with-foreign-object (buffer '(:struct nonclientmetrics))
       (let ((res (%system-parameters-info action
                                           (foreign-type-size '(:struct nonclientmetrics))
                                           buffer
                                           (if winini
                                               (ecase winini 
                                                 (:update-ini-file #x1)
                                                 (:send-win-ini-change #x2))
                                               0))))
         (unless res (get-last-error))
         (foreign-nonclientmetrics buffer))))
    (t (error "Current unsupported action -- please add support for this"))))


(defcfun (%set-focus "SetFocus" :convention :stdcall)
    :pointer
  (hwnd :pointer))

(defun set-focus (hwnd)
  (%set-focus hwnd))

(defcfun (%get-focus "GetFocus" :convention :stdcall)
    :pointer)

(defun get-focus ()
  (%get-focus))
