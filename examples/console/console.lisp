
(defpackage #:ftw.console
  (:use #:cl #:ftw))

(in-package #:ftw.console)

(defconstant +width+ 100)
(defconstant +height+ 30)

(defvar *init* nil)
(defun init-console ()
  (unless *init*
    (show-window (get-console-window) :show)
    (show-window (get-console-window) :show)
    (show-window (get-console-window) :maximize)
    (set-std-handle (open-console-std-input) :input)
    (set-std-handle (create-console-screen-buffer) :output)
    (set-console-active-screen-buffer (get-std-handle :output))
    (setf *init* t)))

(defun justify-text (text width &optional (justify :left))
  (let ((lines nil))
    (do ((i 0 (1+ i))
	 (col 0 (1+ col))
	 (line-start 0)
	 (line-break 0))
	((= i (length text)))
      (when (or (= i (1- (length text)))
		(char= (char text i) #\space)
		(char= (char text i) #\return)
		(char= (char text i) #\newline))
	(setf line-break i))

      (when (= col width)
	(push (ecase justify
		(:left
		 (concatenate 'string
			      (subseq text line-start line-break)
			      (loop :for j :below (- width (- line-break line-start)) :collect #\space)))
		(:right
		 (concatenate 'string
			      (loop :for j :below (- width (- line-break line-start)) :collect #\space)
			      (subseq text line-start line-break)))
		((:center :centre)
		 (concatenate 'string
			      (loop :for j :below (truncate (- width (- line-break line-start)) 2) :collect #\space)
			      (subseq text line-start line-break)
			      (loop :for j :below (truncate (- width (- line-break line-start)) 2) :collect #\space))))
	      lines)
	(setf i line-break
	      col 0
	      line-start line-break
	      line-break i)))
    (nreverse lines)))

      
		     
(defparameter *lorem-ipsum*
  "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")

(defun console ()
  (init-console)
  (set-console-title "Lisp Console Program")
  (show-window (get-console-window) :show)
  
  (let ((in (get-std-handle :in))
	(out (get-std-handle :out)))
    (set-console-mode in (logior ftw::+enable-processed-input+
				 ftw::+enable-insert-mode+))

    (set-console-cursor-position in 0 0)
    (set-console-cursor-info out :visible nil)
    
    (set-console-screen-buffer-info out
				    :size (list +width+ +height+)
				    :max-size (list +width+ +height+)
				    :fullscreen-p t)
    (show-window (get-console-window) :maximize)
    
    (fill-console-output-character out #\space 0 0 (* +width+ +height+))
    (fill-console-output-attribute out
				   (char-info-attributes :bg-b t :bg-r t :bg-g t)
				   0 0 (* (1- +height+) +width+))

    (fill-console-output-attribute out
				   (char-info-attributes :bg-b t)
				   0 (1- +height+) +width+)

    (write-console-output out
			  (list (string-info "Package"
					     (char-info-attributes
					      :fg-r t :bg-r t :bg-g t :bg-b t)))
			  :x 20 :y 1)
    (write-console-output out
			  (mapcar (lambda (l)
				    (string-info l
						 (char-info-attributes
						  :fg-r t :bg-r t :bg-g t :bg-b t)))
				  (justify-text *lorem-ipsum* (truncate +width+ 3)))
			  :x 2 :y 2)
						 
    
    (do ((row 2 (1+ row)))
	((= row (- +height+ 2)))
      (fill-console-output-attribute out
				     (char-info-attributes :bg-b t :bg-g t)
				     2 row
				     (- (truncate +width+ 2) 5)))

    (write-console-output out
			  (list (string-info "Symbol"
					     (char-info-attributes :fg-r t :bg-r t :bg-g t :bg-b t)))
			  :x 70 :y 1)
    
    (do ((row 2 (1+ row)))
	((= row (- +height+ 2)))
      (fill-console-output-attribute out
				     (char-info-attributes :bg-b t
							   :bg-g t)
				     53 row (- (truncate +width+ 2) 5)))
    


    
    (mapc (lambda (name x)
	    (write-console-output out
				  (list (string-info name
						     (char-info-attributes 
						     :fg-b t :fg-g t :fg-r t
						     :fg-intensity t
						     :bg-b t)))
				  :x x :y (1- +height+)))
	  '("Search F1" "Exit F2")
	  '(5 25))

    (do ((done nil))
	(done)
      (let ((events (read-console-input in)))
	(dolist (event events)
	  (case (car event)
	    (ftw::key
	     (let ((vkey (getf (cdr event) 'ftw::keycode))
		   (keydown (getf (cdr event) 'ftw::keydown)))
	       (unless keydown
		 (switch vkey
		   (ftw::+vk-f1+ nil)
		   (ftw::+vk-f2+ (setf done t)))))))))))

  (show-window (get-console-window) :hide))


	     
  
