
;;; Simple minesweeper game
;;; Things which could be done and are left as an exercise to the reader:
;;; 1. Adding icons/images instead of using text for the numbers, mines and flags.
;;; 2. Keeping a high-score list saved away e.g. in current user's home directory.
;;; 3. Writing a proper "about" dialog.
;;; 4. Dialog for entering arbitrary game sizes.
;;; 5. Automatically clicking on provable empty cells
;;; 6. Win detection 
;;; 7. Automatically resizing main window to the size of the game board


(defpackage #:ftw.minesweeper
  (:use #:cl #:cffi #:ftw)
  (:export #:minesweeper))

(in-package #:ftw.minesweeper)


(defstruct minesweeper
  x y
  cells
  flags
  finished
  seconds)

(defun cell (ms x y)
  (aref (minesweeper-cells ms) y x))
(defun (setf cell) (value ms x y)
  (setf (aref (minesweeper-cells ms) y x) value))

(defun set-flag (ms x y &optional set)
  (if set 
      (pushnew :flag (cell ms x y))
      (setf (cell ms x y)
	    (remove :flag (cell ms x y)))))

(defun flag-p (ms x y)
  (member :flag (cell ms x y)))

(defun mine-p (ms x y)
  (member :mine (cell ms x y)))

(defun mines (ms x y)
  (let ((m 0))
    (do ((i (1- x) (1+ i)))
	((> i (1+ x)))
      (do ((j (1- y) (1+ j)))
	  ((> j (1+ y)))
	(when (and (>= i 0) (< i (minesweeper-x ms)))
	  (when (and (>= j 0) (< j (minesweeper-y ms)))
	    (unless (and (= i x) (= j y))
	      (when (mine-p ms i j)
		(incf m)))))))
    m))


(defvar *starting-mines* 10)
(defvar *starting-x* 10)
(defvar *starting-y* 10)

(defun random-game (&key x y mines)
  (unless x (setf x *starting-x*))
  (unless y (setf y *starting-y*))
  (unless mines (setf mines *starting-mines*))
  
  (let ((ms (make-minesweeper
	     :x x :y y
	     :cells (make-array (list x y)
				:element-type t
				:initial-contents
				(let ((m mines))
				  (loop :for i :below x :collect 
				     (loop :for j :below y :collect
					(let ((left (- (* x y) (+ (* i y) j))))
					  (cond
					    ((<= left m)
					     (decf m)
					     (list :mine))
					    ((<= (random left) m)
					     (decf m)
					     (list :mine))
					    (t nil)))))))
	     :flags mines
	     :seconds 0)))
    ms))


(defun clicked-p (ms x y)
  (integerp (car (cell ms x y))))

(defun click-cell (ms x y)
  (cond
    ((mine-p ms x y)
     :mine)
    (t 
     (setf (cell ms x y) (list (mines ms x y)))
     (cell ms x y))))


(defvar *ms* nil)

(defun add-menu-bar (hwnd menus)
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

(defwndproc minesweeper-wndproc (hwnd msg wparam lparam)
  (switch msg
    ((const +wm-create+)
     (setf *ms* (random-game))

     ;; add menus
     (add-menu-bar hwnd
		   `((:menu (:popup) :name "&Game"
		      :children
		      ((:item (:string)
			      :name ,(format nil "&New~ACtrl+N" #\tab)
			      :id 1)
		       (:item (:separator))
		       (:item (:string)
			      :name "Beginner" :id 6)
		       (:item (:string)
			      :name "Advanced" :id 7)
		       (:item (:string)
			      :name "Expert" :id 8)
		       (:radio (6 8) :id 6)
		       (:item (:separator))
		       (:item (:string)
			      :name ,(format nil "&Quit~ACtrl+Q" #\tab)
			      :id 2)))
		     (:menu (:popup) :name "&Help"
		      :children
		      ((:item (:string)
			      :name "&About"
			      :id 3)))))

     (let ((right (getf (get-client-rect hwnd) :right 0)))
       (create-window :button
		      :window-name "New"
		      :styles (logior-consts +ws-visible+ +ws-child+ +bs-pushbutton+)
		      :x (truncate right 2) :y 20 :width 45 :height 22
		      :parent hwnd
		      :menu 5))
     
     ;; create accelerator table
     (set-accelerator-table
      '((:keyn 1 :control :virtual-key)
        (:keyq 2 :control :virtual-key)))

     (set-timer :hwnd hwnd :elapse 1000 :replace-timer 4))
    ((const +wm-paint+)
     (with-paint (hwnd hdc)
       ;; draw top region containing seconds and flag counter
       (destructuring-bind (&key (right 0) (bottom 0) &allow-other-keys) (get-client-rect hwnd)
	 (draw-edge hdc (make-rect :left 10 :top 10 :right (- right 10) :bottom 50)
		    :flags '(:left :right :top :bottom :adjust))
	 (text-out hdc (format nil "~A" (minesweeper-seconds *ms*))
		   25 20)
	 (text-out hdc (format nil "~A" (minesweeper-flags *ms*))
		   (- right 40) 20)

	 (draw-edge hdc (make-rect :left 10 :top 70 :right (- right 10) :bottom (- bottom 5))
		    :flags '(:left :right :top :bottom :adjust))


	 (let ((xstart (- (truncate right 2)
			  (* (truncate (minesweeper-x *ms*) 2) 25)))
	       (ystart 85))
	   
	   (dotimes (i (minesweeper-x *ms*))
	     (dotimes (j (minesweeper-y *ms*))
	       (draw-edge hdc (make-rect :left (+ xstart (* i 25))
					 :top (+ ystart (* j 25))
					 :right (+ xstart 25 (* i 25))
					 :bottom (+ ystart 25 (* j 25)))
			  :inner-edge (if (or (clicked-p *ms* i j) (minesweeper-finished *ms*))
					  :sunk
					  :raised)
			  :outer-edge (if (or (clicked-p *ms* i j) (minesweeper-finished *ms*))
					  :sunk
					  :raised)
			  :flags '(:left :right :top :bottom :adjust))
	       (cond
		 ((minesweeper-finished *ms*)
		  (cond
		    ((mine-p *ms* i j)
		     (text-out hdc "M" (+ xstart 9 (* i 25)) (+ ystart 5 (* j 25))))
		    (t 
		     (let ((m (mines *ms* i j)))
		       (unless (zerop m)
			 (text-out hdc (format nil "~A" m)
				   (+ xstart 9 (* i 25))
				   (+ ystart 5 (* j 25))))))))
		 (t
		  (when (flag-p *ms* i j)
		    (text-out hdc "F" (+ xstart 9 (* i 25)) (+ ystart 5 (* j 25))))
		  (when (clicked-p *ms* i j)
		    (let ((m (mines *ms* i j)))
		      (unless (zerop m)
			(text-out hdc (format nil "~A" m)
				  (+ xstart 9 (* i 25))
				  (+ ystart 5 (* j 25))))))))))))))
    ((const +wm-lbuttondown+)
     (destructuring-bind (&key (right 0) &allow-other-keys) (get-client-rect hwnd)
       (let* ((xstart (- (truncate right 2)
			 (* (truncate (minesweeper-x *ms*) 2) 25)))
	      (ystart 85)
	      (x (loword lparam))
	      (y (hiword lparam))
	      (i (truncate (- x xstart) 25))
	      (j (truncate (- y ystart) 25)))
	 (when (= (loword wparam) 1)
	   (when (and (>= i 0) (< i (minesweeper-x *ms*))
		      (>= j 0) (< j (minesweeper-y *ms*)))
	     (unless (flag-p *ms* i j)
	       (when (eq (click-cell *ms* i j) :mine)
		 (setf (minesweeper-finished *ms*) t))))))
       (invalidate-rect hwnd nil t)))
    ((const +wm-rbuttondown+)
     (destructuring-bind (&key (right 0) &allow-other-keys) (get-client-rect hwnd)
       (let* ((xstart (- (truncate right 2)
			 (* (truncate (minesweeper-x *ms*) 2) 25)))
	      (ystart 85)
	      (x (loword lparam))
	      (y (hiword lparam))
	      (i (truncate (- x xstart) 25))
	      (j (truncate (- y ystart) 25)))
	 (when (= (loword wparam) 2)
	   (when (and (>= i 0) (< i (minesweeper-x *ms*))
		      (>= j 0) (< j (minesweeper-y *ms*))
		      (not (clicked-p *ms* i j))
		      (> (minesweeper-flags *ms*) 0))
	     (cond
	       ((flag-p *ms* i j)
		(set-flag *ms* i j nil)
		(incf (minesweeper-flags *ms*)))
	       (t
		(set-flag *ms* i j t)
		(decf (minesweeper-flags *ms*)))))))
       (invalidate-rect hwnd nil t)))
    ((const +wm-command+)
     (switch (loword wparam)
       (1 ;; new
	(setf *ms* (random-game))
	(invalidate-rect hwnd nil t))
       (2 ;; quit
	(destroy-window hwnd))
       (3 ;; about
	(message-box :hwnd hwnd
		     :text "Simple minesweeper game written in Common Lisp.

Copyright (c) Frank James 2016.
"
		     :caption "About"))
       (5 ;; new
	(setf *ms* (random-game))
	(invalidate-rect hwnd nil t))
       (6 ;; beginner
	(setf *starting-mines* 10
	      *starting-x* 10
	      *starting-y* 10)
	(check-menu-radio-item (get-menu hwnd) 6 8 6))
       (7 ;; advanced
	(setf *starting-mines* 40
	      *starting-x* 14
	      *starting-y* 14)
	(check-menu-radio-item (get-menu hwnd) 6 8 7))
       (8 ;; expert
	(setf *starting-mines* 99
	      *starting-x* 24
	      *starting-y* 24)
	(check-menu-radio-item (get-menu hwnd) 6 8 8))))
    ((const +wm-timer+)
     (unless (minesweeper-finished *ms*)
       (incf (minesweeper-seconds *ms*)))
     (invalidate-rect hwnd (make-rect :right 50
				      :bottom 50)
		      t))
    ((const +wm-size+)
     (let ((btn (find-window "button" "New" hwnd)))
       (when btn 
	 (set-window-pos btn :top
			 (- (truncate (getf (get-client-rect hwnd) :right 0) 2)
			    22)
			 20
			 45 22)))
     (invalidate-rect hwnd nil t))
    ((const +wm-destroy+)
     (set-accelerator-table)
     (post-quit-message)))
  (default-window-proc hwnd msg wparam lparam))


(defun minesweeper ()
  (default-message-loop (callback minesweeper-wndproc)
      :class-name "FTW_MINESWEEPER"
      :title "Minesweeper"
      :width 350 :height 425))


  
