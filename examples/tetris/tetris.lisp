

;;; Little tetris game.

(defpackage #:ftw.tetris
  (:use #:cl #:ftw)
  (:export #:tetris))

(in-package #:ftw.tetris)

;; ------------------- Game logic -----------------

(defstruct shape
  x y
  points
  color)

(defun shape-cells (s)
  (mapcar (lambda (p)
	    (destructuring-bind (x y) p
	      (list (+ x (shape-x s))
		    (+ y (shape-y s)))))
	  (shape-points s)))

(defvar *random-color* nil)
(defparameter *width* 12)
(defparameter *height* 28)
(defparameter *tetris* nil)

(defun random-shape ()
  (let ((s (random 6)))
    (make-shape
     :x (truncate *width* 2) :y (- *height* 2)
     :points
     (switch s
       (0 ;; box
	(list (list 0 0) (list 0 1) (list 1 0) (list 1 1)))
       (1 ;; left-L
	(list (list 0 0) (list 1 0) (list 1 1) (list 1 2)))
       (2 ;; right-L
	(list (list 0 0) (list 1 0) (list 0 1) (list 0 2)))
       (3 ;; long
	(list (list 0 0) (list 0 1) (list 0 2) (list 0 3)))
       (4 ;; s
	(list (list 0 0) (list 1 0) (list 1 1) (list 2 1)))
       (5 ;; z
	(list (list 0 1) (list 1 1) (list 1 0) (list 2 0))))
     :color 
     (switch (if *random-color* s (random 6))
       (0 (encode-rgb 255 0 0))
       (1 (encode-rgb 0 255 0))
       (2 (encode-rgb 0 0 255))
       (3 (encode-rgb 0 127 127))
       (4 (encode-rgb 127 0 127))
       (5 (encode-rgb 127 127 0))))))


(defun collision-p (s)
  (dolist (p (shape-points s))
    (destructuring-bind (x y) p
      (when (cell (+ (shape-x s) x)
		  (+ (shape-y s) y))
	(return-from collision-p t))))
  nil)

(defun adjust-shape-pos (s)
  "Ensure no y coord is negative"
  (let ((min-y (apply #'min (mapcar #'second (shape-points s)))))
    (when (< min-y 0)
      (dolist (p (shape-points s))	
	(incf (second p) (- min-y)))))
  (let ((min-x (apply #'min (mapcar #'first (shape-points s)))))
    (when (< min-x 0)
      (dolist (p (shape-points s))
	(incf (first p) (- min-x))))))

(defun rotate-shape-r (s)
  (mapc (lambda (p)
	  ;; x' = y , y' = -x
	  (let ((x (first p))
		(y (second p)))
	    (setf (first p) y
		  (second p) (- x))))
	(shape-points s))
  (adjust-shape-pos s))

(defun rotate-shape-l (s)
  (mapc (lambda (p)
	  ;; x' = -y , y' = x
	  (let ((x (first p))
		(y (second p)))
	    (setf (first p) (- y)
		  (second p) x)))
	(shape-points s))
  (adjust-shape-pos s))

(defun shift-shape-l (s)
  (when (> (shape-x s) 0)
    (decf (shape-x s))
    (when (collision-p s)
      (incf (shape-x s)))))

(defun shift-shape-r (s)
  (let ((max-x (apply #'max (mapcar #'first (shape-points s)))))
    (when (< (+ (shape-x s) max-x) (1- *width*))
      (incf (shape-x s))
      (when (collision-p s)
	(decf (shape-x s))))))
   
(defstruct tetris
  cells
  shape 
  next
  speed
  level
  next-level
  score)

(defun new-game ()
  (setf *tetris*
	(make-tetris
	 :cells (make-array (list *width* *height*)
			    :initial-element nil)
	 :shape (random-shape)
	 :next (random-shape)
	 :speed 250
	 :level 1
	 :next-level 10
	 :score 0)))
(defun cell (x y)
  (when (and (>= x 0) (< x *width*)
	     (>= y 0) (< y *height*))
    (aref (tetris-cells *tetris*) x y)))
(defun (setf cell) (value x y)
  (setf (aref (tetris-cells *tetris*) x y) value))

(defun shift-lines (row)
  (do ((y row (1+ y)))
      ((= y (1- *height*)))
    (do ((x 0 (1+ x)))
	((= x *width*))
      (setf (cell x y)
	    (if (= y (1- *height*))
		nil
		(cell x (1+ y)))))))

(defun shift-shape-down ()
  (let ((s (tetris-shape *tetris*)))
    (cond
      ((some (lambda (p)
	       (destructuring-bind (x y) p
		 ;; collision if bottom row or something in the row below 
		 (or (zerop y)
		     (cell x (1- y)))))
	     (shape-cells s))
       (mapc (lambda (p)
	       (destructuring-bind (x y) p
		 (setf (cell x y) (shape-color s))))
	     (shape-cells s))
       (setf (tetris-shape *tetris*)
	     (tetris-next *tetris*)
	     (tetris-next *tetris*)
	     (random-shape)))
      (t
       (decf (shape-y s))))))
  
(defun update-game (hwnd)
  ;; 1. if the piece would collide with a filled cell then
  ;; we copy the piece into the cells, assign the next shapre
  ;; to the current shape and generate a new next shape
  (let ((s (tetris-shape *tetris*)))

    (shift-shape-down)
    
    ;; 2. if filling in the cells also completes a line,
    ;; then clear the cells, move all cells above downwards and
    ;; keep repeating that until no lines are complete.
    ;; 3. for each completed line, increment the score by width*level.
    (do ((i (- (shape-y s) 2)))
	((= i (+ (shape-y s) 3)))
      (cond
	((do ((j 0 (1+ j))
	      (complete t))
	     ((or (not complete) (= j *width*)) complete)
	   (unless (cell j i) (setf complete nil)))
	 ;; line complete -- move everything downwards 
	 (shift-lines i)
	 (incf (tetris-score *tetris*) (* (tetris-level *tetris*) *width*))
	     
	 ;; 4. if line complete, decrement the next-level counter.
	 ;; if that reaches zero then increment the level,
	 ;; decrement the speed and reset the next-level counter.
	 (decf (tetris-next-level *tetris*))
	 (when (zerop (tetris-next-level *tetris*))
	   (incf (tetris-level *tetris*))
	   (setf (tetris-speed *tetris*)
		 (max 15 (- (tetris-speed *tetris*) 50))
		 (tetris-next-level *tetris*)
		 10)
	   (set-timer :hwnd hwnd
		      :elapse (tetris-speed *tetris*)
		      :replace-timer 1)))
	(t
	 ;; line incomplete -- increment row counter
	 (incf i)))))
      
  *tetris*)

    
;; --------------- GUI -------------------

(defvar *paused* nil)
(defparameter *w* 15)

(defun tetris-paint (hwnd)
  
  (with-paint (hwnd hdc)

    (draw-edge hdc
	       (make-rect :left 49
			  :right (+ 50 (* *w* *width*))
			  :top 50
			  :bottom (+ 100 (* *w* *height*)))
	       :inner-edge :sunk
	       :outer-edge :raised
	       :flags '(:left :top :right :bottom :adjust))
    
    ;; draw cells
    (dotimes (i *width*)
      (dotimes (j *height*)
	(let ((c (cell i j)))
	  (when c
	    (let* ((brush (create-solid-brush c))
		   (hold-brush (select-object hdc brush)))
	      (rectangle hdc
			 (+ 50 (* i *w*)) (+ 85 (* (- *height* j) *w*))
			 (+ 50 *w* (* i *w*)) (+ 85 *w* (* (- *height* j) *w*)))
	      (select-object hdc hold-brush)
	      (delete-object brush))))))

    ;; draw shape
    (dolist (p (shape-cells (tetris-shape *tetris*)))
      (destructuring-bind (i j) p
	(let* ((brush (create-solid-brush (shape-color (tetris-shape *tetris*))))
	       (hold-brush (select-object hdc brush)))
	  (rectangle hdc
		     (+ 50 (* i *w*)) (+ 85 (* (- *height* j) *w*))
		     (+ 50 *w* (* i *w*)) (+ 85 *w* (* (- *height* j) *w*)))
	  (select-object hdc hold-brush)
	(delete-object brush))))
      
    
    ;; print score
    (draw-edge hdc
	       (make-rect :left (+ 100 (* *w* *width*))
			  :top 50
			  :right (+ 180 (* *w* *width*))
			  :bottom 120)
	       :flags '(:left :top :right :bottom))
    (text-out hdc
	      (format nil "Score: ~A" (tetris-score *tetris*))
	      (+ 100 *w* (* *w* *width*))
	      65)
    (text-out hdc
	      (format nil "Level: ~A" (tetris-level *tetris*))
	      (+ 100 *w* (* *w* *width*))
	      85)

    ;; show next shape
    (let* ((brush (create-solid-brush (shape-color (tetris-next *tetris*))))
	   (hold-brush (select-object hdc brush)))
      (dolist (p (shape-points (tetris-next *tetris*)))
	(destructuring-bind (i j) p
	  (rectangle hdc
		     (+ 100 (* *width* *w*) (* i *w*))
		     (+ 200 (* j *w*))
		     (+ 100 *w* (* *width* *w*) (* i *w*))
		     (+ 200 *w* (* j *w*)))))

      (select-object hdc hold-brush)
      (delete-object brush))))


(defun tetris-keydown (hwnd wparam)
  (let ((key (virtual-code-key wparam)))
    (case key
      (:left
       (shift-shape-l (tetris-shape *tetris*)))
      (:right
       (shift-shape-r (tetris-shape *tetris*)))
      (:up
       (rotate-shape-r (tetris-shape *tetris*)))
      (:down
       (update-game hwnd))
      (:keyq
       (destroy-window hwnd))
      (:keyn
       (new-game))
      (:keyp (setf *paused* t))))
  (invalidate-rect hwnd nil t))
    

(defwndproc tetris-wndproc (hwnd msg wparam lparam)
  (switch msg
    ((const +wm-create+)
     ;; new game
     (new-game)
     
     ;; set update timer
     (set-timer :hwnd hwnd :elapse (tetris-speed *tetris*) :replace-timer 1))
    ((const +wm-destroy+)
     (post-quit-message))
    ((const +wm-paint+)
     (tetris-paint hwnd))
    ((const +wm-keydown+)
     (tetris-keydown hwnd wparam))
    ((const +wm-timer+)
     (update-game hwnd)
     (invalidate-rect hwnd nil t)))
  
  (default-window-proc hwnd msg wparam lparam))

(defun tetris ()
  (default-message-loop (cffi:callback tetris-wndproc)
      :class-name "FTW_TETRIS"
      :title "Tetris"
      :width 400 :height (* *w* (+ *height* 12))))

    
