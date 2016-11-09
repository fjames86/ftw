;;;; Copyright (c) Frank James 2016 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

;;; This is a simple pac-man type game.
;;; It's a naive implementation - the following are left as an 
;;; excersise for the reader:
;;; 1. Use bitmaps instead of geometric shapes for the characters especially the ghosts
;;;    - alternative is to work out how to draw the ghosts using poly-polybezier 
;;; 2. Write an actual AI for the ghosts. At the moment they go in a stright line until
;;; they hit a wall then choose randomly.
;;; 3. Add different levels
;;; 4. Add the fruit 
;;; 5. Animations between levels
;;; 6. High score save file. 
;;; 7. Basically all state is held by lots of globals. This could be consolidated into 
;;; a struct which holds all the state.
;;; 
;;; See here for more info 
;;; http://www.gamasutra.com/view/feature/3938/the_pacman_dossier.php?print=1
;;;



(defpackage #:ftw.macroman
  (:use #:cl #:ftw)
  (:export #:macroman))

(in-package #:ftw.macroman)

;; map is 26 wide and 29 high -- we model as a set of bitmaps

(defconstant +map-width+ 26)
(defconstant +map-height+ 29)

;; bitmap of which cells are walls and which are not 
(defparameter *map-walls*
  '(#b00000000000011000000000000
    #b01111011111011011111011110
    #b01111011111011011111011110
    #b01111011111011011111011110
    #b00000000000000000000000000
    #b01111011011111111011011110
    #b01111011011111111011011110
    #b00000011000011000011000000
    #b11111011111011011111011111
    #b11111011111011011111011111
    #b11111011000000000011011111
    #b11111011011100111011011111
    #b11111011010000001011011111
    #b00000000010000001000000000
    #b11111011010000001011011111
    #b11111011011111111011011111
    #b11111011000000000011011111
    #b11111011011111111011011111
    #b11111011011111111011011111
    #b00000000000011000000000000
    #b01111011111011011111011110
    #b01111011111011011111011110
    #b00011000000000000000011000
    #b11011011011111111011011011
    #b11011011011111111011011011
    #b00000011000011000011000000
    #b01111111111011011111111110
    #b01111111111011011111111110
    #b00000000000000000000000000))

;; where to place initial dots (but not pellets)
(defparameter *initial-dots*
  '(#b11111111111100111111111111
    #b10000100000100100000100001
    #b00000100000100100000100000
    #b10000100000100100000100001
    #b11111111111111111111111111
    #b10000100100000000100100001
    #b10000100100000000100100001
    #b11111100111100111100111111
    #b00000100000100100000100000
    #b00000100000100100000100000
    #b00000100000000000000100000
    #b00000100000000000000100000
    #b00000100000000000000100000
    #b00000100000000000000100000
    #b00000100000000000000100000
    #b00000100000000000000100000
    #b00000100000000000000100000
    #b00000100000000000000100000
    #b00000100000000000000100000
    #b11111111111100111111111111
    #b10000100000100100000100001
    #b10000100000100100000100001
    #b01100111111111111111100110
    #b00100100100000000100100100
    #b00100100100000000100100100
    #b11111100111100111100111111
    #b10000000000100100000000001
    #b10000000000100100000000001
    #b11111111111111111111111111))

;; where to draw walls. these are drawn as polygons because their ends meet.
(defparameter *walls*
  '(((1 1) (4 1) (4 3) (1 3))
    ((6 1) (10 1) (10 3) (6 3))
    ((15 1) (19 1) (19 3) (15 3))
    ((21 1) (24 1) (24 3) (21 3))
    ((1 5) (4 5) (4 6) (1 6))
    ((6 5) (7 5) (7 8) (10 8) (10 9) (7 9) (7 12) (6 12))
    ((9 5) (16 5) (16 6) (13 6) (13 9) (12 9) (12 6) (9 6))
    ((18 5) (19 5) (19 12) (18 12) (18 9) (15 9) (15 8) (18 8))
    ((21 5) (24 5) (24 6) (21 6))
    ((6 14) (7 14) (7 18) (6 18))
    ((9 17) (16 17) (16 18) (13 18) (13 21) (12 21) (12 18) (9 18))
    ((18 14) (19 14) (19 18) (18 18))
    ((1 20) (4 20) (4 24) (3 24) (3 21) (1 21))
    ((6 20) (10 20) (10 21) (6 21))
    ((15 20) (19 20) (19 21) (15 21))
    ((21 20) (24 20) (24 21) (22 21) (22 24) (21 24))
    ((1 26) (6 26) (6 23) (7 23) (7 26) (10 26) (10 27) (1 27))
    ((9 23) (16 23) (16 24) (13 24) (13 27) (12 27) (12 24) (9 24))
    ((15 26) (18 26) (18 23) (19 23) (19 26) (24 26) (24 27) (15 27))))

;; where to draw edges. these are drawn as lines because their ends don't meet.
(defparameter *map-edges*
  '(((-1 12) (4 12) (4 8) (-1 8) (-1 -1) (12 -1) (12 3) (13 3) (13 -1) (26 -1) (26 8) (21 8) (21 12) (26 12))
    ((-1 14) (4 14) (4 18) (-1 18) (-1 23) (1 23) (1 24) (-1 24) (-1 29) (26 29)
     (26 24) (24 24) (24 23) (26 23) (26 18) (21 18) (21 14) (26 14))
    ((14 11) (16 11) (16 15) (11 15) (9 15) (9 11) (11 11))))

;; ----------------- game logic ---------------------

(defun wallp (x y)
  "Does this cell have a wall in it? i.e. can ghosts or players pass through it." 
  (cond
    ((and (= y 13) (= x +map-width+))
     nil)
    ((and (= y 13) (= x -1))
     nil)
    ((and (>= x 0) (< x +map-width+)
	  (>= y 0) (< y +map-height+))
     (not
      (zerop
       (logand (nth y *map-walls*)
	       (ash 1 (- (1- +map-width+) x))))))
    (t t)))

(defun leftp (x y)
  ;; special cases for central tunnels
  (cond
    ((and (= y 13) (= x 0))
     (1- +map-width+))
    ((= x 0) nil)
    ((wallp (1- x) y) nil)
    (t (1- x))))

(defun rightp (x y)
  ;; special cases for central tunnels
  (cond
    ((and (= y 13) (= x 0))
     (1- +map-width+))
    ((= x (1- +map-width+)) nil)
    ((wallp (1+ x) y) nil)
    (t (1+ x))))
  
(defun upp (x y)
  (cond
    ((= y 0) nil)
    ((wallp x (1- y)) nil)
    (t (1- y))))

(defun downp (x y)
  (cond
    ((= y (1- +map-height+)) nil)
    ((wallp x (1+ y)) nil)
    (t (1+ y))))

(defun dirp (x y dir)
  (ecase dir
    (:left (leftp x y))
    (:right (rightp x y))
    (:up (upp x y))
    (:down (downp x y))))

(defparameter *dots* nil)
(defun init-dots ()
  (setf *dots*
	(make-array (list +map-width+ +map-height+)
		    :initial-element nil))
  (dotimes (x +map-width+)
    (dotimes (y +map-height+)
      (unless (zerop
	       (logand (nth y *initial-dots*)
		       (ash 1 (- (1- +map-width+) x))))
	(setf (aref *dots* x y) :dot))))
  (setf (aref *dots* 0 2) :pellet
	(aref *dots* (1- +map-width+) 2) :pellet
	(aref *dots* 0 22) :pellet
	(aref *dots* (1- +map-width+) 22) :pellet))

(defun dotp (x y)
  (unless *dots* (init-dots))
  (eq (aref *dots* x y) :dot))
(defun pelletp (x y)
  (unless *dots* (init-dots))
  (eq (aref *dots* x y) :pellet))
(defun clear-dot (x y)
  (unless *dots* (init-dots))
  (setf (aref *dots* x y) nil))


;; game state 
(defparameter *x* 12)
(defparameter *y* 22)
(defparameter *dir* :right)
(defparameter *try-dir* nil)
(defparameter *score* 0)
(defparameter *mouth* 0.0
  "Angle in radians the mouth is open.")
(defparameter *mouth-dir* (/ pi 16.0))
(defparameter *invincible* nil)
(defparameter *lives* 3)
(defparameter *paused* nil)

(defun move-mouth ()
  (incf *mouth* *mouth-dir*)
  (cond
    ((>= *mouth* (/ pi 4.0))
     (setf *mouth-dir* (- *mouth-dir*)
	   *mouth* (/ pi 4.0)))
    ((<= *mouth* 0) 
     (setf *mouth-dir* (- *mouth-dir*)
	   *mouth* 0.0))))

(defun move-player (x y)
  (ecase *dir*
    (:left (setf x (1- x)))
    (:right (setf x (1+ x)))
    (:up (setf y (1- y)))
    (:down (setf y (1+ y))))
  (cond 
    ((and *try-dir* (dirp *x* *y* *try-dir*))
     (setf *dir* *try-dir*
	   *try-dir* nil))
    ((not (wallp x y))
     (setf *x* (mod x +map-width+)
	   *y* y
	   x *x*
	   y *y*)
     (move-mouth)
     
     (when (dotp x y)
       (clear-dot x y)
       (incf *score* 10))
     
     (when (pelletp x y)
       (clear-dot x y)
       (incf *score* 50)
       (setf *invincible* 100)))))


(defstruct ghost 
  name x y dir (eyes 0))

(defparameter *ghosts*
  (list (make-ghost :name :blinky :x 13 :y 13 :dir :up)
	(make-ghost :name :pinky :x 13 :y 13 :dir :up)
	(make-ghost :name :inky :x 13 :y 13 :dir :up)
	(make-ghost :name :clyde :x 13 :y 13 :dir :up)))

(defun ghost-choose-dir (g)
  (let ((dir (ghost-dir g)))
    (dolist (d 
	      (ecase dir
		((:up :down) '(:left :right))
		((:left :right) '(:up :down))))
      (when (dirp (ghost-x g) (ghost-y g) d)
	(setf (ghost-dir g) d)))))

(defun move-ghost (g)
  "Move the ghost one place. This is the function which should dispatch
to the AI to decide how to move. Each ghost should have a slightly different
AI personality. 
Blinky chases macroman.
Pinky and Inky try to position themselves in front of macroman.
Clyde is random but also chases macroman and moves to lower left when macroman gets to close.

At the moment it chooses randomly which is pretty bad and makes the game too easy.
"
  (let ((x (ghost-x g))
	(y (ghost-y g)))
    (ecase (ghost-dir g)
      (:left (setf x (1- x)))
      (:right (setf x (1+ x)))
      (:up (setf y (1- y)))
      (:down (setf y (1+ y))))
    (cond 
      ((wallp x y)
       (setf (ghost-dir g) 
	     (nth (random 2)
		  (ecase (ghost-dir g)
		    ((:left :right) '(:up :down))
		    ((:up :down) '(:left :right))))))
      (t 
       (setf x (mod x +map-width+))
       (setf (ghost-x g) x
	     (ghost-y g) y)))))

(defun init-ghosts ()
  (setf *ghosts*
	(list (make-ghost :name :blinky :x 13 :y 13 :dir :up)
	      (make-ghost :name :pinky :x 13 :y 13 :dir :up)
	      (make-ghost :name :inky :x 13 :y 13 :dir :up)
	      (make-ghost :name :clyde :x 13 :y 13 :dir :up))))
	
(defun new-game ()
  (init-dots)
  (setf *x* 12 
	*y* 22
	*dir* :up
	*score* 0
	*lives* 3)
  (init-ghosts))

(defun detect-hits ()
  (dolist (g *ghosts*)
    (let ((x (ghost-x g))
	  (y (ghost-y g)))
      (when (and (>= x (1- *x*)) (<= x (1+ *x*))
		 (>= y (1- *y*)) (<= y (1+ *y*)))
	(cond
	  (*invincible*
	   ;; kill ghost 
	   (setf (ghost-x g) 13
		 (ghost-y g) 13)
	   (incf *score* 150))
	  (t 
	   ;; kill player 
	   (decf *lives*)
	   (setf *x* 12 *y* 22
		 *invincible* 30)
	   (init-ghosts)
	   (when (= *lives* 0)
	     (new-game))))))))
	  
(defun move ()
  (move-player *x* *y*)
  (dolist (g *ghosts*)
    (move-ghost g))
  (detect-hits))

	  
		


;; ------------- GUI ---------------

(defun macroman-create (hwnd)
  (new-game)
  (set-timer :hwnd hwnd
	     :elapse 75
	     :replace-timer 1))

(defconstant +left-margin+ 100)
(defconstant +top-margin+ 100)
(defconstant +width+ 12)
(defconstant +height+ 15)

(defun translate-point (p)
  (destructuring-bind (x y) p
    (list (+ +left-margin+ (truncate +width+ 2) (* x +width+))
          (+ +top-margin+ (truncate +height+ 2) (* y +height+)))))
  
(defun draw-wall (hdc wall-points)
  (select-object hdc (get-stock-object :black-brush))
  (select-object hdc (get-stock-object :white-pen))
  (polygon hdc (mapcar #'translate-point wall-points)))

(defun draw-map-edge (hdc points)
  (select-object hdc (get-stock-object :black-brush))
  (select-object hdc (get-stock-object :white-pen))
  (polyline hdc (mapcar #'translate-point points)))
            
(defun draw-cell (hdc x y)
  (select-object hdc (get-stock-object :black-brush))
  (rectangle hdc
             (+ +left-margin+ (* x +width+))
             (+ +top-margin+ (* y +height+))
             (+ +left-margin+ (* (1+ x) +width+))
             (+ +top-margin+ (* (1+ y) +height+))))

(defun draw-dot (hdc x y)
  (select-object hdc (get-stock-object :grey-brush))
  (ellipse hdc
           (+ +left-margin+ (- (truncate +width+ 2) 3) (* x +width+))
           (+ +top-margin+ (- (truncate +height+ 2) 3) (* y +height+))
           (+ +left-margin+ (+ (truncate +width+ 2) 3) (* x +width+))
           (+ +top-margin+ (+ (truncate +height+ 2) 3) (* y +height+))))

(defun draw-pellet (hdc x y)
  (select-object hdc (get-stock-object :grey-brush))
  (ellipse hdc
           (+ +left-margin+ (* x +width+))
           (+ +top-margin+ (* y +height+))
           (+ +left-margin+ +width+ (* x +width+))
           (+ +top-margin+ +width+ (* y +height+))))
  
(defun draw-block (hdc x y)
  (select-object hdc (get-stock-object :black-pen))
  (draw-cell hdc x y)
  (when (dotp x y)
    (draw-dot hdc x y))
  (when (pelletp x y)
    (draw-pellet hdc x y)))

  
;; To draw the player we draw a pie shape. The wedge needs to be taken out of 
;; the top/right/bottom/left depending on the directio nthe character is moving 
(defun draw-player (hdc)
  (if (and *invincible* (zerop (mod *invincible* 2)))
      (select-object hdc (get-stock-object :gray-brush))    
      (select-object hdc (get-stock-object :white-brush)))

  (let* ((theta (ecase *dir*
		  (:left (- pi))
		  (:right 0)
		  (:up (- (/ pi 2)))
		  (:down (/ pi 2))))
	 (radius (truncate (+ 5 (truncate (* 3/2 +width+))) 2))
	 (x (+ +left-margin+ (* *x* +width+) radius))
	 (y (+ +top-margin+ (* *y* +height+) radius))

	 (left (- x radius))
	 (top (- y radius))
	 (right (+ x radius))
	 (bottom (+ y radius)))
    (pie hdc
	 left top right bottom 
	 (truncate (+ x (* radius (cos (- theta *mouth*)))))
	 (truncate (+ y (* radius (sin (- theta *mouth*)))))
	 (truncate (+ x (* radius (cos (+ theta *mouth*)))))
	 (truncate (+ y (* radius (sin (+ theta *mouth*))))))))


(defun draw-score (hdc)
  (set-bk-mode hdc :transparent)
  (set-text-color hdc (encode-rgb 255 255 255))
  (text-out hdc "HIGH SCORE" 300 25)
  (text-out hdc (format nil "~A" *score*) 325 50)
  (text-out hdc "ONE UP" 150 25)
  (text-out hdc (format nil "~A" *lives*) 175 50)
  (when *paused* (text-out hdc "PAUSED" 225 60)))

(defun draw-ghost (hdc g)
  (let* ((brush (create-solid-brush 
		 (if *invincible*
		     (if (zerop (mod *invincible* 2))
			 (encode-rgb 0 0 235)
			 (encode-rgb 198 193 182))
		     (ecase (ghost-name g)
		       (:blinky (encode-rgb 236 0 0))
		       (:pinky (encode-rgb 255 87 225))
		       (:inky (encode-rgb 3 198 215))
		       (:clyde (encode-rgb 248 188 45))))))
	 (hold-brush (select-object hdc brush))
	 (x (ghost-x g))
	 (y (ghost-y g))
	 (px (first (translate-point (list x y))))
	 (py (second (translate-point (list x y)))))	   
    (select-object hdc (get-stock-object :white-pen))
    ;; (ellipse hdc 
    ;; 	     (+ +left-margin+ (* x +width+) (- (truncate +width+ 3)))
    ;; 	     (+ +top-margin+ (* y +height+) (- (truncate +height+ 7)))
    ;; 	     (+ +left-margin+ (* x +width+) +width+ (+ (truncate +width+ 3)))
    ;; 	     (+ +top-margin+ (* y +height+) +height+ (truncate +height+ 7)))

    
    (polygon hdc
	     (mapcar (lambda (p)
		       (list (+ (first p) px (- +width+))
			       (+ (second p) py (- +height+))))
		     '((15 5) (16 6) (17 7) (18 7) (20 8) (21 9) (22 10) (23 14) (24 21) (25 25)
		       (25 25) (22 20) (18 25) (18 20) (12 20) (12 25) (8 20) (5 25)
		       (5 25) (6 20) (7 14) (8 10) (9 9) (10 8) (12 7) (13 7) (14 6) (15 5))))
    
    (select-object hdc hold-brush)
    (delete-object brush)

    (select-object hdc (get-stock-object :white-brush))
    (ellipse hdc (+ px -2) (+ py -6) (+ px 3) (+ py 2))
    (ellipse hdc (+ px 4) (+ py -6) (+ px 9) (+ py 2))

    (select-object hdc (get-stock-object :black-brush))
    (select-object hdc (get-stock-object :black-pen))
    (cond
      ((< (ghost-eyes g) 5)
       (ellipse hdc (+ px -2) (+ py -5) (+ px 0) (+ py 0))
       (ellipse hdc (+ px 2) (+ py -5) (+ px 4) (+ py 0)))
      ((< (ghost-eyes g) 10)
       (ellipse hdc (+ px 2) (+ py -5) (+ px 2) (+ py 0))
       (ellipse hdc (+ px 7) (+ py -5) (+ px 9) (+ py 0))))
    (incf (ghost-eyes g))
    (when (> (ghost-eyes g) 10)
      (setf (ghost-eyes g) 0))
    
    ))
    

       
(defun macroman-paint (hwnd)
  (with-double-buffering (hdc hwnd)
    (dotimes (x +map-width+)
      (dotimes (y +map-height+)
	(draw-block hdc x y)))
    (dolist (wall-points *walls*)
      (draw-wall hdc wall-points))
    (dolist (edge-points *map-edges*)
      (draw-map-edge hdc edge-points))
    (draw-player hdc)
    (draw-score hdc)
    (dolist (g *ghosts*)
      (draw-ghost hdc g))))


(defun macroman-timer (hwnd)
  (unless *paused*
    (move)
    (when *invincible*
      (decf *invincible*)
      (when (zerop *invincible*)
	(setf *invincible* nil))))
  (invalidate-rect hwnd nil t))

(defun macroman-keydown (hwnd wparam)
  (let ((key (virtual-code-key wparam)))
    (case key 
      ((:left :right :up :down) 
       (setf *try-dir* key))
      (:keyq (destroy-window hwnd))
      (:keyn (new-game))
      (:keyp (setf *paused* (not *paused*)))
      (:keyh 
       (setf *paused* t)
       (message-box :hwnd hwnd
			  :text "
Macroman - pacman type game written in Common Lisp.
Copyright (c) Frank James 2016. 

Controls:
  left right up down       Move macroman.
  P                        Pause/unpause game.
  N                        New game.
  Q                        Quit.
  H                        This message.
"
			  :caption "Help" 
			  :icon :information)
       (setf *paused* nil)))))

(defwndproc macroman-wndproc (hwnd msg wparam lparam)
  (switch msg
    ((const +wm-create+)
     (macroman-create hwnd))
    ((const +wm-destroy+)
     (post-quit-message))
    ((const +wm-paint+)
     (macroman-paint hwnd))
    ((const +wm-timer+)
     (macroman-timer hwnd))
    ((const +wm-keydown+)
     (macroman-keydown hwnd wparam)))
     
  (default-window-proc hwnd msg wparam lparam))

(defun macroman ()
  (default-message-loop (cffi:callback macroman-wndproc)
      :class-name "FTW_MACROMAN"
      :title "Macroman"
      :width 500 :height 650
      :background (get-stock-object :black-brush)))
