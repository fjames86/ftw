;;;; Copyright (c) Frank James 2016 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

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
(defvar *ms* nil)

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
					    ((or (zerop left) (zerop m)) nil)
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

(defun unclicked-neighbours (ms x y)
  (let ((neighbours nil))
    (do ((i (1- x) (1+ i)))
	((= i (+ x 2)))
      (do ((j (1- y) (1+ j)))
	  ((= j (+ y 2)))
	(when (and (>= i 0) (< i (minesweeper-x ms))
		   (>= j 0) (< j (minesweeper-y ms))
		   (not (and (= i x) (= j y)))
		   (not (clicked-p ms i j)))
	  (push (list i j) neighbours))))
    neighbours))
	 
      
(defun click-provable-cells (ms x y)
  "If this location has a mine count of 0 then
look at all the unclicked cells around this location and click those too.
Repeat recursively." 
  (when (= (mines ms x y) 0)
    (dolist (n (unclicked-neighbours ms x y))
      (click-cell ms (first n) (second n)))))

(defun click-cell (ms x y)
  (cond
    ((mine-p ms x y)
     :mine)
    (t 
     (setf (cell ms x y) (list (mines ms x y)))
     (click-provable-cells ms x y)
     (cell ms x y))))

(defun resize-window (hwnd)
  "Set the window to the size required for the game" 
  (let ((w (minesweeper-x *ms*))
	(h (minesweeper-y *ms*)))	  
    (set-window-pos hwnd :top
		    0 0 
		    (+ 100 (* w 25))
		    (+ 175 (* h 25))
		    '(:no-move))))

(defun game-won-p ()
  "Returns true if all mines have a flag placed on them." 
  (let ((mines 0)
	(correct 0))
    (dotimes (i (minesweeper-x *ms*))
      (dotimes (j (minesweeper-y *ms*))
	(when (mine-p *ms* i j)
	  (incf mines)
	  (when (flag-p *ms* i j)
	    (incf correct)))))
    (= mines correct)))

;; I made a little icon in gimp and exported it as a microsoft icon (*.ico) file. 
;; Then I ran generate-icon-resource on that file. I pasted the output below.
(defvar *MINE-ICON*
        (create-icon 32 32 1 32
                     (make-array 4224 :element-type '(unsigned-byte 8))
                     #(#x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #xFC #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x5F 
                        #x00 #x00 #x00 #xD5 #x00 #x00 #x00 #x3E #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x77 
                        #x00 #x00 #x00 #xB6 #x00 #x00 #x00 #x5A #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #xE9 
                        #x00 #x00 #x00 #xF3 #x00 #x00 #x00 #xF3 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #xAA #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x03 
                        #x00 #x00 #x00 #x35 #x00 #x00 #x00 #x83 #x00 #x00 #x00 #xCA #x00 #x00 #x00 #xF5 
                        #x00 #x00 #x00 #xF5 #x00 #x00 #x00 #xFF #x00 #x00 #x00 #x83 #x00 #x00 #x00 #x35 
                        #x00 #x00 #x00 #x03 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x6C #x00 #x00 #x00 #x6C #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #xCE 
                        #x00 #x00 #x00 #x6C #x00 #x00 #x00 #x01 #x00 #x00 #x00 #x56 #x00 #x00 #x00 #xFF 
                        #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF 
                        #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF 
                        #x00 #x00 #x00 #xFF #x00 #x00 #x00 #x56 #x00 #x00 #x00 #x01 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x6C #x00 #x00 #x00 #xAA #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #xE4 #x00 #x00 #x00 #xC3 #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF 
                        #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF 
                        #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF 
                        #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xC3 #x00 #x00 #x00 #xB0 
                        #x00 #x00 #x00 #xAA #x00 #x00 #x00 #x6C #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x13 
                        #x00 #x00 #x00 #xFA #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF 
                        #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF 
                        #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF 
                        #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xF7 
                        #x00 #x00 #x00 #x13 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x03 #x00 #x00 #x00 #xD3 
                        #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF 
                        #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF 
                        #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF 
                        #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF 
                        #x00 #x00 #x00 #xD3 #x00 #x00 #x00 #x03 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x6B #x00 #x00 #x00 #xFF 
                        #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF 
                        #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF 
                        #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF 
                        #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF 
                        #x00 #x00 #x00 #xFF #x00 #x00 #x00 #x6B #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x09 #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF 
                        #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF 
                        #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF 
                        #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF 
                        #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF 
                        #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #x09 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x4A #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF 
                        #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF 
                        #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF 
                        #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF 
                        #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF 
                        #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #x4A #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x9E #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF 
                        #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF 
                        #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF 
                        #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF 
                        #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF 
                        #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #x9E #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x6F 
                        #x00 #x00 #x00 #xAE #x00 #x00 #x00 #xE0 #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF 
                        #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF 
                        #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x17 #x17 #x17 #xFF 
                        #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF 
                        #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF 
                        #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xE0 #x00 #x00 #x00 #xDA 
                        #x00 #x00 #x00 #x53 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xF3 #x00 #x00 #x00 #xCA 
                        #x00 #x00 #x00 #xE9 #x00 #x00 #x00 #xFB #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF 
                        #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF 
                        #x17 #x17 #x17 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF 
                        #x17 #x17 #x17 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF 
                        #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF 
                        #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFB #x00 #x00 #x00 #xD5 
                        #x00 #x00 #x00 #xA3 #x00 #x00 #x00 #xFC #x00 #x00 #x00 #xFF #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #xAE 
                        #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xE0 #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF 
                        #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x17 #x17 #x17 #xFF #x17 #x17 #x17 #xFF 
                        #x37 #x37 #x37 #xFF #x37 #x37 #x37 #xFF #x37 #x37 #x37 #xFF #x37 #x37 #x37 #xFF 
                        #x37 #x37 #x37 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF 
                        #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF 
                        #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xE0 #x00 #x00 #x00 #xDF 
                        #x00 #x00 #x00 #x98 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x9E #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF 
                        #x00 #x00 #x00 #xFF #x17 #x17 #x17 #xFF #x17 #x17 #x17 #xFF #x37 #x37 #x37 #xFF 
                        #x37 #x37 #x37 #xFF #x5B #x5B #x5B #xFF #x5B #x5B #x5B #xFF #x5B #x5B #x5B #xFF 
                        #x37 #x37 #x37 #xFF #x37 #x37 #x37 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF 
                        #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF 
                        #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #x9E #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x4A #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF 
                        #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x37 #x37 #x37 #xFF #x5B #x5B #x5B #xFF 
                        #x5B #x5B #x5B #xFF #x5B #x5B #x5B #xFF #x5B #x5B #x5B #xFF #x5B #x5B #x5B #xFF 
                        #x5B #x5B #x5B #xFF #x37 #x37 #x37 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF 
                        #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF 
                        #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #x4A #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x09 #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF 
                        #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x88 #x88 #x88 #xFF #x88 #x88 #x88 #xFF 
                        #x5B #x5B #x5B #xFF #x88 #x88 #x88 #xFF #x5B #x5B #x5B #xFF #x5B #x5B #x5B #xFF 
                        #x5B #x5B #x5B #xFF #x17 #x17 #x17 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF 
                        #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF 
                        #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #x09 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x6B #x00 #x00 #x00 #xFF 
                        #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x37 #x37 #x37 #xFF #x88 #x88 #x88 #xFF 
                        #x88 #x88 #x88 #xFF #x88 #x88 #x88 #xFF #x88 #x88 #x88 #xFF #x5B #x5B #x5B #xFF 
                        #x5B #x5B #x5B #xFF #x17 #x17 #x17 #xFF #x17 #x17 #x17 #xFF #x00 #x00 #x00 #xFF 
                        #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF 
                        #x00 #x00 #x00 #xFF #x00 #x00 #x00 #x6B #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x03 #x00 #x00 #x00 #xD3 
                        #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x37 #x37 #x37 #xFF 
                        #x88 #x88 #x88 #xFF #x88 #x88 #x88 #xFF #xC7 #xC7 #xC7 #xFF #x5B #x5B #x5B #xFF 
                        #x37 #x37 #x37 #xFF #x17 #x17 #x17 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF 
                        #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF 
                        #x00 #x00 #x00 #xD3 #x00 #x00 #x00 #x03 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x13 
                        #x00 #x00 #x00 #xFC #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF 
                        #x00 #x00 #x00 #xFF #x37 #x37 #x37 #xFF #x88 #x88 #x88 #xFF #x49 #x49 #x49 #xFF 
                        #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF 
                        #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xF7 
                        #x00 #x00 #x00 #x77 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #xAA 
                        #x00 #x00 #x00 #x76 #x00 #x00 #x00 #xC3 #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF 
                        #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF 
                        #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF 
                        #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xC3 #x00 #x00 #x00 #xB0 
                        #x00 #x00 #x00 #x6C #x00 #x00 #x00 #xAA #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x6C #x00 #x00 #x00 #x6C 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x01 #x00 #x00 #x00 #x56 #x00 #x00 #x00 #xFF 
                        #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF 
                        #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF 
                        #x00 #x00 #x00 #xFF #x00 #x00 #x00 #x56 #x00 #x00 #x00 #x01 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x6C #x00 #x00 #x00 #x6C #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x03 
                        #x00 #x00 #x00 #x35 #x00 #x00 #x00 #x83 #x00 #x00 #x00 #xCA #x00 #x00 #x00 #xF5 
                        #x00 #x00 #x00 #xF5 #x00 #x00 #x00 #xFF #x00 #x00 #x00 #x83 #x00 #x00 #x00 #x35 
                        #x00 #x00 #x00 #x03 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x6C #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #xFF 
                        #x00 #x00 #x00 #xFF #x00 #x00 #x00 #xFF #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #xCC 
                        #x00 #x00 #x00 #xCC #x00 #x00 #x00 #x73 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #xC1 
                        #x00 #x00 #x00 #xF3 #x00 #x00 #x00 #x98 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #xFF #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
                        #xFF #xFF #xFF #xFF #xFF #xFF #x7F #xFF #xFF #xFE #x3F #xFF #xFF #xFE #x3F #xFF 
                        #xFF #xFE #x3F #xFF #xFD #xE0 #x07 #x3F #xFE #x00 #x01 #x3F #xFF #x00 #x00 #x3F 
                        #xFE #x00 #x00 #x7F #xFC #x00 #x00 #x3F #xFC #x00 #x00 #x3F #xF8 #x00 #x00 #x1F 
                        #xF8 #x00 #x00 #x1F #xF8 #x00 #x00 #x1F #xE0 #x00 #x00 #x07 #x80 #x00 #x00 #x01 
                        #xE0 #x00 #x00 #x07 #xF8 #x00 #x00 #x1F #xF8 #x00 #x00 #x1F #xF8 #x00 #x00 #x1F 
                        #xFC #x00 #x00 #x3F #xFC #x00 #x00 #x3F #xFE #x00 #x00 #x7F #xFE #x00 #x00 #x3F 
                        #xFC #x80 #x01 #x3F #xFF #xE0 #x07 #xBF #xFF #xFE #x3F #xFF #xFF #xFE #x3F #xFF 
                        #xFF #xFE #x3F #xFF #xFF #xFF #x7F #xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF )))

(defwndproc minesweeper-wndproc (hwnd msg wparam lparam)
  (switch msg
    ((const +wm-create+)
     (setf *ms* (random-game))
     (resize-window hwnd)

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
       (set-bk-mode hdc :transparent)
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
		      (not (clicked-p *ms* i j)))
	     (cond
	       ((flag-p *ms* i j)
		(set-flag *ms* i j nil)
		(incf (minesweeper-flags *ms*)))
	       (t
		(when (> (minesweeper-flags *ms*) 0)
		  (set-flag *ms* i j t)
		  (decf (minesweeper-flags *ms*))
		  (when (game-won-p)
		    (setf (minesweeper-finished *ms*) t)
		    (message-box :hwnd hwnd 
				 :text "You won!"
				 :caption "Win"))))))))
       (invalidate-rect hwnd nil t)))
    ((const +wm-command+)
     (switch (loword wparam)
       (1 ;; new
	(setf *ms* (random-game))
	(resize-window hwnd)
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
	(resize-window hwnd)
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
			 (- (truncate (getf (get-client-rect hwnd) :right 0) 2) 22)
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
      :width 350 :height 425
      :icon *mine-icon*
      :icon-small *mine-icon*))


  
