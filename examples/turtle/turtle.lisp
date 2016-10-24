;;;; Copyright (c) Frank James 2016 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

;;; This file defines a simple little turtle type program.
;;; Users should be able program its movements like you would with Logo.

(defpackage #:ftw.turtle
  (:use #:cl #:cffi #:ftw)
  (:export #:turtle))

(in-package #:ftw.turtle)

(defstruct turtle
  x y                  ;; current position
  theta                ;; forward direction, measured in radians
  pen                  ;; pen colour
  pen-p                ;; pen up or down?
  hdc                  ;; device context for drawing
  commands             ;; commands to execute
  prev-commands        ;; previous command history 
  )
  

(defparameter *turtle* nil)
(defconstant +2pi+ (* 2.0 pi))
(defconstant +turtle-size+ 10)

(defun degrees-radians (degrees)
  (* degrees (/ +2pi+ 360.0)))
(defun radians-degrees (radians)
  (* radians (/ 360.0 +2pi+)))

;; turtle commnads
(defun turtle-right (theta &optional (turtle *turtle*))
  (setf (turtle-theta turtle)
        (+ (turtle-theta turtle) theta)))
(defun turtle-left (theta &optional (turtle *turtle*))
  (turtle-right (- theta) turtle))
(defun turtle-move (distance &optional (turtle *turtle*))
  (let ((x (turtle-x turtle))
        (y (turtle-y turtle))
        (theta (degrees-radians (turtle-theta turtle))))

    (incf (turtle-x turtle) (* distance (cos theta)))
    (incf (turtle-y turtle) (* distance (sin theta)))

    ;; draw line if pen up
    (when (turtle-pen-p turtle)
      (move-to (turtle-hdc turtle) x y)
      (line-to (turtle-hdc turtle) (turtle-x turtle) (turtle-y turtle)))))
(defun turtle-pen-up (&optional (turtle *turtle*))
  (setf (turtle-pen-p turtle) nil))
(defun turtle-pen-down (&optional (turtle *turtle*))
  (setf (turtle-pen-p turtle) t))
(defun set-turtle-pen (pen &optional (turtle *turtle*))
  (setf (turtle-pen turtle) pen)
  (select-object (turtle-hdc turtle) pen))


(defun draw-turtle (&optional (turtle *turtle*))
  "The turtle is a triangle with two smaller triangles for legs"
  (let ((hdc (turtle-hdc turtle))
        (theta (degrees-radians (turtle-theta turtle))))
    ;; move out to leading vertex of turtle
    (move-to hdc
             (+ (turtle-x turtle) (* +turtle-size+ (cos theta)))
             (+ (turtle-y turtle) (* +turtle-size+ (sin theta))))
    ;; draw to bottom right vertex
    (line-to hdc
             (+ (turtle-x turtle) (* +turtle-size+ (cos (+ theta (* 2/3 pi)))))
             (+ (turtle-y turtle) (* +turtle-size+ (sin (+ theta (* 2/3 pi))))))
    ;; draw to bottom left vertex
    (line-to hdc
             (+ (turtle-x turtle) (* +turtle-size+ (cos (+ theta (* 4/3 pi)))))
             (+ (turtle-y turtle) (* +turtle-size+ (sin (+ theta (* 4/3 pi))))))
    ;; draw line back to top vertex
    (line-to hdc
             (+ (turtle-x turtle) (* +turtle-size+ (cos theta)))
             (+ (turtle-y turtle) (* +turtle-size+ (sin theta))))
    ;; draw little pen line
    (move-to hdc
             (+ (turtle-x turtle) (* (- +turtle-size+ 5) (cos theta)))
             (+ (turtle-y turtle) (* (- +turtle-size+ 5) (sin theta))))
    (line-to hdc
             (+ (turtle-x turtle) (* (+ +turtle-size+ 5) (cos theta)))
             (+ (turtle-y turtle) (* (+ +turtle-size+ 5) (sin theta))))
    
    ;; draw little legs?
    ;; TODO
    nil))
    
(defun eval-turtle-command (command &optional (turtle *turtle*))
  (destructuring-bind (cmd &rest args) command
    (ecase cmd
      (left (turtle-left (car args) turtle))
      (right (turtle-right (car args) turtle))
      (move (turtle-move (car args) turtle))
      (up (turtle-pen-up turtle))
      (down (turtle-pen-down turtle))
      ((color colour)
       (let ((name (car args)))
         (cond
           ((keywordp name)
            (ecase name
              (:blue (encode-rgb 0 0 255))
              (:green (encode-rgb 0 255 0))
              (:red (encode-rgb 255 0 0))
              (:yellow (encode-rgb 255 255 0))
              (:white (encode-rgb 255 255 255))
              (:black (encode-rgb 0 0 0))))
           (t
            (destructuring-bind (r g b) args
              (encode-rgb r g b))))))))
  (setf (turtle-prev-commands turtle)
        (append (turtle-prev-commands turtle) (list command))))

(defun eval-turtle (commands &optional (turtle *turtle*))
  (dolist (c commands)
    (eval-turtle-command c turtle)))

(defun parse-turtle-command (string)
  (ignore-errors 
    (let ((index (or (position #\space string :test #'char=)
                     (position #\return string :test #'char=)
                     (position #\newline string :test #'char=))))            
      (cond
        ((string-equal (subseq string 0 index) "left")
         (list 'left
               (parse-number:parse-number string :start (1+ index))))
        ((string-equal (subseq string 0 index) "right")
         (list 'right 
               (parse-number:parse-number string :start (1+ index))))
        ((string-equal (subseq string 0 index) "move")
         (list 'move (parse-number:parse-number string :start (1+ index))))
        ((string-equal (subseq string 0 index) "up")
         (list 'up))
        ((string-equal (subseq string 0 index) "down")
         (list 'down))))))

(defun parse-turtle-commands (string)
  (with-input-from-string (s string)
    (do ((l (read-line s nil nil) (read-line s nil nil))
         (cmds nil))
        ((null l) (nreverse cmds))
      (let ((cmd (parse-turtle-command l)))
        (when cmd 
          (push cmd cmds))))))

;; ------------ GUI -----------------



;; we define a turtle window class whcih is just for the turtle to move around in
;; we can then place this whereever we want on the main gui

(defvar *wm-turtle* (register-window-message "WM_TURTLE"))

(defwndproc turtle-wndproc (hwnd msg wparam lparam)
  (switch msg
    ((const +wm-create+)
     (destructuring-bind (&key left top right bottom) (get-client-rect hwnd)
       (declare (ignore left top))
       (setf *turtle* (make-turtle :x (truncate right 2) :y (truncate bottom 2) :theta 0 :pen-p t))))
    ((const +wm-paint+)
     (with-paint (hwnd hdc)
       ;; set brushes
       (select-object hdc (get-stock-object :black-brush))
       (select-object hdc (get-stock-object :white-pen))
       
       ;; set turtle device context
       (setf (turtle-hdc *turtle*) hdc)
       
       ;; evaluate all previous and new commands
       (let ((prev (turtle-prev-commands *turtle*)))
         (destructuring-bind (&key left top right bottom) (get-client-rect hwnd)
           (declare (ignore left top))
           (setf (turtle-x *turtle*) (truncate right 2)
                 (turtle-y *turtle*) (truncate bottom 2)
                 (turtle-theta *turtle*) 0
                 (turtle-pen-p *turtle*) t))
         (setf (turtle-prev-commands *turtle*) nil)
         (eval-turtle prev)
         (eval-turtle (turtle-commands *turtle*))
         (draw-turtle))
       
       (setf (turtle-commands *turtle*) nil)
       
       (set-window-text *turtle-label* (turtle-label-text))))
    ((const +wm-size+)
     (invalidate-rect hwnd nil t)
     (update-window hwnd))
    (*wm-turtle*
     ;; our custom message -- this sends us various commands
     nil))
  (default-window-proc hwnd msg wparam lparam))

(register-class "TURTLE" (callback turtle-wndproc)
                :background (get-stock-object :black-brush))





;; -------------------- Main GUI ---------------------



;; we want the turtle to be on the left maybe with x/y/theta/pen info in a static label
;; on the right should be an edit control to accept commands 

(defparameter *turtle-label* nil)
(defparameter *turtle-edit* nil)
(defparameter *turtle-wnd* nil)
(defparameter *turtle-button* nil)
(defparameter *turtle-reset* nil)

(defun turtle-label-text ()
  (format nil "X ~,3F Y ~,3F THETA ~,3F ~A"
          (turtle-x *turtle*) (turtle-y *turtle*)
          (turtle-theta *turtle*)
          (if (turtle-pen-p *turtle*) "Down" "Up")))

(defun turtle-create (hwnd)
  (destructuring-bind (&key left top right bottom) (get-client-rect hwnd)
    (declare (ignore left top))

    (setf *turtle* (make-turtle :x 0 :y 0 :theta 0))
          
    ;; put static label for turtle info in top left
    (setf *turtle-label*
          (create-window :static
                         :window-name (turtle-label-text)
                         :styles (logior-consts +ws-visible+ +ws-child+)
                         :x 25 :y 25 :width (- right 275) :height 25
                         :parent hwnd))
    
    ;; put turtle window below it 
    (setf *turtle-wnd* 
          (create-window "TURTLE"
                         :styles (logior-consts +ws-visible+ +ws-child+ +ws-border+)
                         :parent hwnd
                         :x 25 :y 75 :width (- right 275) :height (- bottom 100)))
    (unless *turtle-wnd* (ftw::get-last-error))
    
    ;; edit control on right for entering command s
    (setf *turtle-edit*
          (create-window :edit
                         :styles (logior-consts +ws-visible+ +ws-child+ +es-multiline+)
                         :x (- right 225) :y 25 :width 200 :height (- bottom 150)
                         :parent hwnd
                         :menu 1))

    (setf *turtle-button*
          (create-window :button
                         :window-name "Done" 
                         :styles (logior-consts +ws-visible+ +ws-child+)
                         :x (- right 255) :y (- bottom 50) :width 25 :height 25
                         :parent hwnd
                         :menu 2)
          *turtle-reset*
          (create-window :button
                         :window-name "Reset"
                         :styles (logior-consts +ws-visible+ +ws-child+)
                         :x (- right 200) :y (- bottom 50) :width 25 :height 25
                         :parent hwnd
                         :menu 3))))

(defun turtle-resize (hwnd)
  (destructuring-bind (&key left top right bottom) (get-client-rect hwnd)
    (declare (ignore left top))
    (when *turtle-wnd*
      (set-window-pos *turtle-wnd* :top 25 75 (- right 275) (- bottom 100)))
    (when *turtle-edit*
      (set-window-pos *turtle-edit* :top (- right 225) 75 200 (- bottom 150)))
    (when *turtle-button*
      (set-window-pos *turtle-button* :top (- right 185) (- bottom 50) 75 25))
    (when *turtle-reset*
      (set-window-pos *turtle-reset* :top (- right 100) (- bottom 50) 75 25))
    (when *turtle-label*
      (set-window-text *turtle-label* (turtle-label-text)))))

  
(defwndproc turtle-main-wndproc (hwnd msg wparam lparam)
  (switch msg
    ((const +wm-create+)
     (turtle-create hwnd))
    ((const +wm-destroy+)
     (post-quit-message))
    ((const +wm-size+)
     (turtle-resize hwnd))
    ((const +wm-command+)
     (switch (loword wparam)
       (1 ;; edit text box
        nil)
       (2 ;; button
        ;; parse commands
        (let ((commands (parse-turtle-commands (get-window-text *turtle-edit*))))

          (setf (turtle-commands *turtle*) commands)
          (invalidate-rect *turtle-wnd* nil t)
          
          ;; clear trext box
          (set-window-text *turtle-edit* "")))
       (3 ;; reset button
        (destructuring-bind (&key left top right bottom) (get-client-rect hwnd)
          (declare (ignore left top))
          (setf *turtle* (make-turtle :x (truncate right 2) :y (truncate bottom 2) :theta 0 :pen-p t)))
        (invalidate-rect *turtle-wnd* nil t)))))
             
  (default-window-proc hwnd msg wparam lparam))

(defun turtle ()
  (default-message-loop (callback turtle-main-wndproc)
      :class-name "TURTLE_WINDOW"
      :title "Turtle"
      :width 600 :height 400))




          
