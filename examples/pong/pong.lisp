
(defpackage #:ftw.pong
  (:use #:cl #:cffi #:ftw))

(in-package #:ftw.pong)

;;; This file should define a simple pong type game.
;;; It needs to do the following:
;;; - run a timer, on each tick update the screen i.e. repaint
;;; - intercept keystrokes to for up and down keys

(defparameter *timestep* 1.0)
(defparameter *friction* -0.01)
(defparameter *pad-height* 0.2)
(defparameter *pad-width* 0.05)
(defparameter *pad-height-phys* 50)
(defparameter *pad-width-phys* 25)
(defparameter *ball-width* 0.05)
(defparameter *ball-width-phys* 15)

(defstruct pos
  (x 0)
  (y 0)
  (vx 0)
  (vy 0)
  (ax *friction*)
  (ay *friction*))

;; x'' = a
;; x' = at (v0 = 0)
;; x = at^2/2 (x0 = 0)
(defun update-pos (p)
  (incf (pos-x p) (* *timestep* (pos-vx p)))
  (incf (pos-y p) (* *timestep* (pos-vy p)))
  (incf (pos-vx p) (* *timestep* (pos-vx p) (pos-ax p)))
  (incf (pos-vy p) (* *timestep* (pos-vy p) (pos-ay p))))
  

(defparameter *phys-x* 300)
(defparameter *phys-y* 300)
(defparameter *log-x* 1.0)
(defparameter *log-y* 1.0)

(defun pos-physical (p) 
  (make-pos :x (truncate (* (pos-x p) (/ *phys-x* *log-x*)))
	    :y (truncate (* (pos-y p) (/ *phys-y* *log-y*)))
	    :vx (truncate (* (pos-vx p) (/ *phys-x* *log-x*)))
	    :vy (truncate (* (pos-vy p) (/ *phys-y* *log-y*)))))


(defparameter *p1* (make-pos :x 0.1 :y 0.5 :vx 0 :vy 0 :ax *friction* :ay *friction*))
(defparameter *p2* (make-pos :x 0.8 :y 0.5 :vx 0 :vy 0 :ax *friction* :ay *friction*))
(defparameter *ball* (make-pos :x 0.5 :y 0.5 :vx 0.01 :y 0 :ax 0 :ay 0))

(defun reset-game ()
  (setf *p1* (make-pos :x 0.1 :y 0.5 :vx 0 :vy 0 :ax *friction* :ay *friction*)
	*p2* (make-pos :x 0.9 :y 0.5 :vx 0 :vy 0 :ax *friction* :ay *friction*)
	*ball* (make-pos :x 0.5 :y 0.5 :vx 0.01 :y 0 :ax 0 :ay 0)))

(defun update-game ()
  (let ((items (list *p1* *p2* *ball*)))
    ;; update all positions
    (dolist (item items)
      (update-pos item))

    (when (< (pos-y *p2*) (pos-y *ball*))
      (setf (pos-vy *p2*) 0.005))
    (when (> (pos-y *p2*) (pos-y *ball*))
      (setf (pos-vy *p2*) -0.005))
    
    ;; detect collisions -- disallow p1 and p2 from going outside screen
    ;; and invert velicity of ball if contacts p1 or p2
    (dolist (p (list *p1* *p2*))
      (when (> (+ (pos-y p) *pad-height*) *log-y*)
	(setf (pos-y p) (- *log-y* *pad-height*)
	      (pos-vy p) 0))
      (when (< (pos-y p) 0)
	(setf (pos-y p) 0
	      (pos-vy p) 0))

      (when (and (>= (+ (pos-x *ball*) *ball-width*)
		     (pos-x p))
		 (<= (pos-x *ball*) 
		     (+ (pos-x p) *pad-width*))
		 (>= (+ (pos-y *ball*) *ball-width*)
		     (pos-y p))
		 (<= (pos-y *ball*)
		     (+ (pos-y p) *pad-height*)))

        (let* ((dy (- (/ (- (+ (pos-y p) *pad-height*)
                            (+ (pos-y *ball*) *ball-width*))
                         *pad-height*)
                      0.5))
               (fx (cos dy))
               (fy (sin dy))
               (f (sqrt (+ (* fx fx) (* fy fy)))))
          ;; adjust velocities
          ;; FIXME: this needs touching up because the mechanics aren't right,
          ;; ball behaves strangely 
          (setf (pos-vx *ball*)
                (- (* (pos-vx *ball*) (/ fx f)))
                (pos-x *ball*)
                (if (< (pos-x *ball*) 0.5)
                    (+ (pos-x p) *ball-width*)
                    (- (pos-x p) *ball-width*))
                                    
                (pos-vy *ball*)
                (+ (pos-vy *ball*)
                   (* (cond
                        ((> (pos-y p) 0.6) -1)
                        ((< (pos-y p) 0.4) -1)
                        (t 1))
                      (/ fy f)
                      (sqrt (+ (* (pos-vx *ball*) (pos-vx *ball*))
                               (* (pos-vy *ball*) (pos-vy *ball*))))))))))

    (when (or (> (pos-x *ball*) *log-x*)
	      (< (pos-x *ball*) 0)
	      (> (pos-y *ball*) *log-y*)
	      (< (pos-y *ball*) 0))
      (reset-game))))



(defun pong-create (hwnd)
  ;; initialize the client area ... we don't have any extra controls just yet 
  ;; we could add a static for player scores ... but not done that yet
  
  ;; set the timer to start ticking 
  (set-timer :hwnd hwnd :elapse 1 :replace-timer 1)
  
  nil)

(defun pong-paint (hwnd)
  ;; repaint the client area
  (with-paint (hwnd hdc)
    ;; paint rectangles, line and ball
    (let* ((black (get-stock-object :black-brush))
	   (white (get-stock-object :white-brush))
	   (hold-brush (select-object hdc black)))
      (select-object hdc white)

      (let ((p (pos-physical *p1*)))
	(rectangle hdc (pos-x p) (pos-y p)
		   (+ (pos-x p) *pad-width-phys*)
		   (+ (pos-y p) *pad-height-phys*)))
      (let ((p (pos-physical *p2*)))
	(rectangle hdc (pos-x p) (pos-y p)
		   (+ (pos-x p) *pad-width-phys*)
		   (+ (pos-y p) *pad-height-phys*)))

      (let* ((pen (get-stock-object :white-pen))
	     (hold-pen (select-object hdc pen)))
	(move-to hdc (truncate *phys-x* 2) 0)
	(line-to hdc (truncate *phys-x* 2) *phys-y*)

	(move-to hdc 0 0)
	(line-to hdc *phys-x* 0)
	(line-to hdc *phys-x* *phys-y*)
	(line-to hdc 0 *phys-y*)
	(line-to hdc 0 0)

	(select-object hdc hold-pen))
      
      (let ((p (pos-physical *ball*)))
	(ellipse hdc (pos-x p) (pos-y p)
		 (+ (pos-x p) *ball-width-phys*)
		 (+ (pos-y p) *ball-width-phys*)))
      
      (select-object hdc hold-brush))))

(defun pong-timer (hwnd)
  (update-game)  
  (invalidate-rect hwnd nil t))

(defun pong-keydown (hwnd wparam)
  (let ((key (virtual-code-key wparam)))
    (case key
      (:up (setf (pos-vy *p1*) -0.005))
      (:down (setf (pos-vy *p1*) 0.005))
      (:keyr ;; reset game
       (reset-game))
      (:keyq ;; quit
       (destroy-window hwnd))
      (:keyh ;; help
       (message-box :hwnd hwnd
                    :text "
Simple pong game. 
Up/down       move paddle
R             reset game
Q             quit
"
                    :caption "Help"
                    :icon :information)))))

(defwndproc pong-wndproc (hwnd msg wparam lparam)
  (switch msg
    ((const +wm-create+)
     (pong-create hwnd))
    ((const +wm-paint+)
     (pong-paint hwnd))
    ((const +wm-timer+)
     (pong-timer hwnd))
    ((const +wm-keydown+)
     (pong-keydown hwnd wparam))
    ((const +wm-destroy+)
     (post-quit-message)))
  (default-window-proc hwnd msg wparam lparam))


    
(defun pong ()
  (register-class "PONG" (callback pong-wndproc)
                  :cursor (load-cursor :arrow)
                  :background (get-stock-object :black-brush))
  (let ((hwnd (create-window "PONG"
                             :window-name "Pong"
                             :styles (logior-consts +ws-overlappedwindow+ +ws-visible+)
                             :x 100 :y 100 :width 400 :height 300))
        (msg (make-msg)))
    (show-window hwnd)
    (update-window hwnd)
    (do ((done nil))
        (done)
      (let ((r (get-message msg)))
        (cond
          ((zerop r) (setf done t))
          (t
           (translate-message msg)
           (dispatch-message msg)))))))
