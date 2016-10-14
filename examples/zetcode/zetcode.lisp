
(defpackage #:ftw.zetcode
  (:use #:cl #:cffi #:ftw)
  (:export #:simplewindow
           #:centering
           #:hotkey
           #:morewindows
           #:escapekey
           #:moving
           #:flashing
           #:simplemenu
           #:popupmenu
           #:checkmenuitem
           #:radiomenuitem
           #:submenu
           #:modaldialog
           #:staticimage
           #:buttons
           #:checkbox
           #:editcontrol
           #:trackbar
           #:tooltip
           #:updown
           #:monthcal
           #:radiobutton
           #:combobox
           #:progressbar
           #:tabcontrol
           #:listbox
           #:burning
           #:pixels
	   #:lines
	   #:zc-rectangle
	   #:zc-bezier
	   #:zc-penstyles
	   #:zc-linejoints
	   #:solidbrush
	   #:hatchbrushes
	   #:custombrush
	   #:shapes
	   #:star
	   #:sonnet
	   #:drawbitmap))
           
          

(in-package #:ftw.zetcode)

;;; This file implements the examples by following the tutorial
;;; in http://zetcode.com/gui/winapi/

(defun zetcode-main (class-name wndproc &key (width 250) (height 180))
  (register-class class-name 
                  wndproc 
                  :background (get-sys-color-brush :3d-face)
                  :cursor (load-cursor :arrow))
  (let ((hwnd (create-window class-name 
                             :window-name class-name 
                             :styles '(:overlapped-window :visible)
                             :x 100 :y 100 :width width :height height))
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
                 (t
                  (translate-message msg)
                  (dispatch-message msg))))))
      (unregister-class class-name))))

;; ---------------------------------------

(defwndproc simplewindow-wndproc (hwnd msg wparam lparam)
  (switch msg
    ((const +wm-destroy+)
     (post-quit-message)))
  (default-window-proc hwnd msg wparam lparam))

(defun simplewindow ()
  (zetcode-main "SIMPLEWINDOW"	       
                (callback simplewindow-wndproc)))

;; ---------------------------------------

(defun center-window (hwnd)
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

(defwndproc centering-wndproc (hwnd msg wparam lparam)
  (switch msg
    ((const +wm-create+)
     (center-window hwnd))
    ((const +wm-destroy+)
     (post-quit-message)))
  (default-window-proc hwnd msg wparam lparam))


(defun centering ()
  (zetcode-main "CENTERING" 
                (callback centering-wndproc)))

;; ---------------------------------------

(defwndproc hotkey-wndproc (hwnd msg wparam lparam)
  (switch msg
    ((const +wm-create+)
     (register-hot-key hwnd 1 #x43 '(:control)))
    ((const +wm-hotkey+)
     (when (= wparam 1)
       (center-window hwnd)))
    ((const +wm-destroy+)
     (unregister-hot-key hwnd 1)
     (post-quit-message)))
  (default-window-proc hwnd msg wparam lparam))

(defun hotkey ()
  (zetcode-main "HOTKEY" 
                (callback hotkey-wndproc)))

;; ---------------------------------------

(defwndproc panel-wndproc (hwnd msg wparam lparam)
  (switch msg
    ((const +wm-lbuttonup+)
     (message-beep :ok)))
  (default-window-proc hwnd msg wparam lparam))

(defun register-red-panel-class ()
  (register-class "REDPANELCLASS"
                  (callback panel-wndproc)
                  :background (create-solid-brush (encode-rgb 255 0 0))
                  :cursor (load-cursor :arrow)))

(defun register-blue-panel-class ()
  (register-class "BLUEPANELCLASS"
                  (callback panel-wndproc)
                  :background (create-solid-brush (encode-rgb 0 0 255))
                  :cursor (load-cursor :arrow)))

(defwndproc morewindows-wndproc (hwnd msg wparam lparam)
  (switch msg
    ((const +wm-create+)
     (register-red-panel-class)
     (create-window "REDPANELCLASS"
                    :styles '(:child :visible)
                    :x 20 :y 20 :width 80 :height 80
                    :parent hwnd
                    :menu (make-pointer 1))
     (register-blue-panel-class)
     (create-window "BLUEPANELCLASS"
                    :styles '(:child :visible)
                    :x 120 :y 20 :width 80 :height 80
                    :parent hwnd
                    :menu (make-pointer 2)))
    ((const +wm-destroy+)
     (post-quit-message)))
  (default-window-proc hwnd msg wparam lparam))

(defun morewindows ()
  (zetcode-main "MOREWINDOWS"
                (callback morewindows-wndproc)))

;; ---------------------------------------


(defwndproc escapekey-wndproc (hwnd msg wparam lparam)
  (switch msg
    ((const +wm-keydown+)
     (when (= wparam (virtual-key-code :escape))
       (let ((ret (message-box :hwnd hwnd
                               :text "Are you sure to quit?"
                               :caption "Message"
                               :button :ok-cancel)))
         (when (eq ret :ok)
           (send-message hwnd (const +wm-close+))))))
    ((const +wm-destroy+)
     (post-quit-message)))
  (default-window-proc hwnd msg wparam lparam))


(defun escapekey ()
  (zetcode-main "ESCAPEKEY" 
                (callback escapekey-wndproc)))

;; ---------------------------------------


(defvar *moving-sta1* nil)
(defvar *moving-sta2* nil)

(defun create-labels (hwnd)
  (create-window :static
                 :window-name "x: " 
                 :styles '(:child :visible)
                 :x 10 :y 10 :width 25 :height 25
                 :parent hwnd
                 :menu (make-pointer 1))
  (setf *moving-sta1*
        (create-window :static
                       :window-name "150" 
                       :styles '(:child :visible)
                       :x 40 :y 10 :width 55 :height 25
                       :parent hwnd
                       :menu (make-pointer 2)))
  (create-window :static
                 :window-name "y: " 
                 :styles '(:child :visible)
                 :x 10 :y 30 :width 25 :height 25
                 :parent hwnd
                 :menu (make-pointer 3))
  (setf *moving-sta2*
        (create-window :static
                       :window-name "150" 
                       :styles '(:child :visible)
                       :x 40 :y 30 :width 55 :height 25
                       :parent hwnd
                       :menu (make-pointer 4))))

(defwndproc moving-wndproc (hwnd msg wparam lparam)
  (switch msg
    ((const +wm-create+)
     (create-labels hwnd))
    ((const +wm-move+)
     (let ((rect (get-window-rect hwnd)))
       (set-window-text *moving-sta1* (format nil "~A" (getf rect :left)))
       (set-window-text *moving-sta2* (format nil "~A" (getf rect :top)))))
    ((const +wm-destroy+)
     (post-quit-message)))
  (default-window-proc hwnd msg wparam lparam))

(defun moving ()
  (zetcode-main "MOVING"
                (callback moving-wndproc)))

;; ---------------------------------------

(defwndproc flashing-wndproc (hwnd msg wparam lparam)
  (switch msg
    ((const +wm-create+)
     (create-window :button
                    :window-name "Flash"
                    :styles '(:child :visible)
                    :x 10 :y 10 :width 80 :height 25
                    :parent hwnd
                    :menu (make-pointer 1)))
    ((const +wm-command+)
     (flash-window hwnd
                   :flags '(:all)
                   :count 4))
    ((const +wm-destroy+)
     (post-quit-message)))
  (default-window-proc hwnd msg wparam lparam))

(defun flashing ()
  (zetcode-main "FLASHING"
                (callback flashing-wndproc)))

;; ---------------------------------------

(defun add-menus (hwnd)
  (let ((menu-bar (create-menu))
        (menu (create-menu)))
    (append-menu menu '(:string) 1 "&New")
    (append-menu menu '(:string) 2 "&Open")
    (append-menu menu '(:separator) 0)
    (append-menu menu '(:string) 3 "&Quit")

    (append-menu menu-bar '(:popup) menu "&File")
    (set-menu hwnd menu-bar)))

(defwndproc simplemenu-wndproc (hwnd msg wparam lparam)
  (switch msg
    ((const +wm-create+)
     (add-menus hwnd))
    ((const +wm-command+)
     (switch (logand wparam #xffff)
       (1
        (message-beep :information))
       (2
        (message-beep :information))
       (3
        (send-message hwnd (const +wm-close+)))))
    ((const +wm-destroy+)
     (post-quit-message)))
  (default-window-proc hwnd msg wparam lparam))

(defun simplemenu ()
  (zetcode-main "SIMPLEMENU"
                (callback simplemenu-wndproc)		
                :width 350 :height 250))


;; ---------------------------------------

(defwndproc popupmenu-wndproc (hwnd msg wparam lparam)
  (switch msg
    ((const +wm-command+)
     (switch (logand wparam #xffff)
       (1
        (message-beep :information))
       (2 (message-beep :information))
       (3
        (send-message hwnd (const +wm-close+)))))
    ((const +wm-rbuttonup+)
     (let* ((menu (create-popup-menu))
            (rect (client-to-screen hwnd
                                    (logand lparam #xffff)
                                    (ash (logand lparam #xffff0000)
                                         -16))))
       (append-menu menu '(:string) 1 "&New")
       (append-menu menu '(:string) 2 "&Open")
       (append-menu menu '(:separator) 0)
       (append-menu menu '(:string) 3 "&Quit")
       
       (track-popup-menu menu
                         (first rect) (second rect)
                         hwnd
                         :flags '(:right-button))
       (destroy-menu menu)))
    ((const +wm-destroy+)
     (post-quit-message)))
  (default-window-proc hwnd msg wparam lparam))

(defun popupmenu ()
  (zetcode-main "POPUPMENU" (callback popupmenu-wndproc)))

;; ---------------------------------------

(defvar *cmi-ghsb* nil)
(defvar *cmi-menu* nil)

(defun add-checkitem-menus (hwnd)
  (let ((bar (create-menu)))
    (setf *cmi-menu* (create-menu))

    (append-menu *cmi-menu* '(:string) 1 "&Statusbar")
    (check-menu-item *cmi-menu* 1 t)
    (append-menu bar '(:popup) *cmi-menu* "&View")
    (set-menu hwnd bar)))

(defwndproc checkmenuitem-wndproc (hwnd msg wparam lparam)
  (switch msg
    ((const +wm-create+)
     (add-checkitem-menus hwnd)
     (init-common-controls)
     (setf *cmi-ghsb*
           (create-window :status
                          :styles '(:child :visible)
                          :parent hwnd
                          :menu (make-pointer 1))))
    ((const +wm-command+)
     (switch (logand wparam #xffff)
       (1
        (let ((state (get-menu-state *cmi-menu* 1)))
          (cond
            ((member :checked state)
             (show-window *cmi-ghsb* :hide)
             (check-menu-item *cmi-menu* 1))
            (t
             (show-window *cmi-ghsb* :show-noactive)
             (check-menu-item *cmi-menu* 1 t)))))))
    ((const +wm-size+)
     (send-message *cmi-ghsb* (const +wm-size+)
                   :wparam wparam
                   :lparam lparam))
    ((const +wm-destroy+)
     (post-quit-message)))
  (default-window-proc hwnd msg wparam lparam))

(defun checkmenuitem ()
  (zetcode-main "CHECKMENUITEM"
                (callback checkmenuitem-wndproc)))


;; ---------------------------------------

(defvar *rmi-menu* nil)

(defun add-rmi-menus (hwnd)
  (let ((bar (create-menu)))
    (setf *rmi-menu* (create-menu))

    (append-menu *rmi-menu* '(:string) 1 "&Map")
    (append-menu *rmi-menu* '(:string) 2 "&Satellite")
    (append-menu *rmi-menu* '(:string) 3 "&Traffic")
    (append-menu *rmi-menu* '(:string) 4 "Street &view")

    (check-menu-radio-item *rmi-menu* 1 4 1)

    (append-menu bar '(:popup) *rmi-menu* "&Map mode")
    (set-menu hwnd bar)))

(defwndproc radiomenuitem-wndproc (hwnd msg wparam lparam)
  (switch msg
    ((const +wm-create+)
     (add-rmi-menus hwnd))
    ((const +wm-command+)
     (switch (logand wparam #xffff)
       (1
        (check-menu-radio-item *rmi-menu* 1 4 1)
        (message-beep :error))
       (2
        (check-menu-radio-item *rmi-menu* 1 4 2)
        (message-beep))
       (3
        (check-menu-radio-item *rmi-menu* 1 4 3)
        (message-beep :warning))
       (4
        (check-menu-radio-item *rmi-menu* 1 4 4)
        (message-beep :information))))
    ((const +wm-destroy+)
     (post-quit-message)))
  (default-window-proc hwnd msg wparam lparam))


(defun radiomenuitem ()
  (zetcode-main "RADIOMENUITEM"
                (callback radiomenuitem-wndproc)))


;; ---------------------------------------

(defun add-sub-menus (hwnd)
  (let ((bar (create-menu))
        (menu (create-menu))
        (submenu (create-popup-menu)))
    (append-menu menu '(:string) 1 "&New")
    (append-menu menu '(:string :popup) submenu "&Import")
    (append-menu submenu '(:string) 2 "Import &mail")
    (append-menu bar '(:popup) menu "&File")
    (set-menu hwnd bar)))

(defwndproc submenu-wndproc (hwnd msg wparam lparam)
  (switch msg
    ((const +wm-create+)
     (add-sub-menus hwnd))
    ((const +wm-command+)
     (switch (logand wparam #xffff)
       (1
        (message-box :text "New file selected" :caption "Information"))
       (2
        (message-box :text "Import mail selected" :caption "Information"))))
    ((const +wm-destroy+)
     (post-quit-message)))
  (default-window-proc hwnd msg wparam lparam))

(defun submenu ()
  (zetcode-main "SUBMENU" (callback submenu-wndproc)))

;; ---------------------------------------

(defwndproc modaldialog-wndproc (hwnd msg wparam lparam)
  (switch msg 
    ((const +wm-create+)
     (register-class "MODALDIALOGCLASS"
                     (callback modaldialogclass-wndproc)
                     :background (get-sys-color-brush :3d-face))
     (create-window :button
                    :window-name "Show dialog"
                    :styles '(:visible :child)
                    :x 20 :y 50 :width 95 :height 25
                    :parent hwnd
                    :menu (make-pointer 1)))
    ((const +wm-command+)
     (create-window "MODALDIALOGCLASS"
                    :window-name "Dialog box"
                    :ex-styles '(:dialog-modal-frame :topmost)
                    :styles '(:visible :sysmenu :caption)
                    :x 100 :y 100 :width 200 :height 150
                    :parent hwnd))
    ((const +wm-destroy+)
     (post-quit-message)))
  (default-window-proc hwnd msg wparam lparam))


(defwndproc modaldialogclass-wndproc (hwnd msg wparam lparam)
  (switch msg
    ((const +wm-create+)
     (create-window :button
                    :window-name "Ok"
                    :styles '(:visible :child)
                    :x 50 :y 50 :width 80 :height 25
                    :menu (make-pointer 1)
                    :parent hwnd))
    ((const +wm-command+)
     (destroy-window hwnd))
    ((const +wm-close+)
     (destroy-window hwnd)))
  (default-window-proc hwnd msg wparam lparam))

(defun modaldialog ()
  (zetcode-main "MODALDIALOG"
                (callback modaldialog-wndproc)))


;; ---------------------------------------

(defwndproc statictext-wndproc (hwnd msg wparam lparam)
  (switch msg
    ((const +wm-create+)
     (create-window :static
                    :window-name "I know you told me I should stay away
I know you said he's just a dog astray
He is a bad boy with a tainted heart
And even I know this ain't smart

But mama, I'm in love with a criminal
And this type of love isn't rational, it's physical
Mama, please don't cry, I will be alright
All reason aside, I just can't deny, love the guy.
"
                    :styles '(:child :visible)
                    :x 20 :y 20 :width 300 :height 230
                    :parent hwnd
                    :menu (make-pointer 1)))
    ((const +wm-destroy+)
     (post-quit-message)))
  (default-window-proc hwnd msg wparam lparam))

(defun static-text ()
  (zetcode-main "STATICTEXT"
                (callback statictext-wndproc)))

;; ---------------------------------------

(defvar *staticimage* nil)

(defwndproc staticimage-wndproc (hwnd msg wparam lparam)
  (switch msg
    ((const +wm-create+)
     (setf *staticimage*
           (load-image "C:\\Users\\fjames\\fred.bmp" 
                       :type :bitmap 
                       :flags '(:load-from-file)))
     (let ((hsti (create-window :static
                                :styles '(:child :visible :ss-bitmap)
                                :x 5 :y 5 :width 300 :height 300
                                :menu (make-pointer 1)
                                :parent hwnd)))
       (send-message hsti (const +stm-setimage+)
                     :wparam 0
                     :lparam (pointer-address *staticimage*))))
    ((const +wm-destroy+)
     (delete-object *staticimage*)
     (post-quit-message)))
  (default-window-proc hwnd msg wparam lparam))

(defun staticimage ()
  (zetcode-main "STATICIMAGE"
                (callback staticimage-wndproc)))

;; ----------------------------------------

(defwndproc buttons-wndproc (hwnd msg wparam lparam)
  (switch msg
    ((const +wm-create+)
     (create-window :button
                    :window-name "Beep"
                    :styles '(:visible :child)
                    :x 20 :y 50 :width 80 :height 25
                    :parent hwnd
                    :menu (make-pointer 1))
     (create-window :button
                    :window-name "Quit"
                    :styles '(:visible :child)
                    :x 120 :y 50 :width 80 :height 25
                    :parent hwnd
                    :menu (make-pointer 2)))
    ((const +wm-command+)
     (when (= (logand wparam #xffff) 1)
       (message-beep :ok))
     (when (= (logand wparam #xffff) 2)
       (destroy-window hwnd)))
    ((const +wm-destroy+)
     (post-quit-message)))
  (default-window-proc hwnd msg wparam lparam))


(defun buttons ()
  (zetcode-main "BUTTONS"
                (callback buttons-wndproc)))

;; --------------------------------------

(defwndproc checkbox-wndproc (hwnd msg wparam lparam)
  (switch msg
    ((const +wm-create+)
     (create-window :button
                    :window-name "Show title"
                    :styles '(:visible :child :bs-checkbox)
                    :x 20 :y 20 :width 185 :height 35
                    :parent hwnd
                    :menu (make-pointer 1))
     (check-dialog-button hwnd 1 :checked))
    ((const +wm-command+)
     (let ((checked (is-dialog-button-checked hwnd 1)))
       (cond
         ((eq checked :unchecked)
          (check-dialog-button hwnd 1 :checked)
          (set-window-text hwnd "Check box"))
         (t
          (check-dialog-button hwnd 1 :unchecked)
          (set-window-text hwnd "")))))
    ((const +wm-destroy+)
     (post-quit-message)))
  (default-window-proc hwnd msg wparam lparam))

(defun checkbox ()
  (zetcode-main "CHECKBOXCLASS"
                (callback checkbox-wndproc)))

;; -------------------------------

(defvar *hwnd-edit* nil)

(defwndproc editcontrol-wndproc (hwnd msg wparam lparam)
  (switch msg
    ((const +wm-create+)
     (setf *hwnd-edit*
           (create-window :edit
                          :styles '(:child :visible :border)
                          :x 50 :y 50 :width 150 :height 20
                          :parent hwnd
                          :menu (make-pointer 1)))
     (create-window :button
                    :window-name "Set title" 
                    :styles '(:visible :child)
                    :x 50 :y 100 :width 80 :height 25
                    :parent hwnd
                    :menu (make-pointer 2)))
    ((const +wm-command+)
     (when (= (ash (logand wparam #xffff0000) -16) (const +bn-clicked+))
       (let ((text (get-window-text *hwnd-edit*)))
         (set-window-text hwnd text))))
    ((const +wm-destroy+)
     (post-quit-message)))
  (default-window-proc hwnd msg wparam lparam))

(defun editcontrol ()
  (zetcode-main "EDITCONTROL" (callback editcontrol-wndproc)))

;; -----------------------------

(defvar *tb-track* nil)
(defvar *tb-lbl* nil)

(defun create-trackbar-controls (hwnd)
  (let ((left-label (create-window :static
                                   :window-name "0"
                                   :styles '(:child :visible)
                                   :x 0 :y 0 :width 10 :height 30
                                   :parent hwnd
                                   :menu (make-pointer 1)))
        (right-label (create-window :static
                                    :window-name "100"
                                    :styles '(:child :visible)
                                    :x 0 :y 0 :width 30 :height 30
                                    :parent hwnd
                                    :menu (make-pointer 2))))
    (setf *tb-lbl* (create-window :static
                                  :window-name "0"
                                  :styles '(:child :visible)
                                  :x 270 :y 20 :width 30 :height 30
                                  :parent hwnd
                                  :menu (make-pointer 3)))

    (init-common-controls '(:listview))
    
    (setf *tb-track* (create-window "msctls_trackbar32"
                                    :window-name "Trackbar Control"
                                    :styles '(:child :visible :tbs-autoticks)
                                    :x 20 :y 20 :width 170 :height 30
                                    :parent hwnd
                                    :menu (make-pointer 3)))

    (send-message *tb-track* (const +tbm-setrange+)
                  :wparam 1
                  :lparam (ash 100 16))
    (send-message *tb-track* (const +tbm-setpagesize+)
                  :lparam 10)
    (send-message *tb-track* (const +tbm-setticfreq+)
                  :wparam 10)
    (send-message *tb-track* (const +tbm-setpos+))
    (send-message *tb-track* (const +tbm-setbuddy+)
                  :wparam 1
                  :lparam (pointer-address left-label))
    (send-message *tb-track* (const +tbm-setbuddy+)
                  :wparam 0
                  :lparam (pointer-address right-label))))

(defun update-trackbar-label ()
  (let ((pos (send-message *tb-track* (const +tbm-getpos+))))
    (set-window-text *tb-lbl* (format nil "~A" pos))))



(defwndproc trackbar-wndproc (hwnd msg wparam lparam)
  (switch msg
    ((const +wm-create+) (create-trackbar-controls hwnd))
    ((const +wm-hscroll+) (update-trackbar-label))
    ((const +wm-destroy+) (post-quit-message)))
  (default-window-proc hwnd msg wparam lparam))

(defun trackbar ()
  (zetcode-main "TRACKBARCLASS"
                (callback trackbar-wndproc)))


;; ----------------------------------------

(defwndproc tooltip-wndproc (hwnd msg wparam lparam)
  (switch msg
    ((const +wm-create+)
     (init-common-controls '(:win95))
     (let ((hwndtt (create-window (const +tooltips-class+)
                                  :ex-styles (const +ws-ex-topmost+)
                                  :styles (logior-consts +ws-popup+ +tts-noprefix+ +tts-alwaystip+)
                                  :parent hwnd)))
       (set-window-pos hwndtt :topmost 0 0 0 0
                       (logior-consts +swp-nomove+ +swp-nosize+ +swp-noactivate+))
       (with-tool-info (ti hwnd "A main window" (get-window-rect hwnd)
                           :flags (const +ttf-subclass+))
         (send-message hwndtt (const +ttm-addtool+)
                       :lparam (pointer-address ti)))))
    ((const +wm-destroy+)
     (post-quit-message)))
  (default-window-proc hwnd msg wparam lparam))

(defun tooltip ()
  (zetcode-main "ZETCODE_TOOLTIP"
                (callback tooltip-wndproc)))

;; ------------------------------------


(defvar *ud-static* nil)

(defwndproc updown-wndproc (hwnd msg wparam lparam)
  (switch msg
    ((const +wm-create+)
     (init-common-controls '(:updown))
     (let ((updown (create-window (const +updown-class+)
                                  :styles (logior-consts +ws-child+ +ws-visible+
                                                         +uds-setbuddyint+ +uds-alignright+)
                                  :parent hwnd
                                  :menu 1))
           (edit (create-window :edit
                                :ex-styles (logior-consts +ws-ex-clientedge+)
                                :styles (logior-consts +ws-child+ +ws-visible+ +es-right+)
                                :x 15 :y 15 :width 70 :height 25
                                :parent hwnd
                                :menu 2)))
       (setf *ud-static* (create-window :static
                                        :window-name "0"
                                        :styles (logior-consts +ws-child+ +ws-visible+ +ss-left+)
                                        :x 15 :y 60 :width 300 :height 230
                                        :parent hwnd
                                        :menu 3))
       (send-message updown (const +udm-setbuddy+) :wparam edit)
       (send-message updown (const +udm-setrange+) :lparam (make-lparam 30 0))
       (send-message updown (const +udm-setpos32+))))
    ((const +wm-notify+)
     (let ((nmhdr (foreign-nmhdr (make-pointer lparam))))
       (when (= (getf nmhdr :code) (const +udn-deltapos+))
         (let* ((updown (foreign-updown (make-pointer lparam)))
                (value (+ (getf updown :pos) (getf updown :delta))))
           (when (< value 0) (setf value 0))
           (when (> value 30) (setf value 30))

           (set-window-text *ud-static* (format nil "~A" value))))))
    ((const +wm-destroy+)
     (post-quit-message)))
  (default-window-proc hwnd msg wparam lparam))

(defun updown ()
  (zetcode-main "ZETCODE_UPDOWN" (callback updown-wndproc)))

;; -----------------------------

(defvar *mc-static* nil)
(defvar *monthcal* nil)

(defwndproc monthcal-wndproc (hwnd msg wparam lparam)
  (switch msg
    ((const +wm-create+)
     (setf *mc-static* (create-window :static
                                      :window-name ""
                                      :styles (logior-consts +ws-child+ +ws-visible+)
                                      :x 80 :y 240 :width 80 :height 30
                                      :parent hwnd
                                      :menu 1))
     (init-common-controls '(:date))
     (setf *monthcal* (create-window (const +monthcal-class+)
                                     :window-name ""
                                     :styles (logior-consts +ws-border+
                                                            +ws-child+
                                                            +ws-visible+
                                                            +mcs-notodaycircle+)
                                     :x 20 :y 20 :width 200 :height 200
                                     :parent hwnd
                                     :menu 2)))
    ((const +wm-notify+)
     (let ((nmhdr (foreign-nmhdr (make-pointer lparam))))
       (when (= (getf nmhdr :code) (const +mcn-select+))
         (with-foreign-object (st '(:struct systemtime))
           (send-message *monthcal* (const +mcm-getcursel+) :lparam st)
           (let ((ss (foreign-systemtime st)))
             (set-window-text *mc-static*
                              (format nil "~A-~A-~A"
                                      (systemtime-year ss)
                                      (systemtime-month ss)
                                      (systemtime-day ss))))))))
    ((const +wm-destroy+)
     (post-quit-message)))
  (default-window-proc hwnd msg wparam lparam))

(defun monthcal ()
  (zetcode-main "ZETCODE_MONTHCAL" (callback monthcal-wndproc)
                :width 400 :height 400))



;; ----------------------------

(defparameter *radio-color* (encode-rgb 255 0 0))

(defwndproc radio-wndproc (hwnd msg wparam lparam)
  (switch msg
    ((const +wm-create+)
     (create-window :button
                    :window-name "Choose colour"
                    :styles (logior-consts +ws-child+ +ws-visible+ +bs-groupbox+)
                    :x 10 :y 10 :width 120 :height 110
                    :parent hwnd
                    :menu 0)
     (dolist (binfo '(("Blue" 1 30) ("Yellow" 2 55) ("Orange" 3 80)))
       (create-window :button
                      :window-name (first binfo)
                      :styles (logior-consts +ws-child+ +ws-visible+ +bs-autoradiobutton+)
                      :x 20 :y (third binfo) :width 100 :height 30
                      :parent hwnd
                      :menu (second binfo))))
    ((const +wm-command+)
     (when (= (ash (logand wparam #xffff0000) -16) (const +bn-clicked+))
       (setf *radio-color* 
             (switch (logand wparam #xffff)
               (1 (encode-rgb 0 76 255))
               (2 (encode-rgb 255 255 0))
               (3 (encode-rgb 255 123 0))))
       (invalidate-rect hwnd nil t)))
    ((const +wm-paint+)
     (multiple-value-bind (hdc ps) (begin-paint hwnd)
       (let* ((brush (create-solid-brush *radio-color*))
              (pen (create-pen :null (encode-rgb 0 0 0) 1))
              (hold-pen (select-object hdc pen))
              (hold-brush (select-object hdc brush)))
         (rectangle hdc 160 20 260 120)

         (select-object hdc hold-brush)
         (select-object hdc hold-pen)
         (delete-object pen)
         (delete-object brush)
         
         (end-paint hdc ps))))
    ((const +wm-destroy+)
     (post-quit-message)))
  (default-window-proc hwnd msg wparam lparam))


(defun radiobutton ()
  (zetcode-main "ZETCODE_RADIO" (callback radio-wndproc)))

;; -----------------------

(defvar *combobox* nil)
(defvar *combobox-static* nil)
(defvar *combobox-strings* '("FreeBSD" "OpenBSD" "NetBSD" "Solaris" "Arch"))

(defwndproc combobox-wndproc (hwnd msg wparam lparam)
  (switch msg
    ((const +wm-create+)
     (setf *combobox* (create-window :combobox
                                     :styles (logior-consts +ws-child+ +ws-visible+ +cbs-dropdown+)
                                     :x 10 :y 10 :width 120 :height 110
                                     :parent hwnd)
           *combobox-static* 
           (create-window :button
                          :window-name "Drop down"
                          :styles (logior-consts +ws-child+ +ws-visible+)
                          :x 150 :y 80 :width 90 :height 25
                          :parent hwnd))
     (dolist (str *combobox-strings*)
       (with-wide-string (s str)
         (send-message *combobox* (const +cb-addstring+) :lparam s))))
    ((const +wm-command+)
     (when (= (ash (logand wparam #xffff0000) -16) (const +bn-clicked+))
       (send-message *combobox* (const +cb-showdropdown+) :wparam 1))
     (when (= (ash (logand wparam #xffff0000) -16) (const +cbn-selchange+))
       (let ((sel (send-message *combobox* (const +cb-getcursel+))))
         (set-window-text *combobox-static* (nth sel *combobox-strings*)))))
    ((const +wm-destroy+)
     (post-quit-message)))
  (default-window-proc hwnd msg wparam lparam))

(defun combobox ()
  (zetcode-main "ZETCODE_COMBOBOX" (callback combobox-wndproc)))


;; -----------------------------------------

(defvar *progress* nil)
(defvar *progress-button* nil)
(defvar *progress-counter* 0)

(defwndproc progress-wndproc (hwnd msg wparam lparam)
  (switch msg
    ((const +wm-create+)
     (setf *progress-counter* 0)
     (init-common-controls :progress)
     (setf *progress* (create-window (const +progress-class+)
                                     :styles (logior-consts +ws-child+ +ws-visible+ +pbs-smooth+)
                                     :x 30 :y 20 :width 190 :height 25
                                     :parent hwnd)
           *progress-button* (create-window :button
                                            :window-name "Start"
                                            :styles (logior-consts +ws-child+ +ws-visible+)
                                            :x 85 :y 90 :width 85 :height 25
                                            :parent hwnd
                                            :menu 1))
     (send-message *progress* (const +pbm-setrange+) :lparam (make-lparam 0 150))
     (send-message *progress* (const +pbm-setstep+) :wparam 1))
    ((const +wm-timer+)
     (send-message *progress* (const +pbm-stepit+))
     (incf *progress-counter*)
     (when (= *progress-counter* 150)
       (kill-timer 2 hwnd)
       (with-wide-string (s "Start")
         (send-message *progress-button* (const +wm-settext+) :lparam s))
       (setf *progress-counter* 0)))
    ((const +wm-command+)
     (when (zerop *progress-counter*)
       (setf *progress-counter* 1)
       (send-message *progress* (const +pbm-setpos+))
       (set-timer :hwnd hwnd :replace-timer 2 :elapse 5)
       (with-wide-string (s "In progress")
         (send-message *progress-button* (const +wm-settext+) :lparam s))))
    ((const +wm-destroy+)
     (kill-timer 2 hwnd)
     (post-quit-message)))
  (default-window-proc hwnd msg wparam lparam))


(defun progressbar ()
  (zetcode-main "ZETCODE_PROGRESS" (callback progress-wndproc)))

;; --------------------------------------

(defvar *tabcontrol* nil)
(defvar *tabcontrol-edit* nil)

(defwndproc tabcontrol-wndproc (hwnd msg wparam lparam)
  (switch msg
    ((const +wm-create+)
     (init-common-controls :tab)
     (setf *tabcontrol* (create-window (const +wc-tabcontrol+)
                                       :styles (logior-consts +ws-child+ +ws-visible+)
                                       :x 0 :y 0 :width 200 :height 150
                                       :parent hwnd
                                       :menu 1)
           *tabcontrol-edit* (create-window :edit
                                            :styles (logior-consts +ws-child+ +ws-visible+ +ws-border+)
                                            :x 250 :y 20 :width 100 :height 25
                                            :parent hwnd
                                            :menu 2))
     (send-message *tabcontrol-edit* (const +em-setlimittext+) :wparam 15)
     (create-window :button
                    :window-name "Add"
                    :styles (logior-consts +ws-child+ +ws-visible+ +bs-pushbutton+)
                    :x 250 :y 50 :width 100 :height 25
                    :parent hwnd
                    :menu 3)
     (create-window :button
                    :window-name "Delete"
                    :styles (logior-consts +ws-child+ +ws-visible+ +bs-pushbutton+)
                    :x 250 :y 80 :width 100 :height 25
                    :parent hwnd
                    :menu 4)
     (create-window :button
                    :window-name "Clear"
                    :styles (logior-consts +ws-child+ +ws-visible+ +bs-pushbutton+)
                    :x 250 :y 110 :width 100 :height 25
                    :parent hwnd
                    :menu 5))    
    ((const +wm-command+)
     (switch (logand wparam #xffff)
       (3 ;; add 
        (let ((txt (get-window-text *tabcontrol-edit*)))
          (unless (zerop (length txt))
            (let ((count (send-message *tabcontrol* (const +tcm-getitemcount+))))
              (with-foreign-object (tie '(:struct tcitem))
                (with-wide-string (ts txt)
                  (tcitem-foreign tie :mask (const +tcif-text+) :text ts)
                  (send-message *tabcontrol* (const +tcm-insertitem+)
                                :wparam count
                                :lparam tie)))))))
       (4 ;; delete
        (let ((id (send-message *tabcontrol* (const +tcm-getcursel+))))
          (unless (= id -1)
            (send-message *tabcontrol* (const +tcm-deleteitem+) :lparam id))))
       (5 ;; clear
        (send-message *tabcontrol* (const +tcm-deleteallitems+)))))
    ((const +wm-destroy+)
     (post-quit-message)))
  (default-window-proc hwnd msg wparam lparam))

(defun tabcontrol ()
  (zetcode-main "ZETCODE_TABCONTROL" (callback tabcontrol-wndproc)))

;; -----------------------------------

(defvar *listbox* nil)
(defvar *listbox-static* nil)
(defvar *listbox-friends*
  '(("Lucy" "waitress" 18)
    ("Thomas" "programmer" 25)
    ("George" "policer officer" 26)
    ("Michael" "producer" 38)
    ("Jane" "steward" 28)))

(defwndproc listbox-wndproc (hwnd msg wparam lparam)
  (switch msg
    ((const +wm-create+)
     (setf *listbox* (create-window :listbox
                                    :styles (logior-consts +ws-child+ +ws-visible+ +lbs-notify+)
                                    :x 10 :y 10 :width 150 :height 120
                                    :parent hwnd
                                    :menu 1)
           *listbox-static* (create-window :static
                                           :styles (logior-consts +ws-child+ +ws-visible+)
                                           :x 200 :y 10 :width 120 :height 45
                                           :parent hwnd
                                           :menu 2))
     (dolist (f *listbox-friends*)
       (with-wide-string (s (first f))
         (send-message *listbox* (const +lb-addstring+) :lparam s))))
    ((const +wm-command+)
     (when (= (logand wparam #xffff) 1)
       (when (= (ash (logand wparam #xffff0000) -16) (const +lbn-selchange+))
         (let ((sel (send-message *listbox* (const +lb-getcursel+))))
           (set-window-text *listbox-static*
                            (format nil "Job: ~A~%Age: ~A"
                                    (second (nth sel *listbox-friends*))
                                    (third (nth sel *listbox-friends*))))))))
    ((const +wm-destroy+)
     (post-quit-message)))
  (default-window-proc hwnd msg wparam lparam))

(defun listbox ()
  (zetcode-main "ZETCODE_LISTBOX" (callback listbox-wndproc)))


;; ----------------------------------

(defvar *burning* nil)
(defvar *burning-track* nil)
(defvar *burning-pos* 0)
(defvar *burning-cap* '("75" "150" "225" "300" "375" "450" "525" "600" "675"))

(defwndproc burningpanel-wndproc (hwnd msg wparam lparam)
  (switch msg
    ((const +wm-paint+)
     (multiple-value-bind (hdc ps) (begin-paint hwnd)
       (let* ((rect (get-client-rect hwnd))
              (till (* (truncate (getf rect :right) 750) *burning-pos*))
              (step (truncate (getf rect :right) 10))
              (full (* (truncate (getf rect :right) 750) 700))
              (brush-yellow (create-solid-brush (encode-rgb 255 255 184)))
              (brush-red (create-solid-brush (encode-rgb 255 110 110)))
              (pen (create-pen :null (encode-rgb 0 0 0) 1))
              (hold-pen (select-object hdc pen))
              (font (create-font "Tahoma"
                                 :height 13
                                 :weight (const +fw-medium+)))
              (hold-font (select-object hdc font))
              (hold-brush nil))
         (cond
           ((> till full)
            (select-object hdc brush-yellow)
            (rectangle hdc 0 0 full 30)
            (setf hold-brush (select-object hdc brush-red))
            (rectangle hdc full 0 till 30))
           (t
            (setf hold-brush (select-object hdc brush-yellow))
            (rectangle hdc 0 0 till 30)))
         (select-object hdc hold-pen)

         (do ((i 1 (1+ i)))
             ((= i 10))	   
           (move-to hdc (* i step) 0)
           (line-to hdc (* i step) 7)
           (set-bk-mode hdc :transparent)
           (let ((rect2 (make-rect :left (- (* i step) 10)
                                   :right (+ (* i step) 10)
                                   :bottom 28
                                   :top 8)))
             (draw-text hdc (nth (1- i) *burning-cap*) rect2 '(:center))))

         (select-object hdc hold-brush)
         (delete-object brush-yellow)
         (delete-object brush-red)
         (delete-object pen)
         (select-object hdc hold-font)
         (delete-object font)
         
         (end-paint hdc ps)))))
  (default-window-proc hwnd msg wparam lparam))

(defwndproc burning-wndproc (hwnd msg wparam lparam)
  (init-common-controls :bar)

  (switch msg
    ((const +wm-create+)
     (register-class "BurningControl"
                     (callback burningpanel-wndproc)
                     :styles (const +cs-hredraw+)
                     :cursor (load-cursor :arrow)
                     :background (get-sys-color-brush :button-face))
     (setf *burning* (create-window "BurningControl"
                                    :ex-styles (const +ws-ex-staticedge+)
                                    :styles (logior-consts +ws-child+ +ws-visible+)
                                    :x 0 :y 330 :width 490 :height 30
                                    :parent hwnd
                                    :menu 1)
           *burning-track* (create-window (const +trackbar-class+)
                                          :styles (logior-consts +ws-child+ +ws-visible+ +tbs-fixedlength+ +tbs-noticks+)
                                          :x 40 :y 25 :width 150 :height 25
                                          :parent hwnd
                                          :menu 2))
     (send-message *burning-track* (const +tbm-setrange+) :wparam 1 :lparam (make-long 0 750))
     (send-message *burning-track* (const +tbm-setpagesize+) :lparam 20)
     (send-message *burning-track* (const +tbm-setticfreq+) :wparam 20)
     (send-message *burning-track* (const +tbm-setpos+) :wparam 1 :lparam 150))
    ((const +wm-size+)
     (set-window-pos *burning* nil 0 (- (hiword lparam) 30) (loword lparam) 30
                     (const +swp-nozorder+)))
    ((const +wm-hscroll+)
     (setf *burning-pos* (send-message *burning-track* (const +tbm-getpos+)))
     (invalidate-rect *burning* nil t))
    ((const +wm-destroy+)
     (post-quit-message)))
  (default-window-proc hwnd msg wparam lparam))



(defun burning ()
  (zetcode-main "ZETCODE_BURNING" (callback burning-wndproc)))

;; -----------------------------------

(defwndproc pixel-wndproc (hwnd msg wparam lparam)
  (switch msg
    ((const +wm-paint+)
     (let ((rect (get-client-rect hwnd)))
       (unless (zerop (rect-bottom rect))
         (multiple-value-bind (hdc ps) (begin-paint hwnd)
           (dotimes (i 1000)
             (set-pixel hdc
                        (random (rect-right rect))
                        (random (rect-bottom rect))
                        (encode-rgb 255 0 0)))
           (end-paint hdc ps)))))
    ((const +wm-destroy+)
     (post-quit-message)))
  (default-window-proc hwnd msg wparam lparam))

(defun pixels ()
  (zetcode-main "ZETCODE_PIXELS" (callback pixel-wndproc)))


;; ------------------------------------

(defwndproc lines-wndproc (hwnd msg wparam lparam)
  (switch msg
    ((const +wm-paint+)
     (with-paint (hwnd hdc)
       (move-to hdc 50 50)
       (line-to hdc 250 50)
       (let* ((white-pen (get-stock-object :white-pen))
              (old-pen (select-object hdc white-pen)))
         (move-to hdc 50 100)
         (line-to hdc 250 100)
         (select-object hdc old-pen))))
    ((const +wm-destroy+)
     (post-quit-message)))
  (default-window-proc hwnd msg wparam lparam))

(defun lines ()
  (zetcode-main "ZETCODE_LINES" (callback lines-wndproc)))

;; --------------------------------

(defwndproc rectangle-wndproc (hwnd msg wparam lparam)
  (switch msg
    ((const +wm-paint+)
     (with-paint (hwnd hdc)
       (rectangle hdc 50 50 200 100)))
    ((const +wm-destroy+)
     (post-quit-message)))
  (default-window-proc hwnd msg wparam lparam))

(defun zc-rectangle ()
  (zetcode-main "ZETCODE_RECT" (callback rectangle-wndproc)))

;; --------------------------------

(defwndproc bezier-wndproc (hwnd msg wparam lparam)
  (switch msg
    ((const +wm-paint+)
     (with-paint (hwnd hdc)
       (poly-bezier hdc '((20 40) (320 200) (330 110) (450 40)))))
    ((const +wm-destroy+)
     (post-quit-message)))
  (default-window-proc hwnd msg wparam lparam))

(defun zc-bezier ()
  (zetcode-main "ZETCODE_BEZIER" (callback bezier-wndproc)))

;; ----------------------------------

(defwndproc penstyles-wndproc (hwnd msg wparam lparam)
  (switch msg
    ((const +wm-paint+)
     (with-paint (hwnd hdc)
       (let ((pen1 (create-pen :solid (encode-rgb 0 0 0) 1))
             (pen2 (create-pen :dash (encode-rgb 0 0 0) 1))
             (pen3 (create-pen :dot (encode-rgb 0 0 0) 1))
             (pen4 (create-pen :dash-dot (encode-rgb 0 0 0) 1))
             (pen5 (create-pen :dash-dot-dot (encode-rgb 0 0 0) 1)))
         (let ((hold-pen (select-object hdc pen1)))
           (move-to hdc 50 30)
           (line-to hdc 300 30)

           (select-object hdc pen2)
           (move-to hdc 50 50)
           (line-to hdc 300 50)

           (select-object hdc pen3)
           (move-to hdc 50 70)
           (line-to hdc 300 70)

           (select-object hdc pen4)
           (move-to hdc 50 90)
           (line-to hdc 300 90)

           (select-object hdc pen5)
           (move-to hdc 50 110)
           (line-to hdc 300 110)

           (select-object hdc hold-pen)
           (delete-object pen1)
           (delete-object pen2)
           (delete-object pen3)
           (delete-object pen4)
           (delete-object pen5)))))
    ((const +wm-destroy+)
     (post-quit-message)))
  (default-window-proc hwnd msg wparam lparam))

(defun zc-penstyles ()
  (zetcode-main "ZETCODE_PENSTYLES" (callback penstyles-wndproc)))


;; -------------------------------------------

(defwndproc linejoins-wndproc (hwnd msg wparam lparam)
  (switch msg
    ((const +wm-paint+)
     (with-paint (hwnd hdc)
       (let* ((pen1 (ext-create-pen (logior-consts +ps-solid+ +ps-geometric+ +ps-join-miter+)
                                   :width 8
                                   :brush-style (const +bs-solid+)
                                   :brush-color (encode-rgb 0 0 0)))
              (hold-pen (select-object hdc pen1)))
         (polygon hdc '((30 30) (130 30) (130 100) (30 100) (30 30)))

         (let ((pen2 (ext-create-pen (logior-consts +ps-solid+ +ps-geometric+ +ps-join-bevel+)
                                      :width 8
                                      :brush-style (const +bs-solid+)
                                      :brush-color (encode-rgb 0 0 0))))
           (select-object hdc pen2)
           (delete-object pen1)

           (move-to hdc 130 30)
           (polygon hdc '((160 30) (260 30) (260 100) (160 100) (160 30)))

           (let ((pen3 (ext-create-pen (logior-consts +ps-solid+ +ps-geometric+ +ps-join-round+)
                                       :width 8
                                       :brush-style (const +bs-solid+)
                                       :brush-color (encode-rgb 0 0 0))))
             (select-object hdc pen3)
             (delete-object pen2)

             (move-to hdc 260 30)
             (polygon hdc '((290 30) (390 30) (390 100) (290 100) (290 30)))

             (select-object hdc hold-pen)
             (delete-object pen3))))))
    ((const +wm-destroy+)
     (post-quit-message)))
  (default-window-proc hwnd msg wparam lparam))

(defun zc-linejoins ()
  (zetcode-main "ZETCODE_LINEJOINES" (callback linejoins-wndproc)))


;; -------------------------------------

(defwndproc solidbrush-wndproc (hwnd msg wparam lparam)
  (switch msg
    ((const +wm-paint+)
     (with-paint (hwnd hdc)
       (let* ((pen (create-pen (const +ps-null+) (encode-rgb 0 0 0) 1))
              (hold-pen (select-object hdc pen))
              (brush1 (create-solid-brush (encode-rgb 121 90 0)))
              (brush2 (create-solid-brush (encode-rgb 240 63 19)))
              (brush3(create-solid-brush (encode-rgb 240 210 18)))
              (brush4 (create-solid-brush (encode-rgb 9 189 21)))
              (hold-brush (select-object hdc brush1)))
         
         (rectangle hdc 30 30 100 100)
         (select-object hdc brush2)
         (rectangle hdc 110 30 180 100)
         (select-object hdc brush3)
         (rectangle hdc 30 110 100 180)
         (select-object hdc brush4)
         (rectangle hdc 110 110 180 180)

         (select-object hdc hold-pen)
         (select-object hdc hold-brush)

         (delete-object pen)
         (delete-object brush1)
         (delete-object brush2)
         (delete-object brush3)
         (delete-object brush4))))
    ((const +wm-destroy+)
     (post-quit-message)))
  (default-window-proc hwnd msg wparam lparam))

(defun solidbrush ()
  (zetcode-main "ZETCODE_SOLIDBRUSH" (callback solidbrush-wndproc)))

                
;; -------------------------------------

(defwndproc hatchbrushes-wndproc (hwnd msg wparam lparam)
  (switch msg
    ((const +wm-paint+)
     (with-paint (hwnd hdc)
       (let* ((pen (create-pen (const +ps-null+) (encode-rgb 0 0 0) 1))
              (hold-pen (select-object hdc pen))
              (brush1 (create-hatch-brush (encode-rgb 0 0 0) (const +hs-bdiagonal+)))
              (brush2 (create-hatch-brush (encode-rgb 0 0 0) (const +hs-fdiagonal+)))
              (brush3 (create-hatch-brush (encode-rgb 0 0 0) (const +hs-cross+)))
              (brush4 (create-hatch-brush (encode-rgb 0 0 0) (const +hs-horizontal+)))
              (brush5 (create-hatch-brush (encode-rgb 0 0 0) (const +hs-diagcross+)))
              (brush6 (create-hatch-brush (encode-rgb 0 0 0) (const +hs-vertical+)))
              (hold-brush (select-object hdc brush1))
              (col (get-sys-color :button-face)))

         (set-bk-color hdc col)
                 
         (rectangle hdc 30 30 100 100)
         (select-object hdc brush2)
         (rectangle hdc 110 30 180 100)
         (select-object hdc brush3)
         (rectangle hdc 190 30 260 80)
         (select-object hdc brush4)
         (rectangle hdc 30 110 100 160)
         (select-object hdc brush5)
         (rectangle hdc 110 110 180 160)
         (select-object hdc brush6)
         (rectangle hdc 190 110 260 160)
         
         (select-object hdc hold-pen)
         (select-object hdc hold-brush)

         (delete-object pen)
         (delete-object brush1)
         (delete-object brush2)
         (delete-object brush3)
         (delete-object brush4)
         (delete-object brush5)
         (delete-object brush6))))
    ((const +wm-destroy+)
     (post-quit-message)))
  (default-window-proc hwnd msg wparam lparam))

(defun hatchbrushes ()
  (zetcode-main "ZETCODE_HATCHBRUSHES" (callback hatchbrushes-wndproc)))


;; ---------------------------------------

(defvar *cb-bitmap* nil)

(defwndproc custombrush-wndproc (hwnd msg wparam lparam)
  (switch msg
    ((const +wm-create+)
     (setf *cb-bitmap* (create-bitmap 8 8 1 1
                                      #(#xff #x11 #x11 #x11
                                        #xff #xff #xff #xff
                                        #xff #xff #xff #xff
                                        #xff #xff #xff #xff
                                        #x00 #x00 #x00 #x00
                                        #x00 #x00 #x00 #x00
                                        #x00 #x00 #x00 #x00
                                        #x00 #x00 #x00 #x00))))
    ((const +wm-paint+)
     (with-paint (hwnd hdc)
       (let* ((brush (create-pattern-brush *cb-bitmap*))
              (old-brush (select-object hdc brush)))
         (select-object hdc (get-stock-object :null-pen))
         (rectangle hdc 20 20 250 160)
         (select-object hdc old-brush)
         (delete-object brush)
         (select-object hdc (get-stock-object :black-pen)))))
    ((const +wm-destroy+)
     (delete-object *cb-bitmap*)
     (post-quit-message)))
  (default-window-proc hwnd msg wparam lparam))

(defun custombrush ()
  (zetcode-main "CUSTOMBRUSH" (callback custombrush-wndproc)))

     
;; -------------------------------

(defwndproc shapes-wndproc (hwnd msg wparam lparam)
  (switch msg
    ((const +wm-paint+)
     (with-paint (hwnd hdc)
       (ellipse hdc 30 30 120 90)
       (round-rect hdc 150 30 240 90 15 20)
       (chord hdc 270 30 360 90 270 45 360 45)
       (polygon hdc '((30 145) (85 165) (105 110) (65 125) (30 105)))
       (rectangle hdc 150 110 230 160)))
    ((const +wm-destroy+)
     (post-quit-message)))
  (default-window-proc hwnd msg wparam lparam))

(defun shapes ()
  (zetcode-main "ZETCODE_SHAPES" (callback shapes-wndproc)))


;; ------------------------------------

(defwndproc star-wndproc (hwnd msg wparam lparam)
  (switch msg
    ((const +wm-paint+)
     (with-paint (hwnd hdc)
       (polyline hdc '((10 85) (85 75) (110 10) (135 75) (210 85) (160 125) (170 190)
                       (110 150) (50 190) (60 125) (10 85)))))
    ((const +wm-destroy+)
     (post-quit-message)))
  (default-window-proc hwnd msg wparam lparam))

(defun star ()
  (zetcode-main "ZETCODE_STAR" (callback star-wndproc)))

;; ---------------------------------------

(defwndproc sonnet-wndproc (hwnd msg wparam lparam)
  (switch msg
    ((const +wm-paint+)
     (with-paint (hwnd hdc)
       (let ((color (get-sys-color :button-face)))
         (set-bk-color hdc color)

         (let* ((font (create-font "Georgia"
                                  :height 15
                                  :weight (const +fw-medium+)))
                (hold-font (select-object hdc font)))

           (text-out hdc "Not marble, nor the gilded monuments" 50 20)
           (text-out hdc "Of princes, shall outlive this powerful rhyme;" 50 40)
           (text-out hdc "But you shall shine more bright in these contents" 50 60)
           (text-out hdc "Than unswept stone, besmear'd with sluttish time." 50 80)
           (text-out hdc "When wasteful war shall statues overturn," 50 100)
           (text-out hdc "And broils root out the work of masonry," 50 120)
           (text-out hdc "Nor Mars his sword, nor war's quick fire shall burn" 50 140)
           (text-out hdc "The living record of your memory." 50 160)
           (text-out hdc "'Gainst death, and all oblivious enmity" 50 180)
           (text-out hdc "Shall you pace forth; your praise shall still find room" 50 200)
           (text-out hdc "Even in the eyes of all posterity" 50 220)
           (text-out hdc "That wear this world out to the ending doom." 50 240)
           (text-out hdc "So, till the judgment that yourself arise," 50 260)
           (text-out hdc "You live in this, and dwell in lovers' eyes." 50 280)

           (select-object hdc hold-font)
           (delete-object font)))))
    ((const +wm-destroy+)
     (post-quit-message)))
  (default-window-proc hwnd msg wparam lparam))

(defun sonnet ()
  (zetcode-main "SONNET" (callback sonnet-wndproc)))

;; ----------------------------------------------

(defvar *db-bitmap* nil)

(defwndproc drawbitmap-wndproc (hwnd msg wparam lparam)
  (switch msg
    ((const +wm-create+)
     (setf *db-bitmap* (load-image "C:\\Users\\fjames\\fred.bmp" :type :bitmap :flags '(:load-from-file))))
    ((const +wm-paint+)
     (with-paint (hwnd hdc)
       (let* ((hdc-mem (create-compatible-dc hdc))
              (old-bitmap (select-object hdc-mem *db-bitmap*)))
         (let ((bitmap (get-object *db-bitmap* :bitmap)))
           (bit-blt hdc 5 5 hdc-mem 0 0
                    :width (bitmap-width bitmap)
                    :height (bitmap-height bitmap)
                    :raster-op :srccopy))

         (select-object hdc-mem old-bitmap)
         (delete-dc hdc-mem))))
    ((const +wm-destroy+)
     (delete-object *db-bitmap*)
     (post-quit-message)))
  (default-window-proc hwnd msg wparam lparam))

(defun drawbitmap ()
  (zetcode-main "ZETCODE_DRAWBITMAP" (callback drawbitmap-wndproc)))
