;;;; Copyright (c) Frank James 2016 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

(defpackage #:ftw.dialogs
  (:use #:cl #:cffi #:ftw))

(in-package #:ftw.dialogs)

;; ----------- FindReplace ---------------

;; The FindReplace dialogs FindText and ReplaceText are
;; modeless and therefore require a little bit more work than
;; the modal dialogs. This is because they require us allocating
;; buffers which live for the lifetime of the dialog. In addition
;; we must call IsDialogMessage() and intercept the FINDMSGSTRING
;; message which tells us the user clicked on the "find next" button
;; in the dialog. 


(defvar *fr* nil)

(defwndproc findtext-wndproc (hwnd msg wparam lparam)
  (switch msg
    ((const +wm-create+)
     (setf *fr* (find-text :hwnd hwnd)))
    ((get-findmsgstring)
     (multiple-value-bind (flags find replace) (foreign-findreplace (make-pointer lparam))
       (format t "find replace ~S ~S ~S~%" flags find replace)
       (when (member :dialog-term flags)
         (free-findreplace *fr*)
         (setf *fr* nil))))
    ((const +wm-destroy+)
     (post-quit-message)))
  (default-window-proc hwnd msg wparam lparam))

    
(defun findtext-test ()
  (let ((cname "FTW_FINDTEXT"))
    (register-class cname 
                    (callback findtext-wndproc)
                    :cursor (load-cursor :arrow)
                    :background (get-sys-color-brush :3d-face))
    (let ((hwnd (create-window cname 
                               :window-name "Find text" 
                               :styles (logior-consts +ws-overlappedwindow+ +ws-visible+)
                               :x 100 :y 100 :width 400 :height 300))
          (msg (make-msg)))
      (unless hwnd (return-from findtext-test nil))
      
      (show-window hwnd)
      (update-window hwnd)
      (set-foreground-window hwnd)
      (do ((done nil))
          (done)
        (let ((r (get-message msg)))
          (cond
            ((zerop r) (setf done t))
            ;; Note: we need to call is-dialog-message in the mesasge loop as per MSDN instructions 
            ((not (is-dialog-message hwnd msg))
             (translate-message msg)
             (dispatch-message msg))))))))

;; --------------------- ChooseColor -------------------

(defun choose-color-test ()
  (choose-color))

;; ------------------- Choose Font ------------

(defun choose-font-test ()
  (choose-font))

;; -------------------- Open file -----------

(defun open-file-test ()
  (get-open-file-name))

;; -------------------- Save file -------------

(defun save-file-test ()
  (get-save-file-name))

;; --------------- Print --------------

(defun print-test ()
  (print-dialog))

;; ------------------ Page setup ----------

(defun page-setup-test ()
  (page-setup-dialog))


;; ------------- Example ------------

(defvar *show-find-dialog-text* nil)

(defwndproc find-dialog-dlgproc (hwnd msg wparam lparam)
  (declare (ignore lparam))
  (switch msg
    ((const +wm-initdialog+)
     1)
    ((const +wm-command+)
     (switch (loword wparam)
       (1 ;; text box
        nil)
       (2 ;; ok
        (setf *show-find-dialog-text* (get-window-text (get-dialog-item hwnd 1)))
        (end-dialog hwnd))
       (3 ;; cancel
        (setf *show-find-dialog-text* nil)
        (end-dialog hwnd)))
     1)
    (t 
     0)))

(defun show-find-dialog (&optional hwnd)
  (setf *show-find-dialog-text* nil)
  (dialog-box (callback find-dialog-dlgproc)
              `((:class-name :static
                             :x 10 :y 10 :cx 40 :cy 10
                             :styles ,(logior-consts +ws-child+ +ws-visible+ +ss-left+)
                             :title "Find what:")
                (:class-name :edit
                             :id 1
                             :x 55 :y 10 :cx 105 :cy 8
                             :styles ,(logior-consts +ws-child+ +ws-visible+ +ws-tabstop+))
                (:class-name :button
                             :id 2
                             :title "OK"
                             :x 55 :y 25 :cx 50 :cy 14
                             :styles ,(logior-consts +ws-child+ +ws-visible+ +bs-defpushbutton+ +ws-tabstop+))
                (:class-name :button
                             :id 3
                             :title "Cancel"
                             :x 110 :y 25 :cx 50 :cy 14
                             :styles ,(logior-consts +ws-child+ +ws-visible+ +ws-tabstop+)))
              :hwnd (or hwnd (null-pointer))
              :styles (logior-consts +ws-popup+ +ws-border+ +ws-sysmenu+
                                     +ds-modalframe+ +ws-caption+
                                     +ws-visible+ +ds-setfont+)
              :title "Find"
              :point-size 8 :font "Microsoft Sans Serif" 
              :x 50 :y 50 :cx 170 :cy 45)
  *show-find-dialog-text*)
