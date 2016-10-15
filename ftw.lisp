
;;; This file defines a CLOS based layer to simplify defining and working 
;;; with gui elements. 

(in-package #:ftw)

(defun set-default-font (hwnd &optional font)
  (send-message hwnd (const +wm-setfont+) :wparam (or font (get-stock-object :default-gui-font))))

