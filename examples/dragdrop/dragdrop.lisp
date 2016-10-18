

(defpackage #:ftw.dragdrop
  (:use #:cl #:cffi #:ftw))

(in-package #:ftw.dragdrop)

;;; We define an empty gui and wait for wm_dropfiles message.
;;; When we receive it we issue a messagebox to display them

(defwndproc dragdrop-wndproc (hwnd msg wparam lparam)
  (switch msg
    ((const +wm-create+)
     (drag-accept-files hwnd t))
    ((const +wm-dropfiles+)
     (let ((hdrop (make-pointer wparam)))
       (message-box :hwnd hwnd
                    :text (format nil "Files:~%~{~A~%~}~%" (drag-query-files hdrop))
                    :caption "Dragged files?")))
    ((const +wm-destroy+)
     (drag-accept-files hwnd nil)
     (post-quit-message)))
  (default-window-proc hwnd msg wparam lparam))


(defun dragdrop ()
  (default-message-loop (callback dragdrop-wndproc)
      :class-name "FTW_DRAGDROP"
      :title "Drag and drop"
      :width 500 :height 400))

;; ----------- TODO -----------------------

(defcfun (%open-clipboard "OpenClipboard" :convention :stdcall)
    :boolean
  (hwnd :pointer))

(defun open-clipboard (&optional hwnd)
  (%open-clipboard (or hwnd (null-pointer))))

(defcfun (%close-clipboard "CloseClipboard" :convention :stdcall)
    :boolean)

(defun close-clipboard ()
  (%close-clipboard))

(defcfun (%get-clipboard-data "GetClipboardData" :convention :stdcall)
    :pointer
  (format :uint32))

(defun get-clipboard-data (format)
  (%get-clipboard-data format))

(defcfun (%empty-clipboard "EmptyClipboard" :convention :stdcall)
    :boolean)

(defun empty-clipboard ()
  (%empty-clipboard))

(defcfun (%set-clipboard-data "SetClipboardData" :convention :stdcall)
    :boolean
  (format :uint32)
  (mem :pointer))

(defun set-clipboard-format (format mem)
  (%set-clipboard-data format mem))

