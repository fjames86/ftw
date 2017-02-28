
(defpackage #:ftw.treeview
  (:use #:cl #:ftw)
  (:export #:treeview))

(in-package #:ftw.treeview)

(defun treeview-create (hwnd)

  (init-common-controls)

  (let ((h (create-window ftw::+wc-treeview+
			  :window-name "fred"
			  :styles (logior ftw::+ws-visible+ ftw::+ws-child+ ftw::+tvs-haslines+ ftw::+tvs-hasbuttons+ ftw::+tvs-checkboxes+ ftw::+tvs-linesatroot+)
			  :x 0 :y 0 :width 200 :height 200
			  :parent hwnd)))

    (set-default-font h)

    (let ((parent (ftw::treeview-insert-item h "Parent" :insert-after :root)))
      (let ((child (ftw::treeview-insert-item h "Child" :insert-after :last :parent parent)))
	(ftw::treeview-select-drop-target h parent)))

  nil))

(defwndproc treeview-wndproc (hwnd msg wparam lparam)
  (switch msg
    ((const +wm-create+)
     (treeview-create hwnd))
    ((const +wm-destroy+)
     (post-quit-message)))  
  (default-window-proc hwnd msg wparam lparam))

(defun treeview ()
  (default-message-loop 'treeview-wndproc
      :class-name "FTW_TREEVIEW"
      :title "Treeview"
      :width 400 :height 400))


