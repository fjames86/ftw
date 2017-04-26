
(defpackage #:ftw.dragons
  (:use #:cl #:ftw)
  (:export #:dragons))

(in-package #:ftw.dragons)

(defparameter *hwnds* nil)
(defun add-hwnd (name hwnd)
  (push (cons name hwnd) *hwnds*))
(defun get-hwnd (name)
  (cdr (assoc name *hwnds*)))

(defvar *recordtypes*
  '(("A" :a)
    ("NS" :ns)
    ("CNAME" :cname)
    ("PTR" :ptr)
    ("TXT" :txt)
    ("AAAA" :aaaa)
    ("SRV" :srv)
    ("ALL" :all)))

(defun dragons-create (hwnd)
  (create-static "DNS Address:" :parent hwnd :styles (logior ftw::+ws-visible+ ftw::+ws-child+) :x 25 :y 27 :width 100 :height 25)

  (let ((default-font (get-default-font)))
    (let ((h (create-window ftw::+wc-ipaddress+
			    :parent hwnd
			    :styles (logior ftw::+ws-visible+ ftw::+ws-child+)
			    :x 140 :y 25 :width 200 :height 25)))
      (set-default-font h default-font)

      (let ((ips dns:*dns-addrs*))
	(when ips
	  (let ((inaddr (fsocket:sockaddr-in-addr (first ips))))
	    (send-message h
			  ftw::+ipm-setaddress+
			  0
			  (logior (ash (aref inaddr 0) 24)
				  (ash (aref inaddr 1) 16)
				  (ash (aref inaddr 2) 8)
				  (aref inaddr 3))))))
      (add-hwnd 'ipaddress h))
    
    (let ((h (create-window :button
			    :window-name "Search"
			    :styles (logior ftw::+ws-visible+ ftw::+ws-child+
					    ftw::+bs-groupbox+)
			    :x 15 :y 65 :width 335 :height 130
			    :parent hwnd)))
      (set-default-font h default-font))
  
    (create-static "Record Type:" :parent hwnd :x 25 :y 93 :width 100 :height 25)
    (create-static "Name:" :parent hwnd :x 25 :y 123 :width 100 :height 25)
    
    (let ((h (create-window :combobox :window-name "Fred" :parent hwnd
			    :styles (logior ftw::+ws-visible+ ftw::+ws-child+ ftw::+cbs-dropdownlist+ ftw::+ws-vscroll+)
			    :x 140 :y 95 :width 200 :height 200
			    :menu 2)))
      (set-default-font h default-font)
      (dolist (str (mapcar #'car *recordtypes*))
	(with-wide-string (s str)
	  (send-message h ftw::+cb-addstring+ 0 s)))
      (send-message h ftw::+cb-setcursel+ 0 0)
      (add-hwnd 'recordtype h))
    
    (let ((h (create-edit :parent hwnd
			  :x 140 :y 125 :width 200 :height 25)))
      (add-hwnd 'name h))
    
    (let ((h (create-window :button
			    :window-name "Query"
			    :styles (logior ftw::+ws-visible+ ftw::+ws-child+)
			    :x 265 :y 160 :width 75 :height 23
			    :parent hwnd
			    :menu 1)))
      (set-default-font h default-font)
      (add-hwnd 'query h))

    (let ((h (create-window :listbox
			    :x 15 :y 215 :width 335 :height 200
			    :parent hwnd
			    :ex-styles ftw::+ws-ex-clientedge+
			    :styles (logior ftw::+ws-visible+ ftw::+ws-child+))))
      (set-default-font h default-font)
      (add-hwnd 'rlist h))))

(defun get-record-type ()
  (let ((hwnd (get-hwnd 'recordtype)))
    (let ((idx (send-message hwnd ftw::+cb-getcursel+ 0 0)))
      (when (>= idx 0)
	(cadr (nth idx *recordtypes*))))))

(defun get-dns-addr ()
  (let ((hwnd (get-hwnd 'ipaddress)))
    (cffi:with-foreign-object (inaddr :uint32)
      (send-message hwnd ftw::+ipm-getaddress+ 0 inaddr)
      (fsocket:sockaddr-in (cffi:mem-ref inaddr :uint32) 53))))

(defun format-rr (rr)
  (format nil "~A ~A ~A" (dragons:rr-type rr) (dragons:rr-name rr) (dragons:rr-rdata rr)))

(defun format-results (results)
  (let ((h (get-hwnd 'rlist)))
    (send-message h ftw::+lb-resetcontent+ 0 0)
    (dolist (rr results)
      (ftw:with-wide-string (ws (format-rr rr))
	(send-message h ftw::+lb-addstring+ 0 ws)))))

(defun dragons-command (hwnd id)
  (switch id
    (1 ;; query button
     (handler-case 
	 (let ((answers (dns:query (dns:question (get-window-text (get-hwnd 'name))
						 (get-record-type))
				   :addr (get-dns-addr)
				   :timeout 500)))
	   (format-results answers))
       (error (e)
	 (message-box :hwnd hwnd
		      :text (format nil "~A" e)
		      :caption "Error"
		      :icon :error))))))

(defwndproc dragons-wndproc (hwnd msg wparam lparam)
  (switch msg
    (ftw::+wm-create+
     (dragons-create hwnd))
    (ftw::+wm-command+
     (dragons-command hwnd (loword wparam)))
    (ftw::+wm-close+
     (post-quit-message)))
  (default-window-proc hwnd msg wparam lparam))

(defun dragons ()
  (default-message-loop 'dragons-wndproc
      :class-name "FTW_DRAGONS_MAIN"
      :title "Dragons DNS Viewer"
      :height 500
      :styles (logior ftw::+ws-overlapped+ ftw::+ws-caption+ ftw::+ws-sysmenu+
		      ftw::+ws-minimizebox+)))
      
