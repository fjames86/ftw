;;;; Copyright (c) Frank James 2016 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

;;; This file shows how to print files using the print spooler

(defpackage #:ftw.printer
  (:use #:cl #:ftw)
  (:export #:print-file
	   #:print-data))

(in-package #:ftw.printer)

;; These use the print spooler APIs 
(defun print-data (data)
  (let* ((printinfo (print-dialog)))
    (when printinfo 
      (let ((hp (open-printer (getf printinfo :device))))
        (unwind-protect
             (progn
               (start-doc-printer hp
                                  :name "My Document"
                                  :datatype "RAW") ;; "NT EMF 1.008" 
               (start-page-printer hp)
               (write-printer hp data)
               (end-page-printer hp)
               (end-doc-printer hp))
          (close-printer hp))))))



(defun print-file ()
  (let ((filenames (nth-value 1 (get-open-file-name :title "Print file"))))
    (when filenames
      (with-open-file (f (first filenames) :direction :input :element-type '(unsigned-byte 8))
        (let ((data (make-array (file-length f) :element-type '(unsigned-byte 8))))
          (read-sequence data f)
          (print-data data))))))


;; --------------------

;; these use the GDI APIs
(defun print-rectangle ()
  "Print a solid black rectangle" 
  (let ((printinfo (print-dialog)))
    (when printinfo
      (let ((pname (getf printinfo :device)))
        (let ((hdc (create-dc pname)))
          (unwind-protect
               (progn
                 (start-doc hdc "Print file")
                 (start-page hdc)
                 (select-object hdc (get-stock-object :black-brush))
                 (rectangle hdc 100 100 250 250)
                 (end-page hdc)
                 (end-doc hdc))
            (delete-dc hdc)))))))

;; this uses the with-printer-dc macro 
(defun print-rectangles ()
  "Print a solid black rectangle with a white rectangle inside it." 
  (let ((device-name (getf (print-dialog) :device)))
    (when device-name
      (with-printer-dc (hdc device-name)
        (select-object hdc (get-stock-object :black-brush))
        (print-page
         (rectangle hdc 200 200 300 300)
         (select-object hdc (get-stock-object :white-brush))
         (rectangle hdc 250 250 275 275))))))
