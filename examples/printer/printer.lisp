(defpackage #:ftw.printer
  (:use #:cl #:ftw)
  (:export #:print-file
	   #:print-data))

(in-package #:ftw.printer)

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
