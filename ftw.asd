;;;; Copyright (c) Frank James 2016 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

(asdf:defsystem :ftw
  :name "ftw"
  :author "Frank James <frank.a.james@gmail.com>"
  :description "Common Lisp For the Win(32). A very thin layer over the top of Win32 GUI APIs." 
  :license "MIT"
  :serial t
  :components
  ((:file "package")
   (:file "constants")
   (:file "ffi")
   (:file "ftw"))
  :depends-on (:cffi :alexandria))



