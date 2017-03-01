
(defpackage #:ftw.treeview
  (:use #:cl #:ftw)
  (:export #:treeview))

(in-package #:ftw.treeview)

(defvar *TEST2-BITMAP*
        (create-bitmap-resource 32 32 1 32
#(         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x01 #x00 #x02  #x00 #x02 #x00 #x03  #x01 #x03 #x00 #x05  #x00 #x02 #x00 #x04 
                         #x00 #x01 #x00 #x02  #x00 #x00 #x00 #x01  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 

                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x01  #x01 #x05 #x01 #x08 
                         #x04 #x0E #x03 #x15  #x08 #x1A #x06 #x25  #x0A #x1E #x08 #x2B  #x09 #x1B #x07 #x27 
                         #x06 #x14 #x05 #x1D  #x03 #x0B #x03 #x10  #x01 #x04 #x01 #x06  #x00 #x00 #x00 #x01 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x01  #x02 #x07 #x03 #x0C  #x09 #x1D #x09 #x2B 
                         #x12 #x38 #x0F #x51  #x18 #x4B #x13 #x6A  #x1B #x51 #x15 #x73  #x1A #x50 #x15 #x72 
                         #x18 #x49 #x13 #x67  #x13 #x3B #x0F #x54  #x0C #x27 #x0A #x37  #x06 #x14 #x05 #x1D 
                         #x02 #x06 #x01 #x09  #x00 #x01 #x00 #x02  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 

                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x03 #x04  #x01 #x03 #x0C #x12  #x07 #x16 #x15 #x31  #x14 #x3D #x1C #x65 
                         #x1D #x58 #x1E #x85  #x1F #x5E #x1C #x8A  #x1F #x5E #x19 #x87  #x1F #x5F #x19 #x86 
                         #x1F #x5F #x19 #x86  #x1F #x5D #x18 #x84  #x1D #x58 #x17 #x7C  #x17 #x47 #x13 #x65 
                         #x0F #x2E #x0C #x41  #x06 #x13 #x05 #x1C  #x02 #x07 #x02 #x0B  #x00 #x00 #x01 #x03 
                         #x00 #x00 #x01 #x02  #x00 #x00 #x00 #x01  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x06 #x07 
                         #x00 #x00 #x1A #x1E  #x03 #x08 #x33 #x42  #x0E #x2C #x41 #x7A  #x1C #x55 #x3F #xA6 
                         #x1F #x5F #x3A #xAB  #x1F #x5F #x30 #xA1  #x1F #x5F #x24 #x94  #x1F #x5F #x1D #x8C 
                         #x1F #x5F #x1A #x89  #x1F #x5F #x1A #x88  #x1F #x5F #x19 #x86  #x1F #x5E #x19 #x85 
                         #x1C #x56 #x16 #x7A  #x14 #x3F #x13 #x5C  #x09 #x1D #x0E #x32  #x02 #x06 #x0E #x17 
                         #x00 #x00 #x0E #x10  #x00 #x00 #x09 #x0B  #x00 #x00 #x03 #x04  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 

                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x03 #x04  #x00 #x00 #x16 #x19 
                         #x01 #x02 #x40 #x49  #x05 #x0E #x61 #x7C  #x13 #x38 #x5F #xA9  #x1F #x5D #x54 #xC6 
                         #x20 #x5F #x55 #xCA  #x20 #x60 #x54 #xC9  #x20 #x5F #x4B #xBF  #x1F #x5F #x3B #xAD 
                         #x1F #x60 #x2A #x9A  #x1F #x5F #x22 #x91  #x1F #x5F #x1C #x8B  #x1F #x5F #x19 #x87 
                         #x1F #x5E #x19 #x87  #x1C #x58 #x1E #x84  #x12 #x36 #x26 #x68  #x05 #x0E #x34 #x4A 
                         #x01 #x02 #x37 #x40  #x00 #x00 #x29 #x2E  #x00 #x00 #x14 #x17  #x00 #x00 #x04 #x05 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x01  #x00 #x00 #x07 #x08  #x00 #x00 #x27 #x2C 
                         #x01 #x02 #x5C #x69  #x05 #x0E #x71 #x8E  #x14 #x3C #x64 #xB2  #x1F #x5D #x5C #xCF 
                         #x20 #x60 #x62 #xD9  #x20 #x60 #x68 #xDF  #x20 #x60 #x68 #xDF  #x20 #x60 #x61 #xD8 
                         #x1F #x60 #x4D #xC1  #x20 #x60 #x38 #xAA  #x1F #x60 #x29 #x99  #x1F #x5F #x1E #x8D 
                         #x1F #x5F #x1B #x8A  #x1E #x5E #x27 #x95  #x17 #x47 #x3D #x94  #x08 #x17 #x5D #x81 
                         #x01 #x03 #x66 #x75  #x01 #x01 #x54 #x5E  #x00 #x00 #x31 #x37  #x00 #x00 #x0F #x11 
                         #x00 #x00 #x01 #x02  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 

                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x01  #x00 #x00 #x0A #x0C  #x00 #x00 #x31 #x37 
                         #x01 #x01 #x68 #x75  #x04 #x0A #x79 #x92  #x0F #x2C #x74 #xB3  #x1D #x57 #x6A #xD8 
                         #x20 #x60 #x6C #xE3  #x20 #x60 #x6C #xE4  #x20 #x60 #x6C #xE4  #x20 #x60 #x6C #xE4 
                         #x20 #x60 #x64 #xDB  #x1F #x60 #x51 #xC6  #x1F #x60 #x3D #xAF  #x1F #x60 #x29 #x99 
                         #x1F #x5F #x21 #x90  #x1E #x5D #x2E #x9D  #x17 #x46 #x4D #xA4  #x08 #x16 #x6D #x92 
                         #x02 #x03 #x75 #x86  #x01 #x01 #x6F #x7C  #x01 #x01 #x4F #x58  #x00 #x00 #x1C #x20 
                         #x00 #x00 #x03 #x04  #x00 #x00 #x00 #x01  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x01  #x00 #x00 #x06 #x07  #x00 #x00 #x28 #x2D 
                         #x01 #x01 #x63 #x6F  #x02 #x04 #x87 #x9B  #x09 #x19 #x92 #xBF  #x16 #x40 #x82 #xD9 
                         #x1F #x5B #x70 #xE3  #x20 #x5F #x6D #xE4  #x20 #x60 #x6D #xE5  #x20 #x60 #x6C #xE4 
                         #x20 #x60 #x6C #xE4  #x20 #x5F #x62 #xD8  #x20 #x60 #x4F #xC4  #x1F #x5F #x3A #xAC 
                         #x1F #x5F #x27 #x96  #x1D #x5A #x34 #x9F  #x14 #x3B #x55 #xA1  #x06 #x12 #x6F #x90 
                         #x01 #x02 #x77 #x86  #x01 #x01 #x76 #x84  #x01 #x01 #x5D #x68  #x00 #x00 #x25 #x2A 
                         #x00 #x00 #x06 #x07  #x00 #x00 #x00 #x01  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 

                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x03 #x04  #x00 #x00 #x18 #x1B 
                         #x01 #x01 #x4E #x57  #x01 #x01 #x8F #xA0  #x03 #x06 #xAA #xC3  #x0A #x1D #x9D #xCF 
                         #x14 #x3B #x88 #xD9  #x1B #x50 #x78 #xE0  #x1E #x57 #x73 #xE2  #x1E #x59 #x71 #xE3 
                         #x1F #x5B #x70 #xE2  #x1F #x5D #x6D #xE2  #x1F #x5D #x60 #xD4  #x1F #x5E #x48 #xBA 
                         #x1D #x59 #x31 #x9C  #x17 #x45 #x3D #x92  #x0C #x23 #x62 #x94  #x03 #x09 #x74 #x8B 
                         #x01 #x02 #x78 #x87  #x01 #x01 #x76 #x84  #x01 #x01 #x5E #x69  #x00 #x00 #x24 #x29 
                         #x00 #x00 #x05 #x06  #x00 #x00 #x00 #x01  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x08 #x09 
                         #x00 #x00 #x30 #x36  #x01 #x01 #x88 #x98  #x02 #x02 #xB0 #xC5  #x03 #x04 #xB0 #xC8 
                         #x06 #x10 #xA8 #xCC  #x0B #x1E #x9D #xD0  #x0E #x28 #x95 #xD3  #x0F #x2D #x91 #xD5 
                         #x10 #x32 #x8E #xD6  #x14 #x3B #x87 #xD9  #x16 #x43 #x7D #xD6  #x16 #x42 #x62 #xB7 
                         #x12 #x38 #x42 #x88  #x0B #x22 #x4F #x7E  #x05 #x0E #x6D #x89  #x02 #x03 #x76 #x87 
                         #x01 #x01 #x78 #x86  #x01 #x01 #x73 #x81  #x01 #x01 #x54 #x5E  #x00 #x00 #x1C #x20 
                         #x00 #x00 #x03 #x04  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 

                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x01 
                         #x00 #x00 #x16 #x19  #x01 #x01 #x69 #x76  #x02 #x02 #xAD #xC1  #x02 #x02 #xB2 #xC7 
                         #x02 #x03 #xB0 #xC7  #x03 #x0A #xAB #xCA  #x06 #x11 #xA5 #xCC  #x08 #x18 #xA0 #xCE 
                         #x0A #x1E #x9C #xD0  #x0B #x21 #x9A #xD1  #x0A #x1E #x9C #xCF  #x08 #x16 #x88 #xB1 
                         #x06 #x10 #x5A #x76  #x03 #x08 #x62 #x76  #x02 #x04 #x76 #x87  #x01 #x01 #x79 #x87 
                         #x01 #x01 #x78 #x86  #x01 #x01 #x6F #x7C  #x00 #x00 #x46 #x4E  #x00 #x00 #x11 #x14 
                         #x00 #x00 #x00 #x01  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x09 #x0B  #x00 #x00 #x49 #x52  #x02 #x02 #x9D #xAF  #x04 #x09 #xAB #xC8 
                         #x08 #x17 #xA2 #xCE  #x0F #x2C #x92 #xD4  #x15 #x3C #x85 #xD9  #x18 #x45 #x7F #xDC 
                         #x19 #x48 #x7D #xDD  #x16 #x40 #x84 #xDB  #x0F #x2E #x8F #xD4  #x08 #x19 #x92 #xBE 
                         #x03 #x09 #x66 #x7C  #x01 #x03 #x6B #x7B  #x01 #x02 #x77 #x86  #x01 #x01 #x79 #x87 
                         #x01 #x01 #x78 #x86  #x01 #x01 #x69 #x76  #x00 #x00 #x36 #x3D  #x00 #x00 #x09 #x0B 
                         #x00 #x00 #x00 #x01  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 

                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x03 #x04  #x00 #x01 #x29 #x2F  #x04 #x0B #x7A #x95  #x10 #x2E #x89 #xCC 
                         #x1A #x4B #x7B #xDE  #x1F #x5B #x70 #xE2  #x20 #x5E #x6E #xE4  #x20 #x5E #x6D #xE4 
                         #x20 #x5E #x6D #xE4  #x1F #x5D #x6F #xE4  #x1B #x51 #x77 #xDF  #x12 #x34 #x84 #xCC 
                         #x0A #x1E #x64 #x91  #x06 #x12 #x68 #x88  #x04 #x0A #x73 #x8B  #x02 #x03 #x78 #x88 
                         #x01 #x01 #x77 #x85  #x01 #x01 #x65 #x71  #x00 #x00 #x2C #x31  #x00 #x00 #x05 #x06 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x01 #x02 #x04  #x03 #x0A #x1B #x2A  #x10 #x2F #x51 #x8F  #x1D #x55 #x5E #xC8 
                         #x1F #x5F #x69 #xDF  #x20 #x60 #x6C #xE4  #x20 #x60 #x6C #xE4  #x20 #x60 #x6D #xE5 
                         #x20 #x60 #x6D #xE5  #x20 #x60 #x6D #xE5  #x20 #x5F #x6C #xE3  #x1D #x57 #x6C #xD9 
                         #x18 #x4A #x58 #xB6  #x14 #x3C #x5C #xAA  #x0D #x27 #x68 #xA0  #x06 #x11 #x72 #x91 
                         #x02 #x04 #x75 #x86  #x01 #x01 #x62 #x6E  #x00 #x00 #x26 #x2B  #x00 #x00 #x03 #x04 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 

                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x01 #x00 #x02 
                         #x03 #x0B #x03 #x11  #x0F #x2D #x14 #x4A  #x1C #x55 #x37 #x9D  #x1F #x5F #x4C #xC0 
                         #x20 #x60 #x57 #xCD  #x20 #x60 #x66 #xDD  #x20 #x60 #x6C #xE4  #x20 #x60 #x6C #xE4 
                         #x20 #x60 #x6C #xE4  #x20 #x60 #x6C #xE4  #x20 #x60 #x68 #xE0  #x20 #x5F #x5E #xD4 
                         #x1F #x5E #x54 #xC7  #x1E #x5A #x53 #xC2  #x18 #x48 #x5B #xB6  #x0C #x23 #x6A #x9E 
                         #x03 #x08 #x73 #x89  #x01 #x01 #x61 #x6E  #x00 #x00 #x25 #x2A  #x00 #x00 #x02 #x03 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x01  #x02 #x06 #x01 #x09 
                         #x0B #x21 #x08 #x2F  #x19 #x4D #x17 #x71  #x1F #x5E #x26 #x94  #x1F #x60 #x36 #xA8 
                         #x1F #x5F #x45 #xB8  #x1F #x5F #x52 #xC7  #x20 #x60 #x62 #xD9  #x20 #x60 #x69 #xE1 
                         #x20 #x60 #x68 #xE0  #x1F #x60 #x64 #xDC  #x20 #x5F #x5D #xD3  #x1F #x60 #x55 #xCB 
                         #x1F #x5F #x52 #xC7  #x1F #x5F #x52 #xC6  #x1D #x57 #x55 #xC1  #x11 #x32 #x64 #xA8 
                         #x04 #x0D #x71 #x8C  #x01 #x02 #x5D #x6A  #x00 #x00 #x22 #x26  #x00 #x00 #x03 #x04 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 

                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x01 #x00 #x02  #x04 #x0C #x03 #x11 
                         #x12 #x38 #x0E #x4F  #x1E #x5B #x18 #x81  #x1F #x5F #x1B #x8A  #x1F #x5F #x1F #x8E 
                         #x1F #x5F #x27 #x97  #x1F #x5F #x33 #xA4  #x20 #x5F #x4B #xBF  #x20 #x60 #x59 #xCF 
                         #x20 #x60 #x59 #xCF  #x20 #x60 #x57 #xCC  #x20 #x60 #x54 #xC9  #x1F #x5F #x52 #xC7 
                         #x1F #x5F #x52 #xC7  #x1F #x5F #x52 #xC6  #x1D #x57 #x55 #xC1  #x11 #x32 #x64 #xA8 
                         #x04 #x0D #x71 #x8C  #x01 #x02 #x56 #x62  #x00 #x00 #x1C #x20  #x00 #x00 #x02 #x03 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x01 #x00 #x02  #x06 #x14 #x05 #x1D 
                         #x17 #x45 #x12 #x62  #x1F #x5E #x19 #x85  #x1F #x5F #x19 #x87  #x1F #x5F #x1A #x88 
                         #x1F #x5F #x1C #x8B  #x1F #x60 #x2C #x9C  #x1F #x5F #x45 #xB8  #x1F #x60 #x51 #xC6 
                         #x1F #x5F #x52 #xC7  #x1F #x5F #x52 #xC7  #x1F #x5F #x52 #xC7  #x1F #x5F #x52 #xC7 
                         #x1F #x5F #x52 #xC6  #x1E #x5D #x53 #xC4  #x19 #x49 #x5B #xB7  #x0C #x23 #x69 #x9D 
                         #x03 #x08 #x6D #x82  #x01 #x01 #x4B #x55  #x00 #x00 #x13 #x16  #x00 #x00 #x01 #x02 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 

                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x02 #x00 #x03  #x07 #x16 #x05 #x1F 
                         #x18 #x49 #x13 #x67  #x1F #x5E #x19 #x85  #x1F #x5F #x19 #x87  #x1F #x5F #x19 #x87 
                         #x1F #x5F #x1D #x8C  #x1F #x60 #x2E #x9F  #x1F #x5F #x48 #xBC  #x1F #x60 #x51 #xC6 
                         #x1F #x5F #x52 #xC7  #x1F #x5F #x52 #xC7  #x1F #x60 #x51 #xC6  #x1F #x5F #x52 #xC6 
                         #x1E #x5A #x53 #xC2  #x18 #x48 #x5C #xB7  #x0E #x2B #x67 #xA3  #x06 #x10 #x71 #x90 
                         #x01 #x03 #x6F #x7F  #x01 #x01 #x4F #x58  #x00 #x00 #x1A #x1E  #x00 #x00 #x00 #x01 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x02 #x00 #x03  #x07 #x16 #x06 #x20 
                         #x18 #x48 #x13 #x66  #x1F #x5E #x19 #x85  #x1F #x5F #x19 #x87  #x1F #x5F #x19 #x87 
                         #x1F #x5F #x1B #x8A  #x1F #x60 #x2B #x9B  #x1F #x5F #x45 #xB8  #x1F #x60 #x51 #xC6 
                         #x1F #x5F #x52 #xC6  #x1F #x5F #x52 #xC6  #x1F #x5E #x52 #xC5  #x1C #x54 #x55 #xBE 
                         #x13 #x39 #x61 #xAB  #x0A #x1C #x6D #x98  #x04 #x0B #x75 #x8E  #x02 #x03 #x76 #x87 
                         #x01 #x01 #x76 #x84  #x01 #x01 #x65 #x71  #x00 #x00 #x32 #x38  #x00 #x00 #x0A #x0C 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 

                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x01 #x00 #x02  #x05 #x11 #x04 #x18 
                         #x14 #x3C #x10 #x55  #x1E #x5B #x18 #x81  #x1F #x5F #x19 #x86  #x1F #x5F #x19 #x87 
                         #x1F #x5F #x1A #x89  #x1F #x5F #x24 #x93  #x1F #x5E #x3B #xAB  #x1E #x5B #x4E #xBE 
                         #x1D #x58 #x54 #xC1  #x1B #x52 #x57 #xBD  #x17 #x43 #x5D #xB4  #x0F #x2C #x66 #xA3 
                         #x06 #x12 #x71 #x92  #x02 #x04 #x76 #x88  #x01 #x02 #x78 #x87  #x01 #x01 #x79 #x87 
                         #x01 #x01 #x78 #x86  #x01 #x01 #x72 #x80  #x00 #x00 #x47 #x50  #x00 #x00 #x11 #x14 
                         #x00 #x00 #x01 #x02  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x01  #x03 #x09 #x02 #x0D 
                         #x0C #x27 #x0A #x37  #x18 #x49 #x13 #x68  #x1D #x5A #x17 #x7F  #x1E #x5C #x18 #x83 
                         #x1E #x5C #x18 #x83  #x1D #x58 #x1B #x82  #x19 #x4C #x2B #x85  #x13 #x38 #x48 #x90 
                         #x0E #x2A #x60 #x9A  #x0A #x1E #x6B #x98  #x07 #x14 #x70 #x93  #x04 #x0A #x74 #x8C 
                         #x02 #x03 #x78 #x88  #x01 #x01 #x79 #x87  #x01 #x01 #x79 #x87  #x01 #x01 #x79 #x87 
                         #x01 #x01 #x79 #x87  #x01 #x01 #x76 #x84  #x01 #x01 #x51 #x5B  #x00 #x00 #x18 #x1B 
                         #x00 #x00 #x01 #x02  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 

                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x02 #x00 #x04 
                         #x05 #x0F #x04 #x16  #x0B #x23 #x09 #x32  #x12 #x38 #x0E #x4F  #x15 #x3F #x10 #x5A 
                         #x14 #x3E #x10 #x58  #x10 #x32 #x0E #x48  #x0A #x1F #x12 #x37  #x05 #x0F #x27 #x3D 
                         #x02 #x04 #x47 #x54  #x01 #x02 #x5F #x6D  #x01 #x02 #x6C #x7A  #x01 #x02 #x73 #x82 
                         #x01 #x01 #x77 #x85  #x01 #x01 #x78 #x86  #x01 #x01 #x79 #x87  #x01 #x01 #x79 #x87 
                         #x01 #x01 #x78 #x86  #x01 #x01 #x70 #x7D  #x00 #x00 #x46 #x4E  #x00 #x00 #x11 #x14 
                         #x00 #x00 #x01 #x02  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x02 #x00 #x03  #x02 #x07 #x01 #x0A  #x04 #x0E #x03 #x15  #x05 #x11 #x04 #x19 
                         #x05 #x11 #x04 #x18  #x03 #x0B #x03 #x10  #x01 #x05 #x01 #x08  #x00 #x01 #x07 #x0A 
                         #x00 #x00 #x15 #x18  #x00 #x00 #x2A #x2F  #x00 #x00 #x3F #x47  #x01 #x01 #x56 #x60 
                         #x01 #x01 #x67 #x73  #x01 #x01 #x71 #x7E  #x01 #x01 #x73 #x81  #x01 #x01 #x73 #x81 
                         #x01 #x01 #x6D #x7A  #x01 #x01 #x55 #x5F  #x00 #x00 #x2C #x31  #x00 #x00 #x0A #x0C 
                         #x00 #x00 #x00 #x01  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 

                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x01  #x00 #x00 #x00 #x01  #x00 #x01 #x00 #x02 
                         #x00 #x01 #x00 #x02  #x00 #x00 #x00 #x01  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x01  #x00 #x00 #x05 #x06  #x00 #x00 #x11 #x13  #x00 #x00 #x22 #x26 
                         #x00 #x00 #x34 #x3B  #x00 #x00 #x45 #x4D  #x01 #x01 #x4D #x56  #x00 #x00 #x4A #x53 
                         #x00 #x00 #x3D #x45  #x00 #x00 #x24 #x29  #x00 #x00 #x0F #x11  #x00 #x00 #x01 #x02 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x01  #x00 #x00 #x04 #x05 
                         #x00 #x00 #x0A #x0C  #x00 #x00 #x11 #x14  #x00 #x00 #x14 #x17  #x00 #x00 #x14 #x17 
                         #x00 #x00 #x0D #x0F  #x00 #x00 #x06 #x07  #x00 #x00 #x01 #x02  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 

                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x01  #x00 #x00 #x00 #x01  #x00 #x00 #x01 #x02  #x00 #x00 #x01 #x02 
                         #x00 #x00 #x00 #x01  #x00 #x00 #x00 #x01  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 

                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 
                         #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 )))


(defun treeview-create (hwnd)

  (init-common-controls)

  (let ((h (create-window ftw::+wc-treeview+
			  :window-name "fred"
			  :styles (logior ftw::+ws-visible+ ftw::+ws-child+ ftw::+tvs-haslines+ ftw::+tvs-linesatroot+ ftw::+tvs-hasbuttons+)
			  :ex-styles ftw::+ws-ex-clientedge+
			  :x 0 :y 0 :width 200 :height 200
			  :parent hwnd)))

    (set-default-font h)

    (let ((il (imagelist-create 32 32)))
      (imagelist-add il *test2-bitmap*)
      (treeview-set-imagelist h il))

    (let ((parent (ftw::treeview-insert-item h "Parent" :insert-after :root :image 0 :selected-image 0)))
      (ftw::treeview-insert-item h "Child" :insert-after :last :parent parent))

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


