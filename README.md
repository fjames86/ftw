
# FTW - Common Lisp For the Win(32) 

# 1. Introduction 
This library provides a very thin interface to the underlying
APIs for writing native Windows GUIs in Common Lisp.

The intention is to be able to write the same sort of codes in Lisp as you
would if writing normal Win32 GUIs in C. This also opens the possibility
for writing other more generate graphical applications like games.

# 2. Functions
All underlying Win32 functions have Lisp equivalents, mostly with CamelCase replaced with the Lisp style kebab-case.

Because this is a very thin wrapper over the top of the underlying Win32 API,
it is assumed the user is at least familiar with the equivalent C programming.
Documentation for each of the functions can be found on MSDN or any of
the other C language resource.

## 2.1 Limitations
Several functions accept IDs for so called "resources", which normally get linked
in with the object code by the resource compiler (when writing in C). For
obviously reasons this is not possible when using Lisp.

## 2.2 Lispy CLOS based interface
It would be nice to have a more Lispy CLOS based interface where you
can define classes, methods etc. This should be provided by a higher level
library and does not belong here.

## 2.3 Other platforms
This is a Windows only library and does not work on any other platform.
It is not a cross platform GUI library.

# 3. Examples
Various examples are provided which show various levels of abstractions and a
good showcase of how to use it.

## 3.1 Zetcode samples
The rather comprehensive tutorial for the C programming language can be
found here [zetcode website](http://zetcode.com/gui/winapi/).
These have been translated to Lisp and show that the same GUIs can be written
which correspond to largely the same structure.

## 3.2 Climage
This example GUI displays a two list boxes which show the packages and
exported symbols. Clicking on a symbol displays the documentation for it.

In addition, this GUI shows how to write and handle modal dialogs
and accelerator keys -- these are the keyboard combinations which
are used as shortcuts for menu items.
Ctrl+F brings up a Find dialog to search for a given symbol. Ctrl+Q quits.

## 3.3 Dragdrop
This shows how to support drag and drop functionality by handling the WM_DROPFILES message.

## 3.4 Pong
This is a small and not very well written example of how you might go about
writing games. It's just a silly little pong game but shows the basic idea.

# 4. Notes
Requires CFFI. Developed on Windows 8.1 and Windows 7 but should work on
basically any version because all the APIs are pretty stable and haven't changed
for a long time.

Licensed under the terms of the MIT license.

Frank James
October 2016.





