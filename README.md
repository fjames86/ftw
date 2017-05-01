
# FTW - Common Lisp For the Win(32) 

# 1. Introduction 
This library provides a very thin interface to the underlying
APIs for writing native Windows GUIs in Common Lisp.

The intention is to be able to write the same sort of codes in Lisp as you
would if writing normal Win32 GUIs in C. This also opens the possibility
for writing other more general graphical applications like games.

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

## 2.2 Other platforms
This is a Windows only library and does not work on any other platform.
It is not a cross platform GUI library.


# 3. Extra utilities
Several extra functions and macros are provided which the author has found useful.
These are found in ftw.lisp.

## 3.1 Constants 
To use the Win32 API you need access to a vast number of predefined constants. 
These are defined in constants.lisp. Rather than export each of these symbols from 
the FTW package the programmer has two options: either access directly 
or use the macros `const` or `logior-consts`:
```
;; directly 
ftw::+fred+
(logior ftw::+fred+ ftw::+jim+)
;; sugar coating macro 
(ftw:const +fred+)
(ftw:logior-consts +fred+ +jim+)
```

The macro `CONST` takes a string designator and converts to the symbol with that name in the `FTW` package. 
In many places you need to pass a bitmask of logical-OR of several flags, use `LOGIOR-CONSTS` for this
which performs the same transformation. 

Note that there are possibly many constants which have not been defined in constants.lisp. These should be added over time as they become useful.

## 3.2 Resources 
When writing Win32 programs in the C programming language it is common to embed binary resources such 
as icons, cursors and bitmaps using the resource compiler. These can then be referenced by an integer 
ID from various Win32 calls. This is not possible when calling these functions at from Lisp because 
we have to do everthing at runtime. Where possible I have included the functions for generating 
these at runtime either by loading from files or from raw binary data. 

To make it easier I have also included several functions for pregenerating Lisp code for icons, cursors and 
bitmaps. This has the equivalent semantics as the normal Win32 resource compiler but we're still doing 
all the work at runtime. 

The advantage of pregenerating code and putting that into your project is you don't need to ship 
external images which need to be loaded at runtime - you need only compile your code. 

To e.g. embed an icon into your project do the following: 
 1. Get your icon file e.g. by drawing it in gimp. make sure it is 32x32 pixels and exported as 32-bit
with 8 bits each of alpha and rgb. 
 2. Run `(ftw:generate-icon-resource "myicon.ico")`
This will print out the code you need to paste into your project. 

See the minesweeper example of how you can have a custom icon without shipping the file separately. 

## 3.3 Dialogs
The standard mechanism for drawing modal and modeless dialogs with Win32 is to use the 
resource compiler to generate the specification. This is not possible for us so we must do it at runtime.

The functions `DIALOG-BOX` and `CREATE-DIALOG` create modal and modeless dialogs respectively. Both accept 
the same inputs. The difference is that modal dialogs do not return control to the caller until 
the dialog has been closed whereas modeless dialogs return control immediately and run alongside the original
window. 

## Hwnd registry
You may associate a window handle (hwnd) with a symbol name and optionally integer ID using `ADD-HWND`. Perform lookups by name or ID using `HWND-BY-NAME` and `HWND-BY-ID`:
```
(add-hwnd 'fred hwnd 1)
(hwnd-by-name 'fred)
(hwnd-by-id 1)
(hwnd-name-by-id 1)
```

This makes it very simple to keep references to window handles in a consistent
way rather than implementing private lists or globals in each program.

# 4. Examples
Various examples are provided which show various levels of abstractions and a
good showcase of how to use it.

## 4.1 Zetcode samples
The rather comprehensive tutorial for the C programming language can be
found here [zetcode website](http://zetcode.com/gui/winapi/).
These have been translated to Lisp and show that the same GUIs can be written
which correspond to largely the same structure.

## 4.2 Climage
This example GUI displays a two list boxes which show the packages and
exported symbols. Clicking on a symbol displays the documentation for it.

In addition, this GUI shows how to write and handle modal dialogs
and accelerator keys -- these are the keyboard combinations which
are used as shortcuts for menu items.
Ctrl+F brings up a Find dialog to search for a given symbol. Ctrl+Q quits.

## 4.3 Dragdrop
This shows how to support drag and drop functionality by handling the `WM_DROPFILES` message.

## 4.4 Pong
This is a small and not very well written example of how you might go about
writing games. It's just a silly little pong game but shows the basic idea.

## 4.5 Icon
Shows how to add icons and other graphics.

## 4.6 Minesweeper
Simple minesweeper game.

## 4.7 Tetris
Simple tetris clone.

## 4.8 Macroman
Simple pacman clone. Shows how to reduce flicker by double buffering. 

## 4.9 Scrollbar 
How to add scrollbars and response to scoll messages. 

## 4.10 Dragons: DNS client
This implements a simple DNS client using the DNS client [dragons](http://github.com/fjames86/dragons). Enter the DNS address in the IP address field, select the
record type and entry name and click Query. The list box below is filled with
the results returned from the server, or a message box indicates an error status.

## 4.11 RPC: MsgWaitForMultipleObjects example
This shows how to interleave networking and the message pump in the main thread,
thereby making it possible to do asynchronous processing without blocking the
gui. The example broadcasts to the rpcbinf null procedure and fills in results
as they are received. This means the gui never blocks. The same technique can
be applied to do background refreshes of data. 

# 5. Notes
Requires CFFI. Developed on Windows 8.1 and Windows 7 using SBCL 
but should work on basically any Windows version because all the APIs are 
pretty stable and haven't changed for a long time. 
Should work with any Lisp implementation which provides FFI callbacks

## 5.1 TODO
 - [ ] Try with CCL, Lispworks etc.
 - [ ] Better error handling.


Licensed under the terms of the MIT license.

Frank James
October 2016.





