;;; Output by lambda-gtk on 31.12.2004 at 13:58:11
;;; This software is released under the Lisp Lesser Gnu Public License
;;; (LLGPL). See http://opensource.franz.com/preamble.html for the
;;; terms of this agreement.

;;;
;;; Gtk lib loading. *gtk-libdir* must point to where GTK's libs are
;;; located and *gtk-libfiles* better agree with whatever the unix
;;; command 'pkg-config gtk+-2.0 --libs' returns on your system.

(in-package :cl-user)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(*gtk-libdir* *gtk-libfiles*) :cl-user)
  (defvar *gtk-libdir* #+:darwin "/sw/lib/" #-:darwin "/usr/lib/")
  (defvar *gtk-libfiles* 
    '("libgtk-x11-2.0" "libgdk-x11-2.0" "libatk-1.0"
      "libgdk_pixbuf-2.0"
      #-:darwin "libm" #+:darwin "/usr/lib/libm"
      "libpangoxft-1.0" "libpangox-1.0" "libpango-1.0"
      "libgobject-2.0" "libgmodule-2.0"
      #-:darwin "libdl"
      "libglib-2.0"
      #+:darwin "libintl"
      #+:darwin "libiconv"
     ))
  (flet ((libpath (lib &aux p)
           (setq p (namestring
                     (merge-pathnames (format nil "~A.~A"
                                              lib
                                              #+:darwin "dylib"
                                              #-:darwin "so")
                                      *gtk-libdir*)))
           (if (probe-file p)
             p
             (error "Library ~S not found. Either GTK is not installed or else cl-user:*gtk-libdir* needs to be set to the directory containing GTK on your machine." p))))
  #+:sbcl
  (dolist (l *gtk-libfiles*) (load-shared-object (libpath l)))
  #+:cmu
  (dolist (l *gtk-libfiles*) (ext:load-foreign (libpath l)))))

;;; begin generated output

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage :gtk
              (:use #+:sbcl
                    :sb-alien
                    #+:cmu
                    :alien
                    #+:cmu
                    :c-call
                    :common-lisp)
              (:shadow :true :false :fill))
  (defpackage :gdk
              (:use #+:sbcl
                    :sb-alien
                    #+:cmu
                    :alien
                    #+:cmu
                    :c-call
                    :common-lisp)
              (:shadow :copy
                       :invert
                       :xor
                       :clear
                       :and
                       :or
                       :set
                       :delete
                       :plus
                       :mouse
                       :destroy
                       :map
                       :unmap
                       :scroll))
  (defpackage :g
              (:use #+:sbcl
                    :sb-alien
                    #+:cmu
                    :alien
                    #+:cmu
                    :c-call
                    :common-lisp)
              #+:cmu
              (:shadow :callback))
  (defpackage :atk
              (:use #+:sbcl
                    :sb-alien
                    #+:cmu
                    :alien
                    #+:cmu
                    :c-call
                    :common-lisp))
  (defpackage :pango
              (:use #+:sbcl
                    :sb-alien
                    #+:cmu
                    :alien
                    #+:cmu
                    :c-call
                    :common-lisp)
              (:shadow :break)))

;; make :gtk the default package.
(in-package :gtk)

;;; GTK glue code

(declaim #+:cmu
         (optimize (extensions:inhibit-warnings 3))
         #+:sbcl
         (sb-ext:muffle-conditions style-warning sb-ext:compiler-note))
(defconstant gtk::+false+ 0)
(defconstant gtk::+true+ 1)
(defun g::nullptr () #+:sbcl (sb-sys:int-sap 0) #+:cmu  (system:int-sap 0))
(defun g::nullptr? (p)
  (if (typep p '(alien (* t)))
      ;; then
      #+:sbcl (null-alien p)
      #+:cmu  (zerop (system:sap-int (alien-sap p)))
      ;; else
      #+:sbcl (zerop (sb-sys:sap-int p))
      #+:cmu  (zerop (system:sap-int p))))
(defvar *gtk-init* nil)
(defun gtk::init-ensure (&optional strings)
  (declare (ignore strings))
  (unless *gtk-init* (gtk::init 0 (g::nullptr)) (setq *gtk-init* t))
  *gtk-init*)
(defmacro defalien (name return &rest args)
  (let* ((l (if (consp name) name (list name (intern name)))) (s (second l)))
    #+:sbcl
    `(progn (declaim (inline ,s)) (define-alien-routine ,l ,return ,@args))
    #+:cmu
    `(progn (declaim (inline ,s)) (def-alien-routine ,l ,return ,@args))))
(defmacro defalien-type (name type)
  #+:sbcl
  `(define-alien-type ,name ,type)
  #+:cmu
  `(def-alien-type ,name ,type))
(defmacro struct-alloc (type &rest inits)
  (let ((inst (gensym)))
    `(let ((,inst (make-alien ,(parse-alien-type type))))
       ,@(loop for
               (a b)
               on
               inits
               by
               #'cddr
               collect
               `(setf (slot ,inst ',(parse-alien-type a)) ,b))
       ,inst)))
(defun struct-free (x) (free-alien x))
(defun cstring->string (str) (error "Fix me: not yet implemented."))
(defun string->cstring (str)
  (if (= (length str) 0)
      (sap-alien #+:cmu
                 (system:int-sap 0)
                 #+:sbcl
                 (sb-sys:int-sap 0)
                 (* (unsigned 8)))
      (let* ((size (length str))
             (cstr (make-alien (unsigned 8) (1+ size)))
             (str* (cast cstr (* (unsigned 8)))))
        (dotimes (i size) (setf (deref cstr i) (char-code (char str i))))
        (setf (deref cstr size) 0)
        str*)))
(defun cstring-free (gstr) gstr (values))
(defun parse-alien-type (k)
  (cond
   ((consp k) (cons (parse-alien-type (car k)) (parse-alien-type (cdr k))))
   ((eql k t) t)
   ((keywordp k)
    (let ((s (string k)))
      (if (find #\< s)
          (intern (delete-if (lambda (x) (member x '(#\< #\>))) s) :gtk)
          (intern s :gtk))))
   ((symbolp k)
    (if (eql (symbol-package k) (find-package :gtk))
        k
        (intern (string k) :gtk)))
   (t (error "Not an alien type: ~S." k))))
(defmacro define-signal-handler (name return params &body body)
  (let ((args
         (loop for
               p
               in
               params
               collect
               (if (consp p)
                   (list (first p) (parse-alien-type (second p)))
                   `(,p (* t)))))
        (type (parse-alien-type return)))
    #+:sbcl
    `(define-alien-function ,name (,type ,@args) ,@body)
    #+:cmu
    `(def-callback ,name (,type ,@args) ,@body)))
(defmacro g::callback (x)
  #+:sbcl
  `(alien-function-sap ,x)
  #+:cmu `(alien::callback ,x))
(defun g::signal-connect (instance detailed-signal c-handler data)
  (g::signal-connect-data instance detailed-signal c-handler data (g::nullptr)
   0))
(defun g::signal-connect-after (instance detailed-signal c-handler data)
  (g::signal-connect-data instance detailed-signal c-handler data (g::nullptr)
   1))
(defun g::signal-connect-swapped (instance detailed_signal c_handler data)
  (g::signal-connect-data instance detailed_signal c_handler data (g::nullptr)
   2))

;;; Constants

(defconstant pango::coverage-none 0)
(defconstant pango::coverage-fallback 1)
(defconstant pango::coverage-approximate 2)
(defconstant pango::coverage-exact 3)
(defconstant pango::direction-ltr 0)
(defconstant pango::direction-rtl 1)
(defconstant pango::direction-ttb-ltr 2)
(defconstant pango::direction-ttb-rtl 3)
(defconstant pango::style-normal 0)
(defconstant pango::style-oblique 1)
(defconstant pango::style-italic 2)
(defconstant pango::variant-normal 0)
(defconstant pango::variant-small-caps 1)
(defconstant pango::weight-ultralight 200)
(defconstant pango::weight-light 300)
(defconstant pango::weight-normal 400)
(defconstant pango::weight-bold 700)
(defconstant pango::weight-ultrabold 800)
(defconstant pango::weight-heavy 900)
(defconstant pango::stretch-ultra-condensed 0)
(defconstant pango::stretch-extra-condensed 1)
(defconstant pango::stretch-condensed 2)
(defconstant pango::stretch-semi-condensed 3)
(defconstant pango::stretch-normal 4)
(defconstant pango::stretch-semi-expanded 5)
(defconstant pango::stretch-expanded 6)
(defconstant pango::stretch-extra-expanded 7)
(defconstant pango::stretch-ultra-expanded 8)
(defconstant pango::font-mask-family 1)
(defconstant pango::font-mask-style 2)
(defconstant pango::font-mask-variant 4)
(defconstant pango::font-mask-weight 8)
(defconstant pango::font-mask-stretch 16)
(defconstant pango::font-mask-size 32)
(defconstant pango::attr-invalid 0)
(defconstant pango::attr-language 1)
(defconstant pango::attr-family 2)
(defconstant pango::attr-style 3)
(defconstant pango::attr-weight 4)
(defconstant pango::attr-variant 5)
(defconstant pango::attr-stretch 6)
(defconstant pango::attr-size 7)
(defconstant pango::attr-font-desc 8)
(defconstant pango::attr-foreground 9)
(defconstant pango::attr-background 10)
(defconstant pango::attr-underline 11)
(defconstant pango::attr-strikethrough 12)
(defconstant pango::attr-rise 13)
(defconstant pango::attr-shape 14)
(defconstant pango::attr-scale 15)
(defconstant pango::underline-none 0)
(defconstant pango::underline-single 1)
(defconstant pango::underline-double 2)
(defconstant pango::underline-low 3)
(defconstant pango::tab-left 0)
(defconstant pango::align-left 0)
(defconstant pango::align-center 1)
(defconstant pango::align-right 2)
(defconstant pango::wrap-word 0)
(defconstant pango::wrap-char 1)
(defconstant pango::wrap-word-char 2)
(defconstant gdk::lsb-first 0)
(defconstant gdk::msb-first 1)
(defconstant gdk::shift-mask 1)
(defconstant gdk::lock-mask 2)
(defconstant gdk::control-mask 4)
(defconstant gdk::mod1-mask 8)
(defconstant gdk::mod2-mask 16)
(defconstant gdk::mod3-mask 32)
(defconstant gdk::mod4-mask 64)
(defconstant gdk::mod5-mask 128)
(defconstant gdk::button1-mask 256)
(defconstant gdk::button2-mask 512)
(defconstant gdk::button3-mask 1024)
(defconstant gdk::button4-mask 2048)
(defconstant gdk::button5-mask 4096)
(defconstant gdk::release-mask 1073741824)
(defconstant gdk::modifier-mask 1073750015)
(defconstant gdk::input-read 1)
(defconstant gdk::input-write 2)
(defconstant gdk::input-exception 4)
(defconstant gdk::grab-success 0)
(defconstant gdk::grab-already-grabbed 1)
(defconstant gdk::grab-invalid-time 2)
(defconstant gdk::grab-not-viewable 3)
(defconstant gdk::grab-frozen 4)
(defconstant gdk::x-cursor 0)
(defconstant gdk::arrow 2)
(defconstant gdk::based-arrow-down 4)
(defconstant gdk::based-arrow-up 6)
(defconstant gdk::boat 8)
(defconstant gdk::bogosity 10)
(defconstant gdk::bottom-left-corner 12)
(defconstant gdk::bottom-right-corner 14)
(defconstant gdk::bottom-side 16)
(defconstant gdk::bottom-tee 18)
(defconstant gdk::box-spiral 20)
(defconstant gdk::center-ptr 22)
(defconstant gdk::circle 24)
(defconstant gdk::clock 26)
(defconstant gdk::coffee-mug 28)
(defconstant gdk::cross 30)
(defconstant gdk::cross-reverse 32)
(defconstant gdk::crosshair 34)
(defconstant gdk::diamond-cross 36)
(defconstant gdk::dot 38)
(defconstant gdk::dotbox 40)
(defconstant gdk::double-arrow 42)
(defconstant gdk::draft-large 44)
(defconstant gdk::draft-small 46)
(defconstant gdk::draped-box 48)
(defconstant gdk::exchange 50)
(defconstant gdk::fleur 52)
(defconstant gdk::gobbler 54)
(defconstant gdk::gumby 56)
(defconstant gdk::hand1 58)
(defconstant gdk::hand2 60)
(defconstant gdk::heart 62)
(defconstant gdk::icon 64)
(defconstant gdk::iron-cross 66)
(defconstant gdk::left-ptr 68)
(defconstant gdk::left-side 70)
(defconstant gdk::left-tee 72)
(defconstant gdk::leftbutton 74)
(defconstant gdk::ll-angle 76)
(defconstant gdk::lr-angle 78)
(defconstant gdk::man 80)
(defconstant gdk::middlebutton 82)
(defconstant gdk::mouse 84)
(defconstant gdk::pencil 86)
(defconstant gdk::pirate 88)
(defconstant gdk::plus 90)
(defconstant gdk::question-arrow 92)
(defconstant gdk::right-ptr 94)
(defconstant gdk::right-side 96)
(defconstant gdk::right-tee 98)
(defconstant gdk::rightbutton 100)
(defconstant gdk::rtl-logo 102)
(defconstant gdk::sailboat 104)
(defconstant gdk::sb-down-arrow 106)
(defconstant gdk::sb-h-double-arrow 108)
(defconstant gdk::sb-left-arrow 110)
(defconstant gdk::sb-right-arrow 112)
(defconstant gdk::sb-up-arrow 114)
(defconstant gdk::sb-v-double-arrow 116)
(defconstant gdk::shuttle 118)
(defconstant gdk::sizing 120)
(defconstant gdk::spider 122)
(defconstant gdk::spraycan 124)
(defconstant gdk::star 126)
(defconstant gdk::target 128)
(defconstant gdk::tcross 130)
(defconstant gdk::top-left-arrow 132)
(defconstant gdk::top-left-corner 134)
(defconstant gdk::top-right-corner 136)
(defconstant gdk::top-side 138)
(defconstant gdk::top-tee 140)
(defconstant gdk::trek 142)
(defconstant gdk::ul-angle 144)
(defconstant gdk::umbrella 146)
(defconstant gdk::ur-angle 148)
(defconstant gdk::watch 150)
(defconstant gdk::xterm 152)
(defconstant gdk::last-cursor 153)
(defconstant gdk::cursor-is-pixmap -1)
(defconstant gdk::action-default 1)
(defconstant gdk::action-copy 2)
(defconstant gdk::action-move 4)
(defconstant gdk::action-link 8)
(defconstant gdk::action-private 16)
(defconstant gdk::action-ask 32)
(defconstant gdk::drag-proto-motif 0)
(defconstant gdk::drag-proto-xdnd 1)
(defconstant gdk::drag-proto-rootwin 2)
(defconstant gdk::drag-proto-none 3)
(defconstant gdk::drag-proto-win32-dropfiles 4)
(defconstant gdk::drag-proto-ole2 5)
(defconstant gdk::drag-proto-local 6)
(defconstant gdk::extension-events-none 0)
(defconstant gdk::extension-events-all 1)
(defconstant gdk::extension-events-cursor 2)
(defconstant gdk::source-mouse 0)
(defconstant gdk::source-pen 1)
(defconstant gdk::source-eraser 2)
(defconstant gdk::source-cursor 3)
(defconstant gdk::mode-disabled 0)
(defconstant gdk::mode-screen 1)
(defconstant gdk::mode-window 2)
(defconstant gdk::axis-ignore 0)
(defconstant gdk::axis-x 1)
(defconstant gdk::axis-y 2)
(defconstant gdk::axis-pressure 3)
(defconstant gdk::axis-xtilt 4)
(defconstant gdk::axis-ytilt 5)
(defconstant gdk::axis-wheel 6)
(defconstant gdk::axis-last 7)
(defconstant gdk::nothing -1)
(defconstant gdk::delete 0)
(defconstant gdk::destroy 1)
(defconstant gdk::expose 2)
(defconstant gdk::motion-notify 3)
(defconstant gdk::button-press 4)
(defconstant gdk::2button-press 5)
(defconstant gdk::3button-press 6)
(defconstant gdk::button-release 7)
(defconstant gdk::key-press 8)
(defconstant gdk::key-release 9)
(defconstant gdk::enter-notify 10)
(defconstant gdk::leave-notify 11)
(defconstant gdk::focus-change 12)
(defconstant gdk::configure 13)
(defconstant gdk::map 14)
(defconstant gdk::unmap 15)
(defconstant gdk::property-notify 16)
(defconstant gdk::selection-clear 17)
(defconstant gdk::selection-request 18)
(defconstant gdk::selection-notify 19)
(defconstant gdk::proximity-in 20)
(defconstant gdk::proximity-out 21)
(defconstant gdk::drag-enter 22)
(defconstant gdk::drag-leave 23)
(defconstant gdk::drag-motion 24)
(defconstant gdk::drag-status 25)
(defconstant gdk::drop-start 26)
(defconstant gdk::drop-finished 27)
(defconstant gdk::client-event 28)
(defconstant gdk::visibility-notify 29)
(defconstant gdk::no-expose 30)
(defconstant gdk::scroll 31)
(defconstant gdk::window-state 32)
(defconstant gdk::setting 33)
(defconstant gdk::exposure-mask 2)
(defconstant gdk::pointer-motion-mask 4)
(defconstant gdk::pointer-motion-hint-mask 8)
(defconstant gdk::button-motion-mask 16)
(defconstant gdk::button1-motion-mask 32)
(defconstant gdk::button2-motion-mask 64)
(defconstant gdk::button3-motion-mask 128)
(defconstant gdk::button-press-mask 256)
(defconstant gdk::button-release-mask 512)
(defconstant gdk::key-press-mask 1024)
(defconstant gdk::key-release-mask 2048)
(defconstant gdk::enter-notify-mask 4096)
(defconstant gdk::leave-notify-mask 8192)
(defconstant gdk::focus-change-mask 16384)
(defconstant gdk::structure-mask 32768)
(defconstant gdk::property-change-mask 65536)
(defconstant gdk::visibility-notify-mask 131072)
(defconstant gdk::proximity-in-mask 262144)
(defconstant gdk::proximity-out-mask 524288)
(defconstant gdk::substructure-mask 1048576)
(defconstant gdk::scroll-mask 2097152)
(defconstant gdk::all-events-mask 4194302)
(defconstant gdk::window-state-withdrawn 1)
(defconstant gdk::window-state-iconified 2)
(defconstant gdk::window-state-maximized 4)
(defconstant gdk::window-state-sticky 8)
(defconstant gdk::window-state-fullscreen 16)
(defconstant gdk::cap-not-last 0)
(defconstant gdk::cap-butt 1)
(defconstant gdk::cap-round 2)
(defconstant gdk::cap-projecting 3)
(defconstant gdk::solid 0)
(defconstant gdk::tiled 1)
(defconstant gdk::stippled 2)
(defconstant gdk::opaque-stippled 3)
(defconstant gdk::copy 0)
(defconstant gdk::invert 1)
(defconstant gdk::xor 2)
(defconstant gdk::clear 3)
(defconstant gdk::and 4)
(defconstant gdk::and-reverse 5)
(defconstant gdk::and-invert 6)
(defconstant gdk::noop 7)
(defconstant gdk::or 8)
(defconstant gdk::equiv 9)
(defconstant gdk::or-reverse 10)
(defconstant gdk::copy-invert 11)
(defconstant gdk::or-invert 12)
(defconstant gdk::nand 13)
(defconstant gdk::nor 14)
(defconstant gdk::set 15)
(defconstant gdk::join-miter 0)
(defconstant gdk::join-round 1)
(defconstant gdk::join-bevel 2)
(defconstant gdk::line-solid 0)
(defconstant gdk::line-on-off-dash 1)
(defconstant gdk::line-double-dash 2)
(defconstant gdk::clip-by-children 0)
(defconstant gdk::include-inferiors 1)
(defconstant gdk::gc-foreground 1)
(defconstant gdk::gc-background 2)
(defconstant gdk::gc-font 4)
(defconstant gdk::gc-function 8)
(defconstant gdk::gc-fill 16)
(defconstant gdk::gc-tile 32)
(defconstant gdk::gc-stipple 64)
(defconstant gdk::gc-clip-mask 128)
(defconstant gdk::gc-subwindow 256)
(defconstant gdk::gc-ts-x-origin 512)
(defconstant gdk::gc-ts-y-origin 1024)
(defconstant gdk::gc-clip-x-origin 2048)
(defconstant gdk::gc-clip-y-origin 4096)
(defconstant gdk::gc-exposures 8192)
(defconstant gdk::gc-line-width 16384)
(defconstant gdk::gc-line-style 32768)
(defconstant gdk::gc-cap-style 65536)
(defconstant gdk::gc-join-style 131072)
(defconstant gdk::rgb-dither-none 0)
(defconstant gdk::rgb-dither-normal 1)
(defconstant gdk::rgb-dither-max 2)
(defconstant gdk::pixbuf-alpha-bilevel 0)
(defconstant gdk::pixbuf-alpha-full 1)
(defconstant gdk::colorspace-rgb 0)
(defconstant gdk::interp-nearest 0)
(defconstant gdk::interp-tiles 1)
(defconstant gdk::interp-bilinear 2)
(defconstant gdk::interp-hyper 3)
(defconstant gdk::font-font 0)
(defconstant gdk::font-fontset 1)
(defconstant gdk::image-normal 0)
(defconstant gdk::image-shared 1)
(defconstant gdk::image-fastest 2)
(defconstant gdk::prop-mode-replace 0)
(defconstant gdk::prop-mode-prepend 1)
(defconstant gdk::prop-mode-append 2)
(defconstant gdk::visual-static-gray 0)
(defconstant gdk::visual-grayscale 1)
(defconstant gdk::visual-static-color 2)
(defconstant gdk::visual-pseudo-color 3)
(defconstant gdk::visual-true-color 4)
(defconstant gdk::visual-direct-color 5)
(defconstant gdk::window-root 0)
(defconstant gdk::window-toplevel 1)
(defconstant gdk::window-child 2)
(defconstant gdk::window-dialog 3)
(defconstant gdk::window-temp 4)
(defconstant gdk::window-foreign 5)
(defconstant gdk::hint-pos 1)
(defconstant gdk::hint-min-size 2)
(defconstant gdk::hint-max-size 4)
(defconstant gdk::hint-base-size 8)
(defconstant gdk::hint-aspect 16)
(defconstant gdk::hint-resize-inc 32)
(defconstant gdk::hint-win-gravity 64)
(defconstant gdk::hint-user-pos 128)
(defconstant gdk::hint-user-size 256)
(defconstant gdk::window-type-hint-normal 0)
(defconstant gdk::window-type-hint-dialog 1)
(defconstant gdk::window-type-hint-menu 2)
(defconstant gdk::window-type-hint-toolbar 3)
(defconstant gdk::window-type-hint-splashscreen 4)
(defconstant gdk::window-type-hint-utility 5)
(defconstant gdk::window-type-hint-dock 6)
(defconstant gdk::window-type-hint-desktop 7)
(defconstant gdk::decor-all 1)
(defconstant gdk::decor-border 2)
(defconstant gdk::decor-resizeh 4)
(defconstant gdk::decor-title 8)
(defconstant gdk::decor-menu 16)
(defconstant gdk::decor-minimize 32)
(defconstant gdk::decor-maximize 64)
(defconstant gdk::func-all 1)
(defconstant gdk::func-resize 2)
(defconstant gdk::func-move 4)
(defconstant gdk::func-minimize 8)
(defconstant gdk::func-maximize 16)
(defconstant gdk::func-close 32)
(defconstant gdk::gravity-north-west 1)
(defconstant gdk::gravity-north 2)
(defconstant gdk::gravity-north-east 3)
(defconstant gdk::gravity-west 4)
(defconstant gdk::gravity-center 5)
(defconstant gdk::gravity-east 6)
(defconstant gdk::gravity-south-west 7)
(defconstant gdk::gravity-south 8)
(defconstant gdk::gravity-south-east 9)
(defconstant gdk::gravity-static 10)
(defconstant gdk::window-edge-north-west 0)
(defconstant gdk::window-edge-north 1)
(defconstant gdk::window-edge-north-east 2)
(defconstant gdk::window-edge-west 3)
(defconstant gdk::window-edge-east 4)
(defconstant gdk::window-edge-south-west 5)
(defconstant gdk::window-edge-south 6)
(defconstant gdk::window-edge-south-east 7)
(defconstant gtk::arrow-up 0)
(defconstant gtk::arrow-down 1)
(defconstant gtk::arrow-left 2)
(defconstant gtk::arrow-right 3)
(defconstant gtk::expand 1)
(defconstant gtk::shrink 2)
(defconstant gtk::fill 4)
(defconstant gtk::buttonbox-default-style 0)
(defconstant gtk::buttonbox-spread 1)
(defconstant gtk::buttonbox-edge 2)
(defconstant gtk::buttonbox-start 3)
(defconstant gtk::buttonbox-end 4)
(defconstant gtk::curve-type-linear 0)
(defconstant gtk::curve-type-spline 1)
(defconstant gtk::curve-type-free 2)
(defconstant gtk::dir-tab-forward 0)
(defconstant gtk::dir-tab-backward 1)
(defconstant gtk::dir-up 2)
(defconstant gtk::dir-down 3)
(defconstant gtk::dir-left 4)
(defconstant gtk::dir-right 5)
(defconstant gtk::expander-collapsed 0)
(defconstant gtk::expander-semi-collapsed 1)
(defconstant gtk::expander-semi-expanded 2)
(defconstant gtk::expander-expanded 3)
(defconstant gtk::icon-size-invalid 0)
(defconstant gtk::icon-size-menu 1)
(defconstant gtk::icon-size-small-toolbar 2)
(defconstant gtk::icon-size-large-toolbar 3)
(defconstant gtk::icon-size-button 4)
(defconstant gtk::icon-size-dnd 5)
(defconstant gtk::icon-size-dialog 6)
(defconstant gtk::text-dir-none 0)
(defconstant gtk::text-dir-ltr 1)
(defconstant gtk::text-dir-rtl 2)
(defconstant gtk::justify-left 0)
(defconstant gtk::justify-right 1)
(defconstant gtk::justify-center 2)
(defconstant gtk::justify-fill 3)
(defconstant gtk::pixels 0)
(defconstant gtk::inches 1)
(defconstant gtk::centimeters 2)
(defconstant gtk::orientation-horizontal 0)
(defconstant gtk::orientation-vertical 1)
(defconstant gtk::corner-top-left 0)
(defconstant gtk::corner-bottom-left 1)
(defconstant gtk::corner-top-right 2)
(defconstant gtk::corner-bottom-right 3)
(defconstant gtk::pack-start 0)
(defconstant gtk::pack-end 1)
(defconstant gtk::path-prio-lowest 0)
(defconstant gtk::path-prio-gtk 4)
(defconstant gtk::path-prio-application 8)
(defconstant gtk::path-prio-theme 10)
(defconstant gtk::path-prio-rc 12)
(defconstant gtk::path-prio-highest 15)
(defconstant gtk::path-widget 0)
(defconstant gtk::path-widget-class 1)
(defconstant gtk::path-class 2)
(defconstant gtk::policy-always 0)
(defconstant gtk::policy-automatic 1)
(defconstant gtk::policy-never 2)
(defconstant gtk::pos-left 0)
(defconstant gtk::pos-right 1)
(defconstant gtk::pos-top 2)
(defconstant gtk::pos-bottom 3)
(defconstant gtk::relief-normal 0)
(defconstant gtk::relief-half 1)
(defconstant gtk::relief-none 2)
(defconstant gtk::resize-parent 0)
(defconstant gtk::resize-queue 1)
(defconstant gtk::resize-immediate 2)
(defconstant gtk::selection-none 0)
(defconstant gtk::selection-single 1)
(defconstant gtk::selection-browse 2)
(defconstant gtk::selection-multiple 3)
(defconstant gtk::selection-extended 3)
(defconstant gtk::shadow-none 0)
(defconstant gtk::shadow-in 1)
(defconstant gtk::shadow-out 2)
(defconstant gtk::shadow-etched-in 3)
(defconstant gtk::shadow-etched-out 4)
(defconstant gtk::state-normal 0)
(defconstant gtk::state-active 1)
(defconstant gtk::state-prelight 2)
(defconstant gtk::state-selected 3)
(defconstant gtk::state-insensitive 4)
(defconstant gtk::toolbar-icons 0)
(defconstant gtk::toolbar-text 1)
(defconstant gtk::toolbar-both 2)
(defconstant gtk::toolbar-both-horiz 3)
(defconstant gtk::update-continuous 0)
(defconstant gtk::update-discontinuous 1)
(defconstant gtk::update-delayed 2)
(defconstant gtk::visibility-none 0)
(defconstant gtk::visibility-partial 1)
(defconstant gtk::visibility-full 2)
(defconstant gtk::win-pos-none 0)
(defconstant gtk::win-pos-center 1)
(defconstant gtk::win-pos-mouse 2)
(defconstant gtk::win-pos-center-always 3)
(defconstant gtk::win-pos-center-on-parent 4)
(defconstant gtk::window-toplevel 0)
(defconstant gtk::window-popup 1)
(defconstant gtk::wrap-none 0)
(defconstant gtk::wrap-char 1)
(defconstant gtk::wrap-word 2)
(defconstant gtk::sort-ascending 0)
(defconstant gtk::sort-descending 1)
(defconstant gtk::accel-visible 1)
(defconstant gtk::accel-locked 2)
(defconstant gtk::accel-mask 7)
(defconstant atk::state-invalid 0)
(defconstant atk::state-active 1)
(defconstant atk::state-armed 2)
(defconstant atk::state-busy 3)
(defconstant atk::state-checked 4)
(defconstant atk::state-defunct 5)
(defconstant atk::state-editable 6)
(defconstant atk::state-enabled 7)
(defconstant atk::state-expandable 8)
(defconstant atk::state-expanded 9)
(defconstant atk::state-focusable 10)
(defconstant atk::state-focused 11)
(defconstant atk::state-horizontal 12)
(defconstant atk::state-iconified 13)
(defconstant atk::state-modal 14)
(defconstant atk::state-multi-line 15)
(defconstant atk::state-multiselectable 16)
(defconstant atk::state-opaque 17)
(defconstant atk::state-pressed 18)
(defconstant atk::state-resizable 19)
(defconstant atk::state-selectable 20)
(defconstant atk::state-selected 21)
(defconstant atk::state-sensitive 22)
(defconstant atk::state-showing 23)
(defconstant atk::state-single-line 24)
(defconstant atk::state-stale 25)
(defconstant atk::state-transient 26)
(defconstant atk::state-vertical 27)
(defconstant atk::state-visible 28)
(defconstant atk::state-manages-descendants 29)
(defconstant atk::state-indeterminate 30)
(defconstant atk::state-last-defined 31)
(defconstant atk::relation-null 0)
(defconstant atk::relation-controlled-by 1)
(defconstant atk::relation-controller-for 2)
(defconstant atk::relation-label-for 3)
(defconstant atk::relation-labelled-by 4)
(defconstant atk::relation-member-of 5)
(defconstant atk::relation-node-child-of 6)
(defconstant atk::relation-flows-to 7)
(defconstant atk::relation-flows-from 8)
(defconstant atk::relation-subwindow-of 9)
(defconstant atk::relation-embeds 10)
(defconstant atk::relation-embedded-by 11)
(defconstant atk::relation-popup-for 12)
(defconstant atk::relation-last-defined 13)
(defconstant atk::role-invalid 0)
(defconstant atk::role-accel-label 1)
(defconstant atk::role-alert 2)
(defconstant atk::role-animation 3)
(defconstant atk::role-arrow 4)
(defconstant atk::role-calendar 5)
(defconstant atk::role-canvas 6)
(defconstant atk::role-check-box 7)
(defconstant atk::role-check-menu-item 8)
(defconstant atk::role-color-chooser 9)
(defconstant atk::role-column-header 10)
(defconstant atk::role-combo-box 11)
(defconstant atk::role-date-editor 12)
(defconstant atk::role-desktop-icon 13)
(defconstant atk::role-desktop-frame 14)
(defconstant atk::role-dial 15)
(defconstant atk::role-dialog 16)
(defconstant atk::role-directory-pane 17)
(defconstant atk::role-drawing-area 18)
(defconstant atk::role-file-chooser 19)
(defconstant atk::role-filler 20)
(defconstant atk::role-font-chooser 21)
(defconstant atk::role-frame 22)
(defconstant atk::role-glass-pane 23)
(defconstant atk::role-html-container 24)
(defconstant atk::role-icon 25)
(defconstant atk::role-image 26)
(defconstant atk::role-internal-frame 27)
(defconstant atk::role-label 28)
(defconstant atk::role-layered-pane 29)
(defconstant atk::role-list 30)
(defconstant atk::role-list-item 31)
(defconstant atk::role-menu 32)
(defconstant atk::role-menu-bar 33)
(defconstant atk::role-menu-item 34)
(defconstant atk::role-option-pane 35)
(defconstant atk::role-page-tab 36)
(defconstant atk::role-page-tab-list 37)
(defconstant atk::role-panel 38)
(defconstant atk::role-password-text 39)
(defconstant atk::role-popup-menu 40)
(defconstant atk::role-progress-bar 41)
(defconstant atk::role-push-button 42)
(defconstant atk::role-radio-button 43)
(defconstant atk::role-radio-menu-item 44)
(defconstant atk::role-root-pane 45)
(defconstant atk::role-row-header 46)
(defconstant atk::role-scroll-bar 47)
(defconstant atk::role-scroll-pane 48)
(defconstant atk::role-separator 49)
(defconstant atk::role-slider 50)
(defconstant atk::role-split-pane 51)
(defconstant atk::role-spin-button 52)
(defconstant atk::role-statusbar 53)
(defconstant atk::role-table 54)
(defconstant atk::role-table-cell 55)
(defconstant atk::role-table-column-header 56)
(defconstant atk::role-table-row-header 57)
(defconstant atk::role-tear-off-menu-item 58)
(defconstant atk::role-terminal 59)
(defconstant atk::role-text 60)
(defconstant atk::role-toggle-button 61)
(defconstant atk::role-tool-bar 62)
(defconstant atk::role-tool-tip 63)
(defconstant atk::role-tree 64)
(defconstant atk::role-tree-table 65)
(defconstant atk::role-unknown 66)
(defconstant atk::role-viewport 67)
(defconstant atk::role-window 68)
(defconstant atk::role-header 69)
(defconstant atk::role-footer 70)
(defconstant atk::role-paragraph 71)
(defconstant atk::role-ruler 72)
(defconstant atk::role-application 73)
(defconstant atk::role-autocomplete 74)
(defconstant atk::role-last-defined 75)
(defconstant atk::layer-invalid 0)
(defconstant atk::layer-background 1)
(defconstant atk::layer-canvas 2)
(defconstant atk::layer-widget 3)
(defconstant atk::layer-mdi 4)
(defconstant atk::layer-popup 5)
(defconstant atk::layer-overlay 6)
(defconstant atk::layer-window 7)
(defconstant atk::xy-screen 0)
(defconstant atk::xy-window 1)
(defconstant atk::text-attr-invalid 0)
(defconstant atk::text-attr-left-margin 1)
(defconstant atk::text-attr-right-margin 2)
(defconstant atk::text-attr-indent 3)
(defconstant atk::text-attr-invisible 4)
(defconstant atk::text-attr-editable 5)
(defconstant atk::text-attr-pixels-above-lines 6)
(defconstant atk::text-attr-pixels-below-lines 7)
(defconstant atk::text-attr-pixels-inside-wrap 8)
(defconstant atk::text-attr-bg-full-height 9)
(defconstant atk::text-attr-rise 10)
(defconstant atk::text-attr-underline 11)
(defconstant atk::text-attr-strikethrough 12)
(defconstant atk::text-attr-size 13)
(defconstant atk::text-attr-scale 14)
(defconstant atk::text-attr-weight 15)
(defconstant atk::text-attr-language 16)
(defconstant atk::text-attr-family-name 17)
(defconstant atk::text-attr-bg-color 18)
(defconstant atk::text-attr-fg-color 19)
(defconstant atk::text-attr-bg-stipple 20)
(defconstant atk::text-attr-fg-stipple 21)
(defconstant atk::text-attr-wrap-mode 22)
(defconstant atk::text-attr-direction 23)
(defconstant atk::text-attr-justification 24)
(defconstant atk::text-attr-stretch 25)
(defconstant atk::text-attr-variant 26)
(defconstant atk::text-attr-style 27)
(defconstant atk::text-attr-last-defined 28)
(defconstant atk::text-boundary-char 0)
(defconstant atk::text-boundary-word-start 1)
(defconstant atk::text-boundary-word-end 2)
(defconstant atk::text-boundary-sentence-start 3)
(defconstant atk::text-boundary-sentence-end 4)
(defconstant atk::text-boundary-line-start 5)
(defconstant atk::text-boundary-line-end 6)
(defconstant gtk::calendar-show-heading 1)
(defconstant gtk::calendar-show-day-names 2)
(defconstant gtk::calendar-no-month-change 4)
(defconstant gtk::calendar-show-week-numbers 8)
(defconstant gtk::calendar-week-start-monday 16)
(defconstant gtk::cell-renderer-selected 1)
(defconstant gtk::cell-renderer-prelit 2)
(defconstant gtk::cell-renderer-insensitive 4)
(defconstant gtk::cell-renderer-sorted 8)
(defconstant gtk::cell-renderer-focused 16)
(defconstant gtk::cell-empty 0)
(defconstant gtk::cell-text 1)
(defconstant gtk::cell-pixmap 2)
(defconstant gtk::cell-pixtext 3)
(defconstant gtk::cell-widget 4)
(defconstant gtk::dialog-modal 1)
(defconstant gtk::dialog-destroy-with-parent 2)
(defconstant gtk::dialog-no-separator 4)
(defconstant gtk::ctree-lines-none 0)
(defconstant gtk::ctree-lines-solid 1)
(defconstant gtk::ctree-lines-dotted 2)
(defconstant gtk::ctree-lines-tabbed 3)
(defconstant gtk::ctree-expander-none 0)
(defconstant gtk::ctree-expander-square 1)
(defconstant gtk::ctree-expander-triangle 2)
(defconstant gtk::ctree-expander-circular 3)
(defconstant gtk::dest-default-motion 1)
(defconstant gtk::dest-default-highlight 2)
(defconstant gtk::dest-default-drop 4)
(defconstant gtk::dest-default-all 7)
(defconstant gtk::image-empty 0)
(defconstant gtk::image-pixmap 1)
(defconstant gtk::image-image 2)
(defconstant gtk::image-pixbuf 3)
(defconstant gtk::image-stock 4)
(defconstant gtk::image-icon-set 5)
(defconstant gtk::image-animation 6)
(defconstant gtk::tree-model-iters-persist 1)
(defconstant gtk::tree-model-list-only 2)
(defconstant gtk::message-info 0)
(defconstant gtk::message-warning 1)
(defconstant gtk::message-question 2)
(defconstant gtk::message-error 3)
(defconstant gtk::buttons-none 0)
(defconstant gtk::buttons-ok 1)
(defconstant gtk::buttons-close 2)
(defconstant gtk::buttons-cancel 3)
(defconstant gtk::buttons-yes-no 4)
(defconstant gtk::buttons-ok-cancel 5)
(defconstant gtk::progress-left-to-right 0)
(defconstant gtk::progress-right-to-left 1)
(defconstant gtk::progress-bottom-to-top 2)
(defconstant gtk::progress-top-to-bottom 3)
(defconstant gtk::size-group-none 0)
(defconstant gtk::size-group-horizontal 1)
(defconstant gtk::size-group-vertical 2)
(defconstant gtk::size-group-both 3)
(defconstant gtk::update-always 0)
(defconstant gtk::update-if-valid 1)
(defconstant gtk::spin-step-forward 0)
(defconstant gtk::spin-step-backward 1)
(defconstant gtk::spin-page-forward 2)
(defconstant gtk::spin-page-backward 3)
(defconstant gtk::spin-home 4)
(defconstant gtk::spin-end 5)
(defconstant gtk::spin-user-defined 6)
(defconstant gtk::text-search-visible-only 0)
(defconstant gtk::text-search-text-only 1)
(defconstant gtk::text-window-private 0)
(defconstant gtk::text-window-widget 1)
(defconstant gtk::text-window-text 2)
(defconstant gtk::text-window-left 3)
(defconstant gtk::text-window-right 4)
(defconstant gtk::text-window-top 5)
(defconstant gtk::text-window-bottom 6)
(defconstant gtk::toolbar-child-space 0)
(defconstant gtk::toolbar-child-button 1)
(defconstant gtk::toolbar-child-togglebutton 2)
(defconstant gtk::toolbar-child-radiobutton 3)
(defconstant gtk::toolbar-child-widget 4)
(defconstant gtk::tree-view-column-grow-only 0)
(defconstant gtk::tree-view-column-autosize 1)
(defconstant gtk::tree-view-column-fixed 2)
(defconstant gtk::tree-view-drop-before 0)
(defconstant gtk::tree-view-drop-after 1)
(defconstant gtk::tree-view-drop-into-or-before 2)
(defconstant gtk::tree-view-drop-into-or-after 3)

;;; C types

(defalien-type int8_t char)
(defalien-type u_int8_t unsigned-char)
(defalien-type int16_t short)
(defalien-type u_int16_t unsigned-short)
(defalien-type int32_t int)
(defalien-type u_int32_t unsigned-int)
(defalien-type int64_t (integer 64))
(defalien-type u_int64_t (unsigned 64))
(defalien-type intptr_t long)
(defalien-type uintptr_t unsigned-long)
(defalien-type ptrdiff_t int)
(defalien-type size_t unsigned-long)
(defalien-type ct_rune_t int)
(defalien-type rune_t int)
(defalien-type wchar_t int)
(defalien-type wint_t int)
(defalien-type gint8 char)
(defalien-type guint8 unsigned-char)
(defalien-type gint16 short)
(defalien-type guint16 unsigned-short)
(defalien-type gint32 int)
(defalien-type guint32 unsigned-int)
(defalien-type gint64 (integer 64))
(defalien-type guint64 (unsigned 64))
(defalien-type gssize long)
(defalien-type gsize unsigned-long)
(defalien-type gchar char)
(defalien-type gshort short)
(defalien-type glong long)
(defalien-type gint int)
(defalien-type guchar unsigned-char)
(defalien-type gushort unsigned-short)
(defalien-type gulong unsigned-long)
(defalien-type guint unsigned-int)
(defalien-type gfloat float)
(defalien-type gdouble double)
(defalien-type GThreadError int)
(defalien-type GThreadPriority int)
(defalien-type GConvertError int)
(defalien-type GDateDMY int)
(defalien-type GDateWeekday int)
(defalien-type GDateMonth int)
(defalien-type GFileError int)
(defalien-type GFileTest int)
(defalien-type GHookFlagMask int)
(defalien-type GUnicodeType int)
(defalien-type GUnicodeBreakType int)
(defalien-type GNormalizeMode int)
(defalien-type GIOError int)
(defalien-type GIOChannelError int)
(defalien-type GIOStatus int)
(defalien-type GSeekType int)
(defalien-type GIOCondition int)
(defalien-type GIOFlags int)
(defalien-type GMarkupError int)
(defalien-type GMarkupParseFlags int)
(defalien-type GLogLevelFlags int)
(defalien-type GTraverseFlags int)
(defalien-type GTraverseType int)
(defalien-type GErrorType int)
(defalien-type GTokenType int)
(defalien-type GShellError int)
(defalien-type GSpawnError int)
(defalien-type GSpawnFlags int)
(defalien-type GAsciiType int)
(defalien-type PangoCoverageLevel int)
(defalien-type GTypeDebugFlags int)
(defalien-type GTypeFundamentalFlags int)
(defalien-type GTypeFlags int)
(defalien-type GParamFlags int)
(defalien-type GSignalFlags int)
(defalien-type GConnectFlags int)
(defalien-type GSignalMatchType int)
(defalien-type PangoDirection int)
(defalien-type PangoStyle int)
(defalien-type PangoVariant int)
(defalien-type PangoWeight int)
(defalien-type PangoStretch int)
(defalien-type PangoFontMask int)
(defalien-type PangoAttrType int)
(defalien-type PangoUnderline int)
(defalien-type PangoTabAlign int)
(defalien-type PangoAlignment int)
(defalien-type PangoWrapMode int)
(defalien-type GdkByteOrder int)
(defalien-type GdkModifierType int)
(defalien-type GdkInputCondition int)
(defalien-type GdkStatus int)
(defalien-type GdkGrabStatus int)
(defalien-type GdkCursorType int)
(defalien-type GdkDragAction int)
(defalien-type GdkDragProtocol int)
(defalien-type GdkExtensionMode int)
(defalien-type GdkInputSource int)
(defalien-type GdkInputMode int)
(defalien-type GdkAxisUse int)
(defalien-type GdkXEvent (struct nil))
(defalien-type GdkFilterReturn int)
(defalien-type GdkEventType int)
(defalien-type GdkEventMask int)
(defalien-type GdkVisibilityState int)
(defalien-type GdkScrollDirection int)
(defalien-type GdkNotifyType int)
(defalien-type GdkCrossingMode int)
(defalien-type GdkPropertyState int)
(defalien-type GdkWindowState int)
(defalien-type GdkSettingAction int)
(defalien-type GdkCapStyle int)
(defalien-type GdkFill int)
(defalien-type GdkFunction int)
(defalien-type GdkJoinStyle int)
(defalien-type GdkLineStyle int)
(defalien-type GdkSubwindowMode int)
(defalien-type GdkGCValuesMask int)
(defalien-type GdkRgbDither int)
(defalien-type GdkPixbufAlphaMode int)
(defalien-type GdkColorspace int)
(defalien-type GdkPixbufError int)
(defalien-type GdkInterpType int)
(defalien-type GdkFontType int)
(defalien-type GdkImageType int)
(defalien-type GdkPropMode int)
(defalien-type GdkFillRule int)
(defalien-type GdkOverlapType int)
(defalien-type GdkVisualType int)
(defalien-type GdkWindowClass int)
(defalien-type GdkWindowType int)
(defalien-type GdkWindowAttributesType int)
(defalien-type GdkWindowHints int)
(defalien-type GdkWindowTypeHint int)
(defalien-type GdkWMDecoration int)
(defalien-type GdkWMFunction int)
(defalien-type GdkGravity int)
(defalien-type GdkWindowEdge int)
(defalien-type GtkAnchorType int)
(defalien-type GtkArrowType int)
(defalien-type GtkAttachOptions int)
(defalien-type GtkButtonBoxStyle int)
(defalien-type GtkCurveType int)
(defalien-type GtkDeleteType int)
(defalien-type GtkDirectionType int)
(defalien-type GtkExpanderStyle int)
(defalien-type GtkIconSize int)
(defalien-type GtkSideType int)
(defalien-type GtkTextDirection int)
(defalien-type GtkJustification int)
(defalien-type GtkMatchType int)
(defalien-type GtkMenuDirectionType int)
(defalien-type GtkMetricType int)
(defalien-type GtkMovementStep int)
(defalien-type GtkOrientation int)
(defalien-type GtkCornerType int)
(defalien-type GtkPackType int)
(defalien-type GtkPathPriorityType int)
(defalien-type GtkPathType int)
(defalien-type GtkPolicyType int)
(defalien-type GtkPositionType int)
(defalien-type GtkPreviewType int)
(defalien-type GtkReliefStyle int)
(defalien-type GtkResizeMode int)
(defalien-type GtkSignalRunType int)
(defalien-type GtkScrollType int)
(defalien-type GtkSelectionMode int)
(defalien-type GtkShadowType int)
(defalien-type GtkStateType int)
(defalien-type GtkSubmenuDirection int)
(defalien-type GtkSubmenuPlacement int)
(defalien-type GtkToolbarStyle int)
(defalien-type GtkUpdateType int)
(defalien-type GtkVisibility int)
(defalien-type GtkWindowPosition int)
(defalien-type GtkWindowType int)
(defalien-type GtkWrapMode int)
(defalien-type GtkSortType int)
(defalien-type GtkIMPreeditStyle int)
(defalien-type GtkIMStatusStyle int)
(defalien-type GtkAccelFlags int)
(defalien-type GtkDebugFlag int)
(defalien-type GtkObjectFlags int)
(defalien-type GtkArgFlags int)
(defalien-type GtkRcFlags int)
(defalien-type GtkRcTokenType int)
(defalien-type AtkStateType int)
(defalien-type AtkRelationType int)
(defalien-type AtkRole int)
(defalien-type AtkLayer int)
(defalien-type GtkWidgetFlags int)
(defalien-type GtkWidgetHelpType int)
(defalien-type AtkKeyEventType int)
(defalien-type AtkCoordType int)
(defalien-type AtkTextAttribute int)
(defalien-type AtkTextBoundary int)
(defalien-type AtkTextClipType int)
(defalien-type AtkHyperlinkStateFlags int)
(defalien-type GtkCalendarDisplayOptions int)
(defalien-type GtkCellRendererState int)
(defalien-type GtkCellRendererMode int)
(defalien-type GtkCellType int)
(defalien-type GtkCListDragPos int)
(defalien-type GtkButtonAction int)
(defalien-type GtkDialogFlags int)
(defalien-type GtkResponseType int)
(defalien-type GtkCTreePos int)
(defalien-type GtkCTreeLineStyle int)
(defalien-type GtkCTreeExpanderStyle int)
(defalien-type GtkCTreeExpansionType int)
(defalien-type GtkDestDefaults int)
(defalien-type GtkTargetFlags int)
(defalien-type GtkImageType int)
(defalien-type GtkTreeModelFlags int)
(defalien-type GtkMessageType int)
(defalien-type GtkButtonsType int)
(defalien-type GtkNotebookTab int)
(defalien-type GtkProgressBarStyle int)
(defalien-type GtkProgressBarOrientation int)
(defalien-type GtkSizeGroupMode int)
(defalien-type GtkSpinButtonUpdatePolicy int)
(defalien-type GtkSpinType int)
(defalien-type GtkTextSearchFlags int)
(defalien-type GtkTextWindowType int)
(defalien-type GtkToolbarChildType int)
(defalien-type GtkToolbarSpaceStyle int)
(defalien-type GtkTreeViewColumnSizing int)
(defalien-type GtkTreeViewDropPosition int)

;;; Pointer types

(defalien-type gpointer (* t))
(defalien-type gconstpointer (* t))
(defalien-type GCompareFunc (* t))
(defalien-type GCompareDataFunc (* t))
(defalien-type GEqualFunc (* t))
(defalien-type GDestroyNotify (* t))
(defalien-type GFunc (* t))
(defalien-type GHashFunc (* t))
(defalien-type GHFunc (* t))
(defalien-type GFreeFunc (* t))
(defalien-type GThreadFunc (* t))
(defalien-type GCacheNewFunc (* t))
(defalien-type GCacheDupFunc (* t))
(defalien-type GCacheDestroyFunc (* t))
(defalien-type GCompletionFunc (* t))
(defalien-type GCompletionStrncmpFunc (* t))
(defalien-type GIConv (* t))
(defalien-type GDataForeachFunc (* t))
(defalien-type GHRFunc (* t))
(defalien-type GHookCompareFunc (* t))
(defalien-type GHookFindFunc (* t))
(defalien-type GHookMarshaller (* t))
(defalien-type GHookCheckMarshaller (* t))
(defalien-type GHookFunc (* t))
(defalien-type GHookCheckFunc (* t))
(defalien-type GHookFinalizeFunc (* t))
(defalien-type GSourceFunc (* t))
(defalien-type GSourceDummyMarshal (* t))
(defalien-type GPollFunc (* t))
(defalien-type GIOFunc (* t))
(defalien-type __gnuc_va_list (* t))
(defalien-type GLogFunc (* t))
(defalien-type GPrintFunc (* t))
(defalien-type GNodeTraverseFunc (* t))
(defalien-type GNodeForeachFunc (* t))
(defalien-type GScannerMsgFunc (* t))
(defalien-type GSpawnChildSetupFunc (* t))
(defalien-type GTraverseFunc (* t))
(defalien-type GVoidFunc (* t))
(defalien-type GBaseInitFunc (* t))
(defalien-type GBaseFinalizeFunc (* t))
(defalien-type GClassInitFunc (* t))
(defalien-type GClassFinalizeFunc (* t))
(defalien-type GInstanceInitFunc (* t))
(defalien-type GInterfaceInitFunc (* t))
(defalien-type GInterfaceFinalizeFunc (* t))
(defalien-type GTypeClassCacheFunc (* t))
(defalien-type GBoxedCopyFunc (* t))
(defalien-type GBoxedFreeFunc (* t))
(defalien-type GValueTransform (* t))
(defalien-type GCallback (* t))
(defalien-type GClosureNotify (* t))
(defalien-type GClosureMarshal (* t))
(defalien-type GSignalEmissionHook (* t))
(defalien-type GSignalAccumulator (* t))
(defalien-type GObjectGetPropertyFunc (* t))
(defalien-type GObjectSetPropertyFunc (* t))
(defalien-type GObjectFinalizeFunc (* t))
(defalien-type GWeakNotify (* t))
(defalien-type GTypePluginUse (* t))
(defalien-type GTypePluginUnuse (* t))
(defalien-type GTypePluginCompleteTypeInfo (* t))
(defalien-type GTypePluginCompleteInterfaceInfo (* t))
(defalien-type gchararray (* t))
(defalien-type PangoAttrFilterFunc (* t))
(defalien-type GdkAtom (* t))
(defalien-type GdkInputFunction (* t))
(defalien-type GdkDestroyNotify (* t))
(defalien-type GdkEventFunc (* t))
(defalien-type GdkFilterFunc (* t))
(defalien-type GdkPixbufDestroyNotify (* t))
(defalien-type GdkSpanFunc (* t))
(defalien-type GtkAccelGroupActivate (* t))
(defalien-type GtkAccelGroupFindFunc (* t))
(defalien-type GtkFunction (* t))
(defalien-type GtkDestroyNotify (* t))
(defalien-type GtkCallbackMarshal (* t))
(defalien-type GtkSignalFunc (* t))
(defalien-type GtkRcPropertyParser (* t))
(defalien-type AtkFunction (* t))
(defalien-type AtkPropertyChangeHandler (* t))
(defalien-type GtkCallback (* t))
(defalien-type GtkWindowKeysForeachFunc (* t))
(defalien-type GtkMenuPositionFunc (* t))
(defalien-type GtkMenuDetachFunc (* t))
(defalien-type GtkAccelMapForeach (* t))
(defalien-type AtkEventListener (* t))
(defalien-type AtkEventListenerInit (* t))
(defalien-type AtkKeySnoopFunc (* t))
(defalien-type AtkFocusHandler (* t))
(defalien-type GtkClipboardReceivedFunc (* t))
(defalien-type GtkClipboardTextReceivedFunc (* t))
(defalien-type GtkClipboardGetFunc (* t))
(defalien-type GtkClipboardClearFunc (* t))
(defalien-type GtkCListCompareFunc (* t))
(defalien-type GtkColorSelectionChangePaletteFunc (* t))
(defalien-type GtkColorSelectionChangePaletteWithScreenFunc (* t))
(defalien-type GtkCTreeFunc (* t))
(defalien-type GtkCTreeGNodeFunc (* t))
(defalien-type GtkCTreeCompareDragFunc (* t))
(defalien-type GtkPrintFunc (* t))
(defalien-type GtkTranslateFunc (* t))
(defalien-type GtkItemFactoryCallback (* t))
(defalien-type GtkItemFactoryCallback1 (* t))
(defalien-type GtkMenuCallback (* t))
(defalien-type GtkItemFactoryCallback2 (* t))
(defalien-type GtkTreeModelForeachFunc (* t))
(defalien-type GtkTreeIterCompareFunc (* t))
(defalien-type GtkModuleInitFunc (* t))
(defalien-type GtkModuleDisplayInitFunc (* t))
(defalien-type GtkKeySnoopFunc (* t))
(defalien-type GtkTextFunction (* t))
(defalien-type GtkTextTagTableForeach (* t))
(defalien-type GtkTextCharPredicate (* t))
(defalien-type GtkTreeCellDataFunc (* t))
(defalien-type GtkTreeViewColumnDropFunc (* t))
(defalien-type GtkTreeViewMappingFunc (* t))
(defalien-type GtkTreeViewSearchEqualFunc (* t))
(defalien-type GtkTreeDestroyCountFunc (* t))
(defalien-type GtkTreeSelectionFunc (* t))
(defalien-type GtkTreeSelectionForeachFunc (* t))

;;; Gtk types

(defalien-type register_t int32_t)
(defalien-type gboolean gint)
(defalien-type GQuark guint32)
(defalien-type GTime gint32)
(defalien-type GDateYear guint16)
(defalien-type GDateDay guint8)
(defalien-type gunichar guint32)
(defalien-type gunichar2 guint16)
(defalien-type va_list __gnuc_va_list)
(defalien-type GType gulong)
(defalien-type PangoGlyph guint32)
(defalien-type PangoGlyphUnit gint32)
(defalien-type GdkWChar guint32)
(defalien-type GdkNativeWindow guint32)
(defalien-type GtkFundamentalType GType)
(defalien-type GtkType GType)
(defalien-type GtkClassInitFunc GBaseInitFunc)
(defalien-type AtkState guint64)

;;; Structs

(defalien-type GTimeVal (struct _GTimeVal (tv_sec glong) (tv_usec glong)))
(defalien-type GList (struct _GList (data gpointer) (next (* t)) (prev (* t))))
(defalien-type GTypeInstance (struct _GTypeInstance (g_class (* t))))
(defalien-type PangoRectangle
               (struct _PangoRectangle
                       (x int)
                       (y int)
                       (width int)
                       (height int)))
(defalien-type PangoColor
               (struct _PangoColor
                       (red guint16)
                       (green guint16)
                       (blue guint16)))
(defalien-type PangoGlyphString
               (struct _PangoGlyphString
                       (num_glyphs gint)
                       (glyphs (* t))
                       (log_clusters (* t))
                       (space gint)))
(defalien-type GdkRectangle
               (struct _GdkRectangle
                       (x gint)
                       (y gint)
                       (width gint)
                       (height gint)))
(defalien-type GdkColor
               (struct _GdkColor
                       (pixel guint32)
                       (red guint16)
                       (green guint16)
                       (blue guint16)))
(defalien-type GdkCursor
               (struct _GdkCursor (type GdkCursorType) (ref_count guint)))
(defalien-type GdkEventAny
               (struct _GdkEventAny
                       (type GdkEventType)
                       (window (* t))
                       (send_event gint8)))
(defalien-type GdkEventNoExpose
               (struct _GdkEventNoExpose
                       (type GdkEventType)
                       (window (* t))
                       (send_event gint8)))
(defalien-type GdkEventVisibility
               (struct _GdkEventVisibility
                       (type GdkEventType)
                       (window (* t))
                       (send_event gint8)
                       (state GdkVisibilityState)))
(defalien-type GdkEventMotion
               (struct _GdkEventMotion
                       (type GdkEventType)
                       (window (* t))
                       (send_event gint8)
                       (time guint32)
                       (x gdouble)
                       (y gdouble)
                       (axes (* t))
                       (state guint)
                       (is_hint gint16)
                       (device (* t))
                       (x_root gdouble)
                       (y_root gdouble)))
(defalien-type GdkEventButton
               (struct _GdkEventButton
                       (type GdkEventType)
                       (window (* t))
                       (send_event gint8)
                       (time guint32)
                       (x gdouble)
                       (y gdouble)
                       (axes (* t))
                       (state guint)
                       (button guint)
                       (device (* t))
                       (x_root gdouble)
                       (y_root gdouble)))
(defalien-type GdkEventScroll
               (struct _GdkEventScroll
                       (type GdkEventType)
                       (window (* t))
                       (send_event gint8)
                       (time guint32)
                       (x gdouble)
                       (y gdouble)
                       (state guint)
                       (direction GdkScrollDirection)
                       (device (* t))
                       (x_root gdouble)
                       (y_root gdouble)))
(defalien-type GdkEventKey
               (struct _GdkEventKey
                       (type GdkEventType)
                       (window (* t))
                       (send_event gint8)
                       (time guint32)
                       (state guint)
                       (keyval guint)
                       (length gint)
                       (string (* t))
                       (hardware_keycode guint16)
                       (group guint8)))
(defalien-type GdkEventCrossing
               (struct _GdkEventCrossing
                       (type GdkEventType)
                       (window (* t))
                       (send_event gint8)
                       (subwindow (* t))
                       (time guint32)
                       (x gdouble)
                       (y gdouble)
                       (x_root gdouble)
                       (y_root gdouble)
                       (mode GdkCrossingMode)
                       (detail GdkNotifyType)
                       (focus gboolean)
                       (state guint)))
(defalien-type GdkEventFocus
               (struct _GdkEventFocus
                       (type GdkEventType)
                       (window (* t))
                       (send_event gint8)
                       (in gint16)))
(defalien-type GdkEventConfigure
               (struct _GdkEventConfigure
                       (type GdkEventType)
                       (window (* t))
                       (send_event gint8)
                       (x gint)
                       (y gint)
                       (width gint)
                       (height gint)))
(defalien-type GdkEventProximity
               (struct _GdkEventProximity
                       (type GdkEventType)
                       (window (* t))
                       (send_event gint8)
                       (time guint32)
                       (device (* t))))
(defalien-type u1949
               (union _u1949
                      (b (array char 20))
                      (s (array short 10))
                      (l (array long 5))))
(defalien-type GdkEventSetting
               (struct _GdkEventSetting
                       (type GdkEventType)
                       (window (* t))
                       (send_event gint8)
                       (action GdkSettingAction)
                       (name c-string)))
(defalien-type GdkEventWindowState
               (struct _GdkEventWindowState
                       (type GdkEventType)
                       (window (* t))
                       (send_event gint8)
                       (changed_mask GdkWindowState)
                       (new_window_state GdkWindowState)))
(defalien-type GdkEventDND
               (struct _GdkEventDND
                       (type GdkEventType)
                       (window (* t))
                       (send_event gint8)
                       (context (* t))
                       (time guint32)
                       (x_root gshort)
                       (y_root gshort)))
(defalien-type GdkFont
               (struct _GdkFont
                       (type GdkFontType)
                       (ascent gint)
                       (descent gint)))
(defalien-type GdkGeometry
               (struct _GdkGeometry
                       (min_width gint)
                       (min_height gint)
                       (max_width gint)
                       (max_height gint)
                       (base_width gint)
                       (base_height gint)
                       (width_inc gint)
                       (height_inc gint)
                       (min_aspect gdouble)
                       (max_aspect gdouble)
                       (win_gravity GdkGravity)))
(defalien-type GtkRequisition
               (struct _GtkRequisition (width gint) (height gint)))
(defalien-type GtkCListCellInfo
               (struct _GtkCListCellInfo (row gint) (column gint)))
(defalien-type GtkImagePixmapData (struct _GtkImagePixmapData (pixmap (* t))))
(defalien-type GtkImageImageData (struct _GtkImageImageData (image (* t))))
(defalien-type GtkImagePixbufData (struct _GtkImagePixbufData (pixbuf (* t))))
(defalien-type GtkImageStockData (struct _GtkImageStockData (stock_id (* t))))
(defalien-type GtkImageIconSetData
               (struct _GtkImageIconSetData (icon_set (* t))))
(defalien-type GtkImageAnimationData
               (struct _GtkImageAnimationData
                       (anim (* t))
                       (iter (* t))
                       (frame_timeout guint)))
(defalien-type GtkTreeIter
               (struct _GtkTreeIter
                       (stamp gint)
                       (user_data gpointer)
                       (user_data2 gpointer)
                       (user_data3 gpointer)))
(defalien-type GtkTextIter
               (struct _GtkTextIter
                       (dummy1 gpointer)
                       (dummy2 gpointer)
                       (dummy3 gint)
                       (dummy4 gint)
                       (dummy5 gint)
                       (dummy6 gint)
                       (dummy7 gint)
                       (dummy8 gint)
                       (dummy9 gpointer)
                       (dummy10 gpointer)
                       (dummy11 gint)
                       (dummy12 gint)
                       (dummy13 gint)
                       (dummy14 gpointer)))

;;; Structs that reference structs

(defalien-type GObject
               (struct _GObject
                       (g_type_instance GTypeInstance)
                       (ref_count guint)
                       (qdata (* t))))
(defalien-type GdkColormap
               (struct _GdkColormap
                       (parent_instance GObject)
                       (size gint)
                       (colors (* t))
                       (visual (* t))
                       (windowing_data gpointer)))
(defalien-type GdkDragContext
               (struct _GdkDragContext
                       (parent_instance GObject)
                       (protocol GdkDragProtocol)
                       (is_source gboolean)
                       (source_window (* t))
                       (dest_window (* t))
                       (targets (* t))
                       (actions GdkDragAction)
                       (suggested_action GdkDragAction)
                       (action GdkDragAction)
                       (start_time guint32)
                       (windowing_data gpointer)))
(defalien-type GdkDevice
               (struct _GdkDevice
                       (parent_instance GObject)
                       (name (* t))
                       (source GdkInputSource)
                       (mode GdkInputMode)
                       (has_cursor gboolean)
                       (num_axes gint)
                       (axes (* t))
                       (num_keys gint)
                       (keys (* t))))
(defalien-type GdkEventExpose
               (struct _GdkEventExpose
                       (type GdkEventType)
                       (window (* t))
                       (send_event gint8)
                       (area GdkRectangle)
                       (region (* t))
                       (count gint)))
(defalien-type GdkEventProperty
               (struct _GdkEventProperty
                       (type GdkEventType)
                       (window (* t))
                       (send_event gint8)
                       (atom GdkAtom)
                       (time guint32)
                       (state guint)))
(defalien-type GdkEventSelection
               (struct _GdkEventSelection
                       (type GdkEventType)
                       (window (* t))
                       (send_event gint8)
                       (selection GdkAtom)
                       (target GdkAtom)
                       (property GdkAtom)
                       (time guint32)
                       (requestor GdkNativeWindow)))
(defalien-type GdkEventClient
               (struct _GdkEventClient
                       (type GdkEventType)
                       (window (* t))
                       (send_event gint8)
                       (message_type GdkAtom)
                       (data_format gushort)
                       (data (union _u1949))))
(defalien-type GdkEvent
               (union _GdkEvent
                      (type GdkEventType)
                      (any GdkEventAny)
                      (expose GdkEventExpose)
                      (no_expose GdkEventNoExpose)
                      (visibility GdkEventVisibility)
                      (motion GdkEventMotion)
                      (button GdkEventButton)
                      (scroll GdkEventScroll)
                      (key GdkEventKey)
                      (crossing GdkEventCrossing)
                      (focus_change GdkEventFocus)
                      (configure GdkEventConfigure)
                      (property GdkEventProperty)
                      (selection GdkEventSelection)
                      (proximity GdkEventProximity)
                      (client GdkEventClient)
                      (dnd GdkEventDND)
                      (window_state GdkEventWindowState)
                      (setting GdkEventSetting)))
(defalien-type GdkGC
               (struct _GdkGC
                       (parent_instance GObject)
                       (clip_x_origin gint)
                       (clip_y_origin gint)
                       (ts_x_origin gint)
                       (ts_y_origin gint)
                       (colormap (* t))))
(defalien-type GdkPixbufLoader
               (struct _GdkPixbufLoader
                       (parent_instance GObject)
                       (priv gpointer)))
(defalien-type GdkDrawable (struct _GdkDrawable (parent_instance GObject)))
(defalien-type GdkImage
               (struct _GdkImage
                       (parent_instance GObject)
                       (type GdkImageType)
                       (visual (* t))
                       (byte_order GdkByteOrder)
                       (width gint)
                       (height gint)
                       (depth guint16)
                       (bpp guint16)
                       (bpl guint16)
                       (bits_per_pixel guint16)
                       (mem gpointer)
                       (colormap (* t))
                       (windowing_data gpointer)))
(defalien-type GdkKeymap
               (struct _GdkKeymap (parent_instance GObject) (display (* t))))
(defalien-type GdkVisual
               (struct _GdkVisual
                       (parent_instance GObject)
                       (type GdkVisualType)
                       (depth gint)
                       (byte_order GdkByteOrder)
                       (colormap_size gint)
                       (bits_per_rgb gint)
                       (red_mask guint32)
                       (red_shift gint)
                       (red_prec gint)
                       (green_mask guint32)
                       (green_shift gint)
                       (green_prec gint)
                       (blue_mask guint32)
                       (blue_shift gint)
                       (blue_prec gint)))
(defalien-type GtkAccelGroup
               (struct _GtkAccelGroup
                       (parent GObject)
                       (lock_count guint)
                       (modifier_mask GdkModifierType)
                       (acceleratables (* t))
                       (n_accels guint)
                       (priv_accels (* t))))
(defalien-type GtkObject
               (struct _GtkObject (parent_instance GObject) (flags guint32)))
(defalien-type GtkAdjustment
               (struct _GtkAdjustment
                       (parent_instance GtkObject)
                       (lower gdouble)
                       (upper gdouble)
                       (value gdouble)
                       (step_increment gdouble)
                       (page_increment gdouble)
                       (page_size gdouble)))
(defalien-type GtkStyle
               (struct _GtkStyle
                       (parent_instance GObject)
                       (fg (array GdkColor 5))
                       (bg (array GdkColor 5))
                       (light (array GdkColor 5))
                       (dark (array GdkColor 5))
                       (mid (array GdkColor 5))
                       (text (array GdkColor 5))
                       (base (array GdkColor 5))
                       (text_aa (array GdkColor 5))
                       (black GdkColor)
                       (white GdkColor)
                       (font_desc (* t))
                       (xthickness gint)
                       (ythickness gint)
                       (fg_gc (array (* t) 5))
                       (bg_gc (array (* t) 5))
                       (light_gc (array (* t) 5))
                       (dark_gc (array (* t) 5))
                       (mid_gc (array (* t) 5))
                       (text_gc (array (* t) 5))
                       (base_gc (array (* t) 5))
                       (text_aa_gc (array (* t) 5))
                       (black_gc (* t))
                       (white_gc (* t))
                       (bg_pixmap (array (* t) 5))
                       (attach_count gint)
                       (depth gint)
                       (colormap (* t))
                       (private_font (* t))
                       (private_font_desc (* t))
                       (rc_style (* t))
                       (styles (* t))
                       (property_cache (* t))
                       (icon_factories (* t))))
(defalien-type GtkRcStyle
               (struct _GtkRcStyle
                       (parent_instance GObject)
                       (name (* t))
                       (bg_pixmap_name (array (* t) 5))
                       (font_desc (* t))
                       (color_flags (array GtkRcFlags 5))
                       (fg (array GdkColor 5))
                       (bg (array GdkColor 5))
                       (text (array GdkColor 5))
                       (base (array GdkColor 5))
                       (xthickness gint)
                       (ythickness gint)
                       (rc_properties (* t))
                       (rc_style_lists (* t))
                       (icon_factories (* t))
                       (bf_padding1 guint)))
(defalien-type GtkSettings
               (struct _GtkSettings
                       (parent_instance GObject)
                       (queued_settings (* t))
                       (property_values (* t))
                       (rc_context (* t))
                       (screen (* t))))
(defalien-type AtkObject
               (struct _AtkObject
                       (parent GObject)
                       (description (* t))
                       (name (* t))
                       (accessible_parent (* t))
                       (role AtkRole)
                       (relation_set (* t))
                       (layer AtkLayer)))
(defalien-type GtkAllocation GdkRectangle)
(defalien-type GtkWidget
               (struct _GtkWidget
                       (object GtkObject)
                       (private_flags guint16)
                       (state guint8)
                       (saved_state guint8)
                       (name (* t))
                       (style (* t))
                       (requisition GtkRequisition)
                       (allocation GtkAllocation)
                       (window (* t))
                       (parent (* t))))
(defalien-type GtkMisc
               (struct _GtkMisc
                       (widget GtkWidget)
                       (xalign gfloat)
                       (yalign gfloat)
                       (xpad guint16)
                       (ypad guint16)))
(defalien-type GtkContainer
               (struct _GtkContainer
                       (widget GtkWidget)
                       (focus_child (* t))
                       (bf_padding1 guint)))
(defalien-type GtkBin (struct _GtkBin (container GtkContainer) (child (* t))))
(defalien-type GtkWindow
               (struct _GtkWindow
                       (bin GtkBin)
                       (title (* t))
                       (wmclass_name (* t))
                       (wmclass_class (* t))
                       (wm_role (* t))
                       (focus_widget (* t))
                       (default_widget (* t))
                       (transient_parent (* t))
                       (geometry_info (* t))
                       (frame (* t))
                       (group (* t))
                       (configure_request_count guint16)
                       (bf_padding1 guint)
                       (frame_left guint)
                       (frame_top guint)
                       (frame_right guint)
                       (frame_bottom guint)
                       (keys_changed_handler guint)
                       (mnemonic_modifier GdkModifierType)
                       (screen (* t))))
(defalien-type GtkWindowGroup
               (struct _GtkWindowGroup (parent_instance GObject) (grabs (* t))))
(defalien-type GtkMenuShell
               (struct _GtkMenuShell
                       (container GtkContainer)
                       (children (* t))
                       (active_menu_item (* t))
                       (parent_menu_shell (* t))
                       (button guint)
                       (activate_time guint32)
                       (bf_padding1 guint)))
(defalien-type GtkMenu
               (struct _GtkMenu
                       (menu_shell GtkMenuShell)
                       (parent_menu_item (* t))
                       (old_active_menu_item (* t))
                       (accel_group (* t))
                       (accel_path (* t))
                       (position_func GtkMenuPositionFunc)
                       (position_func_data gpointer)
                       (toggle_size guint)
                       (toplevel (* t))
                       (tearoff_window (* t))
                       (tearoff_hbox (* t))
                       (tearoff_scrollbar (* t))
                       (tearoff_adjustment (* t))
                       (view_window (* t))
                       (bin_window (* t))
                       (scroll_offset gint)
                       (saved_scroll_offset gint)
                       (scroll_step gint)
                       (timeout_id guint)
                       (navigation_region (* t))
                       (navigation_timeout guint)
                       (bf_padding1 guint)))
(defalien-type GtkLabel
               (struct _GtkLabel
                       (misc GtkMisc)
                       (label (* t))
                       (bf_padding1 guint)
                       (mnemonic_keyval guint)
                       (text (* t))
                       (attrs (* t))
                       (effective_attrs (* t))
                       (layout (* t))
                       (mnemonic_widget (* t))
                       (mnemonic_window (* t))
                       (select_info (* t))))
(defalien-type GtkAccelLabel
               (struct _GtkAccelLabel
                       (label GtkLabel)
                       (gtk_reserved guint)
                       (accel_padding guint)
                       (accel_widget (* t))
                       (accel_closure (* t))
                       (accel_group (* t))
                       (accel_string (* t))
                       (accel_string_width guint16)))
(defalien-type AtkUtil (struct _AtkUtil (parent GObject)))
(defalien-type AtkHyperlink (struct _AtkHyperlink (parent GObject)))
(defalien-type AtkNoOpObject (struct _AtkNoOpObject (parent AtkObject)))
(defalien-type AtkObjectFactory (struct _AtkObjectFactory (parent GObject)))
(defalien-type AtkNoOpObjectFactory
               (struct _AtkNoOpObjectFactory (parent AtkObjectFactory)))
(defalien-type AtkRegistry
               (struct _AtkRegistry
                       (parent GObject)
                       (factory_type_registry (* t))
                       (factory_singleton_cache (* t))))
(defalien-type AtkRelation
               (struct _AtkRelation
                       (parent GObject)
                       (target (* t))
                       (relationship AtkRelationType)))
(defalien-type AtkRelationSet
               (struct _AtkRelationSet (parent GObject) (relations (* t))))
(defalien-type AtkStateSet (struct _AtkStateSet (parent GObject)))
(defalien-type GtkAccessible
               (struct _GtkAccessible (parent AtkObject) (widget (* t))))
(defalien-type GtkAlignment
               (struct _GtkAlignment
                       (bin GtkBin)
                       (xalign gfloat)
                       (yalign gfloat)
                       (xscale gfloat)
                       (yscale gfloat)))
(defalien-type GtkFrame
               (struct _GtkFrame
                       (bin GtkBin)
                       (label_widget (* t))
                       (shadow_type gint16)
                       (label_xalign gfloat)
                       (label_yalign gfloat)
                       (child_allocation GtkAllocation)))
(defalien-type GtkAspectFrame
               (struct _GtkAspectFrame
                       (frame GtkFrame)
                       (xalign gfloat)
                       (yalign gfloat)
                       (ratio gfloat)
                       (obey_child gboolean)
                       (center_allocation GtkAllocation)))
(defalien-type GtkArrow
               (struct _GtkArrow
                       (misc GtkMisc)
                       (arrow_type gint16)
                       (shadow_type gint16)))
(defalien-type GtkBox
               (struct _GtkBox
                       (container GtkContainer)
                       (children (* t))
                       (spacing gint16)
                       (bf_padding1 guint)))
(defalien-type GtkButtonBox
               (struct _GtkButtonBox
                       (box GtkBox)
                       (child_min_width gint)
                       (child_min_height gint)
                       (child_ipad_x gint)
                       (child_ipad_y gint)
                       (layout_style GtkButtonBoxStyle)))
(defalien-type GtkButton
               (struct _GtkButton
                       (bin GtkBin)
                       (event_window (* t))
                       (label_text (* t))
                       (activate_timeout guint)
                       (bf_padding1 guint)))
(defalien-type GtkCalendar
               (struct _GtkCalendar
                       (widget GtkWidget)
                       (header_style (* t))
                       (label_style (* t))
                       (month gint)
                       (year gint)
                       (selected_day gint)
                       (day_month (array (array gint 7) 6))
                       (day (array (array gint 7) 6))
                       (num_marked_dates gint)
                       (marked_date (array gint 31))
                       (display_flags GtkCalendarDisplayOptions)
                       (marked_date_color (array GdkColor 31))
                       (gc (* t))
                       (xor_gc (* t))
                       (focus_row gint)
                       (focus_col gint)
                       (highlight_row gint)
                       (highlight_col gint)
                       (private_data gpointer)
                       (grow_space (array gchar 32))
                       (_gtk_reserved1 (* t))
                       (_gtk_reserved2 (* t))
                       (_gtk_reserved3 (* t))
                       (_gtk_reserved4 (* t))))
(defalien-type GtkCellRenderer
               (struct _GtkCellRenderer
                       (parent GtkObject)
                       (xalign gfloat)
                       (yalign gfloat)
                       (width gint)
                       (height gint)
                       (xpad guint16)
                       (ypad guint16)
                       (bf_padding1 guint)))
(defalien-type GtkCellRendererText
               (struct _GtkCellRendererText
                       (parent GtkCellRenderer)
                       (text (* t))
                       (font (* t))
                       (font_scale gdouble)
                       (foreground PangoColor)
                       (background PangoColor)
                       (extra_attrs (* t))
                       (underline_style PangoUnderline)
                       (rise gint)
                       (fixed_height_rows gint)
                       (bf_padding1 guint)))
(defalien-type GtkCellRendererToggle
               (struct _GtkCellRendererToggle
                       (parent GtkCellRenderer)
                       (bf_padding1 guint)))
(defalien-type GtkCellRendererPixbuf
               (struct _GtkCellRendererPixbuf
                       (parent GtkCellRenderer)
                       (pixbuf (* t))
                       (pixbuf_expander_open (* t))
                       (pixbuf_expander_closed (* t))))
(defalien-type GtkToggleButton
               (struct _GtkToggleButton (button GtkButton) (bf_padding1 guint)))
(defalien-type GtkCheckButton
               (struct _GtkCheckButton (toggle_button GtkToggleButton)))
(defalien-type GtkItem (struct _GtkItem (bin GtkBin)))
(defalien-type GtkMenuItem
               (struct _GtkMenuItem
                       (item GtkItem)
                       (submenu (* t))
                       (event_window (* t))
                       (toggle_size guint16)
                       (accelerator_width guint16)
                       (accel_path (* t))
                       (bf_padding1 guint)
                       (timer guint)))
(defalien-type GtkCheckMenuItem
               (struct _GtkCheckMenuItem
                       (menu_item GtkMenuItem)
                       (bf_padding1 guint)))
(defalien-type GtkSelectionData
               (struct _GtkSelectionData
                       (selection GdkAtom)
                       (target GdkAtom)
                       (type GdkAtom)
                       (format gint)
                       (data (* t))
                       (length gint)
                       (display (* t))))
(defalien-type GtkRange
               (struct _GtkRange
                       (widget GtkWidget)
                       (adjustment (* t))
                       (update_policy GtkUpdateType)
                       (bf_padding1 guint)
                       (min_slider_size gint)
                       (orientation GtkOrientation)
                       (range_rect GdkRectangle)
                       (slider_start gint)
                       (slider_end gint)
                       (round_digits gint)
                       (bf_padding2 guint)
                       (layout (* t))
                       (timer (* t))
                       (slide_initial_slider_position gint)
                       (slide_initial_coordinate gint)
                       (update_timeout_id guint)
                       (event_window (* t))))
(defalien-type GtkScrollbar (struct _GtkScrollbar (range GtkRange)))
(defalien-type GtkHScrollbar (struct _GtkHScrollbar (scrollbar GtkScrollbar)))
(defalien-type GtkVScrollbar (struct _GtkVScrollbar (scrollbar GtkScrollbar)))
(defalien-type GtkCList
               (struct _GtkCList
                       (container GtkContainer)
                       (flags guint16)
                       (row_mem_chunk (* t))
                       (cell_mem_chunk (* t))
                       (freeze_count guint)
                       (internal_allocation GdkRectangle)
                       (rows gint)
                       (row_height gint)
                       (row_list (* t))
                       (row_list_end (* t))
                       (columns gint)
                       (column_title_area GdkRectangle)
                       (title_window (* t))
                       (column (* t))
                       (clist_window (* t))
                       (clist_window_width gint)
                       (clist_window_height gint)
                       (hoffset gint)
                       (voffset gint)
                       (shadow_type GtkShadowType)
                       (selection_mode GtkSelectionMode)
                       (selection (* t))
                       (selection_end (* t))
                       (undo_selection (* t))
                       (undo_unselection (* t))
                       (undo_anchor gint)
                       (button_actions (array guint8 5))
                       (drag_button guint8)
                       (click_cell GtkCListCellInfo)
                       (hadjustment (* t))
                       (vadjustment (* t))
                       (xor_gc (* t))
                       (fg_gc (* t))
                       (bg_gc (* t))
                       (cursor_drag (* t))
                       (x_drag gint)
                       (focus_row gint)
                       (focus_header_column gint)
                       (anchor gint)
                       (anchor_state GtkStateType)
                       (drag_pos gint)
                       (htimer gint)
                       (vtimer gint)
                       (sort_type GtkSortType)
                       (compare GtkCListCompareFunc)
                       (sort_column gint)
                       (drag_highlight_row gint)
                       (drag_highlight_pos GtkCListDragPos)))
(defalien-type GtkDialog
               (struct _GtkDialog
                       (window GtkWindow)
                       (vbox (* t))
                       (action_area (* t))
                       (separator (* t))))
(defalien-type GtkVBox (struct _GtkVBox (box GtkBox)))
(defalien-type GtkColorSelection
               (struct _GtkColorSelection
                       (parent_instance GtkVBox)
                       (private_data gpointer)))
(defalien-type GtkColorSelectionDialog
               (struct _GtkColorSelectionDialog
                       (parent_instance GtkDialog)
                       (colorsel (* t))
                       (ok_button (* t))
                       (cancel_button (* t))
                       (help_button (* t))))
(defalien-type GtkHBox (struct _GtkHBox (box GtkBox)))
(defalien-type GtkCombo
               (struct _GtkCombo
                       (hbox GtkHBox)
                       (entry (* t))
                       (button (* t))
                       (popup (* t))
                       (popwin (* t))
                       (list (* t))
                       (entry_change_id guint)
                       (list_change_id guint)
                       (bf_padding1 guint)
                       (current_button guint16)
                       (activate_id guint)))
(defalien-type GtkCTree
               (struct _GtkCTree
                       (clist GtkCList)
                       (lines_gc (* t))
                       (tree_indent gint)
                       (tree_spacing gint)
                       (tree_column gint)
                       (bf_padding1 guint)
                       (drag_compare GtkCTreeCompareDragFunc)))
(defalien-type GtkDrawingArea
               (struct _GtkDrawingArea (widget GtkWidget) (draw_data gpointer)))
(defalien-type GtkCurve
               (struct _GtkCurve
                       (graph GtkDrawingArea)
                       (cursor_type gint)
                       (min_x gfloat)
                       (max_x gfloat)
                       (min_y gfloat)
                       (max_y gfloat)
                       (pixmap (* t))
                       (curve_type GtkCurveType)
                       (height gint)
                       (grab_point gint)
                       (last gint)
                       (num_points gint)
                       (point (* t))
                       (num_ctlpoints gint)
                       (ctlpoint (* t))))
(defalien-type GtkIMContext (struct _GtkIMContext (parent_instance GObject)))
(defalien-type GtkEntry
               (struct _GtkEntry
                       (widget GtkWidget)
                       (text (* t))
                       (bf_padding1 guint)
                       (text_length guint16)
                       (text_max_length guint16)
                       (text_area (* t))
                       (im_context (* t))
                       (popup_menu (* t))
                       (current_pos gint)
                       (selection_bound gint)
                       (cached_layout (* t))
                       (bf_padding2 guint)
                       (button guint)
                       (blink_timeout guint)
                       (recompute_idle guint)
                       (scroll_offset gint)
                       (ascent gint)
                       (descent gint)
                       (text_size guint16)
                       (n_bytes guint16)
                       (preedit_length guint16)
                       (preedit_cursor guint16)
                       (dnd_position gint)
                       (drag_start_x gint)
                       (drag_start_y gint)
                       (invisible_char gunichar)
                       (width_chars gint)))
(defalien-type GtkEventBox (struct _GtkEventBox (bin GtkBin)))
(defalien-type GtkFileSelection
               (struct _GtkFileSelection
                       (parent_instance GtkDialog)
                       (dir_list (* t))
                       (file_list (* t))
                       (selection_entry (* t))
                       (selection_text (* t))
                       (main_vbox (* t))
                       (ok_button (* t))
                       (cancel_button (* t))
                       (help_button (* t))
                       (history_pulldown (* t))
                       (history_menu (* t))
                       (history_list (* t))
                       (fileop_dialog (* t))
                       (fileop_entry (* t))
                       (fileop_file (* t))
                       (cmpl_state gpointer)
                       (fileop_c_dir (* t))
                       (fileop_del_file (* t))
                       (fileop_ren_file (* t))
                       (button_area (* t))
                       (action_area (* t))
                       (selected_names (* t))
                       (last_selected (* t))))
(defalien-type GtkFixed
               (struct _GtkFixed (container GtkContainer) (children (* t))))
(defalien-type GtkFontSelection
               (struct _GtkFontSelection
                       (parent_instance GtkVBox)
                       (font_entry (* t))
                       (family_list (* t))
                       (font_style_entry (* t))
                       (face_list (* t))
                       (size_entry (* t))
                       (size_list (* t))
                       (pixels_button (* t))
                       (points_button (* t))
                       (filter_button (* t))
                       (preview_entry (* t))
                       (family (* t))
                       (face (* t))
                       (size gint)
                       (font (* t))))
(defalien-type GtkFontSelectionDialog
               (struct _GtkFontSelectionDialog
                       (parent_instance GtkDialog)
                       (fontsel (* t))
                       (main_vbox (* t))
                       (action_area (* t))
                       (ok_button (* t))
                       (apply_button (* t))
                       (cancel_button (* t))
                       (dialog_width gint)
                       (auto_resize gboolean)))
(defalien-type GtkGammaCurve
               (struct _GtkGammaCurve
                       (vbox GtkVBox)
                       (table (* t))
                       (curve (* t))
                       (button (array (* t) 5))
                       (gamma gfloat)
                       (gamma_dialog (* t))
                       (gamma_text (* t))))
(defalien-type GtkHandleBox
               (struct _GtkHandleBox
                       (bin GtkBin)
                       (bin_window (* t))
                       (float_window (* t))
                       (shadow_type GtkShadowType)
                       (bf_padding1 guint)
                       (deskoff_x gint)
                       (deskoff_y gint)
                       (attach_allocation GtkAllocation)
                       (float_allocation GtkAllocation)))
(defalien-type GtkHButtonBox (struct _GtkHButtonBox (button_box GtkButtonBox)))
(defalien-type GtkPaned
               (struct _GtkPaned
                       (container GtkContainer)
                       (child1 (* t))
                       (child2 (* t))
                       (handle (* t))
                       (xor_gc (* t))
                       (cursor_type GdkCursorType)
                       (handle_pos GdkRectangle)
                       (child1_size gint)
                       (last_allocation gint)
                       (min_position gint)
                       (max_position gint)
                       (bf_padding1 guint)
                       (last_child1_focus (* t))
                       (last_child2_focus (* t))
                       (priv (* t))
                       (drag_pos gint)
                       (original_position gint)))
(defalien-type GtkHPaned (struct _GtkHPaned (paned GtkPaned)))
(defalien-type GtkRuler
               (struct _GtkRuler
                       (widget GtkWidget)
                       (backing_store (* t))
                       (non_gr_exp_gc (* t))
                       (metric (* t))
                       (xsrc gint)
                       (ysrc gint)
                       (slider_size gint)
                       (lower gdouble)
                       (upper gdouble)
                       (position gdouble)
                       (max_size gdouble)))
(defalien-type GtkHRuler (struct _GtkHRuler (ruler GtkRuler)))
(defalien-type GtkScale
               (struct _GtkScale
                       (range GtkRange)
                       (digits gint)
                       (bf_padding1 guint)))
(defalien-type GtkHScale (struct _GtkHScale (scale GtkScale)))
(defalien-type GtkSeparator (struct _GtkSeparator (widget GtkWidget)))
(defalien-type GtkHSeparator (struct _GtkHSeparator (separator GtkSeparator)))
(defalien-type GtkIconFactory
               (struct _GtkIconFactory (parent_instance GObject) (icons (* t))))
(defalien-type u4562
               (union _u4562
                      (pixmap GtkImagePixmapData)
                      (image GtkImageImageData)
                      (pixbuf GtkImagePixbufData)
                      (stock GtkImageStockData)
                      (icon_set GtkImageIconSetData)
                      (anim GtkImageAnimationData)))
(defalien-type GtkImage
               (struct _GtkImage
                       (misc GtkMisc)
                       (storage_type GtkImageType)
                       (data (union _u4562))
                       (mask (* t))
                       (icon_size GtkIconSize)))
(defalien-type GtkImageMenuItem
               (struct _GtkImageMenuItem (menu_item GtkMenuItem) (image (* t))))
(defalien-type GtkIMContextSimple
               (struct _GtkIMContextSimple
                       (object GtkIMContext)
                       (tables (* t))
                       (compose_buffer (array guint 8))
                       (tentative_match gunichar)
                       (tentative_match_len gint)
                       (bf_padding1 guint)))
(defalien-type GtkIMMulticontext
               (struct _GtkIMMulticontext
                       (object GtkIMContext)
                       (slave (* t))
                       (priv (* t))
                       (context_id c-string)))
(defalien-type GtkInputDialog
               (struct _GtkInputDialog
                       (dialog GtkDialog)
                       (axis_list (* t))
                       (axis_listbox (* t))
                       (mode_optionmenu (* t))
                       (close_button (* t))
                       (save_button (* t))
                       (axis_items (array (* t) 7))
                       (current_device (* t))
                       (keys_list (* t))
                       (keys_listbox (* t))))
(defalien-type GtkInvisible
               (struct _GtkInvisible
                       (widget GtkWidget)
                       (has_user_ref_count gboolean)
                       (screen (* t))))
(defalien-type GtkItemFactory
               (struct _GtkItemFactory
                       (object GtkObject)
                       (path (* t))
                       (accel_group (* t))
                       (widget (* t))
                       (items (* t))
                       (translate_func GtkTranslateFunc)
                       (translate_data gpointer)
                       (translate_notify GtkDestroyNotify)))
(defalien-type GtkLayout
               (struct _GtkLayout
                       (container GtkContainer)
                       (children (* t))
                       (width guint)
                       (height guint)
                       (hadjustment (* t))
                       (vadjustment (* t))
                       (bin_window (* t))
                       (visibility GdkVisibilityState)
                       (scroll_x gint)
                       (scroll_y gint)
                       (freeze_count guint)))
(defalien-type GtkListItem (struct _GtkListItem (item GtkItem)))
(defalien-type GtkList
               (struct _GtkList
                       (container GtkContainer)
                       (children (* t))
                       (selection (* t))
                       (undo_selection (* t))
                       (undo_unselection (* t))
                       (last_focus_child (* t))
                       (undo_focus_child (* t))
                       (htimer guint)
                       (vtimer guint)
                       (anchor gint)
                       (drag_pos gint)
                       (anchor_state GtkStateType)
                       (bf_padding1 guint)))
(defalien-type GtkListStore
               (struct _GtkListStore
                       (parent GObject)
                       (stamp gint)
                       (root gpointer)
                       (tail gpointer)
                       (sort_list (* t))
                       (n_columns gint)
                       (sort_column_id gint)
                       (order GtkSortType)
                       (column_headers (* t))
                       (length gint)
                       (default_sort_func GtkTreeIterCompareFunc)
                       (default_sort_data gpointer)
                       (default_sort_destroy GtkDestroyNotify)
                       (bf_padding1 guint)))
(defalien-type GtkMenuBar (struct _GtkMenuBar (menu_shell GtkMenuShell)))
(defalien-type GtkMessageDialog
               (struct _GtkMessageDialog
                       (parent_instance GtkDialog)
                       (image (* t))
                       (label (* t))))
(defalien-type GtkNotebook
               (struct _GtkNotebook
                       (container GtkContainer)
                       (cur_page (* t))
                       (children (* t))
                       (first_tab (* t))
                       (focus_tab (* t))
                       (menu (* t))
                       (event_window (* t))
                       (timer guint32)
                       (tab_hborder guint16)
                       (tab_vborder guint16)
                       (bf_padding1 guint)))
(defalien-type GtkOldEditable
               (struct _GtkOldEditable
                       (widget GtkWidget)
                       (current_pos guint)
                       (selection_start_pos guint)
                       (selection_end_pos guint)
                       (bf_padding1 guint)
                       (clipboard_text (* t))))
(defalien-type GtkOptionMenu
               (struct _GtkOptionMenu
                       (button GtkButton)
                       (menu (* t))
                       (menu_item (* t))
                       (width guint16)
                       (height guint16)))
(defalien-type GtkPixmap
               (struct _GtkPixmap
                       (misc GtkMisc)
                       (pixmap (* t))
                       (mask (* t))
                       (pixmap_insensitive (* t))
                       (bf_padding1 guint)))
(defalien-type GtkSocket
               (struct _GtkSocket
                       (container GtkContainer)
                       (request_width guint16)
                       (request_height guint16)
                       (current_width guint16)
                       (current_height guint16)
                       (plug_window (* t))
                       (plug_widget (* t))
                       (xembed_version gshort)
                       (bf_padding1 guint)
                       (accel_group (* t))
                       (toplevel (* t))))
(defalien-type GtkPlug
               (struct _GtkPlug
                       (window GtkWindow)
                       (socket_window (* t))
                       (modality_window (* t))
                       (modality_group (* t))
                       (grabbed_keys (* t))
                       (bf_padding1 guint)))
(defalien-type GtkPreview
               (struct _GtkPreview
                       (widget GtkWidget)
                       (buffer (* t))
                       (buffer_width guint16)
                       (buffer_height guint16)
                       (bpp guint16)
                       (rowstride guint16)
                       (dither GdkRgbDither)
                       (bf_padding1 guint)))
(defalien-type GtkProgress
               (struct _GtkProgress
                       (widget GtkWidget)
                       (adjustment (* t))
                       (offscreen_pixmap (* t))
                       (format (* t))
                       (x_align gfloat)
                       (y_align gfloat)
                       (bf_padding1 guint)))
(defalien-type GtkProgressBar
               (struct _GtkProgressBar
                       (progress GtkProgress)
                       (bar_style GtkProgressBarStyle)
                       (orientation GtkProgressBarOrientation)
                       (blocks guint)
                       (in_block gint)
                       (activity_pos gint)
                       (activity_step guint)
                       (activity_blocks guint)
                       (pulse_fraction gdouble)
                       (bf_padding1 guint)))
(defalien-type GtkRadioButton
               (struct _GtkRadioButton
                       (check_button GtkCheckButton)
                       (group (* t))))
(defalien-type GtkRadioMenuItem
               (struct _GtkRadioMenuItem
                       (check_menu_item GtkCheckMenuItem)
                       (group (* t))))
(defalien-type GtkViewport
               (struct _GtkViewport
                       (bin GtkBin)
                       (shadow_type GtkShadowType)
                       (view_window (* t))
                       (bin_window (* t))
                       (hadjustment (* t))
                       (vadjustment (* t))))
(defalien-type GtkScrolledWindow
               (struct _GtkScrolledWindow
                       (container GtkBin)
                       (hscrollbar (* t))
                       (vscrollbar (* t))
                       (bf_padding1 guint)
                       (shadow_type guint16)))
(defalien-type GtkSeparatorMenuItem
               (struct _GtkSeparatorMenuItem (menu_item GtkMenuItem)))
(defalien-type GtkSizeGroup
               (struct _GtkSizeGroup
                       (parent_instance GObject)
                       (widgets (* t))
                       (mode guint8)
                       (bf_padding1 guint)
                       (requisition GtkRequisition)))
(defalien-type GtkSpinButton
               (struct _GtkSpinButton
                       (entry GtkEntry)
                       (adjustment (* t))
                       (panel (* t))
                       (timer guint32)
                       (climb_rate gdouble)
                       (timer_step gdouble)
                       (update_policy GtkSpinButtonUpdatePolicy)
                       (bf_padding1 guint)))
(defalien-type GtkStatusbar
               (struct _GtkStatusbar
                       (parent_widget GtkHBox)
                       (frame (* t))
                       (label (* t))
                       (messages (* t))
                       (keys (* t))
                       (seq_context_id guint)
                       (seq_message_id guint)
                       (grip_window (* t))
                       (bf_padding1 guint)))
(defalien-type GtkTable
               (struct _GtkTable
                       (container GtkContainer)
                       (children (* t))
                       (rows (* t))
                       (cols (* t))
                       (nrows guint16)
                       (ncols guint16)
                       (column_spacing guint16)
                       (row_spacing guint16)
                       (bf_padding1 guint)))
(defalien-type GtkTearoffMenuItem
               (struct _GtkTearoffMenuItem
                       (menu_item GtkMenuItem)
                       (bf_padding1 guint)))
(defalien-type GtkTextTag
               (struct _GtkTextTag
                       (parent_instance GObject)
                       (table (* t))
                       (name c-string)
                       (priority int)
                       (values (* t))
                       (bf_padding1 guint)))
(defalien-type GtkTextAppearance
               (struct _GtkTextAppearance
                       (bg_color GdkColor)
                       (fg_color GdkColor)
                       (bg_stipple (* t))
                       (fg_stipple (* t))
                       (rise gint)
                       (padding1 gpointer)
                       (bf_padding1 guint)))
(defalien-type GtkTextAttributes
               (struct _GtkTextAttributes
                       (refcount guint)
                       (appearance GtkTextAppearance)
                       (justification GtkJustification)
                       (direction GtkTextDirection)
                       (font (* t))
                       (font_scale gdouble)
                       (left_margin gint)
                       (indent gint)
                       (right_margin gint)
                       (pixels_above_lines gint)
                       (pixels_below_lines gint)
                       (pixels_inside_wrap gint)
                       (tabs (* t))
                       (wrap_mode GtkWrapMode)
                       (language (* t))
                       (padding1 gpointer)
                       (bf_padding1 guint)))
(defalien-type GtkTextTagTable
               (struct _GtkTextTagTable
                       (parent_instance GObject)
                       (hash (* t))
                       (anonymous (* t))
                       (anon_count gint)
                       (buffers (* t))))
(defalien-type GtkTextChildAnchor
               (struct _GtkTextChildAnchor
                       (parent_instance GObject)
                       (segment gpointer)))
(defalien-type GtkTextMark
               (struct _GtkTextMark
                       (parent_instance GObject)
                       (segment gpointer)))
(defalien-type GtkTextBuffer
               (struct _GtkTextBuffer
                       (parent_instance GObject)
                       (tag_table (* t))
                       (btree (* t))
                       (clipboard_contents_buffers (* t))
                       (selection_clipboards (* t))
                       (log_attr_cache (* t))
                       (user_action_count guint)
                       (bf_padding1 guint)))
(defalien-type GtkTextView
               (struct _GtkTextView
                       (parent_instance GtkContainer)
                       (layout (* t))
                       (buffer (* t))
                       (selection_drag_handler guint)
                       (scroll_timeout guint)
                       (pixels_above_lines gint)
                       (pixels_below_lines gint)
                       (pixels_inside_wrap gint)
                       (wrap_mode GtkWrapMode)
                       (justify GtkJustification)
                       (left_margin gint)
                       (right_margin gint)
                       (indent gint)
                       (tabs (* t))
                       (bf_padding1 guint)
                       (text_window (* t))
                       (left_window (* t))
                       (right_window (* t))
                       (top_window (* t))
                       (bottom_window (* t))
                       (hadjustment (* t))
                       (vadjustment (* t))
                       (xoffset gint)
                       (yoffset gint)
                       (width gint)
                       (height gint)
                       (virtual_cursor_x gint)
                       (virtual_cursor_y gint)
                       (first_para_mark (* t))
                       (first_para_pixels gint)
                       (dnd_mark (* t))
                       (blink_timeout guint)
                       (first_validate_idle guint)
                       (incremental_validate_idle guint)
                       (im_context (* t))
                       (popup_menu (* t))
                       (drag_start_x gint)
                       (drag_start_y gint)
                       (children (* t))
                       (pending_scroll (* t))
                       (pending_place_cursor_button gint)))
(defalien-type GtkTooltips
               (struct _GtkTooltips
                       (parent_instance GtkObject)
                       (tip_window (* t))
                       (tip_label (* t))
                       (active_tips_data (* t))
                       (tips_data_list (* t))
                       (bf_padding1 guint)
                       (timer_tag gint)
                       (last_popdown GTimeVal)))
(defalien-type GtkToolbar
               (struct _GtkToolbar
                       (container GtkContainer)
                       (num_children gint)
                       (children (* t))
                       (orientation GtkOrientation)
                       (style GtkToolbarStyle)
                       (icon_size GtkIconSize)
                       (tooltips (* t))
                       (button_maxw gint)
                       (button_maxh gint)
                       (style_set_connection guint)
                       (icon_size_connection guint)
                       (bf_padding1 guint)))
(defalien-type GtkTreeModelSort
               (struct _GtkTreeModelSort
                       (parent GObject)
                       (root gpointer)
                       (stamp gint)
                       (child_flags guint)
                       (child_model (* t))
                       (zero_ref_count gint)
                       (sort_list (* t))
                       (sort_column_id gint)
                       (order GtkSortType)
                       (default_sort_func GtkTreeIterCompareFunc)
                       (default_sort_data gpointer)
                       (default_sort_destroy GtkDestroyNotify)
                       (changed_id guint)
                       (inserted_id guint)
                       (has_child_toggled_id guint)
                       (deleted_id guint)
                       (reordered_id guint)))
(defalien-type GtkTreeViewColumn
               (struct _GtkTreeViewColumn
                       (parent GtkObject)
                       (tree_view (* t))
                       (button (* t))
                       (child (* t))
                       (arrow (* t))
                       (alignment (* t))
                       (window (* t))
                       (editable_widget (* t))
                       (xalign gfloat)
                       (property_changed_signal guint)
                       (spacing gint)
                       (column_type GtkTreeViewColumnSizing)
                       (requested_width gint)
                       (button_request gint)
                       (resized_width gint)
                       (width gint)
                       (fixed_width gint)
                       (min_width gint)
                       (max_width gint)
                       (drag_x gint)
                       (drag_y gint)
                       (title (* t))
                       (cell_list (* t))
                       (sort_clicked_signal guint)
                       (sort_column_changed_signal guint)
                       (sort_column_id gint)
                       (sort_order GtkSortType)
                       (bf_padding1 guint)))
(defalien-type GtkTreeView
               (struct _GtkTreeView (parent GtkContainer) (priv (* t))))
(defalien-type GtkTreeSelection
               (struct _GtkTreeSelection
                       (parent GObject)
                       (tree_view (* t))
                       (type GtkSelectionMode)
                       (user_func GtkTreeSelectionFunc)
                       (user_data gpointer)
                       (destroy GtkDestroyNotify)))
(defalien-type GtkTreeStore
               (struct _GtkTreeStore
                       (parent GObject)
                       (stamp gint)
                       (root gpointer)
                       (last gpointer)
                       (n_columns gint)
                       (sort_column_id gint)
                       (sort_list (* t))
                       (order GtkSortType)
                       (column_headers (* t))
                       (default_sort_func GtkTreeIterCompareFunc)
                       (default_sort_data gpointer)
                       (default_sort_destroy GtkDestroyNotify)
                       (bf_padding1 guint)))
(defalien-type GtkVButtonBox (struct _GtkVButtonBox (button_box GtkButtonBox)))
(defalien-type GtkVPaned (struct _GtkVPaned (paned GtkPaned)))
(defalien-type GtkVRuler (struct _GtkVRuler (ruler GtkRuler)))
(defalien-type GtkVScale (struct _GtkVScale (scale GtkScale)))
(defalien-type GtkVSeparator (struct _GtkVSeparator (separator GtkSeparator)))

;;; accessors

(defun g::List.data (ptr &optional (val nil vp))
  (let ((v1 (if (typep ptr '(alien (* GList))) ptr (cast ptr (* GList)))))
    (declare (type (alien (* GList)) v1))
    (if vp (setf (slot v1 'data) val) (slot v1 'data))))
(defun g::List.next (ptr &optional (val nil vp))
  (let ((v2 (if (typep ptr '(alien (* GList))) ptr (cast ptr (* GList)))))
    (declare (type (alien (* GList)) v2))
    (if vp (setf (slot v2 'next) val) (slot v2 'next))))
(defun g::List.prev (ptr &optional (val nil vp))
  (let ((v3 (if (typep ptr '(alien (* GList))) ptr (cast ptr (* GList)))))
    (declare (type (alien (* GList)) v3))
    (if vp (setf (slot v3 'prev) val) (slot v3 'prev))))
(defun gdk::Rectangle.x (ptr &optional (val nil vp))
  (let ((v4
         (if (typep ptr '(alien (* GdkRectangle)))
             ptr
             (cast ptr (* GdkRectangle)))))
    (declare (type (alien (* GdkRectangle)) v4))
    (if vp (setf (slot v4 'x) val) (slot v4 'x))))
(defun gdk::Rectangle.y (ptr &optional (val nil vp))
  (let ((v5
         (if (typep ptr '(alien (* GdkRectangle)))
             ptr
             (cast ptr (* GdkRectangle)))))
    (declare (type (alien (* GdkRectangle)) v5))
    (if vp (setf (slot v5 'y) val) (slot v5 'y))))
(defun gdk::Rectangle.width (ptr &optional (val nil vp))
  (let ((v6
         (if (typep ptr '(alien (* GdkRectangle)))
             ptr
             (cast ptr (* GdkRectangle)))))
    (declare (type (alien (* GdkRectangle)) v6))
    (if vp (setf (slot v6 'width) val) (slot v6 'width))))
(defun gdk::Rectangle.height (ptr &optional (val nil vp))
  (let ((v7
         (if (typep ptr '(alien (* GdkRectangle)))
             ptr
             (cast ptr (* GdkRectangle)))))
    (declare (type (alien (* GdkRectangle)) v7))
    (if vp (setf (slot v7 'height) val) (slot v7 'height))))
(defun gdk::Color.pixel (ptr &optional (val nil vp))
  (let ((v8 (if (typep ptr '(alien (* GdkColor))) ptr (cast ptr (* GdkColor)))))
    (declare (type (alien (* GdkColor)) v8))
    (if vp (setf (slot v8 'pixel) val) (slot v8 'pixel))))
(defun gdk::Color.red (ptr &optional (val nil vp))
  (let ((v9 (if (typep ptr '(alien (* GdkColor))) ptr (cast ptr (* GdkColor)))))
    (declare (type (alien (* GdkColor)) v9))
    (if vp (setf (slot v9 'red) val) (slot v9 'red))))
(defun gdk::Color.green (ptr &optional (val nil vp))
  (let ((v10
         (if (typep ptr '(alien (* GdkColor))) ptr (cast ptr (* GdkColor)))))
    (declare (type (alien (* GdkColor)) v10))
    (if vp (setf (slot v10 'green) val) (slot v10 'green))))
(defun gdk::Color.blue (ptr &optional (val nil vp))
  (let ((v11
         (if (typep ptr '(alien (* GdkColor))) ptr (cast ptr (* GdkColor)))))
    (declare (type (alien (* GdkColor)) v11))
    (if vp (setf (slot v11 'blue) val) (slot v11 'blue))))
(defun gdk::EventMotion.type (ptr &optional (val nil vp))
  (let ((v12
         (if (typep ptr '(alien (* GdkEventMotion)))
             ptr
             (cast ptr (* GdkEventMotion)))))
    (declare (type (alien (* GdkEventMotion)) v12))
    (if vp (setf (slot v12 'type) val) (slot v12 'type))))
(defun gdk::EventMotion.window (ptr &optional (val nil vp))
  (let ((v13
         (if (typep ptr '(alien (* GdkEventMotion)))
             ptr
             (cast ptr (* GdkEventMotion)))))
    (declare (type (alien (* GdkEventMotion)) v13))
    (if vp (setf (slot v13 'window) val) (slot v13 'window))))
(defun gdk::EventMotion.send-event (ptr &optional (val nil vp))
  (let ((v14
         (if (typep ptr '(alien (* GdkEventMotion)))
             ptr
             (cast ptr (* GdkEventMotion)))))
    (declare (type (alien (* GdkEventMotion)) v14))
    (if vp (setf (slot v14 'send_event) val) (slot v14 'send_event))))
(defun gdk::EventMotion.time (ptr &optional (val nil vp))
  (let ((v15
         (if (typep ptr '(alien (* GdkEventMotion)))
             ptr
             (cast ptr (* GdkEventMotion)))))
    (declare (type (alien (* GdkEventMotion)) v15))
    (if vp (setf (slot v15 'time) val) (slot v15 'time))))
(defun gdk::EventMotion.x (ptr &optional (val nil vp))
  (let ((v16
         (if (typep ptr '(alien (* GdkEventMotion)))
             ptr
             (cast ptr (* GdkEventMotion)))))
    (declare (type (alien (* GdkEventMotion)) v16))
    (if vp (setf (slot v16 'x) (coerce val 'double-float)) (slot v16 'x))))
(defun gdk::EventMotion.y (ptr &optional (val nil vp))
  (let ((v17
         (if (typep ptr '(alien (* GdkEventMotion)))
             ptr
             (cast ptr (* GdkEventMotion)))))
    (declare (type (alien (* GdkEventMotion)) v17))
    (if vp (setf (slot v17 'y) (coerce val 'double-float)) (slot v17 'y))))
(defun gdk::EventMotion.state (ptr &optional (val nil vp))
  (let ((v18
         (if (typep ptr '(alien (* GdkEventMotion)))
             ptr
             (cast ptr (* GdkEventMotion)))))
    (declare (type (alien (* GdkEventMotion)) v18))
    (if vp (setf (slot v18 'state) val) (slot v18 'state))))
(defun gdk::EventMotion.is-hint (ptr &optional (val nil vp))
  (let ((v19
         (if (typep ptr '(alien (* GdkEventMotion)))
             ptr
             (cast ptr (* GdkEventMotion)))))
    (declare (type (alien (* GdkEventMotion)) v19))
    (if vp (setf (slot v19 'is_hint) val) (slot v19 'is_hint))))
(defun gdk::EventMotion.device (ptr &optional (val nil vp))
  (let ((v20
         (if (typep ptr '(alien (* GdkEventMotion)))
             ptr
             (cast ptr (* GdkEventMotion)))))
    (declare (type (alien (* GdkEventMotion)) v20))
    (if vp (setf (slot v20 'device) val) (slot v20 'device))))
(defun gdk::EventMotion.x-root (ptr &optional (val nil vp))
  (let ((v21
         (if (typep ptr '(alien (* GdkEventMotion)))
             ptr
             (cast ptr (* GdkEventMotion)))))
    (declare (type (alien (* GdkEventMotion)) v21))
    (if vp
        (setf (slot v21 'x_root) (coerce val 'double-float))
        (slot v21 'x_root))))
(defun gdk::EventMotion.y-root (ptr &optional (val nil vp))
  (let ((v22
         (if (typep ptr '(alien (* GdkEventMotion)))
             ptr
             (cast ptr (* GdkEventMotion)))))
    (declare (type (alien (* GdkEventMotion)) v22))
    (if vp
        (setf (slot v22 'y_root) (coerce val 'double-float))
        (slot v22 'y_root))))
(defun gdk::EventButton.type (ptr &optional (val nil vp))
  (let ((v23
         (if (typep ptr '(alien (* GdkEventButton)))
             ptr
             (cast ptr (* GdkEventButton)))))
    (declare (type (alien (* GdkEventButton)) v23))
    (if vp (setf (slot v23 'type) val) (slot v23 'type))))
(defun gdk::EventButton.window (ptr &optional (val nil vp))
  (let ((v24
         (if (typep ptr '(alien (* GdkEventButton)))
             ptr
             (cast ptr (* GdkEventButton)))))
    (declare (type (alien (* GdkEventButton)) v24))
    (if vp (setf (slot v24 'window) val) (slot v24 'window))))
(defun gdk::EventButton.send-event (ptr &optional (val nil vp))
  (let ((v25
         (if (typep ptr '(alien (* GdkEventButton)))
             ptr
             (cast ptr (* GdkEventButton)))))
    (declare (type (alien (* GdkEventButton)) v25))
    (if vp (setf (slot v25 'send_event) val) (slot v25 'send_event))))
(defun gdk::EventButton.time (ptr &optional (val nil vp))
  (let ((v26
         (if (typep ptr '(alien (* GdkEventButton)))
             ptr
             (cast ptr (* GdkEventButton)))))
    (declare (type (alien (* GdkEventButton)) v26))
    (if vp (setf (slot v26 'time) val) (slot v26 'time))))
(defun gdk::EventButton.x (ptr &optional (val nil vp))
  (let ((v27
         (if (typep ptr '(alien (* GdkEventButton)))
             ptr
             (cast ptr (* GdkEventButton)))))
    (declare (type (alien (* GdkEventButton)) v27))
    (if vp (setf (slot v27 'x) (coerce val 'double-float)) (slot v27 'x))))
(defun gdk::EventButton.y (ptr &optional (val nil vp))
  (let ((v28
         (if (typep ptr '(alien (* GdkEventButton)))
             ptr
             (cast ptr (* GdkEventButton)))))
    (declare (type (alien (* GdkEventButton)) v28))
    (if vp (setf (slot v28 'y) (coerce val 'double-float)) (slot v28 'y))))
(defun gdk::EventButton.state (ptr &optional (val nil vp))
  (let ((v29
         (if (typep ptr '(alien (* GdkEventButton)))
             ptr
             (cast ptr (* GdkEventButton)))))
    (declare (type (alien (* GdkEventButton)) v29))
    (if vp (setf (slot v29 'state) val) (slot v29 'state))))
(defun gdk::EventButton.button (ptr &optional (val nil vp))
  (let ((v30
         (if (typep ptr '(alien (* GdkEventButton)))
             ptr
             (cast ptr (* GdkEventButton)))))
    (declare (type (alien (* GdkEventButton)) v30))
    (if vp (setf (slot v30 'button) val) (slot v30 'button))))
(defun gdk::EventButton.device (ptr &optional (val nil vp))
  (let ((v31
         (if (typep ptr '(alien (* GdkEventButton)))
             ptr
             (cast ptr (* GdkEventButton)))))
    (declare (type (alien (* GdkEventButton)) v31))
    (if vp (setf (slot v31 'device) val) (slot v31 'device))))
(defun gdk::EventButton.x-root (ptr &optional (val nil vp))
  (let ((v32
         (if (typep ptr '(alien (* GdkEventButton)))
             ptr
             (cast ptr (* GdkEventButton)))))
    (declare (type (alien (* GdkEventButton)) v32))
    (if vp
        (setf (slot v32 'x_root) (coerce val 'double-float))
        (slot v32 'x_root))))
(defun gdk::EventButton.y-root (ptr &optional (val nil vp))
  (let ((v33
         (if (typep ptr '(alien (* GdkEventButton)))
             ptr
             (cast ptr (* GdkEventButton)))))
    (declare (type (alien (* GdkEventButton)) v33))
    (if vp
        (setf (slot v33 'y_root) (coerce val 'double-float))
        (slot v33 'y_root))))
(defun gdk::EventKey.type (ptr &optional (val nil vp))
  (let ((v34
         (if (typep ptr '(alien (* GdkEventKey)))
             ptr
             (cast ptr (* GdkEventKey)))))
    (declare (type (alien (* GdkEventKey)) v34))
    (if vp (setf (slot v34 'type) val) (slot v34 'type))))
(defun gdk::EventKey.window (ptr &optional (val nil vp))
  (let ((v35
         (if (typep ptr '(alien (* GdkEventKey)))
             ptr
             (cast ptr (* GdkEventKey)))))
    (declare (type (alien (* GdkEventKey)) v35))
    (if vp (setf (slot v35 'window) val) (slot v35 'window))))
(defun gdk::EventKey.send-event (ptr &optional (val nil vp))
  (let ((v36
         (if (typep ptr '(alien (* GdkEventKey)))
             ptr
             (cast ptr (* GdkEventKey)))))
    (declare (type (alien (* GdkEventKey)) v36))
    (if vp (setf (slot v36 'send_event) val) (slot v36 'send_event))))
(defun gdk::EventKey.time (ptr &optional (val nil vp))
  (let ((v37
         (if (typep ptr '(alien (* GdkEventKey)))
             ptr
             (cast ptr (* GdkEventKey)))))
    (declare (type (alien (* GdkEventKey)) v37))
    (if vp (setf (slot v37 'time) val) (slot v37 'time))))
(defun gdk::EventKey.state (ptr &optional (val nil vp))
  (let ((v38
         (if (typep ptr '(alien (* GdkEventKey)))
             ptr
             (cast ptr (* GdkEventKey)))))
    (declare (type (alien (* GdkEventKey)) v38))
    (if vp (setf (slot v38 'state) val) (slot v38 'state))))
(defun gdk::EventKey.keyval (ptr &optional (val nil vp))
  (let ((v39
         (if (typep ptr '(alien (* GdkEventKey)))
             ptr
             (cast ptr (* GdkEventKey)))))
    (declare (type (alien (* GdkEventKey)) v39))
    (if vp (setf (slot v39 'keyval) val) (slot v39 'keyval))))
(defun gdk::EventKey.length (ptr &optional (val nil vp))
  (let ((v40
         (if (typep ptr '(alien (* GdkEventKey)))
             ptr
             (cast ptr (* GdkEventKey)))))
    (declare (type (alien (* GdkEventKey)) v40))
    (if vp (setf (slot v40 'length) val) (slot v40 'length))))
(defun gdk::EventKey.string (ptr &optional (val nil vp))
  (let ((v41
         (if (typep ptr '(alien (* GdkEventKey)))
             ptr
             (cast ptr (* GdkEventKey)))))
    (declare (type (alien (* GdkEventKey)) v41))
    (if vp (setf (slot v41 'string) val) (slot v41 'string))))
(defun gdk::Font.type (ptr &optional (val nil vp))
  (let ((v42 (if (typep ptr '(alien (* GdkFont))) ptr (cast ptr (* GdkFont)))))
    (declare (type (alien (* GdkFont)) v42))
    (if vp (setf (slot v42 'type) val) (slot v42 'type))))
(defun gdk::Font.ascent (ptr &optional (val nil vp))
  (let ((v43 (if (typep ptr '(alien (* GdkFont))) ptr (cast ptr (* GdkFont)))))
    (declare (type (alien (* GdkFont)) v43))
    (if vp (setf (slot v43 'ascent) val) (slot v43 'ascent))))
(defun gdk::Font.descent (ptr &optional (val nil vp))
  (let ((v44 (if (typep ptr '(alien (* GdkFont))) ptr (cast ptr (* GdkFont)))))
    (declare (type (alien (* GdkFont)) v44))
    (if vp (setf (slot v44 'descent) val) (slot v44 'descent))))
(defun gdk::Geometry.min-width (ptr &optional (val nil vp))
  (let ((v45
         (if (typep ptr '(alien (* GdkGeometry)))
             ptr
             (cast ptr (* GdkGeometry)))))
    (declare (type (alien (* GdkGeometry)) v45))
    (if vp (setf (slot v45 'min_width) val) (slot v45 'min_width))))
(defun gdk::Geometry.min-height (ptr &optional (val nil vp))
  (let ((v46
         (if (typep ptr '(alien (* GdkGeometry)))
             ptr
             (cast ptr (* GdkGeometry)))))
    (declare (type (alien (* GdkGeometry)) v46))
    (if vp (setf (slot v46 'min_height) val) (slot v46 'min_height))))
(defun gdk::Geometry.max-width (ptr &optional (val nil vp))
  (let ((v47
         (if (typep ptr '(alien (* GdkGeometry)))
             ptr
             (cast ptr (* GdkGeometry)))))
    (declare (type (alien (* GdkGeometry)) v47))
    (if vp (setf (slot v47 'max_width) val) (slot v47 'max_width))))
(defun gdk::Geometry.max-height (ptr &optional (val nil vp))
  (let ((v48
         (if (typep ptr '(alien (* GdkGeometry)))
             ptr
             (cast ptr (* GdkGeometry)))))
    (declare (type (alien (* GdkGeometry)) v48))
    (if vp (setf (slot v48 'max_height) val) (slot v48 'max_height))))
(defun gdk::Geometry.base-width (ptr &optional (val nil vp))
  (let ((v49
         (if (typep ptr '(alien (* GdkGeometry)))
             ptr
             (cast ptr (* GdkGeometry)))))
    (declare (type (alien (* GdkGeometry)) v49))
    (if vp (setf (slot v49 'base_width) val) (slot v49 'base_width))))
(defun gdk::Geometry.base-height (ptr &optional (val nil vp))
  (let ((v50
         (if (typep ptr '(alien (* GdkGeometry)))
             ptr
             (cast ptr (* GdkGeometry)))))
    (declare (type (alien (* GdkGeometry)) v50))
    (if vp (setf (slot v50 'base_height) val) (slot v50 'base_height))))
(defun gdk::Geometry.width-inc (ptr &optional (val nil vp))
  (let ((v51
         (if (typep ptr '(alien (* GdkGeometry)))
             ptr
             (cast ptr (* GdkGeometry)))))
    (declare (type (alien (* GdkGeometry)) v51))
    (if vp (setf (slot v51 'width_inc) val) (slot v51 'width_inc))))
(defun gdk::Geometry.height-inc (ptr &optional (val nil vp))
  (let ((v52
         (if (typep ptr '(alien (* GdkGeometry)))
             ptr
             (cast ptr (* GdkGeometry)))))
    (declare (type (alien (* GdkGeometry)) v52))
    (if vp (setf (slot v52 'height_inc) val) (slot v52 'height_inc))))
(defun gdk::Geometry.min-aspect (ptr &optional (val nil vp))
  (let ((v53
         (if (typep ptr '(alien (* GdkGeometry)))
             ptr
             (cast ptr (* GdkGeometry)))))
    (declare (type (alien (* GdkGeometry)) v53))
    (if vp
        (setf (slot v53 'min_aspect) (coerce val 'double-float))
        (slot v53 'min_aspect))))
(defun gdk::Geometry.max-aspect (ptr &optional (val nil vp))
  (let ((v54
         (if (typep ptr '(alien (* GdkGeometry)))
             ptr
             (cast ptr (* GdkGeometry)))))
    (declare (type (alien (* GdkGeometry)) v54))
    (if vp
        (setf (slot v54 'max_aspect) (coerce val 'double-float))
        (slot v54 'max_aspect))))
(defun gdk::Geometry.win-gravity (ptr &optional (val nil vp))
  (let ((v55
         (if (typep ptr '(alien (* GdkGeometry)))
             ptr
             (cast ptr (* GdkGeometry)))))
    (declare (type (alien (* GdkGeometry)) v55))
    (if vp (setf (slot v55 'win_gravity) val) (slot v55 'win_gravity))))
(defun gdk::EventExpose.type (ptr &optional (val nil vp))
  (let ((v56
         (if (typep ptr '(alien (* GdkEventExpose)))
             ptr
             (cast ptr (* GdkEventExpose)))))
    (declare (type (alien (* GdkEventExpose)) v56))
    (if vp (setf (slot v56 'type) val) (slot v56 'type))))
(defun gdk::EventExpose.window (ptr &optional (val nil vp))
  (let ((v57
         (if (typep ptr '(alien (* GdkEventExpose)))
             ptr
             (cast ptr (* GdkEventExpose)))))
    (declare (type (alien (* GdkEventExpose)) v57))
    (if vp (setf (slot v57 'window) val) (slot v57 'window))))
(defun gdk::EventExpose.send-event (ptr &optional (val nil vp))
  (let ((v58
         (if (typep ptr '(alien (* GdkEventExpose)))
             ptr
             (cast ptr (* GdkEventExpose)))))
    (declare (type (alien (* GdkEventExpose)) v58))
    (if vp (setf (slot v58 'send_event) val) (slot v58 'send_event))))
(defun gdk::EventExpose.area.x (ptr &optional (val nil vp))
  (let ((v59
         (if (typep ptr '(alien (* GdkEventExpose)))
             ptr
             (cast ptr (* GdkEventExpose)))))
    (declare (type (alien (* GdkEventExpose)) v59))
    (if vp (setf (slot (slot v59 'area) 'x) val) (slot (slot v59 'area) 'x))))
(defun gdk::EventExpose.area.y (ptr &optional (val nil vp))
  (let ((v60
         (if (typep ptr '(alien (* GdkEventExpose)))
             ptr
             (cast ptr (* GdkEventExpose)))))
    (declare (type (alien (* GdkEventExpose)) v60))
    (if vp (setf (slot (slot v60 'area) 'y) val) (slot (slot v60 'area) 'y))))
(defun gdk::EventExpose.area.width (ptr &optional (val nil vp))
  (let ((v61
         (if (typep ptr '(alien (* GdkEventExpose)))
             ptr
             (cast ptr (* GdkEventExpose)))))
    (declare (type (alien (* GdkEventExpose)) v61))
    (if vp
        (setf (slot (slot v61 'area) 'width) val)
        (slot (slot v61 'area) 'width))))
(defun gdk::EventExpose.area.height (ptr &optional (val nil vp))
  (let ((v62
         (if (typep ptr '(alien (* GdkEventExpose)))
             ptr
             (cast ptr (* GdkEventExpose)))))
    (declare (type (alien (* GdkEventExpose)) v62))
    (if vp
        (setf (slot (slot v62 'area) 'height) val)
        (slot (slot v62 'area) 'height))))
(defun gdk::EventExpose.region (ptr &optional (val nil vp))
  (let ((v63
         (if (typep ptr '(alien (* GdkEventExpose)))
             ptr
             (cast ptr (* GdkEventExpose)))))
    (declare (type (alien (* GdkEventExpose)) v63))
    (if vp (setf (slot v63 'region) val) (slot v63 'region))))
(defun gdk::EventExpose.count (ptr &optional (val nil vp))
  (let ((v64
         (if (typep ptr '(alien (* GdkEventExpose)))
             ptr
             (cast ptr (* GdkEventExpose)))))
    (declare (type (alien (* GdkEventExpose)) v64))
    (if vp (setf (slot v64 'count) val) (slot v64 'count))))
(defun gdk::Image.type (ptr &optional (val nil vp))
  (let ((v65
         (if (typep ptr '(alien (* GdkImage))) ptr (cast ptr (* GdkImage)))))
    (declare (type (alien (* GdkImage)) v65))
    (if vp (setf (slot v65 'type) val) (slot v65 'type))))
(defun gdk::Image.visual (ptr &optional (val nil vp))
  (let ((v66
         (if (typep ptr '(alien (* GdkImage))) ptr (cast ptr (* GdkImage)))))
    (declare (type (alien (* GdkImage)) v66))
    (if vp (setf (slot v66 'visual) val) (slot v66 'visual))))
(defun gdk::Image.byte-order (ptr &optional (val nil vp))
  (let ((v67
         (if (typep ptr '(alien (* GdkImage))) ptr (cast ptr (* GdkImage)))))
    (declare (type (alien (* GdkImage)) v67))
    (if vp (setf (slot v67 'byte_order) val) (slot v67 'byte_order))))
(defun gdk::Image.width (ptr &optional (val nil vp))
  (let ((v68
         (if (typep ptr '(alien (* GdkImage))) ptr (cast ptr (* GdkImage)))))
    (declare (type (alien (* GdkImage)) v68))
    (if vp (setf (slot v68 'width) val) (slot v68 'width))))
(defun gdk::Image.height (ptr &optional (val nil vp))
  (let ((v69
         (if (typep ptr '(alien (* GdkImage))) ptr (cast ptr (* GdkImage)))))
    (declare (type (alien (* GdkImage)) v69))
    (if vp (setf (slot v69 'height) val) (slot v69 'height))))
(defun gdk::Image.depth (ptr &optional (val nil vp))
  (let ((v70
         (if (typep ptr '(alien (* GdkImage))) ptr (cast ptr (* GdkImage)))))
    (declare (type (alien (* GdkImage)) v70))
    (if vp (setf (slot v70 'depth) val) (slot v70 'depth))))
(defun gdk::Image.bpp (ptr &optional (val nil vp))
  (let ((v71
         (if (typep ptr '(alien (* GdkImage))) ptr (cast ptr (* GdkImage)))))
    (declare (type (alien (* GdkImage)) v71))
    (if vp (setf (slot v71 'bpp) val) (slot v71 'bpp))))
(defun gdk::Image.bpl (ptr &optional (val nil vp))
  (let ((v72
         (if (typep ptr '(alien (* GdkImage))) ptr (cast ptr (* GdkImage)))))
    (declare (type (alien (* GdkImage)) v72))
    (if vp (setf (slot v72 'bpl) val) (slot v72 'bpl))))
(defun gdk::Image.bits-per-pixel (ptr &optional (val nil vp))
  (let ((v73
         (if (typep ptr '(alien (* GdkImage))) ptr (cast ptr (* GdkImage)))))
    (declare (type (alien (* GdkImage)) v73))
    (if vp (setf (slot v73 'bits_per_pixel) val) (slot v73 'bits_per_pixel))))
(defun gdk::Image.mem (ptr &optional (val nil vp))
  (let ((v74
         (if (typep ptr '(alien (* GdkImage))) ptr (cast ptr (* GdkImage)))))
    (declare (type (alien (* GdkImage)) v74))
    (if vp (setf (slot v74 'mem) val) (slot v74 'mem))))
(defun gdk::Image.colormap (ptr &optional (val nil vp))
  (let ((v75
         (if (typep ptr '(alien (* GdkImage))) ptr (cast ptr (* GdkImage)))))
    (declare (type (alien (* GdkImage)) v75))
    (if vp (setf (slot v75 'colormap) val) (slot v75 'colormap))))
(defun gdk::Image.windowing-data (ptr &optional (val nil vp))
  (let ((v76
         (if (typep ptr '(alien (* GdkImage))) ptr (cast ptr (* GdkImage)))))
    (declare (type (alien (* GdkImage)) v76))
    (if vp (setf (slot v76 'windowing_data) val) (slot v76 'windowing_data))))
(defun gdk::Visual.type (ptr &optional (val nil vp))
  (let ((v77
         (if (typep ptr '(alien (* GdkVisual))) ptr (cast ptr (* GdkVisual)))))
    (declare (type (alien (* GdkVisual)) v77))
    (if vp (setf (slot v77 'type) val) (slot v77 'type))))
(defun gdk::Visual.depth (ptr &optional (val nil vp))
  (let ((v78
         (if (typep ptr '(alien (* GdkVisual))) ptr (cast ptr (* GdkVisual)))))
    (declare (type (alien (* GdkVisual)) v78))
    (if vp (setf (slot v78 'depth) val) (slot v78 'depth))))
(defun gdk::Visual.byte-order (ptr &optional (val nil vp))
  (let ((v79
         (if (typep ptr '(alien (* GdkVisual))) ptr (cast ptr (* GdkVisual)))))
    (declare (type (alien (* GdkVisual)) v79))
    (if vp (setf (slot v79 'byte_order) val) (slot v79 'byte_order))))
(defun gdk::Visual.colormap-size (ptr &optional (val nil vp))
  (let ((v80
         (if (typep ptr '(alien (* GdkVisual))) ptr (cast ptr (* GdkVisual)))))
    (declare (type (alien (* GdkVisual)) v80))
    (if vp (setf (slot v80 'colormap_size) val) (slot v80 'colormap_size))))
(defun gdk::Visual.bits-per-rgb (ptr &optional (val nil vp))
  (let ((v81
         (if (typep ptr '(alien (* GdkVisual))) ptr (cast ptr (* GdkVisual)))))
    (declare (type (alien (* GdkVisual)) v81))
    (if vp (setf (slot v81 'bits_per_rgb) val) (slot v81 'bits_per_rgb))))
(defun gdk::Visual.red-mask (ptr &optional (val nil vp))
  (let ((v82
         (if (typep ptr '(alien (* GdkVisual))) ptr (cast ptr (* GdkVisual)))))
    (declare (type (alien (* GdkVisual)) v82))
    (if vp (setf (slot v82 'red_mask) val) (slot v82 'red_mask))))
(defun gdk::Visual.red-shift (ptr &optional (val nil vp))
  (let ((v83
         (if (typep ptr '(alien (* GdkVisual))) ptr (cast ptr (* GdkVisual)))))
    (declare (type (alien (* GdkVisual)) v83))
    (if vp (setf (slot v83 'red_shift) val) (slot v83 'red_shift))))
(defun gdk::Visual.green-prec (ptr &optional (val nil vp))
  (let ((v84
         (if (typep ptr '(alien (* GdkVisual))) ptr (cast ptr (* GdkVisual)))))
    (declare (type (alien (* GdkVisual)) v84))
    (if vp (setf (slot v84 'green_prec) val) (slot v84 'green_prec))))
(defun gdk::Visual.blue-mask (ptr &optional (val nil vp))
  (let ((v85
         (if (typep ptr '(alien (* GdkVisual))) ptr (cast ptr (* GdkVisual)))))
    (declare (type (alien (* GdkVisual)) v85))
    (if vp (setf (slot v85 'blue_mask) val) (slot v85 'blue_mask))))
(defun gdk::Visual.blue-shift (ptr &optional (val nil vp))
  (let ((v86
         (if (typep ptr '(alien (* GdkVisual))) ptr (cast ptr (* GdkVisual)))))
    (declare (type (alien (* GdkVisual)) v86))
    (if vp (setf (slot v86 'blue_shift) val) (slot v86 'blue_shift))))
(defun gdk::Visual.blue-prec (ptr &optional (val nil vp))
  (let ((v87
         (if (typep ptr '(alien (* GdkVisual))) ptr (cast ptr (* GdkVisual)))))
    (declare (type (alien (* GdkVisual)) v87))
    (if vp (setf (slot v87 'blue_prec) val) (slot v87 'blue_prec))))
(defun gtk::Adjustment.lower (ptr &optional (val nil vp))
  (let ((v88
         (if (typep ptr '(alien (* GtkAdjustment)))
             ptr
             (cast ptr (* GtkAdjustment)))))
    (declare (type (alien (* GtkAdjustment)) v88))
    (if vp
        (setf (slot v88 'lower) (coerce val 'double-float))
        (slot v88 'lower))))
(defun gtk::Adjustment.upper (ptr &optional (val nil vp))
  (let ((v89
         (if (typep ptr '(alien (* GtkAdjustment)))
             ptr
             (cast ptr (* GtkAdjustment)))))
    (declare (type (alien (* GtkAdjustment)) v89))
    (if vp
        (setf (slot v89 'upper) (coerce val 'double-float))
        (slot v89 'upper))))
(defun gtk::Adjustment.value (ptr &optional (val nil vp))
  (let ((v90
         (if (typep ptr '(alien (* GtkAdjustment)))
             ptr
             (cast ptr (* GtkAdjustment)))))
    (declare (type (alien (* GtkAdjustment)) v90))
    (if vp
        (setf (slot v90 'value) (coerce val 'double-float))
        (slot v90 'value))))
(defun gtk::Adjustment.step-increment (ptr &optional (val nil vp))
  (let ((v91
         (if (typep ptr '(alien (* GtkAdjustment)))
             ptr
             (cast ptr (* GtkAdjustment)))))
    (declare (type (alien (* GtkAdjustment)) v91))
    (if vp
        (setf (slot v91 'step_increment) (coerce val 'double-float))
        (slot v91 'step_increment))))
(defun gtk::Adjustment.page-increment (ptr &optional (val nil vp))
  (let ((v92
         (if (typep ptr '(alien (* GtkAdjustment)))
             ptr
             (cast ptr (* GtkAdjustment)))))
    (declare (type (alien (* GtkAdjustment)) v92))
    (if vp
        (setf (slot v92 'page_increment) (coerce val 'double-float))
        (slot v92 'page_increment))))
(defun gtk::Adjustment.page-size (ptr &optional (val nil vp))
  (let ((v93
         (if (typep ptr '(alien (* GtkAdjustment)))
             ptr
             (cast ptr (* GtkAdjustment)))))
    (declare (type (alien (* GtkAdjustment)) v93))
    (if vp
        (setf (slot v93 'page_size) (coerce val 'double-float))
        (slot v93 'page_size))))
(defun gtk::Style.fg.pixel (ptr index &optional (val nil vp))
  (let ((v94
         (if (typep ptr '(alien (* GtkStyle))) ptr (cast ptr (* GtkStyle)))))
    (declare (type (alien (* GtkStyle)) v94))
    (unless (< index 5) (error "struct array index ~D more than 5." index))
    (if vp
        (setf (slot (deref (slot v94 'fg) index) 'pixel) val)
        (slot (deref (slot v94 'fg) index) 'pixel))))
(defun gtk::Style.fg.red (ptr index &optional (val nil vp))
  (let ((v95
         (if (typep ptr '(alien (* GtkStyle))) ptr (cast ptr (* GtkStyle)))))
    (declare (type (alien (* GtkStyle)) v95))
    (unless (< index 5) (error "struct array index ~D more than 5." index))
    (if vp
        (setf (slot (deref (slot v95 'fg) index) 'red) val)
        (slot (deref (slot v95 'fg) index) 'red))))
(defun gtk::Style.fg.green (ptr index &optional (val nil vp))
  (let ((v96
         (if (typep ptr '(alien (* GtkStyle))) ptr (cast ptr (* GtkStyle)))))
    (declare (type (alien (* GtkStyle)) v96))
    (unless (< index 5) (error "struct array index ~D more than 5." index))
    (if vp
        (setf (slot (deref (slot v96 'fg) index) 'green) val)
        (slot (deref (slot v96 'fg) index) 'green))))
(defun gtk::Style.fg.blue (ptr index &optional (val nil vp))
  (let ((v97
         (if (typep ptr '(alien (* GtkStyle))) ptr (cast ptr (* GtkStyle)))))
    (declare (type (alien (* GtkStyle)) v97))
    (unless (< index 5) (error "struct array index ~D more than 5." index))
    (if vp
        (setf (slot (deref (slot v97 'fg) index) 'blue) val)
        (slot (deref (slot v97 'fg) index) 'blue))))
(defun gtk::Style.bg.pixel (ptr index &optional (val nil vp))
  (let ((v98
         (if (typep ptr '(alien (* GtkStyle))) ptr (cast ptr (* GtkStyle)))))
    (declare (type (alien (* GtkStyle)) v98))
    (unless (< index 5) (error "struct array index ~D more than 5." index))
    (if vp
        (setf (slot (deref (slot v98 'bg) index) 'pixel) val)
        (slot (deref (slot v98 'bg) index) 'pixel))))
(defun gtk::Style.bg.red (ptr index &optional (val nil vp))
  (let ((v99
         (if (typep ptr '(alien (* GtkStyle))) ptr (cast ptr (* GtkStyle)))))
    (declare (type (alien (* GtkStyle)) v99))
    (unless (< index 5) (error "struct array index ~D more than 5." index))
    (if vp
        (setf (slot (deref (slot v99 'bg) index) 'red) val)
        (slot (deref (slot v99 'bg) index) 'red))))
(defun gtk::Style.bg.green (ptr index &optional (val nil vp))
  (let ((v100
         (if (typep ptr '(alien (* GtkStyle))) ptr (cast ptr (* GtkStyle)))))
    (declare (type (alien (* GtkStyle)) v100))
    (unless (< index 5) (error "struct array index ~D more than 5." index))
    (if vp
        (setf (slot (deref (slot v100 'bg) index) 'green) val)
        (slot (deref (slot v100 'bg) index) 'green))))
(defun gtk::Style.bg.blue (ptr index &optional (val nil vp))
  (let ((v101
         (if (typep ptr '(alien (* GtkStyle))) ptr (cast ptr (* GtkStyle)))))
    (declare (type (alien (* GtkStyle)) v101))
    (unless (< index 5) (error "struct array index ~D more than 5." index))
    (if vp
        (setf (slot (deref (slot v101 'bg) index) 'blue) val)
        (slot (deref (slot v101 'bg) index) 'blue))))
(defun gtk::Style.light.pixel (ptr index &optional (val nil vp))
  (let ((v102
         (if (typep ptr '(alien (* GtkStyle))) ptr (cast ptr (* GtkStyle)))))
    (declare (type (alien (* GtkStyle)) v102))
    (unless (< index 5) (error "struct array index ~D more than 5." index))
    (if vp
        (setf (slot (deref (slot v102 'light) index) 'pixel) val)
        (slot (deref (slot v102 'light) index) 'pixel))))
(defun gtk::Style.light.red (ptr index &optional (val nil vp))
  (let ((v103
         (if (typep ptr '(alien (* GtkStyle))) ptr (cast ptr (* GtkStyle)))))
    (declare (type (alien (* GtkStyle)) v103))
    (unless (< index 5) (error "struct array index ~D more than 5." index))
    (if vp
        (setf (slot (deref (slot v103 'light) index) 'red) val)
        (slot (deref (slot v103 'light) index) 'red))))
(defun gtk::Style.light.green (ptr index &optional (val nil vp))
  (let ((v104
         (if (typep ptr '(alien (* GtkStyle))) ptr (cast ptr (* GtkStyle)))))
    (declare (type (alien (* GtkStyle)) v104))
    (unless (< index 5) (error "struct array index ~D more than 5." index))
    (if vp
        (setf (slot (deref (slot v104 'light) index) 'green) val)
        (slot (deref (slot v104 'light) index) 'green))))
(defun gtk::Style.light.blue (ptr index &optional (val nil vp))
  (let ((v105
         (if (typep ptr '(alien (* GtkStyle))) ptr (cast ptr (* GtkStyle)))))
    (declare (type (alien (* GtkStyle)) v105))
    (unless (< index 5) (error "struct array index ~D more than 5." index))
    (if vp
        (setf (slot (deref (slot v105 'light) index) 'blue) val)
        (slot (deref (slot v105 'light) index) 'blue))))
(defun gtk::Style.dark.pixel (ptr index &optional (val nil vp))
  (let ((v106
         (if (typep ptr '(alien (* GtkStyle))) ptr (cast ptr (* GtkStyle)))))
    (declare (type (alien (* GtkStyle)) v106))
    (unless (< index 5) (error "struct array index ~D more than 5." index))
    (if vp
        (setf (slot (deref (slot v106 'dark) index) 'pixel) val)
        (slot (deref (slot v106 'dark) index) 'pixel))))
(defun gtk::Style.dark.red (ptr index &optional (val nil vp))
  (let ((v107
         (if (typep ptr '(alien (* GtkStyle))) ptr (cast ptr (* GtkStyle)))))
    (declare (type (alien (* GtkStyle)) v107))
    (unless (< index 5) (error "struct array index ~D more than 5." index))
    (if vp
        (setf (slot (deref (slot v107 'dark) index) 'red) val)
        (slot (deref (slot v107 'dark) index) 'red))))
(defun gtk::Style.dark.green (ptr index &optional (val nil vp))
  (let ((v108
         (if (typep ptr '(alien (* GtkStyle))) ptr (cast ptr (* GtkStyle)))))
    (declare (type (alien (* GtkStyle)) v108))
    (unless (< index 5) (error "struct array index ~D more than 5." index))
    (if vp
        (setf (slot (deref (slot v108 'dark) index) 'green) val)
        (slot (deref (slot v108 'dark) index) 'green))))
(defun gtk::Style.dark.blue (ptr index &optional (val nil vp))
  (let ((v109
         (if (typep ptr '(alien (* GtkStyle))) ptr (cast ptr (* GtkStyle)))))
    (declare (type (alien (* GtkStyle)) v109))
    (unless (< index 5) (error "struct array index ~D more than 5." index))
    (if vp
        (setf (slot (deref (slot v109 'dark) index) 'blue) val)
        (slot (deref (slot v109 'dark) index) 'blue))))
(defun gtk::Style.mid.pixel (ptr index &optional (val nil vp))
  (let ((v110
         (if (typep ptr '(alien (* GtkStyle))) ptr (cast ptr (* GtkStyle)))))
    (declare (type (alien (* GtkStyle)) v110))
    (unless (< index 5) (error "struct array index ~D more than 5." index))
    (if vp
        (setf (slot (deref (slot v110 'mid) index) 'pixel) val)
        (slot (deref (slot v110 'mid) index) 'pixel))))
(defun gtk::Style.mid.red (ptr index &optional (val nil vp))
  (let ((v111
         (if (typep ptr '(alien (* GtkStyle))) ptr (cast ptr (* GtkStyle)))))
    (declare (type (alien (* GtkStyle)) v111))
    (unless (< index 5) (error "struct array index ~D more than 5." index))
    (if vp
        (setf (slot (deref (slot v111 'mid) index) 'red) val)
        (slot (deref (slot v111 'mid) index) 'red))))
(defun gtk::Style.mid.green (ptr index &optional (val nil vp))
  (let ((v112
         (if (typep ptr '(alien (* GtkStyle))) ptr (cast ptr (* GtkStyle)))))
    (declare (type (alien (* GtkStyle)) v112))
    (unless (< index 5) (error "struct array index ~D more than 5." index))
    (if vp
        (setf (slot (deref (slot v112 'mid) index) 'green) val)
        (slot (deref (slot v112 'mid) index) 'green))))
(defun gtk::Style.mid.blue (ptr index &optional (val nil vp))
  (let ((v113
         (if (typep ptr '(alien (* GtkStyle))) ptr (cast ptr (* GtkStyle)))))
    (declare (type (alien (* GtkStyle)) v113))
    (unless (< index 5) (error "struct array index ~D more than 5." index))
    (if vp
        (setf (slot (deref (slot v113 'mid) index) 'blue) val)
        (slot (deref (slot v113 'mid) index) 'blue))))
(defun gtk::Style.text.pixel (ptr index &optional (val nil vp))
  (let ((v114
         (if (typep ptr '(alien (* GtkStyle))) ptr (cast ptr (* GtkStyle)))))
    (declare (type (alien (* GtkStyle)) v114))
    (unless (< index 5) (error "struct array index ~D more than 5." index))
    (if vp
        (setf (slot (deref (slot v114 'text) index) 'pixel) val)
        (slot (deref (slot v114 'text) index) 'pixel))))
(defun gtk::Style.text.red (ptr index &optional (val nil vp))
  (let ((v115
         (if (typep ptr '(alien (* GtkStyle))) ptr (cast ptr (* GtkStyle)))))
    (declare (type (alien (* GtkStyle)) v115))
    (unless (< index 5) (error "struct array index ~D more than 5." index))
    (if vp
        (setf (slot (deref (slot v115 'text) index) 'red) val)
        (slot (deref (slot v115 'text) index) 'red))))
(defun gtk::Style.text.green (ptr index &optional (val nil vp))
  (let ((v116
         (if (typep ptr '(alien (* GtkStyle))) ptr (cast ptr (* GtkStyle)))))
    (declare (type (alien (* GtkStyle)) v116))
    (unless (< index 5) (error "struct array index ~D more than 5." index))
    (if vp
        (setf (slot (deref (slot v116 'text) index) 'green) val)
        (slot (deref (slot v116 'text) index) 'green))))
(defun gtk::Style.text.blue (ptr index &optional (val nil vp))
  (let ((v117
         (if (typep ptr '(alien (* GtkStyle))) ptr (cast ptr (* GtkStyle)))))
    (declare (type (alien (* GtkStyle)) v117))
    (unless (< index 5) (error "struct array index ~D more than 5." index))
    (if vp
        (setf (slot (deref (slot v117 'text) index) 'blue) val)
        (slot (deref (slot v117 'text) index) 'blue))))
(defun gtk::Style.base.pixel (ptr index &optional (val nil vp))
  (let ((v118
         (if (typep ptr '(alien (* GtkStyle))) ptr (cast ptr (* GtkStyle)))))
    (declare (type (alien (* GtkStyle)) v118))
    (unless (< index 5) (error "struct array index ~D more than 5." index))
    (if vp
        (setf (slot (deref (slot v118 'base) index) 'pixel) val)
        (slot (deref (slot v118 'base) index) 'pixel))))
(defun gtk::Style.base.red (ptr index &optional (val nil vp))
  (let ((v119
         (if (typep ptr '(alien (* GtkStyle))) ptr (cast ptr (* GtkStyle)))))
    (declare (type (alien (* GtkStyle)) v119))
    (unless (< index 5) (error "struct array index ~D more than 5." index))
    (if vp
        (setf (slot (deref (slot v119 'base) index) 'red) val)
        (slot (deref (slot v119 'base) index) 'red))))
(defun gtk::Style.base.green (ptr index &optional (val nil vp))
  (let ((v120
         (if (typep ptr '(alien (* GtkStyle))) ptr (cast ptr (* GtkStyle)))))
    (declare (type (alien (* GtkStyle)) v120))
    (unless (< index 5) (error "struct array index ~D more than 5." index))
    (if vp
        (setf (slot (deref (slot v120 'base) index) 'green) val)
        (slot (deref (slot v120 'base) index) 'green))))
(defun gtk::Style.base.blue (ptr index &optional (val nil vp))
  (let ((v121
         (if (typep ptr '(alien (* GtkStyle))) ptr (cast ptr (* GtkStyle)))))
    (declare (type (alien (* GtkStyle)) v121))
    (unless (< index 5) (error "struct array index ~D more than 5." index))
    (if vp
        (setf (slot (deref (slot v121 'base) index) 'blue) val)
        (slot (deref (slot v121 'base) index) 'blue))))
(defun gtk::Style.text-aa.pixel (ptr index &optional (val nil vp))
  (let ((v122
         (if (typep ptr '(alien (* GtkStyle))) ptr (cast ptr (* GtkStyle)))))
    (declare (type (alien (* GtkStyle)) v122))
    (unless (< index 5) (error "struct array index ~D more than 5." index))
    (if vp
        (setf (slot (deref (slot v122 'text_aa) index) 'pixel) val)
        (slot (deref (slot v122 'text_aa) index) 'pixel))))
(defun gtk::Style.text-aa.red (ptr index &optional (val nil vp))
  (let ((v123
         (if (typep ptr '(alien (* GtkStyle))) ptr (cast ptr (* GtkStyle)))))
    (declare (type (alien (* GtkStyle)) v123))
    (unless (< index 5) (error "struct array index ~D more than 5." index))
    (if vp
        (setf (slot (deref (slot v123 'text_aa) index) 'red) val)
        (slot (deref (slot v123 'text_aa) index) 'red))))
(defun gtk::Style.text-aa.green (ptr index &optional (val nil vp))
  (let ((v124
         (if (typep ptr '(alien (* GtkStyle))) ptr (cast ptr (* GtkStyle)))))
    (declare (type (alien (* GtkStyle)) v124))
    (unless (< index 5) (error "struct array index ~D more than 5." index))
    (if vp
        (setf (slot (deref (slot v124 'text_aa) index) 'green) val)
        (slot (deref (slot v124 'text_aa) index) 'green))))
(defun gtk::Style.text-aa.blue (ptr index &optional (val nil vp))
  (let ((v125
         (if (typep ptr '(alien (* GtkStyle))) ptr (cast ptr (* GtkStyle)))))
    (declare (type (alien (* GtkStyle)) v125))
    (unless (< index 5) (error "struct array index ~D more than 5." index))
    (if vp
        (setf (slot (deref (slot v125 'text_aa) index) 'blue) val)
        (slot (deref (slot v125 'text_aa) index) 'blue))))
(defun gtk::Style.black.pixel (ptr &optional (val nil vp))
  (let ((v126
         (if (typep ptr '(alien (* GtkStyle))) ptr (cast ptr (* GtkStyle)))))
    (declare (type (alien (* GtkStyle)) v126))
    (if vp
        (setf (slot (slot v126 'black) 'pixel) val)
        (slot (slot v126 'black) 'pixel))))
(defun gtk::Style.black.red (ptr &optional (val nil vp))
  (let ((v127
         (if (typep ptr '(alien (* GtkStyle))) ptr (cast ptr (* GtkStyle)))))
    (declare (type (alien (* GtkStyle)) v127))
    (if vp
        (setf (slot (slot v127 'black) 'red) val)
        (slot (slot v127 'black) 'red))))
(defun gtk::Style.black.green (ptr &optional (val nil vp))
  (let ((v128
         (if (typep ptr '(alien (* GtkStyle))) ptr (cast ptr (* GtkStyle)))))
    (declare (type (alien (* GtkStyle)) v128))
    (if vp
        (setf (slot (slot v128 'black) 'green) val)
        (slot (slot v128 'black) 'green))))
(defun gtk::Style.black.blue (ptr &optional (val nil vp))
  (let ((v129
         (if (typep ptr '(alien (* GtkStyle))) ptr (cast ptr (* GtkStyle)))))
    (declare (type (alien (* GtkStyle)) v129))
    (if vp
        (setf (slot (slot v129 'black) 'blue) val)
        (slot (slot v129 'black) 'blue))))
(defun gtk::Style.white.pixel (ptr &optional (val nil vp))
  (let ((v130
         (if (typep ptr '(alien (* GtkStyle))) ptr (cast ptr (* GtkStyle)))))
    (declare (type (alien (* GtkStyle)) v130))
    (if vp
        (setf (slot (slot v130 'white) 'pixel) val)
        (slot (slot v130 'white) 'pixel))))
(defun gtk::Style.white.red (ptr &optional (val nil vp))
  (let ((v131
         (if (typep ptr '(alien (* GtkStyle))) ptr (cast ptr (* GtkStyle)))))
    (declare (type (alien (* GtkStyle)) v131))
    (if vp
        (setf (slot (slot v131 'white) 'red) val)
        (slot (slot v131 'white) 'red))))
(defun gtk::Style.white.green (ptr &optional (val nil vp))
  (let ((v132
         (if (typep ptr '(alien (* GtkStyle))) ptr (cast ptr (* GtkStyle)))))
    (declare (type (alien (* GtkStyle)) v132))
    (if vp
        (setf (slot (slot v132 'white) 'green) val)
        (slot (slot v132 'white) 'green))))
(defun gtk::Style.white.blue (ptr &optional (val nil vp))
  (let ((v133
         (if (typep ptr '(alien (* GtkStyle))) ptr (cast ptr (* GtkStyle)))))
    (declare (type (alien (* GtkStyle)) v133))
    (if vp
        (setf (slot (slot v133 'white) 'blue) val)
        (slot (slot v133 'white) 'blue))))
(defun gtk::Style.font-desc (ptr &optional (val nil vp))
  (let ((v134
         (if (typep ptr '(alien (* GtkStyle))) ptr (cast ptr (* GtkStyle)))))
    (declare (type (alien (* GtkStyle)) v134))
    (if vp (setf (slot v134 'font_desc) val) (slot v134 'font_desc))))
(defun gtk::Style.xthickness (ptr &optional (val nil vp))
  (let ((v135
         (if (typep ptr '(alien (* GtkStyle))) ptr (cast ptr (* GtkStyle)))))
    (declare (type (alien (* GtkStyle)) v135))
    (if vp (setf (slot v135 'xthickness) val) (slot v135 'xthickness))))
(defun gtk::Style.ythickness (ptr &optional (val nil vp))
  (let ((v136
         (if (typep ptr '(alien (* GtkStyle))) ptr (cast ptr (* GtkStyle)))))
    (declare (type (alien (* GtkStyle)) v136))
    (if vp (setf (slot v136 'ythickness) val) (slot v136 'ythickness))))
(defun gtk::Style.fg-gc (ptr index &optional (val nil vp))
  (let ((v137
         (if (typep ptr '(alien (* GtkStyle))) ptr (cast ptr (* GtkStyle)))))
    (declare (type (alien (* GtkStyle)) v137))
    (unless (< index 5) (error "struct array index ~D more than 5." index))
    (if vp
        (setf (deref (slot v137 'fg_gc) index) val)
        (deref (slot v137 'fg_gc) index))))
(defun gtk::Style.bg-gc (ptr index &optional (val nil vp))
  (let ((v138
         (if (typep ptr '(alien (* GtkStyle))) ptr (cast ptr (* GtkStyle)))))
    (declare (type (alien (* GtkStyle)) v138))
    (unless (< index 5) (error "struct array index ~D more than 5." index))
    (if vp
        (setf (deref (slot v138 'bg_gc) index) val)
        (deref (slot v138 'bg_gc) index))))
(defun gtk::Style.light-gc (ptr index &optional (val nil vp))
  (let ((v139
         (if (typep ptr '(alien (* GtkStyle))) ptr (cast ptr (* GtkStyle)))))
    (declare (type (alien (* GtkStyle)) v139))
    (unless (< index 5) (error "struct array index ~D more than 5." index))
    (if vp
        (setf (deref (slot v139 'light_gc) index) val)
        (deref (slot v139 'light_gc) index))))
(defun gtk::Style.dark-gc (ptr index &optional (val nil vp))
  (let ((v140
         (if (typep ptr '(alien (* GtkStyle))) ptr (cast ptr (* GtkStyle)))))
    (declare (type (alien (* GtkStyle)) v140))
    (unless (< index 5) (error "struct array index ~D more than 5." index))
    (if vp
        (setf (deref (slot v140 'dark_gc) index) val)
        (deref (slot v140 'dark_gc) index))))
(defun gtk::Style.mid-gc (ptr index &optional (val nil vp))
  (let ((v141
         (if (typep ptr '(alien (* GtkStyle))) ptr (cast ptr (* GtkStyle)))))
    (declare (type (alien (* GtkStyle)) v141))
    (unless (< index 5) (error "struct array index ~D more than 5." index))
    (if vp
        (setf (deref (slot v141 'mid_gc) index) val)
        (deref (slot v141 'mid_gc) index))))
(defun gtk::Style.text-gc (ptr index &optional (val nil vp))
  (let ((v142
         (if (typep ptr '(alien (* GtkStyle))) ptr (cast ptr (* GtkStyle)))))
    (declare (type (alien (* GtkStyle)) v142))
    (unless (< index 5) (error "struct array index ~D more than 5." index))
    (if vp
        (setf (deref (slot v142 'text_gc) index) val)
        (deref (slot v142 'text_gc) index))))
(defun gtk::Style.base-gc (ptr index &optional (val nil vp))
  (let ((v143
         (if (typep ptr '(alien (* GtkStyle))) ptr (cast ptr (* GtkStyle)))))
    (declare (type (alien (* GtkStyle)) v143))
    (unless (< index 5) (error "struct array index ~D more than 5." index))
    (if vp
        (setf (deref (slot v143 'base_gc) index) val)
        (deref (slot v143 'base_gc) index))))
(defun gtk::Style.text-aa-gc (ptr index &optional (val nil vp))
  (let ((v144
         (if (typep ptr '(alien (* GtkStyle))) ptr (cast ptr (* GtkStyle)))))
    (declare (type (alien (* GtkStyle)) v144))
    (unless (< index 5) (error "struct array index ~D more than 5." index))
    (if vp
        (setf (deref (slot v144 'text_aa_gc) index) val)
        (deref (slot v144 'text_aa_gc) index))))
(defun gtk::Style.black-gc (ptr &optional (val nil vp))
  (let ((v145
         (if (typep ptr '(alien (* GtkStyle))) ptr (cast ptr (* GtkStyle)))))
    (declare (type (alien (* GtkStyle)) v145))
    (if vp (setf (slot v145 'black_gc) val) (slot v145 'black_gc))))
(defun gtk::Style.white-gc (ptr &optional (val nil vp))
  (let ((v146
         (if (typep ptr '(alien (* GtkStyle))) ptr (cast ptr (* GtkStyle)))))
    (declare (type (alien (* GtkStyle)) v146))
    (if vp (setf (slot v146 'white_gc) val) (slot v146 'white_gc))))
(defun gtk::Style.bg-pixmap (ptr index &optional (val nil vp))
  (let ((v147
         (if (typep ptr '(alien (* GtkStyle))) ptr (cast ptr (* GtkStyle)))))
    (declare (type (alien (* GtkStyle)) v147))
    (unless (< index 5) (error "struct array index ~D more than 5." index))
    (if vp
        (setf (deref (slot v147 'bg_pixmap) index) val)
        (deref (slot v147 'bg_pixmap) index))))
(defun gtk::RcStyle.name (ptr &optional (val nil vp))
  (let ((v148
         (if (typep ptr '(alien (* GtkRcStyle)))
             ptr
             (cast ptr (* GtkRcStyle)))))
    (declare (type (alien (* GtkRcStyle)) v148))
    (if vp (setf (slot v148 'name) val) (slot v148 'name))))
(defun gtk::RcStyle.bg-pixmap-name (ptr index &optional (val nil vp))
  (let ((v149
         (if (typep ptr '(alien (* GtkRcStyle)))
             ptr
             (cast ptr (* GtkRcStyle)))))
    (declare (type (alien (* GtkRcStyle)) v149))
    (unless (< index 5) (error "struct array index ~D more than 5." index))
    (if vp
        (setf (deref (slot v149 'bg_pixmap_name) index) val)
        (deref (slot v149 'bg_pixmap_name) index))))
(defun gtk::RcStyle.font-desc (ptr &optional (val nil vp))
  (let ((v150
         (if (typep ptr '(alien (* GtkRcStyle)))
             ptr
             (cast ptr (* GtkRcStyle)))))
    (declare (type (alien (* GtkRcStyle)) v150))
    (if vp (setf (slot v150 'font_desc) val) (slot v150 'font_desc))))
(defun gtk::RcStyle.color-flags (ptr index &optional (val nil vp))
  (let ((v151
         (if (typep ptr '(alien (* GtkRcStyle)))
             ptr
             (cast ptr (* GtkRcStyle)))))
    (declare (type (alien (* GtkRcStyle)) v151))
    (unless (< index 5) (error "struct array index ~D more than 5." index))
    (if vp
        (setf (deref (slot v151 'color_flags) index) val)
        (deref (slot v151 'color_flags) index))))
(defun gtk::RcStyle.fg.pixel (ptr index &optional (val nil vp))
  (let ((v152
         (if (typep ptr '(alien (* GtkRcStyle)))
             ptr
             (cast ptr (* GtkRcStyle)))))
    (declare (type (alien (* GtkRcStyle)) v152))
    (unless (< index 5) (error "struct array index ~D more than 5." index))
    (if vp
        (setf (slot (deref (slot v152 'fg) index) 'pixel) val)
        (slot (deref (slot v152 'fg) index) 'pixel))))
(defun gtk::RcStyle.fg.red (ptr index &optional (val nil vp))
  (let ((v153
         (if (typep ptr '(alien (* GtkRcStyle)))
             ptr
             (cast ptr (* GtkRcStyle)))))
    (declare (type (alien (* GtkRcStyle)) v153))
    (unless (< index 5) (error "struct array index ~D more than 5." index))
    (if vp
        (setf (slot (deref (slot v153 'fg) index) 'red) val)
        (slot (deref (slot v153 'fg) index) 'red))))
(defun gtk::RcStyle.fg.green (ptr index &optional (val nil vp))
  (let ((v154
         (if (typep ptr '(alien (* GtkRcStyle)))
             ptr
             (cast ptr (* GtkRcStyle)))))
    (declare (type (alien (* GtkRcStyle)) v154))
    (unless (< index 5) (error "struct array index ~D more than 5." index))
    (if vp
        (setf (slot (deref (slot v154 'fg) index) 'green) val)
        (slot (deref (slot v154 'fg) index) 'green))))
(defun gtk::RcStyle.fg.blue (ptr index &optional (val nil vp))
  (let ((v155
         (if (typep ptr '(alien (* GtkRcStyle)))
             ptr
             (cast ptr (* GtkRcStyle)))))
    (declare (type (alien (* GtkRcStyle)) v155))
    (unless (< index 5) (error "struct array index ~D more than 5." index))
    (if vp
        (setf (slot (deref (slot v155 'fg) index) 'blue) val)
        (slot (deref (slot v155 'fg) index) 'blue))))
(defun gtk::RcStyle.bg.pixel (ptr index &optional (val nil vp))
  (let ((v156
         (if (typep ptr '(alien (* GtkRcStyle)))
             ptr
             (cast ptr (* GtkRcStyle)))))
    (declare (type (alien (* GtkRcStyle)) v156))
    (unless (< index 5) (error "struct array index ~D more than 5." index))
    (if vp
        (setf (slot (deref (slot v156 'bg) index) 'pixel) val)
        (slot (deref (slot v156 'bg) index) 'pixel))))
(defun gtk::RcStyle.bg.red (ptr index &optional (val nil vp))
  (let ((v157
         (if (typep ptr '(alien (* GtkRcStyle)))
             ptr
             (cast ptr (* GtkRcStyle)))))
    (declare (type (alien (* GtkRcStyle)) v157))
    (unless (< index 5) (error "struct array index ~D more than 5." index))
    (if vp
        (setf (slot (deref (slot v157 'bg) index) 'red) val)
        (slot (deref (slot v157 'bg) index) 'red))))
(defun gtk::RcStyle.bg.green (ptr index &optional (val nil vp))
  (let ((v158
         (if (typep ptr '(alien (* GtkRcStyle)))
             ptr
             (cast ptr (* GtkRcStyle)))))
    (declare (type (alien (* GtkRcStyle)) v158))
    (unless (< index 5) (error "struct array index ~D more than 5." index))
    (if vp
        (setf (slot (deref (slot v158 'bg) index) 'green) val)
        (slot (deref (slot v158 'bg) index) 'green))))
(defun gtk::RcStyle.bg.blue (ptr index &optional (val nil vp))
  (let ((v159
         (if (typep ptr '(alien (* GtkRcStyle)))
             ptr
             (cast ptr (* GtkRcStyle)))))
    (declare (type (alien (* GtkRcStyle)) v159))
    (unless (< index 5) (error "struct array index ~D more than 5." index))
    (if vp
        (setf (slot (deref (slot v159 'bg) index) 'blue) val)
        (slot (deref (slot v159 'bg) index) 'blue))))
(defun gtk::RcStyle.text.pixel (ptr index &optional (val nil vp))
  (let ((v160
         (if (typep ptr '(alien (* GtkRcStyle)))
             ptr
             (cast ptr (* GtkRcStyle)))))
    (declare (type (alien (* GtkRcStyle)) v160))
    (unless (< index 5) (error "struct array index ~D more than 5." index))
    (if vp
        (setf (slot (deref (slot v160 'text) index) 'pixel) val)
        (slot (deref (slot v160 'text) index) 'pixel))))
(defun gtk::RcStyle.text.red (ptr index &optional (val nil vp))
  (let ((v161
         (if (typep ptr '(alien (* GtkRcStyle)))
             ptr
             (cast ptr (* GtkRcStyle)))))
    (declare (type (alien (* GtkRcStyle)) v161))
    (unless (< index 5) (error "struct array index ~D more than 5." index))
    (if vp
        (setf (slot (deref (slot v161 'text) index) 'red) val)
        (slot (deref (slot v161 'text) index) 'red))))
(defun gtk::RcStyle.text.green (ptr index &optional (val nil vp))
  (let ((v162
         (if (typep ptr '(alien (* GtkRcStyle)))
             ptr
             (cast ptr (* GtkRcStyle)))))
    (declare (type (alien (* GtkRcStyle)) v162))
    (unless (< index 5) (error "struct array index ~D more than 5." index))
    (if vp
        (setf (slot (deref (slot v162 'text) index) 'green) val)
        (slot (deref (slot v162 'text) index) 'green))))
(defun gtk::RcStyle.text.blue (ptr index &optional (val nil vp))
  (let ((v163
         (if (typep ptr '(alien (* GtkRcStyle)))
             ptr
             (cast ptr (* GtkRcStyle)))))
    (declare (type (alien (* GtkRcStyle)) v163))
    (unless (< index 5) (error "struct array index ~D more than 5." index))
    (if vp
        (setf (slot (deref (slot v163 'text) index) 'blue) val)
        (slot (deref (slot v163 'text) index) 'blue))))
(defun gtk::RcStyle.base.pixel (ptr index &optional (val nil vp))
  (let ((v164
         (if (typep ptr '(alien (* GtkRcStyle)))
             ptr
             (cast ptr (* GtkRcStyle)))))
    (declare (type (alien (* GtkRcStyle)) v164))
    (unless (< index 5) (error "struct array index ~D more than 5." index))
    (if vp
        (setf (slot (deref (slot v164 'base) index) 'pixel) val)
        (slot (deref (slot v164 'base) index) 'pixel))))
(defun gtk::RcStyle.base.red (ptr index &optional (val nil vp))
  (let ((v165
         (if (typep ptr '(alien (* GtkRcStyle)))
             ptr
             (cast ptr (* GtkRcStyle)))))
    (declare (type (alien (* GtkRcStyle)) v165))
    (unless (< index 5) (error "struct array index ~D more than 5." index))
    (if vp
        (setf (slot (deref (slot v165 'base) index) 'red) val)
        (slot (deref (slot v165 'base) index) 'red))))
(defun gtk::RcStyle.base.green (ptr index &optional (val nil vp))
  (let ((v166
         (if (typep ptr '(alien (* GtkRcStyle)))
             ptr
             (cast ptr (* GtkRcStyle)))))
    (declare (type (alien (* GtkRcStyle)) v166))
    (unless (< index 5) (error "struct array index ~D more than 5." index))
    (if vp
        (setf (slot (deref (slot v166 'base) index) 'green) val)
        (slot (deref (slot v166 'base) index) 'green))))
(defun gtk::RcStyle.base.blue (ptr index &optional (val nil vp))
  (let ((v167
         (if (typep ptr '(alien (* GtkRcStyle)))
             ptr
             (cast ptr (* GtkRcStyle)))))
    (declare (type (alien (* GtkRcStyle)) v167))
    (unless (< index 5) (error "struct array index ~D more than 5." index))
    (if vp
        (setf (slot (deref (slot v167 'base) index) 'blue) val)
        (slot (deref (slot v167 'base) index) 'blue))))
(defun gtk::RcStyle.xthickness (ptr &optional (val nil vp))
  (let ((v168
         (if (typep ptr '(alien (* GtkRcStyle)))
             ptr
             (cast ptr (* GtkRcStyle)))))
    (declare (type (alien (* GtkRcStyle)) v168))
    (if vp (setf (slot v168 'xthickness) val) (slot v168 'xthickness))))
(defun gtk::RcStyle.ythickness (ptr &optional (val nil vp))
  (let ((v169
         (if (typep ptr '(alien (* GtkRcStyle)))
             ptr
             (cast ptr (* GtkRcStyle)))))
    (declare (type (alien (* GtkRcStyle)) v169))
    (if vp (setf (slot v169 'ythickness) val) (slot v169 'ythickness))))
(defun gtk::Widget.state (ptr &optional (val nil vp))
  (let ((v170
         (if (typep ptr '(alien (* GtkWidget))) ptr (cast ptr (* GtkWidget)))))
    (declare (type (alien (* GtkWidget)) v170))
    (if vp (setf (slot v170 'state) val) (slot v170 'state))))
(defun gtk::Widget.name (ptr &optional (val nil vp))
  (let ((v171
         (if (typep ptr '(alien (* GtkWidget))) ptr (cast ptr (* GtkWidget)))))
    (declare (type (alien (* GtkWidget)) v171))
    (if vp (setf (slot v171 'name) val) (slot v171 'name))))
(defun gtk::Widget.style (ptr &optional (val nil vp))
  (let ((v172
         (if (typep ptr '(alien (* GtkWidget))) ptr (cast ptr (* GtkWidget)))))
    (declare (type (alien (* GtkWidget)) v172))
    (if vp (setf (slot v172 'style) val) (slot v172 'style))))
(defun gtk::Widget.allocation.x (ptr &optional (val nil vp))
  (let ((v173
         (if (typep ptr '(alien (* GtkWidget))) ptr (cast ptr (* GtkWidget)))))
    (declare (type (alien (* GtkWidget)) v173))
    (if vp
        (setf (slot (slot v173 'allocation) 'x) val)
        (slot (slot v173 'allocation) 'x))))
(defun gtk::Widget.allocation.y (ptr &optional (val nil vp))
  (let ((v174
         (if (typep ptr '(alien (* GtkWidget))) ptr (cast ptr (* GtkWidget)))))
    (declare (type (alien (* GtkWidget)) v174))
    (if vp
        (setf (slot (slot v174 'allocation) 'y) val)
        (slot (slot v174 'allocation) 'y))))
(defun gtk::Widget.allocation.width (ptr &optional (val nil vp))
  (let ((v175
         (if (typep ptr '(alien (* GtkWidget))) ptr (cast ptr (* GtkWidget)))))
    (declare (type (alien (* GtkWidget)) v175))
    (if vp
        (setf (slot (slot v175 'allocation) 'width) val)
        (slot (slot v175 'allocation) 'width))))
(defun gtk::Widget.allocation.height (ptr &optional (val nil vp))
  (let ((v176
         (if (typep ptr '(alien (* GtkWidget))) ptr (cast ptr (* GtkWidget)))))
    (declare (type (alien (* GtkWidget)) v176))
    (if vp
        (setf (slot (slot v176 'allocation) 'height) val)
        (slot (slot v176 'allocation) 'height))))
(defun gtk::Widget.window (ptr &optional (val nil vp))
  (let ((v177
         (if (typep ptr '(alien (* GtkWidget))) ptr (cast ptr (* GtkWidget)))))
    (declare (type (alien (* GtkWidget)) v177))
    (if vp (setf (slot v177 'window) val) (slot v177 'window))))
(defun gtk::Widget.parent (ptr &optional (val nil vp))
  (let ((v178
         (if (typep ptr '(alien (* GtkWidget))) ptr (cast ptr (* GtkWidget)))))
    (declare (type (alien (* GtkWidget)) v178))
    (if vp (setf (slot v178 'parent) val) (slot v178 'parent))))
(defun gtk::MenuShell.children (ptr &optional (val nil vp))
  (let ((v179
         (if (typep ptr '(alien (* GtkMenuShell)))
             ptr
             (cast ptr (* GtkMenuShell)))))
    (declare (type (alien (* GtkMenuShell)) v179))
    (if vp (setf (slot v179 'children) val) (slot v179 'children))))
(defun gtk::Arrow.arrow-type (ptr &optional (val nil vp))
  (let ((v180
         (if (typep ptr '(alien (* GtkArrow))) ptr (cast ptr (* GtkArrow)))))
    (declare (type (alien (* GtkArrow)) v180))
    (if vp (setf (slot v180 'arrow_type) val) (slot v180 'arrow_type))))
(defun gtk::Arrow.shadow-type (ptr &optional (val nil vp))
  (let ((v181
         (if (typep ptr '(alien (* GtkArrow))) ptr (cast ptr (* GtkArrow)))))
    (declare (type (alien (* GtkArrow)) v181))
    (if vp (setf (slot v181 'shadow_type) val) (slot v181 'shadow_type))))
(defun gtk::Box.children (ptr &optional (val nil vp))
  (let ((v182 (if (typep ptr '(alien (* GtkBox))) ptr (cast ptr (* GtkBox)))))
    (declare (type (alien (* GtkBox)) v182))
    (if vp (setf (slot v182 'children) val) (slot v182 'children))))
(defun gtk::Box.spacing (ptr &optional (val nil vp))
  (let ((v183 (if (typep ptr '(alien (* GtkBox))) ptr (cast ptr (* GtkBox)))))
    (declare (type (alien (* GtkBox)) v183))
    (if vp (setf (slot v183 'spacing) val) (slot v183 'spacing))))
(defun gtk::Calendar.month (ptr &optional (val nil vp))
  (let ((v184
         (if (typep ptr '(alien (* GtkCalendar)))
             ptr
             (cast ptr (* GtkCalendar)))))
    (declare (type (alien (* GtkCalendar)) v184))
    (if vp (setf (slot v184 'month) val) (slot v184 'month))))
(defun gtk::Calendar.year (ptr &optional (val nil vp))
  (let ((v185
         (if (typep ptr '(alien (* GtkCalendar)))
             ptr
             (cast ptr (* GtkCalendar)))))
    (declare (type (alien (* GtkCalendar)) v185))
    (if vp (setf (slot v185 'year) val) (slot v185 'year))))
(defun gtk::Calendar.selected-day (ptr &optional (val nil vp))
  (let ((v186
         (if (typep ptr '(alien (* GtkCalendar)))
             ptr
             (cast ptr (* GtkCalendar)))))
    (declare (type (alien (* GtkCalendar)) v186))
    (if vp (setf (slot v186 'selected_day) val) (slot v186 'selected_day))))
(defun gtk::Calendar.num-marked-dates (ptr &optional (val nil vp))
  (let ((v187
         (if (typep ptr '(alien (* GtkCalendar)))
             ptr
             (cast ptr (* GtkCalendar)))))
    (declare (type (alien (* GtkCalendar)) v187))
    (if vp
        (setf (slot v187 'num_marked_dates) val)
        (slot v187 'num_marked_dates))))
(defun gtk::Calendar.marked-date (ptr index &optional (val nil vp))
  (let ((v188
         (if (typep ptr '(alien (* GtkCalendar)))
             ptr
             (cast ptr (* GtkCalendar)))))
    (declare (type (alien (* GtkCalendar)) v188))
    (unless (< index 31) (error "struct array index ~D more than 31." index))
    (if vp
        (setf (deref (slot v188 'marked_date) index) val)
        (deref (slot v188 'marked_date) index))))
(defun gtk::Dialog.vbox (ptr &optional (val nil vp))
  (let ((v189
         (if (typep ptr '(alien (* GtkDialog))) ptr (cast ptr (* GtkDialog)))))
    (declare (type (alien (* GtkDialog)) v189))
    (if vp (setf (slot v189 'vbox) val) (slot v189 'vbox))))
(defun gtk::Dialog.action-area (ptr &optional (val nil vp))
  (let ((v190
         (if (typep ptr '(alien (* GtkDialog))) ptr (cast ptr (* GtkDialog)))))
    (declare (type (alien (* GtkDialog)) v190))
    (if vp (setf (slot v190 'action_area) val) (slot v190 'action_area))))
(defun gtk::ColorSelectionDialog.colorsel (ptr &optional (val nil vp))
  (let ((v191
         (if (typep ptr '(alien (* GtkColorSelectionDialog)))
             ptr
             (cast ptr (* GtkColorSelectionDialog)))))
    (declare (type (alien (* GtkColorSelectionDialog)) v191))
    (if vp (setf (slot v191 'colorsel) val) (slot v191 'colorsel))))
(defun gtk::ColorSelectionDialog.ok-button (ptr &optional (val nil vp))
  (let ((v192
         (if (typep ptr '(alien (* GtkColorSelectionDialog)))
             ptr
             (cast ptr (* GtkColorSelectionDialog)))))
    (declare (type (alien (* GtkColorSelectionDialog)) v192))
    (if vp (setf (slot v192 'ok_button) val) (slot v192 'ok_button))))
(defun gtk::ColorSelectionDialog.cancel-button (ptr &optional (val nil vp))
  (let ((v193
         (if (typep ptr '(alien (* GtkColorSelectionDialog)))
             ptr
             (cast ptr (* GtkColorSelectionDialog)))))
    (declare (type (alien (* GtkColorSelectionDialog)) v193))
    (if vp (setf (slot v193 'cancel_button) val) (slot v193 'cancel_button))))
(defun gtk::ColorSelectionDialog.help-button (ptr &optional (val nil vp))
  (let ((v194
         (if (typep ptr '(alien (* GtkColorSelectionDialog)))
             ptr
             (cast ptr (* GtkColorSelectionDialog)))))
    (declare (type (alien (* GtkColorSelectionDialog)) v194))
    (if vp (setf (slot v194 'help_button) val) (slot v194 'help_button))))
(defun gtk::Combo.entry (ptr &optional (val nil vp))
  (let ((v195
         (if (typep ptr '(alien (* GtkCombo))) ptr (cast ptr (* GtkCombo)))))
    (declare (type (alien (* GtkCombo)) v195))
    (if vp (setf (slot v195 'entry) val) (slot v195 'entry))))
(defun gtk::Combo.list (ptr &optional (val nil vp))
  (let ((v196
         (if (typep ptr '(alien (* GtkCombo))) ptr (cast ptr (* GtkCombo)))))
    (declare (type (alien (* GtkCombo)) v196))
    (if vp (setf (slot v196 'list) val) (slot v196 'list))))
(defun gtk::FileSelection.dir-list (ptr &optional (val nil vp))
  (let ((v197
         (if (typep ptr '(alien (* GtkFileSelection)))
             ptr
             (cast ptr (* GtkFileSelection)))))
    (declare (type (alien (* GtkFileSelection)) v197))
    (if vp (setf (slot v197 'dir_list) val) (slot v197 'dir_list))))
(defun gtk::FileSelection.file-list (ptr &optional (val nil vp))
  (let ((v198
         (if (typep ptr '(alien (* GtkFileSelection)))
             ptr
             (cast ptr (* GtkFileSelection)))))
    (declare (type (alien (* GtkFileSelection)) v198))
    (if vp (setf (slot v198 'file_list) val) (slot v198 'file_list))))
(defun gtk::FileSelection.selection-entry (ptr &optional (val nil vp))
  (let ((v199
         (if (typep ptr '(alien (* GtkFileSelection)))
             ptr
             (cast ptr (* GtkFileSelection)))))
    (declare (type (alien (* GtkFileSelection)) v199))
    (if vp
        (setf (slot v199 'selection_entry) val)
        (slot v199 'selection_entry))))
(defun gtk::FileSelection.selection-text (ptr &optional (val nil vp))
  (let ((v200
         (if (typep ptr '(alien (* GtkFileSelection)))
             ptr
             (cast ptr (* GtkFileSelection)))))
    (declare (type (alien (* GtkFileSelection)) v200))
    (if vp (setf (slot v200 'selection_text) val) (slot v200 'selection_text))))
(defun gtk::FileSelection.main-vbox (ptr &optional (val nil vp))
  (let ((v201
         (if (typep ptr '(alien (* GtkFileSelection)))
             ptr
             (cast ptr (* GtkFileSelection)))))
    (declare (type (alien (* GtkFileSelection)) v201))
    (if vp (setf (slot v201 'main_vbox) val) (slot v201 'main_vbox))))
(defun gtk::FileSelection.ok-button (ptr &optional (val nil vp))
  (let ((v202
         (if (typep ptr '(alien (* GtkFileSelection)))
             ptr
             (cast ptr (* GtkFileSelection)))))
    (declare (type (alien (* GtkFileSelection)) v202))
    (if vp (setf (slot v202 'ok_button) val) (slot v202 'ok_button))))
(defun gtk::FileSelection.cancel-button (ptr &optional (val nil vp))
  (let ((v203
         (if (typep ptr '(alien (* GtkFileSelection)))
             ptr
             (cast ptr (* GtkFileSelection)))))
    (declare (type (alien (* GtkFileSelection)) v203))
    (if vp (setf (slot v203 'cancel_button) val) (slot v203 'cancel_button))))
(defun gtk::FileSelection.help-button (ptr &optional (val nil vp))
  (let ((v204
         (if (typep ptr '(alien (* GtkFileSelection)))
             ptr
             (cast ptr (* GtkFileSelection)))))
    (declare (type (alien (* GtkFileSelection)) v204))
    (if vp (setf (slot v204 'help_button) val) (slot v204 'help_button))))
(defun gtk::FileSelection.history-pulldown (ptr &optional (val nil vp))
  (let ((v205
         (if (typep ptr '(alien (* GtkFileSelection)))
             ptr
             (cast ptr (* GtkFileSelection)))))
    (declare (type (alien (* GtkFileSelection)) v205))
    (if vp
        (setf (slot v205 'history_pulldown) val)
        (slot v205 'history_pulldown))))
(defun gtk::FileSelection.history-menu (ptr &optional (val nil vp))
  (let ((v206
         (if (typep ptr '(alien (* GtkFileSelection)))
             ptr
             (cast ptr (* GtkFileSelection)))))
    (declare (type (alien (* GtkFileSelection)) v206))
    (if vp (setf (slot v206 'history_menu) val) (slot v206 'history_menu))))
(defun gtk::FileSelection.history-list (ptr &optional (val nil vp))
  (let ((v207
         (if (typep ptr '(alien (* GtkFileSelection)))
             ptr
             (cast ptr (* GtkFileSelection)))))
    (declare (type (alien (* GtkFileSelection)) v207))
    (if vp (setf (slot v207 'history_list) val) (slot v207 'history_list))))
(defun gtk::FileSelection.fileop-dialog (ptr &optional (val nil vp))
  (let ((v208
         (if (typep ptr '(alien (* GtkFileSelection)))
             ptr
             (cast ptr (* GtkFileSelection)))))
    (declare (type (alien (* GtkFileSelection)) v208))
    (if vp (setf (slot v208 'fileop_dialog) val) (slot v208 'fileop_dialog))))
(defun gtk::FileSelection.fileop-entry (ptr &optional (val nil vp))
  (let ((v209
         (if (typep ptr '(alien (* GtkFileSelection)))
             ptr
             (cast ptr (* GtkFileSelection)))))
    (declare (type (alien (* GtkFileSelection)) v209))
    (if vp (setf (slot v209 'fileop_entry) val) (slot v209 'fileop_entry))))
(defun gtk::FileSelection.fileop-file (ptr &optional (val nil vp))
  (let ((v210
         (if (typep ptr '(alien (* GtkFileSelection)))
             ptr
             (cast ptr (* GtkFileSelection)))))
    (declare (type (alien (* GtkFileSelection)) v210))
    (if vp (setf (slot v210 'fileop_file) val) (slot v210 'fileop_file))))
(defun gtk::FileSelection.cmpl-state (ptr &optional (val nil vp))
  (let ((v211
         (if (typep ptr '(alien (* GtkFileSelection)))
             ptr
             (cast ptr (* GtkFileSelection)))))
    (declare (type (alien (* GtkFileSelection)) v211))
    (if vp (setf (slot v211 'cmpl_state) val) (slot v211 'cmpl_state))))
(defun gtk::FileSelection.fileop-c-dir (ptr &optional (val nil vp))
  (let ((v212
         (if (typep ptr '(alien (* GtkFileSelection)))
             ptr
             (cast ptr (* GtkFileSelection)))))
    (declare (type (alien (* GtkFileSelection)) v212))
    (if vp (setf (slot v212 'fileop_c_dir) val) (slot v212 'fileop_c_dir))))
(defun gtk::FileSelection.fileop-del-file (ptr &optional (val nil vp))
  (let ((v213
         (if (typep ptr '(alien (* GtkFileSelection)))
             ptr
             (cast ptr (* GtkFileSelection)))))
    (declare (type (alien (* GtkFileSelection)) v213))
    (if vp
        (setf (slot v213 'fileop_del_file) val)
        (slot v213 'fileop_del_file))))
(defun gtk::FileSelection.fileop-ren-file (ptr &optional (val nil vp))
  (let ((v214
         (if (typep ptr '(alien (* GtkFileSelection)))
             ptr
             (cast ptr (* GtkFileSelection)))))
    (declare (type (alien (* GtkFileSelection)) v214))
    (if vp
        (setf (slot v214 'fileop_ren_file) val)
        (slot v214 'fileop_ren_file))))
(defun gtk::FileSelection.button-area (ptr &optional (val nil vp))
  (let ((v215
         (if (typep ptr '(alien (* GtkFileSelection)))
             ptr
             (cast ptr (* GtkFileSelection)))))
    (declare (type (alien (* GtkFileSelection)) v215))
    (if vp (setf (slot v215 'button_area) val) (slot v215 'button_area))))
(defun gtk::FileSelection.action-area (ptr &optional (val nil vp))
  (let ((v216
         (if (typep ptr '(alien (* GtkFileSelection)))
             ptr
             (cast ptr (* GtkFileSelection)))))
    (declare (type (alien (* GtkFileSelection)) v216))
    (if vp (setf (slot v216 'action_area) val) (slot v216 'action_area))))
(defun gtk::Fixed.children (ptr &optional (val nil vp))
  (let ((v217
         (if (typep ptr '(alien (* GtkFixed))) ptr (cast ptr (* GtkFixed)))))
    (declare (type (alien (* GtkFixed)) v217))
    (if vp (setf (slot v217 'children) val) (slot v217 'children))))
(defun gtk::FontSelectionDialog.ok-button (ptr &optional (val nil vp))
  (let ((v218
         (if (typep ptr '(alien (* GtkFontSelectionDialog)))
             ptr
             (cast ptr (* GtkFontSelectionDialog)))))
    (declare (type (alien (* GtkFontSelectionDialog)) v218))
    (if vp (setf (slot v218 'ok_button) val) (slot v218 'ok_button))))
(defun gtk::FontSelectionDialog.apply-button (ptr &optional (val nil vp))
  (let ((v219
         (if (typep ptr '(alien (* GtkFontSelectionDialog)))
             ptr
             (cast ptr (* GtkFontSelectionDialog)))))
    (declare (type (alien (* GtkFontSelectionDialog)) v219))
    (if vp (setf (slot v219 'apply_button) val) (slot v219 'apply_button))))
(defun gtk::FontSelectionDialog.cancel-button (ptr &optional (val nil vp))
  (let ((v220
         (if (typep ptr '(alien (* GtkFontSelectionDialog)))
             ptr
             (cast ptr (* GtkFontSelectionDialog)))))
    (declare (type (alien (* GtkFontSelectionDialog)) v220))
    (if vp (setf (slot v220 'cancel_button) val) (slot v220 'cancel_button))))
(defun gtk::HandleBox.shadow-type (ptr &optional (val nil vp))
  (let ((v221
         (if (typep ptr '(alien (* GtkHandleBox)))
             ptr
             (cast ptr (* GtkHandleBox)))))
    (declare (type (alien (* GtkHandleBox)) v221))
    (if vp (setf (slot v221 'shadow_type) val) (slot v221 'shadow_type))))
(defun gtk::Layout.bin-window (ptr &optional (val nil vp))
  (let ((v222
         (if (typep ptr '(alien (* GtkLayout))) ptr (cast ptr (* GtkLayout)))))
    (declare (type (alien (* GtkLayout)) v222))
    (if vp (setf (slot v222 'bin_window) val) (slot v222 'bin_window))))
(defun gtk::Table.children (ptr &optional (val nil vp))
  (let ((v223
         (if (typep ptr '(alien (* GtkTable))) ptr (cast ptr (* GtkTable)))))
    (declare (type (alien (* GtkTable)) v223))
    (if vp (setf (slot v223 'children) val) (slot v223 'children))))
(defun gtk::Table.rows (ptr &optional (val nil vp))
  (let ((v224
         (if (typep ptr '(alien (* GtkTable))) ptr (cast ptr (* GtkTable)))))
    (declare (type (alien (* GtkTable)) v224))
    (if vp (setf (slot v224 'rows) val) (slot v224 'rows))))
(defun gtk::Table.cols (ptr &optional (val nil vp))
  (let ((v225
         (if (typep ptr '(alien (* GtkTable))) ptr (cast ptr (* GtkTable)))))
    (declare (type (alien (* GtkTable)) v225))
    (if vp (setf (slot v225 'cols) val) (slot v225 'cols))))

;;; functions

(progn
 (defalien "g_list_alloc" (* t))
 (defun g::list-alloc () (|g_list_alloc|)))
(progn
 (defalien "g_list_free" void (a (* t)))
 (defun g::list-free (a) (|g_list_free| a)))
(progn
 (defalien "g_list_append" (* t) (a (* t)) (b gpointer))
 (defun g::list-append (a b) (|g_list_append| a b)))
(progn
 (defalien "g_list_prepend" (* t) (a (* t)) (b gpointer))
 (defun g::list-prepend (a b) (|g_list_prepend| a b)))
(progn
 (defalien "g_list_insert" (* t) (a (* t)) (b gpointer) (c gint))
 (defun g::list-insert (a b c) (|g_list_insert| a b c)))
(progn
 (defalien "g_list_insert_sorted"
           (* t)
           (a (* t))
           (b gpointer)
           (c GCompareFunc))
 (defun g::list-insert-sorted (a b c) (|g_list_insert_sorted| a b c)))
(progn
 (defalien "g_list_insert_before" (* t) (a (* t)) (b (* t)) (c gpointer))
 (defun g::list-insert-before (a b c) (|g_list_insert_before| a b c)))
(progn
 (defalien "g_list_concat" (* t) (a (* t)) (b (* t)))
 (defun g::list-concat (a b) (|g_list_concat| a b)))
(progn
 (defalien "g_list_remove" (* t) (a (* t)) (b gconstpointer))
 (defun g::list-remove (a b) (|g_list_remove| a b)))
(progn
 (defalien "g_list_remove_all" (* t) (a (* t)) (b gconstpointer))
 (defun g::list-remove-all (a b) (|g_list_remove_all| a b)))
(progn
 (defalien "g_list_reverse" (* t) (a (* t)))
 (defun g::list-reverse (a) (|g_list_reverse| a)))
(progn
 (defalien "g_list_copy" (* t) (a (* t)))
 (defun g::list-copy (a) (|g_list_copy| a)))
(progn
 (defalien "g_list_nth" (* t) (a (* t)) (b guint))
 (defun g::list-nth (a b) (|g_list_nth| a b)))
(progn
 (defalien "g_list_find" (* t) (a (* t)) (b gconstpointer))
 (defun g::list-find (a b) (|g_list_find| a b)))
(progn
 (defalien "g_list_position" gint (a (* t)) (b (* t)))
 (defun g::list-position (a b) (|g_list_position| a b)))
(progn
 (defalien "pango_coverage_ref" (* t) (a (* t)))
 (defun pango::coverage-ref (a) (|pango_coverage_ref| a)))
(progn
 (defalien "pango_coverage_unref" void (a (* t)))
 (defun pango::coverage-unref (a) (|pango_coverage_unref| a)))
(progn
 (defalien "pango_coverage_copy" (* t) (a (* t)))
 (defun pango::coverage-copy (a) (|pango_coverage_copy| a)))
(progn
 (defalien "pango_coverage_get" PangoCoverageLevel (a (* t)) (b int))
 (defun pango::coverage-get (a b) (|pango_coverage_get| a b)))
(progn
 (defalien "pango_coverage_set" void (a (* t)) (b int) (c PangoCoverageLevel))
 (defun pango::coverage-set (a b c) (|pango_coverage_set| a b c)))
(progn
 (defalien "pango_coverage_max" void (a (* t)) (b (* t)))
 (defun pango::coverage-max (a b) (|pango_coverage_max| a b)))
(progn
 (defalien "pango_coverage_to_bytes" void (a (* t)) (b (* t)) (c int :in-out))
 (defun pango::coverage-to-bytes (a b c) (|pango_coverage_to_bytes| a b c)))
(progn
 (defalien "pango_coverage_from_bytes" (* t) (a guchar :in-out) (b int))
 (defun pango::coverage-from-bytes (a b) (|pango_coverage_from_bytes| a b)))
(progn
 (defalien "g_signal_emit_by_name" void (a gpointer) (b c-string))
 (defun g::signal-emit-by-name (a b) (|g_signal_emit_by_name| a b)))
(progn
 (defalien "g_signal_stop_emission_by_name" void (a gpointer) (b c-string))
 (defun g::signal-stop-emission-by-name (a b)
   (|g_signal_stop_emission_by_name| a b)))
(progn
 (defalien "g_signal_connect_data"
           gulong
           (a gpointer)
           (b c-string)
           (c GCallback)
           (d gpointer)
           (e GClosureNotify)
           (f GConnectFlags))
 (defun g::signal-connect-data (a b c d e f)
   (|g_signal_connect_data| a b c d e f)))
(progn
 (defalien "g_signal_handler_disconnect" void (a gpointer) (b gulong))
 (defun g::signal-handler-disconnect (a b) (|g_signal_handler_disconnect| a b)))
(progn
 (defalien "g_object_set_property" void (a (* t)) (b c-string) (c (* t)))
 (defun g::object-set-property (a b c) (|g_object_set_property| a b c)))
(progn
 (defalien "g_object_get_property" void (a (* t)) (b c-string) (c (* t)))
 (defun g::object-get-property (a b c) (|g_object_get_property| a b c)))
(progn
 (defalien "g_object_freeze_notify" void (a (* t)))
 (defun g::object-freeze-notify (a) (|g_object_freeze_notify| a)))
(progn
 (defalien "g_object_notify" void (a (* t)) (b c-string))
 (defun g::object-notify (a b) (|g_object_notify| a b)))
(progn
 (defalien "g_object_thaw_notify" void (a (* t)))
 (defun g::object-thaw-notify (a) (|g_object_thaw_notify| a)))
(progn
 (defalien "g_object_unref" void (a gpointer))
 (defun g::object-unref (a) (|g_object_unref| a)))
(progn
 (defalien "g_object_get_data" gpointer (a (* t)) (b c-string))
 (defun g::object-get-data (a b) (|g_object_get_data| a b)))
(progn
 (defalien "g_object_set_data" void (a (* t)) (b c-string) (c gpointer))
 (defun g::object-set-data (a b c) (|g_object_set_data| a b c)))
(progn
 (defalien "pango_language_from_string" (* t) (a c-string))
 (defun pango::language-from-string (a) (|pango_language_from_string| a)))
(progn
 (defalien "pango_language_matches" gboolean (a (* t)) (b c-string))
 (defun pango::language-matches (a b)
   (let ((v226 (|pango_language_matches| a b)))
     (if (= v226 1) t nil))))
(progn
 (defalien "pango_font_description_new" (* t))
 (defun pango::font-description-new () (|pango_font_description_new|)))
(progn
 (defalien "pango_font_description_copy" (* t) (a (* t)))
 (defun pango::font-description-copy (a) (|pango_font_description_copy| a)))
(progn
 (defalien "pango_font_description_copy_static" (* t) (a (* t)))
 (defun pango::font-description-copy-static (a)
   (|pango_font_description_copy_static| a)))
(progn
 (defalien "pango_font_description_hash" guint (a (* t)))
 (defun pango::font-description-hash (a) (|pango_font_description_hash| a)))
(progn
 (defalien "pango_font_description_equal" gboolean (a (* t)) (b (* t)))
 (defun pango::font-description-equal (a b)
   (let ((v227 (|pango_font_description_equal| a b)))
     (if (= v227 1) t nil))))
(progn
 (defalien "pango_font_description_free" void (a (* t)))
 (defun pango::font-description-free (a) (|pango_font_description_free| a)))
(progn
 (defalien "pango_font_descriptions_free" void (a (* t)) (b int))
 (defun pango::font-descriptions-free (a b)
   (|pango_font_descriptions_free| a b)))
(progn
 (defalien "pango_font_description_set_family" void (a (* t)) (b c-string))
 (defun pango::font-description-set-family (a b)
   (|pango_font_description_set_family| a b)))
(progn
 (defalien "pango_font_description_set_family_static"
           void
           (a (* t))
           (b c-string))
 (defun pango::font-description-set-family-static (a b)
   (|pango_font_description_set_family_static| a b)))
(progn
 (defalien "pango_font_description_get_family" c-string (a (* t)))
 (defun pango::font-description-get-family (a)
   (|pango_font_description_get_family| a)))
(progn
 (defalien "pango_font_description_set_style" void (a (* t)) (b PangoStyle))
 (defun pango::font-description-set-style (a b)
   (|pango_font_description_set_style| a b)))
(progn
 (defalien "pango_font_description_get_style" PangoStyle (a (* t)))
 (defun pango::font-description-get-style (a)
   (|pango_font_description_get_style| a)))
(progn
 (defalien "pango_font_description_set_variant"
           void
           (a (* t))
           (b PangoVariant))
 (defun pango::font-description-set-variant (a b)
   (|pango_font_description_set_variant| a b)))
(progn
 (defalien "pango_font_description_get_variant" PangoVariant (a (* t)))
 (defun pango::font-description-get-variant (a)
   (|pango_font_description_get_variant| a)))
(progn
 (defalien "pango_font_description_set_weight" void (a (* t)) (b PangoWeight))
 (defun pango::font-description-set-weight (a b)
   (|pango_font_description_set_weight| a b)))
(progn
 (defalien "pango_font_description_get_weight" PangoWeight (a (* t)))
 (defun pango::font-description-get-weight (a)
   (|pango_font_description_get_weight| a)))
(progn
 (defalien "pango_font_description_set_stretch"
           void
           (a (* t))
           (b PangoStretch))
 (defun pango::font-description-set-stretch (a b)
   (|pango_font_description_set_stretch| a b)))
(progn
 (defalien "pango_font_description_get_stretch" PangoStretch (a (* t)))
 (defun pango::font-description-get-stretch (a)
   (|pango_font_description_get_stretch| a)))
(progn
 (defalien "pango_font_description_set_size" void (a (* t)) (b gint))
 (defun pango::font-description-set-size (a b)
   (|pango_font_description_set_size| a b)))
(progn
 (defalien "pango_font_description_get_size" gint (a (* t)))
 (defun pango::font-description-get-size (a)
   (|pango_font_description_get_size| a)))
(progn
 (defalien "pango_font_description_get_set_fields" PangoFontMask (a (* t)))
 (defun pango::font-description-get-set-fields (a)
   (|pango_font_description_get_set_fields| a)))
(progn
 (defalien "pango_font_description_unset_fields"
           void
           (a (* t))
           (b PangoFontMask))
 (defun pango::font-description-unset-fields (a b)
   (|pango_font_description_unset_fields| a b)))
(progn
 (defalien "pango_font_description_merge"
           void
           (a (* t))
           (b (* t))
           (c gboolean))
 (defun pango::font-description-merge (a b c)
   (|pango_font_description_merge| a b (if c (if (eq c 0) 0 1) 0))))
(progn
 (defalien "pango_font_description_merge_static"
           void
           (a (* t))
           (b (* t))
           (c gboolean))
 (defun pango::font-description-merge-static (a b c)
   (|pango_font_description_merge_static| a b (if c (if (eq c 0) 0 1) 0))))
(progn
 (defalien "pango_font_description_better_match"
           gboolean
           (a (* t))
           (b (* t))
           (c (* t)))
 (defun pango::font-description-better-match (a b c)
   (let ((v228 (|pango_font_description_better_match| a b c)))
     (if (= v228 1) t nil))))
(progn
 (defalien "pango_font_description_from_string" (* t) (a c-string))
 (defun pango::font-description-from-string (a)
   (|pango_font_description_from_string| a)))
(progn
 (defalien "pango_font_description_to_string" c-string (a (* t)))
 (defun pango::font-description-to-string (a)
   (|pango_font_description_to_string| a)))
(progn
 (defalien "pango_font_description_to_filename" c-string (a (* t)))
 (defun pango::font-description-to-filename (a)
   (|pango_font_description_to_filename| a)))
(progn
 (defalien "pango_font_metrics_get_type" GType)
 (defun pango::font-metrics-get-type () (|pango_font_metrics_get_type|)))
(progn
 (defalien "pango_font_metrics_ref" (* t) (a (* t)))
 (defun pango::font-metrics-ref (a) (|pango_font_metrics_ref| a)))
(progn
 (defalien "pango_font_metrics_unref" void (a (* t)))
 (defun pango::font-metrics-unref (a) (|pango_font_metrics_unref| a)))
(progn
 (defalien "pango_font_metrics_get_ascent" int (a (* t)))
 (defun pango::font-metrics-get-ascent (a) (|pango_font_metrics_get_ascent| a)))
(progn
 (defalien "pango_font_metrics_get_descent" int (a (* t)))
 (defun pango::font-metrics-get-descent (a)
   (|pango_font_metrics_get_descent| a)))
(progn
 (defalien "pango_font_metrics_get_approximate_char_width" int (a (* t)))
 (defun pango::font-metrics-get-approximate-char-width (a)
   (|pango_font_metrics_get_approximate_char_width| a)))
(progn
 (defalien "pango_font_metrics_get_approximate_digit_width" int (a (* t)))
 (defun pango::font-metrics-get-approximate-digit-width (a)
   (|pango_font_metrics_get_approximate_digit_width| a)))
(progn
 (defalien "pango_font_family_get_type" GType)
 (defun pango::font-family-get-type () (|pango_font_family_get_type|)))
(progn
 (defalien "pango_font_family_list_faces"
           void
           (a (* t))
           (b (* t))
           (c int :in-out))
 (defun pango::font-family-list-faces (a b c)
   (|pango_font_family_list_faces| a b c)))
(progn
 (defalien "pango_font_family_get_name" c-string (a (* t)))
 (defun pango::font-family-get-name (a) (|pango_font_family_get_name| a)))
(progn
 (defalien "pango_font_face_get_type" GType)
 (defun pango::font-face-get-type () (|pango_font_face_get_type|)))
(progn
 (defalien "pango_font_face_describe" (* t) (a (* t)))
 (defun pango::font-face-describe (a) (|pango_font_face_describe| a)))
(progn
 (defalien "pango_font_face_get_face_name" c-string (a (* t)))
 (defun pango::font-face-get-face-name (a) (|pango_font_face_get_face_name| a)))
(progn
 (defalien "pango_font_get_type" GType)
 (defun pango::font-get-type () (|pango_font_get_type|)))
(progn
 (defalien "pango_font_describe" (* t) (a (* t)))
 (defun pango::font-describe (a) (|pango_font_describe| a)))
(progn
 (defalien "pango_font_get_coverage" (* t) (a (* t)) (b (* t)))
 (defun pango::font-get-coverage (a b) (|pango_font_get_coverage| a b)))
(progn
 (defalien "pango_font_find_shaper" (* t) (a (* t)) (b (* t)) (c guint32))
 (defun pango::font-find-shaper (a b c) (|pango_font_find_shaper| a b c)))
(progn
 (defalien "pango_font_get_metrics" (* t) (a (* t)) (b (* t)))
 (defun pango::font-get-metrics (a b) (|pango_font_get_metrics| a b)))
(progn
 (defalien "pango_font_get_glyph_extents"
           void
           (a (* t))
           (b PangoGlyph)
           (c (* t))
           (d (* t)))
 (defun pango::font-get-glyph-extents (a b c d)
   (|pango_font_get_glyph_extents| a b c d)))
(progn
 (defalien "pango_color_get_type" GType)
 (defun pango::color-get-type () (|pango_color_get_type|)))
(progn
 (defalien "pango_color_copy" (* t) (a (* t)))
 (defun pango::color-copy (a) (|pango_color_copy| a)))
(progn
 (defalien "pango_color_free" void (a (* t)))
 (defun pango::color-free (a) (|pango_color_free| a)))
(progn
 (defalien "pango_color_parse" gboolean (a (* t)) (b c-string))
 (defun pango::color-parse (a b)
   (let ((v229 (|pango_color_parse| a b)))
     (if (= v229 1) t nil))))
(progn
 (defalien "pango_attr_type_register" PangoAttrType (a c-string))
 (defun pango::attr-type-register (a) (|pango_attr_type_register| a)))
(progn
 (defalien "pango_attribute_copy" (* t) (a (* t)))
 (defun pango::attribute-copy (a) (|pango_attribute_copy| a)))
(progn
 (defalien "pango_attribute_destroy" void (a (* t)))
 (defun pango::attribute-destroy (a) (|pango_attribute_destroy| a)))
(progn
 (defalien "pango_attribute_equal" gboolean (a (* t)) (b (* t)))
 (defun pango::attribute-equal (a b)
   (let ((v230 (|pango_attribute_equal| a b)))
     (if (= v230 1) t nil))))
(progn
 (defalien "pango_attr_language_new" (* t) (a (* t)))
 (defun pango::attr-language-new (a) (|pango_attr_language_new| a)))
(progn
 (defalien "pango_attr_family_new" (* t) (a c-string))
 (defun pango::attr-family-new (a) (|pango_attr_family_new| a)))
(progn
 (defalien "pango_attr_foreground_new"
           (* t)
           (a guint16)
           (b guint16)
           (c guint16))
 (defun pango::attr-foreground-new (a b c) (|pango_attr_foreground_new| a b c)))
(progn
 (defalien "pango_attr_background_new"
           (* t)
           (a guint16)
           (b guint16)
           (c guint16))
 (defun pango::attr-background-new (a b c) (|pango_attr_background_new| a b c)))
(progn
 (defalien "pango_attr_size_new" (* t) (a int))
 (defun pango::attr-size-new (a) (|pango_attr_size_new| a)))
(progn
 (defalien "pango_attr_style_new" (* t) (a PangoStyle))
 (defun pango::attr-style-new (a) (|pango_attr_style_new| a)))
(progn
 (defalien "pango_attr_weight_new" (* t) (a PangoWeight))
 (defun pango::attr-weight-new (a) (|pango_attr_weight_new| a)))
(progn
 (defalien "pango_attr_variant_new" (* t) (a PangoVariant))
 (defun pango::attr-variant-new (a) (|pango_attr_variant_new| a)))
(progn
 (defalien "pango_attr_stretch_new" (* t) (a PangoStretch))
 (defun pango::attr-stretch-new (a) (|pango_attr_stretch_new| a)))
(progn
 (defalien "pango_attr_font_desc_new" (* t) (a (* t)))
 (defun pango::attr-font-desc-new (a) (|pango_attr_font_desc_new| a)))
(progn
 (defalien "pango_attr_underline_new" (* t) (a PangoUnderline))
 (defun pango::attr-underline-new (a) (|pango_attr_underline_new| a)))
(progn
 (defalien "pango_attr_strikethrough_new" (* t) (a gboolean))
 (defun pango::attr-strikethrough-new (a)
   (|pango_attr_strikethrough_new| (if a (if (eq a 0) 0 1) 0))))
(progn
 (defalien "pango_attr_rise_new" (* t) (a int))
 (defun pango::attr-rise-new (a) (|pango_attr_rise_new| a)))
(progn
 (defalien "pango_attr_shape_new" (* t) (a (* t)) (b (* t)))
 (defun pango::attr-shape-new (a b) (|pango_attr_shape_new| a b)))
(progn
 (defalien "pango_attr_scale_new" (* t) (a double))
 (defun pango::attr-scale-new (a) (|pango_attr_scale_new| a)))
(progn
 (defalien "pango_attr_list_get_type" GType)
 (defun pango::attr-list-get-type () (|pango_attr_list_get_type|)))
(progn
 (defalien "pango_attr_list_new" (* t))
 (defun pango::attr-list-new () (|pango_attr_list_new|)))
(progn
 (defalien "pango_attr_list_ref" void (a (* t)))
 (defun pango::attr-list-ref (a) (|pango_attr_list_ref| a)))
(progn
 (defalien "pango_attr_list_unref" void (a (* t)))
 (defun pango::attr-list-unref (a) (|pango_attr_list_unref| a)))
(progn
 (defalien "pango_attr_list_copy" (* t) (a (* t)))
 (defun pango::attr-list-copy (a) (|pango_attr_list_copy| a)))
(progn
 (defalien "pango_attr_list_insert" void (a (* t)) (b (* t)))
 (defun pango::attr-list-insert (a b) (|pango_attr_list_insert| a b)))
(progn
 (defalien "pango_attr_list_insert_before" void (a (* t)) (b (* t)))
 (defun pango::attr-list-insert-before (a b)
   (|pango_attr_list_insert_before| a b)))
(progn
 (defalien "pango_attr_list_change" void (a (* t)) (b (* t)))
 (defun pango::attr-list-change (a b) (|pango_attr_list_change| a b)))
(progn
 (defalien "pango_attr_list_splice" void (a (* t)) (b (* t)) (c gint) (d gint))
 (defun pango::attr-list-splice (a b c d) (|pango_attr_list_splice| a b c d)))
(progn
 (defalien "pango_attr_list_get_iterator" (* t) (a (* t)))
 (defun pango::attr-list-get-iterator (a) (|pango_attr_list_get_iterator| a)))
(progn
 (defalien "pango_attr_iterator_range"
           void
           (a (* t))
           (b gint :in-out)
           (c gint :in-out))
 (defun pango::attr-iterator-range (a b c) (|pango_attr_iterator_range| a b c)))
(progn
 (defalien "pango_attr_iterator_next" gboolean (a (* t)))
 (defun pango::attr-iterator-next (a)
   (let ((v231 (|pango_attr_iterator_next| a)))
     (if (= v231 1) t nil))))
(progn
 (defalien "pango_attr_iterator_copy" (* t) (a (* t)))
 (defun pango::attr-iterator-copy (a) (|pango_attr_iterator_copy| a)))
(progn
 (defalien "pango_attr_iterator_destroy" void (a (* t)))
 (defun pango::attr-iterator-destroy (a) (|pango_attr_iterator_destroy| a)))
(progn
 (defalien "pango_attr_iterator_get" (* t) (a (* t)) (b PangoAttrType))
 (defun pango::attr-iterator-get (a b) (|pango_attr_iterator_get| a b)))
(progn
 (defalien "pango_attr_iterator_get_font"
           void
           (a (* t))
           (b (* t))
           (c (* t))
           (d (* t)))
 (defun pango::attr-iterator-get-font (a b c d)
   (|pango_attr_iterator_get_font| a b c d)))
(progn
 (defalien "pango_parse_markup"
           gboolean
           (a c-string)
           (b int)
           (c gunichar)
           (d (* t))
           (e (* t))
           (f (* t))
           (g (* t)))
 (defun pango::parse-markup (a b c d e f g)
   (let ((v232 (|pango_parse_markup| a b c d e f g)))
     (if (= v232 1) t nil))))
(progn
 (defalien "pango_break" void (a c-string) (b int) (c (* t)) (d (* t)) (e int))
 (defun pango::break (a b c d e) (|pango_break| a b c d e)))
(progn
 (defalien "pango_find_paragraph_boundary"
           void
           (a c-string)
           (b gint)
           (c gint :in-out)
           (d gint :in-out))
 (defun pango::find-paragraph-boundary (a b c d)
   (|pango_find_paragraph_boundary| a b c d)))
(progn
 (defalien "pango_get_log_attrs"
           void
           (a c-string)
           (b int)
           (c int)
           (d (* t))
           (e (* t))
           (f int))
 (defun pango::get-log-attrs (a b c d e f) (|pango_get_log_attrs| a b c d e f)))
(progn
 (defalien "pango_fontset_get_font" (* t) (a (* t)) (b guint))
 (defun pango::fontset-get-font (a b) (|pango_fontset_get_font| a b)))
(progn
 (defalien "pango_fontset_get_metrics" (* t) (a (* t)))
 (defun pango::fontset-get-metrics (a) (|pango_fontset_get_metrics| a)))
(progn
 (defalien "pango_font_map_load_font" (* t) (a (* t)) (b (* t)) (c (* t)))
 (defun pango::font-map-load-font (a b c) (|pango_font_map_load_font| a b c)))
(progn
 (defalien "pango_font_map_load_fontset"
           (* t)
           (a (* t))
           (b (* t))
           (c (* t))
           (d (* t)))
 (defun pango::font-map-load-fontset (a b c d)
   (|pango_font_map_load_fontset| a b c d)))
(progn
 (defalien "pango_font_map_list_families"
           void
           (a (* t))
           (b (* t))
           (c int :in-out))
 (defun pango::font-map-list-families (a b c)
   (|pango_font_map_list_families| a b c)))
(progn
 (defalien "pango_context_list_families"
           void
           (a (* t))
           (b (* t))
           (c int :in-out))
 (defun pango::context-list-families (a b c)
   (|pango_context_list_families| a b c)))
(progn
 (defalien "pango_context_load_font" (* t) (a (* t)) (b (* t)))
 (defun pango::context-load-font (a b) (|pango_context_load_font| a b)))
(progn
 (defalien "pango_context_load_fontset" (* t) (a (* t)) (b (* t)) (c (* t)))
 (defun pango::context-load-fontset (a b c)
   (|pango_context_load_fontset| a b c)))
(progn
 (defalien "pango_context_get_metrics" (* t) (a (* t)) (b (* t)) (c (* t)))
 (defun pango::context-get-metrics (a b c) (|pango_context_get_metrics| a b c)))
(progn
 (defalien "pango_context_set_font_description" void (a (* t)) (b (* t)))
 (defun pango::context-set-font-description (a b)
   (|pango_context_set_font_description| a b)))
(progn
 (defalien "pango_context_get_font_description" (* t) (a (* t)))
 (defun pango::context-get-font-description (a)
   (|pango_context_get_font_description| a)))
(progn
 (defalien "pango_context_get_language" (* t) (a (* t)))
 (defun pango::context-get-language (a) (|pango_context_get_language| a)))
(progn
 (defalien "pango_context_set_language" void (a (* t)) (b (* t)))
 (defun pango::context-set-language (a b) (|pango_context_set_language| a b)))
(progn
 (defalien "pango_context_set_base_dir" void (a (* t)) (b PangoDirection))
 (defun pango::context-set-base-dir (a b) (|pango_context_set_base_dir| a b)))
(progn
 (defalien "pango_context_get_base_dir" PangoDirection (a (* t)))
 (defun pango::context-get-base-dir (a) (|pango_context_get_base_dir| a)))
(progn
 (defalien "pango_itemize"
           (* t)
           (a (* t))
           (b c-string)
           (c int)
           (d int)
           (e (* t))
           (f (* t)))
 (defun pango::itemize (a b c d e f) (|pango_itemize| a b c d e f)))
(progn
 (defalien "pango_glyph_string_new" (* t))
 (defun pango::glyph-string-new () (|pango_glyph_string_new|)))
(progn
 (defalien "pango_glyph_string_set_size" void (a (* t)) (b gint))
 (defun pango::glyph-string-set-size (a b) (|pango_glyph_string_set_size| a b)))
(progn
 (defalien "pango_glyph_string_get_type" GType)
 (defun pango::glyph-string-get-type () (|pango_glyph_string_get_type|)))
(progn
 (defalien "pango_glyph_string_copy" (* t) (a (* t)))
 (defun pango::glyph-string-copy (a) (|pango_glyph_string_copy| a)))
(progn
 (defalien "pango_glyph_string_free" void (a (* t)))
 (defun pango::glyph-string-free (a) (|pango_glyph_string_free| a)))
(progn
 (defalien "pango_glyph_string_extents"
           void
           (a (* t))
           (b (* t))
           (c (* t))
           (d (* t)))
 (defun pango::glyph-string-extents (a b c d)
   (|pango_glyph_string_extents| a b c d)))
(progn
 (defalien "pango_glyph_string_extents_range"
           void
           (a (* t))
           (b int)
           (c int)
           (d (* t))
           (e (* t))
           (f (* t)))
 (defun pango::glyph-string-extents-range (a b c d e f)
   (|pango_glyph_string_extents_range| a b c d e f)))
(progn
 (defalien "pango_glyph_string_get_logical_widths"
           void
           (a (* t))
           (b c-string)
           (c int)
           (d int)
           (e int :in-out))
 (defun pango::glyph-string-get-logical-widths (a b c d e)
   (|pango_glyph_string_get_logical_widths| a b c d e)))
(progn
 (defalien "pango_glyph_string_index_to_x"
           void
           (a (* t))
           (b c-string)
           (c int)
           (d (* t))
           (e int)
           (f gboolean)
           (g int :in-out))
 (defun pango::glyph-string-index-to-x (a b c d e f g)
   (|pango_glyph_string_index_to_x| a b c d e (if f (if (eq f 0) 0 1) 0) g)))
(progn
 (defalien "pango_glyph_string_x_to_index"
           void
           (a (* t))
           (b c-string)
           (c int)
           (d (* t))
           (e int)
           (f int :in-out)
           (g int :in-out))
 (defun pango::glyph-string-x-to-index (a b c d e f g)
   (|pango_glyph_string_x_to_index| a b c d e f g)))
(progn
 (defalien "pango_shape" void (a c-string) (b gint) (c (* t)) (d (* t)))
 (defun pango::shape (a b c d) (|pango_shape| a b c d)))
(progn
 (defalien "pango_reorder_items" (* t) (a (* t)))
 (defun pango::reorder-items (a) (|pango_reorder_items| a)))
(progn
 (defalien "pango_tab_array_new" (* t) (a gint) (b gboolean))
 (defun pango::tab-array-new (a b)
   (|pango_tab_array_new| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "pango_tab_array_new_with_positions"
           (* t)
           (a gint)
           (b gboolean)
           (c PangoTabAlign)
           (d gint))
 (defun pango::tab-array-new-with-positions (a b c d)
   (|pango_tab_array_new_with_positions| a (if b (if (eq b 0) 0 1) 0) c d)))
(progn
 (defalien "pango_tab_array_get_type" GType)
 (defun pango::tab-array-get-type () (|pango_tab_array_get_type|)))
(progn
 (defalien "pango_tab_array_copy" (* t) (a (* t)))
 (defun pango::tab-array-copy (a) (|pango_tab_array_copy| a)))
(progn
 (defalien "pango_tab_array_free" void (a (* t)))
 (defun pango::tab-array-free (a) (|pango_tab_array_free| a)))
(progn
 (defalien "pango_tab_array_get_size" gint (a (* t)))
 (defun pango::tab-array-get-size (a) (|pango_tab_array_get_size| a)))
(progn
 (defalien "pango_tab_array_resize" void (a (* t)) (b gint))
 (defun pango::tab-array-resize (a b) (|pango_tab_array_resize| a b)))
(progn
 (defalien "pango_tab_array_set_tab"
           void
           (a (* t))
           (b gint)
           (c PangoTabAlign)
           (d gint))
 (defun pango::tab-array-set-tab (a b c d) (|pango_tab_array_set_tab| a b c d)))
(progn
 (defalien "pango_tab_array_get_tab"
           void
           (a (* t))
           (b gint)
           (c PangoTabAlign :in-out)
           (d gint :in-out))
 (defun pango::tab-array-get-tab (a b c d) (|pango_tab_array_get_tab| a b c d)))
(progn
 (defalien "pango_tab_array_get_tabs" void (a (* t)) (b (* t)) (c (* t)))
 (defun pango::tab-array-get-tabs (a b c) (|pango_tab_array_get_tabs| a b c)))
(progn
 (defalien "pango_tab_array_get_positions_in_pixels" gboolean (a (* t)))
 (defun pango::tab-array-get-positions-in-pixels (a)
   (let ((v233 (|pango_tab_array_get_positions_in_pixels| a)))
     (if (= v233 1) t nil))))
(progn
 (defalien "pango_layout_get_type" GType)
 (defun pango::layout-get-type () (|pango_layout_get_type|)))
(progn
 (defalien "pango_layout_new" (* t) (a (* t)))
 (defun pango::layout-new (a) (|pango_layout_new| a)))
(progn
 (defalien "pango_layout_copy" (* t) (a (* t)))
 (defun pango::layout-copy (a) (|pango_layout_copy| a)))
(progn
 (defalien "pango_layout_get_context" (* t) (a (* t)))
 (defun pango::layout-get-context (a) (|pango_layout_get_context| a)))
(progn
 (defalien "pango_layout_set_attributes" void (a (* t)) (b (* t)))
 (defun pango::layout-set-attributes (a b) (|pango_layout_set_attributes| a b)))
(progn
 (defalien "pango_layout_get_attributes" (* t) (a (* t)))
 (defun pango::layout-get-attributes (a) (|pango_layout_get_attributes| a)))
(progn
 (defalien "pango_layout_set_text" void (a (* t)) (b c-string) (c int))
 (defun pango::layout-set-text (a b c) (|pango_layout_set_text| a b c)))
(progn
 (defalien "pango_layout_get_text" c-string (a (* t)))
 (defun pango::layout-get-text (a) (|pango_layout_get_text| a)))
(progn
 (defalien "pango_layout_set_markup" void (a (* t)) (b c-string) (c int))
 (defun pango::layout-set-markup (a b c) (|pango_layout_set_markup| a b c)))
(progn
 (defalien "pango_layout_set_markup_with_accel"
           void
           (a (* t))
           (b c-string)
           (c int)
           (d gunichar)
           (e (* t)))
 (defun pango::layout-set-markup-with-accel (a b c d e)
   (|pango_layout_set_markup_with_accel| a b c d e)))
(progn
 (defalien "pango_layout_set_font_description" void (a (* t)) (b (* t)))
 (defun pango::layout-set-font-description (a b)
   (|pango_layout_set_font_description| a b)))
(progn
 (defalien "pango_layout_set_width" void (a (* t)) (b int))
 (defun pango::layout-set-width (a b) (|pango_layout_set_width| a b)))
(progn
 (defalien "pango_layout_get_width" int (a (* t)))
 (defun pango::layout-get-width (a) (|pango_layout_get_width| a)))
(progn
 (defalien "pango_layout_set_wrap" void (a (* t)) (b PangoWrapMode))
 (defun pango::layout-set-wrap (a b) (|pango_layout_set_wrap| a b)))
(progn
 (defalien "pango_layout_get_wrap" PangoWrapMode (a (* t)))
 (defun pango::layout-get-wrap (a) (|pango_layout_get_wrap| a)))
(progn
 (defalien "pango_layout_set_indent" void (a (* t)) (b int))
 (defun pango::layout-set-indent (a b) (|pango_layout_set_indent| a b)))
(progn
 (defalien "pango_layout_get_indent" int (a (* t)))
 (defun pango::layout-get-indent (a) (|pango_layout_get_indent| a)))
(progn
 (defalien "pango_layout_set_spacing" void (a (* t)) (b int))
 (defun pango::layout-set-spacing (a b) (|pango_layout_set_spacing| a b)))
(progn
 (defalien "pango_layout_get_spacing" int (a (* t)))
 (defun pango::layout-get-spacing (a) (|pango_layout_get_spacing| a)))
(progn
 (defalien "pango_layout_set_justify" void (a (* t)) (b gboolean))
 (defun pango::layout-set-justify (a b)
   (|pango_layout_set_justify| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "pango_layout_get_justify" gboolean (a (* t)))
 (defun pango::layout-get-justify (a)
   (let ((v234 (|pango_layout_get_justify| a)))
     (if (= v234 1) t nil))))
(progn
 (defalien "pango_layout_set_alignment" void (a (* t)) (b PangoAlignment))
 (defun pango::layout-set-alignment (a b) (|pango_layout_set_alignment| a b)))
(progn
 (defalien "pango_layout_get_alignment" PangoAlignment (a (* t)))
 (defun pango::layout-get-alignment (a) (|pango_layout_get_alignment| a)))
(progn
 (defalien "pango_layout_set_tabs" void (a (* t)) (b (* t)))
 (defun pango::layout-set-tabs (a b) (|pango_layout_set_tabs| a b)))
(progn
 (defalien "pango_layout_get_tabs" (* t) (a (* t)))
 (defun pango::layout-get-tabs (a) (|pango_layout_get_tabs| a)))
(progn
 (defalien "pango_layout_set_single_paragraph_mode"
           void
           (a (* t))
           (b gboolean))
 (defun pango::layout-set-single-paragraph-mode (a b)
   (|pango_layout_set_single_paragraph_mode| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "pango_layout_get_single_paragraph_mode" gboolean (a (* t)))
 (defun pango::layout-get-single-paragraph-mode (a)
   (let ((v235 (|pango_layout_get_single_paragraph_mode| a)))
     (if (= v235 1) t nil))))
(progn
 (defalien "pango_layout_context_changed" void (a (* t)))
 (defun pango::layout-context-changed (a) (|pango_layout_context_changed| a)))
(progn
 (defalien "pango_layout_get_log_attrs"
           void
           (a (* t))
           (b (* t))
           (c gint :in-out))
 (defun pango::layout-get-log-attrs (a b c)
   (|pango_layout_get_log_attrs| a b c)))
(progn
 (defalien "pango_layout_index_to_pos" void (a (* t)) (b int) (c (* t)))
 (defun pango::layout-index-to-pos (a b c) (|pango_layout_index_to_pos| a b c)))
(progn
 (defalien "pango_layout_get_cursor_pos"
           void
           (a (* t))
           (b int)
           (c (* t))
           (d (* t)))
 (defun pango::layout-get-cursor-pos (a b c d)
   (|pango_layout_get_cursor_pos| a b c d)))
(progn
 (defalien "pango_layout_move_cursor_visually"
           void
           (a (* t))
           (b gboolean)
           (c int)
           (d int)
           (e int)
           (f int :in-out)
           (g int :in-out))
 (defun pango::layout-move-cursor-visually (a b c d e f g)
   (|pango_layout_move_cursor_visually| a (if b (if (eq b 0) 0 1) 0) c d e f
    g)))
(progn
 (defalien "pango_layout_xy_to_index"
           gboolean
           (a (* t))
           (b int)
           (c int)
           (d int :in-out)
           (e int :in-out))
 (defun pango::layout-xy-to-index (a b c d e)
   (let ((v236 (multiple-value-list (|pango_layout_xy_to_index| a b c d e))))
     (apply #'values (if (= 1 (car v236)) t nil) (cdr v236)))))
(progn
 (defalien "pango_layout_get_extents" void (a (* t)) (b (* t)) (c (* t)))
 (defun pango::layout-get-extents (a b c) (|pango_layout_get_extents| a b c)))
(progn
 (defalien "pango_layout_get_pixel_extents" void (a (* t)) (b (* t)) (c (* t)))
 (defun pango::layout-get-pixel-extents (a b c)
   (|pango_layout_get_pixel_extents| a b c)))
(progn
 (defalien "pango_layout_get_size"
           void
           (a (* t))
           (b int :in-out)
           (c int :in-out))
 (defun pango::layout-get-size (a b c) (|pango_layout_get_size| a b c)))
(progn
 (defalien "pango_layout_get_pixel_size"
           void
           (a (* t))
           (b int :in-out)
           (c int :in-out))
 (defun pango::layout-get-pixel-size (a b c)
   (|pango_layout_get_pixel_size| a b c)))
(progn
 (defalien "pango_layout_get_line_count" int (a (* t)))
 (defun pango::layout-get-line-count (a) (|pango_layout_get_line_count| a)))
(progn
 (defalien "pango_layout_get_line" (* t) (a (* t)) (b int))
 (defun pango::layout-get-line (a b) (|pango_layout_get_line| a b)))
(progn
 (defalien "pango_layout_get_lines" (* t) (a (* t)))
 (defun pango::layout-get-lines (a) (|pango_layout_get_lines| a)))
(progn
 (defalien "pango_layout_line_ref" void (a (* t)))
 (defun pango::layout-line-ref (a) (|pango_layout_line_ref| a)))
(progn
 (defalien "pango_layout_line_unref" void (a (* t)))
 (defun pango::layout-line-unref (a) (|pango_layout_line_unref| a)))
(progn
 (defalien "pango_layout_line_x_to_index"
           gboolean
           (a (* t))
           (b int)
           (c int :in-out)
           (d int :in-out))
 (defun pango::layout-line-x-to-index (a b c d)
   (let ((v237 (multiple-value-list (|pango_layout_line_x_to_index| a b c d))))
     (apply #'values (if (= 1 (car v237)) t nil) (cdr v237)))))
(progn
 (defalien "pango_layout_line_index_to_x"
           void
           (a (* t))
           (b int)
           (c gboolean)
           (d int :in-out))
 (defun pango::layout-line-index-to-x (a b c d)
   (|pango_layout_line_index_to_x| a b (if c (if (eq c 0) 0 1) 0) d)))
(progn
 (defalien "pango_layout_line_get_x_ranges"
           void
           (a (* t))
           (b int)
           (c int)
           (d (* t))
           (e int :in-out))
 (defun pango::layout-line-get-x-ranges (a b c d e)
   (|pango_layout_line_get_x_ranges| a b c d e)))
(progn
 (defalien "pango_layout_line_get_extents" void (a (* t)) (b (* t)) (c (* t)))
 (defun pango::layout-line-get-extents (a b c)
   (|pango_layout_line_get_extents| a b c)))
(progn
 (defalien "pango_layout_line_get_pixel_extents"
           void
           (a (* t))
           (b (* t))
           (c (* t)))
 (defun pango::layout-line-get-pixel-extents (a b c)
   (|pango_layout_line_get_pixel_extents| a b c)))
(progn
 (defalien "pango_layout_get_iter" (* t) (a (* t)))
 (defun pango::layout-get-iter (a) (|pango_layout_get_iter| a)))
(progn
 (defalien "pango_layout_iter_free" void (a (* t)))
 (defun pango::layout-iter-free (a) (|pango_layout_iter_free| a)))
(progn
 (defalien "pango_layout_iter_get_index" int (a (* t)))
 (defun pango::layout-iter-get-index (a) (|pango_layout_iter_get_index| a)))
(progn
 (defalien "pango_layout_iter_get_run" (* t) (a (* t)))
 (defun pango::layout-iter-get-run (a) (|pango_layout_iter_get_run| a)))
(progn
 (defalien "pango_layout_iter_get_line" (* t) (a (* t)))
 (defun pango::layout-iter-get-line (a) (|pango_layout_iter_get_line| a)))
(progn
 (defalien "pango_layout_iter_at_last_line" gboolean (a (* t)))
 (defun pango::layout-iter-at-last-line (a)
   (let ((v238 (|pango_layout_iter_at_last_line| a)))
     (if (= v238 1) t nil))))
(progn
 (defalien "pango_layout_iter_next_char" gboolean (a (* t)))
 (defun pango::layout-iter-next-char (a)
   (let ((v239 (|pango_layout_iter_next_char| a)))
     (if (= v239 1) t nil))))
(progn
 (defalien "pango_layout_iter_next_cluster" gboolean (a (* t)))
 (defun pango::layout-iter-next-cluster (a)
   (let ((v240 (|pango_layout_iter_next_cluster| a)))
     (if (= v240 1) t nil))))
(progn
 (defalien "pango_layout_iter_next_run" gboolean (a (* t)))
 (defun pango::layout-iter-next-run (a)
   (let ((v241 (|pango_layout_iter_next_run| a)))
     (if (= v241 1) t nil))))
(progn
 (defalien "pango_layout_iter_next_line" gboolean (a (* t)))
 (defun pango::layout-iter-next-line (a)
   (let ((v242 (|pango_layout_iter_next_line| a)))
     (if (= v242 1) t nil))))
(progn
 (defalien "pango_layout_iter_get_char_extents" void (a (* t)) (b (* t)))
 (defun pango::layout-iter-get-char-extents (a b)
   (|pango_layout_iter_get_char_extents| a b)))
(progn
 (defalien "pango_layout_iter_get_cluster_extents"
           void
           (a (* t))
           (b (* t))
           (c (* t)))
 (defun pango::layout-iter-get-cluster-extents (a b c)
   (|pango_layout_iter_get_cluster_extents| a b c)))
(progn
 (defalien "pango_layout_iter_get_run_extents"
           void
           (a (* t))
           (b (* t))
           (c (* t)))
 (defun pango::layout-iter-get-run-extents (a b c)
   (|pango_layout_iter_get_run_extents| a b c)))
(progn
 (defalien "pango_layout_iter_get_line_extents"
           void
           (a (* t))
           (b (* t))
           (c (* t)))
 (defun pango::layout-iter-get-line-extents (a b c)
   (|pango_layout_iter_get_line_extents| a b c)))
(progn
 (defalien "pango_layout_iter_get_line_yrange"
           void
           (a (* t))
           (b int :in-out)
           (c int :in-out))
 (defun pango::layout-iter-get-line-yrange (a b c)
   (|pango_layout_iter_get_line_yrange| a b c)))
(progn
 (defalien "pango_layout_iter_get_layout_extents"
           void
           (a (* t))
           (b (* t))
           (c (* t)))
 (defun pango::layout-iter-get-layout-extents (a b c)
   (|pango_layout_iter_get_layout_extents| a b c)))
(progn
 (defalien "pango_layout_iter_get_baseline" int (a (* t)))
 (defun pango::layout-iter-get-baseline (a)
   (|pango_layout_iter_get_baseline| a)))
(progn
 (defalien "gdk_colormap_get_type" GType)
 (defun gdk::colormap-get-type () (|gdk_colormap_get_type|)))
(progn
 (defalien "gdk_colormap_new" (* t) (a (* t)) (b gboolean))
 (defun gdk::colormap-new (a b)
   (|gdk_colormap_new| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gdk_colormap_get_system" (* t))
 (defun gdk::colormap-get-system () (|gdk_colormap_get_system|)))
(progn
 (defalien "gdk_colormap_get_system_size" gint)
 (defun gdk::colormap-get-system-size () (|gdk_colormap_get_system_size|)))
(progn
 (defalien "gdk_colormap_alloc_color"
           gboolean
           (a (* t))
           (b (* t))
           (c gboolean)
           (d gboolean))
 (defun gdk::colormap-alloc-color (a b c d)
   (let ((v243
          (|gdk_colormap_alloc_color| a b (if c (if (eq c 0) 0 1) 0)
           (if d (if (eq d 0) 0 1) 0))))
     (if (= v243 1) t nil))))
(progn
 (defalien "gdk_colormap_get_visual" (* t) (a (* t)))
 (defun gdk::colormap-get-visual (a) (|gdk_colormap_get_visual| a)))
(progn
 (defalien "gdk_color_copy" (* t) (a (* t)))
 (defun gdk::color-copy (a) (|gdk_color_copy| a)))
(progn
 (defalien "gdk_color_free" void (a (* t)))
 (defun gdk::color-free (a) (|gdk_color_free| a)))
(progn
 (defalien "gdk_color_parse" gint (a c-string) (b (* t)))
 (defun gdk::color-parse (a b) (|gdk_color_parse| a b)))
(progn
 (defalien "gdk_color_white" gint (a (* t)) (b (* t)))
 (defun gdk::color-white (a b) (|gdk_color_white| a b)))
(progn
 (defalien "gdk_color_black" gint (a (* t)) (b (* t)))
 (defun gdk::color-black (a b) (|gdk_color_black| a b)))
(progn
 (defalien "gdk_color_alloc" gint (a (* t)) (b (* t)))
 (defun gdk::color-alloc (a b) (|gdk_color_alloc| a b)))
(progn
 (defalien "gdk_color_change" gint (a (* t)) (b (* t)))
 (defun gdk::color-change (a b) (|gdk_color_change| a b)))
(progn
 (defalien "gdk_cursor_new" (* t) (a GdkCursorType))
 (defun gdk::cursor-new (a) (|gdk_cursor_new| a)))
(progn
 (defalien "gdk_cursor_new_from_pixmap"
           (* t)
           (a (* t))
           (b (* t))
           (c (* t))
           (d (* t))
           (e gint)
           (f gint))
 (defun gdk::cursor-new-from-pixmap (a b c d e f)
   (|gdk_cursor_new_from_pixmap| a b c d e f)))
(progn
 (defalien "gdk_cursor_ref" (* t) (a (* t)))
 (defun gdk::cursor-ref (a) (|gdk_cursor_ref| a)))
(progn
 (defalien "gdk_cursor_unref" void (a (* t)))
 (defun gdk::cursor-unref (a) (|gdk_cursor_unref| a)))
(progn
 (defalien "gdk_drag_context_get_type" GType)
 (defun gdk::drag-context-get-type () (|gdk_drag_context_get_type|)))
(progn
 (defalien "gdk_drag_context_new" (* t))
 (defun gdk::drag-context-new () (|gdk_drag_context_new|)))
(progn
 (defalien "gdk_drag_context_ref" void (a (* t)))
 (defun gdk::drag-context-ref (a) (|gdk_drag_context_ref| a)))
(progn
 (defalien "gdk_drag_context_unref" void (a (* t)))
 (defun gdk::drag-context-unref (a) (|gdk_drag_context_unref| a)))
(progn
 (defalien "gdk_drag_status" void (a (* t)) (b GdkDragAction) (c guint32))
 (defun gdk::drag-status (a b c) (|gdk_drag_status| a b c)))
(progn
 (defalien "gdk_drop_reply" void (a (* t)) (b gboolean) (c guint32))
 (defun gdk::drop-reply (a b c)
   (|gdk_drop_reply| a (if b (if (eq b 0) 0 1) 0) c)))
(progn
 (defalien "gdk_drop_finish" void (a (* t)) (b gboolean) (c guint32))
 (defun gdk::drop-finish (a b c)
   (|gdk_drop_finish| a (if b (if (eq b 0) 0 1) 0) c)))
(progn
 (defalien "gdk_drag_get_selection" GdkAtom (a (* t)))
 (defun gdk::drag-get-selection (a) (|gdk_drag_get_selection| a)))
(progn
 (defalien "gdk_drag_begin" (* t) (a (* t)) (b (* t)))
 (defun gdk::drag-begin (a b) (|gdk_drag_begin| a b)))
(progn
 (defalien "gdk_drag_get_protocol"
           guint32
           (a guint32)
           (b GdkDragProtocol :in-out))
 (defun gdk::drag-get-protocol (a b) (|gdk_drag_get_protocol| a b)))
(progn
 (defalien "gdk_drag_find_window"
           void
           (a (* t))
           (b (* t))
           (c gint)
           (d gint)
           (e (* t))
           (f GdkDragProtocol :in-out))
 (defun gdk::drag-find-window (a b c d e f)
   (|gdk_drag_find_window| a b c d e f)))
(progn
 (defalien "gdk_drag_motion"
           gboolean
           (a (* t))
           (b (* t))
           (c GdkDragProtocol)
           (d gint)
           (e gint)
           (f GdkDragAction)
           (g GdkDragAction)
           (h guint32))
 (defun gdk::drag-motion (a b c d e f g h)
   (let ((v244 (|gdk_drag_motion| a b c d e f g h)))
     (if (= v244 1) t nil))))
(progn
 (defalien "gdk_drag_drop" void (a (* t)) (b guint32))
 (defun gdk::drag-drop (a b) (|gdk_drag_drop| a b)))
(progn
 (defalien "gdk_drag_abort" void (a (* t)) (b guint32))
 (defun gdk::drag-abort (a b) (|gdk_drag_abort| a b)))
(progn
 (defalien "gdk_device_get_type" GType)
 (defun gdk::device-get-type () (|gdk_device_get_type|)))
(progn
 (defalien "gdk_devices_list" (* t))
 (defun gdk::devices-list () (|gdk_devices_list|)))
(progn
 (defalien "gdk_device_set_source" void (a (* t)) (b GdkInputSource))
 (defun gdk::device-set-source (a b) (|gdk_device_set_source| a b)))
(progn
 (defalien "gdk_device_set_mode" gboolean (a (* t)) (b GdkInputMode))
 (defun gdk::device-set-mode (a b)
   (let ((v245 (|gdk_device_set_mode| a b)))
     (if (= v245 1) t nil))))
(progn
 (defalien "gdk_device_set_key"
           void
           (a (* t))
           (b guint)
           (c guint)
           (d GdkModifierType))
 (defun gdk::device-set-key (a b c d) (|gdk_device_set_key| a b c d)))
(progn
 (defalien "gdk_device_set_axis_use" void (a (* t)) (b guint) (c GdkAxisUse))
 (defun gdk::device-set-axis-use (a b c) (|gdk_device_set_axis_use| a b c)))
(progn
 (defalien "gdk_device_get_state"
           void
           (a (* t))
           (b (* t))
           (c gdouble :in-out)
           (d GdkModifierType :in-out))
 (defun gdk::device-get-state (a b c d)
   (|gdk_device_get_state| a b (coerce c 'double-float) d)))
(progn
 (defalien "gdk_device_get_history"
           gboolean
           (a (* t))
           (b (* t))
           (c guint32)
           (d guint32)
           (e (* t))
           (f gint :in-out))
 (defun gdk::device-get-history (a b c d e f)
   (let ((v246 (multiple-value-list (|gdk_device_get_history| a b c d e f))))
     (apply #'values (if (= 1 (car v246)) t nil) (cdr v246)))))
(progn
 (defalien "gdk_device_free_history" void (a (* t)) (b gint))
 (defun gdk::device-free-history (a b) (|gdk_device_free_history| a b)))
(progn
 (defalien "gdk_device_get_axis"
           gboolean
           (a (* t))
           (b gdouble :in-out)
           (c GdkAxisUse)
           (d gdouble :in-out))
 (defun gdk::device-get-axis (a b c d)
   (let ((v247
          (multiple-value-list
           (|gdk_device_get_axis| a (coerce b 'double-float) c
            (coerce d 'double-float)))))
     (apply #'values (if (= 1 (car v247)) t nil) (cdr v247)))))
(progn
 (defalien "gdk_input_set_extension_events"
           void
           (a (* t))
           (b gint)
           (c GdkExtensionMode))
 (defun gdk::input-set-extension-events (a b c)
   (|gdk_input_set_extension_events| a b c)))
(progn
 (defalien "gdk_device_get_core_pointer" (* t))
 (defun gdk::device-get-core-pointer () (|gdk_device_get_core_pointer|)))
(progn
 (defalien "gdk_event_get_type" GType)
 (defun gdk::event-get-type () (|gdk_event_get_type|)))
(progn
 (defalien "gdk_events_pending" gboolean)
 (defun gdk::events-pending ()
   (let ((v248 (|gdk_events_pending|)))
     (if (= v248 1) t nil))))
(progn
 (defalien "gdk_event_get" (* t))
 (defun gdk::event-get () (|gdk_event_get|)))
(progn
 (defalien "gdk_event_peek" (* t))
 (defun gdk::event-peek () (|gdk_event_peek|)))
(progn
 (defalien "gdk_event_get_graphics_expose" (* t) (a (* t)))
 (defun gdk::event-get-graphics-expose (a) (|gdk_event_get_graphics_expose| a)))
(progn
 (defalien "gdk_event_put" void (a (* t)))
 (defun gdk::event-put (a) (|gdk_event_put| a)))
(progn
 (defalien "gdk_event_copy" (* t) (a (* t)))
 (defun gdk::event-copy (a) (|gdk_event_copy| a)))
(progn
 (defalien "gdk_event_free" void (a (* t)))
 (defun gdk::event-free (a) (|gdk_event_free| a)))
(progn
 (defalien "gdk_event_get_time" guint32 (a (* t)))
 (defun gdk::event-get-time (a) (|gdk_event_get_time| a)))
(progn
 (defalien "gdk_event_get_state"
           gboolean
           (a (* t))
           (b GdkModifierType :in-out))
 (defun gdk::event-get-state (a b)
   (let ((v249 (multiple-value-list (|gdk_event_get_state| a b))))
     (apply #'values (if (= 1 (car v249)) t nil) (cdr v249)))))
(progn
 (defalien "gdk_event_get_coords"
           gboolean
           (a (* t))
           (b gdouble :in-out)
           (c gdouble :in-out))
 (defun gdk::event-get-coords (a b c)
   (let ((v250
          (multiple-value-list
           (|gdk_event_get_coords| a (coerce b 'double-float)
            (coerce c 'double-float)))))
     (apply #'values (if (= 1 (car v250)) t nil) (cdr v250)))))
(progn
 (defalien "gdk_event_get_root_coords"
           gboolean
           (a (* t))
           (b gdouble :in-out)
           (c gdouble :in-out))
 (defun gdk::event-get-root-coords (a b c)
   (let ((v251
          (multiple-value-list
           (|gdk_event_get_root_coords| a (coerce b 'double-float)
            (coerce c 'double-float)))))
     (apply #'values (if (= 1 (car v251)) t nil) (cdr v251)))))
(progn
 (defalien "gdk_event_get_axis"
           gboolean
           (a (* t))
           (b GdkAxisUse)
           (c gdouble :in-out))
 (defun gdk::event-get-axis (a b c)
   (let ((v252
          (multiple-value-list
           (|gdk_event_get_axis| a b (coerce c 'double-float)))))
     (apply #'values (if (= 1 (car v252)) t nil) (cdr v252)))))
(progn
 (defalien "gdk_event_handler_set"
           void
           (a GdkEventFunc)
           (b gpointer)
           (c GDestroyNotify))
 (defun gdk::event-handler-set (a b c) (|gdk_event_handler_set| a b c)))
(progn
 (defalien "gdk_set_show_events" void (a gboolean))
 (defun gdk::set-show-events (a)
   (|gdk_set_show_events| (if a (if (eq a 0) 0 1) 0))))
(progn
 (defalien "gdk_get_show_events" gboolean)
 (defun gdk::get-show-events ()
   (let ((v253 (|gdk_get_show_events|)))
     (if (= v253 1) t nil))))
(progn
 (defalien "gdk_add_client_message_filter"
           void
           (a GdkAtom)
           (b GdkFilterFunc)
           (c gpointer))
 (defun gdk::add-client-message-filter (a b c)
   (|gdk_add_client_message_filter| a b c)))
(progn
 (defalien "gdk_setting_get" gboolean (a c-string) (b (* t)))
 (defun gdk::setting-get (a b)
   (let ((v254 (|gdk_setting_get| a b)))
     (if (= v254 1) t nil))))
(progn
 (defalien "gdk_gc_get_type" GType)
 (defun gdk::gc-get-type () (|gdk_gc_get_type|)))
(progn
 (defalien "gdk_gc_new" (* t) (a (* t)))
 (defun gdk::gc-new (a) (|gdk_gc_new| a)))
(progn
 (defalien "gdk_gc_new_with_values"
           (* t)
           (a (* t))
           (b (* t))
           (c GdkGCValuesMask))
 (defun gdk::gc-new-with-values (a b c) (|gdk_gc_new_with_values| a b c)))
(progn
 (defalien "gdk_gc_get_values" void (a (* t)) (b (* t)))
 (defun gdk::gc-get-values (a b) (|gdk_gc_get_values| a b)))
(progn
 (defalien "gdk_gc_set_values" void (a (* t)) (b (* t)) (c GdkGCValuesMask))
 (defun gdk::gc-set-values (a b c) (|gdk_gc_set_values| a b c)))
(progn
 (defalien "gdk_gc_set_foreground" void (a (* t)) (b (* t)))
 (defun gdk::gc-set-foreground (a b) (|gdk_gc_set_foreground| a b)))
(progn
 (defalien "gdk_gc_set_background" void (a (* t)) (b (* t)))
 (defun gdk::gc-set-background (a b) (|gdk_gc_set_background| a b)))
(progn
 (defalien "gdk_gc_set_font" void (a (* t)) (b (* t)))
 (defun gdk::gc-set-font (a b) (|gdk_gc_set_font| a b)))
(progn
 (defalien "gdk_gc_set_function" void (a (* t)) (b GdkFunction))
 (defun gdk::gc-set-function (a b) (|gdk_gc_set_function| a b)))
(progn
 (defalien "gdk_gc_set_fill" void (a (* t)) (b GdkFill))
 (defun gdk::gc-set-fill (a b) (|gdk_gc_set_fill| a b)))
(progn
 (defalien "gdk_gc_set_tile" void (a (* t)) (b (* t)))
 (defun gdk::gc-set-tile (a b) (|gdk_gc_set_tile| a b)))
(progn
 (defalien "gdk_gc_set_stipple" void (a (* t)) (b (* t)))
 (defun gdk::gc-set-stipple (a b) (|gdk_gc_set_stipple| a b)))
(progn
 (defalien "gdk_gc_set_ts_origin" void (a (* t)) (b gint) (c gint))
 (defun gdk::gc-set-ts-origin (a b c) (|gdk_gc_set_ts_origin| a b c)))
(progn
 (defalien "gdk_gc_set_clip_origin" void (a (* t)) (b gint) (c gint))
 (defun gdk::gc-set-clip-origin (a b c) (|gdk_gc_set_clip_origin| a b c)))
(progn
 (defalien "gdk_gc_set_clip_mask" void (a (* t)) (b (* t)))
 (defun gdk::gc-set-clip-mask (a b) (|gdk_gc_set_clip_mask| a b)))
(progn
 (defalien "gdk_gc_set_clip_rectangle" void (a (* t)) (b (* t)))
 (defun gdk::gc-set-clip-rectangle (a b) (|gdk_gc_set_clip_rectangle| a b)))
(progn
 (defalien "gdk_gc_set_clip_region" void (a (* t)) (b (* t)))
 (defun gdk::gc-set-clip-region (a b) (|gdk_gc_set_clip_region| a b)))
(progn
 (defalien "gdk_gc_set_subwindow" void (a (* t)) (b GdkSubwindowMode))
 (defun gdk::gc-set-subwindow (a b) (|gdk_gc_set_subwindow| a b)))
(progn
 (defalien "gdk_gc_set_exposures" void (a (* t)) (b gboolean))
 (defun gdk::gc-set-exposures (a b)
   (|gdk_gc_set_exposures| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gdk_gc_set_line_attributes"
           void
           (a (* t))
           (b gint)
           (c GdkLineStyle)
           (d GdkCapStyle)
           (e GdkJoinStyle))
 (defun gdk::gc-set-line-attributes (a b c d e)
   (|gdk_gc_set_line_attributes| a b c d e)))
(progn
 (defalien "gdk_gc_set_dashes"
           void
           (a (* t))
           (b gint)
           (c gint8 :in-out)
           (d gint))
 (defun gdk::gc-set-dashes (a b c d) (|gdk_gc_set_dashes| a b c d)))
(progn
 (defalien "gdk_gc_offset" void (a (* t)) (b gint) (c gint))
 (defun gdk::gc-offset (a b c) (|gdk_gc_offset| a b c)))
(progn
 (defalien "gdk_gc_copy" void (a (* t)) (b (* t)))
 (defun gdk::gc-copy (a b) (|gdk_gc_copy| a b)))
(progn
 (defalien "gdk_gc_set_colormap" void (a (* t)) (b (* t)))
 (defun gdk::gc-set-colormap (a b) (|gdk_gc_set_colormap| a b)))
(progn
 (defalien "gdk_gc_get_colormap" (* t) (a (* t)))
 (defun gdk::gc-get-colormap (a) (|gdk_gc_get_colormap| a)))
(progn
 (defalien "gdk_gc_set_rgb_fg_color" void (a (* t)) (b (* t)))
 (defun gdk::gc-set-rgb-fg-color (a b) (|gdk_gc_set_rgb_fg_color| a b)))
(progn
 (defalien "gdk_gc_set_rgb_bg_color" void (a (* t)) (b (* t)))
 (defun gdk::gc-set-rgb-bg-color (a b) (|gdk_gc_set_rgb_bg_color| a b)))
(progn
 (defalien "gdk_rgb_xpixel_from_rgb" gulong (a guint32))
 (defun gdk::rgb-xpixel-from-rgb (a) (|gdk_rgb_xpixel_from_rgb| a)))
(progn
 (defalien "gdk_rgb_gc_set_foreground" void (a (* t)) (b guint32))
 (defun gdk::rgb-gc-set-foreground (a b) (|gdk_rgb_gc_set_foreground| a b)))
(progn
 (defalien "gdk_rgb_gc_set_background" void (a (* t)) (b guint32))
 (defun gdk::rgb-gc-set-background (a b) (|gdk_rgb_gc_set_background| a b)))
(progn
 (defalien "gdk_draw_rgb_image"
           void
           (a (* t))
           (b (* t))
           (c gint)
           (d gint)
           (e gint)
           (f gint)
           (g GdkRgbDither)
           (h guchar :in-out)
           (i gint))
 (defun gdk::draw-rgb-image (a b c d e f g h i)
   (|gdk_draw_rgb_image| a b c d e f g h i)))
(progn
 (defalien "gdk_draw_rgb_image_dithalign"
           void
           (a (* t))
           (b (* t))
           (c gint)
           (d gint)
           (e gint)
           (f gint)
           (g GdkRgbDither)
           (h guchar :in-out)
           (i gint)
           (j gint)
           (k gint))
 (defun gdk::draw-rgb-image-dithalign (a b c d e f g h i j k)
   (|gdk_draw_rgb_image_dithalign| a b c d e f g h i j k)))
(progn
 (defalien "gdk_draw_rgb_32_image"
           void
           (a (* t))
           (b (* t))
           (c gint)
           (d gint)
           (e gint)
           (f gint)
           (g GdkRgbDither)
           (h guchar :in-out)
           (i gint))
 (defun gdk::draw-rgb-32-image (a b c d e f g h i)
   (|gdk_draw_rgb_32_image| a b c d e f g h i)))
(progn
 (defalien "gdk_draw_gray_image"
           void
           (a (* t))
           (b (* t))
           (c gint)
           (d gint)
           (e gint)
           (f gint)
           (g GdkRgbDither)
           (h guchar :in-out)
           (i gint))
 (defun gdk::draw-gray-image (a b c d e f g h i)
   (|gdk_draw_gray_image| a b c d e f g h i)))
(progn
 (defalien "gdk_draw_indexed_image"
           void
           (a (* t))
           (b (* t))
           (c gint)
           (d gint)
           (e gint)
           (f gint)
           (g GdkRgbDither)
           (h guchar :in-out)
           (i gint)
           (j (* t)))
 (defun gdk::draw-indexed-image (a b c d e f g h i j)
   (|gdk_draw_indexed_image| a b c d e f g h i j)))
(progn
 (defalien "gdk_rgb_cmap_new" (* t) (a guint32 :in-out) (b gint))
 (defun gdk::rgb-cmap-new (a b) (|gdk_rgb_cmap_new| a b)))
(progn
 (defalien "gdk_rgb_set_verbose" void (a gboolean))
 (defun gdk::rgb-set-verbose (a)
   (|gdk_rgb_set_verbose| (if a (if (eq a 0) 0 1) 0))))
(progn
 (defalien "gdk_rgb_set_install" void (a gboolean))
 (defun gdk::rgb-set-install (a)
   (|gdk_rgb_set_install| (if a (if (eq a 0) 0 1) 0))))
(progn
 (defalien "gdk_rgb_set_min_colors" void (a gint))
 (defun gdk::rgb-set-min-colors (a) (|gdk_rgb_set_min_colors| a)))
(progn
 (defalien "gdk_rgb_get_colormap" (* t))
 (defun gdk::rgb-get-colormap () (|gdk_rgb_get_colormap|)))
(progn
 (defalien "gdk_rgb_get_visual" (* t))
 (defun gdk::rgb-get-visual () (|gdk_rgb_get_visual|)))
(progn
 (defalien "gdk_rgb_ditherable" gboolean)
 (defun gdk::rgb-ditherable ()
   (let ((v255 (|gdk_rgb_ditherable|)))
     (if (= v255 1) t nil))))
(progn
 (defalien "gdk_pixbuf_get_colorspace" GdkColorspace (a (* t)))
 (defun gdk::pixbuf-get-colorspace (a) (|gdk_pixbuf_get_colorspace| a)))
(progn
 (defalien "gdk_pixbuf_get_n_channels" int (a (* t)))
 (defun gdk::pixbuf-get-n-channels (a) (|gdk_pixbuf_get_n_channels| a)))
(progn
 (defalien "gdk_pixbuf_get_has_alpha" gboolean (a (* t)))
 (defun gdk::pixbuf-get-has-alpha (a)
   (let ((v256 (|gdk_pixbuf_get_has_alpha| a)))
     (if (= v256 1) t nil))))
(progn
 (defalien "gdk_pixbuf_get_bits_per_sample" int (a (* t)))
 (defun gdk::pixbuf-get-bits-per-sample (a)
   (|gdk_pixbuf_get_bits_per_sample| a)))
(progn
 (defalien "gdk_pixbuf_get_pixels" guchar (a (* t)))
 (defun gdk::pixbuf-get-pixels (a) (|gdk_pixbuf_get_pixels| a)))
(progn
 (defalien "gdk_pixbuf_get_width" int (a (* t)))
 (defun gdk::pixbuf-get-width (a) (|gdk_pixbuf_get_width| a)))
(progn
 (defalien "gdk_pixbuf_get_height" int (a (* t)))
 (defun gdk::pixbuf-get-height (a) (|gdk_pixbuf_get_height| a)))
(progn
 (defalien "gdk_pixbuf_get_rowstride" int (a (* t)))
 (defun gdk::pixbuf-get-rowstride (a) (|gdk_pixbuf_get_rowstride| a)))
(progn
 (defalien "gdk_pixbuf_new"
           (* t)
           (a GdkColorspace)
           (b gboolean)
           (c int)
           (d int)
           (e int))
 (defun gdk::pixbuf-new (a b c d e)
   (|gdk_pixbuf_new| a (if b (if (eq b 0) 0 1) 0) c d e)))
(progn
 (defalien "gdk_pixbuf_copy" (* t) (a (* t)))
 (defun gdk::pixbuf-copy (a) (|gdk_pixbuf_copy| a)))
(progn
 (defalien "gdk_pixbuf_new_from_file" (* t) (a c-string) (b (* t)))
 (defun gdk::pixbuf-new-from-file (a b) (|gdk_pixbuf_new_from_file| a b)))
(progn
 (defalien "gdk_pixbuf_new_from_data"
           (* t)
           (a unsigned-char :in-out)
           (b GdkColorspace)
           (c gboolean)
           (d int)
           (e int)
           (f int)
           (g int)
           (h GdkPixbufDestroyNotify)
           (i gpointer))
 (defun gdk::pixbuf-new-from-data (a b c d e f g h i)
   (|gdk_pixbuf_new_from_data| a b (if c (if (eq c 0) 0 1) 0) d e f g h i)))
(progn
 (defalien "gdk_pixbuf_new_from_xpm_data" (* t) (a (* t)))
 (defun gdk::pixbuf-new-from-xpm-data (a) (|gdk_pixbuf_new_from_xpm_data| a)))
(progn
 (defalien "gdk_pixbuf_new_from_inline"
           (* t)
           (a gint)
           (b unsigned-char :in-out)
           (c gboolean)
           (d (* t)))
 (defun gdk::pixbuf-new-from-inline (a b c d)
   (|gdk_pixbuf_new_from_inline| a b (if c (if (eq c 0) 0 1) 0) d)))
(progn
 (defalien "gdk_pixbuf_fill" void (a (* t)) (b guint32))
 (defun gdk::pixbuf-fill (a b) (|gdk_pixbuf_fill| a b)))
(progn
 (defalien "gdk_pixbuf_save"
           gboolean
           (a (* t))
           (b c-string)
           (c c-string)
           (d (* t)))
 (defun gdk::pixbuf-save (a b c d)
   (let ((v257 (|gdk_pixbuf_save| a b c d)))
     (if (= v257 1) t nil))))
(progn
 (defalien "gdk_pixbuf_savev"
           gboolean
           (a (* t))
           (b c-string)
           (c c-string)
           (d (* t))
           (e (* t))
           (f (* t)))
 (defun gdk::pixbuf-savev (a b c d e f)
   (let ((v258 (|gdk_pixbuf_savev| a b c d e f)))
     (if (= v258 1) t nil))))
(progn
 (defalien "gdk_pixbuf_add_alpha"
           (* t)
           (a (* t))
           (b gboolean)
           (c guchar)
           (d guchar)
           (e guchar))
 (defun gdk::pixbuf-add-alpha (a b c d e)
   (|gdk_pixbuf_add_alpha| a (if b (if (eq b 0) 0 1) 0) c d e)))
(progn
 (defalien "gdk_pixbuf_copy_area"
           void
           (a (* t))
           (b int)
           (c int)
           (d int)
           (e int)
           (f (* t))
           (g int)
           (h int))
 (defun gdk::pixbuf-copy-area (a b c d e f g h)
   (|gdk_pixbuf_copy_area| a b c d e f g h)))
(progn
 (defalien "gdk_pixbuf_saturate_and_pixelate"
           void
           (a (* t))
           (b (* t))
           (c gfloat)
           (d gboolean))
 (defun gdk::pixbuf-saturate-and-pixelate (a b c d)
   (|gdk_pixbuf_saturate_and_pixelate| a b (coerce c 'single-float)
    (if d (if (eq d 0) 0 1) 0))))
(progn
 (defalien "gdk_pixbuf_scale"
           void
           (a (* t))
           (b (* t))
           (c int)
           (d int)
           (e int)
           (f int)
           (g double)
           (h double)
           (i double)
           (j double)
           (k GdkInterpType))
 (defun gdk::pixbuf-scale (a b c d e f g h i j k)
   (|gdk_pixbuf_scale| a b c d e f g h i j k)))
(progn
 (defalien "gdk_pixbuf_composite"
           void
           (a (* t))
           (b (* t))
           (c int)
           (d int)
           (e int)
           (f int)
           (g double)
           (h double)
           (i double)
           (j double)
           (k GdkInterpType)
           (l int))
 (defun gdk::pixbuf-composite (a b c d e f g h i j k l)
   (|gdk_pixbuf_composite| a b c d e f g h i j k l)))
(progn
 (defalien "gdk_pixbuf_composite_color"
           void
           (a (* t))
           (b (* t))
           (c int)
           (d int)
           (e int)
           (f int)
           (g double)
           (h double)
           (i double)
           (j double)
           (k GdkInterpType)
           (l int)
           (m int)
           (n int)
           (o int)
           (p guint32)
           (q guint32))
 (defun gdk::pixbuf-composite-color (a b c d e f g h i j k l m n o p q)
   (|gdk_pixbuf_composite_color| a b c d e f g h i j k l m n o p q)))
(progn
 (defalien "gdk_pixbuf_scale_simple"
           (* t)
           (a (* t))
           (b int)
           (c int)
           (d GdkInterpType))
 (defun gdk::pixbuf-scale-simple (a b c d) (|gdk_pixbuf_scale_simple| a b c d)))
(progn
 (defalien "gdk_pixbuf_composite_color_simple"
           (* t)
           (a (* t))
           (b int)
           (c int)
           (d GdkInterpType)
           (e int)
           (f int)
           (g guint32)
           (h guint32))
 (defun gdk::pixbuf-composite-color-simple (a b c d e f g h)
   (|gdk_pixbuf_composite_color_simple| a b c d e f g h)))
(progn
 (defalien "gdk_pixbuf_animation_get_type" GType)
 (defun gdk::pixbuf-animation-get-type () (|gdk_pixbuf_animation_get_type|)))
(progn
 (defalien "gdk_pixbuf_animation_new_from_file" (* t) (a c-string) (b (* t)))
 (defun gdk::pixbuf-animation-new-from-file (a b)
   (|gdk_pixbuf_animation_new_from_file| a b)))
(progn
 (defalien "gdk_pixbuf_animation_get_width" int (a (* t)))
 (defun gdk::pixbuf-animation-get-width (a)
   (|gdk_pixbuf_animation_get_width| a)))
(progn
 (defalien "gdk_pixbuf_animation_get_height" int (a (* t)))
 (defun gdk::pixbuf-animation-get-height (a)
   (|gdk_pixbuf_animation_get_height| a)))
(progn
 (defalien "gdk_pixbuf_animation_is_static_image" gboolean (a (* t)))
 (defun gdk::pixbuf-animation-is-static-image (a)
   (let ((v259 (|gdk_pixbuf_animation_is_static_image| a)))
     (if (= v259 1) t nil))))
(progn
 (defalien "gdk_pixbuf_animation_get_static_image" (* t) (a (* t)))
 (defun gdk::pixbuf-animation-get-static-image (a)
   (|gdk_pixbuf_animation_get_static_image| a)))
(progn
 (defalien "gdk_pixbuf_animation_get_iter" (* t) (a (* t)) (b (* t)))
 (defun gdk::pixbuf-animation-get-iter (a b)
   (|gdk_pixbuf_animation_get_iter| a b)))
(progn
 (defalien "gdk_pixbuf_animation_iter_get_type" GType)
 (defun gdk::pixbuf-animation-iter-get-type ()
   (|gdk_pixbuf_animation_iter_get_type|)))
(progn
 (defalien "gdk_pixbuf_animation_iter_get_delay_time" int (a (* t)))
 (defun gdk::pixbuf-animation-iter-get-delay-time (a)
   (|gdk_pixbuf_animation_iter_get_delay_time| a)))
(progn
 (defalien "gdk_pixbuf_animation_iter_get_pixbuf" (* t) (a (* t)))
 (defun gdk::pixbuf-animation-iter-get-pixbuf (a)
   (|gdk_pixbuf_animation_iter_get_pixbuf| a)))
(progn
 (defalien "gdk_pixbuf_animation_iter_on_currently_loading_frame"
           gboolean
           (a (* t)))
 (defun gdk::pixbuf-animation-iter-on-currently-loading-frame (a)
   (let ((v260 (|gdk_pixbuf_animation_iter_on_currently_loading_frame| a)))
     (if (= v260 1) t nil))))
(progn
 (defalien "gdk_pixbuf_animation_iter_advance" gboolean (a (* t)) (b (* t)))
 (defun gdk::pixbuf-animation-iter-advance (a b)
   (let ((v261 (|gdk_pixbuf_animation_iter_advance| a b)))
     (if (= v261 1) t nil))))
(progn
 (defalien "gdk_pixbuf_get_option" c-string (a (* t)) (b c-string))
 (defun gdk::pixbuf-get-option (a b) (|gdk_pixbuf_get_option| a b)))
(progn
 (defalien "gdk_pixbuf_loader_get_type" GType)
 (defun gdk::pixbuf-loader-get-type () (|gdk_pixbuf_loader_get_type|)))
(progn
 (defalien "gdk_pixbuf_loader_new" (* t))
 (defun gdk::pixbuf-loader-new () (|gdk_pixbuf_loader_new|)))
(progn
 (defalien "gdk_pixbuf_loader_new_with_type" (* t) (a c-string) (b (* t)))
 (defun gdk::pixbuf-loader-new-with-type (a b)
   (|gdk_pixbuf_loader_new_with_type| a b)))
(progn
 (defalien "gdk_pixbuf_loader_write"
           gboolean
           (a (* t))
           (b unsigned-char :in-out)
           (c gsize)
           (d (* t)))
 (defun gdk::pixbuf-loader-write (a b c d)
   (let ((v262 (multiple-value-list (|gdk_pixbuf_loader_write| a b c d))))
     (apply #'values (if (= 1 (car v262)) t nil) (cdr v262)))))
(progn
 (defalien "gdk_pixbuf_loader_get_pixbuf" (* t) (a (* t)))
 (defun gdk::pixbuf-loader-get-pixbuf (a) (|gdk_pixbuf_loader_get_pixbuf| a)))
(progn
 (defalien "gdk_pixbuf_loader_get_animation" (* t) (a (* t)))
 (defun gdk::pixbuf-loader-get-animation (a)
   (|gdk_pixbuf_loader_get_animation| a)))
(progn
 (defalien "gdk_pixbuf_loader_close" gboolean (a (* t)) (b (* t)))
 (defun gdk::pixbuf-loader-close (a b)
   (let ((v263 (|gdk_pixbuf_loader_close| a b)))
     (if (= v263 1) t nil))))
(progn
 (defalien "gdk_drawable_get_type" GType)
 (defun gdk::drawable-get-type () (|gdk_drawable_get_type|)))
(progn
 (defalien "gdk_drawable_set_data"
           void
           (a (* t))
           (b c-string)
           (c gpointer)
           (d GDestroyNotify))
 (defun gdk::drawable-set-data (a b c d) (|gdk_drawable_set_data| a b c d)))
(progn
 (defalien "gdk_drawable_get_data" gpointer (a (* t)) (b c-string))
 (defun gdk::drawable-get-data (a b) (|gdk_drawable_get_data| a b)))
(progn
 (defalien "gdk_drawable_get_size"
           void
           (a (* t))
           (b gint :in-out)
           (c gint :in-out))
 (defun gdk::drawable-get-size (a b c) (|gdk_drawable_get_size| a b c)))
(progn
 (defalien "gdk_drawable_set_colormap" void (a (* t)) (b (* t)))
 (defun gdk::drawable-set-colormap (a b) (|gdk_drawable_set_colormap| a b)))
(progn
 (defalien "gdk_drawable_get_colormap" (* t) (a (* t)))
 (defun gdk::drawable-get-colormap (a) (|gdk_drawable_get_colormap| a)))
(progn
 (defalien "gdk_drawable_get_visual" (* t) (a (* t)))
 (defun gdk::drawable-get-visual (a) (|gdk_drawable_get_visual| a)))
(progn
 (defalien "gdk_drawable_get_depth" gint (a (* t)))
 (defun gdk::drawable-get-depth (a) (|gdk_drawable_get_depth| a)))
(progn
 (defalien "gdk_drawable_ref" (* t) (a (* t)))
 (defun gdk::drawable-ref (a) (|gdk_drawable_ref| a)))
(progn
 (defalien "gdk_drawable_unref" void (a (* t)))
 (defun gdk::drawable-unref (a) (|gdk_drawable_unref| a)))
(progn
 (defalien "gdk_draw_point" void (a (* t)) (b (* t)) (c gint) (d gint))
 (defun gdk::draw-point (a b c d) (|gdk_draw_point| a b c d)))
(progn
 (defalien "gdk_draw_line"
           void
           (a (* t))
           (b (* t))
           (c gint)
           (d gint)
           (e gint)
           (f gint))
 (defun gdk::draw-line (a b c d e f) (|gdk_draw_line| a b c d e f)))
(progn
 (defalien "gdk_draw_rectangle"
           void
           (a (* t))
           (b (* t))
           (c gboolean)
           (d gint)
           (e gint)
           (f gint)
           (g gint))
 (defun gdk::draw-rectangle (a b c d e f g)
   (|gdk_draw_rectangle| a b (if c (if (eq c 0) 0 1) 0) d e f g)))
(progn
 (defalien "gdk_draw_arc"
           void
           (a (* t))
           (b (* t))
           (c gboolean)
           (d gint)
           (e gint)
           (f gint)
           (g gint)
           (h gint)
           (i gint))
 (defun gdk::draw-arc (a b c d e f g h i)
   (|gdk_draw_arc| a b (if c (if (eq c 0) 0 1) 0) d e f g h i)))
(progn
 (defalien "gdk_draw_polygon"
           void
           (a (* t))
           (b (* t))
           (c gboolean)
           (d (* t))
           (e gint))
 (defun gdk::draw-polygon (a b c d e)
   (|gdk_draw_polygon| a b (if c (if (eq c 0) 0 1) 0) d e)))
(progn
 (defalien "gdk_draw_string"
           void
           (a (* t))
           (b (* t))
           (c (* t))
           (d gint)
           (e gint)
           (f c-string))
 (defun gdk::draw-string (a b c d e f) (|gdk_draw_string| a b c d e f)))
(progn
 (defalien "gdk_draw_text"
           void
           (a (* t))
           (b (* t))
           (c (* t))
           (d gint)
           (e gint)
           (f c-string)
           (g gint))
 (defun gdk::draw-text (a b c d e f g) (|gdk_draw_text| a b c d e f g)))
(progn
 (defalien "gdk_draw_text_wc"
           void
           (a (* t))
           (b (* t))
           (c (* t))
           (d gint)
           (e gint)
           (f unsigned-int :in-out)
           (g gint))
 (defun gdk::draw-text-wc (a b c d e f g) (|gdk_draw_text_wc| a b c d e f g)))
(progn
 (defalien "gdk_draw_drawable"
           void
           (a (* t))
           (b (* t))
           (c (* t))
           (d gint)
           (e gint)
           (f gint)
           (g gint)
           (h gint)
           (i gint))
 (defun gdk::draw-drawable (a b c d e f g h i)
   (|gdk_draw_drawable| a b c d e f g h i)))
(progn
 (defalien "gdk_draw_image"
           void
           (a (* t))
           (b (* t))
           (c (* t))
           (d gint)
           (e gint)
           (f gint)
           (g gint)
           (h gint)
           (i gint))
 (defun gdk::draw-image (a b c d e f g h i)
   (|gdk_draw_image| a b c d e f g h i)))
(progn
 (defalien "gdk_draw_points" void (a (* t)) (b (* t)) (c (* t)) (d gint))
 (defun gdk::draw-points (a b c d) (|gdk_draw_points| a b c d)))
(progn
 (defalien "gdk_draw_segments" void (a (* t)) (b (* t)) (c (* t)) (d gint))
 (defun gdk::draw-segments (a b c d) (|gdk_draw_segments| a b c d)))
(progn
 (defalien "gdk_draw_lines" void (a (* t)) (b (* t)) (c (* t)) (d gint))
 (defun gdk::draw-lines (a b c d) (|gdk_draw_lines| a b c d)))
(progn
 (defalien "gdk_draw_glyphs"
           void
           (a (* t))
           (b (* t))
           (c (* t))
           (d gint)
           (e gint)
           (f (* t)))
 (defun gdk::draw-glyphs (a b c d e f) (|gdk_draw_glyphs| a b c d e f)))
(progn
 (defalien "gdk_draw_layout_line"
           void
           (a (* t))
           (b (* t))
           (c gint)
           (d gint)
           (e (* t)))
 (defun gdk::draw-layout-line (a b c d e) (|gdk_draw_layout_line| a b c d e)))
(progn
 (defalien "gdk_draw_layout"
           void
           (a (* t))
           (b (* t))
           (c gint)
           (d gint)
           (e (* t)))
 (defun gdk::draw-layout (a b c d e) (|gdk_draw_layout| a b c d e)))
(progn
 (defalien "gdk_draw_layout_line_with_colors"
           void
           (a (* t))
           (b (* t))
           (c gint)
           (d gint)
           (e (* t))
           (f (* t))
           (g (* t)))
 (defun gdk::draw-layout-line-with-colors (a b c d e f g)
   (|gdk_draw_layout_line_with_colors| a b c d e f g)))
(progn
 (defalien "gdk_drawable_get_image"
           (* t)
           (a (* t))
           (b gint)
           (c gint)
           (d gint)
           (e gint))
 (defun gdk::drawable-get-image (a b c d e)
   (|gdk_drawable_get_image| a b c d e)))
(progn
 (defalien "gdk_drawable_get_clip_region" (* t) (a (* t)))
 (defun gdk::drawable-get-clip-region (a) (|gdk_drawable_get_clip_region| a)))
(progn
 (defalien "gdk_drawable_get_visible_region" (* t) (a (* t)))
 (defun gdk::drawable-get-visible-region (a)
   (|gdk_drawable_get_visible_region| a)))
(progn
 (defalien "gdk_font_ref" (* t) (a (* t)))
 (defun gdk::font-ref (a) (|gdk_font_ref| a)))
(progn
 (defalien "gdk_font_unref" void (a (* t)))
 (defun gdk::font-unref (a) (|gdk_font_unref| a)))
(progn
 (defalien "gdk_font_id" gint (a (* t)))
 (defun gdk::font-id (a) (|gdk_font_id| a)))
(progn
 (defalien "gdk_font_load" (* t) (a c-string))
 (defun gdk::font-load (a) (|gdk_font_load| a)))
(progn
 (defalien "gdk_fontset_load" (* t) (a c-string))
 (defun gdk::fontset-load (a) (|gdk_fontset_load| a)))
(progn
 (defalien "gdk_font_from_description" (* t) (a (* t)))
 (defun gdk::font-from-description (a) (|gdk_font_from_description| a)))
(progn
 (defalien "gdk_string_width" gint (a (* t)) (b c-string))
 (defun gdk::string-width (a b) (|gdk_string_width| a b)))
(progn
 (defalien "gdk_text_width" gint (a (* t)) (b c-string) (c gint))
 (defun gdk::text-width (a b c) (|gdk_text_width| a b c)))
(progn
 (defalien "gdk_text_width_wc"
           gint
           (a (* t))
           (b unsigned-int :in-out)
           (c gint))
 (defun gdk::text-width-wc (a b c) (|gdk_text_width_wc| a b c)))
(progn
 (defalien "gdk_char_width" gint (a (* t)) (b gchar))
 (defun gdk::char-width (a b) (|gdk_char_width| a b)))
(progn
 (defalien "gdk_char_width_wc" gint (a (* t)) (b GdkWChar))
 (defun gdk::char-width-wc (a b) (|gdk_char_width_wc| a b)))
(progn
 (defalien "gdk_string_measure" gint (a (* t)) (b c-string))
 (defun gdk::string-measure (a b) (|gdk_string_measure| a b)))
(progn
 (defalien "gdk_text_measure" gint (a (* t)) (b c-string) (c gint))
 (defun gdk::text-measure (a b c) (|gdk_text_measure| a b c)))
(progn
 (defalien "gdk_char_measure" gint (a (* t)) (b gchar))
 (defun gdk::char-measure (a b) (|gdk_char_measure| a b)))
(progn
 (defalien "gdk_string_height" gint (a (* t)) (b c-string))
 (defun gdk::string-height (a b) (|gdk_string_height| a b)))
(progn
 (defalien "gdk_text_height" gint (a (* t)) (b c-string) (c gint))
 (defun gdk::text-height (a b c) (|gdk_text_height| a b c)))
(progn
 (defalien "gdk_char_height" gint (a (* t)) (b gchar))
 (defun gdk::char-height (a b) (|gdk_char_height| a b)))
(progn
 (defalien "gdk_text_extents"
           void
           (a (* t))
           (b c-string)
           (c gint)
           (d gint :in-out)
           (e gint :in-out)
           (f gint :in-out)
           (g gint :in-out)
           (h gint :in-out))
 (defun gdk::text-extents (a b c d e f g h)
   (|gdk_text_extents| a b c d e f g h)))
(progn
 (defalien "gdk_text_extents_wc"
           void
           (a (* t))
           (b unsigned-int :in-out)
           (c gint)
           (d gint :in-out)
           (e gint :in-out)
           (f gint :in-out)
           (g gint :in-out)
           (h gint :in-out))
 (defun gdk::text-extents-wc (a b c d e f g h)
   (|gdk_text_extents_wc| a b c d e f g h)))
(progn
 (defalien "gdk_string_extents"
           void
           (a (* t))
           (b c-string)
           (c gint :in-out)
           (d gint :in-out)
           (e gint :in-out)
           (f gint :in-out)
           (g gint :in-out))
 (defun gdk::string-extents (a b c d e f g)
   (|gdk_string_extents| a b c d e f g)))
(progn
 (defalien "gdk_image_get_type" GType)
 (defun gdk::image-get-type () (|gdk_image_get_type|)))
(progn
 (defalien "gdk_image_new" (* t) (a GdkImageType) (b (* t)) (c gint) (d gint))
 (defun gdk::image-new (a b c d) (|gdk_image_new| a b c d)))
(progn
 (defalien "gdk_image_put_pixel" void (a (* t)) (b gint) (c gint) (d guint32))
 (defun gdk::image-put-pixel (a b c d) (|gdk_image_put_pixel| a b c d)))
(progn
 (defalien "gdk_image_get_pixel" guint32 (a (* t)) (b gint) (c gint))
 (defun gdk::image-get-pixel (a b c) (|gdk_image_get_pixel| a b c)))
(progn
 (defalien "gdk_image_set_colormap" void (a (* t)) (b (* t)))
 (defun gdk::image-set-colormap (a b) (|gdk_image_set_colormap| a b)))
(progn
 (defalien "gdk_image_get_colormap" (* t) (a (* t)))
 (defun gdk::image-get-colormap (a) (|gdk_image_get_colormap| a)))
(progn
 (defalien "gdk_keymap_get_type" GType)
 (defun gdk::keymap-get-type () (|gdk_keymap_get_type|)))
(progn
 (defalien "gdk_keymap_get_default" (* t))
 (defun gdk::keymap-get-default () (|gdk_keymap_get_default|)))
(progn
 (defalien "gdk_keymap_lookup_key" guint (a (* t)) (b (* t)))
 (defun gdk::keymap-lookup-key (a b) (|gdk_keymap_lookup_key| a b)))
(progn
 (defalien "gdk_keymap_translate_keyboard_state"
           gboolean
           (a (* t))
           (b guint)
           (c GdkModifierType)
           (d gint)
           (e guint :in-out)
           (f gint :in-out)
           (g gint :in-out)
           (h GdkModifierType :in-out))
 (defun gdk::keymap-translate-keyboard-state (a b c d e f g h)
   (let ((v264
          (multiple-value-list
           (|gdk_keymap_translate_keyboard_state| a b c d e f g h))))
     (apply #'values (if (= 1 (car v264)) t nil) (cdr v264)))))
(progn
 (defalien "gdk_keymap_get_entries_for_keyval"
           gboolean
           (a (* t))
           (b guint)
           (c (* t))
           (d gint :in-out))
 (defun gdk::keymap-get-entries-for-keyval (a b c d)
   (let ((v265
          (multiple-value-list (|gdk_keymap_get_entries_for_keyval| a b c d))))
     (apply #'values (if (= 1 (car v265)) t nil) (cdr v265)))))
(progn
 (defalien "gdk_keymap_get_entries_for_keycode"
           gboolean
           (a (* t))
           (b guint)
           (c (* t))
           (d (* t))
           (e gint :in-out))
 (defun gdk::keymap-get-entries-for-keycode (a b c d e)
   (let ((v266
          (multiple-value-list
           (|gdk_keymap_get_entries_for_keycode| a b c d e))))
     (apply #'values (if (= 1 (car v266)) t nil) (cdr v266)))))
(progn
 (defalien "gdk_keymap_get_direction" PangoDirection (a (* t)))
 (defun gdk::keymap-get-direction (a) (|gdk_keymap_get_direction| a)))
(progn
 (defalien "gdk_keyval_name" gchar (a guint))
 (defun gdk::keyval-name (a) (|gdk_keyval_name| a)))
(progn
 (defalien "gdk_keyval_from_name" guint (a c-string))
 (defun gdk::keyval-from-name (a) (|gdk_keyval_from_name| a)))
(progn
 (defalien "gdk_keyval_convert_case"
           void
           (a guint)
           (b guint :in-out)
           (c guint :in-out))
 (defun gdk::keyval-convert-case (a b c) (|gdk_keyval_convert_case| a b c)))
(progn
 (defalien "gdk_keyval_to_upper" guint (a guint))
 (defun gdk::keyval-to-upper (a) (|gdk_keyval_to_upper| a)))
(progn
 (defalien "gdk_keyval_to_lower" guint (a guint))
 (defun gdk::keyval-to-lower (a) (|gdk_keyval_to_lower| a)))
(progn
 (defalien "gdk_keyval_is_upper" gboolean (a guint))
 (defun gdk::keyval-is-upper (a)
   (let ((v267 (|gdk_keyval_is_upper| a)))
     (if (= v267 1) t nil))))
(progn
 (defalien "gdk_keyval_is_lower" gboolean (a guint))
 (defun gdk::keyval-is-lower (a)
   (let ((v268 (|gdk_keyval_is_lower| a)))
     (if (= v268 1) t nil))))
(progn
 (defalien "gdk_keyval_to_unicode" guint32 (a guint))
 (defun gdk::keyval-to-unicode (a) (|gdk_keyval_to_unicode| a)))
(progn
 (defalien "gdk_unicode_to_keyval" guint (a guint32))
 (defun gdk::unicode-to-keyval (a) (|gdk_unicode_to_keyval| a)))
(progn
 (defalien "gdk_pixbuf_render_to_drawable"
           void
           (a (* t))
           (b (* t))
           (c (* t))
           (d int)
           (e int)
           (f int)
           (g int)
           (h int)
           (i int)
           (j GdkRgbDither)
           (k int)
           (l int))
 (defun gdk::pixbuf-render-to-drawable (a b c d e f g h i j k l)
   (|gdk_pixbuf_render_to_drawable| a b c d e f g h i j k l)))
(progn
 (defalien "gdk_pixbuf_render_to_drawable_alpha"
           void
           (a (* t))
           (b (* t))
           (c int)
           (d int)
           (e int)
           (f int)
           (g int)
           (h int)
           (i GdkPixbufAlphaMode)
           (j int)
           (k GdkRgbDither)
           (l int)
           (m int))
 (defun gdk::pixbuf-render-to-drawable-alpha (a b c d e f g h i j k l m)
   (|gdk_pixbuf_render_to_drawable_alpha| a b c d e f g h i j k l m)))
(progn
 (defalien "gdk_pixbuf_render_pixmap_and_mask"
           void
           (a (* t))
           (b (* t))
           (c (* t))
           (d int))
 (defun gdk::pixbuf-render-pixmap-and-mask (a b c d)
   (|gdk_pixbuf_render_pixmap_and_mask| a b c d)))
(progn
 (defalien "gdk_pixbuf_get_from_drawable"
           (* t)
           (a (* t))
           (b (* t))
           (c (* t))
           (d int)
           (e int)
           (f int)
           (g int)
           (h int)
           (i int))
 (defun gdk::pixbuf-get-from-drawable (a b c d e f g h i)
   (|gdk_pixbuf_get_from_drawable| a b c d e f g h i)))
(progn
 (defalien "gdk_pixbuf_get_from_image"
           (* t)
           (a (* t))
           (b (* t))
           (c (* t))
           (d int)
           (e int)
           (f int)
           (g int)
           (h int)
           (i int))
 (defun gdk::pixbuf-get-from-image (a b c d e f g h i)
   (|gdk_pixbuf_get_from_image| a b c d e f g h i)))
(progn
 (defalien "gdk_pixmap_get_type" GType)
 (defun gdk::pixmap-get-type () (|gdk_pixmap_get_type|)))
(progn
 (defalien "gdk_pixmap_new" (* t) (a (* t)) (b gint) (c gint) (d gint))
 (defun gdk::pixmap-new (a b c d) (|gdk_pixmap_new| a b c d)))
(progn
 (defalien "gdk_bitmap_create_from_data"
           (* t)
           (a (* t))
           (b c-string)
           (c gint)
           (d gint))
 (defun gdk::bitmap-create-from-data (a b c d)
   (|gdk_bitmap_create_from_data| a b c d)))
(progn
 (defalien "gdk_pixmap_create_from_data"
           (* t)
           (a (* t))
           (b c-string)
           (c gint)
           (d gint)
           (e gint)
           (f (* t))
           (g (* t)))
 (defun gdk::pixmap-create-from-data (a b c d e f g)
   (|gdk_pixmap_create_from_data| a b c d e f g)))
(progn
 (defalien "gdk_pixmap_create_from_xpm"
           (* t)
           (a (* t))
           (b (* t))
           (c (* t))
           (d c-string))
 (defun gdk::pixmap-create-from-xpm (a b c d)
   (|gdk_pixmap_create_from_xpm| a b c d)))
(progn
 (defalien "gdk_pixmap_colormap_create_from_xpm"
           (* t)
           (a (* t))
           (b (* t))
           (c (* t))
           (d (* t))
           (e c-string))
 (defun gdk::pixmap-colormap-create-from-xpm (a b c d e)
   (|gdk_pixmap_colormap_create_from_xpm| a b c d e)))
(progn
 (defalien "gdk_pixmap_create_from_xpm_d"
           (* t)
           (a (* t))
           (b (* t))
           (c (* t))
           (d (* t)))
 (defun gdk::pixmap-create-from-xpm-d (a b c d)
   (|gdk_pixmap_create_from_xpm_d| a b c d)))
(progn
 (defalien "gdk_pixmap_colormap_create_from_xpm_d"
           (* t)
           (a (* t))
           (b (* t))
           (c (* t))
           (d (* t))
           (e (* t)))
 (defun gdk::pixmap-colormap-create-from-xpm-d (a b c d e)
   (|gdk_pixmap_colormap_create_from_xpm_d| a b c d e)))
(progn
 (defalien "gdk_pixmap_foreign_new" (* t) (a GdkNativeWindow))
 (defun gdk::pixmap-foreign-new (a) (|gdk_pixmap_foreign_new| a)))
(progn
 (defalien "gdk_pixmap_lookup" (* t) (a GdkNativeWindow))
 (defun gdk::pixmap-lookup (a) (|gdk_pixmap_lookup| a)))
(progn
 (defalien "gdk_atom_intern" GdkAtom (a c-string) (b gboolean))
 (defun gdk::atom-intern (a b)
   (|gdk_atom_intern| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gdk_atom_name" gchar (a GdkAtom))
 (defun gdk::atom-name (a) (|gdk_atom_name| a)))
(progn
 (defalien "gdk_property_get"
           gboolean
           (a (* t))
           (b GdkAtom)
           (c GdkAtom)
           (d gulong)
           (e gulong)
           (f gint)
           (g (* t))
           (h gint :in-out)
           (i gint :in-out)
           (j (* t)))
 (defun gdk::property-get (a b c d e f g h i j)
   (let ((v269 (multiple-value-list (|gdk_property_get| a b c d e f g h i j))))
     (apply #'values (if (= 1 (car v269)) t nil) (cdr v269)))))
(progn
 (defalien "gdk_property_change"
           void
           (a (* t))
           (b GdkAtom)
           (c GdkAtom)
           (d gint)
           (e GdkPropMode)
           (f unsigned-char :in-out)
           (g gint))
 (defun gdk::property-change (a b c d e f g)
   (|gdk_property_change| a b c d e f g)))
(progn
 (defalien "gdk_property_delete" void (a (* t)) (b GdkAtom))
 (defun gdk::property-delete (a b) (|gdk_property_delete| a b)))
(progn
 (defalien "gdk_selection_owner_get" (* t) (a GdkAtom))
 (defun gdk::selection-owner-get (a) (|gdk_selection_owner_get| a)))
(progn
 (defalien "gdk_selection_convert"
           void
           (a (* t))
           (b GdkAtom)
           (c GdkAtom)
           (d guint32))
 (defun gdk::selection-convert (a b c d) (|gdk_selection_convert| a b c d)))
(progn
 (defalien "gdk_selection_property_get"
           gboolean
           (a (* t))
           (b (* t))
           (c (* t))
           (d gint :in-out))
 (defun gdk::selection-property-get (a b c d)
   (let ((v270 (multiple-value-list (|gdk_selection_property_get| a b c d))))
     (apply #'values (if (= 1 (car v270)) t nil) (cdr v270)))))
(progn
 (defalien "gdk_selection_send_notify"
           void
           (a guint32)
           (b GdkAtom)
           (c GdkAtom)
           (d GdkAtom)
           (e guint32))
 (defun gdk::selection-send-notify (a b c d e)
   (|gdk_selection_send_notify| a b c d e)))
(progn
 (defalien "gdk_visual_get_best_depth" gint)
 (defun gdk::visual-get-best-depth () (|gdk_visual_get_best_depth|)))
(progn
 (defalien "gdk_visual_get_best_type" GdkVisualType)
 (defun gdk::visual-get-best-type () (|gdk_visual_get_best_type|)))
(progn
 (defalien "gdk_visual_get_system" (* t))
 (defun gdk::visual-get-system () (|gdk_visual_get_system|)))
(progn
 (defalien "gdk_visual_get_best" (* t))
 (defun gdk::visual-get-best () (|gdk_visual_get_best|)))
(progn
 (defalien "gdk_visual_get_best_with_depth" (* t) (a gint))
 (defun gdk::visual-get-best-with-depth (a)
   (|gdk_visual_get_best_with_depth| a)))
(progn
 (defalien "gdk_visual_get_best_with_type" (* t) (a GdkVisualType))
 (defun gdk::visual-get-best-with-type (a) (|gdk_visual_get_best_with_type| a)))
(progn
 (defalien "gdk_visual_get_best_with_both" (* t) (a gint) (b GdkVisualType))
 (defun gdk::visual-get-best-with-both (a b)
   (|gdk_visual_get_best_with_both| a b)))
(progn
 (defalien "gdk_query_depths" void (a (* t)) (b gint :in-out))
 (defun gdk::query-depths (a b) (|gdk_query_depths| a b)))
(progn
 (defalien "gdk_query_visual_types" void (a (* t)) (b gint :in-out))
 (defun gdk::query-visual-types (a b) (|gdk_query_visual_types| a b)))
(progn
 (defalien "gdk_list_visuals" (* t))
 (defun gdk::list-visuals () (|gdk_list_visuals|)))
(progn
 (defalien "gdk_window_object_get_type" GType)
 (defun gdk::window-object-get-type () (|gdk_window_object_get_type|)))
(progn
 (defalien "gdk_window_new" (* t) (a (* t)) (b (* t)) (c gint))
 (defun gdk::window-new (a b c) (|gdk_window_new| a b c)))
(progn
 (defalien "gdk_window_destroy" void (a (* t)))
 (defun gdk::window-destroy (a) (|gdk_window_destroy| a)))
(progn
 (defalien "gdk_window_get_window_type" GdkWindowType (a (* t)))
 (defun gdk::window-get-window-type (a) (|gdk_window_get_window_type| a)))
(progn
 (defalien "gdk_window_at_pointer" (* t) (a gint :in-out) (b gint :in-out))
 (defun gdk::window-at-pointer (a b) (|gdk_window_at_pointer| a b)))
(progn
 (defalien "gdk_window_show" void (a (* t)))
 (defun gdk::window-show (a) (|gdk_window_show| a)))
(progn
 (defalien "gdk_window_hide" void (a (* t)))
 (defun gdk::window-hide (a) (|gdk_window_hide| a)))
(progn
 (defalien "gdk_window_withdraw" void (a (* t)))
 (defun gdk::window-withdraw (a) (|gdk_window_withdraw| a)))
(progn
 (defalien "gdk_window_move" void (a (* t)) (b gint) (c gint))
 (defun gdk::window-move (a b c) (|gdk_window_move| a b c)))
(progn
 (defalien "gdk_window_resize" void (a (* t)) (b gint) (c gint))
 (defun gdk::window-resize (a b c) (|gdk_window_resize| a b c)))
(progn
 (defalien "gdk_window_move_resize"
           void
           (a (* t))
           (b gint)
           (c gint)
           (d gint)
           (e gint))
 (defun gdk::window-move-resize (a b c d e)
   (|gdk_window_move_resize| a b c d e)))
(progn
 (defalien "gdk_window_reparent" void (a (* t)) (b (* t)) (c gint) (d gint))
 (defun gdk::window-reparent (a b c d) (|gdk_window_reparent| a b c d)))
(progn
 (defalien "gdk_window_clear" void (a (* t)))
 (defun gdk::window-clear (a) (|gdk_window_clear| a)))
(progn
 (defalien "gdk_window_clear_area"
           void
           (a (* t))
           (b gint)
           (c gint)
           (d gint)
           (e gint))
 (defun gdk::window-clear-area (a b c d e) (|gdk_window_clear_area| a b c d e)))
(progn
 (defalien "gdk_window_clear_area_e"
           void
           (a (* t))
           (b gint)
           (c gint)
           (d gint)
           (e gint))
 (defun gdk::window-clear-area-e (a b c d e)
   (|gdk_window_clear_area_e| a b c d e)))
(progn
 (defalien "gdk_window_raise" void (a (* t)))
 (defun gdk::window-raise (a) (|gdk_window_raise| a)))
(progn
 (defalien "gdk_window_lower" void (a (* t)))
 (defun gdk::window-lower (a) (|gdk_window_lower| a)))
(progn
 (defalien "gdk_window_focus" void (a (* t)) (b guint32))
 (defun gdk::window-focus (a b) (|gdk_window_focus| a b)))
(progn
 (defalien "gdk_window_set_user_data" void (a (* t)) (b gpointer))
 (defun gdk::window-set-user-data (a b) (|gdk_window_set_user_data| a b)))
(progn
 (defalien "gdk_window_set_override_redirect" void (a (* t)) (b gboolean))
 (defun gdk::window-set-override-redirect (a b)
   (|gdk_window_set_override_redirect| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gdk_window_add_filter"
           void
           (a (* t))
           (b GdkFilterFunc)
           (c gpointer))
 (defun gdk::window-add-filter (a b c) (|gdk_window_add_filter| a b c)))
(progn
 (defalien "gdk_window_remove_filter"
           void
           (a (* t))
           (b GdkFilterFunc)
           (c gpointer))
 (defun gdk::window-remove-filter (a b c) (|gdk_window_remove_filter| a b c)))
(progn
 (defalien "gdk_window_scroll" void (a (* t)) (b gint) (c gint))
 (defun gdk::window-scroll (a b c) (|gdk_window_scroll| a b c)))
(progn
 (defalien "gdk_window_shape_combine_mask"
           void
           (a (* t))
           (b (* t))
           (c gint)
           (d gint))
 (defun gdk::window-shape-combine-mask (a b c d)
   (|gdk_window_shape_combine_mask| a b c d)))
(progn
 (defalien "gdk_window_set_child_shapes" void (a (* t)))
 (defun gdk::window-set-child-shapes (a) (|gdk_window_set_child_shapes| a)))
(progn
 (defalien "gdk_window_merge_child_shapes" void (a (* t)))
 (defun gdk::window-merge-child-shapes (a) (|gdk_window_merge_child_shapes| a)))
(progn
 (defalien "gdk_window_is_visible" gboolean (a (* t)))
 (defun gdk::window-is-visible (a)
   (let ((v271 (|gdk_window_is_visible| a)))
     (if (= v271 1) t nil))))
(progn
 (defalien "gdk_window_is_viewable" gboolean (a (* t)))
 (defun gdk::window-is-viewable (a)
   (let ((v272 (|gdk_window_is_viewable| a)))
     (if (= v272 1) t nil))))
(progn
 (defalien "gdk_window_get_state" GdkWindowState (a (* t)))
 (defun gdk::window-get-state (a) (|gdk_window_get_state| a)))
(progn
 (defalien "gdk_window_set_static_gravities" gboolean (a (* t)) (b gboolean))
 (defun gdk::window-set-static-gravities (a b)
   (let ((v273
          (|gdk_window_set_static_gravities| a (if b (if (eq b 0) 0 1) 0))))
     (if (= v273 1) t nil))))
(progn
 (defalien "gdk_window_foreign_new" (* t) (a GdkNativeWindow))
 (defun gdk::window-foreign-new (a) (|gdk_window_foreign_new| a)))
(progn
 (defalien "gdk_window_lookup" (* t) (a GdkNativeWindow))
 (defun gdk::window-lookup (a) (|gdk_window_lookup| a)))
(progn
 (defalien "gdk_window_set_hints"
           void
           (a (* t))
           (b gint)
           (c gint)
           (d gint)
           (e gint)
           (f gint)
           (g gint)
           (h gint))
 (defun gdk::window-set-hints (a b c d e f g h)
   (|gdk_window_set_hints| a b c d e f g h)))
(progn
 (defalien "gdk_window_set_type_hint" void (a (* t)) (b GdkWindowTypeHint))
 (defun gdk::window-set-type-hint (a b) (|gdk_window_set_type_hint| a b)))
(progn
 (defalien "gdk_window_set_modal_hint" void (a (* t)) (b gboolean))
 (defun gdk::window-set-modal-hint (a b)
   (|gdk_window_set_modal_hint| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gdk_window_set_geometry_hints"
           void
           (a (* t))
           (b (* t))
           (c GdkWindowHints))
 (defun gdk::window-set-geometry-hints (a b c)
   (|gdk_window_set_geometry_hints| a b c)))
(progn
 (defalien "gdk_set_sm_client_id" void (a c-string))
 (defun gdk::set-sm-client-id (a) (|gdk_set_sm_client_id| a)))
(progn
 (defalien "gdk_window_begin_paint_rect" void (a (* t)) (b (* t)))
 (defun gdk::window-begin-paint-rect (a b) (|gdk_window_begin_paint_rect| a b)))
(progn
 (defalien "gdk_window_begin_paint_region" void (a (* t)) (b (* t)))
 (defun gdk::window-begin-paint-region (a b)
   (|gdk_window_begin_paint_region| a b)))
(progn
 (defalien "gdk_window_end_paint" void (a (* t)))
 (defun gdk::window-end-paint (a) (|gdk_window_end_paint| a)))
(progn
 (defalien "gdk_window_set_title" void (a (* t)) (b c-string))
 (defun gdk::window-set-title (a b) (|gdk_window_set_title| a b)))
(progn
 (defalien "gdk_window_set_role" void (a (* t)) (b c-string))
 (defun gdk::window-set-role (a b) (|gdk_window_set_role| a b)))
(progn
 (defalien "gdk_window_set_transient_for" void (a (* t)) (b (* t)))
 (defun gdk::window-set-transient-for (a b)
   (|gdk_window_set_transient_for| a b)))
(progn
 (defalien "gdk_window_set_background" void (a (* t)) (b (* t)))
 (defun gdk::window-set-background (a b) (|gdk_window_set_background| a b)))
(progn
 (defalien "gdk_window_set_back_pixmap" void (a (* t)) (b (* t)) (c gboolean))
 (defun gdk::window-set-back-pixmap (a b c)
   (|gdk_window_set_back_pixmap| a b (if c (if (eq c 0) 0 1) 0))))
(progn
 (defalien "gdk_window_set_cursor" void (a (* t)) (b (* t)))
 (defun gdk::window-set-cursor (a b) (|gdk_window_set_cursor| a b)))
(progn
 (defalien "gdk_window_get_geometry"
           void
           (a (* t))
           (b gint :in-out)
           (c gint :in-out)
           (d gint :in-out)
           (e gint :in-out)
           (f gint :in-out))
 (defun gdk::window-get-geometry (a b c d e f)
   (|gdk_window_get_geometry| a b c d e f)))
(progn
 (defalien "gdk_window_get_position"
           void
           (a (* t))
           (b gint :in-out)
           (c gint :in-out))
 (defun gdk::window-get-position (a b c) (|gdk_window_get_position| a b c)))
(progn
 (defalien "gdk_window_get_origin"
           gint
           (a (* t))
           (b gint :in-out)
           (c gint :in-out))
 (defun gdk::window-get-origin (a b c) (|gdk_window_get_origin| a b c)))
(progn
 (defalien "gdk_window_get_deskrelative_origin"
           gboolean
           (a (* t))
           (b gint :in-out)
           (c gint :in-out))
 (defun gdk::window-get-deskrelative-origin (a b c)
   (let ((v274
          (multiple-value-list (|gdk_window_get_deskrelative_origin| a b c))))
     (apply #'values (if (= 1 (car v274)) t nil) (cdr v274)))))
(progn
 (defalien "gdk_window_get_root_origin"
           void
           (a (* t))
           (b gint :in-out)
           (c gint :in-out))
 (defun gdk::window-get-root-origin (a b c)
   (|gdk_window_get_root_origin| a b c)))
(progn
 (defalien "gdk_window_get_frame_extents" void (a (* t)) (b (* t)))
 (defun gdk::window-get-frame-extents (a b)
   (|gdk_window_get_frame_extents| a b)))
(progn
 (defalien "gdk_window_get_pointer"
           (* t)
           (a (* t))
           (b gint :in-out)
           (c gint :in-out)
           (d GdkModifierType :in-out))
 (defun gdk::window-get-pointer (a b c d) (|gdk_window_get_pointer| a b c d)))
(progn
 (defalien "gdk_window_get_parent" (* t) (a (* t)))
 (defun gdk::window-get-parent (a) (|gdk_window_get_parent| a)))
(progn
 (defalien "gdk_window_get_toplevel" (* t) (a (* t)))
 (defun gdk::window-get-toplevel (a) (|gdk_window_get_toplevel| a)))
(progn
 (defalien "gdk_window_get_children" (* t) (a (* t)))
 (defun gdk::window-get-children (a) (|gdk_window_get_children| a)))
(progn
 (defalien "gdk_window_peek_children" (* t) (a (* t)))
 (defun gdk::window-peek-children (a) (|gdk_window_peek_children| a)))
(progn
 (defalien "gdk_window_get_events" GdkEventMask (a (* t)))
 (defun gdk::window-get-events (a) (|gdk_window_get_events| a)))
(progn
 (defalien "gdk_window_set_events" void (a (* t)) (b GdkEventMask))
 (defun gdk::window-set-events (a b) (|gdk_window_set_events| a b)))
(progn
 (defalien "gdk_window_set_icon_list" void (a (* t)) (b (* t)))
 (defun gdk::window-set-icon-list (a b) (|gdk_window_set_icon_list| a b)))
(progn
 (defalien "gdk_window_set_icon" void (a (* t)) (b (* t)) (c (* t)) (d (* t)))
 (defun gdk::window-set-icon (a b c d) (|gdk_window_set_icon| a b c d)))
(progn
 (defalien "gdk_window_set_icon_name" void (a (* t)) (b c-string))
 (defun gdk::window-set-icon-name (a b) (|gdk_window_set_icon_name| a b)))
(progn
 (defalien "gdk_window_set_group" void (a (* t)) (b (* t)))
 (defun gdk::window-set-group (a b) (|gdk_window_set_group| a b)))
(progn
 (defalien "gdk_window_set_decorations" void (a (* t)) (b GdkWMDecoration))
 (defun gdk::window-set-decorations (a b) (|gdk_window_set_decorations| a b)))
(progn
 (defalien "gdk_window_get_decorations"
           gboolean
           (a (* t))
           (b GdkWMDecoration :in-out))
 (defun gdk::window-get-decorations (a b)
   (let ((v275 (multiple-value-list (|gdk_window_get_decorations| a b))))
     (apply #'values (if (= 1 (car v275)) t nil) (cdr v275)))))
(progn
 (defalien "gdk_window_set_functions" void (a (* t)) (b GdkWMFunction))
 (defun gdk::window-set-functions (a b) (|gdk_window_set_functions| a b)))
(progn
 (defalien "gdk_window_get_toplevels" (* t))
 (defun gdk::window-get-toplevels () (|gdk_window_get_toplevels|)))
(progn
 (defalien "gdk_window_iconify" void (a (* t)))
 (defun gdk::window-iconify (a) (|gdk_window_iconify| a)))
(progn
 (defalien "gdk_window_deiconify" void (a (* t)))
 (defun gdk::window-deiconify (a) (|gdk_window_deiconify| a)))
(progn
 (defalien "gdk_window_stick" void (a (* t)))
 (defun gdk::window-stick (a) (|gdk_window_stick| a)))
(progn
 (defalien "gdk_window_unstick" void (a (* t)))
 (defun gdk::window-unstick (a) (|gdk_window_unstick| a)))
(progn
 (defalien "gdk_window_maximize" void (a (* t)))
 (defun gdk::window-maximize (a) (|gdk_window_maximize| a)))
(progn
 (defalien "gdk_window_unmaximize" void (a (* t)))
 (defun gdk::window-unmaximize (a) (|gdk_window_unmaximize| a)))
(progn
 (defalien "gdk_window_register_dnd" void (a (* t)))
 (defun gdk::window-register-dnd (a) (|gdk_window_register_dnd| a)))
(progn
 (defalien "gdk_window_begin_resize_drag"
           void
           (a (* t))
           (b GdkWindowEdge)
           (c gint)
           (d gint)
           (e gint)
           (f guint32))
 (defun gdk::window-begin-resize-drag (a b c d e f)
   (|gdk_window_begin_resize_drag| a b c d e f)))
(progn
 (defalien "gdk_window_begin_move_drag"
           void
           (a (* t))
           (b gint)
           (c gint)
           (d gint)
           (e guint32))
 (defun gdk::window-begin-move-drag (a b c d e)
   (|gdk_window_begin_move_drag| a b c d e)))
(progn
 (defalien "gdk_window_invalidate_rect" void (a (* t)) (b (* t)) (c gboolean))
 (defun gdk::window-invalidate-rect (a b c)
   (|gdk_window_invalidate_rect| a b (if c (if (eq c 0) 0 1) 0))))
(progn
 (defalien "gdk_window_invalidate_region"
           void
           (a (* t))
           (b (* t))
           (c gboolean))
 (defun gdk::window-invalidate-region (a b c)
   (|gdk_window_invalidate_region| a b (if c (if (eq c 0) 0 1) 0))))
(progn
 (defalien "gdk_window_invalidate_maybe_recurse"
           void
           (a (* t))
           (b (* t))
           (c (* t))
           (d gpointer))
 (defun gdk::window-invalidate-maybe-recurse (a b c d)
   (|gdk_window_invalidate_maybe_recurse| a b c d)))
(progn
 (defalien "gdk_window_get_update_area" (* t) (a (* t)))
 (defun gdk::window-get-update-area (a) (|gdk_window_get_update_area| a)))
(progn
 (defalien "gdk_window_freeze_updates" void (a (* t)))
 (defun gdk::window-freeze-updates (a) (|gdk_window_freeze_updates| a)))
(progn
 (defalien "gdk_window_thaw_updates" void (a (* t)))
 (defun gdk::window-thaw-updates (a) (|gdk_window_thaw_updates| a)))
(progn
 (defalien "gdk_window_process_all_updates" void)
 (defun gdk::window-process-all-updates () (|gdk_window_process_all_updates|)))
(progn
 (defalien "gdk_window_process_updates" void (a (* t)) (b gboolean))
 (defun gdk::window-process-updates (a b)
   (|gdk_window_process_updates| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gdk_window_set_debug_updates" void (a gboolean))
 (defun gdk::window-set-debug-updates (a)
   (|gdk_window_set_debug_updates| (if a (if (eq a 0) 0 1) 0))))
(progn
 (defalien "gdk_window_constrain_size"
           void
           (a (* t))
           (b guint)
           (c gint)
           (d gint)
           (e gint :in-out)
           (f gint :in-out))
 (defun gdk::window-constrain-size (a b c d e f)
   (|gdk_window_constrain_size| a b c d e f)))
(progn
 (defalien "gdk_window_get_internal_paint_info"
           void
           (a (* t))
           (b (* t))
           (c gint :in-out)
           (d gint :in-out))
 (defun gdk::window-get-internal-paint-info (a b c d)
   (|gdk_window_get_internal_paint_info| a b c d)))
(progn
 (defalien "gdk_set_pointer_hooks" (* t) (a (* t)))
 (defun gdk::set-pointer-hooks (a) (|gdk_set_pointer_hooks| a)))
(progn
 (defalien "gdk_get_default_root_window" (* t))
 (defun gdk::get-default-root-window () (|gdk_get_default_root_window|)))
(progn
 (defalien "gdk_pointer_grab"
           GdkGrabStatus
           (a (* t))
           (b gboolean)
           (c GdkEventMask)
           (d (* t))
           (e (* t))
           (f guint32))
 (defun gdk::pointer-grab (a b c d e f)
   (|gdk_pointer_grab| a (if b (if (eq b 0) 0 1) 0) c d e f)))
(progn
 (defalien "gdk_keyboard_grab"
           GdkGrabStatus
           (a (* t))
           (b gboolean)
           (c guint32))
 (defun gdk::keyboard-grab (a b c)
   (|gdk_keyboard_grab| a (if b (if (eq b 0) 0 1) 0) c)))
(progn
 (defalien "gdk_pointer_ungrab" void (a guint32))
 (defun gdk::pointer-ungrab (a) (|gdk_pointer_ungrab| a)))
(progn
 (defalien "gdk_keyboard_ungrab" void (a guint32))
 (defun gdk::keyboard-ungrab (a) (|gdk_keyboard_ungrab| a)))
(progn
 (defalien "gdk_pointer_is_grabbed" gboolean)
 (defun gdk::pointer-is-grabbed ()
   (let ((v276 (|gdk_pointer_is_grabbed|)))
     (if (= v276 1) t nil))))
(progn
 (defalien "gdk_screen_width" gint)
 (defun gdk::screen-width () (|gdk_screen_width|)))
(progn
 (defalien "gdk_screen_height" gint)
 (defun gdk::screen-height () (|gdk_screen_height|)))
(progn
 (defalien "gdk_screen_width_mm" gint)
 (defun gdk::screen-width-mm () (|gdk_screen_width_mm|)))
(progn
 (defalien "gdk_screen_height_mm" gint)
 (defun gdk::screen-height-mm () (|gdk_screen_height_mm|)))
(progn (defalien "gdk_beep" void) (defun gdk::beep () (|gdk_beep|)))
(progn (defalien "gdk_flush" void) (defun gdk::flush () (|gdk_flush|)))
(progn
 (defalien "gdk_set_double_click_time" void (a guint))
 (defun gdk::set-double-click-time (a) (|gdk_set_double_click_time| a)))
(progn
 (defalien "gdk_rectangle_intersect" gboolean (a (* t)) (b (* t)) (c (* t)))
 (defun gdk::rectangle-intersect (a b c)
   (let ((v277 (|gdk_rectangle_intersect| a b c)))
     (if (= v277 1) t nil))))
(progn
 (defalien "gdk_rectangle_union" void (a (* t)) (b (* t)) (c (* t)))
 (defun gdk::rectangle-union (a b c) (|gdk_rectangle_union| a b c)))
(progn
 (defalien "gdk_threads_enter" void)
 (defun gdk::threads-enter () (|gdk_threads_enter|)))
(progn
 (defalien "gdk_threads_leave" void)
 (defun gdk::threads-leave () (|gdk_threads_leave|)))
(progn
 (defalien "gdk_threads_init" void)
 (defun gdk::threads-init () (|gdk_threads_init|)))
(progn
 (defalien "gtk_accel_group_get_type" GType)
 (defun gtk::accel-group-get-type () (|gtk_accel_group_get_type|)))
(progn
 (defalien "gtk_accel_group_new" (* t))
 (defun gtk::accel-group-new () (|gtk_accel_group_new|)))
(progn
 (defalien "gtk_accel_group_lock" void (a (* t)))
 (defun gtk::accel-group-lock (a) (|gtk_accel_group_lock| a)))
(progn
 (defalien "gtk_accel_group_unlock" void (a (* t)))
 (defun gtk::accel-group-unlock (a) (|gtk_accel_group_unlock| a)))
(progn
 (defalien "gtk_accel_group_connect"
           void
           (a (* t))
           (b guint)
           (c GdkModifierType)
           (d GtkAccelFlags)
           (e (* t)))
 (defun gtk::accel-group-connect (a b c d e)
   (|gtk_accel_group_connect| a b c d e)))
(progn
 (defalien "gtk_accel_group_connect_by_path"
           void
           (a (* t))
           (b c-string)
           (c (* t)))
 (defun gtk::accel-group-connect-by-path (a b c)
   (|gtk_accel_group_connect_by_path| a b c)))
(progn
 (defalien "gtk_accel_group_disconnect" gboolean (a (* t)) (b (* t)))
 (defun gtk::accel-group-disconnect (a b)
   (let ((v278 (|gtk_accel_group_disconnect| a b)))
     (if (= v278 1) t nil))))
(progn
 (defalien "gtk_accel_group_disconnect_key"
           gboolean
           (a (* t))
           (b guint)
           (c GdkModifierType))
 (defun gtk::accel-group-disconnect-key (a b c)
   (let ((v279 (|gtk_accel_group_disconnect_key| a b c)))
     (if (= v279 1) t nil))))
(progn
 (defalien "gtk_accel_groups_activate"
           gboolean
           (a (* t))
           (b guint)
           (c GdkModifierType))
 (defun gtk::accel-groups-activate (a b c)
   (let ((v280 (|gtk_accel_groups_activate| a b c)))
     (if (= v280 1) t nil))))
(progn
 (defalien "gtk_accel_groups_from_object" (* t) (a (* t)))
 (defun gtk::accel-groups-from-object (a) (|gtk_accel_groups_from_object| a)))
(progn
 (defalien "gtk_accel_group_find"
           (* t)
           (a (* t))
           (b GtkAccelGroupFindFunc)
           (c gpointer))
 (defun gtk::accel-group-find (a b c) (|gtk_accel_group_find| a b c)))
(progn
 (defalien "gtk_accel_group_from_accel_closure" (* t) (a (* t)))
 (defun gtk::accel-group-from-accel-closure (a)
   (|gtk_accel_group_from_accel_closure| a)))
(progn
 (defalien "gtk_accelerator_valid" gboolean (a guint) (b GdkModifierType))
 (defun gtk::accelerator-valid (a b)
   (let ((v281 (|gtk_accelerator_valid| a b)))
     (if (= v281 1) t nil))))
(progn
 (defalien "gtk_accelerator_parse"
           void
           (a c-string)
           (b guint :in-out)
           (c GdkModifierType :in-out))
 (defun gtk::accelerator-parse (a b c) (|gtk_accelerator_parse| a b c)))
(progn
 (defalien "gtk_accelerator_name" gchar (a guint) (b GdkModifierType))
 (defun gtk::accelerator-name (a b) (|gtk_accelerator_name| a b)))
(progn
 (defalien "gtk_accelerator_set_default_mod_mask" void (a GdkModifierType))
 (defun gtk::accelerator-set-default-mod-mask (a)
   (|gtk_accelerator_set_default_mod_mask| a)))
(progn
 (defalien "gtk_accelerator_get_default_mod_mask" guint)
 (defun gtk::accelerator-get-default-mod-mask ()
   (|gtk_accelerator_get_default_mod_mask|)))
(progn
 (defalien "gtk_object_get_type" GtkType)
 (defun gtk::object-get-type () (|gtk_object_get_type|)))
(progn
 (defalien "gtk_object_new" (* t) (a GtkType) (b c-string))
 (defun gtk::object-new (a b) (|gtk_object_new| a b)))
(progn
 (defalien "gtk_object_sink" void (a (* t)))
 (defun gtk::object-sink (a) (|gtk_object_sink| a)))
(progn
 (defalien "gtk_object_destroy" void (a (* t)))
 (defun gtk::object-destroy (a) (|gtk_object_destroy| a)))
(progn
 (defalien "gtk_adjustment_get_type" GType)
 (defun gtk::adjustment-get-type () (|gtk_adjustment_get_type|)))
(progn
 (defalien "gtk_adjustment_new"
           (* t)
           (a gdouble)
           (b gdouble)
           (c gdouble)
           (d gdouble)
           (e gdouble)
           (f gdouble))
 (defun gtk::adjustment-new (a b c d e f)
   (|gtk_adjustment_new| (coerce a 'double-float) (coerce b 'double-float)
    (coerce c 'double-float) (coerce d 'double-float) (coerce e 'double-float)
    (coerce f 'double-float))))
(progn
 (defalien "gtk_adjustment_changed" void (a (* t)))
 (defun gtk::adjustment-changed (a) (|gtk_adjustment_changed| a)))
(progn
 (defalien "gtk_adjustment_value_changed" void (a (* t)))
 (defun gtk::adjustment-value-changed (a) (|gtk_adjustment_value_changed| a)))
(progn
 (defalien "gtk_adjustment_clamp_page" void (a (* t)) (b gdouble) (c gdouble))
 (defun gtk::adjustment-clamp-page (a b c)
   (|gtk_adjustment_clamp_page| a (coerce b 'double-float)
    (coerce c 'double-float))))
(progn
 (defalien "gtk_adjustment_get_value" gdouble (a (* t)))
 (defun gtk::adjustment-get-value (a) (|gtk_adjustment_get_value| a)))
(progn
 (defalien "gtk_adjustment_set_value" void (a (* t)) (b gdouble))
 (defun gtk::adjustment-set-value (a b)
   (|gtk_adjustment_set_value| a (coerce b 'double-float))))
(progn
 (defalien "gtk_style_get_type" GType)
 (defun gtk::style-get-type () (|gtk_style_get_type|)))
(progn
 (defalien "gtk_style_new" (* t))
 (defun gtk::style-new () (|gtk_style_new|)))
(progn
 (defalien "gtk_style_copy" (* t) (a (* t)))
 (defun gtk::style-copy (a) (|gtk_style_copy| a)))
(progn
 (defalien "gtk_style_attach" (* t) (a (* t)) (b (* t)))
 (defun gtk::style-attach (a b) (|gtk_style_attach| a b)))
(progn
 (defalien "gtk_style_detach" void (a (* t)))
 (defun gtk::style-detach (a) (|gtk_style_detach| a)))
(progn
 (defalien "gtk_style_set_background"
           void
           (a (* t))
           (b (* t))
           (c GtkStateType))
 (defun gtk::style-set-background (a b c) (|gtk_style_set_background| a b c)))
(progn
 (defalien "gtk_style_apply_default_background"
           void
           (a (* t))
           (b (* t))
           (c gboolean)
           (d GtkStateType)
           (e (* t))
           (f gint)
           (g gint)
           (h gint)
           (i gint))
 (defun gtk::style-apply-default-background (a b c d e f g h i)
   (|gtk_style_apply_default_background| a b (if c (if (eq c 0) 0 1) 0) d e f g
    h i)))
(progn
 (defalien "gtk_style_lookup_icon_set" (* t) (a (* t)) (b c-string))
 (defun gtk::style-lookup-icon-set (a b) (|gtk_style_lookup_icon_set| a b)))
(progn
 (defalien "gtk_style_render_icon"
           (* t)
           (a (* t))
           (b (* t))
           (c GtkTextDirection)
           (d GtkStateType)
           (e GtkIconSize)
           (f (* t))
           (g c-string))
 (defun gtk::style-render-icon (a b c d e f g)
   (|gtk_style_render_icon| a b c d e f g)))
(progn
 (defalien "gtk_draw_check"
           void
           (a (* t))
           (b (* t))
           (c GtkStateType)
           (d GtkShadowType)
           (e gint)
           (f gint)
           (g gint)
           (h gint))
 (defun gtk::draw-check (a b c d e f g h) (|gtk_draw_check| a b c d e f g h)))
(progn
 (defalien "gtk_paint_hline"
           void
           (a (* t))
           (b (* t))
           (c GtkStateType)
           (d (* t))
           (e (* t))
           (f c-string)
           (g gint)
           (h gint)
           (i gint))
 (defun gtk::paint-hline (a b c d e f g h i)
   (|gtk_paint_hline| a b c d e f g h i)))
(progn
 (defalien "gtk_paint_vline"
           void
           (a (* t))
           (b (* t))
           (c GtkStateType)
           (d (* t))
           (e (* t))
           (f c-string)
           (g gint)
           (h gint)
           (i gint))
 (defun gtk::paint-vline (a b c d e f g h i)
   (|gtk_paint_vline| a b c d e f g h i)))
(progn
 (defalien "gtk_paint_shadow"
           void
           (a (* t))
           (b (* t))
           (c GtkStateType)
           (d GtkShadowType)
           (e (* t))
           (f (* t))
           (g c-string)
           (h gint)
           (i gint)
           (j gint)
           (k gint))
 (defun gtk::paint-shadow (a b c d e f g h i j k)
   (|gtk_paint_shadow| a b c d e f g h i j k)))
(progn
 (defalien "gtk_paint_polygon"
           void
           (a (* t))
           (b (* t))
           (c GtkStateType)
           (d GtkShadowType)
           (e (* t))
           (f (* t))
           (g c-string)
           (h (* t))
           (i gint)
           (j gboolean))
 (defun gtk::paint-polygon (a b c d e f g h i j)
   (|gtk_paint_polygon| a b c d e f g h i (if j (if (eq j 0) 0 1) 0))))
(progn
 (defalien "gtk_paint_arrow"
           void
           (a (* t))
           (b (* t))
           (c GtkStateType)
           (d GtkShadowType)
           (e (* t))
           (f (* t))
           (g c-string)
           (h GtkArrowType)
           (i gboolean)
           (j gint)
           (k gint)
           (l gint)
           (m gint))
 (defun gtk::paint-arrow (a b c d e f g h i j k l m)
   (|gtk_paint_arrow| a b c d e f g h (if i (if (eq i 0) 0 1) 0) j k l m)))
(progn
 (defalien "gtk_paint_diamond"
           void
           (a (* t))
           (b (* t))
           (c GtkStateType)
           (d GtkShadowType)
           (e (* t))
           (f (* t))
           (g c-string)
           (h gint)
           (i gint)
           (j gint)
           (k gint))
 (defun gtk::paint-diamond (a b c d e f g h i j k)
   (|gtk_paint_diamond| a b c d e f g h i j k)))
(progn
 (defalien "gtk_paint_box"
           void
           (a (* t))
           (b (* t))
           (c GtkStateType)
           (d GtkShadowType)
           (e (* t))
           (f (* t))
           (g c-string)
           (h gint)
           (i gint)
           (j gint)
           (k gint))
 (defun gtk::paint-box (a b c d e f g h i j k)
   (|gtk_paint_box| a b c d e f g h i j k)))
(progn
 (defalien "gtk_paint_flat_box"
           void
           (a (* t))
           (b (* t))
           (c GtkStateType)
           (d GtkShadowType)
           (e (* t))
           (f (* t))
           (g c-string)
           (h gint)
           (i gint)
           (j gint)
           (k gint))
 (defun gtk::paint-flat-box (a b c d e f g h i j k)
   (|gtk_paint_flat_box| a b c d e f g h i j k)))
(progn
 (defalien "gtk_paint_check"
           void
           (a (* t))
           (b (* t))
           (c GtkStateType)
           (d GtkShadowType)
           (e (* t))
           (f (* t))
           (g c-string)
           (h gint)
           (i gint)
           (j gint)
           (k gint))
 (defun gtk::paint-check (a b c d e f g h i j k)
   (|gtk_paint_check| a b c d e f g h i j k)))
(progn
 (defalien "gtk_paint_option"
           void
           (a (* t))
           (b (* t))
           (c GtkStateType)
           (d GtkShadowType)
           (e (* t))
           (f (* t))
           (g c-string)
           (h gint)
           (i gint)
           (j gint)
           (k gint))
 (defun gtk::paint-option (a b c d e f g h i j k)
   (|gtk_paint_option| a b c d e f g h i j k)))
(progn
 (defalien "gtk_paint_tab"
           void
           (a (* t))
           (b (* t))
           (c GtkStateType)
           (d GtkShadowType)
           (e (* t))
           (f (* t))
           (g c-string)
           (h gint)
           (i gint)
           (j gint)
           (k gint))
 (defun gtk::paint-tab (a b c d e f g h i j k)
   (|gtk_paint_tab| a b c d e f g h i j k)))
(progn
 (defalien "gtk_paint_shadow_gap"
           void
           (a (* t))
           (b (* t))
           (c GtkStateType)
           (d GtkShadowType)
           (e (* t))
           (f (* t))
           (g gchar :in-out)
           (h gint)
           (i gint)
           (j gint)
           (k gint)
           (l GtkPositionType)
           (m gint)
           (n gint))
 (defun gtk::paint-shadow-gap (a b c d e f g h i j k l m n)
   (|gtk_paint_shadow_gap| a b c d e f g h i j k l m n)))
(progn
 (defalien "gtk_paint_box_gap"
           void
           (a (* t))
           (b (* t))
           (c GtkStateType)
           (d GtkShadowType)
           (e (* t))
           (f (* t))
           (g gchar :in-out)
           (h gint)
           (i gint)
           (j gint)
           (k gint)
           (l GtkPositionType)
           (m gint)
           (n gint))
 (defun gtk::paint-box-gap (a b c d e f g h i j k l m n)
   (|gtk_paint_box_gap| a b c d e f g h i j k l m n)))
(progn
 (defalien "gtk_paint_extension"
           void
           (a (* t))
           (b (* t))
           (c GtkStateType)
           (d GtkShadowType)
           (e (* t))
           (f (* t))
           (g gchar :in-out)
           (h gint)
           (i gint)
           (j gint)
           (k gint)
           (l GtkPositionType))
 (defun gtk::paint-extension (a b c d e f g h i j k l)
   (|gtk_paint_extension| a b c d e f g h i j k l)))
(progn
 (defalien "gtk_paint_focus"
           void
           (a (* t))
           (b (* t))
           (c GtkStateType)
           (d (* t))
           (e (* t))
           (f c-string)
           (g gint)
           (h gint)
           (i gint)
           (j gint))
 (defun gtk::paint-focus (a b c d e f g h i j)
   (|gtk_paint_focus| a b c d e f g h i j)))
(progn
 (defalien "gtk_paint_slider"
           void
           (a (* t))
           (b (* t))
           (c GtkStateType)
           (d GtkShadowType)
           (e (* t))
           (f (* t))
           (g c-string)
           (h gint)
           (i gint)
           (j gint)
           (k gint)
           (l GtkOrientation))
 (defun gtk::paint-slider (a b c d e f g h i j k l)
   (|gtk_paint_slider| a b c d e f g h i j k l)))
(progn
 (defalien "gtk_paint_handle"
           void
           (a (* t))
           (b (* t))
           (c GtkStateType)
           (d GtkShadowType)
           (e (* t))
           (f (* t))
           (g c-string)
           (h gint)
           (i gint)
           (j gint)
           (k gint)
           (l GtkOrientation))
 (defun gtk::paint-handle (a b c d e f g h i j k l)
   (|gtk_paint_handle| a b c d e f g h i j k l)))
(progn
 (defalien "gtk_paint_expander"
           void
           (a (* t))
           (b (* t))
           (c GtkStateType)
           (d (* t))
           (e (* t))
           (f c-string)
           (g gint)
           (h gint)
           (i GtkExpanderStyle))
 (defun gtk::paint-expander (a b c d e f g h i)
   (|gtk_paint_expander| a b c d e f g h i)))
(progn
 (defalien "gtk_paint_layout"
           void
           (a (* t))
           (b (* t))
           (c GtkStateType)
           (d gboolean)
           (e (* t))
           (f (* t))
           (g c-string)
           (h gint)
           (i gint)
           (j (* t)))
 (defun gtk::paint-layout (a b c d e f g h i j)
   (|gtk_paint_layout| a b c (if d (if (eq d 0) 0 1) 0) e f g h i j)))
(progn
 (defalien "gtk_paint_resize_grip"
           void
           (a (* t))
           (b (* t))
           (c GtkStateType)
           (d (* t))
           (e (* t))
           (f c-string)
           (g GdkWindowEdge)
           (h gint)
           (i gint)
           (j gint)
           (k gint))
 (defun gtk::paint-resize-grip (a b c d e f g h i j k)
   (|gtk_paint_resize_grip| a b c d e f g h i j k)))
(progn
 (defalien "gtk_border_get_type" GType)
 (defun gtk::border-get-type () (|gtk_border_get_type|)))
(progn
 (defalien "gtk_border_copy" (* t) (a (* t)))
 (defun gtk::border-copy (a) (|gtk_border_copy| a)))
(progn
 (defalien "gtk_border_free" void (a (* t)))
 (defun gtk::border-free (a) (|gtk_border_free| a)))
(progn
 (defalien "gtk_rc_add_default_file" void (a c-string))
 (defun gtk::rc-add-default-file (a) (|gtk_rc_add_default_file| a)))
(progn
 (defalien "gtk_rc_set_default_files" void (a (* t)))
 (defun gtk::rc-set-default-files (a) (|gtk_rc_set_default_files| a)))
(progn
 (defalien "gtk_rc_get_default_files" (* t))
 (defun gtk::rc-get-default-files () (|gtk_rc_get_default_files|)))
(progn
 (defalien "gtk_rc_get_style" (* t) (a (* t)))
 (defun gtk::rc-get-style (a) (|gtk_rc_get_style| a)))
(progn
 (defalien "gtk_rc_get_style_by_paths"
           (* t)
           (a (* t))
           (b c-string)
           (c c-string)
           (d GType))
 (defun gtk::rc-get-style-by-paths (a b c d)
   (|gtk_rc_get_style_by_paths| a b c d)))
(progn
 (defalien "gtk_rc_reparse_all_for_settings" gboolean (a (* t)) (b gboolean))
 (defun gtk::rc-reparse-all-for-settings (a b)
   (let ((v282
          (|gtk_rc_reparse_all_for_settings| a (if b (if (eq b 0) 0 1) 0))))
     (if (= v282 1) t nil))))
(progn
 (defalien "gtk_rc_find_pixmap_in_path" gchar (a (* t)) (b (* t)) (c c-string))
 (defun gtk::rc-find-pixmap-in-path (a b c)
   (|gtk_rc_find_pixmap_in_path| a b c)))
(progn
 (defalien "gtk_rc_parse" void (a c-string))
 (defun gtk::rc-parse (a) (|gtk_rc_parse| a)))
(progn
 (defalien "gtk_rc_parse_string" void (a c-string))
 (defun gtk::rc-parse-string (a) (|gtk_rc_parse_string| a)))
(progn
 (defalien "gtk_rc_reparse_all" gboolean)
 (defun gtk::rc-reparse-all ()
   (let ((v283 (|gtk_rc_reparse_all|)))
     (if (= v283 1) t nil))))
(progn
 (defalien "gtk_rc_style_get_type" GType)
 (defun gtk::rc-style-get-type () (|gtk_rc_style_get_type|)))
(progn
 (defalien "gtk_rc_style_new" (* t))
 (defun gtk::rc-style-new () (|gtk_rc_style_new|)))
(progn
 (defalien "gtk_rc_style_copy" (* t) (a (* t)))
 (defun gtk::rc-style-copy (a) (|gtk_rc_style_copy| a)))
(progn
 (defalien "gtk_rc_style_ref" void (a (* t)))
 (defun gtk::rc-style-ref (a) (|gtk_rc_style_ref| a)))
(progn
 (defalien "gtk_rc_style_unref" void (a (* t)))
 (defun gtk::rc-style-unref (a) (|gtk_rc_style_unref| a)))
(progn
 (defalien "gtk_rc_find_module_in_path" gchar (a c-string))
 (defun gtk::rc-find-module-in-path (a) (|gtk_rc_find_module_in_path| a)))
(progn
 (defalien "gtk_rc_get_theme_dir" gchar)
 (defun gtk::rc-get-theme-dir () (|gtk_rc_get_theme_dir|)))
(progn
 (defalien "gtk_rc_get_module_dir" gchar)
 (defun gtk::rc-get-module-dir () (|gtk_rc_get_module_dir|)))
(progn
 (defalien "gtk_rc_get_im_module_path" gchar)
 (defun gtk::rc-get-im-module-path () (|gtk_rc_get_im_module_path|)))
(progn
 (defalien "gtk_rc_get_im_module_file" gchar)
 (defun gtk::rc-get-im-module-file () (|gtk_rc_get_im_module_file|)))
(progn
 (defalien "gtk_rc_scanner_new" (* t))
 (defun gtk::rc-scanner-new () (|gtk_rc_scanner_new|)))
(progn
 (defalien "gtk_rc_parse_color" guint (a (* t)) (b (* t)))
 (defun gtk::rc-parse-color (a b) (|gtk_rc_parse_color| a b)))
(progn
 (defalien "gtk_rc_parse_state" guint (a (* t)) (b GtkStateType :in-out))
 (defun gtk::rc-parse-state (a b) (|gtk_rc_parse_state| a b)))
(progn
 (defalien "gtk_rc_parse_priority"
           guint
           (a (* t))
           (b GtkPathPriorityType :in-out))
 (defun gtk::rc-parse-priority (a b) (|gtk_rc_parse_priority| a b)))
(progn
 (defalien "gtk_settings_get_type" GType)
 (defun gtk::settings-get-type () (|gtk_settings_get_type|)))
(progn
 (defalien "gtk_settings_get_default" (* t))
 (defun gtk::settings-get-default () (|gtk_settings_get_default|)))
(progn
 (defalien "gtk_settings_install_property" void (a (* t)))
 (defun gtk::settings-install-property (a) (|gtk_settings_install_property| a)))
(progn
 (defalien "gtk_settings_install_property_parser"
           void
           (a (* t))
           (b GtkRcPropertyParser))
 (defun gtk::settings-install-property-parser (a b)
   (|gtk_settings_install_property_parser| a b)))
(progn
 (defalien "gtk_rc_property_parse_color"
           gboolean
           (a (* t))
           (b (* t))
           (c (* t)))
 (defun gtk::rc-property-parse-color (a b c)
   (let ((v284 (|gtk_rc_property_parse_color| a b c)))
     (if (= v284 1) t nil))))
(progn
 (defalien "gtk_rc_property_parse_enum" gboolean (a (* t)) (b (* t)) (c (* t)))
 (defun gtk::rc-property-parse-enum (a b c)
   (let ((v285 (|gtk_rc_property_parse_enum| a b c)))
     (if (= v285 1) t nil))))
(progn
 (defalien "gtk_rc_property_parse_flags"
           gboolean
           (a (* t))
           (b (* t))
           (c (* t)))
 (defun gtk::rc-property-parse-flags (a b c)
   (let ((v286 (|gtk_rc_property_parse_flags| a b c)))
     (if (= v286 1) t nil))))
(progn
 (defalien "gtk_rc_property_parse_requisition"
           gboolean
           (a (* t))
           (b (* t))
           (c (* t)))
 (defun gtk::rc-property-parse-requisition (a b c)
   (let ((v287 (|gtk_rc_property_parse_requisition| a b c)))
     (if (= v287 1) t nil))))
(progn
 (defalien "gtk_rc_property_parse_border"
           gboolean
           (a (* t))
           (b (* t))
           (c (* t)))
 (defun gtk::rc-property-parse-border (a b c)
   (let ((v288 (|gtk_rc_property_parse_border| a b c)))
     (if (= v288 1) t nil))))
(progn
 (defalien "gtk_settings_set_property_value"
           void
           (a (* t))
           (b c-string)
           (c (* t)))
 (defun gtk::settings-set-property-value (a b c)
   (|gtk_settings_set_property_value| a b c)))
(progn
 (defalien "gtk_settings_set_string_property"
           void
           (a (* t))
           (b c-string)
           (c c-string)
           (d c-string))
 (defun gtk::settings-set-string-property (a b c d)
   (|gtk_settings_set_string_property| a b c d)))
(progn
 (defalien "gtk_settings_set_long_property"
           void
           (a (* t))
           (b c-string)
           (c glong)
           (d c-string))
 (defun gtk::settings-set-long-property (a b c d)
   (|gtk_settings_set_long_property| a b c d)))
(progn
 (defalien "gtk_settings_set_double_property"
           void
           (a (* t))
           (b c-string)
           (c gdouble)
           (d c-string))
 (defun gtk::settings-set-double-property (a b c d)
   (|gtk_settings_set_double_property| a b (coerce c 'double-float) d)))
(progn
 (defalien "atk_state_type_get_name" c-string (a AtkStateType))
 (defun atk::state-type-get-name (a) (|atk_state_type_get_name| a)))
(progn
 (defalien "atk_state_type_for_name" AtkStateType (a c-string))
 (defun atk::state-type-for-name (a) (|atk_state_type_for_name| a)))
(progn
 (defalien "atk_object_get_type" GType)
 (defun atk::object-get-type () (|atk_object_get_type|)))
(progn
 (defalien "atk_implementor_get_type" GType)
 (defun atk::implementor-get-type () (|atk_implementor_get_type|)))
(progn
 (defalien "atk_implementor_ref_accessible" (* t) (a (* t)))
 (defun atk::implementor-ref-accessible (a)
   (|atk_implementor_ref_accessible| a)))
(progn
 (defalien "atk_object_get_name" c-string (a (* t)))
 (defun atk::object-get-name (a) (|atk_object_get_name| a)))
(progn
 (defalien "atk_object_get_description" c-string (a (* t)))
 (defun atk::object-get-description (a) (|atk_object_get_description| a)))
(progn
 (defalien "atk_object_get_parent" (* t) (a (* t)))
 (defun atk::object-get-parent (a) (|atk_object_get_parent| a)))
(progn
 (defalien "atk_object_get_n_accessible_children" gint (a (* t)))
 (defun atk::object-get-n-accessible-children (a)
   (|atk_object_get_n_accessible_children| a)))
(progn
 (defalien "atk_object_ref_accessible_child" (* t) (a (* t)) (b gint))
 (defun atk::object-ref-accessible-child (a b)
   (|atk_object_ref_accessible_child| a b)))
(progn
 (defalien "atk_object_ref_relation_set" (* t) (a (* t)))
 (defun atk::object-ref-relation-set (a) (|atk_object_ref_relation_set| a)))
(progn
 (defalien "atk_object_get_role" AtkRole (a (* t)))
 (defun atk::object-get-role (a) (|atk_object_get_role| a)))
(progn
 (defalien "atk_object_get_layer" AtkLayer (a (* t)))
 (defun atk::object-get-layer (a) (|atk_object_get_layer| a)))
(progn
 (defalien "atk_object_get_mdi_zorder" gint (a (* t)))
 (defun atk::object-get-mdi-zorder (a) (|atk_object_get_mdi_zorder| a)))
(progn
 (defalien "atk_object_ref_state_set" (* t) (a (* t)))
 (defun atk::object-ref-state-set (a) (|atk_object_ref_state_set| a)))
(progn
 (defalien "atk_object_get_index_in_parent" gint (a (* t)))
 (defun atk::object-get-index-in-parent (a)
   (|atk_object_get_index_in_parent| a)))
(progn
 (defalien "atk_object_set_name" void (a (* t)) (b c-string))
 (defun atk::object-set-name (a b) (|atk_object_set_name| a b)))
(progn
 (defalien "atk_object_set_description" void (a (* t)) (b c-string))
 (defun atk::object-set-description (a b) (|atk_object_set_description| a b)))
(progn
 (defalien "atk_object_set_parent" void (a (* t)) (b (* t)))
 (defun atk::object-set-parent (a b) (|atk_object_set_parent| a b)))
(progn
 (defalien "atk_object_set_role" void (a (* t)) (b AtkRole))
 (defun atk::object-set-role (a b) (|atk_object_set_role| a b)))
(progn
 (defalien "atk_object_connect_property_change_handler"
           guint
           (a (* t))
           (b (* t)))
 (defun atk::object-connect-property-change-handler (a b)
   (|atk_object_connect_property_change_handler| a b)))
(progn
 (defalien "atk_object_remove_property_change_handler"
           void
           (a (* t))
           (b guint))
 (defun atk::object-remove-property-change-handler (a b)
   (|atk_object_remove_property_change_handler| a b)))
(progn
 (defalien "atk_object_notify_state_change"
           void
           (a (* t))
           (b AtkState)
           (c gboolean))
 (defun atk::object-notify-state-change (a b c)
   (|atk_object_notify_state_change| a b (if c (if (eq c 0) 0 1) 0))))
(progn
 (defalien "atk_role_get_name" c-string (a AtkRole))
 (defun atk::role-get-name (a) (|atk_role_get_name| a)))
(progn
 (defalien "atk_role_for_name" AtkRole (a c-string))
 (defun atk::role-for-name (a) (|atk_role_for_name| a)))
(progn
 (defalien "gtk_widget_get_type" GType)
 (defun gtk::widget-get-type () (|gtk_widget_get_type|)))
(progn
 (defalien "gtk_widget_new" (* t) (a GType) (b c-string))
 (defun gtk::widget-new (a b) (|gtk_widget_new| a b)))
(progn
 (defalien "gtk_widget_ref" (* t) (a (* t)))
 (defun gtk::widget-ref (a) (|gtk_widget_ref| a)))
(progn
 (defalien "gtk_widget_unref" void (a (* t)))
 (defun gtk::widget-unref (a) (|gtk_widget_unref| a)))
(progn
 (defalien "gtk_widget_destroy" void (a (* t)))
 (defun gtk::widget-destroy (a) (|gtk_widget_destroy| a)))
(progn
 (defalien "gtk_widget_destroyed" void (a (* t)) (b (* t)))
 (defun gtk::widget-destroyed (a b) (|gtk_widget_destroyed| a b)))
(progn
 (defalien "gtk_widget_set" void (a (* t)) (b c-string))
 (defun gtk::widget-set (a b) (|gtk_widget_set| a b)))
(progn
 (defalien "gtk_widget_unparent" void (a (* t)))
 (defun gtk::widget-unparent (a) (|gtk_widget_unparent| a)))
(progn
 (defalien "gtk_widget_show" void (a (* t)))
 (defun gtk::widget-show (a) (|gtk_widget_show| a)))
(progn
 (defalien "gtk_widget_show_now" void (a (* t)))
 (defun gtk::widget-show-now (a) (|gtk_widget_show_now| a)))
(progn
 (defalien "gtk_widget_hide" void (a (* t)))
 (defun gtk::widget-hide (a) (|gtk_widget_hide| a)))
(progn
 (defalien "gtk_widget_show_all" void (a (* t)))
 (defun gtk::widget-show-all (a) (|gtk_widget_show_all| a)))
(progn
 (defalien "gtk_widget_hide_all" void (a (* t)))
 (defun gtk::widget-hide-all (a) (|gtk_widget_hide_all| a)))
(progn
 (defalien "gtk_widget_map" void (a (* t)))
 (defun gtk::widget-map (a) (|gtk_widget_map| a)))
(progn
 (defalien "gtk_widget_unmap" void (a (* t)))
 (defun gtk::widget-unmap (a) (|gtk_widget_unmap| a)))
(progn
 (defalien "gtk_widget_realize" void (a (* t)))
 (defun gtk::widget-realize (a) (|gtk_widget_realize| a)))
(progn
 (defalien "gtk_widget_unrealize" void (a (* t)))
 (defun gtk::widget-unrealize (a) (|gtk_widget_unrealize| a)))
(progn
 (defalien "gtk_widget_queue_draw" void (a (* t)))
 (defun gtk::widget-queue-draw (a) (|gtk_widget_queue_draw| a)))
(progn
 (defalien "gtk_widget_queue_draw_area"
           void
           (a (* t))
           (b gint)
           (c gint)
           (d gint)
           (e gint))
 (defun gtk::widget-queue-draw-area (a b c d e)
   (|gtk_widget_queue_draw_area| a b c d e)))
(progn
 (defalien "gtk_widget_queue_resize" void (a (* t)))
 (defun gtk::widget-queue-resize (a) (|gtk_widget_queue_resize| a)))
(progn
 (defalien "gtk_widget_size_request" void (a (* t)) (b (* t)))
 (defun gtk::widget-size-request (a b) (|gtk_widget_size_request| a b)))
(progn
 (defalien "gtk_widget_size_allocate" void (a (* t)) (b (* t)))
 (defun gtk::widget-size-allocate (a b) (|gtk_widget_size_allocate| a b)))
(progn
 (defalien "gtk_widget_get_child_requisition" void (a (* t)) (b (* t)))
 (defun gtk::widget-get-child-requisition (a b)
   (|gtk_widget_get_child_requisition| a b)))
(progn
 (defalien "gtk_widget_add_accelerator"
           void
           (a (* t))
           (b c-string)
           (c (* t))
           (d guint)
           (e GdkModifierType)
           (f GtkAccelFlags))
 (defun gtk::widget-add-accelerator (a b c d e f)
   (|gtk_widget_add_accelerator| a b c d e f)))
(progn
 (defalien "gtk_widget_remove_accelerator"
           gboolean
           (a (* t))
           (b (* t))
           (c guint)
           (d GdkModifierType))
 (defun gtk::widget-remove-accelerator (a b c d)
   (let ((v289 (|gtk_widget_remove_accelerator| a b c d)))
     (if (= v289 1) t nil))))
(progn
 (defalien "gtk_widget_set_accel_path" void (a (* t)) (b c-string) (c (* t)))
 (defun gtk::widget-set-accel-path (a b c) (|gtk_widget_set_accel_path| a b c)))
(progn
 (defalien "gtk_widget_list_accel_closures" (* t) (a (* t)))
 (defun gtk::widget-list-accel-closures (a)
   (|gtk_widget_list_accel_closures| a)))
(progn
 (defalien "gtk_widget_mnemonic_activate" gboolean (a (* t)) (b gboolean))
 (defun gtk::widget-mnemonic-activate (a b)
   (let ((v290 (|gtk_widget_mnemonic_activate| a (if b (if (eq b 0) 0 1) 0))))
     (if (= v290 1) t nil))))
(progn
 (defalien "gtk_widget_event" gboolean (a (* t)) (b (* t)))
 (defun gtk::widget-event (a b)
   (let ((v291 (|gtk_widget_event| a b)))
     (if (= v291 1) t nil))))
(progn
 (defalien "gtk_widget_send_expose" gint (a (* t)) (b (* t)))
 (defun gtk::widget-send-expose (a b) (|gtk_widget_send_expose| a b)))
(progn
 (defalien "gtk_widget_activate" gboolean (a (* t)))
 (defun gtk::widget-activate (a)
   (let ((v292 (|gtk_widget_activate| a)))
     (if (= v292 1) t nil))))
(progn
 (defalien "gtk_widget_set_scroll_adjustments"
           gboolean
           (a (* t))
           (b (* t))
           (c (* t)))
 (defun gtk::widget-set-scroll-adjustments (a b c)
   (let ((v293 (|gtk_widget_set_scroll_adjustments| a b c)))
     (if (= v293 1) t nil))))
(progn
 (defalien "gtk_widget_reparent" void (a (* t)) (b (* t)))
 (defun gtk::widget-reparent (a b) (|gtk_widget_reparent| a b)))
(progn
 (defalien "gtk_widget_intersect" gboolean (a (* t)) (b (* t)) (c (* t)))
 (defun gtk::widget-intersect (a b c)
   (let ((v294 (|gtk_widget_intersect| a b c)))
     (if (= v294 1) t nil))))
(progn
 (defalien "gtk_widget_region_intersect" (* t) (a (* t)) (b (* t)))
 (defun gtk::widget-region-intersect (a b) (|gtk_widget_region_intersect| a b)))
(progn
 (defalien "gtk_widget_freeze_child_notify" void (a (* t)))
 (defun gtk::widget-freeze-child-notify (a)
   (|gtk_widget_freeze_child_notify| a)))
(progn
 (defalien "gtk_widget_child_notify" void (a (* t)) (b c-string))
 (defun gtk::widget-child-notify (a b) (|gtk_widget_child_notify| a b)))
(progn
 (defalien "gtk_widget_thaw_child_notify" void (a (* t)))
 (defun gtk::widget-thaw-child-notify (a) (|gtk_widget_thaw_child_notify| a)))
(progn
 (defalien "gtk_widget_is_focus" gboolean (a (* t)))
 (defun gtk::widget-is-focus (a)
   (let ((v295 (|gtk_widget_is_focus| a)))
     (if (= v295 1) t nil))))
(progn
 (defalien "gtk_widget_grab_focus" void (a (* t)))
 (defun gtk::widget-grab-focus (a) (|gtk_widget_grab_focus| a)))
(progn
 (defalien "gtk_widget_grab_default" void (a (* t)))
 (defun gtk::widget-grab-default (a) (|gtk_widget_grab_default| a)))
(progn
 (defalien "gtk_widget_set_name" void (a (* t)) (b c-string))
 (defun gtk::widget-set-name (a b) (|gtk_widget_set_name| a b)))
(progn
 (defalien "gtk_widget_get_name" c-string (a (* t)))
 (defun gtk::widget-get-name (a) (|gtk_widget_get_name| a)))
(progn
 (defalien "gtk_widget_set_state" void (a (* t)) (b GtkStateType))
 (defun gtk::widget-set-state (a b) (|gtk_widget_set_state| a b)))
(progn
 (defalien "gtk_widget_set_sensitive" void (a (* t)) (b gboolean))
 (defun gtk::widget-set-sensitive (a b)
   (|gtk_widget_set_sensitive| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gtk_widget_set_app_paintable" void (a (* t)) (b gboolean))
 (defun gtk::widget-set-app-paintable (a b)
   (|gtk_widget_set_app_paintable| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gtk_widget_set_double_buffered" void (a (* t)) (b gboolean))
 (defun gtk::widget-set-double-buffered (a b)
   (|gtk_widget_set_double_buffered| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gtk_widget_set_redraw_on_allocate" void (a (* t)) (b gboolean))
 (defun gtk::widget-set-redraw-on-allocate (a b)
   (|gtk_widget_set_redraw_on_allocate| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gtk_widget_set_parent" void (a (* t)) (b (* t)))
 (defun gtk::widget-set-parent (a b) (|gtk_widget_set_parent| a b)))
(progn
 (defalien "gtk_widget_set_parent_window" void (a (* t)) (b (* t)))
 (defun gtk::widget-set-parent-window (a b)
   (|gtk_widget_set_parent_window| a b)))
(progn
 (defalien "gtk_widget_set_child_visible" void (a (* t)) (b gboolean))
 (defun gtk::widget-set-child-visible (a b)
   (|gtk_widget_set_child_visible| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gtk_widget_get_child_visible" gboolean (a (* t)))
 (defun gtk::widget-get-child-visible (a)
   (let ((v296 (|gtk_widget_get_child_visible| a)))
     (if (= v296 1) t nil))))
(progn
 (defalien "gtk_widget_get_parent" (* t) (a (* t)))
 (defun gtk::widget-get-parent (a) (|gtk_widget_get_parent| a)))
(progn
 (defalien "gtk_widget_get_parent_window" (* t) (a (* t)))
 (defun gtk::widget-get-parent-window (a) (|gtk_widget_get_parent_window| a)))
(progn
 (defalien "gtk_widget_child_focus" gboolean (a (* t)) (b GtkDirectionType))
 (defun gtk::widget-child-focus (a b)
   (let ((v297 (|gtk_widget_child_focus| a b)))
     (if (= v297 1) t nil))))
(progn
 (defalien "gtk_widget_set_size_request" void (a (* t)) (b gint) (c gint))
 (defun gtk::widget-set-size-request (a b c)
   (|gtk_widget_set_size_request| a b c)))
(progn
 (defalien "gtk_widget_get_size_request"
           void
           (a (* t))
           (b gint :in-out)
           (c gint :in-out))
 (defun gtk::widget-get-size-request (a b c)
   (|gtk_widget_get_size_request| a b c)))
(progn
 (defalien "gtk_widget_set_events" void (a (* t)) (b gint))
 (defun gtk::widget-set-events (a b) (|gtk_widget_set_events| a b)))
(progn
 (defalien "gtk_widget_add_events" void (a (* t)) (b gint))
 (defun gtk::widget-add-events (a b) (|gtk_widget_add_events| a b)))
(progn
 (defalien "gtk_widget_set_extension_events"
           void
           (a (* t))
           (b GdkExtensionMode))
 (defun gtk::widget-set-extension-events (a b)
   (|gtk_widget_set_extension_events| a b)))
(progn
 (defalien "gtk_widget_get_extension_events" GdkExtensionMode (a (* t)))
 (defun gtk::widget-get-extension-events (a)
   (|gtk_widget_get_extension_events| a)))
(progn
 (defalien "gtk_widget_get_toplevel" (* t) (a (* t)))
 (defun gtk::widget-get-toplevel (a) (|gtk_widget_get_toplevel| a)))
(progn
 (defalien "gtk_widget_get_ancestor" (* t) (a (* t)) (b GType))
 (defun gtk::widget-get-ancestor (a b) (|gtk_widget_get_ancestor| a b)))
(progn
 (defalien "gtk_widget_get_colormap" (* t) (a (* t)))
 (defun gtk::widget-get-colormap (a) (|gtk_widget_get_colormap| a)))
(progn
 (defalien "gtk_widget_get_visual" (* t) (a (* t)))
 (defun gtk::widget-get-visual (a) (|gtk_widget_get_visual| a)))
(progn
 (defalien "gtk_widget_get_settings" (* t) (a (* t)))
 (defun gtk::widget-get-settings (a) (|gtk_widget_get_settings| a)))
(progn
 (defalien "gtk_widget_get_accessible" (* t) (a (* t)))
 (defun gtk::widget-get-accessible (a) (|gtk_widget_get_accessible| a)))
(progn
 (defalien "gtk_widget_set_colormap" void (a (* t)) (b (* t)))
 (defun gtk::widget-set-colormap (a b) (|gtk_widget_set_colormap| a b)))
(progn
 (defalien "gtk_widget_get_events" gint (a (* t)))
 (defun gtk::widget-get-events (a) (|gtk_widget_get_events| a)))
(progn
 (defalien "gtk_widget_get_pointer"
           void
           (a (* t))
           (b gint :in-out)
           (c gint :in-out))
 (defun gtk::widget-get-pointer (a b c) (|gtk_widget_get_pointer| a b c)))
(progn
 (defalien "gtk_widget_is_ancestor" gboolean (a (* t)) (b (* t)))
 (defun gtk::widget-is-ancestor (a b)
   (let ((v298 (|gtk_widget_is_ancestor| a b)))
     (if (= v298 1) t nil))))
(progn
 (defalien "gtk_widget_translate_coordinates"
           gboolean
           (a (* t))
           (b (* t))
           (c gint)
           (d gint)
           (e gint :in-out)
           (f gint :in-out))
 (defun gtk::widget-translate-coordinates (a b c d e f)
   (let ((v299
          (multiple-value-list
           (|gtk_widget_translate_coordinates| a b c d e f))))
     (apply #'values (if (= 1 (car v299)) t nil) (cdr v299)))))
(progn
 (defalien "gtk_widget_hide_on_delete" gboolean (a (* t)))
 (defun gtk::widget-hide-on-delete (a)
   (let ((v300 (|gtk_widget_hide_on_delete| a)))
     (if (= v300 1) t nil))))
(progn
 (defalien "gtk_widget_set_style" void (a (* t)) (b (* t)))
 (defun gtk::widget-set-style (a b) (|gtk_widget_set_style| a b)))
(progn
 (defalien "gtk_widget_ensure_style" void (a (* t)))
 (defun gtk::widget-ensure-style (a) (|gtk_widget_ensure_style| a)))
(progn
 (defalien "gtk_widget_get_style" (* t) (a (* t)))
 (defun gtk::widget-get-style (a) (|gtk_widget_get_style| a)))
(progn
 (defalien "gtk_widget_modify_style" void (a (* t)) (b (* t)))
 (defun gtk::widget-modify-style (a b) (|gtk_widget_modify_style| a b)))
(progn
 (defalien "gtk_widget_get_modifier_style" (* t) (a (* t)))
 (defun gtk::widget-get-modifier-style (a) (|gtk_widget_get_modifier_style| a)))
(progn
 (defalien "gtk_widget_modify_fg" void (a (* t)) (b GtkStateType) (c (* t)))
 (defun gtk::widget-modify-fg (a b c) (|gtk_widget_modify_fg| a b c)))
(progn
 (defalien "gtk_widget_modify_bg" void (a (* t)) (b GtkStateType) (c (* t)))
 (defun gtk::widget-modify-bg (a b c) (|gtk_widget_modify_bg| a b c)))
(progn
 (defalien "gtk_widget_modify_text" void (a (* t)) (b GtkStateType) (c (* t)))
 (defun gtk::widget-modify-text (a b c) (|gtk_widget_modify_text| a b c)))
(progn
 (defalien "gtk_widget_modify_base" void (a (* t)) (b GtkStateType) (c (* t)))
 (defun gtk::widget-modify-base (a b c) (|gtk_widget_modify_base| a b c)))
(progn
 (defalien "gtk_widget_modify_font" void (a (* t)) (b (* t)))
 (defun gtk::widget-modify-font (a b) (|gtk_widget_modify_font| a b)))
(progn
 (defalien "gtk_widget_create_pango_context" (* t) (a (* t)))
 (defun gtk::widget-create-pango-context (a)
   (|gtk_widget_create_pango_context| a)))
(progn
 (defalien "gtk_widget_get_pango_context" (* t) (a (* t)))
 (defun gtk::widget-get-pango-context (a) (|gtk_widget_get_pango_context| a)))
(progn
 (defalien "gtk_widget_create_pango_layout" (* t) (a (* t)) (b c-string))
 (defun gtk::widget-create-pango-layout (a b)
   (|gtk_widget_create_pango_layout| a b)))
(progn
 (defalien "gtk_widget_render_icon"
           (* t)
           (a (* t))
           (b c-string)
           (c GtkIconSize)
           (d c-string))
 (defun gtk::widget-render-icon (a b c d) (|gtk_widget_render_icon| a b c d)))
(progn
 (defalien "gtk_widget_set_composite_name" void (a (* t)) (b c-string))
 (defun gtk::widget-set-composite-name (a b)
   (|gtk_widget_set_composite_name| a b)))
(progn
 (defalien "gtk_widget_get_composite_name" gchar (a (* t)))
 (defun gtk::widget-get-composite-name (a) (|gtk_widget_get_composite_name| a)))
(progn
 (defalien "gtk_widget_reset_rc_styles" void (a (* t)))
 (defun gtk::widget-reset-rc-styles (a) (|gtk_widget_reset_rc_styles| a)))
(progn
 (defalien "gtk_widget_push_colormap" void (a (* t)))
 (defun gtk::widget-push-colormap (a) (|gtk_widget_push_colormap| a)))
(progn
 (defalien "gtk_widget_push_composite_child" void)
 (defun gtk::widget-push-composite-child ()
   (|gtk_widget_push_composite_child|)))
(progn
 (defalien "gtk_widget_pop_composite_child" void)
 (defun gtk::widget-pop-composite-child () (|gtk_widget_pop_composite_child|)))
(progn
 (defalien "gtk_widget_pop_colormap" void)
 (defun gtk::widget-pop-colormap () (|gtk_widget_pop_colormap|)))
(progn
 (defalien "gtk_widget_class_install_style_property" void (a (* t)) (b (* t)))
 (defun gtk::widget-class-install-style-property (a b)
   (|gtk_widget_class_install_style_property| a b)))
(progn
 (defalien "gtk_widget_class_install_style_property_parser"
           void
           (a (* t))
           (b (* t))
           (c GtkRcPropertyParser))
 (defun gtk::widget-class-install-style-property-parser (a b c)
   (|gtk_widget_class_install_style_property_parser| a b c)))
(progn
 (defalien "gtk_widget_style_get_property"
           void
           (a (* t))
           (b c-string)
           (c (* t)))
 (defun gtk::widget-style-get-property (a b c)
   (|gtk_widget_style_get_property| a b c)))
(progn
 (defalien "gtk_widget_style_get_valist"
           void
           (a (* t))
           (b c-string)
           (c va_list))
 (defun gtk::widget-style-get-valist (a b c)
   (|gtk_widget_style_get_valist| a b c)))
(progn
 (defalien "gtk_widget_style_get" void (a (* t)) (b c-string))
 (defun gtk::widget-style-get (a b) (|gtk_widget_style_get| a b)))
(progn
 (defalien "gtk_widget_set_default_colormap" void (a (* t)))
 (defun gtk::widget-set-default-colormap (a)
   (|gtk_widget_set_default_colormap| a)))
(progn
 (defalien "gtk_widget_get_default_style" (* t))
 (defun gtk::widget-get-default-style () (|gtk_widget_get_default_style|)))
(progn
 (defalien "gtk_widget_get_default_colormap" (* t))
 (defun gtk::widget-get-default-colormap ()
   (|gtk_widget_get_default_colormap|)))
(progn
 (defalien "gtk_widget_get_default_visual" (* t))
 (defun gtk::widget-get-default-visual () (|gtk_widget_get_default_visual|)))
(progn
 (defalien "gtk_widget_set_direction" void (a (* t)) (b GtkTextDirection))
 (defun gtk::widget-set-direction (a b) (|gtk_widget_set_direction| a b)))
(progn
 (defalien "gtk_widget_get_direction" GtkTextDirection (a (* t)))
 (defun gtk::widget-get-direction (a) (|gtk_widget_get_direction| a)))
(progn
 (defalien "gtk_widget_set_default_direction" void (a GtkTextDirection))
 (defun gtk::widget-set-default-direction (a)
   (|gtk_widget_set_default_direction| a)))
(progn
 (defalien "gtk_widget_get_default_direction" GtkTextDirection)
 (defun gtk::widget-get-default-direction ()
   (|gtk_widget_get_default_direction|)))
(progn
 (defalien "gtk_widget_shape_combine_mask"
           void
           (a (* t))
           (b (* t))
           (c gint)
           (d gint))
 (defun gtk::widget-shape-combine-mask (a b c d)
   (|gtk_widget_shape_combine_mask| a b c d)))
(progn
 (defalien "gtk_widget_reset_shapes" void (a (* t)))
 (defun gtk::widget-reset-shapes (a) (|gtk_widget_reset_shapes| a)))
(progn
 (defalien "gtk_widget_path"
           void
           (a (* t))
           (b guint :in-out)
           (c (* t))
           (d (* t)))
 (defun gtk::widget-path (a b c d) (|gtk_widget_path| a b c d)))
(progn
 (defalien "gtk_widget_class_path"
           void
           (a (* t))
           (b guint :in-out)
           (c (* t))
           (d (* t)))
 (defun gtk::widget-class-path (a b c d) (|gtk_widget_class_path| a b c d)))
(progn
 (defalien "gtk_requisition_get_type" GType)
 (defun gtk::requisition-get-type () (|gtk_requisition_get_type|)))
(progn
 (defalien "gtk_requisition_copy" (* t) (a (* t)))
 (defun gtk::requisition-copy (a) (|gtk_requisition_copy| a)))
(progn
 (defalien "gtk_requisition_free" void (a (* t)))
 (defun gtk::requisition-free (a) (|gtk_requisition_free| a)))
(progn
 (defalien "gtk_misc_get_type" GType)
 (defun gtk::misc-get-type () (|gtk_misc_get_type|)))
(progn
 (defalien "gtk_misc_set_alignment" void (a (* t)) (b gfloat) (c gfloat))
 (defun gtk::misc-set-alignment (a b c)
   (|gtk_misc_set_alignment| a (coerce b 'single-float)
    (coerce c 'single-float))))
(progn
 (defalien "gtk_misc_get_alignment"
           void
           (a (* t))
           (b gfloat :in-out)
           (c gfloat :in-out))
 (defun gtk::misc-get-alignment (a b c)
   (|gtk_misc_get_alignment| a (coerce b 'single-float)
    (coerce c 'single-float))))
(progn
 (defalien "gtk_misc_set_padding" void (a (* t)) (b gint) (c gint))
 (defun gtk::misc-set-padding (a b c) (|gtk_misc_set_padding| a b c)))
(progn
 (defalien "gtk_misc_get_padding"
           void
           (a (* t))
           (b gint :in-out)
           (c gint :in-out))
 (defun gtk::misc-get-padding (a b c) (|gtk_misc_get_padding| a b c)))
(progn
 (defalien "gtk_container_get_type" GType)
 (defun gtk::container-get-type () (|gtk_container_get_type|)))
(progn
 (defalien "gtk_container_set_border_width" void (a (* t)) (b guint))
 (defun gtk::container-set-border-width (a b)
   (|gtk_container_set_border_width| a b)))
(progn
 (defalien "gtk_container_get_border_width" guint (a (* t)))
 (defun gtk::container-get-border-width (a)
   (|gtk_container_get_border_width| a)))
(progn
 (defalien "gtk_container_add" void (a (* t)) (b (* t)))
 (defun gtk::container-add (a b) (|gtk_container_add| a b)))
(progn
 (defalien "gtk_container_remove" void (a (* t)) (b (* t)))
 (defun gtk::container-remove (a b) (|gtk_container_remove| a b)))
(progn
 (defalien "gtk_container_set_resize_mode" void (a (* t)) (b GtkResizeMode))
 (defun gtk::container-set-resize-mode (a b)
   (|gtk_container_set_resize_mode| a b)))
(progn
 (defalien "gtk_container_get_resize_mode" GtkResizeMode (a (* t)))
 (defun gtk::container-get-resize-mode (a) (|gtk_container_get_resize_mode| a)))
(progn
 (defalien "gtk_container_check_resize" void (a (* t)))
 (defun gtk::container-check-resize (a) (|gtk_container_check_resize| a)))
(progn
 (defalien "gtk_container_foreach" void (a (* t)) (b GtkCallback) (c gpointer))
 (defun gtk::container-foreach (a b c) (|gtk_container_foreach| a b c)))
(progn
 (defalien "gtk_container_get_children" (* t) (a (* t)))
 (defun gtk::container-get-children (a) (|gtk_container_get_children| a)))
(progn
 (defalien "gtk_container_propagate_expose" void (a (* t)) (b (* t)) (c (* t)))
 (defun gtk::container-propagate-expose (a b c)
   (|gtk_container_propagate_expose| a b c)))
(progn
 (defalien "gtk_container_set_focus_chain" void (a (* t)) (b (* t)))
 (defun gtk::container-set-focus-chain (a b)
   (|gtk_container_set_focus_chain| a b)))
(progn
 (defalien "gtk_container_get_focus_chain" gboolean (a (* t)) (b (* t)))
 (defun gtk::container-get-focus-chain (a b)
   (let ((v301 (|gtk_container_get_focus_chain| a b)))
     (if (= v301 1) t nil))))
(progn
 (defalien "gtk_container_unset_focus_chain" void (a (* t)))
 (defun gtk::container-unset-focus-chain (a)
   (|gtk_container_unset_focus_chain| a)))
(progn
 (defalien "gtk_container_set_reallocate_redraws" void (a (* t)) (b gboolean))
 (defun gtk::container-set-reallocate-redraws (a b)
   (|gtk_container_set_reallocate_redraws| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gtk_container_set_focus_child" void (a (* t)) (b (* t)))
 (defun gtk::container-set-focus-child (a b)
   (|gtk_container_set_focus_child| a b)))
(progn
 (defalien "gtk_container_set_focus_vadjustment" void (a (* t)) (b (* t)))
 (defun gtk::container-set-focus-vadjustment (a b)
   (|gtk_container_set_focus_vadjustment| a b)))
(progn
 (defalien "gtk_container_get_focus_vadjustment" (* t) (a (* t)))
 (defun gtk::container-get-focus-vadjustment (a)
   (|gtk_container_get_focus_vadjustment| a)))
(progn
 (defalien "gtk_container_set_focus_hadjustment" void (a (* t)) (b (* t)))
 (defun gtk::container-set-focus-hadjustment (a b)
   (|gtk_container_set_focus_hadjustment| a b)))
(progn
 (defalien "gtk_container_get_focus_hadjustment" (* t) (a (* t)))
 (defun gtk::container-get-focus-hadjustment (a)
   (|gtk_container_get_focus_hadjustment| a)))
(progn
 (defalien "gtk_container_resize_children" void (a (* t)))
 (defun gtk::container-resize-children (a) (|gtk_container_resize_children| a)))
(progn
 (defalien "gtk_container_child_type" GType (a (* t)))
 (defun gtk::container-child-type (a) (|gtk_container_child_type| a)))
(progn
 (defalien "gtk_container_class_install_child_property"
           void
           (a (* t))
           (b guint)
           (c (* t)))
 (defun gtk::container-class-install-child-property (a b c)
   (|gtk_container_class_install_child_property| a b c)))
(progn
 (defalien "gtk_container_class_find_child_property"
           (* t)
           (a (* t))
           (b c-string))
 (defun gtk::container-class-find-child-property (a b)
   (|gtk_container_class_find_child_property| a b)))
(progn
 (defalien "gtk_container_class_list_child_properties"
           (* t)
           (a (* t))
           (b guint :in-out))
 (defun gtk::container-class-list-child-properties (a b)
   (|gtk_container_class_list_child_properties| a b)))
(progn
 (defalien "gtk_container_add_with_properties"
           void
           (a (* t))
           (b (* t))
           (c c-string))
 (defun gtk::container-add-with-properties (a b c)
   (|gtk_container_add_with_properties| a b c)))
(progn
 (defalien "gtk_container_child_set" void (a (* t)) (b (* t)) (c c-string))
 (defun gtk::container-child-set (a b c) (|gtk_container_child_set| a b c)))
(progn
 (defalien "gtk_container_child_get" void (a (* t)) (b (* t)) (c c-string))
 (defun gtk::container-child-get (a b c) (|gtk_container_child_get| a b c)))
(progn
 (defalien "gtk_container_child_set_valist"
           void
           (a (* t))
           (b (* t))
           (c c-string)
           (d va_list))
 (defun gtk::container-child-set-valist (a b c d)
   (|gtk_container_child_set_valist| a b c d)))
(progn
 (defalien "gtk_container_child_get_valist"
           void
           (a (* t))
           (b (* t))
           (c c-string)
           (d va_list))
 (defun gtk::container-child-get-valist (a b c d)
   (|gtk_container_child_get_valist| a b c d)))
(progn
 (defalien "gtk_container_child_set_property"
           void
           (a (* t))
           (b (* t))
           (c c-string)
           (d (* t)))
 (defun gtk::container-child-set-property (a b c d)
   (|gtk_container_child_set_property| a b c d)))
(progn
 (defalien "gtk_container_child_get_property"
           void
           (a (* t))
           (b (* t))
           (c c-string)
           (d (* t)))
 (defun gtk::container-child-get-property (a b c d)
   (|gtk_container_child_get_property| a b c d)))
(progn
 (defalien "gtk_container_forall" void (a (* t)) (b GtkCallback) (c gpointer))
 (defun gtk::container-forall (a b c) (|gtk_container_forall| a b c)))
(progn
 (defalien "gtk_bin_get_type" GType)
 (defun gtk::bin-get-type () (|gtk_bin_get_type|)))
(progn
 (defalien "gtk_bin_get_child" (* t) (a (* t)))
 (defun gtk::bin-get-child (a) (|gtk_bin_get_child| a)))
(progn
 (defalien "gtk_window_get_type" GType)
 (defun gtk::window-get-type () (|gtk_window_get_type|)))
(progn
 (defalien "gtk_window_new" (* t) (a GtkWindowType))
 (defun gtk::window-new (a) (|gtk_window_new| a)))
(progn
 (defalien "gtk_window_set_title" void (a (* t)) (b c-string))
 (defun gtk::window-set-title (a b) (|gtk_window_set_title| a b)))
(progn
 (defalien "gtk_window_get_title" c-string (a (* t)))
 (defun gtk::window-get-title (a) (|gtk_window_get_title| a)))
(progn
 (defalien "gtk_window_set_wmclass" void (a (* t)) (b c-string) (c c-string))
 (defun gtk::window-set-wmclass (a b c) (|gtk_window_set_wmclass| a b c)))
(progn
 (defalien "gtk_window_set_role" void (a (* t)) (b c-string))
 (defun gtk::window-set-role (a b) (|gtk_window_set_role| a b)))
(progn
 (defalien "gtk_window_get_role" c-string (a (* t)))
 (defun gtk::window-get-role (a) (|gtk_window_get_role| a)))
(progn
 (defalien "gtk_window_add_accel_group" void (a (* t)) (b (* t)))
 (defun gtk::window-add-accel-group (a b) (|gtk_window_add_accel_group| a b)))
(progn
 (defalien "gtk_window_remove_accel_group" void (a (* t)) (b (* t)))
 (defun gtk::window-remove-accel-group (a b)
   (|gtk_window_remove_accel_group| a b)))
(progn
 (defalien "gtk_window_set_position" void (a (* t)) (b GtkWindowPosition))
 (defun gtk::window-set-position (a b) (|gtk_window_set_position| a b)))
(progn
 (defalien "gtk_window_activate_focus" gboolean (a (* t)))
 (defun gtk::window-activate-focus (a)
   (let ((v302 (|gtk_window_activate_focus| a)))
     (if (= v302 1) t nil))))
(progn
 (defalien "gtk_window_set_focus" void (a (* t)) (b (* t)))
 (defun gtk::window-set-focus (a b) (|gtk_window_set_focus| a b)))
(progn
 (defalien "gtk_window_get_focus" (* t) (a (* t)))
 (defun gtk::window-get-focus (a) (|gtk_window_get_focus| a)))
(progn
 (defalien "gtk_window_set_default" void (a (* t)) (b (* t)))
 (defun gtk::window-set-default (a b) (|gtk_window_set_default| a b)))
(progn
 (defalien "gtk_window_activate_default" gboolean (a (* t)))
 (defun gtk::window-activate-default (a)
   (let ((v303 (|gtk_window_activate_default| a)))
     (if (= v303 1) t nil))))
(progn
 (defalien "gtk_window_set_transient_for" void (a (* t)) (b (* t)))
 (defun gtk::window-set-transient-for (a b)
   (|gtk_window_set_transient_for| a b)))
(progn
 (defalien "gtk_window_get_transient_for" (* t) (a (* t)))
 (defun gtk::window-get-transient-for (a) (|gtk_window_get_transient_for| a)))
(progn
 (defalien "gtk_window_set_type_hint" void (a (* t)) (b GdkWindowTypeHint))
 (defun gtk::window-set-type-hint (a b) (|gtk_window_set_type_hint| a b)))
(progn
 (defalien "gtk_window_get_type_hint" GdkWindowTypeHint (a (* t)))
 (defun gtk::window-get-type-hint (a) (|gtk_window_get_type_hint| a)))
(progn
 (defalien "gtk_window_set_destroy_with_parent" void (a (* t)) (b gboolean))
 (defun gtk::window-set-destroy-with-parent (a b)
   (|gtk_window_set_destroy_with_parent| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gtk_window_get_destroy_with_parent" gboolean (a (* t)))
 (defun gtk::window-get-destroy-with-parent (a)
   (let ((v304 (|gtk_window_get_destroy_with_parent| a)))
     (if (= v304 1) t nil))))
(progn
 (defalien "gtk_window_set_resizable" void (a (* t)) (b gboolean))
 (defun gtk::window-set-resizable (a b)
   (|gtk_window_set_resizable| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gtk_window_get_resizable" gboolean (a (* t)))
 (defun gtk::window-get-resizable (a)
   (let ((v305 (|gtk_window_get_resizable| a)))
     (if (= v305 1) t nil))))
(progn
 (defalien "gtk_window_set_gravity" void (a (* t)) (b GdkGravity))
 (defun gtk::window-set-gravity (a b) (|gtk_window_set_gravity| a b)))
(progn
 (defalien "gtk_window_get_gravity" GdkGravity (a (* t)))
 (defun gtk::window-get-gravity (a) (|gtk_window_get_gravity| a)))
(progn
 (defalien "gtk_window_set_geometry_hints"
           void
           (a (* t))
           (b (* t))
           (c (* t))
           (d GdkWindowHints))
 (defun gtk::window-set-geometry-hints (a b c d)
   (|gtk_window_set_geometry_hints| a b c d)))
(progn
 (defalien "gtk_window_set_has_frame" void (a (* t)) (b gboolean))
 (defun gtk::window-set-has-frame (a b)
   (|gtk_window_set_has_frame| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gtk_window_get_has_frame" gboolean (a (* t)))
 (defun gtk::window-get-has-frame (a)
   (let ((v306 (|gtk_window_get_has_frame| a)))
     (if (= v306 1) t nil))))
(progn
 (defalien "gtk_window_set_frame_dimensions"
           void
           (a (* t))
           (b gint)
           (c gint)
           (d gint)
           (e gint))
 (defun gtk::window-set-frame-dimensions (a b c d e)
   (|gtk_window_set_frame_dimensions| a b c d e)))
(progn
 (defalien "gtk_window_get_frame_dimensions"
           void
           (a (* t))
           (b gint :in-out)
           (c gint :in-out)
           (d gint :in-out)
           (e gint :in-out))
 (defun gtk::window-get-frame-dimensions (a b c d e)
   (|gtk_window_get_frame_dimensions| a b c d e)))
(progn
 (defalien "gtk_window_set_decorated" void (a (* t)) (b gboolean))
 (defun gtk::window-set-decorated (a b)
   (|gtk_window_set_decorated| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gtk_window_get_decorated" gboolean (a (* t)))
 (defun gtk::window-get-decorated (a)
   (let ((v307 (|gtk_window_get_decorated| a)))
     (if (= v307 1) t nil))))
(progn
 (defalien "gtk_window_set_icon_list" void (a (* t)) (b (* t)))
 (defun gtk::window-set-icon-list (a b) (|gtk_window_set_icon_list| a b)))
(progn
 (defalien "gtk_window_get_icon_list" (* t) (a (* t)))
 (defun gtk::window-get-icon-list (a) (|gtk_window_get_icon_list| a)))
(progn
 (defalien "gtk_window_set_icon" void (a (* t)) (b (* t)))
 (defun gtk::window-set-icon (a b) (|gtk_window_set_icon| a b)))
(progn
 (defalien "gtk_window_get_icon" (* t) (a (* t)))
 (defun gtk::window-get-icon (a) (|gtk_window_get_icon| a)))
(progn
 (defalien "gtk_window_set_default_icon_list" void (a (* t)))
 (defun gtk::window-set-default-icon-list (a)
   (|gtk_window_set_default_icon_list| a)))
(progn
 (defalien "gtk_window_get_default_icon_list" (* t))
 (defun gtk::window-get-default-icon-list ()
   (|gtk_window_get_default_icon_list|)))
(progn
 (defalien "gtk_window_set_modal" void (a (* t)) (b gboolean))
 (defun gtk::window-set-modal (a b)
   (|gtk_window_set_modal| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gtk_window_get_modal" gboolean (a (* t)))
 (defun gtk::window-get-modal (a)
   (let ((v308 (|gtk_window_get_modal| a)))
     (if (= v308 1) t nil))))
(progn
 (defalien "gtk_window_list_toplevels" (* t))
 (defun gtk::window-list-toplevels () (|gtk_window_list_toplevels|)))
(progn
 (defalien "gtk_window_add_mnemonic" void (a (* t)) (b guint) (c (* t)))
 (defun gtk::window-add-mnemonic (a b c) (|gtk_window_add_mnemonic| a b c)))
(progn
 (defalien "gtk_window_remove_mnemonic" void (a (* t)) (b guint) (c (* t)))
 (defun gtk::window-remove-mnemonic (a b c)
   (|gtk_window_remove_mnemonic| a b c)))
(progn
 (defalien "gtk_window_mnemonic_activate"
           gboolean
           (a (* t))
           (b guint)
           (c GdkModifierType))
 (defun gtk::window-mnemonic-activate (a b c)
   (let ((v309 (|gtk_window_mnemonic_activate| a b c)))
     (if (= v309 1) t nil))))
(progn
 (defalien "gtk_window_set_mnemonic_modifier"
           void
           (a (* t))
           (b GdkModifierType))
 (defun gtk::window-set-mnemonic-modifier (a b)
   (|gtk_window_set_mnemonic_modifier| a b)))
(progn
 (defalien "gtk_window_get_mnemonic_modifier" GdkModifierType (a (* t)))
 (defun gtk::window-get-mnemonic-modifier (a)
   (|gtk_window_get_mnemonic_modifier| a)))
(progn
 (defalien "gtk_window_present" void (a (* t)))
 (defun gtk::window-present (a) (|gtk_window_present| a)))
(progn
 (defalien "gtk_window_iconify" void (a (* t)))
 (defun gtk::window-iconify (a) (|gtk_window_iconify| a)))
(progn
 (defalien "gtk_window_deiconify" void (a (* t)))
 (defun gtk::window-deiconify (a) (|gtk_window_deiconify| a)))
(progn
 (defalien "gtk_window_stick" void (a (* t)))
 (defun gtk::window-stick (a) (|gtk_window_stick| a)))
(progn
 (defalien "gtk_window_unstick" void (a (* t)))
 (defun gtk::window-unstick (a) (|gtk_window_unstick| a)))
(progn
 (defalien "gtk_window_maximize" void (a (* t)))
 (defun gtk::window-maximize (a) (|gtk_window_maximize| a)))
(progn
 (defalien "gtk_window_unmaximize" void (a (* t)))
 (defun gtk::window-unmaximize (a) (|gtk_window_unmaximize| a)))
(progn
 (defalien "gtk_window_begin_resize_drag"
           void
           (a (* t))
           (b GdkWindowEdge)
           (c gint)
           (d gint)
           (e gint)
           (f guint32))
 (defun gtk::window-begin-resize-drag (a b c d e f)
   (|gtk_window_begin_resize_drag| a b c d e f)))
(progn
 (defalien "gtk_window_begin_move_drag"
           void
           (a (* t))
           (b gint)
           (c gint)
           (d gint)
           (e guint32))
 (defun gtk::window-begin-move-drag (a b c d e)
   (|gtk_window_begin_move_drag| a b c d e)))
(progn
 (defalien "gtk_window_set_default_size" void (a (* t)) (b gint) (c gint))
 (defun gtk::window-set-default-size (a b c)
   (|gtk_window_set_default_size| a b c)))
(progn
 (defalien "gtk_window_get_default_size"
           void
           (a (* t))
           (b gint :in-out)
           (c gint :in-out))
 (defun gtk::window-get-default-size (a b c)
   (|gtk_window_get_default_size| a b c)))
(progn
 (defalien "gtk_window_resize" void (a (* t)) (b gint) (c gint))
 (defun gtk::window-resize (a b c) (|gtk_window_resize| a b c)))
(progn
 (defalien "gtk_window_get_size"
           void
           (a (* t))
           (b gint :in-out)
           (c gint :in-out))
 (defun gtk::window-get-size (a b c) (|gtk_window_get_size| a b c)))
(progn
 (defalien "gtk_window_move" void (a (* t)) (b gint) (c gint))
 (defun gtk::window-move (a b c) (|gtk_window_move| a b c)))
(progn
 (defalien "gtk_window_get_position"
           void
           (a (* t))
           (b gint :in-out)
           (c gint :in-out))
 (defun gtk::window-get-position (a b c) (|gtk_window_get_position| a b c)))
(progn
 (defalien "gtk_window_parse_geometry" gboolean (a (* t)) (b c-string))
 (defun gtk::window-parse-geometry (a b)
   (let ((v310 (|gtk_window_parse_geometry| a b)))
     (if (= v310 1) t nil))))
(progn
 (defalien "gtk_window_reshow_with_initial_size" void (a (* t)))
 (defun gtk::window-reshow-with-initial-size (a)
   (|gtk_window_reshow_with_initial_size| a)))
(progn
 (defalien "gtk_window_group_get_type" GType)
 (defun gtk::window-group-get-type () (|gtk_window_group_get_type|)))
(progn
 (defalien "gtk_window_group_new" (* t))
 (defun gtk::window-group-new () (|gtk_window_group_new|)))
(progn
 (defalien "gtk_window_group_add_window" void (a (* t)) (b (* t)))
 (defun gtk::window-group-add-window (a b) (|gtk_window_group_add_window| a b)))
(progn
 (defalien "gtk_window_group_remove_window" void (a (* t)) (b (* t)))
 (defun gtk::window-group-remove-window (a b)
   (|gtk_window_group_remove_window| a b)))
(progn
 (defalien "gtk_window_remove_embedded_xid" void (a (* t)) (b guint))
 (defun gtk::window-remove-embedded-xid (a b)
   (|gtk_window_remove_embedded_xid| a b)))
(progn
 (defalien "gtk_window_add_embedded_xid" void (a (* t)) (b guint))
 (defun gtk::window-add-embedded-xid (a b) (|gtk_window_add_embedded_xid| a b)))
(progn
 (defalien "gtk_menu_shell_get_type" GType)
 (defun gtk::menu-shell-get-type () (|gtk_menu_shell_get_type|)))
(progn
 (defalien "gtk_menu_shell_append" void (a (* t)) (b (* t)))
 (defun gtk::menu-shell-append (a b) (|gtk_menu_shell_append| a b)))
(progn
 (defalien "gtk_menu_shell_prepend" void (a (* t)) (b (* t)))
 (defun gtk::menu-shell-prepend (a b) (|gtk_menu_shell_prepend| a b)))
(progn
 (defalien "gtk_menu_shell_insert" void (a (* t)) (b (* t)) (c gint))
 (defun gtk::menu-shell-insert (a b c) (|gtk_menu_shell_insert| a b c)))
(progn
 (defalien "gtk_menu_shell_deactivate" void (a (* t)))
 (defun gtk::menu-shell-deactivate (a) (|gtk_menu_shell_deactivate| a)))
(progn
 (defalien "gtk_menu_shell_select_item" void (a (* t)) (b (* t)))
 (defun gtk::menu-shell-select-item (a b) (|gtk_menu_shell_select_item| a b)))
(progn
 (defalien "gtk_menu_shell_deselect" void (a (* t)))
 (defun gtk::menu-shell-deselect (a) (|gtk_menu_shell_deselect| a)))
(progn
 (defalien "gtk_menu_shell_activate_item"
           void
           (a (* t))
           (b (* t))
           (c gboolean))
 (defun gtk::menu-shell-activate-item (a b c)
   (|gtk_menu_shell_activate_item| a b (if c (if (eq c 0) 0 1) 0))))
(progn
 (defalien "gtk_menu_get_type" GType)
 (defun gtk::menu-get-type () (|gtk_menu_get_type|)))
(progn
 (defalien "gtk_menu_new" (* t))
 (defun gtk::menu-new () (|gtk_menu_new|)))
(progn
 (defalien "gtk_menu_popup"
           void
           (a (* t))
           (b (* t))
           (c (* t))
           (d GtkMenuPositionFunc)
           (e gpointer)
           (f guint)
           (g guint32))
 (defun gtk::menu-popup (a b c d e f g) (|gtk_menu_popup| a b c d e f g)))
(progn
 (defalien "gtk_menu_reposition" void (a (* t)))
 (defun gtk::menu-reposition (a) (|gtk_menu_reposition| a)))
(progn
 (defalien "gtk_menu_popdown" void (a (* t)))
 (defun gtk::menu-popdown (a) (|gtk_menu_popdown| a)))
(progn
 (defalien "gtk_menu_get_active" (* t) (a (* t)))
 (defun gtk::menu-get-active (a) (|gtk_menu_get_active| a)))
(progn
 (defalien "gtk_menu_set_active" void (a (* t)) (b guint))
 (defun gtk::menu-set-active (a b) (|gtk_menu_set_active| a b)))
(progn
 (defalien "gtk_menu_set_accel_group" void (a (* t)) (b (* t)))
 (defun gtk::menu-set-accel-group (a b) (|gtk_menu_set_accel_group| a b)))
(progn
 (defalien "gtk_menu_get_accel_group" (* t) (a (* t)))
 (defun gtk::menu-get-accel-group (a) (|gtk_menu_get_accel_group| a)))
(progn
 (defalien "gtk_menu_set_accel_path" void (a (* t)) (b c-string))
 (defun gtk::menu-set-accel-path (a b) (|gtk_menu_set_accel_path| a b)))
(progn
 (defalien "gtk_menu_attach_to_widget"
           void
           (a (* t))
           (b (* t))
           (c GtkMenuDetachFunc))
 (defun gtk::menu-attach-to-widget (a b c) (|gtk_menu_attach_to_widget| a b c)))
(progn
 (defalien "gtk_menu_detach" void (a (* t)))
 (defun gtk::menu-detach (a) (|gtk_menu_detach| a)))
(progn
 (defalien "gtk_menu_get_attach_widget" (* t) (a (* t)))
 (defun gtk::menu-get-attach-widget (a) (|gtk_menu_get_attach_widget| a)))
(progn
 (defalien "gtk_menu_set_tearoff_state" void (a (* t)) (b gboolean))
 (defun gtk::menu-set-tearoff-state (a b)
   (|gtk_menu_set_tearoff_state| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gtk_menu_get_tearoff_state" gboolean (a (* t)))
 (defun gtk::menu-get-tearoff-state (a)
   (let ((v311 (|gtk_menu_get_tearoff_state| a)))
     (if (= v311 1) t nil))))
(progn
 (defalien "gtk_menu_set_title" void (a (* t)) (b c-string))
 (defun gtk::menu-set-title (a b) (|gtk_menu_set_title| a b)))
(progn
 (defalien "gtk_menu_get_title" c-string (a (* t)))
 (defun gtk::menu-get-title (a) (|gtk_menu_get_title| a)))
(progn
 (defalien "gtk_menu_reorder_child" void (a (* t)) (b (* t)) (c gint))
 (defun gtk::menu-reorder-child (a b c) (|gtk_menu_reorder_child| a b c)))
(progn
 (defalien "gtk_label_get_type" GType)
 (defun gtk::label-get-type () (|gtk_label_get_type|)))
(progn
 (defalien "gtk_label_new" (* t) (a c-string))
 (defun gtk::label-new (a) (|gtk_label_new| a)))
(progn
 (defalien "gtk_label_new_with_mnemonic" (* t) (a c-string))
 (defun gtk::label-new-with-mnemonic (a) (|gtk_label_new_with_mnemonic| a)))
(progn
 (defalien "gtk_label_set_text" void (a (* t)) (b c-string))
 (defun gtk::label-set-text (a b) (|gtk_label_set_text| a b)))
(progn
 (defalien "gtk_label_get_text" c-string (a (* t)))
 (defun gtk::label-get-text (a) (|gtk_label_get_text| a)))
(progn
 (defalien "gtk_label_set_attributes" void (a (* t)) (b (* t)))
 (defun gtk::label-set-attributes (a b) (|gtk_label_set_attributes| a b)))
(progn
 (defalien "gtk_label_get_attributes" (* t) (a (* t)))
 (defun gtk::label-get-attributes (a) (|gtk_label_get_attributes| a)))
(progn
 (defalien "gtk_label_set_label" void (a (* t)) (b c-string))
 (defun gtk::label-set-label (a b) (|gtk_label_set_label| a b)))
(progn
 (defalien "gtk_label_get_label" c-string (a (* t)))
 (defun gtk::label-get-label (a) (|gtk_label_get_label| a)))
(progn
 (defalien "gtk_label_set_markup" void (a (* t)) (b c-string))
 (defun gtk::label-set-markup (a b) (|gtk_label_set_markup| a b)))
(progn
 (defalien "gtk_label_set_use_markup" void (a (* t)) (b gboolean))
 (defun gtk::label-set-use-markup (a b)
   (|gtk_label_set_use_markup| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gtk_label_get_use_markup" gboolean (a (* t)))
 (defun gtk::label-get-use-markup (a)
   (let ((v312 (|gtk_label_get_use_markup| a)))
     (if (= v312 1) t nil))))
(progn
 (defalien "gtk_label_set_use_underline" void (a (* t)) (b gboolean))
 (defun gtk::label-set-use-underline (a b)
   (|gtk_label_set_use_underline| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gtk_label_get_use_underline" gboolean (a (* t)))
 (defun gtk::label-get-use-underline (a)
   (let ((v313 (|gtk_label_get_use_underline| a)))
     (if (= v313 1) t nil))))
(progn
 (defalien "gtk_label_set_markup_with_mnemonic" void (a (* t)) (b c-string))
 (defun gtk::label-set-markup-with-mnemonic (a b)
   (|gtk_label_set_markup_with_mnemonic| a b)))
(progn
 (defalien "gtk_label_get_mnemonic_keyval" guint (a (* t)))
 (defun gtk::label-get-mnemonic-keyval (a) (|gtk_label_get_mnemonic_keyval| a)))
(progn
 (defalien "gtk_label_set_mnemonic_widget" void (a (* t)) (b (* t)))
 (defun gtk::label-set-mnemonic-widget (a b)
   (|gtk_label_set_mnemonic_widget| a b)))
(progn
 (defalien "gtk_label_get_mnemonic_widget" (* t) (a (* t)))
 (defun gtk::label-get-mnemonic-widget (a) (|gtk_label_get_mnemonic_widget| a)))
(progn
 (defalien "gtk_label_set_text_with_mnemonic" void (a (* t)) (b c-string))
 (defun gtk::label-set-text-with-mnemonic (a b)
   (|gtk_label_set_text_with_mnemonic| a b)))
(progn
 (defalien "gtk_label_set_justify" void (a (* t)) (b GtkJustification))
 (defun gtk::label-set-justify (a b) (|gtk_label_set_justify| a b)))
(progn
 (defalien "gtk_label_get_justify" GtkJustification (a (* t)))
 (defun gtk::label-get-justify (a) (|gtk_label_get_justify| a)))
(progn
 (defalien "gtk_label_set_pattern" void (a (* t)) (b c-string))
 (defun gtk::label-set-pattern (a b) (|gtk_label_set_pattern| a b)))
(progn
 (defalien "gtk_label_set_line_wrap" void (a (* t)) (b gboolean))
 (defun gtk::label-set-line-wrap (a b)
   (|gtk_label_set_line_wrap| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gtk_label_get_line_wrap" gboolean (a (* t)))
 (defun gtk::label-get-line-wrap (a)
   (let ((v314 (|gtk_label_get_line_wrap| a)))
     (if (= v314 1) t nil))))
(progn
 (defalien "gtk_label_set_selectable" void (a (* t)) (b gboolean))
 (defun gtk::label-set-selectable (a b)
   (|gtk_label_set_selectable| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gtk_label_get_selectable" gboolean (a (* t)))
 (defun gtk::label-get-selectable (a)
   (let ((v315 (|gtk_label_get_selectable| a)))
     (if (= v315 1) t nil))))
(progn
 (defalien "gtk_label_select_region" void (a (* t)) (b gint) (c gint))
 (defun gtk::label-select-region (a b c) (|gtk_label_select_region| a b c)))
(progn
 (defalien "gtk_label_get_selection_bounds"
           gboolean
           (a (* t))
           (b gint :in-out)
           (c gint :in-out))
 (defun gtk::label-get-selection-bounds (a b c)
   (let ((v316 (multiple-value-list (|gtk_label_get_selection_bounds| a b c))))
     (apply #'values (if (= 1 (car v316)) t nil) (cdr v316)))))
(progn
 (defalien "gtk_label_get_layout" (* t) (a (* t)))
 (defun gtk::label-get-layout (a) (|gtk_label_get_layout| a)))
(progn
 (defalien "gtk_label_get_layout_offsets"
           void
           (a (* t))
           (b gint :in-out)
           (c gint :in-out))
 (defun gtk::label-get-layout-offsets (a b c)
   (|gtk_label_get_layout_offsets| a b c)))
(progn
 (defalien "gtk_accel_label_get_type" GType)
 (defun gtk::accel-label-get-type () (|gtk_accel_label_get_type|)))
(progn
 (defalien "gtk_accel_label_new" (* t) (a c-string))
 (defun gtk::accel-label-new (a) (|gtk_accel_label_new| a)))
(progn
 (defalien "gtk_accel_label_get_accel_widget" (* t) (a (* t)))
 (defun gtk::accel-label-get-accel-widget (a)
   (|gtk_accel_label_get_accel_widget| a)))
(progn
 (defalien "gtk_accel_label_get_accel_width" guint (a (* t)))
 (defun gtk::accel-label-get-accel-width (a)
   (|gtk_accel_label_get_accel_width| a)))
(progn
 (defalien "gtk_accel_label_set_accel_widget" void (a (* t)) (b (* t)))
 (defun gtk::accel-label-set-accel-widget (a b)
   (|gtk_accel_label_set_accel_widget| a b)))
(progn
 (defalien "gtk_accel_label_set_accel_closure" void (a (* t)) (b (* t)))
 (defun gtk::accel-label-set-accel-closure (a b)
   (|gtk_accel_label_set_accel_closure| a b)))
(progn
 (defalien "gtk_accel_label_refetch" gboolean (a (* t)))
 (defun gtk::accel-label-refetch (a)
   (let ((v317 (|gtk_accel_label_refetch| a)))
     (if (= v317 1) t nil))))
(progn
 (defalien "gtk_accel_map_lookup_entry" gboolean (a c-string) (b (* t)))
 (defun gtk::accel-map-lookup-entry (a b)
   (let ((v318 (|gtk_accel_map_lookup_entry| a b)))
     (if (= v318 1) t nil))))
(progn
 (defalien "gtk_accel_map_change_entry"
           gboolean
           (a c-string)
           (b guint)
           (c GdkModifierType)
           (d gboolean))
 (defun gtk::accel-map-change-entry (a b c d)
   (let ((v319 (|gtk_accel_map_change_entry| a b c (if d (if (eq d 0) 0 1) 0))))
     (if (= v319 1) t nil))))
(progn
 (defalien "gtk_accel_map_load" void (a c-string))
 (defun gtk::accel-map-load (a) (|gtk_accel_map_load| a)))
(progn
 (defalien "gtk_accel_map_save" void (a c-string))
 (defun gtk::accel-map-save (a) (|gtk_accel_map_save| a)))
(progn
 (defalien "gtk_accel_map_foreach" void (a gpointer) (b GtkAccelMapForeach))
 (defun gtk::accel-map-foreach (a b) (|gtk_accel_map_foreach| a b)))
(progn
 (defalien "gtk_accel_map_load_fd" void (a gint))
 (defun gtk::accel-map-load-fd (a) (|gtk_accel_map_load_fd| a)))
(progn
 (defalien "gtk_accel_map_load_scanner" void (a (* t)))
 (defun gtk::accel-map-load-scanner (a) (|gtk_accel_map_load_scanner| a)))
(progn
 (defalien "gtk_accel_map_save_fd" void (a gint))
 (defun gtk::accel-map-save-fd (a) (|gtk_accel_map_save_fd| a)))
(progn
 (defalien "gtk_accel_map_add_filter" void (a c-string))
 (defun gtk::accel-map-add-filter (a) (|gtk_accel_map_add_filter| a)))
(progn
 (defalien "gtk_accel_map_foreach_unfiltered"
           void
           (a gpointer)
           (b GtkAccelMapForeach))
 (defun gtk::accel-map-foreach-unfiltered (a b)
   (|gtk_accel_map_foreach_unfiltered| a b)))
(progn
 (defalien "atk_action_get_type" GType)
 (defun atk::action-get-type () (|atk_action_get_type|)))
(progn
 (defalien "atk_action_do_action" gboolean (a (* t)) (b gint))
 (defun atk::action-do-action (a b)
   (let ((v320 (|atk_action_do_action| a b)))
     (if (= v320 1) t nil))))
(progn
 (defalien "atk_action_get_n_actions" gint (a (* t)))
 (defun atk::action-get-n-actions (a) (|atk_action_get_n_actions| a)))
(progn
 (defalien "atk_action_get_description" c-string (a (* t)) (b gint))
 (defun atk::action-get-description (a b) (|atk_action_get_description| a b)))
(progn
 (defalien "atk_action_get_name" c-string (a (* t)) (b gint))
 (defun atk::action-get-name (a b) (|atk_action_get_name| a b)))
(progn
 (defalien "atk_action_get_keybinding" c-string (a (* t)) (b gint))
 (defun atk::action-get-keybinding (a b) (|atk_action_get_keybinding| a b)))
(progn
 (defalien "atk_action_set_description"
           gboolean
           (a (* t))
           (b gint)
           (c c-string))
 (defun atk::action-set-description (a b c)
   (let ((v321 (|atk_action_set_description| a b c)))
     (if (= v321 1) t nil))))
(progn
 (defalien "atk_util_get_type" GType)
 (defun atk::util-get-type () (|atk_util_get_type|)))
(progn
 (defalien "atk_add_focus_tracker" guint (a AtkEventListener))
 (defun atk::add-focus-tracker (a) (|atk_add_focus_tracker| a)))
(progn
 (defalien "atk_remove_focus_tracker" void (a guint))
 (defun atk::remove-focus-tracker (a) (|atk_remove_focus_tracker| a)))
(progn
 (defalien "atk_focus_tracker_init" void (a AtkEventListenerInit))
 (defun atk::focus-tracker-init (a) (|atk_focus_tracker_init| a)))
(progn
 (defalien "atk_focus_tracker_notify" void (a (* t)))
 (defun atk::focus-tracker-notify (a) (|atk_focus_tracker_notify| a)))
(progn
 (defalien "atk_add_global_event_listener"
           guint
           (a GSignalEmissionHook)
           (b c-string))
 (defun atk::add-global-event-listener (a b)
   (|atk_add_global_event_listener| a b)))
(progn
 (defalien "atk_remove_global_event_listener" void (a guint))
 (defun atk::remove-global-event-listener (a)
   (|atk_remove_global_event_listener| a)))
(progn
 (defalien "atk_add_key_event_listener" guint (a AtkKeySnoopFunc) (b gpointer))
 (defun atk::add-key-event-listener (a b) (|atk_add_key_event_listener| a b)))
(progn
 (defalien "atk_remove_key_event_listener" void (a guint))
 (defun atk::remove-key-event-listener (a) (|atk_remove_key_event_listener| a)))
(progn
 (defalien "atk_get_root" (* t))
 (defun atk::get-root () (|atk_get_root|)))
(progn
 (defalien "atk_get_toolkit_name" c-string)
 (defun atk::get-toolkit-name () (|atk_get_toolkit_name|)))
(progn
 (defalien "atk_get_toolkit_version" c-string)
 (defun atk::get-toolkit-version () (|atk_get_toolkit_version|)))
(progn
 (defalien "atk_component_get_type" GType)
 (defun atk::component-get-type () (|atk_component_get_type|)))
(progn
 (defalien "atk_component_add_focus_handler"
           guint
           (a (* t))
           (b AtkFocusHandler))
 (defun atk::component-add-focus-handler (a b)
   (|atk_component_add_focus_handler| a b)))
(progn
 (defalien "atk_component_contains"
           gboolean
           (a (* t))
           (b gint)
           (c gint)
           (d AtkCoordType))
 (defun atk::component-contains (a b c d)
   (let ((v322 (|atk_component_contains| a b c d)))
     (if (= v322 1) t nil))))
(progn
 (defalien "atk_component_ref_accessible_at_point"
           (* t)
           (a (* t))
           (b gint)
           (c gint)
           (d AtkCoordType))
 (defun atk::component-ref-accessible-at-point (a b c d)
   (|atk_component_ref_accessible_at_point| a b c d)))
(progn
 (defalien "atk_component_get_extents"
           void
           (a (* t))
           (b gint :in-out)
           (c gint :in-out)
           (d gint :in-out)
           (e gint :in-out)
           (f AtkCoordType))
 (defun atk::component-get-extents (a b c d e f)
   (|atk_component_get_extents| a b c d e f)))
(progn
 (defalien "atk_component_get_position"
           void
           (a (* t))
           (b gint :in-out)
           (c gint :in-out)
           (d AtkCoordType))
 (defun atk::component-get-position (a b c d)
   (|atk_component_get_position| a b c d)))
(progn
 (defalien "atk_component_get_size"
           void
           (a (* t))
           (b gint :in-out)
           (c gint :in-out))
 (defun atk::component-get-size (a b c) (|atk_component_get_size| a b c)))
(progn
 (defalien "atk_component_grab_focus" gboolean (a (* t)))
 (defun atk::component-grab-focus (a)
   (let ((v323 (|atk_component_grab_focus| a)))
     (if (= v323 1) t nil))))
(progn
 (defalien "atk_component_remove_focus_handler" void (a (* t)) (b guint))
 (defun atk::component-remove-focus-handler (a b)
   (|atk_component_remove_focus_handler| a b)))
(progn
 (defalien "atk_component_set_extents"
           gboolean
           (a (* t))
           (b gint)
           (c gint)
           (d gint)
           (e gint)
           (f AtkCoordType))
 (defun atk::component-set-extents (a b c d e f)
   (let ((v324 (|atk_component_set_extents| a b c d e f)))
     (if (= v324 1) t nil))))
(progn
 (defalien "atk_component_set_position"
           gboolean
           (a (* t))
           (b gint)
           (c gint)
           (d AtkCoordType))
 (defun atk::component-set-position (a b c d)
   (let ((v325 (|atk_component_set_position| a b c d)))
     (if (= v325 1) t nil))))
(progn
 (defalien "atk_component_set_size" gboolean (a (* t)) (b gint) (c gint))
 (defun atk::component-set-size (a b c)
   (let ((v326 (|atk_component_set_size| a b c)))
     (if (= v326 1) t nil))))
(progn
 (defalien "atk_document_get_type" GType)
 (defun atk::document-get-type () (|atk_document_get_type|)))
(progn
 (defalien "atk_document_get_document_type" c-string (a (* t)))
 (defun atk::document-get-document-type (a)
   (|atk_document_get_document_type| a)))
(progn
 (defalien "atk_document_get_document" gpointer (a (* t)))
 (defun atk::document-get-document (a) (|atk_document_get_document| a)))
(progn
 (defalien "atk_text_get_type" GType)
 (defun atk::text-get-type () (|atk_text_get_type|)))
(progn
 (defalien "atk_text_get_text" gchar (a (* t)) (b gint) (c gint))
 (defun atk::text-get-text (a b c) (|atk_text_get_text| a b c)))
(progn
 (defalien "atk_text_get_character_at_offset" gunichar (a (* t)) (b gint))
 (defun atk::text-get-character-at-offset (a b)
   (|atk_text_get_character_at_offset| a b)))
(progn
 (defalien "atk_text_get_text_after_offset"
           gchar
           (a (* t))
           (b gint)
           (c AtkTextBoundary)
           (d gint :in-out)
           (e gint :in-out))
 (defun atk::text-get-text-after-offset (a b c d e)
   (|atk_text_get_text_after_offset| a b c d e)))
(progn
 (defalien "atk_text_get_text_at_offset"
           gchar
           (a (* t))
           (b gint)
           (c AtkTextBoundary)
           (d gint :in-out)
           (e gint :in-out))
 (defun atk::text-get-text-at-offset (a b c d e)
   (|atk_text_get_text_at_offset| a b c d e)))
(progn
 (defalien "atk_text_get_text_before_offset"
           gchar
           (a (* t))
           (b gint)
           (c AtkTextBoundary)
           (d gint :in-out)
           (e gint :in-out))
 (defun atk::text-get-text-before-offset (a b c d e)
   (|atk_text_get_text_before_offset| a b c d e)))
(progn
 (defalien "atk_text_get_caret_offset" gint (a (* t)))
 (defun atk::text-get-caret-offset (a) (|atk_text_get_caret_offset| a)))
(progn
 (defalien "atk_text_get_character_extents"
           void
           (a (* t))
           (b gint)
           (c gint :in-out)
           (d gint :in-out)
           (e gint :in-out)
           (f gint :in-out)
           (g AtkCoordType))
 (defun atk::text-get-character-extents (a b c d e f g)
   (|atk_text_get_character_extents| a b c d e f g)))
(progn
 (defalien "atk_text_get_run_attributes"
           (* t)
           (a (* t))
           (b gint)
           (c gint :in-out)
           (d gint :in-out))
 (defun atk::text-get-run-attributes (a b c d)
   (|atk_text_get_run_attributes| a b c d)))
(progn
 (defalien "atk_text_get_default_attributes" (* t) (a (* t)))
 (defun atk::text-get-default-attributes (a)
   (|atk_text_get_default_attributes| a)))
(progn
 (defalien "atk_text_get_character_count" gint (a (* t)))
 (defun atk::text-get-character-count (a) (|atk_text_get_character_count| a)))
(progn
 (defalien "atk_text_get_offset_at_point"
           gint
           (a (* t))
           (b gint)
           (c gint)
           (d AtkCoordType))
 (defun atk::text-get-offset-at-point (a b c d)
   (|atk_text_get_offset_at_point| a b c d)))
(progn
 (defalien "atk_text_get_n_selections" gint (a (* t)))
 (defun atk::text-get-n-selections (a) (|atk_text_get_n_selections| a)))
(progn
 (defalien "atk_text_get_selection"
           gchar
           (a (* t))
           (b gint)
           (c gint :in-out)
           (d gint :in-out))
 (defun atk::text-get-selection (a b c d) (|atk_text_get_selection| a b c d)))
(progn
 (defalien "atk_text_add_selection" gboolean (a (* t)) (b gint) (c gint))
 (defun atk::text-add-selection (a b c)
   (let ((v327 (|atk_text_add_selection| a b c)))
     (if (= v327 1) t nil))))
(progn
 (defalien "atk_text_remove_selection" gboolean (a (* t)) (b gint))
 (defun atk::text-remove-selection (a b)
   (let ((v328 (|atk_text_remove_selection| a b)))
     (if (= v328 1) t nil))))
(progn
 (defalien "atk_text_set_selection"
           gboolean
           (a (* t))
           (b gint)
           (c gint)
           (d gint))
 (defun atk::text-set-selection (a b c d)
   (let ((v329 (|atk_text_set_selection| a b c d)))
     (if (= v329 1) t nil))))
(progn
 (defalien "atk_text_set_caret_offset" gboolean (a (* t)) (b gint))
 (defun atk::text-set-caret-offset (a b)
   (let ((v330 (|atk_text_set_caret_offset| a b)))
     (if (= v330 1) t nil))))
(progn
 (defalien "atk_attribute_set_free" void (a (* t)))
 (defun atk::attribute-set-free (a) (|atk_attribute_set_free| a)))
(progn
 (defalien "atk_text_attribute_get_name" c-string (a AtkTextAttribute))
 (defun atk::text-attribute-get-name (a) (|atk_text_attribute_get_name| a)))
(progn
 (defalien "atk_text_attribute_get_value"
           c-string
           (a AtkTextAttribute)
           (b gint))
 (defun atk::text-attribute-get-value (a b)
   (|atk_text_attribute_get_value| a b)))
(progn
 (defalien "atk_editable_text_get_type" GType)
 (defun atk::editable-text-get-type () (|atk_editable_text_get_type|)))
(progn
 (defalien "atk_editable_text_set_run_attributes"
           gboolean
           (a (* t))
           (b (* t))
           (c gint)
           (d gint))
 (defun atk::editable-text-set-run-attributes (a b c d)
   (let ((v331 (|atk_editable_text_set_run_attributes| a b c d)))
     (if (= v331 1) t nil))))
(progn
 (defalien "atk_editable_text_set_text_contents" void (a (* t)) (b c-string))
 (defun atk::editable-text-set-text-contents (a b)
   (|atk_editable_text_set_text_contents| a b)))
(progn
 (defalien "atk_editable_text_insert_text"
           void
           (a (* t))
           (b c-string)
           (c gint)
           (d gint :in-out))
 (defun atk::editable-text-insert-text (a b c d)
   (|atk_editable_text_insert_text| a b c d)))
(progn
 (defalien "atk_editable_text_copy_text" void (a (* t)) (b gint) (c gint))
 (defun atk::editable-text-copy-text (a b c)
   (|atk_editable_text_copy_text| a b c)))
(progn
 (defalien "atk_editable_text_cut_text" void (a (* t)) (b gint) (c gint))
 (defun atk::editable-text-cut-text (a b c)
   (|atk_editable_text_cut_text| a b c)))
(progn
 (defalien "atk_editable_text_delete_text" void (a (* t)) (b gint) (c gint))
 (defun atk::editable-text-delete-text (a b c)
   (|atk_editable_text_delete_text| a b c)))
(progn
 (defalien "atk_editable_text_paste_text" void (a (* t)) (b gint))
 (defun atk::editable-text-paste-text (a b)
   (|atk_editable_text_paste_text| a b)))
(progn
 (defalien "atk_hyperlink_get_type" GType)
 (defun atk::hyperlink-get-type () (|atk_hyperlink_get_type|)))
(progn
 (defalien "atk_hyperlink_get_uri" gchar (a (* t)) (b gint))
 (defun atk::hyperlink-get-uri (a b) (|atk_hyperlink_get_uri| a b)))
(progn
 (defalien "atk_hyperlink_get_object" (* t) (a (* t)) (b gint))
 (defun atk::hyperlink-get-object (a b) (|atk_hyperlink_get_object| a b)))
(progn
 (defalien "atk_hyperlink_get_end_index" gint (a (* t)))
 (defun atk::hyperlink-get-end-index (a) (|atk_hyperlink_get_end_index| a)))
(progn
 (defalien "atk_hyperlink_get_start_index" gint (a (* t)))
 (defun atk::hyperlink-get-start-index (a) (|atk_hyperlink_get_start_index| a)))
(progn
 (defalien "atk_hyperlink_is_valid" gboolean (a (* t)))
 (defun atk::hyperlink-is-valid (a)
   (let ((v332 (|atk_hyperlink_is_valid| a)))
     (if (= v332 1) t nil))))
(progn
 (defalien "atk_hyperlink_get_n_anchors" gint (a (* t)))
 (defun atk::hyperlink-get-n-anchors (a) (|atk_hyperlink_get_n_anchors| a)))
(progn
 (defalien "atk_hypertext_get_type" GType)
 (defun atk::hypertext-get-type () (|atk_hypertext_get_type|)))
(progn
 (defalien "atk_hypertext_get_link" (* t) (a (* t)) (b gint))
 (defun atk::hypertext-get-link (a b) (|atk_hypertext_get_link| a b)))
(progn
 (defalien "atk_hypertext_get_n_links" gint (a (* t)))
 (defun atk::hypertext-get-n-links (a) (|atk_hypertext_get_n_links| a)))
(progn
 (defalien "atk_hypertext_get_link_index" gint (a (* t)) (b gint))
 (defun atk::hypertext-get-link-index (a b)
   (|atk_hypertext_get_link_index| a b)))
(progn
 (defalien "atk_image_get_type" GType)
 (defun atk::image-get-type () (|atk_image_get_type|)))
(progn
 (defalien "atk_image_get_image_description" c-string (a (* t)))
 (defun atk::image-get-image-description (a)
   (|atk_image_get_image_description| a)))
(progn
 (defalien "atk_image_get_image_size"
           void
           (a (* t))
           (b gint :in-out)
           (c gint :in-out))
 (defun atk::image-get-image-size (a b c) (|atk_image_get_image_size| a b c)))
(progn
 (defalien "atk_image_set_image_description" gboolean (a (* t)) (b c-string))
 (defun atk::image-set-image-description (a b)
   (let ((v333 (|atk_image_set_image_description| a b)))
     (if (= v333 1) t nil))))
(progn
 (defalien "atk_image_get_image_position"
           void
           (a (* t))
           (b gint :in-out)
           (c gint :in-out)
           (d AtkCoordType))
 (defun atk::image-get-image-position (a b c d)
   (|atk_image_get_image_position| a b c d)))
(progn
 (defalien "atk_no_op_object_get_type" GType)
 (defun atk::no-op-object-get-type () (|atk_no_op_object_get_type|)))
(progn
 (defalien "atk_no_op_object_new" (* t) (a (* t)))
 (defun atk::no-op-object-new (a) (|atk_no_op_object_new| a)))
(progn
 (defalien "atk_object_factory_get_type" GType)
 (defun atk::object-factory-get-type () (|atk_object_factory_get_type|)))
(progn
 (defalien "atk_object_factory_create_accessible" (* t) (a (* t)) (b (* t)))
 (defun atk::object-factory-create-accessible (a b)
   (|atk_object_factory_create_accessible| a b)))
(progn
 (defalien "atk_object_factory_invalidate" void (a (* t)))
 (defun atk::object-factory-invalidate (a) (|atk_object_factory_invalidate| a)))
(progn
 (defalien "atk_no_op_object_factory_get_type" GType)
 (defun atk::no-op-object-factory-get-type ()
   (|atk_no_op_object_factory_get_type|)))
(progn
 (defalien "atk_no_op_object_factory_new" (* t))
 (defun atk::no-op-object-factory-new () (|atk_no_op_object_factory_new|)))
(progn
 (defalien "atk_registry_get_type" GType)
 (defun atk::registry-get-type () (|atk_registry_get_type|)))
(progn
 (defalien "atk_registry_set_factory_type" void (a (* t)) (b GType) (c GType))
 (defun atk::registry-set-factory-type (a b c)
   (|atk_registry_set_factory_type| a b c)))
(progn
 (defalien "atk_registry_get_factory_type" GType (a (* t)) (b GType))
 (defun atk::registry-get-factory-type (a b)
   (|atk_registry_get_factory_type| a b)))
(progn
 (defalien "atk_registry_get_factory" (* t) (a (* t)) (b GType))
 (defun atk::registry-get-factory (a b) (|atk_registry_get_factory| a b)))
(progn
 (defalien "atk_get_default_registry" (* t))
 (defun atk::get-default-registry () (|atk_get_default_registry|)))
(progn
 (defalien "atk_relation_get_type" GType)
 (defun atk::relation-get-type () (|atk_relation_get_type|)))
(progn
 (defalien "atk_relation_type_register" AtkRelationType (a c-string))
 (defun atk::relation-type-register (a) (|atk_relation_type_register| a)))
(progn
 (defalien "atk_relation_type_get_name" c-string (a AtkRelationType))
 (defun atk::relation-type-get-name (a) (|atk_relation_type_get_name| a)))
(progn
 (defalien "atk_relation_type_for_name" AtkRelationType (a c-string))
 (defun atk::relation-type-for-name (a) (|atk_relation_type_for_name| a)))
(progn
 (defalien "atk_relation_new" (* t) (a (* t)) (b gint) (c AtkRelationType))
 (defun atk::relation-new (a b c) (|atk_relation_new| a b c)))
(progn
 (defalien "atk_relation_get_relation_type" AtkRelationType (a (* t)))
 (defun atk::relation-get-relation-type (a)
   (|atk_relation_get_relation_type| a)))
(progn
 (defalien "atk_relation_get_target" (* t) (a (* t)))
 (defun atk::relation-get-target (a) (|atk_relation_get_target| a)))
(progn
 (defalien "atk_relation_set_get_type" GType)
 (defun atk::relation-set-get-type () (|atk_relation_set_get_type|)))
(progn
 (defalien "atk_relation_set_new" (* t))
 (defun atk::relation-set-new () (|atk_relation_set_new|)))
(progn
 (defalien "atk_relation_set_contains" gboolean (a (* t)) (b AtkRelationType))
 (defun atk::relation-set-contains (a b)
   (let ((v334 (|atk_relation_set_contains| a b)))
     (if (= v334 1) t nil))))
(progn
 (defalien "atk_relation_set_remove" void (a (* t)) (b (* t)))
 (defun atk::relation-set-remove (a b) (|atk_relation_set_remove| a b)))
(progn
 (defalien "atk_relation_set_add" void (a (* t)) (b (* t)))
 (defun atk::relation-set-add (a b) (|atk_relation_set_add| a b)))
(progn
 (defalien "atk_relation_set_get_n_relations" gint (a (* t)))
 (defun atk::relation-set-get-n-relations (a)
   (|atk_relation_set_get_n_relations| a)))
(progn
 (defalien "atk_relation_set_get_relation" (* t) (a (* t)) (b gint))
 (defun atk::relation-set-get-relation (a b)
   (|atk_relation_set_get_relation| a b)))
(progn
 (defalien "atk_relation_set_get_relation_by_type"
           (* t)
           (a (* t))
           (b AtkRelationType))
 (defun atk::relation-set-get-relation-by-type (a b)
   (|atk_relation_set_get_relation_by_type| a b)))
(progn
 (defalien "atk_selection_get_type" GType)
 (defun atk::selection-get-type () (|atk_selection_get_type|)))
(progn
 (defalien "atk_selection_add_selection" gboolean (a (* t)) (b gint))
 (defun atk::selection-add-selection (a b)
   (let ((v335 (|atk_selection_add_selection| a b)))
     (if (= v335 1) t nil))))
(progn
 (defalien "atk_selection_clear_selection" gboolean (a (* t)))
 (defun atk::selection-clear-selection (a)
   (let ((v336 (|atk_selection_clear_selection| a)))
     (if (= v336 1) t nil))))
(progn
 (defalien "atk_selection_ref_selection" (* t) (a (* t)) (b gint))
 (defun atk::selection-ref-selection (a b) (|atk_selection_ref_selection| a b)))
(progn
 (defalien "atk_selection_get_selection_count" gint (a (* t)))
 (defun atk::selection-get-selection-count (a)
   (|atk_selection_get_selection_count| a)))
(progn
 (defalien "atk_selection_is_child_selected" gboolean (a (* t)) (b gint))
 (defun atk::selection-is-child-selected (a b)
   (let ((v337 (|atk_selection_is_child_selected| a b)))
     (if (= v337 1) t nil))))
(progn
 (defalien "atk_selection_remove_selection" gboolean (a (* t)) (b gint))
 (defun atk::selection-remove-selection (a b)
   (let ((v338 (|atk_selection_remove_selection| a b)))
     (if (= v338 1) t nil))))
(progn
 (defalien "atk_selection_select_all_selection" gboolean (a (* t)))
 (defun atk::selection-select-all-selection (a)
   (let ((v339 (|atk_selection_select_all_selection| a)))
     (if (= v339 1) t nil))))
(progn
 (defalien "atk_state_set_get_type" GType)
 (defun atk::state-set-get-type () (|atk_state_set_get_type|)))
(progn
 (defalien "atk_state_set_new" (* t))
 (defun atk::state-set-new () (|atk_state_set_new|)))
(progn
 (defalien "atk_state_set_is_empty" gboolean (a (* t)))
 (defun atk::state-set-is-empty (a)
   (let ((v340 (|atk_state_set_is_empty| a)))
     (if (= v340 1) t nil))))
(progn
 (defalien "atk_state_set_add_state" gboolean (a (* t)) (b AtkStateType))
 (defun atk::state-set-add-state (a b)
   (let ((v341 (|atk_state_set_add_state| a b)))
     (if (= v341 1) t nil))))
(progn
 (defalien "atk_state_set_add_states"
           void
           (a (* t))
           (b AtkStateType :in-out)
           (c gint))
 (defun atk::state-set-add-states (a b c) (|atk_state_set_add_states| a b c)))
(progn
 (defalien "atk_state_set_clear_states" void (a (* t)))
 (defun atk::state-set-clear-states (a) (|atk_state_set_clear_states| a)))
(progn
 (defalien "atk_state_set_contains_state" gboolean (a (* t)) (b AtkStateType))
 (defun atk::state-set-contains-state (a b)
   (let ((v342 (|atk_state_set_contains_state| a b)))
     (if (= v342 1) t nil))))
(progn
 (defalien "atk_state_set_contains_states"
           gboolean
           (a (* t))
           (b AtkStateType :in-out)
           (c gint))
 (defun atk::state-set-contains-states (a b c)
   (let ((v343 (multiple-value-list (|atk_state_set_contains_states| a b c))))
     (apply #'values (if (= 1 (car v343)) t nil) (cdr v343)))))
(progn
 (defalien "atk_state_set_remove_state" gboolean (a (* t)) (b AtkStateType))
 (defun atk::state-set-remove-state (a b)
   (let ((v344 (|atk_state_set_remove_state| a b)))
     (if (= v344 1) t nil))))
(progn
 (defalien "atk_state_set_and_sets" (* t) (a (* t)) (b (* t)))
 (defun atk::state-set-and-sets (a b) (|atk_state_set_and_sets| a b)))
(progn
 (defalien "atk_state_set_or_sets" (* t) (a (* t)) (b (* t)))
 (defun atk::state-set-or-sets (a b) (|atk_state_set_or_sets| a b)))
(progn
 (defalien "atk_state_set_xor_sets" (* t) (a (* t)) (b (* t)))
 (defun atk::state-set-xor-sets (a b) (|atk_state_set_xor_sets| a b)))
(progn
 (defalien "atk_streamable_content_get_type" GType)
 (defun atk::streamable-content-get-type ()
   (|atk_streamable_content_get_type|)))
(progn
 (defalien "atk_streamable_content_get_n_mime_types" gint (a (* t)))
 (defun atk::streamable-content-get-n-mime-types (a)
   (|atk_streamable_content_get_n_mime_types| a)))
(progn
 (defalien "atk_streamable_content_get_mime_type" c-string (a (* t)) (b gint))
 (defun atk::streamable-content-get-mime-type (a b)
   (|atk_streamable_content_get_mime_type| a b)))
(progn
 (defalien "atk_streamable_content_get_stream" (* t) (a (* t)) (b c-string))
 (defun atk::streamable-content-get-stream (a b)
   (|atk_streamable_content_get_stream| a b)))
(progn
 (defalien "atk_table_get_type" GType)
 (defun atk::table-get-type () (|atk_table_get_type|)))
(progn
 (defalien "atk_table_ref_at" (* t) (a (* t)) (b gint) (c gint))
 (defun atk::table-ref-at (a b c) (|atk_table_ref_at| a b c)))
(progn
 (defalien "atk_table_get_index_at" gint (a (* t)) (b gint) (c gint))
 (defun atk::table-get-index-at (a b c) (|atk_table_get_index_at| a b c)))
(progn
 (defalien "atk_table_get_column_at_index" gint (a (* t)) (b gint))
 (defun atk::table-get-column-at-index (a b)
   (|atk_table_get_column_at_index| a b)))
(progn
 (defalien "atk_table_get_row_at_index" gint (a (* t)) (b gint))
 (defun atk::table-get-row-at-index (a b) (|atk_table_get_row_at_index| a b)))
(progn
 (defalien "atk_table_get_n_columns" gint (a (* t)))
 (defun atk::table-get-n-columns (a) (|atk_table_get_n_columns| a)))
(progn
 (defalien "atk_table_get_n_rows" gint (a (* t)))
 (defun atk::table-get-n-rows (a) (|atk_table_get_n_rows| a)))
(progn
 (defalien "atk_table_get_column_extent_at" gint (a (* t)) (b gint) (c gint))
 (defun atk::table-get-column-extent-at (a b c)
   (|atk_table_get_column_extent_at| a b c)))
(progn
 (defalien "atk_table_get_row_extent_at" gint (a (* t)) (b gint) (c gint))
 (defun atk::table-get-row-extent-at (a b c)
   (|atk_table_get_row_extent_at| a b c)))
(progn
 (defalien "atk_table_get_caption" (* t) (a (* t)))
 (defun atk::table-get-caption (a) (|atk_table_get_caption| a)))
(progn
 (defalien "atk_table_get_column_description" c-string (a (* t)) (b gint))
 (defun atk::table-get-column-description (a b)
   (|atk_table_get_column_description| a b)))
(progn
 (defalien "atk_table_get_column_header" (* t) (a (* t)) (b gint))
 (defun atk::table-get-column-header (a b) (|atk_table_get_column_header| a b)))
(progn
 (defalien "atk_table_get_row_description" c-string (a (* t)) (b gint))
 (defun atk::table-get-row-description (a b)
   (|atk_table_get_row_description| a b)))
(progn
 (defalien "atk_table_get_row_header" (* t) (a (* t)) (b gint))
 (defun atk::table-get-row-header (a b) (|atk_table_get_row_header| a b)))
(progn
 (defalien "atk_table_get_summary" (* t) (a (* t)))
 (defun atk::table-get-summary (a) (|atk_table_get_summary| a)))
(progn
 (defalien "atk_table_set_caption" void (a (* t)) (b (* t)))
 (defun atk::table-set-caption (a b) (|atk_table_set_caption| a b)))
(progn
 (defalien "atk_table_set_column_description"
           void
           (a (* t))
           (b gint)
           (c c-string))
 (defun atk::table-set-column-description (a b c)
   (|atk_table_set_column_description| a b c)))
(progn
 (defalien "atk_table_set_column_header" void (a (* t)) (b gint) (c (* t)))
 (defun atk::table-set-column-header (a b c)
   (|atk_table_set_column_header| a b c)))
(progn
 (defalien "atk_table_set_row_description"
           void
           (a (* t))
           (b gint)
           (c c-string))
 (defun atk::table-set-row-description (a b c)
   (|atk_table_set_row_description| a b c)))
(progn
 (defalien "atk_table_set_row_header" void (a (* t)) (b gint) (c (* t)))
 (defun atk::table-set-row-header (a b c) (|atk_table_set_row_header| a b c)))
(progn
 (defalien "atk_table_set_summary" void (a (* t)) (b (* t)))
 (defun atk::table-set-summary (a b) (|atk_table_set_summary| a b)))
(progn
 (defalien "atk_table_get_selected_columns" gint (a (* t)) (b (* t)))
 (defun atk::table-get-selected-columns (a b)
   (|atk_table_get_selected_columns| a b)))
(progn
 (defalien "atk_table_get_selected_rows" gint (a (* t)) (b (* t)))
 (defun atk::table-get-selected-rows (a b) (|atk_table_get_selected_rows| a b)))
(progn
 (defalien "atk_table_is_column_selected" gboolean (a (* t)) (b gint))
 (defun atk::table-is-column-selected (a b)
   (let ((v345 (|atk_table_is_column_selected| a b)))
     (if (= v345 1) t nil))))
(progn
 (defalien "atk_table_is_row_selected" gboolean (a (* t)) (b gint))
 (defun atk::table-is-row-selected (a b)
   (let ((v346 (|atk_table_is_row_selected| a b)))
     (if (= v346 1) t nil))))
(progn
 (defalien "atk_table_is_selected" gboolean (a (* t)) (b gint) (c gint))
 (defun atk::table-is-selected (a b c)
   (let ((v347 (|atk_table_is_selected| a b c)))
     (if (= v347 1) t nil))))
(progn
 (defalien "atk_table_add_row_selection" gboolean (a (* t)) (b gint))
 (defun atk::table-add-row-selection (a b)
   (let ((v348 (|atk_table_add_row_selection| a b)))
     (if (= v348 1) t nil))))
(progn
 (defalien "atk_table_remove_row_selection" gboolean (a (* t)) (b gint))
 (defun atk::table-remove-row-selection (a b)
   (let ((v349 (|atk_table_remove_row_selection| a b)))
     (if (= v349 1) t nil))))
(progn
 (defalien "atk_table_add_column_selection" gboolean (a (* t)) (b gint))
 (defun atk::table-add-column-selection (a b)
   (let ((v350 (|atk_table_add_column_selection| a b)))
     (if (= v350 1) t nil))))
(progn
 (defalien "atk_table_remove_column_selection" gboolean (a (* t)) (b gint))
 (defun atk::table-remove-column-selection (a b)
   (let ((v351 (|atk_table_remove_column_selection| a b)))
     (if (= v351 1) t nil))))
(progn
 (defalien "atk_value_get_type" GType)
 (defun atk::value-get-type () (|atk_value_get_type|)))
(progn
 (defalien "atk_value_get_current_value" void (a (* t)) (b (* t)))
 (defun atk::value-get-current-value (a b) (|atk_value_get_current_value| a b)))
(progn
 (defalien "atk_value_get_maximum_value" void (a (* t)) (b (* t)))
 (defun atk::value-get-maximum-value (a b) (|atk_value_get_maximum_value| a b)))
(progn
 (defalien "atk_value_get_minimum_value" void (a (* t)) (b (* t)))
 (defun atk::value-get-minimum-value (a b) (|atk_value_get_minimum_value| a b)))
(progn
 (defalien "atk_value_set_current_value" gboolean (a (* t)) (b (* t)))
 (defun atk::value-set-current-value (a b)
   (let ((v352 (|atk_value_set_current_value| a b)))
     (if (= v352 1) t nil))))
(progn
 (defalien "gtk_accessible_get_type" GType)
 (defun gtk::accessible-get-type () (|gtk_accessible_get_type|)))
(progn
 (defalien "gtk_accessible_connect_widget_destroyed" void (a (* t)))
 (defun gtk::accessible-connect-widget-destroyed (a)
   (|gtk_accessible_connect_widget_destroyed| a)))
(progn
 (defalien "gtk_alignment_get_type" GType)
 (defun gtk::alignment-get-type () (|gtk_alignment_get_type|)))
(progn
 (defalien "gtk_alignment_new"
           (* t)
           (a gfloat)
           (b gfloat)
           (c gfloat)
           (d gfloat))
 (defun gtk::alignment-new (a b c d)
   (|gtk_alignment_new| (coerce a 'single-float) (coerce b 'single-float)
    (coerce c 'single-float) (coerce d 'single-float))))
(progn
 (defalien "gtk_alignment_set"
           void
           (a (* t))
           (b gfloat)
           (c gfloat)
           (d gfloat)
           (e gfloat))
 (defun gtk::alignment-set (a b c d e)
   (|gtk_alignment_set| a (coerce b 'single-float) (coerce c 'single-float)
    (coerce d 'single-float) (coerce e 'single-float))))
(progn
 (defalien "gtk_frame_get_type" GType)
 (defun gtk::frame-get-type () (|gtk_frame_get_type|)))
(progn
 (defalien "gtk_frame_new" (* t) (a c-string))
 (defun gtk::frame-new (a) (|gtk_frame_new| a)))
(progn
 (defalien "gtk_frame_set_label" void (a (* t)) (b c-string))
 (defun gtk::frame-set-label (a b) (|gtk_frame_set_label| a b)))
(progn
 (defalien "gtk_frame_get_label" c-string (a (* t)))
 (defun gtk::frame-get-label (a) (|gtk_frame_get_label| a)))
(progn
 (defalien "gtk_frame_set_label_widget" void (a (* t)) (b (* t)))
 (defun gtk::frame-set-label-widget (a b) (|gtk_frame_set_label_widget| a b)))
(progn
 (defalien "gtk_frame_get_label_widget" (* t) (a (* t)))
 (defun gtk::frame-get-label-widget (a) (|gtk_frame_get_label_widget| a)))
(progn
 (defalien "gtk_frame_set_label_align" void (a (* t)) (b gfloat) (c gfloat))
 (defun gtk::frame-set-label-align (a b c)
   (|gtk_frame_set_label_align| a (coerce b 'single-float)
    (coerce c 'single-float))))
(progn
 (defalien "gtk_frame_get_label_align"
           void
           (a (* t))
           (b gfloat :in-out)
           (c gfloat :in-out))
 (defun gtk::frame-get-label-align (a b c)
   (|gtk_frame_get_label_align| a (coerce b 'single-float)
    (coerce c 'single-float))))
(progn
 (defalien "gtk_frame_set_shadow_type" void (a (* t)) (b GtkShadowType))
 (defun gtk::frame-set-shadow-type (a b) (|gtk_frame_set_shadow_type| a b)))
(progn
 (defalien "gtk_frame_get_shadow_type" GtkShadowType (a (* t)))
 (defun gtk::frame-get-shadow-type (a) (|gtk_frame_get_shadow_type| a)))
(progn
 (defalien "gtk_aspect_frame_get_type" GType)
 (defun gtk::aspect-frame-get-type () (|gtk_aspect_frame_get_type|)))
(progn
 (defalien "gtk_aspect_frame_new"
           (* t)
           (a c-string)
           (b gfloat)
           (c gfloat)
           (d gfloat)
           (e gboolean))
 (defun gtk::aspect-frame-new (a b c d e)
   (|gtk_aspect_frame_new| a (coerce b 'single-float) (coerce c 'single-float)
    (coerce d 'single-float) (if e (if (eq e 0) 0 1) 0))))
(progn
 (defalien "gtk_aspect_frame_set"
           void
           (a (* t))
           (b gfloat)
           (c gfloat)
           (d gfloat)
           (e gboolean))
 (defun gtk::aspect-frame-set (a b c d e)
   (|gtk_aspect_frame_set| a (coerce b 'single-float) (coerce c 'single-float)
    (coerce d 'single-float) (if e (if (eq e 0) 0 1) 0))))
(progn
 (defalien "gtk_arrow_get_type" GType)
 (defun gtk::arrow-get-type () (|gtk_arrow_get_type|)))
(progn
 (defalien "gtk_arrow_new" (* t) (a GtkArrowType) (b GtkShadowType))
 (defun gtk::arrow-new (a b) (|gtk_arrow_new| a b)))
(progn
 (defalien "gtk_arrow_set" void (a (* t)) (b GtkArrowType) (c GtkShadowType))
 (defun gtk::arrow-set (a b c) (|gtk_arrow_set| a b c)))
(progn
 (defalien "gtk_binding_set_new" (* t) (a c-string))
 (defun gtk::binding-set-new (a) (|gtk_binding_set_new| a)))
(progn
 (defalien "gtk_binding_set_by_class" (* t) (a gpointer))
 (defun gtk::binding-set-by-class (a) (|gtk_binding_set_by_class| a)))
(progn
 (defalien "gtk_binding_set_find" (* t) (a c-string))
 (defun gtk::binding-set-find (a) (|gtk_binding_set_find| a)))
(progn
 (defalien "gtk_bindings_activate"
           gboolean
           (a (* t))
           (b guint)
           (c GdkModifierType))
 (defun gtk::bindings-activate (a b c)
   (let ((v353 (|gtk_bindings_activate| a b c)))
     (if (= v353 1) t nil))))
(progn
 (defalien "gtk_binding_set_activate"
           gboolean
           (a (* t))
           (b guint)
           (c GdkModifierType)
           (d (* t)))
 (defun gtk::binding-set-activate (a b c d)
   (let ((v354 (|gtk_binding_set_activate| a b c d)))
     (if (= v354 1) t nil))))
(progn
 (defalien "gtk_binding_entry_clear"
           void
           (a (* t))
           (b guint)
           (c GdkModifierType))
 (defun gtk::binding-entry-clear (a b c) (|gtk_binding_entry_clear| a b c)))
(progn
 (defalien "gtk_binding_entry_add_signal"
           void
           (a (* t))
           (b guint)
           (c GdkModifierType)
           (d c-string)
           (e guint))
 (defun gtk::binding-entry-add-signal (a b c d e)
   (|gtk_binding_entry_add_signal| a b c d e)))
(progn
 (defalien "gtk_binding_set_add_path"
           void
           (a (* t))
           (b GtkPathType)
           (c c-string)
           (d GtkPathPriorityType))
 (defun gtk::binding-set-add-path (a b c d)
   (|gtk_binding_set_add_path| a b c d)))
(progn
 (defalien "gtk_binding_entry_remove"
           void
           (a (* t))
           (b guint)
           (c GdkModifierType))
 (defun gtk::binding-entry-remove (a b c) (|gtk_binding_entry_remove| a b c)))
(progn
 (defalien "gtk_binding_entry_add_signall"
           void
           (a (* t))
           (b guint)
           (c GdkModifierType)
           (d c-string)
           (e (* t)))
 (defun gtk::binding-entry-add-signall (a b c d e)
   (|gtk_binding_entry_add_signall| a b c d e)))
(progn
 (defalien "gtk_binding_parse_binding" guint (a (* t)))
 (defun gtk::binding-parse-binding (a) (|gtk_binding_parse_binding| a)))
(progn
 (defalien "gtk_box_get_type" GType)
 (defun gtk::box-get-type () (|gtk_box_get_type|)))
(progn
 (defalien "gtk_box_pack_start"
           void
           (a (* t))
           (b (* t))
           (c gboolean)
           (d gboolean)
           (e guint))
 (defun gtk::box-pack-start (a b c d e)
   (|gtk_box_pack_start| a b (if c (if (eq c 0) 0 1) 0)
    (if d (if (eq d 0) 0 1) 0) e)))
(progn
 (defalien "gtk_box_pack_end"
           void
           (a (* t))
           (b (* t))
           (c gboolean)
           (d gboolean)
           (e guint))
 (defun gtk::box-pack-end (a b c d e)
   (|gtk_box_pack_end| a b (if c (if (eq c 0) 0 1) 0)
    (if d (if (eq d 0) 0 1) 0) e)))
(progn
 (defalien "gtk_box_set_homogeneous" void (a (* t)) (b gboolean))
 (defun gtk::box-set-homogeneous (a b)
   (|gtk_box_set_homogeneous| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gtk_box_get_homogeneous" gboolean (a (* t)))
 (defun gtk::box-get-homogeneous (a)
   (let ((v355 (|gtk_box_get_homogeneous| a)))
     (if (= v355 1) t nil))))
(progn
 (defalien "gtk_box_set_spacing" void (a (* t)) (b gint))
 (defun gtk::box-set-spacing (a b) (|gtk_box_set_spacing| a b)))
(progn
 (defalien "gtk_box_get_spacing" gint (a (* t)))
 (defun gtk::box-get-spacing (a) (|gtk_box_get_spacing| a)))
(progn
 (defalien "gtk_box_reorder_child" void (a (* t)) (b (* t)) (c gint))
 (defun gtk::box-reorder-child (a b c) (|gtk_box_reorder_child| a b c)))
(progn
 (defalien "gtk_box_query_child_packing"
           void
           (a (* t))
           (b (* t))
           (c (* t))
           (d (* t))
           (e guint :in-out)
           (f GtkPackType :in-out))
 (defun gtk::box-query-child-packing (a b c d e f)
   (|gtk_box_query_child_packing| a b c d e f)))
(progn
 (defalien "gtk_box_set_child_packing"
           void
           (a (* t))
           (b (* t))
           (c gboolean)
           (d gboolean)
           (e guint)
           (f GtkPackType))
 (defun gtk::box-set-child-packing (a b c d e f)
   (|gtk_box_set_child_packing| a b (if c (if (eq c 0) 0 1) 0)
    (if d (if (eq d 0) 0 1) 0) e f)))
(progn
 (defalien "gtk_button_box_get_type" GType)
 (defun gtk::button-box-get-type () (|gtk_button_box_get_type|)))
(progn
 (defalien "gtk_button_box_get_layout" GtkButtonBoxStyle (a (* t)))
 (defun gtk::button-box-get-layout (a) (|gtk_button_box_get_layout| a)))
(progn
 (defalien "gtk_button_box_set_layout" void (a (* t)) (b GtkButtonBoxStyle))
 (defun gtk::button-box-set-layout (a b) (|gtk_button_box_set_layout| a b)))
(progn
 (defalien "gtk_button_box_set_child_secondary"
           void
           (a (* t))
           (b (* t))
           (c gboolean))
 (defun gtk::button-box-set-child-secondary (a b c)
   (|gtk_button_box_set_child_secondary| a b (if c (if (eq c 0) 0 1) 0))))
(progn
 (defalien "gtk_button_get_type" GType)
 (defun gtk::button-get-type () (|gtk_button_get_type|)))
(progn
 (defalien "gtk_button_new" (* t))
 (defun gtk::button-new () (|gtk_button_new|)))
(progn
 (defalien "gtk_button_new_with_label" (* t) (a c-string))
 (defun gtk::button-new-with-label (a) (|gtk_button_new_with_label| a)))
(progn
 (defalien "gtk_button_new_from_stock" (* t) (a c-string))
 (defun gtk::button-new-from-stock (a) (|gtk_button_new_from_stock| a)))
(progn
 (defalien "gtk_button_new_with_mnemonic" (* t) (a c-string))
 (defun gtk::button-new-with-mnemonic (a) (|gtk_button_new_with_mnemonic| a)))
(progn
 (defalien "gtk_button_pressed" void (a (* t)))
 (defun gtk::button-pressed (a) (|gtk_button_pressed| a)))
(progn
 (defalien "gtk_button_released" void (a (* t)))
 (defun gtk::button-released (a) (|gtk_button_released| a)))
(progn
 (defalien "gtk_button_clicked" void (a (* t)))
 (defun gtk::button-clicked (a) (|gtk_button_clicked| a)))
(progn
 (defalien "gtk_button_enter" void (a (* t)))
 (defun gtk::button-enter (a) (|gtk_button_enter| a)))
(progn
 (defalien "gtk_button_leave" void (a (* t)))
 (defun gtk::button-leave (a) (|gtk_button_leave| a)))
(progn
 (defalien "gtk_button_set_relief" void (a (* t)) (b GtkReliefStyle))
 (defun gtk::button-set-relief (a b) (|gtk_button_set_relief| a b)))
(progn
 (defalien "gtk_button_get_relief" GtkReliefStyle (a (* t)))
 (defun gtk::button-get-relief (a) (|gtk_button_get_relief| a)))
(progn
 (defalien "gtk_button_set_label" void (a (* t)) (b c-string))
 (defun gtk::button-set-label (a b) (|gtk_button_set_label| a b)))
(progn
 (defalien "gtk_button_get_label" c-string (a (* t)))
 (defun gtk::button-get-label (a) (|gtk_button_get_label| a)))
(progn
 (defalien "gtk_button_set_use_underline" void (a (* t)) (b gboolean))
 (defun gtk::button-set-use-underline (a b)
   (|gtk_button_set_use_underline| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gtk_button_get_use_underline" gboolean (a (* t)))
 (defun gtk::button-get-use-underline (a)
   (let ((v356 (|gtk_button_get_use_underline| a)))
     (if (= v356 1) t nil))))
(progn
 (defalien "gtk_button_set_use_stock" void (a (* t)) (b gboolean))
 (defun gtk::button-set-use-stock (a b)
   (|gtk_button_set_use_stock| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gtk_button_get_use_stock" gboolean (a (* t)))
 (defun gtk::button-get-use-stock (a)
   (let ((v357 (|gtk_button_get_use_stock| a)))
     (if (= v357 1) t nil))))
(progn
 (defalien "gtk_calendar_get_type" GType)
 (defun gtk::calendar-get-type () (|gtk_calendar_get_type|)))
(progn
 (defalien "gtk_calendar_new" (* t))
 (defun gtk::calendar-new () (|gtk_calendar_new|)))
(progn
 (defalien "gtk_calendar_select_month" gboolean (a (* t)) (b guint) (c guint))
 (defun gtk::calendar-select-month (a b c)
   (let ((v358 (|gtk_calendar_select_month| a b c)))
     (if (= v358 1) t nil))))
(progn
 (defalien "gtk_calendar_select_day" void (a (* t)) (b guint))
 (defun gtk::calendar-select-day (a b) (|gtk_calendar_select_day| a b)))
(progn
 (defalien "gtk_calendar_mark_day" gboolean (a (* t)) (b guint))
 (defun gtk::calendar-mark-day (a b)
   (let ((v359 (|gtk_calendar_mark_day| a b)))
     (if (= v359 1) t nil))))
(progn
 (defalien "gtk_calendar_unmark_day" gboolean (a (* t)) (b guint))
 (defun gtk::calendar-unmark-day (a b)
   (let ((v360 (|gtk_calendar_unmark_day| a b)))
     (if (= v360 1) t nil))))
(progn
 (defalien "gtk_calendar_clear_marks" void (a (* t)))
 (defun gtk::calendar-clear-marks (a) (|gtk_calendar_clear_marks| a)))
(progn
 (defalien "gtk_calendar_display_options"
           void
           (a (* t))
           (b GtkCalendarDisplayOptions))
 (defun gtk::calendar-display-options (a b)
   (|gtk_calendar_display_options| a b)))
(progn
 (defalien "gtk_calendar_get_date"
           void
           (a (* t))
           (b guint :in-out)
           (c guint :in-out)
           (d guint :in-out))
 (defun gtk::calendar-get-date (a b c d) (|gtk_calendar_get_date| a b c d)))
(progn
 (defalien "gtk_calendar_freeze" void (a (* t)))
 (defun gtk::calendar-freeze (a) (|gtk_calendar_freeze| a)))
(progn
 (defalien "gtk_calendar_thaw" void (a (* t)))
 (defun gtk::calendar-thaw (a) (|gtk_calendar_thaw| a)))
(progn
 (defalien "gtk_cell_editable_get_type" GType)
 (defun gtk::cell-editable-get-type () (|gtk_cell_editable_get_type|)))
(progn
 (defalien "gtk_cell_editable_start_editing" void (a (* t)) (b (* t)))
 (defun gtk::cell-editable-start-editing (a b)
   (|gtk_cell_editable_start_editing| a b)))
(progn
 (defalien "gtk_cell_editable_editing_done" void (a (* t)))
 (defun gtk::cell-editable-editing-done (a)
   (|gtk_cell_editable_editing_done| a)))
(progn
 (defalien "gtk_cell_editable_remove_widget" void (a (* t)))
 (defun gtk::cell-editable-remove-widget (a)
   (|gtk_cell_editable_remove_widget| a)))
(progn
 (defalien "gtk_cell_renderer_get_type" GType)
 (defun gtk::cell-renderer-get-type () (|gtk_cell_renderer_get_type|)))
(progn
 (defalien "gtk_cell_renderer_get_size"
           void
           (a (* t))
           (b (* t))
           (c (* t))
           (d gint :in-out)
           (e gint :in-out)
           (f gint :in-out)
           (g gint :in-out))
 (defun gtk::cell-renderer-get-size (a b c d e f g)
   (|gtk_cell_renderer_get_size| a b c d e f g)))
(progn
 (defalien "gtk_cell_renderer_render"
           void
           (a (* t))
           (b (* t))
           (c (* t))
           (d (* t))
           (e (* t))
           (f (* t))
           (g GtkCellRendererState))
 (defun gtk::cell-renderer-render (a b c d e f g)
   (|gtk_cell_renderer_render| a b c d e f g)))
(progn
 (defalien "gtk_cell_renderer_activate"
           gboolean
           (a (* t))
           (b (* t))
           (c (* t))
           (d c-string)
           (e (* t))
           (f (* t))
           (g GtkCellRendererState))
 (defun gtk::cell-renderer-activate (a b c d e f g)
   (let ((v361 (|gtk_cell_renderer_activate| a b c d e f g)))
     (if (= v361 1) t nil))))
(progn
 (defalien "gtk_cell_renderer_start_editing"
           (* t)
           (a (* t))
           (b (* t))
           (c (* t))
           (d c-string)
           (e (* t))
           (f (* t))
           (g GtkCellRendererState))
 (defun gtk::cell-renderer-start-editing (a b c d e f g)
   (|gtk_cell_renderer_start_editing| a b c d e f g)))
(progn
 (defalien "gtk_cell_renderer_set_fixed_size" void (a (* t)) (b gint) (c gint))
 (defun gtk::cell-renderer-set-fixed-size (a b c)
   (|gtk_cell_renderer_set_fixed_size| a b c)))
(progn
 (defalien "gtk_cell_renderer_get_fixed_size"
           void
           (a (* t))
           (b gint :in-out)
           (c gint :in-out))
 (defun gtk::cell-renderer-get-fixed-size (a b c)
   (|gtk_cell_renderer_get_fixed_size| a b c)))
(progn
 (defalien "gtk_cell_renderer_text_get_type" GType)
 (defun gtk::cell-renderer-text-get-type ()
   (|gtk_cell_renderer_text_get_type|)))
(progn
 (defalien "gtk_cell_renderer_text_new" (* t))
 (defun gtk::cell-renderer-text-new () (|gtk_cell_renderer_text_new|)))
(progn
 (defalien "gtk_cell_renderer_text_set_fixed_height_from_font"
           void
           (a (* t))
           (b gint))
 (defun gtk::cell-renderer-text-set-fixed-height-from-font (a b)
   (|gtk_cell_renderer_text_set_fixed_height_from_font| a b)))
(progn
 (defalien "gtk_cell_renderer_toggle_get_type" GType)
 (defun gtk::cell-renderer-toggle-get-type ()
   (|gtk_cell_renderer_toggle_get_type|)))
(progn
 (defalien "gtk_cell_renderer_toggle_new" (* t))
 (defun gtk::cell-renderer-toggle-new () (|gtk_cell_renderer_toggle_new|)))
(progn
 (defalien "gtk_cell_renderer_toggle_get_radio" gboolean (a (* t)))
 (defun gtk::cell-renderer-toggle-get-radio (a)
   (let ((v362 (|gtk_cell_renderer_toggle_get_radio| a)))
     (if (= v362 1) t nil))))
(progn
 (defalien "gtk_cell_renderer_toggle_set_radio" void (a (* t)) (b gboolean))
 (defun gtk::cell-renderer-toggle-set-radio (a b)
   (|gtk_cell_renderer_toggle_set_radio| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gtk_cell_renderer_toggle_get_active" gboolean (a (* t)))
 (defun gtk::cell-renderer-toggle-get-active (a)
   (let ((v363 (|gtk_cell_renderer_toggle_get_active| a)))
     (if (= v363 1) t nil))))
(progn
 (defalien "gtk_cell_renderer_toggle_set_active" void (a (* t)) (b gboolean))
 (defun gtk::cell-renderer-toggle-set-active (a b)
   (|gtk_cell_renderer_toggle_set_active| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gtk_cell_renderer_pixbuf_get_type" GType)
 (defun gtk::cell-renderer-pixbuf-get-type ()
   (|gtk_cell_renderer_pixbuf_get_type|)))
(progn
 (defalien "gtk_cell_renderer_pixbuf_new" (* t))
 (defun gtk::cell-renderer-pixbuf-new () (|gtk_cell_renderer_pixbuf_new|)))
(progn
 (defalien "gtk_toggle_button_get_type" GType)
 (defun gtk::toggle-button-get-type () (|gtk_toggle_button_get_type|)))
(progn
 (defalien "gtk_toggle_button_new" (* t))
 (defun gtk::toggle-button-new () (|gtk_toggle_button_new|)))
(progn
 (defalien "gtk_toggle_button_new_with_label" (* t) (a c-string))
 (defun gtk::toggle-button-new-with-label (a)
   (|gtk_toggle_button_new_with_label| a)))
(progn
 (defalien "gtk_toggle_button_new_with_mnemonic" (* t) (a c-string))
 (defun gtk::toggle-button-new-with-mnemonic (a)
   (|gtk_toggle_button_new_with_mnemonic| a)))
(progn
 (defalien "gtk_toggle_button_set_mode" void (a (* t)) (b gboolean))
 (defun gtk::toggle-button-set-mode (a b)
   (|gtk_toggle_button_set_mode| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gtk_toggle_button_get_mode" gboolean (a (* t)))
 (defun gtk::toggle-button-get-mode (a)
   (let ((v364 (|gtk_toggle_button_get_mode| a)))
     (if (= v364 1) t nil))))
(progn
 (defalien "gtk_toggle_button_set_active" void (a (* t)) (b gboolean))
 (defun gtk::toggle-button-set-active (a b)
   (|gtk_toggle_button_set_active| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gtk_toggle_button_get_active" gboolean (a (* t)))
 (defun gtk::toggle-button-get-active (a)
   (let ((v365 (|gtk_toggle_button_get_active| a)))
     (if (= v365 1) t nil))))
(progn
 (defalien "gtk_toggle_button_toggled" void (a (* t)))
 (defun gtk::toggle-button-toggled (a) (|gtk_toggle_button_toggled| a)))
(progn
 (defalien "gtk_toggle_button_set_inconsistent" void (a (* t)) (b gboolean))
 (defun gtk::toggle-button-set-inconsistent (a b)
   (|gtk_toggle_button_set_inconsistent| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gtk_toggle_button_get_inconsistent" gboolean (a (* t)))
 (defun gtk::toggle-button-get-inconsistent (a)
   (let ((v366 (|gtk_toggle_button_get_inconsistent| a)))
     (if (= v366 1) t nil))))
(progn
 (defalien "gtk_check_button_get_type" GType)
 (defun gtk::check-button-get-type () (|gtk_check_button_get_type|)))
(progn
 (defalien "gtk_check_button_new" (* t))
 (defun gtk::check-button-new () (|gtk_check_button_new|)))
(progn
 (defalien "gtk_check_button_new_with_label" (* t) (a c-string))
 (defun gtk::check-button-new-with-label (a)
   (|gtk_check_button_new_with_label| a)))
(progn
 (defalien "gtk_check_button_new_with_mnemonic" (* t) (a c-string))
 (defun gtk::check-button-new-with-mnemonic (a)
   (|gtk_check_button_new_with_mnemonic| a)))
(progn
 (defalien "gtk_item_get_type" GType)
 (defun gtk::item-get-type () (|gtk_item_get_type|)))
(progn
 (defalien "gtk_item_select" void (a (* t)))
 (defun gtk::item-select (a) (|gtk_item_select| a)))
(progn
 (defalien "gtk_item_deselect" void (a (* t)))
 (defun gtk::item-deselect (a) (|gtk_item_deselect| a)))
(progn
 (defalien "gtk_item_toggle" void (a (* t)))
 (defun gtk::item-toggle (a) (|gtk_item_toggle| a)))
(progn
 (defalien "gtk_menu_item_get_type" GType)
 (defun gtk::menu-item-get-type () (|gtk_menu_item_get_type|)))
(progn
 (defalien "gtk_menu_item_new" (* t))
 (defun gtk::menu-item-new () (|gtk_menu_item_new|)))
(progn
 (defalien "gtk_menu_item_new_with_label" (* t) (a c-string))
 (defun gtk::menu-item-new-with-label (a) (|gtk_menu_item_new_with_label| a)))
(progn
 (defalien "gtk_menu_item_new_with_mnemonic" (* t) (a c-string))
 (defun gtk::menu-item-new-with-mnemonic (a)
   (|gtk_menu_item_new_with_mnemonic| a)))
(progn
 (defalien "gtk_menu_item_set_submenu" void (a (* t)) (b (* t)))
 (defun gtk::menu-item-set-submenu (a b) (|gtk_menu_item_set_submenu| a b)))
(progn
 (defalien "gtk_menu_item_get_submenu" (* t) (a (* t)))
 (defun gtk::menu-item-get-submenu (a) (|gtk_menu_item_get_submenu| a)))
(progn
 (defalien "gtk_menu_item_remove_submenu" void (a (* t)))
 (defun gtk::menu-item-remove-submenu (a) (|gtk_menu_item_remove_submenu| a)))
(progn
 (defalien "gtk_menu_item_select" void (a (* t)))
 (defun gtk::menu-item-select (a) (|gtk_menu_item_select| a)))
(progn
 (defalien "gtk_menu_item_deselect" void (a (* t)))
 (defun gtk::menu-item-deselect (a) (|gtk_menu_item_deselect| a)))
(progn
 (defalien "gtk_menu_item_activate" void (a (* t)))
 (defun gtk::menu-item-activate (a) (|gtk_menu_item_activate| a)))
(progn
 (defalien "gtk_menu_item_toggle_size_request" void (a (* t)) (b gint :in-out))
 (defun gtk::menu-item-toggle-size-request (a b)
   (|gtk_menu_item_toggle_size_request| a b)))
(progn
 (defalien "gtk_menu_item_toggle_size_allocate" void (a (* t)) (b gint))
 (defun gtk::menu-item-toggle-size-allocate (a b)
   (|gtk_menu_item_toggle_size_allocate| a b)))
(progn
 (defalien "gtk_menu_item_set_right_justified" void (a (* t)) (b gboolean))
 (defun gtk::menu-item-set-right-justified (a b)
   (|gtk_menu_item_set_right_justified| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gtk_menu_item_get_right_justified" gboolean (a (* t)))
 (defun gtk::menu-item-get-right-justified (a)
   (let ((v367 (|gtk_menu_item_get_right_justified| a)))
     (if (= v367 1) t nil))))
(progn
 (defalien "gtk_menu_item_set_accel_path" void (a (* t)) (b c-string))
 (defun gtk::menu-item-set-accel-path (a b)
   (|gtk_menu_item_set_accel_path| a b)))
(progn
 (defalien "gtk_check_menu_item_get_type" GType)
 (defun gtk::check-menu-item-get-type () (|gtk_check_menu_item_get_type|)))
(progn
 (defalien "gtk_check_menu_item_new" (* t))
 (defun gtk::check-menu-item-new () (|gtk_check_menu_item_new|)))
(progn
 (defalien "gtk_check_menu_item_new_with_label" (* t) (a c-string))
 (defun gtk::check-menu-item-new-with-label (a)
   (|gtk_check_menu_item_new_with_label| a)))
(progn
 (defalien "gtk_check_menu_item_new_with_mnemonic" (* t) (a c-string))
 (defun gtk::check-menu-item-new-with-mnemonic (a)
   (|gtk_check_menu_item_new_with_mnemonic| a)))
(progn
 (defalien "gtk_check_menu_item_set_active" void (a (* t)) (b gboolean))
 (defun gtk::check-menu-item-set-active (a b)
   (|gtk_check_menu_item_set_active| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gtk_check_menu_item_get_active" gboolean (a (* t)))
 (defun gtk::check-menu-item-get-active (a)
   (let ((v368 (|gtk_check_menu_item_get_active| a)))
     (if (= v368 1) t nil))))
(progn
 (defalien "gtk_check_menu_item_toggled" void (a (* t)))
 (defun gtk::check-menu-item-toggled (a) (|gtk_check_menu_item_toggled| a)))
(progn
 (defalien "gtk_check_menu_item_set_inconsistent" void (a (* t)) (b gboolean))
 (defun gtk::check-menu-item-set-inconsistent (a b)
   (|gtk_check_menu_item_set_inconsistent| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gtk_check_menu_item_get_inconsistent" gboolean (a (* t)))
 (defun gtk::check-menu-item-get-inconsistent (a)
   (let ((v369 (|gtk_check_menu_item_get_inconsistent| a)))
     (if (= v369 1) t nil))))
(progn
 (defalien "gtk_target_list_new" (* t) (a (* t)) (b guint))
 (defun gtk::target-list-new (a b) (|gtk_target_list_new| a b)))
(progn
 (defalien "gtk_target_list_ref" void (a (* t)))
 (defun gtk::target-list-ref (a) (|gtk_target_list_ref| a)))
(progn
 (defalien "gtk_target_list_unref" void (a (* t)))
 (defun gtk::target-list-unref (a) (|gtk_target_list_unref| a)))
(progn
 (defalien "gtk_target_list_add"
           void
           (a (* t))
           (b GdkAtom)
           (c guint)
           (d guint))
 (defun gtk::target-list-add (a b c d) (|gtk_target_list_add| a b c d)))
(progn
 (defalien "gtk_target_list_add_table" void (a (* t)) (b (* t)) (c guint))
 (defun gtk::target-list-add-table (a b c) (|gtk_target_list_add_table| a b c)))
(progn
 (defalien "gtk_target_list_remove" void (a (* t)) (b GdkAtom))
 (defun gtk::target-list-remove (a b) (|gtk_target_list_remove| a b)))
(progn
 (defalien "gtk_target_list_find"
           gboolean
           (a (* t))
           (b GdkAtom)
           (c guint :in-out))
 (defun gtk::target-list-find (a b c)
   (let ((v370 (multiple-value-list (|gtk_target_list_find| a b c))))
     (apply #'values (if (= 1 (car v370)) t nil) (cdr v370)))))
(progn
 (defalien "gtk_selection_owner_set"
           gboolean
           (a (* t))
           (b GdkAtom)
           (c guint32))
 (defun gtk::selection-owner-set (a b c)
   (let ((v371 (|gtk_selection_owner_set| a b c)))
     (if (= v371 1) t nil))))
(progn
 (defalien "gtk_selection_add_target"
           void
           (a (* t))
           (b GdkAtom)
           (c GdkAtom)
           (d guint))
 (defun gtk::selection-add-target (a b c d)
   (|gtk_selection_add_target| a b c d)))
(progn
 (defalien "gtk_selection_add_targets"
           void
           (a (* t))
           (b GdkAtom)
           (c (* t))
           (d guint))
 (defun gtk::selection-add-targets (a b c d)
   (|gtk_selection_add_targets| a b c d)))
(progn
 (defalien "gtk_selection_clear_targets" void (a (* t)) (b GdkAtom))
 (defun gtk::selection-clear-targets (a b) (|gtk_selection_clear_targets| a b)))
(progn
 (defalien "gtk_selection_convert"
           gboolean
           (a (* t))
           (b GdkAtom)
           (c GdkAtom)
           (d guint32))
 (defun gtk::selection-convert (a b c d)
   (let ((v372 (|gtk_selection_convert| a b c d)))
     (if (= v372 1) t nil))))
(progn
 (defalien "gtk_selection_data_set"
           void
           (a (* t))
           (b GdkAtom)
           (c gint)
           (d unsigned-char :in-out)
           (e gint))
 (defun gtk::selection-data-set (a b c d e)
   (|gtk_selection_data_set| a b c d e)))
(progn
 (defalien "gtk_selection_data_set_text"
           gboolean
           (a (* t))
           (b c-string)
           (c gint))
 (defun gtk::selection-data-set-text (a b c)
   (let ((v373 (|gtk_selection_data_set_text| a b c)))
     (if (= v373 1) t nil))))
(progn
 (defalien "gtk_selection_data_get_text" guchar (a (* t)))
 (defun gtk::selection-data-get-text (a) (|gtk_selection_data_get_text| a)))
(progn
 (defalien "gtk_selection_data_get_targets"
           gboolean
           (a (* t))
           (b (* t))
           (c gint :in-out))
 (defun gtk::selection-data-get-targets (a b c)
   (let ((v374 (multiple-value-list (|gtk_selection_data_get_targets| a b c))))
     (apply #'values (if (= 1 (car v374)) t nil) (cdr v374)))))
(progn
 (defalien "gtk_selection_data_targets_include_text" gboolean (a (* t)))
 (defun gtk::selection-data-targets-include-text (a)
   (let ((v375 (|gtk_selection_data_targets_include_text| a)))
     (if (= v375 1) t nil))))
(progn
 (defalien "gtk_selection_remove_all" void (a (* t)))
 (defun gtk::selection-remove-all (a) (|gtk_selection_remove_all| a)))
(progn
 (defalien "gtk_selection_clear" gboolean (a (* t)) (b (* t)))
 (defun gtk::selection-clear (a b)
   (let ((v376 (|gtk_selection_clear| a b)))
     (if (= v376 1) t nil))))
(progn
 (defalien "gtk_selection_data_copy" (* t) (a (* t)))
 (defun gtk::selection-data-copy (a) (|gtk_selection_data_copy| a)))
(progn
 (defalien "gtk_selection_data_free" void (a (* t)))
 (defun gtk::selection-data-free (a) (|gtk_selection_data_free| a)))
(progn
 (defalien "gtk_clipboard_set_with_data"
           gboolean
           (a (* t))
           (b (* t))
           (c guint)
           (d GtkClipboardGetFunc)
           (e GtkClipboardClearFunc)
           (f gpointer))
 (defun gtk::clipboard-set-with-data (a b c d e f)
   (let ((v377 (|gtk_clipboard_set_with_data| a b c d e f)))
     (if (= v377 1) t nil))))
(progn
 (defalien "gtk_clipboard_set_with_owner"
           gboolean
           (a (* t))
           (b (* t))
           (c guint)
           (d GtkClipboardGetFunc)
           (e GtkClipboardClearFunc)
           (f (* t)))
 (defun gtk::clipboard-set-with-owner (a b c d e f)
   (let ((v378 (|gtk_clipboard_set_with_owner| a b c d e f)))
     (if (= v378 1) t nil))))
(progn
 (defalien "gtk_clipboard_get_owner" (* t) (a (* t)))
 (defun gtk::clipboard-get-owner (a) (|gtk_clipboard_get_owner| a)))
(progn
 (defalien "gtk_clipboard_clear" void (a (* t)))
 (defun gtk::clipboard-clear (a) (|gtk_clipboard_clear| a)))
(progn
 (defalien "gtk_clipboard_set_text" void (a (* t)) (b c-string) (c gint))
 (defun gtk::clipboard-set-text (a b c) (|gtk_clipboard_set_text| a b c)))
(progn
 (defalien "gtk_clipboard_request_contents"
           void
           (a (* t))
           (b GdkAtom)
           (c GtkClipboardReceivedFunc)
           (d gpointer))
 (defun gtk::clipboard-request-contents (a b c d)
   (|gtk_clipboard_request_contents| a b c d)))
(progn
 (defalien "gtk_clipboard_request_text"
           void
           (a (* t))
           (b GtkClipboardTextReceivedFunc)
           (c gpointer))
 (defun gtk::clipboard-request-text (a b c)
   (|gtk_clipboard_request_text| a b c)))
(progn
 (defalien "gtk_clipboard_wait_for_contents" (* t) (a (* t)) (b GdkAtom))
 (defun gtk::clipboard-wait-for-contents (a b)
   (|gtk_clipboard_wait_for_contents| a b)))
(progn
 (defalien "gtk_clipboard_wait_for_text" gchar (a (* t)))
 (defun gtk::clipboard-wait-for-text (a) (|gtk_clipboard_wait_for_text| a)))
(progn
 (defalien "gtk_clipboard_wait_is_text_available" gboolean (a (* t)))
 (defun gtk::clipboard-wait-is-text-available (a)
   (let ((v379 (|gtk_clipboard_wait_is_text_available| a)))
     (if (= v379 1) t nil))))
(progn
 (defalien "gtk_range_get_type" GType)
 (defun gtk::range-get-type () (|gtk_range_get_type|)))
(progn
 (defalien "gtk_range_set_update_policy" void (a (* t)) (b GtkUpdateType))
 (defun gtk::range-set-update-policy (a b) (|gtk_range_set_update_policy| a b)))
(progn
 (defalien "gtk_range_get_update_policy" GtkUpdateType (a (* t)))
 (defun gtk::range-get-update-policy (a) (|gtk_range_get_update_policy| a)))
(progn
 (defalien "gtk_range_set_adjustment" void (a (* t)) (b (* t)))
 (defun gtk::range-set-adjustment (a b) (|gtk_range_set_adjustment| a b)))
(progn
 (defalien "gtk_range_get_adjustment" (* t) (a (* t)))
 (defun gtk::range-get-adjustment (a) (|gtk_range_get_adjustment| a)))
(progn
 (defalien "gtk_range_set_inverted" void (a (* t)) (b gboolean))
 (defun gtk::range-set-inverted (a b)
   (|gtk_range_set_inverted| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gtk_range_get_inverted" gboolean (a (* t)))
 (defun gtk::range-get-inverted (a)
   (let ((v380 (|gtk_range_get_inverted| a)))
     (if (= v380 1) t nil))))
(progn
 (defalien "gtk_range_set_increments" void (a (* t)) (b gdouble) (c gdouble))
 (defun gtk::range-set-increments (a b c)
   (|gtk_range_set_increments| a (coerce b 'double-float)
    (coerce c 'double-float))))
(progn
 (defalien "gtk_range_set_range" void (a (* t)) (b gdouble) (c gdouble))
 (defun gtk::range-set-range (a b c)
   (|gtk_range_set_range| a (coerce b 'double-float) (coerce c 'double-float))))
(progn
 (defalien "gtk_range_set_value" void (a (* t)) (b gdouble))
 (defun gtk::range-set-value (a b)
   (|gtk_range_set_value| a (coerce b 'double-float))))
(progn
 (defalien "gtk_range_get_value" gdouble (a (* t)))
 (defun gtk::range-get-value (a) (|gtk_range_get_value| a)))
(progn
 (defalien "gtk_scrollbar_get_type" GType)
 (defun gtk::scrollbar-get-type () (|gtk_scrollbar_get_type|)))
(progn
 (defalien "gtk_hscrollbar_get_type" GType)
 (defun gtk::hscrollbar-get-type () (|gtk_hscrollbar_get_type|)))
(progn
 (defalien "gtk_hscrollbar_new" (* t) (a (* t)))
 (defun gtk::hscrollbar-new (a) (|gtk_hscrollbar_new| a)))
(progn
 (defalien "gtk_vscrollbar_get_type" GType)
 (defun gtk::vscrollbar-get-type () (|gtk_vscrollbar_get_type|)))
(progn
 (defalien "gtk_vscrollbar_new" (* t) (a (* t)))
 (defun gtk::vscrollbar-new (a) (|gtk_vscrollbar_new| a)))
(progn
 (defalien "gtk_clist_get_type" GtkType)
 (defun gtk::clist-get-type () (|gtk_clist_get_type|)))
(progn
 (defalien "gtk_clist_set_hadjustment" void (a (* t)) (b (* t)))
 (defun gtk::clist-set-hadjustment (a b) (|gtk_clist_set_hadjustment| a b)))
(progn
 (defalien "gtk_clist_set_vadjustment" void (a (* t)) (b (* t)))
 (defun gtk::clist-set-vadjustment (a b) (|gtk_clist_set_vadjustment| a b)))
(progn
 (defalien "gtk_clist_get_hadjustment" (* t) (a (* t)))
 (defun gtk::clist-get-hadjustment (a) (|gtk_clist_get_hadjustment| a)))
(progn
 (defalien "gtk_clist_get_vadjustment" (* t) (a (* t)))
 (defun gtk::clist-get-vadjustment (a) (|gtk_clist_get_vadjustment| a)))
(progn
 (defalien "gtk_clist_set_shadow_type" void (a (* t)) (b GtkShadowType))
 (defun gtk::clist-set-shadow-type (a b) (|gtk_clist_set_shadow_type| a b)))
(progn
 (defalien "gtk_clist_set_selection_mode" void (a (* t)) (b GtkSelectionMode))
 (defun gtk::clist-set-selection-mode (a b)
   (|gtk_clist_set_selection_mode| a b)))
(progn
 (defalien "gtk_clist_set_reorderable" void (a (* t)) (b gboolean))
 (defun gtk::clist-set-reorderable (a b)
   (|gtk_clist_set_reorderable| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gtk_clist_set_use_drag_icons" void (a (* t)) (b gboolean))
 (defun gtk::clist-set-use-drag-icons (a b)
   (|gtk_clist_set_use_drag_icons| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gtk_clist_set_button_actions" void (a (* t)) (b guint) (c guint8))
 (defun gtk::clist-set-button-actions (a b c)
   (|gtk_clist_set_button_actions| a b c)))
(progn
 (defalien "gtk_clist_freeze" void (a (* t)))
 (defun gtk::clist-freeze (a) (|gtk_clist_freeze| a)))
(progn
 (defalien "gtk_clist_thaw" void (a (* t)))
 (defun gtk::clist-thaw (a) (|gtk_clist_thaw| a)))
(progn
 (defalien "gtk_clist_column_titles_show" void (a (* t)))
 (defun gtk::clist-column-titles-show (a) (|gtk_clist_column_titles_show| a)))
(progn
 (defalien "gtk_clist_column_titles_hide" void (a (* t)))
 (defun gtk::clist-column-titles-hide (a) (|gtk_clist_column_titles_hide| a)))
(progn
 (defalien "gtk_clist_column_title_active" void (a (* t)) (b gint))
 (defun gtk::clist-column-title-active (a b)
   (|gtk_clist_column_title_active| a b)))
(progn
 (defalien "gtk_clist_column_title_passive" void (a (* t)) (b gint))
 (defun gtk::clist-column-title-passive (a b)
   (|gtk_clist_column_title_passive| a b)))
(progn
 (defalien "gtk_clist_column_titles_active" void (a (* t)))
 (defun gtk::clist-column-titles-active (a)
   (|gtk_clist_column_titles_active| a)))
(progn
 (defalien "gtk_clist_column_titles_passive" void (a (* t)))
 (defun gtk::clist-column-titles-passive (a)
   (|gtk_clist_column_titles_passive| a)))
(progn
 (defalien "gtk_clist_set_column_title" void (a (* t)) (b gint) (c c-string))
 (defun gtk::clist-set-column-title (a b c)
   (|gtk_clist_set_column_title| a b c)))
(progn
 (defalien "gtk_clist_get_column_title" gchar (a (* t)) (b gint))
 (defun gtk::clist-get-column-title (a b) (|gtk_clist_get_column_title| a b)))
(progn
 (defalien "gtk_clist_set_column_widget" void (a (* t)) (b gint) (c (* t)))
 (defun gtk::clist-set-column-widget (a b c)
   (|gtk_clist_set_column_widget| a b c)))
(progn
 (defalien "gtk_clist_get_column_widget" (* t) (a (* t)) (b gint))
 (defun gtk::clist-get-column-widget (a b) (|gtk_clist_get_column_widget| a b)))
(progn
 (defalien "gtk_clist_set_column_justification"
           void
           (a (* t))
           (b gint)
           (c GtkJustification))
 (defun gtk::clist-set-column-justification (a b c)
   (|gtk_clist_set_column_justification| a b c)))
(progn
 (defalien "gtk_clist_set_column_visibility"
           void
           (a (* t))
           (b gint)
           (c gboolean))
 (defun gtk::clist-set-column-visibility (a b c)
   (|gtk_clist_set_column_visibility| a b (if c (if (eq c 0) 0 1) 0))))
(progn
 (defalien "gtk_clist_set_column_resizeable"
           void
           (a (* t))
           (b gint)
           (c gboolean))
 (defun gtk::clist-set-column-resizeable (a b c)
   (|gtk_clist_set_column_resizeable| a b (if c (if (eq c 0) 0 1) 0))))
(progn
 (defalien "gtk_clist_set_column_auto_resize"
           void
           (a (* t))
           (b gint)
           (c gboolean))
 (defun gtk::clist-set-column-auto-resize (a b c)
   (|gtk_clist_set_column_auto_resize| a b (if c (if (eq c 0) 0 1) 0))))
(progn
 (defalien "gtk_clist_columns_autosize" gint (a (* t)))
 (defun gtk::clist-columns-autosize (a) (|gtk_clist_columns_autosize| a)))
(progn
 (defalien "gtk_clist_optimal_column_width" gint (a (* t)) (b gint))
 (defun gtk::clist-optimal-column-width (a b)
   (|gtk_clist_optimal_column_width| a b)))
(progn
 (defalien "gtk_clist_set_column_width" void (a (* t)) (b gint) (c gint))
 (defun gtk::clist-set-column-width (a b c)
   (|gtk_clist_set_column_width| a b c)))
(progn
 (defalien "gtk_clist_set_column_min_width" void (a (* t)) (b gint) (c gint))
 (defun gtk::clist-set-column-min-width (a b c)
   (|gtk_clist_set_column_min_width| a b c)))
(progn
 (defalien "gtk_clist_set_column_max_width" void (a (* t)) (b gint) (c gint))
 (defun gtk::clist-set-column-max-width (a b c)
   (|gtk_clist_set_column_max_width| a b c)))
(progn
 (defalien "gtk_clist_set_row_height" void (a (* t)) (b guint))
 (defun gtk::clist-set-row-height (a b) (|gtk_clist_set_row_height| a b)))
(progn
 (defalien "gtk_clist_moveto"
           void
           (a (* t))
           (b gint)
           (c gint)
           (d gfloat)
           (e gfloat))
 (defun gtk::clist-moveto (a b c d e)
   (|gtk_clist_moveto| a b c (coerce d 'single-float)
    (coerce e 'single-float))))
(progn
 (defalien "gtk_clist_row_is_visible" GtkVisibility (a (* t)) (b gint))
 (defun gtk::clist-row-is-visible (a b) (|gtk_clist_row_is_visible| a b)))
(progn
 (defalien "gtk_clist_get_cell_type" GtkCellType (a (* t)) (b gint) (c gint))
 (defun gtk::clist-get-cell-type (a b c) (|gtk_clist_get_cell_type| a b c)))
(progn
 (defalien "gtk_clist_set_text" void (a (* t)) (b gint) (c gint) (d c-string))
 (defun gtk::clist-set-text (a b c d) (|gtk_clist_set_text| a b c d)))
(progn
 (defalien "gtk_clist_get_text" gint (a (* t)) (b gint) (c gint) (d (* t)))
 (defun gtk::clist-get-text (a b c d) (|gtk_clist_get_text| a b c d)))
(progn
 (defalien "gtk_clist_set_pixmap"
           void
           (a (* t))
           (b gint)
           (c gint)
           (d (* t))
           (e (* t)))
 (defun gtk::clist-set-pixmap (a b c d e) (|gtk_clist_set_pixmap| a b c d e)))
(progn
 (defalien "gtk_clist_get_pixmap"
           gint
           (a (* t))
           (b gint)
           (c gint)
           (d (* t))
           (e (* t)))
 (defun gtk::clist-get-pixmap (a b c d e) (|gtk_clist_get_pixmap| a b c d e)))
(progn
 (defalien "gtk_clist_set_pixtext"
           void
           (a (* t))
           (b gint)
           (c gint)
           (d c-string)
           (e guint8)
           (f (* t))
           (g (* t)))
 (defun gtk::clist-set-pixtext (a b c d e f g)
   (|gtk_clist_set_pixtext| a b c d e f g)))
(progn
 (defalien "gtk_clist_get_pixtext"
           gint
           (a (* t))
           (b gint)
           (c gint)
           (d (* t))
           (e guint8 :in-out)
           (f (* t))
           (g (* t)))
 (defun gtk::clist-get-pixtext (a b c d e f g)
   (|gtk_clist_get_pixtext| a b c d e f g)))
(progn
 (defalien "gtk_clist_set_foreground" void (a (* t)) (b gint) (c (* t)))
 (defun gtk::clist-set-foreground (a b c) (|gtk_clist_set_foreground| a b c)))
(progn
 (defalien "gtk_clist_set_background" void (a (* t)) (b gint) (c (* t)))
 (defun gtk::clist-set-background (a b c) (|gtk_clist_set_background| a b c)))
(progn
 (defalien "gtk_clist_set_cell_style"
           void
           (a (* t))
           (b gint)
           (c gint)
           (d (* t)))
 (defun gtk::clist-set-cell-style (a b c d)
   (|gtk_clist_set_cell_style| a b c d)))
(progn
 (defalien "gtk_clist_get_cell_style" (* t) (a (* t)) (b gint) (c gint))
 (defun gtk::clist-get-cell-style (a b c) (|gtk_clist_get_cell_style| a b c)))
(progn
 (defalien "gtk_clist_set_row_style" void (a (* t)) (b gint) (c (* t)))
 (defun gtk::clist-set-row-style (a b c) (|gtk_clist_set_row_style| a b c)))
(progn
 (defalien "gtk_clist_get_row_style" (* t) (a (* t)) (b gint))
 (defun gtk::clist-get-row-style (a b) (|gtk_clist_get_row_style| a b)))
(progn
 (defalien "gtk_clist_set_shift"
           void
           (a (* t))
           (b gint)
           (c gint)
           (d gint)
           (e gint))
 (defun gtk::clist-set-shift (a b c d e) (|gtk_clist_set_shift| a b c d e)))
(progn
 (defalien "gtk_clist_set_selectable" void (a (* t)) (b gint) (c gboolean))
 (defun gtk::clist-set-selectable (a b c)
   (|gtk_clist_set_selectable| a b (if c (if (eq c 0) 0 1) 0))))
(progn
 (defalien "gtk_clist_get_selectable" gboolean (a (* t)) (b gint))
 (defun gtk::clist-get-selectable (a b)
   (let ((v381 (|gtk_clist_get_selectable| a b)))
     (if (= v381 1) t nil))))
(progn
 (defalien "gtk_clist_prepend" gint (a (* t)) (b (* t)))
 (defun gtk::clist-prepend (a b) (|gtk_clist_prepend| a b)))
(progn
 (defalien "gtk_clist_append" gint (a (* t)) (b (* t)))
 (defun gtk::clist-append (a b) (|gtk_clist_append| a b)))
(progn
 (defalien "gtk_clist_insert" gint (a (* t)) (b gint) (c (* t)))
 (defun gtk::clist-insert (a b c) (|gtk_clist_insert| a b c)))
(progn
 (defalien "gtk_clist_remove" void (a (* t)) (b gint))
 (defun gtk::clist-remove (a b) (|gtk_clist_remove| a b)))
(progn
 (defalien "gtk_clist_set_row_data" void (a (* t)) (b gint) (c gpointer))
 (defun gtk::clist-set-row-data (a b c) (|gtk_clist_set_row_data| a b c)))
(progn
 (defalien "gtk_clist_set_row_data_full"
           void
           (a (* t))
           (b gint)
           (c gpointer)
           (d GtkDestroyNotify))
 (defun gtk::clist-set-row-data-full (a b c d)
   (|gtk_clist_set_row_data_full| a b c d)))
(progn
 (defalien "gtk_clist_get_row_data" gpointer (a (* t)) (b gint))
 (defun gtk::clist-get-row-data (a b) (|gtk_clist_get_row_data| a b)))
(progn
 (defalien "gtk_clist_find_row_from_data" gint (a (* t)) (b gpointer))
 (defun gtk::clist-find-row-from-data (a b)
   (|gtk_clist_find_row_from_data| a b)))
(progn
 (defalien "gtk_clist_select_row" void (a (* t)) (b gint) (c gint))
 (defun gtk::clist-select-row (a b c) (|gtk_clist_select_row| a b c)))
(progn
 (defalien "gtk_clist_unselect_row" void (a (* t)) (b gint) (c gint))
 (defun gtk::clist-unselect-row (a b c) (|gtk_clist_unselect_row| a b c)))
(progn
 (defalien "gtk_clist_undo_selection" void (a (* t)))
 (defun gtk::clist-undo-selection (a) (|gtk_clist_undo_selection| a)))
(progn
 (defalien "gtk_clist_clear" void (a (* t)))
 (defun gtk::clist-clear (a) (|gtk_clist_clear| a)))
(progn
 (defalien "gtk_clist_get_selection_info"
           gint
           (a (* t))
           (b gint)
           (c gint)
           (d gint :in-out)
           (e gint :in-out))
 (defun gtk::clist-get-selection-info (a b c d e)
   (|gtk_clist_get_selection_info| a b c d e)))
(progn
 (defalien "gtk_clist_select_all" void (a (* t)))
 (defun gtk::clist-select-all (a) (|gtk_clist_select_all| a)))
(progn
 (defalien "gtk_clist_unselect_all" void (a (* t)))
 (defun gtk::clist-unselect-all (a) (|gtk_clist_unselect_all| a)))
(progn
 (defalien "gtk_clist_swap_rows" void (a (* t)) (b gint) (c gint))
 (defun gtk::clist-swap-rows (a b c) (|gtk_clist_swap_rows| a b c)))
(progn
 (defalien "gtk_clist_row_move" void (a (* t)) (b gint) (c gint))
 (defun gtk::clist-row-move (a b c) (|gtk_clist_row_move| a b c)))
(progn
 (defalien "gtk_clist_set_compare_func" void (a (* t)) (b GtkCListCompareFunc))
 (defun gtk::clist-set-compare-func (a b) (|gtk_clist_set_compare_func| a b)))
(progn
 (defalien "gtk_clist_set_sort_column" void (a (* t)) (b gint))
 (defun gtk::clist-set-sort-column (a b) (|gtk_clist_set_sort_column| a b)))
(progn
 (defalien "gtk_clist_set_sort_type" void (a (* t)) (b GtkSortType))
 (defun gtk::clist-set-sort-type (a b) (|gtk_clist_set_sort_type| a b)))
(progn
 (defalien "gtk_clist_sort" void (a (* t)))
 (defun gtk::clist-sort (a) (|gtk_clist_sort| a)))
(progn
 (defalien "gtk_clist_set_auto_sort" void (a (* t)) (b gboolean))
 (defun gtk::clist-set-auto-sort (a b)
   (|gtk_clist_set_auto_sort| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gtk_dialog_get_type" GType)
 (defun gtk::dialog-get-type () (|gtk_dialog_get_type|)))
(progn
 (defalien "gtk_dialog_new" (* t))
 (defun gtk::dialog-new () (|gtk_dialog_new|)))
(progn
 (defalien "gtk_dialog_new_with_buttons"
           (* t)
           (a c-string)
           (b (* t))
           (c GtkDialogFlags)
           (d c-string))
 (defun gtk::dialog-new-with-buttons (a b c d)
   (|gtk_dialog_new_with_buttons| a b c d)))
(progn
 (defalien "gtk_dialog_add_action_widget" void (a (* t)) (b (* t)) (c gint))
 (defun gtk::dialog-add-action-widget (a b c)
   (|gtk_dialog_add_action_widget| a b c)))
(progn
 (defalien "gtk_dialog_add_button" (* t) (a (* t)) (b c-string) (c gint))
 (defun gtk::dialog-add-button (a b c) (|gtk_dialog_add_button| a b c)))
(progn
 (defalien "gtk_dialog_add_buttons" void (a (* t)) (b c-string))
 (defun gtk::dialog-add-buttons (a b) (|gtk_dialog_add_buttons| a b)))
(progn
 (defalien "gtk_dialog_set_response_sensitive"
           void
           (a (* t))
           (b gint)
           (c gboolean))
 (defun gtk::dialog-set-response-sensitive (a b c)
   (|gtk_dialog_set_response_sensitive| a b (if c (if (eq c 0) 0 1) 0))))
(progn
 (defalien "gtk_dialog_set_default_response" void (a (* t)) (b gint))
 (defun gtk::dialog-set-default-response (a b)
   (|gtk_dialog_set_default_response| a b)))
(progn
 (defalien "gtk_dialog_set_has_separator" void (a (* t)) (b gboolean))
 (defun gtk::dialog-set-has-separator (a b)
   (|gtk_dialog_set_has_separator| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gtk_dialog_get_has_separator" gboolean (a (* t)))
 (defun gtk::dialog-get-has-separator (a)
   (let ((v382 (|gtk_dialog_get_has_separator| a)))
     (if (= v382 1) t nil))))
(progn
 (defalien "gtk_dialog_response" void (a (* t)) (b gint))
 (defun gtk::dialog-response (a b) (|gtk_dialog_response| a b)))
(progn
 (defalien "gtk_dialog_run" gint (a (* t)))
 (defun gtk::dialog-run (a) (|gtk_dialog_run| a)))
(progn
 (defalien "gtk_vbox_get_type" GType)
 (defun gtk::vbox-get-type () (|gtk_vbox_get_type|)))
(progn
 (defalien "gtk_vbox_new" (* t) (a gboolean) (b gint))
 (defun gtk::vbox-new (a b) (|gtk_vbox_new| (if a (if (eq a 0) 0 1) 0) b)))
(progn
 (defalien "gtk_color_selection_get_type" GType)
 (defun gtk::color-selection-get-type () (|gtk_color_selection_get_type|)))
(progn
 (defalien "gtk_color_selection_new" (* t))
 (defun gtk::color-selection-new () (|gtk_color_selection_new|)))
(progn
 (defalien "gtk_color_selection_get_has_opacity_control" gboolean (a (* t)))
 (defun gtk::color-selection-get-has-opacity-control (a)
   (let ((v383 (|gtk_color_selection_get_has_opacity_control| a)))
     (if (= v383 1) t nil))))
(progn
 (defalien "gtk_color_selection_set_has_opacity_control"
           void
           (a (* t))
           (b gboolean))
 (defun gtk::color-selection-set-has-opacity-control (a b)
   (|gtk_color_selection_set_has_opacity_control| a
    (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gtk_color_selection_get_has_palette" gboolean (a (* t)))
 (defun gtk::color-selection-get-has-palette (a)
   (let ((v384 (|gtk_color_selection_get_has_palette| a)))
     (if (= v384 1) t nil))))
(progn
 (defalien "gtk_color_selection_set_has_palette" void (a (* t)) (b gboolean))
 (defun gtk::color-selection-set-has-palette (a b)
   (|gtk_color_selection_set_has_palette| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gtk_color_selection_set_current_color" void (a (* t)) (b (* t)))
 (defun gtk::color-selection-set-current-color (a b)
   (|gtk_color_selection_set_current_color| a b)))
(progn
 (defalien "gtk_color_selection_set_current_alpha" void (a (* t)) (b guint16))
 (defun gtk::color-selection-set-current-alpha (a b)
   (|gtk_color_selection_set_current_alpha| a b)))
(progn
 (defalien "gtk_color_selection_get_current_color" void (a (* t)) (b (* t)))
 (defun gtk::color-selection-get-current-color (a b)
   (|gtk_color_selection_get_current_color| a b)))
(progn
 (defalien "gtk_color_selection_get_current_alpha" guint16 (a (* t)))
 (defun gtk::color-selection-get-current-alpha (a)
   (|gtk_color_selection_get_current_alpha| a)))
(progn
 (defalien "gtk_color_selection_set_previous_color" void (a (* t)) (b (* t)))
 (defun gtk::color-selection-set-previous-color (a b)
   (|gtk_color_selection_set_previous_color| a b)))
(progn
 (defalien "gtk_color_selection_set_previous_alpha" void (a (* t)) (b guint16))
 (defun gtk::color-selection-set-previous-alpha (a b)
   (|gtk_color_selection_set_previous_alpha| a b)))
(progn
 (defalien "gtk_color_selection_get_previous_color" void (a (* t)) (b (* t)))
 (defun gtk::color-selection-get-previous-color (a b)
   (|gtk_color_selection_get_previous_color| a b)))
(progn
 (defalien "gtk_color_selection_get_previous_alpha" guint16 (a (* t)))
 (defun gtk::color-selection-get-previous-alpha (a)
   (|gtk_color_selection_get_previous_alpha| a)))
(progn
 (defalien "gtk_color_selection_is_adjusting" gboolean (a (* t)))
 (defun gtk::color-selection-is-adjusting (a)
   (let ((v385 (|gtk_color_selection_is_adjusting| a)))
     (if (= v385 1) t nil))))
(progn
 (defalien "gtk_color_selection_palette_from_string"
           gboolean
           (a c-string)
           (b (* t))
           (c gint :in-out))
 (defun gtk::color-selection-palette-from-string (a b c)
   (let ((v386
          (multiple-value-list
           (|gtk_color_selection_palette_from_string| a b c))))
     (apply #'values (if (= 1 (car v386)) t nil) (cdr v386)))))
(progn
 (defalien "gtk_color_selection_palette_to_string" gchar (a (* t)) (b gint))
 (defun gtk::color-selection-palette-to-string (a b)
   (|gtk_color_selection_palette_to_string| a b)))
(progn
 (defalien "gtk_color_selection_set_change_palette_hook"
           GtkColorSelectionChangePaletteFunc
           (a GtkColorSelectionChangePaletteFunc))
 (defun gtk::color-selection-set-change-palette-hook (a)
   (|gtk_color_selection_set_change_palette_hook| a)))
(progn
 (defalien "gtk_color_selection_dialog_get_type" GType)
 (defun gtk::color-selection-dialog-get-type ()
   (|gtk_color_selection_dialog_get_type|)))
(progn
 (defalien "gtk_color_selection_dialog_new" (* t) (a c-string))
 (defun gtk::color-selection-dialog-new (a)
   (|gtk_color_selection_dialog_new| a)))
(progn
 (defalien "gtk_hbox_get_type" GType)
 (defun gtk::hbox-get-type () (|gtk_hbox_get_type|)))
(progn
 (defalien "gtk_hbox_new" (* t) (a gboolean) (b gint))
 (defun gtk::hbox-new (a b) (|gtk_hbox_new| (if a (if (eq a 0) 0 1) 0) b)))
(progn
 (defalien "gtk_combo_get_type" GType)
 (defun gtk::combo-get-type () (|gtk_combo_get_type|)))
(progn
 (defalien "gtk_combo_new" (* t))
 (defun gtk::combo-new () (|gtk_combo_new|)))
(progn
 (defalien "gtk_combo_set_value_in_list"
           void
           (a (* t))
           (b gboolean)
           (c gboolean))
 (defun gtk::combo-set-value-in-list (a b c)
   (|gtk_combo_set_value_in_list| a (if b (if (eq b 0) 0 1) 0)
    (if c (if (eq c 0) 0 1) 0))))
(progn
 (defalien "gtk_combo_set_use_arrows" void (a (* t)) (b gboolean))
 (defun gtk::combo-set-use-arrows (a b)
   (|gtk_combo_set_use_arrows| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gtk_combo_set_use_arrows_always" void (a (* t)) (b gboolean))
 (defun gtk::combo-set-use-arrows-always (a b)
   (|gtk_combo_set_use_arrows_always| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gtk_combo_set_case_sensitive" void (a (* t)) (b gboolean))
 (defun gtk::combo-set-case-sensitive (a b)
   (|gtk_combo_set_case_sensitive| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gtk_combo_set_item_string" void (a (* t)) (b (* t)) (c c-string))
 (defun gtk::combo-set-item-string (a b c) (|gtk_combo_set_item_string| a b c)))
(progn
 (defalien "gtk_combo_set_popdown_strings" void (a (* t)) (b (* t)))
 (defun gtk::combo-set-popdown-strings (a b)
   (|gtk_combo_set_popdown_strings| a b)))
(progn
 (defalien "gtk_combo_disable_activate" void (a (* t)))
 (defun gtk::combo-disable-activate (a) (|gtk_combo_disable_activate| a)))
(progn
 (defalien "gtk_ctree_get_type" GtkType)
 (defun gtk::ctree-get-type () (|gtk_ctree_get_type|)))
(progn
 (defalien "gtk_ctree_insert_node"
           (* t)
           (a (* t))
           (b (* t))
           (c (* t))
           (d (* t))
           (e guint8)
           (f (* t))
           (g (* t))
           (h (* t))
           (i (* t))
           (j gboolean)
           (k gboolean))
 (defun gtk::ctree-insert-node (a b c d e f g h i j k)
   (|gtk_ctree_insert_node| a b c d e f g h i (if j (if (eq j 0) 0 1) 0)
    (if k (if (eq k 0) 0 1) 0))))
(progn
 (defalien "gtk_ctree_remove_node" void (a (* t)) (b (* t)))
 (defun gtk::ctree-remove-node (a b) (|gtk_ctree_remove_node| a b)))
(progn
 (defalien "gtk_ctree_insert_gnode"
           (* t)
           (a (* t))
           (b (* t))
           (c (* t))
           (d (* t))
           (e GtkCTreeGNodeFunc)
           (f gpointer))
 (defun gtk::ctree-insert-gnode (a b c d e f)
   (|gtk_ctree_insert_gnode| a b c d e f)))
(progn
 (defalien "gtk_ctree_export_to_gnode"
           (* t)
           (a (* t))
           (b (* t))
           (c (* t))
           (d (* t))
           (e GtkCTreeGNodeFunc)
           (f gpointer))
 (defun gtk::ctree-export-to-gnode (a b c d e f)
   (|gtk_ctree_export_to_gnode| a b c d e f)))
(progn
 (defalien "gtk_ctree_post_recursive"
           void
           (a (* t))
           (b (* t))
           (c GtkCTreeFunc)
           (d gpointer))
 (defun gtk::ctree-post-recursive (a b c d)
   (|gtk_ctree_post_recursive| a b c d)))
(progn
 (defalien "gtk_ctree_post_recursive_to_depth"
           void
           (a (* t))
           (b (* t))
           (c gint)
           (d GtkCTreeFunc)
           (e gpointer))
 (defun gtk::ctree-post-recursive-to-depth (a b c d e)
   (|gtk_ctree_post_recursive_to_depth| a b c d e)))
(progn
 (defalien "gtk_ctree_pre_recursive"
           void
           (a (* t))
           (b (* t))
           (c GtkCTreeFunc)
           (d gpointer))
 (defun gtk::ctree-pre-recursive (a b c d) (|gtk_ctree_pre_recursive| a b c d)))
(progn
 (defalien "gtk_ctree_pre_recursive_to_depth"
           void
           (a (* t))
           (b (* t))
           (c gint)
           (d GtkCTreeFunc)
           (e gpointer))
 (defun gtk::ctree-pre-recursive-to-depth (a b c d e)
   (|gtk_ctree_pre_recursive_to_depth| a b c d e)))
(progn
 (defalien "gtk_ctree_is_viewable" gboolean (a (* t)) (b (* t)))
 (defun gtk::ctree-is-viewable (a b)
   (let ((v387 (|gtk_ctree_is_viewable| a b)))
     (if (= v387 1) t nil))))
(progn
 (defalien "gtk_ctree_last" (* t) (a (* t)) (b (* t)))
 (defun gtk::ctree-last (a b) (|gtk_ctree_last| a b)))
(progn
 (defalien "gtk_ctree_find_node_ptr" (* t) (a (* t)) (b (* t)))
 (defun gtk::ctree-find-node-ptr (a b) (|gtk_ctree_find_node_ptr| a b)))
(progn
 (defalien "gtk_ctree_node_nth" (* t) (a (* t)) (b guint))
 (defun gtk::ctree-node-nth (a b) (|gtk_ctree_node_nth| a b)))
(progn
 (defalien "gtk_ctree_find" gboolean (a (* t)) (b (* t)) (c (* t)))
 (defun gtk::ctree-find (a b c)
   (let ((v388 (|gtk_ctree_find| a b c)))
     (if (= v388 1) t nil))))
(progn
 (defalien "gtk_ctree_is_ancestor" gboolean (a (* t)) (b (* t)) (c (* t)))
 (defun gtk::ctree-is-ancestor (a b c)
   (let ((v389 (|gtk_ctree_is_ancestor| a b c)))
     (if (= v389 1) t nil))))
(progn
 (defalien "gtk_ctree_find_by_row_data" (* t) (a (* t)) (b (* t)) (c gpointer))
 (defun gtk::ctree-find-by-row-data (a b c)
   (|gtk_ctree_find_by_row_data| a b c)))
(progn
 (defalien "gtk_ctree_find_all_by_row_data"
           (* t)
           (a (* t))
           (b (* t))
           (c gpointer))
 (defun gtk::ctree-find-all-by-row-data (a b c)
   (|gtk_ctree_find_all_by_row_data| a b c)))
(progn
 (defalien "gtk_ctree_find_by_row_data_custom"
           (* t)
           (a (* t))
           (b (* t))
           (c gpointer)
           (d GCompareFunc))
 (defun gtk::ctree-find-by-row-data-custom (a b c d)
   (|gtk_ctree_find_by_row_data_custom| a b c d)))
(progn
 (defalien "gtk_ctree_find_all_by_row_data_custom"
           (* t)
           (a (* t))
           (b (* t))
           (c gpointer)
           (d GCompareFunc))
 (defun gtk::ctree-find-all-by-row-data-custom (a b c d)
   (|gtk_ctree_find_all_by_row_data_custom| a b c d)))
(progn
 (defalien "gtk_ctree_is_hot_spot" gboolean (a (* t)) (b gint) (c gint))
 (defun gtk::ctree-is-hot-spot (a b c)
   (let ((v390 (|gtk_ctree_is_hot_spot| a b c)))
     (if (= v390 1) t nil))))
(progn
 (defalien "gtk_ctree_move" void (a (* t)) (b (* t)) (c (* t)) (d (* t)))
 (defun gtk::ctree-move (a b c d) (|gtk_ctree_move| a b c d)))
(progn
 (defalien "gtk_ctree_expand" void (a (* t)) (b (* t)))
 (defun gtk::ctree-expand (a b) (|gtk_ctree_expand| a b)))
(progn
 (defalien "gtk_ctree_expand_recursive" void (a (* t)) (b (* t)))
 (defun gtk::ctree-expand-recursive (a b) (|gtk_ctree_expand_recursive| a b)))
(progn
 (defalien "gtk_ctree_expand_to_depth" void (a (* t)) (b (* t)) (c gint))
 (defun gtk::ctree-expand-to-depth (a b c) (|gtk_ctree_expand_to_depth| a b c)))
(progn
 (defalien "gtk_ctree_collapse" void (a (* t)) (b (* t)))
 (defun gtk::ctree-collapse (a b) (|gtk_ctree_collapse| a b)))
(progn
 (defalien "gtk_ctree_collapse_recursive" void (a (* t)) (b (* t)))
 (defun gtk::ctree-collapse-recursive (a b)
   (|gtk_ctree_collapse_recursive| a b)))
(progn
 (defalien "gtk_ctree_collapse_to_depth" void (a (* t)) (b (* t)) (c gint))
 (defun gtk::ctree-collapse-to-depth (a b c)
   (|gtk_ctree_collapse_to_depth| a b c)))
(progn
 (defalien "gtk_ctree_toggle_expansion" void (a (* t)) (b (* t)))
 (defun gtk::ctree-toggle-expansion (a b) (|gtk_ctree_toggle_expansion| a b)))
(progn
 (defalien "gtk_ctree_toggle_expansion_recursive" void (a (* t)) (b (* t)))
 (defun gtk::ctree-toggle-expansion-recursive (a b)
   (|gtk_ctree_toggle_expansion_recursive| a b)))
(progn
 (defalien "gtk_ctree_select" void (a (* t)) (b (* t)))
 (defun gtk::ctree-select (a b) (|gtk_ctree_select| a b)))
(progn
 (defalien "gtk_ctree_select_recursive" void (a (* t)) (b (* t)))
 (defun gtk::ctree-select-recursive (a b) (|gtk_ctree_select_recursive| a b)))
(progn
 (defalien "gtk_ctree_unselect" void (a (* t)) (b (* t)))
 (defun gtk::ctree-unselect (a b) (|gtk_ctree_unselect| a b)))
(progn
 (defalien "gtk_ctree_unselect_recursive" void (a (* t)) (b (* t)))
 (defun gtk::ctree-unselect-recursive (a b)
   (|gtk_ctree_unselect_recursive| a b)))
(progn
 (defalien "gtk_ctree_real_select_recursive" void (a (* t)) (b (* t)) (c gint))
 (defun gtk::ctree-real-select-recursive (a b c)
   (|gtk_ctree_real_select_recursive| a b c)))
(progn
 (defalien "gtk_ctree_node_set_text"
           void
           (a (* t))
           (b (* t))
           (c gint)
           (d c-string))
 (defun gtk::ctree-node-set-text (a b c d) (|gtk_ctree_node_set_text| a b c d)))
(progn
 (defalien "gtk_ctree_node_set_pixmap"
           void
           (a (* t))
           (b (* t))
           (c gint)
           (d (* t))
           (e (* t)))
 (defun gtk::ctree-node-set-pixmap (a b c d e)
   (|gtk_ctree_node_set_pixmap| a b c d e)))
(progn
 (defalien "gtk_ctree_node_set_pixtext"
           void
           (a (* t))
           (b (* t))
           (c gint)
           (d c-string)
           (e guint8)
           (f (* t))
           (g (* t)))
 (defun gtk::ctree-node-set-pixtext (a b c d e f g)
   (|gtk_ctree_node_set_pixtext| a b c d e f g)))
(progn
 (defalien "gtk_ctree_set_node_info"
           void
           (a (* t))
           (b (* t))
           (c c-string)
           (d guint8)
           (e (* t))
           (f (* t))
           (g (* t))
           (h (* t))
           (i gboolean)
           (j gboolean))
 (defun gtk::ctree-set-node-info (a b c d e f g h i j)
   (|gtk_ctree_set_node_info| a b c d e f g h (if i (if (eq i 0) 0 1) 0)
    (if j (if (eq j 0) 0 1) 0))))
(progn
 (defalien "gtk_ctree_node_set_shift"
           void
           (a (* t))
           (b (* t))
           (c gint)
           (d gint)
           (e gint))
 (defun gtk::ctree-node-set-shift (a b c d e)
   (|gtk_ctree_node_set_shift| a b c d e)))
(progn
 (defalien "gtk_ctree_node_set_selectable"
           void
           (a (* t))
           (b (* t))
           (c gboolean))
 (defun gtk::ctree-node-set-selectable (a b c)
   (|gtk_ctree_node_set_selectable| a b (if c (if (eq c 0) 0 1) 0))))
(progn
 (defalien "gtk_ctree_node_get_selectable" gboolean (a (* t)) (b (* t)))
 (defun gtk::ctree-node-get-selectable (a b)
   (let ((v391 (|gtk_ctree_node_get_selectable| a b)))
     (if (= v391 1) t nil))))
(progn
 (defalien "gtk_ctree_node_get_cell_type"
           GtkCellType
           (a (* t))
           (b (* t))
           (c gint))
 (defun gtk::ctree-node-get-cell-type (a b c)
   (|gtk_ctree_node_get_cell_type| a b c)))
(progn
 (defalien "gtk_ctree_node_get_text"
           gboolean
           (a (* t))
           (b (* t))
           (c gint)
           (d (* t)))
 (defun gtk::ctree-node-get-text (a b c d)
   (let ((v392 (|gtk_ctree_node_get_text| a b c d)))
     (if (= v392 1) t nil))))
(progn
 (defalien "gtk_ctree_node_get_pixmap"
           gboolean
           (a (* t))
           (b (* t))
           (c gint)
           (d (* t))
           (e (* t)))
 (defun gtk::ctree-node-get-pixmap (a b c d e)
   (let ((v393 (|gtk_ctree_node_get_pixmap| a b c d e)))
     (if (= v393 1) t nil))))
(progn
 (defalien "gtk_ctree_node_get_pixtext"
           gboolean
           (a (* t))
           (b (* t))
           (c gint)
           (d (* t))
           (e guint8 :in-out)
           (f (* t))
           (g (* t)))
 (defun gtk::ctree-node-get-pixtext (a b c d e f g)
   (let ((v394
          (multiple-value-list (|gtk_ctree_node_get_pixtext| a b c d e f g))))
     (apply #'values (if (= 1 (car v394)) t nil) (cdr v394)))))
(progn
 (defalien "gtk_ctree_get_node_info"
           gboolean
           (a (* t))
           (b (* t))
           (c (* t))
           (d guint8 :in-out)
           (e (* t))
           (f (* t))
           (g (* t))
           (h (* t))
           (i (* t))
           (j (* t)))
 (defun gtk::ctree-get-node-info (a b c d e f g h i j)
   (let ((v395
          (multiple-value-list
           (|gtk_ctree_get_node_info| a b c d e f g h i j))))
     (apply #'values (if (= 1 (car v395)) t nil) (cdr v395)))))
(progn
 (defalien "gtk_ctree_node_set_row_style" void (a (* t)) (b (* t)) (c (* t)))
 (defun gtk::ctree-node-set-row-style (a b c)
   (|gtk_ctree_node_set_row_style| a b c)))
(progn
 (defalien "gtk_ctree_node_get_row_style" (* t) (a (* t)) (b (* t)))
 (defun gtk::ctree-node-get-row-style (a b)
   (|gtk_ctree_node_get_row_style| a b)))
(progn
 (defalien "gtk_ctree_node_set_cell_style"
           void
           (a (* t))
           (b (* t))
           (c gint)
           (d (* t)))
 (defun gtk::ctree-node-set-cell-style (a b c d)
   (|gtk_ctree_node_set_cell_style| a b c d)))
(progn
 (defalien "gtk_ctree_node_get_cell_style" (* t) (a (* t)) (b (* t)) (c gint))
 (defun gtk::ctree-node-get-cell-style (a b c)
   (|gtk_ctree_node_get_cell_style| a b c)))
(progn
 (defalien "gtk_ctree_node_set_foreground" void (a (* t)) (b (* t)) (c (* t)))
 (defun gtk::ctree-node-set-foreground (a b c)
   (|gtk_ctree_node_set_foreground| a b c)))
(progn
 (defalien "gtk_ctree_node_set_background" void (a (* t)) (b (* t)) (c (* t)))
 (defun gtk::ctree-node-set-background (a b c)
   (|gtk_ctree_node_set_background| a b c)))
(progn
 (defalien "gtk_ctree_node_set_row_data" void (a (* t)) (b (* t)) (c gpointer))
 (defun gtk::ctree-node-set-row-data (a b c)
   (|gtk_ctree_node_set_row_data| a b c)))
(progn
 (defalien "gtk_ctree_node_set_row_data_full"
           void
           (a (* t))
           (b (* t))
           (c gpointer)
           (d GtkDestroyNotify))
 (defun gtk::ctree-node-set-row-data-full (a b c d)
   (|gtk_ctree_node_set_row_data_full| a b c d)))
(progn
 (defalien "gtk_ctree_node_get_row_data" gpointer (a (* t)) (b (* t)))
 (defun gtk::ctree-node-get-row-data (a b) (|gtk_ctree_node_get_row_data| a b)))
(progn
 (defalien "gtk_ctree_node_moveto"
           void
           (a (* t))
           (b (* t))
           (c gint)
           (d gfloat)
           (e gfloat))
 (defun gtk::ctree-node-moveto (a b c d e)
   (|gtk_ctree_node_moveto| a b c (coerce d 'single-float)
    (coerce e 'single-float))))
(progn
 (defalien "gtk_ctree_node_is_visible" GtkVisibility (a (* t)) (b (* t)))
 (defun gtk::ctree-node-is-visible (a b) (|gtk_ctree_node_is_visible| a b)))
(progn
 (defalien "gtk_ctree_set_indent" void (a (* t)) (b gint))
 (defun gtk::ctree-set-indent (a b) (|gtk_ctree_set_indent| a b)))
(progn
 (defalien "gtk_ctree_set_spacing" void (a (* t)) (b gint))
 (defun gtk::ctree-set-spacing (a b) (|gtk_ctree_set_spacing| a b)))
(progn
 (defalien "gtk_ctree_set_show_stub" void (a (* t)) (b gboolean))
 (defun gtk::ctree-set-show-stub (a b)
   (|gtk_ctree_set_show_stub| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gtk_ctree_set_line_style" void (a (* t)) (b GtkCTreeLineStyle))
 (defun gtk::ctree-set-line-style (a b) (|gtk_ctree_set_line_style| a b)))
(progn
 (defalien "gtk_ctree_set_expander_style"
           void
           (a (* t))
           (b GtkCTreeExpanderStyle))
 (defun gtk::ctree-set-expander-style (a b)
   (|gtk_ctree_set_expander_style| a b)))
(progn
 (defalien "gtk_ctree_set_drag_compare_func"
           void
           (a (* t))
           (b GtkCTreeCompareDragFunc))
 (defun gtk::ctree-set-drag-compare-func (a b)
   (|gtk_ctree_set_drag_compare_func| a b)))
(progn
 (defalien "gtk_ctree_sort_node" void (a (* t)) (b (* t)))
 (defun gtk::ctree-sort-node (a b) (|gtk_ctree_sort_node| a b)))
(progn
 (defalien "gtk_ctree_sort_recursive" void (a (* t)) (b (* t)))
 (defun gtk::ctree-sort-recursive (a b) (|gtk_ctree_sort_recursive| a b)))
(progn
 (defalien "gtk_ctree_node_get_type" GType)
 (defun gtk::ctree-node-get-type () (|gtk_ctree_node_get_type|)))
(progn
 (defalien "gtk_drawing_area_get_type" GType)
 (defun gtk::drawing-area-get-type () (|gtk_drawing_area_get_type|)))
(progn
 (defalien "gtk_drawing_area_new" (* t))
 (defun gtk::drawing-area-new () (|gtk_drawing_area_new|)))
(progn
 (defalien "gtk_curve_get_type" GType)
 (defun gtk::curve-get-type () (|gtk_curve_get_type|)))
(progn
 (defalien "gtk_curve_new" (* t))
 (defun gtk::curve-new () (|gtk_curve_new|)))
(progn
 (defalien "gtk_curve_reset" void (a (* t)))
 (defun gtk::curve-reset (a) (|gtk_curve_reset| a)))
(progn
 (defalien "gtk_curve_set_gamma" void (a (* t)) (b gfloat))
 (defun gtk::curve-set-gamma (a b)
   (|gtk_curve_set_gamma| a (coerce b 'single-float))))
(progn
 (defalien "gtk_curve_set_range"
           void
           (a (* t))
           (b gfloat)
           (c gfloat)
           (d gfloat)
           (e gfloat))
 (defun gtk::curve-set-range (a b c d e)
   (|gtk_curve_set_range| a (coerce b 'single-float) (coerce c 'single-float)
    (coerce d 'single-float) (coerce e 'single-float))))
(progn
 (defalien "gtk_curve_get_vector" void (a (* t)) (b int) (c gfloat :in-out))
 (defun gtk::curve-get-vector (a b c)
   (|gtk_curve_get_vector| a b (coerce c 'single-float))))
(progn
 (defalien "gtk_curve_set_vector" void (a (* t)) (b int) (c gfloat :in-out))
 (defun gtk::curve-set-vector (a b c)
   (|gtk_curve_set_vector| a b (coerce c 'single-float))))
(progn
 (defalien "gtk_curve_set_curve_type" void (a (* t)) (b GtkCurveType))
 (defun gtk::curve-set-curve-type (a b) (|gtk_curve_set_curve_type| a b)))
(progn
 (defalien "gtk_drag_get_data"
           void
           (a (* t))
           (b (* t))
           (c GdkAtom)
           (d guint32))
 (defun gtk::drag-get-data (a b c d) (|gtk_drag_get_data| a b c d)))
(progn
 (defalien "gtk_drag_finish"
           void
           (a (* t))
           (b gboolean)
           (c gboolean)
           (d guint32))
 (defun gtk::drag-finish (a b c d)
   (|gtk_drag_finish| a (if b (if (eq b 0) 0 1) 0) (if c (if (eq c 0) 0 1) 0)
    d)))
(progn
 (defalien "gtk_drag_get_source_widget" (* t) (a (* t)))
 (defun gtk::drag-get-source-widget (a) (|gtk_drag_get_source_widget| a)))
(progn
 (defalien "gtk_drag_highlight" void (a (* t)))
 (defun gtk::drag-highlight (a) (|gtk_drag_highlight| a)))
(progn
 (defalien "gtk_drag_unhighlight" void (a (* t)))
 (defun gtk::drag-unhighlight (a) (|gtk_drag_unhighlight| a)))
(progn
 (defalien "gtk_drag_dest_set"
           void
           (a (* t))
           (b GtkDestDefaults)
           (c (* t))
           (d gint)
           (e GdkDragAction))
 (defun gtk::drag-dest-set (a b c d e) (|gtk_drag_dest_set| a b c d e)))
(progn
 (defalien "gtk_drag_dest_set_proxy"
           void
           (a (* t))
           (b (* t))
           (c GdkDragProtocol)
           (d gboolean))
 (defun gtk::drag-dest-set-proxy (a b c d)
   (|gtk_drag_dest_set_proxy| a b c (if d (if (eq d 0) 0 1) 0))))
(progn
 (defalien "gtk_drag_dest_unset" void (a (* t)))
 (defun gtk::drag-dest-unset (a) (|gtk_drag_dest_unset| a)))
(progn
 (defalien "gtk_drag_dest_find_target" GdkAtom (a (* t)) (b (* t)) (c (* t)))
 (defun gtk::drag-dest-find-target (a b c) (|gtk_drag_dest_find_target| a b c)))
(progn
 (defalien "gtk_drag_dest_get_target_list" (* t) (a (* t)))
 (defun gtk::drag-dest-get-target-list (a) (|gtk_drag_dest_get_target_list| a)))
(progn
 (defalien "gtk_drag_dest_set_target_list" void (a (* t)) (b (* t)))
 (defun gtk::drag-dest-set-target-list (a b)
   (|gtk_drag_dest_set_target_list| a b)))
(progn
 (defalien "gtk_drag_source_set"
           void
           (a (* t))
           (b GdkModifierType)
           (c (* t))
           (d gint)
           (e GdkDragAction))
 (defun gtk::drag-source-set (a b c d e) (|gtk_drag_source_set| a b c d e)))
(progn
 (defalien "gtk_drag_source_unset" void (a (* t)))
 (defun gtk::drag-source-unset (a) (|gtk_drag_source_unset| a)))
(progn
 (defalien "gtk_drag_source_set_icon"
           void
           (a (* t))
           (b (* t))
           (c (* t))
           (d (* t)))
 (defun gtk::drag-source-set-icon (a b c d)
   (|gtk_drag_source_set_icon| a b c d)))
(progn
 (defalien "gtk_drag_source_set_icon_pixbuf" void (a (* t)) (b (* t)))
 (defun gtk::drag-source-set-icon-pixbuf (a b)
   (|gtk_drag_source_set_icon_pixbuf| a b)))
(progn
 (defalien "gtk_drag_source_set_icon_stock" void (a (* t)) (b c-string))
 (defun gtk::drag-source-set-icon-stock (a b)
   (|gtk_drag_source_set_icon_stock| a b)))
(progn
 (defalien "gtk_drag_begin"
           (* t)
           (a (* t))
           (b (* t))
           (c GdkDragAction)
           (d gint)
           (e (* t)))
 (defun gtk::drag-begin (a b c d e) (|gtk_drag_begin| a b c d e)))
(progn
 (defalien "gtk_drag_set_icon_widget"
           void
           (a (* t))
           (b (* t))
           (c gint)
           (d gint))
 (defun gtk::drag-set-icon-widget (a b c d)
   (|gtk_drag_set_icon_widget| a b c d)))
(progn
 (defalien "gtk_drag_set_icon_pixmap"
           void
           (a (* t))
           (b (* t))
           (c (* t))
           (d (* t))
           (e gint)
           (f gint))
 (defun gtk::drag-set-icon-pixmap (a b c d e f)
   (|gtk_drag_set_icon_pixmap| a b c d e f)))
(progn
 (defalien "gtk_drag_set_icon_pixbuf"
           void
           (a (* t))
           (b (* t))
           (c gint)
           (d gint))
 (defun gtk::drag-set-icon-pixbuf (a b c d)
   (|gtk_drag_set_icon_pixbuf| a b c d)))
(progn
 (defalien "gtk_drag_set_icon_stock"
           void
           (a (* t))
           (b c-string)
           (c gint)
           (d gint))
 (defun gtk::drag-set-icon-stock (a b c d) (|gtk_drag_set_icon_stock| a b c d)))
(progn
 (defalien "gtk_drag_set_icon_default" void (a (* t)))
 (defun gtk::drag-set-icon-default (a) (|gtk_drag_set_icon_default| a)))
(progn
 (defalien "gtk_drag_check_threshold"
           gboolean
           (a (* t))
           (b gint)
           (c gint)
           (d gint)
           (e gint))
 (defun gtk::drag-check-threshold (a b c d e)
   (let ((v396 (|gtk_drag_check_threshold| a b c d e)))
     (if (= v396 1) t nil))))
(progn
 (defalien "gtk_editable_get_type" GType)
 (defun gtk::editable-get-type () (|gtk_editable_get_type|)))
(progn
 (defalien "gtk_editable_select_region" void (a (* t)) (b gint) (c gint))
 (defun gtk::editable-select-region (a b c)
   (|gtk_editable_select_region| a b c)))
(progn
 (defalien "gtk_editable_get_selection_bounds"
           gboolean
           (a (* t))
           (b gint :in-out)
           (c gint :in-out))
 (defun gtk::editable-get-selection-bounds (a b c)
   (let ((v397
          (multiple-value-list (|gtk_editable_get_selection_bounds| a b c))))
     (apply #'values (if (= 1 (car v397)) t nil) (cdr v397)))))
(progn
 (defalien "gtk_editable_insert_text"
           void
           (a (* t))
           (b c-string)
           (c gint)
           (d gint :in-out))
 (defun gtk::editable-insert-text (a b c d)
   (|gtk_editable_insert_text| a b c d)))
(progn
 (defalien "gtk_editable_delete_text" void (a (* t)) (b gint) (c gint))
 (defun gtk::editable-delete-text (a b c) (|gtk_editable_delete_text| a b c)))
(progn
 (defalien "gtk_editable_get_chars" gchar (a (* t)) (b gint) (c gint))
 (defun gtk::editable-get-chars (a b c) (|gtk_editable_get_chars| a b c)))
(progn
 (defalien "gtk_editable_cut_clipboard" void (a (* t)))
 (defun gtk::editable-cut-clipboard (a) (|gtk_editable_cut_clipboard| a)))
(progn
 (defalien "gtk_editable_copy_clipboard" void (a (* t)))
 (defun gtk::editable-copy-clipboard (a) (|gtk_editable_copy_clipboard| a)))
(progn
 (defalien "gtk_editable_paste_clipboard" void (a (* t)))
 (defun gtk::editable-paste-clipboard (a) (|gtk_editable_paste_clipboard| a)))
(progn
 (defalien "gtk_editable_delete_selection" void (a (* t)))
 (defun gtk::editable-delete-selection (a) (|gtk_editable_delete_selection| a)))
(progn
 (defalien "gtk_editable_set_position" void (a (* t)) (b gint))
 (defun gtk::editable-set-position (a b) (|gtk_editable_set_position| a b)))
(progn
 (defalien "gtk_editable_get_position" gint (a (* t)))
 (defun gtk::editable-get-position (a) (|gtk_editable_get_position| a)))
(progn
 (defalien "gtk_editable_set_editable" void (a (* t)) (b gboolean))
 (defun gtk::editable-set-editable (a b)
   (|gtk_editable_set_editable| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gtk_editable_get_editable" gboolean (a (* t)))
 (defun gtk::editable-get-editable (a)
   (let ((v398 (|gtk_editable_get_editable| a)))
     (if (= v398 1) t nil))))
(progn
 (defalien "gtk_im_context_get_type" GType)
 (defun gtk::im-context-get-type () (|gtk_im_context_get_type|)))
(progn
 (defalien "gtk_im_context_set_client_window" void (a (* t)) (b (* t)))
 (defun gtk::im-context-set-client-window (a b)
   (|gtk_im_context_set_client_window| a b)))
(progn
 (defalien "gtk_im_context_get_preedit_string"
           void
           (a (* t))
           (b (* t))
           (c (* t))
           (d gint :in-out))
 (defun gtk::im-context-get-preedit-string (a b c d)
   (|gtk_im_context_get_preedit_string| a b c d)))
(progn
 (defalien "gtk_im_context_filter_keypress" gboolean (a (* t)) (b (* t)))
 (defun gtk::im-context-filter-keypress (a b)
   (let ((v399 (|gtk_im_context_filter_keypress| a b)))
     (if (= v399 1) t nil))))
(progn
 (defalien "gtk_im_context_focus_in" void (a (* t)))
 (defun gtk::im-context-focus-in (a) (|gtk_im_context_focus_in| a)))
(progn
 (defalien "gtk_im_context_focus_out" void (a (* t)))
 (defun gtk::im-context-focus-out (a) (|gtk_im_context_focus_out| a)))
(progn
 (defalien "gtk_im_context_reset" void (a (* t)))
 (defun gtk::im-context-reset (a) (|gtk_im_context_reset| a)))
(progn
 (defalien "gtk_im_context_set_cursor_location" void (a (* t)) (b (* t)))
 (defun gtk::im-context-set-cursor-location (a b)
   (|gtk_im_context_set_cursor_location| a b)))
(progn
 (defalien "gtk_im_context_set_use_preedit" void (a (* t)) (b gboolean))
 (defun gtk::im-context-set-use-preedit (a b)
   (|gtk_im_context_set_use_preedit| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gtk_im_context_set_surrounding"
           void
           (a (* t))
           (b c-string)
           (c gint)
           (d gint))
 (defun gtk::im-context-set-surrounding (a b c d)
   (|gtk_im_context_set_surrounding| a b c d)))
(progn
 (defalien "gtk_im_context_get_surrounding"
           gboolean
           (a (* t))
           (b (* t))
           (c gint :in-out))
 (defun gtk::im-context-get-surrounding (a b c)
   (let ((v400 (multiple-value-list (|gtk_im_context_get_surrounding| a b c))))
     (apply #'values (if (= 1 (car v400)) t nil) (cdr v400)))))
(progn
 (defalien "gtk_im_context_delete_surrounding"
           gboolean
           (a (* t))
           (b gint)
           (c gint))
 (defun gtk::im-context-delete-surrounding (a b c)
   (let ((v401 (|gtk_im_context_delete_surrounding| a b c)))
     (if (= v401 1) t nil))))
(progn
 (defalien "gtk_entry_get_type" GType)
 (defun gtk::entry-get-type () (|gtk_entry_get_type|)))
(progn
 (defalien "gtk_entry_new" (* t))
 (defun gtk::entry-new () (|gtk_entry_new|)))
(progn
 (defalien "gtk_entry_set_visibility" void (a (* t)) (b gboolean))
 (defun gtk::entry-set-visibility (a b)
   (|gtk_entry_set_visibility| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gtk_entry_get_visibility" gboolean (a (* t)))
 (defun gtk::entry-get-visibility (a)
   (let ((v402 (|gtk_entry_get_visibility| a)))
     (if (= v402 1) t nil))))
(progn
 (defalien "gtk_entry_set_invisible_char" void (a (* t)) (b gunichar))
 (defun gtk::entry-set-invisible-char (a b)
   (|gtk_entry_set_invisible_char| a b)))
(progn
 (defalien "gtk_entry_get_invisible_char" gunichar (a (* t)))
 (defun gtk::entry-get-invisible-char (a) (|gtk_entry_get_invisible_char| a)))
(progn
 (defalien "gtk_entry_set_has_frame" void (a (* t)) (b gboolean))
 (defun gtk::entry-set-has-frame (a b)
   (|gtk_entry_set_has_frame| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gtk_entry_get_has_frame" gboolean (a (* t)))
 (defun gtk::entry-get-has-frame (a)
   (let ((v403 (|gtk_entry_get_has_frame| a)))
     (if (= v403 1) t nil))))
(progn
 (defalien "gtk_entry_set_max_length" void (a (* t)) (b gint))
 (defun gtk::entry-set-max-length (a b) (|gtk_entry_set_max_length| a b)))
(progn
 (defalien "gtk_entry_get_max_length" gint (a (* t)))
 (defun gtk::entry-get-max-length (a) (|gtk_entry_get_max_length| a)))
(progn
 (defalien "gtk_entry_set_activates_default" void (a (* t)) (b gboolean))
 (defun gtk::entry-set-activates-default (a b)
   (|gtk_entry_set_activates_default| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gtk_entry_get_activates_default" gboolean (a (* t)))
 (defun gtk::entry-get-activates-default (a)
   (let ((v404 (|gtk_entry_get_activates_default| a)))
     (if (= v404 1) t nil))))
(progn
 (defalien "gtk_entry_set_width_chars" void (a (* t)) (b gint))
 (defun gtk::entry-set-width-chars (a b) (|gtk_entry_set_width_chars| a b)))
(progn
 (defalien "gtk_entry_get_width_chars" gint (a (* t)))
 (defun gtk::entry-get-width-chars (a) (|gtk_entry_get_width_chars| a)))
(progn
 (defalien "gtk_entry_set_text" void (a (* t)) (b c-string))
 (defun gtk::entry-set-text (a b) (|gtk_entry_set_text| a b)))
(progn
 (defalien "gtk_entry_get_text" c-string (a (* t)))
 (defun gtk::entry-get-text (a) (|gtk_entry_get_text| a)))
(progn
 (defalien "gtk_entry_get_layout" (* t) (a (* t)))
 (defun gtk::entry-get-layout (a) (|gtk_entry_get_layout| a)))
(progn
 (defalien "gtk_entry_get_layout_offsets"
           void
           (a (* t))
           (b gint :in-out)
           (c gint :in-out))
 (defun gtk::entry-get-layout-offsets (a b c)
   (|gtk_entry_get_layout_offsets| a b c)))
(progn
 (defalien "gtk_entry_new_with_max_length" (* t) (a gint))
 (defun gtk::entry-new-with-max-length (a) (|gtk_entry_new_with_max_length| a)))
(progn
 (defalien "gtk_event_box_get_type" GType)
 (defun gtk::event-box-get-type () (|gtk_event_box_get_type|)))
(progn
 (defalien "gtk_event_box_new" (* t))
 (defun gtk::event-box-new () (|gtk_event_box_new|)))
(progn
 (defalien "gtk_file_selection_get_type" GType)
 (defun gtk::file-selection-get-type () (|gtk_file_selection_get_type|)))
(progn
 (defalien "gtk_file_selection_new" (* t) (a c-string))
 (defun gtk::file-selection-new (a) (|gtk_file_selection_new| a)))
(progn
 (defalien "gtk_file_selection_set_filename" void (a (* t)) (b c-string))
 (defun gtk::file-selection-set-filename (a b)
   (|gtk_file_selection_set_filename| a b)))
(progn
 (defalien "gtk_file_selection_get_filename" c-string (a (* t)))
 (defun gtk::file-selection-get-filename (a)
   (|gtk_file_selection_get_filename| a)))
(progn
 (defalien "gtk_file_selection_complete" void (a (* t)) (b c-string))
 (defun gtk::file-selection-complete (a b) (|gtk_file_selection_complete| a b)))
(progn
 (defalien "gtk_file_selection_show_fileop_buttons" void (a (* t)))
 (defun gtk::file-selection-show-fileop-buttons (a)
   (|gtk_file_selection_show_fileop_buttons| a)))
(progn
 (defalien "gtk_file_selection_hide_fileop_buttons" void (a (* t)))
 (defun gtk::file-selection-hide-fileop-buttons (a)
   (|gtk_file_selection_hide_fileop_buttons| a)))
(progn
 (defalien "gtk_file_selection_get_selections" (* t) (a (* t)))
 (defun gtk::file-selection-get-selections (a)
   (|gtk_file_selection_get_selections| a)))
(progn
 (defalien "gtk_file_selection_set_select_multiple"
           void
           (a (* t))
           (b gboolean))
 (defun gtk::file-selection-set-select-multiple (a b)
   (|gtk_file_selection_set_select_multiple| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gtk_file_selection_get_select_multiple" gboolean (a (* t)))
 (defun gtk::file-selection-get-select-multiple (a)
   (let ((v405 (|gtk_file_selection_get_select_multiple| a)))
     (if (= v405 1) t nil))))
(progn
 (defalien "gtk_fixed_get_type" GType)
 (defun gtk::fixed-get-type () (|gtk_fixed_get_type|)))
(progn
 (defalien "gtk_fixed_new" (* t))
 (defun gtk::fixed-new () (|gtk_fixed_new|)))
(progn
 (defalien "gtk_fixed_put" void (a (* t)) (b (* t)) (c gint) (d gint))
 (defun gtk::fixed-put (a b c d) (|gtk_fixed_put| a b c d)))
(progn
 (defalien "gtk_fixed_move" void (a (* t)) (b (* t)) (c gint) (d gint))
 (defun gtk::fixed-move (a b c d) (|gtk_fixed_move| a b c d)))
(progn
 (defalien "gtk_fixed_set_has_window" void (a (* t)) (b gboolean))
 (defun gtk::fixed-set-has-window (a b)
   (|gtk_fixed_set_has_window| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gtk_fixed_get_has_window" gboolean (a (* t)))
 (defun gtk::fixed-get-has-window (a)
   (let ((v406 (|gtk_fixed_get_has_window| a)))
     (if (= v406 1) t nil))))
(progn
 (defalien "gtk_font_selection_get_type" GType)
 (defun gtk::font-selection-get-type () (|gtk_font_selection_get_type|)))
(progn
 (defalien "gtk_font_selection_new" (* t))
 (defun gtk::font-selection-new () (|gtk_font_selection_new|)))
(progn
 (defalien "gtk_font_selection_get_font_name" gchar (a (* t)))
 (defun gtk::font-selection-get-font-name (a)
   (|gtk_font_selection_get_font_name| a)))
(progn
 (defalien "gtk_font_selection_set_font_name" gboolean (a (* t)) (b c-string))
 (defun gtk::font-selection-set-font-name (a b)
   (let ((v407 (|gtk_font_selection_set_font_name| a b)))
     (if (= v407 1) t nil))))
(progn
 (defalien "gtk_font_selection_get_preview_text" c-string (a (* t)))
 (defun gtk::font-selection-get-preview-text (a)
   (|gtk_font_selection_get_preview_text| a)))
(progn
 (defalien "gtk_font_selection_set_preview_text" void (a (* t)) (b c-string))
 (defun gtk::font-selection-set-preview-text (a b)
   (|gtk_font_selection_set_preview_text| a b)))
(progn
 (defalien "gtk_font_selection_dialog_get_type" GType)
 (defun gtk::font-selection-dialog-get-type ()
   (|gtk_font_selection_dialog_get_type|)))
(progn
 (defalien "gtk_font_selection_dialog_new" (* t) (a c-string))
 (defun gtk::font-selection-dialog-new (a) (|gtk_font_selection_dialog_new| a)))
(progn
 (defalien "gtk_font_selection_dialog_get_font_name" gchar (a (* t)))
 (defun gtk::font-selection-dialog-get-font-name (a)
   (|gtk_font_selection_dialog_get_font_name| a)))
(progn
 (defalien "gtk_font_selection_dialog_set_font_name"
           gboolean
           (a (* t))
           (b c-string))
 (defun gtk::font-selection-dialog-set-font-name (a b)
   (let ((v408 (|gtk_font_selection_dialog_set_font_name| a b)))
     (if (= v408 1) t nil))))
(progn
 (defalien "gtk_font_selection_dialog_get_preview_text" c-string (a (* t)))
 (defun gtk::font-selection-dialog-get-preview-text (a)
   (|gtk_font_selection_dialog_get_preview_text| a)))
(progn
 (defalien "gtk_font_selection_dialog_set_preview_text"
           void
           (a (* t))
           (b c-string))
 (defun gtk::font-selection-dialog-set-preview-text (a b)
   (|gtk_font_selection_dialog_set_preview_text| a b)))
(progn
 (defalien "gtk_gamma_curve_get_type" GType)
 (defun gtk::gamma-curve-get-type () (|gtk_gamma_curve_get_type|)))
(progn
 (defalien "gtk_gamma_curve_new" (* t))
 (defun gtk::gamma-curve-new () (|gtk_gamma_curve_new|)))
(progn
 (defalien "gtk_gc_get" (* t) (a gint) (b (* t)) (c (* t)) (d GdkGCValuesMask))
 (defun gtk::gc-get (a b c d) (|gtk_gc_get| a b c d)))
(progn
 (defalien "gtk_gc_release" void (a (* t)))
 (defun gtk::gc-release (a) (|gtk_gc_release| a)))
(progn
 (defalien "gtk_handle_box_get_type" GType)
 (defun gtk::handle-box-get-type () (|gtk_handle_box_get_type|)))
(progn
 (defalien "gtk_handle_box_new" (* t))
 (defun gtk::handle-box-new () (|gtk_handle_box_new|)))
(progn
 (defalien "gtk_handle_box_set_shadow_type" void (a (* t)) (b GtkShadowType))
 (defun gtk::handle-box-set-shadow-type (a b)
   (|gtk_handle_box_set_shadow_type| a b)))
(progn
 (defalien "gtk_handle_box_get_shadow_type" GtkShadowType (a (* t)))
 (defun gtk::handle-box-get-shadow-type (a)
   (|gtk_handle_box_get_shadow_type| a)))
(progn
 (defalien "gtk_handle_box_set_handle_position"
           void
           (a (* t))
           (b GtkPositionType))
 (defun gtk::handle-box-set-handle-position (a b)
   (|gtk_handle_box_set_handle_position| a b)))
(progn
 (defalien "gtk_handle_box_get_handle_position" GtkPositionType (a (* t)))
 (defun gtk::handle-box-get-handle-position (a)
   (|gtk_handle_box_get_handle_position| a)))
(progn
 (defalien "gtk_handle_box_set_snap_edge" void (a (* t)) (b GtkPositionType))
 (defun gtk::handle-box-set-snap-edge (a b)
   (|gtk_handle_box_set_snap_edge| a b)))
(progn
 (defalien "gtk_handle_box_get_snap_edge" GtkPositionType (a (* t)))
 (defun gtk::handle-box-get-snap-edge (a) (|gtk_handle_box_get_snap_edge| a)))
(progn
 (defalien "gtk_hbutton_box_get_type" GType)
 (defun gtk::hbutton-box-get-type () (|gtk_hbutton_box_get_type|)))
(progn
 (defalien "gtk_hbutton_box_new" (* t))
 (defun gtk::hbutton-box-new () (|gtk_hbutton_box_new|)))
(progn
 (defalien "gtk_paned_get_type" GType)
 (defun gtk::paned-get-type () (|gtk_paned_get_type|)))
(progn
 (defalien "gtk_paned_add1" void (a (* t)) (b (* t)))
 (defun gtk::paned-add1 (a b) (|gtk_paned_add1| a b)))
(progn
 (defalien "gtk_paned_add2" void (a (* t)) (b (* t)))
 (defun gtk::paned-add2 (a b) (|gtk_paned_add2| a b)))
(progn
 (defalien "gtk_paned_pack1"
           void
           (a (* t))
           (b (* t))
           (c gboolean)
           (d gboolean))
 (defun gtk::paned-pack1 (a b c d)
   (|gtk_paned_pack1| a b (if c (if (eq c 0) 0 1) 0)
    (if d (if (eq d 0) 0 1) 0))))
(progn
 (defalien "gtk_paned_pack2"
           void
           (a (* t))
           (b (* t))
           (c gboolean)
           (d gboolean))
 (defun gtk::paned-pack2 (a b c d)
   (|gtk_paned_pack2| a b (if c (if (eq c 0) 0 1) 0)
    (if d (if (eq d 0) 0 1) 0))))
(progn
 (defalien "gtk_paned_get_position" gint (a (* t)))
 (defun gtk::paned-get-position (a) (|gtk_paned_get_position| a)))
(progn
 (defalien "gtk_paned_set_position" void (a (* t)) (b gint))
 (defun gtk::paned-set-position (a b) (|gtk_paned_set_position| a b)))
(progn
 (defalien "gtk_paned_compute_position"
           void
           (a (* t))
           (b gint)
           (c gint)
           (d gint))
 (defun gtk::paned-compute-position (a b c d)
   (|gtk_paned_compute_position| a b c d)))
(progn
 (defalien "gtk_hpaned_get_type" GType)
 (defun gtk::hpaned-get-type () (|gtk_hpaned_get_type|)))
(progn
 (defalien "gtk_hpaned_new" (* t))
 (defun gtk::hpaned-new () (|gtk_hpaned_new|)))
(progn
 (defalien "gtk_ruler_get_type" GType)
 (defun gtk::ruler-get-type () (|gtk_ruler_get_type|)))
(progn
 (defalien "gtk_ruler_set_metric" void (a (* t)) (b GtkMetricType))
 (defun gtk::ruler-set-metric (a b) (|gtk_ruler_set_metric| a b)))
(progn
 (defalien "gtk_ruler_set_range"
           void
           (a (* t))
           (b gdouble)
           (c gdouble)
           (d gdouble)
           (e gdouble))
 (defun gtk::ruler-set-range (a b c d e)
   (|gtk_ruler_set_range| a (coerce b 'double-float) (coerce c 'double-float)
    (coerce d 'double-float) (coerce e 'double-float))))
(progn
 (defalien "gtk_ruler_draw_ticks" void (a (* t)))
 (defun gtk::ruler-draw-ticks (a) (|gtk_ruler_draw_ticks| a)))
(progn
 (defalien "gtk_ruler_draw_pos" void (a (* t)))
 (defun gtk::ruler-draw-pos (a) (|gtk_ruler_draw_pos| a)))
(progn
 (defalien "gtk_ruler_get_metric" GtkMetricType (a (* t)))
 (defun gtk::ruler-get-metric (a) (|gtk_ruler_get_metric| a)))
(progn
 (defalien "gtk_ruler_get_range"
           void
           (a (* t))
           (b gdouble :in-out)
           (c gdouble :in-out)
           (d gdouble :in-out)
           (e gdouble :in-out))
 (defun gtk::ruler-get-range (a b c d e)
   (|gtk_ruler_get_range| a (coerce b 'double-float) (coerce c 'double-float)
    (coerce d 'double-float) (coerce e 'double-float))))
(progn
 (defalien "gtk_hruler_get_type" GType)
 (defun gtk::hruler-get-type () (|gtk_hruler_get_type|)))
(progn
 (defalien "gtk_hruler_new" (* t))
 (defun gtk::hruler-new () (|gtk_hruler_new|)))
(progn
 (defalien "gtk_scale_get_type" GType)
 (defun gtk::scale-get-type () (|gtk_scale_get_type|)))
(progn
 (defalien "gtk_scale_set_digits" void (a (* t)) (b gint))
 (defun gtk::scale-set-digits (a b) (|gtk_scale_set_digits| a b)))
(progn
 (defalien "gtk_scale_get_digits" gint (a (* t)))
 (defun gtk::scale-get-digits (a) (|gtk_scale_get_digits| a)))
(progn
 (defalien "gtk_scale_set_draw_value" void (a (* t)) (b gboolean))
 (defun gtk::scale-set-draw-value (a b)
   (|gtk_scale_set_draw_value| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gtk_scale_get_draw_value" gboolean (a (* t)))
 (defun gtk::scale-get-draw-value (a)
   (let ((v409 (|gtk_scale_get_draw_value| a)))
     (if (= v409 1) t nil))))
(progn
 (defalien "gtk_scale_set_value_pos" void (a (* t)) (b GtkPositionType))
 (defun gtk::scale-set-value-pos (a b) (|gtk_scale_set_value_pos| a b)))
(progn
 (defalien "gtk_scale_get_value_pos" GtkPositionType (a (* t)))
 (defun gtk::scale-get-value-pos (a) (|gtk_scale_get_value_pos| a)))
(progn
 (defalien "gtk_hscale_get_type" GType)
 (defun gtk::hscale-get-type () (|gtk_hscale_get_type|)))
(progn
 (defalien "gtk_hscale_new" (* t) (a (* t)))
 (defun gtk::hscale-new (a) (|gtk_hscale_new| a)))
(progn
 (defalien "gtk_hscale_new_with_range"
           (* t)
           (a gdouble)
           (b gdouble)
           (c gdouble))
 (defun gtk::hscale-new-with-range (a b c)
   (|gtk_hscale_new_with_range| (coerce a 'double-float)
    (coerce b 'double-float) (coerce c 'double-float))))
(progn
 (defalien "gtk_separator_get_type" GType)
 (defun gtk::separator-get-type () (|gtk_separator_get_type|)))
(progn
 (defalien "gtk_hseparator_get_type" GType)
 (defun gtk::hseparator-get-type () (|gtk_hseparator_get_type|)))
(progn
 (defalien "gtk_hseparator_new" (* t))
 (defun gtk::hseparator-new () (|gtk_hseparator_new|)))
(progn
 (defalien "gtk_icon_factory_get_type" GType)
 (defun gtk::icon-factory-get-type () (|gtk_icon_factory_get_type|)))
(progn
 (defalien "gtk_icon_factory_new" (* t))
 (defun gtk::icon-factory-new () (|gtk_icon_factory_new|)))
(progn
 (defalien "gtk_icon_factory_add" void (a (* t)) (b c-string) (c (* t)))
 (defun gtk::icon-factory-add (a b c) (|gtk_icon_factory_add| a b c)))
(progn
 (defalien "gtk_icon_factory_lookup" (* t) (a (* t)) (b c-string))
 (defun gtk::icon-factory-lookup (a b) (|gtk_icon_factory_lookup| a b)))
(progn
 (defalien "gtk_icon_factory_add_default" void (a (* t)))
 (defun gtk::icon-factory-add-default (a) (|gtk_icon_factory_add_default| a)))
(progn
 (defalien "gtk_icon_factory_remove_default" void (a (* t)))
 (defun gtk::icon-factory-remove-default (a)
   (|gtk_icon_factory_remove_default| a)))
(progn
 (defalien "gtk_icon_factory_lookup_default" (* t) (a c-string))
 (defun gtk::icon-factory-lookup-default (a)
   (|gtk_icon_factory_lookup_default| a)))
(progn
 (defalien "gtk_icon_size_lookup"
           gboolean
           (a GtkIconSize)
           (b gint :in-out)
           (c gint :in-out))
 (defun gtk::icon-size-lookup (a b c)
   (let ((v410 (multiple-value-list (|gtk_icon_size_lookup| a b c))))
     (apply #'values (if (= 1 (car v410)) t nil) (cdr v410)))))
(progn
 (defalien "gtk_icon_size_register" GtkIconSize (a c-string) (b gint) (c gint))
 (defun gtk::icon-size-register (a b c) (|gtk_icon_size_register| a b c)))
(progn
 (defalien "gtk_icon_size_register_alias" void (a c-string) (b GtkIconSize))
 (defun gtk::icon-size-register-alias (a b)
   (|gtk_icon_size_register_alias| a b)))
(progn
 (defalien "gtk_icon_size_from_name" GtkIconSize (a c-string))
 (defun gtk::icon-size-from-name (a) (|gtk_icon_size_from_name| a)))
(progn
 (defalien "gtk_icon_size_get_name" c-string (a GtkIconSize))
 (defun gtk::icon-size-get-name (a) (|gtk_icon_size_get_name| a)))
(progn
 (defalien "gtk_icon_set_new" (* t))
 (defun gtk::icon-set-new () (|gtk_icon_set_new|)))
(progn
 (defalien "gtk_icon_set_new_from_pixbuf" (* t) (a (* t)))
 (defun gtk::icon-set-new-from-pixbuf (a) (|gtk_icon_set_new_from_pixbuf| a)))
(progn
 (defalien "gtk_icon_set_ref" (* t) (a (* t)))
 (defun gtk::icon-set-ref (a) (|gtk_icon_set_ref| a)))
(progn
 (defalien "gtk_icon_set_unref" void (a (* t)))
 (defun gtk::icon-set-unref (a) (|gtk_icon_set_unref| a)))
(progn
 (defalien "gtk_icon_set_copy" (* t) (a (* t)))
 (defun gtk::icon-set-copy (a) (|gtk_icon_set_copy| a)))
(progn
 (defalien "gtk_icon_set_render_icon"
           (* t)
           (a (* t))
           (b (* t))
           (c GtkTextDirection)
           (d GtkStateType)
           (e GtkIconSize)
           (f (* t))
           (g c-string))
 (defun gtk::icon-set-render-icon (a b c d e f g)
   (|gtk_icon_set_render_icon| a b c d e f g)))
(progn
 (defalien "gtk_icon_set_add_source" void (a (* t)) (b (* t)))
 (defun gtk::icon-set-add-source (a b) (|gtk_icon_set_add_source| a b)))
(progn
 (defalien "gtk_icon_set_get_sizes" void (a (* t)) (b (* t)) (c gint :in-out))
 (defun gtk::icon-set-get-sizes (a b c) (|gtk_icon_set_get_sizes| a b c)))
(progn
 (defalien "gtk_icon_source_get_type" GType)
 (defun gtk::icon-source-get-type () (|gtk_icon_source_get_type|)))
(progn
 (defalien "gtk_icon_source_new" (* t))
 (defun gtk::icon-source-new () (|gtk_icon_source_new|)))
(progn
 (defalien "gtk_icon_source_copy" (* t) (a (* t)))
 (defun gtk::icon-source-copy (a) (|gtk_icon_source_copy| a)))
(progn
 (defalien "gtk_icon_source_free" void (a (* t)))
 (defun gtk::icon-source-free (a) (|gtk_icon_source_free| a)))
(progn
 (defalien "gtk_icon_source_set_filename" void (a (* t)) (b c-string))
 (defun gtk::icon-source-set-filename (a b)
   (|gtk_icon_source_set_filename| a b)))
(progn
 (defalien "gtk_icon_source_set_pixbuf" void (a (* t)) (b (* t)))
 (defun gtk::icon-source-set-pixbuf (a b) (|gtk_icon_source_set_pixbuf| a b)))
(progn
 (defalien "gtk_icon_source_get_filename" c-string (a (* t)))
 (defun gtk::icon-source-get-filename (a) (|gtk_icon_source_get_filename| a)))
(progn
 (defalien "gtk_icon_source_get_pixbuf" (* t) (a (* t)))
 (defun gtk::icon-source-get-pixbuf (a) (|gtk_icon_source_get_pixbuf| a)))
(progn
 (defalien "gtk_icon_source_set_direction_wildcarded"
           void
           (a (* t))
           (b gboolean))
 (defun gtk::icon-source-set-direction-wildcarded (a b)
   (|gtk_icon_source_set_direction_wildcarded| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gtk_icon_source_set_state_wildcarded" void (a (* t)) (b gboolean))
 (defun gtk::icon-source-set-state-wildcarded (a b)
   (|gtk_icon_source_set_state_wildcarded| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gtk_icon_source_set_size_wildcarded" void (a (* t)) (b gboolean))
 (defun gtk::icon-source-set-size-wildcarded (a b)
   (|gtk_icon_source_set_size_wildcarded| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gtk_icon_source_get_size_wildcarded" gboolean (a (* t)))
 (defun gtk::icon-source-get-size-wildcarded (a)
   (let ((v411 (|gtk_icon_source_get_size_wildcarded| a)))
     (if (= v411 1) t nil))))
(progn
 (defalien "gtk_icon_source_get_state_wildcarded" gboolean (a (* t)))
 (defun gtk::icon-source-get-state-wildcarded (a)
   (let ((v412 (|gtk_icon_source_get_state_wildcarded| a)))
     (if (= v412 1) t nil))))
(progn
 (defalien "gtk_icon_source_get_direction_wildcarded" gboolean (a (* t)))
 (defun gtk::icon-source-get-direction-wildcarded (a)
   (let ((v413 (|gtk_icon_source_get_direction_wildcarded| a)))
     (if (= v413 1) t nil))))
(progn
 (defalien "gtk_icon_source_set_direction" void (a (* t)) (b GtkTextDirection))
 (defun gtk::icon-source-set-direction (a b)
   (|gtk_icon_source_set_direction| a b)))
(progn
 (defalien "gtk_icon_source_set_state" void (a (* t)) (b GtkStateType))
 (defun gtk::icon-source-set-state (a b) (|gtk_icon_source_set_state| a b)))
(progn
 (defalien "gtk_icon_source_set_size" void (a (* t)) (b GtkIconSize))
 (defun gtk::icon-source-set-size (a b) (|gtk_icon_source_set_size| a b)))
(progn
 (defalien "gtk_icon_source_get_direction" GtkTextDirection (a (* t)))
 (defun gtk::icon-source-get-direction (a) (|gtk_icon_source_get_direction| a)))
(progn
 (defalien "gtk_icon_source_get_state" GtkStateType (a (* t)))
 (defun gtk::icon-source-get-state (a) (|gtk_icon_source_get_state| a)))
(progn
 (defalien "gtk_icon_source_get_size" GtkIconSize (a (* t)))
 (defun gtk::icon-source-get-size (a) (|gtk_icon_source_get_size| a)))
(progn
 (defalien "gtk_image_get_type" GType)
 (defun gtk::image-get-type () (|gtk_image_get_type|)))
(progn
 (defalien "gtk_image_new" (* t))
 (defun gtk::image-new () (|gtk_image_new|)))
(progn
 (defalien "gtk_image_new_from_pixmap" (* t) (a (* t)) (b (* t)))
 (defun gtk::image-new-from-pixmap (a b) (|gtk_image_new_from_pixmap| a b)))
(progn
 (defalien "gtk_image_new_from_image" (* t) (a (* t)) (b (* t)))
 (defun gtk::image-new-from-image (a b) (|gtk_image_new_from_image| a b)))
(progn
 (defalien "gtk_image_new_from_file" (* t) (a c-string))
 (defun gtk::image-new-from-file (a) (|gtk_image_new_from_file| a)))
(progn
 (defalien "gtk_image_new_from_pixbuf" (* t) (a (* t)))
 (defun gtk::image-new-from-pixbuf (a) (|gtk_image_new_from_pixbuf| a)))
(progn
 (defalien "gtk_image_new_from_stock" (* t) (a c-string) (b GtkIconSize))
 (defun gtk::image-new-from-stock (a b) (|gtk_image_new_from_stock| a b)))
(progn
 (defalien "gtk_image_new_from_icon_set" (* t) (a (* t)) (b GtkIconSize))
 (defun gtk::image-new-from-icon-set (a b) (|gtk_image_new_from_icon_set| a b)))
(progn
 (defalien "gtk_image_new_from_animation" (* t) (a (* t)))
 (defun gtk::image-new-from-animation (a) (|gtk_image_new_from_animation| a)))
(progn
 (defalien "gtk_image_set_from_pixmap" void (a (* t)) (b (* t)) (c (* t)))
 (defun gtk::image-set-from-pixmap (a b c) (|gtk_image_set_from_pixmap| a b c)))
(progn
 (defalien "gtk_image_set_from_image" void (a (* t)) (b (* t)) (c (* t)))
 (defun gtk::image-set-from-image (a b c) (|gtk_image_set_from_image| a b c)))
(progn
 (defalien "gtk_image_set_from_file" void (a (* t)) (b c-string))
 (defun gtk::image-set-from-file (a b) (|gtk_image_set_from_file| a b)))
(progn
 (defalien "gtk_image_set_from_pixbuf" void (a (* t)) (b (* t)))
 (defun gtk::image-set-from-pixbuf (a b) (|gtk_image_set_from_pixbuf| a b)))
(progn
 (defalien "gtk_image_set_from_stock"
           void
           (a (* t))
           (b c-string)
           (c GtkIconSize))
 (defun gtk::image-set-from-stock (a b c) (|gtk_image_set_from_stock| a b c)))
(progn
 (defalien "gtk_image_set_from_icon_set"
           void
           (a (* t))
           (b (* t))
           (c GtkIconSize))
 (defun gtk::image-set-from-icon-set (a b c)
   (|gtk_image_set_from_icon_set| a b c)))
(progn
 (defalien "gtk_image_set_from_animation" void (a (* t)) (b (* t)))
 (defun gtk::image-set-from-animation (a b)
   (|gtk_image_set_from_animation| a b)))
(progn
 (defalien "gtk_image_get_storage_type" GtkImageType (a (* t)))
 (defun gtk::image-get-storage-type (a) (|gtk_image_get_storage_type| a)))
(progn
 (defalien "gtk_image_get_pixmap" void (a (* t)) (b (* t)) (c (* t)))
 (defun gtk::image-get-pixmap (a b c) (|gtk_image_get_pixmap| a b c)))
(progn
 (defalien "gtk_image_get_image" void (a (* t)) (b (* t)) (c (* t)))
 (defun gtk::image-get-image (a b c) (|gtk_image_get_image| a b c)))
(progn
 (defalien "gtk_image_get_pixbuf" (* t) (a (* t)))
 (defun gtk::image-get-pixbuf (a) (|gtk_image_get_pixbuf| a)))
(progn
 (defalien "gtk_image_get_stock"
           void
           (a (* t))
           (b (* t))
           (c GtkIconSize :in-out))
 (defun gtk::image-get-stock (a b c) (|gtk_image_get_stock| a b c)))
(progn
 (defalien "gtk_image_get_icon_set"
           void
           (a (* t))
           (b (* t))
           (c GtkIconSize :in-out))
 (defun gtk::image-get-icon-set (a b c) (|gtk_image_get_icon_set| a b c)))
(progn
 (defalien "gtk_image_get_animation" (* t) (a (* t)))
 (defun gtk::image-get-animation (a) (|gtk_image_get_animation| a)))
(progn
 (defalien "gtk_image_menu_item_get_type" GType)
 (defun gtk::image-menu-item-get-type () (|gtk_image_menu_item_get_type|)))
(progn
 (defalien "gtk_image_menu_item_new" (* t))
 (defun gtk::image-menu-item-new () (|gtk_image_menu_item_new|)))
(progn
 (defalien "gtk_image_menu_item_new_with_label" (* t) (a c-string))
 (defun gtk::image-menu-item-new-with-label (a)
   (|gtk_image_menu_item_new_with_label| a)))
(progn
 (defalien "gtk_image_menu_item_new_with_mnemonic" (* t) (a c-string))
 (defun gtk::image-menu-item-new-with-mnemonic (a)
   (|gtk_image_menu_item_new_with_mnemonic| a)))
(progn
 (defalien "gtk_image_menu_item_new_from_stock" (* t) (a c-string) (b (* t)))
 (defun gtk::image-menu-item-new-from-stock (a b)
   (|gtk_image_menu_item_new_from_stock| a b)))
(progn
 (defalien "gtk_image_menu_item_set_image" void (a (* t)) (b (* t)))
 (defun gtk::image-menu-item-set-image (a b)
   (|gtk_image_menu_item_set_image| a b)))
(progn
 (defalien "gtk_image_menu_item_get_image" (* t) (a (* t)))
 (defun gtk::image-menu-item-get-image (a) (|gtk_image_menu_item_get_image| a)))
(progn
 (defalien "gtk_im_context_simple_get_type" GType)
 (defun gtk::im-context-simple-get-type () (|gtk_im_context_simple_get_type|)))
(progn
 (defalien "gtk_im_context_simple_new" (* t))
 (defun gtk::im-context-simple-new () (|gtk_im_context_simple_new|)))
(progn
 (defalien "gtk_im_context_simple_add_table"
           void
           (a (* t))
           (b guint16 :in-out)
           (c gint)
           (d gint))
 (defun gtk::im-context-simple-add-table (a b c d)
   (|gtk_im_context_simple_add_table| a b c d)))
(progn
 (defalien "gtk_im_multicontext_get_type" GType)
 (defun gtk::im-multicontext-get-type () (|gtk_im_multicontext_get_type|)))
(progn
 (defalien "gtk_im_multicontext_new" (* t))
 (defun gtk::im-multicontext-new () (|gtk_im_multicontext_new|)))
(progn
 (defalien "gtk_im_multicontext_append_menuitems" void (a (* t)) (b (* t)))
 (defun gtk::im-multicontext-append-menuitems (a b)
   (|gtk_im_multicontext_append_menuitems| a b)))
(progn
 (defalien "gtk_input_dialog_get_type" GType)
 (defun gtk::input-dialog-get-type () (|gtk_input_dialog_get_type|)))
(progn
 (defalien "gtk_input_dialog_new" (* t))
 (defun gtk::input-dialog-new () (|gtk_input_dialog_new|)))
(progn
 (defalien "gtk_invisible_get_type" GType)
 (defun gtk::invisible-get-type () (|gtk_invisible_get_type|)))
(progn
 (defalien "gtk_invisible_new" (* t))
 (defun gtk::invisible-new () (|gtk_invisible_new|)))
(progn
 (defalien "gtk_item_factory_get_type" GType)
 (defun gtk::item-factory-get-type () (|gtk_item_factory_get_type|)))
(progn
 (defalien "gtk_item_factory_new" (* t) (a GType) (b c-string) (c (* t)))
 (defun gtk::item-factory-new (a b c) (|gtk_item_factory_new| a b c)))
(progn
 (defalien "gtk_item_factory_construct"
           void
           (a (* t))
           (b GType)
           (c c-string)
           (d (* t)))
 (defun gtk::item-factory-construct (a b c d)
   (|gtk_item_factory_construct| a b c d)))
(progn
 (defalien "gtk_item_factory_add_foreign"
           void
           (a (* t))
           (b c-string)
           (c (* t))
           (d guint)
           (e GdkModifierType))
 (defun gtk::item-factory-add-foreign (a b c d e)
   (|gtk_item_factory_add_foreign| a b c d e)))
(progn
 (defalien "gtk_item_factory_from_widget" (* t) (a (* t)))
 (defun gtk::item-factory-from-widget (a) (|gtk_item_factory_from_widget| a)))
(progn
 (defalien "gtk_item_factory_path_from_widget" c-string (a (* t)))
 (defun gtk::item-factory-path-from-widget (a)
   (|gtk_item_factory_path_from_widget| a)))
(progn
 (defalien "gtk_item_factory_get_item" (* t) (a (* t)) (b c-string))
 (defun gtk::item-factory-get-item (a b) (|gtk_item_factory_get_item| a b)))
(progn
 (defalien "gtk_item_factory_get_widget" (* t) (a (* t)) (b c-string))
 (defun gtk::item-factory-get-widget (a b) (|gtk_item_factory_get_widget| a b)))
(progn
 (defalien "gtk_item_factory_get_widget_by_action" (* t) (a (* t)) (b guint))
 (defun gtk::item-factory-get-widget-by-action (a b)
   (|gtk_item_factory_get_widget_by_action| a b)))
(progn
 (defalien "gtk_item_factory_get_item_by_action" (* t) (a (* t)) (b guint))
 (defun gtk::item-factory-get-item-by-action (a b)
   (|gtk_item_factory_get_item_by_action| a b)))
(progn
 (defalien "gtk_item_factory_create_item"
           void
           (a (* t))
           (b (* t))
           (c gpointer)
           (d guint))
 (defun gtk::item-factory-create-item (a b c d)
   (|gtk_item_factory_create_item| a b c d)))
(progn
 (defalien "gtk_item_factory_create_items"
           void
           (a (* t))
           (b guint)
           (c (* t))
           (d gpointer))
 (defun gtk::item-factory-create-items (a b c d)
   (|gtk_item_factory_create_items| a b c d)))
(progn
 (defalien "gtk_item_factory_delete_item" void (a (* t)) (b c-string))
 (defun gtk::item-factory-delete-item (a b)
   (|gtk_item_factory_delete_item| a b)))
(progn
 (defalien "gtk_item_factory_delete_entry" void (a (* t)) (b (* t)))
 (defun gtk::item-factory-delete-entry (a b)
   (|gtk_item_factory_delete_entry| a b)))
(progn
 (defalien "gtk_item_factory_delete_entries"
           void
           (a (* t))
           (b guint)
           (c (* t)))
 (defun gtk::item-factory-delete-entries (a b c)
   (|gtk_item_factory_delete_entries| a b c)))
(progn
 (defalien "gtk_item_factory_popup"
           void
           (a (* t))
           (b guint)
           (c guint)
           (d guint)
           (e guint32))
 (defun gtk::item-factory-popup (a b c d e)
   (|gtk_item_factory_popup| a b c d e)))
(progn
 (defalien "gtk_item_factory_popup_with_data"
           void
           (a (* t))
           (b gpointer)
           (c GtkDestroyNotify)
           (d guint)
           (e guint)
           (f guint)
           (g guint32))
 (defun gtk::item-factory-popup-with-data (a b c d e f g)
   (|gtk_item_factory_popup_with_data| a b c d e f g)))
(progn
 (defalien "gtk_item_factory_popup_data" gpointer (a (* t)))
 (defun gtk::item-factory-popup-data (a) (|gtk_item_factory_popup_data| a)))
(progn
 (defalien "gtk_item_factory_popup_data_from_widget" gpointer (a (* t)))
 (defun gtk::item-factory-popup-data-from-widget (a)
   (|gtk_item_factory_popup_data_from_widget| a)))
(progn
 (defalien "gtk_item_factory_set_translate_func"
           void
           (a (* t))
           (b GtkTranslateFunc)
           (c gpointer)
           (d GtkDestroyNotify))
 (defun gtk::item-factory-set-translate-func (a b c d)
   (|gtk_item_factory_set_translate_func| a b c d)))
(progn
 (defalien "gtk_layout_get_type" GType)
 (defun gtk::layout-get-type () (|gtk_layout_get_type|)))
(progn
 (defalien "gtk_layout_new" (* t) (a (* t)) (b (* t)))
 (defun gtk::layout-new (a b) (|gtk_layout_new| a b)))
(progn
 (defalien "gtk_layout_put" void (a (* t)) (b (* t)) (c gint) (d gint))
 (defun gtk::layout-put (a b c d) (|gtk_layout_put| a b c d)))
(progn
 (defalien "gtk_layout_move" void (a (* t)) (b (* t)) (c gint) (d gint))
 (defun gtk::layout-move (a b c d) (|gtk_layout_move| a b c d)))
(progn
 (defalien "gtk_layout_set_size" void (a (* t)) (b guint) (c guint))
 (defun gtk::layout-set-size (a b c) (|gtk_layout_set_size| a b c)))
(progn
 (defalien "gtk_layout_get_size"
           void
           (a (* t))
           (b guint :in-out)
           (c guint :in-out))
 (defun gtk::layout-get-size (a b c) (|gtk_layout_get_size| a b c)))
(progn
 (defalien "gtk_layout_get_hadjustment" (* t) (a (* t)))
 (defun gtk::layout-get-hadjustment (a) (|gtk_layout_get_hadjustment| a)))
(progn
 (defalien "gtk_layout_get_vadjustment" (* t) (a (* t)))
 (defun gtk::layout-get-vadjustment (a) (|gtk_layout_get_vadjustment| a)))
(progn
 (defalien "gtk_layout_set_hadjustment" void (a (* t)) (b (* t)))
 (defun gtk::layout-set-hadjustment (a b) (|gtk_layout_set_hadjustment| a b)))
(progn
 (defalien "gtk_layout_set_vadjustment" void (a (* t)) (b (* t)))
 (defun gtk::layout-set-vadjustment (a b) (|gtk_layout_set_vadjustment| a b)))
(progn
 (defalien "gtk_list_item_get_type" GtkType)
 (defun gtk::list-item-get-type () (|gtk_list_item_get_type|)))
(progn
 (defalien "gtk_list_item_new" (* t))
 (defun gtk::list-item-new () (|gtk_list_item_new|)))
(progn
 (defalien "gtk_list_item_new_with_label" (* t) (a c-string))
 (defun gtk::list-item-new-with-label (a) (|gtk_list_item_new_with_label| a)))
(progn
 (defalien "gtk_list_item_select" void (a (* t)))
 (defun gtk::list-item-select (a) (|gtk_list_item_select| a)))
(progn
 (defalien "gtk_list_item_deselect" void (a (* t)))
 (defun gtk::list-item-deselect (a) (|gtk_list_item_deselect| a)))
(progn
 (defalien "gtk_list_insert_items" void (a (* t)) (b (* t)) (c gint))
 (defun gtk::list-insert-items (a b c) (|gtk_list_insert_items| a b c)))
(progn
 (defalien "gtk_list_append_items" void (a (* t)) (b (* t)))
 (defun gtk::list-append-items (a b) (|gtk_list_append_items| a b)))
(progn
 (defalien "gtk_list_prepend_items" void (a (* t)) (b (* t)))
 (defun gtk::list-prepend-items (a b) (|gtk_list_prepend_items| a b)))
(progn
 (defalien "gtk_list_remove_items" void (a (* t)) (b (* t)))
 (defun gtk::list-remove-items (a b) (|gtk_list_remove_items| a b)))
(progn
 (defalien "gtk_list_clear_items" void (a (* t)) (b gint) (c gint))
 (defun gtk::list-clear-items (a b c) (|gtk_list_clear_items| a b c)))
(progn
 (defalien "gtk_list_select_item" void (a (* t)) (b gint))
 (defun gtk::list-select-item (a b) (|gtk_list_select_item| a b)))
(progn
 (defalien "gtk_list_unselect_item" void (a (* t)) (b gint))
 (defun gtk::list-unselect-item (a b) (|gtk_list_unselect_item| a b)))
(progn
 (defalien "gtk_list_select_child" void (a (* t)) (b (* t)))
 (defun gtk::list-select-child (a b) (|gtk_list_select_child| a b)))
(progn
 (defalien "gtk_list_unselect_child" void (a (* t)) (b (* t)))
 (defun gtk::list-unselect-child (a b) (|gtk_list_unselect_child| a b)))
(progn
 (defalien "gtk_list_child_position" gint (a (* t)) (b (* t)))
 (defun gtk::list-child-position (a b) (|gtk_list_child_position| a b)))
(progn
 (defalien "gtk_tree_path_new" (* t))
 (defun gtk::tree-path-new () (|gtk_tree_path_new|)))
(progn
 (defalien "gtk_tree_path_new_from_string" (* t) (a c-string))
 (defun gtk::tree-path-new-from-string (a) (|gtk_tree_path_new_from_string| a)))
(progn
 (defalien "gtk_tree_path_to_string" gchar (a (* t)))
 (defun gtk::tree-path-to-string (a) (|gtk_tree_path_to_string| a)))
(progn
 (defalien "gtk_tree_path_new_first" (* t))
 (defun gtk::tree-path-new-first () (|gtk_tree_path_new_first|)))
(progn
 (defalien "gtk_tree_path_append_index" void (a (* t)) (b gint))
 (defun gtk::tree-path-append-index (a b) (|gtk_tree_path_append_index| a b)))
(progn
 (defalien "gtk_tree_path_prepend_index" void (a (* t)) (b gint))
 (defun gtk::tree-path-prepend-index (a b) (|gtk_tree_path_prepend_index| a b)))
(progn
 (defalien "gtk_tree_path_get_depth" gint (a (* t)))
 (defun gtk::tree-path-get-depth (a) (|gtk_tree_path_get_depth| a)))
(progn
 (defalien "gtk_tree_path_get_indices" gint (a (* t)))
 (defun gtk::tree-path-get-indices (a) (|gtk_tree_path_get_indices| a)))
(progn
 (defalien "gtk_tree_path_free" void (a (* t)))
 (defun gtk::tree-path-free (a) (|gtk_tree_path_free| a)))
(progn
 (defalien "gtk_tree_path_copy" (* t) (a (* t)))
 (defun gtk::tree-path-copy (a) (|gtk_tree_path_copy| a)))
(progn
 (defalien "gtk_tree_path_compare" gint (a (* t)) (b (* t)))
 (defun gtk::tree-path-compare (a b) (|gtk_tree_path_compare| a b)))
(progn
 (defalien "gtk_tree_path_next" void (a (* t)))
 (defun gtk::tree-path-next (a) (|gtk_tree_path_next| a)))
(progn
 (defalien "gtk_tree_path_prev" gboolean (a (* t)))
 (defun gtk::tree-path-prev (a)
   (let ((v414 (|gtk_tree_path_prev| a)))
     (if (= v414 1) t nil))))
(progn
 (defalien "gtk_tree_path_up" gboolean (a (* t)))
 (defun gtk::tree-path-up (a)
   (let ((v415 (|gtk_tree_path_up| a)))
     (if (= v415 1) t nil))))
(progn
 (defalien "gtk_tree_path_down" void (a (* t)))
 (defun gtk::tree-path-down (a) (|gtk_tree_path_down| a)))
(progn
 (defalien "gtk_tree_path_is_ancestor" gboolean (a (* t)) (b (* t)))
 (defun gtk::tree-path-is-ancestor (a b)
   (let ((v416 (|gtk_tree_path_is_ancestor| a b)))
     (if (= v416 1) t nil))))
(progn
 (defalien "gtk_tree_path_is_descendant" gboolean (a (* t)) (b (* t)))
 (defun gtk::tree-path-is-descendant (a b)
   (let ((v417 (|gtk_tree_path_is_descendant| a b)))
     (if (= v417 1) t nil))))
(progn
 (defalien "gtk_tree_row_reference_new" (* t) (a (* t)) (b (* t)))
 (defun gtk::tree-row-reference-new (a b) (|gtk_tree_row_reference_new| a b)))
(progn
 (defalien "gtk_tree_row_reference_new_proxy"
           (* t)
           (a (* t))
           (b (* t))
           (c (* t)))
 (defun gtk::tree-row-reference-new-proxy (a b c)
   (|gtk_tree_row_reference_new_proxy| a b c)))
(progn
 (defalien "gtk_tree_row_reference_get_path" (* t) (a (* t)))
 (defun gtk::tree-row-reference-get-path (a)
   (|gtk_tree_row_reference_get_path| a)))
(progn
 (defalien "gtk_tree_row_reference_valid" gboolean (a (* t)))
 (defun gtk::tree-row-reference-valid (a)
   (let ((v418 (|gtk_tree_row_reference_valid| a)))
     (if (= v418 1) t nil))))
(progn
 (defalien "gtk_tree_row_reference_free" void (a (* t)))
 (defun gtk::tree-row-reference-free (a) (|gtk_tree_row_reference_free| a)))
(progn
 (defalien "gtk_tree_row_reference_inserted" void (a (* t)) (b (* t)))
 (defun gtk::tree-row-reference-inserted (a b)
   (|gtk_tree_row_reference_inserted| a b)))
(progn
 (defalien "gtk_tree_row_reference_deleted" void (a (* t)) (b (* t)))
 (defun gtk::tree-row-reference-deleted (a b)
   (|gtk_tree_row_reference_deleted| a b)))
(progn
 (defalien "gtk_tree_row_reference_reordered"
           void
           (a (* t))
           (b (* t))
           (c (* t))
           (d gint :in-out))
 (defun gtk::tree-row-reference-reordered (a b c d)
   (|gtk_tree_row_reference_reordered| a b c d)))
(progn
 (defalien "gtk_tree_iter_copy" (* t) (a (* t)))
 (defun gtk::tree-iter-copy (a) (|gtk_tree_iter_copy| a)))
(progn
 (defalien "gtk_tree_iter_free" void (a (* t)))
 (defun gtk::tree-iter-free (a) (|gtk_tree_iter_free| a)))
(progn
 (defalien "gtk_tree_iter_get_type" GType)
 (defun gtk::tree-iter-get-type () (|gtk_tree_iter_get_type|)))
(progn
 (defalien "gtk_tree_model_get_type" GType)
 (defun gtk::tree-model-get-type () (|gtk_tree_model_get_type|)))
(progn
 (defalien "gtk_tree_model_get_flags" GtkTreeModelFlags (a (* t)))
 (defun gtk::tree-model-get-flags (a) (|gtk_tree_model_get_flags| a)))
(progn
 (defalien "gtk_tree_model_get_n_columns" gint (a (* t)))
 (defun gtk::tree-model-get-n-columns (a) (|gtk_tree_model_get_n_columns| a)))
(progn
 (defalien "gtk_tree_model_get_column_type" GType (a (* t)) (b gint))
 (defun gtk::tree-model-get-column-type (a b)
   (|gtk_tree_model_get_column_type| a b)))
(progn
 (defalien "gtk_tree_model_get_iter" gboolean (a (* t)) (b (* t)) (c (* t)))
 (defun gtk::tree-model-get-iter (a b c)
   (let ((v419 (|gtk_tree_model_get_iter| a b c)))
     (if (= v419 1) t nil))))
(progn
 (defalien "gtk_tree_model_get_iter_from_string"
           gboolean
           (a (* t))
           (b (* t))
           (c c-string))
 (defun gtk::tree-model-get-iter-from-string (a b c)
   (let ((v420 (|gtk_tree_model_get_iter_from_string| a b c)))
     (if (= v420 1) t nil))))
(progn
 (defalien "gtk_tree_model_get_iter_first" gboolean (a (* t)) (b (* t)))
 (defun gtk::tree-model-get-iter-first (a b)
   (let ((v421 (|gtk_tree_model_get_iter_first| a b)))
     (if (= v421 1) t nil))))
(progn
 (defalien "gtk_tree_model_get_path" (* t) (a (* t)) (b (* t)))
 (defun gtk::tree-model-get-path (a b) (|gtk_tree_model_get_path| a b)))
(progn
 (defalien "gtk_tree_model_get_value"
           void
           (a (* t))
           (b (* t))
           (c gint)
           (d (* t)))
 (defun gtk::tree-model-get-value (a b c d)
   (|gtk_tree_model_get_value| a b c d)))
(progn
 (defalien "gtk_tree_model_iter_next" gboolean (a (* t)) (b (* t)))
 (defun gtk::tree-model-iter-next (a b)
   (let ((v422 (|gtk_tree_model_iter_next| a b)))
     (if (= v422 1) t nil))))
(progn
 (defalien "gtk_tree_model_iter_children"
           gboolean
           (a (* t))
           (b (* t))
           (c (* t)))
 (defun gtk::tree-model-iter-children (a b c)
   (let ((v423 (|gtk_tree_model_iter_children| a b c)))
     (if (= v423 1) t nil))))
(progn
 (defalien "gtk_tree_model_iter_has_child" gboolean (a (* t)) (b (* t)))
 (defun gtk::tree-model-iter-has-child (a b)
   (let ((v424 (|gtk_tree_model_iter_has_child| a b)))
     (if (= v424 1) t nil))))
(progn
 (defalien "gtk_tree_model_iter_n_children" gint (a (* t)) (b (* t)))
 (defun gtk::tree-model-iter-n-children (a b)
   (|gtk_tree_model_iter_n_children| a b)))
(progn
 (defalien "gtk_tree_model_iter_nth_child"
           gboolean
           (a (* t))
           (b (* t))
           (c (* t))
           (d gint))
 (defun gtk::tree-model-iter-nth-child (a b c d)
   (let ((v425 (|gtk_tree_model_iter_nth_child| a b c d)))
     (if (= v425 1) t nil))))
(progn
 (defalien "gtk_tree_model_iter_parent" gboolean (a (* t)) (b (* t)) (c (* t)))
 (defun gtk::tree-model-iter-parent (a b c)
   (let ((v426 (|gtk_tree_model_iter_parent| a b c)))
     (if (= v426 1) t nil))))
(progn
 (defalien "gtk_tree_model_ref_node" void (a (* t)) (b (* t)))
 (defun gtk::tree-model-ref-node (a b) (|gtk_tree_model_ref_node| a b)))
(progn
 (defalien "gtk_tree_model_unref_node" void (a (* t)) (b (* t)))
 (defun gtk::tree-model-unref-node (a b) (|gtk_tree_model_unref_node| a b)))
(progn
 (defalien "gtk_tree_model_get" void (a (* t)) (b (* t)))
 (defun gtk::tree-model-get (a b) (|gtk_tree_model_get| a b)))
(progn
 (defalien "gtk_tree_model_get_valist" void (a (* t)) (b (* t)) (c va_list))
 (defun gtk::tree-model-get-valist (a b c) (|gtk_tree_model_get_valist| a b c)))
(progn
 (defalien "gtk_tree_model_foreach"
           void
           (a (* t))
           (b GtkTreeModelForeachFunc)
           (c gpointer))
 (defun gtk::tree-model-foreach (a b c) (|gtk_tree_model_foreach| a b c)))
(progn
 (defalien "gtk_tree_model_row_changed" void (a (* t)) (b (* t)) (c (* t)))
 (defun gtk::tree-model-row-changed (a b c)
   (|gtk_tree_model_row_changed| a b c)))
(progn
 (defalien "gtk_tree_model_row_inserted" void (a (* t)) (b (* t)) (c (* t)))
 (defun gtk::tree-model-row-inserted (a b c)
   (|gtk_tree_model_row_inserted| a b c)))
(progn
 (defalien "gtk_tree_model_row_has_child_toggled"
           void
           (a (* t))
           (b (* t))
           (c (* t)))
 (defun gtk::tree-model-row-has-child-toggled (a b c)
   (|gtk_tree_model_row_has_child_toggled| a b c)))
(progn
 (defalien "gtk_tree_model_row_deleted" void (a (* t)) (b (* t)))
 (defun gtk::tree-model-row-deleted (a b) (|gtk_tree_model_row_deleted| a b)))
(progn
 (defalien "gtk_tree_model_rows_reordered"
           void
           (a (* t))
           (b (* t))
           (c (* t))
           (d gint :in-out))
 (defun gtk::tree-model-rows-reordered (a b c d)
   (|gtk_tree_model_rows_reordered| a b c d)))
(progn
 (defalien "gtk_tree_sortable_get_type" GType)
 (defun gtk::tree-sortable-get-type () (|gtk_tree_sortable_get_type|)))
(progn
 (defalien "gtk_tree_sortable_sort_column_changed" void (a (* t)))
 (defun gtk::tree-sortable-sort-column-changed (a)
   (|gtk_tree_sortable_sort_column_changed| a)))
(progn
 (defalien "gtk_tree_sortable_get_sort_column_id"
           gboolean
           (a (* t))
           (b gint :in-out)
           (c GtkSortType :in-out))
 (defun gtk::tree-sortable-get-sort-column-id (a b c)
   (let ((v427
          (multiple-value-list (|gtk_tree_sortable_get_sort_column_id| a b c))))
     (apply #'values (if (= 1 (car v427)) t nil) (cdr v427)))))
(progn
 (defalien "gtk_tree_sortable_set_sort_column_id"
           void
           (a (* t))
           (b gint)
           (c GtkSortType))
 (defun gtk::tree-sortable-set-sort-column-id (a b c)
   (|gtk_tree_sortable_set_sort_column_id| a b c)))
(progn
 (defalien "gtk_tree_sortable_set_sort_func"
           void
           (a (* t))
           (b gint)
           (c GtkTreeIterCompareFunc)
           (d gpointer)
           (e GtkDestroyNotify))
 (defun gtk::tree-sortable-set-sort-func (a b c d e)
   (|gtk_tree_sortable_set_sort_func| a b c d e)))
(progn
 (defalien "gtk_tree_sortable_set_default_sort_func"
           void
           (a (* t))
           (b GtkTreeIterCompareFunc)
           (c gpointer)
           (d GtkDestroyNotify))
 (defun gtk::tree-sortable-set-default-sort-func (a b c d)
   (|gtk_tree_sortable_set_default_sort_func| a b c d)))
(progn
 (defalien "gtk_tree_sortable_has_default_sort_func" gboolean (a (* t)))
 (defun gtk::tree-sortable-has-default-sort-func (a)
   (let ((v428 (|gtk_tree_sortable_has_default_sort_func| a)))
     (if (= v428 1) t nil))))
(progn
 (defalien "gtk_list_store_get_type" GType)
 (defun gtk::list-store-get-type () (|gtk_list_store_get_type|)))
(progn
 (defalien "gtk_list_store_new" (* t) (a gint))
 (defun gtk::list-store-new (a) (|gtk_list_store_new| a)))
(progn
 (defalien "gtk_list_store_newv" (* t) (a gint) (b (* t)))
 (defun gtk::list-store-newv (a b) (|gtk_list_store_newv| a b)))
(progn
 (defalien "gtk_list_store_set_column_types" void (a (* t)) (b gint) (c (* t)))
 (defun gtk::list-store-set-column-types (a b c)
   (|gtk_list_store_set_column_types| a b c)))
(progn
 (defalien "gtk_list_store_set_value"
           void
           (a (* t))
           (b (* t))
           (c gint)
           (d (* t)))
 (defun gtk::list-store-set-value (a b c d)
   (|gtk_list_store_set_value| a b c d)))
(progn
 (defalien "gtk_list_store_set" void (a (* t)) (b (* t)))
 (defun gtk::list-store-set (a b) (|gtk_list_store_set| a b)))
(progn
 (defalien "gtk_list_store_set_valist" void (a (* t)) (b (* t)) (c va_list))
 (defun gtk::list-store-set-valist (a b c) (|gtk_list_store_set_valist| a b c)))
(progn
 (defalien "gtk_list_store_remove" gboolean (a (* t)) (b (* t)))
 (defun gtk::list-store-remove (a b)
   (let ((v429 (|gtk_list_store_remove| a b)))
     (if (= v429 1) t nil))))
(progn
 (defalien "gtk_list_store_insert" void (a (* t)) (b (* t)) (c gint))
 (defun gtk::list-store-insert (a b c) (|gtk_list_store_insert| a b c)))
(progn
 (defalien "gtk_list_store_insert_before" void (a (* t)) (b (* t)) (c (* t)))
 (defun gtk::list-store-insert-before (a b c)
   (|gtk_list_store_insert_before| a b c)))
(progn
 (defalien "gtk_list_store_insert_after" void (a (* t)) (b (* t)) (c (* t)))
 (defun gtk::list-store-insert-after (a b c)
   (|gtk_list_store_insert_after| a b c)))
(progn
 (defalien "gtk_list_store_prepend" void (a (* t)) (b (* t)))
 (defun gtk::list-store-prepend (a b) (|gtk_list_store_prepend| a b)))
(progn
 (defalien "gtk_list_store_append" void (a (* t)) (b (* t)))
 (defun gtk::list-store-append (a b) (|gtk_list_store_append| a b)))
(progn
 (defalien "gtk_list_store_clear" void (a (* t)))
 (defun gtk::list-store-clear (a) (|gtk_list_store_clear| a)))
(progn
 (defalien "gtk_check_version" gchar (a guint) (b guint) (c guint))
 (defun gtk::check-version (a b c) (|gtk_check_version| a b c)))
(progn
 (defalien "gtk_init" void (a int :in-out) (b (* t)))
 (defun gtk::init (a b) (|gtk_init| a b)))
(progn
 (defalien "gtk_init_check" gboolean (a int :in-out) (b (* t)))
 (defun gtk::init-check (a b)
   (let ((v430 (multiple-value-list (|gtk_init_check| a b))))
     (apply #'values (if (= 1 (car v430)) t nil) (cdr v430)))))
(progn
 (defalien "gtk_disable_setlocale" void)
 (defun gtk::disable-setlocale () (|gtk_disable_setlocale|)))
(progn
 (defalien "gtk_set_locale" gchar)
 (defun gtk::set-locale () (|gtk_set_locale|)))
(progn
 (defalien "gtk_get_default_language" (* t))
 (defun gtk::get-default-language () (|gtk_get_default_language|)))
(progn
 (defalien "gtk_events_pending" gint)
 (defun gtk::events-pending () (|gtk_events_pending|)))
(progn
 (defalien "gtk_main_do_event" void (a (* t)))
 (defun gtk::main-do-event (a) (|gtk_main_do_event| a)))
(progn (defalien "gtk_main" void) (defun gtk::main () (|gtk_main|)))
(progn
 (defalien "gtk_main_level" guint)
 (defun gtk::main-level () (|gtk_main_level|)))
(progn
 (defalien "gtk_main_quit" void)
 (defun gtk::main-quit () (|gtk_main_quit|)))
(progn
 (defalien "gtk_main_iteration" gboolean)
 (defun gtk::main-iteration ()
   (let ((v431 (|gtk_main_iteration|)))
     (if (= v431 1) t nil))))
(progn
 (defalien "gtk_main_iteration_do" gboolean (a gboolean))
 (defun gtk::main-iteration-do (a)
   (let ((v432 (|gtk_main_iteration_do| (if a (if (eq a 0) 0 1) 0))))
     (if (= v432 1) t nil))))
(progn
 (defalien "gtk_true" gboolean)
 (defun gtk::true ()
   (let ((v433 (|gtk_true|)))
     (if (= v433 1) t nil))))
(progn
 (defalien "gtk_false" gboolean)
 (defun gtk::false ()
   (let ((v434 (|gtk_false|)))
     (if (= v434 1) t nil))))
(progn
 (defalien "gtk_grab_add" void (a (* t)))
 (defun gtk::grab-add (a) (|gtk_grab_add| a)))
(progn
 (defalien "gtk_grab_get_current" (* t))
 (defun gtk::grab-get-current () (|gtk_grab_get_current|)))
(progn
 (defalien "gtk_grab_remove" void (a (* t)))
 (defun gtk::grab-remove (a) (|gtk_grab_remove| a)))
(progn
 (defalien "gtk_init_add" void (a GtkFunction) (b gpointer))
 (defun gtk::init-add (a b) (|gtk_init_add| a b)))
(progn
 (defalien "gtk_quit_add_destroy" void (a guint) (b (* t)))
 (defun gtk::quit-add-destroy (a b) (|gtk_quit_add_destroy| a b)))
(progn
 (defalien "gtk_quit_add" guint (a guint) (b GtkFunction) (c gpointer))
 (defun gtk::quit-add (a b c) (|gtk_quit_add| a b c)))
(progn
 (defalien "gtk_quit_add_full"
           guint
           (a guint)
           (b GtkFunction)
           (c GtkCallbackMarshal)
           (d gpointer)
           (e GtkDestroyNotify))
 (defun gtk::quit-add-full (a b c d e) (|gtk_quit_add_full| a b c d e)))
(progn
 (defalien "gtk_quit_remove" void (a guint))
 (defun gtk::quit-remove (a) (|gtk_quit_remove| a)))
(progn
 (defalien "gtk_quit_remove_by_data" void (a gpointer))
 (defun gtk::quit-remove-by-data (a) (|gtk_quit_remove_by_data| a)))
(progn
 (defalien "gtk_timeout_add" guint (a guint32) (b GtkFunction) (c gpointer))
 (defun gtk::timeout-add (a b c) (|gtk_timeout_add| a b c)))
(progn
 (defalien "gtk_timeout_add_full"
           guint
           (a guint32)
           (b GtkFunction)
           (c GtkCallbackMarshal)
           (d gpointer)
           (e GtkDestroyNotify))
 (defun gtk::timeout-add-full (a b c d e) (|gtk_timeout_add_full| a b c d e)))
(progn
 (defalien "gtk_timeout_remove" void (a guint))
 (defun gtk::timeout-remove (a) (|gtk_timeout_remove| a)))
(progn
 (defalien "gtk_idle_add" guint (a GtkFunction) (b gpointer))
 (defun gtk::idle-add (a b) (|gtk_idle_add| a b)))
(progn
 (defalien "gtk_idle_add_priority" guint (a gint) (b GtkFunction) (c gpointer))
 (defun gtk::idle-add-priority (a b c) (|gtk_idle_add_priority| a b c)))
(progn
 (defalien "gtk_idle_add_full"
           guint
           (a gint)
           (b GtkFunction)
           (c GtkCallbackMarshal)
           (d gpointer)
           (e GtkDestroyNotify))
 (defun gtk::idle-add-full (a b c d e) (|gtk_idle_add_full| a b c d e)))
(progn
 (defalien "gtk_idle_remove" void (a guint))
 (defun gtk::idle-remove (a) (|gtk_idle_remove| a)))
(progn
 (defalien "gtk_idle_remove_by_data" void (a gpointer))
 (defun gtk::idle-remove-by-data (a) (|gtk_idle_remove_by_data| a)))
(progn
 (defalien "gtk_input_add_full"
           guint
           (a gint)
           (b GdkInputCondition)
           (c GdkInputFunction)
           (d GtkCallbackMarshal)
           (e gpointer)
           (f GtkDestroyNotify))
 (defun gtk::input-add-full (a b c d e f) (|gtk_input_add_full| a b c d e f)))
(progn
 (defalien "gtk_input_remove" void (a guint))
 (defun gtk::input-remove (a) (|gtk_input_remove| a)))
(progn
 (defalien "gtk_key_snooper_install" guint (a GtkKeySnoopFunc) (b gpointer))
 (defun gtk::key-snooper-install (a b) (|gtk_key_snooper_install| a b)))
(progn
 (defalien "gtk_key_snooper_remove" void (a guint))
 (defun gtk::key-snooper-remove (a) (|gtk_key_snooper_remove| a)))
(progn
 (defalien "gtk_get_current_event" (* t))
 (defun gtk::get-current-event () (|gtk_get_current_event|)))
(progn
 (defalien "gtk_get_current_event_time" guint32)
 (defun gtk::get-current-event-time () (|gtk_get_current_event_time|)))
(progn
 (defalien "gtk_get_current_event_state" gboolean (a GdkModifierType :in-out))
 (defun gtk::get-current-event-state (a)
   (let ((v435 (multiple-value-list (|gtk_get_current_event_state| a))))
     (apply #'values (if (= 1 (car v435)) t nil) (cdr v435)))))
(progn
 (defalien "gtk_get_event_widget" (* t) (a (* t)))
 (defun gtk::get-event-widget (a) (|gtk_get_event_widget| a)))
(progn
 (defalien "gtk_propagate_event" void (a (* t)) (b (* t)))
 (defun gtk::propagate-event (a b) (|gtk_propagate_event| a b)))
(progn
 (defalien "gtk_menu_bar_get_type" GType)
 (defun gtk::menu-bar-get-type () (|gtk_menu_bar_get_type|)))
(progn
 (defalien "gtk_menu_bar_new" (* t))
 (defun gtk::menu-bar-new () (|gtk_menu_bar_new|)))
(progn
 (defalien "gtk_message_dialog_get_type" GType)
 (defun gtk::message-dialog-get-type () (|gtk_message_dialog_get_type|)))
(progn
 (defalien "gtk_message_dialog_new"
           (* t)
           (a (* t))
           (b GtkDialogFlags)
           (c GtkMessageType)
           (d GtkButtonsType)
           (e c-string))
 (defun gtk::message-dialog-new (a b c d e)
   (|gtk_message_dialog_new| a b c d e)))
(progn
 (defalien "gtk_notebook_get_type" GType)
 (defun gtk::notebook-get-type () (|gtk_notebook_get_type|)))
(progn
 (defalien "gtk_notebook_new" (* t))
 (defun gtk::notebook-new () (|gtk_notebook_new|)))
(progn
 (defalien "gtk_notebook_append_page" void (a (* t)) (b (* t)) (c (* t)))
 (defun gtk::notebook-append-page (a b c) (|gtk_notebook_append_page| a b c)))
(progn
 (defalien "gtk_notebook_append_page_menu"
           void
           (a (* t))
           (b (* t))
           (c (* t))
           (d (* t)))
 (defun gtk::notebook-append-page-menu (a b c d)
   (|gtk_notebook_append_page_menu| a b c d)))
(progn
 (defalien "gtk_notebook_prepend_page" void (a (* t)) (b (* t)) (c (* t)))
 (defun gtk::notebook-prepend-page (a b c) (|gtk_notebook_prepend_page| a b c)))
(progn
 (defalien "gtk_notebook_prepend_page_menu"
           void
           (a (* t))
           (b (* t))
           (c (* t))
           (d (* t)))
 (defun gtk::notebook-prepend-page-menu (a b c d)
   (|gtk_notebook_prepend_page_menu| a b c d)))
(progn
 (defalien "gtk_notebook_insert_page"
           void
           (a (* t))
           (b (* t))
           (c (* t))
           (d gint))
 (defun gtk::notebook-insert-page (a b c d)
   (|gtk_notebook_insert_page| a b c d)))
(progn
 (defalien "gtk_notebook_insert_page_menu"
           void
           (a (* t))
           (b (* t))
           (c (* t))
           (d (* t))
           (e gint))
 (defun gtk::notebook-insert-page-menu (a b c d e)
   (|gtk_notebook_insert_page_menu| a b c d e)))
(progn
 (defalien "gtk_notebook_remove_page" void (a (* t)) (b gint))
 (defun gtk::notebook-remove-page (a b) (|gtk_notebook_remove_page| a b)))
(progn
 (defalien "gtk_notebook_get_current_page" gint (a (* t)))
 (defun gtk::notebook-get-current-page (a) (|gtk_notebook_get_current_page| a)))
(progn
 (defalien "gtk_notebook_get_nth_page" (* t) (a (* t)) (b gint))
 (defun gtk::notebook-get-nth-page (a b) (|gtk_notebook_get_nth_page| a b)))
(progn
 (defalien "gtk_notebook_page_num" gint (a (* t)) (b (* t)))
 (defun gtk::notebook-page-num (a b) (|gtk_notebook_page_num| a b)))
(progn
 (defalien "gtk_notebook_set_current_page" void (a (* t)) (b gint))
 (defun gtk::notebook-set-current-page (a b)
   (|gtk_notebook_set_current_page| a b)))
(progn
 (defalien "gtk_notebook_next_page" void (a (* t)))
 (defun gtk::notebook-next-page (a) (|gtk_notebook_next_page| a)))
(progn
 (defalien "gtk_notebook_prev_page" void (a (* t)))
 (defun gtk::notebook-prev-page (a) (|gtk_notebook_prev_page| a)))
(progn
 (defalien "gtk_notebook_set_show_border" void (a (* t)) (b gboolean))
 (defun gtk::notebook-set-show-border (a b)
   (|gtk_notebook_set_show_border| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gtk_notebook_get_show_border" gboolean (a (* t)))
 (defun gtk::notebook-get-show-border (a)
   (let ((v436 (|gtk_notebook_get_show_border| a)))
     (if (= v436 1) t nil))))
(progn
 (defalien "gtk_notebook_set_show_tabs" void (a (* t)) (b gboolean))
 (defun gtk::notebook-set-show-tabs (a b)
   (|gtk_notebook_set_show_tabs| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gtk_notebook_get_show_tabs" gboolean (a (* t)))
 (defun gtk::notebook-get-show-tabs (a)
   (let ((v437 (|gtk_notebook_get_show_tabs| a)))
     (if (= v437 1) t nil))))
(progn
 (defalien "gtk_notebook_set_tab_pos" void (a (* t)) (b GtkPositionType))
 (defun gtk::notebook-set-tab-pos (a b) (|gtk_notebook_set_tab_pos| a b)))
(progn
 (defalien "gtk_notebook_get_tab_pos" GtkPositionType (a (* t)))
 (defun gtk::notebook-get-tab-pos (a) (|gtk_notebook_get_tab_pos| a)))
(progn
 (defalien "gtk_notebook_set_scrollable" void (a (* t)) (b gboolean))
 (defun gtk::notebook-set-scrollable (a b)
   (|gtk_notebook_set_scrollable| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gtk_notebook_get_scrollable" gboolean (a (* t)))
 (defun gtk::notebook-get-scrollable (a)
   (let ((v438 (|gtk_notebook_get_scrollable| a)))
     (if (= v438 1) t nil))))
(progn
 (defalien "gtk_notebook_popup_enable" void (a (* t)))
 (defun gtk::notebook-popup-enable (a) (|gtk_notebook_popup_enable| a)))
(progn
 (defalien "gtk_notebook_popup_disable" void (a (* t)))
 (defun gtk::notebook-popup-disable (a) (|gtk_notebook_popup_disable| a)))
(progn
 (defalien "gtk_notebook_get_tab_label" (* t) (a (* t)) (b (* t)))
 (defun gtk::notebook-get-tab-label (a b) (|gtk_notebook_get_tab_label| a b)))
(progn
 (defalien "gtk_notebook_set_tab_label" void (a (* t)) (b (* t)) (c (* t)))
 (defun gtk::notebook-set-tab-label (a b c)
   (|gtk_notebook_set_tab_label| a b c)))
(progn
 (defalien "gtk_notebook_set_tab_label_text"
           void
           (a (* t))
           (b (* t))
           (c c-string))
 (defun gtk::notebook-set-tab-label-text (a b c)
   (|gtk_notebook_set_tab_label_text| a b c)))
(progn
 (defalien "gtk_notebook_get_tab_label_text" c-string (a (* t)) (b (* t)))
 (defun gtk::notebook-get-tab-label-text (a b)
   (|gtk_notebook_get_tab_label_text| a b)))
(progn
 (defalien "gtk_notebook_get_menu_label" (* t) (a (* t)) (b (* t)))
 (defun gtk::notebook-get-menu-label (a b) (|gtk_notebook_get_menu_label| a b)))
(progn
 (defalien "gtk_notebook_set_menu_label" void (a (* t)) (b (* t)) (c (* t)))
 (defun gtk::notebook-set-menu-label (a b c)
   (|gtk_notebook_set_menu_label| a b c)))
(progn
 (defalien "gtk_notebook_set_menu_label_text"
           void
           (a (* t))
           (b (* t))
           (c c-string))
 (defun gtk::notebook-set-menu-label-text (a b c)
   (|gtk_notebook_set_menu_label_text| a b c)))
(progn
 (defalien "gtk_notebook_get_menu_label_text" c-string (a (* t)) (b (* t)))
 (defun gtk::notebook-get-menu-label-text (a b)
   (|gtk_notebook_get_menu_label_text| a b)))
(progn
 (defalien "gtk_notebook_query_tab_label_packing"
           void
           (a (* t))
           (b (* t))
           (c (* t))
           (d (* t))
           (e GtkPackType :in-out))
 (defun gtk::notebook-query-tab-label-packing (a b c d e)
   (|gtk_notebook_query_tab_label_packing| a b c d e)))
(progn
 (defalien "gtk_notebook_set_tab_label_packing"
           void
           (a (* t))
           (b (* t))
           (c gboolean)
           (d gboolean)
           (e GtkPackType))
 (defun gtk::notebook-set-tab-label-packing (a b c d e)
   (|gtk_notebook_set_tab_label_packing| a b (if c (if (eq c 0) 0 1) 0)
    (if d (if (eq d 0) 0 1) 0) e)))
(progn
 (defalien "gtk_notebook_reorder_child" void (a (* t)) (b (* t)) (c gint))
 (defun gtk::notebook-reorder-child (a b c)
   (|gtk_notebook_reorder_child| a b c)))
(progn
 (defalien "gtk_old_editable_get_type" GtkType)
 (defun gtk::old-editable-get-type () (|gtk_old_editable_get_type|)))
(progn
 (defalien "gtk_old_editable_claim_selection"
           void
           (a (* t))
           (b gboolean)
           (c guint32))
 (defun gtk::old-editable-claim-selection (a b c)
   (|gtk_old_editable_claim_selection| a (if b (if (eq b 0) 0 1) 0) c)))
(progn
 (defalien "gtk_old_editable_changed" void (a (* t)))
 (defun gtk::old-editable-changed (a) (|gtk_old_editable_changed| a)))
(progn
 (defalien "gtk_option_menu_get_type" GType)
 (defun gtk::option-menu-get-type () (|gtk_option_menu_get_type|)))
(progn
 (defalien "gtk_option_menu_new" (* t))
 (defun gtk::option-menu-new () (|gtk_option_menu_new|)))
(progn
 (defalien "gtk_option_menu_get_menu" (* t) (a (* t)))
 (defun gtk::option-menu-get-menu (a) (|gtk_option_menu_get_menu| a)))
(progn
 (defalien "gtk_option_menu_set_menu" void (a (* t)) (b (* t)))
 (defun gtk::option-menu-set-menu (a b) (|gtk_option_menu_set_menu| a b)))
(progn
 (defalien "gtk_option_menu_remove_menu" void (a (* t)))
 (defun gtk::option-menu-remove-menu (a) (|gtk_option_menu_remove_menu| a)))
(progn
 (defalien "gtk_option_menu_get_history" gint (a (* t)))
 (defun gtk::option-menu-get-history (a) (|gtk_option_menu_get_history| a)))
(progn
 (defalien "gtk_option_menu_set_history" void (a (* t)) (b guint))
 (defun gtk::option-menu-set-history (a b) (|gtk_option_menu_set_history| a b)))
(progn
 (defalien "gtk_pixmap_get_type" GtkType)
 (defun gtk::pixmap-get-type () (|gtk_pixmap_get_type|)))
(progn
 (defalien "gtk_pixmap_set" void (a (* t)) (b (* t)) (c (* t)))
 (defun gtk::pixmap-set (a b c) (|gtk_pixmap_set| a b c)))
(progn
 (defalien "gtk_pixmap_get" void (a (* t)) (b (* t)) (c (* t)))
 (defun gtk::pixmap-get (a b c) (|gtk_pixmap_get| a b c)))
(progn
 (defalien "gtk_pixmap_set_build_insensitive" void (a (* t)) (b gboolean))
 (defun gtk::pixmap-set-build-insensitive (a b)
   (|gtk_pixmap_set_build_insensitive| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gtk_socket_get_type" GType)
 (defun gtk::socket-get-type () (|gtk_socket_get_type|)))
(progn
 (defalien "gtk_socket_new" (* t))
 (defun gtk::socket-new () (|gtk_socket_new|)))
(progn
 (defalien "gtk_socket_add_id" void (a (* t)) (b GdkNativeWindow))
 (defun gtk::socket-add-id (a b) (|gtk_socket_add_id| a b)))
(progn
 (defalien "gtk_socket_get_id" GdkNativeWindow (a (* t)))
 (defun gtk::socket-get-id (a) (|gtk_socket_get_id| a)))
(progn
 (defalien "gtk_plug_get_type" GType)
 (defun gtk::plug-get-type () (|gtk_plug_get_type|)))
(progn
 (defalien "gtk_plug_construct" void (a (* t)) (b GdkNativeWindow))
 (defun gtk::plug-construct (a b) (|gtk_plug_construct| a b)))
(progn
 (defalien "gtk_plug_new" (* t) (a GdkNativeWindow))
 (defun gtk::plug-new (a) (|gtk_plug_new| a)))
(progn
 (defalien "gtk_plug_get_id" GdkNativeWindow (a (* t)))
 (defun gtk::plug-get-id (a) (|gtk_plug_get_id| a)))
(progn
 (defalien "gtk_preview_get_type" GtkType)
 (defun gtk::preview-get-type () (|gtk_preview_get_type|)))
(progn
 (defalien "gtk_preview_size" void (a (* t)) (b gint) (c gint))
 (defun gtk::preview-size (a b c) (|gtk_preview_size| a b c)))
(progn
 (defalien "gtk_preview_put"
           void
           (a (* t))
           (b (* t))
           (c (* t))
           (d gint)
           (e gint)
           (f gint)
           (g gint)
           (h gint)
           (i gint))
 (defun gtk::preview-put (a b c d e f g h i)
   (|gtk_preview_put| a b c d e f g h i)))
(progn
 (defalien "gtk_preview_draw_row"
           void
           (a (* t))
           (b guchar :in-out)
           (c gint)
           (d gint)
           (e gint))
 (defun gtk::preview-draw-row (a b c d e) (|gtk_preview_draw_row| a b c d e)))
(progn
 (defalien "gtk_preview_set_expand" void (a (* t)) (b gboolean))
 (defun gtk::preview-set-expand (a b)
   (|gtk_preview_set_expand| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gtk_progress_get_type" GType)
 (defun gtk::progress-get-type () (|gtk_progress_get_type|)))
(progn
 (defalien "gtk_progress_bar_get_type" GType)
 (defun gtk::progress-bar-get-type () (|gtk_progress_bar_get_type|)))
(progn
 (defalien "gtk_progress_bar_new" (* t))
 (defun gtk::progress-bar-new () (|gtk_progress_bar_new|)))
(progn
 (defalien "gtk_progress_bar_pulse" void (a (* t)))
 (defun gtk::progress-bar-pulse (a) (|gtk_progress_bar_pulse| a)))
(progn
 (defalien "gtk_progress_bar_set_text" void (a (* t)) (b c-string))
 (defun gtk::progress-bar-set-text (a b) (|gtk_progress_bar_set_text| a b)))
(progn
 (defalien "gtk_progress_bar_set_fraction" void (a (* t)) (b gdouble))
 (defun gtk::progress-bar-set-fraction (a b)
   (|gtk_progress_bar_set_fraction| a (coerce b 'double-float))))
(progn
 (defalien "gtk_progress_bar_set_pulse_step" void (a (* t)) (b gdouble))
 (defun gtk::progress-bar-set-pulse-step (a b)
   (|gtk_progress_bar_set_pulse_step| a (coerce b 'double-float))))
(progn
 (defalien "gtk_progress_bar_set_orientation"
           void
           (a (* t))
           (b GtkProgressBarOrientation))
 (defun gtk::progress-bar-set-orientation (a b)
   (|gtk_progress_bar_set_orientation| a b)))
(progn
 (defalien "gtk_progress_bar_get_text" c-string (a (* t)))
 (defun gtk::progress-bar-get-text (a) (|gtk_progress_bar_get_text| a)))
(progn
 (defalien "gtk_progress_bar_get_fraction" gdouble (a (* t)))
 (defun gtk::progress-bar-get-fraction (a) (|gtk_progress_bar_get_fraction| a)))
(progn
 (defalien "gtk_progress_bar_get_pulse_step" gdouble (a (* t)))
 (defun gtk::progress-bar-get-pulse-step (a)
   (|gtk_progress_bar_get_pulse_step| a)))
(progn
 (defalien "gtk_progress_bar_get_orientation"
           GtkProgressBarOrientation
           (a (* t)))
 (defun gtk::progress-bar-get-orientation (a)
   (|gtk_progress_bar_get_orientation| a)))
(progn
 (defalien "gtk_radio_button_get_type" GType)
 (defun gtk::radio-button-get-type () (|gtk_radio_button_get_type|)))
(progn
 (defalien "gtk_radio_button_new" (* t) (a (* t)))
 (defun gtk::radio-button-new (a) (|gtk_radio_button_new| a)))
(progn
 (defalien "gtk_radio_button_new_from_widget" (* t) (a (* t)))
 (defun gtk::radio-button-new-from-widget (a)
   (|gtk_radio_button_new_from_widget| a)))
(progn
 (defalien "gtk_radio_button_new_with_label" (* t) (a (* t)) (b c-string))
 (defun gtk::radio-button-new-with-label (a b)
   (|gtk_radio_button_new_with_label| a b)))
(progn
 (defalien "gtk_radio_button_new_with_label_from_widget"
           (* t)
           (a (* t))
           (b c-string))
 (defun gtk::radio-button-new-with-label-from-widget (a b)
   (|gtk_radio_button_new_with_label_from_widget| a b)))
(progn
 (defalien "gtk_radio_button_new_with_mnemonic" (* t) (a (* t)) (b c-string))
 (defun gtk::radio-button-new-with-mnemonic (a b)
   (|gtk_radio_button_new_with_mnemonic| a b)))
(progn
 (defalien "gtk_radio_button_new_with_mnemonic_from_widget"
           (* t)
           (a (* t))
           (b c-string))
 (defun gtk::radio-button-new-with-mnemonic-from-widget (a b)
   (|gtk_radio_button_new_with_mnemonic_from_widget| a b)))
(progn
 (defalien "gtk_radio_button_get_group" (* t) (a (* t)))
 (defun gtk::radio-button-get-group (a) (|gtk_radio_button_get_group| a)))
(progn
 (defalien "gtk_radio_button_set_group" void (a (* t)) (b (* t)))
 (defun gtk::radio-button-set-group (a b) (|gtk_radio_button_set_group| a b)))
(progn
 (defalien "gtk_radio_menu_item_get_type" GType)
 (defun gtk::radio-menu-item-get-type () (|gtk_radio_menu_item_get_type|)))
(progn
 (defalien "gtk_radio_menu_item_new" (* t) (a (* t)))
 (defun gtk::radio-menu-item-new (a) (|gtk_radio_menu_item_new| a)))
(progn
 (defalien "gtk_radio_menu_item_new_with_label" (* t) (a (* t)) (b c-string))
 (defun gtk::radio-menu-item-new-with-label (a b)
   (|gtk_radio_menu_item_new_with_label| a b)))
(progn
 (defalien "gtk_radio_menu_item_new_with_mnemonic"
           (* t)
           (a (* t))
           (b c-string))
 (defun gtk::radio-menu-item-new-with-mnemonic (a b)
   (|gtk_radio_menu_item_new_with_mnemonic| a b)))
(progn
 (defalien "gtk_radio_menu_item_get_group" (* t) (a (* t)))
 (defun gtk::radio-menu-item-get-group (a) (|gtk_radio_menu_item_get_group| a)))
(progn
 (defalien "gtk_radio_menu_item_set_group" void (a (* t)) (b (* t)))
 (defun gtk::radio-menu-item-set-group (a b)
   (|gtk_radio_menu_item_set_group| a b)))
(progn
 (defalien "gtk_viewport_get_type" GType)
 (defun gtk::viewport-get-type () (|gtk_viewport_get_type|)))
(progn
 (defalien "gtk_viewport_new" (* t) (a (* t)) (b (* t)))
 (defun gtk::viewport-new (a b) (|gtk_viewport_new| a b)))
(progn
 (defalien "gtk_viewport_get_hadjustment" (* t) (a (* t)))
 (defun gtk::viewport-get-hadjustment (a) (|gtk_viewport_get_hadjustment| a)))
(progn
 (defalien "gtk_viewport_get_vadjustment" (* t) (a (* t)))
 (defun gtk::viewport-get-vadjustment (a) (|gtk_viewport_get_vadjustment| a)))
(progn
 (defalien "gtk_viewport_set_hadjustment" void (a (* t)) (b (* t)))
 (defun gtk::viewport-set-hadjustment (a b)
   (|gtk_viewport_set_hadjustment| a b)))
(progn
 (defalien "gtk_viewport_set_vadjustment" void (a (* t)) (b (* t)))
 (defun gtk::viewport-set-vadjustment (a b)
   (|gtk_viewport_set_vadjustment| a b)))
(progn
 (defalien "gtk_viewport_set_shadow_type" void (a (* t)) (b GtkShadowType))
 (defun gtk::viewport-set-shadow-type (a b)
   (|gtk_viewport_set_shadow_type| a b)))
(progn
 (defalien "gtk_viewport_get_shadow_type" GtkShadowType (a (* t)))
 (defun gtk::viewport-get-shadow-type (a) (|gtk_viewport_get_shadow_type| a)))
(progn
 (defalien "gtk_scrolled_window_get_type" GType)
 (defun gtk::scrolled-window-get-type () (|gtk_scrolled_window_get_type|)))
(progn
 (defalien "gtk_scrolled_window_new" (* t) (a (* t)) (b (* t)))
 (defun gtk::scrolled-window-new (a b) (|gtk_scrolled_window_new| a b)))
(progn
 (defalien "gtk_scrolled_window_set_hadjustment" void (a (* t)) (b (* t)))
 (defun gtk::scrolled-window-set-hadjustment (a b)
   (|gtk_scrolled_window_set_hadjustment| a b)))
(progn
 (defalien "gtk_scrolled_window_set_vadjustment" void (a (* t)) (b (* t)))
 (defun gtk::scrolled-window-set-vadjustment (a b)
   (|gtk_scrolled_window_set_vadjustment| a b)))
(progn
 (defalien "gtk_scrolled_window_get_hadjustment" (* t) (a (* t)))
 (defun gtk::scrolled-window-get-hadjustment (a)
   (|gtk_scrolled_window_get_hadjustment| a)))
(progn
 (defalien "gtk_scrolled_window_get_vadjustment" (* t) (a (* t)))
 (defun gtk::scrolled-window-get-vadjustment (a)
   (|gtk_scrolled_window_get_vadjustment| a)))
(progn
 (defalien "gtk_scrolled_window_set_policy"
           void
           (a (* t))
           (b GtkPolicyType)
           (c GtkPolicyType))
 (defun gtk::scrolled-window-set-policy (a b c)
   (|gtk_scrolled_window_set_policy| a b c)))
(progn
 (defalien "gtk_scrolled_window_get_policy"
           void
           (a (* t))
           (b GtkPolicyType :in-out)
           (c GtkPolicyType :in-out))
 (defun gtk::scrolled-window-get-policy (a b c)
   (|gtk_scrolled_window_get_policy| a b c)))
(progn
 (defalien "gtk_scrolled_window_set_placement"
           void
           (a (* t))
           (b GtkCornerType))
 (defun gtk::scrolled-window-set-placement (a b)
   (|gtk_scrolled_window_set_placement| a b)))
(progn
 (defalien "gtk_scrolled_window_get_placement" GtkCornerType (a (* t)))
 (defun gtk::scrolled-window-get-placement (a)
   (|gtk_scrolled_window_get_placement| a)))
(progn
 (defalien "gtk_scrolled_window_set_shadow_type"
           void
           (a (* t))
           (b GtkShadowType))
 (defun gtk::scrolled-window-set-shadow-type (a b)
   (|gtk_scrolled_window_set_shadow_type| a b)))
(progn
 (defalien "gtk_scrolled_window_get_shadow_type" GtkShadowType (a (* t)))
 (defun gtk::scrolled-window-get-shadow-type (a)
   (|gtk_scrolled_window_get_shadow_type| a)))
(progn
 (defalien "gtk_scrolled_window_add_with_viewport" void (a (* t)) (b (* t)))
 (defun gtk::scrolled-window-add-with-viewport (a b)
   (|gtk_scrolled_window_add_with_viewport| a b)))
(progn
 (defalien "gtk_separator_menu_item_get_type" GType)
 (defun gtk::separator-menu-item-get-type ()
   (|gtk_separator_menu_item_get_type|)))
(progn
 (defalien "gtk_separator_menu_item_new" (* t))
 (defun gtk::separator-menu-item-new () (|gtk_separator_menu_item_new|)))
(progn
 (defalien "gtk_size_group_get_type" GType)
 (defun gtk::size-group-get-type () (|gtk_size_group_get_type|)))
(progn
 (defalien "gtk_size_group_new" (* t) (a GtkSizeGroupMode))
 (defun gtk::size-group-new (a) (|gtk_size_group_new| a)))
(progn
 (defalien "gtk_size_group_set_mode" void (a (* t)) (b GtkSizeGroupMode))
 (defun gtk::size-group-set-mode (a b) (|gtk_size_group_set_mode| a b)))
(progn
 (defalien "gtk_size_group_get_mode" GtkSizeGroupMode (a (* t)))
 (defun gtk::size-group-get-mode (a) (|gtk_size_group_get_mode| a)))
(progn
 (defalien "gtk_size_group_add_widget" void (a (* t)) (b (* t)))
 (defun gtk::size-group-add-widget (a b) (|gtk_size_group_add_widget| a b)))
(progn
 (defalien "gtk_size_group_remove_widget" void (a (* t)) (b (* t)))
 (defun gtk::size-group-remove-widget (a b)
   (|gtk_size_group_remove_widget| a b)))
(progn
 (defalien "gtk_spin_button_get_type" GType)
 (defun gtk::spin-button-get-type () (|gtk_spin_button_get_type|)))
(progn
 (defalien "gtk_spin_button_configure"
           void
           (a (* t))
           (b (* t))
           (c gdouble)
           (d guint))
 (defun gtk::spin-button-configure (a b c d)
   (|gtk_spin_button_configure| a b (coerce c 'double-float) d)))
(progn
 (defalien "gtk_spin_button_new" (* t) (a (* t)) (b gdouble) (c guint))
 (defun gtk::spin-button-new (a b c)
   (|gtk_spin_button_new| a (coerce b 'double-float) c)))
(progn
 (defalien "gtk_spin_button_new_with_range"
           (* t)
           (a gdouble)
           (b gdouble)
           (c gdouble))
 (defun gtk::spin-button-new-with-range (a b c)
   (|gtk_spin_button_new_with_range| (coerce a 'double-float)
    (coerce b 'double-float) (coerce c 'double-float))))
(progn
 (defalien "gtk_spin_button_set_adjustment" void (a (* t)) (b (* t)))
 (defun gtk::spin-button-set-adjustment (a b)
   (|gtk_spin_button_set_adjustment| a b)))
(progn
 (defalien "gtk_spin_button_get_adjustment" (* t) (a (* t)))
 (defun gtk::spin-button-get-adjustment (a)
   (|gtk_spin_button_get_adjustment| a)))
(progn
 (defalien "gtk_spin_button_set_digits" void (a (* t)) (b guint))
 (defun gtk::spin-button-set-digits (a b) (|gtk_spin_button_set_digits| a b)))
(progn
 (defalien "gtk_spin_button_get_digits" guint (a (* t)))
 (defun gtk::spin-button-get-digits (a) (|gtk_spin_button_get_digits| a)))
(progn
 (defalien "gtk_spin_button_set_increments"
           void
           (a (* t))
           (b gdouble)
           (c gdouble))
 (defun gtk::spin-button-set-increments (a b c)
   (|gtk_spin_button_set_increments| a (coerce b 'double-float)
    (coerce c 'double-float))))
(progn
 (defalien "gtk_spin_button_get_increments"
           void
           (a (* t))
           (b gdouble :in-out)
           (c gdouble :in-out))
 (defun gtk::spin-button-get-increments (a b c)
   (|gtk_spin_button_get_increments| a (coerce b 'double-float)
    (coerce c 'double-float))))
(progn
 (defalien "gtk_spin_button_set_range" void (a (* t)) (b gdouble) (c gdouble))
 (defun gtk::spin-button-set-range (a b c)
   (|gtk_spin_button_set_range| a (coerce b 'double-float)
    (coerce c 'double-float))))
(progn
 (defalien "gtk_spin_button_get_range"
           void
           (a (* t))
           (b gdouble :in-out)
           (c gdouble :in-out))
 (defun gtk::spin-button-get-range (a b c)
   (|gtk_spin_button_get_range| a (coerce b 'double-float)
    (coerce c 'double-float))))
(progn
 (defalien "gtk_spin_button_get_value" gdouble (a (* t)))
 (defun gtk::spin-button-get-value (a) (|gtk_spin_button_get_value| a)))
(progn
 (defalien "gtk_spin_button_get_value_as_int" gint (a (* t)))
 (defun gtk::spin-button-get-value-as-int (a)
   (|gtk_spin_button_get_value_as_int| a)))
(progn
 (defalien "gtk_spin_button_set_value" void (a (* t)) (b gdouble))
 (defun gtk::spin-button-set-value (a b)
   (|gtk_spin_button_set_value| a (coerce b 'double-float))))
(progn
 (defalien "gtk_spin_button_set_update_policy"
           void
           (a (* t))
           (b GtkSpinButtonUpdatePolicy))
 (defun gtk::spin-button-set-update-policy (a b)
   (|gtk_spin_button_set_update_policy| a b)))
(progn
 (defalien "gtk_spin_button_get_update_policy"
           GtkSpinButtonUpdatePolicy
           (a (* t)))
 (defun gtk::spin-button-get-update-policy (a)
   (|gtk_spin_button_get_update_policy| a)))
(progn
 (defalien "gtk_spin_button_set_numeric" void (a (* t)) (b gboolean))
 (defun gtk::spin-button-set-numeric (a b)
   (|gtk_spin_button_set_numeric| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gtk_spin_button_get_numeric" gboolean (a (* t)))
 (defun gtk::spin-button-get-numeric (a)
   (let ((v439 (|gtk_spin_button_get_numeric| a)))
     (if (= v439 1) t nil))))
(progn
 (defalien "gtk_spin_button_spin" void (a (* t)) (b GtkSpinType) (c gdouble))
 (defun gtk::spin-button-spin (a b c)
   (|gtk_spin_button_spin| a b (coerce c 'double-float))))
(progn
 (defalien "gtk_spin_button_set_wrap" void (a (* t)) (b gboolean))
 (defun gtk::spin-button-set-wrap (a b)
   (|gtk_spin_button_set_wrap| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gtk_spin_button_get_wrap" gboolean (a (* t)))
 (defun gtk::spin-button-get-wrap (a)
   (let ((v440 (|gtk_spin_button_get_wrap| a)))
     (if (= v440 1) t nil))))
(progn
 (defalien "gtk_spin_button_set_snap_to_ticks" void (a (* t)) (b gboolean))
 (defun gtk::spin-button-set-snap-to-ticks (a b)
   (|gtk_spin_button_set_snap_to_ticks| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gtk_spin_button_get_snap_to_ticks" gboolean (a (* t)))
 (defun gtk::spin-button-get-snap-to-ticks (a)
   (let ((v441 (|gtk_spin_button_get_snap_to_ticks| a)))
     (if (= v441 1) t nil))))
(progn
 (defalien "gtk_spin_button_update" void (a (* t)))
 (defun gtk::spin-button-update (a) (|gtk_spin_button_update| a)))
(progn
 (defalien "gtk_stock_add" void (a (* t)) (b guint))
 (defun gtk::stock-add (a b) (|gtk_stock_add| a b)))
(progn
 (defalien "gtk_stock_add_static" void (a (* t)) (b guint))
 (defun gtk::stock-add-static (a b) (|gtk_stock_add_static| a b)))
(progn
 (defalien "gtk_stock_lookup" gboolean (a c-string) (b (* t)))
 (defun gtk::stock-lookup (a b)
   (let ((v442 (|gtk_stock_lookup| a b)))
     (if (= v442 1) t nil))))
(progn
 (defalien "gtk_stock_list_ids" (* t))
 (defun gtk::stock-list-ids () (|gtk_stock_list_ids|)))
(progn
 (defalien "gtk_stock_item_copy" (* t) (a (* t)))
 (defun gtk::stock-item-copy (a) (|gtk_stock_item_copy| a)))
(progn
 (defalien "gtk_stock_item_free" void (a (* t)))
 (defun gtk::stock-item-free (a) (|gtk_stock_item_free| a)))
(progn
 (defalien "gtk_statusbar_get_type" GType)
 (defun gtk::statusbar-get-type () (|gtk_statusbar_get_type|)))
(progn
 (defalien "gtk_statusbar_new" (* t))
 (defun gtk::statusbar-new () (|gtk_statusbar_new|)))
(progn
 (defalien "gtk_statusbar_get_context_id" guint (a (* t)) (b c-string))
 (defun gtk::statusbar-get-context-id (a b)
   (|gtk_statusbar_get_context_id| a b)))
(progn
 (defalien "gtk_statusbar_push" guint (a (* t)) (b guint) (c c-string))
 (defun gtk::statusbar-push (a b c) (|gtk_statusbar_push| a b c)))
(progn
 (defalien "gtk_statusbar_pop" void (a (* t)) (b guint))
 (defun gtk::statusbar-pop (a b) (|gtk_statusbar_pop| a b)))
(progn
 (defalien "gtk_statusbar_remove" void (a (* t)) (b guint) (c guint))
 (defun gtk::statusbar-remove (a b c) (|gtk_statusbar_remove| a b c)))
(progn
 (defalien "gtk_statusbar_set_has_resize_grip" void (a (* t)) (b gboolean))
 (defun gtk::statusbar-set-has-resize-grip (a b)
   (|gtk_statusbar_set_has_resize_grip| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gtk_statusbar_get_has_resize_grip" gboolean (a (* t)))
 (defun gtk::statusbar-get-has-resize-grip (a)
   (let ((v443 (|gtk_statusbar_get_has_resize_grip| a)))
     (if (= v443 1) t nil))))
(progn
 (defalien "gtk_table_get_type" GType)
 (defun gtk::table-get-type () (|gtk_table_get_type|)))
(progn
 (defalien "gtk_table_new" (* t) (a guint) (b guint) (c gboolean))
 (defun gtk::table-new (a b c)
   (|gtk_table_new| a b (if c (if (eq c 0) 0 1) 0))))
(progn
 (defalien "gtk_table_resize" void (a (* t)) (b guint) (c guint))
 (defun gtk::table-resize (a b c) (|gtk_table_resize| a b c)))
(progn
 (defalien "gtk_table_attach"
           void
           (a (* t))
           (b (* t))
           (c guint)
           (d guint)
           (e guint)
           (f guint)
           (g GtkAttachOptions)
           (h GtkAttachOptions)
           (i guint)
           (j guint))
 (defun gtk::table-attach (a b c d e f g h i j)
   (|gtk_table_attach| a b c d e f g h i j)))
(progn
 (defalien "gtk_table_attach_defaults"
           void
           (a (* t))
           (b (* t))
           (c guint)
           (d guint)
           (e guint)
           (f guint))
 (defun gtk::table-attach-defaults (a b c d e f)
   (|gtk_table_attach_defaults| a b c d e f)))
(progn
 (defalien "gtk_table_set_row_spacing" void (a (* t)) (b guint) (c guint))
 (defun gtk::table-set-row-spacing (a b c) (|gtk_table_set_row_spacing| a b c)))
(progn
 (defalien "gtk_table_get_row_spacing" guint (a (* t)) (b guint))
 (defun gtk::table-get-row-spacing (a b) (|gtk_table_get_row_spacing| a b)))
(progn
 (defalien "gtk_table_set_col_spacing" void (a (* t)) (b guint) (c guint))
 (defun gtk::table-set-col-spacing (a b c) (|gtk_table_set_col_spacing| a b c)))
(progn
 (defalien "gtk_table_get_col_spacing" guint (a (* t)) (b guint))
 (defun gtk::table-get-col-spacing (a b) (|gtk_table_get_col_spacing| a b)))
(progn
 (defalien "gtk_table_set_row_spacings" void (a (* t)) (b guint))
 (defun gtk::table-set-row-spacings (a b) (|gtk_table_set_row_spacings| a b)))
(progn
 (defalien "gtk_table_get_default_row_spacing" guint (a (* t)))
 (defun gtk::table-get-default-row-spacing (a)
   (|gtk_table_get_default_row_spacing| a)))
(progn
 (defalien "gtk_table_set_col_spacings" void (a (* t)) (b guint))
 (defun gtk::table-set-col-spacings (a b) (|gtk_table_set_col_spacings| a b)))
(progn
 (defalien "gtk_table_get_default_col_spacing" guint (a (* t)))
 (defun gtk::table-get-default-col-spacing (a)
   (|gtk_table_get_default_col_spacing| a)))
(progn
 (defalien "gtk_table_set_homogeneous" void (a (* t)) (b gboolean))
 (defun gtk::table-set-homogeneous (a b)
   (|gtk_table_set_homogeneous| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gtk_table_get_homogeneous" gboolean (a (* t)))
 (defun gtk::table-get-homogeneous (a)
   (let ((v444 (|gtk_table_get_homogeneous| a)))
     (if (= v444 1) t nil))))
(progn
 (defalien "gtk_tearoff_menu_item_get_type" GType)
 (defun gtk::tearoff-menu-item-get-type () (|gtk_tearoff_menu_item_get_type|)))
(progn
 (defalien "gtk_tearoff_menu_item_new" (* t))
 (defun gtk::tearoff-menu-item-new () (|gtk_tearoff_menu_item_new|)))
(progn
 (defalien "gtk_text_tag_get_type" GType)
 (defun gtk::text-tag-get-type () (|gtk_text_tag_get_type|)))
(progn
 (defalien "gtk_text_tag_new" (* t) (a c-string))
 (defun gtk::text-tag-new (a) (|gtk_text_tag_new| a)))
(progn
 (defalien "gtk_text_tag_get_priority" gint (a (* t)))
 (defun gtk::text-tag-get-priority (a) (|gtk_text_tag_get_priority| a)))
(progn
 (defalien "gtk_text_tag_set_priority" void (a (* t)) (b gint))
 (defun gtk::text-tag-set-priority (a b) (|gtk_text_tag_set_priority| a b)))
(progn
 (defalien "gtk_text_tag_event"
           gboolean
           (a (* t))
           (b (* t))
           (c (* t))
           (d (* t)))
 (defun gtk::text-tag-event (a b c d)
   (let ((v445 (|gtk_text_tag_event| a b c d)))
     (if (= v445 1) t nil))))
(progn
 (defalien "gtk_text_attributes_new" (* t))
 (defun gtk::text-attributes-new () (|gtk_text_attributes_new|)))
(progn
 (defalien "gtk_text_attributes_copy" (* t) (a (* t)))
 (defun gtk::text-attributes-copy (a) (|gtk_text_attributes_copy| a)))
(progn
 (defalien "gtk_text_attributes_copy_values" void (a (* t)) (b (* t)))
 (defun gtk::text-attributes-copy-values (a b)
   (|gtk_text_attributes_copy_values| a b)))
(progn
 (defalien "gtk_text_attributes_unref" void (a (* t)))
 (defun gtk::text-attributes-unref (a) (|gtk_text_attributes_unref| a)))
(progn
 (defalien "gtk_text_attributes_ref" void (a (* t)))
 (defun gtk::text-attributes-ref (a) (|gtk_text_attributes_ref| a)))
(progn
 (defalien "gtk_text_attributes_get_type" GType)
 (defun gtk::text-attributes-get-type () (|gtk_text_attributes_get_type|)))
(progn
 (defalien "gtk_text_tag_table_get_type" GType)
 (defun gtk::text-tag-table-get-type () (|gtk_text_tag_table_get_type|)))
(progn
 (defalien "gtk_text_tag_table_new" (* t))
 (defun gtk::text-tag-table-new () (|gtk_text_tag_table_new|)))
(progn
 (defalien "gtk_text_tag_table_add" void (a (* t)) (b (* t)))
 (defun gtk::text-tag-table-add (a b) (|gtk_text_tag_table_add| a b)))
(progn
 (defalien "gtk_text_tag_table_remove" void (a (* t)) (b (* t)))
 (defun gtk::text-tag-table-remove (a b) (|gtk_text_tag_table_remove| a b)))
(progn
 (defalien "gtk_text_tag_table_lookup" (* t) (a (* t)) (b c-string))
 (defun gtk::text-tag-table-lookup (a b) (|gtk_text_tag_table_lookup| a b)))
(progn
 (defalien "gtk_text_tag_table_foreach"
           void
           (a (* t))
           (b GtkTextTagTableForeach)
           (c gpointer))
 (defun gtk::text-tag-table-foreach (a b c)
   (|gtk_text_tag_table_foreach| a b c)))
(progn
 (defalien "gtk_text_tag_table_get_size" gint (a (* t)))
 (defun gtk::text-tag-table-get-size (a) (|gtk_text_tag_table_get_size| a)))
(progn
 (defalien "gtk_text_child_anchor_get_type" GType)
 (defun gtk::text-child-anchor-get-type () (|gtk_text_child_anchor_get_type|)))
(progn
 (defalien "gtk_text_child_anchor_new" (* t))
 (defun gtk::text-child-anchor-new () (|gtk_text_child_anchor_new|)))
(progn
 (defalien "gtk_text_child_anchor_get_widgets" (* t) (a (* t)))
 (defun gtk::text-child-anchor-get-widgets (a)
   (|gtk_text_child_anchor_get_widgets| a)))
(progn
 (defalien "gtk_text_child_anchor_get_deleted" gboolean (a (* t)))
 (defun gtk::text-child-anchor-get-deleted (a)
   (let ((v446 (|gtk_text_child_anchor_get_deleted| a)))
     (if (= v446 1) t nil))))
(progn
 (defalien "gtk_text_iter_get_buffer" (* t) (a (* t)))
 (defun gtk::text-iter-get-buffer (a) (|gtk_text_iter_get_buffer| a)))
(progn
 (defalien "gtk_text_iter_copy" (* t) (a (* t)))
 (defun gtk::text-iter-copy (a) (|gtk_text_iter_copy| a)))
(progn
 (defalien "gtk_text_iter_free" void (a (* t)))
 (defun gtk::text-iter-free (a) (|gtk_text_iter_free| a)))
(progn
 (defalien "gtk_text_iter_get_type" GType)
 (defun gtk::text-iter-get-type () (|gtk_text_iter_get_type|)))
(progn
 (defalien "gtk_text_iter_get_offset" gint (a (* t)))
 (defun gtk::text-iter-get-offset (a) (|gtk_text_iter_get_offset| a)))
(progn
 (defalien "gtk_text_iter_get_line" gint (a (* t)))
 (defun gtk::text-iter-get-line (a) (|gtk_text_iter_get_line| a)))
(progn
 (defalien "gtk_text_iter_get_line_offset" gint (a (* t)))
 (defun gtk::text-iter-get-line-offset (a) (|gtk_text_iter_get_line_offset| a)))
(progn
 (defalien "gtk_text_iter_get_line_index" gint (a (* t)))
 (defun gtk::text-iter-get-line-index (a) (|gtk_text_iter_get_line_index| a)))
(progn
 (defalien "gtk_text_iter_get_visible_line_offset" gint (a (* t)))
 (defun gtk::text-iter-get-visible-line-offset (a)
   (|gtk_text_iter_get_visible_line_offset| a)))
(progn
 (defalien "gtk_text_iter_get_visible_line_index" gint (a (* t)))
 (defun gtk::text-iter-get-visible-line-index (a)
   (|gtk_text_iter_get_visible_line_index| a)))
(progn
 (defalien "gtk_text_iter_get_char" gunichar (a (* t)))
 (defun gtk::text-iter-get-char (a) (|gtk_text_iter_get_char| a)))
(progn
 (defalien "gtk_text_iter_get_slice" gchar (a (* t)) (b (* t)))
 (defun gtk::text-iter-get-slice (a b) (|gtk_text_iter_get_slice| a b)))
(progn
 (defalien "gtk_text_iter_get_text" gchar (a (* t)) (b (* t)))
 (defun gtk::text-iter-get-text (a b) (|gtk_text_iter_get_text| a b)))
(progn
 (defalien "gtk_text_iter_get_visible_slice" gchar (a (* t)) (b (* t)))
 (defun gtk::text-iter-get-visible-slice (a b)
   (|gtk_text_iter_get_visible_slice| a b)))
(progn
 (defalien "gtk_text_iter_get_visible_text" gchar (a (* t)) (b (* t)))
 (defun gtk::text-iter-get-visible-text (a b)
   (|gtk_text_iter_get_visible_text| a b)))
(progn
 (defalien "gtk_text_iter_get_pixbuf" (* t) (a (* t)))
 (defun gtk::text-iter-get-pixbuf (a) (|gtk_text_iter_get_pixbuf| a)))
(progn
 (defalien "gtk_text_iter_get_marks" (* t) (a (* t)))
 (defun gtk::text-iter-get-marks (a) (|gtk_text_iter_get_marks| a)))
(progn
 (defalien "gtk_text_iter_get_child_anchor" (* t) (a (* t)))
 (defun gtk::text-iter-get-child-anchor (a)
   (|gtk_text_iter_get_child_anchor| a)))
(progn
 (defalien "gtk_text_iter_get_toggled_tags" (* t) (a (* t)) (b gboolean))
 (defun gtk::text-iter-get-toggled-tags (a b)
   (|gtk_text_iter_get_toggled_tags| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gtk_text_iter_begins_tag" gboolean (a (* t)) (b (* t)))
 (defun gtk::text-iter-begins-tag (a b)
   (let ((v447 (|gtk_text_iter_begins_tag| a b)))
     (if (= v447 1) t nil))))
(progn
 (defalien "gtk_text_iter_ends_tag" gboolean (a (* t)) (b (* t)))
 (defun gtk::text-iter-ends-tag (a b)
   (let ((v448 (|gtk_text_iter_ends_tag| a b)))
     (if (= v448 1) t nil))))
(progn
 (defalien "gtk_text_iter_toggles_tag" gboolean (a (* t)) (b (* t)))
 (defun gtk::text-iter-toggles-tag (a b)
   (let ((v449 (|gtk_text_iter_toggles_tag| a b)))
     (if (= v449 1) t nil))))
(progn
 (defalien "gtk_text_iter_has_tag" gboolean (a (* t)) (b (* t)))
 (defun gtk::text-iter-has-tag (a b)
   (let ((v450 (|gtk_text_iter_has_tag| a b)))
     (if (= v450 1) t nil))))
(progn
 (defalien "gtk_text_iter_get_tags" (* t) (a (* t)))
 (defun gtk::text-iter-get-tags (a) (|gtk_text_iter_get_tags| a)))
(progn
 (defalien "gtk_text_iter_editable" gboolean (a (* t)) (b gboolean))
 (defun gtk::text-iter-editable (a b)
   (let ((v451 (|gtk_text_iter_editable| a (if b (if (eq b 0) 0 1) 0))))
     (if (= v451 1) t nil))))
(progn
 (defalien "gtk_text_iter_can_insert" gboolean (a (* t)) (b gboolean))
 (defun gtk::text-iter-can-insert (a b)
   (let ((v452 (|gtk_text_iter_can_insert| a (if b (if (eq b 0) 0 1) 0))))
     (if (= v452 1) t nil))))
(progn
 (defalien "gtk_text_iter_starts_word" gboolean (a (* t)))
 (defun gtk::text-iter-starts-word (a)
   (let ((v453 (|gtk_text_iter_starts_word| a)))
     (if (= v453 1) t nil))))
(progn
 (defalien "gtk_text_iter_ends_word" gboolean (a (* t)))
 (defun gtk::text-iter-ends-word (a)
   (let ((v454 (|gtk_text_iter_ends_word| a)))
     (if (= v454 1) t nil))))
(progn
 (defalien "gtk_text_iter_inside_word" gboolean (a (* t)))
 (defun gtk::text-iter-inside-word (a)
   (let ((v455 (|gtk_text_iter_inside_word| a)))
     (if (= v455 1) t nil))))
(progn
 (defalien "gtk_text_iter_starts_sentence" gboolean (a (* t)))
 (defun gtk::text-iter-starts-sentence (a)
   (let ((v456 (|gtk_text_iter_starts_sentence| a)))
     (if (= v456 1) t nil))))
(progn
 (defalien "gtk_text_iter_ends_sentence" gboolean (a (* t)))
 (defun gtk::text-iter-ends-sentence (a)
   (let ((v457 (|gtk_text_iter_ends_sentence| a)))
     (if (= v457 1) t nil))))
(progn
 (defalien "gtk_text_iter_inside_sentence" gboolean (a (* t)))
 (defun gtk::text-iter-inside-sentence (a)
   (let ((v458 (|gtk_text_iter_inside_sentence| a)))
     (if (= v458 1) t nil))))
(progn
 (defalien "gtk_text_iter_starts_line" gboolean (a (* t)))
 (defun gtk::text-iter-starts-line (a)
   (let ((v459 (|gtk_text_iter_starts_line| a)))
     (if (= v459 1) t nil))))
(progn
 (defalien "gtk_text_iter_ends_line" gboolean (a (* t)))
 (defun gtk::text-iter-ends-line (a)
   (let ((v460 (|gtk_text_iter_ends_line| a)))
     (if (= v460 1) t nil))))
(progn
 (defalien "gtk_text_iter_is_cursor_position" gboolean (a (* t)))
 (defun gtk::text-iter-is-cursor-position (a)
   (let ((v461 (|gtk_text_iter_is_cursor_position| a)))
     (if (= v461 1) t nil))))
(progn
 (defalien "gtk_text_iter_get_chars_in_line" gint (a (* t)))
 (defun gtk::text-iter-get-chars-in-line (a)
   (|gtk_text_iter_get_chars_in_line| a)))
(progn
 (defalien "gtk_text_iter_get_bytes_in_line" gint (a (* t)))
 (defun gtk::text-iter-get-bytes-in-line (a)
   (|gtk_text_iter_get_bytes_in_line| a)))
(progn
 (defalien "gtk_text_iter_get_attributes" gboolean (a (* t)) (b (* t)))
 (defun gtk::text-iter-get-attributes (a b)
   (let ((v462 (|gtk_text_iter_get_attributes| a b)))
     (if (= v462 1) t nil))))
(progn
 (defalien "gtk_text_iter_get_language" (* t) (a (* t)))
 (defun gtk::text-iter-get-language (a) (|gtk_text_iter_get_language| a)))
(progn
 (defalien "gtk_text_iter_is_end" gboolean (a (* t)))
 (defun gtk::text-iter-is-end (a)
   (let ((v463 (|gtk_text_iter_is_end| a)))
     (if (= v463 1) t nil))))
(progn
 (defalien "gtk_text_iter_is_start" gboolean (a (* t)))
 (defun gtk::text-iter-is-start (a)
   (let ((v464 (|gtk_text_iter_is_start| a)))
     (if (= v464 1) t nil))))
(progn
 (defalien "gtk_text_iter_forward_char" gboolean (a (* t)))
 (defun gtk::text-iter-forward-char (a)
   (let ((v465 (|gtk_text_iter_forward_char| a)))
     (if (= v465 1) t nil))))
(progn
 (defalien "gtk_text_iter_backward_char" gboolean (a (* t)))
 (defun gtk::text-iter-backward-char (a)
   (let ((v466 (|gtk_text_iter_backward_char| a)))
     (if (= v466 1) t nil))))
(progn
 (defalien "gtk_text_iter_forward_chars" gboolean (a (* t)) (b gint))
 (defun gtk::text-iter-forward-chars (a b)
   (let ((v467 (|gtk_text_iter_forward_chars| a b)))
     (if (= v467 1) t nil))))
(progn
 (defalien "gtk_text_iter_backward_chars" gboolean (a (* t)) (b gint))
 (defun gtk::text-iter-backward-chars (a b)
   (let ((v468 (|gtk_text_iter_backward_chars| a b)))
     (if (= v468 1) t nil))))
(progn
 (defalien "gtk_text_iter_forward_line" gboolean (a (* t)))
 (defun gtk::text-iter-forward-line (a)
   (let ((v469 (|gtk_text_iter_forward_line| a)))
     (if (= v469 1) t nil))))
(progn
 (defalien "gtk_text_iter_backward_line" gboolean (a (* t)))
 (defun gtk::text-iter-backward-line (a)
   (let ((v470 (|gtk_text_iter_backward_line| a)))
     (if (= v470 1) t nil))))
(progn
 (defalien "gtk_text_iter_forward_lines" gboolean (a (* t)) (b gint))
 (defun gtk::text-iter-forward-lines (a b)
   (let ((v471 (|gtk_text_iter_forward_lines| a b)))
     (if (= v471 1) t nil))))
(progn
 (defalien "gtk_text_iter_backward_lines" gboolean (a (* t)) (b gint))
 (defun gtk::text-iter-backward-lines (a b)
   (let ((v472 (|gtk_text_iter_backward_lines| a b)))
     (if (= v472 1) t nil))))
(progn
 (defalien "gtk_text_iter_forward_word_end" gboolean (a (* t)))
 (defun gtk::text-iter-forward-word-end (a)
   (let ((v473 (|gtk_text_iter_forward_word_end| a)))
     (if (= v473 1) t nil))))
(progn
 (defalien "gtk_text_iter_backward_word_start" gboolean (a (* t)))
 (defun gtk::text-iter-backward-word-start (a)
   (let ((v474 (|gtk_text_iter_backward_word_start| a)))
     (if (= v474 1) t nil))))
(progn
 (defalien "gtk_text_iter_forward_word_ends" gboolean (a (* t)) (b gint))
 (defun gtk::text-iter-forward-word-ends (a b)
   (let ((v475 (|gtk_text_iter_forward_word_ends| a b)))
     (if (= v475 1) t nil))))
(progn
 (defalien "gtk_text_iter_backward_word_starts" gboolean (a (* t)) (b gint))
 (defun gtk::text-iter-backward-word-starts (a b)
   (let ((v476 (|gtk_text_iter_backward_word_starts| a b)))
     (if (= v476 1) t nil))))
(progn
 (defalien "gtk_text_iter_forward_sentence_end" gboolean (a (* t)))
 (defun gtk::text-iter-forward-sentence-end (a)
   (let ((v477 (|gtk_text_iter_forward_sentence_end| a)))
     (if (= v477 1) t nil))))
(progn
 (defalien "gtk_text_iter_backward_sentence_start" gboolean (a (* t)))
 (defun gtk::text-iter-backward-sentence-start (a)
   (let ((v478 (|gtk_text_iter_backward_sentence_start| a)))
     (if (= v478 1) t nil))))
(progn
 (defalien "gtk_text_iter_forward_sentence_ends" gboolean (a (* t)) (b gint))
 (defun gtk::text-iter-forward-sentence-ends (a b)
   (let ((v479 (|gtk_text_iter_forward_sentence_ends| a b)))
     (if (= v479 1) t nil))))
(progn
 (defalien "gtk_text_iter_backward_sentence_starts"
           gboolean
           (a (* t))
           (b gint))
 (defun gtk::text-iter-backward-sentence-starts (a b)
   (let ((v480 (|gtk_text_iter_backward_sentence_starts| a b)))
     (if (= v480 1) t nil))))
(progn
 (defalien "gtk_text_iter_forward_cursor_position" gboolean (a (* t)))
 (defun gtk::text-iter-forward-cursor-position (a)
   (let ((v481 (|gtk_text_iter_forward_cursor_position| a)))
     (if (= v481 1) t nil))))
(progn
 (defalien "gtk_text_iter_backward_cursor_position" gboolean (a (* t)))
 (defun gtk::text-iter-backward-cursor-position (a)
   (let ((v482 (|gtk_text_iter_backward_cursor_position| a)))
     (if (= v482 1) t nil))))
(progn
 (defalien "gtk_text_iter_forward_cursor_positions"
           gboolean
           (a (* t))
           (b gint))
 (defun gtk::text-iter-forward-cursor-positions (a b)
   (let ((v483 (|gtk_text_iter_forward_cursor_positions| a b)))
     (if (= v483 1) t nil))))
(progn
 (defalien "gtk_text_iter_backward_cursor_positions"
           gboolean
           (a (* t))
           (b gint))
 (defun gtk::text-iter-backward-cursor-positions (a b)
   (let ((v484 (|gtk_text_iter_backward_cursor_positions| a b)))
     (if (= v484 1) t nil))))
(progn
 (defalien "gtk_text_iter_set_offset" void (a (* t)) (b gint))
 (defun gtk::text-iter-set-offset (a b) (|gtk_text_iter_set_offset| a b)))
(progn
 (defalien "gtk_text_iter_set_line" void (a (* t)) (b gint))
 (defun gtk::text-iter-set-line (a b) (|gtk_text_iter_set_line| a b)))
(progn
 (defalien "gtk_text_iter_set_line_offset" void (a (* t)) (b gint))
 (defun gtk::text-iter-set-line-offset (a b)
   (|gtk_text_iter_set_line_offset| a b)))
(progn
 (defalien "gtk_text_iter_set_line_index" void (a (* t)) (b gint))
 (defun gtk::text-iter-set-line-index (a b)
   (|gtk_text_iter_set_line_index| a b)))
(progn
 (defalien "gtk_text_iter_forward_to_end" void (a (* t)))
 (defun gtk::text-iter-forward-to-end (a) (|gtk_text_iter_forward_to_end| a)))
(progn
 (defalien "gtk_text_iter_forward_to_line_end" gboolean (a (* t)))
 (defun gtk::text-iter-forward-to-line-end (a)
   (let ((v485 (|gtk_text_iter_forward_to_line_end| a)))
     (if (= v485 1) t nil))))
(progn
 (defalien "gtk_text_iter_set_visible_line_offset" void (a (* t)) (b gint))
 (defun gtk::text-iter-set-visible-line-offset (a b)
   (|gtk_text_iter_set_visible_line_offset| a b)))
(progn
 (defalien "gtk_text_iter_set_visible_line_index" void (a (* t)) (b gint))
 (defun gtk::text-iter-set-visible-line-index (a b)
   (|gtk_text_iter_set_visible_line_index| a b)))
(progn
 (defalien "gtk_text_iter_forward_to_tag_toggle" gboolean (a (* t)) (b (* t)))
 (defun gtk::text-iter-forward-to-tag-toggle (a b)
   (let ((v486 (|gtk_text_iter_forward_to_tag_toggle| a b)))
     (if (= v486 1) t nil))))
(progn
 (defalien "gtk_text_iter_backward_to_tag_toggle" gboolean (a (* t)) (b (* t)))
 (defun gtk::text-iter-backward-to-tag-toggle (a b)
   (let ((v487 (|gtk_text_iter_backward_to_tag_toggle| a b)))
     (if (= v487 1) t nil))))
(progn
 (defalien "gtk_text_iter_forward_find_char"
           gboolean
           (a (* t))
           (b GtkTextCharPredicate)
           (c gpointer)
           (d (* t)))
 (defun gtk::text-iter-forward-find-char (a b c d)
   (let ((v488 (|gtk_text_iter_forward_find_char| a b c d)))
     (if (= v488 1) t nil))))
(progn
 (defalien "gtk_text_iter_backward_find_char"
           gboolean
           (a (* t))
           (b GtkTextCharPredicate)
           (c gpointer)
           (d (* t)))
 (defun gtk::text-iter-backward-find-char (a b c d)
   (let ((v489 (|gtk_text_iter_backward_find_char| a b c d)))
     (if (= v489 1) t nil))))
(progn
 (defalien "gtk_text_iter_forward_search"
           gboolean
           (a (* t))
           (b c-string)
           (c GtkTextSearchFlags)
           (d (* t))
           (e (* t))
           (f (* t)))
 (defun gtk::text-iter-forward-search (a b c d e f)
   (let ((v490 (|gtk_text_iter_forward_search| a b c d e f)))
     (if (= v490 1) t nil))))
(progn
 (defalien "gtk_text_iter_backward_search"
           gboolean
           (a (* t))
           (b c-string)
           (c GtkTextSearchFlags)
           (d (* t))
           (e (* t))
           (f (* t)))
 (defun gtk::text-iter-backward-search (a b c d e f)
   (let ((v491 (|gtk_text_iter_backward_search| a b c d e f)))
     (if (= v491 1) t nil))))
(progn
 (defalien "gtk_text_iter_equal" gboolean (a (* t)) (b (* t)))
 (defun gtk::text-iter-equal (a b)
   (let ((v492 (|gtk_text_iter_equal| a b)))
     (if (= v492 1) t nil))))
(progn
 (defalien "gtk_text_iter_compare" gint (a (* t)) (b (* t)))
 (defun gtk::text-iter-compare (a b) (|gtk_text_iter_compare| a b)))
(progn
 (defalien "gtk_text_iter_in_range" gboolean (a (* t)) (b (* t)) (c (* t)))
 (defun gtk::text-iter-in-range (a b c)
   (let ((v493 (|gtk_text_iter_in_range| a b c)))
     (if (= v493 1) t nil))))
(progn
 (defalien "gtk_text_iter_order" void (a (* t)) (b (* t)))
 (defun gtk::text-iter-order (a b) (|gtk_text_iter_order| a b)))
(progn
 (defalien "gtk_text_mark_get_type" GType)
 (defun gtk::text-mark-get-type () (|gtk_text_mark_get_type|)))
(progn
 (defalien "gtk_text_mark_set_visible" void (a (* t)) (b gboolean))
 (defun gtk::text-mark-set-visible (a b)
   (|gtk_text_mark_set_visible| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gtk_text_mark_get_visible" gboolean (a (* t)))
 (defun gtk::text-mark-get-visible (a)
   (let ((v494 (|gtk_text_mark_get_visible| a)))
     (if (= v494 1) t nil))))
(progn
 (defalien "gtk_text_mark_get_name" c-string (a (* t)))
 (defun gtk::text-mark-get-name (a) (|gtk_text_mark_get_name| a)))
(progn
 (defalien "gtk_text_mark_get_deleted" gboolean (a (* t)))
 (defun gtk::text-mark-get-deleted (a)
   (let ((v495 (|gtk_text_mark_get_deleted| a)))
     (if (= v495 1) t nil))))
(progn
 (defalien "gtk_text_mark_get_buffer" (* t) (a (* t)))
 (defun gtk::text-mark-get-buffer (a) (|gtk_text_mark_get_buffer| a)))
(progn
 (defalien "gtk_text_mark_get_left_gravity" gboolean (a (* t)))
 (defun gtk::text-mark-get-left-gravity (a)
   (let ((v496 (|gtk_text_mark_get_left_gravity| a)))
     (if (= v496 1) t nil))))
(progn
 (defalien "gtk_text_buffer_get_type" GType)
 (defun gtk::text-buffer-get-type () (|gtk_text_buffer_get_type|)))
(progn
 (defalien "gtk_text_buffer_new" (* t) (a (* t)))
 (defun gtk::text-buffer-new (a) (|gtk_text_buffer_new| a)))
(progn
 (defalien "gtk_text_buffer_get_line_count" gint (a (* t)))
 (defun gtk::text-buffer-get-line-count (a)
   (|gtk_text_buffer_get_line_count| a)))
(progn
 (defalien "gtk_text_buffer_get_char_count" gint (a (* t)))
 (defun gtk::text-buffer-get-char-count (a)
   (|gtk_text_buffer_get_char_count| a)))
(progn
 (defalien "gtk_text_buffer_get_tag_table" (* t) (a (* t)))
 (defun gtk::text-buffer-get-tag-table (a) (|gtk_text_buffer_get_tag_table| a)))
(progn
 (defalien "gtk_text_buffer_set_text" void (a (* t)) (b c-string) (c gint))
 (defun gtk::text-buffer-set-text (a b c) (|gtk_text_buffer_set_text| a b c)))
(progn
 (defalien "gtk_text_buffer_insert"
           void
           (a (* t))
           (b (* t))
           (c c-string)
           (d gint))
 (defun gtk::text-buffer-insert (a b c d) (|gtk_text_buffer_insert| a b c d)))
(progn
 (defalien "gtk_text_buffer_insert_at_cursor"
           void
           (a (* t))
           (b c-string)
           (c gint))
 (defun gtk::text-buffer-insert-at-cursor (a b c)
   (|gtk_text_buffer_insert_at_cursor| a b c)))
(progn
 (defalien "gtk_text_buffer_insert_interactive"
           gboolean
           (a (* t))
           (b (* t))
           (c c-string)
           (d gint)
           (e gboolean))
 (defun gtk::text-buffer-insert-interactive (a b c d e)
   (let ((v497
          (|gtk_text_buffer_insert_interactive| a b c d
           (if e (if (eq e 0) 0 1) 0))))
     (if (= v497 1) t nil))))
(progn
 (defalien "gtk_text_buffer_insert_interactive_at_cursor"
           gboolean
           (a (* t))
           (b c-string)
           (c gint)
           (d gboolean))
 (defun gtk::text-buffer-insert-interactive-at-cursor (a b c d)
   (let ((v498
          (|gtk_text_buffer_insert_interactive_at_cursor| a b c
           (if d (if (eq d 0) 0 1) 0))))
     (if (= v498 1) t nil))))
(progn
 (defalien "gtk_text_buffer_insert_range"
           void
           (a (* t))
           (b (* t))
           (c (* t))
           (d (* t)))
 (defun gtk::text-buffer-insert-range (a b c d)
   (|gtk_text_buffer_insert_range| a b c d)))
(progn
 (defalien "gtk_text_buffer_insert_range_interactive"
           gboolean
           (a (* t))
           (b (* t))
           (c (* t))
           (d (* t))
           (e gboolean))
 (defun gtk::text-buffer-insert-range-interactive (a b c d e)
   (let ((v499
          (|gtk_text_buffer_insert_range_interactive| a b c d
           (if e (if (eq e 0) 0 1) 0))))
     (if (= v499 1) t nil))))
(progn
 (defalien "gtk_text_buffer_insert_with_tags"
           void
           (a (* t))
           (b (* t))
           (c c-string)
           (d gint)
           (e (* t)))
 (defun gtk::text-buffer-insert-with-tags (a b c d e)
   (|gtk_text_buffer_insert_with_tags| a b c d e)))
(progn
 (defalien "gtk_text_buffer_insert_with_tags_by_name"
           void
           (a (* t))
           (b (* t))
           (c c-string)
           (d gint)
           (e c-string))
 (defun gtk::text-buffer-insert-with-tags-by-name (a b c d e)
   (|gtk_text_buffer_insert_with_tags_by_name| a b c d e)))
(progn
 (defalien "gtk_text_buffer_delete" void (a (* t)) (b (* t)) (c (* t)))
 (defun gtk::text-buffer-delete (a b c) (|gtk_text_buffer_delete| a b c)))
(progn
 (defalien "gtk_text_buffer_delete_interactive"
           gboolean
           (a (* t))
           (b (* t))
           (c (* t))
           (d gboolean))
 (defun gtk::text-buffer-delete-interactive (a b c d)
   (let ((v500
          (|gtk_text_buffer_delete_interactive| a b c
           (if d (if (eq d 0) 0 1) 0))))
     (if (= v500 1) t nil))))
(progn
 (defalien "gtk_text_buffer_get_text"
           gchar
           (a (* t))
           (b (* t))
           (c (* t))
           (d gboolean))
 (defun gtk::text-buffer-get-text (a b c d)
   (|gtk_text_buffer_get_text| a b c (if d (if (eq d 0) 0 1) 0))))
(progn
 (defalien "gtk_text_buffer_get_slice"
           gchar
           (a (* t))
           (b (* t))
           (c (* t))
           (d gboolean))
 (defun gtk::text-buffer-get-slice (a b c d)
   (|gtk_text_buffer_get_slice| a b c (if d (if (eq d 0) 0 1) 0))))
(progn
 (defalien "gtk_text_buffer_insert_pixbuf" void (a (* t)) (b (* t)) (c (* t)))
 (defun gtk::text-buffer-insert-pixbuf (a b c)
   (|gtk_text_buffer_insert_pixbuf| a b c)))
(progn
 (defalien "gtk_text_buffer_insert_child_anchor"
           void
           (a (* t))
           (b (* t))
           (c (* t)))
 (defun gtk::text-buffer-insert-child-anchor (a b c)
   (|gtk_text_buffer_insert_child_anchor| a b c)))
(progn
 (defalien "gtk_text_buffer_create_child_anchor" (* t) (a (* t)) (b (* t)))
 (defun gtk::text-buffer-create-child-anchor (a b)
   (|gtk_text_buffer_create_child_anchor| a b)))
(progn
 (defalien "gtk_text_buffer_create_mark"
           (* t)
           (a (* t))
           (b c-string)
           (c (* t))
           (d gboolean))
 (defun gtk::text-buffer-create-mark (a b c d)
   (|gtk_text_buffer_create_mark| a b c (if d (if (eq d 0) 0 1) 0))))
(progn
 (defalien "gtk_text_buffer_move_mark" void (a (* t)) (b (* t)) (c (* t)))
 (defun gtk::text-buffer-move-mark (a b c) (|gtk_text_buffer_move_mark| a b c)))
(progn
 (defalien "gtk_text_buffer_delete_mark" void (a (* t)) (b (* t)))
 (defun gtk::text-buffer-delete-mark (a b) (|gtk_text_buffer_delete_mark| a b)))
(progn
 (defalien "gtk_text_buffer_get_mark" (* t) (a (* t)) (b c-string))
 (defun gtk::text-buffer-get-mark (a b) (|gtk_text_buffer_get_mark| a b)))
(progn
 (defalien "gtk_text_buffer_move_mark_by_name"
           void
           (a (* t))
           (b c-string)
           (c (* t)))
 (defun gtk::text-buffer-move-mark-by-name (a b c)
   (|gtk_text_buffer_move_mark_by_name| a b c)))
(progn
 (defalien "gtk_text_buffer_delete_mark_by_name" void (a (* t)) (b c-string))
 (defun gtk::text-buffer-delete-mark-by-name (a b)
   (|gtk_text_buffer_delete_mark_by_name| a b)))
(progn
 (defalien "gtk_text_buffer_get_insert" (* t) (a (* t)))
 (defun gtk::text-buffer-get-insert (a) (|gtk_text_buffer_get_insert| a)))
(progn
 (defalien "gtk_text_buffer_get_selection_bound" (* t) (a (* t)))
 (defun gtk::text-buffer-get-selection-bound (a)
   (|gtk_text_buffer_get_selection_bound| a)))
(progn
 (defalien "gtk_text_buffer_place_cursor" void (a (* t)) (b (* t)))
 (defun gtk::text-buffer-place-cursor (a b)
   (|gtk_text_buffer_place_cursor| a b)))
(progn
 (defalien "gtk_text_buffer_apply_tag"
           void
           (a (* t))
           (b (* t))
           (c (* t))
           (d (* t)))
 (defun gtk::text-buffer-apply-tag (a b c d)
   (|gtk_text_buffer_apply_tag| a b c d)))
(progn
 (defalien "gtk_text_buffer_remove_tag"
           void
           (a (* t))
           (b (* t))
           (c (* t))
           (d (* t)))
 (defun gtk::text-buffer-remove-tag (a b c d)
   (|gtk_text_buffer_remove_tag| a b c d)))
(progn
 (defalien "gtk_text_buffer_apply_tag_by_name"
           void
           (a (* t))
           (b c-string)
           (c (* t))
           (d (* t)))
 (defun gtk::text-buffer-apply-tag-by-name (a b c d)
   (|gtk_text_buffer_apply_tag_by_name| a b c d)))
(progn
 (defalien "gtk_text_buffer_remove_tag_by_name"
           void
           (a (* t))
           (b c-string)
           (c (* t))
           (d (* t)))
 (defun gtk::text-buffer-remove-tag-by-name (a b c d)
   (|gtk_text_buffer_remove_tag_by_name| a b c d)))
(progn
 (defalien "gtk_text_buffer_remove_all_tags"
           void
           (a (* t))
           (b (* t))
           (c (* t)))
 (defun gtk::text-buffer-remove-all-tags (a b c)
   (|gtk_text_buffer_remove_all_tags| a b c)))
(progn
 (defalien "gtk_text_buffer_create_tag"
           (* t)
           (a (* t))
           (b c-string)
           (c c-string))
 (defun gtk::text-buffer-create-tag (a b c)
   (|gtk_text_buffer_create_tag| a b c)))
(progn
 (defalien "gtk_text_buffer_get_iter_at_line_offset"
           void
           (a (* t))
           (b (* t))
           (c gint)
           (d gint))
 (defun gtk::text-buffer-get-iter-at-line-offset (a b c d)
   (|gtk_text_buffer_get_iter_at_line_offset| a b c d)))
(progn
 (defalien "gtk_text_buffer_get_iter_at_line_index"
           void
           (a (* t))
           (b (* t))
           (c gint)
           (d gint))
 (defun gtk::text-buffer-get-iter-at-line-index (a b c d)
   (|gtk_text_buffer_get_iter_at_line_index| a b c d)))
(progn
 (defalien "gtk_text_buffer_get_iter_at_offset"
           void
           (a (* t))
           (b (* t))
           (c gint))
 (defun gtk::text-buffer-get-iter-at-offset (a b c)
   (|gtk_text_buffer_get_iter_at_offset| a b c)))
(progn
 (defalien "gtk_text_buffer_get_iter_at_line"
           void
           (a (* t))
           (b (* t))
           (c gint))
 (defun gtk::text-buffer-get-iter-at-line (a b c)
   (|gtk_text_buffer_get_iter_at_line| a b c)))
(progn
 (defalien "gtk_text_buffer_get_start_iter" void (a (* t)) (b (* t)))
 (defun gtk::text-buffer-get-start-iter (a b)
   (|gtk_text_buffer_get_start_iter| a b)))
(progn
 (defalien "gtk_text_buffer_get_end_iter" void (a (* t)) (b (* t)))
 (defun gtk::text-buffer-get-end-iter (a b)
   (|gtk_text_buffer_get_end_iter| a b)))
(progn
 (defalien "gtk_text_buffer_get_bounds" void (a (* t)) (b (* t)) (c (* t)))
 (defun gtk::text-buffer-get-bounds (a b c)
   (|gtk_text_buffer_get_bounds| a b c)))
(progn
 (defalien "gtk_text_buffer_get_iter_at_mark"
           void
           (a (* t))
           (b (* t))
           (c (* t)))
 (defun gtk::text-buffer-get-iter-at-mark (a b c)
   (|gtk_text_buffer_get_iter_at_mark| a b c)))
(progn
 (defalien "gtk_text_buffer_get_iter_at_child_anchor"
           void
           (a (* t))
           (b (* t))
           (c (* t)))
 (defun gtk::text-buffer-get-iter-at-child-anchor (a b c)
   (|gtk_text_buffer_get_iter_at_child_anchor| a b c)))
(progn
 (defalien "gtk_text_buffer_get_modified" gboolean (a (* t)))
 (defun gtk::text-buffer-get-modified (a)
   (let ((v501 (|gtk_text_buffer_get_modified| a)))
     (if (= v501 1) t nil))))
(progn
 (defalien "gtk_text_buffer_set_modified" void (a (* t)) (b gboolean))
 (defun gtk::text-buffer-set-modified (a b)
   (|gtk_text_buffer_set_modified| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gtk_text_buffer_add_selection_clipboard" void (a (* t)) (b (* t)))
 (defun gtk::text-buffer-add-selection-clipboard (a b)
   (|gtk_text_buffer_add_selection_clipboard| a b)))
(progn
 (defalien "gtk_text_buffer_remove_selection_clipboard"
           void
           (a (* t))
           (b (* t)))
 (defun gtk::text-buffer-remove-selection-clipboard (a b)
   (|gtk_text_buffer_remove_selection_clipboard| a b)))
(progn
 (defalien "gtk_text_buffer_cut_clipboard"
           void
           (a (* t))
           (b (* t))
           (c gboolean))
 (defun gtk::text-buffer-cut-clipboard (a b c)
   (|gtk_text_buffer_cut_clipboard| a b (if c (if (eq c 0) 0 1) 0))))
(progn
 (defalien "gtk_text_buffer_copy_clipboard" void (a (* t)) (b (* t)))
 (defun gtk::text-buffer-copy-clipboard (a b)
   (|gtk_text_buffer_copy_clipboard| a b)))
(progn
 (defalien "gtk_text_buffer_paste_clipboard"
           void
           (a (* t))
           (b (* t))
           (c (* t))
           (d gboolean))
 (defun gtk::text-buffer-paste-clipboard (a b c d)
   (|gtk_text_buffer_paste_clipboard| a b c (if d (if (eq d 0) 0 1) 0))))
(progn
 (defalien "gtk_text_buffer_get_selection_bounds"
           gboolean
           (a (* t))
           (b (* t))
           (c (* t)))
 (defun gtk::text-buffer-get-selection-bounds (a b c)
   (let ((v502 (|gtk_text_buffer_get_selection_bounds| a b c)))
     (if (= v502 1) t nil))))
(progn
 (defalien "gtk_text_buffer_delete_selection"
           gboolean
           (a (* t))
           (b gboolean)
           (c gboolean))
 (defun gtk::text-buffer-delete-selection (a b c)
   (let ((v503
          (|gtk_text_buffer_delete_selection| a (if b (if (eq b 0) 0 1) 0)
           (if c (if (eq c 0) 0 1) 0))))
     (if (= v503 1) t nil))))
(progn
 (defalien "gtk_text_buffer_begin_user_action" void (a (* t)))
 (defun gtk::text-buffer-begin-user-action (a)
   (|gtk_text_buffer_begin_user_action| a)))
(progn
 (defalien "gtk_text_buffer_end_user_action" void (a (* t)))
 (defun gtk::text-buffer-end-user-action (a)
   (|gtk_text_buffer_end_user_action| a)))
(progn
 (defalien "gtk_text_view_get_type" GType)
 (defun gtk::text-view-get-type () (|gtk_text_view_get_type|)))
(progn
 (defalien "gtk_text_view_new" (* t))
 (defun gtk::text-view-new () (|gtk_text_view_new|)))
(progn
 (defalien "gtk_text_view_new_with_buffer" (* t) (a (* t)))
 (defun gtk::text-view-new-with-buffer (a) (|gtk_text_view_new_with_buffer| a)))
(progn
 (defalien "gtk_text_view_set_buffer" void (a (* t)) (b (* t)))
 (defun gtk::text-view-set-buffer (a b) (|gtk_text_view_set_buffer| a b)))
(progn
 (defalien "gtk_text_view_get_buffer" (* t) (a (* t)))
 (defun gtk::text-view-get-buffer (a) (|gtk_text_view_get_buffer| a)))
(progn
 (defalien "gtk_text_view_scroll_to_iter"
           gboolean
           (a (* t))
           (b (* t))
           (c gdouble)
           (d gboolean)
           (e gdouble)
           (f gdouble))
 (defun gtk::text-view-scroll-to-iter (a b c d e f)
   (let ((v504
          (|gtk_text_view_scroll_to_iter| a b (coerce c 'double-float)
           (if d (if (eq d 0) 0 1) 0) (coerce e 'double-float)
           (coerce f 'double-float))))
     (if (= v504 1) t nil))))
(progn
 (defalien "gtk_text_view_scroll_to_mark"
           void
           (a (* t))
           (b (* t))
           (c gdouble)
           (d gboolean)
           (e gdouble)
           (f gdouble))
 (defun gtk::text-view-scroll-to-mark (a b c d e f)
   (|gtk_text_view_scroll_to_mark| a b (coerce c 'double-float)
    (if d (if (eq d 0) 0 1) 0) (coerce e 'double-float)
    (coerce f 'double-float))))
(progn
 (defalien "gtk_text_view_scroll_mark_onscreen" void (a (* t)) (b (* t)))
 (defun gtk::text-view-scroll-mark-onscreen (a b)
   (|gtk_text_view_scroll_mark_onscreen| a b)))
(progn
 (defalien "gtk_text_view_move_mark_onscreen" gboolean (a (* t)) (b (* t)))
 (defun gtk::text-view-move-mark-onscreen (a b)
   (let ((v505 (|gtk_text_view_move_mark_onscreen| a b)))
     (if (= v505 1) t nil))))
(progn
 (defalien "gtk_text_view_place_cursor_onscreen" gboolean (a (* t)))
 (defun gtk::text-view-place-cursor-onscreen (a)
   (let ((v506 (|gtk_text_view_place_cursor_onscreen| a)))
     (if (= v506 1) t nil))))
(progn
 (defalien "gtk_text_view_get_visible_rect" void (a (* t)) (b (* t)))
 (defun gtk::text-view-get-visible-rect (a b)
   (|gtk_text_view_get_visible_rect| a b)))
(progn
 (defalien "gtk_text_view_set_cursor_visible" void (a (* t)) (b gboolean))
 (defun gtk::text-view-set-cursor-visible (a b)
   (|gtk_text_view_set_cursor_visible| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gtk_text_view_get_cursor_visible" gboolean (a (* t)))
 (defun gtk::text-view-get-cursor-visible (a)
   (let ((v507 (|gtk_text_view_get_cursor_visible| a)))
     (if (= v507 1) t nil))))
(progn
 (defalien "gtk_text_view_get_iter_location"
           void
           (a (* t))
           (b (* t))
           (c (* t)))
 (defun gtk::text-view-get-iter-location (a b c)
   (|gtk_text_view_get_iter_location| a b c)))
(progn
 (defalien "gtk_text_view_get_iter_at_location"
           void
           (a (* t))
           (b (* t))
           (c gint)
           (d gint))
 (defun gtk::text-view-get-iter-at-location (a b c d)
   (|gtk_text_view_get_iter_at_location| a b c d)))
(progn
 (defalien "gtk_text_view_get_line_yrange"
           void
           (a (* t))
           (b (* t))
           (c gint :in-out)
           (d gint :in-out))
 (defun gtk::text-view-get-line-yrange (a b c d)
   (|gtk_text_view_get_line_yrange| a b c d)))
(progn
 (defalien "gtk_text_view_get_line_at_y"
           void
           (a (* t))
           (b (* t))
           (c gint)
           (d gint :in-out))
 (defun gtk::text-view-get-line-at-y (a b c d)
   (|gtk_text_view_get_line_at_y| a b c d)))
(progn
 (defalien "gtk_text_view_buffer_to_window_coords"
           void
           (a (* t))
           (b GtkTextWindowType)
           (c gint)
           (d gint)
           (e gint :in-out)
           (f gint :in-out))
 (defun gtk::text-view-buffer-to-window-coords (a b c d e f)
   (|gtk_text_view_buffer_to_window_coords| a b c d e f)))
(progn
 (defalien "gtk_text_view_window_to_buffer_coords"
           void
           (a (* t))
           (b GtkTextWindowType)
           (c gint)
           (d gint)
           (e gint :in-out)
           (f gint :in-out))
 (defun gtk::text-view-window-to-buffer-coords (a b c d e f)
   (|gtk_text_view_window_to_buffer_coords| a b c d e f)))
(progn
 (defalien "gtk_text_view_get_window" (* t) (a (* t)) (b GtkTextWindowType))
 (defun gtk::text-view-get-window (a b) (|gtk_text_view_get_window| a b)))
(progn
 (defalien "gtk_text_view_get_window_type"
           GtkTextWindowType
           (a (* t))
           (b (* t)))
 (defun gtk::text-view-get-window-type (a b)
   (|gtk_text_view_get_window_type| a b)))
(progn
 (defalien "gtk_text_view_set_border_window_size"
           void
           (a (* t))
           (b GtkTextWindowType)
           (c gint))
 (defun gtk::text-view-set-border-window-size (a b c)
   (|gtk_text_view_set_border_window_size| a b c)))
(progn
 (defalien "gtk_text_view_get_border_window_size"
           gint
           (a (* t))
           (b GtkTextWindowType))
 (defun gtk::text-view-get-border-window-size (a b)
   (|gtk_text_view_get_border_window_size| a b)))
(progn
 (defalien "gtk_text_view_forward_display_line" gboolean (a (* t)) (b (* t)))
 (defun gtk::text-view-forward-display-line (a b)
   (let ((v508 (|gtk_text_view_forward_display_line| a b)))
     (if (= v508 1) t nil))))
(progn
 (defalien "gtk_text_view_backward_display_line" gboolean (a (* t)) (b (* t)))
 (defun gtk::text-view-backward-display-line (a b)
   (let ((v509 (|gtk_text_view_backward_display_line| a b)))
     (if (= v509 1) t nil))))
(progn
 (defalien "gtk_text_view_forward_display_line_end"
           gboolean
           (a (* t))
           (b (* t)))
 (defun gtk::text-view-forward-display-line-end (a b)
   (let ((v510 (|gtk_text_view_forward_display_line_end| a b)))
     (if (= v510 1) t nil))))
(progn
 (defalien "gtk_text_view_backward_display_line_start"
           gboolean
           (a (* t))
           (b (* t)))
 (defun gtk::text-view-backward-display-line-start (a b)
   (let ((v511 (|gtk_text_view_backward_display_line_start| a b)))
     (if (= v511 1) t nil))))
(progn
 (defalien "gtk_text_view_starts_display_line" gboolean (a (* t)) (b (* t)))
 (defun gtk::text-view-starts-display-line (a b)
   (let ((v512 (|gtk_text_view_starts_display_line| a b)))
     (if (= v512 1) t nil))))
(progn
 (defalien "gtk_text_view_move_visually" gboolean (a (* t)) (b (* t)) (c gint))
 (defun gtk::text-view-move-visually (a b c)
   (let ((v513 (|gtk_text_view_move_visually| a b c)))
     (if (= v513 1) t nil))))
(progn
 (defalien "gtk_text_view_add_child_at_anchor"
           void
           (a (* t))
           (b (* t))
           (c (* t)))
 (defun gtk::text-view-add-child-at-anchor (a b c)
   (|gtk_text_view_add_child_at_anchor| a b c)))
(progn
 (defalien "gtk_text_view_add_child_in_window"
           void
           (a (* t))
           (b (* t))
           (c GtkTextWindowType)
           (d gint)
           (e gint))
 (defun gtk::text-view-add-child-in-window (a b c d e)
   (|gtk_text_view_add_child_in_window| a b c d e)))
(progn
 (defalien "gtk_text_view_move_child"
           void
           (a (* t))
           (b (* t))
           (c gint)
           (d gint))
 (defun gtk::text-view-move-child (a b c d)
   (|gtk_text_view_move_child| a b c d)))
(progn
 (defalien "gtk_text_view_set_wrap_mode" void (a (* t)) (b GtkWrapMode))
 (defun gtk::text-view-set-wrap-mode (a b) (|gtk_text_view_set_wrap_mode| a b)))
(progn
 (defalien "gtk_text_view_get_wrap_mode" GtkWrapMode (a (* t)))
 (defun gtk::text-view-get-wrap-mode (a) (|gtk_text_view_get_wrap_mode| a)))
(progn
 (defalien "gtk_text_view_set_editable" void (a (* t)) (b gboolean))
 (defun gtk::text-view-set-editable (a b)
   (|gtk_text_view_set_editable| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gtk_text_view_get_editable" gboolean (a (* t)))
 (defun gtk::text-view-get-editable (a)
   (let ((v514 (|gtk_text_view_get_editable| a)))
     (if (= v514 1) t nil))))
(progn
 (defalien "gtk_text_view_set_pixels_above_lines" void (a (* t)) (b gint))
 (defun gtk::text-view-set-pixels-above-lines (a b)
   (|gtk_text_view_set_pixels_above_lines| a b)))
(progn
 (defalien "gtk_text_view_get_pixels_above_lines" gint (a (* t)))
 (defun gtk::text-view-get-pixels-above-lines (a)
   (|gtk_text_view_get_pixels_above_lines| a)))
(progn
 (defalien "gtk_text_view_set_pixels_below_lines" void (a (* t)) (b gint))
 (defun gtk::text-view-set-pixels-below-lines (a b)
   (|gtk_text_view_set_pixels_below_lines| a b)))
(progn
 (defalien "gtk_text_view_get_pixels_below_lines" gint (a (* t)))
 (defun gtk::text-view-get-pixels-below-lines (a)
   (|gtk_text_view_get_pixels_below_lines| a)))
(progn
 (defalien "gtk_text_view_set_pixels_inside_wrap" void (a (* t)) (b gint))
 (defun gtk::text-view-set-pixels-inside-wrap (a b)
   (|gtk_text_view_set_pixels_inside_wrap| a b)))
(progn
 (defalien "gtk_text_view_get_pixels_inside_wrap" gint (a (* t)))
 (defun gtk::text-view-get-pixels-inside-wrap (a)
   (|gtk_text_view_get_pixels_inside_wrap| a)))
(progn
 (defalien "gtk_text_view_set_justification"
           void
           (a (* t))
           (b GtkJustification))
 (defun gtk::text-view-set-justification (a b)
   (|gtk_text_view_set_justification| a b)))
(progn
 (defalien "gtk_text_view_get_justification" GtkJustification (a (* t)))
 (defun gtk::text-view-get-justification (a)
   (|gtk_text_view_get_justification| a)))
(progn
 (defalien "gtk_text_view_set_left_margin" void (a (* t)) (b gint))
 (defun gtk::text-view-set-left-margin (a b)
   (|gtk_text_view_set_left_margin| a b)))
(progn
 (defalien "gtk_text_view_get_left_margin" gint (a (* t)))
 (defun gtk::text-view-get-left-margin (a) (|gtk_text_view_get_left_margin| a)))
(progn
 (defalien "gtk_text_view_set_right_margin" void (a (* t)) (b gint))
 (defun gtk::text-view-set-right-margin (a b)
   (|gtk_text_view_set_right_margin| a b)))
(progn
 (defalien "gtk_text_view_get_right_margin" gint (a (* t)))
 (defun gtk::text-view-get-right-margin (a)
   (|gtk_text_view_get_right_margin| a)))
(progn
 (defalien "gtk_text_view_set_indent" void (a (* t)) (b gint))
 (defun gtk::text-view-set-indent (a b) (|gtk_text_view_set_indent| a b)))
(progn
 (defalien "gtk_text_view_get_indent" gint (a (* t)))
 (defun gtk::text-view-get-indent (a) (|gtk_text_view_get_indent| a)))
(progn
 (defalien "gtk_text_view_set_tabs" void (a (* t)) (b (* t)))
 (defun gtk::text-view-set-tabs (a b) (|gtk_text_view_set_tabs| a b)))
(progn
 (defalien "gtk_text_view_get_tabs" (* t) (a (* t)))
 (defun gtk::text-view-get-tabs (a) (|gtk_text_view_get_tabs| a)))
(progn
 (defalien "gtk_text_view_get_default_attributes" (* t) (a (* t)))
 (defun gtk::text-view-get-default-attributes (a)
   (|gtk_text_view_get_default_attributes| a)))
(progn
 (defalien "gtk_tips_query_get_type" GtkType)
 (defun gtk::tips-query-get-type () (|gtk_tips_query_get_type|)))
(progn
 (defalien "gtk_tooltips_get_type" GType)
 (defun gtk::tooltips-get-type () (|gtk_tooltips_get_type|)))
(progn
 (defalien "gtk_tooltips_new" (* t))
 (defun gtk::tooltips-new () (|gtk_tooltips_new|)))
(progn
 (defalien "gtk_tooltips_enable" void (a (* t)))
 (defun gtk::tooltips-enable (a) (|gtk_tooltips_enable| a)))
(progn
 (defalien "gtk_tooltips_disable" void (a (* t)))
 (defun gtk::tooltips-disable (a) (|gtk_tooltips_disable| a)))
(progn
 (defalien "gtk_tooltips_set_tip"
           void
           (a (* t))
           (b (* t))
           (c c-string)
           (d c-string))
 (defun gtk::tooltips-set-tip (a b c d) (|gtk_tooltips_set_tip| a b c d)))
(progn
 (defalien "gtk_tooltips_data_get" (* t) (a (* t)))
 (defun gtk::tooltips-data-get (a) (|gtk_tooltips_data_get| a)))
(progn
 (defalien "gtk_tooltips_force_window" void (a (* t)))
 (defun gtk::tooltips-force-window (a) (|gtk_tooltips_force_window| a)))
(progn
 (defalien "gtk_toolbar_get_type" GType)
 (defun gtk::toolbar-get-type () (|gtk_toolbar_get_type|)))
(progn
 (defalien "gtk_toolbar_new" (* t))
 (defun gtk::toolbar-new () (|gtk_toolbar_new|)))
(progn
 (defalien "gtk_toolbar_append_item"
           (* t)
           (a (* t))
           (b c-string)
           (c c-string)
           (d c-string)
           (e (* t))
           (f GtkSignalFunc)
           (g gpointer))
 (defun gtk::toolbar-append-item (a b c d e f g)
   (|gtk_toolbar_append_item| a b c d e f g)))
(progn
 (defalien "gtk_toolbar_prepend_item"
           (* t)
           (a (* t))
           (b c-string)
           (c c-string)
           (d c-string)
           (e (* t))
           (f GtkSignalFunc)
           (g gpointer))
 (defun gtk::toolbar-prepend-item (a b c d e f g)
   (|gtk_toolbar_prepend_item| a b c d e f g)))
(progn
 (defalien "gtk_toolbar_insert_item"
           (* t)
           (a (* t))
           (b c-string)
           (c c-string)
           (d c-string)
           (e (* t))
           (f GtkSignalFunc)
           (g gpointer)
           (h gint))
 (defun gtk::toolbar-insert-item (a b c d e f g h)
   (|gtk_toolbar_insert_item| a b c d e f g h)))
(progn
 (defalien "gtk_toolbar_insert_stock"
           (* t)
           (a (* t))
           (b c-string)
           (c c-string)
           (d c-string)
           (e GtkSignalFunc)
           (f gpointer)
           (g gint))
 (defun gtk::toolbar-insert-stock (a b c d e f g)
   (|gtk_toolbar_insert_stock| a b c d e f g)))
(progn
 (defalien "gtk_toolbar_append_space" void (a (* t)))
 (defun gtk::toolbar-append-space (a) (|gtk_toolbar_append_space| a)))
(progn
 (defalien "gtk_toolbar_prepend_space" void (a (* t)))
 (defun gtk::toolbar-prepend-space (a) (|gtk_toolbar_prepend_space| a)))
(progn
 (defalien "gtk_toolbar_insert_space" void (a (* t)) (b gint))
 (defun gtk::toolbar-insert-space (a b) (|gtk_toolbar_insert_space| a b)))
(progn
 (defalien "gtk_toolbar_remove_space" void (a (* t)) (b gint))
 (defun gtk::toolbar-remove-space (a b) (|gtk_toolbar_remove_space| a b)))
(progn
 (defalien "gtk_toolbar_append_element"
           (* t)
           (a (* t))
           (b GtkToolbarChildType)
           (c (* t))
           (d c-string)
           (e c-string)
           (f c-string)
           (g (* t))
           (h GtkSignalFunc)
           (i gpointer))
 (defun gtk::toolbar-append-element (a b c d e f g h i)
   (|gtk_toolbar_append_element| a b c d e f g h i)))
(progn
 (defalien "gtk_toolbar_prepend_element"
           (* t)
           (a (* t))
           (b GtkToolbarChildType)
           (c (* t))
           (d c-string)
           (e c-string)
           (f c-string)
           (g (* t))
           (h GtkSignalFunc)
           (i gpointer))
 (defun gtk::toolbar-prepend-element (a b c d e f g h i)
   (|gtk_toolbar_prepend_element| a b c d e f g h i)))
(progn
 (defalien "gtk_toolbar_insert_element"
           (* t)
           (a (* t))
           (b GtkToolbarChildType)
           (c (* t))
           (d c-string)
           (e c-string)
           (f c-string)
           (g (* t))
           (h GtkSignalFunc)
           (i gpointer)
           (j gint))
 (defun gtk::toolbar-insert-element (a b c d e f g h i j)
   (|gtk_toolbar_insert_element| a b c d e f g h i j)))
(progn
 (defalien "gtk_toolbar_append_widget"
           void
           (a (* t))
           (b (* t))
           (c c-string)
           (d c-string))
 (defun gtk::toolbar-append-widget (a b c d)
   (|gtk_toolbar_append_widget| a b c d)))
(progn
 (defalien "gtk_toolbar_prepend_widget"
           void
           (a (* t))
           (b (* t))
           (c c-string)
           (d c-string))
 (defun gtk::toolbar-prepend-widget (a b c d)
   (|gtk_toolbar_prepend_widget| a b c d)))
(progn
 (defalien "gtk_toolbar_insert_widget"
           void
           (a (* t))
           (b (* t))
           (c c-string)
           (d c-string)
           (e gint))
 (defun gtk::toolbar-insert-widget (a b c d e)
   (|gtk_toolbar_insert_widget| a b c d e)))
(progn
 (defalien "gtk_toolbar_set_orientation" void (a (* t)) (b GtkOrientation))
 (defun gtk::toolbar-set-orientation (a b) (|gtk_toolbar_set_orientation| a b)))
(progn
 (defalien "gtk_toolbar_set_style" void (a (* t)) (b GtkToolbarStyle))
 (defun gtk::toolbar-set-style (a b) (|gtk_toolbar_set_style| a b)))
(progn
 (defalien "gtk_toolbar_set_icon_size" void (a (* t)) (b GtkIconSize))
 (defun gtk::toolbar-set-icon-size (a b) (|gtk_toolbar_set_icon_size| a b)))
(progn
 (defalien "gtk_toolbar_set_tooltips" void (a (* t)) (b gboolean))
 (defun gtk::toolbar-set-tooltips (a b)
   (|gtk_toolbar_set_tooltips| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gtk_toolbar_unset_style" void (a (* t)))
 (defun gtk::toolbar-unset-style (a) (|gtk_toolbar_unset_style| a)))
(progn
 (defalien "gtk_toolbar_unset_icon_size" void (a (* t)))
 (defun gtk::toolbar-unset-icon-size (a) (|gtk_toolbar_unset_icon_size| a)))
(progn
 (defalien "gtk_toolbar_get_orientation" GtkOrientation (a (* t)))
 (defun gtk::toolbar-get-orientation (a) (|gtk_toolbar_get_orientation| a)))
(progn
 (defalien "gtk_toolbar_get_style" GtkToolbarStyle (a (* t)))
 (defun gtk::toolbar-get-style (a) (|gtk_toolbar_get_style| a)))
(progn
 (defalien "gtk_toolbar_get_icon_size" GtkIconSize (a (* t)))
 (defun gtk::toolbar-get-icon-size (a) (|gtk_toolbar_get_icon_size| a)))
(progn
 (defalien "gtk_toolbar_get_tooltips" gboolean (a (* t)))
 (defun gtk::toolbar-get-tooltips (a)
   (let ((v515 (|gtk_toolbar_get_tooltips| a)))
     (if (= v515 1) t nil))))
(progn
 (defalien "gtk_tree_drag_source_get_type" GType)
 (defun gtk::tree-drag-source-get-type () (|gtk_tree_drag_source_get_type|)))
(progn
 (defalien "gtk_tree_drag_source_row_draggable" gboolean (a (* t)) (b (* t)))
 (defun gtk::tree-drag-source-row-draggable (a b)
   (let ((v516 (|gtk_tree_drag_source_row_draggable| a b)))
     (if (= v516 1) t nil))))
(progn
 (defalien "gtk_tree_drag_source_drag_data_delete"
           gboolean
           (a (* t))
           (b (* t)))
 (defun gtk::tree-drag-source-drag-data-delete (a b)
   (let ((v517 (|gtk_tree_drag_source_drag_data_delete| a b)))
     (if (= v517 1) t nil))))
(progn
 (defalien "gtk_tree_drag_source_drag_data_get"
           gboolean
           (a (* t))
           (b (* t))
           (c (* t)))
 (defun gtk::tree-drag-source-drag-data-get (a b c)
   (let ((v518 (|gtk_tree_drag_source_drag_data_get| a b c)))
     (if (= v518 1) t nil))))
(progn
 (defalien "gtk_tree_drag_dest_get_type" GType)
 (defun gtk::tree-drag-dest-get-type () (|gtk_tree_drag_dest_get_type|)))
(progn
 (defalien "gtk_tree_drag_dest_drag_data_received"
           gboolean
           (a (* t))
           (b (* t))
           (c (* t)))
 (defun gtk::tree-drag-dest-drag-data-received (a b c)
   (let ((v519 (|gtk_tree_drag_dest_drag_data_received| a b c)))
     (if (= v519 1) t nil))))
(progn
 (defalien "gtk_tree_drag_dest_row_drop_possible"
           gboolean
           (a (* t))
           (b (* t))
           (c (* t)))
 (defun gtk::tree-drag-dest-row-drop-possible (a b c)
   (let ((v520 (|gtk_tree_drag_dest_row_drop_possible| a b c)))
     (if (= v520 1) t nil))))
(progn
 (defalien "gtk_tree_set_row_drag_data" gboolean (a (* t)) (b (* t)) (c (* t)))
 (defun gtk::tree-set-row-drag-data (a b c)
   (let ((v521 (|gtk_tree_set_row_drag_data| a b c)))
     (if (= v521 1) t nil))))
(progn
 (defalien "gtk_tree_get_row_drag_data" gboolean (a (* t)) (b (* t)) (c (* t)))
 (defun gtk::tree-get-row-drag-data (a b c)
   (let ((v522 (|gtk_tree_get_row_drag_data| a b c)))
     (if (= v522 1) t nil))))
(progn
 (defalien "gtk_tree_model_sort_get_type" GType)
 (defun gtk::tree-model-sort-get-type () (|gtk_tree_model_sort_get_type|)))
(progn
 (defalien "gtk_tree_model_sort_new_with_model" (* t) (a (* t)))
 (defun gtk::tree-model-sort-new-with-model (a)
   (|gtk_tree_model_sort_new_with_model| a)))
(progn
 (defalien "gtk_tree_model_sort_get_model" (* t) (a (* t)))
 (defun gtk::tree-model-sort-get-model (a) (|gtk_tree_model_sort_get_model| a)))
(progn
 (defalien "gtk_tree_model_sort_convert_child_path_to_path"
           (* t)
           (a (* t))
           (b (* t)))
 (defun gtk::tree-model-sort-convert-child-path-to-path (a b)
   (|gtk_tree_model_sort_convert_child_path_to_path| a b)))
(progn
 (defalien "gtk_tree_model_sort_convert_child_iter_to_iter"
           void
           (a (* t))
           (b (* t))
           (c (* t)))
 (defun gtk::tree-model-sort-convert-child-iter-to-iter (a b c)
   (|gtk_tree_model_sort_convert_child_iter_to_iter| a b c)))
(progn
 (defalien "gtk_tree_model_sort_convert_path_to_child_path"
           (* t)
           (a (* t))
           (b (* t)))
 (defun gtk::tree-model-sort-convert-path-to-child-path (a b)
   (|gtk_tree_model_sort_convert_path_to_child_path| a b)))
(progn
 (defalien "gtk_tree_model_sort_convert_iter_to_child_iter"
           void
           (a (* t))
           (b (* t))
           (c (* t)))
 (defun gtk::tree-model-sort-convert-iter-to-child-iter (a b c)
   (|gtk_tree_model_sort_convert_iter_to_child_iter| a b c)))
(progn
 (defalien "gtk_tree_model_sort_reset_default_sort_func" void (a (* t)))
 (defun gtk::tree-model-sort-reset-default-sort-func (a)
   (|gtk_tree_model_sort_reset_default_sort_func| a)))
(progn
 (defalien "gtk_tree_model_sort_clear_cache" void (a (* t)))
 (defun gtk::tree-model-sort-clear-cache (a)
   (|gtk_tree_model_sort_clear_cache| a)))
(progn
 (defalien "gtk_tree_view_column_get_type" GType)
 (defun gtk::tree-view-column-get-type () (|gtk_tree_view_column_get_type|)))
(progn
 (defalien "gtk_tree_view_column_new" (* t))
 (defun gtk::tree-view-column-new () (|gtk_tree_view_column_new|)))
(progn
 (defalien "gtk_tree_view_column_new_with_attributes"
           (* t)
           (a c-string)
           (b (* t)))
 (defun gtk::tree-view-column-new-with-attributes (a b)
   (|gtk_tree_view_column_new_with_attributes| a b)))
(progn
 (defalien "gtk_tree_view_column_pack_start"
           void
           (a (* t))
           (b (* t))
           (c gboolean))
 (defun gtk::tree-view-column-pack-start (a b c)
   (|gtk_tree_view_column_pack_start| a b (if c (if (eq c 0) 0 1) 0))))
(progn
 (defalien "gtk_tree_view_column_pack_end"
           void
           (a (* t))
           (b (* t))
           (c gboolean))
 (defun gtk::tree-view-column-pack-end (a b c)
   (|gtk_tree_view_column_pack_end| a b (if c (if (eq c 0) 0 1) 0))))
(progn
 (defalien "gtk_tree_view_column_clear" void (a (* t)))
 (defun gtk::tree-view-column-clear (a) (|gtk_tree_view_column_clear| a)))
(progn
 (defalien "gtk_tree_view_column_get_cell_renderers" (* t) (a (* t)))
 (defun gtk::tree-view-column-get-cell-renderers (a)
   (|gtk_tree_view_column_get_cell_renderers| a)))
(progn
 (defalien "gtk_tree_view_column_add_attribute"
           void
           (a (* t))
           (b (* t))
           (c c-string)
           (d gint))
 (defun gtk::tree-view-column-add-attribute (a b c d)
   (|gtk_tree_view_column_add_attribute| a b c d)))
(progn
 (defalien "gtk_tree_view_column_set_attributes" void (a (* t)) (b (* t)))
 (defun gtk::tree-view-column-set-attributes (a b)
   (|gtk_tree_view_column_set_attributes| a b)))
(progn
 (defalien "gtk_tree_view_column_set_cell_data_func"
           void
           (a (* t))
           (b (* t))
           (c GtkTreeCellDataFunc)
           (d gpointer)
           (e GtkDestroyNotify))
 (defun gtk::tree-view-column-set-cell-data-func (a b c d e)
   (|gtk_tree_view_column_set_cell_data_func| a b c d e)))
(progn
 (defalien "gtk_tree_view_column_clear_attributes" void (a (* t)) (b (* t)))
 (defun gtk::tree-view-column-clear-attributes (a b)
   (|gtk_tree_view_column_clear_attributes| a b)))
(progn
 (defalien "gtk_tree_view_column_set_spacing" void (a (* t)) (b gint))
 (defun gtk::tree-view-column-set-spacing (a b)
   (|gtk_tree_view_column_set_spacing| a b)))
(progn
 (defalien "gtk_tree_view_column_get_spacing" gint (a (* t)))
 (defun gtk::tree-view-column-get-spacing (a)
   (|gtk_tree_view_column_get_spacing| a)))
(progn
 (defalien "gtk_tree_view_column_set_visible" void (a (* t)) (b gboolean))
 (defun gtk::tree-view-column-set-visible (a b)
   (|gtk_tree_view_column_set_visible| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gtk_tree_view_column_get_visible" gboolean (a (* t)))
 (defun gtk::tree-view-column-get-visible (a)
   (let ((v523 (|gtk_tree_view_column_get_visible| a)))
     (if (= v523 1) t nil))))
(progn
 (defalien "gtk_tree_view_column_set_resizable" void (a (* t)) (b gboolean))
 (defun gtk::tree-view-column-set-resizable (a b)
   (|gtk_tree_view_column_set_resizable| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gtk_tree_view_column_get_resizable" gboolean (a (* t)))
 (defun gtk::tree-view-column-get-resizable (a)
   (let ((v524 (|gtk_tree_view_column_get_resizable| a)))
     (if (= v524 1) t nil))))
(progn
 (defalien "gtk_tree_view_column_set_sizing"
           void
           (a (* t))
           (b GtkTreeViewColumnSizing))
 (defun gtk::tree-view-column-set-sizing (a b)
   (|gtk_tree_view_column_set_sizing| a b)))
(progn
 (defalien "gtk_tree_view_column_get_sizing" GtkTreeViewColumnSizing (a (* t)))
 (defun gtk::tree-view-column-get-sizing (a)
   (|gtk_tree_view_column_get_sizing| a)))
(progn
 (defalien "gtk_tree_view_column_get_width" gint (a (* t)))
 (defun gtk::tree-view-column-get-width (a)
   (|gtk_tree_view_column_get_width| a)))
(progn
 (defalien "gtk_tree_view_column_get_fixed_width" gint (a (* t)))
 (defun gtk::tree-view-column-get-fixed-width (a)
   (|gtk_tree_view_column_get_fixed_width| a)))
(progn
 (defalien "gtk_tree_view_column_set_fixed_width" void (a (* t)) (b gint))
 (defun gtk::tree-view-column-set-fixed-width (a b)
   (|gtk_tree_view_column_set_fixed_width| a b)))
(progn
 (defalien "gtk_tree_view_column_set_min_width" void (a (* t)) (b gint))
 (defun gtk::tree-view-column-set-min-width (a b)
   (|gtk_tree_view_column_set_min_width| a b)))
(progn
 (defalien "gtk_tree_view_column_get_min_width" gint (a (* t)))
 (defun gtk::tree-view-column-get-min-width (a)
   (|gtk_tree_view_column_get_min_width| a)))
(progn
 (defalien "gtk_tree_view_column_set_max_width" void (a (* t)) (b gint))
 (defun gtk::tree-view-column-set-max-width (a b)
   (|gtk_tree_view_column_set_max_width| a b)))
(progn
 (defalien "gtk_tree_view_column_get_max_width" gint (a (* t)))
 (defun gtk::tree-view-column-get-max-width (a)
   (|gtk_tree_view_column_get_max_width| a)))
(progn
 (defalien "gtk_tree_view_column_clicked" void (a (* t)))
 (defun gtk::tree-view-column-clicked (a) (|gtk_tree_view_column_clicked| a)))
(progn
 (defalien "gtk_tree_view_column_set_title" void (a (* t)) (b c-string))
 (defun gtk::tree-view-column-set-title (a b)
   (|gtk_tree_view_column_set_title| a b)))
(progn
 (defalien "gtk_tree_view_column_get_title" c-string (a (* t)))
 (defun gtk::tree-view-column-get-title (a)
   (|gtk_tree_view_column_get_title| a)))
(progn
 (defalien "gtk_tree_view_column_set_clickable" void (a (* t)) (b gboolean))
 (defun gtk::tree-view-column-set-clickable (a b)
   (|gtk_tree_view_column_set_clickable| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gtk_tree_view_column_get_clickable" gboolean (a (* t)))
 (defun gtk::tree-view-column-get-clickable (a)
   (let ((v525 (|gtk_tree_view_column_get_clickable| a)))
     (if (= v525 1) t nil))))
(progn
 (defalien "gtk_tree_view_column_set_widget" void (a (* t)) (b (* t)))
 (defun gtk::tree-view-column-set-widget (a b)
   (|gtk_tree_view_column_set_widget| a b)))
(progn
 (defalien "gtk_tree_view_column_get_widget" (* t) (a (* t)))
 (defun gtk::tree-view-column-get-widget (a)
   (|gtk_tree_view_column_get_widget| a)))
(progn
 (defalien "gtk_tree_view_column_set_alignment" void (a (* t)) (b gfloat))
 (defun gtk::tree-view-column-set-alignment (a b)
   (|gtk_tree_view_column_set_alignment| a (coerce b 'single-float))))
(progn
 (defalien "gtk_tree_view_column_get_alignment" gfloat (a (* t)))
 (defun gtk::tree-view-column-get-alignment (a)
   (|gtk_tree_view_column_get_alignment| a)))
(progn
 (defalien "gtk_tree_view_column_set_reorderable" void (a (* t)) (b gboolean))
 (defun gtk::tree-view-column-set-reorderable (a b)
   (|gtk_tree_view_column_set_reorderable| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gtk_tree_view_column_get_reorderable" gboolean (a (* t)))
 (defun gtk::tree-view-column-get-reorderable (a)
   (let ((v526 (|gtk_tree_view_column_get_reorderable| a)))
     (if (= v526 1) t nil))))
(progn
 (defalien "gtk_tree_view_column_set_sort_column_id" void (a (* t)) (b gint))
 (defun gtk::tree-view-column-set-sort-column-id (a b)
   (|gtk_tree_view_column_set_sort_column_id| a b)))
(progn
 (defalien "gtk_tree_view_column_get_sort_column_id" gint (a (* t)))
 (defun gtk::tree-view-column-get-sort-column-id (a)
   (|gtk_tree_view_column_get_sort_column_id| a)))
(progn
 (defalien "gtk_tree_view_column_set_sort_indicator"
           void
           (a (* t))
           (b gboolean))
 (defun gtk::tree-view-column-set-sort-indicator (a b)
   (|gtk_tree_view_column_set_sort_indicator| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gtk_tree_view_column_get_sort_indicator" gboolean (a (* t)))
 (defun gtk::tree-view-column-get-sort-indicator (a)
   (let ((v527 (|gtk_tree_view_column_get_sort_indicator| a)))
     (if (= v527 1) t nil))))
(progn
 (defalien "gtk_tree_view_column_set_sort_order"
           void
           (a (* t))
           (b GtkSortType))
 (defun gtk::tree-view-column-set-sort-order (a b)
   (|gtk_tree_view_column_set_sort_order| a b)))
(progn
 (defalien "gtk_tree_view_column_get_sort_order" GtkSortType (a (* t)))
 (defun gtk::tree-view-column-get-sort-order (a)
   (|gtk_tree_view_column_get_sort_order| a)))
(progn
 (defalien "gtk_tree_view_column_cell_set_cell_data"
           void
           (a (* t))
           (b (* t))
           (c (* t))
           (d gboolean)
           (e gboolean))
 (defun gtk::tree-view-column-cell-set-cell-data (a b c d e)
   (|gtk_tree_view_column_cell_set_cell_data| a b c (if d (if (eq d 0) 0 1) 0)
    (if e (if (eq e 0) 0 1) 0))))
(progn
 (defalien "gtk_tree_view_column_cell_get_size"
           void
           (a (* t))
           (b (* t))
           (c gint :in-out)
           (d gint :in-out)
           (e gint :in-out)
           (f gint :in-out))
 (defun gtk::tree-view-column-cell-get-size (a b c d e f)
   (|gtk_tree_view_column_cell_get_size| a b c d e f)))
(progn
 (defalien "gtk_tree_view_column_cell_is_visible" gboolean (a (* t)))
 (defun gtk::tree-view-column-cell-is-visible (a)
   (let ((v528 (|gtk_tree_view_column_cell_is_visible| a)))
     (if (= v528 1) t nil))))
(progn
 (defalien "gtk_tree_view_get_type" GType)
 (defun gtk::tree-view-get-type () (|gtk_tree_view_get_type|)))
(progn
 (defalien "gtk_tree_view_new" (* t))
 (defun gtk::tree-view-new () (|gtk_tree_view_new|)))
(progn
 (defalien "gtk_tree_view_new_with_model" (* t) (a (* t)))
 (defun gtk::tree-view-new-with-model (a) (|gtk_tree_view_new_with_model| a)))
(progn
 (defalien "gtk_tree_view_get_model" (* t) (a (* t)))
 (defun gtk::tree-view-get-model (a) (|gtk_tree_view_get_model| a)))
(progn
 (defalien "gtk_tree_view_set_model" void (a (* t)) (b (* t)))
 (defun gtk::tree-view-set-model (a b) (|gtk_tree_view_set_model| a b)))
(progn
 (defalien "gtk_tree_view_get_selection" (* t) (a (* t)))
 (defun gtk::tree-view-get-selection (a) (|gtk_tree_view_get_selection| a)))
(progn
 (defalien "gtk_tree_view_get_hadjustment" (* t) (a (* t)))
 (defun gtk::tree-view-get-hadjustment (a) (|gtk_tree_view_get_hadjustment| a)))
(progn
 (defalien "gtk_tree_view_set_hadjustment" void (a (* t)) (b (* t)))
 (defun gtk::tree-view-set-hadjustment (a b)
   (|gtk_tree_view_set_hadjustment| a b)))
(progn
 (defalien "gtk_tree_view_get_vadjustment" (* t) (a (* t)))
 (defun gtk::tree-view-get-vadjustment (a) (|gtk_tree_view_get_vadjustment| a)))
(progn
 (defalien "gtk_tree_view_set_vadjustment" void (a (* t)) (b (* t)))
 (defun gtk::tree-view-set-vadjustment (a b)
   (|gtk_tree_view_set_vadjustment| a b)))
(progn
 (defalien "gtk_tree_view_get_headers_visible" gboolean (a (* t)))
 (defun gtk::tree-view-get-headers-visible (a)
   (let ((v529 (|gtk_tree_view_get_headers_visible| a)))
     (if (= v529 1) t nil))))
(progn
 (defalien "gtk_tree_view_set_headers_visible" void (a (* t)) (b gboolean))
 (defun gtk::tree-view-set-headers-visible (a b)
   (|gtk_tree_view_set_headers_visible| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gtk_tree_view_columns_autosize" void (a (* t)))
 (defun gtk::tree-view-columns-autosize (a)
   (|gtk_tree_view_columns_autosize| a)))
(progn
 (defalien "gtk_tree_view_set_headers_clickable" void (a (* t)) (b gboolean))
 (defun gtk::tree-view-set-headers-clickable (a b)
   (|gtk_tree_view_set_headers_clickable| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gtk_tree_view_set_rules_hint" void (a (* t)) (b gboolean))
 (defun gtk::tree-view-set-rules-hint (a b)
   (|gtk_tree_view_set_rules_hint| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gtk_tree_view_get_rules_hint" gboolean (a (* t)))
 (defun gtk::tree-view-get-rules-hint (a)
   (let ((v530 (|gtk_tree_view_get_rules_hint| a)))
     (if (= v530 1) t nil))))
(progn
 (defalien "gtk_tree_view_append_column" gint (a (* t)) (b (* t)))
 (defun gtk::tree-view-append-column (a b) (|gtk_tree_view_append_column| a b)))
(progn
 (defalien "gtk_tree_view_remove_column" gint (a (* t)) (b (* t)))
 (defun gtk::tree-view-remove-column (a b) (|gtk_tree_view_remove_column| a b)))
(progn
 (defalien "gtk_tree_view_insert_column" gint (a (* t)) (b (* t)) (c gint))
 (defun gtk::tree-view-insert-column (a b c)
   (|gtk_tree_view_insert_column| a b c)))
(progn
 (defalien "gtk_tree_view_insert_column_with_attributes"
           gint
           (a (* t))
           (b gint)
           (c c-string)
           (d (* t)))
 (defun gtk::tree-view-insert-column-with-attributes (a b c d)
   (|gtk_tree_view_insert_column_with_attributes| a b c d)))
(progn
 (defalien "gtk_tree_view_insert_column_with_data_func"
           gint
           (a (* t))
           (b gint)
           (c c-string)
           (d (* t))
           (e GtkTreeCellDataFunc)
           (f gpointer)
           (g GDestroyNotify))
 (defun gtk::tree-view-insert-column-with-data-func (a b c d e f g)
   (|gtk_tree_view_insert_column_with_data_func| a b c d e f g)))
(progn
 (defalien "gtk_tree_view_get_column" (* t) (a (* t)) (b gint))
 (defun gtk::tree-view-get-column (a b) (|gtk_tree_view_get_column| a b)))
(progn
 (defalien "gtk_tree_view_get_columns" (* t) (a (* t)))
 (defun gtk::tree-view-get-columns (a) (|gtk_tree_view_get_columns| a)))
(progn
 (defalien "gtk_tree_view_move_column_after"
           void
           (a (* t))
           (b (* t))
           (c (* t)))
 (defun gtk::tree-view-move-column-after (a b c)
   (|gtk_tree_view_move_column_after| a b c)))
(progn
 (defalien "gtk_tree_view_set_expander_column" void (a (* t)) (b (* t)))
 (defun gtk::tree-view-set-expander-column (a b)
   (|gtk_tree_view_set_expander_column| a b)))
(progn
 (defalien "gtk_tree_view_get_expander_column" (* t) (a (* t)))
 (defun gtk::tree-view-get-expander-column (a)
   (|gtk_tree_view_get_expander_column| a)))
(progn
 (defalien "gtk_tree_view_set_column_drag_function"
           void
           (a (* t))
           (b GtkTreeViewColumnDropFunc)
           (c gpointer)
           (d GtkDestroyNotify))
 (defun gtk::tree-view-set-column-drag-function (a b c d)
   (|gtk_tree_view_set_column_drag_function| a b c d)))
(progn
 (defalien "gtk_tree_view_scroll_to_point" void (a (* t)) (b gint) (c gint))
 (defun gtk::tree-view-scroll-to-point (a b c)
   (|gtk_tree_view_scroll_to_point| a b c)))
(progn
 (defalien "gtk_tree_view_scroll_to_cell"
           void
           (a (* t))
           (b (* t))
           (c (* t))
           (d gboolean)
           (e gfloat)
           (f gfloat))
 (defun gtk::tree-view-scroll-to-cell (a b c d e f)
   (|gtk_tree_view_scroll_to_cell| a b c (if d (if (eq d 0) 0 1) 0)
    (coerce e 'single-float) (coerce f 'single-float))))
(progn
 (defalien "gtk_tree_view_row_activated" void (a (* t)) (b (* t)) (c (* t)))
 (defun gtk::tree-view-row-activated (a b c)
   (|gtk_tree_view_row_activated| a b c)))
(progn
 (defalien "gtk_tree_view_expand_all" void (a (* t)))
 (defun gtk::tree-view-expand-all (a) (|gtk_tree_view_expand_all| a)))
(progn
 (defalien "gtk_tree_view_collapse_all" void (a (* t)))
 (defun gtk::tree-view-collapse-all (a) (|gtk_tree_view_collapse_all| a)))
(progn
 (defalien "gtk_tree_view_expand_row"
           gboolean
           (a (* t))
           (b (* t))
           (c gboolean))
 (defun gtk::tree-view-expand-row (a b c)
   (let ((v531 (|gtk_tree_view_expand_row| a b (if c (if (eq c 0) 0 1) 0))))
     (if (= v531 1) t nil))))
(progn
 (defalien "gtk_tree_view_collapse_row" gboolean (a (* t)) (b (* t)))
 (defun gtk::tree-view-collapse-row (a b)
   (let ((v532 (|gtk_tree_view_collapse_row| a b)))
     (if (= v532 1) t nil))))
(progn
 (defalien "gtk_tree_view_map_expanded_rows"
           void
           (a (* t))
           (b GtkTreeViewMappingFunc)
           (c gpointer))
 (defun gtk::tree-view-map-expanded-rows (a b c)
   (|gtk_tree_view_map_expanded_rows| a b c)))
(progn
 (defalien "gtk_tree_view_row_expanded" gboolean (a (* t)) (b (* t)))
 (defun gtk::tree-view-row-expanded (a b)
   (let ((v533 (|gtk_tree_view_row_expanded| a b)))
     (if (= v533 1) t nil))))
(progn
 (defalien "gtk_tree_view_set_reorderable" void (a (* t)) (b gboolean))
 (defun gtk::tree-view-set-reorderable (a b)
   (|gtk_tree_view_set_reorderable| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gtk_tree_view_get_reorderable" gboolean (a (* t)))
 (defun gtk::tree-view-get-reorderable (a)
   (let ((v534 (|gtk_tree_view_get_reorderable| a)))
     (if (= v534 1) t nil))))
(progn
 (defalien "gtk_tree_view_set_cursor"
           void
           (a (* t))
           (b (* t))
           (c (* t))
           (d gboolean))
 (defun gtk::tree-view-set-cursor (a b c d)
   (|gtk_tree_view_set_cursor| a b c (if d (if (eq d 0) 0 1) 0))))
(progn
 (defalien "gtk_tree_view_get_cursor" void (a (* t)) (b (* t)) (c (* t)))
 (defun gtk::tree-view-get-cursor (a b c) (|gtk_tree_view_get_cursor| a b c)))
(progn
 (defalien "gtk_tree_view_get_bin_window" (* t) (a (* t)))
 (defun gtk::tree-view-get-bin-window (a) (|gtk_tree_view_get_bin_window| a)))
(progn
 (defalien "gtk_tree_view_get_path_at_pos"
           gboolean
           (a (* t))
           (b gint)
           (c gint)
           (d (* t))
           (e (* t))
           (f gint :in-out)
           (g gint :in-out))
 (defun gtk::tree-view-get-path-at-pos (a b c d e f g)
   (let ((v535
          (multiple-value-list
           (|gtk_tree_view_get_path_at_pos| a b c d e f g))))
     (apply #'values (if (= 1 (car v535)) t nil) (cdr v535)))))
(progn
 (defalien "gtk_tree_view_get_cell_area"
           void
           (a (* t))
           (b (* t))
           (c (* t))
           (d (* t)))
 (defun gtk::tree-view-get-cell-area (a b c d)
   (|gtk_tree_view_get_cell_area| a b c d)))
(progn
 (defalien "gtk_tree_view_get_background_area"
           void
           (a (* t))
           (b (* t))
           (c (* t))
           (d (* t)))
 (defun gtk::tree-view-get-background-area (a b c d)
   (|gtk_tree_view_get_background_area| a b c d)))
(progn
 (defalien "gtk_tree_view_get_visible_rect" void (a (* t)) (b (* t)))
 (defun gtk::tree-view-get-visible-rect (a b)
   (|gtk_tree_view_get_visible_rect| a b)))
(progn
 (defalien "gtk_tree_view_widget_to_tree_coords"
           void
           (a (* t))
           (b gint)
           (c gint)
           (d gint :in-out)
           (e gint :in-out))
 (defun gtk::tree-view-widget-to-tree-coords (a b c d e)
   (|gtk_tree_view_widget_to_tree_coords| a b c d e)))
(progn
 (defalien "gtk_tree_view_tree_to_widget_coords"
           void
           (a (* t))
           (b gint)
           (c gint)
           (d gint :in-out)
           (e gint :in-out))
 (defun gtk::tree-view-tree-to-widget-coords (a b c d e)
   (|gtk_tree_view_tree_to_widget_coords| a b c d e)))
(progn
 (defalien "gtk_tree_view_enable_model_drag_source"
           void
           (a (* t))
           (b GdkModifierType)
           (c (* t))
           (d gint)
           (e GdkDragAction))
 (defun gtk::tree-view-enable-model-drag-source (a b c d e)
   (|gtk_tree_view_enable_model_drag_source| a b c d e)))
(progn
 (defalien "gtk_tree_view_enable_model_drag_dest"
           void
           (a (* t))
           (b (* t))
           (c gint)
           (d GdkDragAction))
 (defun gtk::tree-view-enable-model-drag-dest (a b c d)
   (|gtk_tree_view_enable_model_drag_dest| a b c d)))
(progn
 (defalien "gtk_tree_view_unset_rows_drag_source" void (a (* t)))
 (defun gtk::tree-view-unset-rows-drag-source (a)
   (|gtk_tree_view_unset_rows_drag_source| a)))
(progn
 (defalien "gtk_tree_view_unset_rows_drag_dest" void (a (* t)))
 (defun gtk::tree-view-unset-rows-drag-dest (a)
   (|gtk_tree_view_unset_rows_drag_dest| a)))
(progn
 (defalien "gtk_tree_view_set_drag_dest_row"
           void
           (a (* t))
           (b (* t))
           (c GtkTreeViewDropPosition))
 (defun gtk::tree-view-set-drag-dest-row (a b c)
   (|gtk_tree_view_set_drag_dest_row| a b c)))
(progn
 (defalien "gtk_tree_view_get_drag_dest_row"
           void
           (a (* t))
           (b (* t))
           (c GtkTreeViewDropPosition :in-out))
 (defun gtk::tree-view-get-drag-dest-row (a b c)
   (|gtk_tree_view_get_drag_dest_row| a b c)))
(progn
 (defalien "gtk_tree_view_get_dest_row_at_pos"
           gboolean
           (a (* t))
           (b gint)
           (c gint)
           (d (* t))
           (e GtkTreeViewDropPosition :in-out))
 (defun gtk::tree-view-get-dest-row-at-pos (a b c d e)
   (let ((v536
          (multiple-value-list
           (|gtk_tree_view_get_dest_row_at_pos| a b c d e))))
     (apply #'values (if (= 1 (car v536)) t nil) (cdr v536)))))
(progn
 (defalien "gtk_tree_view_create_row_drag_icon" (* t) (a (* t)) (b (* t)))
 (defun gtk::tree-view-create-row-drag-icon (a b)
   (|gtk_tree_view_create_row_drag_icon| a b)))
(progn
 (defalien "gtk_tree_view_set_enable_search" void (a (* t)) (b gboolean))
 (defun gtk::tree-view-set-enable-search (a b)
   (|gtk_tree_view_set_enable_search| a (if b (if (eq b 0) 0 1) 0))))
(progn
 (defalien "gtk_tree_view_get_enable_search" gboolean (a (* t)))
 (defun gtk::tree-view-get-enable-search (a)
   (let ((v537 (|gtk_tree_view_get_enable_search| a)))
     (if (= v537 1) t nil))))
(progn
 (defalien "gtk_tree_view_get_search_column" gint (a (* t)))
 (defun gtk::tree-view-get-search-column (a)
   (|gtk_tree_view_get_search_column| a)))
(progn
 (defalien "gtk_tree_view_set_search_column" void (a (* t)) (b gint))
 (defun gtk::tree-view-set-search-column (a b)
   (|gtk_tree_view_set_search_column| a b)))
(progn
 (defalien "gtk_tree_view_get_search_equal_func"
           GtkTreeViewSearchEqualFunc
           (a (* t)))
 (defun gtk::tree-view-get-search-equal-func (a)
   (|gtk_tree_view_get_search_equal_func| a)))
(progn
 (defalien "gtk_tree_view_set_search_equal_func"
           void
           (a (* t))
           (b GtkTreeViewSearchEqualFunc)
           (c gpointer)
           (d GtkDestroyNotify))
 (defun gtk::tree-view-set-search-equal-func (a b c d)
   (|gtk_tree_view_set_search_equal_func| a b c d)))
(progn
 (defalien "gtk_tree_view_set_destroy_count_func"
           void
           (a (* t))
           (b GtkTreeDestroyCountFunc)
           (c gpointer)
           (d GtkDestroyNotify))
 (defun gtk::tree-view-set-destroy-count-func (a b c d)
   (|gtk_tree_view_set_destroy_count_func| a b c d)))
(progn
 (defalien "gtk_tree_selection_get_type" GType)
 (defun gtk::tree-selection-get-type () (|gtk_tree_selection_get_type|)))
(progn
 (defalien "gtk_tree_selection_set_mode" void (a (* t)) (b GtkSelectionMode))
 (defun gtk::tree-selection-set-mode (a b) (|gtk_tree_selection_set_mode| a b)))
(progn
 (defalien "gtk_tree_selection_get_mode" GtkSelectionMode (a (* t)))
 (defun gtk::tree-selection-get-mode (a) (|gtk_tree_selection_get_mode| a)))
(progn
 (defalien "gtk_tree_selection_set_select_function"
           void
           (a (* t))
           (b GtkTreeSelectionFunc)
           (c gpointer)
           (d GtkDestroyNotify))
 (defun gtk::tree-selection-set-select-function (a b c d)
   (|gtk_tree_selection_set_select_function| a b c d)))
(progn
 (defalien "gtk_tree_selection_get_user_data" gpointer (a (* t)))
 (defun gtk::tree-selection-get-user-data (a)
   (|gtk_tree_selection_get_user_data| a)))
(progn
 (defalien "gtk_tree_selection_get_tree_view" (* t) (a (* t)))
 (defun gtk::tree-selection-get-tree-view (a)
   (|gtk_tree_selection_get_tree_view| a)))
(progn
 (defalien "gtk_tree_selection_get_selected"
           gboolean
           (a (* t))
           (b (* t))
           (c (* t)))
 (defun gtk::tree-selection-get-selected (a b c)
   (let ((v538 (|gtk_tree_selection_get_selected| a b c)))
     (if (= v538 1) t nil))))
(progn
 (defalien "gtk_tree_selection_selected_foreach"
           void
           (a (* t))
           (b GtkTreeSelectionForeachFunc)
           (c gpointer))
 (defun gtk::tree-selection-selected-foreach (a b c)
   (|gtk_tree_selection_selected_foreach| a b c)))
(progn
 (defalien "gtk_tree_selection_select_path" void (a (* t)) (b (* t)))
 (defun gtk::tree-selection-select-path (a b)
   (|gtk_tree_selection_select_path| a b)))
(progn
 (defalien "gtk_tree_selection_unselect_path" void (a (* t)) (b (* t)))
 (defun gtk::tree-selection-unselect-path (a b)
   (|gtk_tree_selection_unselect_path| a b)))
(progn
 (defalien "gtk_tree_selection_select_iter" void (a (* t)) (b (* t)))
 (defun gtk::tree-selection-select-iter (a b)
   (|gtk_tree_selection_select_iter| a b)))
(progn
 (defalien "gtk_tree_selection_unselect_iter" void (a (* t)) (b (* t)))
 (defun gtk::tree-selection-unselect-iter (a b)
   (|gtk_tree_selection_unselect_iter| a b)))
(progn
 (defalien "gtk_tree_selection_path_is_selected" gboolean (a (* t)) (b (* t)))
 (defun gtk::tree-selection-path-is-selected (a b)
   (let ((v539 (|gtk_tree_selection_path_is_selected| a b)))
     (if (= v539 1) t nil))))
(progn
 (defalien "gtk_tree_selection_iter_is_selected" gboolean (a (* t)) (b (* t)))
 (defun gtk::tree-selection-iter-is-selected (a b)
   (let ((v540 (|gtk_tree_selection_iter_is_selected| a b)))
     (if (= v540 1) t nil))))
(progn
 (defalien "gtk_tree_selection_select_all" void (a (* t)))
 (defun gtk::tree-selection-select-all (a) (|gtk_tree_selection_select_all| a)))
(progn
 (defalien "gtk_tree_selection_unselect_all" void (a (* t)))
 (defun gtk::tree-selection-unselect-all (a)
   (|gtk_tree_selection_unselect_all| a)))
(progn
 (defalien "gtk_tree_selection_select_range"
           void
           (a (* t))
           (b (* t))
           (c (* t)))
 (defun gtk::tree-selection-select-range (a b c)
   (|gtk_tree_selection_select_range| a b c)))
(progn
 (defalien "gtk_tree_store_get_type" GType)
 (defun gtk::tree-store-get-type () (|gtk_tree_store_get_type|)))
(progn
 (defalien "gtk_tree_store_new" (* t) (a gint))
 (defun gtk::tree-store-new (a) (|gtk_tree_store_new| a)))
(progn
 (defalien "gtk_tree_store_newv" (* t) (a gint) (b (* t)))
 (defun gtk::tree-store-newv (a b) (|gtk_tree_store_newv| a b)))
(progn
 (defalien "gtk_tree_store_set_column_types" void (a (* t)) (b gint) (c (* t)))
 (defun gtk::tree-store-set-column-types (a b c)
   (|gtk_tree_store_set_column_types| a b c)))
(progn
 (defalien "gtk_tree_store_set_value"
           void
           (a (* t))
           (b (* t))
           (c gint)
           (d (* t)))
 (defun gtk::tree-store-set-value (a b c d)
   (|gtk_tree_store_set_value| a b c d)))
(progn
 (defalien "gtk_tree_store_set" void (a (* t)) (b (* t)))
 (defun gtk::tree-store-set (a b) (|gtk_tree_store_set| a b)))
(progn
 (defalien "gtk_tree_store_set_valist" void (a (* t)) (b (* t)) (c va_list))
 (defun gtk::tree-store-set-valist (a b c) (|gtk_tree_store_set_valist| a b c)))
(progn
 (defalien "gtk_tree_store_remove" gboolean (a (* t)) (b (* t)))
 (defun gtk::tree-store-remove (a b)
   (let ((v541 (|gtk_tree_store_remove| a b)))
     (if (= v541 1) t nil))))
(progn
 (defalien "gtk_tree_store_insert" void (a (* t)) (b (* t)) (c (* t)) (d gint))
 (defun gtk::tree-store-insert (a b c d) (|gtk_tree_store_insert| a b c d)))
(progn
 (defalien "gtk_tree_store_insert_before"
           void
           (a (* t))
           (b (* t))
           (c (* t))
           (d (* t)))
 (defun gtk::tree-store-insert-before (a b c d)
   (|gtk_tree_store_insert_before| a b c d)))
(progn
 (defalien "gtk_tree_store_insert_after"
           void
           (a (* t))
           (b (* t))
           (c (* t))
           (d (* t)))
 (defun gtk::tree-store-insert-after (a b c d)
   (|gtk_tree_store_insert_after| a b c d)))
(progn
 (defalien "gtk_tree_store_prepend" void (a (* t)) (b (* t)) (c (* t)))
 (defun gtk::tree-store-prepend (a b c) (|gtk_tree_store_prepend| a b c)))
(progn
 (defalien "gtk_tree_store_append" void (a (* t)) (b (* t)) (c (* t)))
 (defun gtk::tree-store-append (a b c) (|gtk_tree_store_append| a b c)))
(progn
 (defalien "gtk_tree_store_is_ancestor" gboolean (a (* t)) (b (* t)) (c (* t)))
 (defun gtk::tree-store-is-ancestor (a b c)
   (let ((v542 (|gtk_tree_store_is_ancestor| a b c)))
     (if (= v542 1) t nil))))
(progn
 (defalien "gtk_tree_store_iter_depth" gint (a (* t)) (b (* t)))
 (defun gtk::tree-store-iter-depth (a b) (|gtk_tree_store_iter_depth| a b)))
(progn
 (defalien "gtk_tree_store_clear" void (a (* t)))
 (defun gtk::tree-store-clear (a) (|gtk_tree_store_clear| a)))
(progn
 (defalien "gtk_vbutton_box_get_type" GType)
 (defun gtk::vbutton-box-get-type () (|gtk_vbutton_box_get_type|)))
(progn
 (defalien "gtk_vbutton_box_new" (* t))
 (defun gtk::vbutton-box-new () (|gtk_vbutton_box_new|)))
(progn
 (defalien "gtk_vpaned_get_type" GType)
 (defun gtk::vpaned-get-type () (|gtk_vpaned_get_type|)))
(progn
 (defalien "gtk_vpaned_new" (* t))
 (defun gtk::vpaned-new () (|gtk_vpaned_new|)))
(progn
 (defalien "gtk_vruler_get_type" GType)
 (defun gtk::vruler-get-type () (|gtk_vruler_get_type|)))
(progn
 (defalien "gtk_vruler_new" (* t))
 (defun gtk::vruler-new () (|gtk_vruler_new|)))
(progn
 (defalien "gtk_vscale_get_type" GType)
 (defun gtk::vscale-get-type () (|gtk_vscale_get_type|)))
(progn
 (defalien "gtk_vscale_new" (* t) (a (* t)))
 (defun gtk::vscale-new (a) (|gtk_vscale_new| a)))
(progn
 (defalien "gtk_vscale_new_with_range"
           (* t)
           (a gdouble)
           (b gdouble)
           (c gdouble))
 (defun gtk::vscale-new-with-range (a b c)
   (|gtk_vscale_new_with_range| (coerce a 'double-float)
    (coerce b 'double-float) (coerce c 'double-float))))
(progn
 (defalien "gtk_vseparator_get_type" GType)
 (defun gtk::vseparator-get-type () (|gtk_vseparator_get_type|)))
(progn
 (defalien "gtk_vseparator_new" (* t))
 (defun gtk::vseparator-new () (|gtk_vseparator_new|)))

#+:sbcl
(declaim (sb-ext:unmuffle-conditions style-warning 
                                     sb-ext:compiler-note))

(export
 '(g::nullptr g::nullptr? g::callback g::signal-connect g::signal-connect-after
   g::signal-connect-swapped g::List.data g::List.next g::List.prev
   g::list-alloc g::list-free g::list-append g::list-prepend g::list-insert
   g::list-insert-sorted g::list-insert-before g::list-concat g::list-remove
   g::list-remove-all g::list-reverse g::list-copy g::list-nth g::list-find
   g::list-position g::signal-emit-by-name g::signal-stop-emission-by-name
   g::signal-connect-data g::signal-handler-disconnect g::object-set-property
   g::object-get-property g::object-freeze-notify g::object-notify
   g::object-thaw-notify g::object-unref g::object-get-data g::object-set-data)
 :g)
(export
 '(gdk::lsb-first gdk::msb-first gdk::shift-mask gdk::lock-mask
   gdk::control-mask gdk::mod1-mask gdk::mod2-mask gdk::mod3-mask
   gdk::mod4-mask gdk::mod5-mask gdk::button1-mask gdk::button2-mask
   gdk::button3-mask gdk::button4-mask gdk::button5-mask gdk::release-mask
   gdk::modifier-mask gdk::input-read gdk::input-write gdk::input-exception
   gdk::grab-success gdk::grab-already-grabbed gdk::grab-invalid-time
   gdk::grab-not-viewable gdk::grab-frozen gdk::x-cursor gdk::arrow
   gdk::based-arrow-down gdk::based-arrow-up gdk::boat gdk::bogosity
   gdk::bottom-left-corner gdk::bottom-right-corner gdk::bottom-side
   gdk::bottom-tee gdk::box-spiral gdk::center-ptr gdk::circle gdk::clock
   gdk::coffee-mug gdk::cross gdk::cross-reverse gdk::crosshair
   gdk::diamond-cross gdk::dot gdk::dotbox gdk::double-arrow gdk::draft-large
   gdk::draft-small gdk::draped-box gdk::exchange gdk::fleur gdk::gobbler
   gdk::gumby gdk::hand1 gdk::hand2 gdk::heart gdk::icon gdk::iron-cross
   gdk::left-ptr gdk::left-side gdk::left-tee gdk::leftbutton gdk::ll-angle
   gdk::lr-angle gdk::man gdk::middlebutton gdk::mouse gdk::pencil gdk::pirate
   gdk::plus gdk::question-arrow gdk::right-ptr gdk::right-side gdk::right-tee
   gdk::rightbutton gdk::rtl-logo gdk::sailboat gdk::sb-down-arrow
   gdk::sb-h-double-arrow gdk::sb-left-arrow gdk::sb-right-arrow
   gdk::sb-up-arrow gdk::sb-v-double-arrow gdk::shuttle gdk::sizing gdk::spider
   gdk::spraycan gdk::star gdk::target gdk::tcross gdk::top-left-arrow
   gdk::top-left-corner gdk::top-right-corner gdk::top-side gdk::top-tee
   gdk::trek gdk::ul-angle gdk::umbrella gdk::ur-angle gdk::watch gdk::xterm
   gdk::last-cursor gdk::cursor-is-pixmap gdk::action-default gdk::action-copy
   gdk::action-move gdk::action-link gdk::action-private gdk::action-ask
   gdk::drag-proto-motif gdk::drag-proto-xdnd gdk::drag-proto-rootwin
   gdk::drag-proto-none gdk::drag-proto-win32-dropfiles gdk::drag-proto-ole2
   gdk::drag-proto-local gdk::extension-events-none gdk::extension-events-all
   gdk::extension-events-cursor gdk::source-mouse gdk::source-pen
   gdk::source-eraser gdk::source-cursor gdk::mode-disabled gdk::mode-screen
   gdk::mode-window gdk::axis-ignore gdk::axis-x gdk::axis-y gdk::axis-pressure
   gdk::axis-xtilt gdk::axis-ytilt gdk::axis-wheel gdk::axis-last gdk::nothing
   gdk::delete gdk::destroy gdk::expose gdk::motion-notify gdk::button-press
   gdk::2button-press gdk::3button-press gdk::button-release gdk::key-press
   gdk::key-release gdk::enter-notify gdk::leave-notify gdk::focus-change
   gdk::configure gdk::map gdk::unmap gdk::property-notify gdk::selection-clear
   gdk::selection-request gdk::selection-notify gdk::proximity-in
   gdk::proximity-out gdk::drag-enter gdk::drag-leave gdk::drag-motion
   gdk::drag-status gdk::drop-start gdk::drop-finished gdk::client-event
   gdk::visibility-notify gdk::no-expose gdk::scroll gdk::window-state
   gdk::setting gdk::exposure-mask gdk::pointer-motion-mask
   gdk::pointer-motion-hint-mask gdk::button-motion-mask
   gdk::button1-motion-mask gdk::button2-motion-mask gdk::button3-motion-mask
   gdk::button-press-mask gdk::button-release-mask gdk::key-press-mask
   gdk::key-release-mask gdk::enter-notify-mask gdk::leave-notify-mask
   gdk::focus-change-mask gdk::structure-mask gdk::property-change-mask
   gdk::visibility-notify-mask gdk::proximity-in-mask gdk::proximity-out-mask
   gdk::substructure-mask gdk::scroll-mask gdk::all-events-mask
   gdk::window-state-withdrawn gdk::window-state-iconified
   gdk::window-state-maximized gdk::window-state-sticky
   gdk::window-state-fullscreen gdk::cap-not-last gdk::cap-butt gdk::cap-round
   gdk::cap-projecting gdk::solid gdk::tiled gdk::stippled gdk::opaque-stippled
   gdk::copy gdk::invert gdk::xor gdk::clear gdk::and gdk::and-reverse
   gdk::and-invert gdk::noop gdk::or gdk::equiv gdk::or-reverse
   gdk::copy-invert gdk::or-invert gdk::nand gdk::nor gdk::set gdk::join-miter
   gdk::join-round gdk::join-bevel gdk::line-solid gdk::line-on-off-dash
   gdk::line-double-dash gdk::clip-by-children gdk::include-inferiors
   gdk::gc-foreground gdk::gc-background gdk::gc-font gdk::gc-function
   gdk::gc-fill gdk::gc-tile gdk::gc-stipple gdk::gc-clip-mask
   gdk::gc-subwindow gdk::gc-ts-x-origin gdk::gc-ts-y-origin
   gdk::gc-clip-x-origin gdk::gc-clip-y-origin gdk::gc-exposures
   gdk::gc-line-width gdk::gc-line-style gdk::gc-cap-style gdk::gc-join-style
   gdk::rgb-dither-none gdk::rgb-dither-normal gdk::rgb-dither-max
   gdk::pixbuf-alpha-bilevel gdk::pixbuf-alpha-full gdk::colorspace-rgb
   gdk::interp-nearest gdk::interp-tiles gdk::interp-bilinear gdk::interp-hyper
   gdk::font-font gdk::font-fontset gdk::image-normal gdk::image-shared
   gdk::image-fastest gdk::prop-mode-replace gdk::prop-mode-prepend
   gdk::prop-mode-append gdk::visual-static-gray gdk::visual-grayscale
   gdk::visual-static-color gdk::visual-pseudo-color gdk::visual-true-color
   gdk::visual-direct-color gdk::window-root gdk::window-toplevel
   gdk::window-child gdk::window-dialog gdk::window-temp gdk::window-foreign
   gdk::hint-pos gdk::hint-min-size gdk::hint-max-size gdk::hint-base-size
   gdk::hint-aspect gdk::hint-resize-inc gdk::hint-win-gravity
   gdk::hint-user-pos gdk::hint-user-size gdk::window-type-hint-normal
   gdk::window-type-hint-dialog gdk::window-type-hint-menu
   gdk::window-type-hint-toolbar gdk::window-type-hint-splashscreen
   gdk::window-type-hint-utility gdk::window-type-hint-dock
   gdk::window-type-hint-desktop gdk::decor-all gdk::decor-border
   gdk::decor-resizeh gdk::decor-title gdk::decor-menu gdk::decor-minimize
   gdk::decor-maximize gdk::func-all gdk::func-resize gdk::func-move
   gdk::func-minimize gdk::func-maximize gdk::func-close
   gdk::gravity-north-west gdk::gravity-north gdk::gravity-north-east
   gdk::gravity-west gdk::gravity-center gdk::gravity-east
   gdk::gravity-south-west gdk::gravity-south gdk::gravity-south-east
   gdk::gravity-static gdk::window-edge-north-west gdk::window-edge-north
   gdk::window-edge-north-east gdk::window-edge-west gdk::window-edge-east
   gdk::window-edge-south-west gdk::window-edge-south
   gdk::window-edge-south-east gdk::Rectangle.x gdk::Rectangle.y
   gdk::Rectangle.width gdk::Rectangle.height gdk::Color.pixel gdk::Color.red
   gdk::Color.green gdk::Color.blue gdk::EventMotion.type
   gdk::EventMotion.window gdk::EventMotion.send-event gdk::EventMotion.time
   gdk::EventMotion.x gdk::EventMotion.y gdk::EventMotion.state
   gdk::EventMotion.is-hint gdk::EventMotion.device gdk::EventMotion.x-root
   gdk::EventMotion.y-root gdk::EventButton.type gdk::EventButton.window
   gdk::EventButton.send-event gdk::EventButton.time gdk::EventButton.x
   gdk::EventButton.y gdk::EventButton.state gdk::EventButton.button
   gdk::EventButton.device gdk::EventButton.x-root gdk::EventButton.y-root
   gdk::EventKey.type gdk::EventKey.window gdk::EventKey.send-event
   gdk::EventKey.time gdk::EventKey.state gdk::EventKey.keyval
   gdk::EventKey.length gdk::EventKey.string gdk::Font.type gdk::Font.ascent
   gdk::Font.descent gdk::Geometry.min-width gdk::Geometry.min-height
   gdk::Geometry.max-width gdk::Geometry.max-height gdk::Geometry.base-width
   gdk::Geometry.base-height gdk::Geometry.width-inc gdk::Geometry.height-inc
   gdk::Geometry.min-aspect gdk::Geometry.max-aspect gdk::Geometry.win-gravity
   gdk::EventExpose.type gdk::EventExpose.window gdk::EventExpose.send-event
   gdk::EventExpose.area.x gdk::EventExpose.area.y gdk::EventExpose.area.width
   gdk::EventExpose.area.height gdk::EventExpose.region gdk::EventExpose.count
   gdk::Image.type gdk::Image.visual gdk::Image.byte-order gdk::Image.width
   gdk::Image.height gdk::Image.depth gdk::Image.bpp gdk::Image.bpl
   gdk::Image.bits-per-pixel gdk::Image.mem gdk::Image.colormap
   gdk::Image.windowing-data gdk::Visual.type gdk::Visual.depth
   gdk::Visual.byte-order gdk::Visual.colormap-size gdk::Visual.bits-per-rgb
   gdk::Visual.red-mask gdk::Visual.red-shift gdk::Visual.green-prec
   gdk::Visual.blue-mask gdk::Visual.blue-shift gdk::Visual.blue-prec
   gdk::colormap-get-type gdk::colormap-new gdk::colormap-get-system
   gdk::colormap-get-system-size gdk::colormap-alloc-color
   gdk::colormap-get-visual gdk::color-copy gdk::color-free gdk::color-parse
   gdk::color-white gdk::color-black gdk::color-alloc gdk::color-change
   gdk::cursor-new gdk::cursor-new-from-pixmap gdk::cursor-ref
   gdk::cursor-unref gdk::drag-context-get-type gdk::drag-context-new
   gdk::drag-context-ref gdk::drag-context-unref gdk::drag-status
   gdk::drop-reply gdk::drop-finish gdk::drag-get-selection gdk::drag-begin
   gdk::drag-get-protocol gdk::drag-find-window gdk::drag-motion gdk::drag-drop
   gdk::drag-abort gdk::device-get-type gdk::devices-list
   gdk::device-set-source gdk::device-set-mode gdk::device-set-key
   gdk::device-set-axis-use gdk::device-get-state gdk::device-get-history
   gdk::device-free-history gdk::device-get-axis
   gdk::input-set-extension-events gdk::device-get-core-pointer
   gdk::event-get-type gdk::events-pending gdk::event-get gdk::event-peek
   gdk::event-get-graphics-expose gdk::event-put gdk::event-copy
   gdk::event-free gdk::event-get-time gdk::event-get-state
   gdk::event-get-coords gdk::event-get-root-coords gdk::event-get-axis
   gdk::event-handler-set gdk::set-show-events gdk::get-show-events
   gdk::add-client-message-filter gdk::setting-get gdk::gc-get-type gdk::gc-new
   gdk::gc-new-with-values gdk::gc-get-values gdk::gc-set-values
   gdk::gc-set-foreground gdk::gc-set-background gdk::gc-set-font
   gdk::gc-set-function gdk::gc-set-fill gdk::gc-set-tile gdk::gc-set-stipple
   gdk::gc-set-ts-origin gdk::gc-set-clip-origin gdk::gc-set-clip-mask
   gdk::gc-set-clip-rectangle gdk::gc-set-clip-region gdk::gc-set-subwindow
   gdk::gc-set-exposures gdk::gc-set-line-attributes gdk::gc-set-dashes
   gdk::gc-offset gdk::gc-copy gdk::gc-set-colormap gdk::gc-get-colormap
   gdk::gc-set-rgb-fg-color gdk::gc-set-rgb-bg-color gdk::rgb-xpixel-from-rgb
   gdk::rgb-gc-set-foreground gdk::rgb-gc-set-background gdk::draw-rgb-image
   gdk::draw-rgb-image-dithalign gdk::draw-rgb-32-image gdk::draw-gray-image
   gdk::draw-indexed-image gdk::rgb-cmap-new gdk::rgb-set-verbose
   gdk::rgb-set-install gdk::rgb-set-min-colors gdk::rgb-get-colormap
   gdk::rgb-get-visual gdk::rgb-ditherable gdk::pixbuf-get-colorspace
   gdk::pixbuf-get-n-channels gdk::pixbuf-get-has-alpha
   gdk::pixbuf-get-bits-per-sample gdk::pixbuf-get-pixels gdk::pixbuf-get-width
   gdk::pixbuf-get-height gdk::pixbuf-get-rowstride gdk::pixbuf-new
   gdk::pixbuf-copy gdk::pixbuf-new-from-file gdk::pixbuf-new-from-data
   gdk::pixbuf-new-from-xpm-data gdk::pixbuf-new-from-inline gdk::pixbuf-fill
   gdk::pixbuf-save gdk::pixbuf-savev gdk::pixbuf-add-alpha
   gdk::pixbuf-copy-area gdk::pixbuf-saturate-and-pixelate gdk::pixbuf-scale
   gdk::pixbuf-composite gdk::pixbuf-composite-color gdk::pixbuf-scale-simple
   gdk::pixbuf-composite-color-simple gdk::pixbuf-animation-get-type
   gdk::pixbuf-animation-new-from-file gdk::pixbuf-animation-get-width
   gdk::pixbuf-animation-get-height gdk::pixbuf-animation-is-static-image
   gdk::pixbuf-animation-get-static-image gdk::pixbuf-animation-get-iter
   gdk::pixbuf-animation-iter-get-type
   gdk::pixbuf-animation-iter-get-delay-time
   gdk::pixbuf-animation-iter-get-pixbuf
   gdk::pixbuf-animation-iter-on-currently-loading-frame
   gdk::pixbuf-animation-iter-advance gdk::pixbuf-get-option
   gdk::pixbuf-loader-get-type gdk::pixbuf-loader-new
   gdk::pixbuf-loader-new-with-type gdk::pixbuf-loader-write
   gdk::pixbuf-loader-get-pixbuf gdk::pixbuf-loader-get-animation
   gdk::pixbuf-loader-close gdk::drawable-get-type gdk::drawable-set-data
   gdk::drawable-get-data gdk::drawable-get-size gdk::drawable-set-colormap
   gdk::drawable-get-colormap gdk::drawable-get-visual gdk::drawable-get-depth
   gdk::drawable-ref gdk::drawable-unref gdk::draw-point gdk::draw-line
   gdk::draw-rectangle gdk::draw-arc gdk::draw-polygon gdk::draw-string
   gdk::draw-text gdk::draw-text-wc gdk::draw-drawable gdk::draw-image
   gdk::draw-points gdk::draw-segments gdk::draw-lines gdk::draw-glyphs
   gdk::draw-layout-line gdk::draw-layout gdk::draw-layout-line-with-colors
   gdk::drawable-get-image gdk::drawable-get-clip-region
   gdk::drawable-get-visible-region gdk::font-ref gdk::font-unref gdk::font-id
   gdk::font-load gdk::fontset-load gdk::font-from-description
   gdk::string-width gdk::text-width gdk::text-width-wc gdk::char-width
   gdk::char-width-wc gdk::string-measure gdk::text-measure gdk::char-measure
   gdk::string-height gdk::text-height gdk::char-height gdk::text-extents
   gdk::text-extents-wc gdk::string-extents gdk::image-get-type gdk::image-new
   gdk::image-put-pixel gdk::image-get-pixel gdk::image-set-colormap
   gdk::image-get-colormap gdk::keymap-get-type gdk::keymap-get-default
   gdk::keymap-lookup-key gdk::keymap-translate-keyboard-state
   gdk::keymap-get-entries-for-keyval gdk::keymap-get-entries-for-keycode
   gdk::keymap-get-direction gdk::keyval-name gdk::keyval-from-name
   gdk::keyval-convert-case gdk::keyval-to-upper gdk::keyval-to-lower
   gdk::keyval-is-upper gdk::keyval-is-lower gdk::keyval-to-unicode
   gdk::unicode-to-keyval gdk::pixbuf-render-to-drawable
   gdk::pixbuf-render-to-drawable-alpha gdk::pixbuf-render-pixmap-and-mask
   gdk::pixbuf-get-from-drawable gdk::pixbuf-get-from-image
   gdk::pixmap-get-type gdk::pixmap-new gdk::bitmap-create-from-data
   gdk::pixmap-create-from-data gdk::pixmap-create-from-xpm
   gdk::pixmap-colormap-create-from-xpm gdk::pixmap-create-from-xpm-d
   gdk::pixmap-colormap-create-from-xpm-d gdk::pixmap-foreign-new
   gdk::pixmap-lookup gdk::atom-intern gdk::atom-name gdk::property-get
   gdk::property-change gdk::property-delete gdk::selection-owner-get
   gdk::selection-convert gdk::selection-property-get
   gdk::selection-send-notify gdk::visual-get-best-depth
   gdk::visual-get-best-type gdk::visual-get-system gdk::visual-get-best
   gdk::visual-get-best-with-depth gdk::visual-get-best-with-type
   gdk::visual-get-best-with-both gdk::query-depths gdk::query-visual-types
   gdk::list-visuals gdk::window-object-get-type gdk::window-new
   gdk::window-destroy gdk::window-get-window-type gdk::window-at-pointer
   gdk::window-show gdk::window-hide gdk::window-withdraw gdk::window-move
   gdk::window-resize gdk::window-move-resize gdk::window-reparent
   gdk::window-clear gdk::window-clear-area gdk::window-clear-area-e
   gdk::window-raise gdk::window-lower gdk::window-focus
   gdk::window-set-user-data gdk::window-set-override-redirect
   gdk::window-add-filter gdk::window-remove-filter gdk::window-scroll
   gdk::window-shape-combine-mask gdk::window-set-child-shapes
   gdk::window-merge-child-shapes gdk::window-is-visible
   gdk::window-is-viewable gdk::window-get-state
   gdk::window-set-static-gravities gdk::window-foreign-new gdk::window-lookup
   gdk::window-set-hints gdk::window-set-type-hint gdk::window-set-modal-hint
   gdk::window-set-geometry-hints gdk::set-sm-client-id
   gdk::window-begin-paint-rect gdk::window-begin-paint-region
   gdk::window-end-paint gdk::window-set-title gdk::window-set-role
   gdk::window-set-transient-for gdk::window-set-background
   gdk::window-set-back-pixmap gdk::window-set-cursor gdk::window-get-geometry
   gdk::window-get-position gdk::window-get-origin
   gdk::window-get-deskrelative-origin gdk::window-get-root-origin
   gdk::window-get-frame-extents gdk::window-get-pointer gdk::window-get-parent
   gdk::window-get-toplevel gdk::window-get-children gdk::window-peek-children
   gdk::window-get-events gdk::window-set-events gdk::window-set-icon-list
   gdk::window-set-icon gdk::window-set-icon-name gdk::window-set-group
   gdk::window-set-decorations gdk::window-get-decorations
   gdk::window-set-functions gdk::window-get-toplevels gdk::window-iconify
   gdk::window-deiconify gdk::window-stick gdk::window-unstick
   gdk::window-maximize gdk::window-unmaximize gdk::window-register-dnd
   gdk::window-begin-resize-drag gdk::window-begin-move-drag
   gdk::window-invalidate-rect gdk::window-invalidate-region
   gdk::window-invalidate-maybe-recurse gdk::window-get-update-area
   gdk::window-freeze-updates gdk::window-thaw-updates
   gdk::window-process-all-updates gdk::window-process-updates
   gdk::window-set-debug-updates gdk::window-constrain-size
   gdk::window-get-internal-paint-info gdk::set-pointer-hooks
   gdk::get-default-root-window gdk::pointer-grab gdk::keyboard-grab
   gdk::pointer-ungrab gdk::keyboard-ungrab gdk::pointer-is-grabbed
   gdk::screen-width gdk::screen-height gdk::screen-width-mm
   gdk::screen-height-mm gdk::beep gdk::flush gdk::set-double-click-time
   gdk::rectangle-intersect gdk::rectangle-union gdk::threads-enter
   gdk::threads-leave gdk::threads-init)
 :gdk)
(export
 '(atk::state-invalid atk::state-active atk::state-armed atk::state-busy
   atk::state-checked atk::state-defunct atk::state-editable atk::state-enabled
   atk::state-expandable atk::state-expanded atk::state-focusable
   atk::state-focused atk::state-horizontal atk::state-iconified
   atk::state-modal atk::state-multi-line atk::state-multiselectable
   atk::state-opaque atk::state-pressed atk::state-resizable
   atk::state-selectable atk::state-selected atk::state-sensitive
   atk::state-showing atk::state-single-line atk::state-stale
   atk::state-transient atk::state-vertical atk::state-visible
   atk::state-manages-descendants atk::state-indeterminate
   atk::state-last-defined atk::relation-null atk::relation-controlled-by
   atk::relation-controller-for atk::relation-label-for
   atk::relation-labelled-by atk::relation-member-of
   atk::relation-node-child-of atk::relation-flows-to atk::relation-flows-from
   atk::relation-subwindow-of atk::relation-embeds atk::relation-embedded-by
   atk::relation-popup-for atk::relation-last-defined atk::role-invalid
   atk::role-accel-label atk::role-alert atk::role-animation atk::role-arrow
   atk::role-calendar atk::role-canvas atk::role-check-box
   atk::role-check-menu-item atk::role-color-chooser atk::role-column-header
   atk::role-combo-box atk::role-date-editor atk::role-desktop-icon
   atk::role-desktop-frame atk::role-dial atk::role-dialog
   atk::role-directory-pane atk::role-drawing-area atk::role-file-chooser
   atk::role-filler atk::role-font-chooser atk::role-frame atk::role-glass-pane
   atk::role-html-container atk::role-icon atk::role-image
   atk::role-internal-frame atk::role-label atk::role-layered-pane
   atk::role-list atk::role-list-item atk::role-menu atk::role-menu-bar
   atk::role-menu-item atk::role-option-pane atk::role-page-tab
   atk::role-page-tab-list atk::role-panel atk::role-password-text
   atk::role-popup-menu atk::role-progress-bar atk::role-push-button
   atk::role-radio-button atk::role-radio-menu-item atk::role-root-pane
   atk::role-row-header atk::role-scroll-bar atk::role-scroll-pane
   atk::role-separator atk::role-slider atk::role-split-pane
   atk::role-spin-button atk::role-statusbar atk::role-table
   atk::role-table-cell atk::role-table-column-header
   atk::role-table-row-header atk::role-tear-off-menu-item atk::role-terminal
   atk::role-text atk::role-toggle-button atk::role-tool-bar atk::role-tool-tip
   atk::role-tree atk::role-tree-table atk::role-unknown atk::role-viewport
   atk::role-window atk::role-header atk::role-footer atk::role-paragraph
   atk::role-ruler atk::role-application atk::role-autocomplete
   atk::role-last-defined atk::layer-invalid atk::layer-background
   atk::layer-canvas atk::layer-widget atk::layer-mdi atk::layer-popup
   atk::layer-overlay atk::layer-window atk::xy-screen atk::xy-window
   atk::text-attr-invalid atk::text-attr-left-margin
   atk::text-attr-right-margin atk::text-attr-indent atk::text-attr-invisible
   atk::text-attr-editable atk::text-attr-pixels-above-lines
   atk::text-attr-pixels-below-lines atk::text-attr-pixels-inside-wrap
   atk::text-attr-bg-full-height atk::text-attr-rise atk::text-attr-underline
   atk::text-attr-strikethrough atk::text-attr-size atk::text-attr-scale
   atk::text-attr-weight atk::text-attr-language atk::text-attr-family-name
   atk::text-attr-bg-color atk::text-attr-fg-color atk::text-attr-bg-stipple
   atk::text-attr-fg-stipple atk::text-attr-wrap-mode atk::text-attr-direction
   atk::text-attr-justification atk::text-attr-stretch atk::text-attr-variant
   atk::text-attr-style atk::text-attr-last-defined atk::text-boundary-char
   atk::text-boundary-word-start atk::text-boundary-word-end
   atk::text-boundary-sentence-start atk::text-boundary-sentence-end
   atk::text-boundary-line-start atk::text-boundary-line-end
   atk::state-type-get-name atk::state-type-for-name atk::object-get-type
   atk::implementor-get-type atk::implementor-ref-accessible
   atk::object-get-name atk::object-get-description atk::object-get-parent
   atk::object-get-n-accessible-children atk::object-ref-accessible-child
   atk::object-ref-relation-set atk::object-get-role atk::object-get-layer
   atk::object-get-mdi-zorder atk::object-ref-state-set
   atk::object-get-index-in-parent atk::object-set-name
   atk::object-set-description atk::object-set-parent atk::object-set-role
   atk::object-connect-property-change-handler
   atk::object-remove-property-change-handler atk::object-notify-state-change
   atk::role-get-name atk::role-for-name atk::action-get-type
   atk::action-do-action atk::action-get-n-actions atk::action-get-description
   atk::action-get-name atk::action-get-keybinding atk::action-set-description
   atk::util-get-type atk::add-focus-tracker atk::remove-focus-tracker
   atk::focus-tracker-init atk::focus-tracker-notify
   atk::add-global-event-listener atk::remove-global-event-listener
   atk::add-key-event-listener atk::remove-key-event-listener atk::get-root
   atk::get-toolkit-name atk::get-toolkit-version atk::component-get-type
   atk::component-add-focus-handler atk::component-contains
   atk::component-ref-accessible-at-point atk::component-get-extents
   atk::component-get-position atk::component-get-size
   atk::component-grab-focus atk::component-remove-focus-handler
   atk::component-set-extents atk::component-set-position
   atk::component-set-size atk::document-get-type
   atk::document-get-document-type atk::document-get-document
   atk::text-get-type atk::text-get-text atk::text-get-character-at-offset
   atk::text-get-text-after-offset atk::text-get-text-at-offset
   atk::text-get-text-before-offset atk::text-get-caret-offset
   atk::text-get-character-extents atk::text-get-run-attributes
   atk::text-get-default-attributes atk::text-get-character-count
   atk::text-get-offset-at-point atk::text-get-n-selections
   atk::text-get-selection atk::text-add-selection atk::text-remove-selection
   atk::text-set-selection atk::text-set-caret-offset atk::attribute-set-free
   atk::text-attribute-get-name atk::text-attribute-get-value
   atk::editable-text-get-type atk::editable-text-set-run-attributes
   atk::editable-text-set-text-contents atk::editable-text-insert-text
   atk::editable-text-copy-text atk::editable-text-cut-text
   atk::editable-text-delete-text atk::editable-text-paste-text
   atk::hyperlink-get-type atk::hyperlink-get-uri atk::hyperlink-get-object
   atk::hyperlink-get-end-index atk::hyperlink-get-start-index
   atk::hyperlink-is-valid atk::hyperlink-get-n-anchors atk::hypertext-get-type
   atk::hypertext-get-link atk::hypertext-get-n-links
   atk::hypertext-get-link-index atk::image-get-type
   atk::image-get-image-description atk::image-get-image-size
   atk::image-set-image-description atk::image-get-image-position
   atk::no-op-object-get-type atk::no-op-object-new
   atk::object-factory-get-type atk::object-factory-create-accessible
   atk::object-factory-invalidate atk::no-op-object-factory-get-type
   atk::no-op-object-factory-new atk::registry-get-type
   atk::registry-set-factory-type atk::registry-get-factory-type
   atk::registry-get-factory atk::get-default-registry atk::relation-get-type
   atk::relation-type-register atk::relation-type-get-name
   atk::relation-type-for-name atk::relation-new
   atk::relation-get-relation-type atk::relation-get-target
   atk::relation-set-get-type atk::relation-set-new atk::relation-set-contains
   atk::relation-set-remove atk::relation-set-add
   atk::relation-set-get-n-relations atk::relation-set-get-relation
   atk::relation-set-get-relation-by-type atk::selection-get-type
   atk::selection-add-selection atk::selection-clear-selection
   atk::selection-ref-selection atk::selection-get-selection-count
   atk::selection-is-child-selected atk::selection-remove-selection
   atk::selection-select-all-selection atk::state-set-get-type
   atk::state-set-new atk::state-set-is-empty atk::state-set-add-state
   atk::state-set-add-states atk::state-set-clear-states
   atk::state-set-contains-state atk::state-set-contains-states
   atk::state-set-remove-state atk::state-set-and-sets atk::state-set-or-sets
   atk::state-set-xor-sets atk::streamable-content-get-type
   atk::streamable-content-get-n-mime-types
   atk::streamable-content-get-mime-type atk::streamable-content-get-stream
   atk::table-get-type atk::table-ref-at atk::table-get-index-at
   atk::table-get-column-at-index atk::table-get-row-at-index
   atk::table-get-n-columns atk::table-get-n-rows
   atk::table-get-column-extent-at atk::table-get-row-extent-at
   atk::table-get-caption atk::table-get-column-description
   atk::table-get-column-header atk::table-get-row-description
   atk::table-get-row-header atk::table-get-summary atk::table-set-caption
   atk::table-set-column-description atk::table-set-column-header
   atk::table-set-row-description atk::table-set-row-header
   atk::table-set-summary atk::table-get-selected-columns
   atk::table-get-selected-rows atk::table-is-column-selected
   atk::table-is-row-selected atk::table-is-selected
   atk::table-add-row-selection atk::table-remove-row-selection
   atk::table-add-column-selection atk::table-remove-column-selection
   atk::value-get-type atk::value-get-current-value
   atk::value-get-maximum-value atk::value-get-minimum-value
   atk::value-set-current-value)
 :atk)
(export
 '(pango::coverage-none pango::coverage-fallback pango::coverage-approximate
   pango::coverage-exact pango::direction-ltr pango::direction-rtl
   pango::direction-ttb-ltr pango::direction-ttb-rtl pango::style-normal
   pango::style-oblique pango::style-italic pango::variant-normal
   pango::variant-small-caps pango::weight-ultralight pango::weight-light
   pango::weight-normal pango::weight-bold pango::weight-ultrabold
   pango::weight-heavy pango::stretch-ultra-condensed
   pango::stretch-extra-condensed pango::stretch-condensed
   pango::stretch-semi-condensed pango::stretch-normal
   pango::stretch-semi-expanded pango::stretch-expanded
   pango::stretch-extra-expanded pango::stretch-ultra-expanded
   pango::font-mask-family pango::font-mask-style pango::font-mask-variant
   pango::font-mask-weight pango::font-mask-stretch pango::font-mask-size
   pango::attr-invalid pango::attr-language pango::attr-family
   pango::attr-style pango::attr-weight pango::attr-variant pango::attr-stretch
   pango::attr-size pango::attr-font-desc pango::attr-foreground
   pango::attr-background pango::attr-underline pango::attr-strikethrough
   pango::attr-rise pango::attr-shape pango::attr-scale pango::underline-none
   pango::underline-single pango::underline-double pango::underline-low
   pango::tab-left pango::align-left pango::align-center pango::align-right
   pango::wrap-word pango::wrap-char pango::wrap-word-char pango::coverage-ref
   pango::coverage-unref pango::coverage-copy pango::coverage-get
   pango::coverage-set pango::coverage-max pango::coverage-to-bytes
   pango::coverage-from-bytes pango::language-from-string
   pango::language-matches pango::font-description-new
   pango::font-description-copy pango::font-description-copy-static
   pango::font-description-hash pango::font-description-equal
   pango::font-description-free pango::font-descriptions-free
   pango::font-description-set-family pango::font-description-set-family-static
   pango::font-description-get-family pango::font-description-set-style
   pango::font-description-get-style pango::font-description-set-variant
   pango::font-description-get-variant pango::font-description-set-weight
   pango::font-description-get-weight pango::font-description-set-stretch
   pango::font-description-get-stretch pango::font-description-set-size
   pango::font-description-get-size pango::font-description-get-set-fields
   pango::font-description-unset-fields pango::font-description-merge
   pango::font-description-merge-static pango::font-description-better-match
   pango::font-description-from-string pango::font-description-to-string
   pango::font-description-to-filename pango::font-metrics-get-type
   pango::font-metrics-ref pango::font-metrics-unref
   pango::font-metrics-get-ascent pango::font-metrics-get-descent
   pango::font-metrics-get-approximate-char-width
   pango::font-metrics-get-approximate-digit-width pango::font-family-get-type
   pango::font-family-list-faces pango::font-family-get-name
   pango::font-face-get-type pango::font-face-describe
   pango::font-face-get-face-name pango::font-get-type pango::font-describe
   pango::font-get-coverage pango::font-find-shaper pango::font-get-metrics
   pango::font-get-glyph-extents pango::color-get-type pango::color-copy
   pango::color-free pango::color-parse pango::attr-type-register
   pango::attribute-copy pango::attribute-destroy pango::attribute-equal
   pango::attr-language-new pango::attr-family-new pango::attr-foreground-new
   pango::attr-background-new pango::attr-size-new pango::attr-style-new
   pango::attr-weight-new pango::attr-variant-new pango::attr-stretch-new
   pango::attr-font-desc-new pango::attr-underline-new
   pango::attr-strikethrough-new pango::attr-rise-new pango::attr-shape-new
   pango::attr-scale-new pango::attr-list-get-type pango::attr-list-new
   pango::attr-list-ref pango::attr-list-unref pango::attr-list-copy
   pango::attr-list-insert pango::attr-list-insert-before
   pango::attr-list-change pango::attr-list-splice
   pango::attr-list-get-iterator pango::attr-iterator-range
   pango::attr-iterator-next pango::attr-iterator-copy
   pango::attr-iterator-destroy pango::attr-iterator-get
   pango::attr-iterator-get-font pango::parse-markup pango::break
   pango::find-paragraph-boundary pango::get-log-attrs pango::fontset-get-font
   pango::fontset-get-metrics pango::font-map-load-font
   pango::font-map-load-fontset pango::font-map-list-families
   pango::context-list-families pango::context-load-font
   pango::context-load-fontset pango::context-get-metrics
   pango::context-set-font-description pango::context-get-font-description
   pango::context-get-language pango::context-set-language
   pango::context-set-base-dir pango::context-get-base-dir pango::itemize
   pango::glyph-string-new pango::glyph-string-set-size
   pango::glyph-string-get-type pango::glyph-string-copy
   pango::glyph-string-free pango::glyph-string-extents
   pango::glyph-string-extents-range pango::glyph-string-get-logical-widths
   pango::glyph-string-index-to-x pango::glyph-string-x-to-index pango::shape
   pango::reorder-items pango::tab-array-new
   pango::tab-array-new-with-positions pango::tab-array-get-type
   pango::tab-array-copy pango::tab-array-free pango::tab-array-get-size
   pango::tab-array-resize pango::tab-array-set-tab pango::tab-array-get-tab
   pango::tab-array-get-tabs pango::tab-array-get-positions-in-pixels
   pango::layout-get-type pango::layout-new pango::layout-copy
   pango::layout-get-context pango::layout-set-attributes
   pango::layout-get-attributes pango::layout-set-text pango::layout-get-text
   pango::layout-set-markup pango::layout-set-markup-with-accel
   pango::layout-set-font-description pango::layout-set-width
   pango::layout-get-width pango::layout-set-wrap pango::layout-get-wrap
   pango::layout-set-indent pango::layout-get-indent pango::layout-set-spacing
   pango::layout-get-spacing pango::layout-set-justify
   pango::layout-get-justify pango::layout-set-alignment
   pango::layout-get-alignment pango::layout-set-tabs pango::layout-get-tabs
   pango::layout-set-single-paragraph-mode
   pango::layout-get-single-paragraph-mode pango::layout-context-changed
   pango::layout-get-log-attrs pango::layout-index-to-pos
   pango::layout-get-cursor-pos pango::layout-move-cursor-visually
   pango::layout-xy-to-index pango::layout-get-extents
   pango::layout-get-pixel-extents pango::layout-get-size
   pango::layout-get-pixel-size pango::layout-get-line-count
   pango::layout-get-line pango::layout-get-lines pango::layout-line-ref
   pango::layout-line-unref pango::layout-line-x-to-index
   pango::layout-line-index-to-x pango::layout-line-get-x-ranges
   pango::layout-line-get-extents pango::layout-line-get-pixel-extents
   pango::layout-get-iter pango::layout-iter-free pango::layout-iter-get-index
   pango::layout-iter-get-run pango::layout-iter-get-line
   pango::layout-iter-at-last-line pango::layout-iter-next-char
   pango::layout-iter-next-cluster pango::layout-iter-next-run
   pango::layout-iter-next-line pango::layout-iter-get-char-extents
   pango::layout-iter-get-cluster-extents pango::layout-iter-get-run-extents
   pango::layout-iter-get-line-extents pango::layout-iter-get-line-yrange
   pango::layout-iter-get-layout-extents pango::layout-iter-get-baseline)
 :pango)
(export
 '(gtk::+false+ gtk::+true+ gtk::init-ensure struct-alloc struct-free
   cstring->string string->cstring cstring-free define-signal-handler
   gtk::arrow-up gtk::arrow-down gtk::arrow-left gtk::arrow-right gtk::expand
   gtk::shrink gtk::fill gtk::buttonbox-default-style gtk::buttonbox-spread
   gtk::buttonbox-edge gtk::buttonbox-start gtk::buttonbox-end
   gtk::curve-type-linear gtk::curve-type-spline gtk::curve-type-free
   gtk::dir-tab-forward gtk::dir-tab-backward gtk::dir-up gtk::dir-down
   gtk::dir-left gtk::dir-right gtk::expander-collapsed
   gtk::expander-semi-collapsed gtk::expander-semi-expanded
   gtk::expander-expanded gtk::icon-size-invalid gtk::icon-size-menu
   gtk::icon-size-small-toolbar gtk::icon-size-large-toolbar
   gtk::icon-size-button gtk::icon-size-dnd gtk::icon-size-dialog
   gtk::text-dir-none gtk::text-dir-ltr gtk::text-dir-rtl gtk::justify-left
   gtk::justify-right gtk::justify-center gtk::justify-fill gtk::pixels
   gtk::inches gtk::centimeters gtk::orientation-horizontal
   gtk::orientation-vertical gtk::corner-top-left gtk::corner-bottom-left
   gtk::corner-top-right gtk::corner-bottom-right gtk::pack-start gtk::pack-end
   gtk::path-prio-lowest gtk::path-prio-gtk gtk::path-prio-application
   gtk::path-prio-theme gtk::path-prio-rc gtk::path-prio-highest
   gtk::path-widget gtk::path-widget-class gtk::path-class gtk::policy-always
   gtk::policy-automatic gtk::policy-never gtk::pos-left gtk::pos-right
   gtk::pos-top gtk::pos-bottom gtk::relief-normal gtk::relief-half
   gtk::relief-none gtk::resize-parent gtk::resize-queue gtk::resize-immediate
   gtk::selection-none gtk::selection-single gtk::selection-browse
   gtk::selection-multiple gtk::selection-extended gtk::shadow-none
   gtk::shadow-in gtk::shadow-out gtk::shadow-etched-in gtk::shadow-etched-out
   gtk::state-normal gtk::state-active gtk::state-prelight gtk::state-selected
   gtk::state-insensitive gtk::toolbar-icons gtk::toolbar-text
   gtk::toolbar-both gtk::toolbar-both-horiz gtk::update-continuous
   gtk::update-discontinuous gtk::update-delayed gtk::visibility-none
   gtk::visibility-partial gtk::visibility-full gtk::win-pos-none
   gtk::win-pos-center gtk::win-pos-mouse gtk::win-pos-center-always
   gtk::win-pos-center-on-parent gtk::window-toplevel gtk::window-popup
   gtk::wrap-none gtk::wrap-char gtk::wrap-word gtk::sort-ascending
   gtk::sort-descending gtk::accel-visible gtk::accel-locked gtk::accel-mask
   gtk::calendar-show-heading gtk::calendar-show-day-names
   gtk::calendar-no-month-change gtk::calendar-show-week-numbers
   gtk::calendar-week-start-monday gtk::cell-renderer-selected
   gtk::cell-renderer-prelit gtk::cell-renderer-insensitive
   gtk::cell-renderer-sorted gtk::cell-renderer-focused gtk::cell-empty
   gtk::cell-text gtk::cell-pixmap gtk::cell-pixtext gtk::cell-widget
   gtk::dialog-modal gtk::dialog-destroy-with-parent gtk::dialog-no-separator
   gtk::ctree-lines-none gtk::ctree-lines-solid gtk::ctree-lines-dotted
   gtk::ctree-lines-tabbed gtk::ctree-expander-none gtk::ctree-expander-square
   gtk::ctree-expander-triangle gtk::ctree-expander-circular
   gtk::dest-default-motion gtk::dest-default-highlight gtk::dest-default-drop
   gtk::dest-default-all gtk::image-empty gtk::image-pixmap gtk::image-image
   gtk::image-pixbuf gtk::image-stock gtk::image-icon-set gtk::image-animation
   gtk::tree-model-iters-persist gtk::tree-model-list-only gtk::message-info
   gtk::message-warning gtk::message-question gtk::message-error
   gtk::buttons-none gtk::buttons-ok gtk::buttons-close gtk::buttons-cancel
   gtk::buttons-yes-no gtk::buttons-ok-cancel gtk::progress-left-to-right
   gtk::progress-right-to-left gtk::progress-bottom-to-top
   gtk::progress-top-to-bottom gtk::size-group-none gtk::size-group-horizontal
   gtk::size-group-vertical gtk::size-group-both gtk::update-always
   gtk::update-if-valid gtk::spin-step-forward gtk::spin-step-backward
   gtk::spin-page-forward gtk::spin-page-backward gtk::spin-home gtk::spin-end
   gtk::spin-user-defined gtk::text-search-visible-only
   gtk::text-search-text-only gtk::text-window-private gtk::text-window-widget
   gtk::text-window-text gtk::text-window-left gtk::text-window-right
   gtk::text-window-top gtk::text-window-bottom gtk::toolbar-child-space
   gtk::toolbar-child-button gtk::toolbar-child-togglebutton
   gtk::toolbar-child-radiobutton gtk::toolbar-child-widget
   gtk::tree-view-column-grow-only gtk::tree-view-column-autosize
   gtk::tree-view-column-fixed gtk::tree-view-drop-before
   gtk::tree-view-drop-after gtk::tree-view-drop-into-or-before
   gtk::tree-view-drop-into-or-after gtk::Adjustment.lower
   gtk::Adjustment.upper gtk::Adjustment.value gtk::Adjustment.step-increment
   gtk::Adjustment.page-increment gtk::Adjustment.page-size gtk::Style.fg.pixel
   gtk::Style.fg.red gtk::Style.fg.green gtk::Style.fg.blue gtk::Style.bg.pixel
   gtk::Style.bg.red gtk::Style.bg.green gtk::Style.bg.blue
   gtk::Style.light.pixel gtk::Style.light.red gtk::Style.light.green
   gtk::Style.light.blue gtk::Style.dark.pixel gtk::Style.dark.red
   gtk::Style.dark.green gtk::Style.dark.blue gtk::Style.mid.pixel
   gtk::Style.mid.red gtk::Style.mid.green gtk::Style.mid.blue
   gtk::Style.text.pixel gtk::Style.text.red gtk::Style.text.green
   gtk::Style.text.blue gtk::Style.base.pixel gtk::Style.base.red
   gtk::Style.base.green gtk::Style.base.blue gtk::Style.text-aa.pixel
   gtk::Style.text-aa.red gtk::Style.text-aa.green gtk::Style.text-aa.blue
   gtk::Style.black.pixel gtk::Style.black.red gtk::Style.black.green
   gtk::Style.black.blue gtk::Style.white.pixel gtk::Style.white.red
   gtk::Style.white.green gtk::Style.white.blue gtk::Style.font-desc
   gtk::Style.xthickness gtk::Style.ythickness gtk::Style.fg-gc
   gtk::Style.bg-gc gtk::Style.light-gc gtk::Style.dark-gc gtk::Style.mid-gc
   gtk::Style.text-gc gtk::Style.base-gc gtk::Style.text-aa-gc
   gtk::Style.black-gc gtk::Style.white-gc gtk::Style.bg-pixmap
   gtk::RcStyle.name gtk::RcStyle.bg-pixmap-name gtk::RcStyle.font-desc
   gtk::RcStyle.color-flags gtk::RcStyle.fg.pixel gtk::RcStyle.fg.red
   gtk::RcStyle.fg.green gtk::RcStyle.fg.blue gtk::RcStyle.bg.pixel
   gtk::RcStyle.bg.red gtk::RcStyle.bg.green gtk::RcStyle.bg.blue
   gtk::RcStyle.text.pixel gtk::RcStyle.text.red gtk::RcStyle.text.green
   gtk::RcStyle.text.blue gtk::RcStyle.base.pixel gtk::RcStyle.base.red
   gtk::RcStyle.base.green gtk::RcStyle.base.blue gtk::RcStyle.xthickness
   gtk::RcStyle.ythickness gtk::Widget.state gtk::Widget.name gtk::Widget.style
   gtk::Widget.allocation.x gtk::Widget.allocation.y
   gtk::Widget.allocation.width gtk::Widget.allocation.height
   gtk::Widget.window gtk::Widget.parent gtk::MenuShell.children
   gtk::Arrow.arrow-type gtk::Arrow.shadow-type gtk::Box.children
   gtk::Box.spacing gtk::Calendar.month gtk::Calendar.year
   gtk::Calendar.selected-day gtk::Calendar.num-marked-dates
   gtk::Calendar.marked-date gtk::Dialog.vbox gtk::Dialog.action-area
   gtk::ColorSelectionDialog.colorsel gtk::ColorSelectionDialog.ok-button
   gtk::ColorSelectionDialog.cancel-button
   gtk::ColorSelectionDialog.help-button gtk::Combo.entry gtk::Combo.list
   gtk::FileSelection.dir-list gtk::FileSelection.file-list
   gtk::FileSelection.selection-entry gtk::FileSelection.selection-text
   gtk::FileSelection.main-vbox gtk::FileSelection.ok-button
   gtk::FileSelection.cancel-button gtk::FileSelection.help-button
   gtk::FileSelection.history-pulldown gtk::FileSelection.history-menu
   gtk::FileSelection.history-list gtk::FileSelection.fileop-dialog
   gtk::FileSelection.fileop-entry gtk::FileSelection.fileop-file
   gtk::FileSelection.cmpl-state gtk::FileSelection.fileop-c-dir
   gtk::FileSelection.fileop-del-file gtk::FileSelection.fileop-ren-file
   gtk::FileSelection.button-area gtk::FileSelection.action-area
   gtk::Fixed.children gtk::FontSelectionDialog.ok-button
   gtk::FontSelectionDialog.apply-button gtk::FontSelectionDialog.cancel-button
   gtk::HandleBox.shadow-type gtk::Layout.bin-window gtk::Table.children
   gtk::Table.rows gtk::Table.cols gtk::accel-group-get-type
   gtk::accel-group-new gtk::accel-group-lock gtk::accel-group-unlock
   gtk::accel-group-connect gtk::accel-group-connect-by-path
   gtk::accel-group-disconnect gtk::accel-group-disconnect-key
   gtk::accel-groups-activate gtk::accel-groups-from-object
   gtk::accel-group-find gtk::accel-group-from-accel-closure
   gtk::accelerator-valid gtk::accelerator-parse gtk::accelerator-name
   gtk::accelerator-set-default-mod-mask gtk::accelerator-get-default-mod-mask
   gtk::object-get-type gtk::object-new gtk::object-sink gtk::object-destroy
   gtk::adjustment-get-type gtk::adjustment-new gtk::adjustment-changed
   gtk::adjustment-value-changed gtk::adjustment-clamp-page
   gtk::adjustment-get-value gtk::adjustment-set-value gtk::style-get-type
   gtk::style-new gtk::style-copy gtk::style-attach gtk::style-detach
   gtk::style-set-background gtk::style-apply-default-background
   gtk::style-lookup-icon-set gtk::style-render-icon gtk::draw-check
   gtk::paint-hline gtk::paint-vline gtk::paint-shadow gtk::paint-polygon
   gtk::paint-arrow gtk::paint-diamond gtk::paint-box gtk::paint-flat-box
   gtk::paint-check gtk::paint-option gtk::paint-tab gtk::paint-shadow-gap
   gtk::paint-box-gap gtk::paint-extension gtk::paint-focus gtk::paint-slider
   gtk::paint-handle gtk::paint-expander gtk::paint-layout
   gtk::paint-resize-grip gtk::border-get-type gtk::border-copy
   gtk::border-free gtk::rc-add-default-file gtk::rc-set-default-files
   gtk::rc-get-default-files gtk::rc-get-style gtk::rc-get-style-by-paths
   gtk::rc-reparse-all-for-settings gtk::rc-find-pixmap-in-path gtk::rc-parse
   gtk::rc-parse-string gtk::rc-reparse-all gtk::rc-style-get-type
   gtk::rc-style-new gtk::rc-style-copy gtk::rc-style-ref gtk::rc-style-unref
   gtk::rc-find-module-in-path gtk::rc-get-theme-dir gtk::rc-get-module-dir
   gtk::rc-get-im-module-path gtk::rc-get-im-module-file gtk::rc-scanner-new
   gtk::rc-parse-color gtk::rc-parse-state gtk::rc-parse-priority
   gtk::settings-get-type gtk::settings-get-default
   gtk::settings-install-property gtk::settings-install-property-parser
   gtk::rc-property-parse-color gtk::rc-property-parse-enum
   gtk::rc-property-parse-flags gtk::rc-property-parse-requisition
   gtk::rc-property-parse-border gtk::settings-set-property-value
   gtk::settings-set-string-property gtk::settings-set-long-property
   gtk::settings-set-double-property gtk::widget-get-type gtk::widget-new
   gtk::widget-ref gtk::widget-unref gtk::widget-destroy gtk::widget-destroyed
   gtk::widget-set gtk::widget-unparent gtk::widget-show gtk::widget-show-now
   gtk::widget-hide gtk::widget-show-all gtk::widget-hide-all gtk::widget-map
   gtk::widget-unmap gtk::widget-realize gtk::widget-unrealize
   gtk::widget-queue-draw gtk::widget-queue-draw-area gtk::widget-queue-resize
   gtk::widget-size-request gtk::widget-size-allocate
   gtk::widget-get-child-requisition gtk::widget-add-accelerator
   gtk::widget-remove-accelerator gtk::widget-set-accel-path
   gtk::widget-list-accel-closures gtk::widget-mnemonic-activate
   gtk::widget-event gtk::widget-send-expose gtk::widget-activate
   gtk::widget-set-scroll-adjustments gtk::widget-reparent
   gtk::widget-intersect gtk::widget-region-intersect
   gtk::widget-freeze-child-notify gtk::widget-child-notify
   gtk::widget-thaw-child-notify gtk::widget-is-focus gtk::widget-grab-focus
   gtk::widget-grab-default gtk::widget-set-name gtk::widget-get-name
   gtk::widget-set-state gtk::widget-set-sensitive
   gtk::widget-set-app-paintable gtk::widget-set-double-buffered
   gtk::widget-set-redraw-on-allocate gtk::widget-set-parent
   gtk::widget-set-parent-window gtk::widget-set-child-visible
   gtk::widget-get-child-visible gtk::widget-get-parent
   gtk::widget-get-parent-window gtk::widget-child-focus
   gtk::widget-set-size-request gtk::widget-get-size-request
   gtk::widget-set-events gtk::widget-add-events
   gtk::widget-set-extension-events gtk::widget-get-extension-events
   gtk::widget-get-toplevel gtk::widget-get-ancestor gtk::widget-get-colormap
   gtk::widget-get-visual gtk::widget-get-settings gtk::widget-get-accessible
   gtk::widget-set-colormap gtk::widget-get-events gtk::widget-get-pointer
   gtk::widget-is-ancestor gtk::widget-translate-coordinates
   gtk::widget-hide-on-delete gtk::widget-set-style gtk::widget-ensure-style
   gtk::widget-get-style gtk::widget-modify-style
   gtk::widget-get-modifier-style gtk::widget-modify-fg gtk::widget-modify-bg
   gtk::widget-modify-text gtk::widget-modify-base gtk::widget-modify-font
   gtk::widget-create-pango-context gtk::widget-get-pango-context
   gtk::widget-create-pango-layout gtk::widget-render-icon
   gtk::widget-set-composite-name gtk::widget-get-composite-name
   gtk::widget-reset-rc-styles gtk::widget-push-colormap
   gtk::widget-push-composite-child gtk::widget-pop-composite-child
   gtk::widget-pop-colormap gtk::widget-class-install-style-property
   gtk::widget-class-install-style-property-parser
   gtk::widget-style-get-property gtk::widget-style-get-valist
   gtk::widget-style-get gtk::widget-set-default-colormap
   gtk::widget-get-default-style gtk::widget-get-default-colormap
   gtk::widget-get-default-visual gtk::widget-set-direction
   gtk::widget-get-direction gtk::widget-set-default-direction
   gtk::widget-get-default-direction gtk::widget-shape-combine-mask
   gtk::widget-reset-shapes gtk::widget-path gtk::widget-class-path
   gtk::requisition-get-type gtk::requisition-copy gtk::requisition-free
   gtk::misc-get-type gtk::misc-set-alignment gtk::misc-get-alignment
   gtk::misc-set-padding gtk::misc-get-padding gtk::container-get-type
   gtk::container-set-border-width gtk::container-get-border-width
   gtk::container-add gtk::container-remove gtk::container-set-resize-mode
   gtk::container-get-resize-mode gtk::container-check-resize
   gtk::container-foreach gtk::container-get-children
   gtk::container-propagate-expose gtk::container-set-focus-chain
   gtk::container-get-focus-chain gtk::container-unset-focus-chain
   gtk::container-set-reallocate-redraws gtk::container-set-focus-child
   gtk::container-set-focus-vadjustment gtk::container-get-focus-vadjustment
   gtk::container-set-focus-hadjustment gtk::container-get-focus-hadjustment
   gtk::container-resize-children gtk::container-child-type
   gtk::container-class-install-child-property
   gtk::container-class-find-child-property
   gtk::container-class-list-child-properties
   gtk::container-add-with-properties gtk::container-child-set
   gtk::container-child-get gtk::container-child-set-valist
   gtk::container-child-get-valist gtk::container-child-set-property
   gtk::container-child-get-property gtk::container-forall gtk::bin-get-type
   gtk::bin-get-child gtk::window-get-type gtk::window-new
   gtk::window-set-title gtk::window-get-title gtk::window-set-wmclass
   gtk::window-set-role gtk::window-get-role gtk::window-add-accel-group
   gtk::window-remove-accel-group gtk::window-set-position
   gtk::window-activate-focus gtk::window-set-focus gtk::window-get-focus
   gtk::window-set-default gtk::window-activate-default
   gtk::window-set-transient-for gtk::window-get-transient-for
   gtk::window-set-type-hint gtk::window-get-type-hint
   gtk::window-set-destroy-with-parent gtk::window-get-destroy-with-parent
   gtk::window-set-resizable gtk::window-get-resizable gtk::window-set-gravity
   gtk::window-get-gravity gtk::window-set-geometry-hints
   gtk::window-set-has-frame gtk::window-get-has-frame
   gtk::window-set-frame-dimensions gtk::window-get-frame-dimensions
   gtk::window-set-decorated gtk::window-get-decorated
   gtk::window-set-icon-list gtk::window-get-icon-list gtk::window-set-icon
   gtk::window-get-icon gtk::window-set-default-icon-list
   gtk::window-get-default-icon-list gtk::window-set-modal
   gtk::window-get-modal gtk::window-list-toplevels gtk::window-add-mnemonic
   gtk::window-remove-mnemonic gtk::window-mnemonic-activate
   gtk::window-set-mnemonic-modifier gtk::window-get-mnemonic-modifier
   gtk::window-present gtk::window-iconify gtk::window-deiconify
   gtk::window-stick gtk::window-unstick gtk::window-maximize
   gtk::window-unmaximize gtk::window-begin-resize-drag
   gtk::window-begin-move-drag gtk::window-set-default-size
   gtk::window-get-default-size gtk::window-resize gtk::window-get-size
   gtk::window-move gtk::window-get-position gtk::window-parse-geometry
   gtk::window-reshow-with-initial-size gtk::window-group-get-type
   gtk::window-group-new gtk::window-group-add-window
   gtk::window-group-remove-window gtk::window-remove-embedded-xid
   gtk::window-add-embedded-xid gtk::menu-shell-get-type gtk::menu-shell-append
   gtk::menu-shell-prepend gtk::menu-shell-insert gtk::menu-shell-deactivate
   gtk::menu-shell-select-item gtk::menu-shell-deselect
   gtk::menu-shell-activate-item gtk::menu-get-type gtk::menu-new
   gtk::menu-popup gtk::menu-reposition gtk::menu-popdown gtk::menu-get-active
   gtk::menu-set-active gtk::menu-set-accel-group gtk::menu-get-accel-group
   gtk::menu-set-accel-path gtk::menu-attach-to-widget gtk::menu-detach
   gtk::menu-get-attach-widget gtk::menu-set-tearoff-state
   gtk::menu-get-tearoff-state gtk::menu-set-title gtk::menu-get-title
   gtk::menu-reorder-child gtk::label-get-type gtk::label-new
   gtk::label-new-with-mnemonic gtk::label-set-text gtk::label-get-text
   gtk::label-set-attributes gtk::label-get-attributes gtk::label-set-label
   gtk::label-get-label gtk::label-set-markup gtk::label-set-use-markup
   gtk::label-get-use-markup gtk::label-set-use-underline
   gtk::label-get-use-underline gtk::label-set-markup-with-mnemonic
   gtk::label-get-mnemonic-keyval gtk::label-set-mnemonic-widget
   gtk::label-get-mnemonic-widget gtk::label-set-text-with-mnemonic
   gtk::label-set-justify gtk::label-get-justify gtk::label-set-pattern
   gtk::label-set-line-wrap gtk::label-get-line-wrap gtk::label-set-selectable
   gtk::label-get-selectable gtk::label-select-region
   gtk::label-get-selection-bounds gtk::label-get-layout
   gtk::label-get-layout-offsets gtk::accel-label-get-type gtk::accel-label-new
   gtk::accel-label-get-accel-widget gtk::accel-label-get-accel-width
   gtk::accel-label-set-accel-widget gtk::accel-label-set-accel-closure
   gtk::accel-label-refetch gtk::accel-map-lookup-entry
   gtk::accel-map-change-entry gtk::accel-map-load gtk::accel-map-save
   gtk::accel-map-foreach gtk::accel-map-load-fd gtk::accel-map-load-scanner
   gtk::accel-map-save-fd gtk::accel-map-add-filter
   gtk::accel-map-foreach-unfiltered gtk::accessible-get-type
   gtk::accessible-connect-widget-destroyed gtk::alignment-get-type
   gtk::alignment-new gtk::alignment-set gtk::frame-get-type gtk::frame-new
   gtk::frame-set-label gtk::frame-get-label gtk::frame-set-label-widget
   gtk::frame-get-label-widget gtk::frame-set-label-align
   gtk::frame-get-label-align gtk::frame-set-shadow-type
   gtk::frame-get-shadow-type gtk::aspect-frame-get-type gtk::aspect-frame-new
   gtk::aspect-frame-set gtk::arrow-get-type gtk::arrow-new gtk::arrow-set
   gtk::binding-set-new gtk::binding-set-by-class gtk::binding-set-find
   gtk::bindings-activate gtk::binding-set-activate gtk::binding-entry-clear
   gtk::binding-entry-add-signal gtk::binding-set-add-path
   gtk::binding-entry-remove gtk::binding-entry-add-signall
   gtk::binding-parse-binding gtk::box-get-type gtk::box-pack-start
   gtk::box-pack-end gtk::box-set-homogeneous gtk::box-get-homogeneous
   gtk::box-set-spacing gtk::box-get-spacing gtk::box-reorder-child
   gtk::box-query-child-packing gtk::box-set-child-packing
   gtk::button-box-get-type gtk::button-box-get-layout
   gtk::button-box-set-layout gtk::button-box-set-child-secondary
   gtk::button-get-type gtk::button-new gtk::button-new-with-label
   gtk::button-new-from-stock gtk::button-new-with-mnemonic gtk::button-pressed
   gtk::button-released gtk::button-clicked gtk::button-enter gtk::button-leave
   gtk::button-set-relief gtk::button-get-relief gtk::button-set-label
   gtk::button-get-label gtk::button-set-use-underline
   gtk::button-get-use-underline gtk::button-set-use-stock
   gtk::button-get-use-stock gtk::calendar-get-type gtk::calendar-new
   gtk::calendar-select-month gtk::calendar-select-day gtk::calendar-mark-day
   gtk::calendar-unmark-day gtk::calendar-clear-marks
   gtk::calendar-display-options gtk::calendar-get-date gtk::calendar-freeze
   gtk::calendar-thaw gtk::cell-editable-get-type
   gtk::cell-editable-start-editing gtk::cell-editable-editing-done
   gtk::cell-editable-remove-widget gtk::cell-renderer-get-type
   gtk::cell-renderer-get-size gtk::cell-renderer-render
   gtk::cell-renderer-activate gtk::cell-renderer-start-editing
   gtk::cell-renderer-set-fixed-size gtk::cell-renderer-get-fixed-size
   gtk::cell-renderer-text-get-type gtk::cell-renderer-text-new
   gtk::cell-renderer-text-set-fixed-height-from-font
   gtk::cell-renderer-toggle-get-type gtk::cell-renderer-toggle-new
   gtk::cell-renderer-toggle-get-radio gtk::cell-renderer-toggle-set-radio
   gtk::cell-renderer-toggle-get-active gtk::cell-renderer-toggle-set-active
   gtk::cell-renderer-pixbuf-get-type gtk::cell-renderer-pixbuf-new
   gtk::toggle-button-get-type gtk::toggle-button-new
   gtk::toggle-button-new-with-label gtk::toggle-button-new-with-mnemonic
   gtk::toggle-button-set-mode gtk::toggle-button-get-mode
   gtk::toggle-button-set-active gtk::toggle-button-get-active
   gtk::toggle-button-toggled gtk::toggle-button-set-inconsistent
   gtk::toggle-button-get-inconsistent gtk::check-button-get-type
   gtk::check-button-new gtk::check-button-new-with-label
   gtk::check-button-new-with-mnemonic gtk::item-get-type gtk::item-select
   gtk::item-deselect gtk::item-toggle gtk::menu-item-get-type
   gtk::menu-item-new gtk::menu-item-new-with-label
   gtk::menu-item-new-with-mnemonic gtk::menu-item-set-submenu
   gtk::menu-item-get-submenu gtk::menu-item-remove-submenu
   gtk::menu-item-select gtk::menu-item-deselect gtk::menu-item-activate
   gtk::menu-item-toggle-size-request gtk::menu-item-toggle-size-allocate
   gtk::menu-item-set-right-justified gtk::menu-item-get-right-justified
   gtk::menu-item-set-accel-path gtk::check-menu-item-get-type
   gtk::check-menu-item-new gtk::check-menu-item-new-with-label
   gtk::check-menu-item-new-with-mnemonic gtk::check-menu-item-set-active
   gtk::check-menu-item-get-active gtk::check-menu-item-toggled
   gtk::check-menu-item-set-inconsistent gtk::check-menu-item-get-inconsistent
   gtk::target-list-new gtk::target-list-ref gtk::target-list-unref
   gtk::target-list-add gtk::target-list-add-table gtk::target-list-remove
   gtk::target-list-find gtk::selection-owner-set gtk::selection-add-target
   gtk::selection-add-targets gtk::selection-clear-targets
   gtk::selection-convert gtk::selection-data-set gtk::selection-data-set-text
   gtk::selection-data-get-text gtk::selection-data-get-targets
   gtk::selection-data-targets-include-text gtk::selection-remove-all
   gtk::selection-clear gtk::selection-data-copy gtk::selection-data-free
   gtk::clipboard-set-with-data gtk::clipboard-set-with-owner
   gtk::clipboard-get-owner gtk::clipboard-clear gtk::clipboard-set-text
   gtk::clipboard-request-contents gtk::clipboard-request-text
   gtk::clipboard-wait-for-contents gtk::clipboard-wait-for-text
   gtk::clipboard-wait-is-text-available gtk::range-get-type
   gtk::range-set-update-policy gtk::range-get-update-policy
   gtk::range-set-adjustment gtk::range-get-adjustment gtk::range-set-inverted
   gtk::range-get-inverted gtk::range-set-increments gtk::range-set-range
   gtk::range-set-value gtk::range-get-value gtk::scrollbar-get-type
   gtk::hscrollbar-get-type gtk::hscrollbar-new gtk::vscrollbar-get-type
   gtk::vscrollbar-new gtk::clist-get-type gtk::clist-set-hadjustment
   gtk::clist-set-vadjustment gtk::clist-get-hadjustment
   gtk::clist-get-vadjustment gtk::clist-set-shadow-type
   gtk::clist-set-selection-mode gtk::clist-set-reorderable
   gtk::clist-set-use-drag-icons gtk::clist-set-button-actions
   gtk::clist-freeze gtk::clist-thaw gtk::clist-column-titles-show
   gtk::clist-column-titles-hide gtk::clist-column-title-active
   gtk::clist-column-title-passive gtk::clist-column-titles-active
   gtk::clist-column-titles-passive gtk::clist-set-column-title
   gtk::clist-get-column-title gtk::clist-set-column-widget
   gtk::clist-get-column-widget gtk::clist-set-column-justification
   gtk::clist-set-column-visibility gtk::clist-set-column-resizeable
   gtk::clist-set-column-auto-resize gtk::clist-columns-autosize
   gtk::clist-optimal-column-width gtk::clist-set-column-width
   gtk::clist-set-column-min-width gtk::clist-set-column-max-width
   gtk::clist-set-row-height gtk::clist-moveto gtk::clist-row-is-visible
   gtk::clist-get-cell-type gtk::clist-set-text gtk::clist-get-text
   gtk::clist-set-pixmap gtk::clist-get-pixmap gtk::clist-set-pixtext
   gtk::clist-get-pixtext gtk::clist-set-foreground gtk::clist-set-background
   gtk::clist-set-cell-style gtk::clist-get-cell-style gtk::clist-set-row-style
   gtk::clist-get-row-style gtk::clist-set-shift gtk::clist-set-selectable
   gtk::clist-get-selectable gtk::clist-prepend gtk::clist-append
   gtk::clist-insert gtk::clist-remove gtk::clist-set-row-data
   gtk::clist-set-row-data-full gtk::clist-get-row-data
   gtk::clist-find-row-from-data gtk::clist-select-row gtk::clist-unselect-row
   gtk::clist-undo-selection gtk::clist-clear gtk::clist-get-selection-info
   gtk::clist-select-all gtk::clist-unselect-all gtk::clist-swap-rows
   gtk::clist-row-move gtk::clist-set-compare-func gtk::clist-set-sort-column
   gtk::clist-set-sort-type gtk::clist-sort gtk::clist-set-auto-sort
   gtk::dialog-get-type gtk::dialog-new gtk::dialog-new-with-buttons
   gtk::dialog-add-action-widget gtk::dialog-add-button gtk::dialog-add-buttons
   gtk::dialog-set-response-sensitive gtk::dialog-set-default-response
   gtk::dialog-set-has-separator gtk::dialog-get-has-separator
   gtk::dialog-response gtk::dialog-run gtk::vbox-get-type gtk::vbox-new
   gtk::color-selection-get-type gtk::color-selection-new
   gtk::color-selection-get-has-opacity-control
   gtk::color-selection-set-has-opacity-control
   gtk::color-selection-get-has-palette gtk::color-selection-set-has-palette
   gtk::color-selection-set-current-color
   gtk::color-selection-set-current-alpha
   gtk::color-selection-get-current-color
   gtk::color-selection-get-current-alpha
   gtk::color-selection-set-previous-color
   gtk::color-selection-set-previous-alpha
   gtk::color-selection-get-previous-color
   gtk::color-selection-get-previous-alpha gtk::color-selection-is-adjusting
   gtk::color-selection-palette-from-string
   gtk::color-selection-palette-to-string
   gtk::color-selection-set-change-palette-hook
   gtk::color-selection-dialog-get-type gtk::color-selection-dialog-new
   gtk::hbox-get-type gtk::hbox-new gtk::combo-get-type gtk::combo-new
   gtk::combo-set-value-in-list gtk::combo-set-use-arrows
   gtk::combo-set-use-arrows-always gtk::combo-set-case-sensitive
   gtk::combo-set-item-string gtk::combo-set-popdown-strings
   gtk::combo-disable-activate gtk::ctree-get-type gtk::ctree-insert-node
   gtk::ctree-remove-node gtk::ctree-insert-gnode gtk::ctree-export-to-gnode
   gtk::ctree-post-recursive gtk::ctree-post-recursive-to-depth
   gtk::ctree-pre-recursive gtk::ctree-pre-recursive-to-depth
   gtk::ctree-is-viewable gtk::ctree-last gtk::ctree-find-node-ptr
   gtk::ctree-node-nth gtk::ctree-find gtk::ctree-is-ancestor
   gtk::ctree-find-by-row-data gtk::ctree-find-all-by-row-data
   gtk::ctree-find-by-row-data-custom gtk::ctree-find-all-by-row-data-custom
   gtk::ctree-is-hot-spot gtk::ctree-move gtk::ctree-expand
   gtk::ctree-expand-recursive gtk::ctree-expand-to-depth gtk::ctree-collapse
   gtk::ctree-collapse-recursive gtk::ctree-collapse-to-depth
   gtk::ctree-toggle-expansion gtk::ctree-toggle-expansion-recursive
   gtk::ctree-select gtk::ctree-select-recursive gtk::ctree-unselect
   gtk::ctree-unselect-recursive gtk::ctree-real-select-recursive
   gtk::ctree-node-set-text gtk::ctree-node-set-pixmap
   gtk::ctree-node-set-pixtext gtk::ctree-set-node-info
   gtk::ctree-node-set-shift gtk::ctree-node-set-selectable
   gtk::ctree-node-get-selectable gtk::ctree-node-get-cell-type
   gtk::ctree-node-get-text gtk::ctree-node-get-pixmap
   gtk::ctree-node-get-pixtext gtk::ctree-get-node-info
   gtk::ctree-node-set-row-style gtk::ctree-node-get-row-style
   gtk::ctree-node-set-cell-style gtk::ctree-node-get-cell-style
   gtk::ctree-node-set-foreground gtk::ctree-node-set-background
   gtk::ctree-node-set-row-data gtk::ctree-node-set-row-data-full
   gtk::ctree-node-get-row-data gtk::ctree-node-moveto
   gtk::ctree-node-is-visible gtk::ctree-set-indent gtk::ctree-set-spacing
   gtk::ctree-set-show-stub gtk::ctree-set-line-style
   gtk::ctree-set-expander-style gtk::ctree-set-drag-compare-func
   gtk::ctree-sort-node gtk::ctree-sort-recursive gtk::ctree-node-get-type
   gtk::drawing-area-get-type gtk::drawing-area-new gtk::curve-get-type
   gtk::curve-new gtk::curve-reset gtk::curve-set-gamma gtk::curve-set-range
   gtk::curve-get-vector gtk::curve-set-vector gtk::curve-set-curve-type
   gtk::drag-get-data gtk::drag-finish gtk::drag-get-source-widget
   gtk::drag-highlight gtk::drag-unhighlight gtk::drag-dest-set
   gtk::drag-dest-set-proxy gtk::drag-dest-unset gtk::drag-dest-find-target
   gtk::drag-dest-get-target-list gtk::drag-dest-set-target-list
   gtk::drag-source-set gtk::drag-source-unset gtk::drag-source-set-icon
   gtk::drag-source-set-icon-pixbuf gtk::drag-source-set-icon-stock
   gtk::drag-begin gtk::drag-set-icon-widget gtk::drag-set-icon-pixmap
   gtk::drag-set-icon-pixbuf gtk::drag-set-icon-stock
   gtk::drag-set-icon-default gtk::drag-check-threshold gtk::editable-get-type
   gtk::editable-select-region gtk::editable-get-selection-bounds
   gtk::editable-insert-text gtk::editable-delete-text gtk::editable-get-chars
   gtk::editable-cut-clipboard gtk::editable-copy-clipboard
   gtk::editable-paste-clipboard gtk::editable-delete-selection
   gtk::editable-set-position gtk::editable-get-position
   gtk::editable-set-editable gtk::editable-get-editable
   gtk::im-context-get-type gtk::im-context-set-client-window
   gtk::im-context-get-preedit-string gtk::im-context-filter-keypress
   gtk::im-context-focus-in gtk::im-context-focus-out gtk::im-context-reset
   gtk::im-context-set-cursor-location gtk::im-context-set-use-preedit
   gtk::im-context-set-surrounding gtk::im-context-get-surrounding
   gtk::im-context-delete-surrounding gtk::entry-get-type gtk::entry-new
   gtk::entry-set-visibility gtk::entry-get-visibility
   gtk::entry-set-invisible-char gtk::entry-get-invisible-char
   gtk::entry-set-has-frame gtk::entry-get-has-frame gtk::entry-set-max-length
   gtk::entry-get-max-length gtk::entry-set-activates-default
   gtk::entry-get-activates-default gtk::entry-set-width-chars
   gtk::entry-get-width-chars gtk::entry-set-text gtk::entry-get-text
   gtk::entry-get-layout gtk::entry-get-layout-offsets
   gtk::entry-new-with-max-length gtk::event-box-get-type gtk::event-box-new
   gtk::file-selection-get-type gtk::file-selection-new
   gtk::file-selection-set-filename gtk::file-selection-get-filename
   gtk::file-selection-complete gtk::file-selection-show-fileop-buttons
   gtk::file-selection-hide-fileop-buttons gtk::file-selection-get-selections
   gtk::file-selection-set-select-multiple
   gtk::file-selection-get-select-multiple gtk::fixed-get-type gtk::fixed-new
   gtk::fixed-put gtk::fixed-move gtk::fixed-set-has-window
   gtk::fixed-get-has-window gtk::font-selection-get-type
   gtk::font-selection-new gtk::font-selection-get-font-name
   gtk::font-selection-set-font-name gtk::font-selection-get-preview-text
   gtk::font-selection-set-preview-text gtk::font-selection-dialog-get-type
   gtk::font-selection-dialog-new gtk::font-selection-dialog-get-font-name
   gtk::font-selection-dialog-set-font-name
   gtk::font-selection-dialog-get-preview-text
   gtk::font-selection-dialog-set-preview-text gtk::gamma-curve-get-type
   gtk::gamma-curve-new gtk::gc-get gtk::gc-release gtk::handle-box-get-type
   gtk::handle-box-new gtk::handle-box-set-shadow-type
   gtk::handle-box-get-shadow-type gtk::handle-box-set-handle-position
   gtk::handle-box-get-handle-position gtk::handle-box-set-snap-edge
   gtk::handle-box-get-snap-edge gtk::hbutton-box-get-type gtk::hbutton-box-new
   gtk::paned-get-type gtk::paned-add1 gtk::paned-add2 gtk::paned-pack1
   gtk::paned-pack2 gtk::paned-get-position gtk::paned-set-position
   gtk::paned-compute-position gtk::hpaned-get-type gtk::hpaned-new
   gtk::ruler-get-type gtk::ruler-set-metric gtk::ruler-set-range
   gtk::ruler-draw-ticks gtk::ruler-draw-pos gtk::ruler-get-metric
   gtk::ruler-get-range gtk::hruler-get-type gtk::hruler-new
   gtk::scale-get-type gtk::scale-set-digits gtk::scale-get-digits
   gtk::scale-set-draw-value gtk::scale-get-draw-value gtk::scale-set-value-pos
   gtk::scale-get-value-pos gtk::hscale-get-type gtk::hscale-new
   gtk::hscale-new-with-range gtk::separator-get-type gtk::hseparator-get-type
   gtk::hseparator-new gtk::icon-factory-get-type gtk::icon-factory-new
   gtk::icon-factory-add gtk::icon-factory-lookup gtk::icon-factory-add-default
   gtk::icon-factory-remove-default gtk::icon-factory-lookup-default
   gtk::icon-size-lookup gtk::icon-size-register gtk::icon-size-register-alias
   gtk::icon-size-from-name gtk::icon-size-get-name gtk::icon-set-new
   gtk::icon-set-new-from-pixbuf gtk::icon-set-ref gtk::icon-set-unref
   gtk::icon-set-copy gtk::icon-set-render-icon gtk::icon-set-add-source
   gtk::icon-set-get-sizes gtk::icon-source-get-type gtk::icon-source-new
   gtk::icon-source-copy gtk::icon-source-free gtk::icon-source-set-filename
   gtk::icon-source-set-pixbuf gtk::icon-source-get-filename
   gtk::icon-source-get-pixbuf gtk::icon-source-set-direction-wildcarded
   gtk::icon-source-set-state-wildcarded gtk::icon-source-set-size-wildcarded
   gtk::icon-source-get-size-wildcarded gtk::icon-source-get-state-wildcarded
   gtk::icon-source-get-direction-wildcarded gtk::icon-source-set-direction
   gtk::icon-source-set-state gtk::icon-source-set-size
   gtk::icon-source-get-direction gtk::icon-source-get-state
   gtk::icon-source-get-size gtk::image-get-type gtk::image-new
   gtk::image-new-from-pixmap gtk::image-new-from-image
   gtk::image-new-from-file gtk::image-new-from-pixbuf
   gtk::image-new-from-stock gtk::image-new-from-icon-set
   gtk::image-new-from-animation gtk::image-set-from-pixmap
   gtk::image-set-from-image gtk::image-set-from-file
   gtk::image-set-from-pixbuf gtk::image-set-from-stock
   gtk::image-set-from-icon-set gtk::image-set-from-animation
   gtk::image-get-storage-type gtk::image-get-pixmap gtk::image-get-image
   gtk::image-get-pixbuf gtk::image-get-stock gtk::image-get-icon-set
   gtk::image-get-animation gtk::image-menu-item-get-type
   gtk::image-menu-item-new gtk::image-menu-item-new-with-label
   gtk::image-menu-item-new-with-mnemonic gtk::image-menu-item-new-from-stock
   gtk::image-menu-item-set-image gtk::image-menu-item-get-image
   gtk::im-context-simple-get-type gtk::im-context-simple-new
   gtk::im-context-simple-add-table gtk::im-multicontext-get-type
   gtk::im-multicontext-new gtk::im-multicontext-append-menuitems
   gtk::input-dialog-get-type gtk::input-dialog-new gtk::invisible-get-type
   gtk::invisible-new gtk::item-factory-get-type gtk::item-factory-new
   gtk::item-factory-construct gtk::item-factory-add-foreign
   gtk::item-factory-from-widget gtk::item-factory-path-from-widget
   gtk::item-factory-get-item gtk::item-factory-get-widget
   gtk::item-factory-get-widget-by-action gtk::item-factory-get-item-by-action
   gtk::item-factory-create-item gtk::item-factory-create-items
   gtk::item-factory-delete-item gtk::item-factory-delete-entry
   gtk::item-factory-delete-entries gtk::item-factory-popup
   gtk::item-factory-popup-with-data gtk::item-factory-popup-data
   gtk::item-factory-popup-data-from-widget
   gtk::item-factory-set-translate-func gtk::layout-get-type gtk::layout-new
   gtk::layout-put gtk::layout-move gtk::layout-set-size gtk::layout-get-size
   gtk::layout-get-hadjustment gtk::layout-get-vadjustment
   gtk::layout-set-hadjustment gtk::layout-set-vadjustment
   gtk::list-item-get-type gtk::list-item-new gtk::list-item-new-with-label
   gtk::list-item-select gtk::list-item-deselect gtk::list-insert-items
   gtk::list-append-items gtk::list-prepend-items gtk::list-remove-items
   gtk::list-clear-items gtk::list-select-item gtk::list-unselect-item
   gtk::list-select-child gtk::list-unselect-child gtk::list-child-position
   gtk::tree-path-new gtk::tree-path-new-from-string gtk::tree-path-to-string
   gtk::tree-path-new-first gtk::tree-path-append-index
   gtk::tree-path-prepend-index gtk::tree-path-get-depth
   gtk::tree-path-get-indices gtk::tree-path-free gtk::tree-path-copy
   gtk::tree-path-compare gtk::tree-path-next gtk::tree-path-prev
   gtk::tree-path-up gtk::tree-path-down gtk::tree-path-is-ancestor
   gtk::tree-path-is-descendant gtk::tree-row-reference-new
   gtk::tree-row-reference-new-proxy gtk::tree-row-reference-get-path
   gtk::tree-row-reference-valid gtk::tree-row-reference-free
   gtk::tree-row-reference-inserted gtk::tree-row-reference-deleted
   gtk::tree-row-reference-reordered gtk::tree-iter-copy gtk::tree-iter-free
   gtk::tree-iter-get-type gtk::tree-model-get-type gtk::tree-model-get-flags
   gtk::tree-model-get-n-columns gtk::tree-model-get-column-type
   gtk::tree-model-get-iter gtk::tree-model-get-iter-from-string
   gtk::tree-model-get-iter-first gtk::tree-model-get-path
   gtk::tree-model-get-value gtk::tree-model-iter-next
   gtk::tree-model-iter-children gtk::tree-model-iter-has-child
   gtk::tree-model-iter-n-children gtk::tree-model-iter-nth-child
   gtk::tree-model-iter-parent gtk::tree-model-ref-node
   gtk::tree-model-unref-node gtk::tree-model-get gtk::tree-model-get-valist
   gtk::tree-model-foreach gtk::tree-model-row-changed
   gtk::tree-model-row-inserted gtk::tree-model-row-has-child-toggled
   gtk::tree-model-row-deleted gtk::tree-model-rows-reordered
   gtk::tree-sortable-get-type gtk::tree-sortable-sort-column-changed
   gtk::tree-sortable-get-sort-column-id gtk::tree-sortable-set-sort-column-id
   gtk::tree-sortable-set-sort-func gtk::tree-sortable-set-default-sort-func
   gtk::tree-sortable-has-default-sort-func gtk::list-store-get-type
   gtk::list-store-new gtk::list-store-newv gtk::list-store-set-column-types
   gtk::list-store-set-value gtk::list-store-set gtk::list-store-set-valist
   gtk::list-store-remove gtk::list-store-insert gtk::list-store-insert-before
   gtk::list-store-insert-after gtk::list-store-prepend gtk::list-store-append
   gtk::list-store-clear gtk::check-version gtk::init gtk::init-check
   gtk::disable-setlocale gtk::set-locale gtk::get-default-language
   gtk::events-pending gtk::main-do-event gtk::main gtk::main-level
   gtk::main-quit gtk::main-iteration gtk::main-iteration-do gtk::true
   gtk::false gtk::grab-add gtk::grab-get-current gtk::grab-remove
   gtk::init-add gtk::quit-add-destroy gtk::quit-add gtk::quit-add-full
   gtk::quit-remove gtk::quit-remove-by-data gtk::timeout-add
   gtk::timeout-add-full gtk::timeout-remove gtk::idle-add
   gtk::idle-add-priority gtk::idle-add-full gtk::idle-remove
   gtk::idle-remove-by-data gtk::input-add-full gtk::input-remove
   gtk::key-snooper-install gtk::key-snooper-remove gtk::get-current-event
   gtk::get-current-event-time gtk::get-current-event-state
   gtk::get-event-widget gtk::propagate-event gtk::menu-bar-get-type
   gtk::menu-bar-new gtk::message-dialog-get-type gtk::message-dialog-new
   gtk::notebook-get-type gtk::notebook-new gtk::notebook-append-page
   gtk::notebook-append-page-menu gtk::notebook-prepend-page
   gtk::notebook-prepend-page-menu gtk::notebook-insert-page
   gtk::notebook-insert-page-menu gtk::notebook-remove-page
   gtk::notebook-get-current-page gtk::notebook-get-nth-page
   gtk::notebook-page-num gtk::notebook-set-current-page
   gtk::notebook-next-page gtk::notebook-prev-page
   gtk::notebook-set-show-border gtk::notebook-get-show-border
   gtk::notebook-set-show-tabs gtk::notebook-get-show-tabs
   gtk::notebook-set-tab-pos gtk::notebook-get-tab-pos
   gtk::notebook-set-scrollable gtk::notebook-get-scrollable
   gtk::notebook-popup-enable gtk::notebook-popup-disable
   gtk::notebook-get-tab-label gtk::notebook-set-tab-label
   gtk::notebook-set-tab-label-text gtk::notebook-get-tab-label-text
   gtk::notebook-get-menu-label gtk::notebook-set-menu-label
   gtk::notebook-set-menu-label-text gtk::notebook-get-menu-label-text
   gtk::notebook-query-tab-label-packing gtk::notebook-set-tab-label-packing
   gtk::notebook-reorder-child gtk::old-editable-get-type
   gtk::old-editable-claim-selection gtk::old-editable-changed
   gtk::option-menu-get-type gtk::option-menu-new gtk::option-menu-get-menu
   gtk::option-menu-set-menu gtk::option-menu-remove-menu
   gtk::option-menu-get-history gtk::option-menu-set-history
   gtk::pixmap-get-type gtk::pixmap-set gtk::pixmap-get
   gtk::pixmap-set-build-insensitive gtk::socket-get-type gtk::socket-new
   gtk::socket-add-id gtk::socket-get-id gtk::plug-get-type gtk::plug-construct
   gtk::plug-new gtk::plug-get-id gtk::preview-get-type gtk::preview-size
   gtk::preview-put gtk::preview-draw-row gtk::preview-set-expand
   gtk::progress-get-type gtk::progress-bar-get-type gtk::progress-bar-new
   gtk::progress-bar-pulse gtk::progress-bar-set-text
   gtk::progress-bar-set-fraction gtk::progress-bar-set-pulse-step
   gtk::progress-bar-set-orientation gtk::progress-bar-get-text
   gtk::progress-bar-get-fraction gtk::progress-bar-get-pulse-step
   gtk::progress-bar-get-orientation gtk::radio-button-get-type
   gtk::radio-button-new gtk::radio-button-new-from-widget
   gtk::radio-button-new-with-label
   gtk::radio-button-new-with-label-from-widget
   gtk::radio-button-new-with-mnemonic
   gtk::radio-button-new-with-mnemonic-from-widget gtk::radio-button-get-group
   gtk::radio-button-set-group gtk::radio-menu-item-get-type
   gtk::radio-menu-item-new gtk::radio-menu-item-new-with-label
   gtk::radio-menu-item-new-with-mnemonic gtk::radio-menu-item-get-group
   gtk::radio-menu-item-set-group gtk::viewport-get-type gtk::viewport-new
   gtk::viewport-get-hadjustment gtk::viewport-get-vadjustment
   gtk::viewport-set-hadjustment gtk::viewport-set-vadjustment
   gtk::viewport-set-shadow-type gtk::viewport-get-shadow-type
   gtk::scrolled-window-get-type gtk::scrolled-window-new
   gtk::scrolled-window-set-hadjustment gtk::scrolled-window-set-vadjustment
   gtk::scrolled-window-get-hadjustment gtk::scrolled-window-get-vadjustment
   gtk::scrolled-window-set-policy gtk::scrolled-window-get-policy
   gtk::scrolled-window-set-placement gtk::scrolled-window-get-placement
   gtk::scrolled-window-set-shadow-type gtk::scrolled-window-get-shadow-type
   gtk::scrolled-window-add-with-viewport gtk::separator-menu-item-get-type
   gtk::separator-menu-item-new gtk::size-group-get-type gtk::size-group-new
   gtk::size-group-set-mode gtk::size-group-get-mode gtk::size-group-add-widget
   gtk::size-group-remove-widget gtk::spin-button-get-type
   gtk::spin-button-configure gtk::spin-button-new
   gtk::spin-button-new-with-range gtk::spin-button-set-adjustment
   gtk::spin-button-get-adjustment gtk::spin-button-set-digits
   gtk::spin-button-get-digits gtk::spin-button-set-increments
   gtk::spin-button-get-increments gtk::spin-button-set-range
   gtk::spin-button-get-range gtk::spin-button-get-value
   gtk::spin-button-get-value-as-int gtk::spin-button-set-value
   gtk::spin-button-set-update-policy gtk::spin-button-get-update-policy
   gtk::spin-button-set-numeric gtk::spin-button-get-numeric
   gtk::spin-button-spin gtk::spin-button-set-wrap gtk::spin-button-get-wrap
   gtk::spin-button-set-snap-to-ticks gtk::spin-button-get-snap-to-ticks
   gtk::spin-button-update gtk::stock-add gtk::stock-add-static
   gtk::stock-lookup gtk::stock-list-ids gtk::stock-item-copy
   gtk::stock-item-free gtk::statusbar-get-type gtk::statusbar-new
   gtk::statusbar-get-context-id gtk::statusbar-push gtk::statusbar-pop
   gtk::statusbar-remove gtk::statusbar-set-has-resize-grip
   gtk::statusbar-get-has-resize-grip gtk::table-get-type gtk::table-new
   gtk::table-resize gtk::table-attach gtk::table-attach-defaults
   gtk::table-set-row-spacing gtk::table-get-row-spacing
   gtk::table-set-col-spacing gtk::table-get-col-spacing
   gtk::table-set-row-spacings gtk::table-get-default-row-spacing
   gtk::table-set-col-spacings gtk::table-get-default-col-spacing
   gtk::table-set-homogeneous gtk::table-get-homogeneous
   gtk::tearoff-menu-item-get-type gtk::tearoff-menu-item-new
   gtk::text-tag-get-type gtk::text-tag-new gtk::text-tag-get-priority
   gtk::text-tag-set-priority gtk::text-tag-event gtk::text-attributes-new
   gtk::text-attributes-copy gtk::text-attributes-copy-values
   gtk::text-attributes-unref gtk::text-attributes-ref
   gtk::text-attributes-get-type gtk::text-tag-table-get-type
   gtk::text-tag-table-new gtk::text-tag-table-add gtk::text-tag-table-remove
   gtk::text-tag-table-lookup gtk::text-tag-table-foreach
   gtk::text-tag-table-get-size gtk::text-child-anchor-get-type
   gtk::text-child-anchor-new gtk::text-child-anchor-get-widgets
   gtk::text-child-anchor-get-deleted gtk::text-iter-get-buffer
   gtk::text-iter-copy gtk::text-iter-free gtk::text-iter-get-type
   gtk::text-iter-get-offset gtk::text-iter-get-line
   gtk::text-iter-get-line-offset gtk::text-iter-get-line-index
   gtk::text-iter-get-visible-line-offset gtk::text-iter-get-visible-line-index
   gtk::text-iter-get-char gtk::text-iter-get-slice gtk::text-iter-get-text
   gtk::text-iter-get-visible-slice gtk::text-iter-get-visible-text
   gtk::text-iter-get-pixbuf gtk::text-iter-get-marks
   gtk::text-iter-get-child-anchor gtk::text-iter-get-toggled-tags
   gtk::text-iter-begins-tag gtk::text-iter-ends-tag gtk::text-iter-toggles-tag
   gtk::text-iter-has-tag gtk::text-iter-get-tags gtk::text-iter-editable
   gtk::text-iter-can-insert gtk::text-iter-starts-word
   gtk::text-iter-ends-word gtk::text-iter-inside-word
   gtk::text-iter-starts-sentence gtk::text-iter-ends-sentence
   gtk::text-iter-inside-sentence gtk::text-iter-starts-line
   gtk::text-iter-ends-line gtk::text-iter-is-cursor-position
   gtk::text-iter-get-chars-in-line gtk::text-iter-get-bytes-in-line
   gtk::text-iter-get-attributes gtk::text-iter-get-language
   gtk::text-iter-is-end gtk::text-iter-is-start gtk::text-iter-forward-char
   gtk::text-iter-backward-char gtk::text-iter-forward-chars
   gtk::text-iter-backward-chars gtk::text-iter-forward-line
   gtk::text-iter-backward-line gtk::text-iter-forward-lines
   gtk::text-iter-backward-lines gtk::text-iter-forward-word-end
   gtk::text-iter-backward-word-start gtk::text-iter-forward-word-ends
   gtk::text-iter-backward-word-starts gtk::text-iter-forward-sentence-end
   gtk::text-iter-backward-sentence-start gtk::text-iter-forward-sentence-ends
   gtk::text-iter-backward-sentence-starts
   gtk::text-iter-forward-cursor-position
   gtk::text-iter-backward-cursor-position
   gtk::text-iter-forward-cursor-positions
   gtk::text-iter-backward-cursor-positions gtk::text-iter-set-offset
   gtk::text-iter-set-line gtk::text-iter-set-line-offset
   gtk::text-iter-set-line-index gtk::text-iter-forward-to-end
   gtk::text-iter-forward-to-line-end gtk::text-iter-set-visible-line-offset
   gtk::text-iter-set-visible-line-index gtk::text-iter-forward-to-tag-toggle
   gtk::text-iter-backward-to-tag-toggle gtk::text-iter-forward-find-char
   gtk::text-iter-backward-find-char gtk::text-iter-forward-search
   gtk::text-iter-backward-search gtk::text-iter-equal gtk::text-iter-compare
   gtk::text-iter-in-range gtk::text-iter-order gtk::text-mark-get-type
   gtk::text-mark-set-visible gtk::text-mark-get-visible
   gtk::text-mark-get-name gtk::text-mark-get-deleted gtk::text-mark-get-buffer
   gtk::text-mark-get-left-gravity gtk::text-buffer-get-type
   gtk::text-buffer-new gtk::text-buffer-get-line-count
   gtk::text-buffer-get-char-count gtk::text-buffer-get-tag-table
   gtk::text-buffer-set-text gtk::text-buffer-insert
   gtk::text-buffer-insert-at-cursor gtk::text-buffer-insert-interactive
   gtk::text-buffer-insert-interactive-at-cursor gtk::text-buffer-insert-range
   gtk::text-buffer-insert-range-interactive gtk::text-buffer-insert-with-tags
   gtk::text-buffer-insert-with-tags-by-name gtk::text-buffer-delete
   gtk::text-buffer-delete-interactive gtk::text-buffer-get-text
   gtk::text-buffer-get-slice gtk::text-buffer-insert-pixbuf
   gtk::text-buffer-insert-child-anchor gtk::text-buffer-create-child-anchor
   gtk::text-buffer-create-mark gtk::text-buffer-move-mark
   gtk::text-buffer-delete-mark gtk::text-buffer-get-mark
   gtk::text-buffer-move-mark-by-name gtk::text-buffer-delete-mark-by-name
   gtk::text-buffer-get-insert gtk::text-buffer-get-selection-bound
   gtk::text-buffer-place-cursor gtk::text-buffer-apply-tag
   gtk::text-buffer-remove-tag gtk::text-buffer-apply-tag-by-name
   gtk::text-buffer-remove-tag-by-name gtk::text-buffer-remove-all-tags
   gtk::text-buffer-create-tag gtk::text-buffer-get-iter-at-line-offset
   gtk::text-buffer-get-iter-at-line-index gtk::text-buffer-get-iter-at-offset
   gtk::text-buffer-get-iter-at-line gtk::text-buffer-get-start-iter
   gtk::text-buffer-get-end-iter gtk::text-buffer-get-bounds
   gtk::text-buffer-get-iter-at-mark gtk::text-buffer-get-iter-at-child-anchor
   gtk::text-buffer-get-modified gtk::text-buffer-set-modified
   gtk::text-buffer-add-selection-clipboard
   gtk::text-buffer-remove-selection-clipboard gtk::text-buffer-cut-clipboard
   gtk::text-buffer-copy-clipboard gtk::text-buffer-paste-clipboard
   gtk::text-buffer-get-selection-bounds gtk::text-buffer-delete-selection
   gtk::text-buffer-begin-user-action gtk::text-buffer-end-user-action
   gtk::text-view-get-type gtk::text-view-new gtk::text-view-new-with-buffer
   gtk::text-view-set-buffer gtk::text-view-get-buffer
   gtk::text-view-scroll-to-iter gtk::text-view-scroll-to-mark
   gtk::text-view-scroll-mark-onscreen gtk::text-view-move-mark-onscreen
   gtk::text-view-place-cursor-onscreen gtk::text-view-get-visible-rect
   gtk::text-view-set-cursor-visible gtk::text-view-get-cursor-visible
   gtk::text-view-get-iter-location gtk::text-view-get-iter-at-location
   gtk::text-view-get-line-yrange gtk::text-view-get-line-at-y
   gtk::text-view-buffer-to-window-coords
   gtk::text-view-window-to-buffer-coords gtk::text-view-get-window
   gtk::text-view-get-window-type gtk::text-view-set-border-window-size
   gtk::text-view-get-border-window-size gtk::text-view-forward-display-line
   gtk::text-view-backward-display-line gtk::text-view-forward-display-line-end
   gtk::text-view-backward-display-line-start
   gtk::text-view-starts-display-line gtk::text-view-move-visually
   gtk::text-view-add-child-at-anchor gtk::text-view-add-child-in-window
   gtk::text-view-move-child gtk::text-view-set-wrap-mode
   gtk::text-view-get-wrap-mode gtk::text-view-set-editable
   gtk::text-view-get-editable gtk::text-view-set-pixels-above-lines
   gtk::text-view-get-pixels-above-lines gtk::text-view-set-pixels-below-lines
   gtk::text-view-get-pixels-below-lines gtk::text-view-set-pixels-inside-wrap
   gtk::text-view-get-pixels-inside-wrap gtk::text-view-set-justification
   gtk::text-view-get-justification gtk::text-view-set-left-margin
   gtk::text-view-get-left-margin gtk::text-view-set-right-margin
   gtk::text-view-get-right-margin gtk::text-view-set-indent
   gtk::text-view-get-indent gtk::text-view-set-tabs gtk::text-view-get-tabs
   gtk::text-view-get-default-attributes gtk::tips-query-get-type
   gtk::tooltips-get-type gtk::tooltips-new gtk::tooltips-enable
   gtk::tooltips-disable gtk::tooltips-set-tip gtk::tooltips-data-get
   gtk::tooltips-force-window gtk::toolbar-get-type gtk::toolbar-new
   gtk::toolbar-append-item gtk::toolbar-prepend-item gtk::toolbar-insert-item
   gtk::toolbar-insert-stock gtk::toolbar-append-space
   gtk::toolbar-prepend-space gtk::toolbar-insert-space
   gtk::toolbar-remove-space gtk::toolbar-append-element
   gtk::toolbar-prepend-element gtk::toolbar-insert-element
   gtk::toolbar-append-widget gtk::toolbar-prepend-widget
   gtk::toolbar-insert-widget gtk::toolbar-set-orientation
   gtk::toolbar-set-style gtk::toolbar-set-icon-size gtk::toolbar-set-tooltips
   gtk::toolbar-unset-style gtk::toolbar-unset-icon-size
   gtk::toolbar-get-orientation gtk::toolbar-get-style
   gtk::toolbar-get-icon-size gtk::toolbar-get-tooltips
   gtk::tree-drag-source-get-type gtk::tree-drag-source-row-draggable
   gtk::tree-drag-source-drag-data-delete gtk::tree-drag-source-drag-data-get
   gtk::tree-drag-dest-get-type gtk::tree-drag-dest-drag-data-received
   gtk::tree-drag-dest-row-drop-possible gtk::tree-set-row-drag-data
   gtk::tree-get-row-drag-data gtk::tree-model-sort-get-type
   gtk::tree-model-sort-new-with-model gtk::tree-model-sort-get-model
   gtk::tree-model-sort-convert-child-path-to-path
   gtk::tree-model-sort-convert-child-iter-to-iter
   gtk::tree-model-sort-convert-path-to-child-path
   gtk::tree-model-sort-convert-iter-to-child-iter
   gtk::tree-model-sort-reset-default-sort-func
   gtk::tree-model-sort-clear-cache gtk::tree-view-column-get-type
   gtk::tree-view-column-new gtk::tree-view-column-new-with-attributes
   gtk::tree-view-column-pack-start gtk::tree-view-column-pack-end
   gtk::tree-view-column-clear gtk::tree-view-column-get-cell-renderers
   gtk::tree-view-column-add-attribute gtk::tree-view-column-set-attributes
   gtk::tree-view-column-set-cell-data-func
   gtk::tree-view-column-clear-attributes gtk::tree-view-column-set-spacing
   gtk::tree-view-column-get-spacing gtk::tree-view-column-set-visible
   gtk::tree-view-column-get-visible gtk::tree-view-column-set-resizable
   gtk::tree-view-column-get-resizable gtk::tree-view-column-set-sizing
   gtk::tree-view-column-get-sizing gtk::tree-view-column-get-width
   gtk::tree-view-column-get-fixed-width gtk::tree-view-column-set-fixed-width
   gtk::tree-view-column-set-min-width gtk::tree-view-column-get-min-width
   gtk::tree-view-column-set-max-width gtk::tree-view-column-get-max-width
   gtk::tree-view-column-clicked gtk::tree-view-column-set-title
   gtk::tree-view-column-get-title gtk::tree-view-column-set-clickable
   gtk::tree-view-column-get-clickable gtk::tree-view-column-set-widget
   gtk::tree-view-column-get-widget gtk::tree-view-column-set-alignment
   gtk::tree-view-column-get-alignment gtk::tree-view-column-set-reorderable
   gtk::tree-view-column-get-reorderable
   gtk::tree-view-column-set-sort-column-id
   gtk::tree-view-column-get-sort-column-id
   gtk::tree-view-column-set-sort-indicator
   gtk::tree-view-column-get-sort-indicator
   gtk::tree-view-column-set-sort-order gtk::tree-view-column-get-sort-order
   gtk::tree-view-column-cell-set-cell-data gtk::tree-view-column-cell-get-size
   gtk::tree-view-column-cell-is-visible gtk::tree-view-get-type
   gtk::tree-view-new gtk::tree-view-new-with-model gtk::tree-view-get-model
   gtk::tree-view-set-model gtk::tree-view-get-selection
   gtk::tree-view-get-hadjustment gtk::tree-view-set-hadjustment
   gtk::tree-view-get-vadjustment gtk::tree-view-set-vadjustment
   gtk::tree-view-get-headers-visible gtk::tree-view-set-headers-visible
   gtk::tree-view-columns-autosize gtk::tree-view-set-headers-clickable
   gtk::tree-view-set-rules-hint gtk::tree-view-get-rules-hint
   gtk::tree-view-append-column gtk::tree-view-remove-column
   gtk::tree-view-insert-column gtk::tree-view-insert-column-with-attributes
   gtk::tree-view-insert-column-with-data-func gtk::tree-view-get-column
   gtk::tree-view-get-columns gtk::tree-view-move-column-after
   gtk::tree-view-set-expander-column gtk::tree-view-get-expander-column
   gtk::tree-view-set-column-drag-function gtk::tree-view-scroll-to-point
   gtk::tree-view-scroll-to-cell gtk::tree-view-row-activated
   gtk::tree-view-expand-all gtk::tree-view-collapse-all
   gtk::tree-view-expand-row gtk::tree-view-collapse-row
   gtk::tree-view-map-expanded-rows gtk::tree-view-row-expanded
   gtk::tree-view-set-reorderable gtk::tree-view-get-reorderable
   gtk::tree-view-set-cursor gtk::tree-view-get-cursor
   gtk::tree-view-get-bin-window gtk::tree-view-get-path-at-pos
   gtk::tree-view-get-cell-area gtk::tree-view-get-background-area
   gtk::tree-view-get-visible-rect gtk::tree-view-widget-to-tree-coords
   gtk::tree-view-tree-to-widget-coords gtk::tree-view-enable-model-drag-source
   gtk::tree-view-enable-model-drag-dest gtk::tree-view-unset-rows-drag-source
   gtk::tree-view-unset-rows-drag-dest gtk::tree-view-set-drag-dest-row
   gtk::tree-view-get-drag-dest-row gtk::tree-view-get-dest-row-at-pos
   gtk::tree-view-create-row-drag-icon gtk::tree-view-set-enable-search
   gtk::tree-view-get-enable-search gtk::tree-view-get-search-column
   gtk::tree-view-set-search-column gtk::tree-view-get-search-equal-func
   gtk::tree-view-set-search-equal-func gtk::tree-view-set-destroy-count-func
   gtk::tree-selection-get-type gtk::tree-selection-set-mode
   gtk::tree-selection-get-mode gtk::tree-selection-set-select-function
   gtk::tree-selection-get-user-data gtk::tree-selection-get-tree-view
   gtk::tree-selection-get-selected gtk::tree-selection-selected-foreach
   gtk::tree-selection-select-path gtk::tree-selection-unselect-path
   gtk::tree-selection-select-iter gtk::tree-selection-unselect-iter
   gtk::tree-selection-path-is-selected gtk::tree-selection-iter-is-selected
   gtk::tree-selection-select-all gtk::tree-selection-unselect-all
   gtk::tree-selection-select-range gtk::tree-store-get-type
   gtk::tree-store-new gtk::tree-store-newv gtk::tree-store-set-column-types
   gtk::tree-store-set-value gtk::tree-store-set gtk::tree-store-set-valist
   gtk::tree-store-remove gtk::tree-store-insert gtk::tree-store-insert-before
   gtk::tree-store-insert-after gtk::tree-store-prepend gtk::tree-store-append
   gtk::tree-store-is-ancestor gtk::tree-store-iter-depth gtk::tree-store-clear
   gtk::vbutton-box-get-type gtk::vbutton-box-new gtk::vpaned-get-type
   gtk::vpaned-new gtk::vruler-get-type gtk::vruler-new gtk::vscale-get-type
   gtk::vscale-new gtk::vscale-new-with-range gtk::vseparator-get-type
   gtk::vseparator-new)
 :gtk)

(pushnew ':gtk *features*)

;;; end generated output

