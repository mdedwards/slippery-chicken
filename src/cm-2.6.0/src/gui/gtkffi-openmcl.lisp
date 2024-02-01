;;; Output by lambda-gtk on 11.12.2004 at 13:44:10
;;; This software is released under the Lisp Lesser Gnu Public License
;;; (LLGPL). See http://opensource.franz.com/preamble.html for the
;;; terms of this agreement.

(in-package :cl-user)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(*gtk-libdir* *gtk-libfiles*) :cl-user)
  (defvar *gtk-libdir* "/sw/lib/")
  (defvar *gtk-libfiles*  '("libgtk-x11-2.*.dylib"))
  (flet ((libpath (lib &aux p m)
           (setq p (concatenate 'string *gtk-libdir* lib))
           ;; sigh, apparently Fink's lib numbers can vary...
           (setq m  (directory p))
           (cond ((null m)
                  (error "Library ~S not found. Either GTK is not installed or else cl-user:*gtk-libdir* needs to be set to the directory containing GTK on your machine." p))
                 ((cdr m)
                  (ccl::native-translated-namestring
                   (first (sort #'string-greaterp m
                                           :key #'string))))
                 (t
                  (ccl::native-translated-namestring (car m))))))
    (dolist (l *gtk-libfiles*)
      (open-shared-library (libpath l)))))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (if (probe-file "ccl:darwin-headers;gtk2;" )
    (use-interface-dir :gtk2)
    (error "Interface directory ccl:darwin-headers;gtk2; does not exist.")))

;;; begin generated output

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage :gtk (:use :ccl :common-lisp) (:shadow :true :false :fill))
  (defpackage :gdk
              (:use :ccl :common-lisp)
              (:shadow :copy :invert :xor :clear :and :or :set :delete :plus
               :mouse :destroy :map :unmap :scroll))
  (defpackage :g (:use :ccl :common-lisp))
  (defpackage :atk (:use :ccl :common-lisp))
  (defpackage :pango (:use :ccl :common-lisp) (:shadow :break)))

;; make :gtk the default package.
(in-package :gtk)

;;; GTK glue code

(defconstant gtk::+false+ 0)
(defconstant gtk::+true+ 1)
(defun g::nullptr () (%null-ptr))
(defun g::nullptr? (x) (%null-ptr-p x))
(defvar *gtk-init* ())
(defun gtk::init-ensure (&optional strings)
  (declare (ignore strings))
  (unless *gtk-init* (gtk::init 0 (g::nullptr)) (setq *gtk-init* t))
  *gtk-init*)
(defmacro struct-alloc (&rest args) (list* 'make-record args))
(defun struct-free (x) (#_free x))
(defun string->cstring (str) (ccl::make-cstring str))
(defun cstring->string (cstr) (%get-cstring cstr))
(defun cstring-alloc (str) (ccl::make-cstring str))
(defun cstring-free (cstr) (ccl::free cstr) (values))
(defmacro define-signal-handler (name return params &body body)
  (list* 'defcallback
         (list* name
                (list* (nconc (loop for p in params
                                    collect ':address
                                    collect p)
                                    (list return))
                              body))))
(defun g::callback (x) x)
(defun g::signal-connect (instance detailed-signal c-handler data)
  (g::signal-connect-data instance detailed-signal c-handler data (g::nullptr)
   0))
(defun g::signal-connect-after (instance detailed-signal c-handler data)
  (g::signal-connect-data instance detailed-signal c-handler data (g::nullptr)
   1))
(defun g::signal-connect-swapped (instance detailed_signal c_handler data)
  (g::signal-connect-data instance detailed_signal c_handler data (g::nullptr)
   2))

;;; constants

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

;;; accessors

(defun g::List.data (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<GL>ist.data) val) val)
      (pref ptr :<GL>ist.data)))
(defun g::List.next (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<GL>ist.next) val) val)
      (pref ptr :<GL>ist.next)))
(defun g::List.prev (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<GL>ist.prev) val) val)
      (pref ptr :<GL>ist.prev)))
(defun gdk::Rectangle.x (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<R>ectangle.x) val) val)
      (pref ptr :<G>dk<R>ectangle.x)))
(defun gdk::Rectangle.y (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<R>ectangle.y) val) val)
      (pref ptr :<G>dk<R>ectangle.y)))
(defun gdk::Rectangle.width (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<R>ectangle.width) val) val)
      (pref ptr :<G>dk<R>ectangle.width)))
(defun gdk::Rectangle.height (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<R>ectangle.height) val) val)
      (pref ptr :<G>dk<R>ectangle.height)))
(defun gdk::Color.pixel (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<C>olor.pixel) val) val)
      (pref ptr :<G>dk<C>olor.pixel)))
(defun gdk::Color.red (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<C>olor.red) val) val)
      (pref ptr :<G>dk<C>olor.red)))
(defun gdk::Color.green (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<C>olor.green) val) val)
      (pref ptr :<G>dk<C>olor.green)))
(defun gdk::Color.blue (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<C>olor.blue) val) val)
      (pref ptr :<G>dk<C>olor.blue)))
(defun gdk::EventExpose.type (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<E>vent<E>xpose.type) val) val)
      (pref ptr :<G>dk<E>vent<E>xpose.type)))
(defun gdk::EventExpose.window (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<E>vent<E>xpose.window) val) val)
      (pref ptr :<G>dk<E>vent<E>xpose.window)))
(defun gdk::EventExpose.send-event (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<E>vent<E>xpose.send_event) val) val)
      (pref ptr :<G>dk<E>vent<E>xpose.send_event)))
(defun gdk::EventExpose.area.x (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<E>vent<E>xpose.area.x) val) val)
      (pref ptr :<G>dk<E>vent<E>xpose.area.x)))
(defun gdk::EventExpose.area.y (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<E>vent<E>xpose.area.y) val) val)
      (pref ptr :<G>dk<E>vent<E>xpose.area.y)))
(defun gdk::EventExpose.area.width (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<E>vent<E>xpose.area.width) val) val)
      (pref ptr :<G>dk<E>vent<E>xpose.area.width)))
(defun gdk::EventExpose.area.height (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<E>vent<E>xpose.area.height) val) val)
      (pref ptr :<G>dk<E>vent<E>xpose.area.height)))
(defun gdk::EventExpose.region (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<E>vent<E>xpose.region) val) val)
      (pref ptr :<G>dk<E>vent<E>xpose.region)))
(defun gdk::EventExpose.count (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<E>vent<E>xpose.count) val) val)
      (pref ptr :<G>dk<E>vent<E>xpose.count)))
(defun gdk::EventMotion.type (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<E>vent<M>otion.type) val) val)
      (pref ptr :<G>dk<E>vent<M>otion.type)))
(defun gdk::EventMotion.window (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<E>vent<M>otion.window) val) val)
      (pref ptr :<G>dk<E>vent<M>otion.window)))
(defun gdk::EventMotion.send-event (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<E>vent<M>otion.send_event) val) val)
      (pref ptr :<G>dk<E>vent<M>otion.send_event)))
(defun gdk::EventMotion.time (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<E>vent<M>otion.time) val) val)
      (pref ptr :<G>dk<E>vent<M>otion.time)))
(defun gdk::EventMotion.x (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<E>vent<M>otion.x)
                   (coerce val 'double-float))
             val)
      (pref ptr :<G>dk<E>vent<M>otion.x)))
(defun gdk::EventMotion.y (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<E>vent<M>otion.y)
                   (coerce val 'double-float))
             val)
      (pref ptr :<G>dk<E>vent<M>otion.y)))
(defun gdk::EventMotion.state (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<E>vent<M>otion.state) val) val)
      (pref ptr :<G>dk<E>vent<M>otion.state)))
(defun gdk::EventMotion.is-hint (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<E>vent<M>otion.is_hint) val) val)
      (pref ptr :<G>dk<E>vent<M>otion.is_hint)))
(defun gdk::EventMotion.device (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<E>vent<M>otion.device) val) val)
      (pref ptr :<G>dk<E>vent<M>otion.device)))
(defun gdk::EventMotion.x-root (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<E>vent<M>otion.x_root)
                   (coerce val 'double-float))
             val)
      (pref ptr :<G>dk<E>vent<M>otion.x_root)))
(defun gdk::EventMotion.y-root (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<E>vent<M>otion.y_root)
                   (coerce val 'double-float))
             val)
      (pref ptr :<G>dk<E>vent<M>otion.y_root)))
(defun gdk::EventButton.type (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<E>vent<B>utton.type) val) val)
      (pref ptr :<G>dk<E>vent<B>utton.type)))
(defun gdk::EventButton.window (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<E>vent<B>utton.window) val) val)
      (pref ptr :<G>dk<E>vent<B>utton.window)))
(defun gdk::EventButton.send-event (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<E>vent<B>utton.send_event) val) val)
      (pref ptr :<G>dk<E>vent<B>utton.send_event)))
(defun gdk::EventButton.time (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<E>vent<B>utton.time) val) val)
      (pref ptr :<G>dk<E>vent<B>utton.time)))
(defun gdk::EventButton.x (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<E>vent<B>utton.x)
                   (coerce val 'double-float))
             val)
      (pref ptr :<G>dk<E>vent<B>utton.x)))
(defun gdk::EventButton.y (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<E>vent<B>utton.y)
                   (coerce val 'double-float))
             val)
      (pref ptr :<G>dk<E>vent<B>utton.y)))
(defun gdk::EventButton.state (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<E>vent<B>utton.state) val) val)
      (pref ptr :<G>dk<E>vent<B>utton.state)))
(defun gdk::EventButton.button (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<E>vent<B>utton.button) val) val)
      (pref ptr :<G>dk<E>vent<B>utton.button)))
(defun gdk::EventButton.device (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<E>vent<B>utton.device) val) val)
      (pref ptr :<G>dk<E>vent<B>utton.device)))
(defun gdk::EventButton.x-root (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<E>vent<B>utton.x_root)
                   (coerce val 'double-float))
             val)
      (pref ptr :<G>dk<E>vent<B>utton.x_root)))
(defun gdk::EventButton.y-root (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<E>vent<B>utton.y_root)
                   (coerce val 'double-float))
             val)
      (pref ptr :<G>dk<E>vent<B>utton.y_root)))
(defun gdk::EventKey.type (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<E>vent<K>ey.type) val) val)
      (pref ptr :<G>dk<E>vent<K>ey.type)))
(defun gdk::EventKey.window (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<E>vent<K>ey.window) val) val)
      (pref ptr :<G>dk<E>vent<K>ey.window)))
(defun gdk::EventKey.send-event (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<E>vent<K>ey.send_event) val) val)
      (pref ptr :<G>dk<E>vent<K>ey.send_event)))
(defun gdk::EventKey.time (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<E>vent<K>ey.time) val) val)
      (pref ptr :<G>dk<E>vent<K>ey.time)))
(defun gdk::EventKey.state (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<E>vent<K>ey.state) val) val)
      (pref ptr :<G>dk<E>vent<K>ey.state)))
(defun gdk::EventKey.keyval (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<E>vent<K>ey.keyval) val) val)
      (pref ptr :<G>dk<E>vent<K>ey.keyval)))
(defun gdk::EventKey.length (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<E>vent<K>ey.length) val) val)
      (pref ptr :<G>dk<E>vent<K>ey.length)))
(defun gdk::EventKey.string (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<E>vent<K>ey.string)
                   (ccl::make-cstring val))
             val)
      (pref ptr :<G>dk<E>vent<K>ey.string)))
(defun gdk::Font.type (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<F>ont.type) val) val)
      (pref ptr :<G>dk<F>ont.type)))
(defun gdk::Font.ascent (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<F>ont.ascent) val) val)
      (pref ptr :<G>dk<F>ont.ascent)))
(defun gdk::Font.descent (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<F>ont.descent) val) val)
      (pref ptr :<G>dk<F>ont.descent)))
(defun gdk::Image.type (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<I>mage.type) val) val)
      (pref ptr :<G>dk<I>mage.type)))
(defun gdk::Image.visual (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<I>mage.visual) val) val)
      (pref ptr :<G>dk<I>mage.visual)))
(defun gdk::Image.byte-order (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<I>mage.byte_order) val) val)
      (pref ptr :<G>dk<I>mage.byte_order)))
(defun gdk::Image.width (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<I>mage.width) val) val)
      (pref ptr :<G>dk<I>mage.width)))
(defun gdk::Image.height (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<I>mage.height) val) val)
      (pref ptr :<G>dk<I>mage.height)))
(defun gdk::Image.depth (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<I>mage.depth) val) val)
      (pref ptr :<G>dk<I>mage.depth)))
(defun gdk::Image.bpp (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<I>mage.bpp) val) val)
      (pref ptr :<G>dk<I>mage.bpp)))
(defun gdk::Image.bpl (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<I>mage.bpl) val) val)
      (pref ptr :<G>dk<I>mage.bpl)))
(defun gdk::Image.bits-per-pixel (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<I>mage.bits_per_pixel) val) val)
      (pref ptr :<G>dk<I>mage.bits_per_pixel)))
(defun gdk::Image.mem (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<I>mage.mem) val) val)
      (pref ptr :<G>dk<I>mage.mem)))
(defun gdk::Image.colormap (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<I>mage.colormap) val) val)
      (pref ptr :<G>dk<I>mage.colormap)))
(defun gdk::Image.windowing-data (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<I>mage.windowing_data) val) val)
      (pref ptr :<G>dk<I>mage.windowing_data)))
(defun gdk::Visual.type (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<V>isual.type) val) val)
      (pref ptr :<G>dk<V>isual.type)))
(defun gdk::Visual.depth (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<V>isual.depth) val) val)
      (pref ptr :<G>dk<V>isual.depth)))
(defun gdk::Visual.byte-order (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<V>isual.byte_order) val) val)
      (pref ptr :<G>dk<V>isual.byte_order)))
(defun gdk::Visual.colormap-size (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<V>isual.colormap_size) val) val)
      (pref ptr :<G>dk<V>isual.colormap_size)))
(defun gdk::Visual.bits-per-rgb (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<V>isual.bits_per_rgb) val) val)
      (pref ptr :<G>dk<V>isual.bits_per_rgb)))
(defun gdk::Visual.red-mask (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<V>isual.red_mask) val) val)
      (pref ptr :<G>dk<V>isual.red_mask)))
(defun gdk::Visual.red-shift (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<V>isual.red_shift) val) val)
      (pref ptr :<G>dk<V>isual.red_shift)))
(defun gdk::Visual.green-prec (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<V>isual.green_prec) val) val)
      (pref ptr :<G>dk<V>isual.green_prec)))
(defun gdk::Visual.blue-mask (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<V>isual.blue_mask) val) val)
      (pref ptr :<G>dk<V>isual.blue_mask)))
(defun gdk::Visual.blue-shift (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<V>isual.blue_shift) val) val)
      (pref ptr :<G>dk<V>isual.blue_shift)))
(defun gdk::Visual.blue-prec (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<V>isual.blue_prec) val) val)
      (pref ptr :<G>dk<V>isual.blue_prec)))
(defun gdk::Geometry.min-width (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<G>eometry.min_width) val) val)
      (pref ptr :<G>dk<G>eometry.min_width)))
(defun gdk::Geometry.min-height (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<G>eometry.min_height) val) val)
      (pref ptr :<G>dk<G>eometry.min_height)))
(defun gdk::Geometry.max-width (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<G>eometry.max_width) val) val)
      (pref ptr :<G>dk<G>eometry.max_width)))
(defun gdk::Geometry.max-height (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<G>eometry.max_height) val) val)
      (pref ptr :<G>dk<G>eometry.max_height)))
(defun gdk::Geometry.base-width (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<G>eometry.base_width) val) val)
      (pref ptr :<G>dk<G>eometry.base_width)))
(defun gdk::Geometry.base-height (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<G>eometry.base_height) val) val)
      (pref ptr :<G>dk<G>eometry.base_height)))
(defun gdk::Geometry.width-inc (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<G>eometry.width_inc) val) val)
      (pref ptr :<G>dk<G>eometry.width_inc)))
(defun gdk::Geometry.height-inc (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<G>eometry.height_inc) val) val)
      (pref ptr :<G>dk<G>eometry.height_inc)))
(defun gdk::Geometry.min-aspect (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<G>eometry.min_aspect)
                   (coerce val 'double-float))
             val)
      (pref ptr :<G>dk<G>eometry.min_aspect)))
(defun gdk::Geometry.max-aspect (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<G>eometry.max_aspect)
                   (coerce val 'double-float))
             val)
      (pref ptr :<G>dk<G>eometry.max_aspect)))
(defun gdk::Geometry.win-gravity (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>dk<G>eometry.win_gravity) val) val)
      (pref ptr :<G>dk<G>eometry.win_gravity)))
(defun gtk::Adjustment.lower (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>tk<A>djustment.lower)
                   (coerce val 'double-float))
             val)
      (pref ptr :<G>tk<A>djustment.lower)))
(defun gtk::Adjustment.upper (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>tk<A>djustment.upper)
                   (coerce val 'double-float))
             val)
      (pref ptr :<G>tk<A>djustment.upper)))
(defun gtk::Adjustment.value (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>tk<A>djustment.value)
                   (coerce val 'double-float))
             val)
      (pref ptr :<G>tk<A>djustment.value)))
(defun gtk::Adjustment.step-increment (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>tk<A>djustment.step_increment)
                   (coerce val 'double-float))
             val)
      (pref ptr :<G>tk<A>djustment.step_increment)))
(defun gtk::Adjustment.page-increment (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>tk<A>djustment.page_increment)
                   (coerce val 'double-float))
             val)
      (pref ptr :<G>tk<A>djustment.page_increment)))
(defun gtk::Adjustment.page-size (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>tk<A>djustment.page_size)
                   (coerce val 'double-float))
             val)
      (pref ptr :<G>tk<A>djustment.page_size)))
(defun gtk::Style.black.pixel (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>tk<S>tyle.black.pixel) val) val)
      (pref ptr :<G>tk<S>tyle.black.pixel)))
(defun gtk::Style.black.red (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>tk<S>tyle.black.red) val) val)
      (pref ptr :<G>tk<S>tyle.black.red)))
(defun gtk::Style.black.green (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>tk<S>tyle.black.green) val) val)
      (pref ptr :<G>tk<S>tyle.black.green)))
(defun gtk::Style.black.blue (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>tk<S>tyle.black.blue) val) val)
      (pref ptr :<G>tk<S>tyle.black.blue)))
(defun gtk::Style.white.pixel (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>tk<S>tyle.white.pixel) val) val)
      (pref ptr :<G>tk<S>tyle.white.pixel)))
(defun gtk::Style.white.red (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>tk<S>tyle.white.red) val) val)
      (pref ptr :<G>tk<S>tyle.white.red)))
(defun gtk::Style.white.green (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>tk<S>tyle.white.green) val) val)
      (pref ptr :<G>tk<S>tyle.white.green)))
(defun gtk::Style.white.blue (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>tk<S>tyle.white.blue) val) val)
      (pref ptr :<G>tk<S>tyle.white.blue)))
(defun gtk::Style.font-desc (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>tk<S>tyle.font_desc) val) val)
      (pref ptr :<G>tk<S>tyle.font_desc)))
(defun gtk::Style.xthickness (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>tk<S>tyle.xthickness) val) val)
      (pref ptr :<G>tk<S>tyle.xthickness)))
(defun gtk::Style.ythickness (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>tk<S>tyle.ythickness) val) val)
      (pref ptr :<G>tk<S>tyle.ythickness)))
(defun gtk::Style.fg-gc (ptr index &optional val)
  (unless (< index 5)
    (error ":<G>tk<S>tyle.fg_gc[~d]: index larger than 5." index))
  (if val
      (progn (setf (%get-ptr (pref ptr :<G>tk<S>tyle.fg_gc) (* index 4)) val)
             val)
      (%get-ptr (pref ptr :<G>tk<S>tyle.fg_gc) (* index 4))))
(defun gtk::Style.bg-gc (ptr index &optional val)
  (unless (< index 5)
    (error ":<G>tk<S>tyle.bg_gc[~d]: index larger than 5." index))
  (if val
      (progn (setf (%get-ptr (pref ptr :<G>tk<S>tyle.bg_gc) (* index 4)) val)
             val)
      (%get-ptr (pref ptr :<G>tk<S>tyle.bg_gc) (* index 4))))
(defun gtk::Style.light-gc (ptr index &optional val)
  (unless (< index 5)
    (error ":<G>tk<S>tyle.light_gc[~d]: index larger than 5." index))
  (if val
      (progn (setf (%get-ptr (pref ptr :<G>tk<S>tyle.light_gc) (* index 4))
                   val)
             val)
      (%get-ptr (pref ptr :<G>tk<S>tyle.light_gc) (* index 4))))
(defun gtk::Style.dark-gc (ptr index &optional val)
  (unless (< index 5)
    (error ":<G>tk<S>tyle.dark_gc[~d]: index larger than 5." index))
  (if val
      (progn (setf (%get-ptr (pref ptr :<G>tk<S>tyle.dark_gc) (* index 4)) val)
             val)
      (%get-ptr (pref ptr :<G>tk<S>tyle.dark_gc) (* index 4))))
(defun gtk::Style.mid-gc (ptr index &optional val)
  (unless (< index 5)
    (error ":<G>tk<S>tyle.mid_gc[~d]: index larger than 5." index))
  (if val
      (progn (setf (%get-ptr (pref ptr :<G>tk<S>tyle.mid_gc) (* index 4)) val)
             val)
      (%get-ptr (pref ptr :<G>tk<S>tyle.mid_gc) (* index 4))))
(defun gtk::Style.text-gc (ptr index &optional val)
  (unless (< index 5)
    (error ":<G>tk<S>tyle.text_gc[~d]: index larger than 5." index))
  (if val
      (progn (setf (%get-ptr (pref ptr :<G>tk<S>tyle.text_gc) (* index 4)) val)
             val)
      (%get-ptr (pref ptr :<G>tk<S>tyle.text_gc) (* index 4))))
(defun gtk::Style.base-gc (ptr index &optional val)
  (unless (< index 5)
    (error ":<G>tk<S>tyle.base_gc[~d]: index larger than 5." index))
  (if val
      (progn (setf (%get-ptr (pref ptr :<G>tk<S>tyle.base_gc) (* index 4)) val)
             val)
      (%get-ptr (pref ptr :<G>tk<S>tyle.base_gc) (* index 4))))
(defun gtk::Style.text-aa-gc (ptr index &optional val)
  (unless (< index 5)
    (error ":<G>tk<S>tyle.text_aa_gc[~d]: index larger than 5." index))
  (if val
      (progn (setf (%get-ptr (pref ptr :<G>tk<S>tyle.text_aa_gc) (* index 4))
                   val)
             val)
      (%get-ptr (pref ptr :<G>tk<S>tyle.text_aa_gc) (* index 4))))
(defun gtk::Style.black-gc (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>tk<S>tyle.black_gc) val) val)
      (pref ptr :<G>tk<S>tyle.black_gc)))
(defun gtk::Style.white-gc (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>tk<S>tyle.white_gc) val) val)
      (pref ptr :<G>tk<S>tyle.white_gc)))
(defun gtk::Style.bg-pixmap (ptr index &optional val)
  (unless (< index 5)
    (error ":<G>tk<S>tyle.bg_pixmap[~d]: index larger than 5." index))
  (if val
      (progn (setf (%get-ptr (pref ptr :<G>tk<S>tyle.bg_pixmap) (* index 4))
                   val)
             val)
      (%get-ptr (pref ptr :<G>tk<S>tyle.bg_pixmap) (* index 4))))
(defun gtk::RcStyle.name (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>tk<R>c<S>tyle.name) (ccl::make-cstring val))
             val)
      (pref ptr :<G>tk<R>c<S>tyle.name)))
(defun gtk::RcStyle.font-desc (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>tk<R>c<S>tyle.font_desc) val) val)
      (pref ptr :<G>tk<R>c<S>tyle.font_desc)))
(defun gtk::RcStyle.color-flags (ptr index &optional val)
  (unless (< index 5)
    (error ":<G>tk<R>c<S>tyle.color_flags[~d]: index larger than 5." index))
  (if val
      (progn (setf (%get-signed-long
                     (pref ptr :<G>tk<R>c<S>tyle.color_flags)
                     (* index 4))
                   val)
             val)
      (%get-signed-long (pref ptr :<G>tk<R>c<S>tyle.color_flags) (* index 4))))
(defun gtk::RcStyle.xthickness (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>tk<R>c<S>tyle.xthickness) val) val)
      (pref ptr :<G>tk<R>c<S>tyle.xthickness)))
(defun gtk::RcStyle.ythickness (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>tk<R>c<S>tyle.ythickness) val) val)
      (pref ptr :<G>tk<R>c<S>tyle.ythickness)))
(defun gtk::Widget.state (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>tk<W>idget.state) val) val)
      (pref ptr :<G>tk<W>idget.state)))
(defun gtk::Widget.name (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>tk<W>idget.name) (ccl::make-cstring val)) val)
      (pref ptr :<G>tk<W>idget.name)))
(defun gtk::Widget.style (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>tk<W>idget.style) val) val)
      (pref ptr :<G>tk<W>idget.style)))
(defun gtk::Widget.allocation.x (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>tk<W>idget.allocation.x) val) val)
      (pref ptr :<G>tk<W>idget.allocation.x)))
(defun gtk::Widget.allocation.y (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>tk<W>idget.allocation.y) val) val)
      (pref ptr :<G>tk<W>idget.allocation.y)))
(defun gtk::Widget.allocation.width (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>tk<W>idget.allocation.width) val) val)
      (pref ptr :<G>tk<W>idget.allocation.width)))
(defun gtk::Widget.allocation.height (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>tk<W>idget.allocation.height) val) val)
      (pref ptr :<G>tk<W>idget.allocation.height)))
(defun gtk::Widget.window (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>tk<W>idget.window) val) val)
      (pref ptr :<G>tk<W>idget.window)))
(defun gtk::Widget.parent (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>tk<W>idget.parent) val) val)
      (pref ptr :<G>tk<W>idget.parent)))
(defun gtk::MenuShell.children (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>tk<M>enu<S>hell.children) val) val)
      (pref ptr :<G>tk<M>enu<S>hell.children)))
(defun gtk::Arrow.arrow-type (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>tk<A>rrow.arrow_type) val) val)
      (pref ptr :<G>tk<A>rrow.arrow_type)))
(defun gtk::Arrow.shadow-type (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>tk<A>rrow.shadow_type) val) val)
      (pref ptr :<G>tk<A>rrow.shadow_type)))
(defun gtk::Box.children (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>tk<B>ox.children) val) val)
      (pref ptr :<G>tk<B>ox.children)))
(defun gtk::Box.spacing (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>tk<B>ox.spacing) val) val)
      (pref ptr :<G>tk<B>ox.spacing)))
(defun gtk::Calendar.month (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>tk<C>alendar.month) val) val)
      (pref ptr :<G>tk<C>alendar.month)))
(defun gtk::Calendar.year (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>tk<C>alendar.year) val) val)
      (pref ptr :<G>tk<C>alendar.year)))
(defun gtk::Calendar.selected-day (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>tk<C>alendar.selected_day) val) val)
      (pref ptr :<G>tk<C>alendar.selected_day)))
(defun gtk::Calendar.num-marked-dates (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>tk<C>alendar.num_marked_dates) val) val)
      (pref ptr :<G>tk<C>alendar.num_marked_dates)))
(defun gtk::Calendar.marked-date (ptr index &optional val)
  (unless (< index 31)
    (error ":<G>tk<C>alendar.marked_date[~d]: index larger than 31." index))
  (if val
      (progn (setf (%get-signed-long
                     (pref ptr :<G>tk<C>alendar.marked_date)
                     (* index 4))
                   val)
             val)
      (%get-signed-long (pref ptr :<G>tk<C>alendar.marked_date) (* index 4))))
(defun gtk::Dialog.vbox (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>tk<D>ialog.vbox) val) val)
      (pref ptr :<G>tk<D>ialog.vbox)))
(defun gtk::Dialog.action-area (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>tk<D>ialog.action_area) val) val)
      (pref ptr :<G>tk<D>ialog.action_area)))
(defun gtk::ColorSelectionDialog.colorsel (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>tk<C>olor<S>election<D>ialog.colorsel) val)
             val)
      (pref ptr :<G>tk<C>olor<S>election<D>ialog.colorsel)))
(defun gtk::ColorSelectionDialog.ok-button (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>tk<C>olor<S>election<D>ialog.ok_button) val)
             val)
      (pref ptr :<G>tk<C>olor<S>election<D>ialog.ok_button)))
(defun gtk::ColorSelectionDialog.cancel-button (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>tk<C>olor<S>election<D>ialog.cancel_button)
                   val)
             val)
      (pref ptr :<G>tk<C>olor<S>election<D>ialog.cancel_button)))
(defun gtk::ColorSelectionDialog.help-button (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>tk<C>olor<S>election<D>ialog.help_button) val)
             val)
      (pref ptr :<G>tk<C>olor<S>election<D>ialog.help_button)))
(defun gtk::Combo.entry (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>tk<C>ombo.entry) val) val)
      (pref ptr :<G>tk<C>ombo.entry)))
(defun gtk::Combo.list (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>tk<C>ombo.list) val) val)
      (pref ptr :<G>tk<C>ombo.list)))
(defun gtk::FileSelection.dir-list (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>tk<F>ile<S>election.dir_list) val) val)
      (pref ptr :<G>tk<F>ile<S>election.dir_list)))
(defun gtk::FileSelection.file-list (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>tk<F>ile<S>election.file_list) val) val)
      (pref ptr :<G>tk<F>ile<S>election.file_list)))
(defun gtk::FileSelection.selection-entry (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>tk<F>ile<S>election.selection_entry) val) val)
      (pref ptr :<G>tk<F>ile<S>election.selection_entry)))
(defun gtk::FileSelection.selection-text (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>tk<F>ile<S>election.selection_text) val) val)
      (pref ptr :<G>tk<F>ile<S>election.selection_text)))
(defun gtk::FileSelection.main-vbox (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>tk<F>ile<S>election.main_vbox) val) val)
      (pref ptr :<G>tk<F>ile<S>election.main_vbox)))
(defun gtk::FileSelection.ok-button (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>tk<F>ile<S>election.ok_button) val) val)
      (pref ptr :<G>tk<F>ile<S>election.ok_button)))
(defun gtk::FileSelection.cancel-button (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>tk<F>ile<S>election.cancel_button) val) val)
      (pref ptr :<G>tk<F>ile<S>election.cancel_button)))
(defun gtk::FileSelection.help-button (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>tk<F>ile<S>election.help_button) val) val)
      (pref ptr :<G>tk<F>ile<S>election.help_button)))
(defun gtk::FileSelection.history-pulldown (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>tk<F>ile<S>election.history_pulldown) val)
             val)
      (pref ptr :<G>tk<F>ile<S>election.history_pulldown)))
(defun gtk::FileSelection.history-menu (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>tk<F>ile<S>election.history_menu) val) val)
      (pref ptr :<G>tk<F>ile<S>election.history_menu)))
(defun gtk::FileSelection.history-list (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>tk<F>ile<S>election.history_list) val) val)
      (pref ptr :<G>tk<F>ile<S>election.history_list)))
(defun gtk::FileSelection.fileop-dialog (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>tk<F>ile<S>election.fileop_dialog) val) val)
      (pref ptr :<G>tk<F>ile<S>election.fileop_dialog)))
(defun gtk::FileSelection.fileop-entry (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>tk<F>ile<S>election.fileop_entry) val) val)
      (pref ptr :<G>tk<F>ile<S>election.fileop_entry)))
(defun gtk::FileSelection.fileop-file (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>tk<F>ile<S>election.fileop_file)
                   (ccl::make-cstring val))
             val)
      (pref ptr :<G>tk<F>ile<S>election.fileop_file)))
(defun gtk::FileSelection.cmpl-state (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>tk<F>ile<S>election.cmpl_state) val) val)
      (pref ptr :<G>tk<F>ile<S>election.cmpl_state)))
(defun gtk::FileSelection.fileop-c-dir (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>tk<F>ile<S>election.fileop_c_dir) val) val)
      (pref ptr :<G>tk<F>ile<S>election.fileop_c_dir)))
(defun gtk::FileSelection.fileop-del-file (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>tk<F>ile<S>election.fileop_del_file) val) val)
      (pref ptr :<G>tk<F>ile<S>election.fileop_del_file)))
(defun gtk::FileSelection.fileop-ren-file (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>tk<F>ile<S>election.fileop_ren_file) val) val)
      (pref ptr :<G>tk<F>ile<S>election.fileop_ren_file)))
(defun gtk::FileSelection.button-area (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>tk<F>ile<S>election.button_area) val) val)
      (pref ptr :<G>tk<F>ile<S>election.button_area)))
(defun gtk::FileSelection.action-area (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>tk<F>ile<S>election.action_area) val) val)
      (pref ptr :<G>tk<F>ile<S>election.action_area)))
(defun gtk::Fixed.children (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>tk<F>ixed.children) val) val)
      (pref ptr :<G>tk<F>ixed.children)))
(defun gtk::FontSelectionDialog.ok-button (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>tk<F>ont<S>election<D>ialog.ok_button) val)
             val)
      (pref ptr :<G>tk<F>ont<S>election<D>ialog.ok_button)))
(defun gtk::FontSelectionDialog.apply-button (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>tk<F>ont<S>election<D>ialog.apply_button) val)
             val)
      (pref ptr :<G>tk<F>ont<S>election<D>ialog.apply_button)))
(defun gtk::FontSelectionDialog.cancel-button (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>tk<F>ont<S>election<D>ialog.cancel_button)
                   val)
             val)
      (pref ptr :<G>tk<F>ont<S>election<D>ialog.cancel_button)))
(defun gtk::HandleBox.shadow-type (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>tk<H>andle<B>ox.shadow_type) val) val)
      (pref ptr :<G>tk<H>andle<B>ox.shadow_type)))
(defun gtk::Layout.bin-window (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>tk<L>ayout.bin_window) val) val)
      (pref ptr :<G>tk<L>ayout.bin_window)))
(defun gtk::Table.children (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>tk<T>able.children) val) val)
      (pref ptr :<G>tk<T>able.children)))
(defun gtk::Table.rows (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>tk<T>able.rows) val) val)
      (pref ptr :<G>tk<T>able.rows)))
(defun gtk::Table.cols (ptr &optional (val nil vp))
  (if vp
      (progn (setf (pref ptr :<G>tk<T>able.cols) val) val)
      (pref ptr :<G>tk<T>able.cols)))

;;; functions

(defun g::list-alloc () (#_g_list_alloc))
(defun g::list-free (a) (#_g_list_free a))
(defun g::list-append (a b) (#_g_list_append a b))
(defun g::list-prepend (a b) (#_g_list_prepend a b))
(defun g::list-insert (a b c) (#_g_list_insert a b c))
(defun g::list-insert-sorted (a b c) (#_g_list_insert_sorted a b c))
(defun g::list-insert-before (a b c) (#_g_list_insert_before a b c))
(defun g::list-concat (a b) (#_g_list_concat a b))
(defun g::list-remove (a b) (#_g_list_remove a b))
(defun g::list-remove-all (a b) (#_g_list_remove_all a b))
(defun g::list-reverse (a) (#_g_list_reverse a))
(defun g::list-copy (a) (#_g_list_copy a))
(defun g::list-nth (a b) (#_g_list_nth a b))
(defun g::list-find (a b) (#_g_list_find a b))
(defun g::list-position (a b) (#_g_list_position a b))
(defun pango::coverage-ref (a) (#_pango_coverage_ref a))
(defun pango::coverage-unref (a) (#_pango_coverage_unref a))
(defun pango::coverage-copy (a) (#_pango_coverage_copy a))
(defun pango::coverage-get (a b) (#_pango_coverage_get a b))
(defun pango::coverage-set (a b c) (#_pango_coverage_set a b c))
(defun pango::coverage-max (a b) (#_pango_coverage_max a b))
(defun pango::coverage-to-bytes (a b c)
  (rlet ((r1 :int c))
        (let ((z (#_pango_coverage_to_bytes a b r1)))
          (values z (%get-signed-long r1)))))
(defun pango::coverage-from-bytes (a b)
  (with-cstrs ((s1 a)) (#_pango_coverage_from_bytes s1 b)))
(defun g::signal-emit-by-name (a b)
  (with-cstrs ((s1 b)) (#_g_signal_emit_by_name a s1)))
(defun g::signal-stop-emission-by-name (a b)
  (with-cstrs ((s1 b)) (#_g_signal_stop_emission_by_name a s1)))
(defun g::signal-connect-data (a b c d e f)
  (with-cstrs ((s1 b)) (#_g_signal_connect_data a s1 c d e f)))
(defun g::signal-handler-disconnect (a b) (#_g_signal_handler_disconnect a b))
(defun g::object-set-property (a b c)
  (with-cstrs ((s1 b)) (#_g_object_set_property a s1 c)))
(defun g::object-get-property (a b c)
  (with-cstrs ((s1 b)) (#_g_object_get_property a s1 c)))
(defun g::object-freeze-notify (a) (#_g_object_freeze_notify a))
(defun g::object-notify (a b) (with-cstrs ((s1 b)) (#_g_object_notify a s1)))
(defun g::object-thaw-notify (a) (#_g_object_thaw_notify a))
(defun g::object-unref (a) (#_g_object_unref a))
(defun g::object-get-data (a b)
  (with-cstrs ((s1 b)) (#_g_object_get_data a s1)))
(defun g::object-set-data (a b c)
  (with-cstrs ((s1 b)) (#_g_object_set_data a s1 c)))
(defun pango::language-from-string (a)
  (with-cstrs ((s1 a)) (#_pango_language_from_string s1)))
(defun pango::language-matches (a b)
  (with-cstrs ((s1 b))
              (let ((z (#_pango_language_matches a s1)))
                (values (if (= 1 z) t nil)))))
(defun pango::font-description-new () (#_pango_font_description_new))
(defun pango::font-description-copy (a) (#_pango_font_description_copy a))
(defun pango::font-description-copy-static (a)
  (#_pango_font_description_copy_static a))
(defun pango::font-description-hash (a) (#_pango_font_description_hash a))
(defun pango::font-description-equal (a b)
  (let ((z (#_pango_font_description_equal a b))) (values (if (= 1 z) t nil))))
(defun pango::font-description-free (a) (#_pango_font_description_free a))
(defun pango::font-descriptions-free (a b) (#_pango_font_descriptions_free a b))
(defun pango::font-description-set-family (a b)
  (with-cstrs ((s1 b)) (#_pango_font_description_set_family a s1)))
(defun pango::font-description-set-family-static (a b)
  (with-cstrs ((s1 b)) (#_pango_font_description_set_family_static a s1)))
(defun pango::font-description-get-family (a)
  (let ((z (#_pango_font_description_get_family a))) (values (%get-cstring z))))
(defun pango::font-description-set-style (a b)
  (#_pango_font_description_set_style a b))
(defun pango::font-description-get-style (a)
  (#_pango_font_description_get_style a))
(defun pango::font-description-set-variant (a b)
  (#_pango_font_description_set_variant a b))
(defun pango::font-description-get-variant (a)
  (#_pango_font_description_get_variant a))
(defun pango::font-description-set-weight (a b)
  (#_pango_font_description_set_weight a b))
(defun pango::font-description-get-weight (a)
  (#_pango_font_description_get_weight a))
(defun pango::font-description-set-stretch (a b)
  (#_pango_font_description_set_stretch a b))
(defun pango::font-description-get-stretch (a)
  (#_pango_font_description_get_stretch a))
(defun pango::font-description-set-size (a b)
  (#_pango_font_description_set_size a b))
(defun pango::font-description-get-size (a)
  (#_pango_font_description_get_size a))
(defun pango::font-description-get-set-fields (a)
  (#_pango_font_description_get_set_fields a))
(defun pango::font-description-unset-fields (a b)
  (#_pango_font_description_unset_fields a b))
(defun pango::font-description-merge (a b c)
  (#_pango_font_description_merge a b (if c (if (eq c 0) 0 1) 0)))
(defun pango::font-description-merge-static (a b c)
  (#_pango_font_description_merge_static a b (if c (if (eq c 0) 0 1) 0)))
(defun pango::font-description-better-match (a b c)
  (let ((z (#_pango_font_description_better_match a b c)))
    (values (if (= 1 z) t nil))))
(defun pango::font-description-from-string (a)
  (with-cstrs ((s1 a)) (#_pango_font_description_from_string s1)))
(defun pango::font-description-to-string (a)
  (let ((z (#_pango_font_description_to_string a))) (values (%get-cstring z))))
(defun pango::font-description-to-filename (a)
  (let ((z (#_pango_font_description_to_filename a)))
    (values (%get-cstring z))))
(defun pango::font-metrics-get-type () (#_pango_font_metrics_get_type))
(defun pango::font-metrics-ref (a) (#_pango_font_metrics_ref a))
(defun pango::font-metrics-unref (a) (#_pango_font_metrics_unref a))
(defun pango::font-metrics-get-ascent (a) (#_pango_font_metrics_get_ascent a))
(defun pango::font-metrics-get-descent (a) (#_pango_font_metrics_get_descent a))
(defun pango::font-metrics-get-approximate-char-width (a)
  (#_pango_font_metrics_get_approximate_char_width a))
(defun pango::font-metrics-get-approximate-digit-width (a)
  (#_pango_font_metrics_get_approximate_digit_width a))
(defun pango::font-family-get-type () (#_pango_font_family_get_type))
(defun pango::font-family-list-faces (a b c)
  (rlet ((r1 :int c))
        (let ((z (#_pango_font_family_list_faces a b r1)))
          (values z (%get-signed-long r1)))))
(defun pango::font-family-get-name (a)
  (let ((z (#_pango_font_family_get_name a))) (values (%get-cstring z))))
(defun pango::font-face-get-type () (#_pango_font_face_get_type))
(defun pango::font-face-describe (a) (#_pango_font_face_describe a))
(defun pango::font-face-get-face-name (a)
  (let ((z (#_pango_font_face_get_face_name a))) (values (%get-cstring z))))
(defun pango::font-get-type () (#_pango_font_get_type))
(defun pango::font-describe (a) (#_pango_font_describe a))
(defun pango::font-get-coverage (a b) (#_pango_font_get_coverage a b))
(defun pango::font-find-shaper (a b c) (#_pango_font_find_shaper a b c))
(defun pango::font-get-metrics (a b) (#_pango_font_get_metrics a b))
(defun pango::font-get-glyph-extents (a b c d)
  (#_pango_font_get_glyph_extents a b c d))
(defun pango::color-get-type () (#_pango_color_get_type))
(defun pango::color-copy (a) (#_pango_color_copy a))
(defun pango::color-free (a) (#_pango_color_free a))
(defun pango::color-parse (a b)
  (with-cstrs ((s1 b))
              (let ((z (#_pango_color_parse a s1)))
                (values (if (= 1 z) t nil)))))
(defun pango::attr-type-register (a)
  (with-cstrs ((s1 a)) (#_pango_attr_type_register s1)))
(defun pango::attribute-copy (a) (#_pango_attribute_copy a))
(defun pango::attribute-destroy (a) (#_pango_attribute_destroy a))
(defun pango::attribute-equal (a b)
  (let ((z (#_pango_attribute_equal a b))) (values (if (= 1 z) t nil))))
(defun pango::attr-language-new (a) (#_pango_attr_language_new a))
(defun pango::attr-family-new (a)
  (with-cstrs ((s1 a)) (#_pango_attr_family_new s1)))
(defun pango::attr-foreground-new (a b c) (#_pango_attr_foreground_new a b c))
(defun pango::attr-background-new (a b c) (#_pango_attr_background_new a b c))
(defun pango::attr-size-new (a) (#_pango_attr_size_new a))
(defun pango::attr-style-new (a) (#_pango_attr_style_new a))
(defun pango::attr-weight-new (a) (#_pango_attr_weight_new a))
(defun pango::attr-variant-new (a) (#_pango_attr_variant_new a))
(defun pango::attr-stretch-new (a) (#_pango_attr_stretch_new a))
(defun pango::attr-font-desc-new (a) (#_pango_attr_font_desc_new a))
(defun pango::attr-underline-new (a) (#_pango_attr_underline_new a))
(defun pango::attr-strikethrough-new (a)
  (#_pango_attr_strikethrough_new (if a (if (eq a 0) 0 1) 0)))
(defun pango::attr-rise-new (a) (#_pango_attr_rise_new a))
(defun pango::attr-shape-new (a b) (#_pango_attr_shape_new a b))
(defun pango::attr-scale-new (a)
  (#_pango_attr_scale_new (coerce a 'double-float)))
(defun pango::attr-list-get-type () (#_pango_attr_list_get_type))
(defun pango::attr-list-new () (#_pango_attr_list_new))
(defun pango::attr-list-ref (a) (#_pango_attr_list_ref a))
(defun pango::attr-list-unref (a) (#_pango_attr_list_unref a))
(defun pango::attr-list-copy (a) (#_pango_attr_list_copy a))
(defun pango::attr-list-insert (a b) (#_pango_attr_list_insert a b))
(defun pango::attr-list-insert-before (a b)
  (#_pango_attr_list_insert_before a b))
(defun pango::attr-list-change (a b) (#_pango_attr_list_change a b))
(defun pango::attr-list-splice (a b c d) (#_pango_attr_list_splice a b c d))
(defun pango::attr-list-get-iterator (a) (#_pango_attr_list_get_iterator a))
(defun pango::attr-iterator-range (a b c)
  (rlet ((r1 :int b) (r2 :int c))
        (let ((z (#_pango_attr_iterator_range a r1 r2)))
          (values z (%get-signed-long r1) (%get-signed-long r2)))))
(defun pango::attr-iterator-next (a)
  (let ((z (#_pango_attr_iterator_next a))) (values (if (= 1 z) t nil))))
(defun pango::attr-iterator-copy (a) (#_pango_attr_iterator_copy a))
(defun pango::attr-iterator-destroy (a) (#_pango_attr_iterator_destroy a))
(defun pango::attr-iterator-get (a b) (#_pango_attr_iterator_get a b))
(defun pango::attr-iterator-get-font (a b c d)
  (#_pango_attr_iterator_get_font a b c d))
(defun pango::parse-markup (a b c d e f g)
  (with-cstrs ((s1 a))
              (let ((z (#_pango_parse_markup s1 b c d e f g)))
                (values (if (= 1 z) t nil)))))
(defun pango::break (a b c d e)
  (with-cstrs ((s1 a)) (#_pango_break s1 b c d e)))
(defun pango::find-paragraph-boundary (a b c d)
  (with-cstrs ((s1 a))
              (rlet ((r1 :int c) (r2 :int d))
                    (let ((z (#_pango_find_paragraph_boundary s1 b r1 r2)))
                      (values z (%get-signed-long r1) (%get-signed-long r2))))))
(defun pango::get-log-attrs (a b c d e f)
  (with-cstrs ((s1 a)) (#_pango_get_log_attrs s1 b c d e f)))
(defun pango::fontset-get-font (a b) (#_pango_fontset_get_font a b))
(defun pango::fontset-get-metrics (a) (#_pango_fontset_get_metrics a))
(defun pango::font-map-load-font (a b c) (#_pango_font_map_load_font a b c))
(defun pango::font-map-load-fontset (a b c d)
  (#_pango_font_map_load_fontset a b c d))
(defun pango::font-map-list-families (a b c)
  (rlet ((r1 :int c))
        (let ((z (#_pango_font_map_list_families a b r1)))
          (values z (%get-signed-long r1)))))
(defun pango::context-list-families (a b c)
  (rlet ((r1 :int c))
        (let ((z (#_pango_context_list_families a b r1)))
          (values z (%get-signed-long r1)))))
(defun pango::context-load-font (a b) (#_pango_context_load_font a b))
(defun pango::context-load-fontset (a b c) (#_pango_context_load_fontset a b c))
(defun pango::context-get-metrics (a b c) (#_pango_context_get_metrics a b c))
(defun pango::context-set-font-description (a b)
  (#_pango_context_set_font_description a b))
(defun pango::context-get-font-description (a)
  (#_pango_context_get_font_description a))
(defun pango::context-get-language (a) (#_pango_context_get_language a))
(defun pango::context-set-language (a b) (#_pango_context_set_language a b))
(defun pango::context-set-base-dir (a b) (#_pango_context_set_base_dir a b))
(defun pango::context-get-base-dir (a) (#_pango_context_get_base_dir a))
(defun pango::itemize (a b c d e f)
  (with-cstrs ((s1 b)) (#_pango_itemize a s1 c d e f)))
(defun pango::glyph-string-new () (#_pango_glyph_string_new))
(defun pango::glyph-string-set-size (a b) (#_pango_glyph_string_set_size a b))
(defun pango::glyph-string-get-type () (#_pango_glyph_string_get_type))
(defun pango::glyph-string-copy (a) (#_pango_glyph_string_copy a))
(defun pango::glyph-string-free (a) (#_pango_glyph_string_free a))
(defun pango::glyph-string-extents (a b c d)
  (#_pango_glyph_string_extents a b c d))
(defun pango::glyph-string-extents-range (a b c d e f)
  (#_pango_glyph_string_extents_range a b c d e f))
(defun pango::glyph-string-get-logical-widths (a b c d e)
  (with-cstrs ((s1 b))
              (rlet ((r1 :int e))
                    (let ((z
                           (#_pango_glyph_string_get_logical_widths a s1 c d
                            r1)))
                      (values z (%get-signed-long r1))))))
(defun pango::glyph-string-index-to-x (a b c d e f g)
  (with-cstrs ((s1 b))
              (rlet ((r1 :int g))
                    (let ((z
                           (#_pango_glyph_string_index_to_x a s1 c d e
                            (if f (if (eq f 0) 0 1) 0) r1)))
                      (values z (%get-signed-long r1))))))
(defun pango::glyph-string-x-to-index (a b c d e f g)
  (with-cstrs ((s1 b))
              (rlet ((r1 :int f) (r2 :int g))
                    (let ((z
                           (#_pango_glyph_string_x_to_index a s1 c d e r1 r2)))
                      (values z (%get-signed-long r1) (%get-signed-long r2))))))
(defun pango::shape (a b c d) (with-cstrs ((s1 a)) (#_pango_shape s1 b c d)))
(defun pango::reorder-items (a) (#_pango_reorder_items a))
(defun pango::tab-array-new (a b)
  (#_pango_tab_array_new a (if b (if (eq b 0) 0 1) 0)))
(defun pango::tab-array-new-with-positions (a b c d)
  (#_pango_tab_array_new_with_positions a (if b (if (eq b 0) 0 1) 0) c d))
(defun pango::tab-array-get-type () (#_pango_tab_array_get_type))
(defun pango::tab-array-copy (a) (#_pango_tab_array_copy a))
(defun pango::tab-array-free (a) (#_pango_tab_array_free a))
(defun pango::tab-array-get-size (a) (#_pango_tab_array_get_size a))
(defun pango::tab-array-resize (a b) (#_pango_tab_array_resize a b))
(defun pango::tab-array-set-tab (a b c d) (#_pango_tab_array_set_tab a b c d))
(defun pango::tab-array-get-tab (a b c d)
  (rlet ((r1 :int c) (r2 :int d))
        (let ((z (#_pango_tab_array_get_tab a b r1 r2)))
          (values z (%get-signed-long r1) (%get-signed-long r2)))))
(defun pango::tab-array-get-tabs (a b c) (#_pango_tab_array_get_tabs a b c))
(defun pango::tab-array-get-positions-in-pixels (a)
  (let ((z (#_pango_tab_array_get_positions_in_pixels a)))
    (values (if (= 1 z) t nil))))
(defun pango::layout-get-type () (#_pango_layout_get_type))
(defun pango::layout-new (a) (#_pango_layout_new a))
(defun pango::layout-copy (a) (#_pango_layout_copy a))
(defun pango::layout-get-context (a) (#_pango_layout_get_context a))
(defun pango::layout-set-attributes (a b) (#_pango_layout_set_attributes a b))
(defun pango::layout-get-attributes (a) (#_pango_layout_get_attributes a))
(defun pango::layout-set-text (a b c)
  (with-cstrs ((s1 b)) (#_pango_layout_set_text a s1 c)))
(defun pango::layout-get-text (a)
  (let ((z (#_pango_layout_get_text a))) (values (%get-cstring z))))
(defun pango::layout-set-markup (a b c)
  (with-cstrs ((s1 b)) (#_pango_layout_set_markup a s1 c)))
(defun pango::layout-set-markup-with-accel (a b c d e)
  (with-cstrs ((s1 b)) (#_pango_layout_set_markup_with_accel a s1 c d e)))
(defun pango::layout-set-font-description (a b)
  (#_pango_layout_set_font_description a b))
(defun pango::layout-set-width (a b) (#_pango_layout_set_width a b))
(defun pango::layout-get-width (a) (#_pango_layout_get_width a))
(defun pango::layout-set-wrap (a b) (#_pango_layout_set_wrap a b))
(defun pango::layout-get-wrap (a) (#_pango_layout_get_wrap a))
(defun pango::layout-set-indent (a b) (#_pango_layout_set_indent a b))
(defun pango::layout-get-indent (a) (#_pango_layout_get_indent a))
(defun pango::layout-set-spacing (a b) (#_pango_layout_set_spacing a b))
(defun pango::layout-get-spacing (a) (#_pango_layout_get_spacing a))
(defun pango::layout-set-justify (a b)
  (#_pango_layout_set_justify a (if b (if (eq b 0) 0 1) 0)))
(defun pango::layout-get-justify (a)
  (let ((z (#_pango_layout_get_justify a))) (values (if (= 1 z) t nil))))
(defun pango::layout-set-alignment (a b) (#_pango_layout_set_alignment a b))
(defun pango::layout-get-alignment (a) (#_pango_layout_get_alignment a))
(defun pango::layout-set-tabs (a b) (#_pango_layout_set_tabs a b))
(defun pango::layout-get-tabs (a) (#_pango_layout_get_tabs a))
(defun pango::layout-set-single-paragraph-mode (a b)
  (#_pango_layout_set_single_paragraph_mode a (if b (if (eq b 0) 0 1) 0)))
(defun pango::layout-get-single-paragraph-mode (a)
  (let ((z (#_pango_layout_get_single_paragraph_mode a)))
    (values (if (= 1 z) t nil))))
(defun pango::layout-context-changed (a) (#_pango_layout_context_changed a))
(defun pango::layout-get-log-attrs (a b c)
  (rlet ((r1 :int c))
        (let ((z (#_pango_layout_get_log_attrs a b r1)))
          (values z (%get-signed-long r1)))))
(defun pango::layout-index-to-pos (a b c) (#_pango_layout_index_to_pos a b c))
(defun pango::layout-get-cursor-pos (a b c d)
  (#_pango_layout_get_cursor_pos a b c d))
(defun pango::layout-move-cursor-visually (a b c d e f g)
  (rlet ((r1 :int f) (r2 :int g))
        (let ((z
               (#_pango_layout_move_cursor_visually a
                (if b (if (eq b 0) 0 1) 0) c d e r1 r2)))
          (values z (%get-signed-long r1) (%get-signed-long r2)))))
(defun pango::layout-xy-to-index (a b c d e)
  (rlet ((r1 :int d) (r2 :int e))
        (let ((z (#_pango_layout_xy_to_index a b c r1 r2)))
          (values (if (= 1 z) t nil)
                  (%get-signed-long r1)
                  (%get-signed-long r2)))))
(defun pango::layout-get-extents (a b c) (#_pango_layout_get_extents a b c))
(defun pango::layout-get-pixel-extents (a b c)
  (#_pango_layout_get_pixel_extents a b c))
(defun pango::layout-get-size (a b c)
  (rlet ((r1 :int b) (r2 :int c))
        (let ((z (#_pango_layout_get_size a r1 r2)))
          (values z (%get-signed-long r1) (%get-signed-long r2)))))
(defun pango::layout-get-pixel-size (a b c)
  (rlet ((r1 :int b) (r2 :int c))
        (let ((z (#_pango_layout_get_pixel_size a r1 r2)))
          (values z (%get-signed-long r1) (%get-signed-long r2)))))
(defun pango::layout-get-line-count (a) (#_pango_layout_get_line_count a))
(defun pango::layout-get-line (a b) (#_pango_layout_get_line a b))
(defun pango::layout-get-lines (a) (#_pango_layout_get_lines a))
(defun pango::layout-line-ref (a) (#_pango_layout_line_ref a))
(defun pango::layout-line-unref (a) (#_pango_layout_line_unref a))
(defun pango::layout-line-x-to-index (a b c d)
  (rlet ((r1 :int c) (r2 :int d))
        (let ((z (#_pango_layout_line_x_to_index a b r1 r2)))
          (values (if (= 1 z) t nil)
                  (%get-signed-long r1)
                  (%get-signed-long r2)))))
(defun pango::layout-line-index-to-x (a b c d)
  (rlet ((r1 :int d))
        (let ((z
               (#_pango_layout_line_index_to_x a b (if c (if (eq c 0) 0 1) 0)
                r1)))
          (values z (%get-signed-long r1)))))
(defun pango::layout-line-get-x-ranges (a b c d e)
  (rlet ((r1 :int e))
        (let ((z (#_pango_layout_line_get_x_ranges a b c d r1)))
          (values z (%get-signed-long r1)))))
(defun pango::layout-line-get-extents (a b c)
  (#_pango_layout_line_get_extents a b c))
(defun pango::layout-line-get-pixel-extents (a b c)
  (#_pango_layout_line_get_pixel_extents a b c))
(defun pango::layout-get-iter (a) (#_pango_layout_get_iter a))
(defun pango::layout-iter-free (a) (#_pango_layout_iter_free a))
(defun pango::layout-iter-get-index (a) (#_pango_layout_iter_get_index a))
(defun pango::layout-iter-get-run (a) (#_pango_layout_iter_get_run a))
(defun pango::layout-iter-get-line (a) (#_pango_layout_iter_get_line a))
(defun pango::layout-iter-at-last-line (a)
  (let ((z (#_pango_layout_iter_at_last_line a))) (values (if (= 1 z) t nil))))
(defun pango::layout-iter-next-char (a)
  (let ((z (#_pango_layout_iter_next_char a))) (values (if (= 1 z) t nil))))
(defun pango::layout-iter-next-cluster (a)
  (let ((z (#_pango_layout_iter_next_cluster a))) (values (if (= 1 z) t nil))))
(defun pango::layout-iter-next-run (a)
  (let ((z (#_pango_layout_iter_next_run a))) (values (if (= 1 z) t nil))))
(defun pango::layout-iter-next-line (a)
  (let ((z (#_pango_layout_iter_next_line a))) (values (if (= 1 z) t nil))))
(defun pango::layout-iter-get-char-extents (a b)
  (#_pango_layout_iter_get_char_extents a b))
(defun pango::layout-iter-get-cluster-extents (a b c)
  (#_pango_layout_iter_get_cluster_extents a b c))
(defun pango::layout-iter-get-run-extents (a b c)
  (#_pango_layout_iter_get_run_extents a b c))
(defun pango::layout-iter-get-line-extents (a b c)
  (#_pango_layout_iter_get_line_extents a b c))
(defun pango::layout-iter-get-line-yrange (a b c)
  (rlet ((r1 :int b) (r2 :int c))
        (let ((z (#_pango_layout_iter_get_line_yrange a r1 r2)))
          (values z (%get-signed-long r1) (%get-signed-long r2)))))
(defun pango::layout-iter-get-layout-extents (a b c)
  (#_pango_layout_iter_get_layout_extents a b c))
(defun pango::layout-iter-get-baseline (a) (#_pango_layout_iter_get_baseline a))
(defun gdk::colormap-get-type () (#_gdk_colormap_get_type))
(defun gdk::colormap-new (a b)
  (#_gdk_colormap_new a (if b (if (eq b 0) 0 1) 0)))
(defun gdk::colormap-get-system () (#_gdk_colormap_get_system))
(defun gdk::colormap-get-system-size () (#_gdk_colormap_get_system_size))
(defun gdk::colormap-alloc-color (a b c d)
  (let ((z
         (#_gdk_colormap_alloc_color a b (if c (if (eq c 0) 0 1) 0)
          (if d (if (eq d 0) 0 1) 0))))
    (values (if (= 1 z) t nil))))
(defun gdk::colormap-get-visual (a) (#_gdk_colormap_get_visual a))
(defun gdk::color-copy (a) (#_gdk_color_copy a))
(defun gdk::color-free (a) (#_gdk_color_free a))
(defun gdk::color-parse (a b) (with-cstrs ((s1 a)) (#_gdk_color_parse s1 b)))
(defun gdk::color-white (a b) (#_gdk_color_white a b))
(defun gdk::color-black (a b) (#_gdk_color_black a b))
(defun gdk::color-alloc (a b) (#_gdk_color_alloc a b))
(defun gdk::color-change (a b) (#_gdk_color_change a b))
(defun gdk::cursor-new (a) (#_gdk_cursor_new a))
(defun gdk::cursor-new-from-pixmap (a b c d e f)
  (#_gdk_cursor_new_from_pixmap a b c d e f))
(defun gdk::cursor-ref (a) (#_gdk_cursor_ref a))
(defun gdk::cursor-unref (a) (#_gdk_cursor_unref a))
(defun gdk::drag-context-get-type () (#_gdk_drag_context_get_type))
(defun gdk::drag-context-new () (#_gdk_drag_context_new))
(defun gdk::drag-context-ref (a) (#_gdk_drag_context_ref a))
(defun gdk::drag-context-unref (a) (#_gdk_drag_context_unref a))
(defun gdk::drag-status (a b c) (#_gdk_drag_status a b c))
(defun gdk::drop-reply (a b c)
  (#_gdk_drop_reply a (if b (if (eq b 0) 0 1) 0) c))
(defun gdk::drop-finish (a b c)
  (#_gdk_drop_finish a (if b (if (eq b 0) 0 1) 0) c))
(defun gdk::drag-get-selection (a) (#_gdk_drag_get_selection a))
(defun gdk::drag-begin (a b) (#_gdk_drag_begin a b))
(defun gdk::drag-get-protocol (a b)
  (rlet ((r1 :int b))
        (let ((z (#_gdk_drag_get_protocol a r1)))
          (values z (%get-signed-long r1)))))
(defun gdk::drag-find-window (a b c d e f)
  (rlet ((r1 :int f))
        (let ((z (#_gdk_drag_find_window a b c d e r1)))
          (values z (%get-signed-long r1)))))
(defun gdk::drag-motion (a b c d e f g h)
  (let ((z (#_gdk_drag_motion a b c d e f g h))) (values (if (= 1 z) t nil))))
(defun gdk::drag-drop (a b) (#_gdk_drag_drop a b))
(defun gdk::drag-abort (a b) (#_gdk_drag_abort a b))
(defun gdk::device-get-type () (#_gdk_device_get_type))
(defun gdk::devices-list () (#_gdk_devices_list))
(defun gdk::device-set-source (a b) (#_gdk_device_set_source a b))
(defun gdk::device-set-mode (a b)
  (let ((z (#_gdk_device_set_mode a b))) (values (if (= 1 z) t nil))))
(defun gdk::device-set-key (a b c d) (#_gdk_device_set_key a b c d))
(defun gdk::device-set-axis-use (a b c) (#_gdk_device_set_axis_use a b c))
(defun gdk::device-get-state (a b c d)
  (rlet ((r1 :double (coerce c 'double-float)) (r2 :int d))
        (let ((z (#_gdk_device_get_state a b r1 r2)))
          (values z (%get-double-float r1) (%get-signed-long r2)))))
(defun gdk::device-get-history (a b c d e f)
  (rlet ((r1 :int f))
        (let ((z (#_gdk_device_get_history a b c d e r1)))
          (values (if (= 1 z) t nil) (%get-signed-long r1)))))
(defun gdk::device-free-history (a b) (#_gdk_device_free_history a b))
(defun gdk::device-get-axis (a b c d)
  (rlet ((r1 :double (coerce b 'double-float))
         (r2 :double (coerce d 'double-float)))
        (let ((z (#_gdk_device_get_axis a r1 c r2)))
          (values (if (= 1 z) t nil)
                  (%get-double-float r1)
                  (%get-double-float r2)))))
(defun gdk::input-set-extension-events (a b c)
  (#_gdk_input_set_extension_events a b c))
(defun gdk::device-get-core-pointer () (#_gdk_device_get_core_pointer))
(defun gdk::event-get-type () (#_gdk_event_get_type))
(defun gdk::events-pending ()
  (let ((z (#_gdk_events_pending))) (values (if (= 1 z) t nil))))
(defun gdk::event-get () (#_gdk_event_get))
(defun gdk::event-peek () (#_gdk_event_peek))
(defun gdk::event-get-graphics-expose (a) (#_gdk_event_get_graphics_expose a))
(defun gdk::event-put (a) (#_gdk_event_put a))
(defun gdk::event-copy (a) (#_gdk_event_copy a))
(defun gdk::event-free (a) (#_gdk_event_free a))
(defun gdk::event-get-time (a) (#_gdk_event_get_time a))
(defun gdk::event-get-state (a b)
  (rlet ((r1 :int b))
        (let ((z (#_gdk_event_get_state a r1)))
          (values (if (= 1 z) t nil) (%get-signed-long r1)))))
(defun gdk::event-get-coords (a b c)
  (rlet ((r1 :double (coerce b 'double-float))
         (r2 :double (coerce c 'double-float)))
        (let ((z (#_gdk_event_get_coords a r1 r2)))
          (values (if (= 1 z) t nil)
                  (%get-double-float r1)
                  (%get-double-float r2)))))
(defun gdk::event-get-root-coords (a b c)
  (rlet ((r1 :double (coerce b 'double-float))
         (r2 :double (coerce c 'double-float)))
        (let ((z (#_gdk_event_get_root_coords a r1 r2)))
          (values (if (= 1 z) t nil)
                  (%get-double-float r1)
                  (%get-double-float r2)))))
(defun gdk::event-get-axis (a b c)
  (rlet ((r1 :double (coerce c 'double-float)))
        (let ((z (#_gdk_event_get_axis a b r1)))
          (values (if (= 1 z) t nil) (%get-double-float r1)))))
(defun gdk::event-handler-set (a b c) (#_gdk_event_handler_set a b c))
(defun gdk::set-show-events (a)
  (#_gdk_set_show_events (if a (if (eq a 0) 0 1) 0)))
(defun gdk::get-show-events ()
  (let ((z (#_gdk_get_show_events))) (values (if (= 1 z) t nil))))
(defun gdk::add-client-message-filter (a b c)
  (#_gdk_add_client_message_filter a b c))
(defun gdk::setting-get (a b)
  (with-cstrs ((s1 a))
              (let ((z (#_gdk_setting_get s1 b))) (values (if (= 1 z) t nil)))))
(defun gdk::gc-get-type () (#_gdk_gc_get_type))
(defun gdk::gc-new (a) (#_gdk_gc_new a))
(defun gdk::gc-new-with-values (a b c) (#_gdk_gc_new_with_values a b c))
(defun gdk::gc-get-values (a b) (#_gdk_gc_get_values a b))
(defun gdk::gc-set-values (a b c) (#_gdk_gc_set_values a b c))
(defun gdk::gc-set-foreground (a b) (#_gdk_gc_set_foreground a b))
(defun gdk::gc-set-background (a b) (#_gdk_gc_set_background a b))
(defun gdk::gc-set-font (a b) (#_gdk_gc_set_font a b))
(defun gdk::gc-set-function (a b) (#_gdk_gc_set_function a b))
(defun gdk::gc-set-fill (a b) (#_gdk_gc_set_fill a b))
(defun gdk::gc-set-tile (a b) (#_gdk_gc_set_tile a b))
(defun gdk::gc-set-stipple (a b) (#_gdk_gc_set_stipple a b))
(defun gdk::gc-set-ts-origin (a b c) (#_gdk_gc_set_ts_origin a b c))
(defun gdk::gc-set-clip-origin (a b c) (#_gdk_gc_set_clip_origin a b c))
(defun gdk::gc-set-clip-mask (a b) (#_gdk_gc_set_clip_mask a b))
(defun gdk::gc-set-clip-rectangle (a b) (#_gdk_gc_set_clip_rectangle a b))
(defun gdk::gc-set-clip-region (a b) (#_gdk_gc_set_clip_region a b))
(defun gdk::gc-set-subwindow (a b) (#_gdk_gc_set_subwindow a b))
(defun gdk::gc-set-exposures (a b)
  (#_gdk_gc_set_exposures a (if b (if (eq b 0) 0 1) 0)))
(defun gdk::gc-set-line-attributes (a b c d e)
  (#_gdk_gc_set_line_attributes a b c d e))
(defun gdk::gc-set-dashes (a b c d) (#_gdk_gc_set_dashes a b c d))
(defun gdk::gc-offset (a b c) (#_gdk_gc_offset a b c))
(defun gdk::gc-copy (a b) (#_gdk_gc_copy a b))
(defun gdk::gc-set-colormap (a b) (#_gdk_gc_set_colormap a b))
(defun gdk::gc-get-colormap (a) (#_gdk_gc_get_colormap a))
(defun gdk::gc-set-rgb-fg-color (a b) (#_gdk_gc_set_rgb_fg_color a b))
(defun gdk::gc-set-rgb-bg-color (a b) (#_gdk_gc_set_rgb_bg_color a b))
(defun gdk::rgb-xpixel-from-rgb (a) (#_gdk_rgb_xpixel_from_rgb a))
(defun gdk::rgb-gc-set-foreground (a b) (#_gdk_rgb_gc_set_foreground a b))
(defun gdk::rgb-gc-set-background (a b) (#_gdk_rgb_gc_set_background a b))
(defun gdk::draw-rgb-image (a b c d e f g h i)
  (with-cstrs ((s1 h)) (#_gdk_draw_rgb_image a b c d e f g s1 i)))
(defun gdk::draw-rgb-image-dithalign (a b c d e f g h i j k)
  (with-cstrs ((s1 h)) (#_gdk_draw_rgb_image_dithalign a b c d e f g s1 i j k)))
(defun gdk::draw-rgb-32-image (a b c d e f g h i)
  (with-cstrs ((s1 h)) (#_gdk_draw_rgb_32_image a b c d e f g s1 i)))
(defun gdk::draw-gray-image (a b c d e f g h i)
  (with-cstrs ((s1 h)) (#_gdk_draw_gray_image a b c d e f g s1 i)))
(defun gdk::draw-indexed-image (a b c d e f g h i j)
  (with-cstrs ((s1 h)) (#_gdk_draw_indexed_image a b c d e f g s1 i j)))
(defun gdk::rgb-cmap-new (a b) (#_gdk_rgb_cmap_new a b))
(defun gdk::rgb-set-verbose (a)
  (#_gdk_rgb_set_verbose (if a (if (eq a 0) 0 1) 0)))
(defun gdk::rgb-set-install (a)
  (#_gdk_rgb_set_install (if a (if (eq a 0) 0 1) 0)))
(defun gdk::rgb-set-min-colors (a) (#_gdk_rgb_set_min_colors a))
(defun gdk::rgb-get-colormap () (#_gdk_rgb_get_colormap))
(defun gdk::rgb-get-visual () (#_gdk_rgb_get_visual))
(defun gdk::rgb-ditherable ()
  (let ((z (#_gdk_rgb_ditherable))) (values (if (= 1 z) t nil))))
(defun gdk::pixbuf-get-colorspace (a) (#_gdk_pixbuf_get_colorspace a))
(defun gdk::pixbuf-get-n-channels (a) (#_gdk_pixbuf_get_n_channels a))
(defun gdk::pixbuf-get-has-alpha (a)
  (let ((z (#_gdk_pixbuf_get_has_alpha a))) (values (if (= 1 z) t nil))))
(defun gdk::pixbuf-get-bits-per-sample (a) (#_gdk_pixbuf_get_bits_per_sample a))
(defun gdk::pixbuf-get-pixels (a)
  (let ((z (#_gdk_pixbuf_get_pixels a))) (values (%get-cstring z))))
(defun gdk::pixbuf-get-width (a) (#_gdk_pixbuf_get_width a))
(defun gdk::pixbuf-get-height (a) (#_gdk_pixbuf_get_height a))
(defun gdk::pixbuf-get-rowstride (a) (#_gdk_pixbuf_get_rowstride a))
(defun gdk::pixbuf-new (a b c d e)
  (#_gdk_pixbuf_new a (if b (if (eq b 0) 0 1) 0) c d e))
(defun gdk::pixbuf-copy (a) (#_gdk_pixbuf_copy a))
(defun gdk::pixbuf-new-from-file (a b)
  (with-cstrs ((s1 a)) (#_gdk_pixbuf_new_from_file s1 b)))
(defun gdk::pixbuf-new-from-data (a b c d e f g h i)
  (#_gdk_pixbuf_new_from_data a b (if c (if (eq c 0) 0 1) 0) d e f g h i))
(defun gdk::pixbuf-new-from-xpm-data (a) (#_gdk_pixbuf_new_from_xpm_data a))
(defun gdk::pixbuf-new-from-inline (a b c d)
  (#_gdk_pixbuf_new_from_inline a b (if c (if (eq c 0) 0 1) 0) d))
(defun gdk::pixbuf-fill (a b) (#_gdk_pixbuf_fill a b))
(defun gdk::pixbuf-save (a b c d)
  (with-cstrs ((s1 b) (s2 c))
              (let ((z (#_gdk_pixbuf_save a s1 s2 d)))
                (values (if (= 1 z) t nil)))))
(defun gdk::pixbuf-savev (a b c d e f)
  (with-cstrs ((s1 b) (s2 c))
              (let ((z (#_gdk_pixbuf_savev a s1 s2 d e f)))
                (values (if (= 1 z) t nil)))))
(defun gdk::pixbuf-add-alpha (a b c d e)
  (#_gdk_pixbuf_add_alpha a (if b (if (eq b 0) 0 1) 0) c d e))
(defun gdk::pixbuf-copy-area (a b c d e f g h)
  (#_gdk_pixbuf_copy_area a b c d e f g h))
(defun gdk::pixbuf-saturate-and-pixelate (a b c d)
  (#_gdk_pixbuf_saturate_and_pixelate a b (coerce c 'single-float)
   (if d (if (eq d 0) 0 1) 0)))
(defun gdk::pixbuf-scale (a b c d e f g h i j k)
  (#_gdk_pixbuf_scale a b c d e f (coerce g 'double-float)
   (coerce h 'double-float) (coerce i 'double-float) (coerce j 'double-float)
   k))
(defun gdk::pixbuf-composite (a b c d e f g h i j k l)
  (#_gdk_pixbuf_composite a b c d e f (coerce g 'double-float)
   (coerce h 'double-float) (coerce i 'double-float) (coerce j 'double-float) k
   l))
(defun gdk::pixbuf-composite-color (a b c d e f g h i j k l m n o p q)
  (#_gdk_pixbuf_composite_color a b c d e f (coerce g 'double-float)
   (coerce h 'double-float) (coerce i 'double-float) (coerce j 'double-float) k
   l m n o p q))
(defun gdk::pixbuf-scale-simple (a b c d) (#_gdk_pixbuf_scale_simple a b c d))
(defun gdk::pixbuf-composite-color-simple (a b c d e f g h)
  (#_gdk_pixbuf_composite_color_simple a b c d e f g h))
(defun gdk::pixbuf-animation-get-type () (#_gdk_pixbuf_animation_get_type))
(defun gdk::pixbuf-animation-new-from-file (a b)
  (with-cstrs ((s1 a)) (#_gdk_pixbuf_animation_new_from_file s1 b)))
(defun gdk::pixbuf-animation-get-width (a) (#_gdk_pixbuf_animation_get_width a))
(defun gdk::pixbuf-animation-get-height (a)
  (#_gdk_pixbuf_animation_get_height a))
(defun gdk::pixbuf-animation-is-static-image (a)
  (let ((z (#_gdk_pixbuf_animation_is_static_image a)))
    (values (if (= 1 z) t nil))))
(defun gdk::pixbuf-animation-get-static-image (a)
  (#_gdk_pixbuf_animation_get_static_image a))
(defun gdk::pixbuf-animation-get-iter (a b)
  (#_gdk_pixbuf_animation_get_iter a b))
(defun gdk::pixbuf-animation-iter-get-type ()
  (#_gdk_pixbuf_animation_iter_get_type))
(defun gdk::pixbuf-animation-iter-get-delay-time (a)
  (#_gdk_pixbuf_animation_iter_get_delay_time a))
(defun gdk::pixbuf-animation-iter-get-pixbuf (a)
  (#_gdk_pixbuf_animation_iter_get_pixbuf a))
(defun gdk::pixbuf-animation-iter-on-currently-loading-frame (a)
  (let ((z (#_gdk_pixbuf_animation_iter_on_currently_loading_frame a)))
    (values (if (= 1 z) t nil))))
(defun gdk::pixbuf-animation-iter-advance (a b)
  (let ((z (#_gdk_pixbuf_animation_iter_advance a b)))
    (values (if (= 1 z) t nil))))
(defun gdk::pixbuf-get-option (a b)
  (with-cstrs ((s1 b))
              (let ((z (#_gdk_pixbuf_get_option a s1)))
                (values (%get-cstring z)))))
(defun gdk::pixbuf-loader-get-type () (#_gdk_pixbuf_loader_get_type))
(defun gdk::pixbuf-loader-new () (#_gdk_pixbuf_loader_new))
(defun gdk::pixbuf-loader-new-with-type (a b)
  (with-cstrs ((s1 a)) (#_gdk_pixbuf_loader_new_with_type s1 b)))
(defun gdk::pixbuf-loader-write (a b c d)
  (let ((z (#_gdk_pixbuf_loader_write a b c d))) (values (if (= 1 z) t nil))))
(defun gdk::pixbuf-loader-get-pixbuf (a) (#_gdk_pixbuf_loader_get_pixbuf a))
(defun gdk::pixbuf-loader-get-animation (a)
  (#_gdk_pixbuf_loader_get_animation a))
(defun gdk::pixbuf-loader-close (a b)
  (let ((z (#_gdk_pixbuf_loader_close a b))) (values (if (= 1 z) t nil))))
(defun gdk::drawable-get-type () (#_gdk_drawable_get_type))
(defun gdk::drawable-set-data (a b c d)
  (with-cstrs ((s1 b)) (#_gdk_drawable_set_data a s1 c d)))
(defun gdk::drawable-get-data (a b)
  (with-cstrs ((s1 b)) (#_gdk_drawable_get_data a s1)))
(defun gdk::drawable-get-size (a b c)
  (rlet ((r1 :int b) (r2 :int c))
        (let ((z (#_gdk_drawable_get_size a r1 r2)))
          (values z (%get-signed-long r1) (%get-signed-long r2)))))
(defun gdk::drawable-set-colormap (a b) (#_gdk_drawable_set_colormap a b))
(defun gdk::drawable-get-colormap (a) (#_gdk_drawable_get_colormap a))
(defun gdk::drawable-get-visual (a) (#_gdk_drawable_get_visual a))
(defun gdk::drawable-get-depth (a) (#_gdk_drawable_get_depth a))
(defun gdk::drawable-ref (a) (#_gdk_drawable_ref a))
(defun gdk::drawable-unref (a) (#_gdk_drawable_unref a))
(defun gdk::draw-point (a b c d) (#_gdk_draw_point a b c d))
(defun gdk::draw-line (a b c d e f) (#_gdk_draw_line a b c d e f))
(defun gdk::draw-rectangle (a b c d e f g)
  (#_gdk_draw_rectangle a b (if c (if (eq c 0) 0 1) 0) d e f g))
(defun gdk::draw-arc (a b c d e f g h i)
  (#_gdk_draw_arc a b (if c (if (eq c 0) 0 1) 0) d e f g h i))
(defun gdk::draw-polygon (a b c d e)
  (#_gdk_draw_polygon a b (if c (if (eq c 0) 0 1) 0) d e))
(defun gdk::draw-string (a b c d e f)
  (with-cstrs ((s1 f)) (#_gdk_draw_string a b c d e s1)))
(defun gdk::draw-text (a b c d e f g)
  (with-cstrs ((s1 f)) (#_gdk_draw_text a b c d e s1 g)))
(defun gdk::draw-text-wc (a b c d e f g) (#_gdk_draw_text_wc a b c d e f g))
(defun gdk::draw-drawable (a b c d e f g h i)
  (#_gdk_draw_drawable a b c d e f g h i))
(defun gdk::draw-image (a b c d e f g h i) (#_gdk_draw_image a b c d e f g h i))
(defun gdk::draw-points (a b c d) (#_gdk_draw_points a b c d))
(defun gdk::draw-segments (a b c d) (#_gdk_draw_segments a b c d))
(defun gdk::draw-lines (a b c d) (#_gdk_draw_lines a b c d))
(defun gdk::draw-glyphs (a b c d e f) (#_gdk_draw_glyphs a b c d e f))
(defun gdk::draw-layout-line (a b c d e) (#_gdk_draw_layout_line a b c d e))
(defun gdk::draw-layout (a b c d e) (#_gdk_draw_layout a b c d e))
(defun gdk::draw-layout-line-with-colors (a b c d e f g)
  (#_gdk_draw_layout_line_with_colors a b c d e f g))
(defun gdk::drawable-get-image (a b c d e) (#_gdk_drawable_get_image a b c d e))
(defun gdk::drawable-get-clip-region (a) (#_gdk_drawable_get_clip_region a))
(defun gdk::drawable-get-visible-region (a)
  (#_gdk_drawable_get_visible_region a))
(defun gdk::font-ref (a) (#_gdk_font_ref a))
(defun gdk::font-unref (a) (#_gdk_font_unref a))
(defun gdk::font-id (a) (#_gdk_font_id a))
(defun gdk::font-load (a) (with-cstrs ((s1 a)) (#_gdk_font_load s1)))
(defun gdk::fontset-load (a) (with-cstrs ((s1 a)) (#_gdk_fontset_load s1)))
(defun gdk::font-from-description (a) (#_gdk_font_from_description a))
(defun gdk::string-width (a b) (with-cstrs ((s1 b)) (#_gdk_string_width a s1)))
(defun gdk::text-width (a b c) (with-cstrs ((s1 b)) (#_gdk_text_width a s1 c)))
(defun gdk::text-width-wc (a b c) (#_gdk_text_width_wc a b c))
(defun gdk::char-width (a b) (#_gdk_char_width a b))
(defun gdk::char-width-wc (a b) (#_gdk_char_width_wc a b))
(defun gdk::string-measure (a b)
  (with-cstrs ((s1 b)) (#_gdk_string_measure a s1)))
(defun gdk::text-measure (a b c)
  (with-cstrs ((s1 b)) (#_gdk_text_measure a s1 c)))
(defun gdk::char-measure (a b) (#_gdk_char_measure a b))
(defun gdk::string-height (a b)
  (with-cstrs ((s1 b)) (#_gdk_string_height a s1)))
(defun gdk::text-height (a b c)
  (with-cstrs ((s1 b)) (#_gdk_text_height a s1 c)))
(defun gdk::char-height (a b) (#_gdk_char_height a b))
(defun gdk::text-extents (a b c d e f g h)
  (with-cstrs ((s1 b))
              (rlet ((r1 :int d) (r2 :int e) (r3 :int f) (r4 :int g)
                     (r5 :int h))
                    (let ((z (#_gdk_text_extents a s1 c r1 r2 r3 r4 r5)))
                      (values z
                              (%get-signed-long r1)
                              (%get-signed-long r2)
                              (%get-signed-long r3)
                              (%get-signed-long r4)
                              (%get-signed-long r5))))))
(defun gdk::text-extents-wc (a b c d e f g h)
  (rlet ((r1 :int d) (r2 :int e) (r3 :int f) (r4 :int g) (r5 :int h))
        (let ((z (#_gdk_text_extents_wc a b c r1 r2 r3 r4 r5)))
          (values z
                  (%get-signed-long r1)
                  (%get-signed-long r2)
                  (%get-signed-long r3)
                  (%get-signed-long r4)
                  (%get-signed-long r5)))))
(defun gdk::string-extents (a b c d e f g)
  (with-cstrs ((s1 b))
              (rlet ((r1 :int c) (r2 :int d) (r3 :int e) (r4 :int f)
                     (r5 :int g))
                    (let ((z (#_gdk_string_extents a s1 r1 r2 r3 r4 r5)))
                      (values z
                              (%get-signed-long r1)
                              (%get-signed-long r2)
                              (%get-signed-long r3)
                              (%get-signed-long r4)
                              (%get-signed-long r5))))))
(defun gdk::image-get-type () (#_gdk_image_get_type))
(defun gdk::image-new (a b c d) (#_gdk_image_new a b c d))
(defun gdk::image-put-pixel (a b c d) (#_gdk_image_put_pixel a b c d))
(defun gdk::image-get-pixel (a b c) (#_gdk_image_get_pixel a b c))
(defun gdk::image-set-colormap (a b) (#_gdk_image_set_colormap a b))
(defun gdk::image-get-colormap (a) (#_gdk_image_get_colormap a))
(defun gdk::keymap-get-type () (#_gdk_keymap_get_type))
(defun gdk::keymap-get-default () (#_gdk_keymap_get_default))
(defun gdk::keymap-lookup-key (a b) (#_gdk_keymap_lookup_key a b))
(defun gdk::keymap-translate-keyboard-state (a b c d e f g h)
  (rlet ((r1 :int f) (r2 :int g) (r3 :int h))
        (let ((z (#_gdk_keymap_translate_keyboard_state a b c d e r1 r2 r3)))
          (values (if (= 1 z) t nil)
                  (%get-signed-long r1)
                  (%get-signed-long r2)
                  (%get-signed-long r3)))))
(defun gdk::keymap-get-entries-for-keyval (a b c d)
  (rlet ((r1 :int d))
        (let ((z (#_gdk_keymap_get_entries_for_keyval a b c r1)))
          (values (if (= 1 z) t nil) (%get-signed-long r1)))))
(defun gdk::keymap-get-entries-for-keycode (a b c d e)
  (rlet ((r1 :int e))
        (let ((z (#_gdk_keymap_get_entries_for_keycode a b c d r1)))
          (values (if (= 1 z) t nil) (%get-signed-long r1)))))
(defun gdk::keymap-get-direction (a) (#_gdk_keymap_get_direction a))
(defun gdk::keyval-name (a)
  (let ((z (#_gdk_keyval_name a))) (values (%get-cstring z))))
(defun gdk::keyval-from-name (a)
  (with-cstrs ((s1 a)) (#_gdk_keyval_from_name s1)))
(defun gdk::keyval-convert-case (a b c) (#_gdk_keyval_convert_case a b c))
(defun gdk::keyval-to-upper (a) (#_gdk_keyval_to_upper a))
(defun gdk::keyval-to-lower (a) (#_gdk_keyval_to_lower a))
(defun gdk::keyval-is-upper (a)
  (let ((z (#_gdk_keyval_is_upper a))) (values (if (= 1 z) t nil))))
(defun gdk::keyval-is-lower (a)
  (let ((z (#_gdk_keyval_is_lower a))) (values (if (= 1 z) t nil))))
(defun gdk::keyval-to-unicode (a) (#_gdk_keyval_to_unicode a))
(defun gdk::unicode-to-keyval (a) (#_gdk_unicode_to_keyval a))
(defun gdk::pixbuf-render-to-drawable (a b c d e f g h i j k l)
  (#_gdk_pixbuf_render_to_drawable a b c d e f g h i j k l))
(defun gdk::pixbuf-render-to-drawable-alpha (a b c d e f g h i j k l m)
  (#_gdk_pixbuf_render_to_drawable_alpha a b c d e f g h i j k l m))
(defun gdk::pixbuf-render-pixmap-and-mask (a b c d)
  (#_gdk_pixbuf_render_pixmap_and_mask a b c d))
(defun gdk::pixbuf-get-from-drawable (a b c d e f g h i)
  (#_gdk_pixbuf_get_from_drawable a b c d e f g h i))
(defun gdk::pixbuf-get-from-image (a b c d e f g h i)
  (#_gdk_pixbuf_get_from_image a b c d e f g h i))
(defun gdk::pixmap-get-type () (#_gdk_pixmap_get_type))
(defun gdk::pixmap-new (a b c d) (#_gdk_pixmap_new a b c d))
(defun gdk::bitmap-create-from-data (a b c d)
  (with-cstrs ((s1 b)) (#_gdk_bitmap_create_from_data a s1 c d)))
(defun gdk::pixmap-create-from-data (a b c d e f g)
  (with-cstrs ((s1 b)) (#_gdk_pixmap_create_from_data a s1 c d e f g)))
(defun gdk::pixmap-create-from-xpm (a b c d)
  (with-cstrs ((s1 d)) (#_gdk_pixmap_create_from_xpm a b c s1)))
(defun gdk::pixmap-colormap-create-from-xpm (a b c d e)
  (with-cstrs ((s1 e)) (#_gdk_pixmap_colormap_create_from_xpm a b c d s1)))
(defun gdk::pixmap-create-from-xpm-d (a b c d)
  (#_gdk_pixmap_create_from_xpm_d a b c d))
(defun gdk::pixmap-colormap-create-from-xpm-d (a b c d e)
  (#_gdk_pixmap_colormap_create_from_xpm_d a b c d e))
(defun gdk::pixmap-foreign-new (a) (#_gdk_pixmap_foreign_new a))
(defun gdk::pixmap-lookup (a) (#_gdk_pixmap_lookup a))
(defun gdk::atom-intern (a b)
  (with-cstrs ((s1 a)) (#_gdk_atom_intern s1 (if b (if (eq b 0) 0 1) 0))))
(defun gdk::atom-name (a)
  (let ((z (#_gdk_atom_name a))) (values (%get-cstring z))))
(defun gdk::property-get (a b c d e f g h i j)
  (rlet ((r1 :int h) (r2 :int i))
        (let ((z (#_gdk_property_get a b c d e f g r1 r2 j)))
          (values (if (= 1 z) t nil)
                  (%get-signed-long r1)
                  (%get-signed-long r2)))))
(defun gdk::property-change (a b c d e f g)
  (#_gdk_property_change a b c d e f g))
(defun gdk::property-delete (a b) (#_gdk_property_delete a b))
(defun gdk::selection-owner-get (a) (#_gdk_selection_owner_get a))
(defun gdk::selection-convert (a b c d) (#_gdk_selection_convert a b c d))
(defun gdk::selection-property-get (a b c d)
  (rlet ((r1 :int d))
        (let ((z (#_gdk_selection_property_get a b c r1)))
          (values (if (= 1 z) t nil) (%get-signed-long r1)))))
(defun gdk::selection-send-notify (a b c d e)
  (#_gdk_selection_send_notify a b c d e))
(defun gdk::visual-get-best-depth () (#_gdk_visual_get_best_depth))
(defun gdk::visual-get-best-type () (#_gdk_visual_get_best_type))
(defun gdk::visual-get-system () (#_gdk_visual_get_system))
(defun gdk::visual-get-best () (#_gdk_visual_get_best))
(defun gdk::visual-get-best-with-depth (a) (#_gdk_visual_get_best_with_depth a))
(defun gdk::visual-get-best-with-type (a) (#_gdk_visual_get_best_with_type a))
(defun gdk::visual-get-best-with-both (a b)
  (#_gdk_visual_get_best_with_both a b))
(defun gdk::query-depths (a b)
  (rlet ((r1 :int b))
        (let ((z (#_gdk_query_depths a r1))) (values z (%get-signed-long r1)))))
(defun gdk::query-visual-types (a b)
  (rlet ((r1 :int b))
        (let ((z (#_gdk_query_visual_types a r1)))
          (values z (%get-signed-long r1)))))
(defun gdk::list-visuals () (#_gdk_list_visuals))
(defun gdk::window-object-get-type () (#_gdk_window_object_get_type))
(defun gdk::window-new (a b c) (#_gdk_window_new a b c))
(defun gdk::window-destroy (a) (#_gdk_window_destroy a))
(defun gdk::window-get-window-type (a) (#_gdk_window_get_window_type a))
(defun gdk::window-at-pointer (a b)
  (rlet ((r1 :int a) (r2 :int b))
        (let ((z (#_gdk_window_at_pointer r1 r2)))
          (values z (%get-signed-long r1) (%get-signed-long r2)))))
(defun gdk::window-show (a) (#_gdk_window_show a))
(defun gdk::window-hide (a) (#_gdk_window_hide a))
(defun gdk::window-withdraw (a) (#_gdk_window_withdraw a))
(defun gdk::window-move (a b c) (#_gdk_window_move a b c))
(defun gdk::window-resize (a b c) (#_gdk_window_resize a b c))
(defun gdk::window-move-resize (a b c d e) (#_gdk_window_move_resize a b c d e))
(defun gdk::window-reparent (a b c d) (#_gdk_window_reparent a b c d))
(defun gdk::window-clear (a) (#_gdk_window_clear a))
(defun gdk::window-clear-area (a b c d e) (#_gdk_window_clear_area a b c d e))
(defun gdk::window-clear-area-e (a b c d e)
  (#_gdk_window_clear_area_e a b c d e))
(defun gdk::window-raise (a) (#_gdk_window_raise a))
(defun gdk::window-lower (a) (#_gdk_window_lower a))
(defun gdk::window-focus (a b) (#_gdk_window_focus a b))
(defun gdk::window-set-user-data (a b) (#_gdk_window_set_user_data a b))
(defun gdk::window-set-override-redirect (a b)
  (#_gdk_window_set_override_redirect a (if b (if (eq b 0) 0 1) 0)))
(defun gdk::window-add-filter (a b c) (#_gdk_window_add_filter a b c))
(defun gdk::window-remove-filter (a b c) (#_gdk_window_remove_filter a b c))
(defun gdk::window-scroll (a b c) (#_gdk_window_scroll a b c))
(defun gdk::window-shape-combine-mask (a b c d)
  (#_gdk_window_shape_combine_mask a b c d))
(defun gdk::window-set-child-shapes (a) (#_gdk_window_set_child_shapes a))
(defun gdk::window-merge-child-shapes (a) (#_gdk_window_merge_child_shapes a))
(defun gdk::window-is-visible (a)
  (let ((z (#_gdk_window_is_visible a))) (values (if (= 1 z) t nil))))
(defun gdk::window-is-viewable (a)
  (let ((z (#_gdk_window_is_viewable a))) (values (if (= 1 z) t nil))))
(defun gdk::window-get-state (a) (#_gdk_window_get_state a))
(defun gdk::window-set-static-gravities (a b)
  (let ((z (#_gdk_window_set_static_gravities a (if b (if (eq b 0) 0 1) 0))))
    (values (if (= 1 z) t nil))))
(defun gdk::window-foreign-new (a) (#_gdk_window_foreign_new a))
(defun gdk::window-lookup (a) (#_gdk_window_lookup a))
(defun gdk::window-set-hints (a b c d e f g h)
  (#_gdk_window_set_hints a b c d e f g h))
(defun gdk::window-set-type-hint (a b) (#_gdk_window_set_type_hint a b))
(defun gdk::window-set-modal-hint (a b)
  (#_gdk_window_set_modal_hint a (if b (if (eq b 0) 0 1) 0)))
(defun gdk::window-set-geometry-hints (a b c)
  (#_gdk_window_set_geometry_hints a b c))
(defun gdk::set-sm-client-id (a)
  (with-cstrs ((s1 a)) (#_gdk_set_sm_client_id s1)))
(defun gdk::window-begin-paint-rect (a b) (#_gdk_window_begin_paint_rect a b))
(defun gdk::window-begin-paint-region (a b)
  (#_gdk_window_begin_paint_region a b))
(defun gdk::window-end-paint (a) (#_gdk_window_end_paint a))
(defun gdk::window-set-title (a b)
  (with-cstrs ((s1 b)) (#_gdk_window_set_title a s1)))
(defun gdk::window-set-role (a b)
  (with-cstrs ((s1 b)) (#_gdk_window_set_role a s1)))
(defun gdk::window-set-transient-for (a b) (#_gdk_window_set_transient_for a b))
(defun gdk::window-set-background (a b) (#_gdk_window_set_background a b))
(defun gdk::window-set-back-pixmap (a b c)
  (#_gdk_window_set_back_pixmap a b (if c (if (eq c 0) 0 1) 0)))
(defun gdk::window-set-cursor (a b) (#_gdk_window_set_cursor a b))
(defun gdk::window-get-geometry (a b c d e f)
  (rlet ((r1 :int b) (r2 :int c) (r3 :int d) (r4 :int e) (r5 :int f))
        (let ((z (#_gdk_window_get_geometry a r1 r2 r3 r4 r5)))
          (values z
                  (%get-signed-long r1)
                  (%get-signed-long r2)
                  (%get-signed-long r3)
                  (%get-signed-long r4)
                  (%get-signed-long r5)))))
(defun gdk::window-get-position (a b c)
  (rlet ((r1 :int b) (r2 :int c))
        (let ((z (#_gdk_window_get_position a r1 r2)))
          (values z (%get-signed-long r1) (%get-signed-long r2)))))
(defun gdk::window-get-origin (a b c)
  (rlet ((r1 :int b) (r2 :int c))
        (let ((z (#_gdk_window_get_origin a r1 r2)))
          (values z (%get-signed-long r1) (%get-signed-long r2)))))
(defun gdk::window-get-deskrelative-origin (a b c)
  (rlet ((r1 :int b) (r2 :int c))
        (let ((z (#_gdk_window_get_deskrelative_origin a r1 r2)))
          (values (if (= 1 z) t nil)
                  (%get-signed-long r1)
                  (%get-signed-long r2)))))
(defun gdk::window-get-root-origin (a b c)
  (rlet ((r1 :int b) (r2 :int c))
        (let ((z (#_gdk_window_get_root_origin a r1 r2)))
          (values z (%get-signed-long r1) (%get-signed-long r2)))))
(defun gdk::window-get-frame-extents (a b) (#_gdk_window_get_frame_extents a b))
(defun gdk::window-get-pointer (a b c d)
  (rlet ((r1 :int b) (r2 :int c) (r3 :int d))
        (let ((z (#_gdk_window_get_pointer a r1 r2 r3)))
          (values z
                  (%get-signed-long r1)
                  (%get-signed-long r2)
                  (%get-signed-long r3)))))
(defun gdk::window-get-parent (a) (#_gdk_window_get_parent a))
(defun gdk::window-get-toplevel (a) (#_gdk_window_get_toplevel a))
(defun gdk::window-get-children (a) (#_gdk_window_get_children a))
(defun gdk::window-peek-children (a) (#_gdk_window_peek_children a))
(defun gdk::window-get-events (a) (#_gdk_window_get_events a))
(defun gdk::window-set-events (a b) (#_gdk_window_set_events a b))
(defun gdk::window-set-icon-list (a b) (#_gdk_window_set_icon_list a b))
(defun gdk::window-set-icon (a b c d) (#_gdk_window_set_icon a b c d))
(defun gdk::window-set-icon-name (a b)
  (with-cstrs ((s1 b)) (#_gdk_window_set_icon_name a s1)))
(defun gdk::window-set-group (a b) (#_gdk_window_set_group a b))
(defun gdk::window-set-decorations (a b) (#_gdk_window_set_decorations a b))
(defun gdk::window-get-decorations (a b)
  (rlet ((r1 :int b))
        (let ((z (#_gdk_window_get_decorations a r1)))
          (values (if (= 1 z) t nil) (%get-signed-long r1)))))
(defun gdk::window-set-functions (a b) (#_gdk_window_set_functions a b))
(defun gdk::window-get-toplevels () (#_gdk_window_get_toplevels))
(defun gdk::window-iconify (a) (#_gdk_window_iconify a))
(defun gdk::window-deiconify (a) (#_gdk_window_deiconify a))
(defun gdk::window-stick (a) (#_gdk_window_stick a))
(defun gdk::window-unstick (a) (#_gdk_window_unstick a))
(defun gdk::window-maximize (a) (#_gdk_window_maximize a))
(defun gdk::window-unmaximize (a) (#_gdk_window_unmaximize a))
(defun gdk::window-register-dnd (a) (#_gdk_window_register_dnd a))
(defun gdk::window-begin-resize-drag (a b c d e f)
  (#_gdk_window_begin_resize_drag a b c d e f))
(defun gdk::window-begin-move-drag (a b c d e)
  (#_gdk_window_begin_move_drag a b c d e))
(defun gdk::window-invalidate-rect (a b c)
  (#_gdk_window_invalidate_rect a b (if c (if (eq c 0) 0 1) 0)))
(defun gdk::window-invalidate-region (a b c)
  (#_gdk_window_invalidate_region a b (if c (if (eq c 0) 0 1) 0)))
(defun gdk::window-invalidate-maybe-recurse (a b c d)
  (#_gdk_window_invalidate_maybe_recurse a b c d))
(defun gdk::window-get-update-area (a) (#_gdk_window_get_update_area a))
(defun gdk::window-freeze-updates (a) (#_gdk_window_freeze_updates a))
(defun gdk::window-thaw-updates (a) (#_gdk_window_thaw_updates a))
(defun gdk::window-process-all-updates () (#_gdk_window_process_all_updates))
(defun gdk::window-process-updates (a b)
  (#_gdk_window_process_updates a (if b (if (eq b 0) 0 1) 0)))
(defun gdk::window-set-debug-updates (a)
  (#_gdk_window_set_debug_updates (if a (if (eq a 0) 0 1) 0)))
(defun gdk::window-constrain-size (a b c d e f)
  (rlet ((r1 :int e) (r2 :int f))
        (let ((z (#_gdk_window_constrain_size a b c d r1 r2)))
          (values z (%get-signed-long r1) (%get-signed-long r2)))))
(defun gdk::window-get-internal-paint-info (a b c d)
  (rlet ((r1 :int c) (r2 :int d))
        (let ((z (#_gdk_window_get_internal_paint_info a b r1 r2)))
          (values z (%get-signed-long r1) (%get-signed-long r2)))))
(defun gdk::set-pointer-hooks (a) (#_gdk_set_pointer_hooks a))
(defun gdk::get-default-root-window () (#_gdk_get_default_root_window))
(defun gdk::pointer-grab (a b c d e f)
  (#_gdk_pointer_grab a (if b (if (eq b 0) 0 1) 0) c d e f))
(defun gdk::keyboard-grab (a b c)
  (#_gdk_keyboard_grab a (if b (if (eq b 0) 0 1) 0) c))
(defun gdk::pointer-ungrab (a) (#_gdk_pointer_ungrab a))
(defun gdk::keyboard-ungrab (a) (#_gdk_keyboard_ungrab a))
(defun gdk::pointer-is-grabbed ()
  (let ((z (#_gdk_pointer_is_grabbed))) (values (if (= 1 z) t nil))))
(defun gdk::screen-width () (#_gdk_screen_width))
(defun gdk::screen-height () (#_gdk_screen_height))
(defun gdk::screen-width-mm () (#_gdk_screen_width_mm))
(defun gdk::screen-height-mm () (#_gdk_screen_height_mm))
(defun gdk::beep () (#_gdk_beep))
(defun gdk::flush () (#_gdk_flush))
(defun gdk::set-double-click-time (a) (#_gdk_set_double_click_time a))
(defun gdk::rectangle-intersect (a b c)
  (let ((z (#_gdk_rectangle_intersect a b c))) (values (if (= 1 z) t nil))))
(defun gdk::rectangle-union (a b c) (#_gdk_rectangle_union a b c))
(defun gdk::threads-enter () (#_gdk_threads_enter))
(defun gdk::threads-leave () (#_gdk_threads_leave))
(defun gdk::threads-init () (#_gdk_threads_init))
(defun gtk::accel-group-get-type () (#_gtk_accel_group_get_type))
(defun gtk::accel-group-new () (#_gtk_accel_group_new))
(defun gtk::accel-group-lock (a) (#_gtk_accel_group_lock a))
(defun gtk::accel-group-unlock (a) (#_gtk_accel_group_unlock a))
(defun gtk::accel-group-connect (a b c d e)
  (#_gtk_accel_group_connect a b c d e))
(defun gtk::accel-group-connect-by-path (a b c)
  (with-cstrs ((s1 b)) (#_gtk_accel_group_connect_by_path a s1 c)))
(defun gtk::accel-group-disconnect (a b)
  (let ((z (#_gtk_accel_group_disconnect a b))) (values (if (= 1 z) t nil))))
(defun gtk::accel-group-disconnect-key (a b c)
  (let ((z (#_gtk_accel_group_disconnect_key a b c)))
    (values (if (= 1 z) t nil))))
(defun gtk::accel-groups-activate (a b c)
  (let ((z (#_gtk_accel_groups_activate a b c))) (values (if (= 1 z) t nil))))
(defun gtk::accel-groups-from-object (a) (#_gtk_accel_groups_from_object a))
(defun gtk::accel-group-find (a b c) (#_gtk_accel_group_find a b c))
(defun gtk::accel-group-from-accel-closure (a)
  (#_gtk_accel_group_from_accel_closure a))
(defun gtk::accelerator-valid (a b)
  (let ((z (#_gtk_accelerator_valid a b))) (values (if (= 1 z) t nil))))
(defun gtk::accelerator-parse (a b c)
  (with-cstrs ((s1 a))
              (rlet ((r1 :int c))
                    (let ((z (#_gtk_accelerator_parse s1 b r1)))
                      (values z (%get-signed-long r1))))))
(defun gtk::accelerator-name (a b)
  (let ((z (#_gtk_accelerator_name a b))) (values (%get-cstring z))))
(defun gtk::accelerator-set-default-mod-mask (a)
  (#_gtk_accelerator_set_default_mod_mask a))
(defun gtk::accelerator-get-default-mod-mask ()
  (#_gtk_accelerator_get_default_mod_mask))
(defun gtk::object-get-type () (#_gtk_object_get_type))
(defun gtk::object-new (a b) (with-cstrs ((s1 b)) (#_gtk_object_new a s1)))
(defun gtk::object-sink (a) (#_gtk_object_sink a))
(defun gtk::object-destroy (a) (#_gtk_object_destroy a))
(defun gtk::adjustment-get-type () (#_gtk_adjustment_get_type))
(defun gtk::adjustment-new (a b c d e f)
  (#_gtk_adjustment_new (coerce a 'double-float) (coerce b 'double-float)
   (coerce c 'double-float) (coerce d 'double-float) (coerce e 'double-float)
   (coerce f 'double-float)))
(defun gtk::adjustment-changed (a) (#_gtk_adjustment_changed a))
(defun gtk::adjustment-value-changed (a) (#_gtk_adjustment_value_changed a))
(defun gtk::adjustment-clamp-page (a b c)
  (#_gtk_adjustment_clamp_page a (coerce b 'double-float)
   (coerce c 'double-float)))
(defun gtk::adjustment-get-value (a) (#_gtk_adjustment_get_value a))
(defun gtk::adjustment-set-value (a b)
  (#_gtk_adjustment_set_value a (coerce b 'double-float)))
(defun gtk::style-get-type () (#_gtk_style_get_type))
(defun gtk::style-new () (#_gtk_style_new))
(defun gtk::style-copy (a) (#_gtk_style_copy a))
(defun gtk::style-attach (a b) (#_gtk_style_attach a b))
(defun gtk::style-detach (a) (#_gtk_style_detach a))
(defun gtk::style-set-background (a b c) (#_gtk_style_set_background a b c))
(defun gtk::style-apply-default-background (a b c d e f g h i)
  (#_gtk_style_apply_default_background a b (if c (if (eq c 0) 0 1) 0) d e f g
   h i))
(defun gtk::style-lookup-icon-set (a b)
  (with-cstrs ((s1 b)) (#_gtk_style_lookup_icon_set a s1)))
(defun gtk::style-render-icon (a b c d e f g)
  (with-cstrs ((s1 g)) (#_gtk_style_render_icon a b c d e f s1)))
(defun gtk::draw-check (a b c d e f g h) (#_gtk_draw_check a b c d e f g h))
(defun gtk::paint-hline (a b c d e f g h i)
  (with-cstrs ((s1 f)) (#_gtk_paint_hline a b c d e s1 g h i)))
(defun gtk::paint-vline (a b c d e f g h i)
  (with-cstrs ((s1 f)) (#_gtk_paint_vline a b c d e s1 g h i)))
(defun gtk::paint-shadow (a b c d e f g h i j k)
  (with-cstrs ((s1 g)) (#_gtk_paint_shadow a b c d e f s1 h i j k)))
(defun gtk::paint-polygon (a b c d e f g h i j)
  (with-cstrs ((s1 g))
              (#_gtk_paint_polygon a b c d e f s1 h i
               (if j (if (eq j 0) 0 1) 0))))
(defun gtk::paint-arrow (a b c d e f g h i j k l m)
  (with-cstrs ((s1 g))
              (#_gtk_paint_arrow a b c d e f s1 h (if i (if (eq i 0) 0 1) 0) j
               k l m)))
(defun gtk::paint-diamond (a b c d e f g h i j k)
  (with-cstrs ((s1 g)) (#_gtk_paint_diamond a b c d e f s1 h i j k)))
(defun gtk::paint-box (a b c d e f g h i j k)
  (with-cstrs ((s1 g)) (#_gtk_paint_box a b c d e f s1 h i j k)))
(defun gtk::paint-flat-box (a b c d e f g h i j k)
  (with-cstrs ((s1 g)) (#_gtk_paint_flat_box a b c d e f s1 h i j k)))
(defun gtk::paint-check (a b c d e f g h i j k)
  (with-cstrs ((s1 g)) (#_gtk_paint_check a b c d e f s1 h i j k)))
(defun gtk::paint-option (a b c d e f g h i j k)
  (with-cstrs ((s1 g)) (#_gtk_paint_option a b c d e f s1 h i j k)))
(defun gtk::paint-tab (a b c d e f g h i j k)
  (with-cstrs ((s1 g)) (#_gtk_paint_tab a b c d e f s1 h i j k)))
(defun gtk::paint-shadow-gap (a b c d e f g h i j k l m n)
  (with-cstrs ((s1 g)) (#_gtk_paint_shadow_gap a b c d e f s1 h i j k l m n)))
(defun gtk::paint-box-gap (a b c d e f g h i j k l m n)
  (with-cstrs ((s1 g)) (#_gtk_paint_box_gap a b c d e f s1 h i j k l m n)))
(defun gtk::paint-extension (a b c d e f g h i j k l)
  (with-cstrs ((s1 g)) (#_gtk_paint_extension a b c d e f s1 h i j k l)))
(defun gtk::paint-focus (a b c d e f g h i j)
  (with-cstrs ((s1 f)) (#_gtk_paint_focus a b c d e s1 g h i j)))
(defun gtk::paint-slider (a b c d e f g h i j k l)
  (with-cstrs ((s1 g)) (#_gtk_paint_slider a b c d e f s1 h i j k l)))
(defun gtk::paint-handle (a b c d e f g h i j k l)
  (with-cstrs ((s1 g)) (#_gtk_paint_handle a b c d e f s1 h i j k l)))
(defun gtk::paint-expander (a b c d e f g h i)
  (with-cstrs ((s1 f)) (#_gtk_paint_expander a b c d e s1 g h i)))
(defun gtk::paint-layout (a b c d e f g h i j)
  (with-cstrs ((s1 g))
              (#_gtk_paint_layout a b c (if d (if (eq d 0) 0 1) 0) e f s1 h i
               j)))
(defun gtk::paint-resize-grip (a b c d e f g h i j k)
  (with-cstrs ((s1 f)) (#_gtk_paint_resize_grip a b c d e s1 g h i j k)))
(defun gtk::border-get-type () (#_gtk_border_get_type))
(defun gtk::border-copy (a) (#_gtk_border_copy a))
(defun gtk::border-free (a) (#_gtk_border_free a))
(defun gtk::rc-add-default-file (a)
  (with-cstrs ((s1 a)) (#_gtk_rc_add_default_file s1)))
(defun gtk::rc-set-default-files (a) (#_gtk_rc_set_default_files a))
(defun gtk::rc-get-default-files () (#_gtk_rc_get_default_files))
(defun gtk::rc-get-style (a) (#_gtk_rc_get_style a))
(defun gtk::rc-get-style-by-paths (a b c d)
  (with-cstrs ((s1 b) (s2 c)) (#_gtk_rc_get_style_by_paths a s1 s2 d)))
(defun gtk::rc-reparse-all-for-settings (a b)
  (let ((z (#_gtk_rc_reparse_all_for_settings a (if b (if (eq b 0) 0 1) 0))))
    (values (if (= 1 z) t nil))))
(defun gtk::rc-find-pixmap-in-path (a b c)
  (with-cstrs ((s1 c))
              (let ((z (#_gtk_rc_find_pixmap_in_path a b s1)))
                (values (%get-cstring z)))))
(defun gtk::rc-parse (a) (with-cstrs ((s1 a)) (#_gtk_rc_parse s1)))
(defun gtk::rc-parse-string (a)
  (with-cstrs ((s1 a)) (#_gtk_rc_parse_string s1)))
(defun gtk::rc-reparse-all ()
  (let ((z (#_gtk_rc_reparse_all))) (values (if (= 1 z) t nil))))
(defun gtk::rc-style-get-type () (#_gtk_rc_style_get_type))
(defun gtk::rc-style-new () (#_gtk_rc_style_new))
(defun gtk::rc-style-copy (a) (#_gtk_rc_style_copy a))
(defun gtk::rc-style-ref (a) (#_gtk_rc_style_ref a))
(defun gtk::rc-style-unref (a) (#_gtk_rc_style_unref a))
(defun gtk::rc-find-module-in-path (a)
  (with-cstrs ((s1 a))
              (let ((z (#_gtk_rc_find_module_in_path s1)))
                (values (%get-cstring z)))))
(defun gtk::rc-get-theme-dir ()
  (let ((z (#_gtk_rc_get_theme_dir))) (values (%get-cstring z))))
(defun gtk::rc-get-module-dir ()
  (let ((z (#_gtk_rc_get_module_dir))) (values (%get-cstring z))))
(defun gtk::rc-get-im-module-path ()
  (let ((z (#_gtk_rc_get_im_module_path))) (values (%get-cstring z))))
(defun gtk::rc-get-im-module-file ()
  (let ((z (#_gtk_rc_get_im_module_file))) (values (%get-cstring z))))
(defun gtk::rc-scanner-new () (#_gtk_rc_scanner_new))
(defun gtk::rc-parse-color (a b) (#_gtk_rc_parse_color a b))
(defun gtk::rc-parse-state (a b)
  (rlet ((r1 :int b))
        (let ((z (#_gtk_rc_parse_state a r1)))
          (values z (%get-signed-long r1)))))
(defun gtk::rc-parse-priority (a b)
  (rlet ((r1 :int b))
        (let ((z (#_gtk_rc_parse_priority a r1)))
          (values z (%get-signed-long r1)))))
(defun gtk::settings-get-type () (#_gtk_settings_get_type))
(defun gtk::settings-get-default () (#_gtk_settings_get_default))
(defun gtk::settings-install-property (a) (#_gtk_settings_install_property a))
(defun gtk::settings-install-property-parser (a b)
  (#_gtk_settings_install_property_parser a b))
(defun gtk::rc-property-parse-color (a b c)
  (let ((z (#_gtk_rc_property_parse_color a b c))) (values (if (= 1 z) t nil))))
(defun gtk::rc-property-parse-enum (a b c)
  (let ((z (#_gtk_rc_property_parse_enum a b c))) (values (if (= 1 z) t nil))))
(defun gtk::rc-property-parse-flags (a b c)
  (let ((z (#_gtk_rc_property_parse_flags a b c))) (values (if (= 1 z) t nil))))
(defun gtk::rc-property-parse-requisition (a b c)
  (let ((z (#_gtk_rc_property_parse_requisition a b c)))
    (values (if (= 1 z) t nil))))
(defun gtk::rc-property-parse-border (a b c)
  (let ((z (#_gtk_rc_property_parse_border a b c)))
    (values (if (= 1 z) t nil))))
(defun gtk::settings-set-property-value (a b c)
  (with-cstrs ((s1 b)) (#_gtk_settings_set_property_value a s1 c)))
(defun gtk::settings-set-string-property (a b c d)
  (with-cstrs ((s1 b) (s2 c) (s3 d))
              (#_gtk_settings_set_string_property a s1 s2 s3)))
(defun gtk::settings-set-long-property (a b c d)
  (with-cstrs ((s1 b) (s2 d)) (#_gtk_settings_set_long_property a s1 c s2)))
(defun gtk::settings-set-double-property (a b c d)
  (with-cstrs ((s1 b) (s2 d))
              (#_gtk_settings_set_double_property a s1 (coerce c 'double-float)
               s2)))
(defun atk::state-type-get-name (a)
  (let ((z (#_atk_state_type_get_name a))) (values (%get-cstring z))))
(defun atk::state-type-for-name (a)
  (with-cstrs ((s1 a)) (#_atk_state_type_for_name s1)))
(defun atk::object-get-type () (#_atk_object_get_type))
(defun atk::implementor-get-type () (#_atk_implementor_get_type))
(defun atk::implementor-ref-accessible (a) (#_atk_implementor_ref_accessible a))
(defun atk::object-get-name (a)
  (let ((z (#_atk_object_get_name a))) (values (%get-cstring z))))
(defun atk::object-get-description (a)
  (let ((z (#_atk_object_get_description a))) (values (%get-cstring z))))
(defun atk::object-get-parent (a) (#_atk_object_get_parent a))
(defun atk::object-get-n-accessible-children (a)
  (#_atk_object_get_n_accessible_children a))
(defun atk::object-ref-accessible-child (a b)
  (#_atk_object_ref_accessible_child a b))
(defun atk::object-ref-relation-set (a) (#_atk_object_ref_relation_set a))
(defun atk::object-get-role (a) (#_atk_object_get_role a))
(defun atk::object-get-layer (a) (#_atk_object_get_layer a))
(defun atk::object-get-mdi-zorder (a) (#_atk_object_get_mdi_zorder a))
(defun atk::object-ref-state-set (a) (#_atk_object_ref_state_set a))
(defun atk::object-get-index-in-parent (a) (#_atk_object_get_index_in_parent a))
(defun atk::object-set-name (a b)
  (with-cstrs ((s1 b)) (#_atk_object_set_name a s1)))
(defun atk::object-set-description (a b)
  (with-cstrs ((s1 b)) (#_atk_object_set_description a s1)))
(defun atk::object-set-parent (a b) (#_atk_object_set_parent a b))
(defun atk::object-set-role (a b) (#_atk_object_set_role a b))
(defun atk::object-connect-property-change-handler (a b)
  (#_atk_object_connect_property_change_handler a b))
(defun atk::object-remove-property-change-handler (a b)
  (#_atk_object_remove_property_change_handler a b))
(defun atk::object-notify-state-change (a b c)
  (#_atk_object_notify_state_change a b (if c (if (eq c 0) 0 1) 0)))
(defun atk::role-get-name (a)
  (let ((z (#_atk_role_get_name a))) (values (%get-cstring z))))
(defun atk::role-for-name (a) (with-cstrs ((s1 a)) (#_atk_role_for_name s1)))
(defun gtk::widget-get-type () (#_gtk_widget_get_type))
(defun gtk::widget-new (a b) (with-cstrs ((s1 b)) (#_gtk_widget_new a s1)))
(defun gtk::widget-ref (a) (#_gtk_widget_ref a))
(defun gtk::widget-unref (a) (#_gtk_widget_unref a))
(defun gtk::widget-destroy (a) (#_gtk_widget_destroy a))
(defun gtk::widget-destroyed (a b) (#_gtk_widget_destroyed a b))
(defun gtk::widget-set (a b) (with-cstrs ((s1 b)) (#_gtk_widget_set a s1)))
(defun gtk::widget-unparent (a) (#_gtk_widget_unparent a))
(defun gtk::widget-show (a) (#_gtk_widget_show a))
(defun gtk::widget-show-now (a) (#_gtk_widget_show_now a))
(defun gtk::widget-hide (a) (#_gtk_widget_hide a))
(defun gtk::widget-show-all (a) (#_gtk_widget_show_all a))
(defun gtk::widget-hide-all (a) (#_gtk_widget_hide_all a))
(defun gtk::widget-map (a) (#_gtk_widget_map a))
(defun gtk::widget-unmap (a) (#_gtk_widget_unmap a))
(defun gtk::widget-realize (a) (#_gtk_widget_realize a))
(defun gtk::widget-unrealize (a) (#_gtk_widget_unrealize a))
(defun gtk::widget-queue-draw (a) (#_gtk_widget_queue_draw a))
(defun gtk::widget-queue-draw-area (a b c d e)
  (#_gtk_widget_queue_draw_area a b c d e))
(defun gtk::widget-queue-resize (a) (#_gtk_widget_queue_resize a))
(defun gtk::widget-size-request (a b) (#_gtk_widget_size_request a b))
(defun gtk::widget-size-allocate (a b) (#_gtk_widget_size_allocate a b))
(defun gtk::widget-get-child-requisition (a b)
  (#_gtk_widget_get_child_requisition a b))
(defun gtk::widget-add-accelerator (a b c d e f)
  (with-cstrs ((s1 b)) (#_gtk_widget_add_accelerator a s1 c d e f)))
(defun gtk::widget-remove-accelerator (a b c d)
  (let ((z (#_gtk_widget_remove_accelerator a b c d)))
    (values (if (= 1 z) t nil))))
(defun gtk::widget-set-accel-path (a b c)
  (with-cstrs ((s1 b)) (#_gtk_widget_set_accel_path a s1 c)))
(defun gtk::widget-list-accel-closures (a) (#_gtk_widget_list_accel_closures a))
(defun gtk::widget-mnemonic-activate (a b)
  (let ((z (#_gtk_widget_mnemonic_activate a (if b (if (eq b 0) 0 1) 0))))
    (values (if (= 1 z) t nil))))
(defun gtk::widget-event (a b)
  (let ((z (#_gtk_widget_event a b))) (values (if (= 1 z) t nil))))
(defun gtk::widget-send-expose (a b) (#_gtk_widget_send_expose a b))
(defun gtk::widget-activate (a)
  (let ((z (#_gtk_widget_activate a))) (values (if (= 1 z) t nil))))
(defun gtk::widget-set-scroll-adjustments (a b c)
  (let ((z (#_gtk_widget_set_scroll_adjustments a b c)))
    (values (if (= 1 z) t nil))))
(defun gtk::widget-reparent (a b) (#_gtk_widget_reparent a b))
(defun gtk::widget-intersect (a b c)
  (let ((z (#_gtk_widget_intersect a b c))) (values (if (= 1 z) t nil))))
(defun gtk::widget-region-intersect (a b) (#_gtk_widget_region_intersect a b))
(defun gtk::widget-freeze-child-notify (a) (#_gtk_widget_freeze_child_notify a))
(defun gtk::widget-child-notify (a b)
  (with-cstrs ((s1 b)) (#_gtk_widget_child_notify a s1)))
(defun gtk::widget-thaw-child-notify (a) (#_gtk_widget_thaw_child_notify a))
(defun gtk::widget-is-focus (a)
  (let ((z (#_gtk_widget_is_focus a))) (values (if (= 1 z) t nil))))
(defun gtk::widget-grab-focus (a) (#_gtk_widget_grab_focus a))
(defun gtk::widget-grab-default (a) (#_gtk_widget_grab_default a))
(defun gtk::widget-set-name (a b)
  (with-cstrs ((s1 b)) (#_gtk_widget_set_name a s1)))
(defun gtk::widget-get-name (a)
  (let ((z (#_gtk_widget_get_name a))) (values (%get-cstring z))))
(defun gtk::widget-set-state (a b) (#_gtk_widget_set_state a b))
(defun gtk::widget-set-sensitive (a b)
  (#_gtk_widget_set_sensitive a (if b (if (eq b 0) 0 1) 0)))
(defun gtk::widget-set-app-paintable (a b)
  (#_gtk_widget_set_app_paintable a (if b (if (eq b 0) 0 1) 0)))
(defun gtk::widget-set-double-buffered (a b)
  (#_gtk_widget_set_double_buffered a (if b (if (eq b 0) 0 1) 0)))
(defun gtk::widget-set-redraw-on-allocate (a b)
  (#_gtk_widget_set_redraw_on_allocate a (if b (if (eq b 0) 0 1) 0)))
(defun gtk::widget-set-parent (a b) (#_gtk_widget_set_parent a b))
(defun gtk::widget-set-parent-window (a b) (#_gtk_widget_set_parent_window a b))
(defun gtk::widget-set-child-visible (a b)
  (#_gtk_widget_set_child_visible a (if b (if (eq b 0) 0 1) 0)))
(defun gtk::widget-get-child-visible (a)
  (let ((z (#_gtk_widget_get_child_visible a))) (values (if (= 1 z) t nil))))
(defun gtk::widget-get-parent (a) (#_gtk_widget_get_parent a))
(defun gtk::widget-get-parent-window (a) (#_gtk_widget_get_parent_window a))
(defun gtk::widget-child-focus (a b)
  (let ((z (#_gtk_widget_child_focus a b))) (values (if (= 1 z) t nil))))
(defun gtk::widget-set-size-request (a b c)
  (#_gtk_widget_set_size_request a b c))
(defun gtk::widget-get-size-request (a b c)
  (rlet ((r1 :int b) (r2 :int c))
        (let ((z (#_gtk_widget_get_size_request a r1 r2)))
          (values z (%get-signed-long r1) (%get-signed-long r2)))))
(defun gtk::widget-set-events (a b) (#_gtk_widget_set_events a b))
(defun gtk::widget-add-events (a b) (#_gtk_widget_add_events a b))
(defun gtk::widget-set-extension-events (a b)
  (#_gtk_widget_set_extension_events a b))
(defun gtk::widget-get-extension-events (a)
  (#_gtk_widget_get_extension_events a))
(defun gtk::widget-get-toplevel (a) (#_gtk_widget_get_toplevel a))
(defun gtk::widget-get-ancestor (a b) (#_gtk_widget_get_ancestor a b))
(defun gtk::widget-get-colormap (a) (#_gtk_widget_get_colormap a))
(defun gtk::widget-get-visual (a) (#_gtk_widget_get_visual a))
(defun gtk::widget-get-settings (a) (#_gtk_widget_get_settings a))
(defun gtk::widget-get-accessible (a) (#_gtk_widget_get_accessible a))
(defun gtk::widget-set-colormap (a b) (#_gtk_widget_set_colormap a b))
(defun gtk::widget-get-events (a) (#_gtk_widget_get_events a))
(defun gtk::widget-get-pointer (a b c)
  (rlet ((r1 :int b) (r2 :int c))
        (let ((z (#_gtk_widget_get_pointer a r1 r2)))
          (values z (%get-signed-long r1) (%get-signed-long r2)))))
(defun gtk::widget-is-ancestor (a b)
  (let ((z (#_gtk_widget_is_ancestor a b))) (values (if (= 1 z) t nil))))
(defun gtk::widget-translate-coordinates (a b c d e f)
  (rlet ((r1 :int e) (r2 :int f))
        (let ((z (#_gtk_widget_translate_coordinates a b c d r1 r2)))
          (values (if (= 1 z) t nil)
                  (%get-signed-long r1)
                  (%get-signed-long r2)))))
(defun gtk::widget-hide-on-delete (a)
  (let ((z (#_gtk_widget_hide_on_delete a))) (values (if (= 1 z) t nil))))
(defun gtk::widget-set-style (a b) (#_gtk_widget_set_style a b))
(defun gtk::widget-ensure-style (a) (#_gtk_widget_ensure_style a))
(defun gtk::widget-get-style (a) (#_gtk_widget_get_style a))
(defun gtk::widget-modify-style (a b) (#_gtk_widget_modify_style a b))
(defun gtk::widget-get-modifier-style (a) (#_gtk_widget_get_modifier_style a))
(defun gtk::widget-modify-fg (a b c) (#_gtk_widget_modify_fg a b c))
(defun gtk::widget-modify-bg (a b c) (#_gtk_widget_modify_bg a b c))
(defun gtk::widget-modify-text (a b c) (#_gtk_widget_modify_text a b c))
(defun gtk::widget-modify-base (a b c) (#_gtk_widget_modify_base a b c))
(defun gtk::widget-modify-font (a b) (#_gtk_widget_modify_font a b))
(defun gtk::widget-create-pango-context (a)
  (#_gtk_widget_create_pango_context a))
(defun gtk::widget-get-pango-context (a) (#_gtk_widget_get_pango_context a))
(defun gtk::widget-create-pango-layout (a b)
  (with-cstrs ((s1 b)) (#_gtk_widget_create_pango_layout a s1)))
(defun gtk::widget-render-icon (a b c d)
  (with-cstrs ((s1 b) (s2 d)) (#_gtk_widget_render_icon a s1 c s2)))
(defun gtk::widget-set-composite-name (a b)
  (with-cstrs ((s1 b)) (#_gtk_widget_set_composite_name a s1)))
(defun gtk::widget-get-composite-name (a)
  (let ((z (#_gtk_widget_get_composite_name a))) (values (%get-cstring z))))
(defun gtk::widget-reset-rc-styles (a) (#_gtk_widget_reset_rc_styles a))
(defun gtk::widget-push-colormap (a) (#_gtk_widget_push_colormap a))
(defun gtk::widget-push-composite-child () (#_gtk_widget_push_composite_child))
(defun gtk::widget-pop-composite-child () (#_gtk_widget_pop_composite_child))
(defun gtk::widget-pop-colormap () (#_gtk_widget_pop_colormap))
(defun gtk::widget-class-install-style-property (a b)
  (#_gtk_widget_class_install_style_property a b))
(defun gtk::widget-class-install-style-property-parser (a b c)
  (#_gtk_widget_class_install_style_property_parser a b c))
(defun gtk::widget-style-get-property (a b c)
  (with-cstrs ((s1 b)) (#_gtk_widget_style_get_property a s1 c)))
(defun gtk::widget-style-get-valist (a b c)
  (with-cstrs ((s1 b) (s2 c)) (#_gtk_widget_style_get_valist a s1 s2)))
(defun gtk::widget-style-get (a b)
  (with-cstrs ((s1 b)) (#_gtk_widget_style_get a s1)))
(defun gtk::widget-set-default-colormap (a)
  (#_gtk_widget_set_default_colormap a))
(defun gtk::widget-get-default-style () (#_gtk_widget_get_default_style))
(defun gtk::widget-get-default-colormap () (#_gtk_widget_get_default_colormap))
(defun gtk::widget-get-default-visual () (#_gtk_widget_get_default_visual))
(defun gtk::widget-set-direction (a b) (#_gtk_widget_set_direction a b))
(defun gtk::widget-get-direction (a) (#_gtk_widget_get_direction a))
(defun gtk::widget-set-default-direction (a)
  (#_gtk_widget_set_default_direction a))
(defun gtk::widget-get-default-direction ()
  (#_gtk_widget_get_default_direction))
(defun gtk::widget-shape-combine-mask (a b c d)
  (#_gtk_widget_shape_combine_mask a b c d))
(defun gtk::widget-reset-shapes (a) (#_gtk_widget_reset_shapes a))
(defun gtk::widget-path (a b c d) (#_gtk_widget_path a b c d))
(defun gtk::widget-class-path (a b c d) (#_gtk_widget_class_path a b c d))
(defun gtk::requisition-get-type () (#_gtk_requisition_get_type))
(defun gtk::requisition-copy (a) (#_gtk_requisition_copy a))
(defun gtk::requisition-free (a) (#_gtk_requisition_free a))
(defun gtk::misc-get-type () (#_gtk_misc_get_type))
(defun gtk::misc-set-alignment (a b c)
  (#_gtk_misc_set_alignment a (coerce b 'single-float)
   (coerce c 'single-float)))
(defun gtk::misc-get-alignment (a b c)
  (rlet ((r1 :float (coerce b 'single-float))
         (r2 :float (coerce c 'single-float)))
        (let ((z (#_gtk_misc_get_alignment a r1 r2)))
          (values z (%get-single-float r1) (%get-single-float r2)))))
(defun gtk::misc-set-padding (a b c) (#_gtk_misc_set_padding a b c))
(defun gtk::misc-get-padding (a b c)
  (rlet ((r1 :int b) (r2 :int c))
        (let ((z (#_gtk_misc_get_padding a r1 r2)))
          (values z (%get-signed-long r1) (%get-signed-long r2)))))
(defun gtk::container-get-type () (#_gtk_container_get_type))
(defun gtk::container-set-border-width (a b)
  (#_gtk_container_set_border_width a b))
(defun gtk::container-get-border-width (a) (#_gtk_container_get_border_width a))
(defun gtk::container-add (a b) (#_gtk_container_add a b))
(defun gtk::container-remove (a b) (#_gtk_container_remove a b))
(defun gtk::container-set-resize-mode (a b)
  (#_gtk_container_set_resize_mode a b))
(defun gtk::container-get-resize-mode (a) (#_gtk_container_get_resize_mode a))
(defun gtk::container-check-resize (a) (#_gtk_container_check_resize a))
(defun gtk::container-foreach (a b c) (#_gtk_container_foreach a b c))
(defun gtk::container-get-children (a) (#_gtk_container_get_children a))
(defun gtk::container-propagate-expose (a b c)
  (#_gtk_container_propagate_expose a b c))
(defun gtk::container-set-focus-chain (a b)
  (#_gtk_container_set_focus_chain a b))
(defun gtk::container-get-focus-chain (a b)
  (let ((z (#_gtk_container_get_focus_chain a b))) (values (if (= 1 z) t nil))))
(defun gtk::container-unset-focus-chain (a)
  (#_gtk_container_unset_focus_chain a))
(defun gtk::container-set-reallocate-redraws (a b)
  (#_gtk_container_set_reallocate_redraws a (if b (if (eq b 0) 0 1) 0)))
(defun gtk::container-set-focus-child (a b)
  (#_gtk_container_set_focus_child a b))
(defun gtk::container-set-focus-vadjustment (a b)
  (#_gtk_container_set_focus_vadjustment a b))
(defun gtk::container-get-focus-vadjustment (a)
  (#_gtk_container_get_focus_vadjustment a))
(defun gtk::container-set-focus-hadjustment (a b)
  (#_gtk_container_set_focus_hadjustment a b))
(defun gtk::container-get-focus-hadjustment (a)
  (#_gtk_container_get_focus_hadjustment a))
(defun gtk::container-resize-children (a) (#_gtk_container_resize_children a))
(defun gtk::container-child-type (a) (#_gtk_container_child_type a))
(defun gtk::container-class-install-child-property (a b c)
  (#_gtk_container_class_install_child_property a b c))
(defun gtk::container-class-find-child-property (a b)
  (with-cstrs ((s1 b)) (#_gtk_container_class_find_child_property a s1)))
(defun gtk::container-class-list-child-properties (a b)
  (#_gtk_container_class_list_child_properties a b))
(defun gtk::container-add-with-properties (a b c)
  (with-cstrs ((s1 c)) (#_gtk_container_add_with_properties a b s1)))
(defun gtk::container-child-set (a b c)
  (with-cstrs ((s1 c)) (#_gtk_container_child_set a b s1)))
(defun gtk::container-child-get (a b c)
  (with-cstrs ((s1 c)) (#_gtk_container_child_get a b s1)))
(defun gtk::container-child-set-valist (a b c d)
  (with-cstrs ((s1 c) (s2 d)) (#_gtk_container_child_set_valist a b s1 s2)))
(defun gtk::container-child-get-valist (a b c d)
  (with-cstrs ((s1 c) (s2 d)) (#_gtk_container_child_get_valist a b s1 s2)))
(defun gtk::container-child-set-property (a b c d)
  (with-cstrs ((s1 c)) (#_gtk_container_child_set_property a b s1 d)))
(defun gtk::container-child-get-property (a b c d)
  (with-cstrs ((s1 c)) (#_gtk_container_child_get_property a b s1 d)))
(defun gtk::container-forall (a b c) (#_gtk_container_forall a b c))
(defun gtk::bin-get-type () (#_gtk_bin_get_type))
(defun gtk::bin-get-child (a) (#_gtk_bin_get_child a))
(defun gtk::window-get-type () (#_gtk_window_get_type))
(defun gtk::window-new (a) (#_gtk_window_new a))
(defun gtk::window-set-title (a b)
  (with-cstrs ((s1 b)) (#_gtk_window_set_title a s1)))
(defun gtk::window-get-title (a)
  (let ((z (#_gtk_window_get_title a))) (values (%get-cstring z))))
(defun gtk::window-set-wmclass (a b c)
  (with-cstrs ((s1 b) (s2 c)) (#_gtk_window_set_wmclass a s1 s2)))
(defun gtk::window-set-role (a b)
  (with-cstrs ((s1 b)) (#_gtk_window_set_role a s1)))
(defun gtk::window-get-role (a)
  (let ((z (#_gtk_window_get_role a))) (values (%get-cstring z))))
(defun gtk::window-add-accel-group (a b) (#_gtk_window_add_accel_group a b))
(defun gtk::window-remove-accel-group (a b)
  (#_gtk_window_remove_accel_group a b))
(defun gtk::window-set-position (a b) (#_gtk_window_set_position a b))
(defun gtk::window-activate-focus (a)
  (let ((z (#_gtk_window_activate_focus a))) (values (if (= 1 z) t nil))))
(defun gtk::window-set-focus (a b) (#_gtk_window_set_focus a b))
(defun gtk::window-get-focus (a) (#_gtk_window_get_focus a))
(defun gtk::window-set-default (a b) (#_gtk_window_set_default a b))
(defun gtk::window-activate-default (a)
  (let ((z (#_gtk_window_activate_default a))) (values (if (= 1 z) t nil))))
(defun gtk::window-set-transient-for (a b) (#_gtk_window_set_transient_for a b))
(defun gtk::window-get-transient-for (a) (#_gtk_window_get_transient_for a))
(defun gtk::window-set-type-hint (a b) (#_gtk_window_set_type_hint a b))
(defun gtk::window-get-type-hint (a) (#_gtk_window_get_type_hint a))
(defun gtk::window-set-destroy-with-parent (a b)
  (#_gtk_window_set_destroy_with_parent a (if b (if (eq b 0) 0 1) 0)))
(defun gtk::window-get-destroy-with-parent (a)
  (let ((z (#_gtk_window_get_destroy_with_parent a)))
    (values (if (= 1 z) t nil))))
(defun gtk::window-set-resizable (a b)
  (#_gtk_window_set_resizable a (if b (if (eq b 0) 0 1) 0)))
(defun gtk::window-get-resizable (a)
  (let ((z (#_gtk_window_get_resizable a))) (values (if (= 1 z) t nil))))
(defun gtk::window-set-gravity (a b) (#_gtk_window_set_gravity a b))
(defun gtk::window-get-gravity (a) (#_gtk_window_get_gravity a))
(defun gtk::window-set-geometry-hints (a b c d)
  (#_gtk_window_set_geometry_hints a b c d))
(defun gtk::window-set-has-frame (a b)
  (#_gtk_window_set_has_frame a (if b (if (eq b 0) 0 1) 0)))
(defun gtk::window-get-has-frame (a)
  (let ((z (#_gtk_window_get_has_frame a))) (values (if (= 1 z) t nil))))
(defun gtk::window-set-frame-dimensions (a b c d e)
  (#_gtk_window_set_frame_dimensions a b c d e))
(defun gtk::window-get-frame-dimensions (a b c d e)
  (rlet ((r1 :int b) (r2 :int c) (r3 :int d) (r4 :int e))
        (let ((z (#_gtk_window_get_frame_dimensions a r1 r2 r3 r4)))
          (values z
                  (%get-signed-long r1)
                  (%get-signed-long r2)
                  (%get-signed-long r3)
                  (%get-signed-long r4)))))
(defun gtk::window-set-decorated (a b)
  (#_gtk_window_set_decorated a (if b (if (eq b 0) 0 1) 0)))
(defun gtk::window-get-decorated (a)
  (let ((z (#_gtk_window_get_decorated a))) (values (if (= 1 z) t nil))))
(defun gtk::window-set-icon-list (a b) (#_gtk_window_set_icon_list a b))
(defun gtk::window-get-icon-list (a) (#_gtk_window_get_icon_list a))
(defun gtk::window-set-icon (a b) (#_gtk_window_set_icon a b))
(defun gtk::window-get-icon (a) (#_gtk_window_get_icon a))
(defun gtk::window-set-default-icon-list (a)
  (#_gtk_window_set_default_icon_list a))
(defun gtk::window-get-default-icon-list ()
  (#_gtk_window_get_default_icon_list))
(defun gtk::window-set-modal (a b)
  (#_gtk_window_set_modal a (if b (if (eq b 0) 0 1) 0)))
(defun gtk::window-get-modal (a)
  (let ((z (#_gtk_window_get_modal a))) (values (if (= 1 z) t nil))))
(defun gtk::window-list-toplevels () (#_gtk_window_list_toplevels))
(defun gtk::window-add-mnemonic (a b c) (#_gtk_window_add_mnemonic a b c))
(defun gtk::window-remove-mnemonic (a b c) (#_gtk_window_remove_mnemonic a b c))
(defun gtk::window-mnemonic-activate (a b c)
  (let ((z (#_gtk_window_mnemonic_activate a b c)))
    (values (if (= 1 z) t nil))))
(defun gtk::window-set-mnemonic-modifier (a b)
  (#_gtk_window_set_mnemonic_modifier a b))
(defun gtk::window-get-mnemonic-modifier (a)
  (#_gtk_window_get_mnemonic_modifier a))
(defun gtk::window-present (a) (#_gtk_window_present a))
(defun gtk::window-iconify (a) (#_gtk_window_iconify a))
(defun gtk::window-deiconify (a) (#_gtk_window_deiconify a))
(defun gtk::window-stick (a) (#_gtk_window_stick a))
(defun gtk::window-unstick (a) (#_gtk_window_unstick a))
(defun gtk::window-maximize (a) (#_gtk_window_maximize a))
(defun gtk::window-unmaximize (a) (#_gtk_window_unmaximize a))
(defun gtk::window-begin-resize-drag (a b c d e f)
  (#_gtk_window_begin_resize_drag a b c d e f))
(defun gtk::window-begin-move-drag (a b c d e)
  (#_gtk_window_begin_move_drag a b c d e))
(defun gtk::window-set-default-size (a b c)
  (#_gtk_window_set_default_size a b c))
(defun gtk::window-get-default-size (a b c)
  (rlet ((r1 :int b) (r2 :int c))
        (let ((z (#_gtk_window_get_default_size a r1 r2)))
          (values z (%get-signed-long r1) (%get-signed-long r2)))))
(defun gtk::window-resize (a b c) (#_gtk_window_resize a b c))
(defun gtk::window-get-size (a b c)
  (rlet ((r1 :int b) (r2 :int c))
        (let ((z (#_gtk_window_get_size a r1 r2)))
          (values z (%get-signed-long r1) (%get-signed-long r2)))))
(defun gtk::window-move (a b c) (#_gtk_window_move a b c))
(defun gtk::window-get-position (a b c)
  (rlet ((r1 :int b) (r2 :int c))
        (let ((z (#_gtk_window_get_position a r1 r2)))
          (values z (%get-signed-long r1) (%get-signed-long r2)))))
(defun gtk::window-parse-geometry (a b)
  (with-cstrs ((s1 b))
              (let ((z (#_gtk_window_parse_geometry a s1)))
                (values (if (= 1 z) t nil)))))
(defun gtk::window-reshow-with-initial-size (a)
  (#_gtk_window_reshow_with_initial_size a))
(defun gtk::window-group-get-type () (#_gtk_window_group_get_type))
(defun gtk::window-group-new () (#_gtk_window_group_new))
(defun gtk::window-group-add-window (a b) (#_gtk_window_group_add_window a b))
(defun gtk::window-group-remove-window (a b)
  (#_gtk_window_group_remove_window a b))
(defun gtk::window-remove-embedded-xid (a b)
  (#_gtk_window_remove_embedded_xid a b))
(defun gtk::window-add-embedded-xid (a b) (#_gtk_window_add_embedded_xid a b))
(defun gtk::menu-shell-get-type () (#_gtk_menu_shell_get_type))
(defun gtk::menu-shell-append (a b) (#_gtk_menu_shell_append a b))
(defun gtk::menu-shell-prepend (a b) (#_gtk_menu_shell_prepend a b))
(defun gtk::menu-shell-insert (a b c) (#_gtk_menu_shell_insert a b c))
(defun gtk::menu-shell-deactivate (a) (#_gtk_menu_shell_deactivate a))
(defun gtk::menu-shell-select-item (a b) (#_gtk_menu_shell_select_item a b))
(defun gtk::menu-shell-deselect (a) (#_gtk_menu_shell_deselect a))
(defun gtk::menu-shell-activate-item (a b c)
  (#_gtk_menu_shell_activate_item a b (if c (if (eq c 0) 0 1) 0)))
(defun gtk::menu-get-type () (#_gtk_menu_get_type))
(defun gtk::menu-new () (#_gtk_menu_new))
(defun gtk::menu-popup (a b c d e f g) (#_gtk_menu_popup a b c d e f g))
(defun gtk::menu-reposition (a) (#_gtk_menu_reposition a))
(defun gtk::menu-popdown (a) (#_gtk_menu_popdown a))
(defun gtk::menu-get-active (a) (#_gtk_menu_get_active a))
(defun gtk::menu-set-active (a b) (#_gtk_menu_set_active a b))
(defun gtk::menu-set-accel-group (a b) (#_gtk_menu_set_accel_group a b))
(defun gtk::menu-get-accel-group (a) (#_gtk_menu_get_accel_group a))
(defun gtk::menu-set-accel-path (a b)
  (with-cstrs ((s1 b)) (#_gtk_menu_set_accel_path a s1)))
(defun gtk::menu-attach-to-widget (a b c) (#_gtk_menu_attach_to_widget a b c))
(defun gtk::menu-detach (a) (#_gtk_menu_detach a))
(defun gtk::menu-get-attach-widget (a) (#_gtk_menu_get_attach_widget a))
(defun gtk::menu-set-tearoff-state (a b)
  (#_gtk_menu_set_tearoff_state a (if b (if (eq b 0) 0 1) 0)))
(defun gtk::menu-get-tearoff-state (a)
  (let ((z (#_gtk_menu_get_tearoff_state a))) (values (if (= 1 z) t nil))))
(defun gtk::menu-set-title (a b)
  (with-cstrs ((s1 b)) (#_gtk_menu_set_title a s1)))
(defun gtk::menu-get-title (a)
  (let ((z (#_gtk_menu_get_title a))) (values (%get-cstring z))))
(defun gtk::menu-reorder-child (a b c) (#_gtk_menu_reorder_child a b c))
(defun gtk::label-get-type () (#_gtk_label_get_type))
(defun gtk::label-new (a) (with-cstrs ((s1 a)) (#_gtk_label_new s1)))
(defun gtk::label-new-with-mnemonic (a)
  (with-cstrs ((s1 a)) (#_gtk_label_new_with_mnemonic s1)))
(defun gtk::label-set-text (a b)
  (with-cstrs ((s1 b)) (#_gtk_label_set_text a s1)))
(defun gtk::label-get-text (a)
  (let ((z (#_gtk_label_get_text a))) (values (%get-cstring z))))
(defun gtk::label-set-attributes (a b) (#_gtk_label_set_attributes a b))
(defun gtk::label-get-attributes (a) (#_gtk_label_get_attributes a))
(defun gtk::label-set-label (a b)
  (with-cstrs ((s1 b)) (#_gtk_label_set_label a s1)))
(defun gtk::label-get-label (a)
  (let ((z (#_gtk_label_get_label a))) (values (%get-cstring z))))
(defun gtk::label-set-markup (a b)
  (with-cstrs ((s1 b)) (#_gtk_label_set_markup a s1)))
(defun gtk::label-set-use-markup (a b)
  (#_gtk_label_set_use_markup a (if b (if (eq b 0) 0 1) 0)))
(defun gtk::label-get-use-markup (a)
  (let ((z (#_gtk_label_get_use_markup a))) (values (if (= 1 z) t nil))))
(defun gtk::label-set-use-underline (a b)
  (#_gtk_label_set_use_underline a (if b (if (eq b 0) 0 1) 0)))
(defun gtk::label-get-use-underline (a)
  (let ((z (#_gtk_label_get_use_underline a))) (values (if (= 1 z) t nil))))
(defun gtk::label-set-markup-with-mnemonic (a b)
  (with-cstrs ((s1 b)) (#_gtk_label_set_markup_with_mnemonic a s1)))
(defun gtk::label-get-mnemonic-keyval (a) (#_gtk_label_get_mnemonic_keyval a))
(defun gtk::label-set-mnemonic-widget (a b)
  (#_gtk_label_set_mnemonic_widget a b))
(defun gtk::label-get-mnemonic-widget (a) (#_gtk_label_get_mnemonic_widget a))
(defun gtk::label-set-text-with-mnemonic (a b)
  (with-cstrs ((s1 b)) (#_gtk_label_set_text_with_mnemonic a s1)))
(defun gtk::label-set-justify (a b) (#_gtk_label_set_justify a b))
(defun gtk::label-get-justify (a) (#_gtk_label_get_justify a))
(defun gtk::label-set-pattern (a b)
  (with-cstrs ((s1 b)) (#_gtk_label_set_pattern a s1)))
(defun gtk::label-set-line-wrap (a b)
  (#_gtk_label_set_line_wrap a (if b (if (eq b 0) 0 1) 0)))
(defun gtk::label-get-line-wrap (a)
  (let ((z (#_gtk_label_get_line_wrap a))) (values (if (= 1 z) t nil))))
(defun gtk::label-set-selectable (a b)
  (#_gtk_label_set_selectable a (if b (if (eq b 0) 0 1) 0)))
(defun gtk::label-get-selectable (a)
  (let ((z (#_gtk_label_get_selectable a))) (values (if (= 1 z) t nil))))
(defun gtk::label-select-region (a b c) (#_gtk_label_select_region a b c))
(defun gtk::label-get-selection-bounds (a b c)
  (rlet ((r1 :int b) (r2 :int c))
        (let ((z (#_gtk_label_get_selection_bounds a r1 r2)))
          (values (if (= 1 z) t nil)
                  (%get-signed-long r1)
                  (%get-signed-long r2)))))
(defun gtk::label-get-layout (a) (#_gtk_label_get_layout a))
(defun gtk::label-get-layout-offsets (a b c)
  (rlet ((r1 :int b) (r2 :int c))
        (let ((z (#_gtk_label_get_layout_offsets a r1 r2)))
          (values z (%get-signed-long r1) (%get-signed-long r2)))))
(defun gtk::accel-label-get-type () (#_gtk_accel_label_get_type))
(defun gtk::accel-label-new (a)
  (with-cstrs ((s1 a)) (#_gtk_accel_label_new s1)))
(defun gtk::accel-label-get-accel-widget (a)
  (#_gtk_accel_label_get_accel_widget a))
(defun gtk::accel-label-get-accel-width (a)
  (#_gtk_accel_label_get_accel_width a))
(defun gtk::accel-label-set-accel-widget (a b)
  (#_gtk_accel_label_set_accel_widget a b))
(defun gtk::accel-label-set-accel-closure (a b)
  (#_gtk_accel_label_set_accel_closure a b))
(defun gtk::accel-label-refetch (a)
  (let ((z (#_gtk_accel_label_refetch a))) (values (if (= 1 z) t nil))))
(defun gtk::accel-map-lookup-entry (a b)
  (with-cstrs ((s1 a))
              (let ((z (#_gtk_accel_map_lookup_entry s1 b)))
                (values (if (= 1 z) t nil)))))
(defun gtk::accel-map-change-entry (a b c d)
  (with-cstrs ((s1 a))
              (let ((z
                     (#_gtk_accel_map_change_entry s1 b c
                      (if d (if (eq d 0) 0 1) 0))))
                (values (if (= 1 z) t nil)))))
(defun gtk::accel-map-load (a) (with-cstrs ((s1 a)) (#_gtk_accel_map_load s1)))
(defun gtk::accel-map-save (a) (with-cstrs ((s1 a)) (#_gtk_accel_map_save s1)))
(defun gtk::accel-map-foreach (a b) (#_gtk_accel_map_foreach a b))
(defun gtk::accel-map-load-fd (a) (#_gtk_accel_map_load_fd a))
(defun gtk::accel-map-load-scanner (a) (#_gtk_accel_map_load_scanner a))
(defun gtk::accel-map-save-fd (a) (#_gtk_accel_map_save_fd a))
(defun gtk::accel-map-add-filter (a)
  (with-cstrs ((s1 a)) (#_gtk_accel_map_add_filter s1)))
(defun gtk::accel-map-foreach-unfiltered (a b)
  (#_gtk_accel_map_foreach_unfiltered a b))
(defun atk::action-get-type () (#_atk_action_get_type))
(defun atk::action-do-action (a b)
  (let ((z (#_atk_action_do_action a b))) (values (if (= 1 z) t nil))))
(defun atk::action-get-n-actions (a) (#_atk_action_get_n_actions a))
(defun atk::action-get-description (a b)
  (let ((z (#_atk_action_get_description a b))) (values (%get-cstring z))))
(defun atk::action-get-name (a b)
  (let ((z (#_atk_action_get_name a b))) (values (%get-cstring z))))
(defun atk::action-get-keybinding (a b)
  (let ((z (#_atk_action_get_keybinding a b))) (values (%get-cstring z))))
(defun atk::action-set-description (a b c)
  (with-cstrs ((s1 c))
              (let ((z (#_atk_action_set_description a b s1)))
                (values (if (= 1 z) t nil)))))
(defun atk::util-get-type () (#_atk_util_get_type))
(defun atk::add-focus-tracker (a) (#_atk_add_focus_tracker a))
(defun atk::remove-focus-tracker (a) (#_atk_remove_focus_tracker a))
(defun atk::focus-tracker-init (a) (#_atk_focus_tracker_init a))
(defun atk::focus-tracker-notify (a) (#_atk_focus_tracker_notify a))
(defun atk::add-global-event-listener (a b)
  (with-cstrs ((s1 b)) (#_atk_add_global_event_listener a s1)))
(defun atk::remove-global-event-listener (a)
  (#_atk_remove_global_event_listener a))
(defun atk::add-key-event-listener (a b) (#_atk_add_key_event_listener a b))
(defun atk::remove-key-event-listener (a) (#_atk_remove_key_event_listener a))
(defun atk::get-root () (#_atk_get_root))
(defun atk::get-toolkit-name ()
  (let ((z (#_atk_get_toolkit_name))) (values (%get-cstring z))))
(defun atk::get-toolkit-version ()
  (let ((z (#_atk_get_toolkit_version))) (values (%get-cstring z))))
(defun atk::component-get-type () (#_atk_component_get_type))
(defun atk::component-add-focus-handler (a b)
  (#_atk_component_add_focus_handler a b))
(defun atk::component-contains (a b c d)
  (let ((z (#_atk_component_contains a b c d))) (values (if (= 1 z) t nil))))
(defun atk::component-ref-accessible-at-point (a b c d)
  (#_atk_component_ref_accessible_at_point a b c d))
(defun atk::component-get-extents (a b c d e f)
  (rlet ((r1 :int b) (r2 :int c) (r3 :int d) (r4 :int e))
        (let ((z (#_atk_component_get_extents a r1 r2 r3 r4 f)))
          (values z
                  (%get-signed-long r1)
                  (%get-signed-long r2)
                  (%get-signed-long r3)
                  (%get-signed-long r4)))))
(defun atk::component-get-position (a b c d)
  (rlet ((r1 :int b) (r2 :int c))
        (let ((z (#_atk_component_get_position a r1 r2 d)))
          (values z (%get-signed-long r1) (%get-signed-long r2)))))
(defun atk::component-get-size (a b c)
  (rlet ((r1 :int b) (r2 :int c))
        (let ((z (#_atk_component_get_size a r1 r2)))
          (values z (%get-signed-long r1) (%get-signed-long r2)))))
(defun atk::component-grab-focus (a)
  (let ((z (#_atk_component_grab_focus a))) (values (if (= 1 z) t nil))))
(defun atk::component-remove-focus-handler (a b)
  (#_atk_component_remove_focus_handler a b))
(defun atk::component-set-extents (a b c d e f)
  (let ((z (#_atk_component_set_extents a b c d e f)))
    (values (if (= 1 z) t nil))))
(defun atk::component-set-position (a b c d)
  (let ((z (#_atk_component_set_position a b c d)))
    (values (if (= 1 z) t nil))))
(defun atk::component-set-size (a b c)
  (let ((z (#_atk_component_set_size a b c))) (values (if (= 1 z) t nil))))
(defun atk::document-get-type () (#_atk_document_get_type))
(defun atk::document-get-document-type (a)
  (let ((z (#_atk_document_get_document_type a))) (values (%get-cstring z))))
(defun atk::document-get-document (a) (#_atk_document_get_document a))
(defun atk::text-get-type () (#_atk_text_get_type))
(defun atk::text-get-text (a b c)
  (let ((z (#_atk_text_get_text a b c))) (values (%get-cstring z))))
(defun atk::text-get-character-at-offset (a b)
  (#_atk_text_get_character_at_offset a b))
(defun atk::text-get-text-after-offset (a b c d e)
  (rlet ((r1 :int d) (r2 :int e))
        (let ((z (#_atk_text_get_text_after_offset a b c r1 r2)))
          (values (%get-cstring z)
                  (%get-signed-long r1)
                  (%get-signed-long r2)))))
(defun atk::text-get-text-at-offset (a b c d e)
  (rlet ((r1 :int d) (r2 :int e))
        (let ((z (#_atk_text_get_text_at_offset a b c r1 r2)))
          (values (%get-cstring z)
                  (%get-signed-long r1)
                  (%get-signed-long r2)))))
(defun atk::text-get-text-before-offset (a b c d e)
  (rlet ((r1 :int d) (r2 :int e))
        (let ((z (#_atk_text_get_text_before_offset a b c r1 r2)))
          (values (%get-cstring z)
                  (%get-signed-long r1)
                  (%get-signed-long r2)))))
(defun atk::text-get-caret-offset (a) (#_atk_text_get_caret_offset a))
(defun atk::text-get-character-extents (a b c d e f g)
  (rlet ((r1 :int c) (r2 :int d) (r3 :int e) (r4 :int f))
        (let ((z (#_atk_text_get_character_extents a b r1 r2 r3 r4 g)))
          (values z
                  (%get-signed-long r1)
                  (%get-signed-long r2)
                  (%get-signed-long r3)
                  (%get-signed-long r4)))))
(defun atk::text-get-run-attributes (a b c d)
  (rlet ((r1 :int c) (r2 :int d))
        (let ((z (#_atk_text_get_run_attributes a b r1 r2)))
          (values z (%get-signed-long r1) (%get-signed-long r2)))))
(defun atk::text-get-default-attributes (a)
  (#_atk_text_get_default_attributes a))
(defun atk::text-get-character-count (a) (#_atk_text_get_character_count a))
(defun atk::text-get-offset-at-point (a b c d)
  (#_atk_text_get_offset_at_point a b c d))
(defun atk::text-get-n-selections (a) (#_atk_text_get_n_selections a))
(defun atk::text-get-selection (a b c d)
  (rlet ((r1 :int c) (r2 :int d))
        (let ((z (#_atk_text_get_selection a b r1 r2)))
          (values (%get-cstring z)
                  (%get-signed-long r1)
                  (%get-signed-long r2)))))
(defun atk::text-add-selection (a b c)
  (let ((z (#_atk_text_add_selection a b c))) (values (if (= 1 z) t nil))))
(defun atk::text-remove-selection (a b)
  (let ((z (#_atk_text_remove_selection a b))) (values (if (= 1 z) t nil))))
(defun atk::text-set-selection (a b c d)
  (let ((z (#_atk_text_set_selection a b c d))) (values (if (= 1 z) t nil))))
(defun atk::text-set-caret-offset (a b)
  (let ((z (#_atk_text_set_caret_offset a b))) (values (if (= 1 z) t nil))))
(defun atk::attribute-set-free (a) (#_atk_attribute_set_free a))
(defun atk::text-attribute-get-name (a)
  (let ((z (#_atk_text_attribute_get_name a))) (values (%get-cstring z))))
(defun atk::text-attribute-get-value (a b)
  (let ((z (#_atk_text_attribute_get_value a b))) (values (%get-cstring z))))
(defun atk::editable-text-get-type () (#_atk_editable_text_get_type))
(defun atk::editable-text-set-run-attributes (a b c d)
  (let ((z (#_atk_editable_text_set_run_attributes a b c d)))
    (values (if (= 1 z) t nil))))
(defun atk::editable-text-set-text-contents (a b)
  (with-cstrs ((s1 b)) (#_atk_editable_text_set_text_contents a s1)))
(defun atk::editable-text-insert-text (a b c d)
  (with-cstrs ((s1 b))
              (rlet ((r1 :int d))
                    (let ((z (#_atk_editable_text_insert_text a s1 c r1)))
                      (values z (%get-signed-long r1))))))
(defun atk::editable-text-copy-text (a b c)
  (#_atk_editable_text_copy_text a b c))
(defun atk::editable-text-cut-text (a b c) (#_atk_editable_text_cut_text a b c))
(defun atk::editable-text-delete-text (a b c)
  (#_atk_editable_text_delete_text a b c))
(defun atk::editable-text-paste-text (a b) (#_atk_editable_text_paste_text a b))
(defun atk::hyperlink-get-type () (#_atk_hyperlink_get_type))
(defun atk::hyperlink-get-uri (a b)
  (let ((z (#_atk_hyperlink_get_uri a b))) (values (%get-cstring z))))
(defun atk::hyperlink-get-object (a b) (#_atk_hyperlink_get_object a b))
(defun atk::hyperlink-get-end-index (a) (#_atk_hyperlink_get_end_index a))
(defun atk::hyperlink-get-start-index (a) (#_atk_hyperlink_get_start_index a))
(defun atk::hyperlink-is-valid (a)
  (let ((z (#_atk_hyperlink_is_valid a))) (values (if (= 1 z) t nil))))
(defun atk::hyperlink-get-n-anchors (a) (#_atk_hyperlink_get_n_anchors a))
(defun atk::hypertext-get-type () (#_atk_hypertext_get_type))
(defun atk::hypertext-get-link (a b) (#_atk_hypertext_get_link a b))
(defun atk::hypertext-get-n-links (a) (#_atk_hypertext_get_n_links a))
(defun atk::hypertext-get-link-index (a b) (#_atk_hypertext_get_link_index a b))
(defun atk::image-get-type () (#_atk_image_get_type))
(defun atk::image-get-image-description (a)
  (let ((z (#_atk_image_get_image_description a))) (values (%get-cstring z))))
(defun atk::image-get-image-size (a b c)
  (rlet ((r1 :int b) (r2 :int c))
        (let ((z (#_atk_image_get_image_size a r1 r2)))
          (values z (%get-signed-long r1) (%get-signed-long r2)))))
(defun atk::image-set-image-description (a b)
  (with-cstrs ((s1 b))
              (let ((z (#_atk_image_set_image_description a s1)))
                (values (if (= 1 z) t nil)))))
(defun atk::image-get-image-position (a b c d)
  (rlet ((r1 :int b) (r2 :int c))
        (let ((z (#_atk_image_get_image_position a r1 r2 d)))
          (values z (%get-signed-long r1) (%get-signed-long r2)))))
(defun atk::no-op-object-get-type () (#_atk_no_op_object_get_type))
(defun atk::no-op-object-new (a) (#_atk_no_op_object_new a))
(defun atk::object-factory-get-type () (#_atk_object_factory_get_type))
(defun atk::object-factory-create-accessible (a b)
  (#_atk_object_factory_create_accessible a b))
(defun atk::object-factory-invalidate (a) (#_atk_object_factory_invalidate a))
(defun atk::no-op-object-factory-get-type ()
  (#_atk_no_op_object_factory_get_type))
(defun atk::no-op-object-factory-new () (#_atk_no_op_object_factory_new))
(defun atk::registry-get-type () (#_atk_registry_get_type))
(defun atk::registry-set-factory-type (a b c)
  (#_atk_registry_set_factory_type a b c))
(defun atk::registry-get-factory-type (a b)
  (#_atk_registry_get_factory_type a b))
(defun atk::registry-get-factory (a b) (#_atk_registry_get_factory a b))
(defun atk::get-default-registry () (#_atk_get_default_registry))
(defun atk::relation-get-type () (#_atk_relation_get_type))
(defun atk::relation-type-register (a)
  (with-cstrs ((s1 a)) (#_atk_relation_type_register s1)))
(defun atk::relation-type-get-name (a)
  (let ((z (#_atk_relation_type_get_name a))) (values (%get-cstring z))))
(defun atk::relation-type-for-name (a)
  (with-cstrs ((s1 a)) (#_atk_relation_type_for_name s1)))
(defun atk::relation-new (a b c) (#_atk_relation_new a b c))
(defun atk::relation-get-relation-type (a) (#_atk_relation_get_relation_type a))
(defun atk::relation-get-target (a) (#_atk_relation_get_target a))
(defun atk::relation-set-get-type () (#_atk_relation_set_get_type))
(defun atk::relation-set-new () (#_atk_relation_set_new))
(defun atk::relation-set-contains (a b)
  (let ((z (#_atk_relation_set_contains a b))) (values (if (= 1 z) t nil))))
(defun atk::relation-set-remove (a b) (#_atk_relation_set_remove a b))
(defun atk::relation-set-add (a b) (#_atk_relation_set_add a b))
(defun atk::relation-set-get-n-relations (a)
  (#_atk_relation_set_get_n_relations a))
(defun atk::relation-set-get-relation (a b)
  (#_atk_relation_set_get_relation a b))
(defun atk::relation-set-get-relation-by-type (a b)
  (#_atk_relation_set_get_relation_by_type a b))
(defun atk::selection-get-type () (#_atk_selection_get_type))
(defun atk::selection-add-selection (a b)
  (let ((z (#_atk_selection_add_selection a b))) (values (if (= 1 z) t nil))))
(defun atk::selection-clear-selection (a)
  (let ((z (#_atk_selection_clear_selection a))) (values (if (= 1 z) t nil))))
(defun atk::selection-ref-selection (a b) (#_atk_selection_ref_selection a b))
(defun atk::selection-get-selection-count (a)
  (#_atk_selection_get_selection_count a))
(defun atk::selection-is-child-selected (a b)
  (let ((z (#_atk_selection_is_child_selected a b)))
    (values (if (= 1 z) t nil))))
(defun atk::selection-remove-selection (a b)
  (let ((z (#_atk_selection_remove_selection a b)))
    (values (if (= 1 z) t nil))))
(defun atk::selection-select-all-selection (a)
  (let ((z (#_atk_selection_select_all_selection a)))
    (values (if (= 1 z) t nil))))
(defun atk::state-set-get-type () (#_atk_state_set_get_type))
(defun atk::state-set-new () (#_atk_state_set_new))
(defun atk::state-set-is-empty (a)
  (let ((z (#_atk_state_set_is_empty a))) (values (if (= 1 z) t nil))))
(defun atk::state-set-add-state (a b)
  (let ((z (#_atk_state_set_add_state a b))) (values (if (= 1 z) t nil))))
(defun atk::state-set-add-states (a b c)
  (rlet ((r1 :int b))
        (let ((z (#_atk_state_set_add_states a r1 c)))
          (values z (%get-signed-long r1)))))
(defun atk::state-set-clear-states (a) (#_atk_state_set_clear_states a))
(defun atk::state-set-contains-state (a b)
  (let ((z (#_atk_state_set_contains_state a b))) (values (if (= 1 z) t nil))))
(defun atk::state-set-contains-states (a b c)
  (rlet ((r1 :int b))
        (let ((z (#_atk_state_set_contains_states a r1 c)))
          (values (if (= 1 z) t nil) (%get-signed-long r1)))))
(defun atk::state-set-remove-state (a b)
  (let ((z (#_atk_state_set_remove_state a b))) (values (if (= 1 z) t nil))))
(defun atk::state-set-and-sets (a b) (#_atk_state_set_and_sets a b))
(defun atk::state-set-or-sets (a b) (#_atk_state_set_or_sets a b))
(defun atk::state-set-xor-sets (a b) (#_atk_state_set_xor_sets a b))
(defun atk::streamable-content-get-type () (#_atk_streamable_content_get_type))
(defun atk::streamable-content-get-n-mime-types (a)
  (#_atk_streamable_content_get_n_mime_types a))
(defun atk::streamable-content-get-mime-type (a b)
  (let ((z (#_atk_streamable_content_get_mime_type a b)))
    (values (%get-cstring z))))
(defun atk::streamable-content-get-stream (a b)
  (with-cstrs ((s1 b)) (#_atk_streamable_content_get_stream a s1)))
(defun atk::table-get-type () (#_atk_table_get_type))
(defun atk::table-ref-at (a b c) (#_atk_table_ref_at a b c))
(defun atk::table-get-index-at (a b c) (#_atk_table_get_index_at a b c))
(defun atk::table-get-column-at-index (a b)
  (#_atk_table_get_column_at_index a b))
(defun atk::table-get-row-at-index (a b) (#_atk_table_get_row_at_index a b))
(defun atk::table-get-n-columns (a) (#_atk_table_get_n_columns a))
(defun atk::table-get-n-rows (a) (#_atk_table_get_n_rows a))
(defun atk::table-get-column-extent-at (a b c)
  (#_atk_table_get_column_extent_at a b c))
(defun atk::table-get-row-extent-at (a b c)
  (#_atk_table_get_row_extent_at a b c))
(defun atk::table-get-caption (a) (#_atk_table_get_caption a))
(defun atk::table-get-column-description (a b)
  (let ((z (#_atk_table_get_column_description a b)))
    (values (%get-cstring z))))
(defun atk::table-get-column-header (a b) (#_atk_table_get_column_header a b))
(defun atk::table-get-row-description (a b)
  (let ((z (#_atk_table_get_row_description a b))) (values (%get-cstring z))))
(defun atk::table-get-row-header (a b) (#_atk_table_get_row_header a b))
(defun atk::table-get-summary (a) (#_atk_table_get_summary a))
(defun atk::table-set-caption (a b) (#_atk_table_set_caption a b))
(defun atk::table-set-column-description (a b c)
  (with-cstrs ((s1 c)) (#_atk_table_set_column_description a b s1)))
(defun atk::table-set-column-header (a b c)
  (#_atk_table_set_column_header a b c))
(defun atk::table-set-row-description (a b c)
  (with-cstrs ((s1 c)) (#_atk_table_set_row_description a b s1)))
(defun atk::table-set-row-header (a b c) (#_atk_table_set_row_header a b c))
(defun atk::table-set-summary (a b) (#_atk_table_set_summary a b))
(defun atk::table-get-selected-columns (a b)
  (#_atk_table_get_selected_columns a b))
(defun atk::table-get-selected-rows (a b) (#_atk_table_get_selected_rows a b))
(defun atk::table-is-column-selected (a b)
  (let ((z (#_atk_table_is_column_selected a b))) (values (if (= 1 z) t nil))))
(defun atk::table-is-row-selected (a b)
  (let ((z (#_atk_table_is_row_selected a b))) (values (if (= 1 z) t nil))))
(defun atk::table-is-selected (a b c)
  (let ((z (#_atk_table_is_selected a b c))) (values (if (= 1 z) t nil))))
(defun atk::table-add-row-selection (a b)
  (let ((z (#_atk_table_add_row_selection a b))) (values (if (= 1 z) t nil))))
(defun atk::table-remove-row-selection (a b)
  (let ((z (#_atk_table_remove_row_selection a b)))
    (values (if (= 1 z) t nil))))
(defun atk::table-add-column-selection (a b)
  (let ((z (#_atk_table_add_column_selection a b)))
    (values (if (= 1 z) t nil))))
(defun atk::table-remove-column-selection (a b)
  (let ((z (#_atk_table_remove_column_selection a b)))
    (values (if (= 1 z) t nil))))
(defun atk::value-get-type () (#_atk_value_get_type))
(defun atk::value-get-current-value (a b) (#_atk_value_get_current_value a b))
(defun atk::value-get-maximum-value (a b) (#_atk_value_get_maximum_value a b))
(defun atk::value-get-minimum-value (a b) (#_atk_value_get_minimum_value a b))
(defun atk::value-set-current-value (a b)
  (let ((z (#_atk_value_set_current_value a b))) (values (if (= 1 z) t nil))))
(defun gtk::accessible-get-type () (#_gtk_accessible_get_type))
(defun gtk::accessible-connect-widget-destroyed (a)
  (#_gtk_accessible_connect_widget_destroyed a))
(defun gtk::alignment-get-type () (#_gtk_alignment_get_type))
(defun gtk::alignment-new (a b c d)
  (#_gtk_alignment_new (coerce a 'single-float) (coerce b 'single-float)
   (coerce c 'single-float) (coerce d 'single-float)))
(defun gtk::alignment-set (a b c d e)
  (#_gtk_alignment_set a (coerce b 'single-float) (coerce c 'single-float)
   (coerce d 'single-float) (coerce e 'single-float)))
(defun gtk::frame-get-type () (#_gtk_frame_get_type))
(defun gtk::frame-new (a) (with-cstrs ((s1 a)) (#_gtk_frame_new s1)))
(defun gtk::frame-set-label (a b)
  (with-cstrs ((s1 b)) (#_gtk_frame_set_label a s1)))
(defun gtk::frame-get-label (a)
  (let ((z (#_gtk_frame_get_label a))) (values (%get-cstring z))))
(defun gtk::frame-set-label-widget (a b) (#_gtk_frame_set_label_widget a b))
(defun gtk::frame-get-label-widget (a) (#_gtk_frame_get_label_widget a))
(defun gtk::frame-set-label-align (a b c)
  (#_gtk_frame_set_label_align a (coerce b 'single-float)
   (coerce c 'single-float)))
(defun gtk::frame-get-label-align (a b c)
  (rlet ((r1 :float (coerce b 'single-float))
         (r2 :float (coerce c 'single-float)))
        (let ((z (#_gtk_frame_get_label_align a r1 r2)))
          (values z (%get-single-float r1) (%get-single-float r2)))))
(defun gtk::frame-set-shadow-type (a b) (#_gtk_frame_set_shadow_type a b))
(defun gtk::frame-get-shadow-type (a) (#_gtk_frame_get_shadow_type a))
(defun gtk::aspect-frame-get-type () (#_gtk_aspect_frame_get_type))
(defun gtk::aspect-frame-new (a b c d e)
  (with-cstrs ((s1 a))
              (#_gtk_aspect_frame_new s1 (coerce b 'single-float)
               (coerce c 'single-float) (coerce d 'single-float)
               (if e (if (eq e 0) 0 1) 0))))
(defun gtk::aspect-frame-set (a b c d e)
  (#_gtk_aspect_frame_set a (coerce b 'single-float) (coerce c 'single-float)
   (coerce d 'single-float) (if e (if (eq e 0) 0 1) 0)))
(defun gtk::arrow-get-type () (#_gtk_arrow_get_type))
(defun gtk::arrow-new (a b) (#_gtk_arrow_new a b))
(defun gtk::arrow-set (a b c) (#_gtk_arrow_set a b c))
(defun gtk::binding-set-new (a)
  (with-cstrs ((s1 a)) (#_gtk_binding_set_new s1)))
(defun gtk::binding-set-by-class (a) (#_gtk_binding_set_by_class a))
(defun gtk::binding-set-find (a)
  (with-cstrs ((s1 a)) (#_gtk_binding_set_find s1)))
(defun gtk::bindings-activate (a b c)
  (let ((z (#_gtk_bindings_activate a b c))) (values (if (= 1 z) t nil))))
(defun gtk::binding-set-activate (a b c d)
  (let ((z (#_gtk_binding_set_activate a b c d))) (values (if (= 1 z) t nil))))
(defun gtk::binding-entry-clear (a b c) (#_gtk_binding_entry_clear a b c))
(defun gtk::binding-entry-add-signal (a b c d e)
  (with-cstrs ((s1 d)) (#_gtk_binding_entry_add_signal a b c s1 e)))
(defun gtk::binding-set-add-path (a b c d)
  (with-cstrs ((s1 c)) (#_gtk_binding_set_add_path a b s1 d)))
(defun gtk::binding-entry-remove (a b c) (#_gtk_binding_entry_remove a b c))
(defun gtk::binding-entry-add-signall (a b c d e)
  (with-cstrs ((s1 d)) (#_gtk_binding_entry_add_signall a b c s1 e)))
(defun gtk::binding-parse-binding (a) (#_gtk_binding_parse_binding a))
(defun gtk::box-get-type () (#_gtk_box_get_type))
(defun gtk::box-pack-start (a b c d e)
  (#_gtk_box_pack_start a b (if c (if (eq c 0) 0 1) 0)
   (if d (if (eq d 0) 0 1) 0) e))
(defun gtk::box-pack-end (a b c d e)
  (#_gtk_box_pack_end a b (if c (if (eq c 0) 0 1) 0) (if d (if (eq d 0) 0 1) 0)
   e))
(defun gtk::box-set-homogeneous (a b)
  (#_gtk_box_set_homogeneous a (if b (if (eq b 0) 0 1) 0)))
(defun gtk::box-get-homogeneous (a)
  (let ((z (#_gtk_box_get_homogeneous a))) (values (if (= 1 z) t nil))))
(defun gtk::box-set-spacing (a b) (#_gtk_box_set_spacing a b))
(defun gtk::box-get-spacing (a) (#_gtk_box_get_spacing a))
(defun gtk::box-reorder-child (a b c) (#_gtk_box_reorder_child a b c))
(defun gtk::box-query-child-packing (a b c d e f)
  (rlet ((r1 :int f))
        (let ((z (#_gtk_box_query_child_packing a b c d e r1)))
          (values z (%get-signed-long r1)))))
(defun gtk::box-set-child-packing (a b c d e f)
  (#_gtk_box_set_child_packing a b (if c (if (eq c 0) 0 1) 0)
   (if d (if (eq d 0) 0 1) 0) e f))
(defun gtk::button-box-get-type () (#_gtk_button_box_get_type))
(defun gtk::button-box-get-layout (a) (#_gtk_button_box_get_layout a))
(defun gtk::button-box-set-layout (a b) (#_gtk_button_box_set_layout a b))
(defun gtk::button-box-set-child-secondary (a b c)
  (#_gtk_button_box_set_child_secondary a b (if c (if (eq c 0) 0 1) 0)))
(defun gtk::button-get-type () (#_gtk_button_get_type))
(defun gtk::button-new () (#_gtk_button_new))
(defun gtk::button-new-with-label (a)
  (with-cstrs ((s1 a)) (#_gtk_button_new_with_label s1)))
(defun gtk::button-new-from-stock (a)
  (with-cstrs ((s1 a)) (#_gtk_button_new_from_stock s1)))
(defun gtk::button-new-with-mnemonic (a)
  (with-cstrs ((s1 a)) (#_gtk_button_new_with_mnemonic s1)))
(defun gtk::button-pressed (a) (#_gtk_button_pressed a))
(defun gtk::button-released (a) (#_gtk_button_released a))
(defun gtk::button-clicked (a) (#_gtk_button_clicked a))
(defun gtk::button-enter (a) (#_gtk_button_enter a))
(defun gtk::button-leave (a) (#_gtk_button_leave a))
(defun gtk::button-set-relief (a b) (#_gtk_button_set_relief a b))
(defun gtk::button-get-relief (a) (#_gtk_button_get_relief a))
(defun gtk::button-set-label (a b)
  (with-cstrs ((s1 b)) (#_gtk_button_set_label a s1)))
(defun gtk::button-get-label (a)
  (let ((z (#_gtk_button_get_label a))) (values (%get-cstring z))))
(defun gtk::button-set-use-underline (a b)
  (#_gtk_button_set_use_underline a (if b (if (eq b 0) 0 1) 0)))
(defun gtk::button-get-use-underline (a)
  (let ((z (#_gtk_button_get_use_underline a))) (values (if (= 1 z) t nil))))
(defun gtk::button-set-use-stock (a b)
  (#_gtk_button_set_use_stock a (if b (if (eq b 0) 0 1) 0)))
(defun gtk::button-get-use-stock (a)
  (let ((z (#_gtk_button_get_use_stock a))) (values (if (= 1 z) t nil))))
(defun gtk::calendar-get-type () (#_gtk_calendar_get_type))
(defun gtk::calendar-new () (#_gtk_calendar_new))
(defun gtk::calendar-select-month (a b c)
  (let ((z (#_gtk_calendar_select_month a b c))) (values (if (= 1 z) t nil))))
(defun gtk::calendar-select-day (a b) (#_gtk_calendar_select_day a b))
(defun gtk::calendar-mark-day (a b)
  (let ((z (#_gtk_calendar_mark_day a b))) (values (if (= 1 z) t nil))))
(defun gtk::calendar-unmark-day (a b)
  (let ((z (#_gtk_calendar_unmark_day a b))) (values (if (= 1 z) t nil))))
(defun gtk::calendar-clear-marks (a) (#_gtk_calendar_clear_marks a))
(defun gtk::calendar-display-options (a b) (#_gtk_calendar_display_options a b))
(defun gtk::calendar-get-date (a b c d) (#_gtk_calendar_get_date a b c d))
(defun gtk::calendar-freeze (a) (#_gtk_calendar_freeze a))
(defun gtk::calendar-thaw (a) (#_gtk_calendar_thaw a))
(defun gtk::cell-editable-get-type () (#_gtk_cell_editable_get_type))
(defun gtk::cell-editable-start-editing (a b)
  (#_gtk_cell_editable_start_editing a b))
(defun gtk::cell-editable-editing-done (a) (#_gtk_cell_editable_editing_done a))
(defun gtk::cell-editable-remove-widget (a)
  (#_gtk_cell_editable_remove_widget a))
(defun gtk::cell-renderer-get-type () (#_gtk_cell_renderer_get_type))
(defun gtk::cell-renderer-get-size (a b c d e f g)
  (rlet ((r1 :int d) (r2 :int e) (r3 :int f) (r4 :int g))
        (let ((z (#_gtk_cell_renderer_get_size a b c r1 r2 r3 r4)))
          (values z
                  (%get-signed-long r1)
                  (%get-signed-long r2)
                  (%get-signed-long r3)
                  (%get-signed-long r4)))))
(defun gtk::cell-renderer-render (a b c d e f g)
  (#_gtk_cell_renderer_render a b c d e f g))
(defun gtk::cell-renderer-activate (a b c d e f g)
  (with-cstrs ((s1 d))
              (let ((z (#_gtk_cell_renderer_activate a b c s1 e f g)))
                (values (if (= 1 z) t nil)))))
(defun gtk::cell-renderer-start-editing (a b c d e f g)
  (with-cstrs ((s1 d)) (#_gtk_cell_renderer_start_editing a b c s1 e f g)))
(defun gtk::cell-renderer-set-fixed-size (a b c)
  (#_gtk_cell_renderer_set_fixed_size a b c))
(defun gtk::cell-renderer-get-fixed-size (a b c)
  (rlet ((r1 :int b) (r2 :int c))
        (let ((z (#_gtk_cell_renderer_get_fixed_size a r1 r2)))
          (values z (%get-signed-long r1) (%get-signed-long r2)))))
(defun gtk::cell-renderer-text-get-type () (#_gtk_cell_renderer_text_get_type))
(defun gtk::cell-renderer-text-new () (#_gtk_cell_renderer_text_new))
(defun gtk::cell-renderer-text-set-fixed-height-from-font (a b)
  (#_gtk_cell_renderer_text_set_fixed_height_from_font a b))
(defun gtk::cell-renderer-toggle-get-type ()
  (#_gtk_cell_renderer_toggle_get_type))
(defun gtk::cell-renderer-toggle-new () (#_gtk_cell_renderer_toggle_new))
(defun gtk::cell-renderer-toggle-get-radio (a)
  (let ((z (#_gtk_cell_renderer_toggle_get_radio a)))
    (values (if (= 1 z) t nil))))
(defun gtk::cell-renderer-toggle-set-radio (a b)
  (#_gtk_cell_renderer_toggle_set_radio a (if b (if (eq b 0) 0 1) 0)))
(defun gtk::cell-renderer-toggle-get-active (a)
  (let ((z (#_gtk_cell_renderer_toggle_get_active a)))
    (values (if (= 1 z) t nil))))
(defun gtk::cell-renderer-toggle-set-active (a b)
  (#_gtk_cell_renderer_toggle_set_active a (if b (if (eq b 0) 0 1) 0)))
(defun gtk::cell-renderer-pixbuf-get-type ()
  (#_gtk_cell_renderer_pixbuf_get_type))
(defun gtk::cell-renderer-pixbuf-new () (#_gtk_cell_renderer_pixbuf_new))
(defun gtk::toggle-button-get-type () (#_gtk_toggle_button_get_type))
(defun gtk::toggle-button-new () (#_gtk_toggle_button_new))
(defun gtk::toggle-button-new-with-label (a)
  (with-cstrs ((s1 a)) (#_gtk_toggle_button_new_with_label s1)))
(defun gtk::toggle-button-new-with-mnemonic (a)
  (with-cstrs ((s1 a)) (#_gtk_toggle_button_new_with_mnemonic s1)))
(defun gtk::toggle-button-set-mode (a b)
  (#_gtk_toggle_button_set_mode a (if b (if (eq b 0) 0 1) 0)))
(defun gtk::toggle-button-get-mode (a)
  (let ((z (#_gtk_toggle_button_get_mode a))) (values (if (= 1 z) t nil))))
(defun gtk::toggle-button-set-active (a b)
  (#_gtk_toggle_button_set_active a (if b (if (eq b 0) 0 1) 0)))
(defun gtk::toggle-button-get-active (a)
  (let ((z (#_gtk_toggle_button_get_active a))) (values (if (= 1 z) t nil))))
(defun gtk::toggle-button-toggled (a) (#_gtk_toggle_button_toggled a))
(defun gtk::toggle-button-set-inconsistent (a b)
  (#_gtk_toggle_button_set_inconsistent a (if b (if (eq b 0) 0 1) 0)))
(defun gtk::toggle-button-get-inconsistent (a)
  (let ((z (#_gtk_toggle_button_get_inconsistent a)))
    (values (if (= 1 z) t nil))))
(defun gtk::check-button-get-type () (#_gtk_check_button_get_type))
(defun gtk::check-button-new () (#_gtk_check_button_new))
(defun gtk::check-button-new-with-label (a)
  (with-cstrs ((s1 a)) (#_gtk_check_button_new_with_label s1)))
(defun gtk::check-button-new-with-mnemonic (a)
  (with-cstrs ((s1 a)) (#_gtk_check_button_new_with_mnemonic s1)))
(defun gtk::item-get-type () (#_gtk_item_get_type))
(defun gtk::item-select (a) (#_gtk_item_select a))
(defun gtk::item-deselect (a) (#_gtk_item_deselect a))
(defun gtk::item-toggle (a) (#_gtk_item_toggle a))
(defun gtk::menu-item-get-type () (#_gtk_menu_item_get_type))
(defun gtk::menu-item-new () (#_gtk_menu_item_new))
(defun gtk::menu-item-new-with-label (a)
  (with-cstrs ((s1 a)) (#_gtk_menu_item_new_with_label s1)))
(defun gtk::menu-item-new-with-mnemonic (a)
  (with-cstrs ((s1 a)) (#_gtk_menu_item_new_with_mnemonic s1)))
(defun gtk::menu-item-set-submenu (a b) (#_gtk_menu_item_set_submenu a b))
(defun gtk::menu-item-get-submenu (a) (#_gtk_menu_item_get_submenu a))
(defun gtk::menu-item-remove-submenu (a) (#_gtk_menu_item_remove_submenu a))
(defun gtk::menu-item-select (a) (#_gtk_menu_item_select a))
(defun gtk::menu-item-deselect (a) (#_gtk_menu_item_deselect a))
(defun gtk::menu-item-activate (a) (#_gtk_menu_item_activate a))
(defun gtk::menu-item-toggle-size-request (a b)
  (rlet ((r1 :int b))
        (let ((z (#_gtk_menu_item_toggle_size_request a r1)))
          (values z (%get-signed-long r1)))))
(defun gtk::menu-item-toggle-size-allocate (a b)
  (#_gtk_menu_item_toggle_size_allocate a b))
(defun gtk::menu-item-set-right-justified (a b)
  (#_gtk_menu_item_set_right_justified a (if b (if (eq b 0) 0 1) 0)))
(defun gtk::menu-item-get-right-justified (a)
  (let ((z (#_gtk_menu_item_get_right_justified a)))
    (values (if (= 1 z) t nil))))
(defun gtk::menu-item-set-accel-path (a b)
  (with-cstrs ((s1 b)) (#_gtk_menu_item_set_accel_path a s1)))
(defun gtk::check-menu-item-get-type () (#_gtk_check_menu_item_get_type))
(defun gtk::check-menu-item-new () (#_gtk_check_menu_item_new))
(defun gtk::check-menu-item-new-with-label (a)
  (with-cstrs ((s1 a)) (#_gtk_check_menu_item_new_with_label s1)))
(defun gtk::check-menu-item-new-with-mnemonic (a)
  (with-cstrs ((s1 a)) (#_gtk_check_menu_item_new_with_mnemonic s1)))
(defun gtk::check-menu-item-set-active (a b)
  (#_gtk_check_menu_item_set_active a (if b (if (eq b 0) 0 1) 0)))
(defun gtk::check-menu-item-get-active (a)
  (let ((z (#_gtk_check_menu_item_get_active a))) (values (if (= 1 z) t nil))))
(defun gtk::check-menu-item-toggled (a) (#_gtk_check_menu_item_toggled a))
(defun gtk::check-menu-item-set-inconsistent (a b)
  (#_gtk_check_menu_item_set_inconsistent a (if b (if (eq b 0) 0 1) 0)))
(defun gtk::check-menu-item-get-inconsistent (a)
  (let ((z (#_gtk_check_menu_item_get_inconsistent a)))
    (values (if (= 1 z) t nil))))
(defun gtk::target-list-new (a b) (#_gtk_target_list_new a b))
(defun gtk::target-list-ref (a) (#_gtk_target_list_ref a))
(defun gtk::target-list-unref (a) (#_gtk_target_list_unref a))
(defun gtk::target-list-add (a b c d) (#_gtk_target_list_add a b c d))
(defun gtk::target-list-add-table (a b c) (#_gtk_target_list_add_table a b c))
(defun gtk::target-list-remove (a b) (#_gtk_target_list_remove a b))
(defun gtk::target-list-find (a b c)
  (let ((z (#_gtk_target_list_find a b c))) (values (if (= 1 z) t nil))))
(defun gtk::selection-owner-set (a b c)
  (let ((z (#_gtk_selection_owner_set a b c))) (values (if (= 1 z) t nil))))
(defun gtk::selection-add-target (a b c d) (#_gtk_selection_add_target a b c d))
(defun gtk::selection-add-targets (a b c d)
  (#_gtk_selection_add_targets a b c d))
(defun gtk::selection-clear-targets (a b) (#_gtk_selection_clear_targets a b))
(defun gtk::selection-convert (a b c d)
  (let ((z (#_gtk_selection_convert a b c d))) (values (if (= 1 z) t nil))))
(defun gtk::selection-data-set (a b c d e) (#_gtk_selection_data_set a b c d e))
(defun gtk::selection-data-set-text (a b c)
  (with-cstrs ((s1 b))
              (let ((z (#_gtk_selection_data_set_text a s1 c)))
                (values (if (= 1 z) t nil)))))
(defun gtk::selection-data-get-text (a)
  (let ((z (#_gtk_selection_data_get_text a))) (values (%get-cstring z))))
(defun gtk::selection-data-get-targets (a b c)
  (rlet ((r1 :int c))
        (let ((z (#_gtk_selection_data_get_targets a b r1)))
          (values (if (= 1 z) t nil) (%get-signed-long r1)))))
(defun gtk::selection-data-targets-include-text (a)
  (let ((z (#_gtk_selection_data_targets_include_text a)))
    (values (if (= 1 z) t nil))))
(defun gtk::selection-remove-all (a) (#_gtk_selection_remove_all a))
(defun gtk::selection-clear (a b)
  (let ((z (#_gtk_selection_clear a b))) (values (if (= 1 z) t nil))))
(defun gtk::selection-data-copy (a) (#_gtk_selection_data_copy a))
(defun gtk::selection-data-free (a) (#_gtk_selection_data_free a))
(defun gtk::clipboard-set-with-data (a b c d e f)
  (let ((z (#_gtk_clipboard_set_with_data a b c d e f)))
    (values (if (= 1 z) t nil))))
(defun gtk::clipboard-set-with-owner (a b c d e f)
  (let ((z (#_gtk_clipboard_set_with_owner a b c d e f)))
    (values (if (= 1 z) t nil))))
(defun gtk::clipboard-get-owner (a) (#_gtk_clipboard_get_owner a))
(defun gtk::clipboard-clear (a) (#_gtk_clipboard_clear a))
(defun gtk::clipboard-set-text (a b c)
  (with-cstrs ((s1 b)) (#_gtk_clipboard_set_text a s1 c)))
(defun gtk::clipboard-request-contents (a b c d)
  (#_gtk_clipboard_request_contents a b c d))
(defun gtk::clipboard-request-text (a b c) (#_gtk_clipboard_request_text a b c))
(defun gtk::clipboard-wait-for-contents (a b)
  (#_gtk_clipboard_wait_for_contents a b))
(defun gtk::clipboard-wait-for-text (a)
  (let ((z (#_gtk_clipboard_wait_for_text a))) (values (%get-cstring z))))
(defun gtk::clipboard-wait-is-text-available (a)
  (let ((z (#_gtk_clipboard_wait_is_text_available a)))
    (values (if (= 1 z) t nil))))
(defun gtk::range-get-type () (#_gtk_range_get_type))
(defun gtk::range-set-update-policy (a b) (#_gtk_range_set_update_policy a b))
(defun gtk::range-get-update-policy (a) (#_gtk_range_get_update_policy a))
(defun gtk::range-set-adjustment (a b) (#_gtk_range_set_adjustment a b))
(defun gtk::range-get-adjustment (a) (#_gtk_range_get_adjustment a))
(defun gtk::range-set-inverted (a b)
  (#_gtk_range_set_inverted a (if b (if (eq b 0) 0 1) 0)))
(defun gtk::range-get-inverted (a)
  (let ((z (#_gtk_range_get_inverted a))) (values (if (= 1 z) t nil))))
(defun gtk::range-set-increments (a b c)
  (#_gtk_range_set_increments a (coerce b 'double-float)
   (coerce c 'double-float)))
(defun gtk::range-set-range (a b c)
  (#_gtk_range_set_range a (coerce b 'double-float) (coerce c 'double-float)))
(defun gtk::range-set-value (a b)
  (#_gtk_range_set_value a (coerce b 'double-float)))
(defun gtk::range-get-value (a) (#_gtk_range_get_value a))
(defun gtk::scrollbar-get-type () (#_gtk_scrollbar_get_type))
(defun gtk::hscrollbar-get-type () (#_gtk_hscrollbar_get_type))
(defun gtk::hscrollbar-new (a) (#_gtk_hscrollbar_new a))
(defun gtk::vscrollbar-get-type () (#_gtk_vscrollbar_get_type))
(defun gtk::vscrollbar-new (a) (#_gtk_vscrollbar_new a))
(defun gtk::clist-get-type () (#_gtk_clist_get_type))
(defun gtk::clist-set-hadjustment (a b) (#_gtk_clist_set_hadjustment a b))
(defun gtk::clist-set-vadjustment (a b) (#_gtk_clist_set_vadjustment a b))
(defun gtk::clist-get-hadjustment (a) (#_gtk_clist_get_hadjustment a))
(defun gtk::clist-get-vadjustment (a) (#_gtk_clist_get_vadjustment a))
(defun gtk::clist-set-shadow-type (a b) (#_gtk_clist_set_shadow_type a b))
(defun gtk::clist-set-selection-mode (a b) (#_gtk_clist_set_selection_mode a b))
(defun gtk::clist-set-reorderable (a b)
  (#_gtk_clist_set_reorderable a (if b (if (eq b 0) 0 1) 0)))
(defun gtk::clist-set-use-drag-icons (a b)
  (#_gtk_clist_set_use_drag_icons a (if b (if (eq b 0) 0 1) 0)))
(defun gtk::clist-set-button-actions (a b c)
  (#_gtk_clist_set_button_actions a b c))
(defun gtk::clist-freeze (a) (#_gtk_clist_freeze a))
(defun gtk::clist-thaw (a) (#_gtk_clist_thaw a))
(defun gtk::clist-column-titles-show (a) (#_gtk_clist_column_titles_show a))
(defun gtk::clist-column-titles-hide (a) (#_gtk_clist_column_titles_hide a))
(defun gtk::clist-column-title-active (a b)
  (#_gtk_clist_column_title_active a b))
(defun gtk::clist-column-title-passive (a b)
  (#_gtk_clist_column_title_passive a b))
(defun gtk::clist-column-titles-active (a) (#_gtk_clist_column_titles_active a))
(defun gtk::clist-column-titles-passive (a)
  (#_gtk_clist_column_titles_passive a))
(defun gtk::clist-set-column-title (a b c)
  (with-cstrs ((s1 c)) (#_gtk_clist_set_column_title a b s1)))
(defun gtk::clist-get-column-title (a b)
  (let ((z (#_gtk_clist_get_column_title a b))) (values (%get-cstring z))))
(defun gtk::clist-set-column-widget (a b c)
  (#_gtk_clist_set_column_widget a b c))
(defun gtk::clist-get-column-widget (a b) (#_gtk_clist_get_column_widget a b))
(defun gtk::clist-set-column-justification (a b c)
  (#_gtk_clist_set_column_justification a b c))
(defun gtk::clist-set-column-visibility (a b c)
  (#_gtk_clist_set_column_visibility a b (if c (if (eq c 0) 0 1) 0)))
(defun gtk::clist-set-column-resizeable (a b c)
  (#_gtk_clist_set_column_resizeable a b (if c (if (eq c 0) 0 1) 0)))
(defun gtk::clist-set-column-auto-resize (a b c)
  (#_gtk_clist_set_column_auto_resize a b (if c (if (eq c 0) 0 1) 0)))
(defun gtk::clist-columns-autosize (a) (#_gtk_clist_columns_autosize a))
(defun gtk::clist-optimal-column-width (a b)
  (#_gtk_clist_optimal_column_width a b))
(defun gtk::clist-set-column-width (a b c) (#_gtk_clist_set_column_width a b c))
(defun gtk::clist-set-column-min-width (a b c)
  (#_gtk_clist_set_column_min_width a b c))
(defun gtk::clist-set-column-max-width (a b c)
  (#_gtk_clist_set_column_max_width a b c))
(defun gtk::clist-set-row-height (a b) (#_gtk_clist_set_row_height a b))
(defun gtk::clist-moveto (a b c d e)
  (#_gtk_clist_moveto a b c (coerce d 'single-float) (coerce e 'single-float)))
(defun gtk::clist-row-is-visible (a b) (#_gtk_clist_row_is_visible a b))
(defun gtk::clist-get-cell-type (a b c) (#_gtk_clist_get_cell_type a b c))
(defun gtk::clist-set-text (a b c d)
  (with-cstrs ((s1 d)) (#_gtk_clist_set_text a b c s1)))
(defun gtk::clist-get-text (a b c d) (#_gtk_clist_get_text a b c d))
(defun gtk::clist-set-pixmap (a b c d e) (#_gtk_clist_set_pixmap a b c d e))
(defun gtk::clist-get-pixmap (a b c d e) (#_gtk_clist_get_pixmap a b c d e))
(defun gtk::clist-set-pixtext (a b c d e f g)
  (with-cstrs ((s1 d)) (#_gtk_clist_set_pixtext a b c s1 e f g)))
(defun gtk::clist-get-pixtext (a b c d e f g)
  (#_gtk_clist_get_pixtext a b c d e f g))
(defun gtk::clist-set-foreground (a b c) (#_gtk_clist_set_foreground a b c))
(defun gtk::clist-set-background (a b c) (#_gtk_clist_set_background a b c))
(defun gtk::clist-set-cell-style (a b c d) (#_gtk_clist_set_cell_style a b c d))
(defun gtk::clist-get-cell-style (a b c) (#_gtk_clist_get_cell_style a b c))
(defun gtk::clist-set-row-style (a b c) (#_gtk_clist_set_row_style a b c))
(defun gtk::clist-get-row-style (a b) (#_gtk_clist_get_row_style a b))
(defun gtk::clist-set-shift (a b c d e) (#_gtk_clist_set_shift a b c d e))
(defun gtk::clist-set-selectable (a b c)
  (#_gtk_clist_set_selectable a b (if c (if (eq c 0) 0 1) 0)))
(defun gtk::clist-get-selectable (a b)
  (let ((z (#_gtk_clist_get_selectable a b))) (values (if (= 1 z) t nil))))
(defun gtk::clist-prepend (a b) (#_gtk_clist_prepend a b))
(defun gtk::clist-append (a b) (#_gtk_clist_append a b))
(defun gtk::clist-insert (a b c) (#_gtk_clist_insert a b c))
(defun gtk::clist-remove (a b) (#_gtk_clist_remove a b))
(defun gtk::clist-set-row-data (a b c) (#_gtk_clist_set_row_data a b c))
(defun gtk::clist-set-row-data-full (a b c d)
  (#_gtk_clist_set_row_data_full a b c d))
(defun gtk::clist-get-row-data (a b) (#_gtk_clist_get_row_data a b))
(defun gtk::clist-find-row-from-data (a b) (#_gtk_clist_find_row_from_data a b))
(defun gtk::clist-select-row (a b c) (#_gtk_clist_select_row a b c))
(defun gtk::clist-unselect-row (a b c) (#_gtk_clist_unselect_row a b c))
(defun gtk::clist-undo-selection (a) (#_gtk_clist_undo_selection a))
(defun gtk::clist-clear (a) (#_gtk_clist_clear a))
(defun gtk::clist-get-selection-info (a b c d e)
  (rlet ((r1 :int d) (r2 :int e))
        (let ((z (#_gtk_clist_get_selection_info a b c r1 r2)))
          (values z (%get-signed-long r1) (%get-signed-long r2)))))
(defun gtk::clist-select-all (a) (#_gtk_clist_select_all a))
(defun gtk::clist-unselect-all (a) (#_gtk_clist_unselect_all a))
(defun gtk::clist-swap-rows (a b c) (#_gtk_clist_swap_rows a b c))
(defun gtk::clist-row-move (a b c) (#_gtk_clist_row_move a b c))
(defun gtk::clist-set-compare-func (a b) (#_gtk_clist_set_compare_func a b))
(defun gtk::clist-set-sort-column (a b) (#_gtk_clist_set_sort_column a b))
(defun gtk::clist-set-sort-type (a b) (#_gtk_clist_set_sort_type a b))
(defun gtk::clist-sort (a) (#_gtk_clist_sort a))
(defun gtk::clist-set-auto-sort (a b)
  (#_gtk_clist_set_auto_sort a (if b (if (eq b 0) 0 1) 0)))
(defun gtk::dialog-get-type () (#_gtk_dialog_get_type))
(defun gtk::dialog-new () (#_gtk_dialog_new))
(defun gtk::dialog-new-with-buttons (a b c d)
  (with-cstrs ((s1 a) (s2 d)) (#_gtk_dialog_new_with_buttons s1 b c s2)))
(defun gtk::dialog-add-action-widget (a b c)
  (#_gtk_dialog_add_action_widget a b c))
(defun gtk::dialog-add-button (a b c)
  (with-cstrs ((s1 b)) (#_gtk_dialog_add_button a s1 c)))
(defun gtk::dialog-add-buttons (a b)
  (with-cstrs ((s1 b)) (#_gtk_dialog_add_buttons a s1)))
(defun gtk::dialog-set-response-sensitive (a b c)
  (#_gtk_dialog_set_response_sensitive a b (if c (if (eq c 0) 0 1) 0)))
(defun gtk::dialog-set-default-response (a b)
  (#_gtk_dialog_set_default_response a b))
(defun gtk::dialog-set-has-separator (a b)
  (#_gtk_dialog_set_has_separator a (if b (if (eq b 0) 0 1) 0)))
(defun gtk::dialog-get-has-separator (a)
  (let ((z (#_gtk_dialog_get_has_separator a))) (values (if (= 1 z) t nil))))
(defun gtk::dialog-response (a b) (#_gtk_dialog_response a b))
(defun gtk::dialog-run (a) (#_gtk_dialog_run a))
(defun gtk::vbox-get-type () (#_gtk_vbox_get_type))
(defun gtk::vbox-new (a b) (#_gtk_vbox_new (if a (if (eq a 0) 0 1) 0) b))
(defun gtk::color-selection-get-type () (#_gtk_color_selection_get_type))
(defun gtk::color-selection-new () (#_gtk_color_selection_new))
(defun gtk::color-selection-get-has-opacity-control (a)
  (let ((z (#_gtk_color_selection_get_has_opacity_control a)))
    (values (if (= 1 z) t nil))))
(defun gtk::color-selection-set-has-opacity-control (a b)
  (#_gtk_color_selection_set_has_opacity_control a (if b (if (eq b 0) 0 1) 0)))
(defun gtk::color-selection-get-has-palette (a)
  (let ((z (#_gtk_color_selection_get_has_palette a)))
    (values (if (= 1 z) t nil))))
(defun gtk::color-selection-set-has-palette (a b)
  (#_gtk_color_selection_set_has_palette a (if b (if (eq b 0) 0 1) 0)))
(defun gtk::color-selection-set-current-color (a b)
  (#_gtk_color_selection_set_current_color a b))
(defun gtk::color-selection-set-current-alpha (a b)
  (#_gtk_color_selection_set_current_alpha a b))
(defun gtk::color-selection-get-current-color (a b)
  (#_gtk_color_selection_get_current_color a b))
(defun gtk::color-selection-get-current-alpha (a)
  (#_gtk_color_selection_get_current_alpha a))
(defun gtk::color-selection-set-previous-color (a b)
  (#_gtk_color_selection_set_previous_color a b))
(defun gtk::color-selection-set-previous-alpha (a b)
  (#_gtk_color_selection_set_previous_alpha a b))
(defun gtk::color-selection-get-previous-color (a b)
  (#_gtk_color_selection_get_previous_color a b))
(defun gtk::color-selection-get-previous-alpha (a)
  (#_gtk_color_selection_get_previous_alpha a))
(defun gtk::color-selection-is-adjusting (a)
  (let ((z (#_gtk_color_selection_is_adjusting a)))
    (values (if (= 1 z) t nil))))
(defun gtk::color-selection-palette-from-string (a b c)
  (with-cstrs ((s1 a))
              (rlet ((r1 :int c))
                    (let ((z
                           (#_gtk_color_selection_palette_from_string s1 b
                            r1)))
                      (values (if (= 1 z) t nil) (%get-signed-long r1))))))
(defun gtk::color-selection-palette-to-string (a b)
  (let ((z (#_gtk_color_selection_palette_to_string a b)))
    (values (%get-cstring z))))
(defun gtk::color-selection-set-change-palette-hook (a)
  (#_gtk_color_selection_set_change_palette_hook a))
(defun gtk::color-selection-dialog-get-type ()
  (#_gtk_color_selection_dialog_get_type))
(defun gtk::color-selection-dialog-new (a)
  (with-cstrs ((s1 a)) (#_gtk_color_selection_dialog_new s1)))
(defun gtk::hbox-get-type () (#_gtk_hbox_get_type))
(defun gtk::hbox-new (a b) (#_gtk_hbox_new (if a (if (eq a 0) 0 1) 0) b))
(defun gtk::combo-get-type () (#_gtk_combo_get_type))
(defun gtk::combo-new () (#_gtk_combo_new))
(defun gtk::combo-set-value-in-list (a b c)
  (#_gtk_combo_set_value_in_list a (if b (if (eq b 0) 0 1) 0)
   (if c (if (eq c 0) 0 1) 0)))
(defun gtk::combo-set-use-arrows (a b)
  (#_gtk_combo_set_use_arrows a (if b (if (eq b 0) 0 1) 0)))
(defun gtk::combo-set-use-arrows-always (a b)
  (#_gtk_combo_set_use_arrows_always a (if b (if (eq b 0) 0 1) 0)))
(defun gtk::combo-set-case-sensitive (a b)
  (#_gtk_combo_set_case_sensitive a (if b (if (eq b 0) 0 1) 0)))
(defun gtk::combo-set-item-string (a b c)
  (with-cstrs ((s1 c)) (#_gtk_combo_set_item_string a b s1)))
(defun gtk::combo-set-popdown-strings (a b)
  (#_gtk_combo_set_popdown_strings a b))
(defun gtk::combo-disable-activate (a) (#_gtk_combo_disable_activate a))
(defun gtk::ctree-get-type () (#_gtk_ctree_get_type))
(defun gtk::ctree-insert-node (a b c d e f g h i j k)
  (#_gtk_ctree_insert_node a b c d e f g h i (if j (if (eq j 0) 0 1) 0)
   (if k (if (eq k 0) 0 1) 0)))
(defun gtk::ctree-remove-node (a b) (#_gtk_ctree_remove_node a b))
(defun gtk::ctree-insert-gnode (a b c d e f)
  (#_gtk_ctree_insert_gnode a b c d e f))
(defun gtk::ctree-export-to-gnode (a b c d e f)
  (#_gtk_ctree_export_to_gnode a b c d e f))
(defun gtk::ctree-post-recursive (a b c d) (#_gtk_ctree_post_recursive a b c d))
(defun gtk::ctree-post-recursive-to-depth (a b c d e)
  (#_gtk_ctree_post_recursive_to_depth a b c d e))
(defun gtk::ctree-pre-recursive (a b c d) (#_gtk_ctree_pre_recursive a b c d))
(defun gtk::ctree-pre-recursive-to-depth (a b c d e)
  (#_gtk_ctree_pre_recursive_to_depth a b c d e))
(defun gtk::ctree-is-viewable (a b)
  (let ((z (#_gtk_ctree_is_viewable a b))) (values (if (= 1 z) t nil))))
(defun gtk::ctree-last (a b) (#_gtk_ctree_last a b))
(defun gtk::ctree-find-node-ptr (a b) (#_gtk_ctree_find_node_ptr a b))
(defun gtk::ctree-node-nth (a b) (#_gtk_ctree_node_nth a b))
(defun gtk::ctree-find (a b c)
  (let ((z (#_gtk_ctree_find a b c))) (values (if (= 1 z) t nil))))
(defun gtk::ctree-is-ancestor (a b c)
  (let ((z (#_gtk_ctree_is_ancestor a b c))) (values (if (= 1 z) t nil))))
(defun gtk::ctree-find-by-row-data (a b c) (#_gtk_ctree_find_by_row_data a b c))
(defun gtk::ctree-find-all-by-row-data (a b c)
  (#_gtk_ctree_find_all_by_row_data a b c))
(defun gtk::ctree-find-by-row-data-custom (a b c d)
  (#_gtk_ctree_find_by_row_data_custom a b c d))
(defun gtk::ctree-find-all-by-row-data-custom (a b c d)
  (#_gtk_ctree_find_all_by_row_data_custom a b c d))
(defun gtk::ctree-is-hot-spot (a b c)
  (let ((z (#_gtk_ctree_is_hot_spot a b c))) (values (if (= 1 z) t nil))))
(defun gtk::ctree-move (a b c d) (#_gtk_ctree_move a b c d))
(defun gtk::ctree-expand (a b) (#_gtk_ctree_expand a b))
(defun gtk::ctree-expand-recursive (a b) (#_gtk_ctree_expand_recursive a b))
(defun gtk::ctree-expand-to-depth (a b c) (#_gtk_ctree_expand_to_depth a b c))
(defun gtk::ctree-collapse (a b) (#_gtk_ctree_collapse a b))
(defun gtk::ctree-collapse-recursive (a b) (#_gtk_ctree_collapse_recursive a b))
(defun gtk::ctree-collapse-to-depth (a b c)
  (#_gtk_ctree_collapse_to_depth a b c))
(defun gtk::ctree-toggle-expansion (a b) (#_gtk_ctree_toggle_expansion a b))
(defun gtk::ctree-toggle-expansion-recursive (a b)
  (#_gtk_ctree_toggle_expansion_recursive a b))
(defun gtk::ctree-select (a b) (#_gtk_ctree_select a b))
(defun gtk::ctree-select-recursive (a b) (#_gtk_ctree_select_recursive a b))
(defun gtk::ctree-unselect (a b) (#_gtk_ctree_unselect a b))
(defun gtk::ctree-unselect-recursive (a b) (#_gtk_ctree_unselect_recursive a b))
(defun gtk::ctree-real-select-recursive (a b c)
  (#_gtk_ctree_real_select_recursive a b c))
(defun gtk::ctree-node-set-text (a b c d)
  (with-cstrs ((s1 d)) (#_gtk_ctree_node_set_text a b c s1)))
(defun gtk::ctree-node-set-pixmap (a b c d e)
  (#_gtk_ctree_node_set_pixmap a b c d e))
(defun gtk::ctree-node-set-pixtext (a b c d e f g)
  (with-cstrs ((s1 d)) (#_gtk_ctree_node_set_pixtext a b c s1 e f g)))
(defun gtk::ctree-set-node-info (a b c d e f g h i j)
  (with-cstrs ((s1 c))
              (#_gtk_ctree_set_node_info a b s1 d e f g h
               (if i (if (eq i 0) 0 1) 0) (if j (if (eq j 0) 0 1) 0))))
(defun gtk::ctree-node-set-shift (a b c d e)
  (#_gtk_ctree_node_set_shift a b c d e))
(defun gtk::ctree-node-set-selectable (a b c)
  (#_gtk_ctree_node_set_selectable a b (if c (if (eq c 0) 0 1) 0)))
(defun gtk::ctree-node-get-selectable (a b)
  (let ((z (#_gtk_ctree_node_get_selectable a b))) (values (if (= 1 z) t nil))))
(defun gtk::ctree-node-get-cell-type (a b c)
  (#_gtk_ctree_node_get_cell_type a b c))
(defun gtk::ctree-node-get-text (a b c d)
  (let ((z (#_gtk_ctree_node_get_text a b c d))) (values (if (= 1 z) t nil))))
(defun gtk::ctree-node-get-pixmap (a b c d e)
  (let ((z (#_gtk_ctree_node_get_pixmap a b c d e)))
    (values (if (= 1 z) t nil))))
(defun gtk::ctree-node-get-pixtext (a b c d e f g)
  (let ((z (#_gtk_ctree_node_get_pixtext a b c d e f g)))
    (values (if (= 1 z) t nil))))
(defun gtk::ctree-get-node-info (a b c d e f g h i j)
  (let ((z (#_gtk_ctree_get_node_info a b c d e f g h i j)))
    (values (if (= 1 z) t nil))))
(defun gtk::ctree-node-set-row-style (a b c)
  (#_gtk_ctree_node_set_row_style a b c))
(defun gtk::ctree-node-get-row-style (a b) (#_gtk_ctree_node_get_row_style a b))
(defun gtk::ctree-node-set-cell-style (a b c d)
  (#_gtk_ctree_node_set_cell_style a b c d))
(defun gtk::ctree-node-get-cell-style (a b c)
  (#_gtk_ctree_node_get_cell_style a b c))
(defun gtk::ctree-node-set-foreground (a b c)
  (#_gtk_ctree_node_set_foreground a b c))
(defun gtk::ctree-node-set-background (a b c)
  (#_gtk_ctree_node_set_background a b c))
(defun gtk::ctree-node-set-row-data (a b c)
  (#_gtk_ctree_node_set_row_data a b c))
(defun gtk::ctree-node-set-row-data-full (a b c d)
  (#_gtk_ctree_node_set_row_data_full a b c d))
(defun gtk::ctree-node-get-row-data (a b) (#_gtk_ctree_node_get_row_data a b))
(defun gtk::ctree-node-moveto (a b c d e)
  (#_gtk_ctree_node_moveto a b c (coerce d 'single-float)
   (coerce e 'single-float)))
(defun gtk::ctree-node-is-visible (a b) (#_gtk_ctree_node_is_visible a b))
(defun gtk::ctree-set-indent (a b) (#_gtk_ctree_set_indent a b))
(defun gtk::ctree-set-spacing (a b) (#_gtk_ctree_set_spacing a b))
(defun gtk::ctree-set-show-stub (a b)
  (#_gtk_ctree_set_show_stub a (if b (if (eq b 0) 0 1) 0)))
(defun gtk::ctree-set-line-style (a b) (#_gtk_ctree_set_line_style a b))
(defun gtk::ctree-set-expander-style (a b) (#_gtk_ctree_set_expander_style a b))
(defun gtk::ctree-set-drag-compare-func (a b)
  (#_gtk_ctree_set_drag_compare_func a b))
(defun gtk::ctree-sort-node (a b) (#_gtk_ctree_sort_node a b))
(defun gtk::ctree-sort-recursive (a b) (#_gtk_ctree_sort_recursive a b))
(defun gtk::ctree-node-get-type () (#_gtk_ctree_node_get_type))
(defun gtk::drawing-area-get-type () (#_gtk_drawing_area_get_type))
(defun gtk::drawing-area-new () (#_gtk_drawing_area_new))
(defun gtk::curve-get-type () (#_gtk_curve_get_type))
(defun gtk::curve-new () (#_gtk_curve_new))
(defun gtk::curve-reset (a) (#_gtk_curve_reset a))
(defun gtk::curve-set-gamma (a b)
  (#_gtk_curve_set_gamma a (coerce b 'single-float)))
(defun gtk::curve-set-range (a b c d e)
  (#_gtk_curve_set_range a (coerce b 'single-float) (coerce c 'single-float)
   (coerce d 'single-float) (coerce e 'single-float)))
(defun gtk::curve-get-vector (a b c)
  (rlet ((r1 :float (coerce c 'single-float)))
        (let ((z (#_gtk_curve_get_vector a b r1)))
          (values z (%get-single-float r1)))))
(defun gtk::curve-set-vector (a b c)
  (rlet ((r1 :float (coerce c 'single-float)))
        (let ((z (#_gtk_curve_set_vector a b r1)))
          (values z (%get-single-float r1)))))
(defun gtk::curve-set-curve-type (a b) (#_gtk_curve_set_curve_type a b))
(defun gtk::drag-get-data (a b c d) (#_gtk_drag_get_data a b c d))
(defun gtk::drag-finish (a b c d)
  (#_gtk_drag_finish a (if b (if (eq b 0) 0 1) 0) (if c (if (eq c 0) 0 1) 0) d))
(defun gtk::drag-get-source-widget (a) (#_gtk_drag_get_source_widget a))
(defun gtk::drag-highlight (a) (#_gtk_drag_highlight a))
(defun gtk::drag-unhighlight (a) (#_gtk_drag_unhighlight a))
(defun gtk::drag-dest-set (a b c d e) (#_gtk_drag_dest_set a b c d e))
(defun gtk::drag-dest-set-proxy (a b c d)
  (#_gtk_drag_dest_set_proxy a b c (if d (if (eq d 0) 0 1) 0)))
(defun gtk::drag-dest-unset (a) (#_gtk_drag_dest_unset a))
(defun gtk::drag-dest-find-target (a b c) (#_gtk_drag_dest_find_target a b c))
(defun gtk::drag-dest-get-target-list (a) (#_gtk_drag_dest_get_target_list a))
(defun gtk::drag-dest-set-target-list (a b)
  (#_gtk_drag_dest_set_target_list a b))
(defun gtk::drag-source-set (a b c d e) (#_gtk_drag_source_set a b c d e))
(defun gtk::drag-source-unset (a) (#_gtk_drag_source_unset a))
(defun gtk::drag-source-set-icon (a b c d) (#_gtk_drag_source_set_icon a b c d))
(defun gtk::drag-source-set-icon-pixbuf (a b)
  (#_gtk_drag_source_set_icon_pixbuf a b))
(defun gtk::drag-source-set-icon-stock (a b)
  (with-cstrs ((s1 b)) (#_gtk_drag_source_set_icon_stock a s1)))
(defun gtk::drag-begin (a b c d e) (#_gtk_drag_begin a b c d e))
(defun gtk::drag-set-icon-widget (a b c d) (#_gtk_drag_set_icon_widget a b c d))
(defun gtk::drag-set-icon-pixmap (a b c d e f)
  (#_gtk_drag_set_icon_pixmap a b c d e f))
(defun gtk::drag-set-icon-pixbuf (a b c d) (#_gtk_drag_set_icon_pixbuf a b c d))
(defun gtk::drag-set-icon-stock (a b c d)
  (with-cstrs ((s1 b)) (#_gtk_drag_set_icon_stock a s1 c d)))
(defun gtk::drag-set-icon-default (a) (#_gtk_drag_set_icon_default a))
(defun gtk::drag-check-threshold (a b c d e)
  (let ((z (#_gtk_drag_check_threshold a b c d e)))
    (values (if (= 1 z) t nil))))
(defun gtk::editable-get-type () (#_gtk_editable_get_type))
(defun gtk::editable-select-region (a b c) (#_gtk_editable_select_region a b c))
(defun gtk::editable-get-selection-bounds (a b c)
  (rlet ((r1 :int b) (r2 :int c))
        (let ((z (#_gtk_editable_get_selection_bounds a r1 r2)))
          (values (if (= 1 z) t nil)
                  (%get-signed-long r1)
                  (%get-signed-long r2)))))
(defun gtk::editable-insert-text (a b c d)
  (with-cstrs ((s1 b))
              (rlet ((r1 :int d))
                    (let ((z (#_gtk_editable_insert_text a s1 c r1)))
                      (values z (%get-signed-long r1))))))
(defun gtk::editable-delete-text (a b c) (#_gtk_editable_delete_text a b c))
(defun gtk::editable-get-chars (a b c)
  (let ((z (#_gtk_editable_get_chars a b c))) (values (%get-cstring z))))
(defun gtk::editable-cut-clipboard (a) (#_gtk_editable_cut_clipboard a))
(defun gtk::editable-copy-clipboard (a) (#_gtk_editable_copy_clipboard a))
(defun gtk::editable-paste-clipboard (a) (#_gtk_editable_paste_clipboard a))
(defun gtk::editable-delete-selection (a) (#_gtk_editable_delete_selection a))
(defun gtk::editable-set-position (a b) (#_gtk_editable_set_position a b))
(defun gtk::editable-get-position (a) (#_gtk_editable_get_position a))
(defun gtk::editable-set-editable (a b)
  (#_gtk_editable_set_editable a (if b (if (eq b 0) 0 1) 0)))
(defun gtk::editable-get-editable (a)
  (let ((z (#_gtk_editable_get_editable a))) (values (if (= 1 z) t nil))))
(defun gtk::im-context-get-type () (#_gtk_im_context_get_type))
(defun gtk::im-context-set-client-window (a b)
  (#_gtk_im_context_set_client_window a b))
(defun gtk::im-context-get-preedit-string (a b c d)
  (rlet ((r1 :int d))
        (let ((z (#_gtk_im_context_get_preedit_string a b c r1)))
          (values z (%get-signed-long r1)))))
(defun gtk::im-context-filter-keypress (a b)
  (let ((z (#_gtk_im_context_filter_keypress a b)))
    (values (if (= 1 z) t nil))))
(defun gtk::im-context-focus-in (a) (#_gtk_im_context_focus_in a))
(defun gtk::im-context-focus-out (a) (#_gtk_im_context_focus_out a))
(defun gtk::im-context-reset (a) (#_gtk_im_context_reset a))
(defun gtk::im-context-set-cursor-location (a b)
  (#_gtk_im_context_set_cursor_location a b))
(defun gtk::im-context-set-use-preedit (a b)
  (#_gtk_im_context_set_use_preedit a (if b (if (eq b 0) 0 1) 0)))
(defun gtk::im-context-set-surrounding (a b c d)
  (with-cstrs ((s1 b)) (#_gtk_im_context_set_surrounding a s1 c d)))
(defun gtk::im-context-get-surrounding (a b c)
  (rlet ((r1 :int c))
        (let ((z (#_gtk_im_context_get_surrounding a b r1)))
          (values (if (= 1 z) t nil) (%get-signed-long r1)))))
(defun gtk::im-context-delete-surrounding (a b c)
  (let ((z (#_gtk_im_context_delete_surrounding a b c)))
    (values (if (= 1 z) t nil))))
(defun gtk::entry-get-type () (#_gtk_entry_get_type))
(defun gtk::entry-new () (#_gtk_entry_new))
(defun gtk::entry-set-visibility (a b)
  (#_gtk_entry_set_visibility a (if b (if (eq b 0) 0 1) 0)))
(defun gtk::entry-get-visibility (a)
  (let ((z (#_gtk_entry_get_visibility a))) (values (if (= 1 z) t nil))))
(defun gtk::entry-set-invisible-char (a b) (#_gtk_entry_set_invisible_char a b))
(defun gtk::entry-get-invisible-char (a) (#_gtk_entry_get_invisible_char a))
(defun gtk::entry-set-has-frame (a b)
  (#_gtk_entry_set_has_frame a (if b (if (eq b 0) 0 1) 0)))
(defun gtk::entry-get-has-frame (a)
  (let ((z (#_gtk_entry_get_has_frame a))) (values (if (= 1 z) t nil))))
(defun gtk::entry-set-max-length (a b) (#_gtk_entry_set_max_length a b))
(defun gtk::entry-get-max-length (a) (#_gtk_entry_get_max_length a))
(defun gtk::entry-set-activates-default (a b)
  (#_gtk_entry_set_activates_default a (if b (if (eq b 0) 0 1) 0)))
(defun gtk::entry-get-activates-default (a)
  (let ((z (#_gtk_entry_get_activates_default a))) (values (if (= 1 z) t nil))))
(defun gtk::entry-set-width-chars (a b) (#_gtk_entry_set_width_chars a b))
(defun gtk::entry-get-width-chars (a) (#_gtk_entry_get_width_chars a))
(defun gtk::entry-set-text (a b)
  (with-cstrs ((s1 b)) (#_gtk_entry_set_text a s1)))
(defun gtk::entry-get-text (a)
  (let ((z (#_gtk_entry_get_text a))) (values (%get-cstring z))))
(defun gtk::entry-get-layout (a) (#_gtk_entry_get_layout a))
(defun gtk::entry-get-layout-offsets (a b c)
  (rlet ((r1 :int b) (r2 :int c))
        (let ((z (#_gtk_entry_get_layout_offsets a r1 r2)))
          (values z (%get-signed-long r1) (%get-signed-long r2)))))
(defun gtk::entry-new-with-max-length (a) (#_gtk_entry_new_with_max_length a))
(defun gtk::event-box-get-type () (#_gtk_event_box_get_type))
(defun gtk::event-box-new () (#_gtk_event_box_new))
(defun gtk::file-selection-get-type () (#_gtk_file_selection_get_type))
(defun gtk::file-selection-new (a)
  (with-cstrs ((s1 a)) (#_gtk_file_selection_new s1)))
(defun gtk::file-selection-set-filename (a b)
  (with-cstrs ((s1 b)) (#_gtk_file_selection_set_filename a s1)))
(defun gtk::file-selection-get-filename (a)
  (let ((z (#_gtk_file_selection_get_filename a))) (values (%get-cstring z))))
(defun gtk::file-selection-complete (a b)
  (with-cstrs ((s1 b)) (#_gtk_file_selection_complete a s1)))
(defun gtk::file-selection-show-fileop-buttons (a)
  (#_gtk_file_selection_show_fileop_buttons a))
(defun gtk::file-selection-hide-fileop-buttons (a)
  (#_gtk_file_selection_hide_fileop_buttons a))
(defun gtk::file-selection-get-selections (a)
  (#_gtk_file_selection_get_selections a))
(defun gtk::file-selection-set-select-multiple (a b)
  (#_gtk_file_selection_set_select_multiple a (if b (if (eq b 0) 0 1) 0)))
(defun gtk::file-selection-get-select-multiple (a)
  (let ((z (#_gtk_file_selection_get_select_multiple a)))
    (values (if (= 1 z) t nil))))
(defun gtk::fixed-get-type () (#_gtk_fixed_get_type))
(defun gtk::fixed-new () (#_gtk_fixed_new))
(defun gtk::fixed-put (a b c d) (#_gtk_fixed_put a b c d))
(defun gtk::fixed-move (a b c d) (#_gtk_fixed_move a b c d))
(defun gtk::fixed-set-has-window (a b)
  (#_gtk_fixed_set_has_window a (if b (if (eq b 0) 0 1) 0)))
(defun gtk::fixed-get-has-window (a)
  (let ((z (#_gtk_fixed_get_has_window a))) (values (if (= 1 z) t nil))))
(defun gtk::font-selection-get-type () (#_gtk_font_selection_get_type))
(defun gtk::font-selection-new () (#_gtk_font_selection_new))
(defun gtk::font-selection-get-font-name (a)
  (let ((z (#_gtk_font_selection_get_font_name a))) (values (%get-cstring z))))
(defun gtk::font-selection-set-font-name (a b)
  (with-cstrs ((s1 b))
              (let ((z (#_gtk_font_selection_set_font_name a s1)))
                (values (if (= 1 z) t nil)))))
(defun gtk::font-selection-get-preview-text (a)
  (let ((z (#_gtk_font_selection_get_preview_text a)))
    (values (%get-cstring z))))
(defun gtk::font-selection-set-preview-text (a b)
  (with-cstrs ((s1 b)) (#_gtk_font_selection_set_preview_text a s1)))
(defun gtk::font-selection-dialog-get-type ()
  (#_gtk_font_selection_dialog_get_type))
(defun gtk::font-selection-dialog-new (a)
  (with-cstrs ((s1 a)) (#_gtk_font_selection_dialog_new s1)))
(defun gtk::font-selection-dialog-get-font-name (a)
  (let ((z (#_gtk_font_selection_dialog_get_font_name a)))
    (values (%get-cstring z))))
(defun gtk::font-selection-dialog-set-font-name (a b)
  (with-cstrs ((s1 b))
              (let ((z (#_gtk_font_selection_dialog_set_font_name a s1)))
                (values (if (= 1 z) t nil)))))
(defun gtk::font-selection-dialog-get-preview-text (a)
  (let ((z (#_gtk_font_selection_dialog_get_preview_text a)))
    (values (%get-cstring z))))
(defun gtk::font-selection-dialog-set-preview-text (a b)
  (with-cstrs ((s1 b)) (#_gtk_font_selection_dialog_set_preview_text a s1)))
(defun gtk::gamma-curve-get-type () (#_gtk_gamma_curve_get_type))
(defun gtk::gamma-curve-new () (#_gtk_gamma_curve_new))
(defun gtk::gc-get (a b c d) (#_gtk_gc_get a b c d))
(defun gtk::gc-release (a) (#_gtk_gc_release a))
(defun gtk::handle-box-get-type () (#_gtk_handle_box_get_type))
(defun gtk::handle-box-new () (#_gtk_handle_box_new))
(defun gtk::handle-box-set-shadow-type (a b)
  (#_gtk_handle_box_set_shadow_type a b))
(defun gtk::handle-box-get-shadow-type (a) (#_gtk_handle_box_get_shadow_type a))
(defun gtk::handle-box-set-handle-position (a b)
  (#_gtk_handle_box_set_handle_position a b))
(defun gtk::handle-box-get-handle-position (a)
  (#_gtk_handle_box_get_handle_position a))
(defun gtk::handle-box-set-snap-edge (a b) (#_gtk_handle_box_set_snap_edge a b))
(defun gtk::handle-box-get-snap-edge (a) (#_gtk_handle_box_get_snap_edge a))
(defun gtk::hbutton-box-get-type () (#_gtk_hbutton_box_get_type))
(defun gtk::hbutton-box-new () (#_gtk_hbutton_box_new))
(defun gtk::paned-get-type () (#_gtk_paned_get_type))
(defun gtk::paned-add1 (a b) (#_gtk_paned_add1 a b))
(defun gtk::paned-add2 (a b) (#_gtk_paned_add2 a b))
(defun gtk::paned-pack1 (a b c d)
  (#_gtk_paned_pack1 a b (if c (if (eq c 0) 0 1) 0) (if d (if (eq d 0) 0 1) 0)))
(defun gtk::paned-pack2 (a b c d)
  (#_gtk_paned_pack2 a b (if c (if (eq c 0) 0 1) 0) (if d (if (eq d 0) 0 1) 0)))
(defun gtk::paned-get-position (a) (#_gtk_paned_get_position a))
(defun gtk::paned-set-position (a b) (#_gtk_paned_set_position a b))
(defun gtk::paned-compute-position (a b c d)
  (#_gtk_paned_compute_position a b c d))
(defun gtk::hpaned-get-type () (#_gtk_hpaned_get_type))
(defun gtk::hpaned-new () (#_gtk_hpaned_new))
(defun gtk::ruler-get-type () (#_gtk_ruler_get_type))
(defun gtk::ruler-set-metric (a b) (#_gtk_ruler_set_metric a b))
(defun gtk::ruler-set-range (a b c d e)
  (#_gtk_ruler_set_range a (coerce b 'double-float) (coerce c 'double-float)
   (coerce d 'double-float) (coerce e 'double-float)))
(defun gtk::ruler-draw-ticks (a) (#_gtk_ruler_draw_ticks a))
(defun gtk::ruler-draw-pos (a) (#_gtk_ruler_draw_pos a))
(defun gtk::ruler-get-metric (a) (#_gtk_ruler_get_metric a))
(defun gtk::ruler-get-range (a b c d e)
  (rlet ((r1 :double (coerce b 'double-float))
         (r2 :double (coerce c 'double-float))
         (r3 :double (coerce d 'double-float))
         (r4 :double (coerce e 'double-float)))
        (let ((z (#_gtk_ruler_get_range a r1 r2 r3 r4)))
          (values z
                  (%get-double-float r1)
                  (%get-double-float r2)
                  (%get-double-float r3)
                  (%get-double-float r4)))))
(defun gtk::hruler-get-type () (#_gtk_hruler_get_type))
(defun gtk::hruler-new () (#_gtk_hruler_new))
(defun gtk::scale-get-type () (#_gtk_scale_get_type))
(defun gtk::scale-set-digits (a b) (#_gtk_scale_set_digits a b))
(defun gtk::scale-get-digits (a) (#_gtk_scale_get_digits a))
(defun gtk::scale-set-draw-value (a b)
  (#_gtk_scale_set_draw_value a (if b (if (eq b 0) 0 1) 0)))
(defun gtk::scale-get-draw-value (a)
  (let ((z (#_gtk_scale_get_draw_value a))) (values (if (= 1 z) t nil))))
(defun gtk::scale-set-value-pos (a b) (#_gtk_scale_set_value_pos a b))
(defun gtk::scale-get-value-pos (a) (#_gtk_scale_get_value_pos a))
(defun gtk::hscale-get-type () (#_gtk_hscale_get_type))
(defun gtk::hscale-new (a) (#_gtk_hscale_new a))
(defun gtk::hscale-new-with-range (a b c)
  (#_gtk_hscale_new_with_range (coerce a 'double-float)
   (coerce b 'double-float) (coerce c 'double-float)))
(defun gtk::separator-get-type () (#_gtk_separator_get_type))
(defun gtk::hseparator-get-type () (#_gtk_hseparator_get_type))
(defun gtk::hseparator-new () (#_gtk_hseparator_new))
(defun gtk::icon-factory-get-type () (#_gtk_icon_factory_get_type))
(defun gtk::icon-factory-new () (#_gtk_icon_factory_new))
(defun gtk::icon-factory-add (a b c)
  (with-cstrs ((s1 b)) (#_gtk_icon_factory_add a s1 c)))
(defun gtk::icon-factory-lookup (a b)
  (with-cstrs ((s1 b)) (#_gtk_icon_factory_lookup a s1)))
(defun gtk::icon-factory-add-default (a) (#_gtk_icon_factory_add_default a))
(defun gtk::icon-factory-remove-default (a)
  (#_gtk_icon_factory_remove_default a))
(defun gtk::icon-factory-lookup-default (a)
  (with-cstrs ((s1 a)) (#_gtk_icon_factory_lookup_default s1)))
(defun gtk::icon-size-lookup (a b c)
  (rlet ((r1 :int b) (r2 :int c))
        (let ((z (#_gtk_icon_size_lookup a r1 r2)))
          (values (if (= 1 z) t nil)
                  (%get-signed-long r1)
                  (%get-signed-long r2)))))
(defun gtk::icon-size-register (a b c)
  (with-cstrs ((s1 a)) (#_gtk_icon_size_register s1 b c)))
(defun gtk::icon-size-register-alias (a b)
  (with-cstrs ((s1 a)) (#_gtk_icon_size_register_alias s1 b)))
(defun gtk::icon-size-from-name (a)
  (with-cstrs ((s1 a)) (#_gtk_icon_size_from_name s1)))
(defun gtk::icon-size-get-name (a)
  (let ((z (#_gtk_icon_size_get_name a))) (values (%get-cstring z))))
(defun gtk::icon-set-new () (#_gtk_icon_set_new))
(defun gtk::icon-set-new-from-pixbuf (a) (#_gtk_icon_set_new_from_pixbuf a))
(defun gtk::icon-set-ref (a) (#_gtk_icon_set_ref a))
(defun gtk::icon-set-unref (a) (#_gtk_icon_set_unref a))
(defun gtk::icon-set-copy (a) (#_gtk_icon_set_copy a))
(defun gtk::icon-set-render-icon (a b c d e f g)
  (with-cstrs ((s1 g)) (#_gtk_icon_set_render_icon a b c d e f s1)))
(defun gtk::icon-set-add-source (a b) (#_gtk_icon_set_add_source a b))
(defun gtk::icon-set-get-sizes (a b c)
  (rlet ((r1 :int c))
        (let ((z (#_gtk_icon_set_get_sizes a b r1)))
          (values z (%get-signed-long r1)))))
(defun gtk::icon-source-get-type () (#_gtk_icon_source_get_type))
(defun gtk::icon-source-new () (#_gtk_icon_source_new))
(defun gtk::icon-source-copy (a) (#_gtk_icon_source_copy a))
(defun gtk::icon-source-free (a) (#_gtk_icon_source_free a))
(defun gtk::icon-source-set-filename (a b)
  (with-cstrs ((s1 b)) (#_gtk_icon_source_set_filename a s1)))
(defun gtk::icon-source-set-pixbuf (a b) (#_gtk_icon_source_set_pixbuf a b))
(defun gtk::icon-source-get-filename (a)
  (let ((z (#_gtk_icon_source_get_filename a))) (values (%get-cstring z))))
(defun gtk::icon-source-get-pixbuf (a) (#_gtk_icon_source_get_pixbuf a))
(defun gtk::icon-source-set-direction-wildcarded (a b)
  (#_gtk_icon_source_set_direction_wildcarded a (if b (if (eq b 0) 0 1) 0)))
(defun gtk::icon-source-set-state-wildcarded (a b)
  (#_gtk_icon_source_set_state_wildcarded a (if b (if (eq b 0) 0 1) 0)))
(defun gtk::icon-source-set-size-wildcarded (a b)
  (#_gtk_icon_source_set_size_wildcarded a (if b (if (eq b 0) 0 1) 0)))
(defun gtk::icon-source-get-size-wildcarded (a)
  (let ((z (#_gtk_icon_source_get_size_wildcarded a)))
    (values (if (= 1 z) t nil))))
(defun gtk::icon-source-get-state-wildcarded (a)
  (let ((z (#_gtk_icon_source_get_state_wildcarded a)))
    (values (if (= 1 z) t nil))))
(defun gtk::icon-source-get-direction-wildcarded (a)
  (let ((z (#_gtk_icon_source_get_direction_wildcarded a)))
    (values (if (= 1 z) t nil))))
(defun gtk::icon-source-set-direction (a b)
  (#_gtk_icon_source_set_direction a b))
(defun gtk::icon-source-set-state (a b) (#_gtk_icon_source_set_state a b))
(defun gtk::icon-source-set-size (a b) (#_gtk_icon_source_set_size a b))
(defun gtk::icon-source-get-direction (a) (#_gtk_icon_source_get_direction a))
(defun gtk::icon-source-get-state (a) (#_gtk_icon_source_get_state a))
(defun gtk::icon-source-get-size (a) (#_gtk_icon_source_get_size a))
(defun gtk::image-get-type () (#_gtk_image_get_type))
(defun gtk::image-new () (#_gtk_image_new))
(defun gtk::image-new-from-pixmap (a b) (#_gtk_image_new_from_pixmap a b))
(defun gtk::image-new-from-image (a b) (#_gtk_image_new_from_image a b))
(defun gtk::image-new-from-file (a)
  (with-cstrs ((s1 a)) (#_gtk_image_new_from_file s1)))
(defun gtk::image-new-from-pixbuf (a) (#_gtk_image_new_from_pixbuf a))
(defun gtk::image-new-from-stock (a b)
  (with-cstrs ((s1 a)) (#_gtk_image_new_from_stock s1 b)))
(defun gtk::image-new-from-icon-set (a b) (#_gtk_image_new_from_icon_set a b))
(defun gtk::image-new-from-animation (a) (#_gtk_image_new_from_animation a))
(defun gtk::image-set-from-pixmap (a b c) (#_gtk_image_set_from_pixmap a b c))
(defun gtk::image-set-from-image (a b c) (#_gtk_image_set_from_image a b c))
(defun gtk::image-set-from-file (a b)
  (with-cstrs ((s1 b)) (#_gtk_image_set_from_file a s1)))
(defun gtk::image-set-from-pixbuf (a b) (#_gtk_image_set_from_pixbuf a b))
(defun gtk::image-set-from-stock (a b c)
  (with-cstrs ((s1 b)) (#_gtk_image_set_from_stock a s1 c)))
(defun gtk::image-set-from-icon-set (a b c)
  (#_gtk_image_set_from_icon_set a b c))
(defun gtk::image-set-from-animation (a b) (#_gtk_image_set_from_animation a b))
(defun gtk::image-get-storage-type (a) (#_gtk_image_get_storage_type a))
(defun gtk::image-get-pixmap (a b c) (#_gtk_image_get_pixmap a b c))
(defun gtk::image-get-image (a b c) (#_gtk_image_get_image a b c))
(defun gtk::image-get-pixbuf (a) (#_gtk_image_get_pixbuf a))
(defun gtk::image-get-stock (a b c)
  (rlet ((r1 :int c))
        (let ((z (#_gtk_image_get_stock a b r1)))
          (values z (%get-signed-long r1)))))
(defun gtk::image-get-icon-set (a b c)
  (rlet ((r1 :int c))
        (let ((z (#_gtk_image_get_icon_set a b r1)))
          (values z (%get-signed-long r1)))))
(defun gtk::image-get-animation (a) (#_gtk_image_get_animation a))
(defun gtk::image-menu-item-get-type () (#_gtk_image_menu_item_get_type))
(defun gtk::image-menu-item-new () (#_gtk_image_menu_item_new))
(defun gtk::image-menu-item-new-with-label (a)
  (with-cstrs ((s1 a)) (#_gtk_image_menu_item_new_with_label s1)))
(defun gtk::image-menu-item-new-with-mnemonic (a)
  (with-cstrs ((s1 a)) (#_gtk_image_menu_item_new_with_mnemonic s1)))
(defun gtk::image-menu-item-new-from-stock (a b)
  (with-cstrs ((s1 a)) (#_gtk_image_menu_item_new_from_stock s1 b)))
(defun gtk::image-menu-item-set-image (a b)
  (#_gtk_image_menu_item_set_image a b))
(defun gtk::image-menu-item-get-image (a) (#_gtk_image_menu_item_get_image a))
(defun gtk::im-context-simple-get-type () (#_gtk_im_context_simple_get_type))
(defun gtk::im-context-simple-new () (#_gtk_im_context_simple_new))
(defun gtk::im-context-simple-add-table (a b c d)
  (#_gtk_im_context_simple_add_table a b c d))
(defun gtk::im-multicontext-get-type () (#_gtk_im_multicontext_get_type))
(defun gtk::im-multicontext-new () (#_gtk_im_multicontext_new))
(defun gtk::im-multicontext-append-menuitems (a b)
  (#_gtk_im_multicontext_append_menuitems a b))
(defun gtk::input-dialog-get-type () (#_gtk_input_dialog_get_type))
(defun gtk::input-dialog-new () (#_gtk_input_dialog_new))
(defun gtk::invisible-get-type () (#_gtk_invisible_get_type))
(defun gtk::invisible-new () (#_gtk_invisible_new))
(defun gtk::item-factory-get-type () (#_gtk_item_factory_get_type))
(defun gtk::item-factory-new (a b c)
  (with-cstrs ((s1 b)) (#_gtk_item_factory_new a s1 c)))
(defun gtk::item-factory-construct (a b c d)
  (with-cstrs ((s1 c)) (#_gtk_item_factory_construct a b s1 d)))
(defun gtk::item-factory-add-foreign (a b c d e)
  (with-cstrs ((s1 b)) (#_gtk_item_factory_add_foreign a s1 c d e)))
(defun gtk::item-factory-from-widget (a) (#_gtk_item_factory_from_widget a))
(defun gtk::item-factory-path-from-widget (a)
  (let ((z (#_gtk_item_factory_path_from_widget a))) (values (%get-cstring z))))
(defun gtk::item-factory-get-item (a b)
  (with-cstrs ((s1 b)) (#_gtk_item_factory_get_item a s1)))
(defun gtk::item-factory-get-widget (a b)
  (with-cstrs ((s1 b)) (#_gtk_item_factory_get_widget a s1)))
(defun gtk::item-factory-get-widget-by-action (a b)
  (#_gtk_item_factory_get_widget_by_action a b))
(defun gtk::item-factory-get-item-by-action (a b)
  (#_gtk_item_factory_get_item_by_action a b))
(defun gtk::item-factory-create-item (a b c d)
  (#_gtk_item_factory_create_item a b c d))
(defun gtk::item-factory-create-items (a b c d)
  (#_gtk_item_factory_create_items a b c d))
(defun gtk::item-factory-delete-item (a b)
  (with-cstrs ((s1 b)) (#_gtk_item_factory_delete_item a s1)))
(defun gtk::item-factory-delete-entry (a b)
  (#_gtk_item_factory_delete_entry a b))
(defun gtk::item-factory-delete-entries (a b c)
  (#_gtk_item_factory_delete_entries a b c))
(defun gtk::item-factory-popup (a b c d e) (#_gtk_item_factory_popup a b c d e))
(defun gtk::item-factory-popup-with-data (a b c d e f g)
  (#_gtk_item_factory_popup_with_data a b c d e f g))
(defun gtk::item-factory-popup-data (a) (#_gtk_item_factory_popup_data a))
(defun gtk::item-factory-popup-data-from-widget (a)
  (#_gtk_item_factory_popup_data_from_widget a))
(defun gtk::item-factory-set-translate-func (a b c d)
  (#_gtk_item_factory_set_translate_func a b c d))
(defun gtk::layout-get-type () (#_gtk_layout_get_type))
(defun gtk::layout-new (a b) (#_gtk_layout_new a b))
(defun gtk::layout-put (a b c d) (#_gtk_layout_put a b c d))
(defun gtk::layout-move (a b c d) (#_gtk_layout_move a b c d))
(defun gtk::layout-set-size (a b c) (#_gtk_layout_set_size a b c))
(defun gtk::layout-get-size (a b c) (#_gtk_layout_get_size a b c))
(defun gtk::layout-get-hadjustment (a) (#_gtk_layout_get_hadjustment a))
(defun gtk::layout-get-vadjustment (a) (#_gtk_layout_get_vadjustment a))
(defun gtk::layout-set-hadjustment (a b) (#_gtk_layout_set_hadjustment a b))
(defun gtk::layout-set-vadjustment (a b) (#_gtk_layout_set_vadjustment a b))
(defun gtk::list-item-get-type () (#_gtk_list_item_get_type))
(defun gtk::list-item-new () (#_gtk_list_item_new))
(defun gtk::list-item-new-with-label (a)
  (with-cstrs ((s1 a)) (#_gtk_list_item_new_with_label s1)))
(defun gtk::list-item-select (a) (#_gtk_list_item_select a))
(defun gtk::list-item-deselect (a) (#_gtk_list_item_deselect a))
(defun gtk::list-insert-items (a b c) (#_gtk_list_insert_items a b c))
(defun gtk::list-append-items (a b) (#_gtk_list_append_items a b))
(defun gtk::list-prepend-items (a b) (#_gtk_list_prepend_items a b))
(defun gtk::list-remove-items (a b) (#_gtk_list_remove_items a b))
(defun gtk::list-clear-items (a b c) (#_gtk_list_clear_items a b c))
(defun gtk::list-select-item (a b) (#_gtk_list_select_item a b))
(defun gtk::list-unselect-item (a b) (#_gtk_list_unselect_item a b))
(defun gtk::list-select-child (a b) (#_gtk_list_select_child a b))
(defun gtk::list-unselect-child (a b) (#_gtk_list_unselect_child a b))
(defun gtk::list-child-position (a b) (#_gtk_list_child_position a b))
(defun gtk::tree-path-new () (#_gtk_tree_path_new))
(defun gtk::tree-path-new-from-string (a)
  (with-cstrs ((s1 a)) (#_gtk_tree_path_new_from_string s1)))
(defun gtk::tree-path-to-string (a)
  (let ((z (#_gtk_tree_path_to_string a))) (values (%get-cstring z))))
(defun gtk::tree-path-new-first () (#_gtk_tree_path_new_first))
(defun gtk::tree-path-append-index (a b) (#_gtk_tree_path_append_index a b))
(defun gtk::tree-path-prepend-index (a b) (#_gtk_tree_path_prepend_index a b))
(defun gtk::tree-path-get-depth (a) (#_gtk_tree_path_get_depth a))
(defun gtk::tree-path-get-indices (a)
  (let ((z (#_gtk_tree_path_get_indices a))) (values (%get-signed-long z))))
(defun gtk::tree-path-free (a) (#_gtk_tree_path_free a))
(defun gtk::tree-path-copy (a) (#_gtk_tree_path_copy a))
(defun gtk::tree-path-compare (a b) (#_gtk_tree_path_compare a b))
(defun gtk::tree-path-next (a) (#_gtk_tree_path_next a))
(defun gtk::tree-path-prev (a)
  (let ((z (#_gtk_tree_path_prev a))) (values (if (= 1 z) t nil))))
(defun gtk::tree-path-up (a)
  (let ((z (#_gtk_tree_path_up a))) (values (if (= 1 z) t nil))))
(defun gtk::tree-path-down (a) (#_gtk_tree_path_down a))
(defun gtk::tree-path-is-ancestor (a b)
  (let ((z (#_gtk_tree_path_is_ancestor a b))) (values (if (= 1 z) t nil))))
(defun gtk::tree-path-is-descendant (a b)
  (let ((z (#_gtk_tree_path_is_descendant a b))) (values (if (= 1 z) t nil))))
(defun gtk::tree-row-reference-new (a b) (#_gtk_tree_row_reference_new a b))
(defun gtk::tree-row-reference-new-proxy (a b c)
  (#_gtk_tree_row_reference_new_proxy a b c))
(defun gtk::tree-row-reference-get-path (a)
  (#_gtk_tree_row_reference_get_path a))
(defun gtk::tree-row-reference-valid (a)
  (let ((z (#_gtk_tree_row_reference_valid a))) (values (if (= 1 z) t nil))))
(defun gtk::tree-row-reference-free (a) (#_gtk_tree_row_reference_free a))
(defun gtk::tree-row-reference-inserted (a b)
  (#_gtk_tree_row_reference_inserted a b))
(defun gtk::tree-row-reference-deleted (a b)
  (#_gtk_tree_row_reference_deleted a b))
(defun gtk::tree-row-reference-reordered (a b c d)
  (rlet ((r1 :int d))
        (let ((z (#_gtk_tree_row_reference_reordered a b c r1)))
          (values z (%get-signed-long r1)))))
(defun gtk::tree-iter-copy (a) (#_gtk_tree_iter_copy a))
(defun gtk::tree-iter-free (a) (#_gtk_tree_iter_free a))
(defun gtk::tree-iter-get-type () (#_gtk_tree_iter_get_type))
(defun gtk::tree-model-get-type () (#_gtk_tree_model_get_type))
(defun gtk::tree-model-get-flags (a) (#_gtk_tree_model_get_flags a))
(defun gtk::tree-model-get-n-columns (a) (#_gtk_tree_model_get_n_columns a))
(defun gtk::tree-model-get-column-type (a b)
  (#_gtk_tree_model_get_column_type a b))
(defun gtk::tree-model-get-iter (a b c)
  (let ((z (#_gtk_tree_model_get_iter a b c))) (values (if (= 1 z) t nil))))
(defun gtk::tree-model-get-iter-from-string (a b c)
  (with-cstrs ((s1 c))
              (let ((z (#_gtk_tree_model_get_iter_from_string a b s1)))
                (values (if (= 1 z) t nil)))))
(defun gtk::tree-model-get-iter-first (a b)
  (let ((z (#_gtk_tree_model_get_iter_first a b))) (values (if (= 1 z) t nil))))
(defun gtk::tree-model-get-path (a b) (#_gtk_tree_model_get_path a b))
(defun gtk::tree-model-get-value (a b c d) (#_gtk_tree_model_get_value a b c d))
(defun gtk::tree-model-iter-next (a b)
  (let ((z (#_gtk_tree_model_iter_next a b))) (values (if (= 1 z) t nil))))
(defun gtk::tree-model-iter-children (a b c)
  (let ((z (#_gtk_tree_model_iter_children a b c)))
    (values (if (= 1 z) t nil))))
(defun gtk::tree-model-iter-has-child (a b)
  (let ((z (#_gtk_tree_model_iter_has_child a b))) (values (if (= 1 z) t nil))))
(defun gtk::tree-model-iter-n-children (a b)
  (#_gtk_tree_model_iter_n_children a b))
(defun gtk::tree-model-iter-nth-child (a b c d)
  (let ((z (#_gtk_tree_model_iter_nth_child a b c d)))
    (values (if (= 1 z) t nil))))
(defun gtk::tree-model-iter-parent (a b c)
  (let ((z (#_gtk_tree_model_iter_parent a b c))) (values (if (= 1 z) t nil))))
(defun gtk::tree-model-ref-node (a b) (#_gtk_tree_model_ref_node a b))
(defun gtk::tree-model-unref-node (a b) (#_gtk_tree_model_unref_node a b))
(defun gtk::tree-model-get (a b) (#_gtk_tree_model_get a b))
(defun gtk::tree-model-get-valist (a b c)
  (with-cstrs ((s1 c)) (#_gtk_tree_model_get_valist a b s1)))
(defun gtk::tree-model-foreach (a b c) (#_gtk_tree_model_foreach a b c))
(defun gtk::tree-model-row-changed (a b c) (#_gtk_tree_model_row_changed a b c))
(defun gtk::tree-model-row-inserted (a b c)
  (#_gtk_tree_model_row_inserted a b c))
(defun gtk::tree-model-row-has-child-toggled (a b c)
  (#_gtk_tree_model_row_has_child_toggled a b c))
(defun gtk::tree-model-row-deleted (a b) (#_gtk_tree_model_row_deleted a b))
(defun gtk::tree-model-rows-reordered (a b c d)
  (rlet ((r1 :int d))
        (let ((z (#_gtk_tree_model_rows_reordered a b c r1)))
          (values z (%get-signed-long r1)))))
(defun gtk::tree-sortable-get-type () (#_gtk_tree_sortable_get_type))
(defun gtk::tree-sortable-sort-column-changed (a)
  (#_gtk_tree_sortable_sort_column_changed a))
(defun gtk::tree-sortable-get-sort-column-id (a b c)
  (rlet ((r1 :int b) (r2 :int c))
        (let ((z (#_gtk_tree_sortable_get_sort_column_id a r1 r2)))
          (values (if (= 1 z) t nil)
                  (%get-signed-long r1)
                  (%get-signed-long r2)))))
(defun gtk::tree-sortable-set-sort-column-id (a b c)
  (#_gtk_tree_sortable_set_sort_column_id a b c))
(defun gtk::tree-sortable-set-sort-func (a b c d e)
  (#_gtk_tree_sortable_set_sort_func a b c d e))
(defun gtk::tree-sortable-set-default-sort-func (a b c d)
  (#_gtk_tree_sortable_set_default_sort_func a b c d))
(defun gtk::tree-sortable-has-default-sort-func (a)
  (let ((z (#_gtk_tree_sortable_has_default_sort_func a)))
    (values (if (= 1 z) t nil))))
(defun gtk::list-store-get-type () (#_gtk_list_store_get_type))
(defun gtk::list-store-new (a) (#_gtk_list_store_new a))
(defun gtk::list-store-newv (a b) (#_gtk_list_store_newv a b))
(defun gtk::list-store-set-column-types (a b c)
  (#_gtk_list_store_set_column_types a b c))
(defun gtk::list-store-set-value (a b c d) (#_gtk_list_store_set_value a b c d))
(defun gtk::list-store-set (a b) (#_gtk_list_store_set a b))
(defun gtk::list-store-set-valist (a b c)
  (with-cstrs ((s1 c)) (#_gtk_list_store_set_valist a b s1)))
(defun gtk::list-store-remove (a b)
  (let ((z (#_gtk_list_store_remove a b))) (values (if (= 1 z) t nil))))
(defun gtk::list-store-insert (a b c) (#_gtk_list_store_insert a b c))
(defun gtk::list-store-insert-before (a b c)
  (#_gtk_list_store_insert_before a b c))
(defun gtk::list-store-insert-after (a b c)
  (#_gtk_list_store_insert_after a b c))
(defun gtk::list-store-prepend (a b) (#_gtk_list_store_prepend a b))
(defun gtk::list-store-append (a b) (#_gtk_list_store_append a b))
(defun gtk::list-store-clear (a) (#_gtk_list_store_clear a))
(defun gtk::check-version (a b c)
  (let ((z (#_gtk_check_version a b c))) (values (%get-cstring z))))
(defun gtk::init (a b)
  (rlet ((r1 :int a))
        (let ((z (#_gtk_init r1 b))) (values z (%get-signed-long r1)))))
(defun gtk::init-check (a b)
  (rlet ((r1 :int a))
        (let ((z (#_gtk_init_check r1 b)))
          (values (if (= 1 z) t nil) (%get-signed-long r1)))))
(defun gtk::disable-setlocale () (#_gtk_disable_setlocale))
(defun gtk::set-locale ()
  (let ((z (#_gtk_set_locale))) (values (%get-cstring z))))
(defun gtk::get-default-language () (#_gtk_get_default_language))
(defun gtk::events-pending () (#_gtk_events_pending))
(defun gtk::main-do-event (a) (#_gtk_main_do_event a))
(defun gtk::main () (#_gtk_main))
(defun gtk::main-level () (#_gtk_main_level))
(defun gtk::main-quit () (#_gtk_main_quit))
(defun gtk::main-iteration ()
  (let ((z (#_gtk_main_iteration))) (values (if (= 1 z) t nil))))
(defun gtk::main-iteration-do (a)
  (let ((z (#_gtk_main_iteration_do (if a (if (eq a 0) 0 1) 0))))
    (values (if (= 1 z) t nil))))
(defun gtk::true () (let ((z (#_gtk_true))) (values (if (= 1 z) t nil))))
(defun gtk::false () (let ((z (#_gtk_false))) (values (if (= 1 z) t nil))))
(defun gtk::grab-add (a) (#_gtk_grab_add a))
(defun gtk::grab-get-current () (#_gtk_grab_get_current))
(defun gtk::grab-remove (a) (#_gtk_grab_remove a))
(defun gtk::init-add (a b) (#_gtk_init_add a b))
(defun gtk::quit-add-destroy (a b) (#_gtk_quit_add_destroy a b))
(defun gtk::quit-add (a b c) (#_gtk_quit_add a b c))
(defun gtk::quit-add-full (a b c d e) (#_gtk_quit_add_full a b c d e))
(defun gtk::quit-remove (a) (#_gtk_quit_remove a))
(defun gtk::quit-remove-by-data (a) (#_gtk_quit_remove_by_data a))
(defun gtk::timeout-add (a b c) (#_gtk_timeout_add a b c))
(defun gtk::timeout-add-full (a b c d e) (#_gtk_timeout_add_full a b c d e))
(defun gtk::timeout-remove (a) (#_gtk_timeout_remove a))
(defun gtk::idle-add (a b) (#_gtk_idle_add a b))
(defun gtk::idle-add-priority (a b c) (#_gtk_idle_add_priority a b c))
(defun gtk::idle-add-full (a b c d e) (#_gtk_idle_add_full a b c d e))
(defun gtk::idle-remove (a) (#_gtk_idle_remove a))
(defun gtk::idle-remove-by-data (a) (#_gtk_idle_remove_by_data a))
(defun gtk::input-add-full (a b c d e f) (#_gtk_input_add_full a b c d e f))
(defun gtk::input-remove (a) (#_gtk_input_remove a))
(defun gtk::key-snooper-install (a b) (#_gtk_key_snooper_install a b))
(defun gtk::key-snooper-remove (a) (#_gtk_key_snooper_remove a))
(defun gtk::get-current-event () (#_gtk_get_current_event))
(defun gtk::get-current-event-time () (#_gtk_get_current_event_time))
(defun gtk::get-current-event-state (a)
  (rlet ((r1 :int a))
        (let ((z (#_gtk_get_current_event_state r1)))
          (values (if (= 1 z) t nil) (%get-signed-long r1)))))
(defun gtk::get-event-widget (a) (#_gtk_get_event_widget a))
(defun gtk::propagate-event (a b) (#_gtk_propagate_event a b))
(defun gtk::menu-bar-get-type () (#_gtk_menu_bar_get_type))
(defun gtk::menu-bar-new () (#_gtk_menu_bar_new))
(defun gtk::message-dialog-get-type () (#_gtk_message_dialog_get_type))
(defun gtk::message-dialog-new (a b c d e)
  (with-cstrs ((s1 e)) (#_gtk_message_dialog_new a b c d s1)))
(defun gtk::notebook-get-type () (#_gtk_notebook_get_type))
(defun gtk::notebook-new () (#_gtk_notebook_new))
(defun gtk::notebook-append-page (a b c) (#_gtk_notebook_append_page a b c))
(defun gtk::notebook-append-page-menu (a b c d)
  (#_gtk_notebook_append_page_menu a b c d))
(defun gtk::notebook-prepend-page (a b c) (#_gtk_notebook_prepend_page a b c))
(defun gtk::notebook-prepend-page-menu (a b c d)
  (#_gtk_notebook_prepend_page_menu a b c d))
(defun gtk::notebook-insert-page (a b c d) (#_gtk_notebook_insert_page a b c d))
(defun gtk::notebook-insert-page-menu (a b c d e)
  (#_gtk_notebook_insert_page_menu a b c d e))
(defun gtk::notebook-remove-page (a b) (#_gtk_notebook_remove_page a b))
(defun gtk::notebook-get-current-page (a) (#_gtk_notebook_get_current_page a))
(defun gtk::notebook-get-nth-page (a b) (#_gtk_notebook_get_nth_page a b))
(defun gtk::notebook-page-num (a b) (#_gtk_notebook_page_num a b))
(defun gtk::notebook-set-current-page (a b)
  (#_gtk_notebook_set_current_page a b))
(defun gtk::notebook-next-page (a) (#_gtk_notebook_next_page a))
(defun gtk::notebook-prev-page (a) (#_gtk_notebook_prev_page a))
(defun gtk::notebook-set-show-border (a b)
  (#_gtk_notebook_set_show_border a (if b (if (eq b 0) 0 1) 0)))
(defun gtk::notebook-get-show-border (a)
  (let ((z (#_gtk_notebook_get_show_border a))) (values (if (= 1 z) t nil))))
(defun gtk::notebook-set-show-tabs (a b)
  (#_gtk_notebook_set_show_tabs a (if b (if (eq b 0) 0 1) 0)))
(defun gtk::notebook-get-show-tabs (a)
  (let ((z (#_gtk_notebook_get_show_tabs a))) (values (if (= 1 z) t nil))))
(defun gtk::notebook-set-tab-pos (a b) (#_gtk_notebook_set_tab_pos a b))
(defun gtk::notebook-get-tab-pos (a) (#_gtk_notebook_get_tab_pos a))
(defun gtk::notebook-set-scrollable (a b)
  (#_gtk_notebook_set_scrollable a (if b (if (eq b 0) 0 1) 0)))
(defun gtk::notebook-get-scrollable (a)
  (let ((z (#_gtk_notebook_get_scrollable a))) (values (if (= 1 z) t nil))))
(defun gtk::notebook-popup-enable (a) (#_gtk_notebook_popup_enable a))
(defun gtk::notebook-popup-disable (a) (#_gtk_notebook_popup_disable a))
(defun gtk::notebook-get-tab-label (a b) (#_gtk_notebook_get_tab_label a b))
(defun gtk::notebook-set-tab-label (a b c) (#_gtk_notebook_set_tab_label a b c))
(defun gtk::notebook-set-tab-label-text (a b c)
  (with-cstrs ((s1 c)) (#_gtk_notebook_set_tab_label_text a b s1)))
(defun gtk::notebook-get-tab-label-text (a b)
  (let ((z (#_gtk_notebook_get_tab_label_text a b))) (values (%get-cstring z))))
(defun gtk::notebook-get-menu-label (a b) (#_gtk_notebook_get_menu_label a b))
(defun gtk::notebook-set-menu-label (a b c)
  (#_gtk_notebook_set_menu_label a b c))
(defun gtk::notebook-set-menu-label-text (a b c)
  (with-cstrs ((s1 c)) (#_gtk_notebook_set_menu_label_text a b s1)))
(defun gtk::notebook-get-menu-label-text (a b)
  (let ((z (#_gtk_notebook_get_menu_label_text a b)))
    (values (%get-cstring z))))
(defun gtk::notebook-query-tab-label-packing (a b c d e)
  (rlet ((r1 :int e))
        (let ((z (#_gtk_notebook_query_tab_label_packing a b c d r1)))
          (values z (%get-signed-long r1)))))
(defun gtk::notebook-set-tab-label-packing (a b c d e)
  (#_gtk_notebook_set_tab_label_packing a b (if c (if (eq c 0) 0 1) 0)
   (if d (if (eq d 0) 0 1) 0) e))
(defun gtk::notebook-reorder-child (a b c) (#_gtk_notebook_reorder_child a b c))
(defun gtk::old-editable-get-type () (#_gtk_old_editable_get_type))
(defun gtk::old-editable-claim-selection (a b c)
  (#_gtk_old_editable_claim_selection a (if b (if (eq b 0) 0 1) 0) c))
(defun gtk::old-editable-changed (a) (#_gtk_old_editable_changed a))
(defun gtk::option-menu-get-type () (#_gtk_option_menu_get_type))
(defun gtk::option-menu-new () (#_gtk_option_menu_new))
(defun gtk::option-menu-get-menu (a) (#_gtk_option_menu_get_menu a))
(defun gtk::option-menu-set-menu (a b) (#_gtk_option_menu_set_menu a b))
(defun gtk::option-menu-remove-menu (a) (#_gtk_option_menu_remove_menu a))
(defun gtk::option-menu-get-history (a) (#_gtk_option_menu_get_history a))
(defun gtk::option-menu-set-history (a b) (#_gtk_option_menu_set_history a b))
(defun gtk::pixmap-get-type () (#_gtk_pixmap_get_type))
(defun gtk::pixmap-set (a b c) (#_gtk_pixmap_set a b c))
(defun gtk::pixmap-get (a b c) (#_gtk_pixmap_get a b c))
(defun gtk::pixmap-set-build-insensitive (a b)
  (#_gtk_pixmap_set_build_insensitive a (if b (if (eq b 0) 0 1) 0)))
(defun gtk::socket-get-type () (#_gtk_socket_get_type))
(defun gtk::socket-new () (#_gtk_socket_new))
(defun gtk::socket-add-id (a b) (#_gtk_socket_add_id a b))
(defun gtk::socket-get-id (a) (#_gtk_socket_get_id a))
(defun gtk::plug-get-type () (#_gtk_plug_get_type))
(defun gtk::plug-construct (a b) (#_gtk_plug_construct a b))
(defun gtk::plug-new (a) (#_gtk_plug_new a))
(defun gtk::plug-get-id (a) (#_gtk_plug_get_id a))
(defun gtk::preview-get-type () (#_gtk_preview_get_type))
(defun gtk::preview-size (a b c) (#_gtk_preview_size a b c))
(defun gtk::preview-put (a b c d e f g h i)
  (#_gtk_preview_put a b c d e f g h i))
(defun gtk::preview-draw-row (a b c d e)
  (with-cstrs ((s1 b)) (#_gtk_preview_draw_row a s1 c d e)))
(defun gtk::preview-set-expand (a b)
  (#_gtk_preview_set_expand a (if b (if (eq b 0) 0 1) 0)))
(defun gtk::progress-get-type () (#_gtk_progress_get_type))
(defun gtk::progress-bar-get-type () (#_gtk_progress_bar_get_type))
(defun gtk::progress-bar-new () (#_gtk_progress_bar_new))
(defun gtk::progress-bar-pulse (a) (#_gtk_progress_bar_pulse a))
(defun gtk::progress-bar-set-text (a b)
  (with-cstrs ((s1 b)) (#_gtk_progress_bar_set_text a s1)))
(defun gtk::progress-bar-set-fraction (a b)
  (#_gtk_progress_bar_set_fraction a (coerce b 'double-float)))
(defun gtk::progress-bar-set-pulse-step (a b)
  (#_gtk_progress_bar_set_pulse_step a (coerce b 'double-float)))
(defun gtk::progress-bar-set-orientation (a b)
  (#_gtk_progress_bar_set_orientation a b))
(defun gtk::progress-bar-get-text (a)
  (let ((z (#_gtk_progress_bar_get_text a))) (values (%get-cstring z))))
(defun gtk::progress-bar-get-fraction (a) (#_gtk_progress_bar_get_fraction a))
(defun gtk::progress-bar-get-pulse-step (a)
  (#_gtk_progress_bar_get_pulse_step a))
(defun gtk::progress-bar-get-orientation (a)
  (#_gtk_progress_bar_get_orientation a))
(defun gtk::radio-button-get-type () (#_gtk_radio_button_get_type))
(defun gtk::radio-button-new (a) (#_gtk_radio_button_new a))
(defun gtk::radio-button-new-from-widget (a)
  (#_gtk_radio_button_new_from_widget a))
(defun gtk::radio-button-new-with-label (a b)
  (with-cstrs ((s1 b)) (#_gtk_radio_button_new_with_label a s1)))
(defun gtk::radio-button-new-with-label-from-widget (a b)
  (with-cstrs ((s1 b)) (#_gtk_radio_button_new_with_label_from_widget a s1)))
(defun gtk::radio-button-new-with-mnemonic (a b)
  (with-cstrs ((s1 b)) (#_gtk_radio_button_new_with_mnemonic a s1)))
(defun gtk::radio-button-new-with-mnemonic-from-widget (a b)
  (with-cstrs ((s1 b)) (#_gtk_radio_button_new_with_mnemonic_from_widget a s1)))
(defun gtk::radio-button-get-group (a) (#_gtk_radio_button_get_group a))
(defun gtk::radio-button-set-group (a b) (#_gtk_radio_button_set_group a b))
(defun gtk::radio-menu-item-get-type () (#_gtk_radio_menu_item_get_type))
(defun gtk::radio-menu-item-new (a) (#_gtk_radio_menu_item_new a))
(defun gtk::radio-menu-item-new-with-label (a b)
  (with-cstrs ((s1 b)) (#_gtk_radio_menu_item_new_with_label a s1)))
(defun gtk::radio-menu-item-new-with-mnemonic (a b)
  (with-cstrs ((s1 b)) (#_gtk_radio_menu_item_new_with_mnemonic a s1)))
(defun gtk::radio-menu-item-get-group (a) (#_gtk_radio_menu_item_get_group a))
(defun gtk::radio-menu-item-set-group (a b)
  (#_gtk_radio_menu_item_set_group a b))
(defun gtk::viewport-get-type () (#_gtk_viewport_get_type))
(defun gtk::viewport-new (a b) (#_gtk_viewport_new a b))
(defun gtk::viewport-get-hadjustment (a) (#_gtk_viewport_get_hadjustment a))
(defun gtk::viewport-get-vadjustment (a) (#_gtk_viewport_get_vadjustment a))
(defun gtk::viewport-set-hadjustment (a b) (#_gtk_viewport_set_hadjustment a b))
(defun gtk::viewport-set-vadjustment (a b) (#_gtk_viewport_set_vadjustment a b))
(defun gtk::viewport-set-shadow-type (a b) (#_gtk_viewport_set_shadow_type a b))
(defun gtk::viewport-get-shadow-type (a) (#_gtk_viewport_get_shadow_type a))
(defun gtk::scrolled-window-get-type () (#_gtk_scrolled_window_get_type))
(defun gtk::scrolled-window-new (a b) (#_gtk_scrolled_window_new a b))
(defun gtk::scrolled-window-set-hadjustment (a b)
  (#_gtk_scrolled_window_set_hadjustment a b))
(defun gtk::scrolled-window-set-vadjustment (a b)
  (#_gtk_scrolled_window_set_vadjustment a b))
(defun gtk::scrolled-window-get-hadjustment (a)
  (#_gtk_scrolled_window_get_hadjustment a))
(defun gtk::scrolled-window-get-vadjustment (a)
  (#_gtk_scrolled_window_get_vadjustment a))
(defun gtk::scrolled-window-set-policy (a b c)
  (#_gtk_scrolled_window_set_policy a b c))
(defun gtk::scrolled-window-get-policy (a b c)
  (rlet ((r1 :int b) (r2 :int c))
        (let ((z (#_gtk_scrolled_window_get_policy a r1 r2)))
          (values z (%get-signed-long r1) (%get-signed-long r2)))))
(defun gtk::scrolled-window-set-placement (a b)
  (#_gtk_scrolled_window_set_placement a b))
(defun gtk::scrolled-window-get-placement (a)
  (#_gtk_scrolled_window_get_placement a))
(defun gtk::scrolled-window-set-shadow-type (a b)
  (#_gtk_scrolled_window_set_shadow_type a b))
(defun gtk::scrolled-window-get-shadow-type (a)
  (#_gtk_scrolled_window_get_shadow_type a))
(defun gtk::scrolled-window-add-with-viewport (a b)
  (#_gtk_scrolled_window_add_with_viewport a b))
(defun gtk::separator-menu-item-get-type ()
  (#_gtk_separator_menu_item_get_type))
(defun gtk::separator-menu-item-new () (#_gtk_separator_menu_item_new))
(defun gtk::size-group-get-type () (#_gtk_size_group_get_type))
(defun gtk::size-group-new (a) (#_gtk_size_group_new a))
(defun gtk::size-group-set-mode (a b) (#_gtk_size_group_set_mode a b))
(defun gtk::size-group-get-mode (a) (#_gtk_size_group_get_mode a))
(defun gtk::size-group-add-widget (a b) (#_gtk_size_group_add_widget a b))
(defun gtk::size-group-remove-widget (a b) (#_gtk_size_group_remove_widget a b))
(defun gtk::spin-button-get-type () (#_gtk_spin_button_get_type))
(defun gtk::spin-button-configure (a b c d)
  (#_gtk_spin_button_configure a b (coerce c 'double-float) d))
(defun gtk::spin-button-new (a b c)
  (#_gtk_spin_button_new a (coerce b 'double-float) c))
(defun gtk::spin-button-new-with-range (a b c)
  (#_gtk_spin_button_new_with_range (coerce a 'double-float)
   (coerce b 'double-float) (coerce c 'double-float)))
(defun gtk::spin-button-set-adjustment (a b)
  (#_gtk_spin_button_set_adjustment a b))
(defun gtk::spin-button-get-adjustment (a) (#_gtk_spin_button_get_adjustment a))
(defun gtk::spin-button-set-digits (a b) (#_gtk_spin_button_set_digits a b))
(defun gtk::spin-button-get-digits (a) (#_gtk_spin_button_get_digits a))
(defun gtk::spin-button-set-increments (a b c)
  (#_gtk_spin_button_set_increments a (coerce b 'double-float)
   (coerce c 'double-float)))
(defun gtk::spin-button-get-increments (a b c)
  (rlet ((r1 :double (coerce b 'double-float))
         (r2 :double (coerce c 'double-float)))
        (let ((z (#_gtk_spin_button_get_increments a r1 r2)))
          (values z (%get-double-float r1) (%get-double-float r2)))))
(defun gtk::spin-button-set-range (a b c)
  (#_gtk_spin_button_set_range a (coerce b 'double-float)
   (coerce c 'double-float)))
(defun gtk::spin-button-get-range (a b c)
  (rlet ((r1 :double (coerce b 'double-float))
         (r2 :double (coerce c 'double-float)))
        (let ((z (#_gtk_spin_button_get_range a r1 r2)))
          (values z (%get-double-float r1) (%get-double-float r2)))))
(defun gtk::spin-button-get-value (a) (#_gtk_spin_button_get_value a))
(defun gtk::spin-button-get-value-as-int (a)
  (#_gtk_spin_button_get_value_as_int a))
(defun gtk::spin-button-set-value (a b)
  (#_gtk_spin_button_set_value a (coerce b 'double-float)))
(defun gtk::spin-button-set-update-policy (a b)
  (#_gtk_spin_button_set_update_policy a b))
(defun gtk::spin-button-get-update-policy (a)
  (#_gtk_spin_button_get_update_policy a))
(defun gtk::spin-button-set-numeric (a b)
  (#_gtk_spin_button_set_numeric a (if b (if (eq b 0) 0 1) 0)))
(defun gtk::spin-button-get-numeric (a)
  (let ((z (#_gtk_spin_button_get_numeric a))) (values (if (= 1 z) t nil))))
(defun gtk::spin-button-spin (a b c)
  (#_gtk_spin_button_spin a b (coerce c 'double-float)))
(defun gtk::spin-button-set-wrap (a b)
  (#_gtk_spin_button_set_wrap a (if b (if (eq b 0) 0 1) 0)))
(defun gtk::spin-button-get-wrap (a)
  (let ((z (#_gtk_spin_button_get_wrap a))) (values (if (= 1 z) t nil))))
(defun gtk::spin-button-set-snap-to-ticks (a b)
  (#_gtk_spin_button_set_snap_to_ticks a (if b (if (eq b 0) 0 1) 0)))
(defun gtk::spin-button-get-snap-to-ticks (a)
  (let ((z (#_gtk_spin_button_get_snap_to_ticks a)))
    (values (if (= 1 z) t nil))))
(defun gtk::spin-button-update (a) (#_gtk_spin_button_update a))
(defun gtk::stock-add (a b) (#_gtk_stock_add a b))
(defun gtk::stock-add-static (a b) (#_gtk_stock_add_static a b))
(defun gtk::stock-lookup (a b)
  (with-cstrs ((s1 a))
              (let ((z (#_gtk_stock_lookup s1 b)))
                (values (if (= 1 z) t nil)))))
(defun gtk::stock-list-ids () (#_gtk_stock_list_ids))
(defun gtk::stock-item-copy (a) (#_gtk_stock_item_copy a))
(defun gtk::stock-item-free (a) (#_gtk_stock_item_free a))
(defun gtk::statusbar-get-type () (#_gtk_statusbar_get_type))
(defun gtk::statusbar-new () (#_gtk_statusbar_new))
(defun gtk::statusbar-get-context-id (a b)
  (with-cstrs ((s1 b)) (#_gtk_statusbar_get_context_id a s1)))
(defun gtk::statusbar-push (a b c)
  (with-cstrs ((s1 c)) (#_gtk_statusbar_push a b s1)))
(defun gtk::statusbar-pop (a b) (#_gtk_statusbar_pop a b))
(defun gtk::statusbar-remove (a b c) (#_gtk_statusbar_remove a b c))
(defun gtk::statusbar-set-has-resize-grip (a b)
  (#_gtk_statusbar_set_has_resize_grip a (if b (if (eq b 0) 0 1) 0)))
(defun gtk::statusbar-get-has-resize-grip (a)
  (let ((z (#_gtk_statusbar_get_has_resize_grip a)))
    (values (if (= 1 z) t nil))))
(defun gtk::table-get-type () (#_gtk_table_get_type))
(defun gtk::table-new (a b c) (#_gtk_table_new a b (if c (if (eq c 0) 0 1) 0)))
(defun gtk::table-resize (a b c) (#_gtk_table_resize a b c))
(defun gtk::table-attach (a b c d e f g h i j)
  (#_gtk_table_attach a b c d e f g h i j))
(defun gtk::table-attach-defaults (a b c d e f)
  (#_gtk_table_attach_defaults a b c d e f))
(defun gtk::table-set-row-spacing (a b c) (#_gtk_table_set_row_spacing a b c))
(defun gtk::table-get-row-spacing (a b) (#_gtk_table_get_row_spacing a b))
(defun gtk::table-set-col-spacing (a b c) (#_gtk_table_set_col_spacing a b c))
(defun gtk::table-get-col-spacing (a b) (#_gtk_table_get_col_spacing a b))
(defun gtk::table-set-row-spacings (a b) (#_gtk_table_set_row_spacings a b))
(defun gtk::table-get-default-row-spacing (a)
  (#_gtk_table_get_default_row_spacing a))
(defun gtk::table-set-col-spacings (a b) (#_gtk_table_set_col_spacings a b))
(defun gtk::table-get-default-col-spacing (a)
  (#_gtk_table_get_default_col_spacing a))
(defun gtk::table-set-homogeneous (a b)
  (#_gtk_table_set_homogeneous a (if b (if (eq b 0) 0 1) 0)))
(defun gtk::table-get-homogeneous (a)
  (let ((z (#_gtk_table_get_homogeneous a))) (values (if (= 1 z) t nil))))
(defun gtk::tearoff-menu-item-get-type () (#_gtk_tearoff_menu_item_get_type))
(defun gtk::tearoff-menu-item-new () (#_gtk_tearoff_menu_item_new))
(defun gtk::text-tag-get-type () (#_gtk_text_tag_get_type))
(defun gtk::text-tag-new (a) (with-cstrs ((s1 a)) (#_gtk_text_tag_new s1)))
(defun gtk::text-tag-get-priority (a) (#_gtk_text_tag_get_priority a))
(defun gtk::text-tag-set-priority (a b) (#_gtk_text_tag_set_priority a b))
(defun gtk::text-tag-event (a b c d)
  (let ((z (#_gtk_text_tag_event a b c d))) (values (if (= 1 z) t nil))))
(defun gtk::text-attributes-new () (#_gtk_text_attributes_new))
(defun gtk::text-attributes-copy (a) (#_gtk_text_attributes_copy a))
(defun gtk::text-attributes-copy-values (a b)
  (#_gtk_text_attributes_copy_values a b))
(defun gtk::text-attributes-unref (a) (#_gtk_text_attributes_unref a))
(defun gtk::text-attributes-ref (a) (#_gtk_text_attributes_ref a))
(defun gtk::text-attributes-get-type () (#_gtk_text_attributes_get_type))
(defun gtk::text-tag-table-get-type () (#_gtk_text_tag_table_get_type))
(defun gtk::text-tag-table-new () (#_gtk_text_tag_table_new))
(defun gtk::text-tag-table-add (a b) (#_gtk_text_tag_table_add a b))
(defun gtk::text-tag-table-remove (a b) (#_gtk_text_tag_table_remove a b))
(defun gtk::text-tag-table-lookup (a b)
  (with-cstrs ((s1 b)) (#_gtk_text_tag_table_lookup a s1)))
(defun gtk::text-tag-table-foreach (a b c) (#_gtk_text_tag_table_foreach a b c))
(defun gtk::text-tag-table-get-size (a) (#_gtk_text_tag_table_get_size a))
(defun gtk::text-child-anchor-get-type () (#_gtk_text_child_anchor_get_type))
(defun gtk::text-child-anchor-new () (#_gtk_text_child_anchor_new))
(defun gtk::text-child-anchor-get-widgets (a)
  (#_gtk_text_child_anchor_get_widgets a))
(defun gtk::text-child-anchor-get-deleted (a)
  (let ((z (#_gtk_text_child_anchor_get_deleted a)))
    (values (if (= 1 z) t nil))))
(defun gtk::text-iter-get-buffer (a) (#_gtk_text_iter_get_buffer a))
(defun gtk::text-iter-copy (a) (#_gtk_text_iter_copy a))
(defun gtk::text-iter-free (a) (#_gtk_text_iter_free a))
(defun gtk::text-iter-get-type () (#_gtk_text_iter_get_type))
(defun gtk::text-iter-get-offset (a) (#_gtk_text_iter_get_offset a))
(defun gtk::text-iter-get-line (a) (#_gtk_text_iter_get_line a))
(defun gtk::text-iter-get-line-offset (a) (#_gtk_text_iter_get_line_offset a))
(defun gtk::text-iter-get-line-index (a) (#_gtk_text_iter_get_line_index a))
(defun gtk::text-iter-get-visible-line-offset (a)
  (#_gtk_text_iter_get_visible_line_offset a))
(defun gtk::text-iter-get-visible-line-index (a)
  (#_gtk_text_iter_get_visible_line_index a))
(defun gtk::text-iter-get-char (a) (#_gtk_text_iter_get_char a))
(defun gtk::text-iter-get-slice (a b)
  (let ((z (#_gtk_text_iter_get_slice a b))) (values (%get-cstring z))))
(defun gtk::text-iter-get-text (a b)
  (let ((z (#_gtk_text_iter_get_text a b))) (values (%get-cstring z))))
(defun gtk::text-iter-get-visible-slice (a b)
  (let ((z (#_gtk_text_iter_get_visible_slice a b))) (values (%get-cstring z))))
(defun gtk::text-iter-get-visible-text (a b)
  (let ((z (#_gtk_text_iter_get_visible_text a b))) (values (%get-cstring z))))
(defun gtk::text-iter-get-pixbuf (a) (#_gtk_text_iter_get_pixbuf a))
(defun gtk::text-iter-get-marks (a) (#_gtk_text_iter_get_marks a))
(defun gtk::text-iter-get-child-anchor (a) (#_gtk_text_iter_get_child_anchor a))
(defun gtk::text-iter-get-toggled-tags (a b)
  (#_gtk_text_iter_get_toggled_tags a (if b (if (eq b 0) 0 1) 0)))
(defun gtk::text-iter-begins-tag (a b)
  (let ((z (#_gtk_text_iter_begins_tag a b))) (values (if (= 1 z) t nil))))
(defun gtk::text-iter-ends-tag (a b)
  (let ((z (#_gtk_text_iter_ends_tag a b))) (values (if (= 1 z) t nil))))
(defun gtk::text-iter-toggles-tag (a b)
  (let ((z (#_gtk_text_iter_toggles_tag a b))) (values (if (= 1 z) t nil))))
(defun gtk::text-iter-has-tag (a b)
  (let ((z (#_gtk_text_iter_has_tag a b))) (values (if (= 1 z) t nil))))
(defun gtk::text-iter-get-tags (a) (#_gtk_text_iter_get_tags a))
(defun gtk::text-iter-editable (a b)
  (let ((z (#_gtk_text_iter_editable a (if b (if (eq b 0) 0 1) 0))))
    (values (if (= 1 z) t nil))))
(defun gtk::text-iter-can-insert (a b)
  (let ((z (#_gtk_text_iter_can_insert a (if b (if (eq b 0) 0 1) 0))))
    (values (if (= 1 z) t nil))))
(defun gtk::text-iter-starts-word (a)
  (let ((z (#_gtk_text_iter_starts_word a))) (values (if (= 1 z) t nil))))
(defun gtk::text-iter-ends-word (a)
  (let ((z (#_gtk_text_iter_ends_word a))) (values (if (= 1 z) t nil))))
(defun gtk::text-iter-inside-word (a)
  (let ((z (#_gtk_text_iter_inside_word a))) (values (if (= 1 z) t nil))))
(defun gtk::text-iter-starts-sentence (a)
  (let ((z (#_gtk_text_iter_starts_sentence a))) (values (if (= 1 z) t nil))))
(defun gtk::text-iter-ends-sentence (a)
  (let ((z (#_gtk_text_iter_ends_sentence a))) (values (if (= 1 z) t nil))))
(defun gtk::text-iter-inside-sentence (a)
  (let ((z (#_gtk_text_iter_inside_sentence a))) (values (if (= 1 z) t nil))))
(defun gtk::text-iter-starts-line (a)
  (let ((z (#_gtk_text_iter_starts_line a))) (values (if (= 1 z) t nil))))
(defun gtk::text-iter-ends-line (a)
  (let ((z (#_gtk_text_iter_ends_line a))) (values (if (= 1 z) t nil))))
(defun gtk::text-iter-is-cursor-position (a)
  (let ((z (#_gtk_text_iter_is_cursor_position a)))
    (values (if (= 1 z) t nil))))
(defun gtk::text-iter-get-chars-in-line (a)
  (#_gtk_text_iter_get_chars_in_line a))
(defun gtk::text-iter-get-bytes-in-line (a)
  (#_gtk_text_iter_get_bytes_in_line a))
(defun gtk::text-iter-get-attributes (a b)
  (let ((z (#_gtk_text_iter_get_attributes a b))) (values (if (= 1 z) t nil))))
(defun gtk::text-iter-get-language (a) (#_gtk_text_iter_get_language a))
(defun gtk::text-iter-is-end (a)
  (let ((z (#_gtk_text_iter_is_end a))) (values (if (= 1 z) t nil))))
(defun gtk::text-iter-is-start (a)
  (let ((z (#_gtk_text_iter_is_start a))) (values (if (= 1 z) t nil))))
(defun gtk::text-iter-forward-char (a)
  (let ((z (#_gtk_text_iter_forward_char a))) (values (if (= 1 z) t nil))))
(defun gtk::text-iter-backward-char (a)
  (let ((z (#_gtk_text_iter_backward_char a))) (values (if (= 1 z) t nil))))
(defun gtk::text-iter-forward-chars (a b)
  (let ((z (#_gtk_text_iter_forward_chars a b))) (values (if (= 1 z) t nil))))
(defun gtk::text-iter-backward-chars (a b)
  (let ((z (#_gtk_text_iter_backward_chars a b))) (values (if (= 1 z) t nil))))
(defun gtk::text-iter-forward-line (a)
  (let ((z (#_gtk_text_iter_forward_line a))) (values (if (= 1 z) t nil))))
(defun gtk::text-iter-backward-line (a)
  (let ((z (#_gtk_text_iter_backward_line a))) (values (if (= 1 z) t nil))))
(defun gtk::text-iter-forward-lines (a b)
  (let ((z (#_gtk_text_iter_forward_lines a b))) (values (if (= 1 z) t nil))))
(defun gtk::text-iter-backward-lines (a b)
  (let ((z (#_gtk_text_iter_backward_lines a b))) (values (if (= 1 z) t nil))))
(defun gtk::text-iter-forward-word-end (a)
  (let ((z (#_gtk_text_iter_forward_word_end a))) (values (if (= 1 z) t nil))))
(defun gtk::text-iter-backward-word-start (a)
  (let ((z (#_gtk_text_iter_backward_word_start a)))
    (values (if (= 1 z) t nil))))
(defun gtk::text-iter-forward-word-ends (a b)
  (let ((z (#_gtk_text_iter_forward_word_ends a b)))
    (values (if (= 1 z) t nil))))
(defun gtk::text-iter-backward-word-starts (a b)
  (let ((z (#_gtk_text_iter_backward_word_starts a b)))
    (values (if (= 1 z) t nil))))
(defun gtk::text-iter-forward-sentence-end (a)
  (let ((z (#_gtk_text_iter_forward_sentence_end a)))
    (values (if (= 1 z) t nil))))
(defun gtk::text-iter-backward-sentence-start (a)
  (let ((z (#_gtk_text_iter_backward_sentence_start a)))
    (values (if (= 1 z) t nil))))
(defun gtk::text-iter-forward-sentence-ends (a b)
  (let ((z (#_gtk_text_iter_forward_sentence_ends a b)))
    (values (if (= 1 z) t nil))))
(defun gtk::text-iter-backward-sentence-starts (a b)
  (let ((z (#_gtk_text_iter_backward_sentence_starts a b)))
    (values (if (= 1 z) t nil))))
(defun gtk::text-iter-forward-cursor-position (a)
  (let ((z (#_gtk_text_iter_forward_cursor_position a)))
    (values (if (= 1 z) t nil))))
(defun gtk::text-iter-backward-cursor-position (a)
  (let ((z (#_gtk_text_iter_backward_cursor_position a)))
    (values (if (= 1 z) t nil))))
(defun gtk::text-iter-forward-cursor-positions (a b)
  (let ((z (#_gtk_text_iter_forward_cursor_positions a b)))
    (values (if (= 1 z) t nil))))
(defun gtk::text-iter-backward-cursor-positions (a b)
  (let ((z (#_gtk_text_iter_backward_cursor_positions a b)))
    (values (if (= 1 z) t nil))))
(defun gtk::text-iter-set-offset (a b) (#_gtk_text_iter_set_offset a b))
(defun gtk::text-iter-set-line (a b) (#_gtk_text_iter_set_line a b))
(defun gtk::text-iter-set-line-offset (a b)
  (#_gtk_text_iter_set_line_offset a b))
(defun gtk::text-iter-set-line-index (a b) (#_gtk_text_iter_set_line_index a b))
(defun gtk::text-iter-forward-to-end (a) (#_gtk_text_iter_forward_to_end a))
(defun gtk::text-iter-forward-to-line-end (a)
  (let ((z (#_gtk_text_iter_forward_to_line_end a)))
    (values (if (= 1 z) t nil))))
(defun gtk::text-iter-set-visible-line-offset (a b)
  (#_gtk_text_iter_set_visible_line_offset a b))
(defun gtk::text-iter-set-visible-line-index (a b)
  (#_gtk_text_iter_set_visible_line_index a b))
(defun gtk::text-iter-forward-to-tag-toggle (a b)
  (let ((z (#_gtk_text_iter_forward_to_tag_toggle a b)))
    (values (if (= 1 z) t nil))))
(defun gtk::text-iter-backward-to-tag-toggle (a b)
  (let ((z (#_gtk_text_iter_backward_to_tag_toggle a b)))
    (values (if (= 1 z) t nil))))
(defun gtk::text-iter-forward-find-char (a b c d)
  (let ((z (#_gtk_text_iter_forward_find_char a b c d)))
    (values (if (= 1 z) t nil))))
(defun gtk::text-iter-backward-find-char (a b c d)
  (let ((z (#_gtk_text_iter_backward_find_char a b c d)))
    (values (if (= 1 z) t nil))))
(defun gtk::text-iter-forward-search (a b c d e f)
  (with-cstrs ((s1 b))
              (let ((z (#_gtk_text_iter_forward_search a s1 c d e f)))
                (values (if (= 1 z) t nil)))))
(defun gtk::text-iter-backward-search (a b c d e f)
  (with-cstrs ((s1 b))
              (let ((z (#_gtk_text_iter_backward_search a s1 c d e f)))
                (values (if (= 1 z) t nil)))))
(defun gtk::text-iter-equal (a b)
  (let ((z (#_gtk_text_iter_equal a b))) (values (if (= 1 z) t nil))))
(defun gtk::text-iter-compare (a b) (#_gtk_text_iter_compare a b))
(defun gtk::text-iter-in-range (a b c)
  (let ((z (#_gtk_text_iter_in_range a b c))) (values (if (= 1 z) t nil))))
(defun gtk::text-iter-order (a b) (#_gtk_text_iter_order a b))
(defun gtk::text-mark-get-type () (#_gtk_text_mark_get_type))
(defun gtk::text-mark-set-visible (a b)
  (#_gtk_text_mark_set_visible a (if b (if (eq b 0) 0 1) 0)))
(defun gtk::text-mark-get-visible (a)
  (let ((z (#_gtk_text_mark_get_visible a))) (values (if (= 1 z) t nil))))
(defun gtk::text-mark-get-name (a)
  (let ((z (#_gtk_text_mark_get_name a))) (values (%get-cstring z))))
(defun gtk::text-mark-get-deleted (a)
  (let ((z (#_gtk_text_mark_get_deleted a))) (values (if (= 1 z) t nil))))
(defun gtk::text-mark-get-buffer (a) (#_gtk_text_mark_get_buffer a))
(defun gtk::text-mark-get-left-gravity (a)
  (let ((z (#_gtk_text_mark_get_left_gravity a))) (values (if (= 1 z) t nil))))
(defun gtk::text-buffer-get-type () (#_gtk_text_buffer_get_type))
(defun gtk::text-buffer-new (a) (#_gtk_text_buffer_new a))
(defun gtk::text-buffer-get-line-count (a) (#_gtk_text_buffer_get_line_count a))
(defun gtk::text-buffer-get-char-count (a) (#_gtk_text_buffer_get_char_count a))
(defun gtk::text-buffer-get-tag-table (a) (#_gtk_text_buffer_get_tag_table a))
(defun gtk::text-buffer-set-text (a b c)
  (with-cstrs ((s1 b)) (#_gtk_text_buffer_set_text a s1 c)))
(defun gtk::text-buffer-insert (a b c d)
  (with-cstrs ((s1 c)) (#_gtk_text_buffer_insert a b s1 d)))
(defun gtk::text-buffer-insert-at-cursor (a b c)
  (with-cstrs ((s1 b)) (#_gtk_text_buffer_insert_at_cursor a s1 c)))
(defun gtk::text-buffer-insert-interactive (a b c d e)
  (with-cstrs ((s1 c))
              (let ((z
                     (#_gtk_text_buffer_insert_interactive a b s1 d
                      (if e (if (eq e 0) 0 1) 0))))
                (values (if (= 1 z) t nil)))))
(defun gtk::text-buffer-insert-interactive-at-cursor (a b c d)
  (with-cstrs ((s1 b))
              (let ((z
                     (#_gtk_text_buffer_insert_interactive_at_cursor a s1 c
                      (if d (if (eq d 0) 0 1) 0))))
                (values (if (= 1 z) t nil)))))
(defun gtk::text-buffer-insert-range (a b c d)
  (#_gtk_text_buffer_insert_range a b c d))
(defun gtk::text-buffer-insert-range-interactive (a b c d e)
  (let ((z
         (#_gtk_text_buffer_insert_range_interactive a b c d
          (if e (if (eq e 0) 0 1) 0))))
    (values (if (= 1 z) t nil))))
(defun gtk::text-buffer-insert-with-tags (a b c d e)
  (with-cstrs ((s1 c)) (#_gtk_text_buffer_insert_with_tags a b s1 d e)))
(defun gtk::text-buffer-insert-with-tags-by-name (a b c d e)
  (with-cstrs ((s1 c) (s2 e))
              (#_gtk_text_buffer_insert_with_tags_by_name a b s1 d s2)))
(defun gtk::text-buffer-delete (a b c) (#_gtk_text_buffer_delete a b c))
(defun gtk::text-buffer-delete-interactive (a b c d)
  (let ((z
         (#_gtk_text_buffer_delete_interactive a b c
          (if d (if (eq d 0) 0 1) 0))))
    (values (if (= 1 z) t nil))))
(defun gtk::text-buffer-get-text (a b c d)
  (let ((z (#_gtk_text_buffer_get_text a b c (if d (if (eq d 0) 0 1) 0))))
    (values (%get-cstring z))))
(defun gtk::text-buffer-get-slice (a b c d)
  (let ((z (#_gtk_text_buffer_get_slice a b c (if d (if (eq d 0) 0 1) 0))))
    (values (%get-cstring z))))
(defun gtk::text-buffer-insert-pixbuf (a b c)
  (#_gtk_text_buffer_insert_pixbuf a b c))
(defun gtk::text-buffer-insert-child-anchor (a b c)
  (#_gtk_text_buffer_insert_child_anchor a b c))
(defun gtk::text-buffer-create-child-anchor (a b)
  (#_gtk_text_buffer_create_child_anchor a b))
(defun gtk::text-buffer-create-mark (a b c d)
  (with-cstrs ((s1 b))
              (#_gtk_text_buffer_create_mark a s1 c
               (if d (if (eq d 0) 0 1) 0))))
(defun gtk::text-buffer-move-mark (a b c) (#_gtk_text_buffer_move_mark a b c))
(defun gtk::text-buffer-delete-mark (a b) (#_gtk_text_buffer_delete_mark a b))
(defun gtk::text-buffer-get-mark (a b)
  (with-cstrs ((s1 b)) (#_gtk_text_buffer_get_mark a s1)))
(defun gtk::text-buffer-move-mark-by-name (a b c)
  (with-cstrs ((s1 b)) (#_gtk_text_buffer_move_mark_by_name a s1 c)))
(defun gtk::text-buffer-delete-mark-by-name (a b)
  (with-cstrs ((s1 b)) (#_gtk_text_buffer_delete_mark_by_name a s1)))
(defun gtk::text-buffer-get-insert (a) (#_gtk_text_buffer_get_insert a))
(defun gtk::text-buffer-get-selection-bound (a)
  (#_gtk_text_buffer_get_selection_bound a))
(defun gtk::text-buffer-place-cursor (a b) (#_gtk_text_buffer_place_cursor a b))
(defun gtk::text-buffer-apply-tag (a b c d)
  (#_gtk_text_buffer_apply_tag a b c d))
(defun gtk::text-buffer-remove-tag (a b c d)
  (#_gtk_text_buffer_remove_tag a b c d))
(defun gtk::text-buffer-apply-tag-by-name (a b c d)
  (with-cstrs ((s1 b)) (#_gtk_text_buffer_apply_tag_by_name a s1 c d)))
(defun gtk::text-buffer-remove-tag-by-name (a b c d)
  (with-cstrs ((s1 b)) (#_gtk_text_buffer_remove_tag_by_name a s1 c d)))
(defun gtk::text-buffer-remove-all-tags (a b c)
  (#_gtk_text_buffer_remove_all_tags a b c))
(defun gtk::text-buffer-create-tag (a b c)
  (with-cstrs ((s1 b) (s2 c)) (#_gtk_text_buffer_create_tag a s1 s2)))
(defun gtk::text-buffer-get-iter-at-line-offset (a b c d)
  (#_gtk_text_buffer_get_iter_at_line_offset a b c d))
(defun gtk::text-buffer-get-iter-at-line-index (a b c d)
  (#_gtk_text_buffer_get_iter_at_line_index a b c d))
(defun gtk::text-buffer-get-iter-at-offset (a b c)
  (#_gtk_text_buffer_get_iter_at_offset a b c))
(defun gtk::text-buffer-get-iter-at-line (a b c)
  (#_gtk_text_buffer_get_iter_at_line a b c))
(defun gtk::text-buffer-get-start-iter (a b)
  (#_gtk_text_buffer_get_start_iter a b))
(defun gtk::text-buffer-get-end-iter (a b) (#_gtk_text_buffer_get_end_iter a b))
(defun gtk::text-buffer-get-bounds (a b c) (#_gtk_text_buffer_get_bounds a b c))
(defun gtk::text-buffer-get-iter-at-mark (a b c)
  (#_gtk_text_buffer_get_iter_at_mark a b c))
(defun gtk::text-buffer-get-iter-at-child-anchor (a b c)
  (#_gtk_text_buffer_get_iter_at_child_anchor a b c))
(defun gtk::text-buffer-get-modified (a)
  (let ((z (#_gtk_text_buffer_get_modified a))) (values (if (= 1 z) t nil))))
(defun gtk::text-buffer-set-modified (a b)
  (#_gtk_text_buffer_set_modified a (if b (if (eq b 0) 0 1) 0)))
(defun gtk::text-buffer-add-selection-clipboard (a b)
  (#_gtk_text_buffer_add_selection_clipboard a b))
(defun gtk::text-buffer-remove-selection-clipboard (a b)
  (#_gtk_text_buffer_remove_selection_clipboard a b))
(defun gtk::text-buffer-cut-clipboard (a b c)
  (#_gtk_text_buffer_cut_clipboard a b (if c (if (eq c 0) 0 1) 0)))
(defun gtk::text-buffer-copy-clipboard (a b)
  (#_gtk_text_buffer_copy_clipboard a b))
(defun gtk::text-buffer-paste-clipboard (a b c d)
  (#_gtk_text_buffer_paste_clipboard a b c (if d (if (eq d 0) 0 1) 0)))
(defun gtk::text-buffer-get-selection-bounds (a b c)
  (let ((z (#_gtk_text_buffer_get_selection_bounds a b c)))
    (values (if (= 1 z) t nil))))
(defun gtk::text-buffer-delete-selection (a b c)
  (let ((z
         (#_gtk_text_buffer_delete_selection a (if b (if (eq b 0) 0 1) 0)
          (if c (if (eq c 0) 0 1) 0))))
    (values (if (= 1 z) t nil))))
(defun gtk::text-buffer-begin-user-action (a)
  (#_gtk_text_buffer_begin_user_action a))
(defun gtk::text-buffer-end-user-action (a)
  (#_gtk_text_buffer_end_user_action a))
(defun gtk::text-view-get-type () (#_gtk_text_view_get_type))
(defun gtk::text-view-new () (#_gtk_text_view_new))
(defun gtk::text-view-new-with-buffer (a) (#_gtk_text_view_new_with_buffer a))
(defun gtk::text-view-set-buffer (a b) (#_gtk_text_view_set_buffer a b))
(defun gtk::text-view-get-buffer (a) (#_gtk_text_view_get_buffer a))
(defun gtk::text-view-scroll-to-iter (a b c d e f)
  (let ((z
         (#_gtk_text_view_scroll_to_iter a b (coerce c 'double-float)
          (if d (if (eq d 0) 0 1) 0) (coerce e 'double-float)
          (coerce f 'double-float))))
    (values (if (= 1 z) t nil))))
(defun gtk::text-view-scroll-to-mark (a b c d e f)
  (#_gtk_text_view_scroll_to_mark a b (coerce c 'double-float)
   (if d (if (eq d 0) 0 1) 0) (coerce e 'double-float)
   (coerce f 'double-float)))
(defun gtk::text-view-scroll-mark-onscreen (a b)
  (#_gtk_text_view_scroll_mark_onscreen a b))
(defun gtk::text-view-move-mark-onscreen (a b)
  (let ((z (#_gtk_text_view_move_mark_onscreen a b)))
    (values (if (= 1 z) t nil))))
(defun gtk::text-view-place-cursor-onscreen (a)
  (let ((z (#_gtk_text_view_place_cursor_onscreen a)))
    (values (if (= 1 z) t nil))))
(defun gtk::text-view-get-visible-rect (a b)
  (#_gtk_text_view_get_visible_rect a b))
(defun gtk::text-view-set-cursor-visible (a b)
  (#_gtk_text_view_set_cursor_visible a (if b (if (eq b 0) 0 1) 0)))
(defun gtk::text-view-get-cursor-visible (a)
  (let ((z (#_gtk_text_view_get_cursor_visible a)))
    (values (if (= 1 z) t nil))))
(defun gtk::text-view-get-iter-location (a b c)
  (#_gtk_text_view_get_iter_location a b c))
(defun gtk::text-view-get-iter-at-location (a b c d)
  (#_gtk_text_view_get_iter_at_location a b c d))
(defun gtk::text-view-get-line-yrange (a b c d)
  (rlet ((r1 :int c) (r2 :int d))
        (let ((z (#_gtk_text_view_get_line_yrange a b r1 r2)))
          (values z (%get-signed-long r1) (%get-signed-long r2)))))
(defun gtk::text-view-get-line-at-y (a b c d)
  (rlet ((r1 :int d))
        (let ((z (#_gtk_text_view_get_line_at_y a b c r1)))
          (values z (%get-signed-long r1)))))
(defun gtk::text-view-buffer-to-window-coords (a b c d e f)
  (rlet ((r1 :int e) (r2 :int f))
        (let ((z (#_gtk_text_view_buffer_to_window_coords a b c d r1 r2)))
          (values z (%get-signed-long r1) (%get-signed-long r2)))))
(defun gtk::text-view-window-to-buffer-coords (a b c d e f)
  (rlet ((r1 :int e) (r2 :int f))
        (let ((z (#_gtk_text_view_window_to_buffer_coords a b c d r1 r2)))
          (values z (%get-signed-long r1) (%get-signed-long r2)))))
(defun gtk::text-view-get-window (a b) (#_gtk_text_view_get_window a b))
(defun gtk::text-view-get-window-type (a b)
  (#_gtk_text_view_get_window_type a b))
(defun gtk::text-view-set-border-window-size (a b c)
  (#_gtk_text_view_set_border_window_size a b c))
(defun gtk::text-view-get-border-window-size (a b)
  (#_gtk_text_view_get_border_window_size a b))
(defun gtk::text-view-forward-display-line (a b)
  (let ((z (#_gtk_text_view_forward_display_line a b)))
    (values (if (= 1 z) t nil))))
(defun gtk::text-view-backward-display-line (a b)
  (let ((z (#_gtk_text_view_backward_display_line a b)))
    (values (if (= 1 z) t nil))))
(defun gtk::text-view-forward-display-line-end (a b)
  (let ((z (#_gtk_text_view_forward_display_line_end a b)))
    (values (if (= 1 z) t nil))))
(defun gtk::text-view-backward-display-line-start (a b)
  (let ((z (#_gtk_text_view_backward_display_line_start a b)))
    (values (if (= 1 z) t nil))))
(defun gtk::text-view-starts-display-line (a b)
  (let ((z (#_gtk_text_view_starts_display_line a b)))
    (values (if (= 1 z) t nil))))
(defun gtk::text-view-move-visually (a b c)
  (let ((z (#_gtk_text_view_move_visually a b c))) (values (if (= 1 z) t nil))))
(defun gtk::text-view-add-child-at-anchor (a b c)
  (#_gtk_text_view_add_child_at_anchor a b c))
(defun gtk::text-view-add-child-in-window (a b c d e)
  (#_gtk_text_view_add_child_in_window a b c d e))
(defun gtk::text-view-move-child (a b c d) (#_gtk_text_view_move_child a b c d))
(defun gtk::text-view-set-wrap-mode (a b) (#_gtk_text_view_set_wrap_mode a b))
(defun gtk::text-view-get-wrap-mode (a) (#_gtk_text_view_get_wrap_mode a))
(defun gtk::text-view-set-editable (a b)
  (#_gtk_text_view_set_editable a (if b (if (eq b 0) 0 1) 0)))
(defun gtk::text-view-get-editable (a)
  (let ((z (#_gtk_text_view_get_editable a))) (values (if (= 1 z) t nil))))
(defun gtk::text-view-set-pixels-above-lines (a b)
  (#_gtk_text_view_set_pixels_above_lines a b))
(defun gtk::text-view-get-pixels-above-lines (a)
  (#_gtk_text_view_get_pixels_above_lines a))
(defun gtk::text-view-set-pixels-below-lines (a b)
  (#_gtk_text_view_set_pixels_below_lines a b))
(defun gtk::text-view-get-pixels-below-lines (a)
  (#_gtk_text_view_get_pixels_below_lines a))
(defun gtk::text-view-set-pixels-inside-wrap (a b)
  (#_gtk_text_view_set_pixels_inside_wrap a b))
(defun gtk::text-view-get-pixels-inside-wrap (a)
  (#_gtk_text_view_get_pixels_inside_wrap a))
(defun gtk::text-view-set-justification (a b)
  (#_gtk_text_view_set_justification a b))
(defun gtk::text-view-get-justification (a)
  (#_gtk_text_view_get_justification a))
(defun gtk::text-view-set-left-margin (a b)
  (#_gtk_text_view_set_left_margin a b))
(defun gtk::text-view-get-left-margin (a) (#_gtk_text_view_get_left_margin a))
(defun gtk::text-view-set-right-margin (a b)
  (#_gtk_text_view_set_right_margin a b))
(defun gtk::text-view-get-right-margin (a) (#_gtk_text_view_get_right_margin a))
(defun gtk::text-view-set-indent (a b) (#_gtk_text_view_set_indent a b))
(defun gtk::text-view-get-indent (a) (#_gtk_text_view_get_indent a))
(defun gtk::text-view-set-tabs (a b) (#_gtk_text_view_set_tabs a b))
(defun gtk::text-view-get-tabs (a) (#_gtk_text_view_get_tabs a))
(defun gtk::text-view-get-default-attributes (a)
  (#_gtk_text_view_get_default_attributes a))
(defun gtk::tips-query-get-type () (#_gtk_tips_query_get_type))
(defun gtk::tooltips-get-type () (#_gtk_tooltips_get_type))
(defun gtk::tooltips-new () (#_gtk_tooltips_new))
(defun gtk::tooltips-enable (a) (#_gtk_tooltips_enable a))
(defun gtk::tooltips-disable (a) (#_gtk_tooltips_disable a))
(defun gtk::tooltips-set-tip (a b c d)
  (with-cstrs ((s1 c) (s2 d)) (#_gtk_tooltips_set_tip a b s1 s2)))
(defun gtk::tooltips-data-get (a) (#_gtk_tooltips_data_get a))
(defun gtk::tooltips-force-window (a) (#_gtk_tooltips_force_window a))
(defun gtk::toolbar-get-type () (#_gtk_toolbar_get_type))
(defun gtk::toolbar-new () (#_gtk_toolbar_new))
(defun gtk::toolbar-append-item (a b c d e f g)
  (with-cstrs ((s1 b) (s2 c) (s3 d))
              (#_gtk_toolbar_append_item a s1 s2 s3 e f g)))
(defun gtk::toolbar-prepend-item (a b c d e f g)
  (with-cstrs ((s1 b) (s2 c) (s3 d))
              (#_gtk_toolbar_prepend_item a s1 s2 s3 e f g)))
(defun gtk::toolbar-insert-item (a b c d e f g h)
  (with-cstrs ((s1 b) (s2 c) (s3 d))
              (#_gtk_toolbar_insert_item a s1 s2 s3 e f g h)))
(defun gtk::toolbar-insert-stock (a b c d e f g)
  (with-cstrs ((s1 b) (s2 c) (s3 d))
              (#_gtk_toolbar_insert_stock a s1 s2 s3 e f g)))
(defun gtk::toolbar-append-space (a) (#_gtk_toolbar_append_space a))
(defun gtk::toolbar-prepend-space (a) (#_gtk_toolbar_prepend_space a))
(defun gtk::toolbar-insert-space (a b) (#_gtk_toolbar_insert_space a b))
(defun gtk::toolbar-remove-space (a b) (#_gtk_toolbar_remove_space a b))
(defun gtk::toolbar-append-element (a b c d e f g h i)
  (with-cstrs ((s1 d) (s2 e) (s3 f))
              (#_gtk_toolbar_append_element a b c s1 s2 s3 g h i)))
(defun gtk::toolbar-prepend-element (a b c d e f g h i)
  (with-cstrs ((s1 d) (s2 e) (s3 f))
              (#_gtk_toolbar_prepend_element a b c s1 s2 s3 g h i)))
(defun gtk::toolbar-insert-element (a b c d e f g h i j)
  (with-cstrs ((s1 d) (s2 e) (s3 f))
              (#_gtk_toolbar_insert_element a b c s1 s2 s3 g h i j)))
(defun gtk::toolbar-append-widget (a b c d)
  (with-cstrs ((s1 c) (s2 d)) (#_gtk_toolbar_append_widget a b s1 s2)))
(defun gtk::toolbar-prepend-widget (a b c d)
  (with-cstrs ((s1 c) (s2 d)) (#_gtk_toolbar_prepend_widget a b s1 s2)))
(defun gtk::toolbar-insert-widget (a b c d e)
  (with-cstrs ((s1 c) (s2 d)) (#_gtk_toolbar_insert_widget a b s1 s2 e)))
(defun gtk::toolbar-set-orientation (a b) (#_gtk_toolbar_set_orientation a b))
(defun gtk::toolbar-set-style (a b) (#_gtk_toolbar_set_style a b))
(defun gtk::toolbar-set-icon-size (a b) (#_gtk_toolbar_set_icon_size a b))
(defun gtk::toolbar-set-tooltips (a b)
  (#_gtk_toolbar_set_tooltips a (if b (if (eq b 0) 0 1) 0)))
(defun gtk::toolbar-unset-style (a) (#_gtk_toolbar_unset_style a))
(defun gtk::toolbar-unset-icon-size (a) (#_gtk_toolbar_unset_icon_size a))
(defun gtk::toolbar-get-orientation (a) (#_gtk_toolbar_get_orientation a))
(defun gtk::toolbar-get-style (a) (#_gtk_toolbar_get_style a))
(defun gtk::toolbar-get-icon-size (a) (#_gtk_toolbar_get_icon_size a))
(defun gtk::toolbar-get-tooltips (a)
  (let ((z (#_gtk_toolbar_get_tooltips a))) (values (if (= 1 z) t nil))))
(defun gtk::tree-drag-source-get-type () (#_gtk_tree_drag_source_get_type))
(defun gtk::tree-drag-source-row-draggable (a b)
  (let ((z (#_gtk_tree_drag_source_row_draggable a b)))
    (values (if (= 1 z) t nil))))
(defun gtk::tree-drag-source-drag-data-delete (a b)
  (let ((z (#_gtk_tree_drag_source_drag_data_delete a b)))
    (values (if (= 1 z) t nil))))
(defun gtk::tree-drag-source-drag-data-get (a b c)
  (let ((z (#_gtk_tree_drag_source_drag_data_get a b c)))
    (values (if (= 1 z) t nil))))
(defun gtk::tree-drag-dest-get-type () (#_gtk_tree_drag_dest_get_type))
(defun gtk::tree-drag-dest-drag-data-received (a b c)
  (let ((z (#_gtk_tree_drag_dest_drag_data_received a b c)))
    (values (if (= 1 z) t nil))))
(defun gtk::tree-drag-dest-row-drop-possible (a b c)
  (let ((z (#_gtk_tree_drag_dest_row_drop_possible a b c)))
    (values (if (= 1 z) t nil))))
(defun gtk::tree-set-row-drag-data (a b c)
  (let ((z (#_gtk_tree_set_row_drag_data a b c))) (values (if (= 1 z) t nil))))
(defun gtk::tree-get-row-drag-data (a b c)
  (let ((z (#_gtk_tree_get_row_drag_data a b c))) (values (if (= 1 z) t nil))))
(defun gtk::tree-model-sort-get-type () (#_gtk_tree_model_sort_get_type))
(defun gtk::tree-model-sort-new-with-model (a)
  (#_gtk_tree_model_sort_new_with_model a))
(defun gtk::tree-model-sort-get-model (a) (#_gtk_tree_model_sort_get_model a))
(defun gtk::tree-model-sort-convert-child-path-to-path (a b)
  (#_gtk_tree_model_sort_convert_child_path_to_path a b))
(defun gtk::tree-model-sort-convert-child-iter-to-iter (a b c)
  (#_gtk_tree_model_sort_convert_child_iter_to_iter a b c))
(defun gtk::tree-model-sort-convert-path-to-child-path (a b)
  (#_gtk_tree_model_sort_convert_path_to_child_path a b))
(defun gtk::tree-model-sort-convert-iter-to-child-iter (a b c)
  (#_gtk_tree_model_sort_convert_iter_to_child_iter a b c))
(defun gtk::tree-model-sort-reset-default-sort-func (a)
  (#_gtk_tree_model_sort_reset_default_sort_func a))
(defun gtk::tree-model-sort-clear-cache (a)
  (#_gtk_tree_model_sort_clear_cache a))
(defun gtk::tree-view-column-get-type () (#_gtk_tree_view_column_get_type))
(defun gtk::tree-view-column-new () (#_gtk_tree_view_column_new))
(defun gtk::tree-view-column-new-with-attributes (a b)
  (with-cstrs ((s1 a)) (#_gtk_tree_view_column_new_with_attributes s1 b)))
(defun gtk::tree-view-column-pack-start (a b c)
  (#_gtk_tree_view_column_pack_start a b (if c (if (eq c 0) 0 1) 0)))
(defun gtk::tree-view-column-pack-end (a b c)
  (#_gtk_tree_view_column_pack_end a b (if c (if (eq c 0) 0 1) 0)))
(defun gtk::tree-view-column-clear (a) (#_gtk_tree_view_column_clear a))
(defun gtk::tree-view-column-get-cell-renderers (a)
  (#_gtk_tree_view_column_get_cell_renderers a))
(defun gtk::tree-view-column-add-attribute (a b c d)
  (with-cstrs ((s1 c)) (#_gtk_tree_view_column_add_attribute a b s1 d)))
(defun gtk::tree-view-column-set-attributes (a b)
  (#_gtk_tree_view_column_set_attributes a b))
(defun gtk::tree-view-column-set-cell-data-func (a b c d e)
  (#_gtk_tree_view_column_set_cell_data_func a b c d e))
(defun gtk::tree-view-column-clear-attributes (a b)
  (#_gtk_tree_view_column_clear_attributes a b))
(defun gtk::tree-view-column-set-spacing (a b)
  (#_gtk_tree_view_column_set_spacing a b))
(defun gtk::tree-view-column-get-spacing (a)
  (#_gtk_tree_view_column_get_spacing a))
(defun gtk::tree-view-column-set-visible (a b)
  (#_gtk_tree_view_column_set_visible a (if b (if (eq b 0) 0 1) 0)))
(defun gtk::tree-view-column-get-visible (a)
  (let ((z (#_gtk_tree_view_column_get_visible a)))
    (values (if (= 1 z) t nil))))
(defun gtk::tree-view-column-set-resizable (a b)
  (#_gtk_tree_view_column_set_resizable a (if b (if (eq b 0) 0 1) 0)))
(defun gtk::tree-view-column-get-resizable (a)
  (let ((z (#_gtk_tree_view_column_get_resizable a)))
    (values (if (= 1 z) t nil))))
(defun gtk::tree-view-column-set-sizing (a b)
  (#_gtk_tree_view_column_set_sizing a b))
(defun gtk::tree-view-column-get-sizing (a)
  (#_gtk_tree_view_column_get_sizing a))
(defun gtk::tree-view-column-get-width (a) (#_gtk_tree_view_column_get_width a))
(defun gtk::tree-view-column-get-fixed-width (a)
  (#_gtk_tree_view_column_get_fixed_width a))
(defun gtk::tree-view-column-set-fixed-width (a b)
  (#_gtk_tree_view_column_set_fixed_width a b))
(defun gtk::tree-view-column-set-min-width (a b)
  (#_gtk_tree_view_column_set_min_width a b))
(defun gtk::tree-view-column-get-min-width (a)
  (#_gtk_tree_view_column_get_min_width a))
(defun gtk::tree-view-column-set-max-width (a b)
  (#_gtk_tree_view_column_set_max_width a b))
(defun gtk::tree-view-column-get-max-width (a)
  (#_gtk_tree_view_column_get_max_width a))
(defun gtk::tree-view-column-clicked (a) (#_gtk_tree_view_column_clicked a))
(defun gtk::tree-view-column-set-title (a b)
  (with-cstrs ((s1 b)) (#_gtk_tree_view_column_set_title a s1)))
(defun gtk::tree-view-column-get-title (a)
  (let ((z (#_gtk_tree_view_column_get_title a))) (values (%get-cstring z))))
(defun gtk::tree-view-column-set-clickable (a b)
  (#_gtk_tree_view_column_set_clickable a (if b (if (eq b 0) 0 1) 0)))
(defun gtk::tree-view-column-get-clickable (a)
  (let ((z (#_gtk_tree_view_column_get_clickable a)))
    (values (if (= 1 z) t nil))))
(defun gtk::tree-view-column-set-widget (a b)
  (#_gtk_tree_view_column_set_widget a b))
(defun gtk::tree-view-column-get-widget (a)
  (#_gtk_tree_view_column_get_widget a))
(defun gtk::tree-view-column-set-alignment (a b)
  (#_gtk_tree_view_column_set_alignment a (coerce b 'single-float)))
(defun gtk::tree-view-column-get-alignment (a)
  (#_gtk_tree_view_column_get_alignment a))
(defun gtk::tree-view-column-set-reorderable (a b)
  (#_gtk_tree_view_column_set_reorderable a (if b (if (eq b 0) 0 1) 0)))
(defun gtk::tree-view-column-get-reorderable (a)
  (let ((z (#_gtk_tree_view_column_get_reorderable a)))
    (values (if (= 1 z) t nil))))
(defun gtk::tree-view-column-set-sort-column-id (a b)
  (#_gtk_tree_view_column_set_sort_column_id a b))
(defun gtk::tree-view-column-get-sort-column-id (a)
  (#_gtk_tree_view_column_get_sort_column_id a))
(defun gtk::tree-view-column-set-sort-indicator (a b)
  (#_gtk_tree_view_column_set_sort_indicator a (if b (if (eq b 0) 0 1) 0)))
(defun gtk::tree-view-column-get-sort-indicator (a)
  (let ((z (#_gtk_tree_view_column_get_sort_indicator a)))
    (values (if (= 1 z) t nil))))
(defun gtk::tree-view-column-set-sort-order (a b)
  (#_gtk_tree_view_column_set_sort_order a b))
(defun gtk::tree-view-column-get-sort-order (a)
  (#_gtk_tree_view_column_get_sort_order a))
(defun gtk::tree-view-column-cell-set-cell-data (a b c d e)
  (#_gtk_tree_view_column_cell_set_cell_data a b c (if d (if (eq d 0) 0 1) 0)
   (if e (if (eq e 0) 0 1) 0)))
(defun gtk::tree-view-column-cell-get-size (a b c d e f)
  (rlet ((r1 :int c) (r2 :int d) (r3 :int e) (r4 :int f))
        (let ((z (#_gtk_tree_view_column_cell_get_size a b r1 r2 r3 r4)))
          (values z
                  (%get-signed-long r1)
                  (%get-signed-long r2)
                  (%get-signed-long r3)
                  (%get-signed-long r4)))))
(defun gtk::tree-view-column-cell-is-visible (a)
  (let ((z (#_gtk_tree_view_column_cell_is_visible a)))
    (values (if (= 1 z) t nil))))
(defun gtk::tree-view-get-type () (#_gtk_tree_view_get_type))
(defun gtk::tree-view-new () (#_gtk_tree_view_new))
(defun gtk::tree-view-new-with-model (a) (#_gtk_tree_view_new_with_model a))
(defun gtk::tree-view-get-model (a) (#_gtk_tree_view_get_model a))
(defun gtk::tree-view-set-model (a b) (#_gtk_tree_view_set_model a b))
(defun gtk::tree-view-get-selection (a) (#_gtk_tree_view_get_selection a))
(defun gtk::tree-view-get-hadjustment (a) (#_gtk_tree_view_get_hadjustment a))
(defun gtk::tree-view-set-hadjustment (a b)
  (#_gtk_tree_view_set_hadjustment a b))
(defun gtk::tree-view-get-vadjustment (a) (#_gtk_tree_view_get_vadjustment a))
(defun gtk::tree-view-set-vadjustment (a b)
  (#_gtk_tree_view_set_vadjustment a b))
(defun gtk::tree-view-get-headers-visible (a)
  (let ((z (#_gtk_tree_view_get_headers_visible a)))
    (values (if (= 1 z) t nil))))
(defun gtk::tree-view-set-headers-visible (a b)
  (#_gtk_tree_view_set_headers_visible a (if b (if (eq b 0) 0 1) 0)))
(defun gtk::tree-view-columns-autosize (a) (#_gtk_tree_view_columns_autosize a))
(defun gtk::tree-view-set-headers-clickable (a b)
  (#_gtk_tree_view_set_headers_clickable a (if b (if (eq b 0) 0 1) 0)))
(defun gtk::tree-view-set-rules-hint (a b)
  (#_gtk_tree_view_set_rules_hint a (if b (if (eq b 0) 0 1) 0)))
(defun gtk::tree-view-get-rules-hint (a)
  (let ((z (#_gtk_tree_view_get_rules_hint a))) (values (if (= 1 z) t nil))))
(defun gtk::tree-view-append-column (a b) (#_gtk_tree_view_append_column a b))
(defun gtk::tree-view-remove-column (a b) (#_gtk_tree_view_remove_column a b))
(defun gtk::tree-view-insert-column (a b c)
  (#_gtk_tree_view_insert_column a b c))
(defun gtk::tree-view-insert-column-with-attributes (a b c d)
  (with-cstrs ((s1 c))
              (#_gtk_tree_view_insert_column_with_attributes a b s1 d)))
(defun gtk::tree-view-insert-column-with-data-func (a b c d e f g)
  (with-cstrs ((s1 c))
              (#_gtk_tree_view_insert_column_with_data_func a b s1 d e f g)))
(defun gtk::tree-view-get-column (a b) (#_gtk_tree_view_get_column a b))
(defun gtk::tree-view-get-columns (a) (#_gtk_tree_view_get_columns a))
(defun gtk::tree-view-move-column-after (a b c)
  (#_gtk_tree_view_move_column_after a b c))
(defun gtk::tree-view-set-expander-column (a b)
  (#_gtk_tree_view_set_expander_column a b))
(defun gtk::tree-view-get-expander-column (a)
  (#_gtk_tree_view_get_expander_column a))
(defun gtk::tree-view-set-column-drag-function (a b c d)
  (#_gtk_tree_view_set_column_drag_function a b c d))
(defun gtk::tree-view-scroll-to-point (a b c)
  (#_gtk_tree_view_scroll_to_point a b c))
(defun gtk::tree-view-scroll-to-cell (a b c d e f)
  (#_gtk_tree_view_scroll_to_cell a b c (if d (if (eq d 0) 0 1) 0)
   (coerce e 'single-float) (coerce f 'single-float)))
(defun gtk::tree-view-row-activated (a b c)
  (#_gtk_tree_view_row_activated a b c))
(defun gtk::tree-view-expand-all (a) (#_gtk_tree_view_expand_all a))
(defun gtk::tree-view-collapse-all (a) (#_gtk_tree_view_collapse_all a))
(defun gtk::tree-view-expand-row (a b c)
  (let ((z (#_gtk_tree_view_expand_row a b (if c (if (eq c 0) 0 1) 0))))
    (values (if (= 1 z) t nil))))
(defun gtk::tree-view-collapse-row (a b)
  (let ((z (#_gtk_tree_view_collapse_row a b))) (values (if (= 1 z) t nil))))
(defun gtk::tree-view-map-expanded-rows (a b c)
  (#_gtk_tree_view_map_expanded_rows a b c))
(defun gtk::tree-view-row-expanded (a b)
  (let ((z (#_gtk_tree_view_row_expanded a b))) (values (if (= 1 z) t nil))))
(defun gtk::tree-view-set-reorderable (a b)
  (#_gtk_tree_view_set_reorderable a (if b (if (eq b 0) 0 1) 0)))
(defun gtk::tree-view-get-reorderable (a)
  (let ((z (#_gtk_tree_view_get_reorderable a))) (values (if (= 1 z) t nil))))
(defun gtk::tree-view-set-cursor (a b c d)
  (#_gtk_tree_view_set_cursor a b c (if d (if (eq d 0) 0 1) 0)))
(defun gtk::tree-view-get-cursor (a b c) (#_gtk_tree_view_get_cursor a b c))
(defun gtk::tree-view-get-bin-window (a) (#_gtk_tree_view_get_bin_window a))
(defun gtk::tree-view-get-path-at-pos (a b c d e f g)
  (rlet ((r1 :int f) (r2 :int g))
        (let ((z (#_gtk_tree_view_get_path_at_pos a b c d e r1 r2)))
          (values (if (= 1 z) t nil)
                  (%get-signed-long r1)
                  (%get-signed-long r2)))))
(defun gtk::tree-view-get-cell-area (a b c d)
  (#_gtk_tree_view_get_cell_area a b c d))
(defun gtk::tree-view-get-background-area (a b c d)
  (#_gtk_tree_view_get_background_area a b c d))
(defun gtk::tree-view-get-visible-rect (a b)
  (#_gtk_tree_view_get_visible_rect a b))
(defun gtk::tree-view-widget-to-tree-coords (a b c d e)
  (rlet ((r1 :int d) (r2 :int e))
        (let ((z (#_gtk_tree_view_widget_to_tree_coords a b c r1 r2)))
          (values z (%get-signed-long r1) (%get-signed-long r2)))))
(defun gtk::tree-view-tree-to-widget-coords (a b c d e)
  (rlet ((r1 :int d) (r2 :int e))
        (let ((z (#_gtk_tree_view_tree_to_widget_coords a b c r1 r2)))
          (values z (%get-signed-long r1) (%get-signed-long r2)))))
(defun gtk::tree-view-enable-model-drag-source (a b c d e)
  (#_gtk_tree_view_enable_model_drag_source a b c d e))
(defun gtk::tree-view-enable-model-drag-dest (a b c d)
  (#_gtk_tree_view_enable_model_drag_dest a b c d))
(defun gtk::tree-view-unset-rows-drag-source (a)
  (#_gtk_tree_view_unset_rows_drag_source a))
(defun gtk::tree-view-unset-rows-drag-dest (a)
  (#_gtk_tree_view_unset_rows_drag_dest a))
(defun gtk::tree-view-set-drag-dest-row (a b c)
  (#_gtk_tree_view_set_drag_dest_row a b c))
(defun gtk::tree-view-get-drag-dest-row (a b c)
  (rlet ((r1 :int c))
        (let ((z (#_gtk_tree_view_get_drag_dest_row a b r1)))
          (values z (%get-signed-long r1)))))
(defun gtk::tree-view-get-dest-row-at-pos (a b c d e)
  (rlet ((r1 :int e))
        (let ((z (#_gtk_tree_view_get_dest_row_at_pos a b c d r1)))
          (values (if (= 1 z) t nil) (%get-signed-long r1)))))
(defun gtk::tree-view-create-row-drag-icon (a b)
  (#_gtk_tree_view_create_row_drag_icon a b))
(defun gtk::tree-view-set-enable-search (a b)
  (#_gtk_tree_view_set_enable_search a (if b (if (eq b 0) 0 1) 0)))
(defun gtk::tree-view-get-enable-search (a)
  (let ((z (#_gtk_tree_view_get_enable_search a))) (values (if (= 1 z) t nil))))
(defun gtk::tree-view-get-search-column (a)
  (#_gtk_tree_view_get_search_column a))
(defun gtk::tree-view-set-search-column (a b)
  (#_gtk_tree_view_set_search_column a b))
(defun gtk::tree-view-get-search-equal-func (a)
  (#_gtk_tree_view_get_search_equal_func a))
(defun gtk::tree-view-set-search-equal-func (a b c d)
  (#_gtk_tree_view_set_search_equal_func a b c d))
(defun gtk::tree-view-set-destroy-count-func (a b c d)
  (#_gtk_tree_view_set_destroy_count_func a b c d))
(defun gtk::tree-selection-get-type () (#_gtk_tree_selection_get_type))
(defun gtk::tree-selection-set-mode (a b) (#_gtk_tree_selection_set_mode a b))
(defun gtk::tree-selection-get-mode (a) (#_gtk_tree_selection_get_mode a))
(defun gtk::tree-selection-set-select-function (a b c d)
  (#_gtk_tree_selection_set_select_function a b c d))
(defun gtk::tree-selection-get-user-data (a)
  (#_gtk_tree_selection_get_user_data a))
(defun gtk::tree-selection-get-tree-view (a)
  (#_gtk_tree_selection_get_tree_view a))
(defun gtk::tree-selection-get-selected (a b c)
  (let ((z (#_gtk_tree_selection_get_selected a b c)))
    (values (if (= 1 z) t nil))))
(defun gtk::tree-selection-selected-foreach (a b c)
  (#_gtk_tree_selection_selected_foreach a b c))
(defun gtk::tree-selection-select-path (a b)
  (#_gtk_tree_selection_select_path a b))
(defun gtk::tree-selection-unselect-path (a b)
  (#_gtk_tree_selection_unselect_path a b))
(defun gtk::tree-selection-select-iter (a b)
  (#_gtk_tree_selection_select_iter a b))
(defun gtk::tree-selection-unselect-iter (a b)
  (#_gtk_tree_selection_unselect_iter a b))
(defun gtk::tree-selection-path-is-selected (a b)
  (let ((z (#_gtk_tree_selection_path_is_selected a b)))
    (values (if (= 1 z) t nil))))
(defun gtk::tree-selection-iter-is-selected (a b)
  (let ((z (#_gtk_tree_selection_iter_is_selected a b)))
    (values (if (= 1 z) t nil))))
(defun gtk::tree-selection-select-all (a) (#_gtk_tree_selection_select_all a))
(defun gtk::tree-selection-unselect-all (a)
  (#_gtk_tree_selection_unselect_all a))
(defun gtk::tree-selection-select-range (a b c)
  (#_gtk_tree_selection_select_range a b c))
(defun gtk::tree-store-get-type () (#_gtk_tree_store_get_type))
(defun gtk::tree-store-new (a) (#_gtk_tree_store_new a))
(defun gtk::tree-store-newv (a b) (#_gtk_tree_store_newv a b))
(defun gtk::tree-store-set-column-types (a b c)
  (#_gtk_tree_store_set_column_types a b c))
(defun gtk::tree-store-set-value (a b c d) (#_gtk_tree_store_set_value a b c d))
(defun gtk::tree-store-set (a b) (#_gtk_tree_store_set a b))
(defun gtk::tree-store-set-valist (a b c)
  (with-cstrs ((s1 c)) (#_gtk_tree_store_set_valist a b s1)))
(defun gtk::tree-store-remove (a b)
  (let ((z (#_gtk_tree_store_remove a b))) (values (if (= 1 z) t nil))))
(defun gtk::tree-store-insert (a b c d) (#_gtk_tree_store_insert a b c d))
(defun gtk::tree-store-insert-before (a b c d)
  (#_gtk_tree_store_insert_before a b c d))
(defun gtk::tree-store-insert-after (a b c d)
  (#_gtk_tree_store_insert_after a b c d))
(defun gtk::tree-store-prepend (a b c) (#_gtk_tree_store_prepend a b c))
(defun gtk::tree-store-append (a b c) (#_gtk_tree_store_append a b c))
(defun gtk::tree-store-is-ancestor (a b c)
  (let ((z (#_gtk_tree_store_is_ancestor a b c))) (values (if (= 1 z) t nil))))
(defun gtk::tree-store-iter-depth (a b) (#_gtk_tree_store_iter_depth a b))
(defun gtk::tree-store-clear (a) (#_gtk_tree_store_clear a))
(defun gtk::vbutton-box-get-type () (#_gtk_vbutton_box_get_type))
(defun gtk::vbutton-box-new () (#_gtk_vbutton_box_new))
(defun gtk::vpaned-get-type () (#_gtk_vpaned_get_type))
(defun gtk::vpaned-new () (#_gtk_vpaned_new))
(defun gtk::vruler-get-type () (#_gtk_vruler_get_type))
(defun gtk::vruler-new () (#_gtk_vruler_new))
(defun gtk::vscale-get-type () (#_gtk_vscale_get_type))
(defun gtk::vscale-new (a) (#_gtk_vscale_new a))
(defun gtk::vscale-new-with-range (a b c)
  (#_gtk_vscale_new_with_range (coerce a 'double-float)
   (coerce b 'double-float) (coerce c 'double-float)))
(defun gtk::vseparator-get-type () (#_gtk_vseparator_get_type))
(defun gtk::vseparator-new () (#_gtk_vseparator_new))

(export '(g::nullptr g::nullptr? g::callback g::signal-connect
          g::signal-connect-after g::signal-connect-swapped g::List.data
          g::List.next g::List.prev g::list-alloc g::list-free g::list-append
          g::list-prepend g::list-insert g::list-insert-sorted
          g::list-insert-before g::list-concat g::list-remove
          g::list-remove-all g::list-reverse g::list-copy g::list-nth
          g::list-find g::list-position g::signal-emit-by-name
          g::signal-stop-emission-by-name g::signal-connect-data
          g::signal-handler-disconnect g::object-set-property
          g::object-get-property g::object-freeze-notify g::object-notify
          g::object-thaw-notify g::object-unref g::object-get-data
          g::object-set-data)
        :g)
(export '(gdk::lsb-first gdk::msb-first gdk::shift-mask gdk::lock-mask
          gdk::control-mask gdk::mod1-mask gdk::mod2-mask gdk::mod3-mask
          gdk::mod4-mask gdk::mod5-mask gdk::button1-mask gdk::button2-mask
          gdk::button3-mask gdk::button4-mask gdk::button5-mask
          gdk::release-mask gdk::modifier-mask gdk::input-read gdk::input-write
          gdk::input-exception gdk::grab-success gdk::grab-already-grabbed
          gdk::grab-invalid-time gdk::grab-not-viewable gdk::grab-frozen
          gdk::x-cursor gdk::arrow gdk::based-arrow-down gdk::based-arrow-up
          gdk::boat gdk::bogosity gdk::bottom-left-corner
          gdk::bottom-right-corner gdk::bottom-side gdk::bottom-tee
          gdk::box-spiral gdk::center-ptr gdk::circle gdk::clock
          gdk::coffee-mug gdk::cross gdk::cross-reverse gdk::crosshair
          gdk::diamond-cross gdk::dot gdk::dotbox gdk::double-arrow
          gdk::draft-large gdk::draft-small gdk::draped-box gdk::exchange
          gdk::fleur gdk::gobbler gdk::gumby gdk::hand1 gdk::hand2 gdk::heart
          gdk::icon gdk::iron-cross gdk::left-ptr gdk::left-side gdk::left-tee
          gdk::leftbutton gdk::ll-angle gdk::lr-angle gdk::man
          gdk::middlebutton gdk::mouse gdk::pencil gdk::pirate gdk::plus
          gdk::question-arrow gdk::right-ptr gdk::right-side gdk::right-tee
          gdk::rightbutton gdk::rtl-logo gdk::sailboat gdk::sb-down-arrow
          gdk::sb-h-double-arrow gdk::sb-left-arrow gdk::sb-right-arrow
          gdk::sb-up-arrow gdk::sb-v-double-arrow gdk::shuttle gdk::sizing
          gdk::spider gdk::spraycan gdk::star gdk::target gdk::tcross
          gdk::top-left-arrow gdk::top-left-corner gdk::top-right-corner
          gdk::top-side gdk::top-tee gdk::trek gdk::ul-angle gdk::umbrella
          gdk::ur-angle gdk::watch gdk::xterm gdk::last-cursor
          gdk::cursor-is-pixmap gdk::action-default gdk::action-copy
          gdk::action-move gdk::action-link gdk::action-private gdk::action-ask
          gdk::drag-proto-motif gdk::drag-proto-xdnd gdk::drag-proto-rootwin
          gdk::drag-proto-none gdk::drag-proto-win32-dropfiles
          gdk::drag-proto-ole2 gdk::drag-proto-local gdk::extension-events-none
          gdk::extension-events-all gdk::extension-events-cursor
          gdk::source-mouse gdk::source-pen gdk::source-eraser
          gdk::source-cursor gdk::mode-disabled gdk::mode-screen
          gdk::mode-window gdk::axis-ignore gdk::axis-x gdk::axis-y
          gdk::axis-pressure gdk::axis-xtilt gdk::axis-ytilt gdk::axis-wheel
          gdk::axis-last gdk::nothing gdk::delete gdk::destroy gdk::expose
          gdk::motion-notify gdk::button-press gdk::2button-press
          gdk::3button-press gdk::button-release gdk::key-press
          gdk::key-release gdk::enter-notify gdk::leave-notify
          gdk::focus-change gdk::configure gdk::map gdk::unmap
          gdk::property-notify gdk::selection-clear gdk::selection-request
          gdk::selection-notify gdk::proximity-in gdk::proximity-out
          gdk::drag-enter gdk::drag-leave gdk::drag-motion gdk::drag-status
          gdk::drop-start gdk::drop-finished gdk::client-event
          gdk::visibility-notify gdk::no-expose gdk::scroll gdk::window-state
          gdk::setting gdk::exposure-mask gdk::pointer-motion-mask
          gdk::pointer-motion-hint-mask gdk::button-motion-mask
          gdk::button1-motion-mask gdk::button2-motion-mask
          gdk::button3-motion-mask gdk::button-press-mask
          gdk::button-release-mask gdk::key-press-mask gdk::key-release-mask
          gdk::enter-notify-mask gdk::leave-notify-mask gdk::focus-change-mask
          gdk::structure-mask gdk::property-change-mask
          gdk::visibility-notify-mask gdk::proximity-in-mask
          gdk::proximity-out-mask gdk::substructure-mask gdk::scroll-mask
          gdk::all-events-mask gdk::window-state-withdrawn
          gdk::window-state-iconified gdk::window-state-maximized
          gdk::window-state-sticky gdk::window-state-fullscreen
          gdk::cap-not-last gdk::cap-butt gdk::cap-round gdk::cap-projecting
          gdk::solid gdk::tiled gdk::stippled gdk::opaque-stippled gdk::copy
          gdk::invert gdk::xor gdk::clear gdk::and gdk::and-reverse
          gdk::and-invert gdk::noop gdk::or gdk::equiv gdk::or-reverse
          gdk::copy-invert gdk::or-invert gdk::nand gdk::nor gdk::set
          gdk::join-miter gdk::join-round gdk::join-bevel gdk::line-solid
          gdk::line-on-off-dash gdk::line-double-dash gdk::clip-by-children
          gdk::include-inferiors gdk::gc-foreground gdk::gc-background
          gdk::gc-font gdk::gc-function gdk::gc-fill gdk::gc-tile
          gdk::gc-stipple gdk::gc-clip-mask gdk::gc-subwindow
          gdk::gc-ts-x-origin gdk::gc-ts-y-origin gdk::gc-clip-x-origin
          gdk::gc-clip-y-origin gdk::gc-exposures gdk::gc-line-width
          gdk::gc-line-style gdk::gc-cap-style gdk::gc-join-style
          gdk::rgb-dither-none gdk::rgb-dither-normal gdk::rgb-dither-max
          gdk::pixbuf-alpha-bilevel gdk::pixbuf-alpha-full gdk::colorspace-rgb
          gdk::interp-nearest gdk::interp-tiles gdk::interp-bilinear
          gdk::interp-hyper gdk::font-font gdk::font-fontset gdk::image-normal
          gdk::image-shared gdk::image-fastest gdk::prop-mode-replace
          gdk::prop-mode-prepend gdk::prop-mode-append gdk::visual-static-gray
          gdk::visual-grayscale gdk::visual-static-color
          gdk::visual-pseudo-color gdk::visual-true-color
          gdk::visual-direct-color gdk::window-root gdk::window-toplevel
          gdk::window-child gdk::window-dialog gdk::window-temp
          gdk::window-foreign gdk::hint-pos gdk::hint-min-size
          gdk::hint-max-size gdk::hint-base-size gdk::hint-aspect
          gdk::hint-resize-inc gdk::hint-win-gravity gdk::hint-user-pos
          gdk::hint-user-size gdk::window-type-hint-normal
          gdk::window-type-hint-dialog gdk::window-type-hint-menu
          gdk::window-type-hint-toolbar gdk::window-type-hint-splashscreen
          gdk::window-type-hint-utility gdk::window-type-hint-dock
          gdk::window-type-hint-desktop gdk::decor-all gdk::decor-border
          gdk::decor-resizeh gdk::decor-title gdk::decor-menu
          gdk::decor-minimize gdk::decor-maximize gdk::func-all
          gdk::func-resize gdk::func-move gdk::func-minimize gdk::func-maximize
          gdk::func-close gdk::gravity-north-west gdk::gravity-north
          gdk::gravity-north-east gdk::gravity-west gdk::gravity-center
          gdk::gravity-east gdk::gravity-south-west gdk::gravity-south
          gdk::gravity-south-east gdk::gravity-static
          gdk::window-edge-north-west gdk::window-edge-north
          gdk::window-edge-north-east gdk::window-edge-west
          gdk::window-edge-east gdk::window-edge-south-west
          gdk::window-edge-south gdk::window-edge-south-east gdk::Rectangle.x
          gdk::Rectangle.y gdk::Rectangle.width gdk::Rectangle.height
          gdk::Color.pixel gdk::Color.red gdk::Color.green gdk::Color.blue
          gdk::EventExpose.type gdk::EventExpose.window
          gdk::EventExpose.send-event gdk::EventExpose.area.x
          gdk::EventExpose.area.y gdk::EventExpose.area.width
          gdk::EventExpose.area.height gdk::EventExpose.region
          gdk::EventExpose.count gdk::EventMotion.type gdk::EventMotion.window
          gdk::EventMotion.send-event gdk::EventMotion.time gdk::EventMotion.x
          gdk::EventMotion.y gdk::EventMotion.state gdk::EventMotion.is-hint
          gdk::EventMotion.device gdk::EventMotion.x-root
          gdk::EventMotion.y-root gdk::EventButton.type gdk::EventButton.window
          gdk::EventButton.send-event gdk::EventButton.time gdk::EventButton.x
          gdk::EventButton.y gdk::EventButton.state gdk::EventButton.button
          gdk::EventButton.device gdk::EventButton.x-root
          gdk::EventButton.y-root gdk::EventKey.type gdk::EventKey.window
          gdk::EventKey.send-event gdk::EventKey.time gdk::EventKey.state
          gdk::EventKey.keyval gdk::EventKey.length gdk::EventKey.string
          gdk::Font.type gdk::Font.ascent gdk::Font.descent gdk::Image.type
          gdk::Image.visual gdk::Image.byte-order gdk::Image.width
          gdk::Image.height gdk::Image.depth gdk::Image.bpp gdk::Image.bpl
          gdk::Image.bits-per-pixel gdk::Image.mem gdk::Image.colormap
          gdk::Image.windowing-data gdk::Visual.type gdk::Visual.depth
          gdk::Visual.byte-order gdk::Visual.colormap-size
          gdk::Visual.bits-per-rgb gdk::Visual.red-mask gdk::Visual.red-shift
          gdk::Visual.green-prec gdk::Visual.blue-mask gdk::Visual.blue-shift
          gdk::Visual.blue-prec gdk::Geometry.min-width
          gdk::Geometry.min-height gdk::Geometry.max-width
          gdk::Geometry.max-height gdk::Geometry.base-width
          gdk::Geometry.base-height gdk::Geometry.width-inc
          gdk::Geometry.height-inc gdk::Geometry.min-aspect
          gdk::Geometry.max-aspect gdk::Geometry.win-gravity
          gdk::colormap-get-type gdk::colormap-new gdk::colormap-get-system
          gdk::colormap-get-system-size gdk::colormap-alloc-color
          gdk::colormap-get-visual gdk::color-copy gdk::color-free
          gdk::color-parse gdk::color-white gdk::color-black gdk::color-alloc
          gdk::color-change gdk::cursor-new gdk::cursor-new-from-pixmap
          gdk::cursor-ref gdk::cursor-unref gdk::drag-context-get-type
          gdk::drag-context-new gdk::drag-context-ref gdk::drag-context-unref
          gdk::drag-status gdk::drop-reply gdk::drop-finish
          gdk::drag-get-selection gdk::drag-begin gdk::drag-get-protocol
          gdk::drag-find-window gdk::drag-motion gdk::drag-drop gdk::drag-abort
          gdk::device-get-type gdk::devices-list gdk::device-set-source
          gdk::device-set-mode gdk::device-set-key gdk::device-set-axis-use
          gdk::device-get-state gdk::device-get-history
          gdk::device-free-history gdk::device-get-axis
          gdk::input-set-extension-events gdk::device-get-core-pointer
          gdk::event-get-type gdk::events-pending gdk::event-get
          gdk::event-peek gdk::event-get-graphics-expose gdk::event-put
          gdk::event-copy gdk::event-free gdk::event-get-time
          gdk::event-get-state gdk::event-get-coords gdk::event-get-root-coords
          gdk::event-get-axis gdk::event-handler-set gdk::set-show-events
          gdk::get-show-events gdk::add-client-message-filter gdk::setting-get
          gdk::gc-get-type gdk::gc-new gdk::gc-new-with-values
          gdk::gc-get-values gdk::gc-set-values gdk::gc-set-foreground
          gdk::gc-set-background gdk::gc-set-font gdk::gc-set-function
          gdk::gc-set-fill gdk::gc-set-tile gdk::gc-set-stipple
          gdk::gc-set-ts-origin gdk::gc-set-clip-origin gdk::gc-set-clip-mask
          gdk::gc-set-clip-rectangle gdk::gc-set-clip-region
          gdk::gc-set-subwindow gdk::gc-set-exposures
          gdk::gc-set-line-attributes gdk::gc-set-dashes gdk::gc-offset
          gdk::gc-copy gdk::gc-set-colormap gdk::gc-get-colormap
          gdk::gc-set-rgb-fg-color gdk::gc-set-rgb-bg-color
          gdk::rgb-xpixel-from-rgb gdk::rgb-gc-set-foreground
          gdk::rgb-gc-set-background gdk::draw-rgb-image
          gdk::draw-rgb-image-dithalign gdk::draw-rgb-32-image
          gdk::draw-gray-image gdk::draw-indexed-image gdk::rgb-cmap-new
          gdk::rgb-set-verbose gdk::rgb-set-install gdk::rgb-set-min-colors
          gdk::rgb-get-colormap gdk::rgb-get-visual gdk::rgb-ditherable
          gdk::pixbuf-get-colorspace gdk::pixbuf-get-n-channels
          gdk::pixbuf-get-has-alpha gdk::pixbuf-get-bits-per-sample
          gdk::pixbuf-get-pixels gdk::pixbuf-get-width gdk::pixbuf-get-height
          gdk::pixbuf-get-rowstride gdk::pixbuf-new gdk::pixbuf-copy
          gdk::pixbuf-new-from-file gdk::pixbuf-new-from-data
          gdk::pixbuf-new-from-xpm-data gdk::pixbuf-new-from-inline
          gdk::pixbuf-fill gdk::pixbuf-save gdk::pixbuf-savev
          gdk::pixbuf-add-alpha gdk::pixbuf-copy-area
          gdk::pixbuf-saturate-and-pixelate gdk::pixbuf-scale
          gdk::pixbuf-composite gdk::pixbuf-composite-color
          gdk::pixbuf-scale-simple gdk::pixbuf-composite-color-simple
          gdk::pixbuf-animation-get-type gdk::pixbuf-animation-new-from-file
          gdk::pixbuf-animation-get-width gdk::pixbuf-animation-get-height
          gdk::pixbuf-animation-is-static-image
          gdk::pixbuf-animation-get-static-image gdk::pixbuf-animation-get-iter
          gdk::pixbuf-animation-iter-get-type
          gdk::pixbuf-animation-iter-get-delay-time
          gdk::pixbuf-animation-iter-get-pixbuf
          gdk::pixbuf-animation-iter-on-currently-loading-frame
          gdk::pixbuf-animation-iter-advance gdk::pixbuf-get-option
          gdk::pixbuf-loader-get-type gdk::pixbuf-loader-new
          gdk::pixbuf-loader-new-with-type gdk::pixbuf-loader-write
          gdk::pixbuf-loader-get-pixbuf gdk::pixbuf-loader-get-animation
          gdk::pixbuf-loader-close gdk::drawable-get-type
          gdk::drawable-set-data gdk::drawable-get-data gdk::drawable-get-size
          gdk::drawable-set-colormap gdk::drawable-get-colormap
          gdk::drawable-get-visual gdk::drawable-get-depth gdk::drawable-ref
          gdk::drawable-unref gdk::draw-point gdk::draw-line
          gdk::draw-rectangle gdk::draw-arc gdk::draw-polygon gdk::draw-string
          gdk::draw-text gdk::draw-text-wc gdk::draw-drawable gdk::draw-image
          gdk::draw-points gdk::draw-segments gdk::draw-lines gdk::draw-glyphs
          gdk::draw-layout-line gdk::draw-layout
          gdk::draw-layout-line-with-colors gdk::drawable-get-image
          gdk::drawable-get-clip-region gdk::drawable-get-visible-region
          gdk::font-ref gdk::font-unref gdk::font-id gdk::font-load
          gdk::fontset-load gdk::font-from-description gdk::string-width
          gdk::text-width gdk::text-width-wc gdk::char-width gdk::char-width-wc
          gdk::string-measure gdk::text-measure gdk::char-measure
          gdk::string-height gdk::text-height gdk::char-height
          gdk::text-extents gdk::text-extents-wc gdk::string-extents
          gdk::image-get-type gdk::image-new gdk::image-put-pixel
          gdk::image-get-pixel gdk::image-set-colormap gdk::image-get-colormap
          gdk::keymap-get-type gdk::keymap-get-default gdk::keymap-lookup-key
          gdk::keymap-translate-keyboard-state
          gdk::keymap-get-entries-for-keyval
          gdk::keymap-get-entries-for-keycode gdk::keymap-get-direction
          gdk::keyval-name gdk::keyval-from-name gdk::keyval-convert-case
          gdk::keyval-to-upper gdk::keyval-to-lower gdk::keyval-is-upper
          gdk::keyval-is-lower gdk::keyval-to-unicode gdk::unicode-to-keyval
          gdk::pixbuf-render-to-drawable gdk::pixbuf-render-to-drawable-alpha
          gdk::pixbuf-render-pixmap-and-mask gdk::pixbuf-get-from-drawable
          gdk::pixbuf-get-from-image gdk::pixmap-get-type gdk::pixmap-new
          gdk::bitmap-create-from-data gdk::pixmap-create-from-data
          gdk::pixmap-create-from-xpm gdk::pixmap-colormap-create-from-xpm
          gdk::pixmap-create-from-xpm-d gdk::pixmap-colormap-create-from-xpm-d
          gdk::pixmap-foreign-new gdk::pixmap-lookup gdk::atom-intern
          gdk::atom-name gdk::property-get gdk::property-change
          gdk::property-delete gdk::selection-owner-get gdk::selection-convert
          gdk::selection-property-get gdk::selection-send-notify
          gdk::visual-get-best-depth gdk::visual-get-best-type
          gdk::visual-get-system gdk::visual-get-best
          gdk::visual-get-best-with-depth gdk::visual-get-best-with-type
          gdk::visual-get-best-with-both gdk::query-depths
          gdk::query-visual-types gdk::list-visuals gdk::window-object-get-type
          gdk::window-new gdk::window-destroy gdk::window-get-window-type
          gdk::window-at-pointer gdk::window-show gdk::window-hide
          gdk::window-withdraw gdk::window-move gdk::window-resize
          gdk::window-move-resize gdk::window-reparent gdk::window-clear
          gdk::window-clear-area gdk::window-clear-area-e gdk::window-raise
          gdk::window-lower gdk::window-focus gdk::window-set-user-data
          gdk::window-set-override-redirect gdk::window-add-filter
          gdk::window-remove-filter gdk::window-scroll
          gdk::window-shape-combine-mask gdk::window-set-child-shapes
          gdk::window-merge-child-shapes gdk::window-is-visible
          gdk::window-is-viewable gdk::window-get-state
          gdk::window-set-static-gravities gdk::window-foreign-new
          gdk::window-lookup gdk::window-set-hints gdk::window-set-type-hint
          gdk::window-set-modal-hint gdk::window-set-geometry-hints
          gdk::set-sm-client-id gdk::window-begin-paint-rect
          gdk::window-begin-paint-region gdk::window-end-paint
          gdk::window-set-title gdk::window-set-role
          gdk::window-set-transient-for gdk::window-set-background
          gdk::window-set-back-pixmap gdk::window-set-cursor
          gdk::window-get-geometry gdk::window-get-position
          gdk::window-get-origin gdk::window-get-deskrelative-origin
          gdk::window-get-root-origin gdk::window-get-frame-extents
          gdk::window-get-pointer gdk::window-get-parent
          gdk::window-get-toplevel gdk::window-get-children
          gdk::window-peek-children gdk::window-get-events
          gdk::window-set-events gdk::window-set-icon-list gdk::window-set-icon
          gdk::window-set-icon-name gdk::window-set-group
          gdk::window-set-decorations gdk::window-get-decorations
          gdk::window-set-functions gdk::window-get-toplevels
          gdk::window-iconify gdk::window-deiconify gdk::window-stick
          gdk::window-unstick gdk::window-maximize gdk::window-unmaximize
          gdk::window-register-dnd gdk::window-begin-resize-drag
          gdk::window-begin-move-drag gdk::window-invalidate-rect
          gdk::window-invalidate-region gdk::window-invalidate-maybe-recurse
          gdk::window-get-update-area gdk::window-freeze-updates
          gdk::window-thaw-updates gdk::window-process-all-updates
          gdk::window-process-updates gdk::window-set-debug-updates
          gdk::window-constrain-size gdk::window-get-internal-paint-info
          gdk::set-pointer-hooks gdk::get-default-root-window gdk::pointer-grab
          gdk::keyboard-grab gdk::pointer-ungrab gdk::keyboard-ungrab
          gdk::pointer-is-grabbed gdk::screen-width gdk::screen-height
          gdk::screen-width-mm gdk::screen-height-mm gdk::beep gdk::flush
          gdk::set-double-click-time gdk::rectangle-intersect
          gdk::rectangle-union gdk::threads-enter gdk::threads-leave
          gdk::threads-init)
        :gdk)
(export '(atk::state-invalid atk::state-active atk::state-armed atk::state-busy
          atk::state-checked atk::state-defunct atk::state-editable
          atk::state-enabled atk::state-expandable atk::state-expanded
          atk::state-focusable atk::state-focused atk::state-horizontal
          atk::state-iconified atk::state-modal atk::state-multi-line
          atk::state-multiselectable atk::state-opaque atk::state-pressed
          atk::state-resizable atk::state-selectable atk::state-selected
          atk::state-sensitive atk::state-showing atk::state-single-line
          atk::state-stale atk::state-transient atk::state-vertical
          atk::state-visible atk::state-manages-descendants
          atk::state-indeterminate atk::state-last-defined atk::relation-null
          atk::relation-controlled-by atk::relation-controller-for
          atk::relation-label-for atk::relation-labelled-by
          atk::relation-member-of atk::relation-node-child-of
          atk::relation-flows-to atk::relation-flows-from
          atk::relation-subwindow-of atk::relation-embeds
          atk::relation-embedded-by atk::relation-popup-for
          atk::relation-last-defined atk::role-invalid atk::role-accel-label
          atk::role-alert atk::role-animation atk::role-arrow
          atk::role-calendar atk::role-canvas atk::role-check-box
          atk::role-check-menu-item atk::role-color-chooser
          atk::role-column-header atk::role-combo-box atk::role-date-editor
          atk::role-desktop-icon atk::role-desktop-frame atk::role-dial
          atk::role-dialog atk::role-directory-pane atk::role-drawing-area
          atk::role-file-chooser atk::role-filler atk::role-font-chooser
          atk::role-frame atk::role-glass-pane atk::role-html-container
          atk::role-icon atk::role-image atk::role-internal-frame
          atk::role-label atk::role-layered-pane atk::role-list
          atk::role-list-item atk::role-menu atk::role-menu-bar
          atk::role-menu-item atk::role-option-pane atk::role-page-tab
          atk::role-page-tab-list atk::role-panel atk::role-password-text
          atk::role-popup-menu atk::role-progress-bar atk::role-push-button
          atk::role-radio-button atk::role-radio-menu-item atk::role-root-pane
          atk::role-row-header atk::role-scroll-bar atk::role-scroll-pane
          atk::role-separator atk::role-slider atk::role-split-pane
          atk::role-spin-button atk::role-statusbar atk::role-table
          atk::role-table-cell atk::role-table-column-header
          atk::role-table-row-header atk::role-tear-off-menu-item
          atk::role-terminal atk::role-text atk::role-toggle-button
          atk::role-tool-bar atk::role-tool-tip atk::role-tree
          atk::role-tree-table atk::role-unknown atk::role-viewport
          atk::role-window atk::role-header atk::role-footer
          atk::role-paragraph atk::role-ruler atk::role-application
          atk::role-autocomplete atk::role-last-defined atk::layer-invalid
          atk::layer-background atk::layer-canvas atk::layer-widget
          atk::layer-mdi atk::layer-popup atk::layer-overlay atk::layer-window
          atk::xy-screen atk::xy-window atk::text-attr-invalid
          atk::text-attr-left-margin atk::text-attr-right-margin
          atk::text-attr-indent atk::text-attr-invisible
          atk::text-attr-editable atk::text-attr-pixels-above-lines
          atk::text-attr-pixels-below-lines atk::text-attr-pixels-inside-wrap
          atk::text-attr-bg-full-height atk::text-attr-rise
          atk::text-attr-underline atk::text-attr-strikethrough
          atk::text-attr-size atk::text-attr-scale atk::text-attr-weight
          atk::text-attr-language atk::text-attr-family-name
          atk::text-attr-bg-color atk::text-attr-fg-color
          atk::text-attr-bg-stipple atk::text-attr-fg-stipple
          atk::text-attr-wrap-mode atk::text-attr-direction
          atk::text-attr-justification atk::text-attr-stretch
          atk::text-attr-variant atk::text-attr-style
          atk::text-attr-last-defined atk::text-boundary-char
          atk::text-boundary-word-start atk::text-boundary-word-end
          atk::text-boundary-sentence-start atk::text-boundary-sentence-end
          atk::text-boundary-line-start atk::text-boundary-line-end
          atk::state-type-get-name atk::state-type-for-name
          atk::object-get-type atk::implementor-get-type
          atk::implementor-ref-accessible atk::object-get-name
          atk::object-get-description atk::object-get-parent
          atk::object-get-n-accessible-children
          atk::object-ref-accessible-child atk::object-ref-relation-set
          atk::object-get-role atk::object-get-layer atk::object-get-mdi-zorder
          atk::object-ref-state-set atk::object-get-index-in-parent
          atk::object-set-name atk::object-set-description
          atk::object-set-parent atk::object-set-role
          atk::object-connect-property-change-handler
          atk::object-remove-property-change-handler
          atk::object-notify-state-change atk::role-get-name atk::role-for-name
          atk::action-get-type atk::action-do-action atk::action-get-n-actions
          atk::action-get-description atk::action-get-name
          atk::action-get-keybinding atk::action-set-description
          atk::util-get-type atk::add-focus-tracker atk::remove-focus-tracker
          atk::focus-tracker-init atk::focus-tracker-notify
          atk::add-global-event-listener atk::remove-global-event-listener
          atk::add-key-event-listener atk::remove-key-event-listener
          atk::get-root atk::get-toolkit-name atk::get-toolkit-version
          atk::component-get-type atk::component-add-focus-handler
          atk::component-contains atk::component-ref-accessible-at-point
          atk::component-get-extents atk::component-get-position
          atk::component-get-size atk::component-grab-focus
          atk::component-remove-focus-handler atk::component-set-extents
          atk::component-set-position atk::component-set-size
          atk::document-get-type atk::document-get-document-type
          atk::document-get-document atk::text-get-type atk::text-get-text
          atk::text-get-character-at-offset atk::text-get-text-after-offset
          atk::text-get-text-at-offset atk::text-get-text-before-offset
          atk::text-get-caret-offset atk::text-get-character-extents
          atk::text-get-run-attributes atk::text-get-default-attributes
          atk::text-get-character-count atk::text-get-offset-at-point
          atk::text-get-n-selections atk::text-get-selection
          atk::text-add-selection atk::text-remove-selection
          atk::text-set-selection atk::text-set-caret-offset
          atk::attribute-set-free atk::text-attribute-get-name
          atk::text-attribute-get-value atk::editable-text-get-type
          atk::editable-text-set-run-attributes
          atk::editable-text-set-text-contents atk::editable-text-insert-text
          atk::editable-text-copy-text atk::editable-text-cut-text
          atk::editable-text-delete-text atk::editable-text-paste-text
          atk::hyperlink-get-type atk::hyperlink-get-uri
          atk::hyperlink-get-object atk::hyperlink-get-end-index
          atk::hyperlink-get-start-index atk::hyperlink-is-valid
          atk::hyperlink-get-n-anchors atk::hypertext-get-type
          atk::hypertext-get-link atk::hypertext-get-n-links
          atk::hypertext-get-link-index atk::image-get-type
          atk::image-get-image-description atk::image-get-image-size
          atk::image-set-image-description atk::image-get-image-position
          atk::no-op-object-get-type atk::no-op-object-new
          atk::object-factory-get-type atk::object-factory-create-accessible
          atk::object-factory-invalidate atk::no-op-object-factory-get-type
          atk::no-op-object-factory-new atk::registry-get-type
          atk::registry-set-factory-type atk::registry-get-factory-type
          atk::registry-get-factory atk::get-default-registry
          atk::relation-get-type atk::relation-type-register
          atk::relation-type-get-name atk::relation-type-for-name
          atk::relation-new atk::relation-get-relation-type
          atk::relation-get-target atk::relation-set-get-type
          atk::relation-set-new atk::relation-set-contains
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
          atk::state-set-remove-state atk::state-set-and-sets
          atk::state-set-or-sets atk::state-set-xor-sets
          atk::streamable-content-get-type
          atk::streamable-content-get-n-mime-types
          atk::streamable-content-get-mime-type
          atk::streamable-content-get-stream atk::table-get-type
          atk::table-ref-at atk::table-get-index-at
          atk::table-get-column-at-index atk::table-get-row-at-index
          atk::table-get-n-columns atk::table-get-n-rows
          atk::table-get-column-extent-at atk::table-get-row-extent-at
          atk::table-get-caption atk::table-get-column-description
          atk::table-get-column-header atk::table-get-row-description
          atk::table-get-row-header atk::table-get-summary
          atk::table-set-caption atk::table-set-column-description
          atk::table-set-column-header atk::table-set-row-description
          atk::table-set-row-header atk::table-set-summary
          atk::table-get-selected-columns atk::table-get-selected-rows
          atk::table-is-column-selected atk::table-is-row-selected
          atk::table-is-selected atk::table-add-row-selection
          atk::table-remove-row-selection atk::table-add-column-selection
          atk::table-remove-column-selection atk::value-get-type
          atk::value-get-current-value atk::value-get-maximum-value
          atk::value-get-minimum-value atk::value-set-current-value)
        :atk)
(export '(pango::coverage-none pango::coverage-fallback
          pango::coverage-approximate pango::coverage-exact
          pango::direction-ltr pango::direction-rtl pango::direction-ttb-ltr
          pango::direction-ttb-rtl pango::style-normal pango::style-oblique
          pango::style-italic pango::variant-normal pango::variant-small-caps
          pango::weight-ultralight pango::weight-light pango::weight-normal
          pango::weight-bold pango::weight-ultrabold pango::weight-heavy
          pango::stretch-ultra-condensed pango::stretch-extra-condensed
          pango::stretch-condensed pango::stretch-semi-condensed
          pango::stretch-normal pango::stretch-semi-expanded
          pango::stretch-expanded pango::stretch-extra-expanded
          pango::stretch-ultra-expanded pango::font-mask-family
          pango::font-mask-style pango::font-mask-variant
          pango::font-mask-weight pango::font-mask-stretch
          pango::font-mask-size pango::attr-invalid pango::attr-language
          pango::attr-family pango::attr-style pango::attr-weight
          pango::attr-variant pango::attr-stretch pango::attr-size
          pango::attr-font-desc pango::attr-foreground pango::attr-background
          pango::attr-underline pango::attr-strikethrough pango::attr-rise
          pango::attr-shape pango::attr-scale pango::underline-none
          pango::underline-single pango::underline-double pango::underline-low
          pango::tab-left pango::align-left pango::align-center
          pango::align-right pango::wrap-word pango::wrap-char
          pango::wrap-word-char pango::coverage-ref pango::coverage-unref
          pango::coverage-copy pango::coverage-get pango::coverage-set
          pango::coverage-max pango::coverage-to-bytes
          pango::coverage-from-bytes pango::language-from-string
          pango::language-matches pango::font-description-new
          pango::font-description-copy pango::font-description-copy-static
          pango::font-description-hash pango::font-description-equal
          pango::font-description-free pango::font-descriptions-free
          pango::font-description-set-family
          pango::font-description-set-family-static
          pango::font-description-get-family pango::font-description-set-style
          pango::font-description-get-style pango::font-description-set-variant
          pango::font-description-get-variant
          pango::font-description-set-weight pango::font-description-get-weight
          pango::font-description-set-stretch
          pango::font-description-get-stretch pango::font-description-set-size
          pango::font-description-get-size
          pango::font-description-get-set-fields
          pango::font-description-unset-fields pango::font-description-merge
          pango::font-description-merge-static
          pango::font-description-better-match
          pango::font-description-from-string pango::font-description-to-string
          pango::font-description-to-filename pango::font-metrics-get-type
          pango::font-metrics-ref pango::font-metrics-unref
          pango::font-metrics-get-ascent pango::font-metrics-get-descent
          pango::font-metrics-get-approximate-char-width
          pango::font-metrics-get-approximate-digit-width
          pango::font-family-get-type pango::font-family-list-faces
          pango::font-family-get-name pango::font-face-get-type
          pango::font-face-describe pango::font-face-get-face-name
          pango::font-get-type pango::font-describe pango::font-get-coverage
          pango::font-find-shaper pango::font-get-metrics
          pango::font-get-glyph-extents pango::color-get-type pango::color-copy
          pango::color-free pango::color-parse pango::attr-type-register
          pango::attribute-copy pango::attribute-destroy pango::attribute-equal
          pango::attr-language-new pango::attr-family-new
          pango::attr-foreground-new pango::attr-background-new
          pango::attr-size-new pango::attr-style-new pango::attr-weight-new
          pango::attr-variant-new pango::attr-stretch-new
          pango::attr-font-desc-new pango::attr-underline-new
          pango::attr-strikethrough-new pango::attr-rise-new
          pango::attr-shape-new pango::attr-scale-new pango::attr-list-get-type
          pango::attr-list-new pango::attr-list-ref pango::attr-list-unref
          pango::attr-list-copy pango::attr-list-insert
          pango::attr-list-insert-before pango::attr-list-change
          pango::attr-list-splice pango::attr-list-get-iterator
          pango::attr-iterator-range pango::attr-iterator-next
          pango::attr-iterator-copy pango::attr-iterator-destroy
          pango::attr-iterator-get pango::attr-iterator-get-font
          pango::parse-markup pango::break pango::find-paragraph-boundary
          pango::get-log-attrs pango::fontset-get-font
          pango::fontset-get-metrics pango::font-map-load-font
          pango::font-map-load-fontset pango::font-map-list-families
          pango::context-list-families pango::context-load-font
          pango::context-load-fontset pango::context-get-metrics
          pango::context-set-font-description
          pango::context-get-font-description pango::context-get-language
          pango::context-set-language pango::context-set-base-dir
          pango::context-get-base-dir pango::itemize pango::glyph-string-new
          pango::glyph-string-set-size pango::glyph-string-get-type
          pango::glyph-string-copy pango::glyph-string-free
          pango::glyph-string-extents pango::glyph-string-extents-range
          pango::glyph-string-get-logical-widths pango::glyph-string-index-to-x
          pango::glyph-string-x-to-index pango::shape pango::reorder-items
          pango::tab-array-new pango::tab-array-new-with-positions
          pango::tab-array-get-type pango::tab-array-copy pango::tab-array-free
          pango::tab-array-get-size pango::tab-array-resize
          pango::tab-array-set-tab pango::tab-array-get-tab
          pango::tab-array-get-tabs pango::tab-array-get-positions-in-pixels
          pango::layout-get-type pango::layout-new pango::layout-copy
          pango::layout-get-context pango::layout-set-attributes
          pango::layout-get-attributes pango::layout-set-text
          pango::layout-get-text pango::layout-set-markup
          pango::layout-set-markup-with-accel
          pango::layout-set-font-description pango::layout-set-width
          pango::layout-get-width pango::layout-set-wrap pango::layout-get-wrap
          pango::layout-set-indent pango::layout-get-indent
          pango::layout-set-spacing pango::layout-get-spacing
          pango::layout-set-justify pango::layout-get-justify
          pango::layout-set-alignment pango::layout-get-alignment
          pango::layout-set-tabs pango::layout-get-tabs
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
          pango::layout-get-iter pango::layout-iter-free
          pango::layout-iter-get-index pango::layout-iter-get-run
          pango::layout-iter-get-line pango::layout-iter-at-last-line
          pango::layout-iter-next-char pango::layout-iter-next-cluster
          pango::layout-iter-next-run pango::layout-iter-next-line
          pango::layout-iter-get-char-extents
          pango::layout-iter-get-cluster-extents
          pango::layout-iter-get-run-extents
          pango::layout-iter-get-line-extents
          pango::layout-iter-get-line-yrange
          pango::layout-iter-get-layout-extents
          pango::layout-iter-get-baseline)
        :pango)
(export '(gtk::+false+ gtk::+true+ gtk::init-ensure struct-alloc struct-free
          string->cstring cstring->string cstring-alloc cstring-free
          define-signal-handler gtk::arrow-up gtk::arrow-down gtk::arrow-left
          gtk::arrow-right gtk::expand gtk::shrink gtk::fill
          gtk::buttonbox-default-style gtk::buttonbox-spread
          gtk::buttonbox-edge gtk::buttonbox-start gtk::buttonbox-end
          gtk::curve-type-linear gtk::curve-type-spline gtk::curve-type-free
          gtk::dir-tab-forward gtk::dir-tab-backward gtk::dir-up gtk::dir-down
          gtk::dir-left gtk::dir-right gtk::expander-collapsed
          gtk::expander-semi-collapsed gtk::expander-semi-expanded
          gtk::expander-expanded gtk::icon-size-invalid gtk::icon-size-menu
          gtk::icon-size-small-toolbar gtk::icon-size-large-toolbar
          gtk::icon-size-button gtk::icon-size-dnd gtk::icon-size-dialog
          gtk::text-dir-none gtk::text-dir-ltr gtk::text-dir-rtl
          gtk::justify-left gtk::justify-right gtk::justify-center
          gtk::justify-fill gtk::pixels gtk::inches gtk::centimeters
          gtk::orientation-horizontal gtk::orientation-vertical
          gtk::corner-top-left gtk::corner-bottom-left gtk::corner-top-right
          gtk::corner-bottom-right gtk::pack-start gtk::pack-end
          gtk::path-prio-lowest gtk::path-prio-gtk gtk::path-prio-application
          gtk::path-prio-theme gtk::path-prio-rc gtk::path-prio-highest
          gtk::path-widget gtk::path-widget-class gtk::path-class
          gtk::policy-always gtk::policy-automatic gtk::policy-never
          gtk::pos-left gtk::pos-right gtk::pos-top gtk::pos-bottom
          gtk::relief-normal gtk::relief-half gtk::relief-none
          gtk::resize-parent gtk::resize-queue gtk::resize-immediate
          gtk::selection-none gtk::selection-single gtk::selection-browse
          gtk::selection-multiple gtk::selection-extended gtk::shadow-none
          gtk::shadow-in gtk::shadow-out gtk::shadow-etched-in
          gtk::shadow-etched-out gtk::state-normal gtk::state-active
          gtk::state-prelight gtk::state-selected gtk::state-insensitive
          gtk::toolbar-icons gtk::toolbar-text gtk::toolbar-both
          gtk::toolbar-both-horiz gtk::update-continuous
          gtk::update-discontinuous gtk::update-delayed gtk::visibility-none
          gtk::visibility-partial gtk::visibility-full gtk::win-pos-none
          gtk::win-pos-center gtk::win-pos-mouse gtk::win-pos-center-always
          gtk::win-pos-center-on-parent gtk::window-toplevel gtk::window-popup
          gtk::wrap-none gtk::wrap-char gtk::wrap-word gtk::sort-ascending
          gtk::sort-descending gtk::accel-visible gtk::accel-locked
          gtk::accel-mask gtk::calendar-show-heading
          gtk::calendar-show-day-names gtk::calendar-no-month-change
          gtk::calendar-show-week-numbers gtk::calendar-week-start-monday
          gtk::cell-renderer-selected gtk::cell-renderer-prelit
          gtk::cell-renderer-insensitive gtk::cell-renderer-sorted
          gtk::cell-renderer-focused gtk::cell-empty gtk::cell-text
          gtk::cell-pixmap gtk::cell-pixtext gtk::cell-widget gtk::dialog-modal
          gtk::dialog-destroy-with-parent gtk::dialog-no-separator
          gtk::ctree-lines-none gtk::ctree-lines-solid gtk::ctree-lines-dotted
          gtk::ctree-lines-tabbed gtk::ctree-expander-none
          gtk::ctree-expander-square gtk::ctree-expander-triangle
          gtk::ctree-expander-circular gtk::dest-default-motion
          gtk::dest-default-highlight gtk::dest-default-drop
          gtk::dest-default-all gtk::image-empty gtk::image-pixmap
          gtk::image-image gtk::image-pixbuf gtk::image-stock
          gtk::image-icon-set gtk::image-animation
          gtk::tree-model-iters-persist gtk::tree-model-list-only
          gtk::message-info gtk::message-warning gtk::message-question
          gtk::message-error gtk::buttons-none gtk::buttons-ok
          gtk::buttons-close gtk::buttons-cancel gtk::buttons-yes-no
          gtk::buttons-ok-cancel gtk::progress-left-to-right
          gtk::progress-right-to-left gtk::progress-bottom-to-top
          gtk::progress-top-to-bottom gtk::size-group-none
          gtk::size-group-horizontal gtk::size-group-vertical
          gtk::size-group-both gtk::update-always gtk::update-if-valid
          gtk::spin-step-forward gtk::spin-step-backward gtk::spin-page-forward
          gtk::spin-page-backward gtk::spin-home gtk::spin-end
          gtk::spin-user-defined gtk::text-search-visible-only
          gtk::text-search-text-only gtk::text-window-private
          gtk::text-window-widget gtk::text-window-text gtk::text-window-left
          gtk::text-window-right gtk::text-window-top gtk::text-window-bottom
          gtk::toolbar-child-space gtk::toolbar-child-button
          gtk::toolbar-child-togglebutton gtk::toolbar-child-radiobutton
          gtk::toolbar-child-widget gtk::tree-view-column-grow-only
          gtk::tree-view-column-autosize gtk::tree-view-column-fixed
          gtk::tree-view-drop-before gtk::tree-view-drop-after
          gtk::tree-view-drop-into-or-before gtk::tree-view-drop-into-or-after
          gtk::Adjustment.lower gtk::Adjustment.upper gtk::Adjustment.value
          gtk::Adjustment.step-increment gtk::Adjustment.page-increment
          gtk::Adjustment.page-size gtk::Style.black.pixel gtk::Style.black.red
          gtk::Style.black.green gtk::Style.black.blue gtk::Style.white.pixel
          gtk::Style.white.red gtk::Style.white.green gtk::Style.white.blue
          gtk::Style.font-desc gtk::Style.xthickness gtk::Style.ythickness
          gtk::Style.fg-gc gtk::Style.bg-gc gtk::Style.light-gc
          gtk::Style.dark-gc gtk::Style.mid-gc gtk::Style.text-gc
          gtk::Style.base-gc gtk::Style.text-aa-gc gtk::Style.black-gc
          gtk::Style.white-gc gtk::Style.bg-pixmap gtk::RcStyle.name
          gtk::RcStyle.font-desc gtk::RcStyle.color-flags
          gtk::RcStyle.xthickness gtk::RcStyle.ythickness gtk::Widget.state
          gtk::Widget.name gtk::Widget.style gtk::Widget.allocation.x
          gtk::Widget.allocation.y gtk::Widget.allocation.width
          gtk::Widget.allocation.height gtk::Widget.window gtk::Widget.parent
          gtk::MenuShell.children gtk::Arrow.arrow-type gtk::Arrow.shadow-type
          gtk::Box.children gtk::Box.spacing gtk::Calendar.month
          gtk::Calendar.year gtk::Calendar.selected-day
          gtk::Calendar.num-marked-dates gtk::Calendar.marked-date
          gtk::Dialog.vbox gtk::Dialog.action-area
          gtk::ColorSelectionDialog.colorsel
          gtk::ColorSelectionDialog.ok-button
          gtk::ColorSelectionDialog.cancel-button
          gtk::ColorSelectionDialog.help-button gtk::Combo.entry
          gtk::Combo.list gtk::FileSelection.dir-list
          gtk::FileSelection.file-list gtk::FileSelection.selection-entry
          gtk::FileSelection.selection-text gtk::FileSelection.main-vbox
          gtk::FileSelection.ok-button gtk::FileSelection.cancel-button
          gtk::FileSelection.help-button gtk::FileSelection.history-pulldown
          gtk::FileSelection.history-menu gtk::FileSelection.history-list
          gtk::FileSelection.fileop-dialog gtk::FileSelection.fileop-entry
          gtk::FileSelection.fileop-file gtk::FileSelection.cmpl-state
          gtk::FileSelection.fileop-c-dir gtk::FileSelection.fileop-del-file
          gtk::FileSelection.fileop-ren-file gtk::FileSelection.button-area
          gtk::FileSelection.action-area gtk::Fixed.children
          gtk::FontSelectionDialog.ok-button
          gtk::FontSelectionDialog.apply-button
          gtk::FontSelectionDialog.cancel-button gtk::HandleBox.shadow-type
          gtk::Layout.bin-window gtk::Table.children gtk::Table.rows
          gtk::Table.cols gtk::accel-group-get-type gtk::accel-group-new
          gtk::accel-group-lock gtk::accel-group-unlock
          gtk::accel-group-connect gtk::accel-group-connect-by-path
          gtk::accel-group-disconnect gtk::accel-group-disconnect-key
          gtk::accel-groups-activate gtk::accel-groups-from-object
          gtk::accel-group-find gtk::accel-group-from-accel-closure
          gtk::accelerator-valid gtk::accelerator-parse gtk::accelerator-name
          gtk::accelerator-set-default-mod-mask
          gtk::accelerator-get-default-mod-mask gtk::object-get-type
          gtk::object-new gtk::object-sink gtk::object-destroy
          gtk::adjustment-get-type gtk::adjustment-new gtk::adjustment-changed
          gtk::adjustment-value-changed gtk::adjustment-clamp-page
          gtk::adjustment-get-value gtk::adjustment-set-value
          gtk::style-get-type gtk::style-new gtk::style-copy gtk::style-attach
          gtk::style-detach gtk::style-set-background
          gtk::style-apply-default-background gtk::style-lookup-icon-set
          gtk::style-render-icon gtk::draw-check gtk::paint-hline
          gtk::paint-vline gtk::paint-shadow gtk::paint-polygon
          gtk::paint-arrow gtk::paint-diamond gtk::paint-box
          gtk::paint-flat-box gtk::paint-check gtk::paint-option gtk::paint-tab
          gtk::paint-shadow-gap gtk::paint-box-gap gtk::paint-extension
          gtk::paint-focus gtk::paint-slider gtk::paint-handle
          gtk::paint-expander gtk::paint-layout gtk::paint-resize-grip
          gtk::border-get-type gtk::border-copy gtk::border-free
          gtk::rc-add-default-file gtk::rc-set-default-files
          gtk::rc-get-default-files gtk::rc-get-style
          gtk::rc-get-style-by-paths gtk::rc-reparse-all-for-settings
          gtk::rc-find-pixmap-in-path gtk::rc-parse gtk::rc-parse-string
          gtk::rc-reparse-all gtk::rc-style-get-type gtk::rc-style-new
          gtk::rc-style-copy gtk::rc-style-ref gtk::rc-style-unref
          gtk::rc-find-module-in-path gtk::rc-get-theme-dir
          gtk::rc-get-module-dir gtk::rc-get-im-module-path
          gtk::rc-get-im-module-file gtk::rc-scanner-new gtk::rc-parse-color
          gtk::rc-parse-state gtk::rc-parse-priority gtk::settings-get-type
          gtk::settings-get-default gtk::settings-install-property
          gtk::settings-install-property-parser gtk::rc-property-parse-color
          gtk::rc-property-parse-enum gtk::rc-property-parse-flags
          gtk::rc-property-parse-requisition gtk::rc-property-parse-border
          gtk::settings-set-property-value gtk::settings-set-string-property
          gtk::settings-set-long-property gtk::settings-set-double-property
          gtk::widget-get-type gtk::widget-new gtk::widget-ref
          gtk::widget-unref gtk::widget-destroy gtk::widget-destroyed
          gtk::widget-set gtk::widget-unparent gtk::widget-show
          gtk::widget-show-now gtk::widget-hide gtk::widget-show-all
          gtk::widget-hide-all gtk::widget-map gtk::widget-unmap
          gtk::widget-realize gtk::widget-unrealize gtk::widget-queue-draw
          gtk::widget-queue-draw-area gtk::widget-queue-resize
          gtk::widget-size-request gtk::widget-size-allocate
          gtk::widget-get-child-requisition gtk::widget-add-accelerator
          gtk::widget-remove-accelerator gtk::widget-set-accel-path
          gtk::widget-list-accel-closures gtk::widget-mnemonic-activate
          gtk::widget-event gtk::widget-send-expose gtk::widget-activate
          gtk::widget-set-scroll-adjustments gtk::widget-reparent
          gtk::widget-intersect gtk::widget-region-intersect
          gtk::widget-freeze-child-notify gtk::widget-child-notify
          gtk::widget-thaw-child-notify gtk::widget-is-focus
          gtk::widget-grab-focus gtk::widget-grab-default gtk::widget-set-name
          gtk::widget-get-name gtk::widget-set-state gtk::widget-set-sensitive
          gtk::widget-set-app-paintable gtk::widget-set-double-buffered
          gtk::widget-set-redraw-on-allocate gtk::widget-set-parent
          gtk::widget-set-parent-window gtk::widget-set-child-visible
          gtk::widget-get-child-visible gtk::widget-get-parent
          gtk::widget-get-parent-window gtk::widget-child-focus
          gtk::widget-set-size-request gtk::widget-get-size-request
          gtk::widget-set-events gtk::widget-add-events
          gtk::widget-set-extension-events gtk::widget-get-extension-events
          gtk::widget-get-toplevel gtk::widget-get-ancestor
          gtk::widget-get-colormap gtk::widget-get-visual
          gtk::widget-get-settings gtk::widget-get-accessible
          gtk::widget-set-colormap gtk::widget-get-events
          gtk::widget-get-pointer gtk::widget-is-ancestor
          gtk::widget-translate-coordinates gtk::widget-hide-on-delete
          gtk::widget-set-style gtk::widget-ensure-style gtk::widget-get-style
          gtk::widget-modify-style gtk::widget-get-modifier-style
          gtk::widget-modify-fg gtk::widget-modify-bg gtk::widget-modify-text
          gtk::widget-modify-base gtk::widget-modify-font
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
          gtk::container-add gtk::container-remove
          gtk::container-set-resize-mode gtk::container-get-resize-mode
          gtk::container-check-resize gtk::container-foreach
          gtk::container-get-children gtk::container-propagate-expose
          gtk::container-set-focus-chain gtk::container-get-focus-chain
          gtk::container-unset-focus-chain
          gtk::container-set-reallocate-redraws gtk::container-set-focus-child
          gtk::container-set-focus-vadjustment
          gtk::container-get-focus-vadjustment
          gtk::container-set-focus-hadjustment
          gtk::container-get-focus-hadjustment gtk::container-resize-children
          gtk::container-child-type gtk::container-class-install-child-property
          gtk::container-class-find-child-property
          gtk::container-class-list-child-properties
          gtk::container-add-with-properties gtk::container-child-set
          gtk::container-child-get gtk::container-child-set-valist
          gtk::container-child-get-valist gtk::container-child-set-property
          gtk::container-child-get-property gtk::container-forall
          gtk::bin-get-type gtk::bin-get-child gtk::window-get-type
          gtk::window-new gtk::window-set-title gtk::window-get-title
          gtk::window-set-wmclass gtk::window-set-role gtk::window-get-role
          gtk::window-add-accel-group gtk::window-remove-accel-group
          gtk::window-set-position gtk::window-activate-focus
          gtk::window-set-focus gtk::window-get-focus gtk::window-set-default
          gtk::window-activate-default gtk::window-set-transient-for
          gtk::window-get-transient-for gtk::window-set-type-hint
          gtk::window-get-type-hint gtk::window-set-destroy-with-parent
          gtk::window-get-destroy-with-parent gtk::window-set-resizable
          gtk::window-get-resizable gtk::window-set-gravity
          gtk::window-get-gravity gtk::window-set-geometry-hints
          gtk::window-set-has-frame gtk::window-get-has-frame
          gtk::window-set-frame-dimensions gtk::window-get-frame-dimensions
          gtk::window-set-decorated gtk::window-get-decorated
          gtk::window-set-icon-list gtk::window-get-icon-list
          gtk::window-set-icon gtk::window-get-icon
          gtk::window-set-default-icon-list gtk::window-get-default-icon-list
          gtk::window-set-modal gtk::window-get-modal
          gtk::window-list-toplevels gtk::window-add-mnemonic
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
          gtk::window-add-embedded-xid gtk::menu-shell-get-type
          gtk::menu-shell-append gtk::menu-shell-prepend gtk::menu-shell-insert
          gtk::menu-shell-deactivate gtk::menu-shell-select-item
          gtk::menu-shell-deselect gtk::menu-shell-activate-item
          gtk::menu-get-type gtk::menu-new gtk::menu-popup gtk::menu-reposition
          gtk::menu-popdown gtk::menu-get-active gtk::menu-set-active
          gtk::menu-set-accel-group gtk::menu-get-accel-group
          gtk::menu-set-accel-path gtk::menu-attach-to-widget gtk::menu-detach
          gtk::menu-get-attach-widget gtk::menu-set-tearoff-state
          gtk::menu-get-tearoff-state gtk::menu-set-title gtk::menu-get-title
          gtk::menu-reorder-child gtk::label-get-type gtk::label-new
          gtk::label-new-with-mnemonic gtk::label-set-text gtk::label-get-text
          gtk::label-set-attributes gtk::label-get-attributes
          gtk::label-set-label gtk::label-get-label gtk::label-set-markup
          gtk::label-set-use-markup gtk::label-get-use-markup
          gtk::label-set-use-underline gtk::label-get-use-underline
          gtk::label-set-markup-with-mnemonic gtk::label-get-mnemonic-keyval
          gtk::label-set-mnemonic-widget gtk::label-get-mnemonic-widget
          gtk::label-set-text-with-mnemonic gtk::label-set-justify
          gtk::label-get-justify gtk::label-set-pattern
          gtk::label-set-line-wrap gtk::label-get-line-wrap
          gtk::label-set-selectable gtk::label-get-selectable
          gtk::label-select-region gtk::label-get-selection-bounds
          gtk::label-get-layout gtk::label-get-layout-offsets
          gtk::accel-label-get-type gtk::accel-label-new
          gtk::accel-label-get-accel-widget gtk::accel-label-get-accel-width
          gtk::accel-label-set-accel-widget gtk::accel-label-set-accel-closure
          gtk::accel-label-refetch gtk::accel-map-lookup-entry
          gtk::accel-map-change-entry gtk::accel-map-load gtk::accel-map-save
          gtk::accel-map-foreach gtk::accel-map-load-fd
          gtk::accel-map-load-scanner gtk::accel-map-save-fd
          gtk::accel-map-add-filter gtk::accel-map-foreach-unfiltered
          gtk::accessible-get-type gtk::accessible-connect-widget-destroyed
          gtk::alignment-get-type gtk::alignment-new gtk::alignment-set
          gtk::frame-get-type gtk::frame-new gtk::frame-set-label
          gtk::frame-get-label gtk::frame-set-label-widget
          gtk::frame-get-label-widget gtk::frame-set-label-align
          gtk::frame-get-label-align gtk::frame-set-shadow-type
          gtk::frame-get-shadow-type gtk::aspect-frame-get-type
          gtk::aspect-frame-new gtk::aspect-frame-set gtk::arrow-get-type
          gtk::arrow-new gtk::arrow-set gtk::binding-set-new
          gtk::binding-set-by-class gtk::binding-set-find
          gtk::bindings-activate gtk::binding-set-activate
          gtk::binding-entry-clear gtk::binding-entry-add-signal
          gtk::binding-set-add-path gtk::binding-entry-remove
          gtk::binding-entry-add-signall gtk::binding-parse-binding
          gtk::box-get-type gtk::box-pack-start gtk::box-pack-end
          gtk::box-set-homogeneous gtk::box-get-homogeneous
          gtk::box-set-spacing gtk::box-get-spacing gtk::box-reorder-child
          gtk::box-query-child-packing gtk::box-set-child-packing
          gtk::button-box-get-type gtk::button-box-get-layout
          gtk::button-box-set-layout gtk::button-box-set-child-secondary
          gtk::button-get-type gtk::button-new gtk::button-new-with-label
          gtk::button-new-from-stock gtk::button-new-with-mnemonic
          gtk::button-pressed gtk::button-released gtk::button-clicked
          gtk::button-enter gtk::button-leave gtk::button-set-relief
          gtk::button-get-relief gtk::button-set-label gtk::button-get-label
          gtk::button-set-use-underline gtk::button-get-use-underline
          gtk::button-set-use-stock gtk::button-get-use-stock
          gtk::calendar-get-type gtk::calendar-new gtk::calendar-select-month
          gtk::calendar-select-day gtk::calendar-mark-day
          gtk::calendar-unmark-day gtk::calendar-clear-marks
          gtk::calendar-display-options gtk::calendar-get-date
          gtk::calendar-freeze gtk::calendar-thaw gtk::cell-editable-get-type
          gtk::cell-editable-start-editing gtk::cell-editable-editing-done
          gtk::cell-editable-remove-widget gtk::cell-renderer-get-type
          gtk::cell-renderer-get-size gtk::cell-renderer-render
          gtk::cell-renderer-activate gtk::cell-renderer-start-editing
          gtk::cell-renderer-set-fixed-size gtk::cell-renderer-get-fixed-size
          gtk::cell-renderer-text-get-type gtk::cell-renderer-text-new
          gtk::cell-renderer-text-set-fixed-height-from-font
          gtk::cell-renderer-toggle-get-type gtk::cell-renderer-toggle-new
          gtk::cell-renderer-toggle-get-radio
          gtk::cell-renderer-toggle-set-radio
          gtk::cell-renderer-toggle-get-active
          gtk::cell-renderer-toggle-set-active
          gtk::cell-renderer-pixbuf-get-type gtk::cell-renderer-pixbuf-new
          gtk::toggle-button-get-type gtk::toggle-button-new
          gtk::toggle-button-new-with-label
          gtk::toggle-button-new-with-mnemonic gtk::toggle-button-set-mode
          gtk::toggle-button-get-mode gtk::toggle-button-set-active
          gtk::toggle-button-get-active gtk::toggle-button-toggled
          gtk::toggle-button-set-inconsistent
          gtk::toggle-button-get-inconsistent gtk::check-button-get-type
          gtk::check-button-new gtk::check-button-new-with-label
          gtk::check-button-new-with-mnemonic gtk::item-get-type
          gtk::item-select gtk::item-deselect gtk::item-toggle
          gtk::menu-item-get-type gtk::menu-item-new
          gtk::menu-item-new-with-label gtk::menu-item-new-with-mnemonic
          gtk::menu-item-set-submenu gtk::menu-item-get-submenu
          gtk::menu-item-remove-submenu gtk::menu-item-select
          gtk::menu-item-deselect gtk::menu-item-activate
          gtk::menu-item-toggle-size-request
          gtk::menu-item-toggle-size-allocate
          gtk::menu-item-set-right-justified gtk::menu-item-get-right-justified
          gtk::menu-item-set-accel-path gtk::check-menu-item-get-type
          gtk::check-menu-item-new gtk::check-menu-item-new-with-label
          gtk::check-menu-item-new-with-mnemonic
          gtk::check-menu-item-set-active gtk::check-menu-item-get-active
          gtk::check-menu-item-toggled gtk::check-menu-item-set-inconsistent
          gtk::check-menu-item-get-inconsistent gtk::target-list-new
          gtk::target-list-ref gtk::target-list-unref gtk::target-list-add
          gtk::target-list-add-table gtk::target-list-remove
          gtk::target-list-find gtk::selection-owner-set
          gtk::selection-add-target gtk::selection-add-targets
          gtk::selection-clear-targets gtk::selection-convert
          gtk::selection-data-set gtk::selection-data-set-text
          gtk::selection-data-get-text gtk::selection-data-get-targets
          gtk::selection-data-targets-include-text gtk::selection-remove-all
          gtk::selection-clear gtk::selection-data-copy
          gtk::selection-data-free gtk::clipboard-set-with-data
          gtk::clipboard-set-with-owner gtk::clipboard-get-owner
          gtk::clipboard-clear gtk::clipboard-set-text
          gtk::clipboard-request-contents gtk::clipboard-request-text
          gtk::clipboard-wait-for-contents gtk::clipboard-wait-for-text
          gtk::clipboard-wait-is-text-available gtk::range-get-type
          gtk::range-set-update-policy gtk::range-get-update-policy
          gtk::range-set-adjustment gtk::range-get-adjustment
          gtk::range-set-inverted gtk::range-get-inverted
          gtk::range-set-increments gtk::range-set-range gtk::range-set-value
          gtk::range-get-value gtk::scrollbar-get-type gtk::hscrollbar-get-type
          gtk::hscrollbar-new gtk::vscrollbar-get-type gtk::vscrollbar-new
          gtk::clist-get-type gtk::clist-set-hadjustment
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
          gtk::clist-get-pixtext gtk::clist-set-foreground
          gtk::clist-set-background gtk::clist-set-cell-style
          gtk::clist-get-cell-style gtk::clist-set-row-style
          gtk::clist-get-row-style gtk::clist-set-shift
          gtk::clist-set-selectable gtk::clist-get-selectable
          gtk::clist-prepend gtk::clist-append gtk::clist-insert
          gtk::clist-remove gtk::clist-set-row-data
          gtk::clist-set-row-data-full gtk::clist-get-row-data
          gtk::clist-find-row-from-data gtk::clist-select-row
          gtk::clist-unselect-row gtk::clist-undo-selection gtk::clist-clear
          gtk::clist-get-selection-info gtk::clist-select-all
          gtk::clist-unselect-all gtk::clist-swap-rows gtk::clist-row-move
          gtk::clist-set-compare-func gtk::clist-set-sort-column
          gtk::clist-set-sort-type gtk::clist-sort gtk::clist-set-auto-sort
          gtk::dialog-get-type gtk::dialog-new gtk::dialog-new-with-buttons
          gtk::dialog-add-action-widget gtk::dialog-add-button
          gtk::dialog-add-buttons gtk::dialog-set-response-sensitive
          gtk::dialog-set-default-response gtk::dialog-set-has-separator
          gtk::dialog-get-has-separator gtk::dialog-response gtk::dialog-run
          gtk::vbox-get-type gtk::vbox-new gtk::color-selection-get-type
          gtk::color-selection-new gtk::color-selection-get-has-opacity-control
          gtk::color-selection-set-has-opacity-control
          gtk::color-selection-get-has-palette
          gtk::color-selection-set-has-palette
          gtk::color-selection-set-current-color
          gtk::color-selection-set-current-alpha
          gtk::color-selection-get-current-color
          gtk::color-selection-get-current-alpha
          gtk::color-selection-set-previous-color
          gtk::color-selection-set-previous-alpha
          gtk::color-selection-get-previous-color
          gtk::color-selection-get-previous-alpha
          gtk::color-selection-is-adjusting
          gtk::color-selection-palette-from-string
          gtk::color-selection-palette-to-string
          gtk::color-selection-set-change-palette-hook
          gtk::color-selection-dialog-get-type gtk::color-selection-dialog-new
          gtk::hbox-get-type gtk::hbox-new gtk::combo-get-type gtk::combo-new
          gtk::combo-set-value-in-list gtk::combo-set-use-arrows
          gtk::combo-set-use-arrows-always gtk::combo-set-case-sensitive
          gtk::combo-set-item-string gtk::combo-set-popdown-strings
          gtk::combo-disable-activate gtk::ctree-get-type
          gtk::ctree-insert-node gtk::ctree-remove-node gtk::ctree-insert-gnode
          gtk::ctree-export-to-gnode gtk::ctree-post-recursive
          gtk::ctree-post-recursive-to-depth gtk::ctree-pre-recursive
          gtk::ctree-pre-recursive-to-depth gtk::ctree-is-viewable
          gtk::ctree-last gtk::ctree-find-node-ptr gtk::ctree-node-nth
          gtk::ctree-find gtk::ctree-is-ancestor gtk::ctree-find-by-row-data
          gtk::ctree-find-all-by-row-data gtk::ctree-find-by-row-data-custom
          gtk::ctree-find-all-by-row-data-custom gtk::ctree-is-hot-spot
          gtk::ctree-move gtk::ctree-expand gtk::ctree-expand-recursive
          gtk::ctree-expand-to-depth gtk::ctree-collapse
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
          gtk::ctree-node-is-visible gtk::ctree-set-indent
          gtk::ctree-set-spacing gtk::ctree-set-show-stub
          gtk::ctree-set-line-style gtk::ctree-set-expander-style
          gtk::ctree-set-drag-compare-func gtk::ctree-sort-node
          gtk::ctree-sort-recursive gtk::ctree-node-get-type
          gtk::drawing-area-get-type gtk::drawing-area-new gtk::curve-get-type
          gtk::curve-new gtk::curve-reset gtk::curve-set-gamma
          gtk::curve-set-range gtk::curve-get-vector gtk::curve-set-vector
          gtk::curve-set-curve-type gtk::drag-get-data gtk::drag-finish
          gtk::drag-get-source-widget gtk::drag-highlight gtk::drag-unhighlight
          gtk::drag-dest-set gtk::drag-dest-set-proxy gtk::drag-dest-unset
          gtk::drag-dest-find-target gtk::drag-dest-get-target-list
          gtk::drag-dest-set-target-list gtk::drag-source-set
          gtk::drag-source-unset gtk::drag-source-set-icon
          gtk::drag-source-set-icon-pixbuf gtk::drag-source-set-icon-stock
          gtk::drag-begin gtk::drag-set-icon-widget gtk::drag-set-icon-pixmap
          gtk::drag-set-icon-pixbuf gtk::drag-set-icon-stock
          gtk::drag-set-icon-default gtk::drag-check-threshold
          gtk::editable-get-type gtk::editable-select-region
          gtk::editable-get-selection-bounds gtk::editable-insert-text
          gtk::editable-delete-text gtk::editable-get-chars
          gtk::editable-cut-clipboard gtk::editable-copy-clipboard
          gtk::editable-paste-clipboard gtk::editable-delete-selection
          gtk::editable-set-position gtk::editable-get-position
          gtk::editable-set-editable gtk::editable-get-editable
          gtk::im-context-get-type gtk::im-context-set-client-window
          gtk::im-context-get-preedit-string gtk::im-context-filter-keypress
          gtk::im-context-focus-in gtk::im-context-focus-out
          gtk::im-context-reset gtk::im-context-set-cursor-location
          gtk::im-context-set-use-preedit gtk::im-context-set-surrounding
          gtk::im-context-get-surrounding gtk::im-context-delete-surrounding
          gtk::entry-get-type gtk::entry-new gtk::entry-set-visibility
          gtk::entry-get-visibility gtk::entry-set-invisible-char
          gtk::entry-get-invisible-char gtk::entry-set-has-frame
          gtk::entry-get-has-frame gtk::entry-set-max-length
          gtk::entry-get-max-length gtk::entry-set-activates-default
          gtk::entry-get-activates-default gtk::entry-set-width-chars
          gtk::entry-get-width-chars gtk::entry-set-text gtk::entry-get-text
          gtk::entry-get-layout gtk::entry-get-layout-offsets
          gtk::entry-new-with-max-length gtk::event-box-get-type
          gtk::event-box-new gtk::file-selection-get-type
          gtk::file-selection-new gtk::file-selection-set-filename
          gtk::file-selection-get-filename gtk::file-selection-complete
          gtk::file-selection-show-fileop-buttons
          gtk::file-selection-hide-fileop-buttons
          gtk::file-selection-get-selections
          gtk::file-selection-set-select-multiple
          gtk::file-selection-get-select-multiple gtk::fixed-get-type
          gtk::fixed-new gtk::fixed-put gtk::fixed-move
          gtk::fixed-set-has-window gtk::fixed-get-has-window
          gtk::font-selection-get-type gtk::font-selection-new
          gtk::font-selection-get-font-name gtk::font-selection-set-font-name
          gtk::font-selection-get-preview-text
          gtk::font-selection-set-preview-text
          gtk::font-selection-dialog-get-type gtk::font-selection-dialog-new
          gtk::font-selection-dialog-get-font-name
          gtk::font-selection-dialog-set-font-name
          gtk::font-selection-dialog-get-preview-text
          gtk::font-selection-dialog-set-preview-text gtk::gamma-curve-get-type
          gtk::gamma-curve-new gtk::gc-get gtk::gc-release
          gtk::handle-box-get-type gtk::handle-box-new
          gtk::handle-box-set-shadow-type gtk::handle-box-get-shadow-type
          gtk::handle-box-set-handle-position
          gtk::handle-box-get-handle-position gtk::handle-box-set-snap-edge
          gtk::handle-box-get-snap-edge gtk::hbutton-box-get-type
          gtk::hbutton-box-new gtk::paned-get-type gtk::paned-add1
          gtk::paned-add2 gtk::paned-pack1 gtk::paned-pack2
          gtk::paned-get-position gtk::paned-set-position
          gtk::paned-compute-position gtk::hpaned-get-type gtk::hpaned-new
          gtk::ruler-get-type gtk::ruler-set-metric gtk::ruler-set-range
          gtk::ruler-draw-ticks gtk::ruler-draw-pos gtk::ruler-get-metric
          gtk::ruler-get-range gtk::hruler-get-type gtk::hruler-new
          gtk::scale-get-type gtk::scale-set-digits gtk::scale-get-digits
          gtk::scale-set-draw-value gtk::scale-get-draw-value
          gtk::scale-set-value-pos gtk::scale-get-value-pos
          gtk::hscale-get-type gtk::hscale-new gtk::hscale-new-with-range
          gtk::separator-get-type gtk::hseparator-get-type gtk::hseparator-new
          gtk::icon-factory-get-type gtk::icon-factory-new
          gtk::icon-factory-add gtk::icon-factory-lookup
          gtk::icon-factory-add-default gtk::icon-factory-remove-default
          gtk::icon-factory-lookup-default gtk::icon-size-lookup
          gtk::icon-size-register gtk::icon-size-register-alias
          gtk::icon-size-from-name gtk::icon-size-get-name gtk::icon-set-new
          gtk::icon-set-new-from-pixbuf gtk::icon-set-ref gtk::icon-set-unref
          gtk::icon-set-copy gtk::icon-set-render-icon gtk::icon-set-add-source
          gtk::icon-set-get-sizes gtk::icon-source-get-type
          gtk::icon-source-new gtk::icon-source-copy gtk::icon-source-free
          gtk::icon-source-set-filename gtk::icon-source-set-pixbuf
          gtk::icon-source-get-filename gtk::icon-source-get-pixbuf
          gtk::icon-source-set-direction-wildcarded
          gtk::icon-source-set-state-wildcarded
          gtk::icon-source-set-size-wildcarded
          gtk::icon-source-get-size-wildcarded
          gtk::icon-source-get-state-wildcarded
          gtk::icon-source-get-direction-wildcarded
          gtk::icon-source-set-direction gtk::icon-source-set-state
          gtk::icon-source-set-size gtk::icon-source-get-direction
          gtk::icon-source-get-state gtk::icon-source-get-size
          gtk::image-get-type gtk::image-new gtk::image-new-from-pixmap
          gtk::image-new-from-image gtk::image-new-from-file
          gtk::image-new-from-pixbuf gtk::image-new-from-stock
          gtk::image-new-from-icon-set gtk::image-new-from-animation
          gtk::image-set-from-pixmap gtk::image-set-from-image
          gtk::image-set-from-file gtk::image-set-from-pixbuf
          gtk::image-set-from-stock gtk::image-set-from-icon-set
          gtk::image-set-from-animation gtk::image-get-storage-type
          gtk::image-get-pixmap gtk::image-get-image gtk::image-get-pixbuf
          gtk::image-get-stock gtk::image-get-icon-set gtk::image-get-animation
          gtk::image-menu-item-get-type gtk::image-menu-item-new
          gtk::image-menu-item-new-with-label
          gtk::image-menu-item-new-with-mnemonic
          gtk::image-menu-item-new-from-stock gtk::image-menu-item-set-image
          gtk::image-menu-item-get-image gtk::im-context-simple-get-type
          gtk::im-context-simple-new gtk::im-context-simple-add-table
          gtk::im-multicontext-get-type gtk::im-multicontext-new
          gtk::im-multicontext-append-menuitems gtk::input-dialog-get-type
          gtk::input-dialog-new gtk::invisible-get-type gtk::invisible-new
          gtk::item-factory-get-type gtk::item-factory-new
          gtk::item-factory-construct gtk::item-factory-add-foreign
          gtk::item-factory-from-widget gtk::item-factory-path-from-widget
          gtk::item-factory-get-item gtk::item-factory-get-widget
          gtk::item-factory-get-widget-by-action
          gtk::item-factory-get-item-by-action gtk::item-factory-create-item
          gtk::item-factory-create-items gtk::item-factory-delete-item
          gtk::item-factory-delete-entry gtk::item-factory-delete-entries
          gtk::item-factory-popup gtk::item-factory-popup-with-data
          gtk::item-factory-popup-data gtk::item-factory-popup-data-from-widget
          gtk::item-factory-set-translate-func gtk::layout-get-type
          gtk::layout-new gtk::layout-put gtk::layout-move gtk::layout-set-size
          gtk::layout-get-size gtk::layout-get-hadjustment
          gtk::layout-get-vadjustment gtk::layout-set-hadjustment
          gtk::layout-set-vadjustment gtk::list-item-get-type
          gtk::list-item-new gtk::list-item-new-with-label
          gtk::list-item-select gtk::list-item-deselect gtk::list-insert-items
          gtk::list-append-items gtk::list-prepend-items gtk::list-remove-items
          gtk::list-clear-items gtk::list-select-item gtk::list-unselect-item
          gtk::list-select-child gtk::list-unselect-child
          gtk::list-child-position gtk::tree-path-new
          gtk::tree-path-new-from-string gtk::tree-path-to-string
          gtk::tree-path-new-first gtk::tree-path-append-index
          gtk::tree-path-prepend-index gtk::tree-path-get-depth
          gtk::tree-path-get-indices gtk::tree-path-free gtk::tree-path-copy
          gtk::tree-path-compare gtk::tree-path-next gtk::tree-path-prev
          gtk::tree-path-up gtk::tree-path-down gtk::tree-path-is-ancestor
          gtk::tree-path-is-descendant gtk::tree-row-reference-new
          gtk::tree-row-reference-new-proxy gtk::tree-row-reference-get-path
          gtk::tree-row-reference-valid gtk::tree-row-reference-free
          gtk::tree-row-reference-inserted gtk::tree-row-reference-deleted
          gtk::tree-row-reference-reordered gtk::tree-iter-copy
          gtk::tree-iter-free gtk::tree-iter-get-type gtk::tree-model-get-type
          gtk::tree-model-get-flags gtk::tree-model-get-n-columns
          gtk::tree-model-get-column-type gtk::tree-model-get-iter
          gtk::tree-model-get-iter-from-string gtk::tree-model-get-iter-first
          gtk::tree-model-get-path gtk::tree-model-get-value
          gtk::tree-model-iter-next gtk::tree-model-iter-children
          gtk::tree-model-iter-has-child gtk::tree-model-iter-n-children
          gtk::tree-model-iter-nth-child gtk::tree-model-iter-parent
          gtk::tree-model-ref-node gtk::tree-model-unref-node
          gtk::tree-model-get gtk::tree-model-get-valist
          gtk::tree-model-foreach gtk::tree-model-row-changed
          gtk::tree-model-row-inserted gtk::tree-model-row-has-child-toggled
          gtk::tree-model-row-deleted gtk::tree-model-rows-reordered
          gtk::tree-sortable-get-type gtk::tree-sortable-sort-column-changed
          gtk::tree-sortable-get-sort-column-id
          gtk::tree-sortable-set-sort-column-id
          gtk::tree-sortable-set-sort-func
          gtk::tree-sortable-set-default-sort-func
          gtk::tree-sortable-has-default-sort-func gtk::list-store-get-type
          gtk::list-store-new gtk::list-store-newv
          gtk::list-store-set-column-types gtk::list-store-set-value
          gtk::list-store-set gtk::list-store-set-valist gtk::list-store-remove
          gtk::list-store-insert gtk::list-store-insert-before
          gtk::list-store-insert-after gtk::list-store-prepend
          gtk::list-store-append gtk::list-store-clear gtk::check-version
          gtk::init gtk::init-check gtk::disable-setlocale gtk::set-locale
          gtk::get-default-language gtk::events-pending gtk::main-do-event
          gtk::main gtk::main-level gtk::main-quit gtk::main-iteration
          gtk::main-iteration-do gtk::true gtk::false gtk::grab-add
          gtk::grab-get-current gtk::grab-remove gtk::init-add
          gtk::quit-add-destroy gtk::quit-add gtk::quit-add-full
          gtk::quit-remove gtk::quit-remove-by-data gtk::timeout-add
          gtk::timeout-add-full gtk::timeout-remove gtk::idle-add
          gtk::idle-add-priority gtk::idle-add-full gtk::idle-remove
          gtk::idle-remove-by-data gtk::input-add-full gtk::input-remove
          gtk::key-snooper-install gtk::key-snooper-remove
          gtk::get-current-event gtk::get-current-event-time
          gtk::get-current-event-state gtk::get-event-widget
          gtk::propagate-event gtk::menu-bar-get-type gtk::menu-bar-new
          gtk::message-dialog-get-type gtk::message-dialog-new
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
          gtk::notebook-query-tab-label-packing
          gtk::notebook-set-tab-label-packing gtk::notebook-reorder-child
          gtk::old-editable-get-type gtk::old-editable-claim-selection
          gtk::old-editable-changed gtk::option-menu-get-type
          gtk::option-menu-new gtk::option-menu-get-menu
          gtk::option-menu-set-menu gtk::option-menu-remove-menu
          gtk::option-menu-get-history gtk::option-menu-set-history
          gtk::pixmap-get-type gtk::pixmap-set gtk::pixmap-get
          gtk::pixmap-set-build-insensitive gtk::socket-get-type
          gtk::socket-new gtk::socket-add-id gtk::socket-get-id
          gtk::plug-get-type gtk::plug-construct gtk::plug-new gtk::plug-get-id
          gtk::preview-get-type gtk::preview-size gtk::preview-put
          gtk::preview-draw-row gtk::preview-set-expand gtk::progress-get-type
          gtk::progress-bar-get-type gtk::progress-bar-new
          gtk::progress-bar-pulse gtk::progress-bar-set-text
          gtk::progress-bar-set-fraction gtk::progress-bar-set-pulse-step
          gtk::progress-bar-set-orientation gtk::progress-bar-get-text
          gtk::progress-bar-get-fraction gtk::progress-bar-get-pulse-step
          gtk::progress-bar-get-orientation gtk::radio-button-get-type
          gtk::radio-button-new gtk::radio-button-new-from-widget
          gtk::radio-button-new-with-label
          gtk::radio-button-new-with-label-from-widget
          gtk::radio-button-new-with-mnemonic
          gtk::radio-button-new-with-mnemonic-from-widget
          gtk::radio-button-get-group gtk::radio-button-set-group
          gtk::radio-menu-item-get-type gtk::radio-menu-item-new
          gtk::radio-menu-item-new-with-label
          gtk::radio-menu-item-new-with-mnemonic gtk::radio-menu-item-get-group
          gtk::radio-menu-item-set-group gtk::viewport-get-type
          gtk::viewport-new gtk::viewport-get-hadjustment
          gtk::viewport-get-vadjustment gtk::viewport-set-hadjustment
          gtk::viewport-set-vadjustment gtk::viewport-set-shadow-type
          gtk::viewport-get-shadow-type gtk::scrolled-window-get-type
          gtk::scrolled-window-new gtk::scrolled-window-set-hadjustment
          gtk::scrolled-window-set-vadjustment
          gtk::scrolled-window-get-hadjustment
          gtk::scrolled-window-get-vadjustment gtk::scrolled-window-set-policy
          gtk::scrolled-window-get-policy gtk::scrolled-window-set-placement
          gtk::scrolled-window-get-placement
          gtk::scrolled-window-set-shadow-type
          gtk::scrolled-window-get-shadow-type
          gtk::scrolled-window-add-with-viewport
          gtk::separator-menu-item-get-type gtk::separator-menu-item-new
          gtk::size-group-get-type gtk::size-group-new gtk::size-group-set-mode
          gtk::size-group-get-mode gtk::size-group-add-widget
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
          gtk::spin-button-spin gtk::spin-button-set-wrap
          gtk::spin-button-get-wrap gtk::spin-button-set-snap-to-ticks
          gtk::spin-button-get-snap-to-ticks gtk::spin-button-update
          gtk::stock-add gtk::stock-add-static gtk::stock-lookup
          gtk::stock-list-ids gtk::stock-item-copy gtk::stock-item-free
          gtk::statusbar-get-type gtk::statusbar-new
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
          gtk::text-tag-set-priority gtk::text-tag-event
          gtk::text-attributes-new gtk::text-attributes-copy
          gtk::text-attributes-copy-values gtk::text-attributes-unref
          gtk::text-attributes-ref gtk::text-attributes-get-type
          gtk::text-tag-table-get-type gtk::text-tag-table-new
          gtk::text-tag-table-add gtk::text-tag-table-remove
          gtk::text-tag-table-lookup gtk::text-tag-table-foreach
          gtk::text-tag-table-get-size gtk::text-child-anchor-get-type
          gtk::text-child-anchor-new gtk::text-child-anchor-get-widgets
          gtk::text-child-anchor-get-deleted gtk::text-iter-get-buffer
          gtk::text-iter-copy gtk::text-iter-free gtk::text-iter-get-type
          gtk::text-iter-get-offset gtk::text-iter-get-line
          gtk::text-iter-get-line-offset gtk::text-iter-get-line-index
          gtk::text-iter-get-visible-line-offset
          gtk::text-iter-get-visible-line-index gtk::text-iter-get-char
          gtk::text-iter-get-slice gtk::text-iter-get-text
          gtk::text-iter-get-visible-slice gtk::text-iter-get-visible-text
          gtk::text-iter-get-pixbuf gtk::text-iter-get-marks
          gtk::text-iter-get-child-anchor gtk::text-iter-get-toggled-tags
          gtk::text-iter-begins-tag gtk::text-iter-ends-tag
          gtk::text-iter-toggles-tag gtk::text-iter-has-tag
          gtk::text-iter-get-tags gtk::text-iter-editable
          gtk::text-iter-can-insert gtk::text-iter-starts-word
          gtk::text-iter-ends-word gtk::text-iter-inside-word
          gtk::text-iter-starts-sentence gtk::text-iter-ends-sentence
          gtk::text-iter-inside-sentence gtk::text-iter-starts-line
          gtk::text-iter-ends-line gtk::text-iter-is-cursor-position
          gtk::text-iter-get-chars-in-line gtk::text-iter-get-bytes-in-line
          gtk::text-iter-get-attributes gtk::text-iter-get-language
          gtk::text-iter-is-end gtk::text-iter-is-start
          gtk::text-iter-forward-char gtk::text-iter-backward-char
          gtk::text-iter-forward-chars gtk::text-iter-backward-chars
          gtk::text-iter-forward-line gtk::text-iter-backward-line
          gtk::text-iter-forward-lines gtk::text-iter-backward-lines
          gtk::text-iter-forward-word-end gtk::text-iter-backward-word-start
          gtk::text-iter-forward-word-ends gtk::text-iter-backward-word-starts
          gtk::text-iter-forward-sentence-end
          gtk::text-iter-backward-sentence-start
          gtk::text-iter-forward-sentence-ends
          gtk::text-iter-backward-sentence-starts
          gtk::text-iter-forward-cursor-position
          gtk::text-iter-backward-cursor-position
          gtk::text-iter-forward-cursor-positions
          gtk::text-iter-backward-cursor-positions gtk::text-iter-set-offset
          gtk::text-iter-set-line gtk::text-iter-set-line-offset
          gtk::text-iter-set-line-index gtk::text-iter-forward-to-end
          gtk::text-iter-forward-to-line-end
          gtk::text-iter-set-visible-line-offset
          gtk::text-iter-set-visible-line-index
          gtk::text-iter-forward-to-tag-toggle
          gtk::text-iter-backward-to-tag-toggle
          gtk::text-iter-forward-find-char gtk::text-iter-backward-find-char
          gtk::text-iter-forward-search gtk::text-iter-backward-search
          gtk::text-iter-equal gtk::text-iter-compare gtk::text-iter-in-range
          gtk::text-iter-order gtk::text-mark-get-type
          gtk::text-mark-set-visible gtk::text-mark-get-visible
          gtk::text-mark-get-name gtk::text-mark-get-deleted
          gtk::text-mark-get-buffer gtk::text-mark-get-left-gravity
          gtk::text-buffer-get-type gtk::text-buffer-new
          gtk::text-buffer-get-line-count gtk::text-buffer-get-char-count
          gtk::text-buffer-get-tag-table gtk::text-buffer-set-text
          gtk::text-buffer-insert gtk::text-buffer-insert-at-cursor
          gtk::text-buffer-insert-interactive
          gtk::text-buffer-insert-interactive-at-cursor
          gtk::text-buffer-insert-range
          gtk::text-buffer-insert-range-interactive
          gtk::text-buffer-insert-with-tags
          gtk::text-buffer-insert-with-tags-by-name gtk::text-buffer-delete
          gtk::text-buffer-delete-interactive gtk::text-buffer-get-text
          gtk::text-buffer-get-slice gtk::text-buffer-insert-pixbuf
          gtk::text-buffer-insert-child-anchor
          gtk::text-buffer-create-child-anchor gtk::text-buffer-create-mark
          gtk::text-buffer-move-mark gtk::text-buffer-delete-mark
          gtk::text-buffer-get-mark gtk::text-buffer-move-mark-by-name
          gtk::text-buffer-delete-mark-by-name gtk::text-buffer-get-insert
          gtk::text-buffer-get-selection-bound gtk::text-buffer-place-cursor
          gtk::text-buffer-apply-tag gtk::text-buffer-remove-tag
          gtk::text-buffer-apply-tag-by-name
          gtk::text-buffer-remove-tag-by-name gtk::text-buffer-remove-all-tags
          gtk::text-buffer-create-tag gtk::text-buffer-get-iter-at-line-offset
          gtk::text-buffer-get-iter-at-line-index
          gtk::text-buffer-get-iter-at-offset gtk::text-buffer-get-iter-at-line
          gtk::text-buffer-get-start-iter gtk::text-buffer-get-end-iter
          gtk::text-buffer-get-bounds gtk::text-buffer-get-iter-at-mark
          gtk::text-buffer-get-iter-at-child-anchor
          gtk::text-buffer-get-modified gtk::text-buffer-set-modified
          gtk::text-buffer-add-selection-clipboard
          gtk::text-buffer-remove-selection-clipboard
          gtk::text-buffer-cut-clipboard gtk::text-buffer-copy-clipboard
          gtk::text-buffer-paste-clipboard
          gtk::text-buffer-get-selection-bounds
          gtk::text-buffer-delete-selection gtk::text-buffer-begin-user-action
          gtk::text-buffer-end-user-action gtk::text-view-get-type
          gtk::text-view-new gtk::text-view-new-with-buffer
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
          gtk::text-view-get-border-window-size
          gtk::text-view-forward-display-line
          gtk::text-view-backward-display-line
          gtk::text-view-forward-display-line-end
          gtk::text-view-backward-display-line-start
          gtk::text-view-starts-display-line gtk::text-view-move-visually
          gtk::text-view-add-child-at-anchor gtk::text-view-add-child-in-window
          gtk::text-view-move-child gtk::text-view-set-wrap-mode
          gtk::text-view-get-wrap-mode gtk::text-view-set-editable
          gtk::text-view-get-editable gtk::text-view-set-pixels-above-lines
          gtk::text-view-get-pixels-above-lines
          gtk::text-view-set-pixels-below-lines
          gtk::text-view-get-pixels-below-lines
          gtk::text-view-set-pixels-inside-wrap
          gtk::text-view-get-pixels-inside-wrap
          gtk::text-view-set-justification gtk::text-view-get-justification
          gtk::text-view-set-left-margin gtk::text-view-get-left-margin
          gtk::text-view-set-right-margin gtk::text-view-get-right-margin
          gtk::text-view-set-indent gtk::text-view-get-indent
          gtk::text-view-set-tabs gtk::text-view-get-tabs
          gtk::text-view-get-default-attributes gtk::tips-query-get-type
          gtk::tooltips-get-type gtk::tooltips-new gtk::tooltips-enable
          gtk::tooltips-disable gtk::tooltips-set-tip gtk::tooltips-data-get
          gtk::tooltips-force-window gtk::toolbar-get-type gtk::toolbar-new
          gtk::toolbar-append-item gtk::toolbar-prepend-item
          gtk::toolbar-insert-item gtk::toolbar-insert-stock
          gtk::toolbar-append-space gtk::toolbar-prepend-space
          gtk::toolbar-insert-space gtk::toolbar-remove-space
          gtk::toolbar-append-element gtk::toolbar-prepend-element
          gtk::toolbar-insert-element gtk::toolbar-append-widget
          gtk::toolbar-prepend-widget gtk::toolbar-insert-widget
          gtk::toolbar-set-orientation gtk::toolbar-set-style
          gtk::toolbar-set-icon-size gtk::toolbar-set-tooltips
          gtk::toolbar-unset-style gtk::toolbar-unset-icon-size
          gtk::toolbar-get-orientation gtk::toolbar-get-style
          gtk::toolbar-get-icon-size gtk::toolbar-get-tooltips
          gtk::tree-drag-source-get-type gtk::tree-drag-source-row-draggable
          gtk::tree-drag-source-drag-data-delete
          gtk::tree-drag-source-drag-data-get gtk::tree-drag-dest-get-type
          gtk::tree-drag-dest-drag-data-received
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
          gtk::tree-view-column-add-attribute
          gtk::tree-view-column-set-attributes
          gtk::tree-view-column-set-cell-data-func
          gtk::tree-view-column-clear-attributes
          gtk::tree-view-column-set-spacing gtk::tree-view-column-get-spacing
          gtk::tree-view-column-set-visible gtk::tree-view-column-get-visible
          gtk::tree-view-column-set-resizable
          gtk::tree-view-column-get-resizable gtk::tree-view-column-set-sizing
          gtk::tree-view-column-get-sizing gtk::tree-view-column-get-width
          gtk::tree-view-column-get-fixed-width
          gtk::tree-view-column-set-fixed-width
          gtk::tree-view-column-set-min-width
          gtk::tree-view-column-get-min-width
          gtk::tree-view-column-set-max-width
          gtk::tree-view-column-get-max-width gtk::tree-view-column-clicked
          gtk::tree-view-column-set-title gtk::tree-view-column-get-title
          gtk::tree-view-column-set-clickable
          gtk::tree-view-column-get-clickable gtk::tree-view-column-set-widget
          gtk::tree-view-column-get-widget gtk::tree-view-column-set-alignment
          gtk::tree-view-column-get-alignment
          gtk::tree-view-column-set-reorderable
          gtk::tree-view-column-get-reorderable
          gtk::tree-view-column-set-sort-column-id
          gtk::tree-view-column-get-sort-column-id
          gtk::tree-view-column-set-sort-indicator
          gtk::tree-view-column-get-sort-indicator
          gtk::tree-view-column-set-sort-order
          gtk::tree-view-column-get-sort-order
          gtk::tree-view-column-cell-set-cell-data
          gtk::tree-view-column-cell-get-size
          gtk::tree-view-column-cell-is-visible gtk::tree-view-get-type
          gtk::tree-view-new gtk::tree-view-new-with-model
          gtk::tree-view-get-model gtk::tree-view-set-model
          gtk::tree-view-get-selection gtk::tree-view-get-hadjustment
          gtk::tree-view-set-hadjustment gtk::tree-view-get-vadjustment
          gtk::tree-view-set-vadjustment gtk::tree-view-get-headers-visible
          gtk::tree-view-set-headers-visible gtk::tree-view-columns-autosize
          gtk::tree-view-set-headers-clickable gtk::tree-view-set-rules-hint
          gtk::tree-view-get-rules-hint gtk::tree-view-append-column
          gtk::tree-view-remove-column gtk::tree-view-insert-column
          gtk::tree-view-insert-column-with-attributes
          gtk::tree-view-insert-column-with-data-func gtk::tree-view-get-column
          gtk::tree-view-get-columns gtk::tree-view-move-column-after
          gtk::tree-view-set-expander-column gtk::tree-view-get-expander-column
          gtk::tree-view-set-column-drag-function
          gtk::tree-view-scroll-to-point gtk::tree-view-scroll-to-cell
          gtk::tree-view-row-activated gtk::tree-view-expand-all
          gtk::tree-view-collapse-all gtk::tree-view-expand-row
          gtk::tree-view-collapse-row gtk::tree-view-map-expanded-rows
          gtk::tree-view-row-expanded gtk::tree-view-set-reorderable
          gtk::tree-view-get-reorderable gtk::tree-view-set-cursor
          gtk::tree-view-get-cursor gtk::tree-view-get-bin-window
          gtk::tree-view-get-path-at-pos gtk::tree-view-get-cell-area
          gtk::tree-view-get-background-area gtk::tree-view-get-visible-rect
          gtk::tree-view-widget-to-tree-coords
          gtk::tree-view-tree-to-widget-coords
          gtk::tree-view-enable-model-drag-source
          gtk::tree-view-enable-model-drag-dest
          gtk::tree-view-unset-rows-drag-source
          gtk::tree-view-unset-rows-drag-dest gtk::tree-view-set-drag-dest-row
          gtk::tree-view-get-drag-dest-row gtk::tree-view-get-dest-row-at-pos
          gtk::tree-view-create-row-drag-icon gtk::tree-view-set-enable-search
          gtk::tree-view-get-enable-search gtk::tree-view-get-search-column
          gtk::tree-view-set-search-column gtk::tree-view-get-search-equal-func
          gtk::tree-view-set-search-equal-func
          gtk::tree-view-set-destroy-count-func gtk::tree-selection-get-type
          gtk::tree-selection-set-mode gtk::tree-selection-get-mode
          gtk::tree-selection-set-select-function
          gtk::tree-selection-get-user-data gtk::tree-selection-get-tree-view
          gtk::tree-selection-get-selected gtk::tree-selection-selected-foreach
          gtk::tree-selection-select-path gtk::tree-selection-unselect-path
          gtk::tree-selection-select-iter gtk::tree-selection-unselect-iter
          gtk::tree-selection-path-is-selected
          gtk::tree-selection-iter-is-selected gtk::tree-selection-select-all
          gtk::tree-selection-unselect-all gtk::tree-selection-select-range
          gtk::tree-store-get-type gtk::tree-store-new gtk::tree-store-newv
          gtk::tree-store-set-column-types gtk::tree-store-set-value
          gtk::tree-store-set gtk::tree-store-set-valist gtk::tree-store-remove
          gtk::tree-store-insert gtk::tree-store-insert-before
          gtk::tree-store-insert-after gtk::tree-store-prepend
          gtk::tree-store-append gtk::tree-store-is-ancestor
          gtk::tree-store-iter-depth gtk::tree-store-clear
          gtk::vbutton-box-get-type gtk::vbutton-box-new gtk::vpaned-get-type
          gtk::vpaned-new gtk::vruler-get-type gtk::vruler-new
          gtk::vscale-get-type gtk::vscale-new gtk::vscale-new-with-range
          gtk::vseparator-get-type gtk::vseparator-new)
        :gtk)

(pushnew ':gtk *features*)

;;; end generated output

