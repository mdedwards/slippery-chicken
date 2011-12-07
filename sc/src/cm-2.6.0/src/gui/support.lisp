;;; **********************************************************************
;;; Copyright (C) 2004 Heinrich Taube (taube@uiuc.edu) 
;;; 
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation; either version 2
;;; of the License, or (at your option) any later version.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; 
;;; **********************************************************************

;;; $Name: rel-2_6_0 $
;;; $Revision: 1.19 $
;;; $Date: 2005/02/01 22:30:46 $

(in-package :cm)

;;;
;;; this file implements common, low-level gui functionalty

(defparameter *cm-break-on-errors* nil)

(defmacro error-block (&body body)
  `(block :error ,@body))

(defmacro error-abort () `(return-from :error nil))

(defmacro without-error-breaks (&body body)
  `(error-block
     (handler-bind ((t #'(lambda (c)
                           (return-from :error (values nil c)))))
       ,@body)))

(defun safecall (fn &rest args)
  (if *cm-break-on-errors*
    (values (apply fn args) nil)        ; second value must be nil
    (without-error-breaks (apply fn args))))

(defun safeapply (fn args)
  (if *cm-break-on-errors*
    (values (apply fn args) nil)        ; second value must be nil
    (without-error-breaks (apply fn args))))

(defun safeeval (expr)
  (if *cm-break-on-errors*
    (values (eval expr) nil)            ; second value must be nil
    (without-error-breaks (eval expr))))

(defun entry-expr (entry &rest args)
  (apply #'string->expr (gtk:entry-get-text entry) args))

;;
;; GTK starting, stopping and window management
;;

#+darwin
(progn
  (defvar x11 nil)
  (defun darwin-x11-running? (&optional (verbose t))
    (or x11
        (let ((s #+openmcl
                (ccl:external-process-output-stream
                 (ccl:run-program "/bin/ps" '("-xco command") 
                                  :wait t :output ':stream))
                #+sbcl
                (sb-ext:process-output
                 (sb-ext:run-program "/bin/ps" '("-xco command") 
                                     :output :stream))))
          (do ((l (read-line s nil) (read-line s nil))
               (f nil))
              ((or (null l) f)
               (setq x11 f)
               (when (and verbose (not f))
                 (format t "Can't open window because X11 is not running."))
               f)
            (setq f (equal l "X11")))))))

;;;
;;; GTK main loop starting and stopping. We try to avoid calling
;;; gtk_main directly at all costs because it blocks all input/output
;;; in the REPL while it is running. In CMU we run a non-blocking
;;; substitute as a polling function (This advice came from
;;; cphchi-square-works.com, who pointed to the clg sources as an
;;; example, thank you!) For some reason the polling method doesn't
;;; work in OpenMCL even though there is an example of its use in
;;; ccl/library/openmcl-gtk-support.lisp. So for OpenMCL we run the
;;; non-blocking gtkloop in its own process and then "eval" new open
;;; window requests from the REPL process via a process lock.
;;;

(defparameter *gtk-main* nil)

;;#+SBCL
;;(defun gtkloop ()
;;  (let ((*package* (find-package :cm))
;;        (*readtable* cm::*cm-readtable*))
;;    (gtk:main)))

#+openmcl
(progn
  (defvar *gtk-call* nil)
  (defvar .gtk-call. (ccl:make-lock '*gtk-call*))
  (defun gtk-call (fn)
    (ccl:with-lock-grabbed (.gtk-call.) (setq *gtk-call* fn)))
  (defun gtkloop ()
    (let ((*package* (find-package :cm))
          (*readtable* cm::*cm-readtable*))
      (loop doing
           (if NIL ;;(eql (gtk:events-pending) 0)
             (ccl:process-allow-schedule)
             (progn
               (ccl:with-lock-grabbed (.gtk-call.)
                 (when *gtk-call*
                   (funcall *gtk-call*)
                   (setq *gtk-call* NIL)))
               (gtk:main-iteration-do nil)
               ))))))

;; Sadly this does not work in OpenMCL and I don't know why.
;;(ccl::def-load-pointers gtk-main ()
;;  (ccl::%install-periodic-task 'gtk-main
;;			  #'(lambda ()
;;			      (do ()
;;                                  ((eql (gtk:events-pending) 0))
;;                                (gtk:main-iteration-do nil)))
;;                          10))

;;;
;;; In CMU and SBCL we keep the Lisp REPL active by running gtkloop as
;;; a non-blocking polling function.

#+(or cmu sbcl)
(defun gtkloop ()
  (do ()
      ((eql 0 (gtk:events-pending)) )
    (gtk:main-iteration-do nil)))

#+cmu
(progn
  (defvar gtk-polling-save
    (vector lisp::*periodic-polling-function*
            lisp::*max-event-to-sec*
            lisp::*max-event-to-usec*))
  (defun gtk-polling-start ()
    (setq lisp::*periodic-polling-function* #'gtkloop)
    (setq lisp::*max-event-to-sec* 0)
    (setq lisp::*max-event-to-usec* 1000)
    )
  (defun gtk-polling-stop ()
    (setq lisp::*periodic-polling-function* (elt gtk-polling-save 0))
    (setq lisp::*max-event-to-sec* (elt gtk-polling-save 1))
    (setq lisp::*max-event-to-usec* (elt gtk-polling-save 1)))
  )

#+sbcl
(progn
  (defvar gtk-polling-save
    (vector sb-impl::*periodic-polling-function*
            sb-impl::*max-event-to-sec*
            sb-impl::*max-event-to-usec*))
  (defun gtk-polling-start ()
    (setq sb-impl::*periodic-polling-function* #'gtkloop)
    (setq sb-impl::*max-event-to-sec* 0)
    (setq sb-impl::*max-event-to-usec* 1000))
  (defun gtk-polling-stop ()
    (setq sb-impl::*periodic-polling-function* (elt gtk-polling-save 0))
    (setq sb-impl::*max-event-to-sec* (elt gtk-polling-save 1))
    (setq sb-impl::*max-event-to-usec* (elt gtk-polling-save 1)))
  )

(defun gtk-main-start ()
  (when (not *gtk-main*)
    (setq *gtk-main* t)
    #+openmcl (ccl:process-run-function "gtk_main" #'gtkloop)
    #+cmu (gtk-polling-start)
    #+sbcl (gtk-polling-start)
    ))

(defun gtk-main-stop ()
  ;; In OpenMCL we dont the GTK main process even if there are no
  ;; window open. I don't know if this is a good idea or not.
  #-openmcl (setq *gtk-main* NIL)
  #+cmu (gtk-polling-stop)
  #+sbcl (gtk-polling-stop)
  )

(defun gtk-running? () *gtk-main*)

;;;
;;; Toplevel window mananagement, when last window closes then call
;;; gtk-main-stop. Variable *gtk-open-toplevels* holds the list of
;;; _CLOS_ objects representing open toplevel windows. (I dont use
;;; window pointers because these are not necessarily eq across ffi
;;; calls).
;;;

(defparameter *gtk-open-toplevels* ())

(defun gtk-remove-toplevel (object) 
  ;; object is a clos instance so we can eq'ness to find it.
  (setq *gtk-open-toplevels* (remove object *gtk-open-toplevels*))
  (values))

(defun gtk-open-toplevels? () (not (null *gtk-open-toplevels*)))

;;; iterator for GLists
;;(defmacro doGList ((x gl &optional e) &body body)
;;  (let ((var (gensym)))
;;    `(do ((, x)
;;          (,var ,gl (GList.next ,var)))
;;      ((g:nullptr? ,var) , e)
;;      (setq ,x (GList.data ,var))
;;      ,@body)))

;;;
;;; widget->object maps gtk widgets to lisp objects that represent
;;; them in Lisp.  All plotter callbacks use this function to quicklu
;;; access plotter stuctures given a pointer to the gtk window.  To
;;; associate a lisp object with a widget use (setf widget->object)
;;; and to remove a widget association from the hashtable specify nil
;;; as te value of the setf.
;;;

(defun ptr->int (ptr)
  #+:openmcl (ccl::%ptr-to-int ptr)
  #+:cmu (system:sap-int (alien:alien-sap ptr))
  #+:sbcl (sb-sys:sap-int (sb-alien:alien-sap ptr)))

(defun int->ptr (int)
  #+:openmcl (ccl::%int-to-ptr int)
  #+:cmu (sap-alien (system:int-sap int))
  #+:sbcl (sap-alien (sb-sys:int-sap int)))

(defparameter *widgets* (make-hash-table))

(defun widget->object (w &optional (err t))
  ;; alien pointer change across lisp entries! so we cant hash the
  ;; pointer itself, but only its int.
  (multiple-value-bind (val flg)
      (gethash (ptr->int w) 
               ;#+:openmcl (ccl::%ptr-to-int w)
               ;#+:cmu (system:sap-int (alien:alien-sap w))
               ;#+:sbcl (sb-sys:sap-int (sb-alien:alien-sap w))
               *widgets*)
    (if flg
      val
      (if err (error "No object for ~S. " w) 
          nil))))

(defun (setf widget->object) (o w)
  (setf (gethash (ptr->int w)
                 ;#+:openmcl (ccl::%ptr-to-int w)
                 ;#+:cmu (system:sap-int (alien:alien-sap w))
                 ;#+:sbcl (sb-sys:sap-int (sb-alien:alien-sap w)) 
                 *widgets*)
        o))

(defun mapwidgets (fn)
  ;; coerce widgets back to pointer and call function.
  (maphash (lambda (k v)
             (funcall fn
                      #+openmcl (int->ptr k)
                      #+(or cmu sbcl)  (int->ptr v)
                      ;#+:openmcl (ccl::%int-to-ptr k)
                      ;#+:sbcl (sap-alien (sb-sys:int-sap v))
                      ;#+:cmu (sap-alien (system:int-sap v))
                      v))
           *widgets*))

(defun prinwidgets ()
  (maphash (lambda (a b) (format t "key=~S value=~S~%" a b))
           *widgets*))

(defun remove-widget (w)
  (remhash (ptr->int w)
           ;#+:openmcl (ccl::%ptr-to-int w)
           ;#+:cmu (system:sap-int (alien:alien-sap w))
           ;#+:sbcl (sb-sys:sap-int (sb-alien:alien-sap w))
           *widgets*))

;;;
;;; widget-property and (setf widget-property) associate lisp property
;;; lists with gtk widgets. currently all the tool windows use this to
;;; cache/access their data sets.
;;;

(defun widget-property (widget name)
  (getf (widget->object widget) name))

(defun (setf widget-property) (data widget name)
  (setf (getf (widget->object widget) name) data))

;;; Menus....  all the GTK menu examples ive looked at create menu
;;; structures in a top-down manner: parent widgets are first
;;; allocated and then passed into routines that add children.  this
;;; is different than the bottoms-up approach that Lisp normally
;;; takes, where menuitems are first created and the passed as args to
;;; the functions that create their menus. but whatever!
;;;
;;; To define menu structure make a list of the format:
;;; (menu {name | (name &key props)} {item}*)
;;; where  name is a string and props are:
;;;   :right bool         if true menu is right justified
;;;   :constructor func   call func to create items
;;;   :selector callback  make callback the "activate" handler
;;; following the name comes zero or more item for the menu.
;;; To define a menu item make a list of the format:
;;; (item {name | (name &key props)} {callback}+ {data}+)
;;; where name is a string (or NIL for a seperator),
;;; and props are:
;;;   :insensitive  if true item is initially unselectable
;;;   :check        if true item is a checkbox menuitem
;;;   :radio        if true item is a radio menuitem
;;; callback is the "activate" or "toggled" callback as
;;; appropriate to the item and data is the user data to
;;; pass to the callback. See the end of this file for an
;;; example.

(defun create-menubar (menus &optional gdata box expand (fill t)
                             (padding 0))
  ;; menus is a list of menu specifications, gdata is a default datum
  ;; to pass to the menu callbacks as user_data.  if box is specified
  ;; the menubar is packed into to it according to the other args.
  (unless gdata (setf gdata (g:nullptr)))
  (let ((mbar (gtk:menu-bar-new)))
    (when box (gtk:box-pack-start box mbar expand fill padding))
    (gtk:widget-show mbar)
    (dolist (spec menus)
      (if (and (consp spec)
               (eql (car spec) 'menu))
        (create-menu spec mbar gdata)
        (error "Expected menu definition but got ~S instead."
               spec)))
    mbar))

(defun create-menu (spec shell gdata)
  ;; window is main window, shell is the parent menu/menubar object
  ;; (NOT the item...) and spec is a list: (MENU name ...)
  (pop spec);; remove MENU symbol
  (let ((name (pop spec))
        make just root menu call)
    (when (consp name)
      (setf just (getf (cdr name) ':right))
      (setf make (getf (cdr name) ':constructor))
      (setf call (getf (cdr name) ':selector))
      (setf name (pop name)))
    (setq root (gtk:menu-item-new-with-label name))
    (if just (gtk:menu-item-set-right-justified root t))
    ;; mbar is nil if its a submenu...
    (gtk:menu-shell-append shell root)
    (gtk:widget-show root)
    ;; now create actual menu and menu items.
    (setq menu (gtk:menu-new ))
    (gtk:menu-item-set-submenu root menu)
    ;; do NOT show menu.
    (GTK:WIDGET-SHOW MENU);; im not sure...
    ;; process items, pass in group for radio items.  if a constructor
    ;; is specfied then it creates/adds the items to the menu
    (if call
      (g:signal-connect root "select" call gdata))
    (if make
      (funcall make MENU gdata)
      (let ((group (g:nullptr))
            type item subm)
        (dolist (i spec)
          (case (car i)
            (ITEM
             (multiple-value-setq (item type)
               (create-menu-item i menu group gdata))
             (setq group (if (eq type ':radio)
                           (gtk:radio-menu-item-get-group item)
                           (g:nullptr))))
            (MENU
             (setq group (g:nullptr))
             (multiple-value-setq (item subm)
               (create-menu i MENU gdata))
             item subm)))))
    ;; root is the menu item holding the menu's name, menu is the
    ;; menushell this function created and packed with items and
    ;; submenus.
    (values root menu)))

(defun create-menu-item (spec shell group gdata)
  ;; gdata is main gdata, shell is parent menu, group is the current
  ;; radio group or NULL, and spec is the item's definition list:
  ;; (item name func data ...)
  (pop spec) ;; remove 'item symbol
  (let ((name (pop spec))  ;; spec are not item props
        (args '())
        item type func data)
    (when (consp name)
      (setf args (cdr name))
      (setf name (car name)))
    ;; spec=(func data ...)
    (setq func (if (consp spec) (pop spec) nil))
    (setq data (if (consp spec) (pop spec) gdata))
    (cond ((null name) ;; (item nil) = seperator
           (setq item (gtk:menu-item-new))
           (setq type :separator))
          ((getf args ':check)
           (setq type :check)
           (setq item (gtk:check-menu-item-new-with-label name))
           (when (eq (getf args ':check) ':checked)
             (gtk:check-menu-item-set-active item t))
           (when func
             (g:signal-connect-swapped item "toggled" func
                                       data)))
          ((getf args ':radio)
           (setq type :radio)
           (setq item (gtk:radio-menu-item-new-with-label group name))
           (when (eq (getf args ':radio) ':checked)
             (gtk:check-menu-item-set-active item t))
           (when func
             (g:signal-connect-swapped item "toggled" func
                                       data)))
          (t
           (setq item (gtk:menu-item-new-with-label name))
           (setq type :item)
           (when func
             (g:signal-connect-swapped item "activate" func
                                       (or data gdata)))))
    (if (getf args ':insensitive)
      (gtk:widget-set-sensitive item nil))      
    ;; add item to menu and show
    (gtk:menu-shell-append shell item)
    (gtk:widget-show item)
    (values item type)))

(defun menu-item-text (item)
  ;; return the text of a menu item.
  (let ((lab (gtk:bin-get-child item)))
    (if (not (g:nullptr? lab))
      (gtk:label-get-text lab)
      nil)))

(defun create-option-menu (strings &key changed data menu select)
  (let ((shell (gtk:menu-new))
        item)
    (loop for s in strings
       for i from 0
       do (setq item (gtk:menu-item-new-with-label s))
       (gtk:menu-shell-append shell item)
       ;;(if (eql i select) (gtk:menu-shell-select-item shell item))
       (gtk:widget-show item))
    (if menu
      (gtk:option-menu-remove-menu menu)
      (setq menu  (gtk:option-menu-new)))
    (gtk:option-menu-set-menu menu shell)
    (gtk:widget-show shell)
    ;; connect to the changed signal handler, if specified
    (when select
      (gtk:option-menu-set-history menu select))
    (when changed
      (g:signal-connect menu "changed" changed
                        (or data (g:nullptr))))
    menu))

(defun entry-value (entry &key (error-value ':error)
                    (null-value nil nvp) 
                    (null-text "<value>")
                    test (test-is-filter))
  (let ((str (gtk:entry-get-text entry)))
    (if (eql ':err (safe-read? str))
      (if (equal str "")
        (if nvp
          null-value
          (progn
            (when null-text
              (gtk:entry-set-text entry null-text)
              (gtk:editable-select-region entry 0 -1))
            error-value))
        (progn 
          (gtk:editable-select-region entry 0 -1)
          error-value))
      (if (not test)
        (read-from-string str)
        (let* ((val (read-from-string str))
               (flg (funcall test val)))
          (if flg (if test-is-filter flg val)
              error-value))))))

#|
;; test pointer eq-ness to see if widget->objects works...
(gtk:init 0 (g:nullptr))
(defparameter win nil)
(gtk:define-signal-handler delev :int (widget event data)
  widget event data
  gtk:+false+)
(gtk:define-signal-handler domsg :void (widget data)
  (let ((obj (widget->object data nil)))
    (format t "lisp object=~S~%" obj)))
(gtk:define-signal-handler bye :void (widget data)
  widget data
  (print :bye!)
  (gtk:main-quit))
(defun hello-world ()
  (let ((window (gtk:window-new gtk:window-toplevel))
        (button (gtk:button-new-with-label "Hello World!")))
    (gtk:container-add window button)
    (gtk:container-set-border-width window 10)
    (gtk:widget-show button)
    (gtk:widget-show window)
    (setf (widget->object window) (make-instance 'widget))
    (g:signal-connect button "clicked" (g:callback domsg)
                      window)
    (g:signal-connect window "delete-event" (g:callback delev)
                      (g:nullptr))
    (g:signal-connect window "destroy" (g:callback bye)
                      (g:nullptr))
    (gtk:main)))
(hello-world)
|#


#|
;;; Test menu example. Callbacks have to be defined before menus
;;; that use them.

(gtk:define-signal-handler bye :void (window data)
  window data
  (format t "Bye!~%")
  (gtk:main-quit))

(gtk:define-signal-handler file-menu-quit :void (window)
  ;; widget is main window...
  (gtk:widget-destroy window))

(gtk:define-signal-handler front-menu-select :void (item window)
  window
  (let* ((shell (gtk:menu-item-get-submenu item))
         (glist (and shell
                     (GtkMenuShell.children shell))))
    (format t ":front-menu-select ~S~%" glist)))

;;; A test menubar with menus, submenus, items, seperators
;;; check items, radio items, "dynamic" menu creation and
;;; activation.

(defun menu-maker (shell gdata)
  ;; shell is menu to add childer to, gdata is
  ;; user_data passed to the menu.
  ;; (GtkMenuShell.children shell)
  (declare (ignore gdata))
  (loop for n from 1 to 5
        for s = (format nil "Layer ~D" n)
        for i = (gtk:menu-item-new-with-label s)
        do
        (gtk:menu-shell-append shell i)
        (gtk:widget-show i)))

(defparameter *mybar*
  `((menu "File"
     (item ("Open" :insensitive t))
     (item nil)
     (item "Quit" ,(g:callback file-menu-quit)))
    (menu "Plot" 
     (item "New")
     (menu "FeFiFoFum" (item "Fe") (item "Fi") (item "Fo") 
      (item nil) (item "Fum"))
     (item nil)
     (item "Delete")
     (item "Export")
     )
    (menu "View"
     (menu ("Front" :constructor , (function menu-maker )
                    :selector , (g:callback front-menu-select) ))
     (item nil)
     (item ("Envelope" :radio t) )
     (item ("Line" :radio t) )
     (item ("Point" :radio t)  )
     (item nil)
     (item ("Show Grid" :check t) )
     )
    (menu ("Help" :right t) (item "Hiho"))))

(defun testmenu ()
  (let (window hints mask vbox menubar)
    hints mask
    (setq window (gtk:window-new gtk:window-toplevel))
;    (setq hints (ccl::make-record :<G>dk<G>eometry
;                                  :min_width 400 :min_height 16
;                                  :max_width -1 :max_height -1))
;    (setq mask (logior gdk:hint-min-size
;                       ;gdk:hint-max-size
;                       ))
;    (gtk:window-set-geometry-hints window (g:nullptr) hints mask)
    (setq vbox (gtk:vbox-new nil 5))
    (gtk:container-add window vbox)
;    (gtk:window-set-geometry-hints window vbox hints mask)
    (gtk:widget-show vbox)
    ;; create menubar and add to top of vbox
    (setq menubar (create-menubar *mybar* window vbox))
    menubar
    (gtk:widget-show window)
    (g:signal-connect window "destroy" (g:callback bye)
                      (g:nullptr))
    window
    #-:openmcl (gtk:main)
    ))

; (gtk:init 0 (g:nullptr))
; (testmenu)

|#
