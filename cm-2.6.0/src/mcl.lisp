;;; **********************************************************************
;;; Copyright (C) 2002 Heinrich Taube (taube@uiuc.edu) 
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation; either version 2
;;; of the License, or (at your option) any later version.
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; **********************************************************************

;;; $Name: rel-2_6_0 $
;;; $Revision: 1.5 $
;;; $Date: 2004/01/02 18:23:47 $

(in-package :cm)


(pushnew :metaclasses *features*)

(import '(ccl:slot-definition-name 
          ccl:slot-definition-initargs 
          ccl:slot-definition-initform 
          ccl:class-direct-superclasses
          ccl:class-direct-subclasses 
          ;ccl:without-interrupts
          ;ccl:arglist 
          ;ccl:class-prototype
          ))

(defun finalize-class (class) class t)

(defmethod validate-class ((class t) (superclass t))
  ;; this is a no-op except in OpenMCL 014
  t)

(defmethod make-load-form (obj) (cl:make-load-form obj))

(defun slot-definition-reader (slot) slot nil)

(defun class-slots (class) 
  (ccl::class-instance-slots class))

(defun class-direct-slots (class)
  (ccl:class-direct-instance-slots class))

(defun object-address (x)
  (ccl:%address-of x))

(defun generic-function-name (fn)
  (ccl::function-name fn))

;(defun cd (&optional dir)
;  (if (null dir)
;    (namestring (ccl::mac-default-directory))
;    (let ((nam (namestring (truename dir))))
;      (namestring (ccl::set-mac-default-directory nam)))))

(defun pwd ()
  (namestring (ccl::mac-default-directory)))

(defun cd (&optional (dir (user-homedir-pathname )))
  (namestring (ccl::set-mac-default-directory dir)))

(defun shell (&rest args)
  args
  (warn "Shell not implemented in MacOS."))

(defconstant directory-delimiter #\:)

;;;
;;; this checks Mac file attributes under the root cm directory.
;;;

(defun macify-cm (cmdir)
  (unless (char= (elt cmdir (1- (length cmdir))) #\:)
    (setf cmdir (concatenate 'string cmdir ":")))
  (let ((path (concatenate 'string cmdir "**:*.*"))
        ext appl type)
    (format t "~%; Restoring Macintosh file attributes for \"~A**.*.*\"~%"
            cmdir)
    (dolist (f (directory path))
      (setf ext (pathname-type f))
      (cond 
       ((member ext '("lisp" "scm" "el" "ins" "clm" "cmn")
                :test #'equal) 
        (setf appl :ccl2 type :text))
       ((equal ext "html") 
        (setf appl :msie type :text))
       ((or (equal ext "html") (null ext))
        (setf appl "ttxt" type ':text))
       ((equal ext "gif") (setf appl :msie type "GIFf"))
       ((equal ext "cm") (setf appl :cm-2 type :text))
       (t (setf appl nil)))
      (when appl
        (ccl:set-mac-file-creator f appl)
        (ccl:set-mac-file-type f type)))))


;;;
;;; Apple events
;;;

(defconstant qt-signature :TVOD)

(eval-when (load compile eval)
  (load "ccl:examples;processes.lisp")
  (ccl:require ':appleevent-toolkit "ccl:examples;appleevent-toolkit"))

(defun ensure-foreground-application (creator)
  ;; like ccl::ensure-application-active, but doesn't return to mcl.
  (if (ccl::find-process creator)
    t
    (let* ((foreground ccl::*foreground*)
           (filename (ccl::launch-creator creator)))
      (when (and filename foreground)
        (loop
          (ccl:event-dispatch)
          (unless ccl::*foreground* (return)))
        )
      filename)))

(defmacro with-application ((var sign) &body body)
  ;; insure applicatoin is running as an appleevent target
  (let ((sig (gensym)))
    `(let ((,sig ,sign))
       (unless (ccl::find-process ,sig)
         (if (ccl::get-creator-path ,sig)
           (progn
             (unless (ensure-foreground-application ,sig)
               (error "~a failed to launch. Try launching it by hand." 
                      (namestring (ccl::get-creator-path ,sig)))))
           (error "Application for signature ~S not found." ,sig)))
       (ccl:with-aedescs (,var reply)
         (ccl::create-signature-target ,var ,sig)
         ,@body)
       (values))))

;;;
;;; Midi support
;;;

(defun mac-play-midi-file (file &rest args)
  (declare (ignore args))
  (setf file (namestring (truename file)))
  ;(ccl:set-mac-file-creator file :TVOD)
  (ccl:set-mac-file-type file "Midi")
  ;; make sure QuickTime launches
  (with-application (qt :TVOD)
    (ccl:with-aedescs (open repl)
      (ccl::create-odoc open qt (list file))
      (ccl::send-appleevent open repl :reply-mode ':wait-reply))))

;;;
;;; HTML support
;;;

(defparameter *browser* ':msie)

(defun create-OpenURL (desc target url)
  (ccl::create-appleevent desc ':|WWW!| ':|OURL| target )
  (ccl::ae-put-parameter-char desc ':---- url))

(defun open-url (url &key (browser *browser*))
  (with-application (br browser)
    (ccl:with-aedescs (open reply)
      (create-OpenURL open br url)
      (ccl::send-appleevent open reply
			    :reply-mode ':wait-reply))))

;;;
;;; cm application and splashscreen
;;; 

(defclass cm (ccl::lisp-development-system) ())
(defmethod ccl:application-file-creator ((app cm)) ':cm-2)
(defmethod ccl:application-name         ((app cm)) "Common Music")
(defmethod ccl:application-resource-file ((app cm)) 
  nil ; (translate-logical-pathname "cm:etc;cm.rsrc")
  )

(defparameter %splash-text-color #xC3C3E5 ) ; #x3d3d4c hsv=240,25,30%
(defparameter %splash-view-color #x000000) ; #xFFFFFF

(defclass cm-about-dialog (ccl:color-dialog)
  ((pict-hdl :initarg :pict-hdl :initform nil :reader pict-hdl)
   (timeout :initform nil :initarg :timeout ))
  (:default-initargs
    :view-position '(:top 50)
    :view-size #@(300 300)
    :window-type :double-edge-box
    :content-color %splash-view-color)) ;#x000000

(defmethod ccl:view-click-event-handler :after ((view cm-about-dialog) 
                                                where)
  (declare (ignore where))
  ;; dont return more than once if user clicks on window before timeout.
  (let ((time (slot-value view 'timeout)))
    (when (and (numberp time) (>= time 0))
      (sleep time)
      (setf time (slot-value view 'timeout)))
    (when (or (null time) (>= time 0))
      (setf (slot-value view 'timeout) -1)
      (ccl:return-from-modal-dialog t))))

(defmethod ccl:view-draw-contents :before ((dlg cm-about-dialog))
  (let ((pict-hdl (slot-value dlg 'pict-hdl)))
    (when pict-hdl
      (ccl:rlet ((rect :rect
                       :topleft #@(50 50)
                       :bottomright #@(260 260)))
        (#_DrawPicture pict-hdl rect)))))

(defmethod ccl:window-select :after ((view cm-about-dialog))
  (when (slot-value view 'timeout)
    (ccl:view-click-event-handler view 0)))

(defmethod ccl:window-close :after ((dlg cm-about-dialog))
  (let ((pict-hdl (slot-value dlg 'pict-hdl)))
    (when pict-hdl
      (#_DisposeHandle pict-hdl)
      (setf (slot-value dlg 'pict-hdl) nil))))

(defparameter *cm-splashscreen* t)

(defun cm-splashscreen (&optional (time 2.5))
  (let ((w (ccl:application-about-dialog ccl:*application*)))
    (setf (slot-value w 'timeout) time)
    (ccl:modal-dialog w t)))

(defmethod ccl:application-about-dialog ((app cm))
  (let* ((pict-hdl (ccl::ignore-errors  
                    (let ((pict-hdl (#_Get1Resource :|PICT| 128)))
                        (unless (or (ccl:%null-ptr-p pict-hdl)
                                    (ccl:%null-ptr-p 
                                     (ccl:%get-ptr pict-hdl)))
                          (#_DetachResource pict-hdl)
                          pict-hdl))))
         (height (if pict-hdl 300 100))
         (textcolor `(:text ,%splash-text-color)))
    (make-instance 'cm-about-dialog
      :auto-position :centermainscreen
      :window-type :double-edge-box
      :window-show nil
      :pict-hdl pict-hdl
      :view-subviews
      (list (ccl:make-dialog-item 
             'ccl:static-text-dialog-item
             #@(5 5)
             #@(310 28)
             "Common Music"
             nil
             :view-font '("Times" 24 :plain :srcor)
             :text-justification #$teJustCenter
             :part-color-list textcolor)
            (ccl:make-dialog-item
             'ccl:static-text-dialog-item
             #@(5 30)
             #@(310 15)
             (format nil "Version ~a" (cm-version-number))
             nil
             :view-font '("Times" 12 :plain :srcor)
             :text-justification #$teJustCenter
             :part-color-list textcolor)
            (ccl:make-dialog-item
             'ccl:static-text-dialog-item
             (ccl:make-point  40
                              (- height 40))
             #@(220 38)
             "Copyright © 1989-2001 Heinrich Taube"
             nil
             :view-font '("Geneva" 9 :plain :srcor)
             :text-justification #$teJustCenter
             :part-color-list textcolor )))))

;;;
;;; save cm
;;;


(defun cm-image-dir ()
  (namestring (ccl:mac-default-directory)))

(defun env-var (var)
  var 
  nil)

(defmethod ccl:window-show :after ((w ccl:listener))
  (if (eql *package* (find-package :cm))
      (cm-logo)))

(defun save-cm (path &key (type :cmdev) rsrc)
  (declare (special *cm-readtable*))
  type rsrc
  (let (
        #|(reso 
         (namestring (make-pathname
                      :directory
                      (substitute "etc" "src"
                                  (pathname-directory 
                                   ccl:*loading-file-source-file*)
                                  :test #'equal)
                      :name "cm" :type "rsrc")))|#
        (sign ':cm-2))
    
    ;; Update the About... menu item
    (let ((menu (first (ccl:menubar))))
      (when (typep menu 'ccl:apple-menu)
        (let ((item (first (ccl:menu-items menu))))
          (when item 
            (ccl:set-menu-item-title item "About Common Music")))))
    
    (setf ccl::*inhibit-greeting* t)
    (setf ccl::*listener-window-size* #@(502 150))
    (setf ccl:*lisp-startup-functions*
          (append ccl:*lisp-startup-functions*
                  (list
                   #'(lambda ()
                       (declare (special *cm-readtable*))
                       (setf *package* (find-package :cm))
                       (setf *readtable* *cm-readtable*)
                       (let* ((dir (ccl::mac-default-directory))
                              (fil (OR 
                                    (probe-file
                                     (merge-pathnames "cminit.lisp"
                                                      dir))
                                    )))
                         (when fil
                           (load fil :verbose nil)))
                       ;(cm-logo)
                       ;(when *cm-splashscreen*
                       ;  (cm-splashscreen))
                       ))))
    (ccl:save-application path
                          ;:application-class (find-class 'cm)
                          :size '(#-clm #xA00000 
                                  #+clm 24000000
                                  #x500000)
                          :excise-compiler (eql type :cm)
                          :init-file nil
                          ;:resources (if (probe-file reso)
                          ;             (ccl::get-app-resources reso sign)
                          ;             (error "Can't find ~S" reso))
                          :creator sign)))

;;;
;;; cm-listener
;;;

#|
(setf (logical-pathname-translations "CCL")
      `(("interfaces;**;*.*" "ccl:library;interfaces;**;*.*")
        ("inspector;**;*.*" "ccl:library;inspector folder;**;*.*")
        ("l1;**;*.*" "ccl:level-1;**;*.*")
        ("**;*.*" , 
         (format nil "Macintosh HD:Lisp:MCL 4.2:**:*.*"
                 (loop for x in (cdr (pathname-directory 
                                      ccl:*loading-file-source-file*) )
                       collect x collect ":"
                       until (equal x "Lisp"))))))

(unless (probe-file "ccl:l1;")
  (ccl::add-logical-pathname-translation 
   "ccl" '("l1;**;*.*" "ccl:Additional MCL Source Code;level-1;**;*.*")))

(unless (probe-file "ccl:compiler;")
  (ccl::add-logical-pathname-translation 
   "ccl" '("compiler;**;*.*" "ccl:Additional MCL Source Code;compiler;**;*.*")))

(unless (probe-file "ccl:lib;")
  (ccl::add-logical-pathname-translation 
   "ccl" '("lib;**;*.*" "ccl:Additional MCL Source Code;lib;**;*.*")))

(defparameter *listener-prompt* "cm>")
(defparameter *listener-back-color* #xeeeeee)
(defparameter *listener-break-on-errors* nil)

(defclass cm-fred-item (ccl::listener-fred-item)
  ()
  (:default-initargs
    :part-color-list '(:body  #xeeeeee)))
; (make-instance 'cm-fred-item)

(defmethod initialize-instance :after ((obj cm-fred-item) &rest args)
  (declare (ignore args))
 ; (ccl:set-part-color obj ':body *listener-back-color*)
  )

(defclass cm-listener (ccl:listener)
  ()
  (:default-initargs
    :fred-item-class 'cm-fred-item
    ))

; (describe (setf x (make-instance 'cm-listener)))
; (ccl:set-part-color (ccl::fred-item (first (ccl:subviews (ccl::current-listener)))) :body #xeeeeee )


|#

