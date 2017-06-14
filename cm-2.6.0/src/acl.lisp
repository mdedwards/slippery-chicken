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
;;; $Date: 2003/12/18 13:16:42 $

(in-package :cm)

;;;
;;; CLOS twiddling
;;;      

(import '(clos:slot-definition-initargs
	  clos:slot-definition-initform
	  clos:slot-definition-name
	  clos:class-direct-subclasses
	  clos:class-direct-superclasses
	  clos:class-direct-slots
	  clos:class-slots
	  clos:generic-function-name
	  ))

(defun finalize-class (class) (clos:finalize-inheritance class))

(defmethod validate-class ((class t) (superclass t))
  ;; this is a no-op except in OpenMCL 014
  t)

(defun slot-definition-reader (x) x nil)

(defmethod make-load-form (obj) (cl:make-load-form obj))

;;;
;;; misc stuff
;;;

(defun exit () (excl:exit))

(defun quit () (exit))

(defun object-address (x)
  (excl::pointer-to-address x))

(defconstant directory-delimiter #\/)

;(defun cd (&optional dir)
;  (if (null dir)
;    (namestring (excl:current-directory))
;    (progn
;      (tpl::cd-command dir)
;      ;(namestring (excl:current-directory))
;      (values))))

(defun pwd ()
  (namestring (excl:current-directory)))

(defun cd (&optional (dir (user-homedir-pathname )))
  (tpl::cd-command (namestring (truename dir))))

(defun shell (cmd &rest args) 
  (excl:shell (apply #'format nil cmd args)))

(defun make-cm-script (dir mempath initpath)
  (let ((lisp (system:command-line-argument 0))
	(script (namestring (merge-pathnames #+win32 "cm.bat"
                                             #-win32 "cm" 
                                             dir))))
    (format t "~%; Saving CM startup script in ~S" script)
    (with-open-file (f script :direction :output
                       :if-does-not-exist :create
                       :if-exists :supersede)
      (format f 
              #+win32 "~a -I ~a -L ~a~%"
              #-win32 "~a -I ~a -L ~a~%"
              lisp mempath initpath))))

(defun cm-image-dir ()
  (namestring
   (make-pathname
    :directory (pathname-directory excl::*image-file*))))

(defun env-var (var)
  (sys:getenv var))

(defun save-cm (path &rest args)
  (declare (ignore args))
  ;; what a pain...
  (dolist (c (class-subclasses (find-class 'container)))
    (finalize-class c))
  (let ((fn #'(lambda ()
		(declare (special *cm-readtable*)
                         (function load-cminit cm))
                (tpl:setq-default *readtable* *cm-readtable*)
                (tpl:setq-default *package* (find-package :cm))
                (load-cminit)
                (cm-logo)
                )))
    (setf excl::*read-init-files* t)
    (push fn excl::*restart-actions*)
    (excl:dumplisp :name path)))
