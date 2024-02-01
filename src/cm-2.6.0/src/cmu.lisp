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
;;; $Revision: 1.9 $
;;; $Date: 2005/01/08 15:10:33 $

(in-package :cm)

(import '(ext::load-foreign
          pcl:slot-definition-initargs
          pcl:slot-definition-initform
          pcl:slot-definition-name
          pcl:class-direct-slots
          pcl:class-slots
          ;pcl::class-direct-subclasses
          pcl::class-direct-superclasses
          pcl:generic-function-name
          )
	:cm)

(defun quit () (ext::quit))
(defun exit () (quit))

#+cmu19
(defun class-subclasses (c)
  (let ((subs (pcl::class-direct-subclasses c)))
    (if (null subs)
	'()
      (loop for s in subs
	append (cons s (class-subclasses s))))))
	  
#+cmu18
(defun class-subclasses (class)
  (let ((tbl (kernel:class-subclasses class))
        (sub '()))
    (maphash (lambda (k v) v (push k sub)) tbl)
    (nreverse sub)))

#-(or cmu19 cmu18)
(error "Fix class-subclasses for this version of cmu.")

;(defun make-load-form (obj)
;  (pcl::make-load-form obj))

(defun finalize-class (class) 
  ;(pcl::finalize-inheritance class)
  )

(defmethod validate-class ((class t) (superclass t))
  ;; this is a no-op except in OpenMCL 014
  t)

(defun slot-defintion-reader (slot) slot nil)

;;;
;;; misc. stuff
;;;

(defun object-address (thing)
  (kernel:get-lisp-obj-address thing))

(defconstant directory-delimiter #\/)

;(defun cd (&optional dir)
;  (if dir
;    (progn (setf (ext:default-directory) dir)
;	   (namestring (ext:default-directory)))
;    (namestring (ext:default-directory))))

(defun cd (&optional (dir (user-homedir-pathname )))
  (setf (ext:default-directory) dir)
  (namestring (ext:default-directory)))

(defun pwd ()
  (namestring (ext:default-directory)))

;;(defun shell (format &rest strings) 
;;  (let ((str (apply #'format nil format strings)))
;;    (extensions:run-program "/bin/csh" (list "-fc" str) :output t)))

(defun shell (cmd &key (wait t) (output t))
  (extensions:run-program "/bin/csh" (list "-fc" cmd)
                          :output output :wait wait))

(defun env-var (var)
  (let ((x (assoc var ext:*environment-list*
                  :test #'string=)))
    (and x (cdr x) )))

(defun cm-image-dir ()
  (let ((img (member "-core" ext:*command-line-strings*)))
    (if img
	(namestring
	 (make-pathname
	  :directory (pathname-directory (cadr img))))
      nil)))

(defun save-cm (path &rest args)
  (declare (ignore args))
  (extensions:save-lisp path :print-herald NIL
                        :init-function
                        #'(lambda ()
                            (declare (special *cm-readtable*))
                            (setf *readtable* *cm-readtable*)
                            (setf *package* (find-package :cm))
                            (load-cminit)                            
                            (cm-logo)
                            (lisp::%top-level))))
