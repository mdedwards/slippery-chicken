;;; **********************************************************************
;;; Copyright (C) 2003 Heinrich Taube (taube@uiuc.edu) 
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
;;; $Revision: 1.1.1.1 $
;;; $Date: 2003/06/25 10:45:05 $

(in-package :cm)

#-(or clisp cmu)
(defun class-subclasses (class)
  ;; return a list of all subclases of class
  (let ((subs (class-direct-subclasses class)))
    (append subs
            (loop for s in subs
                  append (class-subclasses s)))))

(defun slot-getter-form (obj slot)
  `(slot-value ,obj ',slot))

(defun slot-setter-form (obj slot value)
  `(setf (slot-value ,obj ',slot) ,value))

(defun slot-value-or-default (obj slot &optional default)
  (if (and (slot-exists-p obj slot)
	   (slot-boundp obj slot))
    (slot-value obj slot)
    default))

;;;
;;; The #-metaclass code should work for all CLTL's that -- for one
;;; reason or another -- don't allow me use CLOS metaclasses
;;;

#-metaclasses
(progn
  
  (defvar *metainfo* (make-hash-table))
  
  (defun get-meta-info (class type)
    ;; yikes! for some reason classes are not eq in cmu
    ;; so i have to hash on the class name not the class.
    (let ((entry (gethash (class-name class) *metainfo*)))
      (if entry
        (getf (cdr entry) type)
        nil)))
  
  (defun set-meta-info (class type value)
    (let ((entry (gethash (class-name class) *metainfo*)))
      (if (null entry)
        (setf (gethash (class-name class) *metainfo*) 
              (list class type value))
        (setf (getf (cdr entry) type) value))
      value))
  
  (defmethod class-parameters ((class standard-class))
    (get-meta-info class :parameters))
  
  (defmethod (setf class-parameters) (value class)
    (set-meta-info class :parameters value))
  
  (defmethod io-class-file-types ((class standard-class))
    (get-meta-info class :file-types))
  
  (defmethod (setf io-class-file-types) (value class)
    (set-meta-info class :file-types value))
  
  (defmethod io-class-mime-type ((class standard-class))
    (get-meta-info class :mime-type))
  
  (defmethod (setf io-class-mime-type) (value class)
    (set-meta-info class :mime-type value))

  (defmethod io-class-output-hook ((class standard-class))
    (get-meta-info class :output-hook))
  
  (defmethod (setf io-class-output-hook) (value class)
    (set-meta-info class :output-hook value))
  
  (defmethod io-class-definer ((class standard-class))
    (get-meta-info class :definer))
  
  (defmethod (setf io-class-definer) (value class)
    (set-meta-info class :definer value))
  
  (defmethod io-class-file-versions ((class standard-class))
    (get-meta-info class :versions))
  
  (defmethod (setf io-class-file-versions) (value class)
    (set-meta-info class :versions value))
  
  )

