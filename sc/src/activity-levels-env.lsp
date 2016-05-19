;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* named-object/activity-levels-env
;;; NAME 
;;; activity-levels
;;;
;;; File:             activity-levels-env.lsp
;;;
;;; Class Hierarchy:  named-object -> activity-levels -> activity-levels-env
;;;
;;; Version:          
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          An extension of the activity-levels class to incorporate
;;;                   a restarting envelope which is used to determine the 
;;;                   current 'level' argument for a call to the active method. 
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    30th June 2015
;;;
;;; $$ Last modified: 12:56:46 Wed Jul  1 2015 BST
;;;
;;; SVN ID: $Id: activity-levels.lsp 5048 2014-10-20 17:10:38Z medward2 $
;;;
;;; ****
;;; Licence:          Copyright (c) 2010 Michael Edwards
;;;
;;;                   This file is part of slippery-chicken
;;;
;;;                   slippery-chicken is free software; you can redistribute it
;;;                   and/or modify it under the terms of the GNU General
;;;                   Public License as published by the Free Software
;;;                   Foundation; either version 3 of the License, or (at your
;;;                   option) any later version.
;;;
;;;                   slippery-chicken is distributed in the hope that it will
;;;                   be useful, but WITHOUT ANY WARRANTY; without even the
;;;                   implied warranty of MERCHANTABILITY or FITNESS FOR A
;;;                   PARTICULAR PURPOSE.  See the GNU General Public License
;;;                   for more details.
;;;
;;;                   You should have received a copy of the GNU General Public
;;;                   License along with slippery-chicken; if not, write to the
;;;                   Free Software Foundation, Inc., 59 Temple Place, Suite
;;;                   330, Boston, MA 02111-1307 USA
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :slippery-chicken)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass activity-levels-env (activity-levels)
  ;; the envelope in x y form. y values should range from 0 to 10, as with the
  ;; level argument to an 'active' call in the parent class. x range can be any
  ;; arbitrary value but start with 0 to get full-range results.
  ((env :accessor env :type list :initarg :env :initform '(0 5 100 5))
   ;; the envelope repeats after a specified cycle length, so that no matter
   ;; how many times you call 'active' you always get results.
   (cycle-length :accessor cycle-length :type integer :initarg :cycle-length
                 :initform 101)
   ;; for internal use.
   (current :accessor current :type integer :initform 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; for internal use
(defmethod init-data ((ale activity-levels-env))
  (setf (slot-value ale 'env) (new-lastx (env ale) (1- (cycle-length ale)))
        (current ale) 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod initialize-instance :after ((ale activity-levels-env) &rest initargs)
  (declare (ignore initargs))
  (init-data ale))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod (setf cycle-length) :after (value (ale activity-levels-env))
  (declare (ignore value))
  (init-data ale))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod (setf env) :after (value (ale activity-levels-env))
  (declare (ignore value))
  (init-data ale))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod clone ((ale activity-levels-env))
  (clone-with-new-class ale 'activity-levels-env))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod clone-with-new-class :around ((ale activity-levels-env) new-class)
  (declare (ignore new-class))
  (let ((al (call-next-method)))
    (setf (slot-value al 'env) (copy-list (env ale))
          (slot-value al 'cycle-length) (cycle-length ale)
          (slot-value al 'current) (current ale))
    al))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod print-object :before ((ale activity-levels-env) stream)
  (format stream "~%ACTIVITY-LEVELS-ENV: current: ~a, cycle-length: ~a~
                  ~%env: ~a" (current ale) (cycle-length ale) (env ale)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* activity-levels-env/active
;;; DESCRIPTION
;;; Similar to the parent class method except that the level argument in that
;;; class is here supplied by the envelope and cycle-length.
;;; 
;;; ARGUMENTS
;;; - the activity-levels-env object
;;; 
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to print debugging information.
;;; 
;;; RETURN VALUE
;;; T or NIL to indicate whether we're 'active' or not.
;;; 
;;; EXAMPLE
#|
;;; we can see here that with a cycle-length of 10, calling active 31 times
;;; means we go through the envelope three times before arriving back at 0. 
(let ((ale (make-ale '(0 0 2 10 3 1) 10 2)))
  (loop repeat 31 collect (active ale t)))
->
current: 0 level: 0.0
current: 1 level: 1.6666667
current: 2 level: 3.3333335
current: 3 level: 5.0
current: 4 level: 6.666667
current: 5 level: 8.333333
current: 6 level: 10.0
current: 7 level: 7.0
current: 8 level: 4.0
current: 9 level: 1.0
current: 0 level: 0.0
current: 1 level: 1.6666667
current: 2 level: 3.3333335
current: 3 level: 5.0
current: 4 level: 6.666667
current: 5 level: 8.333333
current: 6 level: 10.0
current: 7 level: 7.0
current: 8 level: 4.0
current: 9 level: 1.0
current: 0 level: 0.0
current: 1 level: 1.6666667
current: 2 level: 3.3333335
current: 3 level: 5.0
current: 4 level: 6.666667
current: 5 level: 8.333333
current: 6 level: 10.0
current: 7 level: 7.0
current: 8 level: 4.0
current: 9 level: 1.0
current: 0 level: 0.0
(NIL NIL NIL NIL T T T T NIL NIL NIL NIL NIL T NIL T T T T NIL NIL NIL NIL NIL
 NIL T T T NIL NIL NIL)
|#
;;; SYNOPSIS
(defmethod active :around ((ale activity-levels-env) &optional print)
;;; **** 
  (let ((level (interpolate (current ale) (env ale))))
    (when print (format t "~&current: ~a level: ~a" (current ale) level))
    (if (= (current ale) (1- (cycle-length ale)))
        (setf (current ale) 0)
        (incf (current ale)))
    (call-next-method ale level)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Related functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* activity-levels-env/make-ale
;;; DESCRIPTION

;;; For an overview of what this might be used for and how it's done in its
;;; basic form, see the description to make-al in the parent class
;;; 'activity-levels'
;;; 
;;; ARGUMENTS
;;; None required.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - the envelope in x y form. y values should range from 0 to 10, as with the
;;;   level argument to an 'active' call in the parent class. x range can be
;;;   any arbitrary value but start with 0 to get full-range results.
;;; - the cycle length: the envelope repeats after a specified cycle length, so
;;;   that no matter how many times you call 'active' you always get results.
;;; - start-at (default NIL): which of the three 10-element lists in the parent
;;;   class to start with (reset to).  Should be 1, 2, or 3 though if NIL will
;;;   default to 1.
;;; 
;;; RETURN VALUE
;;; The activities-level-env object.
;;;
;;; SYNOPSIS
(defun make-ale (&optional (env '(0 5 100 5)) (cycle-length 101) start-at)
;;; ****
  (let ((ale (make-instance 'activity-levels-env :cycle-length cycle-length
                            :env env)))
    (when start-at
      (reset ale start-at))
    ale))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF activity-levels-env.lsp
