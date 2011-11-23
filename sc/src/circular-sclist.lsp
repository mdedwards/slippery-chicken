;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* _sc/circular-sclist
;;; NAME 
;;; circular-sclist
;;;
;;; File:             circular-sclist.lsp
;;;
;;; Class Hierarchy:  named-object -> linked-named-object -> sclist -> 
;;;                   circular-sclist
;;;
;;; Version:          1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Implementation of the circular-sclist class which offers
;;;                   the use of a function to cycle through the values in the
;;;                   sclist, starting at the beginning again once we've
;;;                   reached the end.
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    February 19th 2001
;;;
;;; $$ Last modified: 21:06:18 Mon Nov  7 2011 GMT
;;;
;;; SVN ID: $Id$
;;;
;;; ****
;;; Licence:          Copyright (c) 2010 Michael Edwards
;;;
;;;                   This file is part of slippery-chicken
;;;
;;;                   slippery-chicken is free software; you can redistribute it
;;;                   and/or modify it under the terms of the GNU General
;;;                   Public License as published by the Free Software
;;;                   Foundation; either version 2 of the License, or (at your
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

(eval-when (compile)
  (declaim (optimize (speed 3) (safety 1) (space 0) (debug 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :slippery-chicken)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass circular-sclist (sclist)
  ((current :accessor current :type integer :initform 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone ((cscl circular-sclist))
  (clone-with-new-class cscl 'circular-sclist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone-with-new-class :around ((cscl circular-sclist) new-class)
  (declare (ignore new-class))
  (let ((sclist (call-next-method)))
    (setf (slot-value sclist 'current) (current cscl))
    sclist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* circular-sclist/get-next
;;; FUNCTION
;;; get-next:
;;;
;;; 
;;; 
;;; ARGUMENTS:
;;; 
;;; 
;;; RETURN VALUE: 
;;; 
;;; 
;;; EXAMPLE
;;; 
;;; 
;;; DATE
;;; 
;;; 
;;; SYNOPSIS
(defmethod get-next ((cscl circular-sclist))
;;; ****
  (when (data cscl)
    (prog1
        (nth (current cscl) (data cscl))
      (incf (current cscl))
      (when (= (current cscl) (sclist-length cscl))
        (setf (current cscl) 0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 4/2/10 I should have handled increments better in the above
;;; function (what was I thinking?) but don't want to change this basic
;;; functionality now for fear of corrupting other dependent methods.

;;; ****m* circular-sclist/get-last
;;; FUNCTION
;;; get-last:
;;;
;;; 
;;; 
;;; ARGUMENTS:
;;; 
;;; 
;;; RETURN VALUE: 
;;; 
;;; 
;;; EXAMPLE
;;; 
;;; 
;;; DATE
;;; 
;;; 
;;; SYNOPSIS
(defmethod get-last ((cscl circular-sclist))
;;; ****
  (when (data cscl)
    (let ((index (1- (if (zerop (current cscl))
                         (sclist-length cscl)
                         (current cscl)))))
      (nth index (data cscl)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod at-start ((cscl circular-sclist))
  (zerop (current cscl)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; if this isn't an after list assoc-list init will fail (sclist-length
;;; remains -1  
(defmethod verify-and-store :after ((cscl circular-sclist))
  ;; (print 'cscl-verify)
  ;; don't call reset as that calls subclass methods which are dependent on
  ;; data not yet processed
  ;; (reset cscl)
  ;; have to do this though in case we (setf (data ...)) as that will call
  ;; verify-and-store via the sclist method
  (setf (current cscl) 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* circular-sclist/reset
;;; FUNCTION
;;; reset:
;;;
;;; 
;;; 
;;; ARGUMENTS:
;;; 
;;; 
;;; RETURN VALUE: 
;;; 
;;; 
;;; EXAMPLE
;;; 
;;; 
;;; DATE
;;; 
;;; 
;;; SYNOPSIS
(defmethod reset ((cscl circular-sclist) &optional where)
;;; ****
  ;; (print 'cscl-reset)
  (let ((index 0))
    (when where
      (if (< where (sclist-length cscl))
          (setf index where)
        (error "circular-sclist::reset: id: ~a, <where>:~a >= list length: ~a"
               (id cscl) where (sclist-length cscl))))
    (setf (current cscl) index)
    t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object :before ((cscl circular-sclist) stream)
  (format stream "~%CIRCULAR-SCLIST: current ~a" (current cscl)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Related functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****f* circular-sclist/make-cscl
;;; FUNCTION
;;; make-cscl:
;;;
;;; 
;;; 
;;; ARGUMENTS:
;;; 
;;; 
;;; RETURN VALUE: 
;;; 
;;; 
;;; EXAMPLE
;;; 
;;; 
;;; DATE
;;; 
;;; 
;;; SYNOPSIS
(defun make-cscl (list &key (id nil) (bounds-alert t) (copy t))
;;; ****
  (make-instance 'circular-sclist :data list :bounds-alert bounds-alert
                 :id id :copy copy))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cscl-p (thing)
  (typep thing 'circular-sclist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; EOF circular-sclist.lsp
