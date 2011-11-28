;;; 26.11.11 SEAN: Corrected NAME
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* _sc/activity-levels
;;; NAME 
;;; activity-levels
;;;
;;; File:             activity-levels.lsp
;;;
;;; Class Hierarchy:  named-object -> activity-levels
;;;
;;; Version:          1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          class used in rthm-chain.
;;;                   No public interface envisaged (so no robodoc entries).
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    4th February 2010
;;;
;;; $$ Last modified: 21:07:36 Mon Nov  7 2011 GMT
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

(defclass activity-levels (named-object)
  ())

(defmethod initialize-instance :after ((al activity-levels) &rest initargs)
  (declare (ignore initargs))
  (setf (data al)
        (loop for level in
           ;; these are the states that indicate whether there should be
           ;; activity (1) or not (0).
             '( ;; 1
               ((1 0 0 0 0 0 0 0 0 0)
                (0 0 0 1 0 0 0 0 0 0)
                (0 0 0 0 0 0 1 0 0 0))
               ;; 2
               ((1 0 0 0 0 0 1 0 0 0)
                (0 0 0 1 0 1 0 0 0 0)
                (0 0 0 0 0 0 1 1 0 0))
               ;; 3
               ((1 0 0 0 1 0 1 0 0 0)
                (0 0 0 1 0 1 1 0 0 0)
                (0 0 1 0 0 0 1 1 0 0))
               ;; 4
               ((1 0 0 0 1 0 1 1 0 0)
                (0 1 0 1 0 1 1 0 0 0)
                (0 0 1 0 0 0 1 1 0 1))
               ;; 5
               ((1 1 0 0 1 0 1 1 0 0)
                (0 1 0 1 0 1 1 0 0 1)
                (0 0 1 0 1 0 1 1 0 1))
               ;; 6
               ((1 1 0 1 1 0 1 1 0 0)
                (0 1 0 1 0 1 1 0 1 1)
                (0 1 1 0 1 0 1 1 0 1))
               ;; 7
               ((1 1 0 1 1 0 1 1 0 1)
                (1 1 0 1 0 1 1 0 1 1)
                (1 1 1 0 1 0 1 1 0 1))
               ;; 8
               ((1 1 0 1 1 1 1 1 0 1)
                (1 1 1 1 0 1 1 0 1 1)
                (1 1 1 0 1 1 1 1 0 1))
               ;; 9
               ((1 1 0 1 1 1 1 1 1 1)
                (1 1 1 1 0 1 1 1 1 1)
                (1 1 1 1 1 1 1 1 0 1)))
           collect
             (make-cscl (loop for ten in level 
                           collect (make-cscl ten)))))
  ;; got to do this so get-last returns the first ... doh!
  (loop for l in (data al) do (reset l 1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object :before ((al activity-levels) stream)
  (format stream "~%ACTIVITY-LEVELS: "))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; <start-at> should be between 1 and 3; it indicates which of the 10-lists
;;; we're going to start with.
(defmethod reset ((al activity-levels) &optional (start-at 1))
  (unless (and (>= start-at 1) 
               (<= start-at 3))
    (error "activity-levels::reset: <start-at> should be between 1 and 3: ~a"
           start-at))
  (loop for l in (data al) do 
       (loop for 10-list in (data l) do (reset 10-list))
       (reset l start-at)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; returns t or nil depending on whether we're active at this point (instance
;;; remembers where we were last time--this means if we change level before
;;; getting to the end of a ten-list, we'll pick up where we left off next time
;;; we return to that level.
;;; <level> can be a floating point number: it will be rounded.
(defmethod active ((al activity-levels) level)
  (let ((max (1+ (length (data al))))) ; the 0 and 10 cases are implicit
    (flet ((active-error ()
             (error "activity-levels::active: level must be >=0 and <=~a: ~a"
                    max level)))
      (setf level
            (typecase level
              (integer level)
              (float (round level))
              (t (active-error))))
      (cond ((zerop level) nil)
            ((= max level) t)
            ((and (>= level 0)
                  (<= level max))
             (let* ((l (nth (1- level) (data al))) ; the cscl with 3 sublists
                    (lcurrent (get-last l))        ; the 10 1s or 0s
                    (result (get-next lcurrent)))
               (when (at-start lcurrent)
                 ;; we don't actually use the value this returns, we just make
                 ;; sure we go to the next list of 10 1s/0s 
                 (get-next l))
               (not (zerop result))))
            (t (active-error))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Related functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; EOF activity-levels.lsp
