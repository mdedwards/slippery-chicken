;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* named-object/activity-levels
;;; NAME 
;;; activity-levels
;;;
;;; File:             activity-levels.lsp
;;;
;;; Class Hierarchy:  named-object -> activity-levels
;;;
;;; Version:          1.0.2
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Class used in rthm-chain.  Used on a call-by-call basis
;;;                   to determine (deterministically) whether a process is
;;;                   active or not (boolean).
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    4th February 2010
;;;
;;; $$ Last modified: 14:31:33 Fri May 24 2013 BST
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

(eval-when (compile)
  (declaim (optimize (speed 3) (safety 1) (space 0) (debug 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :slippery-chicken)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass activity-levels (named-object)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Sat Apr 28 12:17:30 2012 -- clone-with-new-class method not necessary
;;; as no new slots but the clone method is so that we get an al not a
;;; named-object. 

(defmethod clone ((al activity-levels))
  (clone-with-new-class al 'activity-levels))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; At the moment there's no way for the user to use their own activity lists
;;; but this can easily be provided upon request.
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

;;; ****m* activity-levels/reset
;;; DESCRIPTION
;;; Reset the activity-levels object to restart at the first element of the 1st
;;; (or user-specificed) 10-element list. 
;;; 
;;; ARGUMENTS
;;; The activity-levels object. 
;;; 
;;; OPTIONAL ARGUMENTS
;;; start-at: should be between 1 and 3; it indicates which of the 10-lists
;;; we're going to start with.  Default = 1.
;;; 
;;; RETURN VALUE
;;; T
;;;
;;; SYNOPSIS
(defmethod reset ((al activity-levels) &optional (start-at 1) ignore)
;;; ****
  (declare (ignore ignore))
  (unless (and (>= start-at 1) 
               (<= start-at 3))
    (error "activity-levels::reset: <start-at> should be between 1 and 3: ~a"
           start-at))
  (loop for l in (data al) do 
       (loop for 10-list in (data l) do (reset 10-list))
       (reset l start-at))
  t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* activity-levels/active
;;; DESCRIPTION
;;; Returns t or nil depending on whether we're active at this point.  The
;;; object remembers where we were last time; this means if we change level
;;; before getting to the end of a ten-list, we'll pick up where we left off
;;; next time we return to that level.  <level> can be a floating point number:
;;; in this case it will be rounded. But <level> must be between 0 and 10,
;;; where 0 is always inactive, 10 is always active, and anything inbetween
;;; will use the data lists circularly.
;;; 
;;; ARGUMENTS
;;; - the activity-levels object
;;; - the activity-level number we want to test
;;; RETURN VALUE
;;; T or NIL
;;; 
;;; EXAMPLE
#|
(let ((al (make-al)))
  (print (loop for i below 15 collect (active al 0)))
  (print (loop for i below 15 collect (active al 5)))
  (print (loop for i below 15 collect (active al 1)))
  (print (loop for i below 15 collect (active al 9)))
  (loop for i below 15 collect (active al 10)))

=>
(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL) 
(T T NIL NIL T NIL T T NIL NIL NIL T NIL T NIL) 
(T NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL T NIL) 
(T T NIL T T T T T T T T T T T NIL) 
(T T T T T T T T T T T T T T T)
|#
;;; SYNOPSIS
(defmethod active ((al activity-levels) level)
;;; ****
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

;;; ****f* activity-levels/make-al
;;; DESCRIPTION

;;; Make an activities-level object for determining (deterministically) on a
;;; call-by-call basis whether a process is active or not (boolean).  This is
;;; determined by nine 10-element lists (actually three versions of each) of
;;; hand-coded 1s and 0s, each list representing an 'activity-level' (how
;;; active the process should be).  The first three 10-element lists have only
;;; one 1 in them, the rest being zeros.  The second three have two 1s,
;;; etc. Activity-levels of 0 and 10 would return never active and always
;;; active respectively.
;;; 
;;; ARGUMENTS
;;; None required.
;;; 
;;; OPTIONAL ARGUMENTS
;;; start-at (default NIL): which of the three 10-element lists to start with
;;; (reset to).  Should be 1, 2, or 3 though if NIL will default to 1.
;;; 
;;; RETURN VALUE
;;; The activities-level object.
;;;
;;; SYNOPSIS
(defun make-al (&optional start-at)
;;; ****
  (let ((al (make-instance 'activity-levels)))
    (when start-at
      (reset al start-at))
    al))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF activity-levels.lsp
