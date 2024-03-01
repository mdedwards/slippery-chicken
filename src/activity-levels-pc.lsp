;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* named-object/activity-levels-pc
;;; NAME 
;;; activity-levels
;;;
;;; File:             activity-levels-pc.lsp
;;;
;;; Class Hierarchy:  named-object -> activity-levels-pc
;;;
;;; Version:          1.0.12
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          At a minimum, this is replacement for fixed-seed weighted
;;;                   randomness using percentages--which will produce different
;;;                   results with different Lisp implementations--but in fact,
;;;                   because it uses two activity-levels objects, it will
;;;                   always produce pattern-based results. (The 'pc' stands for
;;;                   per cent.)
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    1st March 2024
;;;
;;; $$ Last modified:  12:47:52 Fri Mar  1 2024 CET
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

(in-package :slippery-chicken)

(defclass activity-levels-pc (named-object)
  ((al1 :accessor al1 :initform nil)
   (al2 :accessor al2 :initform nil)))

(defmethod initialize-instance :after ((alpc activity-levels-pc) &rest initargs)
  (declare (ignore initargs))
  (setf (al1 alpc) (make-al)
        (al2 alpc) (make-al)))

(defmethod print-object :before ((alpc activity-levels-pc) stream)
  (format stream "~%ACTIVITY-LEVELS-PC: ~%al1:~%~a~%al2:~%~a"
          (al1 alpc) (al2 alpc)))

(defmethod clone ((alpc activity-levels-pc))
  (clone-with-new-class alpc 'activity-levels-pc))

(defmethod clone-with-new-class :around ((alpc activity-levels-pc) new-class)
  (declare (ignore new-class))
  (let ((no (call-next-method)))
    (setf (slot-value no 'al1) (clone (al1 alpc))
          (slot-value no 'al2) (clone (al2 alpc)))
    no))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* activity-levels-pc/active
;;; DATE
;;; 1st March 2024
;;; 
;;; DESCRIPTION
;;; Returns T or NIL depending on whether we're active given a number between 0
;;; and 100.
;;; 
;;; ARGUMENTS
;;; The activity-levels-pc object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; the 'level', between 0 and 100 inclusive, indicating the frequency of
;;; occurrence of the result T; comparable to a percentage chance but
;;; deterministic and pattern-oriented (see the data slot state lists in
;;; activity-levels.lsp). Although this is an optional argument (because of
;;; related classes' methods) this method only really makes if it is
;;; passed. Default = 50
;;; 
;;; RETURN VALUE
;;; T or NIL
;;; 
;;; EXAMPLE
#|
(let ((alpc (make-alpc)))
  (count t (loop repeat 1000 collect (active alpc 31))))
--> 310
|#
;;; SYNOPSIS
(defmethod active ((alpc activity-levels-pc) &optional level)
;;; ****
  (unless level (setq level 50))        ; 50/50, although what's the point here?
  (unless (and (numberp level) (<= 0 level 100))
    (error "activity-levels-pc::active: 2nd argument should be a number ~
            between 0 and 100,~%not ~a" level))
  (setq level (round level)) ; level can be a float
  (cond ((zerop level) nil)
        ((= 100 level) t)
        (t (multiple-value-bind
                 (tens ones)
               (floor level 10)
             ;; use the remainder of (level % 10) to decide whether we'll return
             ;; active with the modulo + 1, or not 
             (active (al2 alpc) (if (active (al1 alpc) ones)
                                  (1+ tens)
                                  tens))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod reset ((alpc activity-levels-pc) &optional (start-at 0) ignore)
  (declare (ignore ignore))  
  (reset (al1 alpc) start-at)
  (reset (al2 alpc) start-at))

(defun make-alpc (&optional start-at id)
  (let ((alpc (make-instance 'activity-levels-pc :id id)))
    (when start-at
      (reset alpc start-at))
    alpc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF
