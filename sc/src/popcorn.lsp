;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* circular-sclist/popcorn
;;; NAME 
;;; assoc-list
;;;
;;; File:             popcorn.lsp
;;;
;;; Class Hierarchy:  named-object -> linked-named-object -> sclist -> 
;;;                   circular-sclist -> popcorn
;;;
;;; Version:          0.91
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Inspired by popping popcorn, generate a series of values
;;;                   ranging between > 0.0 and <= 1.0 by (optionally fixed)
;;;                   random selection.  Given 1 or more starting values (not
;;;                   zero) we generate tendentially increasing new values
;;;                   until we reach 1.0.  This is not a linear process,
;;;                   rather, we get spike values that increase the average
;;;                   value and thus increase the chance of further spikes.
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    3rd February 2011 (Ko Lanta, Thailand)
;;;
;;; $$ Last modified: 10:56:31 Sat Dec 17 2011 ICT
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

;;; We only subclass the circular version for convenience; it's a straight list
;;; we're generally after.  The starting data is stored in the data slot.

(defclass popcorn (circular-sclist)
  ;; our results
  ((kernels :accessor kernels :initform nil)
   ;; the running total
   (total :accessor total :initform -1.0)
   ;; the number of kernels we've generated so far
   (numk :accessor numk :initform -1)
   ;; the current average
   (mean :accessor mean :initform -1.0)
   ;; whether to use fixed-randomness (for repeatable results) or not
   (fixed-random :accessor fixed-random :initarg :fixed-random
                 :initform t)
   ;; the min/max multipliers we'll use to scale the average when creating
   ;; spikes
   (min-spike :accessor min-spike :initarg :min-spike :initform 2.0)
   (max-spike :accessor max-spike :initarg :max-spike :initform 4.0)
   ;; the min/max value of the kernels so far
   (mink :accessor mink :initform -1.0)
   (maxk :accessor maxk :initform -1.0)))
   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize-instance :after ((pc popcorn) &rest initargs)
  (declare (ignore initargs))
  ;; reset the random-rep function
  (when (fixed-random pc)
    (random-rep 1.0 t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod verify-and-store :after ((pc popcorn))
  (loop for val in (data pc) do
       (when (or (<= val 0.0) (>= val 1.0))
         (error "popcorn::verify-and-store: starting values should be ~
                 > 0.0 and < 1.0; found ~a in ~a" val (data pc))))
  (setf (total pc) (loop for k in (data pc) sum k)
        (numk pc) (length (data pc)))
  ;; all named-objects have to be able to be initialised with data=nil so that
  ;; clone works
  (when (data pc)
    (unless (> (numk pc) 1)
      (error "popcorn:verify-and-store: popcorn must be initialised with ~
              at least two values: ~a" (data pc)))
    ;; we'll be pushing new kernels in so need to reverse
    (setf (kernels pc) (reverse (data pc))
          (mink pc) (loop for k in (data pc) minimize k)
          (maxk pc) (loop for k in (data pc) maximize k))
    (set-mean pc nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone ((pc popcorn))
  (clone-with-new-class pc 'popcorn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone-with-new-class :around ((pc popcorn) new-class)
  (declare (ignore new-class))
  (let ((cscl (call-next-method)))
    (setf (slot-value cscl 'kernels) (copy-list (kernels pc))
          (slot-value cscl 'total) (total pc)
          (slot-value cscl 'numk) (numk pc)
          (slot-value cscl 'mink) (mink pc)
          (slot-value cscl 'maxk) (maxk pc)
          (slot-value cscl 'fixed-random) (fixed-random pc)
          (slot-value cscl 'min-spike) (min-spike pc)
          (slot-value cscl 'mean) (mean pc)
          (slot-value cscl 'max-spike) (max-spike pc))
    cscl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object :before ((pc popcorn) stream)
  (format stream "~%POPCORN: kernels: ~a ~%total: ~a, numk: ~a, mink: ~a, ~
                   maxk: ~a~%min-spike: ~a, max-spike: ~a, ~
                   fixed-random: ~a, mean: ~a"
          (kernels pc) (total pc) (numk pc) (mink pc) (maxk pc)
          (min-spike pc) (max-spike pc) (fixed-random pc) (mean pc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; this uses the internal slots to calculate the average and stores a new
;;; value as well.
(defmethod set-mean ((pc popcorn) new-kernel)
  (when new-kernel
    (incf (total pc) new-kernel)
    (incf (numk pc))
    (push new-kernel (kernels pc)))
  (setf (mean pc) (/ (total pc) (numk pc))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; this is the main function we'll call
(defmethod heat ((pc popcorn))
  (loop for k = (get-kernel pc) while k)
  (setf (kernels pc) (nreverse (kernels pc)))
  pc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; our values range between >0 <= 1; put them in a new range
;;; NB this doesn't change our internal state except for the kernels slot
(defmethod scale ((pc popcorn) max &optional (min 0.0) ignore1 ignore2)
  (declare (ignore ignore1)
           (ignore ignore2))
  (let ((scaler (/ (- max min) (- (maxk pc) (mink pc)))))
    (setf (kernels pc)
          (loop for k in (kernels pc) collect
               (+ min (* scaler (- k (mink pc))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod fit-to-length ((pc popcorn) length)
  (setf (kernels pc) (force-length (kernels pc) length)
        (numk pc) length))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; get the next kernel and change internal state.  returns nil when kernel >
;;; 1.0 
(defmethod get-kernel ((pc popcorn))
  (let ((k (get-kernel-aux pc)))
    (when (<= k 1.0)
      (set-mean pc k)
      (when (> k (maxk pc))
        (setf (maxk pc) k))
      (when (< k (mink pc))
        (setf (mink pc) k))
      k)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; just return the next kernel (spike or no spike) without changing internal
;;; state 
(defmethod get-kernel-aux ((pc popcorn))
  (let ((spike (<= (between 0.0 1.0 (fixed-random pc))
                   ;; so the lower the mean, the less chance i.e. the more we
                   ;; do this, the more chance of a spike
                   (mean pc))))
    (if spike
        ;; random value between min/max spikes * the current mean
        (* (between (min-spike pc) (max-spike pc) (fixed-random pc))
           (mean pc))
        ;; if this isn't a spike, just get a value between mean and max
        (between (mean pc) (maxk pc) (fixed-random pc)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; create text and data files suitable for plotting with gnuplit
;;; file should be without extension as we'll create a .txt and a .data file,
;;; for the command and data files repectively.  call gnuplot in a terminal
;;; with something like "gnuplot popcorn.txt; open popcorn.ps"
;;; draw data points connected by lines by default
(defmethod plot ((pc popcorn) file &optional (lines t))
  (with-open-file 
      (command (concatenate 'string file ".txt")
               :direction :output :if-does-not-exist :create
               :if-exists :rename-and-delete)
    (format command "~&set terminal postscript default ~%set output \"~a.ps\"~
                  ~%plot \"~a.data\" notitle ~a~%~%" file file 
                  (if lines "with linespoints" "")))
  (with-open-file 
      (data (concatenate 'string file ".data")
               :direction :output :if-does-not-exist :create
               :if-exists :rename-and-delete)
    (loop for k in (kernels pc) and x from 0 do
         (format data "~%~a ~a" x k)))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Related functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; starting-values must have at least 2 elements
(defun make-popcorn (starting-values &key (id nil) (fixed-random t)
                     (max-spike 4.0) (min-spike 2.0))
  (make-instance 'popcorn :data starting-values :id id 
                 :fixed-random fixed-random :max-spike max-spike
                 :min-spike min-spike))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; EOF popcorn.lsp

