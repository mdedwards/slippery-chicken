;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* _sc/recurring-event
;;; NAME 
;;; rthm-chain
;;;
;;; File:             recurring-event.lsp
;;;
;;; Class Hierarchy:  named-object -> linked-named-object -> sclist -> 
;;;                   circular-sclist -> recurring-event
;;;
;;; Version:          1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          class used in rthm-chain
;;; This class allows for the periodic/cyclic return of given data.  It is
;;; intended for situations where you want to do/collect something every
;;; several events, but the cycle period changes.  E.g. (the data slot is)
;;; something like '((2 3) (3 2) (5 3) (8 2)) which means every two events
;;; three times, then every 3 events twice, every 5 events thrice etc.
;;;
;;; If you want to return specific data on these cycle points, provide it in
;;; the the return-data slot, with the indices into this data in the
;;; return-data-cycle slot.
;;;
;;; simple example, without return-data
;;; (let* ((re (make-re '((2 3) (3 2) (5 3) (8 2)) 
;;;                    :return-data nil :return-data-cycle nil)))
;;;   (loop repeat 100 collect (on-it re)))
;;; -> (NIL NIL T NIL T NIL T NIL NIL T NIL NIL T NIL NIL NIL NIL T NIL NIL NIL
;;; NIL T NIL NIL NIL NIL T NIL NIL NIL NIL NIL NIL NIL T NIL NIL NIL NIL NIL
;;; NIL NIL T NIL T NIL T NIL T NIL NIL T NIL NIL T NIL NIL NIL NIL T NIL NIL
;;; NIL NIL T NIL NIL NIL NIL T NIL NIL NIL NIL NIL NIL NIL T NIL NIL NIL NIL
;;; NIL NIL NIL T NIL T NIL T NIL T NIL NIL T NIL NIL T NIL)
;;; 
;;; (let* ((re (make-re '((2 3) (3 2) (5 3) (8 2)) 
;;;                     ;; the data about to be collected
;;;                     :return-data '(a b c d)
;;;                     ;; the indices into the data; this means we'll return A
;;;                     ;; (nth 0) thrice, D (nth 3) twice, C once, and b 5x
;;;                     :return-data-cycle '((0 3) (3 2) (2 1) (1 5)))))
;;;   (loop repeat 100 collect (get-it re))))
;;; -> (NIL NIL A NIL A NIL A NIL NIL D NIL NIL D NIL NIL NIL NIL C NIL NIL NIL
;;; NIL B NIL NIL NIL NIL B NIL NIL NIL NIL NIL NIL NIL B NIL NIL NIL NIL NIL
;;; NIL NIL B NIL B NIL A NIL A NIL NIL A NIL NIL D NIL NIL NIL NIL D NIL NIL
;;; NIL NIL C NIL NIL NIL NIL B NIL NIL NIL NIL NIL NIL NIL B NIL NIL NIL NIL
;;; NIL NIL NIL B NIL B NIL B NIL A NIL NIL A NIL NIL A NIL)
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    4th February 2010
;;;
;;; $$ Last modified: 17:26:28 Wed Nov  2 2011 GMT
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass recurring-event (circular-sclist)
  ;; every how many events?
  ((current-period :accessor current-period :type integer)
   ;; how many times this period will be used
   (current-repeats :accessor current-repeats :type integer)
   ;; period count
   (pcount :accessor pcount :type integer :initform 0)
   ;; repeats count
   (rcount :accessor rcount :type integer :initform 0)
   ;; the list of data we're going to return in the order determined by
   ;; return-data-cycle  
   (return-data :accessor return-data :type list :initarg :return-data
                :initform nil)
   (return-data-length :accessor return-data-length :type integer :initform 0)
   ;; a cycle-repeats instance
   (return-data-cycle :accessor return-data-cycle :initarg :return-data-cycle
                      :initform nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; don't need this method (yet); will auto-use that of sclist
;;; (defmethod initialize-instance :after ((re recurring-event) &rest initargs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object :before ((re recurring-event) stream)
  (format stream "~&RECURRING-EVENT: current-period: ~a, current-repeats: ~a~
                  ~%                 pcount: ~a, rcount: ~a~
                  ~%                 return-data: ~a, return-data-cycle: ~a"
          (current-period re) (current-repeats re) (pcount re) (rcount re)
          (return-data re) (return-data-cycle re)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod verify-and-store :after ((re recurring-event))
  (loop for r in (data re) do
       (unless (and (listp r)
                    (= 2 (length r))
                    (integerp (first r))
                    (integerp (second r)))
         (error "recurring-event:initialize-instance: data must be 2 ~
                 element sublists")))
  (when (return-data-cycle re)
    (setf (return-data-cycle re) (make-instance 'cycle-repeats :data
                                                (return-data-cycle re))))
  (when (and (return-data-cycle re)
             (return-data re))
    ;; all the numbers in return-data-cycle are references (nth) into
    ;; return-data: make sure they're legal.
    (loop with len = (length (return-data re))
       for ref in (data (return-data-cycle re)) do
         (unless (integer-between ref 0 len)
           (error 
            "recurring-event::verify-and-store: indices in ~
             return-data-cycle must be legal references into return-data"))))
  ;; this has to be here as not called in super-classes and the current-* slots
  ;; would otherwise be unbound
  (reset re))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod reset :after ((re recurring-event) &optional where)
  (declare (ignore where))
  ;; (print 're-reset)
  (when (return-data-cycle re)
    (reset (return-data-cycle re)))
  (handle-return-data re)
  (period-begin re)
  ;; have to do this so we don't trigger a recurrence one too early.
  (setf (pcount re) -1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod handle-return-data ((re recurring-event))
  (let ((len (length (return-data re))))
    (setf (return-data-length re) len)
    (unless (zerop len)
      (loop for rdc in (folded (return-data-cycle re)) do
           (when (>= (first rdc) len)
             (error "recurring-event::handle-return-data: return-data-cycle ~
                     should be a list of 2-element ~%lists (offending here: ~
                     ~a), the first element ~
                     of which should be a legal ~%index into return-data ~
                     (~a). ~%If you're inititalising a rthm-chain, chances are ~
                     you changed the rests slot but ~%forgot to change ~
                     rest-cycle."
                    rdc (return-data re)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; move to the next data pair and reinitialize
(defmethod period-begin ((re recurring-event))
  (let ((new (get-next re)))
    (setf (rcount re) 0
          (pcount re) 0
          (current-period re) (first new)
          (current-repeats re) (second new))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* rthm-chain/get-it
;;; FUNCTION
;;; get-it:
;;;
;;; Call this repeatedly in a loop when the return-data and return-data-cycle
;;; slots of have been set: the return-data-cycle element will be used as
;;; lookup into return-data.  If no return-data, then the element itself will
;;; be returned.
;;; 
;;; ARGUMENTS:
;;; - the recurring-event object
;;; 
;;; RETURN VALUE: 
;;; Data from the return-data slot (or the return-data-cycle element) when
;;; we're on a boundary, otherwise nil 
;;;
;;; EXAMPLE
;;; (let* ((re (make-re '((2 3) (3 2) (5 3) (8 2)) 
;;;                     :return-data '(a b c d)
;;;                     :return-data-cycle '((0 3) (3 2) (2 1) (1 5)))))
;;;   (loop repeat 50 collect (get-it re)))
;;; -->
;;; (NIL NIL A NIL A NIL A NIL NIL D NIL NIL D NIL NIL NIL NIL C NIL NIL NIL
;;;  NIL B NIL NIL NIL NIL B NIL NIL NIL NIL NIL NIL NIL B NIL NIL NIL NIL NIL
;;;  NIL NIL B NIL B NIL A NIL A)
;;; 
;;; (let* ((re (make-re '((2 3) (3 2) (5 3) (8 2)) 
;;;                     :return-data-cycle '((0 3) (3 2) (2 1) (1 5)))))
;;;   (loop repeat 50 collect (get-it re)))
;;; -->
;;; (NIL NIL 0 NIL 0 NIL 0 NIL NIL 3 NIL NIL 3 NIL NIL NIL NIL 2 NIL NIL NIL
;;;  NIL 1 NIL NIL NIL NIL 1 NIL NIL NIL NIL NIL NIL NIL 1 NIL NIL NIL NIL NIL
;;;  NIL NIL 1 NIL 1 NIL 0 NIL 0)
;;; 
;;; SYNOPSIS
(defmethod get-it ((re recurring-event))
;;; ****
  ;; when there's no return-data, just return the first element of the
  ;; return-data-cycle pair
  (when (on-it re)
    (let ((rdc (get-next (return-data-cycle re))))
      (if (return-data re)
          (if (< rdc (return-data-length re))
              (nth rdc (return-data re))
              (error "recurring-event::get-it: return-data-cycle should only ~
                      contain legal references ~%into return-data (~a): ~a"
                     (return-data re) rdc))
          rdc))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* rthm-chain/on-it
;;; FUNCTION
;;; on-it:
;;; 
;;; Call this repeatedly in a loop to find out whether we're on a period
;;; boundary (t) or not (nil).  Object keeps track of its own internal state.
;;; 
;;; ARGUMENTS:
;;; - the recurring-event object
;;; 
;;; RETURN VALUE: 
;;; t or nil
;;; 
;;; SYNOPSIS
(defmethod on-it ((re recurring-event))
;;; ****
  (incf (pcount re))
  (if (= (pcount re) (current-period re))
      (progn
        (incf (rcount re))
        (if (= (current-repeats re) (rcount re))
            (period-begin re)
            (setf (pcount re) 0))
        t)
      nil))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Related functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-re (data &key return-data return-data-cycle id)
  (make-instance 'recurring-event :id id :data data :return-data return-data
                 :return-data-cycle return-data-cycle))

(defun make-al (&optional start-at)
  (let ((al (make-instance 'activity-levels)))
    (when start-at
      (reset al start-at))
    al))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF recurring-event.lsp
