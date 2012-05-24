;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* circular-sclist/recurring-event
;;; NAME 
;;; rthm-chain
;;;
;;; File:             recurring-event.lsp
;;;
;;; Class Hierarchy:  named-object -> linked-named-object -> sclist -> 
;;;                   circular-sclist -> recurring-event
;;;
;;; Version:          1.0.0-beta2
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          class used in rthm-chain
;;;                   This class allows for the periodic/cyclic return of given
;;;                   data.  It is intended for situations where you want to
;;;                   do/collect something every several events, but the cycle
;;;                   period changes.  E.g. (the data slot is) something like
;;;                   '((2 3) (3 2) (5 3) (8 2)) which means every two events
;;;                   three times, then every 3 events twice, every 5 events
;;;                   thrice etc.
;;;
;;;                   If you want to return specific data on these cycle
;;;                   points, provide it in the the return-data slot, with the
;;;                   indices into this data in the return-data-cycle slot.
;;;
;;;                   simple example, without return-data
;;;                   (let* ((re (make-re '((2 3) (3 2) (5 3) (8 2))
;;;                                       :return-data nil 
;;;                                       :return-data-cycle nil)))
;;;                     (loop repeat 100 collect (on-it re)))
;;;
;;;                   => (NIL NIL T NIL T NIL T NIL NIL T NIL NIL T NIL NIL NIL
;;;                       NIL T NIL NIL NIL NIL T NIL NIL NIL NIL T NIL NIL NIL
;;;                       NIL NIL NIL NIL T NIL NIL NIL NIL NIL NIL NIL T NIL T
;;;                       NIL T NIL T NIL NIL T NIL NIL T NIL NIL NIL NIL T NIL
;;;                       NIL NIL NIL T NIL NIL NIL NIL T NIL NIL NIL NIL NIL
;;;                       NIL NIL T NIL NIL NIL NIL NIL NIL NIL T NIL T NIL T
;;;                       NIL T NIL NIL T NIL NIL T NIL)
;;; 
;;;                   (let* ((re (make-re '((2 3) (3 2) (5 3) (8 2)) 
;;;                                       ;; the data about to be collected 
;;;                                       :return-data '(a b c d)
;;;                                       ;; the indices into the data; this
;;;                                       ;; means we'll return A (nth 0)
;;;                                       ;; thrice, D (nth 3) twice, C once,
;;;                                       ;; and B 5x
;;;                                       :return-data-cycle 
;;;                                       '((0 3) (3 2) (2 1) (1 5))))) 
;;;                     (loop repeat 100 collect (get-it re)))
;;;
;;;                   => (NIL NIL A NIL A NIL A NIL NIL D NIL NIL D NIL NIL NIL
;;;                       NIL C NIL NIL NIL NIL B NIL NIL NIL NIL B NIL NIL NIL
;;;                       NIL NIL NIL NIL B NIL NIL NIL NIL NIL NIL NIL B NIL B
;;;                       NIL A NIL A NIL NIL A NIL NIL D NIL NIL NIL NIL D NIL
;;;                       NIL NIL NIL C NIL NIL NIL NIL B NIL NIL NIL NIL NIL
;;;                       NIL NIL B NIL NIL NIL NIL NIL NIL NIL B NIL B NIL B
;;;                       NIL A NIL NIL A NIL NIL A NIL)
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    4th February 2010
;;;
;;; $$ Last modified: 08:04:05 Sun Apr 29 2012 BST
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

;;; 02.12.11 SEAN: changed robodoc header to reflect class hierarchy

(eval-when (compile)
  (declaim (optimize (speed 3) (safety 1) (space 0) (debug 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :slippery-chicken)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass recurring-event (circular-sclist)
  ;; every how many events?
  ((current-period :accessor current-period)
   ;; how many times this period will be used
   (current-repeats :accessor current-repeats)
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
;;; MDE Sat Apr 28 11:49:47 2012 
(defmethod clone ((re recurring-event))
  (clone-with-new-class re 'recurring-event))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Sat Apr 28 11:49:50 2012 

(defmethod clone-with-new-class :around ((re recurring-event) new-class)
  (declare (ignore new-class))
  (let ((cscl (call-next-method)))
    (setf (slot-value cscl 'current-period) (current-period re)
          (slot-value cscl 'current-repeats) (current-repeats re)
          (slot-value cscl 'pcount) (pcount re)
          (slot-value cscl 'rcount) (rcount re)
          (slot-value cscl 'return-data) (my-copy-list (return-data re))
          (slot-value cscl 'return-data-length) (return-data-length re)
          (slot-value cscl 'return-data-cycle) (clone (return-data-cycle re)))
    cscl))

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

;;; SAR Fri Apr 27 17:09:56 BST 2012: Conformed robodoc entry

;;; ****m* recurring-event/get-it
;;; DESCRIPTION
;;; Get the next element from the return-data. This method is most effective
;;; when called repeatedly (e.g. within a loop) when the return-data and
;;; return-data-cycle slots have been set. In those cases the return-data-cycle
;;; element will be used as lookup into return-data. If no return-data has been
;;; specified, then the element itself will be returned.
;;; 
;;; ARGUMENTS 
;;; - A recurring-event object.
;;; 
;;; RETURN VALUE  
;;; Data from the return-data slot (or the return-data-cycle element) when
;;; we're on a boundary, otherwise NIL. 
;;;
;;; EXAMPLE
#|
;;; Used together with return-data
(let ((re (make-re '((2 3) (3 2) (5 3) (8 2)) 
                   :return-data '(a b c d)
                   :return-data-cycle '((0 3) (3 2) (2 1) (1 5)))))
  (loop repeat 50 collect (get-it re)))

=> (NIL NIL A NIL A NIL A NIL NIL D NIL NIL D NIL NIL NIL NIL C NIL NIL NIL NIL
    B NIL NIL NIL NIL B NIL NIL NIL NIL NIL NIL NIL B NIL NIL NIL NIL NIL NIL
    NIL B NIL B NIL A NIL A)

;;; Used without return-data
(let ((re (make-re '((2 3) (3 2) (5 3) (8 2)) 
                   :return-data-cycle '((0 3) (3 2) (2 1) (1 5)))))
  (loop repeat 50 collect (get-it re)))

=> (NIL NIL 0 NIL 0 NIL 0 NIL NIL 3 NIL NIL 3 NIL NIL NIL NIL 2 NIL NIL NIL NIL
    1 NIL NIL NIL NIL 1 NIL NIL NIL NIL NIL NIL NIL 1 NIL NIL NIL NIL NIL NIL
    NIL 1 NIL 1 NIL 0 NIL 0)

|#
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

;;; SAR Fri Apr 27 17:22:20 BST 2012: Conformed robodoc entry

;;; ****m* recurring-event/on-it
;;; DESCRIPTION
;;; Test to determine whether the method is currently at a period boundary. The
;;; object keeps track of its own internal state and position counter. This
;;; method is most effective when called repeatedly in a loop.
;;; 
;;; ARGUMENTS 
;;; - A recurring-event object.
;;; 
;;; RETURN VALUE  
;;; T or NIL.
;;;
;;; EXAMPLE
#|
;;; Straightforward usage
(let ((re (make-re '((2 3) (3 2) (5 3) (8 2)) 
                   :return-data '(a b c d)
                   :return-data-cycle '((0 3) (3 2) (2 1) (1 5)))))
  (loop repeat 50 collect (on-it re)))

=> (NIL NIL T NIL T NIL T NIL NIL T NIL NIL T NIL NIL NIL NIL T NIL NIL NIL NIL
    T NIL NIL NIL NIL T NIL NIL NIL NIL NIL NIL NIL T NIL NIL NIL NIL NIL NIL
    NIL T NIL T NIL T NIL T)



|#
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

;;; ****f* recurring-event/make-re
;;; DESCRIPTION
;;; Make an instance of a recurring-event object, which allows for the
;;; periodic/cyclic return of given data. The recurring-event object is
;;; intended for situations in which the user would like to perform an action
;;; or collect data every several events, but with a varying cycle period.
;;; 
;;; ARGUMENTS
;;; - A list of two-item lists that indicate the period pattern by which the
;;;   action or data collection is to be performed. For example, a value such
;;;   as '((2 3) (3 2) (5 3) (8 2)) will result in the action being performed
;;;   every 2 events three times, then every 3 events twice, every 5 events
;;;   thrice etc.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :return-data. If the recurring-event object is to be used to collect
;;;   data, that data can be specified in this slot, with the indices into this
;;;   data in the return-data-cycle slot. The return-data and return-data-cycle
;;;   slots must be used together.
;;; - :return-data-cycle. If data is specified using :return-data, the indices
;;;   into that data must be specified here. For example, the value 
;;;   '((0 3) (3 2) (2 1) (1 5)) will the data item at (nth 0) thrice, that at
;;;   (nth 3) twice, that at (nth 2) once, and that at (nth 1) five times.
;;; - :id. An optional ID can also be specified for the recurring-event object
;;;   created. 
;;; 
;;; RETURN VALUE
;;; A recurring-event object.
;;; 
;;; EXAMPLE
#|
;;; Simple usage with no specified data
(make-re '((2 3) (3 2) (5 3) (8 2)))
=> 
RECURRING-EVENT: current-period: 2, current-repeats: 3
                 pcount: -1, rcount: 0
                 return-data: NIL, return-data-cycle: NIL
CIRCULAR-SCLIST: current 1
SCLIST: sclist-length: 4, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: NIL, tag: NIL, 
data: ((2 3) (3 2) (5 3) (8 2))

;;; Usage with specified :return-data and :return-data-cycle
(make-re '((2 3) (3 2) (5 3) (8 2))
         :return-data '(a b c d)
         :return-data-cycle '((0 3) (3 2) (2 1) (1 5)))

=>
RECURRING-EVENT: current-period: 2, current-repeats: 3
                 pcount: -1, rcount: 0
                 return-data: (A B C D), return-data-cycle: 
CYCLE-REPEATS: folded: ((0 3) (3 2) (2 1) (1 5))
CIRCULAR-SCLIST: current 0
SCLIST: sclist-length: 11, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: NIL, tag: NIL, 
data: (0 0 0 3 3 2 1 1 1 1 1)
**************

CIRCULAR-SCLIST: current 1
SCLIST: sclist-length: 4, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: NIL, tag: NIL, 
data: ((2 3) (3 2) (5 3) (8 2))

|#
;;; SYNOPSIS
(defun make-re (data &key return-data return-data-cycle id)
  (make-instance 'recurring-event :id id :data data :return-data return-data
                 :return-data-cycle return-data-cycle))

(defun make-al (&optional start-at)
;;; ****
  (let ((al (make-instance 'activity-levels)))
    (when start-at
      (reset al start-at))
    al))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF recurring-event.lsp
