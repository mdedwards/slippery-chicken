;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* sclist/circular-sclist
;;; NAME 
;;; circular-sclist
;;;
;;; File:             circular-sclist.lsp
;;;
;;; Class Hierarchy:  named-object -> linked-named-object -> sclist -> 
;;;                   circular-sclist
;;;
;;; Version:          1.0.0-beta2
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
;;; $$ Last modified: 21:25:56 Mon May 14 2012 BST
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

;;; SAR Fri Jan 13 12:36:30 GMT 2012: Added robodoc info

;;; ****m* circular-sclist/get-next
;;; DESCRIPTION
;;; Get the next item in a given circular-sclist object. The class
;;; automatically keeps track of the last item retrieved. If the final item of
;;; the given circular-sclist object was the last item retrieved, the method
;;; begins again at the beginning of the list.
;;; 
;;; ARGUMENTS 
;;; - A circular-sclist object.
;;; 
;;; RETURN VALUE  
;;; An item from the given circular-sclist object.
;;; 
;;; EXAMPLE
#|
;; Repeatedly calling get-next retrieves each subsequent item from the
;; given circular-sclist object. When the list has been exhausted, retrieval
;; begins again from the head of the list.
(let ((cscl (make-cscl '(0 1 2 3 4))))
  (loop repeat 10 
     do (print (get-next cscl))))

=>
0 
1 
2 
3 
4 
0 
1 
2 
3 
4

|#
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

;;; SAR Fri Jan 13 12:48:06 GMT 2012: Added robodoc info

;;; ****m* circular-sclist/get-last
;;; DESCRIPTION
;;; Return the the most recent item retrieved in a circular-sclist object.
;;; 
;;; ARGUMENTS 
;;; - A circular-sclist object.
;;; 
;;; RETURN VALUE  
;;; An item from the given circular-sclist object.
;;; 
;;; EXAMPLE
#|
;; Retrieves the final item in the list at creation
(let ((cscl (make-cscl '(0 1 2 3 4))))
  (get-last cscl))

=> 4

;; Get and print a number of items from the list using get-next, then return
;; the most recent item retrieved using get-last
(let ((cscl (make-cscl '(0 1 2 3 4))))
  (loop repeat 7 do (print (get-next cscl)))
  (get-last cscl))

=> 1

|#
;;; SYNOPSIS
(defmethod get-last ((cscl circular-sclist))
;;; ****
  (when (data cscl)
    (let ((index (1- (if (zerop (current cscl))
                         (sclist-length cscl)
                         (current cscl)))))
      (nth index (data cscl)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Fri Jan 13 12:53:41 GMT 2012: Added robodoc info

;;; ****m* circular-sclist/at-start
;;; DESCRIPTION
;;; Determines whether the pointer for the given circular-sclist object is at
;;; the head of its list.
;;; 
;;; ARGUMENTS
;;; - A circular-sclist object.
;;; 
;;; RETURN VALUE
;;; 
;;; 
;;; EXAMPLE
#|
;; At creation the pointer is located at the start of the list
(let ((cscl (make-cscl '(0 1 2 3 4))))
  (at-start cscl))

=> T

;; Retrieve a number of the items using get-next, then determine whether the
;; pointer is located at the start of the list
(let ((cscl (make-cscl '(0 1 2 3 4))))
  (loop repeat 7 do (get-next cscl))
  (at-start cscl))

=> NIL

|#
;;; SYNOPSIS
(defmethod at-start ((cscl circular-sclist))
;;; ****
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
;;; DESCRIPTION
;;; Reset the pointer of a given circular-sclist object. The pointer is reset
;;; to 0 by default, but the desired index may be specified using the optional
;;; argument. 
;;;
;;; NB: An immediately subsequent get-next call will retrieve the item at the
;;;     index to which the pointer is reset. An immediately subsequent
;;;     get-last call will retrieve the item at the index one-less
;;;     than the value to which the pointer is set.
;;; 
;;; ARGUMENTS 
;;; - A circular-sclist object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - An index integer to which the pointer for the given circular-sclist
;;;   object should be reset.
;;;
;;; RETURN VALUE  
;;; Returns T.
;;; 
;;; EXAMPLE
#|
;; Resets to 0 by default. Here: Get a number of items using get-next, reset
;; the pointer, and apply get-next again.
(let ((cscl (make-cscl '(0 1 2 3 4))))
  (loop repeat 8 do (print (get-next cscl)))
  (reset cscl)
  (get-next cscl))

=> 0

;; Reset to a specified index
(let ((cscl (make-cscl '(0 1 2 3 4))))
  (loop repeat 8 do (print (get-next cscl)))
  (reset cscl 3)
  (get-next cscl))

=> 3

;; By default, get-last will then retrieve the item at index one less than the
;; reset value
(let ((cscl (make-cscl '(0 1 2 3 4))))
  (loop repeat 8 do (print (get-next cscl)))
  (reset cscl 3)
  (get-last cscl))

=> 2

|#
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
;;; DESCRIPTION
;;; Create a circular-sclist object from a specified list of items. The items
;;; themselves may also be lists. 
;;; 
;;; ARGUMENTS 
;;; - A list.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :id. A symbol that will be used as the ID for the created circular-sclist
;;;   object. Default = NIL.
;;; - :bounds-alert. T or NIL to indicate whether or not to print a warning if
;;;   when an attempt is made to access the object using an out-of-bounds index
;;;   number (i.e., not enough elements in the list).  T = print a
;;;   warning. Default = T.
;;; - :copy. T or NIL to indicate whether the given data list should be copied
;;;   (any slippery-chicken class instances will be cloned), with subsequent
;;;   modifications being applied to the copy. T = copy. Default = T.
;;;
;;; RETURN VALUE  
;;; A circular-sclist object.
;;; 
;;; EXAMPLE
#|
;; Returns a circular-sclist object with ID of NIL, bounds-alert=T and copy=T
;; by default
(make-cscl '(1 2 3 4))

=> 
CIRCULAR-SCLIST: current 0
SCLIST: sclist-length: 4, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: NIL, tag: NIL, 
data: (1 2 3 4)

;; Can be created using nested lists
(let ((cscl (make-cscl '((1 (4 5 6))
                         (2 (7 8 9))
                         (3 (10 11 12))))))
  (data cscl))

=> ((1 (4 5 6)) (2 (7 8 9)) (3 (10 11 12)))

;; Setting the ID
(make-cscl '(1 2 3 4) :id 'test-cscl)

=> 
CIRCULAR-SCLIST: current 0
SCLIST: sclist-length: 4, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: TEST-CSCL, tag: NIL, 
data: (1 2 3 4)

;; By default, attempts to access the object with an out-of-bounds index result 
;; in a warning being printed
(let ((cscl (make-cscl '(1 2 3 4))))
  (get-nth 11 cscl))

=>
NIL
WARNING: sclist::sclist-check-bounds: Illegal list reference: 11 
(length = 4) (sclist id = NIL)

;; This can be suppressed by creating the object with :bounds-alert set to NIL 
(let ((cscl (make-cscl '(1 2 3 4) :bounds-alert nil)))
  (get-nth 11 cscl))

=> NIL

|#
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
