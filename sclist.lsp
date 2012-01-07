;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* linked-named-object/sclist
;;; NAME 
;;; player
;;;
;;; File:             sclist.lsp
;;;
;;; Class Hierarchy:  named-object -> linked-named-object -> sclist
;;;
;;; Version:          1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Implementation of a simple but self-checking (hence
;;;                   sclist) list class.  
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    February 11th 2001
;;;
;;; $$ Last modified: 08:03:16 Sat Jan  7 2012 ICT
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

;;; 02.12.11 SEAN: Changed ROBODoc header to reflect class hierarchy

(in-package :slippery-chicken)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass sclist (linked-named-object)
  ;; the length of the list stored in named-object's data slot
  ((sclist-length :accessor sclist-length :type integer :initform -1)
   ;; whether the data list should be copied instead of just setf'd--yes as
   ;; default;  
   (copy :accessor copy :type boolean :initarg :copy :initform t)
   ;; whether a warning should be issued when a request for an out-of-bounds
   ;; element to set or get is given (i.e. not enough elements in list).
   (bounds-alert :type boolean :accessor bounds-alert :initarg :bounds-alert
                 :initform t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize-instance :after ((i sclist) &rest initargs)
  (declare (ignore initargs))
  (when (copy i)
    (setf (slot-value i 'data) (my-copy-list (data i))))
  (verify-and-store i))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone ((scl sclist))
  (clone-with-new-class scl 'sclist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone-with-new-class :around ((scl sclist) new-class)
  (declare (ignore new-class))
  (let ((named-object (call-next-method)))
    ;; the data list is copied by the named-object class clone method
    (setf (slot-value named-object 'copy) (copy scl)
          (slot-value named-object 'bounds-alert) (bounds-alert scl)
          (slot-value named-object 'sclist-length) (sclist-length scl))
    named-object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod (setf data) :after (value (i sclist))
  (declare (ignore value))
  (verify-and-store i))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This is a very important method that will be extended with :after variants
;;; by sclist subclasses.  Those classes should however only concern themselves
;;; with the data slot when extending this method.

(defmethod verify-and-store ((i sclist))
  (let ((data (data i)))
    (unless (listp data)
      (error "sclist::verify-and-store: ~
              The data slot of the sclist class (or subclasses) must ~
              be a list: ~a" data))
    (setf (sclist-length i) (length data))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object :before ((o sclist) stream)
  (format stream "~&SCLIST: sclist-length: ~a, bounds-alert: ~a, copy: ~a"
          (sclist-length o) (bounds-alert o) (copy o)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Safe version of subseq that checks start and end points.

;;; ****m* sclist/sc-subseq
;;; FUNCTION
;;; 
;;; 
;;; ARGUMENTS
;;; 
;;; 
;;; OPTIONAL ARGUMENTS
;;; 
;;; 
;;; RETURN VALUE
;;; 
;;; 
;;; EXAMPLE
#|

|#
;;; SYNOPSIS
(defmethod sc-subseq ((scl sclist) start finish &optional (fun #'error))
;;; ****
  (unless (and (>= start 0)
               (<= finish (sclist-length scl))
               (< start finish))
    (funcall fun "~a~%sclist::sc-subseq: Illegal indices for above list: ~
                  ~a ~a (length = ~a)" 
             (data scl) start finish (sclist-length scl)))
  (subseq (data scl) start finish))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* sclist/sc-nthcdr
;;; FUNCTION
;;; 
;;; 
;;; ARGUMENTS
;;; 
;;; 
;;; OPTIONAL ARGUMENTS
;;; 
;;; 
;;; RETURN VALUE
;;; 
;;; 
;;; EXAMPLE
#|

|#
;;; SYNOPSIS
(defmethod sc-nthcdr (nth (scl sclist))
;;; ****
  (setf (data scl) (nthcdr nth (data scl))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; add something destructively to the end of the list (inefficient of course).
;;; ****m* sclist/sclist-econs
;;; FUNCTION
;;; 
;;; 
;;; ARGUMENTS
;;; 
;;; 
;;; OPTIONAL ARGUMENTS
;;; 
;;; 
;;; RETURN VALUE
;;; 
;;; 
;;; EXAMPLE
#|

|#
;;; SYNOPSIS
(defmethod sclist-econs ((scl sclist) element)
;;; ****
  (setf (data scl) (econs (data scl) element)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Get the nth element (zero-based) of data.

;;; ****m* sclist/get-nth
;;; FUNCTION
;;; 
;;; 
;;; ARGUMENTS
;;; 
;;; 
;;; OPTIONAL ARGUMENTS
;;; 
;;; 
;;; RETURN VALUE
;;; 
;;; 
;;; EXAMPLE
#|

|#
;;; SYNOPSIS
(defmethod get-nth (index (i sclist))
;;; ****
  (when (and i (sclist-check-bounds i index))
    (nth index (data i))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Set the nth element (zero-based) of data.  NB this doesn't auto-grow the
;;; list!. 

;;; ****m* sclist/set-nth
;;; FUNCTION
;;; 
;;; 
;;; ARGUMENTS
;;; 
;;; 
;;; OPTIONAL ARGUMENTS
;;; 
;;; 
;;; RETURN VALUE
;;; 
;;; 
;;; EXAMPLE
#|

|#
;;; SYNOPSIS
(defmethod set-nth (index new-element (i sclist))
;;; ****
  (when (sclist-check-bounds i index)
    (setf (nth index (data i)) new-element)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod sclist-check-bounds ((i sclist) index)
  (let ((ok (and (integerp index) 
                 (>= index 0)
                 (< index (sclist-length i)))))
    (cond (ok t)
          ((bounds-alert i) 
           (warn "sclist::sclist-check-bounds: ~
                  Illegal list reference: ~a ~%(length = ~a) (sclist id = ~a)"
                 index (sclist-length i) (id i)))
          (t nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* sclist/sclist-remove-elements
;;; FUNCTION
;;; 
;;; 
;;; ARGUMENTS
;;; 
;;; 
;;; OPTIONAL ARGUMENTS
;;; 
;;; 
;;; RETURN VALUE
;;; 
;;; 
;;; EXAMPLE
#|

|#
;;; SYNOPSIS
(defmethod sclist-remove-elements ((scl sclist) start how-many)
;;; ****
  (sclist-check-bounds scl start)
  (sclist-check-bounds scl (1- (+ start how-many)))
  ;; do this to avoid the automatic calling of verify-and-store and
  ;; therefore the automatic calculating of the list length 
  (setf (slot-value scl 'data) (remove-elements (data scl) start how-many))
  (decf (sclist-length scl) how-many)
  scl)
                                   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* sclist/combine
;;; FUNCTION
;;; 
;;; 
;;; ARGUMENTS
;;; 
;;; 
;;; OPTIONAL ARGUMENTS
;;; 
;;; 
;;; RETURN VALUE
;;; 
;;; 
;;; EXAMPLE
#|

|#
;;; SYNOPSIS
(defmethod combine ((scl1 sclist) (scl2 sclist))
;;; ****
  (let ((result (clone scl1)))
    (setf (data result) (append (data scl1) (my-copy-list (data scl2))))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod 1st ((scl sclist))
  (get-nth 0 scl))

(defmethod 2nd ((scl sclist))
  (get-nth 1 scl))

(defmethod 3rd ((scl sclist))
  (get-nth 2 scl))

(defmethod 4th ((scl sclist))
  (get-nth 3 scl))

(defmethod 5th ((scl sclist))
  (get-nth 4 scl))

(defmethod 6th ((scl sclist))
  (get-nth 5 scl))

(defmethod 7th ((scl sclist))
  (get-nth 6 scl))

(defmethod 8th ((scl sclist))
  (get-nth 7 scl))

(defmethod 9th ((scl sclist))
  (get-nth 8 scl))

(defmethod 10th ((scl sclist))
  (get-nth 9 scl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Related functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****f* sclist/make-sclist
;;; FUNCTION
;;; 
;;; 
;;; ARGUMENTS
;;; 
;;; 
;;; OPTIONAL ARGUMENTS
;;; 
;;; 
;;; RETURN VALUE
;;; 
;;; 
;;; EXAMPLE
#|

|#
;;; SYNOPSIS
(defun make-sclist (list &key (id nil) (bounds-alert t) (copy t))
;;; ****
  (make-instance 'sclist :id id :data list :bounds-alert bounds-alert
                 :copy copy))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Lisp's copy-list does not make explicit copies of objects stored in a list.
;;; Do that here.  

(defun my-copy-list (list)
  (if (typep list 'sclist)
      (clone list)
    (loop for i in list collect 
          (basic-copy-object i))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sclist-p (thing)
  (typep thing 'sclist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; EOF sclist.lsp

