;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* sclist/change-data
;;; NAME 
;;; change-data
;;;
;;; File:             change-data.lsp
;;;
;;; Class Hierarchy:  named-object -> linked-named-object -> sclist -> 
;;;                   change-data
;;;
;;; Version:          1.0.0-beta1
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Implementation of the change-data class.  Holds data
;;;                   regarding parameter changes for a whole section
;;;                   (e.g. tempo).  For use in change-map.  The data in the
;;;                   <changes> slot is a three-element list: the sequence
;;;                   number, the bar number of the sequence where the change
;;;                   takes place (defaults to 1) and the new data (e.g. a
;;;                   tempo value).
;;;
;;;                   When giving this data, the sequence number and bar
;;;                   numbers are always integers > 0, unlike sequences
;;;                   themselves which may be given any kind of id.  Therefore
;;;                   it's OK to sort the given data according to integer
;;;                   precedence and perform numeric tests on them too.
;;;
;;;                   No public interface envisaged (so no robodoc entries).
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    2nd April 2001
;;;
;;; $$ Last modified: 20:31:51 Mon May 14 2012 BST
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

;;; 02.12.11 SEAN: Changed ROBODoc header to reflect class hierarchy

(eval-when (compile)
  (declaim (optimize (speed 3) (safety 1) (space 0) (debug 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :slippery-chicken)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; id will be the section id, data the given changes after sorting etc.

(defclass change-data (sclist)
  ;; the <last> slot of the previous data list, for using when a change is
  ;; not given right at the beginning of this section.
  ((previous-data :accessor previous-data :initform nil)
   ;; the last data change given, for passing onto the next change
   (last-data :accessor last-data :initform nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize-instance :after ((cd change-data) &rest initargs)
  (declare (ignore initargs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object :before ((cd change-data) stream)
  (format stream "~&CHANGE-DATA: ~
                  ~%            previous-data: ~a, ~
                  ~%            last-data: ~a"
          (previous-data cd) (last-data cd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone ((cd change-data))
  (clone-with-new-class cd 'change-data))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone-with-new-class :around ((cd change-data) new-class)
  (declare (ignore new-class))
  (let ((sclist (call-next-method)))
    (setf (slot-value sclist 'previous-data) (previous-data cd)
          (slot-value sclist 'last-data) (last-data cd))
    sclist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod verify-and-store :after ((cd change-data))
  (insert-missing-bar-nums cd)
  (sort-changes cd)
  (setf (last-data cd) (third (first (last (data cd))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-previous ((cd change-data) parent-ral
                         &optional (how-many 1))
  (get-previous parent-ral (previous cd) (1- how-many)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Sequence here is 1-based, not 0-based.

;;; ****m* change-data/get-change-data
;;; DESCRIPTION
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
(defmethod get-change-data ((cd change-data) sequence &optional (bar 1))
;;; ****
  (unless (and (integer>0 sequence)
               (integer>0 bar))
    (error "change-data::get-change-data ~
            Sequence and Bar arguments to get-change-data must be ~
            integers > 0: ~a ~a" sequence bar))
  ;; (print sequence) (print bar)
  (let* ((current (previous-data cd))
         (gotit nil)
         (changes-here nil)
         (result (loop 
                    for change in (data cd)
                    for s = (first change)
                    for b = (second change) 
                    for d = (third change) do
                    ;; (print change)
                      (cond ((and (= sequence s)
                                  (= bar b))
                             (setf changes-here t
                                   gotit t)
                             (return d))
                            
                            ((or (< sequence s)
                                 (and (= sequence s)
                                      (< bar b)))
                             (setf gotit t)
                             (return current))
                            (t (setf current d))))))
    (unless result
      (setf result
            (if gotit
                ;; we asked for data before any was defined....
                (warn "change-data::get-change-data: ~
                       No previous data to return: ~a ~a ~%~a"
                      sequence bar cd)
                ;; if we gave a sequence/bar number higher than the changes
                ;; made, then we just return the last change.
                (last-data cd))))
    (values result changes-here)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod sort-changes ((cd change-data))
  (setf (slot-value cd 'data)
    (sort (data cd)
          #'(lambda (x y)
              ;; there is more than one change in a single sequence
              (if (= (first x) (first y))
                  ;; there shouldn't be more than one change in a bar!
                  (if (= (second x) (second y))
                      (error "change-data::sort-changes ~
                              Change specified for the same bar of the ~
                              same sequence: ~%~a ~a  ~
                              ~%(1 inserted as bar number when ~
                              originally missing)" x y)
                    (< (second x) (second y)))
                (< (first x) (first y)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; We provide data by specifying the sequence number, a bar number and the
;;; data to be applied to this bar.  When the bar number is not given, we
;;; assume the first bar of the sequence is implied.  This function inserts the
;;; bar number 1 when it is not given as well as checks the given data for
;;; basic consistency.

(defmethod insert-missing-bar-nums ((cd change-data))
  (let ((new 
         (loop for change in (data cd) do
               (unless (listp change)
                 (error "change-data:insert-missing-bar-nums: ~
                         All change-map data is given in the form of lists: ~a"
                        change))
             collect
               (case (length change)
                 (2 (list (first change) 1 (second change)))
                 (3 change)
                 (t (error
                     "change-data:insert-missing-bar-nums: ~
                      change-map data lists should have a length of 2 or 3: ~a"
                     change))))))
    ;; NB verify-and-store calls this method, and sclists (of which change-data
    ;; is a subclass) call verify-and-store when (setf data) is called, which
    ;; would cause an endless loop if we were to call that now, so be sneeky:
    ;; set the slot-value 'data instead.
    (setf (slot-value cd 'data) new)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Related functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Fri May  4 18:08:00 BST 2012: Added robodoc entry

;;; ****f* change-data/make-change-data
;;; DESCRIPTION
;;; Create a change-data object, which holds data for use by a change-map
;;; object. The data stored in change-data object will be that of parameter
;;; changes for a whole section, such as tempo values.
;;;
;;; The data is passed to the make-change-data function as a list of
;;; three-element lists, each consisting of the number of the sequence, the
;;; number of the bar within that sequence, and the new data.
;;; 
;;; ARGUMENTS
;;; - An ID for the change-data object to be created.
;;; - A list of three-item lists, each consisting of the number of the sequence
;;;   in which the data is to change, the number of the bar within that
;;;   sequence in which the data is to change, and the data value itself. The
;;;   sequence number and bar number are always integers > 0. If no bar-number
;;;   is given, it will default to 1.
;;;
;;; RETURN VALUE
;;; A change-data object.
;;; 
;;; EXAMPLE
#|
(make-change-data 'cd-test '((1 1 23) (6 1 28) (18 1 35)))

=> 
CHANGE-DATA: 
            previous-data: NIL, 
            last-data: 35
SCLIST: sclist-length: 3, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: CD-TEST, tag: NIL, 
data: ((1 1 23) (6 1 28) (18 1 35))

|#
;;; SYNOPSIS
(defun make-change-data (id data)
;;; ****
  (make-instance 'change-data :id id :data data))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF change-data.lsp
