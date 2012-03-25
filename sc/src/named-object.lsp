;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* sc/named-object
;;; NAME 
;;; named-object
;;;
;;; File:             named-object.lsp
;;;
;;; Class Hierarchy:  None: base class of all slippery-chicken classes.
;;;
;;; Version:          0.91
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Implementation of the named-object class which is the
;;;                   base class for all of the slippery-chicken classes.
;;;
;;;                   The data slot of the named-object class and its
;;;                   subclasses generally holds the original data passed when
;;;                   creating the object. In anything but the simplest of
;;;                   classes this may quickly become out-of-date as the object
;;;                   is manipulated, but is nevertheless retained so that a)
;;;                   the user can see what data was used to create an object,
;;;                   and b) the user can derive new objects from an object's
;;;                   original data. Data relevant to a specific subclass is
;;;                   often stored in slots other than :data, e.g. bars,
;;;                   rhythms, etc. so the user should not be alarmed if the
;;;                   data slot itself does not seem to reflect changes made to
;;;                   an object.   
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    4th December 2000
;;;
;;; $$ Last modified: 23:54:14 Mon Dec 12 2011 ICT
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

(defclass named-object ()
  ((id :accessor id :initarg :id :initform nil)
   ;; another name, description, tag etc. for identification but not searching
   ;; purposes  
   (tag :accessor tag :initarg :tag :initform nil)
   (data :accessor data :initarg :data :initform nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize-instance :after  ((i named-object) &rest initargs)
  (declare (ignore initargs))
  ;; (print (data i))
  (check-named-object-id-type (id i)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object :after ((i named-object) stream)
  (format stream "~&**************~%"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object ((i named-object) stream)
  (let* ((id (id i))
         (data (data i))
         ;; when id is a string, print it surrounded by double-quotes so we can
         ;; distinguish it from symbols.
         (id-print (if (stringp id)
                       (concatenate 'string "\"" id "\"")
                       id)))
    (format stream "~&NAMED-OBJECT: id: ~a, tag: ~a, ~&data: ~a"
            id-print 
            (tag i)
            (if (typep i 'sc-set)
                (get-ids-from-pitch-list data)
                data))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone ((no named-object))
  (clone-with-new-class no 'named-object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; In order for this to work, every class must be able to be initialised with
;;; all slots nil.
(defmethod clone-with-new-class ((no named-object) new-class)
  (let ((new (make-instance new-class :id nil :data nil)))
    (setf (slot-value new 'id) (basic-copy-object (id no))
          (slot-value new 'tag) (basic-copy-object (tag no))
          ;; this is the very friendly part that does all the recursive
          ;; copying of data in recursive-assoc-lists etc. 
          (slot-value new 'data) (basic-copy-object (data no)))
    new))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; clisp doesn't have change-class!!!!!!  In order for this to work, every
;;; object must be able to be initialised with just an id and a data slot. This
;;; will only work when changing a base class to one of it's derived classes
;;; which is all we need for now.

;; Don't just change for clisp, causes too many oversights when working in ACL.
;; Instead use a different function name and call that throughout. 
;; #+clisp

(defmethod sc-change-class ((no named-object) new-class)
  (clone-with-new-class no new-class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod (setf id) :before (value (i named-object))
  (check-named-object-id-type value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod statistics :before ((no named-object) &optional (stream t))
  (print (id no) stream))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod statistics ((no named-object) &optional (stream t))
  (declare (ignore stream)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod id-eq (test (i named-object))
  (let ((id (id i)))
    (when id
      (typecase id
                (string (when (stringp test) (string= id test)))
                (symbol (when (symbolp test)
                          (eq (rm-package id) (rm-package test))))
                ;; NB 1 != 1.0 
                (number (eql id test))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod combine-ids ((no1 named-object) (no2 named-object))
  (format nil "~a-~a" 
          (id no1) (id no2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod cmn-display :before ((no named-object) &key ignore1 ignore2 ignore3
                                                       ignore4 ignore5)
  (declare (ignore ignore1 ignore2 ignore3 ignore4 ignore5))
  ;; (format t "~&Resetting grace notes...")
  (setf cmn::*cmn-grace-notes-for-sc* nil))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Related functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun make-named-object (id data &optional tag)
  (make-instance 'named-object :id id :data data :tag tag))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun named-object-p (thing)
  (typep thing 'named-object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-named-object-from-derived-class (no)
  (when no
    (make-named-object (basic-copy-object (id no))
                       (basic-copy-object (data no)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; NIL is a valid id!

(defun check-named-object-id-type (arg)
  (when arg
    (unless (or (stringp arg)
                (symbolp arg)
                (numberp arg))
      (error "named-object::check-named-object-id-type: ~
              The id slot of named-object (or it's subclasses) must be a ~
              string, a symbol or a number: ~a" arg)))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun basic-copy-object (object)
  (typecase object
            (named-object (clone object))
            (list (my-copy-list object))
            (string (strcpy object))
            (t object)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; EOF named-object.lsp

