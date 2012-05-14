;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* change-map/simple-change-map
;;; NAME 
;;; simple-change-map
;;;
;;; File:             simple-change-map.lsp
;;;
;;; Class Hierarchy:  named-object -> linked-named-object -> sclist -> 
;;;                   circular-sclist -> assoc-list -> recursive-assoc-list ->
;;;                   sc-map -> change-map -> simple-change-map
;;;
;;; Version:          1.0.0-beta
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Implementation of the simple-change-map class which gives
;;;                   data associated with a non-recursive list of number ids.
;;;                   For example, good for specifying data which changes at
;;;                   specific bar numbers.
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    March 31st 2002
;;;
;;; $$ Last modified: 09:55:56 Thu Feb 23 2012 GMT
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

(defclass simple-change-map (change-map)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object :before ((scm simple-change-map) stream)
  (format stream "~%SIMPLE-CHANGE-MAP: "))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone ((scm simple-change-map))
  (clone-with-new-class scm 'simple-change-map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod verify-and-store :after ((scm simple-change-map))
  (when (recursivep scm)
    (error "simple-change-map::initialize-instance: Recursive data ~
            illegal!: ~a"
           scm))
  (loop for i in (data scm) do
      (unless (numberp (id i))
        (error "simple-change-map::verify-and-store: Only number ids ~
                allowed: ~a in ~a"
               (id i) (id scm))))
  (sort (data scm) #'(lambda (x y) (< (id x) (id y)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Fri May  4 17:43:47 BST 2012: Added robodoc entry

;;; MDE original comment:
;;; Section may be a simple key reference into the map or a list of references.
;;; sequence is 1-based.

;;; ****m* simple-change-map/scm-get-data
;;; FUNCTION
;;; Get the data associated with the specified key within a given
;;; simple-change-map object.
;;; 
;;; ARGUMENTS
;;; - An integer that is an existing key ID within the given simple-change-map
;;;   object. 
;;; - A simple-change-map-object.
;;; 
;;; RETURN VALUE
;;; The data associated with the specified key ID, as a named object.
;;; 
;;; EXAMPLE
#|
(let ((scm (make-simple-change-map 'bar-map '((1 3) (34 3) (38 4)))))
  (scm-get-data 34 scm))

=> 
NAMED-OBJECT: id: 34, tag: NIL, 
data: 3

|#
;;; SYNOPSIS
(defmethod scm-get-data (ref (scm simple-change-map))
;;; ****
  (when (data scm)
    (let ((result (get-data ref scm nil)))
      (if result
          result
	  (get-nearest-by-number scm ref)))))
     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Related functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Fri May  4 17:35:17 BST 2012: Added robodoc entry

;;; ****f* simple-change-map/make-simple-change-map
;;; FUNCTION
;;; Create a simple-change-map object, which stores data associated with a
;;; non-recursive list of number IDs. This object could be good, for example,
;;; for specifying data which changes at specific bar numbers.
;;; 
;;; ARGUMENTS
;;; - An ID for the simple-change-map object to be created.
;;; - A list of non-recursive lists consisting of ID/data pairs, of which the
;;;   first item is a numerical ID.
;;; 
;;; RETURN VALUE
;;; A simple-change-map object.
;;; 
;;; EXAMPLE
#|
(make-simple-change-map 'bar-map '((1 3) (34 3) (38 4)))

=> 
SIMPLE-CHANGE-MAP: 
CHANGE-MAP: last-ref-required: NIL
SC-MAP: palette id: NIL
RECURSIVE-ASSOC-LIST: recurse-simple-data: NIL
                      num-data: 3
                      linked: NIL
                      full-ref: NIL
ASSOC-LIST: warn-not-found NIL
CIRCULAR-SCLIST: current 0
SCLIST: sclist-length: 3, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: BAR-MAP, tag: NIL, 
data: (
NAMED-OBJECT: id: 1, tag: NIL, 
data: 3
**************

       
NAMED-OBJECT: id: 34, tag: NIL, 
data: 3
**************

       
NAMED-OBJECT: id: 38, tag: NIL, 
data: 4
**************
)
**************

|#
;;; SYNOPSIS
(defun make-simple-change-map (id scm)
;;; ****
  (make-instance 'simple-change-map :data scm :id id 
                 :warn-not-found nil
                 :last-ref-required nil
                 :recurse-simple-data nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun simple-change-map-p (thing)
  (typep thing 'simple-change-map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; EOF simple-change-map.lsp
