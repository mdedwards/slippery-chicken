;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* assoc-list/recursive-assoc-list
;;; NAME 
;;; recursive-assoc-list
;;;
;;; File:             recursive-assoc-list.lsp
;;;
;;; Class Hierarchy:  named-object -> linked-named-object -> sclist -> 
;;;                   circular-sclist -> assoc-list -> recursive-assoc-list
;;;
;;; Version:          1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Extension of the assoc-list class to allow and
;;;                   automatically instantiate association lists inside of
;;;                   association lists to any level of nesting. E.g.
;;;                   (setf x 
;;;                     '((1 one)
;;;                       (2 two)
;;;                       (3 ((cat "cat")
;;;                           (dog ((mickey mouse)
;;;                                 (donald duck)
;;;                                 (daffy duck)
;;;                                 (uncle ((james dean)
;;;                                         (dean martin)
;;;                                         (fred astaire)
;;;                                         (ginger ((wolfgang mozart)
;;;                                                  (johann bach)
;;;                                                  (george gershwin)))))))
;;;                           (mouse "mouse")))
;;;                       (4 four)))
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    March 18th 2001
;;;
;;; $$ Last modified: 09:14:30 Thu Jan 12 2012 ICT
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

;;; When we recurse, then the full-ref of the nested ral is the reference into
;;; the top-level ral of this ral (can be used with get-data to get to the
;;; it).

(defclass recursive-assoc-list (assoc-list)
  ;; when the data associated with a key looks like an assoc-list, i.e. it's a
  ;; 2-element list, then we recursively instantiate a recursive-assoc-list in
  ;; it's place.  If this switch is set to nil, then data of 2-element lists,
  ;; with the second element a number or symbol, will be ignored, i.e. the data
  ;; will remain as a list.  e.g. the following would normally result in a
  ;; recursive call: (y ((2 23) (7 28) (18 2)))
  ((recurse-simple-data :accessor recurse-simple-data :type boolean 
                        :initarg :recurse-simple-data :initform t)
   ;;; The reference into a top-level ral to get to this ral.  When this is
   ;;; nil, then the ral is top-level itself.
   (full-ref :accessor full-ref :type list :initarg :full-ref 
	     :initform nil)
   (num-data :accessor num-data :type integer :initform -1)
   ;; whether or not we've already called link-named-objects (which is a bit of
   ;; an expensive function that we won't always need to call so only do it
   ;; when necessary and remember that we already did it). 
   (linked :accessor linked :type boolean :initform nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone ((ral recursive-assoc-list))
  (clone-with-new-class ral 'recursive-assoc-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The clone method of the named-object class handles the copying of the data
;;; lists, even recursively! 

(defmethod clone-with-new-class :around ((ral recursive-assoc-list) new-class)
  (declare (ignore new-class))
  (let ((al (call-next-method)))
    (setf (slot-value al 'recurse-simple-data) (recurse-simple-data ral)
          (slot-value al 'num-data) (num-data ral)
          (slot-value al 'full-ref) (basic-copy-object (full-ref ral))
          (slot-value al 'linked) (linked ral))
    al))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This will return a ral where all the data slots are nil but the recursive
;;; structure is duplicated.  Useful, for example, for duplicating the section
;;; structure as defined in a rthm-seq-map, but leaving out the data for that
;;; object, allowing it to be filled with other data, for instance with data
;;; that a piece object contains.

(defmethod duplicate-structure ((ral recursive-assoc-list) &optional new-class)
  (let ((result (make-ral (basic-copy-object (id ral)) nil)))
    (setf (data result)
      (loop for object in (data ral)
          for data = (data object)
          collect (make-named-object (basic-copy-object (id object))
                                     (if (is-ral data)
                                         (duplicate-structure data)
                                       nil))))
    (if new-class 
        (clone-with-new-class result new-class)
      result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Get the previous named-object in the ral (or <how-many previous>), looking
;;; into all recursive rals along the way.

;;; SAR Thu Jan 26 14:03:17 GMT 2012: Added robodoc info

;;; ****m* recursive-assoc-list/get-previous
;;; FUNCTION
;;; Get the previous named-object in the given recursive-assoc-list object by
;;; specifying the ID of a named-object contained within that given
;;; recursive-assoc-list object.  
;;;
;;; An optional argument allows for the retrieval of a previous named-object
;;; that is more than one step back in the given recursive-assoc-list object
;;; (i.e., not the named-object that immediately precedes the specified key).
;;;
;;; If the number given for the optional <how-many> argument is greater than
;;; the number of items in the given recursive-assoc-list object, the value
;;; returned will be a negative number.
;;;
;;; The method proceeds linearly, not hierarchically, when getting previous
;;; named-objects from further down into nested assoc-lists. In other words,
;;; the named-object immediately previous to (white ribbon) in this nested list 
;;; is (fox hole), which is at a deeper level, not (red ...) or (blue velvet),
;;; which are at the same level: 
;;; ((blue velvet) 
;;;  (red ((dragon den) 
;;;        (viper nest) 
;;;        (fox hole))) 
;;;  (white ribbon)
;;;
;;; In order to retrieve objects that are nested more deeply, the list that is
;;; the <keys> argument must consist of the consecutive path of keys leading to
;;; that object. If only the key of a named object that is deeper in the list
;;; is given, and not the path of keys to that object, a warning will be
;;; printed that the given key cannot be found in the list.
;;;
;;; NB: When this method is applied to keys that contain further assoc-list
;;;     objects, the method will drop into the debugger with an error. 
;;; 
;;; ARGUMENTS
;;; - A recursive-assoc-list object.
;;; - A list containing one or more symbols that are either the ID of the
;;;   specified named object or the path of keys to that object within the
;;;   given recursive-assoc-list object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - An integer indicating how many steps back in the given
;;;   recursive-assoc-list from the specified named-object to look when
;;;   retrieving the desired ojbect (e.g. 1 = immediately previous object, 2 =
;;;   the one before that etc.)
;;; 
;;; RETURN VALUE
;;; A linked-named-object.
;;; 
;;; EXAMPLE
#|

;; Get the object immediately previous to that with the key WILD returns the
;; object with key JIM and data BEAM
(let ((ral (make-ral 'mixed-bag 
		     '((jim beam)
		       (wild turkey)
		       (four ((roses red)
			      (violets ((blue velvet)
					(red ((dragon den)
					      (viper nest)
					      (fox hole)))
					(white ribbon)))))))))
  (get-previous ral '(wild)))

=> 
LINKED-NAMED-OBJECT: previous: NIL, this: (JIM), next: (WILD)
NAMED-OBJECT: id: JIM, tag: NIL, 
data: BEAM

;; Attempting to get the previous object from the key FOUR, which contains a
;; nested list, returns an error unless the first key in the nested list is
;; also included
(let ((ral (make-ral 'mixed-bag 
		     '((jim beam)
		       (wild turkey)
		       (four ((roses red)
			      (violets ((blue velvet)
					(red ((dragon den)
					      (viper nest)
					      (fox hole)))
					(white ribbon)))))))))
  (get-previous ral '(four)))

=>
There is no applicable method for the generic function
  #<STANDARD-GENERIC-FUNCTION PREVIOUS (1)>
when called with arguments
  (
NAMED-OBJECT: id: FOUR, tag: NIL, 

(let ((ral (make-ral 'mixed-bag 
		     '((jim beam)
		       (wild turkey)
		       (four ((roses red)
			      (violets ((blue velvet)
					(red ((dragon den)
					      (viper nest)
					      (fox hole)))
					(white ribbon)))))))))
  (get-previous ral '(four roses)))

=> 
LINKED-NAMED-OBJECT: previous: (JIM), this: (WILD), next: (FOUR ROSES)
NAMED-OBJECT: id: WILD, tag: NIL, 
data: TURKEY

;; The method defines the previous object linearly, not hierarchically; i.e.,
;; the previous object to (white ribbon) here is (fox hole) and not (red ...)
(let ((ral (make-ral 'mixed-bag 
		     '((jim beam)
		       (wild turkey)
		       (four ((roses red)
			      (violets ((blue velvet)
					(red ((dragon den)
					      (viper nest)
					      (fox hole)))
					(white ribbon)))))))))
  (get-previous ral '(four violets white)))

=> 
LINKED-NAMED-OBJECT: previous: (FOUR VIOLETS RED VIPER), 
this: (FOUR VIOLETS RED FOX), 
next: (FOUR VIOLETS WHITE)
NAMED-OBJECT: id: FOX, tag: NIL, 
data: HOLE

;; Use the <how-many> argument to retrieve previous objects further back than
;; the immediate predecessor
(let ((ral (make-ral 'mixed-bag 
		     '((jim beam)
		       (wild turkey)
		       (four ((roses red)
			      (violets ((blue velvet)
					(red ((dragon den)
					      (viper nest)
					      (fox hole)))
					(white ribbon)))))))))
  (get-previous ral '(four violets white) 4))

=> 
LINKED-NAMED-OBJECT: previous: (FOUR ROSES), 
this: (FOUR VIOLETS BLUE), 
next: (FOUR VIOLETS RED DRAGON)
NAMED-OBJECT: id: BLUE, tag: NIL, 
data: VELVET

;; Using a <how-many> value greater than the number of items in the given
;; recursive-assoc-list object returns a negative number
(let ((ral (make-ral 'mixed-bag 
		     '((jim beam)
		       (wild turkey)
		       (four ((roses red)
			      (violets ((blue velvet)
					(red ((dragon den)
					      (viper nest)
					      (fox hole)))
					(white ribbon)))))))))
  (get-previous ral '(four violets white) 14))

=> -7

|#
;;; SYNOPSIS
(defmethod get-previous ((ral recursive-assoc-list) keys 
                         &optional (how-many 1))
;;; ****
  (link-named-objects ral)
  (let ((this (get-data keys ral)))
    (when this
      (loop for i downfrom how-many above 0 do
            (setf this (get-data (previous this) ral nil))
            (unless this 
              (return (- i)))
          finally (return this)))))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object :before ((ral recursive-assoc-list) stream)
  (format stream "~&RECURSIVE-ASSOC-LIST: recurse-simple-data: ~a~
                  ~%                      num-data: ~a~
                  ~%                      linked: ~a~
                  ~%                      full-ref: ~a"
          (recurse-simple-data ral)
          (num-data ral)
          (linked ral)
          (full-ref ral)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod verify-and-store :after ((ral recursive-assoc-list))
  (loop for i in (data ral) do
        (when (and (typep i 'named-object) 
                   (lisp-assoc-listp (data i)
                                     (recurse-simple-data ral)))
          (setf (data i) 
            (make-ral (format nil "sub-ral-of-~a" (id ral))
                      (data i)
                      :full-ref (econs (full-ref ral) (id i))
                      :recurse-simple-data (recurse-simple-data ral)
                      :warn-not-found (warn-not-found ral)))))
  ;; psp's parse their data later so don't do this here.
  (unless (typep ral 'pitch-seq-palette)
    (setf (num-data ral) (r-count-elements ral))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Thu Jan 26 20:50:19 GMT 2012: Added robodoc info

;;; ****m* recursive-assoc-list/relink-named-objects
;;; FUNCTION
;;; This method is essentially the same as the method link-named objects, but
;;; resets the LINKED slot to NIL and forces the link-named-objects method to
;;; be applied again.
;;; 
;;; ARGUMENTS
;;; - A recursive-alloc-list object.
;;; 
;;; RETURN VALUE
;;; A recursive-alloc-list object.
;;; 
;;; EXAMPLE
#|
;; Usage as presented here; see the documentation for method link-named-objects
;; for more detail
(let ((ral (make-ral 'mixed-bag 
		     '((jim beam)
		       (wild turkey)
		       (four ((roses red)
			      (violets ((blue velvet)
					(red ((dragon den)
					      (viper nest)
					      (fox hole)))
					(white ribbon)))))))))
  (relink-named-objects ral))

=>
RECURSIVE-ASSOC-LIST: recurse-simple-data: T
                      num-data: 8
                      linked: T
                      full-ref: NIL
ASSOC-LIST: warn-not-found T
CIRCULAR-SCLIST: current 0
SCLIST: sclist-length: 3, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: MIXED-BAG, tag: NIL, 
data: (
LINKED-NAMED-OBJECT: previous: NIL, this: (JIM), next: (WILD)
NAMED-OBJECT: id: JIM, tag: NIL, 
data: BEAM
[...]

|#
;;; SYNOPSIS
(defmethod relink-named-objects ((ral recursive-assoc-list))
;;; ****
  (setf (linked ral) nil)
  (link-named-objects ral))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; MDE's original comment:
;;; From the named objects in the data slots of the rals, create
;;; linked-named-objects to hold pointers (actually keys) to the previous and
;;; next objects in the ral, whether recursive or not. The previous and next
;;; args are only for recursive calls (and almost certainly because I'm too
;;; tired right now to figure out how to do the recursion properly :)  

;;; SAR Thu Jan 26 16:21:49 GMT 2012: Edited robodoc info

;;; ****m* recursive-assoc-list/link-named-objects
;;; FUNCTION
;;; Create linked-named-objects from the named-objects in the data slots of the
;;; given recursive-assoc-list object. The linked-named-objects created hold
;;; keys that serve as pointers to the previous and next objects in the given
;;; recursive-assoc-list object, whether recursive or not. 
;;;
;;; The optional <previous> and <higher-next> arguments are only for recursive
;;; calls. 
;;; 
;;; ARGUMENTS 
;;; - A recursive-assoc-list object.
;;;
;;; OPTIONAL ARGUMENTS
;;; - <previous>
;;; - <higher-next>
;;;
;;; EXAMPLE
#|

;;; The recursive-assoc-list may not be linked on creation, evident here
;;; through the value of the LINKED slot
(make-ral 'mixed-bag 
		 '((jim beam)
		   (wild turkey)
		   (four ((roses red)
			  (violets ((blue velvet)
				    (red ((dragon den)
					  (viper nest)
					  (fox hole)))
				    (white ribbon)))))))

=>
RECURSIVE-ASSOC-LIST: recurse-simple-data: T
                      num-data: 8
                      linked: NIL
                      full-ref: NIL
ASSOC-LIST: warn-not-found T
CIRCULAR-SCLIST: current 0
SCLIST: sclist-length: 3, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: MIXED-BAG, tag: NIL, 
data: (
NAMED-OBJECT: id: JIM, tag: NIL, 
data: BEAM
       
NAMED-OBJECT: id: WILD, tag: NIL, 
data: TURKEY
[...]

;; The recursive-assoc-list object and the named-objects it contains are linked
;; after applying the link-named-objects method
(let ((ral (make-ral 'mixed-bag 
		     '((jim beam)
		       (wild turkey)
		       (four ((roses red)
			      (violets ((blue velvet)
					(red ((dragon den)
					      (viper nest)
					      (fox hole)))
					(white ribbon)))))))))
  (link-named-objects ral))

=>
RECURSIVE-ASSOC-LIST: recurse-simple-data: T
                      num-data: 8
                      linked: T
                      full-ref: NIL
ASSOC-LIST: warn-not-found T
CIRCULAR-SCLIST: current 0
SCLIST: sclist-length: 3, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: MIXED-BAG, tag: NIL, 
data: (
LINKED-NAMED-OBJECT: previous: NIL, this: (JIM), next: (WILD)
NAMED-OBJECT: id: JIM, tag: NIL, 
data: BEAM
       
LINKED-NAMED-OBJECT: previous: (JIM), this: (WILD), next: (FOUR ROSES)
NAMED-OBJECT: id: WILD, tag: NIL, 
data: TURKEY

|#
;;; 
;;; RETURN VALUE  
;;; the recursive-assoc-list object
;;; 
;;; SYNOPSIS
(defmethod link-named-objects ((ral recursive-assoc-list) 
                               &optional previous higher-next)
;;; ****
  ;; don't do it again!
  (unless (linked ral)
    (let ((next nil)
          (next-in-list nil)
          (next-in-list-data nil)
          (data (data ral))
          (this-data nil)
          (this-ref nil)
          (ral-full-ref (full-ref ral)))
      (loop for this in data and i from 0 do
           (setf this-ref (econs ral-full-ref (id this))
                 next-in-list (nth (1+ i) data)
                 next-in-list-data (when next-in-list (data next-in-list))
                 next (cond ((is-ral next-in-list-data)
                             (get-first-ref next-in-list-data))
                            (next-in-list 
                             (econs ral-full-ref (id next-in-list)))
                            ;; when we're at the end of the data list, then
                            ;; next as calculated above is nil, but it could
                            ;; be that this ral is a nested one so the next is
                            ;; the next in the ral one-level higher up,
                            ;; i.e. the higher-next arg that the method was
                            ;; recursively called with.
                            ((null next-in-list) higher-next))
                 this-data (data this))
           (if (is-ral this-data)
               (progn 
                 ;; in case we're relinking
                 (setf (linked this-data) nil)
                 (link-named-objects this-data previous next)
                 ;; previous is the last ref in the this ral!
                 (setf previous (get-last-ref this-data)))
               (progn 
                 (if (typep this 'linked-named-object)
                     (setf (previous this) previous
                           (this this) this-ref
                           (next this) next)
                     ;; Here's where we create the new linked-named-object and
                     ;; replace the current named-object with it.
                     (setf (nth i (data ral)) (no-to-lno this previous this-ref
                                                         next)))
                 (setf previous this-ref)))))
    ;; remember that we did this!
    (setf (linked ral) t))
  ral)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Thu Jan 26 20:54:14 GMT 2012: Added robodoc info

;;; ****m* recursive-assoc-list/r-count-elements
;;; FUNCTION
;;; Return the total number of elements recursively (accross all depths) of the
;;; given recursive-assoc-list object.
;;; 
;;; ARGUMENTS
;;; - A recursive-assoc-list object.
;;; 
;;; RETURN VALUE
;;; An integer.
;;; 
;;; EXAMPLE
#|
(let ((ral (make-ral 'mixed-bag 
		     '((jim beam)
		       (wild turkey)
		       (four ((roses red)
			      (violets ((blue velvet)
					(red ((dragon den)
					      (viper nest)
					      (fox hole)))
					(white ribbon)))))))))
  (r-count-elements ral))

=> 8

|#
;;; SYNOPSIS
(defmethod r-count-elements ((ral recursive-assoc-list))
;;; ****
  (let ((len (sclist-length ral)))
    (loop for thing in (data ral) do
          (when (is-ral (data thing))
            ;; we're only interested in the data, not how many ids there are.
            (decf len)
            (incf len (r-count-elements (data thing)))))
    len))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Thu Jan 26 20:59:03 GMT 2012: Added robodoc info

;;; ****m* recursive-assoc-list/get-data
;;; FUNCTION
;;; Return the named-object (or linked-named-object) that is identified by a
;;; specified key within a given recursive-assoc-list object.
;;;
;;; NB: This method returns the named object itself, not jus the data
;;;     associated with the key (use assoc-list::get-data-data for that). 
;;; 
;;; ARGUMENTS
;;; - A symbol that is the key (id) of the named-object sought, or a list of
;;;   symbols that are the path to the desired named-object within the given
;;;   recursive-assoc-list.
;;; - The recursive-assoc-list object in which it is sought.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to indicate whether a warning is printed if the specified key
;;;   cannot be found within the given assoc-list. T = print. Default = T.
;;; 
;;; RETURN VALUE
;;; A named-object is returned if the specified key is found within the given
;;; recursive-assoc-list object. 
;;;
;;; NIL is returned and a warning is printed if the specified key is not found
;;; in the given recursive-assoc-list object. This applies, too, when a nested
;;; key is specified without including the other keys that are the path to that
;;; key (see example).
;;; 
;;; EXAMPLE
#|

;; Get a named-object from the top-level of the recursive-assoc-list object
(let ((ral (make-ral 'mixed-bag 
		     '((jim beam)
		       (wild turkey)
		       (four ((roses red)
			      (violets ((blue velvet)
					(red ((dragon den)
					      (viper nest)
					      (fox hole)))
					(white ribbon)))))))))
  (get-data 'wild ral))

=> 
NAMED-OBJECT: id: WILD, tag: NIL, 
data: TURKEY

;; A list including all keys that are the path to the specified key is required
;; to get nested named-objects
(let ((ral (make-ral 'mixed-bag 
		     '((jim beam)
		       (wild turkey)
		       (four ((roses red)
			      (violets ((blue velvet)
					(red ((dragon den)
					      (viper nest)
					      (fox hole)))
					(white ribbon)))))))))
  (get-data '(four violets white) ral))

=> 
NAMED-OBJECT: id: WHITE, tag: NIL, 
data: RIBBON

;; Searching for a key that is not present in the given recursive-assoc-list
;; object returns NIL and a warning
(let ((ral (make-ral 'mixed-bag 
		     '((jim beam)
		       (wild turkey)
		       (four ((roses red)
			      (violets ((blue velvet)
					(red ((dragon den)
					      (viper nest)
					      (fox hole)))
					(white ribbon)))))))))
  (get-data 'johnnie ral))

=> NIL
WARNING:
   assoc-list::get-data: Could not find data with key JOHNNIE 
   in assoc-list with id MIXED-BAG

;; Searching for a nested key without specifying the path to that key within a
;; list also returns a NIL and a warning
(let ((ral (make-ral 'mixed-bag 
		     '((jim beam)
		       (wild turkey)
		       (four ((roses red)
			      (violets ((blue velvet)
					(red ((dragon den)
					      (viper nest)
					      (fox hole)))
					(white ribbon)))))))))
  (get-data 'fox ral))

=> NIL
WARNING:
   assoc-list::get-data: Could not find data with key FOX 
   in assoc-list with id MIXED-BAG

|#
;;; SYNOPSIS
(defmethod get-data :around (ids (ral recursive-assoc-list) 
                                 &optional (warn t))
;;; ****
  (unless (listp ids)
    (setf ids (list ids)))
  (let* ((next (call-next-method (first ids) ral warn))
         (next-data (when next (data next))))
    (if (endp (rest ids))
        next
      (if (is-ral next-data)
          (get-data (rest ids) next-data warn)
        (if (is-ral next)
            (get-data (rest ids) next warn)
          (progn 
            (when (and warn (warn-not-found ral))
              (warn "recursive-assoc-list::get-data: ~
                     Couldn't find data with key(s) ~a in ~
                     recursive-assoc-list with id ~a"
                    ids (id ral)))
            nil))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Thu Jan 26 21:28:54 GMT 2012: Added robodoc info

;;; ****m* recursive-assoc-list/add
;;; FUNCTION
;;; Add a new element (key/data pair) to the given recursive-assoc-list
;;; object. 
;;;
;;; If no value is specified for the optional argument, the new element is
;;; added at the end of the top level. The optional argument allows for the
;;; FULL-REF to be specified, i.e. a recursive path of keys down to the nested
;;; level where the new element is to be placed.
;;; 
;;; ARGUMENTS
;;; - A key/data pair.
;;; - A recursive-assoc-list object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - A list that is the FULL-REF, i.e. a recursive path of keys, down to the
;;;   nested level where the new element is to be placed. 
;;; 
;;; RETURN VALUE
;;; Returns T if the specified named-object is successfully added to the given
;;; recursive-assoc-list. 
;;;
;;; Returns an error if an attempt is made to add NIL to the given
;;; recursive-assoc-list or if the given named-object is already present at the
;;; same level within the given recursive-assoc-list. 
;;; 
;;; EXAMPLE
#|

;; Adding an element while specifiying no optional argument results in the new
;; element being placed at the end of the top level by default (evident here by
;; the fact that the ref for (MAKERS) is a single-item list) 
(let ((ral (make-ral 'mixed-bag 
		     '((jim beam)
		       (wild turkey)
		       (four ((roses red)
			      (violets ((blue velvet)
					(red ((dragon den)
					      (viper nest)
					      (fox hole)))
					(white ribbon)))))))))
  (add '(makers mark) ral)
  (get-all-refs ral))

=> ((JIM) (WILD) (FOUR ROSES) (FOUR VIOLETS BLUE) (FOUR VIOLETS RED DRAGON)
    (FOUR VIOLETS RED VIPER) (FOUR VIOLETS RED FOX) (FOUR VIOLETS WHITE)
    (MAKERS))

;; A list that is a path of keys (FULL-REF) to the desired recursive level must
;; be given as the optional argument in order to place the specified element
;; deeper in the given recursive-assoc-list object
(let ((ral (make-ral 'mixed-bag 
		     '((jim beam)
		       (wild turkey)
		       (four ((roses red)
			      (violets ((blue velvet)
					(red ((dragon den)
					      (viper nest)
					      (fox hole)))
					(white ribbon)))))))))
  (add '(yellow sky) ral '(four violets))
  (get-all-refs ral))

=> ((JIM) (WILD) (FOUR ROSES) (FOUR VIOLETS BLUE) (FOUR VIOLETS RED DRAGON)
    (FOUR VIOLETS RED VIPER) (FOUR VIOLETS RED FOX) (FOUR VIOLETS WHITE)
    (FOUR VIOLETS YELLOW))

;; Attempting to add an element that is already present at the given level of
;; the given recursive-assoc-list object results in an error
(let ((ral (make-ral 'mixed-bag 
		     '((jim beam)
		       (wild turkey)
		       (four ((roses red)
			      (violets ((blue velvet)
					(red ((dragon den)
					      (viper nest)
					      (fox hole)))
					(white ribbon)))))))))
  (add '(makers mark) ral)
  (add '(makers mark) ral))

=>
assoc-list::add: Can't add MAKERS to assoc-list with id MIXED-BAG
    because key already exists!
   [Condition of type SIMPLE-ERROR]

;; Attempting to add NIL also results in an error
(let ((ral (make-ral 'mixed-bag 
		     '((jim beam)
		       (wild turkey)
		       (four ((roses red)
			      (violets ((blue velvet)
					(red ((dragon den)
					      (viper nest)
					      (fox hole)))
					(white ribbon)))))))))
  (add '() ral))

=>
assoc-list::add: named-object is NIL!
   [Condition of type SIMPLE-ERROR]

|#
;;; SYNOPSIS
(defmethod add (named-object (ral recursive-assoc-list) &optional ref)
;;; ****
  (let ((where (if ref 
                   (data (get-data ref ral))
                   ral)))
    (unless (is-ral where)
      (error "recursive-assoc-list::add: can't add to reference ~a ~
              because object with this reference is not a ~
              recursive-assoc-list!"
             ref))
    (call-next-method named-object where)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod add :after (named-object (ral recursive-assoc-list)
                       &optional ignore)
  (declare (ignore ignore named-object))
  (incf (num-data ral)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Allows new rals to be created automatically; however this assumes that if
;;; you reference a key that exists, then it's data is a list that data will be
;;; added to the end of.

;;; ****m* recursive-assoc-list/ral-econs
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
(defmethod ral-econs (data key (ral recursive-assoc-list))
;;; ****
  (unless (listp key)
    (setf key (list key)))
  (let ((butlast (butlast key))
        (last (first (last key))))
    (unless (get-data key ral nil)
      (loop 
          for k in butlast
          with keys = '()
          do
            (unless (get-data (reverse (cons k keys)) ral nil)
              (add (make-named-object k (make-ral nil nil)) 
                   (if keys 
                       (get-data-data (reverse keys) ral)
                     ral)))
            (push k keys))
      (add (make-named-object last nil)
           (get-data-data butlast ral)))
    (set-data key (list last (econs (get-data-data key ral) data))
              ral)
    data))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; ****m* recursive-assoc-list/parcel-data
;;; FUNCTION
;;; Put all the data into a named-object, i.e. add a level of recursion.  This
;;; is a means of making a collection of data before perhaps adding more with
;;; potentially conflicting ids.
;;; 
;;; ARGUMENTS 
;;; - the recursive-assoc-list
;;; - the top-level new id for the current data
;;; 
;;; RETURN VALUE  
;;; the new recursive-assoc-list
;;; 
;;; DATE 10.4.10
;;; 
;;; SYNOPSIS
(defmethod parcel-data ((ral recursive-assoc-list) new-id)
;;; ****
  (make-ral (id ral) (list (make-named-object new-id ral))
            :tag (if (tag ral) (tag ral) 'from-parcel-data)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; add an empty level of recursion
;;; 
;;; ****m* recursive-assoc-list/add-empty-parcel
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
(defmethod add-empty-parcel ((ral recursive-assoc-list) id &optional new-class)
;;; ****
  (let ((sub-ral (make-ral (format nil "sub-ral-of-~a" (id ral))
                           nil)))
    (when new-class
      (setf sub-ral (clone-with-new-class sub-ral new-class)))
    (add (make-named-object id sub-ral)
         ral)
    sub-ral))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* recursive-assoc-list/set-data
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
(defmethod set-data (key new-value (ral recursive-assoc-list))
;;; ****
  (if (or (atom key)
          (= 1 (length key)))
      (call-next-method (if (atom key) key (first key)) new-value ral)
    (let* ((deepest-ref (butlast key))
           (deepest-replace (first (last key)))
           (deepest-ral (when deepest-ref (data (get-data deepest-ref ral)))))
      (when deepest-ral
        (call-next-method deepest-replace new-value deepest-ral)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod set-nth-of-data (key nth new-value (ral recursive-assoc-list))
  (if (atom key)
      (call-next-method)
      (let* ((deepest-ref (butlast key))
             (deepest-replace (first (last key)))
             (deepest-ral (when deepest-ref (data (get-data deepest-ref ral)))))
        (when deepest-ral
          (call-next-method deepest-replace nth new-value deepest-ral)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* recursive-assoc-list/get-first
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
(defmethod get-first ((ral recursive-assoc-list))
;;; ****
  (let* ((first (call-next-method))
         (data-first (when first (data first))))
    (if (is-ral data-first)
        (get-first data-first)
      first)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Get the full reference in to the ral of the first named-object in the
;;; ral. N.B If the ral argument happens to be part of another ral, i.e. is a
;;; nested ral, then the result is the reference into the top-level ral, not
;;; the argument.

;;; ****m* recursive-assoc-list/get-first-ref
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
(defmethod get-first-ref ((ral recursive-assoc-list))
;;; ****
  (let* ((first (first (data ral)))
         (first-data (data first)))
    (if (is-ral first-data)
        (get-first-ref first-data)
      (econs (full-ref ral) (id first)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* recursive-assoc-list/get-last
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
(defmethod get-last ((ral recursive-assoc-list))
;;; ****
  (let* ((last (call-next-method))
         (last-data (data last)))
    (if (is-ral last-data)
        (get-last last-data)
      last)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* recursive-assoc-list/get-all-refs
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
(defmethod get-all-refs ((ral recursive-assoc-list) 
                         &optional 
                         (single-ref-as-list t))
;;; ****
  (when (data ral)
    (link-named-objects ral)
    (loop 
        with ref = (get-first-ref ral) 
        while ref 
        for current = (get-data ref ral)
        for this = (when current (this current))
        do (when (and (not single-ref-as-list)
                      (= 1 (length this)))
             (setf this (first this)))
        collect this
        do (setf ref (when current (next current))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* recursive-assoc-list/get-last-ref
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
(defmethod get-last-ref ((ral recursive-assoc-list))
;;; ****
  (let* ((last (first (last (data ral))))
         (last-data (data last)))
    (if (is-ral last-data)
        (get-last-ref last-data)
      (econs (full-ref ral) (id last)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Check whether the data really is recursive

;;; ****m* recursive-assoc-list/recursivep
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
(defmethod recursivep ((ral recursive-assoc-list))
;;; ****
  (loop for i in (data ral)
      if (is-ral (data i)) do (return t)
      finally (return nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Related functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Wed Jan 25 11:36:51 GMT 2012: Added robodoc info

;;; ****f* recursive-assoc-list/make-ral
;;; FUNCTION
;;; Create a recursive-assoc-list object, whic allows and automatically
;;; instantiates association lists inside of association lists to any level of
;;; nesting. 
;;; 
;;; ARGUMENTS
;;; - A symbol that is the object's ID.
;;; - A list of nested lists, or a list.
;;; 
;;; OPTIONAL ARGUMENTS

;;; - keyword argument :recurse-simple-data. T or NIL to indicate whether to
;;;   recursively instantiate a recursive-assoc-list in place of data that
;;;   appears to be a simple assoc-list (i.e. a 2-element list). If NIL, the
;;;   data of 2-element lists whose second element is a number or a symbol will
;;;   be ignored, therefore remaining as a list. For example, this data would
;;;   normally result in a recursive call: (y ((2 23) (7 28) (18 2))). 
;;;   T = replace assoc-list data with recursive-assoc-lists. Default = T. 
;;; - keyword argument :full-ref. Nil or a list representing the path to a
;;;   nested recursive-assoc-list object within the given recursive-assoc-list
;;;   object, starting from the top level of the given object. When NIL, the
;;;   given recursive-assoc-list object itself is the top level. 
;;;   Default = NIL. 
;;; - keyword argument :tag. A symbol that is another name, description
;;;   etc. for the given recursive-assoc-list object. The tag may be used for
;;;   identification but not for searching purposes. Default = NIL.
;;; - keyword argument :warn-not-found. T or NIL to indicate whether a warning
;;;   is printed when an index which doesn't exist is used for lookup.
;;;   Default = T. 
;;; 
;;; RETURN VALUE
;;; Returns a recursive-assoc-list object.
;;; 
;;; EXAMPLE
#|
;; Create a recursive-assoc-list object with default keyword argument values 
(make-ral 'mixed-bag 
	  '((jim beam)
	    (wild turkey)
	    (four ((roses red)
		   (violets ((blue velvet)
			     (red ((dragon den)
				   (viper nest)
				   (fox hole)))
			     (white ribbon)))))))

=> 
RECURSIVE-ASSOC-LIST: recurse-simple-data: T
                      num-data: 8
                      linked: NIL
                      full-ref: NIL
ASSOC-LIST: warn-not-found T
CIRCULAR-SCLIST: current 0
SCLIST: sclist-length: 3, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: MIXED-BAG, tag: NIL, 
data: (
[...]

;; Use the class's get-all-refs method to show that by default, simple data is
;; recursed. The sublists in the second list in this example are processed as
;; nested lists
(let ((ral (make-ral 'ral-test
		     '((1 one)
		       (2 ((3 4) (5 6)))
		       (3 three)))))
  (get-all-refs ral))

=> ((1) (2 3) (2 5) (3))

;; Using the same data, but setting the :recurse-simple-data argument to NIL
;; will cause the method to process simple data as a unit rather than nested
;; lists 
(let ((ral (make-ral 'ral-test
		     '((1 one)
		       (2 ((3 4) (5 6)))
		       (3 three))
		     :recurse-simple-data nil)))
  (get-all-refs ral))

=> ((1) (2) (3))

|#
;;; SYNOPSIS
(defun make-ral (id ral &key (recurse-simple-data t) (warn-not-found t)
                 (tag nil) (full-ref nil))
;;; ****
  (make-instance 'recursive-assoc-list :data ral :id id 
                 :recurse-simple-data recurse-simple-data
                 :warn-not-found warn-not-found
                 :tag tag :full-ref full-ref))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****f* recursive-assoc-list/lisp-assoc-listp
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
(defun lisp-assoc-listp (candidate &optional (recurse-simple-data t))
;;; ****
  (when (and candidate (listp candidate))
    (dolist (x candidate t)
            ;; (print x)
            (unless (and (listp x)
                         (= 2 (length x))
                         ;; 23/3/04 added this clause because the first element
                         ;; of the list must be   
                         (assoc-list-id-p (first x))
                         ;; 31/12/10: why these tests? this leads to bugs with
                         ;; 2-element lists being parsed as recursive when
                         ;; second element is a string as opposed to a symbol 
                         (or recurse-simple-data
                         
                             (and (not (symbolp (second x)))
                                  (not (numberp (second x)))))
                         )
              (return nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Check whether a list contains only atoms which could be used as assoc-list
;;; ids. 

;;; ****f* recursive-assoc-list/assoc-list-id-list
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
(defun assoc-list-id-list (id-list)
;;; ****
  (when (listp id-list)
    (loop for i in id-list unless (assoc-list-id-p i) do (return nil)
        finally (return t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun is-ral (candidate)
  (typep candidate 'recursive-assoc-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; EOF recursive-assoc-list.lsp
