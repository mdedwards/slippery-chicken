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
;;; Version:          1.0.0-beta1
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
;;; $$ Last modified: 10:27:35 Thu May 17 2012 BST
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
;;; The optional <previous> and <higher-next> arguments are only for internal
;;; recursive calls and so shouldn't be given by the user.
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
  ;; (print previous)
  ;; (print higher-next)
  (unless (linked ral)
    (let ((next nil)
          (next-in-list nil)
          (next-in-list-data nil)
          (data (data ral))
          (this-data nil)
          (this-ref nil)
          (ral-full-ref (full-ref ral)))
      (loop for this in data and i from 0 do
           ;; (print ral-full-ref)
           (setf this-ref (econs ral-full-ref (id this))
                 next-in-list (nth (1+ i) data)
                 next-in-list-data (when next-in-list (data next-in-list))
                 next (cond ((and (is-ral next-in-list-data)
                                  ;; MDE Sat Jan 28 18:20:14 2012 -- only if
                                  ;; there's data in there!
                                  (data next-in-list-data))
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
         ;; MDE Sat Jan 28 18:20:29 2012 -- only if the ral has data
           (if (and (is-ral this-data) (data this-data))
               (progn 
                 ;; in case we're relinking
                 (setf (linked this-data) nil)
                 ;; MDE Wed May 16 20:21:11 2012 -- if we don't do this, we
                 ;; lose references. Problem shows up e.g. with ral-econs
                 (setf (full-ref this-data) this-ref)
                 ;; (print 'recurse)
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
;;; you reference a key that exists, then its data is a list that data will be
;;; added to the end of (an error will be signalled if this is not the case).

;;; ****m* recursive-assoc-list/ral-econs
;;; FUNCTION
;;; 
;;; 
;;; ARGUMENTS
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
  ;; MDE Thu May 17 10:20:51 2012 -- 
  (if (and key (atom key))
      (if (get-data key ral nil)
          (add-to-list-data data key ral)
          (add (make-named-object key (if (listp data) data (list data))) ral))
      (progn
        (unless (listp key)
          (setf key (list key)))
        (let ((butlast (butlast key))
              (last (first (last key)))
              data-data)
          ;; so we can't just add to the list rather we have to create the ral
          ;; leaves  
          (unless (get-data key ral nil) 
            (loop 
               for k in butlast
               with keys = '()
               do
               ;; if we don't have data at this key ...
               (unless (get-data (reverse (cons k keys)) ral nil)
                 ;; ... add it but keep empty for now ...
                 (add (make-named-object k (make-ral nil nil)) 
                      (if keys 
                          ;; ... to the sub-ral if we have keys ...
                          (get-data-data (reverse keys) ral)
                          ;; ... or to the top-level ral if not ...
                          ral)))
               (push k keys))
            ;; MDE Thu May 17 10:02:08 2012 
            (setf data-data (get-data-data butlast ral))
            (when data-data
              (add (make-named-object last nil) data-data)))
          ;; now we put the data in there
          (set-data key (list last (econs (get-data-data key ral) data))
                    ral)
          ;; MDE Wed May 16 21:52:03 2012 -- 
          (relink-named-objects ral)
          data))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SAR Fri Jan 27 16:41:42 GMT 2012: Added robodoc info

;;; ****m* recursive-assoc-list/parcel-data
;;; DATE
;;; 10 Apr 2010
;;;
;;; FUNCTION
;;; Put all the data of a given recursive-assoc-list object into a new
;;; named-object at the top level of that recursive-assoc-list object; i.e. add
;;; a level of recursion.  This is a means of making a collection of data
;;; before perhaps adding more with potentially conflicting ids. 
;;; 
;;; ARGUMENTS 
;;; - A recursive-assoc-list object.
;;; - A symbol that is new the top-level id for the current data
;;; 
;;; RETURN VALUE  
;;; The new recursive-assoc-list object.
;;; 
;;; EXAMPLE
#|
;; Collect all the data contained within the object 'mixed-bag and store it at
;; the top-level of 'mixed-bag within a new named-object with the id 'potpourri 
(let ((ral (make-ral 'mixed-bag 
                     '((jim beam)
                       (wild turkey)
                       (four ((roses red)
                              (violets ((blue velvet)
                                        (red ((dragon den)
                                              (viper nest)
                                              (fox hole)))
                                        (white ribbon)))))))))
  (parcel-data ral 'potpourri))

=>
RECURSIVE-ASSOC-LIST: recurse-simple-data: T
                      num-data: 8
                      linked: NIL
                      full-ref: NIL
ASSOC-LIST: warn-not-found T
CIRCULAR-SCLIST: current 0
SCLIST: sclist-length: 1, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: MIXED-BAG, tag: FROM-PARCEL-DATA, 
data: (
NAMED-OBJECT: id: POTPOURRI, tag: NIL, 
data: 
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
[...]

|#
;;; SYNOPSIS
(defmethod parcel-data ((ral recursive-assoc-list) new-id)
;;; ****
  (make-ral (id ral) (list (make-named-object new-id ral))
            :tag (if (tag ral) (tag ral) 'from-parcel-data)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Fri Jan 27 17:07:27 GMT 2012: Add robodoc info

;;; MDE comment:
;;; add an empty level of recursion

;;; ****m* recursive-assoc-list/add-empty-parcel
;;; FUNCTION
;;; Add an recursive-assoc-list object with NIL data (an empty level of
;;; recursion) to the end of the top-level of a given recursive-assoc-list
;;; object. 
;;;
;;; NB: Adding an empty parcel to a given recursive-assoc-list object will
;;;     cause the method get-all-refs to fail on that recursive-assoc-list
;;;     object. 
;;; 
;;; ARGUMENTS
;;; - A recursive-assoc-list object.
;;; - A symbol that will be the ID of the new, empty recursive-assoc-list
;;;   object that is to be added.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - <new-class> The name of an existing subclass of recursive-assoc-list that
;;; the parcel should be promoted to.
;;; 
;;; RETURN VALUE
;;; A recursive-assoc-list object with DATA of NIL (the "empty parcel")
;;; 
;;; EXAMPLE
#|
;; Add two new empty parcels (the first a recursive-assoc-list, by default, the
;; second a rthm-seq-palette) and return the new list of REFS:
(let ((ral (make-ral 'mixed-bag 
                     '((jim beam)
                       (wild turkey)
                       (four ((roses red)
                              (violets ((blue velvet)
                                        (red ((dragon den)
                                              (viper nest)
                                              (fox hole)))
                                        (white ribbon)))))))))
  (add-empty-parcel ral 'bricolage)
  (add-empty-parcel ral 'rsp 'rthm-seq-palette)
  (get-all-refs ral))

Mark set
=> 

((JIM) (WILD) (FOUR ROSES) (FOUR VIOLETS BLUE) (FOUR VIOLETS RED DRAGON)
 (FOUR VIOLETS RED VIPER) (FOUR VIOLETS RED FOX) (FOUR VIOLETS WHITE)
 (BRICOLAGE) (RSP))


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
    ;; MDE Sat Jan 28 17:05:03 2012 
    (relink-named-objects ral)
    sub-ral))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Fri Jan 27 15:33:01 GMT 2012: Added robodoc info

;;; ****m* recursive-assoc-list/set-data
;;; FUNCTION
;;; Replace the named-object associated with a specified key within a given
;;; recursive-assoc-list object. This method replaces the whole named-object,
;;; not just the data of that object. 
;;; 
;;; ARGUMENTS
;;; - A key present within the given recursive-assoc-list object. This must be
;;;   a list that is the FULL-REF (path of keys) if replacing a nested
;;;   named-object. If replacing a named-object at the top level, the
;;;   key can be given either as a single-item list or an individual symbol. 
;;; - A key/data pair as a quoted list.
;;; - The recursive-assoc-list object in which to find and replace the
;;;   named-object associated with the specified key.
;;; 
;;; RETURN VALUE
;;; Returns the new named-object.
;;; 
;;; Returns NIL when the specified key is not found within the given
;;; recursive-assoc-list object. 
;;; 
;;; EXAMPLE
#|

;;; Replace a named-object at the top level using a single symbol
(let ((ral (make-ral 'mixed-bag 
                     '((jim beam)
                       (wild turkey)
                       (four ((roses red)
                              (violets ((blue velvet)
                                        (red ((dragon den)
                                              (viper nest)
                                              (fox hole)))
                                        (white ribbon)))))))))
  (set-data 'wild '(makers mark) ral))

=> 
NAMED-OBJECT: id: MAKERS, tag: NIL, 
data: MARK

;; The same can be done stating the top-level key as a single-item list. Apply
;; the get-all-refs method in this example to see the change
(let ((ral (make-ral 'mixed-bag 
                     '((jim beam)
                       (wild turkey)
                       (four ((roses red)
                              (violets ((blue velvet)
                                        (red ((dragon den)
                                              (viper nest)
                                              (fox hole)))
                                        (white ribbon)))))))))
  (set-data '(wild) '(makers mark) ral)
  (get-all-refs ral))

=> ((JIM) (MAKERS) (FOUR ROSES) (FOUR VIOLETS BLUE) (FOUR VIOLETS RED DRAGON)
    (FOUR VIOLETS RED VIPER) (FOUR VIOLETS RED FOX) (FOUR VIOLETS WHITE))

;; Replace a nested named-object using a list that is the FULL-REF to that
;; object. Print the application of the method as well as the results from
;; applying the get-all-refs method in this example to see the effects
(let ((ral (make-ral 'mixed-bag 
                     '((jim beam)
                       (wild turkey)
                       (four ((roses red)
                              (violets ((blue velvet)
                                        (red ((dragon den)
                                              (viper nest)
                                              (fox hole)))
                                        (white ribbon)))))))))
  (print (set-data '(four violets red fox) '(bee hive) ral))
  (print (get-all-refs ral)))

=>
NAMED-OBJECT: id: BEE, tag: NIL, 
data: HIVE
**************
 
((JIM) (WILD) (FOUR ROSES) (FOUR VIOLETS BLUE) (FOUR VIOLETS RED DRAGON)
 (FOUR VIOLETS RED VIPER) (FOUR VIOLETS RED BEE) (FOUR VIOLETS WHITE))


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

;; MDE Tue Jan 31 11:22:36 2012 
(defmethod set-data :after (key new-value (ral recursive-assoc-list))
  (relink-named-objects ral))

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

;;; SAR Fri Jan 27 14:42:30 GMT 2012: Added robodoc info

;;; ****m* recursive-assoc-list/get-first
;;; FUNCTION
;;; Returns the first named-object in the DATA slot of the given
;;; recursive-assoc-list object. 
;;; 
;;; ARGUMENTS
;;; - A recursive-assoc-list object.
;;; 
;;; RETURN VALUE
;;; A named-object that is the first object in the DATA slot of the given
;;; recursive-assoc-list object.
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
  (get-first ral))

=> 
NAMED-OBJECT: id: JIM, tag: NIL, 
data: BEAM

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

;;; SAR Fri Jan 27 14:53:30 GMT 2012: Deleted MDE's comment here, as it has
;;; been placed nearly verbatim into the robodoc info below.

;;; SAR Fri Jan 27 14:53:18 GMT 2012: Added robodoc info

;;; ****m* recursive-assoc-list/get-first-ref
;;; FUNCTION
;;; Get the full reference into the given recursive-assoc-list object of the
;;; first named-object in the given recursive-assoc-list object. 
;;;
;;; NB: If the <ral> argument happens to be a recursive-assoc-list object that
;;;     is contained within another recursive-assoc-list object (i.e. is a
;;;     nested recursive-assoc-list object), then the result is the reference
;;;     into the top-level recursive-assoc-list object, not the argument.
;;; 
;;; ARGUMENTS
;;; - A recursive-assoc-list object.
;;; 
;;; RETURN VALUE
;;; 
;;; 
;;; EXAMPLE
#|
;; A simple call returns the first top-level named-object
(let ((ral (make-ral 'mixed-bag 
                     '((jim beam)
                       (wild turkey)
                       (four ((roses red)
                              (violets ((blue velvet)
                                        (red ((dragon den)
                                              (viper nest)
                                              (fox hole)))
                                        (white ribbon)))))))))
  (get-first-ref ral))

=> (JIM)

;; Return the first ref of a nested recursive-assoc-list object
(let ((ral (make-ral 'mixed-bag 
                     '((jim beam)
                       (wild turkey)
                       (four ((roses red)
                              (violets ((blue velvet)
                                        (red ((dragon den)
                                              (viper nest)
                                              (fox hole)))
                                        (white ribbon)))))))))
  (get-first-ref (get-data-data '(four violets) ral)))

=> (FOUR VIOLETS BLUE)

|#
;;; SYNOPSIS
(defmethod get-first-ref ((ral recursive-assoc-list))
;;; ****
  (when (data ral) ; MDE Sat Jan 28 16:50:54 2012 -- only when there's data
    (let* ((first (first (data ral)))
           (first-data (data first)))
      (if (is-ral first-data)
          (get-first-ref first-data)
          (econs (full-ref ral) (id first))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Fri Jan 27 14:59:57 GMT 2012: Added robodoc info

;;; ****m* recursive-assoc-list/get-last
;;; FUNCTION
;;; Get the last named-object in a given recursive-assoc-list object. 
;;;
;;; NB: This method functions linearly, not hierarchically. The last named
;;;     object is therefore not necessarily the deepest of a nest, but the last
;;;     listed.  
;;; 
;;; ARGUMENTS
;;; - A recursive-assoc-list object.
;;; 
;;; RETURN VALUE
;;; A named-object (or linked-named-object).
;;; 
;;; EXAMPLE
#|

;; This returns '(white ribbon), not '(fox hole)
(let ((ral (make-ral 'mixed-bag 
                     '((jim beam)
                       (wild turkey)
                       (four ((roses red)
                              (violets ((blue velvet)
                                        (red ((dragon den)
                                              (viper nest)
                                              (fox hole)))
                                        (white ribbon)))))))))
  (get-last ral))

=> 
NAMED-OBJECT: id: WHITE, tag: NIL, 
data: RIBBON

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

;;; SAR Fri Jan 27 15:05:18 GMT 2012: Added robodoc info

;;; ****m* recursive-assoc-list/get-all-refs
;;; FUNCTION
;;; Return a list of all the keys (REFS) in a given recursive-assoc-list
;;; object. Nested keys are given in FULL-REF form, i.e. a list that is the
;;; path of keys to the specific key.
;;;
;;; Keys that are not part of nesting-path are also returned as lists
;;; (single-item lists) by default. An optional argument allows these to be
;;; returned as individual symbols rather than lists.
;;;
;;; NB This will only work on the top-level object due to the creation of
;;; references when linking.  
;;; 
;;; ARGUMENTS
;;; - A recursive-assoc-list object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to indicate whether to return single REFS (non-nested keys) as
;;;   lists or as individual symbols. T = as list. Default = T.
;;; 
;;; RETURN VALUE
;;; A list.
;;; 
;;; EXAMPLE
#|

;; By default all keys are returned as lists, even single (non-nested) keys
(let ((ral (make-ral 'mixed-bag 
                     '((jim beam)
                       (wild turkey)
                       (four ((roses red)
                              (violets ((blue velvet)
                                        (red ((dragon den)
                                              (viper nest)
                                              (fox hole)))
                                        (white ribbon)))))))))
  (get-all-refs ral))

=> ((JIM) (WILD) (FOUR ROSES) (FOUR VIOLETS BLUE) (FOUR VIOLETS RED DRAGON)
    (FOUR VIOLETS RED VIPER) (FOUR VIOLETS RED FOX) (FOUR VIOLETS WHITE))

;; Setting the optional argument to NIL returns non-nested keys as symbols
;; rather than lists  
(let ((ral (make-ral 'mixed-bag 
                     '((jim beam)
                       (wild turkey)
                       (four ((roses red)
                              (violets ((blue velvet)
                                        (red ((dragon den)
                                              (viper nest)
                                              (fox hole)))
                                        (white ribbon)))))))))
  (get-all-refs ral nil))

=> (JIM WILD (FOUR ROSES) (FOUR VIOLETS BLUE) (FOUR VIOLETS RED DRAGON)
    (FOUR VIOLETS RED VIPER) (FOUR VIOLETS RED FOX) (FOUR VIOLETS WHITE))

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
        do 
         ;; (print ref)
         (when (and (not single-ref-as-list)
                    (= 1 (length this)))
           (setf this (first this)))
        collect this
        do (setf ref (when current (next current))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Fri Jan 27 15:16:47 GMT 2012: Added robodoc info

;;; ****m* recursive-assoc-list/get-last-ref
;;; FUNCTION
;;; Get the last REF (path of nested keys) of the given recursive-assoc-list
;;; object. 
;;;
;;; NB: This method functions linearly, not hierarchically. The last-ref may
;;;     not be the deepest nested.
;;; 
;;; ARGUMENTS
;;; - A recursive-assoc-list object.
;;; 
;;; RETURN VALUE
;;; Returns a list that is the last REF of the given recursive-assoc-list object.
;;; 
;;; EXAMPLE
#|

;; Typical usage with nesting
(let ((ral (make-ral 'mixed-bag 
                     '((jim beam)
                       (wild turkey)
                       (four ((roses red)
                              (violets ((blue velvet)
                                        (red ((dragon den)
                                              (viper nest)
                                              (fox hole)))
                                        (white ribbon)))))))))
  (get-last-ref ral))

=> (FOUR VIOLETS WHITE)

;; Returns the last-ref as a list even if the given recursive-assoc-list object
;; contains no nesting
(let ((ral (make-ral 'mixed-bag 
                     '((jim beam)
                       (wild turkey)
                       (four roses)))))
  (get-last-ref ral))

=> (FOUR)

|#
;;; SYNOPSIS
(defmethod get-last-ref ((ral recursive-assoc-list))
;;; ****
  (when (data ral) ; MDE Sat Jan 28 16:51:33 2012 -- only when there's data
    (let* ((last (first (last (data ral))))
           (last-data (data last)))
      ;; MDE Sat Jan 28 17:10:25 2012 -- only recurse if a sub-ral has data
      (if (and (is-ral last-data) (data last-data))
          (get-last-ref last-data)
          (econs (full-ref ral) (id last))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Fri Jan 27 17:46:10 GMT 2012: Delete MDE's comment here as it is taken
;;; nearly verbatim into the robodoc info below
;;; SAR Fri Jan 27 17:45:55 GMT 2012: Added robodoc info

;;; ****m* recursive-assoc-list/recursivep
;;; FUNCTION
;;; Check whether the data in a recursive-assoc-list object is really
;;; recursive. 
;;; 
;;; ARGUMENTS
;;; - A recursive-assoc-list object.
;;; 
;;; RETURN VALUE
;;; T or NIL to indicate whether or not the tested data is recursive. 
;;; T = recursive. 
;;; 
;;; EXAMPLE
#|
;; The data in this recursive-assoc-list object is really recursive, and
;; the method therefore returns T
(let ((ral (make-ral 'mixed-bag 
                     '((jim beam)
                       (wild turkey)
                       (four ((roses red)
                              (violets ((blue velvet)
                                        (red ((dragon den)
                                              (viper nest)
                                              (fox hole)))
                                        (white ribbon)))))))))
  (recursivep ral))

=> T

;; The data in this recursive-assoc-list object is not actually recursive, and
;; the method therefore returns NIL
(let ((ral (make-ral 'mixed-bag 
                     '((jim beam)
                       (wild turkey)
                       (four roses)))))
  (recursivep ral))

=> NIL


|#
;;; SYNOPSIS
(defmethod recursivep ((ral recursive-assoc-list))
;;; ****
  (loop for i in (data ral)
      if (is-ral (data i)) do (return t)
      finally (return nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Tue Feb 28 18:03:28 2012 -- get the object for <id> and set its <slot>
;;; to <value> NB we setf slot-value here rather than calling the setf method
;;; so if a setf method has been explicitly defined it won't be called.
;;; ****m* rthm-seq-palette/set-slot

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
(largest-fast-leap
 (get-data 'oboe +slippery-chicken-standard-instrument-palette+))
==> 999

(set-slot 'largest-fast-leap 10 'oboe
          +slippery-chicken-standard-instrument-palette+)
                                       
|#
;;; SYNOPSIS
(defmethod set-slot (slot value id (ral recursive-assoc-list))
;;; ****
  (setf (slot-value (get-data id ral) slot) value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;; keyword arguments:
;;; - :recurse-simple-data. T or NIL to indicate whether to recursively
;;;   instantiate a recursive-assoc-list in place of data that appears to be a
;;;   simple assoc-list (i.e. a 2-element list). If NIL, the data of 2-element
;;;   lists whose second element is a number or a symbol will be ignored,
;;;   therefore remaining as a list. For example, this data would normally
;;;   result in a recursive call: (y ((2 23) (7 28) (18 2))).  T = replace
;;;   assoc-list data with recursive-assoc-lists. Default = T.
;;; - :full-ref. Nil or a list representing the path to a nested
;;;   recursive-assoc-list object within the given recursive-assoc-list object,
;;;   starting from the top level of the given object. When NIL, the given
;;;   recursive-assoc-list object itself is the top level.  Default = NIL.
;;; - :tag. A symbol that is another name, description etc. for the given
;;;   recursive-assoc-list object. The tag may be used for identification but
;;;   not for searching purposes. Default = NIL.
;;; - :warn-not-found. T or NIL to indicate whether a warning is printed when
;;;   an index which doesn't exist is used for lookup.  Default = T.
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

;;; SAR Fri Jan 27 17:46:50 GMT 2012: Add robodoc info

;;; ****f* recursive-assoc-list/lisp-assoc-listp
;;; FUNCTION
;;; Determine whether a given list can has the structure of a lisp
;;; assoc-list. This is assed based on each of the elements being a 2-item
;;; list, of which the first is a symbol, number or string (qualifies as a
;;; key). 
;;; 
;;; The optional argument <recurse-simple-data> allows the data portion of
;;; key/data pairs to be viewed as flat lists rather than as recursive lists. 
;;; 
;;; ARGUMENTS
;;; - A list.
;;; 
;;; OPTIONAL ARGUMENTS
;;; T or NIL to indicate whether to consider lists of 2-item lists in the data
;;; position of a given key/data pair to be a list or a recursive list.
;;; T = list. Default = T.
;;; 
;;; RETURN VALUE
;;; T or NIL. T = the tested list can be considered a Lisp assoc-list.
;;; 
;;; EXAMPLE
#|
;; A list of 2-item lists, each of whose item are all either a symbol, number, 
;; or string, can be considered a Lisp assoc-list.
(let ((lal '((roses red) (3 "allegro") (5 flute))))
  (lisp-assoc-listp lal))

=> T

;; By default, lists of 2-item lists in the DATA portion of a key/data pair
;; will be considered as a simple list, rather than a recursive list, resulting
;; in the tested list passing as T.
(let ((lal '((1 2) (3 ((4 5) (6 7))) (8 9))))
  (lisp-assoc-listp lal))

=> T

;; Setting the optional argument to NIL will cause the same list to fail with
(let ((lal '((1 2) (3 ((4 5) (6 7))) (8 9))))
  (lisp-assoc-listp lal nil))

=> NIL

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

;;; SAR Fri Jan 27 18:15:27 GMT 2012: Deleted MDE's comment here as it is taken
;;; nearly verbatim into the robodoc info
;;; SAR Fri Jan 27 18:15:04 GMT 2012: Added robodoc info

;;; ****f* recursive-assoc-list/assoc-list-id-list
;;; FUNCTION
;;; Determine whether a given list contains only atoms which could be used as
;;; assoc-list IDs. To pass the test, a given atom must be either a symbol, a
;;; number or a string.
;;; 
;;; ARGUMENTS
;;; A list.
;;; 
;;; RETURN VALUE
;;; T or NIL indicating whether the atoms of the given list are all capable of
;;; being used as assoc-list IDs. T = all can be used as assoc-list IDs.
;;; 
;;; EXAMPLE
#|
;; All of the elements in this list are either a symbol, a number or a
;; string. The list therefore returns a T when tested.
(let ((alil '(jim beam 3 "Allegro" 5 flute)))
  (assoc-list-id-list alil))

=> T

;; This list fails, as the last element is a list (and therefore not of type
;; string, number or symbol)
(let ((alil '(jim beam 3 "Allegro" 5 (flute))))
  (assoc-list-id-list alil))

=> NIL

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
