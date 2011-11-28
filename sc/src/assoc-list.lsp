;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* _sc/assoc-list
;;; NAME 
;;; assoc-list
;;;
;;; File:             assoc-list.lsp
;;;
;;; Class Hierarchy:  named-object -> linked-named-object -> sclist ->
;;;                   circular-sclist -> assoc-list 
;;;
;;; Version:          1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Implementation of the assoc-list class that is somewhat
;;;                   like the lisp association list but with more
;;;                   error-checking.   
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    February 18th 2001
;;;
;;; $$ Last modified: 14:24:29 Tue Nov  8 2011 GMT
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

(defclass assoc-list (circular-sclist)
  ;;****S* assoc-list/warn-not-found
  ;; NAME
  ;; warn-not-found
  ;; whether to issue a warning when an index which doesn't exist is used for
  ;; lookup.  Set to nil if you want to check for data without getting warnings.
  ;; ****
  ((warn-not-found :accessor warn-not-found :type boolean :initarg
                   :warn-not-found :initform t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Nothing to be done here as it is taken care of in the sclist class,
;;; including the call of verify-and-store which we have overridden below.  

;;; (defmethod initialize-instance :after ((al assoc-list) &rest initargs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#-(and clisp win32)
(defmethod print-object :before ((al assoc-list) stream)
  (format stream "~%ASSOC-LIST: warn-not-found ~a" (warn-not-found al)))

;;; For some reason, when this is a :before method, clisp in windows crashes
;;; with a SIGSEV fault (it's no problem with clisp under linux)!  Can't find
;;; the reason so don't descend into any further methods.  todo: fix this!
#+(and clisp win32)
(defmethod print-object ((al assoc-list) stream)
  (format stream "~%ASSOC-LIST: warn-not-found ~a, ~
                  data: Not displayed due to clisp bug."
          (warn-not-found al)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod verify-and-store :after ((al assoc-list))
  (let ((data (data al)))
    (when data
      (loop for pair in data and i from 0 do
            (unless (or (typep pair 'named-object)
                        ;; pitch-seq-palettes don't have to give a key/id as
                        ;; that can be provided automatically in
                        ;; make-pitch-seq, so handle them in that class itself
                        (typep al 'pitch-seq-palette))
              (unless (and (listp pair) (= 2 (length pair)))
                (error "~a~%assoc-list::verify-and-store: ~
                        assoc list with id ~a: ~%~
                        The data slot of assoc-list must be a list containing ~
                        2-element sublists." pair (id al)))
              (unless (assoc-list-id-p (first pair))
                (error "assoc-list::verify-and-store: ~
                        assoc-list ids may only be symbols, strings or ~
                        numbers: ~a"
                       (first pair)))
              (setf (nth i data) (make-instance 'named-object 
                                   :id (first pair)
                                   :data (second pair)))))))
  (unless 
      (typep al 'pitch-seq-palette)
    (all-ids-unique al)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone ((al assoc-list))
  (clone-with-new-class al 'assoc-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone-with-new-class :around ((al assoc-list) new-class)
  (declare (ignore new-class))
  (let ((cscl (call-next-method)))
    (setf (slot-value cscl 'warn-not-found) (warn-not-found al))
    cscl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 28.11.11 SEAN: Added info for robodoc
;;; ****m* assoc-list/get-keys
;;; FUNCTION
;;; get-keys:
;;; Get a simple list of the keys in a given association list.
;;; 
;;; ARGUMENTS:
;;; An assoc-list.
;;; 
;;; RETURN VALUE: 
;;; The keys only of all top-level association list pairs in the given
;;; assoc-list. 
;;;
;;; get-keys is a method of the assoc-list class and therefore returns only
;;; top-level keys if accessing a recursive assoc-list.
;;; 
;;; EXAMPLE
;;; (setf x (make-instance 'assoc-list :data '((cat felix) 
;;; 					    (dog fido) 
;;; 					    (cow bessie))))
;;; (get-keys x) ; => (CAT DOG COW)
;;; 
;;; (setf y (make-instance 'assoc-list 
;;; 		       :data '((cat felix) 
;;; 			       (dog ((scottish terrier)
;;; 				     (german shepherd)
;;; 				     (irish wolfhound))) 
;;; 			       (cow bessie))))
;;; (get-keys y) 
;;; ; => (CAT DOG COW)
;;; 
;;; SYNOPSIS
(defmethod get-keys ((al assoc-list))
;;; ****
  (when (is-ral al)
    (warn "assoc-list::get-keys: ~
           The get-keys method comes from the assoc-list class and ~
           therefore ~%only the top-level keys are returned!"))
  (loop for obj in (data al) collect (id obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod all-ids-unique ((al assoc-list))
  (loop for obj in (data al) and i from 0 
     when (get-position (id obj) al (1+ i)) do
     (error "assoc-list::all-ids-unique: ~
                All ids in an assoc-list must be unique.  Found duplicate: ~
                ~a in ~%~a" (id obj) al)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 28.11.11 SEAN: Added info for ROBODoc
;;; ****m* assoc-list/get-first
;;; FUNCTION
;;; get-first:
;;; Returns the first named-object in the data list of the given assoc-list. 
;;; 
;;; ARGUMENTS:
;;; An assoc-list.
;;; 
;;; RETURN VALUE: 
;;; The first object in the data list of a given assoc-list.
;;; 
;;; EXAMPLE
;;; (setf x (make-instance 'assoc-list :id 'kentucky :tag 'bourbon
;;;  		       :data '((jim beam)
;;;  			       (four roses)
;;; 			       (wild turkey))))
;;; (get-first x)
;;; ; => 
;;; ; NAMED-OBJECT: id: JIM, tag: NIL,
;;; ; data BEAM
;;; ; ***************************************************************************
;;; 
;;; SYNOPSIS
(defmethod get-first ((al assoc-list))
;;; ****
  (first (data al)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 28.11.11 SEAN: Added info for ROBODoc
;;; ****m* assoc-list/get-last
;;; FUNCTION
;;; get-last:
;;; Returns the last named-object in the data list of a given assoc-list.
;;; 
;;; ARGUMENTS:
;;; An assoc-list.
;;; 
;;; RETURN VALUE: 
;;; The last object in the data list of a given assoc-list.
;;; 
;;; EXAMPLE
;;; (setf x (make-instance 'assoc-list :id 'kentucky :tag 'bourbon
;;;  		       :data '((jim beam)
;;;  			       (four roses)
;;; 			       (wild turkey))))
;;; (get-last x)
;;; ; => 
;;; ; NAMED-OBJECT: id: WILD, tag: NIL,
;;; ; data TURKEY
;;; ; ***************************************************************************
;;; 
;;; SYNOPSIS
(defmethod get-last ((al assoc-list))
;;; ****
  (first (last (data al))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Given the key into the al, return the object before this one.  If key is
;;; nil, the last in the al will be returned.  If how-many would mean going
;;; back to a negative index, return how many we've gone too far (negative
;;; number).  

(defmethod get-previous ((al assoc-list) key &optional (how-many 1))
  (let* ((start (get-position key al)) ;;; returns nil if key is nil
         (index -1))
    (if (null key)
        (get-last al)
      (when start
        (setf index (- start how-many))
        (if (>= index 0)
          (nth index (data al))
          ;; this would on success always return a named-object so for failure
          ;; return the number by which we tried to go too far backwards.
          index)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 28.11.11 SEAN: Added info for ROBODoc
;;; ****m* assoc-list/get-position
;;; FUNCTION
;;; get-position:
;;; Returns the index position (zero-based) of a named-object within a given 
;;; assoc-list. 
;;; 
;;; ARGUMENTS:
;;; The assoc-list key symbol (named-object id) of the object for which the
;;; position is sought, and the assoc-list in which it is to be sought.
;;;
;;; Optional argument: An indexing integer. In this case, get-position will
;;; search for the given object starting part-way into the list, skipping all
;;; objects located at indices lower than the given integer (default = 0).
;;; 
;;; RETURN VALUE: 
;;; The integer index of the named-object within the given assoc-list.
;;;
;;; NIL is returned if the object is not present in the assoc-list starting
;;; with the index number given as the start argument (i.e., in the entire list
;;; if the optional start argument is omitted).  
;;; 
;;; EXAMPLE
;;; (setf x (make-instance 'assoc-list :id 'kentucky :tag 'bourbon
;;;  		       :data '((jim beam)
;;;  			       (four roses)
;;; 			       (wild turkey))))
;;; (get-position 'four x)
;;; ; => 1 (1 bit, #x1, #o1, #b1)
;;;
;;; (get-position 'jack x)
;;; ; => NIL
;;;
;;; (get-position 'jim x 1)
;;; ; => NIL
;;; 
;;; SYNOPSIS
(defmethod get-position (key (al assoc-list) &optional (start 0))
;;; ****
  (loop for i in (nthcdr start (data al)) and j from start
    if (id-eq key i) do (return j)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* assoc-list/get-data-data
;;; FUNCTION
;;; get-data-data:
;;; Just a short-cut for (data (get-data ...))
;;; 
;;; 
;;; ARGUMENTS:
;;; 
;;; 
;;; RETURN VALUE: 
;;; 
;;; 
;;; EXAMPLE
;;; 
;;; 
;;; DATE
;;; 
;;; 
;;; SYNOPSIS
(defmethod get-data-data (key (al assoc-list) &optional (warn t))
;;; ****
  (data (get-data key al warn)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Mostly we define whether we want to warn in the instance itself but
;;; sometimes it would be good to warn or not on a call basis, hence the
;;; optional argument.

;;; NB This method actually returns the named object, not just the data
;;; associated with the key (use get-data-data for that)

;;; ****m* assoc-list/get-data
;;; FUNCTION
;;; get-data:
;;;
;;; 
;;; 
;;; ARGUMENTS:
;;; 
;;; 
;;; RETURN VALUE: 
;;; 
;;; 
;;; EXAMPLE
;;; 
;;; 
;;; DATE
;;; 
;;; 
;;; SYNOPSIS
(defmethod get-data (key (al assoc-list) &optional (warn t))
;;; ****
  (let ((pos (get-position key al)))
    (if pos
        (get-nth pos al)
      (when (or warn 
                (and warn (warn-not-found al)))
        (warn "assoc-list::get-data: ~
               Could not find data with key ~a in assoc-list with id ~a"
              key (id al))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; todo: adding a two-bar rthm-seq to a rthm-seq-palette fails, possibly
;; because the two bars thing gets recursively make into something else???

;;; ****m* assoc-list/add
;;; FUNCTION
;;; add:
;;; Add a new element to the assoc-list.
;;; 
;;; 
;;; ARGUMENTS:
;;; 
;;; 
;;; RETURN VALUE: 
;;; 
;;; 
;;; EXAMPLE
;;; 
;;; 
;;; DATE
;;; 
;;; 
;;; SYNOPSIS
(defmethod add (named-object (al assoc-list) &optional ignore)
;;; ****
  (declare (ignore ignore))  
  (unless named-object
    (error "assoc-list::add: named-object is NIL!"))
  (setf named-object (check-or-force-named-object named-object))
  (when (get-position (id named-object) al)
    (error "assoc-list::add: Can't add ~a to assoc-list with id ~a ~
            because key already exists!"
           (id named-object) (id al)))
  (setf (slot-value al 'data) (econs (data al) (clone named-object)))
  (incf (sclist-length al))
  ;; (push (clone named-object) (data al))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Not to be used for adding data, only replacing!
;;; N.B. This replaces the whole named-object in the data list, not just the
;;; data of that object.  

;;; ****m* assoc-list/set-data
;;; FUNCTION
;;; set-data:
;;;
;;; 
;;; 
;;; ARGUMENTS:
;;; 
;;; 
;;; RETURN VALUE: 
;;; 
;;; 
;;; EXAMPLE
;;; 
;;; 
;;; DATE
;;; 
;;; 
;;; SYNOPSIS
(defmethod set-data (key new-value (al assoc-list))
;;; ****
  (setf new-value (check-or-force-named-object new-value))
  (let ((pos (get-position key al)))
    (when (and (not pos) (warn-not-found al))
      (warn "assoc-list::set-data: ~
             Could not find data with key ~a in assoc-list with id ~a"
            key (id al)))
    (when pos
      (setf (nth pos (data al)) new-value))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Add an element (could be any type) to the end of the list which is
;;; the data for <key>  

;;; ****m* assoc-list/add-to-list-data
;;; FUNCTION
;;; add-to-list-data:
;;;
;;; 
;;; 
;;; ARGUMENTS:
;;; 
;;; 
;;; RETURN VALUE: 
;;; 
;;; 
;;; EXAMPLE
;;; 
;;; 
;;; DATE
;;; 
;;; 
;;; SYNOPSIS
(defmethod add-to-list-data (new-element key (al assoc-list))
;;; ****
  (let ((data (get-data-data key al)))
    (unless (listp data)
      (error "assoc-list::add-to-list-data: existing data must be a list!: ~
              ~%key: ~a, new-element: ~a, assoc-list id: ~a" 
             key new-element (id al)))
    ;; (print data)
    (set-data key (list key (econs data new-element)) al)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 26.1.11: similar to add-to-list-data but if the key doesn't already exist,
;;; add it first, then the new element as a 1-element list
;;; ****m* assoc-list/add-to-list-data-force
;;; FUNCTION
;;; add-to-list-data-force:
;;;
;;; 
;;; 
;;; ARGUMENTS:
;;; 
;;; 
;;; RETURN VALUE: 
;;; 
;;; 
;;; EXAMPLE
;;; 
;;; 
;;; DATE
;;; 
;;; 
;;; SYNOPSIS
(defmethod add-to-list-data-force (new-element key (al assoc-list))
;;; ****
  (if (get-data key al nil)
      (add-to-list-data new-element key al)
      (add (list key (list new-element)) al)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* assoc-list/set-nth-of-data
;;; FUNCTION
;;; set-nth-of-data:
;;;
;;; 
;;; 
;;; ARGUMENTS:
;;; 
;;; 
;;; RETURN VALUE: 
;;; 
;;; 
;;; EXAMPLE
;;; 
;;; 
;;; DATE
;;; 
;;; 
;;; SYNOPSIS
(defmethod set-nth-of-data (key nth new-value (al assoc-list))
;;; ****
  (let ((pos (get-position key al)))
    (when (and (not pos) (warn-not-found al))
      (warn "assoc-list::set-nth-of-data: ~
             Could not find data with key ~a in assoc-list with id ~a"
            key (id al)))
    (when pos
      (unless (listp (data (nth pos (data al))))
        (error "assoc-list::set-nth-of-data: ~
                Data for key ~a is not a list in assoc-list with id ~a"
               key (id al)))
      (setf (nth nth (data (nth pos (data al)))) new-value))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 26.1.11: map a function over the data in the assoc-list; returns a list of
;;; the return values of the function call on the data.  arguments is a list of
;;; arguments to be passed to the function.  the function must take the data in
;;; the assoc-list as a first argument.
;;; ****m* assoc-list/map-data
;;; FUNCTION
;;; map-data:
;;;
;;; 
;;; 
;;; ARGUMENTS:
;;; 
;;; 
;;; RETURN VALUE: 
;;; 
;;; 
;;; EXAMPLE
;;; 
;;; 
;;; DATE
;;; 
;;; 
;;; SYNOPSIS
(defmethod map-data ((al assoc-list) function &optional further-arguments)
;;; ****
  (loop for no in (data al) 
     for nod = (data no)
     ;; check for recursion
     if (assoc-list-p nod)
     append (map-data nod function further-arguments)
     else collect 
     (apply function (if further-arguments
                         (cons nod further-arguments)
                         (list no)))))
                         ;;; (list nod)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Related functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****f* assoc-list/make-assoc-list
;;; FUNCTION
;;; make-assoc-list:
;;;
;;; 
;;; 
;;; ARGUMENTS:
;;; 
;;; 
;;; RETURN VALUE: 
;;; 
;;; 
;;; EXAMPLE
;;; 
;;; 
;;; DATE
;;; 
;;; 
;;; SYNOPSIS
(defun make-assoc-list (id al &key (warn-not-found t))
;;; ****
  (make-instance 'assoc-list :data al :id id :warn-not-found warn-not-found))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; keys to assoc-lists can only be symbols, strings or numbers.

(defun assoc-list-id-p (id)
  (when id
    (or (symbolp id)
        (stringp id)
        (numberp id))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun check-or-force-named-object (thing)
  (if (typep thing 'named-object)
      (progn
        (unless (id thing)
          (error "~a~%assoc-list::check-or-force-named-object: ~
                  thing must have an id!" thing))
        thing)
    (progn
      (unless (and (listp thing)
                   (= 2 (length thing)))
        (error "assoc-list::check-or-force-named-object: Expected a ~
                named-object or at least a 2-element list that can be ~
                turned into one.....~%~a"
               thing))
      (make-named-object (first thing) 
                         (second thing)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun assoc-list-p (candidate)
  (typep candidate 'assoc-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; EOF assoc-list.lsp
