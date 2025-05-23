;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* circular-sclist/assoc-list
;;; NAME             
;;; assoc-list
;;;
;;; File:             assoc-list.lsp
;;;
;;; Class Hierarchy:  named-object -> linked-named-object -> sclist ->
;;;                   circular-sclist -> assoc-list 
;;;
;;; Version:          1.1.0
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
;;; $$ Last modified:  16:59:19 Wed May 21 2025 CEST
;;;
;;; SVN ID: $Id$
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

(in-package :slippery-chicken)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass assoc-list (circular-sclist)
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

#+(and clisp win32)
(defmethod print-object ((al assoc-list) stream)
  (format stream "~%ASSOC-LIST: warn-not-found ~a, ~
                  data: Not displayed due to clisp bug."
          (warn-not-found al)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Wed Aug 5 13:21:30 2015 -- this assumes the data is something simple
;;; like a list of numbers. In any case we try to print in such a way that
;;; reading back in will create an assoc-list object with the requisite
;;; data. Use the data-printer for more complex data (see e.g. sndfile's
;;; get-slots-list or indeed its class method)
(defmethod print-for-init ((al assoc-list) &key (stream t)
                                             (call 'make-assoc-list)
                                             (data-printer
                                              #'(lambda (d s) (print d s))))
  (format stream "~&(~a '~a~%  '(" call (id al))
  (loop for d in (data al) and i from 1 do
       (format stream "(~a " (id d))
       (funcall data-printer (data d) stream)
       (format stream ")")
       (unless (= i (sclist-length al))
         (format stream "~%    ")))
  (format stream ")~%  :warn-not-found ~a)~%" (warn-not-found al))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod verify-and-store :after ((al assoc-list))
  (let ((data (data al)))
    (when data
      (loop for pair in data and i from 0 do
           (if (or (typep pair 'named-object)
                   ;; pitch-seq-palettes don't have to give a key/id as
                   ;; that can be provided automatically in
                   ;; make-pitch-seq, so handle them in that class itself
                   (typep al 'pitch-seq-palette))
               ;; MDE Tue Dec 3 15:02:57 2013 -- can't have NIL IDs in an
               ;; assoc-list!
               (when (and (not (pitch-seq-palette-p al)) (not (id pair)))
                 (error "~a~%assoc-list::verify-and-store: ~
                          assoc list with id ~a: ~%~
                          all objects in an assoc-list need an ID (NIL)."
                        pair (id al)))
               ;; so nothing is done in the T clause above, if we've got a
               ;; named-object  
               (progn
                 ;; (print (first pair))
                 (unless (and (listp pair) (= 2 (length pair)))
                   (error "~a~%assoc-list::verify-and-store: ~
                         assoc list with id ~a: ~%~
                         The data slot of assoc-list must be a list containing ~
                         2-element sublists." pair (id al)))
                 (unless (assoc-list-id-p (first pair))
                   (error "assoc-list::verify-and-store: ~
                           assoc-list ids may only be symbols, strings,~%or ~
                           numbers: ~a"
                          (first pair)))
                 (setf (nth i data) (make-instance 'named-object 
                                                   :id (first pair)
                                                   :data (second pair))))))))
  (unless (typep al 'pitch-seq-palette)
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
;;; DESCRIPTION
;;; Get a simple list of the keys in a given association list.
;;;
;;; ARGUMENTS
;;; - An assoc-list.
;;;
;;; OPTIONAL ARGUMENTS
;;; - Optional argument: T or NIL (default T) to indicate whether a warning  
;;;   should be printed when the first argument is a recursive assoc-list. 
;;;
;;; RETURN VALUE 
;;; A list of the keys only of all top-level association list pairs in the
;;; given assoc-list. 
;;;
;;; get-keys is a method of the assoc-list class and therefore returns only
;;; top-level keys if accessing a recursive assoc-list.
;;; EXAMPLE 
#| 
(let ((al (make-assoc-list 'test '((cat felix) 
                                   (dog fido) 
                                   (cow bessie)))))
  (get-keys al))

=> (CAT DOG COW)

(let ((al (make-assoc-list 'test '((cat felix) 
                                   (dog ((scottish terrier)
                                         (german shepherd)
                                         (irish wolfhound))) 
                                   (cow bessie)))))
  (get-keys al))

=> (CAT DOG COW)
|# 
;;; SYNOPSIS
(defmethod get-keys ((al assoc-list) &optional (warn t))
  ;; ****
  (when (and warn (is-ral al))
    (warn "assoc-list::get-keys: ~%~
           The get-keys method comes from the assoc-list class and ~
           therefore ~%only the top-level keys are returned!"))
  (loop for obj in (data al) collect (id obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(defmethod all-ids-unique ((al assoc-list))
  (loop for obj in (data al) and i from 0 
     when (get-position (id obj) al (1+ i)) do
     (error "assoc-list::all-ids-unique: ~
                All ids in an assoc-list must be unique. ~%Found duplicate: ~
                ~a in ~%~a" (id obj) al)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* assoc-list/ascending-ids?
;;; DATE
;;; September 19th 2020, Heidhausen
;;; 
;;; DESCRIPTION
;;; Check whether all IDs are ascending integers (though order can be any), none
;;; missing, optionally starting from particular integer.
;;; 
;;; ARGUMENTS
;;; - the assoc-list object
;;; 
;;; OPTIONAL ARGUMENTS
;;; - an integer representing the lowest required ID
;;; 
;;; RETURN VALUE
;;; T or NIL
;;; 
;;; EXAMPLE
#|

(ascending-ids? (make-assoc-list 'test '((1 dog) (2 cat) (3 horse)))) -> T
;;; 3 is missing
(ascending-ids? (make-assoc-list 'test '((1 dog) (2 cat) (4 horse)))) -> NIL
;; doesn't start at 2
(ascending-ids? (make-assoc-list 'test '((1 dog) (2 cat) (3 horse))) 2) -> NIL
;; missing several integers
(ascending-ids? (make-assoc-list 'test '((4 dog) (2 cat) (7 horse))) 2) -> NIL
(ascending-ids? (make-assoc-list 'test '((4 dog) (2 cat) (3 horse))) 2) -> T
|#
;;; SYNOPSIS
(defmethod ascending-ids? ((al assoc-list) &optional starting-from)
;;; ****
  ;; we know that there are no duplicate ids
  (let* ((keys (get-keys al))
         (len (length keys))
         first last)
    (when (every #'integerp keys)
      (setq keys (sort (get-keys al) #'<)
            first (first keys)
            last (first (last keys)))
      (when (= len (1+ (- last first)))
        (or (not starting-from) (= first starting-from))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* assoc-list/get-first
;;; DESCRIPTION
;;; Returns the first named-object in the DATA slot of the given assoc-list
;;; object.  
;;; 
;;; ARGUMENTS
;;; - An assoc-list object.
;;; 
;;; RETURN VALUE 
;;; A named-object that is the first object in the DATA slot of the given
;;; assoc-list object.
;;; 
;;; EXAMPLE
#|
(let ((al (make-assoc-list 'test '((jim beam)
                                   (four roses)
                                   (wild turkey)))))
  (get-first al))

=> 
NAMED-OBJECT: id: JIM, tag: NIL,
data BEAM
|#
;;; 
;;; SYNOPSIS 
(defmethod get-first ((al assoc-list))
;;; ****
  (first (data al)))

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
;;; 07.12.11 SEAN: modified example
;;; ****m* assoc-list/get-position
;;; DESCRIPTION
;;; Returns the index position (zero-based) of a named-object within a given 
;;; assoc-list. 
;;; 
;;; ARGUMENTS
;;; - The assoc-list key symbol (named-object id) of the object for which the 
;;;   position is sought.
;;; - The assoc-list in which it is to be sought.
;;;
;;; OPTIONAL ARGUMENTS
;;; - Optional argument: An indexing integer. In this case, get-position will 
;;;   search for the given object starting part-way into the list, skipping all
;;;   objects located at indices lower than the given integer (default = 0). 
;;; 
;;; RETURN VALUE 
;;; The integer index of the named-object within the given assoc-list.
;;;
;;; NIL is returned if the object is not present in the assoc-list starting
;;; with the index number given as the start argument (i.e., in the entire list
;;; if the optional start argument is omitted).  
;;; 
;;; EXAMPLE
#|
(let ((al (make-assoc-list 'test '((jim beam) 
                                   (four roses) 
                                   (wild turkey)))))
  (get-position 'four al))

=> 1

(let ((al (make-assoc-list 'test '((jim beam) 
                                   (four roses) 
                                   (wild turkey)))))
  (get-position 'jack al))

=> NIL

(let ((al (make-assoc-list 'test '((jim beam) 
                                   (four roses) 
                                   (wild turkey)))))
  (get-position 'jim al 1))

=> NIL

;;; SYNOPSIS
|#
(defmethod get-position (key (al assoc-list) &optional (start 0))
;;; ****
  (loop for i in (nthcdr start (data al)) and j from start
     unless (named-object-p i) do
       (error "~a~&assoc-list::get-position: element ~a is not a named-object."
              al j)
        when (id-eq key i) do (return j)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Sat Jan 27 16:13:17 2024, Heidhausen -- this wasn't part of assoc-list
;;; until today but using the cscl method causes some errors. This is
;;; essentially the sclist method (grandparent instead of parent)
(defmethod get-last ((al assoc-list))
  (first (last (data al))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 28.11.11 SEAN: Added ROBODoc info
;;; 07.12.11 SEAN: modified example

;;; ****m* assoc-list/get-data-data
;;; DESCRIPTION
;;; (Short-cut for (data (get-data ...))
;;; Get the data associated with the given key of the given assoc-list. 
;;; 
;;; ARGUMENTS
;;; - The assoc-list key symbol associated with the data list which is sought. 
;;; - The assoc-list in which it is to be sought.
;;;
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to indicate whether to print a warning if no such named-object
;;;   can be found within the given assoc-list (default = T). 
;;;
;;; RETURN VALUE 
;;; If the given key is found within the given assoc-list, the data associated
;;; with that key is returned.
;;; 
;;; EXAMPLE

#| 
(let ((al (make-assoc-list 'test '((jim beam) 
                                   (four roses) 
                                   (wild turkey)))))
  (get-data-data 'jim al))

=> BEAM 

(let ((al (make-assoc-list 'test '((jim beam) 
                                   (four roses) 
                                   (wild turkey)))))
  (get-data-data 'jack al))

=> NIL
WARNING:
   assoc-list::get-data: Could not find data with key JACK in assoc-list with
   id TEST 
|#
;;; SYNOPSIS
(defmethod get-data-data (key (al assoc-list) &optional (warn t))
;;; ****
  (let ((no (get-data key al warn)))
    (when no
        (data no))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Thu Jan 26 21:03:12 GMT 2012: Minor edits to sync with ral doc
;;; 28.11.11 SEAN: Added ROBODoc info
;;; 07.12.11 SEAN: Moved Michael's comments into the ROBODoc documentation
;;; 07.12.11 SEAN: Modified the EXAMPLE block

;;; ****m* assoc-list/get-data
;;; DESCRIPTION
;;; Return the named-object (id, tag and data) that is identified by a
;;; specified key within a given assoc-list. 
;;;
;;; NB: This method returns the named object itself, not just the data
;;;     associated with the key (use get-data-data for that). 
;;; 
;;; ARGUMENTS
;;; - A symbol that is the key (id) of the named-object sought.
;;; - The assoc-list object in which it is be sought.
;;;
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to indicate whether a warning is printed if the specified key
;;;   cannot be found within the given assoc-list. T = print. Default = T.  
;;;   Mostly we define whether we want to warn in the instance itself, but  
;;;   sometimes it would be good to warn or not on a call basis, hence the 
;;;   optional argument. If a function is passed here that will be called
;;;   instead of warn (e.g. #'error)
;;; 
;;; RETURN VALUE 
;;; A named-object is returned if the specified key is found within the given
;;; assoc-list object. 
;;;
;;; NIL is returned and a warning is printed if the specified key is not found
;;; in the given assoc-list object. 
;;; 
;;; EXAMPLE
#|
(let ((al (make-assoc-list 'test '((jim beam) 
                                   (four roses) 
                                   (wild turkey)))))
  (get-data 'four al))

=> 
NAMED-OBJECT: id: FOUR, tag: NIL, 
data: ROSES

(let ((al (make-assoc-list 'test '((jim beam) 
                                   (four roses) 
                                   (wild turkey)))))
  (get-data 'jack al))

=> NIL
WARNING:
   assoc-list::get-data: Could not find data with key JACK in assoc-list with
   id TEST

(let ((al (make-assoc-list 'test '((jim beam) 
                                   (four roses) 
                                   (wild turkey)))))
  (get-data 'jack al t))

=> NIL 
WARNING:
   assoc-list::get-data: Could not find data with key JACK in assoc-list with
   id TEST

(let ((al (make-assoc-list 'test '((jim beam) 
                                   (four roses) 
                                   (wild turkey)))))
  (get-data 'jack al nil))

=> NIL
|#
;;; SYNOPSIS
(defmethod get-data (key (al assoc-list) &optional (warn t))
;;; ****
  (let ((pos (get-position key al)))
    (if pos
        (get-nth pos al)
        (when (or warn 
                  (and warn (warn-not-found al)))
          (funcall (if (functionp warn) warn #'warn)
                   "assoc-list::get-data: ~
                    Could not find data with key ~a ~%in assoc-list with id ~a"
                key (id al))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Thu Jan 26 21:28:27 GMT 2012: Edited robodoc to sync with ral
;;; 01.12.11 SEAN: Added ROBODoc info
;;; 07.12.11 SEAN: Modified example

;;; ****m* assoc-list/add
;;; DESCRIPTION
;;; Add a new element to the given assoc-list object. By default the new element
;;; is added at the end of the data list but the optional argument can change
;;; this. 
;;; 
;;; ARGUMENTS
;;; - A key/data pair as a list, or a named-object.
;;; - The assoc-list object to which it is to be added.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to add items to the front of the data list. Default = NIL = to
;;; the end
;;; 
;;; RETURN VALUE 
;;; Returns T if the specified named-object is successfully added to the given
;;; assoc-list. 
;;;
;;; Returns an error if an attempt is made to add NIL to the given assoc-list
;;; or if the given named-object is already present in the given assoc-list. 
;;; 
;;; EXAMPLE
#|
(let ((al (make-assoc-list 'test '((jim beam)
                                   (four roses)
                                   (wild turkey)))))
  (add '(makers mark) al))

=> T

(let ((al (make-assoc-list 'test '((jim beam)
                                   (four roses)
                                   (wild turkey)))))
  (add '(makers mark) al)
  (get-data 'makers al))

=>
NAMED-OBJECT: id: MAKERS, tag: NIL, 
data: MARK

(let ((al (make-assoc-list 'test '((jim beam)
                                   (four roses)
                                   (wild turkey)))))
  (add '(makers mark) al)
  (get-position 'makers al))

=> 3

(let ((al (make-assoc-list 'test '((jim beam)
                                   (four roses)
                                   (wild turkey)))))
  (add '(knob creek) al))

=> T
|#
;;; SYNOPSIS
(defmethod add (named-object (al assoc-list) &optional front)
;;; ****
  (unless named-object
    (error "assoc-list::add: named-object is NIL!"))
  (setf named-object (check-or-force-named-object named-object))
  (when (get-position (id named-object) al)
    (error "assoc-list::add: Can't add ~a to assoc-list with id ~%~a ~
            ~%because key already exists!"
           (id named-object) (id al)))
  ;; MDE Fri Nov  8 09:18:41 2019 -- until today we used to clone the
  ;; named-object but this would make all changes to it after adding null and
  ;; void 
  ;; (setf (slot-value al 'data) (econs (data al) (clone named-object)))
  ;; MDE Tue Mar 31 12:38:33 2020 -- allow items to be added to the front
  ;; MDE Thu Dec  7 14:05:26 2023, Heidhausen -- this needs thinking through
  ;; again, as basic as it is: we should be calling (setf (data al) ... so that
  ;; setf methods of all dependent classes are called, but at the moment the
  ;; tests don't pass when doing that. Might take a while to figure out.
  ;; (setf (slot-value al 'data) (if front
  ;; MDE Fri Jan 26 18:23:16 2024, Heidhausen -- fixed. The issues was in
  ;; ral-to-set-palette where one tricky case wasn't being handled properly  
  (setf (data al) (if front
                      (cons named-object (data al))
                      (econs (data al) named-object)))
  ;; MDE Wed Jan 31 18:15:27 2024, Heidhausen -- no need to do this here anymore
  ;; as the verify-and-store method recalculates the length 
  ;; (incf (sclist-length al))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Fri Jan 27 15:33:35 GMT 2012: Edited robodoc info to sync with ral doc 
;;; 01.12.11 SEAN: Added ROBODoc info
;;; 07.12.11 SEAN: Modified example

;;; ****m* assoc-list/set-data
;;; DESCRIPTION
;;; Replace the named-object associated with a specified key within a given
;;; assoc-list object. This method replaces the whole named-object, not just
;;; the data of that object. 
;;; 
;;; ARGUMENTS
;;; - A key present within the given assoc-list object.
;;; - A key/data pair as a list.
;;; - The assoc-list object in which to find and replace the named-object
;;;   associated with the specified key.
;;; 
;;; RETURN VALUE 
;;; Returns the new named-object.
;;; 
;;; Returns NIL when the given key is not present within the given assoc-list.
;;;
;;; EXAMPLE
#|
(let ((al (make-assoc-list 'test '((cat felix)
                                   (dog fido)
                                   (cow bessie)))))
  (set-data 'dog '(dog spot) al))

=> 
NAMED-OBJECT: id: DOG, tag: NIL, 
data: SPOT

(let ((al (make-assoc-list 'test '((cat felix)
                                   (dog fido)
                                   (cow bessie)))))
  (set-data 'pig '(pig wilbur) al))

=> NIL
WARNING:
   assoc-list::set-data: Could not find data with key PIG in assoc-list with id
   TEST 

(let ((al (make-assoc-list 'test '((cat felix)
                                   (dog fido)
                                   (cow bessie)))))
  (set-data 'dog '(pig wilbur) al))

=> 
NAMED-OBJECT: id: PIG, tag: NIL, 
data: WILBUR
|#
;;; SYNOPSIS
(defmethod set-data (key new-value (al assoc-list))
;;; ****
  (setq new-value (check-or-force-named-object new-value))
  (let ((pos (get-position key al)))
    (when (and (not pos) (warn-not-found al))
      (warn "assoc-list::set-data: ~
             Could not find data with key ~a in assoc-list with id ~a"
            key (id al)))
    (when pos
      (setf (nth pos (data al)) new-value))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* assoc-list/replace-data
;;; DATE 19th June 2013
;;; 
;;; DESCRIPTION
;;; A convenience method that's essentially the same as set-data but without
;;; having to pass the new ID in a list along with the new value. 
;;; 
;;; ARGUMENTS
;;; - The key (number, symbol, string) to the existing data.
;;; - The new value to be associated with this key (any data).
;;; - The assoc-list object
;;; 
;;; RETURN VALUE
;;; The named-object for the datum.
;;; 
;;; SYNOPSIS
(defmethod replace-data (key new-value (al assoc-list))
;;; ****
  (set-data key (list key new-value) al))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 01.12.11 SEAN: Added ROBODoc info
;;; 07.12.11 SEAN: Modified example
;;; ****m* assoc-list/add-to-list-data
;;; DESCRIPTION
;;; Add an element of any type to the end of the data (list) associated with a
;;; given key of a given assoc-list.
;;;
;;; The data associated with the given key must already be a list.
;;; 
;;; ARGUMENTS
;;; - An item of any type.
;;; - A given key that must be present in the given assoc-list.
;;; - The given assoc-list.
;;; 
;;; RETURN VALUE 
;;; Returns the whole named-object to which the new element was added.
;;;
;;; This method will abort with an error if a key is sought which does not
;;; exist within the given assoc-list. For such cases, use
;;; add-to-list-data-force instead.
;;; 
;;; EXAMPLE
#|
(let ((al (make-assoc-list 'test '((cat felix)
                                   (dog (fido spot))
                                   (cow bessie)))))
  (add-to-list-data 'rover 'dog al))

=> 
NAMED-OBJECT: id: DOG, tag: NIL, 
data: (FIDO SPOT ROVER)
|#
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

;;; 01.12.11 SEAN: Added ROBODoc info
;;; 07.12.11 SEAN: Modified example
;;; ****m* assoc-list/add-to-list-data-force
;;; DESCRIPTION
;;; Similar to add-to-list-data, but if the given key doesn't already exist in
;;; the given assoc-list, it is first added, then the given new element is
;;; added to that as a 1-element list. 
;;;
;;; If the given key already exists within the given assoc-list, its data must
;;; already be in the form of a list.
;;; 
;;; ARGUMENTS
;;; - A (new) element of any type.
;;; - A given key that may or may not be present in the given assoc-list.
;;; - The the given assoc-list.
;;; 
;;; RETURN VALUE 
;;; Returns the whole named-object to which the element was added when used
;;; with a key that already exists within the given assoc-list.
;;;
;;; Returns T when used with a key that does not already exist in the given
;;; assoc-list. 
;;; 
;;; EXAMPLE
#|
(let ((al (make-assoc-list 'test '((cat felix)
                                   (dog (fido spot))
                                   (cow bessie)))))
  (add-to-list-data-force 'rover 'dog al))

=> 
NAMED-OBJECT: id: DOG, tag: NIL, 
data: (FIDO SPOT ROVER)

(let ((al (make-assoc-list 'test '((cat felix)
                                   (dog (fido spot))
                                   (cow bessie)))))
  (add-to-list-data-force 'wilbur 'pig al)
  (get-keys al))

=> (CAT DOG COW PIG)
|#
;;; SYNOPSIS
(defmethod add-to-list-data-force (new-element key (al assoc-list))
;;; ****
  (if (get-data key al nil)
      (add-to-list-data new-element key al)
      (add (list key (list new-element)) al)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 01.12.11 SEAN: Added ROBODoc info
;;; 08.12.11 SEAN: Modified example
;;; ****m* assoc-list/set-nth-of-data
;;; DESCRIPTION
;;; Replace a given member of a given data list within a given assoc-list.
;;; 
;;; ARGUMENTS
;;; - The key (named-object id) associated with the data to be changed.
;;; - The zero-based integer index of the member of the list to be changed.
;;; - The new value.
;;; - The assoc-list in which the change is to be made.
;;; 
;;; The data to be modified must already be in the form of a list.
;;;
;;; The index integer given must be less than the length of the data list to be
;;; modified. 
;;;
;;; RETURN VALUE 
;;; Returns the new value only.
;;;
;;; EXAMPLE
#|
(let ((al (make-assoc-list 'test '((cat felix)
                                   (dog (fido spot rover))
                                   (cow bessie)))))
  (set-nth-of-data 'dog 0 'snoopy al))

=> SNOOPY

(let ((al (make-assoc-list 'test '((cat felix)
                                   (dog (fido spot rover))
                                   (cow bessie)))))
  (set-nth-of-data 'dog 0 'snoopy al) 
  (get-data 'dog al))

=> 
NAMED-OBJECT: id: DOG, tag: NIL, 
data: (SNOOPY SPOT ROVER)
|#
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
;;; ****m* assoc-list/map-data
;;; DESCRIPTION
;;; Map a function over the data in the assoc-list, i.e. call the given
;;; function/method on each data slot of each named-object in the assoc-list
;;; data slot. See also recursive-assoc-list's rmap method which does pretty
;;; much the same but acting recursively on each named-object (unless it is
;;; itself recursive), rather than the named-object's data.
;;; 
;;; ARGUMENTS
;;; - The assoc-list to which the function is to be applied.
;;; - The function to be applied.  This must take the data in the assoc-list as
;;;   a first argument. 
;;;
;;; OPTIONAL ARGUMENTS
;;; - &rest: Further arguments for the function.
;;; 
;;; RETURN VALUE 
;;; Returns a list of the values returned by the function call on the data.
;;; 
;;; EXAMPLE
#|
(let ((al (make-assoc-list 'al-test
                           '((1 (1 2 3 4))
                             (2 (5 6 7 8))
                             (3 (9 10 11 12))))))
  (map-data al #'(lambda (y) 
                   (loop for i in y collect (* i 2)))))

=> ((2 4 6 8) (10 12 14 16) (18 20 22 24))
|#
;;; SYNOPSIS
(defmethod map-data ((al assoc-list) function &rest further-arguments)
;;; ****
  (loop for no in (data al) 
     for nod = (data no)
     ;; check for recursion
     if (assoc-list-p nod)
     append (map-data nod function further-arguments)
     else collect 
     (apply function (if further-arguments
                         (cons nod further-arguments)
                         ;; MDE Thu Nov  1 11:20:02 2018 -- this was an error:
                         ;; we should pass nod not no 
                         ;; (list no)))))
                         (list nod)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* assoc-list/nmap-data
;;; DATE
;;; November 1st 2018, Heidhausen
;;; 
;;; DESCRIPTION
;;; A destructive version of map-data: replace the data slot with the
;;; named-object returned by the 2nd argument after it is passed one
;;; named-object after another. 
;;; 
;;; ARGUMENTS
;;; See the map-data method in this class.
;;; 
;;; RETURN VALUE
;;; the assoc-list object with modified data in its elements.
;;; 
;;; EXAMPLE
#|
(let ((al (make-assoc-list 'test '((1 (2 3))
                                   (2 (4 5 6))
                                   (3 (7))))))
  (nmap-data al #'(lambda (data) (length data)))
  al)
...
data: (
NAMED-OBJECT: id: 1, tag: NIL, 
data: 2
**************
NAMED-OBJECT: id: 2, tag: NIL, 
data: 3
**************
NAMED-OBJECT: id: 3, tag: NIL, 
data: 1
...
|#
;;; SYNOPSIS
(defmethod nmap-data ((al assoc-list) function &rest further-arguments)
;;; ****
  (let ((new-data (apply #'map-data
                         (cons al (cons function further-arguments)))))
    (loop for no in (data al) and nd in new-data do
         (setf (data no) nd))
    al))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* assoc-list/get-nearest
;;; DATE
;;; 4/8/2015
;;; 
;;; DESCRIPTION
;;; In assoc-lists with numeric IDs, return the object with the closest ID to
;;; the given key.
;;; 
;;; ARGUMENTS
;;; - they key (number)
;;; - the assoc-list object
;;; 
;;; RETURN VALUE
;;; a named-object
;;;
;;; SYNOPSIS
(defmethod get-nearest (key (al assoc-list) &optional ignore1 ignore2)
;;; ****
  (declare (ignore ignore1 ignore2))
  (unless (numberp key)
    (error "assoc-list::get-nearest: key should be numeric: ~a~%~a"
           key al))
  (let* ((diff most-positive-double-float)
         (cdiff diff)
         it)
    ;; we can't assume the data is sorted from high to low so we have to look
    ;; at each datum 
    (loop for no in (data al) for id = (id no) do
         (unless (numberp id)
           (error "assoc-list::get-nearest: all keys should be numeric: ~a~%~a"
                  id al))
         (setq diff (abs (- id key)))
         ;; <= rather than < so that 80.5 rounds to 81
         (when (<= diff cdiff)
           (setq it no
                 cdiff diff)))
    it))
   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* assoc-list/remove-data
;;; DATE
;;; February 1st 2016
;;; 
;;; DESCRIPTION
;;; Convenience function to allow objects to be removed from an assoc-list (and
;;; subclasses e.g. palettes and maps) 
;;; 
;;; ARGUMENTS
;;; - the assoc-list object
;;; - as many keys as you like for the members of the assoc-list. If none are
;;;   passed then all are removed. 
;;; 
;;; RETURN VALUE
;;; The assoc-list object's new data list, i.e. with elements removed. Could of
;;; course be NIL if you've not passed any keys.
;;; 
;;; SYNOPSIS
(defmethod remove-data ((al assoc-list) &rest keys)
;;; ****
  ;; (print 'remove-data) (print keys) ;(print al)
  (unless keys (setq keys (get-keys al)))
  (let ((data (data al)))
    (loop for k in keys do
         ;; MDE Wed Jun 28 09:50:39 2017 -- mainly for ral's
         (when (id-as-list k) 
           (setq k (first k)))
         (setq data (remove k data :test #'id-eq)))
    (setf (data al) data)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod (setf data) :after (value (al assoc-list))
  (declare (ignore value))
  (verify-and-store al))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; returns a list of the keys removed
;;; ****m* assoc-list/remove-when
;;; DATE
;;; June 28th 2017, Edinburgh
;;; 
;;; DESCRIPTION
;;; Use a function to test each object in an assoc-list and if the test returns
;;; T, then remove the object. Works in recursive-assoc-list class too. 
;;; 
;;; ARGUMENTS
;;; - an assoc-list object
;;; - a function that returns T or NIL after interrogating a named-object or
;;;   subclass's slots
;;; 
;;; RETURN VALUE
;;; a list of the IDs of the objects removed.
;;; 
;;; EXAMPLE
#|
(let ((al (make-assoc-list 'mixed-bag 
                            '((jim beam)
                              (wild turkey)
                              (four roses)))))
  (remove-when al #'(lambda (x) (eq (data x) 'roses))))
|#
;;; SYNOPSIS
(defmethod remove-when ((al assoc-list) test)
;;; ****
  (remove-when-aux al #'get-keys test))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* assoc-list/reindex
;;; DATE
;;; 14th June 2017, Edinburgh
;;; 
;;; DESCRIPTION
;;; Re-index the named-objects to consecutive integers starting from a given
;;; number.  
;;; 
;;; ARGUMENTS
;;; - the assoc-list object
;;; 
;;; OPTIONAL ARGUMENTS
;;; - the integer to start reindexing at
;;; 
;;; RETURN VALUE
;;; the assoc-list object
;;; 
;;; SYNOPSIS
(defmethod reindex ((al assoc-list) &optional (start 1))
  ;;; ****
  (loop for thing in (data al) and i from start do (setf (id thing) i))
  al)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Related functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 01.12.11 SEAN: Added ROBODoc info
;; ****f* assoc-list/make-assoc-list
;; DESCRIPTION
;; A function that provides a shortcut to creating an assoc-list, filling it
;; with data, and assigning a name to it.
;;
;; ARGUMENTS
;; - The name of the assoc-list to be created.
;; - The data with which to fill it.
;;
;; OPTIONAL ARGUMENTS
;; keyword arguments:
;; - :warn-not-found. T or NIL to indicate whether a warning is printed when an
;;   index which doesn't exist is used for look-up.  T = warn. Default = T.
;;
;; RETURN VALUE 
;; Returns the assoc-list as a named-object.
;;
;; EXAMPLE
#|
(make-assoc-list 'looney-tunes '((bugs bunny)
                                 (daffy duck)
                                 (porky pig)))
=> 
ASSOC-LIST: warn-not-found T
CIRCULAR-SCLIST: current 0
SCLIST: sclist-length: 3, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL
                     this: NIL
                     next: NIL
NAMED-OBJECT: id: LOONEY-TUNES, tag: NIL, 
data: (
NAMED-OBJECT: id: BUGS, tag: NIL, 
data: BUNNY
       
NAMED-OBJECT: id: DAFFY, tag: NIL, 
data: DUCK
      
NAMED-OBJECT: id: PORKY, tag: NIL, 
data: PIG)
|#
;; SYNOPSIS
(defun make-assoc-list (id al &key (warn-not-found t))
;;; ****
  (make-instance 'assoc-list :data al :id id :warn-not-found warn-not-found))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; keys to assoc-lists can only be symbols, strings or numbers.

(defun assoc-list-id-p (id)
  (when id
    (or (symbolp id)
        (stringp id)
        ;; MDE Thu May 26 12:17:33 2016
        (morph-p id)
        (numberp id))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun check-or-force-named-object (thing)
  (if (typep thing 'named-object)
      (progn
        (unless (id thing)
          (error "~a~%assoc-list::check-or-force-named-object: ~
                  thing must have an id!" thing))
        thing)
      (let ((len (length thing)))
        (unless (and (listp thing)
                     ;; MDE Tue Mar 31 12:16:11 2020 -- allow tags to be passed
                     (or (= 3 len) (= 2 len)))
          (error "assoc-list::check-or-force-named-object: Expected a ~
                  named-object or at least ~%a 2- or 3-element list that can ~
                  be turned into one: ~%~a"
                 thing))
        (make-named-object (first thing) 
                           (second thing)
                           ;; MDE Tue Mar 31 12:16:11 2020 -- allow tags to be
                           ;; passed
                           (third thing)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun assoc-list-p (candidate)
  (typep candidate 'assoc-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Tue Jun 27 10:53:04 2017 -- used by this and ral class, the
;;; key-accessor is either get-keys or get-all-refs, according to class 
(defun remove-when-aux (assoc-list key-accessor test)
  (let* ((all-keys (funcall key-accessor assoc-list))
         ;; we can use the remove-data method (assoc-list and ral classes) to
         ;; remove the data so just find the refs of the data to remove first
         (rm-keys
          (loop for key in all-keys
             for thing = (get-data key assoc-list)
             when (funcall test thing)
             collect key)))
    ;; (print rm-keys)
    (apply #'remove-data (cons assoc-list rm-keys))
    (verify-and-store assoc-list)
    rm-keys))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; EOF assoc-list.lsp
