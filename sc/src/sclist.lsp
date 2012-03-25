;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* linked-named-object/sclist
;;; NAME 
;;; player
;;;
;;; File:             sclist.lsp
;;;
;;; Class Hierarchy:  named-object -> linked-named-object -> sclist
;;;
;;; Version:          0.92
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
;;; Return a subsequence from a given sclist based on starting and finishing
;;; indices. 
;;;
;;; NB: This method uses Common Lisp's subseq function and thus inherits its
;;; attributes, whereby the START argument indicates the zero-based index of
;;; the first list item to be returned and teh FINISH argument indicates the
;;; zero-based index of the first list item after that NOT to be returned.
;;; 
;;; ARGUMENTS
;;; - An sclist object.
;;; - An integer indicating the zero-based index of the first list item to be
;;;   returned. 
;;; - An integer indicating the zero-based index of the first list item after
;;;   the START item to not be returned.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - (fun #'error).
;;; 
;;; RETURN VALUE
;;; A list.
;;;
;;; An error is returned if the user attempts to apply the method with START
;;; and FINISH arguments that are beyond the bounds of the given sclist
;;; object. 
;;; 
;;; EXAMPLE
#|
;; Returns a sublist from the given list. The START argument indicates the
;; zero-based index of the first item in the given list to be returned and the
;; FINISH argument indicates the zero-based index of the first item after that
;; to NOT be returned. 
(let ((scl (make-sclist '(1 2 3 4 5 6 7 8 9))))
  (sc-subseq scl 2 7))

=> (3 4 5 6 7)

;; Drops into the debugger with an error if one of the indexing arguments is
;; beyond the bounds of the given sclist object
(let ((scl (make-sclist '(1 2 3 4 5 6 7 8 9))))
  (sc-subseq scl 0 15))

=
sclist::sc-subseq: Illegal indices for above list: 0 15 (length = 9)
   [Condition of type SIMPLE-ERROR]

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

;;; SAR Thu Jan 12 21:11:34 GMT 2012: Added robodoc info

;;; ****m* sclist/sc-nthcdr
;;; FUNCTION
;;; Get the tail (rest) of a given sclist object beginning with and including
;;; the specified zero-based index number.
;;;
;;; NB: This method is destructive and replaces the contents of the given list
;;; with the sublist returned by the method.
;;; 
;;; ARGUMENTS
;;; - An index number.
;;; - An sclist object
;;; 
;;; RETURN VALUE
;;; Returns a list.
;;;
;;; Returns NIL if the specified index is greater (minus 1) than the number of
;;; items in the given list.
;;; 
;;; EXAMPLE
#|
;; Create an sclist object and get the tail of the list starting at place
;; 4. The subset returned replaces the data of the original.
(let ((scl (make-sclist '(0 1 2 3 4 5 6 7 8 9))))
  (sc-nthcdr 4 scl)
  (data scl))

=> (4 5 6 7 8 9)

(let ((scl (make-sclist '(0 1 2 3 4 5 6 7 8 9))))
  (sc-nthcdr 14 scl))

=> NIL

|#
;;; SYNOPSIS
(defmethod sc-nthcdr (nth (scl sclist))
;;; ****
  (setf (data scl) (nthcdr nth (data scl))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Thu Jan 12 21:34:40 GMT 2012: Added robodoc info

;;; ****m* sclist/sclist-econs
;;; FUNCTION
;;; Add a single item to the end of a given sclist object.
;;;
;;; NB: This method destructively modifies the list.
;;;
;;; NB: This method adds any element specified as a single item. For combining
;;; two lists into one see sclist/combine.
;;; 
;;; ARGUMENTS
;;; - An sclist object.
;;; - An item to add to the end of the given sclist object.
;;; 
;;; RETURN VALUE
;;; - The new value (list) of the given sclist object.
;;; 
;;; EXAMPLE
#|
;; Add a single integer to the end of a list of integers
(let ((scl (make-sclist '(0 1 2 3 4))))
  (sclist-econs scl 5))

=> (0 1 2 3 4 5)

|#
;;; SYNOPSIS
(defmethod sclist-econs ((scl sclist) element)
;;; ****
  (setf (data scl) (econs (data scl) element)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Thu Jan 12 22:27:11 GMT 2012: Added robodoc info

;;; ****m* sclist/get-nth
;;; FUNCTION
;;; Get the nth element (zero-based) of data in a given sclist object.
;;; 
;;; ARGUMENTS
;;; - An index integer.
;;; - An sclist object.
;;; 
;;; RETURN VALUE
;;; Returns the item at index n within the given sclist object.
;;; 
;;; Returns NIL and prints a warning if the specified index is greater than the
;;; number of items in the given list (minus 1).
;;;
;;; EXAMPLE

#|
;; Get the 3th item from the given sclist object
(let ((scl (make-sclist '(cat dog cow pig sheep))))
  (get-nth 3 scl))

=> PIG

;; Returns NIL and prints a warning when the specified index is beyond the
;; bounds of the given list
(let ((scl (make-sclist '(cat dog cow pig sheep))))
  (get-nth 31 scl))

=> 
NIL
WARNING: sclist::sclist-check-bounds: Illegal list reference: 31 
(length = 5) (sclist id = NIL)

|#
;;; SYNOPSIS
(defmethod get-nth (index (i sclist))
;;; ****
  (when (and i (sclist-check-bounds i index))
    (nth index (data i))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Thu Jan 12 22:35:56 GMT 2012: Added robodoc info

;;; ****m* sclist/set-nth
;;; FUNCTION
;;; Set the nth element (zero-based) of a given sclist object.
;;;
;;; NB: This doesn't auto-grow the list.  
;;; 
;;; ARGUMENTS
;;; - An index integer.
;;; - An sclist object.
;;; 
;;; RETURN VALUE
;;; Returns the item added if successful.
;;;
;;; Returns NIL and prints a warning if the specified index number is greater
;;; than the number of items in the list (minus 1)
;;; 
;;; EXAMPLE

#|
;; Returns the item added
(let ((scl (make-sclist '(cat dog cow pig sheep))))
  (set-nth 3 'horse scl))

=> HORSE

;; Access the DATA slot to see the change
(let ((scl (make-sclist '(cat dog cow pig sheep))))
  (set-nth 3 'horse scl)
  (data scl))

=> (CAT DOG COW HORSE SHEEP)

;; Returns NIL and prints a warning if the index number is beyond the bounds of
;; the list
(let ((scl (make-sclist '(cat dog cow pig sheep))))
  (set-nth 31 'horse scl))

=> NIL
WARNING: sclist::sclist-check-bounds: Illegal list reference: 31 
(length = 5) (sclist id = NIL)

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

;;; SAR Thu Jan 12 22:49:24 GMT 2012: Added robodoc info

;;; ****m* sclist/sclist-remove-elements
;;; FUNCTION
;;; Remove a specified number of consecutive items from a given sclist object. 
;;;
;;; NB: This is a destructive method and replaces the data of the given sclist
;;; object with the newly created sublist.
;;; 
;;; ARGUMENTS
;;; - An sclist object.
;;; - The index integer within the given list with which to start (inclusive
;;;   and zero-based).
;;; - An integer that is the number of items to remove.
;;; 
;;; RETURN VALUE
;;; Returns 
;;; 
;;; EXAMPLE
#|
;;; Returns an sclist object.
(let ((scl (make-sclist '(0 1 2 3 4 5 6 7 8 9))))
  (sclist-remove-elements scl 3 4))

=> 
SCLIST: sclist-length: 6, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: NIL, tag: NIL, 
data: (0 1 2 7 8 9)

;; Drops into the debugger with an error if the given sclist object has fewer
;; items than specified for the START or HOW-MANY arguments
(let ((scl (make-sclist '(0 1 2 3 4 5 6 7 8 9))))
  (data (sclist-remove-elements scl 3 41)))

=> 
remove-elements: arguments 2 and 3 must be integers < the length of argument 1: 
3 41 10 
   [Condition of type SIMPLE-ERROR]

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

;;; SAR Thu Jan 12 23:01:18 GMT 2012: Added robodoc info

;;; ****m* sclist/combine
;;; FUNCTION
;;; Combine the contents of two given sclist objects into one list.
;;; 
;;; ARGUMENTS
;;; - A first sclist object.
;;; - A second sclist ojbect.
;;; 
;;; RETURN VALUE
;;; Returns an sclist object.
;;; 
;;; EXAMPLE
#|
;; Combine the contents of two sclist objects to make a new one
(let ((scl1 (make-sclist '(0 1 2 3 4)))
      (scl2 (make-sclist '(5 6 7 8 9))))
  (combine scl1 scl2))

=> 
SCLIST: sclist-length: 10, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: NIL, tag: NIL, 
data: (0 1 2 3 4 5 6 7 8 9)

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
;;; Create an sclist object with the specified list.
;;; 
;;; ARGUMENTS
;;; - A list of numbers or symbols.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - keyword argument :id. A symbol that will be the ID of the given sclist
;;;   object. Default = NIL.
;;; - keyword argument :bounds-alert. T or NIL to indicate whether a warning
;;;   should be issued when a request is given to set or get an out-of-bounds
;;;   element (i.e. not enough elements in list). T = print warning. Default =
;;;   NIL.  
;;; - keyword argument :copy. T or NIL to indicate whether the data list of the
;;;   given sclist object should be copied, with modifications being applied to
;;;   the copy, or modified itself (setf'd). T = copy. Default = T.
;;; 
;;; RETURN VALUE
;;; Returns an sclist object. 
;;; 
;;; EXAMPLE
#|
;; Create a simple object with just a list of numbers
(make-sclist '(1 2 3 4 5 6 7))

=> 
SCLIST: sclist-length: 7, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: NIL, tag: NIL, 
data: (1 2 3 4 5 6 7)

;; Create the same object and assign an ID to it
(make-sclist '(1 2 3 4 5 6 7) :id 'number-list)

=> 
SCLIST: sclist-length: 7, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: NUMBER-LIST, tag: NIL, 
data: (1 2 3 4 5 6 7)

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

