;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* assoc-list/l-for-lookup
;;; NAME 
;;; l-for-lookup
;;;
;;; File:             l-for-lookup
;;;
;;; Class Hierarchy:  named-object -> linked-named-object -> sclist -> 
;;;                   circular-sclist -> assoc-list -> l-for-lookup
;;;
;;; Version:          1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Implementation of the l-for-lookup class.  The name
;;;                   stands for L-System for Lookups (L for
;;;                   Lindenmayer).  This provides an L-System
;;;                   function for generating sequences of numbers
;;;                   from rules and seeds, and then using these
;;;                   numbers for lookups into the assoc-list.  In the
;;;                   assoc list are stored groups of numbers, meant
;;;                   to represent in the first place, for example,
;;;                   rhythmic sequences.  The grouping could be as
;;;                   follows: ((2 3 7) (11 12 16) (24 27 29) and
;;;                   would mean that a transition should take place
;;;                   (over the length of the number of calls
;;;                   represented by the number of L-Sequence results)
;;;                   from the first group to the second, then from
;;;                   the second to the third.  When the first group
;;;                   is in use, then we will simple cycle around the
;;;                   given values, similar with the other groups.
;;;                   The transition is based on a fibonacci algorithm
;;;                   (see below).
;;;
;;;                   The sequences are stored in the data slot. The l-sequence
;;;                   will be a list like (3 1 1 2 1 2 2 3 1 2 2 3 2 3 3 1).
;;;                   These are the references into the assoc-list (the 1, 2, 3
;;;                   ids in the list below).
;;;
;;;                   e.g. ((1 ((2 3 7) (11 16 12)))
;;;                         (2 ((4 5 9) (13 14 17)))
;;;                         (3 ((1 6 8) (15 18 19))))
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    15th February 2002
;;;
;;; $$ Last modified: 11:55:43 Mon Jan  2 2012 ICT
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

(defclass l-for-lookup (assoc-list)
  ((rules :accessor rules :initarg :rules :initform nil)
   (auto-check-redundancy :accessor auto-check-redundancy :type boolean
                          :initarg :auto-check-redundancy :initform nil)
   ;; what get-l-sequence returns
   (l-sequence :accessor l-sequence :type list :initform nil)
   ;; a list with the number of repetitions of each rule key in l-sequence
   (l-distribution :accessor l-distribution :type list :initform nil)
   ;; a list with the number of repetitions of each rule key in the
   ;; result of do-lookup  
   (ll-distribution :accessor ll-distribution :type list :initform nil)
   ;; in do-lookup, what to scale the values returned by.
   (scaler :accessor scaler :type number :initarg :scaler :initform 1)
   ;; sim. but added to values (after they are scaled)
   (offset :accessor offset :type number :initarg :offset :initform 0)
   ;; when the l-sequence calls a specific group, then we have to know which
   ;; sequence in that group we're now going to access to return the next
   ;; element from the circular-sclist.  Which sequence depends on how many
   ;; times a group is called (as stored in l-distribution).  This number is
   ;; used to create the fibonacci transitions from the first group to the
   ;; last.  These are what we store here as a list of circular-sclists, one
   ;; for each group (element in the assoc-list).
   (group-indices :accessor group-indices :initform nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize-instance :after ((lflu l-for-lookup) &rest initargs)
  (declare (ignore initargs))
  (when (auto-check-redundancy lflu)
    ;; try different stop lengths to catch identical results
    (loop for i in '(10 20 50 100 200) do
          (check-redundant-seeds lflu i))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
(defmethod verify-and-store :after ((lflu l-for-lookup))
  (let ((al (make-assoc-list (format nil "~a-rules" (id lflu))
                             (rules lflu))))
    ;; the data of each item in the data slot (the sequences) is a list of
    ;; references or whatever: this will be looped in a circular fashion so
    ;; create circular-sclists from this data.
    (loop for no in (data lflu) and i from 0 do
          (setf (data (nth i (data lflu)))
            (loop for group in (data no) and j from 1
                collect (make-cscl group :id (format nil "~a-~a" (id no) j)))))
    (setf (rules lflu) al)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object :before ((lflu l-for-lookup) stream)
  (format stream "~%L-FOR-LOOKUP: rules: ~a~
                  ~%              l-sequence: ~a~
                  ~%              l-distribution: ~a~
                  ~%              ll-distribution: ~a~
                  ~%              group-indices: ~a~
                  ~%              scaler: ~a~
                  ~%              offset: ~a~
                  ~%              auto-check-redundancy: ~a"
          (rules lflu)
          (l-sequence lflu)
          (l-distribution lflu)
          (ll-distribution lflu)
          (group-indices lflu)
          (scaler lflu)
          (offset lflu)
          (auto-check-redundancy lflu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone ((lflu l-for-lookup))
  (clone-with-new-class lflu 'l-for-lookup))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone-with-new-class :around ((lflu l-for-lookup) new-class)
  (declare (ignore new-class))
  (let ((assoc-list (call-next-method)))
    (setf (slot-value assoc-list 'rules) (clone (rules lflu))
          (slot-value assoc-list 'scaler) (scaler lflu)
          (slot-value assoc-list 'offset) (offset lflu)
          (slot-value assoc-list 'auto-check-redundancy)
          (auto-check-redundancy lflu)
          (slot-value assoc-list 'l-sequence) (my-copy-list (l-sequence lflu))
          (slot-value assoc-list 'l-distribution)
          (my-copy-list (l-distribution lflu))
          (slot-value assoc-list 'll-distribution)
          (my-copy-list (ll-distribution lflu))
          (slot-value assoc-list 'group-indices)
          (my-copy-list (group-indices lflu)))
    assoc-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat Jan 14 14:07:14 GMT 2012: Edited robodoc info

;;; ****m* l-for-lookup/do-simple-lookup
;;; FUNCTION
;;; Performs a simple lookup procedure whereby a given reference key always
;;; returns a specific and single piece of data. This is different from
;;; do-lookup, which performs a transitioning between lists and returns items
;;; from those lists in a circular manner. do-simple-lookup always returns the
;;; first element of the sequence list associated with a given key-ID.
;;;
;;; N.B. the SCALER and OFFSET slots are ignored by this method.
;;; 
;;; ARGUMENTS 
;;; - An l-for-lookup object.
;;; - The start seed, or axiom, that is the initial state of the L-system. This
;;;   must be the key-id of one of the sequences.
;;; - An integer that is the number of elements to be returned.
;;; 
;;; RETURN VALUE  
;;; 
;;; 
;;; EXAMPLE
#|
;; Create an l-for-lookup object using three production rules and three
;; sequences of three lists. Applying do-simple-lookup returns the first
;; element of each sequence based on the L-sequence of keys created by the
;; rules of the give l-for-lookup object.
(let ((lfl (make-l-for-lookup 
	    'lfl-test
	    '((1 ((ax1 ax2 ax3) (ay1 ay2 ay3 ay4) (az1 az2 az3 az4 az5)))
	      (2 ((bx1 bx2 bx3) (by1 by2 by3 by4) (bz1 bz2 bz3 bz4 bz5)))
	      (3 ((cx1 cx2 cx3) (cy2 cy2 cy3 cy4) (cz1 cz2 cz3 cz4 cz5))))
	    '((1 (1 2 2 2 1 1))
	      (2 (2 1 2 3 2))
	      (3 (2 3 2 2 2 3 3))))))
  (do-simple-lookup lfl 1 21))

=> ((AX1 AX2 AX3) (BX1 BX2 BX3) (BX1 BX2 BX3) (BX1 BX2 BX3) (AX1 AX2 AX3)
    (AX1 AX2 AX3) (BX1 BX2 BX3) (AX1 AX2 AX3) (BX1 BX2 BX3) (CX1 CX2 CX3)
    (BX1 BX2 BX3) (BX1 BX2 BX3) (AX1 AX2 AX3) (BX1 BX2 BX3) (CX1 CX2 CX3)
    (BX1 BX2 BX3) (BX1 BX2 BX3) (AX1 AX2 AX3) (BX1 BX2 BX3) (CX1 CX2 CX3)
    (BX1 BX2 BX3))

|#
;;; SYNOPSIS
(defmethod do-simple-lookup ((lflu l-for-lookup) seed stop)
;;; ****
  (reset lflu)
  (get-l-sequence lflu seed stop)
  (loop for ref in (l-sequence lflu) collect
        ;; the first data is the data of the named-object, the data of which is
        ;; a list of circular-sclists, we have simple data so we have to get
        ;; the data from the first cscl in the list.
        (data (first (data (get-data ref lflu))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat Jan 14 13:29:20 GMT 2012: Edited robodoc info

;;; ****m* l-for-lookup/do-lookup
;;; FUNCTION
;;; Generate an L-sequence from the rules of the specified l-for-lookup object
;;; and use it to perform the Fibonacci-based transitioning lookup of values in
;;; the specified sequences. 
;;; 
;;; ARGUMENTS 
;;; - An l-for-lookup object.
;;; - The start seed, or axiom, that is the initial state of the L-system. This
;;;   must be the key-id of one of the sequences.
;;; - An integer that is the length of the sequence to be returned. NB: This
;;;   number does not indicate the number of L-system passes, but only the
;;;   number of elements in the list returned, which may be the first segment
;;;   of a sequence returned by a pass that actually generates a much longer
;;;   sequence. 
;;;
;;; OPTIONAL ARGUMENTS
;;; - A number which is the factor by which returned numerical values are to be
;;;   scaled. If NIL, the method will use the value in the given l-for-lookup
;;;   object's SCALER slot instead. Default = NIL. NB: The value of the given
;;;   l-for-lookup object's OFFSET slot is additionally used to increase
;;;   numerical values before they are returned.
;;; 
;;; RETURN VALUE
;;; This method returns three lists:
;;; - The resulting sequence.
;;; - The distribution of the values returned by the lookup.
;;; - The L-sequence of the key-IDs.
;;; 
;;; EXAMPLE
#|

;; Create an l-for-lookup object in which the sequences are defined such that
;; the transition takes place over the 3 given lists and from x to y to z, and
;; apply the do-lookup method to see the results. Each time one of these lists
;; is accessed, it will circularly return the next value. 
(let ((lfl (make-l-for-lookup 
	    'lfl-test
	    '((1 ((ax1 ax2 ax3) (ay1 ay2 ay3 ay4) (az1 az2 az3 az4 az5)))
	      (2 ((bx1 bx2 bx3) (by1 by2 by3 by4) (bz1 bz2 bz3 bz4 bz5)))
	      (3 ((cx1 cx2 cx3) (cy2 cy2 cy3 cy4) (cz1 cz2 cz3 cz4 cz5))))
	    '((1 (1 2 2 2 1 1))
	      (2 (2 1 2 3 2))
	      (3 (2 3 2 2 2 3 3))))))
  (do-lookup lfl 1 211))

=> 
(AX1 BX1 BX2 BX3 AX2 AX3 BX1 AX1 BX2 CX1 BX3 BX1 AX2 BX2 CX2 BX3 BX1 AX3 BX2
     CX3 BX3 AX1 BY1 BX1 BX2 AY1 AX2 AX3 BX3 BX1 BX2 AX1 AX2 BX3 AY2 BX1 CX1
     BY2 AX3 BX2 BX3 BX1 AX1 AY3 BX2 AX2 BY3 CY2 BX3 BX1 CX2 BY4 BX2 BX3 CX3
     CY2 BY1 AY4 BX1 CX1 BY2 BX2 AY1 BY3 CY3 BY4 AX3 BX3 BY1 BX1 AY2 AX1 BY2
     AY3 BY3 CY4 BX2 BY4 CX2 BY1 BX3 BY2 CY2 CX3 BY3 AY4 BY4 CY2 BY1 BX1 AX2
     BY2 CY3 BY3 AY1 BY4 BY1 BY2 AY2 AY3 BY3 AY4 BY4 CY4 BY1 BY2 CY2 BY3 BY4
     BY1 CY2 CY3 BY2 AY1 BY3 CY4 BY4 AY2 BY1 BY2 BY3 AY3 AY4 BY4 AY1 BY1 CZ1
     BZ1 BY2 AY2 BY3 CY2 BY4 BY1 AY3 BY2 CY2 BY3 AY4 BY4 BZ2 BY1 AZ1 AY1 AY2
     BY2 BY3 BY4 AY3 AY4 AZ2 BZ3 BY1 BY2 AY1 AY2 BZ4 AZ3 BY3 CZ2 BY4 BZ5 AY3
     BY1 CY3 BZ1 BY2 AZ4 BZ2 CZ3 BZ3 AZ5 BY3 BZ4 BY4 AY4 AZ1 AY1 BZ5 BZ1 BY1
     AZ2 AZ3 BZ2 AY2 BZ3 CY4 BY2 AZ4 BZ4 BZ5 BZ1 AZ5 AZ1 BZ2 AZ2 BY3 CZ4 BZ3
     BZ4 CY2 BZ5 BZ1 BZ2 CZ5 CZ1 BZ3 AZ3 BZ4 CZ2 BZ5), 
((CX1 3) (AX3 5) (AX1 6) (BX2 11) (CX2 3) (BX3 11) (CX3 3) (BX1 12) (AX2 6)
 (AY3 7) (CY3 4) (CZ3 1) (BY4 14) (AY4 7) (AY1 8) (BY1 15) (AY2 8) (CY4 4) 
 (BY2 15) (AZ4 2) (AZ5 2) (AZ1 3) (AZ2 3) (BY3 15) (CZ4 1) (CY2 9) (BZ1 5) 
 (BZ25) (CZ5 1) (CZ1 2) (BZ3 5) (AZ3 3) (BZ4 5) (CZ2 2) (BZ5 5)), 
(1 2 2 2 1 1 2 1 2 3 2 2 1 2 3 2 2 1 2 3 2 1 2 2 2 1 1 1 2 2 2 1 1 2 1 2 3 2 1
   2 2 2 1 1 2 1 2 3 2 2 3 2 2 2 3 3 2 1 2 3 2 2 1 2 3 2 1 2 2 2 1 1 2 1 2 3 2 
   2 3 2 2 2 3 3 2 1 2 3 2 2 1 2 3 2 1 2 2 2 1 1 2 1 2 3 2 2 3 2 2 2 3 3 2 1 2
   3 2 1 2 2 2 1 1 2 1 2 3 2 2 1 2 3 2 2 1 2 3 2 1 2 2 2 1 1 1 2 2 2 1 1 1 2 2
   2 1 1 2 1 2 3 2 2 1 2 3 2 2 1 2 3 2 1 2 2 2 1 1 1 2 2 2 1 1 2 1 2 3 2 1 2 2
   2 1 1 2 1 2 3 2 2 3 2 2 2 3 3 2 1 2 3 2)
|#
;;; SYNOPSIS
(defmethod do-lookup ((lflu l-for-lookup) seed stop &optional scaler)
;;; ****
  (reset lflu)
  (get-l-sequence lflu seed stop)
  (get-group-indices lflu)
  (let* ((result
          (loop with scaler = (if scaler scaler (scaler lflu))
             with offset = (offset lflu)
             for group-ref in (l-sequence lflu) 
             for i = (get-position group-ref lflu)
             ;; issue the warning if it's not there! but it should be a list
             ;; of circular-sclists
             for group = (data (get-data group-ref lflu t))
             for seq-index = (get-next (nth i (group-indices lflu)))
             for this = (get-next (nth seq-index group))
             collect (if (numberp this)
                       (+ offset (* scaler this))
                       this)))
         (elements (remove-duplicates result))
         (lld (make-list (length elements))))
    (when (list-of-numbers-p elements)
      (setf elements (sort elements #'<)))
    (loop for e in elements and i from 0 do
         (setf (nth i lld) (list e (count e result))))
    (setf (ll-distribution lflu) lld)
    (values result lld (l-sequence lflu))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat Jan 14 15:02:42 GMT 2012: Added robodoc info

;;; ****m* l-for-lookup/reset
;;; FUNCTION
;;; Sets the counters (index pointers) of all circular-sclist objects stored
;;; within a given l-for-lookup object back to zero.
;;; 
;;; ARGUMENTS 
;;; - An l-for-lookup object.
;;;
;;; OPTIONAL ARGUMENTS
;;; - (an optional IGNORE argument for internal use only).
;;; 
;;; RETURN VALUE  
;;; Always NIL.
;;; 
;;; EXAMPLE
#|

|#
;;; SYNOPSIS
(defmethod reset ((lflu l-for-lookup) &optional ignore)
;;; ****
  (declare (ignore ignore))
  (loop for no in (data lflu) do
        (loop for cscl in (data no) do
              (reset cscl))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Stores a list of circular-sclists the elements of which are indices into
;;; the sequences of the groups.

(defmethod get-group-indices ((lflu l-for-lookup))
  (let ((ld (l-distribution lflu)))
    (unless ld
      (error "l-for-lookup::get-group-indices: Call get-l-sequence before ~
              this method!"))
    (setf (group-indices lflu)
          (loop 
           ;; after get-l-sequence is called we know how many times each group
           ;; will be called; this is stored in ld.
           for times-group-called in ld 
           for i from 0
           ;; we need to know how many sequences there are in this group
           ;; e.g. in 
           ;; ((1 ((2 3 7) (11 16 12)))
           ;;  (2 ((4 5 9) (13 14 17)))
           ;;  (3 ((1 6 8) (15 18 19))))
           ;; the groups are labelled 1,2,3 and each group contains 2
           ;; sequences which we'll transition between.
           for num-seqs = (length (data (get-nth i lflu)))
           ;; e.g. if we know there's 2 sequences and the group
           ;; will be called 17 times, then the first sequence
           ;; will be called 9 times, the second 8 times.
           ;; Calculate these repetitions here storing them in a
           ;; list. 
           for index-distribution = (items-per-transition times-group-called 
                                                          num-seqs)
           ;; now we can finally do the fibonacci
           ;; transitioning between the groups
           collect (if index-distribution
                       (get-group-indices-aux index-distribution)
                     (make-cscl (make-list 35 :initial-element 0)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat Jan 14 14:28:09 GMT 2012: Edited robodoc info

;;; ****m* l-for-lookup/get-linear-sequence
;;; FUNCTION
;;; Instead of creating L-sequences with specified rules, use the given
;;; sequences to generate a simply sequential list. 
;;;
;;; The method first returns the first element in the list whose ID matches the 
;;; SEED argument, then that element is used as the ID for the next
;;; lookup. Each time a sequence is accessed, the next element in the sequence 
;;; is returned (if there is more than one), circling to the head of the list
;;; once its end is reached.  
;;;
;;; In order for this method to function properly, no rules can have been
;;; entered for the given l-for-lookup object (that slot must be set to NIL). 
;;; 
;;; Seen very loosely, this method functions a bit like a first-order Markov
;;; chain, but without the randomness. 
;;; 
;;; ARGUMENTS 
;;; - An l-for-lookup object.
;;; - The seed, which is the starting key for the resulting sequence. This must
;;;   be the key-ID of one of the sequences. 
;;; - An integer that is the number of elements to be in the resulting list.
;;;
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to indicate whether to reset the pointers of the given circular
;;;   lists before proceeding. T = reset. Default = T. 
;;; 
;;; RETURN VALUE  
;;; A list of results of user-defined length.
;;; 
;;; EXAMPLE
#|
(let ((lfl (make-l-for-lookup 'lfl-test
			      '((1 ((2 3)))
				(2 ((3 1 2)))
				(3 ((1))))
			      NIL)))
  (get-linear-sequence lfl 1 23))

=> (1 2 3 1 3 1 2 1 3 1 2 2 3 1 3 1 2 1 3 1 2 2 3)

|#
;;; SYNOPSIS
(defmethod get-linear-sequence ((lflu l-for-lookup) seed stop-length
                                &optional (reset t))
;;; ****
  ;; 14/8/07: reset lists so that get-next starts at beginning and we generate
  ;; the same results each time method called with same data  
  (when reset
    (reset lflu))
  (loop 
      with current = seed 
      repeat stop-length
      collect current
      do
        ;; the circular lists are in a list of the data
        (setf current (get-next (first (data (get-data current lflu)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat Jan 14 12:10:43 GMT 2012: Edited robodoc info

;;; ****m* l-for-lookup/get-l-sequence
;;; FUNCTION
;;; Return an L-sequence of the key-ids for the rules of a given l-for-lookup
;;; object, created using the rules of that object. This method can be called
;;; with an l-for-lookup object that contains no sequences, as it only returns
;;; a list of the key-ids for the object's rules. 
;;;
;;; Tip: It seems that systems where one rule key yields all other keys as a
;;; result makes for evenly distributed results which are different for each
;;; seed.  
;;; 
;;; ARGUMENTS 
;;; - An l-for-lookup object.
;;; - The start seed, or axiom, that is the initial state of the L-system. This
;;;   must be the key-id of one of the sequences.
;;; - An integer that is the length of the sequence to be returned. NB: This
;;;   number does not indicate the number of L-system passes, but only the
;;;   number of elements in the list returned, which may be the first segment
;;;   of a sequence returned by a pass that actually generates a much longer
;;;   sequence. 
;;; 
;;; RETURN VALUE  
;;; A list that is the L-sequence of rule key-ids.
;;;
;;; The second value returned is a count of each of the rule keys in the
;;; sequence created, in their given order. 
;;; 
;;; EXAMPLE
#|

;; Create an l-for-lookup object with three rules and generate a new sequence
;; of 29 rule keys from those rules. The l-for-lookup object here has been
;; created with the SEQUENCES argument set to NIL, as the get-l-sequence
;; function requires no sequences. The second list returned indicates the
;; number of times each key appears in the resulting sequence (thus 1 appears 5
;; times, 2 appears 12 times etc.)
(let ((lfl (make-l-for-lookup 'lfl-test
			      NIL
			      '((1 (2))
				(2 (1 3))
				(3 (3 2))))))
  (get-l-sequence lfl 1 29))

=> (2 3 2 3 2 1 3 2 3 2 3 2 1 3 2 3 2 1 3 3 2 1 3 2 3 2 3 2 1), (5 12 12) 

;; A similar example using symbols rather than numbers as keys and data
(let ((lfl (make-l-for-lookup 'lfl-test
			      NIL
			      '((a (b))
				(b (a c))
				(c (c b))))))
  (get-l-sequence lfl 'a 19))

=> (A C C B A C C B A C B C B A C C B A C), (5 5 9)

|#
;;; SYNOPSIS
(defmethod get-l-sequence ((lflu l-for-lookup) seed stop-length)
;;; ****
  (let* ((rules (rules lflu))
         (keys (get-keys rules))
         (result '()))
    (unless (integerp stop-length)
      (warn "l-for-lookup::get-l-sequence: stop-length should be an integer ~
             but is ~a so rounding!!!" 
             stop-length)
      (setf stop-length (round stop-length)))
    (unless (> stop-length 0)
      (error "l-for-lookup::get-l-sequence: stop-length is ~a!!!" 
             stop-length))
    (unless (get-data seed rules)
      (error "l-for-lookup::get-l-sequence: seed must be in rules! : ~a" 
             seed))
    (setf result (get-l-sequence-aux rules stop-length (* stop-length 10) 0
                                     (list seed)))
    (unless result
      (error "l-for-lookup::get-l-sequence: Recursion: your rules don't ~
              yield enough results"))
    (setf result (subseq result 0 stop-length))
    (unless result 
      (error "l-for-lookup::get-l-sequence: ~a ~
              Recursion too deep!  Please check your rules: ~a"
             (id lflu) rules))
    (when keys
      (setf (l-distribution lflu) (loop for k in keys collect (count k result))
            (l-sequence lflu) result))
    (values result (l-distribution lflu))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Try to find which seeds produce identical results--not perfect as it's
;;; dependant on the stop length.
;;; Keep stop low enough to catch redundancy at the beginning of the sequence.

(defmethod check-redundant-seeds ((lflu l-for-lookup) &optional (stop 20))
  (let* ((num-rules (sclist-length (rules lflu)))
         (seeds (get-keys lflu))
         (seqs (loop for seed in seeds 
                   collect (get-l-sequence lflu seed stop))))
    (loop for seq in seqs and i from 1 do
          (loop 
              for j from i below num-rules 
              for comp = (nth j seqs)
              do
                ;; (format t "~%~a ~a" (1- i) j)
                (when (equal seq comp)
                  (warn "l-for-lookup::check-redundant-seeds ~
                         ~%In ~a: Seeds ~a and ~a produce identical ~
                         results with length ~a."
                        (id lflu) (nth (1- i) seeds) (nth j seeds) stop))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Related functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Fri Jan 13 17:09:19 GMT 2012: Add robodoc info

;;; ****f* l-for-lookup/make-l-for-lookup
;;; FUNCTION
;;; Create an l-for-lookup object. The l-for-lookup object uses techniques
;;; associated with Lindenmayer-systems (or L-systems) by storing a series of
;;; rules about how to produce new, self-referential sequences from the data of
;;; original, shorter sequences.
;;;
;;; NB: This method just stores the data concerning sequences and rules. To
;;; manipulate the data and create new sequences, see do-lookup or
;;; get-l-sequence etc.
;;; 
;;; ARGUMENTS 
;;; - A symbol that will be the object's ID.
;;; - A sequence (list) or list of sequences, that serve(s) as the initial
;;;   material, from which the new sequence is to be produced.
;;; - A production rule or list of production rules, each consisting of a
;;;   predecessor and a sucessor, defining how to expand and replace the
;;;   individual predecessor items.
;;;
;;; OPTIONAL ARGUMENTS
;;; - keyword argument :auto-check-redundancy. Default = NIL.
;;; - keyword argument :scaler. Factor by which to scale the values returned by
;;;   do-lookup. Default = 1. Does not modify the original data.
;;; - keyword argument :offest. Number to be added to values returned by
;;;   do-lookup (after they are scaled). Default = NIL. Does not modify the
;;;   original data. 
;;; 
;;; RETURN VALUE  
;;; Returns an l-for-lookup object.
;;; 
;;; EXAMPLE
#|
;; Create an l-for-lookup object based on the Lindenmayer rules (A->AB) and
;; (B->A), using the defaults for the keyword arguments
(make-l-for-lookup 'l-sys-a
		   '((1 ((a)))
		     (2 ((b))))
		   '((1 (1 2)) (2 (1))))

=>
L-FOR-LOOKUP:
[...]
              l-sequence: NIL
              l-distribution: NIL
              ll-distribution: NIL
              group-indices: NIL
              scaler: 1
              offset: 0
              auto-check-redundancy: NIL
ASSOC-LIST: warn-not-found T
CIRCULAR-SCLIST: current 0
SCLIST: sclist-length: 2, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: L-SYS-A, tag: NIL, 
data: (
[...]

;; A larger list of sequences, with keyword arguments specified
(make-l-for-lookup 'lfl-test
			      '((1 ((2 3 4) (5 6 7)))
				(2 ((3 4 5) (6 7 8)))
				(3 ((4 5 6) (7 8 9))))
			      '((1 (3)) (2 (3 1)) (3 (1 2)))
			      :scaler 1
			      :offset 0
			      :auto-check-redundancy nil)

|#
;;; SYNOPSIS
(defun make-l-for-lookup (id sequences rules &key (auto-check-redundancy nil)
                                                  (offset 0)
                                                  (scaler 1))
;;; ****
  (make-instance 'l-for-lookup :id id :data sequences :rules rules
                 :auto-check-redundancy auto-check-redundancy 
                 :offset offset
                 :scaler scaler))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-l-sequence-aux (rules stop-length max-recurse 
                           current-recurse l-values)
  ;; some rules rule out the opportunity to ever accrue stop-length results so
  ;; recursion would go on until we run out of stack space.  Avoid that.
  (cond ((> current-recurse max-recurse) nil)
        ((>= (length l-values) stop-length) l-values)
        (t (get-l-sequence-aux rules stop-length max-recurse 
                               (1+ current-recurse) 
                               (loop 
                                   for i in l-values 
                                   for tr = (data (get-data i rules))
                                   if (listp tr) append tr
                                  else collect tr)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; arg is a list like (17 17 16): do fibonacci transitions using this number
;;; of repetitions.

(defun get-group-indices-aux (group-repetitions)
  (make-cscl 
   (loop for num in group-repetitions and i from 0 
         append (fibonacci-transition num i (1+ i)))))
   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat Jan 14 16:14:01 GMT 2012: Edited robodoc info

;;; ****f* l-for-lookup/fibonacci
;;; FUNCTION
;;; Return the longest possible list of sequential Fibonacci numbers whose
;;; combined sum is less than the specified value. The list is returned in
;;; descending sequential order, ending with 0. 
;;;
;;; The function also returns the greatest possible Fibonacci number below the
;;; specified test number as a second value. 
;;; 
;;; ARGUMENTS 
;;; A number that is to be the test number.
;;; 
;;; RETURN VALUE  
;;; A list of descending sequential Fibonacci numbers, of which list the last
;;; element is 0.
;;; 
;;; Also returns the greatest possible Fibonacci number below the specified
;;; test number as a second result.
;;;
;;; EXAMPLE
#|
(fibonacci 5000)
 => (1597 987 610 377 233 144 89 55 34 21 13 8 5 3 2 1 1 0), 4181
|#
;;; 
;;; SYNOPSIS
(defun fibonacci (max-sum)
;;; ****
  (loop 
     ;; our result will be in descending order
     with result = '(1 0) 
     ;; the running total of sums
     with cumulative-sum = 1
     for x = 0 
     for y = 0 
     ;; the sum of our two most recent numbers.
     for sum = 0 
     do
     (setf x (first result)
           y (second result)
           sum (+ x y))
     (incf cumulative-sum sum)
     (when (> cumulative-sum max-sum)
       ;; we're not using sum this time as we're over our limit.
       ;; return can be used in loops to exit immediately
       (return (values result (1+ (- cumulative-sum sum)))))
     (push sum result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat Jan 14 17:11:23 GMT 2012: Added robodoc info

;;; ****f* l-for-lookup/fibonacci-start-at-2
;;; FUNCTION
;;; Return the longest possible list of sequential Fibonacci numbers, excluding 
;;; 0 and 1, whose combined sum is less than or equal to the specified
;;; value. The list is returned in descending sequential order.
;;;
;;; The function also returns as a second value the greatest possible
;;; Fibonacci number that is less than or equal to the specified test number.
;;;
;;; NB: This function differs from the plain fibonacci function in three
;;; points: a) it excludes 0 and 1, b) the combined sum may also be equal
;;; to the specified value as well as being below it, and c) the second
;;; returned value may also be equal to the test number.
;;; 
;;; ARGUMENTS 
;;; A number that is to be the test number.
;;; 
;;; RETURN VALUE  
;;; A list of descending sequential Fibonacci numbers, of which list the last
;;; element is 2.
;;; 
;;; Also returns as a second result the greatest possible Fibonacci number that
;;; is less than or equal to the specified test number.
;;;
;;; EXAMPLE

#|
;; The combined sum may also be equal to the test number.
(fibonacci-start-at-2 18)

=> (8 5 3 2), 18

|#
;;; SYNOPSIS
(defun fibonacci-start-at-2 (max-sum)
;;; ****
  (multiple-value-bind
      (series sum)
      (fibonacci (+ 2 max-sum)) ; + 2 so we can hit max-sum if need be
    ;; subseq returns a sequence out of our list
    (values (subseq series 0 (- (length series) 3))
            (- sum 3))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat Jan 14 17:27:09 GMT 2012: Edited robodoc info

;;; ****f* l-for-lookup/fibonacci-transition
;;; FUNCTION
;;; Uses Fibonacci relationships to produces a sequence that is a gradual
;;; transition from one repeating state to a second over n repetitions. The
;;; function gradually increases the frequency of the second repeating state
;;; until it completely dominates. 
;;;
;;; NB: The similar but separate function fibonacci-transition-aux1 gradually
;;; decreases state 1 and increases state 2.
;;; 
;;; ARGUMENTS 
;;; - An integer that is the desired number of elements in the resulting list
;;;   (i.e., the number of repetitions over which the transition is to occur).
;;;
;;; OPTIONAL ARGUMENTS
;;; - Repeating item 1 (starting state). This can be any Lisp type, including
;;;   lists. Default = 0.  
;;; - Repeating item 2 (target state): This can also be any Lisp type. 
;;;   Default = 1.  
;;;
;;; RETURN VALUE  
;;; A list.
;;; 
;;; EXAMPLE
#|
;; Defaults to 0 and 1 (no optional arguments)
(fibonacci-transition 31)

=> (0 0 0 0 0 0 0 1 0 0 0 0 1 0 0 1 0 1 1 1 0 1 0 1 1 0 1 1 1 1 1)

;; Using optional arguments set to numbers
(fibonacci-transition 23 11 37)

=> (11 11 11 11 37 11 11 37 11 37 11 37 11 37 37 11 37 11 37 11 37 37 37) 

;; Using lists
(fibonacci-transition 27 '(1 2 3) '(5 6 7))

=> ((1 2 3) (1 2 3) (1 2 3) (1 2 3) (5 6 7) (1 2 3) (1 2 3) (5 6 7) (1 2 3)
    (1 2 3) (5 6 7) (1 2 3) (5 6 7) (1 2 3) (5 6 7) (1 2 3) (5 6 7) (5 6 7)
    (1 2 3) (5 6 7) (5 6 7) (1 2 3) (5 6 7) (5 6 7) (5 6 7) (5 6 7) (5 6 7))

|#
;;; SYNOPSIS
(defun fibonacci-transition (num-items &optional
                                       (item1 0)
                                       (item2 1))
;;; ****
  ;; just some sanity checks
  (unless item1
    (setf item1 0))
  (unless item2
    (setf item2 1))
  ;; we use the aux1 function to first move towards more of item2, but then
  ;; again for less of item1.  The point at which this shift occurs is at the
  ;; golden section (where else?).
  (let* ((left-num (round (* num-items .618)))
         (right-num (- num-items left-num))
         ;; get the two transitions.
         (left (fibonacci-transition-aux1 left-num item1 item2))
         ;; this one will be reversed
         (right (fibonacci-transition-aux1 right-num item2 item1)))
    ;; avoid two item1s at the crossover. we use equal as it can handle number
    ;; and symbol comparison
    (when (equal (first (last right))
                 item1)
      ;; butlast returns it's argument minus the last element
      ;; e.g. (butlast '(1 2 3 4)) -> (1 2 3)
      (setf right (butlast right))
      (push item2 right))
    ;; append the two lists and return.  we can use nreverse (which is more
    ;; efficient) rather than reverse as we won't need the original version of
    ;; result
    (append left (nreverse right))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Say you want a transition between two repeating states over a period of x
;;; repetitions; this gives you a gradual break in of the second state using
;;; Fibonacci relationships.
;;; <item1> is the start item, <item2> the item we want to transition towards
;;; e.g. (fibonacci-transition-aux1 21 0 1) ->
;;; (0 0 0 0 0 0 0 1 0 0 0 0 1 0 0 1 0 1 0 1 1)   
(defun fibonacci-transition-aux1 (num-items &optional
                                  (item1 0)
                                  (item2 1))
  ;; local function: usually done with flet but you can't call flet functions
  ;; recursively...
  (labels ((ftar (num) 
             ;; lisp functions can return more than one value (e.g. (floor
             ;; 3.24) usually you will only want the first value (as in the
             ;; case of floor) but we can get them all using
             ;; multiple-value-bind and friends.
             (multiple-value-bind
                   (series sum)
                 ;; returns a list of descending fib numbers and their sum--this
                 ;; will be < num-items
                 (fibonacci-start-at-2 num)
               (let ((remainder (- num sum)))
                 (if (> remainder 2)
                     ;; recursive call: what we're looking for is a descending
                     ;; list of fib numbers that total <num-items> exactly,
                     ;; hence we have to keep doing this until we've got
                     ;; num-items
                     (append series (ftar remainder))
                     ;; we're done so just store the remainder and return
                     (progn
                       (when (> remainder 0) 
                         (push remainder series))
                       series))))))
    ;; we might have something like (2 5 3 2 8 5 3 2) so make sure we sort them
    ;; in descending order.  Note that our sort algorithm takes a function as
    ;; argument.
    (fibonacci-transition-aux2 
     (stable-sort (ftar num-items) #'>)
     item1 item2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Once we have the numbers e.g. (8 5 3 2 1) we convert into indices e.g. 
;;; (0 0 0 0 0 0 0 1 0 0 0 0 1 0 0 1 0 1 1)
;;;                8         5     3   2 1
(defun fibonacci-transition-aux2 (list item1 item2)
  (let ((result '()))
    (loop for num in list do 
       ;; so each time we have 'num' items, all but one of which are item1
         (loop repeat (1- num) do 
              (push item1 result))
         (push item2 result))
    ;; we've used push so we need to reverse the list before returning
    (nreverse result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat Jan 14 17:50:08 GMT 2012: Edited robodoc info

;;; ****f* l-for-lookup/fibonacci-transitions
;;; DATE
;;; 18 Feb 2010
;;;
;;; FUNCTION
;;; This function builds on the concept of the function fibonacci-transition by
;;; allowing multiple consecutive transitions over a specified number of
;;; repetitions. The function either produces sequences consisting of
;;; transitions from each consecutive increasing number to its upper
;;; neighbor, starting from 0 and continuing through a specified number of
;;; integers, or it can be used to produce a sequence by transitioning between
;;; each element of a user-specified list of items.
;;;
;;; ARGUMENTS 
;;; - An integer indicating the number of repetitions over which the
;;;   transitions are to be performed.
;;; - Either:
;;;   - An integer indicating the number of consecutive values, starting from
;;;     0, the function is to transition (i.e. 3 will produce a sequence that
;;;     transitions from 0 to 1, then from 1 to 2 and finally from 2 to 3), or 
;;;   - A list of items of any type (including lists) through which the
;;;     function is to transition.
;;;
;;; RETURN VALUE  
;;; A list.
;;; 
;;; EXAMPLE
#|
;; Using just an integer transitions from 0 to 1 below that integer
(fibonacci-transitions 76 4)

=> (0 0 0 0 0 0 0 1 0 0 0 0 1 0 0 1 0 1 0 1 1 0 1 0 1 1 1 1 1 1 1 2 1 1 2 1 2 1
    2 2 1 2 1 2 2 2 2 2 2 2 3 2 2 3 2 3 2 3 3 2 3 2 3 3 3 2 3 3 3 3 3 3 3 3 3 3)

;; Using a list transitions consecutively through that list
(fibonacci-transitions 152 '(1 2 3 4))

=> (1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 1 1 1 1 1 1 1 2 1 1 1 1 2 1 1 2 1 1 2 1 2 1 2
    2 1 2 1 2 2 1 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 3 2 2 2 2 3 2 2 3 2 2 3 2 3 2 3
    3 2 3 2 3 3 2 3 3 2 3 3 3 3 3 3 3 3 3 3 3 3 4 3 3 3 3 4 3 3 4 3 3 4 3 4 3 4
    4 3 4 3 4 4 3 4 4 3 4 4 4 4 4 3 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4)

;; A list of lists is also viable
(fibonacci-transitions 45 '((1 2 3) (4 5 4) (3 2 1)))

=> ((1 2 3) (1 2 3) (1 2 3) (1 2 3) (1 2 3) (4 5 4) (1 2 3) (1 2 3) (4 5 4)
    (1 2 3) (4 5 4) (1 2 3) (4 5 4) (1 2 3) (4 5 4) (1 2 3) (4 5 4) (1 2 3)
    (4 5 4) (4 5 4) (4 5 4) (4 5 4) (4 5 4) (3 2 1) (4 5 4) (3 2 1) (4 5 4)
    (3 2 1) (4 5 4) (3 2 1) (4 5 4) (3 2 1) (4 5 4) (3 2 1) (3 2 1) (3 2 1)
    (4 5 4) (3 2 1) (3 2 1) (3 2 1) (3 2 1) (3 2 1) (3 2 1) (3 2 1) (3 2 1))

|#
;;; SYNOPSIS
(defun fibonacci-transitions (total-items levels)
;;; ****
  (let ((len (typecase levels 
                      (list (length levels))
                      (integer levels)
                      (t (error "l-for-lookup::fibonacci-transitions: ~
                                 levels must be a list or an integer: ~a"
                                levels)))))
    (when (<= (floor total-items len) 2)
      (error "l-for-lookup::fibonacci-transitions: can't do ~a transitions ~
              over ~a items." len total-items))
    (if (= 1 len)
        (ml 0 total-items)
        (let* ((lop-off (floor total-items len))
               (new-len (- total-items lop-off))
               (result (fibonacci-transitions-aux new-len len))
               (add-end (floor (* .618 lop-off)))
               (add-beg (- total-items new-len add-end))
               (beg (append (ml 0 (1- add-beg)) (list 1)))
               (end (ml (1- len) (1- add-end)))
               (transition (progn
                             (push (- len 2) end)
                             (append beg result end))))
          (if (listp levels)
              (loop for el in transition collect (nth el levels))
              transition)))))

(defun fibonacci-transitions-aux (total-items levels)         
  (let ((ipt (items-per-transition total-items levels)))
    (loop for num in ipt and i from 0 appending
         (fibonacci-transition num i (1+ i)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; When we know we have 50 refs to get and 4 groups, we have to make three
;;; transitions (1->2, 2->3 and 3->4) so distribute the 50
;;; evenly but with extra at the front to make up the whole 50 e.g.
;;; (items-per-transition 50 4) => (17 17 16)

(defun items-per-transition (num-items num-groups)
  (if (= 1 num-groups)
      nil
    (let ((transitions (1- num-groups)))
      (multiple-value-bind
       (floor remainder)
       (floor num-items transitions)
       (loop repeat transitions
             with plus = 1
             do
             (if (> remainder 0)
                 (decf remainder)
               (setf plus 0))
             collect (+ plus floor))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat Jan 14 16:04:03 GMT 2012: Edited robodoc info

;;; MDE:  A (generally flat) list of numbers or symbols (or anything that EQL,
;;; which is MEMBER's default test) can match on).

;;; 15.8.10 MDE: Modified to show items in pairs. 

;;; ****f* l-for-lookup/count-elements
;;; FUNCTION
;;; Count the number of times each element occurs in a given list. 
;;; 
;;; ARGUMENTS 
;;; - A list of numbers or symbols (or anything which can be compared using
;;;   EQL). 
;;; 
;;; RETURN VALUE  
;;; Returns a sorted list of two-element lists, each consisting of one list
;;; element from the specified list and the number of times that element occurs
;;; in the list.   
;;; 
;;; EXAMPLE
#|
(count-elements '(1 4 5 7 3 4 1 5 4 8 5 7 3 2 3 6 3 4 5 4 1 4 8 5 7 3 2))

=> ((1 3) (2 2) (3 5) (4 6) (5 5) (6 1) (7 3) (8 2))

|#
;;; SYNOPSIS
(defun count-elements (list)
;;; ****
  (loop with result = '() with found = '() for e in list do
        (unless (member e found)
          (push e found)
          (push (list e (count e list)) result))
      finally (return (sort result #'(lambda (x y) (< (first x) (first y)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; EOF l-for-lookup.lsp
