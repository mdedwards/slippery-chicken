;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* palette/pitch-seq-palette
;;; NAME 
;;; pitch-seq-palette
;;;
;;; File:             pitch-seq-palette.lsp
;;;
;;; Class Hierarchy:  named-object -> linked-named-object -> sclist -> 
;;;                   circular-sclist -> assoc-list -> recursive-assoc-list ->
;;;                   palette -> pitch-seq-palette
;;;
;;; Version:          1.0.0-beta
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Implementation of the pitch-seq-palette class.
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    19th February 2001
;;;
;;; $$ Last modified: 15:01:36 Mon May 14 2012 BST
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

(defclass pitch-seq-palette (palette)
  ((num-notes :accessor num-notes :initarg :num-notes 
              :initform nil)
   ;; 24/3/07: a list of instruments that are specifically mentioned in the
   ;; pitch-seqs  
   (instruments :accessor instruments :type list :initarg :instruments 
                :initform nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; NB verify-and-store is called automatically from the sclist init and that
;;; concerns the data slot, here we initialize slots particular to this class.

(defmethod initialize-instance :after ((psp pitch-seq-palette) &rest initargs)
  (declare (ignore initargs))
  (let ((nn (num-notes psp)))
    (when nn
      (unless (and (integerp nn) (>= nn 0))
        (error "pitch-seq-palette::initialize-instance: ~
                The num-notes slot of pitch-seq-palette must be set ~
                and must also be an integer >= 0.")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object :before ((psp pitch-seq-palette) stream)
  (format stream "~%PITCH-SEQ-PALETTE: num-notes: ~a, instruments: ~a"
          (num-notes psp) (instruments psp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone ((psp pitch-seq-palette))
  (clone-with-new-class psp 'pitch-seq-palette))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone-with-new-class :around ((psp pitch-seq-palette) new-class)
  (declare (ignore new-class))
  (let ((palette (call-next-method)))
    (setf (slot-value palette 'num-notes) (num-notes psp)
          (slot-value palette 'instruments) (instruments psp))
    palette))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod verify-and-store :after ((psp pitch-seq-palette))
  ;; this will be the case if we're cloning
  (unless (typep (first (data psp)) 'pitch-seq)
    ;; at this point the data is untouched due to the clause in the assoc-list
    ;; method.  If this data is recursive, the error will be picked up
    ;; elsewhere.
    (loop for i in (data psp) and j from 0
          for ps = 
          (progn
            (when (and (listp i)
                       (or (lisp-assoc-listp i) 
                           (lisp-assoc-listp (second i))))
              (error "pitch-seq-palette::verify-and-store: ~
                      Recursive pitch-seq-palettes are not allowed: ~a"
                     i))
            (make-pitch-seq i (format nil "~a-ps-~a" 
                                      (id psp) (1+ j))))
          unless (= (num-notes psp) (sclist-length ps))
          do (error "pitch-seq-palette::verify-and-store: ~%~
                     In pitch-seq ~a from palette ~a:~%~
                     Each pitch sequence must have ~a notes (you have ~a): ~%~a"
                    (id ps) (id psp) (num-notes psp) (sclist-length ps)
                    (data psp))
          do (setf (nth j (data psp)) ps))
    ;; we call this now explicitly from the assoc-list-class because it was
    ;; avoided there. 
    (all-ids-unique psp))
  (setf (instruments psp) 
    (remove-duplicates
     (loop for ps in (data psp) appending (instruments ps))))
  (setf (num-data psp) (r-count-elements psp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Tue Jan 31 17:28:52 GMT 2012: Added robodoc info

;;; ****m* pitch-seq-palette/add-inversions
;;; FUNCTION
;;; Add inversions of the existing pitch-seq objects in a given
;;; pitch-seq-palette object to the end of that pitch-seq-palette object. (See
;;; pitch-seq::invert for more details on slippery-chicken inversions.)
;;; 
;;; ARGUMENTS
;;; - A pitch-seq-palette object.
;;; 
;;; RETURN VALUE
;;; Always returns T.
;;; 
;;; EXAMPLE
#|
;; Create a pitch-seq-palette object and print the DATA of the pitch-seq
;; objects it contains; then apply the add-inversions method and print the same
;; DATA to see the changes
(let ((mpsp (make-psp 'mpsp 5 '((2 5 3 1 4)
                                (1 4 2 5 3)
                                (5 1 3 2 4)
                                (2 3 4 5 1)
                                (3 2 4 1 5)))))
  (print (loop for ps in (data mpsp)
            collect (data ps)))
  (add-inversions mpsp)
  (print (loop for ps in (data mpsp)
            collect (data ps))))

=>
((2 5 3 1 4) (1 4 2 5 3) (5 1 3 2 4) (2 3 4 5 1) (3 2 4 1 5)) 

((2 5 3 1 4) (1 4 2 5 3) (5 1 3 2 4) (2 3 4 5 1) (3 2 4 1 5) (4 1 3 5 2)
 (5 2 4 1 3) (1 5 3 4 2) (4 3 2 1 5) (3 4 2 5 1))

|#
;;; SYNOPSIS
(defmethod add-inversions ((psp pitch-seq-palette))
;;; ****
  (loop for ps in (data psp) do
        (add (invert ps) psp))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Tue Jan 31 17:40:07 GMT 2012: Added robodoc info

;;; ****m* pitch-seq-palette/combine 
;;; FUNCTION 
;;; Create a new pitch-seq-palette object by combining the pitch-seq lists from
;;; one pitch-seq-palette object with those of another.
;;;
;;; The method combines the contents of the two given rthm-seq-palette objects
;;; consecutivelyl; i.e., the first pitch-seq object of the first
;;; pitch-seq-palette is combined with the first pitch-seq object of the other,
;;; then the second with the second, the third with the third etc. 
;;;
;;; If one pitch-seq-palette object contains more pitch-seq objects than the
;;; other, the method cycles through the shorter one until all of the members
;;; of the longer one have been handled. The new pitch-seq-palette object will
;;; therefore contain the same number of pitch-seq objects as is in the longest
;;; of the two starting pitch-seq-palette objects.
;;;
;;; It is not necessary for the lengths of the pitch-seq objects in the two
;;; starting pitch-seq-palette objects to be the same.
;;; 
;;; ARGUMENTS
;;; - A first pitch-seq-palette object.
;;; - A second pitch-seq-palette object.
;;; 
;;; RETURN VALUE
;;; A pitch-seq-palette object.
;;; 
;;; EXAMPLE
#|
;;; Combine two pitch-seq-palette objects of the same length, each of whose
;;; pitch-seqs are the same length
(let ((mpsp1 (make-psp 'mpsp1 5 '((2 5 3 1 4) (1 4 2 5 3) (5 1 3 2 4))))
      (mpsp2 (make-psp 'mpsp2 5 '((2 3 4 5 1) (3 2 4 1 5) (3 2 1 5 4)))))
  (combine mpsp1 mpsp2))

=>
PITCH-SEQ-PALETTE: num-notes: 10, instruments: NIL
PALETTE: 
RECURSIVE-ASSOC-LIST: recurse-simple-data: T
                      num-data: 3
                      linked: NIL
                      full-ref: NIL
ASSOC-LIST: warn-not-found T
CIRCULAR-SCLIST: current 0
SCLIST: sclist-length: 3, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: "MPSP1-MPSP2", tag: NIL, 
data: (
PITCH-SEQ: notes: NIL
[...]
data: (2 5 3 1 4 2 3 4 5 1)
PITCH-SEQ: notes: NIL
[...]
data: (1 4 2 5 3 3 2 4 1 5)
[...]       
PITCH-SEQ: notes: NIL
[...]
data: (5 1 3 2 4 3 2 1 5 4)
)

;; When combining pitch-seq-palette objects of different lengths, the method
;; cyles through the shorter of the two
(let ((mpsp1 (make-psp 'mpsp1 5 '((2 5 3 1 4) (1 4 2 5 3) (5 1 3 2 4))))
      (mpsp2 (make-psp 'mpsp2 5 '((2 3 4 5 1) (3 2 4 1 5)))))
  (loop for ps in (data (combine mpsp1 mpsp2)) 
       collect (data ps)))

=> ((2 5 3 1 4 2 3 4 5 1) (1 4 2 5 3 3 2 4 1 5) (5 1 3 2 4 2 3 4 5 1))

;; The two starting pitch-seq-palette objects are not required to have
;; pitch-seq objects of the same length
(let ((mpsp1 (make-psp 'mpsp1 5 '((2 5 3 1 4) (1 4 2 5 3) (5 1 3 2 4))))
      (mpsp2 (make-psp 'mpsp2 3 '((2 3 4) (3 2 4)))))
  (loop for ps in (data (combine mpsp1 mpsp2)) 
       collect (data ps)))

=> ((2 5 3 1 4 2 3 4) (1 4 2 5 3 3 2 4) (5 1 3 2 4 2 3 4))

|#
;;; SYNOPSIS
(defmethod combine ((psp1 pitch-seq-palette) (psp2 pitch-seq-palette))
;;; ****
  (let ((result (clone psp1)))
    (setf (id result) (combine-ids psp1 psp2)
          (num-notes result) (+ (num-notes psp1) (num-notes psp2)))
    (loop 
        for ps1 in (data psp1) 
        for ps2 = (get-next psp2)
        and i from 0
        do (setf (nth i (data result)) 
             (make-pitch-seq (append (data (clone ps1)) 
                                     (data (clone ps2)))
                             (id ps1))))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 19/3/07: This method looks to see if a ps was named after an
;;; instrument--this means that the string of player is searched for in the ids
;;; of the psp ("flute2-inverted" would match "flute").  If true, then we call
;;; get-next until we get a match, otherwise we just return get-next.  By
;;; implication we can have several ps's for an instrument and they should be
;;; alternated.
;;; 
;;; 24/3/07: changed this so we can specify instruments a pitch-seq applies to
;;; like this:
;;;     ((4 5 6) sdf pno vc rew)
;;; or  (psp4 ((2 4 3) pno vc cl))
;;; this implies that such a ps is __only__ for those instruments!  So a psp
;;; that specifies one of more pitch-seqs with instruments means that only such
;;; pitch-seqs will be used for those instruments and instruments not mentioned
;;; will only use pitch-seqs who have __no__ instruments.

(defmethod get-next-for-ins ((psp pitch-seq-palette) ins)
  (if (member ins (instruments psp))
      (loop for ps = (get-next psp) do
            (when (member ins (instruments ps))
              ;; (format t "~&~a (member): ~a" ins (id ps))
              (return ps)))
    (loop for ps = (get-next psp) do
          (unless (instruments ps)
            ;; (format t "~&~a: ~a" ins (id ps))
            (return ps)))))

#|    
(defmethod get-next-for-ins ((psp pitch-seq-palette) ins)
  (let* ((ids (get-all-refs psp))
         (instr (string ins))
         (instrlen (length instr))
         (doit (loop 
                   for id in ids
                             ;; all ids are returned as a string, even if
                             ;; there's no recursion
                   for idstr = (string (first id))
                   do
                     (when (and (>= (length idstr) instrlen)
                                (string= instr idstr :end2 instrlen))
                       (return t)))))
    (if doit
        (loop 
            for ps = (get-next psp)
            for idstr = (string (id ps))
            for match = (and (>= (length idstr) instrlen)
                             (string= instr idstr :end2 instrlen))
            do
              (when match
                ;; (format t "~&get-next-for-ins: using ~a" (id ps))
                (return ps)))
      (get-next psp))))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod psp-subseq ((psp pitch-seq-palette) start end)
  (let* ((num-notes (- end start))
         (new-psp (make-psp (format nil "~a-post-psp-subseq"
                                    (id psp))
                            num-notes nil)))
    (loop 
        for ps in (data psp) 
        for new-ps = (ps-subseq ps start end)
        do
          (unless (= num-notes (sclist-length new-ps))
            (error "pitch-seq-palette::psp-subseq: ~
                    num-notes: ~a != ~a: ~a ~&start ~a, end ~a"
                   num-notes (sclist-length new-ps) new-ps start end))
          (add new-ps new-psp))
    (setf (instruments new-psp) (copy-list (instruments psp)))
    new-psp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Related functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Tue Jan 31 14:36:09 GMT 2012: Added robodoc info

;;; ****f* pitch-seq-palette/make-psp
;;; FUNCTION
;;; Create a pitch-seq-palette object from an ID, a specified number of notes,
;;; and a list of lists of numbers representing the pitch curve of the intended
;;; pitch-seq objects.
;;; 
;;; ARGUMENTS
;;; - A symbol that is to be the ID of the pitch-seq-palette to be created. 
;;; - An integer that is the number of notes there are to be in each pitch-seq
;;;   object created.
;;; - A list of lists, each of which contained list is a list of numbers
;;;   representing the pitch curve of the intended pitch-seq object.
;;; 
;;; RETURN VALUE
;;; A pitch-seq-palette object.
;;; 
;;; EXAMPLE
#|
;; Returns a pitch-seq-palette object
(make-psp 'mpsp 5 '((2 5 3 1 4)
                    (1 4 2 5 3)
                    (5 1 3 2 4)
                    (2 3 4 5 1)
                    (3 2 4 1 5)))

=> 
PITCH-SEQ-PALETTE: num-notes: 5, instruments: NIL
PALETTE: 
RECURSIVE-ASSOC-LIST: recurse-simple-data: T
                      num-data: 5
                      linked: NIL
                      full-ref: NIL
ASSOC-LIST: warn-not-found T
CIRCULAR-SCLIST: current 0
SCLIST: sclist-length: 5, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: MPSP, tag: NIL, 
data: (
PITCH-SEQ: notes: NIL
[...]
data: (2 5 3 1 4)
PITCH-SEQ: notes: NIL
[...]
data: (1 4 2 5 3)
PITCH-SEQ: notes: NIL
[...]
data: (5 1 3 2 4)
PITCH-SEQ: notes: NIL
[...]
data: (2 3 4 5 1)
PITCH-SEQ: notes: NIL
[...]
data: (3 2 4 1 5)
)

;; Interrupts with an error if any of the <pitch-seqs> lists is not of the
;; length specified by <num-notes>
(make-psp 'mpsp 5 '((1 2 1 1 3)
                    (1 3 2 1 5)
                    (1 3 5 6 7 8)))

=>
pitch-seq-palette::verify-and-store: 
In pitch-seq MPSP-ps-3 from palette MPSP:
Each pitch sequence must have 5 notes (you have 6): 
[...]
 (1 3 5 6 7 8))
   [Condition of type SIMPLE-ERROR]

|#
;;; SYNOPSIS
(defun make-psp (id num-notes pitch-seqs)
;;; ****
  ;; let's try and catch errors here before sclist complains...
  (loop for i in pitch-seqs unless (listp i)
     do (error "pitch-seq-palette::make-psp: ~
                The argument to make-psp should be a list of lists: ~%~a"
               pitch-seqs))
  (make-instance 'pitch-seq-palette :id id :data pitch-seqs
                 :num-notes num-notes))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((seqs '((1 (((3)) ((3)) ((1)) ((25))))
              (2 ((3 4) (5 2) ((25) 25) (1 25)))
              (3 (((3) 4 (3)) (5 (9) 6) (1 2 4) (5 2 (2)) (6 2 3)))
              (4 ((3 4 3 4) (5 3 6 4) ((9) 4 5 (11)) (2 (10) 4 8)))
              (5 (((5) (5) 6 (5) 8) ((7) (7) (7) 4 8) (11 8 4 10 2) 
                  ((7) (7) 4 (9) (9))))
              (6 ((4 (5) (5) 3 (6) (6)) ((3) 8 (3) 9 (3) 8) (9 3 9 5 10 6)))
              (7 (((8) (8) (8) 5 (9) 6 (9)) (9 3 8 4 7 5 4)
                  ((3) 4 (3) 5 (3) 4 (3))))
              (8 (((3) (3) 4 (3) (3) 1 5 4) (10 3 9 3 8 3 7 4)
                  (3 5 8 2 8 9 4 11)))
              (9 ((3 6 4 (7) 4 (7) 3 6 (7)) (10 (2) 9 (2) 8 (2) 7 (2) 3) 
                  (2 (9) 3 (9) 4 (9) (9) 6 (11))))
              (10 ((9 9 9 (3) 9 9 (3) 5 9 5) (8 9 8 9 5 9 9 5 6 6)))
              (12 ((1 2 5 5 5 5 5 5 5 5 4 5) (2 1 5 1 5 1 6 5 1 5 2 5)))
              (13 ((1 2 5 5 5 5 5 5 5 5 4 5 2) (2 1 5 1 5 1 6 5 1 5 2 5 1)))
              (14 ((1 2 5 5 5 5 5 5 5 5 4 5 2 1) 
                   (2 1 5 1 5 1 6 5 1 5 2 5 1 2)))
              (15 ((1 2 5 5 5 5 5 5 5 5 4 5 2 1 2) 
                   (2 1 5 1 5 1 6 5 1 5 2 5 1 2 6)))))
      (seqs-al nil))

  ;; SAR Tue Jan 31 18:09:46 GMT 2012: Edited robodoc info

  ;; ****f* rthm-seq-palette/create-psps-default
  ;; FUNCTION
  ;; Create pitch-sequences for the create-psps method. This is the callback
  ;; function that is passed by default. If data isn't provided for a sequence
  ;; of a certain length, a (recursive!) attempt will be made to make one up
  ;; from two sequences of lesser length.
  ;;
  ;; This (and the above lists) was first used in the piece "I Kill by Proxy".
  ;; 
  ;; ARGUMENTS 
  ;; - An integer that is the number of notes for which a pitch-seq-palette
  ;;   object is needed.
  ;; - the pitch-seq data (see documentation for create psps method).  Ideally
  ;;   this would only be passed the first time the function is called.
  ;; 
  ;; RETURN VALUE  
  ;; A list of numbers suitable for use in creating a pitch-seq object.
  ;; 
  ;; SYNOPSIS
  (defun create-psps-default (num-notes data-lists)
    ;; ****
    ;; 3.2.11 need to reinitialize our cscls the first time we call
    ;; create-psps, otherwise the only way to get the same piece each time is
    ;; to reload all sc source files i.e. restart lisp
    (unless (or num-notes data-lists)
      (setf seqs-al nil))
    (when (or data-lists (not seqs-al))
      (let ((dl (if (and data-lists (listp data-lists))
                    data-lists
                    ;; e.g. if data-lists is t, we'll use the default data again
                    seqs)))
        ;; make an assoc-list of circular sc-lists out of the sequences.
        (setf dl
              (loop
                 for l in dl
                 for order = (first l)
                 for pss = (second l)
                 do
                 (loop for ps in pss do
                    ;; check we've the right number of notes.
                      (unless (= order (length ps))
                        (error "kill-get-ps: need ~a elements, got ~a: ~a"
                               order (length ps) ps)))
                 collect
                 (list order (make-cscl pss)))
              seqs-al (make-assoc-list 'create-psps-default dl))))
    (when (and (numberp num-notes) (> num-notes 0))
      (let ((ps (get-data num-notes seqs-al nil)))
        (if ps
            (get-next (data ps))
            (progn
              ;; must avoid infinite recursions....
              (when (= 1 num-notes)
                (error "rthm-seq-palette::create-psps-default: no data for 1!~
                    Avoiding infinite recursion."))
              (let* ((left (floor num-notes 2))
                     (right left))
                (when (oddp num-notes)
                  (incf left))
                (append (create-psps-default left nil) 
                        (create-psps-default right nil)))))))))
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Fri Dec 30 18:45:55 2011 -- used in rthm-seq::add-bar

(defun get-psps-as-list (num-notes num-pss)
  ;; reset our lists
  (create-psps-default nil nil)
  (loop repeat num-pss collect (create-psps-default num-notes nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; EOF pitch-seq-palette.lsp

