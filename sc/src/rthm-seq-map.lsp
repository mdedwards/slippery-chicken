;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* sc-map/rthm-seq-map
;;; NAME 
;;; rthm-seq-map
;;;
;;; File:             rthm-seq-map.lsp
;;;
;;; Class Hierarchy:  named-object -> linked-named-object -> sclist -> 
;;;                   circular-sclist -> assoc-list -> recursive-assoc-list ->
;;;                   sc-map -> rthm-seq-map
;;;
;;; Version:          1.0.0-beta2
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Implementation of the rthm-seq-map class which maps
;;;                   references to rthm-seq objects for the players in the
;;;                   piece.  Extensions to the sc-map superclass are the
;;;                   collection of all the players in the piece and a
;;;                   check to make sure that each list each instrument has the
;;;                   same number of rthm-seq references for each section. 
;;;
;;;                   Instances of this class must declare sections and
;;;                   players so if the piece is in one section, give it
;;;                   the label 1 or whatever, e.g.
;;;
;;;                   '((1
;;;                      ((vln (2 20 1 9 10 22 16 25 6 14 21 17 4 9 13 2))
;;;                       (vla (2 23 3 7 13 22 19 3 8 12 23 14 2 10 15 4))
;;;                       (vc (2 21 3 12 11 22 16 1 8 17 23 20 24 9 12 2)))))
;;;
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    July 28th 2001
;;;
;;; $$ Last modified: 21:51:55 Tue May  8 2012 BST
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

(defclass rthm-seq-map (sc-map)
  ((num-players :accessor num-players :initform nil)
   ;; 15.2.10 allow players to be specified rather than just auto-generated so
   ;; that the rthm-chain subclass can specify it.
   (players :accessor players :type list :initarg :players :initform nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize-instance :after ((rsm rthm-seq-map) &rest initargs)
  (declare (ignore initargs))
  (when (data rsm)
    (setf (players rsm) (get-rsm-players rsm)
          (num-players rsm) (length (players rsm)))
    (check-num-sequences rsm)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
(defmethod print-object :before ((rsm rthm-seq-map) stream)
  (format stream "~%RTHM-SEQ-MAP: num-players: ~a ~
                  ~%              players: ~a"
          (num-players rsm) (players rsm)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone ((rsm rthm-seq-map))
  (clone-with-new-class rsm 'rthm-seq-map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone-with-new-class :around ((rsm rthm-seq-map) new-class)
  (declare (ignore new-class))
  (let ((map (call-next-method)))
    (setf (slot-value map 'num-players) (num-players rsm)
          (slot-value map 'players) (players rsm))
    map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod generate-pitch-sequence-map ((rsm rthm-seq-map) sc)
  (make-sc-map (format nil "~a-DERIVED-PITCH-SEQ-MAP" (id rsm))
               (generate-pitch-sequence-map-aux rsm nil sc)
               :warn-not-found nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod (setf palette) :after (value (rsm rthm-seq-map))
  (declare (ignore value))
  (check-rthm-seq-durations rsm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 15.2.10 add this method so we can add the data (map) after init (mainly for
;;; the rthm-seq-chain class 
(defmethod (setf data) :after (value (rsm rthm-seq-map))
  (declare (ignore value))
  (setf (players rsm) (get-rsm-players rsm)
        (num-players rsm) (length (players rsm)))
  (check-num-sequences rsm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* rthm-seq-map/get-map-refs
;;; DATE
;;; 29-Dec-2010
;;;
;;; DESCRIPTION
;;; Return the list of rthm-seq-palette references for the given player and
;;; section.
;;; 
;;; ARGUMENTS 
;;; - A rthm-seq-map object.
;;; - The ID of the section in which the references are sought.
;;; - The ID of the player for whom the references are sought.
;;; 
;;; RETURN VALUE  
;;; A list of references (each of which might also  be a list).
;;; 
;;; EXAMPLE
#|
(let ((rsmt (make-rthm-seq-map 
             'rsm-test-5
             '((sec1 ((vn (rs1 rs3 rs2))
                      (va (rs2 rs3 rs1))
                      (vc (rs3 rs1 rs2))))
               (sec2 ((vn (rs1 rs2 rs1))
                      (va (rs2 rs1 rs3))
                      (vc (rs1 rs3 rs3))))
               (sec3 ((vn (rs1 rs1 rs3))
                      (va (rs1 rs3 rs2))
                      (vc (rs3 rs2 rs3)))))
             :palette (make-rsp 
                       'rs-pal
                       '((rs1 ((((2 4) q e s s))))
                         (rs2 ((((2 4) e s s q))))
                         (rs3 ((((2 4) s s q e)))))))))
  (get-map-refs rsmt 'sec3 'vc))

=> (RS3 RS2 RS3)

|#
;;; 
;;; SYNOPSIS
(defmethod get-map-refs ((rsm rthm-seq-map) section player)
;;; ****
  (get-data-data (make-set-map-ref section player) rsm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Fri Apr 27 13:39:58 BST 2012: Added robodoc entry

;;; ****m* rthm-seq-map/set-map-refs
;;; DATE
;;; 30-Dec-2010
;;;
;;; DESCRIPTION
;;; Change the reference IDs of the specified rthm-seq objects in the given
;;; rthm-seq-map object.
;;; 
;;; ARGUMENTS 
;;; - A rthm-seq-map object.
;;; - The ID of the section in which references are to be set.
;;; - The ID of the player for whom the references are to be set.
;;; - A list of the new rthm-seq IDs (references) 
;;; 
;;; RETURN VALUE  
;;; Returns the modified named object whose ID is the specified player.
;;; 
;;; EXAMPLE
#|
(let ((rsmt (make-rthm-seq-map 
             'rsm-test-5
             '((sec1 ((vn (rs1 rs3 rs2))
                      (va (rs2 rs3 rs1))
                      (vc (rs3 rs1 rs2))))
               (sec2 ((vn (rs1 rs2 rs1))
                      (va (rs2 rs1 rs3))
                      (vc (rs1 rs3 rs3))))
               (sec3 ((vn (rs1 rs1 rs3))
                      (va (rs1 rs3 rs2))
                      (vc (rs3 rs2 rs3)))))
             :palette (make-rsp 
                       'rs-pal
                       '((rs1 ((((2 4) q e s s))))
                         (rs2 ((((2 4) e s s q))))
                         (rs3 ((((2 4) s s q e)))))))))
  (set-map-refs rsmt 'sec2 'vc '(rs2 rs3 rs2)))

=> 
NAMED-OBJECT: id: VC, tag: NIL, 
data: (RS2 RS3 RS2)

|#
;;; 
;;; SYNOPSIS
(defmethod set-map-refs ((rsm rthm-seq-map) section player new-refs)
;;; ****
  (set-data (make-set-map-ref section player) 
            (list player new-refs)
            rsm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Fri Apr 27 13:49:01 BST 2012: Conformed robodoc entry
;;; SAR Wed Jun 13 12:22:11 BST 2012: Revisited robodoc entry

;;; ****m* rthm-seq-map/add-repeats
;;; DATE 
;;; 30-Dec-2010
;;; 
;;; DESCRIPTION
;;; Generate repeating sequences at given cycle points using recurring-event
;;; data. This process modifies the number of beats.
;;; 
;;; ARGUMENTS 
;;; - A rthm-seq-map object.
;;; - A list of two-item lists of integers that determine the cycle
;;;   pattern. This list will have the format of the recurring-event class's
;;;   DATA slot (see recurring-event.lsp).
;;; - A list of two-item lists of integers that determine the number of repeats
;;;   made (or references into the :repeats list). This list is also processed
;;;   cyclically (i.e. the recurring-event class's RETURN-DATA-CYCLE slot).
;;;
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :section. The section map reference. Default = 1.
;;; - :repeats-indices. A list of the number of repeat bars returned by the
;;;    cycle data (i.e. recurring-event class's RETURN-DATA slot). Generally
;;;    this will remain NIL and the number of repeats will be expressed
;;;    directly in the third argument, but it could be useful to use references
;;;    into this list there instead, since the recurring-event class already
;;;    makes this possible. Default = NIL.
;;; - :start. An integer that is the number of the bar/rthm-seq where the
;;;   process is to begin. Default = 1.
;;; - :end. An integer that is the number of the bar/rthm-seq where the process
;;;   is to end. NIL = process all bars/rthm-seqs. Default = NIL.
;;; - :print. T or NIL to indicate whether to print the rthm-seq ID and the
;;;   number repetitions to the listener. T = print. Default = NIL.
;;; 
;;; RETURN VALUE  
;;; An integer that is the number of bars added.
;;; 
;;; EXAMPLE
#|
;;; Straightforward usage, additionally printing the DATA slot before and after
;;; applying the method
(let ((mrsm
       (make-rthm-seq-map 
	'rsm-test
	'((1 ((vn (1 2 3 2 1 3 1 3 2 3 1 2 1 3 1 3 2 1)))))
	:palette (make-rsp 
		  'rs-pal
		  '((1 ((((2 4) q e s s))))
		    (2 ((((2 4) e s s q))))
		    (3 ((((2 4) s s q e)))))))))
  (print (get-data-data '(1 vn) mrsm))
  (add-repeats mrsm '((1 6) (2 6)) '((11 6) (23 3)))
  (print (get-data-data '(1 vn) mrsm)))

=>
(1 2 3 2 1 3 1 3 2 3 1 2 1 3 1 3 2 1) 
(1 2 2 2 2 2 2 2 2 2 2 2 3 3 3 3 3 3 3 3 3 3 3 2 2 2 2 2 2 2 2 2 2 2 1 1 1
 1 1 1 1 1 1 1 1 3 3 3 3 3 3 3 3 3 3 3 1 1 1 1 1 1 1 1 1 1 1 3 2 2 2 2 2 2
 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
 1 1 1 1 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 3 1 1 1 1 1 1 1 1
 1 1 1 3 2 2 2 2 2 2 2 2 2 2 2 1)

;;; Using the :start, :end, and :print arguments
(let ((mrsm
       (make-rthm-seq-map 
	'rsm-test
	'((1 ((vn (1 2 3 2 1 3 1 3 2 3 1 2 1 3 1 3 2 1)))))
	:palette (make-rsp 
		  'rs-pal
		  '((1 ((((2 4) q e s s))))
		    (2 ((((2 4) e s s q))))
		    (3 ((((2 4) s s q e)))))))))
  (print (get-data-data '(1 vn) mrsm))
  (add-repeats mrsm '((1 6) (2 6)) '((11 6) (23 3))
	       :start 3
	       :end 11
	       :print t)
  (print (get-data-data '(1 vn) mrsm)))

=>
(1 2 3 2 1 3 1 3 2 3 1 2 1 3 1 3 2 1) 
2 x 11
1 x 11
3 x 11
1 x 11
3 x 11
2 x 11
1 x 23
(1 2 3 2 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 1 3 3 3 3 3 3 3 3 3 3 3 1
 1 1 1 1 1 1 1 1 1 1 3 3 3 3 3 3 3 3 3 3 3 2 2 2 2 2 2 2 2 2 2 2 3 1 1 1 1
 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 1 3 1 3 2 1) 

|#
;;;
;;; SYNOPSIS
(defmethod add-repeats ((rsm rthm-seq-map) repeat-every repeats &key
                        (section 1) repeats-indices (start 1) end print)
;;; ****
  (let ((seqs-added 0))
    (loop for player in (players rsm) do
         (setf seqs-added 0)
         (let* ((re (make-re repeat-every :return-data-cycle repeats
                             :return-data repeats-indices))
                (new-refs '())
                (repeats 1)
                (refs (get-map-refs rsm section player))
                (nd (if end end (length refs))))
           (loop for ref in refs and i from 1 do
                (setf repeats nil)
                (when (and (>= i start) (<= i nd))
                  (setf repeats (get-it re)))
                (if repeats
                    ;; 1- because we've already got the ref once and we won't
                    ;; repeat it <repeats> times rather have it <repeats> times
                    ;; total
                    (progn
                      (when print
                        (format t "~&~a x ~a" ref repeats))
                      (incf seqs-added (1- repeats)))
                    (setf repeats 1))
                (loop repeat repeats do (push ref new-refs)))
           (set-map-refs rsm section player (nreverse new-refs))))
    ;; MDE Tue May  8 21:03:20 2012 -- only inc when it's a rthm-chain
    (when (rthm-chain-p rsm)
      (incf (num-rthm-seqs rsm) seqs-added))
    (relink-named-objects rsm)
    seqs-added))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; simply repeat rthm-seqs without a repeat-every structure.
;;; start-seq is 1-based

;;; ****m* rthm-seq-map/add-repeats-simple
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
(defmethod add-repeats-simple ((rsm rthm-seq-map) start-seq repeats &key
                               (section 1) print)
;;; ****
  (loop for player in (players rsm) do
       (let* ((refs (get-map-refs rsm section player))
              (repeat-seq (nth (1- start-seq) refs))
              (new-refs (splice (ml repeat-seq repeats) refs start-seq)))
         (when print 
           (format t "~&~a: repeating ~a:~%~a" player repeat-seq new-refs))
         (set-map-refs rsm section player new-refs)))
  ;; MDE Tue May  8 21:03:20 2012 -- only inc when it's a rthm-chain
  (when (rthm-chain-p rsm)
    (incf (num-rthm-seqs rsm) repeats))
  (relink-named-objects rsm)
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Wed Jun 13 13:21:50 BST 2012: Added robodoc entry

;;; ****m* rthm-seq-map/get-time-sig-ral
;;; DESCRIPTION
;;; Collate the IDs of all rthm-seq objects in a given rthm-seq-map into groups
;;; based on identical time signatures and return this as a
;;; recursive-assoc-list object that has the same structure of the map. 
;;;
;;; Instead of having the list of rthm-seq references for each
;;; section/instrument, the method returns an assoc-list whose keys are the
;;; time-sig tags, with the corresponding data being circular-sclists of
;;; rthm-seq IDs.  
;;;
;;; The result is a recursive-assoc-list for each instrument/section that can
;;; be queried to find the rthm-seq refs of all rthm-seqs that share the same
;;; bar/time-sig structure; e.g., all those that have a 2/4 bar followed by a
;;; 3/4 bar etc.
;;; 
;;; ARGUMENTS
;;; - A rthm-seq-map object.
;;; - A rthm-seq-palette object.
;;; 
;;; RETURN VALUE
;;; A recursive-assoc-list object.
;;; 
;;; EXAMPLE
#|
(let* ((mini
	(make-slippery-chicken
	 '+mini+
	 :ensemble '(((sax (alto-sax :midi-channel 1))))
	 :set-palette '((1 ((c2 d2 g2 a2 e3 fs3 b3 cs4 fs4 gs4 ds5 f5 bf5)))) 
	 :set-map '((1 (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)))
	 :rthm-seq-palette '((1 ((((4 4) h q e s s) ((2 4) h))))
			     (2 ((((4 4) h q e s s))))
			     (3 ((((4 4) h q e s s))))
			     (4 ((((4 4) h q q) ((2 4) q q))))
			     (5 ((((4 4) h q e s s))))
			     (6 ((((4 4) h q q) ((2 4) q q)))))
	 :rthm-seq-map '((1 ((sax (1 2 3 5 2 4 6 2 3 1 3 2 3 2 1 3 2)))))))
       (tsral (get-time-sig-ral (rthm-seq-map mini) (rthm-seq-palette mini))))
  (get-data-data 1 tsral))

=> 
RECURSIVE-ASSOC-LIST: recurse-simple-data: T
                      num-data: 1
                      linked: T
                      full-ref: (1)
ASSOC-LIST: warn-not-found T
CIRCULAR-SCLIST: current 0
SCLIST: sclist-length: 1, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: "sub-ral-of-+MINI+-RTHM-SEQ-MAP", tag: NIL, 
data: (
ASSOC-LIST: warn-not-found T
CIRCULAR-SCLIST: current 0
SCLIST: sclist-length: 2, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: (1 SAX), next: NIL
NAMED-OBJECT: id: SAX, tag: NIL, 
data: (
CIRCULAR-SCLIST: current 0
SCLIST: sclist-length: 3, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: "0404-0204", tag: NIL, 
data: (4 6 1)
**************

       
CIRCULAR-SCLIST: current 0
SCLIST: sclist-length: 3, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: "0404", tag: NIL, 
data: (5 3 2)
**************
)
**************
)
**************




|#
;;; SYNOPSIS
(defmethod get-time-sig-ral ((rsm rthm-seq-map) (rsp rthm-seq-palette))
;;; ****
       ;; section/player refs e.g. ((1 PERC1) (1 PERC2))
  (let* ((refs (get-all-refs rsm)) 
         (ral (duplicate-structure rsm)))
    (loop for ref in refs 
       ;; get the rthm-seq ids from the map for this section/player
       for seqids = (get-data-data ref rsm) 
       ;; create an empty assoc list to hold the rthm-seq ids that all share
       ;; the same time-sig structure
       for al = (make-assoc-list (first (last ref)) nil)
       do
       ;; loop through each rthm-seq id, get the tag that indicates the
       ;; time-sig structure (e.g. "0204-0304" for 2/4 3/4) and add the
       ;; rthm-seq ids associated with that tag (i.e. with the same time-sig
       ;; structure) to the assoc-list
       (loop for seqid in seqids 
          for seq = (get-data seqid rsp)
          for rstag = (get-time-sigs-tag seq)
          do
          ;; add to the assoc list, putting this rthm-seq id at the end of
          ;; the list if this time-sig structure has already been seen, or
          ;; creating a new entry automatically if it hasn't
          (add-to-list-data-force seqid rstag al) ;; (id seq) rstag al)
          finally 
          ;; add the assoc-list to the overall ral that's an empty copy of
          ;; the rthm-seq map
          (set-data ref al ral)
          ;; now create a circular-sclist from the simple lisp list that
          ;; we've created holding the rthm-seq ids
          (loop for seq-type in (data al) and i from 0 do
               (setf (nth i (data al)) 
                     (make-cscl (remove-duplicates (data seq-type)
                                                   :test #'equal)
                                :id (id seq-type))))))
    ral))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Related functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Thu Apr 26 17:20:57 BST 2012: Added robodoc entry

;;; ****f* rthm-seq-map/make-rthm-seq-map
;;; DESCRIPTION
;;; Make a rthm-seq-map object.
;;; 
;;; ARGUMENTS
;;; - The ID of the rthm-seq-map object to be made.
;;; - A list of nested lists, generally taking the form 
;;;   '((section1 ((player1 (rthm-seq ids))
;;;                (player2 (rthm-seq ids))
;;;                (etc... (etc...))))
;;;     (section2 ((player1 (rthm-seq ids))
;;;                (player2 (rthm-seq ids))
;;;                (etc...)))
;;;     (etc...))
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :palette. A palette object or NIL. If a palette object is specified or
;;;   defined here, it will be automatically bound to the given rthm-seq-map
;;;   object. Default = NIL.
;;; - :warn-not-found. T or NIL to indicate whether a warning is printed when
;;;   an index which doesn't exist is used for lookup.  
;;;   T = warn. Default = NIL.
;;; - :replacements. A list of lists in the format 
;;;   '(((1 2 vla) 3 20b) ((2 3 vln) 4 16a)) that indicate changes to
;;;   individual elements of lists within the given rthm-seq-map object. (Often
;;;   rthm-seq-map data is generated algorithmically but individual elements of
;;;   the lists need to be changed.)  Each such list indicates a change, the
;;;   first element of the list being the reference into the rthm-seq-map (the
;;;   vla player of section 1, subsection 2 in the first example here), the
;;;   second element is the nth of the data list for this key to change, and
;;;   the third is the new data. Default = NIL.
;;; - :recurse-simple-data. T or NIL to indicate whether to recursively
;;;   instantiate a recursive-assoc-list in place of data that appears to be a
;;;   simple assoc-list (i.e. a 2-element list). If NIL, the data of 2-element
;;;   lists whose second element is a number or a symbol will be ignored,
;;;   therefore remaining as a list. For example, this data would normally
;;;   result in a recursive call: (y ((2 23) (7 28) (18 2))).  
;;;   T = recurse. Default = T.
;;; 
;;; RETURN VALUE
;;; A rthm-seq-map object.
;;; 
;;; EXAMPLE
#|
;;; Straightforward usage
(make-rthm-seq-map 'rsm-test
                    '((1 ((vn (1 2 3 4))
                          (va (2 3 4 1))
                          (vc (3 4 1 2))))
                      (2 ((vn (4 5 6))
                          (va (5 6 4))
                          (vc (6 4 5))))
                      (3 ((vn (7 8 9 1 2))
                          (va (8 9 1 2 7))
                          (vc (9 1 2 7 8))))))

=>

RTHM-SEQ-MAP: num-players: 3 
              players: (VA VC VN)
SC-MAP: palette id: NIL
RECURSIVE-ASSOC-LIST: recurse-simple-data: T
                      num-data: 9
                      linked: NIL
                      full-ref: NIL
ASSOC-LIST: warn-not-found NIL
CIRCULAR-SCLIST: current 0
SCLIST: sclist-length: 3, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: RSM-TEST, tag: NIL, 
data: (
[...]

;;; An example using the :replacements argument and binding directly to a
;;; specified rthm-seq-palette object.
(make-rthm-seq-map 'rsm-test
                   '((1 ((vn (1 2 3 4))
                         (va (2 3 4 1))
                         (vc (3 4 1 2))))
                     (2 ((vn (4 5 6))
                         (va (5 6 4))
                         (vc (6 4 5))))
                     (3 ((vn (7 8 9 1 2))
                         (va (8 9 1 2 7))
                         (vc (9 1 2 7 8)))))
                   :palette (make-rsp 
                             'rs-pal
                             '((rs1 ((((2 4) q e s s))))
                               (rs2 ((((2 4) e s s q))))
                               (rs3 ((((2 4) s s q e))))))
                   :replacements '(((1 vn) 2 7)
                                   ((2 va) 1 1)
                                   ((3 vc) 1 0)))

=>
RTHM-SEQ-MAP: num-players: 3 
              players: (VA VC VN)
SC-MAP: palette id: RS-PAL
RECURSIVE-ASSOC-LIST: recurse-simple-data: T
                      num-data: 9
                      linked: NIL
                      full-ref: NIL
ASSOC-LIST: warn-not-found NIL
CIRCULAR-SCLIST: current 0
SCLIST: sclist-length: 3, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: RSM-TEST, tag: NIL, 
data: (
[...]

|#
;;; SYNOPSIS
(defun make-rthm-seq-map (id rsm &key (palette nil) (warn-not-found nil)
                                      (replacements nil)
                                      (recurse-simple-data t))
;;; ****
  (make-instance 'rthm-seq-map :data rsm :id id :warn-not-found warn-not-found
                 :replacements replacements
                 :recurse-simple-data recurse-simple-data
                 :palette palette))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; These next three functions can't be class methods because they call
;;; themselves recursively with ral arguments that form part of the rsm.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The result of a get-data call is a named-object with the id the given id
;;; for the section.  The data slot is a recursive-assoc-list (ral).  When
;;; there are no subsecions, the data slot of this ral is a list of
;;; named-obects, id the instrument, data the list of rthm-seq-palette
;;; references.  When the section contains subsections, then the data slot of
;;; the ral is also a list of named-objects but each of whose data slots are
;;; further rals whose data slots are the named-objects for each instrument.
;;; i.e.
;;;
;;; no subsections: 
;;;   (listp (data (data (get-data 3 rsm)))) -> t
;;;   (typep (first (data (data (get-data 3 rsm)))) 'named-object) -> t
;;;   (typep (data (first (data (data (get-data 3 rsm))))) 
;;;          'recursive-assoc-list) -> nil
;;; subsections: 
;;;   (listp (data (data (get-data 4 rsm))))
;;;   (typep (first (data (data (get-data 4 rsm)))) 'named-object) -> t
;;;   (typep (data (first (data (data (get-data 4 rsm))))) 
;;;          'recursive-assoc-list) -> t

(defun get-rsm-players (rsm)
  (let ((players '()))
    (loop for i below (sclist-length rsm) do
          (let* ((section (get-next rsm))
                 (players-or-subsections (data (data section))))
            (if (is-ral (data (first players-or-subsections)))
                (setf players
                  (append players (get-rsm-players (data section))))
              (loop for no in players-or-subsections do
                    (push (id no) players)))))
    (reset rsm)
    (sort-symbol-list (remove-duplicates (nreverse players)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; palette has already been set.... 
;;; Each rthm-seq given to each instrument should have the same duration as the
;;; other players in this 'vertical' position.  Check this here. 

(defun check-rthm-seq-durations (rsm &optional palette)
  (let ((rthm-seqs '())
        (num-ins 0)
        (num-seqs 0)
        (current-rs nil)
        (duration nil))
    (unless palette ;; first time: get it from the rsm: it must be there!!!
      (setf palette (palette rsm)))
    (loop for section-num below (sclist-length rsm) do
         (let* ((section (get-next rsm))
                (players-or-subsections (data (data section)))) ;; list!
           (if (is-ral (data (first players-or-subsections)))
               (check-rthm-seq-durations (data section) palette)
               (progn 
                 (setf rthm-seqs 
                       (loop for ins in players-or-subsections collect
                            (loop 
                               for ref in (data ins) 
                               for thing = (get-data ref palette nil) do
                               (when ref
                                 ;; see if this is a legal reference into the 
                                 ;; rthm-seq-palette
                                 (unless thing 
                                   (error 
                                    "rthm-seq-map::check-rthm-seq-durations ~%~
                                    Illegal map ref for palette ~a: ~a"
                                    (id palette) ref)))
                               collect thing))
                       num-ins (length rthm-seqs)
                       num-seqs (length (first rthm-seqs)))
                 (loop for i below num-seqs do
                      (loop for j below num-ins do
                           (setf current-rs (nth i (nth j rthm-seqs)))
                           (when current-rs
                             (if duration
                                 (unless (equal-within-tolerance
                                          duration (duration current-rs))
                                   (error 
                                    "rthm-seq-map::check-rthm-seq-durations~%~
                                      Rthm-seqs must have the same ~
                                      duration!~%section: ~a, ~
                                      sequence number: ~a (= ~a, should be ~a)~
                                      ~%(current-rs id = ~a)"
                                    (id section)
                                    (1+ i) (duration current-rs) duration
                                    (id current-rs)))
                                 (setf duration (duration current-rs)))))
                      (setf duration nil)))))))
  t)
                            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun generate-pitch-sequence-map-aux (rsm &optional palette sc)
  (when (and (not palette) (typep rsm 'rthm-seq-map))
    (setf palette (palette rsm)))
  (unless palette
    (error "rthm-seq-map::generate-pitch-sequence-map: The palette slot ~
            is not set for rthm-seq-map with id ~a" (id rsm)))
  (unless rsm
    (error "rthm-seq-map::generate-pitch-sequence-map: rsm is nil!"))
  (reset rsm)
  (reset-psps palette)
  (let (section players-or-subsections alist ref)
    (loop for section-num below (sclist-length rsm) do
          (setf section (get-next rsm)
                players-or-subsections (data (data section)))
        if (is-ral (data (first players-or-subsections)))
        collect (list (id section)
                      (generate-pitch-sequence-map-aux (data section) palette
                                                       sc))
        else 
        do 
        (let* ((num-sequences (length 
                               (data
                                (first players-or-subsections))))
               (num-ins (length players-or-subsections))
               (psp nil))
          (setf alist (make-list num-ins))
          (loop for i below num-sequences do
                (loop 
                    with ins
                    for player in players-or-subsections
                    for j from 0 do
                      ;; make the empty assoc-lists storing the player id
                      (setf ins (get-current-instrument-for-player
                                 (full-ref (data section)) (id player)
                                 (1+ i) sc))
                      ;; (format t "~&player=~a ins=~a" (id player) (id ins))
                      (when (zerop i)
                        (setf (nth j alist)
                          (list (id player) (make-list num-sequences))))
                      (setf ref (nth i (data player))
                            psp (when ref
                                  (let ((psp (pitch-seq-palette
                                              (get-data ref palette))))
                                    (when psp 
                                      ;; 19/3/07: we now see if any ps's were
                                      ;; given from specific instruments--just
                                      ;; using the instrument's id as the 
                                      ;; beginning of an id for a ps is enough
                                      ;; to trigger
                                      (get-next-for-ins psp (id ins)))))
                            (nth i (second (nth j alist))) (when psp
                                                             (clone psp))))))
        and collect (list (id section) alist))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Fri Apr 27 14:11:10 BST 2012: Added robodoc entry

;;; MDE original comment (some taken directly into robodoc)
;;; In an rsm, each instrument receives references into the rhythm-seq-palette.

;;; ****f* rthm-seq-map/check-num-sequences
;;; DESCRIPTION
;;; Check to ensure that each player in each section of the given rthm-seq-map
;;; object has the same number of references as every other instrument. If not,
;;; drop into the debugger with an error.
;;;
;;; NB: This function is called automatically every time make-rthm-seq-map is
;;;     called so it shouldn't generally be necessary for the user to call this
;;;     method.  However, if the rthm-seq-map is changed somehow, it might be a
;;;     good idea to recheck.
;;; 
;;; ARGUMENTS
;;; - A rthm-seq-map object.
;;; 
;;; RETURN VALUE
;;; Returns T if all players have the same number of references in each
;;; section, otherwise drops into the debugger with an error.
;;;
;;; EXAMPLE
#|
;;; Passes the test:
(let ((rsmt (make-rthm-seq-map 
             'rsm-test
             '((sec1 ((vn (rs1a rs3a rs2a))
                      (va (rs1b rs3b rs2b))
                      (vc (rs1a rs3b rs2a))))
               (sec2 ((vn (rs1a rs2a rs1a))
                      (va (rs1a rs2a rs1b))
                      (vc (rs1a rs2b rs1a))))
               (sec3 ((vn (rs1a rs1a rs3a))
                      (va (rs1a rs1a rs3b))
                      (vc (rs1a rs1b rs3a))))
               (sec4 ((vn (rs1a rs1a rs1a))
                      (va (rs1a rs1a rs1b))
                      (vc (rs1a rs1b rs1a))))))))
  (check-num-sequences rsmt))

=> T

;;; Doesn't pass the test; drops into debugger with an error.
(let ((rsmt (make-rthm-seq-map 
             'rsm-test
             '((sec1 ((vn (rs1a rs3a rs2a))
                      (va (rs1b rs3b))
                      (vc (rs1a rs3b rs2a))))
               (sec2 ((vn (rs1a))
                      (va (rs1a rs2a rs1b))
                      (vc (rs1a rs2b rs1a))))
               (sec3 ((vn (rs1a rs3a))
                      (va (rs1a))
                      (vc (rs1a rs1b rs3a))))
               (sec4 ((vn (rs1a))
                      (va (rs1a rs1a rs1b))
                      (vc (rs1a rs1a))))))))
  (check-num-sequences rsmt))

=>
rthm-seq-map::check-num-sequences: In rthm-seq-map RSM-TEST-5, instrument VA: 
Each instrument must have the same number of sequences for any given section: 
(RS1B RS3B)
   [Condition of type SIMPLE-ERROR]

|#
;;; SYNOPSIS
(defun check-num-sequences (rsm)
;;; **** 
  (loop for i below (sclist-length rsm) do
       (let* ((section (get-next rsm))
              (players-or-subsections (data (data section))))
         (if (is-ral (data (first players-or-subsections)))
             (check-num-sequences (data section))
             (loop for num-sequences = 
                  (length (data (first players-or-subsections)))
                  for no in (rest players-or-subsections) do
                  (unless (= num-sequences (length (data no)))
                    (error "rthm-seq-map::check-num-sequences: ~
                            In rthm-seq-map ~a, instrument ~a: ~
                            ~%Each instrument must have the same number of ~
                            sequences for any given section: ~%~a"
                           (id rsm) (id no) (data no)))))))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rthm-seq-map-p (thing)
  (typep thing 'rthm-seq-map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Fri Apr 27 16:10:06 BST 2012: Conformed robodoc entry

;;; ****f* rthm-seq-map/rsm-count-notes
;;; DESCRIPTION
;;; Returns the number of notes in the given rthm-seq-map object for the
;;; specified player and rthm-seq-palette.
;;; 
;;; ARGUMENTS 
;;; - A rthm-seq-map object.
;;; - The ID of the player whose notes are to be counted.
;;; - The rthm-seq-palette object whose rthm-seq object IDs are referred to by
;;;   the given rthm-seq-map object.
;;;
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to indicate whether to count just the number of notes that need
;;;   new events (i.e., not counting tied notes; also not counting chords,
;;;   since chords need only one event) or the total number of notes in that
;;;   player's part in the score. T = count just attacked notes. Default = T. 
;;; 
;;; RETURN VALUE  
;;; Returns an integer that is the number of notes counted.
;;; 
;;; EXAMPLE
#|
(let ((rsmt (make-rthm-seq-map 
             'rsm-test
             '((sec1 ((vn (rs1 rs3 rs2))
                      (va (rs2 rs3 rs1))
                      (vc (rs3 rs1 rs2))))
               (sec2 ((vn (rs1 rs2 rs1))
                      (va (rs2 rs1 rs3))
                      (vc (rs1 rs3 rs3))))
               (sec3 ((vn (rs1 rs1 rs3))
                      (va (rs1 rs3 rs2))
                      (vc (rs3 rs2 rs3)))))))
      (rspt (make-rsp 
             'rs-pal
             '((rs1 ((((2 4) q (e) s s))))
               (rs2 ((((2 4) e +s (s) q))))
               (rs3 ((((2 4) (s) s +q e))))))))
  (print (rsm-count-notes rsmt 'vn rspt))
  (print (rsm-count-notes rsmt 'va rspt nil)))

=> 
23 
27

|#
;;; 
;;; SYNOPSIS
(defun rsm-count-notes (rthm-seq-map player palette &optional (just-attacks t))
;;; ****
  (let ((num 0))
    (loop for i below (sclist-length rthm-seq-map) do
         (let* ((section (get-next rthm-seq-map))
                (players-or-subsections (data (data section))))
           (if (is-ral (data (first players-or-subsections)))
               (incf num (rsm-count-notes (data section) player palette
                                          just-attacks))  
               (loop for ref in (get-data-data player (data section))
                  for rs = (get-data ref palette)
                  do
                    (unless rs
                      (error "rthm-seq-map::num-notes: no rthm-seq with ~
                            reference ~a for ~a in palette ~a" 
                             ref player (id palette)))
                    (incf num (if just-attacks
                                  (num-notes rs)
                                  (num-score-notes rs)))))))
    num))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-set-map-ref (section player)
  (if (listp section) 
      (append section player)
      (list section player)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; EOF rthm-seq-map.lsp

