;;; 02.12.11 SEAN: changed robodoc header to reflect hierarchy
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
;;; Version:          1.0
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
;;; $$ Last modified: 21:23:42 Thu Dec  8 2011 ICT
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
;;; FUNCTION
;;; get-map-refs:
;;;
;;; Return the list of rthm-seq-palette references for the given player and
;;; section.
;;; 
;;; ARGUMENTS:
;;; - the rthm-seq-map object
;;; - the section (list or symbol)
;;; - the player (symbol)
;;; 
;;; RETURN VALUE: 
;;; a list of references, each of which could itself be a list
;;; 
;;; DATE 29.12.10
;;; 
;;; SYNOPSIS
(defmethod get-map-refs ((rsm rthm-seq-map) section player)
;;; ****
  (get-data-data (make-set-map-ref section player) rsm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* rthm-seq-map/set-map-refs
;;; FUNCTION
;;; set-map-refs:
;;;
;;; Change the rthm-seq references in the rthm-seq-map.
;;; 
;;; ARGUMENTS:
;;; - the rthm-seq-map object
;;; - the section reference (symbol or list)
;;; - the player reference (symbol)
;;; - the list of new reference ids
;;; 
;;; RETURN VALUE: 
;;; - the named object whose id is the player and data is the new list of
;;; references. 
;;; 
;;; EXAMPLE
;;; (set-map-refs +coming-rthm-chain-main+ 1 'perc2 '(1 2 3 4)) -->
;;; NAMED-OBJECT: id: PERC2, tag: NIL, 
;;; data: (1 2 3 4)
;;; 
;;; DATE 30.12.10
;;; 
;;; SYNOPSIS
(defmethod set-map-refs ((rsm rthm-seq-map) section player new-refs)
;;; ****
  (set-data (make-set-map-ref section player) 
            (list player new-refs)
            rsm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* rthm-seq-map/add-repeats
;;; FUNCTION
;;; add-repeats:
;;;
;;; Using recurring-event data, generate repeating sequences at given cycle
;;; points.  Modifies num-beats.
;;; 
;;; ARGUMENTS:
;;; - the rthm-seq-map instance
;;; - the cycle data (i.e. recurring-event class's data slot--see
;;;   rthm-chain.lsp)  
;;; - the number of repeats made (or references into :repeats list), also in
;;;   cycle (i.e. recurring-event class's return-data-cycle slot) 
;;; - (key :section default 1): the section map reference
;;; - (key :repeats-indices default nil): a list of the number of repeat bars
;;;    returned by the cycle data (i.e. recurring-event class's return-data
;;;    slot).  Generally this will remain nil and we'll express the number of
;;;    repeats directly in the third argument, but it could be useful to use
;;;    references into this list there instead, as the recurring-event class
;;;    already makes possible.
;;; - (key :start default 1): what bar/rthm-seq to start at
;;; - (key :end default nil = don't stop): what bar/rthm-seq to end at
;;; - (key :print default nil): print the rthm-seq id and number of times
;;; -  repeated 
;;; 
;;; RETURN VALUE: 
;;; the number of bars added
;;; 
;;; EXAMPLE
;;;                                        ;; when to repeat   how many repeats
;;; (add-repeats +coming-rthm-chain-main+ '((3 2) (4 3)) '((3 3) (4 1)))
;;;  --> 146
;;; 
;;; DATE 30.12.10
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
    (incf (num-rthm-seqs rsm) seqs-added)
    (relink-named-objects rsm)
    seqs-added))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; simply repeat rthm-seqs without a repeat-every structure.
;;; start-seq is 1-based
(defmethod add-repeats-simple ((rsm rthm-seq-map) start-seq repeats &key
                               (section 1) print)
  (loop for player in (players rsm) do
       (let* ((refs (get-map-refs rsm section player))
              (repeat-seq (nth (1- start-seq) refs))
              (new-refs (splice (ml repeat-seq repeats) refs start-seq)))
         (when print 
           (format t "~&~a: repeating ~a:~%~a" player repeat-seq new-refs))
         (set-map-refs rsm section player new-refs)))
  (incf (num-rthm-seqs rsm) repeats)
  (relink-named-objects rsm)
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; create a recursive association list that mirrors the structure of the map,
;;; but instead of having the list of rthm-seq references for each
;;; section/instrument we have an assoc-list, the keys of which are the
;;; time-sig tags, with the corresponding data being a circular-sclist of
;;; rthm-seq ids.  So what we end up with, for each instrument/section, is a
;;; ral we can query to find rthm-seq refs of all rthm-seqs that share the same
;;; bar/time-sig structure, e.g. all those that have a 2/4 bar followed by a
;;; 3/4 bar.
(defmethod get-time-sig-ral ((rsm rthm-seq-map) (rsp rthm-seq-palette))
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

(defun make-rthm-seq-map (id rsm &key (palette nil) (warn-not-found nil)
                                      (replacements nil)
                                      (recurse-simple-data t))
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

;;; In an rsm, each instrument receives references into the rhythm-seq-palette.
;;; Each instrument in a section must have the same number of references as
;;; every other instrument: check this here.

(defun check-num-sequences (rsm)
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

;;; ****f* rthm-seq-map/rsm-count-notes
;;; FUNCTION
;;; rsm-count-notes:
;;;
;;; Returns the number of notes in the rthm-seq-map for the given player and
;;; palette. 
;;; 
;;; ARGUMENTS:
;;; - the rthm-seq-map object
;;; - the player (symbol)
;;; - the palette the references in the map refer to
;;; - (optional just-attacks default t): whether to count just the number of
;;; notes that need new events or the number of notes in the score. NB a chord
;;; counts as one note only.
;;; 
;;; RETURN VALUE: 
;;; the number of notes (integer)
;;; 
;;; EXAMPLE
;;; (rsm-count-notes +altogether-rthm-chain-intro+ 
;;;                  'pno-rh 
;;;                  (palette +altogether-rthm-chain-intro+))
;;; -> 1087
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

