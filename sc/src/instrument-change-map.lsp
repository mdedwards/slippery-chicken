;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* change-map/instrument-change-map
;;; NAME 
;;; instrument-change-map
;;;
;;; File:             instrument-change-map
;;;
;;; Class Hierarchy:  named-object -> linked-named-object -> sclist -> 
;;;                   circular-sclist -> assoc-list -> recursive-assoc-list ->
;;;                   sc-map -> change-map -> instrument-change-map
;;;
;;; Version:          1.0.0-beta1
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Extends change-map to check that instruments defined in
;;;                   the map have data for the first bar of the first section.
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    12th April 2002
;;;
;;; $$ Last modified: 18:12:52 Tue Apr  3 2012 BST
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

(defclass instrument-change-map (change-map)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object :before ((icm instrument-change-map) stream)
  (format stream "~%INSTRUMENT-CHANGE-MAP: "))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone ((icm instrument-change-map))
  (clone-with-new-class icm 'instrument-change-map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod instrument-for-first-bar ((icm instrument-change-map)
                                     player first-section-ref)
  (unless (cm-get-data icm (econs first-section-ref player))
    (error "instrument-change-map::instrument-for-first-bar: ~
            ~a doubles but the instrument for the first bar has not been set!"
           player)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Fri May  4 15:02:50 BST 2012: Added robodoc entry

;;; ****m* instrument-change-map/get-first-for-player
;;; DESCRIPTION
;;; Return the first instrument object assigned to a given player in cases
;;; where a player has been assigned more than one instrument.
;;; 
;;; ARGUMENTS
;;; - An instrument-change-map
;;; - The ID of the player for whom to return the first instrument.
;;; 
;;; RETURN VALUE
;;; The ID of the first instrument object assigned to the specified player. 
;;; 
;;; EXAMPLE
#|
(let ((mini
       (make-slippery-chicken
	'+mini+
	:ensemble '(((sax ((alto-sax tenor-sax) :midi-channel 1))
		     (db (double-bass :midi-channel 2))))
	:instrument-change-map '((1 ((sax ((1 alto-sax) (3 tenor-sax))))))
	:set-palette '((1 ((c2 d2 g2 a2 e3 fs3 b3 cs4 fs4 gs4 ds5 f5 bf5)))) 
	:set-map '((1 (1 1 1 1 1)))
	:rthm-seq-palette '((1 ((((4 4) h q e s s))
				:pitch-seq-palette ((1 2 3 4 5)))))
	:rthm-seq-map '((1 ((sax (1 1 1 1 1))
			    (db (1 1 1 1 1))))))))
  (get-first-for-player (instrument-change-map mini) 'sax))

=> ALTO-SAX, T

|#
;;; SYNOPSIS
(defmethod get-first-for-player ((icm instrument-change-map)
                                 player)
;;; ****
  (when (data icm)
    (get-change-data (get-data player (data (first (data icm))))
                     1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Related functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Fri May  4 16:55:45 BST 2012: Added robodoc entry

;;; ****f* instrument-change-map/make-instrument-change-map
;;; DESCRIPTION
;;; Create an instrument-change-map object. 
;;; 
;;; ARGUMENTS
;;; - An ID for the instrument-change-map to be created.
;;; - A list of lists. The top level of these lists consists of the section IDs
;;;   for the given slippery-chicken object paired with lists of data for the
;;;   specified players for each section. Each player list then consists of the
;;;   ID of the player paired with a list of two-item lists pairing measure
;;;   numbers with the instrument to which that player is to change, e.g.:
;;;
;;;   '((1 ((fl ((1 flute) (3 piccolo) (5 flute)))
;;;         (cl ((1 b-flat-clarinet) (2 bass-clarinet) (6 b-flat-clarinet)))))
;;;     (2 ((fl ((2 piccolo) (4 flute)))
;;;         (cl ((2 bass-clarinet) (3 b-flat-clarinet))))))
;;; 
;;; OPTIONAL ARGUMENTS
;;; - :warn-not-found. T or NIL to indicate whether a warning is printed when
;;;   an index which doesn't exist is used for lookup. T = warn. Default = T.
;;; 
;;; RETURN VALUE
;;; 
;;; 
;;; EXAMPLE
#|
(make-instrument-change-map 
 'icm-test
 '((1 ((fl ((1 flute) (3 piccolo) (5 flute)))
       (cl ((1 b-flat-clarinet) (2 bass-clarinet) (6 b-flat-clarinet))))) 
   (2 ((fl ((2 piccolo) (4 flute)))
       (cl ((2 bass-clarinet) (3 b-flat-clarinet)))))))

=>

INSTRUMENT-CHANGE-MAP: 
CHANGE-MAP: last-ref-required: T
SC-MAP: palette id: NIL
RECURSIVE-ASSOC-LIST: recurse-simple-data: NIL
                      num-data: 4
                      linked: T
                      full-ref: NIL
ASSOC-LIST: warn-not-found NIL
CIRCULAR-SCLIST: current 0
SCLIST: sclist-length: 2, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: ICM-TEST, tag: NIL, 
data: (
NAMED-OBJECT: id: 1, tag: NIL, 
data: 
CHANGE-MAP: last-ref-required: T
SC-MAP: palette id: NIL
RECURSIVE-ASSOC-LIST: recurse-simple-data: NIL
                      num-data: 2
                      linked: T
                      full-ref: (1)
ASSOC-LIST: warn-not-found NIL
CIRCULAR-SCLIST: current 0
SCLIST: sclist-length: 2, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: "sub-ral-of-ICM-TEST", tag: NIL, 
data: (
CHANGE-DATA: 
            previous-data: NIL, 
            last-data: FLUTE
SCLIST: sclist-length: 3, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: (1 FL), next: (1 CL)
NAMED-OBJECT: id: FL, tag: NIL, 
data: ((1 1 FLUTE) (3 1 PICCOLO) (5 1 FLUTE))
**************

       
CHANGE-DATA: 
            previous-data: NIL, 
            last-data: B-FLAT-CLARINET
SCLIST: sclist-length: 3, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: (1 FL), this: (1 CL), next: (2 FL)
NAMED-OBJECT: id: CL, tag: NIL, 
data: ((1 1 B-FLAT-CLARINET) (2 1 BASS-CLARINET) (6 1 B-FLAT-CLARINET))
**************
)
**************

**************

       
NAMED-OBJECT: id: 2, tag: NIL, 
data: 
CHANGE-MAP: last-ref-required: T
SC-MAP: palette id: NIL
RECURSIVE-ASSOC-LIST: recurse-simple-data: NIL
                      num-data: 2
                      linked: T
                      full-ref: (2)
ASSOC-LIST: warn-not-found NIL
CIRCULAR-SCLIST: current 0
SCLIST: sclist-length: 2, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: "sub-ral-of-ICM-TEST", tag: NIL, 
data: (
CHANGE-DATA: 
            previous-data: FLUTE, 
            last-data: FLUTE
SCLIST: sclist-length: 2, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: (1 CL), this: (2 FL), next: (2 CL)
NAMED-OBJECT: id: FL, tag: NIL, 
data: ((2 1 PICCOLO) (4 1 FLUTE))
**************

       
CHANGE-DATA: 
            previous-data: B-FLAT-CLARINET, 
            last-data: B-FLAT-CLARINET
SCLIST: sclist-length: 2, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: (2 FL), this: (2 CL), next: NIL
NAMED-OBJECT: id: CL, tag: NIL, 
data: ((2 1 BASS-CLARINET) (3 1 B-FLAT-CLARINET))
**************
)
**************

**************
)

|#
;;; SYNOPSIS
(defun make-instrument-change-map (id icm &key (warn-not-found nil))
;;; ****
  (make-instance 'instrument-change-map :data icm :id id 
                 :last-ref-required t
                 :warn-not-found warn-not-found
                 :recurse-simple-data nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun instrument-change-map-p (thing)
  (typep thing 'instrument-change-map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; EOF instrument-change-map.lsp
