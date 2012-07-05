;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* sc-map/set-map
;;; NAME 
;;; set-map
;;;
;;; File:             set-map.lsp
;;;
;;; Class Hierarchy:  named-object -> linked-named-object -> sclist -> 
;;;                   circular-sclist -> assoc-list -> recursive-assoc-list ->
;;;                   sc-map -> set-map
;;;
;;; Version:          1.0.0-beta3
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Implementation of the set-map class for mapping sets for
;;;                   a piece.   
;;; 
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    March 11th 2010
;;;
;;; $$ Last modified: 20:10:09 Fri Mar 19 2010 GMT
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

;;; 02.12.11 SEAN: changed robodoc header to reflect class hierarchy

(eval-when (compile)
  (declaim (optimize (speed 3) (safety 1) (space 0) (debug 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :slippery-chicken)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; no new slots
(defclass set-map (sc-map) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sun Apr 29 18:21:09 BST 2012: Added robodoc entry

;;; ****m* set-map/gen-midi-chord-seq
;;; DESCRIPTION
;;; Write a midi file with each set in the set-map played as a chord at 1
;;; second intervals.
;;; 
;;; ARGUMENTS 
;;; - A set-map object.
;;; - The path+file-name for the midi file to be written.
;;;
;;; RETURN VALUE  
;;; Returns T.
;;;
;;; EXAMPLE
#|
(let ((mini
       (make-slippery-chicken
	'+mini+
	:ensemble '(((vn (violin :midi-channel 1))))
	:set-palette '((1 ((c3 e3 g3 a3 c4 d4 g4 a4 b4 e5))))
	:set-map '((1 (1 1 1)))
	:rthm-seq-palette '((1 ((((2 4) q e s s))
				:pitch-seq-palette ((1 2 3 4)))))
	:rthm-seq-map '((1 ((vn (1 1 1))))))))
  (gen-midi-chord-seq (set-map mini) "/tmp/mchsq.mid"))

=> T

|#
;;; 
;;; SYNOPSIS
(defmethod gen-midi-chord-seq ((sm set-map) midi-file)
;;; ****
  (reset sm)
  (let* ((sets (get-all-data-from-palette sm))
         (events (loop with rthm = (make-rhythm 'q)
                    for start-time from 0
                    for s in sets
                    collect (create-event s rthm start-time))))
    (cm::process-voices (list (list events))
                        midi-file (make-tempo 60) nil 0))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object :before ((sm set-map) stream)
  (format stream "~%SET-MAP: "))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone ((sm set-map))
  (clone-with-new-class sm 'set-map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone-with-new-class :around ((sm set-map) new-class)
  (declare (ignore new-class))
  (call-next-method))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF sc-map.lsp

