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
;;; Version:          1.0.12
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
;;; $$ Last modified:  15:12:08 Sat Mar 20 2021 CET
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

(in-package :slippery-chicken)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; no new slots
(defclass set-map (sc-map) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* set-map/midi-play
;;; DESCRIPTION
;;; Write a MIDI file containing the sets in order of the set-map.
;;; 
;;; ARGUMENTS
;;; - the set-map object
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :midi-file. The path (string) of the midi file to write. Default is to
;;;   use the ID of the set-palette and write it into the default directory.
;;; - :tempo. The tempo of the MIDI file in beats per minute. Default = 60.
;;; - :auto-open. Whether to open the midi file after generating (not available
;;;   on all operating systems). Default is (get-sc-config
;;;   'midi-play-auto-open) which itself defaults to sc-auto-open in the
;;;   *features* list.
;;; - :related-sets-id :subsets-id. If you want to generate a MIDI file of just
;;;   a named subset or related-set, pass the ID here. NB this ID will have to
;;;   exist for all sets.
;;; 
;;; RETURN VALUE
;;; T
;;; 
;;; SYNOPSIS
(defmethod midi-play ((sm set-map)
                      &key
                        ;; MDE Sat Mar 20 12:05:17 2021, Heidhausen
                        related-sets-id subsets-id
                        (tempo 60.0)
                        (auto-open (get-sc-config 'midi-play-auto-open))
                        (midi-file
                         (format nil "~a~a.mid"
                                 (get-sc-config 'default-dir)
                                 (string-downcase (string (id sm))))))
;;; ****
  (gen-midi-chord-seq sm midi-file tempo subsets-id related-sets-id)
  (when auto-open
    (system-open-file midi-file))
  midi-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* set-map/gen-midi-chord-seq
;;; DESCRIPTION
;;; Write a midi file with each set in the set-map played as a chord at 1
;;; second intervals.
;;; 
;;; ARGUMENTS 
;;; - A set-map object.
;;; - The path+file-name for the midi file to be written.
;;;
;;; OPTIONAL ARGUMENTS
;;; - the tempo in beats per minute. Default = 60.
;;; - subsets-id, related-sets-id. If you want to generate a MIDI file of just
;;;   a named subset or related-set, pass the ID here. NB this ID will have to
;;;   exist for all sets.
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
(defmethod gen-midi-chord-seq ((sm set-map) midi-file
                               &optional (tempo 60.0) subsets-id
                                 related-sets-id)
;;; ****
  (reset sm)
  (let* ((sets (get-all-data-from-palette sm))
         (events (loop with rthm = (make-rhythm 'q)
                    for start-time from 0
                    for s in sets
                    collect (create-event s rthm start-time start-time
                                          subsets-id related-sets-id))))
    (cm::process-voices (list (list events))
                        midi-file (make-tempo tempo) nil 0))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Sat Jan 30 13:07:48 2016 -- can't believe we didn't have this method
;;; already 
;;; ****m* set-map/cmn-display
;;; DATE
;;; January 30th 2016
;;; 
;;; DESCRIPTION
;;; Generate musical notation of the set-map object using CMN. 
;;; 
;;; ARGUMENTS
;;; a set-map object
;;; 
;;; OPTIONAL ARGUMENTS
;;; see the set-palette class for a description of the keyword arguments.
;;; 
;;; RETURN VALUE
;;; T
;;; 
;;; SYNOPSIS
(defmethod cmn-display ((sm set-map) &rest keyargs &key &allow-other-keys)
;;; ****
  (unless (palette sm)
    (error "set-map::cmn-display: the palette slot must be set. ~%Use the ~
            bind-palette method before calling this method."))
  (apply #'cmn-display-sets (cons (get-all-data-from-palette sm)
                                  (add-file-keyword sm keyargs))))
    
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
;;;
;;; Related functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Sat Jan 30 10:10:30 2016 -- never need this explicitly as it's always
;;; been implicit to make-slippery-chicken.
(defun make-set-map (id map &key replacements)
  (clone-with-new-class
   (make-sc-map id map :replacements replacements :recurse-simple-data nil)
   'set-map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF sc-map.lsp

