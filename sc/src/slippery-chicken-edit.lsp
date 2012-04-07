;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* sc/slippery-chicken
;;; NAME 
;;; slippery-chicken-edit
;;; 
;;; File:             slippery-chicken-edit.lsp
;;;
;;; Class Hierarchy:  named-object -> slippery-chicken
;;;
;;; Version:          0.9.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Post-generation editing methods for the slippery-chicken
;;;                   class.  
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    April 7th 2012
;;;
;;; $$ Last modified: 11:47:15 Sat Apr  7 2012 BST
;;;
;;; SVN ID: $Id: slippery-chicken-edit.lsp 1367 2012-04-06 22:15:32Z medward2 $ 
;;;
;;; ****
;;; Licence:          Copyright (c) 2010 Michael Edwards
;;;
;;;                   This file is part of slippery-chicken
;;;
;;;                   slippery-chicken is free software; you can redistribute
;;;                   it and/or modify it under the terms of the GNU General
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

(in-package :slippery-chicken)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken/replace-tempo-map
;;; FUNCTION
;;; A post-generation editing method: Calls not only the setf method (which
;;; converts bar references like (section-number sequence-number bar-number) to
;;; numbers, and makes a tempo-map object) but updates all events to reflect
;;; new start times etc.
;;;
;;; ARGUMENTS
;;; - a slippery-chicken object
;;; - the new tempo-map (as a list)
;;; 
;;; RETURN VALUE
;;; T
;;; 
;;; EXAMPLE
#|
(let ((mini
       (make-slippery-chicken
        '+mini+
        :title "mini"
        :instrument-palette +slippery-chicken-standard-instrument-palette+
        :ensemble '(((pno (piano :midi-channel 1))))
        :tempo-map '((1 (q 60)))
        :set-palette '((1 ((c4 d4 f4 g4 a4 c5 d5 f5 g5 a5 c6)))
                       (2 ((cs4 ds4 fs4 gs4 as4 cs5 ds5 fs5 gs5 as5
                                cs6))))
        :set-map (list (list 1 (fibonacci-transition 17 1 2)))
        :rthm-seq-palette '((1 ((((2 4) q q))
                                :pitch-seq-palette ((1 (2))))))
        :rthm-seq-map '((1 ((pno (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
                                    1))))))))
  (replace-tempo-map mini '((1 (q 60 "Andante")) ((1 3 1) (e 80)))))

=>
T

|#
;;; SYNOPSIS
(defmethod replace-tempo-map ((sc slippery-chicken) tm)
;;; ****
  (setf (tempo-map sc) tm)
  (update-events-tempo sc)
  (update-slots sc (tempo-map sc))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken/add-event-to-bar
;;; FUNCTION
;;; A post-generation editing method: Add an event object to a bar either at
;;; the end of at the given position.  
;;; 
;;; ARGUMENTS 
;;; - the slippery-chicken object
;;; - the new event object
;;; - the bar number or reference (of the form '(section sequence bar) where
;;;   sequence and bar are numbers counting from 1)
;;; - the player (symbol)
;;; - (key :position default nil): the position in the bar (0-based) where the
;;; event should be spliced; if nil then it's put at the end.
;;; 
;;; RETURN VALUE  
;;; T
;;; 
;;; SYNOPSIS
(defmethod add-event-to-bar ((sc slippery-chicken) event bar-num-or-ref player
                             &key (position nil))
;;; ****
  (let ((bar (get-bar sc bar-num-or-ref player)))
    (add-event bar event :position position))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken/replace-events
;;; FUNCTION
;;; replace-events:
;;; 
;;; A post-generation editing method: Replace events already in the parts with
;;; new events.  All references are 1-based.  Works for one bar at a time only.
;;; 
;;; ARGUMENTS 
;;; - the slippery-chicken object
;;; - the player (symbol)
;;; - the bar number; can also be a reference like 
;;;   '(section sequence-no. bar-no.)
;;; - the event number in the bar to start at
;;; - the number of events to replace
;;; - a list of the new events
;;; - (optional default nil): whether to automatically beam the new events.
;;; - (optional default nil): tuplet bracket info e.g. '(3 0 5) which means a
;;;   triplet bracket starting at event  0 and ending at event 5 (inclusive and
;;;   counting rests). 
;;; 
;;; RETURN VALUE  
;;; T (from piece class method)
;;; 
;;; SYNOPSIS
(defmethod replace-events ((sc slippery-chicken) player bar-num start-event
                           replace-num-events new-events
                           &optional (auto-beam nil) tuplet-brackets)
;;; ****
  ;; 14.3.11 if we're passing note data in lists, rather than events, init them
  ;; here  
  (unless (event-p (first new-events))
    (let ((player (get-data player (ensemble sc))))
      (setf new-events (make-events new-events (midi-channel player)
                                    (microtones-midi-channel player)))))
  (replace-events (piece sc) player bar-num start-event replace-num-events
                  new-events auto-beam)
  (when tuplet-brackets
    (add-tuplet-bracket (get-bar sc bar-num player) tuplet-brackets)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken/replace-multi-bar-events
;;; FUNCTION
;;; A post-generation editing method: Replace events across several bars.  All
;;; bars have to be filled, i.e. we can't just leave the last bar half-filled
;;; expecting the existing events to make up the rest.
;;; 
;;; ARGUMENTS 
;;; - the slippery-chicken object
;;; - the player (symbol)
;;; - the start bar.  This can be an absolute bar number or a list of the form 
;;;   '(section sequence bar) (if there a subsections then e.g. '((3 1) 4 2))
;;; - the number of bars we'll replace events in
;;; - the list of events
;;; - (key :interleaved default t) if <new-events> are not already event
;;;   objects we have two ways of passing the event data.  If this argument is
;;;   t, we pass a list of 2-element lists (note rhythm) that we can pass to
;;;   make-events (but this can contain no ties). If nil, then rhythm and pitch
;;;   data is passed as two separate lists within <new-events> to make-events2
;;;   where + can be used to indicate ties.  Pitch data is the usual cs4 or
;;;   (cs4 cd3) for chords, and nil or r indicates a rest.  NB all pitches are
;;;   sounding pitches, so written pitches will be created for transposing
;;;   instruments where necessary.
;;; - (key :consolidate-rests default t): whether shorter rests should
;;;   automatically be collapsed into a single longer rest.
;;; - (key :beat default nil): what beat will be used to consolidate rests
;;;   (rhythm symbol).  If nil, the beat of the meter will be used
;;;   (e.q. crotchet/quarter in 4/4).
;;; - (key :auto-beam default t): whether to automatically beam the new events.
;;; - (key :tuplet-bracket default nil): whether to automatically add tuplet
;;;   (e.g. triplet) brackets to the new events (integer).
;;; 
;;; RETURN VALUE  
;;; The number of new events used to replace the old ones.
;;;
;;; EXAMPLE
;;; (replace-multi-bar-events +coming-rthm-chain+ 'cello 401 1 '((b4 e) e q q))
;;; (replace-multi-bar-events +coming-rthm-chain+ 'cello 401 1 
;;;                           '((h.+h.+h+e e q q) (b4 r r r)) :interleaved nil)
;;; 
;;; SYNOPSIS
(defmethod replace-multi-bar-events ((sc slippery-chicken)
                                     player start-bar num-bars new-events 
                                     &key
                                     ;; 24.3.11: see above.
                                     (interleaved t)
                                     (consolidate-rests t)
                                     ;; for consolidate rests
                                     (beat nil)
                                     (auto-beam t)
                                     ;; 31.3.11: if this is t, then rthms > a
                                     ;; beat will case an error 
                                     (auto-beam-check-dur t)
                                     (tuplet-bracket nil))
;;; ****
  ;; 21.3.11 if we're passing note data in lists, rather than events, init them
  ;; here  
  (unless (event-p (first new-events))
    (let* ((player (get-data player (ensemble sc)))
           (mc (midi-channel player))
           (mmc (microtones-midi-channel player)))
      (setf new-events 
            (if interleaved
                (make-events new-events mc mmc)
                (make-events2 (first new-events) (second new-events) mc mmc)))))
  (replace-multi-bar-events (piece sc) player start-bar num-bars new-events
                            :tempo-map (tempo-map sc)
                            :sc sc
                            :beat beat
                            :consolidate-rests consolidate-rests
                            :auto-beam auto-beam
                            :auto-beam-check-dur auto-beam-check-dur
                            :tuplet-bracket tuplet-bracket))
                      

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF slippery-chicken-edit.lsp
