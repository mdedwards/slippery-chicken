;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****h* sc/slippery-chicken-edit
;;; NAME 
;;; slippery-chicken-edit
;;; 
;;; File:             slippery-chicken-edit.lsp
;;;
;;; Class Hierarchy:  named-object -> slippery-chicken
;;;
;;; Version:          1.1.0
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
;;; $$ Last modified:  15:30:00 Tue Jun 24 2025 CEST
;;;
;;; SVN ID: $Id$ 
;;;
;;; ****
;;; Licence:          Copyright (c) 2010 Michael Edwards
;;;
;;;                   This file is part of slippery-chicken
;;;
;;;                   slippery-chicken is free software; you can redistribute
;;;                   it and/or modify it under the terms of the GNU General
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

;;; NB only include methods here that the user should access (i.e. no -aux
;;; methods) as all of these will be automatically listed and linked on a
;;; manual page. Also, in order for these links to work we need 
;;; slippery-chicken-edit/replace-tempo-map 
;;; not slippery-chicken/replace-tempo-map

(in-package :slippery-chicken)

(proclaim '(special +slippery-chicken-standard-instrument-palette+))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken-edit/replace-tempo-map
;;; DESCRIPTION
;;; Replace the tempo data for a given slippery-chicken object with new
;;; specified tempo indications.
;;;
;;; Calls not only the setf method - which converts bar references like
;;; (section-num sequence-num bar-num) to numbers and makes a tempo-map object,
;;; but also updates all event objects to reflect new start times etc.
;;;
;;; ARGUMENTS
;;; - A slippery-chicken object
;;; - A list that is the new tempo-map.
;;; 
;;; RETURN VALUE
;;; T
;;; 
;;; EXAMPLE
#|
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((pno (piano :midi-channel 1))))
        :tempo-map '((1 (q 60)))
        :set-palette '((1 ((c4 d4 f4 g4 a4 c5 d5 f5 g5 a5 c6))))
        :set-map '((1 (1 1 1 1 1 1 1 1)))
        :rthm-seq-palette '((1 ((((2 4) q q))
                                :pitch-seq-palette ((1 (2))))))
        :rthm-seq-map '((1 ((pno (1 1 1 1 1 1 1 1))))))))
  (replace-tempo-map mini '((1 (q 60 "Andante")) ((1 3 1) (e 80)))))

=> T

|#
;;; 
;;; SYNOPSIS
(defmethod replace-tempo-map ((sc slippery-chicken) tm)
;;; ****
  (setf (tempo-map sc) tm)
  (update-events-tempo sc)
  (update-slots sc (tempo-map sc))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken-edit/add-event-to-bar
;;; DESCRIPTION
;;; Add an event object to a specified bar either at the end of that bar or at
;;; a specified position within that bar.
;;; 
;;; It's the user's responsibility to make sure the bar is full and
;;; update-slots is called after all post-generation editing such as this.
;;; 
;;; ARGUMENTS 
;;; - A slippery-chicken object.
;;; - An event object.
;;; - An integer that is the bar number or a list that is the reference to the
;;;   bar in the form '(section sequence bar), where sequence and bar are
;;;   numbers counting from 1)
;;; - The ID of the player to whose part the event should be added.
;;;
;;; OPTIONAL ARGUMENTS
;;; keyword argument:
;;; - :position. NIL or an integer indicating the position in the bar (0-based)
;;;    where the event should be added. If NIL, the new event is placed at the
;;;    end of the bar. Default = NIL.
;;; 
;;; RETURN VALUE  
;;; T
;;;
;;; EXAMPLE
#|

;;; Adding two events to separate bars, once using a bar number with
;;; :position's default to NIL, and once using a bar number reference list with
;;; :position specified as 2. Print the bars after adding to see the changes.

(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((vn (violin :midi-channel 1))))
        :tempo-map '((1 (q 60)))
        :set-palette '((1 ((c4 d4 f4 g4 a4 c5 d5 f5 g5 a5 c6)))
                       (2 ((cs4 ds4 fs4 gs4 as4 cs5 ds5 fs5 gs5 as5 cs6))))
        :set-map '((1 (1 1 1 1 1 1))
                   (2 (2 2 2 2 2 2)))
        :rthm-seq-palette '((1 ((((2 4) q e s s))
                                :pitch-seq-palette ((1 2 3 4))))
                            (2 ((((2 4) e s s q))
                                :pitch-seq-palette ((1 2 3 4)))))
        :rthm-seq-map '((1 ((vn (1 1 1 1 1 1))))
                        (2 ((vn (2 2 2 2 2 2))))))))
  (add-event-to-bar mini (make-event 'cs4 'e) 2 'vn)
  (print-simple (first (get-bar mini 2)))
  (add-event-to-bar mini (make-event 'c4 'q) '(2 2 1) 'vn :position 2)
  (print-simple (first (get-bar mini '(2 2 1)))))

=> 
(2 4): C4 Q, D4 E, F4 S, G4 S, CS4 E
(2 4): CS4 E, DS4 S, C4 Q, FS4 S, GS4 Q

|#
;;; 
;;; SYNOPSIS
(defmethod add-event-to-bar ((sc slippery-chicken) event bar-num-or-ref player
                             &key (position nil))
;;; ****
  (let ((bar (get-bar sc bar-num-or-ref player)))
    (add-event bar event :position position))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken-edit/replace-events
;;; DESCRIPTION
;;; Replace one or more consecutive existing event objects with new event
;;; objects. All references are 1-based. This method can be applied to only one
;;; bar at a time.
;;;
;;; One or more new event objects can be specified as a replacement for one
;;; single original event object.
;;;
;;; It's the user's responsibility to make sure the bar is full and
;;; update-slots is called after all post-generation editing. 
;;; 
;;; ARGUMENTS 
;;; - A slippery-chicken object.
;;; - The ID of the player whose part is to be modified.
;;; - An integer that is the number of the bar in which the change is to be
;;;   made; or a reference to the bar in the format '(section sequence bar).
;;; - An integer that is the number of the first event object in the given bar
;;;   to replace. 
;;; - An integer that is the total number of consecutive original event objects
;;;   to replace.
;;; - A list of the new event objects, each in turn specified as a 2-item list
;;;   in the format (pitch rhythm), e.g. '((c4 e)). Rests are indicated with
;;;   NIL or 'r, e.g. (nil s) (r h). Chords are indicated by enclosing the
;;;   pitches of the chord in a list, e.g. ((c4 e4) e).
;;; 
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to indicate whether to automatically re-beam the given bar after
;;;   replacing the events. T = beam. Default = NIL.
;;; - A list of integers to indicate tuplet bracket placement, in the format
;;;  '(tuplet-value start-event end-event). These numbers are 0-based and
;;;  inclusive and count rests.
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
        :tempo-map '((1 (q 60)))
        :set-palette '((1 ((c2 e2 d4 e4 f4 g4 a4 f5))))
        :set-map '((1 (1 1 1 1)))
        :rthm-seq-palette '((1 ((((2 4) q (e) s s))
                                :pitch-seq-palette ((1 2 3)))))
        :rthm-seq-map '((1 ((vn (1 1 1 1))))))))
  (replace-events mini 'vn 1 2 1 '((nil s) ((ds5 fs5) s)) t)
  (replace-events mini 'vn 2 2 1 '((cs5 e)))
  (replace-events mini 'vn '(1 3 1) 3 1 '((df4 s)))
  (replace-events mini 'vn 4 1 1 '((ds4 te) (r te) (b3 te)) t '(3 0 2)))

=> T

|#
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
    (add-tuplet-bracket (get-bar sc bar-num player) tuplet-brackets))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken-edit/add-tuplet-bracket-to-bar
;;; DESCRIPTION

;;; Add a tuplet bracket (with number) to a specified bar in a slippery-chicken
;;; object. This method adds only one tuplet bracket of one tuplet type
;;; (triplet, quintuplet etc.) at a time. 
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; - An integer that is the number of the bar to which the tuplet bracket is
;;;   to be added.
;;; - The ID of the player to whose part the tuplet bracket is to be added.

;;; - The bracket info defining the tuplet bracket to be added. This takes the
;;;   form of a three-element list specifying tuplet value, number of the event
;;;   (zero-based) on which the bracket is to begin, and number of the event on
;;;   which the bracket is to end, e.g. '(3 0 2).
;;; 
;;; OPTIONAL ARGUMENTS
;;; T or NIL to indicate whether all existing tuplet brackets in the given bar
;;; are to be deleted first. T = delete. Default = NIL>
;;; 
;;; RETURN VALUE
;;; T
;;; 
;;; EXAMPLE
#|

(let ((mini
       (make-slippery-chicken
        '+sc-object+
        :ensemble '(((va (viola :midi-channel 2))))
        :set-palette '((1 ((c3 d3 e3 f3 g3 a3 b3 c4 d4))))
        :set-map '((1 (1 1 1)))
        :rthm-seq-palette '((1 ((((3 4) (te) - te te - { 3 te ts+ts te } 
                                  - fs fs fs fs fs -))
                                :pitch-seq-palette ((1 2 3 4 5 6 7 8 9 8)))))
        :rthm-seq-map '((1 ((va (1 1 1))))))))
  (add-tuplet-bracket-to-bar mini 1 'va '(3 0 2))
  (add-tuplet-bracket-to-bar mini 2 'va '(5 7 11))
  (add-tuplet-bracket-to-bar mini 3 'va '(3 3 4) t)
  (add-tuplet-bracket-to-bar mini 3 'va '(3 5 6)))

=> T

|#
;;; SYNOPSIS
(defmethod add-tuplet-bracket-to-bar ((sc slippery-chicken) bar-num player
                                      bracket-info 
                                      &optional (delete-all-tuplets-first nil))
;;; ****
  (let ((bar (get-bar sc bar-num player)))
    (if bar
        (add-tuplet-bracket bar bracket-info delete-all-tuplets-first)
        (error "slippery-chicken-edit::add-tuplet-bracket-to-bar: no bar ~a ~
                for ~a." bar-num player))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (add-tuplet-brackets-to-beats +mini+ 'vc '((2 3 0 5) (3 3 0 3) (5 5 0 4)))

;;; SAR Wed Aug  8 12:59:51 BST 2012: Added robodoc entry

;;; ****m* slippery-chicken-edit/add-tuplet-brackets-to-beats
;;; DESCRIPTION
;;; Add the specified tuplet brackets (and numbers) to the specified event
;;; objects in the specified bars within the given slippery-chicken object.
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; - The ID of the player to whose part the tuplet brackets are to be added.
;;; - A list of 4-element sublists that is the bracket info. Each sublist must
;;;   consist of: the number of the bar to which the bracket is to be added;
;;;   the number that is the tuplet type (3 = triplet, 5 = quintuplet etc.);
;;;   the zero-based number of the event where the bracket is to begin; the
;;;   zero-based number that is the number of the event where the bracket is to
;;;   end; e.g. '((2 3 0 5) (3 3 0 3) (5 5 0 4))
;;; 
;;; OPTIONAL ARGUMENTS
;;; T or NIL to indicate whether all existing tuplet bracket info in the given
;;; bars is to first be deleted. T = delete. Default = NIL.
;;; 
;;; RETURN VALUE
;;; NIL.
;;; 
;;; EXAMPLE
#|
(let ((mini
       (make-slippery-chicken
        '+sc-object+
        :ensemble '(((va (viola :midi-channel 2))))
        :set-palette '((1 ((c3 e3 g3 c4))))
        :set-map '((1 (1 1 1)))
        :rthm-seq-palette '((1 ((((3 4) - te te te - - fs fs fs fs fs -
                                  - 28 28 28 28 28 28 28 -))
                                :pitch-seq-palette ((1 2 3 4 1 2 3 4 1 2 3 4 1
                                                       2 3)))))
        :rthm-seq-map '((1 ((va (1 1 1))))))))
  (add-tuplet-brackets-to-beats mini 'va '((1 3 0 2) (2 5 3 7) (3 7 8 14))))

=> NIL

|#
;;; SYNOPSIS
(defmethod add-tuplet-brackets-to-beats 
    ((sc slippery-chicken) player bracket-info
     &optional (delete-all-tuplets-first nil))
;;; ****
  (loop for bi in bracket-info
       for bar-num = (first bi) 
       for bar = (get-bar sc bar-num player)
       do
       (add-tuplet-bracket bar (rest bi) delete-all-tuplets-first)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken-edit/replace-multi-bar-events
;;; DESCRIPTION
;;; Replace specified consecutive event objects across several bars. 
;;;
;;; The new rhythms provided must produce full bars for all bars specified;
;;; i.e., if only a quarter note is provided as the new event for a 2/4 bar, the
;;; method will not automatically fill up the remainder of the bar. In that case
;;; you'll get a warning and the procedure will exit. This means if you don't
;;; know how many bars you want to replace events for, you can pass a large
;;; number and ignore the warning and providing you've filled the bars, all
;;; should be well.
;;; 
;;; ARGUMENTS 
;;; - A slippery-chicken object.
;;; - The ID of the player whose part is to be modified.
;;; - An integer that is the number of the first bar in which event objects are
;;;   to be replaced. This can be an absolute bar number or a list in the form
;;;   '(section sequence bar); or with subsections then e.g. '((3 1) 4 2)).  
;;; - An integer that is the number of bars in which event objects will be
;;;   replaced.
;;; - The list of new event objects. The new event objects can be passed as
;;;   complete event objects; as a list of 2-item lists that are
;;;   note-name/rhythm pairs, e.g: '((c4 q) (d4 e)); or as a list with two
;;;   sub-lists, the first being just the sequence of rhythms and the second
;;;   being just the sequence of pitches, e.g: '((q e) (c4 d4)). For the
;;;   latter, :interleaved must be set to NIL. (see :interleaved below). Pitch 
;;;   data is the usual cs4 or (cs4 cd3) for chords, and NIL or 'r indicate a
;;;   rest. NB: All pitches are sounding pitches; written pitches will be
;;;   created for transposing instruments where necessary.
;;;
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :interleaved. T or NIL to indicate whether the new event data is to be
;;;   processed as a list of note-name/rhythm pairs (or existing event
;;;   objects), or if it is to be processed as a list with two sub-lists, the
;;;   first containing the sequence of rhythms and the second containing the
;;;   sequence of pitches (see above). T = interleaved, i.e. already existing
;;;   event objects or a list of note-name/rhythm pairs. NIL = separate lists
;;;   for rhythms and pitches. Default = T.
;;;   If this argument is T, the list of 2-element lists (note-name/rhythm
;;;   pairs) is passed to make-events, but such a list can contain no ties. If
;;;   the argument is set to NIL, the rhythm and pitch data is passed as two
;;;   separate lists to make-events2 where + can be used to indicate ties.
;;; - :consolidate-rests. T or NIL to indicate whether shorter rests should
;;;   automatically be consolidated into a single longer rest.  
;;;   T = consolidate. Default = T. 
;;;   NB: slippery chicken will always consolidate full bars of rest into
;;;   measure-rests, regardless of the value of this argument.
;;; - :beat. NIL or an integer (rhythm symbol) that indicates which beat basis
;;;   will be used when consolidating rests. If NIL, the beat of the time
;;;   signature will be used (e.g. quarter in 4/4). Default = NIL.
;;; - :auto-beam. T or NIL to indicate whether to automatically beam the new
;;;   events. T = automatically beam. Default = T.
;;; - :tuplet-bracket. NIL or an integer to indicate whether to automatically
;;;   add tuplet (e.g. triplet/quintuplet) brackets to the new events where
;;;   applicable. If this is an integer, all tuplets in the given bar will be
;;;   given a tuplet bracket with that integer as the tuplet number. NB: This
;;;   option does not allow for setting tuplets of different numbers for the
;;;   same bar. To do that, set :tuplet-bracket to NIL and add the
;;;   tuplet-brackets manually. NIL = place no brackets. Default = NIL.
;;; 
;;; RETURN VALUE  
;;; The number of new events used to replace the old ones.
;;;
;;; EXAMPLE
#|
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((vn (violin :midi-channel 1))))
        :set-palette '((1 ((d4 e4 f4 g4))))
        :set-map '((1 (1 1 1 1 1 1))
                   (2 (1 1 1 1 1 1)))
        :rthm-seq-palette '((1 ((((2 4) q e s s))
                                :pitch-seq-palette ((1 2 3 4))))
                            (2 ((((2 4) e s s q)
                                 (s s e +e e))
                                :pitch-seq-palette ((1 2 3 4 3 2 4 1)))))
        :rthm-seq-map '((1 ((vn (1 1 1 1 1 1))))
                        (2 ((vn (2 2 2 2 2 2))))))))
  (replace-multi-bar-events mini 'vn 2 3 
                            '((cs5 h) ((ds5 fs5) h) (nil h)))
  (replace-multi-bar-events mini 'vn '(2 2 2) '3 
                            '((h h h) (cs5 (ds5 fs5) nil))
                            :interleaved nil)
  (replace-multi-bar-events mini 'vn 1 1
                            '((nil e) (nil e) (nil e) (cs4 e))
                            :consolidate-rests t)
  (replace-multi-bar-events mini 'vn 8 1
                            '((nil q) (b3 e) (cs4 s) (ds4 s))
                            :auto-beam t))

=> 4

|#
;;; SYNOPSIS
(defmethod replace-multi-bar-events ((sc slippery-chicken)
                                     player start-bar num-bars new-events 
                                     &key
                                       ;; 24.3.11: see above.
                                       (interleaved t)
                                       ;; MDE Mon Apr 23 12:36:08 2012 --
                                       ;; changed default to nil
                                       (consolidate-rests nil)
                                       ;; for consolidate rests
                                       (beat nil)
                                       ;; MDE Mon Apr 23 12:36:08 2012 --
                                       ;; changed default to nil
                                       (auto-beam nil)
                                       ;; MDE Fri Aug 29 10:18:29 2014 
                                       (warn t)
                                       ;; MDE Mon Sep  1 16:41:02 2014 
                                       (delete-beams t)
                                       (delete-tuplets t)
                                       ;; MDE Sat Jan 17 17:52:07 2015 --
                                       ;; should we update the player slot of
                                       ;; each event? 
                                       (write-player nil)
                                       ;; MDE Thu Dec  2 23:22:18 2021
                                       (update-slots t)
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
                            :sc sc :warn warn
                            :beat beat :update-slots update-slots
                            :write-player write-player
                            :delete-beams delete-beams
                            :delete-tuplets delete-tuplets
                            :consolidate-rests consolidate-rests
                            :auto-beam auto-beam
                            :auto-beam-check-dur auto-beam-check-dur
                            :tuplet-bracket tuplet-bracket))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken-edit/auto-accidentals
;;; DESCRIPTION
;;; Automatically determine which notes in each bar need accidentals and which
;;; don't. 
;;;
;;; This method also places cautionary accidentals (in parentheses) based on
;;; how many notes back the last occurrence of that note/accidental combination
;;; appeared in the bar. The first optional argument to the method allows the
;;; user to specify how many notes back to look.
;;;
;;; NB: As both cmn-display and write-lp-data-for-all call respell-notes by
;;;     default, that option must be set to NIL for this method to be
;;;     effective (see below).
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - An integer that is the number of notes back to look when placing
;;;   cautionary accidentals in parentheses. If the last occurrence of a given
;;;   repeated note/accidental combination was farther back than this number,
;;;   the accidental will be placed in the score in parentheses.
;;; 
;;; RETURN VALUE
;;; Returns NIL.
;;; 
;;; EXAMPLE
#|
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((vn (violin :midi-channel 1))))
        :set-palette '((1 ((fs4 gs4 as4))))
        :set-map '((1 (1 1 1 1)))
        :rthm-seq-palette '((1 ((((4 4) e e e e e e e e))
                                :pitch-seq-palette ((1 2 3 2 1 2 3 2)))))
        :rthm-seq-map '((1 ((vn (1 1 1 1))))))))
  (auto-accidentals mini 4)
  (cmn-display mini :respell-notes nil))

=> NIL

|#
;;; SYNOPSIS
(defmethod auto-accidentals ((sc slippery-chicken) &optional 
                             (cautionary-distance 3)
                             ignore1 ignore2)
;;; ****
  (declare (ignore ignore1 ignore2))
  (loop with players = (players sc) with bar with last-attack with last-notes
     = (ml nil (length players)) for bar-num from 1 to (num-bars (piece sc)) do
     (loop 
        for player in players 
        for i from 0
        do 
        (setf bar (get-bar sc bar-num player))
        ;; MDE Fri Apr 20 14:45:31 2012 -- cautionary-distance added
        (auto-accidentals bar (nth i last-notes) nil cautionary-distance)
        ;; we can't ignore instruments that only sound octaves +/-
        ;; written note as that would leave written and sounding notes
        ;; potentially different, hence nil last argument here.
        (when (plays-transposing-instrument (get-player sc player) nil)
          ;; MDE Fri Apr 20 14:45:31 2012 -- cautionary-distance added
          (auto-accidentals bar (nth i last-notes) t cautionary-distance))
        (setf last-attack (get-last-attack bar nil))
        (when last-attack
          (setf (nth i last-notes) last-attack)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken-edit/respell-notes
;;; DESCRIPTION
;;; Pass through the entire given slippery-chicken object and change some of
;;; the pitch objects to their enharmonic equivalents to produce more sensible
;;; spellings of consecutive pitches in the score.
;;;
;;; An optional argument takes a list specifying which pitches to change in the
;;; same format found in the method enharmonic-spellings; i.e.
;;; '((player (bar note-num))). These notes are changed after the respelling
;;; routine has run.  
;;;
;;; NB: If a list of corrections is specified, the :respell-notes argument of
;;;     any subsequent call to cmn-display or write-lp-data-for-all must be set
;;;     NIL, otherwise the modified pitches may be overwritten.  Also, although
;;;     this algorithm corrects tied notes when respelling, notes referenced in
;;;     the corrections list will not be followed through to any subsequent
;;;     ties.
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - A list of specific notes whose pitches are to be enharmonically flipped,
;;;   in the format, e.g. '((vn (1 1) (1 4)) (vc (2 3) (3 3)))
;;; 
;;; RETURN VALUE
;;; Returns T.
;;; 
;;; EXAMPLE
#|
;; An example using respell-notes for the whole slippery-chicken object.
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((vn (violin :midi-channel 1))))
        :tempo-map '((1 (q 60)))
        :set-palette '((1 ((cs4 ds4 df5 ef5))))
        :set-map '((1 (1 1 1 1 1)))
        :rthm-seq-palette '((1 ((((2 4) q e s s))
                                :pitch-seq-palette ((1 2 3 4)))))
        :rthm-seq-map '((1 ((vn (1 1 1 1 1))))))))
  (respell-notes mini))

;; An example specifying which pitches are to be enharmonically changed.
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((vn (violin :midi-channel 1))))
        :tempo-map '((1 (q 60)))
        :set-palette '((1 ((cs4 ds4 df5 ef5))))
        :set-map '((1 (1 1 1 1 1)))
        :rthm-seq-palette '((1 ((((2 4) q e s s))
                                :pitch-seq-palette ((1 2 3 4)))))
        :rthm-seq-map '((1 ((vn (1 1 1 1 1))))))))
  (respell-notes mini '((vn (1 1) (1 4))))
  (cmn-display mini :respell-notes nil))

=> T

|#
;;; SYNOPSIS
(defmethod respell-notes ((sc slippery-chicken) &optional corrections)
;;; ****
  (format t "~&Respelling notes...")
  ;; this respells written and sounding notes if transposing instrument
  (respell-notes-aux sc)
  ;; 10/5/07: a second pass does pick up some more mistakes...
  (respell-notes-aux sc (when (listp corrections) corrections)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Wed Apr 18 11:57:11 2012 -- added pitches keyword

;;; DJR Tue 18 Feb 2020 13:33:03 GMT -- multiple players allowed plus arguments
;;; for start, end and players can now be nil. Also updated doc.

;;; ****m* slippery-chicken-edit/enharmonics
;;; DESCRIPTION
;;; Change the sharp/flat pitches of a specified region of a specified player's
;;; part to their enharmonic equivalent.
;;;
;;; NB: This method only affects pitches that already have sharp/flat
;;;     accidentals. It does not affect "white-key" notes (e.g. C-natural =
;;;     B-sharp etc.)
;;;
;;; NB: As the cmn-display and write-lp-data-for-all methods call
;;;    :respell-notes by default, this option must be explicitly set to NIL for
;;;    this method to be effective.
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; - An integer or a 2-item list of integers that indicates the first bar in
;;;   which the enharmonics are to be changed. If an integer, the method will
;;;   be applied to all sharp/flat pitches in the bar of that number. If a
;;;   2-item list of integers, these represent '(bar-number note-number). The
;;;   note number is 1-based and counts ties. If nil, it defaults to 1.
;;; - An integer or a 2-item list of integers that indicates the last bar in
;;;   which the enharmonics are to be changed. If an integer, the method will
;;;   be applied to all sharp/flat pitches in the bar of that number. If a
;;;   2-item list of integers, these represent '(bar-number note-number). The
;;;   note number is 1-based and counts ties. If nil it defaults to (num-bars sc)
;;; - The ID of the player or players whose part is to be changed. If nil, the
;;;   method will be applied to all players.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments
;;; - :written. T or NIL to indicate whether to change written-only pitches or
;;;   sounding-only pitches. T = written-only. Default = T.
;;; - :pitches. NIL or a list of note-name symbols. If NIL, all sharp/flat
;;;   pitches in the specified region will be changed to their enharmonic
;;;   equivalents. If a list of one or more note-name symbols, only those
;;;   pitches will be affected.
;;; - :force-naturals. T or NIL to indicate whether to force "natural" note
;;;   names that contain no F or S in their name to convert to their enharmonic
;;;   equivalent (ie, B3 = CF4).  NB double-flats/sharps are not implemented so
;;;   this will only work on F/E  B/C. 
;;; 
;;; RETURN VALUE
;;; Returns T.
;;; 
;;; EXAMPLE
#|
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((cl (b-flat-clarinet :midi-channel 1))
                     (pn (piano :midi-channel 2))
                     (vn (violin :midi-channel 3))))
        :set-palette '((1 ((cs4 ds4 e4 fs4 gs4 as4 b4 cs5))))
        :set-map '((1 (1 1 1 1 1)))
        :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -))
                                :pitch-seq-palette ((1 (2) 3 4 (5) 6 (7) 8)))))
        :rthm-seq-map '((1 ((cl (1 1 1 1 1))
                            (pn (1 1 1 1 1))
                            (vn (1 1 1 1 1))))))))
  (enharmonics mini 1 2 'vn)
  (enharmonics mini 2 3 'pn :pitches '(cs4 ds4))
  (enharmonics mini 3 4 'cl :written nil)
  (enharmonics mini nil nil nil))

=> T


|#
;;; SYNOPSIS
(defmethod enharmonics ((sc slippery-chicken) start end players
                        &key (written t) pitches force-naturals)
;;; ****
  ;; DJR Tue 18 Feb 2020 13:26:23 GMT
  ;; Allow for multiple players and for start, end & players to be nil.  
  (unless players (setf players (players sc)))
  (setf players (force-list players))
  (unless start (setf start 1))
  (unless end (setf end (num-bars sc)))
  (setf pitches (init-pitch-list pitches))
  (loop for player in players do
       (let* ((stlist (listp start))
              (ndlist (listp end))
              (stbar (if stlist (first start) start))
              (ndbar (if ndlist (first end) end))
              (stnote (if stlist (second start) 1))
              ;; NB this means we'll add marks to tied notes too
              (ndnote (when ndlist (second end)))) ; nil processed in do-bar
         (flet ((do-bar (bar-num start-note end-note)
                  (let ((bar (get-bar sc bar-num player))
                        ;; MDE Tue Apr 24 18:02:25 2012 -- 
                        (transp (transposition-semitones
                                 (get-instrument-for-player-at-bar
                                  player bar-num sc))))
                    ;; MDE Tue Apr 24 18:02:30 2012 -- 
                    (when (zerop transp)
                      (setf transp nil))
                    (unless end-note
                      (setf end-note (num-score-notes bar)))
                    (loop for i from start-note to end-note 
                       for e = (get-nth-non-rest-rhythm (1- i) bar)
                       do
                       ;; MDE Mon Apr 23 13:21:16 2012 -- handle chords too
                         (when (and (event-p e) (is-chord e))
                           (when (and transp written
                                      (not (written-pitch-or-chord e)))
                             (warn "~a~%slippery-chicken-edit::enharmonics: ~
                             no written-pitch-or-chord (bar ~a, ~a)." 
                                   e bar-num player))
                           (loop for p in (data
                                           (if (and written transp
                                                    (written-pitch-or-chord e))
                                               (written-pitch-or-chord e)
                                               (pitch-or-chord e)))
                              and chord-note-ref from 1
                              do
                                (when 
                                    (or (not pitches)
                                        ;; enharmonics not equal!
                                        (pitch-member p pitches nil))
                                  (enharmonic e :written (and transp written)
                                              ;; MDE Tue May 28 11:04:42 2013 --
                                              ;; :force-naturals added
                                              :force-naturals force-naturals
                                              :chord-note-ref chord-note-ref))))
                       ;; MDE Wed Apr 18 12:08:51 2012 
                         (when (and (event-p e)
                                    (is-single-pitch e)
                                    (or (not pitches)
                                        (pitch-member
                                         (if (and transp written)
                                             (written-pitch-or-chord e)
                                             (pitch-or-chord e))
                                         ;; enharmonics not equal!
                                         pitches nil)))
                           ;; MDE Tue May 28 11:04:42 2013 -- :force-naturals
                           (enharmonic e :force-naturals force-naturals
                                       :written (and transp written)))))))
           (if (= stbar ndbar)
               (do-bar stbar stnote ndnote)
               (progn 
                 (do-bar stbar stnote nil)
                 (do-bar ndbar 1 ndnote)
                 ;; now the bars in the middle
                 (loop for bnum from (1+ stbar) to (1- ndbar) do
                      (do-bar bnum 1 nil)))))))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Fri Apr 20 15:45:41 BST 2012: Added robodoc entry

;;; SAR: some of MDE's original comment taken directly into robodoc
;;; SAR: Fri Apr 20 16:18:17 BST 2012: I changed "from high to low" to "from
;;; low to high", since that's the way it appears to work.

;;; where (596 (1 2)) is accessing the second chord note (counting from high to
;;; low) of the first sounding event of bar 596

;;; SAR: Fri Apr 20 16:20:35 BST 2012: I don't see this in cmn-display:
;;; NB Designed to be called from cmn-display but can be called by user.

;;; ****m* slippery-chicken-edit/enharmonic-spellings
;;; DESCRIPTION
;;; Change the pitch of specified event objects to their enharmonic
;;; equivalents. 
;;;
;;; This takes as its second argument a list of lists, each of which consists
;;; of the ID of the player whose part is to be altered and a series of
;;; bar-number/event-number pairs, where (2 3) indicates that the pitch of the
;;; third event of the second bar is to be changed to its enharmonic
;;; equivalent. 
;;;
;;; Pitches within chords are specified by following the bar number with a
;;; 2-item list consisting of the event number and the number of the pitch
;;; within the chord, counting from low to high, where (2 (2 4)) indicates that
;;; the fourth pitch from the bottom of the chord located in the second event
;;; object of bar 2 should be changed to its enharmonic equivalent.
;;;
;;; An optional T can be included to indicate that the written pitch is to be
;;; changed, but not the sounding pitch, as in (cl (3 4 t)).
;;;
;;; NB: In order for this method to work, the :respell-notes option of
;;;     cmn-display and write-lp-data-for-all must be set to NIL.
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; - The list of changes to be made, in the format '((player changes...)),
;;;   e.g.: 
;;; 
;;;   '((cl (3 3 t) (3 4 t))
;;;     (pn (2 (2 4)))
;;;     (vc (1 1) (1 3) (1 4) (1 6)))
;;; 
;;; RETURN VALUE
;;; Returns T.
;;; 
;;; EXAMPLE
#|

|#
;;; SYNOPSIS
(defmethod enharmonic-spellings ((sc slippery-chicken) corrections)
;;; ****
  (loop for player in corrections do
       (loop 
          with p = (first player)
          for correct in (rest player)
          for bar-num = (first correct)
          for note-num = (second correct)
          for chord = (listp note-num)
          for written = (third correct)
          for note = (get-note sc bar-num 
                               ;; this makes sure we get the chord object
                               ;; instead of a single pitch. 
                               (if chord 
                                   (first note-num)
                                   note-num)
                               p)
          do
            (unless note
              (error "slippery-chicken::enharmonic-spellings: ~
                        No note! ~a: bar ~a note ~a" p bar-num note-num))
            (enharmonic note 
                        :written written
                        :force-naturals t
                        :chord-note-ref (when chord (second note-num))
                        ;; 22.10.11
                        :force-naturals t)))
  t)
              
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This does things by looking at enharmonic spellings in a whole bar

;;; SAR Sun Apr 22 09:46:08 BST 2012: Added robodoc entry

;;; ****m* slippery-chicken-edit/respell-bars
;;; DESCRIPTION
;;; Look for enharmonically equivalent pitches in the same bar and try to unify
;;; their spelling. The method applies this process to every bar in the given
;;; slippery-chicken object.
;;;
;;; Also see rthm-seq-bar/respell-bar and slippery-chicken/respell-notes.
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; 
;;; RETURN VALUE
;;; Returns NIL.
;;; 
;;; EXAMPLE
#|
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((vn (violin :midi-channel 1))))
        :tempo-map '((1 (q 60)))
        :set-palette '((1 ((cs4 ds4 df5 ef5))))
        :set-map '((1 (1 1 1 1 1)))
        :rthm-seq-palette '((1 ((((2 4) q e s s))
                                :pitch-seq-palette ((1 2 3 4)))))
        :rthm-seq-map '((1 ((vn (1 1 1 1 1))))))))
  (respell-bars mini))

=> T

|#
;;; SYNOPSIS
(defmethod respell-bars ((sc slippery-chicken))
;;; ****
  (loop 
     for player in (players sc) 
     with last-attack
     do
       (setf last-attack nil)
       (loop
          for bnum from 1 to (num-bars sc) 
          for bar = (get-bar sc bnum player)
          ;; 9.2.11: have to find out whether player is playing a transposing
          ;; instrument on a bar by bar basis
          for written-too = (transposing-instrument-p 
                             (get-instrument-for-player-at-bar
                              player bar sc))
          do
            (respell-bar bar sc player nil last-attack)
            (when written-too
              (respell-bar bar sc player t last-attack))
            (setf last-attack (get-last-attack bar nil))))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sun Apr 22 10:57:57 BST 2012: Added robodoc entry

;;; SAR Sun Apr 22 10:58:56 BST 2012: Moved MDE's comment into the entry.

;;; MDE 8/4/07: keep track of the last two now in order to make better
;;; decisions.

;;; ****m* slippery-chicken-edit/respell-notes-for-player
;;; DESCRIPTION
;;; Pass through the pitches of a specified player's part and change some of
;;; the pitches to their enharmonic equivalents in order to produce more
;;; sensible spellings of consecutive notes.
;;;
;;; This is just a very simple attempt to better spell notes by comparing each
;;; note to the previous two and making it the same accidental type. It
;;; doesn't look further back or ahead as of yet. 
;;;
;;; If the optional argument is set to T, then look at the written notes
;;; instead of the sounding notes.
;;;
;;; NB: Since both the cmn-display and write-lp-data-for-all methods
;;;     automatically call respell-notes for all players of an entire
;;;     sc-object, their :respell-notes argument may need to be set to NIL for
;;;     this method to produce the desired results.
;;;
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; - The ID of the player whose pitches are to be modified.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to indicate whether to change written pitches only or sounding
;;;   pitches only. T = change written pitches only. Default = NIL.
;;; 
;;; RETURN VALUE
;;; Returns NIL.
;;; 
;;; EXAMPLE
#|

(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((cl (b-flat-clarinet :midi-channel 1))
                     (vn (violin :midi-channel 2))))
        :tempo-map '((1 (q 60)))
        :set-palette '((1 ((b3 cs4 b4 cs5))))
        :set-map '((1 (1 1 1 1 1)))
        :rthm-seq-palette '((1 ((((2 4) q e s s))
                                :pitch-seq-palette ((1 2 3 4)))))
        :rthm-seq-map '((1 ((cl (1 1 1 1 1))
                            (vn (1 1 1 1 1))))))))
  (respell-notes-for-player mini 'cl t)
  (cmn-display mini :respell-notes nil :in-c nil))

=> T

|#
;;; SYNOPSIS
(defmethod respell-notes-for-player ((sc slippery-chicken) player 
                                     &optional written)
;;; ****
  ;; reset to the first event
  (next-event sc player nil t)
  (loop 
     with last = (next-event sc player t)
     with last-but-one
     with respelled
     for this = (next-event sc player)
     ;;for this = (next-event sc player t) ;; attacked notes only!
     for this-porc = (when this
                       (if written
                           (written-pitch-or-chord this)
                           (pitch-or-chord this)))
     for last-porc = (when last
                       (if written
                           (written-pitch-or-chord last)
                           (pitch-or-chord last)))
     while this
     do
     (setf respelled nil)
     (unless (and this last)
       (error "slippery-chicken::respell-notes-for-player: ~
                 this or last nil!: ~%this: ~a~%last ~a" this last))
     ;; we could hit a rest, or change from a transposing instrument to
     ;; non-transposing... 
     (cond ((chord-p this-porc) 
            (respell-chord (if written
                               (written-pitch-or-chord this)
                               (pitch-or-chord this)))
            ;; chords are almost always respelled so make sure we do the
            ;; ties. 
            (setf respelled 2))
           ((or (not this-porc) (not last-porc))
            nil)       ; do nothing
           ;; (error "slippery-chicken::respell-notes-for-player: ~
           ;; written is t but no written-pitch-or-chord!"))
           ((and (is-single-pitch this) (is-single-pitch last))
            (setf respelled (respell last this written 
                                     ;; only allow 'this' to be respelled if
                                     ;; 'last' is tied to 
                                     (is-tied-to last)
                                     ;; t
                                     ))
            ;; if we've gone and changed last and it will now form a bad
            ;; interval with the one before that, change it back and change
            ;; this too!
            (when (and respelled
                       (= 1 respelled) ;; we respelled last
                       last-but-one
                       ;; 16/4/07:
                       ;; if there's been a bar or more between last notes
                       ;; then we can forget our interval rules between last
                       ;; note and this note.
                       ;; MDE Wed Apr 18 11:27:47 2012 -- 
                       ;; (= (bar-num last) (bar-num last-but-one))
                       (and (= (bar-num last) (bar-num last-but-one))
                            (> (- (bar-num this) (bar-num last)) 1))
                       (bad-interval-p last last-but-one))
              ;; (format t "~%changing back ~a bar ~a"
              ;;     (data (written-pitch-or-chord last)) (bar-num last))
              (enharmonic last :written written) ;; change it back
              (enharmonic this :written written)))
           ((and (chord-p last) (is-single-pitch this))))
     ;; we've now got to make sure any notes tied from a respelled last or
     ;; this are spelled the same 
     (when respelled ;; either nil, 1 or 2
       (flet ((do-tie (event porc)
                (unless porc
                  (error "slippery-chicken::respell-notes-for-player: ~
                             can't tie to a non-existent pitch! ~
                             player: ~a, bar ~a"
                         player (next-event sc nil)))
                (let ((clone (clone porc)))
                  (if written
                      (setf (written-pitch-or-chord event) clone)
                      (setf (pitch-or-chord event) clone)))))
         (when (and (= respelled 1) this last
                    (is-tied-from last)
                    (is-tied-to this))
           ;; can't use last-porc as that will have the old spelling!
           (do-tie this (if written
                            (written-pitch-or-chord last)
                            (pitch-or-chord last))))
         (when (is-tied-from this)
           (loop 
              with last-tied
              for e = (next-event sc player)
              while e
              do
                #|(unless e
                (error "slippery-chicken::respell-notes-for-player: ~
                          e is nil with this of ~%~a" this))|#
              ;; can't use this-porc as that will have the old spelling!
              (if (is-tied-to e)
                  (progn
                    (do-tie e (if written
                                  (written-pitch-or-chord this)
                                  (pitch-or-chord this)))
                    (setf last-tied e))
                  (progn
                    ;; this sets the counter back one so we next compare
                    ;; the last tied note to the first attacked
                    ;; note after the tie
                    ;; (format t "~&~a ~a" (get-pitch-symbol e) (data e))
                    (next-event sc player -1)
                    (setf this last-tied
                          ;; so that last-but-one becomes nil so we don't
                          ;; test it--no longer necessary after ties (?)
                          last nil)
                    (return)))
              ;; (setf this e)
              ;; this ensures that we start over with last being the
              ;; first note after the tie, which of course means we don't
              ;; improve the spelling of the tied note based on the note
              ;; after...ah well, I said this was only a simple attempt
              ;; at good spelling...
              ;; 7/4/07
              ;; no...get this right...make sure we restart on last note
              ;; of tie!
              ;; finally (next-event sc nil :attacked-notes-only -1)
              ;; (setf this last-e)
              ))))
     (when (and this (not (is-rest this)))
       (setf last-but-one last
             last this))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Fri Apr 20 11:06:00 BST 2012: Added robodoc entry

;;; ****m* slippery-chicken-edit/change-pitch
;;; DESCRIPTION
;;; Change the pitch of a specified event to a new specified pitch. The new
;;; pitch is not required to be a member of the current set.
;;;
;;; NB The new pitch is the sounding pitch if a transposing instrument, unless
;;; optional argument is set to T.
;;;
;;; NB In the case of ties, this doesn't update the following 'tied-to' notes
;;; but see the documentation for the change-pitches method below (or call
;;; check-ties, which fixes them too).
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; - An integer that is the bar number in which the pitch is to be changed.
;;; - An integer that is the number of the note in the specified bar whose
;;;   pitch is to be changed (1-based).
;;; - The ID of the player for whom the pitch is to be changed.
;;; - A note-name symbol that is the new pitch.
;;;
;;; OPTIONAL ARGUMENTS
;;; T or NIL to indicate whether the written or sounding pitch should be
;;; changed.  Default = NIL (sounding).
;;; 
;;; RETURN VALUE
;;; Returns T.
;;; 
;;; EXAMPLE
#|
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((vc (cello :midi-channel 1))))
        :tempo-map '((1 (q 60)))
        :set-palette '((1 ((d3 e3 f3 g3 a3 b3 c4 e4))))
        :set-map '((1 (1 1 1 1)))
        :rthm-seq-palette '((1 ((((4 4) e e e e e e e e))
                                :pitch-seq-palette ((1 2 3 4 5 6 7 8)))))
        :rthm-seq-map '((1 ((vc (1 1 1 1))))))))
  (change-pitch mini 1 3 'vc 'fs3))

=> T
|#
;;; SYNOPSIS
(defmethod change-pitch ((sc slippery-chicken) bar-num note-num player
                         new-pitch &optional written)
;;; ****
  (change-pitch (piece sc) bar-num note-num player new-pitch written))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sun Apr 22 11:56:20 BST 2012: Added robodoc entry

;;; ****m* slippery-chicken-edit/rest-to-note
;;; DESCRIPTION
;;; Change a specified event object from a rest into a note by supplying a
;;; pitch or chord (as objects or symbols). 
;;;
;;; Marks to be attached to the new note can be supplied as a symbol or a list
;;; as an optional argument.
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; - An integer that is the number of the bar in which the rest is to be
;;;   changed to a note.
;;; - An integer that is the number of the rest in the given bar that is to be
;;;   changed. This number counts rests only, not sounding notes or events.
;;; - The ID of the player whose part is to be changed.
;;; - A note-name symbol that is to be the pitch of the new note, or a list of
;;;   note-name symbols that will make up a chord.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - A mark or list of marks to be attached to the new note.
;;; 
;;; RETURN VALUE
;;; Returns the new event object created.
;;; 
;;; EXAMPLE
#|
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((vn (violin :midi-channel 1))))
        :tempo-map '((1 (q 60)))
        :set-palette '((1 ((cs4 ds4 fs4))))
        :set-map '((1 (1 1 1 1 1)))
        :rthm-seq-palette '((1 ((((2 4) q (e) s s))
                                :pitch-seq-palette ((1 2 3)))))
        :rthm-seq-map '((1 ((vn (1 1 1 1 1))))))))
  (rest-to-note mini 2 1 'vn 'gs5)
  (rest-to-note mini 3 1 'vn '(gs5 b5))
  (rest-to-note mini 4 1 'vn '(gs4 b4) 'ppp)
  (rest-to-note mini 5 1 'vn '(gs4 b4) '(fff pizz)))

=> 
EVENT: start-time: 9.000, end-time: 9.500, 
       duration-in-tempo: 0.500, 
       compound-duration-in-tempo: 0.500, 
       amplitude: 0.900 
       bar-num: 5, marks-before: NIL, 
       tempo-change: NIL 
       instrument-change: NIL 
       display-tempo: NIL, start-time-qtrs: 9.000, 
       midi-time-sig: NIL, midi-program-changes: NIL, 
       8va: 0
       pitch-or-chord: 
CHORD: auto-sort: T, marks: NIL, micro-tone: NIL
SCLIST: sclist-length: 2, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: NIL, tag: NIL, 
data: (
[...]

|#
;;; SYNOPSIS
(defmethod rest-to-note ((sc slippery-chicken) bar-num rest-num player new-note
                         &rest marks)
;;; ****
  (let ((event (rest-to-note (piece sc) bar-num rest-num player new-note marks))
        (player (get-player sc player))
        (bar (get-bar sc bar-num player)))
    (unless bar
      (error "slippery-chicken::rest-to-note: can't get bar ~a for ~a"
             bar-num player))
    (set-midi-channel event (midi-channel player)
                      (microtones-midi-channel player))
    (gen-stats bar)
    event))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken-edit/change-pitches
;;; DESCRIPTION
;;; Change the pitches of the specified event objects for a given player to the
;;; specified new pitches. 
;;;
;;; If the new pitches are passed as a simple flat list, the method will just
;;; change the pitch of each consecutive attacked event object (with NIL
;;; indicating no change), moving from bar to bar as necessary, until all of
;;; the specified new pitches are used up. Also, if a flat list is passed, each
;;; new pitch specified will be applied to each consecutive attacked note;
;;; i.e., ties don't count as new pitches.
;;;
;;; Tied-to events are left with the old pitch information, which is of course
;;; a potential problem. When generating scores though, we usually call
;;; respell-notes, which calls check-ties, which corrects spellings of tied-to
;;; notes and therefore in effect changes those notes too. So generally, we
;;; don't have to worry about this, but if you explicitly tell slippery chicken
;;; not to respell notes, you'll need to call check-ties with the first
;;; optional argument as T.
;;;
;;; Also see the documentation in the bar-holder class for the method of the
;;; same name.
;;;
;;; NB As various methods (e.g. transposition, change-pitch(es)) in various
;;; classes (slippery-chicken, bar-holder, rthm-seq-bar, rthm-seq) may change
;;; pitch information and midi-channels stored in events, chords, pitches,
;;; etc. it is recommended to call the slippery-chicken reset-midi-channels
;;; method before calling midi-play, if any of those methods have been
;;; used. This way midi channels for pitches belonging to particular players
;;; will use channels originally allocated in the ensemble for chromatic and
;;; microtonal pitches, no matter where they originated or how they were
;;; changed.
;;;
;;; ARGUMENTS 
;;; - A slippery-chicken object.
;;; - The ID of the player whose part is to be modified.
;;; - An integer that is the number of the first bar whose pitches are to be
;;;   modified. 
;;; - A list note-name symbols and NILs, or a list of lists of note-name
;;;   symbols and NILs, which are the new pitches. If a simple flat list, see
;;;   the comment in the description above. If a list of lists, each sub-list
;;;   will represent a full bar; e.g., (change-pitches bh 'vla 5 '((g3 gs4) nil
;;;   (nil nil aqf5))) will change the pitches in bars 5 and 7 (for the player
;;;   'vla), whereas bar six, indicated by nil, wouldn't be changed; similarly
;;;   the first two notes of bar 7, being nil, will also not be changed, but
;;;   note 3 will.
;;;
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :use-last-octave. T or NIL to indicate whether or not each consecutive
;;;   new pitch listed will automatically take the most recent octave number
;;;   specified; e.g. '((a3 b g cs4)). T = use last octave number. Default = T.
;;; - :marks. A list of marks to be added to the events objects. This option
;;;   can only be used in conjunction with the simple flat list of pitches. In
;;;   this case the list of pitches and list of marks must be the same length
;;;   and correspond to each other item by item. Sub-lists can be used to add
;;;   several marks to a single event. NB: See cmn.lsp::get-cmn-marks for the
;;;   list of recognised marks. If NIL, no marks will be added. Default = NIL.
;;; - :written.  T or NIL to indicate whether these are the written or sounding
;;;   notes for a transposing instrument. Default = NIL. 
;;; - :warn.  If there are more pitches in the given list than there are events
;;;   in the slippery-chicken structure, issue a warning, unless NIL.  
;;;   Default = T.  
;;; 
;;; RETURN VALUE  
;;; If the new pitches are passed as a simple flat list, the method returns
;;; the number of the bar in which the pitches were changed; 
;;; otherwise returns T. 
;;;
;;; EXAMPLE
#|
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((vc (cello :midi-channel 1))))
        :tempo-map '((1 (q 60)))
        :set-palette '((1 ((d3 e3 f3 g3 a3 b3 c4 e4))))
        :set-map '((1 (1 1 1 1 1 1)))
        :rthm-seq-palette '((1 ((((4 4) e e e e e e e e))
                                :pitch-seq-palette ((1 2 3 4 5 6 7 8)))))
        :rthm-seq-map '((1 ((vc (1 1 1 1 1 1))))))))
  (change-pitches mini 'vc 2 '((fs3 gs3 as3)))
  (change-pitches mini 'vc 3 '((nil nil fs3 gs as ds fs gs) 
                               nil
                               (cs4 ds fs))))

=> T

|#
;;; 
;;; SYNOPSIS
(defmethod change-pitches ((sc slippery-chicken) player start-bar new-pitches
                           &key (use-last-octave t) marks written
                             (warn t))
;;; ****
  (if (simple-listp new-pitches)
      (progn
        (when marks
          (unless (= (length new-pitches) (length marks))
            (error "slippery-chicken::change-pitches: marks (~a) ~
                    and new-pitches (~a) must be the same length"
                   (length marks) (length new-pitches))))
        (loop for note in new-pitches 
           for count from 0
           ;; this just resets to start-bar; doesn't get an event
           with e = (next-event sc player t start-bar)
           with last
           do
             (setf e (next-event sc player t)) ; attacked notes only
             (unless (event-p e)
               (when warn
                 (warn "slippery-chicken::change-pitches: couldn't get event ~
                        no ~a ~%(ran out of bars with ~a new pitches?). ~
                        ~%Last event was ~&~a."
                       (1+ count) (length new-pitches) last))
               (return))
             (when note
               ;; MDE Thu May 30 18:17:24 2013 
               (unless (or (chord-p note) (pitch-p note))
                 (when use-last-octave
                   (multiple-value-bind
                         (n o)
                       (get-note-octave note t)
                     (setf note (join-note-octave n o)))))
               (if written
                   (set-written-pitch-or-chord e note)
                   (setf (pitch-or-chord e) note)))
           ;; NB note might be nil but mark not hence this isn't in the when 
             (rhythm-add-marks e (nth count marks))
             (setf last e))
        ;; this hack gets the current bar number so we return where we left off
        (next-event sc nil))
      ;; the bar-holder method
      (change-pitches (piece sc) player start-bar new-pitches 
                      :use-last-octave use-last-octave :written written)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Fri Apr 20 11:13:46 BST 2012: Added robodoc entry
;;; SAR Fri Apr 20 11:22:21 BST 2012: Deleted MDE comments here as they have
;;; been taken into the robodoc in full.

;;; ****m* slippery-chicken-edit/change-time-sig
;;; DESCRIPTION
;;; Force a change of the time-sig associated with a specified bar. 
;;; 
;;; NB: This method does not check to see if the rhythms in the bar add up to a 
;;;     complete bar in the new time-sig.
;;;
;;; Also see rthm-seq-bar (setf time-sig).
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; - An integer that is the number of the bar whose time signature should be
;;;   changed or a list that is a reference to the bar whose time signature is
;;;   to be changed in the format (section sequence bar).
;;; - The new signature in the format (number-of-beats beat-unit).
;;; 
;;; RETURN VALUE
;;; Returns T.
;;; 
;;; EXAMPLE
#|
;;; Changing two time signatures; once using the integer bar reference, the
;;; second time using the list reference to the bar number.
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((vc (cello :midi-channel 1))))
        :tempo-map '((1 (q 60)))
        :set-palette '((1 ((d3 e3 f3 g3 a3 b3 c4 e4))))
        :set-map '((1 (1 1 1 1)))
        :rthm-seq-palette '((1 ((((4 4) e e e e e e e e))
                                :pitch-seq-palette ((1 2 3 4 5 6 7 8)))))
        :rthm-seq-map '((1 ((vc (1 1 1 1))))))))
  (change-time-sig mini 2 '(3 8))
  (change-time-sig mini '(1 1 1) '(5 8)))

=> T 

|#
;;; SYNOPSIS
(defmethod change-time-sig ((sc slippery-chicken) bar-num-or-ref new-time-sig)
;;; ****
  (change-time-sig (piece sc) bar-num-or-ref new-time-sig))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken-edit/add-mark-to-note
;;; DESCRIPTION
;;; Add the specified mark to the specified note of a given slippery-chicken
;;; object. 
;;;
;;; NB: This method counts notes, not events; i.e., not rests.
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; - An integer that is the bar number to which to add the mark
;;; - An integer that is the note number two which to add the mark. This is
;;;   1-based, and counts notes not events; i.e., not rests. If a list, then it
;;;   is of the form (note-num chord-note), where chord-note is 1-based and
;;;   counts from the lowest note up.
;;; - The ID of the player to whose part the mark is to be added.
;;; - The mark to add.
;;; 
;;; RETURN VALUE
;;; Returns T.
;;; 
;;; EXAMPLE
#|
;;; Add a mark to a note in a bar with a rest. Print the corresponding event
;;; object to see the result. 
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((vn (violin :midi-channel 1))))
        :tempo-map '((1 (q 60)))
        :set-palette '((1 ((c4 d4 f4 g4 a4 c5 d5 f5 g5 a5 c6))))
        :set-map '((1 (1 1 1 1 1 1)))
        :rthm-seq-palette '((1 ((((2 4) q (e) s s))
                                :pitch-seq-palette ((1 2 3)))))
        :rthm-seq-map '((1 ((vn (1 1 1 1 1 1))))))))
  (add-mark-to-note mini 3 2 'vn 'ppp)
  (print (marks (get-event mini 3 2 'vn)))
  (print (marks (get-event mini 3 3 'vn))))

=>
NIL 
(PPP)

|#
;;; SYNOPSIS
(defmethod add-mark-to-note ((sc slippery-chicken)
                             bar-num note-num player mark)
;;; ****
  (add-mark-to-note (piece sc) bar-num note-num player mark))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Thu Apr 19 13:27:06 BST 2012: Added robodoc entry

;;; 1-based
;;; ****m* slippery-chicken-edit/add-mark-to-event
;;; DESCRIPTION
;;; Add the specified mark to the MARKS slot of the specified event within the
;;; given slippery-chicken object.
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; - An integer that is the bar number to which the mark is to be added.
;;; - An integer that is the event number in the specified bar to which the
;;;   mark is to be added.
;;; - The ID of the player to which to add the mark.
;;; - The mark to add.
;;;
;;; RETURN VALUE
;;; Returns T.
;;; 
;;; EXAMPLE
#|
;;; Add a mark to an event object then read the value of the MARKS slot of that
;;; event to see the result
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((vn (violin :midi-channel 1))))
        :tempo-map '((1 (q 60)))
        :set-palette '((1 ((c4 d4 f4 g4 a4 c5 d5 f5 g5 a5 c6))))
        :set-map '((1 (1 1 1 1 1 1)))
        :rthm-seq-palette '((1 ((((2 4) q (e) s s))
                                :pitch-seq-palette ((1 2 3)))))
        :rthm-seq-map '((1 ((vn (1 1 1 1 1 1))))))))
  (add-mark-to-event mini 3 2 'vn 'ppp)
  (marks (get-event mini 3 2 'vn)))

=> (PPP)

|#
;;; SYNOPSIS
(defmethod add-mark-to-event ((sc slippery-chicken) bar-num event-num player
                              mark)
;;; ****
  (add-mark-to-event (piece sc) bar-num event-num player mark))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Thu Apr 19 12:42:20 BST 2012: Added robodoc entry

;;; 28.2.11  event-num can be an integer (1-based) or a list of event numbers 1
;;; for each instrument counting from the top of the score down

;;; ****m* slippery-chicken-edit/add-mark-all-players
;;; DESCRIPTION
;;; Add a specified mark to a specified event in the parts of all players. The
;;; event can either be specified as a 1-based integer, in which case the mark
;;; will be attached to the same event in all parts, or as a list of integers,
;;; in which the mark is attached to different events in the same bar for each
;;; player, passing from the top of the ensemble downwards.
;;;
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; - An integer that is the bar number or a list of integers that is a
;;;   reference to the bar number in the form (section sequence bar).
;;; - An integer that is the event to which to attach the specified mark in all
;;;   parts, or a list of integers that are the individual events to which to
;;;   attach the mark in the consecutive players.
;;; - The mark to be added.
;;; 
;;; RETURN VALUE
;;; Always returns T.
;;; 
;;; EXAMPLE
#|

;;; Apply the method twice: Once using an integer to attach the mark to the
;;; same event in all players, and once using a list to attach the mark to
;;; different events in the consecutive players. Print the corresponding marks
;;; slots to see the results.
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((cl (b-flat-clarinet :midi-channel 1))
                     (hn (french-horn :midi-channel 2))
                     (vc (cello :midi-channel 3))))
        :tempo-map '((1 (q 60)))
        :set-palette '((1 ((f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5))))
        :set-map '((1 (1 1 1 1 1))
                   (2 (1 1 1 1 1)))
        :rthm-seq-palette '((1 ((((4 4) h q e s s))
                                :pitch-seq-palette ((1 2 3 4 5)))))
        :rthm-seq-map '((1 ((cl (1 1 1 1 1))
                            (hn (1 1 1 1 1))
                            (vc (1 1 1 1 1))))
                        (2 ((cl (1 1 1 1 1))
                            (hn (1 1 1 1 1))
                            (vc (1 1 1 1 1))))))))
  (add-mark-all-players mini 3 1 'ppp)
  (add-mark-all-players mini '(2 2 1) '(1 2 3) 'fff)
  (loop for i in '(cl hn vc)
     do (print (marks (get-event mini 3 1 i))))
  (loop for i in '(cl hn vc)
     for e in '(1 2 3)
     do (print (marks (get-event mini '(2 2 1) e i)))))

=>
(PPP) 
(PPP) 
(PPP) 
(FFF) 
(FFF) 
(FFF)


|#
;;; SYNOPSIS
(defmethod add-mark-all-players ((sc slippery-chicken)
                                 bar-num event-num mark)
;;; ****
  (if (listp event-num)
      (unless (= (num-players (piece sc)) (length event-num))
        (error "slippery-chicken::add-mark-all-players: event-num list ~
                must contain a number for each player: ~a" event-num))
      (setf event-num (ml event-num (num-players (piece sc)))))
  (loop for player in (players sc) and enum in event-num do
       ;; (format t "~%~a ~a ~a ~a" bar-num enum player mark)
       (add-mark-to-event (piece sc) bar-num enum player mark))
  t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Fri Apr 20 18:04:20 BST 2012: Added robodoc entry

;;; ****m* slippery-chicken-edit/note-add-bracket-offset
;;; DESCRIPTION
;;; For CMN only: Adjust the position, lengths, and angles of the tuplet
;;; bracket attached to a specified event object. 
;;;
;;; NB: The bracket data is stored in the BRACKET slot of the first event
;;;     object of a given tuplet figure.
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; - An integer that is the number of the bar in which the tuplet bracket is
;;;   located. 
;;; - An integer that is the event to which the tuplet bracket is
;;;   attached. Tuplet brackets are attached to the first event object of a
;;;   given tuplet figure.
;;; - The ID of the player in whose part the tuplet bracket is located.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; NB: At least one of these arguments must be set in order to create a
;;;     change.  
;;; - :dx. A positive or negative decimal number to indicate the horizontal
;;;   offset of the entire bracket.
;;; - :dy. A positive or negative decimal number to indicate the vertical
;;;   offset of the entire bracket.
;;; - :dx0. A positive or negative decimal number to indicate the horizontal
;;;   offset of the left corner of the bracket.
;;; - :dy0.A positive or negative decimal number to indicate the vertical
;;;   offset of the left corner of the bracket.
;;; - :dx1. A positive or negative decimal number to indicate the horizontal
;;;   offset of the right corner of the bracket.
;;; - :dy1. A positive or negative decimal number to indicate the vertical
;;;   offset of the right corner of the bracket.
;;; - :index. An integer that indicates which bracket of a nested bracket on
;;;   the same event is to be affected.  0 = outermost bracket, 1 = first nested
;;;   bracket, etc. Default = 0.
;;; 
;;; RETURN VALUE
;;; Returns a list of the bracket start/end indicator and the tuplet value
;;; followed by the offset values passed to the keyword arguments.
;;; 
;;; EXAMPLE
#|
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((vc (cello :midi-channel 1))))
        :tempo-map '((1 (q 60)))
        :set-palette '((1 ((f3 g3 a3 b3))))
        :set-map '((1 (1)))
        :rthm-seq-palette '((1 ((((2 4) { 3 te te te } q ))
                                :pitch-seq-palette ((1 2 3 4)))))
        :rthm-seq-map '((1 ((vc (1))))))))
  (note-add-bracket-offset mini 1 1 'vc 
                           :dx -.1 :dy -.3 
                           :dx0 -.1 :dy0 -.4 
                           :dx1 .3 :dy1 -.1))

=> (1 3 -0.1 -0.3 -0.1 -0.4 0.3 -0.1)

|#
;;; SYNOPSIS
(defmethod note-add-bracket-offset ((sc slippery-chicken)
                                    bar-num note-num player
                                    &key (dx nil) (dy nil) 
                                    (dx0 nil) (dy0 nil) 
                                    (dx1 nil) (dy1 nil) 
                                    (index 0))
;;; ****
  (let ((event (get-note (piece sc) bar-num note-num player)))
    (add-bracket-offset event :dx dx :dy dy :dx0 dx0 :dy0 dy0 :dx1 dx1 :dy1 dy1
                        :index index)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Similar to above but whereas you usually give the notes like '((1 1) (2 2))
;;; meaning bar 1 note 1, bar 2 note 2, here you give it in the form '((1 1 5)
;;; (3 2 7)) meaning bar 1, notes 1 to 5 inclusive, bar 3, notes 2 to 7
;;; inclusive.  NB This method was used in the days when cmn marks were added
;;; explicity by calling cmn mark functions; best use add-marks-to-notes these
;;; days. 

(defmethod add-mark-to-notes-from-to ((sc slippery-chicken)
                                      mark-function player
                                      notes)
  (loop 
     for bar in notes 
     for bar-num-or-ref = (first bar)
     for notes = (rest bar)
     with expansion
     do
       (unless (= (length notes) 2)
         (error "slippery-chicken::add-mark-to-notes-from-to: ~
                Each bar is a 3 note list: bar-num start-note end-note: ~a"
                bar))
       (setf expansion (loop for i from (first notes) to (second notes) 
                          collect i))
       (loop for n in expansion do
            (add-mark-to-note sc bar-num-or-ref n player 
                              (funcall mark-function))))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Thu Apr 19 16:24:43 BST 2012: Added robodoc entry

;;; MDE original comment:
;;; 1.3.11 another method for adding marks to multiple notes, this time we give
;;; a start-bar/note and an end-bar/note and the given marks will be added to
;;; all in between. start and finish are inclusive and 1-based.  If they're
;;; integers then all notes in the bars will be marked, otherwise a 2-element
;;; list sets the exact note to start/stop at.  
;;;
;;; NB note-heads need before to be t in LilyPond but bear in mind they're
;;;    automatically moved over in event::get-lp-data.  players can be a single
;;;    symbol or list.

;;; ****m* slippery-chicken-edit/add-marks-to-notes
;;; DESCRIPTION
;;; Add the specified mark or marks to a consecutive sequence of multiple notes
;;; within the given slippery-chicken object.
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; - An integer or a list consisting of two numbers to indicate the start
;;;   bar/note. If this is an integer, all notes in this bar will receive the
;;;   specified mark or marks. If this is a two-number list, the first number
;;;   determines the bar, the second the note within that bar.
;;; - An integer or a list consisting of two numbers to indicate the end
;;;   bar/note. If this is an integer, all notes in this bar will receive the
;;;   specified mark or marks. If this is a two-number list, the first number
;;;   determines the bar, the second the note within that bar.
;;; - The ID of the player or players to whose parts the mark or marks should
;;;   be attached. This can be a single symbol or a list.
;;; - T or NIL to indicate whether the mark should be added to the MARKS slot
;;;   or the MARKS-BEFORE slot of the given events objects.
;;; - The mark or marks to be added.
;;;
;;; RETURN VALUE
;;; Returns T.
;;; 
;;; EXAMPLE
#|
;;; This example calls the method twice: Once using the single-integer
;;; indication for full bars, with one instrument and one mark; and once using
;;; the bar/note reference lists for more specific placement, a list of several
;;; players that should all receive the marks, and multiple marks to add.
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((vn (violin :midi-channel 1))
                     (va (viola :midi-channel 2))))
        :tempo-map '((1 (q 60)))
        :set-palette '((1 ((c4 d4 f4 g4 a4 c5 d5 f5))))
        :set-map '((1 (1 1 1 1 1 1)))
        :rthm-seq-palette '((1 ((((4 4) e e e e e e e e))
                                :pitch-seq-palette ((1 2 3 4 5 6 7 8)))))
        :rthm-seq-map '((1 ((vn (1 1 1 1 1 1))
                            (va (1 1 1 1 1 1))))))))
  (add-marks-to-notes mini 2 3 'vn nil 'lhp)
  (add-marks-to-notes mini '(1 3) '(2 2) '(vn va) nil 's 'a))

=> T

|#
;;; SYNOPSIS
(defmethod add-marks-to-notes ((sc slippery-chicken) start end players before
                                   &rest marks)
;;; ****
  (let* ((stlist (listp start))
         (ndlist (listp end))
         (stbar (if stlist (first start) start))
         (ndbar (if ndlist (first end) end))
         (stnote (if stlist (second start) 1))
         ;; NB this means we'll add marks to tied notes too
         (ndnote (when ndlist (second end)))) ; nil processed in do-bar
    (unless (listp players)
      (setf players (list players)))
    (flet ((do-bar (bar-num start-note end-note)
             (loop for p in players do
                  (unless end-note
                    (setf end-note (num-score-notes (get-bar sc bar-num p))))
                  (loop for i from start-note to end-note do
                       (loop for m in marks do
                            (funcall (if before 
                                         #'add-mark-before-note
                                         #'add-mark-to-note)
                                     sc bar-num i p m))))))
      (if (= stbar ndbar)
          (do-bar stbar stnote ndnote)
          (progn 
            (do-bar stbar stnote nil)
            (do-bar ndbar 1 ndnote)
            ;; now the bars in the middle
            (loop for bnum from (1+ stbar) to (1- ndbar) do
                 (do-bar bnum 1 nil))))))
  t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SAR Thu Apr 19 14:05:30 BST 2012: Added robodoc entry

;;; 27.6.11: Yet another method for adding marks to notes, this one allowing
;;; 'shorthand' and a very free specification of what goes where.

;;; player-data is something like
;;;     '((flute al 1 1 1 2 5 1 wt 5 2 h 6 1 al 6 2 6 3 sk 9 1 9 2 9 3 ...)
;;;       (clarinet sl 5 1 sarr 6 1 end 7 1 sl 12 2 sl 16 1 18 1 18 2 21 1 ...))
;;; where the first element of the list is the player name that we'll add marks
;;; for.  From then on we have <mark bar-number note-number> triplets, or if a
;;; mark should be added repeatedly then <mark bar note bar note ...>.  A mark
;;; can be a string or a symbol, and if the latter, instead of typing out
;;; recognised marks e.g. short-pause you can supply a shorthand table that
;;; will translate e.g. sp to be short-pause.  This table is a simple lisp
;;; association list e.g.  '((al aeolian-light)
;;;                         (ad aeolian-dark)
;;;                         (wt "WT")
;;;                         (h harm))

;;; ****m* slippery-chicken-edit/add-marks-sh
;;; DATE
;;; 27-Jun-2011
;;;
;;; DESCRIPTION
;;; Add marks in a somewhat more free list form, with the option of
;;; implementing a user-defined shorthand.
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; - A list of lists containing the players, bar and note refs, and marks to
;;;   be added. The first element of each contained list will be the ID of the
;;;   player to whose part the marks are to be added followed by a pattern of
;;;   <mark bar-number note-number> triplets, or if a mark is to be added
;;;   repeatedly then <mark bar note bar note... >. A mark can be a string or a
;;;   symbol. 
;;;
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - For marks given as symbols, the user can supply a shorthand table that
;;;   will expand an abbreviation, such as sp, to the full mark name, such as
;;;   short-pause. This table takes the form of a simple Lisp association list,
;;;   e.g.: '((al aeolian-light)
;;;           (ad aeolian-dark)
;;;           (wt "WT")
;;;           (h harm))
;;; - :warn. T or NIL to indicate whether to print a warning for unrecognized
;;;   marks. T = print warning. Default = T.
;;; - :verbose. T or NIL to indicate whether the method is to print verbose
;;;   feedback about each mark added to the Listener. T = print feedback. 
;;;   Default = NIL.
;;; 
;;; 
;;; RETURN VALUE
;;; Returns NIL.
;;; 
;;; EXAMPLE
#|
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((vn (violin :midi-channel 1))
                     (va (viola :midi-channel 2))))
        :tempo-map '((1 (q 60)))
        :set-palette '((1 ((c4 d4 f4 g4 a4 c5 d5 f5))))
        :set-map '((1 (1 1 1 1 1 1)))
        :rthm-seq-palette '((1 ((((4 4) e e e e e e e e))
                                :pitch-seq-palette ((1 2 3 4 5 6 7 8)))))
        :rthm-seq-map '((1 ((vn (1 1 1 1 1 1))
                            (va (1 1 1 1 1 1))))))))
  (add-marks-sh mini
                '((vn a 1 1 1 2 3 1 s 2 1 2 2 2 5)
                  (va pizz 1 3 2 3 sp 3 1))
                :shorthand '((sp short-pause))
                :verbose t))

=> NIL


|#
;;; SYNOPSIS
(defmethod add-marks-sh ((sc slippery-chicken) player-data 
                         &key shorthand (warn t) verbose)
;;; ****
  (loop for player in player-data
     for p = (first player) do
     (loop with bar with note with mark
        for datum in (rest player) do
        (cond ((symbolp datum)
               (let ((val (second (assoc datum shorthand))))
                 (setf mark (if val 
                                val
                                (progn
                                  (when (and shorthand warn)
                                    (warn "slippery-chicken::add-marks: ~
                                           using non-shorthand mark ~%on ~
                                           the assumption that it's a ~
                                           normal mark: ~a" datum))
                                  datum))
                       bar nil
                       note nil)))
              ((stringp datum)
               (setf mark datum
                     bar nil 
                     note nil))
              (bar (setf note datum))
              (t (setf bar datum)))
        (when (and mark bar note)
          (when verbose
            (format t "~%---add-marks: ~a at bar ~a, note ~a" mark bar note))
          ;; (acmtn p bar note mark)
          (add-marks-to-note sc bar note p mark)
          (setf bar nil
                note nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken-edit/add-marks-to-note
;;; DESCRIPTION
;;; Add one or more specified marks to a specified note within a given
;;; slippery-chicken object.
;;;
;;; NB: This method counts notes, not events; i.e., rests are not counted.
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; - An integer that is the bar number to which the mark or marks should to be
;;;   added. 
;;; - An integer that is the note within the specified bar to which the mark or
;;;   marks should be added.
;;; - The ID of the player to whose part the mark or marks should be added.
;;; - The mark or marks to add.
;;; 
;;; RETURN VALUE
;;; Returns T.
;;; 
;;; EXAMPLE
#|
;;; Add several marks to one note, then print the corresponding MARKS slot to
;;; see the difference.
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((vn (violin :midi-channel 1))
                     (va (viola :midi-channel 2))))
        :tempo-map '((1 (q 60)))
        :set-palette '((1 ((c4 d4 f4 g4 a4 c5 d5 f5))))
        :set-map '((1 (1 1 1 1 1 1)))
        :rthm-seq-palette '((1 ((((4 4) e (e) e e (e) e e e))
                                :pitch-seq-palette ((1 2 3 4 5 6)))))
        :rthm-seq-map '((1 ((vn (1 1 1 1 1 1))
                            (va (1 1 1 1 1 1))))))))
  (add-marks-to-note mini 2 3 'va 'a 's 'lhp 'pizz)
  (print (marks (get-note mini 2 3 'va))))

=> (PIZZ LHP S A)

|#
;;; SYNOPSIS
(defmethod add-marks-to-note ((sc slippery-chicken) bar-num note-num
                                  player &rest marks)
;;; ****
  ;; make sure we have a flat list and no sublists as perhaps created by
  ;; cmn::get-marks 
  (setf marks (flatten marks))
  ;; (print marks)
  (let ((note (get-note (piece sc) bar-num note-num player)))
    (when note
      (loop for mark in marks do
            (add-mark note mark))))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sun Apr 22 12:16:46 BST 2012: Added robodoc entry

;;; ****m* slippery-chicken-edit/rm-marks-from-note
;;; DESCRIPTION
;;; Remove one or more specific marks from the MARKS slot of a specified event
;;; object.
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; - An integer that is the number of the bar from which the marks are to be
;;;   removed. 
;;; - An integer that is the number of the note in that bar from which the
;;;   marks are to be removed.
;;; - The ID of the player from whose part the marks are to be removed.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - A specific mark or list of specific marks that are to be removed. If this
;;;   argument is not specified, no marks will be removed.
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
        :tempo-map '((1 (q 60)))
        :set-palette '((1 ((cs4 ds4 fs4))))
        :set-map '((1 (1 1 1 1)))
        :rthm-seq-palette '((1 ((((2 4) q e s s))
                                :pitch-seq-palette ((1 2 3 4))
                                :marks (a 2 s 2 fff 2 pizz 2))))
        :rthm-seq-map '((1 ((vn (1 1 1 1))))))))
  (rm-marks-from-note mini 2 2 'vn 'pizz)
  (rm-marks-from-note mini 3 2 'vn '(pizz fff))
  (rm-marks-from-note mini 3 2 'vn))

=> T

|#
;;; SYNOPSIS
(defmethod rm-marks-from-note ((sc slippery-chicken) bar-num note-num
                               player &rest marks)
;;; ****
  ;; make sure we have a flat list and no sublists as perhaps created by
  ;; cmn::get-marks 
  (setf marks (flatten marks))
  (let ((note (get-note (piece sc) bar-num note-num player)))
    (when note
      ;; 21.9.11 no warning here
      (rm-marks note marks nil)))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sun Apr 22 12:28:28 BST 2012: Added robodoc entry

;;; ****m* slippery-chicken-edit/rm-marks-from-notes
;;; DESCRIPTION
;;; Remove only the specified marks from the MARKS slots of specified events in
;;; the parts of specified players. If the <players> argument is set to NIL,
;;; remove the mark or marks from all players.
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; - An integer or a 2-item list of integers indicating the first bar and note
;;;   from which to remove marks. If an integer, this is the bar number and the
;;;   mark will be removed from all notes in the bar. If a 2-item list, this is
;;;   a reference to the bar number and number of the first note in the bar
;;;   from which to start removing marks, in the form e.g. '(3 1).
;;; - An integer or a 2-item list of integers indicating the last bar and note
;;;   from which to remove marks. If an integer, this is the bar number and the
;;;   mark will be removed from all notes in the bar. If this is a 2-item list,
;;;   this is a reference to the bar number and number of the first note in the
;;;   bar from which to start removing marks, in the form e.g. '(3 1).
;;; - The ID or a list of IDs of the players from whose parts the marks are to
;;;   be removed.
;;; 
;;; OPTIONAL ARGUMENTS
;;; NB: The <marks> argument is a required argument for this method.
;;; - The mark or a list of the marks to remove. This method will only remove
;;;   specified marks.
;;; 
;;; RETURN VALUE
;;; Returns T.
;;; 
;;; EXAMPLE
#|
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((fl (flute :midi-channel 1))
                     (hn (french-horn :midi-channel 2))
                     (vn (violin :midi-channel 3))))
        :tempo-map '((1 (q 60)))
        :set-palette '((1 ((cs4 ds4 fs4))))
        :set-map '((1 (1 1 1 1 1)))
        :rthm-seq-palette '((1 ((((2 4) q e s s))
                                :pitch-seq-palette ((1 2 3 4))
                                :marks (a 2 s 2 fff 2))))
        :rthm-seq-map '((1 ((fl (1 1 1 1 1))
                            (hn (1 1 1 1 1))
                            (vn (1 1 1 1 1))))))))
  (rm-marks-from-notes mini 1 2 'fl 'fff)
  (rm-marks-from-notes mini '(1 2) '(2 1) 'hn '(fff a))
  (rm-marks-from-notes mini 3 '(4 3) '(hn vn) '(fff s a))
  (rm-marks-from-notes mini 5 5 nil 'fff))

=> T

|#
;;; SYNOPSIS
(defmethod rm-marks-from-notes ((sc slippery-chicken) start end
                                    players &rest marks)
;;; ****
  (unless players
    (setf players (players sc)))
  (unless (listp players)
    (setf players (list players)))
  (let* ((stlist (listp start))
         (ndlist (listp end))
         (stbar (if stlist (first start) start))
         (ndbar (if ndlist (first end) end))
         (stnote (if stlist (second start) 1))
         (ndnote (when ndlist (second end)))) ; nil processed in do-bar
    (flet ((do-bar (bar-num start-note end-note)
             (loop for player in players do
                  (unless end-note
                    (setf end-note (num-score-notes
                                    (get-bar sc bar-num player))))
                  (loop for i from start-note to end-note do
                       (rm-marks-from-note
                        sc bar-num i player marks)))))
      (if (= stbar ndbar)
          (do-bar stbar stnote ndnote)
          (progn 
            (do-bar stbar stnote nil)
            (do-bar ndbar 1 ndnote)
            ;; now the bars in the middle
            (loop for bnum from (1+ stbar) to (1- ndbar) do
                 (do-bar bnum 1 nil))))))
  t)
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sun Apr 22 15:21:30 BST 2012: Added robodoc entry

;;; ****m* slippery-chicken-edit/rm-slurs
;;; DESCRIPTION
;;; Remove the specified slurs from the MARKS slots of specified events in
;;; the parts of specified players. If the <players> argument is set to NIL,
;;; remove the specified slurs from all players.
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; - An integer or a 2-item list of integers indicating the first bar and note
;;;   from which to remove slurs. If an integer, this is the bar number and the
;;;   slurs will be removed from all notes in the bar. If a 2-item list, this is
;;;   a reference to the bar number and number of the first note in the bar
;;;   from which to start removing slurs, in the form e.g. '(3 1).
;;; - An integer or a 2-item list of integers indicating the last bar and note
;;;   from which to remove slurs. If an integer, this is the bar number and the
;;;   slurs will be removed from all notes in the bar. If this is a 2-item list,
;;;   this is a reference to the bar number and number of the first note in the
;;;   bar from which to start removing slurs, in the form e.g. '(3 1).
;;; - The ID or a list of IDs of the players from whose parts the marks are to
;;;   be removed.
;;; 
;;; RETURN VALUE
;;; Returns T.
;;; 
;;; EXAMPLE
#|
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((fl (flute :midi-channel 1))
                     (hn (french-horn :midi-channel 2))
                     (vn (violin :midi-channel 3))))
        :tempo-map '((1 (q 60)))
        :set-palette '((1 ((c4 d4 e4 fs4 gs4 as4 c5 d5))))
        :set-map '((1 (1 1 1 1 1)))
        :rthm-seq-palette '((1 ((((4 4) e e e e e e e e))
                                :pitch-seq-palette ((1 2 3 4 5 6 7 8))
                                :marks (slur 1 2 slur 3 4 slur 5 6 slur 7 8))))
        :rthm-seq-map '((1 ((fl (1 1 1 1 1))
                            (hn (1 1 1 1 1))
                            (vn (1 1 1 1 1))))))))
  (rm-slurs mini 1 2 'fl)
  (rm-slurs mini '(1 3) '(2 1) 'hn)
  (rm-slurs mini 3 '(4 3) '(hn vn))
  (rm-slurs mini 5 5 nil))

=> T

|#
;;; SYNOPSIS
(defmethod rm-slurs ((sc slippery-chicken) start end players)
;;; ****
  (rm-marks-from-notes sc start end players '(beg-sl end-sl)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Thu Apr 19 13:08:23 BST 2012: Added robodoc entry

;;; ****m* slippery-chicken-edit/add-mark-before-note
;;; DESCRIPTION
;;; Add the specified mark to the MARKS-BEFORE slot of the specified note
;;; object within the given slippery-chicken object.
;;;
;;; NB: This method counts notes, not events; i.e., rests are not counted.
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; - An integer that is the bar number in which the mark is to be added.
;;; - An integer that is the NOTE number to which the mark is to be added (not
;;;   the event number; i.e., rests are not counted).
;;; - The ID of the player to which the mark is to be added.
;;; - The mark to be added.
;;; 
;;; RETURN VALUE
;;; Returns the new value of the MARKS-BEFORE slot of the given event object. 
;;; 
;;; EXAMPLE
#|

;;; The method adds the mark to the specified note, not event. Add the mark to
;;; note 2, print the MARKS-BEFORE slots of events 2 (which is a rest) and 3.
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((vn (violin :midi-channel 1))))
        :tempo-map '((1 (q 60)))
        :set-palette '((1 ((c4 d4 f4 g4 a4 c5 d5 f5 g5 a5 c6))))
        :set-map '((1 (1 1 1 1 1 1)))
        :rthm-seq-palette '((1 ((((2 4) q (e) s s))
                                :pitch-seq-palette ((1 2 3)))))
        :rthm-seq-map '((1 ((vn (1 1 1 1 1 1))))))))
  (add-mark-before-note mini 3 2 'vn 'ppp)
  (print (marks-before (get-event mini 3 2 'vn)))
  (print (marks-before (get-event mini 3 3 'vn))))

=>
NIL 
(PPP)

|#
;;; SYNOPSIS
(defmethod add-mark-before-note ((sc slippery-chicken)
                                       bar-num note-num player mark)
;;; ****
  (add-mark-before-note (piece sc) bar-num note-num player mark))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Wed Apr 25 14:31:35 BST 2012: Added robodoc entry

;;; ****m* slippery-chicken-edit/sc-delete-marks
;;; DESCRIPTION
;;; Delete all marks from the MARKS slot of a given note event object and
;;; set the slot to NIL.
;;;
;;; NB: This method counts notes, not rests, and is 1-based.
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; - An integer that is the number of the bar in which the marks are to be
;;;   deleted. 
;;; - An integer that is the number of the note from which the marks are to be
;;;   deleted.
;;; - The ID of the player from whose part the marks are to be deleted.
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
        :set-palette '((1 ((cs4 ds4 fs4))))
        :set-map '((1 (1 1 1 1)))
        :rthm-seq-palette '((1 ((((2 4) q (e) s s))
                                :pitch-seq-palette ((1 2 3))
                                :marks (a 2 s 2 fff 2 pizz 2))))
        :rthm-seq-map '((1 ((vn (1 1 1 1))))))))
  (sc-delete-marks mini 2 2 'vn))

=> T

|#
;;; SYNOPSIS
(defmethod sc-delete-marks ((sc slippery-chicken) bar-num note-num player)
;;; ****
  (bh-delete-marks (piece sc) bar-num note-num player))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Wed Apr 25 15:56:45 BST 2012: Added robodoc entry

;;; ****m* slippery-chicken-edit/sc-delete-marks-from-event
;;; DESCRIPTION
;;; Delete all data from the MARKS slot of the specified event object and
;;; replace it with NIL.
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; - An integer that is the number of the bar from which the marks are to be
;;;   deleted.
;;; - An integer that is the number of the event within the given bar from
;;;   which the marks are to be deleted.
;;; - The ID of the player from whose part the marks are to be deleted.
;;; 
;;; RETURN VALUE
;;; Returns NIL.
;;; 
;;; EXAMPLE
#|

(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((vc (cello :midi-channel 1))))
        :tempo-map '((1 (q 60)))
        :set-palette '((1 ((d3 e3 f3 g3 a3 b3 c4 e4))))
        :set-map '((1 (1 1 1)))
        :rthm-seq-palette '((1 ((((2 4) q e s s))
                                :pitch-seq-palette ((1 2 3 4))
                                :marks (a 1 4 lhp 4 s 3 4 slur 1 2))))
        :rthm-seq-map '((1 ((vc (1 1 1))))))))
  (sc-delete-marks-from-event mini 2 4 'vc))

=> NIL

|#
;;; SYNOPSIS
(defmethod sc-delete-marks-from-event ((sc slippery-chicken)
                                           bar-num event-num player)
;;; ****
  (setf (marks (get-event sc bar-num event-num player)) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Wed Apr 25 15:24:22 BST 2012: Added robodoc entry

;;; ****m* slippery-chicken-edit/sc-delete-marks-before
;;; DESCRIPTION
;;; Deletes all data from the MARKS-BEFORE slot of a specified event object and
;;; replaces it with NIL.
;;;
;;; NB: In addition to clef symbol data, the MARKS-BEFORE slot also stores part
;;;     of the required data for trills and arrows. Deleting just the
;;;     MARKS-BEFORE components of those markings may result in unwanted
;;;     results. 
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; - An integer that is the number of the bar in which the event object is to
;;;   be  modified. 
;;; - An integer that is the number of the note within the given bar for which
;;;   the MARKS-BEFORE slot is to be set to NIL.
;;; - The ID of the player whose part is to be affected.
;;; 
;;; RETURN VALUE
;;; Returns NIL.
;;; 
;;; EXAMPLE
#|

(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((vc (cello :midi-channel 1))))
        :tempo-map '((1 (q 60)))
        :set-palette '((1 ((d3 e3 f3 g3 a3 b3 c4 e4))))
        :set-map '((1 (1 1 1)))
        :rthm-seq-palette '((1 ((((2 4) q e s s))
                                :pitch-seq-palette ((1 2 3 4)))))
        :rthm-seq-map '((1 ((vc (1 1 1))))))))
  (add-mark-before-note mini 2 3 'vc 'fff)
  (add-mark-before-note mini 2 3 'vc 's)
  (add-mark-before-note mini 2 3 'vc 'lhp)
  (sc-delete-marks-before mini 2 3 'vc))

=> NIL

|#
;;; SYNOPSIS
(defmethod sc-delete-marks-before ((sc slippery-chicken)
                                         bar-num note-num player)
;;; ****
  (delete-marks-before (piece sc) bar-num note-num player))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Thu Apr 26 12:58:49 BST 2012: Added robodoc entry
;;; SAR Wed May  2 13:39:41 BST 2012: Added comment about handle-ties to the
;;; robodoc. 

;;; ****m* slippery-chicken-edit/tie
;;; DESCRIPTION
;;; Add a tie to a specified event object. The new tie will be placed starting
;;; from the specified event object and spanning to the next event object. If
;;; the next event object does not have the same pitch, its pitch will be
;;; changed to that of the first event object.
;;;
;;; An optional argument allows the user to adjust the steepness of the tie's
;;; curvature. 
;;;
;;; NB: This method will not automatically update ties in MIDI output. To make
;;;     sure that MIDI ties are also updated, use the handle-ties method.
;;;
;;; NB: If the next event object is a rest and not a note, an error will be
;;;     produced.
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; - An integer that is the number of the bar in which the tie is to be
;;;   placed. 
;;; - An integer that is the number of the note to which the tie is to be
;;;   attached. 
;;; - The ID of the player whose part is to be changed.
;;; 
;;; OPTIONAL ARGUMENTS 
;;; - A positive or negative decimal number to indicate the steepness of the
;;;   tie's curvature.
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
        :set-palette '((1 ((c4 d4 e4))))
        :set-map '((1 (1 1 1 1)))
        :rthm-seq-palette '((1 ((((2 4) q s s (s) s))
                                :pitch-seq-palette ((1 1 2 3)))))
        :rthm-seq-map '((1 ((vn (1 1 1 1))))))))
  (tie mini 2 1 'vn)
  (tie mini 3 2 'vn)
  (tie mini 4 2 'vn -.5))

=> T

|#
;;; SYNOPSIS
(defmethod tie ((sc slippery-chicken) bar-num note-num player 
                &optional curvature)
;;; ****
  (tie (piece sc) bar-num note-num player curvature))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Thu Apr 26 14:44:56 BST 2012: Added robodoc entry

;;; event numbers are 1-based 
;;; ****m* slippery-chicken-edit/trill
;;; DESCRIPTION
;;; Attach a trill mark to a specified event object by adding 'BEG-TRILL-A to
;;; the MARKS-BEFORE slot and TRILL-NOTE with the pitch to the MARKS slot. This
;;; method requires a specified trill pitch.
;;;
;;; By default trills are set to span from the specified note to the next note,
;;; though the length of the span can be specified using the optional
;;; arguments.
;;;
;;; NB: This is a LilyPond-only method and will not affect CMN output.
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; - The player to whose part the trill is to be added.
;;; - An integer that is the number of the bar in which the trill is to start. 
;;; - An integer that is the number of the event object in that bar on which
;;;   the trill is to be placed.
;;; - A note-name symbol that is the pitch of the trill note.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - An integer that is the number of the event object on which the trill span
;;;   is to stop.
;;; - An integer that is the number of the bar in which the trill span is to
;;;   stop. 
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
        :set-palette '((1 ((c4 d4 e4))))
        :set-map '((1 (1 1 1 1 1)))
        :rthm-seq-palette '((1 ((((2 4) q. s s))
                                :pitch-seq-palette ((1 3 2)))))
        :rthm-seq-map '((1 ((vn (1 1 1 1 1))))))))
  (trill mini 'vn 2 1 'e4)
  (trill mini 'vn 3 1 'e4 3)
  (trill mini 'vn 4 1 'e4 3 5))

=> T


|#
;;; SYNOPSIS
(defmethod trill ((sc slippery-chicken) player start-bar start-event trill-note
                  &optional end-event end-bar)
;;; ****
  (unless end-bar
    (setf end-bar start-bar))
  (unless end-event
    (setf end-event (1+ start-event)))
  (let ((start (get-event sc start-bar start-event player))
        (end (get-event sc end-bar end-event player)))
    (unless (and start end)
      (error "slippery-chicken::trill: invalid start/end event references: ~
              bar ~a event ~a to bar ~a event ~a"
             start-bar start-event end-bar end-event))
    (add-trill start trill-note t)
    (end-trill end)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Wed Apr 25 12:00:31 BST 2012: Added robodoc entry
;;; SAR Fri Jun  8 12:28:37 BST 2012: Expanded robodoc entry

;;; ****m* slippery-chicken-edit/tie-all-last-notes-over-rests
;;; DESCRIPTION
;;; Extend the duration of the last note of any bar that precedes a bar which
;;; starts with a rest in the specified region, such that the rest that begins
;;; the next measure is changed to a note and the last note of the first
;;; measure is tied to it.
;;;
;;; NB: This method will not automatically update ties in MIDI output. To make
;;;     sure that MIDI ties are also updated, use the handle-ties method.
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; - An integer that is the first bar in which changes are to be made.
;;; - An integer that is the last bar in which changes are to be made.
;;; - A player ID or list of player IDs.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :to-next-attack. T or NIL to indicate whether ties are to extend over
;;;   only full bars of rest or also over partial bars (until the next attacked
;;;   note). T = until the next attacked note. Default = T.
;;; - :tie-next-attack. T or NIL to indicate whether the new tied notes created 
;;;   should also be further extended over the next attacked note if that note
;;;   has the same pitch as the starting note of the tie. T = also tie next
;;;   attacked note if same pitch. Default = NIL.
;;; - :auto-beam. T or NIL to indicate whether the new events should be
;;;   automatically beamed after placement. T = automatically beam. 
;;;   Default = NIL.
;;; - :last-rhythm. NIL or a rhythmic duration. If a rhythmic duration, the
;;;   last duration of the tie will be forced to this length. Useful, for
;;;   example, when tying into a rest bar without filling that whole
;;;   bar. NIL = fill the bar with a tied note. Default = NIL.
;;; - :update. T or NIL to indicate whether all slots for all events in the
;;;   piece should be updated. This is an expensive operation so set to NIL if
;;;   you plan on calling other similar methds, and call (update-slots sc)
;;;   explicitly at the end. Default = T.
;;; 
;;; RETURN VALUE
;;; Returns NIL.
;;; 
;;; EXAMPLE
#|
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((vn (violin :midi-channel 1))
                     (va (viola :midi-channel 2))
                     (vc (cello :midi-channel 3))))
        :set-palette '((1 ((f3 g3 a3 b3 c4 d4 f4 g4 a4 c5 d5 f5))))
        :set-map '((1 (1 1 1)))
        :rthm-seq-palette '((1 ((((4 4) e (e) e e (e) (e) e e) 
                                 ((w)) 
                                 ((h.) q) 
                                 ((w))
                                 ((w)) 
                                 ((e) e h.))
                                :pitch-seq-palette ((1 2 3 4 5 6 7 7)))))
        :rthm-seq-map '((1 ((vn (1 1 1))
                            (va (1 1 1))
                            (vc (1 1 1))))))))
  (tie-all-last-notes-over-rests mini 2 6 'vn)
  (tie-all-last-notes-over-rests mini 9 12 'vn :auto-beam t)
  (tie-all-last-notes-over-rests mini 3 5 '(va vc) :to-next-attack nil)
  (tie-all-last-notes-over-rests mini 9 12 'vc :tie-next-attack t)
  (tie-all-last-notes-over-rests mini 13 15 'vn :last-rhythm 'e))


=> NIL

|#
;;; SYNOPSIS
(defmethod tie-all-last-notes-over-rests
    ((sc slippery-chicken)
     start-bar end-bar players
     &key
       ;; use up all rests until next attack or (if nil) 
       ;; just the rest bars? 
       (to-next-attack t)
       ;; if the next attack is the same note/chord as
       ;; the previous, tie to it too? 
       (tie-next-attack nil)
       (last-rhythm nil)
       (update t)
       (auto-beam nil))
;;; ****               
  (unless (listp players)
    (setf players (list players)))
  #| MDE Thu Apr 26 17:15:29 2012 -- update: allow this so we can tie _into_ the
  last bar but don't try and tie from it in tie-over-rest-bars-aux 
  ;; MDE Wed Apr 25 16:25:21 2012 -- 
  (when (= end-bar (num-bars sc))
  (error "slippery-chicken::tie-all-last-notes-over-rests: ~
            This method cannot be used ~%on the last bar of the piece."))
  |#
  (loop for p in players do
        (tie-all-last-notes-over-rests-aux sc start-bar end-bar p 
                               :to-next-attack to-next-attack 
                               :tie-next-attack tie-next-attack
                               :last-rhythm last-rhythm
                               :auto-beam auto-beam))
  ;; MDE Sat Jun 28 14:44:23 2014 
  (when update
    (update-slots sc))
  ;; MDE Fri Jun  8 16:08:15 2012 -- just make sure we get a useful error
  ;; message if there's a problem 
  (check-ties sc nil #'warn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Fri Apr 27 11:46:14 BST 2012: Added robodoc entry
;;; SAR Fri Jun  8 12:30:22 BST 2012: Expanded robodoc entry

;;; ****m* slippery-chicken-edit/tie-over-rest-bars
;;; DESCRIPTION
;;; Extend the duration of the last note in a specified bar by changing
;;; immediately subsequent full-rest bars to notes of the same pitch and tying
;;; them to that note.
;;;
;;; NB: This method will not automatically update ties in MIDI output. To make
;;;     sure that MIDI ties are also updated, use the handle-ties method.
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; - An integer that is the number of the bar in which the last note is to be
;;;   tied. 
;;; - An ID or list of IDs of the players whose parts are to be modified.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :end-bar. An integer or NIL. If an integer, this is the number of the
;;;   last bar of full-rests that is to be changed to a note. This can be
;;;   helpful for tying into passages of multiple bars of full-rest.
;;; - :tie-next-attack. T or NIL to indicate whether the new tied notes created 
;;;   should also be further extended over the next attacked note if that note
;;;   has the same pitch as the starting note of the tie. T = also tie next
;;;   attacked note if same pitch. Default = NIL.
;;; - :to-next-attack. T or NIL to indicate whether ties are to extend over
;;;   only full bars of rest or also over partial bars (until the next attacked
;;;   note). T = until the next attacked note. Default = T.
;;; - :auto-beam. T or NIL to indicate whether the method should automatically
;;;   place beams for the notes of the affected measure after the ties over
;;;   rests have been created. T = automatically beam. Default = NIL.
;;; - :last-rhythm. NIL or a rhythmic duration. If a rhythmic duration, the
;;;   last duration of the tie will be forced to this length. Useful, for
;;;   example, when tying into a rest bar without filling that whole
;;;   bar. NIL = fill the bar with a tied note. Default = NIL.
;;; - :update. T or NIL to indicate whether all slots for all events in the
;;;   piece should be updated. This is an expensive operation so set to NIL if
;;;   you plan on calling other similar methds, and call (update-slots sc)
;;;   explicitly at the end. Default = T.
;;; 
;;; RETURN VALUE
;;; Returns t.
;;; 
;;; EXAMPLE
#|
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((vn (violin :midi-channel 1))
                     (va (viola :midi-channel 2))
                     (vc (cello :midi-channel 3))))
        :set-palette '((1 ((c4 d4 e4))))
        :set-map '((1 (1 1)))
        :rthm-seq-palette '((1 ((((2 4) (q) e (s) s)
                                 ((h))
                                 ((s) e. e e)
                                 ((h))
                                 ((h))
                                 ((e) q s (s)))
                                :pitch-seq-palette ((1 2 2 1 3 3 1)))))
        :rthm-seq-map '((1 ((vn (1 1))
                            (va (1 1))
                            (vc (1 1))))))))
  (tie-over-rest-bars mini 1 'vn :end-bar 2)
  (tie-over-rest-bars mini 3 'va :end-bar 5)
  (tie-over-rest-bars mini 3 '(vn vc) :end-bar 6 :tie-next-attack t)
  (tie-over-rest-bars mini 7 'vc 
                      :end-bar 9
                      :to-next-attack t
                      :auto-beam t)
  (tie-over-rest-bars mini 9 'vn :end-bar 11 :last-rhythm 'e))

  => t

  |#
;;; SYNOPSIS
(defmethod tie-over-rest-bars ((sc slippery-chicken) bar-num players
                               &key (end-bar nil) ;; num of empty bars
                                 (tie-next-attack nil)
                                 (to-next-attack t)
                                 (last-rhythm nil)
                                 (update t)
                                 (auto-beam nil))
;;; ****
  (unless (listp players)
    (setf players (list players)))
  (loop for p in players do
       (tie-over-rest-bars-aux sc bar-num p 
                               :end-bar end-bar
                               :tie-next-attack tie-next-attack
                               :auto-beam auto-beam
                               :to-next-attack to-next-attack
                               :last-rhythm last-rhythm))
  (when update
    (update-slots sc))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Thu Apr 26 13:18:26 BST 2012: Added robodoc entry

;;; ****m* slippery-chicken-edit/tie-over-all-rests
;;; DESCRIPTION
;;; Extend the durations of all notes that immediately precede rests in the
;;; specified region by changing the rests to notes and tying the previous notes
;;; to them.
;;;
;;; NB: This method will not automatically update ties in MIDI output. To make
;;;     sure that MIDI ties are also updated, use the handle-ties method.
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; - The ID of the player whose part is to be changed, or a list of players or
;;;   NIL to indicate that we should process all players 
;;; - An integer that is the number of the first bar in which notes are to be
;;;   tied over rests.
;;; - An integer that is the number of the last bar in which notes are to be
;;;   tied over rests. NB: This argument does not necessarily indicate the bar
;;;   in which the ties will stop, but rather the last bar in which a tie will
;;;   be begun; the ties created may extend into the next bar.
;;;
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :start-note. An integer that is the number of the first attacked note
;;;   (not counting rests) in the given start-bar for which ties can be placed.
;;; - :end-note. An integer that is the number of the last attacked note (not
;;;   counting rests) in the given end-bar for which ties can be placed. 
;;;   NB: This argument does not necessarily indicate the note on which the
;;;   ties will stop, but rather the last not on which a tie can begin; the
;;;   ties created may extend to the next note.
;;; - :auto-beam. T or NIL to indicate whether the method should automatically
;;;   place beams for the notes of the affected measure after the ties over
;;;   rests have been created. T = automatically beam. Default = NIL.
;;; - :consolidate-notes. T or NIL to indicate whether the tied note are to be
;;;   consolidated into single rhythmic units of longer durations after the
;;;   ties over rests have been created. T = consolidate notes. Default = NIL. 
;;; - :update. T or NIL to indicate whether all slots for all events in the
;;;   piece should be updated. This is an expensive operation so set to NIL if
;;;   you plan on calling other similar methds, and call (update-slots sc)
;;;   explicitly at the end. Default = T.
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
        :set-palette '((1 ((c4 d4 e4))))
        :set-map '((1 (1 1 1 1 1 1 1)))
        :rthm-seq-palette '((1 ((((2 4) (q) e (s) s))
                                :pitch-seq-palette ((1 2)))))
        :rthm-seq-map '((1 ((vn (1 1 1 1 1 1 1))))))))
  (tie-over-all-rests mini 'vn 2 3 :start-note 2 :auto-beam t)
  (tie-over-all-rests mini 'vn 5 6 :end-note 1 :consolidate-notes t))

  => T

  |#
;;; SYNOPSIS
(defmethod tie-over-all-rests ((sc slippery-chicken) players
                               start-bar end-bar 
                               &key 
                                 (start-note 1)
                                 (end-note 9999999)
                                 (auto-beam nil)
                                 (update t)
                                 (consolidate-notes nil))
;;; ****
  ;; MDE Fri Sep 14 14:31:15 2018 -- make it possible for all players
  (setq players (if (not players)
                    (players sc)
                    (force-list players)))
  (unless end-bar (setq end-bar (num-bars sc)))
  (loop for player in players do
       (next-event sc player nil start-bar)
       (let ((refs '()))
         (loop 
            for bnum from start-bar to end-bar 
            for bar = (get-bar sc bnum player)
            ;; always one ahead
            with next-event = (progn 
                                (next-event sc player)
                                (next-event sc player))
            with note-num
            with event-num
            do
              (setf note-num (if (= bnum start-bar)
                                 (1- start-note)
                                 0))
              (setf ;;; SAR Wed Jul 11 14:12:28 BST 2012: commented this out:
               ;; note-num 0
               event-num 0)
              (loop 
                 while (< event-num (num-rhythms bar))
                 for event = (get-nth-event event-num bar)
                 do
                 ;; (format t "~&~a ~a" bnum note-num) SAR Wed Jul 18 12:50:55
                 ;; BST 2012: Added this safety clause to allow users to specify
                 ;; ties into the last bar but not past the penultimate event.
                   (when (and (= bnum (num-bars sc))
                              (= event-num (1- (num-rhythms bar))))
                     (return))
                   (unless (is-rest event)
                     (incf note-num))
                   (when (and (not (is-rest event))
                              (is-rest next-event)
                              (or (> bnum start-bar)
                                  (>= note-num start-note))
                              (or (< bnum end-bar)
                                  (<= note-num end-note)))
                     (push (list bnum note-num) refs))
                   (incf event-num)
                   (setf next-event (next-event sc player))))
         ;; always do this starting with the highest bar num otherwise we the
         ;; refs get screwed up as we add notes (print refs)
         (loop for ref in refs do
              (tie-over-rests sc (first ref) (second ref) player 
                              :auto-beam auto-beam
                              :consolidate-notes consolidate-notes))))
  (when update
    (update-slots sc))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Fri Apr 27 11:23:11 BST 2012: Added robodoc entry

;;; MDE: 24.3.11: added end-bar

;;; ****m* slippery-chicken-edit/tie-over-rests
;;; DESCRIPTION
;;; Extend the duration of a specified note that precedes a rest by changing
;;; the rest to a note with the same pitch and adding a tie between them.
;;;
;;; NB: This method will not automatically update ties in MIDI output. To make
;;;     sure that MIDI ties are also updated, use the handle-ties method.
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; - An integer that is the number of the bar in which the note is located. 
;;; - An integer that is the number of the note within that bar which is to be
;;;   extended. This number is 1-based and also counts already tied notes.  If
;;;   NIL, then the last note in the bar will be used.
;;; - The ID of the player whose part is to be modified.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments
;;; - :end-bar. An integer that is the number of the last bar into which the
;;;   tie is to extend. This can be helpful if the user wants to tie into only
;;;   the first of several consecutive full-rest bars.
;;; - :auto-beam. T or NIL to indicate whether the method should automatically
;;;   beam the beats of the modified bars after the ties have been added.
;;;   T = automatically beam. Default = NIL.
;;; - :consolidate-notes. T or NIL to indicate whether the method should
;;;   consolidate tied notes into single rhythm units of longer duration.
;;;   T = consolidate. Default = T.
;;; - :update. T or NIL to indicate whether all slots for all events in the
;;;   piece should be updated. This is an expensive operation so set to NIL if
;;;   you plan on calling other similar methds, and call (update-slots sc)
;;;   explicitly at the end. Default = T.
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
        :set-palette '((1 ((c4 d4 e4))))
        :set-map '((1 (1 1 1)))
        :rthm-seq-palette '((1 ((((2 4) (q) e (s) s)
                                 ((h))
                                 ((s) e. (e) e)
                                 ((h))
                                 ((h))
                                 ((e) q s (s)))
                                :pitch-seq-palette ((1 2 2 3 3 1)))))
        :rthm-seq-map '((1 ((vn (1 1 1))))))))
  (tie-over-rests mini 1 2 'vn)
  (tie-over-rests mini 7 1 'vn)
  (tie-over-rests mini 9 2 'vn :end-bar 10)
  (tie-over-rests mini 13 1 'vn :auto-beam t :consolidate-notes nil))

  => T

  |#
;;; SYNOPSIS
(defmethod tie-over-rests ((sc slippery-chicken) bar-num note-num player
                           &key end-bar auto-beam (consolidate-notes t) 
                             (update t))
;;; **** 
  (let ((rsb (get-bar sc bar-num player)))
    ;; MDE Fri Aug 17 15:04:54 2012 -- this would mean we can't tie 
    ;; from the last note in a bar of all notes into the next bar of rests :/ 
    ;; MDE Mon Jul 16 18:10:02 2012 --  
    ;; (unless (zerop (num-rests rsb))
    ;; MDE Thu Aug 16 18:03:39 2012 -- auto-get last note if note-num is nil 
    (unless note-num 
      (setf note-num (num-score-notes rsb)))
    (next-event sc player nil bar-num)
    (if (not (get-note sc bar-num note-num player))
        ;; MDE Mon Jul 16 17:36:24 2012 -- error now warn ;
        (warn "tie-over-rests: can't get note ~a, bar ~a, ~a. Could be this ~
                 bar is a rest bar."
              note-num bar-num player)
        (let* ((start-event (loop 
                               for e = (next-event sc player)
                               with nn = 0
                               do
                                 (unless (is-rest e)
                                   (incf nn))
                                 (when (= nn note-num)
                                   (return e))))
               (porc (when (and start-event
                                (not (is-rest start-event)))
                       (clone (pitch-or-chord start-event))))
               (wporc (when (and start-event
                                 (written-pitch-or-chord start-event))
                        (clone (written-pitch-or-chord start-event))))
               (last-event start-event)
               (new-ties 0)
               (bar-nums (list bar-num)))
          ;; (print start-event) ;    ;
          ;; just check we're not already tied here and exit if we are ;
          ;; because we get ; some strange errors when we enter the wrong ;
          ;; data when calling this fun ; ;
          (when (is-tied-from start-event)
            (warn "slippery-chicken::tie-over-rests: already tied from! ~
                     Bar ~a, note ~a, ~a" bar-num note-num player))
          (setf (is-tied-from start-event) t)
          ;; remove any staccato or tenuto marks from this event ; ;
          (rm-marks start-event '(s as t) nil)
          (when porc
            (delete-marks porc))
          (when wporc
            (delete-marks wporc))
          ;; (print bar-num)
          ;; (print porc)
          (loop 
             for e = (next-event sc player)
             for bnum = (next-event sc nil)
             for bar = (get-bar sc bnum player)
             while (and (if end-bar (<= bnum end-bar) t)
                        e (is-rest e))
             do
             ;; (print (data e))      ;
             ;; keep track of the bars we've changed ;
               (pushnew bnum bar-nums)
               (if (is-rest-bar bar)
                   (let ((events (events-for-full-bar (get-time-sig bar) 
                                                      porc wporc)))
                     ;;(print (data porc)) ;
                     (incf new-ties (length events))
                     (setf last-event (first (last events))
                           (rhythms bar) events))
                   (progn
                     (incf new-ties)
                     (setf (pitch-or-chord e) (clone porc)
                           (written-pitch-or-chord e) (when wporc 
                                                        (clone wporc))
                           (is-rest e) nil
                           (needs-new-note e) nil
                           (is-tied-to e) t
                           (is-tied-from e) t
                           last-event e)))
               (gen-stats bar))
          ;; 3.3.11 if we just test for last-event we might screw up ties ;
          ;; despite the fact that we've done nothing.  MDE Mon Jul 16 ;
          ;; 17:17:02 2012 -- changed error to warn ;
          (when (zerop new-ties)
            (warn "slippery-chicken::tie-over-rests: no ties to make! Bar ~a, ~
                     note ~a, ~a" bar-num note-num player))      
          (when (> new-ties 0)
            (setf (is-tied-from last-event) nil))
          (when (and (> new-ties 0) (or auto-beam consolidate-notes))
            (loop 
               for bnum in bar-nums 
               for bar = (get-bar sc bnum player)
               do
                 (when consolidate-notes
                   (consolidate-notes bar nil auto-beam))
                 (when auto-beam
                   (auto-beam bar auto-beam nil))))
          (when update
            (update-slots sc)))))
  t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken-edit/tie-repeated-notes
;;; AUTHOR
;;; Daniel Ross (mr.danielross[at]gmail[dot]com) 
;;; 
;;; DATE
;;; Wed 18 Sep 2019 18:56:39 BST
;;; 
;;; DESCRIPTION
;;; Tie adjacent notes if they are the same pitch or chord.
;;; NB: this method works on pitches and chords
;;; 
;;; ARGUMENTS
;;; - a slippery chicken object
;;; - the first bar to start looking
;;; - the last bar to look in
;;; - the player or players to tie
;;; 
;;; OPTIONAL ARGUMENTS
;;; - :consolidate - call consolidate-all-notes at the end? Default = t 
;;; 
;;; RETURN VALUE
;;; A list of the total number of ties between notes per instrument
;;; 
;;; EXAMPLE
#|
(let* ((mini (make-slippery-chicken  
                '+mini+ 
                :ensemble '(((pno (piano :midi-channel 1))))
                :staff-groupings '(1)
                :tempo-map '((1 (q 60)))
                :set-palette '((set1 ((fs2 b2 d4 a4 d5 e5 a5 d6))) 
                               (set2 ((b2 fs3 d4 e4 a4 d5 e5 a5 d6))))
                :set-map '((1 (set1 set1 set2 set1 set1 set2)))
                :rthm-seq-palette
                '((seq1 ((((4 4) q (q) q q))   
                         :pitch-seq-palette (1 1 1)))  
                  (seq2 ((((4 4) (e) e q e (e) e e)) 
                         :pitch-seq-palette (1 1 1 (1) (1)))))
                :rthm-seq-map '((1 ((pno (seq1 seq1 seq2 seq1 seq1 seq2))))))))
      (tie-repeated-notes mini nil nil nil))

=> (12)
|#
;;; SYNOPSIS
(defmethod tie-repeated-notes ((sc slippery-chicken) start-bar end-bar players
                               &key (consolidate t) (check-ties? t))
;;; ****
  (unless players (setf players (players sc)))
  (unless start-bar (setf start-bar 1))
  (unless end-bar (setf end-bar (num-bars sc)))
  (setf players (force-list players))
  (let ((count-list '()))
    (loop for player in players do
         (next-event sc player nil start-bar)
         (loop for ne = (next-event sc player nil nil end-bar)
            with le
            with count = 0
            while ne
            do
              (when le
                (unless (or (is-rest ne)
                            (is-rest le))
                  (when (pitch-or-chord= (pitch-or-chord ne)
                                         (pitch-or-chord le))
                    (setf (is-tied-from le) t
                          (is-tied-to ne) t
                          count (incf count)))))
              (setf le ne)
            finally
              (push count count-list))
       finally
         (when consolidate
           (consolidate-all-notes sc start-bar end-bar players))
         (update-slots sc))
    (when check-ties?
      (check-ties sc t))
    count-list))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken-edit/delete-slur
;;; DESCRIPTION
;;; Delete a slur mark that starts on a specified note within a specified bar
;;; of a specified player's part by deleting the BEG-SL and END-SL marks from
;;; the corresponding event objects.
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; - An integer that is the number of the bar from which the slur is to be
;;;   deleted. 
;;; - An integer that is the number of the note on which the slur to be deleted
;;;   starts within the given bar. This number counts tied-notes but not
;;;   rests. 
;;; - The ID of the player from whose part the slur is to be deleted.
;;; 
;;; RETURN VALUE
;;; Returns NIL.
;;; 
;;; EXAMPLE
#|
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((vc (cello :midi-channel 1))))
        :tempo-map '((1 (q 60)))
        :set-palette '((1 ((c2 e2 d4 e4 f4 g4 a4 f5))))
        :set-map '((1 (1 1 1 1 1 1)))
        :rthm-seq-palette '((1 ((((4 4) e e e e e e e e))
                                :pitch-seq-palette ((1 2 3 4 5 6 7 8))
                                :marks (slur 1 8))))
        :rthm-seq-map '((1 ((vc (1 1 1 1 1 1))))))))
  (delete-slur mini 1 1 'vc)
  (delete-slur mini 3 1 'vc))

  => NIL

  |#
;;; SYNOPSIS
(defmethod delete-slur ((sc slippery-chicken) bar-num note-num player)
;;; ****
  (let ((event (next-event sc player nil bar-num))
        (enum 0) 
        (in-slur nil)
        (happy t))
    (loop while happy do
         (setf event (next-event sc player))
         (if event
             (progn
               (unless (is-rest event)
                 (incf enum))
               (if (= enum note-num)
                   (if (begin-slur-p event) 
                       (progn
                         (setf in-slur t
                               ;; do this so the same event number in the next
                               ;; bars doesn't trigger this clause
                               note-num -1)
                         ;; (print-simple event)
                         (rm-marks event 'beg-sl))
                       (warn "slippery-chicken::delete-slur (~a): no slur to ~
                              delete: ~a" player event))
                   (when (and in-slur (end-slur-p event))
                     (setf happy nil)
                     ;; (print-simple event)
                     (rm-marks event 'end-sl))))
             (setf happy nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;  SAR Thu Jun  7 14:06:13 BST 2012: Added robooc entry

;;; add slurs automatically (to wind instruments usually) to phrases: these are
;;; defined as not having any rests in them and not including any repeated
;;; notes. 

;;; ****m* slippery-chicken-edit/auto-slur
;;; DESCRIPTION
;;; Automatically add slurs to note groups in the specified measure range of a
;;; given player's part. 
;;;
;;; This method places slurs above all consecutive notes between rests. If a
;;; value is specified for :end-bar and the last event in the end bar is not a
;;; rest, the final sequence of attacked notes in that bar will not be
;;; slurred. 
;;;
;;; NB: Slurs will automatically stop at repeated pitches.  Staccato marks will
;;; not stop the auto-slurring process but staccatos can be removed (see
;;; below). 
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; - A player ID or list of player IDs for the parts in which the slurs are to
;;;   be placed.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :start-bar. An integer that is the first bar in which to automatically
;;;   place slurs.  
;;; - :end-bar. An integer that is the last bar in which to automatically place
;;;   slurs. 
;;; - :rm-slurs-first. T or NIL to indicate whether to first remove existing
;;;   slurs from the specified region. NB: If you already have slur marks
;;;   attached to events, setting this to NIL can produce unwanted results
;;;   caused by orphaned beg-slur or end-slur marks.  T = remove existing slurs
;;;   first. Default = T.
;;; - :rm-staccatos. T or NIL to indicate whether to first remove existing
;;;   staccato, tenuto, and accented staccato marks from the specified
;;;   region. T = remove staccatos. Default = NIL.
;;; - :over-accents. T or NIL. Default = T.
;;; - :verbose. T or NIL to indicate whether to print feedback from the process
;;;   to the Lisp listener. T = print. Default = NIL.
;;; 
;;; RETURN VALUE
;;; A list of sublists, each of which contains the start and end event, plus
;;; the number of notes under the slur, for each slur added. 
;;; 
;;; EXAMPLE
#|
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((vn (violin :midi-channel 1))))
        :tempo-map '((1 (q 60)))
        :set-palette '((1 ((c3 d3 e3 f3 g4 a3 b3
                               c4 d4 e4 f4 g4 a4 b4 c5))))
        :set-map '((1 (1 1 1)))
        :rthm-seq-palette '((1 ((((4 4) - e e - (s) e.  
                                  - s s e - - s (s) s s -))
                                :pitch-seq-palette ((1 2 3 4 5 6 7 8 9))
                                :marks (a 4))))
        :rthm-seq-map '((1 ((vn (1 1 1))))))))
  (auto-slur mini 'vn
             :start-bar 1
             :end-bar 2))

  |#
;;; SYNOPSIS
(defmethod auto-slur ((sc slippery-chicken) players
                      &key start-bar end-bar
                      (rm-slurs-first t)
                      (rm-staccatos t)
                      ;; 5.4.11
                      (over-accents t)
                      verbose)
;;; ****
  (unless (listp players)
    (setf players (list players)))
  (unless start-bar
    (setf start-bar 1))
  (unless end-bar
    (setf end-bar (num-bars sc)))
  (let ((result '()))
    (loop 
       with start-e 
       with count
       with last-e
       for player in players do
       (next-event sc player nil start-bar)
       (setf start-e nil
             last-e nil
             count 0)
       (loop 
          for e = (next-event sc player nil nil end-bar) 
          while e
          do
          (when verbose
            (format t "~&e: ~a, count ~a, start-e ~a"
                    (when e
                      (get-pitch-symbol e nil))
                    count
                    (when start-e
                      (get-pitch-symbol start-e nil))))
          (when rm-slurs-first
            (rm-marks e '(beg-sl end-sl) nil))
          (cond ((and (needs-new-note e) ;; start slur
                      (not start-e))
                 (setf start-e e)
                 (incf count))
                ((and (or (is-rest e) ;; end slur
                          (and (not over-accents) (accented-p e))
                          ;; 2 of the same notes should stop a slur but
                          ;; without checking that count > 2 this code
                          ;; might actually put a slur over two of the same
                          ;; notes  
                          ;; MDE Thu Jun  7 18:14:45 2012 -- changed > from 2
                          ;; to 1  
                          (and (> count 1)
                               (porc-equal e last-e)))
                      start-e
                      last-e
                      (not (is-rest start-e))
                      (not (is-rest last-e))
                      (> count 1)) 
                 ;; don't add slurs if they're already there
                 (unless (or (begin-slur-p start-e)
                             (end-slur-p last-e))
                   (when rm-staccatos
                     (replace-mark start-e 'as 'a)
                     (replace-mark last-e 'as 'a)
                     (rm-marks start-e 'te nil)
                     (rm-marks last-e 'te nil)
                     (rm-marks start-e 's nil)
                     (rm-marks last-e 's nil))
                   ;; MDE Mon Nov 18 14:31:59 2013 
                   (push (list start-e last-e count) result)
                   (add-mark start-e 'beg-sl)
                   (add-mark last-e 'end-sl))
                 (when verbose
                   (format t "~&~a -> ~a" 
                           (get-pitch-symbol start-e nil)
                           (get-pitch-symbol last-e nil)))
                 (when (and (needs-new-note e)
                            (porc-equal e last-e))
                   (setf start-e e))
                 (setf start-e nil
                       count 0))
                ;; got start of 'phrase' but second note is same as first
                ((and start-e 
                      (= count 1)
                      (needs-new-note e)
                      (porc-equal start-e e))
                 (setf start-e e))
                ((or (is-rest e) ; MDE Thu Jun  7 17:42:10 2012 -- or case added
                     ;; (porc-equal e last-e)
                     (and (not over-accents) (accented-p e)))
                 (setf start-e nil
                       count 0))
                ((not (is-tied-to e))
                 (incf count)))
          ;; MDE Thu Jun  7 18:34:28 2012 -- took this out of the cond
          (when (and rm-staccatos start-e) ; (not last-e))
            ;; in the middle of a slur so remove staccatos etc.
            (when verbose
              (format t " (in slur)"))
            (if over-accents
                (replace-mark e 'as 'a)
                (rm-marks e 'a nil))
            (rm-marks e 'te nil)
            (rm-marks e 's nil))
          (setf last-e e)))
    ;; 9.4.11
    (check-slurs sc)
    (nreverse result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Fri Apr 20 11:52:59 BST 2012: Added robodoc entry

;;; ****m* slippery-chicken-edit/delete-clefs
;;; DESCRIPTION
;;; Delete the clef symbol held in the MARKS-BEFORE slot of the specified event
;;; object within the given slippery-chicken object.
;;; 
;;; ARGUMENTS
;;; NB: The optional arguments are actually required.
;;; - A slippery-chicken object.
;;; - The ID of the player from whose part the clef symbol is to be deleted. 
;;; - An integer that is the number of the bar from which the clef symbol is to
;;;   be deleted.
;;; - An integer that is the number of the event object within the specified
;;;   from whose MARKS-BEFORE slot the clef symbol is to be deleted. This is a
;;;   1-based index but counts rests and ties.
;;; 
;;; RETURN VALUE
;;; Returns NIL.
;;; 
;;; EXAMPLE
#|
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((vc (cello :midi-channel 1))))
        :tempo-map '((1 (q 60)))
        :set-palette '((1 ((c2 e2 d4 e4 f4 g4 a4 f5))))
        :set-map '((1 (1 1 1 1)))
        :rthm-seq-palette '((1 ((((4 4) e e e e e e e e))
                                :pitch-seq-palette ((1 2 3 4 5 6 7 8)))))
        :rthm-seq-map '((1 ((vc (1 1 1 1))))))))
  (auto-clefs mini)
  (delete-clefs mini 'vc 1 3))

  => NIL

  |#
;;; SYNOPSIS
(defmethod delete-clefs ((sc slippery-chicken) &optional
                         player bar-num event-num)
;;; ****
  (let ((e (get-event sc bar-num event-num player)))
    (if e
        (delete-clefs e)
        (warn "slippery-chicken::delete-clefs: Can't delete clef ~
               for ~a at bar ~a, event ~a"
              player bar-num event-num))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Thu Apr 19 11:31:58 BST 2012: Added robodoc entry

;;; MDE original comment:
;;; optional args are actually required but optional because of event class
;;; method  

;;; ****m* slippery-chicken-edit/add-clef
;;; DESCRIPTION
;;; Attach a specified clef symbol to a specified clef object within a given
;;; slippery-chicken object.
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; - The ID of the player to whose part the clef symbol is to be added.
;;;
;;; NB: The optional arguments are actually required.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - An integer that is the bar number in which the clef symbol is to be
;;;   placed. 
;;; - An integer that is the event number within the given bar to which the
;;;   clef symbol is to be attached.
;;; - A symbol that is the clef type to be attached. See the documentation for
;;;   the make-instrument function of the instrument class for a list of
;;;   possible clef types.
;;; 
;;; RETURN VALUE
;;; Returns the new value of the MARKS-BEFORE slot of the given event object. 
;;; 
;;; EXAMPLE
#|
(let ((mini
       (make-slippery-chicken
        '+mini+
        :title "mini"
        :ensemble '(((vn (violin :midi-channel 1))))
        :tempo-map '((1 (q 60)))
        :set-palette '((1 ((c4 d4 f4 g4 a4 c5 d5 f5 g5 a5 c6)))
                       (2 ((cs4 ds4 fs4 gs4 as4 cs5 ds5 fs5 gs5 as5))))
        :set-map '((1 (1 1 1 1 1 1)))
        :rthm-seq-palette '((1 ((((2 4) q e s s))
                                :pitch-seq-palette ((1 2 3 4)))))
        :rthm-seq-map '((1 ((vn (1 1 1 1 1 1))))))))
  (add-clef mini 'vn 3 2 'alto))

  => ((CLEF ALTO))

  |#
;;; SYNOPSIS
(defmethod add-clef ((sc slippery-chicken) player &optional
                     bar-num event-num clef)
;;; ****
  (let ((e (get-event sc bar-num event-num player)))
    (if e
        (add-clef e clef)
        (warn "slippery-chicken::add-clef: Can't add clef ~
               for ~a at bar ~a, event ~a"
              player bar-num event-num))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Fri Apr 20 17:11:01 BST 2012: Added robodoc entry

;;; ****m* slippery-chicken-edit/move-clef
;;; DESCRIPTION
;;; Move a specified clef from a specified event object to another.
;;;
;;; NB: As the :auto-clefs option of cmn-display and write-lp-data-for all
;;;     first deletes all clefs before automatically placing them, this
;;;     argument must be set to NIL. The auto-clefs method can be called
;;;     outside of the cmn-display or write-lp-data-for-all methods instead.  
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; - An integer that is the number of the bar in which the given clef is
;;;   located.
;;; - An integer that is the number of the event object in the given bar to
;;;   which the given clef is attached.
;;; - An integer that is the number of the bar to which the given clef is
;;;   to be moved (this can be the same bar).
;;; - An integer that is the number of the event object in the new bar to
;;;   which the given clef is to attached.
;;; - The ID of the player in whose part the clef is to be moved.
;;; 
;;; RETURN VALUE
;;; Returns the value of the MARKS-BEFORE slot of the event object to which the
;;; clef is moved.
;;; 
;;; EXAMPLE
#|
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((vc (cello :midi-channel 1))))
        :tempo-map '((1 (q 60)))
        :set-palette '((1 ((c2 e2 d4 e4 f4 g4 a4 f5))))
        :set-map '((1 (1 1 1 1)))
        :rthm-seq-palette '((1 ((((4 4) e e e e e e e e))
                                :pitch-seq-palette ((1 2 3 4 5 6 7 8)))))
        :rthm-seq-map '((1 ((vc (1 1 1 1))))))))
  (auto-clefs mini)
  (move-clef mini 1 6 1 8 'vc)
  (cmn-display mini :auto-clefs nil))

  |#
;;; SYNOPSIS
(defmethod move-clef ((sc slippery-chicken) from-bar from-event
                      to-bar to-event player)
;;; ****
  (let ((clef (get-clef sc from-bar from-event player)))
    (if clef
        (progn
          (delete-clefs sc player from-bar from-event)
          (add-clef sc player to-bar to-event clef))
        (error "slippery-chicken::move-clef (~a): no clef at bar ~a event ~a"
               player from-bar from-event))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; NB While this routine generally does a good job of putting the right clefs
;;; in place, it will get confused if notes jump from very high to low
;;; (e.g. over complete piano range).  Called automatically by cmn-display
;;; and write-lp-data-for-all

;;; ****m* slippery-chicken-edit/auto-clefs
;;; DESCRIPTION
;;; Automatically create clef changes in the specified player's or players'
;;; part or parts by adding the appropriate clef symbol to the MARKS-BEFORE
;;; slot of the corresponding event object.
;;;
;;; This routine will only place clef symbols that are present in the given
;;; instrument object's CLEFS slot.
;;;
;;; This method is called automatically by cmn-display and
;;; write-lp-data-for-all, with the delete-clefs option set to T.
;;;
;;; NB: While this routine generally does a good job of putting the proper
;;;     clefs in place, it will get confused if the pitches in a given player's
;;;     part jump from very high to very low (e.g. over the complete range of
;;;     the piano).
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :verbose. T or NIL to indicate whether the method is to print feedback
;;;   about its operations to the Listener. T = print feedback. Default = NIL. 
;;; - :in-c. T or NIL to indicate whether the pitches processed are to be
;;;   handled as sounding or written pitches. T = sounding. Default = T.
;;; - :players. A list containing the IDs of the players whose parts are to be
;;;   to have clefs automatically added.
;;; - :delete-clefs. T or NIL to indicate whether the method should first
;;;   delete all clef symbols from the MARKS-BEFORE slots of all event objects
;;;   it is processing before setting the automatic clef changes.
;;; - :delete-marks-before. T or NIL to indicate whether the MARKS-BEFORE slot
;;;   of all event objects processed should first be set to NIL. 
;;;   T = set to NIL. Default = NIL.
;;; 
;;; RETURN VALUE
;;; Returns T
;;; 
;;; EXAMPLE
#|
;;; Straightforward usage applied to just the VC player ;
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((vn (violin :midi-channel 1))
                     (vc (cello :midi-channel 2))))
        :tempo-map '((1 (q 60)))
        :set-palette '((1 ((c2 e2 d4 e4 f4 g4 a4 f5))))
        :set-map '((1 (1 1 1 1)))
        :rthm-seq-palette '((1 ((((4 4) e e e e e e e e))
                                :pitch-seq-palette ((1 2 3 4 5 6 7 8)))))
        :rthm-seq-map '((1 ((vn (1 1 1 1))
                            (vc (1 1 1 1))))))))
  (auto-clefs mini :players '(vc)))

  => T

  |#
;;; SYNOPSIS
(defmethod auto-clefs ((sc slippery-chicken) 
                       &key verbose in-c players 
                         (delete-clefs t)
                         (delete-marks-before nil))
;;; ****
  ;; MDE Mon Dec 21 15:14:50 2020, Heidhausen
  (loop for player in (force-list (if players players (players sc)))
     do
       (let ((note-count -1)
             (current-clef nil)
             ;; (last-events '(nil nil nil)) wasn't reinitializing each time!
             (last-events (ml nil 3))
             (last-clefs (ml nil 3)))
         (when verbose
           (format t "~%~%auto-clefs: player: ~a" player))
         (loop 
            for bar-num from (start-bar (piece sc)) to (end-bar (piece sc))
            for bar = (get-bar sc bar-num player)
            for section-ref = (butlast (player-section-ref bar))
            for ins = (get-current-instrument-for-player 
                       section-ref player (1+ (nth-seq bar)) sc)
            do
              (when verbose
                (format t "~&bar ~a" bar-num))
              (unless current-clef
                (setf current-clef (starting-clef ins)))
              (loop 
                 for event in (rhythms bar)
                 with clefs with written with pitch
                 do
                   (when delete-marks-before
                     (setf (marks-before event) nil))
                 ;; 1.2.11 delete clefs first
                   (when delete-clefs 
                     (delete-clefs event nil)) ; don't warn if there's no clef
                   (when (needs-new-note event)
                     (if (= 2 note-count)
                         (setf note-count 0)
                         (incf note-count))
                     (when verbose
                       (format t "~&note-count: ~a" note-count))
                     (setf written (written-pitch-or-chord event)
                           pitch (if (and written 
                                          (or (not in-c)
                                              (from-8ve-transposing-ins event)))
                                     written
                                     (pitch-or-chord event))
                           clefs (best-clef ins pitch in-c current-clef 
                                            verbose)
                           (nth note-count last-clefs) clefs
                           (nth note-count last-events) event
                           current-clef (auto-clefs-handle-last-3
                                         ;; for some reason lisp isn't passing
                                         ;; last-clefs, rather last-events
                                         ;; twice...
                                         last-events last-clefs note-count
                                         current-clef verbose in-c)))))))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This is different from set-rehearsal-letters in that we don't use the
;;; rehearsal-letters slot of sc, rather, we use the method argument.

;;; ****m* slippery-chicken-edit/set-rehearsal-letter
;;; DESCRIPTION
;;; Add the specified rehearsal letter/number to the specified bar in one or
;;; more specified players.
;;; 
;;; NB: Since internally this method actually attaches the rehearsal
;;;     letter/number to the REHEARSAL-LETTER slot of the preceding bar
;;;     (bar-num - 1), no rehearsal letter/number can be attached to the first
;;;     bar.
;;;
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; - An integer that is the number of the bar to which the rehearsal
;;;   letter/number is to be added.  
;;; - A symbol that is the rehearsal letter/number to be added (e.g. 'A or '1)
;;; 
;;; OPTIONAL ARGUMENTS
;;; - The player ID or a list of player IDs to whose parts the rehearsal
;;;   letter/number is to be added. If no value is given here, the rehearsal
;;;   letter/number will be added to the first (top) instrument in each group
;;;   of the ensemble, as specified in staff-groupings.
;;; 
;;; RETURN VALUE
;;; Returns T.
;;; 
;;; EXAMPLE
#|
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((vn (violin :midi-channel 1))
                     (va (viola :midi-channel 2))
                     (vc (cello :midi-channel 3))))
        :set-palette '((1 ((ds3 e3 fs3 af3 bf3 c4 ef4 fs4))))
        :set-map '((1 (1 1 1 1)))
        :rthm-seq-palette '((1 ((((2 4) q e s s))
                                :pitch-seq-palette ((1 2 3 4)))))
        :rthm-seq-map '((1 ((vn (1 1 1 1))
                            (va (1 1 1 1))
                            (vc (1 1 1 1))))))))
  (set-rehearsal-letter mini 2 'A)
  (set-rehearsal-letter mini 3 '2 '(va vc))
  (set-rehearsal-letter mini 4 'Z3))

  => T

  |#
;;; SYNOPSIS
(defmethod set-rehearsal-letter ((sc slippery-chicken) bar-num letter
                                 &optional players)
;;; ****
  (unless players
    (setf players (get-groups-top-ins sc)))
  (loop 
     for p in players 
     ;; remember the letter is actually placed on the bar-line of the
     ;; previous bar
     for bar = (get-bar sc (1- bar-num) p)
     do
       (setf (rehearsal-letter bar) letter))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Fri Apr 20 13:25:17 BST 2012: Added robodoc entry

;;; ****m* slippery-chicken-edit/delete-rehearsal-letter
;;; DESCRIPTION
;;; Delete the rehearsal letter from a specified bar of on or more specified
;;; players' parts by setting the REHEARSAL-LETTER slot of the corresponding
;;; rthm-seq-bar object to NIL.
;;;
;;; NB: This deletes the given rehearsal letter without resetting and
;;;     re-ordering the remaining rehearsal letters.
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; - An integer that is the number of the bar from which the rehearsal letter
;;;   is to be deleted. NB: The rehearsal letter for a given bar is internally
;;;   actually attached to the previous bar. The number given here is the
;;;   number from the user's perspective, but the change will be reflected in
;;;   the bar with the number specified -1.
;;; 
;;; OPTIONAL ARGUMENTS 
;;; - A list consisting of the IDs of the players from whose parts the
;;;   rehearsal letter is to be deleted.
;;; 
;;; RETURN VALUE
;;; Returns the deleted rehearsal letter from the last player which had one
;;; attached.  
;;; 
;;; EXAMPLE
#|
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((vc (cello :midi-channel 1))))
        :tempo-map '((1 (q 60)))
        :set-palette '((1 ((c2 e2 d4 e4 f4 g4 a4 f5))))
        :set-map '((1 (1 1 1 1 1 1)))
        :rthm-seq-palette '((1 ((((4 4) e e e e e e e e))
                                :pitch-seq-palette ((1 2 3 4 5 6 7 8)))))
        :rthm-seq-map '((1 ((vc (1 1 1 1 1 1)))))
        :rehearsal-letters '(2 4 6))))
  (delete-rehearsal-letter mini 2 '(vc)))

  => NIL

  |#
;;; SYNOPSIS
(defmethod delete-rehearsal-letter ((sc slippery-chicken) bar-num
                                    &optional players)
;;; ****
  (unless players
    (setf players (players sc)))
  ;; MDE Fri Apr 20 14:25:54 2012 
  (unless (listp players)
    (setf players (list players)))
  (let (bar rl tmp)
    (loop for p in players do
       ;; remember the letter is actually placed on the bar-line of the
       ;; previous bar
         (setf bar (get-bar sc (1- bar-num) p)
               tmp (rehearsal-letter bar)
               (rehearsal-letter bar) nil)
         (when tmp (setq rl tmp)))
    rl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* slippery-chicken-edit/move-rehearsal-letter
;;; DATE
;;; March 15th 2017, Edinburgh
;;; 
;;; DESCRIPTION
;;; Move a rehearsal letter for those players who have one at <from-bar> to
;;; <to-bar> 
;;; 
;;; ARGUMENTS
;;; - a slippery-chicken object
;;; - the bar number (integer) to move the letter from
;;; - the bar number (integer) to move the letter to
;;; 
;;; RETURN VALUE
;;; The last rehearsal-letter that was seen (in the lowest player in the
;;; ensemble) 
;;; 
;;; SYNOPSIS
(defmethod move-rehearsal-letter ((sc slippery-chicken) from-bar to-bar)
;;; ****  
  (let* ((from (get-bar sc (1- from-bar)))
         (to (get-bar sc (1- to-bar)))
         rl last)
    (loop for fbar in from for tbar in to do
         (setq rl (rehearsal-letter fbar))
         (when rl
           (setf last rl
                 (rehearsal-letter fbar) nil
                 (rehearsal-letter tbar) rl)))
    (unless last
      (error "slippery-chicken::move-rehearsal-letter: no rehearsal letter ~
              at bar ~a" from-bar))
    last))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Tue May 27 17:38:24 2014 -- called by (setf rehearsal-letters) :after
;;; method.
(defmethod delete-all-rehearsal-letters ((sc slippery-chicken)
                                         ;; start at 2 because the letter is
                                         ;; always on the previous bar 
                                         &key (start 2) end)
  (when (initialized sc)
    (unless end
      (setf end (num-bars sc)))
    (loop for i from start to end do
         (delete-rehearsal-letter sc i))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat Apr 21 13:47:21 BST 2012: Added robodoc entry.

;;; ****m* slippery-chicken-edit/re-bar
;;; DESCRIPTION
;;; Arrange the events of specified consecutive bars in a given
;;; slippery-chicken object into new bars of a different time signature. If the
;;; number of beats in the specified series of events does not fit evenly into
;;; full measures of the the specified time signature, the method will do its
;;; best to create occasional bars of a different time-signature that are as
;;; close as possible to the desired length.
;;;
;;; This method will only combine existing short bars into longer ones; it
;;; won't split up longer bars and recombine them.
;;;
;;; NB: This method should not be confused with the rebar method (which is used
;;; internally, not directly by the user).
;;;
;;; NB: as this reorganises fundamental structures within the slippery-chicken
;;; object, it may cause methods like clm-play to fail with some arguments
;;; which seem reasonable to the user. So use immediately before creating the
;;; score and after creating other outputs.
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments
;;; - :start-bar. An integer that is the number of the first bar whose events
;;;   are to be re-barred.
;;; - :end-bar. An integer that is the number of the last bar whose events are
;;;   to be re-barred.
;;; - :min-time-sig. A time signature in the form of a 2-item list containing
;;;   the number of beats and the beat unit; e.g. '(3 4). This is a target time
;;;   signature from which the method may occasionally deviate if the number of
;;;   events does not fit evenly into full bars of the specified time
;;;   signature.
;;; - :verbose. T or NIL to indicate whether to print feedback on the
;;;   re-barring process to the Listener. T = print feedback. Default = NIL.
;;; - :check-ties. T or NIL to indicate whether to force the method to ensure
;;;   that all ties have a beginning and ending. T = check. 
;;;   Default = T.
;;; - :auto-beam. T, NIL, or an integer. If T, the method will automatically
;;;   attach beam indications to the corresponding events according to the beat
;;;   unit of the time signature. If an integer, the method will beam in
;;;   accordance with a beat unit that is equal to that integer. If NIL, the
;;;   method will not automatically place beams. Default = T.
;;; - :update-slots. T or NIL to indicate whether to update all slots of the
;;;   given slippery-chicken object after applying the method. This is an
;;;   internal argument and will generally not be needed by the user.
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
        :tempo-map '((1 (q 60)))
        :set-palette '((1 ((c2 e2 d4 e4 f4 g4 a4 f5))))
        :set-map '((1 (1 1 1 1 1 1 1)))
        :rthm-seq-palette '((1 ((((2 4) q e s s))
                                :pitch-seq-palette ((1 2 3 4)))))
        :rthm-seq-map '((1 ((vn (1 1 1 1 1 1 1))))))))
  (re-bar mini :start-bar 2 :end-bar 5 :min-time-sig '(4 4) :auto-beam 4))

  => T

  |#
;;; SYNOPSIS
(defmethod re-bar ((sc slippery-chicken)
                   &key start-bar 
                   end-bar
                   ;; the following is just a list like '(3 8) '(5 8)
                   min-time-sig
                   verbose
                   ;; MDE Thu Feb  9 10:36:02 2012 -- seems if we don't
                   ;; update-slots then the new bar structure isn't displayed 
                   (update-slots t)
                   (check-ties t)
                   ;; could also be a beat rhythmic unit
                   (auto-beam t))
;;; ****
  (unless start-bar
    (setf start-bar 1))
  (unless end-bar
    (setf end-bar (num-bars sc)))
  ;; MDE Thu Feb  9 11:30:39 2012
  (unless (>= end-bar start-bar)
    (error "slippery-chicken::re-bar: end-bar (~a) must be >= start-bar (~a)."
           end-bar start-bar))
  (when verbose
    (format t "~&Re-bar from bar ~a to ~a:" start-bar end-bar))
  (object-is-nil? min-time-sig "slippery-chicken::re-bar" 'min-time-sig)
  ;; make double bar at end of piece a normal bar line
  (change-bar-line-type sc (num-bars (piece sc)) 0)
  (loop 
     for section-no in (data (piece sc)) 
     for section = (data section-no)
     do
     (when verbose
       (format t "~%~%***** Section ~a: bar ~a to ~a...." 
               (id section-no) (start-bar section) (end-bar section)))
       ;; MDE Thu Feb  9 11:43:51 2012 -- fixed the logic here
     (if (and (<= start-bar (end-bar section))
              (>= end-bar (start-bar section)))
         (progn 
           (when verbose 
             (format t "processing."))
           (re-bar section :start-bar start-bar :end-bar end-bar
                   :min-time-sig min-time-sig :verbose verbose
                   :auto-beam auto-beam))
         (format t "ignoring.")))
  ;; midi-time-sig is taken care of in rthm-seq-bar::get-timings when midi-play
  ;; is called.
  (update-slots sc (tempo-map sc) 0.0 0.0 1 nil nil (warn-ties sc))
  (set-write-time-sig sc)
  ;; make double bar line at end of piece
  (change-bar-line-type sc (num-bars (piece sc)) 2)
  (when check-ties
    (check-ties sc))
  ;; have to update the bar numbers of where the rehearsal letters are.
  (setf (rehearsal-letters sc) (find-rehearsal-letters sc))
  ;; MDE Thu Feb  9 10:36:37 2012 
  (when update-slots
    (update-slots sc))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SAR Fri Apr 20 09:12:59 BST 2012: Added robodoc entry

;;; ****m* slippery-chicken-edit/auto-beam
;;; DESCRIPTION
;;; Automatically places indications for beam start- and end-points (1 and 0)
;;; in the BEAMS slot of the corresponding event objects.
;;;
;;; By default, this method determines the start and end indications for beams
;;; on the basis of the beat found in the time signature, but the user can
;;; specify a different beat basis using the first optional argument.
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - NIL, an integer that is a power-of-two rhythmic duration, or the
;;;   alphabetic representation of such a rhythm to specify the beat basis for
;;;   setting beams (e.g. 4 or 'h).
;;; - T or NIL to indicate whether the method is to check whether an exact beat
;;;   of rhythms can be found for each beat of the bar. If T, a warning will be
;;;   printed when an exact beat cannot be found for each beat of the bar. 
;;;   Default = T.
;;; - players, a single symbol or list of players to process. Default = NIL =
;;;   process all players in the ensemble 
;;; 
;;; RETURN VALUE
;;; Returns NIL.
;;; 
;;; EXAMPLE
#|
;; Auto-beam the events of the given slippery-chicken object on the basis of a ;
;; half note:                           ;
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((vn (violin :midi-channel 1))))
        :tempo-map '((1 (q 60)))
        :set-palette '((1 ((fs4 gs4 as4))))
        :set-map '((1 (1 1 1 1)))
        :rthm-seq-palette '((1 ((((4 4) e e e e e e e e))
                                :pitch-seq-palette ((1 2 3 2 1 2 3 2)))))
        :rthm-seq-map '((1 ((vn (1 1 1 1))))))))
  (auto-beam mini 'h))

  => NIL

  |#
;;; SYNOPSIS
(defmethod auto-beam ((sc slippery-chicken) &optional (beat nil) (check-dur t)
                                                      players)
;;; ****
  ;; MDE Wed Aug 23 19:57:16 2023, Heidhausen -- don't do all players by force
  (setq players (if players (force-list players) (players sc)))
  (loop for player in players do
        (loop 
            for bnum from 1 to (num-bars sc) 
            for bar = (get-bar sc bnum player)
            do
              (auto-beam bar beat check-dur))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sort notes in piece--across all instruments--into time-ordered lists and
;;; process them with the given function, which must take one argument, an
;;; event.

;;; SAR Fri Jun  8 12:56:44 BST 2012: Added robodoc entry


;;; ****m* slippery-chicken-edit/process-events-by-time
;;; DESCRIPTION
;;; Apply the given function to all event objects within the given measure
;;; range in order of their chronological occurrence.  The function can take
;;; one argument only: the current event object.  NB If the time of the event
;;; is needed it can be accessed in the given function via the event's
;;; start-time slot.
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; - A function (or variable to which a function has been assigned).
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :start-bar. An integer that is the first bar in which the function is to
;;;   be applied to event objects. Default = 1.
;;; - :end-bar. NIL or an integer that is the last bar in which the function is
;;;   to be applied to event objects. If NIL, the last bar of the
;;;   slippery-chicken object is used. Default = NIL.  
;;; 
;;; RETURN VALUE
;;; T
;;; 
;;; EXAMPLE
#|
(let ((marks (make-cscl '(a s as te ts at))))
  (defun add-random-marks (event)
    (unless (is-rest event) 
      (setf (marks event) (list (get-next marks))))))

(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((vn (violin :midi-channel 1))
                     (va (viola :midi-channel 2))
                     (vc (cello :midi-channel 3))))
        :set-palette '((1 ((f3 g3 a3 b3 c4 d4 e4 f4))))
        :set-map '((1 (1 1 1)))
        :rthm-seq-palette '((1 ((((3 4) s (e.)  (s) s (e) (e) s (s)))
                                :pitch-seq-palette ((1 2 3))))
                            (2 ((((3 4) (s) s (e) (e) s (s) s (e.)))
                                :pitch-seq-palette ((1 2 3))))
                            (3 ((((3 4) (e) s (s) s (e.)  (s) s (e)))
                                :pitch-seq-palette ((1 2 3)))))
        :rthm-seq-map '((1 ((vn (1 2 3))
                            (va (2 3 1))
                            (vc (3 1 2))))))))
  (process-events-by-time mini #'add-random-marks))

  |#
;;; SYNOPSIS
(defmethod process-events-by-time ((sc slippery-chicken) function
                                   &key (start-bar 1) end-bar)
;;; ****
  (loop for e in
       (get-events-sorted-by-time sc :start-bar start-bar :end-bar end-bar) 
     do (funcall function e))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken-edit/sc-move-dynamic
;;; DESCRIPTION
;;; Move the dynamic attached to a specified event object to another specified
;;; event object.
;;;
;;; By default the dynamics are moved between events within the same bar. An
;;; optional argument allows for dynamics to be moved to events in a different
;;; bar. 
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; - An integer that is the number of the bar in which to move the dynamic.
;;; - The ID of the player in whose part the dynamic is located.
;;; - An integer that is the number of the event object from which the dynamic
;;;   is to be moved. This number is 1-based and counts both rests and ties.
;;; - An integer that is the number of the event object to which the dynamic
;;;   is to be moved. This number is 1-based and counts both rests and ties.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - An integer that is the number of the bar to which the dynamic should be
;;;   moved. If this is not specified, the dynamic will be moved to the
;;;   specified event within the same bar.
;;; 
;;; RETURN VALUE
;;; Returns T.
;;; 
;;; EXAMPLE
#|
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((vc (cello :midi-channel 1))))
        :set-palette '((1 ((a3 b3 c4 e4))))
        :set-map '((1 (1 1 1)))
        :rthm-seq-palette '((1 ((((2 4) q e s s))
                                :pitch-seq-palette ((1 2 3 4))
                                :marks (fff 1))))
        :rthm-seq-map '((1 ((vc (1 1 1))))))))
  (sc-move-dynamic mini 1 'vc 1 3)
  (sc-move-dynamic mini 2 'vc 1 4 3))

  => T

  |#
;;; SYNOPSIS
(defmethod sc-move-dynamic ((sc slippery-chicken) bar-num player
                            ;; event numbers 1-based but counting rests and ties
                            from to &optional to-bar)
;;; ****
  (unless to-bar
    (setf to-bar bar-num))
  (let ((dyn (sc-remove-dynamic sc bar-num player from)))
    (add-mark (get-event sc to-bar to player) dyn)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  MDE Fri Dec  7 13:02:26 2018 -- mainly for the orchestrate method
(defmethod move-dynamics-from-rests ((sc slippery-chicken) 
                                     &optional players start-bar end-bar)
  (unless players (setq players (players sc)))
  (unless (listp players) (setq players (list players)))
  (unless start-bar (setq start-bar 1))
  (unless end-bar (setq end-bar (num-bars sc)))
  (loop for player in players with last-note do
     ;; reset to the first event
       (next-event sc player nil t)
     ;; put all the dynamics found on a rest back onto the last note seen
       (loop for e = (next-event sc player nil nil end-bar)
          for dynamics = (when e (get-dynamics e t))
          while e do
            (if (is-rest e)
                (progn
                  (when (and dynamics last-note)
                    (add-marks last-note dynamics t nil))
                  ;; if we've just hit a rest but the last note had the start of
                  ;; a hairpin, remove it 
                  (when (and last-note
                             (intersection (marks last-note)
                                           '(cresc-beg dim-beg)))
                    (rm-marks last-note '(cresc-beg dim-beg) nil)))
                (setq last-note e)))))
       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken-edit/sc-remove-dynamic
;;; DESCRIPTION
;;; Remove all dynamics from the MARKS slot of one or more specified event
;;; objects. 
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; - An integer that is the number of the bar from which the dynamics are to
;;;   be removed. 
;;; - The ID of the player from whose part the dynamics are to be removed. 
;;; - An integer or a list of integers that are the numbers of the events from
;;;   which the dynamics are to be removed. Event numbers include ties and
;;;   rests. 
;;;
;;; RETURN VALUE
;;; Returns the last dynamic removed.
;;; 
;;; EXAMPLE
#|
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((vc (cello :midi-channel 1))))
        :set-palette '((1 ((a3 b3 c4 e4))))
        :set-map '((1 (1 1 1)))
        :rthm-seq-palette '((1 ((((2 4) q e s s))
                                :pitch-seq-palette ((1 2 3 4))
                                :marks (fff 1 ppp 3))))
        :rthm-seq-map '((1 ((vc (1 1 1))))))))
  (sc-remove-dynamic mini 2 'vc 1)
  (sc-remove-dynamic mini 3 'vc '(1 3)))

  => PPP

  |#
;;; SYNOPSIS
(defmethod sc-remove-dynamic ((sc slippery-chicken) bar-num player
                              &rest event-nums)
;;; ****
  ;; just in case we call this method from another function with &rest
  ;; event-nums  
  (setf event-nums (flatten event-nums))
  (loop for en in event-nums 
     for event = (get-event sc bar-num en player)
     for dynamics = (get-dynamics event)
     do
       (remove-dynamics event)
     ;; returns the first dynamic removed from the last requested event
     finally (return (first dynamics))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken-edit/sc-remove-dynamics
;;; DATE 
;;; 16-Mar-2011
;;;
;;; DESCRIPTION
;;; Remove all dynamic marks from the MARKS slots of all consecutive event
;;; objects (or just rests, if preferred) within a specified region of bars. 
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; - An integer or a list of two integers. If a single integer, this is the
;;;   number of the first bar from which the dynamics will be removed, and all
;;;   dynamics will be removed from the full bar. If this is a list of two
;;;   integers, they are the numbers of the first bar and first note within
;;;   that bar from which the dynamics will be removed, in the form '(bar-num
;;;   note-num). Note numbers are 1-based and count ties but not rests.
;;; - An integer or a list of two integers. If a single integer, this is the
;;;   number of the last bar from which the dynamics will be removed, and all
;;;   dynamics will be removed from the full bar. If this is a list of two
;;;   integers, they are the numbers of the last bar and last note within that
;;;   bar from which the dynamics will be removed, in the form '(bar-num
;;;   note-num). Note numbers are 1-based and count ties but not rests.
;;; - A single ID or a list of IDs of the players from whose parts the dynamics
;;;   are to be removed. If NIL then all players will be processed. Default =
;;;   NIL = all players.
;;;
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to indicate that dynamics should be removed only from
;;;   rests. Default = NIL.
;;; 
;;; RETURN VALUE
;;; Returns T.
;;; 
;;; EXAMPLE
#|
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((vn (violin :midi-channel 1))
                     (va (viola :midi-channel 2))
                     (vc (cello :midi-channel 3))))
        :set-palette '((1 ((d3 e3 f3 g3 a3 b3 c4 e4 f4 g4 a4 b4))))
        :set-map '((1 (1 1 1)))
        :rthm-seq-palette '((1 ((((2 4) q e s s))
                                :pitch-seq-palette ((1 2 3 4))
                                :marks (fff 1 ppp 3))))
        :rthm-seq-map '((1 ((vn (1 1 1))
                            (va (1 1 1))
                            (vc (1 1 1))))))))
  (sc-remove-dynamics mini '(1 2) '(2 2) 'vn)
  (sc-remove-dynamics mini 2 3 '(va vc)))

  => T

  |#
;;; SYNOPSIS
(defmethod sc-remove-dynamics ((sc slippery-chicken) start end players
                               &optional just-rests)
;;; ****
  (unless players (setq players (players sc)))
  (unless (listp players) (setq players (list players)))
  (let* ((stlist (listp start))
         (ndlist (listp end))
         (stbar (if stlist (first start) start))
         (ndbar (if ndlist (first end) end))
         (stnote (if stlist (second start) 1))
         (ndnote (when ndlist (second end)))) ; nil processed in do-bar
    (flet ((do-bar (bar-num start-note end-note player)
             (unless end-note
               ;; (setf end-note (num-score-notes (get-bar sc bar-num player))))
               (setf end-note (num-rhythms (get-bar sc bar-num player))))
             (loop with bar = (get-bar sc bar-num player)
                for i from (1- start-note) below end-note 
                ;; for e = (get-nth-non-rest-rhythm i bar)
                for e = (get-nth-event i bar)
                do
                  ;; MDE Mon Nov 26 11:48:41 2018 -- just-rests?
                  (when (or (not just-rests) (is-rest e))
                    (remove-dynamics e t)))))
      (if (= stbar ndbar)
          (loop for p in players do
               (do-bar stbar stnote ndnote p))
          (loop for p in players do
               (do-bar stbar stnote nil p)
               (do-bar ndbar 1 ndnote p)
             ;; now the bars in the middle
               (loop for bnum from (1+ stbar) to (1- ndbar) do
                    (do-bar bnum 1 nil p))))))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken-edit/remove-extraneous-dynamics
;;; DESCRIPTION
;;; A post-generation editing method: If two or more consecutive event objects
;;; have the same dynamic, remove that dynamic marking from all but the first
;;; of these.
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
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
        :tempo-map '((1 (q 60)))
        :set-palette '((1 ((c2 e2 d4 e4 f4 g4 a4 f5))))
        :set-map '((1 (1 1 1 1 1 1 1)))
        :rthm-seq-palette '((1 ((((2 4) q e s s))
                                :pitch-seq-palette ((1 2 3 4))
                                :marks (f 1 f 2 f 3 f 4))))
        :rthm-seq-map '((1 ((vn (1 1 1 1 1 1 1))))))))
  (remove-extraneous-dynamics mini))

  => T

|#
;;; SYNOPSIS
(defmethod remove-extraneous-dynamics ((sc slippery-chicken))
;;; ****
  (loop for player in (players sc) do
     ;; (print player) 
       (loop with last-dynamic with last-note with rest-bars = 0
          for bar-num from 1 to (num-bars sc) 
          for bar = (get-bar sc bar-num player)
          do
          (loop for event in (rhythms bar) 
             for this-dynamic = (get-dynamic event)
             do
             (if (and (eq this-dynamic last-dynamic)
                      ;; 5.4.11 do repeat the dynamic if we've had several 
                      ;; rest bars  
                      (< rest-bars 2)
                      ;; MDE Thu Jan  9 08:38:35 2014 -- don't remove if we've 
                      ;; got hairpins
                      (not (or (and last-note (has-hairpin last-note))
                               (has-hairpin event))))
                 (remove-dynamics event)
                 (when this-dynamic
                   (setf last-dynamic this-dynamic)))
             (unless (is-rest event)
               (setf last-note event)))
          (if (is-rest-bar bar)
            (incf rest-bars)
            (setf rest-bars 0))))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A post-generation editing method

;;; SAR Wed May  2 13:11:53 BST 2012: Added robodoc entry

;;; ****m* slippery-chicken-edit/sc-delete-beams
;;; DESCRIPTION
;;; Delete beam indications from specified notes. If only a bar number is
;;; specified, this method deletes all beams in the bar.
;;;
;;; NB: If specifying start and end notes, the start notes specified must be
;;;     the first note of a beamed group of notes (i.e. the BEAMS slot of the
;;;     corresponding event object must be 1), and the end note must be the
;;;     last note of a beamed group of notes (i.e., the BEAMS slot of the
;;;     corresponding event object must be 0), otherwise errors may
;;;     occur. Also, if specifying one of these arguments, both must be
;;;     specified. 
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; - An integer that is the number of the bar in which the beams are to be
;;;   deleted.
;;; - The ID of the player from whose part the beams are to be deleted.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - An integer that is the number of the note that currently holds the
;;;   start-beam information (i.e., the BEAMS slot is 1). This number is
;;;   1-based and counts ties.
;;; - An integer that is the number of the note that currently holds the
;;;   end-beam information (i.e., the BEAMS slot is 0). This number is 1-based
;;;   and counts ties.
;;; 
;;; RETURN VALUE
;;; If deleting all beams in a bar, returns T, otherwise returns NIL.
;;; 
;;; EXAMPLE
#|
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((vc (cello :midi-channel 1))))
        :set-palette '((1 ((d3 e3 f3 g3 a3 b3 c4 e4))))
        :set-map '((1 (1 1 1 1)))
        :rthm-seq-palette '((1 ((((4 4) - e e - - e e - - e e - - e e -))
                                :pitch-seq-palette ((1 2 3 4 5 6 7 8)))))
        :rthm-seq-map '((1 ((vc (1 1 1 1))))))))
  (sc-delete-beams mini 2 'vc)
  (sc-delete-beams mini 3 'vc 3 4))

  => NIL

  |#
;;; SYNOPSIS
(defmethod sc-delete-beams ((sc slippery-chicken) bar-num player
                            &optional start-note end-note)
;;; ****
  (let* ((bar (get-bar (piece sc) bar-num player))
         ;; MDE Fri Apr 27 17:11:28 2012 -- check arguments
         (start (cond ((integerp start-note)
                       (get-nth-non-rest-rhythm (1- start-note) bar))
                      ((not start-note)
                       (get-nth-non-rest-rhythm 0 bar))
                      (t (error "slippery-chicken-edit::sc-delete-beams: ~
                               illegal argument to start-note: ~a"
                                start-note))))
         (end (cond ((integerp end-note)
                     (get-nth-non-rest-rhythm (1- end-note) bar))
                    ((not end-note) (get-last-event bar))
                    (t (error "slippery-chicken-edit::sc-delete-beams: ~
                               illegal argument to end-note: ~a" end-note)))))
    ;; MDE Thu Apr 26 16:47:22 2012 
    (unless (and start end (= 1 (beam start)) (zerop (beam end)))
      (warn "slippery-chicken-edit::sc-delete-beams: ~
             Beams don't start and end on the ~%indicated notes (~a to ~a, ~
             bar ~a). Proceeding anyway but you may see errors ~%in output."
            start-note end-note bar-num))
    (if (and start-note end-note)
        ;; MDE Wed Apr 25 15:24:43 2012 -- we were just deleting the beam on
        ;; start-note and end-note but let's instead delete all those inbetween
        ;; too 
        (loop for i from (1- start-note) below end-note do
             (delete-beam (get-nth-non-rest-rhythm i bar)))
        ;; (progn
        ;; (delete-beam (get-nth-non-rest-rhythm (1- start-note) bar))
        ;; (delete-beam (get-nth-non-rest-rhythm (1- end-note) bar)))
        (delete-beams bar))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 

;;; SAR Fri Apr 20 11:34:43 BST 2012: Added robodoc entry.
;;; SAR Fri Apr 20 11:34:51 BST 2012: Deleted MDE's original comment as it's
;;; been incorporated directly into the robodoc.

;;; ****m* slippery-chicken-edit/delete-bars
;;; DESCRIPTION
;;; Delete a sequence of consecutive bars from the given slippery-chicken
;;; object. 
;;;
;;; NB This might delete rehearsal letters, instrument changes (and maybe other
;;; things) attached to a bar/event.
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; - An integer that is the first bar to delete.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :num-bars. An integer that is the number of consecutive bars, including
;;;   the start-bar, to delete. This argument cannot be used simultaneously
;;;   with :end-bar
;;; - :end-bar. An integer that is the number of the last of the consecutive
;;;   bars to delete. This argument cannot be used simultaneously with
;;;   :num-bars. 
;;; - :print. Print feedback of the process to the Listener, including a
;;;   print-simple of the bars deleted.
;;; 
;;; RETURN VALUE
;;; Returns T.
;;; 
;;; EXAMPLE
#|
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((vc (cello :midi-channel 1))))
        :tempo-map '((1 (q 60)))
        :set-palette '((1 ((d3 e3 f3 g3 a3 b3 c4 e4))))
        :set-map '((1 (1 1 1 1)))
        :rthm-seq-palette '((1 ((((4 4) e e e e e e e e))
                                :pitch-seq-palette ((1 2 3 4 5 6 7 8)))))
        :rthm-seq-map '((1 ((vc (1 1 1 1))))))))
  (delete-bars mini 2 :end-bar 3)
  (delete-bars mini 2 :num-bars 1))

  => T

  |#
;;; SYNOPSIS
(defmethod delete-bars ((sc slippery-chicken) start-bar
                        &key num-bars end-bar print)
;;; ****
  (when (or (and (not end-bar) (not num-bars))
            (and end-bar num-bars))
    (error "slippery-chicken::delete-bars: either end-bar (~a) or num-bars ~%~
            (~a) must be given (but not both)." end-bar num-bars))
  (when end-bar
    ;; start-bar and end-bar are inclusive.
    (setf num-bars (1+ (- end-bar start-bar))))
  (loop for player in (players sc) do
       (delete-bars-aux sc start-bar num-bars player print))
  (update-write-time-sig (piece sc))
  ;; MDE Sat Apr 20 11:29:05 2013 -- t is ignored!
  ;; (update-write-time-sig2 (piece sc) t)
  (update-write-time-sig2 (piece sc))
  (incf-ids (tempo-map sc) (- num-bars) :start start-bar)
  ;; 27.5.11 have to update rehearsal-letters
  (setf (rehearsal-letters sc)
        (loop with eb = (+ start-bar num-bars -1)
           for rl in (rehearsal-letters sc) collect
             (if (> rl eb)
                 (- rl num-bars)
                 rl)))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A post-generation editing method

;;; ****m* slippery-chicken-edit/move-events
;;; DATE
;;; 20-Jul-2011 (Pula)
;;;
;;; DESCRIPTION
;;; Move a specified sequence of consecutive event objects from one player to
;;; another, deleting the events from the source player.
;;;
;;; NB: Although partial bars can be moved from the source player, the entire
;;;     bars of the target players are always overwritten, resulting in rests
;;;     in those segments of the target players' bars that do not contain the
;;;     moved material. This method thus best lends itself to moving into
;;;     target players parts that have rests in the corresponding bars.
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; - The ID of the source player.
;;; - The ID of the target player.
;;; - A number that is the first bar from which events are to be moved.
;;; - A number that is the first event within the start-bar that is to be
;;;   moved. 
;;; - A number that is the last bar from which events are to be moved.
;;; - A number that is the last event within the end-bar that is to be
;;;   moved. 
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :transposition. A positive or negative number that is the number of
;;;   semitones by which the copied material is to be first transposed. This
;;;   number can be a decimal number, in which case the resulting pitches will
;;;   be rounded to the nearest microtone (if the current tuning environment is
;;;   capable of microtones).
;;; - :consolidate-rests. T or NIL to indicate whether resulting consecutive
;;;   rests should be consolidated each into one longer rest.
;;;   T = consolidate. Default = T.
;;; 
;;; RETURN VALUE
;;; Returns T.
;;; 
;;; EXAMPLE
#|
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((bn (bassoon :midi-channel 1))
                     (vc (cello :midi-channel 2))))
        :tempo-map '((1 (q 60)))
        :set-palette '((1 ((c2 e2 d4 e4 f4 g4 a4 f5))))
        :set-map '((1 (1 1 1 1)))
        :rthm-seq-palette '((1 ((((4 4) e e e e e e e e))
                                :pitch-seq-palette ((1 2 3 4 5 6 7 8))))
                            (2 ((((4 4) (w))))))
        :rthm-seq-map '((1 ((bn (1 1 1 1))
                            (vc (2 2 2 2))))))))
  (move-events mini 'bn 'vc 2 3 3 2)
  (move-events mini 'bn 'vc 4 1 4 2 :transposition 4.5))

  => T

  |#
;;; SYNOPSIS
(defmethod move-events ((sc slippery-chicken) from-player to-player
                        start-bar start-event end-bar end-event
                        &key transposition (consolidate-rests t))
;;; ****
  (double-events sc from-player to-player start-bar start-event
                 end-bar end-event :transposition transposition)
  ;; now delete the events in the from-player
  (delete-events sc start-bar start-event end-bar end-event from-player
                 consolidate-rests))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken-edit/swap-events
;;; DATE
;;; June 10th 2025
;;; 
;;; DESCRIPTION
;;; Move/swap the events from one part to the other and vice-versa.
;;; 
;;; NB does not check that pitches are in range! Call force-in-range afterwards
;;; if necessary/applicable. Also, in case this method is to be called mulitple
;;; times, it does not call update-slots or update-instrument-slots
;;; 
;;; ARGUMENTS
;;; - the slippery-chicken obje ct
;;; - the first player to swap from/to (symbol)
;;; - the second player to swap from/to (symbol)
;;; - the start bar number (inclusive)
;;; - the end bar number (inclusive)
;;; 
;;; RETURN VALUE
;;; T
;;; 
;;; SYNOPSIS
(defmethod swap-events ((sc slippery-chicken) player1 player2 start-bar
                        end-bar)
;;; ****
  (let* ((p1 (get-player sc player1))
         (p2 (get-player sc player2))
         (mc1 (midi-channel p1))
         (mc2 (midi-channel p2))
         (mmc1 (microtones-midi-channel p1))
         (mmc2 (microtones-midi-channel p2)))
    (loop for bar-num from start-bar to end-bar
          for p1bar = (get-bar sc bar-num player1)
          for p2bar = (get-bar sc bar-num player2)
          for p1bar-clone = (clone p1bar)
          do
             (setf (rhythms p1bar)      ; (rhythms p2bar)
                   ;; tuplets, beams
                   (mapcar #'(lambda (e) (setf (player e) player1) e)
                           (rhythms p2bar))
                   (rhythms p2bar)
                   (mapcar #'(lambda (e) (setf (player e) player2) e)
                           (rhythms p1bar-clone))
                   (tuplets p1bar) (tuplets p2bar)
                   (tuplets p2bar) (tuplets p1bar-clone))
             (set-midi-channel p1bar mc1 mmc1)
             (set-midi-channel p2bar mc2 mmc2))
    t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; post-gen-editing method

;;; SAR Fri Apr 20 13:58:23 BST 2012: Added robodoc entry

;;; MDE comment:
;;; if update we update-slots for the whole sc object. 
;;; A nasty side-effect of this method is that at the moment is that any
;;; existing events in the doubling players at the beginning of the start-bar
;;; or end of the end-bar will be deleted, so this only works for copying notes
;;; into completely empty bars, not razor splicing.

;;; ****m* slippery-chicken-edit/double-events
;;; DATE
;;; 20-Jul-2011 (Pula)
;;;
;;; DESCRIPTION
;;; Copy the specified events from one player to the corresponding bars of one
;;; or more other players.
;;;
;;; NB: Although partial bars can be copied from the source player, the entire
;;;     bars of the target players are always overwritten, resulting in rests
;;;     in those segments of the target players' bars that do not contain the
;;;     copied material. This method thus best lends itself to copying into
;;;     target players parts that have rests in the corresponding bars.
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; - The ID of the player from whose part the events are to be copied.
;;; - The ID or a list of IDs of the player or players into whose parts the
;;;   copied events are to be placed.
;;; - An integer that is the number of the first bar from which the events are
;;;   to be copied.
;;; - An integer that is the number of the first event to be copied from the
;;;   specified start bar. This number is 1-based and counts rests and ties. 
;;; - An integer that is the number of the last bar from which the events are
;;;   to be copied.
;;; - NIL or an integer that is the number of the last event to be copied from
;;;   the specified end bar. This number is 1-based and counts rests and
;;;   ties. If NIL, all events from the given bar will be copied.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :transposition. A positive or negative number that is the number of
;;;   semitones by which the copied material is to be first transposed. This
;;;   number can be a decimal number, in which case the resulting pitches will
;;;   be rounded to the nearest microtone (if the current tuning environment is
;;;   capable of microtones).
;;; - :consolidate-rests. T or NIL to indicate whether resulting consecutive
;;;   rests should be consolidated each into one longer rest.
;;;   T = consolidate. Default = T.
;;; - :update. T or NIL to indicate whether to update the slots of the given
;;;   slippery-chicken object after copying. T = update. Default = T.
;;; - :auto-beam. T or NIL to indicate whether to call auto-beam on the doubled
;;;   notes. Default = T.
;;; - :pitches. T or NIL to indicate whether pitch information should be copied
;;;   over. This would usually be T but some methods might just want to
;;;   duplicate events as rests (e.g. orchestrate). Default = T.
;;;
;;; RETURN VALUE
;;; Returns T
;;; 
;;; EXAMPLE
#|
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((bsn (bassoon :midi-channel 1))
                     (tbn (tenor-trombone :midi-channel 2))
                     (vlc (cello :midi-channel 3))))
        :tempo-map '((1 (q 60)))
        :set-palette '((1 ((c2 e2 d4 e4 f4 g4 a4 f5))))
        :set-map '((1 (1 1 1 1 1 1)))
        :rthm-seq-palette '((1 ((((4 4) (w)))))
                            (2 ((((4 4) e e e e e e e e))
                                :pitch-seq-palette ((1 2 3 4 5 6 7 8)))))
        :rthm-seq-map '((1 ((bsn (1 1 1 1 1 1))
                            (tbn (1 1 1 1 1 1))
                            (vlc (2 2 2 2 2 2))))))))
  (double-events mini 'vlc '(bsn tbn) 2 3 4 2)
  (double-events mini 'vlc 'bsn 5 1 5 nil :transposition 3.5))

  => T

  |#
;;; SYNOPSIS
(defmethod double-events ((sc slippery-chicken) master-player doubling-players
                          start-bar start-event end-bar end-event
                          &key transposition (consolidate-rests t) (update t)
                            (pitches t) (auto-beam t))
;;; ****
  (setq doubling-players (force-list doubling-players))
  (unless start-bar (setq start-bar 1))
  (unless start-event (setq start-event 1))
  (unless end-bar (setq end-bar (num-bars sc)))
  ;; end-event handled below
  (loop for doubling-player in doubling-players do       
     ;; clone the master players bars
       (let* ((player-obj (get-data doubling-player (ensemble sc)))
              (mc (midi-channel player-obj))
              (mmc (microtones-midi-channel player-obj))
              ;; clone the master-player bars
              (mbars (loop for bn from start-bar to end-bar 
                        for bar = (clone (get-bar sc bn master-player))
                        do 
                        (unless bar
                          (error "slippery-chicken::double-events: Can't get ~
                                 bar no. ~a for ~a." bn master-player))
                        (set-midi-channel bar mc mmc)
                        collect bar)))
         ;; now handle the given transposition
         (when (and (numberp transposition)
                    (not (zerop transposition)))
           (setf mbars (loop for mb in mbars collect
                            (transpose mb transposition))))
         ;; now the transposing instruments.  NB we don't have to take the
         ;; transposition of the master ins into account because we'll be
         ;; working with its sounding pitches
         (when pitches
           (loop for bn from start-bar to end-bar 
            for mb in mbars ; the cloned master player bars
            for ins-transposition =
            (get-transposition-at-bar doubling-player bn sc)
            do 
            (if (zerop ins-transposition)
                (delete-written mb)     ; just in case master is transposing...
                (set-written mb (- ins-transposition)))))
         ;; now we have bar-to-bar copies we need to delete the events we
         ;; didn't request at the start of the start bar and end of the end bar
         (let* ((sb (first mbars))
                (eb (first (last mbars)))
                (nrs (num-rhythms eb))
                (ee (if end-event end-event nrs))
                (exit (1+ ee)))
           ;; don't forget that start-bar and end-bar could be one and the
           ;; same--shouldn't be a problem until we consolidate rests.
           (unless (= 1 start-event)
             (make-rests sb 1 (1- start-event)))
           (when (<= exit nrs)
             (make-rests eb exit))
           ;; MDE Thu Nov  8 19:35:33 2018 -- it's now an option; before it was
           ;; always called.
           (when auto-beam
             (auto-beam sb)
             (unless (= start-bar end-bar)
               (auto-beam eb)))
           (if consolidate-rests 
               (progn
                 (consolidate-rests sb)
                 (unless (= start-bar end-bar)
                   (consolidate-rests eb)))
               ;; gen-stats is called in consolidate-rests above
               (progn
                 (gen-stats sb)
                 (gen-stats eb))))
         ;; replace whatever was in the doubling players bars
         (loop for bn from start-bar to end-bar 
            for mb in mbars
            for db = (get-bar sc bn doubling-player) 
            do
              (setf (rhythms db) (rhythms mb))
              ;; MDE Sat Nov 10 12:30:42 2018 
              (set-player db doubling-player))
         (when update                   ; could be called elsewhere
           (update-slots sc))))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A post-generation editing method

;;; SAR Fri Apr 20 12:59:28 BST 2012: Added robodoc entry
;;; SAR Fri Apr 20 12:59:41 BST 2012: Removed MDE's original comment because
;;; taken nearly verbatim into robodoc

;;; ****m* slippery-chicken-edit/delete-events
;;; DATE
;;; 21-Jul-2011 (Pula)
;;;
;;; DESCRIPTION
;;; Turn notes into rests by setting the IS-REST slots of the specified
;;; consecutive event objects within the given slippery-chicken object to T.
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; - An integer that is the number of the first bar for which the notes are to
;;;   be changed to rests. 
;;; - An integer that is the index of the first event object within the
;;;   specified start bar for which the IS-REST slot is to be changed to
;;;   T. This number is 1-based and counts rests and ties. 
;;; - An integer that is the number of the last bar for which the notes are to
;;;   be changed to rests. 
;;; - An integer that is the index of the last event object within the
;;;   specified end bar for which the IS-REST slot is to be changed to T. This
;;;   number is 1-based and counts rests and ties. If NIL, apply the change to
;;;   all events in the given bar.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - A list of the IDs of the players whose parts are to be modified. If NIL,
;;;   apply the method to the parts of all players.
;;; - T or NIL to indicate whether to consolidate resulting consecutive rests
;;;   into one longer rest each. T = consolidate. Default = T.
;;; 
;;; RETURN VALUE
;;; Returns T.
;;; 
;;; EXAMPLE
#|
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((vc (cello :midi-channel 1))))
        :tempo-map '((1 (q 60)))
        :set-palette '((1 ((c2 e2 d4 e4 f4 g4 a4 f5))))
        :set-map '((1 (1 1 1 1)))
        :rthm-seq-palette '((1 ((((4 4) e e e e e e e e))
                                :pitch-seq-palette ((1 2 3 4 5 6 7 8)))))
        :rthm-seq-map '((1 ((vc (1 1 1 1))))))))
  (delete-events mini 2 2 3 nil 'vc))

  => T

  |#
;;; SYNOPSIS
(defmethod delete-events ((sc slippery-chicken) start-bar start-event end-bar
                          end-event &optional players (consolidate-rests t))
;;; ****
  (setf players (if players
                    (force-list players)
                    (players sc)))
  (loop for player in players do
       (loop for bar-num from start-bar to end-bar 
          for bar = (get-bar sc bar-num player)
          do
          (cond ((= bar-num start-bar)
                 (make-rests bar start-event (when (= start-bar end-bar)
                                               end-event)))
                ((= bar-num end-bar) ;; can't be start-bar by now....
                 (make-rests bar 1 end-event))
                (t (force-rest-bar bar)))
          (when consolidate-rests
            (consolidate-rests bar)
            ;; 25.7.11 (Pula)
            ;; (auto-put-tuplet-bracket-on-beats bar nil))
            ;; MDE Mon May  7 17:41:59 2012 -- use auto-tuplets instead of the
            ;; above  
            (auto-tuplets bar))
          ;; 26.9.11: this was before consolidate-rests
          (auto-beam bar)))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A post-generation editing method

;;; SAR Wed Apr 25 16:05:55 BST 2012: Added robodoc entry.

;;; ****m* slippery-chicken-edit/sc-force-rest
;;; DATE
;;; 23-Jul-2011 (Pula)
;;;
;;; DESCRIPTION
;;; Change the specified event object to a rest.  If events tied from this
;;; event should automatically be forced to rests also, use the sc-force-rest2
;;; method instead.
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; - An integer that is the number of the bar in which the rest is to be
;;;   forced.
;;; - An integer that is the number of the event within that bar which is to be
;;;   changed into a rest. This number is 1-based and counts tied notes but not
;;;   rests. 
;;; - The ID of the player whose part is to be modified.
;;;
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to indicate whether the specified bar should be automatically
;;;   beamed after the change has been made. NB: In general, calling auto-beam
;;;   is a good idea (esp. when deleting notes under an existing beam);
;;;   however, auto-beam may fail when addressing bars that contain notes
;;;   longer than one beat. T = automatically beam. Default = NIL.
;;;
;;; RETURN VALUE
;;; The new rthm-seq-bar object.
;;; 
;;; EXAMPLE
#|
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((vc (cello :midi-channel 1))))
        :set-palette '((1 ((a3 b3 c4 e4))))
        :set-map '((1 (1 1 1)))
        :rthm-seq-palette '((1 ((((2 4) q e s s))
                                :pitch-seq-palette ((1 2 3 4)))))
        :rthm-seq-map '((1 ((vc (1 1 1))))))))
  (sc-force-rest mini 2 3 'vc)
  (sc-force-rest mini 3 3 'vc t))

  =>

  RTHM-SEQ-BAR: time-sig: 3 (2 4), time-sig-given: T, bar-num: 3, 
  old-bar-nums: NIL, write-bar-num: NIL, start-time: 4.000, 
  start-time-qtrs: 4.0, is-rest-bar: NIL, multi-bar-rest: NIL, 
  show-rest: T, notes-needed: 3, 
  tuplets: NIL, nudge-factor: 0.35, beams: ((1 2)), 
  current-time-sig: 3, write-time-sig: NIL, num-rests: 1, 
  num-rhythms: 4, num-score-notes: 3, parent-start-end: NIL, 
  missing-duration: NIL, bar-line-type: 2, 
  player-section-ref: (1 VC), nth-seq: 2, nth-bar: 0, 
  rehearsal-letter: NIL, all-time-sigs: (too long to print) 
  sounding-duration: 1.750, 
  rhythms: (
[...]

|#
;;; SYNOPSIS
(defmethod sc-force-rest ((sc slippery-chicken) bar-num note-num player
                          &optional auto-beam)
;;; ****
  (let* ((bar (get-bar sc bar-num player))
         (event (when bar (get-nth-non-rest-rhythm (1- note-num) bar))))
    (if event
        (progn
          (force-rest event)
          (when auto-beam
            (auto-beam bar))
          (gen-stats bar))
        (error "slippery-chicken::sc-force-rest: can't get note ~a at bar ~a"
               note-num bar-num))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 22.8.13

;;; ****m* slippery-chicken-edit/sc-force-rest2
;;; DESCRIPTION
;;; Turn events into rests, doing the same with any following tied events.  
;;; 
;;; NB As it is foreseen that this method may be called many times iteratively,
;;; there is no call to check-ties, auto-beam, consolidate-rests, or
;;; update-instrument-slots (for statistics)--it is advised that these methods
;;; are called once the last call to this method has been made.  gen-stats is
;;; however called for each affected bar, so the number of rests vs. notes
;;; should be consistent with the new data.
;;;
;;; NB This fill trigger an error if attempted with grace-notes
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object
;;; - The bar number (integer)
;;; - The event number in the bar (integer, counting from 1)
;;; - The player name (symbol)
;;; 
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to delete all marks when a note is turned into a rest. Default =
;;;   NIL.  
;;; - A function object to be called on error (could be #'error (default),
;;;  #'warn, #'print or simply NIL for no error)
;;; 
;;; RETURN VALUE
;;; The number of events turned into rests.
;;; 
;;; EXAMPLE
#|
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((vc (cello :midi-channel 1))))
        :set-palette '((1 ((a3 b3 c4 e4))))
        :set-map '((1 (1 1 1)))
        :rthm-seq-palette 
        '((1 ((((4 4) - e.. 32 - h.) (+w) (+w) ((w)) ((h) (e) q e)
               (+q - +s e. - +h) (+w) (+w) ((w))))))
        :rthm-seq-map '((1 ((vc (1 1 1))))))))
  (sc-force-rest2 mini 1 3 'vc))
=> 3
|#
;;; SYNOPSIS
(defmethod sc-force-rest2 ((sc slippery-chicken) bar-num event-num player
                           &optional delete-marks (on-error #'error))
;;; ****
  (let ((start-event (get-event sc bar-num event-num player nil))
  ;;                                                         ^ nil = no error
        (bars-affected '())
        (count 0))
    (flet ((init-error (text)
             (when (functionp on-error)
               (funcall 
                on-error "~&slippery-chicken-edit::sc-force-rest2: ~%~
                          for bar number ~a, event number ~a, player: ~
                          ~a~%~a" 
                        bar-num event-num player text))
             ;; hack to make sure we return nil if on-error is something other
             ;; than #'error
             (setf player nil) 
             nil))
      (cond ((not start-event) 
             (init-error "No such event."))
            ((is-rest start-event)
             (init-error "Event is already a rest."))
            ((is-grace-note start-event)
             (init-error "Event is a grace note."))
            ((is-tied-to start-event)
             (init-error "Event is tied to: ~
                          call this method with an attacked note only."))))
  (when player                          ; hack to make sure we return nil
    (loop with bn = bar-num 
       with en = (1- event-num)
       with bar = (get-bar sc bar-num player) 
       with tied
       with e
       do
       (when (>= en (num-rhythms bar))
         (incf bn)
         (setf en 0
               bar (get-bar sc bn player)))
       (setf e (get-nth-event en bar))
       (if (is-rest e)
           (return)
           (progn
             (incf count)
             (setf tied (is-tied-from e))
             (pushnew bn bars-affected)
             (force-rest e)
             ;; MDE Sat May  8 17:22:56 2021, Heidhausen
             (when delete-marks (delete-marks e))
             (if tied
                 (incf en)
                 (return))))))
  (loop for ba in bars-affected do (gen-stats (get-bar sc ba player)))
  count))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken-edit/force-rest-bars
;;; DESCRIPTION
;;; Delete all notes from the specified bars and replace them with full-bar
;;; rests. 
;;; 
;;; NB: The start-bar and end-bar index numbers are inclusive
;;;
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; - An integer that is the number of the first bar to change to a full bar of
;;;   rest. 
;;; - An integer that is the number of the last bar to change to a full bar of
;;;   rest. If NIL then we process all bars.
;;; - A list containing the IDs of the players in whose parts the full-bar
;;;   rests are to be forced. If NIL then all players will be processed.
;;;
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to indicate that only bars containing exclusively rests should be
;;;   processed.
;;;
;;; RETURN VALUE
;;; Returns the number of bars processed (integer).
;;; 
;;; EXAMPLE
#|
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((vn (violin :midi-channel 1))
                     (va (viola :midi-channel 2))
                     (vc (cello :midi-channel 3))))
        :tempo-map '((1 (q 60)))
        :set-palette '((1 ((c4 e4 g4 b4 d5 f5 a5 c6))))
        :set-map '((1 (1 1 1 1 1 1)))
        :rthm-seq-palette '((1 ((((4 4) e e e e e e e e))
                                :pitch-seq-palette ((1 2 3 4 5 6 7 8))))) 
        :rthm-seq-map '((1 ((vn (1 1 1 1 1 1))
                            (va (1 1 1 1 1 1))
                            (vc (1 1 1 1 1 1))))))))
  (force-rest-bars mini 3 5 '(vn vc)))

=> NIL

|#
;;; SYNOPSIS
(defmethod force-rest-bars ((sc slippery-chicken) start-bar end-bar players
                            &optional only-if-all-rests)
;;; ****
  (unless players (setq players (players sc)))
  (unless (listp players) (setq players (list players)))
  ;; MDE Mon Oct 29 12:40:52 2018 
  (unless end-bar (setq end-bar (num-bars sc)))
  (loop with processed = 0
     for bar-num from start-bar to end-bar do
       (loop for player in players 
          for bar = (get-bar sc bar-num player)
          do
            (unless bar
              (error "slippery-chicken::force-rest-bars: no bar ~a for ~a"
                     bar-num player))
            (when (or (not only-if-all-rests)
                      (and only-if-all-rests (all-rests? bar)))
              (force-rest-bar bar)
              (incf processed)))
     finally (return processed)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken-edit/force-artificial-harmonics
;;; DESCRIPTION
;;; For string scoring purposes only: Transpose the pitch of the given event
;;; object down two octaves and add the harmonic symbol at the perfect fourth.
;;;
;;; If this results in a fingered pitch (or even a touched perfect fourth) that
;;; is out of the range of the instrument, a warning will be printed to the
;;; Listener, the pitch will not be transposed, and the harmonic diamond will
;;; not be added.
;;;
;;; NB This method assumes the player doesn't change instrument during the bars
;;; to be processed. 
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; - The ID of the player whose part is to be changed.
;;; - An integer that is the number of the first bar in which artificial
;;;   harmonics are to be created.  
;;; - An integer that is the number of the first event in that bar that is to
;;;   be changed into an artificial harmonic.
;;; - An integer that is the number of the last bar in which artificial
;;;   harmonics are to be created.  
;;; 
;;; OPTIONAL ARGUMENTS
;;; - An integer that is the number of the first event in that bar that is to
;;;   be changed into an artificial harmonic. If no end-event is specified, all
;;;   event objects in the last bar will be changed to artificial harmonics.
;;; - T or NIL to trigger warnings when forcing would take a fingered pitch out
;;;   of the instrument's range, or when we encounter chords or rests
;;; - T or NIL to force natural-harmonics to become artificial. Default = T.
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
        :tempo-map '((1 (q 60)))
        :set-palette '((1 ((c4 f4 b4 e5 a5 d6 g7 c8))))
        :set-map '((1 (1 1 1)))
        :rthm-seq-palette '((1 ((((4 4) e e e e e e e e))
                                :pitch-seq-palette ((1 2 3 4 5 6 7 8))))) 
        :rthm-seq-map '((1 ((vn (1 1 1))))))))
  (force-artificial-harmonics mini 'vn 2 3 3 2))

=> T

|#
;;; SYNOPSIS
(defmethod force-artificial-harmonics ((sc slippery-chicken) player start-bar
                                       start-event end-bar
                                       &optional end-event (warn t)
                                         (naturals t))
;;; ****
  ;; MDE Mon Apr 23 09:10:09 2012 -- assumes we don't change player in the
  ;; midst of making these changes.  Uses instrument to ensure we don't go out
  ;; of range.  
  (let ((ins (get-instrument-for-player-at-bar player start-bar sc)))
    (loop for e in (get-events-from-to sc player start-bar start-event end-bar
                                       end-event)
       do
         (unless (or (is-rest e) (is-chord e))
           (when (or naturals (not (has-mark e 'harm)))
             (force-artificial-harmonic e ins warn naturals))))
    t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken-edit/force-natural-harmonics
;;; DATE
;;; 
;;; 
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
(defmethod force-natural-harmonics ((sc slippery-chicken) player start-bar
                                    &optional start-event end-bar end-event
                                      (warn t) (tolerance 15))
;;; ****
  ;; assumes we don't change player in the midst of making these changes.  Uses
  ;; instrument to ensure we don't go out of range.
  (let ((ins (get-instrument-for-player-at-bar player start-bar sc)))
    (loop for e in (get-events-from-to sc player start-bar start-event end-bar
                                       end-event)
       do
         (unless (or (is-rest e) (is-chord e))
           (force-natural-harmonic e ins warn tolerance)))
    t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken-edit/force-harmonics
;;; DATE
;;; 
;;; 
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
(defmethod force-harmonics ((sc slippery-chicken) player start-bar
                            &key (start-event 1) end-bar end-event warn
                              (tolerance 15))
;;; ****
  (force-natural-harmonics sc player start-bar start-event end-bar end-event
                           warn tolerance)
  (force-artificial-harmonics sc player start-bar start-event end-bar end-event
                              ;; don't replace the new natural harmonics
                              warn nil)
  t)
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken-edit/set-cautionary-accidental
;;; DATE
;;; 28-Sep-2011
;;;
;;; DESCRIPTION
;;; A post-generation editing method: Place a cautionary accidental
;;; (sharp/flat/natural sign in parentheses) before a specified note. 
;;;
;;; NB: Adding cautionary accidentals to pitches within chords is currently
;;;     only possible in LilyPond output. Adding cautionary accidentals to
;;;     single pitches is possible in both CMN and LilyPond.
;;;
;;; NB: Since the cmn-display and write-lp-data-for-all methods call
;;;     respell-notes by default, that option must be explicitly set to NIL
;;;     within the calls to those methods in order for this method to be
;;;     effective. 
;;;
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; - An integer that is the number of the bar in which to add the cautionary
;;;   accidental. 
;;; - An integer or a 2-item list of integers that is the number of the note
;;;   within that bar to which to add the cautionary accidental. This number is
;;;   1-based and counts ties. If a 2-item list such, this indicates that the
;;;   pitch is within a chord; e.g., '(1 2) indicates that a cautionary
;;;   accidental should be added to the 2nd pitch up from the bottom of the
;;;   chord located at the 1st note position in the bar.
;;; - The ID of the player to whose part the cautionary accidental is to be
;;;   added. 
;;;
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to indicate whether to add the cautionary accidental to only the
;;;   written pitch or only the sounding pitch. T = written only.
;;;   Default = NIL.
;;; 
;;; RETURN VALUE
;;; Returns T.
;;; 
;;; EXAMPLE
#|
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((cl (b-flat-clarinet :midi-channel 1))
                     (pn (piano :midi-channel 2))))
        :set-palette '((1 ((ds3 e3 fs3 af3 bf3 c4 ef4 fs4))))
        :set-map '((1 (1 1 1)))
        :rthm-seq-palette '((1 ((((2 4) q e s s))
                                :pitch-seq-palette ((1 2 (3) 4))
                                :marks (fff 1 ppp 3))))
        :rthm-seq-map '((1 ((cl (1 1 1))
                            (pn (1 1 1))))))))
  (respell-notes mini)
  (set-cautionary-accidental mini 3 2 'cl t)
  (set-cautionary-accidental mini 2 1 'pn)
  (set-cautionary-accidental mini 2 2 'pn)
  (set-cautionary-accidental mini 3 '(3 3) 'pn)
  (write-lp-data-for-all mini :respell-notes nil))

=> T

|#
;;; SYNOPSIS
(defmethod set-cautionary-accidental ((sc slippery-chicken) bar-num note-num
                                      player &optional written)
;;; **** 
  (cautionary-accidental-aux sc bar-num note-num player t written))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken-edit/unset-cautionary-accidental
;;; DESCRIPTION
;;; A post-generation editing method: Remove the parentheses from a cautionary
;;; accidental (leaving the accidental itself) by setting the
;;; ACCIDENTAL-IN-PARENTHESES slot of the contained pitch object to NIL.
;;;
;;; NB: Since respell-notes is called by default within cmn-display and
;;;     write-lp-data-for-all, that option must be explicitly set to NIL for
;;;     this method to be effective.
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; - An integer that is the number of the bar in which the cautionary
;;;   accidental is to be unset.
;;; - An integer that is the number of the note in that bar for which the
;;;   cautionary accidental is to be unset.
;;; - The ID of the player whose part is to be changed.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to indicate whether to unset the cautionary accidental for the
;;;   written part only (for transposing instruments). 
;;;   T = written only. Default = NIL.
;;; 
;;; RETURN VALUE
;;; Returns NIL.
;;; 
;;; EXAMPLE
#|
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((cl (b-flat-clarinet :midi-channel 1))
                     (vn (violin :midi-channel 2))))
        :set-palette '((1 ((cs4 ds4 fs4))))
        :set-map '((1 (1 1)))
        :rthm-seq-palette '((1 ((((4 4) e e e e e e e e))
                                :pitch-seq-palette ((1 2 3 2 1 2 3 2)))))
        :rthm-seq-map '((1 ((cl (1 1))
                            (vn (1 1))))))))
  (respell-notes mini)
  (unset-cautionary-accidental mini 2 5 'vn)
  (unset-cautionary-accidental mini 2 7 'cl t)
  (cmn-display mini :respell-notes nil))

|#
;;; SYNOPSIS
(defmethod unset-cautionary-accidental ((sc slippery-chicken) bar-num note-num
                                        player &optional written)
;;; **** 
  (cautionary-accidental-aux sc bar-num note-num player nil written))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken-edit/add-arrow-to-events
;;; DATE
;;; April 9th 2012
;;; 
;;; DESCRIPTION
;;; Adds an arrow above the specified notes of a slippery-chicken object,
;;; coupled with text to be printed in the score at the start and end of the
;;; arrow. Can be used, for example, for transitions from one playing state to
;;; another.
;;;
;;; If no text is desired, this must be indicated by a space in quotes (" ")
;;; rather than empty quotes ("").
;;;
;;; See also the add-arrow method in the event class.
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; - A text string for the beginning of the arrow.
;;; - A text string for the end of the arrow.
;;; - A list that is the starting event reference, in the form (bar-number
;;;   event-number). Event numbers count from 1 and include rests and tied
;;;   notes.
;;; - A list that is the end event reference, in the form (bar-number
;;;   event-number).
;;; - The ID of the player to whose part the arrow should be attached.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to indicate whether or not to print a warning when trying to 
;;;   attach an arrow and accompanying marks to a rest. 
;;;   T = print warning. Default = NIL. 
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
        :ensemble '(((pno (piano :midi-channel 1))))
        :tempo-map '((1 (q 60)))
        :set-palette '((1 ((c4 d4 f4 g4 a4 c5 d5 f5 g5 a5 c6)))
                       (2 ((cs4 ds4 fs4 gs4 as4 cs5 ds5 fs5 gs5 as5))))
        :set-map '((1 (1 1 1 1 1 1)))
        :rthm-seq-palette '((1 ((((2 4) q q))
                                :pitch-seq-palette ((1 (2))))))
        :rthm-seq-map '((1 ((pno (1 1 1 1 1 1))))))))
  (add-arrow-to-events mini "here" "there" '(1 1) '(5 1) 'pno)
  (write-lp-data-for-all mini))

|#
;;; SYNOPSIS
(defmethod add-arrow-to-events ((sc slippery-chicken) start-text end-text
                                event1-ref event2-ref player
                                &optional warn-rest)
;;; ****
  ;;         will signal an error if these events don't exist
  (let ((e1 (get-event sc (first event1-ref) (second event1-ref) player))
        (e2 (get-event sc (first event2-ref) (second event2-ref) player)))
    (add-arrow e1 start-text end-text warn-rest)
    (end-arrow e2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* slippery-chicken/change-bar-line-type
;;; DESCRIPTION
;;; Change single to double or repeat bar lines and vice-versa.  NB This is a
;;; score function only, i.e., if you add repeat bar lines these will not (yet) 
;;; be reflected in playback with MIDI or CLM.
;;; 
;;; ARGUMENTS 
;;; - the slippery-chicken object
;;; - the bar number at the end of which you want the bar line to change
;;; - bar line type: 0 = normal, 1 = double bar, 2 = final double bar, 3 =
;;;   begin repeat, 4 = begin and end repeat, 5 = end repeat. As remembering
;;;   these numbers is a bit arbitrary, you can also just pass symbols, if you
;;;   prefer: 'normal, 'double-bar, 'final-double, 'begin-repeat,
;;;   'begin-end-repeat, 'end-repeat
;;; RETURN VALUE  
;;; returns the bar-line type 
;;; 
;;; EXAMPLE
#|
(let ((min
       (make-slippery-chicken
        '+minimum+
        :instrument-palette +slippery-chicken-standard-instrument-palette+
        :ensemble '(((fl (flute :midi-channel 1))))
        :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
        :set-map '((1 (1)))
        :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -)))))
        :rthm-seq-map '((1 ((fl (1))))))))
;; this piece only has one bar so the bar line will be 2 by default ;
  (print (bar-line-type (get-bar min 1 'fl)))
  (change-bar-line-type min 1 'double-bar)
  (bar-line-type (get-bar min 1 'fl)))
=> 
...
2
1
|#
;;; 
;;; SYNOPSIS
(defmethod change-bar-line-type ((sc slippery-chicken) bar-num type)
;;; ****
  (let ((types '(normal double-bar final-double begin-repeat begin-end-repeat
                 end-repeat)))
    (when (symbolp type)
      (setq type (position type types)))
    (let ((players-bars (get-bar sc bar-num)))
      (loop for bar in players-bars do
           (setf (bar-line-type bar) type)))
    (values (nth type types) type)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken-edit/map-over-notes
;;; AUTHOR
;;; Daniel Ross (mr.danielross[at]gmail[dot]com) 
;;; 
;;; DATE
;;; 1 September 2019, London
;;; 
;;; DESCRIPTION
;;; A simple method that mimics the behaviour of map-over-bars but on notes
;;; instead. At its heart is a next-event loop. Use this method to apply an
;;; arbitrary function to every note within a specific range of bars.
;;; NB This works on NOTES only, rests will be skipped!
;;; 
;;; ARGUMENTS
;;; - A slippery chicken object
;;; - A number that is the first bar to which the function should be
;;;   applied. Default = NIL in which case 1 will be used. 
;;; - A number that is the last bar to which the function should be
;;;   applied. Default = NIL in which case all bars will be processed. 
;;; - A list of the IDs of the players to whose parts the function should be
;;;   applied. Can also be a single symbol. If NIL then all players will be
;;;   processed.  
;;; - The method or function itself. This can be a user-defined function or the
;;;   name of an existing method or function.  It should take at least one
;;;   argument, a note event (can be a single pitch or a chord), and any other
;;;   arguments as supplied.  
;;; 
;;; OPTIONAL ARGUMENTS
;;; - Any additional argument values the specified method/function may
;;;   take or require. See the thin method below for an example that uses
;;;   additional arguments. 
;;; 
;;; RETURN VALUE
;;; - A list containing the number of notes changed per instrument
;;; 
;;; EXAMPLE
#|
(let* ((mini
          (make-slippery-chicken
           '+mini+
           :ensemble '(((sax ((alto-sax tenor-sax) :midi-channel 1))))
           :instrument-change-map '((1 ((sax ((1 alto-sax) (3 tenor-sax)))))
                                    (2 ((sax ((2 alto-sax) (5 tenor-sax)))))
                                    (3 ((sax ((3 alto-sax) (4 tenor-sax))))))
           :set-palette '((1 ((c2 d2 g2 a2 e3 fs3 b3 cs4 fs4 gs4 ds5 f5 bf5))))
           :set-map '((1 (1 1 1 1 1))
                      (2 (1 1 1 1 1))
                      (3 (1 1 1 1 1)))
           :rthm-seq-palette '((1 ((((4 4) h e (s) (s) e+s+s))
                                   :pitch-seq-palette ((1 2 3)))))
           :rthm-seq-map '((1 ((sax (1 1 1 1 1))))
                           (2 ((sax (1 1 1 1 1))))
                           (3 ((sax (1 1 1 1 1))))))))
        (print (is-chord (get-note mini (num-bars mini) 3 'sax)))
        (print (map-over-notes mini 1 nil nil #'add-pitches 'c4 'd4))
        (is-chord (get-note mini (num-bars mini) 3 'sax)))

=> NIL
   (45)
   3

|#
;;; SYNOPSIS
(defmethod map-over-notes ((sc slippery-chicken) start-bar end-bar players
                           function &rest further-args)
;;; ****
  ;; DJR Tue  3 Sep 2019 17:30:32 BST
  ;; Changed to use aux method
  (map-over-events-aux sc start-bar end-bar players t function further-args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken-edit/map-over-events
;;; AUTHOR
;;; Daniel Ross (mr.danielross[at]gmail[dot]com) 
;;; 
;;; DATE
;;; 1 September 2019, London
;;; 
;;; DESCRIPTION
;;; A simple method that mimics the behaviour of map-over-bars but on events
;;; instead. At its heart is a next-event loop. Use this method to apply an
;;; arbitrary function to every event within a specific range of bars.
;;; 
;;; ARGUMENTS
;;; - A slippery chicken object
;;; - A number that is the first bar to which the function should be
;;;   applied. Default = NIL in which case 1 will be used. 
;;; - A number that is the last bar to which the function should be
;;;   applied. Default = NIL in which case all bars will be processed. 
;;; - A list of the IDs of the players to whose parts the function should be
;;;   applied. Can also be a single symbol. If NIL then all players will be
;;;   processed.  
;;; - The method or function itself. This can be a user-defined function or the
;;;   name of an existing method or function.  It should take at least one
;;;   argument, an event, and any other arguments as supplied.  
;;; 
;;; OPTIONAL ARGUMENTS
;;; - Any additional argument values the specified method/function may
;;;   take or require. See the thin method below for an example that uses
;;;   additional arguments. 
;;; 
;;; RETURN VALUE
;;; - A list containing the number of events changed per instrument
;;; 
;;; EXAMPLE
#|
(let* ((mini
          (make-slippery-chicken
           '+mini+
           :ensemble '(((sax ((alto-sax tenor-sax) :midi-channel 1))))
           :instrument-change-map '((1 ((sax ((1 alto-sax) (3 tenor-sax)))))
                                    (2 ((sax ((2 alto-sax) (5 tenor-sax)))))
                                    (3 ((sax ((3 alto-sax) (4 tenor-sax))))))
           :set-palette '((1 ((c2 d2 g2 a2 e3 fs3 b3 cs4 fs4 gs4 ds5 f5 bf5))))
           :set-map '((1 (1 1 1 1 1))
                      (2 (1 1 1 1 1))
                      (3 (1 1 1 1 1)))
           :rthm-seq-palette '((1 ((((4 4) h e (s) (s) e+s+s))
                                   :pitch-seq-palette ((1 2 3)))))
           :rthm-seq-map '((1 ((sax (1 1 1 1 1))))
                           (2 ((sax (1 1 1 1 1))))
                           (3 ((sax (1 1 1 1 1))))))))
        (print (is-rest (get-event mini (num-bars mini) 2 'sax)))
        (print (map-over-events mini 1 nil nil #'force-rest))
        (is-rest (get-event mini (num-bars mini) 3 'sax)))

=> NIL
   (105)
   T

|#
;;; SYNOPSIS
(defmethod map-over-events ((sc slippery-chicken) start-bar end-bar players
                           function &rest further-args)
;;; ****
  ;; DJR Tue  3 Sep 2019 17:30:32 BST
  ;; Changed to use aux method
  (map-over-events-aux sc start-bar end-bar players nil function further-args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Mon Jun 11 18:22:24 2012 -- returns a flat list of the results of
;;; calling the function on all of the bars for each instrument.

;;; MDE Wed Jun 13 16:40:47 2012 -- updated so we can specify a bar range and
;;; players.  If end-bar is nil, we'll go to the last bar; if players is nil,
;;; we'll process all players

;;; SAR Mon Jun 18 17:04:15 BST 2012: Added robodoc entry
;;; SAR Mon Jul  2 16:21:03 BST 2012: Expanded robodoc entry

;;; ****m* slippery-chicken-edit/map-over-bars
;;; DESCRIPTION
;;; Apply the specified method/function to the bars (all rthm-seq-bar objects)
;;; of one or more players' parts in the given slippery-chicken object. 
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; - A number that is the first bar to which the function should be
;;;   applied. Default = NIL in which case 1 will be used. 
;;; - A number that is the last bar to which the function should be
;;;   applied. Default = NIL in which case all bars will be processed. 
;;; - A list of the IDs of the players to whose parts the function should be
;;;   applied. Can also be a single symbol. If NIL then all players will be
;;;   processed.  
;;; - The method or function itself. This can be a user-defined function or the
;;;   name of an existing method or function.  It should take at least one
;;;   argument, a rthm-seq-bar, and any other arguments as supplied. 
;;; 
;;; OPTIONAL ARGUMENTS
;;; - Any additional argument values the specified method/function may
;;;   take or require. See the thin method below for an example that uses
;;;   additional arguments. 
;;; 
;;; RETURN VALUE
;;; - A list of the rthm-seq-bar objects that were modified.  NB This might be
;;;   a long list, and, depending on your Lisp implementation, formatting of the
;;;   bars might cause Lisp to appear to 'hang'.
;;; 
;;; EXAMPLE
#|

(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((sax ((alto-sax tenor-sax) :midi-channel 1))))
        :instrument-change-map '((1 ((sax ((1 alto-sax) (3 tenor-sax)))))
                                 (2 ((sax ((2 alto-sax) (5 tenor-sax)))))
                                 (3 ((sax ((3 alto-sax) (4 tenor-sax))))))
        :set-palette '((1 ((c2 d2 g2 a2 e3 fs3 b3 cs4 fs4 gs4 ds5 f5 bf5))))
        :set-map '((1 (1 1 1 1 1))
                   (2 (1 1 1 1 1))
                   (3 (1 1 1 1 1)))
        :rthm-seq-palette '((1 ((((4 4) h e (s) (s) e+s+s))
                                :pitch-seq-palette ((1 2 3)))))
        :rthm-seq-map '((1 ((sax (1 1 1 1 1))))
                        (2 ((sax (1 1 1 1 1))))
                        (3 ((sax (1 1 1 1 1))))))))
  (print (map-over-bars mini 1 nil nil #'consolidate-notes nil 'q)))

=>
(
RTHM-SEQ-BAR: time-sig: 2 (4 4), time-sig-given: T, bar-num: 1, 
[...]
RTHM-SEQ-BAR: time-sig: 2 (4 4), time-sig-given: T, bar-num: 2, 
[...]
RTHM-SEQ-BAR: time-sig: 2 (4 4), time-sig-given: T, bar-num: 3, 
[...]
RTHM-SEQ-BAR: time-sig: 2 (4 4), time-sig-given: T, bar-num: 4, 
[...]
)

|#
;;; SYNOPSIS
(defmethod map-over-bars ((sc slippery-chicken) start-bar end-bar players
                          function &rest further-args)
;;; ****
  (unless end-bar
    (setf end-bar (num-bars sc)))
  (unless start-bar
    (setf start-bar 1))
  (unless players
    (setf players (players sc)))
  ;; MDE Thu Dec  6 12:51:51 2012
  (unless (listp players)
    (setf players (list players)))
  (loop for player in players appending
       (loop
          for bnum from start-bar to end-bar
          for bar = (get-bar sc bnum player)
          collect
            (apply function (cons bar further-args)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken-edit/thin
;;; DATE
;;; 19th September 2018, Heidhausen
;;; 
;;; DESCRIPTION
;;; Thin out events in a slippery-chicken object using an activity curve (which
;;; is deterministic, not random/stochastic). This turns existing notes into
;;; rests. As this is an expensive method but may be called more than once, it
;;; is up to the user to call consolidate-rests and/or update-slots when ready.
;;; NB See note to :auto-tuplets below re. potential problems.
;;; 
;;; ARGUMENTS
;;; - the (fully-initialised) slippery-chicken object
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments
;;; - :start-bar. The bar to start at. Default = NIL = 1
;;; - :end-bar. The bar to end at. Default = NIL = last bar in piece/object
;;; - :players. A symbol or list of symbols for the players to be processed.
;;; - :curve. A list of breakpoints representing the level of activity. Y-values
;;;   should range over 0 (all events forced to rests) to 10 (no rests
;;;   created). X-values may range over any arbitrary scale but NB that this
;;;   will be rescaled to map over the number of bars in the whole piece,
;;;   despite the start-bar/end-bar arguments. However this rescaling can be
;;;   avoided via the next argument. Default = '(0 1 100 10)
;;; - :rescale-curve. Whether to process the x-values of :curve to range over 
;;;   the number of bars in the piece. Default = T = rescale.
;;; -:auto-tuplets. T or NIL to indicate whether auto-tuplets should be called
;;;   when thinning a bar is finished. Bear in mind that tuplet data can become
;;;   corrupted during this method, as individual events initially reference
;;;   existing tuplet bracket data, but when these are forced to rests (and
;;;   especially when they're consolidated into longer rests) these may no
;;;   longer exist. So if e.g. generated music-xml files have extra beats in
;;;   bars, try setting :auto-tuplets T and all may be well again. Default =
;;;   NIL. 
;;; 
;;; RETURN VALUE
;;; - the processed slippery-chicken object
;;; 
;;; SYNOPSIS
(defmethod thin ((sc slippery-chicken) &key start-bar end-bar players
                                       (curve '(0 1 100 10)) auto-tuplets
                                       (rescale-curve t))
;;; **** 
  (let* ((al (make-al))
         (cve (if rescale-curve
                (new-lastx curve (1- (num-bars sc)))
                curve)))
    (map-over-bars
     sc start-bar end-bar players
     #'(lambda (bar acurve)
         (let ((count 0))
           (loop with anum = (interpolate (1- (bar-num bar)) acurve)
                 for e in (rhythms bar) do
                   (when (and (needs-new-note e)
                              (not (active al anum))
                              ;; MDE Wed Jan  1 16:48:57 2020
                              (not (is-grace-note e)))
                     ;; note that this is a pretty inefficient way of doing
                     ;; things but we need to take care of ties properly so
                     ;; canceling individual events via force-rest won't work
                     (sc-force-rest2 sc (bar-num bar) (1+ (bar-pos e))
                                     (player e))
                     (incf count)))
           (when (and auto-tuplets (> count 0))
             (auto-tuplets bar))))
     cve))
  sc)
           
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken-edit/force-in-range
;;; DATE
;;; October 19th 2018, Heidhausen
;;; 
;;; DESCRIPTION
;;; Force all pitches (and chords) to be within the range of the instruments who
;;; play them. Of course this is usually a part of the slippery-chicken pitch
;;; selection algorithm but sometimes we change pitches (algorithmically in
;;; particular) and this might bring them out of range. Notes that are out of
;;; range will be transposed up and down the required number of octaves. This
;;; applies to single pitches in chords also. Instrument changes will be
;;; respected on a bar-by-bar basis. See also the three force-in-range methods
;;; in instrument.lsp 
;;; 
;;; ARGUMENTS
;;; - the slippery-chicken object
;;; - a symbol or list of symbols for the players to be processed. Default = NIL
;;;   = all players.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :start-bar. The bar to start at. Default = NIL = 1
;;; - :end-bar. The bar to end at. Default = NIL = last bar in piece/object
;;; - any other keyword arguments to the force-in-range instrument method
;;; 
;;; RETURN VALUE
;;; the modified slippery-chicken object
;;; 
;;; SYNOPSIS
(defmethod force-in-range ((sc slippery-chicken) players
                           &rest keyargs &key &allow-other-keys)
;;; ****
  (map-over-bars
   sc (second (member :start-bar keyargs)) (second (member :end-bar keyargs))
   players
     #'(lambda (bar sc)
         (let ((ins (get-instrument-for-player-at-bar (player bar) bar sc)))
           (loop for e in (rhythms bar) do
                (apply #'force-in-range (cons ins (cons e keyargs))))))
     sc) ; NB this sc is necessary to pass it to the lambda fun
  sc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken-edit/map-over-sequenzes
;;; DATE
;;; February 2nd 2018, Heidhausen
;;; 
;;; DESCRIPTION
;;; Apply a function to each sequenz object in a slippery-chicken piece. Note
;;; that as a sequenz is a subclass of rthm-seq (and bar-holder) and that
;;; rthm-seq has its rthm-seq-bars in the bars slot, we've got access to all
;;; kinds of data which we can switch on in the function passed here,
;;; e.g. player-section-ref, rsp-id
;;; 
;;; ARGUMENTS
;;; - the slippery-chicken object
;;; - the player(s) to process (single or list of symbols). If nil then all
;;;   players will be processed. 
;;; - the function to apply to each sequenz. This should accept a sequenz object
;;;   as its first argument.  
;;; 
;;; OPTIONAL ARGUMENTS
;;; &rest: further arguments to be passed to the given function
;;; 
;;; RETURN VALUE
;;; T
;;;
;;; EXAMPLE
#|
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((hn (french-horn :midi-channel 1))
                     (vc (cello :midi-channel 2))))
        :set-palette '((1 ((f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5))))
        :set-map '((1 (1 1 1 1 1))
                   (2 (1 1 1 1 1))
                   (3 (1 1 1 1 1)))
        :rthm-seq-palette '((1 ((((4 4) h q e s s))
                                :pitch-seq-palette ((1 2 3 4 5))))
                            (2 ((((4 4) h h))
                                :pitch-seq-palette ((1 2)))))
        :rthm-seq-map '((1 ((hn (1 1 1 1 1))
                            (vc (1 1 1 1 1))))
                        (2 ((hn (2 2 2 2 2))
                            (vc (2 2 2 2 2))))
                        (3 ((hn (1 1 1 1 1))
                            (vc (1 1 1 1 1))))))))
  (map-over-sequenzes mini 'hn 
                      #'(lambda (seq) (print (player-section-ref seq)))))
=> 
(1 HN) 
(1 HN) 
(1 HN) 
(1 HN) 
(1 HN) 
(2 HN) 
(2 HN) 
(2 HN) 
(2 HN) 
(2 HN) 
(3 HN) 
(3 HN) 
(3 HN) 
(3 HN) 
(3 HN) 
T
|#
;;; SYNOPSIS
(defmethod map-over-sequenzes ((sc slippery-chicken) players function
                               &rest further-args)
;;; ****
  (unless players
    (setf players (players sc)))
  (unless (listp players)
    (setf players (list players)))
  (let* ((section-refs (get-section-refs sc 1 1000)))
    (loop for player in players do
         (loop for section in section-refs 
            for player-section = (get-player-section section player (piece sc))
            do
              (loop for sequenz in (data player-section) do
                   ;; (print (this sequenz))
                   (apply function (cons sequenz further-args))))))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Mon Jun 11 18:36:51 2012 
;;; ****m* slippery-chicken-edit/consolidate-all-notes
;;; DESCRIPTION
;;; A convenience method which just calls the consolidate-notes method from the
;;; rthm-seq-bar class for all the bars specified in the arguments.  
;;; 
;;; ARGUMENTS
;;; - the slippery-chicken object
;;; - the first bar number in which consolidation should take place
;;; - the last bar number in which consolidation should take place
;;;   (inclusive). If NIL then the process will run until the last bar.  
;;; - A list of the IDs of the players to whose parts the consolidation should 
;;;   be applied. Can also be a single symbol.
;;; 
;;; RETURN VALUE
;;; A list of the rthm-seq-bar objects that were modified. See map-over-bars
;;; for more details. 
;;; 
;;; SYNOPSIS
(defmethod consolidate-all-notes ((sc slippery-chicken)
                                  &optional (start-bar 1) end-bar players
                                  ;; MDE Fri Sep 21 09:08:09 2018 -- added
                                  ;; consolidate-notes' optional args  
                                    check-dur beat (auto-beam t))
;;; ****
  (map-over-bars sc start-bar end-bar players
                 #'consolidate-notes check-dur beat auto-beam))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Mon Jun 11 18:36:51 2012 
;;; ****m* slippery-chicken-edit/consolidate-all-rests
;;; DESCRIPTION
;;; A convenience method which just calls the consolidate-rests method from the
;;; rthm-seq-bar class for all the bars specified in the arguments.  
;;; 
;;; ARGUMENTS
;;; - the slippery-chicken object
;;; - the first bar number in which consolidation should take place
;;; - the last bar number in which consolidation should take place
;;;   (inclusive).  If NIL then the process will run until the last bar.  
;;; - A list of the IDs of the players to whose parts the consolidation should 
;;;   be applied. Can also be a single symbol.
;;; 
;;; OPTIONAL ARGUMENTS
;;; T or NIL to indicate whether the method should print a warning to the Lisp
;;; listener if it is mathematically unable to consolidate the rests. T = print
;;; warning. Default = NIL.
;;; 
;;; RETURN VALUE
;;; - A list of the rthm-seq-bar objects that were modified.  See map-over-bars
;;; for more details. 
;;; 
;;; SYNOPSIS
(defmethod consolidate-all-rests ((sc slippery-chicken)
                                  &optional (start-bar 1) end-bar players warn
                                    beat)
;;; ****
  (map-over-bars sc start-bar end-bar players #'consolidate-rests-max
                 :warn warn :beat beat))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Fri Jun 15 11:22:45 BST 2012: Added robodoc entry
;;; MDE Fri Jun 15 13:38:02 2012 -- move over from piece class
;;; SAR Mon Jun 18 12:02:18 BST 2012: Expand robodoc entry

;;; ****m* slippery-chicken/copy-bars
;;; DESCRIPTION
;;; Copy the rhythmic contents (rthm-seq-bar objects) from the specified bars
;;; of one specified player's part to another.  NB No check is performed to
;;; ensure that the copied notes are within the new instrument's range. 
;;;
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; - A 1-based integer or assoc-list reference (section seq-num bar-num) that
;;;   is the number of the first bar in the source player's part whose rhythmic
;;;   contents are to be copied.
;;; - A 1-based integer or assoc-list reference (section seq-num bar-num) that
;;;   is the number of the first bar in the target player's part to which the
;;;   rhythmic contents are to be copied.
;;; - The ID of the source player's part.
;;; - The ID of the target player's part.
;;; - NIL or an integer that is the number of bars to copy, including the
;;;   start-bar. When NIL, all bars in the piece starting from <to-start-bar>
;;;   will be copied.
;;;
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to indicate whether to print feedback to the listener about the
;;;   copying process. T = print. Default = NIL.
;;; 
;;; RETURN VALUE
;;; T
;;; 
;;; EXAMPLE
#|
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((hn (french-horn :midi-channel 1))
                     (vc (cello :midi-channel 2))))
        :set-palette '((1 ((f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5))))
        :set-map '((1 (1 1 1 1 1))
                   (2 (1 1 1 1 1))
                   (3 (1 1 1 1 1)))
        :rthm-seq-palette '((1 ((((4 4) h q e s s))
                                :pitch-seq-palette ((1 2 3 4 5))))
                            (2 ((((4 4) h h))
                                :pitch-seq-palette ((1 2)))))
        :rthm-seq-map '((1 ((hn (1 1 1 1 1))
                            (vc (1 1 1 1 1))))
                        (2 ((hn (2 2 2 2 2))
                            (vc (2 2 2 2 2))))
                        (3 ((hn (1 1 1 1 1))
                            (vc (1 1 1 1 1))))))))
  (copy-bars mini 7 2 'vc 'hn 2 t))

=> T

|#
;;; SYNOPSIS
(defmethod copy-bars ((sc slippery-chicken) from-start-bar to-start-bar 
                      from-player to-player num-bars 
                      &optional (print-bar-nums nil))
;;; **** 
  (flet ((no-bar (bar-num player)
           (error "slippery-chicken-edit::copy-bars: Can't get bar number ~a ~
                   for ~a." bar-num player)))
    (let* ((from-bar (get-bar sc from-start-bar from-player))
           (to-bar (get-bar sc to-start-bar to-player))
           from-plays-transp
           to-plays-transp
           to-transp)
      (unless from-bar
        (no-bar from-start-bar from-player))
      (unless to-bar
        (no-bar to-start-bar to-player))
      (setf from-plays-transp (plays-transposing-instrument sc from-player nil)
            to-plays-transp (plays-transposing-instrument sc to-player nil)
            from-bar (clone from-bar))
      (when print-bar-nums
        (format t "~%from ~a to ~a" from-plays-transp to-plays-transp))
      (unless num-bars
        (setf num-bars (- (num-bars sc) (bar-num to-bar) -1)))
      (loop for fbnum from (bar-num from-bar)
         for tbnum from (bar-num to-bar)
         with first-time = t
         with player-section
         with sequenz
         repeat num-bars 
         do
       ;; MDE Fri Jun 15 13:59:37 2012 
         (when to-plays-transp
           (setf to-transp (get-transposition-at-bar to-player tbnum sc)))
         (unless first-time
           (setf from-bar (get-bar sc fbnum from-player)
                 to-bar (get-bar sc tbnum to-player)))
         (unless from-bar
           (no-bar fbnum from-player))
         (unless to-bar
           (no-bar fbnum to-player))
         (setf from-bar (clone from-bar))
       ;; MDE Fri Jun 15 14:05:04 2012 -- in case we're copying from a 
       ;; transposing to a non-transposing ins 
         (when (and from-plays-transp (not to-transp))
           (delete-written from-bar))
         (when print-bar-nums
           (format t "~%from ~a to ~a to-transp ~a" from-plays-transp
                   to-plays-transp to-transp))
         ;; MDE Fri Jun 15 13:19:27 2012 -- in case we're copying from a 
         ;; non-transposing to a transposing instrument. 
         (when to-transp
           (set-written from-bar (- to-transp)))
         (setf first-time nil)
         (when print-bar-nums
           (format t "~&Copying bar ~a to bar ~a" fbnum tbnum))
         (unless (eq t (time-sig-equal (get-time-sig from-bar)
                                       (get-time-sig to-bar)))
           (error "piece::copy-bars: Can't replace bars with different time ~
                signatures: ~a ~a to ~a ~a"
                  from-player fbnum to-player tbnum))
         ;; copy data that should remain the same into the bar we're going to 
         ;; replace with                  
         (setf (write-bar-num from-bar) (write-bar-num to-bar)
               (start-time from-bar) (start-time to-bar)
               (bar-line-type from-bar) (bar-line-type to-bar)
               (write-time-sig from-bar) (write-time-sig to-bar)
               (player-section-ref from-bar) (player-section-ref to-bar)
               (nth-seq from-bar) (nth-seq to-bar)
               (nth-bar from-bar) (nth-bar to-bar))
         (setf player-section (get-data (player-section-ref to-bar) (piece sc))
               sequenz (get-nth (nth-seq to-bar) player-section))
         (set-nth-bar (nth-bar to-bar) from-bar sequenz))))
  t)
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Fri Aug 10 17:22:54 2012 -- works with sounding pitches (written
;;; pitches will be updated if appropriate)  

;;; SAR Thu Oct  4 16:48:25 BST 2012: Added robodoc entry

;;; ****m* slippery-chicken-edit/add-pitches-to-chord
;;; DESCRIPTION
;;; Add specified pitches to an existing chord object.
;;; 
;;; ARGUMENTS
;;; - The slippery-chicken object which contains the given chord object.
;;; - The ID of the player whose part is to be affected.
;;; - An integer that is the number of the bar that contains the chord object
;;;   that is to be modified.
;;; - An integer that is the number of the note that is the chord object to be
;;;   modified. 
;;; - The pitches to be added. These can be pitch objects or any data that can
;;;   be passed to make-pitch, or indeed lists of these, as they will be
;;;   flattened.
;;; 
;;; RETURN VALUE
;;; The chord object that has been changed.
;;; 
;;; EXAMPLE
#|
(let* ((ip-clone (clone +slippery-chicken-standard-instrument-palette+)))
  (set-slot 'chord-function 'chord-fun1 'guitar ip-clone)
  (let* ((mini
          (make-slippery-chicken
           '+mini+
           :instrument-palette ip-clone
           :ensemble '(((gtr (guitar :midi-channel 1))))
           :set-palette '((1 ((e3 f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5 d5 e5 f5
                                  g5 a5 b5 c6 d6 e6))))
           :set-map '((1 (1)))
           :rthm-seq-palette 
           '((1 ((((4 4) e e e e e e e e))
                 :pitch-seq-palette ((1 (2) 3 (4) 5 (6) 7 (8))))))
           :rthm-seq-map '((1 ((gtr (1))))))))
    (print (get-pitch-symbols 
            (pitch-or-chord (get-event mini 1 2 'gtr))))
    (add-pitches-to-chord mini 'gtr 1 2 'cs4 'ds4)
    (print (get-pitch-symbols 
            (pitch-or-chord (get-event mini 1 2 'gtr))))))

=>
(E3 G3 B3) 
(E3 G3 B3 CS4 DS4) 

|#
;;; SYNOPSIS
(defmethod add-pitches-to-chord ((sc slippery-chicken) player bar-num note-num 
                                 &rest pitches)
;;; ****
  (multiple-value-bind
        (chord event)
      (sc-get-chord sc bar-num note-num player 'force)
    (add-pitches chord pitches)
    ;; do this just to make sure we set the written chord if present
    (setf (pitch-or-chord event) chord)
    chord))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Fri Aug 10 17:22:54 2012 -- works with sounding pitches (written
;;; pitches will be updated if appropriate)  
;;; 
;;; ****m* slippery-chicken-edit/rm-pitches-from-chord
;;; DESCRIPTION
;;; Remove the specified pitches from an existing chord object.
;;; 
;;; ARGUMENTS
;;; - The slippery-chicken object which contains the given chord object.
;;; - The ID of the player whose part is to be affected.
;;; - An integer that is the number of the bar that contains the chord object
;;;   that is to be modified.
;;; - An integer that is the number of the note that is the chord object to be
;;;   modified. 
;;; - The pitches to be removed. These can be pitch objects or any data that
;;;   can be passed to make-pitch, or indeed lists of these, as they will be
;;;   flattened.
;;; 
;;; RETURN VALUE
;;; The chord object that has been changed.
;;; 
;;; EXAMPLE
#|
(let* ((ip-clone (clone +slippery-chicken-standard-instrument-palette+)))
  (set-slot 'chord-function 'chord-fun2 'guitar ip-clone)
  (let* ((mini
          (make-slippery-chicken
           '+mini+
           :instrument-palette ip-clone
           :ensemble '(((gtr (guitar :midi-channel 1))))
           :set-palette '((1 ((e3 f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5 d5 e5 f5
                                  g5 a5 b5 c6 d6 e6))))
           :set-map '((1 (1)))
           :rthm-seq-palette 
           '((1 ((((4 4) e e e e e e e e))
                 :pitch-seq-palette ((1 (2) 3 (4) 5 (6) 7 (8))))))
           :rthm-seq-map '((1 ((gtr (1))))))))
    (print (get-pitch-symbols 
            (pitch-or-chord (get-event mini 1 2 'gtr))))
    (rm-pitches-from-chord mini 'gtr 1 2 'a3 'd4)
    (print (get-pitch-symbols 
            (pitch-or-chord (get-event mini 1 2 'gtr))))))

=>
(E3 A3 D4 G4) 
(E3 G4)

|#
;;; SYNOPSIS
(defmethod rm-pitches-from-chord ((sc slippery-chicken) player bar-num
                                  note-num &rest pitches)
;;; ****
  (multiple-value-bind
        (chord event)
      (sc-get-chord sc bar-num note-num player)
    (rm-pitches chord pitches)
    ;; do this just to make sure we set the written chord if present
    (setf (pitch-or-chord event) chord)
    chord))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken-edit/rm-repeated-pitches
;;; DATE
;;; April 23rd 2016, Edinburgh
;;; 
;;; DESCRIPTION
;;; Sometimes, even pitch curves without repeated notes result in repeated
;;; notes in the generated piece. Often it's the removal of large fast leaps in
;;; instrumental parts that ends up creating repeated notes. This method will
;;; do its best to replace repeated notes, even in chords, with other notes
;;; from the current set and within the range of the current instrument and any
;;; set-limits. If a chord is required, then the current instrument's chord
;;; function will be used to create the replacement chord.
;;;
;;; NB If you're interested in instrument/players statistics like tessitura
;;; you'll have to call update-slots after you're done calling this and related
;;; methods.
;;; 
;;; ARGUMENTS
;;; - the slippery-chicken object
;;; - the player (symbol)
;;; 
;;; OPTIONAL ARGUMENTS
;;; - the start bar (integer). Default = 1.
;;; - the end bar (integer). Default = NIL which means we'll process up to the
;;;   end of the piece.
;;; 
;;; RETURN VALUE
;;; The slippery-chicken object with all possible repeated notes removed.
;;; 
;;; SYNOPSIS
(defmethod rm-repeated-pitches ((sc slippery-chicken) player
                                &optional (start-bar 1) end-bar)
;;; ****
  (next-event sc player t start-bar)
  ;; our set-limits were stretched to fit the number of sequences in the piece,
  ;; but we'll need to range over the number of bars, so fix this here 
  (handle-set-limits sc t)
  (handle-transposition-curve sc t) ; MDE Thu Sep  6 14:25:02 2018 
  (let ((last-event (next-event sc player))
        last-event-chord event-chord find-new poc new-poc)
    (flet ((warn-failed (e)
             (warn "slippery-chicken-edit::rm-repeated-pitches: ~%~
                    Skipping: Failed to find new pitch(es) at bar ~a."
                   (bar-num e))))
      (loop for event = (next-event sc player t nil end-bar) while event do
         ;; make even single pitches chords so that we can get common notes
           (setq last-event-chord (make-chord
                                   (clone (pitch-or-chord last-event)))
                 poc (pitch-or-chord event)
                 new-poc (clone poc)
                 event-chord (make-chord poc)
                 find-new nil)
           (unless (zerop (common-notes event-chord last-event-chord))
             ;; if we've got a chord...only ever change the current event, not
             ;; the last
             ;; (print (bar-num event))
             (if (is-chord event)
                 (progn
                   (rm-pitches new-poc
                               (if (is-chord last-event)
                                   ;; we've got two chords so remove the common
                                   ;; pitches from the current
                                   (data last-event-chord)
                                   ;; last is a single pitch: remove the last
                                   ;; pitch from our current chord
                                   (pitch-or-chord last-event)))
                   ;; the above removals resulted in something less than 2
                   ;; notes so we'll need to find a new chord below
                   (if (> (sclist-length new-poc) 1)
                       (setf (pitch-or-chord event) new-poc)
                       (setq find-new 'chord)))
                 ;; picked up and supplied below
                 (setq find-new 'pitch)))
           (when find-new
             (let* ((set (get-data (set-ref event) (set-palette sc)))
                    (instrument (get-instrument-for-player-at-bar
                                 (player event) (bar-num event) sc))
                    (limits (get-set-limits sc player (bar-num event)))
                    (set-transp (interpolate (bar-num event)
                                             (transposition-curve sc)))
                    index)
               (if (sc-set-p set)
                   (setq set (clone set))
                   (error "slippery-chicken-edit::rm-repeated-pitches: ~%~
                           Couldn't get set with reference ~a" (set-ref event)))
               ;; MDE Thu Sep  6 14:19:23 2018 -- don't forget the new
               ;; transposition-curve!
               (unless (zerop set-transp)
                 (transpose set set-transp))
               ;; get the pitches the instrument can play and remove the last
               ;; event's pitch(es)  
               (limit-for-instrument set instrument
                                     :lower (first limits)
                                     :upper (second limits)
                                     :do-related-sets t)
               (rm-pitches set (data last-event-chord))
               (if (zerop (sclist-length set)) ; can't replace repeated
                   (warn "slippery-chicken-edit::rm-repeated-pitches: ~%~
                          Skipping (bar ~a): Couldn't get alternative pitches."
                         (bar-num event))
                   ;; use the nearest pitch to the existing repeating pitch
                   (setq index (nth-value 1 (find-nearest-pitch
                                             (data set)
                                             (if (is-chord event)
                                                 (first (data poc))
                                                 poc)))))
               (if index
                   (let* ((player-obj (get-player (ensemble sc) player)))
                     (setq new-poc
                           (if (eq find-new 'chord)
                               (funcall
                                ;; use the instrument's own chord function to
                                ;; select new pitches
                                (symbol-function (chord-function instrument))
                                1 index (data set) nil nil nil)
                               ;; single pitch: just the nearest non-repeating
                               (nth index (data set))))
                     (unless (or (and (chord-p new-poc)
                                      (> (sclist-length new-poc) 0))
                                 (pitch-p new-poc))
                       (warn-failed event)
                       (setq new-poc nil))
                     (when new-poc
                       (setf (pitch-or-chord event) new-poc)
                       ;; don't forget to set the correct midi channels
                       (set-midi-channel (pitch-or-chord event)
                                         (midi-channel player-obj)
                                         (microtones-midi-channel player-obj))))
                   ;; index = NIL !!!
                   (warn "slippery-chicken-edit::rm-repeated-pitches: ~%~
                          Skipping (bar ~a): Couldn't find nearest pitch ~
                          (~a) in ~%~a"
                         (bar-num event)
                         (print-simple poc nil)
                         (print-simple-pitch-list (data set) nil)))))
           (setq last-event event))
      ;; just to make sure the tied-to notes are the same as the attacked
      (check-ties sc)
      sc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken-edit/octavise-repeated-notes
;;; DATE
;;; July 13th 2020, Heidhausen (though taken from earlier projects)
;;; 
;;; DESCRIPTION
;;; Transpose fast repeated notes by given intervals, typically, as in piano
;;; music, by an octave. Note that if there are several fast repeated notes then
;;; there will be multiple transpositions e.g. (c4 c4 c4 c4 c4) would become by
;;; default (c4 c5 c4 c5 c4).
;;; 
;;; ARGUMENTS
;;; - the slippery-chicken object
;;; - the player (symbol) or players (list) to process
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :start-bar. The bar to start processing at (integer). Default = 1.
;;; - :end-bar. The bar to stop processing at (inclusive). Default = NIL = last
;;;   bar of the piece.
;;; - :intervals. A list of semitone intervals. Default = '(12) = only transpose
;;;   up one octave. A list of any length may be used for variation; it will be
;;;   used cyclically.
;;; - :threshold. The maximum time difference in seconds between events that
;;;   will result in the 'octavisation'. Default = the slippery-chicken
;;;   object's fast-leap-threshold (0.125 seconds by default).
;;; - :verbose. Print bar numbers where processing takes place. Default = NIL.
;;; - :whole-chords. T or NIL to transpose whole chords or just the repeated
;;;   notes in chords (could result in unplayable chords if NIL). Default = T =
;;;   transpose whole chord.
;;; - :check-ties. Whether to call the check-ties method, which as a side-effect
;;;   makes sure tied notes are of the same pitch. Default = T.
;;; 
;;; RETURN VALUE
;;; the (modified) slippery-chicken object.
;;; 
;;; SYNOPSIS
(defmethod octavise-repeated-notes ((sc slippery-chicken) players
                                    &key (start-bar 1) end-bar verbose
                                      (intervals '(12)) (whole-chords t)
                                      (check-ties t)
                                      (threshold (fast-leap-threshold sc)))
;;; ****
  (loop for player in (force-list players) do
       (let ((transps (make-cscl intervals))
             last time-diff common-pitches transp)
         (unless end-bar (setq end-bar (num-bars sc)))
         (next-event sc player t start-bar)
         (loop for event = (next-event sc player t nil end-bar) while event do
              (when last
                (setq time-diff (- (start-time event) (start-time last)))
                ;; fast repeated notes cause jump of octave +/-
                (when (and (< time-diff threshold)
                           ;; if there are notes in common, then the 2nd value
                           ;; returned will be the list of pitch objects in
                           ;; common
                           (setq common-pitches
                                 (nth-value 1 (common-notes event last))))
                  (setq transp (get-next transps))
                  (when verbose (format t "~&octavise-repeated-notes: bar ~a, ~
                                           last: ~a, this: ~a, diff: ~,3f ~
                                           transp: ~,3f"
                                        (bar-num event)
                                        (get-pitch-symbol last)
                                        (get-pitch-symbol event) 
                                        time-diff transp))
                  ;; (transpose event (get-next transps) :destructively t)))
                  (if (or whole-chords (is-single-pitch event))
                      (transpose event transp :destructively t)
                      ;; MDE Tue Aug 4 11:56:17 2020, Heidhausen -- don't
                      ;; transpose the whole chord, rather, just the repeated
                      ;; notes
                      (setf (pitch-or-chord event) ; has to be a chord!
                            (loop for p in (data (pitch-or-chord event)) collect
                                 (if (member p common-pitches :test #'pitch=)
                                     (transpose p transp)
                                     p))))))
              (setq last event))))
  (when check-ties (check-ties sc t nil)) ; no error or warnings
  sc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken-edit/add-auxiliary-notes
;;; DATE
;;; May 26th 2016, Edinburgh
;;; 
;;; DESCRIPTION
;;; Analyses phrase by phrase to find the most frequent X notes then changes
;;; some of them (according to an activity-levels object) to auxiliary notes
;;; (up a semitone by default). Chords are ignored.
;;; 
;;; ARGUMENTS
;;; - the slippery-chicken object
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments
;;; - :players. A single player (symbol) or list of players to process. If NIL
;;;    then all players will be processed.
;;; - :start-bar. An integer indicating the bar in which the processes should
;;;    start. Default = NIL which means we'll start at the beginning.
;;; - :end-bar. An integer indicating the bar in which the processes should
;;;    stop. Default = NIL which means we'll process through to the end
;;; - :verbose. T or NIL to print information about the pitch changes during
;;;    the process. Default = NIL.
;;; - :ignore. A symbol or pitch-object (or list thereof) to represent pitches
;;;    which should not be processed. Default = NIL = process (potentially) all
;;;    pitches. 
;;; - :activity-level. Whether to change the note is decided by an
;;;   activity-levels object. This argument determines the 'level' argument used
;;;   in that class's active method. Default = 5 = 50% of the notes will be
;;;   changed. NB The activity-levels object will be reinitialised for each
;;;   phrase so the 50/50 spread might not be obvious.
;;; - :num-notes. The number of notes to process per phrase. We calculate the
;;;   most used pitches in every phrase then use this number of pitches to add
;;;   auxiliaries to. Default = 3.
;;; - :intervals. The intervals that the original pitches will be transposed by
;;;   to create the auxiliary notes. This can be a single value in semitones or
;;;   a list of values. The list does not have to have the same length as
;;;   :num-notes as it will be cycled through when necessary. Default = 1.
;;; 
;;; RETURN VALUE
;;; T
;;; 
;;; SYNOPSIS
(defmethod add-auxiliary-notes ((sc slippery-chicken)
                                &key players start-bar end-bar
                                  verbose ignore (activity-level 5)
                                  (num-notes 3) (intervals 1))
;;; ****
  (let ((phrases (get-phrases sc players :start-bar start-bar
                              :end-bar end-bar))
        (ints (make-cscl (force-list intervals))))
    (loop for player in phrases do
         (loop for phrase in player do
              (add-auxiliary-notes-aux phrase :num-notes num-notes
                                       :destructively t :verbose verbose
                                       :activity-level activity-level
                                       :ignore ignore
                                       :interval (get-next ints)))))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken-edit/rehearsal-letters-at-sections
;;; DATE
;;; August 19th 2016, Edinburgh
;;; 
;;; DESCRIPTION
;;; Place a rehearsal letter at the beginning of every section (and subsection)
;;; in the piece. 
;;; 
;;; ARGUMENTS
;;; - a slippery chicken object
;;; 
;;; RETURN VALUE
;;; A list of the bar numbers where rehearsal letters will appear.
;;; 
;;; SYNOPSIS
(defmethod rehearsal-letters-at-sections ((sc slippery-chicken))
;;; ****  
  (let* ((refs (get-section-refs sc 1 9999))
         ;;                         don't need letter at bar 1
         (bar-nums (loop for ref in (rest refs) collect
                        (start-bar (get-section sc ref)))))
    (setf (rehearsal-letters sc) bar-nums)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken-edit/add-half-beat-rest
;;; DATE
;;; August 26th 2016, Edinburgh
;;; 
;;; DESCRIPTION
;;; Calls the method of the same name on each rthm-seq-bar object at the given
;;; bar number. Adds a half beat rest at the end of the bar. This is a
;;; destructive method. It will change the time signature, so a 2/4 bar
;;; becomes a 5/8 bar, with an 1/8th rest at the end. This is done for all the
;;; players in the ensemble (so that scores can be correctly written)
;;; 
;;; ARGUMENTS
;;; - a slippery-chicken object
;;; 
;;; OPTIONAL ARGUMENTS
;;; - the bar number to modify. Default = 1.
;;; 
;;; RETURN VALUE
;;; T
;;; 
;;; SYNOPSIS
(defmethod add-half-beat-rest ((sc slippery-chicken) &optional (bar-num 1))
;;; ****
  (loop for bar in (get-bar sc bar-num) ; gets all players' bars at bar-num
     do (add-half-beat-rest bar))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken-edit/set-limits-by-section
;;; DATE
;;; February 20th 2018, Heidhausen
;;; 
;;; DESCRIPTION
;;; For each section/subsection of a piece, set the highest or lowest pitch,
;;; i.e. create the curve for one player in the set-limits-high or
;;; set-limits-low slot.
;;; 
;;; N.B.1 As well as returning the generated list, the respective slot value
;;; (<which>) is also changed.
;;;
;;; N.B.2 As the internal structure of the slippery-chicken object is modified
;;; here, and this has implications for pitch-selection, i.e. constraints for an
;;; already-existing slippery-chicken object (made, probably, by
;;; make-slippery-chicken), the piece will have to be regenerated by explicitly
;;; calling (sc-init) after calling this method.
;;; 
;;; ARGUMENTS
;;; - the slippery-chicken object
;;; - the pitches: a list of pitch symbols or MIDI note numbers. Note that these
;;;   will be used cyclically if there aren't enough pitches for the number of
;;;   sections/subsections in the piece.
;;; - 'set-limits-high or 'set-limits-low to set the desired curve
;;; 
;;; OPTIONAL ARGUMENTS
;;; - which player (symbol) to set the curve for. Default = 'all, which means
;;;   the curve will apply to all players.
;;; 
;;; RETURN VALUE
;;; The curve created (list)
;;;
;;; EXAMPLE
#|
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((vc (cello :midi-channel 1))))
        :set-palette '((1 ((f3 g3 a3 b3 c4))))
        :set-map '((1 (1 1 1 1 1))
                   (2 (1 1 1 1 1))
                   (3 (1 1 1 1 1)))
        :rthm-seq-palette '((1 ((((4 4) h q e s s))
                                :pitch-seq-palette ((1 2 3 4 5)))))
        :rthm-seq-map '((1 ((vc (1 1 1 1 1))))
                        (2 ((vc (1 1 1 1 1))))
                        (3 ((vc (1 1 1 1 1))))))))
  (flet ((print-em ()
                   ;;                first bars of sections 1, 2, 3
                   (loop for bar in '(1 6 11) do
                         (print (get-pitch-symbols (get-bar mini bar 'vc))))))
    (print-em)
    ;; replace existing curve. NB The y values are degrees which if default is
    ;; quarter-tone scale might be the double of what is perhaps  expected
    ;; (i.e. (note-to-degree 'c4) -> 120)
    (print (set-limits-by-section mini '(g3 a3 c4) 'set-limits-high 'vc))
    (sc-init mini)
    ;; note the new (repeating) pitches
    (print-em)))

---->>

******* section (1)
Getting notes for VC
******* section (2)
Getting notes for VC
******* section (3)
Getting notes for VC
WARNING:
   slippery-chicken::tempo-curve-to-map: No tempo-map or tempo-curve given. 
Using default of crotchet/quarter = 60.
Shortening short, fast leaps...
Shortened 0 large fast leaps
(F3 G3 A3 B3 C4) 
(F3 G3 A3 B3 C4) 
(F3 G3 A3 B3 C4) 
(1 110 5 110 6 114 10 114 11 120 15 120) 
******* section (1)
Getting notes for VC
******* section (2)
Getting notes for VC
******* section (3)
Getting notes for VC
Shortening short, fast leaps...
Shortened 0 large fast leaps
(F3 F3 G3 G3 G3) 
(F3 G3 G3 A3 A3) 
(F3 G3 A3 B3 C4) 
NIL

|#      
;;; SYNOPSIS
(defmethod set-limits-by-section ((sc slippery-chicken) pitches which
                                  &optional (player 'all))
;;; ****
  (unless (or (eq player 'all) (member player (players sc)))
    (error "slippery-chicken::set-limits-by-section: player argument (~a) ~
            ~%should be 'all or a player from the ensemble: ~a"
           player (players sc)))
  (let* ((top (make-cscl pitches))
         (seq-num 1)
         (slot (slot-value sc which))
         (curve (when slot (get-data player slot nil)))
         (result
          (doctor-env
           (loop for section in (get-all-section-refs sc)
              for p = (get-next top)
              appending (list seq-num p
                              (1- (incf seq-num (num-seqs sc section)))
                              p)))))
    (if curve
        (setf (data curve) result)
        (if slot
            (add (list player result) slot)
            (setf (slot-value sc which) (list (list player result)))))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken-edit/pause-last
;;; AUTHOR
;;; Daniel Ross (mr.danielross@gmail.com)
;;;
;;; DATE
;;; 23 September 2016, London
;;; 
;;; DESCRIPTION
;;; Add a pause mark to the last note of every (any) player's part. It defaults
;;; to the last bar but this can be changed with the :bar-num keyword arg. You
;;; also have the option to change the bar line type in the same bar using the
;;; :bar-line keyword arg. See (change-bar-line-type) for details.
;;; 
;;; ARGUMENTS
;;; - the slippery chicken object
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :players. The player (symbol) or list of players that will be
;;;   effected. Default = NIL = all players.
;;; - :bar-num. The bar number to add the pause mark. Default = last bar of
;;;   piece. 
;;; - :bar-line. The bar line type of the bar we'll add a pause to (symbol). Can
;;;   be one of 'normal 'double-bar 'final-double 'begin-repeat
;;;   'begin-end-repeat or 'end-repeat. Default = NIL = no change.
;;; - :pause. The type of pause to add (symbol). Can be one of 'pause
;;;   'long-pause or 'short-pause. Default = 'pause
;;;
;;; RETURN VALUE
;;; - the number of pause marks added 
;;; 
;;; EXAMPLE
#|
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((pno (piano :midi-channel 1))))
        :tempo-map '((1 (q 60)))
        :set-palette '((1 ((c4 d4 f4 g4 a4 c5 d5 f5 g5 a5 c6))))
        :set-map '((1 (1 1 1 1 1 1 1 1)))
        :rthm-seq-palette '((1 ((((2 4) q q))
                                :pitch-seq-palette ((1 (2))))))
        :rthm-seq-map '((1 ((pno (1 1 1 1 1 1 1 1))))))))
  (pause-last mini))

=> 1

|#
;;; SYNOPSIS
(defmethod pause-last ((sc slippery-chicken) 
                       &key
                         ;; MDE Tue Feb 26 09:07:43 2019 -- type of pause added
                         ;; can vary
                         (pause 'pause)
                         ;; MDE Sat Feb 23 12:52:15 2019 -- making players a key
                         ;; arg instead of required  
                         players
                         ;; we might want the pause mark somehere else
                         (bar-num (num-bars sc))
                         bar-line)
;;; ****
  "Add a pause mark to the last note of every part"
  (unless players (setf players (players sc)))
  (when (typep players 'atom) (setf players (list players)))
  (loop with count = 0 for player in players do
       (let ((bar (get-bar sc bar-num player)))
         (unless (is-rest-bar bar)
           (let ((e (get-last-event bar)))
             (when e
               (incf count)
               (add-mark e pause))
             (when bar-line
               (change-bar-line-type sc bar-num bar-line)))))
     finally (return count)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken-edit/round-to-nearest
;;; DATE
;;; October 18th 2018, Heidhausen
;;; 
;;; DESCRIPTION
;;; For the given bars and players, round the events' pitch data to the nearest
;;; in the current or given scale. For example, a slippery-chicken object could
;;; be generated in the :quarter-tone scale, then the scale changed to
;;; :chromatic (or the keyword argument :scale 'chromatic-scale given), this
;;; method then called, and all pitches would be rounded to the nearest
;;; chromatic pitch. See the pitch class method for more details.
;;; 
;;; ARGUMENTS
;;; the slippery-chicken argument
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :start-bar. A number that is the first bar which should be
;;;   processed. Default = NIL in which case 1 will be used.
;;; - :end-bar. A number that is the last bar which should be processed. Default
;;;   = NIL in which case all bars will be processed.
;;; - :players. A list of the IDs of the players to whose parts the function
;;;   should be applied. Can also be a single symbol. If NIL then all players
;;;   will be processed.  
;;; - :scale. The scale to use when rounding. (Common Music tuning object or
;;;   symbol). If a symbol, then 'chromatic-scale, 'twelfth-tone, or
;;;   'quarter-tone only at present. Default is the current scale as set by
;;;   (in-scale :...). 
;;; 
;;; RETURN VALUE
;;; the modified slippery-chicken object
;;; 
;;; SYNOPSIS
(defmethod round-to-nearest ((sc slippery-chicken)
                             &key start-bar end-bar players (scale cm::*scale*))
;;; ****
  (map-over-bars sc start-bar end-bar players
                 #'(lambda (bar)
                     (loop for e in (rhythms bar) do
                          (round-to-nearest e :scale scale))))
  sc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken-edit/set-midi-channels
;;; DATE
;;; October 25th 2018, Heidhausen
;;; 
;;; DESCRIPTION
;;; Change the midi-channels of events in the slippery-chicken object. All
;;; events' pitch objects' midi-channel slots will be destructively overwritten
;;; when using this method.
;;;
;;; The second argument is a list of lists. Each sublist has the player (not
;;; instrument) ID symbol as its first element, followed by one or two channel
;;; numbers. If only one channel is given (i.e. it's a two-element list
;;; e.g. (vln 1)) then it will be used for both chromatic and microtonal pitches
;;; (i.e. all the players' pitches will be written to the one given midi
;;; channel). Otherwise if a second channel is given (e.g. (vln 1 2)) then
;;; microtonal pitches will be written on the second channel.
;;; 
;;; ARGUMENTS
;;; - the slippery-chicken object.
;;; - a list of player and channel info (see above and example below).
;;; 
;;; OPTIONAL ARGUMENTS
;;; - the start-bar in which events should begin to be processed. Default = 1
;;; - the end bar (inclusive) in which the last events should be
;;;   processed. Default = NIL = the last bar of the slippery-chicken object. 
;;; 
;;; RETURN VALUE
;;; T
;;; 
;;; EXAMPLE
#|
(set-midi-channels +sc-obj+ '((solo 1) (computer-a 2) (computer-b 3)
                              ;; this one uses separate midi channels for
                              ;; chromatic and microtonal pitches
                              (computer-c 4 5)))
|#
;;; SYNOPSIS
(defmethod set-midi-channels ((sc slippery-chicken) channel-info
                              &optional (start-bar 1) end-bar)
;;; ****
  (loop for ci in channel-info
     for player = (first ci)
     for mc = (second ci)
     for mmc = (third ci)
     do
       (next-event sc player nil start-bar)
       (loop for e = (next-event sc player nil nil end-bar) while e do
            (set-midi-channel e mc mmc)))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken-edit/reset-midi-channels
;;; DATE
;;; June 9th 2020
;;; 
;;; DESCRIPTION
;;; Reset players' event objects to use the midi-channels given in the ensemble
;;; rather than those inherited created elsewhere.
;;; 
;;; ARGUMENTS
;;; - the slippery-chicken object to process
;;; 
;;; OPTIONAL ARGUMENTS
;;; - a symbol or list of symbols representing the players we want to process
;;;   (i.e. the same player symbols found in the ensemble slot, not
;;;   instruments). Default = NIL = all players
;;; - the start-bar in which events should begin to be processed. Default = 1
;;; - the end bar (inclusive) in which the last events should be
;;;   processed. Default = NIL = the last bar of the slippery-chicken object. 
;;; 
;;; RETURN VALUE
;;; T
;;; 
;;; SYNOPSIS
;;; ****
(defmethod reset-midi-channels ((sc slippery-chicken) 
                                &optional players (start-bar 1) end-bar)
  (set-midi-channels  sc
                      (loop for player in (if players (force-list players)
                                              (players sc))
                         for player-obj = (get-player sc player)
                         collect
                           (list player (midi-channel player-obj)
                                 (microtones-midi-channel player-obj)))
                      start-bar end-bar))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Tue Oct 30 08:12:47 2018 -- which is 1-based
(defmethod staff-groupings-inc ((sc slippery-chicken) &optional which)
  (let ((len (length (staff-groupings sc))))
    (if which
        (when (> which len)
          (error "slippery-chicken::staff-groupings-inc: can't increment at ~
                  position ~a in staff groupings ~a"
                 which (staff-groupings sc)))
        (setq which len)) ; 1-based!
    (incf (nth (1- which) (staff-groupings sc))))
  (staff-groupings sc))
;;; ****

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken-edit/add-player
;;; DATE
;;; October 30th 2018, Heidhausen
;;; 
;;; DESCRIPTION
;;; Add a player to the ensemble and piece slots/objects of the given
;;; slippery-chicken object. This clones the section/sub-section structure of
;;; the slippery-chicken object, creating all the necessary data structures for
;;; empty bars in the right meter, tempo etc.
;;;
;;; NB As this method may be called several times successively, it's the
;;; caller's duty to call (update-slots sc) in order to have timing and other
;;; data updated correctly.
;;; 
;;; ARGUMENTS
;;; - the slippery-chicken object
;;; - a player object or symbol ID for the new player
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :instrument. a symbol ID for an existing instrument in the
;;;   instrument-palette (the next argument). This is actually required unless
;;;   the default of 'computer is acceptable or a player object is passed as
;;;   second argument.
;;; - :instrument-palette. an instrument-palette object in which the instrument
;;;   exists. Default is the standard palette.
;;; - :midi-channel. the midi-channel for the new player
;;; - microtones-midi-channel. the microtones-midi-channel for the new player.
;;;   Default = -1 = same as :midi-channel.
;;; 
;;; RETURN VALUE
;;; the new player object from the ensemble slot of the slippery-chicken object
;;; 
;;; SYNOPSIS
(defmethod add-player ((sc slippery-chicken) player
                       &key (instrument 'computer)
                         (instrument-palette
                          +slippery-chicken-standard-instrument-palette+)
                         ;; MDE Tue Jul 14 19:08:15 2020, Heidhausen
                         (midi-channel 1)
                         (microtones-midi-channel -1))
;;; ****
  (let ((player-id (if (player-p player) (id player) player)))
    (add-player (ensemble sc) player :instrument instrument
                :instrument-palette instrument-palette
                :midi-channel midi-channel
                :microtones-midi-channel microtones-midi-channel)
    ;; MDE Wed Aug  5 14:34:43 2020, Heidhausen -- ins-hier!
    (setf (instruments-hierarchy sc)
          (econs (instruments-hierarchy sc) player-id))
    ;; this calls the rthm-seq-map method
    (add-player-to-players (piece sc) player-id)
    ;; we pass all players so that new ones can clone existing ones (the
    ;; existing ones won't be replaced)
    (add-rest-player-sections-aux (piece sc) (players sc))
    ;; MDE Wed Dec 30 17:47:03 2020, Heidhausen -- if we don't do this then we
    ;; might be missing bars rests at the end of the new part.
    (setf (bar-line-type (get-last-bar-for-player sc player-id)) 2)
    (staff-groupings-inc sc)
    ;; todo: check the new player's events have the right player slot
    (get-player (ensemble sc) player-id)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Sat Nov  3 10:14:39 2018 
(defmethod add-ensemble-players ((sc slippery-chicken) (ens ensemble))
  (loop for player in (data ens) do
     ;; MDE Thu Dec 27 15:44:12 2018 -- don't add an existing player: we might
     ;; want to call the orchestrate method several times for different parts
     ;; of the piece and with different ensembles, so need to be able to
     ;; reference players more than once
       (unless (get-player sc (id player))
         (add-player sc player)))
  (players sc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; called by orchestrate. combo is a list as described below at 
(defmethod copy-to-combo ((sc slippery-chicken) event combo)
  (loop with e for p in combo do
     ;; if an instrument can't play a note the second in
     ;; the list will be nil 
       (when (second p)
         (setq e (get-event sc (bar-num event)
                            (1+ (bar-pos event)) (first p)))
         ;;                       pitch obj  ins obj
         (set-pitch-or-chord e (second p) (third p))
         (when (is-tied-from event)
           (setf (is-tied-from e) t))
         (when (is-tied-to event)
           (setf (is-tied-to e) t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken-edit/orchestrate
;;; DATE
;;; November 2nd 2018, Heidhausen
;;; 
;;; DESCRIPTION
;;; 'Orchestrate' the chords and single pitches in an existing slippery-chicken
;;; object. This involves mapping the existing players' pitch events
;;; onto a group of new players (a 'combo'). The procedure is fairly
;;; complex and by no means perfect (in fact it may exit with an error,
;;; especially if your requirements are :strict (see below)), but it works
;;; like this:
;;; 
;;; First, we assume that your existing music is more vertical/harmonic than
;;; horizontal/linear/melodic, particularly because the mapping process treats
;;; each chord separately and potentially maps each chord onto different
;;; players---the choice of whether to change ensemble is decided by an
;;; activity-levels object by default (see :combo-change-fun below) but also by
;;; the number of notes in any given chord and whether it's possible to play any
;;; subsequent chord on the same instruments. The method also assumes that each
;;; 'player' in the existing slippery-chicken object has a sequence of one or
;;; more chords which can be separated into 'phrases' using the get-phrases
;;; method; that the new ensemble uses player IDs that don't exist in the
;;; original ensemble (though any player can use the same instruments, of
;;; course); and that using the player IDs from the new ensemble we provide
;;; lists of sublists of possible 'combos' that should be used to play the
;;; existing chords. These combos should range from 2 players to however many
;;; we'll need to play the existing chords. We pass these combos to the method
;;; and/or allow them to be created by the 'lotsa-combos' ensemble method (see
;;; :combos below)
;;;
;;; Whether a chord is playable by a given combo is determined by the range of
;;; the chord and the instruments of any given combo, along with those
;;; instruments' abilities to play chords and/or microtones (if the chord is
;;; microtonal). When trying to determine whether a given combo can play a
;;; chord, we permutate the possible instrument orderings to see if the pitches
;;; are in range. If :artificial-harmonics is T we also try for such on notes
;;; normally considered too high for any given string instrument. If we can't
;;; find a combo for any given chord we'll keep track of the best match and if
;;; our :relax argument allows, use that instead of backing out completely.
;;;
;;; One of the complexities we face, after determining that a chord is playable
;;; by a particular combo, is whether the players are free (i.e. not already
;;; playing) at the time the chord needs to be played. We take ties into account
;;; of course, but one limitiation, presently, is that in order be deemed 'free'
;;; each instrument of the combo must have a complete bar free for the start and
;;; end of any given chord.
;;;
;;; As we select the current instrument using the instrument-change-map slot of
;;; the slippery-chicken object (which may have been changed after the original
;;; data was generated) this method works with players who double on two or more
;;; instruments.
;;;
;;; NB If all goes well, each note in a chord will be played by one
;;; instrument. Of course orchestration often involves 'doubling' where each
;;; note is played by more than one instrument. To achieve this, call
;;; orchestrate multiple times, perhaps increasing the :relax value on
;;; subsequent calls so that the method doesn't exit when it can't find a 2nd,
;;; 3rd, or 4th (...) combo to play a given chord.
;;; 
;;; ARGUMENTS
;;; - the slippery-chicken object whose existing players and event data will be
;;;   used to orchestrate onto new players
;;; - a new ensemble, the players/instruments of which will be used for the new
;;;   orchestration. (This shouldn't be a nested ensemble but that's a rarity.)
;;;   This can either be an ensemble object or a list structure that can be made
;;;   into one (as used for most calls to make-slippery-chicken).
;;; - a list of player IDs indicating the original music to be re-orchestrated.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :combos. A list of lists indicating the player groupings to be used to
;;;   orchestrate the events. Default = NIL whereupon the <new-ensemble> will be
;;;   used to generate groupings via the 'lotsa-combos' ensemble class method.
;;; - :add-more-combos. T or NIL to indicate whether the :combos slot should be
;;;   expanded via the 'lotsa-combos' method. If T then whatever is passed to
;;;   :combos will nevertheless be retained after the expansion. Default = NIL.
;;; - :sub-combos. T or NIL to indicate whether the :combos slot should be
;;;   expanded via the sub-groups function. This is quite different to
;;;   :add-more-combos as that approach uses permutations of all the instruments
;;;   in the ensemble whereas this approach uses permutations of sub-sets of the
;;;   given :combos (only) instead.
;;; - :start-bar. The bar number to start orchestrating. Default = NIL which
;;;   will in turn default to 1
;;; - :end-bar. The bar number to stop orchestrating (inclusive). Default = NIL
;;;   which will in turn default to the last bar of the slippery-chicken object.
;;; - :auto-beam. T or NIL to indicate whether to call auto-beam in the new
;;;   parts. Default = T.
;;; - :chords. Whether individual instruments capable of playing chords should
;;;   attempt to do so. This may determine whether a given combo is determined
;;;   to be capable of playing one of the original slippery-chicken object's
;;;   chords, and also whether under some circumstances some notes will be
;;;   doubled across different instruments. Default = T.
;;; - :combo-change-fun. A function which should return T or NIL to indicate
;;;   whether we should change combo on any given event. This should take two
;;;   arguments (the combo as a list of player IDs and an assoc-list of all
;;;   possible combos (as created by the organise-combos method using the
;;;   <combos> argument given here) plus two further optional arguments which
;;;   are free to define. The function should also provide "reset" functionality
;;;   or do nothing if one of the two required arguments is NIL. Default =
;;;   combo-change? (defined in event.lsp).
;;; - :verbose. T or NIL to turn on the printing of orchestration decisions as
;;;   the algorithm is run.
;;; - :artificial-harmonics. T or NIL to allow the selection of string
;;;   artificial harmonics in order to reach high notes outwith normal ranges.
;;; - :sticky-stats. T or NIL to indicate whether the statistics collected and
;;;   returned by the method should be reset or retained between calls. Default
;;;   = NIL = reset.
;;; - :relax. An integer between 0 and 3 inclusive to indicate how strict the
;;;   algorithm should when allocating chord pitches to instruments. The values
;;;   have the following meanings with degrees of relaxedness ascending:
;;;   0: each pitch in the chord has to be playable by at least one instrument
;;;   in the combo; no instrument can sit this chord out but instruments can
;;;   play chords that include notes found in other instruments.
;;;   1. instruments may sit out a chord if a chord-playing instrument can fill
;;;   in the notes (all notes of chord must still be played)
;;;   2. notes can be left out and instruments can sit out a chord
;;;   3. whole chords can be left out
;;; 
;;; RETURN VALUE
;;; Five statistics values representing the number of chords where: 1. all
;;; pitches & every player used; 2. all pitches but some players tacit; 3. some
;;; pitches and instruments missing; 4. chords completely left out; 5. total
;;; chords attempted
;;;
;;; EXAMPLE
#|
(let* ((mini (make-slippery-chicken  
               '+mini+ 
               :ensemble '(((flt (flute :midi-channel 1))))
               :staff-groupings '(1)
               :tempo-map '((1 (q 60)))
               :set-palette '((set1 ((fs2 b2 d4 a4 d5 e5 a5 d6))) 
                              (set2 ((b2 fs3 d4 e4 a4 d5 e5 a5 d6))))
               :set-map '((1 (set1 set1 set2 set1 set1 set2)))
               :rthm-seq-palette
               '((seq1 ((((4 4) q q q q))   
                        :pitch-seq-palette (1 2 1 3)))  
                 (seq2 ((((4 4) (e) e q h)) 
                        :pitch-seq-palette (1 2 3))))
               :rthm-seq-map '((1 ((flt (seq1 seq1 seq2 seq1 seq1 seq2)))))))
       (new-ens (make-ensemble 'new-ens
                               '((vln (violin :midi-channel 2))
                                 (clr (b-flat-clarinet :midi-channel 3))))))
  (orchestrate mini new-ens 'flt)
  (cmn-display mini))

=> T
|#
;;; 
;;; SYNOPSIS
(let (successes)                        ; for stats
  (defmethod orchestrate ((sc slippery-chicken) new-ensemble original-players
                          &key combos add-more-combos sub-combos start-bar
                            end-bar verbose
                            (artificial-harmonics t) (relax 0) (auto-clefs t)
                            (auto-beam t) (combo-change-fun #'combo-change?)
                            (chords t) sticky-stats)
;;; ****
    (when (or (not sticky-stats) (not successes))
      (setq successes (ml 0 5)))
    (unless (integer-between relax 0 3)
      (error "slippery-chicken::orchestrate: :relax should be between 0 and 3 ~
             inclusive, not ~a" relax))
    ;; MDE Tue Mar  2 15:15:45 2021, Heidhausen
    (when (and add-more-combos sub-combos)
      (error "slippery-chicken::orchestrate: either :add-more-combos or ~
              :sub-combos may be T, but not both."))
    (when (and sub-combos (not combos))
      (error "slippery-chicken::orchestrate: :sub-combos can only by T when ~
              :combos is a list of lists"))
    ;; reset our activity-levels object or whatever the given function does to
    ;; reset 
    (funcall combo-change-fun nil nil) 
    (setq new-ensemble (make-ensemble nil new-ensemble) ; clone or instantiate
          original-players (force-list original-players))
    (let* ((phrases (get-phrases sc original-players :start-bar start-bar
                                 :end-bar end-bar :pad nil))
           (lotsa (when (or (not combos) add-more-combos)
                    (lotsa-combos new-ensemble 100)))
           (sub-groups (sub-groups combos))
           (all-combos (append combos sub-groups lotsa))
           ;; turned the given list of instruments into an assoc-list where the
           ;; ids are the number of instruments in the combo(s)
           (combos-al (organise-combos new-ensemble all-combos))
           (num-passes (length phrases))
           combo combo-players combo-relax-val)
      (add-ensemble-players sc new-ensemble)
      (update-slots sc)                 ; so that bar-nums are correct
      (loop for pphrase in phrases
         for player in original-players
         for pass from 1 do
           (when verbose
             (format t "~&****** Orchestrating original player ~a/~a: ~a ******"
                     pass num-passes player)
             (format t "~&        Ensemble: ~a" (players new-ensemble)))
         ;; strictly speaking phrases have no meaning here as combos can change
         ;; at any point but keep the phrase processing in place for now in case
         ;; we do want to somehow differentiate phrases in the future.
           (loop for phrase in pphrase do
              ;; duplicate the events, with all chord notes. we then change
              ;; notes afterwards
              ;; number of notes in each chord/event can vary from event to
              ;; event. within a phrase we'll re-use a combo if the number of
              ;; notes in a chord reoccurs and our combo-change? function tells
              ;; us not to change
                (loop for event in phrase do 
                   ;; NB event is from the original player
                     (unless (is-rest event)
                       ;; not only will this get a combo that can play the chord
                       ;; (if one exists) but it will also see if our combo is
                       ;; free for this event. nb if it's a rest bar, we need to
                       ;; double the events of the phrase but force them into
                       ;; rests then we can change individual rests back into
                       ;; notes. so there are side effects here but all good
                       ;; ones.
                       (unless (is-tied-to event)
                         (multiple-value-setq
                             (combo combo-relax-val)
                           (get-combo sc event combos-al combo-change-fun
                                      artificial-harmonics chords))
                         (setq combo-players (mapcar #'first combo))
                         (when (or (and (not combo) (/= relax 3))
                                   (and combo (> combo-relax-val relax)))
                           (error "slippery-chicken::orchestrate: bar ~a: no ~
                                   combo can play ~%~a."
                                  (bar-num event) (get-pitch-symbol event)))
                         (incf (nth combo-relax-val successes))
                         (incf (nth 4 successes)) ; overall total 
                         (when verbose
                           (format t "~&bar ~a (pass ~a/~a): set ID ~a, ~a: ~
                                    ~%    tried ~a, got ~a~%    "
                                   (bar-num event) pass num-passes
                                   (set-ref event)
                                   (if combo-players combo-players "no players")
                                   (get-pitch-symbol event)
                                   (let ((ps (mapcar
                                              #'(lambda (p)
                                                  (when (second p)
                                                    (get-pitch-symbols
                                                     (second p))))
                                              combo)))
                                     (if ps ps "nothing")))
                           (case combo-relax-val
                             (0 (format t "(all pitches played)"))
                             (1 (format t "(all pitches played, chord-playing ~
                                         instrument filling the gaps."))
                             (2 (format t "(some pitches missing.)"))
                             (3 (format t "(chord will be skipped.)")))))
                       ;; this must be done for attacked notes and tied-to notes
                       (when combo
                         (copy-to-combo sc event combo))))))
      ;; (break)
      ;; (print (get-dynamics (get-note sc 34 1 'vln2) t))
      ;; todo: remove cresc/dim from rests
      (when auto-beam (auto-beam sc))
      (when auto-clefs (auto-clefs sc))
      (force-rest-bars sc 1 nil nil t)
      (update-slots sc)
      (move-dynamics-from-rests sc)
      ;; remove dynamics from rests, as we'll probably still have some since we
      ;; no longer delete them when force-rest is called
      (sc-remove-dynamics sc 1 (num-bars sc) nil t)
      (check-ties sc)   ; we have to do this to make sure tied notes are updated
      (when verbose
        (format
         t "~&Total of ~a chords orchestrated: ~%    ~a with all pitches ~
            and every instrument of the combo, ~%    ~a with all pitches ~
            but some combo instruments missing, ~%    ~a with some ~
            pitches and instruments missing, ~%    ~a left out completely."
         (fifth successes) (first successes) (second successes)
         (third successes) (fourth successes)))
      ;; 1. all pitches & every player, 2. all pitches but some players tacit,
      ;; 3. some pitches and instruments missing, 4. completely left out,
      ;; 5. total chords
      (values-list successes))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; returns a combo that can play the chord __and__ is free (has rests) to play
;;; it, along with a second value which is the success (value of 1|2|3)
(defmethod get-combo ((sc slippery-chicken) (e event) (combos assoc-list)
                      &optional 
                        (combo-change-fun #'combo-change?)
                        (artificial-harmonics t) (chords t))
  (let* ((num-notes (num-notes e))
         (cscl (get-data-data num-notes combos))
         (current-combo (if cscl
                            (get-current cscl)
                            (error "slippery-chicken::get-combo: ~
                                    Can't get a combo for ~a notes."
                                   num-notes)))
         (change (funcall combo-change-fun current-combo combos))
         best best-combo best-success best-count partial partial-combo
         partial-success partial-count free frees possible success
         possible-count)
    (flet ((finalize (result combo fsuccess)
                                        ; convert the index to player ID
             (values (mapcar #'(lambda (p) (cons (nth (first p) combo)
                                                 (rest p)))
                             result)
                     (if (mapcar #'first result) fsuccess 3))))
      (when change
        (setq current-combo (get-next cscl)))
      (loop for i to (sclist-length cscl) do
         ;; NB the chord method (called by event) goes through the combo
         ;; permutations
           (multiple-value-setq (possible success)
             ;; event method, so player IDs convert to instrument objects
             (combo-chord-possible? e current-combo artificial-harmonics sc
                                    chords))
         ;;          must retain the combo perm order returned by chord method
           (setq frees (when possible
                         (mapcar #'(lambda (p)
                                     (free-to-double?
                                      sc e (nth (first p) current-combo)))
                                 possible))
                 free (every #'values frees))
         ;; (unless (and free (zerop success))
           (setq possible-count (count-combo-pitches possible))
         ;;  so we return as the first value a list of 3-element sublists: the
         ;; player (symbol), the pitch or artificial harmonic that they can play
         ;; and the instrument object
           (if free
               ;; we got one where each instrument can play a note
               (if (zerop success) 
                   (return)
                   ;; we got one but we might get a better one
                   (progn
                     (when (or (not best)
                               (and best (> possible-count best-count)))
                       (setq best possible
                             best-combo current-combo
                             best-success success
                             best-count possible-count))
                     (setq current-combo (get-next cscl))))
               (progn        ; not all free, so deal with partial success
                 ;; could be that combo-chord-possible? returned nil
                 (when possible 
                   (loop for f in frees for p in possible do ;for i from 0 do
                      ;; if the player isn't free, remove its note so it's not
                      ;; used or counted 
                        (unless f (setf (second p) nil)))
                   ;; have to recount now we're remove some pitches
                   (setq possible-count (count-combo-pitches possible))
                   (when (and (> possible-count 0)
                              (or (not partial)
                                  (and partial
                                       (> possible-count partial-count))))
                     (setq partial possible
                           partial-combo current-combo
                           partial-count possible-count
                           ;;                  >= because of artificial harms
                           partial-success (if (>= possible-count num-notes)
                                               1 2))))
                 (setq current-combo (get-next cscl))))
         finally (setq possible nil))
      (cond (possible (finalize possible current-combo success))
            ;; couldn't get the perfect fit so return the best we could get. the
            ;; best* vars have notes for the whole combo, the partial* vars for
            ;; just part of the combo, but even so, the latter could have more
            ;; notes. in any case if both have the same success prefer the combo
            ;; with all players playing.
            ((and partial-success (not best-success))
             (finalize partial partial-combo partial-success))
            ((and (not partial-success) best-success)
             (finalize best best-combo best-success))
            ((and best-success partial-success
                  ;;                              each should be 1 or 2
                  (integer-between (+ best-success partial-success) 2 4))
             ;; remember: the lower the success the better here, like UNIX
             (if (<= best-success partial-success)
                 (finalize best best-combo best-success)
                 (finalize partial partial-combo partial-success)))
            (t (finalize nil nil 3))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; <e> is an event from another player but is <player> free to double that
;;; event i.e. does it have a rest bar or another rest at that point? in the
;;; latter case and for now we merely check that there's a rest of the same
;;; rhythmic duration of <e> at the same bar position, so if that's not true
;;; then the whole bar will have to be free. returns t or nil
(defmethod free-to-double? ((sc slippery-chicken) (e event) player)
  (let* ((bar (get-bar sc (bar-num e) player))    ; the player we're querying
         (ne (get-nth-event (bar-pos e) bar nil)) ; ditto
         last-event
         (end-bar (bar-num e)))
    ;; If player's bar is empty then we'll be calling double-events
    ;; below. in that case we have to see if there's a tie into the next
    ;; bar. if so we'll need to double-events in that (and perhaps
    ;; subsequent bars) also. 
    ;; 
    ;; It's actually not enough to find which bar the current event finishes its
    ;; tie in, as that bar could have a last event which ties into the next
    ;; bar--in that case we need to keep going until we find a last event which
    ;; doesn't tie.
    (setq end-bar
          (loop
             (setq last-event (get-last-event (get-bar sc end-bar (player e))))
             (if (is-tied-from last-event)
                 (setq end-bar (bar-num (find-end-tie sc last-event)))
                 (return end-bar))))
    (if (empty-bars? sc (bar-num e) end-bar player t)
        ;; if the last event in the bar we'll copy from is tied, then we have to
        ;; double events in the next bar, maybe even more to the end of the tie
        (progn
          (double-events sc (player e) player (bar-num e) 1 end-bar nil
                         :auto-beam nil :consolidate-rests nil :update nil
                         :pitches nil)
          ;; we have to turn them into rests because another ensemble might play
          ;; the following chords
          (loop for bar-num from (bar-num e) to end-bar
             for bar = (get-bar sc bar-num player) do
               (force-all-rests bar t)
               (setf (is-rest-bar bar) nil))
          t)
        ;; have we already copied over skeleton events or is there (by chance
        ;; even) a rest of the same duration at the same point in the bar?
        ;; todo: an improvement would be to check following rests if <e> is tied
        ;; from. that way we could use this method outside of the
        ;; orchestrate/get-combo context. for now we're assuming rests have been
        ;; created over whole bars
        (and ne (is-rest ne)
             (equal-within-tolerance (start-time e) (start-time ne))
             (equal-within-tolerance (duration e) (duration ne))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken-edit/swap-marks
;;; AUTHOR
;;; Daniel Ross (mr.danielross[at]gmail[dot]com) 
;;; 
;;; DATE
;;; Thu 22 Aug 2019 17:12:27 BST
;;; 
;;; DESCRIPTION
;;; Replace one dynamic mark with another, or a list of marks with another
;;; list.
;;; 
;;; ARGUMENTS
;;; - the slippery chicken object which contains marks to be swapped
;;; - start-bar: the first bar to start swapping
;;; - end-bar: the last bar to end swapping
;;; - players: the player or players
;;; - old-marks: a single mark or a list of marks
;;; - new-marks: a single mark to a list. NB if a list it must be the same
;;; length as the old-marks list. Maybe I should change this?
;;; 
;;; OPTIONAL ARGUMENTS
;;; nil
;;; 
;;; RETURN VALUE
;;; The number of swapped marks
;;; 
;;; EXAMPLE
#|
(let* ((mini (make-slippery-chicken  
               '+mini+ 
               :ensemble '(((flt (flute :midi-channel 1))))
               :staff-groupings '(1)
               :tempo-map '((1 (q 60)))
               :set-palette '((set1 ((fs2 b2 d4 a4 d5 e5 a5 d6))) 
                              (set2 ((b2 fs3 d4 e4 a4 d5 e5 a5 d6))))
               :set-map '((1 (set1 set1 set2 set1 set1 set2)))
               :rthm-seq-palette
               '((seq1 ((((4 4) (q) (q) q q))   
                        :pitch-seq-palette (1 2)
                        :marks (pp 1)))  
                 (seq2 ((((4 4) (e) e q h)) 
                        :pitch-seq-palette (1 2 3)
                        :marks (p 1 a 1 s 1))))
               :rthm-seq-map '((1 ((flt (seq1 seq1 seq2 seq1 seq1 seq2))))))))
   (print (has-mark (get-note mini 1 1 'flt) 'fff))
   (print (swap-marks mini nil nil nil 'pp 'fff))
   (has-mark (get-note mini 1 1 'flt) 'fff))

NIL
4
=> (FFF)
|#
;;; SYNOPSIS
(defmethod swap-marks ((sc slippery-chicken) start-bar end-bar
                       players old-marks new-marks)
;;; ****
  (let ((count 0)) ; for return testing
    (unless end-bar (setf end-bar (num-bars sc)))
    (unless start-bar (setf start-bar 1))
    (unless players (setf players (players sc)))
    (setf players (force-list players))
    (setf old-marks (force-list old-marks))
    (setf new-marks (force-list new-marks))
    (unless (= (length old-marks)(length new-marks))
      (error (format t "~%swap-marks: old-marks and new-marks must be the same
                       length")))
    (loop for player in players do
         (loop for bn from start-bar to end-bar
            for bar = (get-bar sc bn player)
            do
              (loop for e in (rhythms bar) do
                   (loop for om in old-marks
                      for nm in new-marks do
                        (when (has-mark e om)
                          (rm-marks e om)
                          (add-mark-once e nm)
                          (incf count))))))
    count))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken-edit/fast-microtone-to-chromatic
;;; AUTHOR
;;; Daniel Ross (mr.danielross[at]gmail[dot]com) 
;;; 
;;; DATE
;;; 4 September 2019, London
;;; 
;;; DESCRIPTION
;;; Round microtonal pitches to their nearest chromatic pitch if the time
;;; between successive events is less than or equal to a specified threshold in
;;; seconds. This method is intended to make playing fast pasages in microtonal
;;; music easier for human players. 
;;; 
;;; ARGUMENTS
;;; - The slippery chiocken object containing the pitches to be tested.
;;; - The player or players to be tested
;;; 
;;; OPTIONAL ARGUMENTS
;;; - start-bar: the first bar to start testing in. Default = 1
;;; - end-bar: the last bar to test in. Default = (num-bars sc)
;;; - threshold: the time in seconds between successive events that determines
;;; whether or not a pitch should be rounded to its nearest chromatic. Default =
;;; (fast-leap-threshold sc).
;;; 
;;; RETURN VALUE
;;; A list containing the number of rounded pitches per instrument.
;;; 
;;; EXAMPLE
#|
(let ((mini (make-slippery-chicken  
                 '+mini+ 
                 :title "Your Title Here" 
                 :composer "Your Name Here"
                 :ensemble '(((flt (flute :midi-channel 1 :microtones-midi-channel 2))))
                 :staff-groupings '(1)
                 :tempo-map '((1 (q 60)))
                 :set-palette '((set1 ((fqs2 b2 dqs4 aqf4 dqs5 e5 a5 d6))))
                 :set-map '((1 (set1 set1 set1 set1 set1 set1)))
                 :rthm-seq-palette
                 '((seq1 ((((4 4) q - s s s s -  - e e - q))   
                          :pitch-seq-palette (1 2 3 4 5 6 7 8))))
                 :rthm-seq-map
                 '((1 ((flt (seq1 seq1 seq1 seq1 seq1 seq1))))))))
      (fast-microtone-to-chromatic mini nil :threshold 10)
      (cmn-display mini))
|#
;;; SYNOPSIS
(defmethod fast-microtone-to-chromatic ((sc slippery-chicken) players
                                        &key
                                          (start-bar 1)
                                          end-bar
                                          ;; threshold in secs
                                          threshold)
;;; ****
  (unless players (setf players (players sc)))
  (unless end-bar (setf end-bar (num-bars sc)))
  (unless threshold (setf threshold (fast-leap-threshold sc)))
  (setf players (force-list players))
  (let ((count-list '()))
    (loop for player in players do
         (next-event sc player t start-bar)
         (loop for ne = (next-event sc player t nil end-bar)
            with le
            with count = 0
            while ne
            do
              (when (and le
                         ;; DJR Wed 18 Sep 2019 15:15:06 BST
                         ;; accounting for tied notes
                         (not (is-tied-to ne))
                         (<= (- (start-time ne)
                                (start-time le))
                             threshold))
                (if (is-chord le)
                    (loop for p in (data (pitch-or-chord le))
                       with new-pitches = '() do
                         (if (micro-tone p)
                           (progn (push (pitch-round p) new-pitches)
                                  (incf count))
                           (push p new-pitches))
                       finally
                         (setf (pitch-or-chord le) new-pitches))
                    (progn
                      (when (micro-tone (pitch-or-chord le))
                        (if (written-pitch-or-chord le)
                            (set-written-pitch-or-chord
                             le
                             (pitch-round (pitch-or-chord le)))
                            (setf (pitch-or-chord le)
                                  (pitch-round (pitch-or-chord le))))
                        (incf count)))))
            ;; DJR Wed 18 Sep 2019 15:15:06 BST
            ;; accounting for tied notes
              (unless (is-tied-to ne)
                (setf le ne))
            finally
              (push count count-list)))
    ;; DJR Wed 18 Sep 2019 15:15:06 BST
    ;; add some checks
    ;; DJR Wed 22 Jan 2020 11:34:10 GMT
    ;; Add t
    (check-ties sc t) 
    (check-beams sc :start-bar start-bar :end-bar end-bar :players players
                 :auto-beam t :print nil)
    (nreverse count-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken-edit/make-hammer-friendly
;;; DATE
;;; October 4th 2019
;;; 
;;; DESCRIPTION
;;; Yamaha Disklavier pianos can't play a note immediately after a note-off. The
;;; hammer needs a little time to return to its 'off' position before it can be
;;; used to restrike a note. Tests show this time to be around 60
;;; milliseconds. of course this will be instrument-dependent, so a little
;;; experimentation might be needed.
;;;
;;; This method will go through each event and decrement those events which
;;; repeat a note on the same MIDI channel which start < 'min-time' after the
;;; end-time of the previous event.
;;; 
;;; ARGUMENTS
;;; - the slippery-chicken object
;;; - either a single symbol or a list of symbols for the players to be
;;;   processed
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments
;;; - :start-bar. The bar at which to begin processing (integer). Default = 1.
;;; - :end-bar. The bar at which to end processing (integer). Default = NIL =
;;;   last bar
;;; - :min-gap. The minimum time the hammer needs to return, in
;;;   milliseconds. Default = 60.
;;; - :min-dur. The minimum duration of a note in milliseconds, after reducing
;;;   duration. Default = 50 milliseconds.
;;; 
;;; RETURN VALUE
;;; A list of the number of notes affected per player.
;;; 
;;; SYNOPSIS
(defmethod make-hammer-friendly ((sc slippery-chicken) players
                                 &key
                                   (warn t)
                                   (start-bar 1)
                                   end-bar
                                   (min-gap 60)
                                   (min-dur 50))
;;; ****
  (unless players (setf players (players sc)))
  (unless end-bar (setf end-bar (num-bars sc)))
  (setq players (force-list players))
  (let ((count-list '())
        (min-gap-secs (/ min-gap 1000.0))
        (min-dur-secs (/ min-dur 1000.0))
        last)
    (loop for player in players do
         (next-event sc player t start-bar)
         (setq last (next-event sc player t nil end-bar))
         (loop for this = (next-event sc player t nil end-bar)
            with count = 0 while this do
              (when (and (> (common-notes last this) 0)
                         (= (get-midi-channel last) (get-midi-channel this))
                         (< (- (start-time this) (end-time last))
                            min-gap-secs))
                ;; work out decrement, leaving at least min-dur-secs
                (let* ((new-dur (- (compound-duration-in-tempo last)
                                   min-gap-secs))
                       (inc-for-min (- (compound-duration-in-tempo last)
                                       min-dur-secs))
                       (inc (if (> new-dur min-dur-secs)
                                ;; if our dur is long enough, just decrement the
                                ;; miniumum 
                                (- min-gap-secs)
                                ;; otherwise decrement just enough to achieve
                                ;; min-dur, if that's possible...
                                (if (>= inc-for-min 0)
                                    (- inc-for-min)
                                    (when warn
                                      (warn "make-hammer-friendly: ~
                                             duration is too short to handle: ~
                                             ~%~a" last))))))
                  (when inc
                    ;; (print (compound-duration last))
                    ;; use this method as it updates other slots too
                    (inc-duration last inc)
                    ;; (print (compound-duration last))
                    (incf count))))
              (setq last this)
            finally (push count count-list)))
    (nreverse count-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken-edit/double-player-inverted
;;; DATE
;;; July 14th 2020, Heidhausen
;;; 
;;; DESCRIPTION
;;; Add a new player to the ensemble/piece, double the events of an existing
;;; player, then use the invert method for each bars within the given range to
;;; turn rests into notes and vice-versa. To choose new notes we use curves to
;;; limit high and low, using the sets (from the map) used to generate the
;;; existing voices.
;;; 
;;; ARGUMENTS
;;; - the slippery-chicken object
;;; - the ID of a new player (symbol)
;;; - the instrument for the new player (must be in the instrument-palette)
;;; - the ID of the existing player whose events are inverted (symbol)
;;; - a curve for the maximum upper notes
;;; - a curve for the maximum lower notes. Both these curves can have any
;;;   arbitrary x range but the y values should use MIDI note numbers or note
;;;   symbols. Note that the x range will be stretched over the number of bars
;;;   in the piece, not the given bar range, so that the bar number can be used
;;;   for interpolation.
;;; - a curve to decide whether to actually play a note in the new part or
;;;   not. This can also have any arbitrary x range and will also be stretched
;;;   over the number of bars in the piece, but the y range should be from 0 to
;;;   10 as the interpolated value for the notes in each bar is passed to an
;;;   activity-levels object to determine whether to play or not.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; :start-bar :start-event :end-bar :end-event. These are passed to
;;; double-events. See that method for details but note that the defaults for
;;; the end-* arguments are NIL which in turn default to the last bar/event.
;;; :reset-midi-channels. T or NIL to reset all events in the slippery-chicken
;;; object to the original midi channels of the parent player. Default = T.
;;; 
;;; RETURN VALUE
;;; the (modified) slippery-chicken object
;;; 
;;; SYNOPSIS
(defmethod double-player-inverted ((sc slippery-chicken) new-player new-ins
                                   existing-player upper-curve lower-curve
                                   &key (start-bar 1) (start-event 1)
                                     (activity-curve '(0 10 100 10))
                                     end-bar end-event 
                                     (reset-midi-channels t) (update-slots t)
                                     (midi-channel 1)
                                     (microtones-midi-channel -1))
;;; ****
  (add-player sc new-player :instrument new-ins
                            :instrument-palette (instrument-palette sc)
                            :midi-channel midi-channel
                            :microtones-midi-channel microtones-midi-channel)
  (double-events sc existing-player new-player start-bar start-event end-bar
                 end-event :consolidate-rests nil :auto-beam nil)
  (let ((upper (doctor-env upper-curve (num-bars sc)))
        (lower (doctor-env lower-curve (num-bars sc)))
        (al (make-al))
        (ac (new-lastx activity-curve (num-bars sc)))
        last-set)
    ;;    (print upper) (print lower)
    (map-over-bars
     sc start-bar end-bar new-player
     #'(lambda (bar)
         (let* ((set (get-set-for-bar-num sc (bar-num bar)))
                (notes
                  (limit-for-instrument
                   (clone (if set (setq last-set set) last-set))
                   (get-data new-ins (instrument-palette sc))
                   ;; MDE Tue Mar 29 11:21:58 2022, Heidhausen -- this used to
                   ;; call midi-to-note but remember that doctor-env returns
                   ;; degrees as y-values
                   :upper (degree-to-note
                           (round (interpolate (bar-num bar) upper)))
                   :lower (degree-to-note
                           (round (interpolate (bar-num bar) lower))))))
           ;; (print (pitch-list-to-symbols notes))
           (invert bar notes t)
           ;; now use the activity curve to turn notes back off if necessary
           (loop with level = (interpolate (1- (bar-num bar)) ac)
                 for e in (rhythms bar)
                 for active = (active al level)
                 do (unless active (force-rest e)))))))
  (when reset-midi-channels (reset-midi-channels sc))
  (when update-slots (update-slots sc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken-edit/delete-tempi
;;; DATE
;;; June 26th 2022, Karlsruhe
;;; 
;;; DESCRIPTION
;;; Delete all tempo-change slot tempo objects from all events in the given
;;; players. NB Call update-slots after using this and similar methods for the
;;; last time, so that event timing reflects tempo changes.
;;; 
;;; ARGUMENTS
;;; - 
;;; 
;;; OPTIONAL ARGUMENTS
;;; - the start bar. Default = NIL = 1
;;; - the end bar. Default = NIL = last bar of piece
;;; - the player(s). Either a list of or single symbol(s). Default = NIL = all
;;;   players 
;;; 
;;; RETURN VALUE
;;; - the newly modified slippery-chicken object
;;; 
;;; SYNOPSIS
(defmethod delete-tempi ((sc slippery-chicken) &optional start-bar end-bar
                                                         players)
;;; ****  
  (map-over-events sc start-bar end-bar players
                   #'(lambda (e) (delete-tempo-change e)))
  sc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken-edit/set-score-order
;;; DATE
;;; 24th August 2024
;;; 
;;; DESCRIPTION
;;; Set the order of the players in the score. This is basically a convenience
;;; function to set the players slot of the piece object slot but also checks
;;; that your new players are in the piece already.
;;;
;;; NB won't (yet) change order of players in CMN scores.
;;; 
;;; ARGUMENTS
;;; - the slippery-chicken object
;;; - a list of players (symbols). Note that these must already be part of the
;;;   piece. 
;;; 
;;; RETURN VALUE
;;; The slippery chicken object
;;; 
;;; SYNOPSIS
(defmethod set-score-order ((sc slippery-chicken) players)
;;; ****  
  (let ((current-players (players sc)))
    (loop for player in players do
      (unless (member player current-players)
        (error "slippery-chicken-edit::set-score-order: ~a is not in the piece"
               player)))
    (setf (players (piece sc)) players))
  sc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken-edit/double-events-scaled
;;; DATE
;;; 15.5.25
;;; 
;;; DESCRIPTION
;;; Double the events of one player and then rhythmically scale (augment or
;;; diminute) them. Add them to a new part, where time signature and rhythmic
;;; properties allow. Note that
;;; 
;;; ARGUMENTS
;;; - the slippery-chicken object
;;; - the scaler to apply to the rhythms, e.g. 2 would be half speed
;;; - the player whose events will be copied before scaling
;;; - the symbol associated with the new player in the ensemble
;;; - the instrument symbol (from the :instrument-palette) for the new player
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :transposition. The semitone transposition for the master-player's
;;;   pitches. Either this or the :pitches argument may be given, not both.
;;; - :pitches. A list of pitch/chord objects or symbols/symbols lists (chords)
;;;   that will be used to replace the pitches/chords of the events of the 
;;;   master player. If this is shorter than the number of events that we 
;;;   generate, then they will be reused circularly. If this is not passed, then
;;;   the pitches of the master-player will be used, optionally transposed (see
;;;   above).
;;; - :auto-beam. Whether to call auto-beam on the slippery-chicken object after
;;;   the process is finished.
;;; - :update-slots. Whether to call update-slots on the slippery-chicken object
;;;   after the process is finished. This is left to the user as it is
;;;   computationally expensive and thus might be put off to a later stage.
;;; - :verbose. Print useful information as the process takes place.
;;; - :midi-channel. the midi-channel for the new player
;;; - microtones-midi-channel. the microtones-midi-channel for the new player.
;;;   Default = -1 = same as :midi-channel.
;;; - :instrument. a symbol ID for an existing instrument in the
;;;   instrument-palette (the next argument). This is actually required unless
;;;   the default of 'computer is acceptable or a player object is passed as
;;;   second argument.
;;; - :instrument-palette. an instrument-palette object in which the instrument
;;;   exists. Default is the standard palette.
;;; - :start-bar (:start-event, :end-bar, :end-event). The start/end points for
;;;   that determine the range that we first of all copy, before scaling and
;;;   begin pasting from :new-start-bar. Defaults are the whole piece.
;;; - :new-start-bar. The bar at which we start pasting in the scaled
;;;   events (it's not possible to specify a starting event) . Default = 1.
;;; - :new-end-bar. The bar at which we end (inclusive) pasting in the scaled
;;;   events (it's not possible to specify a stopping event) . Default = NIL =
;;;   end of the piece.
;;; - :bar-ok-fun. A function to call in every bar to decide whether to double
;;;   events to this bar (e.g. you might want to skip over bars in which other
;;;   players aren't playing). Requires and will be passed two arguments: the
;;;   rthm-seq-bar and slippery-chicken objects. Default returns T = every bar.
;;; 
;;; RETURN VALUE
;;; the sc object with the new player added
;;;
;;; SYNOPSIS
(defmethod double-events-scaled
    ((sc slippery-chicken) rthm-scaler master-player
     new-player new-instrument
     &key transposition (new-start-bar 1) new-end-bar (start-bar 1) 
     start-event end-bar end-event pitches (auto-beam t) (midi-channel 1)
     (microtones-midi-channel -1) (update-slots t) verbose
     (bar-ok-fun #'(lambda (bar sc) (declare (ignore bar sc)) t))
     (instrument-palette +slippery-chicken-standard-instrument-palette+))
;;; ****
  (when (and transposition pitches)
    (error "double-events-scaled: use either :transposition or :pitches, ~
            not both.")) 
  (when (and pitches (not (cscl-p pitches)))
    (setq pitches (make-cscl (mapcar #'make-pitch pitches)
                             :id 'double-events-scaled)))
  (unless new-end-bar (setq new-end-bar (num-bars sc)))
  ;; this creates empty bars, ready for filling
  (add-player sc new-player :instrument new-instrument
                            :instrument-palette instrument-palette
                            :midi-channel midi-channel
                            :microtones-midi-channel microtones-midi-channel)
  (let* ((m-events (get-events-from-to sc master-player start-bar start-event
                                       end-bar end-event))
         (s-events (loop for me in m-events
                         for se = (scale me rthm-scaler t)
                         do (if pitches
                              (setf (pitch-or-chord se) (get-next pitches))
                              (when (and transposition
                                         (not (zerop transposition)))
                                (transpose se transposition :destructively t)))
                            (setf (player se) new-player)
                         collect se))
         bar used full)
    (update-slots sc) ; this one necessary so that at least bar-nums get updated
    (loop for bar-num from new-start-bar to new-end-bar do
      (setq bar (get-bar sc bar-num new-player))
      (when (funcall bar-ok-fun bar sc)
        ;; long rests can mean we never fill another bar
        (loop until (< (duration (first s-events))
                       (bar-duration bar))
              do (when verbose
                   (format t "~&Skipping ~a rest" (data (first s-events))))
                 (pop s-events))
        (setq used (fill-with-rhythms
                    bar s-events :is-full-error nil :warn nil
                    :midi-channel midi-channel 
                    :microtones-midi-channel
                    microtones-midi-channel)
              full (pad-right bar :dots t))
        (set-player bar new-player)
        (if (and full (> used 0))
          (progn 
            (setq s-events (nthcdr used s-events))
            ;; we might have used up some events but they might all be rests
            (when (all-rests? bar)
              (force-rest-bar bar))
            (when verbose
              (format t "~&Added ~a events to bar ~a" used bar-num)))
          (progn
            (force-rest-bar bar)
            (when verbose
              (format t "~&Couldn't add events to bar ~a" bar-num)
              (format t "~%  Next events: ")
              (loop for e in s-events repeat 5 do
                (print-simple e))))))))
  (when auto-beam (auto-beam sc))
  (when update-slots (update-slots sc))
  sc)
             
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Sat May 24 16:02:05 2025, Heidhausen -- regenerate a tempo map from the
;;; tempo-change slots of events, e.g. in sc-combine. Just generates the list
;;; required to make a tempo-map object, but not the object itself.
;;; <first> (a BPM) will only be used if no tempo is extracted for bar 1
(defmethod tempo-map-from-events ((sc slippery-chicken) &optional tempo1)
  (let ((result '())
        e1)
    ;; (print tempo1)
    ;; all player events should have a tempo-change, not just the top player, so
    ;; it should be safe to just proccess the first player
    (map-over-events sc 1 nil (first (players sc))
                     #'(lambda (e)
                         (unless e1 (setq e1 e)) ; get the first event
                         (let ((tc (get-tempo-for-map e)))
                           (when tc
                             (unless (and tempo1 (= 1 (first tc)))
                               (pushnew tc result :test #'equalp))))))
    (setq result (reverse result))
    ;; (print result)
    ;; whether we got a tempo in bar 1 or not, if tempo1 is given, we used that
    ;; and assume it's 1/4 notes unless tempo1 is a tempo object
    (when (and result tempo1)           ; (/= 1 (first (first result))))
      (push (list 1 (if (tempo-p tempo1)
                      (list (beat tempo1) (bpm tempo1))
                      (list 'q tempo1)))
              result))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken-edit/set-tempo
;;; DATE
;;; June 14th 2025
;;; 
;;; DESCRIPTION
;;; Change the tempo at a given bar. If the optional argument is non nil, then
;;; the 2nd argument is interpreted as a scaler for the existing tempo markings.
;;; 
;;; ARGUMENTS
;;; - a slippery-chicken object
;;; - a tempo object or BPM number; if the latter then a beat-value of quarter
;;;   notes will be assumed.
;;; - the bar number for the change (on the first event). Mid-bar changes upon
;;;   request :)
;;;
;;; OPTIONAL ARGUMENTS
;;; T or NIL to use the 2nd argument as a scaler instead of a BPM.
;;; 
;;; RETURN VALUE
;;; 
;;; SYNOPSIS
(defmethod set-tempo ((sc slippery-chicken) tempo bar-num &optional scaler warn)
  (loop with tpo = (make-tempo tempo)
        for bar in (get-bar sc bar-num) ; all players of course
        do
           (if scaler
             (scale-tempo bar tempo warn)
             (add-tempo bar tempo))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Related functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  The structure of a slippery-chicken object is as follows:
;;; slippery-chicken -> piece -> section (plus subsections where appropriate) ->
;;; player-section ->  sequenz -> rthm-seq-bar -> event
;;; e.g. (get-bar (get-nth 0 (get-data 'vn (get-data-data 1 (piece +mini+)))) 1)
;;; (Remember that the the data slot of the piece object is a list of
;;; named-objects the data of which are the sections. The data slot of a
;;; section is simply a list of player-sections.)
;;; 
;;; So we can create an sc object on-the-fly by stuffing a single section
;;; containing a single player-section containing a single sequenz with all the
;;; given bars.
;;;
;;; ****f* slippery-chicken-edit/bars-to-sc
;;; DESCRIPTION
;;; Take a list of rthm-seq-bars and add them to a new or existing
;;; slippery-chicken object. It works in three different ways:
;;;    1. Create a new slippery chicken object with one player and one section.
;;;    2. Add new parts to an already existing section in an already exisiting
;;;       slippery chicken object. 
;;;    3. Add a new section to an already exisiting part in and already
;;;       exisiting object.
;;; 
;;; In the second case, we assume you are creating a slippery chicken object
;;; part by part with this function. It is not currently possible to
;;; add a part like this in the middle of the score--the new part will be added
;;; to the end of the last group of the ensemble (bottom of score) so make sure
;;; to add parts in the order you want them.
;;;
;;; In the third case, you can add a new section to an exisitng part, but you
;;; cannot add a new section to a part that does not exist.
;;;
;;; NB Bear in mind that if you want to use midi-play, then the events in the
;;;    bars will need to have their midi-channel set (e.g. via make-event).
;;;    It's also the caller's responsibility to ensure that the parts added have
;;;    the same time-signature structure as any existing part.
;;; 
;;; ARGUMENTS
;;; - A list of rthm-seq-bars
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :sc. Either an existing slippery-chicken object or nil if one should be
;;;   created automatically.  If nil, the following three arguments must be
;;;   specified, otherwise they will be ignored.  Default = NIL.
;;; - :sc-name.  The name (symbol) for the slippery-chicken object to be
;;;   created.  This will become a global variable. Default = '*auto*.
;;; - :player.  The name (symbol) of the player to create. Default =
;;;   'player-one. (Remember that Lilypond has problems with player names
;;;    with numbers in them :/ )
;;; - :instrument.  The id (symbol) of an already existing instrument in the
;;;    instrument-palette. Default = 'flute.
;;; - :update. Whether to call update-slots on the new slippery-chicken
;;;   object (to update timing info. etc.).  Default = T.
;;; - :section-id.  The section id.  Default = 1.
;;; - :tempo. The tempo in BPM for the whole slippery-chicken object (piece).
;;; - :midi-channels. A 2-element list specifying the midi-channels for the
;;;   chromatic and microtonal notes of the player. This will not change
;;;   channels of the events in the bars but is assumed to correspond to those.
;;; 
;;; RETURN VALUE
;;; A slippery-chicken object.
;;; 
;;; SYNOPSIS
(defun bars-to-sc (bars &key sc (sc-name '*auto*) (player 'player-one)
                          (midi-channels '(1 2))
                          (instrument-palette 
                           +slippery-chicken-standard-instrument-palette+)
                          (tempo 60) (instrument 'flute) (section-id 1)
                          (update t))
;;; ****
  ;; (unless (and bars (listp bars) (rthm-seq-bar-p (first bars)))
  ;; MDE Wed Sep 19 13:41:36 2018 -- every!
  (unless (and bars (listp bars) (every #'rthm-seq-bar-p bars))
    (error "slippery-chicken-edit::bars-to-sc: first argument should be a ~
            list of rthm-seq-bar objects: ~&~a" bars))
  ;; MDE Thu Sep 24 18:51:20 2020, Heidhausen -- attach tempo to first event
  (when tempo (setf (tempo-change (get-nth-event 0 (first bars))) tempo))
  ;; MDE Fri Mar 18 17:18:19 2022, Paris -- if we have a transposing instrument,
  ;; update the written pitches to reflect this as they probably don't have this
  ;; slot yet
  (let ((ins (get-data instrument instrument-palette)))
    (when (transposing-instrument-p ins)
      (loop for bar in bars do
        (loop for event in (rhythms bar) do
          (set-written-pitch-or-chord event nil ins)))))
  ;; MDE Wed Sep 19 13:39:54 2018 --
  (loop for bar in bars with psf = (list section-id player) do
     ;; MDE Fri Oct 12 08:59:02 2018 -- don't forget this or multi-bar-rests
     ;; will fail miserably
       (setf (player-section-ref bar) psf)
       (update-events-player bar player))
  ;; DJR Thu 9 Jan 2020 16:15:25 GMT
  ;; Safety check if we're adding a new section to an exisiting part, then the
  ;; numbering must be sequential.
  (loop for i from 1 to section-id do
       (when (and (< i section-id)
                  (null (get-section sc i nil)))
         (error "slippery-chicken-edit::bars-to-sc: section ids must be ~
                 sequential")))
  (let* ((seq (clone-with-new-class (make-rthm-seq bars) 'sequenz))
         (ps (make-player-section (list seq) player))
         ;; DJR Thu  9 Jan 2020 10:12:48 GMT
         new-section ; for use when adding a new section
         (section (if sc
                      (let ((s (get-section sc section-id nil)))
                        (if s
                            (push ps (data s))
                            (progn
                              ;; DJR Thu 9 Jan 2020 10:13:36 GMT
                              ;; generate new section if we're adding one to an
                              ;; existing sc object with the same player.
                              (setf new-section t)
                              (make-section (list ps) section-id))))
                      (make-section (list ps) section-id)))
         (piece (if sc
                    (piece sc)
                    (make-piece
                     (list (make-named-object section-id section))
                     sc-name)))
         ;; MDE Thu Nov  7 18:42:54 2019 
         player-obj)
    ;; DJR Thu 9 Jan 2020 16:14:49 GMT
    ;; If we're adding a new section to an existing part
    (when new-section
      (add (make-named-object section-id section) (piece sc))
      (link-named-objects (piece sc)))
    (if sc
        ;; DJR Thu 9 Jan 2020 16:14:49 GMT
        ;; We don't need to do this if we've already added a new section.
        (unless new-section
          (progn
            (setf player-obj (add-player (ensemble sc) player
                                         :instrument instrument
                                         :instrument-palette instrument-palette)
                  ;; MDE Thu Nov 7 18:44:23 2019 -- set the player's
                  ;; midi-channel otherwise we'll put programme changes on
                  ;; channel 1
                  (midi-channel player-obj) (first midi-channels)
                  (microtones-midi-channel player-obj) (second midi-channels)
                  ;; MDE Wed Aug  5 14:30:39 2020, Heidhausen -- also add player
                  ;; to ins-hier  
                  (instruments-hierarchy sc)
                  (econs (instruments-hierarchy sc) player)
                  (players piece) (econs (players piece) player))
            ;; we add to the last staff group by default
            ;; MDE Tue Oct 30 08:20:23 2018 -- use the new method
            (staff-groupings-inc sc)
            ;; (incf (first (last (staff-groupings sc))))
            (incf (num-players piece))))
        (progn
          (unless sc-name
            (error "slippery-chicken-edit::bars-to-sc: sc-name cannot be NIL"))
          (link-named-objects piece)
          (setf sc (make-minimal-sc sc-name player instrument
                                    instrument-palette)
                (piece sc) piece)))
    ;; MDE Wed Aug 29 17:16:56 2018
    (when tempo ; MDE Thu Nov  7 17:19:23 2019 -- only when given
      (setf (tempo-map sc) `((1 (q ,tempo)))))
    (when update
      (update-slots sc nil 0 0 1 nil nil t t)
      (update-write-time-sig2 (piece sc)))
    (update-instruments-total-duration sc)
    (check-time-sigs sc)
    (cleanup-rest-bars sc)
    (check-tuplets sc)
    (check-beams sc)
    sc))
;;; ****

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; if you just want an sc-combine map to splice one range of bars after
;;; another, rather than jumping around in the output, pass a map with 3-element
;;; lists here to get the output start bars as the 4th element
(defun map-follow-on (map &optional (start-bar 1))
  (loop for bars in map
        ;; bear in mind that players might be listed from the 4th element
        for first3 = (subseq bars 0 3)
        for rest = (subseq bars 3)
        collect (append (econs first3 start-bar) rest)
        do (incf start-bar (1+ (- (third bars) (second bars))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* slippery-chicken/sc-combine
;;; DATE
;;; 22nd May 2025
;;; 
;;; DESCRIPTION
;;; Combine an arbitrary number of slippery-chicken objects into a new
;;; slippery-chicken by referencing the bars and players of the existing objects
;;; to copy (clone). This takes a map as the first argument that looks something
;;; like:
;;;
;;; '((sc-object-num start-bar end-bar result-start-bar player1 player2 ...))
;;;
;;; There can be as many sublists as necessary. <sc-object-num> is the 1-based
;;; index into the <sc-objects> list (2nd argument). <start-bar> and <end-bar>
;;; reference the existing sc-object and are inclusive. If <end-bar> is nil,
;;; then it will be set to the last bar of the sc-object. <result-start-bar> is
;;; where things should be written in the new slippery-chicken object--this can
;;; be anywhere, and even overlap/overwrite other copied-in bars, as long as the
;;; metric structure matches. There can be as many players referenced as desired
;;; but each must of course be found in the referenced sc-object (see also the
;;; double-events and delete-events methods for how to move things around
;;; afterwards, if necessary. Players can also be omitted from the map's sublist
;;; whereupon all players in that sc-object will be copied.
;;;
;;; Note that rest bars will automatically be created for players that have no
;;; notes to play and that if there are any bars without notes at all, for any
;;; player, then rest-bars will be created using the :rest-time-sig keyword
;;; argument as the meter.
;;;
;;; Note also that writing into new bars doesn't have to appear in the map in
;;; any particular order, and neither do the <sc-objects> E.g. we could write
;;; bars 50-100 of the result first, using bars from sc-object 3, then 10-20
;;; afterwards using sc-object 1.
;;;
;;; The map can also omit the result-start-bar so that bar ranges are just
;;; appended one after the other in the result. In that case do set :follow-on t
;;; in order for the result start bars to be automatically calculated. The
;;; players can also be omitted here. Note however that the order of the map
;;; will determine the order in the output. E.g. 
;;; 
;;; '((sc-object-num start-bar end-bar player1 player2 ...))
;;; 
;;; ARGUMENTS
;;; - the map: a list as described above
;;; - a list of slippery-chicken objects
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :max-bars. The maximum number of bars that will be generated. Defaults
;;;   generously to 1000 but can go as high as you like, memory permitting.
;;; - :max-players. The maximum number of players we'll use. The same applies
;;;   here as to :max-bars, as both are used to initialise the array we use as
;;;   the workhorse in remapping. Note that the resultant slippery-chicken
;;;   object will only have as many bars and players as needed by the mapping
;;;   procedure. Default = 50.
;;; - :new-sc-name. A symbol to be used as the global variable for the resultant
;;;   slippery-chicken object, as with make-slippery-chicken.
;;; - :instrument-palette. The instrument palette used in creating the
;;;   <sc-objects>. NB it is a (not very daunting) restriction of this method
;;;   that all listed sc-objects must have used the same palette. Default =
;;;   +slippery-chicken-standard-instrument-palette+
;;; - :rest-time-sig. The time signature to be used when creating rest bars
;;;   where no instrument is in play.
;;; - :add-rehearsal-letters. T or NIL to indicated whether rehearsal letters
;;;   should be placed at every point in the map, i.e. for every copy
;;;   procedure. NB in any case, existing rehearsal-letters will not be copied
;;;   over from the sc-objects in the 2nd arguments. Default = T.
;;; - :follow-on. T or NIL or a number to indicate that bar ranges should just
;;;   be appended after each other (see above). If a number is given, this will
;;;   be the start-bar in the result. Default = NIL.
;;; 
;;; RETURN VALUE
;;; - a new slippery chicken object
;;; 
;;; SYNOPSIS
(defun sc-combine (map sc-objects
                   &key (max-bars 1000) (max-players 50)
                   (new-sc-name '*sc-combine*)
                   (add-rehearsal-letters t)
                   follow-on
                   (instrument-palette 
                    +slippery-chicken-standard-instrument-palette+)
                   (rest-time-sig '(2 4)))
;;; ****
  (when follow-on
    (setq map (map-follow-on map (if (numberp follow-on) follow-on 1))))
  (let* ((bars-array (make-array (list max-players max-bars) ; rows columns
                                 :initial-element nil :element-type t))
         ;; just a list of the players, as they occur, in order to access the
         ;; array row for the player
         (all-players '())
         ;; sim. for the instruments (symbols) they play
         (all-instruments '())
         (player-count 0)
         ;; the start bars in the result i.e. fourth elements in the maps
         (rsbs '()) 
         (default-rest-bar (make-rest-bar rest-time-sig))
         (result-first-bar-num 999999)
         (bars '())
         result)
    ;; stuff all the bars we need into bars-array
    (loop for mapping in map
          for sc = (nth (1- (first mapping)) sc-objects)
          for start-bar = (second mapping)
          for end-bar = (third mapping)
          for result-start-bar = (fourth mapping)
          for players = (nthcdr 4 mapping)
          do
             (unless (= 1 result-start-bar) (push result-start-bar rsbs))
             ;; i.e. unless players are listed in this mapping, all wil be used
             (unless players (setq players (players sc)))
             ;; allow the use of nil as an end bar (runs to end of piece then)
             (unless end-bar (setq end-bar (num-bars sc)))
             (when (< result-start-bar result-first-bar-num)
               (setq result-first-bar-num result-start-bar))
             (loop for player in players
                   for player-pos = (position player all-players)
                   ;; this is only needed for bars-to-sc
                   for ins = (id (get-instrument-for-player-at-bar player 1 sc))
                   do
                      (unless ins
                        (error "sc-combine: couuldn't get instrument for ~
                                player ~a in ~a" player (id sc)))
                      (when (player-doubles sc player)
                        (error "sc-combine: player ~a: doubling players ~
                                not (yet) handled." player))
                      (unless player-pos ; got a new player
                        (setq all-players (econs all-players player)
                              all-instruments
                              (econs all-instruments ins)
                              player-pos player-count)
                        (incf player-count))
                      ;; get the bars from the sc as a list then stuff them
                      ;; wherever we need them into the array 
                      (setq bars (get-bars sc start-bar end-bar player))
                      (loop for i from (1- result-start-bar)
                            for bar in bars do
                              (setq bar (clone bar))
                              ;; delete any ending double bars
                              (when (= 2 (bar-line-type bar))
                                (setf (bar-line-type bar) 0))
                              (setf (aref bars-array player-pos i) bar))))
    ;; the the number of bars in the new sc object (result)
    (let ((new-end-bar-num (get-2d-array-last-non-nil-col bars-array))) ;0-based
      ;; any nil bars need to become rest-bars with time-sig of an existing bar
      ;; at that position or rest-time-sig if no bars there
      (loop for bar-n to new-end-bar-num
            with rest-bar
            do
               (multiple-value-bind (existing-bar row)
                   (get-2d-array-non-nil-at-col bars-array bar-n)
                 (setq rest-bar (if existing-bar
                                  (force-rest-bar (clone existing-bar))
                                  ;; no one is playing in this bar so use
                                  ;; default meter for GP
                                  default-rest-bar))
                 (loop for p below player-count
                       with this-bar
                       do
                          (unless (and row (= p row))
                            ;; cloned already if there
                            (setq this-bar (aref bars-array p bar-n))
                            (if this-bar
                              (progn    ; a bar was there
                                (unless (time-sig-equal this-bar existing-bar)
                                  (error "sc-combine: different time ~
                                          signatures at new bar ~
                                          number ~a~%~a"
                                         (1+ bar-n) existing-bar))
                                ;; brutal deletion of tempo marks so that only
                                ;; the first player's tempo counts/overwrites
                                (delete-tempi this-bar))
                              ;; no bar was there so clone and store either an
                              ;; existing bar or the default rest bar
                              (setf this-bar (clone rest-bar)
                                    (aref bars-array p bar-n) this-bar))))))
      ;; two helper functions to extract bars from the array (as a list) ...
      (labels ((get-bars-list (player-index) ; row index
                 (loop for bar-num to new-end-bar-num
                       collect (aref bars-array player-index bar-num)))
               ;; ... and to create the new sc object or add a player to it
               (b2sc (player-index player-sym ins &optional sc) ; ditto
                 (bars-to-sc (get-bars-list player-index)
                             :sc sc :sc-name (unless sc new-sc-name)
                             :instrument-palette instrument-palette
                             :player player-sym :instrument ins :tempo nil
                             :update nil)))
        ;; now create our resultant sc object and fill it with bars from
        ;; bars-array
        (setq result (b2sc 0 (first all-players) (first all-instruments)))
        ;; now add the other players to our sc object just created
        (loop for player-i from 1
              for player in (rest all-players)
              for ins in (rest all-instruments) do
                (b2sc player-i player ins result))))
    ;; got to do this here so that tempo-map-from-events can loop through the
    ;; right bar numbers
    (update-slots result)
    ;; (print result-first-bar-num)
    ;; nb guess-tempo uses tempo-change slot if it's there
    (let* ((tempo1 (guess-tempo
                    (first (rhythms (aref bars-array (1- result-first-bar-num)
                                          0)))))
           (tmes (tempo-map-from-events result tempo1))
           (tm (when tmes (make-tempo-map 'from-sc-combine tmes))))
      ;; now add the actual tempo marks and recalculate timings
      (when tm
        (setf (tempo-map result) tm)
        (update-slots result)))
    ;; do this otherwise scores are unhappy and don't display meter changes:
    (set-write-time-sig result)
    (when add-rehearsal-letters
      (setf (rehearsal-letters result) (sort rsbs #'<)))
    (change-bar-line-type result (num-bars result) 2) ; add the final double bar
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Fri Apr 19 15:03:05 2013 -- make a dummy (pretty empty) sc structure
(defun make-minimal-sc (sc-name player instrument
                        &optional
                          (instrument-palette
                           +slippery-chicken-standard-instrument-palette+))
  (make-slippery-chicken
   sc-name
   :instrument-palette instrument-palette
   :ensemble `(((,player (,instrument :midi-channel 1))))
   :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
   :set-map '((1 (1)))
   :tempo-map '((1 (q 60)))
   :rthm-seq-palette '((1 ((((4 4) w)))))
   :rthm-seq-map `((1 ((,player (1)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* get-nearest-note/player
;;; AUTHOR
;;; Daniel Ross (mr.danielross[at]gmail[dot]com) 
;;; 
;;; DATE
;;; Tue 11 Aug 2020 17:16:10 BST
;;; 
;;; DESCRIPTION
;;; Like get-note, but will return the nearest note before or after the
;;; specified bar / event number place from, potentially, any bar.
;;; 
;;; ARGUMENTS
;;; - an sc object
;;; - the bar number to start looking
;;; - the event number within the bar to start looking
;;;
;;; OPTIONAL ARGUMENTS
;;; None.
;;; 
;;; RETURN VALUE
;;; If found, the nearest sounding event in the stated player's part and the
;;; number of events away (positive for forwards, negative for backwards),
;;; otherwise NIL.
;;; 
;;; EXAMPLE
#|
(let ((mini
          (make-slippery-chicken
           '+mini+
           :ensemble '(((sax (alto-sax :midi-channel 1))
                        (vn (violin :midi-channel 1))))
           :set-palette '((1 ((c2 d2 g2 a2 e3 fs3 b3 cs4 fs4 gs4 ds5 f5 bf5)))) 
           :set-map '((1 (1 1 1 1 1))
                      (2 (1 1 1 1 1))
                      (3 (1 1 1 1 1)))
           :rthm-seq-palette '((1 ((((4 4) h q e s s))
                                   :pitch-seq-palette ((1 2 3 4 5))))
                               (2 ((((4 4) (w)))))
                               (3 ((((4 4) (h) q e (s) s))
                                   :pitch-seq-palette ((1 2 5)))))
           :rthm-seq-map '((1 ((sax (1 1 1 1 1))
                               (vn (2 2 2 2 2))))
                           (2 ((sax (2 2 2 2 2))
                               (vn (2 2 2 2 2))))
                           (3 ((sax (3 3 3 3 3))
                               (vn (2 2 2 2 2))))))))
    (get-nearest-note mini 11 1 'sax))
=>
EVENT: start-time: 42.000, end-time: 43.000, 
       duration-in-tempo: 1.000, 
       compound-duration-in-tempo: 1.000, 
       amplitude: 0.700 
       bar-num: 11, marks-before: NIL, 
       tempo-change: NIL 
       instrument-change: NIL 
       display-tempo: NIL, start-time-qtrs: 42.000, 
       midi-time-sig: NIL, midi-program-changes: NIL, 
       midi-control-changes: NIL, 
       8va: 0, player: SAX
       asco-label: NIL, asco-msgs: NIL
       set-ref: (1)
       pitch-or-chord: 
PITCH: frequency: 164.814, midi-note: 52, midi-channel: 1 
       pitch-bend: 0.0 
       degree: 104, data-consistent: T, white-note: E3
       nearest-chromatic: E3
       src: 0.62996054, src-ref-pitch: C4, score-note: E3 
       qtr-sharp: NIL, qtr-flat: NIL, qtr-tone: NIL,  
       micro-tone: NIL, 
       sharp: NIL, flat: NIL, natural: T, 
       octave: 3, c5ths: 0, no-8ve: E, no-8ve-no-acc: E
       show-accidental: T, white-degree: 30, 
       accidental: N, 
       accidental-in-parentheses: NIL, marks: NIL, 
       marks-before: NIL, amplitude: NIL
LINKED-NAMED-OBJECT: previous: NIL, 
                     this: NIL, 
                     next: NIL
NAMED-OBJECT: id: E3, tag: NIL, 
data: E3
**************

       written-pitch-or-chord: 
PITCH: frequency: 277.183, midi-note: 61, midi-channel: 1 
       pitch-bend: 0.0 
       degree: 122, data-consistent: T, white-note: C4
       nearest-chromatic: CS4
       src: 1.0594631, src-ref-pitch: C4, score-note: CS4 
       qtr-sharp: NIL, qtr-flat: NIL, qtr-tone: NIL,  
       micro-tone: NIL, 
       sharp: T, flat: NIL, natural: NIL, 
       octave: 4, c5ths: 2, no-8ve: CS, no-8ve-no-acc: C
       show-accidental: T, white-degree: 35, 
       accidental: S, 
       accidental-in-parentheses: NIL, marks: NIL, 
       marks-before: NIL, amplitude: NIL
LINKED-NAMED-OBJECT: previous: NIL, 
                     this: NIL, 
                     next: NIL
NAMED-OBJECT: id: CS4, tag: NIL, 
data: CS4
**************
RHYTHM: value: 4.000, duration: 1.000, rq: 1, is-rest: NIL, 
        is-whole-bar-rest: NIL, 
        score-rthm: 4.0, undotted-value: 4, num-flags: 0, num-dots: 0, 
        is-tied-to: NIL, is-tied-from: NIL, compound-duration: 1.000, 
        is-grace-note: NIL, needs-new-note: T, beam: NIL, bracket: NIL, 
        rqq-note: NIL, rqq-info: NIL, marks: NIL, marks-in-part: NIL, 
        letter-value: 4, tuplet-scaler: 1, bar-pos: 1, 
        grace-note-duration: 0.05
LINKED-NAMED-OBJECT: previous: NIL, 
                     this: NIL, 
                     next: NIL
NAMED-OBJECT: id: Q, tag: NIL, 
data: Q
**************

1
|#

;;; SYNOPSIS
(defmethod get-nearest-note ((sc slippery-chicken) bar-num event-num player)
;;; ****
  (let (nearest-note)
    (multiple-value-bind (ev-a num-a)   ; find nearest note after
        (get-nearest-note-after sc bar-num event-num player)
      (multiple-value-bind (ev-b num-b) ; find nearest note after
          (get-nearest-note-before sc bar-num event-num player)
        (if (>= num-a (abs num-b))
          (setq nearest-note (list ev-b num-b))
          (setq nearest-note (list ev-a num-a)))
        (if nearest-note
          (values-list nearest-note)
          (error "~%get-nearest-note:: no nearest note for ~a at bar ~a ev ~a"
                 player bar-num event-num))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tue 11 Aug 2020 17:56:59 BST
;;; See get-nearest-note, above. It works the same but only ever goes forwards
;;; in time.
(defmethod get-nearest-note-after ((sc slippery-chicken)
                                   bar-num event-num player)
  (let (nearest-note-after (nea-count 0))
    (loop named up-loop
          for bn from bar-num to (num-bars sc)
          for bar = (get-bar sc bn player)
          with first-e
          do 
             (if (= bn bar-num)
               (setf first-e event-num)
               (setf first-e 1))
             (loop for en from first-e to (num-rhythms bar)
                   for e = (get-nth-event (1- en) bar nil) ; 0 based
                   do
                      (when (and e (not (is-rest e)))
                        (setf nearest-note-after e)
                        (return-from up-loop))
                      (incf nea-count))
            thereis nearest-note-after)
    (if nearest-note-after
        (values nearest-note-after nea-count)
        (error "~%get-nearest-note-after:: no note after for ~a at bar ~a ev ~a"
               player bar-num event-num))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tue 11 Aug 2020 17:56:59 BST
;;; See get-nearest-note, above. It works the same but only ever goes backwards
;;; in time.
(defmethod get-nearest-note-before ((sc slippery-chicken)
                                   bar-num event-num player)
  (let (nearest-note-before (neb-count 0))
    (loop named down-loop
          for bn from bar-num downto 1
          for bar = (get-bar sc bn player)
          with first-e
          do
             (if (= bn bar-num)
               (setf first-e event-num)
               (setf first-e (num-rhythms bar)))
             (loop for en from first-e above 0
                   for e = (get-nth-event (1- en) bar nil) ; 0 based
                   do
                      (when (and e (not (is-rest e)))
                        (setf nearest-note-before e)
                        (return-from down-loop))
                      (decf neb-count))
            thereis nearest-note-before)
    (if nearest-note-before
        (values nearest-note-before neb-count)
        (error "~%get-nearest-note-after:: no note after for ~a at bar ~a ev ~a"
               player bar-num event-num))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF slippery-chicken-edit.lsp
