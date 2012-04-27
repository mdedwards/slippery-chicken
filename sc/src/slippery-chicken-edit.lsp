;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****h* sc/slippery-chicken-edit
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
;;;                   class.  NB only include methods here that the user should
;;;                   access (i.e. no -aux methods) as all of these will be
;;;                   automatically listed and linked on a manual page. Also,
;;;                   in order for these links to work we need
;;;                   ****m* slippery-chicken-edit/replace-tempo-map
;;                    not
;;;                   ****m* slippery-chicken/replace-tempo-map
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    April 7th 2012
;;;
;;; $$ Last modified: 17:12:47 Fri Apr 27 2012 BST
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
;;; SAR Sun Apr 22 09:08:44 BST 2012: Conformedrobodoc entry

;;; ****m* slippery-chicken-edit/replace-tempo-map
;;; FUNCTION
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
;;; SYNOPSIS
(defmethod replace-tempo-map ((sc slippery-chicken) tm)
;;; ****
  (setf (tempo-map sc) tm)
  (update-events-tempo sc)
  (update-slots sc (tempo-map sc))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SAR Thu Apr 19 11:52:43 BST 2012: Conforming robodoc

;;; ****m* slippery-chicken-edit/add-event-to-bar
;;; FUNCTION
;;; Add an event object to a specified bar either at the end of that bar or at
;;; a specified position within that bar.
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
;;;    where the event should be added. If NIL, the new event is place at the
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
;;; SAR Sat Apr 21 14:29:43 BST 2012: Conformed robodoc entry

;;; ****m* slippery-chicken-edit/replace-events
;;; FUNCTION
;;; Replace one or more consecutive existing event objects with new event
;;; objects. All references are 1-based. This method can be applied to only one
;;; bar at a time.
;;;
;;; One or more new event objects can be specified as a replacement for one
;;; single original event object.
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
;;; SAR Sat Apr 21 17:17:33 BST 2012: Conformed robodoc entry

;;; ****m* slippery-chicken-edit/replace-multi-bar-events
;;; FUNCTION
;;; Replace specified consecutive event objects across several bars. 
;;;
;;; The new rhythms provided must produce full bars for all bars specified;
;;; i.e., if only a quarter note is provided as the new event for a 2/4 bar,
;;; the method will not automatically fill up the remainder of the bar.
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
;;;   being just the sequence of pitches, e.g: '((q e ) (c4 d4)). For the
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
                                     ;; MDE Mon Apr 23 12:36:08 2012 -- changed
                                     ;; default to nil
                                     (consolidate-rests nil)
                                     ;; for consolidate rests
                                     (beat nil)
                                     ;; MDE Mon Apr 23 12:36:08 2012 -- changed
                                     ;; default to nil
                                     (auto-beam nil)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Wed Apr 25 13:09:27 BST 2012: Added robodoc entry

;;; ****m* slippery-chicken-edit/auto-accidentals
;;; FUNCTION
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
;;; SAR Sun Apr 22 10:03:14 BST 2012: Added robodoc entry.

;;; ****m* slippery-chicken-edit/respell-notes
;;; FUNCTION

;;; Pass through the entire given slippery-chicken object and change some of
;;; the pitch objects to their enharmonic equivalents to produce more sensible
;;; spellings of consecutive pitches in the score.
;;;
;;; An optional argument takes a list specifying which pitches to change in the
;;; same format found in the method enharmonic-spellings; i.e.
;;; '((player (bar note-num))). If this approach is chosen, the method will
;;; only change the specified pitches.
;;;
;;; NB: If a list of corrections is specified, the :respell-notes argument of
;;;     any subsequent call to cmn-display or write-lp-data-for-all must be set
;;;     NIL, otherwise the modified pitches may be overwritten.
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

;;; SAR Tue Apr 24 19:24:12 BST 2012: Added robodoc entry

;;; MDE Wed Apr 18 11:57:11 2012 -- added pitches keyword

;;; ****m* slippery-chicken-edit/enharmonics
;;; FUNCTION
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
;;;   note number is 1-based and counts ties.
;;; - An integer or a 2-item list of integers that indicates the last bar in
;;;   which the enharmonics are to be changed. If an integer, the method will
;;;   be applied to all sharp/flat pitches in the bar of that number. If a
;;;   2-item list of integers, these represent '(bar-number note-number). The
;;;   note number is 1-based and counts ties.
;;; - The ID of the player whose part is to be changed.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments
;;; - :written. T or NIL to indicate whether to change written-only pitches or
;;;   sounding-only pitches. T = written-only. Default = T.
;;; - :pitches. NIL or a list of note-name symbols. If NIL, all sharp/flat
;;;   pitches in the specified region will be changed to their enharmonic
;;;   equivalents. If a list of one or more note-name symbols, only those
;;;   pitches will be affected.
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
  (enharmonics mini 3 4 'cl :written nil))

=> T


|#
;;; SYNOPSIS
(defmethod enharmonics ((sc slippery-chicken) start end player
                        &key (written t) pitches)
;;; ****
  (setf pitches (init-pitch-list pitches))
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
                    (when (and transp written (not (written-pitch-or-chord e)))
                      (warn "~a~%slippery-chicken-edit::enharmonics: ~
                             no written-pitch-or-chord (bar ~a, ~a)." 
                            e bar-num player))
                    (loop for p in (data (if (and written transp
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
                                     :chord-note-ref chord-note-ref))))
                  ;; MDE Wed Apr 18 12:08:51 2012 
                  (when (and (event-p e)
                             (is-single-pitch e)
                             (or (not pitches)
                                 (pitch-member (if (and transp written)
                                                   (written-pitch-or-chord e)
                                                   (pitch-or-chord e))
                                               ;; enharmonics not equal!
                                               pitches nil)))
                    (enharmonic e :written (and transp written)))))))
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

;;; SAR Fri Apr 20 15:45:41 BST 2012: Added robodoc entry

;;; SAR: some of MDE's original comment taken directly into robodoc
;;; SAR: Fri Apr 20 16:18:17 BST 2012: I changed "from high to low" to "from
;;; low to high", since that's the way it appears to work.

;;; where (596 (1 2)) is accessing the second chord note (counting from high to
;;; low) of the first sounding event of bar 596

;;; SAR: Fri Apr 20 16:20:35 BST 2012: I don't see this in cmn-display:
;;; NB Designed to be called from cmn-display but can be called by user.

;;; ****m* slippery-chicken-edit/enharmonic-spellings
;;; FUNCTION
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
                               ;; istead of a single pitch. 
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
;;; FUNCTION
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

=> NIL

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
            (setf last-attack (get-last-attack bar nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sun Apr 22 10:57:57 BST 2012: Added robodoc entry

;;; SAR Sun Apr 22 10:58:56 BST 2012: Moved MDE's comment into the entry.

;;; MDE 8/4/07: keep track of the last two now in order to make better decisions.

;;; ****m* slippery-chicken-edit/respell-notes-for-player
;;; FUNCTION
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
                    ;;  
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
;;; FUNCTION
;;; Change the pitch of a specified event to a new specified pitch. The new
;;; pitch is not required to be a member of the current set.
;;;
;;; NB The new pitch is the sounding pitch if a transposing instrument.
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; - An integer that is the bar number in which the pitch is to be changed.
;;; - An integer that is the number of the note in the specified bar whose
;;;   pitch is to be changed.
;;; - The ID of the player for whom the pitch is to be changed.
;;; - A note-name symbol that is the new pitch.
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
                         new-pitch)
;;; ****
  (change-pitch (piece sc) bar-num note-num player new-pitch))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sun Apr 22 11:56:20 BST 2012: Added robodoc entry

;;; ****m* slippery-chicken-edit/rest-to-note
;;; FUNCTION
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

;;; SAR Wed Apr 25 13:33:53 BST 2012: Conformed robodoc entry

;;; ****m* slippery-chicken-edit/change-pitches
;;; FUNCTION
;;; Change the pitches of the specified event objects for a given player to the
;;; specified new pitches. 
;;;
;;; If the new pitches are passed as a simple flat list, the method will just
;;; change the pitch of each consecutive event object (with NIL indicating no
;;; change), moving from bar to bar as necessary, until all of the specified
;;; new pitches are used up. Also, if a flat list is passed, each new pitch
;;; specified will be applied to each consecutive attacked note; i.e., ties
;;; don't count as new pitches.
;;;
;;; Also see the documentation in the bar-holder class for the method of the
;;; same name. 
;;;
;;; ARGUMENTS 
;;; - A slippery-chicken object.
;;; - The ID of the player whose part is to be modified.
;;; - An integer that is the number of the first bar whose pitches are to be
;;;   modified. 

;;; - A list note-name symbols and NILs, or a list of lists of note-name
;;;   symbols and NILs, which are the new pitches. If a simple flat list, see
;;;   the comment in the function description above. If a list of lists, each
;;;   sub-list will represent a full bar; e.g., (change-pitches bh 'vla 5 '((g3
;;;   gs4) nil (nil nil aqf5))) will change the pitches in bars 5 and 7 (for
;;;   the player 'vla), whereas bar six, indicated by nil, wouldn't be changed;
;;;   similarly the first two notes of bar 7, being nil, will also not be
;;;   changed, but note 3 will.
;;;
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to indicate whether or not each consecutive new pitch listed
;;;   will automatically take the most recent octave number specified; 
;;;   e.g. '((a3 b g cs4)). T = use last octave number. Default = T.

;;; - A list of marks to be added to the events objects. This option can only
;;;   be used in conjunction with the simple flat list of pitches. In this case
;;;   the list of pitches and list of marks must be the same length and
;;;   correspond to each other item by item. Sub-lists can be used to add
;;;   several marks to a single event. NB: See cmn.lsp::get-cmn-marks for the
;;;   list of recognised marks. If NIL, no marks will be added. Default = NIL.
;;; 
;;; RETURN VALUE  
;;; If a the new pitches are passed as a simple flat list, the method returns
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
                           &optional (use-last-octave t) marks)
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
           do
           (setf e (next-event sc player t))
           (unless (event-p e)
             (error "slippery-chicken::change-pitches: couldn't get event ~a!"
                    (1+ count)))
           (when note
             (when use-last-octave
               (multiple-value-bind
                     (n o)
                   (get-note-octave note t)
                 (setf note (join-note-octave n o))))
             (setf (pitch-or-chord e) note))
           ;; NB note might be nil but mark not hence this isn't in the when
           (rhythm-add-marks e (nth count marks)))
        ;; this hack gets the current bar number so we return where we left off
        (next-event sc nil))
      ;; the bar-holder method
      (change-pitches (piece sc) player start-bar new-pitches use-last-octave)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Fri Apr 20 11:13:46 BST 2012: Added robodoc entry
;;; SAR Fri Apr 20 11:22:21 BST 2012: Deleted MDE comments here as they have
;;; been taken into the robodoc in full.

;;; ****m* slippery-chicken-edit/change-time-sig
;;; FUNCTION
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

;;; SAR Thu Apr 19 13:35:00 BST 2012: Added robodoc entry

;;; ****m* slippery-chicken-edit/add-mark-to-note
;;; FUNCTION
;;; Add the specified mark to the specified note of a given slippery-chicken
;;; object. 
;;;
;;; NB: This method counts notes, not events; i.e., not rests.
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; - An integer that is the bar number to which to add the mark
;;; - An integer that is the note number two which to add the mark. This is
;;;   1-based, and counts notes not events; i.e., not rests.
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
;;; FUNCTION
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
;;; FUNCTION
;;; Add a specified mark to a specified even in the parts of all players. The
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
;;; FUNCTION
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

;;; - :dy1.A positive or negative decimal number to indicate the vertical
;;;   offset of the right corner of the bracket.

;;; - :index. For internal use only.

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
;;; meaning bar 1 note 1, bar 2 note 2, here you give it in the form 
;;; '((1 1 5) (3 2 7)) meaning bar 1, notes 1 to 5 inclusive, bar 3, notes 2 to
;;; 7 inclusive. 

;;; ****m* slippery-chicken-edit/add-mark-to-notes-from-to
;;; FUNCTION
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
(defmethod add-mark-to-notes-from-to ((sc slippery-chicken)
                                      mark-function player
                                      notes)
;;; ****
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

;;; SAR: I haven't incorporated this MDE comment yet.
;;; NB noteheads need before to be t in lilypond but bear in mind they're
;;; automatically moved over in event::get-lp-data.  players can be a single
;;; symbol or list.

;;; ****m* slippery-chicken-edit/add-marks-to-notes
;;; FUNCTION
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
;;; FUNCTION
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

;;; SAR Thu Apr 19 16:41:09 BST 2012: Added robodoc entry

;;; ****m* slippery-chicken-edit/add-marks-to-note
;;; FUNCTION
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
;;; FUNCTION
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
;;; FUNCTION
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
;;; FUNCTION
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
;;; FUNCTION
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
;;; FUNCTION
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
;;; FUNCTION
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
;;; FUNCTION
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

;;; ****m* slippery-chicken-edit/tie
;;; FUNCTION

;;; Add a tie to a specified event object. The new tie will be placed starting
;;; from the specified event object and spanning to the next event object. If
;;; the next event object does not have the same pitch, its pitch will be
;;; changed to that of the first event object.
;;;
;;; An optional argument allows the user to adjust the steepness of the tie's
;;; curvature. 
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
;;; FUNCTION
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

;;; ****m* slippery-chicken-edit/tie-all-last-notes-over-rests
;;; FUNCTION
;;; Extend the duration of the last note of any bar that precedes a bar which
;;; starts with a rest in the specified region, such that the rest that begins
;;; the next measure is changed to a note and the last note of the first
;;; measure is tied to it.
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

;;; - :last-rhythm. Default = NIL.

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
        :set-map '((1 (1 1)))
        :rthm-seq-palette '((1 ((((4 4) e (e) e e (e) (e) e e) 
				 ((w)) 
				 ((h.) q) 
				 ((w))
				 ((w)) 
				 ((e) e h.))
				:pitch-seq-palette ((1 2 3 4 5 6 7 7)))))
        :rthm-seq-map '((1 ((vn (1 1))
			    (va (1 1))
			    (vc (1 1))))))))
  (tie-all-last-notes-over-rests mini 2 6 'vn)
  (tie-all-last-notes-over-rests mini 9 12 'vn :auto-beam t)
  (tie-all-last-notes-over-rests mini 3 5 '(va vc) :to-next-attack nil)
  (tie-all-last-notes-over-rests mini 9 12 'vc :tie-next-attack t))

=> NIL

|#
;;; SYNOPSIS
(defmethod tie-all-last-notes-over-rests ((sc slippery-chicken)
                              start-bar end-bar players
                              &key
                              ;; use up all rests until next attack or (if nil)
                              ;; just the rest bars?
                              (to-next-attack t)
                              ;; if the next attack is the same note/chord as
                              ;; the previous, tie to it too?
                              (tie-next-attack nil)
                              (last-rhythm nil)
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
                               :auto-beam auto-beam)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Fri Apr 27 11:46:14 BST 2012: Added robodoc entry

;;; ****m* slippery-chicken-edit/tie-over-rest-bars
;;; FUNCTION
;;; Extend the duration of the last note in a specified bar by changing
;;; immediately subsequent full-rest bars to notes of the same pitch and tying
;;; them to that note.
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

;;; - :last-rhythm. Default = NIL.
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
  		      :auto-beam t))

=> NIL

|#
;;; SYNOPSIS
(defmethod tie-over-rest-bars ((sc slippery-chicken) bar-num players
                               &key (end-bar nil) ;; num of empty bars
                                    (tie-next-attack nil)
                                    (to-next-attack t)
                                    (last-rhythm nil)
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
                                :last-rhythm last-rhythm)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Thu Apr 26 13:18:26 BST 2012: Added robodoc entry

;;; ****m* slippery-chicken-edit/tie-over-all-rests
;;; FUNCTION
;;; Extend the durations of all notes that immediately precede rests in the
;;; specified region by changing the rests to notes and tying the previous notes
;;; to them.
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; - The ID of the player whose part is to be changed.
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
        :set-palette '((1 ((c4 d4 e4))))
        :set-map '((1 (1 1 1 1 1 1 1)))
        :rthm-seq-palette '((1 ((((2 4) (q) e (s) s))
                                :pitch-seq-palette ((1 2)))))
        :rthm-seq-map '((1 ((vn (1 1 1 1 1 1 1))))))))
  (tie-over-all-rests mini 'vn 2 3 :start-note 2 :auto-beam t)
  (tie-over-all-rests mini 'vn 5 6 :end-note 1 :consolidate-notes t))

=> NIL

|#
;;; SYNOPSIS
(defmethod tie-over-all-rests ((sc slippery-chicken) player
                               start-bar end-bar 
                               &key 
                               (start-note 1)
                               (end-note 9999999)
                               (auto-beam nil)
                               (consolidate-notes nil))
;;; ****
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
         (setf note-num 0
               event-num 0)
         (loop 
            while (< event-num (num-rhythms bar))
            for event = (get-nth-event event-num bar)
            do
            ;; (format t "~&~a ~a" bnum note-num)
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
    ;; always do this starting with the highest bar num otherwise we the refs
    ;; get screwed up as we add notes
    ;; (print refs)
    (loop for ref in refs do
         (tie-over-rests sc (first ref) (second ref) player 
                         :auto-beam auto-beam
                         :consolidate-notes consolidate-notes))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Fri Apr 27 11:23:11 BST 2012: Added robodoc entry

;;; MDE: 24.3.11: added end-bar

;;; ****m* slippery-chicken-edit/tie-over-rests
;;; FUNCTION
;;; Extend the duration of a specified note that precedes a rest by changing
;;; the rest to a note with the same pitch and adding a tie between them.
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; - An integer that is the number of the bar in which the note is located. 
;;; - An integer that is the number of the note within that bar which is to be
;;;   extended. This number is 1-based and also counts already tied notes.
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

=> NIL

|#
;;; SYNOPSIS
(defmethod tie-over-rests ((sc slippery-chicken) bar-num note-num player
                           &key end-bar auto-beam (consolidate-notes t))
;;; ****
  (next-event sc player nil bar-num)
  (unless (get-note sc bar-num note-num player)
    (error "tie-over-rests: can't get note ~a, bar ~a, ~a"
           note-num bar-num player))
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
    ;; (print start-event)
    ;; just check we're not already tied here and exit if we are because we get
    ;; some strange errors when we enter the wrong data when calling this fun
    (when (is-tied-from start-event)
      (warn "slippery-chicken::tie-over-rests: already tied from! Bar ~a, ~
              note ~a, ~a" bar-num note-num player))
    (setf (is-tied-from start-event) t)
    ;; remove any staccato or tenuto marks from this event
    (rm-marks start-event '(s as t) nil)
    (when porc
      (delete-marks porc))
    (when wporc
      (delete-marks wporc))
    (loop 
       for e = (next-event sc player)
       for bnum = (next-event sc nil)
       for bar = (get-bar sc bnum player)
       while (and (if end-bar (<= bnum end-bar) t)
                  e (is-rest e))
       do
       ;; (print (data e))
       ;; keep track of the bars we've changed
       (pushnew bnum bar-nums)
       (if (is-rest-bar bar)
           (let ((events (events-for-full-bar (get-time-sig bar) 
                                              porc wporc)))
             ;;(print (data porc))
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
    ;; 3.3.11 if we just test for last-event we might screw up ties despite the
    ;; fact that we've done nothing. 
    (when (zerop new-ties)
      (error "slippery-chicken::tie-over-rests: no ties to make! Bar ~a, ~
              note ~a, ~a" bar-num note-num player))      
    (when (> new-ties 0)
      (setf (is-tied-from last-event) nil))
    (when (or auto-beam consolidate-notes)
      (loop 
         for bnum in bar-nums 
         for bar = (get-bar sc bnum player)
         do
         (when consolidate-notes
           (consolidate-notes bar nil auto-beam))
         (when auto-beam
           (auto-beam bar auto-beam nil))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Fri Apr 20 13:42:14 BST 2012: Added robodoc entry

;;; ****m* slippery-chicken-edit/delete-slur
;;; FUNCTION
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

;;; SAR Fri Apr 20 10:38:49 BST 2012: Added robodoc entry

;;; add slurs automatically (to wind instruments usually) to phrases: these are
;;; defined as not having any rests in them and not including any repeated
;;; notes. 

;;; ****m* slippery-chicken-edit/auto-slur
;;; FUNCTION
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
(defmethod auto-slur ((sc slippery-chicken) players
                      &key start-bar end-bar
                      rm-slurs-first
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
                        (and (> count 2)
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
              ;; got start of phrase but second note is same as first
              ((and start-e 
                    (= count 1)
                    (needs-new-note e)
                    (porc-equal start-e e))
               (setf start-e e))
              ((is-rest e)
               (setf start-e nil
                     count 0))
              ((not (is-tied-to e))
               (incf count))
              ((and rm-staccatos start-e (not last-e))
               ;; in the middle of a slur so remove staccatos
               (replace-mark e 'as 'a)
               (rm-marks e 's nil)))
        (setf last-e e)))
  ;; 9.4.11
  (check-slurs sc)
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Fri Apr 20 11:52:59 BST 2012: Added robodoc entry

;;; ****m* slippery-chicken-edit/delete-clefs
;;; FUNCTION
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
;;; FUNCTION
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
;;; FUNCTION
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
;;; FUNCTION
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
;;; Returns NIL
;;; 
;;; EXAMPLE
#|
;;; Straightforward usage applied to just the VC player
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

=> NIL

|#
;;; SYNOPSIS
(defmethod auto-clefs ((sc slippery-chicken) 
                       &key verbose in-c players 
                       (delete-clefs t)
                       (delete-marks-before nil))
;;; ****
  ;; MDE Fri Apr 20 14:27:07 2012 -- 
  (unless players
    (setf players (players sc)))
  ;; MDE Fri Apr 20 14:25:54 2012 
  (unless (listp players)
    (setf players (list players)))
  (loop 
     for player in players
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
                                   ;; last-clefs, rather last-events twice...
                                   last-events last-clefs note-count
                                   current-clef verbose in-c))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Wed Apr 25 18:36:03 BST 2012: Added robodoc entry

;;; MDE original comment:
;;; This is different from set-rehearsal-letters in that we don't use the
;;; rehearsal-letters slot of sc, rather, we use the method argument.

;;; ****m* slippery-chicken-edit/set-rehearsal-letter
;;; FUNCTION
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
;;; FUNCTION
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
  (loop 
      for p in players 
               ;; remember the letter is actually placed on the bar-line of the
               ;; previous bar
      for bar = (get-bar sc (1- bar-num) p)
      do 
        (setf (rehearsal-letter bar) nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat Apr 21 13:47:21 BST 2012: Added robodoc entry.

;;; ****m* slippery-chicken-edit/re-bar
;;; FUNCTION
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
;;; NB: This method should not be confused with the rebar method.
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
;;;   signature from which the method may occasionally if the number of events
;;;   does not fit evenly into full bars of the specified time signature.
;;; - :verbose. T or NIL to indicate whether to print feedback on the
;;;   re-barring process to the Listener. T = print feedback. Default = NIL.
;;; - :check-ties. T or NIL to indicate whether to force the method to ensure
;;;   that all tied notes have the same enharmonic spellings. T = check. 
;;;   Default = T.
;;; - :auto-beam. T, NIL, or an integer. If T, the method will automatically
;;;   attach beam indications to the corresponding events according to the beat
;;;   unit of the time signature. If an integer, the method will beam in
;;;   accordance with a beat unit that is equal to that integer. If NIL, the
;;;   method will not automatically place beams. Default = T.
;;; - :update-slots. T or NIL to indicate whether to update the corresponding
;;;   slots. This is an internal argument and not needed by the user. 
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
;;; FUNCTION
;;; Automatically sets indications for starting beams and ending beams (1 and
;;; 0) to the BEAMS slot of the corresponding event objects. 
;;;
;;; By default, the method places the start and end indications for beams on
;;; the basis of the beat found in the time signature, but the user can specify
;;; a different beat basis using the first optional argument.
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
;;; 
;;; RETURN VALUE
;;; Returns NIL.
;;; 
;;; EXAMPLE
#|
;; Auto-beam the events of the given slippery-chicken object on the basis of a
;; half note:
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
(defmethod auto-beam ((sc slippery-chicken) &optional (beat nil) (check-dur t))
;;; ****
  (loop for player in (players sc) do
        (loop 
            for bnum from 1 to (num-bars sc) 
            for bar = (get-bar sc bnum player)
            do
              (auto-beam bar beat check-dur))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; sort notes in piece--across all instruments--into time-ordered lists and
;;; process them with the given function, which must take one argument, an
;;; event.
;;; ****m* slippery-chicken-edit/process-events-by-time
;;; FUNCTION
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
(defmethod process-events-by-time ((sc slippery-chicken) function
                                   &key (start-bar 1) end-bar)
;;; ****
  (unless end-bar
    (setf end-bar (num-bars sc)))
  (loop for bar-num from start-bar to end-bar 
     for bars = (get-bar sc bar-num)    ; gets for all players
     for events =
     ;; this will collect rests too of course so need to filter them out in
     ;; the supplied function if that's what's needed
     (loop for player-bar in bars appending (rhythms player-bar))
     do
     (setf events (sort events
                        #'(lambda (e1 e2)
                            (< (start-time e1) (start-time e2)))))
     (loop for e in events do
          (funcall function e)))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Wed Apr 25 16:30:06 BST 2012: Added robodoc entry

;;; ****m* slippery-chicken-edit/sc-move-dynamic
;;; FUNCTION
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Wed Apr 25 16:41:35 BST 2012: Added robodoc entry.

;;; ****m* slippery-chicken-edit/sc-remove-dynamic
;;; FUNCTION
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
;;;  A post-generation editing method

;;; SAR Wed Apr 25 17:03:04 BST 2012: Added robodoc entry

;;; ****m* slippery-chicken-edit/sc-remove-dynamics
;;; DATE 
;;; 16-Mar-2011
;;;
;;; FUNCTION
;;; Remove all dynamic marks from the MARKS slots of all consecutive event
;;; objects within a specified region of bars. 
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
;;;   are to be removed.
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
(defmethod sc-remove-dynamics ((sc slippery-chicken) start end players)
;;; ****
  (unless (listp players)
    (setf players (list players)))
  (let* ((stlist (listp start))
         (ndlist (listp end))
         (stbar (if stlist (first start) start))
         (ndbar (if ndlist (first end) end))
         (stnote (if stlist (second start) 1))
         ;; NB this means we'll add marks to tied notes too
         (ndnote (when ndlist (second end)))) ; nil processed in do-bar
    (flet ((do-bar (bar-num start-note end-note player)
             (unless end-note
               (setf end-note (num-score-notes (get-bar sc bar-num player))))
             (loop with bar = (get-bar sc bar-num player)
                for i from (1- start-note) below end-note 
                for e = (get-nth-non-rest-rhythm i bar)
                do
                  (remove-dynamics e))))
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
;;;  A post-generation editing method

;;; SAR Sat Apr 21 14:20:22 BST 2012: Added robodoc entry

;;; ****m* slippery-chicken-edit/remove-extraneous-dynamics
;;; FUNCTION
;;; If two or more consecutive event objects have the same dynamic, remove that
;;; dynamic marking from all but the first of these.
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
       (loop with last-dynamic with rest-bars = 0
          for bar-num from 1 to (num-bars sc) 
          for bar = (get-bar sc bar-num player)
          do
          (loop for event in (rhythms bar) 
             for this-dynamic = (get-dynamic event)
             do
             (if (and (eq this-dynamic last-dynamic)
                      ;; 5.4.11 do repeat the dynamic if we've had several
                      ;; rest bars 
                      (< rest-bars 2))
                 (remove-dynamics event)
                 (when this-dynamic
                   (setf last-dynamic this-dynamic)))
             (setf rest-bars 0))
          (when (is-rest-bar bar)
            ;; (print bar-num)
            (incf rest-bars))))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  A post-generation editing method

;;; SAR Fri Apr 27 13:17:14 BST 2012: Added robodoc entry

;;; start/end-note are 1-based but count ties.  if no optional args, deletes
;;; all beams in the bar.

;;; ****m* slippery-chicken-edit/sc-delete-beams
;;; FUNCTION
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
;;; FUNCTION
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
  (update-write-time-sig2 (piece sc) t)
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
;;;  A post-generation editing method

;;; ****m* slippery-chicken-edit/move-events
;;; DATE
;;; 20-Jul-2011 (Pula)
;;;
;;; FUNCTION
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
;;;  post-gen-editing method

;;; SAR Fri Apr 20 13:58:23 BST 2012: Added robodoc entry

;;; MDE comment:
;;; if update we update-slots for the whole sc object a nasty side-effect at
;;; the moment is that any existing events in the doubling players at the
;;; beginning of the start-bar or end of the end-bar will be deleted, so this
;;; only works for copying notes into completely empty bars, not razor
;;; splicing.

;;; ****m* slippery-chicken-edit/double-events
;;; DATE
;;; 20-Jul-2011 (Pula)
;;;
;;; FUNCTION
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
;;;   ties. If NIL, all event from the given bar will be copied.
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
                          &key transposition (consolidate-rests t) (update t))
;;; ****
  (setf doubling-players (force-list doubling-players))
  (loop for doubling-player in doubling-players do       
     ;; clone the master players bars
       (let* ((player-obj (get-data doubling-player (ensemble sc)))
              (mc (midi-channel player-obj))
              (mmc (microtones-midi-channel player-obj))
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
         (loop for bn from start-bar to end-bar 
            for mb in mbars
            for ins-transposition =
            (get-transposition-at-bar doubling-player bn sc)
            do 
            (if (zerop ins-transposition)
                (delete-written mb)     ; just in case master is transposing...
                (set-written mb (- ins-transposition))))
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
           (auto-beam sb)
           (unless (= start-bar end-bar)
             (auto-beam eb))
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
            (setf (rhythms db) (rhythms mb)))
         (when update                   ; could be called elsewhere
           (update-slots sc))))
  t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  A post-generation editing methdo

;;; SAR Fri Apr 20 12:59:28 BST 2012: Added robodoc entry
;;; SAR Fri Apr 20 12:59:41 BST 2012: Removed MDE's original comment because
;;; taken nearly verbatim into robodoc

;;; ****m* slippery-chicken-edit/delete-events
;;; DATE
;;; 21-Jul-2011 (Pula)
;;;
;;; FUNCTION
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
            (auto-put-tuplet-bracket-on-beats bar nil))
          ;; 26.9.11: this was before consolidate-rests
          (auto-beam bar)))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  A post-generation editing method

;;; SAR Wed Apr 25 16:05:55 BST 2012: Added robodoc entry.

;;; ****m* slippery-chicken-edit/sc-force-rest
;;; DATE
;;; 23-Jul-2011 (Pula)
;;;
;;; FUNCTION
;;; Change the specified event object to a rest.
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
                          &optional (auto-beam nil))
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
;;;  A post-generation editing method

;;; SAR Fri Apr 20 16:45:57 BST 2012: Added robodoc entry

;;; ****m* slippery-chicken-edit/force-rest-bars
;;; FUNCTION
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
;;;   rest. 
;;; - A list containing the IDs of the players in whose parts the full-bar
;;;   rests are to be forced.
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
(defmethod force-rest-bars ((sc slippery-chicken) start-bar end-bar players)
;;; ****
  (unless players
    (setf players (players sc)))
  (unless (listp players)
    (setf players (list players)))
  (loop for bar-num from start-bar to end-bar do
       (loop for player in players 
            for bar = (get-bar sc bar-num player)
            do
            (unless bar
              (error "slippery-chicken::force-rest-bars: no bar ~a for ~a"
                     bar-num player))
            (force-rest-bar bar))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SAR Tue Apr 24 19:46:02 BST 2012: Conformed robodoc entry

;;; ****m* slippery-chicken-edit/force-artificial-harmonics
;;; FUNCTION
;;; For string scoring purposes only: Transpose the pitch of the given event
;;; object down two octaves and add the harmonic symbol at the perfect fourth.
;;;
;;; If this results in a fingered pitch (or even a touched perfect fourth) that
;;; is out of the range of the instrument, a warning will be printed to the
;;; Listener, the pitch will not be transposed, and the harmonic diamond will
;;; not be added.
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
                                       start-event end-bar &optional end-event)
;;; ****
  ;; MDE Mon Apr 23 09:10:09 2012 -- assumes we don't change player in the
  ;; midst of making these changes.  Uses instrument to ensure we don't go out
  ;; of range.  
  (let ((ins (get-instrument-for-player-at-bar player start-bar sc)))
    (loop for e in (get-events-from-to sc player start-bar start-event end-bar
                                       end-event)
       do
       (unless (is-rest e)
         (force-artificial-harmonic e ins)))
    t))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  A post-generation editing method

;;; SAR Fri Apr 27 12:41:30 BST 2012: Added robodoc entry

;;; ****m* slippery-chicken-edit/set-cautionary-accidental
;;; DATE
;;; 28-Sep-2011
;;;
;;; FUNCTION
;;; Place a cautionary accidental (sharp/flat/natural sign in parentheses)
;;; before a specified note. 
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
;;;  A post-generation editing method

;;; SAR Thu Apr 26 15:05:24 BST 2012: Added robodoc entry

;;; ****m* slippery-chicken-edit/unset-cautionary-accidental
;;; FUNCTION

;;; Remove the parentheses from a cautionary accidental (leaving the accidental
;;; itself) by setting the ACCIDENTAL-IN-PARENTHESES slot of the contained
;;; pitch object to NIL.
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

;;; SAR Thu Apr 19 11:14:57 BST 2012: Conforming MDE robodoc entry

;;; ****m* slippery-chicken-edit/add-arrow-to-events
;;; DATE
;;; April 9th 2012
;;; 
;;; FUNCTION
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
;;; FUNCTION
;;; Change single to double or repeat bar lines and vice-versa.  NB This is a
;;; score function only, i.e., if you add repeat bar lines these will not (yet) 
;;; be reflected in playback with MIDI or CLM.
;;; 
;;; ARGUMENTS 
;;; - the slippery-chicken object
;;; - the bar number at the end of which you want the bar line to change
;;; - bar line type: 0 = normal, 1 = double bar, 2 = final double bar, 3 =
;;;   begin repeat, 4 = begin and end repeat, 5 = end repeat 
;;; RETURN VALUE  
;;; always T
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
  ;; this piece only has one bar so the barline will be 2 by default
  (print (bar-line-type (get-bar min 1 'fl)))
  (change-bar-line-type min 1 1)
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
  (let ((players-bars (get-bar sc bar-num)))
    (loop for bar in players-bars do
      (setf (bar-line-type bar) type)))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; EOF slippery-chicken-edit.lsp
