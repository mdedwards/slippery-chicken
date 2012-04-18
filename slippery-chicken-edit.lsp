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
;;; $$ Last modified: 07:57:41 Wed Apr 18 2012 BST
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
;;; ****m* slippery-chicken-edit/replace-tempo-map
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
;;; ****m* slippery-chicken-edit/add-event-to-bar
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
;;; ****m* slippery-chicken-edit/replace-events
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
;;; ****m* slippery-chicken-edit/replace-multi-bar-events
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
                      

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  A post-generation editing method

;;; ****m* slippery-chicken-edit/auto-accidentals
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
(defmethod auto-accidentals ((sc slippery-chicken) &optional ignore1 ignore2)
;;; ****
  (declare (ignore ignore1 ignore2))
  (loop 
      with players = (players sc)
      with bar
      with last-attack
      with last-notes = (ml nil (length players))
      for bar-num from 1 to (num-bars (piece sc)) 
      do
        (loop 
            for player in players 
            for i from 0
            do 
              (setf bar (get-bar sc bar-num player))
              (auto-accidentals bar (nth i last-notes))
              ;; we can't ignore instruments that only sound octaves +/-
              ;; written note as that would leave written and sounding notes
              ;; potentially different, hence nil last argument here.
              (when (plays-transposing-instrument (get-player sc player) nil)
                (auto-accidentals bar (nth i last-notes) t))
              (setf last-attack (get-last-attack bar nil))
              (when last-attack
                (setf (nth i last-notes) last-attack)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  A post-generation editing method

;;; ****m* slippery-chicken-edit/respell-notes
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
(defmethod respell-notes ((sc slippery-chicken) &optional corrections)
;;; ****
  (format t "~&Respelling notes...")
  ;; this respells written and sounding notes if transposing instrument
  (respell-notes-aux sc)
  ;; 10/5/07: a second pass does pick up some more mistakes...
  (respell-notes-aux sc (when (listp corrections) corrections)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  A post-generation editing method

;;; 11.4.11: start and end can be bar numbers or (bar note) pairs where note is
;;; 1-based and counts ties.
;;; ****m* slippery-chicken-edit/enharmonics
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
(defmethod enharmonics ((sc slippery-chicken) start end player
                        &optional (written t))
;;; ****
  (let* ((stlist (listp start))
         (ndlist (listp end))
         (stbar (if stlist (first start) start))
         (ndbar (if ndlist (first end) end))
         (stnote (if stlist (second start) 1))
         ;; NB this means we'll add marks to tied notes too
         (ndnote (when ndlist (second end)))) ; nil processed in do-bar
    (flet ((do-bar (bar-num start-note end-note)
             (let ((bar (get-bar sc bar-num player)))
               (unless end-note
                 (setf end-note (num-score-notes bar)))
               (loop for i from start-note to end-note 
                  for e = (get-nth-non-rest-rhythm (1- i) bar)
                  do
                  (enharmonic e :written written)))))
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
;;;  A post-generation editing method

;;; e.g. (enharmonic-spellings +coming-rthm-chain+
;;;                    '((cello (117 1) (118 2) (135 3) (591 (1 2)) (596 (2 2)))
;;;                      (violin (539 5))
;;;                      (clarinet (1 2 t))
;;;                      (flute (204 1))))
;;; where (596 (1 2)) is accessing the second chord note (counting from high to
;;;                    low) of the first sounding event of bar 596
;;; (clarinet (1 2 t)): the t means change the written note, not sounding
;;; NB Designed to be called from cmn-display but can be called by user.

;;; ****m* slippery-chicken-edit/enharmonic-spellings
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
;;;  A post-generation editing method

;;; This does things by looking at enharmonic spellings in a whole bar
;;; ****m* slippery-chicken-edit/respell-bars
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
;;;  A post-generation editing method

;;; This is just a very simple attempt to spell notes better by comparing each
;;; note to the previous one and making it the same accidental type.  It
;;; doesn't look further back or ahead as of yet.  If <written> then look at
;;; the written notes instead of the sounding notes.
;;;
;;; 8/4/07: keep track of the last two now in order to make better decisions.

;;; ****m* slippery-chicken-edit/respell-notes-for-player
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
                       (= (bar-num last) (bar-num last-but-one))
                       (bad-interval-p last last-but-one))
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
;;;  A post-generation editing method

;;; NB The new pitch is the sounding pitch if a transposing instrument.
;;; ****m* slippery-chicken-edit/change-pitch
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
(defmethod change-pitch ((sc slippery-chicken) bar-num note-num player
                         new-pitch)
;;; ****
  (change-pitch (piece sc) bar-num note-num player new-pitch))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  A post-generation editing method

;;; 30.3.11: turn a rest into a note by supplying a pitch or chord (as objects
;;; or symbols)

;;; ****m* slippery-chicken-edit/rest-to-note
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
;;;  A post-generation editing method

;;; ****m* slippery-chicken-edit/change-notes
;;; FUNCTION
;;; change-notes:
;;;
;;; ARGUMENTS 
;;; same as bar-holder class but with one extra
;;; - (optional default nil): a list of marks to be added to the notes: only
;;; when using the simple flat list; in this case the notes and marks must be
;;; the same length and correspond to each other item by item.  Sublists can be
;;; used to add several marks to a single event.  NB marks are list symbols
;;; like 'x-head--see cmn.lsp::get-cmn-marks for those recognised.
;;; 
;;; Change the piece's notes for a given player.  See the documentation in the
;;; bar-holder class method but note that if new-notes is a simple flat list,
;;; then we'll just change one note after another (with nil indicating no
;;; change), moving from bar to bar as necessary until the new-notes are used
;;; up; in contrast to the bar-holder method, if a flat list is passed then we
;;; only give a note for each attack i.e. ties don't count as new notes.
;;; 
;;; RETURN VALUE  
;;; If a flat note list, the bar at which we stopped, otherwise t.
;;; 
;;; SYNOPSIS
(defmethod change-notes ((sc slippery-chicken) player start-bar new-notes
                         &optional (use-last-octave t) marks)
;;; ****
  (if (simple-listp new-notes)
      (progn
        (when marks
          (unless (= (length new-notes) (length marks))
            (error "slippery-chicken::change-notes: marks (~a) ~
                    and new-notes (~a) must be the same length"
                   (length marks) (length new-notes))))
        (loop for note in new-notes 
           for count from 0
           ;; this just resets to start-bar; doesn't get an event
           with e = (next-event sc player t start-bar)
           do
           (setf e (next-event sc player t))
           (unless (event-p e)
             (error "slippery-chicken::change-notes: couldn't get event ~a!"
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
      (change-notes (piece sc) player start-bar new-notes use-last-octave)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  A post-generation editing method

;;; see rthm-seq-bar (setf time-sig)
;;; this is a 'brutal' method in that it doesn't check to see if the rhythms in
;;; the bar add up to a whole bar in the new time-sig

;;; ****m* slippery-chicken-edit/change-time-sig
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
(defmethod change-time-sig ((sc slippery-chicken) bar-num-or-ref new-time-sig)
;;; ****
  (change-time-sig (piece sc) bar-num-or-ref new-time-sig))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  A post-generation editing method

;;; ****m* slippery-chicken-edit/add-mark-to-note
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
(defmethod add-mark-to-note ((sc slippery-chicken)
                                 bar-num note-num player mark)
;;; ****
  (add-mark-to-note (piece sc) bar-num note-num player mark))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  A post-generation editing method

;;; 1-based
;;; ****m* slippery-chicken-edit/add-mark-to-event
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
(defmethod add-mark-to-event ((sc slippery-chicken) bar-num event-num player
                                  mark)
;;; ****
  (add-mark-to-event (piece sc) bar-num event-num player mark))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  A post-generation editing method

;;; 28.2.11  event-num can be an integer (1-based) or a list of event numbers 1
;;; for each instrument counting from the top of the score down
;;; ****m* slippery-chicken-edit/add-mark-all-players
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
;;;  A post-generation editing method
;;; for CMN only

;;; ****m* slippery-chicken-edit/note-add-bracket-offset
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
;;;  A post-generation editing method

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
;;;  A post-generation editing method

;;; 1.3.11 another method for adding marks to multiple notes, this time we give
;;; a start-bar/note and an end-bar/note and the given marks will be added to
;;; all inbetween.  start and finish are inclusive and 1-based.  If they're
;;; integers then all notes in the bars will be marked, otherwise a 2-element
;;; list sets the exact note to start/stop at.  NB noteheads need before to be
;;; t in lilypond but bear in mind they're automatically moved over in
;;; event::get-lp-data.  players can be a single symbol or list.
;;; ****m* slippery-chicken-edit/add-marks-to-notes
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
;;;  A post-generation editing method

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
                                           using unrecognised mark: ~a" datum))
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
;;;  A post-generation editing method

;;; ****m* slippery-chicken-edit/add-marks-to-note
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
;;;  A post-generation editing method

;;; ****m* slippery-chicken-edit/rm-marks-from-note
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
;;;  A post-generation editing method

;;; 6.4.11: removes only the given marks, not all marks.  if players are nil,
;;; then all players will be processed
;;; ****m* slippery-chicken-edit/rm-marks-from-notes
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
;;;  A post-generation editing method

;;; ****m* slippery-chicken-edit/rm-slurs
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
(defmethod rm-slurs ((sc slippery-chicken) start end players)
;;; ****
  (rm-marks-from-notes sc start end players '(beg-sl end-sl)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  A post-generation editing method

;;; ****m* slippery-chicken-edit/add-mark-before-note
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
(defmethod add-mark-before-note ((sc slippery-chicken)
                                       bar-num note-num player mark)
;;; ****
  (add-mark-before-note (piece sc) bar-num note-num player mark))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  A post-generation editing method

;;; ****m* slippery-chicken-edit/sc-delete-marks
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
(defmethod sc-delete-marks ((sc slippery-chicken) bar-num note-num player)
;;; ****
  (bh-delete-marks (piece sc) bar-num note-num player))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  A post-generation editing method

;;; ****m* slippery-chicken-edit/sc-delete-marks-from-event
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
(defmethod sc-delete-marks-from-event ((sc slippery-chicken)
                                           bar-num event-num player)
;;; ****
  (setf (marks (get-event sc bar-num event-num player)) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  A post-generation editing method

;;; ****m* slippery-chicken-edit/sc-delete-marks-before
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
(defmethod sc-delete-marks-before ((sc slippery-chicken)
                                         bar-num note-num player)
;;; ****
  (delete-marks-before (piece sc) bar-num note-num player))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  A post-generation editing method

;;; ****m* slippery-chicken-edit/tie
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
(defmethod tie ((sc slippery-chicken) bar-num note-num player 
                &optional curvature)
;;; ****
  (tie (piece sc) bar-num note-num player curvature))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  A post-generation editing method

;;; event numbers are 1-based 
;;; ****m* slippery-chicken-edit/trill
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
;;;  A post-generation editing method

;;; ****m* slippery-chicken-edit/all-rests-to-ties
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
(defmethod all-rests-to-ties ((sc slippery-chicken)
                              start-bar end-bar players
                              &key
                              ;; use up all rests until next attack or (if nil)
                              ;; just the rest bars?
                              (to-next-attack t)
                              ;; if the next attack is the same note/chord as
                              ;; the previous, to to it too?
                              (tie-next-attack nil)
                              (last-rhythm nil)
                              (auto-beam nil))
;;; ****
  (unless (listp players)
    (setf players (list players)))
  (loop for p in players do
        (all-rests-to-ties-aux sc start-bar end-bar p 
                               :to-next-attack to-next-attack 
                               :tie-next-attack tie-next-attack
                               :last-rhythm last-rhythm
                               :auto-beam auto-beam)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  A post-generation editing method

;;; ****m* slippery-chicken-edit/tie-over-rest-bars
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
(defmethod tie-over-rest-bars ((sc slippery-chicken) bar-num players
                               &key (end-bar 99999) ;; num of empty bars
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
;;;  A post-generation editing method

;;; NB end-bar is not when the ties stop, but rather when we last find an event
;;; to tie from (so the ties may go beyond end-bar)

;;; ****m* slippery-chicken-edit/tie-over-all-rests
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
;;;  A post-generation editing method

;;; note-num is 1-based and counts tied-to notes as well
;;; 24.3.11: added end-bar
;;; ****m* slippery-chicken-edit/tie-over-rests
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
;;;  A post-generation editing method

;;; 1.4.11: note-num counts tied-notes but not rests

;;; ****m* slippery-chicken-edit/delete-slur
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
;;;  A post-generation editing method

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
;;;  A post-generation editing method

;;; optional args are actually required but optional because of event class
;;; method  
;;; event-num is 1-based but counts rests and ties
;;; ****m* slippery-chicken-edit/delete-clefs
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
;;;  A post-generation editing method

;;; optional args are actually required but optional because of event class
;;; method  
;;; ****m* slippery-chicken-edit/add-clef
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
;;;  A post-generation editing method

;;; ****m* slippery-chicken-edit/move-clef
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
;;;  A post-generation editing method

;;; NB While this routine generally does a good job of putting the right clefs
;;; in place, it will get confused if notes jump from very high to low
;;; (e.g. over complete piano range).  Called automatically by cmn-display
;;; and write-lp-data-for-all

;;; ****m* slippery-chicken-edit/auto-clefs
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
(defmethod auto-clefs ((sc slippery-chicken) 
                       &key verbose in-c players 
                       (delete-clefs t)
                       (delete-marks-before nil))
;;; ****
  (loop 
     for player in (if players players (players sc)) 
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
;;;  A post-generation editing method

;;; This is different from set-rehearsal-letters in that we don't use the
;;; rehearsal-letters slot of sc, rather, we use the method argument.

;;; ****m* slippery-chicken-edit/set-rehearsal-letter
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
;;;  A post-generation editing method

;;; ****m* slippery-chicken-edit/delete-rehearsal-letter
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
(defmethod delete-rehearsal-letter ((sc slippery-chicken) bar-num
                                    &optional players)
;;; ****
  (unless players
    (setf players (players sc)))
  (loop 
      for p in players 
               ;; remember the letter is actually placed on the bar-line of the
               ;; previous bar
      for bar = (get-bar sc (1- bar-num) p)
      do 
        (setf (rehearsal-letter bar) nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  A post-generation editing method

;;; This method will only combine short bars into longer ones, it won't split
;;; up bars and recombine. 

;;; ****m* slippery-chicken-edit/re-bar
;;; FUNCTION
;;; Don't confuse with rebar method.
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
;;;  A post-generation editing method

;;; ****m* slippery-chicken-edit/auto-beam
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
(defmethod auto-beam ((sc slippery-chicken) &optional (beat nil) (check-dur t))
;;; ****
  (loop for player in (players sc) do
        (loop 
            for bnum from 1 to (num-bars sc) 
            for bar = (get-bar sc bnum player)
            do
              (auto-beam bar beat check-dur))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  A post-generation editing method

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
;;;  A post-generation editing method

;;; ****m* slippery-chicken-edit/sc-move-dynamic
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
(defmethod sc-move-dynamic ((sc slippery-chicken) bar-num player
                            ;; event numbers 1-based but counting rests and ties
                            from to &optional to-bar)
;;; ****
  (unless to-bar
    (setf to-bar bar-num))
  (let ((dyn (sc-remove-dynamic sc bar-num player from)))
    (add-mark (get-event sc to-bar to player) dyn)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  A post-generation editing method

;;; 1.4.11: remove all dynamics on a single event.  event-num includes ties and
;;; rests 
;;; ****m* slippery-chicken-edit/sc-remove-dynamic
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
;;;  A post-generation editing methdo

;;; 16.3.11: start end are either bar numbers or (bar-num note-num) pairs.
;;; note-nums are 1-based and count ties but not rests.
;;; ****m* slippery-chicken-edit/sc-remove-dynamics
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
;;;  A post-generation editing methdo

;;; if two or more notes have the same dynamic, remove all but the first
;;; ****m* slippery-chicken-edit/remove-extraneous-dynamics
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
;;;  A post-generation editing methdo

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
  (let ((bar (get-bar (piece sc) bar-num player)))
    (if (and start-note end-note)
        (progn
          (delete-beam (get-nth-non-rest-rhythm (1- start-note) bar))
          (delete-beam (get-nth-non-rest-rhythm (1- end-note) bar)))
        (delete-beams bar))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  A post-generation editing methdo

;;; NB This might delete rehearsal letters, instrument changes (and maybe other
;;; things) attached to a bar/event.
;;; ****m* slippery-chicken-edit/delete-bars
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
;;;  A post-generation editing methdo

;;; 20.7.11 (Pula)
;;; see double-events (below) for details
;;; ****m* slippery-chicken-edit/move-events
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
;;;  A post-generation editing methdo
;;; 20.7.11 (Pula)
;;; start/end-event are 1-based and count rests and ties, not just struck notes
;;; if end-event is nil we use all events until end of end-bar
;;; if update we update-slots for the whole sc object
;;; 
;;; a nasty side-effect at the moment is that any existing events in the
;;; doubling players at the beginning of the start-bar or end of the end-bar
;;; will be deleted, so this only works for copying notes into completely empty
;;; bars, not razor splicing.

;;; ****m* slippery-chicken-edit/double-events
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

;;; 21.7.11 (Pula)
;;; turn notes into rests.
;;; start/end-event are 1-based and count rests and ties, not just struck notes
;;; if players is nil, process all players
;;; if end-event is nil go to the end of the end-bar
;;; ****m* slippery-chicken-edit/delete-events
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
;;;  A post-generation editing methdo

;;; 23.7.11 (Pula) 1-based and counting tied notes but not rests
;;; NB in general calling auto-beam is a good idea (esp. if you're deleting
;;; notes under a beam) but if might fail if you have notes longer than a beat.
;;; ****m* slippery-chicken-edit/sc-force-rest
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
;;;  A post-generation editing methdo

;;; delete any notes in the existing bars
;;; start-bar and end-bar are inclusive
;;; ****m* slippery-chicken-edit/force-rest-bars
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
(defmethod force-rest-bars ((sc slippery-chicken) start-bar end-bar players)
;;; ****
  (loop for bar-num from start-bar to end-bar do
       (loop for player in players 
            for bar = (get-bar sc bar-num player)
            do
            (unless bar
              (error "slippery-chicken::force-rest-bars: no bar ~a for ~a"
                     bar-num player))
            (force-rest-bar bar))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  A post-generation editing methdo

;;; 20.8.11: if no end-event we process all events in the last bar
;;; ****m* slippery-chicken-edit/force-artificial-harmonics
;;; FUNCTION
;;; For string scoring purposes only: Transpose the note down two octaves and
;;; add the harmonic symbol at the perfect fourth.
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
(defmethod force-artificial-harmonics ((sc slippery-chicken) player start-bar
                                       start-event end-bar &optional end-event)
;;; ****
  (loop for e in (get-events-from-to sc player start-bar start-event end-bar
                                     end-event)
       do
       (unless (is-rest e)
         (force-artificial-harmonic e)))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  A post-generation editing methdo

;;; 28.9.11: add accidental in ().  note-num (counting ties, and from 1) can be
;;; an integer or list e.g. '(1 2). If the latter it would be the first chord,
;;; second note up.

;;; ****m* slippery-chicken-edit/set-cautionary-accidental
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
(defmethod set-cautionary-accidental ((sc slippery-chicken) bar-num note-num
                                      player &optional written)
;;; **** 
  (cautionary-accidental-aux sc bar-num note-num player t written))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  A post-generation editing methdo

;;; ****m* slippery-chicken-edit/unset-cautionary-accidental
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
(defmethod unset-cautionary-accidental ((sc slippery-chicken) bar-num note-num
                                        player &optional written)
;;; **** 
  (cautionary-accidental-aux sc bar-num note-num player nil written))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* slippery-chicken-edit/add-arrow-to-events
;;; FUNCTION
;;; Adds an arrow above the notes with a start and ending text.  Used most
;;; often for transitions from one playing state to another.
;;;
;;; See also the add-arrow method in the event class.
;;; 
;;; ARGUMENTS
;;; - a slippery-chicken object
;;; - a text string for the beginning of the arrow
;;; - a text string for the end of the arrow
;;; - the starting event reference (list) of the form (bar-number event-number)
;;;   where event numbers count from 1 and include rests and tied notes.
;;; - the end event reference (list) of the form (bar-number event-number)
;;; - the player this should be attached to
;;; 
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to indicate whether or not to print a warning when trying to 
;;;   attach an arrow and accompanying marks to a rest. Default = NIL. 
;;; 
;;; RETURN VALUE
;;; T
;;; 
;;; DATE
;;; April 9th 2012
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
