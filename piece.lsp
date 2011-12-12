;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* bar-holder/piece
;;; NAME 
;;; piece
;;;
;;; File:             piece.lsp
;;;
;;; Class Hierarchy:  named-object -> linked-named-object -> sclist -> 
;;;                   circular-sclist -> assoc-list -> recursive-assoc-list ->
;;;                   piece
;;;                                   AND
;;;
;;;                   named-object -> linked-named-object -> bar-holder ->
;;;                   piece  
;;;
;;; Version:          1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Implementation of the piece class which holds all the
;;;                   note information for a whole piece in the form of
;;;                   sections (possibly subsections), which then contain
;;;                   player-sections, sequenzes and rthm-seq-bars.  
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    16th February 2002
;;;
;;; $$ Last modified: 23:59:17 Thu Dec  8 2011 ICT
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

;;; The data slot is a list of named objects the data of which is a section.
(defclass piece (bar-holder rthm-seq-map)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone ((p piece))
  (clone-with-new-class p 'piece))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object :before ((p piece) stream)
  (format stream "~&PIECE: "))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+cmn
(defmethod cmn-display ((p piece) 
                        &key
                        (start-bar nil)
                        ;; if nil will auto-number bars
                        (start-bar-numbering nil)
                        (end-bar nil)
                        (page-nums t)
                        (bars-per-system-map nil)
                        (ensemble nil)
                        (all-output-in-one-file t)
                        (one-line-per-page nil)
                        (set-map nil)
                        (instrument-change-map nil)
                        (file "cmn.eps")
                        ;; separation for groups in a score line
                        (group-separation 2)
                        (staff-separation 3)
                        ;; although there's a system-separation parameter for
                        ;; cmn, this is the one that actually seems to do that
                        ;; job... 
                        (line-separation 5)
                        (players nil)
                        ;; write the section number/refs into the score?
                        (write-section-info t)
                        (staff-groupings nil)
                        (system-separation cmn::line-mark)
                        (process-event-fun nil)
                        (multi-bar-rests nil)
                        ;; can be nil, t or 'only
                        (empty-staves nil)
                        (in-c t)
                        (auto-bar-nums :by-line)
                        (page-height 29.7)
                        (page-width 21.0)
                        (automatic-octave-signs nil)
                        (display-marks-in-part nil)
                        (display-time nil)
                        (size 15))
  (object-is-nil? ensemble "piece::cmn-display" 'ensemble)
  (object-is-nil? bars-per-system-map "piece::cmn-display"
                  'bars-per-system-map)
  (object-is-nil? instrument-change-map "piece::cmn-display"
                  'instrument-change-map)
  (if players
      (players-exist ensemble players)
    (setf players (players ensemble)))
  (let* ((just-empty (eq empty-staves 'only))
         (cmn-data (get-cmn-data p players empty-staves set-map 
                                 write-section-info process-event-fun in-c
                                 display-marks-in-part display-time))
         (cmn-data-num-bars (length (first cmn-data)))
         (num-bars (num-bars p))
         ;; (first-ins (first cmn-data))
         (starting-ins-objs
          (loop for player in players collect 
                              (let ((player-obj (get-data player ensemble))
                                    (ins-ref nil))
                                (when (doubles player-obj)
                                  (setf ins-ref (get-first-for-instrument
                                                 instrument-change-map 
                                                 player))
                                  (unless ins-ref
                                    (error "piece::cmn-display: ~a doubles, ~
                                            enter data into ~
                                            instrument-change-map"
                                           player)))
                                (player-get-instrument player-obj
                                                       ins-ref nil))))
         (starting-clefs (loop for ins in starting-ins-objs collect
                               (starting-clef ins)))
         (staff-names (loop for ins in starting-ins-objs collect
                            (staff-name ins)))
         (cmn-staff-args (loop for player in players collect 
                               (cmn-staff-args (get-data player ensemble))))
         (systems nil))
    (unless start-bar
      (setf start-bar 1))
    (unless end-bar
      (setf end-bar num-bars))
    ;; it's too much hassle to do anything but generate the whole piece when
    ;; we're using multi-bar-rests 
    (when multi-bar-rests
      (setf start-bar 1
            end-bar cmn-data-num-bars))
    (setf num-bars (1+ (- end-bar start-bar)))
    ;; if we want multi-bar-rests we can't do a simple subseq here!
    ;; we've created multi-bar rests in the sc and other methods; here we
    ;; have to simply generate the whole piece if we want multi-bar-rests
    ;; because anything else would get pretty messy.
    (unless multi-bar-rests
      (setf cmn-data (loop for ins in cmn-data collect
                           (subseq ins (1- start-bar) end-bar))))
    (unless staff-groupings
      (setf staff-groupings (list (length players))))
    (when (and empty-staves (not just-empty))
      ;; when we want empty staves, then we have to put the starting clefs and
      ;; staff names twice in each system...
      (setf starting-clefs (loop 
                              for clef in starting-clefs
                              collect clef 
                              collect clef)
            staff-names (loop 
                            for staff-name in staff-names 
                            collect staff-name
                            collect staff-name)
            cmn-staff-args (loop 
                            for sa in cmn-staff-args 
                            collect sa
                            collect sa)
            staff-groupings (loop for i in staff-groupings collect (* 2 i))))
    (when set-map
      ;; groupings into bracketed sub-systems doesn't yet work.  take this line
      ;; of code out and put the one marked *** below in when it does work 
      ;; (incf (first staff-groupings) 2)
      ;; we have to put the clefs for the set system into the list
      (setf starting-clefs (append starting-clefs (list 'treble 'bass))
            staff-names (append staff-names
                                (list "(Set Treble)" "(Set Bass)"))
            ;; make the sets a little smaller...
            cmn-staff-args (append cmn-staff-args 
                                   (list (list (cmn::staff-size .7))
                                         (list (cmn::staff-size .7))))
            ;; *** see above
            staff-groupings (econs staff-groupings 2)))
    (format t "~&Inserting line breaks...")
    ;; cmn-data is a list containing a list for each instrument of a list
    ;; of bars for the whole piece.
    (flet ((sys-sep (bar-num)
                    (loop for ins in cmn-data do
                      (setf (nth bar-num ins)
                            (econs (nth bar-num ins)
                                   system-separation)))))
      (loop 
        with bar-num = -1
        do (incf bar-num (if (data bars-per-system-map)
                             (data (scm-get-data (+ 1 start-bar bar-num) 
                                                 bars-per-system-map))
                           ;; !!!!!!!! default bars per system is 4 !!!!!!!!!!
                           4))
        while (< bar-num num-bars) do (sys-sep bar-num))
      (sys-sep (1- num-bars)))
    (format t "~&Creating systems...")
    (setf cmn-data (split-into-sub-groups cmn-data staff-groupings)
          staff-names (split-into-sub-groups staff-names staff-groupings)
          cmn-staff-args (split-into-sub-groups cmn-staff-args staff-groupings)
          starting-clefs (split-into-sub-groups starting-clefs 
                                                staff-groupings))
    (setf cmn-data (loop for group in cmn-data collect
                         (loop for ins in group collect
                               ;; get the bars out of their lists
                               (loop for bar in ins append bar)))
          systems (loop 
                      for group in cmn-data 
                      for st-name in staff-names
                      for st-args in cmn-staff-args
                      for clefs in starting-clefs
                      collect
                        (cmn::cmn-system group st-name clefs st-args)))
    (format t "~&Calling CMN...")
    (cmn::cmn-display systems 
                      :page-height page-height
                      :page-width page-width
                      :file file 
                      :group-separation group-separation
                      :auto-bar-nums auto-bar-nums
                      :automatic-octave-signs automatic-octave-signs
                      :page-nums page-nums
                      :start-bar-numbering start-bar-numbering
                      :size size 
                      :line-separation line-separation
                      :all-output-in-one-file all-output-in-one-file
                      :one-line-per-page one-line-per-page
                      :staff-separation staff-separation)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Returns a list of lists, one for each instrument, containing all the notes
;;; in the piece.
;;;
;;; get-cmn-data goes from the piece to the event level as follows: 
;;; piece -> section (plus subsections where appropriate) -> player-section ->
;;; sequenz -> rthm-seq-bar -> event

#+cmn
(defmethod get-cmn-data ((p piece) &optional
                         (players nil)
                         (empty-staves nil)
                         (set-map nil)
                         (write-section-info t)
                         (process-event-fun nil)
                         (in-c t)
                         display-marks-in-part
                         display-time)
  (let ((just-empty (eq empty-staves 'only))
        (cmn-data
         (loop for player in players 
            for count from 0
            do (format t "~&Generating ~a..." player)
            collect
            ;; this gets a list of bars for the section, appending them one
            ;; after the other
            (loop for no in (data p) append
               ;; the data of the named-object is the section
                 (get-cmn-data (data no) player nil t 
                               write-section-info process-event-fun 
                               in-c display-marks-in-part 
                               (and (zerop count) display-time)))))
        (empty (when empty-staves
                 (format t "~&Generating empty staves...")
                 (loop for player in players
                    collect
                    (loop for no in (data p) append
                       ;; this final t tells the section we just want
                       ;; empty sections. 
                         (get-cmn-data (data no) player t t
                                       write-section-info)))))
        (set-staves (when set-map
                      (format t "~&Generating sets...")
                      (loop
                         for no in (data p) 
                         with first-player = (first players)
                         append
                         ;; get empty sections here too but this time
                         ;; collect each sequenz in its own list instead of
                         ;; appending them all together.
                         ;; we give 64 as the empty parameter here to make
                         ;; invisible rests of the whole bar duration minus
                         ;; a 64th note--this will become the chord later.
                         (get-cmn-data (data no) first-player 64 nil)
                         into treble
                         append 
                         ;; instead of copying the above get-cmn-data call,
                         ;; do it all again (expensive) to ensure we get
                         ;; separate cmn objects (otherwise displaying them
                         ;; twice can screw things up).
                         (get-cmn-data (data no) first-player 64 nil nil)
                         into bass
                         finally (return (list treble bass)))))
        (sets (when set-map
                ;; a list of all the complete-sets 
                (get-all-data-from-palette set-map)))
        (num-bars (num-bars p)))
    ;; simple check for the right number of bars
    (loop for player in cmn-data and i from 0 do
         (unless (= num-bars (length player))
           (warn "piece::get-cmn-data: Should be ~a bars for each ~
                   instrument but ~a has only ~a.  Assuming multi-bar-rests."
                 num-bars (nth i players) (length player))))
    ;; if we want empty staves we now have to interleave the staff with the
    ;; notes with the staff with the empty bars.
    (when empty-staves
      (setf cmn-data (loop for ins-notes in cmn-data and ins-empty in empty
                        unless just-empty collect ins-notes 
                        collect ins-empty)))
    (when set-map
      (setf set-staves (splice-sets-into-staves sets set-staves)
            cmn-data (append cmn-data set-staves)))
    cmn-data))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod insert-bar ((p piece) (rsb rthm-seq-bar) bar-num 
                       ;; these aren't actually optional but we don't
                       ;; need them in the rthm-seq method 
                       &optional section player seq-num ; seq-num is 1-based!
                                 ;; this really is optional
                                 pitch-seq)
  (let ((seq (get-nth-sequenz p section player (1- seq-num) nil)))
    (insert-bar seq rsb bar-num pitch-seq)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This is the primary method that should be called to access instrument
;;; sequences as it handles the case where an instrument doesn't play in a
;;; sequence. 
;;;
;;; When the given player sits this sequence out and create-rest-seq is t,
;;; create a rest sequence (one where there's the correct number and type of
;;; bars but only rest bars) based on a sequence in one of the playing
;;; instruments.
;;;
;;; This is the method called when the player is mentioned in the section but
;;; sits a sequence out (with nil).  

(defmethod get-nth-sequenz ((p piece) section player seq-num ; 0-based
                            &optional (create-rest-seq t))
  (flet ((get-seq (spieler) 
           ;; if we don't get a section, it means the player doesn't play in
           ;; this section. If we don't get a seq, then it means it just
           ;; doesn't play this sequence.
           (let* ((section (get-player-section section spieler p))
                  (seq (when section (nth seq-num (data section)))))
             seq)))
    (let ((seq (get-seq player)))
      (if seq
          seq
        (when create-rest-seq
          (let ((all-players (players p)))
            (unless (member player all-players)
              (error "piece::get-nth-sequenz: Player ~a is ~
                      not a member of the ensemble ~a"
                     player all-players))
            (loop for p in (remove player all-players) do
                  (when (setf seq (get-seq p))
                    (return (clone-as-rest-sequenz seq t nil player))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-player-section (section player (p piece))
  (unless (listp section)
    (setf section (list section)))
  (let ((sec (get-data section p nil)))
    (when sec
      (if (typep (first (data (data sec)))
                 'bar-holder)
          (get-data player (data sec) nil)
        (error "piece::get-player-section: Section ~a has subsections!: ~a"
               section sec)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 

(defmethod get-bar ((p piece) bar-num-or-ref &optional player)
  (if (listp bar-num-or-ref)
      (progn
        (unless (= 3 (length bar-num-or-ref))
          (error "piece::get-bar: ~
                  bar-num-or-ref is either an absolute bar ~
                  number or a reference of the form (section sequence bar): ~a"
                 bar-num-or-ref))
        (get-bar-from-ref p (first bar-num-or-ref) player 
                          (second bar-num-or-ref) 
                          (third bar-num-or-ref)))
    (call-next-method)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; sequenz-num and bar-num are 1-based.

(defmethod get-bar-from-ref ((p piece) section player sequenz-num 
                             bar-num)
  (let ((sequenz (get-nth-sequenz p section player (1- sequenz-num))))
    (when sequenz 
      (get-bar sequenz (1- bar-num) t))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; sequenz-num and bar-num are 1-based and the latter is of course the bar
;;; number in the sequenz, not the whole piece (otherwise, why do we want the
;;; bar-num?).  

(defmethod get-bar-num-from-ref ((p piece) section sequenz-num bar-num)
  ;; just use the first player to get his bar...
  (bar-num (get-bar-from-ref p section (first (players p)) sequenz-num 
                             bar-num)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; all refs are 1-based.

(defmethod replace-events ((p piece) player bar-num start-event
                           replace-num-events new-events
                           &optional (auto-beam nil) ignore)
  (declare (ignore ignore))
  (let* ((bar (get-bar p bar-num player))
         (rthms nil)
         (nth (1- start-event)))
    ;; those events that were previously start or end points for brackets may
    ;; be replaced here leaving the events inbetween with references to now
    ;; deleted brackets (they have bracket slots with negative numbers which
    ;; indicate which bracket they are under when abs'ed).  Delete all tuplets
    ;; and beams here to avoid errors in cmn
    (delete-tuplets bar)
    (delete-beams bar)
    (when bar 
      (setf rthms (my-copy-list (rhythms bar))))
    (unless bar
      (error "piece::replace-events: Couldn't get bar number ~a"
             bar-num))
    ;; a rest bar has no rhythms but we may want to fill it with some so fake
    ;; the rthms here.  
    (unless rthms
      ;; doesn't matter what's in the list as all elements will be replaced.
      (setf rthms (ml nil replace-num-events)))
    (setf rthms (remove-elements rthms nth replace-num-events)
          rthms (splice new-events rthms nth))
    ;; of course, the stats for the sequenz and whole piece are now incorrect,
    ;; but we leave that update to the user, we don't want to always call it
    ;; here.
    (setf (rhythms bar) rthms)
    (when auto-beam
      (auto-beam bar auto-beam))
    (is-full bar))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; As always, the bar numbers can be integers or references (section,
;;; sequence-num, bar-num -- 1-based)
;;;
;;; When num-bars is nil, all bars in the piece starting from to-start-bar will
;;; be transposed.  

(defmethod copy-bars ((p piece) from-start-bar to-start-bar 
                      from-player to-player num-bars 
                      &optional (print-bar-nums nil))
  (let ((from-bar (clone (get-bar p from-start-bar from-player)))
        (to-bar (get-bar p to-start-bar to-player)))
    (unless num-bars
      (setf num-bars (- (num-bars p) (bar-num to-bar) -1)))
    (loop for fbnum from (bar-num from-bar)
          for tbnum from (bar-num to-bar)
          with first-time = t
          with player-section
          with sequenz
          repeat num-bars do
          (unless first-time
            (setf from-bar (clone (get-bar p fbnum from-player))
                  to-bar (get-bar p tbnum to-player)))
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
          (setf player-section (get-data (player-section-ref to-bar) p)
                sequenz (get-nth (nth-seq to-bar) player-section))
          (set-nth-bar (nth-bar to-bar) from-bar sequenz)))
  t)
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod handle-ties ((p piece))
  (loop for player in (players p) do
        (loop 
            for bar-num from 1 to (num-bars p)
            for bar = (get-bar p bar-num player)
            with last-event 
            do
             (unless bar
               (error "piece::handle-ties: bar is nil (bar-num: ~a, player: ~a"
                      bar-num player))
              (loop 
                  with tie-ok
                  for event in (rhythms bar) 
                  for event-num from 1 
                  do
                    (unless (event-p event)
                      (error "piece::handle-ties: event is nil; ~a bar num ~a"
                             player bar-num))
                    ;; 10/5/07: just silently kill any incomplete ties
                    #|
                    (when (and (= bar-num 1158) 
                               (eq player 'fl))
                      (print (is-tied-from last-event))
                      (print (is-tied-to event)))
                      |#
                    (when last-event
                      (when (and (is-tied-from last-event)
                                 (not (is-tied-to event)))
                        ;; (print bar-num)
                        (setf (is-tied-from last-event) nil))
                      (when (and (not (is-tied-from last-event))
                                 (is-tied-to event))
                        (setf (is-tied-to event) nil)))
                    ;; 28/3/07 make sure we're tying to the same note/chord
                    (when (and last-event
                               (is-tied-to event))
                      (setf tie-ok
                        (if (is-single-pitch event)
                            (pitch= (pitch-or-chord event)
                                    (pitch-or-chord last-event)
                                    t)
                          (chord-equal (pitch-or-chord event)
                                       (pitch-or-chord last-event))))
                      (unless tie-ok
                        (error "~a~&piece::handle-ties: in bar ~a, ~a, tied ~
                                notes/chords not the same: ~a ~a!"
                               bar bar-num player (get-pitch-symbol last-event)
                               (get-pitch-symbol event))))
                    (setf last-event event)
                    (when (and (not (is-tied-to event))
                               (is-tied-from event))
                      ;; (print bar-num)
                      (multiple-value-bind
                          (dur dur-tmpo)
                          (get-tied-durations p bar-num event-num
                                              player)
                        (setf (compound-duration event) (+ (duration event) 
                                                           dur)
                              (compound-duration-in-tempo event) 
                              (+ (duration-in-tempo event) dur-tmpo)
                              (end-time event) 
                              (+ (start-time event)
                                 (compound-duration-in-tempo event)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-tied-durations ((p piece) bar-num start-event player)
  ;; (print 'get-tied-durations)
  (loop 
      with result = 0.0
      with result-in-tempo = 0.0
      with happy = t
      for bnum from bar-num
      for bar = (get-bar p bnum player) 
      for start = (if (= bnum bar-num)
                            start-event
                          0)
      do
        (unless bar
          (error "get-tied-durations: ran out of bars at bar-num ~a!" 
                 bnum))
        (loop 
            for event-num from start below (num-rhythms bar)
            for event = (get-nth-event event-num bar)
            do
              (when (is-tied-to event)
                (incf result (duration event))
                (when (event-p event)
                  (incf result-in-tempo (duration-in-tempo event))))
              (unless (is-tied-from event)
                (progn
                  (setf happy nil)
                  (return))))
        (unless happy
          (return (values result result-in-tempo)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; start-bar can be an absolute bar number or a list of the form 
;;; '(section sequence bar) (if there a subsections then e.g. '((3 1) 4 2) )
;;; you have to fill the number of bars, i.e. you can't just leave the last har
;;; half-filled expecting the existing events to make up the rest.

(defmethod replace-multi-bar-events ((p piece) 
                                     player start-bar num-bars new-events 
                                     &key
                                     ;; although a key argument tempo-map is
                                     ;; required: it is given here as a key arg
                                     ;; value nil simply so we don't have to
                                     ;; specify it in the slippery-chicken
                                     ;; class.
                                     (tempo-map nil)
                                     (consolidate-rests t)
                                     (sc nil)
                                     ;; for consolidate rests
                                     (beat nil)
                                     (auto-beam t)
                                     ;;31.3.11: if this is t, then rthms > a
                                     ;;beat will case an error 
                                     (auto-beam-check-dur t)
                                     (tuplet-bracket nil))
  (object-is-nil? tempo-map "bar-holder::replace-multi-bar-events" 'tempo-map)
  (object-is-nil? sc "bar-holder::replace-multi-bar-events" 'sc)
  (when (listp start-bar)
    (unless (= 3 (length start-bar))
      (error "piece::replace-multi-bar-events: ~
              start-bar is either an absolute bar ~
              number or a reference of the form (section sequence bar): ~a"
             start-bar))
    (setf start-bar (get-bar-num-from-ref 
                     p
                     (first start-bar) (second start-bar) (third start-bar))))
  (let ((total-ate-rthms 0))
    (loop 
       with ate-rthms 
       with player-obj = (get-player sc player)
       for bar-num from start-bar repeat num-bars
       for bar = (get-bar p bar-num player)
       for transposition = (get-transposition-at-bar player bar-num sc)
       do
       (unless new-events
         (error "bar-holder::replace-multi-bar-events: ~
                 no new-events (~a, bar ~a)!" player bar-num))
       (unless bar
         (error "bar-holder::replace-multi-bar-events: ~
                 Can't get bar ~a for ~a"
                bar-num player))
       ;; (format t "~%bar-holder::replace-multi-bar-events: ~a"
       ;; (length (rhythms bar)))
       (setf ate-rthms (fill-with-rhythms 
                        bar new-events 
                        :transposition transposition
                        :midi-channel (midi-channel player-obj)
                        :microtones-midi-channel
                        (microtones-midi-channel player-obj)))
       ;; (print ate-rthms)
       (unless ate-rthms
         (error "bar-holder::replace-multi-bar-events: ~
                 Not enough rhythms to fill all the bars!"))
       (incf total-ate-rthms ate-rthms)
       (setf new-events (nthcdr ate-rthms new-events))
       ;; (format t "~%replace-multi-bar-events: before: ~a"
       ;; (length (rhythms bar)))
       (when consolidate-rests
         (consolidate-rests bar :beat beat)
         ;; (format t "~%replace-multi-bar-events: after: ~a" (length
         ;; (rhythms bar))) 
         ;;
         ;; now we've consolidated for the requested beat division,
         ;; do it again for the natural beat value of the bar to
         ;; combine e.g. two 1/8 rests into a 1/4 rest when given
         ;; beat was 1/8 in a 2/4 bar.  but don't consolidate any
         ;; rthms less than the given beat!
         (when (> (beat-duration (get-time-sig bar))
                  (duration (make-rhythm beat)))
           ;; 8.12.11 this call is wrong (picked up by CCL)
           ;; (consolidate-rests bar nil beat)))
           (consolidate-rests bar)))
       (delete-beams bar)
       (delete-tuplets bar)
       (when auto-beam
         (auto-beam bar nil auto-beam-check-dur))
       (when tuplet-bracket
         (auto-put-tuplet-bracket-on-beats bar tuplet-bracket beat))
       (gen-stats bar))
    (when new-events
      (warn "piece::replace-multi-bar-events: still some events left over: ~a"
            new-events))
    (update-slots p tempo-map (start-time p) (start-time-qtrs p) (start-bar p))
    total-ate-rthms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod add-rest-player-sections ((p piece))
  (add-rest-player-sections-aux p (players p)))
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod add-rest-sequenzes ((p piece))
  (let ((player-section (get-first p)))
    (loop while player-section do
      (let ((player (id player-section))
            (section (butlast (this player-section))))
        (loop for seq in (data player-section) and i from 0 do
          (unless seq
            (setf (nth i (data player-section)) 
                  (get-nth-sequenz p section player i))))
        (setf player-section (get-data (next player-section) p nil))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod change-time-sig ((p piece) bar-num-or-ref new-time-sig)
  (unless (time-sig-p new-time-sig)
    (setf new-time-sig (make-time-sig new-time-sig)))
  (loop for player in (players p) do
        (let ((bar (get-bar p bar-num-or-ref player)))
          (setf (time-sig bar) new-time-sig)))
  (update-write-time-sig2 p)
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; the bar-holder method update-write-time-sig is quicker because it only
;; checks first bar of each sequence.  Here we really need to check each bar.

(defmethod update-write-time-sig2 ((p piece)
                                   &optional ignore1 ignore2)
  (declare (ignore ignore1 ignore2))
  (let ((last-bar nil)
        (this-bar nil))
    (loop for player in (players p) do
         (setf last-bar (get-bar p 1 player))
       ;; 1/2/10: make sure the first bar has a time-sig written!
         (setf (write-time-sig last-bar) t)
         (loop for i from 2 to (num-bars p) do
              (setf this-bar (get-bar p i player))
              (unless (eq t (time-sig-equal last-bar this-bar))
                (setf (write-time-sig this-bar) t))
              (setf last-bar this-bar)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* piece/rebar
;;; FUNCTION
;;; rebar:
;;;
;;; Go through the sequences and rebar according to the first one that has the
;;; least number of bars (but following the player hierarchy).
;;; 
;;; DATE 29.1.10
;;;
;;; ARGUMENTS:
;;; - the piece instance (usually provided by calling from the slippery-chicken
;;; class)  
;;; - a list of instruments in the piece, ordered in terms of importance
;;; i.e. which instrument's bar structure should take precedence.
;;; 
;;; NB the optional arguments are actually required in this class (not in
;;; slippery-chicken) but the rebar-fun is not yet used.
;;; 
;;; RETURN VALUE: 
;;; always t
;;; 
;;; SYNOPSIS
(defmethod rebar ((p piece) &optional instruments-hierarchy rebar-fun)
;;; ****
  (declare (ignore rebar-fun))
  (let* ((player-sections 
          ;; sort according to the given instruments hierarchy in case some
          ;; instruments have the same number of bars.
          ;; (get-all-refs p) will get a list of (section instrument) references
          ;; then (get-data '(section ins) p) returns a player-section, the
          ;; sclist-length of which is the number of seqs in the section.
          (sort (get-all-refs p)
                #'(lambda (x y)
                    (let ((xpos (position (second x) instruments-hierarchy))
                          (ypos (position (second y) instruments-hierarchy))
                          (xsec (first x))
                          (ysec (first y)))
                      (unless (and (numberp xpos)
                                   (numberp ypos))
                        (error "piece::rebar: couldn't get positions: ~a ~a ~a"
                               x y instruments-hierarchy))
                      (unless (and (numberp xsec)
                                   (numberp ysec))
                        (error "piece::rebar: can't sort unless section ~
                                ids are numbers: ~a ~a"
                               xsec ysec))
                      ;; got to sort so that each section comes one after the
                      ;; other but each ins according to hierarcy within a
                      ;; section, so make section worth 1000 (there'll never be
                      ;; 1000 instruments will there?
                      (< (+ xpos (* 1000 xsec))
                         (+ ypos (* 1000 ysec))))))))
    (setf player-sections (split-into-sub-groups2 
                           player-sections (length instruments-hierarchy)))
    ;; (print player-sections)
    (loop for section in player-sections 
       for section-ref = (first (first section))
       for num-rthm-seqs = (num-rthm-seqs (get-data (first section) p))
       do
       ;; get each rthm-seq in turn for each player and find the one with the
       ;; least bars 
       (loop for rsi below num-rthm-seqs 
          for seqs = 
          (loop for player in section 
             collect (get-nth-sequenz p section-ref (second player) rsi nil))
          with least-bars-pos
          with least-bars-rs
          with least-bars
          do
          (setf least-bars most-positive-fixnum)
          ;; get the one with the least bars
          (loop for seq in seqs
             for p-i from 0
             do
             (when (< (num-bars seq) least-bars)
               (setf least-bars (num-bars seq)
                     least-bars-pos p-i
                     least-bars-rs seq)))
          ;; now go through them again and rebar them according to metric
          ;; structure of least-bars-rs
          (loop for seq in seqs for p-i from 0 do
               (unless (= p-i least-bars-pos)
                 (adopt-meters seq least-bars-rs :clone nil))))))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; delete the whole sequenz which holds the bar with a given number.  use with
;;; caution: we only delete the sequence for the given player so the rest will
;;; be out of sync
(defmethod delete-sequenzes ((p piece) bar-num player &optional (how-many 1))
  (let* ((bar (get-bar p bar-num player))
         (section-ref (when bar (butlast (player-section-ref bar))))
         (section (when section-ref (get-player-section section-ref player p))))
    (when section
      (setf (data section) 
            (remove-elements (data section) (nth-seq bar) how-many))))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-sequenz-from-bar-num ((p piece) bar-num player)
  (let* ((bar (get-bar p bar-num player))
         (section-ref (when bar (butlast (player-section-ref bar))))
         (section (when section-ref (get-player-section section-ref player p)))
         (seq (when section (nth (nth-seq bar) (data section)))))
    (when seq
      (values seq (nth-bar bar)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Related functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun add-rest-player-sections-aux (ral players)
  ;; (print players)
  (let ((num-players (length players)))
    (loop for no in (data ral) do
          (let* ((section (data no))
                 (section-data (data section))
                 (first-player-section (first section-data))
                 (new-section (make-list num-players))
                 (ps nil))
            (if (typep (data (first section-data)) 'section)
                (add-rest-player-sections-aux section players)
              (loop for player in players and i from 0 do
                    ;; this get-data call will return a player-section or nil
                    (setf ps (get-data player section nil)
                          (nth i new-section)
                          (if ps
                              ps
                            (clone-as-rest-player-section first-player-section
                                                          player)))
                    finally (setf (data section) new-section)))))
    (relink-named-objects ral)))
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; <sets> is a flat list of all the sets in the piece, one for each sequence;
;;; <staves> is a list of two lists, the second being a duplicate of the first
;;; which is a list of lists, one for each sequence, each sequence containing a
;;; list of bars.

#+cmn
(defun splice-sets-into-staves (sets staves)
  (loop 
      with treble-list = (first staves)
      with bass-list = (second staves)
      for treble-seq in treble-list
      for bass-seq in bass-list
      for set in sets
      and i from 0
      do
        ;; TODO: by just processing (data set) we only get the main set not
        ;; the subsets or related sets: fix this.
        (let* ((treble-bass-split (get-cmn-treble-bass (data set)))
               (treble (first treble-bass-split))
               (bass (second treble-bass-split))
               (cmn-treble (cmn::cmn-stemless-chord
                            treble
                            :rq  4/64
                            :chord-text (format nil "~a ~a"
                                                (list-to-string (this set) 
                                                                "." nil)
                                                (if (tag set)
                                                    (tag set)
                                                  ""))))
               (cmn-bass (cmn::cmn-stemless-chord 
                          bass
                          :rq 4/64))
               (treble-seq-bar1 (first treble-seq))
               (bass-seq-bar1 (first bass-seq))
               ;; should be the same for both clefs
               (items-in-bar (length bass-seq-bar1))
               (iib-2 (- items-in-bar 2)))
          ;; first is the first bar of the sequenz
          (setf (first treble-seq) (append
                                    ;; maybe there's a meter, maybe not...
                                    ;; the last two things in the bar are the
                                    ;; wrest and the bar line
                                    (butlast treble-seq-bar1 2)
                                    (cons cmn-treble 
                                          (subseq treble-seq-bar1 iib-2)))
                ;; same again for the bass...
                (first bass-seq) (append
                                  ;; maybe there's a meter, maybe not...
                                  ;; the last two things in the bar
                                  ;; are the wrest and the bar line 
                                  (butlast bass-seq-bar1 2)
                                  (cons cmn-bass 
                                        (subseq bass-seq-bar1 iib-2)))))
      append treble-seq into treble-bars
      append bass-seq into bass-bars
      finally (return (list treble-bars bass-bars))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; EOF piece.lsp
