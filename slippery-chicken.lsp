;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* sc/slippery-chicken
;;; NAME 
;;; slippery-chicken
;;; 
;;; File:             slippery-chicken.lsp
;;;
;;; Class Hierarchy:  named-object -> slippery-chicken
;;;
;;; Version:          1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Implementation of the slippery-chicken class.
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    March 19th 2001
;;;
;;; $$ Last modified: 00:01:22 Fri Dec  9 2011 ICT
;;;
;;; SVN ID: $Id: slippery-chicken.lsp 385 2011-12-02 20:01:04Z reed@seanreed.ie
;;; $ 
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

;;; 28.12.10 for some reason make-slippery-chicken is crashing with this
;;; eval-when  
;;; (eval-when (compile)
;;;    (declaim (optimize (speed 3) (safety 3) (space 0) (debug 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :slippery-chicken)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; todo: add bar-lines field to indicate double bar lines etc.  This will
;;; simply call get-bar (getting all the players!) and change the bar-line-type
;;; of the bar accordingly. 

(defclass slippery-chicken (named-object)
  ((rthm-seq-palette :accessor rthm-seq-palette :initarg :rthm-seq-palette
                     :initform nil)
   (rthm-seq-map :accessor rthm-seq-map :initarg :rthm-seq-map :initform nil)
   ;; 24.3.11: method shorten-large-fast-leaps uses a max duration for a fast
   ;; note in seconds as a key arg, but it's hard to access as a user, so
   ;; specify it as part of the sc class.
   (fast-leap-threshold :accessor fast-leap-threshold 
                        :initarg :fast-leap-threshold :initform 0.125)
   ;;****S* slippery-chicken/rthm-seq-map-replacements 
   ;; NAME
   ;; rthm-seq-map-replacements 
   ;; These are replacements for the map (which may be generated
   ;; algorithmically); they take the form (section-reference
   ;; sequence-number (1-based) replacement) e.g.
   ;;  '(((1 1 vla) 2 20a) ((1 1 vla) 3 1a) ((1 1 vla) 4 9a) ((1 1 vla) ...
   ;; ****
   (rthm-seq-map-replacements :accessor rthm-seq-map-replacements 
                              :initarg :rthm-seq-map-replacements
                              :initform nil)
   (pitch-seq-map :accessor pitch-seq-map :initform nil)
   (set-palette :accessor set-palette :initarg :set-palette :initform nil)
   (set-map :accessor set-map :initarg :set-map :initform nil)
   (set-map-replacements :accessor set-map-replacements 
                         :initarg :set-map-replacements :initform nil)
   (hint-pitches :accessor hint-pitches :initarg :hint-pitches :initform nil)
   (ensemble :accessor ensemble :initarg :ensemble :initform nil)
   ;; when players play more than one instrument this is the change-map where
   ;; the changes of instrument are given (using sequence not bar numbers).
   ;; N.B. instruments cannot be changed mid-sequence, only between
   ;; sequences!!!  When a player plays more than one instrument, then the
   ;; instrument for bar one must be given.
   (instrument-change-map :accessor instrument-change-map :initarg
                          :instrument-change-map :initform nil)
   ;; this can contain bar references or bar numbers and looks something like
   ;; '((1 (q 160)) (200 (q 120 "meno mosso")))
   ;; where 1 is the bar, q is the beat and 160 is the bpm
   (tempo-map :accessor tempo-map :initarg :tempo-map :initform nil)
   ;; 24.1.11 we can also specify a tempo curve and this will generate the
   ;; tempo-map for us.  The x scale will be fitted to the number of bars and a
   ;; new tempo will be written every few bars, as indicated in the first
   ;; argument in the list; the second element is the beat rhythm e.g. 
   ;; '(10 q (0 60 100 120))
   ;; NB the curve should start at 0 but the map will start at 1
   (tempo-curve :accessor tempo-curve :type list :initarg :tempo-curve 
                :initform nil)
   ;; this contains the instrument definitions referenced in the ensemble.
   (instrument-palette :accessor instrument-palette
                       :initarg :instrument-palette :initform nil)
   (instruments-write-bar-nums :accessor instruments-write-bar-nums
                               :type list :initarg :instruments-write-bar-nums
                               :initform nil)
   ;; when choosing notes for instruments we take into account notes that other
   ;; instruments are already playing--this limits the available notes.  So the
   ;; order in which the instruments are allocated notes is important: specify
   ;; the order you want in this slot; the default is simply the list of
   ;; players in (players (ensemble sc))
   (instruments-hierarchy :accessor instruments-hierarchy :type list
                          :initarg :instruments-hierarchy :initform nil)
   ;; in the case of the sndfile-palette, we put the palette first in a list,
   ;; the paths second and the extensions third
   (snd-output-dir :accessor snd-output-dir
                   :initarg :snd-output-dir :initform "~/snd/")
   (sndfile-palette :accessor sndfile-palette :initarg :sndfile-palette
                    :initform nil)
   (bars-per-system-map :accessor bars-per-system-map 
                        :initarg :bars-per-system-map :initform nil)
   ;; where the braces and bar lines should be drawn, starting from the top of
   ;; the score.  i.e. (3 2 7) means the first three instruments are grouped,
   ;; then the next 2, then the next 7.  This is checked at initialization to
   ;; add up to the number of players in the ensemble.
   ;; *** N.B. This overides the bar-line-writers slot of the ensemble!
   (staff-groupings :accessor staff-groupings :type list 
                    :initarg :staff-groupings :initform nil)
   (piece :accessor piece :initform nil)
   ;; ties to beginnings of rthm-seqs are generally not allowed so
   ;; warnings are issued when they're detected; however in some cases
   ;; they're desirable so turn warnings on or off.
   ;; 15/7/05 now obsolete but leave it for backwards compatibility
   (warn-ties :accessor warn-ties :type boolean :initarg :warn-ties
              :initform t)
   ;; the following two slots allow for defining ranges for players across
   ;; the whole piece.  E.g. :set-limits-high '((vibes (0 g4 100 f6))) will
   ;; limit the notes selected from the sets to those below g4 at the beginning
   ;; but up to f6 by the end.  Inbetween there is interpolation.  The x-axis
   ;; can be any arbitrary range.  Y values can be note names or midi note
   ;; numbers.  These always refer to sounding pitches.
   ;; NB if a curve is given with the id 'all', then this will act as a global
   ;; curve and be applied to all instruments, overriding their individual
   ;; curves. Also, though the X axes of these curves can have arbitrary
   ;; ranges, they will be scaled to conform to the number of sequences in the
   ;; piee and interpolated once per sequence; so if you need precision it's
   ;; actually better to use sequence numbers as X values, not bars.
   (set-limits-high :accessor set-limits-high :initarg :set-limits-high 
                    :initform nil)  
   (set-limits-low :accessor set-limits-low :initarg :set-limits-low 
                   :initform nil)  
   ;; 31.1.11: this title will be used in lilypond file names so it's perhaps
   ;; best to avoid spaces etc.
   (title :accessor title :initarg :title :initform "slippery-chicken-piece")
   ;; 10/3/07: simply a list of bar numbers where a rehearsal letter should be
   ;; written (automatically)
   (rehearsal-letters :accessor rehearsal-letters :type list 
                      :initarg :rehearsal-letters :initform nil)
   ;; 1/4/06: this is the number of sections __and__ subsections
   (num-sections :accessor num-sections :type integer :initform -1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize-instance :after ((sc slippery-chicken) &rest initargs)
  (declare (ignore initargs)
           (special +slippery-chicken-standard-instrument-palette+))
  (let ((given-tempo-map (tempo-map sc)))
    (flet ((make-name (name) (format nil "~a-~a" (id sc) name)))
      (setf (instrument-palette sc)
            (cond ((instrument-palette-p (instrument-palette sc))
                ;; clone objects if they're already initialised outside of
                ;; make-slippery-chicken, so that any changes we make here
                ;; don't affect the outside objects.
                   (clone (instrument-palette sc)))
                  ((instrument-palette sc)
                   (make-instrument-palette
                    (make-name 'instrument-palette)
                    (instrument-palette sc)))
                  ;; 15.11.11 if none given, use the standard palette
                  (t +slippery-chicken-standard-instrument-palette+))
            (ensemble sc)
            (if (ensemble-p (ensemble sc))
                (clone (ensemble sc))
                (apply #'make-ensemble 
                       ;; got to do the apply to make sure we use the key
                       ;; arguments, if any
                       (append
                        (cons (make-name 'ensemble)
                              (ensemble sc))
                        (list :instrument-palette
                              (instrument-palette sc)))))
            (instrument-change-map sc)
            (if (instrument-change-map-p
                 (instrument-change-map sc))
                (clone (instrument-change-map sc))
                (make-instrument-change-map
                 (make-name 'instrument-change-map)
                 (instrument-change-map sc)))
            (set-palette sc)
            (if (set-palette-p (set-palette sc))
                (clone (set-palette sc))
                (make-set-palette (make-name 'set-palette)
                                  (set-palette sc))))
      ;; don't just make a set-map with nil!!!!
      (when (set-map sc)
        (setf (set-map sc) 
              (clone-with-new-class ;; 11.3.10 set-map is its own class now
               (if (sc-map-p (set-map sc))
                   (let ((clone (clone (set-map sc))))
                     (setf (replacements clone) (set-map-replacements sc))
                     clone)
                   (make-sc-map (make-name 'set-map)
                                (set-map sc) 
                                :replacements 
                                (set-map-replacements sc)
                                :recurse-simple-data nil))
               'set-map)))
      (unless (sc-map-p (set-map sc))
        (error "~a~%slippery-chicken::initialize-instance:~%~
                    Cannot proceed: set map is either nil or not a set map!"
               (set-map sc)))
      ;; 29/3/10: it's not ok to have nil references in the set-palette
      (link (set-map sc) (set-palette sc) nil)
      (link-named-objects (set-palette sc))
      (link-named-objects (set-map sc))
      (check-first-bar-ins-for-doubling-players (ensemble sc)
                                                (instrument-change-map sc)
                                                (this (get-first 
                                                       (set-map sc))))
      (setf (snd-output-dir sc) (trailing-slash (snd-output-dir sc))
            (rthm-seq-palette sc)
            (if (rsp-p (rthm-seq-palette sc))
                (clone (rthm-seq-palette sc))
                (make-rsp (make-name 'rthm-seq-palette)
                          (rthm-seq-palette sc)))
            (rthm-seq-map sc)
            (if (rthm-seq-map-p (rthm-seq-map sc))
                (let ((clone (clone (rthm-seq-map sc))))
                  (setf (replacements clone) (rthm-seq-map-replacements sc))
                  clone)
                (make-rthm-seq-map 
                 (make-name 'rthm-seq-map)
                 (rthm-seq-map sc)
                 :recurse-simple-data nil
                 :replacements 
                 (rthm-seq-map-replacements sc))))
      ;; it's ok to have nil in the rthm-seq-maps of course
      (link (rthm-seq-map sc) (rthm-seq-palette sc))
      (check-instruments sc)
      (check-maps (set-map sc)
                  (rthm-seq-map sc))
      (setf (pitch-seq-map sc)
            (if (sc-map-p (pitch-seq-map sc))
                (clone (pitch-seq-map sc))
                (generate-pitch-sequence-map (rthm-seq-map sc) sc))
            (hint-pitches sc)
            (if (change-map-p (hint-pitches sc))
                (clone (hint-pitches sc))
                (make-change-map (make-name 'hint-piches) 
                                 t
                                 (hint-pitches sc))))
      (let ((sfp (sndfile-palette sc)))
        (setf (sndfile-palette sc)
              (if (sndfile-palette-p sfp)
                  (clone sfp)
                  (make-sfp (make-name 'sound-file-palette)
                            (first sfp) 
                            :paths (second sfp)
                            :extensions (third sfp)))))
      (setf (num-sections sc) (count-section-refs (set-map sc)))
      (handle-set-limits sc)
      ;; (print (set-limits-low sc))
      ;; we have a chicken before the egg situation here: we can't
      ;; create a tempo-map without a piece because we need
      ;; reference to the bar numbers, but we can't create a piece
      ;; without a tempo-map because we need the tempi to calculate
      ;; start times.  As a solution, create a piece with a
      ;; temporary tempo-map of qtr=60 then update it later.
      (setf (tempo-map sc) '((1 60))
            (piece sc) (sc-make-piece sc (warn-ties sc))
            ;; map might be nil as we have a curve instead so handle this
            (tempo-map sc) (tempo-curve-to-map given-tempo-map (tempo-curve sc)
                                               (num-bars sc))
            ;; this calls the setf method so it's not as useless as it
            ;; looks.  
            (bars-per-system-map sc) (bars-per-system-map sc)))
    (linked (rthm-seq-map sc))
    (link-named-objects (rthm-seq-map sc))
    (let ((sg (staff-groupings sc)))
      (if sg
          (unless 
              (and (listp sg)
                   (= (num-players (ensemble sc))
                      (loop for i in sg do 
                           (unless (integer>0 i)
                             (error 
                              "slippery-chicken::initialize-instance:~
                              staff-groupings should be a list of ~
                              integers: ~a"
                              sg))
                           sum i)))
            (error "slippery-chicken::initialize-instance: ~%~
                  staff-groupings should be a list of integers summing ~
                  to the number ~%of instruments in the ensemble:  ~a"
                   sg))
          ;; 10.11.11: if no staff-groupings given, just make the whole ensemble
          ;; one big group 
          (setf (staff-groupings sc) (list (num-players (ensemble sc))))))
    ;; the order of players in the piece (from rthm-seq-map) is alphabetical,
    ;; but we want them as given in the ensemble...
    (setf (players (piece sc)) (players (ensemble sc)))
    ;; make a double bar at end of piece
    (change-bar-line-type sc (num-bars (piece sc)) 2)
    ;; have to call this again now that we've got the real tempo-map
    (update-slots sc (tempo-map sc) 0.0 0.0 1 nil nil (warn-ties sc))
    ;; (print (get-data 1 (set-palette sc)))
    ;; 25.3.11 the make-slippery-chicken function might set this to nil thus
    ;; overriding the class default 
    (unless (fast-leap-threshold sc)
      (setf (fast-leap-threshold sc) 0.125))
    (format t "~&Shortening short, fast leaps...")
    (format t "~&Shortened ~a large fast leaps"
            (shorten-large-fast-leaps sc :verbose nil))
    ;; make sure tempo changes get registered in midi output
    (update-events-tempo sc)
    ;; 28.1.11: can't believe we haven't done this before
    (check-time-sigs sc)
    ;; 5.4.11
    (cleanup-rest-bars sc)
    (set-rehearsal-letters sc (get-groups-top-ins sc))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* slippery-chicken/clone
;;; FUNCTION
;;; Copy (clone) the instance and all data associated with the slippery-chicken
;;; object.  
;;; 
;;; ARGUMENTS:
;;; - the slippery-chicken object you want to copy/clone
;;; 
;;; RETURN VALUE: 
;;; a slippery-chicken object
;;; 
;;; EXAMPLE
;;; 
;;; 
;;; SYNOPSIS
(defmethod clone ((sc slippery-chicken))
;;; ****
  (clone-with-new-class sc 'slippery-chicken))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; todo: this isn't working for some strange reason...

(defmethod clone-with-new-class :around ((sc slippery-chicken) new-class)
  (declare (ignore new-class))
  (let ((no (call-next-method)))
    (setf (slot-value no 'rthm-seq-palette) (clone (rthm-seq-palette sc))
          (slot-value no 'rthm-seq-map) (clone (rthm-seq-map sc))
          (slot-value no 'rthm-seq-map-replacements)
          (my-copy-list (rthm-seq-map-replacements sc))
          (slot-value no 'pitch-seq-map) (clone (pitch-seq-map sc))
          (slot-value no 'set-palette) (clone (set-palette sc))
          (slot-value no 'set-map) (clone (set-map sc))
          (slot-value no 'set-map-replacements)
          (slot-value no 'fast-leap-threshold) (fast-leap-threshold sc)
          (my-copy-list (set-map-replacements sc))
          (slot-value no 'hint-pitches) (clone (hint-pitches sc))
          ;; (slot-value no 'ensemble) (clone (ensemble sc))
          (slot-value no 'instrument-change-map) 
          (clone (instrument-change-map sc))
          (slot-value no 'tempo-map) (clone (tempo-map sc))
          (slot-value no 'tempo-curve) (my-copy-list (tempo-curve sc))
          ;;(slot-value no 'instrument-palette) (clone (instrument-palette sc))
          (slot-value no 'instruments-write-bar-nums) 
          (copy-list (instruments-write-bar-nums sc))
          (slot-value no 'snd-output-dir) (snd-output-dir sc)
          (slot-value no 'sndfile-palette) (clone (sndfile-palette sc))
          (slot-value no 'bars-per-system-map) (clone (bars-per-system-map sc))
          (slot-value no 'staff-groupings) (copy-list (staff-groupings sc))
          (slot-value no 'piece) (clone (piece sc))
          (slot-value no 'title) (title sc)
          (slot-value no 'warn-ties) (warn-ties sc)
          (slot-value no 'set-limits-high) (my-copy-list (set-limits-high sc))
          (slot-value no 'set-limits-low) (my-copy-list (set-limits-low sc))
          (slot-value no 'rehearsal-letters) 
          (my-copy-list (rehearsal-letters sc))
          (slot-value no 'num-sections) (num-sections sc))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; attacked notes returned i.e. not including ties (or rests)
(defmethod num-notes ((sc slippery-chicken))
  (num-notes (piece sc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* slippery-chicken/change-bar-line-type
;;; FUNCTION
;;; change-bar-line-type:
;;; change single to double bar lines and vice-versa
;;; 
;;; ARGUMENTS:
;;; - the slippery-chicken object
;;; - the bar number at the end of which you want the bar line to change
;;; - bar line type: 0 = normal, 1 double bar, 2 final double bar
;;; RETURN VALUE: 
;;; always t 
;;; 
;;; SYNOPSIS
(defmethod change-bar-line-type ((sc slippery-chicken) bar-num type)
;;; ****
  (let ((players-bars (get-bar sc bar-num)))
    (loop for bar in players-bars do
      (setf (bar-line-type bar) type)))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* slippery-chicken/cmn-display
;;; FUNCTION
;;; cmn-display:
;;; Write the score as an EPS file using CMN. Caveat: this might fail if you
;;; generated Lilypond files first; if so, regenerated your slippery-chicken
;;; and re-call.
;;; 
;;; ARGUMENTS:
;;; - the slippery-chicken object
;;; - and lots of &key args: see synopsis
;;; 
;;; RETURN VALUE: 
;;; always t
;;; 
;;; SYNOPSIS
#+cmn
(defmethod cmn-display ((sc slippery-chicken) 
                       &key
                        ;; could also be a list of notes which should be
                        ;; changed to their enharmonics e.g.
                        ;; '((vln (13 2) (14 3) ...
                        ;;   (cl (14 3 t) ... ))
                        ;; which refers to the player then e.g. bar 13 note 2
                        ;; (1-based and counting tied notes but not rests)
                        ;; the t in the cl refers to changing the written note
                        ;; (default is to change the sounding note spelling).
                        (respell-notes t)
                        (start-bar nil)
                        ;; if nil will auto-number bars
                        (start-bar-numbering nil)
                        (end-bar nil)
                        (file "/tmp/cmn.eps")
                        (all-output-in-one-file t)
                        (one-line-per-page nil)
                        ;; this is the separation between lines within a group
                        (staff-separation 3)
                        ;; although there's a system-separation parameter for
                        ;; cmn, this is the one that actually seems to do that
                        ;; job...this is the separation between what I would
                        ;; call systems i.e. one line of music and the next.
                        (line-separation 5)
                        ;; whether an empty stave should be displayed under
                        ;; each instrument for filling in of notes/edits
                        ;; manually with pencil (!) 
                        (empty-staves nil)
                        ;; write the section number/refs into the score?
                        ;; NB This might not do what you want without
                        ;; regenerating the slippery-chicken object from
                        ;; scratch. 
                        (write-section-info t)
                        ;; separation for groups in a score line
                        (group-separation 2)
                        ;; more than one system could occur on a page, unless
                        ;; this is altered to cmn::page-mark 
                        ;; in CMN this is the gap between groups of instruments
                        ;; in one line of music.
                        (system-separation cmn::line-mark)
                        ;; this should be a user-defined function that takes
                        ;; one argument (an event object) and which will be
                        ;; called for each event in the piece. Could be used to
                        ;; algorithmically add accents, dynamics, or something
                        ;; along those lines. 
                        (process-event-fun nil)
                        (display-sets nil)
                        ;; by default, rehearsal letters are put over bar lines
                        ;; of those instruments at the top of each group.  If
                        ;; this is t, the letters will be put over all
                        ;; instruments (useful when writing parts).
                        (rehearsal-letters-all-players nil)
                        ;; some marks (e.g. text) are added to parts only,
                        ;; i.e. they're not in the main score 
                        (display-cmn-marks-in-part nil)
                        ;; similar to rehearsal-letters-all-players
                        (tempi-all-players nil)
                        (players nil)
                        (page-height 29.7)
                        (page-width 21.0)
                        (size 15)
                        ;; this seems to be separate from the bar-number
                        ;; written in every part every 5 bars, but if set to
                        ;; e.g. 1 it will print every bar num at the top of
                        ;; each system 
                        (auto-bar-nums :by-line)
                        (page-nums t)
                        ;; display the score in C (but piccolo/double bass keep
                        ;; the usual octave transpositions) or at written
                        ;; pitch? 
                        (in-c t)
                        (auto-clefs t)
                        (multi-bar-rests nil)
                        (automatic-octave-signs nil)
                        ;; whether to write time (mins:secs) on the first event
                        ;; of each bar.
                        (display-time nil)
                        ;; ps code to be added to the cmn-produced file after
                        ;; t has been written.  See add-ps-to-file below for
                        ;; details. 
                        (add-postscript nil))
;;; ****
  (when respell-notes
    (respell-notes sc respell-notes))
  (unless players 
    (setf players (players (ensemble sc))))
  (when rehearsal-letters-all-players 
    ;; todo: this is still not working ....
    (set-rehearsal-letters sc players))
  (when tempi-all-players 
    (update-events-tempo sc players))
  (when multi-bar-rests
    (multi-bar-rests sc players)
    (when (or start-bar end-bar)
      (warn "slippery-chicken:: cmn-display: when using multi-bar-rests the ~
             whole piece will be generated: ignoring start/end-bar!")))
  (when auto-clefs
    (format t "~&Inserting automatic clefs....")
    (auto-clefs sc :players players :verbose nil :in-c in-c
                :delete-cmn-objects-before nil))
  ;; 26/4/10: some processes turn notes into rests so turn bars of rests only
  ;; into rest-bars proper 
  (cleanup-rest-bars sc)
  (cmn-display (piece sc)
               :auto-bar-nums auto-bar-nums
               :start-bar start-bar
               :display-cmn-marks-in-part display-cmn-marks-in-part
               :start-bar-numbering start-bar-numbering
               :page-nums page-nums
               :group-separation group-separation
               :end-bar end-bar
               :multi-bar-rests multi-bar-rests
               :bars-per-system-map (bars-per-system-map sc)
               :ensemble (ensemble sc)
               :all-output-in-one-file all-output-in-one-file
               :one-line-per-page one-line-per-page
               :instrument-change-map (instrument-change-map sc)
               :system-separation system-separation
               :file file
               :process-event-fun process-event-fun 
               :set-map (when display-sets (set-map sc))
               :empty-staves empty-staves
               :staff-groupings (staff-groupings sc)
               :write-section-info write-section-info
               :staff-separation staff-separation
               :line-separation line-separation
               :players players
               :in-c in-c
               :page-height page-height
               :page-width page-width
               :automatic-octave-signs automatic-octave-signs
               :display-time display-time
               :size size)
  (when add-postscript
    (add-ps-to-file file add-postscript))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod convert-bar-refs-to-numbers ((sc slippery-chicken) map)
  ;; replace any bar references with the true bar numbers
  (loop 
      for pair in map
      for bar = (first pair)
      for i from 0 do
        ;; when the bar number is a reference (of the form (section
        ;; sequenz-num bar-num)), then get the bar number of that
        ;; reference and replace it before making the
        ;; simple-change-map 
        (when (listp bar)
          (setf (first (nth i map)) 
            (get-bar-num-from-ref sc (first bar) (second bar) (third bar)))))
  map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod (setf bars-per-system-map) (bpsm (sc slippery-chicken))
  (unless (simple-change-map-p bpsm)
    (setf bpsm (make-simple-change-map (format nil "~a-~a" (id sc) 
                                               'bars-per-system-map)
                                       (convert-bar-refs-to-numbers sc bpsm))))
  (setf (slot-value sc 'bars-per-system-map) bpsm)
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod (setf tempo-map) (tm (sc slippery-chicken))
  (setf tm (convert-bar-refs-to-numbers sc tm)
        (slot-value sc 'tempo-map)
        (make-tempo-map (format nil "~a-~a" (id sc) 'tempo-map)
                        tm)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod (setf set-map-replacements) :after (smr (sc slippery-chicken))
  (setf (replacements (set-map sc)) smr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod (setf rthm-seq-map-replacements) :after (rsmr (sc slippery-chicken))
  (setf (replacements (rthm-seq-map sc)) rsmr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod replace-tempo-map ((sc slippery-chicken) tm)
  (setf (tempo-map sc) tm)
  (update-events-tempo sc)
  (update-slots sc (tempo-map sc))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* slippery-chicken/add-event-to-bar
;;; FUNCTION
;;; add-event-to-bar:
;;;
;;; Add an event object to a bar either at the end of at the given position.
;;; 
;;; ARGUMENTS:
;;; - the slippery-chicken object
;;; - the new event object
;;; - the bar number or reference (of the form '(section sequence bar) where
;;;   sequence and bar are numbers counting from 1)
;;; - the player (symbol)
;;; - (key :position default nil): the position in the bar (0-based) where the
;;; event should be spliced; if nil then it's put at the end.
;;; 
;;; RETURN VALUE: 
;;; always t
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
;;; Replace events already in the parts with new events.  All references are
;;; 1-based.  Works for one bar at a time only.
;;; 
;;; ARGUMENTS:
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
;;; RETURN VALUE: 
;;; always t (from piece class method)
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
;;; replace-multi-bar-events:
;;;
;;; Replace events across several bars.  All bars have to be filled, i.e. we
;;; can't just leave the last bar half-filled expecting the existing events to
;;; make up the rest.
;;; 
;;; ARGUMENTS:
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
;;; RETURN VALUE: 
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

;;; Called by initialize-instance

(defmethod check-instruments ((sc slippery-chicken))
  (let* ((rsm-players (players (rthm-seq-map sc)))
         (ensemble-players (players (ensemble sc)))
         (ensemble-players-len (length ensemble-players))
         (hierarchy (instruments-hierarchy sc)))
    (if hierarchy
        (progn
          (unless (= (length hierarchy) (length (remove-duplicates hierarchy)))
            (error "slippery-chicken::check-instruments: instrument-hierarchy ~
                    contains duplicates!: ~a" hierarchy))
          (unless (= (length hierarchy) ensemble-players-len)
            (error "slippery-chicken::check-instruments: instrument-hierarchy ~
                    ~a must contain all the players of the ensemble ~a."
                   hierarchy ensemble-players))
          (loop for ins in hierarchy do
               (unless (member ins ensemble-players)
                 (error "slippery-chicken::check-instruments: ~
                          instrument-hierarchy ~a must contain all the ~
                          players of the ensemble ~a. You gave ~a"
                        hierarchy ensemble-players ins))))
        (setf (instruments-hierarchy sc) (copy-list ensemble-players)))
    (unless (= (length rsm-players) ensemble-players-len)
      (warn "slippery-chicken::check-instruments: ~%Number of instruments ~
             in the rthm-seq-map is not the same ~%as that in the ensemble!"))
    (loop for bw in (instruments-write-bar-nums sc)
       unless (member bw ensemble-players) do
       (error "slippery-chicken::check-instruments:  ~
                  instruments-write-bar-nums contains reference to ~
                  instrument not in ensemble: ~a ~a" bw ensemble-players))
    (loop for rsmp in rsm-players 
       unless (member rsmp ensemble-players) do
       (error "slippery-chicken::check-instruments: rthm-seq-map player ~a ~
                  not defined in ensemble" rsmp))
    t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Called by initialize-instance

;;; Clone the rthm-seq-map, loop through all the instruments in all the
;;; sections of the rsm (get-first then thereafter use the next slot).  Get the
;;; list of rthm-seq refs, pass this with the section ref and the instrument to
;;; make-section-for player.  In the rms-clone, replace the data of the current
;;; element with a section which is a list of sequenzs made by promoting the
;;; rthm-seq to a sequenz.

;;; Go through each and every bar for each instrument and update the
;;; write-time-sig slot of each rthm-seq-bar.

;;; Go through each event of each rthm-seq-bar and update compound-duration to
;;; reflect tempo.  Then call handle-first-note-ties for each rthm-seq (to
;;; update compound durations of tied first notes of a bar dependent upon tempo
;;; also--a tempo could change over a tie between bars). 

;;; Go through each event and update start-time based on compound duration
;;; (only when the event has need-new-note).

(defmethod sc-make-piece ((sc slippery-chicken) &optional (warn-ties t))
  (let* ((rsm-clone (link-named-objects (clone (rthm-seq-map sc)))))
    (setf rsm-clone (sc-change-class rsm-clone 'piece))
    (rsm-to-piece rsm-clone sc)
    (setf (id rsm-clone) (format nil "~a-piece" (id sc))
          ;; the players from the rthm-seq-map might be in a different order
          ;; from those of the ensemble so copy the latter. 
          (players rsm-clone) (players (ensemble sc)))
    (add-rest-player-sections rsm-clone)
    (add-rest-sequenzes rsm-clone)
    (update-slots rsm-clone (tempo-map sc) 0.0 0.0 1 nil nil warn-ties)
    (handle-ties rsm-clone)
    ;; well, stupid, but we have to do this again because of the
    ;; ties.  First we need to do it to get bar numbers etc. for the
    ;; sequenzes, then we have to do it after the handle-ties to make
    ;; sure the compound-durations take tempo into consideration
    (update-slots rsm-clone (tempo-map sc) 0.0 0.0 1 nil nil warn-ties)
    (update-write-time-sig rsm-clone t)
    ;; 28.1.11 use -sig2 also to make sure each bar is really checked.
    (update-write-time-sig2 rsm-clone t)
    rsm-clone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Called by initialize-instance and others.

(defmethod update-slots ((sc slippery-chicken) 
                         &optional
                         (tempo-map nil)
                         (start-time 0.0)
                         (start-time-qtrs 0.0)
                         (start-bar 1)
                         (current-section nil)
                         (nth nil)
                         (warn-ties t))
  (update-slots (piece sc) 
                (if tempo-map tempo-map (tempo-map sc))
                start-time start-time-qtrs start-bar current-section nth
                warn-ties))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Called by rsm-to-piece (function)

;;; Loop through each rthm-seq reference, get the rthm-seq, pull out the
;;; associated pitch-seq from the pitch-seq-map (using the 'this' ref of the
;;; instrument as lookup into the psm).  Get the harmony for this seq, the
;;; hint-pitch, and for each rthm-seq call sc-make-sequenz

;;; In order to be able to limit sets, we have to keep count of the number of
;;; sequences as we proces them.  This has to be done on an instrument by
;;; instrument basis however, as the piece is not generated seq by seq (all
;;; instruments in one go).  So seq-count becomes an assoc-list with each
;;; player as keys and the data being the count of sequences.

(let ((seq-count nil))
  (defmethod make-section-for-player (lno (sc slippery-chicken) 
                                      &optional
                                      ;; this is the last event of the previous
                                      ;; section (which could be nil)
                                      last-event
                                      ;; this is the last pitch we saw (could
                                      ;; be in a previous section)
                                      last-pitch-seen)
    (flet ((inc-seq-count (player-ref)
             (let ((p (get-data player-ref seq-count nil)))
               (if p
                   (incf (data p)) ;; seq-count 1-based
                 (add (list player-ref 1) seq-count)))))
      (if (not (or lno last-event))
          (setf seq-count (make-assoc-list nil nil))
        (let* ((rthm-seq-refs (data lno))
               (player-section-ref (this lno))
               (player-ref (first (last player-section-ref)))
               (section-ref (butlast player-section-ref))
               (last-note-previous-seq last-event)
               (last-pitch last-pitch-seen)
               (player-section (clone-with-new-class lno 'player-section)))
          (setf (data player-section)
            (loop for rsr in rthm-seq-refs and seq-num from 0 do
                  (inc-seq-count player-ref)
                collect
                  ;; collect nil when the player sits this seq out
                  (when rsr
                    (let* ((rs (get-data rsr (rthm-seq-palette sc)))
                           (pitch-seq (get-nth-from-map player-section-ref
                                                        seq-num
                                                        (pitch-seq-map sc)))
                           (set (get-nth-from-palette section-ref seq-num
                                                      (set-map sc)))
                           hint-pitch seq)
                      (multiple-value-bind
                          ;; have to get the second values result here so we
                          ;; know whether to trigger a program-change or not
                          (instrument instrument-change)
                          (get-current-instrument-for-player 
                           section-ref player-ref (1+ seq-num) sc)
                        #|
                        (when instrument-change
                          (format t "~&ins-change! seq ~a, ~a" seq-num
                          instrument))
                          |#
                        (setf hint-pitch (cm-get-data (hint-pitches sc)
                                                      player-section-ref)
                              seq
                              (sc-make-sequenz rs instrument set pitch-seq
                                               hint-pitch player-ref 
                                               player-section-ref seq-num 
                                               last-note-previous-seq
                                               sc (get-data-data player-ref
                                                                 seq-count)
                                               instrument-change last-pitch)
                              last-note-previous-seq (get-last-event seq))
                        (when (pitch-or-chord last-note-previous-seq)
                          (setf last-pitch (pitch-or-chord 
                                            last-note-previous-seq))))
                      seq))))
          player-section)))))
          
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Called by get-set-limits method
(defmethod get-set-limit-high ((sc slippery-chicken) player seq-num)
  (get-set-limit-aux (set-limits-high sc) player seq-num))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Called by get-set-limits method
(defmethod get-set-limit-low ((sc slippery-chicken) player seq-num)
  (get-set-limit-aux (set-limits-low sc) player seq-num))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* slippery-chicken/num-bars
;;; FUNCTION
;;; num-bars:
;;;
;;; Return the number of bars in the piece.
;;; 
;;; ARGUMENTS:
;;; - the slippery-chicken object
;;; 
;;; RETURN VALUE: 
;;; The number of bars (integer).
;;; 
;;; SYNOPSIS
(defmethod num-bars ((sc slippery-chicken))
;;; ****
  (num-bars (piece sc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* slippery-chicken/get-bar-from-ref
;;; FUNCTION
;;; get-bar-from-ref:
;;;
;;; Return a rthm-seq-bar object from the piece.  Sequenz-num and bar-num are
;;; 1-based. 
;;; 
;;; ARGUMENTS:
;;; - the slippery-chicken object
;;; - the section reference/id (symbol, number, list)
;;; - the player (symbol)
;;; - the sequence number in the section (integer, counting from 1)
;;; - the bar number in the sequence (integer, counting from 1)
;;; 
;;; RETURN VALUE: 
;;; the rthm-seq-bar object
;;; 
;;; SYNOPSIS
(defmethod get-bar-from-ref ((sc slippery-chicken) section player
                             sequenz-num bar-num)
;;; ****
  (get-bar-from-ref (piece sc) section player sequenz-num bar-num))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; sequenz-num and bar-num are 1-based.

(defmethod get-bar-num-from-ref ((sc slippery-chicken) section
                                 sequenz-num bar-num)
   (get-bar-num-from-ref (piece sc) section sequenz-num bar-num))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; N.B. although optional, the player argument is required.  It is optional so
;;; that we can have a sequenz method with the same name which only requires
;;; the bar-num argument. 

;;; 15/3/03: change this so that if player is nil, then we get the bar for all
;;; players in the ensemble. 

(defmethod get-bar ((sc slippery-chicken) bar-num &optional player)
  ;; (unless player
  ;; (error "bar-holder::get-bar: player argument is required!"))
  (if player
      (get-bar (piece sc) bar-num player)
    (let ((players (players (ensemble sc))))
      (loop for p in players collect (get-bar (piece sc) bar-num p)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod auto-accidentals ((sc slippery-chicken) &optional ignore1 ignore2)
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

(defmethod respell-notes ((sc slippery-chicken) &optional corrections)
  (format t "~&Respelling notes...")
  ;; this respells written and sounding notes if transposing instrument
  (respell-notes-aux sc)
  ;; 10/5/07: a second pass does pick up some more mistakes...
  (respell-notes-aux sc (when (listp corrections) corrections)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This will only work for single notes, not chords.

(defmethod respell-notes-aux ((sc slippery-chicken) &optional corrections)
  (loop for player in (players sc) do
        ;; (print player)
        (respell-notes-for-player sc player)
        ;; we can't ignore instruments that only sound octaves +/- written note
        ;; as that would leave written and sounding notes potentially
        ;; different. 
        (when (plays-transposing-instrument (get-player sc player) nil)
          (respell-notes-for-player sc player t)))
  (respell-bars sc)
  ;; we're really doing this only for the effect of having all ties go to the
  ;; same note instead of enharmonics
  ;; (print 'check-ties)
  (check-ties sc t)
  ;; (print 'corrections)
  (when (and corrections (listp corrections))
    (enharmonic-spellings sc corrections))
  ;; have to do it here to correct some ties to notes with accidentals
  (auto-accidentals sc)
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 11.4.11: start and end can be bar numbers or (bar note) pairs where note is
;;; 1-based and counts ties.
(defmethod enharmonics ((sc slippery-chicken) start end player
                        &optional (written t))
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

;;; e.g. (enharmonic-spellings +coming-rthm-chain+
;;;                    '((cello (117 1) (118 2) (135 3) (591 (1 2)) (596 (2 2)))
;;;                      (violin (539 5))
;;;                      (clarinet (1 2 t))
;;;                      (flute (204 1))))
;;; where (596 (1 2)) is accessing the second chord note (counting from high to
;;;                    low) of the first sounding event of bar 596
;;; (clarinet (1 2 t)): the t means change the written note, not sounding
;;; NB Designed to be called from cmn-display but can be called by user.

(defmethod enharmonic-spellings ((sc slippery-chicken) corrections)
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
(defmethod respell-bars ((sc slippery-chicken))
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

;;; ****m* slippery-chicken/count-notes
;;; FUNCTION
;;; count-notes:
;;;
;;; Returns the number of notes between start-bar and end-bar (both inclusive).
;;; 
;;; ARGUMENTS:
;;; - the slippery-chicken object
;;; - start-bar (integer)
;;; - end-bar (integer)
;;; - (optional just-attacks default nil): whether to count just the number of
;;; notes that need new events or the number of notes in the score. NB a chord
;;; counts as one note only.
;;; - (optional players default nil): the players whose notes we want to count;
;;; can be a single symbol or a list of players; if nil all players will be
;;; counted. 
;;; 
;;; RETURN VALUE: 
;;; the number of notes (integer)
;;; 
;;; EXAMPLE
;;; (count-notes +altogether+ 416 417 nil 'pno-rh) -> 10
;;; 
;;; SYNOPSIS
(defmethod count-notes ((sc slippery-chicken) start-bar end-bar 
                        &optional just-attacks players)
;;; ****
  (unless players
    (setf players (players sc)))
  (unless (listp players)
    (setf players (list players)))
  (loop 
      with count = 0
      for bnum from start-bar to end-bar do
        (loop 
            for player in players 
            for bar = (get-bar sc bnum player)
            do
              (incf count (if just-attacks 
                              (notes-needed bar)
                            (num-score-notes bar))))
      finally (return count)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 9.4.11: note can be a single pitch or a chord (list). start and end bar are
;;; inclusive.  
(defmethod find-note ((sc slippery-chicken) player note &key (written nil)
                      start-bar end-bar)
  (unless start-bar
    (setf start-bar 1))
  (unless end-bar
    (setf end-bar (num-bars sc)))
  (next-event sc player nil start-bar)
  (let* ((chord? (listp note))
         (find (if chord? (make-chord note) (make-pitch note)))
         p)
    (loop for e = (next-event sc player) while e do
         (when (> (bar-num e) end-bar)
           (return))
         (setf p (if written (written-pitch-or-chord e) (pitch-or-chord e)))
         (when (and p (if chord?
                          (and (chord-p find) (chord-p p) (chord-equal find p))
                          (and (pitch-p find) (pitch-p p) (pitch= find p))))
           (format t "~&bar ~a" (bar-num e))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This is just a very simple attempt to spell notes better by comparing each
;;; note to the previous one and making it the same accidental type.  It
;;; doesn't look further back or ahead as of yet.  If <written> then look at
;;; the written notes instead of the sounding notes.
;;;
;;; 8/4/07: keep track of the last two now in order to make better decisions.

(defmethod respell-notes-for-player ((sc slippery-chicken) player 
                                     &optional written)
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
           ((and (chord-p last) (is-single-pitch this))
            ;; todo: we should make sure a single pitch after a chord uses
            ;; the same spellings as the chord if the pitch was in the chord
            ))
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

(defmethod players ((sc slippery-chicken))
  (players (piece sc)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((current-bar nil)
      (current-bar-num 1)
      (current-event-num 0))
  ;; ****m* slippery-chicken/next-event
  ;; FUNCTION
  ;; next-event:
  ;;
  ;; Get the events from the piece on after the other (e.g. in a loop).  This
  ;; needs to be called with a bar number the first time to reset; this will
  ;; return nil, after which calling without a bar number will return the
  ;; events. 
  ;; 
  ;; ARGUMENTS:
  ;; - the slippery-chicken object
  ;; - the player (symbol)
  ;; - (optional default nil): whether to return only notes that need attacks or
  ;; tied notes too
  ;; - (optional default nil): the bar to start at (number, see above); this
  ;; should be nil after the first call to reset; when it's nil when we
  ;; actually get events returned.
  ;; - (optional default nil): the end bar (number)
  ;; 
  ;; RETURN VALUE: 
  ;; 
  ;; 
  ;; EXAMPLE
  ;; 
  ;; 
  ;; SYNOPSIS
  (defmethod next-event ((sc slippery-chicken) player 
                         &optional
                         (attacked-notes-only nil)
                         ;; could be a number too, whereupon it's the bar
                         ;; number to start at  
                         (start-over nil)
                         (end-bar nil)) ; inclusive
;;; **** 
    (cond (start-over
           (setf current-bar-num (if (integer>0 start-over)
                                     start-over
                                     1)
                 current-bar nil
                 current-event-num 0)
           nil)
          ;; another hacked trick: if attack-notes-only is a
          ;; number, inc current-even-num by that amount
          ;; this of course means attacked-notes-only no longer works so we're
          ;; just dealing with any old events...
          ((numberp attacked-notes-only)
           (unless current-bar
             (error "slippery-chicken::next-event: no current-bar!"))
           (loop 
              with direction = (if (< attacked-notes-only 0)
                                   -1
                                   1)
              repeat (abs attacked-notes-only) do
              (cond ((and (= direction -1)
                          (zerop current-event-num))
                     (decf current-bar-num)
                     (setf current-bar nil
                           current-event-num (1- (num-rhythms 
                                                  current-bar))))
                    ((and (= direction 1)
                          (= current-event-num (num-rhythms current-bar)))
                     (decf current-event-num))
                    (t (incf current-event-num direction)))))
          ;; if player is nil just return the bar num: useful for debugging.
          ((not player) current-bar-num)
          ;; the usual case:
          ((<= current-bar-num (if end-bar
                                   end-bar
                                   (num-bars (piece sc))))
           (unless current-bar
             (setf current-bar (get-bar sc current-bar-num player)
                   current-event-num 0))
           (unless current-bar
             (error "slippery-chicken::next-event: no bar number ~a!" 
                    current-bar-num))
           (if (< current-event-num (num-rhythms current-bar))
               (progn
                 (let ((result (nth current-event-num (rhythms current-bar))))
                   (incf current-event-num)
                   (if (and attacked-notes-only
                            (not (needs-new-note result)))
                       (next-event sc player attacked-notes-only nil end-bar)
                       result)))
               (progn
                 (setf current-bar nil
                       current-event-num 0)
                 (incf current-bar-num)
                 (next-event sc player attacked-notes-only nil end-bar)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-note ((sc slippery-chicken) bar-num note-num player 
                     &optional written)
  (get-note (piece sc) bar-num note-num player written))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-rest ((sc slippery-chicken) bar-num rest-num player)
  (get-rest (piece sc) bar-num rest-num player))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* slippery-chicken/get-event
;;; FUNCTION
;;; get-event:
;;;
;;; 
;;; 
;;; ARGUMENTS:
;;; 
;;; 
;;; RETURN VALUE: 
;;; 
;;; 
;;; EXAMPLE
;;; 
;;; 
;;; DATE
;;; 
;;; 
;;; SYNOPSIS
(defmethod get-event ((sc slippery-chicken) bar-num event-num player)
;;; ****
  (get-event (piece sc) bar-num event-num player))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; NB The new note is the sounding pitch if a transposing instrument.
(defmethod change-note ((sc slippery-chicken) bar-num note-num player new-note)
  (change-note (piece sc) bar-num note-num player new-note))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 30.3.11: turn a rest into a note by supplying a pitch or chord (as objects
;;; or symbols)
(defmethod rest-to-note ((sc slippery-chicken) bar-num rest-num player new-note
                         &rest marks)
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

;;; ****m* slippery-chicken/change-notes
;;; FUNCTION
;;; change-notes:
;;;
;;; ARGUMENTS:
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
;;; RETURN VALUE: 
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
           (rhythm-add-cmn-marks e (nth count marks)))
        ;; this hack gets the current bar number so we return where we left off
        (next-event sc nil))
      ;; the bar-holder method
      (change-notes (piece sc) player start-bar new-notes use-last-octave)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod change-time-sig ((sc slippery-chicken) bar-num-or-ref new-time-sig)
  (change-time-sig (piece sc) bar-num-or-ref new-time-sig))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod add-cmn-mark-to-note ((sc slippery-chicken)
                                 bar-num note-num player mark)
  (add-cmn-mark-to-note (piece sc) bar-num note-num player mark))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 1-based
(defmethod add-cmn-mark-to-event ((sc slippery-chicken) bar-num event-num player
                                  mark)
  (add-cmn-mark-to-event (piece sc) bar-num event-num player mark))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 28.2.11  event-num can be an integer (1-based) or a list of event numbers 1
;;; for each instrument counting from the top of the score down
(defmethod add-cmn-mark-all-players ((sc slippery-chicken)
                                     bar-num event-num mark)
  (if (listp event-num)
      (unless (= (num-players (piece sc)) (length event-num))
        (error "slippery-chicken::add-cmn-mark-all-players: event-num list ~
                must contain a number for each player: ~a" event-num))
      (setf event-num (ml event-num (num-players (piece sc)))))
  (loop for player in (players sc) and enum in event-num do
       ;; (format t "~%~a ~a ~a ~a" bar-num enum player mark)
       (add-cmn-mark-to-event (piece sc) bar-num enum player mark))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod note-add-bracket-offset ((sc slippery-chicken)
                                    bar-num note-num player
                                    &key (dx nil) (dy nil) 
                                    (dx0 nil) (dy0 nil) 
                                    (dx1 nil) (dy1 nil) 
                                    (index 0))
  (let ((event (get-note (piece sc) bar-num note-num player)))
    (add-bracket-offset event :dx dx :dy dy :dx0 dx0 :dy0 dy0 :dx1 dx1 :dy1 dy1
                        :index index)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A cmn mark can only be applied to one note, after that, the mark will not
;;; display, so instead of storing the same mark in several notes, pass a
;;; function that creates a cmn-mark, and call that function to create a
;;; separate instance of the cmn mark for each note that it should be applied
;;; to. 
;;; 1.3.11 as cmn-marks are now all symbols, this is obsolete
#|
(defmethod add-cmn-mark-to-notes ((sc slippery-chicken) mark-function player
                                  notes)
  (loop 
      for bar in notes 
      for bar-num-or-ref = (first bar)
      for notes = (rest bar)
      do
        (loop for n in notes do
              (add-cmn-mark-to-note sc bar-num-or-ref n player 
                                    (funcall mark-function))))
  t)
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Similar to above but whereas you usually give the notes like '((1 1) (2 2))
;;; meaning bar 1 note 1, bar 2 note 2, here you give it in the form 
;;; '((1 1 5) (3 2 7)) meaning bar 1, notes 1 to 5 inclusive, bar 3, notes 2 to
;;; 7 inclusive. 

(defmethod add-cmn-mark-to-notes-from-to ((sc slippery-chicken)
                                          mark-function player
                                          notes)
  (loop 
      for bar in notes 
      for bar-num-or-ref = (first bar)
      for notes = (rest bar)
      with expansion
      do
      (unless (= (length notes) 2)
        (error "slippery-chicken::add-cmn-mark-to-notes-from-to: ~
                Each bar is a 3 note list: bar-num start-note end-note: ~a"
               bar))
      (setf expansion (loop for i from (first notes) to (second notes) 
                        collect i))
      (loop for n in expansion do
        (add-cmn-mark-to-note sc bar-num-or-ref n player 
                              (funcall mark-function))))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 1.3.11 another method for adding marks to multiple notes, this time we give
;;; a start-bar/note and an end-bar/note and the given marks will be added to
;;; all inbetween.  start and finish are inclusive and 1-based.  If they're
;;; integers then all notes in the bars will be marked, otherwise a 2-element
;;; list sets the exact note to start/stop at.  NB noteheads need before to be
;;; t in lilypond but bear in mind they're automatically moved over in
;;; event::get-lp-data.  players can be a single symbol or list.
(defmethod add-cmn-marks-to-notes ((sc slippery-chicken) start end players before
                                   &rest marks)
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
                                         #'add-cmn-object-before-note
                                         #'add-cmn-mark-to-note)
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

(defmethod add-marks ((sc slippery-chicken) player-data 
                      &key shorthand (warn t) verbose)
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
          (add-cmn-marks-to-note sc bar note p mark)
          (setf bar nil
                note nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod add-cmn-marks-to-note ((sc slippery-chicken) bar-num note-num
                                  player &rest marks)
  ;; make sure we have a flat list and no sublists as perhaps created by
  ;; cmn::get-cmn-marks 
  (setf marks (flatten marks))
  ;; (print marks)
  (let ((note (get-note (piece sc) bar-num note-num player)))
    (when note
      (loop for mark in marks do
            (add-cmn-mark note mark))))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod rm-cmn-marks-from-note ((sc slippery-chicken) bar-num note-num
                                   player &rest marks)
  ;; make sure we have a flat list and no sublists as perhaps created by
  ;; cmn::get-cmn-marks 
  (setf marks (flatten marks))
  (let ((note (get-note (piece sc) bar-num note-num player)))
    (when note
      ;; 21.9.11 no warning here
      (rm-cmn-marks note marks nil)))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 6.4.11: removes only the given marks, not all marks.  if players are nil,
;;; then all players will be processed
(defmethod rm-cmn-marks-from-notes ((sc slippery-chicken) start end
                                    players &rest marks)
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
                       (rm-cmn-marks-from-note
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

(defmethod rm-slurs ((sc slippery-chicken) start end players)
  (rm-cmn-marks-from-notes sc start end players '(beg-sl end-sl)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod add-cmn-object-before-note ((sc slippery-chicken)
                                       bar-num note-num player cmn-object)
  (add-cmn-object-before-note (piece sc) bar-num note-num player cmn-object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod sc-delete-cmn-marks ((sc slippery-chicken) bar-num note-num player)
  (bh-delete-cmn-marks (piece sc) bar-num note-num player))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod sc-delete-cmn-marks-from-event ((sc slippery-chicken)
                                           bar-num event-num player)
  (setf (cmn-marks (get-event sc bar-num event-num player)) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod sc-delete-cmn-objects-before ((sc slippery-chicken)
                                         bar-num note-num player)
  (delete-cmn-objects-before (piece sc) bar-num note-num player))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod tie ((sc slippery-chicken) bar-num note-num player 
                &optional curvature)
  (tie (piece sc) bar-num note-num player curvature))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; event numbers are 1-based 
(defmethod trill ((sc slippery-chicken) player start-bar start-event trill-note
                  &optional end-event end-bar)
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
  (unless (listp players)
    (setf players (list players)))
  (loop for p in players do
        (all-rests-to-ties-aux sc start-bar end-bar p 
                               :to-next-attack to-next-attack 
                               :tie-next-attack tie-next-attack
                               :last-rhythm last-rhythm
                               :auto-beam auto-beam)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod all-rests-to-ties-aux ((sc slippery-chicken)
                                  start-bar end-bar player
                                  &key (to-next-attack t)
                                       (tie-next-attack nil)
                                       ;; what to tie to on end-bar only
                                       (last-rhythm nil)
                                       (auto-beam nil))
  (let* ((active-bars (loop
                          for bnum from start-bar to (1- end-bar)
                          for bar = (get-bar (piece sc) bnum player)
                          unless (is-rest-bar bar)
                          collect bnum))
         (last (first (last active-bars))))
    ;; (print active-bars)
    (loop for bnum in active-bars do
          (tie-over-rest-bars-aux sc bnum player 
                                  :end-bar end-bar
                                  :tie-next-attack tie-next-attack
                                  :to-next-attack to-next-attack
                                  :auto-beam auto-beam
                                  :last-rhythm (when (= bnum last)
                                                 last-rhythm)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod tie-over-rest-bars ((sc slippery-chicken) bar-num players
                               &key (end-bar 99999) ;; num of empty bars
                                    (tie-next-attack nil)
                                    (to-next-attack t)
                                    (last-rhythm nil)
                                    (auto-beam nil))
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

;; bar-num is the bar that we start the ties from the last note, not the first
;; rest bar.  
(defmethod tie-over-rest-bars-aux ((sc slippery-chicken) bar-num player
                                   &key (end-bar 99999) 
                                        (to-next-attack t)
                                        (tie-next-attack nil)
                                        (last-rhythm nil)
                                        (auto-beam nil))
  (let* ((bar (get-bar (piece sc) bar-num player))
         (start-event-pos nil)
         (start-event (when bar 
                        (multiple-value-bind
                            (start-event pos)
                            (get-last-attack bar nil)
                          (setf start-event-pos pos)
                          start-event)))
         (porc (when (and start-event
                          (not (is-rest start-event)))
                 (clone (pitch-or-chord start-event))))
         (wporc (when (and start-event
                           (written-pitch-or-chord start-event))
                  (clone (written-pitch-or-chord start-event))))
         (last-event start-event)
         last-bar)
    ;; (print porc)
    (unless (zerop (notes-needed bar))
      (unless porc
        (error "slippery-chicken::tie-over-rest-bars-aux: can't tie from last ~
              event of bar ~a" bar-num))
      ;; remove any staccato or tenuto marks from this event
      (rm-cmn-marks start-event '(s t as) nil)
      (when porc
        (delete-cmn-marks porc))
      (when wporc
        (delete-cmn-marks wporc))
      ;; replace accent-staccato with just accent
      (replace-cmn-mark start-event 'as 'a)
      ;; our last event may have been tied to following events...
      (loop 
          for i from start-event-pos
          while (is-tied-from start-event)
          do
            (setf start-event (get-nth-event (1+ i) bar)))
      (setf (is-tied-from start-event) t)
      (no-accidental porc)
      (when wporc
        (no-accidental wporc))
      ;; (format t "~&acc: ~a" (show-accidental porc)) ;;(first (data porc))))
      (flet ((do-it (e)
               (setf (pitch-or-chord e) (clone porc)
                     (written-pitch-or-chord e) (when wporc 
                                                  (clone wporc))
                     (is-rest e) nil
                     (needs-new-note e) nil
                     (is-tied-to e) t
                     (is-tied-from e) t
                     last-event e)))
        ;; first the events in the current bar
        (loop 
            for i downfrom (1- (num-rhythms bar)) to 0
            for e = (nth i (rhythms bar))
            while (is-rest e)
            do
              (do-it e))
        ;; now the events in the next bars
        (loop 
            for bnum from (1+ bar-num) 
            for bar = (get-bar (piece sc) bnum player)
            while (<= (bar-num bar) end-bar)
            do
              (setf last-bar bar)
              (if (is-rest-bar bar)
                  (let ((events (events-for-full-bar (get-time-sig bar) 
                                                     porc wporc)))
                    (setf last-event (first (last events))
                          (rhythms bar) events)
                    (gen-stats bar))
                (progn
                  (when to-next-attack
                    ;; (loop for e in (rhythms bar) while (is-rest e) do
                    (loop for e in (rhythms bar) do
                          (if (is-rest e) 
                              (do-it e)
                            (progn
                              (when (and tie-next-attack
                                         (porc-equal e last-event))
                                ;; we've hit the next attack after the ties
                                ;; we created but if it's the same
                                ;; pitch/chord, tie to this too
                                (setf last-event nil
                                      ;; so we don't kill the tie
                                      (is-tied-to e) t)
                                (decf (notes-needed bar)))
                              (return)))))
                  (return))))
        (when last-event
          (setf (is-tied-from last-event) nil))
        (when last-rhythm
          (unless last-bar
            (setf last-bar (get-bar (piece sc) end-bar player)))
          (replace-first-event last-bar 
                               (change-rhythm
                                (clone (first (rhythms last-bar)))
                                last-rhythm))
          ;; we might have tied over several notes to the next attack so now
          ;; we've got to make them rests
          (loop 
              for e in (rest (rhythms last-bar)) 
              while (is-tied-to e)
              do
                (force-rest e)))
        (loop 
            for bnum from bar-num
            for bar = (get-bar (piece sc) bnum player)
            while (<= (bar-num bar) end-bar)
            do
              (consolidate-notes bar nil auto-beam)
              (auto-beam bar auto-beam nil))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; todo: make sure this works at the end of a piece too.
;;; NB end-bar is not when the ties stop, but rather when we last find an event
;;; to tie from (so the ties may go beyond end-bar)

(defmethod tie-over-all-rests ((sc slippery-chicken) player
                               start-bar end-bar 
                               &key 
                               (start-note 1)
                               (end-note 9999999)
                               (auto-beam nil)
                               (consolidate-notes nil))
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

;;; note-num is 1-based and counts tied-to notes as well
;;; todo: somehow, when we tie into a tq rest, we lose the bracket info :/
;;; still doesn't handle ties in tuplets well at all 
;;; 24.3.11: added end-bar
(defmethod tie-over-rests ((sc slippery-chicken) bar-num note-num player
                           &key end-bar auto-beam (consolidate-notes t))
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
    (rm-cmn-marks start-event '(s as t) nil)
    (when porc
      (delete-cmn-marks porc))
    (when wporc
      (delete-cmn-marks wporc))
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

;;; 1.4.11: note-num counts tied-notes but not rests

(defmethod delete-slur ((sc slippery-chicken) bar-num note-num player)
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
                         (rm-cmn-marks event 'beg-sl))
                       (warn "slippery-chicken::delete-slur (~a): no slur to ~
                              delete: ~a" player event))
                   (when (and in-slur (end-slur-p event))
                     (setf happy nil)
                     ;; (print-simple event)
                     (rm-cmn-marks event 'end-sl))))
             (setf happy nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; add slurs automatically (to wind instruments usually) to phrases: these are
;;; defined as not having any rests in them and not including any repeated
;;; notes. 

(defmethod auto-slur ((sc slippery-chicken) players
                      &key start-bar end-bar
                      rm-slurs-first
                      (rm-staccatos t)
                      ;; 5.4.11
                      (over-accents t)  ; make this work
                      verbose)
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
          (rm-cmn-marks e '(beg-sl end-sl) nil))
        (cond ((and (needs-new-note e) ;; start slur
                    (not start-e))
               (setf start-e e)
               (incf count))
              ((and (or (is-rest e) ;; end slur
                        ;; 5.4.11 todo: make this work!
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
                   (replace-cmn-mark start-e 'as 'a)
                   (replace-cmn-mark last-e 'as 'a)
                   (rm-cmn-marks start-e 'te nil)
                   (rm-cmn-marks last-e 'te nil)
                   (rm-cmn-marks start-e 's nil)
                   (rm-cmn-marks last-e 's nil))
                 (add-cmn-mark start-e 'beg-sl)
                 (add-cmn-mark last-e 'end-sl))
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
               ;; todo: this doesn't work: we haven't yet found those
               ;; notes in the middle... 
               ;; in the middle of a slur so remove staccatos
               (replace-cmn-mark e 'as 'a)
               (rm-cmn-marks e 's nil)))
        (setf last-e e)))
  ;; 9.4.11
  (check-slurs sc)
  t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Section is the current section reference e.g. '(2 1), player the current
;;; player e.g. 'vln.  For this section/sequence get the instrument the
;;; player is currently playing as defined in the instrument-change-map
;;; change-map.  N.B. Instruments cannot be changed mid-sequence and sequence
;;; is 1-based so we have to 1+ elsewhere if necessary

(defmethod get-current-instrument-for-player (section player sequence
                                              (sc slippery-chicken))
  (unless (listp section)
    (setf section (list section)))
  (let* ((player-obj (get-data player (ensemble sc)))
         ins-obj)
    (if (doubles player-obj)
        (multiple-value-bind
              (current-ins changes-here)
            (cm-get-data (instrument-change-map sc)
                         (econs section player)
                         sequence)
          (setf ins-obj (player-get-instrument player-obj current-ins nil))
          (unless ins-obj
            (error "slippery-chicken::get-current-instrument-for-player: ~
                    Couldn't get instrument at section~a, sequence ~a, for ~a"
                   section sequence player))
          (values ins-obj changes-here))
        ;; doesn't double
        (values (get-starting-ins sc player) nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 9.2.11 do the above but for a bar number instead
(defmethod get-instrument-for-player-at-bar (player bar (sc slippery-chicken))
  (let* ((bar (if (rthm-seq-bar-p bar) bar (get-bar sc bar player)))
         (section (butlast (player-section-ref bar)))
         (seq-num (1+ (nth-seq bar))))
    (get-current-instrument-for-player section player seq-num sc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 24.3.11: returns the number of semitones the note sounds away from the
;;; written pitch e.g. bass clarinet = -14
(defmethod get-transposition-at-bar (player bar (sc slippery-chicken))
  (transposition-semitones (get-instrument-for-player-at-bar player bar sc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object :before ((sc slippery-chicken) stream)
  (format stream "~%SLIPPERY-CHICKEN: ~
                  ~%                      title: ~a ~
                  ~%                set-palette: ~a ~
                  ~%                    set-map: ~a ~
                  ~%               hint-pitches: ~a ~
                  ~%               rthm-seq-map: ~a ~
                  ~%           rthm-seq-palette: ~a ~
                  ~%                  tempo-map: ~a ~
                  ~%                tempo-curve: ~a ~
                  ~%         instrument-palette: ~a ~
                  ~%                   ensemble: ~a 
                  ~%      instruments-hierarchy: ~a 
                  ~%        fast-leap-threshold: ~a "
          (title sc) (id (set-palette sc)) (id (set-map sc))
          (id (hint-pitches sc)) (id (rthm-seq-map sc))
          (id (rthm-seq-palette sc)) (id (tempo-map sc)) (tempo-curve sc)
          (id (instrument-palette sc)) (id (ensemble sc))
          (instruments-hierarchy sc) (fast-leap-threshold sc))
  (statistics sc stream))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Find out the number of sequences in a section.

(defmethod num-seqs ((sc slippery-chicken) section-ref)
  (sclist-length
   (get-data (econs 
              (if (listp section-ref) 
                  section-ref 
                (list section-ref)) 
              (first (players (piece sc))))
             (piece sc))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The linking of the rthm-seq-map (and hence piece) slot only works at the
;;; instrument level so we don't get a pointer to the next section, rather,
;;; only when we ask for instrument data do we get the points.  Eg (get-data 3
;;; (piece sc)) will have previous, this, and next slots all NIL, whereas
;;; (get-data '(3 some-instrument) (piece sc)) will return a player-section
;;; where the previous, this, and next slots are good (this is all as it should
;;; be!).  In order to get the references of a number of contiguous sections
;;; then, we'll have to use instrument references.

(defmethod get-section-refs ((sc slippery-chicken) start-section num-sections)
  (let* ((last-player (first (last (players (ensemble sc)))))
         (section-list (if (listp start-section) 
                           start-section
                         (list start-section)))
         (ref section-list))
    (unless num-sections
      (setf num-sections (get-num-top-level-sections sc)))
    (loop with player-ref
        repeat num-sections
        collect ref
        do 
          (setf player-ref (econs ref last-player)
                ;; todo: bollocks, this isn't the case.... :::
                ;; of course, this would fail if <last-player> wasn't
                ;; actually the last player in the data list of each section
                ;; but that isn't the case is it?  (Haven't tested this)
                ref (butlast (next (get-data player-ref (piece sc))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 24/4/10 this is the number of sections at the top level only

;;; ****m* slippery-chicken/get-num-top-level-sections
;;; FUNCTION
;;; get-num-top-level-sections:
;;;
;;; Return the number of sections in the piece i.e the top-level ones as
;;; defined e.g. in the set-map.  NB the num-sections slot of slippery-chicken
;;; is the number of sections and sub-sections.
;;; 
;;; ARGUMENTS:
;;; - the slippery-chicken object
;;; 
;;; RETURN VALUE: 
;;; the number of sections (integer)
;;; 
;;; SYNOPSIS
(defmethod get-num-top-level-sections ((sc slippery-chicken))
;;; ****
  (num-data (set-map sc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-all-section-refs ((sc slippery-chicken))
  ;; (get-all-refs (set-palette sc)))
  ;; 20/7/05 don't know why the palette was used, it's the map that's useful!
  (get-all-refs (set-map sc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod statistics ((sc slippery-chicken) &optional (stream t))
  (statistics (piece sc) stream))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod check-instrument-doublings ((sc slippery-chicken))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-tempo ((sc slippery-chicken) bar-num)
  (data (scm-get-data bar-num (tempo-map sc))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; bar-num is actually required but optional because of the rthm-seq-bar
;;; method of the same name.

(defmethod get-time-sig ((sc slippery-chicken) &optional bar-num)
  (object-is-nil? bar-num "slippery-chicken::get-time-sig" 'bar-num)
  (get-time-sig (get-bar sc bar-num (first (players (ensemble sc))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Set tempo-change slot of the first event of the bar (but in each voice!)
;;; when a tempo change is indicated in the tempo-map

(defmethod update-events-tempo ((sc slippery-chicken) 
                                ;; this is a list of those players who should
                                ;; display tempo changes in the score
                                &optional display-players)
  (let* ((last-tempo (make-tempo -1))
         (current-tempo nil)
         (players (players sc)))
    (unless display-players
      (setf display-players (get-groups-top-ins sc)))
    ;; start tempo gets set in cm::process-voices
    (loop for bar-num from 1 to (num-bars (piece sc)) do
          (setf current-tempo (get-tempo sc bar-num))
          (unless (tempo-equal last-tempo current-tempo)
            ;; (print current-tempo)
            (loop 
                for player in players 
                for bar = (get-bar sc bar-num player)
                for first-event = (first (rhythms bar))
                do
                  (when (is-grace-note first-event)
                    (loop for e in (rhythms bar) do
                          (unless (is-grace-note e)
                            (setf first-event e)
                            (return))))
                  ;; of course this assumes you can't change tempo mid-bar
                  (setf (tempo-change first-event) current-tempo)
                  (setf (display-tempo first-event) 
                    (when (member player display-players)
                      t))
                  ;; (format t "~&tempo: ~a player: ~a" 
                  ;;     (tempo-change first-event) player)
                  )
            (setf last-tempo current-tempo)))
    t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod handle-set-limits ((sc slippery-chicken))
  (flet ((do-limits (set-limits num-sections)
           (make-assoc-list 
            nil
            (loop for ins in set-limits collect
                  (list (first ins)
                        (doctor-set-limits-env (second ins) num-sections))))))
    (let* ((ns (num-sections sc))
           (high (do-limits (set-limits-high sc) ns))
           (low (do-limits (set-limits-low sc) ns)))
      (setf (set-limits-high sc) high
            (set-limits-low sc) low))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; global-seq-num 1-based.

(defmethod get-set-limits ((sc slippery-chicken) player global-seq-num)
  (let* ((limit-ins-high
          (get-set-limit-high sc player global-seq-num))
         (limit-ins-low 
          (get-set-limit-low sc player global-seq-num))
         ;; 10/3/07: got to take the global limits into account if an entry for
         ;; 'all was made
         (global-limit-high
          (get-set-limit-high sc 'all global-seq-num))
         (global-limit-low
          (get-set-limit-low sc 'all global-seq-num))
         (limit-high (cond ((and global-limit-high limit-ins-high)
                            (pitch-min global-limit-high limit-ins-high))
                           (global-limit-high global-limit-high)
                           ;; this could still return nil but that's handled
                           ;; elsewhere  
                           (t limit-ins-high)))
         (limit-low (cond ((and global-limit-low limit-ins-low)
                           (pitch-max global-limit-low limit-ins-low))
                          (global-limit-low global-limit-low)
                          ;; ditto
                          (t limit-ins-low))))
    ;; (format t "~&hi ~a low ~a ins-hi ~a ins-low ~a"
       ;;      (id global-limit-high) (id global-limit-low) 
          ;;  (id limit-ins-high) (id limit-ins-low))
    (list limit-low limit-high)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* slippery-chicken/shorten-large-fast-leaps
;;; FUNCTION
;;; shorten-large-fast-leaps:
;;;
;;; Attempt to tame those melodic leaps that are very fast and larger than the
;;; limit defined in the instrument class.
;;; 
;;; ARGUMENTS:
;;; - the slippery-chicken object
;;; - threshold: the max duration of a fast note, in seconds.
;;; - whether to print what we're going (t or nil)
;;; 
;;; RETURN VALUE: 
;;; always t
;;; 
;;; SYNOPSIS
(defmethod shorten-large-fast-leaps ((sc slippery-chicken) 
                                     &key threshold (verbose t))
;;; ****
  ;; 24.3.11 get threshold from class if not given
  (unless threshold
    (setf threshold (fast-leap-threshold sc)))
  (loop 
     for player in (get-players (ensemble sc))
     with global-seq-num 
     with count = 0
     do
     (setf global-seq-num 1)
     (loop for section in (get-all-section-refs sc) do
          (loop 
             for seq-num from 0
             for seq = (get-nth-sequenz (piece sc) section player seq-num)
             for set = (get-nth-from-palette section seq-num (set-map sc))
             for first-bar-num = (when seq
                                   (bar-num (first (bars seq))))
             for ins = (get-current-instrument-for-player
                        section player (1+ seq-num) sc)
             for transp = (- (transposition-semitones ins))
             for lfl = (largest-fast-leap ins)
             with qnis with last-seq
             while seq do
             (unless set
               (error "slippery-chicken::shorten-large-fast-leaps: no set!~
                       ~%section = ~a seq-num = ~a" section seq-num))
             (setf qnis (get-quick-notes-indices seq last-seq 
                                                 threshold))
             (when (zerop transp)
               (setf transp nil))
             (when (and qnis lfl)
               (loop 
                  with limits = (get-set-limits sc player 
                                                global-seq-num)
                  with pitches = (limit-for-instrument 
                                  (clone set) ins
                                  :upper (second limits)
                                  :lower (first limits))
                  for qni in qnis
                  ;; a zero means we got a fast note from
                  ;; the last note last seq to the first
                  ;; note this seq
                  for e1 = (if (zerop qni)
                               (get-last-attack last-seq)
                               (get-nth-attack (1- qni) seq))
                  for e2 = (get-nth-attack qni seq)
                  for distance = (event-distance e1 e2)
                  with new-pitch with pos with compare
                  do
                  (when (> (abs distance) lfl)
                    (if (> distance 0) ;; leap up
                        (progn
                          (setf compare (lowest e1)
                                pos (position (highest e2) 
                                              pitches
                                              :test #'pitch=))
                          (unless pos
                            (error "slippery-chicken::~
                                              shorten-large-fast-leaps: ~
                                              ~a not in set!!!:~a~%pitches:~a" 
                                   (data (highest e2)) (data set)
                                   (pitch-list-to-symbols pitches)))
                          (setf new-pitch
                                (loop 
                                   for i downfrom pos to 0 
                                   for p = (nth i pitches)
                                   do
                                   (when (<= (pitch- p compare)
                                             lfl)
                                     ;; a side-effect here is that
                                     ;; quick leaps to chords are
                                     ;; replaced with single pitches 
                                     (return p)))))
                        (progn ;; leap down
                          (setf compare (highest e1)
                                pos (position (lowest e2) pitches
                                              :test #'pitch=))
                          (unless pos
                            (error "slippery-chicken::~
                                            shorten-large-fast-leaps: ~
                                            ~a not in set!!!" (highest e2)))
                          (setf new-pitch
                                (loop 
                                   for i from pos to (1- (length pitches))
                                   for p = (nth i pitches)
                                   do
                                   (when (<= (pitch- compare p)
                                             lfl)
                                     ;; a side-effect here is that
                                     ;; quick leaps to chords are
                                     ;; replaced with single pitches 
                                     (return p))))))
                    (if new-pitch
                        (flet ((doit (event)
                                 (when verbose
                                   (format t "~&seq-num ~a, ~a, ~
                                                        replacing ~a with ~a"
                                           seq-num player
                                           (id new-pitch)
                                           (id (pitch-or-chord e2))))
                                 (setf (midi-channel new-pitch)
                                       (midi-channel 
                                        (if (is-chord event)
                                            (first 
                                             (data 
                                              (pitch-or-chord event)))
                                            (pitch-or-chord event)))
                                       (cmn-marks new-pitch)
                                       (cmn-marks (pitch-or-chord event))
                                       (pitch-or-chord event)
                                       new-pitch
                                       (written-pitch-or-chord event)
                                       (when transp
                                         (set-written event transp)))))
                          (incf count)
                          (doit e2)
                          (when (is-tied-from e2)
                            ;; get the attack again but this time the
                            ;; bar and event indices of where it is 
                            (multiple-value-bind
                                  (e nth-bar nth-event)
                                (get-nth-attack qni seq)
                              (loop 
                                 with bar-num = (+ nth-bar 
                                                   first-bar-num)
                                 with bar = 
                                 (get-bar sc bar-num player)
                                 with happy = t 
                                 while happy do
                                 (if bar
                                     (progn
                                       (incf nth-event)
                                       (when (>= nth-event 
                                                 (num-rhythms bar))
                                         (incf bar-num)
                                         (setf nth-event 0
                                               bar (get-bar 
                                                    sc bar-num player)))
                                       (setf e (get-nth-event
                                                nth-event bar))
                                       (when (is-tied-to e)
                                         (doit e))
                                       (unless (is-tied-from e)
                                         (setf happy nil)))
                                     (setf happy nil))))))
                        (warn "~&slippery-chicken::~
                                       shorten-large-fast-leaps: ~
                                       Couldn't get new pitch for ~a, section ~
                                       ~a, seq-num ~a, e1 ~a, e2 ~a! ~
                                       ~%pitches: ~a" 
                              player section (1+ seq-num)
                              (id (pitch-or-chord e1))
                              (id (pitch-or-chord e2))
                              (pitch-list-to-symbols pitches))))))
             (setf last-seq seq)
             (incf global-seq-num)))
     finally (return count)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; optional args are actually required but optional because of event class
;;; method  
;;; event-num is 1-based but counts rests and ties
(defmethod delete-clefs ((sc slippery-chicken) &optional
                         player bar-num event-num)
  (let ((e (get-event sc bar-num event-num player)))
    (if e
        (delete-clefs e)
        (warn "slippery-chicken::delete-clefs: Can't delete clef ~
               for ~a at bar ~a, event ~a"
              player bar-num event-num))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 11.4.11: event-num is 1-based.  optional args actually required
(defmethod get-clef ((sc slippery-chicken) &optional bar-num event-num player)
  (let* ((bar (get-bar sc bar-num player))
         (e (when bar (get-nth-event (1- event-num) bar))))
    (when e 
      (get-clef e))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; optional args are actually required but optional because of event class
;;; method  
(defmethod add-clef ((sc slippery-chicken) player &optional
                     bar-num event-num clef)
  (let ((e (get-event sc bar-num event-num player)))
    (if e
        (add-clef e clef)
        (warn "slippery-chicken::add-clef: Can't add clef ~
               for ~a at bar ~a, event ~a"
              player bar-num event-num))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod move-clef ((sc slippery-chicken) from-bar from-event
                      to-bar to-event player)
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
;;; (e.g. over complete piano range).  

(defmethod auto-clefs ((sc slippery-chicken) 
                       &key verbose in-c players 
                       (delete-clefs t)
                       (delete-cmn-objects-before nil))
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
             (when delete-cmn-objects-before
               (setf (cmn-objects-before event) nil))
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

;;;  13.4.11: start and end bar are inclusive.
;;; at the moment, we use bass/treble clefs with 8ve signs on to indicate pitch
;;; extremes (assuming an instrument has these clefs), but these can be
;;; converted to octave brackets here.  NB no 15ma/mb handled here.
;;; todo: close bracket before several bars rest and then reopen on next note
(defmethod octave-clefs-to-brackets ((sc slippery-chicken)
                                     &key players start-bar end-bar)
  (unless (listp players)
    (setf players (list players)))
  (unless players
    (setf players (players sc)))
  (unless start-bar
    (setf start-bar 1))
  (unless end-bar
    (setf end-bar (num-bars sc)))
  (loop for player in players do
       (next-event sc player nil start-bar)
       (loop with clef
          with current-clef
          with new-clef
          with octave-clef
          with last-note
          with dt
          with db
          for e = (next-event sc player) while e do
          (when (> (bar-num e) end-bar)
            (when octave-clef
              (error "slippery-chicken::octave-clefs-to-brackets (~a): got to ~
                      bar ~a but octave-clef is still ~a"
                     player (bar-num e) octave-clef))
            (return))
          (setf clef (get-clef e)
                new-clef clef
                dt (eq clef 'double-treble)
                db (eq clef 'double-bass))
          (when clef
            (if (or dt db)
                (progn
                  (setf octave-clef clef
                        new-clef (if dt 'treble 'bass))
                  (delete-clefs e)
                  (unless (eq new-clef current-clef)
                    (add-clef e new-clef))
                  (add-cmn-mark e (if dt 'beg-8va 'beg-8vb)))
                ;; if we see any other clef at all we can assume the octave
                ;; +/- is at and end, but only delete the clef if we've gone
                ;; double-x to x
                (when octave-clef
                  (setf dt (eq octave-clef 'double-treble)
                        db (eq octave-clef 'double-bass))
                  (add-cmn-mark last-note (if dt 'end-8va 'end-8vb))
                  (when (or (and dt (eq clef 'treble))
                            (and db (eq clef 'bass)))
                    (delete-clefs e))
                  (setf octave-clef nil))))
          (when octave-clef
            (setf (8va e) (if (eq octave-clef 'double-treble) 1 -1)))
         ;; (print (8va e))
          (when new-clef
            (setf current-clef new-clef))
          (unless (is-rest e)
            (setf last-note e))))
  ;; (split-octave-brackets sc :players players :start-bar start-bar
     ;;                    :end-bar end-bar)
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 26.7.11 (Pula): don't allow an octave sign to extend over too many
;;; rests, rather end it and restart it.  todo: debug

(defmethod split-octave-brackets ((sc slippery-chicken)
                                  &key players start-bar end-bar
                                  (max-rests 4))
  (unless (listp players)
    (setf players (list players)))
  (unless players
    (setf players (players sc)))
  (unless start-bar
    (setf start-bar 1))
  (unless end-bar
    (setf end-bar (num-bars sc)))
  #|
  todo: need to remove octavation over empty bars first i.e. where whole
                                  bracket has no notes
  ;; first make sure brackets start and end on notes, not rests
  (start-octave-brackets-notes-only sc :players players :start-bar start-bar
  :end-bar end-bar)
  (end-octave-brackets-notes-only sc :players players :start-bar start-bar
  :end-bar end-bar)
  |#
  (let (under8v rests last-note rest-count)
    (flet ((reset ()
             (setf rests '()
                   rest-count 0
                   last-note nil)))
      (loop for player in players do
           (next-event sc player nil start-bar)
           (loop for e = (next-event sc player)
              while (and e (<= (bar-num e) end-bar)) do
              (when (and under8v (not (zerop under8v))
                         (or (has-cmn-mark e 'beg-8va)
                             (has-cmn-mark e 'beg-8vb)))
                (warn "slippery-chicken::split-octave-brackets: ~
                        ~a, bar ~a: new octavation starting when already in one"
                       player (bar-num e)))
              (setf under8v (8va e))
              ;; (print (bar-num e))
              (cond ((zerop under8v) (reset))
                    ((is-rest e)        ; under an 8ve
                     (incf rest-count)
                     (push e rests))
                    ((> rest-count max-rests) ; it's a note
                     (print (is-rest last-note))
                     (print (is-rest e))
                     (when last-note
                       (add-cmn-mark-once last-note
                                          (if (= 1 under8v) 'end-8va 'end-8vb)))
                     (add-cmn-mark-once e (if (= 1 under8v) 'beg-8va 'beg-8vb))
                     (loop for r in rests do
                          (rm-cmn-marks r '(beg-8va beg-8vb) nil)
                          (setf (8va r) 0))
                     (reset)
                     (setf last-note e))
                    (t (reset) ; a note but not too many rests
                       (setf last-note e))))))) 
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod start-octave-brackets-notes-only ((sc slippery-chicken)
                                             &key players start-bar end-bar)
  (unless (listp players)
    (setf players (list players)))
  (unless players
    (setf players (players sc)))
  (unless start-bar
    (setf start-bar 1))
  (unless end-bar
    (setf end-bar (num-bars sc)))
  (loop for player in players do
       (next-event sc player nil start-bar)
       (loop with bracket with carry with rests
          for e = (next-event sc player)
          while (and e (<= (bar-num e) end-bar)) do
          (when (zerop (8va e))
            (setf bracket nil))
          (when (and (not bracket) (not (zerop (8va e)))) ; start bracket
            (setf bracket (8va e))
            (if (is-rest e)
                (progn 
                  (setf carry bracket)
                  (push e rests))
                (setf carry nil
                      rests nil)))
          (when (and carry (not (is-rest e)))
            (print (bar-num e))
            (add-cmn-mark-once e (if (= 1 carry) 'beg-8va 'beg-8vb))
            (loop for r in rests do 
                 (rm-cmn-marks r '(beg-8va beg-8vb) nil)
                 (setf (8va r) 0))
            (setf rests nil
                  carry nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod end-octave-brackets-notes-only ((sc slippery-chicken)
                                           &key players start-bar end-bar)
  (unless (listp players)
    (setf players (list players)))
  (unless players
    (setf players (players sc)))
  (unless start-bar
    (setf start-bar 1))
  (unless end-bar
    (setf end-bar (num-bars sc)))
  (loop for player in players do
       (next-event sc player nil start-bar)
       (loop with bracket with rests with last-note
          for e = (next-event sc player)
          while (and e (<= (bar-num e) end-bar)) do
          (setf bracket (8va e))
          (unless (zerop bracket)
            (if (is-rest e)
                (if (or (has-cmn-mark e 'end-8va)
                        (has-cmn-mark e 'end-8vb))
                    (progn
                      (print (bar-num e))
                      (add-cmn-mark-once last-note
                                         (if (= 1 bracket) 'end-8va 'end-8vb))
                      (push e rests)
                      (loop for r in rests do 
                           (rm-cmn-marks r '(end-8va end-8vb) nil)
                           (setf (8va r) 0)))
                    ;; it's a rest without end bracket
                    (push e rests))
                ;; it's a note
                (setf last-note e))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 
(defmethod multi-bar-rests ((sc slippery-chicken) &optional players)
  (unless (listp players)
    (setf players (list players)))
  (unless players
    (setf players (players sc)))
  (loop for player in players do
       (loop 
          with first-multi = nil
          with got-rehearsal-letter = nil
          with ins-change = nil
          with count = 0
          for bar-num from 1 to (num-bars sc)
          for bar = (get-bar sc bar-num player)
          for rest-bar = (is-rest-bar bar)
          do
          ;; is nil by default but reset here just in case we're redoing
          ;; this after editing the bars somehow.
          (setf (multi-bar-rest bar) nil
                ;; for efficiency's sake only do this if we're on a rest bar
                ins-change (when rest-bar
                             (nth-value 1 (get-current-instrument-for-player
                                           (butlast (player-section-ref bar))
                                           (first (last (player-section-ref
                                                         bar)))
                                           ;; seq is 1-based in this method call
                                           (1+ (nth-seq bar))
                                           sc))))
          (if (or (not rest-bar)
                  got-rehearsal-letter
                  (write-time-sig bar)
                  (> (bar-line-type bar) 0)
                  ;; 28.2.11 there's a pause or something on this rest...
                  (and (rhythms bar) (cmn-marks (first (rhythms bar))))
                  ;; we're in a multi-bar rest but tempo changes i.e. tempo
                  ;; change on first bar of multi is fine 
                  (and (> count 0)
                       (display-tempo (first (rhythms bar))))
                  ins-change)
              (progn
                (when (> count 1)
                  ;; we got the bar after a multi-bar rest
                  (setf (multi-bar-rest first-multi) count
                        (write-bar-num first-multi) nil)
                  ;;(format t "~&~a at bar ~a"
                  ;;           count (bar-num first-multi))
                  )
                (setf count 0)
                (when rest-bar
                  (setf first-multi bar)
                  (incf count)))
              (progn
                (when (zerop count)
                  (setf first-multi bar))
                (incf count)
                (when (> count 1)
                  ;; this bar is part of a multi-bar rest
                  (setf (multi-bar-rest bar) t))))
          ;; remember: rehearsal letters are attached to the barline of the
          ;; __previous__ bar.... 
          (setf got-rehearsal-letter (rehearsal-letter bar)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+cm-2
(defmethod midi-play ((sc slippery-chicken)
                      start-section
                      &key 
                      ;; these voices are used to get the actual sequence
                      ;; orders i.e. each voice will be appended to <section>
                      ;; when calling get-data.
                      ;; if nil then all voices.
                      (voices nil)
                      (midi-file "/temp/sc.mid")
                      (time-scaler 1.0)
                      (from-sequence 1)
                      (num-sequences nil)
                      ;; if nil we'll write all the sections
                      (num-sections nil)
                      ;; if this is a 7-bit number we'll use this for all notes
                      (force-velocity nil)
                      ;; this means durations will carry over rests!
                      (ignore-rests nil))
  (setf voices
    (cond ((and voices (listp voices)) voices)
          ((and voices (atom voices)) (list voices))
          ((not voices) (get-players (ensemble sc)))
          (t (error "slippery-chicken::midi-play: voices = ~a!?" voices))))
  ;; (print voices)
  (let* ((voices-events (get-events-start-time-duration 
                         sc start-section voices 
                         :time-scaler time-scaler
                         :from-sequence from-sequence
                         :num-sequences num-sequences
                         :num-sections num-sections
                         :get-time-sig-changes t
                         :ignore-rests ignore-rests 
                         :include-rests t))
         ;; do all the program changes for the beginning irrespective of
         ;; whether the player changes instrument or not.  subsequent program
         ;; changes are handled in the event class.
         (midi-setup 
          (loop 
              for voice in voices
              for player = (get-player sc voice)
              for current-ins = (id (get-current-instrument-for-player
                                     start-section voice from-sequence sc))
              for ins = (get-data current-ins (instrument-palette sc))
              collect
                (list (midi-channel player) (midi-program ins))
              when (microtonal-chords-p player)
              collect
                (list (microtones-midi-channel player)
                      (midi-program ins)))))
    (cm::process-voices voices-events midi-file (get-tempo sc 1) midi-setup
                        ;; TODO: will this time offset (in seconds!) work when
                        ;; starting in the middle of the piece? shouldn't it be
                        ;; in quarters not secs? 
                        (- (start-time-qtrs
                            (get-nth-sequenz (piece sc) start-section
                                             (first voices) 
                                             (1- from-sequence))))
                        force-velocity)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-player ((sc slippery-chicken) player)
  (get-data player (ensemble sc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; See methods above for a description of the arguments

;;; N.B.  This will fail when an instrument is silent for a section (i.e. has
;;; nil in the rthm-seq-map).  All of these clm methods should be re-written to
;;; conform with the method structure of cmn-get-data.
;;; todo: add multichannel output via a new instrument

;;; N.B. clm's nrev instrument will have to be loaded before calling
;;; this function.  

#+clm
(defmethod clm-play ((sc slippery-chicken) section voices 
                     sound-file-palette-ref 
                     &key 
                     ;; if another ref is given, then we make fibonacci
                     ;; transitions from one group of snds to another!
                     sound-file-palette-ref2
                     ;; this determines the chance that a note will be played
                     ;; or not; it is a random selection but uses a fixed seed
                     ;; that is re-initialized each time clm-play is called.
                     ;; the following default ensures every note will play.
                     (play-chance-env '(0 100 100 100))
                     ;; self-explanatory, in seconds
                     (max-start-time 99999999)
                     ;; the exponent the above env is raised to
                     (play-chance-env-exp 0.5)
                     ;; this scales duration and start-time of events (in
                     ;; effect a tempo scaler)--not to be confused with
                     ;; duration-scaler
                     (time-scaler 1.0)
                     ;; what the samples should be scaled to
                     (normalise .99)
                     ;; if t, then clm won't be called, you'll just see the
                     ;; printouts to the terminal 
                     (simulate nil)
                     (from-sequence 1)
                     ;; specifying nil will simply get them all.
                     (num-sequences nil)
                     (num-sections 1)
                     ;; in contrast to the other methods, rests are ignored
                     ;; per default.
                     (ignore-rests t)
                     (time-offset 0.0)
                     ;; (pitch-object-default-src-ref-pitch 'c4)
                     (chords nil)
                     (chord-accessor nil)
                     ;; the nth note of the chord (from bottom) for the lowest
                     ;; voice  
                     (note-number 0)
                     ;; whether clm should play  or not
                     (play t)
                     (amp-env '(0 1 60 1 100 0))
                     ;; it's not a great idea to start some longer sounds
                     ;; always at the beginning, because of the repetition
                     ;; created and the fact that we never get to other
                     ;; interesting parts of the sound.  Set this to t for
                     ;; start-time incrementing.
                     (inc-start nil)
                     (src-width 20)
                     (src-scaler 1.0)
                     ;; if this is a number or note symbol, then it will
                     ;; be used as a reference pitch instead of that
                     ;; stored in the sndfile.
                     (do-src t)
                     (rev-amt 0.0)
                     ;; this scales duration of events (creates overlaps)--not
                     ;; to be confused with time-scaler!
                     (duration-scaler 1.0)
                     (short-file-names nil)
                     ;; whether to query the user before overwriting existing
                     ;; sound files.
                     (check-overwrite t)
                     ;; when t then we start over at the beginning of the snd
                     ;; list at the beginning of each rthm-seq.
                     (reset-snds-each-rs t)
                     ;; when t then we start over at the beginning of the snd
                     ;; list at the beginning of each voice.
                     (reset-snds-each-voice t)
                     ;; usually we use a smaller segment of a long sound file
                     ;; as a sndfile instance. Allow an event to go beyond the
                     ;; given end point if the following is t.
                     (duration-run-over nil)
                     ;; how far to the left (0) or right (90) can a sound be
                     ;; placed? 
                     ;; No longer needed.
                     ;; (min-degree 10)
                     ;; number of sound output channels
                     (channels 2)
                     ;; sampling rate of output
                     (srate clm::*clm-srate*)
                     (data-format clm::*clm-data-format*)
                     ;; whether clm should print the seconds computed as it
                     ;; works  
                     (print-secs nil)
                     (output-name-uniquifier "")
                     (sndfile-extension ".wav")
                     ;; just in case we want to use an external palette instead
                     ;; of the one in the sc object.
                     (sndfile-palette nil))
  (unless num-sequences
    (setf num-sequences (num-seqs sc section)))
  (unless (listp voices)
    (setf voices (list voices)))
  ;; re-initialise our random number generator.
  (random-rep 100 t)
  ;;; 10/1/07 remove the events with a start-time after max-start-time at this
  ;;; stage rather than rejecting them later (otherwise play-chance-env will
  ;;; range over the full event list instead of those below max-start-time)
  (let* ((events (get-events-with-src sc section voices 
                                      ;; these have 0 duration so must ignore
                                      ;; them for now 
                                      :ignore-grace-notes t
                                      :time-scaler time-scaler
                                      :from-sequence from-sequence
                                      :num-sequences num-sequences
                                      :num-sections num-sections
                                      :ignore-rests ignore-rests
                                      :chords chords
                                      :chord-accessor chord-accessor
                                      :note-number note-number))
         (section1-num-seqs (if num-sequences
                                num-sequences
                              (num-seqs sc section)))
         (num-voices (length voices))
         (events-per-voice (ml 0 num-voices))
         ;; clisp doesn't like (loop for voice in events sum (loop for rs ...
         (total-events (loop 
                           for i from 0
                           for voice in events
                           for len = (loop for rs in voice sum (length rs))
                           do (setf (nth i events-per-voice) len)
                           sum len))
         (snds (make-cscl (get-snds sound-file-palette-ref
                                    (if sndfile-palette
                                        sndfile-palette
                                      (sndfile-palette sc)))))
         (snds2 (when sound-file-palette-ref2
                  (make-cscl 
                   (get-snds sound-file-palette-ref2 
                             (if sndfile-palette
                                 sndfile-palette
                               (sndfile-palette sc))))))
         (snd-transitions (loop for num-events in events-per-voice collect
                                (fibonacci-transition num-events)))
         (snd nil)
         (snd-group nil)
         (srt 0.0)
         (srt-freq (cond ((numberp do-src) do-src)
                         ((and (not (eq do-src t))
                               (symbolp do-src))
                          (note-to-freq do-src))))
         (duration 0.0)
         (wanted-duration 0.0)
         (wanted-duration-string "")
         (first-event-start nil)
         (input-start 0.0)
         (latest-possible-start 0.0)
         (available-dur 0.0)
         (event-count 1)
         (event-count-voice 0)
         (events-this-rs 0)
         (output-start 0.0)
         (output-ok t)
         (this-play-chance-env '())
         (skip-this-event t)
         (total-skipped 0)
         ;; todo: find a way of not writing the reverb stream unless we need it
         ;; (reverb (if (zerop rev-amt) nil clm::nrev))
         (file-name
          (string-downcase        
           (if short-file-names
               (format nil "~{~a-~}~a~{~a-~}~{~a.~}~a-~a~a~a"
                       (if (listp sound-file-palette-ref) 
                           sound-file-palette-ref
                         (list sound-file-palette-ref))
                       (if sound-file-palette-ref2
                           "to-"
                         "")
                       (when sound-file-palette-ref2
                         (if (listp sound-file-palette-ref2) 
                             sound-file-palette-ref2
                           (list sound-file-palette-ref2)))
                       (if (listp section) 
                           section 
                         (list section))
                       from-sequence 
                       (+ -1 from-sequence section1-num-seqs)
                       output-name-uniquifier
                       sndfile-extension)
             (format nil "~a~a~{-~a~}~{-~a~}~{-~a~}to-~{-~a~}-seq~a-~a~a"
                     output-name-uniquifier
                     (string-trim "+" (id sc))
                     (if (listp section) section (list section))
                     voices
                     (if (listp sound-file-palette-ref) 
                         sound-file-palette-ref
                       (list sound-file-palette-ref))
                     (when sound-file-palette-ref2
                       (if (listp sound-file-palette-ref2) 
                           sound-file-palette-ref2
                         (list sound-file-palette-ref2)))
                     from-sequence 
                     (+ -1 from-sequence section1-num-seqs)
                     sndfile-extension))))
         (output 
          (progn
            ;; first convert spaces to -'s in output file name
            (setf file-name (substitute #\- #\Space file-name))
            (format nil "~a~a"
                    (if (snd-output-dir sc)
                        (snd-output-dir sc)
                      "")
                    file-name)))
         ;; keep going (set to nil when max-start-time is exceeded)
         (happy t)
         (rthm-seqs nil))
    ;; (print 'here)
    (when (zerop (sclist-length snds))
      (error "slippery-chicken::clm-play: <snds>: No sounds for reference ~a"
             sound-file-palette-ref))
    (when (and sound-file-palette-ref2 (zerop (sclist-length snds2))
               (error "slippery-chicken::clm-play: <snds2>: ~
                       No sounds for reference ~a"
                      sound-file-palette-ref2)))
    (when (and check-overwrite (probe-file output))
      (setf output-ok 
        (yes-or-no-p "File exists: ~%~a  ~%Overwrite (yes or no) > " 
                     output)))
    (when output-ok
      (format t "~%Output file will be ~%\"~a\"~%~%" output)
      (when inc-start
        (loop for snd in (data snds) do (reset-usage snd))
        (when snds2
          (loop for snd in (data snds2) do (reset-usage snd)))
        (loop for voice in events and snd-trans in snd-transitions do
              (setf snd-trans (copy-list snd-trans))
              (loop for rs in voice do
                    (loop 
                        for evts in rs 
                        for snd = (if (and snds2 (= 1 (pop snd-trans)))
                                      (get-next snds2)
                                    (get-next snds))
                        do
                          ;; just to avoid the compiler warning...
                          (progn evts)
                          (unless snd
                            (error "slippery-chicken::clm-play: ~
                                    snd is nil (whilst counting)!"))
                          (incf (will-be-used snd)))))
        ;; here we reset them before starting, this is correct!
        (reset snds)
        (when snds2
          (reset snds)))
      (setf first-event-start 
        (loop
            for voice in events 
            for ffv = (first (first voice))
            if ffv minimize (start-time ffv)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (clm::with-sound (:scaled-to normalise 
                                   :reverb clm::nrev
                                   :decay-time 3
                                   :output output
                                   ;; these things should be default anyway but
                                   ;; somehow they're not...
                                   :srate srate
                                   :data-format data-format
                                   :header-type clm::*clm-header-type*
                                   :play play :channels channels :statistics t)
        (loop 
            for voice in events and voice-name in voices 
            and snd-trans in snd-transitions
                             ;; and events-this-voice in events-per-voice
            and voice-count from 1
                                 ;; 15/12/06 this while clause causes a voice
                                 ;; not to process when
                                 ;; the previous overstepped the max-start-time
                                 ;; while happy
            do
              (setf snd-trans (copy-list snd-trans)
                    event-count-voice 0
                    ;; 15/12/06 reset happy to the new voice processes
                    happy t
                    this-play-chance-env 
                    (new-lastx play-chance-env
                               ;; 10/1/07 we want to use the whole
                               ;; play-chance-env when we use max-start-time:
                               ;; (1- events-this-voice)))
                               ;; got to take time-offset and the start time of
                               ;; the first event into consideration, not just
                               ;; max-start-time...
                               (count-events-before-max-start 
                                voice
                                (- (+ max-start-time first-event-start)
                                   time-offset))))
              (format t "~%Processing voice ~a/~a: ~a (resting voices will ~
                          not be processed)~%"
                      voice-count num-voices (nth (1- voice-count) voices))
              (when (= 1 num-sections)
                ;; this code will only work when we're processing 1 section
                (setf rthm-seqs 
                  (subseq 
                   (get-data-from-palette
                    (flatten (list section voice-name))
                    (rthm-seq-map sc))
                   (1- from-sequence)
                   (1- (+ from-sequence section1-num-seqs)))))
              (when reset-snds-each-voice
                (reset snds)
                (when snds2
                  (reset snds2)))
              (loop for rs in voice and rs-count from 0 while happy do
                    (setf events-this-rs (length rs))
                    (format t "~%    Processing rthm-seq ~a (~a events)~%"
                            ;; print the rthm-seq id if we're only doing one
                            ;; section otherwise the rthm-seq count
                            (if (= 1 num-sections)
                                (id (nth rs-count rthm-seqs)) 
                              (1+ rs-count))
                            events-this-rs)
                    (when reset-snds-each-rs
                      (reset snds)
                      (when snds2
                        (reset snds2)))
                    ;; (print (length rs))
                    (loop for event in rs and rs-event-count from 0 while happy
                        do
                          ;; (print 'here)
                          (setf snd-group (pop snd-trans)
                                snd (if (and snds2 (= 1 snd-group))
                                        (get-next snds2)
                                      (get-next snds))
                                duration (* duration-scaler
                                            (compound-duration-in-tempo event))
                                skip-this-event (> (random-rep 100.0)
                                                   (interpolate 
                                                    event-count-voice 
                                                    this-play-chance-env 
                                                    :exp play-chance-env-exp))
                                srt (if do-src
                                        (* src-scaler
                                           (src-for-sample-freq 
                                            (if srt-freq
                                                srt-freq
                                              (frequency snd))
                                            (pitch-or-chord event))) 
                                      1.0))
                          (when (zerop srt)
                            (error "slippery-chicken::clm-play: srt=0!"))
                          ;; given the srt, what's the longest output dur
                          ;; this sound can make?  
                          (setf available-dur (/ (duration snd) srt)
                                wanted-duration-string ""
                                input-start (start snd))
                          (when skip-this-event
                            (incf total-skipped))
                          (unless snd
                            (error "slippery-chicken::clm-play: snd is nil!"))
                          (when inc-start
                            (setf latest-possible-start
                              (- (end snd) (* srt duration)))
                            (unless (and (< latest-possible-start (start snd))
                                         (not (zerop (will-be-used snd))))
                              (incf input-start 
                                    (* (has-been-used snd)
                                       (/ (- latest-possible-start (start snd))
                                          (will-be-used snd)))))
                            (incf (has-been-used snd)))
                          (when (> duration available-dur)
                            ;;(warn "slippery-chicken::clm-play: ~
                            ;;     Requested duration ~a > possible ~
                            ;;   duration ~a~%"
                            ;; duration available-dur)
                            (setf wanted-duration duration
                                  wanted-duration-string 
                                  (if duration-run-over
                                      (format nil " (~,3f available but ~
                                                    duration-run-over is t)"
                                              available-dur)
                                    (format nil " (wanted ~,3f)"
                                            wanted-duration)))
                            (unless duration-run-over
                              (setf duration available-dur)))
                          (when (< duration 0)
                            (warn "slippery-chicken::clm-play: ~
                                  Duration < 0  ?????~%"))
                          (unless (start-time event)
                            (error "~a~%slippery-chicken::clm-play: ~
                                   no start time!!!" event))
                          (setf output-start (+ time-offset
                                                (- (start-time event)
                                                   first-event-start)))
                          (when (> output-start max-start-time)
                            (setf happy nil))
                          (when happy
                            (format t "        ~a/~a Events: ~a~
                                 ~%             ~a ~a~
                                 ~%             start-time ~,3f, input-start: ~
                                 ~,3f, ~
                                 ~%             duration ~,3f~a, ~
                                 ~%             amp ~,2f, srt ~,2f ~
                                 (pitch-or-chord ~,3f,sample freq ~,3f)~%"
                                    event-count total-events
                                    (if skip-this-event "Skipped" "Output")
                                    (path snd) 
                                    (if snds2
                                        (format nil "(snd-group ~a)" 
                                                (1+ snd-group))
                                      "")
                                    output-start 
                                    input-start duration wanted-duration-string
                                    (amplitude snd) srt 
                                    (frequency (pitch-or-chord event))
                                    (frequency snd)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                          (unless (or simulate skip-this-event (not happy)
                                      (zerop duration))
                            (clm::samp5 (path snd)
                                        output-start
                                        :duration duration
                                        :start input-start
                                        :srt srt
                                        :width src-width
                                        :amp (amplitude snd)
                                        :amp-env amp-env
                                        #| 2/8/05: what was the idea
  behind this???
  (print (if (zerop input-start)
                                        amp-env
                                        (clm::envelope-concatenate
                                        (list 0 0 
                                        (/ (third amp-env)
                                        30.0)
                                        (fourth amp-env))
                                        amp-env)))
  |#
                                        :degree
                                        ;; 2/8/05: put mono and stereo
                                        ;; files in random space
                                        ;; NB A sound is always put
                                        ;; between two speakers but it
                                        ;; could be two of any number;
                                        ;; see samp5.lsp for details.
                                        (nth (random 7) '(15 25 35 45 55
                                                          65 75))
                                        #|
  (if (stereo snd)
                                        45
                                        (+ min-degree 
                                        (random 
                                        (1+ (- 90 (* 2 min-degree))))))
  |#
                                        :rev-amt rev-amt
                                        :printing print-secs))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                          (incf event-count-voice)
                          (incf event-count))))))
    (unless (zerop total-events)
      (format t "~%~%~d/~d events skipped (~f%)"
              total-skipped total-events 
              (* 100.0 (/ total-skipped total-events))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Just calls the -aux method once for each section required.  See comments to
;;; that method for parameter explanation.

(defmethod get-events-start-time-duration
    ((sc slippery-chicken)
     start-section
     voices
     &key 
     (ignore-grace-notes nil)
     (time-scaler 1.0)
     (from-sequence 1)
     (num-sequences nil)
     (ignore-rests nil)
     (num-sections 1)
     (get-time-sig-changes nil)
     (include-rests nil))
  (unless num-sections
    (setf num-sections (get-num-top-level-sections sc)))
  (let* ((sections (get-section-refs sc start-section num-sections))
         (all-sections
          (loop for section in sections
              collect (get-events-start-time-duration-aux
                       sc section voices 
                       :ignore-grace-notes ignore-grace-notes 
                       :time-scaler time-scaler
                       ;; of course the next two shouldn't be
                       ;; necessary when we're doing more than one
                       ;; section ...
                       :from-sequence from-sequence
                       :num-sequences num-sequences
                       :get-time-sig-changes get-time-sig-changes
                       :ignore-rests ignore-rests
                       :include-rests include-rests)))
         (voices (loop for i below (length voices) collect
                       (loop for j below num-sections
                                         ;; why do we append sections but
                                         ;; collect rthm-seqs?
                           appending (nth i (nth j all-sections))))))
    (handle-grace-notes voices)
    voices))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; For one section, returns a list of lists, one for each voice,
;;; containing lists of event objects, one for each rthm-sequence.

(defmethod get-events-start-time-duration-aux
    ((sc slippery-chicken)
     ;; a list used as the ids for the call to get-data from the <sequences>.
     ;; If a single reference then it will be forced into a list.
     section
     ;; these voices are used to get the actual sequence orders i.e. each
     ;; voice will be appended to <section> when calling get-data
     voices
     &key 
     (ignore-grace-notes nil)
     (time-scaler 1.0)
     (from-sequence 1)
     (num-sequences nil)
     (ignore-rests nil)
     (get-time-sig-changes nil)
     (include-rests nil))
  (let* ((section-list (if (listp section) section (list section)))
         (seqs-all-ins (data (get-data section-list (piece sc)))))
    ;; (print num-sequences)
    (loop for voice in voices 
        for sqces = (data (get-data voice seqs-all-ins t))
                    ;; no longer call get-timings-from-rsp rather get the
                    ;; timings directly from the bars of the sequenzes, not
                    ;; using a tempo argument any more but using a time-scaler
                    ;; instead.  Now that we're using the piece to get the seqs
                    ;; from instead of the rthm-seq-map, we should have updated
                    ;; time information.  What we have to do though is scale
                    ;; the times and durations of each event and add the
                    ;; durations of rests to the previous note if ignore-rests
                    ;; is t.
        for events = 
          (loop for i from (1- from-sequence) to 
                (if num-sequences
                    (+ (1- from-sequence)
                       (1- num-sequences))
                  (1- (length sqces)))
              collect (get-timings (nth i sqces) time-scaler ignore-rests
                                   get-time-sig-changes include-rests 
                                   ignore-grace-notes))
        when (first events)
        collect events)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; returns a list containing each voice: each voice has sublists of rthm-seqs:
;;; each rthm-seq contains the events.

(defmethod get-events-with-src
    ((sc slippery-chicken) from-section voices
     &key 
     (ignore-grace-notes nil)
     (time-scaler 1.0)
     (from-sequence 1)
     (num-sequences nil)
     (num-sections 1)
     (ignore-rests nil)
     ;; (pitch-object-default-src-ref-pitch 'c4) if chords is nil then we
     ;; use the chords in the set-map
     (chords nil)
     ;; sometimes the chord stored in the palette is not a simple list of
     ;; data so we need to access the nth of the chord list
     (chord-accessor nil)
     ;; once we get to the chord, we have a list of notes, now we need the
     ;; reference to the specific note for the voices in this note list.
     ;; Either this should be single reference whereupon the first voice
     ;; will be this nth, the second this plus 1 etc., or this should be a
     ;; list.
     (note-number 0)) ;; 0-based!!!
  (let* ((sections (get-section-refs sc from-section num-sections))
         (all-sections
          (loop for section in sections
              collect (get-events-with-src-aux
                       sc section voices 
                       :ignore-grace-notes ignore-grace-notes
                       :time-scaler time-scaler
                       :from-sequence from-sequence
                       :num-sequences num-sequences
                       :ignore-rests ignore-rests
                       :chords chords
                       :chord-accessor chord-accessor
                       :note-number note-number))))
    (loop for i below (length voices) collect
          (loop for j below num-sections
              for data = (nth i (nth j all-sections))
                         #|
  unless data do 
  (error "slippery-chicken::get-events-with-src: ~
                         no data (i = ~a j = ~a)" i j)
  |#
              when data
              append data))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-events-with-src-aux 
  ((sc slippery-chicken) section voices
   &key 
   (ignore-grace-notes nil)
   (time-scaler 1.0)
   (from-sequence 1)
   (num-sequences nil)
   (ignore-rests nil)
   (chords nil)
   (chord-accessor nil)
   (note-number 0));; 0-based!!!
  (unless num-sequences
    (setf num-sequences (num-seqs sc section)))
  (let ((timings (get-events-start-time-duration-aux
                  sc section voices 
                  :ignore-grace-notes ignore-grace-notes
                  :time-scaler time-scaler
                  :from-sequence from-sequence
                  :num-sequences num-sequences
                  :ignore-rests ignore-rests))
        (note-numbers (if (listp note-number) 
                          note-number 
                        (loop for i from 0 repeat (length voices)
                              collect (+ note-number i))))
        (chds 
         (if chords chords
           (let ((chord-refs 
                  (subseq
                   (data (get-data section (set-map sc)))
                   (1- from-sequence)
                   (1- (+ from-sequence num-sequences)))))
             (loop for ref in chord-refs collect
                   (data (get-data ref (set-palette sc))))))))
    (when chord-accessor
      (setf chds (loop for i in chds collect (nth chord-accessor i))))
    (unless (= num-sequences (length chds))
      (error "slippery-chicken::get-events-with-src: ~
              There must be a chord for every sequence: ~a" 
             chds))
    (unless (= (length voices) (length note-numbers))
      (error "slippery-chicken::get-events-with-src: ~
              When :note-numbers is a list, ~
              then it must be of the same length as :voices"))
    (loop for voice in timings and n in note-numbers do
          (loop for chord in chds 
                for num-notes = (length chord)
                for rs in voice 
                do
                (unless (simple-listp chord)
                  (error "slippery-chicken::get-events-with-src: ~
                          Each chord must be a simple list of notes. ~
                          ~%Perhaps you forgot the set the :chord-accessor? ~
                          ~%~a" chord))
                (loop for event in rs 
                      ;; just in case there's less notes in the chord than
                      ;; there are voices... 
                      for pitch = (nth (mod n num-notes) chord) do
                      (unless pitch
                        (error "slippery-chicken::get-events-with-src: ~%~
                                Pitch is NIL!!!  Probably the reference ~
                                given in :note-number is out of ~%range for ~
                                the chosen chord.  ~%Current reference is ~
                                ~a into the chord ~a"  
                               n chord))
                      (setf (pitch-or-chord event) (clone pitch)))))
    timings))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Use the staff-groupings slot to get the instrument reference for each
;;; instrument that is first (top) in the group.

(defmethod get-groups-top-ins ((sc slippery-chicken))
  (loop 
      with nth = 0
      with players = (get-players (ensemble sc))
      for g in (staff-groupings sc)
      collect
        (nth nth players)
      do
        (incf nth g)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This is different from set-rehearsal-letters in that we don't use the
;;; rehearsal-letters slot of sc, rather, we use the method argument.

(defmethod set-rehearsal-letter ((sc slippery-chicken) bar-num letter
                                 &optional players)
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

;;; 10/3/07: the rehearsal-letters slot lists the bar numbers where cmn should
;;; automatically create letters.  Set the rehearsal-letter slot of the correct
;;; bars for the given player(s).
;;; e.g. (set-rehearsal-letters sc (get-groups-top-ins sc))))

(defmethod set-rehearsal-letters ((sc slippery-chicken) &optional players)
  (unless players
    (setf players (players sc)))
  (unless (listp players)
    (setf players (list players)))
  (loop 
      for bar-num in (rehearsal-letters sc)
                     ;; we have to set the rehearsal letter on the bar
                     ;; line of the previous bar
      for dc from 10 ; todo: should really miss out letter I....
      for letter = (format nil "~a" (digit-char dc 36))
      do 
        (when (> dc 35)
          (error "slippery-chicken::set-rehearsal-letters: ~
                  Can only make rehearsal letters from A-Z"))
        (loop 
            for player in players 
            for bar = (get-bar sc (1- bar-num) player)
            do
              (unless bar
                (error "slippery-chicken::set-rehearsal-letters: couldn't get ~
                        bar ~a for ~a." bar-num player))
              (setf (rehearsal-letter bar) letter)))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod delete-rehearsal-letter ((sc slippery-chicken) bar-num
                                    &optional players)
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

;;; update-write-time-sig only looks at the first bar of a seq so if we want to
;;; look at all bars and time write-time-sig accordingly call this method.

(defmethod set-write-time-sig ((sc slippery-chicken))
  (loop for player in (players sc) do
        (loop 
            with last-bar
            for bar-num from (start-bar (piece sc)) to (end-bar (piece sc))
            for bar = (get-bar sc bar-num player)
            do
              (setf (write-time-sig bar) (or (not last-bar)
                                             (not (eq t (time-sig-equal
                                                         bar last-bar))))
                    last-bar bar)))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; This function will only combine short bars into longer ones, it won't split
;;; up bars and recombine. 
;;; todo: re-beam the bar?
;;; todo: this makes it impossible to display the sets: fix this.

(defmethod re-bar ((sc slippery-chicken)
                   &key start-bar 
                        end-bar
                        ;; the following is just a list like '(3 8) '(5 8)
                        min-time-sig
                        verbose
                        (check-ties t)
                        ;; could also be a beat rhythmic unit
                        (auto-beam t))
  (unless start-bar
    (setf start-bar 1))
  (unless end-bar
    (setf end-bar (num-bars sc)))
  (object-is-nil? min-time-sig "slippery-chicken::re-bar" 'min-time-sig)
  ;; make double bar at end of piece a normal bar line
  (change-bar-line-type sc (num-bars (piece sc)) 0)
  (loop 
      for section-no in (data (piece sc)) 
      for section = (data section-no)
      do
        (when (and (<= start-bar (end-bar section))
                   (>= start-bar (start-bar section)))
          (re-bar section :start-bar start-bar :end-bar end-bar
                  :min-time-sig min-time-sig :verbose verbose
                  :auto-beam auto-beam)))
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
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod find-rehearsal-letters ((sc slippery-chicken))
  (loop 
      with player = (first (get-groups-top-ins sc))
      for bnum from 1 to (num-bars sc) 
      for bar = (get-bar sc bnum player)
      when (rehearsal-letter bar)
      collect (1+ bnum)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod auto-beam ((sc slippery-chicken) &optional (beat nil) (check-dur t))
  (loop for player in (players sc) do
        (loop 
            for bnum from 1 to (num-bars sc) 
            for bar = (get-bar sc bnum player)
            do
              (auto-beam bar beat check-dur))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 9.4.11 NB won't allow notes to be under more than one slur/phrase mark
(defmethod check-slurs ((sc slippery-chicken))
  (loop for player in (players sc) do
     ;; reset to the first event
       (next-event sc player nil t)
       (loop
          with in-slur
          for e = (next-event sc player)
          while e
          do
          (cond ((begin-slur-p e)
                 (if in-slur
                     (warn "slippery-chicken::check-slurs (~a): begin slur ~
                              at bar ~a but already began slur at bar ~a"
                           player (bar-num e) in-slur)
                     (setf in-slur (bar-num e))))
                ((end-slur-p e)
                 (if in-slur
                     (setf in-slur nil)
                     (warn "slippery-chicken::check-slurs (~a): end slur at ~
                              bar ~a but no begin slur"
                           player (bar-num e)))))
          finally
          (when in-slur
            (warn "slippery-chicken::check-slurs (~a): end slur missing at ~
                     end of piece" player)))))
              

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Just for checking really but if same-spellings all ties will be forced to
;;; the same spellings. 
;;; 
;;; cf piece::handle-ties

(defmethod check-ties ((sc slippery-chicken) &optional same-spellings)
  (loop for player in (players sc) do
     ;; reset to the first event
       (next-event sc player nil t)
       (loop
          with last = (next-event sc player)
          for this = (next-event sc player)
          while this
          do
          (when (and same-spellings
                     (is-tied-to this))
            ;; 24.3.11
            (unless (pitch-or-chord last)
              (error "slippery-chicken::check-ties (~a): <this> is tied-to ~
                          but <last> has no pitch.~%THIS:~%~a~%LAST:~%~a"
                     player this last))
            (setf (pitch-or-chord this) (clone (pitch-or-chord last)))
            (when (written-pitch-or-chord this)
              (setf (written-pitch-or-chord this) 
                    (clone (written-pitch-or-chord last)))))
          (when (or (and (is-tied-from last)
                         (not (is-tied-to this)))
                    (and (is-tied-to this)
                         (not (is-tied-from last))))
            (warn "slippery-chicken::check-ties: bad tie, ~a bar ~a" 
                  player (next-event sc nil)))
          (setf last this))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* slippery-chicken/rebar
;;; FUNCTION
;;; rebar:
;;;
;;; See documentation in piece class method.
;;; 
;;; RETURN VALUE: 
;;; always t
;;; 
;;; SYNOPSIS
(defmethod rebar ((sc slippery-chicken) 
                  &optional instruments-hierarchy rebar-fun)
;;; ****
  (rebar (piece sc) (if instruments-hierarchy
                        instruments-hierarchy
                        (instruments-hierarchy sc))
         rebar-fun)
  ;; got to renumber the bars
  (update-slots sc)
  ;; got to call this here rather than in piece class
  (update-write-time-sig2 (piece sc))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod section-num-rthm-seqs ((sc slippery-chicken) section)
  (num-rthm-seqs (get-data (list section (first (instruments-hierarchy sc)))
                           (piece sc))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 26/4/10: called automatically from cmn-display and init

(defmethod cleanup-rest-bars ((sc slippery-chicken) 
                              &optional start-bar end-bar players)
  (unless start-bar
    (setf start-bar 1))
  (unless end-bar
    (setf end-bar (num-bars sc)))
  (unless players
    (setf players (players sc)))
  (unless (listp players)
    (setf players (list players)))
  (loop for player in players do
       (loop for bar-num from start-bar to end-bar 
          for bar = (get-bar sc bar-num player)
          do
          (when (all-rests bar)
            (force-rest-bar bar))))
  t)
            

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 28/1/11: make sure that every bar in the piece has the same time signature
;;; for each instrument in the ensemble
(defmethod check-time-sigs ((sc slippery-chicken))
  (loop for bar-num from 1 to (num-bars sc) 
     for bars = (get-bar sc bar-num) ;; gets bars for all players
     for ts1 = (get-time-sig (first bars))
     do
     (loop for pbar in (rest bars) do
          (unless (time-sig-equal ts1 (get-time-sig pbar))
            (error "slippery-chicken::check-time-sigs: time signatures ~
                    are not the same at bar ~a" bar-num)))))
         

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod player-doubles ((sc slippery-chicken) player)
  (let ((player-obj (get-data player (ensemble sc))))
    (unless player-obj
      (error "slippery-chicken::player-doubles: can't get player ~a" player))
    (doubles player-obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-starting-ins ((sc slippery-chicken) player) ; symbol
  (let ((player-obj (get-data player (ensemble sc)))
        (ins-ref nil))
    (when (doubles player-obj)
      (setf ins-ref (get-first-for-instrument
                     (instrument-change-map sc)
                     player))
      (unless ins-ref
        (error "piece::get-starting-ins: ~a doubles, enter data into ~
                instrument-change-map"
               player)))
    ;; returns an instrument object
    (player-get-instrument player-obj 
                           ins-ref nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; sort notes in piece--across all instruments--into time-ordered lists and
;;; process them with the given function, which must take one argument, an
;;; event.
(defmethod process-events-by-time ((sc slippery-chicken) function
                                   &key (start-bar 1) end-bar)
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

(defmethod sc-move-dynamic ((sc slippery-chicken) bar-num player
                            ;; event numbers 1-based but counting rests and ties
                            from to &optional to-bar)
  (unless to-bar
    (setf to-bar bar-num))
  (let ((dyn (sc-remove-dynamic sc bar-num player from)))
    (add-cmn-mark (get-event sc to-bar to player) dyn)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 1.4.11: remove all dynamics on a single event.  event-num includes ties and
;;; rests 
(defmethod sc-remove-dynamic ((sc slippery-chicken) bar-num player
                              &rest event-nums)
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

;;; 16.3.11: start end are either bar numbers or (bar-num note-num) pairs.
;;; note-nums are 1-based and count ties but not rests.
(defmethod sc-remove-dynamics ((sc slippery-chicken) start end players)
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

;;; if two or more notes have the same dynamic, remove all but the first
(defmethod remove-extraneous-dynamics ((sc slippery-chicken))
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

;;;  lilypond
(defmethod write-lp-data-for-all ((sc slippery-chicken) base-path
                                  &key start-bar end-bar (paper "a4") landscape
                                  ;; if a list, then these are the enharmonic
                                  ;; corrections
                                  (respell-notes t) 
                                  ;; automatically add clefs to instruments who
                                  ;; read more than one? 
                                  (auto-clefs t)
                                  (staff-size 14)
                                  ;; parts will always be transposed but score
                                  ;; can be in in C or not
                                  (in-c nil)
                                  (barline-thickness 0.5)
                                  (top-margin 10) ; mm
                                  (bottom-margin 10) ; mm
                                  (left-margin 20) ;mm
                                  (line-width 17) ;cm
                                  (page-nums t)
                                  ;; print every bar number unless
                                  ;; multi-bar-rest?
                                  (all-bar-nums nil)
                                  ;; this has to be T if we're going to get
                                  ;; letters in the parts--but CMN printing
                                  ;; will have all parts all letters too
                                  ;; thereafter 
                                  (rehearsal-letters-all-players t)
                                  ;; set to t if using bartok pizz and
                                  ;; othersigns  
                                  (use-custom-markup nil)
                                  (rehearsal-letters-font-size 18)
                                  (lp-version "2.14.2") ;"2.12.3")
                                  ;; 24.7.11 (Pula) barlines through whole
                                  ;; staff group or just a stave
                                  (group-barlines t)
                                  ;; 5.11.11 set to t if you want lilypond to
                                  ;; optimize page breaks for page turns in
                                  ;; parts 
                                  (page-turns nil)
                                  ;; minimum rest necessary to do a page turn;
                                  ;; something like a time signature e.g. (2 1)
                                  ;; would mean we need a min. of 2 whole rests
                                  (min-page-turn '(2 1))
                                  ;; sim to rehearsal letters
                                  (tempi-all-players t))
  (declare (special cl-user::*slippery-chicken-src-path*))
  (when (and (numberp start-bar) (numberp end-bar) (>= start-bar end-bar))
    (error "slippery-chicken::write-lp-date-for-all: start-bar = ~a, ~
            end-bar = ~a???" start-bar end-bar))
  (let* ((path (trailing-slash base-path))
         (players (players sc))
         (def-file (string-downcase (format nil "~a-def.ly" (title sc))))
         (staff-group (if group-barlines "StaffGroup" "ChoirStaff"))
         (players-strings
          (loop for player in (players sc)
             ;; lilypond has trouble with variable names containing - or _
             collect (remove #\_ 
                             (remove #\- 
                                     (string-downcase (string player))))))
         (def-file-path (concatenate 'string path def-file)))
    (labels ((no-header-footer (stream)
               (format stream 
                       "~%\\header {~%  tagline = ##f~%  composer = ##f~%}"))
             (new-voice (pname player stream &optional include-name) 
               ;; pname must be the same as the file name we'll write with the
               ;; notes (+ .ly) so no - or _
               (let* ((ins (get-starting-ins sc player)))
                 (format stream "~%~a = \\new Voice " pname)
                 (when page-turns
                   (princ "\\with { \\consists \"Page_turn_engraver\" }"
                          stream))
                 (princ " {" stream)
                 (when (staff-name ins)
                   (format stream " ~%  ~a"
                           (lp-set-instrument (staff-name ins))))
                 (when (staff-short-name ins)
                   (format stream "~%  ~a ~%"
                           (lp-set-instrument (staff-short-name ins) t)))
                 (princ "  \\compressFullBarRests" stream)
                 ;; change the thickness of the barlines globally
                 (format stream "~&  \\override Score.BarLine ~
                                 #'hair-thickness = #~a" barline-thickness)
                 (when page-turns
                   (format stream "~%  \\set Staff.minimumPageTurnLength = ~
                                   #(ly:make-moment ~a ~a)"
                           (first min-page-turn) (second min-page-turn)))
                 (format stream "~%  \\include \"~a-~a.ly\"~%}"
                         (title sc) (if include-name include-name pname))))
             (score-tag (pname stream &optional new-staff-group end-staff-group)
               (when new-staff-group
                 ;; 24.7.11: to avoid barlines across groups
                 (format stream "~%  \\new ~a <<" staff-group))
               (format stream "~%    \\tag #'score \\tag #'~a \\new Staff"
                       pname)
               (format stream "~%    { << \\global #(set-accidental-style ~
                      'modern-cautionary) \\~a >> }" pname)
               (when end-staff-group
                 (format stream "~%  >>")))
             (needs-transposition (player) ; symbol
               (and in-c (plays-transposing-instrument 
                          (get-data player (ensemble sc)))))
             (written-pname (pname)
               (concatenate 'string pname "Written"))
             (part (pname stream &optional (score-tag-var "music"))
               (format stream "~&\\version \"~a\"" lp-version)
               (format stream "~&\\include \"~a\"~%" def-file)
               (no-header-footer stream)
               (terpri stream)
               (princ "\\score {" stream)
               (format stream "~&  \\new ~a \\keepWithTag #'~a \\~a"
                       ;; todo: got to use the written part if transposing
                       staff-group pname score-tag-var)
               (format stream "~%  \\layout { }~%}")))
      (when respell-notes
        (respell-notes sc respell-notes))
      (when auto-clefs
        (format t "~&Inserting automatic clefs....")
        (auto-clefs sc :players players :verbose nil :in-c in-c
                    :delete-cmn-objects-before nil))
      (when rehearsal-letters-all-players 
        (format t "~&Setting rehearsal letters....")
        (set-rehearsal-letters sc players))
      (when tempi-all-players 
        (format t "~&Updating tempo of events....")
        (update-events-tempo sc players))
      ;; this will set the multi-bar-rest slot of the bars; NB it must come
      ;; after rehearsal letters and tempi
      (multi-bar-rests sc)
      ;; write the definitions file
      (with-open-file 
          (out def-file-path :direction :output :if-does-not-exist :create
               :if-exists :rename-and-delete)
        ;; print would print the " marks hence princ
        (princ "\\include \"english.ly\"" out)
        (format out "~%~%\\paper { ~%  #(set-paper-size \"~a\"~a)" 
                paper (if landscape " 'landscape" ""))
        (when page-turns
          (terpri out)
          (princ "  #(define page-breaking ly:page-turn-breaking)" out))
        (format out "~%  page-limit-inter-system-space = ##t")
        (unless page-nums 
          (format out "~%  print-page-number = ##f"))   
        (format out "~%  top-margin = ~a\\mm" top-margin)
        (format out "~%  bottom-margin = ~a\\mm" bottom-margin)
        (format out "~%  left-margin = ~a\\mm" left-margin)
        (format out "~%  line-width = ~a\\cm" line-width)
        ;; paper } closed here
        (format out "~%}~%~%#(set-global-staff-size ~a)~%~%" staff-size)
        (when use-custom-markup
          (format out "~%\\include \"~alilypond.ly\"~%~%"
                  cl-user::*slippery-chicken-src-path*))
        (princ "global = {" out) 
        (terpri out)
        (princ "  \\key c \\major" out)
        (terpri out)
        (princ "}" out)
        (terpri out)
        (terpri out)
        (loop for pname in players-strings
           for player in (players sc) do
           (when (needs-transposition player)
             (new-voice (written-pname pname) player out 
                        (concatenate 'string pname "-written")))
           (new-voice pname player out))
        (terpri out)
        (format out "~%music = {~%  <<")
        ;; write the music variable, staff groupings etc.
        (loop with groups = (copy-list (staff-groupings sc))
           with gnum = (pop groups)
           with gcount = 1
           for pname in players-strings
           ;; this must come after 'in players-strings' otherwise we crash
           for end = (= gcount gnum) do
             ;; (format t "~%~a ~a ~a" pname gcount gnum)
             (score-tag pname out (= 1 gcount) end)
             (if end
                 (setf gnum (pop groups)
                       gcount 1)
                 (incf gcount)))
        (format out "~%  >>~%}")
        ;; create the written parts variable
        (format out "~%written = {~%  <<")
        (loop for pname in players-strings
           for player in (players sc) do
           (when (needs-transposition player)
             (score-tag (written-pname pname) out)))
        (format out "~%  >>~%}"))
      ;; write the main score file
      (with-open-file
          (out 
           (concatenate 'string path (string-downcase 
                                      (format nil "_~a-score.ly" (title sc))))
           :direction :output :if-does-not-exist :create
           :if-exists :rename-and-delete)
        (format out "~&\\version \"~a\"" lp-version)
        (format out "~%\\include \"~a\"" def-file)
        (no-header-footer out)
        (format out "~%\\score {~%  \\keepWithTag #'score ~
                   \\music")
        (format out "~%  \\layout { }~%}~%"))
      ;; write the parts
      (loop for player in (players sc)
         for pname in players-strings do
         (with-open-file 
             (out 
              (concatenate 'string path (string-downcase 
                                         (format nil "~a-~a-part.ly" 
                                                 (title sc) pname)))
              :direction :output :if-does-not-exist :create
              :if-exists :rename-and-delete)
           (if (needs-transposition player)
               (part (written-pname pname) out "written")
               (part pname out))))
      ;; write the notes to individual files
      (loop for player in (players sc)
         for pname in players-strings do
         (write-lp-data-for-player 
          sc player 
          (concatenate 'string path (format nil "~a-~a.ly" (title sc) pname))
          :all-bar-nums all-bar-nums
          :rehearsal-letters-font-size rehearsal-letters-font-size
          :in-c in-c :start-bar start-bar :end-bar end-bar))
      ;; got to write the written (i.e. not sounding) notes for the part
      ;; can't do this in the above loop as we have to re-call auto-clefs
      ;; making sure we don't use the in-c clefs for the instrument
      (when auto-clefs
        (auto-clefs sc :players players :verbose nil :in-c nil
                    :delete-cmn-objects-before nil))
      (loop for player in (players sc)
         for pname in players-strings do
         ;; got to write the written (i.e. not sounding) notes for the part
         (when (needs-transposition player)
           (write-lp-data-for-player 
            sc player 
            (format nil "~a~a-~a-written.ly" path (title sc) pname)
            :all-bar-nums all-bar-nums :in-c nil :start-bar start-bar
            :end-bar end-bar)))))
  t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod write-lp-data-for-player ((sc slippery-chicken) player path
                                     &key start-bar end-bar in-c
                                     ;; print every bar number unless
                                     ;; multi-bar-rest?
                                     all-bar-nums 
                                     rehearsal-letters-font-size)
  (unless start-bar
    (setf start-bar 1))
  (unless end-bar
    (setf end-bar (num-bars sc)))
  (with-open-file
      (out path :direction :output :if-does-not-exist :create
           :if-exists :rename-and-delete)
    (let* ((player-obj (get-data player (ensemble sc)))
           (transposing (plays-transposing-instrument player-obj))
           (ins-obj (get-starting-ins sc player))
           (clef (starting-clef ins-obj)))
      (when all-bar-nums
        (format out "~&\\override Score.BarNumber ~
                     #'break-visibility = #'#(#t #t #t)")
        ;; bar numbers centered over barline
        (format out "~&\\override Score.BarNumber #'self-alignment-X = #0"))
      ;; just write this in all parts, whether there's pedalling or not--does
      ;; no harm  
      (format out "~&\\set Staff.pedalSustainStyle=#'mixed")
      ;; 28.7.11 (Pula)
      (format out "~&\\autoBeamOff")
      (format out "~&\\clef ~a" (string-downcase (format nil "~a" clef)))
      (loop for bar-num from start-bar to end-bar
         for rsb = (get-bar sc bar-num player)
         for lp-data = (get-lp-data rsb (or in-c (not transposing)) 
                                    rehearsal-letters-font-size)
         do
         (format out "~&% bar ~a~%" bar-num)
         (loop for data in lp-data do
              (when data
                ;;(format out "~a " data))))))
                (format out data))))))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; start/end-note are 1-based but count ties.  if no optional args, deletes
;;; all beams in the bar.
(defmethod sc-delete-beams ((sc slippery-chicken) bar-num player
                            &optional start-note end-note)
  (let ((bar (get-bar (piece sc) bar-num player)))
    (if (and start-note end-note)
        (progn
          (delete-beam (get-nth-non-rest-rhythm (1- start-note) bar))
          (delete-beam (get-nth-non-rest-rhythm (1- end-note) bar)))
        (delete-beams bar))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; NB This might delete rehearsal letters, instrument changes (and maybe other
;;; things) attached to a bar/event.
(defmethod delete-bars ((sc slippery-chicken) start-bar
                        &key num-bars end-bar print)
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

(defmethod delete-bars-aux ((sc slippery-chicken) start-bar num-bars player
                            print)
  (loop with seq-num-bars with num-deleted until (zerop num-bars) do
       (multiple-value-bind (seq nth-bar)
           (get-sequenz-from-bar-num (piece sc) start-bar player)
         (unless seq
           (error "slippery-chicken::delete-bars-aux: couldn't get seq ~
                   at bar ~a" start-bar))
         (when print
           (print-simple seq))
         (setf seq-num-bars (num-bars seq)
               num-deleted (min num-bars (- seq-num-bars nth-bar)))
         (when (zerop num-deleted)
           (error "slippery-chicken::delete-bars-aux: num-deleted = 0!"))
         (if (= seq-num-bars num-deleted) 
             ;; delete the whole sequenz
             (delete-sequenzes (piece sc) start-bar player)
             (delete-bars seq nth-bar :num num-deleted))
         ;; have to call this here to get proper bar numbers and rthm-seq-bar
         ;; data.  sadly we don't have method to do this only for this
         ;; player...todo? 
         (update-slots sc)
         ;; don't inc start-bar as bar-nums are adjusted via update-slots
         ;; (incf start-bar num-deleted)
         (decf num-bars num-deleted)
         (when print
           (format t "~%num-deleted: ~a, num-bars in seq: ~a, nth-bar: ~a, ~
                      num-bars: ~a"
                   num-deleted (num-bars seq) nth-bar num-bars))))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 13.2.11 have players play the same rthm-seq (rhythmic doubling only, not
;;; pitch)

(defmethod double ((sc slippery-chicken) section-ref start-seq end-seq
                   master-player doubling-players)
  (double (rthm-seq-map sc) section-ref start-seq end-seq master-player
          doubling-players))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 20.7.11 (Pula)
;;; see double-events (below) for details
(defmethod move-events ((sc slippery-chicken) from-player to-player
                        start-bar start-event end-bar end-event
                        &key transposition (consolidate-rests t))
  (double-events sc from-player to-player start-bar start-event
                 end-bar end-event :transposition transposition)
  ;; now delete the events in the from-player
  (delete-events sc start-bar start-event end-bar end-event from-player
                 consolidate-rests))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;               20.7.11 (Pula)
;;; start/end-event are 1-based and count rests and ties, not just struck notes
;;; if end-event is nil we use all events until end of end-bar
;;; if update we update-slots for the whole sc object
;;; 
;;; a nasty side-effect at the moment is that any existing events in the
;;; doubling players at the beginning of the start-bar or end of the end-bar
;;; will be deleted, so this only works for copying notes into completely empty
;;; bars, not razor splicing.

(defmethod double-events ((sc slippery-chicken) master-player doubling-players
                          start-bar start-event end-bar end-event
                          &key transposition (consolidate-rests t) (update t))
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
;;; 21.7.11 (Pula)
;;; turn notes into rests.
;;; start/end-event are 1-based and count rests and ties, not just struck notes
;;; if players is nil, process all players
;;; if end-event is nil go to the end of the end-bar
(defmethod delete-events ((sc slippery-chicken) start-bar start-event end-bar
                          end-event &optional players (consolidate-rests t))
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

;;; 23.7.11 (Pula) 1-based and counting tied notes but not rests
;;; NB in general calling auto-beam is a good idea (esp. if you're deleting
;;; notes under a beam) but if might fail if you have notes longer than a beat.
(defmethod sc-force-rest ((sc slippery-chicken) bar-num note-num player
                          &optional (auto-beam nil))
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

;;; delete any notes in the existing bars
;;; start-bar and end-bar are inclusive
(defmethod force-rest-bars ((sc slippery-chicken) start-bar end-bar players)
  (loop for bar-num from start-bar to end-bar do
       (loop for player in players 
            for bar = (get-bar sc bar-num player)
            do
            (unless bar
              (error "slippery-chicken::force-rest-bars: no bar ~a for ~a"
                     bar-num player))
            (force-rest-bar bar))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 20.8.11: if no end-event we process all events in the last bar
(defmethod force-artificial-harmonics ((sc slippery-chicken) player start-bar
                                       start-event end-bar &optional end-event)
  (loop for e in (get-events-from-to sc player start-bar start-event end-bar
                                     end-event)
       do
       (unless (is-rest e)
         (force-artificial-harmonic e)))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 22.7.11 (Pula)
;;; all 1-based and inclusive
(defmethod get-events-from-to ((sc slippery-chicken) player start-bar
                               start-event end-bar &optional end-event)
  (unless end-event
    (setf end-event (num-rhythms (get-bar sc end-bar player))))
  (let ((result '()))
    (loop for bar-num from start-bar to end-bar 
       for bar = (get-bar sc bar-num player)
       do
       (loop for e in (rhythms bar) and e-count from 1 do
            (when (or (and (> bar-num start-bar)
                           (< bar-num end-bar))
                      (and (= start-bar end-bar)
                           (>= e-count start-event)
                           (<= e-count end-event))
                      (and (/= start-bar end-bar)
                           (= bar-num start-bar)
                           (>= e-count start-event))
                      (and (/= start-bar end-bar)
                           (= bar-num end-bar)
                           (<= e-count end-event)))
              (push e result))))
    (nreverse result)))

(defmethod transpose-events ((sc slippery-chicken) player start-bar
                             start-event end-bar end-event semitones
                             &key (destructively t))
  (let ((events (get-events-from-to sc player start-bar start-event end-bar
                                    end-event)))
    (loop for e in events do
         (transpose e semitones :destructively destructively))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 28.9.11: add accidental in ().  note-num (counting ties, and from 1) can be
;;; an integer or list e.g. '(1 2). If the latter it would be the first chord,
;;; second note up.

(defmethod set-cautionary-accidental ((sc slippery-chicken) bar-num note-num
                                      player &optional written) 
  (cautionary-accidental-aux sc bar-num note-num player t written))

(defmethod unset-cautionary-accidental ((sc slippery-chicken) bar-num note-num
                                        player &optional written) 
  (cautionary-accidental-aux sc bar-num note-num player nil written))

(defmethod cautionary-accidental-aux ((sc slippery-chicken) bar-num note-num
                                      player value &optional written)
  (let ((note (get-note sc bar-num note-num player written)))
    (when note
      ;; todo: change this if we change bar-holder::get-note
      (when (event-p note)
        (setf note (if written
                       (written-pitch-or-chord note)
                       (pitch-or-chord note))))
      (when value
        (setf (show-accidental note) t))
      (setf (accidental-in-parentheses note) value))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Related functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-slippery-chicken (name &key rthm-seq-palette rthm-seq-map
                                        set-palette set-map sndfile-palette 
                                        tempo-map tempo-curve snd-output-dir
                                        instrument-change-map 
                                        instruments-write-bar-nums
                                        bars-per-system-map
                                        staff-groupings
                                        rthm-seq-map-replacements
                                        set-map-replacements
                                        set-limits-low set-limits-high
                                        instrument-palette ensemble 
                                        rehearsal-letters fast-leap-threshold
                                        instruments-hierarchy title
                                        (warn-ties t))
  ;; we make the given name a global!!!
  (set name
       (make-instance 'slippery-chicken 
         :id name
         :title title
         :rthm-seq-palette rthm-seq-palette
         :rthm-seq-map rthm-seq-map
         :rthm-seq-map-replacements rthm-seq-map-replacements
         :set-palette set-palette 
         :set-map set-map
         :set-map-replacements set-map-replacements
         :instruments-write-bar-nums instruments-write-bar-nums
         :staff-groupings staff-groupings
         :rehearsal-letters rehearsal-letters
         :instrument-change-map instrument-change-map
         :snd-output-dir snd-output-dir
         :sndfile-palette sndfile-palette
         :instrument-palette instrument-palette
         :tempo-map tempo-map
         :tempo-curve tempo-curve
         :bars-per-system-map bars-per-system-map
         :ensemble ensemble
         :instruments-hierarchy instruments-hierarchy
         :set-limits-low set-limits-low 
         :set-limits-high set-limits-high
         :fast-leap-threshold fast-leap-threshold
         :warn-ties warn-ties)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Use this function to randomly generate the <entry-points> to clm-loops

#+clm
(defun random-loop-points (outfile sndfile 
                           &key 
                           ;; the minimum number of time points for an output
                           ;; loop--number of looped sound segments is 1- this
                           (min-points 5)
                           ;; max number of time points--the actual number of
                           ;; points will be randomly chosen between these two
                           ;; numbers. 
                           (max-points 13)
                           ;; minimum duration of a loop segment--this number
                           ;; will actually be used and scaled by scalers
                           (min-dur 0.05)
                           ;; how many sets of loops should be generated
                           (num-loop-sets 20)
                           ;; scalers for the min-dur: these are all
                           ;; proportions relative to min-dur so if we have
                           ;; 13/8 in this list and min-dur of 0.05 then the
                           ;; duration for such a segment would be 0.08125.
                           ;; these will be chosen at random when calculating
                           ;; the next loop segment duration
                           (scalers '(1/1 2/1 3/2 5/3 8/5 13/8)))
  (let* ((snd-dur (clm::sound-duration sndfile))
         (max-scaler (loop for s in scalers maximize s))
         (max-start (- snd-dur (* min-dur (1- max-points) max-scaler)))
         (num-scalers (length scalers)))
    (with-open-file 
        (out outfile :direction :output :if-does-not-exist :create
         :if-exists :error)
      (format out "(")
      (loop 
          repeat num-loop-sets 
          for num-points = (between min-points max-points)
          for point = (random max-start)
          do
            (format out "~&(")
            (loop 
                repeat num-points
                ;; for scaler = (random-from-list scalers num-scalers)
                ;; for point = start
                do
                  (format out "~,3f " point)
                  (incf point (* min-dur
                                 (random-from-list scalers num-scalers))))
            (format out ")"))
      (format out ")"))))
                  
            
         

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This function was first introduced in the composition "breathing Charlie"
;;; (under the name loops): see charlie-loops.lsp in that project for
;;; examples. 
;;;
;;; sndfile is the path of the file to be looped (mono) 
;;;
;;; entry-points is a list of times, in seconds, where attacks (or something
;;; significant) happen in the file.  These are used to create loop start/end
;;; points.
;;; 
;;; Be careful when doing shuffles as if, e.g., the transpositions list is more
;;; than 6 elements, shuffling will take a very long time.
;;; 
;;; The entry-points are used randomly so that any segment may start at any
;;; point and proceed to any other point (i.e. skipping intervening points,
;;; always forwards however).  There are always two segments in use at any
;;; time.  Which ones are used is selected randomly, then a transition (see
;;; Fibonacci-transitions below) from repeated segment 1 to repeated segment 2
;;; is made.  Then the next segment is chosen and the process is repeated
;;; (i.e. from previous segment 2 to new segment) until we go above
;;; max-start-time.
;;; 
;;; Fibonacci-transitions are first shuffled then made into a circular
;;; list.  Then they are expanded to create the transpositions (each
;;; number becomes a series of 1s and 0s--length is the number
;;; itself--with a transition from all 0s to all 1s: 
;;; e.g. (fibonacci-transition 20) -> (0 0 0 0 1 0 0 1 0 1 0 1 0 1 0 1 0 1 1 1)
;;; This is then used to select one or the other of the current two segments.
;;;
;;; The transpositions are simply randomly permutated and selected.

#+clm
(defun clm-loops (sndfile entry-points &key
                                       (max-perms 1000)
                                       (fibonacci-transitions '(34 21 13 8))
                                       (max-start-time 60.0)
                                       (output-dir "./")
                                       (srate clm::*clm-srate*)
                                       (data-format clm::*clm-data-format*)
                                       (channels 1)
                                       ;; semitones
                                       (transpositions '(0))
                                       ;; added 31/7/05 to vary the order of
                                       ;; entry points, transpositions and
                                       ;; fibonacci-transitions (could be 0!)
                                       (num-shuffles 1) 
                                       (suffix "")
                                       (src-width 5))
  (format t "~&num-shuffles: ~a" num-shuffles)
  (let* ((perms (flatten 
                 ;; inefficient-permutations will always return :max results no
                 ;; matter what the first argument
                 (inefficient-permutations (length entry-points)
                                           :max max-perms)))
         (shuffled (multi-shuffle-with-perms entry-points num-shuffles))
         (srcs (loop for i in transpositions collect (semitones i)))
         (transp-perms (make-cscl
                        (multi-shuffle-with-perms
                         (flatten 
                          (permutations (length transpositions)))
                         num-shuffles)))
         (fts (make-cscl 
               (multi-shuffle-with-perms fibonacci-transitions num-shuffles)))
         (transition nil)
         (output-file (format nil "~a~a-loops-from-~a-~a.wav" 
                              output-dir (pathname-name sndfile) 
                              (secs-to-mins-secs (first entry-points) 
                                                 :same-width t
                                                 :separator "m")
                              suffix))
         (start 0.0)
         (end 0.0)
         (start1 0.0)
         (start2 0.0)
         (end1 0.0)
         (end2 0.0)
         (src 0.0)
         (src1 1.0)
         (src2 (nth (get-next transp-perms) srcs))
         (duration 0.0)
         (output-start 0.0)
         (current-perm 0))
    (labels ((get-entry
                 ()
               (let ((this (nth (mod current-perm max-perms) perms))
                     (next (nth (mod (1+ current-perm) max-perms) perms)))
                 (if (= this next)
                     (progn
                       (incf current-perm)
                       (get-entry))
                   (sort (list (nth this shuffled)
                               (nth next shuffled))
                         #'<))))
             (get-entries ()
               (let ((entry (get-entry)))
                 (setf start1 (first entry)
                       end1 (second entry))
                 ;; so this inc happens only once during selection of the two
                 ;; segments i.e. start1 end1 this time is start2 end2 last
                 ;; time. 
                 (incf current-perm)
                 (setf entry (get-entry)
                       start2 (first entry)
                       end2 (second entry)))))
      (format t "~%Output file will be ~a~%" output-file)
      (clm::with-sound 
          (:scaled-to .99 :play nil :output output-file :channels channels
                      :srate srate
                      :data-format data-format
                      :statistics t) 
        (loop while (<= output-start max-start-time)
            do
              (get-entries)
              (format t "~%~%seg1 [time (nth entry point)]: ~a (~a) -> ~a (~a)~
                           ~%seg2:                          ~a (~a) -> ~a (~a)"
                      start1 (position start1 entry-points)
                      end1 (position end1 entry-points)
                      start2 (position start2 entry-points)
                      end2 (position end2 entry-points))
              (setf transition (fibonacci-transition (get-next fts))
                    src1 src2
                    src2 (nth (get-next transp-perms) srcs))
              (loop 
                  for tr in transition 
                  while (<= output-start max-start-time)
                  do
                    ;; transition is a list of 0s and 1s
                    (if (zerop tr)
                        (setf src src1
                              start start1
                              end end1)
                      (setf src src2
                            start start2
                            end end2))
                    (setf duration (/ (- end start) src))
                    (format t "~%   ~a: src: ~a, dur: ~a, ~a -> ~a" 
                            output-start src duration start end)
                    (clm::samp5 sndfile output-start
                                :printing nil
                                :duration duration
                                :start start
                                :degree (nth (random 5) '(15 30 45 60 75))
                                :srt src
                                :width src-width
                                :amp-env '(0 0 3 1 97 1 100 0))
                    ;; 6/10/06: as long as amp-env above doesn't change the *
                    ;; 0.94 for duration should ensure an overlap--ok, the next
                    ;; duration might be shorter/longer than this so it won't
                    ;; perfectly overlap but it will start before this one
                    ;; finishes. 
                    (incf output-start (* 0.94 duration))))))))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+clm
(defun clm-loops-all (sndfile entry-points-list 
                      &key 
                      (max-perms 1000)
                      (fibonacci-transitions '(34 21 13 8))
                      (max-start-time 60.0)
                      (output-dir "./")
                      (srate clm::*clm-srate*)
                      (data-format clm::*clm-data-format*)
                      (channels 1)
                      (do-shuffles t) ;; see clm-loops
                      ;; exclude all those loops who start before this
                      ;; number of seconds. 
                      (start-after -1.0)
                      (stop-after 99999999.0)
                      (suffix "")
                      ;; semitones
                      ;; 6/10/06: using just one list of transpositions passed
                      ;; onto clm-loops created the same tone structure for
                      ;; every file generated (boring).  This list will now be
                      ;; shuffled and 10 versions collected which will then be
                      ;; passed (circularly) one after the other to clm-loops.
                      (transpositions '(0))
                      (transposition-offset 0.0)
                      (src-width 5))
  (let* ((transps-offset (loop for st in transpositions
                             collect (+ transposition-offset st)))
         (transps-shuffled (make-cscl
                            (loop repeat 10 collect
                                  (shuffle transps-offset :reset nil)))))
    (loop for epl in entry-points-list and i from 1 do
          (when (and (> (first epl) start-after)
                     (<= (first epl) stop-after))
            (clm-loops sndfile epl :max-perms max-perms 
                       :fibonacci-transitions fibonacci-transitions
                       :num-shuffles (if do-shuffles
                                         (mod i 7)
                                       0)
                       :max-start-time max-start-time
                       :channels channels
                       :srate srate
                       :suffix suffix
                       :data-format data-format
                       :output-dir output-dir
                       :transpositions (get-next transps-shuffled)
                       :src-width src-width)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Check that the set-map and rthm-seq-map have the same number of sections,
;;; that the sections have the same names, and that each section has the same
;;; number of sequences in both maps.

(defun check-maps (set-map rthm-seq-map)
  (let* ((set-map-len (sclist-length set-map))
         (rthm-seq-map-len (sclist-length rthm-seq-map)))
    ;; are there the same number of sections?
    (unless (= set-map-len rthm-seq-map-len)
      (error "slippery-chicken::check-maps: Different number of sections ~
              in the maps: set-map ~a rthm-seq-map ~a"
             set-map-len rthm-seq-map-len))
    ;; there should be a set for every rthm-seq
    (loop 
        for i below set-map-len
        for sm-sec = (get-nth i set-map)
        for rsm-sec = (get-nth i rthm-seq-map) 
        for id-sm-sec = (id sm-sec) 
        for first-sm-sec = (data sm-sec)
        for first-rsm-sec = (data (first (data (data rsm-sec))))
        do
          (unless (id-eq id-sm-sec rsm-sec)
            (error "slippery-chicken::check-maps: Names of sections should ~
                    be the same in all maps: set-map ~a rthm-seq-map ~a"
                   id-sm-sec (id rsm-sec)))
          (cond ((and (is-ral first-sm-sec)
                      (is-ral first-rsm-sec))
                 (check-maps (data sm-sec) (data rsm-sec)))
                ((or (is-ral first-sm-sec)
                     (is-ral first-rsm-sec))
                 (error "slippery-chicken::check-maps: rthm-seq-map and ~
                         set-map must have the same recursive structure!"))
                ;; in the set-map, the references are the same for each
                ;; instrument so there's only 1 list 
                (t (let ((len-sm-sec (length (data sm-sec)))
                         ;; there's refs for each instrument in the
                         ;; rthm-seq-map 
                         (len-rsm-sec (length (data
                                               (get-nth 0 (data rsm-sec))))))
                     (unless (= len-sm-sec len-rsm-sec)
                       (error "slippery-chicken::check-maps: In section ~
                               ~a the number of references in the maps ~
                               is not equal.~
                               ~% In set-map: ~a, in rthm-seq-map: ~a" 
                              (this sm-sec) len-sm-sec len-rsm-sec))))))))
              
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rsm-to-piece (rsm sc)
  ;; do this to reset the seq count
  (make-section-for-player nil sc nil)
  (loop for i in (data rsm) do
       (let* ((data (data i))
              (data-data (data data))
              (last-event nil)
              (last-pitch-seen nil)
              (section nil))
         ;; data has to be a ral, but the question is, is it a ral of
         ;; player-sections to be or a subsection?
         (if (is-ral (data (first data-data)))
             (progn
               (setf section (sc-change-class (rsm-to-piece data sc) 'section)
                     (data i) section
                     last-event 
                     (unless (zerop (num-bars section))
                       (get-last-event 
                        (get-last-bar section))))
               (when (pitch-or-chord last-event)
                 (setf last-pitch-seen (pitch-or-chord last-event))))
             (progn 
               ;; MDE Thu Dec  8 21:39:07 2011 -- print section ID
               (format t "~&******* section ~a" (full-ref data))
               (setf (data i) (sc-change-class data 'section)
                     (data (data i)) 
                     ;; 7/3/07: we do this convoluted double loop to ensure
                     ;; that we get-notes for the player in the order specified
                     ;; in the given hierarchy.
                     (loop 
                        with result = (ml nil (length data-data))
                        for pl in (instruments-hierarchy sc)
                        do
                        (loop 
                           for player in data-data 
                           for player-name = (first (last (this player)))
                           for player-section = 
                           (when (eq player-name pl)
                             (format t "~&Getting notes for ~a" player-name)
                             (make-section-for-player player sc last-event
                                                      last-pitch-seen))
                           ;; collect player-section
                           for i from 0
                           do
                           (when player-section
                             (setf (nth i result) player-section)
                             ;; todo: is last-event important here???
                             (unless (zerop (num-bars player-section))
                               ;; last-event is used for ties over to the
                               ;; beginning of bar 1 in a new seq
                               (setf last-event
                                     (get-last-event 
                                      (get-last-bar player-section)))
                               (when (pitch-or-chord last-event)
                                 ;; this one is used to avoid 8ves
                                 (setf last-pitch-seen 
                                       (pitch-or-chord last-event))))))
                        finally (return result)))))))
  rsm)
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Adds postscript code to specific pages in a file.
;;; e.g. (add-ps-to-file "file.eps" '((1 "blah") (4 "foo") (3 "bar"))) 
;;; where the strings will be added to pages 1, 4 and 3, just after the
;;; "%%Page: x y" line.  Of course, the strings should be legal postscript
;;; code, not just "blah blah".  The pairs are sorted into ascending page order
;;; first so that the file only has to be searched once when looking for the
;;; pages. 

(defun add-ps-to-file (file page-code-pairs)
  (let ((tmp-file (format nil "~aadd-ps-to-file.tmp" 
                          (directory-namestring file))))
    (with-open-file
        (out tmp-file :direction :output :if-does-not-exist :create
         :if-exists :error)
      (with-open-file
          (in file :direction :input :if-does-not-exist :error)
        (loop 
            with sorted = (sort (copy-list page-code-pairs)
                                #'(lambda (x y) (< (first x) (first y))))
            with page 
            with page-num
            with page-string
            with stop
            for line = (read-line in nil nil nil) 
            for line-num from 1
            while line do
              (unless page
                (when sorted
                  (setf page (pop sorted)
                        page-num (first page)
                        page-string (format nil "%%Page: ~a" page-num)
                        stop (length page-string))))
              (format out "~a~%" line)
              (when page
                (when (and (> (length line) stop)
                           (string= page-string line :end2 stop))
                  (format out "~&%% Code added by add-ps-to-file")
                  (format t "~&Adding postscript code to page ~a~%"
                          page-num)
                  (loop for ps in (rest page) do
                        (format out "~&~a~%" ps))
                  (setf page nil)))
            finally (when sorted
                      (error "slippery-chicken::add-ps-to-file: ~
                              Couldn't find page ~a"
                             page-num)))))
    (delete-file file)
    (rename-file tmp-file file)
    t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ps-text (x y text &key (font "Courier") (font-size 10))
  (format nil "~&/~a findfont ~a scalefont setfont ~%~a ~a newpath moveto ~
               ~%(~a) show ~%"
          font font-size x y text))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Grace notes have a fixed duration as defined by the rhythm class
;;; member grace-note-duration.  We subtract this amount from the
;;; non grace-note rhythm preceding the grace note(s). Of course, if
;;; the piece/section/sequence begins with grace notes, we have to add
;;; an offset to all the notes to make time for these.

(defun handle-grace-notes (voices)
  (let* ((max-beg-grace-notes (grace-notes-at-beg voices))
         ;; just get the standard grace-note duration
         (grace-note-dur (grace-note-duration (make-rhythm 1)))
         (offset (* grace-note-dur max-beg-grace-notes)))
    ;; the grace notes at the beginning have to be handled separately because
    ;; they are dependent on the other voices.
    (handle-opening-grace-notes voices max-beg-grace-notes grace-note-dur)
    (loop 
        for voice in voices
                     ;; start after the opening grace notes
        for index = (voice-grace-notes-at-beg (first voice)) ;; first rthm-seq
                    ;; each voice is a list of rthm-seqs which in turn is a
                    ;; list of events. we want to process a flat list of events
                    ;; per voice so need to flatten it first then rebuild the
                    ;; list afterwards. 
        for lengths = (get-sublist-lengths voice t)
        with next-grace-note ;; these are indices only, not the events
        with next-non-grace-note
        with num-grace-notes
        with num-events
        with decrement
        with previous
        do
          ;; we really don't need separate lists for rthm-seqs...
          (setf voice (flatten voice)
                num-events (length voice))
          ;; (print (last voice))
          (loop until (>= index num-events) do
                (setf next-grace-note (find-next-grace-note
                                       voice index nil nil)) ; no warning
                (unless next-grace-note
                  (return))
                ;; the first normal note after the grace note(s)
                (setf next-non-grace-note (find-next-non-grace-note
                                           voice next-grace-note nil))
                (unless next-non-grace-note
                  ;; (print (nth next-grace-note voice))
                  ;; (print (nth (1+ next-grace-note) voice))
                  ;; TODO: this doesn't seem to hold..could just be a section
                  (warn "slippery-chicken::handle-grace-notes: ~
                         Grace notes seem to end the section...! ~
                         (index = ~a, num-events = ~a, next-grace-note = ~a)"
                        index num-events next-grace-note)
                  (return))
                (setf num-grace-notes (- next-non-grace-note 
                                         next-grace-note)
                      previous (nth (1- next-grace-note) voice)
                      decrement (* grace-note-dur num-grace-notes))
                ;; decrease the duration of the note before the grace
                ;; notes. Perhaps this should only happen if the note
                ;; holds up to the next note? 
                (inc-duration previous (- decrement))
                (loop 
                    with start = 
                      (- (start-time (nth next-non-grace-note voice))
                         (* grace-note-dur num-grace-notes))
                    for i from next-grace-note 
                    for grace-note = (nth i voice)
                    repeat num-grace-notes 
                    do 
                      (setf (start-time grace-note) start
                            (end-time grace-note) (+ start grace-note-dur)
                            (duration-in-tempo grace-note) grace-note-dur
                            (compound-duration-in-tempo grace-note)
                            grace-note-dur)
                      (incf start grace-note-dur))
                (setf index next-non-grace-note))
          ;; put the flattened voice back into rthm-seqs
          (setf voice (split-into-sub-groups voice lengths)))
    ;; now we've done the grace notes, we have to offset every event to
    ;; make room for the opening grace-notes
    (unless (zerop max-beg-grace-notes)
      (loop for voice in voices do
            (loop for rs in voice do
                  (loop for event in rs do
                        (incf (duration-in-tempo event) offset)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This only works for grace notes that open a piece (i.e. not a
;;; section or sequence) as it will set start times beginning at 0.
;;;
;;; max-grace-notes is the number of grace notes at the beginning of
;;; the voice with the most (at the beginning).

(defun handle-opening-grace-notes (voices max-grace-notes grace-note-dur)
  (loop 
    ;; with total-offset = (* max-grace-notes grace-note-dur)
      for voice in voices 
                   ;; each voice is a list of rthm-seqs so just get the first
      for rthm-seq1 = (first voice)
      for opening-grace-notes = (voice-grace-notes-at-beg rthm-seq1)
      for wait = (- max-grace-notes opening-grace-notes)
      for offset = (* grace-note-dur wait)
      do
        (loop for event in rthm-seq1 repeat opening-grace-notes do
              (unless (is-grace-note event)
                (error "~a~%slippery-chicken::handle-opening-grace-notes: ~
                        Not a grace note!" event))
              (setf (start-time event) offset)
              (incf offset grace-note-dur))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Returns the max number of grace notes that are at the beginning of the
;;; voices.  voices is one list of rthm-seqs (a list of events) for each voice
;;; in the piece.

(defun grace-notes-at-beg (voices)
  (loop for voice in voices maximize
        (voice-grace-notes-at-beg (first voice))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Returns the number of grace notes at the beginning of a list of events.
(defun voice-grace-notes-at-beg (events)
  (loop 
      with grace-notes = 0
      for event in events do
        (if (is-grace-note event)
            (incf grace-notes)
          (return grace-notes))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun count-section-refs (sc)
  (let ((count 0))
    (loop 
        for sec in (data sc) 
        for data = (data sec)
        do
          (incf count
                (if (is-ral data)
                    (count-section-refs data)
                  (length data))))
    count))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun doctor-set-limits-env (env num-sections)
  (let ((stretched (new-lastx env num-sections)))
    ;; 14/8/07 first x always needs to be 1
    (setf (first stretched) 1)
    (loop for x in stretched by #'cddr and y in (cdr stretched) by #'cddr
        collect
          ;; convert notes or MIDI note numbers to degrees so that we can
          ;; interpolate.  Note degrees are in cm::*scale* so this is not the
          ;; same as MIDI notes. 
          x collect
          (if (numberp y)
              (midi-to-degree (floor y))
            (note-to-degree y)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-set-limit-aux (limits-al instrument seq-num)
  ;; (print seq-num)
  (let ((ins-curve (get-data instrument limits-al nil)))
    (when ins-curve
      (let ((degree (interpolate seq-num (data ins-curve))))
        (make-pitch (degree-to-note degree))))))
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun count-events-before-max-start (events max-start)
  ;; (format t "count-events-before-max-start: ~a~%~a" max-start events)
  (loop 
      with result = 0
      for event in events do
        (if (listp event)
            (incf result (count-events-before-max-start event max-start))
          (when (<= (start-time event) max-start)
            (incf result)))
      finally (return result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; events is a nested list as return by get-events-with-src.
;;; NB not called from anywhere (not needed after all).

(defun rm-events-after-max-start (events max-start-time)
  (loop for voice in events collect
        (loop for rs in voice collect
              (loop for event in rs 
                  unless (> (start-time event) max-start-time)
                  collect event))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; From the rthm-seq, pitch-seq, set and hint-pitch, get the current
;;; instrument and then notes for this seq.  With the notes and the rthm-seq,
;;; promote the latter to a sequence using the notes (this involves nothing
;;; more than changing the rhythm instances in the rthm-seq-bars of the
;;; rthm-seq to event instances, adding the pitch or chord information).

(defun sc-make-sequenz (rthm-seq instrument set pitch-seq hint-pitch 
                        player player-section-ref seq-num
                        ;; this is actually an event, not a pitch or chord, and
                        ;; could be nil
                        last-note-previous-seq
                        slippery-chicken global-seq-num instrument-change
                        ;; this should always be the last pitch the instrument
                        ;; played, no matter how long ago
                        last-pitch)
  ;; (print "entering sc-make-sequenz")
  (object-is-nil? rthm-seq "slippery-chicken::sc-make-sequenz" 'rthm-seq)
  ;; (object-is-nil? pitch-seq "slippery-chicken::sc-make-sequenz" 'pitch-seq)
  ;; (print instrument-change)
  (let* ((sequenz (clone-with-new-class rthm-seq 'sequenz))
         (player-obj (when player
                       (get-player slippery-chicken player)))
         (midi-channel (if player
                           (midi-channel player-obj)
                           1))
         (microtones-midi-channel (when player
                                    (microtones-midi-channel player-obj)))
         (midi-prog (when instrument
                      (midi-program instrument)))
         (get-set-limits (and slippery-chicken player global-seq-num))
         ;; if any extra range limiting has been given for this instrument
         ;; get it here and pass it to get-notes.
         (set-limits (when get-set-limits
                       (get-set-limits slippery-chicken player 
                                       global-seq-num)))
         ;; these would have to be chords or notes once we get the
         ;; pitch-choosing algorithm going, also the midi-channel setting loop
         ;; below. 
         (notes-from-pitch-seq
          (when pitch-seq
            (get-notes pitch-seq instrument set hint-pitch (second set-limits) 
                       (first set-limits) global-seq-num 
                       last-pitch)))
         (notes (my-copy-list notes-from-pitch-seq))
         (iwbns (when slippery-chicken 
                  (member player 
                          (instruments-write-bar-nums slippery-chicken))))
         (do-prog-changes instrument-change)
         (current-note nil)
         ;; (last-note nil)
         ;; transposition specifies how many semitones the instrument sound
         ;; above/below written pitch so we have to invert the sign of this
         ;; to get the transposition we need to write sounding pitches.
         (transpose (when instrument
                      (- (transposition-semitones instrument)))))
    (when (and transpose (zerop transpose))
      (setf transpose nil))
    ;; 31/3/10: copy over the pitch curve for info
    (when pitch-seq
      (setf (pitch-curve sequenz) (original-data pitch-seq)))
    (loop for n in notes do 
       ;; 5.2.11 this used to ignore microtonality but now sets channel
       ;; correctly 
         (if (or (pitch-p n) (chord-p n))
             (set-midi-channel n midi-channel microtones-midi-channel)
             (error "~a~%slippery-chicken::sc-make-sequenz: ~
                     Can't set MIDI channel for this object!" n)))
    ;; If there are no notes then it must be that this seq contains
    ;; only one tied (to) note so none are needed.  In that case we
    ;; have to have the last note from the previous sequence.
    (setf current-note (if notes 
                           (clone (first notes))
                           (progn
                             (unless (zerop (num-score-notes rthm-seq))
                               (unless 
                                   ;; eg when called from (cmn-display
                                   ;; rthm-seq-pallette  
                                   (and last-note-previous-seq
                                        (pitch-or-chord 
                                         last-note-previous-seq))
                                 (warn ;;"rthm-seq:~%~a  ~%last-previous-seq ~a
                                  "~%slippery-chicken::sc-make-sequenz: ~
                                       last-note-previous-seq error: should ~
                                       be a note to tie from. (rthm-seq id ~
                                        = ~a, player ~a)"
                                  ;; rthm-seq last-note-previous-seq
                                  (id rthm-seq) player)
                                 ;; TODO: fix this:
                                 ;; this is a complete fraud of course
                                 ;; but I've got to get on with composition
                                 ;; and can't find this bug...
                                 (warn "Cludging to 'b4!")
                                 (setf last-note-previous-seq
                                       (make-event 'b4 'q)))
                               (clone (pitch-or-chord 
                                       last-note-previous-seq))))))
    #|
    ;; this checks that there are no ties to the first note in a seq ; ; ; ; ; ;
  (when (is-tied-to (get-nth-event 0 (get-bar sequenz 0 t)))
    (error "slippery-chicken::sc-make-sequenz: ~
              Tied first note of sequenz not allowed!"))
  |#
    (loop for bar in (bars sequenz) and bar-num from 1 do
          ;; first of all set all the bars to write--then change in 
          ;; sequenz::update-slots depending upon real bar num
          (when iwbns
            (setf (write-bar-num bar) t))
          (setf (player-section-ref bar) player-section-ref
                (nth-seq bar) seq-num
                (nth-bar bar) (1- bar-num))
          ;; Here the rhythms in the rthm-seq-bar are upgraded to events
          (loop for rhythm in (rhythms bar) and rthm-num from 0 do
                (let ((event (clone-with-new-class rhythm 'event)))
                  ;; (print event)
                  ;; 8/3/07: need to change midi programmes if an instrument
                  ;; change was detected; these are stored in the event.
                  (when do-prog-changes
                    ;; (format t "~&ins change! ~a ~a" player seq-num)
                    ;; add the instrument change unless this is the first seq
                    ;; (because midi-play handles all the starting program
                    ;; changes) 
                    (unless (= 1 global-seq-num)
                      (push (list midi-channel midi-prog)
                            (midi-program-changes event))
                      (when (microtonal-chords-p player-obj)
                        (push (list microtones-midi-channel midi-prog)
                              (midi-program-changes event)))
                      ;; 8/5/07: also make sure new-staff-name is added to the
                      ;; note (this makes use of new cmn code by me and
                      ;; hopefully added to main repository by Bill).
                      ;; (instrument change is registered here)
                      #|
                      ;; no longer add these as marks, rather store the ;
                      ;; instrument change in the dedicated event slot ;
  (add-cmn-mark event (cmn::new-staff-name 
                      (staff-name instrument)))
  (add-cmn-mark 
                      event (cmn::sc-cmn-text (staff-name instrument))))
                      |#
                      (setf (instrument-change event)
                            (if (staff-short-name instrument)
                                (list (staff-name instrument)
                                      (staff-short-name instrument))
                                (list (staff-name instrument)))))
                    (setf do-prog-changes nil))
                  (unless (is-rest event)
                    (setf (pitch-or-chord event) 
                      (cond ((needs-new-note event)
                             (setf ;; last-note current-note
                                 current-note (pop notes))
                             (unless current-note
                               (error "~a~a~%slippery-chicken::~
                                       sc-make-sequenz: no current-note (1)!"
                                      rthm-seq pitch-seq))
                             (clone current-note))
                            ((is-tied-to event) 
                             (unless current-note
                               (print notes-from-pitch-seq)
                               (print event)
                               (error "~a~a~%slippery-chicken::~
                                       sc-make-sequenz: no current-note (1)!"
                                      rthm-seq pitch-seq))
                             #|
                             (format t "~&seq-num ~a, ~a tying ~a to ~a"
                              global-seq-num player (id last-note)
                              (id current-note))
                             |#
                             (clone current-note)))))
                  (when transpose
                    (set-written event transpose))
                  ;; (when (is-single-pitch event)
                  ;;       (print (midi-channel (pitch-or-chord event))))
                  (setf (nth rthm-num (rhythms bar)) event))))
    ;; all the notes should have been popped off by now
    (when notes
      (error "~a ~a ~%slippery-chicken::sc-make-sequenz: Didn't use all ~
              the notes!  Still have ~a left."
             rthm-seq pitch-seq (length notes)))
    ;; the cmn-marks given in the rthm-seq were not interpreted in that class,
    ;; first here when all the rhythms have been converted to events.
    ;; 19/2/07: move this method over to the rthm-seq class and call it there
    ;; (add-cmn-marks sequenz)
    ;; (print "exiting sc-make-sequenz")
    sequenz))
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; If we get three notes whose best clef without ledger lines is not the
;;; current, change it.  If any note has only a best clef that is different to
;;; current, change it
;;; todo: don't put a clef between beam ends: break the beam first.

(defun auto-clefs-handle-last-3 (last-events last-clefs note-count
                                 current-clef 
                                 &optional verbose in-c)
  ;; weird compilation bug...
  (when (event-p (first last-clefs))
    (error "~a~%slippery-chicken::auto-clefs-handle-last-3: clefs, not events!"
           last-clefs))
  (when verbose
    (format t "~&current-clef: ~a" current-clef))
  (let ((change nil)
        (change-count 0)
        (new-current-clef current-clef)
        (oldest (if (= note-count 2)
                    0
                  (1+ note-count))))
    (loop 
        with i = oldest
        with clefs with c1 ;; with event ;; with c2 
        repeat 3 do
          (setf clefs (nth i last-clefs)
                ;; remember: c is a list of two clefs
                c1 (first clefs)
                ;; c2 (second clefs)
                ;; event (nth i last-events)
                )
          (when (and c1
                     (not (equal new-current-clef c1))
                     ;; (equal new-current-clef c2)
                     ;; we need all three to do the change!
                     (or (not change)
                         (equal c1 change)))
            (incf change-count)
            (setf change c1))
          (if (= 2 i) 
              (setf i 0)
            (incf i)))
    (if (= 3 change-count)
        (let* ((e (nth oldest last-events))
               (psym (get-pitch-symbol e (not in-c))))
          (add-clef e change)
          (when verbose
            (format t "~&backup...change clef!!!: ~a before ~a"
                    change psym))
          (setf new-current-clef change))
      (loop 
          with i = oldest
          with clefs with c1 with c2 with event with psym
          repeat 3 do
            (setf clefs (nth i last-clefs)
                  ;; remember: c is a list of two clefs
                  c1 (first clefs)
                  c2 (second clefs)
                  event (nth i last-events)
                  psym (when event
                         (get-pitch-symbol event)))
            (when (and c1 psym
                       (or (and (not (equal new-current-clef c1))
                                (not (equal new-current-clef c2))
                                ;; could be we just set a clef and now c1,c2
                                ;; are no longer current so make sure we're not
                                ;; in the current clef's range before changing
                                ;; it 
                                (not (best-clef-aux 
                                      nil
                                      (if (written-pitch-or-chord event)
                                          (written-pitch-or-chord event)
                                        (pitch-or-chord event))
                                      nil current-clef verbose)))
                           (and (not (equal new-current-clef c1))
                                (not c2))))
              ;; we really need a new clef now!
              (when verbose
                (format t "~&change clef!!!: ~a before ~a" c1 psym))
              (add-clef event c1)
              ;; (cmn::cmn-get-clef c1))
              ;; this should stop us from adding multiple clefs before the
              ;; same note 
              (setf (first (nth i last-clefs)) nil
                    new-current-clef c1))))
    (when verbose
      (format t "~&change-count: ~a, last-clefs: ~a" change-count last-clefs))
    new-current-clef))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 24.1.11: convert a tempo-curve to a tempo map, unless tempo-map is not nil,
;;; whereupon we simply return that.  remember that the tempo curve's first
;;; element is the frequency in bars, second element is the beat rhythm, and
;;; the third is the curve itself.
;;; NB the curve should start at 0 but the map will start at bar 1
(defun tempo-curve-to-map (tempo-map tempo-curve num-bars)
  (when (and tempo-map tempo-curve)
    (error "slippery-chicken::tempo-curve-to-map: ~
            can't have a tempo-map and a tempo-curve; ~%only one or the other"))
  (if tempo-map
      tempo-map
      (let ((new-curve (new-lastx (third tempo-curve) num-bars))
            (beat (second tempo-curve)))
        (unless (zerop (first new-curve))
          (error "slippery-chicken::tempo-curve-to-map: ~
                  curve should start at 0: ~%~a" (third tempo-curve)))
        (loop for x from (first new-curve) to (lastx new-curve) 
           by (first tempo-curve) collect
             ;; round to the nearest bar
             (list (round x) (list beat (interpolate x new-curve)))))))
             
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; EOF slippery-chicken.lsp

