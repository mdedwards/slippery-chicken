;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* sc/slippery-chicken
;;; NAME 
;;; slippery-chicken
;;; 
;;; File:             slippery-chicken.lsp
;;;
;;; Class Hierarchy:  named-object -> slippery-chicken
;;;
;;; Version:          1.1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Implementation of the slippery-chicken class.
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    March 19th 2001
;;;
;;; $$ Last modified:  16:21:42 Tue Jun 10 2025 CEST
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
;;;                   PARTICULAR PURPOSE. See the GNU General Public License
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

(defclass slippery-chicken (named-object)
  ((rthm-seq-palette :accessor rthm-seq-palette :initarg :rthm-seq-palette
                     :initform nil)
   (rthm-seq-map :accessor rthm-seq-map :initarg :rthm-seq-map :initform nil)
   ;; 24.3.11: method shorten-large-fast-leaps uses a max duration for a fast
   ;; note in seconds as a key arg, but it's hard to access as a user, so
   ;; specify it as part of the sc class.
   (fast-leap-threshold :accessor fast-leap-threshold 
                        :initarg :fast-leap-threshold :initform 0.125)
   ;; These are replacements for the map (which may be generated
   ;; algorithmically); they take the form (section-reference
   ;; sequence-number (1-based) replacement) e.g.
   ;;  '(((1 1 vla) 2 20a) ((1 1 vla) 3 1a) ((1 1 vla) 4 9a) ((1 1 vla) ...
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
   ;; '((1 (q 160)) ((2 2 2) 96)) (200 (q 120 "meno mosso")))
   ;; where 1 is the bar, q is the beat and 160 is the bpm. The (2 2 2) is a
   ;; reference to a bar of the form (section-number sequence-number
   ;; bar-number). References are converted to bar numbers before being stored
   ;; in the map.
   (tempo-map :accessor tempo-map :initarg :tempo-map :initform nil)
   ;; 24.1.11 we can also specify a tempo curve and this will generate the
   ;; tempo-map for us. The x scale will be fitted to the number of bars and a
   ;; new tempo will be written every X bars, as indicated in the first
   ;; argument in the list (10 in the following example); the second element is
   ;; the beat rhythm e.g. '(10 q (0 60 100 120)) NB the curve should start at
   ;; 0 but the map will start at 1
   (tempo-curve :accessor tempo-curve :type list :initarg :tempo-curve 
                :initform nil)
   ;; this contains the instrument definitions referenced in the ensemble.
   (instrument-palette :accessor instrument-palette
                       :initarg :instrument-palette :initform nil)
   ;; in CMN: which instruments should write bar numbers in the score?  
   ;; MDE Wed Apr 18 09:46:40 2012 -- if NIL we'll use the instruments at the
   ;; top of each group 
   (instruments-write-bar-nums :accessor instruments-write-bar-nums
                               :type list :initarg :instruments-write-bar-nums
                               :initform nil)
   ;; when choosing notes for instruments we take into account notes that other
   ;; instruments are already playing--this limits the available notes. So the
   ;; order in which the instruments are allocated notes is important: specify
   ;; the order you want in this slot; the default is simply the list of
   ;; players in (players (ensemble sc))
   (instruments-hierarchy :accessor instruments-hierarchy :type list
                          :initarg :instruments-hierarchy :initform nil)
   ;; in the case of the sndfile-palette, we put the palette first in a list,
   ;; the paths second and the extensions third
   (snd-output-dir :accessor snd-output-dir
                   :initarg :snd-output-dir :initform
                   (get-sc-config 'default-dir))
   ;; see clm-play method for a description of this slot.
   (sndfile-palette :accessor sndfile-palette :initarg :sndfile-palette
                    :initform nil)
   (bars-per-system-map :accessor bars-per-system-map 
                        :initarg :bars-per-system-map :initform nil)
   ;; where the braces and bar lines should be drawn, starting from the top of
   ;; the score. i.e. (3 2 7) means the first three instruments are grouped,
   ;; then the next 2, then the next 7. This is checked at initialization to
   ;; add up to the number of players in the ensemble.
   ;; *** N.B. This overrides the bar-line-writers slot of the ensemble!
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
   ;; the whole piece. E.g. :set-limits-high '((vibes (0 g4 100 f6))) will
   ;; limit the notes selected from the sets to those below g4 at the beginning
   ;; but up to f6 by the end. Inbetween there is interpolation. The x-axis
   ;; can be any arbitrary range. Y values can be note names or midi note
   ;; numbers. These always refer to sounding pitches.
   ;; NB if a curve is given with the id 'all', then this will act as a global
   ;; curve and be applied to all instruments, overriding their individual
   ;; curves. Also, though the X axes of these curves can have arbitrary
   ;; ranges, they will be scaled to conform to the number of sequences in the
   ;; piee and interpolated once per sequence; so if you need precision it's
   ;; actually better to use sequence numbers as X values, not bars.
   ;; MDE Mon Apr 25 16:37:09 2016 -- update: if you call the
   ;; rm-repeated-pitches method these will be stretched to fit the number of
   ;; bars 
   (set-limits-high :accessor set-limits-high :initarg :set-limits-high 
                    :initform nil)  
   (set-limits-low :accessor set-limits-low :initarg :set-limits-low 
                   :initform nil)
   ;; MDE Thu Sep  6 12:34:45 2018 -- set transposition curve
   (transposition-curve :accessor transposition-curve :type list
                        :initarg :transposition-curve :initform '(0 0 100 0))
   ;; 31.1.11: this title will be used in lilypond file names. Any spaces will
   ;; be turned into hyphens when creating file names (but not the title in the
   ;; score of course).
   (title :accessor title :initarg :title :initform "slippery chicken")
   ;; MDE Sun Jun 15 11:37:26 2014 
   (subtitle :accessor subtitle :initarg :subtitle :initform nil)
   ;; MDE Fri Dec  9 19:43:58 2011 -- for lilypond
   (composer :accessor composer :initarg :composer :initform nil)
   ;; MDE Tue May  3 08:20:17 2016 -- concatenate the composer and year (if
   ;; given) in Lilypond output. Nil means nothing will be written otherwise
   ;; add a string of your choice e.g. "2016" or "2016-17"
   (year :accessor year :initarg :year :initform nil)
   ;; MDE Fri Mar 3 10:14:47 2017 -- A string to add dedications to Lilypond
   ;; scores (only) such as "for Mieko Kanno". This will appear centered,
   ;; directly above the title, in a smaller font.  Default = NIL (no
   ;; dedication).
   (dedication :accessor dedication :initarg :dedication :initform nil)
   ;; 10/3/07: simply a list of bar numbers where a rehearsal letter should be
   ;; written (automatically)
   (rehearsal-letters :accessor rehearsal-letters :type list 
                      :initarg :rehearsal-letters :initform nil)
   ;; 1/4/06: this is the number of sequences in all of the sections and
   ;; subsections combined.
   (num-sequences :accessor num-sequences :type integer :initform -1)
   ;; MDE Tue Apr 10 08:27:24 2012 -- the get-notes function would avoid
   ;; melodic octaves by default but make this a slot option now 
   (avoid-melodic-octaves :accessor avoid-melodic-octaves :type boolean
                          :initarg :avoid-melodic-octaves :initform t)
   ;; MDE Sat Jul 14 18:33:47 2012 -- the get-notes function removes notes
   ;; played by prior instruments when choosing notes for each rthm-seq; this
   ;; can be turned off here 
   (avoid-used-notes :accessor avoid-used-notes :type boolean :initarg
                     :avoid-used-notes :initform t)
   ;; MDE Mon May  7 21:29:48 2012 -- if we've called multi-bar-rests method
   ;; but then call cmn-display with :multi-bar-rests nil we'll get an error
   ;; (too few bars returned by get-cmn-data), so we have to remember whether
   ;; we called it or not
   (multi-bar-rests-called :accessor multi-bar-rests-called :initform nil)
   ;; MDE Sat Sep  7 17:20:14 2013 -- process the data and make the piece
   ;; etc. or wait until later and sc-init is called explicitly
   (defer :accessor defer :type boolean :initarg :defer :initform nil)
   ;; MDE Mon Jul  2 16:16:19 2012 -- NB Working in Lilypond but not yet
   ;; implemented in CMN  
   (key-sig :accessor key-sig :type list :initarg :key-sig :initform '(c major))
   ;; MDE Mon Feb 19 18:53:42 2018 -- did we run sc-init yet?
   (did-init :accessor did-init :type boolean :initform nil)
   ;; MDE Mon Mar 26 13:10:15 2012 -- This one defines the lowest scaler we'll
   ;; accept before adding notes from those used i.e. if our pitch-seq needs 6
   ;; notes and only 3 are available, there would be note repetition but as
   ;; this would create a scaler of 0.5, that would be acceptable
   (pitch-seq-index-scaler-min :accessor pitch-seq-index-scaler-min
                               :initarg :pitch-seq-index-scaler-min 
                               :initform 0.5)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken/sc-init
;;; DESCRIPTION
;;; Explicitly initialize the slippery-chicken object. This is usually called
;;; implicitly by initialize-instance (i.e. when you call
;;; make-slippery-chicken) but there could be circumstances (e.g. in subclasses
;;; of slippery-chicken) where you'd like to defer initialization and call this
;;; method explicitly instead. In that case set :defer to t when making the
;;; slippery-chicken object. This can also be called with using :defer, to
;;; regenerate the piece after e.g. calling set-limits-by-section. 
;;; 
;;; ARGUMENTS
;;; - the slippery-chicken object
;;;
;;; OPTIONAL ARUGMENTS
;;; keyword arguments:
;;; - :regenerate-pitch-seq-map: the pitch-seq-map is generated here for each
;;; instrument using the pitch-seqs in the rthm-seq-palette. By setting this
;;; to T we can force regeneration (e.g. if the rthm-seq-palette has changed
;;; and we want to re-init the sc with different data). Default = T.
;;; - :shorten-large-fast-leaps. T or NIL to call the associated method or
;;;   not. Eg. you might want to call this explicitly elsewhere because the
;;;   tempo-map is set after sc-init is called. Default = T.
;;; RETURN VALUE
;;; the now fully initialized slippery-chicken object
;;; 
;;; EXAMPLE
#|

(let ((sc (make-slippery-chicken
           '+mini+
           :ensemble '(((vn (violin :midi-channel 1))))
           :set-palette '((1 ((gs4 af4 bf4))))
           :defer t
           :set-map '((1 (1 1 1)))
           :rthm-seq-palette '((1 ((((4 4) e e e e e e e e))
                                   :pitch-seq-palette ((1 2 1 1 1 1 1 1)))))
           :rthm-seq-map '((1 ((vn (1 1 1))))))))
  (sc-init sc))

|#
;;; SYNOPSIS
(defmethod sc-init ((sc slippery-chicken) &key (regenerate-pitch-seq-map t)
                                          (shorten-large-fast-leaps t))
;;; ****
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
            (let ((sp (set-palette sc)))
              (if (set-palette-p sp)
                  (clone sp)
                  ;; MDE Fri Apr 6 22:28:52 2012 -- a two-note set would
                  ;; result in a recursive ral being made (because
                  ;; recurse-simple-data is T by default) so we can't just
                  ;; call make-set-palette, rather, apply instead, so that
                  ;; :recurse-simple-data NIL can be part of the list that is
                  ;; applied to make-set-palette. But this is tricky because
                  ;; if we want to pass keyword args to make-set-palette when
                  ;; creating a set-palette directly in
                  ;; make-slippery-chicken, then we need an extra level of
                  ;; list e.g.
                  ;; 
                  ;; '(((1 ((g2 d3 a3 b3)))
                  ;;    (2 ((d3 b3)))) 
                  ;;   :recurse-simple-data nil))
                  ;; 
                  ;; as opposed to what we've always done and want to
                  ;; continue doing in the vast majority of cases e.g.
                  ;; 
                  ;;'((1 ((g2 d3 a3 b3)))
                  ;;  (2 ((d3 b3))))
                  ;;
                  ;; so in order to make this backward compatible, see if the
                  ;; second element of the list is a symbol
                  ;; (i.e. keyword--and remembering of course that (symbolp
                  ;; NIL) -> T!) and if it is use apply, otherwise just call
                  ;; directly:
                  (if (and (second sp) (symbolp (second sp)))
                      (apply #'make-set-palette
                             (cons (make-name 'set-palette)
                                   sp))
                      (make-set-palette (make-name 'set-palette)
                                        sp)))))
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
        (error "~a~%slippery-chicken::sc-init:~%~
                    Cannot proceed: set map is either nil or not a set map!"
               (set-map sc)))
      ;; 29/3/10: it's not ok to have nil references in the set-palette
      ;; MDE Thu Mar  1 20:24:39 2012 -- method changed name from (link)
      (bind-palette (set-map sc) (set-palette sc) nil)
      (link-named-objects (set-palette sc))
      (link-named-objects (set-map sc))
      (check-first-bar-ins-for-doubling-players (ensemble sc)
                                                (instrument-change-map sc)
                                                (this (get-first 
                                                       (set-map sc))))
      ;; MDE Tue Jul  6 10:34:19 2021, Heidhausen -- moved the trailing-slash
      ;; call into a setf method  
      (setf (snd-output-dir sc) (snd-output-dir sc) ; to trigger setf method
            (rthm-seq-palette sc)
            (if (rsp-p (rthm-seq-palette sc))
                (clone (rthm-seq-palette sc))
                ;; MDE Wed Feb 10 15:47:35 2016 -- NIL rsp's were not
                ;; triggering an error until further down the line (which was
                ;; confusing).
                (if (rthm-seq-palette sc)
                    (make-rsp (make-name 'rthm-seq-palette)
                              (rthm-seq-palette sc))
                    (error "~%slippery-chicken::sc-init:rthm-seq-palette ~
                             is NIL. Can't proceed.")))
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
      ;; MDE Thu Mar  1 20:24:39 2012 -- method changed name from (link)
      (bind-palette (rthm-seq-map sc) (rthm-seq-palette sc))
      (check-instruments sc)
      (check-maps (set-map sc)
                  (rthm-seq-map sc))
      (setf (pitch-seq-map sc)
            ;; MDE Mon Oct 28 20:09:11 2013
            (if (and (not regenerate-pitch-seq-map) 
                     (sc-map-p (pitch-seq-map sc)))
                (clone (pitch-seq-map sc))
                (generate-pitch-sequence-map (rthm-seq-map sc) sc))
            (hint-pitches sc)
            (if (change-map-p (hint-pitches sc))
                (clone (hint-pitches sc))
                (make-change-map (make-name 'hint-pitches) 
                                 t
                                 (hint-pitches sc))))
      ;; MDE Mon Oct 28 20:01:10 2013 
      (reset-pitch-seq-map sc)
      ;; MDE Mon Sep 24 22:16:25 2012 --  calls the setf method
      (setf (sndfile-palette sc) (sndfile-palette sc)
            (num-sequences sc) (count-sequence-refs (set-map sc)))
      (handle-set-limits sc)
      (handle-transposition-curve sc)
      ;; (print (set-limits-low sc))
      ;; we have a chicken before the egg situation here: we can't
      ;; create a tempo-map without a piece because we need
      ;; reference to the bar numbers, but we can't create a piece
      ;; without a tempo-map because we need the tempi to calculate
      ;; start times. As a solution, create a piece with a
      ;; temporary tempo-map of qtr=60 then update it later.
      (setf (tempo-map sc) '((1 60))
            (piece sc) (sc-make-piece sc (warn-ties sc))
            ;; map might be nil as we have a curve instead so handle this
            (tempo-map sc) (tempo-curve-to-map given-tempo-map
                                               (tempo-curve sc)
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
                              "slippery-chicken::sc-init:~
                              staff-groupings should be a list of ~
                              integers: ~a"
                              sg))
                           sum i)))
            (error "slippery-chicken::sc-init: ~%~
                  staff-groupings should be a list of integers summing ~
                  to the number ~%of instruments in the ensemble:  ~a"
                   sg))
          ;; 10.11.11: if no staff-groupings given, just make the whole
          ;; ensemble one big group
          (setf (staff-groupings sc) (list (num-players (ensemble sc))))))
    ;; the order of players in the piece (from rthm-seq-map) is alphabetical,
    ;; but we want them as given in the ensemble...
    (setf (players (piece sc)) (players (ensemble sc)))
    ;; make a double bar at end of piece
    (change-bar-line-type sc (num-bars (piece sc)) 2)
    ;; have to call this again now that we've got the real tempo-map
    (update-slots sc (tempo-map sc) 0.0 0.0 1 nil nil (warn-ties sc))
    ;; MDE Sat Sep 26 16:48:21 2020, Heidhausen -- now also generate other stats
    ;; such as lowest/highest pitch played (2nd arg was default nil previously) 
    (update-instruments-total-duration sc t)
    ;; (print (get-data 1 (set-palette sc)))
    ;; 25.3.11 the make-slippery-chicken function might set this to nil thus
    ;; overriding the class default
    ;; MDE Tue Aug  4 14:14:20 2020, Heidhausen -- actually, let's allow this to
    ;; be nil so we can avoid calling shorten-large-fast-leaps
    ;; (unless (fast-leap-threshold sc)
    ;; (setf (fast-leap-threshold sc) 0.125))
    (when (and shorten-large-fast-leaps (fast-leap-threshold sc))
      (format t "~&Shortening short, fast leaps...")
      (format t "~&Shortened ~a large fast leaps"
              (shorten-large-fast-leaps sc :verbose nil)))
    ;; make sure tempo changes get registered in midi output
    (update-events-tempo sc)
    ;; 28.1.11
    (check-time-sigs sc)
    ;; 5.4.11
    (cleanup-rest-bars sc)
    ;; MDE Mon May  7 16:25:52 2012 
    (check-tuplets sc)
    ;; MDE Fri Jun 15 08:33:30 2012
    (check-beams sc)
    ;; MDE Fri Nov 13 17:42:22 2015  
    (initial-staff-lines sc) 
    (set-rehearsal-letters sc (get-groups-top-ins sc))
    ;; MDE Mon Feb 19 18:57:28 2018
    (setf (did-init sc) t))
  sc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Fri May 30 18:59:28 2014 
(defmethod initialized ((sc slippery-chicken))
  (bar-holder-p (piece sc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Mon Oct 28 20:00:09 2013 
(defmethod reset-pitch-seq-map ((sc slippery-chicken))
  ;; once we get down to the data, they're named objects, the data of which is
  ;; a list of pitch-seqs
  (rmap (pitch-seq-map sc) #'(lambda (no) (loop for pitch-seq in (data no) do
                                               (when pitch-seq
                                                 (reset pitch-seq))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod initialize-instance :after ((sc slippery-chicken) &rest initargs)
  (declare (ignore initargs)
           (special +slippery-chicken-standard-instrument-palette+))
  ;; MDE Thu Aug 13 11:36:48 2015 -- do this here rather than in
  ;; make-slippery-chicken, so that sc subclasses get the global too:
  ;; we make the given name a global, which is a bit old-fashioned but handy.
  (when (id sc)
    (set (id sc) sc))
  ;; MDE Sat Mar 31 09:27:31 2012 
  (unless (pitch-seq-index-scaler-min sc)
    (setf (pitch-seq-index-scaler-min sc) 0.5))
  ;; MDE Thu Jan 12 11:15:13 2012 -- in order to clone we need to be able to
  ;; init the object without slot values then setf them afterwards 
  (if (and (set-map sc) (ensemble sc) (rthm-seq-map sc) (rthm-seq-palette sc)
           (not (defer sc)) (set-palette sc))
      (sc-init sc)
      (when (get-sc-config 'warn-no-sc-init)
        (warn "slippery-chicken::initialize-instance: sc-init method is being ~
               ~%skipped as one of the following slots's data is missing: ~
               ~%set-map, ensemble, rthm-seq-map, rthm-seq-palette, ~
               set-palette. ~
               ~%Assuming you will call sc-init explicitly later."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object :before ((sc slippery-chicken) stream)
  ;; MDE Sat Sep  7 17:12:16 2013 -- only print things if we're fully init'd
  (flet ((get-id (thing)
           (if (named-object-p thing)
               (id thing)
               "(not yet initialized)")))
    (format stream "~%SLIPPERY-CHICKEN:~
                  ~%                      title: ~a ~
                  ~%                   composer: ~a ~
                  ~%                       year: ~a ~
                  ~%                set-palette: ~a ~
                  ~%                    set-map: ~a ~
                  ~%               hint-pitches: ~a ~
                  ~%               rthm-seq-map: ~a ~
                  ~%           rthm-seq-palette: ~a ~
                  ~%                  tempo-map: ~a ~
                  ~%                tempo-curve: ~a ~
                  ~%         instrument-palette: ~a ~
                  ~%                   ensemble: ~a ~
                  ~%      instruments-hierarchy: ~a ~
                  ~%        fast-leap-threshold: ~a ~
                  ~%      avoid-melodic-octaves: ~a ~
                  ~%           avoid-used-notes: ~a ~
                  ~%                   did-init: ~a ~
                  ~%     multi-bar-rests-called: ~a ~
                  ~% pitch-seq-index-scaler-min: ~a"
            (title sc) (composer sc) (year sc) (get-id (set-palette sc))
            (get-id (set-map sc))
            (get-id (hint-pitches sc)) (get-id (rthm-seq-map sc))
            (get-id (rthm-seq-palette sc)) (get-id (tempo-map sc))
            (tempo-curve sc)
            (get-id (instrument-palette sc)) (get-id (ensemble sc))
            (instruments-hierarchy sc) (fast-leap-threshold sc) 
            (avoid-melodic-octaves sc) (avoid-used-notes sc) 
            (did-init sc) (multi-bar-rests-called sc) 
            (pitch-seq-index-scaler-min sc))
    (when (piece-p (piece sc))
      (statistics sc stream))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Wed May  9 12:08:39 BST 2012: Conformed robodoc entry

;;; ****m* slippery-chicken/clone
;;; DESCRIPTION
;;; Copy (clone) the specified instance and all data associated with the
;;; slippery-chicken object.
;;; 
;;; ARGUMENTS 
;;; - A slippery-chicken object.
;;; 
;;; RETURN VALUE  
;;; A slippery-chicken object.
;;; 
;;; EXAMPLE
#|
(let ((mini
       (make-slippery-chicken
        '+mini+
        :instrument-palette +slippery-chicken-standard-instrument-palette+
        :ensemble '(((fl (flute :midi-channel 1))))
        :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
        :set-map '((1 (1)))
        :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -)))))
        :rthm-seq-map '((1 ((fl (1))))))))
  (clone mini))

|#
;;; SYNOPSIS
(defmethod clone ((sc slippery-chicken))
;;; ****
  (clone-with-new-class sc 'slippery-chicken))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
          (slot-value no 'instrument-palette) (clone (instrument-palette sc))
          (slot-value no 'ensemble) (clone (ensemble sc))
          (slot-value no 'instrument-change-map) 
          (clone (instrument-change-map sc))
          (slot-value no 'tempo-map) (clone (tempo-map sc))
          (slot-value no 'tempo-curve) (my-copy-list (tempo-curve sc))
          (slot-value no 'instruments-write-bar-nums) 
          (copy-list (instruments-write-bar-nums sc))
          ;; MDE Mon Feb 19 10:42:46 2024, Heidhausen 
          (slot-value no 'instruments-hierarchy) 
          (copy-list (instruments-hierarchy sc))
          (slot-value no 'snd-output-dir) (snd-output-dir sc)
          (slot-value no 'sndfile-palette) (clone (sndfile-palette sc))
          (slot-value no 'bars-per-system-map) (clone (bars-per-system-map sc))
          (slot-value no 'staff-groupings) (copy-list (staff-groupings sc))
          (slot-value no 'piece) (clone (piece sc))
          (slot-value no 'title) (title sc)
          (slot-value no 'subtitle) (subtitle sc)
          (slot-value no 'composer) (composer sc)
          (slot-value no 'year) (year sc)
          (slot-value no 'warn-ties) (warn-ties sc)
          (slot-value no 'set-limits-high) (my-copy-list (set-limits-high sc))
          (slot-value no 'set-limits-low) (my-copy-list (set-limits-low sc))
          (slot-value no 'transposition-curve)
          (my-copy-list (transposition-curve sc))
          (slot-value no 'rehearsal-letters) 
          (my-copy-list (rehearsal-letters sc))
          (slot-value no 'avoid-melodic-octaves) (avoid-melodic-octaves sc) 
          (slot-value no 'avoid-used-notes) (avoid-used-notes sc)
          (slot-value no 'did-init) (did-init sc) 
          (slot-value no 'pitch-seq-index-scaler-min)
          (pitch-seq-index-scaler-min sc)
          (slot-value no 'multi-bar-rests-called) (multi-bar-rests-called sc)
          (slot-value no 'num-sequences) (num-sequences sc))
    no))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken/num-notes
;;; DESCRIPTION
;;; Returns the number of attacked notes in a given slippery-chicken object;
;;; i.e., not including ties or rests.
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; 
;;; RETURN VALUE
;;; An integer,
;;; 
;;; EXAMPLE
#|
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((fl (flute :midi-channel 1))))
        :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
        :set-map '((1 (1)))
        :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -)))))
        :rthm-seq-map '((1 ((fl (1))))))))
  (num-notes mini))

=> 8

|#
;;; SYNOPSIS
(defmethod num-notes ((sc slippery-chicken))
;;; ****
  (num-notes (piece sc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Wed May  9 13:11:31 BST 2012: Conformed robodoc entry

;;; ****m* slippery-chicken/cmn-display
;;; DESCRIPTION
;;; Write the data stored in a given slippery-chicken object to disk as an EPS
;;; (Encapsulated Postscript) file using CMN. 
;;;
;;; Several of the keyword arguments for this method are passed directly to CMN
;;; and therefore have identical names to CMN functions.
;;;
;;; NB: This might fail if LilyPond files are generated first. If this happens,
;;;     re-evaluate the slippery-chicken object and call cmn-display again.
;;; 
;;; ARGUMENTS 
;;; - A slippery-chicken object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :file. A string that is the directory path with file name and extension
;;;   for the .eps file to be created. Default = "cmn.eps" in the directory
;;;   (get-sc-config 'default-dir) (default "/tmp/") 
;;; - :players. NIL or a list of player IDs to indicate whether all players'
;;;   parts should be printed to the score. If NIL, all players' parts will be
;;;   written to the score. If a single symbol or a list of player IDs, only
;;;   the parts of those players will be written to the score. Default = NIL.
;;; - :in-c. T or NIL to indicate whether the output should be printed at
;;;   sounding pitch (in C) or at written pitch. NB: If in C, piccolo and
;;;   double bass maintain their usual octave transpositions.
;;;   T = print at sounding pitch. Default = NIL.
;;; - :respell-notes. T, a list of player IDs paired with a sequence of bar and
;;;   note numbers, or NIL to indicate whether to the cmn-display method should
;;;   call the respell-notes method to the pitches contained in the
;;;   slippery-chicken object according to slippery chicken's enharmonics
;;;   algorithm. If T, the all of the pitches in the object will be considered
;;;   and slippery chicken will convert a number of the pitches to their
;;;   enharmonic equivalents to create more sensible linear pitch progression
;;;   within bars. If a list of player IDs paired with a sequence of bar and
;;;   event numbers is passed, in the form 
;;;   '((vln (13 2) (14 3)) (cl (14 3 t))), only the specified pitches are
;;;   changed; e.g., (13 2) = bar 13 note 2 (1-based and counting tied notes
;;;   but not rests). If an additional T is included after the bar number and
;;;   event number (as in the cl example above), only the spelling of the
;;;   written pitch for that event will be changed (the default is to change
;;;   the sounding note spelling only). Chords are not respelled by the default
;;;   algorithm, so if these need to be respelled, this should be indicated by
;;;   sub-grouping the note number portion of the given bar/note pair into a
;;;   2-item sublist, in which the first number is the position of the chord in
;;;   among the attacked notes of that bar and the second number is the
;;;   position of the desired pitch within the chord, counted from the bottom
;;;   up, e.g. (vln (13 (2 1))). If NIL, no changes will be made. Default = T.
;;; - :auto-clefs. T or NIL to indicate whether the cmn-display method should
;;;   call the auto-clefs method, which automatically insert clef changes into
;;;   the parts of those instruments that use more than one clef. 
;;;   T = automatically place clef changes. Default = T.
;;; - :start-bar. An integer that indicates the first bar of the object to be
;;;   written to the resulting .eps file. NIL = the first bar. Default = NIL.
;;; - :end-bar. The last bar to be written to the resulting .eps file. NIL =
;;;   the last bar of the slippery-chicken object. Default = NIL.
;;; - :title. T, a string, or NIL to indicate whether to write the title of the
;;;   given slippery-chicken object to the resulting .eps file. If T, the TITLE
;;;   slot of the slippery-chicken object will be used. If a string, the
;;;   specified string will be used instead. If NIL, no title will be included
;;;   in the output. Default = T.
;;; - :size. A number to indicate the overall size of the symbols in the CMN
;;;   output. Default = 15.
;;; - :page-nums. T or NIL to indicate whether page numbers are to be written. 
;;;   T = write page numbers. Default = T.
;;; - :empty-staves. T or NIL to indicate whether an empty staff should be
;;;   displayed under each instrument. This can be useful for making editing
;;;   notes by hand. T = print empty staff. Default = NIL.
;;; - :display-sets. T or NIL to indicate whether to print the set of pitches
;;;   used for each rthm-seq on a separate treble-bass grand staff at the
;;;   bottom of each system in the score. T = print. Default = NIL.
;;; - :write-section-info. T or NIL to indicate whether to write the section ID
;;;   into the score. NB: This might not work without first regenerating the
;;;   slippery-chicken object. T = write section IDs. Default = NIL.
;;; - :display-time. T or NIL to indicate whether the elapsed time in
;;;   (mins:secs) should printed above each measure in the resulting score. 
;;;   T = print time. Default = NIL.
;;; - :staff-separation. A number that governs the amount of white space to be
;;;   placed between staves, measured in CMN's units. Default = 3.
;;; - :line-separation. A number that governs the amount of white space to be
;;;   placed between systems (i.e. not groups, but a line of music for the
;;;   whole ensemble), measured in CMN's units. Default = 5.
;;; - :group-separation. A number that governs the amount of white space placed
;;;   between groups in a system, measured in CMN's units. Default = 2.
;;; - :system-separation. An indication for how CMN determines the amount of
;;;   white space between systems. If cmn::page-mark, only one system will be
;;;   written per page. Default cmn::line-mark.
;;; - :page-height. A number to indicate the height of the page in centimeters.
;;;   Default = 29.7.
;;; - :page-width. A number to indicate the width of the page in
;;;   centimeters. Default = 21.0. 
;;; - :all-output-in-one-file. T or NIL to indicate whether to write a separate
;;;   file for each page of the resulting score. T = write all pages to the
;;;   same multi-page file. Default = T.
;;; - :one-line-per-page. T or NIL to indicate whether to write just one line
;;;   (system) to each page. T = one line per page. Default = NIL.
;;; - :start-bar-numbering. An integer that indicates the number to be given as
;;;   the first bar number in the resulting EPS file. The bars will be numbered
;;;   every five bars starting from this number. NB: The value of this argument
;;;   is passed directly to a CMN function. If a value is given for this
;;;   argument, slippery chicken's own bar-number writing function will be
;;;   disabled. NB: It is recommended that a value not be passed for this
;;;   argument if a value is given for :auto-bar-nums. NIL = bar 1. Default =
;;;   NIL.
;;; - :auto-bar-nums. An integer or NIL to indicate a secondary bar numbering
;;;   interval. This is separate from and in addition to the bar-number written
;;;   in every part every 5 bars. It corresponds to CMN's
;;;   automatic-measure-numbers. If set to e.g. 1, a bar number will be printed
;;;   for every measure at the top of each system, or if :by-line, a bar number
;;;   will be printed at the start of each line. NB: The value of this argument
;;;   is passed directly to a CMN function. If a value is given for this
;;;   argument, slippery chicken's own bar-number writing function will be
;;;   disabled. NB: It is recommended that a value not be passed for this
;;;   argument if a value is given for :start-bar-numbering. NIL = no secondary
;;;   bar numbering. Default = T.
;;; - :rehearsal-letters-all-players. T or NIL to indicate whether rehearsal
;;;   letters should be placed above the staves of all instruments in a score
;;;   (this can be useful when generating parts). If NIL, rehearsal letters are
;;;   only placed above the staves of the instruments at the top of each
;;;   group. T = place rehearsal letters above all instruments. Default = NIL.
;;; - :tempi-all-players. T or NIL to indicate whether to print the tempo above
;;;   all players' parts in the score. T = print above all players' parts. 
;;;   Default = NIL.
;;; - :process-event-fun. A user-defined function that takes one argument,
;;;   namely an event object. The specified function will then be called for
;;;   each event in the piece. This could be used, for example, to
;;;   algorithmically add accents, dynamics, or change the colour of notes,
;;;   etc. If NIL, no function will be applied. Default = NIL.
;;; - :automatic-octave-signs. T or NIL to indicate whether ottava signs should
;;;   be inserted automatically when notes would otherwise need many ledger
;;;   lines. T = automatically insert. Default = NIL.
;;; - :multi-bar-rests. T or NIL to indicate whether multiple bars of wrests
;;;   should be consolidated when writing parts. T = consolidate. NIL = write
;;;   each consecutive rest bar separately. Default = NIL.
;;; - :display-marks-in-part. T or NIL to indicate whether to print the marks
;;;   stored in the MARKS-IN-PART slot of each rhythm object in the score. If
;;;   NIL, the indications stored in the MARKS-IN-PART slot are added to parts
;;;   only. T = also print to score. Default = NIL.
;;; - :add-postscript. NIL or postscript code to be added to the .eps file
;;;   after it has been generated. See the add-ps-to-file function for details.
;;;   Default = NIL.
;;; - :auto-open. Whether to open the .EPS file once written. Currently only
;;;    available on OSX with SBCL and CCL. Uses the default app for .EPS
;;;    files, as if opened with 'open' in the terminal. Default = Value of
;;;    (get-sc-config cmn-display-auto-open).
;;;
;;; RETURN VALUE  
;;; Always T.
;;;
;;; EXAMPLE
#|
;;; The simplest usage
(let ((mini
       (make-slippery-chicken
        '+mini+
        :title "mini"
        :ensemble '(((vn (violin :midi-channel 1))))
        :tempo-map '((1 (q 60)))
        :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
        :set-map '((1 (1)))
        :rthm-seq-palette '((1 ((((2 4) (s) (s) e e e))
                                :pitch-seq-palette ((1 2 3)))))
        :rthm-seq-map '((1 ((vn (1))))))))
  (cmn-display mini :file "/tmp/mini.eps"))

;;; Used with some of the more frequently implemented keyword arguments
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((cl (b-flat-clarinet :midi-channel 1))
                     (hn (french-horn :midi-channel 2))
                     (vc (cello :midi-channel 3))))
        :tempo-map '((1 (q 60)))
        :set-palette '((1 ((f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5)))
                       (2 ((f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5)))
                       (3 ((f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5))))
        :set-map '((1 (1 1 1 1 1))
                   (2 (2 2 2 2 2))
                   (3 (3 3 3 3 3)))
        :rthm-seq-palette '((1 ((((4 4) h q e s s))
                                :pitch-seq-palette ((1 2 3 4 5))))
                            (2 ((((4 4) q e s s h))
                                :pitch-seq-palette ((1 2 3 4 5))))
                            (3 ((((4 4) e s s h q))
                                :pitch-seq-palette ((1 2 3 4 5)))))
        :rthm-seq-map '((1 ((cl (1 3 2 1 2))
                            (hn (3 1 1 2 2))
                            (vc (1 1 3 2 2))))
                        (2 ((cl (3 1 1 2 2))
                            (hn (1 3 1 2 2))
                            (vc (3 2 2 1 1))))
                        (3 ((cl (1 1 3 2 2))
                            (hn (2 1 1 2 3))
                            (vc (3 1 1 2 2))))))))
  (cmn-display mini 
               :file "/tmp/cmn.eps"
               :players '(cl vc)
               :in-c nil
               :respell-notes nil
               :auto-clefs nil
               :start-bar 8
               :end-bar 13
               :title "CMN Fragment"
               :size 13
               :page-nums nil
               :empty-staves t
               :display-sets t
               :write-section-info t
               :display-time t
               :staff-separation 2
               :line-separation 3))

=> T

|#
;;; 
;;; SYNOPSIS
(defmethod cmn-display ((sc slippery-chicken) 
                        &key
                        (respell-notes t)
                        (start-bar nil)
                        (start-bar-numbering nil)
                        (end-bar nil)
                        ;; MDE Fri Apr  6 13:27:08 2012 
                        (title t)
                        (file (format nil "~a~a.eps"
                                      (get-sc-config 'default-dir)
                                      (filename-from-title (title sc))))
                        (all-output-in-one-file t)
                        (one-line-per-page nil)
                        (staff-separation 3)
                        (line-separation 5)
                        (empty-staves nil)
                        (write-section-info nil)
                        (group-separation 2)
                        (system-separation #+cmn cmn::line-mark)
                        (process-event-fun nil)
                        (display-sets nil)
                        (rehearsal-letters-all-players nil)
                        (display-marks-in-part nil)
                        (tempi-all-players nil)
                        (players nil)
                        (page-height 29.7)
                        (page-width 21.0)
                        (size 15)
                        (auto-bar-nums t)
                        (page-nums t)
                        (in-c nil)
                        (auto-clefs t)
                        (multi-bar-rests nil)
                        (automatic-octave-signs nil)
                        (display-time nil)
                        (auto-open (get-sc-config 'cmn-display-auto-open))
                        (add-postscript nil))
;;; ****
  ;; MDE Wed Apr 18 10:57:41 2012 -- 
  ;; MDE Wed May  9 16:01:57 2012 -- don't have SC write bar nums if we're
  ;; having CMN do it 
  (if (or start-bar-numbering auto-bar-nums)
    (set-write-bar-num sc nil) ; just deletes all bar-nums
    (set-write-bar-num sc)) ; writes them every 5
  (when respell-notes
    (respell-notes sc respell-notes))
  ;; MDE Wed Apr 11 12:09:13 2012
  (setf players
        (cond ((and players (listp players)) players)
              ((and players (symbolp players)) (list players))
              (t (players (ensemble sc)))))
  (when rehearsal-letters-all-players 
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
                :delete-marks-before nil))
  ;; 26/4/10: some processes turn notes into rests so turn bars of rests only
  ;; into rest-bars proper 
  (cleanup-rest-bars sc)
  #+cmn (cmn-display (piece sc)
               :auto-bar-nums auto-bar-nums
               :start-bar start-bar
               :display-marks-in-part display-marks-in-part
               :start-bar-numbering start-bar-numbering
               :page-nums page-nums
               :group-separation group-separation
               :end-bar end-bar
               ;; MDE Mon May  7 21:35:16 2012 -- remember if we've called
               ;; multi-bar-rests beforehand we need to know about it
               :multi-bar-rests (or multi-bar-rests (multi-bar-rests-called sc))
               :bars-per-system-map (bars-per-system-map sc)
               :ensemble (ensemble sc)
               :all-output-in-one-file all-output-in-one-file
               :one-line-per-page one-line-per-page
               :instrument-change-map (instrument-change-map sc)
               :system-separation system-separation
               :file file
               ;; no subtitles in CMN
               :title (cond ((stringp title) title)
                            ((eq title T) (title sc)))
               :process-event-fun process-event-fun 
               :set-map (when display-sets (set-map sc))
               :empty-staves empty-staves
               :staff-groupings (staff-groupings sc)
               :write-section-info write-section-info
               :staff-separation staff-separation
               :line-separation line-separation
               :players players
               :in-c in-c
               ;; MDE Mon Jul  2 19:48:37 2012 
               :key-sig (key-sig sc)
               :page-height page-height
               :page-width page-width
               :automatic-octave-signs automatic-octave-signs
               :display-time display-time
               :size size)
  (when add-postscript
    (add-ps-to-file file add-postscript))
  (when auto-open
    (system-open-file file))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; replace any bar references with the true bar numbers
(defmethod convert-bar-refs-to-numbers ((sc slippery-chicken) map)
  (loop 
     with bnum
     for pair in map
     for bar = (first pair)
     for i from 0 do
     ;; when the bar number is a reference (of the form (section
     ;; sequenz-num bar-num)), then get the bar number of that
     ;; reference and replace it before making the
     ;; simple-change-map 
     (when (listp bar)
       ;; MDE Thu Feb 23 10:37:54 2012 -- make sure the ref is legal
       (setf bnum (get-bar-num-from-ref
                   sc (first bar) (second bar) (third bar)))
       (if bnum
           (setf (first (nth i map)) bnum)
           (error "slippery-chicken::convert-bar-refs-to-numbers:: 
                      can't get bar number for reference ~a" bar))))
  map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod (setf snd-output-dir) (dir (sc slippery-chicken))
  (setf (slot-value sc 'snd-output-dir) (trailing-slash dir)))

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
  (if (tempo-map-p tm)
      (setf (slot-value sc 'tempo-map) tm)
      (setf tm (convert-bar-refs-to-numbers sc tm)
            (slot-value sc 'tempo-map)
            (make-tempo-map (format nil "~a-~a" (id sc) 'tempo-map)
                            tm))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Mon Sep 24 22:06:50 2012 

(defmethod (setf sndfile-palette) (sfp (sc slippery-chicken))
  (setf (slot-value sc 'sndfile-palette)
        (if (sndfile-palette-p sfp)
            (clone sfp)
            ;; MDE Sat Dec 17 13:38:48 2016 -- don't make :extensions nil if no
            ;; third element to sndfile-palette slot--the default would be
            ;; overwritten and it's usually good enough
            (let ((id (format nil "~a-~a" (id sc) 'sound-file-palette))
                  (pal (first sfp))
                  (paths (second sfp))
                  (ext (third sfp)))
              (if ext
                  (make-sfp id pal :paths paths :extensions ext)
                  (make-sfp id pal :paths paths))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod (setf set-map-replacements) :after (smr (sc slippery-chicken))
  (setf (replacements (set-map sc)) smr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod (setf rthm-seq-map-replacements) :after (rsmr (sc slippery-chicken))
  (setf (replacements (rthm-seq-map sc)) rsmr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Mon May  5 19:20:39 2014 
(defmethod (setf rehearsal-letters) :after (list (sc slippery-chicken))
  ;; MDE Tue May 27 17:41:18 2014 -- must delete existing rehearsal letters on
  ;; bar objects first 
  (delete-all-rehearsal-letters sc)
  (when (and (initialized sc) list)
    (set-rehearsal-letters sc (get-groups-top-ins sc))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
                    ~%~a ~%must contain all the players of the ensemble ~a."
                   hierarchy ensemble-players))
          (loop for ins in hierarchy do
               (unless (member ins ensemble-players)
                 (error "slippery-chicken::check-instruments: ~
                          instrument-hierarchy ~a must contain all the ~
                          players of the ensemble ~a. You gave ~a"
                        hierarchy ensemble-players ins))))
        (setf (instruments-hierarchy sc) (copy-list ensemble-players)))
    (unless (= (length rsm-players) ensemble-players-len)
      ;; MDE Tue Aug  4 14:53:55 2020, Heidhausen -- now an option
      (when (get-sc-config 'warn-unused-instruments)
        (warn "slippery-chicken::check-instruments: ~%Number of instruments ~
               in the rthm-seq-map is not the same ~%as that in the ~
               ensemble!")))
    (if (instruments-write-bar-nums sc)
        (loop for bw in (instruments-write-bar-nums sc)
           unless (member bw ensemble-players) do
             (error "slippery-chicken::check-instruments:  ~
                     instruments-write-bar-nums contains reference to ~
                     instrument not in ensemble: ~a ~a" bw ensemble-players))
        ;; MDE Wed Apr 18 09:49:30 2012 -- 
        (when (staff-groupings sc)
          (setf (instruments-write-bar-nums sc)
                (get-groups-top-ins sc))))
    (loop for rsmp in rsm-players 
       unless (member rsmp ensemble-players) do
       (error "slippery-chicken::check-instruments: rthm-seq-map player ~a ~
                  not defined in ensemble" rsmp))
    t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;  Called by initialize-instance. This is where the magic happens :-)

;;; Clone the rthm-seq-map, loop through all the instruments in all the
;;; sections of the rsm (get-first then thereafter use the next slot). Get the
;;; list of rthm-seq refs, pass this with the section ref and the instrument to
;;; make-section-for player. In the rms-clone, replace the data of the current
;;; element with a section which is a list of sequenzs made by promoting the
;;; rthm-seq to a sequenz.

;;; Go through each and every bar for each instrument and update the
;;; write-time-sig slot of each rthm-seq-bar.

;;; Go through each event of each rthm-seq-bar and update compound-duration to
;;; reflect tempo. Then call handle-first-note-ties for each rthm-seq (to
;;; update compound durations of tied first notes of a bar dependent upon tempo
;;; also--a tempo could change over a tie between bars). 

;;; Go through each event and update start-time based on compound duration
;;; (only when the event has need-new-note).

(defmethod sc-make-piece ((sc slippery-chicken) &optional (warn-ties t))
  ;; MDE Mon Oct 28 17:24:54 2013 -- to delete the used-notes slot of each set
  ;; in the palette
  (reset (set-palette sc))
  (reset (pitch-seq-map sc))
  (reset (rthm-seq-palette sc))
  (let* ((rsm-clone (link-named-objects (clone (rthm-seq-map sc)))))
    (setf rsm-clone (sc-change-class rsm-clone 'piece))
    (rsm-to-piece rsm-clone sc)
    (setf (id rsm-clone) (format nil "~a-piece" (id sc))
          ;; the players from the rthm-seq-map might be in a different order
          ;; from those of the ensemble so copy the latter. 
          (players rsm-clone) (players (ensemble sc)))
    ;; (print rsm-clone)
    (add-rest-player-sections rsm-clone)
    (add-rest-sequenzes rsm-clone sc)
    (update-slots rsm-clone (tempo-map sc) 0.0 0.0 1 nil nil warn-ties)
    (handle-ties rsm-clone) ; compound-duration updated here
    ;; well, not ideal, but we have to do this again because of the
    ;; ties. First we need to do it to get bar numbers etc. for the
    ;; sequenzes, then we have to do it after the handle-ties to make
    ;; sure the compound-durations take tempo into consideration
    (update-slots rsm-clone (tempo-map sc) 0.0 0.0 1 nil nil warn-ties)
    (update-write-time-sig rsm-clone t)
    ;; 28.1.11 use -sig2 also to make sure each bar is really checked.
    ;; MDE Sat Apr 20 11:29:05 2013 -- t is ignored!
    ;; (update-write-time-sig2 rsm-clone t)
    (update-write-time-sig2 rsm-clone)
    rsm-clone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken/update-slots
;;; DESCRIPTION
;;; Called by initialize-instance and others. Updates timings of events and
;;; statistics. Not generally called by the user but can be useful if
;;; post-generation editing has changed something fundamental to the structure.
;;;
;;; If you want to update the statistics for each instrument in the ensemble,
;;; use the update-instrument-slots method.
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object
;;; 
;;; OPTIONAL ARGUMENTS
;;; - A tempo-map object (not just as a list). If not given, then the tempo-map
;;;   from the slippery-chicken object will be used. Default = NIL.
;;; - A number that is the start-time of the first event object in
;;;   seconds. Default = 0.0.
;;; - A number that is the start-time of the first event, in 'quarters' (for
;;;   MIDI timing). Default = 0.0.
;;; - A integer that is the number of the starting bar. Default = 1.
;;; - The reference of the current section (for internal recursive use in the
;;;   bar-holder class). Default = NIL.
;;; - The nth sequence (for internal recursive use in the sequenz class).
;;;   Default = NIL.
;;; - T or NIL to indicate whether to print a warning to the Lisp listener when
;;;   ties are being used at the beginning of a sequence. This argument is now
;;;   obsolete and ignored, but remains for some backward compatibility.
;;;   Default = T.
;;; 
;;; RETURN VALUE
;;; The duration in seconds of the object; in this class: the whole generated
;;; piece.
;;;
;;; EXAMPLE
#|
;;; Create a slippery-chicken object and print the start time of one of its
;;; events; call update-slots with a start time of 10.0 and print the start
;;; time of that same event to see the difference
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((vn (violin :midi-channel 1))))
        :tempo-map '((1 (q 60)))
        :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
        :set-map '((1 (1 1 1 1 1)))
        :rthm-seq-palette '((1 ((((2 4) (s) (s) e e e))
                                :pitch-seq-palette ((1 2 3)))))
        :rthm-seq-map '((1 ((vn (1 1 1 1 1))))))))
  (print (start-time (get-event mini 4 1 'vn)))
  (update-slots mini nil 10.0)
  (print (start-time (get-event mini 4 1 'vn))))

=
6.0 
16.0

|#
;;; 
;;; SYNOPSIS
(defmethod update-slots ((sc slippery-chicken) 
                         &optional
                         (tempo-map nil)
                         (start-time 0.0)
                         (start-time-qtrs 0.0)
                         (start-bar 1)
                         (current-section nil)
                         (nth nil)
                         (warn-ties t)
                         (update-write-bar-nums nil))
;;; ****
  (prog1
      (update-slots (piece sc) 
                    (if tempo-map tempo-map (tempo-map sc))
                    start-time start-time-qtrs start-bar current-section nth
                    warn-ties)
    (when (get-sc-config 'update-slots-handles-ties)
      ;; MDE Thu Sep 13 18:42:59 2018 -- surely!
      (handle-ties sc))
    ;; MDE Sat Nov 27 12:17:44 2021, Heidhausen -- in case we've setf the
    ;; tempo-map, we need to add new tempo-change slots to the events of the
    ;; first bar of each voice where tempo changes in the new map
    (update-events-tempo sc)
    (when update-write-bar-nums
      (set-write-bar-num sc))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod update-instruments-total-duration ((sc slippery-chicken)
                                              &optional and-other-slots)
  (loop for player in (players sc) do
       ;; just update total-duration or all stats?
       (reset-instrument-stats (get-player sc player) (not and-other-slots))
       ;; (print player)
       (loop for bar-num from 1 to (num-bars sc) 
          for bar = (get-bar sc bar-num player)
          for ins = (get-instrument-for-player-at-bar player bar-num sc)
          do
            (gen-stats bar)
            ;; (print (total-degrees ins))
            (when and-other-slots
              (unless (is-rest-bar bar)
                ;; MDE Sat Sep 26 16:03:20 2020, Heidhausen
                (update-lowest-highest-played ins (lowest bar) (highest bar))
                (incf (total-bars ins))
                (incf (total-notes ins) (notes-needed bar))
                (incf (total-degrees ins) (total-degrees bar))))
            (incf (total-duration ins) (sounding-duration bar))))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken/update-instrument-slots
;;; DATE
;;; 23rd August 2013
;;; 
;;; DESCRIPTION
;;; This will go through the generated slippery-chicken object's bar structure
;;; and update each players' instruments' total-bars, total-notes,
;;; total-duration, and total-degrees slots, for statistical purposes only.
;;; This might be called by the user after performing one of the editing
;;; routines that deletes or changes notes, etc.
;;; 
;;; ARGUMENTS
;;; - The slippery-chicken object.
;;; 
;;; RETURN VALUE
;;; T
;;; 
;;; SYNOPSIS
(defmethod update-instrument-slots ((sc slippery-chicken))
;;; ****
  (update-instruments-total-duration sc t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Wed Apr 18 10:10:58 2012 -- currently only works for CMN.
(defmethod set-write-bar-num ((sc slippery-chicken) &optional (every 5))
  ;; (print 'write-bar-num)
  (loop for player in (players sc) do
       (loop for bar-num from 1 to (num-bars sc) 
          for bar = (get-bar sc bar-num player)
          do
          (unless bar
            (error "slippery-chicken::set-write-bar-num: no bar ~a for ~a"
                   bar-num player))
          (setf (write-bar-num bar) nil)))
  ;; MDE Wed May  9 16:01:14 2012 
  (when (integerp every)
    (loop for player in (instruments-write-bar-nums sc) do
         (loop for i from (1- every) to (1- (num-bars sc)) by every do
              (setf (write-bar-num (get-bar sc i player)) t))))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Called by rsm-to-piece (function)

;;; Loop through each rthm-seq reference, get the rthm-seq, pull out the
;;; associated pitch-seq from the pitch-seq-map (using the 'this' ref of the
;;; instrument as look-up into the psm). Get the harmony for this seq, the
;;; hint-pitch, and for each rthm-seq call sc-make-sequenz

;;; In order to be able to limit sets, we have to keep count of the number of
;;; sequences as we process them. This has to be done on an instrument by
;;; instrument basis however, as the piece is not generated seq by seq (all
;;; instruments in one go). So seq-count becomes an assoc-list with each
;;; player as keys and the data being the count of sequences.

(let ((seq-count nil)) ; will be an assoc-list with IDs being player symbols
  (defmethod make-section-for-player (lno (sc slippery-chicken) 
                                      &optional
                                        ;; this is the last event of the
                                        ;; previous section (which could be
                                        ;; nil)
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
                                                             (pitch-seq-map
                                                              sc)))
                                ;; MDE Thu May  5 10:04:48 2016 -- this is
                                ;; where we would morph sets if needed
                                (set (get-nth-from-palette section-ref seq-num
                                                           (set-map sc)))
                                hint-pitch seq)
                           #|
                           ;; getting the current set ref and the next
                           (let ((m (get-data-data section-ref
                                                   (set-map sc))))
                             (format t "~&this set: ~a next: ~a"
                                     (nth seq-num m) (nth (1+ seq-num) m)))
                           |#
                           (multiple-value-bind
                                 ;; have to get the second values result here
                                 ;; so we know whether to trigger a
                                 ;; program-change or not
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
;;; ****m* slippery-chicken/get-player
;;; DESCRIPTION
;;; Return the player object for the specified player.
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; - A player ID.
;;; 
;;; RETURN VALUE
;;; A player object.
;;; 
;;; EXAMPLE
;;;
#|
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((fl (flute :midi-channel 1))
                     (tp (b-flat-trumpet :midi-channel 2))
                     (vn (violin :midi-channel 3))))
        :set-palette '((1 ((f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5))))
        :set-map '((1 (1 1 1 1 1)))
        :rthm-seq-palette '((1 ((((4 4) h q e s s))
                                :pitch-seq-palette ((1 2 3 4 5)))))
        :rthm-seq-map '((1 ((fl (1 1 1 1 1))
                            (tp (1 1 1 1 1))
                            (vn (1 1 1 1 1))))))))
  (get-player mini 'vn))
=> 
PLAYER: (id instrument-palette): SLIPPERY-CHICKEN-STANDARD-INSTRUMENT-PALETTE 
doubles: NIL, cmn-staff-args: NIL, total-notes: 25, total-degrees: 3548, 
total-duration: 20.000, total-bars: 5, tessitura: B4
LINKED-NAMED-OBJECT: previous: (TP), this: (VN), next: NIL
NAMED-OBJECT: id: VN, tag: NIL, 
data: 
INSTRUMENT: lowest-written: G3, highest-written: C7
...
|#
;;; SYNOPSIS
(defmethod get-player ((sc slippery-chicken) player)
;;; ****
  (get-player (ensemble sc) player))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Wed May  9 17:47:23 BST 2012: Added robodoc entry

;;; ****m* slippery-chicken/num-bars
;;; DESCRIPTION
;;; Return the number of bars in the piece.
;;; 
;;; ARGUMENTS 
;;; - A slippery-chicken object.
;;; 
;;; RETURN VALUE  
;;; An integer that is the number of bars.
;;;
;;; EXAMPLE
#|
(let ((mini
(make-slippery-chicken
'+mini+
:ensemble '(((fl (flute :midi-channel 1))
(tp (b-flat-trumpet :midi-channel 2))
(vn (violin :midi-channel 3))))
:set-palette '((1 ((f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5))))
:set-map '((1 (1 1 1 1 1)))
:rthm-seq-palette '((1 ((((4 4) h q e s s))
:pitch-seq-palette ((1 2 3 4 5)))))
:rthm-seq-map '((1 ((fl (1 1 1 1 1))
(tp (1 1 1 1 1))
(vn (1 1 1 1 1))))))))
(num-bars mini))

=> 5

|#
;;; 
;;; SYNOPSIS
(defmethod num-bars ((sc slippery-chicken))
;;; ****
  (num-bars (piece sc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Wed May  9 17:50:29 BST 2012: Conformed robodoc entry

;;; ****m* slippery-chicken/get-bar-from-ref
;;; DESCRIPTION
;;; Return a rthm-seq-bar object from the piece by specifying its section,
;;; sequence number, bar number, and the player. Sequenz-num and bar-num are
;;; 1-based.
;;; 
;;; ARGUMENTS 
;;; - A slippery-chicken object.
;;; - A section ID (number or list).
;;; - A player ID.
;;; - An integer that is the number of the sequence in the section from which
;;;   the bar is to be returned (1-based).
;;; - An integer that is the number of the bar within the given sequence
;;;   (1-based). 
;;; 
;;; RETURN VALUE  
;;; A rthm-seq-bar object.
;;;
;;; EXAMPLE
#|
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((cl (b-flat-clarinet :midi-channel 1))
                     (vc (cello :midi-channel 2))))
        :set-palette '((1 ((f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5))))
        :set-map '((1 (1 1 1 1 1))
                   (2 (1 1 1 1 1))
                   (3 (1 1 1 1 1)))
        :rthm-seq-palette '((1 ((((4 4) h q e s s)
                                 (q e s s h)
                                 (e s s q h))
                                :pitch-seq-palette ((1 2 3 4 5 
                                                       1 3 2 4 5 
                                                       3 5 2 4 1))))) 
        :rthm-seq-map '((1 ((cl (1 1 1 1 1))
                            (vc (1 1 1 1 1))))
                        (2 ((cl (1 1 1 1 1))
                            (vc (1 1 1 1 1))))
                        (3 ((cl (1 1 1 1 1))
                            (vc (1 1 1 1 1))))))))
  (get-bar-from-ref mini 2 'vc 3 2))
=>
RTHM-SEQ-BAR: time-sig: 2 (4 4), time-sig-given: NIL, bar-num: 23, 
old-bar-nums: NIL, write-bar-num: NIL, start-time: 88.000, 
start-time-qtrs: 88.0, is-rest-bar: NIL, multi-bar-rest: NIL, 
show-rest: T, notes-needed: 5, 
tuplets: NIL, nudge-factor: 0.35, beams: NIL, 
current-time-sig: 2, write-time-sig: NIL, num-rests: 0, 
num-rhythms: 5, num-score-notes: 5, parent-start-end: NIL, 
missing-duration: NIL, bar-line-type: 0, 
player-section-ref: (2 VC), nth-seq: 2, nth-bar: 1, 
rehearsal-letter: NIL, all-time-sigs: (too long to print) 
sounding-duration: 4.000, 
rhythms: (
[...]

|#
;;; 
;;; SYNOPSIS
(defmethod get-bar-from-ref ((sc slippery-chicken) section player
                             sequenz-num bar-num)
;;; ****
  (get-bar-from-ref (piece sc) section player sequenz-num bar-num))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; sequenz-num and bar-num are 1-based.

;;; ****m* slippery-chicken/get-bar-num-from-ref
;;; DESCRIPTION
;;; Get the bar number of a given rthm-seq-bar object by specifying the
;;; section, sequenz, and number of the bar within that sequenz.
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; - The ID of the section in which the given rthm-seq-bar object is located. 
;;; - An integer that is the number of the sequence (starting from 1 not 0)
;;;   within that section in which the rthm-seq-bar object is located.
;;; - The number of the bar within the given rthm-seq-bar object for which the
;;;   overall bar number (within the entire piece) is sought.
;;; 
;;; RETURN VALUE
;;; An integer.
;;; 
;;; EXAMPLE
#|
(let ((mini
(make-slippery-chicken
'+mini+
:ensemble '(((cl (b-flat-clarinet :midi-channel 1))
(vc (cello :midi-channel 2))))
:set-palette '((1 ((f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5))))
:set-map '((1 (1 1 1 1 1))
(2 (1 1 1 1 1))
(3 (1 1 1 1 1)))
:rthm-seq-palette '((1 ((((4 4) h q e s s)
(q e s s h)
(e s s q h))
:pitch-seq-palette ((1 2 3 4 5 
1 3 2 4 5 
3 5 2 4 1))))) 
:rthm-seq-map '((1 ((cl (1 1 1 1 1))
(vc (1 1 1 1 1))))
(2 ((cl (1 1 1 1 1))
(vc (1 1 1 1 1))))
(3 ((cl (1 1 1 1 1))
(vc (1 1 1 1 1))))))))
(get-bar-num-from-ref mini 2 4 3))

=> 27

|#
;;; SYNOPSIS
(defmethod get-bar-num-from-ref ((sc slippery-chicken) section
                                 sequenz-num bar-num)
;;; ****
   (get-bar-num-from-ref (piece sc) section sequenz-num bar-num))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Wed May  9 18:12:48 BST 2012: Added robodoc entry

;;; MDE original comment: It is optional so that we can have a sequenz method
;;;   with the same name which only requires the bar-num argument.

;;; 15/3/03: change this so that if player is nil, then we get the bar for all
;;; players in the ensemble. 

;;; ****m* slippery-chicken/get-bar
;;; DESCRIPTION
;;; Get the rthm-seq-bar object located at a specified bar number within a
;;; given player's part.
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; - An integer that is the number of the bar within the overall piece for
;;;   which the rthm-seq-bar object is sought.
;;; - The ID of the player from whose part the rthm-seq-bar object is
;;;   sought. If this is passed as NIL, the method will return the rthm-seq-bar
;;;   objects for all players in the ensemble at the specified bar number. 
;;;   NB: Although listed as an optional argument, the player ID is actually
;;;   required. It is listed as optional due to method inheritance.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - (see the comment on the <player> argument above.
;;;
;;; RETURN VALUE
;;; A rthm-seq-bar object (or objects).
;;; 
;;; EXAMPLE
#|
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((cl (b-flat-clarinet :midi-channel 1))
                     (vc (cello :midi-channel 2))))
        :set-palette '((1 ((f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5))))
        :set-map '((1 (1 1 1 1 1))
                   (2 (1 1 1 1 1))
                   (3 (1 1 1 1 1)))
        :rthm-seq-palette '((1 ((((4 4) h q e s s)
                                 (q e s s h)
                                 (e s s q h))
                                :pitch-seq-palette ((1 2 3 4 5 
                                                       1 3 2 4 5 
                                                       3 5 2 4 1))))) 
        :rthm-seq-map '((1 ((cl (1 1 1 1 1))
                            (vc (1 1 1 1 1))))
                        (2 ((cl (1 1 1 1 1))
                            (vc (1 1 1 1 1))))
                        (3 ((cl (1 1 1 1 1))
                            (vc (1 1 1 1 1))))))))
  (get-bar mini 17 'cl))

=>
RTHM-SEQ-BAR: time-sig: 2 (4 4), time-sig-given: NIL, bar-num: 17, 
old-bar-nums: NIL, write-bar-num: NIL, start-time: 64.000, 
start-time-qtrs: 64.0, is-rest-bar: NIL, multi-bar-rest: NIL, 
show-rest: T, notes-needed: 5, 
tuplets: NIL, nudge-factor: 0.35, beams: NIL, 
current-time-sig: 2, write-time-sig: NIL, num-rests: 0, 
num-rhythms: 5, num-score-notes: 5, parent-start-end: NIL, 
missing-duration: NIL, bar-line-type: 0, 
player-section-ref: (2 CL), nth-seq: 0, nth-bar: 1, 
rehearsal-letter: NIL, all-time-sigs: (too long to print) 
sounding-duration: 4.000, 
rhythms: (
[...]

|#
;;; SYNOPSIS
(defmethod get-bar ((sc slippery-chicken) bar-num &optional player)
;;; ****
  (if player
      (get-bar (piece sc) bar-num player)
      (let ((players (players (ensemble sc))))
        (loop for p in players collect (get-bar (piece sc) bar-num p)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod get-bars ((sc slippery-chicken) start-bar end-bar
                     &optional player (on-fail #'error))
  (loop for bn from start-bar to end-bar
        for bar = (get-bar sc bn player)
        do
           (when (and (functionp on-fail) (not bar))
             (funcall on-fail "slippery-chicken::get-bars: no bar ~a" bn))
        collect bar))

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
;;; ****m* slippery-chicken/count-notes
;;; DESCRIPTION
;;; Returns the number of notes between start-bar and end-bar (both
;;; inclusive). 
;;; 
;;; ARGUMENTS 
;;; - A slippery-chicken object.
;;; - An integer that is the first bar in which notes will be counted. If NIL,
;;;   then 1 
;;; - An integer that is the last bar in which notes will be counted. If NIL,
;;;   then the last bar of the piece.
;;;
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to indicate whether to count just the number of attacked notes
;;;   (not including ties) or the number of note events (including ties). 
;;;   T = just attacked notes. Default = NIL.
;;;   NB: A chord counts as one note only. 
;;; - NIL or a list of one or more IDs of the players whose notes should be
;;;   counted. This can be a single symbol or a list of players. If NIL, the
;;;   notes in all players' parts will be counted. Default = NIL.
;;; 
;;; RETURN VALUE  
;;; An integer that is the number of notes.
;;; 
;;; 
;;; EXAMPLE
#|
;;; Using defaults
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((cl (b-flat-clarinet :midi-channel 1))
                     (vc (cello :midi-channel 2))))
        :set-palette '((1 ((f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5))))
        :set-map '((1 (1 1 1 1 1))
                   (2 (1 1 1 1 1))
                   (3 (1 1 1 1 1)))
        :rthm-seq-palette '((1 ((((4 4) h (q) e (s) s)
                                 (q (e) s +s h)
                                 ((e) s (s) (q) h))
                                :pitch-seq-palette ((1 2 3 4 5 1 3 2)))))
        :rthm-seq-map '((1 ((cl (1 1 1 1 1))
                            (vc (1 1 1 1 1))))
                        (2 ((cl (1 1 1 1 1))
                            (vc (1 1 1 1 1))))
                        (3 ((cl (1 1 1 1 1))
                            (vc (1 1 1 1 1))))))))
  (count-notes mini 2 11))

=> 62

;;; Counting all notes just for player 'vc ;
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((cl (b-flat-clarinet :midi-channel 1))
                     (vc (cello :midi-channel 2))))
        :set-palette '((1 ((f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5))))
        :set-map '((1 (1 1 1 1 1))
                   (2 (1 1 1 1 1))
                   (3 (1 1 1 1 1)))
        :rthm-seq-palette '((1 ((((4 4) h (q) e (s) s)
                                 (q (e) s +s h)
                                 ((e) s (s) (q) h))
                                :pitch-seq-palette ((1 2 3 4 5 1 3 2)))))
        :rthm-seq-map '((1 ((cl (1 1 1 1 1))
                            (vc (1 1 1 1 1))))
                        (2 ((cl (1 1 1 1 1))
                            (vc (1 1 1 1 1))))
                        (3 ((cl (1 1 1 1 1))
                            (vc (1 1 1 1 1))))))))
  (count-notes mini 2 11 nil 'vc))

=> 31

;;; Counting just the attacked notes for player 'vc ;
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((cl (b-flat-clarinet :midi-channel 1))
                     (vc (cello :midi-channel 2))))
        :set-palette '((1 ((f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5))))
        :set-map '((1 (1 1 1 1 1))
                   (2 (1 1 1 1 1))
                   (3 (1 1 1 1 1)))
        :rthm-seq-palette '((1 ((((4 4) h (q) e (s) s)
                                 (q (e) s +s h)
                                 ((e) s (s) (q) h))
                                :pitch-seq-palette ((1 2 3 4 5 1 3 2)))))
        :rthm-seq-map '((1 ((cl (1 1 1 1 1))
                            (vc (1 1 1 1 1))))
                        (2 ((cl (1 1 1 1 1))
                            (vc (1 1 1 1 1))))
                        (3 ((cl (1 1 1 1 1))
                            (vc (1 1 1 1 1))))))))
  (count-notes mini 2 11 t 'vc))

=> 27

|#
;;; 
;;; SYNOPSIS
(defmethod count-notes ((sc slippery-chicken) start-bar end-bar 
                        &optional just-attacks players)
;;; ****
  (unless players
    (setf players (players sc)))
  (unless (listp players)
    (setf players (list players)))
  ;; MDE Mon Sep 24 13:48:47 2018 -- 
  (unless start-bar (setq start-bar 1))
  (unless end-bar (setq end-bar (num-bars sc)))
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
;;; ****m* slippery-chicken/find-note
;;; DATE
;;; 09-Apr-2011
;;;
;;; DESCRIPTION
;;; Print to the Listener the numbers of all bars in a specified player's part
;;; of a given slippery-chicken object in which the specified pitch is
;;; found. Note that pitch symbols rather than objects are searched for so
;;; that small microtonal variations are be ignored. 
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; - A player ID.
;;; - A note-name pitch symbol or a list of note-name pitch symbols for the
;;;   pitch to be sought. If a list, this will be handled as a chord.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :written. T or NIL to indicate whether to look for the specified pitch as
;;;   as a written note only. T = as written only. Default = NIL.
;;; - :start-bar. An integer that is the first bar in which to search for the
;;;   given pitch. This number is inclusive. Default = 1.
;;; - :end-bar. An integer that is the last bar in which to search for the
;;;   given pitch. This number is inclusive. Default = number of bars in the
;;;   given slippery-chicken object.
;;; 
;;; RETURN VALUE
;;; Returns a list of all corresponding event objects found. Prints the bar
;;; numbers of the results directly to the Lisp listener.
;;; 
;;; EXAMPLE
#|
;;; Prints the bar number for all occurrences in the entire piece by default ;
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((cl (b-flat-clarinet :midi-channel 1))
                     (vc (cello :midi-channel 2))))
        :set-palette '((1 ((f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5))))
        :set-map '((1 (1 1 1 1 1))
                   (2 (1 1 1 1 1))
                   (3 (1 1 1 1 1)))
        :rthm-seq-palette '((1 ((((4 4) h (q) e (s) s)
                                 (q (e) s +s h)
                                 ((e) s (s) (q) h))
                                :pitch-seq-palette ((1 2 3 4 5 1 3 2)))))
        :rthm-seq-map '((1 ((cl (1 1 1 1 1))
                            (vc (1 1 1 1 1))))
                        (2 ((cl (1 1 1 1 1))
                            (vc (1 1 1 1 1))))
                        (3 ((cl (1 1 1 1 1))
                            (vc (1 1 1 1 1))))))))
  (find-note mini 'vc 'f4))

=>
bar 1
bar 3
bar 4
bar 6
bar 7
bar 9
bar 10
bar 12
bar 13
bar 15
bar 16
bar 18
bar 19
bar 21
bar 22
bar 24
bar 25
bar 27
bar 28
bar 30
bar 31
bar 33
bar 34
bar 36
bar 37
bar 39
bar 40
bar 42
bar 43
bar 45

;;; Examples of use specifying the optional arguments ;
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((cl (b-flat-clarinet :midi-channel 1))
                     (vc (cello :midi-channel 2))))
        :set-palette '((1 ((f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5))))
        :set-map '((1 (1 1 1 1 1))
                   (2 (1 1 1 1 1))
                   (3 (1 1 1 1 1)))
        :rthm-seq-palette '((1 ((((4 4) h (q) e (s) s)
                                 (q (e) s +s h)
                                 ((e) s (s) (q) h))
                                :pitch-seq-palette ((1 2 3 4 5 1 3 2)))))
        :rthm-seq-map '((1 ((cl (1 1 1 1 1))
                            (vc (1 1 1 1 1))))
                        (2 ((cl (1 1 1 1 1))
                            (vc (1 1 1 1 1))))
                        (3 ((cl (1 1 1 1 1))
                            (vc (1 1 1 1 1))))))))
  (find-note mini 'cl 'f3)
  (find-note mini 'cl 'f3 :written t)
  (find-note mini 'vc 'f4 :start-bar 3 :end-bar 17))

|#
;;; SYNOPSIS
(defmethod find-note ((sc slippery-chicken) player note &key (written nil)
                      start-bar end-bar)
;;; ****
  (unless start-bar
    (setf start-bar 1))
  (unless end-bar
    (setf end-bar (num-bars sc)))
  (next-event sc player nil start-bar)
  (let* ((chord? (listp note))
         (find (if chord? (make-chord note) (make-pitch note)))
         (result '())
         p)
    (loop for e = (next-event sc player) while e do
         (when (> (bar-num e) end-bar)
           (return))
         (setq p (if written (written-pitch-or-chord e) (pitch-or-chord e)))
         (when (and p (if chord?
                          ;; chord-equal compares symbols, chord= the objects
                          (and (chord-p find) (chord-p p) (chord-equal find p))
                          ;; MDE Fri Dec 20 15:01:33 2019 -- symbol comparison
                          ;; (and (pitch-p find) (pitch-p p) (pitch= find p))))
                          (and (pitch-p find) (pitch-p p) (eq (data find)
                                                              (data p)))))
           (push e result)
           (format t "~&bar ~a" (bar-num e))))
    (nreverse result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Wed May  9 19:04:54 BST 2012: Added robodoc entry

;;; ****m* slippery-chicken/players
;;; DESCRIPTION
;;; Return a list of all player IDs from the given slippery-chicken object. 
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; 
;;; RETURN VALUE
;;; A list of player IDs.
;;; 
;;; EXAMPLE
#|
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((cl (b-flat-clarinet :midi-channel 1))
                     (hn (french-horn :midi-channel 2))
                     (vc (cello :midi-channel 3))))
        :set-palette '((1 ((f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5))))
        :set-map '((1 (1 1 1)))
        :rthm-seq-palette '((1 ((((4 4) h (q) e (s) s))
                                :pitch-seq-palette ((1 2 3)))))
        :rthm-seq-map '((1 ((cl (1 1 1))
                            (hn (1 1 1))
                            (vc (1 1 1))))))))
  (players mini))

=> (CL HN VC)

|#
;;; SYNOPSIS
(defmethod players ((sc slippery-chicken))
;;; ****
  (when (piece-p (piece sc)) (players (piece sc))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Wed May  9 19:10:24 BST 2012: Conformed robodoc entry

;;; Some of MDE's original comments: ; when it's nil when we actually get
;;; events returned. 

(let ((current-bar nil)
      (current-bar-num 1)
      (current-event-num 0))
;;; ****m* slippery-chicken/next-event
;;; DESCRIPTION
;;; Get the events from a specified player's part within a given
;;; slippery-chicken object one after the other (e.g. in a loop). This method
;;; must be called once with a bar number (or any other non-NIL value,
;;; whereupon we start at bar 1) first in order to reset the counter; doing
;;; this will return NIL. Once the counter has been reset, calling the method
;;; without a bar number will return the events in sequence.
;;; 
;;; ARGUMENTS 
;;; - A slippery-chicken object.
;;; - A player ID.
;;;
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to indicate whether to return only events that consist of
;;;   attacked notes (i.e., no ties or rests). T = return only events with
;;;   attacked notes. Default = NIL.
;;; - T, NIL, or an integer to indicate the first bar from which events are to
;;;   be retrieved. If T, the counter is reset to the first event of the
;;;   player's part. This should be NIL after the first resetting call.
;;;   Default = NIL
;;; - NIL or an integer to indicate the last bar from which events are to be
;;;   retrieved. If NIL, all events will be retrieved from the starting point
;;;   to the last event in the given slippery-chicken object. Default = NIL.
;;;   NB This arg should be used in the second call to next-event.
;;;   E.g.: (next-event sc player nil start-bar)
;;;         (loop for ne = (next-event sc player nil nil end-bar)
;;;               while ne
;;;               etc...
;;; 
;;; RETURN VALUE  
;;; An event object
;;; 
;;; EXAMPLE
#|
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((cl (b-flat-clarinet :midi-channel 1))
                     (hn (french-horn :midi-channel 2))
                     (vc (cello :midi-channel 3))))
        :set-palette '((1 ((f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5))))
        :set-map '((1 (1 1 1)))
        :rthm-seq-palette '((1 ((((4 4) h (q) e (s) s))
                                :pitch-seq-palette ((1 2 3)))))
        :rthm-seq-map '((1 ((cl (1 1 1))
                            (hn (1 1 1))
                            (vc (1 1 1))))))))
  (next-event mini 'vc nil t)
  (loop for ne = (next-event mini 'vc)
     while ne
  collect (get-pitch-symbol ne)))

=> (E4 NIL F4 NIL G4 E4 NIL F4 NIL G4 E4 NIL F4 NIL G4)

|#
;;; 
;;; SYNOPSIS
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
           ;; (print (bar-num current-bar))
           (unless current-bar
             (error "slippery-chicken::next-event: no bar number ~a for ~a!" 
                    current-bar-num player))
           ;; (format t "~&bar ~a cen ~a nr ~a"
           ;; (bar-num current-bar) current-event-num (num-rhythms current-bar))
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

;;; SAR Wed May  9 21:09:59 BST 2012: Conformed robodoc entry

;;; ****m* slippery-chicken/get-note 
;;; DESCRIPTION
;;; Get a numbered event from a specified bar of a given player's part within a
;;; slippery-chicken object.
;;; 
;;; NB: Slippery-chicken doesn't have 'note' and 'rest' classes, rather both of
;;;     these are events. The nomenclature 'note' and 'rest' are thus used here
;;;     and elsewhere merely for convenience, to distinguish between sounding
;;;     and non-sounding events.
;;;
;;; See also rthm-seq-bar methods for accessing notes by other means.
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; - An integer that is the number of the bar from which to get the note
;;;   (counting from 1). 
;;; - An integer that is the number of the note to get within that bar,
;;;   counting tied notes (counting from 1). This can also be a list of numbers
;;;   if accessing pitches in a chord (see below).
;;; - The ID of the player from whose part the note is to be retrieved.
;;; 
;;; OPTIONAL ARGUMENTS 
;;; - T or NIL to indicate whether, when accessing a pitch in a chord, to
;;;   return the written or sounding pitch. T = written. Default = NIL.
;;; 
;;; RETURN VALUE
;;; An event object.
;;; 
;;; EXAMPLE
#|
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((vn (violin :midi-channel 1))))
        :tempo-map '((1 (q 60)))
        :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
        :set-map '((1 (1)))
        :rthm-seq-palette '((1 ((((2 4) (e) e+e. 32 (32)))
                                :pitch-seq-palette (((1) 2)))))
        :rthm-seq-map '((1 ((vn (1))))))))
  (print (data (get-rest mini 1 2 'vn)))
  (print (data (get-note mini 1 2 'vn)))
  (print (data (get-note mini 1 '(2 1) 'vn)))
  (print (data (get-note mini 1 '(2 2) 'vn)))
  (print (is-tied-from (get-note mini 1 1 'vn))))

=>
32 
"E." 
C4 
A4
T
|#
;;; SYNOPSIS
(defmethod get-note ((sc slippery-chicken) bar-num note-num player 
                     &optional written)
;;; ****
  (get-note (piece sc) bar-num note-num player written))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Wed May  9 21:21:47 BST 2012: Added robodoc entry

;;; ****m* slippery-chicken/get-rest
;;; DESCRIPTION
;;; Retrieve the event object that contains the specified rest in a
;;; slippery-chicken object by giving bar number, rest number and player.
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; - An integer that is the number of the bar from which to retrieve the rest
;;;   event object.
;;; - An integer that is the number of the rest (not the number of the event)
;;;   within that bar, counting from 1.
;;; - The ID of the player from whose part to retrieve the rest object.
;;; 
;;; RETURN VALUE
;;; An event object.
;;; 
;;; EXAMPLE
#|
(let ((mini
(make-slippery-chicken
'+mini+
:ensemble '(((vn (violin :midi-channel 1))
(vc (cello :midi-channel 2))))
:tempo-map '((1 (q 60)))
:set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
:set-map '((1 (1 1 1)))
:rthm-seq-palette '((1 ((((2 4) (e) e+e. 32 (32)))
:pitch-seq-palette (((1) 2)))))
:rthm-seq-map '((1 ((vn (1 1 1))
(vc (1 1 1))))))))
(get-rest mini 2 1 'vc))

=> 
EVENT: start-time: 2.000, end-time: 2.500, 
duration-in-tempo: 0.500, 
compound-duration-in-tempo: 0.500, 
amplitude: 0.700 
bar-num: 2, marks-before: NIL, 
tempo-change: NIL 
instrument-change: NIL 
display-tempo: NIL, start-time-qtrs: 2.000, 
midi-time-sig: NIL, midi-program-changes: NIL, 
8va: 0
pitch-or-chord: NIL
written-pitch-or-chord: NIL
RHYTHM: value: 8.000, duration: 0.500, rq: 1/2, is-rest: T, 
score-rthm: 8.0, undotted-value: 8, num-flags: 1, num-dots: 0, 
is-tied-to: NIL, is-tied-from: NIL, compound-duration: 0.500, 
is-grace-note: NIL, needs-new-note: NIL, beam: NIL, bracket: NIL, 
rqq-note: NIL, rqq-info: NIL, marks: NIL, marks-in-part: NIL, 
letter-value: 8, tuplet-scaler: 1, grace-note-duration: 0.05
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: E, tag: NIL, 
data: E

|#
;;; SYNOPSIS
(defmethod get-rest ((sc slippery-chicken) bar-num rest-num player)
;;; ****
  (get-rest (piece sc) bar-num rest-num player))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Wed May  9 21:28:50 BST 2012: Added robodoc entry

;;; ****m* slippery-chicken/get-event
;;; DESCRIPTION
;;; Retrieve a specified event object from a slippery-chicken object, giving
;;; bar number, event number, and player.
;;;
;;; NB: This counts returns event objects, regardless of whether they are notes
;;;     or rests.
;;; 
;;; ARGUMENTS 
;;; - A slippery-chicken object.
;;; - An integer that is the number of the bar from which the event object is
;;;   to be returned.
;;; - An integer that is the number of the event object to be returned from
;;;   that bar. This number is 1-based and counts all events, including notes,
;;;   rests, and tied notes.
;;; - A symbol name for the player.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to indicate whether an error should be signalled if the event
;;;   doesn't exist. 
;;; 
;;; RETURN VALUE  
;;; An event object.
;;; 
;;; EXAMPLE
#|
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((vn (violin :midi-channel 1))
                     (vc (cello :midi-channel 2))))
        :tempo-map '((1 (q 60)))
        :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
        :set-map '((1 (1 1 1)))
        :rthm-seq-palette '((1 ((((2 4) (e) e+e. 32 (32)))
                                :pitch-seq-palette (((1) 2)))))
        :rthm-seq-map '((1 ((vn (1 1 1))
                            (vc (1 1 1))))))))
  (get-event mini 2 4 'vn))

=> 
EVENT: start-time: 3.750, end-time: 3.875, 
duration-in-tempo: 0.125, 
compound-duration-in-tempo: 0.125, 
amplitude: 0.700 
bar-num: 2, marks-before: NIL, 
tempo-change: NIL 
instrument-change: NIL 
display-tempo: NIL, start-time-qtrs: 3.750, 
midi-time-sig: NIL, midi-program-changes: NIL, 
8va: 0
pitch-or-chord: 
PITCH: frequency: 293.665, midi-note: 62, midi-channel: 1 
pitch-bend: 0.0 
degree: 124, data-consistent: T, white-note: D4
nearest-chromatic: D4
src: 1.122462, src-ref-pitch: C4, score-note: D4 
qtr-sharp: NIL, qtr-flat: NIL, qtr-tone: NIL,  
micro-tone: NIL, 
sharp: NIL, flat: NIL, natural: T, 
octave: 4, c5ths: 0, no-8ve: D, no-8ve-no-acc: D
show-accidental: T, white-degree: 29, 
accidental: N, 
accidental-in-parentheses: NIL, marks: NIL
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: D4, tag: NIL, 
data: D4
**************

written-pitch-or-chord: NIL
RHYTHM: value: 32.000, duration: 0.125, rq: 1/8, is-rest: NIL, 
score-rthm: 32.0, undotted-value: 32, num-flags: 3, num-dots: 0, 
is-tied-to: NIL, is-tied-from: NIL, compound-duration: 0.125, 
is-grace-note: NIL, needs-new-note: T, beam: NIL, bracket: NIL, 
rqq-note: NIL, rqq-info: NIL, marks: NIL, marks-in-part: NIL, 
letter-value: 32, tuplet-scaler: 1, grace-note-duration: 0.05
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: 32, tag: NIL, 
data: 32

|#
;;; 
;;; SYNOPSIS
(defmethod get-event ((sc slippery-chicken) bar-num event-num player
                      &optional (error t))
;;; ****
  (get-event (piece sc) bar-num event-num player error))
;;; ****

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod tie-all-last-notes-over-rests-aux ((sc slippery-chicken)
                                  start-bar end-bar player
                                  &key (to-next-attack t)
                                  (tie-next-attack nil)
                                  ;; what rhythm to tie to (on end-bar only)
                                  (last-rhythm nil)
                                  (auto-beam nil))
  (let* ((active-bars (loop
                         for bnum from start-bar to (1- end-bar)
                         for bar = (get-bar (piece sc) bnum player)
                         unless (is-rest-bar bar)
                         collect bnum))
         (last (first (last active-bars))))
    (loop for bnum in active-bars do
         (tie-over-rest-bars-aux sc bnum player 
                                 :end-bar end-bar
                                 :tie-next-attack tie-next-attack
                                 :to-next-attack to-next-attack
                                 :auto-beam auto-beam
                                 :last-rhythm (when (= bnum last)
                                                last-rhythm)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bar-num is the bar that we start the ties from the last note, not the first
;; rest bar. 
(defmethod tie-over-rest-bars-aux ((sc slippery-chicken) bar-num player
                                   &key (end-bar nil) 
                                     (to-next-attack t)
                                     (tie-next-attack nil)
                                     (last-rhythm nil)
                                     (auto-beam nil))
  ;; MDE Thu Apr 26 16:39:58 2012 -- end-bar default was 99999 which is fine ;
  ;; for the loops but not the get-bar call below ;
  (unless end-bar
    (setf end-bar (num-bars sc)))
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
    (unless (zerop (notes-needed bar))
      (unless porc
        (error "slippery-chicken::tie-over-rest-bars-aux: can't tie from last ~
              event of bar ~a" bar-num))
      ;; remove any staccato or tenuto marks from this event ;
      (rm-marks start-event '(s t as) nil)
      (when porc
        (delete-marks porc))
      (when wporc
        (delete-marks wporc))
      ;; replace accent-staccato with just accent ;
      (replace-mark start-event 'as 'a)
      ;; our last event may have been tied to following events... ;
      (loop 
         for i from start-event-pos
         while (is-tied-from start-event)
         do
           (setf start-event (get-nth-event (1+ i) bar)))
      (setf (is-tied-from start-event) t)
      (no-accidental porc)
      (when wporc
        (no-accidental wporc))
      (flet ((do-it (e)
               (setf (pitch-or-chord e) (clone porc)
                     (written-pitch-or-chord e) (when wporc 
                                                  (clone wporc))
                     (is-rest e) nil
                     (needs-new-note e) nil
                     (is-tied-to e) t
                     (is-tied-from e) t
                     last-event e)))
        ;; first the events in the current bar ;
        (loop 
           for i downfrom (1- (num-rhythms bar)) to 0
           for e = (nth i (rhythms bar))
           while (is-rest e)
           do
             (do-it e))
        ;; now the events in the next bars ;
        (loop 
           for bnum from (1+ bar-num) 
           for bar = (get-bar (piece sc) bnum player)
           while (and bar (<= (bar-num bar) end-bar))
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
                   ;; (loop for e in (rhythms bar) while (is-rest e) do ;
                     (loop for e in (rhythms bar) do
                          (if (is-rest e) 
                              (do-it e)
                              (progn
                                (when (and tie-next-attack
                                           (porc-equal e last-event))
                                ;; we've hit the next attack after the ties ;
                                ;; we created but if it's the same ;
                                ;; pitch/chord, tie to this too ;
                                  (setf last-event nil
                                      ;; so we don't kill the tie ;
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
          ;; we might have tied over several notes to the next attack so now ;
          ;; we've got to make them rests ;
          (loop 
             for e in (rest (rhythms last-bar)) 
             while (is-tied-to e)
             do
               (force-rest e)))
        (loop 
           for bnum from bar-num
           for bar = (get-bar (piece sc) bnum player)
           while (and bar (<= (bar-num bar) end-bar))
           do
             (consolidate-notes bar nil auto-beam)
           ;; MDE Sat Jun 28 14:59:24 2014 -- we might have events without ;
           ;; player slots              ;
             (update-events-player bar player)
           ;; MDE Wed Apr 25 16:33:29 2012 -- when clause added! ;
           ;; MDE Sat Jun  9 16:02:47 2012 -- when changed to if so we can at ;
           ;; least check the beams (and call auto-beam no matter what if ;
           ;; there's a problem)        ;
             (if auto-beam
                 (auto-beam bar auto-beam nil)
                 (check-beams bar :auto-beam t)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE original comment:
;;; N.B. Instruments cannot be changed mid-sequence and sequence is 1-based so
;;; we have to 1+ elsewhere if necessary

;;; ****m* slippery-chicken/get-current-instrument-for-player
;;; DESCRIPTION
;;; Get the currently active instrument for a given player in a specified
;;; sequence of a slippery-chicken object, as defined in the
;;; instrument-change-map.
;;; 
;;; ARGUMENTS
;;; - The ID of the section from which to retrieve the current instrument for
;;;   the specified player. This can also be a reference, e.g. in the form 
;;;   '(2 1).
;;; - The ID of the player for whom the current instrument is sought. 
;;; - The number of the sequence from which to retrieve the current
;;;   instrument. This is a 1-based number. A slippery-chicken object.
;;;
;;; RETURN VALUE
;;; An instrument object.
;;; 
;;; EXAMPLE
#|
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((sax ((alto-sax tenor-sax) :midi-channel 1))
                     (db (double-bass :midi-channel 2))))
        :instrument-change-map '((1 ((sax ((1 alto-sax) (3 tenor-sax)))))
                                 (2 ((sax ((2 alto-sax) (5 tenor-sax))))))
        :set-palette '((1 ((c2 d2 g2 a2 e3 fs3 b3 cs4 fs4 gs4 ds5 f5 bf5)))) 
        :set-map '((1 (1 1 1 1 1))
                   (2 (1 1 1 1 1)))
        :rthm-seq-palette '((1 ((((4 4) h q e s s))
                                :pitch-seq-palette ((1 2 3 4 5)))))
        :rthm-seq-map '((1 ((sax (1 1 1 1 1))
                            (db (1 1 1 1 1))))
                        (2 ((sax (1 1 1 1 1))
                            (db (1 1 1 1 1))))))))
  (get-current-instrument-for-player 2 'sax 3 mini))

=> 
INSTRUMENT: lowest-written: BF3, highest-written: FS6
lowest-sounding: CS3, highest-sounding: A5
starting-clef: TREBLE, clefs: (TREBLE), clefs-in-c: (TREBLE)
prefers-notes: NIL, midi-program: 66
transposition: EF, transposition-semitones: -9
score-write-in-c: NIL, score-write-bar-line: NIL
chords: NIL, chord-function: NIL, 
total-bars: 5 total-notes: 25, total-duration: 20.000
total-degrees: 2920, microtones: T
missing-notes: (BQF3 BQF4), subset-id: NIL
staff-name: alto saxophone, staff-short-name: alt sax,
                  
largest-fast-leap: 999, tessitura: BQF3
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: ALTO-SAX, tag: NIL, 
data: NIL

|#
;;; SYNOPSIS
(defmethod get-current-instrument-for-player (section player sequence
                                              (sc slippery-chicken))
;;; ****
  (unless (listp section)
    (setf section (list section)))
  (let* ((player-obj (get-data player (ensemble sc)))
         ins-obj)
    (unless player-obj
      (error "slippery-chicken::get-current-instrument-for-player: ~
              Can't get player object ~%for ~a" player))
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

;;; SAR Wed May  9 21:49:35 BST 2012: Added robodoc entry

;;; 9.2.11 do the above but for a bar number instead

;;; ****m* slippery-chicken/get-instrument-for-player-at-bar
;;; DATE
;;; 09-Feb-2011
;;;
;;; DESCRIPTION
;;; Get the current instrument for a specified player at a specified bar number
;;; in a slippery-chicken object, as defined in the instrument-change-map.
;;; 
;;; ARGUMENTS
;;; - The ID of a player in the slippery-chicken object.
;;; - An integer that is the number of the bar from which to get the current
;;;   instrument.
;;; - A slippery-chicken object.
;;; 
;;; RETURN VALUE
;;; An instrument object.
;;; 
;;; EXAMPLE
#|
(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((sax ((alto-sax tenor-sax) :midi-channel 1))
                      (db (double-bass :midi-channel 2))))
         :instrument-change-map '((1 ((sax ((1 alto-sax) (3 tenor-sax)))))
                                  (2 ((sax ((2 alto-sax) (5 tenor-sax))))))
         :set-palette '((1 ((c2 d2 g2 a2 e3 fs3 b3 cs4 fs4 gs4 ds5 f5 bf5)))) 
         :set-map '((1 (1 1 1 1 1))
                    (2 (1 1 1 1 1)))
         :rthm-seq-palette '((1 ((((4 4) h q e s s))
                                 :pitch-seq-palette ((1 2 3 4 5)))))
         :rthm-seq-map '((1 ((sax (1 1 1 1 1))
                             (db (1 1 1 1 1))))
                         (2 ((sax (1 1 1 1 1))
                             (db (1 1 1 1 1))))))))
  (get-instrument-for-player-at-bar 'sax 3 mini))
=> 
INSTRUMENT: lowest-written: BF3, highest-written: FS6
lowest-sounding: AF2, highest-sounding: E5
starting-clef: TREBLE, clefs: (TREBLE), clefs-in-c: (BASS TREBLE)
prefers-notes: NIL, midi-program: 67
transposition: BF, transposition-semitones: -14
score-write-in-c: NIL, score-write-bar-line: NIL
chords: NIL, chord-function: NIL, 
total-bars: 5 total-notes: 25, total-duration: 20.000
total-degrees: 2710, microtones: T
missing-notes: (FQS3 FQS4), subset-id: NIL
staff-name: tenor sax, staff-short-name: ten sax,
                  
largest-fast-leap: 999, tessitura: FS3
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: TENOR-SAX, tag: NIL, 
data: NIL
|#
;;; SYNOPSIS
(defmethod get-instrument-for-player-at-bar (player bar (sc slippery-chicken))
;;; ****
  (let* ((bar (if (rthm-seq-bar-p bar) bar (get-bar sc bar player)))
         (section
          (if bar
              (butlast (player-section-ref bar))
              (error "slippery-chicken::get-instrument-for-player-at-bar: ~%~
                      couldn't get bar.")))
         (seq-num (1+ (nth-seq bar))))
    (get-current-instrument-for-player section player seq-num sc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Wed May  9 21:59:36 BST 2012: Added robodoc entry

;;; ****m* slippery-chicken/get-transposition-at-bar
;;; DATE
;;; 24-Mar-2011
;;;
;;; DESCRIPTION
;;; Return the number of semitones difference between the sounding pitches and
;;; written pitches of a given player's part in a specified bar within a
;;; slippery-chicken object; e.g. bass clarinet = -14.
;;; 
;;; ARGUMENTS
;;; - The ID of the player for whom the transposition value is sought.
;;; - An integer which is the number of the bar for which the transposition
;;;   value is sought.
;;; - A slippery-chicken object.
;;; 
;;; RETURN VALUE
;;; An integer.
;;; 
;;; EXAMPLE
#|
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((sax (alto-sax :midi-channel 1))))
        :set-palette '((1 ((e3 fs3 b3 cs4 fs4 gs4 ds5 f5)))) 
        :set-map '((1 (1 1 1)))
        :rthm-seq-palette '((1 ((((4 4) h q e s s))
                                :pitch-seq-palette ((1 2 3 4 5)))))
        :rthm-seq-map '((1 ((sax (1 1 1))))))))
  (get-transposition-at-bar 'sax 2 mini))

=> -9
|#
;;; SYNOPSIS
(defmethod get-transposition-at-bar (player bar (sc slippery-chicken))
;;; ****
  (transposition-semitones (get-instrument-for-player-at-bar player bar sc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken/num-seqs
;;; DESCRIPTION
;;; Return the number of sequences (which may contain multiple bars) in a
;;; specified section of a slippery-chicken object. 
;;;
;;; NB This will return the total number of seqs if there are sub-sections.
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; - The ID of the section for which to return the number of sequences.
;;; 
;;; RETURN VALUE
;;; An integer.
;;; 
;;; EXAMPLE
#|
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((sax (alto-sax :midi-channel 1))))
        :set-palette '((1 ((e3 fs3 b3 cs4 fs4 gs4 ds5 f5)))) 
        :set-map '((1 (1 1 1 1))
                   (2 (1 1 1))
                   (3 (1 1 1 1 1)))
        :rthm-seq-palette '((1 ((((4 4) h q e s s))
                                :pitch-seq-palette ((1 2 3 4 5)))))
        :rthm-seq-map '((1 ((sax (1 1 1 1))))
                        (2 ((sax (1 1 1))))
                        (3 ((sax (1 1 1 1 1))))))))
  (num-seqs mini 2))

=> 3

|#
;;; SYNOPSIS
(defmethod num-seqs ((sc slippery-chicken) section-ref)
;;; ****
  ;; MDE Wed Jun 27 15:51:14 2012 
  (let ((section (get-section sc section-ref)))
    (unless section
      (error "slippery-chicken::num-seqs: Section with ID ~a doesn't exist."
             section-ref))
    ;; MDE Sat Jun  2 12:13:01 2012 -- if there are subsections tot them up
    (if (has-subsections section)
        (loop for ref in (get-section-refs sc section-ref 1)
           for nums = (num-seqs sc ref)
           do
           (unless nums
             (error "slippery-chicken::num-seqs: error getting number of ~
                     sequences for reference ~a" ref))
           sum nums)
        (let ((sec (get-data (econs 
                              (if (listp section-ref) 
                                  section-ref 
                                  (list section-ref)) 
                              (first (players (piece sc))))
                             (piece sc) 
                             nil)))
          (when sec
            (sclist-length sec))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken/num-players
;;; DESCRIPTION
;;; Get the number of players in a slippery chicken object's ensemble object.
;;; 
;;; ARGUMENTS
;;; - A slippery chicken object
;;; 
;;; RETURN VALUE
;;; - An integer.
;;; 
;;; SYNOPSIS
(defmethod num-players ((sc slippery-chicken))
;;; ****
  (num-players (ensemble sc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Mon May  7 09:32:10 2012 -- NB num-sections refers to the number of
;;; top-level sections so if any section has subsections they'll all be slurped
;;; up and only count as 1.

;;; ****m* slippery-chicken/get-section-refs
;;; DATE
;;; 07-May-2012
;;;
;;; DESCRIPTION
;;; Return the reference IDs for all section and subsections of a given
;;; slippery-chicken object.
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; - An integer that is the first top-level section for which to return the
;;;   reference IDs. As this number refers to the number of top-level sections
;;;   only, any subsections will be contained in these and only count as 1.
;;; - An integer that is the number of consecutive sections to return section
;;;   reference IDs.
;;; 
;;; RETURN VALUE
;;; A list of lists containing the section reference IDs of the specified
;;; range in the slippery-chicken object.
;;; 
;;; EXAMPLE
;;;
#|
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((sax (alto-sax :midi-channel 1))))
        :set-palette '((1 ((e3 fs3 b3 cs4 fs4 gs4 ds5 f5)))) 
        :set-map '((1 (1 1 1))
                   (2 (1 1 1))
                   (3 ((a (1 1 1))
                       (b ((x (1 1 1))
                           (y (1 1 1))))))
                   (4 ((a (1 1 1))
                       (b (1 1 1))
                       (c (1 1 1 1))))
                   (5 (1 1 1))
                   (6 (1 1 1))
                   (7 (1 1 1)))
        :rthm-seq-palette '((1 ((((4 4) h q e s s))
                                :pitch-seq-palette ((1 2 3 4 5)))))
        :rthm-seq-map '((1 ((sax (1 1 1))))
                        (2 ((sax (1 1 1))))
                        (3 ((a ((sax (1 1 1))))
                            (b ((x ((sax (1 1 1))))
                                (y ((sax (1 1 1))))))))
                        (4 ((a ((sax (1 1 1))))
                            (b ((sax (1 1 1))))
                            (c ((sax (1 1 1 1))))))
                        (5 ((sax (1 1 1))))
                        (6 ((sax (1 1 1))))
                        (7 ((sax (1 1 1))))))))
  (get-section-refs mini 2 4))

=> ((2) (3 A) (3 B X) (3 B Y) (4 A) (4 B) (4 C) (5))

|#
;;;
;;; SYNOPSIS
(defmethod get-section-refs ((sc slippery-chicken) start-section num-sections)
;;; ****
  (unless (and (integerp start-section) (integerp num-sections))
    (error "slippery-chicken::get-section-refs: start-section (~a) and ~
            num-sections (~a) must both be integers."
           start-section num-sections))
  (relink-named-objects (piece sc))
  (labels ((do-subsection (ss)
             (loop for no in (data ss) 
                if (is-ral (data no))
                ;; if (section-p (data no))
                append (do-subsection (data no))
                else collect (butlast (this no)))))
    ;; MDE Fri Jan 10 15:27:44 2020 -- we used to look at the set-map but since
    ;; we can now use bars-to-sc we'll have to look at the piece. We
    ;; remove-duplicates because we'd get a section ref for each of the players
    ;; playing in a section and this will probably create duplicates (though not
    ;; necessarily) 
    (remove-duplicates
     (loop with nd = (+ start-section num-sections)
       for sn from 1
       ;; for section in (data (set-map sc))
       for section in (data (piece sc))
       when (and (>= sn start-section) (< sn nd))
       collect section into sections
       finally
         (return
           (loop for s in sections
              if (is-ral (data s))
              append (do-subsection (data s))
              else collect (this s))))
     :test #'equalp)))

#|
;;; MDE Mon May  7 09:31:17 2012 -- this is the old version
;;; The linking of the rthm-seq-map (and hence piece) slot only works at the
;;; instrument level so we don't get a pointer to the next section, rather,
;;; only when we ask for instrument data do we get the points. E.g. (get-data 3 
;;; (piece sc)) will have previous, this, and next slots all NIL, whereas
;;; (get-data '(3 some-instrument) (piece sc)) will return a player-section
;;; where the previous, this, and next slots are good (this is all as it should
;;; be!). In order to get the references of a number of contiguous sections
;;; then, we'll have to use instrument references.
(defmethod get-section-refs ((sc slippery-chicken) start-section num-sections)
  (let* ((last-player (first (last (players (ensemble sc)))))
         (section-list (if (listp start-section) 
                           start-section
                           (list start-section)))
         (ref section-list))
    (unless num-sections
      (setf num-sections (get-num-sections sc)))
    (loop with player-ref with data
       repeat num-sections
       ;; MDE Mon Apr 16 21:36:44 2012 -- do this only while we can get a ref ;
       ;; because if we start beyond section 1 but don't give num-sections ;
       ;; we'll be in trouble otherwise. ;
while ref
collect ref
do 
(setf player-ref (econs ref last-player)
      data (get-data player-ref (piece sc))
      ref (when data (butlast (next data)))))))

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Wed May  9 22:25:55 BST 2012: Conformed robodoc entry

;;; ****m* slippery-chicken/get-num-sections
;;; DESCRIPTION
;;; Return the number of sections in the given slippery-chicken object, as
;;; defined in e.g. in the set-map. N.B. If the object has subsections these
;;; are counted also, unless the optional argument is NIL.
;;; 
;;; ARGUMENTS 
;;; - A slippery-chicken object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - count-subsections: if T (default) count all sections and subsections. If
;;; NIL just the top-level sections.
;;; 
;;; RETURN VALUE  
;;; An integer that is the number of section.
;;;
;;; EXAMPLE
#|
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((sax (alto-sax :midi-channel 1))))
        :set-palette '((1 ((e3 fs3 b3 cs4 fs4 gs4 ds5 f5)))) 
        :set-map '((1 ((a (1 1 1 1))
                       (b (1 1 1 1))))
                   (2 (1 1 1))
                   (3 (1 1 1 1 1)))
        :rthm-seq-palette '((1 ((((4 4) h q e s s))
                                :pitch-seq-palette ((1 2 3 4 5)))))
        :rthm-seq-map '((1 ((a ((sax (1 1 1 1))))
                            (b ((sax (1 1 1 1))))))
                        (2 ((sax (1 1 1))))
                        (3 ((sax (1 1 1 1 1))))))))
  (get-num-sections mini))

=> 4

|#
;;; 
;;; SYNOPSIS
(defmethod get-num-sections ((sc slippery-chicken)
                             &optional (count-subsections t))
;;; ****
  ;; MDE Fri Jan 10 15:01:21 2020 -- when we create an sc object via bars-to-sc
  ;; we do have maps and palettes butthe number of sections in the piece is
  ;; more accurate 
  (let ((piece-sections (sclist-length (piece sc)))
        (map-sections
         (if count-subsections
             (num-data (set-map sc))
             ;; MDE Thu Jun 15 14:54:24 2017 
             (sclist-length (rthm-seq-map sc)))))
    (max piece-sections map-sections)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Thu May 10 11:31:08 BST 2012: Added robodoc entry

;;; ****m* slippery-chicken/get-all-section-refs
;;; DESCRIPTION
;;; Return all section IDs as a list of lists. Subsection IDs will be contained 
;;; in the same sublists as their enclosing sections.
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; 
;;; RETURN VALUE
;;; A list of lists.
;;; 
;;; EXAMPLE
#|
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((sax (alto-sax :midi-channel 1))))
        :set-palette '((1 ((e3 fs3 b3 cs4 fs4 gs4 ds5 f5)))) 
        :set-map '((1 (1 1 1))
                   (2 (1 1 1))
                   (3 ((a (1 1 1))
                       (b ((x (1 1 1))
                           (y (1 1 1))))))
                   (4 ((a (1 1 1))
                       (b (1 1 1))
                       (c (1 1 1 1))))
                   (5 (1 1 1))
                   (6 (1 1 1))
                   (7 (1 1 1)))
        :rthm-seq-palette '((1 ((((4 4) h q e s s))
                                :pitch-seq-palette ((1 2 3 4 5)))))
        :rthm-seq-map '((1 ((sax (1 1 1))))
                        (2 ((sax (1 1 1))))
                        (3 ((a ((sax (1 1 1))))
                            (b ((x ((sax (1 1 1))))
                                (y ((sax (1 1 1))))))))
                        (4 ((a ((sax (1 1 1))))
                            (b ((sax (1 1 1))))
                            (c ((sax (1 1 1 1))))))
                        (5 ((sax (1 1 1))))
                        (6 ((sax (1 1 1))))
                        (7 ((sax (1 1 1))))))))
  (get-all-section-refs mini))

=> ((1) (2) (3 A) (3 B X) (3 B Y) (4 A) (4 B) (4 C) (5) (6) (7))

|#
;;; SYNOPSIS
(defmethod get-all-section-refs ((sc slippery-chicken))
;;; ****
  (get-all-refs (set-map sc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Thu May 10 11:40:44 BST 2012: Added robodoc entry

;;; ****m* slippery-chicken/statistics
;;; DESCRIPTION
;;; Print various information about the given slippery-chicken object to the
;;; Lisp listener or other specified stream.
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - NIL or a stream to which the information should be printed. If NIL, the
;;;   method will not print the information to any stream. T = print to the
;;;   Lisp listener. Default = T.
;;; 
;;; RETURN VALUE
;;; A number of formatted statistics about the given slippery-chicken object. 
;;; 
;;; EXAMPLE
#|
(let ((mini
(make-slippery-chicken
'+mini+
:ensemble '(((sax (alto-sax :midi-channel 1))))
:set-palette '((1 ((e3 fs3 b3 cs4 fs4 gs4 ds5 f5)))) 
:set-map '((1 (1 1 1))
(2 (1 1 1)))
:rthm-seq-palette '((1 ((((4 4) h q e s s))
:pitch-seq-palette ((1 2 3 4 5)))))
:rthm-seq-map '((1 ((sax (1 1 1))))
(2 ((sax (1 1 1))))))))
(statistics mini))

=>
+MINI+ 
"+MINI+-piece" 
start-bar: 1
end-bar: 6
num-bars: 6
start-time: 0.0
end-time: 24.0
start-time-qtrs: 0
end-time-qtrs: 24.0
num-notes (attacked notes, not tied): 30
num-score-notes (tied notes counted separately): 30 
num-rests: 0
duration-qtrs: 24.0 
duration: 24.0 (24.000)

|#
;;; SYNOPSIS
(defmethod statistics ((sc slippery-chicken) &optional (stream t))
;;; ****
  (statistics (piece sc) stream))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod check-instrument-doublings ((sc slippery-chicken))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken/get-tempo
;;; DESCRIPTION
;;; Return the tempo object in effect for a specified bar of a given
;;; slippery-chicken object.
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; - An integer that is the number of a bar within that slippery-chicken
;;;   object. 
;;; 
;;; RETURN VALUE
;;; A tempo object.
;;; 
;;; EXAMPLE
#|
(let ((mini
(make-slippery-chicken
'+mini+
:ensemble '(((sax (alto-sax :midi-channel 1))))
:tempo-map '((1 (q 60)) (5 (e 72)) (7 (q. 176 "prestissimo")))
:set-palette '((1 ((e3 fs3 b3 cs4 fs4 gs4 ds5 f5)))) 
:set-map '((1 (1 1 1 1 1 1 1 1)))
:rthm-seq-palette '((1 ((((4 4) h q e s s))
:pitch-seq-palette ((1 2 3 4 5)))))
:rthm-seq-map '((1 ((sax (1 1 1 1 1 1 1 1))))))))
(get-tempo mini 6))

=> 
TEMPO: bpm: 72, beat: E, beat-value: 8.0, qtr-dur: 1.6666666 
qtr-bpm: 36.0, usecs: 1666666, description: NIL
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: NIL, tag: NIL, 
data: 72

|#
;;; SYNOPSIS
(defmethod get-tempo ((sc slippery-chicken) bar-num)
;;; ****
  (data (scm-get-data bar-num (tempo-map sc))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Thu May 10 12:03:15 BST 2012: Added robodoc entry

;;; MDE original comment:
;;; bar-num is actually required but optional because of the rthm-seq-bar
;;; method of the same name.

;;; ****m* slippery-chicken/get-time-sig
;;; DESCRIPTION
;;; Get the time-sig object associate with a specified bar number in a given
;;; slippery-chicken object.
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; - An integer that is the number of the bar for which to the time-sig object
;;;   is to be returned. NB: Although this argument is listed as optional in
;;;   the method definition (due to inheritance), it is actually required.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - (see above).
;;; 
;;; RETURN VALUE
;;; A time-sig object.
;;; 
;;; EXAMPLE
#|
(let ((mini
(make-slippery-chicken
'+mini+
:ensemble '(((sax (alto-sax :midi-channel 1))))
:set-palette '((1 ((e3 fs3 b3 cs4 fs4 gs4 ds5 f5)))) 
:set-map '((1 (1 1 1)))
:rthm-seq-palette '((1 ((((4 4) h q e s s)
((5 8) q e s s e)
((3 16) s e))
:pitch-seq-palette ((1 2 3 4 5 1 2 3 4 5 1
2))))) 
:rthm-seq-map '((1 ((sax (1 1 1))))))))
(get-time-sig mini 2))

=> 
TIME-SIG: num: 5, denom: 8, duration: 2.5, compound: NIL, midi-clocks: 24, num-beats: 5
SCLIST: sclist-length: 2, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: "0508", tag: NIL, 
data: (5 8)

|#
;;; SYNOPSIS
(defmethod get-time-sig ((sc slippery-chicken) &optional bar-num)
;;; ****
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
                ;; (print (member player display-players))
              (setf (display-tempo first-event) 
                    (when (member player display-players)
                      t))
              ;; (format t "~&tempo: ~a player: ~a" 
              ;;     (tempo-change first-event) player)
              )
           (setf last-tempo current-tempo)))
    t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Mon Apr 25 16:32:39 2016 -- added optional arg: can be T or an int
(defmethod handle-set-limits ((sc slippery-chicken) &optional num-bars force)
  ;; don't do this twice as degree y values might be reprocessed as midi,
  ;; causing errors
  (when (or force (not (did-init sc)))
    (flet ((do-limits (set-limits x-max)
             ;; MDE Mon Apr 25 16:49:45 2016 -- have to handle the case where
             ;; we've already made an assoc-list
             (typecase set-limits
               (list
                (make-assoc-list 
                 'set-limits
                 (loop for ins in set-limits
                    collect
                      (list (first ins) 
                            (doctor-env (second ins) x-max)))))
               (assoc-list
                (loop for ins in (data set-limits) do
                     (setf (data ins)
                           (doctor-env (data ins) x-max)))
                set-limits)
               (t (error "slippery-chicken::handle-set-limits: ~
                          unhandled data type: ~a" set-limits)))))
      (let* ((num-x (if num-bars
                        (if (integer>0 num-bars) num-bars (num-bars sc))
                        (num-sequences sc)))
             (high (do-limits (set-limits-high sc) num-x))
             (low (do-limits (set-limits-low sc) num-x)))
        (setf (set-limits-high sc) high
              (set-limits-low sc) low)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Thu Sep  6 13:48:07 2018 
(defmethod handle-transposition-curve ((sc slippery-chicken) &optional num-bars)
  (let* ((num-x (if num-bars
                    (if (integer>0 num-bars) num-bars (num-bars sc))
                    (num-sequences sc))))
    (when (> num-x 1)
      (setf (transposition-curve sc)
            (doctor-env (transposition-curve sc) num-x nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; global-seq-num is 1-based.

(defmethod get-set-limits ((sc slippery-chicken) player x)
  (let* ((limit-ins-high
          (get-set-limit-high sc player x))
         (limit-ins-low 
          (get-set-limit-low sc player x))
         ;; 10/3/07: got to take the global limits into account if an entry for
         ;; 'all was made
         (global-limit-high
          (get-set-limit-high sc 'all x))
         (global-limit-low
          (get-set-limit-low sc 'all x))
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
;;; DESCRIPTION
;;; Modify the pitches of each part in a slippery-chicken object to avoid large
;;; melodic leaps at fast speeds, based on the largest-fast-leap slot of the
;;; given instrument object and the fast-leap-threshold slot of the
;;; slippery-chicken object. 
;;;
;;; This method is called automatically at init and as such will most likely
;;; seldom need to be directly accessed by the user.
;;; 
;;; ARGUMENTS 
;;; - A slippery-chicken object.
;;;
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :threshold. A number that is the maximum duration in seconds between two
;;;   consecutive notes in the slippery-chicken object for which linear
;;;   intervals greater than the number specified in the given instrument
;;;   object's largest-fast-leap slot will be allowed. This value is taken from
;;;   the fast-leap-threshold slot of the given slippery-chicken object by
;;;   default. 
;;; - :verbose. T or NIL to indicate whether to print feedback about the
;;;   method's operations to the Lisp listener. T = print. Default = T.
;;; 
;;; RETURN VALUE  
;;; Always T
;;;
;;; EXAMPLE
#|
(let ((mini
(make-slippery-chicken
'+mini+
:ensemble '(((vn (violin :midi-channel 1))))
:tempo-map '((1 (q 96)))
:set-palette '((1 ((g3 a5 b6))))
:set-map '((1 (1 1 1 1 1 1)))
:rthm-seq-palette '((1 ((((2 4) e s 32 64 64 e s 32 64 64))
:pitch-seq-palette ((1 5 1 5 1 5 1 5 1 5))))) 
:rthm-seq-map '((1 ((vn (1 1 1 1 1 1))))))))
(shorten-large-fast-leaps mini :threshold 0.25))

=>
******* section (1)
Getting notes for VN
Shortening short, fast leaps...
Shortened 23 large fast leaps
seq-num 0, VN, replacing B6 with G3
seq-num 0, VN, replacing B6 with G3
seq-num 0, VN, replacing B6 with G3
seq-num 0, VN, replacing B6 with G3
seq-num 0, VN, replacing G3 with B6
seq-num 0, VN, replacing G3 with B6
seq-num 0, VN, replacing G3 with B6
seq-num 1, VN, replacing G3 with B6
seq-num 1, VN, replacing B6 with G3
seq-num 1, VN, replacing B6 with G3
seq-num 1, VN, replacing B6 with G3
seq-num 1, VN, replacing B6 with G3
seq-num 1, VN, replacing G3 with B6
seq-num 1, VN, replacing G3 with B6
seq-num 1, VN, replacing G3 with B6
seq-num 2, VN, replacing G3 with B6
seq-num 2, VN, replacing B6 with G3
seq-num 2, VN, replacing B6 with G3
seq-num 2, VN, replacing B6 with G3
seq-num 2, VN, replacing B6 with G3
seq-num 2, VN, replacing G3 with B6
seq-num 2, VN, replacing G3 with B6
seq-num 2, VN, replacing G3 with B6
seq-num 3, VN, replacing G3 with B6
seq-num 3, VN, replacing B6 with G3
seq-num 3, VN, replacing B6 with G3
seq-num 3, VN, replacing B6 with G3
seq-num 3, VN, replacing B6 with G3
seq-num 3, VN, replacing G3 with B6
seq-num 3, VN, replacing G3 with B6
seq-num 3, VN, replacing G3 with B6
seq-num 4, VN, replacing G3 with B6
seq-num 4, VN, replacing B6 with G3
seq-num 4, VN, replacing B6 with G3
seq-num 4, VN, replacing B6 with G3
seq-num 4, VN, replacing B6 with G3
seq-num 4, VN, replacing G3 with B6
seq-num 4, VN, replacing G3 with B6
seq-num 4, VN, replacing G3 with B6
seq-num 5, VN, replacing G3 with B6
seq-num 5, VN, replacing B6 with G3
seq-num 5, VN, replacing B6 with G3
seq-num 5, VN, replacing B6 with G3
seq-num 5, VN, replacing B6 with G3
seq-num 5, VN, replacing G3 with B6
seq-num 5, VN, replacing G3 with B6
seq-num 5, VN, replacing G3 with B6

|#
;;; 
;;; SYNOPSIS
(defmethod shorten-large-fast-leaps ((sc slippery-chicken) 
                                     &key threshold (verbose t))
;;; ****
  ;; 24.3.11 get threshold from class if not given
  (unless threshold (setq threshold (fast-leap-threshold sc)))
  (loop 
    for player in (get-players (ensemble sc))
    with global-seq-num 
    with count = 0
    do
       (setq global-seq-num 1)
       (loop for section in (get-all-section-refs sc) do
         (loop 
           for seq-num from 0
           ;; MDE Wed Feb  1 14:04:45 2012 -- don't create-rest-seq
           for seq = (get-nth-sequenz (piece sc) section player seq-num nil)
           for set = (get-nth-from-palette section seq-num (set-map sc))
           ;; MDE Fri Sep  7 10:36:46 2018
           for set-transp = (interpolate global-seq-num
                                         ;; MDE Wed Sep 12 10:15:58 2018 --
                                         ;; no need to warn when we run
                                         ;; 1-seq over: it'll be picked up
                                         ;; below  
                                         (transposition-curve sc) :warn nil)
           for first-bar-num = (when seq
                                 (bar-num (first (bars seq))))
           for ins = (get-current-instrument-for-player
                      section player (1+ seq-num) sc)
           for transp = (- (transposition-semitones ins))
           for lfl = (largest-fast-leap ins)
           with qnis with last-seq
           while seq do
             (when verbose            ; MDE Wed May 21 11:58:44 2025, Heidhausen
               (format t "~&Processing seq beginning at bar ~a for ~a (~a)~%  ~
                         (largest fast leap: ~a)"
                       first-bar-num player (id ins) lfl))
             (if set                    ; MDE Fri Sep  7 10:37:50 2018
               (progn
                 (setq set (clone set))
                 (unless (zerop set-transp)
                   (setq set (transpose set set-transp
                                        :destructively t))))
               (error "slippery-chicken::shorten-large-fast-leaps: ~
                       no set! ~%section = ~a seq-num = ~a"
                      section seq-num))
             (setf qnis (get-quick-notes-indices seq last-seq threshold))
             (when (zerop transp)
               (setf transp nil))
             (when verbose            ; MDE Wed May 21 11:58:44 2025, Heidhausen
               (format t " quick-notes-indices: ~a" qnis))
             (when (and qnis lfl)
               (loop 
                 with limits = (get-set-limits sc player 
                                               global-seq-num)
                 with pitches = (limit-for-instrument 
                                 ;; (clone set) ins
                                 ;; MDE Fri Sep  7 10:41:07 2018 -- no need
                                 ;; to clone now as it's cloned above
                                 set ins
                                 :upper (second limits)
                                 :lower (first limits))
                 for qni in qnis
                 ;; a zero means we got a fast note from the last note last
                 ;; seq to the first note this seq
                 for e1 = (if (zerop qni)
                            (get-last-attack last-seq)
                            (get-nth-attack (1- qni) seq))
                 for e2 = (get-nth-attack qni seq)
                 for distance = (event-distance e1 e2)
                 for up = (> distance 0)
                 with new-pitch with pos with compare
                 do
                    (when (> (abs distance) lfl)
                      ;; MDE Mon Aug 3 18:32:37 2020, Heidhausen -- warn rather
                      ;; than error (below) but also neatened code to avoid
                      ;; duplication
                      (setq compare (if up (lowest e1) (highest e1))
                            pos (position (if up (highest e2) (lowest e2))
                                          pitches :test #'pitch=)
                            new-pitch 
                            (if pos
                              (loop
                                with 1-len = (1- (length pitches))
                                with i = pos
                                for p = (nth i pitches)
                                for interval = (abs (pitch- p compare))
                                do
                                   ;; (print (data p))
                                   ;; MDE Tue Aug 4 20:58:48 2020, Heidhausen --
                                   ;; don't create fast repeated notes!
                                   ;; MDE Wed May 21 14:10:59 2025, Heidhausen
                                   ;; -- integer-between causes problems with
                                   ;; micritones! 
                                   ;; (when (integer-between interval 1 lfl)
                                   (when (number-between interval 1 lfl)
                                     ;; a side-effect here is that quick leaps
                                     ;; to chords are replaced with single
                                     ;; pitches
                                     (return p))
                                   (if up
                                     (if (zerop i) (return) (decf i))
                                     (if (= i 1-len) (return) (incf i))))
                              (when (get-sc-config
                                     'shorten-large-fast-leaps-warning)
                                (warn "slippery-chicken::~
                                  shorten-large-fast-leaps: ~
                                  for ~a at bar ~a: skipping ~
                                  ~&pitch ~a: not in set ~a ~%pitches:~a"
                                      player (bar-num e1)
                                      (data (highest e2))
                                      (pitch-list-to-symbols (data set))
                                      (pitch-list-to-symbols pitches)))))
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
                                       (marks new-pitch)
                                       (marks (pitch-or-chord event))
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
                              (unless (and e nth-bar nth-event)
                                (error
                                 "~a~&slippery-chicken::shorten-large-~
                                        fast-leaps: couldn't get-nth-attack"
                                 seq))
                              (unless first-bar-num
                                (error
                                 "slippery-chicken::shorten-large-fast-~
                                        leaps: first-bar-num is NIL!"))
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
                        ;; (if new-pitch ... else:
                        (when (get-sc-config
                               'shorten-large-fast-leaps-warning)
                          (warn "~&slippery-chicken::~
                                       shorten-large-fast-leaps: ~
                                       ~%Couldn't get new pitch for ~a, ~
                                       section ~a, seq-num ~a, e1 ~a, e2 ~a! ~
                                       ~%pitches: ~a" 
                                player section (1+ seq-num)
                                (id (pitch-or-chord e1))
                                (id (pitch-or-chord e2))
                                (pitch-list-to-symbols pitches)))))))
             (setf last-seq seq)
             (incf global-seq-num)))
    finally (return count)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Mon May 14 17:10:09 BST 2012: Thu May 10 12:38:07 BST 2012: Added
;;; robodoc entry

;;; 11.4.11: event-num is 1-based. 

;;; ****m* slippery-chicken/get-clef
;;; DATE
;;; 11-Apr-2011
;;;
;;; DESCRIPTION
;;; Get the clef symbol attached to a specified event. 
;;;
;;; NB: The very first clef symbol in the very first measure of a given
;;;     player's part is determined by the corresponding instrument object and
;;;     attached to differently; as such, it cannot be retrieved using this
;;;     method. 
;;; NB: All clef symbols after the starting clef are added using the auto-clefs
;;;     method, either directly or by default in the cmn-display or
;;;     write-lp-data-for-all methods.
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; - (NB: The optional arguments are actually required.)
;;; 
;;; OPTIONAL ARGUMENTS
;;; NB: The optional arguments are actually required.
;;; - An integer that is the number of the bar from which to return the clef
;;;   symbol.
;;; - An integer that is the number of the event object within that bar from
;;;   which to retrieve the clef symbol.
;;; - The ID of the player from whose part the clef symbol is to be returned. 
;;; 
;;; RETURN VALUE
;;; A clef symbol.
;;; 
;;; EXAMPLE
#|
(let ((mini
(make-slippery-chicken
'+mini+
:ensemble '(((vc (cello :midi-channel 1))))
:tempo-map '((1 (q 96)))
:set-palette '((1 ((g2 f4 e5))))
:set-map '((1 (1 1 1)))
:rthm-seq-palette '((1 ((((5 4) e e e e e e e e e e))
:pitch-seq-palette ((1 1 2 2 2 2 3 3 3 1))))) 
:rthm-seq-map '((1 ((vc (1 1 1))))))))
(auto-clefs mini)
(get-clef mini 1 3 'vc))

=> TENOR

|#
;;; SYNOPSIS
(defmethod get-clef ((sc slippery-chicken) &optional bar-num event-num player)
;;; ****
  (let* ((bar (get-bar sc bar-num player))
         (e (when bar (get-nth-event (1- event-num) bar))))
    (when e 
      (get-clef e))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 13.4.11: start and end bar are inclusive.
;;; at the moment, we use bass/treble clefs with 8ve signs on to indicate pitch
;;; extremes (assuming an instrument has these clefs), but these can be
;;; converted to octave brackets here. NB no 15ma/mb handled here.
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
                  (add-mark e (if dt 'beg-8va 'beg-8vb)))
                ;; if we see any other clef at all we can assume the octave
                ;; +/- is at and end, but only delete the clef if we've gone
                ;; double-x to x
                (when octave-clef
                  (setf dt (eq octave-clef 'double-treble)
                        db (eq octave-clef 'double-bass))
                  (add-mark last-note (if dt 'end-8va 'end-8vb))
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
;;; rests, rather end it and restart it.

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
                         (or (has-mark e 'beg-8va)
                             (has-mark e 'beg-8vb)))
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
                       (add-mark-once last-note
                                          (if (= 1 under8v) 'end-8va 'end-8vb)))
                     (add-mark-once e (if (= 1 under8v) 'beg-8va 'beg-8vb))
                     (loop for r in rests do
                          (rm-marks r '(beg-8va beg-8vb) nil)
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
            (add-mark-once e (if (= 1 carry) 'beg-8va 'beg-8vb))
            (loop for r in rests do 
                 (rm-marks r '(beg-8va beg-8vb) nil)
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
                (if (or (has-mark e 'end-8va)
                        (has-mark e 'end-8vb))
                    (progn
                      (print (bar-num e))
                      (add-mark-once last-note
                                         (if (= 1 bracket) 'end-8va 'end-8vb))
                      (push e rests)
                      (loop for r in rests do 
                           (rm-marks r '(end-8va end-8vb) nil)
                           (setf (8va r) 0)))
                    ;; it's a rest without end bracket
                    (push e rests))
                ;; it's a note
                (setf last-note e))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Called automatically by cmn-display and write-lp-data-for-all

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
                  (and (rhythms bar) (marks (first (rhythms bar))))
                  ;; we're in a multi-bar rest but tempo changes i.e. tempo
                  ;; change on first bar of multi is fine 
                  (and (> count 0)
                       (display-tempo (first (rhythms bar))))
                  ins-change)
              (progn
                (when (> count 1)
                  ;; we got the bar after a multi-bar rest
                  (setf (multi-bar-rest first-multi) count
                        (write-bar-num first-multi) nil))
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
          (setf got-rehearsal-letter (rehearsal-letter bar))))
  (setf (multi-bar-rests-called sc) t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken/midi-play
;;; DESCRIPTION
;;; Generate a MIDI file from the data of the specified slippery-chicken
;;; object. Note that for events that contain chords, the event's amplitude slot
;;; will be used for all pitches unless each individual pitch object in the
;;; chord has its amplitude slot set to a number (in which case the event's
;;; amplitude slot will be ignored.)  
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :midi-file. The name of the MIDI file to produce, including directory
;;;   path and extension. Default is a filename extracted from the title of the
;;;   sc piece, placed in the (get-sc-config 'default-dir) directory (default
;;;   /tmp). 
;;; - :voices. NIL or a list of player IDs indicating which of the players'
;;;   parts are to be included in the resulting MIDI file. If NIL, all players'
;;;   parts will be included. Default = NIL.
;;; - :players. A synonym for voices. This is the preferred keyword as it aligns
;;;   better with other classes, but :voices is retained for older code
;;;   compatibility. 
;;; - :start-section. An integer that is the number of the first section for
;;;   which the MIDI file is to be generated. Default = 1.
;;; - :num-sections. An integer that is the number of sections to produce MIDI
;;;   data for in the MIDI file. If NIL, all sections will be written. 
;;;   Default = NIL.
;;; - :from-sequence. An integer that is the number of the sequence within the
;;;   specified section from which to start generating MIDI data. NB: This
;;;   argument can only be used when the num-sections = 1. Default = 1.
;;; - :num-sequences. An integer that is the number of sequences for which MIDI
;;;   data is to be generated in the resulting MIDI file, including the
;;;   sequence specified in from-sequence. If NIL, all sequences will be
;;;   written. NB: This argument can only be used when the num-sections = 1.
;;;   Default = NIL.
;;; - :update-amplitudes. T or NIL to indicate whether events should have their
;;;   amplitude slots updated to ensure that any dynamic marks that have been
;;;   added (and therefore amplitude slots updated as a side-effect) reflect
;;;   the subsequent events' amplitudes also. This will override any amplitude
;;;   slots that have been changed other than by (setf (amplitude ...)) or
;;;   (add-mark ... 'some-dynamic). Also, if T, then hairpins (cresc, dim) will
;;;   result in increasing/decreasing amplitudes over their extent (but this
;;;   will only work if the amplitude/dynamic of the hairpins' starting and
;;;   ending events are correct, and in that case intervening events'
;;;   amplitudes will be overwritten no matter how they've been set; also this
;;;   won't handle cases where there's a hairpin end and a new hairpin
;;;   beginning on the same event ). Default  = T
;;; - :force-velocity. Either: an integer between 0 and 127 (inclusive) that is
;;;   the MIDI velocity value which will be given to all notes in the resulting
;;;   MIDI file, or a function which takes an event object argument and
;;;   returns a velocity from it (e.g. randomising the existing amplitude
;;;   slightly). Default = NIL.
;;; - :auto-open. Whether to open the MIDI file once written. Currently only
;;;    available on OSX with SBCL or CCL. Uses the default app for MIDI files,
;;;    as if opened with 'open' in the terminal. Default = Value of
;;;    (get-sc-config 'midi-play-auto-open).
;;; - :suffix. Add some text to the filename just before .mid?. Default = ""
;;; 
;;; RETURN VALUE
;;; Returns the path of the file written, as a string.
;;; 
;;; EXAMPLE
#|
;;; An example with some typical values for the keyword arguments. ;
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((cl (b-flat-clarinet :midi-channel 1))
                     (hn (french-horn :midi-channel 2))
                     (vc (cello :midi-channel 3))))
        :set-palette '((1 ((f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5))))
        :set-map '((1 (1 1 1 1 1 1 1))
                   (2 (1 1 1 1 1 1 1))
                   (3 (1 1 1 1 1 1 1)))
        :rthm-seq-palette '((1 ((((4 4) h (q) e (s) s))
                                :pitch-seq-palette ((1 2 3))))
                            (2 ((((4 4) (q) e (s) s h))
                                :pitch-seq-palette ((1 2 3))))
                            (3 ((((4 4) e (s) s h (q)))
                                :pitch-seq-palette ((2 3 3))))
                            (4 ((((4 4) (s) s h (q) e))
                                :pitch-seq-palette ((3 1 2)))))
        :rthm-seq-map '((1 ((cl (1 2 1 2 1 2 1))
                            (hn (1 2 1 2 1 2 1))
                            (vc (1 2 1 2 1 2 1))))
                        (2 ((cl (3 4 3 4 3 4 3))
                            (hn (3 4 3 4 3 4 3))
                            (vc (3 4 3 4 3 4 3))))
                        (3 ((cl (1 2 1 2 1 2 1))
                            (hn (1 2 1 2 1 2 1))
                            (vc (1 2 1 2 1 2 1))))))))
  (midi-play mini 
             :midi-file "/tmp/md-test.mid"
             :voices '(cl vc)
             :start-section 2))

;;; An example that passes a (lambda) function to :force-velocity. Usually, by
;;; default, event amplitudes between 0.0 and 1.0 will map onto MIDI velocities
;;; of 0 to 127. Here we map them to velocities of 0 to 100 instead.
(midi-play +jitterbug+ :force-velocity
           #'(lambda (event)
               (floor (* (amplitude event) 100))))

|#
;;; SYNOPSIS
#+cm-2
(defmethod midi-play ((sc slippery-chicken)
                      &key 
                        ;; no subsection refs: use from-sequence instead
                        (start-section 1) 
                        ;; these voices are used to get the actual sequence
                        ;; orders i.e. each voice will be appended to <section>
                        ;; when calling get-data.
                        ;; if nil then all voices.
                        (voices nil)
                        ;; MDE Thu Oct 18 17:07:57 2018 -- players is a more
                        ;; appropriate keyword than voices but keep the latter
                        ;; for historical code and make the two equivalent
                        (players nil)
                        ;; add something to the filename just before .mid?
                        (suffix "")
                        (midi-file
                         (format nil "~a~a~a.mid"
                                 (get-sc-config 'default-dir)
                                 (filename-from-title (title sc))
                                 suffix))
                        (from-sequence 1)
                        (num-sequences nil)
                        ;; if nil we'll write all the sections
                        (num-sections nil)
                        ;; MDE Mon Jun 13 12:30:55 2016
                        (update-amplitudes t)
                        ;; MDE Tue Jun  4 19:06:11 2013
                        (auto-open (get-sc-config 'midi-play-auto-open))
                        ;; if this is a 7-bit number we'll use this for all
                        ;; notes  
                        (force-velocity nil))
;;; ****
  (when update-amplitudes ; MDE Mon Jun 13 12:32:30 2016
    (format t "~&slippery-chicken::midi-play: updating amplitudes to reflect ~
                dynamic marks. ~%Set :update-amplitudes to NIL if you don't ~
                want this ~%(e.g. if you've set amplitudes directly).")
    (update-amplitudes sc)
    (handle-hairpins sc))
  (when (and players voices)
    (error "slippery-chicken::midi-play: please use either voices or players, ~
            not both."))
  (when players (setq voices players))
  (setq voices
        (cond ((and voices (listp voices)) voices)
              ((and voices (atom voices)) (list voices))
              ((not voices) (get-players (ensemble sc)))
              (t (error "slippery-chicken::midi-play: voices = ~a!?" voices))))
  ;; MDE Fri May 11 15:13:18 2012 -- if there's only one section....
  (when (and (not num-sections)
             (= start-section 1)
             (= 1 (get-num-sections sc)))
    (setf num-sections 1))
  ;; MDE Fri May 11 13:02:06 2012 -- 
  ;; MDE Mon May 14 18:46:01 BST 2012 -- we no longer have this keyword
  #|
  (when (> time-scaler 1.0)
  (error "slippery-chicken::midi-play: scaling durations by more than 1.0 ~
             would ~%interfere with MIDI note-on/note-off combinations."))
  |#
  (unless (integer>0 from-sequence)
    (error "slippery-chicken::midi-play: ~
            from-sequence must be an integer >= 1."))
  ;; MDE Fri May 11 11:42:48 2012 -- 
  (when (and num-sequences 
             (or (not num-sections)     ; MDE Fri May 11 11:56:13 2012 -- 
                 (and num-sections (> num-sections 1))))
    (error "slippery-chicken::midi-play: num-sequences keyword should only ~
            be used ~%when num-sections = 1."))
  ;; MDE Fri May 11 11:58:17 2012
  (when (and from-sequence (/= 1 from-sequence)
             (or (not num-sections)
                 (and num-sections (> num-sections 1))))
    (error "slippery-chicken::midi-play: from-sequence keyword should only ~
            be used ~%when num-sections = 1."))
  (when (and num-sections (= 1 num-sections) (not num-sequences))
    (let ((ns (num-seqs sc start-section)))
      (unless ns 
        (error "slippery-chicken::midi-play: can't get number of sequences ~
                for section ~a." start-section))
      (setf num-sequences (- ns (1- from-sequence)))))
  (let* ((voices-events (get-events-start-time-duration 
                         sc start-section voices 
                         :time-scaler 1.0
                         :from-sequence from-sequence
                         :num-sequences num-sequences
                         :num-sections num-sections
                         :get-time-sig-changes t
                         :ignore-rests nil
                         :include-rests t))
         ;; MDE Mon May  7 10:41:07 2012 -- for pieces with subsections
         (secobj (get-section sc start-section))
         (nth-seq-ref (full-ref (get-first-section secobj)))
         ;; MDE Thu Oct 17 19:10:27 2013 -- the following doesn't work if there
         ;; are sub-sub-sections!  
          #|
  (if (has-subsections secobj)
         (full-ref (print (data (first (data secobj)))))
         start-section))
         |#
         ;; do all the program changes for the beginning irrespective of
         ;; whether the player changes instrument or not. subsequent program
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
                        (- (start-time-qtrs
                            (get-nth-sequenz (piece sc) nth-seq-ref
                                             (first voices) 
                                             (1- from-sequence))))
                        force-velocity)
    (when auto-open
      (system-open-file midi-file))
    midi-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Sat Jun 17 14:28:41 2023, Heidhausen -- to avoid code dpulication in
;;; clm-lay and reaper-play 
(defmethod get-snds-from-palette ((sc slippery-chicken) sfp-refs sfp)
  (when sfp-refs
    (make-cscl
     (loop for sfpr in sfp-refs
           append (get-snds sfpr
                            (if sfp
                                sfp
                                (sndfile-palette sc)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Thu May 10 13:52:19 BST 2012: Conformed robodoc entry

;;; MDE Tue Apr 17 12:16:43 2012 -- added the pitch-synchronous option.

;;; ****m* slippery-chicken/clm-play
;;; DESCRIPTION
;;; Using the sound files (samples) defined for the given reference (group ID)
;;; in the sndfile-palette slot of the slippery-chicken object, use CLM to
;;; generate a new sound file using the pitch and timing information of one or
;;; more players' parts from the slippery-chicken object.
;;;
;;; NB: The sound file will begin with the first sounding event in the section
;;;     at 0.0 seconds. If there are any leading rests in the player's part,
;;;     these will be omitted in the output file.
;;;
;;; By grouping sound files in the sndfile-palette slot, the user can generate
;;; a CLM sound file version of the piece in various 'flavours'; perhaps, for
;;; example, using exclusively source sound files consisting of string samples,
;;; or percussion sounds, or a variety of sounds, as desired. See below for an
;;; example of a sndfile-palette.
;;; 
;;; By default, as :pitch-synchronous defaults to NIL (see below) this method
;;; does not attempt to match the pitches of the output sound file to those
;;; generated for the slippery-chicken object, rather, it generates its own
;;; sequence of pitches based on pitches from the current set. Instead of using
;;; the pitches of the specified players' parts--which might produce extreme
;;; sound file transpositions both upwards and downwards--it accesses each note
;;; of the current set (assigned by the set-map to each rthm-seq) from the
;;; bottom up, one voice after another. If do-src is T, transposition will then
;;; be calculated such that the frequency of the sound file (defaults to C4 in
;;; the sndfile object) will be shifted to the pitch of the given pitch of the
;;; set. When the set changes it will play the same respective note number of
;;; that set, etc. This means that, in effect, any given voice will retain the
;;; same pitch for the duration of the rthm-seq (set) but as the sound files
;;; cycle around, and these often have quite different pitch/spectral content,
;;; there's not necessarily a sense of pitch repetition. If you find that this
;;; transposition process still yields some extreme transpositions, the
;;; :note-number keyword can be specified to indicate an index into the current
;;; set of pitches to serve as the lowest voice. If you increase :note-number
;;; the transpositions will get higher. If the number of voices plus this index
;;; exceeds the number of pitches in the set, the method will wrap around to
;;; the lowest pitch of the set. Bear in mind then that when :pitch-synchronous
;;; is NIL, the pitch-seq curves given in the rthm-seq-palette--and therefore
;;; all the carefully chosen pitches in the piece--will be ignored in favour of
;;; representing the transpositions and therefore intervals present in the
;;; set. That might seem like a strange thing to (want to) do, but it's served
;;; me (Michael Edwards) very well on many occasions, so it's still the
;;; default.
;;;
;;; If instead of the above method the user would like the pitches in the
;;; resulting sound file to be transposed to match the pitches of the
;;; players' notes, the keyword argument :pitch-synchronous can be set
;;; to T (and do-src should be left set to T as well). This will also work with
;;; chords.
;;;
;;; See also make-sfp-from-wavelab-marker-file in sndfile-palette.lsp for
;;; automatically creating a sndfile-palette from markers in a Steinberg
;;; Wavelab marker file.
;;;
;;; Event amplitudes are as yet unused by this method.
;;;
;;; NB: CLM's nrev instrument must be loaded before calling this method. If
;;; you're using the slippery-chicken app, then this is done for you at
;;; startup. 
;;; 
;;; ARGUMENTS
;;; - A slippery chicken object.
;;; - The ID of the starting section.
;;; - The IDs of the player(s) whose events are to be used to obtain the
;;;   rhythmic structure (and optionally, pitch content) of the resulting sound
;;;   file. This can be a single symbol for an individual player, a list of
;;;   player IDs, or NIL. If NIL, the event from all players' parts will be
;;;   reflected in the output file. Default = NIL.
;;; - The ID of the sound file group in the sndfile-palette slot of the
;;;   slippery-chicken object that contains the source sound files from which
;;;   the new sound file is to be generated.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :num-sections. An integer or NIL to indicate how many sections should be
;;;   generated, including the starting section. If NIL, sound file data will
;;;   be generated for all sections of the piece. NB If there are sub-sections
;;;   this will include them in the total, i.e., if section 1 has 3 subsections
;;;   and :num-sections is 2, we'll generate data from the first two
;;;   subsections of section 1, not all subsections of main sections 1 and
;;;   2. Default = NIL. 
;;; - :from-sequence. An integer that is the number of the first sequence
;;;   within the specified starting section to be used to generate the output
;;;   file. This argument can only be used when num-sections = 1. Default = 1. 
;;; - :num-sequences. NIL or an integer that indicates how many sequences are
;;;   to be generated, including that specified by :from-sequence. If NIL, all
;;;   sequences will be played. This argument can only be used when
;;;   num-sections = 1 and the section has no subsections. Default = NIL.
;;; - :srate. A number that is the sampling rate of the output file
;;;   (independent of the input file). This and the following two arguments
;;;   default to the CLM package globals. See clm.html for more options.
;;;   Default = clm::*clm-srate*.
;;; - :header-type: A CLM package header-type specification to designate the
;;;   output sound file format. For example, clm::mus-riff will produce .wav
;;;   files, clm::mus-aiff will produce .aiff files. The value of this argument
;;;   defaults to the CLM package globals. See clm.html for more
;;;   options. Default = clm::*clm-header-type*.
;;; - :data-format. A CLM package data-format specification to designate the
;;;   output sound file sample data format. For example, clm::mus-float will
;;;   produce a 32-bit little-endian floating-point format; clm::mus-l24int
;;;   will produce little-endian 24-bit integer; mus-bshort will produce 16-bit
;;;   big-endian files, and mus-bfloat will produce 32-bit floating-point
;;;   big-endian files. NB: AIFF and AIFC files are not compatible with little
;;;   endian formats. The value of this argument defaults to the CLM package
;;;   globals. See clm.html for more options. 
;;;   Default = clm::*clm-data-format*.
;;; - :sndfile-extension. NIL or a string that will be the extension of the
;;;   output sound file (e.g. ".wav", ".aif"). If NIL, the method will
;;;   determine the extension auto matically based on the header-type. NB: The
;;;   extension does not determine the output sound file format; that is
;;;   determined by :header-type. Default = NIL.
;;; - :channels. An integer that is the number of channels in the output sound
;;;   file, limited only by the sound file format specified. Note that both
;;;   stereo and mono sounds from the palette will be randomly panned between
;;;   any two adjacent channels. Default = 2.
;;; - :rev-amt. A number that determines the amount of reverberation for the
;;;   resulting sound file, passed to CLM's nrev. 
;;;   NB: 0.1 is a lot. Default = 0.0.
;;; - time-offset. A number that is an offset time in seconds. This produces a
;;;   lead time of a specified number of seconds of silence prior to the sound
;;;   output. 
;;; - :play. T or NIL to indicate whether CLM should play the output file
;;;   automatically immediately after it has been written. 
;;;   T = play. Default = NIL.
;;; - :inc-start. T or NIL to indicate whether playback of the source sound
;;;   files is to begin at incrementally increasing positions in those files or
;;;   at their respective 0.0 positions every time. If T, the method will
;;;   increment the position in the source sound file from which playback is
;;;   begun such that it reaches the end of the source sound file the last time
;;;   it is 'played'. T = increment start times. Default = NIL.
;;; - :ignore-rests. T or NIL to indicate whether silence should be
;;;   incorporated into the resulting sound file to correspond with rests in
;;;   the player's parts. If T, the sound files will play over the duration of
;;;   rests. However, this is only true on a bar-by-bar basis; i.e., notes at
;;;   the end of one bar will not be continued over into a rest in the next
;;;   bar. This implies that rests at the start of a bar will not be turned
;;;   into sounding notes. T = ignore resets. Default = T.
;;; - :sound-file-palette-ref2. The ID of a sound file group in the given
;;;   slippery-chicken object's sndfile-palette slot. If this reference is
;;;   given, the method will invoke fibonacci-transitions to transition from
;;;   the first specified group of source sound files to this one. If NIL, only
;;;   one group of source sound files will be used. Default = NIL.
;;; - :do-src. T, NIL, a number, or a note-name pitch symbol to indicate whether
;;;   transposition of the source sound files for playback will be calculated
;;;   such that the perceived fundamental frequencies of those sound files are
;;;   shifted to match the pitches of the current set. If do-src is a number
;;;   (frequency in Hertz) or a note-name pitch symbol, the method will use
;;;   only that pitch instead of the sound files' frequencies when transposing
;;;   to the events' pitches. NB Whichever is used, after being converted to a
;;;   sample rate conversion factor, this is always multiplied by the
;;;   src-scaler (see below). T = match sound files' frequencies to set
;;;   pitches. NIL means no transposition will be performed. Default = T.
;;; - :pitch-synchronous: T or NIL to indicate whether the source sound files
;;;   are to be transposed to match the pitches of the events in the given
;;;   players' part. This will only be effective if the given source sound file
;;;   has a perceptible frequency that has been specified using the sndfile
;;;   object's :frequency slot in the sndfile-palette. :do-src must also be T
;;;   for this to work. T = match pitches. Default = NIL.
;;; - :reset-snds-each-rs. T or NIL to indicate whether to begin with the first
;;;   source sound file of the specified group at the beginning of each
;;;   rthm-seq. T = begin with the first sound file. Default = T.
;;; - :reset-snds-each-player. T or NIL to indicate whether to begin with the
;;;   first source sound file of the specified group for the beginning of each
;;;   player's part. T = begin with the first sound file. Default = T. 
;;; - :play-chance-env. A list of break-point pairs that determines the chance
;;;   that a given event from the source player's part will be reflected in the
;;;   new sound file. It is determined by random selection but uses a fixed
;;;   seed that is re-initialized each time clm-play is called. The following
;;;   default ensures every note will play: '(0 100 100 100).
;;; - :play-chance-env-exp. A number that will be applied as the exponent to
;;;   the play-chance-env's y values to create an exponential interpolation
;;;   between break-point pairs. Default = 0.5.
;;; - :max-start-time. A number that is the last time-point in seconds for
;;;   which events will be processed for the output file. If a maximum start
;;;   time is specified here (in seconds), events after this will be
;;;   skipped. The default value of 99999999 seconds (27778 hours) will result
;;;   in all events being reflected in the sound file.
;;; - :time-scaler. A number that will be the factor by which all start times
;;;   are scaled for the output file (in effect a tempo scaler). If
;;;   :ignore-rests is T, this will also have an indirect effect on
;;;   durations. This argument should not be confused with
;;;   :duration-scaler. Default = 1.0.
;;; - :duration-scaler. A number that is the factor by which the duration of
;;;   all  events in the output sound file will be scaled. This does not alter
;;;   start times, and will therefore result in overlapping sounds if greater
;;;   than 1.0. This is not to be confused with :time-scaler. 
;;;   Instead of a simple number, :duration-scaler can also be an envelope 
;;;   (a list of break-point pairs). This means you could smoothly transition
;;;   from staccato-like to overlapping sounds in a single clm-play call.
;;;   Default = 1.0.
;;; - :normalise. A decimal number that will be the maximum amplitude of the
;;;   resulting output file; i.e., to which the samples will be scaled. Can
;;;   also be NIL, whereupon no normalisation will be performed. 
;;;   Default = 0.99
;;; - :amp-env. A list of break-point pairs that will govern the amplitude
;;;   envelope applied to all source-sound files as it is being written to the
;;;   new output file. NB: If the user wants to maintain the original attack of
;;;   the source sound file and is not employing the :inc-start option, this
;;;   should be set to '(0 1 ...). If :inc-start is T, the resulting sound file
;;;   will probably contain clicks from non-zero crossings. 
;;;   Default = '(0 0 5 1 60 1 100 0).
;;; - :src-width. An integer that reflects the accuracy of the sample-rate
;;;   conversion. The higher the value, the more accurate the transposition,
;;;   but the slower the processing. Values of 100 might be useful for very low
;;;   transpositions. Default = 20.
;;; - :src-scaler: A number that is the factor by which all sample-rate
;;;   conversion values will be scaled (for increasing or decreasing the
;;;   transposition of the overall resulting sound file). 
;;;   Instead of a simple number, :src-scaler can also be an envelope 
;;;   (a list of break-point pairs), similar to :duration-scaler. Default = 1.0.
;;; - :note-number. A number that is an index, representing the the nth pitch
;;;   of the current set or chord (from the bottom) to be used for the lowest
;;;   player. Default = 0.
;;; - :duration-run-over. T or NIL to indicate whether the method will allow a
;;;   sound file event to extend past the end of specified segment boundaries
;;;   of a sound file in the sndfile-palette. T = allow. Default = NIL.
;;; - :short-file-names. T or NIL to indicate whether abbreviated output file
;;;   names will be automatically created instead of the usually rather long
;;;   names. T = short. Default = NIL.
;;; - :output-name-uniquifier. A user-specified string that will be
;;;   incorporated into the file name, either at the end or the beginning
;;;   depending on whether short-file-names is T or NIL. Default = "".
;;; - :check-overwrite. T or NIL to indicate whether to query the user before
;;;   overwriting existing sound files. T = query. Default = T.
;;; - :print-secs. T or NIL to indicate whether CLM should print the seconds
;;;   computed as it works. T = print. Default = NIL.
;;; - :simulate. T or NIL to indicate whether only the sound file sequencing
;;;   information should be calculated and printed for testing purposes,
;;;   without generating a sound file. T = simulate. Default = NIL.
;;; - :sndfile-palette. NIL or a file name including path and extension that
;;;   contains an external definition of a sndfile-palette. This will replace
;;;   any sndfile-palette defined in the slippery-chicken object. If NIL, the
;;;   one in the slippery-chicken object will be used. Default = NIL.
;;; - :chords. NIL or a list of lists consisting of note-name symbols to be
;;;   used as the pitches for the resulting sound file in place of the pitches
;;;   from the set-map. There must be one chord specified for each sequence. If
;;;   NIL, the pitches from the set-map will be used. Default = NIL.
;;; - :chord-accessor. Sometimes the chord stored in the palette is not a
;;;   simple list of data so we need to access the nth of the chord
;;;   list. Default = NIL.
;;; - :clm-ins. The CLM instrument that should be called to generate sound file
;;;   output. This must accept required and keyword arguments like samp5 (see
;;;   src/samp5.lsp or more simply src/sine.lsp) but can of course choose to
;;;   ignore them. Remember that your CLM instruments will (most probably) be
;;;   in the CLM package so you'll need the clm:: qualifer. Default =
;;;   #'clm::samp5. 
;;; - :clm-ins-args. This should be a list of keyword arguments and values to
;;;   pass to each CLM instrument call (e.g. '(:cutoff-freq 2000 :q .6)) or a
;;;   function which takes two arguments (the current event and the event
;;;   number) and returns a list of keyword arguments perhaps based on
;;;   those. Default = NIL.
;;; - :pan-min-max. Each event is panned randomly between any two adjacent
;;;   channels in the output sound file. The values used are chosen from the
;;;   following list of degrees: (15 25 35 45 55 65 75). However, these can be
;;;   scaled to be within new limits by passing a two-element list to
;;;   :pan-mix-max. E.g. '(40 50) would limit most values to the middle area of
;;;   stereo space. NB if stereo soundfiles are used as input the original
;;;   panning will generally be retained. See samp5.lsp for details of how such
;;;   files are handled. Default = NIL (i.e. use the list given above).
;;; - :pan-fun. If you want to take charge of selecting pan positions yourself,
;;;   pass a function via this keyword. The function must take one argument: the
;;;   current event, though of course, it can ignore it completely if
;;;   preferred. The function should return a degree value: a number between 0
;;;   and 90. Bear in mind if using samp5 with more than 2 output channels that
;;;   this will still result in the sound moving around multichannel space as
;;;   all you would be setting is the pan value between any two adjacent
;;;   channels. If this isn't your cup of tea, pass a new instrument entirely
;;;   via :clm-ins. See example below. Default = NIL.
;;; - :snd-selector. By default the sound files in the given group are cycled
;;;   through, one after the other, returning to the beginning when the end is
;;;   reached. This can be changed by passing a function to :snd-selector. This
;;;   function must take three arguments: the circular-sclist object that
;;;   contains the group's sndfile objects (the sndfiles are in a simple list
;;;   in the data slot); the pitch of the current event (if the event happens
;;;   to be a chord, each pitch of the chord will be handled separately
;;;   i.e. the :snd-selector function will be called for each pitch); and the
;;;   event object. Of course any of these arguments may be ignored by the
;;;   :snd-selector function; indeed it is imagined that the event object will
;;;   be mostly ignored and the pitch object used instead, but it is
;;;   nevertheless passed for completeness. For example, the following
;;;   function, declared on-the-fly using lambda rather than separately with
;;;   defun, makes clm-play act as a traditional sampler would: Assuming our
;;;   sndfile-palette group has a list of sound files whose frequencies really
;;;   correspond to their perceived fundamentals (e.g. see the
;;;   set-frequency-from-filename method, and the kontakt-to-sfp function) we
;;;   choose the nearest sound file in the group to the frequency of the
;;;   current event's pitch:
;;;
;;;            :snd-selector #'(lambda (sflist pitch event)
;;;                              (declare (ignore event))
;;;                              (get-nearest-by-freq
;;;                               (frequency pitch) (data sflist)))
;;;
;;;   NB If :pitch-synchronous is NIL, then the pitch-or-chord slot of the event
;;;   will be the pitch selected from the current set, rather than the actual
;;;   pitch of the instrument in question.
;;;   Default = NIL i.e. use the circular selection method.
;;; - :decay-time. Reverb decay time. Default = 3 seconds.
;;; - :snd-transitions. Set the transition from sound-file-ref to sound-file-ref
;;;   over the course of the call to clm-play using a custom envelope instead of
;;;   the default fibonacci-transitions method, e.g. '(0 0 100 1). x-axis is
;;;   arbitrary, y-axis should range from 0 to 1. When reading, y-values will be
;;;   rounded. 
;;; 
;;; RETURN VALUE
;;; Total events generated (integer).
;;; 
;;; EXAMPLE
#|
;;; An example using some of the more frequent arguments
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((cl (b-flat-clarinet :midi-channel 1))
                     (hn (french-horn :midi-channel 2))
                     (vc (cello :midi-channel 3))))
        :set-palette '((1 ((f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5))))
        :set-map '((1 (1 1 1 1 1 1 1))
                   (2 (1 1 1 1 1 1 1))
                   (3 (1 1 1 1 1 1 1)))
        :rthm-seq-palette '((1 ((((4 4) h (q) e (s) s))
                                :pitch-seq-palette ((1 (2) 3))))
                            (2 ((((4 4) (q) e (s) s h))
                                :pitch-seq-palette ((1 2 (3)))))
                            (3 ((((4 4) e (s) s h (q)))
                                :pitch-seq-palette ((2 3 3))))
                            (4 ((((4 4) (s) s h (q) e))
                                :pitch-seq-palette ((3 (1) 2)))))
        :rthm-seq-map '((1 ((cl (2 3 2 4 1 3 1))
                            (hn (2 4 1 2 3 1 3))
                            (vc (1 2 2 3 4 1 3))))
                        (2 ((cl (4 2 1 3 3 1 2))
                            (hn (2 1 4 3 2 1 3))
                            (vc (2 3 4 3 1 2 1))))
                        (3 ((cl (3 1 2 4 3 1 2))
                            (hn (3 4 2 1 3 2 1))
                            (vc (3 2 3 1 4 2 1)))))
        :snd-output-dir (get-sc-config 'default-dir)
        :sndfile-palette '(((sndfile-grp-1
                             ((test-sndfile-1.aiff)
                              (test-sndfile-2.aiff)
                              (test-sndfile-3.aiff)))
                            (sndfile-grp-2
                             ((test-sndfile-4.aiff :frequency 834)
                              (test-sndfile-5.aiff)
                              (test-sndfile-6.aiff))))
                           ("/path/to/sndfiles-dir-1"
                            "/path/to/sndfiles-dir-2")))))
  (clm-play mini 2 '(cl vc) 'sndfile-grp-1
            :num-sections 1
            :srate 48000
            :header-type clm::mus-aiff
            :data-format clm::mus-b24int
            :rev-amt 0.05
            :inc-start t
            :ignore-rests nil
            :sound-file-palette-ref2 'sndfile-grp-2
            :pitch-synchronous t
            :reset-snds-each-rs nil
            :reset-snds-each-player nil))

;;; A minimal working example demonstrating envelope-arguments 
;;; to duration-scaler and src-scaler
(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1 1 1)))
         :rthm-seq-palette '((1 ((((2 4) (s) (s) e e e))
                                 :pitch-seq-palette ((1 2 3)))))
         :rthm-seq-map '((1 ((vn (1 1 1)))))
         :snd-output-dir "/tmp/"
         :sndfile-palette '(((sndfile-grp-1
                              ((test-sndfile-1.aiff)
                               (test-sndfile-2.aiff)
                               (test-sndfile-3.aiff))))
                            ("/path/to/sndfiles-dir-1")))))
  (clm-play mini 1 'vn 'sndfile-grp-1
            :src-scaler '(0 .5  80 1  100 2)
            :do-src 500
            :src-width 5
            :duration-scaler '(0 .1  80 1  100 2)
            :check-overwrite nil
            :play nil :header-type clm::mus-riff))

;;; if you don't want amplitude panning, rather a random placement of each sound
;;; in one channel only, then this should work (no matter how many channels
;;; there are in your output file):

    :pan-fun #'(lambda (event)
                 (declare (ignore event))
                 (nth (random-rep 2) '(0 90)))

|#
;;; SYNOPSIS
#+clm
(defmethod clm-play ((sc slippery-chicken) section players 
                     sound-file-palette-ref 
                     &key 
                     sound-file-palette-ref2
                     (play-chance-env '(0 100 100 100))
                     (max-start-time 99999999)
                     (play-chance-env-exp 0.5)
                     (time-scaler 1.0)
                     (normalise .99)
                     scaled-by
                     (simulate nil)
                     (from-sequence 1)
                     (num-sequences nil)
                     (num-sections nil)
                     (ignore-rests t)
                     (time-offset 0.0)
                     (chords nil)
                     (chord-accessor nil)
                     (note-number 0)
                     (play nil)
                     (amp-env '(0 0 5 1 60 1 100 0))
                     (inc-start nil)
                     (src-width 20)
                     ;; either a number or an envelope
                     (src-scaler 1.0)
                     (do-src t)
                     (pitch-synchronous nil)
                     (rev-amt 0.0)
                     ;; either a number or an envelope
                     (duration-scaler 1.0)
                     (short-file-names nil)
                     (check-overwrite t)
                     (reset-snds-each-rs t)
                     (reset-snds-each-player t)
                     (duration-run-over nil)
                     (channels 2)
                     (srate clm::*clm-srate*)
                     (header-type clm::*clm-header-type*)
                     (data-format clm::*clm-data-format*)
                     (print-secs nil)
                     (output-name-uniquifier "")
                     (sndfile-extension nil)
                     (sndfile-palette nil)
                     ;; MDE Sat Oct  3 18:45:28 2015 -- for Cameron!
                     pan-fun
                     ;; MDE Thu Oct  1 21:03:59 2015 
                     (pan-min-max nil)  ; actually '(15 75) by default below
                     ;; MDE Thu Oct  1 19:13:49 2015
                     snd-selector 
                     ;; MDE Mon Nov 4 10:10:35 2013 -- the following were
                     ;; added so we could use instruments other than samp5 MDE
                     ;; Thu Mar 21 15:42:53 2024, Heidhausen -- now defaults to
                     ;; nil because samp5 fun is not available until it's
                     ;; compiled and this happens within clm-play now rather
                     ;; than in advance
                     (clm-ins nil)    ;#'clm::samp5)
                     ;; either a list or a function (see above)
                     clm-ins-args
                     ;; DJR Thu 22 Aug 2019 15:08:39 BST
                     ;; clm::with-sound let's us set this, so why doesn't 
                     ;; clm-play? Voila!
                     (decay-time 3)
                     ;; DJR Mon 16 Sep 2019 01:26:11 BST
                     ;; We can now set snd-transitions with a custom envelope,
                     ;; e.g. '(0 0 100 1). x-values are arbitrary, y-values
                     ;; are from 0 (for sound-file-palette-ref) and 1 (for
                     ;; sound-file-palette-ref2).
                     snd-transitions)
;;; ****
  (get-clm-ins 'clm::samp5 "samp5.lsp" cl-user::+slippery-chicken-src-path+)
  (get-clm-ins 'clm::sine "sine.lsp" cl-user::+slippery-chicken-src-path+)
  (unless clm-ins (setq clm-ins #'clm::samp5))
  ;; Mde Tue Apr 17 13:28:16 2012 -- guess the extension if none given
  (unless sndfile-extension
    (setf sndfile-extension
          (cond                         ; can't use case with clm globals
            ((or (= header-type clm::mus-aiff)
                 (= header-type clm::mus-aifc))
             ".aif")
            ((= header-type clm::mus-riff) ".wav")
            ((= header-type clm::mus-next) ".snd")
            (t (error ".aif")))))
  ;; MDE Mon Apr  2 10:23:21 2012      
  (unless (fboundp 'clm::nrev)
    (error "slippery-chicken::clm-play: clm's nrev.ins needs to be ~
            compiled ~%and loaded for this method to run."))
  ;; MDE Sat Jun  2 11:41:47 2012 --   
  (when (and (numberp num-sequences)
             (has-subsections (get-section sc section)))
    (error "slippery-chicken::clm-play: :num-sequences (~a) should only be ~
            specified ~%for a section (~a) with no subsections."
           num-sequences section))
  ;; MDE Fri May 11 15:13:18 2012 -- if there's only one section....
  (when (and (not num-sections)
             (= section 1)
             (= 1 (get-num-sections sc)))
    (setf num-sections 1))
  ;; MDE Wed Apr 25 14:45:03 2012 -- if we're playing more than one section
  ;; then we shouldn't specify num-sequences as that might result in gaps in
  ;; playback (e.g. if section 2 had more seqs than requested) ;
  (when (and num-sequences 
             (or (not num-sections)     ; MDE Fri May 11 11:56:13 2012 -- ;
                 (and num-sections (> num-sections 1))))
    (error "slippery-chicken::clm-play: num-sequences keyword should only ~
            be used ~%when num-sections = 1."))
  (when (and from-sequence (/= 1 from-sequence)
             (or (not num-sections)
                 (and num-sections (> num-sections 1))))
    (error "slippery-chicken::clm-play: from-sequence keyword should only ~
            be used ~%when num-sections = 1."))
  #| 
  ;; MDE Sat Jun 2 12:51:03 2012 -- actually, we don't need to do this, and ;
  it                                    ;
  ;; just causes problems now we've updated num-seqs to handle sub-sections ;
  (when (and num-sections (= 1 num-sections) (not num-sequences)) ;
  (let ((ns (num-seqs sc section)))     ;
  (unless ns                            ;
  (error "slippery-chicken::clm-play: can't get number of sequences ~ ; ;
  for section ~a." section))            ; ;
  (setf num-sequences (- ns (1- from-sequence))))) ; ;
  |#
  (unless (listp players)
    (setf players (list players)))
  ;; MDE Mon Apr  2 09:34:36 2012 
  (unless players
    (setf players (players sc)))
  ;; re-initialise our random number generator.
  (random-rep 100 t)
  ;; LMF Mon Jul 19 2021
  ;; the argument to duration-scaler can be either a number or an envelope.
  ;; When it is the former, it will now be converted to an envelope.
  (when (atom duration-scaler)
    (setf duration-scaler (list 0 duration-scaler 100 duration-scaler)))
  ;; same for src-scaler
  (when (atom src-scaler)
    (setf src-scaler (list 0 src-scaler 100 src-scaler)))
  ;; 10/1/07 remove the events with a start-time after max-start-time at this
  ;; stage rather than rejecting them later (otherwise play-chance-env will
  ;; range over the full event list instead of those below max-start-time)
  ;;
  ;; DJR Thu  5 Sep 2019 12:42:07 BST
  ;; (force-list) the soundfile-palette-refs so we can use multiple refs later.
  (setf sound-file-palette-ref (force-list sound-file-palette-ref))
  (setf sound-file-palette-ref2 (force-list sound-file-palette-ref2))
  (let* ((events (get-events-with-src sc section players 
                                      ;; these have 0 duration so we must ignore
                                      ;; them for now 
                                      :ignore-grace-notes t
                                      :time-scaler time-scaler
                                      :from-sequence from-sequence
                                      :num-sequences num-sequences
                                      :num-sections num-sections
                                      :ignore-rests ignore-rests
                                      :chords chords
                                      :pitch-synchronous pitch-synchronous
                                      :chord-accessor chord-accessor
                                      :note-number note-number))
         (section1-num-seqs (if num-sequences
                              num-sequences
                              (num-seqs sc section)))
         (num-players (length players))
         (events-per-player (ml 0 num-players))
         ;; clisp doesn't like (loop for player in events sum (loop for rs ...
         (total-events (loop 
                         for i from 0
                         for player in events
                         for len = (loop for rs in player sum (length rs))
                         do (setf (nth i events-per-player) len)
                         sum len))
         ;; DJR Thu  5 Sep 2019 12:41:00 BST
         ;; Update snds and snds2 to accept multiple sound-file-palette-refs
         (snds (get-snds-from-palette sc sound-file-palette-ref
                                      sndfile-palette))
         (snds2 (get-snds-from-palette sc sound-file-palette-ref2
                                       sndfile-palette))
         #|
         ;; DJR Mon 16 Sep 2019 01:26:11 BST ; ;
         ;; We'll sort this later. See below. ; ;
         (snd-transitions (loop for num-events in events-per-player collect ; ;
         (fibonacci-transition num-events))) ; ;
         |#
         (sndl nil)
         (snd-group nil)
         (srts '())
         (freqs '())
         ;; will be nil when we're using the events' pitch data
         (srt-freq (cond ((numberp do-src) do-src)
                         ((and do-src
                               (not (eq do-src t))
                               (symbolp do-src))
                          (note-to-freq do-src))))
         ;; MDE Thu Oct  1 21:07:00 2015
         (pan-vals '(15 25 35 45 55 65 75))
         (duration 0.0)
         (wanted-duration 0.0)
         (wanted-duration-string "")
         (first-event-start nil)
         (input-start 0.0)
         (latest-possible-start 0.0)
         (available-dur 0.0)
         (event-count 1)
         (event-count-player 0)
         (events-this-rs 0)
         (output-start 0.0)
         (output-ok t)
         (this-play-chance-env '())
         (this-duration-scaler-env '())
         (this-src-scaler-env '())
         (events-before-max-start 0)
         (skip-this-event t)
         (total-skipped 0)
         (file-name
           (string-downcase        
            (if short-file-names
              (format nil "~{~a-~}~a~{~a-~}~{~a.~}~a-~a~a~a~a"
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
                      (if pitch-synchronous "-psync" "")
                      sndfile-extension)
              (format nil "~a~a~{-~a~}~{-~a~}~{-~a~}~{-to-~a~}-seq~a-~a~a~a"
                      output-name-uniquifier
                      (string-trim "+" (id sc))
                      (if (listp section) section (list section))
                      players
                      (if (listp sound-file-palette-ref) 
                        sound-file-palette-ref
                        (list sound-file-palette-ref))
                      (when sound-file-palette-ref2
                        (if (listp sound-file-palette-ref2) 
                          sound-file-palette-ref2
                          (list sound-file-palette-ref2)))
                      from-sequence 
                      (+ -1 from-sequence section1-num-seqs)
                      (if pitch-synchronous "-psync" "")
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
    ;; NB this will be pointless if we've passed a pan-fun
    (when pan-min-max
      (setq pan-vals (loop for p in pan-vals
                           collect (fscale p 15 75 (first pan-min-max)
                                           (second pan-min-max)))))
    (when (and sound-file-palette-ref (zerop (sclist-length snds)))
      (error "slippery-chicken::clm-play: <snds>: No sounds for reference ~a"
             sound-file-palette-ref))
    (when (and sound-file-palette-ref2 (zerop (sclist-length snds2))
               (error "slippery-chicken::clm-play: <snds2>: ~
                       No sounds for reference ~a"
                      sound-file-palette-ref2)))
    ;; DJR Mon 16 Sep 2019 01:26:11 BST
    ;; are we setting snd-transitions by default with finonacci-transitions or
    ;; with a custom envelope?
    (if (and snd-transitions sound-file-palette-ref2)
      (setq snd-transitions
            (loop for num-events in events-per-player
                  collect
                  (loop with sts = (new-lastx snd-transitions num-events)
                        for n below num-events collect
                        (round (interpolate n sts)))))
      (setq snd-transitions (loop for num-events in events-per-player
                                  collect (fibonacci-transition num-events))))
    (when (and check-overwrite (probe-file output))
      (setf output-ok 
            (yes-or-no-p "File exists: ~%~a  ~%Overwrite (yes or no) > " 
                         output)))
    ;; (print snd-transitions) (print snds2)
    ;; this is where we work out how many times we'll use the sndfiles for when
    ;; inc-start is T 
    (when output-ok
      (format t "~%Output file will be ~%\"~a\"~%~%" output)
      (when inc-start
        (loop for snd in (data snds) do (reset-usage snd))
        (when snds2
          (loop for snd in (data snds2) do (reset-usage snd)))
        (loop for player in events and snd-trans in snd-transitions do
          (setf snd-trans (copy-list snd-trans))
          (loop for rs in player do
            (loop 
              for event in rs 
              for sndlist = (when snds
                              (get-sndfiles-from-user-fun
                               event
                               (if (and snds2 (= 1 (pop snd-trans)))
                                 snds2 snds)
                               snd-selector))
              do
                 (loop for snd in sndlist do
                   (unless snd
                     (error "slippery-chicken::clm-play: ~
                                      snd is nil (whilst counting)!"))
                   (incf (will-be-used snd))))))
        ;; here we reset them before starting, this is correct!
        (reset snds)
        (when snds2
          (reset snds)))
      ;; MDE Tue Apr 17 18:53:36 2012 -- get the lowest start time of all the
      ;; players. events only includes sounding events here, not rests.
      ;; (print events)
      ;; (print (first (first events)))
      (setf first-event-start 
            (loop
              for player in events
              ;; MDE Fri Jan  4 17:43:44 2019 -- bug fix: single players with
              ;; bars-rest at beginning don't pick up the minimum start time
              ;; unless we remove nils and flatten 
              ;; for ffv = (print (first (first player)))
              for ffv = (first (remove-if-not #'event-p (flatten player)))
              if ffv minimize (start-time ffv)))
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (clm::with-sound (:scaled-to normalise 
                         :reverb clm::nrev
                         :decay-time decay-time
                         :output output
                         ;; these things should be default anyway but
                         ;; somehow they're not...
                         :srate srate
                         :data-format data-format
                         :header-type header-type
                         :play play :channels channels :statistics t
                         ;; we have normalise so don't need scaled-to 
                         ;; :scaled-to scaled-to
                         :scaled-by scaled-by)
        (loop 
          for player in events and player-name in players 
          and snd-trans in snd-transitions
          ;; and events-this-player in events-per-player
          and player-count from 1
          ;; 15/12/06 this while clause causes a player not to process when
          ;; the previous overstepped the max-start-time while happy
          do
             (setf snd-trans (copy-list snd-trans)
                   event-count-player 0
                   ;; 15/12/06 reset happy to the new player processes
                   happy t
                   events-before-max-start
                   (count-events-before-max-start 
                    player (- (+ max-start-time first-event-start) time-offset))
                   this-play-chance-env 
                   (new-lastx play-chance-env
                              ;; 10/1/07 we want to use the whole
                              ;; play-chance-env when we use max-start-time:
                              ;; (1- events-this-player)))
                              ;; got to take time-offset and the start time of
                              ;; the first event into consideration, not just
                              ;; max-start-time...
                              events-before-max-start)
                   ;; LMF Mon Jul 19 2021 -- same as this-play-chance-env
                   this-duration-scaler-env
                   (new-lastx duration-scaler events-before-max-start)
                   this-src-scaler-env
                   (new-lastx src-scaler events-before-max-start))
             (format t "~%Processing player ~a/~a: ~a (resting players will ~
                          not be processed)~%"
                     player-count num-players (nth (1- player-count) players))
             (when (and (numberp num-sections) (= 1 num-sections))
               ;; MDE Tue Apr 3 09:54:46 2012 -- make sure we don't crash
               ;; if the requested instrument is sitting this section out
               (let ((rss (get-data-from-palette
                           (flatten (list section player-name))
                           (rthm-seq-map sc)
                           nil)))       ; no warning
                 ;; this code will only work when we're processing 1 section
                 (setf rthm-seqs 
                       (when rss
                         (subseq 
                          rss
                          (1- from-sequence)
                          (1- (+ from-sequence section1-num-seqs)))))))
             (when reset-snds-each-player
               (when snds (reset snds))
               (when snds2 (reset snds2)))
             (loop for rs in player and rs-count from 0 while happy do
               (setf events-this-rs (length rs))
               ;; (print rs)
               ;; (print rthm-seqs) (print rs-count)
               (format t "~%    Processing rthm-seq ~a (~a events)~%"
                       ;; print the rthm-seq id if we're only doing one
                       ;; section otherwise the rthm-seq count
                       ;; MDE Tue Apr  3 09:54:46 2012 -- make sure we don't
                       ;; crash if the requested instrument is sitting this
                       ;; section out
                       ;; 
                       ;; MDE Wed Feb 24 19:54:51 2016 -- now rs is the list
                       ;; of events but we need to know the id of the
                       ;; original rthm-seq hence the nth. In any case the
                       ;; following test was wrong; instead we can just see
                       ;; if we've got a list of events in rs
                       (let ((tmp (when (and rthm-seqs rs)
                                    (nth rs-count rthm-seqs))))
                         (if tmp 
                           (id tmp)
                           (1+ rs-count)))
                       events-this-rs)
               (when reset-snds-each-rs
                 (when snds (reset snds))
                 (when snds2 (reset snds2)))
               (loop for event in rs and rs-event-count from 0 while happy
                     do
                        ;; (print 'event)
                        (setq snd-group (pop snd-trans)
                              ;; MDE Mon Nov  4 11:11:07 2013 
                              freqs (let ((f (frequency
                                              (pitch-or-chord event))))
                                      (if (listp f) f (list f)))
                              sndl (if snds
                                     ;; MDE Fri Oct  2 09:48:39 2015 
                                     (get-sndfiles-from-user-fun
                                      event
                                      (if (and snds2 (= 1 snd-group))
                                        snds2 snds)
                                      snd-selector)
                                     ;; MDE Tue Aug 8 16:21:34 2017 -- anything
                                     ;; so long as we can make the loop below
                                     ;; work
                                     (ml nil (length freqs)))
                              duration (* (interpolate event-count-player
                                                       this-duration-scaler-env)
                                          (compound-duration-in-tempo event))
                              skip-this-event 
                              ;; MDE Sat Nov 9 15:20:11 2013 -- only when we've
                              ;; got events to output
                              (unless (zerop events-before-max-start)
                                (> (random-rep 100.0)
                                   (interpolate event-count-player 
                                                this-play-chance-env
                                                :exp play-chance-env-exp)))
                              ;; MDE Tue Apr 10 13:10:37 2012 -- see note to
                              ;; do-src keyword above.
                              srts (if do-src
                                     ;; MDE Tue Apr 17 12:52:40 2012 -- update:
                                     ;; we now have the pitch-synchronous
                                     ;; option so need to handle chords so
                                     ;; we'll not call the pitch method here
                                     ;; but the event. This will return a list,
                                     ;; even for a single pitch, so we'll have
                                     ;; to loop through them.
                                     (src-for-sample-freq 
                                      (if srt-freq
                                        srt-freq
                                        (if (and sndl
                                                 (not (every #'not sndl)))
                                          sndl 261.626))
                                      ;; MDE Tue Apr 17 12:54:06 2012 -- see
                                      ;; comment above. this used to be
                                      ;; (pitch-or-chord event)
                                      event)
                                     '(1.0)))
                        ;; (print srts)
                        ;; (print freqs)
                        (loop for srt in srts and freq in freqs and snd in sndl
                              do
                                 ;; (print srt) (print src-scaler)
                                 ;; (print snd)
                                 (setf srt (* (interpolate event-count-player
                                                           this-src-scaler-env)
                                              srt))
                                 (when (<= srt 0.0)
                                   (error "slippery-chicken::clm-play: illegal ~
                                      sample rate conversion: ~a" srt))
                                 ;; MDE Mon Apr  9 12:31:07 2012
                                 (when snd
                                   (unless (duration snd)
                                     (error "~a~%slippery-chicken::clm-play: ~
                                    sound duration is NIL!" snd)))
                                 ;; given the srt, what's the longest output dur
                                 ;; this sound can make?  
                                 (setf available-dur 
                                       (if snd 
                                         (/ (duration snd) srt)
                                         most-positive-short-float)
                                       wanted-duration-string ""
                                       input-start (if snd (start snd) 0.0))
                                 (when skip-this-event
                                   (incf total-skipped))
                                 ;;(unless snd (error
                                 ;;"slippery-chicken::clm-play: snd is nil!"))
                                 (when inc-start
                                   (unless snd
                                     (error "~%slippery-chicken::clm-play: ~
                                             can't do inc-start with no ~
                                             sndfile-palette."))
                                   (setf latest-possible-start
                                         (- (end snd) (* srt duration)))
                                   (unless (and (< latest-possible-start
                                                   (start snd))
                                                (not (zerop
                                                      (will-be-used snd))))
                                     (incf input-start 
                                           (* (has-been-used snd)
                                              (/ (- latest-possible-start
                                                    (start snd))
                                                 (will-be-used snd)))))
                                   (incf (has-been-used snd)))
                                 (when (> duration available-dur)
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
                                 ;; (print-simple event)
                                 (when happy
                                   (format t "        ~a/~a Events: ~a~
                                 ~%             ~a ~a~
                                 ~%             start-time ~,3f, input-start: ~
                                 ~,3f, ~
                                 ~%             duration ~,3f~a, ~
                                 ~%             amp ~,2f, srt ~,2f ~
                                 (pitch-or-chord ~,3fHz, sample freq ~,3f)~%"
                                           event-count total-events
                                           (if skip-this-event "Skipped" 
                                             "Written (not skipped)")
                                           (if snd (path snd) "")
                                           (if snds2
                                             (format nil "(snd-group ~a)" 
                                                     (1+ snd-group))
                                             "")
                                           output-start 
                                           input-start duration
                                           wanted-duration-string
                                           (if snd (amplitude snd) 1.0) 
                                           srt 
                                           ;; MDE Tue Apr 17 13:14:45 2012 --
                                           ;; added frequency method to chord
                                           ;; also so that this doesn't fail
                                           (frequency (pitch-or-chord event))
                                           ;; freq
                                           (if snd (frequency snd) "n/a")))
                                 (unless (or simulate skip-this-event
                                             (not happy)
                                             (zerop duration))
                                   ;; MDE Mon Nov 4 13:11:08 2013 -- we can now
                                   ;; call user-defined CLM instruments (as long
                                   ;; as they can take the following
                                   ;; arguments). We can pass along further
                                   ;; arguments or provide a function that will
                                   ;; create these for us.
                                   (apply clm-ins
                                          (append 
                                           (list (if snd (path snd)
                                                   "no path required")
                                                 output-start
                                                 :duration duration
                                                 :start input-start
                                                 :srt srt
                                                 :frequency freq
                                                 :width src-width
                                                 :amp (if snd
                                                        (amplitude snd) 1.0)
                                                 :amp-env amp-env
                                                 :degree
                                                 ;; 2/8/05: place both mono and
                                                 ;; stereo files in space
                                                 ;; randomly NB A sound is
                                                 ;; always put between two
                                                 ;; speakers but it could be two
                                                 ;; of any number; see samp5.lsp
                                                 ;; for details.  MDE Sat Oct 3
                                                 ;; 18:48:18 2015 -- we now also
                                                 ;; allow a :pan-fun
                                                 (if pan-fun
                                                   (funcall pan-fun event)
                                                   (nth (random 7) pan-vals))
                                                 :rev-amt rev-amt
                                                 :printing print-secs)
                                           (if (functionp clm-ins-args)
                                             (funcall clm-ins-args event
                                                      event-count)
                                             clm-ins-args)))))
                        (incf event-count-player)
                        (incf event-count))))))
    (unless (zerop total-events)
      (format t "~%~%~d/~d events skipped (~f%)"
              total-skipped total-events 
              (* 100.0 (/ total-skipped total-events))))
    total-events))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Thu Feb 22 11:01:56 2024, Heidhausen
(defmethod get-reaper-markers ((sc slippery-chicken)
                               &optional (time-scaler 1) (seqs t) (tempi t))
  (let ((player1 (first (players sc)))
        (seq-markers '())
        (tempo-markers '())
        (count 0))
    (flet ((marker (time label &optional (colour 'white));white yellow blue red
             (list (incf count) (* time-scaler time) label colour)))
      (when seqs
        (loop for sec-ref in (get-section-refs sc 1 99999999)
              for section = (get-section sc sec-ref) do
                 (loop for i from 1 to (num-sequenzes section)
                       for seq = (get-sequenz section player1 i) do
                          (push (marker (start-time seq)
                                        (format nil "seq ~a" i))
                                seq-markers))))
      (when tempi 
        (map-over-bars sc 1 nil player1
                       #'(lambda (bar)
                           (let* ((first (first (rhythms bar)))
                                  (tempo (tempo-change first)))
                             (when tempo
                               (push (marker (start-time first)
                                             (format nil "tempo: ~a=~a"
                                                     (beat tempo) (bpm tempo))
                                             'yellow)
                                     tempo-markers))))))
      (append (nreverse seq-markers) (nreverse tempo-markers)))))
                 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken/reaper-play
;;; DESCRIPTION
;;; Using the sound files (samples) defined for the given reference (group ID)
;;; in the sndfile-palette slot of the slippery-chicken object, write a reaper
;;; files using the pitch and timing information of one or more players' parts
;;; from the slippery-chicken object.
;;;
;;; See clm-play for details of these related methods.
;;;
;;; One significant difference between this and clm-play is that reaper offers
;;; the possibility of transposing sound files via playback rate (like clm) and
;;; by pitch shift. Set :do-src 'transposition for the latter.
;;;
;;; ARGUMENTS
;;; - A slippery chicken object.
;;; - The ID of the starting section.
;;; - The IDs of the player(s) whose events are to be used to obtain the
;;;   rhythmic structure (and optionally, pitch content) of the resulting sound
;;;   file. This can be a single symbol for an individual player, a list of
;;;   player IDs, or NIL. If NIL, the event from all players' parts will be
;;;   reflected in the output file. Default = NIL.
;;; - The ID of the sound file group in the sndfile-palette slot of the
;;;   slippery-chicken object that contains the source sound files from which
;;;   the new sound file is to be generated.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :num-sections. An integer or NIL to indicate how many sections should be
;;;   generated, including the starting section. If NIL, sound file data will
;;;   be generated for all sections of the piece. NB If there are sub-sections
;;;   this will include them in the total, i.e., if section 1 has 3 subsections
;;;   and :num-sections is 2, we'll generate data from the first two
;;;   subsections of section 1, not all subsections of main sections 1 and
;;;   2. Default = NIL. 
;;; - :from-sequence. An integer that is the number of the first sequence
;;;   within the specified starting section to be used to generate the output
;;;   file. This argument can only be used when num-sections = 1. Default = 1. 
;;; - :num-sequences. NIL or an integer that indicates how many sequences are
;;;   to be generated, including that specified by :from-sequence. If NIL, all
;;;   sequences will be played. This argument can only be used when
;;;   num-sections = 1 and the section has no subsections. Default = NIL.
;;; - :srate. A number that is the sampling rate of the reaper project file
;;;   (independent of the input files). Default = 48000.
;;;   lead time of a specified number of seconds of silence prior to the sound
;;;   output. 
;;; - :inc-start. T or NIL to indicate whether playback of the source sound
;;;   files is to begin at incrementally increasing positions in those files or
;;;   at their respective 0.0 positions every time. If T, the method will
;;;   increment the position in the source sound file from which playback is
;;;   begun such that it reaches the end of the source sound file the last time
;;;   it is 'played'. T = increment start times. Default = NIL.
;;; - :ignore-rests. T or NIL to indicate whether silence should be
;;;   incorporated into the resulting sound file to correspond with rests in
;;;   the player's parts. If T, the sound files will play over the duration of
;;;   rests. However, this is only true on a bar-by-bar basis; i.e., notes at
;;;   the end of one bar will not be continued over into a rest in the next
;;;   bar. This implies that rests at the start of a bar will not be turned
;;;   into sounding notes. T = ignore resets. Default = T.
;;; - :sound-file-palette-ref2. The ID of a sound file group in the given
;;;   slippery-chicken object's sndfile-palette slot. If this reference is
;;;   given, the method will invoke fibonacci-transitions to transition from
;;;   the first specified group of source sound files to this one. If NIL, only
;;;   one group of source sound files will be used. Default = NIL.
;;; - :do-src. T, NIL, 'transposition, a number, or a note-name pitch symbol to
;;;   indicate whether transposition of the source sound files for playback will
;;;   be calculated such that the perceived fundamental frequencies of those
;;;   sound files are shifted to match the pitches of the current set or score
;;;   note. If do-src is a number (frequency in Hertz) or a note-name pitch
;;;   symbol, the method will use only that pitch instead of the sound files'
;;;   frequencies when transposing to the events' pitches. If :do-src
;;;   'transposition, then reaper's pitch-adjust will be used instead of
;;;   playback rate (default). NB Whichever is used, after being converted to a
;;;   sample rate conversion factor, this is always multiplied by the src-scaler
;;;   (see below). T = match sound files' frequencies to set pitches or score
;;;   pitches via playback rate (depending on :pitch-synchronous). NIL means no
;;;   transposition will be performed. Default = T.
;;; - :pitch-synchronous: T or NIL to indicate whether the source sound files
;;;   are to be transposed to match the pitches of the events in the given
;;;   players' part. This will only be effective if the given source sound file
;;;   has a perceptible frequency that has been specified using the sndfile
;;;   object's :frequency slot in the sndfile-palette. :do-src must also be T
;;;   for this to work. T = match pitches. Default = NIL.
;;; - :reset-snds-each-rs. T or NIL to indicate whether to begin with the first
;;;   source sound file of the specified group at the beginning of each
;;;   rthm-seq. T = begin with the first sound file. Default = T.
;;; - :reset-snds-each-player. T or NIL to indicate whether to begin with the
;;;   first source sound file of the specified group for the beginning of each
;;;   player's part. T = begin with the first sound file. Default = T. 
;;; - :play-chance-env. A list of break-point pairs that determines the chance
;;;   that a given event from the source player's part will be reflected in the
;;;   new sound file. It is determined by random selection but uses a fixed
;;;   seed that is re-initialized each time reaper-play is called. The following
;;;   default ensures every note will play: '(0 100 100 100).
;;; - :play-chance-env-exp. A number that will be applied as the exponent to
;;;   the play-chance-env's y values to create an exponential interpolation
;;;   between break-point pairs. Default = 0.5.
;;; - :max-start-time. A number that is the last time-point in seconds for
;;;   which events will be processed for the output file. If a maximum start
;;;   time is specified here (in seconds), events after this will be
;;;   skipped. The default value of 99999999 seconds (27778 hours) will result
;;;   in all events being reflected in the sound file.
;;; - :time-scaler. A number that will be the factor by which all start times
;;;   are scaled for the output file (in effect a tempo scaler). If
;;;   :ignore-rests is T, this will also have an indirect effect on
;;;   durations. This argument should not be confused with
;;;   :duration-scaler. Default = 1.0.
;;; - :duration-scaler. A number that is the factor by which the duration of
;;;   all  events in the output sound file will be scaled. This does not alter
;;;   start times, and will therefore result in overlapping sounds if greater
;;;   than 1.0. This is not to be confused with :time-scaler. 
;;;   Instead of a simple number, :duration-scaler can also be an envelope 
;;;   (a list of break-point pairs). This means you could smoothly transition
;;;   from staccato-like to overlapping sounds in a single reaper-play call.
;;;   Default = 1.0.
;;; - :src-scaler: A number that is the factor by which all sample-rate
;;;   conversion values will be scaled (for increasing or decreasing the
;;;   transposition of the overall resulting sound file). 
;;;   Instead of a simple number, :src-scaler can also be an envelope 
;;;   (a list of break-point pairs), similar to :duration-scaler. Default = 1.0.
;;; - :note-number. A number that is an index, representing the the nth pitch
;;;   of the current set or chord (from the bottom) to be used for the lowest
;;;   player. Default = 0.
;;; - :duration-run-over. T or NIL to indicate whether the method will allow a
;;;   sound file event to extend past the end of specified segment boundaries
;;;   of a sound file in the sndfile-palette. T = allow. Default = NIL.
;;; - :short-file-names. T or NIL to indicate whether abbreviated output file
;;;   names will be automatically created instead of the usually rather long
;;;   names. T = short. Default = NIL.
;;; - :output-name-uniquifier. A user-specified string that will be
;;;   incorporated into the file name, either at the end or the beginning
;;;   depending on whether short-file-names is T or NIL. Default = "".
;;; - :check-overwrite. T or NIL to indicate whether to query the user before
;;;   overwriting existing sound files. T = query. Default = T.
;;; - :sndfile-palette. NIL or a file name including path and extension that
;;;   contains an external definition of a sndfile-palette. This will replace
;;;   any sndfile-palette defined in the slippery-chicken object. If NIL, the
;;;   one in the slippery-chicken object will be used. Default = NIL.
;;; - :chords. NIL or a list of lists consisting of note-name symbols to be
;;;   used as the pitches for the resulting sound file in place of the pitches
;;;   from the set-map. There must be one chord specified for each sequence. If
;;;   NIL, the pitches from the set-map will be used. Default = NIL.
;;; - :chord-accessor. Sometimes the chord stored in the palette is not a
;;;   simple list of data so we need to access the nth of the chord
;;;   list. Default = NIL.
;;; - :pan-min-max. Each event is panned randomly between two channels. Pan
;;;   values are in degrees where 0 is channel 1 (left in stereo files) and 90
;;;   is channel 2 (right). The values used are chosen from the following list
;;;   of degrees: (15 25 35 45 55 65 75). However, these can be scaled to be
;;;   within new limits by passing a two-element list to
;;;   :pan-mix-max. E.g. '(40 50) would limit most values to the middle area
;;;   of stereo space. Note that no matter how many channels a sound file or
;;;   channel has, the item's panner is set as if it were stereo. Also that,
;;;   for convenience, :min-channels and :max-channels can set the tracks'
;;;   channel count, it is up to the user to take care of routing and,
;;;   eventuallay, panning depending on the number of sound file and track
;;;   channels used in the mix. Default = '(5 85).
;;; - :pan-fun. If you want to take charge of selecting pan positions yourself,
;;;   pass a function via this keyword. The function must take one argument: the
;;;   current event, though of course, it can ignore it completely if
;;;   preferred. The function should return a degree value: a number between 0
;;;   and 90. Default = NIL.
;;; - :min-channels and :max-channels 4. Set the reaper tracks' minimum and
;;;   maximum channel counts. See these named slots in the reaper-track class
;;;   definition and the cond in create-tracks for details of how these are
;;;   handled.
;;; - :snd-selector. By default the sound files in the given group are cycled
;;;   through, one after the other, returning to the beginning when the end is
;;;   reached. This can be changed by passing a function to :snd-selector. This
;;;   function must take three arguments: the circular-sclist object that
;;;   contains the group's sndfile objects (the sndfiles are in a simple list
;;;   in the data slot); the pitch of the current event (if the event happens
;;;   to be a chord, each pitch of the chord will be handled separately
;;;   i.e. the :snd-selector function will be called for each pitch); and the
;;;   event object. Of course any of these arguments may be ignored by the
;;;   :snd-selector function; indeed it is imagined that the event object will
;;;   be mostly ignored and the pitch object used instead, but it is
;;;   nevertheless passed for completeness. For example, the following
;;;   function, declared on-the-fly using lambda rather than separately with
;;;   defun, makes reaper-play act as a traditional sampler would: Assuming our
;;;   sndfile-palette group has a list of sound files whose frequencies really
;;;   correspond to their perceived fundamentals (e.g. see the
;;;   set-frequency-from-filename method, and the kontakt-to-sfp function) we
;;;   choose the nearest sound file in the group to the frequency of the
;;;   current event's pitch:
;;;
;;;            :snd-selector #'(lambda (sflist pitch event)
;;;                              (declare (ignore event))
;;;                              (get-nearest-by-freq
;;;                               (frequency pitch) (data sflist)))
;;;
;;;   NB If :pitch-synchronous is NIL, then the pitch-or-chord slot of the event
;;;   will be the pitch selected from the current set, rather than the actual
;;;   pitch of the instrument in question.
;;;   Default = NIL i.e. use the circular selection method.
;;; - :snd-transitions. Set the transition from sound-file-ref to sound-file-ref
;;;   over the course of the call to reaper-play using a custom envelope instead
;;;   of the default fibonacci-transitions method, e.g. '(0 0 100 1). x-axis is
;;;   arbitrary, y-axis should range from 0 to 1. When reading, y-values will be
;;;   rounded.
;;; - :file. reaper file name, if you don't want this to be auto-generated.
;;;   Automatically generated file names will be placed in the default-dir as
;;;   defined in the sc-config. A full path is expected. Default = NIL =
;;;   automatic. 
;;; - :fade-in :fade-out. The fade times expressed as a percentage of the
;;;   duration of any given event/item. Default = 10%
;;; - :tracks-per-player. How many reaper tracks to use per player. If > 1
;;;   then the tracks will be used one after the other, in rotation, for each
;;;   event. Particularly useful if there are lots of overlapping events due to
;;;   e.g. :duration-scaler, or chords, of course. Default = 1.
;;; - :items-processor. A function to call to process the list of reaper-item
;;;   objects just before writing the reaper file. This should take one
;;;   argument, the list of reaper-items, and return a list of reaper-items that
;;;   will be written to the file. Default = NIL = no processing.
;;; - :markers. Whether to write markers at sequenz and tempo changes. This
;;;   should be a list of two booleans: whether to write sequenz and/or
;;;   tempi. NB This only works (for now) if you're generating the whole
;;;   piece. Default = '(t t) 
;;; 
;;; RETURN VALUE
;;; Total events generated (integer).
;;; 
;;; EXAMPLE
#|
;;; using playback speed to effect the transpositions:
(reaper-play +second-law+ 1 nil 'source-sndfile-grp-1
             :check-overwrite nil :tracks-per-player 2
             :pitch-synchronous t)

;;; using reaper's pitch adjust and keeping playback speed implicitly to 1.0
(reaper-play +second-law+ 1 nil 'source-sndfile-grp-1
             :check-overwrite nil :tracks-per-player 2
             :pitch-synchronous t :do-src 'transposition)

|#
;;; SYNOPSIS
(defmethod reaper-play ((sc slippery-chicken) section players 
                        sound-file-palette-ref 
                        &key 
                        sound-file-palette-ref2
                        (play-chance-env '(0 100 100 100))
                        (max-start-time 99999999)
                        (play-chance-env-exp 0.5)
                        (time-scaler 1.0)
                        (from-sequence 1)
                        (num-sequences nil)
                        (num-sections nil)
                        (ignore-rests t)
                        (time-offset 0.0)
                        (chords nil)
                        (chord-accessor nil)
                        (note-number 0)
                        (fade-in 10)
                        (fade-out 10)
                        (inc-start nil)
                        ;; either a number or an envelope
                        (src-scaler 1.0)
                        (do-src t)
                        (pitch-synchronous nil)
                        ;; either a number or an envelope
                        (duration-scaler 1.0)
                        (short-file-names nil)
                        ;; by default we generate filenames according to players
                        ;; and other relevent data but if this is given then
                        ;; we'll use it instead 
                        (file nil)
                        (check-overwrite t)
                        (reset-snds-each-rs t)
                        (reset-snds-each-player t)
                        (duration-run-over nil)
                        (srate 48000)
                        (output-name-uniquifier "")
                        (sndfile-palette nil)
                        (tracks-per-player 1)
                        ;; MDE Sat Oct  3 18:45:28 2015 -- for Cameron!
                        pan-fun
                        ;; MDE Thu Oct  1 21:03:59 2015
                        (pan-min-max '(5 85))
                        ;; MDE Wed Feb 21 13:21:00 2024, Heidhausen
                        items-processor
                        ;; MDE Thu Feb 22 11:40:43 2024, Heidhausen
                        (markers '(t t))
                        ;; MDE Thu Oct  1 19:13:49 2015
                        snd-selector
                        (min-channels 2) (max-channels 4)
                        snd-transitions)
;;; ****                               
  (when (and (numberp num-sequences)
             (has-subsections (get-section sc section)))
    (error "slippery-chicken::reaper-play: :num-sequences (~a) should only be ~
            specified ~%for a section (~a) with no subsections."
           num-sequences section))
  ;; if there's only one section
  (when (and (not num-sections)
             (= section 1)
             (= 1 (get-num-sections sc)))
    (setq num-sections 1))
  ;; if we're playing more than one section then we shouldn't specify
  ;; num-sequences as that might result in gaps in playback (e.g. if section 2
  ;; had more seqs than requested)
  (when (and num-sequences 
             (or (not num-sections)
                 (and num-sections (> num-sections 1))))
    (error "slippery-chicken::reaper-play: num-sequences keyword should only ~
            be used ~%when num-sections = 1."))
  (when (and from-sequence (/= 1 from-sequence)
             (or (not num-sections)
                 (and num-sections (> num-sections 1))))
    (error "slippery-chicken::reaper-play: from-sequence keyword should only ~
            be used ~%when num-sections = 1."))
  (unless (listp players)
    (setq players (list players)))
  ;; MDE Mon Apr  2 09:34:36 2012 
  (unless players
    (setq players (players sc)))
  ;; re-initialise our random number generator.
  (random-rep 100 t)
  ;; carried over from clm-play: LMF Mon Jul 19 2021
  ;; the argument to duration-scaler can be either a number or an envelope.
  ;; When it is the former, it will now be converted to an envelope.
  (when (atom duration-scaler)
    (setq duration-scaler (list 0 duration-scaler 100 duration-scaler)))
  ;; same for src-scaler
  (when (atom src-scaler)
    (setq src-scaler (list 0 src-scaler 100 src-scaler)))
  ;; carried over from ckm-play: DJR Thu  5 Sep 2019 12:42:07 BST
  ;; (force-list) the soundfile-palette-refs so we can use multiple refs later.
  (setq sound-file-palette-ref (force-list sound-file-palette-ref)
        sound-file-palette-ref2 (force-list sound-file-palette-ref2)
        ;; convert percentages to scaler
        fade-in (/ fade-in 100)
        fade-out (/ fade-out 100))
  (let* ((events (get-events-with-src sc section players 
                                      ;; these have 0 duration so we must ignore
                                      ;; them for now 
                                      :ignore-grace-notes t
                                      :time-scaler time-scaler
                                      :from-sequence from-sequence
                                      :num-sequences num-sequences
                                      :num-sections num-sections
                                      :ignore-rests ignore-rests
                                      :chords chords
                                      :pitch-synchronous pitch-synchronous
                                      :chord-accessor chord-accessor
                                      :note-number note-number))
         (section1-num-seqs (if num-sequences
                              num-sequences
                              (num-seqs sc section)))
         (num-players (length players))
         (events-per-player (ml 0 num-players))
         ;; clisp doesn't like (loop for player in events sum (loop for rs ...
         (total-events (loop 
                         for i from 0
                         for player in events
                         for len = (loop for rs in player sum (length rs))
                         do (setf (nth i events-per-player) len)
                         sum len))
         ;; carried over from ckm-play: DJR Thu  5 Sep 2019 12:41:00 BST
         ;; Update snds and snds2 to accept multiple sound-file-palette-refs
         (snds (get-snds-from-palette sc sound-file-palette-ref
                                      sndfile-palette))
         (snds2 (get-snds-from-palette sc sound-file-palette-ref2
                                       sndfile-palette))
         (sndl nil)
         (snd-group nil)
         (srts '())
         (freqs '())
         (pitch-adjust (eq do-src 'transposition))
         ;; will be nil when we're using the events' pitch data
         (srt-freq (cond ((numberp do-src) do-src)
                         ((and do-src
                               (not (eq do-src t))
                               (not pitch-adjust)
                               (symbolp do-src))
                          (note-to-freq do-src))))
         ;; MDE Thu Oct  1 21:07:00 2015
         (pan-vals '(15 25 35 45 55 65 75))
         (duration 0.0)
         (wanted-duration 0.0)
         (wanted-duration-string "")
         (first-event-start nil)
         (input-start 0.0)
         (latest-possible-start 0.0)
         (available-dur 0.0)
         (event-count 1)
         (event-count-player 0)
         (events-this-rs 0)
         (output-start 0.0)
         (output-ok t)
         (this-play-chance-env '())
         (this-duration-scaler-env '())
         (this-src-scaler-env '())
         (events-before-max-start 0)
         (skip-this-event t)
         (total-skipped 0)
         (file-name
           (string-downcase
            (cond
              (file file)
              (short-file-names
               (format nil "~{~a-~}~a~{~a-~}~{~a.~}~a-~a~a~a.rpp"
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
                       (if pitch-synchronous "-psync" "")))
              (t (format nil
                         "~a~a~{-~a~}~{-~a~}~{-~a~}~{-to-~a~}-seq~a-~a~a.rpp"
                         output-name-uniquifier
                         ;; MDE Tue Feb 20 10:26:55 2024, Heidhausen -- now
                         ;; use the title, if it was explicitly given
                         (if (string= (title sc) "slippery chicken")
                           (string-trim "+" (id sc))
                           (filename-from-title (title sc)))
                         (if (listp section) section (list section))
                         players
                         (if (listp sound-file-palette-ref) 
                           sound-file-palette-ref
                           (list sound-file-palette-ref))
                         (when sound-file-palette-ref2
                           (if (listp sound-file-palette-ref2) 
                             sound-file-palette-ref2
                             (list sound-file-palette-ref2)))
                         from-sequence 
                         (+ -1 from-sequence section1-num-seqs)
                         (if pitch-synchronous "-psync" ""))))))
         (output 
           (unless file
             ;; first convert spaces to -'s in output file name
             (setq file-name (substitute #\- #\Space file-name))
             (default-dir-file file-name)))
         ;; keep going (set to nil when max-start-time is exceeded)
         (happy t)
         (amp 1.0)
         (reaper-items '())
         (player-string "")
         (player-strings nil)
         (rthm-seqs nil))
    ;; reaper file's VOLPAN line looks something like this
    ;; VOLPAN 0.745098 -0.54 0.152439 -1
    ;;
    ;; 1) draggable item volume (slot: item-vol), then media item properties: 
    ;; 2) pan (-1=L +1-R) (slot: pan), 
    ;; 3) vol slider (slot: slider-vol)
    ;; 4) unknown (with items seems to always be -1 and with tracks +1)
    ;; Volumes are linear amp (so +6db = *2). Need to update reaper-item.txt to
    ;; include these 3 params and also the reaper-item class and write-item
    ;; method to reflect them
    ;; 
    ;; NB this will be pointless if we've passed a pan-fun
    (when pan-min-max
      (setq pan-vals (loop for p in pan-vals
                           collect
                           (fscale p 15 75 (first pan-min-max)
                                   (second pan-min-max)))))
    ;; now we have degrees but still have to convert to -1.0 to 1.0 whether
    ;; pan-min-max was given or not 
    (setq pan-vals (loop for p in pan-vals
                         collect        ; convert degrees to -1.0 -> 1.0
                         (rescale p 0 90 -1.0 1.0)))
    (when (and sound-file-palette-ref (zerop (sclist-length snds)))
      (error "slippery-chicken::reaper-play: <snds>: No sounds for reference ~a"
             sound-file-palette-ref))
    (when (and sound-file-palette-ref2 (zerop (sclist-length snds2))
               (error "slippery-chicken::reaper-play: <snds2>: ~
                       No sounds for reference ~a"
                      sound-file-palette-ref2)))
    ;; DJR Mon 16 Sep 2019 01:26:11 BST
    ;; are we setting snd-transitions by default with fibonacci-transitions or
    ;; with a custom envelope?
    (if (and snd-transitions sound-file-palette-ref2)
      (setq snd-transitions
            (loop for num-events in events-per-player
                  collect
                  (loop with sts = (new-lastx snd-transitions num-events)
                        for n below num-events collect
                        (round
                         (interpolate n sts)))))
      (setq snd-transitions (loop for num-events in events-per-player
                                  collect (fibonacci-transition num-events))))
    (when (and check-overwrite (probe-file output))
      (setf output-ok 
            (yes-or-no-p "File exists: ~%~a  ~%Overwrite (yes or no) > " 
                         output)))
    ;; this is where we work out how many times we'll use the sndfiles for when
    ;; inc-start is T 
    (when output-ok
      (format t "~%Output file will be ~%\"~a\"~%~%" output)
      (when inc-start
        (loop for snd in (data snds) do (reset-usage snd))
        (when snds2
          (loop for snd in (data snds2) do (reset-usage snd)))
        (loop for player in events and snd-trans in snd-transitions do
          (setf snd-trans (copy-list snd-trans))
          (loop for rs in player do
            (loop 
              for event in rs 
              for sndlist =
                          (when snds
                            (get-sndfiles-from-user-fun
                             event
                             (if (and snds2 (= 1 (pop snd-trans)))
                               snds2 snds)
                             snd-selector))
              do
                 (loop
                   for snd in sndlist do
                     (unless snd
                       (error "slippery-chicken:: reaper-play: ~
                                           snd is nil (whilst counting)!"))
                     (incf (will-be-used snd))))))
        ;; here we reset them before starting, this is correct!
        (reset snds)
        (when snds2 (reset snds)))
      ;; MDE Tue Apr 17 18:53:36 2012 -- get the lowest start time of all the
      ;; players. events only includes sounding events here, not rests.
      (setq first-event-start 
            (loop
              for player in events
              ;; MDE Fri Jan  4 17:43:44 2019 -- bug fix: single players with
              ;; bars-rest at beginning don't pick up the minimum start time
              ;; unless we remove nils and flatten 
              ;; for ffv = (print (first (first player)))
              for ffv = (first (remove-if-not #'event-p (flatten player)))
              if ffv minimize (start-time ffv)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; here we go! ;;;;;;
      (loop
        for player in events and player-name in players and snd-trans in
        snd-transitions and player-count from 1
        do
           (setq player-string (string-downcase (string player-name))
                 player-strings (make-cscl
                                 (loop for i from 1 to tracks-per-player
                                       collect (format nil "~a:~a"
                                                       player-string i)))
                 snd-trans (copy-list snd-trans)
                 event-count-player 0
                 ;; 15/12/06 reset happy to the new player processes
                 happy t
                 events-before-max-start
                 (count-events-before-max-start 
                  player (- (+ max-start-time first-event-start)
                            time-offset))
                 this-play-chance-env 
                 (new-lastx play-chance-env
                            ;; 10/1/07 we want to use the whole
                            ;; play-chance-env when we use max-start-time:
                            ;; (1- events-this-player)))
                            ;; got to take time-offset and the start time of
                            ;; the first event into consideration, not just
                            ;; max-start-time...
                            events-before-max-start)
                 ;; LMF Mon Jul 19 2021 -- same as this-play-chance-env
                 this-duration-scaler-env
                 (new-lastx duration-scaler events-before-max-start)
                 this-src-scaler-env
                 (new-lastx src-scaler events-before-max-start))
           (format t "~%Processing player ~a/~a: ~a (resting players will ~
                      not be processed)~%"
                   player-count num-players (nth (1- player-count) players))
           (when (and (numberp num-sections) (= 1 num-sections))
             ;; MDE Tue Apr 3 09:54:46 2012 -- make sure we don't crash
             ;; if the requested instrument is sitting this section out
             (let ((rss (get-data-from-palette
                         (flatten (list section player-name))
                         (rthm-seq-map sc)
                         nil)))         ; no warning
               ;; this code will only work when we're processing 1 section
               (setf rthm-seqs 
                     (when rss
                       (subseq 
                        rss
                        (1- from-sequence)
                        (1- (+ from-sequence section1-num-seqs)))))))
           (when reset-snds-each-player
             (when snds (reset snds))
             (when snds2 (reset snds2)))
           (loop
             for rs in player and rs-count from 0 while happy do
               (setf events-this-rs (length rs))
               (format t "~%    Processing rthm-seq ~a (~a events)~%"
                       ;; print the rthm-seq id if we're only doing one
                       ;; section otherwise the rthm-seq count MDE Tue Apr 3
                       ;; 09:54:46 2012 -- make sure we don't crash if the
                       ;; requested instrument is sitting this section out
                       ;; 
                       ;; MDE Wed Feb 24 19:54:51 2016 -- now rs is the
                       ;; list of events but we need to know the id of
                       ;; the original rthm-seq hence the nth. In any
                       ;; case the following test was wrong; instead we
                       ;; can just see if we've got a list of events in
                       ;; rs
                       (let ((tmp (when (and rthm-seqs rs)
                                    (nth rs-count rthm-seqs))))
                         (if tmp 
                           (id tmp)
                           (1+ rs-count)))
                       events-this-rs)
               (when reset-snds-each-rs
                 (when snds (reset snds))
                 (when snds2 (reset snds2)))
               (loop
                 for event in rs and rs-event-count from 0
                 while happy
                 do
                    (setq snd-group (pop snd-trans)
                          ;; MDE Mon Nov  4 11:11:07 2013 
                          freqs (let ((f (frequency
                                          (pitch-or-chord event))))
                                  (if (listp f) f (list f)))
                          sndl (if snds
                                 ;; MDE Fri Oct  2 09:48:39 2015 
                                 (get-sndfiles-from-user-fun
                                  event
                                  (if (and snds2 (= 1 snd-group))
                                    snds2 snds)
                                  snd-selector)
                                 ;; MDE Tue Aug 8 16:21:34 2017
                                 ;; -- anything so long as we can
                                 ;; make the loop below work
                                 (ml nil (length freqs)))
                          duration (* (interpolate
                                       event-count-player
                                       this-duration-scaler-env)
                                      (compound-duration-in-tempo
                                       event))
                          skip-this-event 
                          (unless (zerop events-before-max-start)
                            (> (random-rep 100.0)
                               (interpolate event-count-player 
                                            this-play-chance-env
                                            :exp
                                            play-chance-env-exp)))
                          srts (if do-src
                                 (src-for-sample-freq ; returns a list
                                  (if srt-freq
                                    srt-freq
                                    (if
                                        (and sndl
                                             (not
                                              (every #'not sndl)))
                                      sndl 261.626))
                                  event)
                                 '(1.0)))
                    (loop
                      for srt in srts and freq in freqs
                      and snd in sndl
                      do
                         (setq srt (* (interpolate
                                       event-count-player
                                       this-src-scaler-env)
                                      srt)
                               amp (if snd (amplitude snd) 1.0))
                         (when (<= srt 0.0)
                           (error "slippery-chicken:: reaper-play: illegal ~
                                    sample rate conversion: ~a"
                                  srt))
                         ;; MDE Mon Apr  9 12:31:07 2012
                         (when snd
                           (unless (duration snd)
                             (error "~a~%slippery-chicken:: reaper-play: ~
                                      sound duration is NIL!"
                                    snd)))
                         ;; given the srt, what's the longest output dur this
                         ;; sound can make?
                         (setq available-dur 
                               (if snd
                                 ;; handle transpostion vs. play-rate here
                                 (if pitch-adjust
                                   (duration snd)
                                   (/ (duration snd) srt))
                                 most-positive-short-float)
                               wanted-duration-string ""
                               input-start
                               (if snd (start snd) 0.0))
                         (when skip-this-event
                           (incf total-skipped))
                         (when inc-start
                           (unless snd
                             (error "~%slippery-chicken:: reaper-play: ~
                                      can't do inc-start with no ~
                                      sndfile-palette."))
                           ;; handle transpostion vs. play-rate here
                           (setq latest-possible-start
                                 (- (end snd) (if pitch-adjust
                                                duration
                                                (* srt duration))))
                           (unless (and (< latest-possible-start
                                           (start snd))
                                        (not (zerop
                                              (will-be-used snd))))
                             (incf input-start
                                   (* (has-been-used snd)
                                      (/
                                       (- latest-possible-start
                                          (start snd))
                                       (will-be-used snd)))))
                           (incf (has-been-used snd)))
                         ;; MDE Thu Feb 29 09:32:24 2024, Heidhausen -- to be
                         ;; sure  
                         (setq input-start (max 0.0 (min latest-possible-start
                                                         input-start)))
                         ;; (when (>= input-start (end snd))
                         ;; (print latest-possible-start)
                         ;; (break))
                         (when (> duration available-dur)
                           (setq wanted-duration duration
                                 wanted-duration-string 
                                 (if duration-run-over
                                   (format nil " (~,3f available but ~
                                                    duration-run-over is t)"
                                           available-dur)
                                   (format nil " (wanted ~,3f)"
                                           wanted-duration)))
                           (unless duration-run-over
                             (setq duration available-dur)))
                         (when (< duration 0)
                           (warn "slippery-chicken::reaper-play: ~
                                   Duration < 0  ?????~%"))
                         (unless (start-time event)
                           (error "~a~%slippery-chicken::reaper-play: ~
                                    no start time!!!" event))
                         (setq output-start (+ time-offset
                                               (- (start-time event)
                                                  first-event-start)))
                         (when (> output-start max-start-time)
                           (setq happy nil))
                         (when happy
                           (format t "        ~a/~a Events: ~a~
                                 ~%             ~a ~a~
                                 ~%             start-time ~,3f, input-start: ~
                                 ~,3f, ~%             duration ~,3f~a, ~
                                 ~%             amp ~,2f, srt ~,2f ~
                                 (pitch-or-chord ~,3fHz, sample freq ~,3f)~%"
                                   event-count total-events
                                   (if skip-this-event "Skipped" 
                                     "Written (not skipped)")
                                   (if snd (path snd) "")
                                   (if snds2
                                     (format nil "(snd-group ~a)" 
                                             (1+ snd-group))
                                     "")
                                   output-start input-start duration
                                   wanted-duration-string amp srt 
                                   ;; MDE Tue Apr 17 13:14:45 2012 -- added
                                   ;; frequency method to chord also so that
                                   ;; this doesn't fail
                                   (frequency (pitch-or-chord event))
                                   (if snd (frequency snd) "n/a")))
                         (unless (or skip-this-event (not happy)
                                     (zerop duration))
                           (let ((ri (make-reaper-item-fast
                                      (list 'path (path snd)
                                            'fade-in (* duration fade-in)
                                            'fade-out (* duration fade-out)
                                            'duration duration
                                            'start input-start
                                            ;; in this method by default we
                                            ;; simulate the sampling-rate
                                            ;; conversion method of CLM where a
                                            ;; change of 'speed' is accompanied
                                            ;; by a change in pitch. But we
                                            ;; also allow a change of pitch
                                            ;; without a change in speed. This
                                            ;; necessitates the 3rd arg
                                            ;; (semitones) to the PLAYRATE line
                                            ;; in the .rpp file and setting
                                            ;; this to the transposition given
                                            ;; to the reaper-item class
                                            'play-rate (if pitch-adjust
                                                         1.0
                                                         srt)
                                            'transposition (if pitch-adjust
                                                             (srt srt)
                                                             0.0)
                                            'preserve-pitch nil
                                            'channels (channels snd)
                                            'start-time output-start
                                            'item-vol amp
                                            'track (get-next player-strings)
                                            'pan (if pan-fun
                                                   (rescale
                                                    (funcall pan-fun event)
                                                    0 90 -1.0 1.0)
                                                   (nth (random 7)
                                                        pan-vals))))))
                             ;; MDE Thu Jun 27 15:15:30 2024, Heidhausen
                             (set-name-from-path ri)
                             (push ri reaper-items))))
                    (incf event-count-player)
                    (incf event-count)))))
    ;; MDE Wed Feb 21 13:18:49 2024, Heidhausen -- process the reaper-items, if
    ;; desired, before writing the file
    (setq reaper-items (reverse reaper-items))
    (when items-processor
      (setq reaper-items (funcall items-processor reaper-items)))
    ;; (print (first reaper-items))
    (write-reaper-file (make-reaper-file (id sc) reaper-items
                                         :sample-rate srate)
                       :min-channels min-channels
                       :markers (apply #'get-reaper-markers
                                       (cons sc (cons time-scaler markers)))
                       :max-channels max-channels
                       :file output)
    (unless (zerop total-events)
      (format t "~%~%~d/~d events skipped (~f%)"
              total-skipped total-events 
              (* 100.0 (/ total-skipped total-events))))
    (values total-events file-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken/get-last-bar
;;; DATE
;;; September 28th 2020, Heidhausen
;;; 
;;; DESCRIPTION
;;; Get the last bars for each of the players
;;; 
;;; ARGUMENTS
;;; - the slippery-chicken object
;;; 
;;; RETURN VALUE
;;; a list of rthm-seq-bar objects
;;; 
;;; SYNOPSIS
(defmethod get-last-bar ((sc slippery-chicken))
;;; ****
  (get-bar sc (num-bars sc)))

(defmethod get-last-bar-for-player ((sc slippery-chicken) player)
  (get-bar sc (num-bars sc) player))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken/get-last-event
;;; DATE
;;; September 28th 2020, Heidhausen
;;; 
;;; DESCRIPTION
;;; Get the last events (of the last bars) for each of the players
;;; 
;;; ARGUMENTS
;;; - the slippery-chicken object
;;; 
;;; RETURN VALUE
;;; a list of event objects
;;; 
;;; SYNOPSIS
(defmethod get-last-event ((sc slippery-chicken))
;;; ****
  (mapcar #'get-last-event (get-last-bar sc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Just calls the -aux method once for each section required. See comments to
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
     ;; MDE Fri May 11 16:18:51 2012 -- note from rsb::get-timings: 8/3/07:
     ;; ignore-rests means the duration of rests will be added to the duration
     ;; of the previously struck note. include-rests will make sure rests are
     ;; collected into the result. This is somewhat confusing but we need
     ;; rests when generating midi files (they might have program changes on
     ;; them).
     (include-rests nil))
  ;; MDE Sat Dec 17 10:16:25 2011 -- when running cheat-sheet.lsp I was getting
  ;; some invalid data in duration-in-tempo, compound-duration-in-tempo,
  ;; end-time slots; this fixed it
  (update-slots sc)
  (unless num-sections
    (setf num-sections (get-num-sections sc)))
  (let* ((sections (get-section-refs sc start-section num-sections))
         (all-sections
          (loop for section in sections
              collect (get-events-start-time-duration-aux
                       sc section voices 
                       :ignore-grace-notes ignore-grace-notes 
                       :time-scaler time-scaler
                       ;; of course the next two shouldn't be necessary when
                       ;; we're doing more than one section ...
                       :from-sequence from-sequence
                       :num-sequences num-sequences
                       :get-time-sig-changes get-time-sig-changes
                       :ignore-rests ignore-rests
                       :include-rests include-rests)))
         (voices (loop for i below (length voices) collect
                       (loop for j below num-sections
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
  ;; MDE Fri May 11 12:19:10 2012 -- 
  (unless (integer>0 from-sequence)
    (error "slippery-chicken::get-events-start-time-duration-aux: ~
            from-sequence must be an integer >= 1."))
  (let* ((section-list (if (listp section) section (list section)))
         (seqs-all-ins (data (get-data section-list (piece sc)))))
    ;; (print num-sequences)
    (loop for voice in voices 
       for sqces = (data (get-data voice seqs-all-ins t))
       ;; no longer call get-timings-from-rsp rather get the
       ;; timings directly from the bars of the sequenzes, not
       ;; using a tempo argument any more but using a time-scaler
       ;; instead. Now that we're using the piece to get the seqs
       ;; from instead of the rthm-seq-map, we should have updated
       ;; time information. What we have to do though is scale
       ;; the times and durations of each event and add the
       ;; durations of rests to the previous note if ignore-rests
       ;; is t.
       for events = 
       (loop for i from (1- from-sequence) to 
            (if num-sequences
                (+ (1- from-sequence)
                   (1- num-sequences))
                (1- (length sqces)))
          ;; MDE Fri May 11 12:28:07 2012 -- 
            for sqc = (nth i sqces)
            do 
            (unless sqc
              (error "slippery-chicken::get-events-start-time-duration-aux: ~
                      no sequence number ~a. ~%Perhaps your from-sequence or ~
                      num-sequences argument is wrong." (1+ i)))
            collect (get-timings sqc time-scaler ignore-rests
                                 get-time-sig-changes include-rests 
                                 ignore-grace-notes))
         ;; MDE Mon Apr 21 18:32:16 2014 -- we were checking for (first events)
       ;; but that meant a rest sequence at the beginning of a section would
       ;; result in no events being returned for the whole section! Now we just
       ;; make sure we've got a list with an event in it instead of a list of
       ;; nils. 
       when (not (every #'not events))
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
     ;; if chords is nil then we use the chords in the set-map
     (chords nil)
     ;; MDE Tue Apr 17 12:21:07 2012 -- whether to use the pitches of the score
     ;; events or the old method of player by player set pitch access
     (pitch-synchronous nil)
     ;; sometimes the chord stored in the palette is not a simple list of
     ;; data so we need to access the nth of the chord list
     (chord-accessor nil)
     ;; once we get to the chord, we have a list of notes, now we need the
     ;; reference to the specific note for the voices in this note list.
     ;; Either this should be single reference whereupon the first voice
     ;; will be this nth, the second this plus 1 etc., or this should be a
     ;; list.
     (note-number 0)) ;; 0-based!!!
  ;; MDE Mon Apr 16 21:44:19 2012
  (unless num-sections
    (setf num-sections (get-num-sections sc)))
  (let* ((sections (get-section-refs sc from-section num-sections))
         (all-sections
          (loop for section in sections
             collect (get-events-with-src-aux
                      sc section voices 
                      :ignore-grace-notes ignore-grace-notes
                      :time-scaler time-scaler
                      :pitch-synchronous pitch-synchronous
                      :from-sequence from-sequence
                      :num-sequences num-sequences
                      :ignore-rests ignore-rests
                      :chords chords
                      :chord-accessor chord-accessor
                      :note-number note-number))))
    ;; (print 'here)
    (loop for i below (length voices) collect
         (loop for j below num-sections
            for data = (nth i (nth j all-sections))
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
     (pitch-synchronous nil)
     (ignore-rests nil)
     (chords nil)
     (chord-accessor nil)
     (note-number 0)) ;; 0-based!!!
  (unless num-sequences
    (setf num-sequences (num-seqs sc section)))
  ;; MDE Tue Apr 17 12:22:55 2012 -- 
  (when (and pitch-synchronous (or chords chord-accessor))
    (error "slippery-chicken::get-events-with-src-aux: pitch-synchronous ~
            cannot be used in conjunction with chords or chord-accessor."))
  (let ((timings (get-events-start-time-duration-aux ; clones the events 
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
         (if chords 
             chords
             (let ((chord-refs 
                    (subseq
                     (data (get-data section (set-map sc)))
                     (1- from-sequence)
                     (1- (+ from-sequence num-sequences)))))
               (loop for ref in chord-refs collect
                    ;; MDE Thu Sep  6 14:01:59 2018 -- bear in mind this won't
                    ;; use the new transposition-curve slot  
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
    ;; MDE Wed Apr 11 12:36:13 2012 -- so by default we create pitch data for
    ;; each voice that simply accesses the notes in the chord from the bottom
    ;; up, one voice after another.
    ;; MDE Tue Apr 17 12:24:18 2012 -- don't do this if pitch-synchronous
    (unless pitch-synchronous
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
                 for pitch = (nth (mod n num-notes) chord)
                 do
                 ;; (print-simple event)
                 (unless pitch
                   (error "slippery-chicken::get-events-with-src: ~%~
                         Pitch is NIL!!!  Probably the reference ~
                         given in :note-number is out of ~%range for ~
                         the chosen chord. ~%Current reference is ~
                         ~a into the chord ~a"  
                          n chord))
                 ;; MDE Mon Apr 16 22:03:55 2012 -- remember these events have
                 ;; been cloned so we won't be affecting score output by
                 ;; changing the pitch here.
                 ;; MDE Fri May 11 14:23:25 2012 -- make a pitch object if
                 ;; we're given symbols 
                 (setf (pitch-or-chord event) 
                       (if (pitch-p pitch)
                           (clone pitch)
                           (make-pitch pitch)))))))
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
;;; 10/3/07: the rehearsal-letters slot lists the bar numbers where
;;; cmn/lilypond should automatically create letters. Set the rehearsal-letter
;;; slot of the correct bars for the given player(s).
;;; e.g. (set-rehearsal-letters sc (get-groups-top-ins sc)))) 
;;;
;;; MDE Fri Oct 11 17:10:46 2013 -- allow a list of (bar-num letter/id) pairs
;;; so that we can hand-set them.

(defmethod set-rehearsal-letters ((sc slippery-chicken) &optional players)
  (unless players
    (setf players (players sc)))
  (unless (listp players)
    (setf players (list players)))
  (loop 
    for bar-num in (rehearsal-letters sc)
    for dc from 10 
    for letter = (get-rehearsal-letter dc)
    do 
    ;; MDE Fri Oct 11 17:11:56 2013
    ;; MDE Fri Jan  6 18:27:41 2023, Heidhausen -- we can now handle letters
    ;; beyond Z (see get-rehearsal-letter)
       (when (listp bar-num)
         (setq letter (second bar-num)
               bar-num (first bar-num)))
       (loop 
         for player in players
         ;; we have to set the rehearsal letter on the bar line of the previous
         ;; bar
         for bar = (get-bar sc (1- bar-num) player)
         do
            (unless bar
              (error "slippery-chicken::set-rehearsal-letters: couldn't get ~
                        bar ~a for ~a." bar-num player))
            (setf (rehearsal-letter bar) letter)))
  t)

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

;;; SAR Thu May 10 17:34:51 BST 2012: Added robodoc entry

;;; ****m* slippery-chicken/find-rehearsal-letters
;;; DESCRIPTION
;;; Return in list form the numbers of bars in the given slippery-chicken
;;; object that have rehearsal letters.
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; 
;;; RETURN VALUE
;;; A list of numbers.
;;; 
;;; EXAMPLE
#|
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((vn (violin :midi-channel 1))))
        :tempo-map '((1 (q 60)))
        :rehearsal-letters '(2 5 7)
        :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
        :set-map '((1 (1 1 1 1 1 1 1)))
        :rthm-seq-palette '((1 ((((2 4) (s) (s) e e e))
                                :pitch-seq-palette ((1 2 3)))))
        :rthm-seq-map '((1 ((vn (1 1 1 1 1 1 1))))))))
  (find-rehearsal-letters mini))

=> (2 5 7)

|#
;;; SYNOPSIS
(defmethod find-rehearsal-letters ((sc slippery-chicken))
;;; ****
  (loop 
      with player = (first (get-groups-top-ins sc))
      for bnum from 1 to (num-bars sc) 
      for bar = (get-bar sc bnum player)
      when (rehearsal-letter bar)
      collect (1+ bnum)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DATE
;;; 09-Apr-2011
;;;
;;; DESCRIPTION
;;; Print warnings to the Lisp listener if the method finds nested slurs,
;;; beg-sl marks without corresponding end-sl marks, or end-sl marks without
;;; corresponding beg-sl marks.
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;;
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to attempt a fix of any problems arising. NB depending on the
;;; number of problems, this method might need to be called several times before
;;; a clean pass is returned. 
;;; 
;;; RETURN VALUE
;;; 
;;; EXAMPLE
#|
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((vn (violin :midi-channel 1))))
        :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
        :set-map '((1 (1 1 1)))
        :rthm-seq-palette '((1 ((((4 4) e e e e e e e e))
                                :pitch-seq-palette ((1 2 3 4 5 6 7 8))
                                :marks (beg-sl 1 end-sl 4 beg-sl 2 end-sl 3
                                               beg-sl 4))))
        :rthm-seq-map '((1 ((vn (1 1 1))))))))
  (check-slurs mini))

=>
WARNING:
slippery-chicken::check-slurs (VN): 
begin slur at bar 1 but already began slur at bar 1
WARNING:
slippery-chicken::check-slurs (VN): 
begin slur at bar 2 but already began slur at bar 1
WARNING:
slippery-chicken::check-slurs (VN): 
begin slur at bar 2 but already began slur at bar 1
WARNING:
slippery-chicken::check-slurs (VN): 
begin slur at bar 3 but already began slur at bar 2
WARNING:
slippery-chicken::check-slurs (VN): 
begin slur at bar 3 but already began slur at bar 2
WARNING:
slippery-chicken::check-slurs (VN): 
end slur missing at end of piece
Respelling notes...
Inserting automatic clefs....
Generating VN...
Inserting line breaks...
Creating systems...
Calling CMN...
begin-slur without matching end-slur:
(slur :note (f4 e (onset 3/2)) :name 50 :type :left 
:note (f4 e (onset 3/2)) 
:staff-y0 0 :x0 0 :y0 0 :x1 0 :y1 0 #<SLUR>) 
(slur :note (f4 e (onset 11/2)) :name 53 :type :left 
:note (f4 e (onset 11/2)) 
:staff-y0 0 :x0 0 :y0 0 :x1 0 :y1 0 #<SLUR>)
(slur :note (f4 e (onset 19/2)) :name 56 :type :left 
:note (f4 e (onset 19/2)) 
:staff-y0 0 :x0 0 :y0 0 :x1 0 :y1 0 #<SLUR>)

|#
;;; SYNOPSIS
(defmethod check-slurs ((sc slippery-chicken) &optional fix (warn t))
;;; ****
  (check-slurs-aux sc "slurs" #'begin-slur-p #'end-slur-p fix warn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DESCRIPTION
;;; Print warnings to the Lisp listener if the method finds nested phrase
;;; markings, beg-ph marks without corresponding end-ph marks, or end-ph marks
;;; without corresponding beg-ph marks.
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;;
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to attempt a fix of any problems arising. NB depending on the
;;; number of problems, this method might need to be called several times before
;;; a clean pass is returned. 
;;; 
;;; RETURN VALUE
;;; Prints T if no warnings are made, otherwise prints warnings to the listener
;;; and returns NIL.
;;; 
;;; EXAMPLE
#|
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((vn (violin :midi-channel 1))))
        :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
        :set-map '((1 (1 1 1)))
        :rthm-seq-palette '((1 ((((4 4) e e e e e e e e))
                                :pitch-seq-palette ((1 2 3 4 5 6 7 8))
                                :marks (beg-ph 1 end-ph 4 beg-ph 2 end-ph 3
                                               beg-ph 4))))
        :rthm-seq-map '((1 ((vn (1 1 1))))))))
  (check-phrases mini))

=>
WARNING: 
rhythm::validate-mark: no Lilypond mark for BEG-PH (but adding anyway).
WARNING: 
rhythm::validate-mark: no CMN mark for BEG-PH (but adding anyway).
WARNING: 
rhythm::validate-mark: no Lilypond mark for END-PH (but adding anyway).
WARNING: 
rhythm::validate-mark: no CMN mark for END-PH (but adding anyway).
WARNING: 
rhythm::validate-mark: no Lilypond mark for BEG-PH (but adding anyway).
WARNING: 
rhythm::validate-mark: no CMN mark for BEG-PH (but adding anyway).
WARNING: 
rhythm::validate-mark: no Lilypond mark for END-PH (but adding anyway).
WARNING: 
rhythm::validate-mark: no CMN mark for END-PH (but adding anyway).
WARNING: 
rhythm::validate-mark: no Lilypond mark for BEG-PH (but adding anyway).
WARNING: 
rhythm::validate-mark: no CMN mark for BEG-PH (but adding anyway).

|#
;;; SYNOPSIS
(defmethod check-phrases ((sc slippery-chicken) &optional fix (warn t))
;;; ****
  (check-slurs-aux sc "phrases" #'begin-phrase-p #'end-phrase-p fix warn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Fri Dec 3 10:39:31 2021, Heidhausen -- also check for slurs over
;;; rests and try to fix, if desired
(defmethod check-slurs-aux ((sc slippery-chicken) name test-beg test-end
                            &optional fix (warn t))
  (flet ((rm-prob (e &optional end)     ; beg-* is default
           (when fix
             (cond ((string= name "phrases")
                    (rm-marks e (if end 'end-phrase 'beg-phrase)))
                   ((string= name "slurs")
                    (rm-marks e (if end 'end-sl 'beg-sl)))
                   (t (error "can't remove beg/end mark for ~a" name))))))
    (loop with ok = t with last-beg-slur for player in (players sc) do
       ;; reset to the first event
         (next-event sc player nil t)
         (setq last-beg-slur nil)
         (loop
            with in-slur
            for e = (next-event sc player)
            while e
            do
              (if (is-rest e)         ; MDE Fri Dec  3 10:40:34 2021, Heidhausen
                  (progn
                    (when in-slur
                      (when warn
                        (warn "slippery-chicken::check-~a (~a): began ~
                               ~%at bar ~a but ties over rest in bar ~a"
                              name player (bar-num last-beg-slur) (bar-num e)))
                      (setq ok nil)
                      (rm-prob last-beg-slur))
                    (when (funcall test-beg e)
                      (when warn
                        (warn "slippery-chicken::check-~a (~a): ~
                               ~%at bar ~a: can't begin on a rest."
                              name player (bar-num e)))
                      (setq ok nil)
                      (rm-prob e))
                    (when (funcall test-end e)
                      (when warn
                        (warn "slippery-chicken::check-~a (~a): ~
                               ~%at bar ~a: can't end on a rest."
                              name player (bar-num e)))
                      (setq ok nil)
                      (rm-prob e t)))
                  (cond
                    ((funcall test-beg e)
                     (if in-slur
                         (progn
                           (setq ok nil)
                           (when warn
                             (warn "slippery-chicken::check-~a (~a): began ~
                                    ~%at bar ~a but already began ~
                                    at bar ~a"
                                   name player (bar-num e) in-slur))
                           (rm-prob e))
                         (setq in-slur (bar-num e)
                               last-beg-slur e)))
                    ((funcall test-end e)
                     (if in-slur
                         (setq in-slur nil
                               last-beg-slur nil)
                         (progn
                           (rm-prob e t)
                           (setq ok nil)
                           (when warn
                             (warn "slippery-chicken::check-~a (~a): ended ~
                                    at bar ~a~%but no begin slur"
                                   name player (bar-num e))))))))
            finally
              (when in-slur
                (rm-prob last-beg-slur)
                (when warn
                  (warn "slippery-chicken::check-~a (~a): end mark missing at ~
                         end of piece" name player))))
       finally (return ok))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Thu May 17 14:43:33 EDT 2012: Added robodoc entry

;;; ****m* slippery-chicken/check-tuplets 
;;; DESCRIPTION
;;; Check the qualities of the tuplet brackets in a given slippery-chicken
;;; object to make sure they are all formatted properly (i.e. each starting
;;; tuplet bracket has a closing tuplet bracket etc.). If an error is found,
;;; the method will try to fix it, then re-check, and only issue an error then
;;; if another is found.
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - The function to use if something is not ok with the tuplets. This
;;;   defaults to #'error, but could also be #'warn for example
;;; 
;;; RETURN VALUE
;;; T if all tuplets brackets are ok, otherwise performs the on-fail function
;;; and returns NIL.
;;; 
;;; EXAMPLE
#|
;;; Create a slippery-chicken object, manually add an error to the tuplet data ;
;;; and call check-tuplets with #'warn as the on-fail function. ;
(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((cl (b-flat-clarinet :midi-channel 1))))
         :set-palette '((1 ((f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1)))
         :rthm-seq-palette '((1 ((((4 4) { 3 tq tq tq } +q e (s) s)))))
         :rthm-seq-map '((1 ((cl (1)))))))
       (e1 (get-event mini 1 1 'cl)))
  (setf (bracket e1) nil)
  (check-tuplets mini #'warn))

=> rthm-seq-bar::check-tuplets: Can't close non-existent bracket.

|#
;;; SYNOPSIS
(defmethod check-tuplets ((sc slippery-chicken) &optional (on-fail #'error))
;;; ****  
  (loop with ok for player in (players sc) do
       (loop for bar-num from 1 to (num-bars sc)
          for bar = (get-bar sc bar-num player)
          ;; MDE Fri May 18 13:17:58 2012 -- let's have a go at fixing them 
          ;; first   
          for this-ok = (check-tuplets bar nil)
          do
          (if this-ok
              (setf ok this-ok)
              (progn
                (when on-fail
                  ;; (print bar)
                  (warn "slippery-chicken::check-tuplets: failed for ~a ~
                         bar ~a. ~%Will delete current tuplets and try to ~
                         set them automatically." player bar-num))
                ;; auto-tuplets returns check-tuplets
                (setf ok (auto-tuplets bar on-fail)))))
       finally (return ok)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Sat Apr 28 16:42:59 2012 -- just for convenience
(defmethod handle-ties ((sc slippery-chicken))
  (handle-ties (piece sc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Just for checking really but if same-spellings all ties will be forced to
;;; the same spellings. 
;;; 
;;; cf piece::handle-ties

;;; ****m* slippery-chicken/check-ties
;;; DESCRIPTION
;;; Check that all ties are started and ended properly. If the optional
;;; argument <same-spellings> is set to T, all tied pitches will be forced to
;;; have the same enharmonic spellings. 
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to indicate whether to force all tied pitches to have the same
;;;   enharmonic spellings.
;;; - a function to call on error, or NIL for no action.
;;; 
;;; RETURN VALUE 
;;; T if all tie data is ok, otherwise performs the on-fail function and
;;; returns NIL.
;;; 
;;; EXAMPLE
#|
;;; Create a slippery-chicken object, manually create a problem with the ties,
;;; and call check-ties with a #'warn as the on-fail function.
(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((cl (b-flat-clarinet :midi-channel 1))))
         :set-palette '((1 ((f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1)))
         :rthm-seq-palette '((1 ((((4 4) { 3 tq tq tq } +q e (s) s)))))
         :rthm-seq-map '((1 ((cl (1)))))))
       (e4 (get-event mini 1 4 'cl)))
  (setf (is-tied-to e4) nil)
  (check-ties mini nil #'warn))

=> WARNING: slippery-chicken::check-ties: bad tie, CL bar 1

|#
;;; SYNOPSIS
(defmethod check-ties ((sc slippery-chicken)
                       &optional same-spellings (on-fail #'error))
;;; ****
  (loop with ok = t for player in (players sc) do
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
              (setf ok nil)
              (when on-fail
                (funcall on-fail
                         "slippery-chicken::check-ties (~a): <this> is tied-to ~
                          but ~%<last> has no pitch.~%THIS:~%~a~%LAST:~%~a"
                         player this last)))
            (setf (pitch-or-chord this) (clone (pitch-or-chord last)))
            (when (written-pitch-or-chord this)
              (setf (written-pitch-or-chord this) 
                    (clone (written-pitch-or-chord last)))))
          (when (or (and (is-tied-from last)
                         (not (is-tied-to this)))
                    (and (is-tied-to this)
                         (not (is-tied-from last))))
            (setf ok nil)
            (when on-fail
              (funcall on-fail
                       "slippery-chicken::check-ties: bad tie, ~a bar ~a" 
                       player (next-event sc nil))))
          (setf last this))
     finally (return ok)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Thu May 17 14:28:44 EDT 2012: Added robodoc entry.

;;; ****m* slippery-chicken/rebar
;;; DESCRIPTION
;;; Go through the vertically simultaneous sequences in all players of the
;;; given slippery-chicken object and rebar according to the first one that has
;;; the least number of bars (but following the player hierarchy).
;;;
;;; If rthm-seqs or sequenzes are created algorithmically and bundled into the
;;; slippery-chicken piece slot artificially, bypassing the usual generation
;;; structure, it might be difficult to end up with each instrument having the
;;; same metric structure when combined vertically. This method goes through
;;; the vertically combined sequences and rebars as described above.
;;;
;;; NB: See documentation in piece class method. Don't confuse this method with
;;;     the re-bar method.
;;;
;;; NB: This method is used internally and not recommended for direct use. 
;;;
;;; ARGUMENTS 
;;; - A slippery-chicken object.
;;;
;;; OPTIONAL ARGUMENTS
;;; - A list of player IDs from the given slippery-chicken object, ordered in
;;;   terms of importance i.e. which instrument's bar structure should take
;;;   precedence.
;;; - NB: The rebar-fun is not yet used.
;;; 
;;; RETURN VALUE  
;;; Always T.
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
            ;; MDE Mon Jul 23 13:09:10 2012 -- and test added
          (when (and (not (is-rest-bar bar))
                     ;; MDE Mon Jul 23 13:10:17 2012 -- this was all we were
                     ;; testing before  
                     (all-rests? bar))
            (force-rest-bar bar))))
  t)
            

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Thu May 10 18:11:15 BST 2012: Added robodoc entry
;;; SAR Thu Jun  7 12:26:44 BST 2012: Expanded robodoc entry

;;; ****m* slippery-chicken/check-time-sigs
;;; DATE
;;; 28-Jan-2011
;;;
;;; DESCRIPTION
;;; Check every bar in the given slippery-chicken object to see if all players
;;; have the same time signature. Drops into the debugger with an error if not.
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; 
;;; RETURN VALUE
;;; T if all players have the same time signature at the same time, otherwise
;;; drops into the debugger with an error.
;;; 
;;; EXAMPLE
#|
;; A successful test                    ;
(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))
                      (va (viola :midi-channel 2))
                      (vc (cello :midi-channel 3))))
         :set-palette '((1 ((f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1 1 1)))
         :rthm-seq-palette '((1 ((((4 4) { 3 tq tq tq } +q e (s) s)))))
         :rthm-seq-map '((1 ((vn (1 1 1))
                             (va (1 1 1))
                             (vc (1 1 1))))))))
  (check-time-sigs mini))

=> T

;; A failing test                       ;
(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))
                      (va (viola :midi-channel 2))
                      (vc (cello :midi-channel 3))))
         :set-palette '((1 ((f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1 1 1)))
         :rthm-seq-palette '((1 ((((4 4) { 3 tq tq tq } +q e (s) s)))))
         :rthm-seq-map '((1 ((vn (1 1 1))
                             (va (1 1 1))
                             (vc (1 1 1))))))))
  (setf (time-sig (get-bar mini 1 'vn)) '(3 4))
  (check-time-sigs mini))

=>
slippery-chicken::check-time-sigs: time signatures are not the same at bar 1
[Condition of type SIMPLE-ERROR]

|#
;;; SYNOPSIS
(defmethod check-time-sigs ((sc slippery-chicken))
;;; ****
  (loop for bar-num from 1 to (num-bars sc) 
     for bars = (get-bar sc bar-num) ;; gets bars for all players
     for ts1 = (get-time-sig (first bars))
     do
     (loop for pbar in (rest bars) do
          (unless (time-sig-equal ts1 (get-time-sig pbar))
            (error "slippery-chicken::check-time-sigs: time signatures ~
                    are not the same at bar ~a" bar-num))))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Thu May 10 18:17:14 BST 2012: Added robodoc entry

;;; ****m* slippery-chicken/player-doubles
;;; DATE
;;; 02-Apr-2012
;;
;;; DESCRIPTION
;;; Boolean test to check whether a specified player plays more than one
;;; instrument.
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; - A player ID.
;;; 
;;; RETURN VALUE
;;; T if the player has more than one instrument, otherwise NIL>
;;; 
;;; EXAMPLE
#|
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((sax ((alto-sax tenor-sax) :midi-channel 1))
                     (db (double-bass :midi-channel 2))))
        :instrument-change-map '((1 ((sax ((1 alto-sax) (3 tenor-sax)))))
                                 (2 ((sax ((2 alto-sax) (5 tenor-sax))))))
        :set-palette '((1 ((c2 d2 g2 a2 e3 fs3 b3 cs4 fs4 gs4 ds5 f5 bf5)))) 
        :set-map '((1 (1 1 1 1 1))
                   (2 (1 1 1 1 1)))
        :rthm-seq-palette '((1 ((((4 4) h q e s s))
                                :pitch-seq-palette ((1 2 3 4 5)))))
        :rthm-seq-map '((1 ((sax (1 1 1 1 1))
                            (db (1 1 1 1 1))))
                        (2 ((sax (1 1 1 1 1))
                            (db (1 1 1 1 1))))))))
  (player-doubles mini 'sax))

=> T

|#
;;; SYNOPSIS
(defmethod player-doubles ((sc slippery-chicken) player)
;;; ****
  (let ((player-obj (get-data player (ensemble sc))))
    (unless player-obj
      (error "slippery-chicken::player-doubles: can't get player ~a" player))
    (doubles player-obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Thu May 10 18:21:26 BST 2012: Added robodoc entry

;;; ****m* slippery-chicken/get-starting-ins
;;; DESCRIPTION
;;; Return the instrument object that is the first instrument object used by a
;;; specified player in a given slippery-chicken object.
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; - A player ID.
;;; 
;;; RETURN VALUE
;;; An instrument object.
;;; 
;;; EXAMPLE
#|

(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((sax ((alto-sax tenor-sax) :midi-channel 1))
                     (db (double-bass :midi-channel 2))))
        :instrument-change-map '((1 ((sax ((1 alto-sax) (3 tenor-sax)))))
                                 (2 ((sax ((2 alto-sax) (5 tenor-sax))))))
        :set-palette '((1 ((c2 d2 g2 a2 e3 fs3 b3 cs4 fs4 gs4 ds5 f5 bf5)))) 
        :set-map '((1 (1 1 1 1 1))
                   (2 (1 1 1 1 1)))
        :rthm-seq-palette '((1 ((((4 4) h q e s s))
                                :pitch-seq-palette ((1 2 3 4 5)))))
        :rthm-seq-map '((1 ((sax (1 1 1 1 1))
                            (db (1 1 1 1 1))))
                        (2 ((sax (1 1 1 1 1))
                            (db (1 1 1 1 1))))))))
  (get-starting-ins mini 'sax))

=> 
INSTRUMENT: lowest-written: BF3, highest-written: FS6
lowest-sounding: CS3, highest-sounding: A5
starting-clef: TREBLE, clefs: (TREBLE), clefs-in-c: (TREBLE)
prefers-notes: NIL, midi-program: 66
transposition: EF, transposition-semitones: -9
score-write-in-c: NIL, score-write-bar-line: NIL
chords: NIL, chord-function: NIL, 
total-bars: 5 total-notes: 25, total-duration: 20.000
total-degrees: 2920, microtones: T
missing-notes: (BQF3 BQF4), subset-id: NIL
staff-name: alto saxophone, staff-short-name: alt sax,

largest-fast-leap: 999, tessitura: BQF3
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: ALTO-SAX, tag: NIL, 
data: NIL

|#
;;; SYNOPSIS
(defmethod get-starting-ins ((sc slippery-chicken) player) ; symbol
;;; ****
  (let ((player-obj (get-data player (ensemble sc)))
        (ins-ref nil))
    (when (doubles player-obj)
      (setf ins-ref (get-first-for-player
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
;;; SAR Wed May 16 11:45:56 EDT 2012: Added robodoc entry
;;; SAR Tue Jun  5 12:17:53 BST 2012: A bit of conforming

;;; lilypond
;;; ****m* slippery-chicken/write-lp-data-for-all
;;; DESCRIPTION
;;; Generate all of the .ly files required by the LilyPond application for
;;; printable output from the musical data stored in the given slippery-chicken
;;; object.
;;;
;;; This method produces .ly files for the score as well as the parts for all
;;; individual players in the ensemble (unless otherwise specified by the
;;; user). The files are automatically named based on the value passed to the
;;; TITLE slot of the given slippery-chicken object.
;;; 
;;; NB: This method only produces the .ly files. These must be rendered by the
;;;     LilyPond application separately for PDF output. See the slippery
;;;     chicken installation web page and the manual page on Output for more
;;;     detail. Bear in mind that SBCL and CCL users on OSX can use the
;;;     lp-display macro to call Lilypond and display the resultant PDF
;;;     automatically.
;;;
;;; NB: Many of the arguments for this method pass their values directly to
;;;     LilyPond parameters. 
;;;
;;; NB: Clefs added to grace-note events will not be rendered in Lilypond.
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :base-path. A string that is the directory path only for the resulting
;;;   files. The method will automatically generate the file names and
;;;   extensions. Default =  (get-sc-config 'default-dir).
;;; - :start-bar. An integer that is the first bar of the given
;;;   slippery-chicken object for which output is to be generated. If NIL, the
;;;   start-bar will be set to 1. Default = NIL.
;;; - :end-bar. An integer that is the last bar of the given slippery-chicken
;;;   object for which output is to be generated. If NIL, all bars after the
;;;   start bar will be generated. Default = NIL.
;;; - :start-bar-numbering: For bar counting in the score only: the bar number
;;;   that the :start-bar will be counted as (integer). NIL = :start-bar. 
;;;   Default =  NIL. 
;;; - :players. A list of player IDs or NIL to indicate which players' parts
;;;   are to be generated and included in the resulting score. If NIL, all
;;;   players' parts will be generated and included in the score. This can be
;;;   handy, for example, for excluding the computer part of a piece for tape
;;;   and instruments. Default = NIL.
;;; - :respell-notes. NIL, T or a list to indicate whether the method should
;;;   also call the respell-notes method on the given slippery-chicken object
;;;   before generating the output to undertake enharmonic changes. If a list,
;;;   then these are the specific enharmonic corrections to be undertaken. If
;;;   this is T, the method will process all pitches for potential
;;;   respelling. If NIL, no respelling will be undertaken. See the
;;;   documentation for the respell-notes method for more. Default = NIL.
;;; - :auto-clefs. T or NIL to indicate whether the auto-clefs method should be
;;;   called to automatically place mid-measure clefs in the parts of
;;;   instruments that use more than one clef. T = automatically place clefs.
;;;   Default = T
;;; - :in-c. T or NIL to indicate whether the full score is to contain written
;;;   pitches or sounding pitches. NB: Some transposing C instruments still
;;;   transpose at the octave in C scores, such as double-bass and piccolo.
;;;   NB: Parts will always be transposed. T = sounding pitches. Default = NIL.
;;; - :page-nums. T or NIL to indicate whether page numbers should
;;;   automatically be added to each page (not including the start page) of the
;;;   output. T = add page numbers. Default = T.
;;; - :rehearsal-letters-font-size. A number that indicates the font size of
;;;   rehearsal letters in LilyPond output. Default = 18.
;;; - :rehearsal-letters-all-players. T or NIL to indicate whether rehearsal
;;;   letters are to be placed in all parts generated. T = all parts. 
;;;   Default = T. NB: This must be set to T when the user would like the
;;;   rehearsal letters in all individual LilyPond parts, but printing with CMN
;;;   thereafter will result in rehearsal letters in all parts as well.
;;; - :tempi-all-players. T or NIL to indicate whether tempo marks are to be
;;;   placed in all parts generated. T = all parts. Default = T.
;;; - :all-bar-nums. T o NIL to indicate whether the corresponding bar number
;;;   should be printed above every measure in the score (not including
;;;   multi-bar rests). T = add a bar number to every measure. Default = NIL. 
;;; - :paper. A string to indicate the paper size for LilyPond output. Only
;;;   LilyPond's predefined paper sizes are valid here. According to the
;;;   LilyPond manual, these include: "a4, letter, legal, and 11x17... Many
;;;   more paper sizes are supported... For details, see scm/paper.scm, and
;;;   search for the definition of paper-alist." NB: This argument will only
;;;   adjust paper size, but not margins or line widths, which are adjusted
;;;   using the arguments below. Default = "a4"
;;; - :staff-size. An integer that indicates the size of the notes and staves
;;;   in the resulting output. Standard for parts is 20. Default = 14.
;;; - :group-barlines. T or NIL to indicate whether bar lines should be drawn
;;;   through the whole staff group or just one staff. T = through the whole
;;;   staff group. Default = T.
;;; - :landscape. T or NIL to indicate whether the paper format should be
;;;   landscape or portrait. T = landscape. NB: This argument will only adjust
;;;   paper layout, but not margins or line widths, which are adjusted using
;;;   the arguments below. Default = NIL.
;;; - :barline-thickness. A number that is the relative thickness of the bar
;;;   lines. Default = 0.5.
;;; - :top-margin. A number that is the margin at the top of the page in
;;;   millimeters. Default = 10.
;;; - :bottom-margin. A number that is the margin at the bottom of the page in
;;;   millimeters. Default = 10.
;;; - :left-margin. A number that is the margin at the left of the page in
;;;   millimeters. Default = 20.
;;; - :line-width. A number that is the width of each line in centimeters.
;;;   Default = 17.
;;; - :page-turns. T or NIL to indicate if LilyPond should attempt to optimize
;;;   page breaks for page turns in parts. T = optimize page breaks. 
;;;   Default = NIL.
;;; - :min-page-turn. A two-item list indicating the minimum rest necessary for
;;;   the method to automatically place a page turn, in a format similar to
;;;   that of a time signature; i.e., '(2 1) would mean a minimum of 2 whole
;;;   rests. Default = '(2 1))
;;; - :use-custom-markup. T or NIL. Set to T when using a number of marks that
;;;   are specific to LilyPond, such as 'bartok or any of the marks that use
;;;   eps graphics files (whereupon those graphics files would need to be in
;;;   the same folder as your lilypond files). Default = T.
;;; - :lp-version. A string that will be added to each .ly file generated in
;;;   conjunction with the LilyPond \version command. Default = "2.42.1". NB
;;;   Lilypond syntax changes every now and then. If you get errors that can't
;;;   be explained by a user error, please post an issue on GitHub, as it might
;;;   be that we need to update the generated code to reflect Lilypond's new
;;;   syntax.
;;; - :process-event-fun. NIL or a user-defined function that will be applied
;;;   to every event object in the given slippery-chicken object. If NIL, no
;;;   processes will be applied. Default = NIL.
;;; - :extend-hairpins. If you want hairpin (cresc/dim) to extend beyond the
;;;    previous barline (or beyond the note) set to T. Default = NIL.
;;; - :min-hairpin. If you're getting Lilypond warnings about hairpins being too
;;;   short (and then not showing up in the score, or dynamics overlapping), you
;;;   can set this value to, e.g. 10.
;;; - :stemlet-length. NIL or a decimal number < 1.0 that indicates the scaled
;;;   length of stems over rests in LilyPond output, should this feature be
;;;   desired. 0.75 is a recommended value for this. NIL = no stems over
;;;   rests. Default = NIL. NB: LilyPond can be instructed to extend beams over
;;;   rests (without stemlets) simply by using the '-' in the definition of the
;;;   rthm-seq-bar object, as is done with any other note; however,
;;;   starting/ending a beam on a rest and then trying to generate a score with
;;;   CMN will fail.
;;; - :footer. A string to add to the footer of every score page. Default = NIL.
;;; - :title. Usually T or NIL to include the title of the piece at the top of
;;;   the score or not, but could also be a string to specify a new
;;;   title. Default = T.
;;; - :indent. T or NIL to indicate whether the first system in the score
;;;   should be indented or not. Default = T.
;;; - :force-bracket. T or NIL to indicate whether tuplet numbers on beams
;;;   should be forced into a bracket. Default = NIL.
;;; - :two-sided. A two-element list to set the inner and outer margins with
;;;   layout for two-sided printing and binding. E.g. :two-sided '(30 20) means
;;;   the left-hand margin will be 30mm and the right-hand will be 20mm. If
;;;   set, this overrides the :left-margin and :line-width arguments. 
;;;   Default = NIL.
;;; - :dummy-staves. The player name(s) as a symbol or a list for any parts
;;;   that have been created merely as empty staves to add cross-staff
;;;   notation.
;;; - :between-system-space. The space in cm between systems (groups of
;;;   staves). Default = NIL (= Lilypond's default distance).
;;; - :staff-basic-distance. This controls the distance
;;;   between staffs within a system. It's in Lilypond units (relative to
;;;   staff size). Setting to 15 creates a good distance between piano
;;;   staves. Default = NIL (= Lilypond's default distance).
;;; - :fixed-width. Set to a rhythmic value (e.g. 1/32) in order to create
;;;   fixed width rhythmic spacing. This makes it easier, for some, to read
;;;   complex rhythms as you can see how long rhythms last by the horizontal
;;;   space they occupy. The shorter the value given the more space is
;;;   allocated, so experimentation might be necessary. Default = NIL (usual
;;;   engraver's spacing algorithm used, according to context).
;;; - :accidental-style. String. See the lilypond documentation for details of
;;;   applicable styles and their results. Default = 'modern.
;;; - :links. T or NIL to indicate whether the generated PDF should contain
;;;   notes that are clickable links to the lilypond source code or not. Default
;;;   = T. 
;;; 
;;; RETURN VALUE
;;; The path of the main score file generated.
;;; 
;;; EXAMPLE
#|
;;; An example with values for the most frequently used arguments ;
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((fl (flute :midi-channel 1))
                     (cl (b-flat-clarinet :midi-channel 2))
                     (vc (cello :midi-channel 3))))
        :staff-groupings '(2 1)
        :tempo-map '((1 (q 84)) (9 (q 72)))
        :set-palette '((1 ((f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5))))
        :set-map '((1 (1 1 1 1 1 1 1 1))
                   (2 (1 1 1 1 1 1 1 1))
                   (3 (1 1 1 1 1 1 1 1)))
        :rthm-seq-palette '((1 ((((4 4) h (q) e (s) s))
                                :pitch-seq-palette ((1 2 3))
                                :marks (bartok 1)))
                            (2 ((((4 4) (q) e (s) s h))
                                :pitch-seq-palette ((1 2 3)))))
        :rthm-seq-map '((1 ((fl (1 2 1 2 1 2 1 2))
                            (cl (1 2 1 2 1 2 1 2))
                            (vc (1 2 1 2 1 2 1 2))))
                        (2 ((fl (1 2 1 2 1 2 1 2))
                            (cl (1 2 1 2 1 2 1 2))
                            (vc (1 2 1 2 1 2 1 2))))
                        (3 ((fl (1 2 1 2 1 2 1 2))
                            (cl (1 2 1 2 1 2 1 2))
                            (vc (1 2 1 2 1 2 1 2)))))
        :rehearsal-letters '(3 11 19))))
  (write-lp-data-for-all mini 
                         :start-bar 7
                         :end-bar 23
                         :paper "letter"
                         :landscape t
                         :respell-notes nil
                         :auto-clefs nil
                         :staff-size 17
                         :in-c nil
                         :barline-thickness 3.7
                         :top-margin 40
                         :bottom-margin 60
                         :left-margin 40
                         :line-width 22
                         :page-nums t
                         :all-bar-nums t
                         :use-custom-markup t
                         :rehearsal-letters-font-size 24
                         :lp-version "2.20.0"
                         :group-barlines nil
                         :page-turns t
                         :players '(fl cl)
                         :tempi-all-players t))

=> T

|#
;;; SYNOPSIS
(defmethod write-lp-data-for-all
    ((sc slippery-chicken) 
     &key
       (base-path (get-sc-config 'default-dir))
       start-bar end-bar (paper "a4") landscape
       ;; MDE Tue May 29 21:34:53 2012 
       start-bar-numbering
       ;; if a list, then these are the enharmonic corrections
       (respell-notes t) 
       ;; automatically add clefs to instruments who read more than one?
       (auto-clefs t)
       (staff-size 14)
       ;; parts will always be transposed but score can be in in C or not
       (in-c nil)
       (barline-thickness 0.5)
       (top-margin 10)                  ; mm
       (bottom-margin 10)               ; mm
       (left-margin 20)                 ; mm
       (line-width 17)                  ; cm
       ;; if not nil, then in cm
       between-system-space
       ;; MDE Tue May  3 12:12:46 2016 -- this one controls the distance
       ;; between staffs within a system. It's in Lilypond units (relative to
       ;; staff size). Setting to 15 creates a good distance between piano
       ;; staves. 
       staff-basic-distance
       (page-nums t)
       ;; print every bar number unless
       ;; multi-bar-rest?
       (all-bar-nums nil)
       ;; this has to be T if we're going to get letters in the parts--but CMN
       ;; printing will have all parts all letters too thereafter
       (rehearsal-letters-all-players t)
       ;; set to t if using bartok pizz and othersigns
       (use-custom-markup t)
       (rehearsal-letters-font-size 18)
       ;; "2.16.2") "2.14.2") ;"2.12.3") "2.17.95")  "2.20.0")
       (lp-version "2.24.1")
       ;; 24.7.11 (Pula) barlines through whole staff group or just a stave
       (group-barlines t)
       ;; 5.11.11 set to t if you want lilypond to optimize page breaks for
       ;; page turns in parts
       (page-turns nil)
       ;; MDE Sat Mar 10 16:52:31 2012 
       (process-event-fun nil)
       ;; MDE Mon Apr 16 16:08:36 2012 -- added so that we can write a subset
       ;; of players into the score (e.g. leave out a computer part). If nil
       ;; all players will be written. NB the order of these will determine
       ;; the order in the score (overriding the ensemble order).
       (players nil)
       (footer nil)
       ;; minimum rest necessary to do a page turn; something like a time
       ;; signature e.g. (2 1) would mean we need a min. of 2 whole rests
       (min-page-turn '(2 1))
       ;; MDE Tue May 29 22:58:25 2012 
       (stemlet-length nil)
       ;; MDE Thu Jan 9 09:20:33 2014 -- if you want hairpin (cresc/dim) to
       ;; extend beyond the note set to T
       (extend-hairpins nil)
       ;; MDE Wed Dec 30 18:29:28 2020, Heidhausen
       (min-hairpin nil)
       ;; MDE Wed Jul  8 10:45:02 2015 -- tuplet numbers on beams don't usually
       ;; include a bracket but we can force this if we like
       (force-bracket nil)
       ;; MDE Thu Mar 26 18:49:46 2015: show the title? could also be a string
       ;; in order to not use the sc title
       (title t)
       ;; MDE Thu Apr 29 17:39:40 2021, Heidhausen -- show the composer? also
       ;; possibly a string
       (composer t)
       two-sided                        ; MDE Wed Oct 21 18:15:08 2015
       ;; MDE Sat Dec 24 14:54:34 2016 -- try also "dodecaphonic" if you want
       ;; accidentals before every pitch 
       (accidental-style "modern")
       ;; MDE Thu Mar 26 19:18:03 2015
       (indent t)
       ;; MDE Thu Apr 28 15:17:36 2016 -- specify the player name(s) as a
       ;; symbol or in a list for any parts that have been created merely as
       ;; empty staves to add cross-staff notation.
       dummy-staves
       ;; MDE Thu Feb  2 14:45:21 2017 -- fixed width rhythmic notation
       fixed-width
       ;; MDE Sat Sep 30 16:55:40 2023, Heidhausen -- the PDF should contain
       ;; links to the source code?
       (links t)
       ;; sim to rehearsal letters
       (tempi-all-players t)
       (ekmelic-style "gost"))
;;; ****
  (declare (special cl-user::+slippery-chicken-src-path+))
  (setf dummy-staves (force-list dummy-staves))
  (when (and (numberp start-bar) (numberp end-bar) (>= start-bar end-bar))
    (error "slippery-chicken::write-lp-date-for-all: start-bar = ~a, ~
            end-bar = ~a???" start-bar end-bar))
  ;; MDE Mon Apr 16 16:19:47 2012 -- works when players is NIL also
  (unless (every #'(lambda (p) (member p (players sc))) players)
    (error "slippery-chicken::write-lp-data-for-all: players argument ~
            contains ~%player symbol(s) not in the ensemble: ~a" players))
  (let* ((path (trailing-slash base-path))
         ;; MDE Mon Apr 16 16:40:41 2012 -- now we have the players argument,
         ;; we have to put them in the same order as the ensemble and adjust the
         ;; staff-groupings locally to reflect any missing players
         (playrs (if players
                     (remove-if-not #'(lambda (x) (member x (players sc)))
                                    players)
                     (players sc)))
         ;; MDE Mon Apr 16 16:50:20 2012 -- adjust staff-groupings
         (staff-grping
          (let* ((grpsa (split-into-sub-groups (players sc)
                                               (staff-groupings sc)))
                 (grpsb (loop for sg in grpsa collect
                             (remove-if-not #'(lambda (x) (member x playrs))
                                            sg))))
            (loop for sg in grpsb for sgl = (length sg)
               ;; don't try and create groups of zero players
               unless (zerop sgl) collect sgl)))
         ;; MDE Fri Dec  9 19:33:28 2011 -- replace spaces with hyphens so good
         ;; for file names  
         ;; MDE Fri Apr  6 12:46:27 2012 -- and remove ' too
         (title-hyphens (filename-from-title (title sc)))
         (main-score-file
          (concatenate 'string path
                       (format nil "_~a-score.ly" title-hyphens)))
         (def-file (format nil "~a-def.ly" title-hyphens))
         (staff-group (if group-barlines "StaffGroup" "ChoirStaff"))
         (players-strings
          (loop for player in playrs
             ;; lilypond has trouble with variable names containing - or _
             collect (remove #\_ 
                             (remove #\- 
                                     (string-downcase (string player))))))
         (def-file-path (concatenate 'string path def-file)))
    (labels ((no-header-footer (stream)
               (format stream 
                       "~%\\header {~%  title = ~a ~%  subtitle = ~a~
                        ~%  dedication = ~a~%  tagline = ##f~
                        ~%  composer = ~a~%}"
                       (cond ((stringp title) (format nil "\"~a\"" title))
                             (title (format nil "\"~a\"" (title sc)))
                             (t "##f"))
                       ;; MDE Thu Apr 29 17:40:52 2021, Heidhausen -- don't show
                       ;; subtitle if :title nil
                       (if (and title (subtitle sc))
                           (format nil "\"~a\"" (subtitle sc))
                           "##f")
                       (if (dedication sc)
                           (format nil "\"~a\"" (dedication sc))
                           "##f")
                       (let ((comp (if (stringp composer)
                                       composer
                                       (composer sc))))
                         (if (and composer comp)
                             (format nil "\"~a~a\"" comp
                                     (if (year sc)
                                         (format nil " ~a" (year sc))
                                         ""))
                             "##f"))))
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
                 ;; MDE Tue May 29 22:53:58 2012 -- little stems to rests?
                 (when stemlet-length
                   (unless (and (numberp stemlet-length) 
                                (> stemlet-length 0.0)
                                (< stemlet-length 1.0))
                     (error "slippery-chicken::write-lp-data-for-all: ~
                             :stemlet-length should be between 0 and 1.0: ~a"
                            stemlet-length))
                   (format stream 
                           "~&  \\override Staff.Stem #'stemlet-length = #~a~%"
                           stemlet-length))
                 ;; MDE Thu May 11 14:52:16 2023, Heidhausen -- changes for
                 ;; version 2.21  
                 ;; (princ "  \\compressFullBarRests" stream)
                 (princ "  \\compressEmptyMeasures" stream)
                 ;; change the thickness of the barlines globally
                 ;; MDE Thu May 11 15:11:21 2023, Heidhausen -- version 2.21
                 ;; changes 
                 ;; (format stream "~&  \\override Score.BarLine ~
                    ;;             #'hair-thickness = #~a" barline-thickness)
                 (format stream "~&  \\override Score.BarLine.~
                                 hair-thickness = #~a" barline-thickness)
                 (when page-turns
                   (format stream "~%  \\set Staff.minimumPageTurnLength = ~
                                   #(ly:make-moment ~a ~a)"
                           (first min-page-turn) (second min-page-turn)))
                 (when force-bracket
                   (format stream "~%  \\override TupletBracket.bracket-~
                                   visibility = ##t"))
                 (format stream "~%  \\include \"~a-~a.ly\"~%}"
                         title-hyphens (if include-name include-name pname))))
             (score-tag (pname stream player
                               &optional new-staff-group end-staff-group)
               (when new-staff-group
                 ;; 24.7.11: to avoid barlines across groups
                 (format stream "~%  \\new ~a <<" staff-group))
               ;; MDE Tue Apr 26 19:05:11 2016 -- added the = "pname" so we can
               ;; refer to other staves in cross-staff notation 
               (format stream "~%    \\tag #'score \\tag #'~a ~
                               \\new Staff = \"~a\""
                       pname pname)
               ;; MDE Thu Apr 28 15:20:50 2016 -- it's a bit strange this but
               ;; experimentation shows that if we leave out this \global
               ;; statement then a part full of rest bars won't display the
               ;; whole bar rests, so we can use it for cross-staff notation
               (unless (member player dummy-staves)
                 (format stream "~%    { << \\global #(set-accidental-style ~
                                 '~a) \\~a >> }" accidental-style pname))
               (when end-staff-group
                 (format stream "~%  >>")))
             (needs-transposition (player) ; symbol
               (and in-c (plays-transposing-instrument 
                          (get-data player (ensemble sc)))))
             (written-pname (pname)
               (concatenate 'string pname "Written"))
             ;; MDE Thu Feb  2 14:58:22 2017 
             (make-fixed-width ()
               (format nil "~%      proportionalNotationDuration = ~
                            #(ly:make-moment ~a)~
                            ~%      \\override SpacingSpanner.~
                            uniform-stretching = ##t"
                       fixed-width))
             (score-block (stream &optional
                                  (pname "score") (score-tag-var "music"))
               (format stream "~%\\score {~%  \\keepWithTag #'~a \\~a"
                       pname score-tag-var)
               (if staff-basic-distance
                   ;; MDE Tue May  3 12:16:47 2016 
                   (format stream "~%  \\layout { ~%    \\context {~
                         ~%      \\Score \\override ~
                         StaffGrouper.staff-staff-spacing.~
                         basic-distance = #~a~a~%    }~%  }~%}"
                           staff-basic-distance
                           (if fixed-width (make-fixed-width) ""))
                   (if fixed-width
                       (format stream "~%  \\layout {~%    \\context {~
                                       ~%      \\Score ~a
                                       ~%    }~%  }~%}"
                               (make-fixed-width))
                       (format stream "~%  \\layout { }~%}"))))
             (part (pname stream &optional (score-tag-var "music"))
               (format stream "~&\\version \"~a\"" lp-version)
               (format stream "~&\\include \"~a\"~%" def-file)
               (no-header-footer stream)
               (terpri stream)
               ;; (princ "\\score {" stream)
               ;; (format stream "~&  \\new ~a \\keepWithTag #'~a \\~a"
               ;;            staff-group pname score-tag-var)
               ;; (format stream "~%  \\layout { }~%}")))
               (score-block stream pname score-tag-var)))
      (when respell-notes
        (respell-notes sc respell-notes))
      (when auto-clefs
        (format t "~&Inserting automatic clefs....")
        (auto-clefs sc :players playrs :verbose nil :in-c in-c
                    :delete-marks-before nil))
      (when rehearsal-letters-all-players 
        (format t "~&Setting rehearsal letters....")
        (set-rehearsal-letters sc playrs))
      (when tempi-all-players 
        (format t "~&Updating tempo of events....")
        (update-events-tempo sc playrs))
      ;; this will set the multi-bar-rest slot of the bars; NB it must come
      ;; after rehearsal letters and tempi
      (multi-bar-rests sc)
      ;; write the definitions file
      (with-open-file 
          (out def-file-path :direction :output :if-does-not-exist :create
               :if-exists :rename-and-delete :external-format :utf-8)
        (when (equal cm::*scale* (cm::find-object 'twelfth-tone-ekm))
          ;; including 72 edo ekmelily file
          (format out "~&\\include \"ekmel.ily\"")
                                        ; defining accidental style
          (format out "~&\\ekmelicStyle \"~a\"~%" ekmelic-style))
        ;; print would print the " marks hence princ
        (princ "\\include \"english.ly\"" out)
        ;; MDE Thu Dec 10 10:28:42 2020, Heidhausen -- way of setting landscape
        ;; has changed  
        (format out "~%~%\\paper { ~%  #(set-paper-size \"~a~a\")" 
                paper (if landscape "landscape" ""))
        (when page-turns
          (terpri out)
          (princ "  #(define page-breaking ly:page-turn-breaking)" out))
        (format out "~%  page-limit-inter-system-space = ##t")
        (unless page-nums 
          (format out "~%  print-page-number = ##f"))   
        (format out "~%  top-margin = ~a\\mm" top-margin)
        (format out "~%  bottom-margin = ~a\\mm" bottom-margin)
        (if two-sided
            (if (and (list-of-numbers-p two-sided)
                     (= 2 (length two-sided)))
                (progn
                  (format out "~%  two-sided = ##t")
                  (format out "~%  inner-margin = ~a\\mm" (first two-sided))
                  (format out "~%  outer-margin = ~a\\mm" (second two-sided)))
                (error "slippery-chicken::write-lp-data-for-all: two-sided ~
                        must be a list of 2 numbers~%(left margin and right ~
                        margin). You gave ~a" two-sided))
            (progn
              (format out "~%  left-margin = ~a\\mm" left-margin)
              (format out "~%  line-width = ~a\\cm" line-width)))
        (unless indent
          (format out "~%  indent = 0.0"))
        (when between-system-space
          ;; MDE Thu Dec 10 09:56:23 2020, Heidhausen -- changes to lilypond
          ;; (format out "~%  system-system-spacing #'basic-distance  = ~a\\cm"
          (format out "~%  system-system-spacing.basic-distance  = ~a\\cm"
                  between-system-space))
        (when footer
          (format out "~%  oddFooterMarkup = \\markup \\tiny \\fill-line ~
                       { \"~a\" }" footer))
        ;; paper } closed here
        (format out "~%}~%~%#(set-global-staff-size ~a)~%~%" staff-size)
        (when use-custom-markup
          (format out "~%\\include \"~alilypond.ly\"~%~%"
                  cl-user::+slippery-chicken-src-path+))
        (princ "global = {" out) 
        (terpri out)
        ;; MDE Mon Jul  2 16:20:20 2012
        ;; (princ "  \\key c \\major" out)
        (princ (get-lp-key-sig (first (key-sig sc)) (second (key-sig sc))) out)
        (terpri out)
        (princ "  \\numericTimeSignature" out)
        (terpri out)
        (princ (if links "\\pointAndClickOn" "\\pointAndClickOff") out)
        (terpri out)
        (princ "}" out)
        (terpri out)
        (terpri out)
        (loop for pname in players-strings
           for player in playrs do
             (when (needs-transposition player)
               (new-voice (written-pname pname) player out 
                          (concatenate 'string pname "-written")))
             (new-voice pname player out))
        (terpri out)
        (format out "~%music = {~%  <<")
        ;; write the music variable, staff groupings etc.
        ;; MDE Mon Apr 16 16:52:47 2012 -- use the adjusted staff-groupings
        (loop with groups = (copy-list staff-grping) ;(staff-groupings sc))
           with gnum = (pop groups)
           with gcount = 1
           for pname in players-strings
           for player in playrs 
           ;; this must come after 'in players-strings' otherwise we crash
           for end = (= gcount gnum) do
             (score-tag pname out player (= 1 gcount) end)
             (if end
                 (setf gnum (pop groups)
                       gcount 1)
                 (incf gcount)))
        (format out "~%  >>~%}")
        ;; create the written parts variable
        (format out "~%written = {~%  <<")
        (loop for pname in players-strings
           for player in playrs do
             (when (needs-transposition player)
               (score-tag (written-pname pname) out player)))
        (format out "~%  >>~%}"))
      ;; write the main score file
      (with-open-file
          (out main-score-file :direction :output :if-does-not-exist :create
               :if-exists :rename-and-delete :external-format :utf-8)
        (format out "~&\\version \"~a\"" lp-version)
        (format out "~%\\include \"~a\"" def-file)
        (no-header-footer out)
        (score-block out))
      ;; write the parts
      (loop for player in playrs
         for pname in players-strings do
           (with-open-file 
               (out 
                (concatenate 'string path (format nil "~a-~a-part.ly" 
                                                  title-hyphens pname))
                :direction :output :if-does-not-exist :create
                :if-exists :rename-and-delete :external-format :utf-8)
             (if (needs-transposition player)
                 (part (written-pname pname) out "written")
                 (part pname out))))
      ;; write the notes to individual files
      (loop for player in playrs
         for pname in players-strings do
           (write-lp-data-for-player 
            sc player 
            (concatenate 'string path (format nil "~a-~a.ly"
                                              title-hyphens pname))
            :all-bar-nums all-bar-nums
            :extend-hairpins extend-hairpins :min-hairpin min-hairpin
            :process-event-fun process-event-fun
            :rehearsal-letters-font-size rehearsal-letters-font-size
            :start-bar-numbering start-bar-numbering
            :in-c in-c :start-bar start-bar :end-bar end-bar))
      ;; got to write the written (i.e. not sounding) notes for the part
      ;; can't do this in the above loop as we have to re-call auto-clefs
      ;; making sure we don't use the in-c clefs for the instrument
      (when auto-clefs
        (auto-clefs sc :players playrs :verbose nil :in-c nil
                    :delete-marks-before nil))
      (loop for player in playrs
         for pname in players-strings do
         ;; got to write the written (i.e. not sounding) notes for the part
           (when (needs-transposition player)
             (write-lp-data-for-player 
              sc player 
              (format nil "~a~a-~a-written.ly" path title-hyphens pname)
              :all-bar-nums all-bar-nums :in-c nil :start-bar start-bar
              :start-bar-numbering start-bar-numbering
              :rehearsal-letters-font-size rehearsal-letters-font-size
              :extend-hairpins extend-hairpins
              :end-bar end-bar))))
    main-score-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod write-lp-data-for-player ((sc slippery-chicken) player path
                                     &key start-bar end-bar in-c
                                       ;; print every bar number unless
                                       ;; multi-bar-rest?
                                       all-bar-nums 
                                       ;; MDE Tue May 29 21:34:53 2012 
                                       start-bar-numbering
                                       ;; MDE Sat Mar 10 16:53:16 2012 
                                       process-event-fun
                                       extend-hairpins min-hairpin
                                       rehearsal-letters-font-size)
  (unless start-bar
    (setf start-bar 1))
  (unless end-bar
    (setf end-bar (num-bars sc)))
  ;; MDE Tue May 29 21:37:50 2012 
  (unless start-bar-numbering
    (setf start-bar-numbering start-bar))
  (with-open-file
      (out path :direction :output :if-does-not-exist :create
           :if-exists :rename-and-delete :external-format :utf-8)
    (let* ((player-obj (get-data player (ensemble sc)))
           (transposing (plays-transposing-instrument player-obj))
           (ins-obj (get-starting-ins sc player))
           (clef (starting-clef ins-obj)))
      ;; MDE Tue May 29 21:41:59 2012 
      (format out "~&\\set Score.currentBarNumber = #~a" start-bar-numbering)
      (when all-bar-nums
        (format out "~&\\override Score.BarNumber ~
                     #'break-visibility = #'#(#t #t #t)")
        ;; bar numbers centered over barline
        (format out "~&\\override Score.BarNumber #'self-alignment-X = #0"))
      (when extend-hairpins
        (format out "~&\\override Hairpin #'to-barline = ##f"))
      (when min-hairpin
        (format out "~&\\override Hairpin #'minimum-length = #~a" min-hairpin))
      ;; just write this in all parts, whether there's pedalling or not--does
      ;; no harm  
      (format out "~&\\set Staff.pedalSustainStyle=#'mixed")
      ;; 28.7.11 (Pula)
      (format out "~&\\autoBeamOff")
      ;; (format out "~&\\clef ~a" (string-downcase (format nil "~a" clef)))
      (if (eq clef 'percussion) 
          (format out "~&~a" (lp-percussion-clef)) 
          (format out "~&\\clef ~a" (string-downcase (format nil "~a" clef))))
      (loop for bar-num from start-bar to end-bar
         for rsb = (get-bar sc bar-num player)
         for lp-data = (get-lp-data rsb (or in-c (not transposing)) 
                                    rehearsal-letters-font-size
                                    process-event-fun)
         do
           (format out "~&% bar ~a~%" bar-num)
           (loop for data in lp-data do
                (when data
                  ;;(format out "~a " data))))))
                  (format out data))))))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken/write-xml
;;; DATE
;;; March 17th 2017, Edinburgh
;;; 
;;; DESCRIPTION
;;; Write a MusicXML file for import into Sibelius, Finale, Dorico, etc. All
;;; voices for the score and parts are written into one file. As it is assumed
;;; that editing will take place in the notation software of choice, there are
;;; only very basic arguments to this method, some of which may even be ignored
;;; upon import. For example, the :in-c keyword argument of other methods is
;;; missing here, as switching between sounding and transposing scores is
;;; assumed to be the job of the notation software used for MusicXML import.
;;;
;;; NB Sibelius 7.5 messes up some transposing instruments, getting octave
;;; transpositions wrong and adding generally unwanted key signatures.
;;; 
;;; ARGUMENTS
;;; - the slippery-chicken object
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments
;;; - :staff-height.  The suggested height of the staff, in millimetres. Default
;;;   = 7.
;;; - :page-width. The page width in millimetres. Default = 210 = A4
;;; - :page-height The page height in millimetres. Default = 297 = A4
;;; - :tenths. A number representing tenths of interline staff space (positive
;;;   or negative). Both integer and decimal values are allowed, such as 5 for a
;;;   half space and 2.5 for a quarter space. Interline space is measured from
;;;   the middle of a staff line. Distances in a MusicXML file are measured in
;;;   tenths of staff space. Tenths are then scaled to millimeters within the
;;;   scaling element, used in the defaults element at the start of a score. See
;;;   https://usermanuals.musicxml.com/MusicXML/Content/EL-MusicXML-scaling.htm
;;;   for more detail. Default = 40.
;;; - :left-page-margins. A 4-element list specifying for left pages in the
;;;   score the left, right, top, and bottom margins, all in
;;;   millimetres. Default = '(20 20 20 20) 
;;; - :right-page-margins. Similar to :left-page-margins but for right pages.
;;; - :players. Which players to write to the xml file. Default = NIL = process
;;;   all players.
;;; - :hide-players. Which players to leave out of the score.
;;; - :start-bar. The bar number to start at. Default = NIL = 1.
;;; - :end-bar. The bar number to end at. Default = NIL = the last bar in the
;;;   slippery-chicken object. 
;;; - :respell-notes. NIL, T or a list to indicate whether the method should
;;;   also call the respell-notes method on the given slippery-chicken object
;;;   before generating the output to undertake enharmonic changes. If a list,
;;;   then these are the specific enharmonic corrections to be undertaken. If
;;;   this is T, the method will process all pitches for potential
;;;   respelling. If NIL, no respelling will be undertaken. See the
;;;   documentation for the respell-notes method for more detail. Default = T.
;;; - :file. The full path of the file to write. Default will be derived from
;;;   the title slot of the slippery-chicken object and will be written into the
;;;   default directory e.g. (set-sc-config 'default-dir "~/Desktop")
;;; - :suffix. A string to append to the default file name before the .xml
;;;   extension. Default = "".
;;; 
;;; RETURN VALUE
;;; The file name written, as a string.
;;; 
;;; SYNOPSIS
(defmethod write-xml ((sc slippery-chicken)
                      &key
                        (staff-height 7)  ; mm
                        (page-width 210)  ; mm = a4
                        (page-height 297) ; mm = a4
                        (tenths 40)       ; see below
                        ;; left right top bottom, all mm
                        (left-page-margins '(20 20 20 20))
                        (right-page-margins '(20 20 20 20))
                        players hide-players start-bar end-bar
                        (respell-notes t)
                        ;; MDE Tue Nov 30 09:01:48 2021, Heidhausen
                        (suffix "") 
                        file)
;;; ****
  (unless start-bar
    (setq start-bar 1))
  (unless end-bar
    (setq end-bar (num-bars sc)))
  (when respell-notes
    (respell-notes sc respell-notes))
  (unless file
    (setq file (format nil "~a~a~a.xml"
                       (get-sc-config 'default-dir)
                       (filename-from-title (title sc))
                       suffix)))
  (with-open-file 
      (xml file :direction :output :if-does-not-exist :create
           :if-exists :rename-and-delete :external-format :utf-8)
    (format xml "~&<?xml version=\"1.0\" encoding=\"UTF-8\" ")
    (format xml "standalone=\"no\"?>")
    (format xml "~&<!DOCTYPE score-partwise PUBLIC ")
    (format xml "\"-//Recordare//DTD MusicXML 3.0 Partwise//EN\" ")
    (format xml  "\"http://www.musicxml.org/dtds/partwise.dtd\">")
    (format xml "~&<score-partwise version=\"3.0\">")
    (when (title sc)
      (format xml "~&  <work>~&    <work-title> ~&      ~a" (title sc)))
    (when (subtitle sc)
      (format xml "~&      (~a)" (subtitle sc)))
    (format xml "~&    </work-title>~&  </work>")
    (format xml "~&  <identification>")
    (when (composer sc)
      (format xml "~&    <creator type=\"composer\">~a ~a</creator>"
              (composer sc) (if (year sc) (year sc) "")))
    (format xml "~&    <encoding>")
    (format xml "~&      <software>~&        slippery chicken ~a"
             +slippery-chicken-version+)
    (format xml "~&      </software>")
    ;; just date, no time allowed; must be in yyyy-mm-dd format
    (format xml "~&      <encoding-date>~a</encoding-date>"
            (get-date-string nil))
    (format xml "~&      <supports attribute=\"new-system\" element=\"print\"")
    (format xml " type=\"yes\" value=\"yes\"/>")
    (format xml "~&      <supports attribute=\"new-page\" element=\"print\" ")
    (format xml "type=\"yes\" value=\"yes\"/>")
    (format xml "~&      <supports element=\"accidental\" type=\"yes\"/>")
    (format xml "~&      <supports element=\"beam\" type=\"yes\"/>")
    (format xml "~&      <supports element=\"stem\" type=\"yes\"/>")
    (format xml "~&    </encoding>")
    (format xml "~&  </identification>")
    (format xml "~&  <defaults>")
    (format xml "~&    <scaling>")
    ;; this sets staff height
    (format xml "~&      <millimeters>~a</millimeters>" staff-height)
    ;; see comments to defun xml-mm2tenths
    (format xml "~&      <tenths>40</tenths>")
    (format xml "~&    </scaling>")
    (format xml "~&    <page-layout>")
    ;; so this is a pain: these values are in 'tenths'
    (format xml "~&      <page-height>~a</page-height>"
            (xml-mm2tenths page-height staff-height tenths))
    (format xml "~&      <page-width>~a</page-width>"
            (xml-mm2tenths page-width staff-height tenths))
    (format xml "~&      <page-margins type=\"even\">")
    (format xml "~&        <left-margin>~a</left-margin>"
            (xml-mm2tenths (first left-page-margins) staff-height tenths))
    (format xml "~&        <right-margin>~a</right-margin>"
            (xml-mm2tenths (second left-page-margins) staff-height tenths))
    (format xml "~&        <top-margin>~a</top-margin>"
            (xml-mm2tenths (third left-page-margins) staff-height tenths))
    (format xml "~&        <bottom-margin>~a</bottom-margin>"
            (xml-mm2tenths (fourth left-page-margins) staff-height tenths))
    (format xml "~&      </page-margins>")
    (format xml "~&      <page-margins type=\"odd\">")
    (format xml "~&        <left-margin>~a</left-margin>"
            (xml-mm2tenths (first right-page-margins) staff-height tenths))
    (format xml "~&        <right-margin>~a</right-margin>"
            (xml-mm2tenths (second right-page-margins) staff-height tenths))
    (format xml "~&        <top-margin>~a</top-margin>"
            (xml-mm2tenths (third right-page-margins) staff-height tenths))
    (format xml "~&        <bottom-margin>~a</bottom-margin>"
            (xml-mm2tenths (fourth right-page-margins) staff-height tenths))
    (format xml "~&      </page-margins>")
    (format xml "~&    </page-layout>")
    (format xml "~&  </defaults>")
    (when (composer sc)
      (format xml "~&  <credit page=\"1\">")
      (format xml "~&    <credit-type>composer</credit-type>")
      (format xml "~&    <credit-words justify=\"right\" valign=\"top\">")
      (format xml "~&     ~a~&    </credit-words>~&  </credit>" (composer sc)))
    (when (title sc)
      (format xml "~&  <credit page=\"1\">")
      (format xml "~&    <credit-type>title</credit-type>")
    (format xml "~&    <credit-words font-size=\"20\" justify=\"center\" ")
    (format xml "valign=\"top\">~&     ~a~&    </credit-words>~&  </credit>"
            (title sc)))
    (when (subtitle sc)
      (format xml "~&  <credit page=\"1\">")
      (format xml "~&    <credit-type>subtitle</credit-type>")
      (format xml "~&      <credit-words font-size=\"20\" justify=\"center\" ")
      (format xml "valign=\"top\">~&       ~a~&      </credit-words>"
              (subtitle sc))
      (format xml "~&  </credit>"))
    (format xml "~&  <part-list>")
    (let ((the-players (if players (force-list players) (players sc))))
      ;; MDE Tue Mar  2 16:38:50 2021, Heidhausen 
      (when hide-players
        (players-exist (ensemble sc) hide-players)
        ;; reverse because in sbcl and ccl set-difference returns its first
        ;; arg. in reverse order
        (setq the-players (reverse (set-difference the-players hide-players))))
      (loop for p in the-players do
           (xml-score-part (get-player sc p) xml))
      (format xml "~&  </part-list>")
      (loop for p in the-players do
           (write-xml-for-player sc p start-bar end-bar xml)))
    (format xml "~&</score-partwise>~%<!-- EOF -->~%")
    (format t "~&Wrote ~a" file)
    file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 17/3 17
;;; would be right and proper to have this in the player class but as we need
;;; access to the sc object and player is just a symbol, so...
(defmethod write-xml-for-player ((sc slippery-chicken) player start-bar end-bar
                                 stream)
  ;; we handle transposing players by writing the written pitches, with correct
  ;; transposition data, and let the notation software/user decide on the final
  ;; format of the score
  (let* ((player-obj (get-data player (ensemble sc)))
         (clef (starting-clef (get-starting-ins sc player)))
         start-repeat     ; xml needs a start-repeat barline at the beginning of
                                        ; the measure
         (transp 0))
    (unless player-obj
      (error "slippery-chicken::write-xml-for-player: couldn't get player ~a"
             player))
    (format stream "~&  <part id=\"~a\">" (id player-obj))
    (loop for bar-num from start-bar to end-bar
       for rsb = (get-bar sc bar-num player) do
         ;; (print-simple rsb)
         (setq start-repeat
               (write-xml rsb :stream stream :starting-clef clef
                          :start-repeat start-repeat
                          :transposition
                          (let* ((ins (get-instrument-for-player-at-bar
                                       player (bar-num rsb) sc))
                                 (ts (transposition-semitones ins)))
                            (when (and ts (/= ts transp))
                              (setq transp ts)
                              (xml-transposition ins))))))
    (format stream "~&    </part>")
    t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
         ;; data. 
         (update-slots sc)
         ;; don't inc start-bar as bar-nums are adjusted via update-slots
         ;; (incf start-bar num-deleted)
         (decf num-bars num-deleted)
         (when print
           (format t "~%num-deleted: ~a, num-bars in seq: ~a, nth-bar: ~a, ~
                      num-bars: ~a"
                   num-deleted (num-bars seq) nth-bar num-bars))))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Thu May 10 18:25:31 BST 2012: Added robodoc entry

;;; ****m* slippery-chicken/get-events-from-to
;;; DATE
;;; 22-Jul-2011 (Pula)
;;;
;;; DESCRIPTION
;;; Return a list of event objects for a given player, specifying the region by
;;; bar and event number.
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; - A player ID.
;;; - An integer (1-based) that is the first bar from which to return events.
;;; - An integer (1-based) that is the first event object in the start-bar to
;;;   return.
;;; - An integer (1-based) that is the last bar from which to return events. 
;;; 
;;; OPTIONAL ARGUMENTS
;;; - An integer (1-based) that is the last event within the end-bar to
;;;   return. 
;;; 
;;; RETURN VALUE
;;; A flat list of event objects.
;;; 
;;; EXAMPLE
#|
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((sax ((alto-sax tenor-sax) :midi-channel 1))
                     (db (double-bass :midi-channel 2))))
        :instrument-change-map '((1 ((sax ((1 alto-sax) (3 tenor-sax)))))
                                 (2 ((sax ((2 alto-sax) (5 tenor-sax))))))
        :set-palette '((1 ((c2 d2 g2 a2 e3 fs3 b3 cs4 fs4 gs4 ds5 f5 bf5)))) 
        :set-map '((1 (1 1 1 1 1))
                   (2 (1 1 1 1 1)))
        :rthm-seq-palette '((1 ((((4 4) h q e s s))
                                :pitch-seq-palette ((1 2 3 4 5)))))
        :rthm-seq-map '((1 ((sax (1 1 1 1 1))
                            (db (1 1 1 1 1))))
                        (2 ((sax (1 1 1 1 1))
                            (db (1 1 1 1 1))))))))
  (get-events-from-to mini 'sax 3 2 5 3))

=>
(
EVENT: start-time: 10.000, end-time: 11.000, 
duration-in-tempo: 1.000, 
compound-duration-in-tempo: 1.000, 
amplitude: 0.700 
bar-num: 3, marks-before: NIL, 
tempo-change: NIL 
instrument-change: NIL 
display-tempo: NIL, start-time-qtrs: 10.000, 
midi-time-sig: NIL, midi-program-changes: NIL, 
8va: 0
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
show-accidental: T, white-degree: 23, 
accidental: N, 
accidental-in-parentheses: NIL, marks: NIL
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: E3, tag: NIL, 
data: E3
**************

written-pitch-or-chord: 
PITCH: frequency: 369.994, midi-note: 66, midi-channel: 1 
pitch-bend: 0.0 
degree: 132, data-consistent: T, white-note: F4
nearest-chromatic: FS4
src: 1.4142135, src-ref-pitch: C4, score-note: FS4 
qtr-sharp: NIL, qtr-flat: NIL, qtr-tone: NIL,  
micro-tone: NIL, 
sharp: T, flat: NIL, natural: NIL, 
octave: 4, c5ths: 1, no-8ve: FS, no-8ve-no-acc: F
show-accidental: T, white-degree: 31, 
accidental: S, 
accidental-in-parentheses: NIL, marks: NIL
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: FS4, tag: NIL, 
data: FS4
**************

RHYTHM: value: 4.000, duration: 1.000, rq: 1, is-rest: NIL, 
score-rthm: 4.0, undotted-value: 4, num-flags: 0, num-dots: 0, 
is-tied-to: NIL, is-tied-from: NIL, compound-duration: 1.000, 
is-grace-note: NIL, needs-new-note: T, beam: NIL, bracket: NIL, 
rqq-note: NIL, rqq-info: NIL, marks: NIL, marks-in-part: NIL, 
letter-value: 4, tuplet-scaler: 1, grace-note-duration: 0.05
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: Q, tag: NIL, 
data: Q
**************

        
EVENT: start-time: 11.000, end-time: 11.500, 
[...]

|#
;;; SYNOPSIS
(defmethod get-events-from-to ((sc slippery-chicken) player start-bar
                               start-event end-bar &optional end-event)
;;; ****
  ;; MDE Thu Jan 10 14:04:38 2019
  (unless start-event (setq start-event 1))
  ;; MDE Mon Jun 13 12:15:23 2016
  (unless end-bar (setq end-bar (num-bars sc)))
  (unless end-event (setq end-event (num-rhythms (get-bar sc end-bar player))))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken/get-all-events
;;; DATE
;;; August 22nd 2013 (Edinburgh)
;;;
;;; DESCRIPTION
;;; Return a flat list containing all the events of a slippery-chicken object
;;; fo the given player.
;;; 
;;; ARGUMENTS
;;; - The slippery-chicken object
;;; - The player (symbol)
;;;
;;; RETURN VALUE
;;; A list of event objects
;;; 
;;; SYNOPSIS
(defmethod get-all-events ((sc slippery-chicken) player)
;;; ****
  (get-events-from-to sc player 1 1 (num-bars sc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Mon Jun 13 12:22:04 2016 
(defmethod update-amplitudes ((sc slippery-chicken))
  (loop for player in (players sc)
     for events = (get-all-events sc player)
     do (update-events-amplitudes events))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken/transpose-events 
;;; DESCRIPTION
;;; Transpose the pitches of event objects in a specified region and a
;;; specified player's part.
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; - A player ID.
;;; - An integer that is the first bar in which to transpose events.
;;; - An integer that is the first event in that bar to transpose. 
;;; - An integer that is the last bar in which to transpose events.
;;; - An integer that is the last event in that bar to transpose.
;;; - A positive or negative number that is the number of semitones by which
;;;   the pitches of the events in the specified region should be transposed. 
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword argument:
;;; - :destructively. T or NIL to indicate whether the pitches of the original
;;;   event objects should be replaced. T = replace. Default = T.
;;; 
;;; RETURN VALUE
;;; Returns a list of events.
;;; 
;;; EXAMPLE
#|
;;; Print the pitches before and after applying the method ;
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((sax (alto-sax :midi-channel 1))
                     (db (double-bass :midi-channel 2))))
        :set-palette '((1 ((c2 d2 g2 a2 e3 fs3 b3 cs4 fs4 gs4 ds5 f5 bf5)))) 
        :set-map '((1 (1 1 1 1 1)))
        :rthm-seq-palette '((1 ((((4 4) h q e s s))
                                :pitch-seq-palette ((1 2 3 4 5)))))
        :rthm-seq-map '((1 ((sax (1 1 1 1 1))
                            (db (1 1 1 1 1))))))))
  (print 
   (loop for e in (get-events-from-to mini 'sax 3 2 5 3)
      collect (get-pitch-symbol e)))
  (transpose-events mini 'sax 3 2 5 3 11)
  (print 
   (loop for e in (get-events-from-to mini 'sax 3 2 5 3)
      collect (get-pitch-symbol e))))

=>
(EF4 AF4 BF4 EF5 CS4 EF4 AF4 BF4 EF5 CS4 EF4 AF4) 
(D5 G5 A5 D6 C5 D5 G5 A5 D6 C5 D5 G5) 

|#
;;; SYNOPSIS
(defmethod transpose-events ((sc slippery-chicken) player start-bar
                             start-event end-bar end-event semitones
                             &key (destructively t))
;;; ****
  (let ((events (get-events-from-to sc player start-bar start-event end-bar
                                    end-event)))
    (loop for e in events collect
         (transpose e semitones :destructively destructively))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod cautionary-accidental-aux ((sc slippery-chicken) bar-num note-num
                                      player value &optional written)
  (let ((note (get-note sc bar-num note-num player written)))
    (when note
      (when (event-p note)
        (setf note (if written
                       (written-pitch-or-chord note)
                       (pitch-or-chord note))))
      (when value
        (setf (show-accidental note) t))
      (setf (accidental-in-parentheses note) value))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A cmn mark can only be applied to one note, after that, the mark will not
;;; display, so instead of storing the same mark in several notes, pass a
;;; function that creates a mark, and call that function to create a
;;; separate instance of the cmn mark for each note that it should be applied
;;; to. 
;;; 1.3.11 as marks are now all symbols, this is obsolete but keep in file for
;;; legacy purposes.
#|

(defmethod add-mark-to-notes ((sc slippery-chicken) mark-function player
notes)
(loop 
for bar in notes 
for bar-num-or-ref = (first bar)
for notes = (rest bar)
do
(loop for n in notes do
(add-mark-to-note sc bar-num-or-ref n player 
(funcall mark-function))))
t)
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DATE 
;;; 17-Apr-2012
;;;
;;; ****m* slippery-chicken/get-section
;;; DESCRIPTION
;;; Return the section object with the specified reference ID.
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; - A reference ID.
;;; 
;;; RETURN VALUE
;;; A section object.
;;; 
;;; EXAMPLE
#|
(let ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((sax (alto-sax :midi-channel 1))
                      (db (double-bass :midi-channel 2))))
         :set-palette '((1 ((c2 d2 g2 a2 e3 fs3 b3 cs4 fs4 gs4 ds5 f5 bf5)))) 
         :set-map '((1 (1 1 1 1 1))
                    (2 (1 1 1 1 1)))
         :rthm-seq-palette '((1 ((((4 4) h q e s s))
                                 :pitch-seq-palette ((1 2 3 4 5)))))
         :rthm-seq-map '((1 ((sax (1 1 1 1 1))
                             (db (1 1 1 1 1))))
                         (2 ((sax (1 1 1 1 1))
                             (db (1 1 1 1 1))))))))
  (get-section mini 2))
=> 
SECTION: 
RECURSIVE-ASSOC-LIST: recurse-simple-data: NIL
num-data: 2
linked: T
full-ref: (2)
ASSOC-LIST: warn-not-found NIL
CIRCULAR-SCLIST: current 0
SCLIST: sclist-length: 2, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
BAR-HOLDER: 
start-bar: 6
end-bar: 10
num-bars: 5
start-time: 20.0
end-time: 40.0
start-time-qtrs: 0
end-time-qtrs: 40.0
num-notes (attacked notes, not tied): 50
num-score-notes (tied notes counted separately): 50 
num-rests: 0
duration-qtrs: 20.0 
duration: 20.0 (20.000)


|#
;;; SYNOPSIS
(defmethod get-section ((sc slippery-chicken) reference &optional (warn t))
;;; ****
  (get-data-data reference (piece sc) warn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Wed Apr 25 12:18:25 2012 

;;; ****m* slippery-chicken/get-sequenz-from-section
;;; DESCRIPTION
;;; Get the sequenz object specified by the section, player and sequenz number
;;; given. 
;;; 
;;; ARGUMENTS
;;; - a slippery-chicken object
;;; - the player (symbol)
;;; - the sequenz number, counting from 1
;;; 
;;; RETURN VALUE
;;; the respective sequenz object
;;; 
;;; SYNOPSIS
(defmethod get-sequenz-from-section ((sc slippery-chicken)
                                     section-ref player-ref seq-num) ; 1-based
;;; ****
  (let ((section (get-section sc section-ref)))
    (when section
      (get-sequenz section player-ref seq-num))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Fri Jun 15 08:32:00 2012
;;; SAR Mon Jul  2 20:17:41 BST 2012: Added ref to robodc in rthm-seq-bar

;;; ****m* slippery-chicken/check-beams
;;; DESCRIPTION
;;; See the description, with example, for check-beams in rthm-seq-bar.lsp. 
;;; Players can either be a single symbol, a list of symbols, or nil (whereby
;;; all players will be checked).
;;;
;;; SYNOPSIS
(defmethod check-beams ((sc slippery-chicken) &key start-bar end-bar players
                        auto-beam print (on-fail #'warn))
;;; ****
  (map-over-bars sc start-bar end-bar players #'check-beams
                 :auto-beam auto-beam :print print :on-fail on-fail))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Fri Jun 15 13:11:52 2012 -- will set the written pitch/chord for all
;;; events where the player plays a transposing instrument (which can change of
;;; course--handled here as we get instrument on a bar-by-bar basis.

;;; SAR Mon Jun 18 13:07:50 BST 2012: Added robodoc entry

;;; ****m* slippery-chicken/auto-set-written
;;; DESCRIPTION
;;; Automatically set the WRITTEN-PITCH-OR-CHORD slot for all events where the
;;; player plays a transposing instrument (which can change of course).
;;;
;;; ARGUMENTS
;;; - A slippery-chicken object. 
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :start-bar. NIL or an integer that is the first bar for which the written
;;;   pitch/chord data is to be set. If NIL, start from bar 1. Default = NIL.
;;; - :end-bar. NIL or an integer that is the last bar for which the written
;;;   pitch/chord data is to be set. If NIL, do all bars. Default = NIL.
;;; - :players. NIL, the ID or a list of IDs for the player(s) whose part(s)
;;;   are to be affected. If NIL, all players' parts will be affected. 
;;;   Default = NIL.
;;; 
;;; RETURN VALUE
;;; T
;;; 
;;; EXAMPLE
#|
;;; Create a slippery-chicken object, set all the written-pitch-or-chord ;
;;; slots to NIL and print the results. Apply the method and print the results ;
;;; again to see the difference.        ;
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((hn (french-horn :midi-channel 1))))
        :set-palette '((1 ((f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5))))
        :set-map '((1 (1 1 1 1 1)))
        :rthm-seq-palette '((1 ((((4 4) h q e s s))
                                :pitch-seq-palette ((1 2 3 4 5)))))
        :rthm-seq-map '((1 ((hn (1 1 1 1 1))))))))
  (next-event mini 'hn nil 1)
  (loop for ne = (next-event mini 'hn)
     while ne
do (setf (written-pitch-or-chord ne) nil))
  (next-event mini 'hn nil 1)
  (print
   (loop for ne = (next-event mini 'hn)
      while ne
collect (written-pitch-or-chord ne)))
  (auto-set-written mini)
  (next-event mini 'hn nil 1)
  (print
   (loop for ne = (next-event mini 'hn)
      while ne
collect (data (written-pitch-or-chord ne)))))

=>
(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
NIL NIL NIL NIL NIL NIL NIL) 
(C4 D4 E4 FS4 G4 C4 D4 E4 FS4 G4 C4 D4 E4 FS4 G4 C4 D4 E4 FS4 G4 C4 D4 E4
FS4 G4)

|#
;;; SYNOPSIS
(defmethod auto-set-written ((sc slippery-chicken) &key start-bar end-bar
                             players)
;;; ****
  (unless players
    (setf players (players sc)))
  (unless (listp players)
    (setf players (list players)))
  (unless start-bar
    (setf start-bar 1))
  (unless end-bar
    (setf end-bar (num-bars sc)))
  (loop for player in players do
       (loop for bar-num from start-bar to end-bar
            for bar = (get-bar sc bar-num player)
            for ins = (get-instrument-for-player-at-bar player bar-num sc)
            for transp = (transposition-semitones ins)
            do
            (unless (zerop transp)
              (set-written bar (- transp)))))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Fri Jun 15 13:51:07 2012 
;;; player optional arg is actually required
(defmethod plays-transposing-instrument ((sc slippery-chicken) 
                                         &optional player (ignore-octaves t))
  (let ((plyer (get-player sc player)))
    ;; MDE Mon Jun 18 17:02:24 2012 -- 
    (unless plyer
      (error "slippery-chicken::plays-transposing-instrument: ~
              Can't get player ~a" player))
    (plays-transposing-instrument plyer ignore-octaves)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Mon Jul 16 14:54:48 2012 

(defmethod duration ((sc slippery-chicken))
  (duration (piece sc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Fri Aug 10 17:22:54 2012 -- works on the sounding pitches (written
;;; pitches will be updated if appropriate)
;;; MDE Mon Feb 13 15:31:21 2017 -- updated to force an existing single pitch
;;; into a chord so we can add notes to it if necessary 
(defmethod sc-get-chord ((sc slippery-chicken) bar-num note-num player 
                         &optional (on-fail #'error))
  (let* ((event (get-note sc bar-num note-num player))
         (chord (when event (pitch-or-chord event))))
    (if (chord-p chord)
        (values chord event)
        (if (and (eq 'force on-fail) (pitch-p chord))
            (values (make-chord chord) event)
            ;; this means rests will simply be ignored
            (when (functionp on-fail)
              (funcall on-fail "slippery-chicken::get-chord: no chord at ~
                               bar ~a, note ~a for ~a"
                       bar-num note-num player))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken/get-nearest-event
;;; DESCRIPTION
;;; Find the nearest event by time to <reference-event> in the <search-player>.  
;;; 
;;; ARGUMENTS
;;; - the slippery-chicken object
;;; - the reference event object that we want to find the nearest event to in
;;;   another player 
;;; - the player we want to find the nearest event in (symbol).
;;;
;;; OPTIONAL ARGUMENTS
;;; - if T only return sounding events (pitches/chords) otherwise return the
;;;   nearest rest or sounding event.
;;; - if T only return struck events i.e. ignore those that are tied to
;;; 
;;; RETURN VALUE
;;; An event object.
;;; 
;;; SYNOPSIS
(defmethod get-nearest-event ((sc slippery-chicken) reference-event
                              search-player &optional 
                              (ignore-rests t)
                              (ignore-tied t))
;;; ****
  (unless (event-p reference-event)
    (error "slippery-chicken::get-nearest-event: reference-event must be an ~
            ~&event object: ~a" reference-event))
  (unless (and (numberp (start-time reference-event))
               (integer>0 (bar-num reference-event)))
    (error "slippery-chicken::get-nearest-event: reference-event must have ~
            ~%had its start-time and bar-num slots set: ~a" reference-event))
  (let* ((events '())
         (start-bar (bar-num reference-event))
         (end-bar start-bar)
         (max (- (num-bars sc) 3))
         (time (start-time reference-event)))
    (loop until events do
       ;; if we don't get sounding events the first time round we've got to
       ;; look earlier and later
         (if (> start-bar 3)
             (decf start-bar 3)
             (setf start-bar 1))
         (if (< end-bar max)
             (incf end-bar 3)
             (setf end-bar (num-bars sc)))
         (setf events
               (loop for bar-num from start-bar to end-bar
                  for bar = (get-bar sc bar-num search-player)
                  appending (rhythms bar)))
         (when ignore-rests
           (setf events (remove-if #'(lambda (x) (is-rest x)) events)))
         (when ignore-tied
           (setf events (remove-if #'(lambda (x) (is-tied-to x)) events))))
    (setf events
          (sort events #'(lambda (x y) (< (abs (- time (start-time x)))
                                          (abs (- time (start-time y)))))))
    (first events)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; todo: this needs an RT
;;; ****m* slippery-chicken/get-phrases
;;; DESCRIPTION
;;; This returns lists of events that make up phrases. Its notion of phrases
;;; is very simplistic but hopefully useful all the same: it's any sequence of
;;; sounding notes surrounded by rests.
;;; 
;;; ARGUMENTS
;;; - the slippery-chicken object
;;; - a single symbol or list of symbols representing the players from the
;;; ensemble. If nil all players will be processed.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :start-bar. An integer bar number where the process should start. If
;;; NIL we'll default to 1. Default = NIL. 
;;; - :end-bar. An integer bar number where the process should end. If
;;; NIL we'll default to the last bar. Default = NIL.
;;; - :pad. If a phrase starts or ends mid-bar, pad the first bar with leading
;;; rests and the last bar with trailing rests to ensure we have full bars.  
;;; 
;;; RETURN VALUE
;;; A list of sublists, one for each requested player. Each player sublist
;;; contains sublists also, with all the events in each phrase.
;;; 
;;; SYNOPSIS
(defmethod get-phrases ((sc slippery-chicken) players
                        &key start-bar end-bar pad)
;;; ****
  (unless players
    (setf players (players sc)))
  (unless (listp players)
    (setf players (list players)))
  (unless start-bar
    (setf start-bar 1))
  (unless end-bar
    (setf end-bar (num-bars sc)))
  (flet ((finalize (events player)
           (setf events (nreverse events))
           (when pad
             (setf events (pad-phrase sc events player)))
           events))
    (loop for player in players do
         (next-event sc player nil start-bar)
       collect
         (loop 
            with tmp = '()
            with result = '()
            with start-e 
            for e = (next-event sc player nil nil end-bar) 
            while e
            do
              (if (is-rest e)
                  (progn
                    (when (and tmp start-e)
                      (push (finalize tmp player) result))
                    (setf tmp '()))
                  (progn
                    (unless start-e
                      (setf start-e e))
                    (push e tmp)))
            finally 
              (when (and tmp start-e)
                (push (finalize tmp player) result))
              (return (nreverse result))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Fri Aug 29 14:33:05 2014 -- add rests to start/end a bar (bearing in
;;; mind we don't deal with rthm-seq-bars here, and therefore we can't
;;; consolidate rests).
(defmethod pad-phrase ((sc slippery-chicken) phrase parent)
  (let* ((first-event (first phrase))
         (last-event (first (last phrase)))
         (first-bar (get-bar sc (bar-num first-event) parent))
         (last-bar (get-bar sc (bar-num last-event) parent))
         (leading-rests (loop for i below (bar-pos first-event) collect
                             (force-rest (clone (get-nth-event i first-bar)))))
         (trailing-rests (loop for i from (1+ (bar-pos last-event)) below
                              (num-rhythms last-bar) collect
                              (force-rest (clone (get-nth-event i last-bar))))))
    (when leading-rests
      (setf phrase (append leading-rests phrase)))
    (when trailing-rests
      (setf phrase (append phrase trailing-rests)))
    phrase))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken/find-boundaries
;;; DESCRIPTION
;;; This methods tries to find structural boundaries (i.e section starts) in a
;;; complete piece. It does so by looking purely at event densities across bars
;;; and groups of bars; when these change sharply, a section change is noted.
;;; 
;;; ARGUMENTS
;;; - the slippery-chicken object to analyse
;;; 
;;; OPTIONAL ARGUMENTS
;;; - when looking at groups of bars, how many bars at a time? Default = 3
;;; - when looking at the sharpness of the envelope, what percentage change in
;;;   number of events per bar (based in the min/max in the overall envelope)
;;;   is considered a steep enough change. Default = 50 (%)
;;; 
;;; RETURN VALUE
;;; A list of bar numbers
;;;
;;; SYNOPSIS
(defmethod find-boundaries ((sc slippery-chicken) &optional
                            (sum-bars 3) (jump-threshold 50))
;;; ****
  ;; NB We test this function in sc-test-full.lsp, on slippery when wet
  (labels ((sum-from (envelope nth how-many)
             (loop for i from nth for n = (nth i envelope)
                repeat how-many while n sum n))
           (get-sums (envelope skip &optional (multiples-of 1))
             (loop for i below (length envelope) by skip
                collect (1+ i) 
                collect (sum-from envelope i multiples-of))))
    (let* ((nns (loop for bn from 1 to (num-bars sc)
                   for bars = (get-bar sc bn)
                   ;; for each bar number create a list of the number of
                   ;; events across all instruments
                   collect (loop for bar in bars sum (notes-needed bar))))
           ;; this does nothing more than splice in the bar number...
           (sf (get-sums nns 1))
           ;; ... but this sums on a bar-by-bar basis across the next several
           ;; bars i.e. bar 1-3, 2-4, 3-5...
           (sfm (get-sums nns 1 sum-bars))
           ;; now found boundaries: sharp changes in envelope curve
           (eb (envelope-boundaries sf jump-threshold)))
      ;; (print sfm)
      (decide-boundaries (get-clusters eb) sfm))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken/get-events-sorted-by-time
;;; DESCRIPTION
;;; Get all the events of all players between the given bars and then order
;;; them by ascending time.
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :start-bar. An integer that is the first bar in which the function is to
;;;   be applied to event objects. Default = 1.
;;; - :end-bar. NIL or an integer that is the last bar for which the event
;;;   objects should be returned. If NIL, the last bar of the  
;;;   slippery-chicken object is used. Default = NIL. 
;;; 
;;; RETURN VALUE
;;; A list of event objects.
;;; 
;;; SYNOPSIS
(defmethod get-events-sorted-by-time ((sc slippery-chicken)
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
       (setf
        events
        (sort
         events
         #'(lambda (e1 e2)
             ;; MDE Mon May 5 16:49:01 2014 -- if we've got two events that
             ;; start at the same time, place the event from the player higher
             ;; in the hierarchy/ensemble first
             (unless (and (start-time e1) (start-time e2))
               (error "slippery-chicken::get-events-sorted-by-time: ~
                       event's start-time is NIL. ~%Perhaps you need to call ~
                       update-slots."))
             (if (equal-within-tolerance
                  (start-time e1) (start-time e2))
                 (let ((pos1 (position (player e1) 
                                       (instruments-hierarchy sc)))
                       (pos2 (position (player e2) 
                                       (instruments-hierarchy sc))))
                   (unless (and pos1 pos2)
                     (error "slippery-chicken::get-events-sorted-by-time: ~
                             event must be lacking a player slot~%or it's not ~
                             in the instruments-hierarchy slot of sc.~%~
                             player1: ~a, player2: ~a, pos1: ~a, pos2: ~a"
                            (player e1) (player e2) pos1 pos2))
                   (< pos1 pos2))
                 (< (start-time e1) (start-time e2))))))
     appending events))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; An example of a note to follow, followed by actions arising from it. NB
;;; mnote is not an antescofo command, it's just a send name so can be changed
;;; to anything:
;;;      C4 (in midi-cents)  duration in beats   label
;;; NOTE 6000                6.0                 Bar2    
;;;    group a-unique-name
;;;    {   ; delay(beats) midi-pitch (we'll use cents) velocity chan dur
;;;            mnote 79        100             4n
;;;            1.0     mnote 72        100     8n
;;;            0.5     mnote 84        100     4n
;;;            1.0 mnote 79    100     8n
;;;            0.5 mnote 82    100     4nd
;;;            1.5 mnote 80    100     4n
;;;            1.0 mnote 79    100     8n
;;;    }

;;; ****m* slippery-chicken/write-antescofo
;;; DESCRIPTION
;;; Write an antescofo~ (Arshia Cont's/IRCAM's score follower MaxMSP external)
;;; score file. This allows you to specify a single player to follow (for now:
;;; no polyphonic following) and which players' events you'd like to be
;;; triggered along with this, in the form of MIDI notes (sent via antescofo's
;;; group action commands). Of course this doesn't imply that you have to use
;;; MIDI with antescofo, rather, that you have something that picks up midi
;;; note data and uses them (or ignores them) somehow or other. In any case,
;;; the MIDI notes sent consist of four messages: the MIDI note (in midi cents
;;; e.g. middle C = 6000), the velocity (taken as usual from the amplitude slot
;;; of the event), the channel, and the duration in beats. Each may be preceded
;;; by a delay, which will be relative to the previous NOTE or group MIDI note.
;;;
;;; Rehearsal letters already in the slippery-chicken piece will automatically
;;; be written into the antescofo~ file as labels/cues. E.g. if you have
;;; rehearsal letter A, it will show up in the antescofo~ file as "letter-A".
;;; Further labels/cues can be added to any slippery-chicken events and these
;;; will also be written into the antescofo~ file.
;;;
;;; Action messages can also be added to events and thus written into the
;;; antescofo~ file. You can push as many messages as you want into this list;
;;; they'll be reversed before writing out so that they occur in the order in
;;; which you added them. If you prefer you can call the event method
;;; add-antescofo-message. NB For the part we're following, we can add messages
;;; to rests but if it turns out we added messages to rests in other players'
;;; (i.e. group event parts) these won't be written to the antescofo~ file.
;;;
;;; Do remember to set the instruments-hierarchy slot of the slippery-chicken
;;; object so that the player you're going to follow is top of the list,
;;; otherwise some strange event ordering might occur in the antescofo~ file.
;;; 
;;; Bear in mind that if you want to write antescofo~ files without having to
;;; work within the usual slippery chicken workflow, you could generate events
;;; by any method, then put them into rthm-seq-bar objects before then calling
;;; bars-to-sc in order to create a slippery-chicken object by brute force (as
;;; it were).
;;; 
;;; A note about grace notes: For the voice we're following, antescofo~
;;; considers that grace notes are 'out of time' so have a duration of 0. But
;;; for group notes, which we're triggering, they have to have some duration;
;;; we default to the grace-note-duration of the event class (0.05 seconds by
;;; default, which will be written as a fraction of the beat at the current
;;; tempo, of course). Now this means our grace notes are not 'stealing' time
;;; from the previous note, as they do in performed music. This is not ideal,
;;; but if we've got a long group of grace notes, doing that might mean we end
;;; up with a negative duration for the previous note. So we simply make the
;;; grace notes short and allow/hope that the score follower catches up for us
;;; on the next recognised note.
;;;
;;; ARGUMENTS
;;; - The slippery-chicken object
;;; - the player who we'll follow (single player for now, as a symbol)
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :group-players (list of symbols). The players for whom midi-note events
;;;   will be written in the antescofo file as part of a "group" action. If
;;;   NIL, then we'll write all players' events except for the player we're
;;;   following. NB There's no reason why we couldn't include the player we're
;;;   following in these group commands (for unison playing between live and
;;;   digital instruments perhaps). The easist way to write group events for
;;;   all players is to write something like 
;;;   :group-players (players +your-sc-object+)
;;;   Default = NIL.
;;; - :bar-num-receiver. The MaxMSP receiver name to which bar numbers will be
;;;   sent for display other other purposes. Default = "antescofo-bar-num"
;;; - :midi-note-receiver. The MaxMSP receiver name to which midi notes will be
;;;   sent. Default = "midi-note", so a typical group output event could be 
;;;   0.0 midi-note 6500 12 4 0.6666667
;;; - :file. The name of the file to write. If NIL, then the file name will be
;;;   created from the slippery-chicken title and placed in (get-sc-config
;;;   'default-dir). Default = NIL. 
;;; - :group-duration-in-millisecs. Should we write the group notes (i.e. those
;;;    we generate) as fractions of a beat (NIL) or have antescofo~ convert
;;;    this to millisec duration according to the current tempo (T)? Default =
;;;    NIL.
;;; - :warn. Issue a warning when we write labels on group (rather than NOTE)
;;;    events?  (Because labels attached to group event notes can be written
;;;    and read, but they won't show up in MaxMSP as cues which you can jump
;;;    to.)  Default = T.
;;; - :messages-first. Write antescofo messages immediately before the note
;;;    data sent to the midi-note-receiver (T), or after (NIL)? Default = NIL.
;;; - :round-tempi. Round tempo values to the nearest integer? (If you don't do
;;;    this, then MIDI playback could become out of synch with
;;;    antescofo). Default = T. 
;;; - :include-program-changes. If T, we write any programme changes attached
;;;    to an event as "midi-program-change <programme> <channel>". If this is a
;;;    symbol or string then that ID will be used instead of
;;;    "midi-program-change". (Symbols will be made lowercase.) Default = T.
;;; - :makenote-compatible. If T, we'll write midi note information in the
;;;    following order: midi note (rounded to nearest chromatic note i.e. no
;;;    longer midi-cents), velocity, duration, channel. Default = NIL.
;;; 
;;; RETURN VALUE
;;; The number of NOTE plus action events we've written. We also print to the
;;; terminal the number of events and actions separately, which should
;;; then correspond to what Antescofo~ prints when it loads the score in MaxMSP.
;;; 
;;; EXAMPLE
#|
;;; Follow the violin part and generate group events for all other parts ;
(let* ((mini
        (make-slippery-chicken
         '+mini+
         :title "antescofo test"
         :ensemble '(((vn (violin :midi-channel 1))
                      (va (viola :midi-channel 2))
                      (vc (cello :midi-channel 3))))
         :set-palette '((1 ((f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1 1 1)))
         :tempo-map '((1 60))
         :rthm-seq-palette '((1 ((((4 4) { 3 tq tq tq } +q e (s) s)))))
         :rthm-seq-map '((1 ((vn (1 1 1))
                             (va (1 1 1))
                             (vc (1 1 1))))))))
  ;; Adding a label (probably wouldn't need one in bar 1, but to illustrate) 
  (setf (asco-label (get-event mini 1 1 'vn)) "test-label")
  ;; start the (fictitious) vocoder when the first cello note in bar 2 is played 
  (push "max-receiver1 start-vocoder" (asco-msgs (get-event mini 2 1 'vc)))
  (write-antescofo mini 'vn :file "/tmp/asco-test.txt"))

-->
******* section (1)
Getting notes for VN
Getting notes for VA
Getting notes for VC
Shortening short, fast leaps...
Shortened 0 large fast leaps
"/tmp/asco-test.txt" 
Antescofo~ score written successfully with 15 events and 34 actions.
49

The generated file will begin something like this:

; antescofo~ score generated by slippery chicken version ;
; 1.0.4 (svn revision 4733 2014-01-15 11:27:10) ;
; at 12:02:06 on Thursday the 8th of May 2014 ;
BPM 60
antescofo-bar-num 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;
NOTE 6200 0.6666667 test-label
group bar1.1 {
0.0 midi-note 6000 12 2 0.6666667 
0.0 midi-note 6400 12 3 0.6666667 
}
NOTE 6200 0.6666667 
group bar1.2 {
0.0 midi-note 6000 12 2 0.6666667 
0.0 midi-note 6400 12 3 0.6666667 
}
NOTE 6200 1.6666667 
group bar1.3 {
0.0 midi-note 6000 12 2 1.6666667 
0.0 midi-note 6400 12 3 1.6666667 
}
NOTE 6200 0.5 
group bar1.4 {
0.0 midi-note 6000 12 2 0.5 
0.0 midi-note 6400 12 3 0.5 
}
NOTE 0 0.25 
NOTE 6200 0.25 
group bar1.5 {
0.0 midi-note 6000 12 2 0.25 
0.0 midi-note 6400 12 3 0.25 
antescofo-bar-num 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;
}
NOTE 6200 0.6666667 
group bar2.1 {
0.0 midi-note 6000 12 2 0.6666667 
0.0 midi-note 6400 12 3 0.6666667 
max-receiver1 start-vocoder
}
NOTE 6200 0.6666667 
...

|#
;;; SYNOPSIS
(defmethod write-antescofo ((sc slippery-chicken) follow-player
                            &key group-players file (warn t)
                              (group-duration-in-millisecs nil)
                              messages-first
                              ;; could be T, a string, or a symbol
                              ;; todo: include control changes also?
                              (include-program-changes t)
                              (round-tempi t)
                              makenote-compatible
                              (midi-note-receiver "midi-note")
                              (bar-num-receiver "antescofo-bar-num"))
;;; ****
  (unless (and follow-player (member follow-player (players sc)))
    (error "slippery-chicken::write-antescofo: 2nd argument must be a player ~
            in the ensemble: ~a" follow-player))
  (unless group-players
    (setf group-players (remove follow-player (players sc))))
  (unless file
    (setf file (format nil "~a~a.asco.txt"
                       (get-sc-config 'default-dir)
                       (filename-from-title (title sc)))))
  (let ((last-time 0.0)
        (event-count 0)     ; the events for the player we're following ;
        (action-count 0)    ; each event in a group and each receiver we send ;
                                        ; something to ;
        (top-player (first (players sc)))
        ;; this should force a new BPM to be written as this tempo will never ;
        ;; be used (famous last words)  ;
        (tempo (make-tempo -1 :beat 'w))
        (bar-num 0)
        (prog-receiver (cond ((eql T include-program-changes)
                              "midi-program-change")
                             ((not include-program-changes)
                              NIL)
                             ((symbolp include-program-changes)
                              (string-downcase
                               (string include-program-changes)))
                             ((stringp include-program-changes)
                              include-program-changes)
                             (t (error "slippery-chicken::write-antescofo: ~
                                        ~a is not a valid argument for ~
                                        :include-program-changes"
                                       include-program-changes))))
        ;; the group number for this bar ;
        (groupi 0)
        ;; we'll have to write the } next time we see an event for the ;
        ;; follow-player                ;
        in-group 
        rehearsal-letter pitch duration delay)
    (flet ((asco-data (event follower?)
             ;; write midi-notes in cents. this will be a list if it's a chord ;
             (setf pitch 
                   (if (is-rest event)
                       ;; rests don't count in the midi-notes we generate, but ;
                       ;; they do count for the followed player ;
                       -1
                       (midi-note-float (pitch-or-chord event) t))
                   ;; Durations are expressed as fractions/multiples of a
                   ;; beat. There's no concept of meter, as such, in
                   ;; antescofo. In e.g. 6/8 time the BPM would be entered as
                   ;; e.g. 120 or something (where we'd mean dotted quarter =
                   ;; 120, though antescofo doesn't need to know our beat type),
                   ;; and then each 1/8 note would be an antescofo duration of
                   ;; 0.33, just as if it were a triplet in 2/4 time. So the
                   ;; duration is the event's compound-duration * (tempo's
                   ;; beat-value / 4) MDE Fri May 9 10:40:17 2014 -- express
                   ;; duration as a fraction if reasonable ;
                   duration (if (is-grace-note event)
                                ;; MDE Wed May 14 18:07:08 2014 -- see note in
                                ;; documentation above re. grace notes.
                                (if follower? 0.0 
                                    (/ (grace-note-duration event)
                                       (beat-dur tempo)))
                                (rationalize-if-simple
                                 (* (compound-duration event)
                                    (/ (beat-value tempo) 4.0))
                                 3))
                   ;; only used in the group events, as rests are written in
                   ;; the player we're following
                   delay (max 0.0
                              (rationalize-if-simple
                               (/ (- (start-time event) last-time)
                                  (beat-dur tempo))
                               3)))
             ;; MDE Mon Jan 26 13:28:31 2015 -- don't write chords when we've
             ;; only got one pitch, even if the pitch slot of the event says
             ;; it's a chord
             (when (and (listp pitch) (= 1 (length pitch)))
               (setf pitch (first pitch))))
           (write-msgs (event stream group? &optional force-delay?)
             (when prog-receiver
               (loop for pc in (midi-program-changes event) do
                    (format stream "~&~a~a ~a ~a" 
                            (if group? "          " "      ")
                            prog-receiver (second pc) (first pc))))
             (when (asco-msgs event)
               (loop for msg in (reverse (asco-msgs event)) do
                    (incf action-count)
                    (format stream "~&~a~a ~a" 
                            (if group? "          " "      ")
                            (if (or force-delay? messages-first) delay 0.0)
                            msg))))
           (write-group-note (p event midi-channel stream write-labels) 
             (let ((dur
                    (cond (group-duration-in-millisecs
                           (format nil "@beat2ms(~a)" duration))
                          ;; can't have simple fractions here rather they have
                          ;; to be evaluated e.g. (2/3) 
                          ((rationalp duration)
                           (format nil "(~a)" duration))
                          (t (float duration))))
                   (vel (min 127 (floor (* 127.0 (amplitude event))))))
               ;; p = current pitch (of chord or note)
               ;; don't write rests
               (unless (= -1 p)
                 (format stream "~&          ~a ~a ~a ~a ~a ~a ~a"
                         (if messages-first 0.0 delay)
                         midi-note-receiver 
                         (if makenote-compatible (floor p 100) p)
                         vel
                         (if makenote-compatible dur midi-channel)
                         (if makenote-compatible midi-channel dur)
                         (if (and write-labels (asco-label event))
                             (progn
                               (when warn
                                 (warn "slippery-chicken::write-antescofo: ~
                                        Labels attached to group event notes ~
                                        ~%can be written and read, but they ~
                                        will not show up in MaxMSP as cues ~
                                        ~%which you can jump to: ~a, ~a"
                                       (bar-num event) (player event)))
                               (asco-label event))
                             ""))))))
      (with-open-file (out file :direction :output :if-does-not-exist :create
                           :if-exists :rename-and-delete :external-format :utf-8)
        (format out "~&; --------------------------------------------------~
                      --------~
                     ~%; antescofo~~ score generated by slippery chicken ~
                     ~%; version ~a~%; at ~a~
                     ~&; --------------------------------------------------~
                      --------"
                +slippery-chicken-version+ (get-time))
        (when group-duration-in-millisecs
          (format out "~%~%@FUN_DEF beat2ms($X) {~%    ~
                        1000*$X*60.0/$RT_TEMPO~%}"))
        ;; get all the events in the piece, ordered by time
        (loop for e in (get-events-sorted-by-time sc)
           for follow = nil
           do
           ;; new bar
             (unless (= (bar-num e) bar-num)
               (unless (zerop bar-num)
                 ;; remember rehearsal-letters are attached to the previous bar
                 (setf rehearsal-letter (rehearsal-letter 
                                         (get-bar sc bar-num top-player))))
               (setf bar-num (bar-num e)
                     groupi 0)
               (format out "~&;----------------------------------------------~
                            -------------~%")
               ;; MDE Fri Jan 23 16:27:41 2015 -- this is just about getting
               ;; the bar number written with the correct delay. when writing
               ;; following and group players below we're ignoring rests, but
               ;; in this block we'll hit the first event of the following
               ;; player first, whether it's a rest, a tied, or a struck note,
               ;; so this is the moment to write the bar number.
               (let ((ec (clone e)))    
                 (asco-data ec nil)
                 (setf (is-rest ec) nil
                       (asco-msgs ec) nil)
                 (push (format nil "~a ~a" bar-num-receiver bar-num)
                       (asco-msgs ec))
                 (write-msgs ec out in-group t)
                 (setf last-time (start-time ec))))
             (when (and (tempo-change e)
                        (not (tempo-equal tempo (tempo-change e))))
               (setf tempo (tempo-change e))
               (incf action-count)
               ;; don't use BPM, use antescofo::tempo instead so that we can
               ;; include these in groups and set delays on them, as described
               ;; by Arshia at http://tinyurl.com/q9pvudx
               (format out "~&~aantescofo::tempo ~a"
                       (if in-group "          " "      ")
                       (if round-tempi
                           (round (bpm tempo))
                           (bpm tempo))))
           ;; the following player ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
             (when (and (equalp (player e) follow-player)
                        ;; remember not to include tied notes, as these are
                        ;; handled by compound duration when the note is struck
                        (not (is-tied-to e)))
               (setf follow t)
               (asco-data e t)
               (when messages-first
                 (write-msgs e out nil))
               (unless (and (numberp pitch) (= -1 pitch)) ; a rest
                 (incf event-count))
               (when in-group
                 (format out "~&      }")
                 (setf in-group nil))
               ;; antescofo~ will read a line with two labels but the second one
               ;; will be ignored
               (when (and rehearsal-letter (asco-label e))
                 (when (get-sc-config 'asco-two-labels-warning)
                   (warn "~&slippery-chicken::write-antescofo:  ~
                        An event can't have a label and a ~%rehearsal letter ~
                        attached; ignoring rehearsal-letter (bar~a , ~a)" 
                         (bar-num e) (player e)))
                 (setf rehearsal-letter nil))
               (format out "~&~a ~a ~a ~a" 
                       (if (listp pitch) "CHORD" "NOTE")
                       ;; write any labels (e.g. rehearsal letter) after the
                       ;; NOTE or CHORD
                       pitch duration 
                       (cond (rehearsal-letter
                              (prog1
                                  (format nil "letter-~a" 
                                          rehearsal-letter)
                                (setf rehearsal-letter nil)))
                             ((asco-label e) (asco-label e))
                             (t "")))
               (unless messages-first
                 (write-msgs e out nil))
               (setf last-time (start-time e)))
           ;; the other players ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           ;; we could include the follow-player in the group-players in order
           ;; to double the live player with something electronic, hence the
           ;; two 'whens' here (following player and other players) rather than
           ;; an if.
             (when (and (member (player e) group-players)
                        (needs-new-note e))
               (unless in-group
                 (setf in-group t)
                 (format out "~&      group bar~a.~a {" bar-num (incf groupi)))
               (if follow
                   ;; because we've already written the follower and now we
                   ;; just want to 'play' it too
                   (setf delay 0) 
                   ;; we're not writing group events for the player we're
                   ;; following  
                   (progn
                     (asco-data e nil)
                     (when messages-first
                       (write-msgs e out t))))
               (if (listp pitch) ; i.e. a chord
                   (loop for p in pitch and pobj in (data (pitch-or-chord e)) do
                        (incf action-count)
                        (write-group-note p e (midi-channel pobj) out
                                          (not follow)))
                   (progn ; a single pitch
                     (incf action-count)
                     ;; MDE Mon Jan 26 14:37:23 2015 -- hyperboles always
                     ;; thinks we've got chords so single pitches are also
                     ;; chord objects. handle this here rather than lower
                     ;; down. 
                     (when (chord-p (pitch-or-chord e))
                       (setf (slot-value e 'pitch-or-chord) 
                             (first (data (pitch-or-chord e)))))
                     (write-group-note pitch e (midi-channel (pitch-or-chord e))
                                       out (not follow))))
               (when (and (not messages-first) (not follow))
                 (write-msgs e out t))
               (setf last-time (start-time e))))
        ;; end of event loop
        (when in-group (format out "~&      }"))
        (format out "~&; ----------------------------------------------------~
                      ------~
                     ~%; End of file~%")))
    ;; this statement corresponds to what antescofo~ prints to the max window
    ;; when it's loaded a score
    (print file)
    (format t "~&Antescofo~~ score written successfully with ~a events and ~
               ~a actions."
            event-count action-count)
    (+ event-count action-count)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken/empty-bars?
;;; DESCRIPTION
;;; Test whether players have rest/empty bars within a given range.
;;; 
;;; ARGUMENTS
;;; - the slippery-chicken object
;;; - the start bar (integer, inclusive)
;;; - the end bar (integer, inclusive)
;;; 
;;; OPTIONAL ARGUMENTS
;;; - the players we want to test. Either a list of player IDs, a single ID, or
;;;   nil if all players should be tested. Default = NIL = all players.
;;; - T or NIL to indicate whether a bar of only rests (instead of a full bar
;;;   rest) should be considered empty. Default = NIL.
;;; 
;;; RETURN VALUE
;;; T if all players have rest bars within the given range, NIL if no.
;;; 
;;; SYNOPSIS
(defmethod empty-bars? ((sc slippery-chicken) start-bar end-bar
                        &optional players all-rests)
;;; ****
  (unless players (setq players (players sc)))
  (unless (listp players) (setq players (list players)))
  (loop for player in players
     for empty = 
       (loop for bar-num from start-bar to end-bar 
          for bar = (get-bar sc bar-num player)
            ;; MDE Fri Nov 23 18:58:40 2018 -- all-rests?
          unless (or (is-rest-bar bar) (and all-rests (all-rests? bar)))
          do (return nil)
          finally (return t))
     do (unless empty (return nil))
     finally (return t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;; MDE Fri Nov 13 17:37:46 2015 -- make sure we start with the right number of 
;;; staff lines 
(defmethod initial-staff-lines ((sc slippery-chicken)) 
  (loop for p in (players sc) 
     for ins = (get-starting-ins sc p) 
     for lines = (staff-lines ins) 
     for e1 = (get-event sc 1 1 p) 
     do 
       (unless (= 5 lines)
         ;; MDE Tue Dec  3 17:24:39 2019 -- 4th element should be the instrument
         ;; object 
         (setf (instrument-change e1) (list nil nil lines ins)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Mon Jun 13 12:33:24 2016 -- called by midi-play by default
(defmethod handle-hairpins ((sc slippery-chicken) &key players (start-bar 1)
                                                    end-bar)
  (unless players (setq players (players sc)))
  (force-list players)
  (loop for p in players do
       (handle-hairpins-aux sc p start-bar end-bar))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod handle-hairpins-aux ((sc slippery-chicken) player start-bar end-bar)
  (next-event sc player nil start-bar end-bar)
  (let* ((hairpins '())
         (start 0.0)
         (inside nil)
         (events '()))
    (loop for event = (next-event sc player nil nil end-bar)
       while event do
         ;; (print event)
         (cond ((or (has-mark event 'cresc-beg)
                    (has-mark event 'dim-beg))
                (push event events)
                (setq inside t
                      start (amplitude event)))
               ((or (has-mark event 'cresc-end)
                    (has-mark event 'dim-end))
                (push event events)
                (push (list start (amplitude event) (reverse events))
                      hairpins)
                (setq inside nil
                      events nil))
               (inside (push event events))))
    ;; (print hairpins)
    (loop for hairpin in (reverse hairpins) do
         (handle-hairpins-aux2 hairpin))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Mon Jan 30 18:38:43 2017 
;;; ****m* slippery-chicken/count-microtones
;;; DATE
;;; January 30th 2017 
;;; 
;;; DESCRIPTION
;;; Count the number of microtonal pitches in the slippery-chicken object. 
;;; 
;;; ARGUMENTS
;;; - the slippery-chicken object
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :players. the players for whom the count should be executed; either a
;;;   single player symbol or a list of symbols
;;; - :start-bar. The bar number in which the counting should begin. Default =
;;;   1
;;; - :end-bar. The bar number in which counting should end. Default = NIL
;;;   which means count through the final bar.
;;; 
;;; RETURN VALUE
;;; Four values: the number of microtones, the number of non-microtones, the
;;; total number of pitches, the percentage of microtonal pitches.
;;;
;;; Note that the total number of pitches will almost certainly be different
;;; from the number of events as some events will be rests, others single pitch
;;; notes, yet others chords.
;;; 
;;; EXAMPLE
#|
(count-microtones +jitterbug+)
1420
2312
3732
38.049305
|#
;;; SYNOPSIS
(defmethod count-microtones ((sc slippery-chicken) &key players (start-bar 1)
                                                     end-bar)
;;; ****
  (unless players (setq players (players sc)))
  (let ((chromatic 0)
        (micro 0)
        total)
    (loop for p in (force-list players)
       for events = (get-events-from-to sc p start-bar 1 end-bar)
       do
         ;; (print (length events))
         ;; (print events)
         (loop for e in events do
              (unless (is-rest e)
                (multiple-value-bind
                      (mt total)
                    (microtonal e t)
                  (incf micro mt)
                  (incf chromatic (- total mt))))))
    (setq total (+ micro chromatic))
    (values micro chromatic total
            ;; percentage
            (* (/ micro total) 100.0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Thu Feb  9 19:56:22 2017 
;;; ****m* slippery-chicken/player-ambitus
;;; DATE
;;; February 9th 2017
;;; 
;;; DESCRIPTION
;;; Find the highest and lowest events in the (given bars of the) piece,
;;; printing if requested.
;;; 
;;; ARGUMENTS
;;; - the slippery-chicken object
;;; - the player ID (generally symbol)
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :written. T or NIL to indicate if the written (as opposed to sounding)
;;;   pitches of the events should be handled/returned/printed. Default = NIL =
;;;   sounding pitches
;;; - :print. Whether to print results to the Lisp interpreter.
;;; - :start-bar. The bar number to start processing. Default = 1.
;;; - :start-bar. The bar number to stop processing (inclusive). Default = NIL =
;;;   the last bar.
;;; - :attacked-only. Whether only to include attacked notes. Default = T.
;;; 
;;; RETURN VALUE
;;; two values: the lowest event and the highest event.
;;; 
;;; SYNOPSIS
(defmethod player-ambitus ((sc slippery-chicken) player &key written print
                                                          (start-bar 1) end-bar
                                                          (attacked-only t))
;;; ****
  (let (high low e-high e-low)
    ;; initialize next-event
    (next-event sc player attacked-only start-bar)
    (loop for e = (next-event sc player attacked-only nil end-bar)
          while e
          do
             (unless (is-rest e)
               (setq e-high (get-pitch-highest e written)
                     e-low (get-pitch-lowest e written))
               ;; RP  Thu Apr 27 15:23:06 2023
               ;; initialize both values with pitch of first event
               ;; this is necessary as it is not clear at the beginning,
               ;; whether the pitch marks the lower or upper boundary
               ;; of the range
               (when (not high)
                 (setq high e-high
                       low e-low))
               (if (pitch> e-high high)
                   (setq high e-high)
                   (when (pitch< e-low low)
                     (setq low e-low)))))
    (when print
      (format t "~&~a: low: ~a high: ~a" player (data low) (data high)))
    (values low high)))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Sat Aug  5 18:09:57 2017 -- 
;;; ****m* slippery-chicken/players-ambitus
;;; DATE
;;; August 5th 2017
;;; 
;;; DESCRIPTION
;;; Call the player-ambitus method for each player in the slippery-chicken
;;; object. 
;;; 
;;; ARGUMENTS
;;; see player-ambitus method
;;; 
;;; RETURN VALUE
;;; the lowest and highest events of all the players
;;; 
;;; SYNOPSIS
(defmethod players-ambitus ((sc slippery-chicken)
                            &key written print (start-bar 1) end-bar)
;;; ****
  (let (high low)
    (loop for p in (players sc) do
         (multiple-value-bind
               (l h)
             (player-ambitus sc p :written written :print print
                             :start-bar start-bar :end-bar end-bar)
           (when (or (not high) (pitch> h high))
             (setq high h))
           (when (or (not low) (pitch< l low))
             (setq low l))))
    (when print
      (format t "~&all players: high: ~a low: ~a" (data high) (data low)))
    (values low high)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-instruments-for-players-at-bar ((sc slippery-chicken)
                                               players bar-num)
  (loop for player in players collect
       (get-instrument-for-player-at-bar player bar-num sc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken/find-end-tie
;;; DATE
;;; November 8th 2018, Heidhausen
;;; 
;;; DESCRIPTION
;;; Find the event which is the last in a group of tied notes.
;;; 
;;; ARGUMENTS
;;; - the slippery-chicken object
;;; - the event object at the start or in the middle of a the group of tied
;;;   notes  
;;; 
;;; OPTIONAL ARGUMENTS
;;; - the function, generally some kind of error, to call if the given event is
;;;   not tied from. Default = #'error.
;;; 
;;; RETURN VALUE
;;; the last event object which is tied to or NIL if the given event is not tied
;;; from. 
;;; 
;;; SYNOPSIS
(defmethod find-end-tie ((sc slippery-chicken) (e event)
                         &optional (on-fail #'error))
;;; ****
  (if (is-tied-from e)
      (let ((bar-num (bar-num e))
            (e-num (+ 2 (bar-pos e)))
            current)
        ;; no endless loops; surely < 10000 notes are tied?
        (loop repeat 10000 do
           ;;                                                     no error
             (setq current (get-event sc bar-num e-num (player e) nil)) 
             (if current
                 (if (is-tied-from current)
                     (incf e-num)
                     (return))
                 (progn
                   (setq e-num 1)
                   (incf bar-num))))
        current)
      (when (functionp on-fail)
        (funcall on-fail "slippery-chicken::find-end-tie: event is not ~
                          tied-from: ~a" e))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken/section-densities
;;; DATE
;;; January 12th 2019, Heidhausen
;;; 
;;; DESCRIPTION
;;; Calculate the densities of notes or rests in the sections of a
;;; slippery-chicken object. Density is the average number of notes/rests per
;;; bar across a section.
;;; 
;;; ARGUMENTS
;;; - a slippery-chicken object
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :slot. The section slot to use. 'num-score-notes is the default and makes
;;;   sense in that ties indicate a higher level of density than just attacked
;;;   notes by themselves, but further useful slots could be 'num-notes
;;;   (attacks) or indeed 'num-rests
;;; - :percent. Whether to return values as percentages (where 0 would be the
;;;   least dense section and 100 the most). If NIL the actual average notes per
;;;   bar in each section would be returned. Default = T.
;;; 
;;; RETURN VALUE
;;; a list of density numbers, one per section/subsection.
;;; 
;;; SYNOPSIS
(defmethod section-densities ((sc slippery-chicken)
                              &key (slot 'num-score-notes) (percent t))
;;; ****
  (let ((densities (loop for ref in (get-section-refs sc 1 (num-bars sc))
                      for section = (get-data-data ref (piece sc))
                      collect (float (/ (slot-value section slot)
                                        (num-bars section))))))
    (if percent
        (new-limits (make-sclist densities))
        densities)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken/get-section-bar-nums
;;; AUTHOR
;;; Daniel Ross (mr.danielross[at]gmail[dot]com) 
;;; 
;;; DATE
;;; Mon 13 Jan 2020 15:23:17 GMT
;;; 
;;; DESCRIPTION
;;; Get the start and end bar numbers of each section of a slippery chicken
;;; piece, handlily formatted in an assoc list.
;;; 
;;; ARGUMENTS
;;; A slippery chicken object
;;; 
;;; OPTIONAL ARGUMENTS
;;; Keyword args:
;;; :start - the first section to look at. (Default = 1)
;;; :end - the last section to look at. (Default = nil, look to the end.)
;;; 
;;; RETURN VALUE
;;; An assoc list where each key is a section number and its data is a list
;;; containing the start and end bar numbers of that section.
;;;
;;; EXAMPLE
#|
(let ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((sax (alto-sax :midi-channel 1))))
          :set-palette '((1 ((c2 d2 g2 a2 e3 fs3 b3 cs4 fs4 gs4 ds5 f5 bf5)))) 
          :set-map '((1 (1 1 1 1 1))
                     (2 (1 1 1 1 1))
                     (3 (1 1 1 1 1)))
          :rthm-seq-palette '((1 ((((4 4) h q e s s))
                                  :pitch-seq-palette ((1 2 3 4 5)))))
          :rthm-seq-map '((1 ((sax (1 1 1 1 1))))
                          (2 ((sax (1 1 1 1 1))))
                          (3 ((sax (1 1 1 1 1))))))))
      (get-section-bar-nums mini))

=> 
ASSOC-LIST: warn-not-found T
CIRCULAR-SCLIST: current 0
SCLIST: sclist-length: 3, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, 
                     this: NIL, 
                     next: NIL
NAMED-OBJECT: id: SECTION-BAR-NUMS, tag: NIL, 
data: (
NAMED-OBJECT: id: 1, tag: NIL, 
data: (1 5)
**************

       
NAMED-OBJECT: id: 2, tag: NIL, 
data: (6 10)
**************

       
NAMED-OBJECT: id: 3, tag: NIL, 
data: (11 15)
**************
)
**************
|#
;;; SYNOPSIS
(defmethod get-section-bar-nums ((sc slippery-chicken)
                                 &key (start 1) (end nil)
                                   (assoc-list-id 'section-bar-nums))
;;; ****
  (unless end (setf end (get-num-sections sc)))
  (when (< end start)
    (error "slippery-chicken::get-section-bar-nums: End bar (~a) cannot ~
            be lower than start bar (~a)" end start))
  (let ((section-assoc-list
         (make-assoc-list
          'sc-list
          (loop for s from start to end
             for section = (get-section sc s)
             collect
               (list s (list (start-bar section) (end-bar section)))))))
    (setf (id section-assoc-list) assoc-list-id)
    section-assoc-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken/get-set-for-bar-num
;;; DATE
;;; July 11th 2020, Heidhausen
;;; 
;;; DESCRIPTION
;;; Get the complete-set object from the set-palette that was used in the
;;; generation of a specific bar.
;;; 
;;; ARGUMENTS
;;; - the slippery-chicken object
;;; - the bar number (integer)
;;; 
;;; RETURN VALUE
;;; a complete-set object or NIL if the bar doesn't exist or the first event of
;;; the bar has no set-ref slot value, for any of the players, for some reason
;;; (e.g. not initialized by make-slippery-chicken)
;;; 
;;; SYNOPSIS
(defmethod get-set-for-bar-num ((sc slippery-chicken) bar-num)
;;; ****
  (let* ((bar (get-bar sc bar-num (first (players sc))))
         (section-ref (when bar (butlast (player-section-ref bar))))
         (section (when section-ref (get-data-data section-ref (set-map sc))))
         (set-ref (when section (nth (nth-seq bar) section)))
         (set (get-data set-ref (set-palette sc))))
    (if set
        set
        (warn "slippery-chicken::get-set-for-bar-num: no set-ref found ~%~
               at bar ~a" bar-num))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken/get-pitch-symbols-for-player
;;; DATE
;;; April 27th 2022, Heidhausen
;;; 
;;; DESCRIPTION
;;; Get a list of all the pitches (as symbols) used in a piece on a
;;; player-by-player basis.
;;; 
;;; ARGUMENTS
;;; - the slippery-chicken object
;;; - the player (symbol) or list of players we want the pitches for. NB if nil,
;;; pitches for all players will be returned.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - start bar number. If nil, this will be 1. Default = NIL.
;;; - end bar number. If nil, this will be the last bar. Default = NIL.
;;; - whether written as opposed to sounding note symbols should be
;;;   returned. Default = NIL.
;;; 
;;; RETURN VALUE
;;; A nested list: each element is itself a two-element list where the first
;;; element is the player symbol and the second is an ascending ordered list of
;;; the pitches, with enharmonic equivalents removed.
;;; 
;;; EXAMPLE
#|
(get-pitch-symbols-for-player +percussion+ 'perc)
((PERC
  (B4 C5 DF5 D5 DS5 E5 F5 GF5 G5 AF5 A5 BF5 B5 C6 CS6 D6 EF6 E6 F6 FS6 G6 AF6
   A6 BF6)))
|#
;;; SYNOPSIS
(defmethod get-pitch-symbols-for-player ((sc slippery-chicken) player
                                         &optional start-bar end-bar
                                           written)
;;; ****
  (unless start-bar (setq start-bar 1))
  (unless end-bar (setq end-bar (num-bars sc)))
  (setq player (if player (force-list player) (players sc)))
  (loop for plyr in player
        collect
        (list plyr
              ;; now for ease of scanning, remove enharmonics and order by
              ;; ascending pitch
              (get-pitch-symbols
               (rm-duplicates
                (make-chord 
                 (remove-duplicates 
                  (loop for bar-num from start-bar to end-bar
                        append
                        (remove nil (get-pitch-symbols
                                     (get-bar sc bar-num player)
                                     written))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken/note-count
;;; DATE
;;; May 20th 2022, Heidhausen
;;; 
;;; DESCRIPTION
;;; for statistical purposes, generate a list of the count of all the notes in
;;; the whole or part of a piece.
;;; 
;;; ARGUMENTS
;;; - the slippery-chicken object
;;; - the player: either a single symbol, a list of players, or if nil, all the
;;; players 
;;; 
;;; OPTIONAL ARGUMENTS
;;; - how to sort the result. Default = 'pitch meaning that the pitches will
;;; ascend irrespective of their counts. Anything else (e.g. nil) will print in
;;; the order of lowest to highest note count
;;; - the start bar number. Default = NIL = 1
;;; - the end bar number. Default = NIL = last bar
;;; - T or NIL to indicate sounding (nil) or written (t) pitches should be
;;; examined.  Default = NIL = sounding.
;;; 
;;; RETURN VALUE
;;; a list of (note count) pairs
;;; 
;;; SYNOPSIS
(defmethod note-count ((sc slippery-chicken) player
                       &optional (sort 'pitch) start-bar end-bar written)
;;; ****
  (let ((result '()))
    (map-over-events sc start-bar end-bar player
                     #'(lambda (e)
                         (unless (is-rest e)
                           (let* ((porc (get-porc e written))
                                  (pos (position porc result :key #'first :test
                                                 #'(lambda (p1 p2)
                                                     (pitch= p1 p2 t)))))
                             (if pos 
                                 (incf (second (nth pos result)))
                                 (push (list porc 1) result))))))
    (mapcar #'(lambda (l) (list (data (first l)) (second l)))
            (if (eq sort 'pitch)
                (sort result #'pitch< :key #'first)
                (sort result #'< :key #'second)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken/quarter-duration
;;; DATE 
;;; 23rd August 2023
;;; 
;;; DESCRIPTION
;;; return the duration in quarter notes of a slippery-chicken piece
;;; 
;;; ARGUMENTS
;;; the slippery chicken object
;;; 
;;; OPTIONAL ARGUMENTS
;;; start-bar and end-bar, both integers or if nil will default to 1 and the
;;; number of bars in the piece 
;;; 
;;; RETURN VALUE
;;; a float: total number of quarters
;;; 
;;; SYNOPSIS
(defmethod quarter-duration ((sc slippery-chicken) &optional start-bar end-bar)
;;; ****
  (apply #'+ (map-over-bars sc start-bar end-bar (first (players sc))
                            #'bar-duration)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; returns a list of numbers which refer to the number of bars in a particular
;;; meter. The order of the numbers/time-sigs is aligned to the all-time-sigs
;;; slot of the rthm-seq-bar class.
(defmethod count-time-sigs ((sc slippery-chicken))
  (let* (counts)
    (map-over-bars
     sc 1 nil (first (players sc))
     #'(lambda (rsb)
         (unless counts
           (setq counts (ml 0 (length (all-time-sigs rsb)))))
         ;; remember: the time-sig slot is just the index of the time-sig object
         ;; in the all-time-sigs class slot
         (incf (nth (time-sig rsb) counts))))
    (unless (= (num-bars sc) (apply #'+ counts)) ; sanity check
      (error "slippery-chicken::count-time-sigs returned ~a ~%but there ~
              are ~a bars" counts (num-bars sc)))
    counts))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken/find-gps
;;; DATE
;;; June 9th 2025
;;; 
;;; DESCRIPTION
;;; Find all the bar-numbers of the general pauses (hence find-gps: nothing to
;;; do with map coordinates) across a given bar range. A general pause here is
;;; defined as is when all the players have a complete rest bar. NB Consecutive
;;; rest bars count as part of the same general pause.
;;; 
;;; ARGUMENTS
;;; - the slippery- chicken object
;;;
;;; OPTIONAL ARGUMENTS
;;; keyword arguments
;;; - the start bar. Default = 1.
;;; - the end bar. Default = NIL = end of piece.
;;; 
;;; 
;;; RETURN VALUE
;;; a list of bar numbers
;;; 
;;; SYNOPSIS
(defmethod find-gps ((sc slippery-chicken) &key (start-bar 1) end-bar)
;;; ****
  (unless end-bar (setq end-bar (num-bars sc)))
  (let* ((result '())
         (pushok t))
    (loop for bar-num from start-bar to end-bar
          for pbars = (get-bar sc bar-num)
          do
             (if (every #'is-rest-bar pbars)
               (when pushok
                 ;; don't push consecutive rest bar numbers as all the rest bars
                 ;; count as a general pause 
                 (setq pushok nil)
                 (push (bar-num (first pbars))
                       result))
               (setq pushok t)))
    (nreverse result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Related functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Sat Mar 31 09:56:33 2012 -- If any of the sc slots have defaults it's
;;; best to make them the default args here too. But the init method also sets
;;; a couple of them in case they've been made nil and would thus cause an
;;; error.

;;; ****f* slippery-chicken/make-slippery-chicken
;;; DESCRIPTION
;;; Make a slippery-chicken object using the specified data. This is the
;;; function that will be used most often to "put it all together", and many of
;;; its slots require full objects of other classes rather than just straight
;;; data. These objects, such as rthm-seq-palette, rthm-seq-map etc, are also
;;; documented in detail elsewhere in the robodoc and the user's manual.
;;;
;;; NB Usually, when no set pitches are available for an instrument, an error
;;; will be signalled. As of October 2018, you can instead allocate an 'empty
;;; sequence' (rests) in such cases by setting the following before calling
;;; make-slippery-chicken:
;;; (set-sc-config 'pitch-seq-no-pitches-error nil)
;;; 
;;; ARGUMENTS
;;; - A symbol that is the name/ID of the object. The value passed to this
;;;   argument will be made into a global variable, so that the newly created
;;;   slippery-chicken object and the data it contains remain in memory and can
;;;   be accessed and modified after the object is generated.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; NB: Although these arguments are technically optional, the slippery-chicken
;;;     object will only be complete and make musical sense if many of the core
;;;     elements are present.
;;; - :title. A string that will be used as the title of the piece. The value
;;;   given for this object will be used as both the header for the printable
;;;   output as well as the base for any file names generated by the
;;;   write-lp-data-for-all method. Default = "slippery-chicken-piece".
;;; - :subtitle. A string that will be used as the subtitle of the piece. Works
;;;   in Lilypond scores only. 
;;; - :instrument-palette. An instrument-palette object. This will be the
;;;   palette of instrument objects available to the players of in the given
;;;   slippery-chicken object's ensemble slot. 
;;;   Default = +slippery-chicken-standard-instrument-palette+.
;;; - :ensemble. A recursive association list that will be used as the data to
;;;   create an ensemble object populated with player objects within the
;;;   slippery-chicken object. The format of this list will be a list of
;;;   user-defined player IDs each coupled with a list of instrument object IDs
;;;   from the current instrument-palette and various player object
;;;   parameters. See the user's manual and robodoc entries on the ensemble and
;;;   player classes for more detail.
;;; - :set-palette. A recursive association list that will be used as the data
;;;   to create a set-palette object within the slippery-chicken object. This
;;;   object is where the collections of possible pitches for any given
;;;   sequence are defined. The format of this list will be a list of IDs for
;;;   each set of pitches, each coupled with a list of note-name symbols for
;;;   the pitches that will be used to make that set. See the user's manual and
;;;   the robodoc entry on the set-palette class for more detail.
;;; - :set-map. A recursive association list that will be used as the data to
;;;   create a set-map object within the slippery-chicken object. This is where
;;;   the order in which the pitch collections defined in the set-palette will
;;;   be used in the piece. The format of this list will be a list of IDs from
;;;   the given slippery-chicken object's structure coupled with a list of IDs
;;;   from those given to the sets in the set-palette. There must be an equal
;;;   number of sections in this list as there are in the rthm-seq-map, and
;;;   they must have identical names. There must be an equal number of
;;;   individual set IDs in each list paired with the section IDs as there are
;;;   in the corresponding lists of the rthm-seq-map. See the user's manual and
;;;   the robodoc entries for the set-map and sc-map for more detail.
;;; - :rthm-seq-palette. A recursive association list that will be used as the
;;;   data to create a rthm-seq-palette object within the slippery-chicken
;;;   object. This object is where the collections of possible rhythm sequences
;;;   for any given sequence in the piece are defined. This list will take the
;;;   format of a list of IDs paired with a list of data for individual
;;;   rthm-seq objects. These in turn will consist of one or more lists of
;;;   rhythm data for rthm-seq-bar objects, as well pitch-seq-palettes and
;;;   marks data for the individual rthm-seq objects to be created. See the
;;;   user's manual as well as the robodoc entries for rthm-seq-palette,
;;;   rthm-seq, rthm-seq-bar, rhythm, and pitch-seq-palette for more detail.
;;; - :rthm-seq-map. A recursive association list that will be used as the data
;;;   to create a rthm-seq-map object within the slippery-chicken object. This
;;;   is where the order in which the rhythm sequences defined in the
;;;   rthm-seq-palette will be used in the piece. It will take the format of a
;;;   list of section IDs, of which there must be an equal number as are given
;;;   in the set-map, each coupled with a list of player IDs, as defined in the
;;;   ensemble slot of the given slippery-chicken object. The player IDs in
;;;   turn are coupled with a list of IDs for rthm-seq objects, as defined in
;;;   the rthm-seq-palette. Each of these lists must contain the same number of
;;;   elements as are contained in each of the set-map sections. See the user's
;;;   manual and robodoc entries for rthm-seq-map and sc-map for more details.
;;; - :snd-output-dir. A string that will be used as the directory path for any
;;;   output generated by clm-play in conjunction with sound files listed in
;;;   the sndfile-palette (see below). Default = (get-sc-config 'default-dir).
;;; - :sndfile-palette. A recursive association list that will be used as the
;;;   data to create a sndfile-palette object within the slippery-chicken
;;;   object. This is where the list is defined that contains all possible
;;;   source sound files which may be used in conjunction with output generated
;;;   by clm-play. This list will take the format of a list of IDs for
;;;   sound-file groups, coupled with lists of file names and various other
;;;   parameters associated with the sndfile class. The list of sound-file
;;;   groups is followed by a list of directory paths where the given sound
;;;   files are located and an optional list of file extensions. See the user's
;;;   manual and the robodoc entries on sndfile-palette, sndfile, and clm-play
;;;   for more detail.
;;; - :tempo-map. A recursive association list that will be used as the data to
;;;   create tempo objects within the slippery-chicken object. This is one of
;;;   two options for specifying the sequence of tempo changes for a given
;;;   piece (also see tempo-curve below). The format will be a list of integers
;;;   that are measure numbers within the piece, each coupled with tempo
;;;   indications in the form (beat-unit bpm). See the user's manual as well as
;;;   the robodoc entry for tempo-map for more detail. NB: This slot cannot be
;;;   used together with :tempo-curve.
;;; - :tempo-curve. A list of data that will be used to create tempo objects
;;;   within the slippery-chicken object, based on an interpolated list of
;;;   break-point pairs. This is one of two options for specifying the
;;;   sequence of tempo changes for a given piece (also see tempo-map above.)
;;;   The first item in the list will be the number of bars between each new
;;;   tempo object. The second item is the beat basis for the tempo objects
;;;   made. The third and final argument is the list of break-point pairs, of
;;;   which the first is a value on an arbitrary x-axis and the second is a
;;;   number of beats-per-minute. See the user's manual and the robodoc entry
;;;   for tempo-curve for more detail. NB: This slot cannot be used together
;;;   with :tempo-map.
;;; - :staff-groupings. A list of integers that indicate the placement of group
;;;   brackets for the printable output. Each number represents a consecutive
;;;   number of players, in the order they appear in the ensemble object, that
;;;   will be included in each consecutive group. The sum of the numbers in
;;;   this list must be equal to the number of players in the ensemble. See the
;;;   user's manual for more detail.
;;; - :instrument-change-map. A recursive association list that will be used as
;;;   the data to create an instrument-change-map object within the
;;;   slippery-chicken object. This will be used to indicate where those
;;;   players in the ensemble that play multiple instruments will change
;;;   instruments. The format will be a list of section IDs coupled with a list
;;;   of player IDs, each of which in turn is coupled with a list of 2-item
;;;   lists consisting of a sequence number paired with the ID (name) of one of
;;;   the instrument objects assigned to that player in the ensemble
;;;   object. See the user's manual and the robodoc entries for
;;;   instrument-change-map for more detail.
;; - :set-limits-high. A recursive association list that will be used to limit
;;;   the uppermost pitches of either the parts of individual players or of the
;;;   entire ensemble. The format will be a list of player IDs, as defined in
;;;   the ensemble object, each paired with a list of break-point pairs that
;;;   consist of a value on an arbitrary x-axis paired with a note-name pitch
;;;   symbol. These break-point envelopes are applied to the entire duration of
;;;   the piece. See the user's manual for more detail.
;;; - :set-limits-low. A recursive association list that will be used to limit
;;;   the lowermost pitches of either the parts of individual players or of the
;;;   entire ensemble. The format will be a list of player IDs, as defined in
;;;   the ensemble object, each paired with a list of break-point pairs that
;;;   consist of a value on an arbitrary x-axis paired with a note-name pitch
;;;   symbol. These break-point envelopes are applied to the entire duration of
;;;   the piece. See the user's manual for more detail.
;;; - :fast-leap-threshold. A number that is the longest duration of a note in
;;;   seconds that can be followed by a leap of a large interval, as defined in
;;;   the largest-fast-leaps slot of the instrument objects. Default = 0.125.
;;; - :instruments-hierarchy. A list of player IDs from the given
;;;   slippery-chicken object's ensemble that will specify the order in which
;;;   slippery chicken's pitch selection algorithm will choose pitches for the
;;;   instruments. By default (when NIL) this order follows the order in which
;;;   the instrument objects appear in the ensemble object. See the user's
;;;   manual for more detail. Default = NIL.
;;; - :rehearsal-letters. A list of numbers that are measure numbers at which
;;;   consecutive rehearsal letters will be placed. Since rehearsal letters are
;;;   technically actually place on the right-hand bar line of the previous
;;;   measure, measure 1 cannot be entered here. Slippery chicken automatically
;;;   proceeds consecutively through the alphabet, so only numbers are required
;;;   here. See the user's manual for more detail. If NIL, no rehearsal letters
;;;   will be added to the score. Default = NIL.
;;; - :avoid-melodic-octaves. T or NIL to indicate whether two linearly
;;;   consecutive pitches in the part of a given player may be of the same
;;;   pitch class but a different octave. T = avoid melodic octaves. 
;;;   Default = T.
;;; - :instruments-write-bar-nums. A list of player IDs above whose parts in
;;;   the score bar numbers should be written. If NIL, bar numbers will be
;;;   written above the top player in each group. NB: This slot affects CMN
;;;   output only. Default = NIL.
;;; - :pitch-seq-index-scaler-min. A decimal number that affects the likelihood
;;;   that slippery-chicken's pitch selection algorithm will choose pitches for
;;;   an instrument that have also already been assigned to other players. In
;;;   general terms, the higher this number is, the more likely it will be that
;;;   instruments may be assigned the same pitches, though this will of course
;;;   also be dependent on other factors, such as the characteristics of those
;;;   instruments and the pitches in the current set. See the user's manual on
;;;   pitches and the robodoc entries for pitch-seq for more detail. 
;;;   Default = 0.5.
;;; - :bars-per-system-map. A list of 2-item lists, each of which consists of a
;;;   measure number coupled with a number of measures to be placed in each
;;;   system starting at that measure number. NB: This list only affects CMN
;;;   output. See the user's manual on score layout for more details.
;;; - :composer. A string that will be used for the composer portion of the
;;;   header on the score's first page in LilyPond output. If NIL, no
;;;   composer's name will appear in the score. Default = NIL.
;;; - :year. A string that will be concatenated with the composer portion of the
;;;   header on the score's first page in LilyPond output. If NIL, no
;;;   year will appear in the score. Default = NIL.
;;; - :rthm-seq-map-replacements. A list of lists in the format 
;;;   '(((1 2 va) 3 2) ((2 3 vn) 4 3)) that indicate changes to individual
;;;   elements of lists within the given rthm-seq-map object. Each such list
;;;   indicates a change, the first element of the list being the reference
;;;   into the rthm-seq-map (the vla player of section 1, subsection 2 in the
;;;   first example here), the second element is the nth of the data list for
;;;   this key to change, and the third is the new data. If NIL, no changes
;;;   will be made. See the robodoc entries for rthm-seq-map for more
;;;   detail. Default = NIL.
;;; - :set-map-replacements. A list of lists in the format 
;;;   '((1 2 2) (3 3 1)) that indicate changes to individual elements of lists
;;;   within the given set-map object. Each such list indicates a change, the
;;;   first element of the list being the reference into the set-map (the
;;;   section, followed by a subsection if any exist), the second element being
;;;   the nth of the data list for to change, and the third being the new
;;;   data. If NIL, no changes will be made. See the robodoc entries for sc-map
;;;   for more detail. Default = NIL.
;;; - :key-sig. A two-element list indicating starting key signature for the
;;;   piece, e.g. '(ef minor). Usual note name symbols apply (e.g. ds = d
;;;   sharp, bf - b flat). Implies nothing beyond the signature, i.e. no
;;;   conformity to tonality expected. Default '(c major) i.e. no key
;;;   signature.
;;; - :transposition-curve. An envelope describing over the duration of the
;;;   piece (but using any arbitrary x-axis range) what transpositions in
;;;   semitones should be applied to the sets. Default = (0 0 100 0) 
;;; (- :warn-ties. This slot is now obsolete, but is left here for backwards
;;;    compatibility with pieces composed with earlier versions of
;;;    slippery-chicken. Default = T.)
;;; 
;;; RETURN VALUE
;;; T
;;; 
;;; EXAMPLE
#|
;;; An example using all slots
(let ((mini
       (make-slippery-chicken
        '+mini+
        :title "A Little Piece"
        :composer "Joe Green"
        :ensemble '(((fl ((flute piccolo) :midi-channel 1))
                     (cl (b-flat-clarinet :midi-channel 2))
                     (hn (french-horn :midi-channel 3))
                     (tp (b-flat-trumpet :midi-channel 4))
                     (vn (violin :midi-channel 5))
                     (va (viola :midi-channel 6))
                     (vc (cello :midi-channel 7))))
        :set-palette '((1 ((fs2 b2 d4 a4 d5 e5 a5 d6)))
                       (2 ((b2 fs2 d4 e4 a4 d5 e5 a5 d6)))
                       (3 ((cs3 fs3 e4 a4 e5 a5 e6))))
        :set-map '((1 (2 1 2 3 1 3 1))
                   (2 (1 1 3 2 2 3 1))
                   (3 (2 3 1 3 1 1 2)))
        :rthm-seq-palette '((1 ((((4 4) h (q) e (s) s))
                                :pitch-seq-palette ((1 2 3))))
                            (2 ((((4 4) (q) e (s) s h))
                                :pitch-seq-palette ((2 1 3))))
                            (3 ((((4 4) e (s) s h (q)))
                                :pitch-seq-palette ((3 2 1)))))
        :rthm-seq-map '((1 ((fl (2 3 3 1 1 1 2))
                            (cl (3 2 1 1 2 1 3))
                            (hn (1 2 3 1 1 3 2))
                            (tp (2 1 1 3 3 2 1))
                            (vn (3 1 3 2 1 1 2))
                            (va (2 1 1 1 3 2 3))
                            (vc (1 2 3 1 3 2 1))))
                        (2 ((fl (3 1 3 2 2 1 1))
                            (cl (1 1 2 3 1 3 2))
                            (hn (1 3 2 1 3 1 2))
                            (tp (1 1 1 3 3 2 2))
                            (vn (2 1 3 1 3 1 2))
                            (va (2 2 3 1 1 3 1))
                            (vc (1 3 1 2 2 1 3))))
                        (3 ((fl (1 1 3 2 1 3 2))
                            (cl (2 1 2 3 3 1 1))
                            (hn (3 2 1 1 1 3 2))
                            (tp (3 3 1 1 2 1 2))
                            (vn (3 1 3 2 1 1 2))
                            (va (3 2 1 1 3 2 1))
                            (vc (1 3 2 1 2 3 1)))))
        :snd-output-dir "/tmp"
        :sndfile-palette '(((sndfile-grp-1
                             ((test-sndfile-1.aiff :start 0.021 :end 0.283)
                              (test-sndfile-2.aiff)
                              (test-sndfile-3.aiff)))
                            (sndfile-grp-2
                             ((test-sndfile-4.aiff :frequency 834)
                              (test-sndfile-5.aiff)
                              (test-sndfile-6.aiff))))
                           ("/path/to/test-sndfiles-dir-1"
                            "/path/to/test-sndfiles-dir-2"))
        ;; :tempo-map '((1 (q 84)) (9 (q 72))) ; ; ;
        :tempo-curve '(5 q (0 40 25 60 50 80 75 100 100 120))
        :staff-groupings '(2 2 3)
        :instrument-change-map '((1 ((fl ((1 flute) (3 piccolo) (5 flute))))))
        :set-limits-low '((fl (0 c5 50 g5 100 c5))
                          (cl (0 c4 50 f4 100 c4))
                          (hn (0 f3 50 c4 100 f3))
                          (tp (0 c4 50 f4 100 c4))
                          (vn (0 e5 50 a5 100 e5))
                          (va (0 c3 50 f3 100 c3))
                          (vc (0 c2 50 f3 100 c2)))
        :set-limits-high '((fl (0 d6 50 a6 100 d6))
                           (cl (0 c5 50 a5 100 c5))
                           (hn (0 f4 50 c5 100 f4))
                           (tp (0 f5 50 c5 100 f5))
                           (vn (0 c6 50 e6 100 c6))
                           (va (0 g4 50 d5 100 g4))
                           (vc (0 c4 50 f4 100 c4)))
        :fast-leap-threshold 0.5
        :instruments-hierarchy '(fl vn cl tp va hn vc)
        :rehearsal-letters '(3 11 19)
        :avoid-melodic-octaves nil
        :instruments-write-bar-nums '(fl cl hn tp)
        :pitch-seq-index-scaler-min 0.1
        :bars-per-system-map '((1 1) (2 2) (3 3) (7 4) (11 5))
        :rthm-seq-map-replacements '(((1 va) 3 1) ((2 fl) 4 3))
        :set-map-replacements '((1 2 2) (3 3 1)))))
  (midi-play mini :midi-file "/tmp/mini.mid")
  (cmn-display mini)
  (write-lp-data-for-all mini))

|#
;;; SYNOPSIS
(defun make-slippery-chicken (name &key 
                                     rthm-seq-palette 
                                     rthm-seq-map
                                     set-palette 
                                     set-map 
                                     sndfile-palette 
                                     tempo-map 
                                     tempo-curve 
                                     (snd-output-dir (get-sc-config
                                                      'default-dir))
                                     instrument-change-map 
                                     instruments-write-bar-nums
                                     bars-per-system-map
                                     staff-groupings
                                     rthm-seq-map-replacements
                                     set-map-replacements
                                     set-limits-low 
                                     set-limits-high
                                     instrument-palette 
                                     ensemble 
                                     rehearsal-letters 
                                     (fast-leap-threshold 0.125)
                                     instruments-hierarchy 
                                     (title "slippery chicken piece") 
                                     subtitle
                                     composer year
                                     (avoid-melodic-octaves t)
                                     (avoid-used-notes t)
                                     (pitch-seq-index-scaler-min 0.5) 
                                     defer
                                     ;; MDE Mon Jul  2 16:08:42 2012
                                     (key-sig '(c major))
                                     ;; MDE Thu Sep  6 14:34:14 2018
                                     (transposition-curve '(0 0 100 0))
                                     (warn-ties t))
;;; ****                                
  (make-instance 'slippery-chicken 
                 :id name
                 :title title
                 :subtitle subtitle
                 :composer composer :year year
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
                 :snd-output-dir (trailing-slash snd-output-dir)
                 :sndfile-palette sndfile-palette
                 :instrument-palette instrument-palette
                 :tempo-map tempo-map
                 :tempo-curve tempo-curve
                 :defer defer
                 :bars-per-system-map bars-per-system-map
                 :ensemble ensemble
                 :instruments-hierarchy instruments-hierarchy
                 :set-limits-low set-limits-low 
                 :set-limits-high set-limits-high
                 :fast-leap-threshold fast-leap-threshold
                 :pitch-seq-index-scaler-min pitch-seq-index-scaler-min
                 :avoid-melodic-octaves avoid-melodic-octaves
                 :avoid-used-notes avoid-used-notes
                 :key-sig key-sig
                 :transposition-curve transposition-curve
                 :warn-ties warn-ties))

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
              in the maps: ~%set-map ~a rthm-seq-map ~a"
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
                               ~a the number of references ~%in the maps ~
                               is not equal.~
                               ~% In set-map: ~a, in rthm-seq-map: ~a" 
                              (this sm-sec) len-sm-sec len-rsm-sec))))))))
              
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
               ;; MDE Sun May  6 18:40:07 2012
               (when (and last-event (pitch-or-chord last-event))
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
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Adds postscript code to specific pages in a file.
;;; e.g. (add-ps-to-file "file.eps" '((1 "blah") (4 "foo") (3 "bar"))) 
;;; where the strings will be added to pages 1, 4 and 3, just after the
;;; "%%Page: x y" line. Of course, the strings should be legal postscript
;;; code, not just "blah blah". The pairs are sorted into ascending page order
;;; first so that the file only has to be searched once when looking for the
;;; pages. 

(defun add-ps-to-file (file page-code-pairs)
  (let ((tmp-file (format nil "~aadd-ps-to-file.tmp" 
                          (agnostic-directory-pathname file))))
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
;;; member grace-note-duration. We subtract this amount from the
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
;;; voices. voices is one list of rthm-seqs (a list of events) for each voice
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

(defun count-sequence-refs (thing)
  (let ((count 0))
    (loop 
       for sec in (data thing) 
       for data = (data sec)
       do
       (incf count
             (if (is-ral data)
                 (count-sequence-refs data)
                 (length data))))
    count))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Tue Feb 20 12:15:31 2018 -- if x-max is nil, don't stretch, just process
;;; pitch/y values
(defun doctor-env (env &optional x-max (process-ys t))
  ;; MDE Mon Apr  9 13:11:25 2012 
  (when (and x-max (<= x-max 1))
    (error "slippery-chicken::doctor-env: Can't doctor envelopes in a piece ~
            ~%with only one sequence."))
  (let ((stretched (if x-max (new-lastx env x-max) env)))
    ;; 14/8/07 first x always needs to be 1
    (setf (first stretched) 1)
    (loop for x in stretched by #'cddr and y in (cdr stretched) by #'cddr
       collect x
       ;; convert notes or MIDI note numbers to degrees so that we can
       ;; interpolate. Note degrees are in cm::*scale* so this is not the same
       ;; as MIDI notes.
       collect
         (if process-ys
             (if (numberp y)
                 (midi-to-degree (floor y))
                 (note-to-degree y))
             y))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-set-limit-aux (limits-al instrument x)
  ;; (print x)
  (let ((ins-curve (get-data instrument limits-al nil)))
    (when ins-curve
      (let ((degree (interpolate x (data ins-curve))))
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
;;; instrument and then notes for this seq. With the notes and the rthm-seq,
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
  ;; (print instrument-change)
  ;; (print-simple rthm-seq)
  ;; (print (bars rthm-seq))
  ;; (print pitch-seq)
  (object-is-nil? rthm-seq "slippery-chicken::sc-make-sequenz" 'rthm-seq)
  ;; (object-is-nil? pitch-seq "slippery-chicken::sc-make-sequenz" 'pitch-seq)
  ;; (print instrument-change)
  ;; MDE Tue May 24 14:29:01 2016 -- just make sure we've not got any dodgy
  ;; empty bars
  (loop for bar in (bars rthm-seq) do
       (when (and (is-rest-bar bar) (not (rhythms bar)))
         (force-rest-bar bar)))
  (let* ((sequenz (clone-with-new-class rthm-seq 'sequenz))
         (player-obj (when player
                       (get-player slippery-chicken player)))
         (midi-channel (if player
                           (midi-channel player-obj)
                           1))
         ;; MDE Wed Jun 14 18:30:24 2017 -- use midi-channel if microtones
         ;; channel not given.
         (microtones-midi-channel
          (if (and player-obj (> (microtones-midi-channel player-obj) 0))
              (microtones-midi-channel player-obj)
              midi-channel))
         ;; MDE Thu Jul  5 17:00:45 2012 
         (got-ins (instrument-p instrument))
         (midi-prog (when got-ins (midi-program instrument)))
         (get-set-limits (and slippery-chicken player global-seq-num))
         ;; if any extra range limiting has been given for this instrument
         ;; get it here and pass it to get-notes.
         (set-limits (when get-set-limits
                       (get-set-limits slippery-chicken player 
                                       global-seq-num)))
         ;; these would have to be chords or notes once we get the
         ;; pitch-choosing algorithm going, also the midi-channel setting loop
         ;; below.
         ;; MDE Thu Sep  6 14:03:27 2018
         (set-transp (if slippery-chicken
                         (interpolate global-seq-num
                                      (transposition-curve slippery-chicken))
                         0.0))
         (notes-from-pitch-seq
          (when pitch-seq
            (get-notes pitch-seq instrument player-obj
                       ;; MDE Thu Sep  6 14:11:50 2018
                       (if (zerop set-transp)
                           set
                           (transpose set set-transp :destructively nil))
                       hint-pitch (second set-limits) (first set-limits)
                       global-seq-num last-pitch 
                       ;; MDE Mon Mar 26 13:21:29 2012
                       (if slippery-chicken
                           (pitch-seq-index-scaler-min slippery-chicken)
                           0.5)
                       (if slippery-chicken
                           (avoid-melodic-octaves slippery-chicken)
                           t)
                       (if slippery-chicken
                           (avoid-used-notes slippery-chicken)
                           t))))
         (notes (my-copy-list notes-from-pitch-seq))
         #| MDE Wed Apr 18 10:24:10 2012 -- (iwbns (when slippery-chicken 
         (member player 
         (instruments-write-bar-nums slippery-chicken))))
         |#
         (do-prog-changes instrument-change)
         (current-note nil)
         ;; (last-note nil)
         ;; transposition specifies how many semitones the instrument sound
         ;; above/below written pitch so we have to invert the sign of this
         ;; to get the transposition we need to write sounding pitches.
         (transpose (when got-ins
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
    ;; only one tied (to) note so none are needed. In that case we
    ;; have to have the last note from the previous sequence.
    (setf current-note (if notes 
                           (clone (first notes))
                           (progn
                             (unless (or (zerop (num-score-notes rthm-seq))
                                         ;; MDE Wed Oct 10 16:48:54 2018 -- this
                                         ;; is where we allow auto-creation of
                                         ;; rest sequences if no notes available
                                         ;; for an instrument, rather than
                                         ;; signalling an error, as previously
                                         (not (get-sc-config
                                               'pitch-seq-no-pitches-error)))
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
                                 (warn "Setting to 'b4!")
                                 (setf last-note-previous-seq
                                       (make-event 'b4 'q)))
                               (clone (pitch-or-chord 
                                       last-note-previous-seq))))))
    #|
    ;; this checks that there are no ties to the first note in a seq ;
    (when (is-tied-to (get-nth-event 0 (get-bar sequenz 0 t)))
    (error "slippery-chicken::sc-make-sequenz: ~
             Tied first note of sequenz not allowed!"))
    |#
    (loop for bar in (bars sequenz) and bar-num from 1 do
       ;; first of all set all the bars to write--then change in 
       ;; sequenz::update-slots depending upon real bar num
       ;; MDE Wed Apr 18 10:22:57 2012 -- no longer do this here but in sc
       ;;(when iwbns
       ;; (setf (write-bar-num bar) t))
         (setf (player-section-ref bar) player-section-ref
               (nth-seq bar) seq-num
               (nth-bar bar) (1- bar-num))
         ;; MDE Wed Oct 10 17:12:14 2018 
         (unless notes-from-pitch-seq
           (force-rest-bar bar))
       ;; Here the rhythms in the rthm-seq-bar are upgraded to events
         (loop for rhythm in (rhythms bar) and rthm-num from 0 do
              (let ((event (clone-with-new-class rhythm 'event)))
                ;; MDE Thu Aug 22 19:30:33 2013 
                (setf (player event) player
                      (set-ref event) (when (sc-set-p set) (this set)))
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
                    (when got-ins ;; MDE Thu Jul  5 17:02:03 2012
                      ;; (print (staff-short-name instrument))
                      (setf (instrument-change event)
                            (get-instrument-change-list instrument))))
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
                               (clone current-note)))))
                (when transpose
                  (set-written event transpose))
                ;; MDE Thu Apr 19 12:34:52 2012 -- statistics
                ;; MDE Thu Jul  5 17:02:33 2012 -- only when got-ins
                (when (and got-ins (needs-new-note event))
                  ;; so this handles chords
                  (incf (total-degrees instrument) (get-degree event :sum t)))
                ;; (when (is-single-pitch event)
                ;;       (print (midi-channel (pitch-or-chord event))))
                (setf (nth rthm-num (rhythms bar)) event)))
       ;; MDE Thu Apr 19 10:21:07 2012 -- statistics
       ;; MDE Thu Apr 19 14:16:07 2012 -- shoudn't need this now
       ;; (gen-stats bar) 
         (unless (is-rest-bar bar)
           (when got-ins                ; MDE Thu Jul  5 17:02:52 2012 
             (incf (total-bars instrument))
             ;; we can't do total-duration here as we don't have the events'
             ;; duration-in-tempo until later...
             (incf (total-notes instrument) (notes-needed bar)))))
    ;; all the notes should have been popped off by now
    (when notes
      (error "~a ~a ~%slippery-chicken::sc-make-sequenz: Didn't use all ~
              the notes!  Still have ~a left."
             rthm-seq pitch-seq (length notes)))
    ;; MDE Fri May 17 21:45:26 2013
    (dynamics-to-amplitudes sequenz)
    ;; MDE Thu Jul 11 19:17:55 2019
    (pedals-to-controllers sequenz)
    ;; the marks given in the rthm-seq were not interpreted in that class,
    ;; first here when all the rhythms have been converted to events.
    ;; 19/2/07: move this method over to the rthm-seq class and call it there
    ;; (add-marks sequenz)
    ;; (print "exiting sc-make-sequenz")
    sequenz))
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Mon Dec 21 18:39:09 2020, Heidhausen
(defun too-high-for-clef (pitch clef)
  (pitch> (make-pitch pitch)
          (make-pitch (case clef
                        ;; too high is more than 6 ledger lines
                        (bass 'g5)
                        (treble 'e7)
                        (percussion 'e7)
                        (alto 'f6)
                        (tenor 'd6)
                        (double-bass 'e4)
                        (double-treble 'e8)))))

(defun too-low-for-clef (pitch clef)
  (pitch< (make-pitch pitch)
          (make-pitch (case clef
                        ;; too low is different somehow: more than 3/4/5 ledger
                        ;; lines (by feel)
                        (bass 'c0)
                        (treble 'c3)
                        (percussion 'c3)
                        (alto 'e2)
                        (tenor 'd2)
                        (double-bass 'c-1)
                        (double-treble 'g4)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; If we get three notes whose best clef without ledger lines is not the
;;; current, change it. If any note has only a best clef that is different to
;;; current, change it

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
                    ;; we need all three to do the change!
                    (or (not change)
                        (equal c1 change)))
           (incf change-count)
           (setf change c1))
         (if (= 2 i) 
             (setf i 0)
             (incf i)))
    (if (= 3 change-count) ; (= 3 change-count)
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
                   psym (when event (get-pitch-symbol event)))
             (when (and c1 psym
                        (or (and (not (equal new-current-clef c1))
                                 (not (equal new-current-clef c2))
                                 ;; could be we just set a clef and now c1,c2
                                 ;; are no longer current so make sure we're not
                                 ;; in the current clef's range before changing
                                 ;; it 
                                 (not (funcall
                                       (get-sc-config 'best-clef-aux-fun)
                                       nil
                                       (if (written-pitch-or-chord event)
                                           (written-pitch-or-chord event)
                                           (pitch-or-chord event))
                                       nil current-clef verbose)))
                            ;; MDE Mon Dec 21 18:44:57 2020, Heidhausen
                            ;; todo: here
                            (too-high-for-clef (highest event) current-clef)
                            (too-low-for-clef (lowest event) current-clef)
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
;;; whereupon we simply return that. Remember that the tempo curve's first
;;; element is the frequency in bars, second element is the beat rhythm, and
;;; the third is the curve itself.
;;; NB the curve should start at 0 but the map will start at bar 1
(defun tempo-curve-to-map (tempo-map tempo-curve num-bars)
  ;; MDE Thu Mar  8 15:25:33 2012 
  (unless (or tempo-map tempo-curve)
    (warn "slippery-chicken::tempo-curve-to-map: No tempo-map or tempo-curve ~
            given. ~%Using default of crotchet/quarter = 60.")
    (setf tempo-map '((1 (q 60)))))
  (when (and tempo-map tempo-curve)
    (error "slippery-chicken::tempo-curve-to-map: ~
            can't have a tempo-map and a tempo-curve; ~%only one ~
            or the other"))
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
;;; MDE Thu Mar  8 15:42:33 2012 
(defun slippery-chicken-p (thing)
  (typep thing 'slippery-chicken))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* slippery-chicken/lp-display
;;; DESCRIPTION
;;; This function has exactly the same arguments as write-lp-data-for all and
;;; only differs from that method in that after writing all the Lilypond text
;;; files for the score, it calls Lilypond to render the PDF, which is then
;;; opened automatically from within Lisp (if the value of 
;;; (get-sc-config 'lp-display-auto-open) is T).
;;; 
;;; In order to work properly, you'll need to make sure the value of the
;;; 'lilypond-command configuration parameter is set via (set-sc-config ...) to
;;; the full path of your Lilypond command (not the app: it's usually
;;; /path/to/Lilypond.app/Contents/Resources/bin/lilypond on OSX).
;;; 
;;; NB Only available with CCL and SBCL on OSX. With SBCL on OSX the output
;;; from Lilypond is only printed once the process has exited, so it may take a
;;; while until you see anything.
;;; 
;;; ARGUMENTS
;;; See write-lp-data-for-all
;;; 
;;; RETURN VALUE
;;; An integer: the shell's exit code for the PDF open command, usually 0 for
;;; success. Second returned value is the patch to the PDF file.
;;; 
;;; SYNOPSIS
(defun lp-display (&rest args)
;;; ****
  (let* ((lp-file (apply #'write-lp-data-for-all args))
         (no-ext (path-minus-extension lp-file))
         (pdf-file (concatenate 'string no-ext ".pdf"))
         (success (shell (get-sc-config 'lilypond-command) "-o" no-ext
                         lp-file)))
    (print lp-file)
    (if (zerop success)
        (values
         ;; DJR Fri 30 Aug 2019 10:09:13 BST
         ;; Changed the (when) to an (if) so that if 'lp-display-auto-open is
         ;; set to nil the RTs will still pass.
         (if (get-sc-config 'lp-display-auto-open)
             (system-open-file pdf-file)
             t)
         pdf-file)
        (error "slippery-chicken::lp-display: Call to Lilypond failed."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; used in decide-boundaries (called from find-boundaries method).
;;; counts is a list of ascending bar number - note-count pairs
(defun decide-boundaries-aux (contenders counts &optional print)
  ;; get the bar before the first contender also, for context
  ;; (loop for c in (cons (1- (first contenders)) contenders)
  (let* ((counts (loop for c in contenders
                    for count = (nth (1- (* 2 c)) counts)
                       collect count))
         ;; this used to call list-mean instead but this new function is safer
         ;; on long lists
         (mean (average counts))
         ;; we're going to decide on the bar number with the number of notes
         ;; which is furthest away from all other contenders
         (deltas (loop for c in counts collect (abs (- c mean))))
         (result 0))
    (when print
      (format t "~%contenders: ~a ~%counts: ~a deltas: ~a mean: ~a" 
              contenders counts deltas mean))
    (loop with highest = 0 for d in deltas for i from 0 do
         (when (> d highest)
           (setf highest d
                 result i)))
    (nth result contenders)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; used in find-boundaries method.
(defun decide-boundaries (contenders counts)
  (loop for cs in contenders 
     if (listp cs) collect (decide-boundaries-aux cs counts)
     else collect cs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun handle-hairpins-aux2 (hp)
  (let* ((amp1 (first hp))
         (amp2 (second hp))
         (events (third hp))
         (num (length events))
         (env (list 0 amp1 (1- num) amp2)))
    ;; (print env)
    (when (> num 1)
      (loop for e in events for i from 0 do
           (setf (slot-value e 'amplitude) (interpolate i env))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* slippery-chicken-edit/map-over-events-aux
;;; AUTHOR
;;; Daniel Ross (mr.danielross[at]gmail[dot]com) 
;;; 
;;; DATE 
;;; 3 September 2019, London
;;; 
;;; DESCRIPTION
;;; Auxilliary function for map-over-notes and map-over-events. Saves
;;; duplicating code.
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
;;; - T or NIL to indicate whether or not rests will be ignored.
;;;   T = attacked notes only (i.e. no rests), NIL = include rests.
;;; - The method or function itself. This can be a user-defined function or the
;;;   name of an existing method or function.  It should take at least one
;;;   argument, an event, and any other arguments as supplied.  
;;; - Any additional argument values the specified method/function may
;;;   take or require. See the thin method below for an example that uses
;;;   additional arguments.
;;; 
;;; RETURN VALUE
;;; - A list containing the number of events changed per instrument
;;; 
;;; SYNOPSIS
(defun map-over-events-aux (sc start-bar end-bar players attacked-notes-only
                            function further-args)
;;; ****
  (unless end-bar
    (setq end-bar (num-bars sc)))
  (unless start-bar
    (setq start-bar 1))
  (unless players
    (setq players (players sc)))
  ;; DJR Wed 18 Sep 2019 08:15:15 BST
  ;; Forgot to setq this!
  (setq players (force-list players))
  (let ((count-list '()))
    (loop for player in players do
         (next-event sc player attacked-notes-only start-bar)
         (loop for ne = (next-event sc player attacked-notes-only nil end-bar)
            with count = 0
            while ne
            do
              (apply function (cons ne further-args))
              (incf count)
            finally (push count count-list)))
    (nreverse count-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DJR Tue 29 Oct 2019 08:58:30 GMT -- aux function for pitch-or-chord=
(defun pitch-or-chord=-aux (p1 p2 enharmonics-are-equal frequency-tolerance)
  (cond ((null (pitch-or-chord-p p1))
         (warn "~%pitch-or-chord=: p1 is not a pitch or chord: ~a" p1)
         nil)
        ((null (pitch-or-chord-p p2))
         (warn "~%pitch-or-chord=: p2 is not a pitch or chord: ~a" p2)
         nil)
        ((and (pitch-p p1)
              (chord-p p2))
         (single-pitch-chord= p1 p2 enharmonics-are-equal frequency-tolerance))
        ((and (chord-p p1)
              (pitch-p p2))
         (single-pitch-chord= p1 p2 enharmonics-are-equal frequency-tolerance))
        ((and (pitch-p p1)
              (pitch-p p2))
         (pitch= p1 p2 enharmonics-are-equal frequency-tolerance))
        ((and (chord-p p1)
              (chord-p p2))
         (chord= p1 p2 enharmonics-are-equal frequency-tolerance))
        (t nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Fri Jan  6 17:58:37 2023, Heidhausen -- dc is the argument to digit-char
;;; where 10 = "A" 
(defun get-rehearsal-letter (dc)
  (unless (> dc 9) (error "get-rehearsal-letter:: argument should be > 9"))
  (let* ((letter-num (+ 10 (mod (- dc 10) 26)))
         (letter (digit-char letter-num 36))
         (num (1+ (floor dc 36)))
         (result ""))
    ;; after Z we have AA, BB, CC ...
    (loop repeat num do (setq result (format nil "~a~a" result letter)))
    result))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken/write-csound-score
;;; AUTHOR
;;; Ruben Philipp <ruben.philipp@folkwang-uni.de>
;;;
;;; DATE
;;; 2023-02-28, Essen
;;;
;;; DESCRIPTION
;;; Generate a Csound-score file (.sco) from the data of the specified
;;; slippery-chicken object. 
;;;
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; - The ID(s) of the player(s) whose events are used to obtain the
;;;   rhythmic structure of the score events (i.e. the onset and duration
;;;   values) and, dependent on the p-fields function, pitch content,
;;;   amplitude etc. This must be a list with one or more symbols.
;;; - The ID(s) (number) or name(s) (string) of the Csound instruments
;;;   according to the resp. instrument definition in the Csound orchestra.
;;;   Each player of the second argument of this methods needs to be assigned
;;;   one Csound instrument. This must be a list with one or more values.
;;;
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :suffix. A string to be added just before the .sco of the generated
;;;   csound score-file. Will be overridden when manually setting the path
;;;   via :csound-file. Default = "".
;;; - :csound-file. A string indicating the path and filename to the .sco-file
;;;   to be generated with this method. Default is a filename extracted from the
;;;   title of the sc piece, placed in the (get-sc-config 'default-dir)
;;;   directory (per default /tmp).
;;; - :start-section. An integer that is the number of the first section for
;;;   which the Csound-score is to be generated. Default = 1.
;;; - :num-sections. An integer that is the number of sections to generate the
;;;   score data. If NIL, all sections will be written. Default = NIL.
;;; - :from-sequence.  An integer that is the number of the first sequence
;;;   within the specified starting section to be used to generate the output
;;;   file. Default = 1.
;;; - :num-sequences. An integer that s the number of sequences for which
;;;   event data is to be generated in the resulting .sco-file, including the
;;;   sequence spiecified in from-sequence. If NIL, all sequences will be
;;;   written. NB: This argument can only be used when the num-sections = 1.
;;;   Default = NIL.
;;; - :chords. Either a boolean or an integer which determines how to deal with
;;;   chords in events. When T, all notes of a chord are parsed as
;;;   i-statements in the Csound-score. When NIL, chords will be ignored and
;;;   treated like rests (i.e. nothing will be generated). When an integer is
;;;   given, the number determines the max. amount of pitches being parsed as i-
;;;   statements. They are chosen chronologically from the given chord.
;;;   Default = T.
;;; - :offset. An integer or floating point value to indicate the global
;;;   offset of all score events (in seconds). Default = 0.
;;; - :delimiter. A character which separates each score statement. Csound
;;;   recommends using spaces, though #\tab does also work an might, in some
;;;   cases, improve legibility. Default = #\space.
;;; - :p-fields. This should be either NIL, a list of lists or a function used
;;;   to generate the p-fields starting from p4. The lists can be of arbitrary
;;;   length and are solely limited by Csound's internal parsing restrictions.
;;;   When NIL, no additional p-statements are added to any of the score-
;;;   statements, regardless of the amount of included players (i.e. no further
;;;   p-field is added to any of the players).
;;;   When given a list of lists, each sublist contains a set of constant
;;;   p-values for each player. NB: It is mandatory to pass a sub-list for
;;;   each player chosen for score generation (cf. second argument). It is
;;;   possible to pass an empty list (i.e. NIL) for a single player, if it is
;;;   desired not to add any further p-fields other than instrument-name (p1),
;;;   onset (p2), and duration (p3).
;;;   When a function is given as argument, it will be used to generate the
;;;   p-values based on the event-data and other arguments. This enables dynamic
;;;   generation of score content dependent on the data contained in the
;;;   specific event used for generating the i-statement (e.g. pitch or
;;;   amplitude). The pfield-function needs to accept the following arguments:
;;;   - event.         A sc event.
;;;   - event-num.     An integer counting the event index of each player.
;;;   - cs-instrument. The csound-instrument name as defined in the third
;;;                    argument (see above). This comes in handy when the
;;;                    csound instruments require different sets of p-values.
;;;   NB: When chords should be able to be processed (see above), the function
;;;       needs to be able to process events containing chords and return a list
;;;       of the length of the chord (e.g. via is-chord).
;;;       For further detail, take a look at csound-p-fields-simple.
;;; - :comments. A boolean indicating whether additional comments should be
;;;   included into the generated score. The comments include the title,
;;;   composer and generation date of the sc piece resp. the csound-score.
;;;   Additionally, the score sections containing the instrument calls for
;;;   each player will be preceded with a reference to the player ID
;;;   according to the sc ensemble. Default = T.
;;;   
;;;
;;; RETURN VALUE
;;; Returns the path of the file written, as a string.
;;;
;;; EXAMPLE
#|
;;; An example using the csound-p-fields-simple function
(let ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((pno (piano :midi-channel 1))
                      (vln (violin :midi-channel 2))))
         :set-palette '((1 ((f3 g3 as3 a3 bf3 b3 c4 d4 e4 f4 g4 a4 bf4 cs5))))
         :set-map '((1 (1 1 1 1 1 1 1))
                    (2 (1 1 1 1 1 1 1))
                    (3 (1 1 1 1 1 1 1)))
         :tempo-map '((1 (q 60)))
         :rthm-seq-palette '((1 ((((4 4) h (q) e (s) s))
                                 :pitch-seq-palette ((1 (2) 3))))
                             (2 ((((4 4) (q) e (s) s h))
                                 :pitch-seq-palette ((1 2 3))))
                             (3 ((((4 4) e (s) s h (q)))
                                 :pitch-seq-palette ((2 3 3))))
                             (4 ((((4 4) (s) s h (q) e))
                                 :pitch-seq-palette ((3 1 (2))))))
         :rthm-seq-map '((1 ((pno (1 2 1 2 1 2 1))
                             (vln (1 2 1 2 1 2 1))))
                         (2 ((pno (3 4 3 4 3 4 3))
                             (vln (3 4 3 4 3 4 3))))
                         (3 ((pno (1 2 1 2 1 2 1))
                             (vln (1 2 1 2 1 2 1))))))))
  (write-csound-score mini
               '(pno vln)
               '(1 "fmsynth")
               :chords 1
               :delimiter #\tab
               :comments t))

;;; An example that creates different sets of p-field for each
;;; individual player.
;;; - Player 'vln calls the (hypothetical) Csound instrument 1
;;;   which generates tone of frequency p4 and amplitude p5.
;;;   Here, the standard csound-p-fields-simple function is invoked.
;;; - Player 'vlc calls the Csound instrument 2, which is a simple
;;;   sampler capable of playing one out of ten pre-loaded soundfiles.
;;;   These are identified by an index (1 <= i <= 10) which is expected
;;;   to be p4. p5 determines the amplitude of the playback.
;;;   In this examples, the samples will be selected cyclically, while
;;;   each event of the player is allocated one of the ten samples.
;;;   As there is one soundfile per events, chords will be treated as
;;;   pitches, i.e. every chord results in a list instead of a list of
;;;   lists of p-fields.
(defun csound-p-fields-custom (event event-num cs-instrument)
  ;; generate different p-field sets depending on the
  ;; given Csound instrument-number
  (format t "~%ins: ~a; enum: ~a~%"
          cs-instrument
          event-num)
  (case cs-instrument
    (1 (csound-p-fields-simple event
                               event-num
                               cs-instrument))
    (2 (let* ((amplitude (get-amplitude event))
              ;; num of available samples in Csound
              ;; instrument definition / orchestra
              (num-samples 10)
              ;; get the id of the sample to be played
              ;; for this event
              (current-sample (1+ (mod (1- event-num)
                                       num-samples))))
         (list current-sample
               amplitude)))))

(let* ((mini
         (make-slippery-chicken
          '+mini+
          :ensemble '(((vln (violin :midi-channel 1))
                     (vlc (cello :midi-channel 2))))
          :tempo-map '((1 (q 60)))
          :set-palette '((1 ((f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5)))
                         (2 ((f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5)))
                         (3 ((f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5))))
          :set-map '((1 (1 1 1 1 1))
                     (2 (2 2 2 2 2))
                     (3 (3 3 3 3 3)))
          :rthm-seq-palette '((1 ((((4 4) h q e s s))
                                  :pitch-seq-palette ((1 2 3 4 5))))
                              (2 ((((4 4) q e s s h))
                                  :pitch-seq-palette ((1 2 3 4 5))))
                              (3 ((((4 4) e s s h q))
                                  :pitch-seq-palette ((1 2 3 4 5)))))
          :rthm-seq-map '((1 ((vln (1 3 2 1 2))
                              (vlc (3 1 1 2 2))))
                          (2 ((vln (3 1 1 2 2))
                              (vlc (1 3 1 2 2))))
                          (3 ((vln (1 1 3 2 2))
                              (vlc (2 1 1 2 3))))))))
  (write-csound-score mini
               '(vln vlc)
               '(1 2)
               :chords nil
               :delimiter #\tab
               :p-fields #'csound-p-fields-custom
               :comments t))
|#
;;; SYNOPSIS
(defmethod write-csound-score ((sc slippery-chicken)
                               players
                               csound-instruments
                               &key
                                 (start-section 1)
                               ;; offset in seconds
                                 (offset 0)
                               ;; add something just before .sco?
                                 (suffix "")
                                 (csound-file
                                  (format nil "~a~a~a.sco"
                                          (get-sc-config 'default-dir)
                                          (filename-from-title (title sc))
                                          suffix))
                               ;; add a comment sections?
                                 (comments t)
                                 (delimiter #\space)
                                 (from-sequence 1)
                                 (num-sequences nil)
                                 (chords t)
                               ;; either NIL, a list of lists, or a function
                               ;; cf. documentation
                                 (p-fields #'csound-p-fields-simple)
                               ;; when NIL, all sections are considered
                                 (num-sections nil))
;;; ****
  ;; test if a csound instrument is assigned to every selected player
  (when (/= (length players)
            (length csound-instruments))
    (error "slippery-chicken::write-csound-score: ~
            players/voices and csound-instruments differ in length"))
  ;; if the p-fields keywords is not a function, test if it is either
  ;; NIL, or
  ;; a list of lists containing p-field-values for each player/instrument
  (unless (or (functionp p-fields)
              (eq nil p-fields)
              (and (every #'listp p-fields)
                   (= (length p-fields)
                      (length players))))
    (error "slippery-chicken::write-csound-score: ~
            the p-fields keyword argument must be either NIL, ~% ~
            a function, or a list of lists with p-field values ~
            for each player"))
  ;; borrowed from midi-play
  ;; RP  Sun Feb 19 23:55:03 2023
  (when (and (not num-sections)
             (= start-section 1)
             (= 1 (get-num-sections sc)))
    (setf num-sections 1))
  ;; borrowed from midi-play
  ;; RP  Sun Feb 19 23:55:54 2023
  (unless (integer>0 from-sequence)
    (error "slippery-chicken::write-csound-score: ~
            from-sequence must be an integer >= 1."))
  ;; borrowed from midi-play
  ;; RP  Sun Feb 19 23:57:57 2023
  (when (and num-sequences 
             (or (not num-sections)
                 (and num-sections (> num-sections 1))))
    (error "slippery-chicken::write-csound-score: ~
            num-sequences keyword should only ~
            be used ~%when num-sections = 1."))
  ;; borrowed from midi-play
  ;; RP  Sun Feb 19 23:58:48 2023
  (when (and from-sequence (/= 1 from-sequence)
             (or (not num-sections)
                 (and num-sections (> num-sections 1))))
    (error "slippery-chicken::write-csound-score: ~
            from-sequence keyword should only ~
            be used ~%when num-sections = 1."))
  ;; borrowed from midi-play
  ;; RP  Sun Feb 19 23:59:05 2023
  (when (and num-sections (= 1 num-sections) (not num-sequences))
    (let ((ns (num-seqs sc start-section)))
      (unless ns 
        (error "slippery-chicken::write-csound-score: ~
                can't get number of sequences ~
                for section ~a." start-section))
      (setf num-sequences (- ns (1- from-sequence)))))
  ;; start parsing the sc-data
  (let* ((events (get-events-start-time-duration sc
                                                 start-section
                                                 players
                                                 :num-sections num-sections
                                                 :num-sequences num-sequences
                                                 :from-sequence from-sequence
                                                 :ignore-rests nil
                                                 :include-rests t
                                                 :time-scaler 1.0))
         (score-events
           (loop
             for player in events
             for player-i from 0
             for instrument = (nth player-i csound-instruments)
             collect
             ;; create Csound instrument calls for each player
             ;; results in a list of lists with one list for each player
             (let ((event-count 0))
               (loop for rs in player
                     append
                     (loop for event in rs
                           ;; only process a chord if desired
                           unless (and (numberp (is-chord event))
                                       (null chords))
                             append
                             (progn
                               ;; increase event counter
                               ;; NB: event count stays the same for
                               ;; each individual pitch in a chord,
                               ;; thus, a chord is treated here as a
                               ;; single entity
                               (incf event-count)
                               (let* ((actual-chord-length (is-chord event))
                                      ;; set values for p1-p3
                                      ;; i.e. instrument, onset, duration
                                      (p1-value (if (numberp instrument)
                                                    (format nil
                                                            "i~a"
                                                            instrument)
                                                    (format nil
                                                            "i \"~a\""
                                                            instrument)))
                                      (p2-value (+ offset
                                                   (start-time event)))
                                      (p3-value (duration event))
                                      ;; get values for any other p-field
                                      (p-field-values
                                        (cond
                                          ((functionp p-fields)
                                           (funcall p-fields
                                                    event
                                                    event-count
                                                    instrument))
                                          ((listp p-fields)
                                           (nth player-i p-fields))
                                          (t nil)))
                                      ;; determine the amount of i-statements
                                      ;; (here: pitches) for a chord.
                                      ;; this is necessary, as it is possible
                                      ;; to limit the amount of notes via the
                                      ;; :chord keyword-arg.
                                      ;; the value is set to the max. possible
                                      ;; notes (either the :chord-limit or the
                                      ;; available pitches in a chord)
                                      (pitches-per-chord
                                        (cond
                                          ;; when chord size is
                                          ;; limited, set the upmost
                                          ;; limit either to the length
                                          ;; of the chord (if lt) or
                                          ;; to the chord-limit
                                          ((null actual-chord-length) nil)
                                          ((and (numberp chords)
                                                (> actual-chord-length
                                                   chords))
                                           chords)
                                          (t actual-chord-length))))
                                 ;; process first chords, then pitches
                                 (if (numberp actual-chord-length)
                                     ;; process the chord note by note
                                     (loop for i from 0
                                             to (1- pitches-per-chord)
                                           collect
                                           (append
                                            (list p1-value
                                                  p2-value
                                                  p3-value)
                                            ;; could be a list of lists (when
                                            ;; data is generated by a function)
                                            ;; or a one-dimensional list with
                                            ;; n-values (cf. documentation)
                                            (if (every #'listp p-field-values)
                                                (nth i p-field-values)
                                                p-field-values)))
                                     ;; process a (single) pitch
                                     (list (append (list p1-value
                                                         p2-value
                                                         p3-value)
                                                   ;; in this case, the value
                                                   ;; ought to be a list with
                                                   ;; n values
                                                   p-field-values)))))))))))
    ;; write to file
    (with-open-file (stream csound-file
                            :direction :output
                            :if-does-not-exist :create
                            :if-exists :supersede
                            :external-format :utf-8)
      ;; initial comments
      (when comments
        (format stream ";; TITLE: ~a~%~
                        ;; COMPOSER: ~a~%~
                        ;; YEAR: ~a~%~
                        ;; GENERATION DATE: ~a~%"
                (title sc)
                (composer sc)
                (year sc)
                (multiple-value-bind
                      ;; MDE Sat Mar 16 15:40:15 2024, Heidhausen -- compiler
                      ;; warnings about unused vars: don't need to bind all
                      ;; values if they're not going to be used
                      (second minute hour day month year) ; weekday dst-p tz)
                    (get-decoded-time)
                  (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d"
                          year month day hour minute second))))
      (loop for instrument in score-events
            for instrument-i from 0 do
              (when comments
                (format stream "~%;; PLAYER: ~a~%~%"
                        (nth instrument-i players)))
              (loop for ins-data in instrument
                    ;; ignore empty lists (e.g. produced by ignored chords)
                    ;; might be redundant as ignored chords are already
                    ;; sorted out "a priori" in the loop (see above)
                    ;; RP  Mon Feb 20 01:20:51 2023
                    unless (not ins-data)
                      do
                         (let ((control-string (concatenate
                                                'string
                                                "~{~a~^"
                                                (string delimiter)
                                                "~}~%")))
                           (format stream control-string ins-data))))
      (format stream "~%~%"))
    csound-file))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* csound/csound-p-fields-simple
;;; AUTHOR
;;; Ruben Philipp <ruben.philipp@folkwang-uni.de>
;;;
;;; DATE
;;; 2023-02-28, Essen
;;;
;;; DESCRIPTION
;;; This function is intended to be used in conjunction with
;;; the write-csound-score method. It returns a list or -- when the given event
;;; is a chord -- a list of lists of p-values.
;;;
;;; The p-fields are allocated as follows:
;;; - p4: frequency
;;; - p5: amplitude
;;;
;;; N.B.: The event-num and cs-instrument arguments are mandatory as they
;;;       are required by the write-csound-score method, even though they are
;;;       not used in this function.
;;;
;;; ARGUMENTS
;;; - An event object (most likely the event currently processed by
;;;   write-csound-score).
;;; - A number referring as an index to the position in processing sequence.
;;; - A reference to a Csound-instrument (number or string).
;;;
;;; RETURN VALUE
;;; Returns either a list (when the event is a pitch) or a list of lists
;;; (in case the event contains a chord) with p4- and p5-values (see
;;; above).
;;;
;;;
;;; SYNOPSIS
(defun csound-p-fields-simple (event event-num cs-instrument)
  ;;; ****
  ;; event-num and cs-instruments are not used in
  ;; this function. ignore
  ;; RP  Tue Feb 28 14:46:17 2023
  (declare (ignore event-num
                   cs-instrument))
  (let ((freq (get-frequency event))
        (amplitude (get-amplitude event)))
    (if (listp freq)
        ;; return a list of lists for the chord
        (loop for f in freq
              collect
              ;; limit the float decimal places to 4
              ;; as Csound has issues dealing with big floats
              ;; in scores
              (list (format nil "~,4f" f)
                    amplitude))
        (list (format nil "~,4f" freq)
              amplitude))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* slippery-chicken/csound-play
;;; AUTHOR:
;;; Ruben Philipp <ruben.philipp@folkwang-uni.de>
;;;
;;; DATE
;;; 2023-03-02, Essen
;;;
;;; DESCRIPTION
;;; This method outputs a soundfile from a slippery-chicken object by
;;; generating a Csound-score via write-csound-score and calling the Csound
;;; command with a given Csound-Orchestra as an additional argument. Hence,
;;; this method's arguments resemble those of write-csound-score, except for
;;; the argument indicating the orchestra file and a few keyword-arguments
;;; which primarily deal with the soundfile output.
;;;
;;; In order to work properly, please make sure to have Csound properly
;;; installed on your system and set the value of 'csound-command via
;;; (set-sc-config ...) according to your system's configuration.
;;;
;;; ARGUMENTS
;;; - A slippery chicken object.
;;; - The ID(s) of the player(s) whose events are used to obtain the
;;;   rhythmic structure of the score events. This must be a list with one or
;;;   more symbols. See write-csound-score for more detail.
;;; - The ID(s) (number) or name(s) (string) of the Csound instruments
;;;   according to the resp. instrument definition in the Csound orchestra.
;;;   This must be a list with one or more values. See write-csound-score for
;;;   more information.
;;; - The path to the Csound-orchestra file (.orc) to be used for rendering
;;;   the output soundfile. Make sure that it contains instrument definitions
;;;   for all Csound-instruments set in the preceding argument. This must be
;;;   a string.
;;;
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :csound-flags. A list of strings containing the flags, for the Csound
;;;   command. Altering these might be useful e.g. to change the output format
;;;   of the soundfile. N.B.: The "-o" argument, usually preceding the output
;;;   soundfile name, is already set per default and should not be included in
;;;   this list. For a full list of available arguments, see the Csound-manual.
;;;   Default = '("-d" "-W" "-f")
;;; - :sndfile-suffix. A string to be added just before the file-extension of
;;;   the generated soundfile. Will be overridden when manually setting the
;;;   sndfile-path via :sndfile. Default = "".
;;; - :sndfile-extension. A string specifying the file extension of the
;;;   soundfile to be generated by the Csound command. This might be altered
;;;   in case the output format is changed e.g. via :csound-flags. Will be
;;;   overridden when manually setting the sndfile-path via :sndfile.
;;;   Default = ".wav".
;;; - :sndfile. A string indicating the path and filename to the soundfile to be
;;;   generated with this method. Please bear in mind to alter the suffix if you
;;;   change the format via :csound-flags. Default is a filename extracted from
;;;   the title of the sc piece, placed in the (get-sc-config 'default-dir)
;;;   directory (per default /tmp).
;;; - The other keyword arguments are exactly the same as write-csound-score.
;;;   Please see write-csound-score for detailed documentation.
;;;
;;; RETURN VALUE
;;; Returns the path of the soundfile written, as a string.
;;;
;;; EXAMPLE

#|
;;; An example using the csound-p-fields-simple function.
;;; The Csound orchestra might consist of two instruments producing
;;; tones based on simple waveforms (sines, square waves etc.)
(let ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((pno (piano :midi-channel 1))
                      (vln (violin :midi-channel 2))))
         :set-palette '((1 ((f3 g3 as3 a3 bf3 b3 c4 d4 e4 f4 g4 a4 bf4 cs5))))
         :set-map '((1 (1 1 1 1 1 1 1))
                    (2 (1 1 1 1 1 1 1))
                    (3 (1 1 1 1 1 1 1)))
         :tempo-map '((1 (q 60)))
         :rthm-seq-palette '((1 ((((4 4) h (q) e (s) s))
                                 :pitch-seq-palette ((1 (2) 3))))
                             (2 ((((4 4) (q) e (s) s h))
                                 :pitch-seq-palette ((1 2 3))))
                             (3 ((((4 4) e (s) s h (q)))
                                 :pitch-seq-palette ((2 3 3))))
                             (4 ((((4 4) (s) s h (q) e))
                                 :pitch-seq-palette ((3 1 (2))))))
         :rthm-seq-map '((1 ((pno (1 2 1 2 1 2 1))
                             (vln (1 2 1 2 1 2 1))))
                         (2 ((pno (3 4 3 4 3 4 3))
                             (vln (3 4 3 4 3 4 3))))
                         (3 ((pno (1 2 1 2 1 2 1))
                             (vln (1 2 1 2 1 2 1))))))))
  (csound-play mini
               '(pno vln)
               '(1 2)
               "mini-orchestra.orc"
               :comments nil))
|#

;;; SYNOPSIS
(defmethod csound-play (sc
                        players
                        csound-instruments
                        ;; RP  Thu Mar  2 21:17:59 2023
                        ;;; unique to csound-play
                        orchestra-file
                        &key
                          ;; RP  Thu Mar  2 19:10:21 2023
                          ;; specific to csound-play
                          (csound-flags '("-d" "-W" "-f"))
                          ;; add something just before the
                          ;; sndfile-extension?
                          (sndfile-suffix "")
                          ;; choose the right file extension
                          ;; according to your format!
                          (sndfile-extension ".wav")
                          (sndfile (format nil "~a~a~a~a"
                                      (get-sc-config 'default-dir)
                                      (filename-from-title (title sc))
                                      sndfile-suffix
                                      sndfile-extension))
                        ;; RP  Thu Mar  2 19:32:09 2023
                        ;; analogous to write-csound-score
                          (start-section 1)
                          (offset 0)
                          (suffix "")
                          (csound-file
                           (format nil "~a~a~a.sco"
                                   (get-sc-config 'default-dir)
                                   (filename-from-title (title sc))
                                   suffix))
                          (comments t)
                          (delimiter #\space)
                          (from-sequence 1)
                          (num-sequences nil)
                          (chords t)
                          (p-fields #'csound-p-fields-simple)
                          (num-sections nil))
  ;;; ****
  (let* ((sco-file (write-csound-score sc
                                       players
                                       csound-instruments
                                       :start-section start-section
                                       :offset offset
                                       :suffix suffix
                                       :csound-file csound-file
                                       :comments comments
                                       :delimiter delimiter
                                       :from-sequence from-sequence
                                       :num-sequences num-sequences
                                       :chords chords
                                       :p-fields p-fields
                                       :num-sections num-sections))
         ;; RP  Tue Mar  7 12:59:07 2023
         ;; test if Csound command is available before
         ;; trying to render the piece
         (success (if (probe-file (get-sc-config 'csound-command))
                      (apply #'shell
                             (append
                              (list (get-sc-config 'csound-command))
                              csound-flags
                              (list "-o"
                                    sndfile
                                    orchestra-file
                                    sco-file)))
                      (warn "slippery-chicken::csound-play: ~
                             Csound is not available on your system. ~
                             Please make sure that the csound binary is ~
                             correctly installed and (set-sc-config ~
                             'csound-command ...) is properly set."))))
    (if success
        sndfile
        (error "slippery-chicken::csound-play: ~
                Call to Csound failed."))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* slippery-chicken/event-list-to-csound-score
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-05-07
;;; 
;;; DESCRIPTION
;;; This function generates a Csound-score file (.sco) from an event-list.
;;; Its structure is very similar to write-csound-score except that the
;;; allocation process between players and Csound-instruments ist structured
;;; differently. This function uses the event-list's MIDI-channels for
;;; allocating the data to Csound-instruments (if desired).
;;; NB: The function events-update-time might be useful in this context (also
;;; refer to event-list-to-midi-file).
;;;
;;; ARGUMENTS
;;; - A list of event objects.
;;; - The MIDI-channel(s) to take into account for generating the score.
;;;   This must be a list with one or more symbols or NIL. When NIL, all
;;;   MIDI-channels present in the event-list will be used. When a list,
;;;   only the MIDI-channels contained in this list will be used as a basis
;;;   for the rhythmic structure of the Csound-score. 
;;; - The ID(s) (as a number) or name(s) (as a string) of the Csound instruments
;;;   according to the resp. instrument definition in the Csound orchestra as
;;;   list of one or more items.
;;;   Each MIDI-channel of the second argument of this method needs to be
;;;   assigned to one Csound-instrument. Thus, the length of this list must
;;;   be exactly the same as the previous list, except when using NIL in the
;;;   MIDI-channels list. In the latter case, the Csound instruments defined
;;;   in this list will be allocated cyclically to the available MIDI-channels
;;;   in ascending order of the MIDI-channels.
;;;   E.g.: When the MIDI-channels 1, 2, 3, and 10 are present in the
;;;   event-list, and this argument is set to '("ins1" "ins2"), the value of
;;;   this slot will internally be changed to '("ins1" "ins2" "ins1" "ins2").
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword-arguments:
;;; - :suffix. A string to be added just before the .sco of the generated
;;;   csound score-file. Will be overridden when manually setting the path
;;;   via :csound-file. Default = "".
;;; - :csound-file. A string indicating the path and filename to the .sco-file
;;;   to be generated with this method. Default is "tmp.suffix.sco", placed
;;;   in the (get-sc-config 'default-dir) directory (per default /tmp).
;;; - :offset. An integer or floating point value to indicate the global
;;;   offset of all score events (in seconds). Default = 0.
;;; - :comments. A boolean indicating whether additional comments should be
;;;   included into the generated score. Default = T.
;;; - :delimiter. A character which separates each score statement. Csound
;;;   recommends using spaces, though #\tab does also work an might, in some
;;;   cases, improve legibility. Default = #\space.
;;; - :chords. Either a boolean or an integer which determines how to deal with
;;;   chords in events. When T, all notes of a chord are parsed as
;;;   i-statements in the Csound-score. When NIL, chords will be ignored and
;;;   treated like rests (i.e. nothing will be generated). When an integer is
;;;   given, the number determines the max. amount of pitches being parsed as i-
;;;   statements. They are chosen chronologically from the given chord.
;;;   Default = T.
;;; - :p-fields. This should be either NIL, a list of lists or a function used
;;;   to generate the p-fields starting from p4. Cf. the documentation of
;;;   write-csound-score for more detail.
;;;   Default: #'csound-p-fields-simple.
;;; 
;;; RETURN VALUE
;;; Returns the path of the file written, as a string.
;;;
;;; EXAMPLE
#|
(let* ((notes '(c4 d4 e4 f4 g4))
       (notes-len (length notes))
       (rthms '(q e s))
       (rthms-len (length rthms))
       (events (loop repeat 20
                     collect
                     (make-event (nth (random notes-len) notes)
                                 (nth (random rthms-len) rthms)
                                 :midi-channel (1+ (random 3))))))
  (events-update-time events)
  (event-list-to-csound-score events
                              nil ;; use all midi-channels
                              '(1 2)
                              :csound-file "/tmp/example.sco"))

;; => /tmp/example.sco
|#
;;; SYNOPSIS
(defun event-list-to-csound-score (event-list
                                   ;; when NIL, use all available
                                   ;; midi-channels and allocate them
                                   ;; to the given csound instruments
                                   ;; in a cyclical order
                                   midi-channels
                                   csound-instruments
                                   &key
                                     ;; add something just before .sco?
                                     (suffix "")
                                     (csound-file
                                      (format nil "~atmp~a.sco"
                                              (get-sc-config 'default-dir)
                                              suffix))
                                     ;; offset in seconds
                                     (offset 0)
                                     ;; add a comments section?
                                     (comments t)
                                     (delimiter #\space)
                                     (chords t)
                                     (p-fields #'csound-p-fields-simple))
  ;;; ****
  ;; test if a csound instrument is assigned to every midi-channel
  ;; unless midi-channels is not NIL (i.e. all midi-channels will be
  ;; used for generating the score)
  (when (and midi-channels
             (/= (length midi-channels)
                 (length csound-instruments)))
    (error "slippery-chicken::event-list-to-csound-score: ~
            midi-channels and csound-instruments differ in length."))
  ;; if the p-fields keywords is not a function, test if it is either
  ;; NIL, or
  ;; a list of lists containing p-field-values for each player/instrument
  (unless (or (functionp p-fields)
              (eq nil p-fields)
              (and (every #'listp p-fields)
                   (= (length p-fields)
                      (length midi-channels))))
    (error "slippery-chicken::event-list-to-csound-score: ~
            the p-fields keyword argument must be either NIL, ~% ~
            a function, or a list of lists with p-field values ~
            for each player"))
  ;;; when midi-channels is NIL, use all-midi-channels and allocate them
  ;;; to the given instruments chronologically.
  ;;; when less instruments than midi-channels are available,
  ;;; cyclically allocate each channel one instrument from the
  ;;; csound-instruments list
  (when (null midi-channels)
    ;;; get all midi channels available in the event-list
    (let ((available-midi-ch
            (loop for event in event-list
                  for midi-ch = (get-midi-channel event)
                  with midi-channels = '()
                  ;; add the channel to the list when it is not nil (e.g.
                  ;; when the event is a rest) and when the channel is not
                  ;; already represented in the channel-list
                  when (and midi-ch (not (member midi-ch midi-channels)))
                    do
                       (push midi-ch midi-channels)
                  finally
                     (return (sort midi-channels #'<)))))
      (setf midi-channels available-midi-ch)))
  (when (/= (length midi-channels)
            (length csound-instruments))
    (setf csound-instruments
          (loop for i from 0 to (1- (length midi-channels))
                with num-ins = (length csound-instruments)
                collect
                (nth (mod i num-ins) csound-instruments))))
  (let ((score-events
          (loop
            for event in event-list
            ;; the midi-channel of the event
            for midi-ch = (get-midi-channel event)
            for is-chord = (is-chord event)
            ;; event counter for the midi-channels
            with event-count = (make-assoc-list
                                'event-count
                                (mapcar #'(lambda (c) (list c 0))
                                        midi-channels))
            ;; just process the event when non-NIL and
            ;; member of the midi-channels list.
            when (and midi-ch (member midi-ch midi-channels)
                      ;; furthermore, only process chords when desired
                      (not (and (numberp is-chord)
                                (null chords))))
              append
              (progn
                ;;; increment event-counter for the channel
                (replace-data midi-ch
                              (1+ (get-data-data midi-ch event-count))
                              event-count)
                (let* ((current-event-count
                         (get-data-data midi-ch event-count))
                       ;; the position of the current instrument in the
                       ;; csound-instrements resp. midi-channels list
                       ;; analogous to the player-i in write-csound-score
                       (instrument-i (position midi-ch midi-channels))
                       (actual-chord-length is-chord)
                       ;; get the instrument for the event
                       (instrument (nth instrument-i
                                        csound-instruments))
                       ;; set values for p1-p3
                       ;; (i.e. instrument, onset, duration)
                       (p1-value (if (numberp instrument)
                                     (format nil
                                             "i~a"
                                             instrument)
                                     (format nil
                                             "i \"~a\""
                                             instrument)))
                       (p2-value (+ offset
                                    (start-time event)))
                       (p3-value (duration event))
                       ;; get the values for any other p-field
                       (p-field-values
                         (cond
                           ((functionp p-fields)
                            (funcall p-fields
                                     event
                                     current-event-count
                                     instrument))
                           ((listp p-fields)
                            (nth instrument-i p-fields))
                           (t nil)))
                       ;; determine the amount of i-statements
                       ;; (here: pitches) for a chord.
                       ;; this is necessary, as it is possible
                       ;; to limit the amount of notes via the
                       ;; :chord keyword-arg.
                       ;; the value is set to the max. possible
                       ;; notes (either the :chord-limit or the
                       ;; available pitches in a chord)
                       (pitches-per-chord
                         (cond
                           ;; when chord size is
                           ;; limited, set the upmost
                           ;; limit either to the length
                           ;; of the chord (if lt) or
                           ;; to the chord-limit
                           ((null actual-chord-length) nil)
                           ((and (numberp chords)
                                 (> actual-chord-length
                                    chords))
                            chords)
                           (t actual-chord-length))))
                  ;; process first chords, then pitches
                  (if (numberp actual-chord-length)
                      (loop for i from 0
                              to (1- pitches-per-chord)
                            collect
                            (append
                             (list p1-value
                                   p2-value
                                   p3-value)
                             (if (every #'listp p-field-values)
                                 (nth i p-field-values)
                                 p-field-values)))
                      ;; process a (single) pitch
                      (list (append (list p1-value
                                          p2-value
                                          p3-value)
                                    ;; in this case, the value
                                    ;; ought to be a list with
                                    ;; n values
                                    p-field-values))))))))
    ;; write to file
    (with-open-file (stream csound-file
                            :direction :output
                            :if-does-not-exist :create
                            :if-exists :supersede
                            :external-format :utf-8)
      ;; initial comments
      (when comments
        (format stream ";; GENERATION DATE: ~a~%"
                (multiple-value-bind
                      ;; MDE Sat Mar 16 15:40:15 2024, Heidhausen -- compiler
                      ;; warnings about unused vars: don't need to bind all
                      ;; values if they're not going to be used
                      (second minute hour day month year) ; weekday dst-p tz)
                    (get-decoded-time)
                  (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d"
                          year month day hour minute second))))
      (loop for event-data in score-events
            ;; ignore empty lists (e.g. produced by ignored chords)
            ;; might be redundant as ignored chords are already
            ;; sorted out "a priori" in the loop (see above)
            ;; borrowed from write-csound-score
            ;; RP  Sun May  7 18:29:53 2023
            unless (not event-data)
              do
                 (let ((control-string (concatenate
                                        'string
                                        "~{~a~^"
                                        (string delimiter)
                                        "~}~%")))
                   (format stream control-string event-data)))
      (format stream "~%~%"))
    csound-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF slippery-chicken.lsp
