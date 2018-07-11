;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* sclist/rthm-seq-bar
;;; NAME 
;;; rthm-seq-bar
;;;
;;; File:             rthm-seq-bar.lsp
;;;
;;; Class Hierarchy:  named-object -> linked-named-object -> sclist -> 
;;;                   rthm-seq-bar 
;;;
;;; Version:          1.0.8
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Implementation of the rthm-seq-bar class, objects of
;;;                   which make up the individual bars that reside in a
;;;                   rhythmic sequence.  This class is responsible for parsing
;;;                   lists containing rhythms and time signatures (but not
;;;                   parsing these things themselves--that is done by separate
;;;                   classes).
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    13th February 2001
;;;
;;; $$ Last modified:  18:18:13 Mon Jun  4 2018 CEST
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

;;; the rhythm objects are in the rhythms slot, the original data is in the
;;; data slot  
(defclass rthm-seq-bar (sclist)
  ;; This is an index to the time-sig object stored in the all-time-sigs slot.
  ((time-sig :accessor time-sig :type integer :initform -1)
   ;; Whether a time-sig was given for this bar or not.
   (time-sig-given :accessor time-sig-given :type boolean :initform nil)
   ;; The bar number of this rsb, set by sequenz::update-slots
   (bar-num :accessor bar-num :type integer :initform -1)
   ;; when we re-bar it's still useful to know old (original) bar
   ;; numbers
   (old-bar-nums :accessor old-bar-nums :type list :initform nil)
   (write-bar-num :accessor write-bar-num :type boolean :initform nil)
   (start-time :accessor start-time :type number :initarg :start-time
               :initform -1.0)
   (start-time-qtrs :accessor start-time-qtrs :type number :initarg
                    :start-time-qtrs :initform -1.0)
   ;; The parsed rhythms as a list.  N.B. tied notes that are returned by
   ;; parse-possibly-compound-rhythm are appended to the other notes--the list
   ;; disappears! 
   ;; The rhythms are changed to events in the
   ;; slippery-chicken::sc-make-sequenz method 
   (rhythms :accessor rhythms :type list :initform nil)
   ;; MDE Thu Apr 19 10:23:48 2012 -- for statistics: the duration of the
   ;; sounding notes, in the tempo. We'll leave this at nil
   ;; and only update it the first time it's requested.  Note this slot has
   ;; different reader and writer methods.
   (sounding-duration :initform nil :reader sounding-duration
                      :writer set-sounding-duration)
   ;; the length of rhythms.
   (num-rhythms :accessor num-rhythms :type integer :initform -1)
   ;; the number of rests in this bar.
   (num-rests :accessor num-rests :type integer :initform -1)
   ;; the number of notes for the score, whether tied or not N.B. a chord
   ;; counts as one note!  
   (num-score-notes :accessor num-score-notes :type integer :initform -1)
   ;; Whether this bar is simply a rest bar or not.
   (is-rest-bar :accessor is-rest-bar :type boolean :initform nil)
   ;; If this is a rest bar, then should we display a whole-bar rest or not?
   (show-rest :accessor show-rest :type boolean :initarg :show-rest
              :initform t)
   ;; How many notes will be needed for this bar?  i.e. how many are neither
   ;; rests nor ties?
   (notes-needed :accessor notes-needed :type integer :initform -1)
   ;; The parse-rhythms function can handle bracketing of triplets, quintuplets
   ;; (generic: 'tuplet') as indicated in the comments to that function.  Those
   ;; discerned by the function are stored here.  These are indices and
   ;; 0-based. 
   ;; (score-tuplets :accessor score-tuplets :type list :initform nil)
   ;; the above is for SCORE (long defunct, sadly), the following for CMN (see
   ;; parse-rhythms)  
   (tuplets :accessor tuplets :type list :initform nil)
   ;; In SCORE, how far to extend tuplet brackets when it's over a rest at
   ;; either end.  
   (nudge-factor :accessor nudge-factor :type number :initarg :nudge-factor
                 :initform 0.35 :allocation :class)  
   ;; parse-rhythm can also handle beaming information, store that here.  Also
   ;; 0-based indices.
   (beams :accessor beams :type list :initform nil)
   ;; when we make a rest bar, especially one with an invisible rest for
   ;; placing the sets in, we want to place a rest minus a certain duration so
   ;; we can put the chord in with this duration.  This is always a value
   ;; suitable for parse-possibly-compound-rhythm e.g. e. (gets converted
   ;; before storing).
   (missing-duration :accessor missing-duration :initarg :missing-duration
                     :initform nil) 
   ;; When a time-sig is not indicated for a bar, then we assign the index to
   ;; the last one as stored here.
   (current-time-sig :accessor current-time-sig :type integer :initform -1
                     :allocation :class)
   ;; All the time-sigs this class has seen so far are stored here so we don't
   ;; have to duplicate time-sig objects for every bar, but rather give a
   ;; simple reference to those stored in this list.  This list has all the
   ;; unique time-sigs the class has seen so far.
   (all-time-sigs :accessor all-time-sigs :initform nil :allocation :class)
   ;; whether a single (0), double (1), or end-of-piece double (2) bar line
   ;; should be written. 
   ;; MDE Wed Mar 21 07:44:10 2012 -- added repeat barlines: 3 = begin repeat,
   ;; 4 = begin and end repeat, 5 = end repeat
   (bar-line-type :accessor bar-line-type :type integer 
                  :initarg :bar-line-type :initform 0)
   ;; Whether the time-sig should be written or not.  Every rthm-seq starts
   ;; with a bar that includes a time-sig so at first this slot will be set to
   ;; t, because the class sees a time-sig when the rthm-seq-bar is created.
   ;; This would have to be cancelled later however when the rthm-seqs are
   ;; combined to form longer sections and the last bar of one rthm-seq has the
   ;; same time-sig as that of the first of the following.  We can't know that
   ;; for now however so this simply indicates whether a bar was given a
   ;; time-sig or not.
   (write-time-sig :accessor write-time-sig :type boolean :initform nil)
   ;; which section and player did this bar come from?  i.e. which section and
   ;; which player within the whole piece
   (player-section-ref :accessor player-section-ref :type list :initform nil)
   ;; attach a rehearsal letter to the bar line?
   ;; 31.1.11 NB in LilyPond this will be attached to the last note in the bar
   ;; and actually goes on the bar line at the end of this bar.
   (rehearsal-letter :accessor rehearsal-letter :initform nil)
   ;; which sequenz number of the section is this?  definitely 0-based
   (nth-seq :accessor nth-seq :initform nil)
   ;; which bar number of the sequenz is this?
   (nth-bar :accessor nth-bar :initform nil)
   ;; 30/3/07 when writing parts we can get get-cmn-data to ignore the bar if
   ;; it's part of a multi-bar rest.  This slot is set automatically in
   ;; sc::multi-bar-rests and will be T if the bar is part of a multi-bar rest
   ;; (so it should be ignored), nil if it should be processed as normal, and
   ;; an integer if it's the first bar of a multi-bar-rest (so get-cmn-data
   ;; needs to create that rest).  BTW, adding a number to a whole bar's rest
   ;; (e.g. (whole-measure-rest 5)) does not increment cmn's measure count
   ;; accordingly; the var we need is probably *cmn-measure-number* 
   (multi-bar-rest :accessor multi-bar-rest :initform nil)
   ;; MDE Fri Jan 26 20:46:11 2018 -- so we know where it came from: the
   ;; reference into the rthm-seq-palette to the seq where the bar arose
   (rsp-id :accessor rsp-id ::initform nil)
   ;; when we generate an rsb with chop, we need to keep track of the attack
   ;; number of the start and end note that the new bar was extracted from in
   ;; the old bar.
   (parent-start-end :accessor parent-start-end :type list :initform nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 5/4/07: NB after making an rsb, is-tied-from slots of each rhythm are not
;;; set correctly; that only first gets done at the rthm-seq level.

(defmethod initialize-instance :after ((rsb rthm-seq-bar) &rest initargs)
  (declare (ignore initargs))
  (let* ((data (data rsb))
         (first (first data))
         ;; when the first element of data is a 2-element list of atoms, it
         ;; must be a time signature.  
         (ts (cond ((and (simple-listp first) (= 2 (length first)))
                    (make-time-sig first))
                   ((time-sig-p first) first)))
         (rthms (if ts (rest data) data))
         ;; this won't pick up on the mixed cases (rqq for some beats, normally
         ;; typed rhythms for others) but we'll have to live with that as we
         ;; only need this for beam checking and we don't want to disrupt
         ;; explicitly given beams.
         (just-rqq (and (= 1 (length rthms)) (is-rqq-info (first rthms))))
         (parsed (parse-rhythms rthms (nudge-factor rsb)))
         (rthm nil)
         (rhythms nil))
    ;; Find out if a time-sig is given, if so, calling setf time-sig will set
    ;; the fields correctly.  
    (if ts
        (setf (time-sig rsb) ts)
      ;; If not given given, set the time-sig from the current-time-sig slot.
      (setf (slot-value rsb 'time-sig) (current-time-sig rsb)))
    (when parsed 
      ;; Loop through the given rthms and parse them (appending a call to
      ;; parse-possible-compound-rhythm).  
      (setf rhythms (loop for i in (first parsed) 
                          do (setq rthm (if (typep i 'rhythm)
                                            i
                                          (parse-possibly-compound-rhythm i)))
                          if (listp rthm)
                          ;; we store compound rhythms (e+ts) not in a list
                          ;; but just like any other two or more rhythms.
                          appending rthm
                          else collect rthm)
            (rhythms rsb) rhythms)
      ;; Store the given bracketing information in tuplets.
      (setf ;; (score-tuplets rsb) (second parsed)
            (beams rsb) (third parsed)
            ;; sort the tuplets according to the order of the note on which the
            ;; brackets start.
            (tuplets rsb) (sort (fourth parsed)
                                #'(lambda (x y)
                                    (let ((x2 (second x))
                                          (y2 (second y)))
                                      ;; MDE Tue Jun 23 20:27:42 2015 -- if we
                                      ;; have nested tuplets then make sure the
                                      ;; one that spans the most notes comes
                                      ;; first. 
                                      (if (= x2 y2)
                                          (> (third x) (third y))
                                          (< x2 y2))))))
      (unless (is-full rsb 'warn)
        (error "~a~%rthm-seq-bar::initialize-instance:~
               ~%Incorrect number of beats in bar: Expected ~a, got ~a~
               ~%Perhaps you forgot to change the time signature??? ~%~a~%"
               rhythms
               (duration (get-time-sig-from-all-time-sigs rsb))
               (rhythms-duration rsb)
               data)))
    ;; MDE Thu Jun  4 16:10:12 2015
    (when just-rqq
      (check-beams rsb))
    ;;    MDE Sun Jun 28 13:03:04 2015 -- no, rqq handling now sets its own
    ;;    beams (auto-beam rsb nil 'silent))
    (gen-stats rsb)
    (update-missing-duration rsb)
    (update-rhythms-bracket-info rsb)
    (update-rhythms-beam-info rsb)
    (fix-nested-tuplets rsb)
    ;; 2/04
    ;; 17/5/05: now handled at piece level
    ;; (update-compound-durations rsb)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sum the rhythms and compare the duration against that of the time sig.
;;; error can either be t 'warn or nil; if the latter no warning or error will
;;; be issued i.e. it will return nil silently.
;;; MDE Mon Apr 23 13:00:59 2012 -- added not-enough arg to by default check
;;; for underfull bars also
(defmethod is-full ((rsb rthm-seq-bar) &optional (error t) (not-enough t))
  (let* ((rthms-dur (rhythms-duration rsb))
         (ts-dur (duration (get-time-sig-from-all-time-sigs rsb
                                                            (time-sig rsb))))
         (ok (equal-within-tolerance rthms-dur ts-dur .001)))
    (when (and (not ok)
               ;; MDE Mon Apr 23 12:56:57 2012 -- not enough rhythms should
               ;; signal an error/warning too! 
               (or (> rthms-dur ts-dur)
                   (and not-enough (< rthms-dur ts-dur))))
      (when error
        (funcall (if (eq error 'warn) #'warn #'error)
                 "~%rthm-seq-bar::is-full:~%~
                   Duration of rhythms (~a) is not the duration of the ~
                   time-sig: (~a)~%~a"
                 rthms-dur ts-dur rsb)))
    ;; MDE Fri May 13 14:37:57 2016 -- return a second value that shows
    ;; under/overfill: positive will be underfill, negative overfill
    (values ok (- ts-dur rthms-dur))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod rhythms-duration ((rsb rthm-seq-bar))
  (sum-rhythms-duration (rhythms rsb)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod bar-duration ((rsb rthm-seq-bar) &optional (tempo 60.0))
  (unless (typep tempo 'tempo)
    (setf tempo (make-tempo tempo)))
  (* (qtr-dur tempo)
     (duration (get-time-sig rsb))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod player ((rsb rthm-seq-bar))
  (player (get-nth-event 0 rsb)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* rthm-seq-bar/delete-marks
;;; DESCRIPTION
;;; Delete all marks from the rhythm (or event) objects contained within a given
;;; rthm-seq-bar object.
;;; 
;;; ARGUMENT
;;; - A rthm-seq-bar object.
;;; 
;;; RETURN VALUE
;;; Always returns NIL.
;;; 
;;; EXAMPLE

#|

;; Create a rthm-seq-bar object and print the contents of the MARKS slots of
;; the contained event objects to see they're set to NIL by default. Fill them
;; each with a 's (staccato) mark and print the results. Apply the delete-marks
;; method and print the results again to see that the values have been reset to
;; NIL. 
(let ((rsb (make-rthm-seq-bar 
            (list
             '(3 8) 
             (make-event 'cs4 'e)
             (make-event 'cs4 'e)
             (make-event 'cs4 'e)))))
  (print (loop for e in (rhythms rsb) collect (marks e)))
  (loop for e in (rhythms rsb) do (add-mark-once e 's))
  (print (loop for e in (rhythms rsb) collect (marks e)))
  (delete-marks rsb)
  (print (loop for e in (rhythms rsb) collect (marks e))))

=>
(NIL NIL NIL) 
((S) (S) (S)) 
(NIL NIL NIL)

|#

;;; SYNOPSIS
(defmethod delete-marks ((rsb rthm-seq-bar))
;;; ****
  (loop for r in (rhythms rsb) do
       (delete-marks r)))

#|
MDE Thu Dec 29 11:51:19 2011 -- changed the code below to that above so that not only event objects but rhythms too lose their marks
  (loop for event in (rhythms rsb) do
        (when (event-p event)
          (delete-marks event))))
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod delete-rhythms ((rsb rthm-seq-bar))
  (setf (rhythms rsb) nil
        (data rsb) nil)
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* rthm-seq-bar/add-half-beat-rest
;;; DATE
;;; August 25th 2016, Edinburgh
;;; 
;;; DESCRIPTION
;;; Adds a half beat rest at the end of the bar. This is a destructive
;;; method. It will change the time signature, so a 2/4 bar becomes a 5/8
;;; bar, with an 1/8th rest at the end.
;;; 
;;; ARGUMENTS
;;; - rthm-seq-bar object
;;; 
;;; OPTIONAL ARGUMENTS
;;; - ignore: don't pass this; it's only there so we can have a bar number
;;; passed in the slippery-chicken class.
;;; 
;;; RETURN VALUE
;;; The modified rthm-seq-bar object passed as an argument
;;; 
;;; SYNOPSIS
(defmethod add-half-beat-rest ((rsb rthm-seq-bar) &optional ignore)
;;; ****  
  (declare (ignore ignore))
  (let* ((ts (get-time-sig rsb))
         (denom2 (* 2 (denom ts))))
    (setf (time-sig rsb) (list (1+ (* 2 (num ts))) denom2))
    (add-event rsb (make-rest denom2))
    (gen-stats rsb)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod add-event ((rsb rthm-seq-bar) event &key (position nil))
  (setf (rhythms rsb)
        (if position
            (splice (list event) (rhythms rsb) position)
            (econs (rhythms rsb) event))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 12.12.11 SAR: Added ROBODoc info
;;; SAR Sun Apr 29 16:30:23 BST 2012: Expanded robodoc info

;;; ****m* rthm-seq-bar/fill-with-rhythms
;;; DESCRIPTION
;;; Any rhythms (or event objects) in the existing rthm-seq-bar object will be
;;; deleted, and then rhythm (or event) objects will be taken one by one from
;;; the <rhythms> argument until the bar is full. 
;;;
;;; If too few rhythm or event objects are given, a warning will be printed
;;; that there are too few beats in the bar.
;;;
;;; If there are too many and the last rhythm or event object to be placed in
;;; the bar fills out the bar evenly, no warning is printed and the remaining
;;; rhythm or event objects are ignored. If :is-full-error is T (default) and
;;; the last rhythm or event object that the method attempts to place in the
;;; bar is too long to fit evenly into the bar, the method will drop into the
;;; debugger with an error. If in this case :is-full-error is NIL the bar will
;;; remain underfull and the index of the last rhythm added before over-filling
;;; will be returned. 
;;;
;;; The :transposition, :midi-channel, and :microtones-midi-channel arguments
;;; can only be used in conjunction with event objects.
;;;
;;; The number of rhythms (or event objects) used is returned.
;;;
;;; NB: This method does not change the DATA slot of the rthm-seq-bar object
;;;     itself to reflect the new rhythms. Instead, it changes the contents of
;;;     the RHYTHMS slot within that object and changes the DATA of the
;;;     rthm-seq-bar object to NIL. It also assigns the ID of the named-object
;;;     to "rhythms-inserted-by-fill-with-rhythms".
;;; 
;;; ARGUMENTS
;;; - A rthm-seq-bar object.
;;; - A list of rhythm objects or event objects.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :transposition. An integer or NIL to indicate the transposition in
;;;   semitones for written pitches of any event objects passed. If NIL, no
;;;   written-pitches will be created. Default = NIL.
;;; - :midi-channel. An integer that will be used to set the MIDI-CHANNEL slot
;;;   of any event objects passed. Default = 0.
;;; - :microtones-midi-channel. An integer that is the MIDI channel that will
;;;   be assigned to event objects for microtonal MIDI pitches. NB: This value
;;;   is only set when attached to event objects within a slippery-chicken
;;;   object. Default = 0. NB: See player.lsp/make-player for details on
;;;   microtones in MIDI output.  
;;; - :new-id. An optional ID for new rhythm or event objects added. 
;;;   Default = "rhythms-inserted-by-fill-with-rhythms". 
;;; - :warn. T or NIL to indicate whether a warning should be printed if there
;;;   are not enough rhythms to create a full bar. T = warn. Default = T.
;;; - :is-full-error. T or NIL to indicate whether the last rhythm or event
;;;   object that the method attempts to add to the bar is too long to fit
;;;   evenly into the bar. T = drop into the debugger with an error if this is
;;;   the case. Default = T.
;;; 
;;; RETURN VALUE
;;; The number of rhythm or event objects used, which could be 0. NIL will be
;;; returned if, for example, an empty list of rhythms were passed, so do check
;;; for a NIL result. 
;;; 
;;; EXAMPLE
#|

(let ((rsb (make-rthm-seq-bar '((3 4) q q q))))
  (fill-with-rhythms rsb (loop for r in '(e e e e e e)
                          collect (make-rhythm r))))

=> 6

(let ((rsb (make-rthm-seq-bar '((3 4) q q q))))
  (fill-with-rhythms rsb (loop for r in '(e e e e e e)
                          collect (make-rhythm r)))
  (print-simple rsb))

=> NIL
(3 4): note E, note E, note E, note E, note E, note E, 

(let ((rsb (make-rthm-seq-bar '((3 4) q q q))))
  (fill-with-rhythms rsb (loop for r in '(e e e e e e)
                          collect (make-rhythm r)))
  (print rsb))

=>
RTHM-SEQ-BAR: time-sig: 0 (3 4)
              time-sig-given: T
              bar-num: -1
              old-bar-nums: NIL
              write-bar-num: NIL
              start-time: -1.0
              start-time-qtrs: -1.0
              is-rest-bar: NIL
              multi-bar-rest: NIL
              show-rest: T
              notes-needed: 6
              tuplets: NIL
              nudge-factor: 0.35
              beams: NIL
              current-time-sig: 0
              write-time-sig: T
              num-rests: 0
              num-rhythms: 6
              num-score-notes: 6
              rhythms: (
RHYTHM: value: 8.0, duration: 0.5, rq: 1/2, is-rest: NIL, score-rthm: 8.0, 
        undotted-value: 8, num-flags: 1, num-dots: 0, is-tied-to: NIL, 
        is-tied-from: NIL, compound-duration: 0.5, is-grace-note: NIL, 
        needs-new-note: T, beam: NIL, bracket: NIL, rqq-note: NIL, 
        rqq-info: NIL, marks: NIL, marks-in-part: NIL, letter-value: 8, 
        tuplet-scaler: 1, grace-note-duration: 0.05,
LINKED-NAMED-OBJECT: previous: NIL
                     this: NIL
                     next: NIL
NAMED-OBJECT: id: E, tag: NIL, 
data: E
[...]
NAMED-OBJECT: id: "rhythms-inserted-by-fill-with-rhythms", tag: NIL, 
data: NIL

;;; Using the :transpositions and :midi-channel arguments
(let ((rsb (make-rthm-seq-bar '((4 4) q q q q))))
  (fill-with-rhythms rsb (loop for r in '(h q e s s)
                              for p in '(c4 dqs4 e4 gqf4 a4)
                            collect (make-event p r))
                     :microtones-midi-channel 12
                     :transposition -14
                     :midi-channel 11)
  (print
   (loop for e in (rhythms rsb)
      collect (data (pitch-or-chord e))))
  (print 
   (loop for e in (rhythms rsb)
      collect (data (written-pitch-or-chord e))))
  (print 
   (loop for e in (rhythms rsb)
      collect (midi-channel (pitch-or-chord e)))))

=>
(C4 DQS4 E4 GQF4 A4) 
(D5 EQS5 FS5 AQF5 B5) 
(11 12 11 12 11)

|#
;;; SYNOPSIS
(defmethod fill-with-rhythms ((rsb rthm-seq-bar) rhythms
                              &key
                                ;; 24.3.11 add this too to make sure written
                                ;; pitch is set--this is the instrument
                                ;; transposition e.g. -14 for bass clarinet
                                transposition
                                (midi-channel 0)
                                (microtones-midi-channel 0)
                                (new-id "rhythms-inserted-by-fill-with-rhythms")
                                (warn t)
                                (is-full-error t))
;;; ****
  (delete-rhythms rsb)
  (let ((count 
         (loop for r in rhythms and i from 1 do
              (unless (typep r 'rhythm)
                (error "rthm-seq-bar::fill-with-rhythms: ~
                         Second argument must be a list of rhythm objects: ~a"
                       r))
              (when (event-p r)
                ;; 24.3.11
                (when (and transposition (not (zerop transposition)))
                  ;; remember the instrument transposition is from written to
                  ;; sounding so we need to go the other way here as all
                  ;; event pitches will be sounding
                  (if (and (pitch-or-chord r) (written-pitch-or-chord r))
                      (when warn
                        (warn "rthm-seq-bar::fill-with-rhythms: transposition ~
                               given but event has written and sounding ~
                               pitches already. Not altering: ~a" r))
                      (set-written r (- transposition))))
                (set-midi-channel r midi-channel microtones-midi-channel))
              (push r (rhythms rsb))
            ;; MDE Mon Apr 23 13:04:32 2012 -- don't check for underfull now
            ;; we have that option by default
              (multiple-value-bind
                    (bool spill)
                  (is-full rsb is-full-error nil)
              (cond (bool (return i))
                    ((< spill 0)
                     (pop (rhythms rsb))
                     (return (1- i)))))
              finally (return i))))
    (when (and warn (not count))
      (warn "rthm-seq-bar::fill-with-rhythms: Couldn't fill bar num ~a!"
            (bar-num rsb)))
    (when count
      ;; (print 'reverse)
      (setf (rhythms rsb) (reverse (rhythms rsb)) ;; this calls gen-stats
            (id rsb) new-id))
    count))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 12.12.11 SAR: Modified example and tidied ROBODoc descrips.
;;; ****m* rthm-seq-bar/all-rests?
;;; DESCRIPTION
;;; Test whether all rhythms in a rthm-seq-bar object are rests.
;;; 
;;; ARGUMENTS 
;;; - A rthm-seq-bar object.
;;; 
;;; RETURN VALUE  
;;; T if all rhythms are rests, otherwise NIL
;;; 
;;; EXAMPLE
#|
(let ((rsb (make-rthm-seq-bar '((2 4) (q) (e) (s) (s)))))
  (all-rests? rsb))

=> T

(let ((rsb (make-rthm-seq-bar '((2 4) q e s s))))
  (all-rests? rsb))

=> NIL
|#
;;; SYNOPSIS
(defmethod all-rests? ((rsb rthm-seq-bar))
;;; ****
  (rhythms-all-rests? (rhythms rsb)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Wed May  2 17:56:01 BST 2012: Added robodoc entry

;;; ****m* rthm-seq-bar/consolidate-rests-max
;;; DESCRIPTION
;;; Similar to consolidate-rests, but calls that method repeatedly until no
;;; more changes can be made to the given rthm-seq-bar object.
;;;
;;; NB This will still only reduce rests down to a maximum of a beat.  If you
;;;    need e.g. two quarter rests reduced to a single half rest in a 4/4 bar,
;;;    specify :beat 2
;;; 
;;; ARGUMENTS
;;; - A rthm-seq-bar object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :beat. The beat basis into which rests are to be consolidated. If no
;;;   value is given for this option, the method will take the beat from the
;;;   time signature. 
;;; - :min. A seldom-used argument that will only make a difference when there
;;;   are a number of rests of the same duration followed by a note.  This is
;;;   then the minimum duration that such rests may have if they are to be
;;;   consolidated. Default = NIL.
;;; - :warn. T or NIL to indicate whether the method should print a warning to
;;;   the Lisp listener if it is mathematically unable to consolidate the
;;;   rests. T = print warning. Default = NIL.
;;; 
;;; RETURN VALUE
;;; The rthm-seq-bar-object
;;; 
;;; EXAMPLE
#|
;;; Two examples with the same result; the first calling consolidate-rests
;;; twice, the second calling consolidate-rests-max
(let ((rsb (make-rthm-seq-bar '((2 2) (e) (e) (e) (e) (e) (s) (s) (s) e.))))
  (consolidate-rests rsb)
  (consolidate-rests rsb)
  (loop for r in (rhythms rsb) collect (data r)))

=> (2 Q S E.)

(let ((rsb (make-rthm-seq-bar '((2 2) (e) (e) (e) (e) (e) (s) (s) (s) e.))))
  (consolidate-rests-max rsb)
  (loop for r in (rhythms rsb) collect (data r)))

=> (2 Q S E.)

|#
;;; SYNOPSIS
(defmethod consolidate-rests-max ((rsb rthm-seq-bar) &key beat min warn)
;;; ****
  (loop with old-rthms with new-rthms with done = nil
     until done do
       (consolidate-rests rsb :beat beat :min min :warn warn)
       (setf new-rthms (get-rhythm-symbols rsb))
       (unless (setf done (equalp old-rthms new-rthms))
         (setf old-rthms new-rthms)))
  rsb)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Thu Aug 13 10:56:20 2015 -- we need an approach for complex/nested
;;; tuplets here also, as we can't split into beats there.

;;; ****m* rthm-seq-bar/consolidate-rests
;;; DESCRIPTION
;;; Consolidate two or more consecutive rests into one longer rhythmic
;;; unit. This method works on the basis of beats, striving to consolidate into
;;; beats first.
;;;
;;; NB: The user may find it helpful to adjust the :beat and :min values, and
;;;     even to call the method more than once consecutively. For multiple
;;;     calls, the method consolidate-rests-max may also be useful.
;;;
;;; ARGUMENTS
;;; - A rthm-seq-bar object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments
;;; - :beat. The beat basis into which rests are to be consolidated. If no
;;;   value is given for this option, the method will take the beat from the
;;;   time signature. 
;;; - :min. A seldom-used argument that will only make a difference when there
;;;   are a number of rests of the same duration followed by a note.  This is
;;;   then the minimum duration that such rests may have if they are to be
;;;   consolidated. Default = NIL.
;;; - :warn. T or NIL to indicate whether the method should print a warning to
;;;   the Lisp listener if it is mathematically unable to consolidate the
;;;   rests. T = print warning. Default = NIL.
;;; - :auto-tuplets. Whether to force a call to the auto-tuplets method before
;;;   returning. This will happen by default anyway if check-tuplets fails, but
;;;   sometimes that doesn't fail, exactly, but auto-tuplets still makes
;;;   sense. If, for example, Lilypond fails with strange error messages,
;;;   setting this to T might help. Default = NIL. 
;;; 
;;; RETURN VALUE
;;; The rthm-seq-bar object
;;; 
;;; EXAMPLE
#|
;;; Returns a list of rhythm/event objects 
(let ((rsb (make-rthm-seq-bar '((4 4) (e) (e) (e) (e) (e) (s) (s) (s) e.))))
  (consolidate-rests rsb))

=>
(
EVENT: start-time: NIL, end-time: NIL, 
[...]
data: 4
[...]
EVENT: start-time: NIL, end-time: NIL, 
[...]
data: 4
[...]
EVENT: start-time: NIL, end-time: NIL, 
[...]
data: 4
[...]
RHYTHM: value: 16.000, duration: 0.250, rq: 1/4, is-rest: T, 
[...]
data: S
[...]
RHYTHM: value: 5.333, duration: 0.750, rq: 3/4, is-rest: NIL, 
[...]
data: E.
)

;;; Consolidating on the basis of the time-signature's beat by default
(let ((rsb (make-rthm-seq-bar '((4 4) (e) (e) (e) (e) (e) (s) (s) (s) e.))))
  (consolidate-rests rsb)
  (loop for r in (rhythms rsb) collect (data r)))

=> (4 4 4 S E.)

;; Changing the :beat may effect the outcome
(let ((rsb (make-rthm-seq-bar '((4 4) (e) (e) (e) (e) (e) (s) (s) (s) e.))))
  (consolidate-rests rsb :beat 2)
  (loop for r in (rhythms rsb) collect (data r)))

=> (2 E E S E.)

;; Calling multiple times may further consolidate the results
(let ((rsb (make-rthm-seq-bar '((2 2) (e) (e) (e) (e) (e) (s) (s) (s) e.))))
  (consolidate-rests rsb)
  (print (loop for r in (rhythms rsb) collect (data r)))
  (consolidate-rests rsb)
  (print (loop for r in (rhythms rsb) collect (data r))))

=>
(2 E E S E.) 
(2 Q S E.)

|#
;;; SYNOPSIS
(defmethod consolidate-rests ((rsb rthm-seq-bar)
                              &key beat min warn auto-tuplets)
;;; ****
  ;; (print 'consolidate-rests)
  ;; (print (length (rhythms rsb)))
  ;; MDE Mon Nov 26 20:18:12 2012 -- added 'silent to make sure we don't get
  ;; more than a beat's worth of rthms 
  (let* ((beats (get-beats rsb beat 'silent))
         (cbeats '())
         ;; 21.7.11
         (rest-beat (make-rest (if beat beat (get-beat-as-rhythm rsb))))
         (rest-dur nil)
         (count 1)
         (last-rhythm nil)
         ;; MDE Sat Jun 28 14:36:29 2014 
         (e1 (get-nth-event 0 rsb))
         (player (when (event-p e1) (player e1)))
         ;; MDE Tue Mar 13 11:22:18 2012 
         (first-rhythm nil)
         (current '()))
    (setf min (if min 
                  (duration (make-rhythm min))
                  0.0))
    ;; (format t "~%bar ~a ~a" (bar-num rsb) (length (first beats)))
    ;; MDE Mon Aug 26 17:57:30 2013 -- if we can't split the bar into beats, we
    ;; might be able to split into durations of twice the beat 
    (unless beats
      (setf beat (scale (get-beat-as-rhythm rsb) 2)
            rest-beat (make-rest (clone beat))
            beats (get-beats rsb beat)))
    (if (all-rests? rsb)
        (force-rest-bar rsb)
        (flet ((consolidate (rthm count)
                 ;; (print rthm)
                 ;; (print count)
                 (if (< (duration rthm) min)
                     (push (loop repeat count collect (clone rthm))
                           current)
                     (push (consolidate-rests-aux rthm count)
                           current))))
          ;; (format t "~%~a beats" (length beats))
          (loop for beat in beats do
             ;; (print (length beat))
               (if (rhythms-all-rests? beat)
                   ;; 21.7.11
                   ;; MDE Sun Sep  7 18:40:41 2014 -- can't assume that we've
                   ;; only got a single beat's worth of rests; could be we just
                   ;; saw a minim rest in 4/4 time  
                   ;; (setf current (list (clone rest-beat)))
                   (let ((num-beats (/ (sum-rhythms-duration beat)
                                       (duration rest-beat))))
                     (when (float-int-p num-beats)
                       (setf num-beats (round num-beats))
                       (when (>= num-beats 1)
                         (setf current (consolidate-rests-aux
                                        rest-beat num-beats)))))
                   (loop for r in beat do
                      ;; (print (data r))
                        (if (is-rest r)
                            (if rest-dur
                                (if (= rest-dur (duration r))
                                    (incf count)
                                    (progn
                                      ;; MDE Tue Mar 13 11:24:53 2012 -- don't
                                      ;; use last-rhythm as that won't have a
                                      ;; bracket, if there is one. same for the 
                                      ;; two other calls below
                                      (consolidate first-rhythm count)
                                      (setf count 1
                                            ;; MDE Tue May  1 18:06:16 2012 !!!
                                            first-rhythm r
                                            rest-dur (duration r))))
                                ;; MDE Tue Mar 13 11:24:00 2012 -- need the
                                ;; first rhythm for the bracket, if there is
                                ;; one
                                (setf first-rhythm r
                                      rest-dur (duration r)))
                            (progn ;; not a rest!
                              (when (and last-rhythm (is-rest last-rhythm))
                                ;;(format t "~&no rest: ~a ~a ~a"
                                ;;      (value last-rhythm)
                                ;;    (value first-rhythm)
                                ;;  count)
                                ;; MDE Tue Mar 13 11:25:26 2012 
                                (consolidate first-rhythm count))
                              (setf rest-dur nil
                                    count 1)
                              (push r current)))
                        (setf last-rhythm r)))
               (when (and last-rhythm (is-rest last-rhythm))
                 ;; MDE Tue Mar 13 11:25:32 2012 
                 (consolidate first-rhythm count))
               (push (reverse current) cbeats)
             ;; (print cbeats)
             ;; (print current)
               (setf current nil
                     last-rhythm nil
                     rest-dur nil
                     count 1))
          (setf cbeats (flatten (reverse cbeats)))
          ;; (print (loop for r in cbeats collect (data r)))
          ;; (format t "~%cbeats dur ~a " (sum-rhythms-duration cbeats))
          (if (equal-within-tolerance (rhythms-duration rsb)
                                      (sum-rhythms-duration cbeats)
                                      .000004)
              ;; 21.7.11 (Pula) don't fail if we can't do it, just do nothing
              ;; (print (setf (rhythms rsb) cbeats))
              (setf (rhythms rsb) cbeats)
              ;; MDE Tue Nov 27 17:34:14 2012 -- only warn when we could split
              ;; into beats 
              (when (and cbeats warn)
                ;; (print rsb)
                ;; (print-rhythms-rqs (rhythms rsb))
                ;; (print-rhythms-rqs cbeats)
                (warn "~a~%rthm-seq-bar::consolidate-rests: bar ~a~%~
                       Consolidated rthms sum (~a) != previous sum (~a)~
                       ~%cbeats = ~a"
                      (print-simple rsb) (bar-num rsb)
                      (sum-rhythms-duration cbeats)
                      (rhythms-duration rsb) cbeats)))))
    ;; MDE Mon May  7 17:45:59 2012
    (when (or auto-tuplets (not (check-tuplets rsb nil)))
      (auto-tuplets rsb))
    ;; MDE Sat Jun 28 14:36:43 2014
    (update-events-player rsb player)
    rsb))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* rthm-seq-bar/force-rest-bar
;;; 12.12.11 SAR: Added ROBODoc info
;;; DESCRIPTION
;;; Force all rhythms of a rthm-seq-bar object to be replaced by rest.
;;; 
;;; NB: This method changes the value of the RHYTHMS slot of the rthm-seq-bar
;;;     but not the value of the rthm-seq-bar DATA slot.
;;; 
;;; ARGUMENTS 
;;; - A rthm-seq-bar object.
;;; 
;;; RETURN VALUE  
;;; Returns a rthm-seq-bar object.
;;; 
;;; EXAMPLE
#|
(let ((rsb (make-rthm-seq-bar '((2 4) q e s s))))
  (force-rest-bar rsb))

=>
RTHM-SEQ-BAR: time-sig: 1 (2 4)
              time-sig-given: T
              bar-num: -1
              old-bar-nums: NIL
              write-bar-num: NIL
              start-time: -1.0
              start-time-qtrs: -1.0
              is-rest-bar: T
[...]
RHYTHM: value: 2.0, duration: 2.0, rq: 2, is-rest: T,
[...]
NAMED-OBJECT: id: NIL, tag: NIL, 
data: ((2 4) Q E S S)

(let ((rsb (make-rthm-seq-bar '((2 4) q e s s))))
  (force-rest-bar rsb)
  (print-simple rsb))

=>
(2 4): rest 2,
|#
;;; SYNOPSIS
(defmethod force-rest-bar ((rsb rthm-seq-bar))
;;; ****
  (let ((new (get-whole-bar-rest (get-time-sig rsb)))
        (first (first (rhythms rsb))))
    ;; 2.2.11 copy over some useful slots of the event class
    (when (event-p first)
      (setf (start-time new) (start-time first)
            (start-time-qtrs new) (start-time-qtrs first)
            ;; 20.6.11: some marks can only be attached to a note so don't copy
            ;; these over 
            (marks new) (remove-if #'mark-for-note-only (marks first))
            (marks-in-part new) (marks-in-part first)
            (midi-time-sig new) (midi-time-sig first)
            (midi-program-changes new) (midi-program-changes first)
            ;; can't setf nil...
            (slot-value new 'tempo-change) (tempo-change first)
            (display-tempo new) (display-tempo first)
            (bar-num new) (bar-num first)
            (marks-before new) (marks-before first)
            ;; MDE Mon May  5 18:20:19 2014
            (player new) (player first)
            ;; MDE Mon Jul 23 13:13:11 2012 
            (instrument-change new) (instrument-change first)))
    ;; 26.7.11 (Pula): don't copy over 8ve marks: could screw things up but
    ;; then the caller should be aware of this when deleting bars etc.
    (rm-marks new '(beg-8va beg-8vb end-8va end-8vb) nil)
    ;; now this bar
    (setf (rhythms rsb) (list new)
          (show-rest rsb) t
          ;; (score-tuplets rsb) nil
          (tuplets rsb) nil
          (beams rsb) nil)
    (gen-stats rsb)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod num-notes-tied-from ((rsb rthm-seq-bar))
  (loop for r in (rhythms rsb) count (is-tied-from r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SAR Sat May 19 14:17:40 EDT 2012: Added robodoc entry

;;; When a bunch of short notes are tied to each other, make one (or a few)
;;; notes of them.  If check-dur, make sure we get an exact beat's worth of
;;; rhythms (this will fail if we have a rhythm longer than a beat so use NIL
;;; in that case).  

;;; ****m* rthm-seq-bar/consolidate-notes
;;; DESCRIPTION
;;; Combine consecutive tied notes into one (or a few) notes of a longer
;;; rhythmic duration. 
;;;
;;; NB: This method is the core method that is called for rthm-seq objects or
;;;     slippery-chicken objects, at which point it takes ties (and perhaps
;;;     another couple of things) into consideration, after the tie slots
;;;     etc. have been updated. As such, though it will
;;;     work to a certain degree when called directly on a rthm-seq-bar object,
;;;     it should primarily be used when getting a rthm-seq-bar from within a 
;;;     rthm-seq object or slippery-chicken object.
;;; 
;;; ARGUMENTS
;;; - A rthm-seq-bar object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to indicate whether the method sure make sure that an exact
;;;   beat's worth of rhythms is handled. T = check durations. Default = NIL. 
;;; 
;;; RETURN VALUE
;;; The rthm-seq-bar object.
;;; 
;;; EXAMPLE
#|
;;; Create a slippery-chicken object, print-simple a bar from that object,
;;; apply the consolidate-notes method to that bar, and print-simple that bar
;;; again to see the changes.

(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((vn (violin :midi-channel 1))))
        :set-palette '((1 ((gs4 af4 bf4))))
        :set-map '((1 (1 1 1)))
        :rthm-seq-palette '((1 ((((4 4) e +e +e +e e +s +s +s e.))
                                :pitch-seq-palette ((1 2 3)))))
        :rthm-seq-map '((1 ((vn (1 1 1))))))))
  (print-simple (get-bar mini 2 'vn))
  (consolidate-notes (get-bar mini 2 'vn))
  (print-simple (get-bar mini 2 'vn)))

=>
(4 4): GS4 E+, +GS4 E+, +GS4 E+, +GS4 E, AF4 E+, +AF4 S+, +AF4 S+, +AF4 S,
BF4 E.,
(4 4): GS4 H, AF4 Q+, +AF4 S, BF4 E.,

|#
;;; SYNOPSIS
(defmethod consolidate-notes ((rsb rthm-seq-bar)
                              ;; MDE Wed Nov 28 11:40:59 2012 -- added auto-beam
                              &optional check-dur beat (auto-beam t))
;;; ****
  ;; MDE Thu Apr 26 16:26:05 2012 -- tie-over-rests will pass its auto-beam
  ;; arg as our beat arg--this may be a rthm symbol or simply T.  the latter is
  ;; no good to us when used below.
  (when (eq beat t)
    (setf beat nil))
  (let* ((e1 (get-nth-event 0 rsb))
         (player (when (event-p e1) (player e1))))
    (unless (is-rest-bar rsb)
      ;; 11/4/07 only do this if we've got some tied notes in the bar
      ;; (unless (< (num-notes-tied-from rsb) 2)
      (unless (zerop (num-notes-tied-from rsb))
        (let ((beats (get-beats rsb beat check-dur))
              tmp rthms)
          (when beats
            ;; (error "~a~%rthm-seq-bar::consolidate-notes: no rhythms!" rsb))
            (setf rthms (loop for beat in beats
                           ;; aux will return nil if there are no ties so
                           ;; append the unprocessed beat in that case
                           for cns = (consolidate-notes-aux beat (bar-num rsb))
                           ;; 14.2.11
                           appending (if cns cns beat)))
            ;; now consolidate beats
            ;; 14.2.11 surely it's rthms, not (rhythms rsb) we want to
            ;; process here 
            ;; 14.2.11 don't try and consolidate anything beyond a beat if there
            ;; are tuplets.
            (unless (has-tuplets rthms)
              (setf tmp (consolidate-notes-aux rthms 
                                               (bar-num rsb)
                                               (get-beat-as-rhythm rsb)))
              (when tmp (setf rthms tmp)))
            ;; MDE Wed Nov 28 16:46:12 2012 -- only if we've done
            ;; some consolidation 
            (unless (= (length rthms) (num-rhythms rsb))
              (setf (rhythms rsb) 
                    (if (and (event-p (first (rhythms rsb))))
                        (consolidated-rthms-to-events rsb rthms)
                        rthms))))))
      (ties-to-dots rsb check-dur beat)
      ;; now try and get dots that go over two beats e.g. q. e in 2/4
      (ties-to-dots rsb check-dur (scale 
                                   (if beat
                                       (make-rhythm beat)
                                       (get-beat-as-rhythm rsb))
                                   2))
      (fix-brackets rsb)
      ;; MDE Mon May  7 17:43:59 2012 -- check and retry if the above fails
      (unless (check-tuplets rsb nil)
        (auto-tuplets rsb))
      ;; MDE Mon May  7 18:06:30 2012 -- beaming info may also be askew...
      ;; setting 3rd arg to nil means we don't get an error if we have a
      ;; multi-beat note e.g. h in 4/4 
      ;; MDE Wed Nov 28 11:38:18 2012 -- only if requested
      (when auto-beam
        (auto-beam rsb beat nil))
      ;; MDE Sat Jun 28 14:23:37 2014 
      (update-events-player rsb player)
      (gen-stats rsb))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod ties-to-dots ((rsb rthm-seq-bar) &optional check-dur beat)
  ;; (print 'ttd)
  (unless (or (is-rest-bar rsb)
              (< (num-notes-tied-from rsb) 1))
    (setf (rhythms rsb)
          (loop for beat in (get-beats rsb beat check-dur) appending
               (if (= 1 (length beat))
                   beat
                   (loop 
                      with b = '()
                      with skip = nil
                      for r1 in beat and r2 in (cdr beat) do
                      ;; (print 'hello1) we've just absorbed r1 into a dot
                      ;; while it was r2 in the previous loop...
                      (if skip
                          (setf skip nil)
                          (if (and (zerop (num-dots r1))
                                   (zerop (num-dots r2))
                                   (is-tied-from r1)
                                   (is-tied-to r2)
                                   ;; MDE Wed Nov 28 15:56:23 2012 -- don't
                                   ;; slurp up a note if it has a mark on it
                                   (not (marks r2))
                                   (or 
                                    ;; could be e+s or s+e
                                    (equal-within-tolerance
                                     (duration r1) (* 2.0 (duration r2)))
                                    (equal-within-tolerance (duration r2)
                                                            (* 2.0 
                                                               (duration r1)))))
                              (let ((new ;;(if (> (duration r1) (duration r2))
                                        (clone r1)))
                                ;; (print 'here)
                                (incf (rq new) (rq r2))
                                (incf (duration new) (duration r2))
                                (incf (compound-duration new) 
                                      (compound-duration r2))
                                (incf (num-dots new))
                                ;; MDE Mon May  7 20:42:52 2012 -- for LP we've
                                ;; got to have the right undotted value so
                                ;; update value and data while we're at it 
                                (let* ((letter (get-rhythm-letter-for-duration
                                                (duration new)))
                                       (lr (make-rhythm letter)))
                                  (setf (undotted-value new) (undotted-value lr)
                                        (value new) (value lr)
                                        (data new) letter))
                                (unless (is-tied-from r2)
                                  (setf (is-tied-from new) nil))
                                (setf (num-flags new) 
                                      (rthm-num-flags
                                       (/ 4 (* (duration new) 2/3))))
                                ;; beam and marks of r2 will be lost
                                (when (and (event-p r1)
                                           (event-p r2))
                                  (setf (end-time new) (end-time r2))
                                  (incf (duration-in-tempo new) 
                                        (duration-in-tempo r2))
                                  (incf (compound-duration-in-tempo new) 
                                        (compound-duration-in-tempo r2)))
                                (push new b)
                                (setf skip t))
                              ;; else....
                              (push r1 b)))
                      finally 
                      ;; got to get the last rthm in the bar
                      (unless skip
                        (push r2 b))
                      (return (nreverse b)))))))
  rsb)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; this will only work with simple tuplets i.e. not nested
(defmethod fix-brackets ((rsb rthm-seq-bar))
  ;; (print 'fix-brackets)
  (loop with current with dur = 0.0
     for r in (rhythms rsb) 
     for br = (first (bracket r))
     do
     (incf dur (duration r))
     (cond ((and br (listp br))         ; start tuplet
            (setf current (first br)
                  dur (duration r)))
           ((and current (integer>0 br))
            (setf current nil))
           ;; so we ignore existing brackets e.g. (-1)
           (current
            (setf (bracket r) (if (float-int-p dur)
                                  ;; we're in a tuplet and this duration
                                  ;; completes a beat  
                                  (prog1
                                    (list current)
                                    (setf current nil))
                                  ;; we're under a tuplet
                                  (list (- current)))))
           ((integerp (tuplet-scaler r))
            (setf (bracket r) nil)))))
       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Sat Jun  9 13:45:31 2012 -- will only auto-beam if T and beams are
;;; wrong.  Returns t if all ok otherwise nil.  Second returned value is the
;;; problem as a symbol

;;; SAR Mon Jul  2 16:30:47 BST 2012: Added robodoc entry

;;; ****m* rthm-seq-bar/check-beams
;;; DESCRIPTION
;;; Check the BEAM slots of the event objects within a specified rthm-seq-bar
;;; object to ensure that every beginning beam indication (slot value of 1) is
;;; coupled with a corresponding closing beam indication (slot value of 0), and
;;; print a warning and return NIL if this is not the case.
;;; 
;;; ARGUMENTS
;;; - A rthm-seq-bar object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :auto-beam. T or NIL to indicate the method should apply the auto-beam
;;;   algorithm to the given bar after the check has determined that the 
;;;   beaming is wrong (and :on-fail is not NIL). T = auto-beam. Default = NIL. 
;;; - :print. T or NIL to indicate whether the method should print feedback of
;;;   the checking process to the Lisp listener. T = print feedback. Default =
;;;   NIL.
;;; - :fix. T or NIL to indicate that when a beam has been placed over a rhythm
;;;    with no flags (e.g. a 1/4 note), then we delete beams over that note and
;;;    try again. Default = T.
;;; - :on-fail. The function that should be applied when the check does not
;;;   pass. May be NIL for no warning/error or #'error if processing should
;;;   stop. Default = #'warn.  
;;; 
;;; RETURN VALUE
;;; T if the check passes, otherwise NIL.
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
  (check-beams (get-bar mini 1 'sax)))

=> T

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
  (setf (beam (nth 1 (rhythms (get-bar mini 1 'sax)))) 1)
  (check-beams (get-bar mini 1 'sax)))

=> NIL

|#
;;; SYNOPSIS
(defmethod check-beams ((rsb rthm-seq-bar) &key auto-beam print (fix t)
                                             (on-fail #'warn))
;;; ****
  ;; (print (bar-num rsb))
  (let ((bad nil)
        (de-beam nil))
    (loop with last-seen = -1 with open
       for r in (rhythms rsb) 
       for current = (beam r)
       do
         (when (and current (= 1 current))
           (setf open t))
         (when (and current (= 1 last-seen) (= 1 current))
           (setf bad 'two-ones))
         (when (and current (zerop current) (not open))
           (setf bad 'not-open))
       ;; MDE Tue Aug 27 15:22:47 2013 -- don't beam over e.g. 1/4 rests
         (when (and open (zerop (num-flags r)) (not (is-grace-note r)))
           (if fix
               (progn
                 (setf de-beam r)
                 (return))
               (setf bad 'beam-over-non-beamed-rhythms)))
         (when bad
           (return bad))
         (when (and current (zerop current))
           (setf open nil))
         (when current 
           (setf last-seen current))
       finally (when open
                 (setf bad 'not-closed)))
    (if de-beam
        (de-beam rsb de-beam t)
        (progn
          (when (and bad auto-beam)
            ;; auto-beam calls check-beams too, via update-rhythms-beam-info
            (setf bad (not (auto-beam rsb nil nil))))
          (when print
            (print bad))
          (when (and on-fail bad)
            (apply on-fail
                   (list "rthm-seq-bar::check-beams failed with error ~a~%~a"
                         bad rsb)))
          (values (not bad) bad)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Sun Jun 28 12:26:56 2015 -- remove beams placed over a given rhythm
;;; (either a rhythm object or an index) 
(defmethod de-beam ((rsb rthm-seq-bar) rhythm &optional update)
  (let ((r (if (rhythm-p rhythm) rhythm (nth rhythm (rhythms rsb)))))
    (setf (beams rsb)
          (loop for beam in (beams rsb) unless
               (and (>= (bar-pos r) (first beam))
                    (<= (bar-pos r) (second beam)))
             collect beam))
    (when update (update-rhythms-beam-info rsb t))
    rsb))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;;; MDE Fri Apr 19 08:56:49 2013 -- cmn can't handle beams starting/ending on
;;; rests (but Lilypond can). 
(defmethod beams-on-rests? ((rsb rthm-seq-bar))
  (loop for r in (rhythms rsb) do
       (when (and (is-rest r)
                  (beam r))
         (return t))
       finally (return nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod consolidated-rthms-to-events ((rsb rthm-seq-bar) rthms)
  (loop 
     with i = 0
     with new-e
     with current-e = (get-nth-non-rest-rhythm 0 rsb)
     ;; with ceb
     with saw-note = nil
     for r in rthms 
     do
     (when (needs-new-note r)
       (setf current-e (get-nth-attack i rsb)
             saw-note t)
       (incf i))
     ;; (print (bracket current-e))
     ;; (print (bracket r))
     (unless current-e
       (error "rthm-seq-bar::consolidated-rthms-to-events: ~
                  current-e is nil (with i = ~a) in ~a" i rsb))
     (setf new-e (clone-with-new-class r 'event)
           ;; ceb (bracket current-e)
           )
     ;; 5.3.11 this is definitely wrong in some cases
     ;; (bracket new-e) (bracket current-e))
     (when (and saw-note (not (bracket r))
                ;; don't copy the bracket over unless we're at the start or in
                ;; the middle of a bracket  
                ;;(or (and ceb (listp ceb))
                ;;  (and (numberp ceb) (< ceb 0))))
                (/= (tuplet-scaler r) 1))
       (setf (bracket new-e) (bracket current-e)))
     (unless (is-rest r)
       ;; MDE Sat Dec 24 16:22:06 2011 -- 
       (when (event-p current-e)
         ;; some slots, e.g. compound-duration will still be wrong but
         ;; update-slots will take care of that later 
         (copy-event-slots current-e new-e))
       ;; 6/6/07 don't need marks when this is tied to!
       ;; MDE Wed Nov 28 14:18:35 2012 -- note true! could be cresc end or
       ;; something 
       ;;(when (is-tied-to new-e)
        ;; (delete-marks new-e))
       )
     (when (and (needs-new-note new-e)
                ;; MDE Sat Dec 24 16:25:02 2011 -- otherwise we can't
                ;; consolidate a rthm-seq from a palette
                (event-p current-e)
                (not (start-time new-e)))
       (error "rthm-seq-bar::consolidated-rthms-to-events: ~
                  bar ~a no start-time! ~&current-e: ~&~a ~&new-e: ~&~a"
              (bar-num rsb) current-e new-e))
     collect new-e))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-beat-as-rhythm ((rsb rthm-seq-bar) &optional (handle-compound))
  (get-beat-as-rhythm (get-time-sig rsb) handle-compound))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; e.g. bracket-info = '(3 0 5) which means a triplet bracket starting at event
;; 0 and ending at event 5 (inclusive and counting rests). 

(defmethod add-tuplet-bracket ((rsb rthm-seq-bar) bracket-info
                               &optional (delete-all-tuplets-first nil))
  (when delete-all-tuplets-first
    (delete-tuplets rsb))
  (unless (and (listp bracket-info)
               (= 3 (length bracket-info)))
    (error "rthm-seq-bar::add-tuplet-bracket: ~
            bracket-info is a 3-element list indicating bracket number, ~
            starting note and ending note: ~a"
           bracket-info))
  (setf (tuplets rsb) (append (tuplets rsb) (list bracket-info)))
  (update-rhythms-bracket-info rsb)
  t)  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 12.12.11 SAR: Added ROBODoc info
;;; ****m* rthm-seq-bar/delete-beams
;;; DESCRIPTION
;;; Remove any beaming indications from the rthm-seq-bar object. 
;;;
;;; NB: This method changes the data for the rthm-seq-bar object's BEAMS slot
;;;     and the individual BEAM slots of the RHYTHMs contained within the
;;;     rthm-seq-bar's RHYTHMS slot. It does not change the value of the
;;;     rthm-seq-bar's DATA slot.
;;;
;;; NB: Neither the presence nor absence of beams are not reflected in the
;;;     output of the print-simple method.
;;; 
;;; ARGUMENTS 
;;; - A rthm-seq-bar object.
;;; 
;;; RETURN VALUE  
;;; Returns T.
;;; 
;;; EXAMPLE
#|
(let ((rsb (make-rthm-seq-bar '((2 4) - s s - s - s s s - s s))))
  (delete-beams rsb))

=> T

(let ((rbs (make-rthm-seq-bar '((2 4) - s s - s - s s s - s s))))
  (delete-beams rsb)
  (beams rsb))

=> NIL

(let ((rsb (make-rthm-seq-bar '((2 4) - s s - s - s s s - s s))))
  (delete-beams rsb)
  (loop for r in (rhythms rbs) collect (beam r)))

=> (NIL NIL NIL NIL NIL NIL NIL NIL)

(let ((rbs (make-rthm-seq-bar '((2 4) - s s - s - s s s - s s))))
  (delete-beams rsb)
  (print rsb))

=>
RTHM-SEQ-BAR: time-sig: 1 (2 4)
              time-sig-given: T
              bar-num: -1
              old-bar-nums: NIL
              write-bar-num: NIL
              start-time: -1.0
              start-time-qtrs: -1.0
              is-rest-bar: NIL
              multi-bar-rest: NIL
              show-rest: T
              notes-needed: 8
              tuplets: NIL
              nudge-factor: 0.35
              beams: NIL
[...]
NAMED-OBJECT: id: NIL, tag: NIL, 
data: ((2 4) - S S - S - S S S - S S)
|#
;;; SYNOPSIS
(defmethod delete-beams ((rsb rthm-seq-bar))
;;; ****
  (setf (beams rsb) nil)
  (loop for r in (rhythms rsb) do
        (delete-beam r))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 12.12.11 SAR: Added ROBODoc info
;;; ****m* rthm-seq-bar/delete-tuplets
;;; DESCRIPTION
;;; Removes all indications for tuplet brackets from a given rthm-seq-bar
;;; object. 
;;;
;;; NB: This method does not alter the tuplet rhythmic durations; it only
;;; removes the tuplet bracket from the score.
;;;
;;; ARGUMENTS 
;;; - A rthm-seq-bar.
;;; 
;;; RETURN VALUE  
;;; The rthm-seq-bar object
;;;
;;; EXAMPLE
#|
(let ((rsb (make-rthm-seq-bar '((2 4) { 3 te te te } q))))
  (tuplets rsb))

=> ((3 0 2))

(let ((rsb (make-rthm-seq-bar '((2 4) { 3 te te te } q))))
  (delete-tuplets rsb))

=> NIL

(let ((rsb (make-rthm-seq-bar '((2 4) { 3 te te te } q))))
  (delete-tuplets rsb)
  (tuplets rsb))

=> NIL

(let ((rsb (make-rthm-seq-bar '((2 4) { 3 te te te } q))))
  (loop for r in (rhythms rsb) collect (bracket r)))

=> (((1 3)) (-1) (1) NIL)

(let ((rsb (make-rthm-seq-bar '((2 4) { 3 te te te } q))))
  (delete-tuplets rsb)
  (loop for r in (rhythms rsb) collect (bracket r)))

=> (NIL NIL NIL NIL)
|#
;;; SYNOPSIS
(defmethod delete-tuplets ((rsb rthm-seq-bar))
;;; ****
  (setf (tuplets rsb) nil)
  (loop for r in (rhythms rsb) do
       (delete-tuplet-bracket r))
  rsb)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sun May 20 15:51:44 EDT 2012: Added robodoc entry

;;; MDE Mon May  7 16:08:29 2012 -- 
;;; ****m* rthm-seq-bar/check-tuplets

;;; DESCRIPTION
;;; Check the qualities of the tuplets brackets in a given rthm-seq-bar
;;; object to make sure they are all formatted properly (i.e. each starting
;;; tuplet bracket has a closing tuplet bracket etc.). If an error is found,
;;; the method will try to fix it, then re-check, and only issue an error then
;;; if another is found.
;;;
;;; NB this won't check whether tuplet brackets start and stop on notes
;;; spanning a whole beat (as that wouldn't make sense with nested tuplets) so
;;; it won't guarantee that a bar can be notated without errors.
;;; 
;;; ARGUMENTS
;;; - A rthm-seq-bar object.
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
(let ((rsb (make-rthm-seq-bar '((4 4) { 3 te te te } q q q))))
  (setf (bracket (get-nth-event 2 rsb)) nil)
  (check-tuplets rsb #'warn))

=> rthm-seq-bar::check-tuplets: got a nil bracket when brackets still open.

|#
;;; SYNOPSIS
(defmethod check-tuplets ((rsb rthm-seq-bar) &optional (on-fail #'error))
;;; ****
  (let ((open '())
        (result t))
    (flet ((damn (msg)
             (setq result nil)
             (when on-fail
               (funcall on-fail "~&rthm-seq-bar::check-tuplets: ~a:~%~a" 
                        msg rsb))))
      ;; first check the tuplets slot of the rsb
      (loop with max = (1- (num-rhythms rsb))
         for tuplet in (tuplets rsb)
         for st = (second tuplet)
         for nd = (third tuplet) do
           (when (or (< st 0) (< nd 0) (> st max) (> nd max))
             (damn "tuplets slot contains out-of-bounds indices")))
      ;; now if the above loop didn't fail, check individual rthms
      (when result 
        (loop for r in (rhythms rsb) do
             (if (bracket r)
                 (loop for b in (bracket r) do
                      (cond ((listp b) (push (first b) open))
                            ((integer>0 b) 
                             (if (member b open)
                                 (setf open (remove b open))
                                 (damn "Can't close non-existent bracket.")))
                            ((integer<0 b) 
                             (unless (member (abs b) open)
                               (damn "Note under a non-existent bracket.")))
                            (t (damn "Bad bracket."))))
                 (when open
                   (damn "got a nil bracket when brackets still open."))))
        ;; MDE Wed Jul  4 13:42:51 2012
        (when open
          (damn "bracket still open at end of bar."))
        result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Mon May 7 16:38:23 2012 -- this is a better version of
;;; figure-out-and-auto-set-tuplets. Way back when I wrote that method we
;;; didn't have rhythm's tuplet-scaler slot. Now that's there things should be
;;; easier. This still won't handle all tuplet possibilities, especially nested
;;; tuplets, but should still be useful.

;;; ****m* rthm-seq-bar/auto-tuplets
;;; DESCRIPTION
;;; Automatically place the data necessary for tuplet brackets in rthm-seq-bar
;;; objects that contain tuplet rhythms.
;;; 
;;; ARGUMENTS
;;; - A rthm-seq-bar object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - A function to be performed on fail. Default = #'error.
;;; 
;;; RETURN VALUE
;;; Returns T if successful. 
;;; 
;;; EXAMPLE
#|
;;; Make a rthm-seq-bar object and print the values of the BRACKET slots for
;;; the rhythm objects it contains. Then apply auto-brackets and print the same
;;; again to see the change.
(let ((rsb (make-rthm-seq-bar '((4 4) tq tq tq +q fs fs fs fs fs))))
  (print (loop for r in (rhythms rsb) collect (bracket r)))
  (auto-tuplets rsb)
  (print (loop for r in (rhythms rsb) collect (bracket r))))

=>
(NIL NIL NIL NIL NIL NIL NIL NIL NIL) 
(((1 3)) (-1) (1) NIL ((2 5)) (-2) (-2) (-2) (2)) 

|#
;;; SYNOPSIS
(defmethod auto-tuplets ((rsb rthm-seq-bar) &optional (on-fail #'error))
;;; ****
  (delete-tuplets rsb)
  (loop with bag with tuplet with start with dur = 0.0
     for r in (rhythms rsb) 
     for count from 0
     do
       (unless bag
         (setf start count))
       (push r bag)
       (incf dur (duration r))
       ;; (print dur)
     ;; (print r)
       (when (or (float-int-p dur 0.00001)
                 (float-int-p (* 2.0 dur) 0.00001)) ; i.e. 0.5
         ;; (print 'here)
         (when (and (/= 1 (tuplet-scaler (first bag)))
                    (/= 1 (tuplet-scaler (first (last bag)))))
           (setf tuplet (denominator (tuplet-scaler (first (last bag)))))
           (add-tuplet-bracket rsb (list tuplet start count))
           (setf tuplet nil))
         (setf bag nil)))
  ;; (print rsb)
  (check-tuplets rsb on-fail))
         
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod figure-out-and-auto-set-tuplets ((rsb rthm-seq-bar) beat)
  (let ((beats (get-beats rsb beat))
        (tuplet nil)
        (tuplets '())
        (notes-this-beat 0)
        (count 0))
    (loop for beat in beats do
          (setf notes-this-beat (length beat))
          (when (> notes-this-beat 1)
            (setf tuplet (denominator (rq (first beat))))
            ;; (print tuplet)
            (loop for r in beat do (dot-for-triplet? r tuplet))
            (unless (power-of-2 tuplet)
                    (push (list tuplet count (1- (+ count notes-this-beat)))
                          tuplets)))
          (incf count notes-this-beat))
    (setf (tuplets rsb) (reverse tuplets))
    (update-rhythms-bracket-info rsb)
    t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 12.12.11 SAR: Added ROBODoc info

;;; ****m* rthm-seq-bar/auto-put-tuplet-bracket-on-beats
;;; DESCRIPTION
;;; Given a rthm-seq-bar object with tuplet rhythms and an indication of which
;;; tuplet value to place, this method will automatically add the appropriate
;;; tuplet bracket to the beats of the bar in the printed score output. If
;;; the TUPLET argument is set to NIL, the method will proceed on the basis of
;;; best-guessing rules. 
;;;
;;; NB: This method may produce results that encapsulate an entire beat when
;;;     applying brackets to a portion of that beat. Thus bracketing the rhythm
;;;     (e ts ts ts) will return
;;;     { 3 e. ts ts ts } rather than 
;;;     ( e { 3 ts ts ts } )
;;; 
;;; ARGUMENTS
;;; - A rthm-seq-bar object
;;; - An integer indicating the tuplet value (e.g. 3 for triplets, 5 for
;;;   quintuplets etc.)
;;; 
;;; RETURN VALUE  
;;; Returns T.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - An integer indicating beat basis for the bar, or NIL. If NIL (default),
;;;   the beat is taken from the time signature.
;;; - An integer indicating the beat number within the bar to look for
;;;   tuplets, or T. If  T (default), all beats in the bar will be examined for
;;;   possible tuplets. 
;;; - T or NIL to indicate whether to delete the tuplet bracket indicators
;;;   already present in the given rthm-seq-bar object. T = delete. 
;;;   Default = T. 
;;;
;;; EXAMPLE
#|
(let ((rsb (make-rthm-seq-bar '((2 4) te te te q))))
  (tuplets rsb))

=> NIL

(let ((rsb (make-rthm-seq-bar '((2 4) te te te q))))
  (loop for r in (rhythms rsb) collect (bracket r))

=> (NIL NIL NIL NIL)

(let ((rsb (make-rthm-seq-bar '((2 4) te te te q))))
  (auto-put-tuplet-bracket-on-beats rsb 3))

=> T

(let ((rsb (make-rthm-seq-bar '((2 4) te te te q))))
  (auto-put-tuplet-bracket-on-beats rsb 3)
  (print (tuplets rsb))
  (print (loop for r in (rhythms rsb) collect (bracket r))))

=>
((3 0 2)) 
(((1 3)) (-1) (1) NIL)

(let ((rsb (make-rthm-seq-bar '((2 4) te te te q))))
  (auto-put-tuplet-bracket-on-beats rsb nil)
  (tuplets rsb))

=> ((3 0 2))

;;; The method may bracket the entire beat, returning ((3 1 4)) rather than 
;;; ((3 2 4))
(let ((rsb (make-rthm-seq-bar '((2 4) q e ts ts ts))))
  (auto-put-tuplet-bracket-on-beats rsb 3)
  (tuplets rsb))

=> ((3 1 4))

|#
;;; SYNOPSIS
(defmethod auto-put-tuplet-bracket-on-beats ((rsb rthm-seq-bar) tuplet 
                                             &optional 
                                             (beat nil)
                                             ;; can be a beat number or t for
                                             ;; all
                                             (beat-number t)
                                             ;; delete the tuplets already
                                             ;; there?  
                                             (delete t))
;;; **** 
  (when delete 
    (delete-tuplets rsb))
  (if (not tuplet)
      ;; (figure-out-and-auto-set-tuplets rsb beat)
      (auto-tuplets rsb)
      (let ((beats (get-beats rsb beat))
            (do-all (eq beat-number t))
            (tuplets '())
            (notes-this-beat 0)
            (notes 0))
        (loop for b in beats and i from 0 do
             (setf notes-this-beat (length b))
             (when (> notes-this-beat 1)
               (loop for r in b do (dot-for-triplet? r tuplet))
               (when (or do-all (= i beat-number))
                 (push (list tuplet notes (1- (+ notes-this-beat notes)))
                       tuplets)))
             (incf notes notes-this-beat))
        (setf tuplets (reverse tuplets) 
              (tuplets rsb) tuplets) #|(if delete ;; 22.5.11
        tuplets
        (append (tuplets rsb) tuplets))) |#
      (update-rhythms-bracket-info rsb)
      t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* rthm-seq-bar/auto-beam
;;; DESCRIPTION
;;; Automatically add beaming indications to the rhythm objects of the given
;;; rthm-seq-bar object.  This will only set one beam group per beat.
;;;
;;; NB: This method does not modify the data slot of the rthm-seq-bar object
;;;     itself. Instead, it modifies the beam value for the individual rhythms.
;;; 
;;; ARGUMENTS 
;;; - A rthm-seq-bar object.
;;;
;;; OPTIONAL ARGUMENTS
;;; - The beat basis for the given rthm-seq-bar. This will affect which notes
;;;   get beamed together. This value can be either numeric (4, 8 16 etc.) or
;;;   alphabetic (q, e, s etc). If no beat is given, the method defaults this
;;;   value to NIL and takes the beat from the current time signature.
;;; - Check-dur. This can be t, nil, #'warn or #'error, where t is the same as
;;;   #'error. If T, the method will make sure there is a complete beat of
;;;   rhythms for each beat of the bar and issue an error if a full beat's
;;;   worth cannot be returned--this may not mean that your bar is
;;;   malformed. Or if you pass a symbol like 'silent the 
;;;   duration will be checked and NIL returned if we can't get an exact beat's
;;;   worth of rthms. Default = T.
;;; 
;;; RETURN VALUE  
;;; Returns the rthm-seq-bar-object
;;; 
;;; EXAMPLE
#|

(let ((rsb (make-rthm-seq-bar '((2 4) e e s s s s))))
  (auto-beam rsb))

=> NIL

(let ((rsb (make-rthm-seq-bar '((2 4) e e s s s s))))
  (auto-beam rsb)
  (loop for r in (rhythms rsb) collect (beam r)))

=> (1 0 1 NIL NIL 0)

(let ((rsb (make-rthm-seq-bar '((2 4) e e s s s s))))
  (auto-beam rsb 8)
  (loop for r in (rhythms rsb) collect (beam r)))

=> (NIL NIL 1 0 1 0)

(let ((rsb (make-rthm-seq-bar '((2 4) e e s s s s))))
  (auto-beam rsb 8 t)
  (loop for r in (rhythms rsb) collect (beam r)))

=> (NIL NIL 1 0 1 0)

(let ((rsb (make-rthm-seq-bar '((2 4) e e s s s s))))
  (auto-beam rsb 8 nil)
  (loop for r in (rhythms rsb) collect (beam r)))

=> (NIL NIL 1 0 1 0)

  |#
;;; SYNOPSIS
(defmethod auto-beam ((rsb rthm-seq-bar) &optional (beat nil) (check-dur t))
;;; ****
  (unless (is-rest-bar rsb)
    (let ((beats (get-beats rsb beat check-dur))
          (start nil)
          (end nil)
          (result '())
          (flags nil)
          (is-note nil)
          (ok t)
          (note-num 0))
      ;; 5/4/07: first of all delete any prior beams
      ;; MDE Thu Nov 29 19:25:00 2012 -- now called here rather than in the
      ;; loop
      ;; MDE Thu Jun  4 15:24:37 2015 -- only proceed if we could split into
      ;; beats 
      (when beats
        (delete-beams rsb)
        (loop for b in beats do
             (setf start nil
                   end nil
                   ok t)
           ;; (print 'beat)
             (loop for r in b do
                  (setf is-note (not (is-rest r))
                        flags (and is-note
                                   (> (num-flags r) 0)))
                  (when (and ok flags (not start))
                    (setf start note-num))
                  (when (and ok start 
                             (or
                              ;; MDE Thu Nov 29 20:42:11 2012 -- no q rests
                              ;; under beam for LP
                              (and (is-rest r) (zerop (num-flags r)))
                              (and is-note (not flags))))
                    ;; MDE Thu Feb 16 11:19:33 2017 -- don't just give up if we
                    ;; hit a 1/4+ note/rest, rather beam up until that point by
                    ;; simply exiting out of the loop.
                    #|(setf start nil
                            end nil
                            ok nil))|#
                    (return))
                  (when (and ok flags)
                    (setf end note-num))
                ;; (format t "~%~a: ~a ~a ~a flags ~a"
                ;;          (data r) start end ok flags)
                ;; MDE Tue May 29 23:14:16 2012 -- we can now have beams over
                ;; rests so no longer count notes but all events
                ;; (when is-note
                  (incf note-num))
             (when (and start end (/= start end))
               (push (list start end) result)))
        (setf (beams rsb) (reverse result))
        (update-rhythms-beam-info rsb))
      rsb)))
                
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-num-beats ((rsb rthm-seq-bar))
  (num-beats (get-time-sig rsb)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Fri Feb  3 16:21:31 2017
(defmethod get-events-under-tuplets ((rsb rthm-seq-bar))
  (loop for tgroup in (tuplets rsb) collect
       ;; first in group is the actual tuplet
       (subseq (rhythms rsb) (second tgroup) (1+ (third tgroup)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* rthm-seq-bar/get-beats
;;; DESCRIPTION
;;; Try to organise the events of a bar into sublists of a beat's worth of
;;; events. 
;;;
;;; If beat is nil, we'll get the beat from the time-sig. If check-dur is T
;;; we'll make sure we get a complete beat of rhythms for each beat of the bar
;;; Since May 1 2012, check-dur can be T, NIL, #'warn or #'error, where t is
;;; the same as #'error. If you pass a symbol like 'silent the duration will be
;;; checked and NIL returned if we can't get an exact beat's worth of rthms.
;;; ARGUMENTS
;;; - a rthm-seq-bar object
;;; 
;;; OPTIONAL ARGUMENTS
;;; - the beat, as a symbol or rhythm object (see above)
;;; - whether to check the duration of the beats to see if they're full (see
;;; above) 
;;; 
;;; RETURN VALUE
;;; A list of lists of event/rhythm objects
;;; 
;;; EXAMPLE
#|
NB the elements returned will actually be rhythm objects but I'll just show the
rhythm symbol for clarity:  
(get-beats (make-rthm-seq-bar '((4 4) s x 4 q e e s e.)))
-->
((s s s s) (q) (e e) (s e.))
|#
;;; SYNOPSIS
(defmethod get-beats ((rsb rthm-seq-bar) &optional beat check-dur)
;;; ****
  (let ((beat-dur (if (and beat (not (eq beat t)))
                      (duration (make-rhythm beat))
                      (beat-duration (get-time-sig rsb))))
        (current '())
        (failed nil)
        (beats '())
        (dur 0.0))
    ;; MDE Tue May  1 19:04:32 2012
    (if (eq check-dur t)
        (setf check-dur #'error))
    (loop for r in (rhythms rsb) do
         (push r current)
         (incf dur (duration r))
         (when (or (> dur beat-dur)
                   (equal-within-tolerance dur beat-dur .001))
           ;; (format t "~% dur ~a beat-dur ~a" dur beat-dur)
           (when check-dur
             (unless 
                 ;; MDE Tue Nov 27 17:44:10 2012 -- allow more than one beat
                 ;; to be collected
                 (or 
                  (equal-within-tolerance dur beat-dur .001)
                  (equal-within-tolerance (rem dur beat-dur) 0.0 .001))
               ;; MDE Tue May  1 18:58:14 2012 -- added function argument
               ;; possibility for check-dur 
               (when (functionp check-dur)
                 (funcall check-dur
                          "rthm-seq-bar::get-beats: ~
                           Can't find an exact beat of rhythms ~%~
                           (dur: ~a beat-dur: ~a)! ~%~a" 
                          dur beat-dur rsb))
               ;; MDE Mon Nov 26 20:14:29 2012 -- these were under the above
               ;; when 
               (setf failed t)
               (return)))
           (push (reverse current) beats)
           (setf dur 0.0
                 current nil)))
    ;; when a tuplet of some kind goes over a beat, then the last beat won't be
    ;; full: get it anyway!  
    (when current
      (push (reverse current) beats))
    (unless failed
      (nreverse beats))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod gen-stats ((rsb rthm-seq-bar))
  (let ((rhythms (rhythms rsb)))
    (set-sounding-duration rsb nil)
    ;; MDE Thu Apr 19 11:29:25 2012 -- just query the value and it'll be
    ;; calculated 
    (sounding-duration rsb)
    (setf (num-rhythms rsb) (length rhythms)
          (is-rest-bar rsb)
          (if (not rhythms)
              t
              (when (and (= 1 (num-rhythms rsb))
                         (is-rest (first rhythms)))
                (setf (is-whole-bar-rest (first (rhythms rsb)))
                      t)))
          ;; Store the number of notes that will be need for this bar,
          ;; i.e. how many were not rests or ties
          ;; MDE Thu Aug 22 18:15:37 2013 -- also as side-effect store the
          ;; rhythm number  
          (notes-needed rsb) (loop for r in rhythms and i from 0
                                ;; MDE Thu Aug 22 18:16:22 2013 
                                do (setf (bar-pos r) i)
                                count
                                (needs-new-note r))
          (num-rests rsb) (loop for r in rhythms count (is-rest r))
          (num-score-notes rsb) (- (num-rhythms rsb) (num-rests rsb))))
  rsb)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; We don't want to store the duration symbol (e.g. e.) rather it's duration
;;; in qtr notes.

(defmethod update-missing-duration ((rsb rthm-seq-bar))
  (let* ((md (missing-duration rsb))
         (rthm (parse-possibly-compound-rhythm md)))
    (setf (slot-value rsb 'missing-duration)
      (if (and rthm (> (duration rthm) 0))
          (rq rthm)
        nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod update-rhythms-beam-info ((rsb rthm-seq-bar) 
                                     &optional delete-all-beams-first)
  (let ((beams (beams rsb)))
    (when delete-all-beams-first
      (delete-beams rsb))
    ;; the beams slot contains 2-element sublists the elements being 0-based
    ;; indices of the notes that start and end a beam.
    (loop for data in beams do
       ;; MDE Tue May 29 22:40:31 2012 -- we now allow beams on rests
       ;; (start-beam (get-nth-non-rest-rhythm (first data) rsb))
         (start-beam (get-nth-event (first data) rsb))
       ;; (end-beam (get-nth-non-rest-rhythm (second data) rsb))
         (end-beam (get-nth-event (second data) rsb)))
    ;; MDE Sun Jun 28 12:34:58 2015
    (when delete-all-beams-first
      (setf (beams rsb) beams))
    ;; MDE Sat Jun  9 15:22:05 2012
    (check-beams rsb :on-fail #'error)
    ;; MDE Sat Jun  9 11:28:30 2012 -- return the bar object instead of nil
    rsb))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Fri Apr  7 10:21:59 2017 -- sometimes the tuplets slot of the rsb gets
;;; screwed up so recreate it from the rhythms 
(defmethod recreate-tuplets ((rsb rthm-seq-bar))
  (let ((tuplets (ml nil 6)))
    (loop for r in (rhythms rsb) and i from 0 do
         (when (bracket r)
           (loop for b in (bracket r) do
                (if (listp b)
                    (setf (nth (1- (first b)) tuplets)
                          (list (second b) i nil))
                    (when (integer>0 b)
                      (setf (third (nth (1- b) tuplets)) i))))))
    (setf (tuplets rsb) (remove nil tuplets))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Call this when the tuplets slot of the rsb has been altered: it will update
;;; the tuplets info of the rhythms accordingly.

(defmethod update-rhythms-bracket-info ((rsb rthm-seq-bar))
  ;; (print (tuplets rsb))
  (let ((rhythms (rhythms rsb)))
    (flet ((nth-rthm 
            (nth)
            (unless (integer>=0 nth)
              (error "~a~%rthm-seq-bar::update-rhythms-bracket-info: ~
                      nth = ~a" rsb nth))
            (let ((result (nth nth rhythms)))
              (unless result
                (error "~&rthm-seq-bar::update-rhythms-bracket-info: ~
                         Couldn't get rhythm with index ~a in bar ~a:~%~a"
                       nth (bar-num rsb) rhythms))
              result)))
      ;; the tuplets slot contains 3-element sublists, first is the number to
      ;; be placed in the bracket, the second is the index of the start-note,
      ;; the third is the index of the end-note.
      (loop 
          for data in (tuplets rsb) 
          for tuplet = 
            (progn 
              (unless (listp data)
                (error "rthm-seq-bar::update-rhythms-bracket-info: ~
                        tuplet data in bar ~a corrupt! : ~a"
                       (bar-num rsb) (tuplets rsb)))
              (first data))
          for start = (second data)
          for end = (third data)
                    ;; by starting i at 1, we waste one element in the list of
                    ;; cmn brackets but allow the (- i) trick to store the
                    ;; index into that list: (- 0) = 0 so wont' work
          and i from 1 do
            ;; we set the open bracket note's bracket slot to be a list of
            ;; two-element lists where the first element is the index into the
            ;; list of brackets created by cmn (i) and the second number is the
            ;; tuplet number itself.
            (add-tuplet-bracket (nth-rthm start) (list i tuplet))
            ;; set the rhythms in-between start and end to be under the current
            ;; bracket (negative index which is recognised in
            ;; cmn-tuplet-brackets (cmn.lsp) and turned positive again.
            (loop for j from (1+ start) below end do
                  (add-tuplet-bracket (nth-rthm j) (- i)))
            ;; the close bracket is indicated by the reference into the cmn
            ;; list to the opened bracket.
            (add-tuplet-bracket (nth-rthm end) i)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone ((rsb rthm-seq-bar))
  (clone-with-new-class rsb 'rthm-seq-bar))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone-with-new-class :around ((rsb rthm-seq-bar) new-class)
  (declare (ignore new-class))
  (let ((sclist (call-next-method)))
    (setf (slot-value sclist 'time-sig) (time-sig rsb)
          (slot-value sclist 'time-sig-given) (time-sig-given rsb)
          (slot-value sclist 'rhythms) (my-copy-list (rhythms rsb))
          (slot-value sclist 'num-rhythms) (num-rhythms rsb)
          (slot-value sclist 'num-rests) (num-rests rsb)
          (slot-value sclist 'start-time) (start-time rsb)
          (slot-value sclist 'start-time-qtrs) (start-time-qtrs rsb)
          (slot-value sclist 'num-score-notes) (num-score-notes rsb)
          (slot-value sclist 'missing-duration) (missing-duration rsb)
          (slot-value sclist 'is-rest-bar) (is-rest-bar rsb)
          (slot-value sclist 'multi-bar-rest) (multi-bar-rest rsb)
          (slot-value sclist 'show-rest) (show-rest rsb)
          (slot-value sclist 'rehearsal-letter) (rehearsal-letter rsb)
          (slot-value sclist 'notes-needed) (notes-needed rsb)
          ;;(slot-value sclist 'score-tuplets)(my-copy-list (score-tuplets rsb))
          (slot-value sclist 'tuplets) (my-copy-list (tuplets rsb))
          (slot-value sclist 'beams) (my-copy-list (beams rsb))
          (slot-value sclist 'bar-num) (bar-num rsb)
          (slot-value sclist 'old-bar-nums) (copy-list (old-bar-nums rsb))
          (slot-value sclist 'write-bar-num) (write-bar-num rsb)
          (slot-value sclist 'write-time-sig) (write-time-sig rsb)
          (slot-value sclist 'bar-line-type) (bar-line-type rsb)
          (slot-value sclist 'player-section-ref) (player-section-ref rsb)
          (slot-value sclist 'parent-start-end)
          (copy-list (parent-start-end rsb))
          (slot-value sclist 'multi-bar-rest) (multi-bar-rest rsb)
          (slot-value sclist 'nth-seq) (nth-seq rsb)
          (slot-value sclist 'rsp-id) (rsp-id rsb)
          (slot-value sclist 'sounding-duration) (sounding-duration rsb)
          (slot-value sclist 'nth-bar) (nth-bar rsb))
    sclist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Thu Apr 19 10:30:38 2012 -- redefine the accessor to calculate
;;; sounding-duration if it's nil or forced
(defmethod sounding-duration :before ((rsb rthm-seq-bar))
  (if (slot-value rsb 'sounding-duration)
      (slot-value rsb 'sounding-duration)
      (setf (slot-value rsb 'sounding-duration)
            (loop for e in (rhythms rsb) with dur = 0.0 do
                 (if (event-p e)
                     (unless (is-rest e)
                       (incf dur (duration-in-tempo e)))
                     ;; we hit a rhythm object so just back out
                     (return nil))
                 finally (return dur)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod set-sounding-duration ((rsb rthm-seq-bar) value)
  (setf (slot-value rsb 'sounding-duration) value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 12.12.11 SAR: Added ROBODoc info
;;; ****m* rthm-seq-bar/get-nth-non-rest-rhythm
;;; DESCRIPTION
;;; Get the nth non-rest rhythm object stored in the given rthm-seq-bar.  
;;; 
;;; ARGUMENTS
;;; - The zero-based index number indicating which non-rest-rhythm is sought.
;;; - The given rthm-seq-bar object in which to search.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - T or NIL indicating whether to print an error message if the given index 
;;;   is greater than the number of non-rest rhythms in the RHYTHMS list (minus 
;;;   one to compensate for the zero-based indexing). (Default = T).  
;;; 
;;; RETURN VALUE  
;;; A rhythm object.
;;;
;;; Returns NIL if the given index is higher than the highest possible index of
;;; non-rest rhythms in the given rthm-seq-bar object.
;;; 
;;; EXAMPLE
#|
;; The method returns a rhythm object when successful ; ;
(let ((rsb (make-rthm-seq-bar '((2 4) e (e) s s (s) s))))
(get-nth-non-rest-rhythm 0 rsb))

=> 
RHYTHM: value: 8.0, duration: 0.5, rq: 1/2, is-rest: NIL, score-rthm: 8.0, 
undotted-value: 8, num-flags: 1, num-dots: 0, is-tied-to: NIL, 
is-tied-from: NIL, compound-duration: 0.5, is-grace-note: NIL, 
needs-new-note: T, beam: NIL, bracket: NIL, rqq-note: NIL, 
rqq-info: NIL, marks: NIL, marks-in-part: NIL, letter-value: 8, 
tuplet-scaler: 1, grace-note-duration: 0.05,
LINKED-NAMED-OBJECT: previous: NIL
this: NIL
next: NIL
NAMED-OBJECT: id: E, tag: NIL, 
data: E

(let ((rsb (make-rthm-seq-bar '((2 4) e (e) s s (s) s))))
(data (get-nth-non-rest-rhythm 1 rsb)))

=> S

(let ((rsb (make-rthm-seq-bar '((2 4) e (e) s s (s) s))))
(data (get-nth-non-rest-rhythm 4 rsb)))

=>
Evaluation aborted on #<SIMPLE-ERROR>
rthm-seq-bar::get-nth-non-rest-rhythm: Couldn't get non-rest rhythm with index
4 for bar number -1 
[Condition of type SIMPLE-ERROR]

(let ((rsb (make-rthm-seq-bar '((2 4) e (e) s s (s) s))))
(get-nth-non-rest-rhythm 4 rsb nil))

=> NIL

|#
;;; SYNOPSIS
(defmethod get-nth-non-rest-rhythm (index (rsb rthm-seq-bar)
                                    &optional (error t))
;;; ****
  (let ((result
         (loop for r in (rhythms rsb) with i = -1 do
               (unless (is-rest r)
                 (incf i))
               (when (= i index)
                 (return r)))))
    (when error
      (unless result
        (error "~a~%rthm-seq-bar::get-nth-non-rest-rhythm: Couldn't get ~
                non-rest rhythm with index ~%~a for bar number ~a"
               (rhythms rsb) index (bar-num rsb))))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 12.12.11 SAR: Added ROBODoc info
;;; ****m* rthm-seq-bar/get-nth-rest
;;; DESCRIPTION
;;; Gets the rhythm object of the nth rest in a given rthm-seq-bar.
;;; 
;;; ARGUMENTS 
;;; - The zero-based index number indicating which rest is sought.
;;; - The given rthm-seq-bar object in which to search.
;;;
;;; OPTIONAL ARGUMENTS
;;; - T or NIL indicating whether to print an error message if the given index
;;;   is greater than the number of rests in the RHYTHMS list (minus one to 
;;;   compensate for the zero-based indexing) (default = T).    
;;; 
;;; RETURN VALUE  
;;; A rhythm object.
;;;
;;; Returns NIL if the given index is higher than the highest possible index of
;;; rests in the given rthm-seq-bar object.
;;; 
;;; EXAMPLE
#|
(let ((rsb (make-rthm-seq-bar '((3 4) e (e) s s (s) s (q)))))
(get-nth-rest 0 rsb))

=>
RHYTHM: value: 8.0, duration: 0.5, rq: 1/2, is-rest: T, score-rthm: 8.0, 
undotted-value: 8, num-flags: 1, num-dots: 0, is-tied-to: NIL, 
is-tied-from: NIL, compound-duration: 0.5, is-grace-note: NIL, 
needs-new-note: NIL, beam: NIL, bracket: NIL, rqq-note: NIL, 
rqq-info: NIL, marks: NIL, marks-in-part: NIL, letter-value: 8, 
tuplet-scaler: 1, grace-note-duration: 0.05,
LINKED-NAMED-OBJECT: previous: NIL
this: NIL
next: NIL
NAMED-OBJECT: id: E, tag: NIL, 
data: E

(let ((rsb (make-rthm-seq-bar '((3 4) e (e) s s (s) s (q)))))
(data (get-nth-rest 2 rsb)))

=> Q

(let ((rsb (make-rthm-seq-bar '((3 4) e (e) s s (s) s (q)))))
(get-nth-rest 3 rsb t))

Evaluation aborted on #<SIMPLE-ERROR>
rthm-seq-bar::get-nth-rest: Couldn't get rest with index 3
[Condition of type SIMPLE-ERROR]

(let ((rsb (make-rthm-seq-bar '((3 4) e (e) s s (s) s (q)))))
(get-nth-rest 3 rsb nil))

=> NIL

|#
;;; SYNOPSIS
(defmethod get-nth-rest (index (rsb rthm-seq-bar)
                         &optional (error t))
;;; ****
  (let ((result
         (loop for r in (rhythms rsb) with i = -1 do
               (when (is-rest r)
                 (incf i))
               (when (= i index)
                 (return r)))))
    (when error
      (unless result
        (error "~a rthm-seq-bar::get-nth-rest: Couldn't get rest with index ~a"
               (rhythms rsb) index)))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sun Dec 25 20:52:48 EST 2011 Added robodoc info
;;; ****m* rthm-seq-bar/get-nth-event
;;; DESCRIPTION
;;; Get the nth event (rhythm) in the given rthm-seq-bar object. This is a
;;; zero-based index.
;;;
;;; The method defaults to interrupting with an error if the n-value is greater
;;; than the number of items in the rthm-seq-bar. This can be disabled using
;;; the optional argument.
;;; 
;;; ARGUMENTS 
;;; - A rthm-seq-bar object.
;;; - An index number.
;;;
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to indicate whether to interrupt and drop into the debugger with 
;;;   an error. Default = T.
;;; 
;;; RETURN VALUE  
;;; A rhythm object when successful. 
;;;
;;; Returns NIL when the specified index number is greater than the number of
;;; events in the rthm-seq-bar object. Also prints an error in this case by
;;; default, which can be suppressed by setting the optional argument to NIL.
;;; 
;;; EXAMPLE
#|
;; Zero-based indexing. Returns a rhythm object when successful. ; ;
(let ((rsb (make-rthm-seq-bar '((2 4) q e s s))))
(get-nth-event 0 rsb))

=> 
RHYTHM: value: 4.000, duration: 1.000, rq: 1, is-rest: NIL, 
score-rthm: 4.0f0, undotted-value: 4, num-flags: 0, num-dots: 0, 
is-tied-to: NIL, is-tied-from: NIL, compound-duration: 1.000, 
is-grace-note: NIL, needs-new-note: T, beam: NIL, bracket: NIL, 
rqq-note: NIL, rqq-info: NIL, marks: NIL, marks-in-part: NIL, 
letter-value: 4, tuplet-scaler: 1, grace-note-duration: 0.05
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: Q, tag: NIL, 
data: Q

;; Interrupts with an error and drops into the debugger by default if the ; ;
;; specified index number is greater than the number of events in the ; ;
;; rthm-seq-bar.                        ; ;
(let ((rsb (make-rthm-seq-bar '((2 4) q e s s))))
(get-nth-event 4 rsb))

=>
rthm-seq-bar::get-nth-event: Couldn't get event with index 4
[Condition of type SIMPLE-ERROR]

;; The error can be suppressed by setting the optional argument to NIL ; ;
(let ((rsb (make-rthm-seq-bar '((2 4) q e s s))))
(get-nth-event 4 rsb nil))

=> NIL

|#
;;; SYNOPSIS
(defmethod get-nth-event (index (rsb rthm-seq-bar)
                          &optional (error t))
;;; ****
  (unless (integer>=0 index)
    (error "~a ~%rthm-seq-bar::get-nth-event: index = ~a!" rsb index))
  (let ((events (rhythms rsb)))
    (when error
      (unless (< index (num-rhythms rsb))
        (error "~a~%~a~%rthm-seq-bar::get-nth-event: Couldn't get event with ~
                index ~a"
               rsb events index)))
    (nth index events)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sun Dec 25 21:07:00 EST 2011 Added robodoc info
;;; ****m* rthm-seq-bar/get-last-event
;;; DESCRIPTION
;;; Get the last event object (or rhythm object) of a given rthm-seq-bar
;;; object. 
;;; 
;;; ARGUMENTS 
;;; - A rthm-seq-bar object.
;;; 
;;; RETURN VALUE  
;;; Returns a rhythm object.
;;; 
;;; EXAMPLE
#|
;; Returns a rhythm object.             ;
(let ((rsb (make-rthm-seq-bar '((2 4) s s e q))))
(get-last-event rsb))

=> 
RHYTHM: value: 4.000, duration: 1.000, rq: 1, is-rest: NIL, 
score-rthm: 4.0f0, undotted-value: 4, num-flags: 0, num-dots: 0, 
is-tied-to: NIL, is-tied-from: NIL, compound-duration: 1.000, 
is-grace-note: NIL, needs-new-note: T, beam: NIL, bracket: NIL, 
rqq-note: NIL, rqq-info: NIL, marks: NIL, marks-in-part: NIL, 
letter-value: 4, tuplet-scaler: 1, grace-note-duration: 0.05
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: Q, tag: NIL, 
data: Q

|#
;;; SYNOPSIS
(defmethod get-last-event ((rsb rthm-seq-bar))
;;; ****
  (get-nth-event (1- (num-rhythms rsb)) rsb))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 12.12.11 SAR: Added ROBODoc info
;;; ****m* rthm-seq-bar/get-nth-attack
;;; DESCRIPTION
;;; Gets the rhythm object for the nth note in a given rthm-seq-bar that needs
;;; an attack, i.e. not a rest and not tied. 
;;; 
;;; ARGUMENTS 
;;; - The zero-based index number indicating which attack is sought.
;;; - The given rthm-seq-bar object in which to search.
;;;
;;; OPTIONAL ARGUMENTS
;;; - T or NIL indicating whether to print a warning message if the given index
;;;   is greater than the number of attacks in the RHYTHMS list (minus one to 
;;;   compensate for the zero-based indexing) (default = T).   
;;; 
;;; RETURN VALUE  
;;; A rhythm object.
;;;
;;; Returns NIL if the given index is higher than the highest possible index of
;;; attacks in the given rthm-seq-bar object.
;;; 
;;; EXAMPLE
#|
;; The method returns a rhythm object when successful ; ;
(let ((rsb (make-rthm-seq-bar '((3 4) q+e (e) s (s) e))))
(get-nth-attack 0 rsb))

=> 
RHYTHM: value: 4.0, duration: 1.0, rq: 1, is-rest: NIL, score-rthm: 4.0, 
undotted-value: 4, num-flags: 0, num-dots: 0, is-tied-to: NIL, 
is-tied-from: NIL, compound-duration: 1.0, is-grace-note: NIL, 
needs-new-note: T, beam: NIL, bracket: NIL, rqq-note: NIL, 
rqq-info: NIL, marks: NIL, marks-in-part: NIL, letter-value: 4, 
tuplet-scaler: 1, grace-note-duration: 0.05,
LINKED-NAMED-OBJECT: previous: NIL
this: NIL
next: NIL
NAMED-OBJECT: id: "Q", tag: NIL, 
data: Q

(let ((rsb (make-rthm-seq-bar '((3 4) q+e (e) s (s) e))))
(data (get-nth-attack 1 rsb)))

=> S

(Let ((rsb (make-rthm-seq-bar '((3 4) q+e (e) s (s) e))))
(get-nth-attack 3 rsb))

=> NIL
WARNING: rthm-seq-bar::get-nth-attack:  index (3) < 0 or >= notes-needed (3)

(Let ((rsb (make-rthm-seq-bar '((3 4) q+e (e) s (s) e))))
(get-nth-attack 3 rsb nil))

=> NIL

|#
;;; SYNOPSIS
(defmethod get-nth-attack (index (rsb rthm-seq-bar) &optional (warn t))
;;; ****
  (if (or (< index 0)
          (>= index (notes-needed rsb)))
      (when warn
        (warn "~a~&rthm-seq-bar::get-nth-attack:  ~
                index (~a) < 0 or >= notes-needed (~a)"
              rsb index (notes-needed rsb)))
      (let* ((count 0)
             (event-count 0)
             (result (loop for r in (rhythms rsb) do
                          (when (needs-new-note r)
                            (if (= count index)
                                (return r)
                                (incf count)))
                          (incf event-count))))
        (when (and warn (not result))
          (warn "~a~&rthm-seq-bar::get-nth-attack: ~
                Couldn't get nth attack.  Index: ~a,  notes-needed: ~a"
                rsb index (notes-needed rsb)))
        (values result event-count))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Fri Jul 24 11:34:32 2015
;;; ****m* rthm-seq-bar/get-nth-attack-with-tied
;;; DATE
;;; July 24th 2015, Glenferness
;;; 
;;; DESCRIPTION
;;; Same as get-nth-attack method but will always return a list of the attacked
;;; event plus any following events this is tied to.
;;; 
;;; ARGUMENTS 
;;; - The zero-based index number indicating which attack is sought.
;;; - The given rthm-seq-bar object in which to search.
;;;
;;; OPTIONAL ARGUMENTS
;;; - T or NIL indicating whether to print a warning message if the given index
;;;   is greater than the number of attacks in the RHYTHMS list (minus one to 
;;;   compensate for the zero-based indexing) (default = T).   
;;; 
;;; RETURN VALUE  
;;; A rhythm object.
;;;
;;; Returns NIL if the given index is higher than the highest possible index of
;;; attacks in the given rthm-seq-bar object.
;;;
;;; SYNOPSIS
(defmethod get-nth-attack-with-tied (index (rsb rthm-seq-bar)
                                     &optional (warn t))
;;; ****
  (let ((e (get-nth-attack index rsb warn)))
    (when e
      (if (is-tied-from e)
          (cons e
                (loop for i from (1+ (bar-pos e))
                   for tied = (get-nth-event i rsb)
                   while (is-tied-to tied)
                   collect tied))
          (list e)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* rthm-seq-bar/get-last-attacks
;;; DATE
;;; July 24th 2015, Glenferness
;;; 
;;; DESCRIPTION
;;; Return the last nth attacks in a bar, with any notes tied from them.
;;; 
;;; ARGUMENTS
;;; - a rthm-seq-bar object
;;; - an integer indicating how many attacks to return
;;; 
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to indicate whether a warning should be issued if the required
;;;   number of attacks cannot be returned.
;;; 
;;; RETURN VALUE
;;; A list of lists of rhythm or event objects. The length of the outer list
;;; will be the same as the second argument. The sublists will contain the
;;; attack event with any subsequent events tied from it. NB if a request is
;;; made for more attacks than the bar contains the method returns NIL (rather
;;; than all the attacks in the bar).
;;;
;;; SYNOPSIS
(defmethod get-last-attacks ((rsb rthm-seq-bar) how-many &optional (warn t))
;;; ****
  (let ((nn (notes-needed rsb)))
    (if (> how-many nn)
        (when warn
          (warn "rthm-seq-bar::get-last-attacks: requested ~a but only ~a ~
                 attacks in bar." how-many nn))
        (loop for i from (- nn how-many) repeat how-many collect
             (get-nth-attack-with-tied i rsb warn)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Thu Mar 1 13:26:26 GMT 2012: Deleted MDE's comments here as they've
;;; been taken nearly verbatim into the doc entry

;;; SAR Thu Mar  1 13:24:41 GMT 2012: Added robodoc entry

;;; ****m* rthm-seq-bar/scale
;;; DESCRIPTION
;;; Change the values of a rthm-seq-bar objects rhythm durations by a specified
;;; scaling factor.
;;;
;;; This method always returns a new rthm-seq-bar object, recreating scaled
;;; rhythms with beams etc. where appropriate. See time-sig::scale for details
;;; on how the new meter is created.
;;; 
;;; ARGUMENTS 
;;; - A rthm-seq-bar object.
;;; - A number that is the scaling factor.
;;;
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to indicate whether to preserve the original meter (duple,
;;;   triple, quadruple etc.)
;;; - (two ignore arguments for internal use only)
;;; 
;;; RETURN VALUE  
;;; Returns a rthm-seq-bar object
;;; 
;;; EXAMPLE
#|
;;; Create a rthm-seq-bar object and scale its durations by a fact of ; ;
;;; 2. Returns a rthm-seq-bar object.   ; ;
(let ((rsb (make-rthm-seq-bar '((2 4) q e s s))))
(scale rsb 2))

=> 
RTHM-SEQ-BAR: time-sig: 19 (2 2), time-sig-given: T, bar-num: -1, 
[...]
RHYTHM: value: 2.000, duration: 2.000, rq: 2, is-rest: NIL, 
[...]
data: H
[...]
RHYTHM: value: 4.000, duration: 1.000, rq: 1, is-rest: NIL, 
[...]
data: Q
[...]
RHYTHM: value: 8.000, duration: 0.500, rq: 1/2, is-rest: NIL, 
[...]
data: E
[...]
RHYTHM: value: 8.000, duration: 0.500, rq: 1/2, is-rest: NIL, 
[...]
data: E
[...]

;;; Use the print-simple method to see formatted results ;
(let ((rsb (make-rthm-seq-bar '((2 4) q e s s))))
(print-simple (scale rsb .5)))

=>
(2 8): note E, note S, note 32, note 32,

;;; Set the optional <preserve-meter> argument to NIL to allow the method to ;
;;; return results in a different metric quality (this returns a quadruple ;
;;; meter rather than a duple)          ;
(let ((rsb (make-rthm-seq-bar '((6 8) q e q s s))))
(print-simple (scale rsb 2 nil)))

=>
(12 8): note H, note Q, note H, note E, note E,


|#
;;; SYNOPSIS
(defmethod scale ((rsb rthm-seq-bar) scaler
                  &optional (preserve-meter t) ignore1 ignore2)
;;; ****
  (declare (ignore ignore1) (ignore ignore2))
  ;;(print-simple rsb)
  (let* ((new-time-sig (scale (get-time-sig rsb) scaler preserve-meter))
         (result (make-rest-bar new-time-sig (write-time-sig rsb))))
    ;; (print new-time-sig)
    (setf (bar-num result) (bar-num rsb)
          (write-bar-num result) (write-bar-num rsb)
          (start-time result) (start-time rsb)
          (start-time-qtrs result) (start-time-qtrs rsb)
          (rhythms result)
          (loop for r in (rhythms rsb) 
              for new-r = (scale r scaler)
              do
                ;; for good reason scaling doesn't copy over beams, brackets,
                ;; and marks... 
                (setf (beam new-r) (beam r)
                      (bracket new-r) (bracket r)
                      (marks new-r) (my-copy-list (marks r)))
                ;; (format t "~&old ~a new ~a" (num-flags r) (num-flags new-r))
                ;; NB this could mean that the beams slot of rsb is no longer
                ;; correct...  
                (when (zerop (num-flags new-r))
                  (setf (beam new-r) nil))
              collect new-r)
          ;; beams look like ((6 11) (2 4)) where the numbers refer to the nth
          ;; non-rest-rhythm in the bar for start and end beam.
          (beams result)
          (loop 
            ;; with bresult = '()
              for beam in (beams rsb) 
              for st = (first beam)
              for nd = (second beam)
                  ;; do
                 ;; (format t "~&flags: ~a ~a" (num-flags st) (num-flags nd))
              appending
                (loop 
                    with bm = '()
                    with start with end
                    for i from st to nd
                     ;; MDE Tue May 29 23:05:27 2012 -- we can now have beams
                   ;; on rests!
                   ;; for r = (get-nth-non-rest-rhythm i result)
                   for r = (get-nth-event i result)
                    do 
                      (unless r
                        (error "~&rthm-seq-bar::scale: can't get notes ~
                                for beams ~a"
                               beam))
                      (if (has-flags r)
                          (if start
                              (setf end i)
                            (setf start i))
                        (progn
                          (when (and start end
                                     (/= start end))
                            (push (list start end) bm))
                          (setf start nil
                                end nil)))
                    finally 
                      (when (and start end
                                 (/= start end))
                        (push (list start end) bm))
                      (return bm))))
    ;; (gen-stats result)
    ;; (print 'here)
    (update-rhythms-beam-info result t)
    result))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 12.12.11 SAR: Added ROBODoc info
;;; ****m* rthm-seq-bar/set-nth-attack
;;; DESCRIPTION
;;; Sets the value of the nth rhythm object of a given rthm-seq-bar that needs
;;; an attack; i.e., not a rest and not a tied note.
;;;
;;; NB: This method does not check to ensure that the resulting rthm-seq-bar
;;; contains the right number of beats.
;;; 
;;; ARGUMENTS 
;;; - A zero-based index number for the attacked note to change.
;;; - An event.
;;; - A rthm-seq-bar object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - T or NIL indicating whether to print a warning message if the given index
;;;   (minus one) is greater than the number of attacks in the RHYTHMS
;;;   list. Default = T.    
;;; 
;;; RETURN VALUE  
;;; An event object.
;;; 
;;; EXAMPLE
#|
(let ((rsb (make-rthm-seq-bar '((2 4) q+e s s))))
(set-nth-attack 1 (make-event 'e4 'q) rsb))

=>
EVENT: start-time: NIL, end-time: NIL, 
[...]
PITCH: frequency: 329.6275526703903d0, midi-note: 64, midi-channel: NIL 
[...]
NAMED-OBJECT: id: E4, tag: NIL, 
data: E4
[...]
RHYTHM: value: 4.0, duration: 1.0, rq: 1, is-rest: NIL, score-rthm: 4.0, 
[...]
NAMED-OBJECT: id: Q, tag: NIL, 
data: Q

(let ((rsb (make-rthm-seq-bar '((2 4) q+e s s))))
(set-nth-attack 2 (make-event 'e4 'q) rsb)
(loop for r in (rhythms rsb) collect (data r)))

=> ("Q" "E" S Q)

(let ((rsb (make-rthm-seq-bar '((2 4) q+e s s))))
(set-nth-attack 3 (make-event 'e4 'q) rsb))

=> NIL
rthm-seq-bar::set-nth-attack: index (3) < 0 or >= notes-needed (3)

(let ((rsb (make-rthm-seq-bar '((2 4) q+e s s))))
(set-nth-attack 3 (make-event 'e4 'q) rsb nil))

=> NIL
|#
;;; SYNOPSIS
(defmethod set-nth-attack (index (e event) (rsb rthm-seq-bar) 
                           &optional (warn t))
;;; ****
  (if (or (< index 0)
          (>= index (notes-needed rsb)))
      (when warn
        (warn "~a~&rthm-seq-bar::set-nth-attack: ~
                index (~a) < 0 or >= notes-needed (~a)"
              rsb index (notes-needed rsb)))
    (let ((nth (loop
                   with count = 0
                   for i from 0
                   for r in (rhythms rsb) do
                     (when (needs-new-note r)
                       (if (= count index)
                           (return i)
                         (incf count))))))
      (when (and warn (not nth))
        (warn "~a~&rthm-seq-bar::set-nth-attack: ~
                Couldn't set nth attack.  Index: ~a,  notes-needed: ~a"
              rsb index (notes-needed rsb)))
      (when nth
        (setf (nth nth (rhythms rsb)) e)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 12.12.11 SAR: Added ROBODoc info

;;; ****m* rthm-seq-bar/get-last-attack
;;; DESCRIPTION
;;; Gets the rhythm object for the last note that needs an attack (i.e. not a
;;; rest and not a tied note) in a given rthm-seq-bar object.
;;; 
;;; ARGUMENTS 
;;; - The given rthm-seq-bar object.
;;;
;;; OPTIONAL ARGUMENTS
;;; - T or NIL indicating whether to print a warning message if the given index
;;;   (minus one) is greater than the number of attacks in the RHYTHMS list 
;;;   (default = T). This is a carry-over argument from the get-nth-attack
;;;   method called within the get-last-attack method and not likely to be
;;;   needed for use with get-last-attack.
;;; 
;;; RETURN VALUE  
;;; A rhythm object.
;;;
;;; Returns NIL if the given index is higher than the highest possible index of
;;; attacks in the given rthm-seq-bar object.
;;; Get the rhythm object of the last 
;;; 
;;; EXAMPLE
#|
(let ((rsb (make-rthm-seq-bar '((3 4) q+e (e) s (s) e))))
(get-last-attack rsb))

=>
RHYTHM: value: 8.0, duration: 0.5, rq: 1/2, is-rest: NIL, score-rthm: 8.0, 
undotted-value: 8, num-flags: 1, num-dots: 0, is-tied-to: NIL, 
is-tied-from: NIL, compound-duration: 0.5, is-grace-note: NIL, 
needs-new-note: T, beam: NIL, bracket: NIL, rqq-note: NIL, 
rqq-info: NIL, marks: NIL, marks-in-part: NIL, letter-value: 8, 
tuplet-scaler: 1, grace-note-duration: 0.05,
LINKED-NAMED-OBJECT: previous: NIL
this: NIL
next: NIL
NAMED-OBJECT: id: E, tag: NIL, 
data: E

|#
;;; SYNOPSIS
(defmethod get-last-attack ((rsb rthm-seq-bar) &optional (warn t))
;;; ****
  (get-nth-attack (1- (notes-needed rsb)) rsb warn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; All the time-sigs this class has seen so far are stored in all-time-sigs.
;;; Pull the one at <index> out of the list, or if <index> is not given, return
;;; the current-time-sig, i.e. the last one the class saw (not the last one it
;;; stored however!).

(defmethod get-time-sig-from-all-time-sigs ((rsb rthm-seq-bar)
                                            &optional (index nil))
  ;; 13.2.10 hang on: according to the above description we should be able to
  ;; pass a nil index... 
  ;; (unless (integer>=0 index)
  (when index
    (unless (integer>=0 index)
      (error "~a rthm-seq-bar::get-time-sig-from-all-time-sigs: index = ~a!"
             rsb index)))
  (if index
      (nth index (all-time-sigs rsb))
      (nth (current-time-sig rsb) (all-time-sigs rsb))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod bar-qtr-duration ((rsb rthm-seq-bar))
  (duration (get-time-sig rsb)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Called by sequenz::update-slots

(defmethod update-events-bar-nums ((rsb rthm-seq-bar) bar-num)
  (loop for e in (rhythms rsb) do
        (when (event-p e)
          (setf (bar-num e) bar-num))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod rhythms-to-events ((rsb rthm-seq-bar))
  ;; MDE Thu Apr 19 11:19:32 2012 -- this calls the setf method which calls
  ;; gen-stats which updates sounding-duration
  (setf (rhythms rsb) (rhythms-to-events-list (rhythms rsb)))
  rsb)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Thu Feb  2 14:11:35 GMT 2012: Added NB about tuplets and chop. Also
;;; leaving this fragment here for later, and for internal reference:

;;; This is because the chop method determines a new time-signature for each of
;;; the rthm-seq-bar objects it creates based on the value of the <unit>
;;; argument, and no time-signature can be created from a tuplet unit value. 

;;; SAR Tue Jan 17 16:54:39 GMT 2012: Delete MDE's comment here and moved it
;;; nearly verbatim to the documentation below.

;;; SAR Tue Jan 17 16:54:23 GMT 2012: Added robodoc info

;;; ****m* rthm-seq-bar/chop
;;; DESCRIPTION
;;; Creates a list of new rthm-seq-bar objects, with new time signatures, which
;;; are formed by systematically chopping the bar represented by the current
;;; rthm-seq-bar into segments. 
;;;
;;; The method creates these segments based on chop-point pairs specified in
;;; the <chop-points> argument, which is a list of 2-element lists, each of
;;; which specifies the start and end points of a rhythmic span within the
;;; bounds of a given beat, measured in the unit specified by the <unit>
;;; argument. 
;;;
;;; The chop points specified are used to individually process each beat in the
;;; given rthm-seq-bar object; thus, chop-points specified for the subdivisions
;;; of a quarter-note will not work if applied to a 5/8 bar.
;;;
;;; The method fills each newly created rthm-seq-bar object with one rhythmic
;;; duration that is equal to the length of the bar. If the beginning of the
;;; given chop segment coincides with an attack in the original bar, the result
;;; is a sounding note; if not, the result is a rest. NB: In this abstraction
;;; of the class for the sake of this documentation, sounding notes will appear
;;; as NIL. 
;;;
;;; The chop method is the basis for slippery-chicken's feature of
;;; intra-phrasal looping.
;;;
;;; NB: The <unit> argument must be a duplet rhythmic value (i.e. 32, 's, 'e
;;;     etc.) and cannot be a tuplet value (i.e. 'te 'fe etc.). 
;;;
;;; NB: In order for the resulting chopped rhythms to be parsable by LilyPond
;;;     and CMN, there can be no tuplets (triplets etc.) among the rhythms to
;;;     be chopped. Such rhythms will result in LilyPond and CMN errors. This
;;;     has only minimal bearing on any MIDI files produced, however, and these
;;;     can potentially be imported into notation software.
;;;
;;; ARGUMENTS 
;;; - A rthm-seq-bar object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - <chop-points> A list of integer pairs, each of which delineates a segment
;;;   of the beat of the given rthm-seq-bar object measured in the rhythmic
;;;   unit specified by the <unit> argument. Thus, if all possible spans of
;;;   sixteenth-notes within a quarter-note, starting from the first sixteenth,
;;;   were delineated, they would span from 1 to 4 (the full quarter), 1 to 3
;;;   (the first dotted 8th of the quarter), 1 to 2 (the first 8th) and 1 to 1
;;;   (the first 16th of the quarter); the process could continue then with all
;;;   rhythmic durations contained within the bounds of the same quarter
;;;   starting on the second 16th, etc. The default chop-points for a quarter
;;;   are '((1 4) (1 3) (1 2) (2 4) (2 3) (3 4) (1 1) (2 2) (3 3) (4 4)).
;;; - <unit>. The rhythmic duration that serves as the unit of measurement for
;;;   the chop points. Default = 's.
;;; - <rthm-seq-id>. A symbol that will be the ID for the list created.
;;; 
;;; RETURN VALUE  
;;; A list of rthm-seq-bar objects.
;;; 
;;; EXAMPLE
#|
;; Systematically subdivide each quarter-note of a 2/4 bar containing two ; ;
;; quarter-notes into all possible segments whose durations are multiples of a ; ;
;; sixteenth-note unit, and print-simple the resulting list. The quarter-note ; ;
;; subdivision is re-specified here sightly differently to the default for the ; ;
;; sake of systematic clarity. Only those segments whose start point coincide ; ;
;; with an attack in the original bar, i.e. those that begin on the first ; ;
;; sixteenth of each  beat, will be assigned a NIL (which will later become ; ;
;; a sounding note); all others are assigned a rest. ; ;

(let* ((rsb (make-rthm-seq-bar '((2 4) q q)))
(ch (chop rsb 
'((1 4) (1 3) (1 2) (1 1) 
(2 4) (2 3) (2 2) 
(3 4) (3 3) 
(4 4))   
's))) 
(loop for b in ch do (print-simple b)))

=>
(1 4): NIL Q, 
(3 16): NIL E., 
(1 8): NIL E, 
(1 16): NIL S, 
(3 16): rest 16/3, 
(1 8): rest 8, 
(1 16): rest 16, 
(1 8): rest 8, 
(1 16): rest 16, 
(1 16): rest 16, 
(1 4): NIL Q, 
(3 16): NIL E., 
(1 8): NIL E, 
(1 16): NIL S, 
(3 16): rest 16/3, 
(1 8): rest 8, 
(1 16): rest 16, 
(1 8): rest 8, 
(1 16): rest 16, 
(1 16): rest 16,

;; The same thing, but returning all possible segments within the bounds of a ; ;
;; quarter-note whose durations that are multiple of an 8th-note unit ; ;
(let* ((rsb (make-rthm-seq-bar '((2 4) q q)))
(choprsb (chop rsb 
'((1 2) (1 1) (2 2))
'e)))
(loop for b in choprsb do (print-simple b)))

=>
(1 4): NIL Q, 
(1 8): NIL E, 
(1 8): rest 8, 
(1 4): NIL Q, 
(1 8): NIL E, 
(1 8): rest 8,

;; Adapt the 16th-note example above to a starting rthm-seq-bar object with ; ;
;; more complex rhythmic content. Note here, too, that the rthm-seq-bar object ; ;
;; being segmented contains rhythmic durations smaller than the <unit> ; ;
;; argument.                            ; ;
(let* ((rsb (make-rthm-seq-bar '((4 4) - (s) (32) 32 (s) s - - +s+32 (32) (e) -
(q) (s) s (e))))  
(choprsb (chop rsb 
'((1 4) (1 3) (1 2) (1 1) 
(2 4) (2 3) (2 2) 
(3 4) (3 3) 
(4 4))
's)))
(loop for b in choprsb do (print-simple b)))

=>
(1 4): rest S, rest 32, NIL 32, rest S, NIL S, 
(3 16): rest S, rest 32, NIL 32, rest S, 
(1 8): rest S, rest 32, NIL 32, 
(1 16): rest 16, 
(3 16): rest 32, NIL 32, rest S, NIL S, 
(1 8): rest 32, NIL 32, rest S, 
(1 16): rest 32, NIL 32, 
(1 8): rest S, NIL S, 
(1 16): rest 16, 
(1 16): NIL S, 
(1 4): rest 4, 
(3 16): rest 16/3, 
(1 8): rest 8, 
(1 16): rest 16, 
(3 16): rest 16/3, 
(1 8): rest 8, 
(1 16): rest 16, 
(1 8): rest 8, 
(1 16): rest 16, 
(1 16): rest 16, 
(1 4): rest 4, 
(3 16): rest 16/3, 
(1 8): rest 8, 
(1 16): rest 16, 
(3 16): rest 16/3, 
(1 8): rest 8, 
(1 16): rest 16, 
(1 8): rest 8, 
(1 16): rest 16, 
(1 16): rest 16, 
(1 4): rest S, NIL S, rest E, 
(3 16): rest S, NIL S, rest S, 
(1 8): rest S, NIL S, 
(1 16): rest 16, 
(3 16): NIL S, rest E, 
(1 8): NIL S, rest S, 
(1 16): NIL S, 
(1 8): rest 8, 
(1 16): rest 16, 
(1 16): rest 16,

;; The same again with a <unit> of eighths ; ;
(let* ((rsb (make-rthm-seq-bar '((4 4) - (s) (32) 32 (s) s - - +s+32 (32) (e) -
(q) (s) s (e))))  
(choprsb (chop rsb 
'((1 2) (1 1) (2 2))
'e)))
(loop for b in choprsb do (print-simple b)))

=>
(1 4): rest S, rest 32, NIL 32, rest S, NIL S, 
(1 8): rest S, rest 32, NIL 32, 
(1 8): rest S, NIL S, 
(1 4): rest 4, 
(1 8): rest 8, 
(1 8): rest 8, 
(1 4): rest 4, 
(1 8): rest 8, 
(1 8): rest 8, 
(1 4): rest S, NIL S, rest E, 
(1 8): rest S, NIL S, 
(1 8): rest 8,

|#
;;; SYNOPSIS
(defmethod chop ((rsb rthm-seq-bar) 
                 &optional chop-points (unit 's) rthm-seq-id)
;;; **** 
  (let* ((quarter-in-semiquavers
          ;; these are the chop points for a quarter note being divided into
          ;; semiquavers.  The first number is the start semi-quaver, the
          ;; second the end (both inclusive).
          '((1 4) (1 3) (1 2) (2 4) (2 3) (3 4) (1 1) (2 2) (3 3) (4 4)))
         (unit-dur (duration (make-rhythm unit)))
         (num (num (get-time-sig rsb)))
         (time-sig (get-time-sig rsb))
         (beat-dur (beat-duration time-sig))
         (cps (if chop-points chop-points quarter-in-semiquavers))
         (qstimes (loop for point in cps collect
                       (list (* unit-dur (1- (first point)))
                             (* unit-dur (second point))))))
    #|                                  
(when (and (not chop-points)
    (/= denom 4))
    (error "rthm-seq-bar::chop-bar: You have to supply chop-points for ~
      time signatures that are not x/4 (e.g. 3/8)"))
|#
    (loop for beat below num appending
          (loop 
              for point in qstimes 
              for beat-offset = (* beat-dur beat)
              collect 
                (new-bar-from-time-range rsb 
                                         (+ beat-offset (first point))
                                         (+ beat-offset (second point))
                                         :rthm-seq-id rthm-seq-id)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Create a new bar (with a new time sig of course) from the events within the
;;; given time range.  This will try to insert a rest at the beginning and end
;;; if the durations of the rhythms between these times don't add up to the
;;; full duration between start and end time.

(defmethod new-bar-from-time-range ((rsb rthm-seq-bar) start-time end-time
                                    ;; by default specify times relative to the
                                    ;; start time of the bar so that our
                                    ;; start-time and end-time range between 0
                                    ;; and the full duration of the bar.
                                    &key (time-relative-to-bar-start t)
                                    (rthm-seq-id "")
                                    (tempo 60.0)
                                    (midi-channel 0)
                                    (microtones-midi-channel 0))
  ;; (print start-time) (print end-time)
  (let* ((bar-start (start-time rsb))
         (new-dur (- end-time start-time))
         (start (if time-relative-to-bar-start
                    start-time
                    (- start-time bar-start)))
         (end (if time-relative-to-bar-start
                  end-time
                  (- end-time bar-start)))
         ;; clone the bar and update its time starting from 0 to make sure we
         ;; have the right data
         (rsb-clone (clone rsb))
         (time-sig (make-time-sig-from-duration (- end-time start-time) 
                                                tempo))
         (result (make-rest-bar time-sig t)) ; make write-time sig t!
         (beat (get-beat-as-rhythm rsb))
         missing-start missing-end)
    (rhythms-to-events rsb-clone)
    (update-time rsb-clone 0 0 tempo)
    (multiple-value-bind
          (start-attack end-attack events)
        (get-events rsb-clone start end)
      ;; (loop for e in events do (print e))
      (setf missing-start (if events
                              (- (start-time (first events))
                                 start)
                              new-dur)
            missing-end (when events
                          (- end-time (start-time (first (last events))))))
      (unless (almost-zero missing-start)
        (setf events (append 
                      (rhythms-to-events-list
                       (rationalize-if-necessary missing-start
                                                 :tempo tempo
                                                 :keep-it-simple t))
                      events)))
      (when (and missing-end (not (almost-zero missing-end)))
        (setf events (append events 
                             (rhythms-to-events-list
                              (rationalize-if-necessary missing-end
                                                        :keep-it-simple t
                                                        :tempo tempo)))))
      ;; (loop for e in events do (print (data e)))
      (fill-with-rhythms 
       result events 
       :midi-channel midi-channel 
       :microtones-midi-channel microtones-midi-channel
       :new-id (format nil "new-bar-from-~a~a-time-range-~,3f-to-~,3f"
                       rthm-seq-id
                       (if (> (bar-num rsb) 0)
                           (format nil "-b~a"
                                   (bar-num rsb))
                           "")
                       start-time end-time))
      (update-time result 0 0 tempo)
      (if (all-rests? result)
          (force-rest-bar result)
          (setf (parent-start-end result) (list start-attack end-attack)))
      ;; use the beat of the parent rsb to set the beams for this extract.
      (auto-beam result beat)
      (setf (time-sig-given result) t)
      #|
(format t "~%get-events: ~a->~a: num-notes: ~a, ~a"
      start-time end-time (notes-needed result)
      (parent-start-end result))
|#
      (loop for r in (rhythms result) do
            (when (and (is-rest r)
                       (marks r))
              (format t "~a~%rest with marks!" r)))
      ;;(print-simple result)
      result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Get events that are within a certain time range within the bar: from
;;; start-time (inclusive) to end-time (exclusive).

(defmethod get-events ((rsb rthm-seq-bar) start-time end-time)
  (let ((first-event (get-nth-event 0 rsb))
        ;; we need to keep track of the event number in the bar that we're
        ;; starting and ending with so that we can pass this back up to the
        ;; rthm-seq to get data out of the pitch-seq-palette
        (start-attack 0)
        (end-attack 0)
        (attack-count 0)
        (result '()))
    (unless (and (event-p first-event)
                 (start-time first-event))
      (rhythms-to-events rsb)
      (update-time rsb 0 0 60))
    (loop 
       for e in (rhythms rsb) 
       do
       (when (needs-new-note e)
         (incf attack-count))
       (when (and (>= (start-time e) start-time)
                  (< (start-time e) end-time))
         (when (and (needs-new-note e) (zerop start-attack))
           (setf start-attack attack-count))
         ;; this will mean once we're outside our time range, we've got the
         ;; last attack we saw within it
         (setf end-attack attack-count)
         ;; (format t "~&***start/end ~a ~a~&~a" start-attack end-attack e)
         (if (<= (end-time e) end-time)
             (push e result)
             ;; this rhythm is too long: need to chop it off at end-time
             (let* ((dur (- end-time (start-time e)))
                    (new (make-event (pitch-or-chord e) dur 
                                     :start-time (start-time e)
                                     ;; MDE Wed Dec 14 17:37:52 2011 -- if we
                                     ;; use the keyword, it might be overridden
                                     ;; if poc is nil, so set it below
                                     ;; :is-rest (is-rest e)
                                     :is-tied-to (is-tied-to e)
                                     :duration t)))
               ;; (print '*****) (print dur)
               ;; MDE Wed Dec 14 17:37:42 2011 
               (setf (is-rest new) (is-rest e))
               (if new
                   (setf (is-grace-note new) (is-grace-note e)
                         (needs-new-note new) (needs-new-note e)
                         (beam new) (beam e)
                         (bracket new) (bracket e)
                         (marks new) (my-copy-list (marks e))
                         (marks-before new) (marks-before e)
                         ;; MDE Sun Feb  5 10:08:56 2012 -- if we setf
                         ;; amplitude directly we will add a mark!  
                         ;; (amplitude new) (amplitude e))
                         (slot-value new 'amplitude) (amplitude e))
                   ;; if we can't get a single rthm for the new duration then
                   ;; new will be nil and we should just create a rest
                   (progn
                     ;; MDE Wed Dec 14 17:24:09 2011 
                     (unless (is-rest e)
                       (decf end-attack))
                     (setf new (make-rest (- end-time start-time) :duration t
                                          :start-time (start-time e)))
                     (unless new
                       (error "rthm-seq-bar::get-events: can't create rest ~
                              from duration ~a secs"
                              (- end-time start-time)))))
               (when (and (is-rest new)
                          (marks new))
                 (error "~a~%rthm-seq-bar::get-events: rest with marks?"
                        new))
               (push new result)))))
    (setf result (nreverse result))
    (loop for e in result with got-strike = nil do
       ;; delete the beaming info (do auto-beam in new-bar-from-time-range)
       ;; NB the bracketing info could still cause problems....
         (setf (beam e) nil)
         (when (needs-new-note e)
           (setf got-strike t))
         (unless got-strike
           (when (is-tied-to e)
             (force-rest e)
             (delete-marks e))))
    (when result
      (setf (is-tied-from (first (last result))) nil))
    ;; our first attack was numbered 1, so 1- to get a list reference that can
    ;; then be passed to subseq e.g.
    ;; (subseq '(0 1 2 3 4 5 6 7) 2 6) -> (2 3 4 5)
    (values (1- start-attack) end-attack result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Called by sequenz::update-slots

(defmethod update-time ((rsb rthm-seq-bar) start-time start-time-qtrs tempo)
  (setf (start-time rsb) start-time
        (start-time-qtrs rsb) start-time-qtrs)
  (let ((bar-dur (bar-duration rsb tempo)))
    (multiple-value-bind
          (new-events end-time)
        (events-update-time (rhythms rsb) :start-time start-time
                            :start-time-qtrs
                            start-time-qtrs :tempo tempo)
      (setf (rhythms rsb) new-events)
      (unless (is-rest-bar rsb)
        (unless (equal-within-tolerance bar-dur (- end-time start-time) .003)
          (error "~&rthm-seq-bar::update-time: Duration of rhythms don't ~
                  match that of bar: ~%rhythms ~a secs : bar ~a secs:~%~a"
                 (- end-time start-time) bar-dur rsb))))
    bar-dur))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Set write-time-sig to t (this could change later when we're really writing
;;; bars of the piece not just isolated rthm-seqs), store the time-sig in the
;;; all-time-sigs slot and set current-time-sig to the index to this one in
;;; that list.

(defmethod (setf time-sig) (value (rsb rthm-seq-bar))
  (unless (time-sig-p value)
    (setf value (make-time-sig value)))
  #|
(error "rthm-seq-bar::time-sig: Only time-sig objects may be setf'd ~
            here: ~a"
  value))
        |#
  (let ((current (store-time-sig rsb value)))
    (setf (slot-value rsb 'time-sig) current
          (time-sig-given rsb) t
          (current-time-sig rsb) current
          (write-time-sig rsb) t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod (setf rhythms) :after (value (rsb rthm-seq-bar))
  (declare (ignore value))
  (gen-stats rsb))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod (setf missing-duration) :after (value (rsb rthm-seq-bar))
  (declare (ignore value))
  (update-missing-duration rsb))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 13.12.11 SAR: Added ROBODoc info
;;; ****m* rthm-seq-bar/get-time-sig
;;; DESCRIPTION
;;; Return the time-sig object for the given rthm-seq-bar object.
;;; 
;;; ARGUMENTS 
;;; - A rthm-seq-bar object.
;;; 
;;; RETURN VALUE  
;;; A time-sig object.
;;; 
;;; EXAMPLE
#|
(let ((rsb (make-rthm-seq-bar '((2 4) q e s s))))
  (get-time-sig rsb))
=> 
TIME-SIG: num: 2, denom: 4, duration: 2.0, compound: NIL, midi-clocks: 24, 
num-beats: 2 
SCLIST: sclist-length: 2, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: "0204", tag: NIL, 
data: (2 4)

|#
;;; SYNOPSIS
(defmethod get-time-sig ((rsb rthm-seq-bar) &optional ignore)
;;; ****
  (declare (ignore ignore))
  (nth (time-sig rsb) (all-time-sigs rsb)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 13.12.11 SAR: added robodoc info
;;; ****m* rthm-seq-bar/get-time-sig-as-list
;;; DESCRIPTION
;;; Get the time signature for a given rthm-seq-bar object in list form.
;;; 
;;; ARGUMENTS 
;;; - A rthm-seq-bar object.
;;; 
;;; RETURN VALUE  
;;; A list.
;;; 
;;; EXAMPLE
#|
(let ((rsb (make-rthm-seq-bar '((2 4) q e s s))))
  (get-time-sig-as-list rsb))
=> (2 4)
|#
;;; SYNOPSIS
(defmethod get-time-sig-as-list ((rsb rthm-seq-bar))
;;; ****
  (data (get-time-sig rsb)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 13.12.11 SAR: Added robodoc info
;;; ****m* rthm-seq-bar/time-sig-equal
;;; DESCRIPTION
;;; Check to see if two given rthm-seq-bar objects have the same time signature.
;;; 
;;; ARGUMENTS 
;;; - Two rthm-seq-bar objects.
;;; 
;;; RETURN VALUE  
;;; T if the given rthm-seq-bar objects have the same time signature.
;;; NIL if the given rthm-seq-bar objects have different times signatures.
;;; 
;;; EXAMPLE
#|
(let ((rsb1 (make-rthm-seq-bar '((2 4) q e s s)))
      (rsb2 (make-rthm-seq-bar '((2 4) s s e q))))
  (time-sig-equal rsb1 rsb2))

=> T

(let ((rsb1 (make-rthm-seq-bar '((2 4) q e s s)))
      (rsb2 (make-rthm-seq-bar '((3 4) q+e e s s s s))))
  (time-sig-equal rsb1 rsb2))
=> NIL
|#
;;; SYNOPSIS
(defmethod time-sig-equal ((rsb1 rthm-seq-bar) (rsb2 rthm-seq-bar))
;;; ****
  (time-sig-equal (get-time-sig rsb1) (get-time-sig rsb2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#| 17/7/05: obsolete code as ties are handled now at the piece level

;;; Usually only struck (non-tied and non-rest) notes will have their ; ;
;;; compound-duration set to include any following tied notes, but when the ; ;
;;; first note of the bar is tied, this has to be the one to get the updated ; ;
;;; compound duration.  Tied first notes of the bar are handled separately with ; ;
;;; handle-first-note-ties in the rthm-seq class ; ;

        (defmethod update-compound-durations ((rsb rthm-seq-bar))
  ;; (print 'update-compound-durations) ; ; ;
  ;; 0 will ensure that if the first note is a tie, this will nevertheless be ; ; ;
  ;; updated                            ; ; ;
(let ((last-struck 0))
(loop for r in (rest (rhythms rsb)) and i from 1 do
(when (needs-new-note r)
(setq last-struck i))
(when (is-tied-to r)
(inc-nth-rthm rsb last-struck (compound-duration r))))))

        |#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; NB Only increments the COMPOUND-DURATION slot, nothing else!!!

(defmethod inc-nth-rthm ((rsb rthm-seq-bar) nth inc)
  (let ((rthm (nth nth (rhythms rsb))))
    ;; (format t "~%inc-nth-rthm: ~a ~a" (compound-duration rthm) inc)
    (incf (compound-duration rthm) inc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 8/5/06: get-rest-bars is for when we're writing MIDI files: got to get the
;;; time signature change.  NB This will still fail where there's a bar's rest
;;; followed by a rest longer in duration than the previous bar's rest. 
;;; 
;;; 8/3/07: ignore-rests means the duration of rests will be added to the
;;; duration of the previously struck note.  include-rests will make sure rests
;;; are collected into the result.  This is somewhat confusing but we need
;;; rests when generating midi files (they might have program changes on them).

(defmethod get-timings ((rsb rthm-seq-bar) time-scaler ignore-rests
                        get-time-sig-changes 
                        &optional (include-rests nil) (ignore-grace-notes nil))
  ;; (print ignore-grace-notes)  
  ;; (print ignore-rests)
  (let ((result '()))
    (if (and (is-rest-bar rsb) 
             (write-time-sig rsb)
             get-time-sig-changes)
        (progn
          (unless (= time-scaler 1.0)
            (error "rthm-seq-bar::get-timings: can't yet handle scaling ~
                    of rest bars"))
          ;; (print (bar-num rsb))  
          ;; perhaps setting tempo would force a bar line in the cases 
          ;; described in NB above    
          (setf result (my-copy-list (rhythms rsb))))
        (loop 
           for event in (rhythms rsb) 
           for scaled-event = (if (is-grace-note event)
                                  event
                                  ;; clones the event
                                  (scale event time-scaler t t))
           with rests = '()
           do
           (if (needs-new-note scaled-event)
               (unless (and (is-grace-note event) ignore-grace-notes)
                 (when include-rests
                   ;; 25/4/10 sure reverse rests, no?
                   (loop for r in (nreverse rests) do (push r result))
                   (setf rests nil))
                 (push scaled-event result))
               (progn
                 (when (and include-rests (is-rest scaled-event))
                   (push scaled-event rests))
                 ;; (format t "~%~a ~a ~a"
                 ;;     ignore-rests  (is-rest scaled-event) (first result))
                 (when (and ignore-rests result (is-rest scaled-event))
                   (incf (compound-duration-in-tempo (first result))
                         (duration-in-tempo scaled-event)))))
           ;; 25/4/10: had to add this to make sure we get rests when we have a
           ;; bar of rests only (but is-rest-bar is nil...)
           finally (loop for r in (nreverse rests) do (push r result))))
    (setf result (nreverse result))
    (when (and (first result) (write-time-sig rsb))
      (set-midi-time-sig (first result) (get-time-sig rsb)))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-simple ((rsb rthm-seq-bar) &optional written (stream t))
  (format stream "~&bar ~a: ~a: " (bar-num rsb) (get-time-sig-as-list rsb))
  (loop for r in (rhythms rsb) do
       (print-simple r written stream))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object :before ((i rthm-seq-bar) stream)
  (format stream "~%RTHM-SEQ-BAR: time-sig: ~a ~a, ~
                                  time-sig-given: ~a, ~
                                  bar-num: ~a, ~
                  ~%              old-bar-nums: ~a, ~
                                  write-bar-num: ~a, ~
                                  start-time: ~,3f, ~
                  ~%              start-time-qtrs: ~a, ~
                                  is-rest-bar: ~a, ~
                                  multi-bar-rest: ~a, ~
                  ~%              show-rest: ~a, ~
                                  notes-needed: ~a, ~
                  ~%              tuplets: ~a, ~
                                  nudge-factor: ~a, ~
                  ~%              beams: ~a, rsp-id: ~a ~
                  ~%              current-time-sig: ~a, ~
                                  write-time-sig: ~a, ~
                                  num-rests: ~a, ~
                  ~%              num-rhythms: ~a, ~
                                  num-score-notes: ~a, ~
                                  parent-start-end: ~a, ~
                  ~%              missing-duration: ~a, ~
                                  bar-line-type: ~a, ~
                  ~%              player-section-ref: ~a, ~
                                  nth-seq: ~a, ~
                                  nth-bar: ~a, ~
                  ~%              rehearsal-letter: ~a, ~
                                  all-time-sigs: (too long to print) ~
                  ~%              sounding-duration: ~,3f, ~
                  ~%              rhythms: ~a"
          (time-sig i) (get-time-sig-as-list i) (time-sig-given i) (bar-num i)
          (old-bar-nums i) (write-bar-num i)
          (start-time i) (start-time-qtrs i) (is-rest-bar i) (multi-bar-rest i)
          (show-rest i) (notes-needed i) (tuplets i)
          (nudge-factor i) (beams i) (rsp-id i) (current-time-sig i)
          (write-time-sig i) 
          (num-rests i) (num-rhythms i) (num-score-notes i) (parent-start-end i)
          (missing-duration i) (bar-line-type i) (player-section-ref i)
          (nth-seq i) (nth-bar i) (rehearsal-letter i) 
          ;; (slot-value i 'sounding-duration)
          (sounding-duration i)
          (rhythms i)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; If <time-sig> is in the class slot all-time-sigs of rsb, return it's
;;; position, otherwise tack it onto the end and return that position.  We
;;; can't push the new time-sigs in because that would nullify the references
;;; to time-sigs in this list that we have already stored in other rthm-seq-bar
;;; instances.

(defmethod store-time-sig ((rsb rthm-seq-bar) (time-sig time-sig))
  (let ((tsp (time-sig-pos rsb time-sig)))
    (if tsp
        tsp
        (progn
          (setf (all-time-sigs rsb) (econs (all-time-sigs rsb) time-sig))
          ;; we just put the new time-sig at the end so it's position will be 1-
          ;; length--return this.
          (1- (length (all-time-sigs rsb)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Get the position (zero-based) of the given <time-sig> in the all-time-sigs
;;; slot.  The given <time-sig> can be newly made, it is just used to compare
;;; against those stored in all-time-sigs using time-sig-equal.  Return nil,
;;; naturally, when it wasn't there.

(defmethod time-sig-pos ((rsb rthm-seq-bar) (time-sig time-sig))
  (position time-sig (all-time-sigs rsb) :test 
            #'(lambda (x y)
                ;; remember this function will return 'time-sig-equal-duration
                ;; when comparing 3/4 and 6/8
                (eq t (time-sig-equal x y)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Check that the bracket info for the bar looks correct (avoids hard-to-trace
;;; errors in cmn).

(defmethod verify-brackets ((rsb rthm-seq-bar))
  (let ((got-beg nil)
        (got-end nil)
        (was-bracket nil)
        (error nil)
        (br nil)
        (fbr nil))
    (loop for r in (rhythms rsb) do
         (setf br (bracket r)
               fbr (first br))
         (if br
             (progn
               (unless (listp br)
                 (setf error t))
               (setf was-bracket t))
             (when got-beg
               (setf error t)))
         (when fbr
           (if (listp fbr)
               (if got-beg
                   (setf error t)
                   (setf got-beg t
                         got-end nil))
               (if (> fbr 0)
                   (setf got-end t
                         got-beg nil)
                   (unless got-beg
                     (setf error t))))))
    (when (and was-bracket
               (or error (not got-end)))
      (error "~a~%rthm-seq-bar::verify-brackets: ~
              Error in bracket info (see above)!"
             rsb))
    t))
                   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod xml-time-sig ((rsb rthm-seq-bar) stream
                         &optional time-sig) ; if we have it
  (unless time-sig 
    (setf time-sig (get-time-sig rsb)))
  (xml-time-sig time-sig stream))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Mon Apr 3 21:18:47 2017 -- got to get the actual and normal tuplet data
;;; from the sum of the events under the tuplet bracket :/
;;;
;;; This is my current understanding of the situation regarding (esp. nested)
;;; tuplets:  I assumed, quite reasonably I think, that tuplet brackets would
;;; start when I write a <tuplet type="start" ...> tag and end when I write a
;;; <tuplet type="stop">. However, in finale, sibelius, and dorico, that is
;;; definitely not the case. Instead it's the <tuplet-actual> and
;;; <tuplet-normal> tags which determine where brackets stop. So setting the
;;; <tuplet-number> and <tuplet-type> correctly is absolutely essential. Sadly,
;;; we can't do this from existing rhythm slots. We have to look at all the
;;; rhythms under a tuplet bracket in order to determine their duration sum,
;;; then work out what e.g. 13/12 means in the context, 13 in the time of 12
;;; 1/16ths or 1/8ths or what? That's what we do here. 
(defmethod tuplet-actual-normals ((rsb rthm-seq-bar))
  ;; MDE Fri Apr 7 10:47:26 2017 -- could be that the rsb's tuplet slot got
  ;; messed up so recreate if nil
  (unless (tuplets rsb)
    (recreate-tuplets rsb))
  (let ((eut (get-events-under-tuplets rsb)))
    (loop for tuplet in (tuplets rsb)
       for tevents in eut
       for tdur = (loop for e in tevents sum (duration e))
       for ratio = (get-tuplet-ratio (first tuplet))
       collect (jiggle-tuplet-actual-normal
                tdur (denominator ratio) (numerator ratio)                
                (* 4 (/ (numerator ratio) tdur))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Fri Mar 17 14:42:00 2017
;;; spec says "If maximum compatibility with Standard MIDI 1.0 files is
;;; important, do not have the divisions value exceed 16383" (divisions per
;;; quarter note)
;;; 
;;; MDE Wed Jan 10 17:17:56 2018 -- however, this odd number creates xml import
;;; problems with Dorico (not Finale!) when we have a a 5/8 rest bar as that's
;;; 2.5 quarter notes and 2.5 * 16383 is 40957.5. XML durations should be
;;; integers however, hence we use 16382 and all is well. At some point though
;;; it might be worth investigating setting the best number of divisions based
;;; on the actual rhythms in a bar and not just setting for all and sundry
;;; bars/pieces as we do now when we call this method via the slippery chicken
;;; class (and friends).
(defmethod write-xml ((rsb rthm-seq-bar)
                      &key stream (starting-clef 'treble) (divisions 16382)
                        ;; list of semitones and diatonic transpositions; this
                        ;; will be nil if the instrument hasn't changed
                        ;; from the previous bar
                        transposition
                        ;; if the last bar had a begin-end repeat barline, pass
                        ;; T here
                        start-repeat)
  (format stream "~&    <!-- ~a =========================================== -->"
          (player rsb))
  (format stream "~&    <measure number=\"~a\">"
          (bar-num rsb))
  (when start-repeat
    (xml-barline 3 stream "left" "forward"))
  ;; (fix-tuplets-for-xml rsb)
  (let ((ts (get-time-sig rsb)))
    (format stream "~&      <attributes>")
    (when (= (bar-num rsb) 1)
      (format stream "~&        <divisions>~a</divisions>" divisions)
      ;; todo: sc can have a key-sig but we're not writing one for now
      (format stream "~&        <key><fifths>0</fifths></key>"))
    ;; strange: time sig has to come before clef in Finale otherwise you get a
    ;; bogus error message
    (when (write-time-sig rsb)
      (xml-time-sig rsb stream ts))
    (when (= (bar-num rsb) 1)
      (xml-clef starting-clef stream))
    ;; if transposing instrument at bar1 or changes to transp ins later. this
    ;; is separate from change of instrument as text and midi programme (comes
    ;; below) 
    (when transposition
      ;; what a drag: finale _demands_ diatonic then chromatic...why?
      (format stream "~&        <transpose>~
                      ~&          <diatonic>~a</diatonic>~
                      ~&          <chromatic>~a</chromatic>~
                      ~&          <octave-change>~a</octave-change>~
                      ~&        </transpose>"
              (first transposition) (third transposition)
              (second transposition)))
    (format stream "~&      </attributes>")
    ;; is this a rest bar? what about the show-rest slot?
    (if (is-rest-bar rsb)
        (let* ((e1 (first (rhythms rsb))))
          (when (display-tempo e1)
            (unless (tempo-change e1)
              (error "rthm-seq-bar::write-xml: display tempo but no ~
                      tempo-change: ~a" rsb))
            (write-xml (tempo-change e1) :stream stream))
          (write-xml-ins-change e1 stream (player rsb))
          (xml-write-marks (marks-before e1) stream)
          (xml-whole-bar-rest ts divisions stream)
          (xml-write-marks (marks e1) stream))
        ;; not a rest bar ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (loop with tans = (tuplet-actual-normals rsb) ; see method notes above
           for event in (rhythms rsb) do
             (write-xml event :stream stream :divisions divisions
                        :tuplet-actual-normals tans)))
    ;; attach the given rehearsal letter
    (when (rehearsal-letter rsb)
      (xml-rehearsal stream (rehearsal-letter rsb)))
    (xml-barline (bar-line-type rsb) stream)
    (format stream "~&    </measure>")
    ;; return whether or not to open next bar with a start-repeat barline
    (member (bar-line-type rsb) '(3 4))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-lp-data ((rsb rthm-seq-bar) &optional
                        in-c (rehearsal-letters-font-size 18)
                        ;; MDE Sat Mar 10 16:55:31 2012 
                                             process-event-fun)
  ;; 25.7.11 (Pula): why consolidate here?
  ;; (consolidate-rests rsb)
  (let ((result '())
        (ts (get-time-sig rsb)))
    ;; time signature
    (when (write-time-sig rsb)
      (push (lp-time-sig rsb) result))
    ;; is this a rest bar? what about the show-rest slot?
    (if (is-rest-bar rsb)
        (let* ((e1 (first (rhythms rsb))))
          (when (display-tempo e1)
            (unless (tempo-change e1)
              (error "rthm-seq-bar::get-lp-data: display tempo but no ~
                      tempo-change: ~a" rsb))
            (push (get-lp-data (tempo-change e1)) result))
          ;; MDE Mon Jul 23 14:01:44 2012 -- 
          (loop for s in (lp-get-ins-change e1) do (push s result))
          ;; MDE Sat Sep 22 16:01:03 2012 -- e.g. key sigs on rest bars
          (when (marks-before e1)
            (loop for m in (marks-before e1)
               for lpm = (lp-get-mark m)
               do
                 (push lpm result)))
          (push (lp-rest-bar rsb ts) result)
          (when (marks e1)
            (loop for m in (marks e1)
               ;; lilypond has a special fermata markup for rest bars...
               for lpm = (if (eq m 'pause)
                             "^\\fermataMarkup"
                             (lp-get-mark m))
               do
               (push lpm result))))
        ;; not a rest bar
        (loop for event in (rhythms rsb) do
           ;; MDE Sat Mar 10 17:03:07 2012 
             (when process-event-fun
               (funcall process-event-fun event))
             (push (get-lp-data event in-c) result)))
    ;; attach the given rehearsal letter
    (when (rehearsal-letter rsb)
      (push (lp-rehearsal-letter rsb rehearsal-letters-font-size) result))
    ;; special bar line
    (push (case (bar-line-type rsb)
            (0 " | ") ; single
            (1 " \\bar \"||\" ") ; double
            (2 " \\bar \"|.\" ") ; end of piece double
            ;; MDE Wed Mar 21 07:44:10 2012 -- added repeat barlines
            (3 " \\bar \"|:\" ")        ; begin repeat
            (4 " \\bar \":|.|:\" ")     ; begin & end repeat 
            (5 " \\bar \":|\" ")        ; end repeat
            (t (error "rthm-seq-bar::get-lsp-data: ~
                       unhandled barline at bar ~a: ~a"
                      (bar-num rsb) (bar-line-type rsb))))
          result)
    (nreverse result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod lp-rehearsal-letter ((rsb rthm-seq-bar) font-size)
  (if (eq t (rehearsal-letter rsb)) ; auto-increment
      "\\mark \\default"
      (format nil "\\mark \\markup{ \\box \\bold \\abs-fontsize #~a ~a }" 
              font-size (rehearsal-letter rsb))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod lp-rest-bar ((rsb rthm-seq-bar) &optional time-sig) ; if we have it
  (unless time-sig 
    (setf time-sig (get-time-sig rsb)))
  (if (eq t (multi-bar-rest rsb))
      nil
      (format nil "R1*~a/~a~a" (num time-sig) (denom time-sig) 
              (if (numberp (multi-bar-rest rsb))
                  (format nil "*~a" (multi-bar-rest rsb))
                  ""))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod lp-time-sig ((rsb rthm-seq-bar) &optional time-sig) ; if we have it
  (unless time-sig 
    (setf time-sig (get-time-sig rsb)))
  (format nil "\\time ~a/~a " (num time-sig) (denom time-sig)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+cmn
(defmethod get-cmn-data ((rsb rthm-seq-bar) &optional process-event-fun in-c
                         display-marks-in-part display-time
                         ignore1 ignore2 ignore3 ignore4)
  (declare (ignore ignore1 ignore2 ignore3 ignore4))
  ;; MDE Sat Sep 22 16:27:21 2012 
  (flet ((process-marks-before (event result-list)
           (loop for o in (marks-before event)
              ;; a clef change appears as a 2-element list e.g. (clef treble)
              for cmn-o = (if (and (listp o)
                                   (equal (first o) 'clef))
                              (cmn::cmn-get-clef (second o))
                              (cmn::get-cmn-marks o))
              do 
              (if (listp cmn-o) 
                  (loop for co in cmn-o do (push co result-list))
                  (push cmn-o result-list)))
           result-list))
    ;; 4/4/06: don't do this here anymore, rather do it in sc::respell-notes so
    ;; that we can respell notes. 
    ;; (auto-accidentals rsb)
    ;; bar nums are written over the bar line of this bar, so it's actually the
    ;; next bar, if you see what I mean  
    (let* ((e1 (get-nth-event 0 rsb))
           (bar-num (1+ (bar-num rsb)))
           (mbr (numberp (multi-bar-rest rsb)))
           (e1ic (first (instrument-change e1)))
           (result 
            (if (is-rest-bar rsb)
                (list 
                 (if (missing-duration rsb)
                     (apply #'cmn::rest 
                            (append 
                             ;; 1.3.11 got to turn the mark symbols into cmn
                             ;; marks. can use e1 instead of
                             ;; (get-nth-event 0 rsb))
                             (cmn::get-all-cmn-marks (marks e1))
                             ;; MDE Mon Jul 23 13:33:36 2012 -- 
                             (when e1ic
                               (list (cmn::new-staff-name e1ic)
                                     (cmn::sc-cmn-text e1ic)))
                             (list 
                              (cmn::rq
                               (- (rationalize
                                   (bar-qtr-duration rsb))
                                  (missing-duration rsb))))
                             (list cmn::invisible)))
                     ;; if it's under a multi-bar-rest it's skipped in the
                     ;; sequenz class
                     (apply (if mbr #'cmn::measure-rest
                                #'cmn::whole-measure-rest)
                            (append 
                             (cmn::get-all-cmn-marks (marks e1))
                             (when (display-tempo e1)
                               ;; this is now a list
                               (cmn-tempo (tempo-change e1)))
                             ;; MDE Mon Jul 23 13:33:36 2012 -- 
                             (when e1ic
                               (list (cmn::new-staff-name e1ic)
                                     (cmn::sc-cmn-text e1ic)))
                             (list (cmn::dots 0)
                                   ;; 3/4/07 not here anymore
                                   ;; (when (write-bar-num rsb)
                                   ;; (cmn::cmn-bar-number (bar-num rsb)))
                                   (when mbr
                                     (multi-bar-rest rsb))
                                   (unless (show-rest rsb)
                                     cmn::invisible)))))
                 ;; MDE Wed Apr 18 08:33:02 2012 -- only write bar num if
                 ;; this instrument is supposed to. 2nd arg was t.
                 (cmn::cmn-bar-line bar-num (write-bar-num rsb)
                                    (bar-line-type rsb)
                                    (rehearsal-letter rsb)))
                ;; not a rest bar
                (econs 
                 (loop for event in (rhythms rsb)
                    with first = t
                    with result
                    with rqq
                    with pitches
                    with rqqn
                    with rqq-get-events
                    for bnum = (when (and first
                                          ;; 3/4/07: bar-nums are added to the
                                          ;; bar-line not the event so make
                                          ;; sure bnum is nil here
                                          nil
                                          (write-bar-num rsb))
                                 (bar-num rsb))
                    do
                    (when (marks-before event)
                      (setf result (process-marks-before event result)))
                    (setf rqqn (rqq-info event))
                    ;; catch grace note pitches that come just before an rqq.
                    (when (and rqq-get-events
                               (zerop rqq-get-events))
                      (setf result
                            (append
                             (handle-rqq rqq (reverse pitches) bnum 
                                         (rhythms rsb) process-event-fun in-c)
                             result))
                      (setf pitches nil
                            rqq-get-events nil
                            rqq nil))
                    (when (and (not rqq) 
                               (is-grace-note event))
                      (push (pitch-or-chord event) pitches))
                    ;; at the first sight of a normal note, get rid of any
                    ;; grace notes we saw.
                    (when (and (not rqqn) 
                               (not (is-grace-note event))
                               (not (is-rest event)))
                      (setf pitches nil))
                    ;; here's the start of an rqq
                    (when (and rqqn (listp rqqn))
                      (setf rqq rqqn
                            rqq-get-events (nth-value 2 (do-rqq rqq)))
                      (unless (numberp rqq-get-events)
                        (error "rthm-seq-bar::get-cmn-data: ~
                                            rqq-get-events = ~a"
                               rqq-get-events))
                      ;; minus the number of grace notes
                      (decf rqq-get-events (length pitches)))
                    (when (and rqq-get-events
                               (> rqq-get-events 0))
                      (decf rqq-get-events)
                      (unless (is-rest event)
                        (push (pitch-or-chord event) pitches)))
                    ;; these are notes not created by rqq!!!!!!!!
                    (unless rqq
                      (if (is-grace-note event) 
                          ;; 21/4/10 display time on grace note or note
                          (get-cmn-data event bnum nil process-event-fun in-c
                                        display-marks-in-part 
                                        (and first display-time))
                          ;; here we put the bar num in as text
                          (push (get-cmn-data event bnum nil process-event-fun
                                              in-c display-marks-in-part 
                                              (and first display-time))
                                result)))
                    (setf first nil)
                    finally 
                    (when rqq
                      (setf result
                            (append (handle-rqq
                                     rqq (reverse pitches) bnum (rhythms rsb)
                                     process-event-fun in-c)
                                    result)))
                    (return (nreverse result)))
                 ;; change second arg to t if we want real cmn bar
                 ;; nums every five bars (and above!)
                 (cmn::cmn-bar-line 
                  ;; MDE Wed Apr 18 08:33:02 2012 -- only write bar num if
                  ;; this instrument is supposed to. 2nd arg was t.
                  bar-num (write-bar-num rsb)
                  (bar-line-type rsb) (rehearsal-letter rsb))))))
      ;; MDE Sat Sep 22 16:17:47 2012 -- e.g. key sigs on a rest bar
      (when (and (is-rest-bar rsb) (marks-before e1))
        (setf result (process-marks-before e1 result)))
      (when (write-time-sig rsb)
        (push (cmn::meter (get-time-sig-as-list rsb)) result))
      result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Try and work out which notes in the bar need accidentals and which don't.
;;; MDE Fri Apr 20 14:43:54 2012 -- made cautionary-distance an optional arg
;;; instead of a let var 
(defmethod auto-accidentals ((rsb rthm-seq-bar) 
                             &optional last-attack-previous-bar 
                                       written (cautionary-distance 3))
  ;; (print 'auto)
  (let* (;(cautionary-distance 3)
         (porc nil)
         (scale (ml nil 140))) ;; enough for 20 octaves!
    ;; go through each note in the bar and see whether there was already an
    ;; accidental for this white-degree; if not, we need the accidental so do
    ;; nothing unless it's a natural (show-accidental slot is t per default);
    ;; if so, and it's the same accidental as this one, then don't show the
    ;; accidental, or if it's more than cautionary-distance notes back, show
    ;; the accidental in parentheses; if the accidental is different, leave the
    ;; pitch alone.  Then store in the nth white-degree slot of scale a
    ;; 2-element list: the note number for this bar and the accidental.  Chords
    ;; will store exactly like single notes except the note number will be the
    ;; same for each note in the chord.
    (flet ((aa-aux (pitch note-pos)
             (let* ((wd (white-degree pitch))
                    (acc (accidental pitch))
                    (last (nth wd scale))
                    (last-pos (if last (first last) -1)) ; -1 to avoid warning
                    (last-acc (if last (second last) -1)))
               (if last
                   (when (eq acc last-acc)
                     (if (> (- note-pos last-pos) cautionary-distance)
                         (if (and (natural pitch)
                                  (not (accidental-in-parentheses pitch)))
                             (setf (show-accidental pitch) nil)
                           (setf (accidental-in-parentheses pitch) t))
                       ;; repeated note within cautionary-distance: don't show
                       ;; accidental 
                       (unless (accidental-in-parentheses pitch)
                         (setf (show-accidental pitch) nil)))
                     ;; when the accidental is different, do nothing, it will
                     ;; be shown
                     )
                 ;; first time this white-degree appears in the bar
                 (when (and (natural pitch)
                            (not (accidental-in-parentheses pitch)))
                   (setf (show-accidental pitch) nil)
                   ;; otherwise accidental will be shown
                   ))
               ;; store details of this pitch
               (setf (nth wd scale) (list note-pos acc)))))
      (loop 
          with last-porc = (when last-attack-previous-bar
                             (if written
                                 (written-pitch-or-chord 
                                  last-attack-previous-bar)
                               (pitch-or-chord last-attack-previous-bar)))
          for event in (rhythms rsb) and i from 0 do
            (unless (event-p event)
              (error "rthm-seq-bar::auto-accidentals: ~
                      You can only call this method when the bar contains ~
                      events (not simple rhythms)."))
            (setf porc (if written
                           (written-pitch-or-chord event)
                         (pitch-or-chord event)))
            (when porc
              (if (pitch-p porc)
                  (progn
                    ;; 5/4/07: first of all reset accidental info
                    (setf (show-accidental porc) t
                          (accidental-in-parentheses porc) nil)
                    ;; 27/3/07 handle cases like ef3->en4: need cautionary on
                    ;; e4  
                    (when (and last-porc
                               (pitch-p last-porc)
                               (natural porc)
                               (not (natural last-porc))
                               (not (is-octave porc last-porc))
                               (white-octave porc last-porc))
                      ;; (print (id porc))
                      ;; (print 'hello?)
                      (setf (show-accidental porc) t
                            (accidental-in-parentheses porc) t))
                    ;; old tests...
                    (if (and (is-tied-to event)
                             last-porc
                             (pitch= porc last-porc))
                        (setf (show-accidental porc) nil)
                      (aa-aux porc i)))
                ;; it's a chord
                (loop for p in (data porc) do
                      ;; 5/4/07: first of all reset accidental info
                      (setf (show-accidental p) t
                            (accidental-in-parentheses p) nil)
                      (if (is-tied-to event)
                          (setf (show-accidental p) nil
                                (accidental-in-parentheses p) nil)
                        (progn
                          (aa-aux p i)
                          ;; 27/3/07: cautionaries in chords too!
                          (when (and (natural p)
                                     (not (show-accidental p))
                                     (also-different-inflection porc p))     
                            ;; (format t "~&cautionary on ~a in chord, bar ~a"
                            ;;     (id p) (bar-num rsb))
                            (setf (show-accidental p) t
                                  (accidental-in-parentheses p) t))))))
              (setf last-porc porc)))))
  ;; 28/2/07 handle the first note of the bar adding a cautionary if the last
  ;; notes of the previous bar was the same white note but different
  ;; inflection 
  (let* ((first-attack (get-nth-attack 0 rsb nil))
         (second-attack (get-nth-attack 1 rsb nil))
         (fap (when first-attack 
                (if written
                    (written-pitch-or-chord first-attack)
                  (pitch-or-chord first-attack))))
         ;; test second attack as well as first
         (sap (when second-attack 
                (if written
                    (written-pitch-or-chord second-attack)
                  (pitch-or-chord second-attack))))
         ;; we just need the pitch (or chord) object of the last attack, not
         ;; the event/rhythm  
         (lapbp (when last-attack-previous-bar
                  (if written
                      (written-pitch-or-chord last-attack-previous-bar)
                    (pitch-or-chord last-attack-previous-bar))))
         (faps (if (chord-p fap)
                   (data fap)
                 (list fap)))
         (saps (if (chord-p sap)
                   (data sap)
                 (list sap)))
         (lapbps (if (chord-p lapbp)
                     (data lapbp)
                   (list lapbp)))
         (suspects (remove-duplicates (append faps saps) 
                                      ;; have to do this so the pitch from saps
                                      ;;; is removed, not the one from faps
                                      ;;(otherwise we get a () on the second
                                      ;; note, not the first, should the same
                                      ;; note repeat at beg of bar (or could
                                      ;; append the other way round...)
                                      :from-end t
                                      :test #'(lambda (p1 p2)
                                                (when (and p1 p2)
                                                  (pitch= p1 p2 nil))))))
    ;; have to do these loops to find matches between chords
    (loop for fp in suspects do
          (loop for lp in lapbps do
                ;; (format t "~&fp ~a lp ~a" (id fp) (id lp))
                (when (and fp lp 
                           (pitch-p fp) (pitch-p lp)
                           (not (show-accidental fp))
                           (eq (white-note fp) 
                               (white-note lp))
                           (not (eq 'n (accidental lp))))
                  (setf (show-accidental fp) t
                        (accidental-in-parentheses fp) t))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sun Dec 25 21:29:01 EST 2011 SAR Added robodoc info
;;; ****m* rthm-seq-bar/transpose
;;; DESCRIPTION
;;; Transpose the pitches of event objects stored in a rthm-seq-bar object by a
;;; specified number of semitones (positive for up, negative for down).
;;;
;;; ARGUMENTS 
;;; - A rthm-seq-bar object.
;;; - A whole number (positive or negative).
;;;
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :destructively. Set to T or NIL to indicate whether the slot values of
;;;   the original rthm-seq-bar object should be changed or not (even though
;;;   the method always returns a clone). T = change the originals. 
;;;   Default = NIL.
;;; - :chord-function. A function to be used for the transposition of
;;;   chords. Default = #'transpose.
;;; - :pitch-function. A function to be used for the transposition of
;;;   pitches. Default = #'transpose.
;;; 
;;; RETURN VALUE  
;;; This method returns a clone of the rthm-seq-bar object whether the keyword
;;; argument :destructively is set to T or NIL. It does change the
;;; corresponding slot values of the original when set to T even though it
;;; returns the clone.
;;; 
;;; EXAMPLE
#|
;; Create a rthm-seq-bar object using make-event, transpose the contained ; ; ; ; ; ;
;; pitches destructively, and read the values of the corresponding slots to see ; ; ; ; ; ;
;; the change.                          ; ; ; ; ; ;
  (let ((rsb (make-rthm-seq-bar 
(list 
'(3 8) 
(make-event 'cs4 'e)
(make-event 'cs4 'e)
(make-event 'cs4 'e)))))
(transpose rsb 3 :destructively 3)
(loop for p in (rhythms rsb)
collect (data (pitch-or-chord p))))

  => (EF4 EF4 EF4)

;; Do the same thing without the :destructively keyword being set to T ; ; ; ; ; ;
  (let ((rsb (make-rthm-seq-bar 
(list
'(3 8) 
(make-event 'cs4 'e)
(make-event 'cs4 'e)
(make-event 'cs4 'e)))))
(transpose rsb 3)
(loop for p in (rhythms rsb)
collect (data (pitch-or-chord p))))

  => (C4 C4 C4)
  |#
;;; SYNOPSIS
(defmethod transpose ((rsb rthm-seq-bar) semitones
                      &key
                      ;; when t, then the events will be replaced by the
                      ;; transposition.  
                      (destructively nil)
                      ;; the default functions are the class methods for pitch
                      ;; or chord.
                      (chord-function #'transpose)
                      (pitch-function #'transpose))
;;; ****
  (let ((result (clone rsb))
        (events (loop for event in (rhythms rsb) do
                  (unless (event-p event)
                    (error "rthm-seq-bar::transpose: Rhythms slot must be a ~
                            list of events, not rhythms! ID = ~a"
                           (id rsb)))
                  collect (transpose event semitones 
                                     :chord-function chord-function
                                     :pitch-function pitch-function))))
    (setf (rhythms result) events)
    (when destructively
      (setf (rhythms rsb) (my-copy-list events)))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod count-c5ths ((rsb rthm-seq-bar) &optional written)
  (loop 
      with c5ths = 0
      with porc
      for event in (rhythms rsb) 
      do
        (when (and (event-p event)
                   (not (is-rest event)))
          (setf porc (if written
                         (written-pitch-or-chord event)
                       (pitch-or-chord event)))
          (incf c5ths
                (if (is-single-pitch event)
                    (c5ths porc)
                  (count-c5ths porc))))
      finally (return c5ths)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod count-accidentals ((rsb rthm-seq-bar) &optional written)
  (loop 
      with accidentals = 0
      with porc
      for event in (rhythms rsb) 
      do
        (when (and (event-p event)
                   (not (is-rest event)))
          (setf porc (if written
                         (written-pitch-or-chord event)
                       (pitch-or-chord event)))
          (if (is-single-pitch event)
              (when (show-accidental porc)
                (incf accidentals))
            (incf accidentals (count-accidentals porc))))
      finally (return accidentals)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Returns the event positions of enharmonics of the given pitch (counting
;;; rests too).  If octaves-too, then gs4 would match an af6 somewhere else in
;;; the bar.  If written, then look at the written pitch events of the bar.

(defmethod enharmonics-exist ((rsb rthm-seq-bar) (p pitch)
                              &optional (octaves-too t) written)
  (let ((enharmonic (enharmonic p :warn nil))) ;; don't warn
    (when (and enharmonic
               (not (pitch= p enharmonic nil))) ;; e.g. g4 d3
      (loop 
          with porc
          with result = '()
          for event in (rhythms rsb) 
          for i from 0
          do
            (when (and (event-p event)
                       (not (is-rest event)))
              (setf porc (if written
                             (written-pitch-or-chord event)
                           (pitch-or-chord event)))
              (if (is-single-pitch event)
                  (when (or (pitch= enharmonic porc nil)
                            (and octaves-too
                                 (is-octave enharmonic porc nil)))
                    (push i result))
                ;; it's a chord
                (when (chord-member porc enharmonic nil octaves-too)
                  (push i result))))
          finally (return (nreverse result))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Fri May 18 09:25:40 EDT 2012: Added robodoc entry
;;; SAR Wed May 30 21:15:47 BST 2012: Expanded robodoc entry

;;; ****m* rthm-seq-bar/respell-bar
;;; DESCRIPTION
;;; Scan the specified rthm-seq-bar object for enharmonically equivalent
;;; pitches and unify their spelling.  This method won't generally be called by
;;; the user directly, rather, it's called by the respell-bars method in the
;;; slippery-chicken class.  
;;; 
;;; Clearly, this method will not work if the rthm-seq-bar only contains rhythm
;;; objects: it's made to be called when these have been promoted to event
;;; objects during the initialization of a slippery-chicken object.
;;; 
;;; NB: Although this method focuses on just one rthm-seq-bar object, the
;;;     parent slippery-chicken object and player ID are needed in order to
;;;     determine ties that may exist into the next bar from the present bar. 
;;;
;;; NB: The slippery-chicken class version of the method of this name uses
;;;     pitches from previous bars as well when respelling a given rthm-seq-bar
;;;     object, so different results are normal. Users should generally call
;;;     the slippery-chicken method rather than calling this one directly on
;;;     individual bars.
;;; 
;;; ARGUMENTS 
;;; - A rthm-seq-bar object.
;;; - A slippery-chicken object.
;;; - A player ID (symbol).
;;;
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to indicate whether to process only written or only sounding
;;;   pitches. T = only written. Default = NIL. 
;;; - The last attack (event object) of the previous bar. This is usually
;;;   supplied by the calling method. Default = NIL.
;;; 
;;; RETURN VALUE  
;;; Returns the rthm-seq-bar object it was passed.
;;; 
;;; EXAMPLE
#|
;;; Create a slippery-chicken object using pitches GS4 and AF4, print the ; ; ; ; ; ;
;;; pitches of a specified bar within that object. Apply respell-bar and print ; ; ; ; ; ;
;;; the same pitches again to see the difference. ; ; ; ; ; ;

  (let ((mini
(make-slippery-chicken
'+mini+
:ensemble '(((vn (violin :midi-channel 1))))
:set-palette '((1 ((gs4 af4 bf4))))
:set-map '((1 (1 1 1)))
:rthm-seq-palette '((1 ((((4 4) e e e e e e e e))
:pitch-seq-palette ((1 2 1 1 1 1 1 1)))))
:rthm-seq-map '((1 ((vn (1 1 1))))))))
(print (loop for r in (rhythms (get-bar mini 2 'vn))
collect (get-pitch-symbol r)))
(respell-bar (get-bar mini 2 'vn) mini 'vn)
(print (loop for r in (rhythms (get-bar mini 2 'vn))
collect (get-pitch-symbol r))))
  =>
  (GS4 AF4 GS4 GS4 GS4 GS4 GS4 GS4) 
  (GS4 GS4 GS4 GS4 GS4 GS4 GS4 GS4)

  |#
;;; SYNOPSIS
(defmethod respell-bar ((rsb rthm-seq-bar) sc player 
                        &optional written last-attack-previous-bar)
;;; ****
  (loop 
     with p
     ;; with p-enh
     with tied
     with next-attack
     ;; with next-attack-p
     with last-attack = last-attack-previous-bar
     with last-attack-p = 
     (when (and last-attack-previous-bar
                (pitch-or-chord last-attack-previous-bar))
       (if written
           (written-pitch-or-chord last-attack-previous-bar)
           (pitch-or-chord last-attack-previous-bar)))
     with attack-num = -1
     for event-num from 0
     for e in (rhythms rsb) 
     do
     (unless (event-p e)
       (error "~a~&rthm-seq-bar::respell-bar: bar must contain events!"
              rsb))
     (when (needs-new-note e)
       (incf attack-num)
       ;; don't bother with chords, they've already been spelled as well as 
       ;; they could be
       (when (is-single-pitch e)
         (setf p (if written
                     (written-pitch-or-chord e)
                     (pitch-or-chord e))
               ;; p-enh (enharmonic p nil)
               next-attack (get-nth-attack (1+ attack-num) rsb nil))
           ;; 9.2.11
           (unless p
             (error "rthm-seq-bar::respell-bar: expected a pitch in this ~
                     bar's events but one of them ~%was nil--it could be ~
                     that you've created your own events and forgotten to ~&~
                     assign both written and sounding pitches for notes ~
                     played on a transposing ~%instrument: ~%~a!" rsb))
           (when (enharmonics-exist rsb p t written)
             (enharmonic e :written written)
             ;; don't need p as it was anymore so get the (enharmonic) pitch
             (setf p (if written
                         (written-pitch-or-chord e)
                         (pitch-or-chord e)))
             (when next-attack
               ;; make sure we haven't just messed up the spelling of this and 
               ;; the next note; the new spelling (if any) will be picked up
               ;; in the next loop after we've done this (ie changes to lists
               ;; that we're looping through are reflected in the loop)
               (respell e next-attack written t))
             (when (and (pitch-p last-attack-p)
                        (bad-interval p last-attack-p))
               (enharmonic last-attack))
             ;; now for the part that could go beyond our current bar
             (when (is-tied-from e)
               ;; find current bar (this sets vars but doesn't actually get an 
               ;; event!) 
               (next-event sc player nil (bar-num rsb))
               ;; loop along to the note after the present one
               (loop repeat (+ 2 event-num) do
                    (setf tied (next-event sc player)))
               (unless (is-tied-to tied)
                 (error "~a~&rthm-seq-bar::respell-bar: that event should ~
                          be tied-to!" tied))
               (loop 
                  for clone = (clone p)
                  while (is-tied-to tied) 
                  do
                    (if written
                        (setf (written-pitch-or-chord tied) clone)
                        (setf (pitch-or-chord tied) clone))
                    (setf tied (next-event sc player))))))
         (setf last-attack e
               last-attack-p (if written
                                 (written-pitch-or-chord e)
                                 (pitch-or-chord e)))))
  rsb)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Mon Dec 26 11:52:16 EST 2011: Added robodoc info
;;; ****m* rthm-seq-bar/enharmonic
;;; DESCRIPTION
;;; Change the pitches of the events within a given rthm-seq-bar object to
;;; their enharmonic equivalents. 
;;; 
;;; In its default form, this method only applies to note names that already
;;; contain an indication for an accidental (such as DF4 or BS3), while
;;; "white-key" note names (such as B3 or C4) will not produce an enharmonic
;;; equivalent. In order to change white-key pitches to their enharmonic
;;; equivalents, set the :force-naturals argument to T. 
;;;
;;; ARGUMENTS 
;;; - A rthm-seq-bar object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :written. T or NIL to indicate whether the test is to handle the
;;;   written or sounding pitch in the event. T = written. 
;;;   Default = NIL.
;;; - :force-naturals. T or NIL to indicate whether to force "natural"
;;;   note names that contain no F or S in their name to convert to
;;;   their enharmonic equivalent (e.g. B3 = CF4). Default = NIL.
;;; - :pitches. All sharp/flat pitches are changed by default but if a
;;;   list of pitch objects or symbols is given, then only those
;;;   pitches will be changed.  Note that if written is T, then this
;;;   pitch list should be the written not sounding pitches.  
;;;   Default = NIL.
;;; 
;;; RETURN VALUE  
;;; Always returns T.
;;; 
;;; EXAMPLE
#|
;; The method returns T.                ; ; ; ; ;
  (let ((rsb (make-rthm-seq-bar 
(list '(3 8) 
(make-event 'cs4 'e)
(make-event 'cs4 'e)
(make-event 'cs4 'e)))))
(enharmonic rsb))

  => T

;; Create a rthm-seq-bar object with events, apply the enharmonic method, and ; ; ; ; ;
;; print the corresponding slots to see the changes ; ; ; ; ; ;
  (let ((rsb (make-rthm-seq-bar 
(list
'(3 8) 
(make-event 'cs4 'e)
(make-event 'cs4 'e)
(make-event 'cs4 'e))))) 
(enharmonic rsb)
(loop for p in (rhythms rsb)
collect (get-pitch-symbol p)))

  => (DF4 DF4 DF4)

;; By default, the method will not change white-key pitches ; ; ; ; ;
  (let ((rsb (make-rthm-seq-bar 
(list
'(3 8) 
(make-event 'c4 'e)
(make-event 'c4 'e)
(make-event 'c4 'e)))))
(enharmonic rsb)
(loop for p in (rhythms rsb)
collect (get-pitch-symbol p)))

  => (C4 C4 C4)

;; This can be forced by setting the :force-naturals argument to T ; ; ; ; ;
  (let ((rsb (make-rthm-seq-bar 
(list
'(3 8) 
(make-event 'c4 'e)
(make-event 'c4 'e)
(make-event 'c4 'e)))))
(enharmonic rsb :force-naturals t)
(loop for p in (rhythms rsb)
collect (get-pitch-symbol p)))

  => (BS3 BS3 BS3)

;; Apply the set-written method to fill the WRITTEN-PITCH-OR-CHORD slot, print ; ; ; ; ;
;; its contents, apply the enharmonic method with the :written keyword argument ; ; ; ; ;
;; set to T, then print the pitch data of the same slot again to see the ; ; ; ; ;
;; change.                              ; ; ; ; ;
  (let ((rsb (make-rthm-seq-bar 
(list
'(3 8) 
(make-event 'cs4 'e)
(make-event 'cs4 'e)
(make-event 'cs4 'e)))))
(set-written rsb -3)
(print (loop for p in (rhythms rsb)
collect (get-pitch-symbol p)))
(enharmonic rsb :written t)
(print (loop for p in (rhythms rsb)
collect (get-pitch-symbol p))))

  =>
  (BF3 BF3 BF3) 
  (AS3 AS3 AS3)

  |#
;;; SYNOPSIS
(defmethod enharmonic ((rsb rthm-seq-bar) &key written force-naturals
                       ;; MDE Wed Apr 18 11:34:01 2012
                       pitches)
;;; ****                                
  (setf pitches (init-pitch-list pitches))
  (loop for r in (rhythms rsb) do
     ;; MDE Mon Apr 23 13:21:16 2012 -- handle chords too 
       (when (and (event-p r) (is-chord r))
         (loop for p in (data (if written
                                  (written-pitch-or-chord r)
                                  (pitch-or-chord r)))
            and chord-note-ref from 1
            do
            (when 
                (or (not pitches)
                    ;; enharmonics not equal! 
                    (pitch-member p pitches nil))
              (enharmonic r :written written
                          :chord-note-ref chord-note-ref))))
     ;; MDE Wed Apr 18 11:35:49 2012 -- 
       (when (and (event-p r)
                  (is-single-pitch r)
                  (or (not pitches)
                      (pitch-member (if written
                                        (written-pitch-or-chord r)
                                        (pitch-or-chord r))
                                    pitches
                                    ;; enharmonics not equal! 
                                    nil)))
         (enharmonic r :written written :force-naturals force-naturals)))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; If the first rest in the bar is longer than the event, stick the event at
;;; the beginning and fill up the duration of the original rest with new
;;; rests. 
;;;
;;; NB this doesn't update time slots etc. so need to call update-slots
;;;    elsewhere. 

(defmethod replace-first-event ((rsb rthm-seq-bar) event)
  (let* ((first-rest (first (rhythms rsb)))
         (rem (- (duration first-rest) (duration event)))
         new-events)
    (when (> (duration event) (duration first-rest))
      (error "rthm-seq-bar::replace-first-event: first rest (~a) is too
              short at bar number ~a" (data first-rest )(bar-num rsb)))
    (unless (zerop rem)
      (setf new-events (rationalize-if-necessary rem :keep-it-simple t)))
    (push event new-events)
    ;; this calls gen-stats automatically
    (setf (rhythms rsb) (append new-events (rest (rhythms rsb))))
    t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
;;; MDE Thu Apr 19 11:32:37 2012 -- this was defined twice! ; ; ; ; ; ;
  (defmethod rhythms-to-events ((rsb rthm-seq-bar))
(setf (rhythms rsb)
(loop for r in (rhythms rsb) collect (clone-with-new-class r 'event)))
rsb)
  |#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod split-longer-rests ((rsb rthm-seq-bar))
  (let* ((rthms (my-copy-list (rhythms rsb)))
         ;; this will return e.g. e. as a rhythm object if 6/16
         (beat (get-beat-as-rhythm (get-time-sig rsb) t))
         (mult 0)
         (new-rthms '()))
    (setf (is-rest beat) t)
    (loop for r in rthms do
         (if (and (is-rest r)
                  (multiple-value-bind
                        (bool m)
                      (is-multiple r beat)
                    (setf mult m)
                    bool))
             (loop repeat mult do (push (clone beat) new-rthms))
             (push r new-rthms)))
    (setf (rhythms rsb) (nreverse new-rthms))
    rsb))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 27.1.11

;;; Wed Dec 14 18:04:01 GMT 2011 SAR: Added robodoc info
;;; SAR Sat Dec 31 08:58:52 EST 2011: Added DATE back to robodoc info

;;; ****m* rthm-seq-bar/split
;;; DATE
;;; 27 Jan 2011
;;;
;;; DESCRIPTION
;;; Splits a given rthm-seq-bar into multiple smaller rthm-seq-bar
;;; objects. This will only work if the given rthm-seq-bar object can be split
;;; into whole beats; e.g. a 4/4 bar will not be split into 5/8 + 3/8. 
;;;
;;; The keyword arguments :min-beats and :max-beats serve as guidelines rather
;;; than strict cut-offs. In some cases, the method may only be able to
;;; effectively split the given rthm-seq-bar by dividing it into segments that
;;; exceed the length stipulated by these arguments (see example below). 
;;;
;;; Depending on the min-beats/max-beats arguments stipulated by the user or
;;; the rhythmic structure of the given rthm-seq-bar object, the given
;;; rthm-seq-bar may not be splittable, in which case NIL is returned. If the
;;; keyword argument :warn is set to T, a warning will be also be printed in
;;; such cases.
;;;
;;; NB The method does not copy over and update bar start-times (this is meant
;;; to be done at the rthm-seq stage, not once the whole piece has been
;;; generated). 
;;;
;;; ARGUMENTS 
;;; - A rthm-seq-bar object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :min-beats. This argument takes an integer value to indicate the minimum
;;;   number of beats in any of the new rthm-seq-bar objects created. This
;;;   serves as a guideline only and may occasionally be exceeded in value by
;;;   the method. Default value = 2.
;;; - :max-beats. This argument takes an integer value to indicate the maximum
;;;   number of beats in any of the new rthm-seq-bar objects created. This
;;;   serves as a guideline only and may occasionally be exceeded in value by
;;;   the method. Default value = 5.
;;; - :warn. Indicates whether to print a warning if the rthm-seq-bar object is
;;;   unsplittable. Value T = print a warning. Defaults to NIL.
;;; 
;;; RETURN VALUE  
;;; Returns a list of rthm-seq-bar objects if successful, NIL if not. 
;;; 
;;; EXAMPLE
#|
  (let* ((rsb (make-rthm-seq-bar '((7 4) h. e e +e. e. e q)))
(rsb-splt (split rsb)))
(loop for i in rsb-splt collect
(loop for r in (rhythms i) collect (data r))))

  => ((H.) (E E "E." E. E Q))

  (let* ((rsb (make-rthm-seq-bar '((7 4) h. e e +e. e. e q)))
(rsb-splt (split rsb)))
(loop for i in rsb-splt do (print-simple i)))

  =>
  (3 4): note H., 
  (4 4): note E, note E, note E., note E., note E, note Q,

  (let* ((rsb (make-rthm-seq-bar '((7 4) h. e e +e. e. e q)))
(rsb-splt (split rsb :min-beats 1 :max-beats 3)))
(loop for i in rsb-splt do (print-simple i)))

  =>
  (3 4): note H., 
  (1 4): note E, note E, 
  (2 4): note E., note E., note E, 
  (1 4): note Q, 

  (let ((rsb (make-rthm-seq-bar '((7 4) h. e e +e. e. e q))))
(split rsb :max-beats 1 :warn t))

  => NIL
  WARNING: rthm-seq-bar::split: couldn't split bar:

  |#
;;; SYNOPSIS
(defmethod split ((rsb rthm-seq-bar) &key
                                       (min-beats 2) (max-beats 5) warn ignore)
;;; ****
  (declare (ignore ignore))
  (let* ((ts (get-time-sig rsb))
         (num-mult (if (compound ts) 3 1))
         (num (num ts))
         (denom (denom ts))
         ;; we do split up rests which a multiples of the beat
         (rthms (my-copy-list (rhythms (split-longer-rests rsb))))
         (new-bars '()))
    ;; first get the new meter structure
    (loop with got-bar for i below 1000 until (zerop num) do
         (when (= i 999)
           (error "rthm-seq-bar::split: tried 1000 times on ~a" rsb))
         (setf got-bar nil)
         (loop for beats from min-beats to max-beats ;; by num-mult
            for this-num = (* num-mult beats)
            for bar = (make-rest-bar (list this-num denom) nil)
            for ate = (fill-with-rhythms
                       ;; no warning
                       bar rthms :new-id "bar-created-by-split" :warn nil 
                       :is-full-error nil)
            do
            ;; (print ate)
            ;; (print-simple bar)
            ;; MDE Tue May 24 12:59:45 2016 -- fill-with-rhythms will now
            ;; underfill bars so we need to explicitly check for a full bar 
              (when (and ate (is-full bar nil))
                ;; lop off the eaten rhythms and proceed
                (setf got-bar t
                      rthms (nthcdr ate rthms))
                (decf num this-num)
                (push bar new-bars)
                (return)))
         (unless got-bar
           (return)))
    ;; it could be that we're left with 1 beat over so stuff that in the last
    ;; bar 
    (when (and new-bars (= 1 (/ num num-mult)))
      ;; MDE Wed Dec 14 18:31:25 2011 -- we could warn if the extra beat will
      ;; make a bar > max-beats, but not now.
      (let* ((last-bar (first new-bars)) ; we're pushing
             (last-bar-ts (get-time-sig last-bar)))
        (setf (rhythms last-bar) (append (rhythms last-bar) rthms)
              (time-sig last-bar) (list (+ num-mult (num last-bar-ts))
                                        (denom last-bar-ts)))
        (decf num num-mult)))
    (if (zerop num)                     ; success
        (progn
          (loop for bar in new-bars do 
             ;; MDE Sat Jun  9 15:40:35 2012 
               (check-beams bar :on-fail nil :auto-beam t)
               (gen-stats bar)
             ;; MDE Tue May 24 14:09:34 2016
               ;; (print bar)
               (when (and (is-rest-bar bar) (not (rhythms bar)))
                 (setq bar (force-rest-bar bar))))
          (nreverse new-bars))
        (progn
          (when warn
            (warn "rthm-seq-bar::split: couldn't split bar:~%~a" rsb))
          nil))))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; SAR Mon Dec 26 12:41:17 EST 2011: Added robodoc info
;;;
;;; ****m* rthm-seq-bar/set-written
;;; DATE
;;; 20 Jul 2011 (Pula)
;;; 
;;; DESCRIPTION
;;; Set the written pitch (as opposed to sounding; i.e., for transposing
;;; instruments) of an event object within a given rthm-seq-bar object. The
;;; sounding pitch remains unchanged as a pitch object in the PITCH-OR-CHORD
;;; slot, while the written pitch is added as a pitch object to the
;;; WRITTEN-PITCH-OR-CHORD slot. 
;;; 
;;; ARGUMENTS 
;;; - A rthm-seq-bar-object
;;; - A number (positive or negative) indicating the transposition by
;;;   semitones.  See this method in the event class for more information and
;;;   examples.  
;;; 
;;; RETURN VALUE  
;;; Always returns T.
;;; 
;;; EXAMPLE
#|
;; The method returns NIL               ; ; ; ; ;
  (let ((rsb (make-rthm-seq-bar 
(list
'(3 8) 
(make-event 'cs4 'e)
(make-event 'cs4 'e)
(make-event 'cs4 'e)))))
(set-written rsb -2))

  => T

;; Set the written pitch transposition to 2 semitones lower, then check the ; ; ; ; ;
;; data of the WRITTEN-PITCH-OR-CHORD slot of each event to see the ; ; ; ; ;
;; corresponding pitches                ; ; ; ; ;
  (let ((rsb (make-rthm-seq-bar 
(list
'(3 8) 
(make-event 'cs4 'e)
(make-event 'cs4 'e)
(make-event 'cs4 'e)))))
(set-written rsb -2)
(loop for p in (rhythms rsb)
collect (get-pitch-symbol p)))

  => (B3 B3 B3)

  |#
;;; SYNOPSIS
(defmethod set-written ((rsb rthm-seq-bar) transposition)
;;; ****
  (loop for event in (rhythms rsb) do
       (set-written event transposition))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Mon Dec 26 14:11:05 EST 2011 Added robodoc info
;;; ****m* rthm-seq-bar/delete-written
;;; DESCRIPTION
;;; Delete the contents of the WRITTEN-PITCH-OR-CHORD slot of a pitch object
;;; within a given event object and reset to NIL.
;;; 
;;; ARGUMENTS 
;;; - A rthm-seq-bar object.
;;; 
;;; RETURN VALUE  
;;; Always returns NIL.
;;; 
;;; EXAMPLE
#|
;; Create a rthm-seq-bar object consisting of events and print the contents of ; ; ; ; ; ;
;; the WRITTEN-PITCH-OR-CHORD slots to see they're set to NIL. Apply the ; ; ; ; ; ;
;; set-written method with a value of -2 and print the contents of the ; ; ; ; ; ;
;; WRITTEN-PITCH-OR-CHORD slots to see the data of the newly created pitch ; ; ; ; ; ;
;; objects. Apply the delete-written method and print the contents of the ; ; ; ; ; ;
;; WRITTEN-PITCH-OR-CHORD slots to see they're empty. ; ; ; ; ; ;
  (let ((rsb (make-rthm-seq-bar 
(list
'(3 8) 
(make-event 'cs4 'e)
(make-event 'cs4 'e)
(make-event 'cs4 'e)))))
(print (loop for p in (rhythms rsb)
collect (written-pitch-or-chord p)))
(set-written rsb -2)
(print (loop for p in (rhythms rsb)
collect (get-pitch-symbol p)))
(delete-written rsb)
(print (loop for p in (rhythms rsb)
collect (written-pitch-or-chord p))))

  =>
  (NIL NIL NIL) 
  (B3 B3 B3) 
  (NIL NIL NIL)

  |#
;;; SYNOPSIS
(defmethod delete-written ((rsb rthm-seq-bar))
;;; ****
  (loop for event in (rhythms rsb) do
       (delete-written event)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 1-based and inclusive and counting rests and ties, not just struck notes
;;; if end-event is nil go to the end of the bar
(defmethod make-rests ((rsb rthm-seq-bar) start-event &optional end-event)
  (let ((nr (num-rhythms rsb))
        (rthms (rhythms rsb)))
    (unless end-event
      (setf end-event nr))
    (unless (and (> start-event 0)
                 (<= end-event (length rthms))
                 (<= start-event end-event))
      (error "rthm-seq-bar::make-rests: illegal start/end event (~a/~a) in ~%~a"
             start-event end-event rsb))
    (loop for e in (subseq rthms (1- start-event) end-event) do
       ;; (print (data e))
         (force-rest e)))
  (gen-stats rsb)
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Mon Dec 26 14:32:42 EST 2011: Added robodoc info
;;; ****m* rthm-seq-bar/set-midi-channel
;;; DESCRIPTION
;;; Set the MIDI-channel and microtonal MIDI-channel for the pitch object
;;; of an event object within a given rthm-seq-bar object. Sets the
;;; MIDI-CHANNEL slot of all event objects contained in the rthm-seq-bar object
;;; to the same channel.
;;; 
;;; ARGUMENTS 
;;; - A rthm-seq-bar object.
;;; - A whole number indicating the MIDI channel to be used for the
;;;   equal-tempered pitch material of the given rthm-seq-bar object. 
;;; - A whole number indicating the MIDI channel to be used for microtonal
;;;   pitch material of the given rthm-seq-bar object. 
;;; 
;;; RETURN VALUE  
;;; Always returns NIL.
;;; 
;;; EXAMPLE
#|
;; Create a rthm-seq-bar using event objects and check the MIDI-CHANNEL slots ; ; ; ; ; ;
;; of those event objects to see that they are NIL by default. ; ; ; ; ; ;
  (let ((rsb (make-rthm-seq-bar 
(list
'(3 8) 
(make-event 'cs4 'e)
(make-event 'cs4 'e)
(make-event 'cs4 'e)))))
(loop for p in (rhythms rsb)
collect (midi-channel (pitch-or-chord p))))

  => (NIL NIL NIL)

;; Apply the set-midi-channel method to the rthm-seq-bar object and read and ; ; ; ; ; ;
;; print the MIDI-CHANNEL slots of each of the individual events to see that ; ; ; ; ; ;
;; they've been set.                    ; ; ; ; ; ;
  (let ((rsb (make-rthm-seq-bar 
(list
'(3 8) 
(make-event 'cs4 'e)
(make-event 'cs4 'e)
(make-event 'cs4 'e)))))
(set-midi-channel rsb 13 14)
(loop for p in (rhythms rsb)
collect (midi-channel (pitch-or-chord p))))

  => (13 13 13)

  |#
;;; SYNOPSIS
(defmethod set-midi-channel ((rsb rthm-seq-bar) midi-channel
                             &optional microtonal-midi-channel)
;;; ****
  (loop for e in (rhythms rsb) do
       (set-midi-channel e midi-channel microtonal-midi-channel)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Mon Dec 26 14:49:27 EST 2011: Added robodoc info
;;; SAR Sat Dec 31 09:06:03 EST 2011: Added DATE block

;;; ****m* rthm-seq-bar/reset-8va
;;; DATE
;;; 22 Sep 2011 
;;; 
;;; DESCRIPTION
;;; Reset the 8VA slots of all event objects within a given rthm-seq-object to
;;; 0 (no ottava/ottava bassa transposition).
;;; 
;;; ARGUMENTS 
;;; - A rthm-seq-bar object
;;; 
;;; RETURN VALUE  
;;; Always returns NIL.
;;; 
;;; EXAMPLE
#|
;; Create a rthm-seq-bar object consisting of event objects, print the default ; ; ; ; ; ;
;; value of the 8VA slots for those events. Set the 8VA slots to 1 and print ; ; ; ; ; ;
;; the value of those slots to see the change. Apply the reset-8va method to ; ; ; ; ; ;
;; remove any values and reset the slots to NIL, and print the results. ; ; ; ; ; ;

  (let ((rsb (make-rthm-seq-bar 
(list
'(3 8) 
(make-event 'cs4 'e)
(make-event 'cs4 'e)
(make-event 'cs4 'e)))))
(print (loop for e in (rhythms rsb) collect (8va e)))
(set-8va rsb 1)
(print (loop for e in (rhythms rsb) collect (8va e)))
(reset-8va rsb)
(print (loop for e in (rhythms rsb) collect (8va e))))

  =>
  (0 0 0) 
  (1 1 1) 
  (0 0 0)

  |#
;;; SYNOPSIS
(defmethod reset-8va ((rsb rthm-seq-bar))
;;; ****
  (loop for e in (rhythms rsb) do
       ;; SAR Wed Dec 28 09:12:36 EST 2011: changed EVENTP to EVENT-P
       (if (event-p e)
           (reset-8va e)
           (error "~a~&rthm-seq-bar::reset-8va: bar must contain event ~
                   objects (not rhythms)." rsb))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Mon Dec 26 19:34:55 EST 2011: Added robodoc info
;;; SAR Sat Dec 31 09:06:51 EST 2011: Added DATE back

;;; ****m* rthm-seq-bar/set-8va
;;; DATE
;;; 23-Sep-2011 
;;; 
;;; DESCRIPTION
;;; Set the 8VA (ottava) slots of the event objects within a given rthm-seq-bar 
;;; object. This number can be positive or negative. Only the values 1, 0 and
;;; -1 are valid for the number of octaves to be transposed.
;;; 
;;; ARGUMENTS 
;;; - A rthm-seq-bar object.
;;; - A number indicating the number of octaves to be transposed in either
;;;   direction (ottava/ottava bassa). 
;;; 
;;; RETURN VALUE  
;;; Always returns NIL.
;;; 
;;; EXAMPLE
#|
;; The method returns NIL               ; ; ; ; ;

  (let ((rsb (make-rthm-seq-bar 
(list
'(3 8) 
(make-event 'cs4 'e)
(make-event 'cs4 'e)
(make-event 'cs4 'e)))))
(set-8va rsb 1))

  => NIL

;; Create a rthm-seq-bar object with event objects, set the 8va slot to 1, and ; ; ; ; ;
;; access and print it to see it's new value. ; ; ; ; ;

  (let ((rsb (make-rthm-seq-bar 
(list
'(3 8) 
(make-event 'cs4 'e)
(make-event 'cs4 'e)
(make-event 'cs4 'e)))))
(set-8va rsb 1)
(loop for e in (rhythms rsb) collect (8va e)))

  => (1 1 1)

  |#
;;; SYNOPSIS
(defmethod set-8va ((rsb rthm-seq-bar) 8va)
;;; ****
  (loop for e in (rhythms rsb) do
     ;; SAR Wed Dec 28 09:12:36 EST 2011: changed EVENTP to EVENT-P
       (if (event-p e)
           (setf (8va e) 8va)
           (error "~a~&rthm-seq-bar::set-8va: bar must contain event ~
                   objects (not rhythms)." rsb))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod no-accidental ((rsb rthm-seq-bar))
  (loop for r in (rhythms rsb) do 
       (when (event-p r)
         (no-accidental r))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Wed Mar 21 07:48:49 2012 -- added this method to ensure we only get
;;; legal bar-line-types 
(defmethod (setf bar-line-type) :before (value (rsb rthm-seq-bar))
  (unless (and (integerp value) (>= value 0) (<= value 5))
    (error "~a~&rthm-seq-bar::(setf bar-line-type): Value (~a) can only be ~
            an integer ~%between 1 and 5." rsb value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Tue May  1 18:26:30 2012 -- 

;;; ****m* rthm-seq-bar/get-rhythm-symbols
;;; DATE
;;; 01-May-2012
;;;
;;; DESCRIPTION
;;; Return the rhythms of a given rthm-seq-bar object as a list of rhythm
;;; symbols. 
;;; 
;;; ARGUMENTS
;;; - A rthm-seq-bar object.
;;; 
;;; RETURN VALUE
;;; - A list of rhythm symbols.
;;; 
;;; EXAMPLE
#|
  (let ((rsb (make-rthm-seq-bar '((4 4) q e s s q. e))))
(get-rhythm-symbols rsb))

  => (Q E S S Q. E)

  |#
;;; SYNOPSIS
(defmethod get-rhythm-symbols ((rsb rthm-seq-bar))
;;; ****
  (rhythms-as-symbols (rhythms rsb)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; MDE Mon Jun 18 16:37:04 2012 -- 
;;; ****m* rthm-seq-bar/get-pitch-symbols
;;; DESCRIPTION
;;; Return a list of the pitch symbols for the events in the bar.
;;; 
;;; ARGUMENTS
;;; - a rthm-seq-bar object
;;; 
;;; RETURN VALUE
;;; A list of pitch symbols 
;;; 
;;; EXAMPLE
#|
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((vn (violin :midi-channel 1))))
        :set-palette '((1 ((gs4 bf4 c4))))
        :set-map '((1 (1 1 1)))
        :rthm-seq-palette '((1 ((((4 4) e e e e e e e e))
                                :pitch-seq-palette ((1 2 1 2 1 1 3 1)))))
        :rthm-seq-map '((1 ((vn (1 1 1))))))))
  (get-pitch-symbols (get-bar mini 1 'vn)))
  =>
  (C4 GS4 C4 GS4 C4 C4 BF4 C4)
  |#
;;; SYNOPSIS
(defmethod get-pitch-symbols ((rsb rthm-seq-bar) &optional written)
;;; ****
  (loop for e in (rhythms rsb) collect (get-pitch-symbol e written)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Thu Jul  5 19:07:56 2012 

;;; SAR Wed Jul 18 13:28:46 BST 2012: Added robodoc entry

;;; ****m* rthm-seq-bar/set-amplitudes
;;; DESCRIPTION
;;; Add a specified amplitude (between 0.0 and 1.0) to all non-rest event
;;; objects in a specified rthm-seq-bar object.
;;; 
;;; ARGUMENTS
;;; - A rthm-seq-bar object.
;;; - A number that is an amplitude value between 0.0 and 1.0.
;;; 
;;; RETURN VALUE
;;; Returns the amplitude value set.
;;; 
;;; EXAMPLE
#|
  (let* ((mini
(make-slippery-chicken
'+sc-object+
:ensemble '(((va (viola :midi-channel 2))))
:set-palette '((1 ((c3 d3 e3 f3 g3 a3 b3 c4))))
:set-map '((1 (1 1 1)))
:rthm-seq-palette '((1 ((((4 4) e (e) e (e) (e) e e e))
:pitch-seq-palette ((1 2 3 4 5)))))
:rthm-seq-map '((1 ((va (1 1 1))))))))
(set-amplitudes (get-bar mini 2 'va) 0.9)
(cmn-display mini))

  |#
;;; SYNOPSIS
(defmethod set-amplitudes ((rsb rthm-seq-bar) amp)
;;; ****
  (loop for r in (rhythms rsb) do
       (setf (amplitude r) amp))
  amp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Thu Jul  5 19:12:54 2012 

;;; SAR Wed Jul 18 13:35:12 BST 2012: Added robodoc entry

;;; ****m* rthm-seq-bar/set-dynamics
;;; DESCRIPTION
;;; Add a specified dynamic mark to all attacked event objects (i.e. not rests
;;; and not notes that are tied to) in a specified rthm-seq-bar-object. This
;;; method was created mainly to make it easy to set amplitudes for a range of
;;; notes (e.g. with map-over-bars), so that they are, for example, reflected
;;; in MIDI velocities.  If used over many notes the score will probably then be
;;; littered with extraneous dynamic marks.  These can then be removed, if so
;;; desired, with the slippery-chicken class remove-extraneous-dynamics method.
;;; 
;;; ARGUMENTS
;;; - A rthm-seq-bar object.
;;; - A dynamic mark.
;;; 
;;; RETURN VALUE
;;; The specified dynamic mark
;;; 
;;; EXAMPLE
#|
  (let* ((mini
(make-slippery-chicken
'+sc-object+
:ensemble '(((va (viola :midi-channel 2))))
:set-palette '((1 ((c3 d3 e3 f3 g3 a3 b3 c4))))
:set-map '((1 (1 1 1)))
:rthm-seq-palette '((1 ((((4 4) e (e) e (e) (e) e e e))
:pitch-seq-palette ((1 2 3 4 5)))))
:rthm-seq-map '((1 ((va (1 1 1))))))))
(set-dynamics (get-bar mini 2 'va) 'ppp))

  => PPP

  |#
;;; SYNOPSIS
(defmethod set-dynamics ((rsb rthm-seq-bar) dynamic)
;;; ****
  (loop for r in (rhythms rsb) do
       ;; MDE Wed Jul 18 22:08:26 2012 -- only add dynamics to struck notes
       (when (needs-new-note r)
         (add-mark r dynamic)))
  dynamic)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Fri Aug 23 09:53:51 2013 
;;; for chords this will return the average degree.
(defmethod total-degrees ((rsb rthm-seq-bar))
  (loop for e in (rhythms rsb) 
     ;; MDE Wed Oct 30 19:49:10 2013 -- fixed but here that was summing tied
     ;; notes so averaging for tessitura wasn't working when
     ;; sc::update-instruments-total-duration was called.
     when (and (needs-new-note e) (event-p e))
     sum (get-degree e :average t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Sat Jun 28 14:21:50 2014 -- called by consolidate-notes
(defmethod update-events-player ((rsb rthm-seq-bar) player)
  (loop for e in (rhythms rsb) do
       (when (event-p e)
         ;; (unless (player e)
           ;; (format t "~%~a to ~a" (player e) player))
         (setf (player e) player))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Thu May 21 17:16:22 2015 
(defmethod delete-rqq-info ((rsb rthm-seq-bar))
  (loop for r in (rhythms rsb) do (setf (rqq-info r) nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* rthm-seq-bar/remove-dynamics
;;; DATE
;;; June 13th
;;; 
;;; DESCRIPTION
;;; remove all dynamics attached to events in the bar
;;; 
;;; ARGUMENTS
;;; - a rthm-seq-bar object
;;; 
;;; RETURN VALUE
;;; t
;;; SYNOPSIS
(defmethod remove-dynamics ((rsb rthm-seq-bar))
;;; ****
  (loop for e in (rhythms rsb) do (remove-dynamics e))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Wed Feb  8 09:48:17 2017 - start is 0-based
;;; ****m* rthm-seq-bar/remove-rhythms
;;; DATE
;;; February 8th 2017
;;; 
;;; DESCRIPTION
;;; remove a specified number of rhythms/events from a rthm-seq-bar object.
;;; 
;;; ARGUMENTS
;;; - the rthm-seq-bar object
;;; - the rhythm/event to start removing at (1-based)
;;; - how many rhythms/events to remove
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to indicate whether the tuplets slot of the rthm-seq-bar should
;;;   be updated to reflect changes. Default = T.
;;; - T or NIL to indicated whether the bracket slot of the starting
;;;   rhythm/event should take the value of the last rhythm/event removed
;;; 
;;; RETURN VALUE
;;; the list of rhythm/event objects from the rhythms slot of the rthm-seq-bar
;;; 
;;; SYNOPSIS
(defmethod remove-rhythms ((rsb rthm-seq-bar) start how-many
                           &optional (update-tuplets t)
                             (inherit-last-tuplet t))
;;; ****
  ;; unless inherit-last-tuplet, this assumes that any start/closing tuplets in
  ;; the rthms to be removed have already been moved to other existing or added
  ;; rthms. all we're interested in doing here is taking care of the tuplets
  ;; slot of the rthm-seq-bar itself, not its rthms
  ;; (print start) (print how-many)
  (let ((e1 (get-nth-event (1- start) rsb))
        (e2 (get-nth-event (+ start how-many -2) rsb)))
    (setf (rhythms rsb) (remove-elements (rhythms rsb) (1- start) how-many))
    (when update-tuplets
      (setf (tuplets rsb)
            (loop for tup in (tuplets rsb)
               for tstart = (second tup)
               for tend = (third tup)
               for i from 0 do
                 (when (> tstart start)
                   (decf tstart how-many))
               ;; MDE Mon Apr  3 18:37:10 2017 -- has to be >= right? (was >)
                 (when (>= tend start)
                   (decf tend how-many))
               collect (list (first tup) tstart tend))))
    (when inherit-last-tuplet
      (setf (bracket e1) (pos4neg (bracket e1) (bracket e2))))
    (check-tuplets rsb)
    (rhythms rsb)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Wed Jun 24 18:25:35 2015 -- when we've got nested tuplets we run into
;;; the problem of how many flags we need (and therefore, for Lilypond, what
;;; the letter-value slot should be). 
;; ;(on-fail #'warn))
(defmethod fix-nested-tuplets ((rsb rthm-seq-bar) &optional on-fail)
  (flet ((compound-tuplet (r)
           ;; we know when we have a triplet that the duration is 2/3 of the
           ;; letter value, but here we need to find how many tuplet brackets a
           ;; rhythm is and found what the total duration scaler will be. NB we
           ;; have the tuplet
           (loop with result = 1 with pos = (bar-pos r)
              for ts in (tuplets rsb) do
                (when (and (>= pos (second ts))
                           (<= pos (third ts)))
                  (setf result (* result (get-tuplet-ratio (first ts)))))
              finally (return result)))
         (lv (r tup)
           (setf (letter-value r) (round (* (undotted-value r) tup))
                 (num-flags r) (rthm-num-flags (letter-value r)))))
         ;;    (format t "~&lv: data ~a lv ~a"  (data r) (letter-value r))))
    (loop for r in (rhythms rsb) for ct = (compound-tuplet r) do
         (setf (tuplet-scaler r) ct)
         (lv r ct)
         (unless (power-of-2 (letter-value r))
           ;; (print r)
           ;; dots--esp. those added automatically--might screw things up,
           ;; e.g. rhythms like 70/3 might result in a letter-value of 24 which
           ;; is not representable in Lilypond. In that case we probably have
           ;; added a dot somewhere so remove it.
           (setf (num-dots r) 0
                 (undotted-value r) (value r))
           (lv r ct)
           ;; still not got it so force it 
           (unless (power-of-2 (letter-value r))
             (when on-fail
               (when (eq on-fail t)
                 (setq on-fail #'error))
               (funcall on-fail
                        "~arthm-seq-bar::fix-nested-tuplets: bad letter-value:~
                         ~%~a Did you forget to add the tuplet number via ~
                         e.g. { 5 ... ?"
                        rsb r))
             ;; for the sake of the chop method we'll still have to force
             ;; something for cases of e.g. 3/16 bars
             (setf (letter-value r) (nearest-power-of-2 (value r))))))
    rsb))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Thu Jun 25 14:08:27 2015 -- 2nd arg is the rhythm index or a rhythm
;;; object 
(defmethod under-triplet ((rsb rthm-seq-bar) rhythm-or-index)
  (loop with pos = (bar-pos (if (integerp rhythm-or-index)
                                (get-nth-event rhythm-or-index rsb)
                                rhythm-or-index))
     for ts in (tuplets rsb) for tsf = (first ts) do
       (when (and (or (= 3 tsf) (= 2/3 tsf) (= 4/6 tsf) (= 9/6 tsf))
                  (and (>= pos (second ts))
                       (<= pos (third ts))))
         (return t))
     finally (return nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Thu Aug 13 11:11:16 2015  -- start is 0-based
(defmethod change-pitches ((rsb rthm-seq-bar) pitch-list start stop
                           &key written)
  (flet ((get-one ()
           (let ((one (pop pitch-list)))
             (if (chord-p one)
                 one
                 (make-pitch one)))))
    (loop for i from start below (min stop (num-rhythms rsb))
       for e = (nth i (rhythms rsb))
       while pitch-list
       do
         (when (and e (not (is-rest e)))
           (if written
               (set-written-pitch-or-chord e (get-one))
               (setf (pitch-or-chord e) (get-one)))))
    pitch-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Thu Feb 16 17:44:58 2017 -- moved most of the functionality of
;;; replace-events from the piece class over here, where it should have been in
;;; the first place.
;;; ****m* rthm-seq-bar/replace-rhythms
;;; DATE
;;; February 16th 2017, Edinburgh
;;; 
;;; DESCRIPTION
;;; Replace the rhythms/events in a rthm-seq-bar object, with an option to
;;; automatically beam the result. The bar is checked to make sure it is full
;;; after replacing and an error will be issued if it is not.
;;; 
;;; ARGUMENTS
;;; - a rthm-seq-bar object
;;; - the index (1-based) of the rhythm/events to start at (integer)
;;; - the number of rhythms/events to replace (integer)
;;; - a list of the new rhythm/event objects
;;; 
;;; OPTIONAL ARGUMENTS
;;; T or NIL to indicate whether the automatic beaming routine should be called
;;; after the replacement
;;; 
;;; RETURN VALUE
;;; What the is-full method returns: two values: T or NIL to indicate whether
;;; the bar is full, and the difference between the duration of the time
;;; signature and the rhythms (usually a small floating-point error difference)
;;; 
;;; SYNOPSIS
(defmethod replace-rhythms ((rsb rthm-seq-bar) start-rhythm replace-num-rhythms
                            new-rhythms &optional auto-beam)
;;; ****
  (let* ((rthms (my-copy-list (rhythms rsb)))
         (nth (1- start-rhythm)))
    ;; those events that were previously start or end points for brackets may
    ;; be replaced here leaving the events in between with references to now
    ;; deleted brackets (they have bracket slots with negative numbers which
    ;; indicate which bracket they are under when abs'ed).  Delete all tuplets
    ;; and beams here to avoid errors in cmn
    (delete-tuplets rsb)
    (delete-beams rsb)
    ;; a rest bar has no rhythms but we may want to fill it with some so fake
    ;; the rthms here.  
    (unless rthms
      ;; doesn't matter what's in the list as all elements will be replaced.
      (setf rthms (ml nil replace-num-rhythms)))
    (setf rthms (remove-elements rthms nth replace-num-rhythms)
          rthms (splice new-rhythms rthms nth))
    ;; of course, the stats for the sequenz and whole piece are now incorrect,
    ;; but we leave that update to the user, we don't want to always call it
    ;; here.
    (setf (rhythms rsb) rthms)
    (when auto-beam
      (auto-beam rsb auto-beam))
    (is-full rsb)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Thu Feb 15 16:15:30 2018 
(defmethod set-ref ((rsb rthm-seq-bar))
  (set-ref (first (rhythms rsb))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Related functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* rthm-seq-bar/make-rthm-seq-bar
;;; DESCRIPTION
;;; Public interface for creating a rthm-seq-bar object, each instance of which
;;; which holds one of the individual bars that reside in a rhythmic
;;; sequence. 
;;;
;;; This class is responsible for parsing lists containing rhythms and time
;;; signatures, but not for parsing these things themselves--that is done by
;;; separate classes.  
;;;
;;; A { followed by a number means that all the notes from now to the } will be
;;; enclosed in a bracket with the number inside.  This may be nested.  A -
;;; indicates beaming: the first - indicates the start of a beam, the second
;;; the end of that beam.
;;; 
;;; ARGUMENTS 
;;; - A list of rhythmic durations, which may include ties and dots. Durations
;;;   may be written as numeric (integer) values or may use the CM/CMN/SCORE 
;;;   alphabetic shorthand s=16, e=8, q=4, h=2, w=1. NB: Repeating rhythms can
;;;   be indicated using a shorthand notation consisting of a multiplication
;;;   symbol ('x'), e.g.: (make-rthm-seq-bar '((4 4) s x 16)).
;;;
;;; make-rthm-seq-bar requires a time signature. If no time signature is
;;; provided, the most recently defined time signature will be used. If one is
;;; provided, it must be included as the first element of the data list. The
;;; time signature is formulated as a list containing two integers, the first
;;; being the number of beats in the bar and the second being the beat unit for
;;; the bar.
;;;
;;; OPTIONAL ARGUMENTS
;;; - A name (symbol) for the object ID.
;;; 
;;; RETURN VALUE   
;;; Returns a rthm-seq-bar.
;;; 
;;; EXAMPLE
#|
  (make-rthm-seq-bar '((2 4) q e s s))

  => 
  RTHM-SEQ-BAR:
  [...]
  NAMED-OBJECT: id: NIL, tag: NIL, 
  data: ((2 4) Q E S S)

  (make-rthm-seq-bar '((2 4) q e s s) 'test)
  => 
  RTHM-SEQ-BAR:
  [...]
  NAMED-OBJECT: id: TEST, tag: NIL, 
  data: ((2 4) Q E S S)

  (make-rthm-seq-bar '((2 4) q \+16\.+32 e))
  => 
  RTHM-SEQ-BAR:
  [...]
  NAMED-OBJECT: id: NIL, tag: NIL, 
  data: ((2 4) Q +16.+32 E)

  (make-rthm-seq-bar '((2 4) { 3 te te te } q)) 
  => 
  RTHM-SEQ-BAR:
  [...]
  NAMED-OBJECT: id: NIL, tag: NIL, 
  data: ((2 4) { 3 TE TE TE } Q)
  |#
;;; SYNOPSIS
(defun make-rthm-seq-bar (rhythms &optional name)
;;; ****
  (if (rthm-seq-bar-p rhythms)
      rhythms
      (make-instance 'rthm-seq-bar :data rhythms :id name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rthm-seq-bar-p (thing)
  (typep thing 'rthm-seq-bar))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 13.12.11 SAR: Added ROBODoc info
;;; ****f* rthm-seq-bar/make-rest-bar
;;; DESCRIPTION
;;; Make a rthm-seq-bar object that consists of a bar of rest.
;;; 
;;; ARGUMENTS  
;;; - The time signature of the rthm-seq-bar object to be made, as a quoted
;;;   list.
;;;
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to indicate whether to print the time signature in score output.
;;; - show-rest. This argument indicates whether or not to print the rest in
;;;   the printed score output (CMN/LilyPond). Default = T.
;;; 
;;; The remaining optional arguments are set internally by the 
;;; slippery-chicken class, but can be read by the user for debugging.
;;; - missing-duration: Indicates whether the bar is missing a duration. 
;;; - player-section-ref: The current player and section of the given
;;;   rthm-seq-bar object.
;;; - nth-seq: The current sequenz (with a "z") of the given rthm-seq-bar
;;;   object.  
;;; - nth-bar: The current bar number of the given rthm-seq-bar object. 
;;; 
;;; RETURN VALUE    
;;; A rthm-seq-bar object.
;;; 
;;; EXAMPLE
#|
  (let ((rsb-rb (make-rest-bar '(2 4) nil t)))
(format t "~%time-sig: ~a~%is-rest-bar: ~a~%write-time-sig: ~
             ~a~%show-rest: ~a~%"
(data (get-time-sig rsb-rb))
(is-rest-bar rsb-rb)
(write-time-sig rsb-rb)
(show-rest rsb-rb))
(print-simple rsb-rb)
rsb-rb)

  =>
  RTHM-SEQ-BAR: time-sig: 0 (2 4), time-sig-given: T, bar-num: -1, 
  [...]

  time-sig: (2 4)
  is-rest-bar: T
  write-time-sig: NIL
  show-rest: T
  (2 4): rest 2,

  |#
;;; SYNOPSIS
(defun make-rest-bar (time-sig &optional
                                 write-time-sig
                                 (show-rest t)
                                 missing-duration
                                 player-section-ref nth-seq
                                 nth-bar)
;;; ****
  (let* ((ts (if (typep time-sig 'time-sig) 
                 time-sig
               (make-time-sig time-sig)))
         (result (make-rthm-seq-bar 
                  (list ts (get-whole-bar-rest ts)))))
    ;; (print result)
    (setf ;; (time-sig result) ts
          (show-rest result) show-rest
          (write-time-sig result) write-time-sig
          (missing-duration result) missing-duration
          (is-rest-bar result) t
          (player-section-ref result) (basic-copy-object player-section-ref)
          (nth-seq result) nth-seq
          (nth-bar result) nth-bar)
    result))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Sort the given rhythms into real rhythms and bracketing information for
;;; notation as well as expand repeated rhythms like 'e x 5' which means 5
;;; eighth notes.
;;;
;;; A { followed by a number means that all the notes from now to the } will be
;;; enclosed in a bracket with the number inside.  This may be nested.  A -
;;; indicates beaming: the first - indicates the start of a beam, the second
;;; the end of that beam.
;;;
;;; e.g. (parse-rhythms '(q. { 3 te - s x 5 - te q q { 5 e x 5 } s }
;;;                       s+ts. 4\.))
;;;
;;; Returns a 4-element list: the rhythms, the bracketing information for score
;;; (as a list with 3-element sublists: the number for the bracket, the left
;;; rhythm index and the right rhythm index of the bracket), the beaming
;;; positions, and the bracketing info for cmn (relative to note or rest as
;;; opposed to just note as with SCORE)
;;;
;;; Now accepts cmn rqq type definition of rhythms.  See cmn.html for details.

(defun parse-rhythms (rhythms nudge-factor)
  (flet ((num-tied-rthms (rthm)
           ;; rests are given in parentheses...
           (when (listp rthm) 
             (setf rthm (first rthm)))
           (let* ((rthm-string (format nil "~a" rthm))
                  ;; some rthms start with + to indicate that they are tied
                  ;; to previous notes (e.g. in previous bar).
                  (start-pos (if (char= #\+ (aref rthm-string 0))
                                 1
                                 0)))
             (1+ (count #\+ rthm-string :start start-pos)))))
    (when rhythms
      ;; the position at which which we got a '{'; zero-based and with
      ;; e.g. 'e x 5' expanded, i.e. this isn't just the element position
      ;; in the given list.  Also, this has to be a list for the case when
      ;; we have complex rhythms with more than one bracket.
      (let ((score-left-bracket-positions '()) 
            ;; sim but for CMN i.e. including rests
            (left-bracket-positions '())
            (got-left-brackets 0)
            ;; did we see a beam?  if so at which position?
            (start-beam nil)
            ;; storage for the beams
            (expect-right-brackets 0)
            (beam-positions '())
            ;; when we hit an 'x' then the next element will be a rhythm
            ;; repeater e.g. e x 5 which means 5 eighth notes
            (get-repeater nil)
            ;; when we hit a '{' then the next element will be the number
            ;; that goes in the bracket over the rhythms
            (get-tuplet nil)
            ;; For SCORE: we store the bracket info as a 3-element list: the
            ;; number for the bracket, the left rhythm index and the right
            ;; rhythm  index of the bracket
            (score-tuplet-positions '())
            ;; For CMN: sim to above but indices include rests
            (tuplet-positions '())
            ;; this will be the numbers for the brackets we have seen so far.
            ;; This also has to be a list so that we can have multiple
            ;; simultaneously open brackets.
            (tuplets '())
            ;; count the number of notes (not rests!) we've seen so far
            (num-notes 0)
            ;; number of notes or rests we've seen so far
            (num-rthms 0)
            ;; when we use the 'e x 4' syntax then we have to know what the
            ;; last rhythm was so that we can repeat it.
            (last-rthm nil)
            (interned nil)
            ;; list to store the rhythms in
            (rthms '()))
        (loop for i in rhythms do
             (setf interned (if (symbolp i) (rm-package i) i))
             (cond
               ((eq interned '{) (setq get-tuplet t) (incf got-left-brackets))
               ((eq interned '}) (unless (> expect-right-brackets 0)
                                   (error "rthm-seq-bar::parse-rhythms:~%~
                                           Read } without seeing { beforehand:~
                                           ~%~a"
                                          rhythms))
                (decf expect-right-brackets)
                (let ((tuplet-num (pop tuplets)))
                  #| MDE Wed Dec 14 14:21:27 2011 -- obsolete
  (push (list tuplet-num
                  (pop 
                  score-left-bracket-positions) 
                  (+ num-notes -1 
                  (if (atom last-rthm)
                  0
                  nudge-factor))) 
                  score-tuplet-positions)
  |#
                                   (push (list tuplet-num 
                                               (pop left-bracket-positions)
                                               (1- num-rthms))
                                         tuplet-positions)))
               ((eq interned '-) (if start-beam 
                                     (progn 
                                       ;; MDE Tue May 29 22:39:29 2012 -- we
                                       ;; now allow rests to have beams so
                                       ;; don't use num-notes but num-rthms 
                                       ;; (push (list start-beam (1- num-notes))
                                       (push (list start-beam (1- num-rthms))
                                             beam-positions)
                                       (setq start-beam nil))
                                     ;; MDE Tue May 29 22:40:07 2012 -- sim.
                                     ;; (setq start-beam num-notes)))
                                     (setq start-beam num-rthms)))
               ((eq interned 'x) (setq get-repeater t))
               (get-repeater (unless (integerp i)
                               (error "rthm-seq-bar::parse-rhythms:~%~
                                       Expected an integer rhythm-repeater: ~
                                       ~a ~%~a"
                                      interned rhythms))
                             (decf i)
                             (let ((note (atom last-rthm))
                                   (num-tied-rthms (* i (num-tied-rthms 
                                                         last-rthm))))
                               (when note
                                 (incf num-notes num-tied-rthms))
                               (incf num-rthms num-tied-rthms)
                               (setq get-repeater nil)
                               (loop repeat i do (push last-rthm rthms))))
               ;; get the tuplet number for bracket or beam
               (get-tuplet (unless (numberp i) ;(integerp i)
                             (error "rthm-seq-bar::parse-rhythms:~%~
                                     Expected a tuplet number (e.g. 3 or 4/5: ~
                                     ~a~%~a"
                                    interned rhythms))
                           (push interned tuplets)
                           (setq get-tuplet nil)
                           (incf expect-right-brackets))
               ((is-rqq-info interned)
                ;; MDE Fri May 29 10:57:52 2015 -- new way of handling rqq
                ;; specification of rhythms: don't process with CMN rather,
                ;; unravel them here and produce normal SC rhythms
                (let* ((parsed (parse-rhythms (rqq-divide interned)
                                              nudge-factor))
                       (prthms (first parsed))
                       (beams (third parsed))
                       (nr num-rthms) ;(1- num-rthms))
                       (brackets (fourth parsed)))
                  ;; (print prthms)
                  ;; (print beams) ;
                  ;; now just slurp them up into our overall data lists.
                  (loop for r in prthms do
                       (push r rthms)
                       (incf num-rthms)
                       (setq last-rthm r))
                  ;; beam-positions order shouldn't matter
                  (loop for b in (nreverse beams) do
                       (push (list (+ nr (first b)) (+ nr (second b)))
                             ;; todo: got to offset these values according to
                             ;; how many rthms we've already seen
                             beam-positions))
                  (loop for b in brackets do
                       (push (list (first b) (+ nr (second b)) (+ nr (third b)))
                             tuplet-positions))))
                #| 
                ;; MDE Fri May 29 10:02:22 2015 -- this is the way we used to
                ;; do rqq, relying on CMN:
  (multiple-value-bind
               (rqq-rthms rqq-num-notes rqq-num-rthms)
               (do-rqq interned) ;    ; ; ; ; ; ;
               (incf num-rthms rqq-num-rthms)
               (incf num-notes rqq-num-notes)
               (setf rthms (append (reverse rqq-rthms) rthms))))
  |#
               ;; finally we saw a rhythm!
               ;; but when it's in parentheses it's a rest so
               ;; it's not going to  increment num-notes
               (t (let* ((note (atom i))
                         (num-tied-rthms (num-tied-rthms i)))
                    (unless (zerop got-left-brackets)
                      (loop 
                          with score-pos = (- num-notes
                                              (if note 
                                                  0 
                                                nudge-factor))
                          with pos = num-rthms
                          repeat got-left-brackets 
                          do (push score-pos 
                                   score-left-bracket-positions)
                             (push pos left-bracket-positions))
                      (setq got-left-brackets 0))
                    (when note
                      (incf num-notes num-tied-rthms))
                    (incf num-rthms num-tied-rthms)
                    (setq last-rthm i)
                    (push interned rthms)))))
        (when get-tuplet
          (error "rthm-seq-bar::parse-rhythms:~%Missing tuplet: ~%~a"
                 rhythms))
        (when (> expect-right-brackets 0)
          (error "rthm-seq-bar::parse-rhythms:~%Missing }: ~%~a"
                 rhythms))
        (when get-repeater
          (error "rthm-seq-bar::parse-rhythms:~%~
                  Missing integer repeater (saw 'x' but not ~
                  following number: ~%~a"
                 rhythms))
        (when start-beam
          (error "rthm-seq-bar::parse-rhythms:~%~
                  End of beam not specified: saw opening but ~
                  not a closing '-': ~%~a" 
                 rhythms))
        ;; MDE Fri May 29 10:57:14 2015 -- beam-positions doesn't need to be
        ;; reversed
        (setf rthms (nreverse rthms)
              ;; score-tuplet-positions (nreverse score-tuplet-positions)
              ;; MDE Tue Jun 23 20:18:40 2015 -- No need to reverse as they're
              ;; sorted in init-instance
              ;; tuplet-positions (print (nreverse tuplet-positions))
              )
        ;; MDE Wed Dec 14 14:22:35 2011 -- score-tuplet-positions now nil but
        ;; no need to remove: just keep same data structure so as not to
        ;; introduce bugs above
        ;; (print rthms)
        (list rthms score-tuplet-positions beam-positions
              tuplet-positions)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+cmn
(defun get-cmn-notes-from-rqq (rqq the-notes bar-num in-c)
  (object-is-nil? rqq "rthm-seq-bar::get-cmn-notes-from-rqq" 'rqq)
  (object-is-nil? the-notes "rthm-seq-bar::get-cmn-notes-from-rqq" 'the-notes)
  (let ((do-rqq (do-rqq rqq the-notes bar-num in-c)))
    (loop for i in do-rqq
        for cmn-note = (rqq-note i)
        when (and cmn-note (not (or (typep cmn-note 'cmn::note)
                                    (typep cmn-note 'cmn::chord)
                                    (typep cmn-note 'cmn::wrest))))
        do (error "rthm-seq-bar::get-cmn-notes-from-rqq: ~
                   expected cmn-note, got ~a" 
                  cmn-note)
        when cmn-note
        collect (rqq-note i))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; If this is called with the-notes, then they're the actual notes we want to
;;; use and the cmn-notes returned by rqq are stored in the rqq-note slot of
;;; the rhythm.  If the-notes are not given then default notes are created and
;;; the rqq-info slot of the first rhythm is set to the rqq arg and all the
;;; rest become t.  This is so that rhythm durations can be checked but new
;;; cmn-notes created later by another call to do-rqq when the real notes to be
;;; used are available.  N.B. If given, the-notes must be a list of pitch
;;; objects.

#+cmn
(defun do-rqq (rqq &optional the-notes bar-num process-event-fun (in-c t))
  ;; (format t "~%do-rqq: ~a" the-notes)
  (let* ((subdivs (second rqq))
         (aux (loop for i in subdivs
                 collect (do-rqq-aux (if (listp i) (second i) '(1)))))
         (rthms (loop for i in aux appending (first i)))
         (num-notes (loop for r in rthms count (not (is-rest r))))
         (num-cmn-notes 0)
         (were-notes (when the-notes (length the-notes)))
         (stripped (loop 
                      for i in aux
                      for j in subdivs
                      for k = (second i)
                      collect (list (if (listp j) (first j) j) k)
                      do (incf num-cmn-notes (length k))))
         ;; this loops through all the rhythms including rests and grace notes
         ;; but collects only the notes necessary for the rqq i.e. no grace
         ;; notes. 
         (notes (loop 
                   for r in rthms 
                   for event = (sc-change-class r 'event)
                   with cmn-note 
                   with pitch 
                   do
                     (unless (is-rest event)
                       (setf pitch (if were-notes 
                                       (pop the-notes)
                                       (make-pitch 'c4)))
                       (unless pitch
                         (error "rthm-seq-bar::do-rqq: pitch is nil!: ~
                                  ~a ~%~a notes given"
                                rqq were-notes)))
                     (unless (is-rest event)
                       (setf (pitch-or-chord event) (clone pitch)))
                   ;; if r is a grace-note, cmn-note will be nil.
                     (setf cmn-note (get-cmn-data event bar-num t 
                                                  process-event-fun in-c)
                           bar-num nil)
                   when cmn-note collect cmn-note))
         (cmn-notes (cmn::disgorge 
                     (apply #'cmn::rqq 
                            (append 
                             (list (list (first rqq) stripped))
                             notes)))))
    (loop 
       for r in rthms 
       with note 
       with didit
       for i from 0 do
         (unless (is-grace-note r)
           (setf note (pop cmn-notes)
                 (rqq-note r) (when were-notes note)
                 ;; store the rqq list in the first
                 ;; notes of the group and t for all others so
                 ;; that we can recalculate everything when
                 ;; we've actually got the notes.
                 (rqq-info r) (if didit
                                  t
                                  (progn
                                    (setq didit t)
                                    (copy-list rqq)))
                 (rq r) (cmn::quarters note)
                 (duration r) (float (rq r))
                 (value r) (denominator (rq r))
                 (id r) (value r)
                 (data r) (value r))))
    (values rthms num-notes (length rthms))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun do-rqq-aux (list)
  ;; MDE Thu May 21 14:51:33 2015 -- when there are no subdivisions like (3 (1
  ;; 1 1)) we've got an int instead so force it to be something like (3 (1))
  (unless (listp list)
    (setf list (list list '(1))))
  ;; (print list)
  (let* ((rthms (loop 
                    for i in list
                    for r = (parse-possibly-compound-rhythm i)
                    collect (if (listp r)
                                (progn
                                  (when (> (length r) 1)
                                    (error "rthm-seq-bar::do-rqq-aux: ~
                                            Tied notes should be given ~
                                            with a space between them: ~a"
                                           list))
                                  (first r))
                              r)))
         ;; the rqq list to be passed to cmn i.e. without grace notes and
         ;; bracketed numbers indicating rests.
         (stripped (loop for i in rthms 
                       unless (is-grace-note i)
                       collect (id i))))
    (list rthms stripped)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun handle-rqq (rqq pitches bnum events process-event-fun in-c)
  ;; (print 'handle-rqq)
  ;; (print pitches)
  ;; (print events)
  ;; (print (length pitches))
  ;; (print (length events))
  ;; (print rqq)
  ;; some grace notes might have been stored already so delete them here
  ;; (setf cmn::*cmn-grace-notes-for-sc*  nil)
  (let ((cmn-notes (get-cmn-notes-from-rqq rqq pitches bnum in-c))
        (result '()))
    ;; (print cmn-notes)
    (loop 
       for e in events 
       for rqqn = (rqq-info e)
       while cmn-notes
       do
       ;; (print rqqn)
         ;; (print e)
       ;; those events not yet handled will have rqq-info as a list or t
       ;; if they are notes or rests but not grace-notes
         (when (or (and rqqn (listp rqqn))
                   (eq t rqqn))
           (setf (rqq-note e) (pop cmn-notes))
           (push (get-cmn-data e bnum nil process-event-fun in-c) result)
           ;; (print 'here)
           (setf bnum nil)))
    (when cmn-notes
      (error "pitches: ~%~a ~%events:~a~%: ~
              rthm-seq-bar::handle-rqq: Too many cmn notes!"
             pitches events))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Lists like '(1 ((1 (1 1 1 1)) (1 (1 1 2)) (1 (1)))) are to be passed to the
;;; cmn rqq function for auto-beaming etc.  Test to see if thing looks like
;;; this kind of list--not exhaustive but thorough enough compared to the data
;;; format of other rhythms etc.

(defun is-rqq-info (thing)
  ;; (print thing)
  (and (listp thing)
       (numberp (first thing))
       (second thing)
       ;; MDE Thu May 28 12:16:02 2015 -- we don't have to have sublists...
       (or (integerp thing)
           (listp (second thing)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun consolidate-rests-aux (rest number)
  ;; (format t "~%~a ~a" rest number)
  (let ((scaled (scale rest number)))
    (flet ((consolidate (num scale)
             (let ((result (list (scale rest scale))))
               (decf num scale)
               (when (>= num 4)
                 (push (scale rest 4) result)
                 (decf num 4))
               (when (>= num 2)
                 (push (scale rest 2) result)
                 (decf num 2))
               (unless (zerop num)
                 (push (clone rest) result))
               ;; reversing puts longer rests in first!
               ;; MDE Tue May  1 18:18:09 2012 -- this was commented out but
               ;; seems to be more sensible 
               (nreverse result)))
           ;; result))
           (number-error (num)
             (error "rthm-seq-bar::consolidate-rests-aux: ~a x ~a rests ~
                     unhandled!"
                    num (data rest))))
      (cond ((= number 1) 
             (list rest))
            ((< number 1)
             (number-error number))
            ((power-of-2 (duration scaled))
             (list scaled))
            ((>= number 8)
             (consolidate number 8))
            ((>= number 4) 
             (consolidate number 4))
            ((>= number 2) 
             (consolidate number 2))
            (t (number-error number))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; if match-rhythm is given (probably a beat), we'll only consolidate those.

(defun consolidate-notes-aux (rhythms &optional (bar-num -1) match-rhythm)
  ;; (print 'consolidate-notes-aux-in)
  ;; (print-rhythms-rqs rhythms)
  ;; (print rhythms)
  (when (contains-ties rhythms)
    ;; (loop for r in rhythms do (format t "~a " (rq r)))
    (loop for r in rhythms and i from 0 do 
         (when (and (is-grace-note r)
                    (or (is-tied-to r)
                        (is-tied-from r)))
           (error "~a~%rthm-seq-bar::consolidate-notes-aux: bar num ~a ~
                    Element ~a is a grace note and shouldn't be tied to or ~
                    from." bar-num rhythms i))
         (unless (rhythm-p r)
           (error "~a~%rthm-seq-bar::consolidate-notes-aux: bar num ~a ~
                    Element ~a is not a rhythm: ~a" rhythms bar-num rhythms i)))
    (let ((result nil)
          (tied-to (is-tied-to (first rhythms)))
          (tied-from (is-tied-from (first (last rhythms))))
          (sum (sum-rhythms-duration rhythms))
          (sum-consol 0))
      ;; first try is to see if they're all tied and add up to some simple
      ;; rhythm  
      (when (rhythms-all-tied? rhythms)
        ;; (print (length rhythms))
        ;; (print "##########################################################")
        ;; (print rhythms)
        (let* ((letter (if (> sum 0)
                           (let* ((temp (get-rhythm-letter-for-value 
                                         ;; val = 4 / dur
                                         (/ 4 sum) nil)))
                             (if temp
                                 (make-rhythm temp)
                                 (warn "~%rthm-seq-bar::~
                                         consolidate-notes-aux: ~
                                         bar num ~a: Was hoping for a letter. ~
                                         ~%(sum = ~a); backing out."
                                       bar-num sum)))
                           (error "~a~%rthm-seq-bar::consolidate-notes-aux: ~
                               bar-num: ~a: sum is 0!" bar-num rhythms))))
          (when (and letter (whole-num-p (value letter)))
            (setf result (list letter)))))
      (unless result
        ;; (print 'further)
        ;; ok so we'll have to do it the other way.....
        (setf result (consolidate-notes-aux2 rhythms match-rhythm)))
      (when tied-to 
        (setf (is-tied-to (first result)) t))
      (when tied-from
        (setf (is-tied-from (first (last result))) t))
      (setf sum-consol (sum-rhythms-duration result))
      (unless (equal-within-tolerance sum sum-consol)
        (print-rhythms-rqs rhythms)
        (print-rhythms-rqs result)
        (error "~%rthm-seq-bar::consolidate-notes-aux: bar num: ~a ~
                Consolidated rthms sum (~a) != previous sum (~a)"
               bar-num sum-consol sum))
      ;; (print 'consolidate-notes-aux-out)
      ;; (print-rhythms-rqs result)
      ;; MDE Tue Mar 13 11:02:38 2012 -- still need the bracket?
      (when (equalp (tuplet-scaler (first rhythms))
                    (tuplet-scaler (first result)))
        (setf (bracket (first result))
              (bracket (first rhythms))))
      result)))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
  
;;; same repeated tied rhythm case

(defun consolidate-notes-aux2 (rhythms &optional match-rhythm)
  (when rhythms
    (multiple-value-bind
          (start end)
        ;; returns the position of the _first_ repeating rhythm
        (find-repeating-tied-rhythm rhythms match-rhythm)
      (if start 
          (append
           (subseq rhythms 0 start)
           (let ((temp (consolidate-notes-aux3 (nth start rhythms)
                                               (- end start))))
             (when (is-tied-to (nth start rhythms))
               (setf (is-tied-to (first temp)) t))
             (when (is-tied-from (nth (1- end) rhythms))
               (setf (is-tied-from (first (last temp))) t))
             temp)
           (consolidate-notes-aux2 (nthcdr end rhythms)))
          ;; no repeating rhythms--that's it!
          ;; 5/5/07: NO! try it another way...
          ;; rhythms))))
          (consolidate-notes-aux5 rhythms)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun consolidate-notes-aux3 (rhythm num)
  (let ((consol (consolidate-notes-aux4 rhythm num)))
    (loop for r1 in consol and r2 in (cdr consol) do
          (setf (is-tied-from r1) t
                (is-tied-to r2) t))
    consol))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun consolidate-notes-aux4 (rhythm num)
  ;; (print rhythm)
  ;; (print num)
  (let ((rthm (clone rhythm)))
    (setf (is-tied-to rthm) nil
          (is-tied-from rthm) nil)
    (unless (zerop num)
      (if (= num 1)
          (list rthm)
        (let* ((p2 (nearest-power-of-2 num))
               (p2-dotted (* p2 3/2))
               (p2-double-dotted (* p2 7/4))
               (scale-by (cond ((<= p2-double-dotted num) p2-double-dotted)
                               ((<= p2-dotted num) p2-dotted)
                               (t p2)))
               (scaled (scale rthm scale-by)))
          (when (< (duration scaled) 0)
            (error "rthm-seq-bar::consolidate-notes-aux4: ~
                    ~a~%~a~%scaling failed!" rhythm num))
          ;; with irrationals we don't get the number of dots we need...
          (when (zerop (num-dots scaled))
            (setf (num-dots scaled)
              (if (= scale-by p2-double-dotted)
                  2
                (if (= scale-by p2-dotted)
                    1
                  0))))
          (cons scaled
                (consolidate-notes-aux4 rthm (- num scale-by))))))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; try to consolidate any old rthms

(defun consolidate-notes-aux5 (rthms)
  ;; (print 'aux5)
  (let ((ties (get-tied-rthms rthms))
        (result '()))
    (loop for tied in ties do 
         (when tied
           ;; (print 'aux5-tied)
           ;; (print tied)
           (push 
            (if (> (length tied) 1)
                (let ((rat (make-tied
                            (rationalize-if-necessary 
                             (sum-rhythms-duration tied)
                             :rest nil
                             :keep-it-simple t
                             :error-on-fail nil))))
                  ;; (print 'rat)
                  ;; (print rat)
                  (if (or (not rat) (= (length tied) (length rat)))
                      tied
                      rat))
                (first tied))
            result)))
    (flatten (nreverse result))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(defun get-tied-rthms (rhythms)
  ;; (print 'gtr)
  (let ((result '())
        (temp '()))
    (flet ((do-temp ()
             (when temp
               (push (nreverse temp) result)
               (setf temp nil))))
      (loop 
         for r in rhythms do
         ;; MDE Wed Nov 28 14:00:05 2012 -- marks need to stop ties being
         ;; consolidated  
         (cond ((or (and (is-tied-from r)
                         (not (is-tied-to r)))
                    (and (is-tied-to r) (marks r)))
                (do-temp)
                (push r temp))
               ((is-tied-to r)
                (push r temp))
               (t (do-temp)
                  ;; just push the non-tied rhythms in as 1-element lists
                  (push (list r) result)))
         finally
         (do-temp)
         (return (nreverse result))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Returns the start and end points of a repeated (and tied!) rhythm in a list
;; of rhythm objects.  
;; NB start will be the index of where the first repeated rhythm is, end the
;; index of the next different rhythm, so these values are suitable for subseq

(defun find-repeating-tied-rhythm (rhythms &optional match-rhythm)
  (flet ((tied-rthm-equal (r1 r2)
           ;; just consolidate those with the same duration, 
           ;; not things like h,q
           (and (is-tied-from r1) 
                (or (not match-rhythm) 
                    (rhythm-equal r1 match-rhythm))
                (is-tied-to r2)
                (rhythm-equal r1 r2))))
    (let ((start 0)
          (end 0))
      (loop 
          for r1 in (butlast rhythms) 
          and i from 0 do
            (loop 
                for j from (1+ i)
                for r2 in (nthcdr j rhythms) 
                while (tied-rthm-equal r1 r2)
                finally (when (> j (1+ i))
                          (setf end j)))
            (when (> end 0)
              (setf start i)
              (return)))
      (when (/= start end)
        (values start end)))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rhythms-all-tied? (rhythms)
  (loop for r in (butlast rhythms) do
        (unless (is-tied-from r)
          (return nil))
      finally (return t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sum-rhythms-duration (rhythms)
  (loop for r in rhythms sum (duration r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun print-rhythms-rqs (rthms)
  (print "rqs:")
  (loop for r in rthms do
        (format t "~a " (rq r))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rhythms-to-events-list (rthms)
  (loop for rhythm in rthms collect
        (if (event-p rhythm)
            rhythm
          (clone-with-new-class rhythm 'event))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rhythms-all-rests? (rthms)
  (when rthms
    (loop for r in rthms do
         (unless (rhythm-p r)
           (error "~a~%rthm-seq-bar::rhythms-all-rests?: not a rhythm!" r))
         (unless (is-rest r)
           (return nil))
       finally (return t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun contains-ties (rthms)
  (loop for r in rthms do
        ;; the first rthm being tied to doesn't count
        (when (is-tied-from r)
          (return t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun print-simple-rthm-list (rthms)
  (terpri)
  (loop for r in rthms do (print-simple r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun has-tuplets (rthms)
  (loop for r in rthms do
       (unless (= 1 (tuplet-scaler r))
         (return t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mark-for-note-only (mark)
  (or 
   (is-dynamic mark)
   (member mark '(a lhp bartok s nail stopped as at ts te t3 beg-gliss
                  end-gliss 0 1 2 3 4 I II III IV I-II I-II-III II-III III-IV
                  beg-sl end-sl airy-head none circled-x trill-f trill-n
                  trill-s x-head square slash triangle arrow-up arrow-down harm
                  triangle-up open))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Wed Mar 16 10:30:04 2016 -- abstracted out of update-time method so we
;;; can call on a simple list of events

;;; ****f* rthm-seq-bar/events-update-time
;;; DATE
;;; March 16th 2016, Edinburgh
;;; 
;;; DESCRIPTION
;;; Update the start-time and related slots of any events for which timing
;;; information is missing or out-of-sync. 
;;; 
;;; ARGUMENTS
;;; - a list of event objects
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :start-time. The start time in seconds of the first event
;;; - :start-time-qtrs. The start time in quarter notes of the first event (used
;;;   in MIDI files)
;;; - :tempo. The tempo in beats per minute (or a tempo object)
;;; 
;;; RETURN VALUE
;;; two values: the list of events with their times updated, and the time the
;;; last event finishes.
;;; 
;;; EXAMPLE
#|

  (event-list-to-midi-file
(events-update-time (make-events2 '(q q q) '(g4 g4 g4) 1))
:midi-file "/tmp/test.mid" :start-tempo 120)

  |#
;;; SYNOPSIS
(defun events-update-time (events &key (start-time 0.0) (start-time-qtrs 0.0)
                                    (tempo 60.0))
;;; ****
  (unless (typep tempo 'tempo)
    (setf tempo (make-tempo tempo)))
  (unless (every #'event-p events)
    (error "rthm-seq-bar::events-update-time: first argument should be a list ~
            of event objects: ~a" events))
  (let ((qtr-dur (qtr-dur tempo))
        (time start-time)
        (time-qtrs start-time-qtrs))
    (loop for event in events do   
         (setf (start-time event) time
               (start-time-qtrs event) time-qtrs
               (duration-in-tempo event) (* (duration event) qtr-dur)
               (compound-duration-in-tempo event) 
               (* (compound-duration event) qtr-dur)
               (end-time event) (+ (start-time event) 
                                   (compound-duration-in-tempo event)))
         (incf time-qtrs (duration event))
         (incf time (duration-in-tempo event)))
    (values events time)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Fri Apr 14 07:05:07 2017 -- rthm is a power-of-two. Sometimes we
;;; have a simple tuplet like 5, meaning of course 5 in the time of 4, but this
;;; could be over a whole bar e.g. 3/4. In that case the
;;; tuplet-actual-normals method would think 5 qs in the time of 4, which is
;;; clearly wrong, so we need to try various multiples and rhythms in order to
;;; discover that what is in fact meant is e.g. 15 in the time of 12 1/16ths.
(defun jiggle-tuplet-actual-normal (duration num time-of rthm
                                    &optional (warn t))
  (let* ((np2 (nearest-power-of-2 (round rthm)))
         (target (if (and (float-int-p duration)
                          (power-of-2 time-of))
                     duration
                     (* duration (/ rthm np2))))
         (count 0)
         (max 10)
         n tof r)
    (flet ((reset ()
             (setq n num
                   tof time-of
                   r (make-rhythm (floor np2)))))
      (reset)
      ;;(format t "~&duration ~a num ~a time-of ~a rthm ~a target ~a"
      ;;      duration num time-of rthm target)
      (loop until (or (= (incf count) max) ; no floating point errors please
                      (equal-within-tolerance target (* tof (duration r))))
         do
           (incf n num)
           (incf tof time-of)
           (scale r .5 nil))
      (when (= count max)
        (when warn
          (warn "~&rthm-seq-bar::jiggle-tuplet-actual-normal: couldn't find ~
                 ideal values after ~a tries." max))
        (reset))
      (list n tof (xml-simple-rhythm (value r))))))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Thu May 28 12:26:55 2015 -- new rqq handling code to avoid having to
;;; explicitly call CMN routines, instead turning them into our normal SC
;;; rhythmic notation for further parsing. NB None of these routines are meant
;;; to be called by the user, rather, when parse-rhythms detects rqq notation
;;; it calls rqq-divide implicitly.

;;; We need this because we're using flatten on the recursive lists and rests
;;; are indicated with () 
(defstruct rqq-divide-rthm (r) (rest))

;;; This will put tuplets over a whole RQQ list, rather than dividing into
;;; beats and bracketing those. If you don't want this, then split the RQQ
;;; lists into beats.
(defun rqq-divide (divisions)
  (let* ((aux (rqq-divide-aux divisions 4))
         (faux (loop for r in (flatten aux) collect
                    (if (typep r 'rqq-divide-rthm)
                        (if (rqq-divide-rthm-rest r)
                            (list (rqq-divide-rthm-r r))
                            (rqq-divide-rthm-r r))
                        r)))
         ;; (beam-all (beamable (flatten faux))))
         (beam-all (beamable faux)))
    ;; (print faux)
    (if beam-all
        ;; if we can put a beam over all rthms, remove beams we've added
        ;; already and put a beam symbol at the beginning and end. 
        (beamem faux) 
        (debeamem (remove-pair faux '(- -))))))

(defun rqq-got-rest (thing)
  (and (listp thing) (equal 1 (length thing))))

(defun rqq-divide-aux (divisions parent-dur)
  ;; handle rests in the usual way, i.e. those values placed in ()
  ;; (format t "~&divs: ~a pd: ~a" divisions parent-dur)
  (let ((rest nil))
    (when (rqq-got-rest divisions)
      (setf divisions (first divisions)
            rest t))
    (if (integerp divisions)
        (let* ((v (/ parent-dur divisions))
               (r (get-rhythm-letter-for-value v nil))
               ;; some just always create problems...
               (result (if r ;(and r (not (member r '(fs. fe. fq.))))
                           r
                           v))
               ;; when we have something like 8/3 we can just make it 4\.
               ;; (a q. is 8/3 or 8/2. or 4.)
               (dotit (and (numberp result) (or ;(= 3 (numerator result))
                                             (= 3 (denominator result))))))
          ;; (format t "~%pd ~a div ~a res ~a" parent-dur divisions result)
          (format t "~%v ~a r ~a result ~a" v r result)
          ;; try and set dots if possible
          (when dotit
            ;; (print result)
            (setf result 
                  (if (evenp (numerator result))
                      ;; strings work as rthms too
                      (format nil"~a\." (/ (numerator result) 2))
                      (format nil"~a/~a\." (numerator result) 2))))
          ;; (format t "~&result: ~a" result))
          (make-rqq-divide-rthm :r result :rest rest))
        (let* ((2divs (second divisions))
               (rqqnd (rqq-num-divisions 2divs))
               (this-dur (first divisions))
               ;; the ration of the total number of divisions we have to the
               ;; duration  
               (ratio (/ this-dur rqqnd))
               (pd (/ (* parent-dur rqqnd) this-dur))
               (result
                (flatten 
                 (loop for div in 2divs collect
                      (rqq-divide-aux (consolidate-rqq-rests-p div) pd))))
               ;; if we have something like (3 (1 1 1)) then we don't need a
               ;; tuplet bracket
               (tuplet (unless (power-of-2 (/ rqqnd this-dur))
                         (cond ((= ratio 2/3) 3) ; (2 (1 1 1))
                               ((= ratio 1/3) 3) ; (1 (1 1 1))
                               ((= ratio 1/6) 3)
                               ((= ratio 4/6) 3)
                               ((= ratio 4/3) 3)
                               ((= ratio 1/10) 5)
                               ((= ratio 1/5) 5)
                               ((= ratio 2/5) 5)
                               ((= ratio 4/5) 5)
                               (t (calc-tuplet this-dur rqqnd)))))
               (beam (beamable result)))
          ;; (format t "~&~a: beamable: ~a" result beam)
          ;; (format t "~%~a ~a" rqqnd (first divisions))
          ;; (format t "~%this-dur ~a rqqnd ~a pd ~a" this-dur rqqnd pd)
          (print result)
          (format t "~%~a~%ratio ~a tupl ~a this-d ~a" 
                           divisions ratio tuplet this-dur)
          ;; sometimes we'll be under two tuplet brackets but get something
          ;; like a simple TS as the rthm but then under a 2:3 bracket, which
          ;; should be turned into a dotted value 
          (when (and tuplet (or (= tuplet 4/3) (= tuplet 3/2)))
            (setf tuplet nil)
            (loop for r in result
               for rthm = (make-rhythm (rqq-divide-rthm-r r))
               do (setf (rqq-divide-rthm-r r) 
                        (format nil "~a\." (/ 4 2/3 (rq rthm))))))
          ;;; MDE Mon Jun  4 18:17:21 2018 -- this more simple case should start
          ;;; with e. or start the triplet on the 2nd note :/
          ;;; (rqq-divide '(1 (3 1 1 1)))
          (if tuplet
              (progn
                (setf result (append (list '{ tuplet) result '(})))
                (if beam 
                    (beamem result)
                    result))
              (if beam (beamem result) result))))))

;;; returns a ratio like 3/2 which is triplets: 3 in the time of 2, or 5/4
;;; (quintuplets). todo: 4/5 (4 in time of 5) is returning 4/5 just like 5 is!
;;; we don't merely use this-dur as the numerator because that might mean we
;;; get a ratio like 13:2 when the overall duration of a minim is divided into
;;; 13 parts. Instead we use the nearest power of 2 to the denominator (rqqnd)
;;; so we get 13:8.
(defun calc-tuplet (this-dur num-divisions)
  ;; (format t "~&(calc-tuplet ~a ~a)" this-dur num-divisions)
  (unless (integerp (/ this-dur num-divisions))
    (let* ((invert (> this-dur num-divisions))
           (n (if invert num-divisions this-dur))
           (d (if invert this-dur num-divisions))
           (per-beat (/ d this-dur))
           (p2 (nearest-power-of-2 per-beat))
           (result (/ (* n p2) d)))
      (if invert result (/ result)))))


(defun consolidate-rqq-rests-p (div)
  (if (or (integerp div) (rqq-got-rest div))
      div
      (list (first div) (consolidate-rqq-rests (second div)))))

;;; make something like ((1) (1) 1 (1) (1) (1)) -> ((2) 1 (3))
(defun consolidate-rqq-rests (nums)
  (let ((total 0)
        (result '()))
    (flet ((doit ()
             (unless (zerop total)
               (cond ((zerop (mod total 3))
                      ;; avoid dot complications
                      (case total
                        (3 (push '(2) result)
                           (push '(1) result))
                        (6 (push '(4) result)
                           (push '(2) result))
                        (9 (push '(8) result)
                           (push '(1) result))))
                     ((power-of-2 total)
                      (push (list total) result))
                     (t (let ((np2 (nearest-power-of-2 total)))
                          (push (list np2) result)
                          (push (list (- total np2)) result))))
               (setf total 0))))
      (loop for n in nums do
           (if (and (listp n) (= 1 (length n)))
               (incf total (first n))
               (progn
                 (doit)
                 (push n result))))
      (doit)
      (nreverse result))))
           
(defun all-dotted (rthms)
  (loop for r in rthms do
       (when (typep r 'rqq-divide-rthm)
         (let ((rr (rqq-divide-rthm-r r)))
           (if (numberp rr)
               (return nil)
               (when (or (symbolp rr) (stringp rr))
                 (let ((rrs (string rr)))
                   (unless (char= #\. (elt rrs (1- (length rrs))))
                     (return nil)))))))
     finally (return t)))

(defun beamem (rthms)
  ;; (print '***)(print rthms)
  (flet ((rest-pos (list)
           (loop for r in list and i from 0 do
                (unless (or (and (typep r 'rqq-divide-rthm)
                                 (rqq-divide-rthm-rest r))
                            (listp r))
                  (return i)))))
    (let* ((bracket-beg (eq '{ (first rthms)))
           (bracket-end (eq '} (first (last rthms))))
           ;; first remove existing beams
           (rs (remove '- (if bracket-beg
                              (if bracket-end
                                  (butlast (cddr rthms))
                                  (cddr rthms))
                              rthms)))
           (rslen (length rs))
           (start (rest-pos rs))
           (end (- rslen (rest-pos (reverse rs))))
           (before-beam (subseq rs 0 start))
           (after-beam (when (< end rslen) (subseq rs end))))
      (if (< (- end start) 2)
          rthms
          (progn
            (when bracket-beg
              (setf before-beam (cons '{ (cons (second rthms) before-beam)))
              (when bracket-end
                (setf after-beam (econs after-beam '}))))
            (append before-beam (cons '- (subseq rs start end)) 
                    (cons '- after-beam)))))))


(defun debeamem (rthms)
  ;; just the outer beams, unless there are stop/start beams in the middle
  (let ((db (if (and (eq '- (first rthms))
                     (eq '- (first (last rthms))))
                (butlast (rest rthms))
                rthms)))
    (if (member '- db)
        rthms
        db)))

;;; can the given rhythms (numbers or symbols, with/without  - and { notations)
;;; to be put under a beam or not?
(defun beamable (rthms)
  ;; (print rthms)
  (flet ((is-rest (thing)
           (when thing
             (if (typep thing 'rqq-divide-rthm)
                 (rqq-divide-rthm-rest thing)
                 (and (listp thing) (= 1 (length thing)))))))
    (when (> (length rthms) 1)
      (loop with last with val with firstr with lastr with got-rthm
         for elraw in rthms
         for struct = (typep elraw 'rqq-divide-rthm)
         for el = (typecase elraw
                    (rqq-divide-rthm (rqq-divide-rthm-r elraw))
                    (list (first elraw))
                    (t elraw))
         with last-tuplet = 1
         with compound-tuplet = 1
         do
           (setq val (if (numberp el)
                         el
                         (parse-rhythm-symbol el :error nil))
                 got-rthm (or struct (and (not (eq last '{))
                                          (numberp val))))
         ;; keeping track of tuplet scalers helps decide over a whole bar with
         ;; tuplet info (e.g. ({ 4/7 { 7/5 - FS (FS) FS (FS) FS - } { 5/6 -
         ;; 168/5 (42/5) ...) but won't help when we're deciding about the
         ;; parts in that bar (e.g. (5 (1 (4) 1))), as we must so the todo
         ;; below still holds
           (cond ((eq last '{)
                  (setq last-tuplet (get-tuplet-ratio val)
                        compound-tuplet (* compound-tuplet last-tuplet)))
                 ((eq elraw '})
                  (setq compound-tuplet (/ compound-tuplet last-tuplet))))
         ;;(format t "~%~a: ct ~a dur ~a" 
         ;;        val compound-tuplet (when val (* val compound-tuplet)))
           (when got-rthm
             (if firstr
                 (setq lastr elraw)
                 (setq firstr elraw)))
         ;; todo: this still means we can end up trying to put a beam over a
         ;; 1/4 note/rest when it's >8 in duration because of nested tuplets,
         ;; e.g. (make-rthm-seq-bar '((2 4) (2 ((3 (1 (1) 1 1)) (6 (1 1 1)) (4
         ;; (1 (1) (1) (1) (1) 1)))))). Lilypond doesn't break though.
           (when (and got-rthm (< (* compound-tuplet val) 8))
             (return nil))
           (setf last el)
         ;; no beams with rests at start or end
         finally (return (not (or (is-rest firstr) (is-rest lastr))))))))


(defun rqq-num-divisions (rqq)
  (loop for divs in rqq sum
       (typecase divs
         (list (first divs))
         (integer divs)
         (t (error "rthm-seq::rqq-denom: malformed data: ~a in ~%~a"
                   divs rqq)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF rthm-seq-bar.lsp
