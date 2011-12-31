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
;;; Version:          1.0
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
;;; $$ Last modified: 18:32:08 Fri Dec 30 2011 ICT
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
   ;; todo (20.7.11) make into an sclist object instead of a simple list?
   (rhythms :accessor rhythms :type list :initform nil)
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
   ;; the above is for SCORE, the following for CMN (see parse-rhythms)
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
   ;; 31.1.11 NB in lilypond this will be attached to the last note in the bar
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
   ;; accordingly; the var we need is probably *cmn-measure-number* todo: add
   ;; usual clone and print code
   (multi-bar-rest :accessor multi-bar-rest :initform nil)
   ;; when we generate an rsb with chop, we need to keep track of the attack
   ;; number of the start and end note that the new bar was extracted from the
   ;; old. 
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
         (parsed (parse-rhythms (if ts (rest data) data) (nudge-factor rsb)))
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
                                #'(lambda (x y) (< (second x) (second y)))))
      (unless (is-full rsb)
        (error "~a~%rthm-seq-bar::initialize-instance:~
               ~%Incorrect number of beats in bar: Expected ~a, got ~a~
               ~%Perhaps you forgot to change the time signature??? ~%~a~%"
               rhythms
               (duration (get-time-sig-from-all-time-sigs rsb))
               (rhythms-duration rsb)
               data)))
    (gen-stats rsb)
    (update-missing-duration rsb)
    (update-rhythms-bracket-info rsb)
    (update-rhythms-beam-info rsb)
    ;; 2/04
    ;; 17/5/05: now handled at piece level
    ;; (update-compound-durations rsb)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Sum the rhythms and compare the duration against that of the time sig.
;;; error can either be t 'warn or nil; if the latter no warning or error will
;;; be issued i.e. it will return nil silently.
(defmethod is-full ((rsb rthm-seq-bar) &optional (error t))
  (let* ((rthms-dur (rhythms-duration rsb))
         (ts-dur (duration (get-time-sig-from-all-time-sigs rsb
                                                            (time-sig rsb))))
         (ok (equal-within-tolerance rthms-dur ts-dur .001)))
    (when (and (not ok)
               (> rthms-dur ts-dur))
      (when error
        (funcall (if (eq error 'warn) #'warn #'error)
                 "~a: ~%rthm-seq-bar::isfull:~%~
                   Duration of rhythms (~a) > duration of time-sig: (~a)"
                 rsb rthms-dur ts-dur)))
    ok))

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

;;; ****m* rthm-seq-bar/delete-marks
;;; FUNCTION
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
(let ((rsb (make-rthm-seq-bar `((3 8) ,@(loop repeat 3 
                                           collect (make-event 'cs4 'e))))))
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

(defmethod add-event ((rsb rthm-seq-bar) event &key (position nil))
  (setf (rhythms rsb)
    (if position
        (splice (list event) (rhythms rsb) position)
      (econs (rhythms rsb) event))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;  

;;; 12.12.11 SAR: Added ROBODoc info

;;; ****m* rthm-seq-bar/fill-with-rhythms
;;; FUNCTION
;;; Any rhythms in the rthm-seq-bar object will be deleted and then rhythm
;;; objects will be taken one by one from the <rhythms> argument until the bar
;;; is full. The number of rhythms used is returned. 
;;;
;;; NB: This method does not change the DATA of the rthm-seq-bar object itself
;;; to reflect the new rhythms. Instead, it changes the contents of the RHYTHMS
;;; slot within that object and changes the DATA of the rthm-seq-bar object to
;;; NIL. It also assigns the ID of the named-object to
;;; "rhythms-inserted-by-fill-with-rhythms". 
;;; 
;;; ARGUMENTS
;;; - A rthm-seq-bar object.
;;; - A list of rhythms.
;;; 
;;; RETURN VALUE
;;; The number of rhythms used.
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
                      (error "rthm-seq-bar::fill-with-rhythms: transposition ~
                               given but even has written and sounding ~
                               pitches already: ~a" r)
                      (set-written r (- transposition))))
                (set-midi-channel r midi-channel microtones-midi-channel))
              (push r (rhythms rsb))
              (when (is-full rsb is-full-error)
                (return i)))))
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
;;; FUNCTION
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
;;;  21.7.11 (Pula): This method is by no means error-free, especially as it
;;; was developed for combining lots of short rests into a longer one.  In
;;; particular we need to develop an algorithm for resolving simple things e,q
;;; as rests...
;;; ****m* rthm-seq-bar/consolidate-rests
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
(defmethod consolidate-rests ((rsb rthm-seq-bar) &key beat min warn)
;;; ****
  ;; (print 'consolidate-rests)
  ;; (print (length (rhythms rsb)))
  (let ((beats (get-beats rsb beat))
        (cbeats '())
        ;; 21.7.11
        (rest-beat (make-rest (get-beat-as-rhythm rsb)))
        (rest-dur nil)
        (count 1)
        (last-rhythm nil)
        (current '()))
    (setf min (if min 
                  (duration (make-rhythm min))
                  0.0))
    ;; (format t "~%bar ~a ~a" (bar-num rsb) (length (first beats)))
    (if (all-rests? rsb)
        (force-rest-bar rsb)
        (flet ((consolidate (rthm count)
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
                   (setf current (list (clone rest-beat)))
                   (loop for r in beat do
                        (if (is-rest r)
                            (if rest-dur
                                (if (= rest-dur (duration r))
                                    (incf count)
                                    (progn
                                      (consolidate last-rhythm count)
                                      (setf count 1
                                            rest-dur (duration r))))
                                (setf rest-dur (duration r)))
                            (progn ;; not a rest!
                              (when (and last-rhythm (is-rest last-rhythm))
                                (consolidate last-rhythm count))
                              (setf rest-dur nil
                                    count 1)
                              (push r current)))
                        (setf last-rhythm r)))
               (when (and last-rhythm (is-rest last-rhythm))
                 (consolidate last-rhythm count))
               (push (reverse current) cbeats)
               (setf current nil
                     last-rhythm nil
                     rest-dur nil
                     count 1))
          (setf cbeats (flatten (reverse cbeats)))
          (if (equal-within-tolerance (rhythms-duration rsb)
                                      (sum-rhythms-duration cbeats)
                                      .000004)
              ;; 21.7.11 (Pula) don't fail if we can't do it, just do nothing
              (setf (rhythms rsb) cbeats)
              (progn
                (when warn
                  ;; (print rsb)
                  ;; (print-rhythms-rqs (rhythms rsb))
                  ;; (print-rhythms-rqs cbeats)
                  (warn "~a~%rthm-seq-bar::consolidate-rests: ~
                       Consolidated rthms sum (~a) != previous sum (~a)"
                        rsb (sum-rhythms-duration cbeats)
                        (rhythms-duration rsb)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* rthm-seq-bar/force-rest-bar
;;; 12.12.11 SAR: Added ROBODoc info
;;; FUNCTION
;;; Force all rhythms of a rthm-seq-bar object to be replaced by rest.
;;; 
;;; NB: This method changes the value of the RHYTHMS slot of the rthm-seq-bar
;;; but not the value of the rthm-seq-bar DATA slot.
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
            (marks-before new) (marks-before first)))
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
;;; 
;;; When a bunch of short notes are tied to each other, make one (or a few)
;;; notes of them.  If check-dur, make sure we get an exact beat's worth of
;;; rhythms.

;;; ****m* rthm-seq-bar/consolidate-notes
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
(defmethod consolidate-notes ((rsb rthm-seq-bar) &optional check-dur beat)
;;; ****
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
          ;; are tuplets.  todo: extend this to look at consecutive groups of
          ;; beats and test for tuplets
          ;; (print 'here)
          (unless (has-tuplets rthms)
            (setf tmp (consolidate-notes-aux rthms 
                                               (bar-num rsb)
                                               (get-beat-as-rhythm rsb)))
            (when tmp (setf rthms tmp)))
          (setf (rhythms rsb) 
                (if (event-p (first (rhythms rsb)))
                    (consolidated-rthms-to-events rsb rthms)
                    rthms)))))
    (ties-to-dots rsb check-dur beat)
    ;; now try and get dots that go over two beats e.g. q. e in 2/4
    (ties-to-dots rsb check-dur (scale 
                                 (if beat
                                     (make-rhythm beat) 
                                     (get-beat-as-rhythm rsb))
                                 2))
    (fix-brackets rsb)
    (gen-stats rsb)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod ties-to-dots ((rsb rthm-seq-bar) &optional check-dur beat)
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
                    ;; (print 'hello1)
                    ;; we've just absorbed r1 into a dot while it was r2 in the
                    ;; previous loop... 
                    (if skip
                        (setf skip nil)
                      (if (and (zerop (num-dots r1))
                               (zerop (num-dots r2))
                               (is-tied-from r1)
                               (is-tied-to r2)
                               (or 
                                ;; could be e+s or s+e
                                (equal-within-tolerance (duration r1)
                                                        (* 2.0 (duration r2)))
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
                            (unless (is-tied-from r2)
                              (setf (is-tied-from new) nil))
                            (setf (num-flags new) 
                              (rthm-num-flags (/ 4 (* (duration new) 2/3))))
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
                  current-e is nil"))
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
       (when (is-tied-to new-e)
         (delete-marks new-e)))
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
;;; FUNCTION
;;; Remove any beaming indications from the rthm-seq-bar object. 
;;;
;;; NB: This method changes the data for the rthm-seq-bar object's BEAMS slot 
;;; and the individual BEAM slots of the RHYTHMs contained within the
;;; rthm-seq-bar's RHYTHMS slot. It does not change the value of the
;;; rthm-seq-bar's DATA slot.
;;;
;;; NB: Neither the presence nor absence of beams are not reflected in the
;;; output of the print-simple method.
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
;;; FUNCTION
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
;;; NIL
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
        (delete-tuplet-bracket r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; TODO: This case only handles beats which have the same rhythms in
;;; them, e.g. 7 septuplet 1/16ths.  We need a function to analyse a
;;; beat for that case, if so use this function, otherwise do the more
;;; complicated case.

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
;;; FUNCTION
;;; Given a rthm-seq-bar object with tuplet rhythms and an indication of which
;;; tuplet value to place, this method will automatically add the appropriate
;;; tuplet bracket it to the beats of the bar in the printed score output. If
;;; the TUPLET argument is set to NIL, the method will proceed on the basis of
;;; best-guessing rules.   
;;; 
;;; ARGUMENTS
;;; - A rthm-seq-bar object
;;; - An integer indicating the tuplet value (e.g. 3 for triplets, 5 for
;;; quintuplets etc.)
;;; 
;;; RETURN VALUE  
;;; Returns T.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - beat. The beat basis for the bar. If NIL (default), the beat is taken
;;; from the time signature.
;;; - beat-number. The beat number within the bar to look for tuplets. If set
;;; to T (default), all beats in the bar will be examined for possible
;;; tuplets. 
;;; - delete. Whether to delete the tuplet bracket indicators already present
;;; in the given rthm-seq-bar object (default = T).
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
|#
;;; SYNOPSIS
(defmethod auto-put-tuplet-bracket-on-beats 
  ((rsb rthm-seq-bar) tuplet 
   &optional 
   (beat nil)
   ;; can be a beat number or t for all 
   (beat-number t)
   ;; delete the tuplets already there?
   (delete t))
;;; ****
  (when delete 
    (delete-tuplets rsb))
  (if (not tuplet)
      (figure-out-and-auto-set-tuplets rsb beat)
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

;;; 12.12.11 SAR: Added ROBODoc info
;;; ****m* rthm-seq-bar/auto-beam
;;; FUNCTION
;;; Automatically add beaming indications to RHYTHMs of the given rthm-seq-bar
;;; object.
;;;
;;; NB: This method does not modify the DATA slot of the rthm-seq-bar object
;;; itself. Instead, it modifies the BEAM value for the individual RHYTHMs.
;;; 
;;; ARGUMENTS 
;;; - A rthm-seq-bar object.
;;;
;;; OPTIONAL ARGUMENTS
;;; - The beat basis for the given rthm-seq-bar. This will affect which notes
;;; get beamed together. This value can be either numeric (4, 8 16 etc.) or
;;; CMN/CM-shorthand (q, e, s etc). If no beat is given, the method defaults
;;; this value to NIL and takes the beat from the current time signature. 
;;; - Check-dur. This argument can be set to T or NIL. If T, the method will
;;; make sure there is a complete beat of rhythms for each beat of the bar
;;; (default = T). 
;;; 
;;; RETURN VALUE  
;;; Returns NIL.
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
          (note-num 0))
      (loop for b in beats do
           (setf start nil
                 end nil)
           (loop for r in b do
              ;; 5/4/07: first of all delete any prior beams
                (setf (beam r) nil
                      is-note (not (is-rest r))
                      flags (and is-note
                                 (> (num-flags r) 0)))
                (when (and flags (not start))
                  (setf start note-num))
                (when flags
                  (setf end note-num))
                (when is-note
                  (incf note-num)))
           (when (and start end (/= start end))
             (push (list start end) result)))
      (setf (beams rsb) (reverse result))
      (update-rhythms-beam-info rsb))))
                
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-num-beats ((rsb rthm-seq-bar))
  (num-beats (get-time-sig rsb)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; If beat is nil, we'll get the beat from the time-sig; if check-dur we'll
;;; make sure we get a complete beat of rhythms for each beat of the bar
(defmethod get-beats ((rsb rthm-seq-bar) &optional beat check-dur)
  (let ((beat-dur (if (and beat (not (eq beat t)))
                      (duration (make-rhythm beat))
                      (beat-duration (get-time-sig rsb))))
        (current '())
        (beats '())
        (dur 0.0))
    (loop for r in (rhythms rsb) do
       ;; (print r)
         (push r current)
         (incf dur (duration r))
       ;; (when (>= dur beat-dur)
         (when (or (> dur beat-dur)
                   (equal-within-tolerance dur beat-dur .001))
           (when check-dur
             (unless (equal-within-tolerance dur beat-dur .001)
               (error "~a ~%rthm-seq-bar::get-beats: ~
                        Can't find an exact beat of rhythms ~
                        (dur: ~a beat-dur: ~a)!" 
                      rsb dur beat-dur)))
           (push (reverse current) beats)
           (setf dur 0.0
                 current nil)))
    ;; when a tuplet of some kind goes over a beat, then the last beat won't be
    ;; full: get it anyway!  
    (when current
      (push (reverse current) beats))
    (nreverse beats)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod gen-stats ((rsb rthm-seq-bar))
  (let ((rhythms (rhythms rsb)))
    (setf (num-rhythms rsb) (length rhythms)
          (is-rest-bar rsb) (if (not rhythms)
                                t
                              (when (and (= 1 (num-rhythms rsb))
                                         (is-rest (first rhythms)))
                                (setf (is-whole-bar-rest (first (rhythms rsb)))
                                  t)))
          ;; Store the number of notes that will be need for this bar,
          ;; i.e. how many were not rests or ties
          (notes-needed rsb) (loop for r in rhythms count
                                   (needs-new-note r))
          (num-rests rsb) (loop for r in rhythms count (is-rest r))
          (num-score-notes rsb) (- (num-rhythms rsb) (num-rests rsb))))
  rsb)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; We don't want to store the duration symbol (eg. e.) rather it's duration in
;;; qtr notes.

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
          (start-beam (get-nth-non-rest-rhythm (first data) rsb))
          (end-beam (get-nth-non-rest-rhythm (second data) rsb)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Call this when the the tuplets slot of the rsb has been altered: it will
;;; update the tuplets info of the rhythms accordingly.

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
                (error "~a~%rthm-seq-bar::update-rhythms-bracket-info: ~
                         Couldn't get rhythm with index ~a in bar ~a"
                       rhythms nth (bar-num rsb)))
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
            ;; set the rhythms inbetween start and end to be under the current
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
          ;;(slot-value sclist 'score-tuplets) (my-copy-list (score-tuplets rsb))
          (slot-value sclist 'tuplets) (my-copy-list (tuplets rsb))
          (slot-value sclist 'beams) (my-copy-list (beams rsb))
          (slot-value sclist 'bar-num) (bar-num rsb)
          (slot-value sclist 'old-bar-nums) (copy-list (old-bar-nums rsb))
          (slot-value sclist 'write-bar-num) (write-bar-num rsb)
          (slot-value sclist 'write-time-sig) (write-time-sig rsb)
          (slot-value sclist 'bar-line-type) (bar-line-type rsb)
          (slot-value sclist 'player-section-ref) (player-section-ref rsb)
          (slot-value sclist 'nth-seq) (nth-seq rsb)
          (slot-value sclist 'nth-bar) (nth-bar rsb))
    sclist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 12.12.11 SAR: Added ROBODoc info
;;; ****m* rthm-seq-bar/get-nth-non-rest-rhythm
;;; FUNCTION
;;; Get the nth non-rest rhythm object stored in the given rthm-seq-bar.  
;;; 
;;; ARGUMENTS
;;; - The zero-based index number indicating which non-rest-rhythm is sought.
;;; - The given rthm-seq-bar object in which to search.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - T or NIL indicating whether to print an error message
;;; if the given index is greater than the number of non-rest rhythms in the
;;; RHYTHMS list (minus one to compensate for the zero-based indexing).
;;; (Default = T).  
;;; 
;;; RETURN VALUE  
;;; A rhythm object.
;;;
;;; Returns NIL if the given index is higher than the highest possible index of
;;; non-rest rhythms in the given rthm-seq-bar object.
;;; 
;;; EXAMPLE
#|
;; The method returns a rhythm object when successful
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
        (error "~a rthm-seq-bar::get-nth-non-rest-rhythm: Couldn't get ~
                non-rest rhythm with index ~a for bar number ~a"
               (rhythms rsb) index (bar-num rsb))))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 12.12.11 SAR: Added ROBODoc info
;;; ****m* rthm-seq-bar/get-nth-rest
;;; FUNCTION
;;; Gets the rhythm object of the nth rest in a given rthm-seq-bar.
;;; 
;;; ARGUMENTS 
;;; - The zero-based index number indicating which rest is sought.
;;; - The given rthm-seq-bar object in which to search.
;;;
;;; OPTIONAL ARGUMENTS
;;; - T or NIL indicating whether to print an error message
;;; if the given index is greater than the number of rests in the RHYTHMS list
;;; (minus one to compensate for the zero-based indexing) (default = T).  
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
;;; FUNCTION
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
;;; T or NIL to indicate whether to interrupt and drop into the debugger with
;;; an error. Default = T.
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
;; Zero-based indexing. Returns a rhythm object when successful.
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

;; Interrupts with an error and drops into the debugger by default if the
;; specified index number is greater than the number of events in the
;; rthm-seq-bar. 
(let ((rsb (make-rthm-seq-bar '((2 4) q e s s))))
  (get-nth-event 4 rsb))

=>
rthm-seq-bar::get-nth-event: Couldn't get event with index 4
   [Condition of type SIMPLE-ERROR]

;; The error can be suppressed by setting the optional argument to NIL
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
;;; FUNCTION
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
;; Returns a rhythm object.
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
;;; FUNCTION
;;; Gets the rhythm object for the nth note in a given rthm-seq-bar that needs
;;; an attack, i.e. not a rest and not tied. 
;;; 
;;; ARGUMENTS 
;;; - The zero-based index number indicating which attack is sought.
;;; - The given rthm-seq-bar object in which to search.
;;;
;;; OPTIONAL ARGUMENTS
;;; - T or NIL indicating whether to print a warning message
;;; if the given index is greater than the number of attacks in the RHYTHMS
;;; list (minus one to compensate for the zero-based indexing) (default = T).  
;;; 
;;; RETURN VALUE  
;;; A rhythm object.
;;;
;;; Returns NIL if the given index is higher than the highest possible index of
;;; attacks in the given rthm-seq-bar object.
;;; 
;;; EXAMPLE
#|
;; The method returns a rhythm object when successful
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
;;; ****                                ;
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
;;; NB Always returns a new rsb
;;; Just re-creates scaled rhythms with beams etc. where appropriate.
;;; See time-sig::scale for details on how we create the new meter.

;;; ****m* rthm-seq-bar/scale
;;; Wed Dec 14 17:52:07 GMT 2011 SAR: Added robodoc info
;;; FUNCTION
;;; 
;;; 
;;; ARGUMENTS 
;;; 
;;; 
;;; RETURN VALUE  
;;; 
;;; 
;;; EXAMPLE
#|

|#
;;; SYNOPSIS
(defmethod scale ((rsb rthm-seq-bar) scaler
                  &optional (preserve-meter t) ignore1 ignore2)
;;; ****
  (declare (ignore ignore1) (ignore ignore2))
  (let* ((new-time-sig (scale (get-time-sig rsb) scaler preserve-meter))
         (result (make-rest-bar new-time-sig (write-time-sig rsb))))
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
                    for r = (get-nth-non-rest-rhythm i result)
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
    (update-rhythms-beam-info result t)
    result))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 12.12.11 SAR: Added ROBODoc info
;;; ****m* rthm-seq-bar/set-nth-attack
;;; FUNCTION
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
;;; is greater than the number of attacks in the RHYTHMS list (minus one to
;;; compensate for the zero-based indexing) (default = T).   
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
;;; FUNCTION
;;; Gets the rhythm object for the last note that needs an attack (i.e. not a
;;; rest and not a tied note) in a given rthm-seq-bar object.
;;; 
;;; ARGUMENTS 
;;; - The given rthm-seq-bar object.
;;;
;;; OPTIONAL ARGUMENTS
;;; - T or NIL indicating whether to print a warning message if the given index
;;; (minus one) is greater than the number of attacks in the RHYTHMS list
;;; (default = T). This is a carry-over argument from the get-nth-attack method
;;; called within the get-last-attack method and not likely to be needed for
;;; use with get-last-attack.
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
  (setf (rhythms rsb) (rhythms-to-events-list (rhythms rsb)))
  rsb)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Intra-phrasal looping
;;; Creates a list of new bars (with new time signatures) formed by chopping
;;; the current bar at points given in chop-points. This is a list of
;;; 2-element lists each of which specifies the start and end points of a beat
;;; in the unit specified by the unit argument. These chop points are then
;;; used for all beats in the bar (so specifying chop-points adding up to a
;;; quarter note then trying to apply that to a 5/8 bar won't work).

;;; SAR Thu Dec 29 16:50:33 EST 2011: Added robodoc info
;;; ****m* rthm-seq-bar/chop
;;; FUNCTION
;;;
;;; 
;;; ARGUMENTS 
;;; 
;;; 
;;; RETURN VALUE  
;;; 
;;; 
;;; EXAMPLE
#|

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
         ;; (denom (denom time-sig))
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
                                     ;; use the key word, it might be
                                     ;; overridden if poc is nil, so set it
                                     ;; below 
                                     ;; :is-rest (is-rest e)
                                     :is-tied-to (is-tied-to e)
                                     :duration t)))
               ;; MDE Wed Dec 14 17:37:42 2011 
               (setf (is-rest new) (is-rest e))
               (if new
                   (setf (is-grace-note new) (is-grace-note e)
                         (needs-new-note new) (needs-new-note e)
                         (beam new) (beam e)
                         (bracket new) (bracket e)
                         (marks new) (my-copy-list (marks e))
                         (marks-before new) (marks-before e)
                         (amplitude new) (amplitude e))
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
  (unless (typep tempo 'tempo)
    (setf tempo (make-tempo tempo)))
  (let ((qtr-dur (qtr-dur tempo))
        (time start-time)
        (time-qtrs start-time-qtrs)
        (bar-dur (bar-duration rsb tempo)))
    (loop for event in (rhythms rsb) do   
          (setf (start-time event) time
                ;; TODO: this isn't working here; the problem is we need to
                ;; keep track of a global midi start time just in the same way
                ;; that we're tracking real-time via the return of bar-dur from
                ;; this function
                (start-time-qtrs event) time-qtrs
                (duration-in-tempo event) (* (duration event) qtr-dur)
                (compound-duration-in-tempo event) 
                (* (compound-duration event) qtr-dur)
                (end-time event) (+ (start-time event) 
                                    (compound-duration-in-tempo event)))
          #|
      (when (midi-program-changes event)
         (format t "~&rsb::update-time: ~&~a ~a" 
       time (midi-program-changes event)))
      |#
          (incf time-qtrs (duration event))
          (incf time (duration-in-tempo event)))
    (unless (is-rest-bar rsb)
      ;; todo: find out why the rhythms don't add up exactly to the bar
      ;; duration.  
      (unless (equal-within-tolerance bar-dur (- time start-time) .002)
        (error "~a~%rthm-seq-bar::update-time: Duration of rhythms don't ~
                match that of bar: rhythms ~a secs : bar ~a secs"
               rsb (- time start-time) bar-dur)))
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
;;; FUNCTION
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
;;; FUNCTION
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
;;; FUNCTION
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

;;; Usually only struck (non-tied and non-rest) notes will have their ;
;;; compound-duration set to include any following tied notes, but when the ;
;;; first note of the bar is tied, this has to be the one to get the updated ;
;;; compound duration.  Tied first notes of the bar are handled separately with ;
;;; handle-first-note-ties in the rthm-seq class ;

    (defmethod update-compound-durations ((rsb rthm-seq-bar))
  ;; (print 'update-compound-durations) ;
  ;; 0 will ensure that if the first note is a tie, this will nevertheless be ;
  ;; updated                            ;
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
  ;; (print ignore-grace-notes)         ;
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
                 (when (and ignore-rests result (is-rest scaled-event))
                   (incf (compound-duration-in-tempo (first result))
                         (duration-in-tempo scaled-event)))))
         ;; 25/4/10: had to add this to make sure we get rests when we have a
         ;; bar of rests only (but is-rest-bar is nil...)
           finally (loop for r in (nreverse rests) do (push r result))))
    (setf result (nreverse result))
    (when (and (first result) (write-time-sig rsb))
      (set-midi-time-sig (first result) (get-time-sig rsb)))
    ;; (print (first result))           ;
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-simple ((rsb rthm-seq-bar) &optional written (stream t))
  (format stream "~&~a: " (get-time-sig-as-list rsb))
  (loop for r in (rhythms rsb) do
       (print-simple r written stream)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object :before ((i rthm-seq-bar) stream)
  (format stream "~%RTHM-SEQ-BAR: time-sig: ~a ~a, ~
                                  time-sig-given: ~a, ~
                                  bar-num: ~a, ~
                  ~%              old-bar-nums: ~a, ~
                                  write-bar-num: ~a, ~
                                  start-time: ~a, ~
                  ~%              start-time-qtrs: ~a, ~
                                  is-rest-bar: ~a, ~
                                  multi-bar-rest: ~a, ~
                  ~%              show-rest: ~a, ~
                                  notes-needed: ~a, ~
                  ~%              tuplets: ~a, ~
                                  nudge-factor: ~a, ~
                                  beams: ~a, ~
                  ~%              current-time-sig: ~a, ~
                                  write-time-sig: ~a, ~
                                  num-rests: ~a, ~
                  ~%              num-rhythms: ~a, ~
                                  num-score-notes: ~a, ~
                                  parent-start-end: ~a, ~
                  ~%              missing-duration: ~a, ~
                                  bar-line-type: ~a, ~
                                  player-section-ref: ~a, ~
                  ~%              nth-seq: ~a, ~
                                  nth-bar: ~a, ~
                                  rehearsal-letter: ~a, ~
                  ~%              all-time-sigs: (not printed for ~
                                  brevity's sake) ~
                  ~%              rhythms: ~a"
          (time-sig i) (get-time-sig-as-list i) (time-sig-given i) (bar-num i)
          (old-bar-nums i) (write-bar-num i)
          (start-time i) (start-time-qtrs i) (is-rest-bar i) (multi-bar-rest i)
          (show-rest i) (notes-needed i) (tuplets i)
          (nudge-factor i) (beams i) (current-time-sig i) (write-time-sig i) 
          (num-rests i) (num-rhythms i) (num-score-notes i) (parent-start-end i)
          (missing-duration i) (bar-line-type i) (player-section-ref i)
          (nth-seq i) (nth-bar i) (rehearsal-letter i) (rhythms i)))

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

;;; TODO: Fix: This doesn't work when there's two or more brackets
;;; over the notes!  When fixed uncomment call in get-cmn-data (next method)

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

;;; 
(defmethod get-lp-data ((rsb rthm-seq-bar) &optional
                        in-c (rehearsal-letters-font-size 18))
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
             (push (get-lp-data event in-c) result)))
    ;; attach the given rehearsal letter
    (when (rehearsal-letter rsb)
      (push (lp-rehearsal-letter rsb rehearsal-letters-font-size) result))
    ;; special bar line
    (push (case (bar-line-type rsb)
            (0 " | ")
            (1 " \\bar \"||\" ")
            (2 " \\bar \"|.\" ")
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
  ;; 4/4/06: don't do this here anymore, rather do it in sc::respell-notes so
  ;; that we can respell notes. 
  ;; (auto-accidentals rsb)
  ;; TODO: uncomment this when verify-brackets can handle multiple tuplets
  ;; (verify-brackets rsb)
  ;; bar nums are written over the bar line of this bar, so it's actually the
  ;; next bar, if you see what I mean  
  (let* ((e1 (get-nth-event 0 rsb))
         (bar-num (1+ (bar-num rsb))) 
         (mbr (numberp (multi-bar-rest rsb)))
         (result 
          (if (is-rest-bar rsb)
              (list (if (missing-duration rsb)
                        (apply #'cmn::rest 
                               (append 
                                ;; 1.3.11 got to turn the mark symbols into cmn
                                ;; marks. can use e1 instead of
                                ;; (get-nth-event 0 rsb))
                                (cmn::get-all-cmn-marks (marks e1))
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
                                (list (cmn::dots 0)
                                      ;; 3/4/07 not here anymore
                                      ;; (when (write-bar-num rsb)
                                      ;; (cmn::cmn-bar-number (bar-num rsb)))
                                      (when mbr
                                        (multi-bar-rest rsb))
                                      (unless (show-rest rsb)
                                        cmn::invisible)))))
                    (cmn::cmn-bar-line bar-num t (bar-line-type rsb)
                                       (rehearsal-letter rsb)))
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
                                        ;; bar-line not the event so make sure
                                        ;; bnum is nil 
                                        nil
                                        (write-bar-num rsb))
                               (bar-num rsb))
                  do
                  (when (marks-before event)
                    (loop for o in (marks-before event) 
                       ;; a clef change appears as a 2-element
                       ;; list e.g. (clef treble)
                       for cmn-o = (if (and (listp o)
                                            (equal (first o) 'clef))
                                       (cmn::cmn-get-clef (second o))
                                       (cmn::get-cmn-marks o))
                       do
                       (if (listp cmn-o)
                           (loop for co in cmn-o do (push co result))
                           (push cmn-o result))))
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
                  ;; at the first sight of a normal note, get rid of any grace
                  ;; notes we saw.
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
                bar-num t
                (bar-line-type rsb) (rehearsal-letter rsb))))))
    (when (write-time-sig rsb)
      (push (cmn::meter (get-time-sig-as-list rsb)) result))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Try and work out which notes in the bar need accidentals and which don't.
;;; todo: handle tied first notes: no accidental, but if the pitch is then
;;; repeated later in the bar, we need an accidental!
;;; 
;;; TODO: add cautionary when leaping from e.g. gs4 to g(n)5
;;; TODO: add cautionary when note preceded by clef change!

(defmethod auto-accidentals ((rsb rthm-seq-bar) 
                             &optional last-attack-previous-bar 
                                       written)
  (let* ((cautionary-distance 3)
         (porc nil)
         (scale (ml nil 140))) ;; enough for 20 octaves!
    ;; go through each note in the bar and see whether there was already an
    ;; accidental for this white-degree, if not, we need the accidental so do
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
                    (last-pos (when last (first last)))
                    (last-acc (when last (second last))))
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
;;; FUNCTION
;;; Transpose the pitches of event objects stored in a rthm-seq-bar object by a
;;; specified number of semitones (positive for up, negative for down).
;;;
;;; ARGUMENTS 
;;; - A rthm-seq-bar object.
;;; - A whole number (positive or negative).
;;;
;;; OPTIONAL ARGUMENTS
;;; - keyword argument :destructively. Set to T or NIL to indicate whether the
;;; slot values of the original rthm-seq-bar object should be changed or not
;;; (even though the method always returns a clone). T = change the
;;; originals. Default = NIL.
;;; - chord-function
;;; - pitch-function
;;; 
;;; RETURN VALUE  
;;; This method returns a clone of the rthm-seq-bar object whether the keyword
;;; argument :destructively is set to T or NIL. It does change the
;;; corresponding slot values of the original when set to T even though it
;;; returns the clone.
;;; 
;;; EXAMPLE
#|
;; Create a rthm-seq-bar object using make-event, transpose the contained
;; pitches destructively, and read the values of the corresponding slots to see
;; the change.
(let ((rsb (make-rthm-seq-bar `((3 8) ,@(loop repeat 3 
                                           collect (make-event 'c4 'e))))))
  (transpose rsb 3 :destructively 3)
  (loop for p in (rhythms rsb)
     collect (data (pitch-or-chord p))))

=> (EF4 EF4 EF4)

;; Do the same thing without the :destructively keyword being set to T
(let ((rsb (make-rthm-seq-bar `((3 8) ,@(loop repeat 3 
                                           collect (make-event 'c4 'e))))))
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

;;; This method is really just looking for enharmonically-re-spelled notes
;;; within a bar and trying to unify the spelling.
;;; 
;;; Although we're just concentrating on one bar here, we need the parent
;;; slippery chicken object and player (symbol) in order to get subsequent ties
;;; into the next bar from the present bar.
;;;
;;; todo: consider getting to first note with accidental, respelling it and
;;; seeing if this results in less accidentals in the bar (bar 201 pno-lh of
;;; cheat sheet for instance. 

;;; ****m* rthm-seq-bar/respell-bar
;;; FUNCTION
;;; 
;;; 
;;; ARGUMENTS 
;;; 
;;; 
;;; RETURN VALUE  
;;; 
;;; 
;;; EXAMPLE
#|

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
                  next-attack (get-nth-attack (1+ attack-num) rsb nil)
                  #|
    next-attack-p (when next-attack
                  (if written
                  (written-pitch-or-chord next-attack)
                  (pitch-or-chord next-attack)))
    |#
                  )
            ;; 9.2.11
            (unless p
              (error "rthm-seq-bar::respell-bar: p is nil in: ~%~a!" rsb))
            (when (enharmonics-exist rsb p t written)
              #|
    (or (or (chord-p last-attack-p)
              (not (bad-interval p-enh last-attack-p)))
              (or (chord-p next-attack-p)
              (not (bad-interval p-enh next-attack-p)))))
  |#
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
                                (pitch-or-chord e))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Mon Dec 26 11:52:16 EST 2011: Added robodoc info
;;; ****m* rthm-seq-bar/enharmonic
;;; FUNCTION
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
;;; - keyword argument :written. T or NIL to indicate whether the test is to
;;; handle the written or sounding pitch in the event. T = written. Default =
;;; NIL. 
;;; - keyword argument :force-naturals. T or NIL to indicate whether to force
;;; "natural" note names that contain no F or S in their name to convert to
;;; their enharmonic equivalent (ie, B3 = CF4)
;;; 
;;; RETURN VALUE  
;;; Always returns NIL.
;;; 
;;; EXAMPLE
#|
;; The method returns NIL.
(let ((rsb (make-rthm-seq-bar `((3 8) ,@(loop repeat 3 
                                           collect (make-event 'cs4 'e))))))
  (enharmonic rsb))

=> NIL

;; Create a rthm-seq-bar object with events, apply the enharmonic method, and
;; print the corresponding slots to see the changes
(let ((rsb (make-rthm-seq-bar `((3 8) ,@(loop repeat 3 
                                           collect (make-event 'cs4 'e)))))) 
  (enharmonic rsb)
  (loop for p in (rhythms rsb)
     collect (get-pitch-symbol p)))

=> (DF4 DF4 DF4)

;; By default, the method will not change white-key pitches
(let ((rsb (make-rthm-seq-bar `((3 8) ,@(loop repeat 3 
                                           collect (make-event 'c4 'e))))))
  (enharmonic rsb)
  (loop for p in (rhythms rsb)
     collect (get-pitch-symbol p)))

=> (C4 C4 C4)

;; This can be forced by setting the :force-naturals argument to T 
(let ((rsb (make-rthm-seq-bar `((3 8) ,@(loop repeat 3 
                                           collect (make-event 'c4 'e))))))
  (enharmonic rsb :force-naturals t)
  (loop for p in (rhythms rsb)
     collect (get-pitch-symbol p)))

=> (BS3 BS3 BS3)

;; Apply the set-written method to fill the WRITTEN-PITCH-OR-CHORD slot, print
;; its contents, apply the enharmonic method with the :written keyword argument
;; set to T, then print the pitch data of the same slot again to see the
;; change.  
(let ((rsb (make-rthm-seq-bar `((3 8) ,@(loop repeat 3 
                                           collect (make-event 'cs4 'e))))))
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
(defmethod enharmonic ((rsb rthm-seq-bar) &key written force-naturals)
;;; ****
  (loop for r in (rhythms rsb) do
       (enharmonic r :written written :force-naturals force-naturals)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; If the first rest in the bar is longer than the event, stick the event at
;;; the beginning and fill up the duration of the original rest with new
;;; rests. 
;;;
;;; NB this doesn't update time slots etc. so need to call update-slots
;;; elsewhere. 

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

(defmethod rhythms-to-events ((rsb rthm-seq-bar))
  (setf (rhythms rsb)
        (loop for r in (rhythms rsb) collect (clone-with-new-class r 'event)))
  rsb)

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
;;;  27.1.11

;;; Wed Dec 14 18:04:01 GMT 2011 SAR: Added robodoc info
;;; SAR Sat Dec 31 08:58:52 EST 2011: Added DATE back to robodoc info

;;; ****m* rthm-seq-bar/split
;;; DATE
;;; 27 Jan 2011
;;;
;;; FUNCTION
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
;;; - keyword argument :min-beats. This argument takes an integer value to
;;; indicate the minimum number of beats in any of the new rthm-seq-bar
;;; objects created. This serves as a guideline only and may occasionally be
;;; exceeded in value by the method. Default value = 2.
;;; - keyword argument :max-beats. This argument takes an integer value to
;;; indicate the maximum number of beats in any of the new rthm-seq-bar objects
;;; created. This serves as a guideline only and may occasionally be exceeded
;;; in value by the method. Default value = 5.
;;; - keyword argument :warn. Indicates whether to print a warning if the
;;; rthm-seq-bar object is unsplittable. Value T = print a warning. Defaults to
;;; NIL. 
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
    (loop with got-bar until (zerop num) do
         (setf got-bar nil)
         (loop for beats from min-beats to max-beats ;; by num-mult
            for this-num = (* num-mult beats)
            for bar = (make-rest-bar (list this-num denom) nil)
            for ate = (fill-with-rhythms
                       ;; no warning
                       bar rthms :new-id "bar-created-by-split" :warn nil 
                       :is-full-error nil)
            do
            (when ate
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
      (let* ((last-bar (first new-bars)) ; we're pushing ;
             (last-bar-ts (get-time-sig last-bar)))
        (setf (rhythms last-bar) (append (rhythms last-bar) rthms)
              (time-sig last-bar) (list (+ num-mult (num last-bar-ts))
                                        (denom last-bar-ts)))
        (decf num num-mult)))
    (if (zerop num)                     ; success
        (progn
          (loop for bar in new-bars do (gen-stats bar))
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
;;; FUNCTION
;;; Set the written pitch (as opposed to sounding; i.e., for transposing
;;; instruments) of an event object within a given rthm-seq-bar object. The
;;; sounding pitch remains unchanged as a pitch object in the PITCH-OR-CHORD
;;; slot, while the written pitch is added as a pitch object to the
;;; WRITTEN-PITCH-OR-CHORD slot. 
;;; 
;;; ARGUMENTS 
;;; - A rthm-seq-bar-object
;;; - A whole number (positive or negative) indicating the transposition by
;;; semitones.
;;; 
;;; RETURN VALUE  
;;; Always returns NIL.
;;; 
;;; EXAMPLE
#|
;; The method returns NIL
(let ((rsb (make-rthm-seq-bar  `((3 8) ,@(loop repeat 3 
                                           collect (make-event 'cs4 'e))))))
  (set-written rsb -2))

=> NIL

;; Set the written pitch transposition to 2 semitones lower, then check the
;; data of the WRITTEN-PITCH-OR-CHORD slot of each event to see the
;; corresponding pitches 
(let ((rsb (make-rthm-seq-bar `((3 8) ,@(loop repeat 3 
                                           collect (make-event 'cs4 'e))))))
  (set-written rsb -2)
  (loop for p in (rhythms rsb)
     collect (get-pitch-symbol p)))

=> (B3 B3 B3)

 |#
;;; SYNOPSIS
(defmethod set-written ((rsb rthm-seq-bar) transposition)
;;; ****
  (loop for event in (rhythms rsb) do
       (set-written event transposition)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Mon Dec 26 14:11:05 EST 2011 Added robodoc info
;;; ****m* rthm-seq-bar/delete-written
;;; FUNCTION
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
;; Create a rthm-seq-bar object consisting of events and print the contents of
;; the WRITTEN-PITCH-OR-CHORD slots to see they're set to NIL. Apply the
;; set-written method with a value of -2 and print the contents of the
;; WRITTEN-PITCH-OR-CHORD slots to see the data of the newly created pitch
;; objects. Apply the delete-written method and print the contents of the
;; WRITTEN-PITCH-OR-CHORD slots to see they're empty. 
(let ((rsb (make-rthm-seq-bar `((3 8) ,@(loop repeat 3 
                                           collect (make-event 'cs4 'e))))))
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
;;; FUNCTION
;;; Set the MIDI-channel and microtonal MIDI-channel for the pitch object
;;; of an event object within a given rthm-seq-bar object. Sets the
;;; MIDI-CHANNEL slot of all event objects contained in the rthm-seq-bar object
;;; to the same channel.
;;; 
;;; ARGUMENTS 
;;; - A rthm-seq-bar object.
;;; - A whole number indicating the MIDI channel to be used for the
;;; equal-tempered pitch material of the given rthm-seq-bar object.
;;; - A whole number indicating the MIDI channel to be used for microtonal
;;; pitch material of the given rthm-seq-bar object. 
;;; 
;;; RETURN VALUE  
;;; Always returns NIL.
;;; 
;;; EXAMPLE
#|
;; Create a rthm-seq-bar using event objects and check the MIDI-CHANNEL slots
;; of those event objects to see that they are NIL by default.
(let ((rsb (make-rthm-seq-bar `((3 8) ,@(loop repeat 3 
                                           collect (make-event 'cs4 'e))))))
  (loop for p in (rhythms rsb)
     collect (midi-channel (pitch-or-chord p))))

=> (NIL NIL NIL)

;; Apply the set-midi-channel method to the rthm-seq-bar object and read and
;; print the MIDI-CHANNEL slots of each of the individual events to see that
;; they've been set.
(let ((rsb (make-rthm-seq-bar `((3 8) ,@(loop repeat 3 
                                           collect (make-event 'cs4 'e))))))
  (set-midi-channel rsb 13 14)
  (loop for p in (rhythms rsb)
       collect (midi-channel (pitch-or-chord p))))

=> (13 13 13)

|#
;;; SYNOPSIS
(defmethod set-midi-channel ((rsb rthm-seq-bar) midi-channel
                             microtonal-midi-channel)
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
;;; FUNCTION
;;; Reset the 8VA slots of all event ojbects within a given rthm-seq-object to
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

;; Create a rthm-seq-bar object consisting of event objects, print the default
;; value of the 8VA slots for those events. Set the 8VA slots to 1 and print
;; the value of those slots to see the change. Apply the reset-8va method to
;; remove any values and reset the slots to NIL, and print the results.

(let ((rsb (make-rthm-seq-bar `((3 8) ,@(loop repeat 3 
                                           collect (make-event 'cs4 'e))))))
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
                   objects (not rhythms)."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Mon Dec 26 19:34:55 EST 2011: Added robodoc info
;;; SAR Sat Dec 31 09:06:51 EST 2011: Added DATE back

;;; ****m* rthm-seq-bar/set-8va
;;; DATE
;;; 23 Sep 2011 
;;; 
;;; FUNCTION
;;; Set the 8VA (ottava) slots of the event objects within a given rthm-seq-bar 
;;; object. This number can be positive or negative. Only the values 1, 0 and
;;; -1 are valid for the number of octaves to be tranposed.
;;; 
;;; ARGUMENTS 
;;; - A rthm-seq-bar object.
;;; - A number indicating the number of octaves to be transposed in either
;;; direction (ottava/ottava bassa).
;;; 
;;; RETURN VALUE  
;;; Always returns NIL.
;;; 
;;; EXAMPLE
#|

;; The method returns NIL

(let ((rsb (make-rthm-seq-bar `((3 8) ,@(loop repeat 3 
                                           collect (make-event 'cs4 'e))))))
  (set-8va rsb 1))

=> NIL

;; Create a rthm-seq-bar object with event objects, set the 8va slot to 1, and
;; access and print it to see it's new value.

(let ((rsb (make-rthm-seq-bar `((3 8) ,@(loop repeat 3 
                                           collect (make-event 'cs4 'e))))))
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
                   objects (not rhythms)."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Related functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 09.12.11 SEAN: Added ROBODoc info
;;; ****f* rthm-seq-bar/make-rthm-seq-bar
;;; FUNCTION
;;; Public interface for creating a rthm-seq-bar object, each instance of which
;;; which holds one of the individual bars that reside in a rhythmic
;;; sequence. 
;;;
;;; This class is responsible for parsing lists containing rhythms and time
;;; signatures, but not for parsing these things themselves--that is done by
;;; separate classes.  
;;; 
;;; ARGUMENTS 
;;; - A list of rhythmic durations, which may include ties and dots. Durations
;;; may be written as numeric (integer) values or may use the CM/CMN/SCORE
;;; alphabetic shorthand s=16, e=8, q=4, h=2, w=1. 
;;;
;;; make-rthm-seq-bar requires a time signature. If no time signature is
;;; provided, the most recently defined time signature will be used. If one is
;;; provided, it must be included as the first element of the data list. The
;;; time signature is formulated as an unquoted list containing two integers,
;;; the first being the number of beats in the bar and the second being the
;;; beat unit for the bar. 
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
;;; FUNCTION
;;; Make a rthm-seq-bar object that consists of a bar of rest.
;;; 
;;; ARGUMENTS  
;;; - The time signature of the rthm-seq-bar object to be made, as a quoted
;;; list.
;;; - T or NIL instruction on whether to print the time signature in score
;;; output. 
;;;
;;; OPTIONAL ARGUMENTS
;;; - show-rest. This argument indicates whether or not to print the rest in
;;; the printed score output (CMN/LilyPond). Default = T.
;;; 
;;; The remaining optional arguments are set internally by the 
;;; slippery-chicken class, but can be read by the user for debugging.
;;; - missing-duration: Indicates whether the bar is missing a duration. 
;;; - player-section-ref: The current player and section of the given
;;; rthm-seq-bar object.
;;; - nth-seq: The current sequenz (with a "z") of the given rthm-seq-bar
;;; object.  
;;; - nth-bar: The current bar number of the given rthm-seq-bar object. 
;;; 
;;; RETURN VALUE    
;;; A rthm-seq-bar object.
;;; 
;;; EXAMPLE
#|
(let ((rsb-rb (make-rest-bar '(2 4) nil t)))
  (format t "~%time-sig: ~a~%is-rest-bar: ~a~%write-time-sig: ~a~%show-rest: ~a~%"
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
(defun make-rest-bar (time-sig write-time-sig &optional 
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
;;; A { following by a number means that all the notes from now to the } will
;;; be enclosed in a bracket with the number inside.  This may be nested.  A -
;;; indicates beaming: the first - indicates start a beam, the second end the
;;; beam. 
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
                  ;; some rthms start with + to indicate that they are tied to
                  ;; previous notes (e.g. in previous bar).
                  (start-pos (if (char= #\+ (aref rthm-string 0))
                                 1
                                 0)))
             (1+ (count #\+ rthm-string :start start-pos)))))
    (when rhythms
      ;; the position at which which we got a '{'; zero-based and with e.g. 'e
      ;; x 5' expanded, i.e. this isn't just the element position in the given
      ;; list.  Also, this has to be a list for the case when we have complex
      ;; rhythms with more than one bracket.
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
            ;; when we hit a '{' then the next element will be the number that
            ;; goes in the bracket over the rhythms
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
               ((eq interned '{) (setq get-tuplet t)
                (incf got-left-brackets))
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
                                       (push (list start-beam (1- num-notes))
                                             beam-positions)
                                       (setq start-beam nil))
                                   (setq start-beam num-notes)))
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
               (get-tuplet (unless (integerp i)
                             (error "rthm-seq-bar::parse-rhythms:~%~
                                     Expected a tuplet number: ~a~%~a"
                                    interned rhythms))
                           (push interned tuplets)
                           (setq get-tuplet nil)
                           (incf expect-right-brackets))
               ((is-rqq-info interned)
                (multiple-value-bind
                    (rqq-rthms rqq-num-notes rqq-num-rthms)
                    (do-rqq interned)
                  (incf num-rthms rqq-num-rthms)
                  (incf num-notes rqq-num-notes)
                  (setf rthms (append (reverse rqq-rthms) rthms))))
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
        (setf rthms (nreverse rthms)
              ;; score-tuplet-positions (nreverse score-tuplet-positions)
              tuplet-positions (nreverse tuplet-positions))
        ;; MDE Wed Dec 14 14:22:35 2011 -- score-tuplet-positions now nil but
        ;; no need to remove: just keep same data structure so as not to
        ;; introduce bugs above 
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
;;; 

#+cmn
(defun do-rqq (rqq &optional the-notes bar-num process-event-fun (in-c t))
  ;; (format t "~%do-rqq: ~a" the-notes)
  (let* ((subdivs (second rqq))
         (aux (loop for i in subdivs
                  collect (do-rqq-aux (second i))))
         (rthms (loop for i in aux appending (first i)))
         (num-notes (loop for r in rthms count (not (is-rest r))))
         (num-cmn-notes 0)
         (were-notes (when the-notes (length the-notes)))
         (stripped (loop 
                       for i in aux 
                       for j in subdivs
                       for k = (second i)
                       collect (list (first j) k)
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
         ;; bracketted numbers indicating rests.
         (stripped (loop for i in rthms 
                       unless (is-grace-note i)
                       collect (id i))))
    (list rthms stripped)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 

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
          ;; those events not yet handled will have rqq-info as a list or t
          ;; if they are notes or rests but not grace-notes
          (when (or (and rqqn (listp rqqn))
                    (eq t rqqn))
            (setf (rqq-note e) (pop cmn-notes))
            (push (get-cmn-data e bnum nil process-event-fun in-c) result)
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
  (and (listp thing)
       (numberp (first thing))
       (second thing)
       (listp (second thing))))

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
               ;; (nreverse result)))
               result))
           (number-error (num)
             (error "rthm-seq-bar::consolidate-rests-aux: ~a unhandled!"
                    num)))
      (cond ((= number 1) 
             (list rest))
            ((< number 1)
             (number-error number))
            ((power-of-2 (duration scaled))
             (list scaled))
            ((>= number 8)
             ;; (number-error number))
             (consolidate number 8))
            ((>= number 4) 
             (consolidate number 4))
            ((>= number 2) 
             (consolidate number 2))
            (t (number-error number))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; if match-rhythm is given (probably a beat), we'll only consolidate those.
;;; todo: this somehow misses (s) s+e (should be (s) e.)  
;;; 5/4/07 todo: it misses a lot actually; only handles simple cases but this
;;; suffices for now.

(defun consolidate-notes-aux (rhythms &optional (bar-num -1) match-rhythm)
  ;; (print 'consolidate-notes-aux-in)
  ;; (print-rhythms-rqs rhythms)
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
                               (error "~a~%rthm-seq-bar::~
                                       consolidate-notes-aux: ~
                                       bar num ~a: Should have got a letter ~
                                       (sum = ~a)"
                                      bar-num rhythms sum)))
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
  
#|
(defun consolidate-notes-aux5 (rthms)
  (print 'aux5)
  (print-simple-rthm-list rthms)
  (let ((ties (get-tied-rthms rthms))
        (result '()))
;; (print-simple-list (flatten ties))   ; ;
    (terpri)
    (loop for tied in ties 
       for tied-sum = (when tied (sum-rhythms-duration tied))
       with now = 0.0
       do 
       (when tied
  ;; (print tied)                       ; ;
         (push 
          (if (> (length tied) 1)
              (let ((rat (make-tied
                          (rationalize-if-necessary 
                           tied-sum
                           :rest nil
                           :keep-it-simple t
                           :error-on-fail t))))
                (if (or (= (length tied) (length rat))
        ;; 14.2.11 we don't want tuplets being made into ; ;
        ;; non-tuplets scanning beats e.g. tq te+tq te ; ;
        ;; becoming tq q te             ; ;
                        (and (not (float-int-p now))       ; are we on a beat? 
                             (/= 1 (print (tuplet-scaler (first result))))
;; (= 1 (tuplet-scaler (first rat)))))  ; ;
                             (>= (- (print (floor (+ now tied-sum)))
                                    (print (floor now)))
                                 1)))
                    tied
                    rat))
              (first tied))
          result)
         (incf now tied-sum)))
    (flatten (nreverse result))))
                  |#
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(defun get-tied-rthms (rhythms)
  (let ((result '())
        (temp '()))
    (flet ((do-temp ()
             (when temp
               (push (nreverse temp) result)
               (setf temp nil))))
      (loop 
          for r in rhythms do
            (cond ((and (is-tied-from r)
                        (not (is-tied-to r)))
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
  (loop for r in rthms do
        (unless (rhythm-p r)
          (error "~a~%rthm-seq-bar::rhythms-all-rests?: not a rhythm!" r))
        (unless (is-rest r)
          (return nil))
      finally (return t)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; EOF rthm-seq-bar.lsp