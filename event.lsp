;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* rhythm/event
;;; NAME 
;;; event
;;;
;;; File:             event.lsp
;;;
;;; Class Hierarchy:  named-object -> linked-named-object -> rhythm -> event
;;;
;;; Version:          1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Implementation of the event class which holds data for
;;;                   the contstruction of an audible event, be it a midi note,
;;;                   a sample (with corresponding sampling-rate conversion
;;;                   factor) or chord of these types.
;;;
;;;                   It is generally assumed that event instances will be
;;;                   created from (copies of) rhythm instances by promotion
;;;                   through the sc-change-class function, hence this class is
;;;                   derived from rhythm. 
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    March 19th 2001
;;;
;;; $$ Last modified: 10:14:22 Sat Dec 17 2011 ICT
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

(defclass event (rhythm)
  ((start-time :accessor start-time :initarg :start-time :initform nil)
   (end-time :accessor end-time :initform nil)
   ;; either a pitch or a chord object
   (pitch-or-chord :accessor pitch-or-chord :initarg :pitch-or-chord
                   :initform nil) 
   ;; when we have transposing instruments this is the written pitch/chord
   (written-pitch-or-chord :accessor written-pitch-or-chord
                           :initarg :written-pitch-or-chord :initform nil) 
   ;; 13.4.11 separate from transposing instruments, we have 8va/8vb signs that
   ;; force us to transpose the pitches an octave: indicate the number of
   ;; octaves to transpose here
   (8va :accessor 8va :type integer :initform 0)
   ;; the duration slot of rhythm is the duration in seconds of the rthm at
   ;; tempo qtr=60.  This is the duration adjusted for the actual tempo.
   (duration-in-tempo :accessor duration-in-tempo :type number :initform 0.0)
   (compound-duration-in-tempo :accessor compound-duration-in-tempo 
                               :type number :initform 0.0)
   ;; as of yet unused
   (score-marks :accessor score-marks :type list :initarg :score-marks 
                :initform nil)
   ;; 6/5/6 start time in crotchets: now that we're using tempo changes in the
   ;; midi output we have to have different times and durations for midi
   ;; events.  (If we tell CM to output an event at 0.5 secs but then write a
   ;; tempo of 120 into the midi file, then that event will actually start at
   ;; 0.25 secs.).
   (start-time-qtrs :accessor start-time-qtrs :type number :initarg
                    :start-time-qtrs :initform -1)
    ;; 4/5/06 a list e.g. '(6 8) that will be used to create a midi
   ;; time-signature event when this event is output to a midi file via cm.
   (midi-time-sig :accessor midi-time-sig :initarg :midi-time-sig 
                  :initform '())
   ;; these will be set automatically in sc-make-sequenz; this is a list of
   ;; two-element lists specifying the channel and the program; should be 1 or
   ;; two of them, depending on whether an instrument who generated this event
   ;; plays microtonal chords (i.e. plays on two midi channels).
   (midi-program-changes :accessor midi-program-changes :type list 
                         :initarg :midi-program-changes :initform nil)
   ;; 16.3.11 30,000ft over turkmenistan :) instead of writing an instrument
   ;; change as cmn text, indicate it here as plain strings--1 if there's just
   ;; the long name for the instrument, otherwise 2 if there's a short name
   ;; too. 
   (instrument-change :accessor instrument-change :type list :initform nil)
   ;; store the tempo when a change is made, otherwise leave at nil.  NB this
   ;; is a tempo object, not a simple bpm number.  
   (tempo-change :accessor tempo-change :initarg :tempo-change :initform nil)
   ;; whether to display the tempo-change or not
   (display-tempo :accessor display-tempo :type boolean 
                  :initarg :display-tempo :initform nil)
   ;; the bar number this event is in.
   (bar-num :accessor bar-num :type integer :initarg :bar-num :initform -1)
   ;; clefs etc. that come before a note. todo: 1.3.11 change this to
   ;; marks-before because we no longer store cmn objects, just symbols;
   ;; sim for bar-holder add method
   (cmn-objects-before :accessor cmn-objects-before :type list 
                       :initarg :cmn-objects-before :initform nil)
   ;(rqq-notes :accessor rqq-notes :type list :initform nil :allocation :class)
   (amplitude :accessor amplitude :type float :initarg :amplitude 
              :initform 0.7)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod scale :around ((e event) scaler 
                          &optional
                          (clone t) 
                          (scale-start-time nil)
                          (time-offset 0.0))
  (declare (ignore clone))
  (let ((rthm (call-next-method))
        (start (start-time e)))
    (setf rthm (clone-with-new-class rthm 'event)
          (slot-value rthm 'start-time) (when start
                                          (+ time-offset
                                             (if scale-start-time 
                                                 (* scaler (start-time e))
                                               (start-time e))))
          (slot-value rthm 'duration) (* scaler (duration e))
          (slot-value rthm 'compound-duration) (* scaler (compound-duration e))
          (slot-value rthm 'duration-in-tempo)
          (* scaler (duration-in-tempo e))
          (slot-value rthm 'compound-duration-in-tempo)
          (* scaler (compound-duration-in-tempo e))
          (slot-value rthm 'end-time) (when (start-time rthm)
                                        (+ (start-time rthm)
                                           (compound-duration-in-tempo rthm)))
          (slot-value rthm 'pitch-or-chord) (basic-copy-object
                                            (pitch-or-chord e))
          (slot-value rthm 'written-pitch-or-chord)
          (basic-copy-object (written-pitch-or-chord e))
          (slot-value rthm '8va) (8va e)
          (slot-value rthm 'score-marks) (my-copy-list (score-marks e))
          (slot-value rthm 'display-tempo) (display-tempo e)
          (slot-value rthm 'cmn-objects-before) (my-copy-list
                                                 (cmn-objects-before e))
          (slot-value rthm 'midi-program-changes) (my-copy-list
                                                   (midi-program-changes e))
          (slot-value rthm 'amplitude) (amplitude e)
          (slot-value rthm 'bar-num) (bar-num e)
          (slot-value rthm 'midi-time-sig) (midi-time-sig e)
          (slot-value rthm 'start-time-qtrs) (start-time-qtrs e)
          (slot-value rthm 'instrument-change) (copy-list
                                                (instrument-change e))
          (slot-value rthm 'tempo-change) (when (tempo-change e)
                                            (clone (tempo-change e))))
    rthm))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize-instance :after ((e event) &rest initargs)
  (declare (ignore initargs))
  (when (pitch-or-chord e)
    (setf (pitch-or-chord e) (pitch-or-chord e))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m*event/set-midi-channel
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
(defmethod set-midi-channel ((e event) midi-channel microtonal-midi-channel)
;;; ****
  (let ((noc (pitch-or-chord e)))
    (when noc
      (if (is-chord e)
          (set-midi-channel noc midi-channel microtonal-midi-channel)
        (setf (midi-channel noc) midi-channel)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m*event/get-midi-channel
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
(defmethod get-midi-channel ((e event))
  (let ((noc (pitch-or-chord e)))
    (when noc
      (if (is-chord e)
          ;; nb this will just return the midi-channel of the first pitch in
          ;; the chord list so if there are microtones or other midi-channels
          ;; on the other pitches this might not suffice 
          (get-midi-channel noc)
          (midi-channel noc)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; this should work even in rests, i.e. time sigs, tempo changes, and program
;;; changes will all be written despite no new pitches.

#+cm-2
;;; ****
;;; ****m*event/output-midi
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
(defmethod output-midi ((e event) &optional (time-offset 0.0) force-velocity)
  ;; 14.3.11: can't output events that haven't got time etc.
  (unless (start-time e)
    (error "event::output-midi: start-time nil! Call update slots perhaps:~%~a"
           e))
  (let ((noc (pitch-or-chord e))
        ;; 4.8.10 if start-time-qtrs hasn't been set, use start-time instead
        (time (+ time-offset 
                 (if (> (start-time-qtrs e) 0)
                     (start-time-qtrs e)
                     (start-time e))))
        (result '())
        (tc (tempo-change e))
        (pcs (midi-program-changes e))
        (ts (midi-time-sig e)))
    (flet ((store-it (it)
             (if (atom it) ;; it's a single cm midi event generated by a pitch
                 (push it result)
               ;; it's a list of cm midi events generated by a chord
               (loop for n in it do (push n result)))))
      (when pcs
        (loop 
            for pc in pcs 
            for channel = (first pc)
            for program = (second pc)
            do
              (store-it (cm::midi-program-change time channel program))))
      (when tc
        (store-it (cm::output-midi-tempo-change time tc)))
      (when ts
        (store-it
         (cm::output-midi-time-sig time (num ts) (denom ts) (midi-clocks ts))))
      (when noc
        (store-it (output-midi-note noc
                                    time
                                    (if force-velocity
                                        force-velocity
                                        (amplitude e))
                                    ;; rhythm's compound-duration is in 
                                    ;; quarters
                                    (compound-duration e))))
      result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2.4.11: return a list of the dynamics attached to an event.
;;; ****m*event/get-dynamics
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
(defmethod get-dynamics ((e event))
;;; ****
  (remove-if #'(lambda (x) (not (is-dynamic x)))
             (marks e)))
;;; ****

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; NB This doesn't change the amplitude
;;; ****m*event/remove-dynamics
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
(defmethod remove-dynamics ((e event))
  (setf (marks e) 
        (remove-if #'(lambda (x) (is-dynamic x))
                   (marks e))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 5.4.11: remove existing dynamics if we're about to add one
(defmethod add-mark :before ((e event) mark &optional warn-rest)
  (declare (ignore warn-rest))
  (when (is-dynamic mark)
    (remove-dynamics e)))
;;; ****

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 15.3.11: update amplitude if we set a dynamic as a mark
(defmethod add-mark :after ((e event) mark &optional warn-rest)
  (declare (ignore warn-rest))
  (when (is-dynamic mark)
    ;; (remove-dynamics e)
    (setf (slot-value e 'amplitude) (dynamic-to-amplitude mark))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 3.2.11 change amplitude slot and automatically add a mark to set a
;;; corresponding dynamic NB Sean: check that the right label is attached in
;;; robodo i.e. not just event/amplitude but event/setf amplitude
;;; ****m*event/setf amplitude
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
(defmethod (setf amplitude) :after (value (e event))
;;; ****
  (unless value
    (error "event::(setf amplitude): value is nil!"))
  (unless (is-rest e)
    ;; delete existing dynamics first
    ;; (remove-dynamics e)
    (add-mark e (amplitude-to-dynamic value nil)))) ; no warning if > 1.0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m*event/setf tempo-change
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
(defmethod (setf tempo-change) (value (e event))
;;; ****
  (typecase value
    (tempo (setf (slot-value e 'tempo-change) (clone value)))
    (number (setf (slot-value e 'tempo-change) (make-tempo value)))
    (t (error "event::(setf temp-change): argument should be a number ~
               or tempo object: ~a" value))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod (setf pitch-or-chord) (value (e event))
  ;; 13.2.11 really have to change the written note too
  (let* ((wporc (written-pitch-or-chord e))
         (porc (pitch-or-chord e))
         (diff (when wporc (pitch- wporc porc))))
    (typecase value
      (pitch (setf (slot-value e 'pitch-or-chord) (clone value)))
      (chord (setf (slot-value e 'pitch-or-chord) (clone value))
             ;; the cmn-data for a chord should be added to the event (whereas
             ;; the cmn-data for a pitch is only added to that pitch, probably
             ;; just a note-head change) 
             (loop for m in (marks value) do
                  (add-mark e m)))
      ;; 26/3/07: nil shouldn't result in making a chord!
      (list (setf (slot-value e 'pitch-or-chord)
                  (if value
                      (make-chord value :midi-channel (get-midi-channel e))
                      ;; 23.3.11 nil needs to set is-rest slot too!
                      (progn 
                        (setf (is-rest e) t)
                        nil))))
      (symbol (setf (slot-value e 'pitch-or-chord) 
                    (make-pitch value :midi-channel (get-midi-channel e)))))
    (when (pitch-or-chord e)
      (setf (is-rest e) nil))
    (when wporc
      (setf (slot-value e 'written-pitch-or-chord)
            (if (pitch-or-chord e)
                (transpose (pitch-or-chord e) diff)
                nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object :before ((i event) stream)
  (format stream "~%EVENT: start-time: ~a, end-time: ~a, ~
                  ~%       duration-in-tempo: ~a, ~
                  ~%       compound-duration-in-tempo: ~a, ~
                  ~%       amplitude: ~a, score-marks: ~a,  ~
                  ~%       bar-num: ~a, cmn-objects-before: ~a, ~
                  ~%       tempo-change: ~a ~
                  ~%       instrument-change: ~a ~
                  ~%       display-tempo: ~a, start-time-qtrs: ~a, ~
                  ~%       midi-time-sig: ~a, midi-program-changes: ~a, ~
                  ~%       8va: ~a~
                  ~%       pitch-or-chord: ~a~
                  ~%       written-pitch-or-chord: ~a"
          (start-time i) (end-time i) (duration-in-tempo i) 
          (compound-duration-in-tempo i)
          (amplitude i) (score-marks i)
          (bar-num i) (cmn-objects-before i) (tempo-change i)
          (instrument-change i)
          (display-tempo i) (start-time-qtrs i) 
          (when (midi-time-sig i)
            (data (midi-time-sig i)))
          (midi-program-changes i)
          (8va i) (pitch-or-chord i)
          (written-pitch-or-chord i)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone ((e event))
  (clone-with-new-class e 'event))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone-with-new-class :around ((e event) new-class)
  (declare (ignore new-class))
  (let ((rthm (call-next-method)))
    (copy-event-slots e rthm)
    rthm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Don't forget to copy slots over in the scale method above as well!

(defmethod copy-event-slots ((from event) (to event))
  (setf (slot-value to 'start-time) (start-time from)
        (slot-value to 'end-time) (end-time from)
        (slot-value to 'bar-num) (bar-num from)
        (slot-value to 'display-tempo) (display-tempo from)
        (slot-value to 'pitch-or-chord) (basic-copy-object
                                         (pitch-or-chord from))
        (slot-value to 'written-pitch-or-chord) 
        (basic-copy-object (written-pitch-or-chord from))
        (slot-value to 'score-marks) (my-copy-list (score-marks from))
        (slot-value to 'cmn-objects-before) (my-copy-list
                                             (cmn-objects-before from))
        ;; this is actually from the rhythm class but we need it in any case
        (slot-value to 'marks) (my-copy-list (marks from))
        (slot-value to 'duration-in-tempo) (duration-in-tempo from)
        (slot-value to 'compound-duration-in-tempo) 
        (compound-duration-in-tempo from)
        (slot-value to 'amplitude) (amplitude from)
        (slot-value to '8va) (8va from)
        (slot-value to 'tempo-change) (when (tempo-change from)
                                        (clone (tempo-change from)))
        (slot-value to 'instrument-change) (copy-list
                                              (instrument-change from))
        (slot-value to 'start-time-qtrs) (start-time-qtrs from)
        (slot-value to 'midi-program-changes) (my-copy-list
                                               (midi-program-changes from))
        (slot-value to 'midi-time-sig) (when (midi-time-sig from)
                                         (clone (midi-time-sig from))))
  to)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod porc-equal ((e1 event) (e2 event))
  (or (and (is-single-pitch e1)
           (is-single-pitch e2)
           (pitch= (pitch-or-chord e1) (pitch-or-chord e2)))
      (and (is-chord e1)
           (is-chord e2)
           (chord-equal (pitch-or-chord e1) (pitch-or-chord e2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; In the following methods the optional argument refers to whether we should
;;; handle the written or sounding pitch in the event. 

;;; ****m*event/sharp-p
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
(defmethod sharp-p ((e event) &optional written)
;;; ****
  (when (is-single-pitch e)
    (sharp (if written
               (written-pitch-or-chord e)
             (pitch-or-chord e)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m*event/flat-p
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
(defmethod flat-p ((e event) &optional written)
  (when (is-single-pitch e)
    (flat (if written
              (written-pitch-or-chord e)
            (pitch-or-chord e)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m*event/natural-p
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
(defmethod natural-p ((e event) &optional written)
  (when (is-single-pitch e)
    (natural (if written
              (written-pitch-or-chord e)
            (pitch-or-chord e)))))
;;; ****

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; NB doesn't work on chords!
;;; ****m*event/enharmonic
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
(defmethod enharmonic ((e event) &key written force-naturals 
                       ;; 1-based
                       chord-note-ref)
  ;; 5/6/07 works on chords given a reference into the chord counting from 1
  ;; and the lowest note upwards 
  ;; 11.4.11: works on all notes in chords if chord-note-ref is nil
  (unless (is-rest e)
    (if (and (is-chord e)
             (not chord-note-ref))
        ;; (error "~a~&event::enharmonic: need a chord-note-ref for chords." e))
        (loop for i from 1 to (sclist-length (pitch-or-chord e)) do
             (enharmonic e :written written :force-naturals force-naturals
                         :chord-note-ref i)
             finally (return e))
        (let* ((porc (if written
                         (written-pitch-or-chord e)
                         (pitch-or-chord e)))
               pitch new)
          (when (and written (not porc))
            (warn "event::enharmonic: asked for written pitch but none; ~
                   using sounding instead: ~a" e)
            (setf written nil
                  porc (pitch-or-chord e)))
          (setf pitch (if (chord-p porc)
                          (get-pitch porc chord-note-ref)
                          porc))
          ;; 24.7.11 (Pula)
          (unless pitch
            (error "event::enharmonic: couldn't get pitch from ~a, ~
                    chord-note-ref = ~a" e chord-note-ref))
          ;; NB only does it on sharps and flats unless force-naturals!
          (when (or force-naturals (sharp pitch) (flat pitch))
            (setf new (enharmonic pitch)
                  (midi-channel new) (midi-channel pitch)
                  (show-accidental new) (if (eq (accidental new) 'n)
                                            nil
                                            (show-accidental pitch))
                  (marks new) (my-copy-list (marks pitch)))
            (if written
                (if (is-chord e)
                    (setf (nth (1- chord-note-ref) 
                               (data (written-pitch-or-chord e)))
                          new)
                    (setf (written-pitch-or-chord e) new))
                (if (is-chord e)
                    (setf (nth (1- chord-note-ref) 
                               (data (pitch-or-chord e)))
                          new)
                    (setf (pitch-or-chord e) new))))
          e))))
;;; ****

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m*event/pitch-
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
(defmethod pitch- ((e1 event) (e2 event))
;;; ****
  (pitch- (pitch-or-chord e1) (pitch-or-chord e2)))
;;; ****

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; NB could screw up timing info in a bar
;;; ****m*event/inc-duration
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
(defmethod inc-duration ((e event) inc)
  (if (and (numberp (duration-in-tempo e))
           (numberp (compound-duration-in-tempo e))
           (numberp (end-time e)))
      (progn
        (incf (duration-in-tempo e) inc)
        (incf (compound-duration-in-tempo e) inc)
        (incf (end-time e) inc))
      (error "~a~%~%event::inc-duration: can't incrememnt non-number slots ~
              duration-in-tempo, compound-duration-in-tempo, end-time."
             e)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; time-sig should be a time-sig object but we can't compile that class before
;; this one  
;;; ****m*event/set-midi-time-sig
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
(defmethod set-midi-time-sig ((e event) time-sig)
;;; **** 
  (setf (midi-time-sig e) (clone time-sig)))
;;; ****

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 25.6.11: for transitions from one playing state to another.

;;; In Lilypond, arrows with start and end text are made with
;;; TextSpanners. These have to be defined before the note on which the arrow
;;; will start and we have to know the start and end text in advance. So we'll
;;; add a CMN mark before which instead of being the usual symbol or string etc
;;; will be a list, the first element of which will be arrow, as an identifier,
;;; followed by the starting text and end text. This will be processed when we
;;; are writing the lilypond file to create the TextSpanner. We will also add
;;; here start-arrow as a CMN mark and this will be attached to the note. An
;;; end-arrow mark should be attached to the note where the end text should
;;; appear.

;;; ****m*event/add-arrow
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
(defmethod add-arrow ((e event) start-text end-text &optional warn-rest)
;;; ****
  (when (and warn-rest (is-rest e))
    (warn "~a~&event::add-arrow: add arrow to rest?" e))
  ;; 26.7.11 (Pula): if there's not start/end text the arrow won't be shown in
  ;; lilypond :/
  (when (or (and (stringp start-text) (zerop (length start-text)))
            (and (stringp end-text) (zerop (length end-text))))
    (error "~a~%event::add-arrow: start-text/end-text can't be an empty string!"
           e))
  (add-cmn-object-before e (list 'arrow start-text end-text))
  (add-mark e 'start-arrow))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 24.9.11
;;; ****m*event/add-trill
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
(defmethod add-trill ((e event) trill-note &optional warn-rest)
;;; ****
  (when (and warn-rest (is-rest e))
    (warn "~a~&event::add-trill: add trill to rest?" e))
  (add-cmn-object-before e 'beg-trill-a)
  (add-mark e (list 'trill-note trill-note)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 24.9.11
;;; ****m*event/end-trill
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
(defmethod end-trill ((e event))
  (add-mark e 'end-trill-a))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod add-cmn-object-before ((e event) cmn-object)
  ;; (print cmn-object)
  (push cmn-object (cmn-objects-before e)))
;;; ****

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m*event/add-clef
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
(defmethod add-clef ((e event) clef &optional ignore1 ignore2 ignore3)
;;; ****
  (declare (ignore ignore1 ignore2 ignore3))
  (let ((cl (list 'clef clef)))
    (unless (member cl (cmn-objects-before e) :test #'equal)
      (add-cmn-object-before e cl))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m*event/get-clef
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
(defmethod get-clef ((e event) &optional ignore1 ignore2 ignore3)
  (declare (ignore ignore1 ignore2 ignore3))
  (second (find-if #'(lambda (el) (when (listp el) 
                                    (eq 'clef (first el))))
                   (cmn-objects-before e))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m*event/delete-clefs
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
(defmethod delete-clefs ((e event) &optional (warn t) ignore1 ignore2)
  (declare (ignore ignore1 ignore2))
  ;; 12.4.11 warn if no clef
  (if (get-clef e)
      (setf (cmn-objects-before e)
            (remove-if #'(lambda (x) (and (listp x) (eq (first x) 'clef)))
                       (cmn-objects-before e)))
      (when warn
        (warn "event::delete-clefs: no clef to delete: ~a" e))))
;;; ****

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m*event/get-amplitude
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
(defmethod get-amplitude ((e event) &optional (midi nil))
  (if midi
      (round (* (amplitude e) 127))
    (amplitude e)))
;;; ****

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m*event/get-pitch-symbol
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
(defmethod get-pitch-symbol ((e event) &optional (written t))
  (let ((obj (if (and written (written-pitch-or-chord e))
                 (written-pitch-or-chord e)
               (pitch-or-chord e))))
    (when obj
      (if (chord-p obj)
          (get-pitch-symbols obj)
        (id obj)))))
;;; ****

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod add-bracket-offset ((e event) &key (dx nil) (dy nil) 
                                              (dx0 nil) (dy0 nil) 
                                              (dx1 nil) (dy1 nil) 
                                              (index 0))
  (let* ((brackets (bracket e))
         (bracket (nth index brackets))
         (result (make-list 8 :initial-element nil)))
    (unless brackets
      (error "~a~%event::add-bracket-offset: no bracket for this event!"
            e))
    (unless bracket
      (error "~a~%event::add-bracket-offset: no bracket with index ~a!"
             e index))
    (unless (listp bracket)
      (error "~a~%event::add-bracket-offset: no start bracket with index ~a!"
             e index))
    (loop for n in bracket and i from 0 do
          (setf (nth i result) n))
    (setf (third result) dx
          (fourth result) dy
          (fifth result) dx0
          (sixth result) dy0
          (seventh result) dx1
          (eighth result) dy1
          (nth index (bracket e)) result)))
;;; ****

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m*event/no-accidental
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
(defmethod no-accidental ((e event))
;;; ****
  (no-accidental (pitch-or-chord e))
  (when (written-pitch-or-chord e)
    (no-accidental (written-pitch-or-chord e))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod from-8ve-transposing-ins ((e event))
  (let* ((porc (pitch-or-chord e))
         (wporc (written-pitch-or-chord e))
         (sounding (if (is-chord e)
                       (first (data porc))
                     porc))
         (written (when wporc
                    (if (is-chord e)
                        (first (data wporc))
                      wporc))))
    (when written
      (is-octave sounding written))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m*event/get-dynamic
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
(defmethod get-dynamic ((e event))
;;; ****
  (loop for m in (marks e) do
       (when (member m '(niente pppp ppp pp p mp mf f ff fff ffff))
             (return m))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;  lilypond
;;; test nested tuplets, grace notes
(let ((grace-notes '()))
  (defmethod get-lp-data ((e event) &optional in-c ignore)
    (declare (ignore ignore))
    (when (and (not in-c) (not (is-rest e)) (not (written-pitch-or-chord e)))
      (error "event::get-lp-data: request for non-existent ~
              transposed pitch: ~%~a" e))
    ;; 2.3.11: some marks (or rather noteheads) need to be set before in
    ;; lilypond but with the note in cmn; so move these over here 
    (multiple-value-bind 
          (from to)
        ;; 13.4.11 the start of 8va marks need to be before the note, but the
        ;; end comes after the note in order to include it under the bracket 
        (move-elements '(circled-x x-head triangle flag-head beg-8va beg-8vb
                         hairpin0 beg-trill-a triangle-up mensural <<)
                       (marks e) (cmn-objects-before e))
      (setf (marks e) from
            (cmn-objects-before e) to))
    (let* ((poc (if (and in-c (not (from-8ve-transposing-ins e)))
                    (pitch-or-chord e)
                    (written-pitch-or-chord e)))
           (note (cond ((is-rest e) "r")
                       ;; we handle these with the next normal notes
                       ((is-grace-note e) 
                        (push poc grace-notes))
                       ;; it's a note or chord
                       (t (get-lp-data poc))))
           (result '())
           (rthm (unless (is-grace-note e)
                   (round (nearest-power-of-2 (undotted-value e)))))
           ;; so, if the bracket slot is set, and the first element is a list,
           ;; we've got tuplet brackets so loop for each sublist and set the
           ;; e.g. \times 2/3 { to be the second element of the sublist.  if
           ;; it's just a positive integer, that's the end of the tuplet so
           ;; close with }.  otherwise, unless tuplet-scaler is 1, we've got
           ;; tuplets without brackets so use the e.g. 4*2/3 (tq) notation.
           ;; other than that, just use the nearest power of 2 to the value.
           ;; in all cases don't forget to add the dots.
           (close-tuplets 0))
      (when (instrument-change e)
        (let ((long (first (instrument-change e)))
              (short (second (instrument-change e))))
          (push (format nil "~a~%" (lp-set-instrument long)) result)
          (when short
            (push (format nil "~a~%" (lp-set-instrument short t)) result))
          (push (format nil "s1*0\^\\markup { ~a }~%" (lp-flat-sign long))
                result)))
      (when (cmn-objects-before e)
        (loop for thing in (cmn-objects-before e) do
             (if (and (listp thing) (eq (first thing) 'clef))
                 (push 
                  (if (eq 'percussion (second thing))
                      (format nil "~%~a~%" (lp-percussion-clef))
                      (format nil "~%\\clef ~a " 
                              (string-downcase (case (second thing)
                                                 (treble 'treble)
                                                 (bass 'bass)
                                                 (alto 'alto)
                                                 (tenor 'tenor)
                                                 ;; (percussion 'percussion)
                                                 (double-treble "\"treble^8\"")
                                                 (double-bass "\"bass_8\"")
                                                 (t (error "event::get-lp-data:~
                                                           unknown clef: ~a"
                                                           (second thing)))))))
                  result)
                 (push (lp-get-mark thing) result))))
      (when (and (tempo-change e) (display-tempo e))
        (push (get-lp-data (tempo-change e)) result))
      (unless (is-grace-note e)
        (when (bracket e)
          (loop for b in (bracket e) do
               (if (listp b)
                   (push 
                    (format nil "\\times ~a { " 
                            (case (second b)
                              (2 "3/2")
                              (3 "2/3")
                              (4 "3/4")
                              (5 "4/5")
                              (6 "4/6")
                              (7 "4/7")
                              (9 "8/9")
                              (t (error "event::get-lp-data: ~
                                         unhandled tuplet: ~a"
                                        (second b)))))
                    result)
                   (when (integer>0 b)
                     (incf close-tuplets)))))
        ;; hack-alert: if we're under two tuplet brackets our rhythm would be
        ;; twice as fast as it should be notated
        (when (> (length (bracket e)) 1)
          (setf rthm (/ rthm 2)))
        (when (and grace-notes (not (is-grace-note e)))
          (setf grace-notes (nreverse grace-notes))
          (case (length grace-notes)
            (1 (push (format nil "\\acciaccatura ~a8 " 
                             (get-lp-data (first grace-notes)))
                     result))
            (2 (push (format nil "\\acciaccatura \{ ~a8\[ ~a\] } " 
                             (get-lp-data (first grace-notes))
                             (get-lp-data (second grace-notes)))
                     result))
            (t (push (format nil "\\acciaccatura \{ ~a8\[ " 
                             (get-lp-data (first grace-notes)))
                     result)
               (loop for n in (rest (butlast grace-notes)) do
                    (push (format nil "~a " (get-lp-data n)) result))
               (push (format nil "~a\] } " 
                             (get-lp-data (first (last grace-notes))))
                     result)))
          ;; so it should always be nil at the end of a piece, right?
          (setf grace-notes nil))
        (push note result)
        (push (format nil "~a" rthm) result)
        (push (make-string (num-dots e) :initial-element #\.) result)
        (when (and (not (bracket e))    ; tuplets without brackets
                   (/= (tuplet-scaler e) 1))
          (push (format nil "*~a" (tuplet-scaler e)) result))
        (when (is-tied-from e)
          (push "~~" result))
        (when (beam e)
          (when (< rthm 8)
            (warn "event::get-lp-data: beam on rhythm (~a) < 1/8th duration: ~a"
                  rthm e))
          (if (zerop (beam e))
              (push "\]" result)
              (push "\[" result)))
        (push " " result)
        (when (marks e)
          ;; 22.5.11: getting a little tricky this but: in cmn we attach ottava
          ;; begin and end marks to the same note and everything's fine; in
          ;; lilypond, the begin or end must alwyays come before the note.  we
          ;; can't move the end to the next note's marks-before because
          ;; that wouldn't work in cmn, so just move it to the end of the
          ;; marks 
          (loop for mark in (move-to-end
                             'end-8va 
                             (move-to-end 'end-8vb (marks e)))
             for lp-mark = (lp-get-mark mark (num-flags e))
             do
             (when lp-mark
               (push lp-mark result))))
        (loop repeat close-tuplets do (push " \}" result))
        ;; (print result)
        (setf result
              (move-to-end ">> "
                           (move-to-end "} " (reverse result) #'string=)
                           #'string=))
        (list-to-string result "")))))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; cmn should understand the (duration ...) function but seems to fail with
;;; this, so use its rq function instead.

#+cmn
(defmethod get-cmn-data ((e event) &optional bar-num from-pitch-info-only
                         process-event-fun (in-c t) 
                         display-marks-in-part
                         print-time ignore1 ignore2)
  (declare (ignore ignore1 ignore2))
  ;; (print in-c)
  ;; (print (display-tempo e))
  (let ((porc (if (or (and (not in-c)
                           (written-pitch-or-chord e))
                      ;; don't transpose piccolo, db etc.
                      (and in-c
                           (from-8ve-transposing-ins e)))
                  (written-pitch-or-chord e)
                  (pitch-or-chord e))))
    ;; (print e)
    ;; todo: got to add bar num to rqq rhythms
    ;; call the event-processing function finally
    (when process-event-fun
      (funcall process-event-fun e))
    ;; 13.4.11 do the 8ve transposition if necessary
    (when (and porc (not (zerop (8va e))))
      (setf porc (transpose porc (* 12 (- (8va e))))))
    (cond ((and (not (is-rest e)) 
                (not (is-grace-note e))
                from-pitch-info-only)
           (cmn::cmn-note 
            (get-cmn-data porc nil nil)
            ;; (id (pitch-or-chord e))
            nil nil nil nil nil nil nil nil nil nil nil nil))
          ((rqq-note e)
           (if (is-rest e)
               (rqq-note e)
               (cmn::cmn-note nil (rqq-note e) nil (num-dots e) nil nil nil 
                              (is-tied-to e) (is-tied-from e) bar-num 
                              (append (marks e) 
                                      (when display-marks-in-part
                                        (marks e)))
                              (when (display-tempo e)
                                (cmn-tempo (tempo-change e)))
                              ;; can't set short name in cmn: it's auto-done
                              (first (instrument-change e)))))
          ((is-rest e) (cmn::cmn-rest (rq e) (num-dots e) (num-flags e) 
                                      (bracket e) bar-num 
                                      (append (marks e)
                                              (when display-marks-in-part
                                                (marks e)))
                                      (when print-time
                                        (cmn-time e))
                                      (when (display-tempo e)
                                        (cmn-tempo (tempo-change e)))
                                      (first (instrument-change e))))
          ;; All that happens here is that cmn::cmn-grace-note pushes
          ;; the note into *cmn-grace-notes-for-sc* which should be
          ;; added to the next real note 
          ((is-grace-note e) (cmn::cmn-grace-note 
                              (get-cmn-data porc nil nil 'e)
                              ;; (id (pitch-or-chord e))
                              (append (marks e)
                                      (when display-marks-in-part
                                        (marks e)))))
          ;; this note wasn't generated by a cmn rqq call so get the cmn note
          ;; and use it's duration, beaming etc. info. 
          (t (cmn::cmn-note (get-cmn-data porc nil nil)
                            ;; (id (pitch-or-chord e))
                            (rq e)
                            (num-dots e) (num-flags e) (beam e) (bracket e)
                            (is-tied-to e) (is-tied-from e)
                            bar-num 
                            (append (marks e)
                                    (when display-marks-in-part
                                      (marks e)))
                            (when print-time
                              (cmn-time e))
                            (when (display-tempo e)
                              (cmn-tempo (tempo-change e)))
                            (first (instrument-change e)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod cmn-time ((e event))
  (cmn::sc-cmn-text 
   (secs-to-mins-secs (start-time e))
   :font-size 6))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; If a chord, the return the number of notes in the chord.

;;; ****m*event/is-chord
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
(defmethod is-chord ((e event))
;;; ****
  (let ((noc (pitch-or-chord e)))
    (when (typep noc 'chord)
      (sclist-length noc))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m*event/is-single-pitch
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
(defmethod is-single-pitch ((e event))
  (typep (pitch-or-chord e) 'pitch))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 
;;; Transpose the event by semitones (not degrees!).  If functions are given,
;;; they will be used for the note or chord in the event, whereby semitones may
;;; or may not be nil in that case (transposition could be dependent on the
;;; note or chord and not a fixed shift.

;;; ****m*event/transpose
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
(defmethod transpose ((e event) semitones
                      &key
                      destructively
                      ;; the default functions are the class methods for pitch
                      ;; or chord.
                      (chord-function #'transpose)
                      (pitch-function #'transpose))
  ;; 22.7.11 (Pula): handle destructive case now
  ;; (declare (ignore destructively))
  (when (and (not (is-rest e))
             (not (pitch-or-chord e)))
    (error "event::transpose: ~a~%We have a an event that isn't a ~
            rest but has no note or chord slot!"
           e))
  (let* ((result (if destructively e (clone e)))
         (noc (pitch-or-chord result))
         (wnoc (written-pitch-or-chord result)))
    (unless (is-rest e)
      (setf (pitch-or-chord result)
            (if (pitch-p noc)
                (funcall pitch-function noc semitones)
                (funcall chord-function noc semitones)))
      (when wnoc
        ;; 20.7.11: got to handle the transposing instruments too
        (setf (written-pitch-or-chord result)
              (if (pitch-p wnoc)
                  (funcall pitch-function wnoc semitones)
                  (funcall chord-function wnoc semitones)))))
    result))
;;; ****

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m*event/set-written
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
(defmethod set-written ((e event) transposition)
;;; ****
  (when (pitch-or-chord e)
    (setf (written-pitch-or-chord e) 
      (transpose (clone (pitch-or-chord e)) transposition))))
;;; ****

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m*event/delete-written
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
(defmethod delete-written ((e event))
  (setf (written-pitch-or-chord e) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m*event/lowest
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
(defmethod lowest ((e event))
;;; ****
  (let ((porc (pitch-or-chord e)))
    (if (chord-p porc)
        (lowest porc)
      porc)))
;;; ****

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m*event/highest
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
(defmethod highest ((e event))
;;; ****
  (let ((porc (pitch-or-chord e)))
    (if (chord-p porc)
        (highest porc)
      porc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; returns distance in semitones from e1 to e2; chords taken into
;;; consideration. 

;;; ****m*event/event-distance
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
(defmethod event-distance ((e1 event) (e2 event))
;;; ****
  (let ((e1-high (highest e1))
        (e2-high (highest e2))
        (e1-low (lowest e1))
        (e2-low (lowest e2)))
    ;; only high notes are considered important for the 'feel' of the direction
    ;; here     
    (if (pitch> e2-high e1-high)
        ;; we're going up
        (- (midi-note-float e2-high) (midi-note-float e1-low))
      ;; we're going down
      (- (midi-note-float e2-low) (midi-note-float e1-high)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod bad-interval-p ((e1 event) (e2 event) &optional written)
  (when (and (is-single-pitch e1) (is-single-pitch e2))
    (bad-interval (get-porc e1 written) (get-porc e2 written))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-porc ((e event) &optional written)
  (if written
      (written-pitch-or-chord e)
    (pitch-or-chord e)))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; change notes to their enharmonics if the spelling is awkward.
;;; if written, act on the written pitches
;;; if e2-only then only change the spelling of the 2nd arg (useful when
;;; respelling chords)  

(defmethod respell ((e1 event) (e2 event) &optional written e2-only)
  (let ((e1-p (if written
                  (written-pitch-or-chord e1) 
                (pitch-or-chord e1)))
        (e2-p (if written
                  (written-pitch-or-chord e2)
                (pitch-or-chord e2)))
        (result nil))
    (flet ((rsp-enh (event-num &optional force-naturals)
             (let ((event (if (= event-num 1)
                              e1
                            e2)))
               (enharmonic event 
                           :written written 
                           :force-naturals force-naturals)
               (setf result event-num))))
      (when (and (is-single-pitch e1)
                 (is-single-pitch e2)
                 (not (micro-tone e1-p))
                 (not (micro-tone e2-p)))
        (cond ((dim2nd e1-p e2-p) ;; got an enharmonic!
               ;; (format t "~&e2-only: ~a e1 ~a e2 ~a -- " 
               ;;     e2-only (id e1-p) (id e2-p))
               (if (or e2-only
                       (natural e1-p)
                       ;; try and get the 'most natural' spelling
                       (<= (c5ths e1-p) (c5ths e2-p)))
                   (rsp-enh 2 t)
                 (rsp-enh 1))
               ;;(format t "e1 now ~a e2 ~a " 
               ;;      (id (pitch-or-chord e1)) (id (pitch-or-chord e2)))
               )
              ((and (sharp-p e1 written) (flat-p e2 written))
               (rsp-enh 2))
              ((and (flat-p e1 written) (sharp-p e2 written))
               (rsp-enh 2))
              ((aug2nd e1-p e2-p)
               (cond ((and (flat-p e1 written) (natural-p e2 written))
                      (if e2-only
                          (rsp-enh 2 t) ;; could result in df-ff
                        (rsp-enh 1)))
                     ((and (natural-p e1 written) (flat-p e2 written))
                      (rsp-enh 2))
                     ((and (natural-p e1 written) (sharp-p e2 written))
                      (rsp-enh 2))
                     ;; 28/3/07: surely this case isn't possible???
                     ;; ((and (natural-p e2 written) (sharp-p e1 written))
                     ;; (rsp-enh 1))
                     ))
              ;; what about the aug 5th, dim 3rd, dim 6th, dim 7th, dim 8ve, 
              ;; aug 8ve cases?
              ((dim4th e1-p e2-p) 
               ;; (print 'yes) 
               ;; assuming no diminished 4ths have a flat as the lower note!
               (if (or e2-only (flat-p e2 written))
                   (rsp-enh 2 t)
                 (rsp-enh 1 t)))
              ((aug5th e1-p e2-p)
               (if (or e2-only (natural-p e1 written))
                   (rsp-enh 2 t)
                 (rsp-enh 1 t)))
              ;; could be taken care of with first case no?
              ((augaug4th e1-p e2-p) (rsp-enh 2 t))
              ((dim3rd e1-p e2-p) (rsp-enh 2 t))
              ((aug3rd e1-p e2-p) (rsp-enh 2 t))
              ((dim6th e1-p e2-p) (rsp-enh 2 t))
              ((dim7th e1-p e2-p) 
               (if (or e2-only (flat-p e2 written))
                   (rsp-enh 2 t)
                 (rsp-enh 1 t)))
              ((dim8ve e1-p e2-p) 
               (if (or e2-only (not (sharp-p e1 written)))
                   (rsp-enh 2 t)
                 (rsp-enh 1 t)))
              ((aug8ve e1-p e2-p) 
               (if (or e2-only (sharp-p e2 written))
                   (rsp-enh 2 t)
                 (rsp-enh 1 t)))
              )
        result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m*event/force-rest
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
(defmethod force-rest :after ((e event))
;;; **** 
  (setf (pitch-or-chord e) nil
        (written-pitch-or-chord e) nil
        ;; 23.7.11 (Pula) remove marks that can only be used on a note
        (marks e) (remove-if #'mark-for-note-only (marks e))
        ;; (8va e) 0
        (cmn-objects-before e) (remove-if #'mark-for-note-only
                                          (cmn-objects-before e))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 22.9.11 
(defmethod reset-8va ((e event))
  (rm-marks e '(beg-8va beg-8vb end-8va end-8vb) nil)
  (setf (8va e) 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 20.8.11
;;; ****m*event/force-artificial-harmonic
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
(defmethod force-artificial-harmonic ((e event))
;;; ****
  (let* ((p1 (transpose (pitch-or-chord e) -24))
         (p2 (transpose p1 5)))
    (add-mark p2 'flag-head)
    (setf (pitch-or-chord e) (make-chord (list p1 p2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Related functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; code from "snow shoes..." days.  Called from the old clm methods.

(defun make-event-classic (pitch-or-chord start-time duration)
  (let ((result (make-instance 'event :data pitch-or-chord
                               :start-time start-time)))
    (setf (duration result) duration)
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****f* event/make-event
;;; FUNCTION
;;; make-event: create an event object for holding rhythm, pitch, and timing
;;; data. 
;;; 
;;; ARGUMENTS 
;;; - a pitch or chord: this can be one of those objects (will be added to the
;;;   pitch-or-chord slot without cloning), or a pitch symbol or list of pitch
;;;   symbols (for a chord)
;;; - the event's rhythm e.g. 'e.  If this is a number, its interpretation is
;;;   dependent on the value of duration (see below).  NB if this is a rhythm
;;;   object, it will be cloned. 
;;; - (key: start-time default nil): the start time of the event, in seconds.
;;; - (key: is-rest default nil): whether the event is a rest (t or nil)
;;; - (key: is-tied-to default nil): for score and playing purposes, whether
;;;   there's a tie to this event i.e. it won't sound indpendently.
;;; - (key: duration default nil): the duration of the event NB if duration t
;;;   then rthm is a duration in secs, not a known rhythm like 'e
;;;   i.e. (make-event 'c4 4 :duration nil) is  a quarter note, with duration 1,
;;;   but (make-event '(c4 d4) 4 :duration t) is a whole duration, with duration
;;;   4 (both assuming tempo of 60).
;;; - (key: amplitude default 0.7): amplitude of the event: 0.0->1.0
;;; - (key: tempo default 60): the tempo of the event, as a normal bpm number.
;;;   This is only used when creating the rhythm slots e.g. duration.
;;; - (key: midi-channel default nil): the midi channel the event should be
;;;   played back on 
;;; - (key: microtones-midi-channel default nil): if the event is microtonal,
;;;   the midi-channel the microtonal notes will be played on.
;;; 
;;; RETURN VALUE  
;;; an event object
;;; 
;;; EXAMPLE
;;; a whole-note (semi-breve) chord: (make-event '(c4 d4) 4 :duration t)
;;; a quarter-note (crotchet) c#:    (make-event 'cs4 4)
;;; 
;;; SYNOPSIS
(defun make-event (pitch-or-chord rthm &key 
                   start-time
                   is-rest
                   is-tied-to
                   duration
                   midi-channel
                   microtones-midi-channel
                   (amplitude 0.7)
                   (tempo 60))
;;; **** 
  ;; MDE Wed Dec 14 17:32:18 2011 
  (when (and pitch-or-chord is-rest)
    (error "event::make-event: an event can't have pitch data (~a) and be a rest:"
           pitch-or-chord))
  (let* ((r (make-rhythm rthm :is-rest is-rest :is-tied-to is-tied-to
                         :duration duration :tempo tempo))
         (e (when r (clone-with-new-class r 'event))))
    (when e
      (setf (start-time e) start-time
            (pitch-or-chord e) pitch-or-chord
            ;; 24.3.11 if we directly setf amp then we add a mark
            (slot-value e 'amplitude) amplitude)
      (when midi-channel
        (set-midi-channel e midi-channel microtones-midi-channel))
      e)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****f*event/make-rest
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
(defun make-rest (rthm &key start-time duration (tempo 60))
;;; ****
  (make-event nil rthm :start-time start-time :duration duration :tempo tempo
              :is-rest t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; list-of-events can be a simple list or an sclist
;;; if invert, then find the next non grace note

(defun find-next-grace-note (list-of-events start-index 
                             &optional 
                             (invert nil)
                             (warn t))
  (let* ((events (if (sclist-p list-of-events) 
                     (data list-of-events)
                   list-of-events))
         (max (if (sclist-p list-of-events) 
                  (sclist-length list-of-events)
                (length list-of-events)))
         (result (loop 
                     for i from start-index below max
                     for e = (nth i events)
                     do
                       (if invert
                           (unless (is-grace-note e)
                             (return i))
                         (when (is-grace-note e)
                           (return i))))))
    (unless result
      (when warn
        (warn "event::find-next-non-grace-note: Can't find next (non) ~
               grace-note.  start-index = ~a, length = ~a" start-index max)))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-next-non-grace-note (list-of-events start-index 
                                 &optional 
                                 (warn t))
  (find-next-grace-note list-of-events start-index t warn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 
;;; Given a list of numbers and a rhythm, create single rhythms surrounded by
;;; rests at distances indicated by the numbers
;;; e.g. (make-punctuation-events '(3 5 8) 's) -> s (e) s (q) s (e.) (q)
;;; notes can be a single note or a list of notes.  If the latter then they'll
;;; be popped off one after the other.

;;; ****f*event/make-punctuation-events
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
(defun make-punctuation-events (distances rhythm notes)
;;; ****
  (unless (listp notes)
    (setf notes (list notes)))
  (loop for d in distances
        with note 
        with rest = (make-rest rhythm)
        do
        (when notes
          (setf note (pop notes)))
        appending
        (cons (make-event note rhythm)
              (loop repeat (1- d) collect (clone rest)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; makes a list of events with the given data where a list indicates note (or
;;; chord) and rhythm and a single datum is the rhythm of a rest
;;; e.g. (make-events '((g4 q) e s ((fs3 g4) s)))

;;; ****f*event/make-events
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
(defun make-events (data-list &optional midi-channel microtones-midi-channel)
;;; ****
  (loop for data in data-list 
     for event =
     (if (listp data)
         (progn
           (let ((p (first data))
                 (r (second data)))
             (unless (= 2 (length data))
               (error "event::make-events: ~
                          Only single rhythms (for rests) or ~
                          (note/chord,rhythm) 2-element sublists are ~
                          acceptable: ~a"
                      data))
             (make-event (if (typep p 'named-object)
                             (clone p)
                             p)
                         (if (typep r 'named-object)
                             (clone r)
                             r)
                         :midi-channel midi-channel :microtones-midi-channel
                         microtones-midi-channel)))
         (make-rest data))
     collect event))
          

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 23.3.11: like make-events but rhythms and pitches given in separate lists
;;; so that e.g. we can make use of ties with '+'.  nil or r given as a pitch
;;; means it will be a rest otherwise e.g. 'cs4 will set a single pitch or
;;; e.g. '(cs4 ds5) will set a chord.  Pitches for tied notes only have to be
;;; given once.

;;; ****f*event/make-events2
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
(defun make-events2 (rhythms pitches
                     &optional midi-channel microtones-midi-channel)
;;; ****
  (let ((rhythms (rhythm-list rhythms))
        (ps (my-copy-list pitches))
        (poc nil))
    (loop for r in rhythms do
         (unless (is-tied-to r)
           (unless ps
             (error "event::make-events2: not enough pitches for rhythms: ~a ~a"
                    rhythms pitches))
           (setf poc (pop ps)))
         collect
       ;; remember that the fact that r is already a rhythm just means the
       ;; event won't have to parse it when cloning
         (if (or (not poc) (eq poc 'r))
             (make-rest r)
             (make-event poc r :midi-channel midi-channel
                         :microtones-midi-channel microtones-midi-channel)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****f*event/event-p
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
(defun event-p (thing)
;;; ****
  (typep thing 'event))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****f*event/sort-event-list
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
(defun sort-event-list (event-list)
  (sort event-list #'(lambda (x y) (< (start-time x) (start-time y)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****f* event/wrap-events-list
;;; FUNCTION
;;; wrap-events-list:
;;; Given a list of time-ascending events, wrap the list at a given point so we
;;; start there, go to the end, and keep going where the last event
;;; would have ended, using the start times added to this from those we skipped
;;; at the beginning.  NB If the first event doesn't start at 0, its start time
;;; will be conserved.
;;; 
;;; ARGUMENTS 
;;; - flat list of events
;;; - the event in the list to start at, either a time in seconds or a position
;;;   for nth
;;; - (key: time default nil): if nil, the the second argument is interpreted
;;;   as an index; if t, it's a time in seconds that we skip along to in the
;;;   events list.
;;; 
;;; RETURN VALUE  
;;; flat list of wrapped and time-adjusted events
;;; 
;;; DATE: 12.8.2010
;;; 
;;; SYNOPSIS
(defun wrap-events-list (events start-at &key (time nil))
;;; ****
  (let* ((start (if time
                    (loop for e in events and i from 0 do
                         (when (>= (start-time e) start-at)
                           (return i)))
                    start-at))
         (first-start (start-time (first events)))
         (subtract (start-time (nth start events)))
         ;; this will be the start time of the note that would come after the
         ;; last event in the list
         (end-start nil)
         (last-duration 0)
         (last-start 0)
         (time 0)
         (result '()))
    (loop for event in (wrap-list events start)
       for event-start = (start-time event)
       for subtraction = (- event-start subtract)
       do
       (setf time (if (< subtraction 0)
                      (progn
                        (unless end-start
                          ;; we have to use the duration of the last note to
                          ;; set the next start-time (i.e. when to wrap to the
                          ;; beginning) despite the fact that duration could be
                          ;; longer than the the intended rhythm
                          (setf end-start (- (+ last-start last-duration)
                                             first-start)))
                        (- (+ end-start event-start) first-start))
                      subtraction)
             (start-time event) (+ first-start time)
             last-duration (if (zerop (compound-duration-in-tempo event))
                               (compound-duration event)
                               (compound-duration-in-tempo event))
             last-start (start-time event))
       (push event result))
    (nreverse result)))
;;; ****

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****f*event/is-dynamic
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
(defun is-dynamic (mark)
;;; ****
  (member mark '(niente pppp ppp pp p mp mf f ff fff ffff)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; I like my percussion clef to have middle C as on a treble clef but lilypond
;;; has it as with an alto (I think)
(defun lp-percussion-clef ()
    "\\set Staff.middleCPosition = #-6 \\set Staff.clefGlyph = #\"clefs.percussion\" \\set Staff.clefPosition = #0 ")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; EOF event.lsp
