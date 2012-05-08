;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* rhythm/event
;;; NAME 
;;; event
;;;
;;; File:             event.lsp
;;;
;;; Class Hierarchy:  named-object -> linked-named-object -> rhythm -> event
;;;
;;; Version:          0.9.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Implementation of the event class which holds data for
;;;                   the construction of an audible event, be it a midi note,
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
;;; $$ Last modified: 18:37:22 Tue May  8 2012 BST
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
   ;; octaves to transpose here (currently limited to 1 or -1).
   (8va :accessor 8va :type integer :initform 0)
   ;; the duration slot of rhythm is the duration in seconds of the rthm at
   ;; tempo qtr=60.  This is the duration adjusted for the actual tempo.
   (duration-in-tempo :accessor duration-in-tempo :type number :initform 0.0)
   (compound-duration-in-tempo :accessor compound-duration-in-tempo 
                               :type number :initform 0.0)
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
   ;; clefs etc. that come before a note.  24.12.11 changed this to
   ;; marks-before because we no longer store cmn objects, just symbols; sim
   ;; for bar-holder add method 
   (marks-before :accessor marks-before :type list :initarg :marks-before
                 :initform nil)
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
          (slot-value rthm 'display-tempo) (display-tempo e)
          (slot-value rthm 'marks-before) (my-copy-list
                                                 (marks-before e))
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
  ;; (print 'event)
  (when (pitch-or-chord e)
    (setf (pitch-or-chord e) (pitch-or-chord e))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 23.12.11 SAR: Added robodoc info

;;; ****m* event/set-midi-channel
;;; FUNCTION
;;; Set the MIDI-channel and microtonal MIDI-channel for the pitch object
;;; within a given event object.
;;; 
;;; ARGUMENTS
;;; - An event object.
;;; - A whole number indicating the MIDI-channel to be used for playback of
;;;   this event object.
;;; - A whole number indicating the MIDI-channel to be used for playback of the
;;;   microtonal pitch material of this event.
;;; 
;;; RETURN VALUE
;;; Returns the value of the MIDI-channel setting (a whole number) if the
;;; MIDI-channel slot has been set, otherwise NIL.
;;; 
;;; EXAMPLE
#|

;; Unless specified the MIDI channel of a newly created event object defaults
;;; to NIL.
(let ((e (make-event 'c4 'q)))
  (midi-channel (pitch-or-chord e)))

=> NIL

(let ((e (make-event 'c4 'q)))
  (set-midi-channel e 7 8)
  (midi-channel (pitch-or-chord e)))

=> 7

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

;;; 23.12.11 SAR Added Robodoc info

;;; ****m* event/get-midi-channel
;;; FUNCTION
;;; Retrieve the value set for the midi-channel slot of the pitch object within
;;; a given event object.
;;; 
;;; ARGUMENTS
;;; - An event object.
;;; 
;;; RETURN VALUE
;;; An integer representing the given midi-channel value.
;;; 
;;; EXAMPLE
#|

;; The default midi-channel value for a newly created event-object is NIL
;;; unless otherwise specified.
(let ((e (make-event 'c4 'q)))
  (get-midi-channel e))

=> NIL

;; Create an event object, set its MIDI-channel and retrieve it
(let ((e (make-event 'c4 'q)))
  (set-midi-channel e 11 12)
  (get-midi-channel e))

=> 11

|#
;;; SYNOPSIS
(defmethod get-midi-channel ((e event))
;;; ****
  (let ((noc (pitch-or-chord e)))
    (when noc
      (if (is-chord e)
          ;; nb this will just return the midi-channel of the first pitch in
          ;; the chord list so if there are microtones or other midi-channels
          ;; on the other pitches this might not suffice 
          (get-midi-channel noc)
          (midi-channel noc)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sun Apr 29 14:05:45 BST 2012: Added robodoc entry

;;; MDE original comment:
;;; this should work even in rests, i.e. time sigs, tempo changes, and program
;;; changes will all be written despite no new pitches.

#+cm-2
;;; ****m* event/output-midi
;;; FUNCTION
;;; Generate the data necessary for MIDI output for a given event object.
;;;
;;; NB: The given event object must contain data for start-time and
;;;     midi-channel. 
;;; 
;;; ARGUMENTS
;;; - An event object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - A decimal number that is the number of seconds to offset the timing of
;;;   the MIDI output. 
;;; - A decimal number that is to override any other existing event object data
;;;   for amplitude.
;;; 
;;; RETURN VALUE
;;; Returns the data required for MIDI output. 
;;; 
;;; EXAMPLE
#|
;; Simple use
(let ((e (make-event 'c4 'q 
                     :start-time 0.0
                     :midi-channel 1))) 
  (output-midi e))

=> (#i(midi time 0.0 keynum 60 duration 1.0 amplitude 0.7 channel 0))

;; Specifying time offset and forced amplitude value
(let ((e (make-event 'c4 'q 
                     :start-time 0.0
                     :midi-channel 1))) 
  (output-midi e 0.736 0.3))

=> (#i(midi time 0.736 keynum 60 duration 1.0 amplitude 0.3 channel 0))

|#
;;; SYNOPSIS
(defmethod output-midi ((e event) &optional (time-offset 0.0) force-velocity)
;;; ****
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

;;; 23.12.11 SAR Added robodoc info

;;; ****m* event/get-dynamics
;;; FUNCTION
;;; Get the list of dynamic marks from a given event object, assuming there are
;;; multiple dynamics present. If other non-dynamic events are also contained
;;; in the MARKS slot of the rhythm object within the given event object, these
;;; are disregarded and only the dynamic marks are returned.  
;;;
;;; NB: This method is similar to the event::get-dynamic, but is intended for
;;; use should multiple dynamics have somehow become attached to the same
;;; event. The method event::get-dynamic is the method that should generally be
;;; used.
;;; 
;;; ARGUMENTS
;;; - An event object.
;;; 
;;; RETURN VALUE
;;; A list containing the dynamics stored in the MARKS slot of the rhythm
;;; object within the given event object. NIL is returned if no dynamic marks
;;; are attached to the given event object.
;;; 
;;; EXAMPLE
#|
;; Create an event object and get the dynamics attached to that object. These
;; are NIL by default (unless otherwise specified).
(let ((e (make-event 'c4 'q)))
  (get-dynamics e))

=> NIL

;; Create an event object, add one dynamic and one non-dynamic mark, print all
;; marks, then retrieve only the dynamics.
(let ((e (make-event 'c4 'q)))
  (add-mark-once e 'ppp)
  (add-mark-once e 'pizz)
  (print (marks e))
  (get-dynamics e))

=>
(PIZZ PPP)
(PPP)

;; Should multiple dynamics have become attached to the same event object,
;; get-dynamics will return all dynamics present in the MARKS slot
(let ((e (make-event 'c4 'q)))
  (add-mark-once e 'pizz)
  (add-mark-once e 'ppp)
  (push 'fff (marks e))
  (print (marks e))
  (get-dynamics e))

=> (FFF PPP)

|#
;;; SYNOPSIS
(defmethod get-dynamics ((e event))
;;; ****
  (remove-if #'(lambda (x) (not (is-dynamic x)))
             (marks e)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* event/remove-dynamics
;;; FUNCTION
;;; Remove all dynamic symbols from the list of marks attached to a given event
;;; object. 
;;; 
;;; NB: This doesn't change the amplitude.
;;; 
;;; ARGUMENTS
;;; - An event object.
;;; 
;;; RETURN VALUE
;;; Returns the modified list of marks attached to the given event object if
;;; the specified dynamic was initially present in that list and successfully
;;; removed, otherwise returns NIL.
;;; 
;;; EXAMPLE
#|
;; Create an event object, add one dynamic mark and one non-dynamic mark, print
;; all marks attached to the object, and remove just the dynamics from that
;; list of all marks.
(let ((e (make-event 'c4 'q)))
  (add-mark-once e 'ppp)
  (add-mark-once e 'pizz)
  (print (marks e))
  (remove-dynamics e))

=>
(PIZZ PPP)
(PIZZ)

;; Attempting to remove dynamics when none are present returns NIL.
(let ((e (make-event 'c4 'q)))
  (remove-dynamics e))

=> NIL

|#
;;; SYNOPSIS
(defmethod remove-dynamics ((e event))
;;; ****
  (setf (marks e) 
        (remove-if #'(lambda (x) (is-dynamic x))
                   (marks e))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 5.4.11: remove existing dynamics if we're about to add one
(defmethod add-mark :before ((e event) mark &optional warn-rest)
  (declare (ignore warn-rest))
  (when (is-dynamic mark)
    (remove-dynamics e)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 15.3.11: update amplitude if we set a dynamic as a mark
(defmethod add-mark :after ((e event) mark &optional (update-amplitude t))
  (when (and update-amplitude (is-dynamic mark))
    ;; (remove-dynamics e)
    (setf (slot-value e 'amplitude) (dynamic-to-amplitude mark))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Fri Dec 23 16:54:59 EST 2011 Added robodoc info

;;; ****m* event/setf amplitude
;;; FUNCTION
;;; Change the amplitude slot of a given event object and automatically add a
;;; mark to set a corresponding dynamic.
;;;
;;; Numbers greater than 1.0 and less than 0.0 will also be stored in the
;;; amplitude slot of the given event object without printing a warning, though 
;;; corresponding dynamic marks are only available for values between 0.0 and
;;; 1.0. Any value above 1.0 or below 0.0 will result in a dynamic marking of
;;; FFFF and NIENTE respectively.
;;;
;;; ARGUMENTS
;;; - An amplitude value (real number).
;;; - An event object.
;;; 
;;; RETURN VALUE
;;; Returns the specified amplitude value.
;;; 
;;; EXAMPLE
#|
;; When no amplitude is specified, new event objects are created with a default
;; amplitude of 0.7.
(let ((e (make-event 'c4 'q)))
  (amplitude e))

=> 0.7

;; Setting an amplitude returns the amplitude set
(let ((e (make-event 'c4 'q)))
  (setf (amplitude e) .3))

=> 0.3

;; Create an event object, set its amplitude, then print the contents of the
;; amplitude and marks slots to see the dynamic setting.
(let ((e (make-event 'c4 'q)))
  (setf (amplitude e) .3)
  (print (amplitude e))
  (print (marks e)))

=>
0.3 
(PP)

;; Setting an amplitude greater than 1.0 or less than 0.0 sets the amplitude
;; correspondingly and sets the dynamic mark to FFFF or NIENTE respectively.  
(let ((e1 (make-event 'c4 'q))
      (e2 (make-event 'c4 'q)))
  (setf (amplitude e1) 1.3)
  (setf (amplitude e2) -1.3)
  (print (marks e1))
  (print (marks e2)))

=>
(FFFF) 
(NIENTE)

|#
;;; SYNOPSIS
(defmethod (setf amplitude) :after (value (e event))
;;; ****
  (unless (and value (numberp value))
    (error "event::(setf amplitude): value is not a number: ~a" value))
  (unless (is-rest e)
    ;; add-mark :before will remove dynamics 
    ;; no warning if > 1.0 < 0.0
    ;; MDE Fri Dec 30 15:36:42 2011 -- add-mark updates amplitude if we give it
    ;; a dynamic but that would limit us to 0.0->1.0 so don't allow this here 
    (add-mark e (amplitude-to-dynamic value nil) nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Fri Dec 23 18:07:41 EST 2011 Added robodoc info

;;; ****m* event/setf tempo-change
;;; FUNCTION
;;; Store the tempo when a change is made. 
;;;
;;; NB: This creates a full tempo object, not just a number representing bpm. 
;;; 
;;; ARGUMENTS
;;; - An event object.
;;; - A number indicating the new tempo bpm.
;;; 
;;; RETURN VALUE
;;; Returns a tempo object.
;;; 
;;; EXAMPLE
#|
;; Creation of a new event object sets the tempo-change slot to NIL by default,
;; unless otherwise specified.
(let ((e (make-event 'c4 'q)))
  (tempo-change e))

=> NIL

;; The tempo-change method returns a tempo object
(let ((e (make-event 'c4 'q)))
  (setf (tempo-change e) 132))

=> 
TEMPO: bpm: 132, beat: 4, beat-value: 4.0, qtr-dur: 0.45454545454545453 
       qtr-bpm: 132.0, usecs: 454545, description: NIL
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: NIL, tag: NIL, 
data: 132

;; The new tempo object is stored in the event object's tempo-change slot.
(let ((e (make-event 'c4 'q)))
  (setf (tempo-change e) 132)
  e)

=> 
EVENT: start-time: NIL, end-time: NIL, 
       duration-in-tempo: 0.0, 
       compound-duration-in-tempo: 0.0, 
       amplitude: 0.7, score-marks: NIL,  
       bar-num: -1, cmn-objects-before: NIL, 
       tempo-change: 
TEMPO: bpm: 132, beat: 4, beat-value: 4.0, qtr-dur: 0.45454545454545453 
       qtr-bpm: 132.0, usecs: 454545, description: NIL
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: NIL, tag: NIL, 
data: 132
[...]

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
  (format stream "~%EVENT: start-time: ~,3f, end-time: ~,3f, ~
                  ~%       duration-in-tempo: ~,3f, ~
                  ~%       compound-duration-in-tempo: ~,3f, ~
                  ~%       amplitude: ~,3f ~
                  ~%       bar-num: ~a, marks-before: ~a, ~
                  ~%       tempo-change: ~a ~
                  ~%       instrument-change: ~a ~
                  ~%       display-tempo: ~a, start-time-qtrs: ~,3f, ~
                  ~%       midi-time-sig: ~a, midi-program-changes: ~a, ~
                  ~%       8va: ~a~
                  ~%       pitch-or-chord: ~a~
                  ~%       written-pitch-or-chord: ~a"
          (start-time i) (end-time i) (duration-in-tempo i) 
          (compound-duration-in-tempo i)
          (amplitude i) (bar-num i) (marks-before i) (tempo-change i)
          (instrument-change i) (display-tempo i) (start-time-qtrs i) 
          (when (midi-time-sig i)
            (data (midi-time-sig i)))
          (midi-program-changes i) (8va i) (pitch-or-chord i)
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
        (slot-value to 'marks-before) (my-copy-list
                                             (marks-before from))
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

;;; SAR Fri Dec 23 18:37:45 EST 2011 Added Robodoc info

;;; ****m* event/sharp-p
;;; FUNCTION
;;; Determine whether the pitch of a given event object has a sharp.
;;; 
;;; ARGUMENTS
;;; - An event object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to indicate whether the test is to handle the written or
;;;   sounding pitch in the event. T = written. Default = NIL. 
;;; 
;;; RETURN VALUE
;;; Returns T if the note tested has a sharp, otherwise NIL (ie, is natural or
;;; has a flat).
;;; 
;;; EXAMPLE
#|
;; Returns T when the note is sharp
(let ((e (make-event 'cs4 'q)))
  (sharp-p e))

=> T

;; Returns NIL when the note is not sharp (ie, is flat or natural)
(let ((e (make-event 'c4 'q)))
  (sharp-p e))

=> NIL

(let ((e (make-event 'df4 'q)))
  (sharp-p e))

=> NIL

|#
;;; SYNOPSIS
(defmethod sharp-p ((e event) &optional written)
;;; ****
  (when (is-single-pitch e)
    (sharp (if written
               (written-pitch-or-chord e)
             (pitch-or-chord e)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Fri Dec 23 18:46:59 EST 2011 Added robodoc info

;;; ****m* event/flat-p
;;; FUNCTION
;;; Determine whether the pitch of a given event object has a flat.
;;; 
;;; ARGUMENTS
;;; - An event object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to indicate whether the test is to handle the written or
;;;   sounding pitch in the event. T = written. Default = NIL. 
;;; 
;;; RETURN VALUE
;;; Returns T if the note tested has a flat, otherwise NIL (ie, is natural or
;;; has a sharp).
;;; 
;;; EXAMPLE
#|
;; Returns T when the note is flat
(let ((e (make-event 'df4 'q)))
  (flat-p e))

=> T

;; Returns NIL when the note is not flat (ie, is sharp or natural)
(let ((e (make-event 'c4 'q)))
  (flat-p e))

=> NIL

(let ((e (make-event 'cs4 'q)))
  (flat-p e))

=> NIL

|#
;;; SYNOPSIS
(defmethod flat-p ((e event) &optional written)
;;; ****
  (when (is-single-pitch e)
    (flat (if written
              (written-pitch-or-chord e)
            (pitch-or-chord e)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Fri Dec 23 18:52:55 EST 2011 Added robodoc info

;;; ****m* event/natural-p
;;; FUNCTION
;;; Determine whether the pitch of a given event object is a natural note (no
;;; sharps or flats).
;;; 
;;; ARGUMENTS
;;; - An event object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to indicate whether the test is to handle the written or
;;;   sounding pitch in the event. T = written. Default = NIL.
;;; 
;;; RETURN VALUE
;;; Returns T if the note tested is natural, otherwise NIL (ie, has a flat or 
;;; has a sharp).
;;; 
;;; EXAMPLE
#|
;; Returns T when the note is natural
(let ((e (make-event 'c4 'q)))
  (natural-p e))

=> T

;; Returns NIL when the note is not natural (ie, is sharp or flat)
(let ((e (make-event 'cs4 'q)))
  (natural-p e))

=> NIL

(let ((e (make-event 'df4 'q)))
  (natural-p e))

=> NIL

|#
;;; SYNOPSIS
(defmethod natural-p ((e event) &optional written)
;;; ****
  (when (is-single-pitch e)
    (natural (if written
              (written-pitch-or-chord e)
            (pitch-or-chord e)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Fri Dec 23 20:04:27 EST 2011 Added robodoc info

;;; ****m* event/enharmonic
;;; FUNCTION
;;; Change the pitch of the pitch object within the given event object to its
;;; enharmonic equivalent.   
;;;
;;; In its default form, this method only applies to note names that already
;;; contain an indication for an accidental (such as DF4 or BS3), while
;;; "white-key" note names (such as B3 or C4) will not produce an enharmonic
;;; equivalent. In order to change white-key pitches to their enharmonic
;;; equivalents, set the :force-naturals argument to T. 
;;; 
;;; NB: Doesn't work on chords.
;;; 
;;; ARGUMENTS
;;; - An event object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - keyword argument :written. T or NIL to indicate whether the test is to
;;;   handle the written or sounding pitch in the event. T = written. Default = 
;;;   NIL. 
;;; - keyword argument :force-naturals. T or NIL to indicate whether to force
;;;   "natural" note names that contain no F or S in their name to convert to 
;;;   their enharmonic equivalent (ie, B3 = CF4)
;;; 
;;; RETURN VALUE
;;; An event object.
;;; 
;;; EXAMPLE
#|
;; The method alone returns an event object
(let ((e (make-event 'cs4 'q)))
  (enharmonic e))

=> 
EVENT: start-time: NIL, end-time: NIL, 
[...]

;; Create an event, change it's note to the enharmonic equivalent, and print
;; it.
(let ((e (make-event 'cs4 'q)))
  (enharmonic e)
  (data (pitch-or-chord e)))

=> DF4

;; Without the :force-naturals keyword, no "white-key" note names convert to
;; enharmonic equivalents
(let ((e (make-event 'b3 'q)))
  (enharmonic e)
  (data (pitch-or-chord e)))

=> B3

;; Set the :force-naturals keyword argument to T to enable switching white-key
;; note-names to enharmonic equivalents
(let ((e (make-event 'b3 'q)))
  (enharmonic e :force-naturals t)
  (data (pitch-or-chord e)))

=> CF4

|#
;;; SYNOPSIS
(defmethod enharmonic ((e event) &key written force-naturals 
                       ;; 1-based
                       chord-note-ref)
;;; ****
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Fri Dec 23 20:32:23 EST 2011 Added robodoc info

;;; ****m* event/pitch-
;;; FUNCTION
;;; Determine the interval in half-steps between two pitches. 
;;; 
;;; NB: This is determined by subtracting the MIDI note value of one event from
;;; the other. Negative numbers may result if the greater MIDI note value is
;;; subtracted from the lesser.
;;;
;;; ARGUMENTS
;;; - A first event object.
;;; - A second event object.
;;; 
;;; RETURN VALUE
;;; A number.
;;; 
;;; EXAMPLE
#|
(let ((e1 (make-event 'c4 'q))
      (e2 (make-event 'a3 'q)))
  (pitch- e1 e2))

=> 3.0

;; Subtracting the upper from the lower note returns a negative number
(let ((e1 (make-event 'a3 'q))
      (e2 (make-event 'c4 'q)))
  (pitch- e1 e2))

=> -3.0

|#
;;; SYNOPSIS
(defmethod pitch- ((e1 event) (e2 event))
;;; ****
  (pitch- (pitch-or-chord e1) (pitch-or-chord e2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sun Apr 29 14:25:13 BST 2012: Added robodoc entry

;;; MDE: original comment:
;;; NB could screw up timing info in a bar

;;; SAR Fri Dec 23 20:45:16 EST 2011 Added robodoc info

;;; ****m* event/inc-duration
;;; FUNCTION

;;; Increase the duration of a given event object by a specified time in
;;; seconds. This will result in new values for the end-time,
;;; duration-in-tempo, and compound-duration-in-tempo slots. 
;;;
;;; NB: Changing this value directly could result in incorrect timing info in a
;;;     bar.
;;; 
;;; ARGUMENTS
;;; - An event object.
;;; - A value that is the increment in seconds by which the duration is to be
;;;   extended. 
;;; 
;;; RETURN VALUE
;;; The new duration in seconds.
;;; 
;;; EXAMPLE
#|
;;; Create a slippery-chicken object, assign a variable to one of the event
;;; objects it contains, print the corresponding duration slots; apply
;;; inc-duration and print the corresponding duration slots again to see the
;;; change. 
(let* ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((vc (cello :midi-channel 1))))
        :set-palette '((1 ((gs3 as3 b3))))
        :set-map '((1 (1)))
        :rthm-seq-palette '((1 ((((2 4) q (e) s (32) 32))
                                :pitch-seq-palette ((1 2 3)))))
        :rthm-seq-map '((1 ((vc (1)))))))
      (e (get-event mini 1 3 'vc)))
  (print (end-time e))
  (print (duration-in-tempo e))
  (print (compound-duration-in-tempo e))
  (inc-duration e 7.0)
  (print (end-time e))
  (print (duration-in-tempo e))
  (print (compound-duration-in-tempo e)))

=>
1.75 
0.25 
0.25 
8.75 
7.25 
7.25

|#
;;; SYNOPSIS
(defmethod inc-duration ((e event) inc)
;;; ****
  (if (and (numberp (duration-in-tempo e))
           (numberp (compound-duration-in-tempo e))
           (numberp (end-time e)))
      (progn
        (incf (duration-in-tempo e) inc)
        (incf (compound-duration-in-tempo e) inc)
        (incf (end-time e) inc))
      (error "~a~%~%event::inc-duration: can't increment non-number slots ~
              duration-in-tempo, compound-duration-in-tempo, end-time."
             e)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat Dec 24 09:03:26 EST 2011

;;; ****m* event/set-midi-time-sig
;;; FUNCTION
;;; Sets a MIDI time signature for the given event object. This must be a
;;; time-sig object, not just a time signature list.
;;; 
;;; ARGUMENTS
;;; - An event object.
;;; - A time-sig object.
;;; 
;;; RETURN VALUE
;;; Returns a time-sig object.
;;; 
;;; EXAMPLE
#|
;; Creating a new event object sets the midi-time-sig slot to NIL by default
(let ((e (make-event 'c4 'q)))
  (midi-time-sig e))

=> NIL

;; The set-midi-time-sig method returns a time-sig object
(let ((e (make-event 'c4 'q)))
  (set-midi-time-sig e (make-time-sig '(3 4))))

=> 
TIME-SIG: num: 3, denom: 4, duration: 3.0, compound: NIL, midi-clocks: 24,
num-beats: 3 
SCLIST: sclist-length: 2, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: "0304", tag: NIL, 
data: (3 4)

;; Set the midi-time-sig slot and read the data of the given time-sig object
(let ((e (make-event 'c4 'q)))
  (set-midi-time-sig e (make-time-sig '(3 4)))
  (data (midi-time-sig e)))

=> (3 4)
|#
;;; SYNOPSIS
(defmethod set-midi-time-sig ((e event) time-sig)
;;; **** 
  (setf (midi-time-sig e) (clone time-sig)))

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

;;; SAR Sat Dec 24 09:14:50 EST 2011 Added robodoc info
;;; SAR Sat Dec 31 09:22:16 EST 2011 Put date in DATE block

;;; ****m* event/add-arrow
;;; DATE
;;; 25 Jun 2011
;;;
;;; FUNCTION
;;; Adds a start-arrow mark to the given event object and stores text that is
;;; to be attached to the start and end of the given arrow for LilyPond
;;; output. This is a little more complex than the usual mark adding process,
;;; hence this separate method and it not being possible to add arrows to
;;; rthm-seq objects.  Not available for CMN.
;;;
;;; NB: A separate end-arrow mark should be attached to the note where the end
;;;     text is to appear.  Use end-arrow for this or (add-mark e 'end-arrow).
;;; 
;;; ARGUMENTS
;;; - An event object.
;;; - A start-text string.
;;; - An end-text string.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to indicate whether or not to print a warning when trying to 
;;;   attach an arrow and accompanying marks to a rest. Default = NIL. 
;;; 
;;; RETURN VALUE
;;; Returns T.
;;; 
;;; EXAMPLE
#|
;; Create an event object and see that the MARKS-BEFORE and MARKS slots are set
;;; to NIL by default
(let ((e (make-event 'c4 'q)))
  (print (marks-before e))
  (print (marks e)))

=>
NIL
NIL

;; Create an event object, apply the add-arrow method, and print the
;; corresponding slots to see the changes.
(let ((e (make-event 'c4 'q)))
  (add-arrow e "start here" "end here")
  (print (marks-before e))
  (print (marks e)))

=>
((ARROW "start here" "end here")) 
(START-ARROW) 

;; Create an event object that is a rest and apply the add-arrow method with
;; the optional argument set to T to see the warning printed.
(let ((e (make-event nil 'q)))
  (add-arrow e "start here" "end here" t))

=> T
event::add-arrow: add arrow to rest?

|#
;;; SYNOPSIS
(defmethod add-arrow ((e event) start-text end-text &optional warn-rest)
;;; ****
  (unless (and (stringp start-text) (stringp end-text))
    (error "~a~&event::add-arrow: start-text (~a) and end-text (~a) must ~
            both be strings"
           e start-text end-text))
  (when (and warn-rest (is-rest e))
    (warn "~a~&event::add-arrow: add arrow to rest?" e))
  ;; 26.7.11 (Pula): if there's not start/end text the arrow won't be shown in
  ;; lilypond :/
  (when (or (and (stringp start-text) (zerop (length start-text)))
            (and (stringp end-text) (zerop (length end-text))))
    (error "~a~%event::add-arrow: start-text/end-text can't be an empty string!"
           e))
  ;; MDE Thu Mar  1 20:50:16 2012 
  (when (or (has-mark e 'start-arrow) (has-mark e 'end-arrow))
    (error "~a~%event::add-arrow: Lilypond can't have notes with more ~
             than one arrow"
           e))
  (add-mark-before e (list 'arrow start-text end-text))
  (add-mark e 'start-arrow))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Fri Mar  2 13:40:07 GMT 2012: Added robodoc entry

;;; MDE Thu Mar  1 20:27:53 2012 

;;; ****m* event/end-arrow
;;; FUNCTION
;;; Adds an end-arrow mark to the given event object.
;;;
;;; NB: This method works for LilyPond only. When used with CMN, output will
;;;     still be generated, but no mark will be added. The method prints a
;;;     corresponding warning when applied.
;;; 
;;; ARGUMENTS
;;; - An event object.
;;; 
;;; RETURN VALUE
;;; Returns T.
;;; 
;;; EXAMPLE
#|
;; Returns T
(let ((e (make-event 'c4 'q)))
  (end-arrow e))

=> T
WARNING: 
rhythm::validate-mark: no CMN mark for END-ARROW (but adding anyway).

;; Create an event object, add end-arrow, and print the MARKS and MARKS-SLOTS
;; to see the result
(let ((e (make-event 'c4 'q)))
  (end-arrow e) 
  (print (marks-before e))
  (print (marks e)))

=> 
NIL 
(END-ARROW)

|#
;;; SYNOPSIS
(defmethod end-arrow ((e event))
;;; ****
  (add-mark e 'end-arrow))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Fri Dec 30 11:21:12 EST 2011: Added robodoc info
;;; SAR Sat Dec 31 09:22:57 EST 2011: Put date in DATE block

;;; ****m* event/add-trill
;;; DATE
;;; 24 Sep 2011 
;;;
;;; FUNCTION
;;; Used for adding pitched trills to printed score output. Adds the necessary
;;; values to the MARKS and MARKS-BEFORE slots of a given event object.
;;;
;;; NB: The main interface for adding trills by hand is
;;;     slippery-chicken::trill, which is the class-method combination that
;;;     should be accessed for this purpose.
;;;
;;; NB: This method will check to see if the specified trill marks are already
;;;     present in the MARKS and MARKS-BEFORE slots. If they are, the method
;;;     will print a warning but will add the specified trill marks anyway.
;;; 
;;; ARGUMENTS
;;; - An event object.
;;; - A pitch-symbol for the trill note.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to indicate whether or not to print a warning when attaching
;;;   trill information to a rest. Default = NIL.
;;; 
;;; RETURN VALUE
;;; Always returns T.
;;; 
;;; NB: At the moment the method will also print the reminder warning that this
;;; is a LilyPond-only mark.
;;; 
;;; EXAMPLE
#|
;; Create an event object and print the contents of the MARKS-BEFORE and MARKS
;; slots to see that they're empty by default.
(let ((e (make-event 'c4 'q)))
  (print (marks-before e))
  (print (marks e)))

=>
NIL
NIL

;; Create an event object, add a trill to the note 'D4, and print the
;; corresponding slots to see the changes
(let ((e (make-event 'c4 'q)))
  (add-trill e 'd4)
  (print (marks-before e))
  (print (marks e)))

=>
WARNING: 
rhythm::validate-mark: no CMN mark for BEG-TRILL-A (but adding anyway).

(BEG-TRILL-A) 
((TRILL-NOTE D4))

;; By default the method adds prints no warning when adding a mark to a rest
;; (though it still prints the warning that there is no CMN mark)
(let ((e (make-event nil 'q)))
  (add-trill e 'd4)
  (print (marks-before e))
  (print (marks e)))

=>
WARNING: 
rhythm::validate-mark: no CMN mark for BEG-TRILL-A (but adding anyway).

(BEG-TRILL-A) 
((TRILL-NOTE D4))

;; Set the optional argument to T to have the method print a warning when
;; attaching a mark to a rest
(let ((e (make-event nil 'q)))
  (add-trill e 'd4 t)
  (print (marks-before e))
  (print (marks e)))

=>
event::add-trill: add trill to rest?
WARNING: 
rhythm::validate-mark: no CMN mark for BEG-TRILL-A (but adding anyway).

(BEG-TRILL-A) 
((TRILL-NOTE D4))

;; Adding a trill that is already there will result in a warning being printed
;; but will add the mark anyway
(let ((e (make-event 'c4 'q)))
  (loop repeat 4 do (add-trill e 'd4))
  (print (marks-before e))
  (print (marks e)))

=>
WARNING:
   rhythm::add-mark: (TRILL-NOTE D4) already present but adding again!: 
[...]
(BEG-TRILL-A BEG-TRILL-A BEG-TRILL-A BEG-TRILL-A) 
((TRILL-NOTE D4) (TRILL-NOTE D4) (TRILL-NOTE D4) (TRILL-NOTE D4))

|#
;;; SYNOPSIS
(defmethod add-trill ((e event) trill-note &optional warn-rest)
;;; ****
  (when (and warn-rest (is-rest e))
    (warn "~a~&event::add-trill: add trill to rest?" e))
  ;; MDE Sun Dec 25 09:10:39 2011 -- just call this and let it throw an error
  ;; if we've not entered a valid pitch 
  (make-pitch trill-note)
  (add-mark-before e 'beg-trill-a)
  (add-mark e (list 'trill-note trill-note)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat Dec 24 10:50:33 EST 2011: Added robodoc info
;;; SAR Sat Dec 31 09:18:11 EST 2011: Added DATE back

;;; ****m* event/end-trill
;;; DATE
;;; 24 Sep 2011
;;; 
;;; FUNCTION
;;; Adds an 'end-trill-a mark to the MARKS slot of the given event object.
;;; 
;;; ARGUMENTS
;;; - An event object.
;;; 
;;; RETURN VALUE
;;; T
;;; 
;;; EXAMPLE
#|
;; The end-trill method returns T
(let ((e (make-event 'c4 'q)))
  (end-trill e))

=> T

;; Add an 'end-trill-a and check the MARKS slot to see that it's there 
(let ((e (make-event 'c4 'q)))
  (end-trill e)
  (marks e))

=> (END-TRILL-A)
|#
;;; SYNOPSIS
(defmethod end-trill ((e event))
;;; ****
  (add-mark e 'end-trill-a))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod add-mark-before ((e event) mark)
  ;; (print mark)
  ;; MDE Sat Dec 31 10:51:37 2011 
  (when (has-mark-before e mark)
    (warn "rhythm::add-mark-before: ~a already present but adding again!: ~a"
          mark e))
  (validate-mark mark)
  (push mark (marks-before e)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* event/has-mark-before
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
(defmethod has-mark-before ((e event) mark)
;;; ****
  (member mark (marks-before e) :test #'equal))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat Dec 24 10:59:52 EST 2011 Added robodoc info

;;; ****m* event/add-clef
;;; FUNCTION
;;; Add a clef indication to the MARKS-BEFORE slot of the given event object. 
;;;
;;; NB: This method does not check that the clef-name added is indeed a clef,
;;; nor does it check to see if other clefs have already been attached to the
;;; same event object.
;;; 
;;; ARGUMENTS
;;; - An event object.
;;; - A clef name (symbol).
;;; 
;;; OPTIONAL ARGUMENTS
;;; - (Internal "ignore" arguments only; not needed by the user).
;;; 
;;; RETURN VALUE
;;; Returns the contents (list) of the MARKS-BEFORE slot if successful.
;;;
;;; Returns NIL if the clef name is already present in the MARKS-BEFORE slot
;;; and is therefore not added.
;;; 
;;; EXAMPLE
#|
;; Successfully adding a clef returns the contents of the MARKS-BEFORE slot 
(let ((e (make-event 'c4 'q)))
  (add-clef e 'treble))

=> ((CLEF TREBLE))

;; Returns NIL if the clef name is already present
(let ((e (make-event 'c4 'q)))
  (add-clef e 'treble)
  (add-clef e 'treble))

=> NIL

;; Add a clef name to the marks-before slot and check that it's there
(let ((e (make-event 'c4 'q)))
  (add-clef e 'bass)
  (marks-before e))

=> ((CLEF BASS))

|#
;;; SYNOPSIS
(defmethod add-clef ((e event) clef &optional (delete-others t) ignore1 ignore2)
;;; ****
  (declare (ignore ignore1 ignore2))
  (when delete-others
    (delete-clefs e nil))
  ;; no '(clef treble) otherwise we'll end up with '(clef (clef treble))
  (unless (and (symbolp clef) (is-clef clef))
    (error "~a~&event::add-clef: ~a is not a clef." e clef))
  (let ((cl (list 'clef clef)))
    (unless (member cl (marks-before e) :test #'equal)
      (add-mark-before e cl))))
;;; ****

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat Dec 24 11:16:49 EST 2011 Added robodoc info

;;; ****m* event/get-clef
;;; FUNCTION
;;; Return the symbol associated with the key CLEF in the MARKS-BEFORE slot of
;;; the given event object.
;;; 
;;; ARGUMENTS
;;; - An event object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - (Internal "ignore" arguments only; not needed by the user).
;;; 
;;; RETURN VALUE
;;; Returns the given clef name as a symbol if successful.
;;;
;;; Returns NIL if there is no clef name found in the MARKS-BEFORE slot of the
;;; given event object.
;;; 
;;; EXAMPLE
#|
;; Returns NIL when no clef is found
(let ((e (make-event 'c4 'q)))
  (get-clef e))

=> NIL

;; Returns the clef name as symbol when successful.
(let ((e (make-event 'c4 'q)))
  (add-clef e 'treble)
  (get-clef e))

=> TREBLE

|#
;;; SYNOPSIS
(defmethod get-clef ((e event) &optional ignore1 ignore2 ignore3)
;;; ****
  (declare (ignore ignore1 ignore2 ignore3))
  (second (find-if #'(lambda (el) (when (listp el) 
                                    (eq 'clef (first el))))
                   (marks-before e))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat Dec 24 11:47:30 EST 2011

;;; ****m* event/delete-clefs
;;; FUNCTION
;;; Delete any clef names found in the MARKS-BEFORE slot of a given event
;;; object. 
;;; 
;;; ARGUMENTS
;;; - An event object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to indicate whether or not to print a warning when there are no
;;;   clef marks to delete.
;;; - (Other internal "ignore" arguments only; not needed by the user).
;;; 
;;; RETURN VALUE
;;; Always NIL.
;;; 
;;; EXAMPLE
#|
;; Returns NIL when no clef marks are found to delete, and prints a warning by
;; default. 
(let ((e (make-event 'c4 'q)))
  (delete-clefs e))

=> NIL
WARNING: event::delete-clefs: no clef to delete:
[...]

;; Setting the optional WARN argument to T suprresses the warning when no clefs
;; are found.
(let ((e (make-event 'c4 'q)))
  (delete-clefs e nil))

=> NIL

;; Also returns NIL when successful
(let ((e (make-event 'c4 'q)))
  (add-clef e 'treble)
  (delete-clefs e))

=> NIL

;; Create an event, add a clef, print the MARKS-BEFORE slot, delete the event,
;; print MARKS-BEFORE again to make sure it's gone
(let ((e (make-event 'c4 'q)))
  (add-clef e 'treble)
  (print (marks-before e))
  (delete-clefs e)
  (print (marks-before e)))

=>
((CLEF TREBLE)) 
NIL 

|#
;;; SYNOPSIS
(defmethod delete-clefs ((e event) &optional (warn t) ignore1 ignore2)
;;; ****
  (declare (ignore ignore1 ignore2))
  ;; 12.4.11 warn if no clef
  (if (get-clef e)
      (setf (marks-before e)
            (remove-if #'(lambda (x) (and (listp x) (eq (first x) 'clef)))
                       (marks-before e)))
      (when warn
        (warn "event::delete-clefs: no clef to delete: ~a" e))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat Dec 24 12:00:55 EST 2011 Added robodoc info

;;; ****m* event/get-amplitude
;;; FUNCTION
;;; Return the amplitude attached to a given event object.
;;;
;;; An optional argument allows the amplitude to be converted to and returned
;;; as a MIDI value.
;;; 
;;; ARGUMENTS
;;; - An event object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to indicate whether the amplitude value is to be returned as a
;;;   standard digital amplitude (a number between 0.0 and 1.0) or as a
;;;   standard MIDI velocity value (a whole number between 0 and 127). T = MIDI 
;;;   value. Default = NIL.
;;; 
;;; RETURN VALUE
;;; If the optional argument is set to NIL, returns a real number.
;;;
;;; If the optional argument is set to T, returns a whole number (and a
;;; remainder).  
;;; 
;;; EXAMPLE
#|
;; Get the amplitude as a decimal value. (Each new event object has a default
;; amplitude of 0.7). 
(let ((e (make-event 'c4 'q)))
  (get-amplitude e))

=> 0.7

;; Get the amplitude as a rounded MIDI value.
(let ((e (make-event 'c4 'q)))
  (get-amplitude e t))

=> 89, -0.10000000000000853

|#
;;; SYNOPSIS
(defmethod get-amplitude ((e event) &optional (midi nil))
;;; ****
  (if midi
      (round (* (amplitude e) 127))
      (amplitude e)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat Dec 24 12:13:59 EST 2011

;;; ****m* event/get-pitch-symbol
;;; FUNCTION
;;; Retrieve the pitch symbol (CM/CMN note-name notation) of a given event
;;; object. Returns a single symbol if the given event object consists of a
;;; single pitch; otherwise, returns a list of pitch symbols if the given event
;;; object consists of a chord.
;;; 
;;; ARGUMENTS
;;; - An event object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to indicate whether the test is to handle the event's
;;;   written or sounding pitch. T = written. Default = T.
;;; 
;;; RETURN VALUE
;;; A symbol, if the event object consists of only a single pitch, otherwise a
;;; list of pitch symbols if the event object consists of a chord.
;;; 
;;; EXAMPLE
#|
;; Get the pitch symbol of an event object with a single pitch
(let ((e (make-event 'c4 'q)))
  (get-pitch-symbol e))

=> C4

;; Getting the pitch symbol of an event object that consists of a chord returns
;; a list of pitch symbols
(let ((e (make-event '(c4 e4 g4) 'q)))
  (get-pitch-symbol e))

=> (C4 E4 G4)

|#
;;; SYNOPSIS
(defmethod get-pitch-symbol ((e event) &optional (written t))
;;; ****
  (let ((obj (if (and written (written-pitch-or-chord e))
                 (written-pitch-or-chord e)
                 (pitch-or-chord e))))
    (when obj
      (if (chord-p obj)
          (get-pitch-symbols obj)
          (id obj)))))


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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat Dec 24 12:28:31 EST 2011 Added robodoc info

;;; ****m* event/no-accidental
;;; FUNCTION
;;; Sets the SHOW-ACCIDENTAL and ACCIDENTAL-IN-PARENTHESES slots of the given
;;; event object to NIL.
;;; 
;;; ARGUMENTS
;;; - An event object.
;;; 
;;; RETURN VALUE
;;; Always returns NIL.
;;; 
;;; EXAMPLE
#|
;; The SHOW-ACCIDENTAL slot is automatically set to T on new event objects
;; that consist of a sharp or flat note.
(let ((e (make-event 'cs4 'q)))
  (show-accidental (pitch-or-chord e)))

=> T

;; The method no-accidental sets the SHOW-ACCIDENTAL slot to NIL (and the
;; ACCIDENTAL-IN-PARENTHESES if not already).
(let ((e (make-event 'cs4 'q)))
  (no-accidental e)
  (show-accidental (pitch-or-chord e)))

=> NIL  

|#
;;; SYNOPSIS
(defmethod no-accidental ((e event))
;;; ****
  (unless (is-rest e) ; MDE Fri Jan 13 19:49:57 2012 -- just checking...
    (no-accidental (pitch-or-chord e))
    (when (written-pitch-or-chord e)
      (no-accidental (written-pitch-or-chord e)))))

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

;;; SAR Sat Dec 31 12:32:24 EST 2011: Add robodoc info

;;; ****m* event/get-dynamic
;;; FUNCTION
;;; Gets the dynamic marking attached to a given event object. 
;;;
;;; NB: This method is similar to the event::get-dynamics method, but assumes
;;; that there is only one dynamic and returns that dynamic as a single symbol
;;; rather than a list. If the user suspects that multiple dynamics may have
;;; somehow have been added to the MARKS slot of the event class, use
;;; get-dynamics to obtain a list of all dynamics in that slot; otherise, this
;;; is the method that should be generally used.
;;; 
;;; ARGUMENTS
;;; - An event object.
;;; 
;;; RETURN VALUE
;;; The symbol representing the dynamic if there is one attached to that event,
;;; otherwise NIL.
;;; 
;;; EXAMPLE
#|
;; The method returns just the dynamic marking from the MARKS list, as a symbol
(let ((e (make-event 'c4 'q)))
  (add-mark-once e 'ppp)
  (add-mark-once e 'pizz)
  (get-dynamic e))

=> PPP

;; The method returns NIL if there is no dynamic in the MARKS list
(let ((e (make-event 'c4 'q)))
  (add-mark-once e 'pizz)
  (get-dynamic e))

=> NIL

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
  (defmethod get-lp-data ((e event) &optional in-c ignore1 ignore2)
    (declare (ignore ignore1 ignore2))
    (when (and (not in-c) (not (is-rest e)) (not (written-pitch-or-chord e)))
      (error "event::get-lp-data: request for non-existent ~
              transposed pitch: ~%~a" e))
    ;; 2.3.11: some marks (or rather noteheads) need to be set before in
    ;; lilypond but with the note in cmn; so move these over here 
    (multiple-value-bind 
          (from to)
        ;; 13.4.11 the start of 8va marks need to be before the note, but the
        ;; end comes after the note in order to include it under the bracket 
        (move-elements '(circled-x x-head triangle beg-8va beg-8vb
                         hairpin0 beg-trill-a triangle-up mensural <<)
                       (marks e) (marks-before e))
      (setf (marks e) from
            (marks-before e) to))
    (let* ((poc (if (and in-c (not (from-8ve-transposing-ins e)))
                    (pitch-or-chord e)
                    (written-pitch-or-chord e)))
           (note 
            (progn
              ;; MDE Mon Apr 30 20:03:35 2012 -- harmonic note heads must be a
              ;; chord in LP!!!!
              (when (has-mark e 'flag-head)
                (if (is-single-pitch e)
                    (add-mark poc 'flag-head)
                    (loop for p in (data poc) do (add-mark p 'flag-head)))
                (rm-marks e 'flag-head)
                (when (is-single-pitch e)
                  (setf poc (make-chord (list poc)))))
              (cond ((is-rest e) "r")
                    ;; we handle these with the next normal notes
                    ((is-grace-note e) 
                     (push poc grace-notes))
                    ;; it's a note or chord
                    (t (get-lp-data poc)))))
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
      (when (marks-before e)
        (loop for thing in (marks-before e) do
           ;; handle clefs here rather than in lp-get-mark
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
             for lp-mark = (lp-get-mark mark :num-flags (num-flags e))
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
  (when (start-time e)
    (cmn::sc-cmn-text 
     (secs-to-mins-secs (start-time e))
     :font-size 6)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat Dec 24 15:15:57 EST 2011 Added robodoc info

;;; ****m* event/is-chord
;;; FUNCTION
;;; Test to determine whether a given event object consists of a chord (as
;;; opposed to a single pitch or a rest). 
;;; 
;;; ARGUMENTS
;;; - An event object.
;;; 
;;; RETURN VALUE
;;; - If the given event object is a chord, the method returns a number that is
;;;   the number of notes in the chord.
;;; - Returns NIL if the given event object is not a chord.
;;; 
;;; EXAMPLE
#|
;; Returns NIL if not a chord
(let ((e (make-event 'c4 'q)))
  (is-chord e))

=> NIL

;; If a chord, returns the number of notes in the chord 
(let ((e (make-event '(c4 e4 g4) 'q)))
  (is-chord e))

=> 3

;; A rest is not a chord
(let ((e (make-rest 'q)))
  (is-chord e))

=> NIL

|#
;;; SYNOPSIS
(defmethod is-chord ((e event))
;;; ****
  (let ((noc (pitch-or-chord e)))
    (when (typep noc 'chord)
      (sclist-length noc))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat Dec 24 15:31:18 EST 2011 Added robodoc info

;;; ****m* event/is-single-pitch
;;; FUNCTION
;;; Test to see if an event object consists of a single pitch (as opposed to a
;;; chord or a rest).
;;; 
;;; ARGUMENTS
;;; - An event object.
;;; 
;;; RETURN VALUE
;;; Returns T if the given event object consists of a single pitch, otherwise
;;; returns NIL.
;;; 
;;; EXAMPLE
#|
;; Returns T if the event object consists of a single pitch
(let ((e (make-event 'c4 'q)))
  (is-single-pitch e))

=> T

;; Returns NIL if the event object is a chord
(let ((e (make-event '(c4 e4 g4) 'q)))
  (is-single-pitch e))

=> NIL

;; Also returns NIL if the event object is a rest
(let ((e (make-rest 'q)))
  (is-single-pitch e))

=> NIL

|#
;;; SYNOPSIS
(defmethod is-single-pitch ((e event))
;;; ****
  (typep (pitch-or-chord e) 'pitch))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 
;;; If functions are given, they will be used for the note or chord in the
;;; event, whereby semitones may or may not be nil in that case (transposition
;;; could be dependent on the note or chord and not a fixed shift). 

;;; SAR Sat Dec 24 15:46:23 EST 2011 Added robodoc info

;;; ****m* event/transpose
;;; FUNCTION
;;; Transpose the pitch content of a given event object by a specified number
;;; of semitones. This method can be applied to chords or single-pitches.
;;;
;;; NB: By default this method returns a modified clone of the original rather
;;; than changing the values of the original itself. The user can choose to
;;; replace the values of the original by setting the keyword argument
;;; :destructively to T.
;;; 
;;; ARGUMENTS
;;; - An event object.
;;; - A number (can be positive or negative).
;;; 
;;; OPTIONAL ARGUMENTS
;;; - keyword argument :destructively. T or NIL to indicate whether the method
;;;   is to change (replace) the pitch values of the original event object (T)
;;;   or return a new event object with the new pitches (NIL). Default = NIL.
;;; - keyword argument :chord-function
;;; - keyword argument :pitch-function
;;; 
;;; RETURN VALUE
;;; An event object.
;;; 
;;; EXAMPLE
#|
;; Transpose returns an event object
(let ((e (make-event 'c4 'q)))
  (transpose e 1))

=> 
EVENT: start-time: NIL, end-time: NIL, 
       duration-in-tempo: 0.0, 
[...]

;; By default transpose returns a modified clone, leaving the original event
;; object untouched. 
(let ((e (make-event 'c4 'q)))
  (print (data (pitch-or-chord (transpose e 1))))
  (print (data (pitch-or-chord e))))

=>
CS4 
C4 

;; When the keyword argument :destructively is set to T, the data of the
;; original event object is replaced
(let ((e (make-event 'c4 'q)))
  (transpose e 1 :destructively t)
  (data (pitch-or-chord e)))

=> CS4

;; Can transpose by 0 as well (effectively no transposition)
(let ((e (make-event 'c4 'q)))
  (transpose e 0 :destructively t)
  (data (pitch-or-chord e)))

=> C4

;; ...or by negative intervals
(let ((e (make-event 'c4 'q)))
  (transpose e -3 :destructively t)
  (data (pitch-or-chord e)))

=> A3

;; Can transpose chords too
(let ((e (make-event '(c4 e4 g4) 'q)))
  (transpose e -3 :destructively t)
  (loop for p in (data (pitch-or-chord e)) collect (data p)))

=> (A3 CS4 E4)

|#
;;; SYNOPSIS
(defmethod transpose ((e event) semitones
                      &key
                      destructively
                      ;; the default functions are the class methods for pitch
                      ;; or chord.
                      (chord-function #'transpose)
                      (pitch-function #'transpose))
;;; ****
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat Dec 24 16:51:34 EST 2011 Added robodoc info

;;; ****m* event/set-written
;;; FUNCTION
;;; Set the written pitch (as opposed to sounding; i.e., for transposing
;;; instruments) of a given event object. The sounding pitch remains unchanged
;;; as a pitch object in the PITCH-OR-CHORD slot, while the written pitch is
;;; added as a pitch object to the WRITTEN-PITCH-OR-CHORD slot.
;;; 
;;; ARGUMENTS
;;; - An event object.
;;; - A whole number indicating the number of semitones (positive or negative)
;;;   by which the sounding pitch is to be tranposed to create the written 
;;;   pitch. 
;;; 
;;; RETURN VALUE
;;; A pitch object.
;;; 
;;; EXAMPLE
#|
;; Returns a pitch object (here for example for a B-flat Trumpet or Clarinet) 
(let ((e (make-event 'c4 'q)))
  (set-written e -2))

=> 
PITCH: frequency: 233.08186975464196, midi-note: 58, midi-channel: NIL 
       pitch-bend: 0.0 
       degree: 116, data-consistent: T, white-note: B3
       nearest-chromatic: BF3
       src: 0.8908987045288086, src-ref-pitch: C4, score-note: BF3 
       qtr-sharp: NIL, qtr-flat: NIL, qtr-tone: NIL,  
       micro-tone: NIL, 
       sharp: NIL, flat: T, natural: NIL, 
       octave: 3, c5ths: 1, no-8ve: BF, no-8ve-no-acc: B
       show-accidental: T, white-degree: 27, 
       accidental: F, 
       accidental-in-parentheses: NIL, marks: NIL
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: BF3, tag: NIL, 
data: BF3

;; Create a single-pitch event object, set it's written pitch to two half-steps
;; lower, and print the corresponding data slots
(let ((e (make-event 'c4 'q)))
  (set-written e -2)
  (print (data (pitch-or-chord e)))
  (print (data (written-pitch-or-chord e))))

=>
C4
BF3

|#
;;; SYNOPSIS
(defmethod set-written ((e event) transposition)
;;; ****
  (when (pitch-or-chord e)
    (setf (written-pitch-or-chord e) 
      (transpose (clone (pitch-or-chord e)) transposition))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat Dec 24 17:15:48 EST 2011 Added robodoc info

;;; ****m* event/delete-written
;;; FUNCTION
;;; Delete the contents of the WRITTEN-PITCH-OR-CHORD slot of a given event
;;; object and reset to NIL.
;;; 
;;; ARGUMENTS
;;; - An event object.
;;; 
;;; RETURN VALUE
;;; Always returns NIL.
;;; 
;;; EXAMPLE
#|
;; Create an event object, print the contents of the written-pitch-or-chord
;; slot to see it's set to NIL, set-written to -2, print the contents of the
;; corresponding slot to see the data of the newly created pitch object, 
;; delete-written, print the contents of the written-pitch-or-chord slot to see
;; it's empty.
(let ((e (make-event 'c4 'q)))
  (print (written-pitch-or-chord e))
  (set-written e -2)
  (print (data (written-pitch-or-chord e)))
  (delete-written e)
  (print (written-pitch-or-chord e)))

=>
NIL 
BF3 
NIL

|#
;;; SYNOPSIS
(defmethod delete-written ((e event))
;;; ****
  (setf (written-pitch-or-chord e) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat Dec 24 17:32:15 EST 2011 Added robodoc info

;;; ****m* event/lowest
;;; FUNCTION
;;; Get the lowest pitch (of a chord) in a given event object. If the given
;;; event object contains a single pitch only, that pitch is returned.
;;; 
;;; ARGUMENTS
;;; - An event object.
;;; 
;;; RETURN VALUE
;;; A pitch object.
;;; 
;;; EXAMPLE
#|
;; Returns a pitch object
(let ((e (make-event 'c4 'q)))
  (lowest e))

=> 
PITCH: frequency: 261.6255569458008, midi-note: 60, midi-channel: NIL 
       pitch-bend: 0.0 
       degree: 120, data-consistent: T, white-note: C4
       nearest-chromatic: C4
       src: 1.0, src-ref-pitch: C4, score-note: C4 
       qtr-sharp: NIL, qtr-flat: NIL, qtr-tone: NIL,  
       micro-tone: NIL, 
       sharp: NIL, flat: NIL, natural: T, 
       octave: 4, c5ths: 0, no-8ve: C, no-8ve-no-acc: C
       show-accidental: T, white-degree: 28, 
       accidental: N, 
       accidental-in-parentheses: NIL, marks: NIL
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: C4, tag: NIL, 
data: C4

;; Returns the lowest note of a chord object within an event object
(let ((e (make-event '(d4 fs4 a4) 'q)))
  (data (lowest e)))

=> D4

|#
;;; SYNOPSIS
(defmethod lowest ((e event))
;;; ****
  (let ((porc (pitch-or-chord e)))
    (if (chord-p porc)
        (lowest porc)
      porc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat Dec 24 19:08:03 EST 2011 Added robodoc info

;;; ****m* event/highest
;;; FUNCTION
;;; Get the highest pitch (of a chord) in a given event object. If the given
;;; event object contains a single pitch only, that pitch is returned.
;;; 
;;; ARGUMENTS
;;; - An event object.
;;; 
;;; RETURN VALUE
;;; A pitch object.
;;; 
;;; EXAMPLE
#|
;; Returns a pitch object
(let ((e (make-event 'c4 'q)))
  (highest e))

=> 
PITCH: frequency: 261.6255569458008, midi-note: 60, midi-channel: NIL 
       pitch-bend: 0.0 
       degree: 120, data-consistent: T, white-note: C4
       nearest-chromatic: C4
       src: 1.0, src-ref-pitch: C4, score-note: C4 
       qtr-sharp: NIL, qtr-flat: NIL, qtr-tone: NIL,  
       micro-tone: NIL, 
       sharp: NIL, flat: NIL, natural: T, 
       octave: 4, c5ths: 0, no-8ve: C, no-8ve-no-acc: C
       show-accidental: T, white-degree: 28, 
       accidental: N, 
       accidental-in-parentheses: NIL, marks: NIL
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: C4, tag: NIL, 
data: C4

;; Returns the highest note of a chord object within an event object
(let ((e (make-event '(d4 fs4 a4) 'q)))
  (data (highest e)))

=> A4

|#
;;; SYNOPSIS
(defmethod highest ((e event))
;;; ****
  (let ((porc (pitch-or-chord e)))
    (if (chord-p porc)
        (highest porc)
        porc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat Dec 24 19:15:03 EST 2011 Added robodoc info

;;; ****m* event/event-distance
;;; FUNCTION
;;; Get the distance (interval) in semitones between the pitch level of one
;;; event object and a second. Negative numbers indicate direction interval
;;; directionality. An optional argument allows distances to be always printed
;;; as absloute values (positive).
;;;
;;; Event-distance can also be determined between chords, in which case the
;;; distance is measured between the highest pitch of one event object and the
;;; lowest of the other.
;;; 
;;; ARGUMENTS
;;; - A first event object.
;;; - A second event object.
;;;
;;; OPTIONAL ARGUMENTS
;;; - T or NIL for whether the the value should be returned as an absolute
;;;   value (i.e., always positive). Default = NIL. 
;;; 
;;; RETURN VALUE
;;; A number.
;;; 
;;; EXAMPLE
#|
;; The semitone distance between two single pitches in ascending direction
(let ((e1 (make-event 'c4 'q))
      (e2 (make-event 'e4 'q)))
  (event-distance e1 e2))

=> 4.0

;; The semitone distance between two single pitches in descending direction
(let ((e1 (make-event 'c4 'q))
      (e2 (make-event 'e4 'q)))
  (event-distance e2 e1))

=> -4.0

;; Set the optional argument to T to get the absolute distance (positive
;; number)
(let ((e1 (make-event 'c4 'q))
      (e2 (make-event 'e4 'q)))
  (event-distance e2 e1 t))

=> 4.0

;; The semitone distance between two chords in ascending direction
(let ((e1 (make-event '(c4 e4 g4) 'q))
      (e2 (make-event '(d4 f4 a4) 'q)))
  (event-distance e1 e2))

=> 9.0

|#
;;; SYNOPSIS
(defmethod event-distance ((e1 event) (e2 event) &optional absolute)
;;; ****
  (let* ((e1-high (highest e1))
         (e2-high (highest e2))
         (e1-low (lowest e1))
         (e2-low (lowest e2))
         (result
          ;; only high notes are considered important for the 'feel' of the
          ;; direction here
          (if (pitch> e2-high e1-high)
              ;; we're going up
              (- (midi-note-float e2-high) (midi-note-float e1-low))
              ;; we're going down
              (- (midi-note-float e2-low) (midi-note-float e1-high)))))
    (if absolute
        (abs result)
        result)))

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

;;; SAR Sun Dec 25 08:23:34 EST 2011 Added robodoc info

;;; ****m* event/force-rest
;;; FUNCTION
;;; Changes a given event object to a rest by setting both the PITCH-OR-CHORD
;;; and WRITTEN-PITCH-OR-CHORD slots to NIL and the IS-REST slot to T.
;;; 
;;; ARGUMENTS
;;; - An event object.
;;; 
;;; RETURN VALUE
;;; - An event object
;;; 
;;; EXAMPLE
#|

;; The method returns an event object.
(let ((e (make-event 'c4 'q)))
  (force-rest e))

=> 
EVENT: start-time: NIL, end-time: NIL, 
[...]

;; Create an event object, apply force-rest, then print the corresponding slots
;; to see the effectiveness of the method
(let ((e (make-event 'c4 'q)))
  (force-rest e)
  (print (pitch-or-chord e))
  (print (written-pitch-or-chord e))
  (print (is-rest e)))

=>
NIL 
NIL 
T

|#
;;; SYNOPSIS
(defmethod force-rest :after ((e event))
;;; **** 
  (setf (pitch-or-chord e) nil
        (written-pitch-or-chord e) nil
        ;; 23.7.11 (Pula) remove marks that can only be used on a note
        (marks e) (remove-if #'mark-for-note-only (marks e))
        ;; (8va e) 0
        (marks-before e) (remove-if #'mark-for-note-only
                                          (marks-before e))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 22.9.11 
(defmethod reset-8va ((e event))
  (rm-marks e '(beg-8va beg-8vb end-8va end-8vb) nil)
  (setf (8va e) 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sun Dec 25 08:50:52 EST 2011: Added robodoc info
;;; SAR Sat Dec 31 09:20:00 EST 2011: Put date in DATE block

;;; ****m* event/force-artificial-harmonic
;;; DATE 20 Aug 2011
;;; 
;;; FUNCTION
;;; Change the pitch-or-chord content of a given event object such that the
;;; existing pitch will be notated as an artificial harmonic. 
;;;
;;; The method creates pitch data for an artificial harmonic that will result
;;; in the specified pitch, rather than adding an artificial harmonic to the
;;; specified pitch. Thus, the method changes the existing pitch content by
;;; transposing the specified pitch down two octaves and adding a new pitch one
;;; perfect fourth above it (changing the given pitch object to a chord
;;; object). It then also adds the mark 'flag-head to the MARKS slot of the
;;; upper pitch for printing layout so that the upper pitch is printed as a
;;; diamond. 
;;; 
;;; ARGUMENTS
;;; - An event object.
;;; 
;;; RETURN VALUE
;;; Always returns NIL.
;;; 
;;; EXAMPLE
#|
;; The method returns NIL.
(let ((e (make-event 'c7 'q)))
  (force-artificial-harmonic e))

=> NIL

;; Create an event object, apply force-artificial-harmonic, then get the new
;; pitch material
(let ((e (make-event 'c7 'q)))
  (force-artificial-harmonic e)
  (loop for p in (data (pitch-or-chord e)) collect (data p)))

=> (C5 F5)

;; Create an event object, apply force-artificial-harmonic, then get the marks
;; attached to each note in the object to see the 'flag-head
(let ((e (make-event 'c7 'q)))
  (force-artificial-harmonic e)
  (loop for p in (data (pitch-or-chord e)) collect (marks p)))

=> (NIL (FLAG-HEAD))

|#
;;; SYNOPSIS
(defmethod force-artificial-harmonic ((e event) &optional instrument)
;;; ****
  (let* ((p1 (transpose (pitch-or-chord e) -24))
         (p2 (transpose p1 5))
         ;; MDE Mon Apr 23 09:20:44 2012 -- 
         (happy t))
    ;; MDE Mon Apr 23 09:17:24 2012 
    (unless (is-single-pitch e)
      (setf happy nil)
      (warn "~a~%event::force-artificial-harmonic: event is a chord: skipping."
            e))
    ;; MDE Mon Apr 23 09:16:34 2012
    (when instrument
      (unless (instrument-p instrument)
        (error "~a~%event::force-artificial-harmonic: argument should be an ~
                instrument object" instrument))
      (unless (and (in-range instrument p1)
                   (in-range instrument p2))
        (setf happy nil)
        (warn "~a~%event::force-artificial-harmonic: creating an artificial ~
               harmonic for this ~%event would go out of the instrument's ~
               range." e)))
    (when happy
      (add-mark p2 'flag-head)
      (setf (pitch-or-chord e) (make-chord (list p1 p2))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod (setf 8va) :before (value (e event))
  (unless (or (= value 0) (= value -1) (= value 1))
    (error "event::(setf 8va): 8va slot can only be -1, 0, or 1")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod delete-marks :after ((e event))
  ;; (print 'dme)
  (unless (is-rest e)
    (delete-marks (pitch-or-chord e))
    (when (written-pitch-or-chord e)
      (delete-marks (written-pitch-or-chord e)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Tue Apr 17 12:52:22 2012.  Works for chords and single pitches.  Always
;;; returns a list. 

(defmethod src-for-sample-freq (freq (e event))
  (unless (numberp freq)
    (setf freq (note-to-freq freq)))
  (let ((pitches (if (is-chord e)
                     (data (pitch-or-chord e))
                     (list (pitch-or-chord e)))))
    (loop for p in pitches collect
         (/ (frequency p) freq))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Thu Apr 19 11:59:19 2012 

;;; ****m* event/get-degree
;;; FUNCTION
;;; Get the degree of the event. 
;;; 
;;; ARGUMENTS
;;; - an event object
;;; 
;;; OPTIONAL ARGUMENTS
;;; - keyword written default NIL: whether to use the written (in the case of
;;;   transposing instruments) or sounding pitches.
;;; - keyword sum default NIL: return the sum of the degrees instead of a list
;;;   (see below).
;;; 
;;; RETURN VALUE 
;;; By default this returns a list (even if it's a single pitch), unless :sum T
;;; whereupon it will return a single value: the sum of the degrees if a chord,
;;; otherwise just the degree.  A rest would return '(0) or 0.
;;; 
;;; EXAMPLE
#|
;;; NB This uses the quarter-tone scale so degrees are double what they would
;;; be in the chromatic-scale.
(let ((event (make-event '(cs4 d4) 'e))
      (rest (make-rest 'e)))
  (print (get-degree event))
  (print (get-degree rest))
  (get-degree event :sum t))
(122 124) 
(0)
246
|#
;;; SYNOPSIS
(defmethod get-degree ((e event) &key written sum)
;;; ****      
  (let* ((poc (if written
                  (written-pitch-or-chord e)
                  (pitch-or-chord e)))
         (list (cond
                 ((is-rest e) '(0))
                 ((is-chord e)
                  (loop for p in (data poc) collect (degree p)))
                 (t (list (degree poc))))))
    (if sum
        (loop for d in list sum d)
        list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Wed May  2 14:01:53 BST 2012: Added robodoc entry

;;; ****m* event/replace-mark
;;; FUNCTION
;;; Replace a specified mark of a given event object with a second specified
;;; mark. If an event object contains more than one mark, individual marks can
;;; be changed without modifying the remaining marks.
;;; 
;;; ARGUMENTS 
;;; - An event object.
;;; - The mark to be replaced.
;;; - The new mark.
;;;
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to indicate whether the mark to be replaced is in the
;;;   MARKS-BEFORE slot. T = it is in the MARKS-BEFORE slot. Default = NIL. 
;;;
;;; RETURN VALUE  
;;; Returns the new value of the MARKS/MARKS-BEFORE slot of the given object. 
;;; 
;;; EXAMPLE
#|
;; Add marks to the MARKS slot and replace 'a with 'batt
(let ((e (make-event 'c4 'q)))
  (loop for m in '(a s pizz) 
     do (add-mark e m))
  (replace-mark e 'a 'batt))

=> (PIZZ S BATT)

;; Add marks to the MARKS-BEFORE slot and replace 'arco with 'at
(let ((e (make-event 'c4 'q)))
  (loop for m in '(arco col-legno) 
     do (add-mark-before e m))
  (replace-mark e 'arco 'at t))

=> (COL-LEGNO AT)

|#
(defmethod replace-mark ((e event) what with &optional before)
;;; ****
  (let ((new (substitute with what (if before
                                       (marks-before e)
                                       (marks e)))))
    (if before 
        (setf (marks-before e) new)
        (setf (marks e) new))))


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

;;; SAR Thu Dec 22 17:53:00 EST 2011: Minor layout edits to MDE info 

;;; ****f* event/make-event
;;; FUNCTION
;;; Create an event object for holding rhythm, pitch, and timing data.
;;; 
;;; ARGUMENTS 
;;; - A pitch or chord. This can be one of those objects (will be added to the
;;;   pitch-or-chord slot without cloning), or a pitch symbol or list of pitch 
;;;   symbols (for a chord).
;;; - The event's rhythm (e.g. 'e). If this is a number, its interpretation is
;;;   dependent on the value of duration (see below). NB if this is a rhythm 
;;;   object, it will be cloned.  
;;;
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:

;;; - :start-time. The start time of the event in seconds. Default = NIL.

;;; - :is-rest. Set to T or NIL to indicate whether or not the given event is a
;;;   rest. Default = NIL. NB: The make-rest method is better suited to making
;;;   rests; however, if using make-event to do so, the pitch-or-chord slot
;;;   must be set to NIL.

;;; - :is-tied-to. This argument is for score output and playing purposes. Set
;;;   to T or NIL to indicate whether this event is tied to the previous event
;;;   (i.e. it won't sound indpendently). Default = NIL.

;;; - :duration. T or NIL to indicate whether the specified duration of the
;;;   event has been stated in absolute seconds, not a known rhythm like
;;;   'e. Thus (make-event 'c4 4 :duration nil) indicates a quarter note with
;;;   duration 1, but (make-event '(c4 d4) 4 :duration t) indicates a whole
;;;   note with an absolute duration of 4 seconds (both assuming a tempo of
;;;   60). Default = NIL.

;;; - :amplitude sets the amplitude of the event. Possible values span from 0.0
;;;   (silent) to maximum of 1.0. Default = 0.7.

;;; - :tempo. A number to indicate the tempo of the event as a normal bpm
;;;   value. Default = 60. This argument is only used when creating the rhythm
;;;   slots (e.g. duration).

;;; - :midi-channel. A number from 0 to 127 indicating the MIDI channel on
;;;   which the event should be played back. Default = NIL.

;;; - :microtones-midi-channel. If the event is microtonal, this argument
;;;   indicates the MIDI-channel to be used for the playback of the microtonal
;;;   notes. Default = NIL.
;;; 
;;; RETURN VALUE  
;;; - An event object.
;;; 
;;; EXAMPLE
#|
;; A quarter-note (crotchet) C
(make-event 'c4 4)

=> 
EVENT: start-time: NIL, end-time: NIL, 
       duration-in-tempo: 0.0, 
       compound-duration-in-tempo: 0.0, 
       amplitude: 0.7,
       bar-num: -1, marks-before: NIL, 
       tempo-change: NIL 
       instrument-change: NIL 
       display-tempo: NIL, start-time-qtrs: -1, 
       midi-time-sig: NIL, midi-program-changes: NIL, 
       8va: 0
       pitch-or-chord: 
PITCH: frequency: 261.6255569458008, midi-note: 60, midi-channel: NIL 
       pitch-bend: 0.0 
       degree: 120, data-consistent: T, white-note: C4
       nearest-chromatic: C4
       src: 1.0, src-ref-pitch: C4, score-note: C4 
       qtr-sharp: NIL, qtr-flat: NIL, qtr-tone: NIL,  
       micro-tone: NIL, 
       sharp: NIL, flat: NIL, natural: T, 
       octave: 4, c5ths: 0, no-8ve: C, no-8ve-no-acc: C
       show-accidental: T, white-degree: 28, 
       accidental: N, 
       accidental-in-parentheses: NIL, marks: NIL
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: C4, tag: NIL, 
data: C4
       written-pitch-or-chord: NIL
RHYTHM: value: 4.0, duration: 1.0, rq: 1, is-rest: NIL, score-rthm: 4.0f0, 
        undotted-value: 4, num-flags: 0, num-dots: 0, is-tied-to: NIL, 
        is-tied-from: NIL, compound-duration: 1.0, is-grace-note: NIL, 
        needs-new-note: T, beam: NIL, bracket: NIL, rqq-note: NIL, 
        rqq-info: NIL, marks: NIL, marks-in-part: NIL, letter-value: 4, 
        tuplet-scaler: 1, grace-note-duration: 0.05
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: 4, tag: NIL, 
data: 4

;; Create a whole-note (semi-breve) chord, then print its data, value, duration
;; and pitch content
(let ((e (make-event '(c4 e4 g4) 4 :duration t)))
  (print (data e))
  (print (value e))
  (print (duration e))
  (print (loop for p in (data (pitch-or-chord e)) collect (data p))))

=>
W 
1.0f0 
4.0 
(C4 E4 G4) 

;; Create a single-pitch quarter-note event which is tied to, plays back on
;; MIDI channel 1 and has an amplitude of 0.5, then print these values by
;; accessing the corresponding slots.
(let ((e (make-event 'c4 4 
                     :is-tied-to t 
                     :midi-channel 1 
                     :amplitude 0.5)))
  (print (is-tied-to e))
  (print (midi-channel (pitch-or-chord e)))
  (print (amplitude e)))

=>
T 
1 
0.5

;; Create an event object that consists of a quarter-note rest and print the
;; contents of the corresponding slots
(let ((e (make-event nil 'q :is-rest t)))
  (print (pitch-or-chord e))
  (print (data e))
  (print (is-rest e)))

=>
NIL 
Q 
T

|#

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
  ;; MDE Mon Apr 23 13:52:07 2012 -- allow r to indicate a rest
  (when (equalp 'r pitch-or-chord)
    (setf pitch-or-chord nil))
  ;; MDE Wed Dec 14 17:32:18 2011 
  (when (and pitch-or-chord is-rest)
    (error "event::make-event: an rest can't have pitch data (~a):"
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

;;; Thu Dec 22 20:53:16 EST 2011 SAR: Added robodoc info

;;; ****f* event/make-rest
;;; FUNCTION
;;; Create an event object that consists of a rest.
;;; 
;;; ARGUMENTS
;;; - A rhythm (duration).
;;; 
;;; OPTIONAL ARGUMENTS
;;; - keyword argument :start-time. A number that is the start-time of the
;;;   event in seconds.
;;; - keyword argument :duration. T or NIL. T indicates that the duration given
;;;   is a value of absolute seconds rather than a known rhythm
;;;   (e.g. 'e). Default = NIL. 
;;; - keyword duration :tempo. Beats per minute. Default = 60.
;;; 
;;; RETURN VALUE
;;; - An event object.
;;; 
;;; EXAMPLE
#|
;; Make an event object consisting of a quarter rest
(make-rest 4)

=> 
EVENT: start-time: NIL, end-time: NIL, 
       duration-in-tempo: 0.0, 
       compound-duration-in-tempo: 0.0, 
       amplitude: 0.7, 
       bar-num: -1, marks-before: NIL, 
       tempo-change: NIL 
       instrument-change: NIL 
       display-tempo: NIL, start-time-qtrs: -1, 
       midi-time-sig: NIL, midi-program-changes: NIL, 
       8va: 0
       pitch-or-chord: NIL
       written-pitch-or-chord: NIL
RHYTHM: value: 4.0, duration: 1.0, rq: 1, is-rest: T, score-rthm: 4.0f0, 
        undotted-value: 4, num-flags: 0, num-dots: 0, is-tied-to: NIL, 
        is-tied-from: NIL, compound-duration: 1.0, is-grace-note: NIL, 
        needs-new-note: NIL, beam: NIL, bracket: NIL, rqq-note: NIL, 
        rqq-info: NIL, marks: NIL, marks-in-part: NIL, letter-value: 4, 
        tuplet-scaler: 1, grace-note-duration: 0.05
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: 4, tag: NIL, 
data: 4

;; Make an event object consisting of 4 seconds of rest (rather than a quarter;
;; indicated by the :duration t) starting at time-point 13.7 seconds, then
;; print the corresponding slot values.
(let ((e (make-rest 4 :start-time 13.7 :duration t)))
  (print (is-rest e))
  (print (data e))
  (print (duration e))
  (print (value e))
  (print (start-time e)))

=>
T 
W 
4.0 
1.0f0 
13.7

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
;;; Thu Dec 22 20:53:16 EST 2011 SAR: Added robodoc info

;;; ****f* event/make-punctuation-events
;;; FUNCTION
;;; Given a list of numbers, a rhythm, and a note name or list of note names,
;;; create a new list of single events separated by rests. 
;;;
;;; The rhythm specified serves as the basis for the new list. The numbers
;;; specified represent groupings in the new list that are each made up of one 
;;; rhythm followed by rests. Each consecutive grouping in the new list has the
;;; length of each consecutive number in the numbers list multiplied by the
;;; rhythm specified. 
;;;
;;; Notes can be a single note or a list of notes. If the latter, they'll be
;;; used one after the other, repeating the final note once reached.
;;; 
;;; ARGUMENTS
;;; - A list of grouping lengths.
;;; - A rhythm.
;;; - A note name or list of note names.
;;; 
;;; RETURN VALUE
;;; A list.
;;; 
;;; EXAMPLE
#|
;; Create a list of three groups that are 2, 3, and 5 16th-notes long, with the
;; first note of each grouping being a C4, then print-simple it's contents. 
(let ((pe (make-punctuation-events '(2 3 5) 's 'c4)))
  (loop for e in pe do (print-simple e)))

=>
C4 S, rest S, C4 S, rest S, rest S, C4 S, rest S, rest S, rest S, rest S,

;; Create a list of "punctuated" events using a list of note names. Once the
;; final note name is reached, it is repeated for all remaining non-rest
;; rhythms.  
(let ((pe (make-punctuation-events '(2 3 5 8) 'q '(c4 e4))))
  (loop for e in pe do (print-simple e)))

=>
C4 Q, rest Q, E4 Q, rest Q, rest Q, E4 Q, rest Q, rest Q, rest Q, rest Q, E4 Q,
rest Q, rest Q, rest Q, rest Q, rest Q, rest Q, rest Q, 

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

;;; SAR Fri Dec 23 13:41:36 EST 2011: Added robodoc info

;;; ****f* event/make-events
;;; FUNCTION
;;; Make a list of events using the specified data, whereby a list indicates a
;;; note (or chord) and its rhythm and a single datum is the rhythm of a rest.
;;; 
;;; ARGUMENTS
;;; - A list.
;;;  
;;; OPTIONAL ARGUMENTS
;;; - A whole number indicating the MIDI channel on which the event is to be
;;;   played. 
;;; - A whole number indicating the MIDI channel on which microtonal pitches of
;;;   the event are to be played.
;;; 
;;; RETURN VALUE
;;; A list.
;;; 
;;; EXAMPLE
#|
;; Create a list of events including a quarter note, two rests, and a chord,
;; then print-simple its contents
(let ((e (make-events '((g4 q) e s ((d4 fs4 a4) s)))))
  (loop for i in e do (print-simple i)))

=>
G4 Q, rest E, rest S, (D4 FS4 A4) S,

;; Create a list of events to be played on MIDI-channel 3, then check the MIDI
;; channels of each sounding note
(let ((e (make-events '((g4 q) e s (a4 s) q e (b4 s)) 3)))
  (loop for i in e
     when (not (is-rest i))
     collect (midi-channel (pitch-or-chord i))))

=> (3 3 3)

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

;;; SAR Fri Dec 23 13:41:36 EST 2011: Added robodoc info

;;; ****f* event/make-events2
;;; FUNCTION
;;; Like make-events, but rhythms and pitches are given in separate lists to
;;; allow for rhythms with ties using "+" etc. "Nil" or "r" given in the pitch
;;; list indicates a rest; otherwise, a single note name will set a single
;;; pitch while a list of note names will set a chord. Pitches for tied notes
;;; only have to be given once.
;;; 
;;; ARGUMENTS
;;; - A list of rhythms.
;;; - A list of note names (including NIL or R for rests).
;;; 
;;; OPTIONAL ARGUMENTS
;;; - A whole number value to indicate the MIDI channel on which to play back
;;;   the event.
;;; - A whole number value to indicate the MIDI channel on which to play back
;;;   microtonal pitch material for the event.
;;; 
;;; RETURN VALUE
;;; A list.
;;; 
;;; EXAMPLE
#|
;; Create a make-events2 list and use the print-simple function to retrieve its
;; contents. 
(let ((e (make-events2 '(q e e. h+s 32 q+te) '(cs4 d4 (e4 g4 b5) nil a3 r))))
  (loop for i in e do (print-simple i)))

=>
CS4 Q, D4 E, (E4 G4 B5) E., rest H, rest S, A3 32, rest Q, rest TE,

;; Create a list of events using make-events2, indicating they be played back
;; on MIDI-channel 3, then print the corresponding slots to check it
(let ((e (make-events2 '(q e. h+s 32 q+te) '(cs4 b5 nil a3 r) 3)))
  (loop for i in e
     when (not (is-rest i))
     collect (midi-channel (pitch-or-chord i))))

=>
(3 3 3)

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

;;; 23.12.11 SAR Added robodoc info

;;; ****f* event/event-p
;;; FUNCTION
;;; Test to confirm that a given object is an event object.
;;; 
;;; ARGUMENTS
;;; - An object.
;;; 
;;; RETURN VALUE
;;; T if the tested object is indeed an event object, otherwise NIL.
;;; 
;;; EXAMPLE 
#|
;; Create an event and then test whether it is an event object
(let ((e (make-event 'c4 'q)))
  (event-p e))

=> T

;; Create a non-event object and test whether it is an event object
(let ((e (make-rhythm 4)))
  (event-p e))

=> NIL

;; The make-rest function also creates an event
(let ((e (make-rest 4)))
  (event-p e))

=> T

;; The make-punctuation-events, make-events and make-events2 functions create
;; lists of events, not events themselves.
(let ((e (make-events '((g4 q) e s))))
  (event-p e))

=> NIL

|#
;;; SYNOPSIS
(defun event-p (thing)
;;; ****
  (typep thing 'event))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat Apr 28 22:47:00 BST 2012: Added robodoc entry

;;; ****f* event/sort-event-list
;;; FUNCTION
;;; Sort a list of event objects by start time from lowest to highest.
;;; 
;;; ARGUMENTS
;;; - A list of event objects.
;;; 
;;; RETURN VALUE
;;; A list of event objects.
;;; 
;;; EXAMPLE
#|
;; Create a list of event object with non-sequential start-times, sort them,
;; and return the pitches and start times of the newly ordered list.
(let ((e-list (loop repeat 8
                 for nn in '(c4 d4 e4 f4 g4 a4 b4 c5)
                 for st in '(1.0 3.0 2.0 5.0 8.0 4.0 7.0 6.0)
                 collect (make-event nn 'e :start-time st))))
  (sort-event-list e-list)
  (loop for e in e-list 
     collect (get-pitch-symbol e)
     collect (start-time e)))

=> (C4 1.0 E4 2.0 D4 3.0 A4 4.0 F4 5.0 C5 6.0 B4 7.0 G4 8.0)

|#
;;; SYNOPSIS
(defun sort-event-list (event-list)
;;; ****
  (sort event-list #'(lambda (x y) (< (start-time x) (start-time y)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 23.12.11 SAR Added robodoc info

;;; ****f* event/wrap-events-list
;;; FUNCTION

;;; Given a list of time-ascending event objects, rotate their start-times by
;;; moving the lowest start time to a specified point in the list (determined
;;; either by time or by nth position), assigning the subsequent start times
;;; sequentially through the remainder of events in the list, and wrapping
;;; around to the head of the list again to assign the final remaining start
;;; times. If the first event doesn't start at 0, its start time will be
;;; conserved.
;;; 
;;; ARGUMENTS 
;;; - A flat list of event objects.
;;; - An integer that is the number of the event object with which to start
;;;   (nth position), or a decimal time in seconds.
;;;
;;; OPTIONAL ARGUMENTS
;;; keyword argument:
;;; - :time. T or NIL to indicate whether the second argument is a time in
;;;   seconds or an nth index. If a time in seconds, the method skips to the
;;;   closest event object in the list. T = time in seconds. Default = NIL.
;;; 
;;; RETURN VALUE  
;;; Returns a flat list of event objects with adjust start-times.
;;;
;;; EXAMPLE
#|

|#
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 23.12.11 SAR Added Robodoc info

;;; ****f* event/is-dynamic
;;; FUNCTION
;;; Determine whether a specified symbol belongs to the list of predefined
;;; dynamic marks.
;;; 
;;; ARGUMENTS
;;; - A symbol.
;;; 
;;; RETURN VALUE
;;; NIL if the specified mark is not found on the predifined list of possible
;;; dynamic marks, otherwise the tail of the list of possible dynamics starting
;;; with the given dynamic.
;;; 
;;; EXAMPLE
#|
(is-dynamic 'pizz)

=> NIL

(is-dynamic 'f)

=> (F FF FFF FFFF)

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
;;; MDE Sun Dec 25 08:41:20 2011 
(defun is-clef (mark)
  (let ((clef (typecase mark
                (list (when (and (equalp (first mark) 'clef)
                                 (= 2 (length mark)))
                        (second mark)))
                (symbol mark)
                (t nil))))
    (when (and clef
               (member clef '(treble bass alto tenor double-treble double-bass
                              percussion soprano baritone)))
      t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; EOF event.lsp
