;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* rhythm/event
;;; NAME 
;;; event
;;;
;;; File:             event.lsp
;;;
;;; Class Hierarchy:  named-object -> linked-named-object -> rhythm -> event
;;;
;;; Version:          1.0.11
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
;;; $$ Last modified:  16:17:20 Thu Dec 10 2020 CET
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :slippery-chicken)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
   ;; plays microtonal chords (i.e. plays on two midi channels). See also
   ;; add-midi-program-changes method 
   (midi-program-changes :accessor midi-program-changes :type list 
                         :initarg :midi-program-changes :initform nil)
   ;; MDE Tue Apr 26 15:13:54 2016 -- each message should be a 3-element list
   ;; of the form (channel controller-number value). The user can (push ) these
   ;; directly for now. So if you wanted a crescendo on one note, you'd push an
   ;; aftertouch or breath control change here (with a corresponding value on
   ;; the note that terminutes the cresc.) and hope/expect the host application
   ;; to do the interpolation for you.
   ;; todo: as with prog changes write control out to antescofo files too.
   (midi-control-changes :accessor midi-control-changes :type list
                         :initarg :midi-control-changes :initform nil)
   ;; 16.3.11 30,000ft over Turkmenistan :) instead of writing an instrument
   ;; change as cmn text, indicate it here as plain strings--1st,
   ;; long name for the instrument; 2nd short name; 3rd number of staff lines
   ;; the new instrument uses (usually 5)
   ;; MDE Sat Mar 18 13:12:06 2017 -- added 4th element: the instrument object 
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
   ;; MDE Tue May 6 20:38:47 2014 -- an antescofo~ label, suitable for jumping
   ;; to during reharsal. This appears at the end of a NOTE line. Should
   ;; generally be a string but numbers seem to work fine too.
   (asco-label :accessor asco-label :initarg :asco-label :initform nil)
   ;; a list of messages we want antescofo~ to send. This basically anything we
   ;; want on a separate line, after the event details. It'll need to be a
   ;; receiver name and a value, as a string e.g. '("granular on"). A delay in
   ;; beats could be added first e.g.  "0.5 granular on". NB you can push as
   ;; many messages as you want into this list; they'll be reversed before
   ;; writing out so that they occur in the order in which you added them. For
   ;; the part we're following, we can add messages to rests but this won't be
   ;; written to the antescofo file if it turns out we added messages to rests
   ;; in other players' (i.e. group event parts). 
   (asco-msgs :accessor asco-msgs :initarg :asco-msgs :type list :initform nil)
   ;; MDE Thu Aug 22 19:26:11 2013 -- the player whose part this event is in
   (player :accessor player :initform nil)
   ;; MDE Sat Apr 23 14:04:45 2016 -- which set (as a reference to the
   ;; slippery-chicken object's set-palette) the pitch data came from 
   (set-ref :accessor set-ref :initform nil)
   ;; MDE Tue Mar 28 14:27:02 2017 -- keep track of the last midi channel seen
   ;; for an event of any/every player. that way we can always know or guess
   ;; the midi channel of even a rest, e.g. when changing instrument and
   ;; writing midi info for music xml
   (midi-channels :accessor midi-channels :type assoc-list :allocation :class
                  :initform (make-assoc-list 'event-midi-channels nil))
   ;;(rqq-notes :accessor rqq-notes :type list :initform nil :allocation :class)
   ;; For MIDI output this would have to be a floating-point number 0.0-1.0, or
   ;; an integer velocity 0-127  
   (amplitude :accessor amplitude :type number :initarg :amplitude 
              :initform (get-sc-config 'default-amplitude))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod scale :around ((e event) scaler 
                          &optional
                          (clone t) 
                          (scale-start-time nil)
                          (time-offset 0.0))
  (declare (ignore clone))
  ;; (print scaler)
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
          (slot-value rthm 'midi-control-changes) (my-copy-list
                                                   (midi-control-changes e))
          (slot-value rthm 'amplitude) (amplitude e)
          (slot-value rthm 'bar-num) (bar-num e)
          (slot-value rthm 'midi-time-sig) (midi-time-sig e)
          (slot-value rthm 'start-time-qtrs) (start-time-qtrs e)
          ;; MDE Thu Aug 22 19:27:43 2013 
          (slot-value rthm 'player) (player e)
          ;; MDE Tue May  6 20:42:15 2014 
          (slot-value rthm 'asco-label) (asco-label e)
          (slot-value rthm 'asco-msgs) (asco-msgs e)
          (slot-value rthm 'instrument-change) (copy-list
                                                (instrument-change e))
          (slot-value rthm 'tempo-change) (when (tempo-change e)
                                            (clone (tempo-change e))))
    rthm))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize-instance :after ((e event) &rest initargs)
  (declare (ignore initargs))
  ;; (print 'event)
  (when (pitch-or-chord e)
    (setf (pitch-or-chord e) (pitch-or-chord e))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* event/set-midi-channel
;;; DESCRIPTION
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
(defmethod set-midi-channel ((e event) midi-channel
                             &optional microtonal-midi-channel)
;;; ****
  ;; MDE Tue Jun 28 16:04:01 2016 -- made microtonal-midi-channel optional and
  ;; if NIL defaulting to midi-channel (see pitch class method)
  (let ((noc (pitch-or-chord e)))
    (when noc
      #|
      (if (is-chord e)
          (set-midi-channel noc midi-channel microtonal-midi-channel)
          (setf (midi-channel noc) midi-channel)))))
      |#
      ;; MDE Sat May 19 20:42:51 2012
      (set-midi-channel noc midi-channel microtonal-midi-channel))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* event/get-midi-channel
;;; DESCRIPTION
;;; Retrieve the value set for the midi-channel slot of the pitch object within
;;; a given event object. Beware that if the pitch-or-chord slots is a chord, we
;;; will merely return the channel of the first pitch in the chord.  
;;; 
;;; ARGUMENTS
;;; - An event object.
;;; 
;;; RETURN VALUE
;;; An integer representing the given midi-channel value or NIL if there isn't
;;; one.
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE original comment:
;;; this should work even in rests, i.e. time sigs, tempo changes, and program
;;; changes will all be written despite no new pitches.
#+cm-2
;;; ****m* event/output-midi
;;; DESCRIPTION
;;; Generate the data necessary for MIDI output for a given event object. Note
;;; that for events that contain chords, the event's amplitude slot will be used
;;; for all pitches unless each individual pitch object in the chord has its
;;; amplitude slot set to a number.
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
;;; - A decimal number or function that is to override any other existing event
;;;   object data for amplitude.
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
    (error "event::output-midi: start-time nil! Call update-slots perhaps:~%~a"
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
        (ccs (midi-control-changes e))
        (ts (midi-time-sig e))
        omn)
    (flet ((store-it (it)
             (if (atom it) ;; it's a single cm midi event generated by a pitch
                 (push it result)
                 ;; it's a list of cm midi events generated by a chord
                 (loop for n in it do (when n (push n result))))))
      ;; MDE Tue Apr 26 15:17:35 2016 -- control changes
      (when ccs
        (loop 
           for cc in ccs
           for channel = (first cc)
           for controller = (second cc)
           for value = (third cc)
           do
             (store-it (cm::midi-control-change time channel controller
                                                value))))
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
        ;; MDE Sat Dec  7 11:58:42 2019 -- if certain marks mean a note
        ;; shouldn't be played, we ignore it 
        (setq omn (output-midi-note noc
                                    time
                                    (if force-velocity
                                        ;; MDE Thu Sep 17 17:31:18 2015 --
                                        ;; allow functions too 
                                        (if (functionp force-velocity)
                                            (funcall force-velocity e)
                                            force-velocity)
                                        (amplitude e))
                                    ;; rhythm's compound-duration is in 
                                    ;; quarters
                                    (compound-duration e)))
        (when omn (store-it omn)))
      result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* event/get-dynamics
;;; DESCRIPTION
;;; Get the list of dynamic marks from a given event object, assuming there are
;;; multiple dynamics present. If other non-dynamic events are also contained
;;; in the MARKS slot of the rhythm object within the given event object, these
;;; are disregarded and only the dynamic marks are returned.  
;;;
;;; NB: This method is similar to the event::get-dynamic, but is intended for
;;;     use should multiple dynamics have somehow become attached to the same
;;;     event. The method event::get-dynamic is the method that should
;;;     generally be used.
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
(defmethod get-dynamics ((e event) &optional hairpins)
;;; ****
  (remove-if-not #'(lambda (x)
                     ;; (not (is-dynamic x)))
                     (or (and hairpins (or (is-hairpin x)
                                           (eq x 'hairpin0)))
                         (is-dynamic x)))
                 ;; NB remove-if and remove-if-not are not destructive.
             (marks e)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* event/remove-dynamics
;;; DESCRIPTION
;;; Remove all dynamic symbols from the list of marks attached to a given event
;;; object. 
;;; 
;;; NB: This doesn't change the amplitude.
;;; 
;;; ARGUMENTS
;;; - An event object.
;;;
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to remove hairpin marks too (e.g. cresc-beg). Default = NIL.
;;; 
;;; RETURN VALUE
;;; Returns the modified list of marks (which could be empty) attached to the
;;; given event object if the specified dynamic was initially present in that
;;; list and successfully removed, otherwise returns NIL.
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
(defmethod remove-dynamics ((e event) &optional hairpins)
;;; ****
  (setf (marks e) 
        (remove-if #'(lambda (x) (or (and hairpins (or (is-hairpin x)
                                                       (eq x 'hairpin0)))
                                     (is-dynamic x)))
                   (marks e))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 5.4.11: remove existing dynamics if we're about to add one
(defmethod add-mark :before ((e event) mark &optional warn-rest warn-again)
  (declare (ignore warn-rest warn-again))
  (when (is-dynamic mark)
    (remove-dynamics e)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Thu Jul 11 19:19:32 2019 -- this used to be in add-mark :after but we
;;; need it in sc-make-sequenz bzw. sequenz::pedals-to-controllers too
(defmethod pedals-to-controllers ((e event) &optional (update-amplitude t))
  ;; MDE Tue Apr 26 15:29:55 2016 -- (channel controller-number value) with
  ;; piano pedalling we don't have to worry about the microtones-midi-channel as
  ;; there are no microtones on the piano. so we just output on the channel of
  ;; the first pitch in the case of a chord
  ;; MDE Wed May 25 12:38:52 2016 -- allow soft and sost pedals also
  (flet ((pedal (val &optional (controller 64))
           (let ((channel (get-midi-channel e)))
             (unless channel
               (setq channel (get-last-midi-channel e)))
             (unless channel
               (error "add-mark: can't add pedal to an event with no midi ~
                       channel. ~%Did you add pedal marks to a rest by ~
                       mistake?: ~a" e))
             (push (list channel controller val)
                   (midi-control-changes e)))))
    (loop for mark in (marks e) do
         (case mark
           (ped (pedal 127))
           (ped-up (pedal 0))
           ;; up then down but we're pushing so reversed
           ;; (ped^ (pedal 127) (pedal 0))
           ;; MDE Sat Sep 28 16:36:05 2019 -- changed the order as no longer
           ;; working when reversed 
           (ped^ (pedal 0) (pedal 127))
           (uc (pedal 127 67))
           (tc (pedal 0 67))
           ;; MDE Thu Jun 20 17:24:08 2019 -- sost doesn't react with Disklavier
           ;; Enspire 3.10.00 
           (sost (pedal 127 66))
           (sost-up (pedal 0 66))
           (t (when (and update-amplitude (is-dynamic mark))
                (setf (slot-value e 'amplitude)
                      (dynamic-to-amplitude mark))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* event/add-mark
;;; DATE
;;; March 15th 2011
;;; 
;;; DESCRIPTION
;;; An :after method which in addition to the rhythm class's functionality will:
;;; a) update the event's amplitude slot if we set a dynamic as a mark (note
;;; that the :before method will similarly remove dynamics so we don't get
;;; multiple dynamics attached to a single pitch);
;;; b) as of 26/4/16 attach midi controller messages for pedal up/down if you
;;; attach the marks ped, ped-up, or ped^, also una corda / tre corde
;;; 
;;; ARGUMENTS
;;; - the event object
;;; - the mark (symbol)
;;; 
;;; OPTIONAL ARGUMENTS
;;; T or NIL to indicate whether amplitudes should be set if dynamic marks are
;;; set. Default = T.
;;; 
;;; RETURN VALUE
;;; 
;;; SYNOPSIS
(defmethod add-mark :after ((e event) mark
                            &optional (update-amplitude t) warn-again)
;;; ****
  (declare (ignore warn-again))
  (pedals-to-controllers e update-amplitude)
  ;; (print mark)
  e)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Fri Dec 23 16:54:59 EST 2011 Added robodoc info

;;; ****m* event/setf amplitude
;;; DESCRIPTION
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
    (add-mark e (amplitude-to-dynamic value nil) nil))
  value)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Thu May  8 11:02:36 2014 
(defmethod (setf asco-msgs) :after (value (e event))
  (when (and (is-rest e) (get-sc-config 'asco-msg-rest-warning))
    (warn "~aevent::(setf asco-msgs): Adding antescofo messages to a rest. ~
           ~%If this event is not part of the voice you want to follow, then ~
           ~%they will not be written into the antescofo score." e)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* event/setf tempo-change
;;; DESCRIPTION
;;; Store the tempo when a change is made. 
;;;
;;; NB: This creates a full tempo object, not just a number representing
;;; bpm. It also sets the display-tempo object to T as a side effect.
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
    (t (error "event::(setf tempo-change): argument should be a number ~
               or tempo object: ~a" value)))
  ;; MDE Mon Apr 25 11:26:54 2016
  (setf (display-tempo e) t)
  value)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* event/add-pitches
;;; DESCRIPTION
;;; Add pitches to a non-rest event. This works whether the event is a single
;;; pitch or a chord.
;;;
;;; NB This adds to the sounding pitches, not written pitches
;;; of transposing instruments. Events attached to transposing instruments will
;;; not yet work with this method. 
;;;
;;; If pitch objects are given, then the midi-channel of the pitch is presumed
;;; to be correct before this method is called. Symbols will default to MIDI
;;; channel 1.
;;; 
;;; ARGUMENTS
;;; - The event object
;;; - &rest: an arbitrary number of pitches: either pitch objects or symbols.
;;; 
;;; RETURN VALUE
;;; The same event but with the new pitches added.
;;; 
;;; EXAMPLE
#|
(let ((e1 (make-event 'c4 'q))
      (e2 (make-event '(c4 e4 g4) 'e)))
  (add-pitches e1 'cs3 'd5)
  (add-pitches e2 'cs2)))
|#
;;; SYNOPSIS
(defmethod add-pitches ((e event) &rest pitches)
;;; ****
  (setf pitches (init-pitch-list pitches))
  (cond ((is-chord e) (setf (pitch-or-chord e)
                            (add-pitches (pitch-or-chord e) pitches)))
        ((is-single-pitch e) (setf (pitch-or-chord e)
                                   (make-chord
                                    (cons (pitch-or-chord e) pitches)
                                    ;; MDE Thu Aug 22 11:13:04 2019
                                    :force-midi-channel nil)))
        (t (error "event::add-pitches: Can't add pitches to this event: ~a"
                  e)))
  e)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod (setf pitch-or-chord) (value (e event))
  ;; 13.2.11 really have to change the written note too
  (let* ((wporc (written-pitch-or-chord e))
         (porc (pitch-or-chord e))
         ;; MDE Tue Nov 20 17:51:34 2018 -- diff should actually always be an
         ;; integer but just for safety allow more accuracy; however don't allow
         ;; rounding errors to screw up low pitch transpositions in
         ;; :twelfth-tone scale (see test suite)
         (diff (when wporc (decimal-places (pitch- wporc porc) 4))))
    ;; (print diff)
    (setf-pitch-aux e value 'pitch-or-chord)
    (when (pitch-or-chord e)
      (setf (is-rest e) nil
            ;; MDE Thu Mar 20 15:55:52 2014 -- can't believe it's taken this
            ;; long!  
            (is-whole-bar-rest e) nil))
    ;; (print diff)
    (when wporc
      (setf (slot-value e 'written-pitch-or-chord)
            (if (pitch-or-chord e)
                (transpose (pitch-or-chord e) diff)
                nil))))
  value)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Thu May 30 18:49:56 2013 -- auto-sets the pitch-or-chord slot
;;; MDE Sat Dec 24 13:16:07 2016 -- optional instrument means the transposition
;;; will be set from its data rather than from existing sounding and written
;;; pitches.
(defmethod set-written-pitch-or-chord ((e event) value &optional instrument)
  ;; (print (data value))
  (let* ((wporc (written-pitch-or-chord e))
         (porc (pitch-or-chord e))
         (diff (if instrument
                   (transposition-semitones instrument)
                   (when wporc (decimal-places (pitch- porc wporc) 4)))))
    ;; (print diff)
    (setf-pitch-aux e value 'written-pitch-or-chord)
    (when (written-pitch-or-chord e)
      (setf (is-rest e) nil
            ;; MDE Thu Mar 20 15:55:52 2014 -- can't believe it's taken this
            ;; long!  
            (is-whole-bar-rest e) nil
            (slot-value e 'pitch-or-chord)
            (transpose (clone (written-pitch-or-chord e)) diff))))
  e)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Wed Nov 7 18:34:08 2018 -- for times when e.g. there's no existing pitch
;;; data and we've got a transposing instrument, e.g. for turning a rest into a
;;; note. if <value> is a pitch object it wil be cloned in setf-pitch-aux.
(defmethod set-pitch-or-chord ((e event) value &optional instrument)
  (let* ((wporc (written-pitch-or-chord e)) ; if we have it
         (porc (pitch-or-chord e))
         (diff (if instrument
                   (transposition-semitones instrument)
                   (when wporc (decimal-places (pitch- porc wporc) 4)))))
    ;; (print diff)
    (setf-pitch-aux e value 'pitch-or-chord)
    (setf (is-rest e) nil
          (is-whole-bar-rest e) nil)
    (when (and diff (not (zerop diff)))
      (setf (slot-value e 'written-pitch-or-chord)
            (transpose (clone (pitch-or-chord e)) (- diff)))))
  e)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod setf-pitch-aux ((e event) value slot)
  ;; (print '***setf-pitch-aux)  (print e) (print value) (print slot)
  (typecase value
    (pitch (setf (slot-value e slot) (clone value)))
    (chord (setf (slot-value e slot) (let ((c (clone value)))
                                       ;; MDE Sun Mar 18 16:43:14 2018 -- we
                                       ;; don't want the marks duplicated in the
                                       ;; event and the chord
                                       (setf (marks c) nil)
                                       c))
           ;; the cmn-data for a chord should be added to the event (whereas
           ;; the cmn-data for a pitch is only added to that pitch, probably
           ;; just a note-head change)
           (loop for m in (marks value) do
              ;; MDE Tue Feb 13 17:21:58 2018 -- we don't need the checking of
              ;; add-mark because that will already have been done with the
              ;; original chord, so just push directly 
              ;; (add-mark e m)))
                (push m (marks e))))
    ;; 26/3/07: nil shouldn't result in making a chord!
    (list (setf (slot-value e slot)
                (if value
                    (make-chord value :midi-channel (get-midi-channel e))
                    ;; 23.3.11 nil needs to set is-rest slot too!
                    (progn 
                      (setf (is-rest e) t)
                      ;; MDE Sat Nov  9 10:52:57 2013 -- and the tied-* slots?
                      ;; breaks the chop routine :/ 
                      ;; (setf (is-tied-to e) nil
                      ;;       (is-tied-from e) nil)
                      nil))))
    (symbol (setf (slot-value e slot) 
                  (make-pitch value :midi-channel (get-midi-channel e))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
                  ~%       midi-control-changes: ~a, ~
                  ~%       8va: ~a, player: ~a~
                  ~%       asco-label: ~a, asco-msgs: ~a~
                  ~%       set-ref: ~a~
                  ~%       pitch-or-chord: ~a~
                  ~%       written-pitch-or-chord: ~a"
          (start-time i) (end-time i) (duration-in-tempo i) 
          (compound-duration-in-tempo i)
          (amplitude i) (bar-num i) (marks-before i) (tempo-change i)
          (instrument-change i) (display-tempo i) (start-time-qtrs i) 
          (when (midi-time-sig i)
            (data (midi-time-sig i)))
          (midi-program-changes i) (midi-control-changes i) (8va i) (player i)
          (asco-label i) (asco-msgs i) (set-ref i) (pitch-or-chord i)
          (written-pitch-or-chord i)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone ((e event))
  (clone-with-new-class e 'event))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone-with-new-class :around ((e event) new-class)
  (declare (ignore new-class))
  (let ((rthm (call-next-method)))
    (copy-event-slots e rthm)
    rthm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Used e.g. in rthm-seq-bar
;;; Don't forget to copy the appropriate slots over in the scale method above
;;; as well!  

(defmethod copy-event-slots ((from event) (to event))
  (setf (slot-value to 'start-time) (start-time from)
        (slot-value to 'end-time) (end-time from)
        (slot-value to 'bar-num) (bar-num from)
        (slot-value to 'display-tempo) (display-tempo from)
        (slot-value to 'pitch-or-chord) (basic-copy-object
                                         (pitch-or-chord from))
        (slot-value to 'written-pitch-or-chord) 
        (basic-copy-object (written-pitch-or-chord from))
        ;; MDE Sat Apr 23 14:06:06 2016
        (slot-value to 'set-ref) (basic-copy-object (set-ref from))
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
        (slot-value to 'midi-control-changes) (my-copy-list
                                               (midi-control-changes from))
        ;; MDE Thu Aug 22 19:28:27 2013 
        (slot-value to 'player) (player from)
        ;; MDE Tue May  6 20:43:30 2014 
        (slot-value to 'asco-label) (asco-label from)
        (slot-value to 'asco-msgs) (asco-msgs from)
        (slot-value to 'midi-time-sig) (when (midi-time-sig from)
                                         (clone (midi-time-sig from))))
  to)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod porc-equal ((e1 event) (e2 event))
  (or (and (is-single-pitch e1)
           (is-single-pitch e2)
           (pitch= (pitch-or-chord e1) (pitch-or-chord e2)))
      (and (is-chord e1)
           (is-chord e2)
           (chord-equal (pitch-or-chord e1) (pitch-or-chord e2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Fri Dec 23 18:37:45 EST 2011 Added Robodoc info

;;; ****m* event/sharp-p
;;; DESCRIPTION
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Fri Dec 23 18:46:59 EST 2011 Added robodoc info

;;; ****m* event/flat-p
;;; DESCRIPTION
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Fri Dec 23 18:52:55 EST 2011 Added robodoc info

;;; ****m* event/natural-p
;;; DESCRIPTION
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* event/enharmonic
;;; DESCRIPTION
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
;;; keyword arguments:
;;; - :written. T or NIL to indicate whether the test is to handle the written
;;;   or sounding pitch in the event. T = written. Default = NIL.
;;; - :force-naturals. T or NIL to indicate whether to force "natural" note
;;;   names that contain no F or S in their name to convert to their enharmonic
;;;   equivalent (ie, B3 = CF4). NB double-flats/sharps are not implemented so
;;;   this will only work on F/E  B/C. 
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
;;; DJR Tue 29 Oct 2019 12:37:29 GMT -- change sharps to flats or flats to
;;; sharps
;;; DJR Mon 10 Feb 2020 18:27:40 GMT -- changed both the following so that there
;;; is no duplicated code. The method passes the pitch-or-chord slot of the
;;; event to either the method by the sme name for the chord class, or to the
;;; simplfied method for the pitch class.
;;; NB Both the following methods are non-destructive.
;;;
;;; ****m* event/sharp-to-flat
;;; AUTHOR
;;; Daniel Ross (mr.danielross[at]gmail[dot]com) 
;;; 
;;; DATE
;;; Mon 10 Feb 2020 18:31:17 GMT - London
;;; 
;;; DESCRIPTION
;;; Change the sharp (or sharps) in an event to a flat (or flats).
;;; 
;;; ARGUMENTS
;;; An event object
;;; 
;;; OPTIONAL ARGUMENTS
;;; None
;;; 
;;; RETURN VALUE
;;; An event object.
;;; 
;;; EXAMPLE
#|
(let ((e (make-event 'as4 'q)))
      (print-simple (sharp-to-flat e)))

=> BF4 Q, 
NIL

(let ((e (make-event '(bf4 as4) 'q)))
      (print-simple (sharp-to-flat e)))

=> (BF4 BF4) Q, 
NIL

;;; NB This method is non-destructove
(let ((e (make-event 'as4 'q)))
      (print-simple (sharp-to-flat e))
      (print-simple e))

=> BF4 Q, 
AS4 Q, 
NIL

|#
;;; SYNOPSIS
(defmethod sharp-to-flat ((e event) &optional clone written)
;;; ****
  (let* ((event (if clone (clone e) e))
         (slot (if written
                   'written-pitch-or-chord
                   'pitch-or-chord))
         (poc (slot-value event slot))
         (fts (sharp-to-flat poc)))
    (setf (slot-value event slot) fts)
    event))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* event/flat-to-sharp
;;; AUTHOR
;;; Daniel Ross (mr.danielross[at]gmail[dot]com) 
;;; 
;;; DATE
;;; Mon 10 Feb 2020 18:41:40 GMT - London
;;; 
;;; DESCRIPTION
;;; Change the flat (or flats) in an event to a sharp or (sharps).
;;; 
;;; ARGUMENTS
;;; An event object
;;; 
;;; OPTIONAL ARGUMENTS
;;; - clone: clone the evnet. DEFAULT = nil
;;; - written: apply to written or sounding pitch? DEFAULT = nil (sounding)
;;; 
;;; RETURN VALUE
;;; An event object
;;; 
;;; EXAMPLE
#|
(let ((e (make-event 'bf4 'q)))
      (print-simple (flat-to-sharp e)))

=> AS4 Q, 
NIL

(let ((e (make-event '(as4 bf4) 'q)))
      (print-simple (flat-to-sharp e)))

=> (AS4 AS4) Q, 
NIL

;;; NB This method is non-destructive
(let ((e (make-event 'bf4 'q)))
      (print-simple (flat-to-sharp e))
      (print-simple e))

=> AS4 Q, 
BF4 Q, 
NIL
|#
;;; SYNOPSIS
(defmethod flat-to-sharp ((e event) &optional clone written)
;;; ****
  (let* ((event (if clone (clone e) e))
         (slot (if written
                   'written-pitch-or-chord
                   'pitch-or-chord))
         (poc (slot-value event slot))
         (fts (flat-to-sharp poc)))
    (setf (slot-value event slot) fts)
    event))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* event/pitch-
;;; DESCRIPTION
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sun Apr 29 14:25:13 BST 2012: Added robodoc entry

;;; MDE: original comment:
;;; NB could screw up timing info in a bar

;;; SAR Fri Dec 23 20:45:16 EST 2011 Added robodoc info

;;; ****m* event/inc-duration
;;; DESCRIPTION
;;; Increase the duration of a given event object by a specified time in
;;; seconds. This will also result in new values for the end-time,
;;; duration-in-tempo, compound-duration, and compound-duration-in-tempo slots. 
;;;
;;; NB: Changing this value directly could result in incorrect timing info in a
;;;     bar. Also, the *-in-duration slots are changed by the same amount as the
;;;     other slots, i.e. tempo does not play a role here 
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
           (numberp (compound-duration e))
           (numberp (end-time e)))
      (progn
        (incf (duration-in-tempo e) inc)
        (incf (compound-duration-in-tempo e) inc)
        (incf (compound-duration e) inc)
        (incf (end-time e) inc))
      (error "~a~%~%event::inc-duration: can't increment non-number slots ~
              duration-in-tempo, compound-duration-in-tempo, end-time."
             e)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat Dec 24 09:03:26 EST 2011

;;; ****m* event/set-midi-time-sig
;;; DESCRIPTION
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

;;; In LilyPond, arrows with start and end text are made with
;;; TextSpanners. These have to be defined before the note on which the arrow
;;; will start and we have to know the start and end text in advance. So we'll
;;; add a CMN mark before which instead of being the usual symbol or string etc
;;; will be a list, the first element of which will be arrow, as an identifier,
;;; followed by the starting text and end text. This will be processed when we
;;; are writing the LilyPond file to create the TextSpanner. We will also add
;;; here start-arrow as a CMN mark and this will be attached to the note. An
;;; end-arrow mark should be attached to the note where the end text should
;;; appear.

;;; SAR Sat Dec 24 09:14:50 EST 2011 Added robodoc info
;;; SAR Sat Dec 31 09:22:16 EST 2011 Put date in DATE block

;;; ****m* event/add-arrow
;;; DATE
;;; 25 Jun 2011
;;;
;;; DESCRIPTION
;;; Adds a start-arrow mark to the given event object and stores text that is
;;; to be attached to the start and end of the given arrow for LilyPond
;;; output. This is a little more complex than the usual mark adding process,
;;; hence this separate method and it not being possible to add arrows to
;;; rthm-seq objects.  Not available for CMN. See also the slippery-chicken
;;; class method add-arrow-to-events 
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Fri Mar  2 13:40:07 GMT 2012: Added robodoc entry

;;; MDE Thu Mar  1 20:27:53 2012 

;;; ****m* event/end-arrow
;;; DESCRIPTION
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Fri Dec 30 11:21:12 EST 2011: Added robodoc info
;;; SAR Sat Dec 31 09:22:57 EST 2011: Put date in DATE block

;;; ****m* event/add-trill
;;; DATE
;;; 24 Sep 2011 
;;;
;;; DESCRIPTION
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat Dec 24 10:50:33 EST 2011: Added robodoc info
;;; SAR Sat Dec 31 09:18:11 EST 2011: Added DATE back

;;; ****m* event/end-trill
;;; DATE
;;; 24 Sep 2011
;;; 
;;; DESCRIPTION
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod add-mark-before ((e event) mark)
  ;; (print mark)
  ;; MDE Sat Dec 31 10:51:37 2011 
  (when (has-mark-before e mark)
    (warn "rhythm::add-mark-before: ~a already present but adding again!: ~a"
          mark e))
  (validate-mark mark)
  (push mark (marks-before e)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SAR Tue Aug  7 16:19:26 BST 2012: Added robodoc entry
;;; ****m* event/has-mark-before
;;; DESCRIPTION
;;; Determine whether a specifed event object has a specified mark in its
;;; MARKS-BEFORE slot.
;;; 
;;; ARGUMENTS
;;; - An event object.
;;; - A mark.
;;; 
;;; RETURN VALUE
;;; Returns the specified mark if the mark exists in the MARKS-BEFORE slot,
;;; otherwise returns NIL
;;; 
;;; EXAMPLE
#|
;;; Returns the specified mark if that mark is present
(let ((e (make-event 'c4 4)))
  (add-mark-before e 'ppp)
  (has-mark-before e 'ppp))

=> (PPP)

;;; Returns NIL if the specified mark is not present
(let ((e (make-event 'c4 4)))
  (add-mark-before e 'ppp)
  (has-mark-before e 'fff))

=> NIL

|#
;;; SYNOPSIS
(defmethod has-mark-before ((e event) mark &optional (test #'equal))
;;; ****
  (has-mark-aux (marks-before e) mark test))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat Dec 24 10:59:52 EST 2011 Added robodoc info

;;; ****m* event/add-clef
;;; DESCRIPTION
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat Dec 24 11:16:49 EST 2011 Added robodoc info

;;; ****m* event/get-clef
;;; DESCRIPTION
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat Dec 24 11:47:30 EST 2011

;;; ****m* event/delete-clefs
;;; DESCRIPTION
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

;; Setting the optional WARN argument to T suppresses the warning when no clefs
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* event/get-amplitude
;;; DESCRIPTION
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* event/voice-chord
;;; DATE
;;; November 28th 2018, Heidhausen
;;; 
;;; DESCRIPTION
;;; Set the amplitudes of the individual pitches in a chord event. Amplitude
;;; values should either be integers 0-127 or floating-point 0.0-1.0.
;;;
;;; Usually, the amplitude of each pitch in a chord is set by the overall
;;; amplitude slot of the event. However, if the pitch's amplitude slot is a
;;; number, it will be used instead, automatically, when generating MIDI files
;;; (and only those at present).
;;; 
;;; ARGUMENTS
;;; - the event object
;;; - either a list or a function: if a list, it will be cycled through
;;;   (returning to the beginning if necessary) to set the amplitudes of the
;;;   pitches of the chord from bottom to top; if a function then the function
;;;   is passed the event object and it should return a list of amplitudes which
;;;   will then be used to set the pitch objects' amplitudes similarly.
;;; 
;;; OPTIONAL ARGUMENTS
;;; None
;;; 
;;; RETURN VALUE
;;; The (modified) event object
;;; 
;;; EXAMPLE
#|
(let ((e (make-event '(c4 d4 e4 f4 g5 a4 b4) 4 :start-time 0.0)))
  (print (voice-chord e '(50 60 70))) ; velocities 
  (voice-chord e #'(lambda (e)
                     (loop for p in (get-pitch-symbols e) collect
                          (case p ; amplitude floats 
                            (c4 .1) (d4 .2) (e4 .3) (f4 .4) (t .5))))))
|#
;;; SYNOPSIS
(defmethod voice-chord ((e event) processor)
;;; ****
  (when (is-chord e)
    (typecase processor
      (function (voice-chord e (funcall processor e)))
      (list (let ((cscl (make-cscl processor)))
              ;; written-pitch-or-chord slot can't help us here
              (loop for p in (data (pitch-or-chord e)) do
                   (setf (amplitude p) (get-next cscl)))))
      (t (error "event::voice-chord: second argument should be a function or ~
                 list, not ~a" processor))))
  e)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* event/get-pitch-symbol
;;; DESCRIPTION
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
          (data obj)))))

;;; MDE Wed Nov 28 19:15:04 2018 -- for consistency
(defmethod get-pitch-symbols ((e event) &optional (written t))
  (get-pitch-symbol e written))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Thu Feb  9 19:59:45 2017 
(defmethod get-pitch-high-low ((e event) fun written)
  (let ((porc (get-porc e written)))
    (if (chord-p porc)
        (funcall fun porc)
        porc)))

(defmethod get-pitch-highest ((e event) &optional written)
  (get-pitch-high-low e #'highest written))

(defmethod get-pitch-lowest ((e event) &optional written)
  (get-pitch-high-low e #'lowest written))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat Dec 24 12:28:31 EST 2011 Added robodoc info

;;; ****m* event/no-accidental
;;; DESCRIPTION
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat Dec 31 12:32:24 EST 2011: Add robodoc info

;;; ****m* event/get-dynamic
;;; DESCRIPTION
;;; Gets the dynamic marking attached to a given event object. 
;;;
;;; NB: This method is similar to the event::get-dynamics method, but assumes
;;; that there is only one dynamic and returns that dynamic as a single symbol
;;; rather than a list. If the user suspects that multiple dynamics may have
;;; somehow have been added to the MARKS slot of the event class, use
;;; get-dynamics to obtain a list of all dynamics in that slot; otherwise, this 
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Thu Jan  9 08:41:00 2014 
;;; ****m* event/has-hairpin
;;; DESCRIPTION
;;; Return T or NIL depending on whether the event has a beginning or ending
;;; hairpin (cresc. or dim. line). 
;;; 
;;; ARGUMENTS
;;; - an event object
;;; 
;;; RETURN VALUE
;;; The hairpin mark the event has, as a list, or NIL if it has none.
;;;
;;; SYNOPSIS
(defmethod has-hairpin ((e event))
;;; ****
  (or (has-mark e 'cresc-beg)
      (has-mark e 'dim-beg)
      (has-mark e 'cresc-end)
      (has-mark e 'dim-end)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Tue Mar 28 14:29:37 2017 
(defmethod get-last-midi-channel ((e event))
  (let ((no (get-data (player e) (midi-channels e) nil)))
    (when no (data no))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Tue Mar 28 14:29:37 2017 
(defmethod set-last-midi-channel ((e event)); player channel)
  (let ((player (player e))
        (channel (get-midi-channel e)))
    ;; so rests are ignored as they don't have channel data, but midi-channels
    ;; slot is class allocation so data should still be available for the last
    ;; notes we saw
    (when channel 
      (if (get-data player (midi-channels e) nil)
          (set-data player (list player channel) (midi-channels e))
          (add (list player channel) (midi-channels e))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; finale's xml tag order within <note>is not arbitrary; unless you get it
;;; right you'll get error messages :/ it's pitch, duration, type, dot,
;;; accidental, time-mod, notehead, beam, notations
(defmethod write-xml :around ((e event) &key stream (divisions 16383)
                                          tuplet-actual-normals)
  ;; marks-before and marks slots are really just for CMN and lilypond. clefs
  ;; &c (marks-before) must come before <note>, <notehead> within <note>,
  ;; <direction> before note
  (let ((before '())
        (during '())                    ; e.g. <notations>
        (notehead '())
        (after '())
        ;; score not in C
        (poc (if (written-pitch-or-chord e)
                 (written-pitch-or-chord e)
                 (pitch-or-chord e)))
        accidental pitch-notehead
        ;; these must come before </note> i.e. in during var
        (notations '(beg-sl end-sl beg-phrase end-phrase beg-gliss end-gliss
                     a s te ts as at c1 c2 c3 c4 c5 c6 pause short-pause
                     long-pause t3 arp lhp bartok nail flag downbow upbow open
                     ;; harm 0 1 2 3 4 5)) ; fingerings now handled by integerp
                     harm))
        ;;(directions '(beg-8va end-8va beg-8vb end-8vb beg-15ma beg-15mb
        ;;            end-15ma end15mb cresc-beg cresc-end dim-beg dim-end
        ;;          ped ped^ ped-up uc tc sost^
        (noteheads '(circled-x x-head triangle wedge square triangle-up
                     improvOn flag-head)))
    (write-xml-ins-change e stream)     ; if it exists
    (set-last-midi-channel e)
    ;; if the <marks> are in <syms> (or an int=fingering) they'll be removed
    ;; from <rest> and pushed into <syms-holder>
    (macrolet ((separate (marks syms syms-holder rest)
                 `(loop for m in ,marks
                     for int = (integerp m) ; fingerings
                     do
                     ;; this will also have the effect of removing double
                     ;; marks, which is fine.
                       (when (or (member m ,rest) int)
                         (setq ,rest (remove m ,rest)))
                       (if (or (member m ,syms) int)
                           (push m ,syms-holder)
                           (push m ,rest)))))
      ;; (print '**************)
      ;; (print before) (print during) (print notehead) (print after)
      (separate (marks-before e) noteheads notehead before)
      (separate (marks e) noteheads notehead after)
      ;; of course <after> might have some <notations> in it now, hence <when>
      ;; in separate above
      ;; (separate (marks e) notations during after))
      ;; MDE Wed Jul  8 17:30:29 2020, Heidhausen -- we've handled noteheads now
      ;; so don't pass them again
      (separate (set-difference (marks e) noteheads) notations during after))
    ;; (print '****)
    ;; (print before) (print during) (print notehead) (print after)
    ;; we've got the problem of short-pause and long-pause not having symbols
    ;; in xml, so we add a normal fermata with words (e.g. "lunga") above. but
    ;; we can't have those in the same place due to xml tag order...what a
    ;; faff! 
    (macrolet ((mreplace (mark replacement string list)
                 `(when (member ,mark ,list)
                    (setf ,list (substitute ,replacement ,mark ,list))
                    (push ,string after))))
      (mreplace 'long-pause 'pause "lunga" during)
      (mreplace 'short-pause 'pause "breva" during))
    ;; MDE Sun Mar 18 16:12:34 2018, Bangkok -- fingerings are in notehead now
    ;; so remove
    (setq notehead (remove-if #'integerp notehead))
    ;; this will mean we can't have a fingering with an alt. notehead, which may
    ;; need to change 
    (when (> (length notehead) 1)
      ;; (when (> (count-if #'symbolp notehead) 1)
      (error "event::write-xml: can only have one notehead mark, you have ~a ~
              ~&~a" notehead e))
    (when (and (tempo-change e) (display-tempo e))
      (write-xml (tempo-change e) :stream stream))
    ;; make sure end-gliss comes before beg-gliss, just in case we have end and
    ;; beg gliss on same note
    (setq during (move-to-end 'beg-gliss (reverse during))
          before (reverse before)
          after (reverse after))
    (xml-write-marks before stream)
    (xml-write-marks after stream)
    (format stream "~&      <note> <!-- bar-pos=~a - - - - - - - - - - - - -->"
            (bar-pos e))
    (when (is-grace-note e)
      (format stream "~&        <grace />"))
    (if (or (not poc) (pitch-p poc))    ; single pitch or rest
        (progn
          (when poc                     ; single pitch
            ;; MDE Sat Oct 13 10:40:09 2018 -- got to write notehead tag after
            ;; duration and tie---as the rhythm class does---so the pitch method
            ;; now removes noteheads before writing other marks, and instead
            ;; returns it here
            (multiple-value-setq (accidental pitch-notehead)
              (write-xml poc :stream stream)))
                                        ; rhythm class
          (call-next-method e :stream stream :divisions divisions
                            :accidental accidental
                            ;; MDE Sat Oct 13 10:51:38 2018 -- so the notehead
                            ;; var is the one we grabbed from the event's marks,
                            ;; but pitch-notehead may have arrived via the
                            ;; pitch's marks...prefer the event's but do use the
                            ;; pitch's if given
                            :notehead (or notehead pitch-notehead)
                            :tuplet-actual-normals tuplet-actual-normals))
        ;; it's a chord
        (loop for p in (data poc) and i from 0 do
             (when (> i 0)
               (format stream "~&      </note>~
                               ~&      <note>")
               (when (is-grace-note e)
                 (format stream "~&        <grace />"))
               (format stream "~&        <chord />"))
           ;; MDE Sat Oct 13 10:40:09 2018 
             (multiple-value-setq (accidental pitch-notehead)
               (write-xml p :stream stream))
           ;; (setq accidental (write-xml p :stream stream))
           ;; (write-xml p :stream stream)
           ;; chord notes need all the rhythm info of a non-chord note :/
           ;; rhythm class:
             (call-next-method e :stream stream :divisions divisions
                               :basic (> i 0) :accidental accidental
                               :tuplet-actual-normals tuplet-actual-normals
                               :notehead (or notehead pitch-notehead))))
    ;; NB these are the visual elements of the tie. The tie tag is the sonic
    ;; element (i.e. that the note is not restruck) and comes in the rhythm
    ;; class method
    (when (is-tied-to e)
      (xml-notation-with-args stream "tied" "type=\"stop\""))
    (when (is-tied-from e)
      (xml-notation-with-args stream "tied" "type=\"start\""))
    (xml-write-marks during stream)
    ;; now in rhythm class because of enforced tag order :/
    ;; (xml-write-marks notehead stream)
    (format stream "~&      </note>"))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Tue Mar 28 16:01:48 2017
(defmethod write-xml-ins-change ((e event) stream &optional part)
  (when (instrument-change e)
    (let* ((ins (fourth (instrument-change e)))
           (midi-channel (get-midi-channel e))
           (xml-name (xml-staffname ins)))
      (unless midi-channel              ; could be a rest
        (setq midi-channel (get-last-midi-channel e)))
      (format stream "~&       <print>~
                      ~&         <part-name-display>~
                      ~&           ~a~
                      ~&         </part-name-display>~
                      ~&         <part-abbreviation-display>~
                      ~&           ~a~
                      ~&         </part-abbreviation-display>~
                      ~&       </print>"
              xml-name (xml-staffname ins t))
      ;; MDE Wed Mar 14 14:18:59 2018 -- if this is the first event of the bar
      ;; and happens to be a rest then we can't gewt the midi-channel but we
      ;; don't need to write this info. again anyway.
      (when (and midi-channel (or part (player e)))
        (format stream "~&       <sound>~
                        ~&         <midi-instrument id=\"~a\">~
                        ~&         <midi-channel>~a</midi-channel>~
                        ~&         <midi-program>~a</midi-program>~
                        ~&         </midi-instrument>~
                        ~&       </sound>"
                (if part part (player e))
                midi-channel
                (midi-program ins)))
      ;; add "To piccolo" or whatever, as words in the score.
      (format stream "~&       <direction placement=\"above\">~
                      ~&         <direction-type>~
                      ~&           <words>Take ~a</words>~
                      ~&         </direction-type>~
                      ~&       </direction>"
              ;; would like to use xml-name here to get e.g. flat turned into
              ;; the musical flat sign, but music xml doesn't allow any tags
              ;; e.g. <display-text> inside <words>
              (staff-name ins))))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Mon Jul 23 13:50:04 2012 
(defmethod lp-get-ins-change ((e event))
  (let ((result '()))
    (when (instrument-change e)
      (let ((long (first (instrument-change e)))
            (short (second (instrument-change e)))
            (lines (third (instrument-change e))))
        (when long
          (push (format nil "~a~%" (lp-set-instrument long)) result))
        (when short
          (push (format nil "~a~%" (lp-set-instrument short t)) result))
        (when long
          (push (format nil "s1*0\^\\markup { ~a }~%" (lp-flat-sign long))
                result))
        ;; MDE Fri Nov 13 16:35:00 2015 -- different numbers of staff lines now
        ;; working: don't just change the number of staff lines when it's other
        ;; than 5: could be that the last ins had < 5 and now we're changing
        ;; back to 5
        (unless (and (= 5 lines) (= 1 (bar-num e)))
          (push (format nil "\\stopStaff \\override Staff.StaffSymbol.~
                             line-count = #~a \\startStaff " lines)
                result))))
    (nreverse result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; lilypond: grace notes work fine.
(let ((grace-notes '()))
  (defmethod get-lp-data ((e event) &optional in-c ignore1 ignore2)
    (declare (ignore ignore1 ignore2))
    ;; (print (data e))
    ;; MDE Tue Jan 7 10:30:17 2014 -- hairpin0 has to come before \< (cresc) or
    ;; \> (dim) not at end \!
    (when (and (has-mark e 'hairpin0)
               ;; MDE Sat Feb 18 17:46:24 2017 -- we might well have a dim-end
               ;; but also a dim-beg on the same event, so what's crucial is
               ;; that there's a *-beg
               ;; (or (has-mark e 'cresc-end)
               ;; (has-mark e 'dim-end)))
               (not (or (has-mark e 'cresc-beg)
                        (has-mark e 'dim-beg))))
      (error "event::get-lp-data: the mark 'hairpin0 should be attached to ~
             the event ~%with the cresc-beg or dim-beg mark, not at the end of ~
             the hairpin: ~a" e))
    (when (and (not in-c) (not (is-rest e)) (not (written-pitch-or-chord e)))
      (error "event::get-lp-data: request for non-existent ~
              transposed pitch: ~%~a" e))
    ;; 2.3.11: some marks (or rather noteheads) need to be set before in
    ;; lilypond but with the note in cmn; so move these over here 
    (multiple-value-bind 
          (from to)
        (separate-marks-before (marks e) (marks-before e))
      ;; MDE Fri Mar 24 10:49:38 2017 -- todo: really shouldn't be changing
      ;; slots, rather, creating local vars for these to be processed here
      (setf (marks e) from
            (marks-before e) to))
    (flet ((get-poc ()
             (if (and in-c (not (from-8ve-transposing-ins e)))
                 (pitch-or-chord e)
                 (written-pitch-or-chord e))))
      (let* ((poc nil)
             (note 
              (progn
                ;; MDE Mon Apr 30 20:03:35 2012 -- harmonic note heads must be
                ;; a chord in LP!!!!
                ;; MDE Thu Dec 26 14:34:01 2013 -- guitar string numbers have
                ;; to handled similarly: as a chord with the e.g. \1 within the
                ;; chord
                (loop for m in '(flag-head c1 c2 c3 c4 c5 c6) do
                     (when (has-mark e m)
                       (force-chord e)
                       (unless poc
                         (setf poc (get-poc)))
                       (loop for p in (data poc) do (add-mark p m))
                       (rm-marks e m)))
                (unless poc (setf poc (get-poc)))
                (cond ((is-rest e) "r")
                      ;; we handle these with the next normal notes
                      ((is-grace-note e) 
                       (push poc grace-notes))
                      ;; it's a note or chord
                      (t (get-lp-data poc)))))
             (result '())
             (rthm (unless (is-grace-note e)
                     ;; MDE Wed Jun 24 17:17:56 2015 -- we have the
                     ;; letter-value slot for Lilypond's use so this shouldn't
                     ;; be necessary.
                     ;; (round (nearest-power-of-2 (undotted-value e)))))
                     (letter-value e)))
             ;; so, if the bracket slot is set, and the first element is a
             ;; list, we've got tuplet brackets so loop for each sublist and
             ;; set the e.g. \times 2/3 { to be the second element of the
             ;; sublist.  if it's just a positive integer, that's the end of
             ;; the tuplet so close with }.  otherwise, unless tuplet-scaler is
             ;; 1, we've got tuplets without brackets so use the e.g. 4*2/3
             ;; (tq) notation.  other than that, just use the nearest power of
             ;; 2 to the value. In all cases don't forget to add the dots.
             (close-tuplets 0))
        ;; MDE Mon Nov 26 17:42:14 2012 
        ;; MDE Wed May 14 18:53:16 2014 -- make sure it's not nil also as rthm
        ;; will be nil if we've got a grace note
        (when (and (numberp rthm) (zerop rthm))
          (error "event::get-lp-data: can't get nearest-power-of-2 for ~a" e))
        ;; MDE Mon Jul 23 13:52:40 2012 -- split out into above method
        (loop for s in (lp-get-ins-change e) do (push s result))
        (when (and (tempo-change e) (display-tempo e))
          (push (get-lp-data (tempo-change e)) result))
        (unless (is-grace-note e)
          (when (bracket e)
            ;; MDE Mon Jan 12 14:05:17 2015 -- nested tuplets as with vc at
            ;; beginning of slippery when wet
            (cond ((> (length (bracket e)) 1)
                   (loop for b in (bracket e) do
                        (if (listp b)
                            (push (get-lp-tuplet (second b)) result)
                            (when (integer>0 b)
                              (incf close-tuplets)))))
                  ;; MDE Mon Jan 12 14:31:51 2015 -- the simple non-nested case
                  ((listp (first (bracket e)))
                   (push 
                    ;; MDE Fri Apr  4 14:36:48 2014 -- just use the existing
                    ;; slot rather than case!
                    ;; MDE Thu Jun 25 10:48:11 2015 -- use the dedicated
                    ;; function now rather than just the tuplet scaler
                    ;; formatted here.  
                    ;; (format nil "\\times ~a { " (tuplet-scaler e))
                    (get-lp-tuplet (second (first (bracket e)))) 
                    result))
                  ((integer>0 (first (bracket e)))
                   (incf close-tuplets))))
          ;; MDE Wed Jun 24 18:19:17 2015 -- we handle this now at the
          ;; rthm-seq-bar init level.
          ;; hack-alert: if we're under two tuplet brackets our rhythm would be
          ;; twice as fast as it should be notated
          ;; (when (> (length (bracket e)) 1)
          ;; (print 'hack-alert)
          ;; (setf rthm (* rthm 2)))
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
          (when (marks-before e)
            (loop for thing in (marks-before e) do
               ;; handle clefs here rather than in lp-get-mark
                 (if (listp thing)
                     (case (first thing)
                       (clef (push (get-lp-clef (second thing)) result))
                       ;; MDE Thu Dec 10 16:11:12 2020, Heidhausen -- we have to
                       ;; differentiate between rgb marks for individual notes
                       ;; (which use \tweak) and events (e.g. chords) which
                       ;; should use \override  
                       (rgb (push (lp-get-mark
                                   (list 'rgb (econs (second thing) t)))
                                  result))
                       (t (push (lp-get-mark thing) result)))
                     (push (lp-get-mark thing) result))))
          (push note result)
          ;; MDE Thu Jul  2 15:52:52 2015 -- make sure we write an integer
          ;; instead of something like 32.0 
          (push (format nil "~a"
                        ;; MDE Thu Dec  5 11:34:00 2019 -- special cases for
                        ;; breva, longa, maxima. 
                        (cond ((equal-within-tolerance rthm 0.5)
                               "\\breve")
                              ((equal-within-tolerance rthm 0.25)
                               "\\longa")
                              ((equal-within-tolerance rthm 0.125)
                               (warn "event::get-lp-data: in Lilypond, ~
                                      \\maxima is only possible in ~%ancient ~
                                      music notation.")
                               "\\maxima")
                              (t (round rthm))))
                result)
          (push (make-string (num-dots e) :initial-element #\.) result)
          (when (and (not (bracket e))  ; tuplets without brackets
                     (/= (tuplet-scaler e) 1))
            (push (format nil "*~a" (tuplet-scaler e)) result))
          (when (is-tied-from e)
            (push "~~" result))
          (when (beam e)
            (when (< rthm 8)
              (warn "event::get-lp-data: beam on rhythm (~a) < 1/8th ~
                     duration: ~a"
                    rthm e))
            (if (zerop (beam e))
                (push "\]" result)
                (push "\[" result)))
          (push " " result)
          (when (marks e)
            ;; 22.5.11: getting a little tricky this but: in cmn we attach
            ;; ottava begin and end marks to the same note and everything's
            ;; fine; in lilypond, the begin or end must always come before the
            ;; note.  we can't move the end to the next note's marks-before
            ;; because that wouldn't work in cmn, so just move it to the end of
            ;; the marks
            #|(loop for mark in (move-to-end
                               'end-8va 
                               (move-to-end 'end-8vb (marks e)))|#
            (loop for mark in (move-all-to-end
                               '(end-8va end-8vb end-15ma end-15vb)
                               (marks e))
               for lp-mark = (lp-get-mark mark :num-flags (num-flags e))
               do
                 (when lp-mark
                   (push lp-mark result))))
          (loop repeat close-tuplets do (push " \} " result))
          ;; (print result)
          (setf result
                (move-to-end ">> "
                             (move-to-end "} " (reverse result) #'string=)
                             #'string=))
          ;; (print result)
          (list-to-string result ""))))))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; I think cmn should understand the (duration ...) function but seems to fail
;;; with this, so use its rq function instead.

(defmethod get-cmn-data ((e event) &optional bar-num from-pitch-info-only
                         process-event-fun (in-c t) 
                         display-marks-in-part
                         print-time ignore1 ignore2)
  (declare (ignore ignore1 ignore2))
  ;; (print in-c)
  ;; (print (display-tempo e))
  #+cmn
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
                                        ;; MDE Thu Jun  7 15:18:43 2012 -- 
                                        (marks-in-part e)))
                              (when (display-tempo e)
                                (cmn-tempo (tempo-change e)))
                              ;; can't set short name in cmn: it's auto-done
                              (first (instrument-change e)))))
          ((is-rest e) (cmn::cmn-rest (rq e) (num-dots e) (num-flags e) 
                                      (bracket e) bar-num 
                                      (append (marks e)
                                              (when display-marks-in-part
                                                (marks-in-part e)))
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
                                        (marks-in-part e)))))
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
                                      (marks-in-part e)))
                            (when print-time
                              (cmn-time e))
                            (when (display-tempo e)
                              (cmn-tempo (tempo-change e)))
                            (first (instrument-change e)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod cmn-time ((e event))
  (when (start-time e)
    (cmn::sc-cmn-text 
     (secs-to-mins-secs (start-time e))
     :font-size 6)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat Dec 24 15:15:57 EST 2011 Added robodoc info

;;; ****m* event/is-chord
;;; DESCRIPTION
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
;; Returns NIL if not a chord           ;
(let ((e (make-event 'c4 'q)))
  (is-chord e))

=> NIL

;; If a chord, returns the number of notes in the chord ;
(let ((e (make-event '(c4 e4 g4) 'q)))
  (is-chord e))

=> 3

;; A rest is not a chord                ;
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
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat Dec 24 15:31:18 EST 2011 Added robodoc info

;;; ****m* event/is-single-pitch
;;; DESCRIPTION
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
;; Returns T if the event object consists of a single pitch ;
(let ((e (make-event 'c4 'q)))
  (is-single-pitch e))

=> T

;; Returns NIL if the event object is a chord ;
(let ((e (make-event '(c4 e4 g4) 'q)))
  (is-single-pitch e))

=> NIL

;; Also returns NIL if the event object is a rest ;
(let ((e (make-rest 'q)))
  (is-single-pitch e))

=> NIL

|#
;;; SYNOPSIS
(defmethod is-single-pitch ((e event))
;;; ****
  (typep (pitch-or-chord e) 'pitch))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat Dec 24 15:46:23 EST 2011: Added robodoc info
;;; SAR Wed Jun 13 12:16:02 BST 2012: Expanded robodoc

;;; ****m* event/transpose
;;; DESCRIPTION
;;; Transpose the pitch content of a given event object by a specified number
;;; of semitones. This method can be applied to chords or single-pitches.
;;;
;;; If functions are given, these will be used for the note or chord in the
;;; event, whereby semitones may or may not be NIL in that case (transposition
;;; could be dependent on the note or chord rather than being a fixed shift).
;;;
;;; NB: By default this method returns a modified clone of the original rather
;;;     than changing the values of the original itself. The user can choose to
;;;     replace the values of the original by setting the keyword argument
;;;     :destructively to T.
;;; 
;;; ARGUMENTS
;;; - An event object.
;;; - A number (can be positive or negative).
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :destructively. T or NIL to indicate whether the method is to replace the
;;;   pitch values of the original event object (T) or return a new event
;;;   object with the new pitches (NIL). Default = NIL.  
;;; - :chord-function. A function to be used for the transposition of
;;;   chords. Default = #'transpose.
;;; - :pitch-function. A function to be used for the transposition of
;;;   pitches. Default = #'transpose.
;;; 
;;; RETURN VALUE
;;; An event object.
;;; 
;;; EXAMPLE
#|
;; Transpose returns an event object    ;
(let ((e (make-event 'c4 'q)))
  (transpose e 1))

=> 
EVENT: start-time: NIL, end-time: NIL, 
duration-in-tempo: 0.0, 
[...]

;; By default transpose returns a modified clone, leaving the original event ;
;; object untouched.                    ;
(let ((e (make-event 'c4 'q)))
  (print (data (pitch-or-chord (transpose e 1))))
  (print (data (pitch-or-chord e))))

=>
CS4 
C4 

;; When the keyword argument :destructively is set to T, the data of the ;
;; original event object is replaced    ;
(let ((e (make-event 'c4 'q)))
  (transpose e 1 :destructively t)
  (data (pitch-or-chord e)))

=> CS4

;; Can transpose by 0 as well (effectively no transposition) ;
(let ((e (make-event 'c4 'q)))
  (transpose e 0 :destructively t)
  (data (pitch-or-chord e)))

=> C4

;; ...or by negative intervals          ;
(let ((e (make-event 'c4 'q)))
  (transpose e -3 :destructively t)
  (data (pitch-or-chord e)))

=> A3

;; Can transpose chords too             ;
(let ((e (make-event '(c4 e4 g4) 'q)))
  (transpose e -3 :destructively t)
  (loop for p in (data (pitch-or-chord e)) collect (data p)))

=> (A3 CS4 E4)

|#
;;; 
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
;;; ****m* event/set-written
;;; DESCRIPTION
;;; Set the written pitch (as opposed to sounding; i.e., for transposing
;;; instruments) of a given event object. The sounding pitch remains unchanged
;;; as a pitch object in the PITCH-OR-CHORD slot, while the written pitch is
;;; added as a pitch object to the WRITTEN-PITCH-OR-CHORD slot.
;;; 
;;; ARGUMENTS
;;; - An event object.
;;; - A number indicating the difference in semitones (positive or
;;;   negative) between the written and sounding pitches.  E.g. to set the
;;;   written-note for a B-flat Clarinet, this would be 2, for an E-flat
;;;   Clarinet, it would be -3.
;;; 
;;; RETURN VALUE
;;; The 'written' pitch object.
;;; 
;;; EXAMPLE
#|
;; Returns a pitch object (here for example for a D Trumpet or Clarinet) ;
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

;; Create a single-pitch event object, set it's written pitch to two half-steps ;
;; lower, and print the corresponding data slots ;
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
;;; DESCRIPTION
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
;; Create an event object, print the contents of the written-pitch-or-chord ;
;; slot to see it's set to NIL, set-written to -2, print the contents of the ;
;; corresponding slot to see the data of the newly created pitch object, ;
;; delete-written, print the contents of the written-pitch-or-chord slot to see ;
;; it's empty.                          ;
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
;;; ****m* event/lowest
;;; DESCRIPTION
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
;; Returns a pitch object               ;
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

;; Returns the lowest note of a chord object within an event object ;
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
;;; ****m* event/highest
;;; DESCRIPTION
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

;; Returns the highest note of a chord object within an event object ;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* event/event-distance
;;; DESCRIPTION
;;; Get the distance (interval) in semitones between the pitch level of one
;;; event object and a second. Negative numbers indicate direction interval
;;; directionality. An optional argument allows distances to be always printed
;;; as absolute values (positive).
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
;;; - T or NIL for whether the value should be returned as an absolute
;;;   value (i.e., always positive). Default = NIL. 
;;; 
;;; RETURN VALUE
;;; A number.
;;; 
;;; EXAMPLE
#|
;; The semitone distance between two single pitches in ascending direction ;
                     (let ((e1 (make-event 'c4 'q))
(e2 (make-event 'e4 'q)))
(event-distance e1 e2))

                     => 4.0

;; The semitone distance between two single pitches in descending direction ;
                     (let ((e1 (make-event 'c4 'q))
(e2 (make-event 'e4 'q)))
(event-distance e2 e1))

                     => -4.0

;; Set the optional argument to T to get the absolute distance (positive ;
;; number)                              ;
                     (let ((e1 (make-event 'c4 'q))
(e2 (make-event 'e4 'q)))
(event-distance e2 e1 t))

                     => 4.0

;; The semitone distance between two chords in ascending direction ;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod bad-interval-p ((e1 event) (e2 event) &optional written)
  (when (and (is-single-pitch e1) (is-single-pitch e2))
    (bad-interval (get-porc e1 written) (get-porc e2 written))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-porc ((e event) &optional written)
  (if written
      (written-pitch-or-chord e)
      (pitch-or-chord e)))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sun Dec 25 08:23:34 EST 2011 Added robodoc info

;;; ****m* event/force-rest
;;; DESCRIPTION
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

;; The method returns an event object.  ;
                     (let ((e (make-event 'c4 'q)))
(force-rest e))

                     => 
                     EVENT: start-time: NIL, end-time: NIL, 
                     [...]

;; Create an event object, apply force-rest, then print the corresponding slots ;
;; to see the effectiveness of the method ;
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
  ;; MDE Fri Aug 17 17:20:43 2012 
  (delete-beam e)
  (setf (pitch-or-chord e) nil
        (written-pitch-or-chord e) nil
        ;; 23.7.11 (Pula) remove marks that can only be used on a note
        (marks e) (remove-if #'mark-for-note-only (marks e))
        ;; (8va e) 0
        (marks-before e) (remove-if #'mark-for-note-only
                                          (marks-before e))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 22.9.11 
(defmethod reset-8va ((e event))
  (rm-marks e '(beg-8va beg-8vb end-8va end-8vb) nil)
  (setf (8va e) 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SAR Sun Dec 25 08:50:52 EST 2011: Added robodoc info
;;; SAR Sat Dec 31 09:20:00 EST 2011: Put date in DATE block

;;; ****m* event/force-artificial-harmonic
;;; DATE 20 Aug 2011
;;; 
;;; DESCRIPTION
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
;;; OPTIONAL ARGUMENTS
;;; - an instrument object
;;; 
;;; RETURN VALUE
;;; The chord object which creates the harmonic.
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
(defmethod force-artificial-harmonic ((e event)
                                      &optional instrument (warn t)
                                        (naturals t) ignore)
;;; ****
  (declare (ignore ignore))
  ;; MDE Thu Nov 1 18:27:48 2018 -- moved most of the logic over to the pitch
  ;; class
  (let ((chord (if (is-single-pitch e)
                   (force-artificial-harmonic (pitch-or-chord e) instrument
                                              warn t naturals)
                   (when warn
                     (warn "~a~%event::force-artificial-harmonic: event is a ~
                            chord or rest: skipping."
                           e)))))
    (when chord (setf (pitch-or-chord e) chord))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod force-natural-harmonic ((e event)
                                   &optional instrument (warn t) (tolerance 15))
;;; ****
  (let* ((ins (when instrument (if (instrument-p instrument)
                                  instrument
                                  (get-standard-ins instrument))))
         (result (if (is-single-pitch e)
                    (force-natural-harmonic
                     (if (written-pitch-or-chord e)
                         (written-pitch-or-chord e)
                         (pitch-or-chord e))
                     instrument warn tolerance)
                    (when warn
                      (warn "~a~%event::force-natural-harmonic: event is a ~
                             chord or rest: skipping."
                            e)))))
    (when result
      ;; (print (get-pitch-symbol e))
      ;; grab the marks and move them to the event
      (add-marks e (marks result))
      ;; now remove the marks from the pitch object
      (if ins
          (rm-marks result (cons 'harm (open-string-marks ins)) nil)
          ;; NB this might remove previously added marks but without the
          ;; instrument object there's not a lot else we can do 
          (delete-marks result))
      ;; don't need to repeat the string number on tied notes
      (when (and ins (open-string-marks ins) (is-tied-to e))
        ;; (print (bar-num e))
        (rm-marks e (open-string-marks ins) nil))
      (if (written-pitch-or-chord e)
          (setf (written-pitch-or-chord e) result)
          (setf (pitch-or-chord e) result)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod (setf 8va) :before (value (e event))
  (unless (or (= value 0) (= value -1) (= value 1))
    (error "event::(setf 8va): 8va slot can only be -1, 0, or 1"))
  value)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod delete-marks :after ((e event))
  (unless (is-rest e)
    (delete-marks (pitch-or-chord e))
    (when (written-pitch-or-chord e)
      (delete-marks (written-pitch-or-chord e)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Tue Apr 17 12:52:22 2012.  Works for chords and single pitches.  Always
;;; returns a list.
;;; MDE Fri Oct  2 11:11:36 2015 -- updated to work for a first argument of a
;;; list of sndfile objects or freqs

(defmethod src-for-sample-freq (freq (e event))
  ;; (print freq)
  (when (symbolp freq) (setq freq (note-to-freq freq)))
  (let ((pitches (if (is-chord e)
                     (data (pitch-or-chord e))
                     (list (pitch-or-chord e)))))
    (flet ((data-error ()
             (error "event::src-for-sample-freq: first argument should be ~
                     either a number (frequency), a list of numbers (one for ~
                     each pitch in the chord), or a list of sndfile objects: ~a"
                    freq)))
      (loop for p in pitches and i from 0 collect
         ;; MDE Fri Oct  2 09:38:47 2015 -- use the pitch method
           (src-for-sample-freq
            (typecase freq
              (number freq)
              (list (let ((f (nth i freq)))
                      (typecase f
                        (number f)
                        (sndfile (frequency f))
                        (t (data-error)))))
              (t (data-error)))
            p)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Thu Apr 19 11:59:19 2012 

;;; ****m* event/get-degree
;;; DESCRIPTION
;;; Get the degree of the event, summing or averaging for chords.
;;; 
;;; ARGUMENTS
;;; - an event object
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :written. T or NIL to indicate whether to use the written (in the case of
;;;   transposing instruments) or sounding pitches. T = written. Default = NIL.
;;; - :sum. T or NIL to indicate whether to return the sum of the degrees
;;;   instead of a list (see below). T = degrees. Default = NIL.
;;; - :average: sim. to sum but T or NIL to indicate whether to return the
;;;    average instead of sum.
;;; 
;;; RETURN VALUE 
;;; By default this returns a list (even if it's a single pitch), unless :sum T
;;; whereupon it will return a single value: the sum of the degrees if a chord,
;;; otherwise just the degree. :average T will return the average of the sum.
;;; A rest would return '(0) or 0.
;;; 
;;; EXAMPLE
#|
;;; NB This uses the quarter-tone scale so degrees are double what they would ;
;;;    be in the chromatic-scale.       ;
(let ((event (make-event '(cs4 d4) 'e))
      (rest (make-rest 'e)))
  (print (get-degree event))
  (print (get-degree rest))
  (print (get-degree event :average t))
  (get-degree event :sum t))
(122 124) 
(0)
123.0
246
|#
;;; SYNOPSIS
(defmethod get-degree ((e event) &key written sum average)
;;; ****      
  (let* ((poc (if written
                  (written-pitch-or-chord e)
                  (pitch-or-chord e)))
         (list (cond
                 ((is-rest e) '(0))
                 ((is-chord e)
                  (loop for p in (data poc) collect (degree p)))
                 (t (list (degree poc)))))
         (summed (when (or sum average) (loop for d in list sum d))))
    (when (and sum average)
      (error "event::get-degree: either :sum or :average but not both."))
    (cond (sum summed)
          (average (float (/ summed (length list))))
          (t list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* event/get-frequency
;;; DATE
;;; May 26th 2016, Edinburgh
;;; 
;;; DESCRIPTION
;;; Return the frequency of the event's pitch, or a list of frequencies if it's
;;; a chord.
;;; 
;;; ARGUMENTS
;;; - the event object
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :written. T or NIL to indicate whether to use the written (in the case of
;;;   transposing instruments) or sounding pitches. T = written. Default = NIL.
;;; - :force-list. T or NIL to force a single pitch's frequency into a
;;;   list. Default = NIL.
;;; - :average. T or NIL to make chords average their frequency.
;;; 
;;; RETURN VALUE
;;; A single frequency (float) or list of frequencies in the case of a chord.
;;; 
;;; SYNOPSIS
(defmethod get-frequency ((e event) &key written force-list average)
;;; ****
  (if (is-rest e)
      0.0
      (let* ((poc (if written
                      (written-pitch-or-chord e)
                      (pitch-or-chord e))))
        (if (is-chord e)
            (if average
                (/ (loop for p in (data poc) sum (frequency p))
                   (sclist-length poc))
                (loop for p in (data poc) collect (frequency p)))
            ;; single pitch
            (if force-list (list (frequency poc)) (frequency poc))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* event/replace-mark
;;; DESCRIPTION
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
;; Add marks to the MARKS slot and replace 'a with 'batt ;
                     (let ((e (make-event 'c4 'q)))
(loop for m in '(a s pizz) 
do (add-mark e m))
(replace-mark e 'a 'batt))

                     => (PIZZ S BATT)

;; Add marks to the MARKS-BEFORE slot and replace 'arco with 'at ;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Thu Dec 26 14:12:55 2013
(defmethod force-chord ((e event))
  (when (is-single-pitch e)
    (setf (slot-value e 'pitch-or-chord) (make-chord (pitch-or-chord e)))
    (when (written-pitch-or-chord e)
      (setf (slot-value e 'written-pitch-or-chord)
            (make-chord (written-pitch-or-chord e)))))
  e)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Thu May 8 12:58:04 2014 -- do (set-sc-config 'asco-msg-rest-warning
;;; nil) if you're getting unnecessary warnings
(defmethod add-antescofo-message ((e event) receiver message)
  (push (format nil "~a ~a" receiver message) (asco-msgs e)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Tue May 13 09:30:58 2014 -- 
(defmethod event-equal ((e1 event) (e2 event))
  (and (rhythm-equal e1 e2)
       (equal-within-tolerance (start-time e1) (start-time e2))
       (equal-within-tolerance (end-time e1) (end-time e2))
       (= (bar-num e1) (bar-num e2))
       (equalp (marks-before e1) (marks-before e2))
       (equalp (player e1) (player e2))
       (= (amplitude e1) (amplitude e2))
       (or (and (not (pitch-or-chord e1)) (not (pitch-or-chord e2)))
           (pitch= (pitch-or-chord e1) (pitch-or-chord e2)))
       (or (and (not (written-pitch-or-chord e1))
                (not (written-pitch-or-chord e2)))
           (pitch= (written-pitch-or-chord e1) (written-pitch-or-chord e2)))
       (= (8va e1) (8va e2))
       (equal-within-tolerance (duration-in-tempo e1) (duration-in-tempo e2))
       (equal-within-tolerance (compound-duration-in-tempo e1) 
                               (compound-duration-in-tempo e2))
       (equalp (midi-time-sig e1) (midi-time-sig e2))
       (equalp (midi-program-changes e1) (midi-program-changes e2))
       (equalp (midi-control-changes e1) (midi-control-changes e2))
       (or (and (not (tempo-change e1)) (not (tempo-change e2)))
           (tempo-equal (tempo-change e1) (tempo-change e2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Fri Oct  2 09:42:28 2015 -- always returns a list. remember sflist is a
;;; cscl. if user-fun is nil we just use get-next--see below.
(defmethod get-sndfiles-from-user-fun ((e event) sflist user-fun)
  (if user-fun
      (if (is-chord e)
          ;; so user-fun should act on the pitch, but it is passed the event
          ;; too, in case that's useful.
          (loop for p in (data (pitch-or-chord e)) collect
               (funcall user-fun sflist p e))
          (list (funcall user-fun sflist (pitch-or-chord e) e)))
      ;; in the default case, with no user-fun, we cycle through the list, but
      ;; the same sndfile is used for all the notes in a chord
      (let ((sf (get-next sflist))
            (len (if (is-chord e)
                     (sclist-length (pitch-or-chord e))
                     1)))
        (loop repeat len collect sf))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Mon Jan 30 18:54:56 2017 
;;; ****m* event/microtonal
;;; DATE
;;; January 30th 2017 
;;; 
;;; DESCRIPTION
;;; Return numerical information about the microtonality of an event,
;;; essentially how many microtonal pitches there are.
;;; 
;;; ARGUMENTS
;;; - the event object
;;; 
;;; OPTIONAL ARGUMENTS
;;; T or NIL to determine whether numbers should always be returned (i.e. zeros
;;; when there are no microtonal pitches or any pitches at all in the case of
;;; rests) 
;;; 
;;; RETURN VALUE
;;; NIL if the event is a rest (or there are no microtones and <force-number>
;;; is nil), otherwise two values: the number of microtonal pitches in the
;;; event (0, 1, or more if it's a chord), the number of pitches in the event
;;; (2+ for chords otherwise 0 for a rest and 1 for a non-chord).
;;; 
;;; EXAMPLE
#|
(microtonal (make-event 'cs4 'q))
NIL

(microtonal (make-event 'cs4 'q) t)
0
1

(microtonal (make-event 'cqs4 'q) t)
1
1

(microtonal (make-event 'cqs4 'q))
1
1

(microtonal (make-event '(cqs4 e4 g4) 'q))
1
3

(microtonal (make-event nil 'q))
NIL

(microtonal (make-event nil 'q) t)
0
1
|#
;;; SYNOPSIS
(defmethod microtonal ((e event) &optional force-number)
;;; ****
  (let ((count 0)
        (num-pitches 1))
    (unless (is-rest e)
      (if (is-chord e)
          (progn (loop for p in (data (pitch-or-chord e)) do
                      (when (micro-tone p) (incf count)))
                 (setq num-pitches (sclist-length (pitch-or-chord e))))
          (when (micro-tone (pitch-or-chord e))
            (setq count 1))))
    (if (and (zerop count) (not force-number))
        nil
        (values count num-pitches))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* event/round-to-nearest
;;; DESCRIPTION
;;; Round the written and sounding pitch/chord objects to the nearest in the
;;; current or given scale. 
;;; 
;;; ARGUMENTS
;;; the event object
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword argument:
;;; :scale. The scale to use when rounding. (Common Music tuning object or
;;; symbol). If a symbol, then 'chromatic-scale, 'twelfth-tone, or 'quarter-tone
;;; only at present. Default is the current scale as set by (in-scale :...).
;;; 
;;; RETURN VALUE
;;; the modified event object
;;; 
;;; SYNOPSIS
(defmethod round-to-nearest ((e event) &key (scale cm::*scale*))
;;; ****
  (when (pitch-or-chord e)
    (round-to-nearest (pitch-or-chord e) :scale scale))
  (when (written-pitch-or-chord e)
    (round-to-nearest (written-pitch-or-chord e) :scale scale))
  e)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* event/num-notes
;;; DATE
;;; November 6th 2018, Heidhausen
;;; 
;;; DESCRIPTION
;;; Return the number of notes in the event object i.e. the number of notes in
;;; the chord, 1 if it's a single pitch or 0 if it's a rest. 
;;; 
;;; ARGUMENTS
;;; - the event object
;;; 
;;; RETURN VALUE
;;; An integer value for the number of notes in the event.
;;; 
;;; SYNOPSIS
(defmethod num-notes ((e event))
;;; ****
  (cond ((is-rest e) 0)
        ((is-single-pitch e) 1)
        (t (sclist-length (pitch-or-chord e)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; if combo is a list of player IDS then we need the (optional)
;;; slippery-chicken object, otherwise it should be a list of instruments. See
;;; chord method for what it returns.
(defmethod combo-chord-possible? ((e event) combo
                                  &optional artificial-harmonics sc chords)
  (let ((instruments combo))
    (cond ((is-rest e)
           (error "slippery-chicken::combo-chord-possible: event must be a ~
                   chord: ~a" e))
          ;; allow single pitches
          ((is-single-pitch e) (force-chord (setq e (clone e)))))
    (unless (every #'instrument-p instruments)
      (unless (slippery-chicken-p sc)
        (error "event::combo-chord-possible?: combo is ~
                either a list of instrument objects or a ~
                list of player IDs. If IDs then we need ~
                a slippery-chicken object as third argument."))
      ;; this handles instrument changes and means we only search for the
      ;; current instrument once per player instead of potentially hundreds of
      ;; times when trying the combos in the chord class method
      (setq instruments (get-instruments-for-players-at-bar
                         sc combo (bar-num e))))
    ;; (print instruments)
    ;; (print (get-pitch-symbols (pitch-or-chord e)))
    (combo-chord-possible? (pitch-or-chord e) instruments
                           artificial-harmonics chords)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* event/common-notes
;;; DESCRIPTION
;;; Return the number of pitches common to two events. Note that the chord
;;; method returns more information, including the common pitches and pitch
;;; symbols. 
;;; 
;;; ARGUMENTS
;;; - first event object
;;; - second event object
;;; 
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to treat enharmonic equivalents as equal
;;; - T or NIL to treat the same pitches in different octaves as equal
;;; 
;;; RETURN VALUE
;;; the number of common pitches (integer). 2nd (and 3rd) returned values will
;;; be the list of common pitch(es), also as a 3rd value: a list of symbols if
;;; both events are chords).
;;; 
;;; SYNOPSIS
(defmethod common-notes ((e1 event) (e2 event)
                         &optional (enharmonics-are-equal t)
                           (octaves-are-true nil))
;;; ****
  (if (or (is-rest e1) (is-rest e2))
      0
      (let ((poc1 (pitch-or-chord e1))
            (poc2 (pitch-or-chord e2)))
        (cond ((and (is-chord e1) (is-chord e2))
               (common-notes poc1 poc2 enharmonics-are-equal octaves-are-true))
              ((and (is-single-pitch e1) (is-single-pitch e2))
               (let ((p= (pitch= poc1 poc2 enharmonics-are-equal)))
                 (if p=
                     (values 1 (list poc1))
                     (if octaves-are-true
                         (if (is-octave poc1 poc2) (values 1 (list poc1)) 0)
                         0))))
              ((is-single-pitch e1)
               (if (contains-pitches poc2 (list poc1)
                                     enharmonics-are-equal octaves-are-true)
                   (values 1 (list poc1)) 0))
              ((is-single-pitch e2)
               (if (contains-pitches poc1 (list poc2)
                                     enharmonics-are-equal octaves-are-true)
                   (values 1 (list poc2)) 0))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* event/midi-control-change
;;; DATE
;;; August 21st 2019, Heidhausen
;;; 
;;; DESCRIPTION
;;; Add a MIDI control change to the midi-control-changes list slot.
;;; 
;;; ARGUMENTS
;;; - the event
;;; - the MIDI channel (integer)
;;; - the MIDI channel (integer)
;;; - the control change value (integer)
;;; 
;;; EXAMPLE
#|
;;; sustain pedal down
(midi-control-change (make-event 'c4 'q) 1 64 127)
;;; sustain pedal half down
(midi-control-change (make-event 'c4 'q) 1 64 63)
;;; sustain pedal up
(midi-control-change (make-event 'c4 'q) 1 64 0)
;;; una corda
(midi-control-change (make-event 'c4 'q) 1 67 127)
;;; tre corde
(midi-control-change (make-event 'c4 'q) 1 67 0)
;;; sost. (middle) pedal down
(midi-control-change (make-event 'c4 'q) 1 66 127)
|#
;;; RETURN VALUE
;;; The list of control-changes, including the new ones
;;; 
;;; SYNOPSIS
(defmethod midi-control-change ((e event) channel controller-number value)
;;; ****
  (push (list channel controller-number value) (midi-control-changes e)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* event/pitch-or-chord=
;;; AUTHOR
;;; Daniel Ross (mr.danielross[at]gmail[dot]com) 
;;; 
;;; DATE
;;; Tue 29 Oct 2019 08:59:59 GMT - Warwick
;;; 
;;; DESCRIPTION
;;; Convenience method, test to see if the pitch-or-chord slots of two event
;;; objects are the same.
;;; 
;;; ARGUMENTS
;;; - a pitch or chord object
;;; - a pitch or chord object
;;; 
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to indicate whether or not enharmonic pitches are considered 
;;;   equal. T = enharmonic pitches are considered equal. Default = NIL. 
;;; - a number to indicate the frequency deviation allowed before returning NIL.
;;; 
;;; RETURN VALUE
;;; T if the values of the two specified pitch or chord objects are equal, otherwise
;;; NIL. 
;;; 
;;; EXAMPLE
#|
;; Comparison of equal pitch objects created using note-name symbols returns T 
(let ((p1 (make-pitch 'C4))
      (p2 (make-pitch 'C4)))
  (pitch-or-chord= p1 p2))

=> T 

(let ((p1 (make-pitch 'c4))
      (p2 (make-pitch 'bs3)))
  (pitch-or-chord= p1 p2))

=> NIL

(let ((p1 (make-pitch 'c4))
      (p2 (make-pitch 'bs3)))
  (pitch-or-chord= p1 p2 t))

=> T

(let ((p1 (make-pitch 'c4))
      (c1 (make-chord '(c4 e4 g4))))
  (pitch-or-chord= p1 c1))

=> NIL

(let ((c1 (make-chord '(c4 e4 g4)))
      (c2 (make-chord '(c4 e4))))
  (pitch-or-chord= c1 c2))

=> NIL

(let ((c1 (make-chord '(c4 e4 g4)))
      (c2 (make-chord '(bs4 ff4 g4))))
  (pitch-or-chord= c1 c2))

=> NIL

(let ((c1 (make-chord '(c4 e4 g4)))
      (c2 (make-chord '(bs3 ff4 g4))))
  (pitch-or-chord= c1 c2 t))

=> T

|#
;;; SYNOPSIS
(defmethod pitch-or-chord= ((e1 event) (e2 event)
                            &optional enharmonics-are-equal
                              (frequency-tolerance 0.01))
;;; ****
  (pitch-or-chord=-aux (pitch-or-chord e1) (pitch-or-chord e2)
                       enharmonics-are-equal frequency-tolerance))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* event/single-pitch-chord-to-pitch
;;; AUTHOR
;;; Daniel Ross (mr.danielross[at]gmail[dot]com) 
;;; 
;;; DATE
;;; Thu 14 Nov 2019 07:44:02 GMT London
;;; 
;;; DESCRIPTION
;;; Turn a chord object with only one pitch in its chord slot into a pitch
;;; object.
;;; 
;;; ARGUMENTS
;;; An event object
;;; 
;;; OPTIONAL ARGUMENTS
;;; NIL
;;; 
;;; RETURN VALUE
;;; An event object containing a new pitch object, if the original chord
;;; contained a single pitch, or the original event object. 
;;; 
;;; EXAMPLE
#|
(data (single-pitch-chord-to-pitch (make-event '(a4) 'e)))
=> A4
|#
;;; SYNOPSIS
(defmethod single-pitch-chord-to-pitch ((e event))
;;; ****
  (when (is-chord e)
    (when (= (length (data (pitch-or-chord e))) 1)
          (setf (pitch-or-chord e) (first (data (pitch-or-chord e))))))
  e)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* event/add-midi-program-changes
;;; DATE
;;; 8th October 2020, Heidhausen
;;; 
;;; DESCRIPTION
;;; Add programme change data to an event.
;;; 
;;; ARGUMENTS
;;; - the event object to add to
;;; - the MIDI channel to add the program change to. This can either by a single
;;;   integer or a list of (usually 2) integers (for microtonal channels)
;;; - the program change. If a number, just use this directly. If a symbol, get
;;;   the program change for the instrument associated with this ID in the given
;;;   instrument-palette
;;; 
;;; OPTIONAL ARGUMENTS
;;; - the instrument-palette object to get the program change numbers from for
;;;   the given instrument if a symbol is passed as the 3rd argument. Default: 
;;;   +slippery-chicken-standard-instrument-palette+
;;; 
;;; RETURN VALUE
;;; 
;;; 
;;; EXAMPLE
#|

|#
;;; SYNOPSIS
(defmethod add-midi-program-changes
    ((e event) chan pc
     &optional (ins-palette +slippery-chicken-standard-instrument-palette+))
;;; ****
  (let ((change (typecase pc
                  (integer pc)
                  (symbol (let ((ins (get-data pc ins-palette)))
                            (when ins (midi-program ins))))
                  (t (error "event::add-midi-program-changes: ~a is invalid"
                            pc))))
        (chans (force-list chan)))
    (loop for c in chans do
         (push (list c change) (midi-program-changes e))))
  (midi-program-changes e))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Related functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The SC orchestrate method needs to change combos fairly regularly and it
;;; needs a function to do this. This is the default. It uses an activity-levels
;;; object to decide, using <levels> of between 2 and 8 (or the optional args)
;;; according to the size of the ensemble. If either argument is NIL then we
;;; just reset the activity-levels object and return. <combo> is the current
;;; combo which we can decide to keep or change. <combo-al> is the assoc-list
;;; object with all the combos stored and organised according to size.
(let ((al (make-al 1))) ;; <== 1 tends not to change first of all
  (defun combo-change? (combo combo-al &optional (min-level 2) (max-level 8))
    (if (and combo combo-al)
        (let* ((lenc (length combo))
               (level (constrain-int lenc min-level max-level)))
          (unless (get-data lenc combo-al)
            (error "slippery-chicken-edit::combo-change? no ~a-player combos ~
                    in ~%~a" lenc combo-al))
          (active al level))
        (reset al 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun is-hairpin (sym)
  (member sym '(cresc-beg dim-beg cresc-end dim-end)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; code from "snow shoes..." days.  Called from the old clm methods.

(defun make-event-classic (pitch-or-chord start-time duration)
  (let ((result (make-instance 'event :data pitch-or-chord
                               :start-time start-time)))
    (setf (duration result) duration)
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Thu Dec 22 17:53:00 EST 2011: Minor layout edits to MDE info 

;;; ****f* event/make-event
;;; DESCRIPTION
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
;;;   (i.e. it won't sound independently). Default = NIL.
;;; - :duration. T or NIL to indicate whether the specified duration of the
;;;   event has been stated in absolute seconds, not a known rhythm like
;;;   'e. Thus (make-event 'c4 4 :duration nil) indicates a quarter note with
;;;   duration 1, but (make-event '(c4 d4) 4 :duration t) indicates a whole
;;;   note with an absolute duration of 4 seconds (both assuming a tempo of
;;;   60). Default = NIL.
;;; - :amplitude sets the amplitude of the event. Possible values span from 0.0
;;;   (silent) to maximum of 1.0. Default = (get-sc-config 'default-amplitude).
;;; - :tempo. A number to indicate the tempo of the event as a normal bpm
;;;   value. Default = 60. This argument is only used when creating the rhythm
;;;   from a given duration rather than from a letter value.
;;; - :tempo-change. A number or tempo-object to attach a tempo change to the
;;;   event. This is the argument you need to set in order to change playback
;;;   speed. 
;;; - :midi-channel. A number from 0 to 127 indicating the MIDI channel on
;;;   which the event should be played back. Default = NIL.
;;; - :microtones-midi-channel. If the event is microtonal, this argument
;;;   indicates the MIDI-channel to be used for the playback of the microtonal
;;;   notes. Default = NIL. NB: See player.lsp/make-player for details on
;;;   microtones in MIDI output. 
;;; - :transposition. A number in semitones that indicates the transposition of
;;;   the instrument that this event is being created for.  E.g. -2 would be
;;;   for a B-flat clarinet.
;;; - :written.  The given pitch or chord is the written value.  In this case
;;;   the sounding value will be set according to the (required) transposition
;;;   argument.  Default = NIL.
;;; - :bar-num. Set the bar-num slot of the event. Usually maintained by various
;;;   classes that contain events. Default = -1. 
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
(defun make-event (pitch-or-chord rthm
                   &key 
                     start-time
                     is-rest
                     is-tied-to
                     ;; MDE Fri Jan  5 10:38:22 2018 -- added
                     is-tied-from
                     duration
                     ;; MDE Wed Jun 15 20:39:05 2016 -- defaults to 1 and 2
                     (midi-channel 1)
                     (microtones-midi-channel 2)
                     ;; MDE Thu May 31 19:03:59 2012 -- allow us to auto-set the
                     ;; written-pitch-or-chord slot   
                     transposition
                     ;; MDE Sat Apr 20 15:13:41 2013 -- allow us to create
                     ;; written pitch events and auto-set sounding
                     written
                     (amplitude (get-sc-config 'default-amplitude))
                     ;; MDE Thu Jun 16 13:05:21 2016
                     tempo-change
                     ;; MDE Fri Nov  2 16:37:05 2018
                     (bar-num -1)
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
                         :is-tied-from is-tied-from
                         :duration duration :tempo tempo))
         (e (when r (clone-with-new-class r 'event))))
    (when e
      (setf (start-time e) start-time
            (pitch-or-chord e) pitch-or-chord
            ;; MDE Fri Nov  2 16:37:25 2018
            (bar-num e) bar-num
            ;; 24.3.11 if we directly setf amp then we add a mark
            (slot-value e 'amplitude) amplitude)
      ;; MDE Sat Apr 20 15:16:22 2013
      (when (and written (not transposition))
        (error "~a~&event::make-event: need a :transposition when :written T."
               e))
      ;; MDE Thu May 31 19:05:25 2012 
      (when (numberp transposition)
        (when written
          (setf (slot-value e 'pitch-or-chord)
                (transpose (pitch-or-chord e) transposition)))
        (set-written e (- transposition)))
      (when midi-channel
        (set-midi-channel e midi-channel microtones-midi-channel))
      ;; MDE Thu Jun 16 13:06:24 2016 
      (when tempo-change
        (setf (tempo-change e) tempo-change))
      e)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Thu Dec 22 20:53:16 EST 2011 SAR: Added robodoc info

;;; ****f* event/make-rest
;;; DESCRIPTION
;;; Create an event object that consists of a rest.
;;; 
;;; ARGUMENTS
;;; - A rhythm (duration).
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :start-time. A number that is the start-time of the event in seconds.
;;; - :duration. T or NIL. T indicates that the duration given is a value of
;;;   absolute seconds rather than a known rhythm (e.g. 'e). Default = NIL.
;;; - :tempo. Beats per minute. Note that this slot is used for duration
;;;   calculations. If you're making events 'by hand' and you want a new tempo to
;;;   be written to MIDI files you'll need to set the tempo-change slot with
;;;   either a number or tempo object. If you then want the tempo to be written
;;;   to the score, set display-tempo to T. Default = 60.
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

;; Make an event object consisting of 4 seconds of rest (rather than a quarter
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-next-non-grace-note (list-of-events start-index 
                                 &optional 
                                 (warn t))
  (find-next-grace-note list-of-events start-index t warn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* event/make-punctuation-events
;;; DESCRIPTION
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* event/make-events
;;; DESCRIPTION
;;; Make a list of events using the specified data, whereby a list indicates a
;;; note (or chord) and its rhythm and a single datum is the rhythm of a rest.
;;; 
;;; ARGUMENTS
;;; - A list of symbols and/or sublists; see below for examples.
;;;  
;;; OPTIONAL ARGUMENTS
;;; - A whole number indicating the MIDI channel on which the event is to be
;;;   played. 
;;; - A whole number indicating the MIDI channel on which microtonal pitches of
;;;   the event are to be played. NB: See player.lsp/make-player for details on
;;;   microtones in MIDI output. 
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
(defun make-events (data-list &optional (midi-channel 1)
                                (microtones-midi-channel 2))
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
                          (note/chord,rhythm) ~%2-element sublists are ~
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Thu Feb 16 18:03:56 2017 - data list as make-events, tuplets is a
;;; single or list of numbers suitable to be passed to get-tuplet-ratio or
;;; ratios like 5/4 (5 in the time of 4, not a scaler).
;;; this doesn't set the tuplet-scaler or bracket slot of the events but does
;;; set the duration slot correctly
(defun make-tuplet-events (data-list tuplets &optional (midi-channel 1)
                                               (microtones-midi-channel 2))
  (let* ((events (make-events data-list midi-channel microtones-midi-channel))
         (scaler 1))
    (loop for ts in (force-list tuplets) do
         (setq scaler (* scaler (get-tuplet-ratio ts))))
    ;;(print scaler)
    (loop for e in events do
         (setf (tuplet-scaler e) scaler
               (duration e) (* scaler (duration e))))
    events))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* event/make-events2
;;; DESCRIPTION
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
;;;   microtonal pitch material for the event. NB: See player.lsp/make-player
;;;   for details on microtones in MIDI output. 
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
                     &optional (midi-channel 1) (microtones-midi-channel 2))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* event/make-events3
;;; DATE
;;; September 12th 2020, Heidhausen
;;; 
;;; DESCRIPTION
;;; Make a list of events using various approaches. Either with separate lists
;;; of pitches and rhythms make-events and make-events2 will be called according
;;; to the type of arguments given. Note that when specifying pitches, octaves
;;; don't have to be retyped if they don't change.
;;; 
;;; ARGUMENTS
;;; Two required arguments but their type and function vary according to
;;; requirements. See examples below. 
;;; 
;;; OPTIONAL ARGUMENTS
;;; - A whole number value to indicate the MIDI channel on which to play back
;;;   the event.
;;; - A whole number value to indicate the MIDI channel on which to play back
;;;   microtonal pitch material for the event. NB: See player.lsp/make-player
;;;   for details on microtones in MIDI output. 
;;; 
;;; RETURN VALUE
;;; a list of events
;;; 
;;; EXAMPLE
#|
;; simple list of pitch/rhythm pairs but no chords or rests (so no need for list
;; pairs)
(make-events3 '(g4 s a s b 32 c 32) nil)
;; pitch and rhythms in pairs in a single list, including chords and rests
(make-events3 '((g4 q) e s ((d4 fs4 a4) s)) nil)
;; using separate lists for rhythms and pitches, tied rhythms, single pitches, a
;; chord, and both nil and r to indicate rests
(make-events3 '(q e e. h+s 32 q+te) '(cs4 d4 (e4 g4 b5) nil a3 r))
;; just one pitch but various rhythms (but here we can't use tied rhythms) 
(make-events3 '(q e e. s e s s s s) 'cs6)
;; just one rhythm but various pitches/chords and all in octave 4 without having
;; to retype (here we can't use tied rhythms either)
(make-events3 's '(c4 d e (f a) r bf)) ; r = rest here too
|#
;;; SYNOPSIS
(defun make-events3 (data1 data2 &key (midi-channel 1)
                                   (microtones-midi-channel 2))
;;; ****
  ;;    list of rhythms and list of pitches
  (cond ((and (consp data1) (consp data2))
         (make-events2 data1 data2 midi-channel microtones-midi-channel))
        ;; list of rhythms but only one pitch
        ((and (consp data1) data2 (symbolp data2))
         (make-events2 data1 (ml data2 (length data1))
                       midi-channel microtones-midi-channel))
        ;; list of pitches but only one rhythm
        ((and (consp data2) data1 (atom data1))
         (make-events2 (ml data1 (length data2)) data2
                       midi-channel microtones-midi-channel))
        ;; simple list of pitch/rhythm pairs but no chords or rests thus no need
        ;; for 2-element sublists
        ((and (simple-listp data1) (not data2))
         (make-events (split-into-sub-groups2 data1 2)
                      midi-channel microtones-midi-channel))
        ;; list of pitch/rhythm pairs e.g. quarter note, two rests, and a
        ;; chord: '((g4 q) e s ((d4 fs4 a4) s)). if you need rests this is the
        ;; way to go.
        ((and (consp data1) (not data2))
         (make-events data1 midi-channel microtones-midi-channel))
        (t (error "make-snippet: can't make events from ~a and ~a"
                  data1 data2))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* event/event-p
;;; DESCRIPTION
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat Apr 28 22:47:00 BST 2012: Added robodoc entry

;;; ****f* event/sort-event-list
;;; DESCRIPTION
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
=> 
(C4 1.0 E4 2.0 D4 3.0 A4 4.0 F4 5.0 C5 6.0 B4 7.0 G4 8.0)

|#
;;; SYNOPSIS
(defun sort-event-list (event-list)
;;; ****
  (sort event-list
        #'(lambda (x y) 
                       ;; MDE Fri Jan 25 16:54:57 2013 ;
            (unless (and (start-time x) (start-time y))
              (error "event::sort-event-list: missing start time: ~a ~a"
                     x y))
            (< (start-time x) (start-time y)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 23.12.11 SAR Added robodoc info

;;; ****f* event/wrap-events-list
;;; DESCRIPTION
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
;;; Create a list of events of eighth-note durations, specifying start-times at ;
;;; 0.5-second intervals and print the pitches and start-times. Then apply the ;
;;; function and print the pitches and start-times again to see the change. ;
(let ((e-list (loop for st from 1.0 by 0.5
                 for nn in '(c4 d4 e4 f4 g4 a4 b4 c5)
                 collect (make-event nn 'e :start-time st))))
  (print
   (loop for e in e-list
      collect (get-pitch-symbol e)
      collect (start-time e)))
  (wrap-events-list e-list 3)
  (print
   (loop for e in e-list
      collect (get-pitch-symbol e)
      collect (start-time e))))
=>
(C4 1.0 D4 1.5 E4 2.0 F4 2.5 G4 3.0 A4 3.5 B4 4.0 C5 4.5) 
(C4 3.5 D4 4.0 E4 4.5 F4 1.0 G4 1.5 A4 2.0 B4 2.5 C5 3.0)

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
                          ;; longer than the intended rhythm
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Mon Jun 13 12:22:04 2016 -- if an event has a dynamic mark use that as
;;; the amplitude for all successive events, until we see another dynamic
(defun update-events-amplitudes (event-list)
  (loop
     with current = (get-sc-config 'default-amplitude)
     for event in event-list
     for dynamic = (get-dynamic event)
     do
       (when dynamic
         ;; don't just query the amplitude slot as this is a caretaker method
         ;; that overwrites amplitudes from dynamics which means we can't
         ;; necessarily trust the anplitude slot
         (setq current (dynamic-to-amplitude dynamic)))
       (setf (slot-value event 'amplitude) current)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* event/is-dynamic
;;; DESCRIPTION
;;; Determine whether a specified symbol belongs to the list of predefined
;;; dynamic marks.
;;; 
;;; ARGUMENTS
;;; - A symbol.
;;; 
;;; RETURN VALUE
;;; NIL if the specified mark is not found on the predefined list of possible
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; I like my percussion clef to have middle C as on a treble clef but lilypond
;;; has it as with an alto (I think)
(defun lp-percussion-clef ()
    "\\set Staff.middleCPosition = #-6 \\set Staff.clefGlyph = #\"clefs.percussion\" \\set Staff.clefPosition = #0 ")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Sun Dec 25 08:41:20 2011 
(defun is-clef (mark)
  (let ((clef (typecase mark
                (list (when (and (equalp (first mark) 'clef)
                                 (= 2 (length mark)))
                        (second mark)))
                (symbol mark)
                (t nil))))
    (and clef
         (member clef '(treble treble-8vb bass alto tenor double-treble
                        double-bass percussion soprano baritone)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Sat Nov 14 21:13:18 2015 -- split out from get-lp-data because also
;;; used at start of score in write-lp-data-for all
(defun get-lp-clef (clef)
  (if (eq 'percussion clef)
      (format nil "~a~%" (lp-percussion-clef))
      (format nil "~%\\clef ~a "
              (string-downcase 
               (case clef
                 (treble 'treble)
                 (bass 'bass)
                 (alto 'alto)
                 (tenor 'tenor)
                 (treble-8vb "\"treble_8\"")
                 ;; (percussion 'percussion)
                 (double-treble "\"treble^8\"")
                 (double-bass "\"bass_8\"")
                 (t (error "event::get-lp-data: unknown clef: ~a" clef)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Sat Oct 13 10:23:22 2018
(defun is-notehead (sym)
  (first (member sym '(circled-x x-head triangle wedge square triangle-up
                       improvOn flag-head))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Tue Jun 16 13:47:17 2020, Heidhausen -- abstracted out of get-lp-data
;;; above so that we can also use in the pitch class 
(defun separate-marks-before (marks marks-before)
  ;; 13.4.11 the start of 8va marks need to be before the note, but the
  ;; end comes after the note in order to include it under the bracket 
  (move-elements '(circled-x x-head triangle beg-8va beg-8vb wedge square
                   gliss-map hairpin0 beg-trill-a triangle-up mensural
                   << rgb)
                 marks marks-before
                 ;; MDE Thu Nov 14 11:52:26 2013 -- got to be able to
                 ;; handle marks as lists so #'eq not a sufficient member
                 ;; test
                 #'(lambda (x y) (if (listp y)
                                     (eq x (first y))
                                     (eq x y)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF event.lsp
