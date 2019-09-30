;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* linked-named-object/pitch
;;; NAME 
;;; pitch
;;;
;;; File:             pitch.lsp
;;;
;;; Class Hierarchy:  named-object -> linked-named-object -> pitch
;;;
;;; Version:          1.09
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Implementation of the pitch class for holding pitch
;;;                   information: symbolic representation (eg c4), MIDI note
;;;                   number, frequency, sampling-rate conversion etc.
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    March 18th 2001
;;;
;;; $$ Last modified:  12:17:42 Mon Sep 16 2019 CEST
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

;;; The pitch symbol is stored in the <id> and <data> slots.

(defclass pitch (linked-named-object)
  ;; the closest midi-note (integer, c4 = 60) to this pitch; when micro-
  ;; tones, then this will always be the chromatic tone 1/4 tone lower.
  ((midi-note :accessor midi-note :initarg :midi-note :initform nil)
   ;; the midi pitch-bend (in semitones) to create microtones; accurate to 0.01
   ;; (by design: that's 1 cent), so frequency might not reflect this exactly
   ;; MDE Sat Jan  7 17:12:48 2012 -- NB this is no longer always upwards 
   (pitch-bend :accessor pitch-bend :type float :initform 0.0)
   (degree :accessor degree :type integer :initarg :degree :initform -1)
   (score-note :accessor score-note :initform nil)
   ;; the given id minus the accidental e.g. cs4 = c4, dqf2 = d2
   (white-note :accessor white-note :type symbol :initform nil)
   ;; the number of the note in a white-note octave, i.e. c = 0, d = 1, e = 2
   ;; (in octave -1) .... gs4 = 4 + ((1+ 4) x 7) = 39
   (white-degree :accessor white-degree :initform nil)
   (nearest-chromatic :accessor nearest-chromatic :type symbol :initform nil)
   ;; just the accidental part of the note e.g. s, f or qf etc.
   (accidental :accessor accidental :type symbol :initform nil)
   ;; MDE Sat Jan 7 22:40:26 2012 -- NB if a pitch object is created from a
   ;; frequency (rather than note symbol) then the given frequency is stored
   ;; and the note/midi-note etc. nearest to it will be stored also.  So the
   ;; frequency might not be the exact frequency of the reflected note.  This
   ;; is by design, so that unusual temperaments can retain exact frequencies
   ;; and show nearest notes etc.
   (frequency :accessor frequency :initarg :frequency :initform nil)
   (midi-channel :accessor midi-channel :initarg :midi-channel :initform 0)
   (octave :accessor octave :type integer :initform -1)
   ;; MDE Thu Jun 16 13:35:02 2016 -- remember the last octave we saw so we
   ;; don't have to always type octaves  
   (natural :accessor natural :type boolean :initform nil)
   (qtr-sharp :accessor qtr-sharp :initform nil)
   (sharp :accessor sharp :type boolean :initform nil)
   (qtr-flat :accessor qtr-flat :initform nil)
   (flat :accessor flat :type boolean :initform nil)
   (qtr-tone :accessor qtr-tone :initform nil)
   (micro-tone :accessor micro-tone :type boolean :initform nil)
   (show-accidental :accessor show-accidental :type boolean 
                    :initarg :show-accidental :initform t)
   (accidental-in-parentheses :accessor accidental-in-parentheses :type boolean
                              :initform nil)
   ;; e.g. for changing the note head of an individual note in a chord.
   (marks :accessor marks :type list :initarg :marks :initform nil)
   ;; MDE Thu Nov 14 14:22:33 2013 -- as with the event class, some marks need
   ;; to come before and if this pitch is in a chord, we can't do it via the
   ;; event  
   (marks-before :accessor marks-before :type list :initarg :marks-before
                 :initform nil)
   ;; in the circle of 5ths, how far advanced is this pitch i.e. a natural is
   ;; 0, fs and bf are 1, cs and ef are 2 ... microtones are 0.
   (c5ths :accessor c5ths :type number :initform 0)
   ;; e.g. from fs4 -> fs
   (no-8ve :accessor no-8ve :type symbol :initform nil)
   ;; e.g. from fs4 -> f
   (no-8ve-no-acc :accessor no-8ve-no-acc :type symbol :initform nil)
   (src :accessor src :initarg :src :initform nil)
   ;; MDE Thu Nov 14 11:32:18 2013 -- we don't want to issue too many warnings
   ;; about resolving to nearest 1/4 tone when running Lilypond so limit
   ;; warnings via this slot but keep track of how many we resolved.
   (lp-resolutions :accessor lp-resolutions :type integer :initform 0
                :allocation :class)
   ;; MDE Tue Apr 17 12:44:57 2012 -- not currently used by any method.
   (src-ref-pitch :accessor src-ref-pitch :type symbol :initarg :src-ref-pitch
                  :initform 'c4)
   ;; MDE Wed Nov 28 18:30:39 2018 -- by setting this slot we can override the
   ;; event's amplitude, so as to 'voice' notes in chords with different
   ;; amplitudes.  For MIDI output this would have to be a floating-point number
   ;; 0.0-1.0, or an integer velocity 0-127  
   (amplitude :accessor amplitude :initarg :amplitude :initform nil)
   ;; when frequency is given, we have to update id and vice-versa.  This slot
   ;; tells us whether this was done and so avoids endless back-and-forths when
   ;; calling the setf methods.
   (data-consistent :accessor data-consistent :type boolean :initform nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object :before ((i pitch) stream)
  (format stream "~%PITCH: frequency: ~,3f, midi-note: ~a, midi-channel: ~d ~
                  ~%       pitch-bend: ~a ~
                  ~%       degree: ~a, data-consistent: ~a, white-note: ~a~
                  ~%       nearest-chromatic: ~a~
                  ~%       src: ~a, src-ref-pitch: ~a, score-note: ~a ~
                  ~%       qtr-sharp: ~a, qtr-flat: ~a, qtr-tone: ~a,  ~
                  ~%       micro-tone: ~a, ~
                  ~%       sharp: ~a, flat: ~a, natural: ~a, ~
                  ~%       octave: ~a, c5ths: ~a, no-8ve: ~a, ~
                           no-8ve-no-acc: ~a~
                  ~%       show-accidental: ~a, white-degree: ~a, ~
                  ~%       accidental: ~a, ~
                  ~%       accidental-in-parentheses: ~a, marks: ~a, ~
                  ~%       marks-before: ~a, amplitude: ~a"
          (frequency i) (midi-note i) (midi-channel i) 
          (pitch-bend i)
          (degree i) (data-consistent i) (white-note i)
          (nearest-chromatic i)
          (src i) (src-ref-pitch i) (score-note i)
          (qtr-sharp i) (qtr-flat i) (qtr-tone i) 
          (micro-tone i) 
          (sharp i) (flat i) (natural i) 
          (octave i) (c5ths i) (no-8ve i) (no-8ve-no-acc i)
          (show-accidental i) (white-degree i) 
          (accidental i)
          (accidental-in-parentheses i) (marks i) (marks-before i)
          (amplitude i)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize-instance :after ((p pitch) &rest initargs)
  (declare (ignore initargs))
  (update-pitch p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone ((p pitch))
  (clone-with-new-class p 'pitch))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod copy-secondary-slots ((from pitch) (to pitch))
  (setf (slot-value to 'midi-channel) (midi-channel from)
        (slot-value to 'accidental-in-parentheses) (accidental-in-parentheses
                                                    from)  
        (slot-value to 'show-accidental) (show-accidental from)
        (slot-value to 'marks) (my-copy-list (marks from))
        (slot-value to 'marks-before) (my-copy-list (marks-before from))
        (slot-value to 'src-ref-pitch) (src-ref-pitch from)
        (slot-value to 'data-consistent) (data-consistent from)
        (slot-value to 'amplitude) (amplitude from))
  to)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone-with-new-class :around ((p pitch) new-class)
  (declare (ignore new-class))
  (let ((no (call-next-method)))
    (copy-secondary-slots p no)
    (setf (slot-value no 'midi-note) (midi-note p)
          (slot-value no 'degree) (degree p)
          (slot-value no 'score-note) (basic-copy-object (score-note p))
          (slot-value no 'white-note) (white-note p)
          (slot-value no 'nearest-chromatic) (nearest-chromatic p)
          (slot-value no 'frequency) (frequency p)
          (slot-value no 'pitch-bend) (pitch-bend p)
          (slot-value no 'natural) (natural p)
          (slot-value no 'octave) (octave p)
          (slot-value no 'qtr-sharp) (qtr-sharp p)
          (slot-value no 'sharp) (sharp p)
          (slot-value no 'qtr-flat) (qtr-flat p)
          (slot-value no 'c5ths) (c5ths p)
          (slot-value no 'no-8ve) (no-8ve p)
          (slot-value no 'no-8ve-no-acc) (no-8ve-no-acc p)
          (slot-value no 'flat) (flat p)
          (slot-value no 'qtr-tone) (qtr-tone p)
          (slot-value no 'micro-tone) (micro-tone p)
          (slot-value no 'accidental) (accidental p)
          (slot-value no 'white-degree) (white-degree p)
          (slot-value no 'src) (src p))
    no))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+cm-2
(defmethod output-midi-note ((p pitch) time amplitude duration)
  ;; 14.3.11 can't output without midi channel
  (unless (midi-channel p)
    (error "pitch::output-midi-note: midi-channel nil: ~%~a:" p))
  (cm::output-midi-note (midi-note p) 
                        ;; MDE Tue Jun 26 12:30:09 2012 -- the pitch bend is
                        ;; set for every output MIDI note, just to be sure.
                        ;; This means we can have e.g. 1/8th and 1/4 tones on
                        ;; one channel.  NB When creating events, we put
                        ;; microtones on one channel and chromatic tones on
                        ;; another so we can have microtonal chords.  But we
                        ;; can only have one type of microtone in a chord doing
                        ;; it this way (better than nothing).
                        (pitch-bend p)
                        time
                        (if (amplitude p) (amplitude p) amplitude)
                        duration  
                        ;; 1- because cm channels start at 0
                        (1- (midi-channel p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; just change the micro-tone slot to <value>
(defmethod force-micro-tone ((p pitch) &optional value)
  (setf (micro-tone p) value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod midi-note-float ((p pitch) &optional in-cents)
  (let ((mnf (float (+ (midi-note p) (pitch-bend p)))))
    (if in-cents (round (* 100.0 mnf)) mnf)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Mon Jan  2 11:59:06 EST 2012: Added robodoc info

;;; ****m* pitch/delete-marks
;;; DESCRIPTION
;;; Delete all marks stored in the MARKS slot of the given pitch object and
;;; reset the slot to NIL.
;;; 
;;; ARGUMENTS
;;; - A pitch object.
;;; 
;;; RETURN VALUE
;;; Always returns NIL
;;; 
;;; EXAMPLE
#|
;; Add two marks, then delete them. The method returns NIL
(let ((p (make-pitch 'c4)))
  (add-mark p 'pizz)
  (add-mark p 'a)
  (delete-marks p))

=> NIL

;; Add two marks and print the MARKS slot to see the changes. Then apply the
;; delete-marks method and print the MARKS slot to see the changes.
(let ((p (make-pitch 'c4)))
  (add-mark p 'pizz)
  (add-mark p 'a)
  (print (marks p))
  (delete-marks p)
  (print (marks p)))

=> 
(A PIZZ) 
NIL 

|#
;;; SYNOPSIS
(defmethod delete-marks ((p pitch))
;;; ****
  (setf (marks p) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; the ignore fields are there because of the transpose method in tl-set,
;;; chord and event.  

;;; SAR Mon Jan  2 12:52:40 EST 2012: Added robodoc info

;;; ****m* pitch/transpose
;;; DESCRIPTION
;;; Transpose the pitch information (frequency, note-name, midi-note etc.) of a
;;; given pitch object by a specified number of semitones. The number of
;;; semitones specified can be fractional; however, all fractional values will
;;; be rounded to the nearest quarter-tone frequency.
;;;
;;; NB: This method returns a new pitch object rather than altering the values
;;;     of the current pitch object.
;;; 
;;; ARGUMENTS
;;; - A pitch object.
;;; - A number representing the number of semitones to be transposed, and which
;;;   can be fractional.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :as-symbol. T or NIL to indicate whether the method is to return an
;;;   entire pitch object or just a note-name symbol of the new pitch. NIL = a
;;;   new pitch object. Default = NIL.
;;; - :package. Used to identify a separate Lisp package in which to process
;;;   the result. This is really only applicable is combination with :as-symbol
;;;   set to T. Default = :sc.
;;; 
;;; RETURN VALUE
;;; A pitch object by default.
;;;
;;; If the :as-symbol argument is set to T, then a note-name symbol is returned
;;; instead. 
;;; 
;;; EXAMPLE
#|
;; By default the method returns a pitch object
(let ((p (make-pitch 'c4)))
  (transpose p 2))

=> 
PITCH: frequency: 293.665, midi-note: 62, midi-channel: 0 
       pitch-bend: 0.0 
       degree: 124, data-consistent: T, white-note: D4
       nearest-chromatic: D4
       src: 1.1224620342254639, src-ref-pitch: C4, score-note: D4 
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

;; Setting the :as-symbol keyword argument to T returns just the note-name
;; symbol of the new pitch instead
(let ((p (make-pitch 'c4)))
  (transpose p 2 :as-symbol t))

=> D4

;; The semitones argument can be set to a decimal-point fraction, which may
;; result in quarter-tone pitch values being returned
(let ((p (make-pitch 'c4)))
  (transpose p 2.5))

=> 
PITCH: frequency: 302.270, midi-note: 62, midi-channel: 0 
       pitch-bend: 0.5 
       degree: 125, data-consistent: T, white-note: D4
       nearest-chromatic: D4
       src: 1.1553527116775513, src-ref-pitch: C4, score-note: DS4 
       qtr-sharp: 1, qtr-flat: NIL, qtr-tone: 1,  
       micro-tone: T, 
       sharp: NIL, flat: NIL, natural: NIL, 
       octave: 4, c5ths: 0, no-8ve: DQS, no-8ve-no-acc: D
       show-accidental: T, white-degree: 29, 
       accidental: QS, 
       accidental-in-parentheses: NIL, marks: NIL
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: DQS4, tag: NIL, 
data: DQS4

;; Fractional semitone arguments are automatically rounded to the nearest
;; quarter-tone, causing x.5 and x.7, for example, to return the same result,
;; while x.3 and x.1 will return the same value as the given integer
(let ((p (make-pitch 'c4)))
  (print (transpose p 2 :as-symbol t))
  (print (loop for s from 0 to 4 
            collect (transpose p (+ 2 (* s .1)) :as-symbol t)))
  (print (loop for s from 5 to 9
            collect (transpose p (+ 2 (* s .1)) :as-symbol t))))

=>
D4 
(D4 D4 D4 D4 D4) 
(DQS4 DQS4 DQS4 DQS4 DQS4)

|#
;;; SYNOPSIS
(defmethod transpose ((p pitch) semitones &key (as-symbol nil) (package :sc)
                      ignore)
;;; ****
  (declare (ignore ignore))
  (let ((new-note (transpose-note (data p) semitones))
        new-pitch)
    (unless new-note
      (error "pitch::transpose: Couldn't transpose ~a by ~a semitones" 
             (id p) semitones))
    ;; 13.4.11 make sure we get the right enharmonic (i.e. the same) if
    ;; transposing by octaves 
    (setf new-pitch (make-pitch new-note))
    (when (and (zerop (mod semitones 12))
               (not (eq (no-8ve new-pitch) (no-8ve p))))
      (setf new-pitch (enharmonic new-pitch)))
    (copy-secondary-slots p new-pitch)
    (if as-symbol
        ;; 13.4.11:
        ;; (rm-package new-note package)
        (rm-package (data new-pitch) package)
        new-pitch)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Mon Jan  2 13:44:14 EST 2012: Added robodoc info

;;; ****m* pitch/pitch-round
;;; DESCRIPTION
;;; Rounds the value of a specified pitch object to the nearest chromatic
;;; semitone (non-microtonal MIDI) pitch.
;;; 
;;; ARGUMENTS
;;; - A pitch object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :as-symbol. T or NIL to indicate whether the method is to return an
;;;   entire pitch object or just a note-name symbol of the new pitch. NIL = a
;;;   new pitch object. Default = NIL.
;;; - :package. Used to identify a separate Lisp package in which to process
;;;   result. This is really only applicable is combination with :as-symbol set
;;;   to T. Default = :sc.
;;; 
;;; RETURN VALUE
;;; A pitch object by default.
;;;
;;; If the :as-symbol argument is set to T, then a note-name symbol is returned
;;; instead. 
;;; 
;;; EXAMPLE
#|
;; Returns a pitch object by default; here an example rounding a quarter-tone
;;; note-name symbol to the nearest chromatic pitch
(let ((p (make-pitch 'CQS4)))
  (pitch-round p))

=> 
PITCH: frequency: 261.626, midi-note: 60, midi-channel: 0 
[...]
NAMED-OBJECT: id: C4, tag: NIL, 
data: C4

;; Also rounds frequencies to the nearest chromatic pitch. This example first
;; prints the original values automatically stored with frequency 269.0
;; (rounded by default to the nearest quarter-tone), then the new value rounded
;; to the nearest chromatic semitone
(let ((p (make-pitch 269.0)))
  (print (data p))
  (print (pitch-round p :as-symbol t)))

=>
CQS4 
C4 

|#
;;; SYNOPSIS
(defmethod pitch-round ((p pitch) &key (as-symbol nil) (package :sc))
;;; ****
  ;; we can't use lisp's round function as in the case of x.5 it
  ;; rounds to the nearest even number so (round 1.5) => 2 and 
  ;; (round 2.5) => 2 
  (let ((sym (midi-to-note (cond ((qtr-sharp p) (midi-note p))
                                 ((qtr-flat p) (1+ (midi-note p)))
                                 (t (midi-note-float p)))
                           cm::*chromatic-scale*)))
    (if as-symbol
        (rm-package sym package)
        ;; MDE Sat Sep  8 15:02:28 2018 -- used original pitch's midi-channel
        (copy-secondary-slots p (make-pitch sym)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Tue Aug  7 17:23:44 BST 2012: Added robodoc entry

;;; ****M* pitch/no-accidental
;;; DESCRIPTION
;;; Set the SHOW-ACCIDENTAL and ACCIDENTAL-IN-PARENTHESES slots of a specified
;;; pitch object to NIL, preventing any accidentals or accidentals in
;;; parentheses from being shown for that event in the printed score.
;;; 
;;; NB: This will only be effective if the :respell-notes option for
;;;     cmn-display and write-lp-data-for-all is set to NIL.
;;; 
;;; ARGUMENTS
;;; - A pitch object.
;;; 
;;; RETURN VALUE
;;; Always NIL.
;;; 
;;; EXAMPLE
#|
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((vn (violin :midi-channel 1))))
        :set-palette '((1 ((c4 cs4 fs4))))
        :set-map '((1 (1)))
        :rthm-seq-palette '((1 ((((2 4) - s s s s - - s s s s -))
                                :pitch-seq-palette ((1 2 3 2 1 2 3 2)))))
        :rthm-seq-map '((1 ((vn (1))))))))
  (no-accidental (pitch-or-chord (get-note mini 1 7 'vn)))
  (cmn-display mini :respell-notes nil)
  (write-lp-data-for-all mini :respell-notes nil))

|#
;;; SYNOPSIS
(defmethod no-accidental ((p pitch))
;;; ****
  (setf (show-accidental p) nil
        (accidental-in-parentheses p) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Mon Jan  2 14:03:51 EST 2012: Added robodoc info

;;; ****m* pitch/transpose-to-octave
;;; DESCRIPTION
;;; Transpose the values of a given pitch object to a specified octave.
;;;
;;; NB: This method creates a new pitch object rather than replacing the values
;;; of the original.
;;; 
;;; ARGUMENTS
;;; - A pitch object.
;;; - A number indicating the new octave.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :as-symbol. T or NIL to indicate whether the method is to return an
;;;   entire pitch object or just a note-name symbol of the new pitch. NIL = a
;;;   new pitch object. Default = NIL.
;;; - :package. Used to identify a separate Lisp package in which to process
;;;   the result. This is really only applicable is combination with :as-symbol
;;;   set to T. Default = :sc.
;;;
;;; RETURN VALUE
;;; A pitch object by default.
;;;
;;; If the :as-symbol argument is set to T, then a note-name symbol is returned
;;; instead. 
;;; 
;;; EXAMPLE
#|
;; Transpose the values of a pitch object containing middle-C (octave 4) to the
;;; C of the treble clef (octave 5)
(let ((p (make-pitch 'c4)))
  (transpose-to-octave p 5))

=> 
PITCH: frequency: 523.251, midi-note: 72, midi-channel: 0 
       pitch-bend: 0.0 
       degree: 144, data-consistent: T, white-note: C5
       nearest-chromatic: C5
       src: 2.0, src-ref-pitch: C4, score-note: C5 
       qtr-sharp: NIL, qtr-flat: NIL, qtr-tone: NIL,  
       micro-tone: NIL, 
       sharp: NIL, flat: NIL, natural: T, 
       octave: 5, c5ths: 0, no-8ve: C, no-8ve-no-acc: C
       show-accidental: T, white-degree: 35, 
       accidental: N, 
       accidental-in-parentheses: NIL, marks: NIL
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: C5, tag: NIL, 
data: C5

;; Setting the :as-symbol argument to T returns a note-name symbol instead of a
;; pitch object
(let ((p (make-pitch 'c4)))
  (transpose-to-octave p 5 :as-symbol t))

=> C5

|#
;;; SYNOPSIS
(defmethod transpose-to-octave ((p pitch) new-octave 
                                &key
                                (as-symbol nil)
                                (package :sc))
;;; ****
  ;; MDE Wed Aug 19 10:33:21 2015 -- octave -1 is the lowest, not 0
  (unless (and (integerp new-octave) (> new-octave -2)) ;(integer>=0 new-octave)
    (error "pitch::transpose-to-octave: octave must be an integer >= -1: ~a"
           new-octave))
  (let ((transp (* 12 (- new-octave (octave p)))))
    (transpose p transp :as-symbol as-symbol :package package)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Mon Jan  2 14:14:56 EST 2012: Added robodoc info

;;; ****m* pitch/pitch=
;;; DESCRIPTION
;;; Determines if the note-name and chromatic semitone MIDI values of two
;;; specified pitch objects are the same (or very close to each other in the
;;; case of frequency and src slot comparison).  
;;;
;;; By default, this method returns NIL when comparing enharmonic pitches. This
;;; can behavior can be changed by setting the optional argument to T, upon
;;; which enharmonic pitches are considered equal.
;;;
;;; NB: This method may return NIL when comparing pitch objects created using
;;;     frequencies with those created using note-names. The method
;;;     pitch::note= may be more useful in this case.
;;; 
;;; NB: Pitch objects created using frequencies are only considered equal if
;;;     their frequency values are within 0.01Hz of each other.
;;; 
;;; ARGUMENTS
;;; - A first pitch object.
;;; - A second pitch object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to indicate whether or not enharmonic pitches are considered 
;;;   equal. T = enharmonic pitches are considered equal. Default = NIL. 
;;; - a number to indicate the frequency deviation allowed before returning NIL.
;;; 
;;; RETURN VALUE
;;; T if the values of the two specified pitch objects are equal, otherwise
;;; NIL. 
;;; 
;;; EXAMPLE
#|
;; Comparison of equal pitch objects created using note-name symbols returns T 
(let ((p1 (make-pitch 'C4))
      (p2 (make-pitch 'C4)))
  (pitch= p1 p2))

=> T 

;; Comparison of unequal pitch objects created using note-name symbols returns
NIL 
(let ((p1 (make-pitch 'C4))
      (p2 (make-pitch 'D4)))
  (pitch= p1 p2))

=> NIL

;; Comparison of enharmonically equivalent pitch objects returns NIL by default 
;; Comparison of equal pitch objects created using note-name symbols returns T 
(let ((p1 (make-pitch 'CS4))
      (p2 (make-pitch 'DF4)))
  (pitch= p1 p2))

=> NIL

;; Comparison of enharmonically equivalent pitch objects return T when the
;; optional argument is set to T
;; Comparison of equal pitch objects created using note-name symbols returns T 
(let ((p1 (make-pitch 'C4))
      (p2 (make-pitch 'C4)))
  (pitch= p1 p2 t))

=> T

;; Comparison of pitch objects created using frequencies with those created
;; using note-name symbols return NIL
(let ((p1 (make-pitch 'C4))
      (p2 (make-pitch 261.63)))
  (pitch= p1 p2))

=> NIL

|#
;;; SYNOPSIS
(defmethod pitch= ((p1 pitch) (p2 pitch) &optional enharmonics-are-equal
                   (frequency-tolerance 0.01)) ; (src-tolerance 0.0001))
;;; ****
  ;; (print p1) (print p2)
  (and (equal-within-tolerance (frequency p1) (frequency p2)
                               frequency-tolerance)
       (or enharmonics-are-equal (eq (data p1) (data p2)))
       (= (midi-note p1) (midi-note p2))))
       ;; MDE Wed Dec 25 12:30:26 2013 -- src slots are not necessary/useful
       ;; for pitch comparison (used to be in the above 'and' clause)
       ;; (equal-within-tolerance (src p1) (src p2) src-tolerance)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* pitch/enharmonic-equivalents
;;; DATE
;;; 25th December 2013
;;;
;;; DESCRIPTION
;;; Test whether two pitches are enharmonically equivalent.
;;; 
;;; ARGUMENTS
;;; - pitch object 1 
;;; - pitch object 2
;;; 
;;; RETURN VALUE
;;; T or NIL
;;; 
;;; EXAMPLE
#|
(enharmonic-equivalents (make-pitch 'gs4) (make-pitch 'af4))
=> T
(enharmonic-equivalents (make-pitch 'gs4) (make-pitch 'gs4))
=> NIL
|#
;;; SYNOPSIS
(defmethod enharmonic-equivalents ((p1 pitch) (p2 pitch))
;;; ****
  (pitch= p1 (enharmonic p2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* pitch/cents-hertz
;;; DATE
;;; December 24th 2013
;;; 
;;; DESCRIPTION
;;; Convert an offset in cents into the frequency deviation of a pitch.
;;; 
;;; ARGUMENTS
;;; - a pitch object
;;; - the number of cents to offset the pitch and return the frequency
;;;   deviation for  
;;; 
;;; RETURN VALUE
;;; The frequency deviation in Hertz of the offset pitch.
;;; 
;;; EXAMPLE
#|
;;; taking as a given the usual floating point round errors:
(cents-hertz (make-pitch 'a4) 1200) 
=> 439.9999694824219d0 ; i.e. going up an octave from a4 would be 440 Hz higher
(cents-hertz (make-pitch 'a4) -1200)
=> -219.99998474121094d0
(cents-hertz (make-pitch 'a4) 3)
=> 0.7631253666877456d0
(cents-hertz (make-pitch 'a4) 10)
=> 2.5489090105293144d0
(cents-hertz (make-pitch 'a3) 10)
=> 1.2744545052646572d0
|#
;;; SYNOPSIS
(defmethod cents-hertz ((p pitch) cents)
;;; ****
  (- (* (frequency p) (semitones (* .01 cents)))
     (frequency p) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Mon Jan  2 17:05:31 EST 2012: Added robodoc info

;;; ****m* pitch/pitch-class-eq
;;; DATE 
;;; 14 Aug 2010
;;; 
;;; DESCRIPTION
;;; Test whether the values of two pitch objects are of the same pitch class,
;;; i.e. both Cs, or F#s,irrespective of octave. 
;;; 
;;; ARGUMENTS 
;;; - A first pitch object.
;;; - A second pitch object.
;;;
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to indicate whether or not enharmonic pitches are considered 
;;;   equal. T = enharmonic pitches are considered equal. Default = NIL. 
;;; 
;;; RETURN VALUE  
;;; T if the values of the two pitch objects are of the same pitch class,
;;; otherwise NIL.
;;; 
;;; EXAMPLE
#|
;; A comparison of two pitch objects with values of the same pitch class
;;; returns T
(let ((p1 (make-pitch 'c4))
      (p2 (make-pitch 'c5)))
  (pitch-class-eq p1 p2))

=> T

;; A comparison of two pitch objects with values of differing pitch classes
;; returns NIL
(let ((p1 (make-pitch 'c4))
      (p2 (make-pitch 'cs5)))
  (pitch-class-eq p1 p2))

=> NIL

;; A comparison of two pitch objects with enharmonically equivalent pitch
;; classes returns NIL by default
(let ((p1 (make-pitch 'c4))
      (p2 (make-pitch 'bs4)))
  (pitch-class-eq p1 p2))

=> NIL

;; Setting the optional argument to T causes the method to consider
;; enharmonically equivalent pitch classes equal
(let ((p1 (make-pitch 'c4))
      (p2 (make-pitch 'bs4)))
  (pitch-class-eq p1 p2 t))

=> T

|#
;;; SYNOPSIS
(defmethod pitch-class-eq ((p1 pitch) (p2 pitch)
                           &optional enharmonics-are-equal)
;;; ****
  (or (eq (no-8ve p1) (no-8ve p2))
      (and enharmonics-are-equal
           (eq (no-8ve p1) (no-8ve (enharmonic p2))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Mon Jan  2 18:23:01 EST 2012: Added robodoc info

;;; ****m* pitch/note=
;;; DESCRIPTION
;;; Tests to see the note-name symbols (values in the DATA slots) of two given
;;; pitch objects are equal. 
;;;
;;; NB: This method allows for the comparison of pitch objects created using
;;;     frequency numbers and those created using note-name symbols.
;;; 
;;; ARGUMENTS
;;; - A first pitch object.
;;; - A second pitch object.
;;; 
;;; RETURN VALUE
;;; T if the note-name symbols of the given pitch objects are equal, otherwise
;;; NIL.  
;;; 
;;; EXAMPLE
#|
;; Two pitch objects with equal note-name symbols return T
(let ((p1 (make-pitch 'c4))
      (p2 (make-pitch 'c4)))
  (note= p1 p2))

=> T
;; Two pitch objects with unequal note-name symbols return F
(let ((p1 (make-pitch 'c4))
      (p2 (make-pitch 'd4)))
  (note= p1 p2))

=> NIL

;; Pitch objects created using frequency numbers and those created using
;; note-name symbols can be effectively compared using this method
(let ((p1 (make-pitch 'c4))
      (p2 (make-pitch 261.63)))
  (note= p1 p2))

=> T

|#
;;; SYNOPSIS
(defmethod note= ((p1 pitch) (p2 pitch) &optional ignore)
;;; ****
  (declare (ignore ignore))
  (equalp (data p1) (data p2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Mon Jan  2 18:33:30 EST 2012: Added robodoc info

;;; ****m* pitch/pitch<
;;; DESCRIPTION
;;; Test to see if the frequency value of one specified pitch object is less
;;; than that of a second.
;;;
;;; NB: Due to the fact that a given note-name may encompass several
;;;     fractionally different frequencies (e.g. both 261.626 and 261.627 are
;;;     both considered to be C4), this method is not suitable for comparing
;;;     pitch objects of which one was created using a frequency and the other
;;;     was created using a note-name symbol.
;;; 
;;; ARGUMENTS
;;; - A pitch object.
;;; - A second pitch object.
;;; 
;;; RETURN VALUE
;;; Returns T if the frequency value of the first pitch object is less than
;;; that of the second, otherwise NIL.
;;; 
;;; EXAMPLE
#|
;; T is returned when the frequency of the first pitch is less than that of
;; the second
(let ((p1 (make-pitch 'c4))
      (p2 (make-pitch 'd4)))
  (pitch< p1 p2))

=> T

;; NIL is returned when the frequency of the first pitch is not less than
;; that of the second
(let ((p1 (make-pitch 'c4))
      (p2 (make-pitch 'd4)))
  (pitch< p2 p1))

=> NIL

;; Equivalent pitches return NIL
(let ((p1 (make-pitch 'c4))
      (p2 (make-pitch 'c4)))
  (pitch< p2 p1))

=> NIL

;; This method can be effectively used to compare the frequency values of two
;; pitch objects that were both created using frequency numbers
(let ((p1 (make-pitch 261.63))
      (p2 (make-pitch 293.66)))
  (pitch< p1 p2))

=> T  

;; Due to sc's numerical accuracy, this method is not suitable for comparing
;; pitch objects of which one was created using a note-name symbol and the
;; other was created using a numerical frequency value. Such comparisons may
;; return misleading results.
(let ((p1 (make-pitch 'c4))
      (p2 (make-pitch 261.63)))
  (pitch< p1 p2))

=> T

|#
;;; SYNOPSIS
(defmethod pitch< ((p1 pitch) (p2 pitch))
;;; ****
  (< (frequency p1) (frequency p2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Mon Jan  2 20:21:17 EST 2012: Added robodoc info

;;; ****m* pitch/pitch>
;;; DESCRIPTION
;;; Test to see if the frequency value of one specified pitch object is greater
;;; than that of a second.
;;;
;;; NB: Due to the fact that a given note-name may encompass several
;;; fractionally different frequencies (e.g. both 261.626 and 261.627 are both
;;; considered to be C4), this method is not suitable for comparing pitch
;;; objects of which one was created using a frequency and the other was
;;; created using a note-name symbol.
;;; 
;;; ARGUMENTS
;;; - A pitch object.
;;; - A second pitch object.
;;; 
;;; RETURN VALUE
;;; Returns T if the frequency value of the first pitch object is greater than
;;; that of the second, otherwise NIL.
;;; 
;;; EXAMPLE
#|
;; T is returned when the frequency of the first pitch is greater than that of
;; the second
(let ((p1 (make-pitch 'd4))
      (p2 (make-pitch 'c4)))
  (pitch> p1 p2))

=> T

;; NIL is returned when the frequency of the first pitch is not greater than
;; that of the second
(let ((p1 (make-pitch 'd4))
      (p2 (make-pitch 'c4)))
  (pitch> p2 p1))

=> NIL

;; Equivalent pitches return NIL
(let ((p1 (make-pitch 'd4))
      (p2 (make-pitch 'd4)))
  (pitch> p2 p1))

=> NIL

;; This method can be effectively used to compare the frequency values of two
;; pitch objects that were both created using frequency numbers
(let ((p1 (make-pitch 293.66))
      (p2  (make-pitch 261.63)))
  (pitch> p1 p2))

=> T  

;; Due to sc's numerical accuracy, this method is not suitable for comparing
;; pitch objects of which one was created using a note-name symbol and the
;; other was created using a numerical frequency value. Such comparisons may
;; return misleading results.
(let ((p1 (make-pitch 261.63))
      (p2 (make-pitch 'c4)))
  (pitch> p1 p2))

=> T

|#

;;; SYNOPSIS
(defmethod pitch> ((p1 pitch) (p2 pitch))
;;; ****
  (> (frequency p1) (frequency p2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Mon Jan  2 20:30:57 EST 2012: Added robodoc info

;;; ****m* pitch/pitch<=
;;; DESCRIPTION
;;; Test to see if the frequency value of one specified pitch object is less
;;; than or equal to than that of a second.
;;;
;;; NB: Due to the fact that a given note-name may encompass several
;;;     fractionally different frequencies (e.g. both 261.626 and 261.627 are
;;;     both considered to be C4), this method is not suitable for comparing
;;;     pitch objects of which one was created using a frequency and the other
;;;     was created using a note-name symbol.
;;; 
;;; ARGUMENTS
;;; - A pitch object.
;;; - A second pitch object.
;;; 
;;; RETURN VALUE
;;; Returns T if the frequency value of the first pitch object is less than or
;;; equal to that of the second, otherwise NIL.
;;; 
;;; EXAMPLE
#|
;; T is returned when the frequency of the first pitch is less than or equal to
;; that of the second
(let ((p1 (make-pitch 'c4))
      (p2 (make-pitch 'd4)))
  (pitch<= p1 p2))

=> T

;; NIL is returned when the frequency of the first pitch is not less than or
;; equal to that of the second
(let ((p1 (make-pitch 'c4))
      (p2 (make-pitch 'd4)))
  (pitch<= p2 p1))

=> NIL

;; Equivalent pitches return T
(let ((p1 (make-pitch 'c4))
      (p2 (make-pitch 'c4)))
  (pitch<= p2 p1))

=> T

;; This method can be effectively used to compare the frequency values of two
;; pitch objects that were both created using frequency numbers
(let ((p1 (make-pitch 261.63))
      (p2 (make-pitch 293.66)))
  (pitch<= p1 p2))

=> T  

;; Due to sc's numerical accuracy, this method is not suitable for comparing
;; pitch objects of which one was created using a note-name symbol and the
;; other was created using a numerical frequency value. Such comparisons may
;; return misleading results.
(let ((p1 (make-pitch 261.63))
      (p2 (make-pitch 'c4)))
  (pitch<= p1 p2))

=> NIL

|#
;;; SYNOPSIS
(defmethod pitch<= ((p1 pitch) (p2 pitch))
;;; ****
  (<= (frequency p1) (frequency p2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Mon Jan  2 20:41:48 EST 2012: Added robodoc info

;;; ****m* pitch/pitch>=
;;; DESCRIPTION
;;; Test to see if the frequency value of one specified pitch object is greater
;;; than or equal to than that of a second.
;;;
;;; NB: Due to the fact that a given note-name may encompass several
;;;     fractionally different frequencies (e.g. both 261.626 and 261.627 are
;;;     both considered to be C4), this method is not suitable for comparing
;;;     pitch objects of which one was created using a frequency and the other
;;;     was created using a note-name symbol.
;;; 
;;; ARGUMENTS
;;; - A pitch object.
;;; - A second pitch object.
;;; 
;;; RETURN VALUE
;;; Returns T if the frequency value of the first pitch object is greater than
;;; or equal to that of the second, otherwise NIL.
;;; 
;;; EXAMPLE

#|
;; T is returned when the frequency of the first pitch is greater than or equal
;;; to that of the second
(let ((p1 (make-pitch 'd4))
      (p2 (make-pitch 'c4)))
  (pitch>= p1 p2))

=> T

;; NIL is returned when the frequency of the first pitch is not greater than or
;; equal to that of the second
(let ((p1 (make-pitch 'd4))
      (p2 (make-pitch 'c4)))
  (pitch>= p2 p1))

=> NIL

;; Equivalent pitches return T
(let ((p1 (make-pitch 'c4))
      (p2 (make-pitch 'c4)))
  (pitch>= p2 p1))

=> T

;; This method can be effectively used to compare the frequency values of two
;; pitch objects that were both created using frequency numbers
(let ((p1 (make-pitch 293.66)) 
      (p2 (make-pitch 261.63)))
  (pitch>= p1 p2))

=> T  

;; Due to sc's numerical accuracy, this method is not suitable for comparing
;; pitch objects of which one was created using a note-name symbol and the
;; other was created using a numerical frequency value. Such comparisons may
;; return misleading results.
(let ((p1 (make-pitch 'c4))
      (p2 (make-pitch 261.63)))
  (pitch>= p1 p2))

=> NIL

|#
;;; SYNOPSIS
(defmethod pitch>= ((p1 pitch) (p2 pitch))
;;; ****
  (>= (frequency p1) (frequency p2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Mon Jan  2 20:48:13 EST 2012: Added robodoc info

;;; ****m* pitch/pitch-in-range
;;; DESCRIPTION
;;; Determine whether the frequency of a given pitch object falls between the
;;; frequencies of two other given pitch objects.
;;; 
;;; ARGUMENTS
;;; - A first pitch object.
;;; - A second pitch object, which must be lower than the third.
;;; - A third pitch object, which must be higher than the second.
;;;
;;; RETURN VALUE
;;; T if the frequency value of the first specified pitch object falls between
;;; the second and third specified pitch objects, otherwise NIL.
;;; 
;;; EXAMPLE

#|

;; The method returns T when the frequency value of the first pitch object
;; falls between that of the second and third pitch objects.
(let ((p (make-pitch 'c4))
      (l (make-pitch 'g3))
      (h (make-pitch 'a7)))
  (pitch-in-range p l h))

=> T

;; The method returns NIL when the frequency value of the first pitch object is
;; below the range designated by the frequency values of the other two objects.
(let ((p (make-pitch 'g3))
      (l (make-pitch 'c4))
      (h (make-pitch 'a7)))
  (pitch-in-range p l h))

=> NIL

;; The method returns NIL when the frequency value of the first pitch object is
;; above the range designated by the frequency values of the other two objects.
(let ((p (make-pitch 'a7))
      (l (make-pitch 'g3))
      (h (make-pitch 'c4)))
  (pitch-in-range p l h))

=> NIL

;; The method will also return NIL if the frequency value of the second pitch
;; object is higher than that of the third
(let ((p (make-pitch 'c4))
      (l (make-pitch 'a7))
      (h (make-pitch 'g3)))
  (pitch-in-range p l h))

=> NIL

|#
;;; SYNOPSIS
(defmethod pitch-in-range ((p pitch) (lowest pitch) (highest pitch))
;;; ****
  (and (pitch>= p lowest)
       (pitch<= p highest)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Tue Jan  3 08:34:55 EST 2012: Added robodoc info

;;; ****m* pitch/pitch-min
;;; DESCRIPTION
;;; Determine which of two specified pitch objects has the lower frequency
;;; value and return that pitch object.
;;;
;;; NB: If the two frequency values are equal, the method returns a pitch
;;;     object equivalent to both.
;;; 
;;; ARGUMENTS
;;; - A first pitch object.
;;; - A second ptich object.
;;; 
;;; RETURN VALUE
;;; A pitch object.
;;; 
;;; EXAMPLE
#|
;; Compare two pitch objects and return the one with the lower frequency value 
(let ((p1 (make-pitch 'c4))
      (p2 (make-pitch 'd4)))
  (pitch-min p1 p2))

=> 
PITCH: frequency: 261.626, midi-note: 60, midi-channel: 0 
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

;; Comparing two pitch objects with equal frequency values returns a pitch
;; object equal to both
(let ((p1 (make-pitch 'c4))
      (p2 (make-pitch 'c4)))
  (data (pitch-min p1 p2)))

=> C4

|#
;;; SYNOPSIS
(defmethod pitch-min ((p1 pitch) (p2 pitch))
;;; ****
  (if (> (frequency p1) (frequency p2))
      p2
    p1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Tue Jan  3 09:05:50 EST 2012: Added robodoc info

;;; ****m* pitch/pitch-max
;;; DESCRIPTION
;;; Determine which of two specified pitch objects has the greater frequency 
;;; value and return that pitch object.
;;;
;;; NB: If the two frequency values are equal, the method returns a pitch
;;;     object equivalent to both.
;;; 
;;; ARGUMENTS
;;; - A first pitch object.
;;; - A second ptich object.
;;; 
;;; RETURN VALUE
;;; A pitch object.
;;; 
;;; EXAMPLE
#|
;; Compare two pitch objects and return the one with the greater frequency
;; value 
(let ((p1 (make-pitch 'c4))
      (p2 (make-pitch 'd4)))
  (pitch-max p1 p2))

=> 
PITCH: frequency: 293.665, midi-note: 62, midi-channel: 0 
       pitch-bend: 0.0 
       degree: 124, data-consistent: T, white-note: D4
       nearest-chromatic: D4
       src: 1.1224620342254639, src-ref-pitch: C4, score-note: D4 
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

;; Comparing two pitch objects with equal frequency values returns a pitch
;; object equal to both
(let ((p1 (make-pitch 'c4))
      (p2 (make-pitch 'c4)))
  (data (pitch-max p1 p2)))

=> C4

|#
;;; SYNOPSIS
(defmethod pitch-max ((p1 pitch) (p2 pitch))
;;; ****
  (if (< (frequency p1) (frequency p2))
      p2
    p1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Tue Jan  3 09:20:52 EST 2012: Added robodoc info

;;; ****m* pitch/midi-
;;; DESCRIPTION
;;; Determine the difference in number of semitones between the values of the
;;; MIDI values of two given pitch objects. 
;;;
;;; NB: This method does not return absolute difference; instead, it may return
;;;     positive or negative results depending on the order in which the pitch
;;;     objects are given. (This will aid in revealing directionality.)
;;; 
;;; ARGUMENTS
;;; - A first pitch object.
;;; - A second pitch object.
;;; 
;;; RETURN VALUE
;;; Returns a number. The number may be positive or negative.
;;; 
;;; EXAMPLE
#|
;; Subtracting the lower pitch object from the higher returns a positive number
(let ((p1 (make-pitch 'd4))
      (p2 (make-pitch 'c4)))
  (midi- p1 p2))

=> 2

;; Reversing the order in which the pitch objects are entered may return a
;; negative number
(let ((p1 (make-pitch 'd4))
      (p2 (make-pitch 'c4)))
  (midi- p2 p1))

=> -2

|#
;;; SYNOPSIS
(defmethod midi- ((p1 pitch) (p2 pitch))
;;; ****
  (- (midi-note p1) (midi-note p2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Tue Jan  3 09:32:33 EST 2012: Added robodoc info
 
;;; ****m* pitch/degree-
;;; DESCRIPTION
;;; Determine the difference between the quarter-tone degree of one pitch
;;; object and that of a second. 
;;;
;;; NB: This method does not return absolute difference; instead, it may return
;;;     positive or negative results depending on the order in which the pitch
;;;     objects are given. (This will aid in revealing directionality.)
;;; 
;;; NB: The DEGREE slot is measured in quarter-tones, not semitones. Thus,
;;;     middle-C is degree 120, not 60, and the difference between two
;;;     consecutive semitones is 2, not 1.
;;;
;;; ARGUMENTS
;;; - A first pitch object.
;;; - A second pitch object.
;;; 
;;; RETURN VALUE
;;; Returns a number. The number may be positive or negative.
;;; 
;;; EXAMPLE

#|
;; Subtracting the lower pitch object from the higher returns a positive number
(let ((p1 (make-pitch 'd4))
      (p2 (make-pitch 'c4)))
  (degree- p1 p2))

=> 4

;; Reversing the order in which the pitch objects are entered may return a
;; negative number
(let ((p1 (make-pitch 'd4))
      (p2 (make-pitch 'c4)))
  (degree- p2 p1))

=> -4

|#
;;; SYNOPSIS
(defmethod degree- ((p1 pitch) (p2 pitch))
;;; ****
  (- (degree p1) (degree p2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* pitch/pitch-
;;; DESCRIPTION
;;; Get the distance in semitones between the values of two pitch objects. This
;;; method also takes fractional values into consideration. The
;;; 
;;; ARGUMENTS
;;; - A first pitch object.
;;; - A second pitch object.
;;; 
;;; RETURN VALUE
;;; 
;;; 
;;; EXAMPLE
#|
;; Get the distance between two "white-keys"
(let ((p1 (make-pitch 'd4))
      (p2 (make-pitch 'c4)))
 (pitch- p1 p2))

=> 2.0

;; Get the distance in semitones between two frequencies (rounded to the
;; nearest degree, which by default is quarter-tones)
(let ((p1 (make-pitch 293.66))
      (p2 (make-pitch 261.63)))
 (pitch- p1 p2))

=> 2.0

;; Getting the distance in semitones between pitches with fractional values can
;; return fractional results
(let ((p1 (make-pitch 'dqs4))
      (p2 (make-pitch 'c4)))
 (pitch- p1 p2))

=> 2.5

|#
;;; SYNOPSIS
(defmethod pitch- ((p1 pitch) (p2 pitch))
;;; ****
  (- (midi-note-float p1) (midi-note-float p2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Fri Jan  6 14:37:59 EST 2012: Added robodoc info

;;; ****m* pitch/pitch-inc
;;; DESCRIPTION
;;; Increment the value of a given pitch object by one degree (default) or by a
;;; specified number of degrees (optional argument).
;;;
;;; NB: The slippery-chicken package uses a quarter-tone degree system by
;;;     default, so any function or method involving a degree argument will be
;;;     measured in quarter-tones, not semitones. Thus, while the MIDI note
;;;     value for 'C4 is 60 (chromatic semitones), (note-to-degree 'C4) will
;;;     return 120. Thus, this method will increment by one quarter-tone by
;;;     default, and any value the chooser uses for the optional argument is
;;;     also number of quarter-tones.
;;;
;;; NB: This method returns a new pitch object rather than modifying the values
;;;     of the original.
;;; 
;;; ARGUMENTS
;;; - A pitch object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - A number indicating the step (in degrees) by which the pitch value is to
;;;   be incremented. Defaults = 1. 
;;; 
;;; RETURN VALUE
;;; Returns a pitch object.
;;; 
;;; EXAMPLE
#|
;; The method by default returns a pitch object and increments by one
;; quarter-tone 
(let ((p (make-pitch 'c4)))
  (pitch-inc p))

=> 
PITCH: frequency: 269.292, midi-note: 60, midi-channel: 0 
       pitch-bend: 0.5 
       degree: 121, data-consistent: T, white-note: C4
       nearest-chromatic: C4
       src: 1.0293022394180298, src-ref-pitch: C4, score-note: CS4 
       qtr-sharp: 1, qtr-flat: NIL, qtr-tone: 1,  
       micro-tone: T, 
       sharp: NIL, flat: NIL, natural: NIL, 
       octave: 4, c5ths: 0, no-8ve: CQS, no-8ve-no-acc: C
       show-accidental: T, white-degree: 28, 
       accidental: QS, 
       accidental-in-parentheses: NIL, marks: NIL
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: CQS4, tag: NIL, 
data: CQS4

;; Using the optional argument, increment steps can be changed; for example,
;; here to one semitone (= 2 quarter-tones)
(let ((p (make-pitch 'c4)))
  (data (pitch-inc p 2)))

=> CS4

;; Here the method increments by 4 quarter-tones = 1 whole-tone 
(let ((p (make-pitch 'c4)))
  (data (pitch-inc p 4)))

=> D4

;; Incrementing by an additional number of quarter-tones at each pass 
(let ((p (make-pitch 'c4)))
  (loop for i from 0 to 4 collect (data (pitch-inc p i))))

=> (C4 CQS4 CS4 DQF4 D4)

|#
;;; SYNOPSIS
(defmethod pitch-inc ((p pitch) &optional (degrees 1))
;;; ****
  (make-pitch (degree-to-note (+ (degree p) degrees))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Returns number of octaves between p1 and p2 or nil if they're not octaves.
;;; when enharmonics-are-true, then gs4-af5 would be 1

(defmethod is-octave ((p1 pitch) (p2 pitch) &optional (enharmonics-are-true t))
;;; ****
  (let ((diff (- (degree p2) (degree p1))))
    (multiple-value-bind
        (octaves remainder)
        (floor diff (degrees-per-octave))
      (when (and (zerop remainder)
                 (or enharmonics-are-true
                     (eq (no-8ve p1) (no-8ve p2)))
                 (not (zerop octaves)))
        octaves))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+cm
(let* ((c5s '(f c g d a e b))
       (c5f (reverse c5s)))
  (defmethod update-pitch ((p pitch))
    (let* ((id (id p))
           (f (frequency p))
           (note-octave (when id (force-octave id)))
           (no-brackets (remove-accidental-in-parentheses-indicator id))
           (freq (when f (coerce f 'double-float))))
      (when (and (numberp freq)
                 (<= freq 0.0))
        (error "~a~%pitch::update-pitch: weird frequency (~a)"
               p freq))
      ;; accidentals to be placed in brackets will be indicated by a "b"
      ;; straight after the notes e.g.  cbs2 or dbqf4, once this is detected it
      ;; disappears from the id etc.
      (when no-brackets
        (setf (data p) (force-octave no-brackets)
              note-octave (data p) 
              (accidental-in-parentheses p) t))
      ;; MDE Wed Feb 13 11:30:58 2013 -- the (= (degree p)) seems to be a typo
      ;; and I can no longer remember what we wanted to test here.  (= [any
      ;; number]) will always return T so just remove it for now.
      (when (or freq (midi-note p) (data p) id) ; (not (= (degree p))))
        (when freq
          (setf (frequency p) freq))
        (when (and freq (not id))
          (let ((note (freq-to-note freq)))
            (unless note
              (error "pitch::update-pitch: ~
                      Couldn't get the note for frequency ~a!!!" freq))
            (setf (id p) note
                  note-octave note)))
        (when (and id (not freq))
          (let ((cm-freq (note-to-freq note-octave)))
            (unless cm-freq
              (error "pitch::update-pitch: ~
                      Couldn't get the frequency for pitch ~a!!!" id))
            (setf (frequency p) cm-freq)))
        (unless (frequency p)
          (error "pitch::update-pitch: ~
                  A pitch symbol or frequency must be given to initialize ~
                  a pitch: ~a" p))
        (unless (src p)
          (setf (src p) (/ (frequency p)
                           (note-to-freq (src-ref-pitch p)))))
        (setf (qtr-sharp p) (is-qtr-sharp note-octave)
              (sharp p) (is-sharp note-octave)
              (qtr-flat p) (is-qtr-flat note-octave)
              (natural p) (is-natural note-octave)
              (flat p) (is-flat note-octave)
              (qtr-tone p) (or (qtr-sharp p) (qtr-flat p)))
        (setf (data p) note-octave)
        (set-score-note p)
        (set-white-note p)
        ;; MDE Thu Jun 16 17:07:00 2016 -- if we haven't passed an octave then
        ;; the note-to-freq call above will use the CM scale's default octave,
        ;; so just correct that here

        ;; MDE Sat Jan 7 17:00:35 2012 -- freq-to-note will get the nearest
        ;; note; if the freq of that is > our given freq, we'll end up with the
        ;; note above our freq _and_ a high pitch-bend (get-pitch-bend always
        ;; returns > 0)--clearly wrong.
        (let ((pb (get-pitch-bend (frequency p))))
          (when (and (not (zerop pb))
                     ;; MDE Thu Apr 18 11:57:55 2013 -- 
                     ;; (< (frequency p) (note-to-freq (id p))))
                     (< (frequency p) (note-to-freq (nearest-chromatic p))))
            (setf pb (- pb 1.0)))
          (unless (and (> pb -1.0) (< pb 1.0))
            (error "pitch::update-pitch: pitch-bend is ~a!" pb))
          (setf (pitch-bend p) pb
                (micro-tone p) (not (zerop pb))))
        (setf ;; MDE Sun Jan  1 13:02:37 2012 -- otherwise it's single float so
              ;; causes comparison errors
              (frequency p) (coerce (frequency p) 'double-float)
              ;; degree in the current scale
              (degree p) (round (freq-to-degree (frequency p)))
              (midi-note p) (note-to-midi
                             (if (micro-tone p)
                                 (nearest-chromatic p)
                                 note-octave))
              (c5ths p) (cond ((flat p) 
                               ;; MDE Wed Feb 13 11:55:47 2013 -- must call
                               ;; rm-package as this method might be called
                               ;; from another package so symbol won't be found
                               ;; in the cf5 and c5s
                               (1+ (position (rm-package (no-8ve-no-acc p))
                                             c5f)))
                              ((sharp p)
                               (1+ (position (rm-package (no-8ve-no-acc p))
                                             c5s)))
                              (t 0))
              (data-consistent p) t)
        ;; MDE Thu Oct 25 14:49:15 2018
        (when (and (get-sc-config 'warn-low-pitch)
                   (< (octave p) -1))
          (warn "pitch::update-pitch: octave is < -1 ~
                 probably won't be able ~%to display in score format: ~a" 
                p))
        ;; MDE Thu May 30 20:12:17 2013 
        (when (and (get-sc-config 'warn-high-pitch)
                   (> (midi-note p) 119))
          (warn "pitch::update-pitch: midi-note is > 119: ~
                 probably won't be able ~%to display in score format: ~a" 
                p))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* pitch/set-midi-channel
;;; DESCRIPTION
;;; Set the MIDI-CHANNEL slot of the given pitch object. 
;;;
;;; The method takes two mandatory arguments in addition to the given pitch
;;; object, the first being the MIDI-channel used for non-microtonal pitch
;;; objects, the second that used for microtonal pitch objects. 
;;; 
;;; NB: The pitch object only has one MIDI-CHANNEL slot, and determines whether
;;;     that slot is set to the specified non-microtonal or microtonal
;;;     midi-channel argument based on whether or not the pitch of the given
;;;     pitch object is determined to be a microtone or not.
;;; 
;;; ARGUMENTS
;;; - A pitch object.
;;; - A number indicating the MIDI channel which is to be used to play back
;;;   non-microtonal pitches.
;;;
;;; OPTIONAL ARGUMENTS
;;; - A number indicating the MIDI channel which is to be used to play back
;;;   microtonal pitches. NB: See player.lsp/make-player for details on
;;;   microtones in MIDI output. Default = NIL which means it will take on the
;;;   value of the second argument (all pitches on the same channel, whether
;;;   microtonal or not). 
;;; 
;;; RETURN VALUE
;;; A number indicating which value has been set to the given pitch object's
;;; MIDI-CHANEL slot. 
;;; 
;;; EXAMPLE
#|
;; When the pitch of the given pitch object is non-microtonal, the method sets
;; that pitch object's MIDI-CHANNEL slot to the first value specified.
(let ((p (make-pitch 'c4)))
  (set-midi-channel p 11 12)
  (midi-channel p))

=> 11

;; When the pitch of the given pitch object is microtonal, the method sets
;; that pitch object's MIDI-CHANNEL slot to the second value specified.
(let ((p (make-pitch 'cqs4)))
  (set-midi-channel p 11 12))

=> 12

|#
;;; SYNOPSIS
(defmethod set-midi-channel ((p pitch) midi-channel
                              &optional microtones-midi-channel)
;;; ****
  (unless microtones-midi-channel
    (setq microtones-midi-channel midi-channel))
  (setf (midi-channel p) 
        (if (micro-tone p)
            (progn
              (unless (integer>0 microtones-midi-channel)
                (error "~a~&pitch::set-midi-channel: need ~
                        microtones-midi-channel (not 0)!"
                       p))
              microtones-midi-channel)
            midi-channel)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; also sets other slots by default--see below

(defmethod set-white-note ((p pitch))
  ;; (print p)
  ;; MDE Thu Apr 18 13:56:19 2013 -- only when we've got some data!
  (when (data p)
    (multiple-value-bind
          (note octave)
        (get-note-octave (data p) t) 
      ;; MDE Tue Jul 24 19:51:41 2012 
      (unless octave
        (error "~%pitch::set-white-note: no octave given for pitch ~a." (id p)))
      ;; MDE Mon May 14 14:32:17 2012 
      (unless (integerp octave)
        (warn "~a~%pitch::set-white-note: octave is not an integer. ~%~
               Probably won't display or play correctly." p))
      (when (and (get-sc-config 'warn-low-pitch)
                 (< octave -1))
        (warn "~a~%pitch::set-white-note: octave is less than -1.  ~%~
               Pitch probably won't display or play correctly." p))
      (let* ((str (string note))
             (note-letter (read-from-string (subseq str 0 1)))
             (white (read-from-string 
                     (format nil "~a~a"
                             note-letter octave)))
             (accidental (read-from-string
                          (if (> (length str) 1)
                              (subseq str 1)
                              "N")))
             (nacc (case (rm-package accidental)
                     (s 's)
                     (f 'f)
                     (n nil)
                     ;; in the case of microtones, the nearest chromatic is
                     ;; always lower
                     (qs nil)
                     (qf 'f)
                     ;; twelfth-tone scale
                     (ts nil) (ss nil) (ssf nil) (stf nil) (sts 's) (fts 'f)
                     (sss 's) (fss 'f) (sf 'f) (tf 'f)
                     (t (error "pitch::set-white-note: unrecognised ~
                                accidental ~a"
                               accidental))))
             (note-pos (position (rm-package note-letter) '(c d e f g a b))))
        (unless note-pos
          (error "pitch::set-white-note: ~
                Couldn't get note position for ~a (~a)" 
                 (id p) note-letter))
        ;; 22.10.11
        (setf (white-note p) white
              (nearest-chromatic p)
              (read-from-string (format nil "~a~a~a"
                                        note-letter (if nacc nacc "") octave))
              (accidental p) accidental
              (octave p) octave
              (no-8ve p) note
              (no-8ve-no-acc p) note-letter
              ;; MDE Sun Dec 29 13:59:09 2013 -- 1+ octave because the
              ;; lowest octave of the midi scale is -1.  Bear in mind that CMN
              ;; can't display pitches in the lowest octave.
              (white-degree p) (+ note-pos (* (1+ octave) 7)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod set-score-note ((p pitch))
  (setf (score-note p)
    (if (qtr-tone p)
        (remove #\Q (string (id p)))
      (string (id p)))))
                                     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* pitch/add-mark
;;; DESCRIPTION
;;; Add a specified mark to the MARKS slot of the given pitch object.
;;;
;;; NB: The add-mark method does not check first to see whether the mark being
;;;     added is a legitimate mark. It does print a warning, however, when the
;;;     specified mark is already present in the MARKS slot, though it adds it
;;;     anyway.
;;; 
;;; ARGUMENTS
;;; - A pitch object.
;;; - A symbol that is a mark.
;;; 
;;; RETURN VALUE
;;; A list. The method returns the entire contents of the given pitch object's
;;; MARKS slot as a list.
;;;
;;; Prints a warning when the specified mark is already present in the given
;;; pitch object's MARKs slot.
;;; 
;;; EXAMPLE
#|
;; By default the MARKS slot of a newly created pitch object is set to NIL
(let ((p (make-pitch 'c4)))
  (marks p))

=> NIL

;; Add two marks and print the contents of the given pitch object's MARKS slot
;; to see the changes
(let ((p (make-pitch 'c4)))
  (add-mark p 'pizz)
  (add-mark p 'a)
  (print (marks p)))  

=>
(A PIZZ)

;; Prints a warning when the specified mark is already present in the MARKS
;; slot, though it adds it again anyway.
(let ((p (make-pitch 'c4)))
  (add-mark p 'pizz)
  (add-mark p 'pizz)
  (marks p))

=> (PIZZ PIZZ)
WARNING: 
pitch::add-mark: mark PIZZ already present but adding again!


|#
;;; SYNOPSIS
(defmethod add-mark ((p pitch) mark &optional warn-rest warn-again)
;;; ****
  (declare (ignore warn-rest warn-again))
  (when (has-mark p mark)
    (warn "~a~&pitch::add-mark: mark ~a already present but adding again!"
          p mark))
  (validate-mark mark)
  (push mark (marks p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod add-mark-before ((p pitch) mark)
;;; ****
  (when (has-mark-before p mark)
    (warn "~a~&pitch::add-mark-before: pitch has ~a already but adding again!"
          p mark))
  (validate-mark mark)
  (push mark (marks-before p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Wed Jan  4 14:20:30 2012 
(defmethod has-mark ((p pitch) mark &optional (test #'equal))
;;; ****
  (has-mark-aux (marks p) mark test))

;;; MDE Thu Nov 14 14:26:35 2013 
(defmethod has-mark-before ((p pitch) mark &optional (test #'equal))
;;; ****
  (has-mark-aux (marks-before p) mark test))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+cmn
(defmethod get-cmn-12th-tone-accidentals ((p pitch))
  (let ((idstr (string (id p))))
    ;; NB take care of the order of these searches so as not to trigger a less
    ;; specific one before the more specific!  
    (cond ((search "STF" idstr) 'cmn::sharp-12-down)  ;; sharp 12th tone flat
          ((search "SSF" idstr) 'cmn::sharp-down)     ;; sharp 6th flat
          ((search "STS" idstr) 'cmn::sharp-12-up)    ;; sharp 12th sharp
          ((search "SSS" idstr) 'cmn::sharp-up)       ;; sharp 6th sharp
          ((search "TS" idstr) 'cmn::natural-12-up)   ;; natural 12th sharp
          ((search "SS" idstr) 'cmn::natural-up)      ;; natural 6th sharp
          ((search "TF" idstr) 'cmn::natural-12-down) ;; natural 12th flat
          ((search "SF" idstr) 'cmn::natural-down)    ;; natural 6th flat
          (t (error "~a ~%pitch::cmn-get-cmn-12th-tone-accidentals: ~
                     what pitch was that?" p)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Thu Nov 15 17:47:39 2018 -- just for convenience so we have the same
;;; method as the chord class
(defmethod get-pitch-symbols ((p pitch) &optional ignore)
  (declare (ignore ignore))
  (id p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; lilypond output.
;;; MDE Mon Feb 13 11:13:00 2017 -- though we don't use 3/4 sharp or flat, in
;;; English lilypond nomenclature they're -tqs and -tqf
(defmethod get-lp-data ((p pitch) &optional ignore1 ignore2 ignore3)
  (declare (ignore ignore1 ignore2 ignore3))
  ;; MDE Mon Jun 25 17:05:24 2012 
  ;; MDE Tue Aug 27 14:23:09 2013 issue a warning instead of an error.
  (when (micro-but-not-quarter-tone-p p)
    (when (zerop (lp-resolutions p))
      (warn "pitch::get-lp-data: Lilypond cannot display ~a. ~
             Resolving to the nearest ~%quarter tone. (Warning issued only ~
             once; other pitches may resolve~%automatically.)"  
            (data p)))
    (incf (lp-resolutions p))
    (setf p (make-pitch (freq-to-note (frequency p) 'quarter-tone))))
  (let* ((octave (octave p))
         (lp8ve (cond
                  ((= octave 3) "")
                  ((> octave 3) (make-string (- octave 3)
                                             :initial-element #\'))
                  ((< octave 3) (make-string (- 3 octave)
                                             :initial-element #\,))))
         ;; 27.8.11 don't forget marks e.g. harmonic heads
         (marks (format nil "~{~a~^~}" (loop for mark in (marks p)
                                          collect (lp-get-mark mark))))
         (marks-before (format nil "~{~a~^~}"
                               (loop for mark in (marks-before p)
                                  collect (lp-get-mark mark))))
         ;; 22.5.11 there is no n for natural in lilypond, rather just the note
         ;; name e.g. c not cn 
         (note (if (eq (accidental p) 'n)
                   (no-8ve-no-acc p)
                   (no-8ve p))))
    (string-downcase (format nil "~a~a~a~a~a"
                             marks-before note lp8ve 
                             (if (accidental-in-parentheses p) "?" "")
                             marks))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; MDE Mon Jun 25 17:02:12 2012 -- test whether a pitch is a microtone but not
;;; a quarter-tone (e.g. 6th or 12th tone) 
(defmethod micro-but-not-quarter-tone-p ((p pitch))
  (and (micro-tone p) (not (qtr-tone p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Doesn't return a cmn note object, rather a list with the note and correct
;;; accidental info, unless just-list is nil!

#+cmn
(defmethod get-cmn-data ((p pitch)
                         &optional 
                         (force-natural nil)
                         ;; return the cmn-data in a list or call cmn::note to
                         ;; convert it into a real cmn note?
                         (just-list t)
                         ;; used only for forcing grace notes to have one beam
                         (rhythm nil)
                         ignore1 ignore2 ignore3 ignore4 ignore5)
  ;; force natural comes from set-palette, it shouldn't be needed any longer
  ;; but hang on to it for now...
  (declare (ignore ignore1 ignore2 ignore3 ignore4 ignore5 force-natural))
  (let* ((white (rm-package (white-note p) :cmn))
         ;; (id (rm-package (id p) :cmn))
         (natural (natural p))
         (id-string (string (id p)))
         (result
          (cond ((not (show-accidental p)) white)
                ((qtr-sharp p) (list white 'cmn::sharp-with-centered-vertical))
                ((sharp p) (list white 'cmn::sharp))
                ((qtr-flat p) (list white 'cmn::flat-reversed))
                ((flat p) (list white 'cmn::flat))
                #|
                ((or (and natural force-natural)
                     (and natural (accidental-in-parentheses p)))
                 (list white 'cmn::natural))
                 (natural id)
                 |#
                (natural (list white 'cmn::natural))
                (t (list white (get-cmn-12th-tone-accidentals p))))))
    ;; don't allow bsts or bsss
    ;; (when (and (string= (string (id p)) "BS" :end1 2)
    ;;        (not (sharp p)))
    (when (and (> (length id-string) 3)
               (or (string= id-string "BSTS" :end1 3)
                   (string= id-string "BSSS" :end1 3)))
      (setf result (list (rm-package (transpose-note (white-note p) 1) :cmn)
                         (case (second result)
                           (cmn::sharp-12-down 'cmn::natural-12-down)
                           (cmn::sharp-down 'cmn::natural-down)
                           (cmn::sharp-12-up 'cmn::natural-12-up)
                           (cmn::sharp-up 'cmn::natural-up)
                           (t (error "pitch::cmn-get-data: ~
                                      Enharmonic for ~a????"
                                     p))))))
    (when (accidental-in-parentheses p)
      (unless (listp result)
        (error "~a ~%pitch::cmn-get-data: You tried to put a bracket ~
                around a non-existent accidental!" p))
      (setf (second result) (list (second result) 'cmn::in-parentheses
                                  '(cmn::dx -0.14))))
    (when (marks p)
      (setf result (append (if (listp result) result (list result))
                           (cmn::get-all-cmn-marks (marks p)))))
    ;; (format t "~%rthm: ~a" (eval (rm-package rhythm :cmn)))
    (if just-list
        result
      (apply #'cmn::note (econs 
                          (if (listp result)
                              (loop for i in result collect (eval i))
                            (list (eval result)))
                          (eval (rm-package rhythm :cmn)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
;;; No longer used.

#+cmn
(defmethod change-cmn-note-pitch (cmn-note (p pitch) 
                                  &optional (force-natural nil))
  (object-is-nil? cmn-note "pitch::change-cmn-note" 'cmn-note)
  (let* ((pcmn (cmn-note p force-natural))
         (result cmn-note) ;;(cmn::copy cmn-note))
         (note (eval (if (listp pcmn)
                         (first pcmn)
                       pcmn)))
         (accidental (when (listp pcmn)
                       (eval (second pcmn)))))
    (setf (cmn::cclass result) (cmn::cclass note)
          (cmn::octave result) (cmn::octave note)
          (cmn::sign result) accidental)
    result))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod (setf frequency) :after (value (p pitch))
  (declare (ignore value))
  (when (data-consistent p)
    (setf (data-consistent p) nil)
    (setf (src p) nil)
    (setf (id p) nil)
    (update-pitch p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod (setf id) :after (value (p pitch))
  (declare (ignore value))
  (when (data-consistent p)
    (setf (data-consistent p) nil)
    (setf (src p) nil)
    (setf (frequency p) nil)
    (update-pitch p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod (setf src-ref-pitch) :after (value (p pitch))
  (declare (ignore value))
  (setf (src p) nil)
  (update-pitch p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod force-white-note ((p pitch))
  (if (eq (white-note p) (id p))
      p
    (make-pitch (white-note p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; We want to transpose a sample so that it's frequency is that of the
;;; <pitch>.  The sample's main frequency is <freq>, return the necessary src.
;;; freq can either be a note e.g. c4 or a frequency in hertz.

(defmethod src-for-sample-freq (freq (p pitch))
  ;; (print freq) (print p)
  (unless (numberp freq)
    (setf freq (note-to-freq freq)))
  (/ (frequency p) freq))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; whether two pitches are an octave apart but one is differently inflected
(defmethod white-octave ((p1 pitch) (p2 pitch))
  (is-octave (make-pitch (white-note p1))
             (make-pitch (white-note p2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* pitch/enharmonic
;;; DESCRIPTION
;;; Get the enharmonic equivalent of the given pitch object. Two chromatically
;;; consecutive "white-note" pitches (e.g. B-sharp/C-natural) are considered
;;; enharmonically equivalent. If there is no enharmonic equivalent, the
;;; method just returns the same note. 
;;; 
;;; ARGUMENTS
;;; - A pitch object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to print a warning when no enharmonic can be found. Default = T. 
;;; 
;;; RETURN VALUE
;;; A new pitch object i.e. this method is non-destructive.
;;; 
;;; EXAMPLE

#|
;; A "black-key" enharmonic equivalent
(let ((p (make-pitch 'cs4)))
  (data (enharmonic p)))

=> DF4

;; Two chromatically consecutive "white-keys" are enharmonically equivalent
(let ((p (make-pitch 'f4)))
  (data (enharmonic p)))

=> ES4

;; The method returns a pitch object with the same pitch value if there is no
;; enharmonic equivalent
(let ((p (make-pitch 'g4)))
  (data (enharmonic p)))

=> G4

|#
;;; SYNOPSIS
(defmethod enharmonic ((p pitch) &key (warn t))
;;; ****
  (let ((pn (make-pitch (enharmonic-equivalent (data p) warn))))
    ;; MDE Mon Nov 11 11:01:14 2013 -- we'll lose marks (e.g. in octaves)
    ;; unless we do this.
    (when pn
      (setf (marks pn) (marks p)
            (marks-before pn) (marks-before p)))
    pn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod rm-marks ((p pitch) marks &optional (warn t))
  (rm-marks-aux p marks warn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Mon May 14 15:24:07 2012 
(defmethod print-simple ((p pitch) &optional (stream t) ignore)
  (declare (ignore ignore))
  (format stream "~a" (data p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Mon Jan 30 19:54:44 2017
;;; ****m* pitch/round-to-nearest
;;; DATE
;;; January 30th 2017
;;; 
;;; DESCRIPTION
;;; Round the pitch object to the nearest pitch in the current or given scale,
;;; i.e. force frequency, midi-note, pitch bend, etc. to conform to this nearest
;;; pitch.
;;;
;;; NB rounding to the nearest using a microtonal :scale argument might cause
;;; errors if the global (in-scale ...) scale is :chromatic because although
;;; chromatic-scale is a subset of quarter-tone and twelfth-tone (so all
;;; chromatic-pitches work) if you have microtonal pitches generated e.g. by
;;; frequency arguments to make-pitch the note symbol in the microtonal scale
;;; doesn't exist in :chromatic. E.g. the following breaks:
;;;
;;; (in-scale :chromatic)
;;; 264Hz is approx. c 1/12 sharp
;;; (round-to-nearest (make-pitch 264) :scale 'twelfth-tone)
;;; -> fails because rounding in 1/12 tone scale gives us CTS4, which doesn't
;;; exist in the global chromatic scale. Of course all is well if we do this
;;; beforehand: 
;;; (in-scale :twelfth-tone)
;;; 
;;; ARGUMENTS
;;; the pitch object
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword argument:
;;; :scale. The scale to use when rounding. (Common Music tuning object or
;;; symbol). If a symbol, then 'chromatic-scale, 'twelfth-tone, or 'quarter-tone
;;; only at present. Default is the current scale as set by (in-scale :...).
;;; 
;;; RETURN VALUE
;;; the modified pitch object
;;; 
;;; EXAMPLE
#|
(make-pitch 443)
-->
PITCH: frequency: 443.000, midi-note: 69, midi-channel: 1 
       pitch-bend: 0.12 
       degree: 138, data-consistent: T, white-note: A4
       nearest-chromatic: A4
       src: 1.6932597032572d0, src-ref-pitch: C4, score-note: A4 
       qtr-sharp: NIL, qtr-flat: NIL, qtr-tone: NIL,  
       micro-tone: T, 
...

(round-to-nearest (make-pitch 443)) 
-->
PITCH: frequency: 440.000, midi-note: 69, midi-channel: 1 
       pitch-bend: 0.0 
       degree: 138, data-consistent: T, white-note: A4
       nearest-chromatic: A4
       src: 1.6817929, src-ref-pitch: C4, score-note: A4 
       qtr-sharp: NIL, qtr-flat: NIL, qtr-tone: NIL,  
       micro-tone: NIL, 
...

;;; exact quarter tones round down, not up (true for quarter-sharps and flats):
(round-to-nearest (make-pitch 'dqs7) :scale 'chromatic-scale)
-->
PITCH: frequency: 2349.318, midi-note: 98, midi-channel: 1 
       pitch-bend: 0.0 
       degree: 196, data-consistent: T, white-note: D7
       nearest-chromatic: D7
       src: 8.979696, src-ref-pitch: C4, score-note: D7 
       qtr-sharp: NIL, qtr-flat: NIL, qtr-tone: NIL,  
       micro-tone: NIL, 
       sharp: NIL, flat: NIL, natural: T, 
       octave: 7, c5ths: 0, no-8ve: D, no-8ve-no-acc: D
       show-accidental: T, white-degree: 57, 
       accidental: N, 
       accidental-in-parentheses: NIL, marks: NIL, 
       marks-before: NIL
LINKED-NAMED-OBJECT: previous: NIL, 
                     this: NIL, 
                     next: NIL
NAMED-OBJECT: id: D7, tag: NIL, 
data: D7
**************

|#
;;; SYNOPSIS
(defmethod round-to-nearest ((p pitch) &key (scale cm::*scale*))
;;; ****
  ;; setf method updates other related slots
  ;; MDE Thu Oct 18 16:14:17 2018 -- don't use note-to-freq as the note may no
  ;; longer exist in the current scale
  ;; (setf (frequency p) (note-to-freq (data p)))
  (setf (id p) (freq-to-note (frequency p) scale))
  ;;           (midi-to-freq (round (midi-note-float p))))
  p)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Mon Mar 20 21:42:07 2017 -- we leave it to the event class to write the
;;; <note> tags as the pitches might be part of a chord
(defmethod write-xml ((p pitch) &key stream)
  ;; Dorico (at least) has problems with the lowest octave
  (when (< (octave p) 0)
    (warn "pitch::write-xml: Octaves lower than 0 might cause problems in ~
           notation software: ~a" p))
  (xml-write-marks (marks-before p) stream)
  (format stream "~&        <pitch>~
                  ~&          <step>~a</step>" (no-8ve-no-acc p))
  (when (and (accidental p)
             (not (eq 'n (accidental p))))
    (format stream "~&          <alter>~a</alter>"
            (case  (accidental p)
              (f -1) (s 1) (qs 0.5) (qf -0.5) (tqs 1.5) (tqf -1.5)
              (t (error "pitch::write-xml: only 1/4 tone accidentals ~
                         implemented up to now: ~a" p)))))
  (format stream "~&          <octave>~a</octave>~
                  ~&        </pitch>"
          (octave p))
  ;; MDE Sat Oct 13 10:27:33 2018 -- got to avoid writing the notehead tag
  ;; before duration and tie so we'll return it rather than write it here
  (let (notehead not accidental)
    (loop for m in (marks p) do
         (if (is-notehead m)
             (setq notehead m)
             (push m not)))
    (xml-write-marks not stream)
    (when (show-accidental p)
      (setq accidental
            (format nil "~&        <accidental~a>~a</accidental>"
                    (if (accidental-in-parentheses p) " parentheses=\"yes\"" "")
                    (case (accidental p)
                      (f "flat") (s "sharp") (n "natural") (qs "quarter-sharp")
                      (tqs "three-quarters-sharp") (tqf "three-quarters-flat")
                      (qf "quarter-flat")))))
    ;; MDE Sat Oct 13 10:34:46 2018 -- return the notehead too, if there was one
    (values accidental notehead)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Thu Nov  1 18:05:15 2018 -- taken over functionality from event class
(defmethod force-artificial-harmonic ((p pitch)
                                      &optional instrument (warn t)
                                        ;; MDE Thu Dec 6 10:24:12 2018 -- for
                                        ;; checking against ins range
                                        sounding
                                        ;; MDE Thu Jan 10 13:03:14 2019 --
                                        ;; overwrite natural harmonics?
                                        (naturals t))
;;; ****
  (let* ((p1 (transpose (clone p) -24))
         (p2 (transpose p1 5))
         ;; MDE Thu Jan 10 15:22:52 2019 
         (nats (or naturals (not (has-mark p 'harm))))
         ;; MDE Mon Apr 23 09:20:44 2012 
         (happy t))
    ;; MDE Mon Sep 24 18:18:37 2018 -- keep it a P4th
    ;; (print p1) (print p2)
    (when (bad-interval p1 p2)
      (setq p2 (enharmonic p2)))
    ;; MDE Mon Apr 23 09:16:34 2012 -- instrument
    ;; MDE Thu Jan 10 11:44:13 2019 -- don't overwrite natural harmonics
    (when (and instrument nats)
      ;; (print 'here)
      (unless (instrument-p instrument)
        (error "~a~%pitch::force-artificial-harmonic: argument should be an ~
                instrument object" instrument))
      ;; MDE Thu Dec  6 10:38:02 2018 -- deal with e.g. double-bass
      (when sounding
        (let ((tr (- (transposition-semitones instrument))))
          (setq p1 (transpose p1 tr)
                p2 (transpose p2 tr))))
      ;; (when (and p1 p2) (setq happy t))
      ;; (print p2)
      (unless (and (in-range instrument p1 nil nil nil t)
                   (in-range instrument p2 nil nil nil t))
        (setf happy nil)
        (when warn
          (warn "pitch::force-artificial-harmonic: creating an artificial ~
                 harmonic for this ~%pitch would go out of the instrument's ~
                 range: ~%~a" p))))
    (when (and happy nats)
      (rm-marks p1 '(harm) nil)
      (add-mark p2 'flag-head)
      (make-chord (list p1 p2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Thu Jan 10 13:32:28 2019 -- instrument actually required
(defmethod force-natural-harmonic ((p pitch)
                                   &optional instrument (warn t)
                                     (tolerance 15))
  (unless (instrument-p instrument)
    (error "pitch::force-natural-harmonic: instrument must be an object: ~a"
           instrument))
  ;; (print instrument)
  (multiple-value-bind
        (harm node)
      (natural-harmonic? instrument p tolerance)
    ;; (when harm (print (id harm)))
    ;; for now at least we do nothing with the node pitch
    harm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Related functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat Dec 31 12:19:46 EST 2011: Added robodoc info
;;; MDE Mon Jul 23 11:30:20 2018 -- changed default midi channel from 0 to 1

;;; ****f* pitch/make-pitch
;;; DESCRIPTION
;;; Create a pitch object, specifying a note as either a symbol or a
;;; number. When the note is specified as a symbol, it is treated as a
;;; note-name; when it is specified as a number, it is treated as a frequency
;;; in hertz.
;;; 
;;; NB If a pitch object is created from a frequency (rather than note symbol)
;;;    then the given frequency is stored and the note/midi-note etc. nearest
;;;    to it will be stored also.  So the frequency might not be the exact
;;;    frequency of the reflected note.  This is by design, so that unusual
;;;    temperaments can retain exact frequencies and show nearest notes etc.
;;; 
;;; ARGUMENTS
;;; - A note, either as an alphanumeric note name (e.g. cs4) or a numeric hertz
;;;   frequency.  
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :src-ref-pitch. A note-name symbol indicating the perceived fundamental
;;;   pitch of a given digital audio file, to allow for later transposition of
;;;   that audio file using note-names. Default: C4
;;; - :midi-channel. An integer indicating which MIDI channel is to be used for
;;;   playback of this pitch. Default: 1
;;; 
;;; RETURN VALUE
;;; - A pitch object.
;;; 
;;; EXAMPLE
#|
;; Make a pitch object using a note-name symbol
(make-pitch 'c4)

=> 
PITCH: frequency: 261.626, midi-note: 60, midi-channel: 0 
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

;; Make a pitch object using a frequency in hertz and including a value for the
;; keyword argument :midi-channel, then print the DATA and MIDI-NOTE slots to
;; see the method's automatic conversion for those values.
(let ((p (make-pitch 261.63 :midi-channel 1)))
  (print (data p))
  (print (midi-note p)))

=>
C4
60

;; Make a pitch object for use with a digital audio file that includes a
;; note-name symbol for the sample-rate-conversion reference pitch; then print
;; the SRC slot of the resulting pitch object
(let ((p (make-pitch 'c4 :src-ref-pitch 'a4)))
  (src p))

=> 0.5946035487490308

|#
;;; SYNOPSIS
(defun make-pitch (pitch &key (src-ref-pitch 'c4) (midi-channel 1))
;;; ****
  (when pitch
    (typecase pitch
      (pitch pitch) ; ignore midi-channel here
      ;; MDE Sat Dec 15 13:56:36 2018
      (string (make-instance 'pitch :id (read-from-string pitch)
                             :midi-channel midi-channel
                             :src-ref-pitch src-ref-pitch))
      (symbol (make-instance 'pitch :id pitch :midi-channel midi-channel
                             :src-ref-pitch src-ref-pitch))
      (number (make-instance 'pitch :frequency pitch :midi-channel midi-channel
                             :src-ref-pitch src-ref-pitch))
      (t (error "pitch::make-pitch: invalid argument to make-pitch: ~a" 
                pitch)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun transpose-pitch-list-force-white-notes (pitch-list semitones
                                               &optional lowest highest)
  (loop for p in (transpose-pitch-list pitch-list semitones :lowest lowest
                                       :highest highest)
      collect (force-white-note p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Tue Jan  3 10:51:55 EST 2012: Added robodoc info

;;; ****f* pitch/transpose-pitch-list
;;; DESCRIPTION
;;; Transpose the values of a list of pitch objects by a specified number of
;;; semitones. 
;;; 
;;; ARGUMENTS
;;; - A list of pitch objects.
;;; - A number indicating the number of semitones by which the list is to be
;;;   transposed. 
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments
;;; - :return-symbols. T or NIL indicating whether the method is to return a
;;;   list of pitch  objects or a list of note-name symbols for those pitch
;;;   objects. T = note-name symbols. Default = NIL.
;;; - :package. The name of the package to perform the transpositions. Default =
;;;   :sc.
;;; - :lowest. Don't transpose pitches which are lower than this
;;;   argument. Default = C-1 (midi note 0)  
;;; - :lowest. Don't transpose pitches which are higher than this
;;;   argument. Default = B8 (midi note 119) 
;;; 
;;; RETURN VALUE
;;; By default, the method returns a list of pitch objects. When the first
;;; optional argument is set to T, a list of note-name symbols is returned
;;; instead. 
;;; 
;;; EXAMPLE
#|
;; Create a list of pitch objects and apply the transpose-pitch-list method
;; with the semitones argument set to 2
(let ((pl))
  (setf pl (loop for m from 60 to 71 collect (make-pitch (midi-to-note m))))
  (transpose-pitch-list pl 2))

=>
(
PITCH: frequency: 293.665, midi-note: 62, midi-channel: 0 
[...]
PITCH: frequency: 311.127, midi-note: 63, midi-channel: 0 
[...]
PITCH: frequency: 329.628, midi-note: 64, midi-channel: 0 
[...]
PITCH: frequency: 349.228, midi-note: 65, midi-channel: 0 
[...]
PITCH: frequency: 369.994, midi-note: 66, midi-channel: 0 
[...]
PITCH: frequency: 391.995, midi-note: 67, midi-channel: 0 
[...]
PITCH: frequency: 415.305, midi-note: 68, midi-channel: 0 
[...]
PITCH: frequency: 440.000, midi-note: 69, midi-channel: 0 
[...]
PITCH: frequency: 466.164, midi-note: 70, midi-channel: 0 
[...]
PITCH: frequency: 493.883, midi-note: 71, midi-channel: 0 
[...]
PITCH: frequency: 523.251, midi-note: 72, midi-channel: 0 
[...]
PITCH: frequency: 554.365, midi-note: 73, midi-channel: 0 
[...]
)

;; Perform the same action with the return-symbols optional argument set to T
(let ((pl))
  (setf pl (loop for m from 60 to 71 collect (make-pitch (midi-to-note m))))
  (print (transpose-pitch-list pl 2 t)))

=> (D4 EF4 E4 F4 FS4 G4 AF4 A4 BF4 B4 C5 CS5)

|#
;;; SYNOPSIS
(defun transpose-pitch-list (pitch-list semitones
                             &key
                               (return-symbols nil)
                               (package :sc)
                               lowest highest)
;;; ****
  (setq lowest (make-pitch lowest)
        highest (make-pitch highest))
  (unless lowest (setq lowest (make-pitch 'c-1)))
  (unless highest (setq highest (make-pitch 'b8)))
  (let* ((pl (loop for p in pitch-list collect (make-pitch p)))
         ;; MDE Wed Aug 22 09:56:03 2018 -- taken from chord and integrated here
         ;; to avoid code duplication
         (result
          (loop 
           for pitch in pl
           for pir = (pitch-in-range pitch lowest highest)
           for new = (if pir
                         (transpose (make-pitch pitch) semitones)
                         pitch)
           ;; copy over the cmn marks (like special note heads etc.) if a list
           ;; of actual pitch objects was passed (make-pitch above will return
           ;; the original pitch unchanged); if not, no problem.
           do (when pir
                (setf (marks new) (my-copy-list (marks pitch))
                      (marks-before new) (my-copy-list (marks-before pitch))))
           collect new)))
    ;;      (result (loop for p in pl collect (transpose p semitones))))
    (setq result (sort-pitch-list result))
    (if return-symbols
        (pitch-list-to-symbols result package)
        result)))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Tue Jan  3 11:19:09 EST 2012: Added to robodoc info

;;; ****f* pitch/transpose-pitch-list-to-octave
;;; DESCRIPTION
;;; Transpose the pitch values of a list of pitch objects into a specified
;;; octave. The individual initial pitch objects can have initial pitch values
;;; of different octaves.  
;;; 
;;; ARGUMENTS
;;; - A list of pitch objects.
;;; - A number indicating the octave in which the resulting list should be.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :as-symbols. Set to T or NIL to indicate whether the method is to return
;;;   a list of pitch objects or a list of the note-name symbols from those
;;;   pitch objects. T = return as symbols. Default = NIL.
;;; - :package. Used to identify a separate Lisp package in which to itern
;;;   result. This is really only applicable is combination with :as-symbol set
;;;   to T. Default = :sc.
;;; - :remove-duplicates. Set to T or NIL to indicate whether any duplicate
;;;   pitch objects are to be removed from the resulting list.  T = remove
;;;   duplicates. Default = T.
;;; 
;;; RETURN VALUE
;;; Returns a list of pitch objects by default. When the keyword argument
;;; :as-symbols is set to T, the method returns a list of note-name symbols
;;; instead. 
;;; 
;;; EXAMPLE
#|
;; Create a list of four pitch objects from random MIDI numbers and print it,
;; then apply transpose-pitch-list-to-octave, setting the octave argument to 4,
;; and print the result
(let ((pl))
  (setf pl (loop repeat 4 collect (make-pitch (midi-to-note (random 128)))))
  (print (loop for p in pl collect (data p)))
  (print (transpose-pitch-list-to-octave pl 4)))

=>
(CS7 F7 B0 D4) 
(
PITCH: frequency: 493.883, midi-note: 71, midi-channel: 0 
[...]
data: B4
[...]
PITCH: frequency: 293.665, midi-note: 62, midi-channel: 0 
[...]
data: D4
[...]
PITCH: frequency: 277.183, midi-note: 61, midi-channel: 0 
[...]
data: CS4
[...]
PITCH: frequency: 349.228, midi-note: 65, midi-channel: 0 
[...]
data: F4
)

;; Setting the keyword argument :as-symbols to T return a list of note-names 
;; instead 
(let ((pl))
  (setf pl (loop repeat 4 collect (make-pitch (midi-to-note (random 128)))))
  (print (loop for p in pl collect (data p)))
  (print (transpose-pitch-list-to-octave pl 4 :as-symbols t)))

=>
(D5 E1 C7 AF1) 
(E4 AF4 D4 C4)

;; The method removes duplicate pitch objects from the resulting list by
;; default 
(let ((pl))
  (setf pl (loop repeat 4 collect (make-pitch (midi-to-note (random 128)))))
  (print (loop for p in pl collect (data p)))
  (print (transpose-pitch-list-to-octave pl 4 :as-symbols t)))

=>
(B7 AF1 AF7 G1) 
(G4 AF4 B4)

|#
;;; SYNOPSIS
(defun transpose-pitch-list-to-octave (pitch-list octave
                                       &key
                                       as-symbols
                                       (package :sc)
                                       (remove-duplicates t))
;;; ****
  (let ((result (loop for p in (init-pitch-list pitch-list t) collect
                     (transpose-to-octave p octave
                                          :as-symbol as-symbols
                                          :package package))))
    ;; of course, this means if we had octaves in the set then they
    ;; will be removed.
    (if remove-duplicates
        (remove-duplicates result :test (if as-symbols
                                            #'eq
                                            #'pitch=))
        result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Tue Jan  3 11:54:57 EST 2012: Added robodoc info

;;; ****f* pitch/pitch-list-to-symbols
;;; DESCRIPTION
;;; Return as a list the note-name values from a given list of pitch objects.
;;; 
;;; ARGUMENTS
;;; - A list of pitch objects.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - The package in which to process the list of pitches. Default = :sc.
;;; 
;;; RETURN VALUE
;;; A list of note-name symbols
;;; 
;;; EXAMPLE
#|
;; Create a list of pitch objects and apply the pitch-list-to-symbols method 
(let ((pl))
  (setf pl (loop for m from 0 to 127 by 13 
              collect (make-pitch (midi-to-note m))))
  (pitch-list-to-symbols pl))

=> (C-1 CS0 D1 EF2 E3 F4 FS5 G6 AF7 A8)

|#
;;; SYNOPSIS
(defun pitch-list-to-symbols (pitch-list &optional (package :sc))
;;; ****
  (loop for p in pitch-list collect 
        (cond ((pitch-p p) (rm-package (data p) package))
              ((symbolp p) p)
              (t (error "pitch::pitch-list-to-symbols: ~a is not a pitch!"
                        p)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Tue Jan  3 12:26:26 EST 2012: Added robodoc info

;;; ****f* pitch/sort-pitch-list
;;; DESCRIPTION
;;; Sort a list of pitch objects from low to high based on their frequency
;;; value.
;;; 
;;; ARGUMENTS
;;; - A list of pitch objects.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to indicate whether the method is to return a list of pitch
;;;   objects or a list of note-name symbols.
;;; - The package in which the operation is to be performed. Default = :sc.
;;; 
;;; RETURN VALUE
;;; Returns a list of pitch objects by default. When the first optional
;;; argument is set to T, the method returns a list of note-name symbols
;;; instead. 
;;; 
;;; EXAMPLE
#|
;; Create a list of pitch objects by passing downward through a series of MIDI
;; values and print the result. Then apply the sort-pitch-list method and print
;; the result of that to see the list now ordered from low to high.
(let ((pl))
  (setf pl (loop for m from 64 downto 60
              collect (make-pitch (midi-to-note m))))
  (print (loop for p in pl collect (data p)))
  (print (sort-pitch-list pl)))

=>
(E4 EF4 D4 CS4 C4) 
(
PITCH: frequency: 261.626, midi-note: 60, midi-channel: 0 
[...]
data: C4
[...]
PITCH: frequency: 277.183, midi-note: 61, midi-channel: 0 
[...]
data: CS4
[...]
PITCH: frequency: 293.665, midi-note: 62, midi-channel: 0 
[...]
data: D4
[...]
PITCH: frequency: 311.127, midi-note: 63, midi-channel: 0 
[...]
data: EF4
[...]
PITCH: frequency: 329.628, midi-note: 64, midi-channel: 0 
[...]
data: E4
)

;; Setting the first optional argument to T causes the method to return a list
;; of note-name symbols instead
(let ((pl))
  (setf pl (loop for m from 64 downto 60
              collect (make-pitch (midi-to-note m))))
  (sort-pitch-list pl t))

=> (C4 CS4 D4 EF4 E4)

|#
;;; SYNOPSIS
(defun sort-pitch-list (pitch-list &optional 
                        (return-symbols nil)
                        (package :sc))
;;; ****
  (let* ((pl (loop for p in pitch-list collect (make-pitch p)))
         (result (sort pl #'pitch<)))
    (if return-symbols
      (pitch-list-to-symbols result package)
      result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Tue Jan  3 12:40:02 EST 2012: Edited MDE robodoc info

;;; ****f* pitch/remove-octaves
;;; DESCRIPTION
;;; Removes all but one of any pitch items in a given list that have the same
;;; pitch-class but different octaves, keeping the lowest instance only.
;;;
;;; The list of pitch items may be a list of pitch objects or a list of
;;; note-name symbols. 
;;;
;;; ARGUMENTS 
;;; - A list of pitch items. These may be pitch objects or note-name symbols. 
;;;
;;; OPTIONAL ARGUMENTS
;;; keyword arguments
;;; - :as-symbol. T or NIL indicating whether the object is to return a list of
;;;   pitch objects or a list of note-name symbols. T = return pitch
;;;   objects. Default = NIL.
;;; - :package. Used to identify a separate Lisp package in which to itern
;;;   result. This is really only applicable is combination with :as-symbol set
;;;   to T. Default = :sc.
;;; - :allow. T or NIL to indicate whether pitch objects of certain, specified
;;;   octave-doublings are to be kept even if they are not the lowest. This
;;;   argument takes the form of either a single number or a list of
;;;   numbers. NB: This number does not indicate the octave in which the pitch
;;;   object is found, but rather pitch objects that are the specified number
;;;   of octaves above the lowest instance of the pitch class. Thus, :allow 2
;;;   indicates keeping the lowest pitch plus any instances of the same pitch
;;;   class two octaves above that lowest pitch (i.e.,
;;;   double-octaves). However, it is important to note that the function first
;;;   removes any octave doublings that are not excepted by the :allow
;;;   argument, which may produce confusing results. Given a list of
;;;   consecutive octaves, such as '(C1 C2 C3 C4) and an :allow value of 2, the
;;;   function will first remove any equal pitch classes that are are not 2
;;;   octaves apart, resulting in C2, C3, and C4 being removed as they are one
;;;   octave distant from C1, C2 and C3. The result of the function using these
;;;   values would therefore be '(C1).
;;; 
;;; RETURN VALUE  
;;; Returns a list of pitch objects by default. If the keyword argument
;;; :as-symbol is set to T, the method returns a list of note-name symbols
;;; instead.
;;; 
;;; If the first element of the pitch list is a number (i.e. a frequency), the 
;;; method returns a list of frequencies. 
;;; 
;;; EXAMPLE
#|
;; The method returns a list of pitch objects by default
(remove-octaves '(c1 c2 c3 g3))

=> (
PITCH: frequency: 32.703, midi-note: 24, midi-channel: 0 
[...]
data: C1
[...]
PITCH: frequency: 195.998, midi-note: 55, midi-channel: 0 
[...]
data: G3
[...]
)

;; If the first element of the pitch list is a frequency, the method returns a
;; list of frequencies
(remove-octaves '(261.63 523.26 1046.52 196.00))

=> (261.63 196.0)
 
;; Setting keyword argument :as-symbol to T returns a list of note-name symbols
;; instead 
(remove-octaves '(261.63 523.26 1046.52 196.00) :as-symbol t)

=> (C4 G3)

|#
;;; SYNOPSIS
(defun remove-octaves (pitch-list &key as-symbol allow (package :sc))
;;; ****
  (when (and allow (atom allow))
    (setf allow (list allow)))
  (let ((return-freqs (numberp (first pitch-list))))
    (loop 
       ;; 5.4.10: original comment:
       ;; "by reversing it we favour the notes that were
       ;; already there, removing the octaves that come later";
       ;; effect is though that we will remove higher octaves assuming this
       ;; is an ascending pitch list.
       with pitches = (loop for p in (reverse pitch-list) 
                         collect (make-pitch p))
       with len = (length pitch-list)
       with octave
       for p in pitches
       for i from 0
       unless
       (loop for j from (1+ i) below len do
          ;; is-octave returns the distance in octaves between two notes (or
          ;; nil if they're not octaves)
            (when (setf octave (is-octave p (nth j pitches)))
              (unless (member (abs octave) allow)
                (return t)))
            finally (return nil))
       collect (cond (as-symbol (rm-package (id p) package))
                     (return-freqs (frequency p))
                     (t p))
       into result
       finally (return (nreverse result)))))
              
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 

;;; SAR Tue Jan  3 13:58:09 EST 2012: Added robodoc info

;;; ****f* pitch/invert-pitch-list
;;; DESCRIPTION
;;; Using the lowest note in the list as the reference point, invert the rest
;;; of a given list of pitch items according to their distance from it. 
;;;
;;; The list of pitch items may consist of either note-name symbols, pitch
;;; objects or frequency numbers.
;;;
;;; NB: This function adheres to a concept of inversion more related to
;;;     interval set theory than to the traditional inversion of a melodic
;;;     contour. The given list of pitch items is first sorted from low to high
;;;     before the internal semitone intervals are assessed. The resulting list
;;;     will therefore always be in chromatic order, rather than having the
;;;     inverted melodic contour of the original.
;;; 
;;; ARGUMENTS
;;; - A list of pitch items. This may consist of pitch objects, note-name
;;;   symbols, or frequency numbers.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to indicate whether the result should be a list of pitch objects
;;;   or a list of note-name symbols. T = note-name symbols. Default = NIL.
;;; - The package in which the process is to be performed. Default = :sc.
;;; 
;;; RETURN VALUE
;;; Returns list of pitch objects by default. If the first optional argument is
;;; set to T, the function will return a list of note-name symbols instead.
;;; 
;;; EXAMPLE
#|
;; Simple example returning pitch symbols:
(invert-pitch-list '(c4 cs4 f4 b4) t)
=>
(C4 B3 G3 CS3)

;; The function returns a list of pitch objects by default
(let ((pl))
  (setf pl (loop for m in '(E4 G4 A4 C4) collect (make-pitch m)))
  (invert-pitch-list pl))

=>
(
PITCH: frequency: 261.626, midi-note: 60, midi-channel: 0 
[...]
data: C4
[...]
PITCH: frequency: 207.652, midi-note: 56, midi-channel: 0 
[...]
data: AF3
[...]
PITCH: frequency: 174.614, midi-note: 53, midi-channel: 0 
[...]
data: F3
[...]
PITCH: frequency: 155.563, midi-note: 51, midi-channel: 0 
[...]
data: EF3
)

;; Setting the first optional argument to T will cause the function to return a
;; list of note-name symbols instead
(let ((pl))
  (setf pl '(329.63 392.00 440.00 261.63))
  (invert-pitch-list pl t))

=> (C4 AF3 F3 EF3)

|#
;;; SYNOPSIS
(defun invert-pitch-list (pitch-list &optional
                          (return-symbols nil)
                          (package :sc))
;;; ****
  ;; just in case they're symbols 
  (let* ((pl (sort-pitch-list
              (loop for p in pitch-list collect (make-pitch p))))
         (lowest (first pl))
         (distances (loop for p in pl collect (pitch- lowest p)))
         (result (loop for st in distances collect (transpose lowest st))))
    (if return-symbols
        (pitch-list-to-symbols result package)
        result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-ids-from-pitch-list (pitch-list)
  (loop for p in pitch-list collect
     ;; MDE Sat Apr 23 12:22:25 2016 -- particularly for pitch-seq::get-notes
     ;; printing  
       (if (chord-p p)
           (get-ids-from-pitch-list (data p))
           (id p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Tue Apr 10 08:07:57 2012 
;;; ****f* pitch/print-simple-pitch-list
;;; DESCRIPTION
;;; Print the data symbols of a list of pitch objects. NB If you need to print
;;; these in the context of another printing routine, you can get a string by
;;; passing NIL as the stream.
;;; 
;;; DATE
;;; April 10th 2012
;;; 
;;; ARGUMENTS
;;; - A simple list of pitch objects.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - The stream to print to (e.g. an open file). Default: the Lisp Terminal
;;;   (REPL).
;;; 
;;; RETURN VALUE
;;; The list of pitch data symbols.
;;; 
;;; EXAMPLE
#|
(print-simple-pitch-list (init-pitch-list '(c4 d4 e4)))
=>
(C4 D4 E4) 
(C4 D4 E4)
|#
;;; SYNOPSIS
(defun print-simple-pitch-list (pitch-list &optional (stream t))
;;; ****
  (format stream "~a" (get-ids-from-pitch-list pitch-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun remove-accidental-in-parentheses-indicator (pitch-symbol)
  (let* ((string (string pitch-symbol))
         2nd-char)
    (when (> (length string) 1)
      ;; MDE Tue Jul 24 19:25:21 2012 -- moved setting 2nd-char here so we get
      ;; a sensible error message if we don't pass an 8ve 
      (setf 2nd-char (elt string 1))
      (when (equal 2nd-char #\B)
        (values (read-from-string (remove #\B string :start 1 :end 2)))))))
       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pitch-p (thing)
  (typep thing 'pitch))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; two pitches (objects or symbols), the number of semitones between the two
;;; and the number of steps between the white-degrees of the pitches (a unison
;;; would be 0, a 2nd 1, a 3rd 2, etc.)
(defun interval-aux (p1 p2 semitones white)
  (when (and p1 p2)
    (unless (pitch-p p1)
      (setf p1 (make-pitch p1)))
    (unless (pitch-p p2)
      (setf p2 (make-pitch p2)))
    (and (equal-within-tolerance semitones (abs (pitch- p1 p2))
                                 .00001)
         (= white (abs (- (white-degree p1) (white-degree p2)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun count-bad-adjacent-intervals (pitch-list)
  (loop 
      with result = 0
      with pitches = (init-pitch-list pitch-list nil)
      for p1 in pitches and p2 in (cdr pitches) do
        (when (bad-interval p1 p2)
          ;; (format t "~&~a ~a" (id p) (id (nth j ps)))
          (incf result))
      finally (return result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bad-interval (p1 p2)
  (or (augunison p1 p2)
      (dim2nd p1 p2)
      (aug2nd p1 p2)
      (augaug2nd p1 p2)
      (aug3rd p1 p2)
      (dim4th p1 p2)
      (augaug4th p1 p2)
      ;; MDE Thu Nov  1 18:19:41 2018
      (dimdim5th p1 p2)
      (aug5th p1 p2)
      (dim3rd p1 p2)
      (dim6th p1 p2)
      (dim7th p1 p2)
      (aug7th p1 p2)
      (augaug7th p1 p2)
      (dim8ve p1 p2)
      (aug8ve p1 p2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; e.g. gf-fs (gf-gf or fs-gs) aka enharmonic!

(defun dim2nd (p1 p2)
  (interval-aux p1 p2 0 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun augunison (p1 p2)
  (interval-aux p1 p2 1 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun aug2nd (p1 p2)
  (interval-aux p1 p2 3 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; e.g. gf-as (gf-bf)

(defun augaug2nd (p1 p2)
  (interval-aux p1 p2 4 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; e.g. f-as (f-bf), ef-gs (ef-af)

(defun aug3rd (p1 p2)
  (interval-aux p1 p2 5 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; better expressed as a major 3rd
;;; e.g. c-ff (c-e), cs-f (df-f), d-gf (d-fs), ds-g (ef-g), e-af (e-gs),
;;; fs-bf (fs-as or gf-bf), g-cf (g-b), gs-c (af-c), a-df (a-cs), as-d (bf-d),
;;; b-ef (d-ds) assuming no double-sharps/flats

(defun dim4th (p1 p2)
  (interval-aux p1 p2 4 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; e.g. gf-cs (gf-df)

(defun augaug4th (p1 p2)
  (interval-aux p1 p2 7 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Thu Nov  1 18:14:11 2018 -- e.g. ds-af better as ds-gs or P4th
(defun dimdim5th (p1 p2)
  (interval-aux p1 p2 5 4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; e.g. c-gs (c-af), df-a (cs a)

(defun aug5th (p1 p2)
  (interval-aux p1 p2 8 4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; e.g. cs-ef (cs-ds)

(defun dim3rd (p1 p2)
  (interval-aux p1 p2 2 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; e.g. cs-af (cs-gs)

(defun dim6th (p1 p2)
  (interval-aux p1 p2 7 5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; e.g. cs-bf (cs-as)

(defun dim7th (p1 p2)
  (interval-aux p1 p2 9 6))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; e.g. bf-as (bf-bf)

(defun aug7th (p1 p2)
  (interval-aux p1 p2 12 6))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; e.g. cf-bs (b-c)

(defun augaug7th (p1 p2)
  (interval-aux p1 p2 13 6))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; e.g. cs-c (df-c), e-ef (e-ds)

(defun dim8ve (p1 p2)
  (interval-aux p1 p2 11 7))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; e.g. c-cs (c-df) cf-c (b c)

(defun aug8ve (p1 p2)
  (interval-aux p1 p2 13 7))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Tue Jan  3 14:37:17 EST 2012: Added robodoc info

;;; ****f* pitch/pitch-member
;;; DESCRIPTION
;;; Test whether a specified pitch is a member of a given list of pitches.
;;;
;;; This function can take pitch objects, note-name symbols or numerical
;;; frequency values (or lists thereof) as its arguments.
;;; 
;;; ARGUMENTS
;;; - A pitch item. This may be a pitch object, a note-name symbol or a
;;;   numerical frequency value.
;;; - A list of pitch items. These items may be pitch objects, note-name
;;;   symbols, or numerical frequency values.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to indicate whether or not the function should consider
;;;   enharmonically equivalent pitches to be equal. T = enharmonics are 
;;;   considered equal. Default = T.
;;; - The second optional argument allows the user to specify the test for
;;;   comparison, such as note=, pitch-class-eq, or the default pitch=. If the
;;;   user wants to specify his or her own, the test must take three arguments:
;;;   p1, p2 and <enharmonics-are-equivalent> (which may of course be ignored).
;;; 
;;; RETURN VALUE 
;;; Similar to Lisp's "member" function, this method returns the tail of the
;;; tested list starting with the specified pitch if the pitch is indeed a
;;; member of that list, otherwise returns NIL. NB: The list returned is a list
;;; of pitch objects.
;;; 
;;; EXAMPLE

#|
;; Returns NIL if the specified pitch item is not a member of the given list 
(let ((pl '(c4 d4 e4)))
  (pitch-member 'f4 pl))

=> NIL

;; Returns the tail of the given list starting from the specified pitch if that
;; pitch is indeed a member of the tested list
(let ((pl '(c4 d4 e4)))
  (pitch-list-to-symbols (pitch-member 'd4 pl)))

=> (D4 E4)

;; Enharmonically equivalent pitches are considered equal by default
(let ((pl '(c4 ds4 e4)))
  (pitch-list-to-symbols (pitch-member 'ef4 pl)))

=> (DS4 E4)

;; Enharmonic equivalence can be turned off by setting the first optional
;; argument to NIL
(let ((pl '(c4 ds4 e4)))
  (pitch-list-to-symbols (pitch-member 'ef4 pl nil)))

=> NIL

|#
;;; SYNOPSIS
(defun pitch-member (pitch pitch-list 
                     &optional (enharmonics-are-equal t)
                               (test #'pitch=))
;;; ****
  ;; just in case they're not pitch objects already...
  (setf pitch-list (init-pitch-list pitch-list nil)
        pitch (make-pitch pitch))
  (member pitch pitch-list :test
          #'(lambda (p1 p2)
              (funcall test p1 p2 enharmonics-are-equal))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Tue Jan  3 15:30:17 EST 2012: Added robodoc info

;;; ****f* pitch/remove-pitches
;;; DESCRIPTION
;;; Remove a list of specified pitch items from a given list of pitch
;;; items. Even if only one pitch item is to be removed it should be stated as
;;; a list. 
;;;
;;; The pitch items can be in the form of pitch objects, note-name symbols or
;;; numerical frequency values.
;;; 
;;; ARGUMENTS
;;; - A list of pitch items from which the specified list of pitches is to be
;;;   removed. These can take the form of pitch objects, note-name symbols or 
;;;   numerical frequency values.
;;; - A list of pitch items to remove from the given list. These can take the
;;;   form of pitch objects, note-name symbols or numerical frequency 
;;;   values. Even if only one pitch is to be removed is must be stated as a 
;;;   list. 
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :enharmonics-are-equal. Set to T or NIL to indicate whether or not
;;;   enharmonically equivalent pitches are to be considered the same pitch. T
;;;   = enharmonically equivalent pitches are equal.  Default = T.
;;; - :return-symbols. Set to T or NIL to indicate whether the function is to
;;;   return a list of pitch objects or note-name symbols. T = note-name
;;;   symbols. Default = NIL.
;;; 
;;; RETURN VALUE
;;; Returns a list of pitch objects by default. When the keyword argument
;;; :return-symbols is set to T, the function will return a list of note-names
;;; instead. 
;;;
;;; If the specified list of pitches to be removed are not found in the given
;;; list, the entire list is returned.
;;; 
;;; EXAMPLE

#|
;; By default the function returns a list of pitch objects
(let ((pl '(c4 d4 e4)))
  (remove-pitches pl '(d4 e4)))

=> (
PITCH: frequency: 261.626, midi-note: 60, midi-channel: 0 
[...]
data: C4
[...]
)

;; Setting the keyword argument :return-symbols to T causes the function to
;; return a list of note-name symbols instead. Note in this example too that
;; even when only one pitch item is being removed, it must be stated as a list. 
(let ((pl '(261.62 293.66 329.62)))
  (remove-pitches pl '(293.66) :return-symbols t))

=> (C4 E4)

;; The function will also accept pitch objects
(let ((pl (loop for n in '(c4 d4 e4) collect (make-pitch n))))
  (remove-pitches pl `(,(make-pitch 'e4)) :return-symbols t))

=> (C4 D4)

;; By default the function considers enharmonically equivalent pitches to be
;; equal 
(let ((pl (loop for n in '(c4 ds4 e4) collect (make-pitch n))))
  (remove-pitches pl '(ef4) :return-symbols t))

=> (C4 E4)

;; This feature can be turned off by setting the :enharmonics-are-equal keyword
;; argument to NIL. In this case here, the specified pitch is therefore not
;; found in the given list and the entire original list is returned. 
(let ((pl (loop for n in '(c4 ds4 e4) collect (make-pitch n))))
  (remove-pitches pl '(ef4) 
                  :return-symbols t
                  :enharmonics-are-equal nil))

=> (C4 DS4 E4)

|#
;;; SYNOPSIS
(defun remove-pitches (pitch-list remove 
                       &key (enharmonics-are-equal t)
                            (return-symbols nil))
;;; ****
  ;; convert all notes to pitch objects if they aren't already.
  (setf pitch-list (init-pitch-list pitch-list nil)
        remove (init-pitch-list remove nil))
  (let ((result
         (remove-if #'(lambda (p)
                        (pitch-member p remove enharmonics-are-equal))
                    pitch-list)))
    (if return-symbols
        (pitch-list-to-symbols result)
      result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Lisp's intersection function doesn't sort and may change original order of
;;; lists so sort afterwards from lowest to highest pitch.

;;; SAR Tue Jan  3 16:57:15 EST 2012: Added robodoc info

;;; ****f* pitch/pitch-intersection
;;; DESCRIPTION
;;; Return pitch objects whose values consist of pitches common to two given
;;; lists of pitch items. The given lists of pitch items can consist of pitch
;;; objects or note-name symbols, or one list of one type and the second of the
;;; other. 
;;; 
;;; ARGUMENTS
;;; - A first list of pitch objects.
;;; - A second list of pitch objects.
;;; 
;;; RETURN VALUE
;;; Returns a list of pitch objects that are common to both original lists.
;;; 
;;; EXAMPLE
#|
;; Returns a list of pitch objects
(let ((p1 '(c4 d4 e4 f4))
      (p2 (loop for nn in '(d4 e4 f4 g4) collect (make-pitch nn))))
  (pitch-intersection p1 p2))

(
PITCH: frequency: 293.665, midi-note: 62, midi-channel: 0 
[...]
data: D4
[...]
PITCH: frequency: 329.628, midi-note: 64, midi-channel: 0 
[...]
data: E4
[...]
PITCH: frequency: 349.228, midi-note: 65, midi-channel: 0 
[...]
data: F4
[...]
) 

|#
;;; SYNOPSIS
(defun pitch-intersection (pitch-list1 pitch-list2)
;;; ****
  (sort (intersection (init-pitch-list pitch-list1 nil)
                      (init-pitch-list pitch-list2 nil)
                      :test #'pitch=)
        #'pitch<))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Tue Jan  3 17:39:38 EST 2012: Added robodoc info

;;; pitch can be a pitch object, frequency, or note symbol

;;; ****f* pitch/in-octave
;;; DESCRIPTION
;;; Test to see if a specified pitch item falls within a specified octave. The
;;; pitch item can be a pitch object, a numerical frequency value, or a
;;; note-name symbol. 
;;; 
;;; ARGUMENTS
;;; - A pitch item. This can be a pitch object, a numerical frequency value, or
;;;   a note-name symbol. 
;;; - A number that is the specified octave designator (e.g. the "4" in "C4").
;;; 
;;; RETURN VALUE
;;; T if the specified pitch item falls within the specified octave, otherwise
;;; NIL. 
;;; 
;;; EXAMPLE

#|
;; The function returns NIL if the specified pitch item does not fall within
;; the specified octave.
(let ((p (make-pitch 'c4)))
  (in-octave p 3))

=> NIL

;; The function will accept pitch objects
(let ((p (make-pitch 'c4)))
  (in-octave p 4))

=> T

;; The function will accept numerical frequency values
(let ((p 261.63))
  (in-octave p 4))

=> T

;; The function will accept note-name symbols
(let ((p 'c4))
  (in-octave p 4))

=> T

|#

;;; SYNOPSIS
(defun in-octave (pitch octave)
;;; ****
  (let ((p (make-pitch pitch)))
    (= octave (octave p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; MDE Mon Feb 13 15:19:57 2012 
(defun pitch-list= (pl1 pl2)
  (every #'pitch= pl1 pl2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Wed Apr  3 20:53:55 2013 -- 
;;; ****f* pitch/cmn-display-pitch-list
;;; DESCRIPTION
;;; Use CMN to display a list of pitch objects.
;;; 
;;; ARGUMENTS
;;; The list of pitch objects. This could also contain cmn objects if
;;; needed, e.g. cmn::bar, cmn::interior-double-bar, cmn::line-mark, etc.  
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :staff. The CMN staff object to display with.  Default = cmn::treble.
;;; - :size.  The CMN size for the staff.  Default = 20.
;;; - :file.  The path of the file to (over)write.  
;;;    Default = "pitches.eps" in the directory (get-sc-config 'default-dir)
;;;   (default /tmp)
;;; - :auto-open.  Whether to open the .EPS file once written. Currently only
;;;    available on OSX with SBCL and CCL.  Uses the default app for .EPS
;;;    files, as if opened with 'open' in the terminal.  Default = Value of
;;;    (get-sc-config cmn-display-auto-open).
;;; 
;;; RETURN VALUE
;;; A CMN score object.
;;; 
;;; SYNOPSIS
(defun cmn-display-pitch-list 
    (pitches &key (staff cmn::treble) (size 20)
     (auto-open (get-sc-config 'cmn-display-auto-open))
     (file (concatenate 'string (get-sc-config 'default-dir) "pitches.eps")))
;;; ****
  ;; (if (and pitches (every #'pitch-p pitches))
  ;; MDE Sat Nov 12 15:15:59 2016 -- allow cmn bar lines to be mixed in
  (if (and pitches
           (every #'(lambda (x)
                      ;; (member (type-of x) '(pitch
                      ;; cmn::write-protected-bar)))
                      (or (typep x 'pitch)
                          (typep x 'cmn::visible-mixin)))
                  pitches))
      (prog1 
          (cmn::cmn (cmn::output-file file) (cmn::size size)
                    (cmn::all-output-in-one-file t)
                    cmn::staff staff
                    (cmn::engorge
                     (loop for p in pitches collect
                          (if (pitch-p p)
                              (eval (econs (get-cmn-data p) cmn::q))
                              ;; something else like cmn::bar
                              p))))
        ;; MDE Thu Aug 22 10:56:44 2013
        (when auto-open
          (system-open-file file)))
      (error "pitch::cmn-display-pitch-list: argument 1 must be a list of ~
          pitch objects: ~a" pitches)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 6.8.13
(defun rm-bad-intervals (pitch-list bad-intervals)
  (loop 
     with last = (first pitch-list)
     with result = (list last)
     for this in (rest pitch-list) 
     for diff = (abs (pitch- last this))
     do
     (unless (member diff bad-intervals :test #'=) ; so 2.0 = 2
       (setf last this)
       (push this result))
     finally (return (nreverse result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 13.8.13
(defun pitch-list-high-low (pitches) 
  (let (hi low)
    (loop for p in pitches do
         (if (or (not hi) (pitch> p hi))
             (setf hi p))
         (if (or (not low) (pitch< p low))
             (setf low p)))
    (values hi low (id hi) (id low))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* instruments/natural-harmonic
;;; DATE
;;; December 24th 2013
;;;
;;; DESCRIPTION
;;; Determine whether a pitch can be played as a natural harmonic on a (string)
;;; instrument (with the guitar as default).
;;;
;;; MDE Tue Jan 8 18:22:09 2019 -- This function is retained for historical
;;; reasons but has been superseded by the instrument method natural-harmonic?
;;; along with the associated instrument slots open-strings, open-string-marks,
;;; and nodes (which altogether are more flexible and efficient than this
;;; function alone)  
;;; 
;;; ARGUMENTS
;;; - the pitch (symbol or pitch object)
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :tuning. a list of the fundamentals of the open strings, as pitch objects
;;;   or symbols.  These should descend from the highest string.  Default:
;;;   guitar tuning.  
;;; - :highest-partial.  Integer. What we consider the highest harmonic possible
;;;   (counting the fundamental as 1).
;;; - :tolerance.  The deviation in cents that we can accept for the frequency
;;;   comparison.  Default = 10.
;;; - :debug. Print data for debugging/testing purposes.  Default = NIL.
;;; 
;;; RETURN VALUE
;;; The string number and partial number as a list if possible as a harmonic,
;;; or NIL if not.
;;; 
;;; EXAMPLE
#|
(NATURAL-HARMONIC 'b5) ; octave harmonic of B string
=> (2 2)
SC> (NATURAL-HARMONIC 'b6) ; octave + 5th of high E string
=> (1 3)
|#
;;; SYNOPSIS
(defun natural-harmonic (pitch &key (tuning '(e5 b4 g4 d4 a3 e3))
                         (highest-partial 6) (tolerance 15) debug)
;;; ****
  (setf pitch (make-pitch pitch)
        tuning (loop for p in tuning collect (make-pitch p)))
  ;; in the case of a note being a harmonic of more than one string, starting
  ;; with the highest string first guarantees that we'll return the harmonic
  ;; with the lowest partial number
  (loop with hertz = (abs (cents-hertz pitch tolerance))
     ;; with srt = (abs (- (semitones (* tolerance .01)) 1.0))
     with result = nil
     for string in tuning and string-num from 1 
     while (not result)
     do
     (when debug
       (format t "~&String ~a: ~a" string-num (id string)))
     (loop for partial from 2 to highest-partial 
        for harmonic = (make-pitch (* partial (frequency string)))
        do
        (when debug
          (format t "~&seeking: ~,3f: partial ~a: ~,3f, max deviation: ~
                       ~,3fHz, abs diff: ~,3f" 
                  (frequency pitch) partial (frequency harmonic) hertz
                  (- (frequency pitch) (frequency harmonic))))
        ;; don't use pitch= as this might fail because of nearest note
        ;; spellings whereas the frequencies are close enough 
        (when (<= (abs (- (frequency pitch) (frequency harmonic)))
                  hertz)
            ; (pitch= pitch harmonic t hertz)
          (setf result (list string-num partial))
          (return)))
     finally (return result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Sat Apr 23 19:28:28 2016

;;; ****f* pitch/find-nearest-pitch
;;; DATE
;;; April 23rd 2016, Edinburgh
;;; 
;;; DESCRIPTION
;;; In a given list of pitches, find the nearest pitch to the pitch given as
;;; the 2nd argument. 
;;; 
;;; ARGUMENTS
;;; - a list of pitches: either pitch objects or symbols. If symbols they will
;;;   be convered to pitch objects first and optionally sorted from low to
;;;   high.
;;; - the pitch to use in the search for the nearest. Again: either a pitch
;;;   object or symbol.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to indicate whether the pitch list should be sorted from low to
;;;   high before being searched. Default = NIL.
;;; 
;;; RETURN VALUE
;;; Two values: the pitch from the pitch list which is the nearest to the 2nd
;;; argument, and the index of this pitch in the list. NB if the 2nd argument
;;; is equidistant from two pitches in the list, then the first pitch in the
;;; list will be returned.
;;; 
;;; EXAMPLE
#|
(find-nearest-pitch '(b0 d1 fs1 a4) 'e1)
==>
PITCH: frequency: 36.708, midi-note: 26, midi-channel: 0 
       pitch-bend: 0.0 
       degree: 52, data-consistent: T, white-note: D1
       nearest-chromatic: D1
       src: 0.14030775, src-ref-pitch: C4, score-note: D1 
       qtr-sharp: NIL, qtr-flat: NIL, qtr-tone: NIL,  
       micro-tone: NIL, 
       sharp: NIL, flat: NIL, natural: T, 
       octave: 1, c5ths: 0, no-8ve: D, no-8ve-no-acc: D
       show-accidental: T, white-degree: 15, 
       accidental: N, 
       accidental-in-parentheses: NIL, marks: NIL, 
       marks-before: NIL
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: D1, tag: NIL, 
data: D1
**************

1
|#
;;; SYNOPSIS
(defun find-nearest-pitch (pitch-list pitch &optional auto-sort)
;;; ****
  (when (and pitch pitch-list)
    (setq pitch (make-pitch pitch)
          pitch-list (init-pitch-list pitch-list auto-sort))
    (loop with distance = 999999 with it with itp
       for p in pitch-list
       for d = (abs (pitch- p pitch))
       for i from 0 do
         (when (< d distance)
           (setq it i
                 itp p
                 distance d))
       finally (return (values itp it)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get-freq (thing)
  (typecase thing
    (integer (midi-to-freq thing))
    (float thing)
    (symbol (frequency (make-pitch thing)))
    (pitch (frequency thing))
    (event (get-frequency thing))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Thu May 26 12:51:38 2016. Handles note symbols, pitch objects, and
;;; events (chords or single pitches) and treats integers as MIDI pitch and
;;; floats as frequencies
(defun pitch-list-stats (pitch-list)
  (let ((hash (make-hash-table))
        (freqs '()))
    (loop for thing in pitch-list
       for freq = (get-freq thing)
       do (if (listp freq)              ; chords
              (loop for f in freq do (push f freqs))
              (push freq freqs)))
    (loop for freq in freqs do
         (if (gethash freq hash)
             (incf (gethash freq hash))
             (setf (gethash freq hash) 1)))
    hash))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Thu May 26 16:03:07 2016 -- pitch-list could also be events
(defun add-auxiliary-notes-aux (pitch-list &key (num-notes 3) (interval 1)
                                             ignore (activity-level 5)
                                             destructively verbose)
  (let* ((igns (loop for p in (force-list ignore) collect
                    (frequency (make-pitch p))))
         (most-used (hash-least-useds (pitch-list-stats pitch-list)
                                      :ignore igns :num num-notes
                                      :auto-inc nil :invert t))
         (result '())
         (als (loop repeat num-notes collect (make-al 2))))
    (loop for thing in pitch-list
       for freq = (get-freq thing)
       for pos = (position freq most-used)
       do
         (push (if (and (atom freq)     ; don't fiddle with chords
                        pos
                        (active (nth pos als) activity-level))
                   (if (and (event-p thing)
                            ;; thanks to Dan Ross for this; a further
                            ;; improvement might be to change the tied notes if
                            ;; this is tied-from but hey ho...
                            (not (is-tied-to thing))
                            (not (is-tied-from thing)))
                       (prog2
                           (when verbose
                             (format t "~&At bar ~a, transposing ~a "
                                     (bar-num thing) (get-pitch-symbol thing)))
                           (transpose thing interval
                                      :destructively destructively)
                         (when verbose
                           (format t "to ~a" (get-pitch-symbol thing))))
                       (transpose thing interval))
                   thing)
               result))
    (nreverse result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF pitch.lsp
