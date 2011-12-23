;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* linked-named-object/pitch
;;; NAME 
;;; pitch
;;;
;;; File:             pitch.lsp
;;;
;;; Class Hierarchy:  named-object -> linked-named-object -> pitch
;;;
;;; Version:          1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Implementation of the pitch class for holding pitch
;;;                   information: sybmolic representation (eg c4), MIDI note
;;;                   number, frequency, sampling-rate conversion etc.
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    March 18th 2001
;;;
;;; $$ Last modified: 18:50:17 Fri Dec 23 2011 ICT
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

;;; The pitch symbol is stored in the <id> and <data> slots.

(defclass pitch (linked-named-object)
  ;; the closest midi-note (integer, c4 = 60) to this pitch; when micro-
  ;; tones, then this will always be the chromatic tone 1/4 tone lower.
  ((midi-note :accessor midi-note :initarg :midi-note :initform nil)
   ;; the midi pitch-bend (in semitones) to create microtones NB this is always
   ;; upwards 
   (pitch-bend :accessor pitch-bend :type float :initform 0.0)
   (degree :accessor degree :type integer :initarg :degree :initform -1)
   (score-note :accessor score-note :initform nil)
   ;; the given id minus the accidental e.g. cs4 = c4, dqf2 = d2
   (white-note :accessor white-note :type symbol :initform nil)
   ;; the number of the note in a white-note octave, i.e. c = 0, d = 1, e = 2
   ;; (in octave 0) .... gs4 = 4 + (4 x 7) = 32
   (white-degree :accessor white-degree :initform nil)
   ;; 22.10.11 always the note below a microtone as pitch-bend is always up
   (nearest-chromatic :accessor nearest-chromatic :type symbol :initform nil)
   ;; just the accidental part of the note e.g. s, f or qf etc.
   (accidental :accessor accidental :type symbol :initform nil)
   (frequency :accessor frequency :initarg :frequency :initform nil)
   (midi-channel :accessor midi-channel :initarg :midi-channel :initform 0)
   (octave :accessor octave :type integer :initform -1)
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
   (marks :accessor marks :type list :initarg :marks 
              :initform nil)
   ;; in the circle of 5ths, how far advanced is this pitch i.e. a natural is
   ;; 0, fs and bf are 1, cs and ef are 2 ... microtones are 0.
   (c5ths :accessor c5ths :type number :initform 0)
   ;; e.g. from fs4 -> fs
   (no-8ve :accessor no-8ve :type symbol :initform nil)
   ;; e.g. from fs4 -> f
   (no-8ve-no-acc :accessor no-8ve-no-acc :type symbol :initform nil)
   (src :accessor src :initarg :src :initform nil)
   (src-ref-pitch :accessor src-ref-pitch :type symbol :initarg :src-ref-pitch
                  :initform 'c4)
   ;; when frequency is given, we have to update id and vice-versa.  This slot
   ;; tells us whether this was done and so avoids endless back-and-forths when
   ;; calling the setf methods.
   (data-consistent :accessor data-consistent :type boolean :initform nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object :before ((i pitch) stream)
  (format stream "~%PITCH: frequency: ~a, midi-note: ~a, midi-channel: ~d ~
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
                  ~%       accidental-in-parentheses: ~a, marks: ~a"
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
          (accidental-in-parentheses i) (marks i)))

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
        (slot-value to 'src-ref-pitch) (src-ref-pitch from)
        (slot-value to 'data-consistent) (data-consistent from)))

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
                        (pitch-bend p)
                        time
                        amplitude
                        duration  
                        ;; now this means of course we can't have any
                        ;; microtonal chords (with normal and
                        ;; inflected pitches) but somehow this seems
                        ;; preferable to hogging several channels per
                        ;; instrument...doesn't it???  todo: rethink
                        ;; this!!!!
                        ;; 1- because cm channels start at 0
                        (1- (midi-channel p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; just change the micro-tone slot to <value>
(defmethod force-micro-tone ((p pitch) &optional value)
  (setf (micro-tone p) value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod midi-note-float ((p pitch))
  (float (+ (midi-note p) (pitch-bend p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod delete-marks ((p pitch))
  (setf (marks p) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Semitones can be fractional.  Returns a new pitch with make-pitch rather
;;; than altering the current pitch object.  
;;; the ignore fields are there because of the transpose method in tl-set,
;;; chord and event.  

(defmethod transpose ((p pitch) semitones &key (as-symbol nil) (package :sc)
                      ignore)
  (declare (ignore ignore))
  (let ((new-note (transpose-note (id p) semitones))
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

;;; Rounds to the nearest chromatic (MIDI) pitch.

(defmethod pitch-round ((p pitch) 
                        &key
                        (as-symbol nil)
                        (package :sc))
  ;; we can't use lisp's round function as in the case of x.5 it
  ;; rounds to the nearest even number so (round 1.5) => 2 and 
  ;; (round 2.5) => 2 
  (let ((sym (midi-to-note (cond ((qtr-sharp p) (midi-note p))
                                 ((qtr-flat p) (1+ (midi-note p)))
                                 (t (midi-note-float p))))))
    (if as-symbol
        (rm-package sym package)
      (make-pitch sym))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod no-accidental ((p pitch))
  (setf (show-accidental p) nil
        (accidental-in-parentheses p) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod transpose-to-octave ((p pitch) new-octave 
                                &key
                                (as-symbol nil)
                                (package :sc))
  (unless (integer>=0 new-octave)
    (error "pitch::transpose-to-octave: octave must be an integer >= 0: ~a"
           new-octave))
  (let ((transp (* 12 (- new-octave (octave p)))))
    (transpose p transp :as-symbol as-symbol :package package)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod pitch= ((p1 pitch) (p2 pitch) &optional enharmonics-are-equal)
  (and (equal-within-tolerance (frequency p1) (frequency p2))
       (or enharmonics-are-equal (eq (data p1) (data p2)))
       (= (midi-note p1) (midi-note p2))
       (equal-within-tolerance (src p1) (src p2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* pitch/pitch-class-eq
;;; FUNCTION
;;; pitch-class-eq:
;;; test whether two pitches are of the same pitch class i.e. both Cs, or F#s,
;;; irrespective of octave. 
;;; 
;;; ARGUMENTS 
;;; - pitch 1
;;; - pitch 2
;;; - whether to treat enharmonics as equivalents e.g. B# and C
;;; 
;;; RETURN VALUE  
;;; t or nil
;;; 
;;; EXAMPLE
;;; (pitch-class-eq (make-pitch 'c3) (make-pitch 'bs8) t) -> T
;;; 
;;; DATE 14.8.2010
;;; 
;;; SYNOPSIS
(defmethod pitch-class-eq ((p1 pitch) (p2 pitch)
                           &optional enharmonics-are-equal)
;;; ****
  (or (eq (no-8ve p1) (no-8ve p2))
      (and enharmonics-are-equal
           (eq (no-8ve p1) (no-8ve (enharmonic p2))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod note= ((p1 pitch) (p2 pitch))
  (eq (data p1) (data p2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod pitch< ((p1 pitch) (p2 pitch))
  (< (frequency p1) (frequency p2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod pitch> ((p1 pitch) (p2 pitch))
  (> (frequency p1) (frequency p2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod pitch<= ((p1 pitch) (p2 pitch))
  (<= (frequency p1) (frequency p2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod pitch>= ((p1 pitch) (p2 pitch))
  (>= (frequency p1) (frequency p2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod pitch-in-range ((p pitch) (lowest pitch) (highest pitch))
  (and (pitch>= p lowest)
       (pitch<= p highest)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod pitch-min ((p1 pitch) (p2 pitch))
  (if (> (frequency p1) (frequency p2))
      p2
    p1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod pitch-max ((p1 pitch) (p2 pitch))
  (if (< (frequency p1) (frequency p2))
      p2
    p1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; whole semitone subtraction 
(defmethod midi- ((p1 pitch) (p2 pitch))
  (- (midi-note p1) (midi-note p2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod degree- ((p1 pitch) (p2 pitch))
  (- (degree p1) (degree p2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; NB this takes pitch bend into consideration
(defmethod pitch- ((p1 pitch) (p2 pitch))
  (- (midi-note-float p1) (midi-note-float p2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod pitch-inc ((p pitch) semitones)
  (make-pitch (degree-to-note (+ (degree p) semitones))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Returns number of octaves between p1 and p2 or nil if they're not octaves.
;;; when enharmonics-are-true, then gs4-af5 would be 1

(defmethod is-octave ((p1 pitch) (p2 pitch) &optional (enharmonics-are-true t))
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
           (no-brackets (remove-accidental-in-parentheses-indicator id))
           (freq (when f (coerce f 'float))))
      (when (and freq
                 (<= freq 0.0))
        (error "~a~%pitch::update-pitch: weird frequency (~a)"
               p freq))
      ;; accidentals to be placed in brackets will be indicated by a "b"
      ;; straight after the notes e.g.  cbs2 or dbqf4, once this is detected it
      ;; disappears from the id etc.
      (when no-brackets
        (setf (id p) no-brackets
              id no-brackets
              (accidental-in-parentheses p) t))
      (when (or freq (midi-note p) (data p) id (not (= (degree p))))
        (when freq
          (setf (frequency p) freq))
        (when (and freq (not id))
          (let ((note (freq-to-note freq)))
            (unless note
              (error "pitch::update-pitch: ~
                    Couldn't get the note for frequency ~a!!!" freq))
            (setf (id p) note)))
        (when (and id (not freq))
          (let ((cm-freq (note-to-freq id)))
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
        (setf (qtr-sharp p) (is-qtr-sharp (id p))
              (sharp p) (is-sharp (id p))
              (qtr-flat p) (is-qtr-flat (id p))
              (natural p) (is-natural (id p))
              (flat p) (is-flat (id p))
              (pitch-bend p) (get-pitch-bend (frequency p))
              (micro-tone p) (not (zerop (pitch-bend p)))
              (qtr-tone p) (or (qtr-sharp p) (qtr-flat p)))
        (set-score-note p)
        ;; (set-natural p)
        (set-white-note p)
        (setf (data p) (id p)
              ;; degree in the current scale
              (degree p) (round (freq-to-degree (frequency p)))
              (midi-note p) (note-to-midi
                                   (if (micro-tone p)
                                       (nearest-chromatic p)
                                       (id p)))
              (c5ths p) (cond ((flat p) (1+ (position (no-8ve-no-acc p) c5f)))
                              ((sharp p) (1+ (position (no-8ve-no-acc p) c5s)))
                              (t 0))
              (data-consistent p) t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod set-midi-channel ((p pitch) midi-channel microtones-midi-channel)
  (setf (midi-channel p) 
        (if (micro-tone p)
            (progn
              (unless (integer>0 microtones-midi-channel)
                (error "~a~&pitch::set-midi-channel: need ~
                         microtones-midi-channel!"
                       p))
              microtones-midi-channel)
            midi-channel)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; also sets accidental, nearest chromatic, and white-degree as a side effect.

(defmethod set-white-note ((p pitch))
  (multiple-value-bind
      (note octave)
      (get-note-octave (id p)) 
    (let* ((str (string note))
           (note-letter (read-from-string (subseq str 0 1)))
           (white (read-from-string 
                   (format nil "~a~a"
                           note-letter octave)))
           (accidental (read-from-string
                        (if (> (length str) 1)
                            (subseq str 1)
                          "N")))
           (nacc (case accidental
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
                   (t (error "pitch::set-white-note: unrecognised accidental ~a"
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
            (white-degree p) (+ note-pos (* octave 7))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod set-score-note ((p pitch))
  (setf (score-note p)
    (if (qtr-tone p)
        (remove #\Q (string (id p)))
      (string (id p)))))
                                     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod add-mark ((p pitch) mark &optional warn-rest)
  (declare (ignore warn-rest))
  (validate-mark mark)
  (push mark (marks p)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; lilypond output
(defmethod get-lp-data ((p pitch) &optional ignore1 ignore2)
  (declare (ignore ignore1 ignore2))
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
         ;; 22.5.11 there is no n for natural in lilypond, rather just the note
         ;; name e.g. c not cn 
         (note (if (eq (accidental p) 'n)
                   (no-8ve-no-acc p)
                   (no-8ve p))))
    (string-downcase (format nil "~a~a~a~a"
                             note lp8ve 
                             (if (accidental-in-parentheses p) "?" "")
                             marks))))

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
;;; <pitch>.  The sample's main frequency is <freq>, return the necesary src.
;;; freq can either be a note e.g. c4 or a frequency in hertz

(defmethod src-for-sample-freq (freq (p pitch))
  (unless (numberp freq)
    (setf freq (note-to-freq freq)))
  (/ (frequency p) freq))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; whether two pitches are an octave apart but one is differently inflected

(defmethod white-octave ((p1 pitch) (p2 pitch))
  (is-octave (make-pitch (white-note p1))
             (make-pitch (white-note p2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Related functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; If note is a symbol, treat it as a note-name, when a number, as a freq.

(defun make-pitch (note &key (src-ref-pitch 'c4) (midi-channel 0))
  (when note
    (typecase note
      (pitch note) ; ignore midi-channel here
      (symbol (make-instance 'pitch :id note :midi-channel midi-channel
                             :src-ref-pitch src-ref-pitch))
      (number (make-instance 'pitch :frequency note :midi-channel midi-channel
                             :src-ref-pitch src-ref-pitch))
      (t (error "pitch::make-pitch: invalid argument to make-pitch: ~a" 
                note)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun transpose-pitch-list-force-white-notes (pitch-list semitones)
  (loop for p in (transpose-pitch-list pitch-list semitones) 
      collect (force-white-note p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun transpose-pitch-list (pitch-list semitones &optional 
                             (return-symbols nil)
                             (package :sc))
  (let* ((pl (loop for p in pitch-list collect (make-pitch p)))
         (result (loop for p in pl collect (transpose p semitones))))
    (if return-symbols
      (pitch-list-to-symbols result package)
      result)))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun transpose-pitch-list-to-octave (pitch-list octave
                                       &key
                                       as-symbols
                                       (package :sc)
                                       (remove-duplicates t))
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

(defun pitch-list-to-symbols (pitch-list &optional (package :sc))
  (loop for p in pitch-list collect 
        (cond ((pitch-p p) (rm-package (data p) package))
              ((symbolp p) p)
              (t (error "pitch::pitch-list-to-symbols: ~a is not a pitch!"
                        p)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Sort a list from low to high
(defun sort-pitch-list (pitch-list &optional 
                        (return-symbols nil)
                        (package :sc))
  (let* ((pl (loop for p in pitch-list collect (make-pitch p)))
         (result (sort pl #'pitch<)))
    (if return-symbols
      (pitch-list-to-symbols result package)
      result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****f* pitch/remove-octaves
;;; FUNCTION
;;; remove-octaves: removes octaves from a list of pitches.  The lower notes
;;; will remain and octaves of them removed.
;;; 
;;; ARGUMENTS 
;;; - a list of pitches: may be pitch objects or symbols.
;;; - (key :as-symbol default nil): return note symbols.
;;; - (key :package default slippery-chicken): the lisp package the note symbols
;;; should be in.
;;; - (key :allow default nil): could be that you want certain octaves to be
;;; allowed (e.g. double octaves), so this is a single number or list of
;;; acceptable octaves e.g. :allow 2 would mean double-octaves are left alone.
;;; 
;;; RETURN VALUE  
;;; a list of pitch objects (default) or frequencies (default if the first
;;; element of the pitch-list is a number i.e. frequency) or note symbols ff
;;; :as-symbol is t.
;;; 
;;; EXAMPLE
;;; (remove-octaves '(c1 c2 g3) :as-symbol t) -> (C1 C3)
;;; 
;;; SYNOPSIS
(defun remove-octaves (pitch-list &key as-symbol allow (package :sc))
;;; ****
  (when (and allow (atom allow))
    (setf allow (list allow)))
  (let ((return-freqs (numberp (first pitch-list))))
    (loop 
       ;; 5.4.10: don't know what the logic was in this comment; guess it made
       ;; sense in the context I originally developed the function in:
       ;; "by reversing it we favour the notes that were
       ;; already there, removing the octaves that come later"
       ;; end effect is though that we will remove higher octaves assuming this
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
       finally (return (reverse result)))))
              
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 
;;; Using the first note in the list as the reference point, invert
;;; the rest according to their distance from it.

(defun invert-pitch-list (pitch-list &optional
                          (return-symbols nil)
                          (package :sc))
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
  (loop for p in pitch-list collect (id p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun remove-accidental-in-parentheses-indicator (pitch-symbol)
  (let* ((string (string pitch-symbol))
         (2nd-char (elt string 1)))
    (when (> (length string) 1)
      (when (equal 2nd-char #\B)
        (values (read-from-string (remove #\B string :start 1 :end 2)))))))
       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pitch-p (thing)
  (typep thing 'pitch))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defun pitch-member (pitch pitch-list 
                     &optional (enharmonics-are-equal t)
                               (test #'pitch=))
  ;; just in case they're not pitch objects already...
  (setf pitch-list (init-pitch-list pitch-list nil)
        pitch (make-pitch pitch))
  (member pitch pitch-list :test
          #'(lambda (p1 p2)
              (funcall test p1 p2 enharmonics-are-equal))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun remove-pitches (pitch-list remove 
                       &key (enharmonics-are-equal t)
                            (return-symbols nil))
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

(defmethod enharmonic ((p pitch) &key (warn t)) ;; ignore1 ignore2)
  ;; (declare (ignore ignore1 ignore2))
  ;; (print (id p))
  (make-pitch (enharmonic-equivalent (id p) warn)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Return those pitches common to both lists.  Can be lists of pitch objects
;;; or pitch symbols, or a mixture of both.  Lisp's intersection function
;;; doesn't sort and may change original order of lists so sort afterwards from
;;; lowest to highest pitch.

(defun pitch-intersection (pitch-list1 pitch-list2)
  (sort (intersection (init-pitch-list pitch-list1 nil)
                      (init-pitch-list pitch-list2 nil)
                      :test #'pitch=)
        #'pitch<))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; pitch can be a pitch octave, frequency, or note symbol
(defun in-octave (pitch octave)
  (let ((p (make-pitch pitch)))
    (= octave (octave p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; EOF pitch.lsp
