;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* linked-named-object/instrument
;;; NAME 
;;; instrument
;;;
;;; File:             instrument.lsp
;;;
;;; Class Hierarchy:  named-object -> linked-named-object -> instrument
;;;
;;; Version:          0.92
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Implementation of the instrument class which defines
;;;                   musical instrument properties like range and
;;;                   collects/stores information about what the instrument
;;;                   plays: how many notes, in how many bars etc.  
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    4th September 2001
;;;
;;; $$ Last modified: 16:22:34 Tue Mar 20 2012 GMT
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

;;; 02.12.11 SEAN: Changed ROBODoc header to reflect class hierarchy

(eval-when (compile)
  (declaim (optimize (speed 3) (safety 1) (space 0) (debug 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :slippery-chicken)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The id slot stores the instrument name; the data slot is unused.

;;; The lowest and highest notes of the instrument can be given either in
;;; -written or -sounding form whereby if both are given, it is an error, and
;;; whichever is given will be used to calculate the value of the other.

(defclass instrument (linked-named-object)
  ;; the name that should appear on this instrument's staff in the score
  ((staff-name :accessor staff-name :initarg :staff-name :initform nil)
   (staff-short-name :accessor staff-short-name :initarg 
                     :staff-short-name :initform nil)
   ;; the lowest written note of the ins, given as a note symbol, converted to
   ;; pitch object.
   (lowest-written :accessor lowest-written :initarg :lowest-written
                   :initform nil) 
   ;; the highest written note of the ins, given as a note symbol, converted to
   ;; pitch object.
   (highest-written :accessor highest-written :initarg :highest-written
                    :initform nil)
   ;; the lowest sounding note of the ins, given as a note symbol, converted to
   ;; pitch object.
   (lowest-sounding :accessor lowest-sounding :initarg :lowest-sounding
                    :initform nil) 
   ;; the highest sounding note of the ins, given as a note symbol, converted
   ;; to pitch object.
   (highest-sounding :accessor highest-sounding :initarg :highest-sounding
                     :initform nil)
   ;; what key the instrument is in
   (transposition :accessor transposition :initarg :transposition
                  :initform nil)  
   ;; MDE Sat Jan  7 15:19:26 2012 -- if this isn't in clefs list it will be
   ;; added and a warning issued 
   (starting-clef :accessor starting-clef :type symbol :initarg :starting-clef 
                  :initform 'treble)
   ;; what clefs the instrument can use, as symbols; user gives order in
   ;; preference, we then reverse it so we don't have to do that in best-clef
   ;; method 
   (clefs :accessor clefs :type list :initarg :clefs :initform nil)
   ;; same as above but when writing score in C (e.g. bass clarinet uses bass
   ;; clef when notating sounding pitch, but not when written pitch)
   (clefs-in-c :accessor clefs-in-c :type list :initarg :clefs-in-c 
               :initform nil)
   ;; the transposition in semitones of transposition.  If this is not given,
   ;; we will assume the note given in transposition goes down, and print a
   ;; warning that we're assuming this.
   (transposition-semitones :accessor transposition-semitones 
                            :initarg :transposition-semitones :initform nil) 
   ;; whether this instrument should be written in C (t) or not (nil)
   (score-write-in-c :accessor score-write-in-c :type boolean
                     :initarg :score-write-in-c :initform nil)
   ;; whether in score, this instrument should write bar-lines or not; given as
   ;; an integer which specifies for how many instruments above this one the
   ;; bar line should be drawn (for grouping systems).
   (score-write-bar-line :accessor score-write-bar-line
                         :initarg :score-write-bar-line :initform 1)
   (largest-fast-leap :accessor largest-fast-leap  
                      :initarg :largest-fast-leap :initform nil)
   ;; when choosing pitches, we can try to select the highest or lowest,
   ;; otherwise if nil, then we go for the middle (should be nil 'high or 'low)
   (prefers-notes :accessor prefers-notes :initarg :prefers-notes
                  :initform nil)
   ;; whether the ins can play chords (not multiphonics): t or nil
   (chords :accessor chords :type boolean :initarg :chords :initform nil)
   ;; if the instrument can play chords then it will need a reference to a
   ;; function that can select chords for the instrument.  NB This should be a
   ;; symbol not a function object, so when making instruments just 'my-fun not
   ;; #'my-fun.
   ;; MDE Mon Jan  2 16:42:36 2012 
   ;; When defining sets, you can use the subsets assoc-list of a set object to
   ;; assign particular chords to specific instruments.  These could then be
   ;; selected and used/modified in the chord-function given to the instrument,
   ;; e.g.  
   ;;     (let* ((subset (get-data 'guitar (subsets set)))
   (chord-function :accessor chord-function :initarg :chord-function
                   :initform nil)
   ;; we might want to limit our instrument to playing a certain subset of a
   ;; set (for instance to select chords); if so, then set the following slot
   ;; to the id of the subset--obviously this means that this subset would have
   ;; to be present in every set...
   (subset-id :accessor subset-id :initarg :subset-id :initform nil)
   ;; whether the instrument can play microtones or not
   (microtones :accessor microtones :type boolean :initarg :microtones
               :initform nil)
   ;; a list of any notes which the instrument can't play, usually certain
   ;; quarter tones NB These are given as written notes, not sounding, in the
   ;; case of transposing instruments, but are then stored as sounding notes 
   (missing-notes :accessor missing-notes :type list :initarg :missing-notes
                  :initform nil) 
   ;; 5/3/07: no longer needed (never was used...); chord-function obviates
   ;; the need for this slot.
   ;; the maximum width in semitones the instrument can play.
   ;; (chord-max :type integer :accessor chord-max :initarg :chord-max 
   ;;            :initform -1)
   (midi-program :accessor midi-program :type integer :initarg :midi-program
                 :initform 1)
   
   ;; All the following are used for statistics and hence have no initarg

   ;;; The total number of bars in the piece in which this instrument plays.
   (total-bars :accessor total-bars :type integer :initform 0)
   ;;; The total number of notes (not rests or tied notes, therefore
   ;;; midi-notes) in the piece which this instrument plays. 
   (total-notes :accessor total-notes :type integer :initform 0)
   ;;; The total-duration in seconds that the instrument plays for in the
   ;;; piece. 
   (total-duration :accessor total-duration :type float :initform 0.0)
   ;;; Each time a note is played, this slot will be incremented by the degree
   ;;; of the note (in the scale for the piece).  Then, at the end, we can
   ;;; divide this by total-notes and have the mean note (tessitura).
   (total-degrees :accessor total-degrees :type integer :initform 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize-instance :after ((ins instrument) &rest initargs)
  (declare (ignore initargs))
  (check-starting-clef ins)
  (let ((lw (lowest-written ins))
        (ls (lowest-sounding ins))
        (hw (highest-written ins))
        (hs (highest-sounding ins))
        (tr (transposition ins))
        (pn (prefers-notes ins))
        (tr-st (transposition-semitones ins)))
    (flet ((written-sounding-error (arg1 arg2)
             (error "instrument::initialize-instance: ~
                     One and only of the '~a' and '~a' slots ~
                     must be defined!" arg1 arg2)))
      (when (and tr (not tr-st))
        (setf (transposition-semitones ins) (transposition-to-semitones tr)))
      (when (and tr-st (not tr))
        (setf (transposition ins) (semitones-to-transposition tr-st)))
      (when (and tr tr-st)
        (unless (check-transposition-against-semitones tr tr-st)
          (error "instrument::initialize-instance: ~
                  Transposition given as ~a which is not ~a semitones ~
                  (given in the transposition-semitones slot)"
                 tr tr-st)))
      (when (and (not tr)
                 (not tr-st))
        (setf (transposition ins) 'c
              (transposition-semitones ins) 0))
      (when (and lw ls)
        (written-sounding-error "lowest-written" "lowest-sounding"))
      (when (and hw hs)
        (written-sounding-error "highest-written" "highest-sounding"))
      ;;; MDE Fri Dec  9 13:31:15 2011 -- merely to trigger the setf method
      (setf (missing-notes ins) (missing-notes ins))
      (if lw
          (setf (lowest-written ins) lw)
          (setf (lowest-sounding ins) ls))
      (if hw
          (setf (highest-written ins) hw)
          (setf (highest-sounding ins) hs))
      (setf (clefs ins) (if (clefs ins)
                            (reverse (clefs ins))
                            (list (starting-clef ins)))
            (clefs-in-c ins) (if (clefs-in-c ins) 
                                 (reverse (clefs-in-c ins))
                                 (copy-list (clefs ins))))
      ;; MDE Sat Jan  7 15:22:49 2012 -- 
      (when (and (starting-clef ins) (not (member (starting-clef ins)
                                                  (clefs ins))))
        (warn "~a~&instrument::initialize-instance: starting-clef (~a) is not ~
               a member of clefs (~a) so adding automatically"
              ins (starting-clef ins) (clefs ins))
        (setf (clefs ins) (econs (clefs ins) (starting-clef ins))))
      (when pn
        (unless (or (prefers-high ins)
                    (prefers-low ins))
          (error "~a~&instrument::initialize-instance: prefers-notes should be ~
                  either nil, 'high or 'low"
                 ins)))
      (when (and (chords ins)
                 (not (chord-function ins)))
        (setf (chord-function ins) 'default-chord-function)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; MDE Fri Dec  9 13:20:20 2011
(defmethod (setf missing-notes) (new-value (ins instrument))
  (unless (listp new-value)
    (setf new-value (list new-value)))
  (setf (slot-value ins 'missing-notes)
        (loop for pitch in new-value collect 
             (transpose (make-pitch pitch) (transposition-semitones ins)))))

(defmethod (setf lowest-written) (new-value (ins instrument))
  (setf (slot-value ins 'lowest-written) (make-pitch new-value)
        (slot-value ins 'lowest-sounding)
        (when (lowest-written ins)
          (transpose (lowest-written ins) (transposition-semitones ins)))))

(defmethod (setf highest-written) (new-value (ins instrument))
  (setf (slot-value ins 'highest-written) (make-pitch new-value)
        (slot-value ins 'highest-sounding)
        (when (highest-written ins)
          (transpose (highest-written ins) (transposition-semitones ins)))))

(defmethod (setf lowest-sounding) (new-value (ins instrument))
  (setf (slot-value ins 'lowest-sounding) (make-pitch new-value)
        (slot-value ins 'lowest-written)
        (when (lowest-sounding ins)
          (transpose (lowest-sounding ins) (- (transposition-semitones ins))))))

(defmethod (setf highest-sounding) (new-value (ins instrument))
  (setf (slot-value ins 'highest-sounding) (make-pitch new-value)
        (slot-value ins 'highest-written)
        (when (highest-sounding ins)
          (transpose (highest-sounding ins)
                     (- (transposition-semitones ins))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone ((ins instrument))
  (clone-with-new-class ins 'instrument))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone-with-new-class :around ((ins instrument) new-class)
  (declare (ignore new-class))
  (let ((named-object (call-next-method)))
    (setf (slot-value named-object 'lowest-written) (lowest-written ins)
          (slot-value named-object 'highest-written) (highest-written ins) 
          (slot-value named-object 'lowest-sounding) (lowest-sounding ins) 
          (slot-value named-object 'highest-sounding) (highest-sounding ins) 
          (slot-value named-object 'transposition) (transposition ins)
          ;; (slot-value named-object 'midi-channel) (midi-channel ins)
          ;; (slot-value named-object 'microtones-midi-channel)
          ;; (microtones-midi-channel ins) 
          (slot-value named-object 'transposition-semitones)
          (transposition-semitones ins)
          (slot-value named-object 'starting-clef) (starting-clef ins)
          (slot-value named-object 'largest-fast-leap) (largest-fast-leap ins)
          (slot-value named-object 'staff-name) (staff-name ins)
          (slot-value named-object 'staff-short-name) (staff-short-name ins)
          (slot-value named-object 'missing-notes) 
          (my-copy-list (missing-notes ins))
          (slot-value named-object 'score-write-in-c) (score-write-in-c ins)
          (slot-value named-object 'midi-program) (midi-program ins)
          (slot-value named-object 'score-write-bar-line)
          (score-write-bar-line ins)
          (slot-value named-object 'chords) (chords ins)
          (slot-value named-object 'prefers-notes) (prefers-notes ins)
          (slot-value named-object 'clefs) (clefs ins)
          (slot-value named-object 'clefs-in-c) (clefs-in-c ins)
          (slot-value named-object 'chord-function) (chord-function ins)
          (slot-value named-object 'subset-id) (subset-id ins)
          (slot-value named-object 'microtones) (microtones ins)
          ;; (slot-value named-object 'chord-max) (chord-max ins)
          (slot-value named-object 'total-bars) (total-bars ins) 
          (slot-value named-object 'total-notes) (total-notes ins)
          (slot-value named-object 'total-duration) (total-duration ins)
          (slot-value named-object 'total-degrees) (total-degrees ins))
    named-object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object :before ((ins instrument) stream)
  (format stream "~&INSTRUMENT: lowest-written: ~a, highest-written: ~a~
                  ~%            lowest-sounding: ~a, highest-sounding: ~a~
                  ~%            starting-clef: ~a, clefs: ~a, clefs-in-c: ~a~
                  ~%            prefers-notes: ~a, midi-program: ~a~
                  ~%            transposition: ~a, transposition-semitones: ~a~
                  ~%            score-write-in-c: ~a, score-write-bar-line: ~a~
                  ~%            chords: ~a, chord-function: ~a, ~
                  ~%            total-bars: ~a total-notes: ~a, ~
                                total-duration: ~a~
                  ~%            total-degrees: ~a, microtones: ~a~
                  ~%            missing-notes: ~a, subset-id: ~a~
                  ~%            staff-name: ~a, staff-short-name : ~a,
                  ~%            largest-fast-leap: ~a"
          (lowest-written ins) (highest-written ins) 
          (lowest-sounding ins) (highest-sounding ins) 
          (starting-clef ins) (clefs ins) (clefs-in-c ins) 
          (prefers-notes ins) (midi-program ins)
          (transposition ins) (transposition-semitones ins) 
          (score-write-in-c ins)  (score-write-bar-line ins)
          (chords ins) (chord-function ins) (total-bars ins) 
          (total-notes ins) (total-duration ins)
          (total-degrees ins) (microtones ins) 
          (pitch-list-to-symbols (missing-notes ins)) (subset-id ins)
          (staff-name ins) (staff-short-name ins) (largest-fast-leap ins)))
                           
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod check-starting-clef ((ins instrument))
  (unless (is-clef (starting-clef ins))
    (error "instrument::check-starting-clef: Don't recognise ~a ~
            (did you type an extra (superfluous) quote?)"
           (starting-clef ins))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod tessitura-degree ((ins instrument))
  (let ((tnotes (total-notes ins))
        (tdegrees (total-degrees ins)))
    (if (or (zerop tnotes) (zerop tdegrees))
        0
      (/ tdegrees tnotes))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod tessitura-note ((ins instrument))
  (degree-to-note (tessitura-degree ins)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat Jan  7 11:32:53 EST 2012: Added robodoc info

;;; ****m* instrument/transposing-instrument-p
;;; FUNCTION
;;; Determine whether a given instrument object defines a transposing
;;; instrument. 
;;; 
;;; ARGUMENTS
;;; - An instrument object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - ignore-octaves. Default = T.
;;; 
;;; RETURN VALUE
;;; Returns T if the given instrument object defines a transposing instrument,
;;; otherwise NIL.
;;; 
;;; EXAMPLE
#|
;; Returns NIL if the instrument is not a transposing instrument
(let ((i1 (make-instrument 'instrument-one)))
  (transposing-instrument-p i1))

=> NIL

;; Returns T if the instrument object has been defined using a non-NIL value
;; for :transposition
(let ((i2 (make-instrument 'instrument-two :transposition 'bf)))
  (transposing-instrument-p i2))

=> T

;; Returns T if the instrument object has been defined using a non-0 value for
;; :transposition-semitones 
(let ((i3 (make-instrument 'instrument-two :transposition-semitones -3)))
  (transposing-instrument-p i3))

=> T

|#
;;; SYNOPSIS
(defmethod transposing-instrument-p ((ins instrument) 
                                     &optional (ignore-octaves t))
;;; ****
  (cond ((zerop (transposition-semitones ins)) 
         nil)
        ((and ignore-octaves
              (eq (transposition ins) 'C))
         nil)
        (t t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod best-clef ((ins instrument) pitch-or-chord in-c current-clef 
                      verbose)
  (object-is-nil? pitch-or-chord "instrument::best-clef" 'pitch-or-chord)
  (cond ((and in-c
              (= 1 (length (clefs-in-c ins))))
         (econs (clefs-in-c ins) nil))
        ((and (not in-c)
              (= 1 (length (clefs ins))))
         (econs (clefs ins) nil))
        (t (best-clef-aux ins pitch-or-chord in-c current-clef verbose))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat Jan  7 11:52:08 EST 2012

;;; ****m* instrument/set-prefers-low
;;; DATE
;;; 05 Feb 2011
;;; 
;;; FUNCTION
;;; Sets the PREFERS-NOTES slot of the given instrument object to 'LOW.
;;; 
;;; ARGUMENTS
;;; - An instrument object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; (- optional ignore argument; for internal use only).
;;; 
;;; RETURN VALUE
;;; Returns symbol LOW.
;;; 
;;; EXAMPLE
#|
;; Returns symbol LOW by default
(let ((i1 (make-instrument 'inst)))
  (set-prefers-low i1))

=> LOW

;; Create an instrument object with only an ID, print the PREFERS-NOTES slot to
;; see that it is NIL by default, apply the set-prefers-low, and print the
;; slot again to see the changes
(let ((i1 (make-instrument 'inst)))
  (print (prefers-notes i1))
  (set-prefers-low i1)
  (print (prefers-notes i1)))

=> 
NIL 
LOW

;; Reset to LOW from HIGH
(let ((i1 (make-instrument 'inst :prefers-notes 'high)))
  (print (prefers-notes i1))
  (set-prefers-low i1)
  (print (prefers-notes i1)))

=>
HIGH 
LOW

|#
;;; SYNOPSIS
(defmethod set-prefers-low ((ins instrument) &optional ignore)
;;; **** 
  (declare (ignore ignore))
  (setf (prefers-notes ins) 'low))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat Jan  7 12:02:59 EST 2012: Added robodoc info

;;; ****m* instrument/set-prefers-high
;;; DATE
;;; 05 Feb 2011
;;; 
;;; FUNCTION
;;; Sets the PREFERS-NOTES slot of the given instrument object to 'HIGH.
;;; 
;;; ARGUMENTS
;;; - An instrument object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; (- optional ignore argument; for internal use only).
;;; 
;;; RETURN VALUE
;;; Returns symbol HIGH.
;;; 
;;; EXAMPLE
#|

;; Returns symbol HIGH by default
(let ((i1 (make-instrument 'inst)))
  (set-prefers-high i1))

=> HIGH

;; Create an instrument object with only an ID, print the PREFERS-NOTES slot to
;; see that it is NIL by default, apply the set-prefers-high, and print the
;; slot again to see the changes
(let ((i1 (make-instrument 'inst)))
  (print (prefers-notes i1))
  (set-prefers-high i1)
  (print (prefers-notes i1)))

=> 
NIL 
HIGH

;; Reset to HIGH from LOW
(let ((i1 (make-instrument 'inst :prefers-notes 'low)))
  (print (prefers-notes i1))
  (set-prefers-high i1)
  (print (prefers-notes i1)))

=>
LOW
HIGH 

|#
;;; SYNOPSIS
(defmethod set-prefers-high ((ins instrument) &optional ignore) 
;;; ****
  (declare (ignore ignore))
  (setf (prefers-notes ins) 'high))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat Jan  7 12:08:12 EST 2012: Added robodoc info

;;; ****m* instrument/prefers-low
;;; FUNCTION
;;; Determine whether the PREFERS-NOTES slot of a given instrument object is
;;; set to 'LOW. 
;;; 
;;; ARGUMENTS
;;; - An instrument object.
;;; 
;;; RETURN VALUE
;;; Returns T if the PREFERS-NOTES slot of the given instrument object is set
;;; to 'LOW, otherwise NIL.
;;; 
;;; EXAMPLE
#|
;; Returns T if the PREFERS-NOTES slot of the given instrument object is set to
;; 'LOW 
(let ((i1 (make-instrument 'inst :prefers-notes 'low)))
  (prefers-low i1))

=> T

;; Returns NIL if the PREFERS-NOTES slot of the given instrument object is not
;; set to 'LOW
(let ((i1 (make-instrument 'inst1))
      (i2 (make-instrument 'inst2 :prefers-notes 'high)))
  (print (prefers-low i1))
  (print (prefers-low i2)))

=>
NIL 
NIL 

|#
;;; SYNOPSIS
(defmethod prefers-low ((ins instrument))
;;; ****
  (eq (prefers-notes ins) 'low))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat Jan  7 12:15:24 EST 2012: Added robodoc info

;;; ****m* instrument/prefers-high
;;; FUNCTION
;;; Determine whether the PREFERS-NOTES slot of a given instrument object is
;;; set to 'HIGH. 
;;; 
;;; ARGUMENTS
;;; - An instrument object.
;;; 
;;; RETURN VALUE
;;; Returns T if the PREFERS-NOTES slot of the given instrument object is set
;;; to 'HIGH, otherwise NIL.
;;; 
;;; EXAMPLE
#|
;; Returns T if the PREFERS-NOTES slot of the given instrument object is set to
;; 'HIGH
(let ((i1 (make-instrument 'inst :prefers-notes 'high)))
  (prefers-high i1))

=> T

;; Returns NIL if the PREFERS-NOTES slot of the given instrument object is not
;; set to 'HIGH
(let ((i1 (make-instrument 'inst1))
      (i2 (make-instrument 'inst2 :prefers-notes 'low)))
  (print (prefers-high i1))
  (print (prefers-high i2)))

=>
NIL 
NIL 

|#
;;; SYNOPSIS
(defmethod prefers-high ((ins instrument))
;;; ****
  (eq (prefers-notes ins) 'high))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat Jan  7 12:20:15 EST 2012: Added robodoc info

;;; ****m* instrument/in-range
;;; FUNCTION
;;; Checks whether a specified pitch falls within the defined range of a given
;;; instrument object or not.
;;; 
;;; ARGUMENTS 
;;; - An instrument object.
;;; - A pitch item (pitch object or note-naem symbol).
;;;
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to indicate whether the pitch specified ist to be compared with
;;;   the given pitch object's sounding or written range. T = Sounding. 
;;;   Default = NIL. If T, a secondary NIL is also returned to indicate that
;;;   the specified pitch is neither too high nor too low.
;;; 
;;; RETURN VALUE  
;;; Returns T if the specified pitch falls between the
;;; lowest-sounding/lowest-written and the highest-sounding/highest-written
;;; pitches of the given pitch object.
;;;
;;; If the specified pitch is outside of the range, an additional value of 0 or
;;; 1 is also returned to indicate whether the specified pithc is too high (1)
;;; or too low (0).
;;;
;;; EXAMPLE

#|
;; Determine if a pitch provided as a note-name symbol falls within the written
;; range of a non-transposing instrument
(let ((i1 (make-instrument 'inst1 :lowest-written 'bf3 :highest-written 'a6)))
  (in-range i1 'c4))

=> T, NIL

;; Determine if a pitch provided as a note-name symbol falls within the
;; sounding range of a transposing instrument, using the optional argument T
(let ((i2 (make-instrument 'inst1 :lowest-written 'fs3 :highest-written 'c6
                           :transposition 'BF)))
  (in-range i2 'c6 T))

=> NIL, 1

;; A pitch object can be used as the specified pitch
(let ((i2 (make-instrument 'inst1 :lowest-written 'fs3 :highest-written 'c6
                           :transposition 'BF)))
  (in-range i2 (make-pitch 'd6)))

=> NIL, 1

|#
;;; 
;;; SYNOPSIS
(defmethod in-range ((ins instrument) pitch &optional sounding)
;;; ****
  (let* ((p (make-pitch pitch))
         (low (if sounding (lowest-sounding ins) (lowest-written ins)))
         (high (if sounding (highest-sounding ins) (highest-written ins)))
         (too-high (pitch> p high))
         (too-low (pitch< p low))
         (out (not (or too-high too-low))))
    (values out (cond (too-high 1) (too-low 0)))))
;;; ****

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Related functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun check-transposition-against-semitones (transp st)
  (let ((myst (transposition-to-semitones transp nil)))
    ;; contrabass clarinet is in b flat and sounds -26 semitones lower than
    ;; written... 
    (loop for x from (- myst 24) by 12 repeat 5
        if (= x st) do (return t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Fri Jan  6 19:06:39 EST 2012: Added robodoc info

;;; ****f* instrument/make-instrument
;;; FUNCTION
;;; Create an instrument object, specifying the values for a number of
;;; parameters for describing characteristics of a given instrument, such as
;;; lowest and highest pitch, transposition, clefs used by the instrument etc. 
;;;
;;; NB: The user will generally define instruments will in the context of the
;;; make-instrument-palette function and added directly to the
;;; +slippery-chicken-standard-instrument-palette+ parameter of the file
;;; instruments.lsp, using the keyword structure shown below in the OPTIONAL
;;; ARGUMENTS.
;;; 
;;; ARGUMENTS
;;; - A symbol that is the instrument ID.
;;; 
;;; OPTIONAL ARGUMENTS
;;; &key arguments:

;;; - :staff-name. String. This is the unabbreviated instrument name that will
;;;   be used for the first page of printed scores.

;;; - :staff-short-name. String. This is the abbreviated instrument name that
;;;   will be used for subsequent pages of printed scores.

;;; - :lowest-written. Note-name symbol. This is the lowest written pitch
;;;   available on the given instrument. Defaults to NIL. A user may only
;;;   define either the lowest-written value or the lowest-sounding value. If a
;;;   lowest-written value is given, the method automatically determines the
;;;   lowest-sounding value based on the lowest-written value and the
;;;   transposition value.

;;; - :highest-written. Note-name symbol. This is the highest written pitch
;;;   available on the given instrument. Defaults to NIL. A user may only
;;;   define either the highest-written value or the highest-sounding value. If
;;;   a highest-written value is given, the method automatically determines the
;;;   highest-sounding value based on the highest-written value and the
;;;   transposition value.

;;; - :lowest-sounding. Note-name symbol. This is the lowest sounding pitch
;;;   available on the given instrument. Defaults to NIL. A user may only
;;;   define either the lowest-sounding value or the lowest-written value. If a
;;;   lowest-sounding value is given, the method automatically determines the
;;;   lowest-written value based on the lowest-sounding value and the
;;;   transposition value.

;;; - :highest-sounding. Note-name symbol. This is the highest sounding pitch
;;;   available on the given instrument. Defaults to NIL. A user may only
;;;   define either the highest-sounding value or the highest-written value. If
;;;   a highest-sounding value is given, the method automatically determines
;;;   the highest-written value based on the highest-sounding value and the
;;;   transposition value.

;;; - :transposition. Note-name symbol. This is the key of the given instrument
;;;   (such as the "B-flat" of the "B-flat clarinet"), given as a note-name
;;;   symbol (such as 'BF for B-flat). If a value is only given for the
;;;   :transposition argument but not for the :transposition-semitones
;;;   argument, and there are multiple semitone transposition options for the
;;;   key specified, the method will choose the most common semitone
;;;   transposition for that given key. NB: When using keyword argument
;;;   :transposition rather than :transposition-semitones, sc will have a
;;;   warning printed by cm with indications as to which direction the
;;;   transposition has been undertaken.

;;; - :transposition-semitones. Integer (positive or negative). The number of
;;;   semitones lower that a given instrument sounds than written, e.g. -2 for
;;;   B-flat Clarinet. If a value is only given for the
;;;   :transposition-semitones argument but not for the :transposition
;;;   argument, the method will automatically determine the key for the
;;;   :transposition argument. The listener will drop into the debugger with an
;;;   error if a key is given for the :transposition argument and the number
;;;   specified for the :transposition-semitones does not correspond with that
;;;   key.

;;; - :starting-clef. Symbol. This value determines the first clef that a given
;;;   instrument is to use if that instrument can use different clefs.  
;;;   Default = 'treble.

;;; - :clefs. List of symbols. All clefs that a given instrument may use in the
;;;   course of a piece. Clefs are to be given in order of preference. Defaults
;;;   automatically to the value given to :starting-clef if no other clefs are
;;;   specified. NB: If a separate list is indeed given here, the method will
;;;   automatically add the value for :starting-clef as well, should it have
;;;   been omitted. In this case, a warning will also be printed.

;;; - :clefs-in-c. List of symbols. Similar to :clefs, but designates which
;;;   clefs an instrument uses in a C-score; for example, bass clarinet may
;;;   notated in bass cleff for sounding pitches though it is standardly
;;;   notated in treble clef for written pitches.

;;; - :largest-fast-leap. Number. This value indicates the largest interval, in
;;;   semitones, that a player can feasibly perform at a fast tempo on the
;;;   given instrument. Default = 999. "Fast" here is determined for the whole
;;;   piece by the slippery-chicken class's fast-leap-threshold slot.

;;; - :score-write-in-c. T or NIL. Determines whether the musical material for
;;;   the given instrument should be printed in C.  T = print in C. 
;;;   Default = NIL.

;;; - :score-write-bar-line. Integer. This argument is used for indicating
;;;   system-grouping in the printed score. The given integer specifies how
;;;   many instruments above this one should be grouped together with an
;;;   unbroken bar-line. Default = 1.

;;; - :midi-program. Integer. The number of the MIDI program to be used for
;;;   playing back this instrument. Default = 1.

;;; - :chords. T or NIL. Indicates whether the given instrument is capable of
;;;   playing chords (starting with 2-note simultaneities, but not
;;;   multiphonics).

;;; - :subset-id. Symbol, string, number, or NIL.  Indicates the ID of a
;;;   specific subset of the current set to which the instrument's pitch
;;;   selection is limited.  No error will occur if no subset with this ID
;;;   exists in a given set, i.e. some may include this subset, some may not
;;;   and everything will function correctly--if the specified subset is not
;;;   present in the current set the pitch selection routine will select from
;;;   the whole set.  In every case however, the usual set limiting according
;;;   to instrument range etc. will also apply.  Default = NIL.

;;; - :microtones. T or NIL. Indicates whether the instrument can play
;;;   microtones. T = can play microtones. Default = NIL. NB: If this value is
;;;   set to T, a separate :microtones-midi-channel must be specified; this can
;;;   be done for the given instrument object in the :ensemble block of the
;;;   make-slippery-chicken function.

;;; - :missing-notes. A list of note-name symbols. This is a list of any notes
;;;   which the given instrument can't play, for example certain
;;;   quarter-tones. These are to be given by the user as written-pitch
;;;   note-name symbols, but are always stored by the method as sounding
;;;   pitches.

;;; - :prefers-notes. Symbol. 'high, 'low or NIL. This value indicates whether
;;;   to give preference, when choosing notes for the given instrument, to
;;;   pitches from the upper or lower end of the instrument's range. When NIL,
;;;   preference is given to notes from its middle register. Default = NIL.

;;; - :chord-function. If the given instrument can play chords then it will
;;;   need a reference to a function that can select chords for it. NB This
;;;   should be a symbol not a function object; thus, 'my-fun not
;;;   #'my-fun. Default = NIL.

;;; 
;;; RETURN VALUE
;;; Returns an instrument object.
;;; 
;;; EXAMPLE

#|
;; Make-instrument for the flute:
(make-instrument 'flute :staff-name "Flute" :staff-short-name "Fl."
                 :lowest-written 'c4 :highest-written 'd7 
                 :starting-clef 'treble :midi-program 74 :chords nil
                 :microtones t :missing-notes '(cqs4 dqf4))

=> 
INSTRUMENT: lowest-written: 
PITCH: frequency: 261.626, midi-note: 60, midi-channel: 0 
[...]
, highest-written:
PITCH: frequency: 2349.318, midi-note: 98, midi-channel: 0 
[...]
lowest-sounding: 
PITCH: frequency: 261.626, midi-note: 60, midi-channel: 0 
[...]
, highest-sounding: 
PITCH: frequency: 2349.318, midi-note: 98, midi-channel: 0 
            starting-clef: TREBLE, clefs: (TREBLE), clefs-in-c: (TREBLE)
            prefers-notes: NIL, midi-program: 74
            transposition: C, transposition-semitones: 0
            score-write-in-c: NIL, score-write-bar-line: 1
            chords: NIL, chord-function: NIL, 
            total-bars: 0 total-notes: 0, total-duration: 0.0
            total-degrees: 0, microtones: T
            missing-notes: (CQS4 DQF4), subset-id: NIL
            staff-name: Flute, staff-short-name : Fl.,
            largest-fast-leap: 999
[...]
NAMED-OBJECT: id: FLUTE, tag: NIL, 
data: NIL

;; A make-instrument for the b-flat bass clarinet
(make-instrument 'bass-clarinet :staff-name "Bass Clarinet" :lowest-written 'c3 
                 :highest-written 'g6 :staff-short-name "Bass Cl." 
                 :chords nil :midi-program 72 :starting-clef 'treble
                 :microtones t :prefers-notes 'low
                 :missing-notes '(aqs4 bqf4 bqs4 cqs5 dqf5 gqf3 fqs3 fqf3 eqf3
                                  dqs3 dqf3 cqs3)
                 :clefs '(treble) :clefs-in-c '(treble bass)
                 :transposition-semitones -14)

=> 
INSTRUMENT: lowest-written: 
PITCH: frequency: 130.813, midi-note: 48, midi-channel: 0 
[...]
, highest-written: 
PITCH: frequency: 1567.982, midi-note: 91, midi-channel: 0 
[...]
            lowest-sounding: 
PITCH: frequency: 58.270, midi-note: 34, midi-channel: 0 
[...]
, highest-sounding: 
PITCH: frequency: 698.456, midi-note: 77, midi-channel: 0 
[...]
NAMED-OBJECT: id: BASS-CLARINET, tag: NIL, 
data: NIL

|#
;;; SYNOPSIS
(defun make-instrument (id &key 
                        staff-name
                        staff-short-name
                        lowest-written
                        highest-written
                        lowest-sounding
                        highest-sounding
                        transposition
                        transposition-semitones
                        (starting-clef 'treble)
                        clefs
                        (largest-fast-leap 999)
                        score-write-in-c
                        (score-write-bar-line 1)
                        (midi-program 1)
                        chords
                        clefs-in-c
                        subset-id
                        microtones
                        missing-notes
                        prefers-notes
                        chord-function)
;;; ****
  (make-instance 'instrument :id id
                 :staff-name staff-name
                 :staff-short-name staff-short-name
                 :lowest-written lowest-written
                 :largest-fast-leap largest-fast-leap
                 :highest-written highest-written
                 :starting-clef starting-clef
                 :missing-notes missing-notes
                 :lowest-sounding lowest-sounding
                 :highest-sounding highest-sounding
                 :subset-id subset-id
                 :clefs clefs
                 :prefers-notes prefers-notes
                 :clefs-in-c clefs-in-c
                 :transposition transposition
                 :midi-program midi-program
                 :transposition-semitones transposition-semitones
                 :score-write-in-c score-write-in-c
                 :score-write-bar-line score-write-bar-line
                 :microtones microtones
                 :chords chords
                 :chord-function chord-function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((clex (make-assoc-list 
             'clef-extremes
             ;; lowest highest ledgers-below-begin ledgers-above-begin
             '((bass (ef1 b4 e2 c4))
               (treble (ef3 d7 cs4 af5))
               (alto (bf2 fs5 ds3 bf4))
               (tenor (bf2 cs5 b2 gf4))
               ;; these are the clefs with 8ve signs under/over them
               (double-bass (c0 cs2 e1 c3))
               (double-treble (f6 g9 c5 a6))))))
  (loop for clef in (data clex) do 
        (setf (data clef) (loop for note in (data clef)
                              collect (make-pitch note))))
  (defun best-clef-aux (ins pitch-or-chord in-c current-clef 
                            verbose)
    (flet ((in-clef-range (chord-highest chord-lowest clef)
             (let* ((clx (get-data clef clex))
                    (clo (first (data clx)))
                    (chi (second (data clx))))
               (and (pitch-in-range chord-highest clo chi)
                    (pitch-in-range chord-lowest clo chi))))
           (needs-ledgers (chord-highest chord-lowest clef)
             (let* ((clx (get-data clef clex))
                    (thresh-below (third (data clx)))
                    (thresh-above (fourth (data clx)))
                    (below (pitch- thresh-below chord-lowest))
                    (above (pitch- chord-highest thresh-above))
                    (max (max below above)))
               (when verbose
                 (format t "~&needs-ledgers: clef ~a hi ~a low ~a below ~a ~
                            above ~a" 
                         clef (id chord-highest) (id chord-lowest) below 
                         above))
               (when (> max 0)
                 max))))
      (let* ((chord (chord-p pitch-or-chord))
             (hi (if chord
                     (highest pitch-or-chord)
                   pitch-or-chord))
             (low (if chord
                      (lowest pitch-or-chord)
                    pitch-or-chord))
             result)
        ;; if we don't pass an ins argument just return whether we're in the
        ;; clef's range or not 
        (if (not ins)
            (in-clef-range hi low current-clef)
          (progn
            (setf result
              (loop 
                  with best
                  with alternative
                  with ledgers
                       ;; clefs preference order has already been reversed in
                       ;; init; this ensures we will set the clefs in the
                       ;; instrument's preferred order 
                  for clef in (if in-c
                                  (clefs-in-c ins)
                                (clefs ins))
                  do
                    (when (in-clef-range hi low clef)
                      (setf ledgers (needs-ledgers hi low clef))
                      (cond ((and ledgers
                                  (or (not alternative)
                                      (< ledgers (second alternative))))
                             ;; (unless best
                               ;; (setf best clef))
                             (setf alternative (list clef ledgers)))
                            ;; alternative would be better but this is the
                            ;; current clef and we're in range 
                            ((and ledgers alternative
                                  (not best)
                                      ;; (equal best (first alternative)))
                                  (>= ledgers (second alternative))
                                  ;; (equal clef current-clef)
                                  )
                             (setf best (first alternative)
                                   alternative (list clef ledgers)))
                            ((and (not ledgers)
                                  (equal best current-clef)
                                  (not alternative))
                             (setf alternative (list clef 0)))
                            ;; we've already got a best and perhaps an
                            ;; alternative but this is the current clef and
                            ;; there's no need for ledgers 
                            ((and (not ledgers)
                                  (equal clef current-clef)
                                  best)
                             (setf alternative (list best 0)
                                   best clef))
                            ((and (not ledgers) 
                                  (not (equal best current-clef)))
                             (setf best clef))
                            ((and (not ledgers)
                                  (equal best current-clef))
                             (setf alternative (list clef 0)))))
                    (when verbose
                      (format t "~&clef ~a ledgers ~a best ~a alt ~a"
                              clef ledgers best alternative))
                  finally (return 
                            ;; b5 with a choice of treble and bass gives you
                            ;; treble as alternative and nil and best.... 
                            (if best
                                (list best (first alternative))
                              (list (first alternative) nil)))))
            (unless result
              (error "~&instrument::best-clef: didn't work for ~a, ~a: ~
                       clefs: ~a"
                     (if chord 
                         (get-pitch-symbols pitch-or-chord)
                       (id pitch-or-chord))
                     (id ins) (clefs ins)))
            (when verbose 
              (format t "~&best-clef for ~a on ~a with current of ~a: ~a"
                      (if chord
                          (get-pitch-symbols pitch-or-chord)
                        (id pitch-or-chord))
                      (id ins) current-clef result))
            result))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****f* instrument/default-chord-function
;;; FUNCTION
;;; If an instrument is able to play chords, we need to define a function to
;;; select notes from a list that it can play as a chord. This function (as
;;; a symbol) is passed as a slot to the instrument instance.
;;; 
;;; This is the default function; it returns a 2-note chord with the note at
;;; index plus that below it, or that above it if no notes are below. Or it
;;; just returns a single-note chord if neither of those cases are possible.
;;;
;;; NB The arguments are supplied by slippery chicken when it calls the
;;;    function for you. 
;;;
;;; ARGUMENTS 
;;; - The current number from the pitch-seq. Currently ignored by default.
;;; - The index that the first argument was translated into by the offset and
;;;   scaler (based on trying to get a best fit for the instrument and set).
;;;   This can be assumed to be a legal reference into pitch-list as it was
;;;   calculated as fitting in pitch-seq::get-notes.  (Zero-based.)
;;; - The pitch-list that we created from the set, taking the instrument's range
;;;   and other notes already played by other instruments.
;;; - The current pitch-seq object. Currently ignored by default.
;;; - The current instrument object. Currently ignored by default.
;;; - The current set object. Currently ignored by default.
;;; 
;;; RETURN VALUE  
;;; a chord object
;;; 
;;; SYNOPSIS
(defun default-chord-function (curve-num index pitch-list pitch-seq instrument
                               set)
;;; **** 
  (declare (ignore set instrument pitch-seq curve-num))
  (let ((at-index (nth index pitch-list))
        p1 p2)
    (cond ((> index 0) (setf p1 (nth (1- index) pitch-list)
                             p2 at-index))
          ;; 26.2.11 >= instead of >
          ((>= (length pitch-list) 2) (setf p1 at-index 
                                            p2 (nth (1+ index) pitch-list)))
          (t (setf p1 at-index
                   p2 nil)))
    ;; don't create diads > 8ve
    (when (and p2 (> (pitch- p2 p1) 12)) ; assuming pitch-list is sorted
      (setf p2 nil))
    (when (or p1 p2) ; MDE Tue Mar 20 16:20:05 2012 
      (if p2
          (make-chord (list p1 p2))
          (make-chord (list p1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat Jan  7 14:20:45 EST 2012: Added instrument-p function
(defun instrument-p (thing)
  (typep thing 'instrument))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF instrument.lsp
